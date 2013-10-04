// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS loader. Uses a BMS parser (`format::bms::parse`) to produce `format::bms::Bms` structure.

use std::{vec, iter, io};
use std::rand::*;

use format::obj::*;
use format::bms::parse;
use format::bms::types::{Key, MAXKEY};
use format::bms::diag::*;
use format::bms::{ImageRef, SoundRef, DefaultBPM, SinglePlay, BmsMeta, Bms};

/// Loader options for BMS format.
pub struct BmsLoaderOptions {
    /// Parser options.
    parser: parse::BmsParserOptions,
}

impl BmsLoaderOptions {
    /// Returns default loader options.
    pub fn new() -> BmsLoaderOptions {
        BmsLoaderOptions { parser: parse::BmsParserOptions::new() }
    }
}

/// Reads the BMS file with given RNG from given reader. Diagnostic messages are sent via callback.
pub fn load_bms_from_reader<'r,R:Rng,Listener:BmsMessageListener>(
                                f: @io::Reader, r: &mut R, opts: &BmsLoaderOptions,
                                callback: &mut Listener) -> Result<Bms,~str> {
    use format::timeline::builder::{TimelineBuilder, Mark};

    let mut title = None;
    let mut genre = None;
    let mut artist = None;
    let mut stagefile = None;
    let mut basepath = None;
    let mut player = SinglePlay;
    let mut playlevel = 0;
    let mut rank = 2;
    let mut sndpath = vec::from_elem(MAXKEY as uint, None);
    let mut imgpath = vec::from_elem(MAXKEY as uint, None);
    let mut blitcmd = ~[];

    /// The state of the block, for determining which lines should be processed.
    enum BlockState {
        /// Not contained in the #IF block.
        Outside,
        /// Active.
        Process,
        /// Inactive, but (for the purpose of #IF/#ELSEIF/#ELSE/#ENDIF structure) can move to
        /// `Process` state when matching clause appears.
        Ignore,
        /// Inactive and won't be processed until the end of block.
        NoFurther
    }

    impl BlockState {
        /// Returns true if lines should be ignored in the current block given that the parent
        /// block was active.
        fn inactive(self) -> bool {
            match self { Outside | Process => false, Ignore | NoFurther => true }
        }
    }

    /**
     * Block information. The parser keeps a list of nested blocks and determines if
     * a particular line should be processed or not.
     *
     * Sonorous actually recognizes only one kind of blocks, starting with #RANDOM or
     * #SETRANDOM and ending with #ENDRANDOM or #END(IF) outside an #IF block. An #IF block is
     * a state within #RANDOM, so it follows that #RANDOM/#SETRANDOM blocks can nest but #IF
     * can't nest unless its direct parent is #RANDOM/#SETRANDOM.
     */
    struct Block {
        /// A generated value if any. It can be `None` if this block is the topmost one (which
        /// is actually not a block but rather a sentinel) or the last `#RANDOM` or `#SETRANDOM`
        /// command was invalid, and #IF in that case will always evaluates to false.
        val: Option<int>,
        /// The state of the block.
        state: BlockState,
        /// True if the parent block is already ignored so that this block should be ignored
        /// no matter what `state` is.
        skip: bool
    }

    impl Block {
        /// Returns true if lines should be ignored in the current block.
        fn inactive(&self) -> bool { self.skip || self.state.inactive() }
    }

    // Rust: #[deriving(Eq)] does not work inside the function. (#4913)
    impl Eq for BlockState {
        fn eq(&self, other: &BlockState) -> bool {
            match (*self, *other) {
                (Outside, Outside) | (Process, Process) |
                (Ignore, Ignore) | (NoFurther, NoFurther) => true,
                (_, _) => false
            }
        }
        fn ne(&self, other: &BlockState) -> bool { !self.eq(other) }
    }
    impl Eq for Block {
        fn eq(&self, other: &Block) -> bool {
            // Rust: this is for using `ImmutableEqVector<T>::rposition`, which should have been
            //       in `ImmutableVector<T>`.
            self.val == other.val && self.state == other.state && self.skip == other.skip
        }
        fn ne(&self, other: &Block) -> bool { !self.eq(other) }
    }

    // A list of nested blocks.
    let mut blk = ~[Block { val: None, state: Outside, skip: false }];

    /// An unprocessed data line of BMS file.
    struct BmsLine { measure: uint, chan: Key, data: ~str }

    impl Ord for BmsLine {
        fn lt(&self, other: &BmsLine) -> bool {
            self.measure < other.measure ||
            (self.measure == other.measure && self.chan < other.chan)
        }
        fn le(&self, other: &BmsLine) -> bool {
            self.measure < other.measure ||
            (self.measure == other.measure && self.chan <= other.chan)
        }
        fn ge(&self, other: &BmsLine) -> bool { !self.lt(other) }
        fn gt(&self, other: &BmsLine) -> bool { !self.le(other) }
    }

    impl Clone for BmsLine {
        fn clone(&self) -> BmsLine {
            BmsLine { measure: self.measure, chan: self.chan, data: self.data.clone() }
        }
    }

    // A builder for objects.
    let mut builder = TimelineBuilder::new();
    builder.set_initbpm(DefaultBPM);

    // A list of unprocessed data lines. They have to be sorted with a stable algorithm and
    // processed in the order of measure number.
    let mut bmsline = ~[];
    // A table of measure factors (#xxx02). They are eventually converted to `SetMeasureFactor`
    // objects.
    let mut shortens = ~[];
    // A table of BPMs. Maps to BMS #BPMxx command.
    let mut bpmtab = ~[DefaultBPM, ..MAXKEY];
    // A table of the length of scroll stoppers. Maps to BMS #STOP/#STP commands.
    let mut stoptab = ~[Seconds(0.0), ..MAXKEY];

    // Allows LNs to be specified as a consecutive row of same or non-00 alphanumeric keys (MGQ
    // type, #LNTYPE 2). The default is to specify LNs as two endpoints (RDM type, #LNTYPE 1).
    let mut consecutiveln = false;

    // An end-of-LN marker used in LN specification for channels #1x/2x. Maps to BMS #LNOBJ
    // command.
    let mut lnobj = None;

    do parse::each_bms_command(f, &opts.parser, callback) |cmd| {
        assert!(!blk.is_empty());
        match (cmd, blk.last().inactive()) {
            (parse::BmsTitle(s),           false) => { title = Some(s.to_owned()); }
            (parse::BmsGenre(s),           false) => { genre = Some(s.to_owned()); }
            (parse::BmsArtist(s),          false) => { artist = Some(s.to_owned()); }
            (parse::BmsStageFile(s),       false) => { stagefile = Some(s.to_owned()); }
            (parse::BmsPathWAV(s),         false) => { basepath = Some(s.to_owned()); }
            (parse::BmsBPM(bpm),           false) => { builder.set_initbpm(bpm); }
            (parse::BmsExBPM(Key(i), bpm), false) => { bpmtab[i] = bpm; }
            (parse::BmsPlayer(v),          false) => { player = v; }
            (parse::BmsPlayLevel(v),       false) => { playlevel = v; }
            (parse::BmsRank(v),            false) => { rank = v; }
            (parse::BmsLNType(lntype),     false) => { consecutiveln = (lntype == 2); }
            (parse::BmsLNObj(key),         false) => { lnobj = Some(key); }
            (parse::BmsWAV(Key(i), s),     false) => { sndpath[i] = Some(s.to_owned()); }
            (parse::BmsBMP(Key(i), s),     false) => { imgpath[i] = Some(s.to_owned()); }
            (parse::BmsBGA(bc),            false) => { blitcmd.push(bc); }
            (parse::BmsStop(Key(i), dur),  false) => { stoptab[i] = dur; }
            (parse::BmsStp(pos, dur),      false) => { builder.add(pos, Stop(dur)); }

            (parse::BmsShorten(measure, factor), false) => {
                if factor > 0.0 {
                    shortens.grow_set(measure, &1.0, factor);
                }
            }
            (parse::BmsData(measure, chan, data), false) => {
                bmsline.push(BmsLine { measure: measure, chan: chan, data: data.to_owned() })
            }

            // flow commands
            (parse::BmsRandom(val), _) | (parse::BmsSetRandom(val), _) => {
                let val = if val <= 0 {None} else {Some(val)};
                let setrandom = match cmd { parse::BmsSetRandom(_) => true, _ => false };

                // do not generate a random value if the entire block is skipped (but it
                // still marks the start of block)
                let inactive = blk.last().inactive();
                let generated = do val.and_then |val| {
                    if setrandom {
                        Some(val)
                    } else if !inactive {
                        Some(r.gen_integer_range(1, val + 1))
                    } else {
                        None
                    }
                };
                blk.push(Block { val: generated, state: Outside, skip: inactive });
            }
            (parse::BmsEndRandom, _) => {
                if blk.len() > 1 { blk.pop(); }
            }
            (parse::BmsIf(val), _) | (parse::BmsElseIf(val), _) => {
                let val = if val <= 0 {None} else {Some(val)};
                let haspriorelse = match cmd { parse::BmsElseIf(_) => true, _ => false };

                // Rust: `blk.last_ref()` may be useful?
                let last = &mut blk[blk.len() - 1];
                last.state =
                    if (!haspriorelse && !last.state.inactive()) || last.state == Ignore {
                        if val.is_none() || val != last.val {Ignore} else {Process}
                    } else {
                        NoFurther
                    };
            }
            (parse::BmsElse, _) => {
                let last = &mut blk[blk.len() - 1];
                last.state = if last.state == Ignore {Process} else {NoFurther};
            }
            (parse::BmsEndIf, _) => {
                let lastinside = blk.iter().rposition(|&i| i.state != Outside); // XXX #3511
                for &idx in lastinside.iter() {
                    if idx > 0 { blk.truncate(idx + 1); }
                }

                let last = &mut blk[blk.len() - 1];
                last.state = Outside;
            }

            (_, _) => {}
        }
        true
    };

    // Poor BGA defined by #BMP00 wouldn't be played if it is a movie. We can't just let it
    // played at the beginning of the chart as the "beginning" is not always 0.0 (actually,
    // `originoffset`). Thus we add an artificial BGA object at time 0.0 only when the other
    // poor BGA does not exist at this position.
    let mut poorbgafix = true;

    // Indices to last visible object per channels. A marker specified by #LNOBJ will turn
    // this last object to the start of LN.
    let mut lastvis: [Option<Mark>, ..NLANES] = [None, ..NLANES];

    // Indices to last LN start or end inserted (and not finalized yet) per channels.
    // If `consecutiveln` is on (#LNTYPE 2), the position of referenced object gets updated
    // during parsing; if off (#LNTYPE 1), it is solely used for checking if we are inside
    // the LN or not.
    let mut lastln: [Option<Mark>, ..NLANES] = [None, ..NLANES];

    // Handles a non-00 alphanumeric key `v` positioned at the particular channel `chan` and
    // particular position `t`. The position `t2` next to `t` is used for some cases that
    // an alphanumeric key designates an area rather than a point.
    let handle_key = |chan: Key, t: f64, t2: f64, v: Key| {
        match *chan {
            // channel #01: BGM
            1 => { builder.add(t, BGM(SoundRef(v))); }

            // channel #03: BPM as an hexadecimal key
            3 => {
                let v = v.to_hex(); // XXX #3511
                for &v in v.iter() {
                    builder.add(t, SetBPM(BPM(v as f64)))
                }
            }

            // channel #04: BGA layer 1
            4 => { builder.add(t, SetBGA(Layer1, Some(ImageRef(v)))); }

            // channel #06: POOR BGA
            6 => {
                builder.add(t, SetBGA(PoorBGA, Some(ImageRef(v))));
                poorbgafix = false; // we don't add artificial BGA
            }

            // channel #07: BGA layer 2
            7 => { builder.add(t, SetBGA(Layer2, Some(ImageRef(v)))); }

            // channel #08: BPM defined by #BPMxx
            8 => { builder.add(t, SetBPM(bpmtab[*v])); } // TODO bpmtab validity check

            // channel #09: scroll stopper defined by #STOPxx
            9 => { builder.add(t, Stop(stoptab[*v])); } // TODO stoptab validity check

            // channel #0A: BGA layer 3
            10 => { builder.add(t, SetBGA(Layer3, Some(ImageRef(v)))); }

            // channels #1x/2x: visible object, possibly LNs when #LNOBJ is in active
            36/*1*36*/..107/*3*36-1*/ => {
                let lane = chan.to_lane();
                if lnobj.is_some() && lnobj == Some(v) {
                    // change the last inserted visible object to the start of LN if any.
                    let lastvispos = lastvis[*lane];
                    for &mark in lastvispos.iter() {
                        do builder.mutate(mark) |pos, data| {
                            assert!(data.is_visible());
                            (pos, data.to_lnstart())
                        }
                        builder.add(t, LNDone(lane, Some(SoundRef(v))));
                        lastvis[*lane] = None;
                    }
                } else {
                    let mark = builder.add_and_mark(t, Visible(lane, Some(SoundRef(v))));
                    lastvis[*lane] = Some(mark);
                }
            }

            // channels #3x/4x: invisible object
            108/*3*36*/..179/*5*36-1*/ => {
                let lane = chan.to_lane();
                builder.add(t, Invisible(lane, Some(SoundRef(v))));
            }

            // channels #5x/6x, #LNTYPE 1: LN endpoints
            180/*5*36*/..251/*7*36-1*/ if !consecutiveln => {
                let lane = chan.to_lane();

                // a pair of non-00 alphanumeric keys designate one LN. if there are an odd
                // number of them, the last LN is implicitly closed later.
                if lastln[*lane].is_some() {
                    lastln[*lane] = None;
                    builder.add(t, LNDone(lane, Some(SoundRef(v))));
                } else {
                    let mark = builder.add_and_mark(t, LNStart(lane, Some(SoundRef(v))));
                    lastln[*lane] = Some(mark); // TODO unused for now
                }
            }

            // channels #5x/6x, #LNTYPE 2: LN areas
            180/*5*36*/..251/*7*36-1*/ if consecutiveln => {
                let lane = chan.to_lane();

                // one non-00 alphanumeric key, in the absence of other information, inserts one
                // complete LN starting at `t` and ending at `t2`.
                //
                // the next non-00 alphanumeric key also inserts one complete LN from `t` to
                // `t2`, unless there is already an end of LN at `t` in which case the end of LN
                // is simply moved from `t` to `t2` (effectively increasing the length of
                // previous LN).
                match lastln[*lane] {
                    Some(mark) => {
                        do builder.mutate(mark) |pos, data| {
                            if pos == t {
                                assert!(data.is_lndone());
                                (t2, data)
                            } else {
                                (pos, data)
                            }
                        }
                    }
                    _ => {
                        builder.add(t, LNStart(lane, Some(SoundRef(v))));
                        let mark = builder.add_and_mark(t2, LNDone(lane, Some(SoundRef(v))));
                        lastln[*lane] = Some(mark);
                    }
                }
            }

            // channels #Dx/Ex: bombs, base-36 damage value (unit of 0.5% of the full gauge) or
            // instant death (ZZ)
            468/*0xD*36*/..539/*0xF*36-1*/ => {
                let lane = chan.to_lane();
                let damage = match *v {
                    1..200 => Some(GaugeDamage(*v as f64 / 200.0)),
                    1295 => Some(InstantDeath), // XXX 1295=MAXKEY-1
                    _ => None
                };
                for &damage in damage.iter() {
                    builder.add(t, Bomb(lane, Some(SoundRef(Key(0))), damage));
                }
            }

            // unsupported: channels #0B/0C/0D/0E (BGA opacity), #97/98 (sound volume),
            // #99 (text), #A0 (dynamic #RANK), #A1/A2/A3/A4 (BGA color key update),
            // #A5 (BGA on keypress), #A6 (player-specific option)
            _ => {}
        }
    };

    // loop over the sorted bmslines
    ::extra::sort::tim_sort(bmsline);
    for line in bmsline.iter() {
        let measure = line.measure as f64;
        let data: ~[char] = line.data.iter().collect();
        let max = data.len() / 2 * 2;
        let count = max as f64;
        for i in iter::range_step(0, max, 2) {
            let v = Key::from_chars(data.slice(i, i+2));
            for &v in v.iter() {
                if v != Key(0) { // ignores 00
                    let t = measure + i as f64 / count;
                    let t2 = measure + (i + 2) as f64 / count;
                    handle_key(line.chan, t, t2, v);
                }
            }
        }
    }

    // insert an artificial `SetBGA` object at 0.0 if required
    if poorbgafix {
        builder.add(0.0, SetBGA(PoorBGA, Some(ImageRef(Key(0)))));
    }

    // fix the unterminated longnote
    let nmeasures = bmsline.last_opt().map_default(0, |l| l.measure) + 1;
    let endt = nmeasures as f64;
    for i in range(0, NLANES) {
        if lastvis[i].is_some() || (!consecutiveln && lastln[i].is_some()) {
            builder.add(endt, LNDone(Lane(i), None));
        }
    }

    // convert shortens to objects and insert measure bars
    shortens.grow_set(nmeasures, &1.0, 1.0); // so we always have a normal measure at the end
    let mut prevfactor = 1.0;
    for (measure, &factor) in shortens.iter().enumerate() {
        builder.add(measure as f64, MeasureBar);
        if prevfactor != factor {
            builder.add(measure as f64, SetMeasureFactor(factor));
            prevfactor = factor;
        }
    }

    // set the end of the chart (no measure bar at this position)
    builder.set_end(endt + 1.0);

    let timeline = builder.build();
    Ok(Bms { bmspath: ~"",
             meta: BmsMeta { title: title, genre: genre, artist: artist, stagefile: stagefile,
                             basepath: basepath, player: player, playlevel: playlevel, rank: rank,
                             sndpath: sndpath, imgpath: imgpath, blitcmd: blitcmd },
             timeline: timeline })
}

/// Reads the BMS file with given RNG.
pub fn load_bms<'r,R:Rng,Listener:BmsMessageListener>(
                                bmspath: &str, r: &mut R, opts: &BmsLoaderOptions,
                                callback: &mut Listener) -> Result<Bms,~str> {
    do io::file_reader(&Path(bmspath)).and_then |f| {
        do load_bms_from_reader(f, r, opts, callback).map_move |bms| {
            let mut bms = bms;
            bms.bmspath = bmspath.to_owned();
            bms
        }
    }
}

