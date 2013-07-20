// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

/*!
 * BMS parser module.
 *
 * # Structure
 *
 * The BMS format is a plain text format with most directives start with optional whitespace
 * followed by `#`. Besides the metadata (title, artist etc.), a BMS file is a map from the time
 * position to various game play elements (henceforth "objects") and other object-like effects
 * including BGM and BGA changes. It also contains preprocessor directives used to randomize some or
 * all parts of the BMS file, which would only make sense in the loading time.
 *
 * The time position is a virtual time divided by an unit of (musical) measure. It is related to
 * the actual time by the current Beats Per Minute (BPM) value which can, well, also change during
 * the game play. Consequently it is convenient to refer the position in terms of measures, which
 * the BMS format does: the lines `#xxxyy:AABBCC...` indicates that the measure number `xxx`
 * contains objects or object-like effects (of the type specified by `yy`, henceforth "channels"),
 * evenly spaced throughout the measure and which data values are `AA`, `BB`, `CC` respectively.
 *
 * An alphanumeric identifier (henceforth "alphanumeric key") like `AA` or `BB` may mean that
 * the actual numeric value interpreted as base 16 or 36 (depending on the channel), or a reference
 * to other assets (e.g. `#BMPAA foo.png`) or complex values specified by other commands (e.g.
 * `#BPMBB 192.0`). In most cases, an identifier `00` indicates an absence of objects or object-like
 * effects at that position.
 *
 * More detailed information about BMS format, including surveys about how different implementations
 * (so called BMS players) react to underspecified features or edge cases, can be found at [BMS
 * command memo](http://hitkey.nekokan.dyndns.info/cmds.htm).
 */

use std::uint;
use std::rand::*;

use format::obj::*;
use format::timeline::Timeline;
use format::pointer::Pointer;

pub use format::bms::types::{Key, MAXKEY};

pub mod types;
pub mod parse;

/// Sound reference.
#[deriving(Eq)]
pub struct SoundRef(Key);

/// Image reference.
#[deriving(Eq)]
pub struct ImageRef(Key);

impl ToStr for SoundRef {
    fn to_str(&self) -> ~str { (**self).to_str() }
}

impl ToStr for ImageRef {
    fn to_str(&self) -> ~str { (**self).to_str() }
}

/// Default BPM. This value comes from the original BMS specification.
pub static DefaultBPM: BPM = BPM(130.0);

/**
 * Blit commands, which manipulate the image after the image had been loaded. This maps to BMS
 * #BGA command. (C: `struct blitcmd`)
 *
 * Blitting occurs from the region `(x1,y1)-(x2,y2)` in the source surface to the region
 * `(dx,dy)-(dx+(x2-x1),dy+(y2-y1))` in the destination surface. The rectangular region contains
 * the upper-left corner but not the lower-right corner. The region is clipped to make
 * the upper-left corner has non-negative coordinates and the size of the region doesn't exceed
 * 256 by 256 pixels.
 */
pub struct BlitCmd {
    dst: ImageRef, src: ImageRef,
    x1: int, y1: int, x2: int, y2: int, dx: int, dy: int
}

/// A value of BMS #PLAYER command signifying Single Play (SP), where only channels #1x are used
/// for the game play.
pub static SinglePlay: int = 1;
/// A value of BMS #PLAYER command signifying Couple Play, where channels #1x and #2x renders to
/// the different panels. They are originally meant to be played by different players with
/// separate gauges and scores, but this mode of game play is increasingly unsupported by modern
/// implementations. Angolmois has only a limited support for Couple Play.
pub static CouplePlay: int = 2;
/// A value of BMS #PLAYER command signifying Double Play (DP), where both channels #1x and #2x
/// renders to a single wide panel. The chart is still meant to be played by one person.
pub static DoublePlay: int = 3;

/// Loaded BMS metadata and resources.
pub struct BmsMeta {
    /// Title. Maps to BMS #TITLE command. (C: `string[S_TITLE]`)
    title: Option<~str>,
    /// Genre. Maps to BMS #GENRE command. (C: `string[S_GENRE]`)
    genre: Option<~str>,
    /// Artist. Maps to BMS #ARTIST command. (C: `string[S_ARTIST]`)
    artist: Option<~str>,
    /// Path to an image for loading screen. Maps to BMS #STAGEFILE command.
    /// (C: `string[S_STAGEFILE]`)
    stagefile: Option<~str>,
    /// A base path used for loading all other resources. Maps to BMS #PATH_WAV command.
    /// (C: `string[S_BASEPATH]`)
    basepath: Option<~str>,

    /// Game mode. One of `SinglePlay`(1), `CouplePlay`(2) or `DoublePlay`(3). Maps to BMS
    /// #PLAYER command. (C: `value[V_PLAYER]`)
    player: int,
    /// Game level. Does not affect the actual game play. Maps to BMS #PLAYLEVEL command.
    /// (C: `value[V_PLAYLEVEL]`)
    playlevel: int,
    /// Gauge difficulty. Higher is easier. Maps to BMS #RANK command. (C: `value[V_RANK]`)
    rank: int,

    /// Paths to sound file relative to `basepath` or BMS file. (C: `sndpath`)
    //
    // Rust: constant expression in the array size is unsupported.
    sndpath: [Option<~str>, ..MAXKEY],
    /// Paths to image/movie file relative to `basepath` or BMS file. (C: `imgpath`)
    imgpath: [Option<~str>, ..MAXKEY],
    /// List of blit commands to be executed after `imgpath` is loaded. (C: `blitcmd`)
    blitcmd: ~[BlitCmd],
}

/// Timeline for the BMS file.
pub type BmsTimeline = Timeline<SoundRef,ImageRef>;

/// Pointer for the BMS file. Provided for the convenience.
pub type BmsPointer = Pointer<SoundRef,ImageRef>;

/// Loaded BMS data. This is an intermediate structure for metadata, resources and actual chart
/// data, and cannot be used for actual game play (since `Pointer` requires `Timeline` to be
/// a managed pointer).
pub struct Bms {
    /// A path to the BMS file. Also used for finding the resource when `meta.basepath` is not set.
    /// (C: `bmspath`)
    bmspath: ~str,
    /// Metadata and resources.
    meta: BmsMeta,
    /// Timeline.
    timeline: BmsTimeline,
}

/// Reads and parses the BMS file with given RNG from given reader.
pub fn parse_bms_from_reader<R:RngUtil>(f: @::std::io::Reader, r: &mut R) -> Result<Bms,~str> {
    use format::timeline::builder::{TimelineBuilder, Mark};

    let mut title = None;
    let mut genre = None;
    let mut artist = None;
    let mut stagefile = None;
    let mut basepath = None;
    let mut player = SinglePlay;
    let mut playlevel = 0;
    let mut rank = 2;
    let mut sndpath = [None, ..MAXKEY];
    let mut imgpath = [None, ..MAXKEY];
    let mut blitcmd = ~[];

    /// The state of the block, for determining which lines should be processed.
    enum BlockState {
        /// Not contained in the #IF block. (C: `state == -1`)
        Outside,
        /// Active. (C: `state == 0`)
        Process,
        /// Inactive, but (for the purpose of #IF/#ELSEIF/#ELSE/#ENDIF structure) can move to
        /// `Process` state when matching clause appears. (C: `state == 1`)
        Ignore,
        /// Inactive and won't be processed until the end of block. (C: `state == 2`)
        NoFurther
    }

    impl BlockState {
        /// Returns true if lines should be ignored in the current block given that the parent
        /// block was active. (C: `state > 0`)
        fn inactive(self) -> bool {
            match self { Outside | Process => false, Ignore | NoFurther => true }
        }
    }

    /**
     * Block information. The parser keeps a list of nested blocks and determines if
     * a particular line should be processed or not. (C: `struct rnd`)
     *
     * Angomlois actually recognizes only one kind of blocks, starting with #RANDOM or
     * #SETRANDOM and ending with #ENDRANDOM or #END(IF) outside an #IF block. An #IF block is
     * a state within #RANDOM, so it follows that #RANDOM/#SETRANDOM blocks can nest but #IF
     * can't nest unless its direct parent is #RANDOM/#SETRANDOM.
     */
    struct Block {
        /// A generated value if any. It can be `None` if this block is the topmost one (which
        /// is actually not a block but rather a sentinel) or the last `#RANDOM` or `#SETRANDOM`
        /// command was invalid, and #IF in that case will always evaluates to false. (C: `val`
        /// field)
        val: Option<int>,
        /// The state of the block. (C: `state` field)
        state: BlockState,
        /// True if the parent block is already ignored so that this block should be ignored
        /// no matter what `state` is. (C: `skip` field)
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

    // A list of nested blocks. (C: `rnd`)
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

    // A builder for objects.
    let mut builder = TimelineBuilder::new();
    builder.set_initbpm(DefaultBPM);

    // A list of unprocessed data lines. They have to be sorted with a stable algorithm and
    // processed in the order of measure number. (C: `bmsline`)
    let mut bmsline = ~[];
    // A table of measure factors (#xxx02). They are eventually converted to `SetMeasureFactor`
    // objects.
    let mut shortens = ~[];
    // A table of BPMs. Maps to BMS #BPMxx command. (C: `bpmtab`)
    let mut bpmtab = ~[DefaultBPM, ..MAXKEY];
    // A table of the length of scroll stoppers. Maps to BMS #STOP/#STP commands. (C: `stoptab`)
    let mut stoptab = ~[Seconds(0.0), ..MAXKEY];

    // Allows LNs to be specified as a consecutive row of same or non-00 alphanumeric keys (MGQ
    // type, #LNTYPE 2). The default is to specify LNs as two endpoints (RDM type, #LNTYPE 1).
    // (C: `value[V_LNTYPE]`)
    let mut consecutiveln = false;

    // An end-of-LN marker used in LN specification for channels #1x/2x. Maps to BMS #LNOBJ
    // command. (C: `value[V_LNOBJ]`)
    let mut lnobj = None;

    for parse::each_bms_command(f) |cmd| {
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
                let generated = do val.chain |val| {
                    // Rust: there should be `Option<T>::chain` if `T` is copyable.
                    if setrandom {
                        Some(val)
                    } else if !inactive {
                        Some(r.gen_int_range(1, val + 1)) // Rust: not an uniform distribution yet!
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
                let lastinside = blk.rposition(|&i| i.state != Outside); // XXX #3511
                for lastinside.iter().advance |&idx| {
                    if idx > 0 { blk.truncate(idx + 1); }
                }

                let last = &mut blk[blk.len() - 1];
                last.state = Outside;
            }

            (_, _) => {}
        }
    }

    // Poor BGA defined by #BMP00 wouldn't be played if it is a movie. We can't just let it
    // played at the beginning of the chart as the "beginning" is not always 0.0 (actually,
    // `originoffset`). Thus we add an artificial BGA object at time 0.0 only when the other
    // poor BGA does not exist at this position. (C: `poorbgafix`)
    let mut poorbgafix = true;

    // Indices to last visible object per channels. A marker specified by #LNOBJ will turn
    // this last object to the start of LN. (C: `prev12`)
    let mut lastvis: [Option<Mark>, ..NLANES] = [None, ..NLANES];

    // Indices to last LN start or end inserted (and not finalized yet) per channels.
    // If `consecutiveln` is on (#LNTYPE 2), the position of referenced object gets updated
    // during parsing; if off (#LNTYPE 1), it is solely used for checking if we are inside
    // the LN or not. (C: `prev56`)
    let mut lastln: [Option<Mark>, ..NLANES] = [None, ..NLANES];

    // Handles a non-00 alphanumeric key `v` positioned at the particular channel `chan` and
    // particular position `t`. The position `t2` next to `t` is used for some cases that
    // an alphanumeric key designates an area rather than a point.
    let handle_key = |chan: Key, t: float, t2: float, v: Key| {
        match *chan {
            // channel #01: BGM
            1 => { builder.add(t, BGM(SoundRef(v))); }

            // channel #03: BPM as an hexadecimal key
            3 => {
                let v = v.to_hex(); // XXX #3511
                for v.iter().advance |&v| {
                    builder.add(t, SetBPM(BPM(v as float)))
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
                    for lastvispos.iter().advance |&mark| {
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
                    1..200 => Some(GaugeDamage(*v as float / 200.0)),
                    1295 => Some(InstantDeath), // XXX 1295=MAXKEY-1
                    _ => None
                };
                for damage.iter().advance |&damage| {
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
    for bmsline.iter().advance |line| {
        let measure = line.measure as float;
        let data: ~[char] = line.data.iter().collect();
        let max = data.len() / 2 * 2;
        let count = max as float;
        for uint::range_step(0, max, 2) |i| {
            let v = Key::from_chars(data.slice(i, i+2));
            for v.iter().advance |&v| {
                if v != Key(0) { // ignores 00
                    let t = measure + i as float / count;
                    let t2 = measure + (i + 2) as float / count;
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
    let endt = nmeasures as float;
    for uint::range(0, NLANES) |i| {
        if lastvis[i].is_some() || (!consecutiveln && lastln[i].is_some()) {
            builder.add(endt, LNDone(Lane(i), None));
        }
    }

    // convert shortens to objects and insert measure bars
    shortens.grow_set(nmeasures, &1.0, 1.0); // so we always have a normal measure at the end
    let mut prevfactor = 1.0;
    for shortens.iter().enumerate().advance |(measure, &factor)| {
        builder.add(measure as float, MeasureBar);
        if prevfactor != factor {
            builder.add(measure as float, SetMeasureFactor(factor));
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

/// Reads and parses the BMS file with given RNG. (C: `parse_bms`)
pub fn parse_bms<R:RngUtil>(bmspath: &str, r: &mut R) -> Result<Bms,~str> {
    do ::std::io::file_reader(&Path(bmspath)).chain |f| {
        do parse_bms_from_reader(f, r).map |&bms| {
            let mut bms = bms;
            bms.bmspath = bmspath.to_owned();
            bms
        }
    }
}

