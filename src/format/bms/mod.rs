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

use std::{int, uint, float, vec, cmp};
use std::rand::*;

pub use format::obj::{Lane, NLANES};
pub use format::obj::{BGALayer, Layer1, Layer2, Layer3, PoorBGA, NLAYERS};
pub use format::obj::{BPM};
pub use format::obj::{Duration, Seconds, Measures};
pub use format::obj::{Damage, GaugeDamage, InstantDeath};
pub use format::obj::{ObjData, Deleted, Visible, Invisible, LNStart, LNDone, Bomb, BGM, SetBGA,
                      SetBPM, Stop};
pub use format::obj::{ObjQueryOps, ObjConvOps, Obj};
pub use format::bms::types::{Key, MAXKEY};

pub mod types;
pub mod parse;
pub mod pointer;

//----------------------------------------------------------------------------------------------
// BMS data

/// Sound reference.
#[deriving(Eq)]
pub struct SoundRef(Key);

/// Image reference.
#[deriving(Eq)]
pub struct ImageRef(Key);

/// BMS-specific object. Both the sound and the image are referenced in the alphanumeric keys.
pub type BmsObj = Obj<SoundRef,ImageRef>;

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

/// Loaded BMS data. It is not a global state unlike C.
pub struct Bms {
    /// A path to the BMS file. Also used for finding the resource when `basepath` is not set.
    /// (C: `bmspath`)
    bmspath: ~str,

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

    /// Initial BPM. (C: `initbpm`)
    initbpm: BPM,
    /// Paths to sound file relative to `basepath` or BMS file. (C: `sndpath`)
    //
    // Rust: constant expression in the array size is unsupported.
    sndpath: [Option<~str>, ..MAXKEY],
    /// Paths to image/movie file relative to `basepath` or BMS file. (C: `imgpath`)
    imgpath: [Option<~str>, ..MAXKEY],
    /// List of blit commands to be executed after `imgpath` is loaded. (C: `blitcmd`)
    blitcmd: ~[BlitCmd],

    /// List of objects sorted by the position. (C: `objs`)
    objs: ~[BmsObj],
    /// The scaling factor of measures. Defaults to 1.0. (C: `shortens`)
    shortens: ~[float],
    /// The number of measures after the origin, i.e. the length of the BMS file. The play stops
    /// after the last measure. (C: `length`)
    nmeasures: uint
}

/// Creates a default value of BMS data.
pub fn Bms() -> Bms {
    Bms { bmspath: ~"", title: None, genre: None, artist: None, stagefile: None, basepath: None,
          player: SinglePlay, playlevel: 0, rank: 2, initbpm: DefaultBPM,
          sndpath: [None, ..MAXKEY], imgpath: [None, ..MAXKEY], blitcmd: ~[], objs: ~[],
          shortens: ~[], nmeasures: 0 }
}

impl Bms {
    /// Returns a scaling factor of given measure number. The default scaling factor is 1.0, and
    /// that value applies to any out-of-bound measures. (C: `shorten`)
    pub fn shorten(&self, measure: int) -> float {
        if measure < 0 || measure as uint >= self.shortens.len() {
            1.0
        } else {
            self.shortens[measure as uint]
        }
    }

    /// Calculates the virtual time that is `offset` measures away from the virtual time `base`.
    /// This takes account of the scaling factor, so if first four measures are scaled by 1/4,
    /// then `adjust_object_time(0.0, 2.0)` results in `5.0`. (C: `adjust_object_time`)
    pub fn adjust_object_time(&self, base: float, offset: float) -> float {
        let basemeasure = base.floor() as int;
        let baseshorten = self.shorten(basemeasure);
        let basefrac = base - basemeasure as float;
        let tonextmeasure = (1.0 - basefrac) * baseshorten;
        if offset < tonextmeasure {
            base + offset / baseshorten
        } else {
            let mut offset = offset - tonextmeasure;
            let mut i = basemeasure + 1;
            let mut curshorten = self.shorten(i);
            while offset >= curshorten {
                offset -= curshorten;
                i += 1;
                curshorten = self.shorten(i);
            }
            i as float + offset / curshorten
        }
    }

    /// Calculates an adjusted offset between the virtual time `base` and `base + offset`.
    /// This takes account of the measure scaling factor, so for example, the adjusted offset
    /// between the virtual time 0.0 and 2.0 is, if the measure #000 is scaled by 1.2x,
    /// 2.2 measures instead of 2.0 measures. (C: `adjust_object_position`)
    pub fn adjust_object_position(&self, base: float, time: float) -> float {
        let basemeasure = base.floor() as int;
        let timemeasure = time.floor() as int;
        let basefrac = base - basemeasure as float;
        let timefrac = time - timemeasure as float;
        let mut pos = timefrac * self.shorten(timemeasure) -
                      basefrac * self.shorten(basemeasure);
        for int::range(basemeasure, timemeasure) |i| {
            pos += self.shorten(i);
        }
        pos
    }
}

//----------------------------------------------------------------------------------------------
// parsing

/// Reads and parses the BMS file with given RNG from given reader.
pub fn parse_bms_from_reader<R:RngUtil>(f: @::std::io::Reader, r: &mut R) -> Result<Bms,~str> {
    let mut bms = Bms();

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

    // A list of unprocessed data lines. They have to be sorted with a stable algorithm and
    // processed in the order of measure number. (C: `bmsline`)
    let mut bmsline = ~[];
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
            (parse::BmsTitle(s),           false) => { bms.title = Some(s.to_owned()); }
            (parse::BmsGenre(s),           false) => { bms.genre = Some(s.to_owned()); }
            (parse::BmsArtist(s),          false) => { bms.artist = Some(s.to_owned()); }
            (parse::BmsStageFile(s),       false) => { bms.stagefile = Some(s.to_owned()); }
            (parse::BmsPathWAV(s),         false) => { bms.basepath = Some(s.to_owned()); }
            (parse::BmsBPM(bpm),           false) => { bms.initbpm = bpm; }
            (parse::BmsExBPM(Key(i), bpm), false) => { bpmtab[i] = bpm; }
            (parse::BmsPlayer(v),          false) => { bms.player = v; }
            (parse::BmsPlayLevel(v),       false) => { bms.playlevel = v; }
            (parse::BmsRank(v),            false) => { bms.rank = v; }
            (parse::BmsLNType(lntype),     false) => { consecutiveln = (lntype == 2); }
            (parse::BmsLNObj(key),         false) => { lnobj = Some(key); }
            (parse::BmsWAV(Key(i), s),     false) => { bms.sndpath[i] = Some(s.to_owned()); }
            (parse::BmsBMP(Key(i), s),     false) => { bms.imgpath[i] = Some(s.to_owned()); }
            (parse::BmsBGA(bc),            false) => { bms.blitcmd.push(bc); }
            (parse::BmsStop(Key(i), dur),  false) => { stoptab[i] = dur; }
            (parse::BmsStp(pos, dur),      false) => { bms.objs.push(Obj::Stop(pos, dur)); }

            (parse::BmsShorten(measure, shorten), false) => {
                if shorten > 0.001 {
                    bms.shortens.grow_set(measure, &1.0, shorten);
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
    let mut lastvis: [Option<uint>, ..NLANES] = [None, ..NLANES];

    // Indices to last LN start or end inserted (and not finalized yet) per channels.
    // If `consecutiveln` is on (#LNTYPE 2), the position of referenced object gets updated
    // during parsing; if off (#LNTYPE 1), it is solely used for checking if we are inside
    // the LN or not. (C: `prev56`)
    let mut lastln: [Option<uint>, ..NLANES] = [None, ..NLANES];

    // Handles a non-00 alphanumeric key `v` positioned at the particular channel `chan` and
    // particular position `t`. The position `t2` next to `t` is used for some cases that
    // an alphanumeric key designates an area rather than a point.
    let handle_key = |chan: Key, t: float, t2: float, v: Key| {
        // Adds an object. Objects are sorted by its position later.
        let add = |obj: BmsObj| { bms.objs.push(obj); };
        // Adds an object and returns its position. LN parsing generally mutates the existing
        // object for simplicity.
        let mark = |obj: BmsObj| -> Option<uint> {
            let marked = bms.objs.len();
            bms.objs.push(obj);
            Some(marked)
        };

        match *chan {
            // channel #01: BGM
            1 => { add(Obj::BGM(t, SoundRef(v))); }

            // channel #03: BPM as an hexadecimal key
            3 => {
                let v = v.to_hex(); // XXX #3511
                for v.iter().advance |&v| {
                    add(Obj::SetBPM(t, BPM(v as float)))
                }
            }

            // channel #04: BGA layer 1
            4 => { add(Obj::SetBGA(t, Layer1, Some(ImageRef(v)))); }

            // channel #06: POOR BGA
            6 => {
                add(Obj::SetBGA(t, PoorBGA, Some(ImageRef(v))));
                poorbgafix = false; // we don't add artificial BGA
            }

            // channel #07: BGA layer 2
            7 => { add(Obj::SetBGA(t, Layer2, Some(ImageRef(v)))); }

            // channel #08: BPM defined by #BPMxx
            8 => { add(Obj::SetBPM(t, bpmtab[*v])); } // TODO bpmtab validity check

            // channel #09: scroll stopper defined by #STOPxx
            9 => { add(Obj::Stop(t, stoptab[*v])); } // TODO stoptab validity check

            // channel #0A: BGA layer 3
            10 => { add(Obj::SetBGA(t, Layer3, Some(ImageRef(v)))); }

            // channels #1x/2x: visible object, possibly LNs when #LNOBJ is in active
            36/*1*36*/..107/*3*36-1*/ => {
                let lane = chan.to_lane();
                if lnobj.is_some() && lnobj == Some(v) {
                    // change the last inserted visible object to the start of LN if any.
                    let lastvispos = lastvis[*lane];
                    for lastvispos.iter().advance |&pos| {
                        assert!(bms.objs[pos].is_visible());
                        bms.objs[pos] = bms.objs[pos].to_lnstart();
                        add(Obj::LNDone(t, lane, Some(SoundRef(v))));
                        lastvis[*lane] = None;
                    }
                } else {
                    lastvis[*lane] = mark(Obj::Visible(t, lane, Some(SoundRef(v))));
                }
            }

            // channels #3x/4x: invisible object
            108/*3*36*/..179/*5*36-1*/ => {
                let lane = chan.to_lane();
                add(Obj::Invisible(t, lane, Some(SoundRef(v))));
            }

            // channels #5x/6x, #LNTYPE 1: LN endpoints
            180/*5*36*/..251/*7*36-1*/ if !consecutiveln => {
                let lane = chan.to_lane();

                // a pair of non-00 alphanumeric keys designate one LN. if there are an odd
                // number of them, the last LN is implicitly closed later.
                if lastln[*lane].is_some() {
                    lastln[*lane] = None;
                    add(Obj::LNDone(t, lane, Some(SoundRef(v))));
                } else {
                    lastln[*lane] = mark(Obj::LNStart(t, lane, Some(SoundRef(v))));
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
                    Some(pos) if bms.objs[pos].time == t => {
                        assert!(bms.objs[pos].is_lndone());
                        bms.objs[pos].time = t2;
                    }
                    _ => {
                        add(Obj::LNStart(t, lane, Some(SoundRef(v))));
                        lastln[*lane] = mark(Obj::LNDone(t2, lane, Some(SoundRef(v))));
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
                    add(Obj::Bomb(t, lane, Some(SoundRef(Key(0))), damage));
                }
            }

            // unsupported: channels #0B/0C/0D/0E (BGA opacity), #97/98 (sound volume),
            // #99 (text), #A0 (dynamic #RANK), #A1/A2/A3/A4 (BGA color key update),
            // #A5 (BGA on keypress), #A6 (player-specific option)
            _ => {}
        }
    };

    // loops over the sorted bmslines
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

    if poorbgafix {
        bms.objs.push(Obj::SetBGA(0.0, PoorBGA, Some(ImageRef(Key(0)))));
    }

    // fix the unterminated longnote
    bms.nmeasures = bmsline.last_opt().map_default(0, |l| l.measure) + 1;
    let endt = bms.nmeasures as float;
    for uint::range(0, NLANES) |i| {
        if lastvis[i].is_some() || (!consecutiveln && lastln[i].is_some()) {
            bms.objs.push(Obj::LNDone(endt, Lane(i), None));
        }
    }

    Ok(bms)
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

//----------------------------------------------------------------------------------------------
// post-processing

/// Updates the object in place to BGM or placeholder. (C: `remove_or_replace_note`)
pub fn remove_or_replace_note(obj: &mut BmsObj) {
    obj.data = match obj.data {
        Visible(_,Some(sref)) | Invisible(_,Some(sref)) |
        LNStart(_,Some(sref)) | LNDone(_,Some(sref)) => BGM(sref),
        _ => Deleted
    };
}

/// Fixes a problematic data. (C: `sanitize_bms`)
pub fn sanitize_bms(bms: &mut Bms) {
    ::extra::sort::tim_sort(bms.objs);

    fn sanitize(objs: &mut [BmsObj], to_type: &fn(&BmsObj) -> Option<uint>,
                merge_types: &fn(uint) -> uint) {
        let len = objs.len();
        let mut i = 0;
        while i < len {
            let cur = objs[i].time;
            let mut types = 0;
            let mut j = i;
            while j < len && objs[j].time <= cur {
                let obj = &mut objs[j];
                let ty = to_type(obj); // XXX #3511
                for ty.iter().advance |&t| {
                    if (types & (1 << t)) != 0 {
                        // duplicate type
                        remove_or_replace_note(obj);
                    } else {
                        types |= 1 << t;
                    }
                }
                j += 1;
            }

            types = merge_types(types);

            while i < j {
                let obj = &mut objs[i];
                let ty = to_type(obj); // XXX #3511
                for ty.iter().advance |&t| {
                    if (types & (1 << t)) == 0 {
                        remove_or_replace_note(obj);
                    }
                }
                i += 1;
            }
        }
    }

    for uint::range(0, NLANES) |lane| {
        let lane0 = Lane(lane);

        static LNDONE: uint = 0;
        static LNSTART: uint = 1;
        static VISIBLE: uint = 2;
        static INVISIBLE: uint = 3;
        static BOMB: uint = 4;
        let to_type = |obj: &BmsObj| -> Option<uint> {
            match obj.data {
                Visible(lane,_) if lane == lane0 => Some(VISIBLE),
                Invisible(lane,_) if lane == lane0 => Some(INVISIBLE),
                LNStart(lane,_) if lane == lane0 => Some(LNSTART),
                LNDone(lane,_) if lane == lane0 => Some(LNDONE),
                Bomb(lane,_,_) if lane == lane0 => Some(BOMB),
                _ => None,
            }
        };

        let mut inside = false;
        do sanitize(bms.objs, |obj| to_type(obj)) |mut types| { // XXX #7363
            static LNMASK: uint = (1 << LNSTART) | (1 << LNDONE);

            // remove overlapping LN endpoints altogether
            if (types & LNMASK) == LNMASK { types &= !LNMASK; }

            // remove prohibited types according to inside
            if inside {
                types &= !((1 << LNSTART) | (1 << VISIBLE) | (1 << BOMB));
            } else {
                types &= !(1 << LNDONE);
            }

            // invisible note cannot overlap with long note endpoints
            if (types & LNMASK) != 0 { types &= !(1 << INVISIBLE); }

            // keep the most important (lowest) type, except for
            // BOMB/INVISIBLE combination
            let lowest = types & -types;
            if lowest == (1 << INVISIBLE) {
                types = lowest | (types & (1 << BOMB));
            } else {
                types = lowest;
            }

            if (types & (1 << LNSTART)) != 0 {
                inside = true;
            } else if (types & (1 << LNDONE)) != 0 {
                inside = false;
            }

            types
        }

        if inside {
            // remove last starting longnote which is unfinished
            match bms.objs.rposition(|obj| to_type(obj).is_some()) {
                Some(pos) if bms.objs[pos].is_lnstart() =>
                    remove_or_replace_note(&mut bms.objs[pos]),
                _ => {}
            }
        }
    }

    sanitize(bms.objs,
             |&obj| match obj.data {
                        SetBGA(Layer1,_) => Some(0),
                        SetBGA(Layer2,_) => Some(1),
                        SetBGA(Layer3,_) => Some(2),
                        SetBGA(PoorBGA,_) => Some(3),
                        SetBPM(*) => Some(4),
                        Stop(*) => Some(5),
                        _ => None,
                    },
             |types| types);
}

//----------------------------------------------------------------------------------------------
// analysis

/// Derived BMS information. Again, this is not a global state.
pub struct BmsInfo {
    /// The start position of the BMS file. This is either -1.0 or 0.0 depending on the first
    /// measure has any visible objects or not. (C: `originoffset`)
    originoffset: float,
    /// Set to true if the BMS file has a BPM change. (C: `hasbpmchange`)
    hasbpmchange: bool,
    /// Set to true if the BMS file has long note objects. (C: `haslongnote`)
    haslongnote: bool,
    /// The number of visible objects in the BMS file. A long note object counts as one object.
    /// (C: `nnotes`)
    nnotes: int,
    /// The maximum possible score. (C: `maxscore`)
    maxscore: int
}

/// Analyzes the loaded BMS file. (C: `analyze_and_compact_bms`)
pub fn analyze_bms(bms: &Bms) -> BmsInfo {
    let mut infos = BmsInfo { originoffset: 0.0, hasbpmchange: false, haslongnote: false,
                              nnotes: 0, maxscore: 0 };

    for bms.objs.iter().advance |&obj| {
        infos.haslongnote |= obj.is_lnstart();
        infos.hasbpmchange |= obj.is_setbpm();

        if obj.is_lnstart() || obj.is_visible() {
            infos.nnotes += 1;
            if obj.time < 1.0 { infos.originoffset = -1.0; }
        }
    }

    for int::range(0, infos.nnotes) |i| {
        let ratio = (i as float) / (infos.nnotes as float);
        infos.maxscore += (300.0 * (1.0 + ratio)) as int;
    }

    infos
}

/// Calculates the duration of the loaded BMS file in seconds. `sound_length` should return
/// the length of sound resources in seconds or 0.0. (C: `get_bms_duration`)
pub fn bms_duration(bms: &Bms, originoffset: float,
                    sound_length: &fn(SoundRef) -> float) -> float {
    let mut pos = originoffset;
    let mut bpm = bms.initbpm;
    let mut time = 0.0;
    let mut sndtime = 0.0;

    for bms.objs.iter().advance |&obj| {
        let delta = bms.adjust_object_position(pos, obj.time);
        time += bpm.measure_to_msec(delta);
        match obj.data {
            Visible(_,Some(sref)) | LNStart(_,Some(sref)) | BGM(sref) => {
                sndtime = cmp::max(sndtime, time + sound_length(sref) * 1000.0);
            }
            SetBPM(BPM(newbpm)) => {
                if newbpm > 0.0 {
                    bpm = BPM(newbpm);
                } else if newbpm < 0.0 {
                    bpm = BPM(newbpm);
                    let delta = bms.adjust_object_position(originoffset, pos);
                    time += BPM(-newbpm).measure_to_msec(delta);
                    break;
                }
            }
            Stop(duration) => {
                time += duration.to_msec(bpm);
            }
            _ => {}
        }
        pos = obj.time;
    }

    if *bpm > 0.0 { // the chart scrolls backwards to `originoffset` for negative BPM
        let delta = bms.adjust_object_position(pos, (bms.nmeasures + 1) as float);
        time += bpm.measure_to_msec(delta);
    }
    cmp::max(time, sndtime) / 1000.0
 }

//----------------------------------------------------------------------------------------------
// modifiers

/// Applies a function to the object lane if any. This is used to shuffle the lanes without
/// modifying the relative time position.
fn update_object_lane(obj: &mut BmsObj, f: &fn(Lane) -> Lane) {
    obj.data = match obj.data {
        Visible(lane,sref) => Visible(f(lane),sref),
        Invisible(lane,sref) => Invisible(f(lane),sref),
        LNStart(lane,sref) => LNStart(f(lane),sref),
        LNDone(lane,sref) => LNDone(f(lane),sref),
        Bomb(lane,sref,damage) => Bomb(f(lane),sref,damage),
        objdata => objdata
    };
}

/// Swaps given lanes in the reverse order. (C: `shuffle_bms` with `MIRROR_MODF`)
pub fn apply_mirror_modf(bms: &mut Bms, lanes: &[Lane]) {
    let mut map = vec::from_fn(NLANES, |lane| Lane(lane));
    let rlanes = vec::reversed(lanes);
    let assocs = vec::zip_slice(lanes, rlanes); // XXX #3511
    for assocs.iter().advance |&(Lane(from), to)| {
        map[from] = to;
    }

    for bms.objs.mut_iter().advance |obj| {
        update_object_lane(obj, |Lane(lane)| map[lane]);
    }
}

/// Swaps given lanes in the random order. (C: `shuffle_bms` with
/// `SHUFFLE_MODF`/`SHUFFLEEX_MODF`)
pub fn apply_shuffle_modf<R:RngUtil>(bms: &mut Bms, r: &mut R, lanes: &[Lane]) {
    let shuffled = r.shuffle(lanes);
    let mut map = vec::from_fn(NLANES, |lane| Lane(lane));
    let assocs = vec::zip_slice(lanes, shuffled); // XXX #3511
    for assocs.iter().advance |&(Lane(from), to)| {
        map[from] = to;
    }

    for bms.objs.mut_iter().advance |obj| {
        update_object_lane(obj, |Lane(lane)| map[lane]);
    }
}

/// Swaps given lanes in the random order, where the order is determined per object.
/// `bms` should be first sanitized by `sanitize_bms`. It does not cause objects to move within
/// another LN object, or place two objects in the same or very close time position to the same
/// lane. (C: `shuffle_bms` with `RANDOM_MODF`/`RANDOMEX_MODF`)
pub fn apply_random_modf<R:RngUtil>(bms: &mut Bms, r: &mut R, lanes: &[Lane]) {
    let mut movable = lanes.to_owned();
    let mut map = vec::from_fn(NLANES, |lane| Lane(lane));

    let mut lasttime = float::neg_infinity;
    for bms.objs.mut_iter().advance |obj| {
        if obj.is_lnstart() {
            let lane = obj.object_lane().get();
            match movable.position_elem(&lane) {
                Some(i) => { movable.swap_remove(i); }
                None => fail!(~"non-sanitized BMS detected")
            }
        }
        if lasttime < obj.time { // reshuffle required
            lasttime = obj.time + 1e-4;
            let shuffled = r.shuffle(movable);
            let assocs = vec::zip_slice(movable, shuffled); // XXX #3511
            for assocs.iter().advance |&(Lane(from), to)| {
                map[from] = to;
            }
        }
        if obj.is_lnstart() {
            let lane = obj.object_lane().get();
            movable.push(lane);
        }
        update_object_lane(obj, |Lane(lane)| map[lane]);
    }
}

//----------------------------------------------------------------------------------------------

