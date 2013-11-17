// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS loader. Uses a BMS parser (`format::bms::parse`) to produce `format::bms::Bms` structure.

use std::{vec, iter, io, cmp};
use std::rand::Rng;

use format::obj::*;
use format::bms::parse;
use format::bms::types::{Key, MAXKEY};
use format::bms::diag::*;
use format::bms::{ImageRef, SoundRef, DefaultBPM, BmsMeta, Bms};
use format::bms::{SinglePlay, CouplePlay, DoublePlay, BattlePlay, Difficulty};

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

/// An unprocessed data line of BMS file.
#[deriving(Clone)]
struct BmsLine {
    measure: uint,
    chan: Key,
    data: ~str,
    lineno: uint,
}

impl Ord for BmsLine {
    fn lt(&self, other: &BmsLine) -> bool {
        (self.measure, self.chan) < (other.measure, other.chan)
    }
}

/// Reads the BMS file with given RNG from given reader. Diagnostic messages are sent via callback.
pub fn load_bms_from_reader<R:Rng,Listener:BmsMessageListener>(
                                f: @io::Reader, r: &mut R, opts: &BmsLoaderOptions,
                                callback: &mut Listener) -> Result<Bms,~str> {
    use format::timeline::builder::{TimelineBuilder, Mark};

    let mut encoding = ("ascii", 0.0);

    let mut title = None;
    let mut subtitles = ~[];
    let mut genre = None;
    let mut artist = None;
    let mut subartists = ~[];
    let mut comments = ~[];
    let mut stagefile = None;
    let mut banner = None;
    let mut basepath = None;
    let mut mode = SinglePlay;
    let mut playlevel = 0;
    let mut difficulty = None;
    let mut rank = 2;
    let /*mut*/ canvassize = (256.0, 256.0);
    let mut sndpath = vec::from_elem(MAXKEY as uint, None);
    let mut imgpath = vec::from_elem(MAXKEY as uint, None);
    let mut imgslices = vec::from_elem(MAXKEY as uint, None);

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

    let mut callback_ = |line, msg: BmsMessage| {
        match msg {
            // we intercept this internal diagnostic to set the relevant fields in `Bms`
            BmsUsesEncoding(encname, confidence) => {
                encoding = (encname, confidence);
                if confidence > 1.0 || "ascii".equiv(&encname) || "utf-8".equiv(&encname) {
                    true
                } else {
                    callback.on_message(line, BmsUsesLegacyEncoding)
                }
            },
            msg => callback.on_message(line, msg),
        }
    };

    let mut ret = true;
    do parse::each_bms_command(f, r, &opts.parser, &mut callback_) |lineno, cmd| {
        macro_rules! diag(
            ($e:expr) => (
                if !callback.on_message(None, $e) { ret = false; }
            );
            ($e:expr at $line:expr) => (
                if !callback.on_message(Some($line), $e) { ret = false; }
            )
        )

        match cmd {
            parse::BmsTitle(s) => {
                let s = s.into_owned();
                if s.is_empty() { diag!(BmsHasEmptyTITLE at lineno); }
                if title.is_some() { diag!(BmsHasMultipleTITLEs at lineno); }
                title = Some(s);
            }
            parse::BmsSubtitle(s) => {
                subtitles.push(s.into_owned());
            }
            parse::BmsGenre(s) => {
                let s = s.into_owned();
                if s.is_empty() { diag!(BmsHasEmptyGENRE at lineno); }
                if genre.is_some() { diag!(BmsHasMultipleGENREs at lineno); }
                genre = Some(s);
            }
            parse::BmsArtist(s) => {
                let s = s.into_owned();
                if s.is_empty() { diag!(BmsHasEmptyARTIST at lineno); }
                if artist.is_some() { diag!(BmsHasMultipleARTISTs at lineno); }
                artist = Some(s);
            }
            parse::BmsSubartist(s) => {
                subartists.push(s.into_owned());
            }
            parse::BmsComment(s) => {
                let mut s_ = s.as_slice().trim();
                if s_.starts_with("\"") && s_.ends_with("\"") { // strip quotes
                    s_ = s_.slice(1, s_.len()-1);
                }
                comments.push(s_.to_owned());
            }

            parse::BmsStageFile(s) => {
                stagefile = Some(s.into_owned());
            }
            parse::BmsBanner(s) => {
                banner = Some(s.into_owned());
            }

            parse::BmsBPM(bpm) => {
                if *bpm < 0.0 {
                    diag!(BmsHasNegativeInitBPM at lineno);
                } else if *bpm == 0.0 {
                    diag!(BmsHasZeroInitBPM at lineno);
                } else {
                    builder.set_initbpm(bpm);
                }
            }
            parse::BmsExBPM(Key(i), bpm) => {
                if *bpm <= 0.0 {
                    diag!(BmsHasNonpositiveBPM at lineno);
                } else {
                    bpmtab[i] = bpm;
                }
            }

            parse::BmsPlayer(1) => { mode = SinglePlay; }
            parse::BmsPlayer(2) => { mode = CouplePlay; diag!(BmsUsesCouplePlay at lineno); }
            parse::BmsPlayer(3) => { mode = DoublePlay; }
            parse::BmsPlayer(4) => { mode = BattlePlay; diag!(BmsUsesBattlePlay at lineno); }
            parse::BmsPlayer(_) => { diag!(BmsHasInvalidPLAYER at lineno); }

            parse::BmsPlayLevel(v) => {
                if v < 0 { diag!(BmsHasNegativePLAYLEVEL at lineno); }
                playlevel = v;
            }
            parse::BmsDifficulty(v) => {
                if v < 1 || v > 5 { diag!(BmsHasDIFFICULTYOutOfRange at lineno); }
                difficulty = Some(Difficulty(v));
            }
            parse::BmsRank(v) => {
                rank = v;
            }

            parse::BmsLNType(1) => { consecutiveln = false; }
            parse::BmsLNType(2) => { consecutiveln = true; diag!(BmsUsesLNTYPE2 at lineno); }
            parse::BmsLNType(_) => { diag!(BmsHasInvalidLNTYPE at lineno); }

            parse::BmsLNObj(key) => {
                if lnobj.is_some() { diag!(BmsHasMultipleLNOBJs at lineno); }
                if *key == 0 {
                    diag!(BmsHasZeroLNOBJ at lineno);
                } else {
                    lnobj = Some(key);
                }
            }

            parse::BmsWAV(Key(i), s) => { sndpath[i] = Some(s.into_owned()); }
            parse::BmsBMP(Key(i), s) => { imgpath[i] = Some(s.into_owned()); }
            parse::BmsBGA(Key(i), Key(j), slice) => {
                imgslices[i] = Some((ImageRef(Key(j)), slice));
            }

            parse::BmsStop(Key(i), dur) => {
                if dur.sign() < 0 {
                    diag!(BmsHasNegativeSTOPDuration at lineno);
                } else {
                    stoptab[i] = dur;
                }
            }
            parse::BmsStp(pos, dur) => {
                if dur.sign() < 0 {
                    diag!(BmsHasNegativeSTPDuration at lineno);
                } else {
                    builder.add(pos, Stop(dur));
                }
            }

            parse::BmsPathWAV(s) => {
                // TODO this logic assumes that #PATH_WAV is always interpreted as a native path,
                // which the C version doesn't assume. this difference barely makes the practical
                // issue though (#PATH_WAV is not expected to be used in the wild).
                basepath = Some(Path(s.as_slice()));
            }

            parse::BmsShorten(measure, factor) => {
                if factor > 0.0 {
                    shortens.grow_set(measure, &1.0, factor);
                }
            }
            parse::BmsData(measure, chan, data) => {
                bmsline.push(BmsLine { measure: measure, chan: chan,
                                       data: data.into_owned(), lineno: lineno })
            }

            parse::BmsFlow(_) => { fail!("unexpected"); }
            _ => {}
        }
        ret
    };
    if !ret { return Err(~"aborted"); }

    macro_rules! diag(
        ($e:expr) => (
            if !callback.on_message(None, $e) { return Err(~"aborted"); }
        );
        ($e:expr at $line:expr) => (
            if !callback.on_message(Some($line), $e) { return Err(~"aborted"); }
        )
    )

    if title.is_none() { diag!(BmsHasNoTITLE); }
    if artist.is_none() { diag!(BmsHasNoARTIST); }
    if genre.is_none() { diag!(BmsHasNoGENRE); }

    // clip the image slices if needed.
    for slice in imgslices.mut_iter() {
        match slice {
            &Some((_, ref mut slice)) => {
                slice.sx = cmp::max(slice.sx, 0.0);
                slice.sy = cmp::max(slice.sy, 0.0);
                slice.w = cmp::min(cmp::max(slice.w, 0.0), canvassize.n0());
                slice.h = cmp::min(cmp::max(slice.h, 0.0), canvassize.n1());
            }
            &None => {}
        }
    }

    // ----8<----

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

    // Replaces the reference to #BGA/#@BGA keys into a pair of the reference to #BMP keys
    // and corresponding `ImageSlice`, if possible.
    let imgref_to_bgaref = |iref: ImageRef| -> BGARef<ImageRef> {
        match imgslices[**iref] {
            Some((iref, ref slice)) => SlicedImageBGA(iref.clone(), ~slice.clone()),
            None => ImageBGA(iref),
        }
    };

    // Handles a non-00 alphanumeric key `v` positioned at the particular channel `chan` and
    // particular position `t`. The position `t2` next to `t` is used for some cases that
    // an alphanumeric key designates an area rather than a point.
    let handle_key = |chan: Key, t: f64, t2: f64, v: Key, _lineno: uint| -> bool {
        let /*mut*/ ret = true;
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
            4 => { builder.add(t, SetBGA(Layer1, imgref_to_bgaref(ImageRef(v)))); }

            // channel #06: POOR BGA
            6 => {
                builder.add(t, SetBGA(PoorBGA, imgref_to_bgaref(ImageRef(v))));
                poorbgafix = false; // we don't add artificial BGA
            }

            // channel #07: BGA layer 2
            7 => { builder.add(t, SetBGA(Layer2, imgref_to_bgaref(ImageRef(v)))); }

            // channel #08: BPM defined by #BPMxx
            8 => { builder.add(t, SetBPM(bpmtab[*v])); }

            // channel #09: scroll stopper defined by #STOPxx
            9 => { builder.add(t, Stop(stoptab[*v])); }

            // channel #0A: BGA layer 3
            10 => { builder.add(t, SetBGA(Layer3, imgref_to_bgaref(ImageRef(v)))); }

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
        ret
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
                    if !handle_key(line.chan, t, t2, v, line.lineno) {
                        return Err(~"aborted");
                    }
                }
            }
        }
    }

    // insert an artificial `SetBGA` object at 0.0 if required
    if poorbgafix {
        builder.add(0.0, SetBGA(PoorBGA, imgref_to_bgaref(ImageRef(Key(0)))));
    }

    // fix the unterminated longnote
    let nmeasures = bmsline.last_opt().map_default(0, |l| l.measure) + 1;
    let endt = nmeasures as f64;
    for i in range(0, NLANES) {
        if lastvis[i].is_some() || (!consecutiveln && lastln[i].is_some()) {
            builder.add(endt, LNDone(Lane(i), None));
        }
    }

    // convert shortens to objects and insert measure bars.
    // since shortens are properties of the axis and not of the chart itself,
    // it is possible that some shortens can be placed after the end of chart.
    // therefore we ignore any shortens after the logical end of the chart.
    shortens.grow(nmeasures + 1, &1.0); // we would have SetMeasureFactor up to nmeasures
    let mut prevfactor = 1.0;
    for (measure, &factor) in shortens.slice_to(nmeasures + 1).iter().enumerate() {
        builder.add(measure as f64, MeasureBar);
        if prevfactor != factor {
            builder.add(measure as f64, SetMeasureFactor(factor));
            prevfactor = factor;
        }
    }

    // set the end of the chart (no measure bar at this position)
    builder.set_end(endt + 1.0);

    let timeline = builder.build();
    Ok(Bms { bmspath: None,
             meta: BmsMeta { encoding: encoding, title: title, subtitles: subtitles, genre: genre,
                             artist: artist, subartists: subartists, comments: comments,
                             stagefile: stagefile, banner: banner, basepath: basepath, mode: mode,
                             playlevel: playlevel, difficulty: difficulty, rank: rank,
                             sndpath: sndpath, imgpath: imgpath },
             timeline: timeline })
}

/// Reads the BMS file with given RNG.
pub fn load_bms<R:Rng,Listener:BmsMessageListener>(
                                bmspath: &Path, r: &mut R, opts: &BmsLoaderOptions,
                                callback: &mut Listener) -> Result<Bms,~str> {
    do io::file_reader(bmspath).and_then |f| {
        do load_bms_from_reader(f, r, opts, callback).map_move |bms| {
            let mut bms = bms;
            bms.bmspath = Some(bmspath.clone());
            if bms.meta.basepath.is_none() {
                let basepath = bmspath.dir_path();
                // Rust: `Path("")` is not same as `Path(".")` but `Path::dir_path` may produce it!
                let basepath = if basepath == Path("") {Path(".")} else {basepath};
                bms.meta.basepath = Some(basepath);
            }
            bms
        }
    }
}

