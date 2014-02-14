// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS loader. Uses a BMS parser (`format::bms::parse`) to produce `format::bms::Bms` structure.

use std::{vec, iter, cmp};
use std::rand::Rng;

use format::obj::*;
use format::bms::parse;
use format::bms::types::{Key, MAXKEY};
use format::bms::diag::*;
use format::bms::{ImageRef, SoundRef, DefaultBPM, BmsMeta, Bms};
use format::bms::{SinglePlay, CouplePlay, DoublePlay, BattlePlay, Difficulty};

/// Loader options for BMS format.
pub struct LoaderOptions {
    /// Parser options.
    parser: parse::ParserOptions,
}

impl LoaderOptions {
    /// Returns default loader options.
    pub fn new() -> LoaderOptions {
        LoaderOptions { parser: parse::ParserOptions::new() }
    }
}

/// An unprocessed data line of BMS file.
#[deriving(Clone)]
struct BmsLine {
    measure: uint,
    chan: Key,
    data: ~str,
    lineno: Option<uint>,
}

/// A callback function for messages during parsing. The line is optional for global messages.
/// The callback may return `false` to stop the processing.
pub type Callback<'r> = 'r |line: Option<uint>, msg: BmsMessage| -> bool;

/// Reads the BMS file with given RNG from given reader. Diagnostic messages are sent via callback.
pub fn load_bms<'r,R:Rng>(f: &mut Reader, r: &mut R, opts: &LoaderOptions,
                          callback: Callback<'r>) -> Result<Bms,~str> {
    use format::timeline::builder::{TimelineBuilder, Mark};

    macro_rules! diag(
        ($e:expr) => (
            if !callback(None, $e) { return Err(~"aborted"); }
        );
        ($e:expr at $lineno:expr) => (
            if !callback($lineno, $e) { return Err(~"aborted"); }
        )
    )

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
    let /*mut*/ canvassize = (256, 256);
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

    for parsed in parse::PreprocessingParser::new(f, r, &opts.parser).iter() {
        let (lineno, cmd) = match parsed {
            parse::Command(lineno, cmd) => (lineno, cmd),
            parse::Message(lineno, msg) => {
                diag!(msg at lineno);
                continue;
            }
            parse::Encoding(encname, confidence) => {
                encoding = (encname, confidence);
                if confidence <= 1.0 && !("ascii".equiv(&encname) || "utf-8".equiv(&encname)) {
                    diag!(BmsUsesLegacyEncoding);
                }
                continue;
            }
        };

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
                if bpm.to_f64() < 0.0 {
                    diag!(BmsHasNegativeInitBPM at lineno);
                } else if bpm.to_f64() == 0.0 {
                    diag!(BmsHasZeroInitBPM at lineno);
                } else {
                    builder.set_initbpm(bpm);
                }
            }
            parse::BmsExBPM(Key(i), bpm) => {
                if bpm.to_f64() <= 0.0 {
                    diag!(BmsHasNonpositiveBPM at lineno);
                }
                bpmtab[i] = bpm;
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
                if key == Key(0) {
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
                basepath = Some(Path::new(s.as_slice()));
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
    }

    if title.is_none() { diag!(BmsHasNoTITLE); }
    if artist.is_none() { diag!(BmsHasNoARTIST); }
    if genre.is_none() { diag!(BmsHasNoGENRE); }

    // clip the image slices if needed.
    for slice in imgslices.mut_iter() {
        match slice {
            &Some((_, ref mut slice)) => {
                slice.sx = cmp::max(slice.sx, 0);
                slice.sy = cmp::max(slice.sy, 0);
                slice.w = cmp::min(cmp::max(slice.w, 0), canvassize.n0());
                slice.h = cmp::min(cmp::max(slice.h, 0), canvassize.n1());
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
        match imgslices[iref.to_uint()] {
            Some((iref, ref slice)) => SlicedImageBGA(iref.clone(), ~slice.clone()),
            None => ImageBGA(iref),
        }
    };

    {
        // Handles a non-00 alphanumeric key `v` positioned at the particular channel `chan` and
        // particular position `t`. The position `t2` next to `t` is used for some cases that
        // an alphanumeric key designates an area rather than a point.
        let handle_key = |chan: Key, t: f64, t2: f64, v: Key, _lineno: Option<uint>| -> bool {
            let /*mut*/ ret = true;
            match chan {
                // channel #01: BGM
                Key(1) => { builder.add(t, BGM(SoundRef(v))); }

                // channel #03: BPM as an hexadecimal key
                Key(3) => {
                    for &v in v.to_hex().iter() {
                        builder.add(t, SetBPM(BPM(v as f64)))
                    }
                }

                // channel #04: BGA layer 1
                Key(4) => { builder.add(t, SetBGA(Layer1, imgref_to_bgaref(ImageRef(v)))); }

                // channel #06: POOR BGA
                Key(6) => {
                    builder.add(t, SetBGA(PoorBGA, imgref_to_bgaref(ImageRef(v))));
                    poorbgafix = false; // we don't add artificial BGA
                }

                // channel #07: BGA layer 2
                Key(7) => { builder.add(t, SetBGA(Layer2, imgref_to_bgaref(ImageRef(v)))); }

                // channel #08: BPM defined by #BPMxx
                Key(8) => { builder.add(t, SetBPM(bpmtab[v.to_int() as uint])); }

                // channel #09: scroll stopper defined by #STOPxx
                Key(9) => { builder.add(t, Stop(stoptab[v.to_int() as uint])); }

                // channel #0A: BGA layer 3
                Key(10) => { builder.add(t, SetBGA(Layer3, imgref_to_bgaref(ImageRef(v)))); }

                // channels #1x/2x: visible object, possibly LNs when #LNOBJ is in active
                Key(36/*1*36*/..107/*3*36-1*/) => {
                    let lane = chan.to_lane();
                    if lnobj.is_some() && lnobj == Some(v) {
                        // change the last inserted visible object to the start of LN if any.
                        let lastvispos = lastvis[lane.to_uint()];
                        for &mark in lastvispos.iter() {
                            builder.mutate(mark, |pos, data| {
                                assert!(data.is_visible());
                                (pos, data.to_lnstart())
                            });
                            builder.add(t, LNDone(lane, Some(SoundRef(v))));
                            lastvis[lane.to_uint()] = None;
                        }
                    } else {
                        let mark = builder.add_and_mark(t, Visible(lane, Some(SoundRef(v))));
                        lastvis[lane.to_uint()] = Some(mark);
                    }
                }

                // channels #3x/4x: invisible object
                Key(108/*3*36*/..179/*5*36-1*/) => {
                    let lane = chan.to_lane();
                    builder.add(t, Invisible(lane, Some(SoundRef(v))));
                }

                // channels #5x/6x, #LNTYPE 1: LN endpoints
                Key(180/*5*36*/..251/*7*36-1*/) if !consecutiveln => {
                    let lane = chan.to_lane();

                    // a pair of non-00 alphanumeric keys designate one LN. if there are an odd
                    // number of them, the last LN is implicitly closed later.
                    if lastln[lane.to_uint()].is_some() {
                        lastln[lane.to_uint()] = None;
                        builder.add(t, LNDone(lane, Some(SoundRef(v))));
                    } else {
                        let mark = builder.add_and_mark(t, LNStart(lane, Some(SoundRef(v))));
                        lastln[lane.to_uint()] = Some(mark); // TODO unused for now
                    }
                }

                // channels #5x/6x, #LNTYPE 2: LN areas
                Key(180/*5*36*/..251/*7*36-1*/) if consecutiveln => {
                    let lane = chan.to_lane();

                    // one non-00 alphanumeric key, in the absence of other information, inserts one
                    // complete LN starting at `t` and ending at `t2`.
                    //
                    // the next non-00 alphanumeric key also inserts one complete LN from `t` to
                    // `t2`, unless there is already an end of LN at `t` in which case the end of LN
                    // is simply moved from `t` to `t2` (effectively increasing the length of
                    // previous LN).
                    match lastln[lane.to_uint()] {
                        Some(mark) => {
                            builder.mutate(mark, |pos, data| {
                                if pos == t {
                                    assert!(data.is_lndone());
                                    (t2, data)
                                } else {
                                    (pos, data)
                                }
                            });
                        }
                        _ => {
                            builder.add(t, LNStart(lane, Some(SoundRef(v))));
                            let mark = builder.add_and_mark(t2, LNDone(lane, Some(SoundRef(v))));
                            lastln[lane.to_uint()] = Some(mark);
                        }
                    }
                }

                // channels #Dx/Ex: bombs, base-36 damage value (unit of 0.5% of the full gauge) or
                // instant death (ZZ)
                Key(468/*0xD*36*/..539/*0xF*36-1*/) => {
                    let lane = chan.to_lane();
                    let damage = match v {
                        Key(v @ 1..200) => Some(GaugeDamage(v as f64 / 200.0)),
                        Key(1295) => Some(InstantDeath), // XXX 1295=MAXKEY-1
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
        bmsline.sort_by(|a, b| (a.measure, a.chan).cmp(&(b.measure, b.chan)));
        for line in bmsline.iter() {
            let measure = line.measure as f64;
            let data: ~[char] = line.data.chars().collect();
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
    }

    // insert an artificial `SetBGA` object at 0.0 if required
    if poorbgafix {
        builder.add(0.0, SetBGA(PoorBGA, imgref_to_bgaref(ImageRef(Key(0)))));
    }

    // fix the unterminated longnote
    let nmeasures = bmsline.last().map_or(0, |l| l.measure) + 1;
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

