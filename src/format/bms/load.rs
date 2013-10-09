// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS loader. Uses a BMS parser (`format::bms::parse`) to produce `format::bms::Bms` structure.

use std::{vec, iter, io};
use std::rand::Rng;

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

/// An unprocessed data line of BMS file.
#[deriving(Clone)]
struct BmsLine {
    measure: uint,
    chan: Key,
    data: ~str
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

    do parse::each_bms_command(f, r, &opts.parser, callback) |_, cmd| {
        match cmd {
            parse::BmsTitle(s)            => { title = Some(s.into_owned()); }
            parse::BmsGenre(s)            => { genre = Some(s.into_owned()); }
            parse::BmsArtist(s)           => { artist = Some(s.into_owned()); }
            parse::BmsStageFile(s)        => { stagefile = Some(s.into_owned()); }
            parse::BmsBPM(bpm)            => { builder.set_initbpm(bpm); }
            parse::BmsExBPM(Key(i), bpm)  => { bpmtab[i] = bpm; }
            parse::BmsPlayer(v)           => { player = v; }
            parse::BmsPlayLevel(v)        => { playlevel = v; }
            parse::BmsRank(v)             => { rank = v; }
            parse::BmsLNType(lntype)      => { consecutiveln = (lntype == 2); }
            parse::BmsLNObj(key)          => { lnobj = Some(key); }
            parse::BmsWAV(Key(i), s)      => { sndpath[i] = Some(s.into_owned()); }
            parse::BmsBMP(Key(i), s)      => { imgpath[i] = Some(s.into_owned()); }
            parse::BmsBGA(bc)             => { blitcmd.push(bc); }
            parse::BmsStop(Key(i), dur)   => { stoptab[i] = dur; }
            parse::BmsStp(pos, dur)       => { builder.add(pos, Stop(dur)); }

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
                bmsline.push(BmsLine { measure: measure, chan: chan, data: data.into_owned() })
            }

            parse::BmsFlow(_) => { fail!("unexpected"); }
            _ => {}
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
    Ok(Bms { bmspath: None,
             meta: BmsMeta { title: title, genre: genre, artist: artist, stagefile: stagefile,
                             basepath: basepath, player: player, playlevel: playlevel, rank: rank,
                             sndpath: sndpath, imgpath: imgpath, blitcmd: blitcmd },
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

