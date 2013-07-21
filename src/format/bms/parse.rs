// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS parser.

use std::{uint, vec};
use format::obj::{BPM, Duration, Seconds, Measures};
use format::bms::types::{Key};
use format::bms::{ImageRef, BlitCmd};

/// A tuple of four `u8` values. Mainly used for BMS #ARGB command and its family.
pub type ARGB = (u8,u8,u8,u8);

/// Represents one line of BMS file.
pub enum BmsCommand<'self> {
    BmsUnknown(&'self str),             // starting with `#` but unknown otherwise
    BmsTitle(&'self str),               // #TITLE
    BmsSubtitle(&'self str),            // #SUBTITLE
    BmsGenre(&'self str),               // #GENRE
    BmsArtist(&'self str),              // #ARTIST
    BmsSubartist(&'self str),           // #SUBARTIST
    BmsMaker(&'self str),               // #MAKER
    BmsComment(&'self str),             // #COMMENT
    BmsStageFile(&'self str),           // #STAGEFILE
    BmsBanner(&'self str),              // #BANNER
    BmsPathWAV(&'self str),             // #PATH_WAV
    BmsBPM(BPM),                        // #BPM (without a following alphanumeric key)
    BmsExBPM(Key, BPM),                 // #EXBPM or #BPMxx
    BmsPlayer(int),                     // #PLAYER
    BmsLanes(&'self [(Key, &'self str)]), // #SNRS:LANES (experimental)
    BmsPlayLevel(int),                  // #PLAYLEVEL
    BmsDifficulty(int),                 // #DIFFICULTY
    BmsRank(int),                       // #RANK
    BmsDefExRank(int),                  // #DEFEXRANK
    BmsExRank(Key, int),                // #EXRANK
    BmsTotal(int),                      // #TOTAL
    BmsLNType(int),                     // #LNTYPE
    BmsLNObj(Key),                      // #LNOBJ
    BmsWAV(Key, &'self str),            // #WAV
    BmsWAVCmd(int, Key, int),           // #WAVCMD
    BmsExWAV(Key, Option<int>, Option<int>, Option<int>, &'self str), // #EXWAV
    BmsVolWAV(int),                     // #VOLWAV
    BmsMIDIFile(&'self str),            // #MIDIFILE
    BmsBMP(Key, &'self str),            // #BMP
    BmsExBMP(Key, ARGB, &'self str),    // #EXBMP
    BmsBackBMP(&'self str),             // #BACKBMP
    BmsBGA(BlitCmd),                    // #BGA or #@BGA
    BmsPoorBGA(int),                    // #POORBGA
    BmsSwBGA(Key, int, int, Key, bool, ARGB, &'self str), // #SWBGA
    BmsARGB(Key, ARGB),                 // #ARGB
    BmsCharFile(&'self str),            // #CHARFILE
    BmsVideoFile(&'self str),           // #VIDEOFILE
    BmsMovie(&'self str),               // #MOVIE
    BmsCanvasSize(int, int),            // #SNRS:CANVASSIZE (experimental)
    BmsStop(Key, Duration),             // #STOP
    BmsStp(float, Duration),            // #STP
    BmsText(Key, &'self str),           // #TEXT or #SONG
    BmsOption(&'self str),              // #OPTION
    BmsChangeOption(Key, &'self str),   // #CHANGEOPTION
    BmsRandom(int),                     // #RANDOM
    BmsSetRandom(int),                  // #SETRANDOM
    BmsEndRandom,                       // #ENDRANDOM
    BmsIf(int),                         // #IF
    BmsElseIf(int),                     // #ELSEIF
    BmsElse,                            // #ELSE
    BmsEndIf,                           // #ENDIF
    BmsSwitch(int),                     // #SWITCH
    BmsSetSwitch(int),                  // #SETSWITCH
    BmsEndSw,                           // #ENDSW
    BmsCase(int),                       // #CASE
    BmsSkip,                            // #SKIP
    BmsDef,                             // #DEF
    BmsShorten(uint, float),            // #xxx02
    BmsData(uint, Key, &'self str)      // #xxxyy:...
}

impl<'self> ToStr for BmsCommand<'self> {
    /// Returns a reconstructed line for given BMS command.
    fn to_str(&self) -> ~str {
        fn argb_to_str((a,r,g,b): ARGB) -> ~str {
            fmt!("%u,%u,%u,%u", a as uint, r as uint, g as uint, b as uint)
        }

        match *self {
            BmsUnknown(s) => fmt!("#%s", s),
            BmsTitle(s) => fmt!("#TITLE %s", s),
            BmsSubtitle(s) => fmt!("#SUBTITLE %s", s),
            BmsGenre(s) => fmt!("#GENRE %s", s),
            BmsArtist(s) => fmt!("#ARTIST %s", s),
            BmsSubartist(s) => fmt!("#SUBARTIST %s", s),
            BmsMaker(s) => fmt!("#MAKER %s", s),
            BmsComment(s) => fmt!("#COMMENT %s", s),
            BmsStageFile(s) => fmt!("#STAGEFILE %s", s),
            BmsBanner(s) => fmt!("#BANNER %s", s),
            BmsPathWAV(s) => fmt!("#PATH_WAV %s", s),
            BmsBPM(BPM(bpm)) => fmt!("#BPM %f", bpm),
            BmsExBPM(key, BPM(bpm)) => fmt!("#BPM%s %f", key.to_str(), bpm),
            BmsPlayer(v) => fmt!("#PLAYER %d", v),
            BmsLanes(ref lanes) => {
                let specs: ~[~str] = do lanes.iter().transform::<~str> |&(lane,spec)| {
                    fmt!(" %s %s", lane.to_str(), spec)
                }.collect();
                fmt!("#SNRS:LANES%s", specs.concat())
            },
            BmsPlayLevel(v) => fmt!("#PLAYLEVEL %d", v),
            BmsDifficulty(v) => fmt!("#DIFFICULTY %d", v),
            BmsRank(v) => fmt!("#RANK %d", v),
            BmsDefExRank(v) => fmt!("#DEFEXRANK %d", v),
            BmsExRank(key, v) => fmt!("#EXRANK %s %d", key.to_str(), v),
            BmsTotal(v) => fmt!("#TOTAL %d", v),
            BmsLNType(lntype) => fmt!("#LNTYPE %d", lntype),
            BmsLNObj(key) => fmt!("#LNOBJ %s", key.to_str()),
            BmsWAV(key, s) => fmt!("#WAV%s %s", key.to_str(), s),
            BmsWAVCmd(cmd, key, v) => fmt!("#WAVCMD %02d %s%d", cmd, key.to_str(), v),
            BmsExWAV(_key, None, None, None, _s) => fail!(~"unsupported"),
            BmsExWAV(key, pan, vol, freq, s) => {
                let mut flags = ~""; let mut opts = ~"";
                if pan.is_some() { flags.push_char('p'); opts.push_str(fmt!(" %d", pan.get())); }
                if vol.is_some() { flags.push_char('v'); opts.push_str(fmt!(" %d", vol.get())); }
                if freq.is_some() { flags.push_char('f'); opts.push_str(fmt!(" %d", freq.get())); }
                fmt!("#EXWAV%s %s%s %s", key.to_str(), flags, opts, s)
            },
            BmsVolWAV(v) => fmt!("#VOLWAV %d", v),
            BmsMIDIFile(s) => fmt!("#MIDIFILE %s", s),
            BmsBMP(key, s) => fmt!("#BMP%s %s", key.to_str(), s),
            BmsExBMP(key, argb, s) => fmt!("#EXBMP%s %s %s", key.to_str(), argb_to_str(argb), s),
            BmsBackBMP(s) => fmt!("#BACKBMP %s", s),
            BmsBGA(BlitCmd { dst, src, x1, y1, x2, y2, dx, dy }) =>
                fmt!("#BGA%s %s %d %d %d %d %d %d",
                     dst.to_str(), src.to_str(), x1, y1, x2, y2, dx, dy),
            BmsPoorBGA(poorbga) => fmt!("#POORBGA %d", poorbga),
            BmsSwBGA(key, fr, time, line, doloop, argb, pattern) =>
                fmt!("#SWBGA%s %d:%d:%s:%d:%s %s",
                     key.to_str(), fr, time, line.to_str(), if doloop {1} else {0},
                     argb_to_str(argb), pattern),
            BmsARGB(key, argb) => fmt!("#ARGB%s %s", key.to_str(), argb_to_str(argb)),
            BmsCharFile(s) => fmt!("#CHARFILE %s", s),
            BmsVideoFile(s) => fmt!("#VIDEOFILE %s", s),
            BmsMovie(s) => fmt!("#MOVIE %s", s),
            BmsCanvasSize(w, h) => fmt!("#SNRS:CANVASSIZE %d %d", w, h),
            BmsStop(key, Measures(dur)) => fmt!("#STOP%s %d", key.to_str(), (dur * 192.0) as int),
            BmsStop(*) => fail!(~"unsupported"),
            BmsStp(pos, Seconds(dur)) => fmt!("#STP %07.3f %d", pos, (dur * 1000.0) as int),
            BmsStp(*) => fail!(~"unsupported"),
            BmsText(key, s) => fmt!("#TEXT%s %s", key.to_str(), s),
            BmsOption(opt) => fmt!("#OPTION %s", opt),
            BmsChangeOption(key, opt) => fmt!("#CHANGEOPTION%s %s", key.to_str(), opt),
            BmsRandom(val) => fmt!("#RANDOM %d", val),
            BmsSetRandom(val) => fmt!("#SETRANDOM %d", val),
            BmsEndRandom => ~"#ENDRANDOM",
            BmsSwitch(val) => fmt!("#SWITCH %d", val),
            BmsSetSwitch(val) => fmt!("#SETSWITCH %d", val),
            BmsCase(val) => fmt!("#CASE %d", val),
            BmsSkip => ~"#SKIP",
            BmsDef => ~"#DEF",
            BmsEndSw => ~"#ENDSW",
            BmsIf(val) => fmt!("#IF %d", val),
            BmsElseIf(val) => fmt!("#ELSEIF %d", val),
            BmsElse => ~"#ELSE",
            BmsEndIf => ~"#ENDIF",
            BmsShorten(measure, shorten) => fmt!("#%03u02:%f", measure, shorten),
            BmsData(measure, chan, data) => fmt!("#%03u%s:%s", measure, chan.to_str(), data),
        }
    }
}

/// Iterates over the parsed BMS commands.
pub fn each_bms_command(f: @::std::io::Reader, blk: &fn(BmsCommand) -> bool) -> bool {
    use util::std::str::StrUtil;

    let lines = vec::split(f.read_whole_stream(), |&ch| ch == 10u8);
    for lines.iter().advance |&line0| {
        let line0 = ::util::std::str::from_fixed_utf8_bytes(line0, |_| ~"\ufffd");

        // skip non-command lines
        let line = line0.trim_left();
        if line.is_empty() { loop; }
        let (ch, line) = line.slice_shift_char();
        if !(ch == '#' || ch == '\uff03') { loop; } // typo (full-width sharp)

        let upperline = line.to_ascii_upper();

        macro_rules! if_prefix(
            ($prefix:expr -> $constr:ident string) => (
                if_prefix!($prefix { // #<command> <string>
                    let mut text = "";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        if !blk($constr(text)) { return false; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident value) => (
                if_prefix!($prefix { // #<command> <int>
                    let mut value = 0;
                    if lex!(line; ws, int -> value) {
                        if !blk($constr(value)) { return false; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident (ws) value) => (
                if_prefix!($prefix { // #<command> <int>, but the whitespace is omissible
                    let mut value = 0;
                    if lex!(line; ws*, int -> value) {
                        if !blk($constr(value)) { return false; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident key string) => (
                if_prefix!($prefix { // #<command>xx <string>
                    let mut key = Key(-1);
                    let mut text = "";
                    if lex!(line; Key -> key, ws, str -> text, ws*, !) {
                        if !blk($constr(key, text)) { return false; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident) => (
                if_prefix!($prefix { // #<command>
                    if !blk($constr) { return false; }
                })
            );
            ($prefix:expr $blk:expr) => ({
                let prefix: &'static str = $prefix;
                if upperline.starts_with(prefix) {
                    let line = line.slice_from(prefix.len());
                    let _ = line; // removes warning
                    $blk;
                    loop;
                }
            })
        )

        if_prefix!("TITLE"     -> BmsTitle     string)
        if_prefix!("SUBTITLE"  -> BmsSubtitle  string)
        if_prefix!("GENRE"     -> BmsGenre     string)
        if_prefix!("GENLE"     -> BmsGenre     string) // typo
        if_prefix!("ARTIST"    -> BmsArtist    string)
        if_prefix!("SUBARTIST" -> BmsSubartist string)
        if_prefix!("MAKER"     -> BmsMaker     string)
        if_prefix!("COMMENT"   -> BmsComment   string) // quotes are stripped by caller
        if_prefix!("STAGEFILE" -> BmsStageFile string)
        if_prefix!("BANNER"    -> BmsBanner    string)
        if_prefix!("PATH_WAV"  -> BmsPathWAV   string)

        if_prefix!("BPM" { // #BPM <float> or #BPMxx <float>
            let mut key = Key(-1);
            let mut bpm = 0.0;
            if lex!(line; Key -> key, ws, float -> bpm) {
                if !blk(BmsExBPM(key, BPM(bpm))) { return false; }
            } else if lex!(line; ws, float -> bpm) {
                if !blk(BmsBPM(BPM(bpm))) { return false; }
            }
        })

        if_prefix!("PLAYER"     -> BmsPlayer     value)
        if_prefix!("PLAYLEVEL"  -> BmsPlayLevel  value)
        if_prefix!("DIFFICULTY" -> BmsDifficulty value)
        if_prefix!("RANK"       -> BmsRank       value)
        if_prefix!("DEFEXRANK"  -> BmsDefExRank  value)
        if_prefix!("TOTAL"      -> BmsTotal      value)
        if_prefix!("LNTYPE"     -> BmsLNType     value)

        if_prefix!("LNOBJ" { // #LNOBJ <key>
            let mut key = Key(-1);
            if lex!(line; ws, Key -> key) {
                if !blk(BmsLNObj(key)) { return false; }
            }
        })

        if_prefix!("EXRANK" { // #EXRANKxx <int>
            let mut key = Key(-1);
            let mut value = 0;
            if lex!(line; Key -> key, ws, int -> value) {
                if !blk(BmsExRank(key, value)) { return false; }
            }
        })

        if_prefix!("SNRS:LANES" { // #SNRS:LANES <key> <spec> <key> <spec> ...
            let words: ~[&str] = line.word_iter().collect();
            if !words.is_empty() && words.len() % 2 == 0 {
                let mut lanes = ~[];
                let mut okay = true;
                for uint::range_step(0, words.len(), 2) |i| {
                    if words[i].len() != 2 { okay = false; break; }
                    match Key::from_str(words[i]) {
                        Some(key) => { lanes.push((key, words[i+1])); }
                        None => { okay = false; break; }
                    }
                }
                if okay {
                    if !blk(BmsLanes(lanes)) { return false; }
                }
            }
        })

        if_prefix!("WAVCMD" { // #WAVCMD <int> xx <int>
            let mut cmd = 0;
            let mut key = Key(0);
            let mut value = 0;
            if lex!(line; ws, int -> cmd, ws, Key -> key, ws, int -> value) {
                if !blk(BmsWAVCmd(cmd, key, value)) { return false; }
            }
        })

        if_prefix!("WAV"      -> BmsWAV  key string)
        if_prefix!("VOLWAV"   -> BmsVolWAV    value)
        if_prefix!("MIDIFILE" -> BmsMIDIFile string)

        if_prefix!("EXWAV" { // #EXWAV [pvf] <int>* <string>
            let mut key = Key(0);
            let mut line_ = "";
            if lex!(line; Key -> key, ws, str -> line_, !) {
                match line_.find(|c: char| c.is_whitespace()) {
                    Some(sep) => {
                        let flags = line_.slice_to(sep);
                        let mut pan = None;
                        let mut vol = None;
                        let mut freq = None;

                        line_ = line_.slice_from(sep);
                        let mut okay = true;
                        for flags.iter().advance |flag| {
                            let mut value = 0;
                            if !lex!(line_; ws, int -> value, str -> line_) { okay = false; break; }
                            match flag {
                                'p'|'P' => {
                                    if pan.is_some() { okay = false; break; }
                                    pan = Some(value);
                                }
                                'v'|'V' => {
                                    if vol.is_some() { okay = false; break; }
                                    vol = Some(value);
                                }
                                'f'|'F' => {
                                    if freq.is_some() { okay = false; break; }
                                    freq = Some(value);
                                }
                                _ => { okay = false; break; }
                            }
                        }
                        if okay {
                            let mut text = "";
                            if lex!(line_; ws, str -> text, ws*, !) {
                                if !blk(BmsExWAV(key, pan, vol, freq, text)) { return false; }
                            }
                        }
                    }
                    None => {}
                }
            }
        })

        if_prefix!("BMP"      -> BmsBMP key string)
        if_prefix!("BACKBMP"  -> BmsBackBMP string)
        if_prefix!("POORBGA"  -> BmsPoorBGA  value)

        if_prefix!("EXBMP" { // #EXBMPxx <int8>,<int8>,<int8>,<int8> <string>
            let mut key = Key(0);
            let mut argb = (0,0,0,0);
            let mut text = "";
            if lex!(line; Key -> key, ws, ARGB -> argb, ws, str -> text, ws*, !) {
                if !blk(BmsExBMP(key, argb, text)) { return false; }
            }
        })

        if_prefix!("BGA" { // #BGAxx yy <int> <int> <int> <int> <int> <int>
            let mut dst = Key(0);
            let mut src = Key(0);
            let mut bc = BlitCmd { dst: ImageRef(Key(0)), src: ImageRef(Key(0)),
                                   x1: 0, y1: 0, x2: 0, y2: 0, dx: 0, dy: 0 };
            if lex!(line; Key -> dst, ws, Key -> src, ws, int -> bc.x1, ws, int -> bc.y1, ws,
                    int -> bc.x2, ws, int -> bc.y2, ws, int -> bc.dx, ws, int -> bc.dy) {
                bc.src = ImageRef(src);
                bc.dst = ImageRef(dst);
                if !blk(BmsBGA(bc)) { return false; }
            }
        })

        if_prefix!("@BGA" { // #@BGAxx yy <int> <int> <int> <int> <int> <int>
            let mut dst = Key(0);
            let mut src = Key(0);
            let mut bc = BlitCmd { dst: ImageRef(Key(0)), src: ImageRef(Key(0)),
                                   x1: 0, y1: 0, x2: 0, y2: 0, dx: 0, dy: 0 };
            if lex!(line; Key -> dst, ws, Key -> src, ws, int -> bc.x1, ws, int -> bc.y1, ws,
                    int -> bc.x2, ws, int -> bc.y2, ws, int -> bc.dx, ws, int -> bc.dy) {
                bc.src = ImageRef(src);
                bc.dst = ImageRef(dst);
                bc.x2 += bc.x1;
                bc.y2 += bc.y1;
                if !blk(BmsBGA(bc)) { return false; }
            }
        })

        if_prefix!("SWBGA" { // #SWBGAxx <int>:<int>:yy:<int>:<int8>,<int8>,<int8>,<int8> ...
            let mut key = Key(0);
            let mut fr = 0;
            let mut time = 0;
            let mut linekey = Key(0);
            let mut doloop = 0;
            let mut argb = (0,0,0,0);
            let mut pattern = "";
            if lex!(line; Key -> key, ws, int -> fr, ws*, ':', ws*, int -> time, ws*, ':', ws*,
                          Key -> linekey, ws*, ':', ws*, int -> doloop, ws*, ':', ws*,
                          ARGB -> argb, ws, str -> pattern, ws*, !) {
                if doloop == 0 || doloop == 1 {
                    if !blk(BmsSwBGA(key, fr, time, linekey, doloop == 1, argb, pattern)) {
                        return false;
                    }
                }
            }
        })

        if_prefix!("ARGB" { // #ARGBxx <int8>,<int8>,<int8>,<int8>
            let mut key = Key(0);
            let mut argb = (0,0,0,0);
            if lex!(line; Key -> key, ws, ARGB -> argb) {
                if !blk(BmsARGB(key, argb)) { return false; }
            }
        })

        if_prefix!("CHARFILE"  -> BmsCharFile  string)
        if_prefix!("VIDEOFILE" -> BmsVideoFile string)
        if_prefix!("MOVIE"     -> BmsMovie     string)

        if_prefix!("SNRS:CANVASSIZE" { // #SNRS:CANVASSIZE <int> <int>
            let mut width = 0;
            let mut height = 0;
            if lex!(line; ws, int -> width, ws, int -> height) {
                if !blk(BmsCanvasSize(width, height)) { return false; }
            }
        })

        if_prefix!("STOP" { // #STOPxx <int>
            let mut key = Key(-1);
            let mut duration = 0;
            if lex!(line; Key -> key, ws, int -> duration) {
                let duration = Measures(duration as float / 192.0);
                if !blk(BmsStop(key, duration)) { return false; }
            }
        })

        if_prefix!("STP" { // #STP<int>.<int> <int>
            let mut measure = 0;
            let mut frac = 0;
            let mut duration = 0;
            if lex!(line; Measure -> measure, '.', uint -> frac, ws,
                    int -> duration) && duration > 0 {
                let pos = measure as float + frac as float * 0.001;
                let duration = Seconds(duration as float * 0.001);
                if !blk(BmsStp(pos, duration)) { return false; }
            }
        })

        if_prefix!("TEXT"         -> BmsText         key string)
        if_prefix!("SONG"         -> BmsText         key string)
        if_prefix!("OPTION"       -> BmsOption           string)
        if_prefix!("CHANGEOPTION" -> BmsChangeOption key string)

        // we parse SWITCH directives first since we need to parse #END as #ENDIF.
        if_prefix!("SWITCH"    -> BmsSwitch    value)
        if_prefix!("SETSWITCH" -> BmsSetSwitch value)
        if_prefix!("ENDSW"     -> BmsEndSw          )
        if_prefix!("CASE"      -> BmsCase      value)
        if_prefix!("SKIP"      -> BmsSkip           )
        if_prefix!("DEF"       -> BmsDef            )

        if_prefix!("RANDOM"    -> BmsRandom (ws) value) // typo (omissible whitespace)
        if_prefix!("RONDAM"    -> BmsRandom      value) // typo
        if_prefix!("SETRANDOM" -> BmsSetRandom   value)
        if_prefix!("ENDRANDOM" -> BmsEndRandom        )
        if_prefix!("IFEND"     -> BmsEndIf            ) // typo
        if_prefix!("IF"        -> BmsIf     (ws) value) // typo (omissible whitespace)
        if_prefix!("ELSEIF"    -> BmsElseIf      value)
        if_prefix!("ELSE"      -> BmsElse             )
        if_prefix!("END"       -> BmsEndIf            )

        let mut measure = 0;
        let mut chan = Key(0);
        let mut data = "";
        if lex!(line; Measure -> measure, Key -> chan, ':', ws*, str -> data, ws*, !) {
            if chan == Key(2) { // #xxx02:<float>
                let mut shorten = 0.0;
                if lex!(data; ws*, float -> shorten) {
                    if !blk(BmsShorten(measure, shorten)) { return false; }
                    loop;
                }
            } else {
                if !blk(BmsData(measure, chan, data)) { return false; }
                loop;
            }
        }

        if !blk(BmsUnknown(line)) { return false; }
    }
    true
}

