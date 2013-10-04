// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS parser.

use std::{io, iter};
use format::obj::{BPM, Duration, Seconds, Measures};
use format::bms::types::{Key};
use format::bms::diag::*;
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
    BmsStp(f64, Duration),              // #STP
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
    BmsShorten(uint, f64),              // #xxx02
    BmsData(uint, Key, &'self str)      // #xxxyy:...
}

impl<'self> ToStr for BmsCommand<'self> {
    /// Returns a reconstructed line for given BMS command.
    fn to_str(&self) -> ~str {
        fn argb_to_str((a,r,g,b): ARGB) -> ~str { format!("{},{},{},{}", a, r, g, b) }

        match *self {
            BmsUnknown(s) => format!("\\#{}", s),
            BmsTitle(s) => format!("\\#TITLE {}", s),
            BmsSubtitle(s) => format!("\\#SUBTITLE {}", s),
            BmsGenre(s) => format!("\\#GENRE {}", s),
            BmsArtist(s) => format!("\\#ARTIST {}", s),
            BmsSubartist(s) => format!("\\#SUBARTIST {}", s),
            BmsMaker(s) => format!("\\#MAKER {}", s),
            BmsComment(s) => format!("\\#COMMENT {}", s),
            BmsStageFile(s) => format!("\\#STAGEFILE {}", s),
            BmsBanner(s) => format!("\\#BANNER {}", s),
            BmsPathWAV(s) => format!("\\#PATH_WAV {}", s),
            BmsBPM(BPM(bpm)) => format!("\\#BPM {}", bpm),
            BmsExBPM(key, BPM(bpm)) => format!("\\#BPM{} {}", key.to_str(), bpm),
            BmsPlayer(v) => format!("\\#PLAYER {}", v),
            BmsLanes(ref lanes) => {
                let specs: ~[~str] = do lanes.iter().map |&(lane,spec)| {
                    format!(" {} {}", lane.to_str(), spec)
                }.collect();
                format!("\\#SNRS:LANES{}", specs.concat())
            },
            BmsPlayLevel(v) => format!("\\#PLAYLEVEL {}", v),
            BmsDifficulty(v) => format!("\\#DIFFICULTY {}", v),
            BmsRank(v) => format!("\\#RANK {}", v),
            BmsDefExRank(v) => format!("\\#DEFEXRANK {}", v),
            BmsExRank(key, v) => format!("\\#EXRANK {} {}", key.to_str(), v),
            BmsTotal(v) => format!("\\#TOTAL {}", v),
            BmsLNType(lntype) => format!("\\#LNTYPE {}", lntype),
            BmsLNObj(key) => format!("\\#LNOBJ {}", key.to_str()),
            BmsWAV(key, s) => format!("\\#WAV{} {}", key.to_str(), s),
            BmsWAVCmd(cmd, key, v) => format!("\\#WAVCMD {:02} {}{}", cmd, key.to_str(), v),
            BmsExWAV(_key, None, None, None, _s) => fail!(~"unsupported"),
            BmsExWAV(key, pan, vol, freq, s) => {
                let mut flags = ~"";
                let mut opts = ~"";
                if pan.is_some() {
                    flags.push_char('p');
                    opts.push_str(format!(" {}", pan.unwrap()));
                }
                if vol.is_some() {
                    flags.push_char('v');
                    opts.push_str(format!(" {}", vol.unwrap()));
                }
                if freq.is_some() {
                    flags.push_char('f');
                    opts.push_str(format!(" {}", freq.unwrap()));
                }
                format!("\\#EXWAV{} {}{} {}", key.to_str(), flags, opts, s)
            },
            BmsVolWAV(v) => format!("\\#VOLWAV {}", v),
            BmsMIDIFile(s) => format!("\\#MIDIFILE {}", s),
            BmsBMP(key, s) => format!("\\#BMP{} {}", key.to_str(), s),
            BmsExBMP(key, argb, s) =>
                format!("\\#EXBMP{} {} {}", key.to_str(), argb_to_str(argb), s),
            BmsBackBMP(s) => format!("\\#BACKBMP {}", s),
            BmsBGA(BlitCmd { dst, src, x1, y1, x2, y2, dx, dy }) =>
                format!("\\#BGA{} {} {} {} {} {} {} {}",
                     dst.to_str(), src.to_str(), x1, y1, x2, y2, dx, dy),
            BmsPoorBGA(poorbga) => format!("\\#POORBGA {}", poorbga),
            BmsSwBGA(key, fr, time, line, doloop, argb, pattern) =>
                format!("\\#SWBGA{} {}:{}:{}:{}:{} {}",
                     key.to_str(), fr, time, line.to_str(), if doloop {1} else {0},
                     argb_to_str(argb), pattern),
            BmsARGB(key, argb) => format!("\\#ARGB{} {}", key.to_str(), argb_to_str(argb)),
            BmsCharFile(s) => format!("\\#CHARFILE {}", s),
            BmsVideoFile(s) => format!("\\#VIDEOFILE {}", s),
            BmsMovie(s) => format!("\\#MOVIE {}", s),
            BmsCanvasSize(w, h) => format!("\\#SNRS:CANVASSIZE {} {}", w, h),
            BmsStop(key, Measures(dur)) =>
                format!("\\#STOP{} {}", key.to_str(), (dur * 192.0) as int),
            BmsStop(*) => fail!(~"unsupported"),
            BmsStp(pos, Seconds(dur)) => format!("\\#STP {:07.3} {}", pos, (dur * 1000.0) as int),
            BmsStp(*) => fail!(~"unsupported"),
            BmsText(key, s) => format!("\\#TEXT{} {}", key.to_str(), s),
            BmsOption(opt) => format!("\\#OPTION {}", opt),
            BmsChangeOption(key, opt) => format!("\\#CHANGEOPTION{} {}", key.to_str(), opt),
            BmsRandom(val) => format!("\\#RANDOM {}", val),
            BmsSetRandom(val) => format!("\\#SETRANDOM {}", val),
            BmsEndRandom => ~"#ENDRANDOM",
            BmsSwitch(val) => format!("\\#SWITCH {}", val),
            BmsSetSwitch(val) => format!("\\#SETSWITCH {}", val),
            BmsCase(val) => format!("\\#CASE {}", val),
            BmsSkip => ~"#SKIP",
            BmsDef => ~"#DEF",
            BmsEndSw => ~"#ENDSW",
            BmsIf(val) => format!("\\#IF {}", val),
            BmsElseIf(val) => format!("\\#ELSEIF {}", val),
            BmsElse => ~"#ELSE",
            BmsEndIf => ~"#ENDIF",
            BmsShorten(measure, shorten) => format!("\\#{:03}02:{}", measure, shorten),
            BmsData(measure, chan, data) => format!("\\#{:03}{}:{}", measure, chan.to_str(), data),
        }
    }
}

/// Parser options for BMS format.
pub struct BmsParserOptions {
    /// Enables a parsing of several obviously mistyped commands. (Default: true)
    autofix_commands: bool,
}

impl BmsParserOptions {
    /// Returns default parser options.
    pub fn new() -> BmsParserOptions {
        BmsParserOptions { autofix_commands: true }
    }
}

/// Iterates over the parsed BMS commands.
pub fn each_bms_command<Listener:BmsMessageListener>(
                                f: @io::Reader, opts: &BmsParserOptions,
                                callback: &mut Listener, blk: &fn(BmsCommand) -> bool) -> bool {
    use util::std::str::{StrUtil, from_fixed_utf8_bytes};

    let file = f.read_whole_stream();
    let mut lineno = 0;
    let mut ret = true;
    'eachline: for line0 in file.split_iter(|&ch| ch == 10u8) {
        let line0 = from_fixed_utf8_bytes(line0, |_| ~"\ufffd");
        lineno += 1;

        macro_rules! diag(
            ($e:expr) => (
                if !callback.on_message(None, $e) {
                    ret = false;
                    break 'eachline;
                }
            );
            ($e:expr at $line:expr) => (
                if !callback.on_message(Some($line), $e) {
                    ret = false;
                    break 'eachline;
                }
            )
        )

        // skip non-command lines
        let line = line0.trim_left();
        if line.is_empty() { loop; }
        let (ch, line) = line.slice_shift_char();
        if ch == '\uff03' {
            diag!(BmsHasFullWidthSharp at lineno);
        } else if ch != '#' {
            loop;
        }

        let upperline = line.to_ascii_upper();

        // emits a `BmsCommand` and restarts the loop
        macro_rules! emit(
            ($e:expr) => ({
                if blk($e) { loop; } else { ret = false; break 'eachline; }
            })
        )

        // helper macro for `if_prefix!`; "tt" (token tree) cannot directly pasted to the AST.
        macro_rules! tt_to_expr(($e:expr) => ($e))

        // matches a prefix and performs predefined or custom actions
        macro_rules! if_prefix(
            ($prefix:tt -> $constr:ident string) => (
                if_prefix!($prefix |line| { // #<command> <string>
                    let mut text = "";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        emit!($constr(text));
                    }
                })
            );
            ($prefix:tt -> $constr:ident string ($diag:expr)) => (
                if_prefix!($prefix |line| { // #<command> <string>
                    let mut text = "";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        diag!($diag at lineno);
                        emit!($constr(text));
                    }
                })
            );
            ($prefix:tt -> $constr:ident value) => (
                if_prefix!($prefix |line| { // #<command> <int>
                    let mut value = 0;
                    if lex!(line; ws, int -> value) {
                        emit!($constr(value));
                    }
                })
            );
            ($prefix:tt -> $constr:ident value ($diag:expr)) => (
                if_prefix!($prefix |line| { // #<command> <int>
                    let mut value = 0;
                    if lex!(line; ws, int -> value) {
                        diag!($diag at lineno);
                        emit!($constr(value));
                    }
                })
            );
            ($prefix:tt -> $constr:ident (ws) value ($diag:expr)) => (
                if_prefix!($prefix |line| { // #<command> <int>, but the whitespace is omissible
                    let mut value = 0;
                    if lex!(line; ws, int -> value) {
                        emit!($constr(value));
                    } else if lex!(line; int -> value) {
                        diag!($diag at lineno);
                        emit!($constr(value));
                    }
                })
            );
            ($prefix:tt -> $constr:ident key string) => (
                if_prefix!($prefix |line| { // #<command>xx <string>
                    let mut key = Key(-1);
                    let mut text = "";
                    if lex!(line; Key -> key, ws, str -> text, ws*, !) {
                        emit!($constr(key, text));
                    }
                })
            );
            ($prefix:tt -> $constr:ident key string ($diag:expr)) => (
                if_prefix!($prefix |line| { // #<command>xx <string>
                    let mut key = Key(-1);
                    let mut text = "";
                    if lex!(line; Key -> key, ws, str -> text, ws*, !) {
                        diag!($diag at lineno);
                        emit!($constr(key, text));
                    }
                })
            );
            ($prefix:tt -> $constr:ident) => (
                // Rust: unreachable code sometimes causes an ICE. (#7344)
                if upperline.starts_with(tt_to_expr!($prefix)) {
                    emit!($constr);
                }
            );
            ($prefix:tt -> $constr:ident ($diag:expr)) => (
                if upperline.starts_with(tt_to_expr!($prefix)) {
                    diag!($diag at lineno);
                    emit!($constr);
                }
            );
            ($prefix:tt |$v:ident| $then:expr) => ({
                let prefix: &'static str = tt_to_expr!($prefix);
                if upperline.starts_with(prefix) {
                    let $v = line.slice_from(prefix.len());
                    let _ = $v; // removes warning
                    $then;
                    emit!(BmsUnknown($v)); // no more matching possible
                }
            })
        )

        if_prefix!("TITLE"     -> BmsTitle     string)
        if_prefix!("SUBTITLE"  -> BmsSubtitle  string)
        if_prefix!("GENRE"     -> BmsGenre     string)
        if opts.autofix_commands {
            if_prefix!("GENLE" -> BmsGenre     string (BmsHasGENLE))
        }
        if_prefix!("ARTIST"    -> BmsArtist    string)
        if_prefix!("SUBARTIST" -> BmsSubartist string)
        if_prefix!("MAKER"     -> BmsMaker     string)
        if_prefix!("COMMENT"   -> BmsComment   string) // quotes are stripped by caller
        if_prefix!("STAGEFILE" -> BmsStageFile string)
        if_prefix!("BANNER"    -> BmsBanner    string)
        if_prefix!("PATH_WAV"  -> BmsPathWAV   string)

        if_prefix!("BPM" |line| { // #BPM <float> or #BPMxx <float>
            let mut key = Key(-1);
            let mut bpm = 0.0;
            if lex!(line; Key -> key, ws, f64 -> bpm) {
                emit!(BmsExBPM(key, BPM(bpm)));
            } else if lex!(line; ws, f64 -> bpm) {
                emit!(BmsBPM(BPM(bpm)));
            }
        })

        if_prefix!("EXBPM" |line| { // #EXBPMxx <float>
            let mut key = Key(-1);
            let mut bpm = 0.0;
            if lex!(line; Key -> key, ws, f64 -> bpm) {
                diag!(BmsHasEXBPM at lineno);
                emit!(BmsExBPM(key, BPM(bpm)));
            }
        })

        if_prefix!("PLAYER"     -> BmsPlayer     value)
        if_prefix!("PLAYLEVEL"  -> BmsPlayLevel  value)
        if_prefix!("DIFFICULTY" -> BmsDifficulty value)
        if_prefix!("RANK"       -> BmsRank       value)
        if_prefix!("DEFEXRANK"  -> BmsDefExRank  value)
        if_prefix!("TOTAL"      -> BmsTotal      value)
        if_prefix!("LNTYPE"     -> BmsLNType     value)

        if_prefix!("LNOBJ" |line| { // #LNOBJ <key>
            let mut key = Key(-1);
            if lex!(line; ws, Key -> key) {
                emit!(BmsLNObj(key));
            }
        })

        if_prefix!("EXRANK" |line| { // #EXRANKxx <int>
            let mut key = Key(-1);
            let mut value = 0;
            if lex!(line; Key -> key, ws, int -> value) {
                emit!(BmsExRank(key, value));
            }
        })

        if_prefix!("SNRS:LANES" |line| { // #SNRS:LANES <key> <spec> <key> <spec> ...
            let words: ~[&str] = line.word_iter().collect();
            if !words.is_empty() && words.len() % 2 == 0 {
                let mut lanes = ~[];
                let mut okay = true;
                for i in iter::range_step(0, words.len(), 2) {
                    if words[i].len() != 2 { okay = false; break; }
                    match Key::from_str(words[i]) {
                        Some(key) => { lanes.push((key, words[i+1])); }
                        None => { okay = false; break; }
                    }
                }
                if okay {
                    emit!(BmsLanes(lanes));
                }
            }
        })

        if_prefix!("WAVCMD" |line| { // #WAVCMD <int> xx <int>
            let mut cmd = 0;
            let mut key = Key(0);
            let mut value = 0;
            if lex!(line; ws, int -> cmd, ws, Key -> key, ws, int -> value) {
                emit!(BmsWAVCmd(cmd, key, value));
            }
        })

        if_prefix!("WAV"      -> BmsWAV  key string)
        if_prefix!("VOLWAV"   -> BmsVolWAV    value)
        if_prefix!("MIDIFILE" -> BmsMIDIFile string)

        if_prefix!("EXWAV" |line| { // #EXWAV [pvf] <int>* <string>
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
                        for flag in flags.iter() {
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
                                emit!(BmsExWAV(key, pan, vol, freq, text));
                            }
                        }
                    }
                    None => {}
                }
            }
        })

        if_prefix!("BMP"     -> BmsBMP key string)
        if_prefix!("BACKBMP" -> BmsBackBMP string)
        if_prefix!("POORBGA" -> BmsPoorBGA  value)

        if_prefix!("EXBMP" |line| { // #EXBMPxx <int8>,<int8>,<int8>,<int8> <string>
            let mut key = Key(0);
            let mut argb = (0,0,0,0);
            let mut text = "";
            if lex!(line; Key -> key, ws, ARGB -> argb, ws, str -> text, ws*, !) {
                emit!(BmsExBMP(key, argb, text));
            }
        })

        if_prefix!("BGA" |line| { // #BGAxx yy <int> <int> <int> <int> <int> <int>
            let mut dst = Key(0);
            let mut src = Key(0);
            let mut bc = BlitCmd { dst: ImageRef(Key(0)), src: ImageRef(Key(0)),
                                   x1: 0, y1: 0, x2: 0, y2: 0, dx: 0, dy: 0 };
            if lex!(line; Key -> dst, ws, Key -> src, ws, int -> bc.x1, ws, int -> bc.y1, ws,
                    int -> bc.x2, ws, int -> bc.y2, ws, int -> bc.dx, ws, int -> bc.dy) {
                bc.src = ImageRef(src);
                bc.dst = ImageRef(dst);
                emit!(BmsBGA(bc));
            }
        })

        if_prefix!("@BGA" |line| { // #@BGAxx yy <int> <int> <int> <int> <int> <int>
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
                emit!(BmsBGA(bc));
            }
        })

        if_prefix!("SWBGA" |line| { // #SWBGAxx <int>:<int>:yy:<int>:<int8>,<int8>,<int8>,<int8> ...
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
                    emit!(BmsSwBGA(key, fr, time, linekey, doloop == 1, argb, pattern));
                }
            }
        })

        if_prefix!("ARGB" |line| { // #ARGBxx <int8>,<int8>,<int8>,<int8>
            let mut key = Key(0);
            let mut argb = (0,0,0,0);
            if lex!(line; Key -> key, ws, ARGB -> argb) {
                emit!(BmsARGB(key, argb));
            }
        })

        if_prefix!("CHARFILE"  -> BmsCharFile  string)
        if_prefix!("VIDEOFILE" -> BmsVideoFile string)
        if_prefix!("MOVIE"     -> BmsMovie     string)

        if_prefix!("SNRS:CANVASSIZE" |line| { // #SNRS:CANVASSIZE <int> <int>
            let mut width = 0;
            let mut height = 0;
            if lex!(line; ws, int -> width, ws, int -> height) {
                emit!(BmsCanvasSize(width, height));
            }
        })

        if_prefix!("STOP" |line| { // #STOPxx <int>
            let mut key = Key(-1);
            let mut duration = 0;
            if lex!(line; Key -> key, ws, int -> duration) {
                let duration = Measures(duration as f64 / 192.0);
                emit!(BmsStop(key, duration));
            }
        })

        if_prefix!("STP" |line| { // #STP<int>.<int> <int>
            let mut measure = 0;
            let mut frac = 0;
            let mut duration = 0;
            if lex!(line; Measure -> measure, '.', uint -> frac, ws,
                          int -> duration) && duration > 0 {
                let pos = measure as f64 + frac as f64 * 0.001;
                let duration = Seconds(duration as f64 * 0.001);
                emit!(BmsStp(pos, duration));
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

        if_prefix!("RANDOM"       -> BmsRandom (ws) value (BmsHasRANDOMWithoutWhitespace))
        if opts.autofix_commands {
            if_prefix!("RONDAM"   -> BmsRandom      value (BmsHasRONDAM))
        }
        if_prefix!("SETRANDOM"    -> BmsSetRandom   value)
        if_prefix!("ENDRANDOM"    -> BmsEndRandom        )
        if opts.autofix_commands {
            if_prefix!("IFEND"    -> BmsEndIf             (BmsHasIFEND))
            if_prefix!("IF"       -> BmsIf     (ws) value (BmsHasIFWithoutWhitespace))
        } else {
            if_prefix!("IF"       -> BmsIf          value)
        }
        if_prefix!("ELSEIF"       -> BmsElseIf      value)
        if_prefix!("ELSE"         -> BmsElse             )
        if_prefix!("ENDIF"        -> BmsEndIf            )
        if opts.autofix_commands {
            if_prefix!("END"      -> BmsEndIf             (BmsHasENDNotFollowedByIF))
        }

        let mut measure = 0;
        let mut chan = Key(0);
        let mut data = "";
        if lex!(line; Measure -> measure, Key -> chan, ':', ws*, str -> data, ws*, !) {
            if chan == Key(2) { // #xxx02:<float>
                let mut shorten = 0.0;
                if lex!(data; ws*, f64 -> shorten) {
                    emit!(BmsShorten(measure, shorten));
                }
            } else {
                emit!(BmsData(measure, chan, data));
            }
        }

        emit!(BmsUnknown(line));
    }
    ret
}

