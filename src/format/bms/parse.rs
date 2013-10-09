// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS parser.

use std::{io, iter};
use std::rand::Rng;

use util::opt_owned::{OptOwnedStr, IntoOptOwnedStr};
use format::obj::{BPM, Duration, Seconds, Measures};
use format::bms::types::{Key};
use format::bms::diag::*;
use format::bms::{ImageRef, BlitCmd};

/// A tuple of four `u8` values. Mainly used for BMS #ARGB command and its family.
pub type ARGB = (u8,u8,u8,u8);

/// Represents one line of BMS command that may affect the control flow.
#[deriving(Clone)]
pub enum BmsFlowCommand {
    BmsRandom(int),                             // #RANDOM
    BmsSetRandom(int),                          // #SETRANDOM
    BmsEndRandom,                               // #ENDRANDOM
    BmsIf(int),                                 // #IF
    BmsElseIf(int),                             // #ELSEIF
    BmsElse,                                    // #ELSE
    BmsEndIf,                                   // #ENDIF
    BmsSwitch(int),                             // #SWITCH
    BmsSetSwitch(int),                          // #SETSWITCH
    BmsEndSw,                                   // #ENDSW
    BmsCase(int),                               // #CASE
    BmsSkip,                                    // #SKIP
    BmsDef,                                     // #DEF
}

/// Represents one line of BMS file.
#[deriving(Clone)]
pub enum BmsCommand<'self> {
    BmsUnknown(OptOwnedStr<'self>),             // starting with `#` but unknown otherwise
    BmsTitle(OptOwnedStr<'self>),               // #TITLE
    BmsSubtitle(OptOwnedStr<'self>),            // #SUBTITLE
    BmsGenre(OptOwnedStr<'self>),               // #GENRE
    BmsArtist(OptOwnedStr<'self>),              // #ARTIST
    BmsSubartist(OptOwnedStr<'self>),           // #SUBARTIST
    BmsMaker(OptOwnedStr<'self>),               // #MAKER
    BmsComment(OptOwnedStr<'self>),             // #COMMENT
    BmsStageFile(OptOwnedStr<'self>),           // #STAGEFILE
    BmsBanner(OptOwnedStr<'self>),              // #BANNER
    BmsPathWAV(OptOwnedStr<'self>),             // #PATH_WAV
    BmsBPM(BPM),                                // #BPM (without a following alphanumeric key)
    BmsExBPM(Key, BPM),                         // #EXBPM or #BPMxx
    BmsPlayer(int),                             // #PLAYER
    BmsLanes(~[(Key, OptOwnedStr<'self>)]),     // #SNRS:LANES (experimental)
    BmsPlayLevel(int),                          // #PLAYLEVEL
    BmsDifficulty(int),                         // #DIFFICULTY
    BmsRank(int),                               // #RANK
    BmsDefExRank(int),                          // #DEFEXRANK
    BmsExRank(Key, int),                        // #EXRANK
    BmsTotal(int),                              // #TOTAL
    BmsLNType(int),                             // #LNTYPE
    BmsLNObj(Key),                              // #LNOBJ
    BmsWAV(Key, OptOwnedStr<'self>),            // #WAV
    BmsWAVCmd(int, Key, int),                   // #WAVCMD
    BmsExWAV(Key, Option<int>, Option<int>, Option<int>, OptOwnedStr<'self>), // #EXWAV
    BmsVolWAV(int),                             // #VOLWAV
    BmsMIDIFile(OptOwnedStr<'self>),            // #MIDIFILE
    BmsBMP(Key, OptOwnedStr<'self>),            // #BMP
    BmsExBMP(Key, ARGB, OptOwnedStr<'self>),    // #EXBMP
    BmsBackBMP(OptOwnedStr<'self>),             // #BACKBMP
    BmsBGA(BlitCmd),                            // #BGA or #@BGA
    BmsPoorBGA(int),                            // #POORBGA
    BmsSwBGA(Key, int, int, Key, bool, ARGB, OptOwnedStr<'self>), // #SWBGA
    BmsARGB(Key, ARGB),                         // #ARGB
    BmsCharFile(OptOwnedStr<'self>),            // #CHARFILE
    BmsVideoFile(OptOwnedStr<'self>),           // #VIDEOFILE
    BmsMovie(OptOwnedStr<'self>),               // #MOVIE
    BmsCanvasSize(int, int),                    // #SNRS:CANVASSIZE (experimental)
    BmsStop(Key, Duration),                     // #STOP
    BmsStp(f64, Duration),                      // #STP
    BmsText(Key, OptOwnedStr<'self>),           // #TEXT or #SONG
    BmsOption(OptOwnedStr<'self>),              // #OPTION
    BmsChangeOption(Key, OptOwnedStr<'self>),   // #CHANGEOPTION
    BmsShorten(uint, f64),                      // #xxx02
    BmsData(uint, Key, OptOwnedStr<'self>),     // #xxxyy:...
    BmsFlow(BmsFlowCommand),                    // flow commands (#RANDOM, #IF, #ENDIF etc.)
}

impl<'self> BmsCommand<'self> {
    pub fn into_send(self) -> BmsCommand<'static> {
        match self {
            BmsUnknown(s) => BmsUnknown(s.into_send()),
            BmsTitle(s) => BmsTitle(s.into_send()),
            BmsSubtitle(s) => BmsSubtitle(s.into_send()),
            BmsGenre(s) => BmsGenre(s.into_send()),
            BmsArtist(s) => BmsArtist(s.into_send()),
            BmsSubartist(s) => BmsSubartist(s.into_send()),
            BmsMaker(s) => BmsMaker(s.into_send()),
            BmsComment(s) => BmsComment(s.into_send()),
            BmsStageFile(s) => BmsStageFile(s.into_send()),
            BmsBanner(s) => BmsBanner(s.into_send()),
            BmsPathWAV(s) => BmsPathWAV(s.into_send()),
            BmsBPM(bpm) => BmsBPM(bpm),
            BmsExBPM(key, bpm) => BmsExBPM(key, bpm),
            BmsPlayer(v) => BmsPlayer(v),
            BmsLanes(lanes) =>
                BmsLanes(lanes.move_iter().map(|(lane,spec)| (lane,spec.into_send())).collect()),
            BmsPlayLevel(v) => BmsPlayLevel(v),
            BmsDifficulty(v) => BmsDifficulty(v),
            BmsRank(v) => BmsRank(v),
            BmsDefExRank(v) => BmsDefExRank(v),
            BmsExRank(key, v) => BmsExRank(key, v),
            BmsTotal(v) => BmsTotal(v),
            BmsLNType(lntype) => BmsLNType(lntype),
            BmsLNObj(key) => BmsLNObj(key),
            BmsWAV(key, s) => BmsWAV(key, s.into_send()),
            BmsWAVCmd(cmd, key, v) => BmsWAVCmd(cmd, key, v),
            BmsExWAV(key, pan, vol, freq, s) => BmsExWAV(key, pan, vol, freq, s.into_send()),
            BmsVolWAV(v) => BmsVolWAV(v),
            BmsMIDIFile(s) => BmsMIDIFile(s.into_send()),
            BmsBMP(key, s) => BmsBMP(key, s.into_send()),
            BmsExBMP(key, argb, s) => BmsExBMP(key, argb, s.into_send()),
            BmsBackBMP(s) => BmsBackBMP(s.into_send()),
            BmsBGA(blitcmd) => BmsBGA(blitcmd),
            BmsPoorBGA(poorbga) => BmsPoorBGA(poorbga),
            BmsSwBGA(key, fr, time, line, doloop, argb, pattern) =>
                BmsSwBGA(key, fr, time, line, doloop, argb, pattern.into_send()),
            BmsARGB(key, argb) => BmsARGB(key, argb),
            BmsCharFile(s) => BmsCharFile(s.into_send()),
            BmsVideoFile(s) => BmsVideoFile(s.into_send()),
            BmsMovie(s) => BmsMovie(s.into_send()),
            BmsCanvasSize(w, h) => BmsCanvasSize(w, h),
            BmsStop(key, dur) => BmsStop(key, dur),
            BmsStp(pos, dur) => BmsStp(pos, dur),
            BmsText(key, s) => BmsText(key, s.into_send()),
            BmsOption(opt) => BmsOption(opt.into_send()),
            BmsChangeOption(key, opt) => BmsChangeOption(key, opt.into_send()),
            BmsShorten(measure, shorten) => BmsShorten(measure, shorten),
            BmsData(measure, chan, data) => BmsData(measure, chan, data.into_send()),
            BmsFlow(flow) => BmsFlow(flow),
        }
    }
}

impl<'self> ToStr for BmsCommand<'self> {
    /// Returns a reconstructed line for given BMS command.
    fn to_str(&self) -> ~str {
        fn argb_to_str((a,r,g,b): ARGB) -> ~str { format!("{},{},{},{}", a, r, g, b) }

        match *self {
            BmsUnknown(ref s) => format!("\\#{}", *s),
            BmsTitle(ref s) => format!("\\#TITLE {}", *s),
            BmsSubtitle(ref s) => format!("\\#SUBTITLE {}", *s),
            BmsGenre(ref s) => format!("\\#GENRE {}", *s),
            BmsArtist(ref s) => format!("\\#ARTIST {}", *s),
            BmsSubartist(ref s) => format!("\\#SUBARTIST {}", *s),
            BmsMaker(ref s) => format!("\\#MAKER {}", *s),
            BmsComment(ref s) => format!("\\#COMMENT {}", *s),
            BmsStageFile(ref s) => format!("\\#STAGEFILE {}", *s),
            BmsBanner(ref s) => format!("\\#BANNER {}", *s),
            BmsPathWAV(ref s) => format!("\\#PATH_WAV {}", *s),
            BmsBPM(BPM(bpm)) => format!("\\#BPM {}", bpm),
            BmsExBPM(key, BPM(bpm)) => format!("\\#BPM{} {}", key.to_str(), bpm),
            BmsPlayer(v) => format!("\\#PLAYER {}", v),
            BmsLanes(ref lanes) => {
                let specs: ~[~str] = do lanes.iter().map |&(lane,ref spec)| {
                    format!(" {} {}", lane.to_str(), *spec)
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
            BmsWAV(key, ref s) => format!("\\#WAV{} {}", key.to_str(), *s),
            BmsWAVCmd(cmd, key, v) => format!("\\#WAVCMD {:02} {}{}", cmd, key.to_str(), v),
            BmsExWAV(_key, None, None, None, ref _s) => fail!(~"unsupported"),
            BmsExWAV(key, pan, vol, freq, ref s) => {
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
                format!("\\#EXWAV{} {}{} {}", key.to_str(), flags, opts, *s)
            },
            BmsVolWAV(v) => format!("\\#VOLWAV {}", v),
            BmsMIDIFile(ref s) => format!("\\#MIDIFILE {}", *s),
            BmsBMP(key, ref s) => format!("\\#BMP{} {}", key.to_str(), *s),
            BmsExBMP(key, argb, ref s) =>
                format!("\\#EXBMP{} {} {}", key.to_str(), argb_to_str(argb), *s),
            BmsBackBMP(ref s) => format!("\\#BACKBMP {}", *s),
            BmsBGA(BlitCmd { dst, src, x1, y1, x2, y2, dx, dy }) =>
                format!("\\#BGA{} {} {} {} {} {} {} {}",
                     dst.to_str(), src.to_str(), x1, y1, x2, y2, dx, dy),
            BmsPoorBGA(poorbga) => format!("\\#POORBGA {}", poorbga),
            BmsSwBGA(key, fr, time, line, doloop, argb, ref pattern) =>
                format!("\\#SWBGA{} {}:{}:{}:{}:{} {}",
                     key.to_str(), fr, time, line.to_str(), if doloop {1} else {0},
                     argb_to_str(argb), *pattern),
            BmsARGB(key, argb) => format!("\\#ARGB{} {}", key.to_str(), argb_to_str(argb)),
            BmsCharFile(ref s) => format!("\\#CHARFILE {}", *s),
            BmsVideoFile(ref s) => format!("\\#VIDEOFILE {}", *s),
            BmsMovie(ref s) => format!("\\#MOVIE {}", *s),
            BmsCanvasSize(w, h) => format!("\\#SNRS:CANVASSIZE {} {}", w, h),
            BmsStop(key, Measures(dur)) =>
                format!("\\#STOP{} {}", key.to_str(), (dur * 192.0) as int),
            BmsStop(*) => fail!(~"unsupported"),
            BmsStp(pos, Seconds(dur)) => format!("\\#STP {:07.3} {}", pos, (dur * 1000.0) as int),
            BmsStp(*) => fail!(~"unsupported"),
            BmsText(key, ref s) => format!("\\#TEXT{} {}", key.to_str(), *s),
            BmsOption(ref opt) => format!("\\#OPTION {}", *opt),
            BmsChangeOption(key, ref opt) => format!("\\#CHANGEOPTION{} {}", key.to_str(), *opt),
            BmsShorten(measure, shorten) => format!("\\#{:03}02:{}", measure, shorten),
            BmsData(measure, chan, ref data) =>
                format!("\\#{:03}{}:{}", measure, chan.to_str(), *data),
            BmsFlow(BmsRandom(val)) => format!("\\#RANDOM {}", val),
            BmsFlow(BmsSetRandom(val)) => format!("\\#SETRANDOM {}", val),
            BmsFlow(BmsEndRandom) => ~"#ENDRANDOM",
            BmsFlow(BmsIf(val)) => format!("\\#IF {}", val),
            BmsFlow(BmsElseIf(val)) => format!("\\#ELSEIF {}", val),
            BmsFlow(BmsElse) => ~"#ELSE",
            BmsFlow(BmsEndIf) => ~"#ENDIF",
            BmsFlow(BmsSwitch(val)) => format!("\\#SWITCH {}", val),
            BmsFlow(BmsSetSwitch(val)) => format!("\\#SETSWITCH {}", val),
            BmsFlow(BmsEndSw) => ~"#ENDSW",
            BmsFlow(BmsCase(val)) => format!("\\#CASE {}", val),
            BmsFlow(BmsSkip) => ~"#SKIP",
            BmsFlow(BmsDef) => ~"#DEF",
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

/// Iterates over the parsed BMS commands, including flow commands.
pub fn each_bms_command_with_flow<Listener:BmsMessageListener>(
                                f: @io::Reader, opts: &BmsParserOptions, callback: &mut Listener,
                                blk: &fn(uint,BmsCommand) -> bool) -> bool {
    use util::std::str::from_fixed_utf8_bytes;
    use std::ascii::StrAsciiExt;

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
                if blk(lineno, $e) { loop; } else { ret = false; break 'eachline; }
            })
        )

        // helper macro for `if_prefix!`; "tt" (token tree) cannot directly pasted to the AST.
        macro_rules! tt_to_expr(($e:expr) => ($e))

        // matches a prefix and performs predefined or custom actions
        macro_rules! if_prefix(
            ($prefix:tt string -> $constr:expr) => (
                if_prefix!($prefix |line| { // #<command> <string>
                    let mut text = "";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        emit!($constr(text.into_opt_owned_str()));
                    }
                })
            );
            ($prefix:tt string -> $constr:expr; $diag:expr) => (
                if_prefix!($prefix |line| { // #<command> <string>
                    let mut text = "";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        diag!($diag at lineno);
                        emit!($constr(text.into_opt_owned_str()));
                    }
                })
            );
            ($prefix:tt value -> $constr:expr) => (
                if_prefix!($prefix |line| { // #<command> <int>
                    let mut value = 0;
                    if lex!(line; ws, int -> value) {
                        emit!($constr(value));
                    }
                })
            );
            ($prefix:tt value -> $constr:expr; $diag:expr) => (
                if_prefix!($prefix |line| { // #<command> <int>
                    let mut value = 0;
                    if lex!(line; ws, int -> value) {
                        diag!($diag at lineno);
                        emit!($constr(value));
                    }
                })
            );
            ($prefix:tt (ws) value -> $constr:expr; $diag:expr) => (
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
            ($prefix:tt key string -> $constr:expr) => (
                if_prefix!($prefix |line| { // #<command>xx <string>
                    let mut key = Key(-1);
                    let mut text = "";
                    if lex!(line; Key -> key, ws, str -> text, ws*, !) {
                        emit!($constr(key, text.into_opt_owned_str()));
                    }
                })
            );
            ($prefix:tt key string -> $constr:expr; $diag:expr) => (
                if_prefix!($prefix |line| { // #<command>xx <string>
                    let mut key = Key(-1);
                    let mut text = "";
                    if lex!(line; Key -> key, ws, str -> text, ws*, !) {
                        diag!($diag at lineno);
                        emit!($constr(key, text.into_opt_owned_str()));
                    }
                })
            );
            ($prefix:tt -> $constr:expr) => (
                // Rust: unreachable code sometimes causes an ICE. (#7344)
                if upperline.starts_with(tt_to_expr!($prefix)) {
                    emit!($constr);
                }
            );
            ($prefix:tt -> $constr:expr; $diag:expr) => (
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
                    emit!(BmsUnknown($v.into_opt_owned_str())); // no more matching possible
                }
            })
        )

        if_prefix!("TITLE"     string -> BmsTitle)
        if_prefix!("SUBTITLE"  string -> BmsSubtitle)
        if_prefix!("GENRE"     string -> BmsGenre)
        if opts.autofix_commands {
            if_prefix!("GENLE" string -> BmsGenre; BmsHasGENLE)
        }
        if_prefix!("ARTIST"    string -> BmsArtist)
        if_prefix!("SUBARTIST" string -> BmsSubartist)
        if_prefix!("MAKER"     string -> BmsMaker)
        if_prefix!("COMMENT"   string -> BmsComment) // quotes are stripped by caller
        if_prefix!("STAGEFILE" string -> BmsStageFile)
        if_prefix!("BANNER"    string -> BmsBanner)
        if_prefix!("PATH_WAV"  string -> BmsPathWAV)

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

        if_prefix!("PLAYER"     value -> BmsPlayer)
        if_prefix!("PLAYLEVEL"  value -> BmsPlayLevel)
        if_prefix!("DIFFICULTY" value -> BmsDifficulty)
        if_prefix!("RANK"       value -> BmsRank)
        if_prefix!("DEFEXRANK"  value -> BmsDefExRank)
        if_prefix!("TOTAL"      value -> BmsTotal)
        if_prefix!("LNTYPE"     value -> BmsLNType)

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
                        Some(key) => { lanes.push((key, words[i+1].into_opt_owned_str())); }
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

        if_prefix!("WAV"  key string -> BmsWAV)
        if_prefix!("VOLWAV"    value -> BmsVolWAV)
        if_prefix!("MIDIFILE" string -> BmsMIDIFile)

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
                                emit!(BmsExWAV(key, pan, vol, freq, text.into_opt_owned_str()));
                            }
                        }
                    }
                    None => {}
                }
            }
        })

        if_prefix!("BMP" key string -> BmsBMP)
        if_prefix!("BACKBMP" string -> BmsBackBMP)
        if_prefix!("POORBGA"  value -> BmsPoorBGA)

        if_prefix!("EXBMP" |line| { // #EXBMPxx <int8>,<int8>,<int8>,<int8> <string>
            let mut key = Key(0);
            let mut argb = (0,0,0,0);
            let mut text = "";
            if lex!(line; Key -> key, ws, ARGB -> argb, ws, str -> text, ws*, !) {
                emit!(BmsExBMP(key, argb, text.into_opt_owned_str()));
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
                    emit!(BmsSwBGA(key, fr, time, linekey, doloop == 1,
                                   argb, pattern.into_opt_owned_str()));
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

        if_prefix!("CHARFILE"  string -> BmsCharFile)
        if_prefix!("VIDEOFILE" string -> BmsVideoFile)
        if_prefix!("MOVIE"     string -> BmsMovie)

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

        if_prefix!("TEXT"         key string -> BmsText)
        if_prefix!("SONG"         key string -> BmsText; BmsHasSONG)
        if_prefix!("OPTION"           string -> BmsOption)
        if_prefix!("CHANGEOPTION" key string -> BmsChangeOption)

        macro_rules! makeBmsFlow(($e:expr) => (|val| BmsFlow($e(val))))

        // we parse SWITCH directives first since we need to parse #END as #ENDIF.
        if_prefix!("SWITCH"    value -> makeBmsFlow!(BmsSwitch))
        if_prefix!("SETSWITCH" value -> makeBmsFlow!(BmsSetSwitch))
        if_prefix!("ENDSW"           -> BmsFlow(BmsEndSw))
        if_prefix!("CASE"      value -> makeBmsFlow!(BmsCase))
        if_prefix!("SKIP"            -> BmsFlow(BmsSkip))
        if_prefix!("DEF"             -> BmsFlow(BmsDef))

        if_prefix!("RANDOM" (ws) value -> makeBmsFlow!(BmsRandom); BmsHasRANDOMWithoutWhitespace)
        if opts.autofix_commands {
            if_prefix!("RONDAM"  value -> makeBmsFlow!(BmsRandom); BmsHasRONDAM)
        }
        if_prefix!("SETRANDOM"   value -> makeBmsFlow!(BmsSetRandom))
        if_prefix!("ENDRANDOM"         -> BmsFlow(BmsEndRandom))
        if opts.autofix_commands {
            if_prefix!("IFEND"         -> BmsFlow(BmsEndIf); BmsHasIFEND)
            if_prefix!("IF" (ws) value -> makeBmsFlow!(BmsIf); BmsHasIFWithoutWhitespace)
        } else {
            if_prefix!("IF"      value -> makeBmsFlow!(BmsIf))
        }
        if_prefix!("ELSEIF"      value -> makeBmsFlow!(BmsElseIf))
        if_prefix!("ELSE"              -> BmsFlow(BmsElse))
        if_prefix!("ENDIF"             -> BmsFlow(BmsEndIf))
        if opts.autofix_commands {
            if_prefix!("END"           -> BmsFlow(BmsEndIf); BmsHasENDNotFollowedByIF)
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
                emit!(BmsData(measure, chan, data.into_opt_owned_str()));
            }
        }

        emit!(BmsUnknown(line.into_opt_owned_str()));
    }
    ret
}

/// The state of the block, for determining which lines should be processed.
#[deriving(Eq)]
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
#[deriving(Eq)]
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

/// Iterates over the parsed BMS commands, with flow commands have been preprocessed.
pub fn each_bms_command<R:Rng,Listener:BmsMessageListener>(
                                f: @io::Reader, r: &mut R,
                                opts: &BmsParserOptions, callback: &mut Listener,
                                blk: &fn(uint,BmsCommand) -> bool) -> bool {
    let mut blocks = ~[Block { val: None, state: Outside, skip: false }];

    // internal callback wrapper, both the caller and the callee have to use the callback
    let mut callback_ = |line, msg| callback.on_message(line, msg);

    let mut ret = true;
    do each_bms_command_with_flow(f, opts, &mut callback_) |lineno, cmd| {
        assert!(!blocks.is_empty());
        match (cmd, blocks.last().inactive()) {
            (BmsFlow(ref flow), inactive) => match *flow {
                BmsRandom(val) | BmsSetRandom(val) => {
                    let val = if val <= 0 {None} else {Some(val)};
                    let setrandom = match *flow { BmsSetRandom(*) => true, _ => false };

                    // do not generate a random value if the entire block is skipped (but it
                    // still marks the start of block)
                    let generated = do val.and_then |val| {
                        if setrandom {
                            Some(val)
                        } else if !inactive {
                            Some(r.gen_integer_range(1, val + 1))
                        } else {
                            None
                        }
                    };
                    blocks.push(Block { val: generated, state: Outside, skip: inactive });
                }
                BmsEndRandom => {
                    if blocks.len() > 1 { blocks.pop(); }
                }
                BmsIf(val) | BmsElseIf(val) => {
                    let val = if val <= 0 {None} else {Some(val)};
                    let haspriorelse = match *flow { BmsElseIf(*) => true, _ => false };

                    // Rust: `blocks.last_ref()` may be useful?
                    let last = &mut blocks[blocks.len() - 1];
                    last.state =
                        if (!haspriorelse && !last.state.inactive()) || last.state == Ignore {
                            if val.is_none() || val != last.val {Ignore} else {Process}
                        } else {
                            NoFurther
                        };
                }
                BmsElse => {
                    let last = &mut blocks[blocks.len() - 1];
                    last.state = if last.state == Ignore {Process} else {NoFurther};
                }
                BmsEndIf => {
                    let lastinside = blocks.iter().rposition(|&i| i.state != Outside); // XXX #3511
                    for &idx in lastinside.iter() {
                        if idx > 0 { blocks.truncate(idx + 1); }
                    }

                    let last = &mut blocks[blocks.len() - 1];
                    last.state = Outside;
                }
                BmsSwitch(*) | BmsSetSwitch(*) | BmsEndSw | BmsCase(*) | BmsSkip | BmsDef => {
                    if !callback.on_message(Some(lineno), BmsHasUnimplementedFlow) { ret = false; }
                }
            },
            (cmd, false) => {
                if !blk(lineno, cmd) { ret = false; }
            }
            (_cmd, true) => {}
        }
        ret
    };
    ret
}

