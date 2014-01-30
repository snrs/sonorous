// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS parser.

use std::{iter, f64, char};
use std::rand::Rng;
use encoding::EncodingRef;

use util::into_send::IntoSend;
use util::opt_owned::{OptOwnedStr, IntoOptOwnedStr};
use format::obj::{BPM, Duration, Seconds, Measures, ImageSlice};
use format::bms::types::{Key, PartialKey};
use format::bms::diag::*;
use format::bms::preproc::{BmsFlowCommand, Preprocessor};
pub use format::bms::preproc::{BmsRandom, BmsSetRandom, BmsEndRandom};
pub use format::bms::preproc::{BmsIf, BmsElseIf, BmsElse, BmsEndIf};
pub use format::bms::preproc::{BmsSwitch, BmsSetSwitch, BmsEndSw, BmsCase, BmsSkip, BmsDef};
use format::bms::encoding::{decode_stream, guess_decode_stream};

/// A tuple of four `u8` values. Mainly used for BMS #ARGB command and its family.
pub type ARGB = (u8,u8,u8,u8);

/// Represents one line of BMS file.
#[deriving(Clone)]
pub enum BmsCommand<'r> {
    BmsUnknown(OptOwnedStr<'r>),                // starting with `#` but unknown otherwise
    BmsTitle(OptOwnedStr<'r>),                  // #TITLE
    BmsSubtitle(OptOwnedStr<'r>),               // #SUBTITLE
    BmsGenre(OptOwnedStr<'r>),                  // #GENRE
    BmsArtist(OptOwnedStr<'r>),                 // #ARTIST
    BmsSubartist(OptOwnedStr<'r>),              // #SUBARTIST
    BmsMaker(OptOwnedStr<'r>),                  // #MAKER
    BmsComment(OptOwnedStr<'r>),                // #COMMENT
    BmsStageFile(OptOwnedStr<'r>),              // #STAGEFILE
    BmsBanner(OptOwnedStr<'r>),                 // #BANNER
    BmsPathWAV(OptOwnedStr<'r>),                // #PATH_WAV
    BmsBPM(BPM),                                // #BPM (without a following alphanumeric key)
    BmsExBPM(Key, BPM),                         // #EXBPM or #BPMxx
    BmsPlayer(int),                             // #PLAYER
    BmsLanes(~[(Key, OptOwnedStr<'r>)]),        // #SNRS:LANES (experimental)
    BmsPlayLevel(int),                          // #PLAYLEVEL
    BmsDifficulty(int),                         // #DIFFICULTY
    BmsRank(int),                               // #RANK
    BmsDefExRank(int),                          // #DEFEXRANK
    BmsExRank(Key, int),                        // #EXRANK
    BmsTotal(int),                              // #TOTAL
    BmsLNType(int),                             // #LNTYPE
    BmsLNObj(Key),                              // #LNOBJ
    BmsWAV(Key, OptOwnedStr<'r>),               // #WAV
    BmsWAVCmd(int, Key, int),                   // #WAVCMD
    BmsExWAV(Key, Option<int>, Option<int>, Option<int>, OptOwnedStr<'r>), // #EXWAV
    BmsVolWAV(int),                             // #VOLWAV
    BmsMIDIFile(OptOwnedStr<'r>),               // #MIDIFILE
    BmsBMP(Key, OptOwnedStr<'r>),               // #BMP
    BmsExBMP(Key, ARGB, OptOwnedStr<'r>),       // #EXBMP
    BmsBackBMP(OptOwnedStr<'r>),                // #BACKBMP
    BmsBGA(Key, Key, ImageSlice),               // #BGA or #@BGA
    BmsPoorBGA(int),                            // #POORBGA
    BmsSwBGA(Key, int, int, Key, bool, ARGB, OptOwnedStr<'r>), // #SWBGA
    BmsARGB(Key, ARGB),                         // #ARGB
    BmsCharFile(OptOwnedStr<'r>),               // #CHARFILE
    BmsVideoFile(OptOwnedStr<'r>),              // #VIDEOFILE
    BmsMovie(OptOwnedStr<'r>),                  // #MOVIE
    BmsCanvasSize(int, int),                    // #SNRS:CANVASSIZE (experimental)
    BmsStop(Key, Duration),                     // #STOP
    BmsStp(f64, Duration),                      // #STP
    BmsText(Key, OptOwnedStr<'r>),              // #TEXT or #SONG
    BmsOption(OptOwnedStr<'r>),                 // #OPTION
    BmsChangeOption(Key, OptOwnedStr<'r>),      // #CHANGEOPTION
    BmsShorten(uint, f64),                      // #xxx02
    BmsData(uint, Key, OptOwnedStr<'r>),        // #xxxyy:...
    BmsFlow(BmsFlowCommand),                    // flow commands (#RANDOM, #IF, #ENDIF etc.)
}

impl<'r> IntoSend<BmsCommand<'static>> for BmsCommand<'r> {
    fn into_send(self) -> BmsCommand<'static> {
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
            BmsBGA(dst, src, slice) => BmsBGA(dst, src, slice),
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

impl<'r> ToStr for BmsCommand<'r> {
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
                let specs: ~[~str] = lanes.iter().map(|&(lane,ref spec)| {
                    format!(" {} {}", lane.to_str(), *spec)
                }).collect();
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
            BmsBGA(dst, src, ImageSlice { sx, sy, dx, dy, w, h }) =>
                format!("\\#BGA{} {} {} {} {} {} {} {}",
                     dst.to_str(), src.to_str(), sx, sy, sx + w, sy + h, dx, dy),
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
            BmsStop(..) => fail!(~"unsupported"),
            BmsStp(pos, Seconds(dur)) => format!("\\#STP {:07.3} {}", pos, (dur * 1000.0) as int),
            BmsStp(..) => fail!(~"unsupported"),
            BmsText(key, ref s) => format!("\\#TEXT{} {}", key.to_str(), *s),
            BmsOption(ref opt) => format!("\\#OPTION {}", *opt),
            BmsChangeOption(key, ref opt) => format!("\\#CHANGEOPTION{} {}", key.to_str(), *opt),
            BmsShorten(measure, shorten) => format!("\\#{:03}02:{}", measure, shorten),
            BmsData(measure, chan, ref data) =>
                format!("\\#{:03}{}:{}", measure, chan.to_str(), *data),
            BmsFlow(ref flowcmd) => flowcmd.to_str(),
        }
    }
}

/// Parser options for BMS format.
pub struct BmsParserOptions {
    /// Enables a parsing of several obviously mistyped commands. (Default: true)
    autofix_commands: bool,
    /// Disables an automatic encoding detection and forces the use of given encoding.
    force_encoding: Option<EncodingRef>,
}

impl BmsParserOptions {
    /// Returns default parser options.
    pub fn new() -> BmsParserOptions {
        BmsParserOptions { autofix_commands: true, force_encoding: None }
    }
}

/// Returns true if the character is treated as a whitespace for the purpose of parsing.
///
/// Includes the C0 whitespace (e.g. `\n`, handled in `char::is_whitespace`),
/// the Zs/Zl/Zp category (also handled in `char::is_whitespace`),
/// and a portion of the Cf category: U+0085 (NEL), U+FEFF (BOM).
fn is_whitespace_or_similar(c: char) -> bool {
    char::is_whitespace(c) || c == '\u0085' || c == '\ufeff'
}

/// Same as `each_bms_command_with_flow` below.
/// Virtualized arguments are used instead of generic parameters for the smaller binary.
fn each_bms_command_with_flow_(f: &mut Reader, opts: &BmsParserOptions,
                               callback: &mut BmsMessageListener,
                               blk: |uint,BmsCommand| -> bool) -> bool {
    use std::ascii::StrAsciiExt;
    use util::std::str::{StrUtil, ShiftablePrefix};

    let (file, encoding, confidence) = match opts.force_encoding {
        Some(enc) => (decode_stream(f, enc), enc, f64::INFINITY),
        None => guess_decode_stream(f),
    };
    if !callback.on_message(None, BmsUsesEncoding(encoding.name(), confidence)) {
        return false;
    }

    let mut lineno = 0;
    let mut ret = true;
    'eachline: for line in file.split('\u000a') {
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

        macro_rules! warn_on_partial_key(
            ($e:expr) => ({
                let key: PartialKey = $e;
                if key.is_partial() {
                    diag!(BmsHasOneDigitAlphanumericKey at lineno);
                }
                key.into_key()
            })
        )

        // skip non-command lines
        let line = line.trim_left_chars(&is_whitespace_or_similar);
        if line.is_empty() { continue; }
        let (ch, line) = line.slice_shift_char();
        if ch == '\uff03' {
            diag!(BmsHasFullWidthSharp at lineno);
        } else if ch != '#' {
            continue;
        }

        let upperline = line.to_ascii_upper();

        // emits a `BmsCommand` and restarts the loop
        macro_rules! emit(
            ($e:expr) => ({
                if blk(lineno, $e) { continue; } else { ret = false; break 'eachline; }
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
                if_prefix!($prefix |key, line| { // #<command>xx <string>
                    let mut text = "";
                    if lex!(line; str -> text, ws*, !) {
                        emit!($constr(key, text.into_opt_owned_str()));
                    }
                })
            );
            ($prefix:tt key string -> $constr:expr; $diag:expr) => (
                if_prefix!($prefix |key, line| { // #<command>xx <string>
                    let mut text = "";
                    if lex!(line; str -> text, ws*, !) {
                        diag!($diag at lineno);
                        emit!($constr(key, text.into_opt_owned_str()));
                    }
                })
            );
            ($prefix:tt -> $constr:expr) => (
                if_prefix!($prefix |_key, _line| {
                    emit!($constr);
                })
            );
            ($prefix:tt -> $constr:expr; $diag:expr) => (
                if_prefix!($prefix |_key, _line| {
                    diag!($diag at lineno);
                    emit!($constr);
                })
            );
            ($prefix:tt |$v:ident| $then:expr) => ({ // #<command>...
                let prefix: &'static str = tt_to_expr!($prefix);
                if upperline.starts_with(prefix) {
                    let $v = line.slice_from(prefix.len());
                    let _ = $v; // removes warning
                    $then;
                    emit!(BmsUnknown($v.into_opt_owned_str())); // no more matching possible
                }
            });
            ($prefix:tt |$k:ident, $v:ident| $then:expr) => ({ // #<command>xx ...
                // Rust: we cannot write this via an existing `if_prefix!("..." |v| ...)` syntax??
                let prefix: &'static str = tt_to_expr!($prefix);
                if upperline.starts_with(prefix) {
                    let line = line.slice_from(prefix.len());
                    let mut key = PartialKey::dummy();
                    let mut text = "";
                    if lex!(line; PartialKey -> key, ws, str -> text, !) {
                        let $k = warn_on_partial_key!(key);
                        let $v = text;
                        let _ = $k; // removes warning
                        let _ = $v; // ditto
                        $then;
                    }
                    emit!(BmsUnknown(line.into_opt_owned_str())); // no more matching possible
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
            let mut key = PartialKey::dummy();
            let mut bpm = 0.0;
            if lex!(line; PartialKey -> key, ws, f64 -> bpm) {
                let key = warn_on_partial_key!(key);
                emit!(BmsExBPM(key, BPM(bpm)));
            } else if lex!(line; ws, f64 -> bpm) {
                emit!(BmsBPM(BPM(bpm)));
            }
        })

        if_prefix!("EXBPM" |key, line| { // #EXBPMxx <float>
            let mut bpm = 0.0;
            if lex!(line; f64 -> bpm) {
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
            let mut key = PartialKey::dummy();
            if lex!(line; ws, PartialKey -> key) {
                let key = warn_on_partial_key!(key);
                emit!(BmsLNObj(key));
            }
        })

        if_prefix!("EXRANK" |key, line| { // #EXRANKxx <int>
            let mut value = 0;
            if lex!(line; int -> value) {
                emit!(BmsExRank(key, value));
            }
        })

        if_prefix!("SNRS:LANES" |line| { // #SNRS:LANES <key> <spec> <key> <spec> ...
            let words: ~[&str] = line.words().collect();
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
            let mut key = PartialKey::dummy();
            let mut value = 0;
            if lex!(line; ws, int -> cmd, ws, PartialKey -> key, ws, int -> value) {
                let key = warn_on_partial_key!(key);
                emit!(BmsWAVCmd(cmd, key, value));
            }
        })

        if_prefix!("WAV"  key string -> BmsWAV)
        if_prefix!("VOLWAV"    value -> BmsVolWAV)
        if_prefix!("MIDIFILE" string -> BmsMIDIFile)

        if_prefix!("EXWAV" |key, line| { // #EXWAVxx [pvf] <int>* <string>
            let mut line = line;
            match line.find(|c: char| c.is_whitespace()) {
                Some(sep) => {
                    let flags = line.slice_to(sep);
                    let mut pan = None;
                    let mut vol = None;
                    let mut freq = None;

                    line = line.slice_from(sep);
                    let mut okay = true;
                    for flag in flags.chars() {
                        let mut value = 0;
                        if !lex!(line; ws, int -> value, str -> line) { okay = false; break; }
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
                        if lex!(line; ws, str -> text, ws*, !) {
                            emit!(BmsExWAV(key, pan, vol, freq, text.into_opt_owned_str()));
                        }
                    }
                }
                None => {}
            }
        })

        if_prefix!("BMP" key string -> BmsBMP)
        if_prefix!("BACKBMP" string -> BmsBackBMP)
        if_prefix!("POORBGA"  value -> BmsPoorBGA)

        if_prefix!("EXBMP" |key, line| { // #EXBMPxx <int8>,<int8>,<int8>,<int8> <string>
            let mut argb = (0,0,0,0);
            let mut text = "";
            if lex!(line; ARGB -> argb, ws, str -> text, ws*, !) {
                emit!(BmsExBMP(key, argb, text.into_opt_owned_str()));
            }
        })

        if_prefix!("BGA" |dst, line| { // #BGAxx yy <int> <int> <int> <int> <int> <int>
            let mut src = PartialKey::dummy();
            let mut x1 = 0; let mut y1 = 0; let mut x2 = 0; let mut y2 = 0;
            let mut dx = 0; let mut dy = 0;
            if lex!(line; PartialKey -> src, ws, int -> x1, ws, int -> y1, ws,
                    int -> x2, ws, int -> y2, ws, int -> dx, ws, int -> dy) {
                let src = warn_on_partial_key!(src);
                let slice = ImageSlice { sx: x1, sy: y1, dx: dx, dy: dy, w: x2 - x1, h: y2 - y1 };
                emit!(BmsBGA(dst, src, slice));
            }
        })

        if_prefix!("@BGA" |dst, line| { // #@BGAxx yy <int> <int> <int> <int> <int> <int>
            let mut src = PartialKey::dummy();
            let mut sx = 0; let mut sy = 0; let mut w = 0; let mut h = 0;
            let mut dx = 0; let mut dy = 0;
            if lex!(line; PartialKey -> src, ws, int -> sx, ws, int -> sy, ws,
                    int -> w, ws, int -> h, ws, int -> dx, ws, int -> dy) {
                let src = warn_on_partial_key!(src);
                let slice = ImageSlice { sx: sx, sy: sy, dx: dx, dy: dy, w: w, h: h };
                emit!(BmsBGA(dst, src, slice));
            }
        })

        if_prefix!("SWBGA" |key, line| {
            // #SWBGAxx <int>:<int>:yy:<int>:<int8>,<int8>,<int8>,<int8> ...
            let mut fr = 0;
            let mut time = 0;
            let mut linekey = PartialKey::dummy();
            let mut doloop = 0;
            let mut argb = (0,0,0,0);
            let mut pattern = "";
            if lex!(line; int -> fr, ws*, ':', ws*, int -> time, ws*, ':', ws*,
                          PartialKey -> linekey, ws*, ':', ws*, int -> doloop, ws*, ':', ws*,
                          ARGB -> argb, ws, str -> pattern, ws*, !) {
                let linekey = warn_on_partial_key!(linekey);
                if doloop == 0 || doloop == 1 {
                    emit!(BmsSwBGA(key, fr, time, linekey, doloop == 1,
                                   argb, pattern.into_opt_owned_str()));
                }
            }
        })

        if_prefix!("ARGB" |key, line| { // #ARGBxx <int8>,<int8>,<int8>,<int8>
            let mut argb = (0,0,0,0);
            if lex!(line; ARGB -> argb) {
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

        if_prefix!("STOP" |key, line| { // #STOPxx <int>
            let mut duration = 0;
            if lex!(line; int -> duration) {
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
        let mut chan = Key::dummy();
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

/// Iterates over the parsed BMS commands, including flow commands.
pub fn each_bms_command_with_flow<Listener:BmsMessageListener>(
                                f: &mut Reader, opts: &BmsParserOptions,
                                callback: &mut Listener, blk: |uint,BmsCommand| -> bool) -> bool {
    each_bms_command_with_flow_(f, opts, callback as &mut BmsMessageListener, blk)
}

/// Same as `each_bms_command` below.
/// Virtualized arguments are used instead of generic parameters for the smaller binary.
pub fn each_bms_command_<R:Rng>(f: &mut Reader, r: &mut R,
                                opts: &BmsParserOptions, callback: &mut BmsMessageListener,
                                blk: |uint,BmsCommand| -> bool) -> bool {
    // internal callback wrapper, both the caller and the callee have to use the callback
    let mut callback_ = |line, msg| callback.on_message(line, msg);
    let mut pp = Preprocessor::new(r, &mut callback_);

    let mut ret = true;
    each_bms_command_with_flow_(f, opts, callback, |lineno, cmd| {
        let mut out = ~[]; // XXX can we avoid one allocation per iteration?
        match cmd {
            BmsFlow(ref flowcmd) => { pp.feed_flow(Some(lineno), flowcmd, &mut out); }
            cmd => { pp.feed_other((lineno, cmd.into_send()), &mut out); }
        }
        for (lineno, cmd) in out.move_iter() {
            if !blk(lineno, cmd) { ret = false; break; }
        }
        ret
    });
    if ret {
        let mut out = ~[];
        pp.finish(&mut out);
        for (lineno, cmd) in out.move_iter() {
            if !blk(lineno, cmd) { ret = false; break; }
        }
    }
    ret
}

/// Iterates over the parsed BMS commands, with flow commands have been preprocessed.
pub fn each_bms_command<R:Rng,Listener:BmsMessageListener>(
                                f: &mut Reader, r: &mut R,
                                opts: &BmsParserOptions, callback: &mut Listener,
                                blk: |uint,BmsCommand| -> bool) -> bool {
    each_bms_command_(f, r, opts, callback as &mut BmsMessageListener, blk)
}

