// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS parser.

use std::{str, iter, f64, char, fmt};
use std::str::MaybeOwned;
use std::rand::Rng;
use encoding::EncodingRef;

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
#[deriving(PartialEq,Clone)]
pub enum BmsCommand<'r> {
    BmsUnknown(MaybeOwned<'r>),                 // starting with `#` but unknown otherwise
    BmsTitle(MaybeOwned<'r>),                   // #TITLE
    BmsSubtitle(MaybeOwned<'r>),                // #SUBTITLE
    BmsGenre(MaybeOwned<'r>),                   // #GENRE
    BmsArtist(MaybeOwned<'r>),                  // #ARTIST
    BmsSubartist(MaybeOwned<'r>),               // #SUBARTIST
    BmsMaker(MaybeOwned<'r>),                   // #MAKER
    BmsComment(MaybeOwned<'r>),                 // #COMMENT
    BmsStageFile(MaybeOwned<'r>),               // #STAGEFILE
    BmsBanner(MaybeOwned<'r>),                  // #BANNER
    BmsPathWAV(MaybeOwned<'r>),                 // #PATH_WAV
    BmsBPM(BPM),                                // #BPM (without a following alphanumeric key)
    BmsExBPM(Key, BPM),                         // #EXBPM or #BPMxx
    BmsPlayer(int),                             // #PLAYER
    // Rust: unfortunately `Vec<(Key, MaybeOwned<'r>)>` segfaults. (#14088)
    BmsLanes(Vec<(Key, String)>),               // #SNRS:LANES (experimental)
    BmsPlayLevel(int),                          // #PLAYLEVEL
    BmsDifficulty(int),                         // #DIFFICULTY
    BmsRank(int),                               // #RANK
    BmsDefExRank(int),                          // #DEFEXRANK
    BmsExRank(Key, int),                        // #EXRANK
    BmsTotal(int),                              // #TOTAL
    BmsLNType(int),                             // #LNTYPE
    BmsLNObj(Key),                              // #LNOBJ
    BmsWAV(Key, MaybeOwned<'r>),                // #WAV
    BmsWAVCmd(int, Key, int),                   // #WAVCMD
    BmsExWAV(Key, Option<int>, Option<int>, Option<int>, MaybeOwned<'r>), // #EXWAV
    BmsVolWAV(int),                             // #VOLWAV
    BmsMIDIFile(MaybeOwned<'r>),                // #MIDIFILE
    BmsBMP(Key, MaybeOwned<'r>),                // #BMP
    BmsExBMP(Key, ARGB, MaybeOwned<'r>),        // #EXBMP
    BmsBackBMP(MaybeOwned<'r>),                 // #BACKBMP
    BmsBGA(Key, Key, ImageSlice),               // #BGA or #@BGA
    BmsPoorBGA(int),                            // #POORBGA
    BmsSwBGA(Key, int, int, Key, bool, ARGB, MaybeOwned<'r>), // #SWBGA
    BmsARGB(Key, ARGB),                         // #ARGB
    BmsCharFile(MaybeOwned<'r>),                // #CHARFILE
    BmsVideoFile(MaybeOwned<'r>),               // #VIDEOFILE
    BmsMovie(MaybeOwned<'r>),                   // #MOVIE
    BmsCanvasSize(int, int),                    // #SNRS:CANVASSIZE (experimental)
    BmsStop(Key, Duration),                     // #STOP
    BmsStp(f64, Duration),                      // #STP
    BmsText(Key, MaybeOwned<'r>),               // #TEXT or #SONG
    BmsOption(MaybeOwned<'r>),                  // #OPTION
    BmsChangeOption(Key, MaybeOwned<'r>),       // #CHANGEOPTION
    BmsShorten(uint, f64),                      // #xxx02
    BmsData(uint, Key, MaybeOwned<'r>),         // #xxxyy:...
    BmsFlow(BmsFlowCommand),                    // flow commands (#RANDOM, #IF, #ENDIF etc.)
}

impl<'r> BmsCommand<'r> {
    /// Converts the command that refers to the borrowed slice into the command that doesn't.
    pub fn into_send(self) -> BmsCommand<'static> {
        fn into_send_str<'r>(s: MaybeOwned<'r>) -> MaybeOwned<'static> {
            s.into_string().into_maybe_owned()
        }
        match self {
            BmsUnknown(s) => BmsUnknown(into_send_str(s)),
            BmsTitle(s) => BmsTitle(into_send_str(s)),
            BmsSubtitle(s) => BmsSubtitle(into_send_str(s)),
            BmsGenre(s) => BmsGenre(into_send_str(s)),
            BmsArtist(s) => BmsArtist(into_send_str(s)),
            BmsSubartist(s) => BmsSubartist(into_send_str(s)),
            BmsMaker(s) => BmsMaker(into_send_str(s)),
            BmsComment(s) => BmsComment(into_send_str(s)),
            BmsStageFile(s) => BmsStageFile(into_send_str(s)),
            BmsBanner(s) => BmsBanner(into_send_str(s)),
            BmsPathWAV(s) => BmsPathWAV(into_send_str(s)),
            BmsBPM(bpm) => BmsBPM(bpm),
            BmsExBPM(key, bpm) => BmsExBPM(key, bpm),
            BmsPlayer(v) => BmsPlayer(v),
            BmsLanes(lanes) =>
                BmsLanes(lanes.move_iter().map(|(lane,spec)| (lane,spec.clone())).collect()),
            BmsPlayLevel(v) => BmsPlayLevel(v),
            BmsDifficulty(v) => BmsDifficulty(v),
            BmsRank(v) => BmsRank(v),
            BmsDefExRank(v) => BmsDefExRank(v),
            BmsExRank(key, v) => BmsExRank(key, v),
            BmsTotal(v) => BmsTotal(v),
            BmsLNType(lntype) => BmsLNType(lntype),
            BmsLNObj(key) => BmsLNObj(key),
            BmsWAV(key, s) => BmsWAV(key, into_send_str(s)),
            BmsWAVCmd(cmd, key, v) => BmsWAVCmd(cmd, key, v),
            BmsExWAV(key, pan, vol, freq, s) => BmsExWAV(key, pan, vol, freq, into_send_str(s)),
            BmsVolWAV(v) => BmsVolWAV(v),
            BmsMIDIFile(s) => BmsMIDIFile(into_send_str(s)),
            BmsBMP(key, s) => BmsBMP(key, into_send_str(s)),
            BmsExBMP(key, argb, s) => BmsExBMP(key, argb, into_send_str(s)),
            BmsBackBMP(s) => BmsBackBMP(into_send_str(s)),
            BmsBGA(dst, src, slice) => BmsBGA(dst, src, slice),
            BmsPoorBGA(poorbga) => BmsPoorBGA(poorbga),
            BmsSwBGA(key, fr, time, line, doloop, argb, pattern) =>
                BmsSwBGA(key, fr, time, line, doloop, argb, into_send_str(pattern)),
            BmsARGB(key, argb) => BmsARGB(key, argb),
            BmsCharFile(s) => BmsCharFile(into_send_str(s)),
            BmsVideoFile(s) => BmsVideoFile(into_send_str(s)),
            BmsMovie(s) => BmsMovie(into_send_str(s)),
            BmsCanvasSize(w, h) => BmsCanvasSize(w, h),
            BmsStop(key, dur) => BmsStop(key, dur),
            BmsStp(pos, dur) => BmsStp(pos, dur),
            BmsText(key, s) => BmsText(key, into_send_str(s)),
            BmsOption(opt) => BmsOption(into_send_str(opt)),
            BmsChangeOption(key, opt) => BmsChangeOption(key, into_send_str(opt)),
            BmsShorten(measure, shorten) => BmsShorten(measure, shorten),
            BmsData(measure, chan, data) => BmsData(measure, chan, into_send_str(data)),
            BmsFlow(flow) => BmsFlow(flow),
        }
    }
}

impl<'r> fmt::Show for BmsCommand<'r> {
    /// Returns a reconstructed line for given BMS command.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #![allow(non_camel_case_types)]

        struct fmt_argb(ARGB);
        impl fmt::Show for fmt_argb {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let fmt_argb((a,r,g,b)) = *self;
                write!(f, "{},{},{},{}", a, r, g, b)
            }
        }

        match *self {
            BmsUnknown(ref s) => write!(f, "#{}", *s),
            BmsTitle(ref s) => write!(f, "#TITLE {}", *s),
            BmsSubtitle(ref s) => write!(f, "#SUBTITLE {}", *s),
            BmsGenre(ref s) => write!(f, "#GENRE {}", *s),
            BmsArtist(ref s) => write!(f, "#ARTIST {}", *s),
            BmsSubartist(ref s) => write!(f, "#SUBARTIST {}", *s),
            BmsMaker(ref s) => write!(f, "#MAKER {}", *s),
            BmsComment(ref s) => write!(f, "#COMMENT {}", *s),
            BmsStageFile(ref s) => write!(f, "#STAGEFILE {}", *s),
            BmsBanner(ref s) => write!(f, "#BANNER {}", *s),
            BmsPathWAV(ref s) => write!(f, "#PATH_WAV {}", *s),
            BmsBPM(BPM(bpm)) => write!(f, "#BPM {}", bpm),
            BmsExBPM(key, BPM(bpm)) => write!(f, "#BPM{} {}", key, bpm),
            BmsPlayer(v) => write!(f, "#PLAYER {}", v),
            BmsLanes(ref lanes) => {
                try!(write!(f, "#SNRS:LANES"));
                for &(lane, ref spec) in lanes.iter() {
                    try!(write!(f, " {} {}", lane, *spec));
                }
                Ok(())
            },
            BmsPlayLevel(v) => write!(f, "#PLAYLEVEL {}", v),
            BmsDifficulty(v) => write!(f, "#DIFFICULTY {}", v),
            BmsRank(v) => write!(f, "#RANK {}", v),
            BmsDefExRank(v) => write!(f, "#DEFEXRANK {}", v),
            BmsExRank(key, v) => write!(f, "#EXRANK {} {}", key, v),
            BmsTotal(v) => write!(f, "#TOTAL {}", v),
            BmsLNType(lntype) => write!(f, "#LNTYPE {}", lntype),
            BmsLNObj(key) => write!(f, "#LNOBJ {}", key),
            BmsWAV(key, ref s) => write!(f, "#WAV{} {}", key, *s),
            BmsWAVCmd(cmd, key, v) => write!(f, "#WAVCMD {:02} {}{}", cmd, key, v),
            BmsExWAV(_key, None, None, None, ref _s) => fail!("unsupported"),
            BmsExWAV(key, pan, vol, freq, ref s) => {
                let mut flags = String::new();
                let mut opts = String::new();
                if pan.is_some() {
                    flags.push_char('p');
                    opts.push_str(format!(" {}", pan.unwrap()).as_slice());
                }
                if vol.is_some() {
                    flags.push_char('v');
                    opts.push_str(format!(" {}", vol.unwrap()).as_slice());
                }
                if freq.is_some() {
                    flags.push_char('f');
                    opts.push_str(format!(" {}", freq.unwrap()).as_slice());
                }
                write!(f, "#EXWAV{} {}{} {}", key, flags, opts, *s)
            },
            BmsVolWAV(v) => write!(f, "#VOLWAV {}", v),
            BmsMIDIFile(ref s) => write!(f, "#MIDIFILE {}", *s),
            BmsBMP(key, ref s) => write!(f, "#BMP{} {}", key, *s),
            BmsExBMP(key, argb, ref s) => write!(f, "#EXBMP{} {} {}", key, fmt_argb(argb), *s),
            BmsBackBMP(ref s) => write!(f, "#BACKBMP {}", *s),
            BmsBGA(dst, src, ImageSlice { sx, sy, dx, dy, w, h }) =>
                write!(f, "#BGA{} {} {} {} {} {} {} {}",
                       dst, src, sx, sy, sx + w, sy + h, dx, dy),
            BmsPoorBGA(poorbga) => write!(f, "#POORBGA {}", poorbga),
            BmsSwBGA(key, fr, time, line, doloop, argb, ref pattern) =>
                write!(f, "#SWBGA{} {}:{}:{}:{}:{} {}", key, fr, time, line,
                       if doloop {1} else {0}, fmt_argb(argb), *pattern),
            BmsARGB(key, argb) => write!(f, "#ARGB{} {}", key, fmt_argb(argb)),
            BmsCharFile(ref s) => write!(f, "#CHARFILE {}", *s),
            BmsVideoFile(ref s) => write!(f, "#VIDEOFILE {}", *s),
            BmsMovie(ref s) => write!(f, "#MOVIE {}", *s),
            BmsCanvasSize(w, h) => write!(f, "#SNRS:CANVASSIZE {} {}", w, h),
            BmsStop(key, Measures(dur)) => write!(f, "#STOP{} {}", key, (dur * 192.0) as int),
            BmsStop(..) => fail!("unsupported"),
            BmsStp(pos, Seconds(dur)) => write!(f, "#STP {:07.3} {}", pos, (dur * 1000.0) as int),
            BmsStp(..) => fail!("unsupported"),
            BmsText(key, ref s) => write!(f, "#TEXT{} {}", key, *s),
            BmsOption(ref opt) => write!(f, "#OPTION {}", *opt),
            BmsChangeOption(key, ref opt) => write!(f, "#CHANGEOPTION{} {}", key, *opt),
            BmsShorten(measure, shorten) => write!(f, "#{:03}02:{}", measure, shorten),
            BmsData(measure, chan, ref data) => write!(f, "#{:03}{}:{}", measure, chan, *data),
            BmsFlow(ref flowcmd) => flowcmd.fmt(f),
        }
    }
}

/// A parsed item in the stream returned by parsing iterators.
#[deriving(PartialEq,Clone)]
pub enum Parsed<'r> {
    /// A borrowed BMS command in the specified line number (if any).
    Command(Option<uint>, BmsCommand<'r>),
    /// A parser message at the specified line number (if any).
    Message(Option<uint>, BmsMessage),
    /// A character encoding notification with the confidence in `[0,1]`.
    Encoding(&'static str, f64),
}

impl<'r> Parsed<'r> {
    /// Returns true if the parsed item is a command.
    pub fn is_command(&self) -> bool {
        match *self { Command(..) => true, _ => false }
    }

    /// Returns true if the parsed item is a message.
    pub fn is_message(&self) -> bool {
        match *self { Message(..) => true, _ => false }
    }

    /// Returns true if the parsed item is an encoding notification.
    pub fn is_encoding(&self) -> bool {
        match *self { Encoding(..) => true, _ => false }
    }

    /// Extracts a command in the parsed item if any.
    pub fn command<'a>(&'a self) -> Option<(Option<uint>, &'a BmsCommand<'r>)> {
        match *self { Command(lineno, ref cmd) => Some((lineno, cmd)), _ => None }
    }

    /// Extracts a message in the parsed item if any.
    pub fn message<'a>(&'a self) -> Option<(Option<uint>, &'a BmsMessage)> {
        match *self { Message(lineno, ref msg) => Some((lineno, msg)), _ => None }
    }

    /// Extracts an encoding notification in the parsed item if any.
    pub fn encoding<'a>(&'a self) -> Option<(&'static str, f64)> {
        match *self { Encoding(encname, confidence) => Some((encname, confidence)), _ => None }
    }

    /// Converts the parsed item to the sendable form.
    pub fn into_send(self) -> Parsed<'static> {
        match self {
            Command(lineno, cmd) => Command(lineno, cmd.into_send()),
            Message(lineno, msg) => Message(lineno, msg),
            Encoding(encname, confidence) => Encoding(encname, confidence),
        }
    }
}

/// Parser options for BMS format.
pub struct ParserOptions {
    /// Enables a parsing of several obviously mistyped commands. (Default: true)
    pub autofix_commands: bool,
    /// Disables an automatic encoding detection and forces the use of given encoding.
    pub force_encoding: Option<EncodingRef>,
}

impl ParserOptions {
    /// Returns default parser options.
    pub fn new() -> ParserOptions {
        ParserOptions { autofix_commands: true, force_encoding: None }
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

/// The main BMS parser.
pub struct Parser<'r> {
    /// Parser options.
    opts: &'r ParserOptions,
    /// The file contents decoded with a detected encoding.
    /// This is why we have a separate parsing iterator: we need to be able to refer its lifetime.
    file: String,
    /// The detected encoding and its confidence in `[0,1]`.
    encoding: (&'static str, f64),
}

/**
 * The parsing iterator.
 *
 * It is mainly built around the `std::str::CharSplits` iterator with a queue for pending items.
 * Each line is parsed into zero or more parser messages plus zero or one command;
 * as we are not allowed to return multiple items at once,
 * we keep excess items to be returned at the next invocations of `next`.
 */
pub struct ParsingIterator<'r> {
    /// The base iterator over each line in the file.
    iter: str::CharSplits<'r,char>,
    /// The current line number starting at 1.
    lineno: uint,
    /// Parser options.
    opts: &'r ParserOptions,
    /// Queued items to be returned at the next invocations of `next`.
    queued: Vec<Parsed<'r>>,
}

impl<'r> Parser<'r> {
    /// Creates a new parser that returns all BMS commands including flow commands.
    pub fn new(f: &'r mut Reader, opts: &'r ParserOptions) -> Parser<'r> {
        let (file, encoding, confidence) = match opts.force_encoding {
            Some(enc) => (decode_stream(f, enc), enc, f64::INFINITY),
            None => guess_decode_stream(f),
        };
        Parser { opts: opts, file: file, encoding: (encoding.name(), confidence) }
    }

    /// Returns a parsing iterator over this BMS file.
    pub fn iter<'a>(&'a self) -> ParsingIterator<'a> {
        let (encname, confidence) = self.encoding;
        ParsingIterator { iter: self.file.as_slice().split('\u000a'), lineno: 0, opts: self.opts,
                          queued: vec!(Encoding(encname, confidence)) }
    }
}

impl<'r> Iterator<Parsed<'r>> for ParsingIterator<'r> {
    fn next(&mut self) -> Option<Parsed<'r>> {
        use std::ascii::StrAsciiExt;
        use util::std::str::{StrUtil, ShiftablePrefix};

        // return the queued items first
        match self.queued.shift() {
            Some(parsed) => { return Some(parsed); }
            None => {}
        }

        loop {
            self.lineno += 1;
            let line = match self.iter.next() {
                Some(line) => line,
                None => { return None; }
            };

            macro_rules! diag(
                ($e:expr) => ({
                    self.queued.push(Message(Some(self.lineno), $e));
                })
            )

            macro_rules! warn_on_partial_key(
                ($e:expr) => ({
                    let key: PartialKey = $e;
                    if key.is_partial() {
                        diag!(BmsHasOneDigitAlphanumericKey);
                    }
                    key.into_key()
                })
            )

            // skip non-command lines
            let line = line.trim_left_chars(is_whitespace_or_similar);
            if line.is_empty() { continue; }
            let (ch, line) = line.slice_shift_char();
            if ch == Some('\uff03') {
                diag!(BmsHasFullWidthSharp);
            } else if ch != Some('#') {
                continue;
            }

            let upperline = line.to_ascii_upper();

            // emits a `BmsCommand` and restarts the loop
            macro_rules! emit(
                ($e:expr) => ({
                    self.queued.push(Command(Some(self.lineno), $e));
                    return self.queued.shift();
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
                            emit!($constr(text.into_maybe_owned()));
                        }
                    })
                );
                ($prefix:tt string -> $constr:expr; $diag:expr) => (
                    if_prefix!($prefix |line| { // #<command> <string>
                        let mut text = "";
                        if lex!(line; ws, str* -> text, ws*, !) {
                            diag!($diag);
                            emit!($constr(text.into_maybe_owned()));
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
                            diag!($diag);
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
                            diag!($diag);
                            emit!($constr(value));
                        }
                    })
                );
                ($prefix:tt key string -> $constr:expr) => (
                    if_prefix!($prefix |key, line| { // #<command>xx <string>
                        let mut text = "";
                        if lex!(line; str -> text, ws*, !) {
                            emit!($constr(key, text.into_maybe_owned()));
                        }
                    })
                );
                ($prefix:tt key string -> $constr:expr; $diag:expr) => (
                    if_prefix!($prefix |key, line| { // #<command>xx <string>
                        let mut text = "";
                        if lex!(line; str -> text, ws*, !) {
                            diag!($diag);
                            emit!($constr(key, text.into_maybe_owned()));
                        }
                    })
                );
                ($prefix:tt -> $constr:expr) => (
                    // avoids an unreachable code
                    if upperline.as_slice().starts_with(tt_to_expr!($prefix)) {
                        emit!($constr);
                    }
                );
                ($prefix:tt -> $constr:expr; $diag:expr) => (
                    // avoids an unreachable code
                    if upperline.as_slice().starts_with(tt_to_expr!($prefix)) {
                        diag!($diag);
                        emit!($constr);
                    }
                );
                ($prefix:tt |$v:ident| $then:expr) => ({ // #<command>...
                    let prefix: &'static str = tt_to_expr!($prefix);
                    if upperline.as_slice().starts_with(prefix) {
                        let $v = line.slice_from(prefix.len());
                        let _ = $v; // removes warning
                        $then;
                        emit!(BmsUnknown($v.into_maybe_owned())); // no more matching possible
                    }
                });
                ($prefix:tt |$k:ident, $v:ident| $then:expr) => ({ // #<command>xx ...
                    let prefix: &'static str = tt_to_expr!($prefix);
                    if upperline.as_slice().starts_with(prefix) {
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
                        emit!(BmsUnknown(line.into_maybe_owned())); // no more matching possible
                    }
                })
            )

            if_prefix!("TITLE"     string -> BmsTitle)
            if_prefix!("SUBTITLE"  string -> BmsSubtitle)
            if_prefix!("GENRE"     string -> BmsGenre)
            if self.opts.autofix_commands {
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
                    diag!(BmsHasEXBPM);
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
                let words: Vec<&str> = line.words().collect();
                if !words.is_empty() && words.len() % 2 == 0 {
                    let mut lanes = Vec::new();
                    let mut okay = true;
                    for i in iter::range_step(0, words.len(), 2) {
                        if words.as_slice()[i].len() != 2 { okay = false; break; }
                        match Key::from_str(words.as_slice()[i]) {
                            Some(key) => {
                                lanes.push((key, words.as_slice()[i+1].to_string()));
                            }
                            None => {
                                okay = false;
                                break;
                            }
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
                                emit!(BmsExWAV(key, pan, vol, freq, text.into_maybe_owned()));
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
                    emit!(BmsExBMP(key, argb, text.into_maybe_owned()));
                }
            })

            if_prefix!("BGA" |dst, line| { // #BGAxx yy <int> <int> <int> <int> <int> <int>
                let mut src = PartialKey::dummy();
                let mut x1 = 0; let mut y1 = 0; let mut x2 = 0; let mut y2 = 0;
                let mut dx = 0; let mut dy = 0;
                if lex!(line; PartialKey -> src, ws, int -> x1, ws, int -> y1, ws,
                        int -> x2, ws, int -> y2, ws, int -> dx, ws, int -> dy) {
                    let src = warn_on_partial_key!(src);
                    let slice = ImageSlice { sx: x1, sy: y1, dx: dx, dy: dy, w: x2-x1, h: y2-y1 };
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
                                       argb, pattern.into_maybe_owned()));
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

            if_prefix!("RANDOM" (ws) value -> makeBmsFlow!(BmsRandom);
                                              BmsHasRANDOMWithoutWhitespace)
            if self.opts.autofix_commands {
                if_prefix!("RONDAM"  value -> makeBmsFlow!(BmsRandom); BmsHasRONDAM)
            }
            if_prefix!("SETRANDOM"   value -> makeBmsFlow!(BmsSetRandom))
            if_prefix!("ENDRANDOM"         -> BmsFlow(BmsEndRandom))
            if self.opts.autofix_commands {
                if_prefix!("IFEND"         -> BmsFlow(BmsEndIf); BmsHasIFEND)
                if_prefix!("IF" (ws) value -> makeBmsFlow!(BmsIf); BmsHasIFWithoutWhitespace)
            } else {
                if_prefix!("IF"      value -> makeBmsFlow!(BmsIf))
            }
            if_prefix!("ELSEIF"      value -> makeBmsFlow!(BmsElseIf))
            if_prefix!("ELSE"              -> BmsFlow(BmsElse))
            if_prefix!("ENDIF"             -> BmsFlow(BmsEndIf))
            if self.opts.autofix_commands {
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
                    emit!(BmsData(measure, chan, data.into_maybe_owned()));
                }
            }

            emit!(BmsUnknown(line.into_maybe_owned()));
        }
    }
}

/// The preprocessing parser, which feeds parsed items from the main parser into the preprocessor.
pub struct PreprocessingParser<'r,R> {
    /// The internal parser.
    parser: Parser<'r>,
    /// The random number generator.
    r: &'r mut R,
}

/**
 * The parsing iterator with a preprocessor. (Yes, quite an unfortunate naming.)
 *
 * Just like `ParsingIterator`, it is built around the parsing iterator with a queue.
 * One difference is that the preprocessor may return additional commands at the end of file,
 * so we keep the `done` flag to avoid calling the parsing iterator when it already returned None.
 */
pub struct PreprocessingParsingIterator<'r,R> {
    /// The preprocessor that returns a pair of the line number (if any) and fully owned command.
    pp: Preprocessor<'r,(Option<uint>,BmsCommand<'static>),R>,
    /// The base parsing iterator.
    iter: ParsingIterator<'r>,
    /// Set to true when the parsing iterator once returned None.
    done: bool,
    /// Queued items to be returned at the next invocations of `next`.
    /// They take precedence over the parsing iterator's own queued items.
    //
    // Rust: this can't be `Vec<Parsed<'r>>` for some unknown reason.
    queued: Vec<Parsed<'static>>
}

impl<'r,R:Rng> PreprocessingParser<'r,R> {
    /// Iterates over the parsed BMS commands, with flow commands have been preprocessed.
    pub fn new(f: &'r mut Reader, r: &'r mut R,
               opts: &'r ParserOptions) -> PreprocessingParser<'r,R> {
        PreprocessingParser { parser: Parser::new(f, opts), r: r }
    }

    /// Returns a parsing iterator over this BMS file.
    pub fn iter<'a>(&'a mut self) -> PreprocessingParsingIterator<'a,R> {
        PreprocessingParsingIterator { pp: Preprocessor::new(self.r), iter: self.parser.iter(),
                                       done: false, queued: Vec::new() }
    }
}

impl<'r,R:Rng> Iterator<Parsed<'static>> for PreprocessingParsingIterator<'r,R> {
    fn next(&mut self) -> Option<Parsed<'static>> {
        loop {
            // return the queued items first
            match self.queued.shift() {
                Some(parsed) => {
                    return Some(parsed);
                }
                None => {
                    if self.done { return None; }
                }
            }

            match self.iter.next() {
                Some(Command(lineno, cmd)) => {
                    let mut messages = Vec::new();
                    let mut out = Vec::new();
                    match cmd.into_send() {
                        BmsFlow(ref flowcmd) => {
                            self.pp.feed_flow(lineno, flowcmd, &mut messages, &mut out);
                        }
                        cmd => {
                            self.pp.feed_other((lineno, cmd), &mut messages, &mut out);
                        }
                    }

                    self.queued.extend(messages.move_iter().map(|msg| Message(lineno, msg)));
                    self.queued.extend(out.move_iter().map(|(line, cmd)| Command(line, cmd)));
                    // now the next iteration will return a queued item if any
                }
                Some(parsed) => {
                    return Some(parsed.into_send());
                }
                None => {
                    self.done = true; // won't call `self.parser.next()` again

                    let mut messages = Vec::new();
                    let mut out = Vec::new();
                    self.pp.finish(&mut messages, &mut out);
                    self.queued.extend(messages.move_iter().map(|msg| Message(None, msg)));
                    self.queued.extend(out.move_iter().map(|(line, cmd)| Command(line, cmd)));
                }
            }
        }
    }
}

