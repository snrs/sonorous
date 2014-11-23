// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS parser.

use std::{str, iter, f64, fmt};
use std::str::MaybeOwned;
use std::rand::Rng;
use encoding::EncodingRef;

use util::lex::FromStrPrefix;
use format::obj::{BPM, Duration, ImageSlice};
use format::bms::types::{Key, PartialKey};
use format::bms::diag;
use format::bms::diag::BmsMessage;
pub use format::bms::preproc::BmsFlow;
use format::bms::preproc::Preprocessor;
use format::bms::encoding::{decode_stream, guess_decode_stream};

/// A tuple of four `u8` values. Mainly used for BMS #ARGB command and its family.
pub type ARGB = (u8,u8,u8,u8);

impl FromStrPrefix for ARGB {
    fn from_str_prefix<'a>(s: &'a str) -> Option<(ARGB, &'a str)> {
        let mut a: uint = 0;
        let mut r: uint = 0;
        let mut g: uint = 0;
        let mut b: uint = 0;
        let mut remainder: &str = "";
        if lex!(s; uint -> a, ws*, lit ',', ws*, uint -> r, ws*, lit ',', ws*,
                   uint -> g, ws*, lit ',', ws*, uint -> b, str* -> remainder, !) &&
           a < 256 && r < 256 && g < 256 && b < 256 {
            Some(((a as u8, r as u8, g as u8, b as u8), remainder))
        } else {
            None
        }
    }
}

/// A wrapper type for a measure number. Only used for parsing.
struct Measure(uint);

impl FromStrPrefix for Measure {
    fn from_str_prefix<'a>(s: &'a str) -> Option<(Measure, &'a str)> {
        let isdigit = |c| '0' <= c && c <= '9';
        if s.len() >= 3 && isdigit(s.char_at(0)) && isdigit(s.char_at(1)) && isdigit(s.char_at(2)) {
            let measure = from_str::<uint>(s[..3]).unwrap();
            Some((Measure(measure), s[3..]))
        } else {
            None
        }
    }
}

/// Represents one line of BMS file.
#[deriving(PartialEq,Clone)]
pub enum BmsCommand<'r> {
    Unknown(MaybeOwned<'r>),                // starting with `#` but unknown otherwise
    TITLE(MaybeOwned<'r>),                  // #TITLE
    SUBTITLE(MaybeOwned<'r>),               // #SUBTITLE
    GENRE(MaybeOwned<'r>),                  // #GENRE
    ARTIST(MaybeOwned<'r>),                 // #ARTIST
    SUBARTIST(MaybeOwned<'r>),              // #SUBARTIST
    MAKER(MaybeOwned<'r>),                  // #MAKER
    COMMENT(MaybeOwned<'r>),                // #COMMENT
    STAGEFILE(MaybeOwned<'r>),              // #STAGEFILE
    BANNER(MaybeOwned<'r>),                 // #BANNER
    PATHWAV(MaybeOwned<'r>),                // #PATH_WAV
    BPM(BPM),                               // #BPM (without a following alphanumeric key)
    EXBPM(Key, BPM),                        // #EXBPM or #BPMxx
    PLAYER(int),                            // #PLAYER
    // Rust: unfortunately `Vec<(Key, MaybeOwned<'r>)>` segfaults. (#14088)
    LANES(Vec<(Key, String)>),              // #SNRS:LANES (experimental)
    PLAYLEVEL(int),                         // #PLAYLEVEL
    DIFFICULTY(int),                        // #DIFFICULTY
    RANK(int),                              // #RANK
    DEFEXRANK(int),                         // #DEFEXRANK
    EXRANK(Key, int),                       // #EXRANK
    TOTAL(int),                             // #TOTAL
    LNTYPE(int),                            // #LNTYPE
    LNOBJ(Key),                             // #LNOBJ
    WAV(Key, MaybeOwned<'r>),               // #WAV
    WAVCMD(int, Key, int),                  // #WAVCMD
    EXWAV(Key, Option<int>, Option<int>, Option<int>, MaybeOwned<'r>), // #EXWAV
    VOLWAV(int),                            // #VOLWAV
    MIDIFILE(MaybeOwned<'r>),               // #MIDIFILE
    BMP(Key, MaybeOwned<'r>),               // #BMP
    EXBMP(Key, ARGB, MaybeOwned<'r>),       // #EXBMP
    BACKBMP(MaybeOwned<'r>),                // #BACKBMP
    BGA(Key, Key, ImageSlice),              // #BGA or #@BGA
    POORBGA(int),                           // #POORBGA
    SWBGA(Key, int, int, Key, bool, ARGB, MaybeOwned<'r>), // #SWBGA
    ARGB(Key, ARGB),                        // #ARGB
    CHARFILE(MaybeOwned<'r>),               // #CHARFILE
    VIDEOFILE(MaybeOwned<'r>),              // #VIDEOFILE
    MOVIE(MaybeOwned<'r>),                  // #MOVIE
    CANVASSIZE(int, int),                   // #SNRS:CANVASSIZE (experimental)
    STOP(Key, Duration),                    // #STOP
    STP(f64, Duration),                     // #STP
    TEXT(Key, MaybeOwned<'r>),              // #TEXT or #SONG
    OPTION(MaybeOwned<'r>),                 // #OPTION
    CHANGEOPTION(Key, MaybeOwned<'r>),      // #CHANGEOPTION
    Shorten(uint, f64),                     // #xxx02
    Data(uint, Key, MaybeOwned<'r>),        // #xxxyy:...
    Flow(BmsFlow),                          // flow commands (#RANDOM, #IF, #ENDIF etc.)
}

impl<'r> BmsCommand<'r> {
    /// Converts the command that refers to the borrowed slice into the command that doesn't.
    pub fn into_send(self) -> BmsCommand<'static> {
        fn into_send_str<'r>(s: MaybeOwned<'r>) -> MaybeOwned<'static> {
            s.into_string().into_maybe_owned()
        }
        match self {
            BmsCommand::Unknown(s) => BmsCommand::Unknown(into_send_str(s)),
            BmsCommand::TITLE(s) => BmsCommand::TITLE(into_send_str(s)),
            BmsCommand::SUBTITLE(s) => BmsCommand::SUBTITLE(into_send_str(s)),
            BmsCommand::GENRE(s) => BmsCommand::GENRE(into_send_str(s)),
            BmsCommand::ARTIST(s) => BmsCommand::ARTIST(into_send_str(s)),
            BmsCommand::SUBARTIST(s) => BmsCommand::SUBARTIST(into_send_str(s)),
            BmsCommand::MAKER(s) => BmsCommand::MAKER(into_send_str(s)),
            BmsCommand::COMMENT(s) => BmsCommand::COMMENT(into_send_str(s)),
            BmsCommand::STAGEFILE(s) => BmsCommand::STAGEFILE(into_send_str(s)),
            BmsCommand::BANNER(s) => BmsCommand::BANNER(into_send_str(s)),
            BmsCommand::PATHWAV(s) => BmsCommand::PATHWAV(into_send_str(s)),
            BmsCommand::BPM(bpm) => BmsCommand::BPM(bpm),
            BmsCommand::EXBPM(key, bpm) => BmsCommand::EXBPM(key, bpm),
            BmsCommand::PLAYER(v) => BmsCommand::PLAYER(v),
            BmsCommand::LANES(lanes) =>
                BmsCommand::LANES(lanes.into_iter().map(|(l,spec)| (l,spec.clone())).collect()),
            BmsCommand::PLAYLEVEL(v) => BmsCommand::PLAYLEVEL(v),
            BmsCommand::DIFFICULTY(v) => BmsCommand::DIFFICULTY(v),
            BmsCommand::RANK(v) => BmsCommand::RANK(v),
            BmsCommand::DEFEXRANK(v) => BmsCommand::DEFEXRANK(v),
            BmsCommand::EXRANK(key, v) => BmsCommand::EXRANK(key, v),
            BmsCommand::TOTAL(v) => BmsCommand::TOTAL(v),
            BmsCommand::LNTYPE(lntype) => BmsCommand::LNTYPE(lntype),
            BmsCommand::LNOBJ(key) => BmsCommand::LNOBJ(key),
            BmsCommand::WAV(key, s) => BmsCommand::WAV(key, into_send_str(s)),
            BmsCommand::WAVCMD(cmd, key, v) => BmsCommand::WAVCMD(cmd, key, v),
            BmsCommand::EXWAV(key, pan, vol, freq, s) =>
                BmsCommand::EXWAV(key, pan, vol, freq, into_send_str(s)),
            BmsCommand::VOLWAV(v) => BmsCommand::VOLWAV(v),
            BmsCommand::MIDIFILE(s) => BmsCommand::MIDIFILE(into_send_str(s)),
            BmsCommand::BMP(key, s) => BmsCommand::BMP(key, into_send_str(s)),
            BmsCommand::EXBMP(key, argb, s) => BmsCommand::EXBMP(key, argb, into_send_str(s)),
            BmsCommand::BACKBMP(s) => BmsCommand::BACKBMP(into_send_str(s)),
            BmsCommand::BGA(dst, src, slice) => BmsCommand::BGA(dst, src, slice),
            BmsCommand::POORBGA(poorbga) => BmsCommand::POORBGA(poorbga),
            BmsCommand::SWBGA(key, fr, time, line, doloop, argb, pattern) =>
                BmsCommand::SWBGA(key, fr, time, line, doloop, argb, into_send_str(pattern)),
            BmsCommand::ARGB(key, argb) => BmsCommand::ARGB(key, argb),
            BmsCommand::CHARFILE(s) => BmsCommand::CHARFILE(into_send_str(s)),
            BmsCommand::VIDEOFILE(s) => BmsCommand::VIDEOFILE(into_send_str(s)),
            BmsCommand::MOVIE(s) => BmsCommand::MOVIE(into_send_str(s)),
            BmsCommand::CANVASSIZE(w, h) => BmsCommand::CANVASSIZE(w, h),
            BmsCommand::STOP(key, dur) => BmsCommand::STOP(key, dur),
            BmsCommand::STP(pos, dur) => BmsCommand::STP(pos, dur),
            BmsCommand::TEXT(key, s) => BmsCommand::TEXT(key, into_send_str(s)),
            BmsCommand::OPTION(opt) => BmsCommand::OPTION(into_send_str(opt)),
            BmsCommand::CHANGEOPTION(key, opt) => BmsCommand::CHANGEOPTION(key, into_send_str(opt)),
            BmsCommand::Shorten(pos, shorten) => BmsCommand::Shorten(pos, shorten),
            BmsCommand::Data(pos, chan, data) => BmsCommand::Data(pos, chan, into_send_str(data)),
            BmsCommand::Flow(flow) => BmsCommand::Flow(flow),
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
            BmsCommand::Unknown(ref s) => write!(f, "#{}", *s),
            BmsCommand::TITLE(ref s) => write!(f, "#TITLE {}", *s),
            BmsCommand::SUBTITLE(ref s) => write!(f, "#SUBTITLE {}", *s),
            BmsCommand::GENRE(ref s) => write!(f, "#GENRE {}", *s),
            BmsCommand::ARTIST(ref s) => write!(f, "#ARTIST {}", *s),
            BmsCommand::SUBARTIST(ref s) => write!(f, "#SUBARTIST {}", *s),
            BmsCommand::MAKER(ref s) => write!(f, "#MAKER {}", *s),
            BmsCommand::COMMENT(ref s) => write!(f, "#COMMENT {}", *s),
            BmsCommand::STAGEFILE(ref s) => write!(f, "#STAGEFILE {}", *s),
            BmsCommand::BANNER(ref s) => write!(f, "#BANNER {}", *s),
            BmsCommand::PATHWAV(ref s) => write!(f, "#PATH_WAV {}", *s),
            BmsCommand::BPM(BPM(bpm)) => write!(f, "#BPM {}", bpm),
            BmsCommand::EXBPM(key, BPM(bpm)) => write!(f, "#BPM{} {}", key, bpm),
            BmsCommand::PLAYER(v) => write!(f, "#PLAYER {}", v),
            BmsCommand::LANES(ref lanes) => {
                try!(write!(f, "#SNRS:LANES"));
                for &(lane, ref spec) in lanes.iter() {
                    try!(write!(f, " {} {}", lane, *spec));
                }
                Ok(())
            },
            BmsCommand::PLAYLEVEL(v) => write!(f, "#PLAYLEVEL {}", v),
            BmsCommand::DIFFICULTY(v) => write!(f, "#DIFFICULTY {}", v),
            BmsCommand::RANK(v) => write!(f, "#RANK {}", v),
            BmsCommand::DEFEXRANK(v) => write!(f, "#DEFEXRANK {}", v),
            BmsCommand::EXRANK(key, v) => write!(f, "#EXRANK {} {}", key, v),
            BmsCommand::TOTAL(v) => write!(f, "#TOTAL {}", v),
            BmsCommand::LNTYPE(lntype) => write!(f, "#LNTYPE {}", lntype),
            BmsCommand::LNOBJ(key) => write!(f, "#LNOBJ {}", key),
            BmsCommand::WAV(key, ref s) => write!(f, "#WAV{} {}", key, *s),
            BmsCommand::WAVCMD(cmd, key, v) => write!(f, "#WAVCMD {:02} {}{}", cmd, key, v),
            BmsCommand::EXWAV(_key, None, None, None, ref _s) => panic!("unsupported"),
            BmsCommand::EXWAV(key, pan, vol, freq, ref s) => {
                let mut flags = String::new();
                let mut opts = String::new();
                if pan.is_some() {
                    flags.push('p');
                    opts.push_str(format!(" {}", pan.unwrap())[]);
                }
                if vol.is_some() {
                    flags.push('v');
                    opts.push_str(format!(" {}", vol.unwrap())[]);
                }
                if freq.is_some() {
                    flags.push('f');
                    opts.push_str(format!(" {}", freq.unwrap())[]);
                }
                write!(f, "#EXWAV{} {}{} {}", key, flags, opts, *s)
            },
            BmsCommand::VOLWAV(v) => write!(f, "#VOLWAV {}", v),
            BmsCommand::MIDIFILE(ref s) => write!(f, "#MIDIFILE {}", *s),
            BmsCommand::BMP(key, ref s) => write!(f, "#BMP{} {}", key, *s),
            BmsCommand::EXBMP(key, argb, ref s) =>
                write!(f, "#EXBMP{} {} {}", key, fmt_argb(argb), *s),
            BmsCommand::BACKBMP(ref s) => write!(f, "#BACKBMP {}", *s),
            BmsCommand::BGA(dst, src, ImageSlice { sx, sy, dx, dy, w, h }) =>
                write!(f, "#BGA{} {} {} {} {} {} {} {}",
                       dst, src, sx, sy, sx + w, sy + h, dx, dy),
            BmsCommand::POORBGA(poorbga) => write!(f, "#POORBGA {}", poorbga),
            BmsCommand::SWBGA(key, fr, time, line, doloop, argb, ref pattern) =>
                write!(f, "#SWBGA{} {}:{}:{}:{}:{} {}", key, fr, time, line,
                       if doloop {1u} else {0u}, fmt_argb(argb), *pattern),
            BmsCommand::ARGB(key, argb) => write!(f, "#ARGB{} {}", key, fmt_argb(argb)),
            BmsCommand::CHARFILE(ref s) => write!(f, "#CHARFILE {}", *s),
            BmsCommand::VIDEOFILE(ref s) => write!(f, "#VIDEOFILE {}", *s),
            BmsCommand::MOVIE(ref s) => write!(f, "#MOVIE {}", *s),
            BmsCommand::CANVASSIZE(w, h) => write!(f, "#SNRS:CANVASSIZE {} {}", w, h),
            BmsCommand::STOP(key, Duration::Measures(dur)) =>
                write!(f, "#STOP{} {}", key, (dur * 192.0) as int),
            BmsCommand::STOP(..) => panic!("unsupported"),
            BmsCommand::STP(pos, Duration::Seconds(dur)) =>
                write!(f, "#STP {:07.3} {}", pos, (dur * 1000.0) as int),
            BmsCommand::STP(..) => panic!("unsupported"),
            BmsCommand::TEXT(key, ref s) => write!(f, "#TEXT{} {}", key, *s),
            BmsCommand::OPTION(ref opt) => write!(f, "#OPTION {}", *opt),
            BmsCommand::CHANGEOPTION(key, ref opt) => write!(f, "#CHANGEOPTION{} {}", key, *opt),
            BmsCommand::Shorten(pos, shorten) => write!(f, "#{:03}02:{}", pos, shorten),
            BmsCommand::Data(pos, chan, ref data) => write!(f, "#{:03}{}:{}", pos, chan, *data),
            BmsCommand::Flow(ref flowcmd) => flowcmd.fmt(f),
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
        match *self { Parsed::Command(..) => true, _ => false }
    }

    /// Returns true if the parsed item is a message.
    pub fn is_message(&self) -> bool {
        match *self { Parsed::Message(..) => true, _ => false }
    }

    /// Returns true if the parsed item is an encoding notification.
    pub fn is_encoding(&self) -> bool {
        match *self { Parsed::Encoding(..) => true, _ => false }
    }

    /// Extracts a command in the parsed item if any.
    pub fn command<'a>(&'a self) -> Option<(Option<uint>, &'a BmsCommand<'r>)> {
        match *self { Parsed::Command(lineno, ref cmd) => Some((lineno, cmd)), _ => None }
    }

    /// Extracts a message in the parsed item if any.
    pub fn message<'a>(&'a self) -> Option<(Option<uint>, &'a BmsMessage)> {
        match *self { Parsed::Message(lineno, ref msg) => Some((lineno, msg)), _ => None }
    }

    /// Extracts an encoding notification in the parsed item if any.
    pub fn encoding<'a>(&'a self) -> Option<(&'static str, f64)> {
        match *self { Parsed::Encoding(name, confidence) => Some((name, confidence)), _ => None }
    }

    /// Converts the parsed item to the sendable form.
    pub fn into_send(self) -> Parsed<'static> {
        match self {
            Parsed::Command(lineno, cmd) => Parsed::Command(lineno, cmd.into_send()),
            Parsed::Message(lineno, msg) => Parsed::Message(lineno, msg),
            Parsed::Encoding(encname, confidence) => Parsed::Encoding(encname, confidence),
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
    c.is_whitespace() || c == '\u0085' || c == '\ufeff'
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
        ParsingIterator { iter: self.file[].split('\u000a'), lineno: 0, opts: self.opts,
                          queued: vec![Parsed::Encoding(encname, confidence)] }
    }
}

impl<'r> Iterator<Parsed<'r>> for ParsingIterator<'r> {
    fn next(&mut self) -> Option<Parsed<'r>> {
        use std::ascii::AsciiExt;

        // return the queued items first
        match self.queued.remove(0) {
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
                    self.queued.push(Parsed::Message(Some(self.lineno), $e));
                })
            )

            macro_rules! warn_on_partial_key(
                ($e:expr) => ({
                    let key: PartialKey = $e;
                    if key.is_partial() {
                        diag!(diag::BmsHasOneDigitAlphanumericKey);
                    }
                    key.into_key()
                })
            )

            // skip non-command lines
            let line = line.trim_left_chars(is_whitespace_or_similar);
            if line.is_empty() { continue; }
            let line = match line.slice_shift_char() {
                Some(('\uff03', line)) => { diag!(diag::BmsHasFullWidthSharp); line },
                Some(('#', line)) => line,
                _ => continue
            };

            let upperline = line.to_ascii_upper();

            // emits a `BmsCommand` and restarts the loop
            macro_rules! emit(
                ($e:expr) => ({
                    self.queued.push(Parsed::Command(Some(self.lineno), $e));
                    return self.queued.remove(0);
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
                    if upperline[].starts_with(tt_to_expr!($prefix)) {
                        emit!($constr);
                    }
                );
                ($prefix:tt -> $constr:expr; $diag:expr) => (
                    // avoids an unreachable code
                    if upperline[].starts_with(tt_to_expr!($prefix)) {
                        diag!($diag);
                        emit!($constr);
                    }
                );
                ($prefix:tt |$v:ident| $then:expr) => ({ // #<command>...
                    let prefix: &'static str = tt_to_expr!($prefix);
                    if upperline[].starts_with(prefix) {
                        let $v = line[prefix.len()..];
                        let _ = $v; // removes warning
                        $then;
                        // no more matching possible
                        emit!(BmsCommand::Unknown($v.into_maybe_owned()));
                    }
                });
                ($prefix:tt |$k:ident, $v:ident| $then:expr) => ({ // #<command>xx ...
                    let prefix: &'static str = tt_to_expr!($prefix);
                    if upperline[].starts_with(prefix) {
                        let line = line[prefix.len()..];
                        let mut key = PartialKey::dummy();
                        let mut text = "";
                        if lex!(line; PartialKey -> key, ws, str -> text, !) {
                            let $k = warn_on_partial_key!(key);
                            let $v = text;
                            let _ = $k; // removes warning
                            let _ = $v; // ditto
                            $then;
                        }
                        // no more matching possible
                        emit!(BmsCommand::Unknown(line.into_maybe_owned()));
                    }
                })
            )

            if_prefix!("TITLE"     string -> BmsCommand::TITLE)
            if_prefix!("SUBTITLE"  string -> BmsCommand::SUBTITLE)
            if_prefix!("GENRE"     string -> BmsCommand::GENRE)
            if self.opts.autofix_commands {
                if_prefix!("GENLE" string -> BmsCommand::GENRE; diag::BmsHasGENLE)
            }
            if_prefix!("ARTIST"    string -> BmsCommand::ARTIST)
            if_prefix!("SUBARTIST" string -> BmsCommand::SUBARTIST)
            if_prefix!("MAKER"     string -> BmsCommand::MAKER)
            if_prefix!("COMMENT"   string -> BmsCommand::COMMENT) // quotes are stripped by caller
            if_prefix!("STAGEFILE" string -> BmsCommand::STAGEFILE)
            if_prefix!("BANNER"    string -> BmsCommand::BANNER)
            if_prefix!("PATH_WAV"  string -> BmsCommand::PATHWAV)

            if_prefix!("BPM" |line| { // #BPM <float> or #BPMxx <float>
                let mut key = PartialKey::dummy();
                let mut bpm = 0.0;
                if lex!(line; PartialKey -> key, ws, f64 -> bpm) {
                    let key = warn_on_partial_key!(key);
                    emit!(BmsCommand::EXBPM(key, BPM(bpm)));
                } else if lex!(line; ws, f64 -> bpm) {
                    emit!(BmsCommand::BPM(BPM(bpm)));
                }
            })

            if_prefix!("EXBPM" |key, line| { // #EXBPMxx <float>
                let mut bpm = 0.0;
                if lex!(line; f64 -> bpm) {
                    diag!(diag::BmsHasEXBPM);
                    emit!(BmsCommand::EXBPM(key, BPM(bpm)));
                }
            })

            if_prefix!("PLAYER"     value -> BmsCommand::PLAYER)
            if_prefix!("PLAYLEVEL"  value -> BmsCommand::PLAYLEVEL)
            if_prefix!("DIFFICULTY" value -> BmsCommand::DIFFICULTY)
            if_prefix!("RANK"       value -> BmsCommand::RANK)
            if_prefix!("DEFEXRANK"  value -> BmsCommand::DEFEXRANK)
            if_prefix!("TOTAL"      value -> BmsCommand::TOTAL)
            if_prefix!("LNTYPE"     value -> BmsCommand::LNTYPE)

            if_prefix!("LNOBJ" |line| { // #LNOBJ <key>
                let mut key = PartialKey::dummy();
                if lex!(line; ws, PartialKey -> key) {
                    let key = warn_on_partial_key!(key);
                    emit!(BmsCommand::LNOBJ(key));
                }
            })

            if_prefix!("EXRANK" |key, line| { // #EXRANKxx <int>
                let mut value = 0;
                if lex!(line; int -> value) {
                    emit!(BmsCommand::EXRANK(key, value));
                }
            })

            if_prefix!("SNRS:LANES" |line| { // #SNRS:LANES <key> <spec> <key> <spec> ...
                let words: Vec<&str> = line.words().collect();
                if !words.is_empty() && words.len() % 2 == 0 {
                    let mut lanes = Vec::new();
                    let mut okay = true;
                    for i in iter::range_step(0, words.len(), 2) {
                        if words[i].len() != 2 { okay = false; break; }
                        match Key::from_str(words[i]) {
                            Some(key) => {
                                lanes.push((key, words[i+1].to_string()));
                            }
                            None => {
                                okay = false;
                                break;
                            }
                        }
                    }
                    if okay {
                        emit!(BmsCommand::LANES(lanes));
                    }
                }
            })

            if_prefix!("WAVCMD" |line| { // #WAVCMD <int> xx <int>
                let mut cmd = 0;
                let mut key = PartialKey::dummy();
                let mut value = 0;
                if lex!(line; ws, int -> cmd, ws, PartialKey -> key, ws, int -> value) {
                    let key = warn_on_partial_key!(key);
                    emit!(BmsCommand::WAVCMD(cmd, key, value));
                }
            })

            if_prefix!("WAV"  key string -> BmsCommand::WAV)
            if_prefix!("VOLWAV"    value -> BmsCommand::VOLWAV)
            if_prefix!("MIDIFILE" string -> BmsCommand::MIDIFILE)

            if_prefix!("EXWAV" |key, line| { // #EXWAVxx [pvf] <int>* <string>
                let mut line = line;
                match line.find(|c: char| c.is_whitespace()) {
                    Some(sep) => {
                        let flags = line[..sep];
                        let mut pan = None;
                        let mut vol = None;
                        let mut freq = None;

                        line = line[sep..];
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
                                emit!(BmsCommand::EXWAV(key, pan, vol, freq,
                                                        text.into_maybe_owned()));
                            }
                        }
                    }
                    None => {}
                }
            })

            if_prefix!("BMP" key string -> BmsCommand::BMP)
            if_prefix!("BACKBMP" string -> BmsCommand::BACKBMP)
            if_prefix!("POORBGA"  value -> BmsCommand::POORBGA)

            if_prefix!("EXBMP" |key, line| { // #EXBMPxx <int8>,<int8>,<int8>,<int8> <string>
                let mut argb = (0,0,0,0);
                let mut text = "";
                if lex!(line; ARGB -> argb, ws, str -> text, ws*, !) {
                    emit!(BmsCommand::EXBMP(key, argb, text.into_maybe_owned()));
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
                    emit!(BmsCommand::BGA(dst, src, slice));
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
                    emit!(BmsCommand::BGA(dst, src, slice));
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
                if lex!(line; int -> fr, ws*, lit ':', ws*, int -> time, ws*, lit ':', ws*,
                              PartialKey -> linekey, ws*, lit ':', ws*, int -> doloop, ws*,
                              lit ':', ws*, ARGB -> argb, ws, str -> pattern, ws*, !) {
                    let linekey = warn_on_partial_key!(linekey);
                    if doloop == 0 || doloop == 1 {
                        emit!(BmsCommand::SWBGA(key, fr, time, linekey, doloop == 1,
                                                argb, pattern.into_maybe_owned()));
                    }
                }
            })

            if_prefix!("ARGB" |key, line| { // #ARGBxx <int8>,<int8>,<int8>,<int8>
                let mut argb = (0,0,0,0);
                if lex!(line; ARGB -> argb) {
                    emit!(BmsCommand::ARGB(key, argb));
                }
            })

            if_prefix!("CHARFILE"  string -> BmsCommand::CHARFILE)
            if_prefix!("VIDEOFILE" string -> BmsCommand::VIDEOFILE)
            if_prefix!("MOVIE"     string -> BmsCommand::MOVIE)

            if_prefix!("SNRS:CANVASSIZE" |line| { // #SNRS:CANVASSIZE <int> <int>
                let mut width = 0;
                let mut height = 0;
                if lex!(line; ws, int -> width, ws, int -> height) {
                    emit!(BmsCommand::CANVASSIZE(width, height));
                }
            })

            if_prefix!("STOP" |key, line| { // #STOPxx <int>
                let mut duration = 0;
                if lex!(line; int -> duration) {
                    let duration = Duration::Measures(duration as f64 / 192.0);
                    emit!(BmsCommand::STOP(key, duration));
                }
            })

            if_prefix!("STP" |line| { // #STP<int>.<int> <int>
                let mut measure = Measure(0);
                let mut frac = 0;
                let mut duration = 0;
                if lex!(line; Measure -> measure, lit '.', uint -> frac, ws,
                              int -> duration) && duration > 0 {
                    let Measure(measure) = measure;
                    let pos = measure as f64 + frac as f64 * 0.001;
                    let duration = Duration::Seconds(duration as f64 * 0.001);
                    emit!(BmsCommand::STP(pos, duration));
                }
            })

            if_prefix!("TEXT"         key string -> BmsCommand::TEXT)
            if_prefix!("SONG"         key string -> BmsCommand::TEXT; diag::BmsHasSONG)
            if_prefix!("OPTION"           string -> BmsCommand::OPTION)
            if_prefix!("CHANGEOPTION" key string -> BmsCommand::CHANGEOPTION)

            macro_rules! makeBmsFlow(($e:expr) => (|val| BmsCommand::Flow($e(val))))

            // we parse SWITCH directives first since we need to parse #END as #ENDIF.
            if_prefix!("SWITCH"    value -> makeBmsFlow!(BmsFlow::SWITCH))
            if_prefix!("SETSWITCH" value -> makeBmsFlow!(BmsFlow::SETSWITCH))
            if_prefix!("ENDSW"           -> BmsCommand::Flow(BmsFlow::ENDSW))
            if_prefix!("CASE"      value -> makeBmsFlow!(BmsFlow::CASE))
            if_prefix!("SKIP"            -> BmsCommand::Flow(BmsFlow::SKIP))
            if_prefix!("DEF"             -> BmsCommand::Flow(BmsFlow::DEF))

            if_prefix!("RANDOM" (ws) value -> makeBmsFlow!(BmsFlow::RANDOM);
                                              diag::BmsHasRANDOMWithoutWhitespace)
            if self.opts.autofix_commands {
                if_prefix!("RONDAM"  value -> makeBmsFlow!(BmsFlow::RANDOM); diag::BmsHasRONDAM)
            }
            if_prefix!("SETRANDOM"   value -> makeBmsFlow!(BmsFlow::SETRANDOM))
            if_prefix!("ENDRANDOM"         -> BmsCommand::Flow(BmsFlow::ENDRANDOM))
            if self.opts.autofix_commands {
                if_prefix!("IFEND"         -> BmsCommand::Flow(BmsFlow::ENDIF); diag::BmsHasIFEND)
                if_prefix!("IF" (ws) value -> makeBmsFlow!(BmsFlow::IF);
                                              diag::BmsHasIFWithoutWhitespace)
            } else {
                if_prefix!("IF"      value -> makeBmsFlow!(BmsFlow::IF))
            }
            if_prefix!("ELSEIF"      value -> makeBmsFlow!(BmsFlow::ELSEIF))
            if_prefix!("ELSE"              -> BmsCommand::Flow(BmsFlow::ELSE))
            if_prefix!("ENDIF"             -> BmsCommand::Flow(BmsFlow::ENDIF))
            if self.opts.autofix_commands {
                if_prefix!("END"           -> BmsCommand::Flow(BmsFlow::ENDIF);
                                              diag::BmsHasENDNotFollowedByIF)
            }

            let mut measure = Measure(0);
            let mut chan = Key::dummy();
            let mut data = "";
            if lex!(line; Measure -> measure, Key -> chan, lit ':', ws*, str -> data, ws*, !) {
                let Measure(measure) = measure;
                if chan == Key(2) { // #xxx02:<float>
                    let mut shorten = 0.0;
                    if lex!(data; ws*, f64 -> shorten) {
                        emit!(BmsCommand::Shorten(measure, shorten));
                    }
                } else {
                    emit!(BmsCommand::Data(measure, chan, data.into_maybe_owned()));
                }
            }

            emit!(BmsCommand::Unknown(line.into_maybe_owned()));
        }
    }
}

/// The preprocessing parser, which feeds parsed items from the main parser into the preprocessor.
pub struct PreprocessingParser<'r, R:'r> {
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
pub struct PreprocessingParsingIterator<'r, R:'r> {
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
            match self.queued.remove(0) {
                Some(parsed) => {
                    return Some(parsed);
                }
                None => {
                    if self.done { return None; }
                }
            }

            match self.iter.next() {
                Some(Parsed::Command(lineno, cmd)) => {
                    let mut messages = Vec::new();
                    let mut out = Vec::new();
                    match cmd.into_send() {
                        BmsCommand::Flow(ref flowcmd) => {
                            self.pp.feed_flow(lineno, flowcmd, &mut messages, &mut out);
                        }
                        cmd => {
                            self.pp.feed_other((lineno, cmd), &mut messages, &mut out);
                        }
                    }

                    self.queued.extend(messages.into_iter()
                                               .map(|msg| Parsed::Message(lineno, msg)));
                    self.queued.extend(out.into_iter()
                                          .map(|(line, cmd)| Parsed::Command(line, cmd)));
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
                    self.queued.extend(messages.into_iter()
                                               .map(|msg| Parsed::Message(None, msg)));
                    self.queued.extend(out.into_iter()
                                          .map(|(line, cmd)| Parsed::Command(line, cmd)));
                }
            }
        }
    }
}

