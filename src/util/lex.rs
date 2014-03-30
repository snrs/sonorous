// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

/*!
 * A lexer barely powerful enough to parse BMS format. Comparable to C's `sscanf`.
 *
 * `lex!(e; fmt1, fmt2, ..., fmtN)` returns an expression that evaluates to true if and only if
 * all format specification is consumed. The format specification (analogous to `sscanf`'s
 * `%`-string) is as follows:
 *
 * - `ws`: Consumes one or more whitespace.
 * - `ws*`: Consumes zero or more whitespace.
 * - `int [-> e2]`: Consumes an integer and optionally saves it to `e2`. The integer syntax is
 *   slightly limited compared to `sscanf`.
 * - `f64 [-> e2]`: Consumes a real number and optionally saves it to `e2`. Again, the real number
 *   syntax is slightly limited; especially an exponent support is missing.
 * - `str [-> e2]`: Consumes a remaining input as a string and optionally saves it to `e2`.
 *   The string is at least one character long. Implies `!`. It can be followed by `ws*` which
 *   makes the string right-trimmed.
 * - `str* [-> e2]`: Same as above but the string can be empty.
 * - `char [-> e2]`: Consumes exactly one character and optionally saves it to `e2`. Resulting
 *   character can be whitespace.
 * - `!`: Ensures that the entire string has been consumed. Should be the last format
 *   specification.
 * - `"foo"` etc.: An ordinary expression is treated as a literal string or literal character.
 *
 * For the use in Sonorous, the following specifications have been added:
 *
 * - `Key [-> e2]`: Consumes a two-letter alphanumeric key and optionally saves it to `e2`.
 * - `PartialKey [-> e2]`: Consumes an one- or two-letter alphanumeric key and saves it to `e2`.
 *   Use `PartialKey::into_key` method in order to convert it to a proper alphanumeric key.
 * - `Measure [-> e2]`: Consumes exactly three digits and optionally saves it to `e2`.
 * - `ARGB [-> e2]`: Almost same as `uint [-> e2a], ',', uint [-> e2r], ',', uint [-> e2g], ',',
 *   uint [-> e2b]` but `e2` is a tuple of four `u8` values and overflows are considered an error.
 */
// Rust: - there is no `std::libc::sscanf` due to the varargs. maybe regex support will make
//         this obsolete in the future, but not now.
//       - it is desirable to have a matcher only accepts an integer literal or string literal,
//         not a generic expression.
//       - it would be more useful to generate bindings for parsed result. this is related to
//         many issues in general.

#![macro_escape]

macro_rules! lex(
    ($e:expr; ) => (true);
    ($e:expr; !) => ($e.is_empty());

    ($e:expr; int -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        // Rust: `std::num::from_str_bytes_common` does not recognize a number followed
        //        by garbage, so we need to parse it ourselves.
        _line.scan_int().map_or(false, |_endpos| {
            let _prefix = _line.slice_to(_endpos);
            from_str(_prefix).map_or(false, |_value| {
                $dst = _value;
                lex!(_line.slice_from(_endpos); $($tail)*)
            })
        })
    });
    ($e:expr; uint -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        _line.scan_uint().map_or(false, |_endpos| {
            let _prefix = _line.slice_to(_endpos);
            from_str(_prefix).map_or(false, |_value| {
                $dst = _value;
                lex!(_line.slice_from(_endpos); $($tail)*)
            })
        })
    });
    ($e:expr; f64 -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        _line.scan_f64().map_or(false, |_endpos| {
            let _prefix = _line.slice_to(_endpos);
            from_str(_prefix).map_or(false, |_value| {
                $dst = _value;
                lex!(_line.slice_from(_endpos); $($tail)*)
            })
        })
    });
    ($e:expr; str -> $dst:expr, ws*, $($tail:tt)*) => ({
        let _line: &str = $e;
        if !_line.is_empty() {
            $dst = _line.trim_right();
            lex!(""; $($tail)*) // optimization!
        } else {
            false
        }
    });
    ($e:expr; str -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        if !_line.is_empty() {
            $dst = _line.slice_from(0); // Rust: why we need to reborrow `_line` here?!
            lex!(""; $($tail)*) // optimization!
        } else {
            false
        }
    });
    ($e:expr; str* -> $dst:expr, ws*, $($tail:tt)*) => ({
        let _line: &str = $e;
        $dst = _line.trim_right();
        lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; str* -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        $dst = _line.slice_from(0); // Rust: why we need to reborrow `_line` here?!
        lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; char -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        if !_line.is_empty() {
            let _range = _line.char_range_at(0);
            $dst = _range.ch;
            lex!(_line.slice_from(_range.next); $($tail)*)
        } else {
            false
        }
    });
    // start Sonorous-specific
    ($e:expr; Key -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        ::format::bms::types::Key::from_str(_line).map_or(false, |_value| {
            $dst = _value;
            lex!(_line.slice_from(2u); $($tail)*)
        })
    });
    ($e:expr; PartialKey -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        ::format::bms::types::PartialKey::from_str(_line).map_or(false, |(_value, _line)| {
            $dst = _value;
            lex!(_line; $($tail)*)
        })
    });
    ($e:expr; Measure -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        let _isdigit = |c| { '0' <= c && c <= '9' };
        // Rust: this is plain annoying.
        if _line.len() >= 3 && _isdigit(_line.char_at(0)) && _isdigit(_line.char_at(1)) &&
                _isdigit(_line.char_at(2)) {
            $dst = from_str(_line.slice_to(3u)).unwrap();
            lex!(_line.slice_from(3u); $($tail)*)
        } else {
            false
        }
    });
    ($e:expr; ARGB -> $dst:expr, $($tail:tt)*) => ({
        let mut _a: uint = 0;
        let mut _r: uint = 0;
        let mut _g: uint = 0;
        let mut _b: uint = 0;
        let mut _remainder: &str = "";
        if lex!($e; uint -> _a, ws*, ',', ws*, uint -> _r, ws*, ',', ws*,
                    uint -> _g, ws*, ',', ws*, uint -> _b, str* -> _remainder, !) &&
           _a < 256 && _r < 256 && _g < 256 && _b < 256 {
            $dst = (_a as u8, _r as u8, _g as u8, _b as u8);
            lex!(_remainder; $($tail)*)
        } else {
            false
        }
    });
    // end Sonorous-specific

    ($e:expr; ws, $($tail:tt)*) => ({
        let _line: &str = $e;
        if !_line.is_empty() && _line.char_at(0).is_whitespace() {
            lex!(_line.trim_left(); $($tail)*)
        } else {
            false
        }
    });
    ($e:expr; ws*, $($tail:tt)*) => ({
        let _line: &str = $e;
        lex!(_line.trim_left(); $($tail)*)
    });
    ($e:expr; int, $($tail:tt)*) => ({
        let mut _dummy: int = 0;
        lex!($e; int -> _dummy, $($tail)*)
    });
    ($e:expr; uint, $($tail:tt)*) => ({
        let mut _dummy: uint = 0;
        lex!($e; uint -> _dummy, $($tail)*)
    });
    ($e:expr; f64, $($tail:tt)*) => ({
        let mut _dummy: f64 = 0.0;
        lex!($e; f64 -> _dummy, $($tail)*)
    });
    ($e:expr; str, $($tail:tt)*) => ({
        !$e.is_empty() && lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; str*, $($tail:tt)*) => ({
        lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; char, $($tail:tt)*) => ({
        let mut _dummy: char = '\x00';
        lex!($e; char -> _dummy, $($tail)*)
    });
    // start Sonorous-specific
    ($e:expr; Key, $($tail:tt)*) => ({
        let mut _dummy: Key = Key::dummy();
        lex!($e; Key -> _dummy, $($tail)*)
    });
    ($e:expr; PartialKey, $($tail:tt)*) => ({
        let mut _dummy: PartialKey = PartialKey::dummy();
        lex!($e; PartialKey -> _dummy, $($tail)*)
    });
    ($e:expr; Measure, $($tail:tt)*) => ({
        let mut _dummy: uint = 0;
        lex!($e; Measure -> _dummy, $($tail)*)
    });
    ($e:expr; ARGB, $($tail:tt)*) => ({
        let mut _dummy: (u8,u8,u8,u8) = (0,0,0,0);
        lex!($e; ARGB -> _dummy, $($tail)*)
    });
    // end Sonorous-specific
    ($e:expr; $lit:expr, $($tail:tt)*) => ({
        $lit.prefix_shifted($e).map_or(false, |_line| {
            lex!(_line; $($tail)*)
        })
    });

    ($e:expr; int -> $dst:expr) => (lex!($e; int -> $dst, ));
    ($e:expr; uint -> $dst:expr) => (lex!($e; uint -> $dst, ));
    ($e:expr; f64 -> $dst:expr) => (lex!($e; f64 -> $dst, ));
    ($e:expr; str -> $dst:expr) => (lex!($e; str -> $dst, ));
    ($e:expr; str -> $dst:expr, ws*) => (lex!($e; str -> $dst, ws*, ));
    ($e:expr; str* -> $dst:expr) => (lex!($e; str* -> $dst, ));
    ($e:expr; str* -> $dst:expr, ws*) => (lex!($e; str* -> $dst, ws*, ));
    ($e:expr; char -> $dst:expr) => (lex!($e; char -> $dst, ));
    // start Sonorous-specific
    ($e:expr; Key -> $dst:expr) => (lex!($e; Key -> $dst, ));
    ($e:expr; PartialKey -> $dst:expr) => (lex!($e; PartialKey -> $dst, ));
    ($e:expr; Measure -> $dst:expr) => (lex!($e; Measure -> $dst, ));
    ($e:expr; ARGB -> $dst:expr) => (lex!($e; ARGB -> $dst, ));
    // end Sonorous-specific

    ($e:expr; ws) => (lex!($e; ws, ));
    ($e:expr; ws*) => (lex!($e; ws*, ));
    ($e:expr; int) => (lex!($e; int, ));
    ($e:expr; uint) => (lex!($e; uint, ));
    ($e:expr; f64) => (lex!($e; f64, ));
    ($e:expr; str) => (lex!($e; str, ));
    ($e:expr; str*) => (lex!($e; str*, ));
    ($e:expr; char) => (lex!($e; char, ));
    // start Sonorous-specific
    ($e:expr; Key) => (lex!($e; Key, ));
    ($e:expr; PartialKey) => (lex!($e; PartialKey, ));
    ($e:expr; Measure) => (lex!($e; Measure, ));
    ($e:expr; ARGB) => (lex!($e; ARGB, ));
    // end Sonorous-specific
    ($e:expr; $lit:expr) => (lex!($e; $lit, ))
)
