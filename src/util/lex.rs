// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

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
 * - `float [-> e2]`: Consumes a real number and optionally saves it to `e2`. Again, the real number
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
 * - `Measure [-> e2]`: Consumes exactly three digits and optionally saves it to `e2`.
 */
// Rust: - there is no `std::libc::sscanf` due to the varargs. maybe regex support will make
//         this obsolete in the future, but not now.
//       - multiple statements do not expand correctly. (#4375)
//       - it is desirable to have a matcher only accepts an integer literal or string literal,
//         not a generic expression.
//       - no hygienic macro yet. possibly observable names from `$e` should be escaped for now.
//       - it would be more useful to generate bindings for parsed result. this is related to
//         many issues in general.

#[macro_escape];

macro_rules! lex(
    ($e:expr; ) => (true);
    ($e:expr; !) => ($e.is_empty());

    ($e:expr; int -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        // Rust: `std::num::from_str_bytes_common` does not recognize a number followed
        //        by garbage, so we need to parse it ourselves.
        do _line.scan_int().map_default(false) |&_endpos| {
            let _prefix = _line.slice(0, _endpos);
            do ::std::int::from_str(_prefix).map_default(false) |&_value| {
                $dst = _value;
                lex!(_line.slice_to_end(_endpos); $($tail)*)
            }
        }
    });
    ($e:expr; uint -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        do _line.scan_uint().map_default(false) |&_endpos| {
            let _prefix = _line.slice(0, _endpos);
            do ::std::uint::from_str(_prefix).map_default(false) |&_value| {
                $dst = _value;
                lex!(_line.slice_to_end(_endpos); $($tail)*)
            }
        }
    });
    ($e:expr; float -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        do _line.scan_float().map_default(false) |&_endpos| {
            let _prefix = _line.slice(0, _endpos);
            do ::std::float::from_str(_prefix).map_default(false) |&_value| {
                $dst = _value;
                lex!(_line.slice_to_end(_endpos); $($tail)*)
            }
        }
    });
    ($e:expr; str -> $dst:expr, ws*, $($tail:tt)*) => ({
        let _line: &str = $e;
        if !_line.is_empty() {
            // Rust: we should be able to avoid a copy here. (#5550)
            $dst = _line.trim_right().to_owned(); // XXX #5550
            lex!(""; $($tail)*) // optimization!
        } else {
            false
        }
    });
    ($e:expr; str -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        if !_line.is_empty() {
            $dst = _line.to_owned(); // XXX #5550
            lex!(""; $($tail)*) // optimization!
        } else {
            false
        }
    });
    ($e:expr; str* -> $dst:expr, ws*, $($tail:tt)*) => ({
        let _line: &str = $e;
        $dst = _line.trim_right().to_owned(); // XXX #5550
        lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; str* -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        $dst = _line.to_owned(); // XXX #5550
        lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; char -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        if !_line.is_empty() {
            let _range = _line.char_range_at(0);
            $dst = _range.ch;
            lex!(_line.slice_to_end(_range.next); $($tail)*)
        } else {
            false
        }
    });
    // start Sonorous-specific
    ($e:expr; Key -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        do ::format::bms::types::Key::from_str(_line).map_default(false) |&_value| {
            $dst = _value;
            lex!(_line.slice_to_end(2u); $($tail)*)
        }
    });
    ($e:expr; Measure -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        let _isdigit = |c| { '0' <= c && c <= '9' };
        // Rust: this is plain annoying.
        if _line.len() >= 3 && _isdigit(_line.char_at(0)) && _isdigit(_line.char_at(1)) &&
                _isdigit(_line.char_at(2)) {
            $dst = ::std::uint::from_str(_line.slice(0u, 3u)).unwrap();
            lex!(_line.slice_to_end(3u); $($tail)*)
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
    ($e:expr; float, $($tail:tt)*) => ({
        let mut _dummy: float = 0.0;
        lex!($e; float -> _dummy, $($tail)*)
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
        let mut _dummy: Key = Key(0);
        lex!($e; Key -> _dummy, $($tail)*)
    });
    ($e:expr; Measure, $($tail:tt)*) => ({
        let mut _dummy: uint = 0;
        lex!($e; Measure -> _dummy, $($tail)*)
    });
    // end Sonorous-specific
    ($e:expr; $lit:expr, $($tail:tt)*) => ({
        do $lit.prefix_shifted($e).map_default(false) |&_line| {
            lex!(_line; $($tail)*)
        }
    });

    ($e:expr; int -> $dst:expr) => (lex!($e; int -> $dst, ));
    ($e:expr; uint -> $dst:expr) => (lex!($e; uint -> $dst, ));
    ($e:expr; float -> $dst:expr) => (lex!($e; float -> $dst, ));
    ($e:expr; str -> $dst:expr) => (lex!($e; str -> $dst, ));
    ($e:expr; str -> $dst:expr, ws*) => (lex!($e; str -> $dst, ws*, ));
    ($e:expr; str* -> $dst:expr) => (lex!($e; str* -> $dst, ));
    ($e:expr; str* -> $dst:expr, ws*) => (lex!($e; str* -> $dst, ws*, ));
    ($e:expr; char -> $dst:expr) => (lex!($e; char -> $dst, ));
    // start Sonorous-specific
    ($e:expr; Key -> $dst:expr) => (lex!($e; Key -> $dst, ));
    ($e:expr; Measure -> $dst:expr) => (lex!($e; Measure -> $dst, ));
    // end Sonorous-specific

    ($e:expr; ws) => (lex!($e; ws, ));
    ($e:expr; ws*) => (lex!($e; ws*, ));
    ($e:expr; int) => (lex!($e; int, ));
    ($e:expr; uint) => (lex!($e; uint, ));
    ($e:expr; float) => (lex!($e; float, ));
    ($e:expr; str) => (lex!($e; str, ));
    ($e:expr; str*) => (lex!($e; str*, ));
    ($e:expr; char) => (lex!($e; char, ));
    // start Sonorous-specific
    ($e:expr; Key) => (lex!($e; Key, ));
    ($e:expr; Measure) => (lex!($e; Measure, ));
    // end Sonorous-specific
    ($e:expr; $lit:expr) => (lex!($e; $lit, ))
)
