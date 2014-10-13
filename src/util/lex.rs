// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Lexical analysis utilities.

#![macro_escape]

/// A version of `std::from_str::FromStr` which parses a prefix of the input and
/// returns a remaining portion of the input as well.
//
// Rust: `std::num::from_str_bytes_common` does not recognize a number followed
//        by garbage, so we need to parse it ourselves.
pub trait FromStrPrefix {
    /// Returns a parsed value and remaining string slice if possible.
    fn from_str_prefix<'a>(s: &'a str) -> Option<(Self, &'a str)>;
}

/// A convenience function that invokes `FromStrPrefix::from_str_prefix`.
pub fn from_str_prefix<'a, T:FromStrPrefix>(s: &'a str) -> Option<(T, &'a str)> {
    FromStrPrefix::from_str_prefix(s)
}

/// Implementations for `FromStrPrefix`. This avoids exporting internal macros.
mod from_str_prefix_impls {
    use super::FromStrPrefix;

    /// Returns a length of the longest prefix of given string,
    /// which `from_str::<uint>` would in general accept without a failure, if any.
    fn scan_uint(s: &str) -> Option<uint> {
        match s.find(|c| !('0' <= c && c <= '9')) {
            Some(first) if first > 0u => Some(first),
            None if s.len() > 0u => Some(s.len()),
            _ => None
        }
    }

    /// Returns a length of the longest prefix of given string,
    /// which `from_str::<int>` would in general accept without a failure, if any.
    fn scan_int(s: &str) -> Option<uint> {
        match s.slice_shift_char() {
            (Some('-'), s_) | (Some('+'), s_) => scan_uint(s_).map(|pos| pos + 1u),
            _ => scan_uint(s)
        }
    }

    /// Returns a length of the longest prefix of given string,
    /// which `from_str::<f64>` (and so on) would in general accept without a failure, if any.
    fn scan_float(s: &str) -> Option<uint> {
        scan_int(s).and_then(|pos| {
            // scan `.` followed by digits if any
            match s[pos..].slice_shift_char() {
                (Some('.'), s_) => scan_uint(s_).map(|pos2| pos + 1u + pos2),
                _ => Some(pos)
            }
        }).and_then(|pos| {
            // scan `e` or `E` followed by an optional sign and digits if any
            match s[pos..].slice_shift_char() {
                (Some('e'), s_) | (Some('E'), s_) => scan_int(s_).map(|pos2| pos + 1u + pos2),
                _ => Some(pos)
            }
        })
    }

    macro_rules! from_str_prefix_impls(
        ($($scan:ident then $t:ty;)*) => (
            $(
                impl FromStrPrefix for $t {
                    fn from_str_prefix<'a>(s: &'a str) -> Option<($t, &'a str)> {
                        $scan(s).and_then(|pos| {
                            from_str::<$t>(s[..pos]).map(|v| (v, s[pos..]))
                        })
                    }
                }
            )*
        )
    )

    from_str_prefix_impls! {
        scan_int   then int;
        scan_int   then i8;
        scan_int   then i16;
        scan_int   then i32;
        scan_int   then i64;
        scan_uint  then uint;
        scan_uint  then u8;
        scan_uint  then u16;
        scan_uint  then u32;
        scan_uint  then u64;
        scan_float then f32;
        scan_float then f64;
    }

    impl FromStrPrefix for char {
        fn from_str_prefix<'a>(s: &'a str) -> Option<(char, &'a str)> {
            match s.slice_shift_char() {
                (Some(c), s_) => Some((c, s_)),
                (None, _) => None,
            }
        }
    }
}

/// A trait which provides `prefix_shifted` method. Similar to `str::starts_with`, but with
/// swapped `self` and argument.
pub trait ShiftablePrefix {
    /// When given string starts with `self`, returns a slice of that string excluding that prefix.
    /// Otherwise returns `None`.
    fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str>;
}

/// A convenience function that invokes `ShiftablePrefix::prefix_shifted`.
pub fn prefix_shifted<'a, T:ShiftablePrefix>(s: &'a str, prefix: T) -> Option<&'a str> {
    prefix.prefix_shifted(s)
}

impl ShiftablePrefix for char {
    fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str> {
        match s.slice_shift_char() {
            (Some(c), s_) if c == *self => Some(s_),
            (_, _) => None,
        }
    }
}

impl<'r> ShiftablePrefix for &'r str {
    fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str> {
        if s.starts_with(*self) {
            Some(s[self.len()..])
        } else {
            None
        }
    }
}

/**
 * A lexer barely powerful enough to parse BMS format. Comparable to C's `sscanf`.
 *
 * `lex!(e; fmt1, fmt2, ..., fmtN)` returns an expression that evaluates to true if and only if
 * all format specification is consumed. The format specification (analogous to `sscanf`'s
 * `%`-string) is as follows:
 *
 * - `ws`: Consumes one or more whitespace.
 * - `ws*`: Consumes zero or more whitespace.
 * - `int [-> e2]` and so on: Any type implementing the `FromStrPrefix` trait can be used.
 *   Optionally saves the parsed value to `e2`. The default implementations includes all integers,
 *   floating point types and `char`.
 * - `str [-> e2]`: Consumes a remaining input as a string and optionally saves it to `e2`.
 *   The string is at least one character long. Implies `!`. It can be followed by `ws*` which
 *   makes the string right-trimmed.
 * - `str* [-> e2]`: Same as above but the string can be empty.
 * - `!`: Ensures that the entire string has been consumed. Should be the last format
 *   specification.
 * - `lit "foo"`, `lit '.'` etc.: A literal string or literal character.
 *
 * Whitespaces are only trimmed when `ws` or `ws*` specification is used.
 * Therefore `char`, for example, can read a whitespace when not prepended with `ws` or `ws*`.
 */
// Rust: - it is desirable to have a matcher only accepts an integer literal or string literal,
//         not a generic expression.
//       - it would be more useful to generate bindings for parsed result. this is related to
//         many issues in general.
//       - could we elide a `lit` prefix somehow?
macro_rules! lex(
    ($e:expr; ) => (true);
    ($e:expr; !) => ($e.is_empty());

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
            $dst = _line[];
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
        $dst = _line[];
        lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; $t:ty -> $dst:expr, $($tail:tt)*) => ({
        let _line: &str = $e;
        ::util::lex::from_str_prefix::<$t>(_line).map_or(false, |(_value, _line)| {
            $dst = _value;
            lex!(_line; $($tail)*)
        })
    });

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
    ($e:expr; str, $($tail:tt)*) => ({
        !$e.is_empty() && lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; str*, $($tail:tt)*) => ({
        lex!(""; $($tail)*) // optimization!
    });
    ($e:expr; $t:ty, $($tail:tt)*) => ({
        let mut _dummy: $t; // unused
        lex!($e; $t -> _dummy, $($tail)*)
    });
    ($e:expr; lit $lit:expr, $($tail:tt)*) => ({
        ::util::lex::prefix_shifted($e, $lit).map_or(false, |_line| {
            lex!(_line; $($tail)*)
        })
    });

    ($e:expr; str -> $dst:expr) => (lex!($e; str -> $dst, ));
    ($e:expr; str -> $dst:expr, ws*) => (lex!($e; str -> $dst, ws*, ));
    ($e:expr; str* -> $dst:expr) => (lex!($e; str* -> $dst, ));
    ($e:expr; str* -> $dst:expr, ws*) => (lex!($e; str* -> $dst, ws*, ));
    ($e:expr; $t:ty -> $dst:expr) => (lex!($e; $t -> $dst, ));

    ($e:expr; ws) => (lex!($e; ws, ));
    ($e:expr; ws*) => (lex!($e; ws*, ));
    ($e:expr; str) => (lex!($e; str, ));
    ($e:expr; str*) => (lex!($e; str*, ));
    ($e:expr; $t:ty) => (lex!($e; $t, ));
    ($e:expr; lit $lit:expr) => (lex!($e; lit $lit, ))
)
