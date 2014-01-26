// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Additions to the standard library.

/// String utilities for Rust. Parallels to `std::str`.
pub mod str {
    use std::str::*;

    /// Extensions to `str`.
    pub trait StrUtil<'r> {
        /// Returns a slice of the given string starting from `begin` and up to the byte
        /// position `end`. `end` doesn't have to point to valid characters.
        ///
        /// # Failure
        ///
        /// If `begin` does not point to valid characters or beyond the last character of
        /// the string, or `end` points beyond the last character of the string
        fn slice_upto(&self, begin: uint, end: uint) -> &'r str;

        /// Counts the number of bytes in the complete UTF-8 sequences up to `limit` bytes
        /// in `s` starting from `start`.
        fn count_bytes_upto(&self, start: uint, limit: uint) -> uint;

        /// Returns a length of the longest prefix of given string, which `from_str::<uint>`
        /// accepts without a failure, if any.
        //
        // Rust: actually, it is better to have `from_str::<{uint,int,f64}>` returning a tuple.
        fn scan_uint(&self) -> Option<uint>;

        /// Returns a length of the longest prefix of given string, which `from_str::<int>`
        /// accepts without a failure, if any.
        fn scan_int(&self) -> Option<uint>;

        /// Returns a length of the longest prefix of given string, which `from_str::<f64>`
        /// accepts without a failure, if any.
        fn scan_f64(&self) -> Option<uint>;

        /// Work with a null-terminated UTF-16 buffer of the string. Useful for calling
        /// Win32 API.
        fn as_utf16_c_str<T>(&self, f: |*u16| -> T) -> T;
    }

    impl<'r> StrUtil<'r> for &'r str {
        fn slice_upto(&self, begin: uint, end: uint) -> &'r str {
            self.slice(begin, begin + self.count_bytes_upto(begin, end))
        }

        fn count_bytes_upto(&self, start: uint, limit: uint) -> uint {
            assert!(self.is_char_boundary(start));
            let limit = start + limit;
            let l = self.len();
            assert!(limit < l);
            let mut end = start;
            loop {
                assert!(end < l);
                let next = self.char_range_at(end).next;
                if next > limit { break; }
                end = next;
            }
            end - start
        }

        fn scan_uint(&self) -> Option<uint> {
            match self.find(|c| !('0' <= c && c <= '9')) {
                Some(first) if first > 0u => Some(first),
                None if self.len() > 0u => Some(self.len()),
                _ => None
            }
        }

        fn scan_int(&self) -> Option<uint> {
            if self.starts_with("-") || self.starts_with("+") {
                self.slice_from(1u).scan_uint().map(|pos| pos + 1u)
            } else {
                self.scan_uint()
            }
        }

        fn scan_f64(&self) -> Option<uint> {
            self.scan_int().and_then(|pos| {
                if self.len() > pos && self.char_at(pos) == '.' {
                    let pos2 = self.slice_from(pos + 1u).scan_uint();
                    pos2.map(|pos2| pos + pos2 + 1u)
                } else {
                    Some(pos)
                }
            })
        }

        fn as_utf16_c_str<T>(&self, f: |*u16| -> T) -> T {
            let mut s16 = self.to_utf16();
            s16.push(0u16);
            f(s16.as_ptr())
        }
    }

    /// A trait which provides `prefix_shifted` method. Similar to `str::starts_with`, but with
    /// swapped `self` and argument.
    pub trait ShiftablePrefix {
        /// Returns a slice of given string with `self` at the start of the string stripped only
        /// once, if any.
        fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str>;
    }

    impl ShiftablePrefix for char {
        fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str> {
            if !s.is_empty() {
                let CharRange {ch, next} = s.char_range_at(0u);
                if ch == *self {
                    return Some(s.slice_from(next));
                }
            }
            None
        }
    }

    impl<'r> ShiftablePrefix for &'r str {
        fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str> {
            if s.starts_with(*self) {
                Some(s.slice_from(self.len()))
            } else {
                None
            }
        }
    }

}

