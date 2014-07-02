// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
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

        /// Work with a null-terminated UTF-16 buffer of the string. Useful for calling
        /// Win32 API.
        fn as_utf16_c_str<T>(&self, f: |*const u16| -> T) -> T;
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

        fn as_utf16_c_str<T>(&self, f: |*const u16| -> T) -> T {
            let mut s16: Vec<u16> = self.utf16_units().collect();
            s16.push(0u16);
            f(s16.as_ptr())
        }
    }
}

/// Option utilities for Rust. Parallels to `std::option`.
pub mod option {
    /// An utility trait for an option of string or alikes.
    pub trait StrOption {
        /// Returns a string slice in the option if any.
        fn as_ref_slice<'a>(&'a self) -> Option<&'a str>;

        /// Returns a string slice in the option if any, or `default` otherwise.
        fn as_ref_slice_or<'a>(&'a self, default: &'a str) -> &'a str {
            self.as_ref_slice().unwrap_or(default)
        }
    }

    impl<T:Str> StrOption for Option<T> {
        fn as_ref_slice<'a>(&'a self) -> Option<&'a str> {
            self.as_ref().map(|s| s.as_slice())
        }
    }
}

