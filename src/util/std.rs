// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Additions to the standard library. These are not subject to the above copyright notice.

/**
 * String utilities for Rust. Parallels to `std::str`.
 *
 * NOTE: Some of these additions will be eventually sent to `libstd/str.rs` and are not subject
 * to the above copyright notice.
 */
pub mod str {
    use std::str::*;

    static tag_cont_u8: u8 = 128u8; // copied from libstd/str.rs

    /// Given a potentially invalid UTF-8 byte sequence, fixes an invalid UTF-8 sequence with
    /// given error handler.
    pub fn fix_utf8(v: &[u8], handler: &fn(&[u8]) -> ~[u8]) -> ~[u8] {
        let mut i = 0u;
        let total = v.len();
        let mut result = ~[];
        while i < total {
            let chend = i + utf8_char_width(v[i]);
            let mut j = i + 1u;
            while j < total && j < chend && v[j] & 192u8 == tag_cont_u8 {
                j += 1u;
            }
            if j == chend {
                assert!(i != chend);
                result.push_all(v.slice(i, j));
            } else {
                result.push_all(handler(v.slice(i, j)));
            }
            i = j;
        }
        result
    }

    /// Converts a vector of bytes to a UTF-8 string. Any invalid UTF-8 sequences are fixed with
    /// given error handler.
    pub fn from_fixed_utf8_bytes(v: &[u8], handler: &fn(&[u8]) -> ~str) -> ~str {
        let newhandler: &fn(&[u8]) -> ~[u8] = |v: &[u8]| -> ~[u8] {
            let ret = handler(v);
            ret.as_bytes().to_owned()
        };
        let bytes = fix_utf8(v, newhandler);
        unsafe { raw::from_utf8(bytes) }
    }

    /// Extensions to `str`.
    pub trait StrUtil<'self> {
        /// Returns a slice of the given string starting from `begin` and up to the byte
        /// position `end`. `end` doesn't have to point to valid characters.
        ///
        /// # Failure
        ///
        /// If `begin` does not point to valid characters or beyond the last character of
        /// the string, or `end` points beyond the last character of the string
        fn slice_upto(&self, begin: uint, end: uint) -> &'self str;

        /// Given a potentially invalid UTF-8 string, fixes an invalid UTF-8 string with given
        /// error handler.
        fn fix_utf8(&self, handler: &fn(&[u8]) -> ~str) -> ~str;

        /// Counts the number of bytes in the complete UTF-8 sequences up to `limit` bytes
        /// in `s` starting from `start`.
        fn count_bytes_upto(&self, start: uint, limit: uint) -> uint;

        /// Returns a length of the longest prefix of given string, which `uint::from_str`
        /// accepts without a failure, if any.
        //
        // Rust: actually, it is better to have `{uint,int,f64}::from_str` returning a tuple.
        fn scan_uint(&self) -> Option<uint>;

        /// Returns a length of the longest prefix of given string, which `int::from_str`
        /// accepts without a failure, if any.
        fn scan_int(&self) -> Option<uint>;

        /// Returns a length of the longest prefix of given string, which `f64::from_str`
        /// accepts without a failure, if any.
        fn scan_f64(&self) -> Option<uint>;

        /// Converts all ASCII letters (A-Z/a-z, no accent) to uppercase.
        fn to_ascii_upper(&self) -> ~str;

        /// Converts all ASCII letters (A-Z/a-z, no accent) to lowercase.
        fn to_ascii_lower(&self) -> ~str;

        /// Work with a null-terminated UTF-16 buffer of the string. Useful for calling
        /// Win32 API.
        fn as_utf16_c_str<T>(&self, f: &fn(*u16) -> T) -> T;
    }

    impl<'self> StrUtil<'self> for &'self str {
        fn slice_upto(&self, begin: uint, end: uint) -> &'self str {
            self.slice(begin, begin + self.count_bytes_upto(begin, end))
        }

        fn fix_utf8(&self, handler: &fn(&[u8]) -> ~str) -> ~str {
            from_fixed_utf8_bytes(self.as_bytes(), handler)
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
                self.slice_from(1u).scan_uint().map(|&pos| pos + 1u)
            } else {
                self.scan_uint()
            }
        }

        fn scan_f64(&self) -> Option<uint> {
            do self.scan_int().and_then |pos| {
                if self.len() > pos && self.char_at(pos) == '.' {
                    let pos2 = self.slice_from(pos + 1u).scan_uint();
                    pos2.map(|&pos2| pos + pos2 + 1u)
                } else {
                    Some(pos)
                }
            }
        }

        fn to_ascii_upper(&self) -> ~str {
            unsafe { self.to_ascii_nocheck() }.to_upper().to_str_ascii()
        }

        fn to_ascii_lower(&self) -> ~str {
            unsafe { self.to_ascii_nocheck() }.to_lower().to_str_ascii()
        }

        fn as_utf16_c_str<T>(&self, f: &fn(*u16) -> T) -> T {
            let mut s16 = self.to_utf16();
            s16.push(0u16);
            do s16.as_imm_buf |buf, _| { f(buf) }
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

    impl<'self> ShiftablePrefix for &'self str {
        fn prefix_shifted<'r>(&self, s: &'r str) -> Option<&'r str> {
            if s.starts_with(*self) {
                Some(s.slice_from(self.len()))
            } else {
                None
            }
        }
    }

}

/**
 * Option utilities for Rust. Parallels to `std::option`.
 *
 * NOTE: Some of these additions will be eventually sent to `libstd/option.rs` and are not
 * subject to the above copyright notice.
 */
pub mod option {

    /// Filters the value inside the option using the function. Returns `None` if the original
    /// option didn't contain a value.
    #[inline(always)]
    pub fn filter<T:Clone>(opt: Option<T>, f: &fn(t: T) -> bool) -> Option<T> {
        match opt {
            Some(t) => if f(t.clone()) {Some(t)} else {None},
            None => None
        }
    }

    /// Merges two options. When one of options is `None` returns the other option. When both
    /// options contain a value the function is called to get the merged value.
    #[inline(always)]
    pub fn merge<T:Clone>(lhs: Option<T>, rhs: Option<T>, f: &fn(T, T) -> T) -> Option<T> {
        match (lhs, rhs) {
            (None, None) => None,
            (lhs,  None) => lhs,
            (None, rhs ) => rhs,
            (Some(ref lhs), Some(ref rhs)) => Some(f(lhs.clone(), rhs.clone()))
        }
    }

    pub trait CopyableOptionUtil<T:Clone> {
        /// Filters the value inside the option using the function. Returns `None` if
        /// the original option didn't contain a value.
        fn filter(self, f: &fn(x: T) -> bool) -> Option<T>;

        /// Merges two options. When one of options is `None` returns the other option. When
        /// both options contain a value the function is called to get the merged value.
        fn merge(self, other: Option<T>, f: &fn(T, T) -> T) -> Option<T>;
    }

    impl<T:Clone> CopyableOptionUtil<T> for Option<T> {
        #[inline(always)]
        fn filter(self, f: &fn(x: T) -> bool) -> Option<T> {
            filter(self, f)
        }

        #[inline(always)]
        fn merge(self, other: Option<T>, f: &fn(T, T) -> T) -> Option<T> {
            merge(self, other, f)
        }
    }
}

/**
 * I/O utilities for Rust. Parallels to `std::io`.
 *
 * NOTE: Some of these additions will be eventually sent to `libstd/io.rs` and are not subject
 * to the above copyright notice.
 */
pub mod io {

    /// Extensions to `ReaderUtil`.
    pub trait ReaderUtilEx {
        /// Reads up until the first '\n' char (which is not returned), or EOF. Any invalid
        /// UTF-8 sequences are fixed with given error handler.
        fn read_and_fix_utf8_line(&self, handler: &fn(&[u8]) -> ~str) -> ~str;

        /// Iterates over every line until the iterator breaks or EOF. Any invalid UTF-8
        /// sequences are fixed with given error handler.
        fn each_fixed_utf8_line(&self, handler: &fn(&[u8]) -> ~str, it: &fn(&str) -> bool);
    }

    impl<T: Reader> ReaderUtilEx for T {
        fn read_and_fix_utf8_line(&self, handler: &fn(&[u8]) -> ~str) -> ~str {
            let mut bytes = ~[];
            loop {
                let ch = self.read_byte();
                if ch == -1 || ch == 10 { break; }
                bytes.push(ch as u8);
            }
            ::util::std::str::from_fixed_utf8_bytes(bytes, handler)
        }

        fn each_fixed_utf8_line(&self, handler: &fn(&[u8]) -> ~str, it: &fn(&str) -> bool) {
            while !self.eof() {
                if !it(self.read_and_fix_utf8_line(|buf| handler(buf))) { break; } // XXX #7363
            }
        }
    }

}

