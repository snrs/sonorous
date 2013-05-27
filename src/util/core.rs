// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Additions to the standard library. These are not subject to the above copyright notice.

/**
 * String utilities for Rust. Parallels to `core::str`.
 *
 * NOTE: Some of these additions will be eventually sent to `libcore/str.rs` and are not subject
 * to the above copyright notice.
 */
pub mod str {
    use compat::core::iter;
    use core::str::*;

    static tag_cont_u8: u8 = 128u8; // copied from libcore/str.rs

    /// Iterates over the chars in a string, with byte indices.
    pub fn each_chari_byte(s: &str, it: &fn(uint, char) -> bool) -> iter::Ret {
        let mut pos = 0u;
        let len = s.len();
        while pos < len {
            let CharRange {ch, next} = char_range_at(s, pos);
            if !it(pos, ch) { return iter::EarlyExit; }
            pos = next;
        }
        iter::Finished
    }

    /// Given a potentially invalid UTF-8 byte sequence, fixes an invalid UTF-8 sequence with
    /// given error handler.
    pub fn fix_utf8(v: &[u8], handler: &fn(&[u8]) -> ~[u8]) -> ~[u8] {
        let mut i = 0u;
        let total = vec::len::<u8>(v);
        let mut result = ~[];
        while i < total {
            let chend = i + utf8_char_width(v[i]);
            let mut j = i + 1u;
            while j < total && j < chend && v[j] & 192u8 == tag_cont_u8 {
                j += 1u;
            }
            if j == chend {
                assert!(i != chend);
                result = vec::append(result, v.slice(i, j));
            } else {
                result = vec::append(result, handler(v.slice(i, j)));
            }
            i = j;
        }
        result
    }

    /// Given a potentially invalid UTF-8 string, fixes an invalid UTF-8 string with given error
    /// handler.
    pub fn fix_utf8_str(s: &str, handler: &fn(&[u8]) -> ~str) -> ~str {
        from_fixed_utf8_bytes(to_bytes(s), handler)
    }

    /// Converts a vector of bytes to a UTF-8 string. Any invalid UTF-8 sequences are fixed with
    /// given error handler.
    pub fn from_fixed_utf8_bytes(v: &[u8], handler: &fn(&[u8]) -> ~str) -> ~str {
        let newhandler: &fn(&[u8]) -> ~[u8] =
            |v: &[u8]| -> ~[u8] { to_bytes(handler(v)) };
        let bytes = fix_utf8(v, newhandler);
        unsafe { raw::from_bytes(bytes) }
    }

    /// Counts the number of bytes in the complete UTF-8 sequences up to `limit` bytes in `s`
    /// starting from `start`.
    pub fn count_bytes_upto<'b>(s: &'b str, start: uint, limit: uint) -> uint {
        assert!(is_char_boundary(s, start));
        let limit = start + limit;
        let l = len(s);
        assert!(limit < l);
        let mut end = start;
        loop {
            assert!(end < l);
            let next = char_range_at(s, end).next;
            if next > limit { break; }
            end = next;
        }
        end - start
    }

    /// Returns a length of the longest prefix of given string, which `uint::from_str` accepts
    /// without a failure, if any.
    //
    // Rust: actually, it is better to have `{uint,int,float}::from_str` returning an option.
    pub fn scan_uint(s: &str) -> Option<uint> {
        match find(s, |c| !('0' <= c && c <= '9')) {
            Some(first) if first > 0u => Some(first),
            None if s.len() > 0u => Some(s.len()),
            _ => None
        }
    }

    /// Returns a length of the longest prefix of given string, which `int::from_str` accepts
    /// without a failure, if any.
    pub fn scan_int(s: &str) -> Option<uint> {
        if s.starts_with("-") || s.starts_with("+") {
            scan_uint(s.slice_to_end(1u)).map(|&pos| pos + 1u)
        } else {
            scan_uint(s)
        }
    }

    /// Returns a length of the longest prefix of given string, which `float::from_str` accepts
    /// without a failure, if any.
    pub fn scan_float(s: &str) -> Option<uint> {
        do scan_int(s).chain |pos| {
            if s.len() > pos && s.char_at(pos) == '.' {
                let pos2 = scan_uint(s.slice_to_end(pos + 1u));
                pos2.map(|&pos2| pos + pos2 + 1u)
            } else {
                Some(pos)
            }
        }
    }

    /// Work with a null-terminated UTF-16 buffer of the string. Useful for calling Win32 API.
    pub fn as_utf16_c_str<T>(s: &str, f: &fn(*u16) -> T) -> T {
        let mut s16 = str::to_utf16(s);
        s16.push(0u16);
        do vec::as_imm_buf(s16) |buf, _| { f(buf) }
    }

    /// Region-dependent extensions to `str`. (Separated from `StrUtil` for the compatibility
    /// reason.)
    pub trait StrRegionUtil<'self> {
        /// Returns a slice of the given string starting from `begin`.
        ///
        /// # Failure
        ///
        /// If `begin` does not point to valid characters or beyond the last character of
        /// the string
        fn slice_to_end(&self, begin: uint) -> &'self str;

        /// Returns a slice of the given string starting from `begin` and up to the byte
        /// position `end`. `end` doesn't have to point to valid characters.
        ///
        /// # Failure
        ///
        /// If `begin` does not point to valid characters or beyond the last character of
        /// the string, or `end` points beyond the last character of the string
        fn slice_upto(&self, begin: uint, end: uint) -> &'self str;
    }

    // Rust: 0.6 and pre-0.7 differs from the implicitness/explicitness of trait region
    //       parameters, and the syntax is mutually exclusive.
    #[cfg(legacy)]
    impl<'self> StrRegionUtil for &'self str {
        fn slice_to_end(&self, begin: uint) -> &'self str {
            slice(*self, begin, len(*self))
        }
        fn slice_upto(&self, begin: uint, end: uint) -> &'self str {
            slice(*self, begin, begin + count_bytes_upto(*self, begin, end))
        }
    }

    #[cfg(not(legacy))]
    impl<'self> StrRegionUtil<'self> for &'self str {
        fn slice_to_end(&self, begin: uint) -> &'self str {
            slice(*self, begin, len(*self))
        }
        fn slice_upto(&self, begin: uint, end: uint) -> &'self str {
            slice(*self, begin, begin + count_bytes_upto(*self, begin, end))
        }
    }

    /// Remaining extensions to `str`.
    pub trait StrUtil {
        /// Iterates over the chars in a string, with byte indices.
        fn each_chari_byte(&self, it: &fn(uint, char) -> bool) -> iter::Ret;

        /// Given a potentially invalid UTF-8 string, fixes an invalid UTF-8 string with given
        /// error handler.
        fn fix_utf8(&self, handler: &fn(&[u8]) -> ~str) -> ~str;

        /// Counts the number of bytes in the complete UTF-8 sequences up to `limit` bytes
        /// in `s` starting from `start`.
        fn count_bytes_upto(&self, start: uint, limit: uint) -> uint;

        /// Returns a length of the longest prefix of given string, which `uint::from_str`
        /// accepts without a failure, if any.
        fn scan_uint(&self) -> Option<uint>;

        /// Returns a length of the longest prefix of given string, which `int::from_str`
        /// accepts without a failure, if any.
        fn scan_int(&self) -> Option<uint>;

        /// Returns a length of the longest prefix of given string, which `float::from_str`
        /// accepts without a failure, if any.
        fn scan_float(&self) -> Option<uint>;
    }

    impl<'self> StrUtil for &'self str {
        fn each_chari_byte(&self, it: &fn(uint, char) -> bool) -> iter::Ret {
            each_chari_byte(*self, it)
        }
        fn fix_utf8(&self, handler: &fn(&[u8]) -> ~str) -> ~str {
            fix_utf8_str(*self, handler)
        }
        fn count_bytes_upto(&self, start: uint, limit: uint) -> uint {
            count_bytes_upto(*self, start, limit)
        }
        fn scan_uint(&self) -> Option<uint> { scan_uint(*self) }
        fn scan_int(&self) -> Option<uint> { scan_int(*self) }
        fn scan_float(&self) -> Option<uint> { scan_float(*self) }
    }

    /// A trait which provides `prefix_shifted` method. Similar to `str::starts_with`, but with
    /// swapped `self` and argument.
    pub trait ShiftablePrefix {
        /// Returns a slice of given string with `self` at the start of the string stripped only
        /// once, if any.
        fn prefix_shifted(&self, s: &str) -> Option<~str>;
    }

    impl ShiftablePrefix for char {
        fn prefix_shifted(&self, s: &str) -> Option<~str> {
            if !s.is_empty() {
                let CharRange {ch, next} = char_range_at(s, 0u);
                if ch == *self {
                    return Some(s.slice_to_end(next).to_owned());
                }
            }
            None
        }
    }

    impl<'self> ShiftablePrefix for &'self str {
        fn prefix_shifted(&self, s: &str) -> Option<~str> {
            if s.starts_with(*self) {
                Some(s.slice_to_end(self.len()).to_owned())
            } else {
                None
            }
        }
    }

}

/**
 * Option utilities for Rust. Parallels to `core::option`.
 *
 * NOTE: Some of these additions will be eventually sent to `libcore/option.rs` and are not
 * subject to the above copyright notice.
 */
pub mod option {

    /// Filters the value inside the option using the function. Returns `None` if the original
    /// option didn't contain a value.
    #[inline(always)]
    pub fn filter<T:Copy>(opt: Option<T>, f: &fn(t: T) -> bool) -> Option<T> {
        match opt {
            Some(t) => if f(t) {Some(t)} else {None},
            None => None
        }
    }

    /// Merges two options. When one of options is `None` returns the other option. When both
    /// options contain a value the function is called to get the merged value.
    #[inline(always)]
    pub fn merge<T:Copy>(lhs: Option<T>, rhs: Option<T>, f: &fn(T, T) -> T) -> Option<T> {
        match (lhs, rhs) {
            (None, None) => None,
            (_,    None) => lhs,
            (None, _   ) => rhs,
            (Some(ref lhs), Some(ref rhs)) => Some(f(*lhs, *rhs))
        }
    }

    pub trait CopyableOptionUtil<T:Copy> {
        /// Filters the value inside the option using the function. Returns `None` if
        /// the original option didn't contain a value.
        fn filter(self, f: &fn(x: T) -> bool) -> Option<T>;

        /// Merges two options. When one of options is `None` returns the other option. When
        /// both options contain a value the function is called to get the merged value.
        fn merge(self, other: Option<T>, f: &fn(T, T) -> T) -> Option<T>;
    }

    impl<T:Copy> CopyableOptionUtil<T> for Option<T> {
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
 * Iterator utilities for Rust. Parallels to `core::iter`.
 *
 * NOTE: Some of these additions will be eventually sent to `libcore/iter.rs` and are not
 * subject to the above copyright notice.
 */
pub mod iter {
    use compat::core::iter;

    pub trait OptionalIter<A> {
        /// Like `each()`, but only iterates through the value inside options.
        fn each_some(&self, blk: &fn(v: &A) -> bool) -> iter::Ret;
    }

}

/**
 * Vector utilities for Rust. Parallels to `core::vec`.
 *
 * NOTE: Some of these additions will be eventually sent to `libcore/vec.rs` and are not subject
 * to the above copyright notice.
 */
pub mod vec {
    use compat::core::iter;
    use core::vec::*;

    /// Like `each()`, but only iterates through the value inside options.
    #[inline(always)]
    pub fn each_some<'r,A>(vec: &'r [Option<A>], blk: &fn(v: &'r A) -> bool) -> iter::Ret {
        for each(vec) |e| {
            for e.each |v| {
                if !blk(v) { return iter::EarlyExit; }
            }
        }
        iter::Finished
    }

    impl<'self,A> ::util::core::iter::OptionalIter<A> for &'self [Option<A>] {
        #[inline(always)]
        fn each_some(&self, blk: &fn(v: &A) -> bool) -> iter::Ret {
            each_some(*self, blk)
        }
    }

    impl<A> ::util::core::iter::OptionalIter<A> for ~[Option<A>] {
        #[inline(always)]
        fn each_some(&self, blk: &fn(v: &A) -> bool) -> iter::Ret {
            each_some(*self, blk)
        }
    }

    impl<A> ::util::core::iter::OptionalIter<A> for @[Option<A>] {
        #[inline(always)]
        fn each_some(&self, blk: &fn(v: &A) -> bool) -> iter::Ret {
            each_some(*self, blk)
        }
    }

}

/**
 * I/O utilities for Rust. Parallels to `core::io`.
 *
 * NOTE: Some of these additions will be eventually sent to `libcore/io.rs` and are not subject
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
            ::util::core::str::from_fixed_utf8_bytes(bytes, handler)
        }

        fn each_fixed_utf8_line(&self, handler: &fn(&[u8]) -> ~str, it: &fn(&str) -> bool) {
            while !self.eof() {
                if !it(self.read_and_fix_utf8_line(handler)) { break; }
            }
        }
    }

}

/**
 * Comparison routines for Rust. Parallels to `core::cmp`.
 *
 * NOTE: Some of these additions will be eventually sent to `libcore/cmp.rs` and are not subject
 * to the above copyright notice.
 */
pub mod cmp {

    /// Returns `v`, unless it is not between `low` and `high` in which cases returns whichever
    /// is the closest to `v`.
    #[inline(always)]
    pub fn clamp<T:Ord>(low: T, v: T, high: T) -> T {
        if v < low {low} else if v > high {high} else {v}
    }

}

/**
 * Hash table routines for Rust. Parallels to `core::hashmap`.
 *
 * NOTE: Some of these additions will be eventually sent to `libcore/hashmap.rs` and are not
 * subject to the above copyright notice.
 */
pub mod hashmap {
    use compat::core::hashmap::*;

    /// Constructs a new map from a vector of key-value pairs.
    //
    // TODO make this constructible from any suitable iterator
    pub fn map_from_vec<K:Eq+Hash+IterBytes,V>(items: &[(K,V)]) -> HashMap<K,V> {
        let mut map = HashMap::new();
        map.reserve_at_least(items.len());
        for items.each |&(k,v)| { map.insert(k, v); }
        map
    }

    /// Constructs a new set from a vector of key-value pairs.
    //
    // TODO make this constructible from any suitable iterator
    pub fn set_from_vec<V:Eq+Hash+IterBytes>(items: &[V]) -> HashSet<V> {
        let mut set = HashSet::new();
        set.reserve_at_least(items.len());
        for items.each |&v| { set.insert(v); }
        set
    }
}

