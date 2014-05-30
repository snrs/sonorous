// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon. (But see below)
// See README.md for details.
//
// This code is placed in the public domain and the author disclaims any copyright.
// Alternatively, for jurisdictions where authors cannot disclaim their copyright,
// this source code is distributed under the terms of CC0 1.0 Universal license
// as published by Creative Commons <https://creativecommons.org/publicdomain/zero/1.0/>.
//
// This is a translation of the independent implementation of the MD5 digest algorithm due to
// Alexander Peslyak. The original C code is placed in the public domain (so is this Rust code),
// and its (non-)copyright notice is reproduced here:
//
// ----8<----
// This software was written by Alexander Peslyak in 2001.  No copyright is
// claimed, and the software is hereby placed in the public domain.
// In case this attempt to disclaim copyright and place the software in the
// public domain is deemed null and void, then the software is
// Copyright (c) 2001 Alexander Peslyak and it is hereby released to the
// general public under the following terms:
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted.
//
// There's ABSOLUTELY NO WARRANTY, express or implied.
//
// (This is a heavily cut-down "BSD license".)
//
// This differs from Colin Plumb's older public domain implementation in that
// no exactly 32-bit integer data type is required (any 32-bit or wider
// unsigned integer data type will do), there's no compile-time endianness
// configuration, and the function prototypes match OpenSSL's.  No code from
// Colin Plumb's implementation has been reused; this comment merely compares
// the properties of the two independent implementations.
//
// The primary goals of this implementation are portability and ease of use.
// It is meant to be fast, but not as fast as possible.  Some known
// optimizations are not included to reduce source code size and avoid
// compile-time configuration.
// ----8<----

//! The MD5 digest algorithm (RFC 1321).

use std::slice::bytes;
use std::slice::bytes::MutableByteVector;

/// A fast integer type which is at least 32 bits long.
#[allow(non_camel_case_types)] pub type u32plus = u32;

/// Internal MD5 state that processes the raw data block (64 bytes long).
struct MD5State {
    a: u32plus,
    b: u32plus,
    c: u32plus,
    d: u32plus,
    block: [u32plus, ..16],
}

impl MD5State {
    /// Creates a new internal MD5 state.
    fn new() -> MD5State {
        // XXX we don't want to initialize buffer and block at this stage
        MD5State { a: 0x67452301, b: 0xefcdab89, c: 0x98badcfe, d: 0x10325476, block: [0, ..16] }
    }

    /**
     * Processes one or more 64-byte data blocks, but does NOT update
     * the bit counters.  There are no alignment requirements.
     */
    fn body(&mut self, data: &[u8]) {
        /*
         * The basic MD5 functions.
         *
         * F and G are optimized compared to their RFC 1321 definitions for
         * architectures that lack an AND-NOT instruction, just like in Colin Plumb's
         * implementation.
         */
        macro_rules! mix(
            (F:  $x:expr, $y:expr, $z:expr) => ($z ^ ($x & ($y ^ $z)));
            (G:  $x:expr, $y:expr, $z:expr) => ($y ^ ($z & ($x ^ $y)));
            (H:  $x:expr, $y:expr, $z:expr) => (($x ^ $y) ^ $z);
            (H2: $x:expr, $y:expr, $z:expr) => ($x ^ ($y ^ $z));
            (I:  $x:expr, $y:expr, $z:expr) => ($y ^ ($x | !$z))
        )

        /*
         * The MD5 transformation for all four rounds.
         */
        macro_rules! step(($f:ident: $a:expr, $b:expr, $c:expr, $d:expr,
                           $x:expr, $t:expr, $s:expr) => ({
            $a += mix!($f: $b, $c, $d) + $x + $t;
            $a = ($a << $s) | (($a & 0xffffffff) >> (32 - $s));
            $a += $b;
        }))

        let mut ptr = data.as_ptr();
        let mut size = data.len();

        /*
         * SET reads 4 input bytes in little-endian byte order and stores them
         * in a properly aligned word in host byte order.
         *
         * The check for little-endian architectures that tolerate unaligned
         * memory accesses is just an optimization.  Nothing will break if it
         * doesn't work.
         *
         * Rust version: They are unchecked, so we start with an assertion.
         */
        assert!(size > 0 && size % 64 == 0);

        #[cfg(target_arch="x86", not(md5_force_aligned))]
        #[cfg(target_arch="x86_64", not(md5_force_aligned))]
        #[cfg(target_arch="vax", not(md5_force_aligned))]
        #[inline(always)]
        unsafe fn set(ptr: *u8, _block: &mut [u32plus], n: int) -> u32plus {
            use std::mem;
            *mem::transmute::<*u8,*u32>(ptr.offset(n * 4)) as u32plus
        }

        #[cfg(target_arch="x86", not(md5_force_aligned))]
        #[cfg(target_arch="x86_64", not(md5_force_aligned))]
        #[cfg(target_arch="vax", not(md5_force_aligned))]
        #[inline(always)]
        unsafe fn get(ptr: *u8, block: &mut [u32plus], n: int) -> u32plus {
            set(ptr, block, n)
        }

        #[cfg(not(target_arch="x86"), not(target_arch="x86_64"), not(target_arch="vax"))]
        #[cfg(md5_force_aligned)]
        #[inline(always)]
        unsafe fn set(ptr: *u8, block: &mut [u32plus], n: int) -> u32plus {
            let v = *ptr.offset(n * 4) as u32plus |
                    (*ptr.offset(n * 4 + 1) as u32plus << 8) |
                    (*ptr.offset(n * 4 + 2) as u32plus << 16) |
                    (*ptr.offset(n * 4 + 3) as u32plus << 24);
            block.unsafe_set(n as uint, v);
            v
        }

        #[cfg(not(target_arch="x86"), not(target_arch="x86_64"), not(target_arch="vax"))]
        #[cfg(md5_force_aligned)]
        #[inline(always)]
        unsafe fn get(_ptr: *u8, block: &mut [u32plus], n: int) -> u32plus {
            *block.unsafe_ref(n as uint)
        }

        macro_rules! set(($n:expr) => (unsafe {set(ptr, self.block, $n)}))
        macro_rules! get(($n:expr) => (unsafe {get(ptr, self.block, $n)}))

        let mut a = self.a;
        let mut b = self.b;
        let mut c = self.c;
        let mut d = self.d;
        let mut saved_a;
        let mut saved_b;
        let mut saved_c;
        let mut saved_d;

        loop {
            saved_a = a;
            saved_b = b;
            saved_c = c;
            saved_d = d;

            /* Round 1 */
            step!(F: a, b, c, d, set!(0), 0xd76aa478, 7)
            step!(F: d, a, b, c, set!(1), 0xe8c7b756, 12)
            step!(F: c, d, a, b, set!(2), 0x242070db, 17)
            step!(F: b, c, d, a, set!(3), 0xc1bdceee, 22)
            step!(F: a, b, c, d, set!(4), 0xf57c0faf, 7)
            step!(F: d, a, b, c, set!(5), 0x4787c62a, 12)
            step!(F: c, d, a, b, set!(6), 0xa8304613, 17)
            step!(F: b, c, d, a, set!(7), 0xfd469501, 22)
            step!(F: a, b, c, d, set!(8), 0x698098d8, 7)
            step!(F: d, a, b, c, set!(9), 0x8b44f7af, 12)
            step!(F: c, d, a, b, set!(10), 0xffff5bb1, 17)
            step!(F: b, c, d, a, set!(11), 0x895cd7be, 22)
            step!(F: a, b, c, d, set!(12), 0x6b901122, 7)
            step!(F: d, a, b, c, set!(13), 0xfd987193, 12)
            step!(F: c, d, a, b, set!(14), 0xa679438e, 17)
            step!(F: b, c, d, a, set!(15), 0x49b40821, 22)

            /* Round 2 */
            step!(G: a, b, c, d, get!(1), 0xf61e2562, 5)
            step!(G: d, a, b, c, get!(6), 0xc040b340, 9)
            step!(G: c, d, a, b, get!(11), 0x265e5a51, 14)
            step!(G: b, c, d, a, get!(0), 0xe9b6c7aa, 20)
            step!(G: a, b, c, d, get!(5), 0xd62f105d, 5)
            step!(G: d, a, b, c, get!(10), 0x02441453, 9)
            step!(G: c, d, a, b, get!(15), 0xd8a1e681, 14)
            step!(G: b, c, d, a, get!(4), 0xe7d3fbc8, 20)
            step!(G: a, b, c, d, get!(9), 0x21e1cde6, 5)
            step!(G: d, a, b, c, get!(14), 0xc33707d6, 9)
            step!(G: c, d, a, b, get!(3), 0xf4d50d87, 14)
            step!(G: b, c, d, a, get!(8), 0x455a14ed, 20)
            step!(G: a, b, c, d, get!(13), 0xa9e3e905, 5)
            step!(G: d, a, b, c, get!(2), 0xfcefa3f8, 9)
            step!(G: c, d, a, b, get!(7), 0x676f02d9, 14)
            step!(G: b, c, d, a, get!(12), 0x8d2a4c8a, 20)

            /* Round 3 */
            step!(H: a, b, c, d, get!(5), 0xfffa3942, 4)
            step!(H2: d, a, b, c, get!(8), 0x8771f681, 11)
            step!(H: c, d, a, b, get!(11), 0x6d9d6122, 16)
            step!(H2: b, c, d, a, get!(14), 0xfde5380c, 23)
            step!(H: a, b, c, d, get!(1), 0xa4beea44, 4)
            step!(H2: d, a, b, c, get!(4), 0x4bdecfa9, 11)
            step!(H: c, d, a, b, get!(7), 0xf6bb4b60, 16)
            step!(H2: b, c, d, a, get!(10), 0xbebfbc70, 23)
            step!(H: a, b, c, d, get!(13), 0x289b7ec6, 4)
            step!(H2: d, a, b, c, get!(0), 0xeaa127fa, 11)
            step!(H: c, d, a, b, get!(3), 0xd4ef3085, 16)
            step!(H2: b, c, d, a, get!(6), 0x04881d05, 23)
            step!(H: a, b, c, d, get!(9), 0xd9d4d039, 4)
            step!(H2: d, a, b, c, get!(12), 0xe6db99e5, 11)
            step!(H: c, d, a, b, get!(15), 0x1fa27cf8, 16)
            step!(H2: b, c, d, a, get!(2), 0xc4ac5665, 23)

            /* Round 4 */
            step!(I: a, b, c, d, get!(0), 0xf4292244, 6)
            step!(I: d, a, b, c, get!(7), 0x432aff97, 10)
            step!(I: c, d, a, b, get!(14), 0xab9423a7, 15)
            step!(I: b, c, d, a, get!(5), 0xfc93a039, 21)
            step!(I: a, b, c, d, get!(12), 0x655b59c3, 6)
            step!(I: d, a, b, c, get!(3), 0x8f0ccc92, 10)
            step!(I: c, d, a, b, get!(10), 0xffeff47d, 15)
            step!(I: b, c, d, a, get!(1), 0x85845dd1, 21)
            step!(I: a, b, c, d, get!(8), 0x6fa87e4f, 6)
            step!(I: d, a, b, c, get!(15), 0xfe2ce6e0, 10)
            step!(I: c, d, a, b, get!(6), 0xa3014314, 15)
            step!(I: b, c, d, a, get!(13), 0x4e0811a1, 21)
            step!(I: a, b, c, d, get!(4), 0xf7537e82, 6)
            step!(I: d, a, b, c, get!(11), 0xbd3af235, 10)
            step!(I: c, d, a, b, get!(2), 0x2ad7d2bb, 15)
            step!(I: b, c, d, a, get!(9), 0xeb86d391, 21)

            a += saved_a;
            b += saved_b;
            c += saved_c;
            d += saved_d;

            ptr = unsafe {ptr.offset(64)};
            size -= 64;
            if size == 0 { break; }
        }

        self.a = a;
        self.b = b;
        self.c = c;
        self.d = d;
    }
}

/// The full MD5 state. This state gets erased when dropped.
pub struct MD5 {
    lo: u32plus,
    hi: u32plus,
    state: MD5State,
    buffer: [u8, ..64],
}

impl MD5 {
    /// Creates a new MD5 state.
    pub fn new() -> MD5 {
        // XXX we don't want to initialize buffer and block at this stage
        MD5 { lo: 0, hi: 0, state: MD5State::new(), buffer: [0, ..64] }
    }

    /// Creates a new MD5 state and immediately updates it with given buffer.
    pub fn from_buffer(buf: &[u8]) -> MD5 {
        let mut md5 = MD5::new();
        md5.update(buf);
        md5
    }

    /// Feeds the state with a stream of bytes.
    pub fn update(&mut self, mut data: &[u8]) {
        let saved_lo = self.lo;
        self.lo = (saved_lo + data.len() as u32plus) & 0x1fffffff;
        if self.lo < saved_lo {
            self.hi += 1;
        }
        self.hi += (data.len() >> 29) as u32plus;

        let used = (saved_lo & 0x3f) as uint;
        if used > 0 {
            let available = 64 - used;

            if data.len() < available {
                bytes::copy_memory(self.buffer.mut_slice_from(used), data);
                return;
            }

            bytes::copy_memory(self.buffer.mut_slice_from(used), data.slice_to(available));
            data = data.slice_from(available);
            self.state.body(self.buffer);
        }

        if data.len() >= 64 {
            let size_ = data.len() & !0x3f;
            self.state.body(data.slice_to(size_));
            data = data.slice_from(size_);
        }

        bytes::copy_memory(self.buffer, data);
    }

    /// Returns a finished 16-byte digest.
    pub fn final(self) -> [u8, ..16] {
        let mut ctx = self;

        let mut used = (ctx.lo & 0x3f) as uint;
        ctx.buffer[used] = 0x80;
        used += 1;

        let available = 64 - used;
        if available < 8 {
            ctx.buffer.mut_slice_from(used).set_memory(0);
            ctx.state.body(ctx.buffer);
            used = 0;
        }

        ctx.buffer.mut_slice(used, 56).set_memory(0);

        ctx.lo <<= 3;
        ctx.buffer[56] = ctx.lo as u8;
        ctx.buffer[57] = (ctx.lo >> 8) as u8;
        ctx.buffer[58] = (ctx.lo >> 16) as u8;
        ctx.buffer[59] = (ctx.lo >> 24) as u8;
        ctx.buffer[60] = ctx.hi as u8;
        ctx.buffer[61] = (ctx.hi >> 8) as u8;
        ctx.buffer[62] = (ctx.hi >> 16) as u8;
        ctx.buffer[63] = (ctx.hi >> 24) as u8;

        ctx.state.body(ctx.buffer);

        [ctx.state.a as u8,
         (ctx.state.a >> 8) as u8,
         (ctx.state.a >> 16) as u8,
         (ctx.state.a >> 24) as u8,
         ctx.state.b as u8,
         (ctx.state.b >> 8) as u8,
         (ctx.state.b >> 16) as u8,
         (ctx.state.b >> 24) as u8,
         ctx.state.c as u8,
         (ctx.state.c >> 8) as u8,
         (ctx.state.c >> 16) as u8,
         (ctx.state.c >> 24) as u8,
         ctx.state.d as u8,
         (ctx.state.d >> 8) as u8,
         (ctx.state.d >> 16) as u8,
         (ctx.state.d >> 24) as u8]
    }
}

impl Drop for MD5 {
    fn drop(&mut self) {
        self.lo = 0;
        self.hi = 0;
        self.state.a = 0;
        self.state.b = 0;
        self.state.c = 0;
        self.state.d = 0;
        for v in self.state.block.mut_iter() { *v = 0; }
        self.buffer.set_memory(0);
    }
}

/// A trait for `to_hex` method.
pub trait ToHex {
    /// Converts itself to the hexadecimal representation, e.g. `48656c6c6f`.
    /// Mostly useful for hash digests.
    fn to_hex(&self) -> String;
}

impl<'r> ToHex for &'r [u8] {
    fn to_hex(&self) -> String {
        // XXX not quite fast.
        let mut ret = String::new();
        static HEXDIGITS: &'static [u8] = bytes!("0123456789abcdef");
        for &c in self.iter() {
            ret.push_char(HEXDIGITS[(c >> 4) as uint] as char);
            ret.push_char(HEXDIGITS[(c & 15) as uint] as char);
        }
        ret.into_string()
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::{MD5, ToHex};

    #[test]
    fn test_to_hex() {
        assert_eq!([].to_hex().as_slice(), "");
        assert_eq!([0x00].to_hex().as_slice(), "00");
        assert_eq!([0x0f].to_hex().as_slice(), "0f");
        assert_eq!([0xf0].to_hex().as_slice(), "f0");
        assert_eq!([0xff].to_hex().as_slice(), "ff");
        assert_eq!([0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef].to_hex().as_slice(),
                   "0123456789abcdef");
    }

    #[test]
    fn test_md5_suite() {
        fn md5(s: &str) -> String {
            let mut md5 = MD5::new();
            md5.update(s.as_bytes());
            md5.final().to_hex()
        }

        assert_eq!(md5("").as_slice(), "d41d8cd98f00b204e9800998ecf8427e");
        assert_eq!(md5("a").as_slice(), "0cc175b9c0f1b6a831c399e269772661");
        assert_eq!(md5("abc").as_slice(), "900150983cd24fb0d6963f7d28e17f72");
        assert_eq!(md5("message digest").as_slice(), "f96b697d7cb7938d525a2f31aaf161d0");
        assert_eq!(md5("abcdefghijklmnopqrstuvwxyz").as_slice(),
                   "c3fcd3d76192e4007dfb496cca67e13b");
        assert_eq!(md5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789").as_slice(),
                   "d174ab98d277d9f5a5611c2c9f419d9f");
        assert_eq!(md5("1234567890123456789012345678901234567890\
                        1234567890123456789012345678901234567890").as_slice(),
                   "57edf4a22be3c955ac49da2e2107b67a");
    }

    #[bench]
    fn bench_md5_1k_update(bencher: &mut test::Bencher) {
        let mut buf: Vec<u8> = Vec::new();
        for i in range(0u, 0x400) {
            buf.push(i as u8);
        }
        assert!(buf.len() == 0x400);

        let mut md5 = MD5::new();
        bencher.iter(|| {
            md5.update(buf.as_slice());
        });
    }

    #[bench]
    fn bench_md5_1k_update_then_final(bencher: &mut test::Bencher) {
        let mut buf: Vec<u8> = Vec::new();
        for i in range(0u, 0x400) {
            buf.push(i as u8);
        }
        assert!(buf.len() == 0x400);

        bencher.iter(|| {
            let mut md5 = MD5::new();
            md5.update(buf.as_slice());
            md5.final();
        });
    }
}
