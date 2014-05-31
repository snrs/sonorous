// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Common types for BMS format.

use std::{str, fmt};
use format::obj::Lane;

/// Two-letter alphanumeric identifier used for virtually everything, including resource
/// management, variable BPM and chart specification.
#[deriving(PartialEq,PartialOrd,Eq,Ord,Clone)]
pub struct Key(pub int);

/// The number of all possible alphanumeric keys.
pub static MAXKEY: int = 36*36;

impl Deref<int> for Key {
    fn deref<'a>(&'a self) -> &'a int {
        let Key(ref v) = *self;
        v
    }
}

/// All base-36 digits.
static BASE36_MAP: &'static [u8] = bytes!("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ");

/// Converts a single alphanumeric (base-36) letter to an integer.
fn getdigit(n: char) -> Option<int> {
    match n {
        '0'..'9' => Some((n as int) - ('0' as int)),
        'a'..'z' => Some((n as int) - ('a' as int) + 10),
        'A'..'Z' => Some((n as int) - ('A' as int) + 10),
        _ => None
    }
}

impl Key {
    /// Returns a definitely invalid alphanumeric key.
    pub fn dummy() -> Key {
        Key(-1)
    }

    /// Converts the first two letters of `s` to a `Key`.
    pub fn from_chars(s: &[char]) -> Option<Key> {
        if s.len() < 2 { return None; }
        getdigit(s[0]).and_then(|a| {
            getdigit(s[1]).map(|b| Key(a * 36 + b))
        })
    }

    /// Converts the first two letters of `s` to a `Key`.
    pub fn from_str(s: &str) -> Option<Key> {
        if s.len() < 2 { return None; }
        let str::CharRange {ch:c1, next:p1} = s.char_range_at(0);
        getdigit(c1).and_then(|a| {
            let str::CharRange {ch:c2, next:p2} = s.char_range_at(p1);
            getdigit(c2).map(|b| {
                assert!(p2 == 2); // both characters should be in ASCII
                Key(a * 36 + b)
            })
        })
    }

    /// Returns if the alphanumeric key is in the proper range. Sonorous supports the full
    /// range of 00-ZZ (0-1295) for every case.
    pub fn is_valid(&self) -> bool {
        0 <= **self && **self < MAXKEY
    }

    /// Re-reads the alphanumeric key as a hexadecimal number if possible. This is required
    /// due to handling of channel #03 (BPM is expected to be in hexadecimal).
    pub fn to_hex(&self) -> Option<int> {
        let sixteens = **self / 36;
        let ones = **self % 36;
        if sixteens < 16 && ones < 16 {Some(sixteens * 16 + ones)} else {None}
    }

    /// Converts the channel number to the lane number.
    pub fn to_lane(&self) -> Lane {
        let player = match **self / 36 {
            1 | 3 | 5 | 0xD => 0,
            2 | 4 | 6 | 0xE => 1,
            _ => fail!("non-object channel")
        };
        Lane(player * 36 + **self as uint % 36)
    }
}

impl fmt::Show for Key {
    /// Returns a two-letter representation of alphanumeric key.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        assert!(self.is_valid());
        let sixteens = **self / 36;
        let ones = **self % 36;
        write!(f, "{}{}", BASE36_MAP[sixteens as uint] as char,
                          BASE36_MAP[ones as uint] as char)
    }
}

/// Same as `Key` but accepts one-letter alphanumeric keys, encoded as a negative number
/// from -1 (`0`) to -36 (`Z`) which is invalid in a plain `Key`.
#[deriving(PartialEq,PartialOrd,Clone)]
pub struct PartialKey(pub int);

impl Deref<int> for PartialKey {
    fn deref<'a>(&'a self) -> &'a int {
        let PartialKey(ref v) = *self;
        v
    }
}

impl PartialKey {
    /// Returns a definitely invalid partial alphanumeric key.
    pub fn dummy() -> PartialKey {
        PartialKey(-37)
    }

    /// Converts the first one or two letters of `s` to a `PartialKey`.
    /// Also returns a remaining portion of `s`.
    pub fn from_chars<'r>(s: &'r [char]) -> Option<(PartialKey, &'r [char])> {
        if s.len() < 1 { return None; }
        getdigit(s[0]).map(|a| {
            if s.len() < 2 {
                (PartialKey(-a - 1), s.slice_from(1))
            } else {
                match getdigit(s[1]) {
                    Some(b) => (PartialKey(a * 36 + b), s.slice_from(2)),
                    None => (PartialKey(-a - 1), s.slice_from(1))
                }
            }
        })
    }

    /// Converts the first one or two letters of `s` to a `PartialKey`.
    /// Also returns a remaining portion of `s`.
    pub fn from_str<'r>(s: &'r str) -> Option<(PartialKey, &'r str)> {
        if s.len() < 1 { return None; }
        let str::CharRange {ch:c1, next:p1} = s.char_range_at(0);
        getdigit(c1).map(|a| {
            assert!(p1 == 1); // c1 should be in ASCII
            if s.len() < 2 { // do not advance
                (PartialKey(-a - 1), s.slice_from(p1))
            } else {
                let str::CharRange {ch:c2, next:p2} = s.char_range_at(p1);
                match getdigit(c2) {
                    Some(b) => {
                        assert!(p2 == 2); // both characters should be in ASCII
                        (PartialKey(a * 36 + b), s.slice_from(p2))
                    },
                    None => (PartialKey(-a - 1), s.slice_from(p1))
                }
            }
        })
    }

    /// Returns if the alphanumeric key is in the proper range.
    /// In addition to `Key`s 00-ZZ (0 to 1295), `PartialKey` supports 0-Z (-1 to -36).
    pub fn is_valid(&self) -> bool {
        -36 <= **self && **self < MAXKEY
    }

    /// Returns if the alphanumeric key is one-digit long.
    pub fn is_partial(&self) -> bool {
        **self < 0
    }

    /// Re-reads the alphanumeric key as a hexadecimal number if possible.
    pub fn to_hex(&self) -> Option<int> {
        let (sixteens, ones) = if **self < 0 {(0, -**self - 1)} else {(**self / 36, **self % 36)};
        if sixteens < 16 && ones < 16 {Some(sixteens * 16 + ones)} else {None}
    }

    /// Converts a partial alphanumeric key into a `Key`, assuming it's missing a leading `0`.
    pub fn into_key(self) -> Key {
        assert!(self.is_valid());
        if *self < 0 {Key(-*self - 1)} else {Key(*self)}
    }
}

impl fmt::Show for PartialKey {
    /// Returns an one- or two-letter representation of alphanumeric key.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        assert!(self.is_valid());
        if **self < 0 {
            write!(f, "{}", BASE36_MAP[(-**self - 1) as uint] as char)
        } else {
            let sixteens = **self / 36;
            let ones = **self % 36;
            write!(f, "{}{}", BASE36_MAP[sixteens as uint] as char,
                              BASE36_MAP[ones as uint] as char)
        }
    }
}

