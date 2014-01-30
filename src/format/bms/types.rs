// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Common types for BMS format.

use std::str;
use format::obj::Lane;

/// Two-letter alphanumeric identifier used for virtually everything, including resource
/// management, variable BPM and chart specification.
#[deriving(Eq,Ord,TotalEq,TotalOrd,Clone)]
pub struct Key(int);

/// The number of all possible alphanumeric keys.
pub static MAXKEY: int = 36*36;

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

    /// Returns a bare integer value out of the key.
    pub fn to_int(&self) -> int {
        let Key(v) = *self;
        v
    }

    /// Returns if the alphanumeric key is in the proper range. Sonorous supports the full
    /// range of 00-ZZ (0-1295) for every case.
    pub fn is_valid(&self) -> bool {
        let Key(v) = *self;
        0 <= v && v < MAXKEY
    }

    /// Re-reads the alphanumeric key as a hexadecimal number if possible. This is required
    /// due to handling of channel #03 (BPM is expected to be in hexadecimal).
    pub fn to_hex(&self) -> Option<int> {
        let Key(v) = *self;
        let sixteens = v / 36;
        let ones = v % 36;
        if sixteens < 16 && ones < 16 {Some(sixteens * 16 + ones)} else {None}
    }

    /// Converts the channel number to the lane number.
    pub fn to_lane(&self) -> Lane {
        let Key(v) = *self;
        let player = match v / 36 {
            1 | 3 | 5 | 0xD => 0,
            2 | 4 | 6 | 0xE => 1,
            _ => fail!(~"non-object channel")
        };
        Lane(player * 36 + v as uint % 36)
    }
}

impl ToStr for Key {
    /// Returns a two-letter representation of alphanumeric key.
    fn to_str(&self) -> ~str {
        let Key(v) = *self;
        assert!(self.is_valid());
        format!("{}{}", BASE36_MAP[v / 36] as char, BASE36_MAP[v % 36] as char)
    }
}

/// Same as `Key` but accepts one-letter alphanumeric keys, encoded as a negative number
/// from -1 (`0`) to -36 (`Z`) which is invalid in a plain `Key`.
#[deriving(Eq,Ord,Clone)]
pub struct PartialKey(int);

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

    /// Returns a bare integer value out of the key.
    pub fn to_int(&self) -> int {
        let PartialKey(v) = *self;
        v
    }

    /// Returns if the alphanumeric key is in the proper range.
    /// In addition to `Key`s 00-ZZ (0 to 1295), `PartialKey` supports 0-Z (-1 to -36).
    pub fn is_valid(&self) -> bool {
        let PartialKey(v) = *self;
        -36 <= v && v < MAXKEY
    }

    /// Returns if the alphanumeric key is one-digit long.
    pub fn is_partial(&self) -> bool {
        let PartialKey(v) = *self;
        v < 0
    }

    /// Re-reads the alphanumeric key as a hexadecimal number if possible.
    pub fn to_hex(&self) -> Option<int> {
        let PartialKey(v) = *self;
        let (sixteens, ones) = if v < 0 {(0, -v - 1)} else {(v / 36, v % 36)};
        if sixteens < 16 && ones < 16 {Some(sixteens * 16 + ones)} else {None}
    }

    /// Converts a partial alphanumeric key into a `Key`, assuming it's missing a leading `0`.
    pub fn into_key(self) -> Key {
        let PartialKey(v) = self;
        assert!(self.is_valid());
        if v < 0 {Key(-v - 1)} else {Key(v)}
    }
}

impl ToStr for PartialKey {
    /// Returns an one- or two-letter representation of alphanumeric key.
    fn to_str(&self) -> ~str {
        let PartialKey(v) = *self;
        assert!(self.is_valid());
        if v < 0 {
            format!("{}", BASE36_MAP[-v - 1] as char)
        } else {
            format!("{}{}", BASE36_MAP[v / 36] as char, BASE36_MAP[v % 36] as char)
        }
    }
}

