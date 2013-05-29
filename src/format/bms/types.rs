// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

use format::obj::Lane;

//----------------------------------------------------------------------------------------------
// alphanumeric key

/// Two-letter alphanumeric identifier used for virtually everything, including resource
/// management, variable BPM and chart specification.
#[deriving(Eq)]
pub struct Key(int);

/// The number of all possible alphanumeric keys. (C: `MAXKEY`)
pub static MAXKEY: int = 36*36;

/// Converts a single alphanumeric (base-36) letter to an integer. (C: `getdigit`)
fn getdigit(n: char) -> Option<int> {
    match n {
        '0'..'9' => Some((n as int) - ('0' as int)),
        'a'..'z' => Some((n as int) - ('a' as int) + 10),
        'A'..'Z' => Some((n as int) - ('A' as int) + 10),
        _ => None
    }
}

pub impl Key {
    /// Converts the first two letters of `s` to a `Key`. (C: `key2index`)
    fn from_chars(s: &[char]) -> Option<Key> {
        if s.len() < 2 { return None; }
        do getdigit(s[0]).chain |a| {
            do getdigit(s[1]).map |&b| { Key(a * 36 + b) }
        }
    }

    /// Converts the first two letters of `s` to a `Key`. (C: `key2index`)
    fn from_str(s: &str) -> Option<Key> {
        if s.len() < 2 { return None; }
        let str::CharRange {ch:c1, next:p1} = str::char_range_at(s, 0);
        do getdigit(c1).chain |a| {
            let str::CharRange {ch:c2, next:p2} = str::char_range_at(s, p1);
            do getdigit(c2).map |&b| {
                assert!(p2 == 2); // both characters should be in ASCII
                Key(a * 36 + b)
            }
        }
    }

    /// Returns if the alphanumeric key is in the proper range. Angolmois supports the full
    /// range of 00-ZZ (0-1295) for every case.
    fn is_valid(self) -> bool {
        0 <= *self && *self < MAXKEY
    }

    /// Re-reads the alphanumeric key as a hexadecimal number if possible. This is required
    /// due to handling of channel #03 (BPM is expected to be in hexadecimal).
    fn to_hex(self) -> Option<int> {
        let sixteens = *self / 36, ones = *self % 36;
        if sixteens < 16 && ones < 16 {Some(sixteens * 16 + ones)} else {None}
    }

    /// Converts the channel number to the lane number.
    fn to_lane(self) -> Lane {
        let player = match *self / 36 {
            1 | 3 | 5 | 0xD => 0,
            2 | 4 | 6 | 0xE => 1,
            _ => fail!(~"non-object channel")
        };
        Lane(player * 36 + *self as uint % 36)
    }

}

impl Ord for Key {
    // Rust: it is very easy to make an infinite recursion here.
    fn lt(&self, other: &Key) -> bool { **self < **other }
    fn le(&self, other: &Key) -> bool { **self <= **other }
    fn ge(&self, other: &Key) -> bool { **self >= **other }
    fn gt(&self, other: &Key) -> bool { **self > **other }
}


impl ToStr for Key {
    /// Returns a two-letter representation of alphanumeric key. (C: `TO_KEY`)
    fn to_str(&self) -> ~str {
        assert!(self.is_valid());
        let map = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        fmt!("%c%c", map[**self / 36] as char, map[**self % 36] as char)
    }
}

