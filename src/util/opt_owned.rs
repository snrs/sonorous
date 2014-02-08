// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Optionally owned string and vector.

use std::clone::{Clone, DeepClone};
use std::cmp::{Eq, TotalEq, Ord, TotalOrd, Equiv};
use std::cmp::Ordering;
use std::container::Container;
use std::default::Default;
use std::str::{Str, StrSlice};
use std::vec::Vector;
use std::to_str::ToStr;
use std::to_bytes::{IterBytes, Cb};
use std::fmt::{Show, Formatter};
use std::io;

use util::into_send::IntoSend;

/// A string that can hold either `&'r str` or `~str`. This is an extension to
/// `std::send_str::OptOwnedStr` (which itself is homomorphic to `OptOwnedStr<'static>`).
pub enum OptOwnedStr<'r> {
    OptOwnedStrOwned(~str),
    OptOwnedStrBorrowed(&'r str),
}

/// A vector that can hold either `&'r [T]` or `~[T]`.
pub enum OptOwnedVec<'r,T> {
    OptOwnedVecOwned(~[T]),
    OptOwnedVecBorrowed(&'r [T]),
}

impl<'r> OptOwnedStr<'r> {
    /// Returns `true` if the string is owned.
    #[inline]
    pub fn is_owned(&self) -> bool {
        match *self {
            OptOwnedStrOwned(..) => true,
            OptOwnedStrBorrowed(..) => false,
        }
    }

    /// Returns `true` if the string is borrowed.
    #[inline]
    pub fn is_borrowed(&self) -> bool {
        match *self {
            OptOwnedStrOwned(..) => false,
            OptOwnedStrBorrowed(..) => true,
        }
    }
}

impl<'r,T> OptOwnedVec<'r,T> {
    /// Returns `true` if the vector is owned.
    #[inline]
    pub fn is_owned(&self) -> bool {
        match *self {
            OptOwnedVecOwned(..) => true,
            OptOwnedVecBorrowed(..) => false,
        }
    }

    /// Returns `true` if the vector is borrowed.
    #[inline]
    pub fn is_borrowed(&self) -> bool {
        match *self {
            OptOwnedVecOwned(..) => false,
            OptOwnedVecBorrowed(..) => true,
        }
    }
}

// * `IntoSend` implementations

impl<'r> IntoSend<OptOwnedStr<'static>> for OptOwnedStr<'r> {
    #[inline]
    fn into_send(self) -> OptOwnedStr<'static> {
        match self {
            OptOwnedStrOwned(s) => OptOwnedStrOwned(s),
            OptOwnedStrBorrowed(s) => OptOwnedStrOwned(s.to_owned()),
        }
    }
}

impl<'r,T:Clone> IntoSend<OptOwnedVec<'static,T>> for OptOwnedVec<'r,T> {
    #[inline]
    fn into_send(self) -> OptOwnedVec<'static,T> {
        match self {
            OptOwnedVecOwned(v) => OptOwnedVecOwned(v),
            OptOwnedVecBorrowed(v) => OptOwnedVecOwned(v.to_owned()),
        }
    }
}

// * `IntoOptOwned{Str,Vec}` traits

/// A trait for moving into an `OptOwnedStr`.
pub trait IntoOptOwnedStr<'r> {
    /// Moves `self` into an `OptOwnedStr`.
    fn into_opt_owned_str(self) -> OptOwnedStr<'r>;
}

/// A trait for moving into an `OptOwnedVec`.
pub trait IntoOptOwnedVec<'r,T> {
    /// Moves `self` into an `OptOwnedVec`.
    fn into_opt_owned_vec(self) -> OptOwnedVec<'r,T>;
}

impl IntoOptOwnedStr<'static> for ~str {
    #[inline]
    fn into_opt_owned_str(self) -> OptOwnedStr<'static> { OptOwnedStrOwned(self) }
}

impl<'r> IntoOptOwnedStr<'r> for &'r str {
    #[inline]
    fn into_opt_owned_str(self) -> OptOwnedStr<'r> { OptOwnedStrBorrowed(self) }
}

impl<T> IntoOptOwnedVec<'static,T> for ~[T] {
    #[inline]
    fn into_opt_owned_vec(self) -> OptOwnedVec<'static,T> { OptOwnedVecOwned(self) }
}

impl<'r,T> IntoOptOwnedVec<'r,T> for &'r [T] {
    #[inline]
    fn into_opt_owned_vec(self) -> OptOwnedVec<'r,T> { OptOwnedVecBorrowed(self) }
}

// * `Str` implementation

impl<'r> Str for OptOwnedStr<'r> {
    #[inline]
    fn as_slice<'r>(&'r self) -> &'r str {
        match *self {
            OptOwnedStrOwned(ref s) => s.as_slice(),
            OptOwnedStrBorrowed(s) => s,
        }
    }

    #[inline]
    fn into_owned(self) -> ~str {
        match self {
            OptOwnedStrOwned(s) => s,
            OptOwnedStrBorrowed(s) => s.to_owned(),
        }
    }
}

// * `Vector` implementation

impl<'r,T> Vector<T> for OptOwnedVec<'r,T> {
    #[inline]
    fn as_slice<'r>(&'r self) -> &'r [T] {
        match *self {
            OptOwnedVecOwned(ref v) => v.as_slice(),
            OptOwnedVecBorrowed(v) => v,
        }
    }
}

// * `ToStr` implementations

impl<'r> ToStr for OptOwnedStr<'r> {
    #[inline]
    fn to_str(&self) -> ~str { self.as_slice().to_owned() }
}

impl<'r,T:ToStr> ToStr for OptOwnedVec<'r,T> {
    #[inline]
    fn to_str(&self) -> ~str { self.as_slice().to_str() }
}

// * `Eq` implementations

impl<'r> Eq for OptOwnedStr<'r> {
    #[inline]
    fn eq(&self, other: &OptOwnedStr<'r>) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}

impl<'r,T:Eq> Eq for OptOwnedVec<'r,T> {
    #[inline]
    fn eq(&self, other: &OptOwnedVec<'r,T>) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}

// * `TotalEq` implementations

impl<'r> TotalEq for OptOwnedStr<'r> {
    #[inline]
    fn equals(&self, other: &OptOwnedStr<'r>) -> bool {
        self.as_slice().equals(&other.as_slice())
    }
}

impl<'r,T:TotalEq> TotalEq for OptOwnedVec<'r,T> {
    #[inline]
    fn equals(&self, other: &OptOwnedVec<'r,T>) -> bool {
        self.as_slice().equals(&other.as_slice())
    }
}

// * `Equiv` implementations

impl<'r,S:Str> Equiv<S> for OptOwnedStr<'r> {
    #[inline]
    fn equiv(&self, other: &S) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}

impl<'r,T:Eq,V:Vector<T>> Equiv<V> for OptOwnedVec<'r,T> {
    #[inline]
    fn equiv(&self, other: &V) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}

// * `Ord` implementations

impl<'r> Ord for OptOwnedStr<'r> {
    #[inline]
    fn lt(&self, other: &OptOwnedStr<'r>) -> bool {
        self.as_slice().lt(&other.as_slice())
    }

    #[inline]
    fn le(&self, other: &OptOwnedStr<'r>) -> bool {
        self.as_slice().le(&other.as_slice())
    }

    #[inline]
    fn gt(&self, other: &OptOwnedStr<'r>) -> bool {
        self.as_slice().gt(&other.as_slice())
    }

    #[inline]
    fn ge(&self, other: &OptOwnedStr<'r>) -> bool {
        self.as_slice().ge(&other.as_slice())
    }
}

impl<'r,T:Eq+Ord> Ord for OptOwnedVec<'r,T> {
    #[inline]
    fn lt(&self, other: &OptOwnedVec<'r,T>) -> bool {
        self.as_slice().lt(&other.as_slice())
    }

    #[inline]
    fn le(&self, other: &OptOwnedVec<'r,T>) -> bool {
        self.as_slice().le(&other.as_slice())
    }

    #[inline]
    fn gt(&self, other: &OptOwnedVec<'r,T>) -> bool {
        self.as_slice().gt(&other.as_slice())
    }

    #[inline]
    fn ge(&self, other: &OptOwnedVec<'r,T>) -> bool {
        self.as_slice().ge(&other.as_slice())
    }
}

// * `TotalOrd` implementations

impl<'r> TotalOrd for OptOwnedStr<'r> {
    #[inline]
    fn cmp(&self, other: &OptOwnedStr<'r>) -> Ordering {
        self.as_slice().cmp(&other.as_slice())
    }
}

impl<'r,T:TotalOrd> TotalOrd for OptOwnedVec<'r,T> {
    #[inline]
    fn cmp(&self, other: &OptOwnedVec<'r,T>) -> Ordering {
        self.as_slice().cmp(&other.as_slice())
    }
}

// * `Container` implementations

impl<'r> Container for OptOwnedStr<'r> {
    #[inline]
    fn len(&self) -> uint { self.as_slice().len() }
}

impl<'r,T> Container for OptOwnedVec<'r,T> {
    #[inline]
    fn len(&self) -> uint { self.as_slice().len() }
}

// * `Clone` implementations

impl<'r> Clone for OptOwnedStr<'r> {
    #[inline]
    fn clone(&self) -> OptOwnedStr<'r> {
        match *self {
            OptOwnedStrOwned(ref s) => OptOwnedStrOwned(s.clone()),
            OptOwnedStrBorrowed(s) => OptOwnedStrBorrowed(s),
        }
    }
}

impl<'r,T:Clone> Clone for OptOwnedVec<'r,T> {
    #[inline]
    fn clone(&self) -> OptOwnedVec<'r,T> {
        match *self {
            OptOwnedVecOwned(ref v) => OptOwnedVecOwned(v.clone()),
            OptOwnedVecBorrowed(v) => OptOwnedVecBorrowed(v),
        }
    }
}

// * `DeepClone` implementations

impl<'r> DeepClone for OptOwnedStr<'r> {
    #[inline]
    fn deep_clone(&self) -> OptOwnedStr<'r> {
        match *self {
            OptOwnedStrOwned(ref s) => OptOwnedStrOwned(s.deep_clone()),
            OptOwnedStrBorrowed(s) => OptOwnedStrBorrowed(s),
        }
    }
}

impl<'r,T:DeepClone> DeepClone for OptOwnedVec<'r,T> {
    #[inline]
    fn deep_clone(&self) -> OptOwnedVec<'r,T> {
        match *self {
            OptOwnedVecOwned(ref v) => OptOwnedVecOwned(v.deep_clone()),
            OptOwnedVecBorrowed(v) => OptOwnedVecBorrowed(v),
        }
    }
}

// * `Default` implementations

impl<'r> Default for OptOwnedStr<'r> {
    #[inline]
    fn default() -> OptOwnedStr<'r> { OptOwnedStrBorrowed("") }
}

impl<T> Default for OptOwnedVec<'static,T> {
    #[inline]
    fn default() -> OptOwnedVec<'static,T> { OptOwnedVecBorrowed(&'static []) }
}

// * `IterBytes` implementations

impl<'r> IterBytes for OptOwnedStr<'r> {
    #[inline]
    fn iter_bytes(&self, lsb0: bool, f: Cb) -> bool { self.as_slice().iter_bytes(lsb0, f) }
}

impl<'r,T:IterBytes> IterBytes for OptOwnedVec<'r,T> {
    #[inline]
    fn iter_bytes(&self, lsb0: bool, f: Cb) -> bool { self.as_slice().iter_bytes(lsb0, f) }
}

// * `Show` implementation

impl<'r> Show for OptOwnedStr<'r> {
    #[inline]
    fn fmt(s: &OptOwnedStr<'r>, out: &mut Formatter) -> io::IoResult<()> {
        write!(out.buf, "{}", s.as_slice())
    }
}

#[cfg(test)]
mod tests {
    use std::clone::{Clone, DeepClone};
    use std::cmp::{TotalEq, Ord, TotalOrd, Equiv};
    use std::cmp::Equal;
    use std::container::Container;
    use std::default::Default;
    use std::str::Str;
    use std::to_str::ToStr;
    use super::{OptOwnedStrOwned, OptOwnedStrBorrowed};

    #[test]
    fn test_opt_owned_str_traits() {
        let s = OptOwnedStrBorrowed("abcde");
        assert_eq!(s.len(), 5);
        assert_eq!(s.as_slice(), "abcde");
        assert_eq!(s.to_str(), ~"abcde");
        assert!(s.equiv(&~"abcde"));
        assert!(s.lt(&OptOwnedStrOwned(~"bcdef")));
        assert_eq!(OptOwnedStrBorrowed(""), Default::default());

        let o = OptOwnedStrOwned(~"abcde");
        assert_eq!(o.len(), 5);
        assert_eq!(o.as_slice(), "abcde");
        assert_eq!(o.to_str(), ~"abcde");
        assert!(o.equiv(&~"abcde"));
        assert!(o.lt(&OptOwnedStrBorrowed("bcdef")));
        assert_eq!(OptOwnedStrOwned(~""), Default::default());

        assert_eq!(s.cmp(&o), Equal);
        assert!(s.equals(&o));
        assert!(s.equiv(&o));

        assert_eq!(o.cmp(&s), Equal);
        assert!(o.equals(&s));
        assert!(o.equiv(&s));
    }

    #[test]
    fn test_opt_owned_str_methods() {
        let s = OptOwnedStrBorrowed("abcde");
        assert!(s.is_borrowed());
        assert!(!s.is_owned());

        let o = OptOwnedStrOwned(~"abcde");
        assert!(!o.is_borrowed());
        assert!(o.is_owned());
    }

    #[test]
    fn test_opt_owned_str_clone() {
        assert_eq!(OptOwnedStrOwned(~"abcde"), OptOwnedStrBorrowed("abcde").clone());
        assert_eq!(OptOwnedStrOwned(~"abcde"), OptOwnedStrBorrowed("abcde").deep_clone());

        assert_eq!(OptOwnedStrOwned(~"abcde"), OptOwnedStrOwned(~"abcde").clone());
        assert_eq!(OptOwnedStrOwned(~"abcde"), OptOwnedStrOwned(~"abcde").deep_clone());

        assert_eq!(OptOwnedStrBorrowed("abcde"), OptOwnedStrBorrowed("abcde").clone());
        assert_eq!(OptOwnedStrBorrowed("abcde"), OptOwnedStrBorrowed("abcde").deep_clone());

        assert_eq!(OptOwnedStrBorrowed("abcde"), OptOwnedStrOwned(~"abcde").clone());
        assert_eq!(OptOwnedStrBorrowed("abcde"), OptOwnedStrOwned(~"abcde").deep_clone());
    }

    #[test]
    fn test_opt_owned_str_into_owned() {
        assert_eq!(OptOwnedStrBorrowed("abcde").into_owned(), ~"abcde");
        assert_eq!(OptOwnedStrOwned(~"abcde").into_owned(), ~"abcde");
    }

    #[test]
    fn test_into_opt_owned_str() {
        assert_eq!("abcde".into_opt_owned_str(), OptOwnedStrBorrowed("abcde"));
        assert_eq!((~"abcde").into_opt_owned_str(), OptOwnedStrBorrowed("abcde"));
        assert_eq!("abcde".into_opt_owned_str(), OptOwnedStrOwned(~"abcde"));
        assert_eq!((~"abcde").into_opt_owned_str(), OptOwnedStrOwned(~"abcde"));
    }
}
