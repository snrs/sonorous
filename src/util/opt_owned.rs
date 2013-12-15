// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
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
use std::fmt;

use util::into_send::IntoSend;

/// A string that can hold either `&'self str` or `~str`. This is an extension to
/// `std::send_str::OptOwnedStr` (which itself is homomorphic to `OptOwnedStr<'static>`).
pub enum OptOwnedStr<'self> {
    OptOwnedStrOwned(~str),
    OptOwnedStrBorrowed(&'self str),
}

/// A vector that can hold either `&'self [T]` or `~[T]`.
pub enum OptOwnedVec<'self,T> {
    OptOwnedVecOwned(~[T]),
    OptOwnedVecBorrowed(&'self [T]),
}

impl<'self> OptOwnedStr<'self> {
    /// Returns `true` if the string is owned.
    #[inline]
    pub fn is_owned(&self) -> bool {
        match *self {
            OptOwnedStrOwned(*) => true,
            OptOwnedStrBorrowed(*) => false,
        }
    }

    /// Returns `true` if the string is borrowed.
    #[inline]
    pub fn is_borrowed(&self) -> bool {
        match *self {
            OptOwnedStrOwned(*) => false,
            OptOwnedStrBorrowed(*) => true,
        }
    }
}

impl<'self,T> OptOwnedVec<'self,T> {
    /// Returns `true` if the vector is owned.
    #[inline]
    pub fn is_owned(&self) -> bool {
        match *self {
            OptOwnedVecOwned(*) => true,
            OptOwnedVecBorrowed(*) => false,
        }
    }

    /// Returns `true` if the vector is borrowed.
    #[inline]
    pub fn is_borrowed(&self) -> bool {
        match *self {
            OptOwnedVecOwned(*) => false,
            OptOwnedVecBorrowed(*) => true,
        }
    }
}

// * `IntoSend` implementations

impl<'self> IntoSend<OptOwnedStr<'static>> for OptOwnedStr<'self> {
    #[inline]
    fn into_send(self) -> OptOwnedStr<'static> {
        match self {
            OptOwnedStrOwned(s) => OptOwnedStrOwned(s),
            OptOwnedStrBorrowed(s) => OptOwnedStrOwned(s.to_owned()),
        }
    }
}

impl<'self,T:Clone> IntoSend<OptOwnedVec<'static,T>> for OptOwnedVec<'self,T> {
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
pub trait IntoOptOwnedStr<'self> {
    /// Moves `self` into an `OptOwnedStr`.
    fn into_opt_owned_str(self) -> OptOwnedStr<'self>;
}

/// A trait for moving into an `OptOwnedVec`.
pub trait IntoOptOwnedVec<'self,T> {
    /// Moves `self` into an `OptOwnedVec`.
    fn into_opt_owned_vec(self) -> OptOwnedVec<'self,T>;
}

impl IntoOptOwnedStr<'static> for ~str {
    #[inline]
    fn into_opt_owned_str(self) -> OptOwnedStr<'static> { OptOwnedStrOwned(self) }
}

impl<'self> IntoOptOwnedStr<'self> for &'self str {
    #[inline]
    fn into_opt_owned_str(self) -> OptOwnedStr<'self> { OptOwnedStrBorrowed(self) }
}

impl<T> IntoOptOwnedVec<'static,T> for ~[T] {
    #[inline]
    fn into_opt_owned_vec(self) -> OptOwnedVec<'static,T> { OptOwnedVecOwned(self) }
}

impl<'self,T> IntoOptOwnedVec<'self,T> for &'self [T] {
    #[inline]
    fn into_opt_owned_vec(self) -> OptOwnedVec<'self,T> { OptOwnedVecBorrowed(self) }
}

// * `Str` implementation

impl<'self> Str for OptOwnedStr<'self> {
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

impl<'self,T> Vector<T> for OptOwnedVec<'self,T> {
    #[inline]
    fn as_slice<'r>(&'r self) -> &'r [T] {
        match *self {
            OptOwnedVecOwned(ref v) => v.as_slice(),
            OptOwnedVecBorrowed(v) => v,
        }
    }
}

// * `ToStr` implementations

impl<'self> ToStr for OptOwnedStr<'self> {
    #[inline]
    fn to_str(&self) -> ~str { self.as_slice().to_owned() }
}

impl<'self,T:ToStr> ToStr for OptOwnedVec<'self,T> {
    #[inline]
    fn to_str(&self) -> ~str { self.as_slice().to_str() }
}

// * `Eq` implementations

impl<'self> Eq for OptOwnedStr<'self> {
    #[inline]
    fn eq(&self, other: &OptOwnedStr<'self>) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}

impl<'self,T:Eq> Eq for OptOwnedVec<'self,T> {
    #[inline]
    fn eq(&self, other: &OptOwnedVec<'self,T>) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}

// * `TotalEq` implementations

impl<'self> TotalEq for OptOwnedStr<'self> {
    #[inline]
    fn equals(&self, other: &OptOwnedStr<'self>) -> bool {
        self.as_slice().equals(&other.as_slice())
    }
}

impl<'self,T:TotalEq> TotalEq for OptOwnedVec<'self,T> {
    #[inline]
    fn equals(&self, other: &OptOwnedVec<'self,T>) -> bool {
        self.as_slice().equals(&other.as_slice())
    }
}

// * `Equiv` implementations

impl<'self,S:Str> Equiv<S> for OptOwnedStr<'self> {
    #[inline]
    fn equiv(&self, other: &S) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}

impl<'self,T:Eq,V:Vector<T>> Equiv<V> for OptOwnedVec<'self,T> {
    #[inline]
    fn equiv(&self, other: &V) -> bool {
        self.as_slice().eq(&other.as_slice())
    }
}

// * `Ord` implementations

impl<'self> Ord for OptOwnedStr<'self> {
    #[inline]
    fn lt(&self, other: &OptOwnedStr<'self>) -> bool {
        self.as_slice().lt(&other.as_slice())
    }

    #[inline]
    fn le(&self, other: &OptOwnedStr<'self>) -> bool {
        self.as_slice().le(&other.as_slice())
    }

    #[inline]
    fn gt(&self, other: &OptOwnedStr<'self>) -> bool {
        self.as_slice().gt(&other.as_slice())
    }

    #[inline]
    fn ge(&self, other: &OptOwnedStr<'self>) -> bool {
        self.as_slice().ge(&other.as_slice())
    }
}

impl<'self,T:Eq+Ord> Ord for OptOwnedVec<'self,T> {
    #[inline]
    fn lt(&self, other: &OptOwnedVec<'self,T>) -> bool {
        self.as_slice().lt(&other.as_slice())
    }

    #[inline]
    fn le(&self, other: &OptOwnedVec<'self,T>) -> bool {
        self.as_slice().le(&other.as_slice())
    }

    #[inline]
    fn gt(&self, other: &OptOwnedVec<'self,T>) -> bool {
        self.as_slice().gt(&other.as_slice())
    }

    #[inline]
    fn ge(&self, other: &OptOwnedVec<'self,T>) -> bool {
        self.as_slice().ge(&other.as_slice())
    }
}

// * `TotalOrd` implementations

impl<'self> TotalOrd for OptOwnedStr<'self> {
    #[inline]
    fn cmp(&self, other: &OptOwnedStr<'self>) -> Ordering {
        self.as_slice().cmp(&other.as_slice())
    }
}

impl<'self,T:TotalOrd> TotalOrd for OptOwnedVec<'self,T> {
    #[inline]
    fn cmp(&self, other: &OptOwnedVec<'self,T>) -> Ordering {
        self.as_slice().cmp(&other.as_slice())
    }
}

// * `Container` implementations

impl<'self> Container for OptOwnedStr<'self> {
    #[inline]
    fn len(&self) -> uint { self.as_slice().len() }
}

impl<'self,T> Container for OptOwnedVec<'self,T> {
    #[inline]
    fn len(&self) -> uint { self.as_slice().len() }
}

// * `Clone` implementations

impl<'self> Clone for OptOwnedStr<'self> {
    #[inline]
    fn clone(&self) -> OptOwnedStr<'self> {
        match *self {
            OptOwnedStrOwned(ref s) => OptOwnedStrOwned(s.clone()),
            OptOwnedStrBorrowed(s) => OptOwnedStrBorrowed(s),
        }
    }
}

impl<'self,T:Clone> Clone for OptOwnedVec<'self,T> {
    #[inline]
    fn clone(&self) -> OptOwnedVec<'self,T> {
        match *self {
            OptOwnedVecOwned(ref v) => OptOwnedVecOwned(v.clone()),
            OptOwnedVecBorrowed(v) => OptOwnedVecBorrowed(v),
        }
    }
}

// * `DeepClone` implementations

impl<'self> DeepClone for OptOwnedStr<'self> {
    #[inline]
    fn deep_clone(&self) -> OptOwnedStr<'self> {
        match *self {
            OptOwnedStrOwned(ref s) => OptOwnedStrOwned(s.deep_clone()),
            OptOwnedStrBorrowed(s) => OptOwnedStrBorrowed(s),
        }
    }
}

impl<'self,T:DeepClone> DeepClone for OptOwnedVec<'self,T> {
    #[inline]
    fn deep_clone(&self) -> OptOwnedVec<'self,T> {
        match *self {
            OptOwnedVecOwned(ref v) => OptOwnedVecOwned(v.deep_clone()),
            OptOwnedVecBorrowed(v) => OptOwnedVecBorrowed(v),
        }
    }
}

// * `Default` implementations

impl<'self> Default for OptOwnedStr<'self> {
    #[inline]
    fn default() -> OptOwnedStr<'self> { OptOwnedStrBorrowed("") }
}

impl<T> Default for OptOwnedVec<'static,T> {
    #[inline]
    fn default() -> OptOwnedVec<'static,T> { OptOwnedVecBorrowed(&'static []) }
}

// * `IterBytes` implementations

impl<'self> IterBytes for OptOwnedStr<'self> {
    #[inline]
    fn iter_bytes(&self, lsb0: bool, f: Cb) -> bool { self.as_slice().iter_bytes(lsb0, f) }
}

impl<'self,T:IterBytes> IterBytes for OptOwnedVec<'self,T> {
    #[inline]
    fn iter_bytes(&self, lsb0: bool, f: Cb) -> bool { self.as_slice().iter_bytes(lsb0, f) }
}

// * `fmt::Default` implementation

impl<'self> fmt::Default for OptOwnedStr<'self> {
    #[inline]
    fn fmt(s: &OptOwnedStr<'self>, formatter: &mut fmt::Formatter) {
        fmt::Default::fmt(&s.as_slice(), formatter)
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
        assert!(s.equiv(&@"abcde"));
        assert!(s.lt(&OptOwnedStrOwned(~"bcdef")));
        assert_eq!(OptOwnedStrBorrowed(""), Default::default());

        let o = OptOwnedStrOwned(~"abcde");
        assert_eq!(o.len(), 5);
        assert_eq!(o.as_slice(), "abcde");
        assert_eq!(o.to_str(), ~"abcde");
        assert!(o.equiv(&@"abcde"));
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
