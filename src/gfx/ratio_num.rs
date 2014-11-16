// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A combined relative ("ratio") and absolute ("num") value.

use std::fmt;
use std::num::{Int, Float};
use std::default::Default;

/// The sum of relative value (`ratio`) and absolute value (`num`), i.e. `ratio * base + num`.
/// This is widely used for the specification and calculation of texture and window coordinates:
/// `50%-30` would encode as `ratio` of 0.5 and `num` of -30 for example.
#[deriving(Clone)]
pub struct RatioNum<T> {
    /// The relative component.
    pub ratio: T,
    /// The absolute component.
    pub num: T,
}

impl<T:Float> RatioNum<T> {
    /// Returns a zero value.
    pub fn zero() -> RatioNum<T> {
        RatioNum { ratio: Float::zero(), num: Float::zero() }
    }

    /// Returns a combined value out of the absolute value.
    pub fn from_num(num: T) -> RatioNum<T> {
        RatioNum { ratio: Float::zero(), num: num }
    }

    /// Returns a combined value out of the relative value.
    pub fn from_ratio(ratio: T) -> RatioNum<T> {
        RatioNum { ratio: ratio, num: Float::zero() }
    }
}

impl<T:Int> RatioNum<T> {
    /// Returns `Some(self + rhs)` if it wouldn't occur overflow, or `None` otherwise.
    pub fn checked_add(&self, rhs: &RatioNum<T>) -> Option<RatioNum<T>> {
        let ratio = match self.ratio.checked_add(rhs.ratio) { Some(v) => v, None => return None };
        let num   = match self.num  .checked_add(rhs.num  ) { Some(v) => v, None => return None };
        Some(RatioNum { ratio: ratio, num: num })
    }

    /// Returns `Some(self - rhs)` if it wouldn't occur overflow, or `None` otherwise.
    pub fn checked_sub(&self, rhs: &RatioNum<T>) -> Option<RatioNum<T>> {
        let ratio = match self.ratio.checked_sub(rhs.ratio) { Some(v) => v, None => return None };
        let num   = match self.num  .checked_sub(rhs.num  ) { Some(v) => v, None => return None };
        Some(RatioNum { ratio: ratio, num: num })
    }
}

impl<T:Float> RatioNum<T> {
    /// Returns a value equal to the reference base value. Note that this is not
    /// the true multiplicative identity, and `RatioNum` doesn't implement `One`.
    pub fn one() -> RatioNum<T> {
        RatioNum { ratio: Float::one(), num: Float::zero() }
    }
}

impl<T:Add<T,T>+Mul<T,T>> RatioNum<T> {
    /// Given the base value to resolve the relative part, returns a calculated absolute value.
    pub fn to_num(&self, base: &T) -> T {
        self.ratio * *base + self.num
    }
}

impl<T:Add<T,T>+Div<T,T>> RatioNum<T> {
    /// Given the base value to resolve the relative part, returns a calculated relative value.
    pub fn to_ratio(&self, base: &T) -> T {
        self.ratio + self.num / *base
    }
}

impl<T:Add<T,T>+Mul<T,T>> RatioNum<T> {
    /// Substitutes the base value with another combined relative-absolute value:
    ///
    /// ```notrust
    ///       |                      |
    ///       |----|<------>|--------| self
    /// |     |    |        |        |
    /// |-----|<-------------------->|--| other
    /// |          |        |           |
    /// |----------|<------>|-----------| self.subst(&other)
    /// |                               |
    /// ```
    pub fn subst(&self, other: &RatioNum<T>) -> RatioNum<T> {
        // a (a' x + b') + b = (a a') x + (a b' + b)
        RatioNum { ratio: self.ratio * other.ratio, num: self.ratio * other.num + self.num }
    }
}

impl<T:Default> Default for RatioNum<T> {
    fn default() -> RatioNum<T> {
        RatioNum { ratio: Default::default(), num: Default::default() }
    }
}

impl<T:Neg<Result>, Result> Neg<RatioNum<Result>> for RatioNum<T> {
    fn neg(&self) -> RatioNum<Result> {
        RatioNum { ratio: -self.ratio, num: -self.num }
    }
}

impl<T:Add<RHS,Result>, RHS, Result> Add<RatioNum<RHS>, RatioNum<Result>> for RatioNum<T> {
    fn add(&self, rhs: &RatioNum<RHS>) -> RatioNum<Result> {
        RatioNum { ratio: self.ratio + rhs.ratio, num: self.num + rhs.num }
    }
}

impl<T:Sub<RHS,Result>, RHS, Result> Sub<RatioNum<RHS>, RatioNum<Result>> for RatioNum<T> {
    fn sub(&self, rhs: &RatioNum<RHS>) -> RatioNum<Result> {
        RatioNum { ratio: self.ratio - rhs.ratio, num: self.num - rhs.num }
    }
}

impl<T:Mul<RHS,Result>, RHS, Result> Mul<RHS, RatioNum<Result>> for RatioNum<T> {
    fn mul(&self, rhs: &RHS) -> RatioNum<Result> {
        RatioNum { ratio: self.ratio * *rhs, num: self.num * *rhs }
    }
}

impl<T:Div<RHS,Result>, RHS, Result> Div<RHS, RatioNum<Result>> for RatioNum<T> {
    fn div(&self, rhs: &RHS) -> RatioNum<Result> {
        RatioNum { ratio: self.ratio / *rhs, num: self.num / *rhs }
    }
}

impl<T:fmt::Show+Float+Mul<T,T>+FromPrimitive> fmt::Show for RatioNum<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.ratio == Float::zero() {
            self.num.fmt(f)
        } else {
            let hundred = FromPrimitive::from_u64(100).unwrap();
            if self.num == Float::zero() {
                write!(f, "{}%", self.ratio * hundred)
            } else {
                write!(f, "{}%{:+}", self.ratio * hundred, self.num)
            }
        }
    }
}

