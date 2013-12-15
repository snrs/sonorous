// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! `into_send` utility trait.

/// A trait for non-`Send`able types which can be turned into a `Send`able copy.
pub trait IntoSend<T:Send> {
    /// Returns a `Send`able copy of the original value, which is consumed.
    fn into_send(self) -> T;
}

