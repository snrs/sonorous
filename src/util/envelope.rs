// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Envelope for sending a non-sendable but effectively owned type across tasks.

use std::cast;

/// A wrapper to send a non-sendable owned type across tasks. Use with care.
pub struct Envelope<T> {
    priv wrapped: uint
}

impl<T> Envelope<T> {
    /// Creates a sendable wrapper out of the owned value.
    pub fn new(res: T) -> Envelope<T> {
        unsafe {
            Envelope { wrapped: cast::transmute::<~T,uint>(~res) }
        }
    }

    /// Converts a sendable wrapper to the owned value.
    pub fn unwrap(self) -> T {
        unsafe {
            let ret: ~T = cast::transmute::<uint,~T>(self.wrapped);
            cast::forget(self);
            *ret
        }
    }
}

#[unsafe_destructor]
impl<T> Drop for Envelope<T> {
    fn drop(&mut self) {
        unsafe {
            let _: ~T = cast::transmute::<uint,~T>(self.wrapped);
        }
    }
}

