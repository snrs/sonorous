// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Various macros.

#[macro_escape];

// Given a value of `Result<T,E>`, exits from the function with a return type `Result<U,E>` when
// the value is an error. Otherwise returns the value contained in the `Ok` variant.
macro_rules! earlyexit(
    ($e:expr) => (match $e { Ok(v) => v, Err(err) => { return Err(err); } })
)

