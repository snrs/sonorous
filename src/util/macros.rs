// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Various macros.

#[macro_escape];

/// Immediately returns on `Err`, otherwise returns the `Ok` value.
/// This is a copy of the `try!` macro from libstd (the naming is not yet settled).
macro_rules! try(
    ($e:expr) => (match $e { Ok(e) => e, Err(e) => return Err(e) })
)

