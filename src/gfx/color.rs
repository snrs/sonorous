// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Utilities for SDL colors.

pub use sdl::video::{Color, RGB, RGBA};

/// Extracts red, green, blue components from given color.
pub fn to_rgb(c: Color) -> (u8, u8, u8) {
    match c { RGB(r, g, b) | RGBA(r, g, b, _) => (r, g, b) }
}

/// Extracts red, green, blue and alpha components from given color. A color without alpha is
/// assumed to be totally opaque.
pub fn to_rgba(c: Color) -> (u8, u8, u8, u8) {
    match c { RGB(r, g, b) => (r, g, b, 255), RGBA(r, g, b, a) => (r, g, b, a) }
}

/// Linear color gradient.
#[deriving(Eq)]
pub struct Gradient {
    /// A color at the position 0.0. Normally used as a topmost value.
    zero: Color,
    /// A color at the position 1.0. Normally used as a bottommost value.
    one: Color
}

/// Creates a new color gradient (for text printing).
pub fn Gradient(top: Color, bottom: Color) -> Gradient {
    Gradient { zero: top, one: bottom }
}

/// A trait for color or color gradient. The color at the particular position can be calculated
/// with `blend` method.
pub trait Blend {
    /// Returns itself. This is same as `Clone::clone` but redefined here due to the inability
    /// of implementing `Clone` for `Color`.
    fn clone(&self) -> Self;
    /// Calculates the color at the position `num/denom`.
    fn blend(&self, num: int, denom: int) -> Color;
}

impl Blend for Color {
    fn clone(&self) -> Color { *self }
    fn blend(&self, _num: int, _denom: int) -> Color { *self }
}

impl Blend for Gradient {
    fn clone(&self) -> Gradient { *self }
    fn blend(&self, num: int, denom: int) -> Color {
        fn mix(x: u8, y: u8, num: int, denom: int) -> u8 {
            let x = x as int;
            let y = y as int;
            (y + ((x - y) * num / denom)) as u8
        }

        let (r0, g0, b0) = to_rgb(self.zero);
        let (r1, g1, b1) = to_rgb(self.one);
        RGB(mix(r1, r0, num, denom), mix(g1, g0, num, denom), mix(b1, b0, num, denom))
    }
}

