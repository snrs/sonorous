// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Skin engine.

use gfx::color::Blend;
use gfx::bmfont::{NCOLUMNS, Alignment, LeftAligned, Centered, RightAligned};
use gfx::screen::{ShadedFontDrawing};

pub mod scalar;
pub mod ast;
pub mod parse;
pub mod hook;
pub mod render;

/// Extensions to `ShadedFontDrawing`.
pub trait ShadedFontDrawingAdditions {
    /// Same as `ShadedFontDrawing::string` but renders padded zeroes in the other color.
    fn numeral<ColorT:Blend,ZeroColorT:Blend>(&mut self, x: f32, y: f32, zoom: f32,
                                              align: Alignment, s: &str,
                                              color: ColorT, zerocolor: ZeroColorT);
}

impl ShadedFontDrawingAdditions for ShadedFontDrawing {
    fn numeral<ColorT:Blend,ZeroColorT:Blend>(&mut self, x: f32, y: f32, zoom: f32,
                                              align: Alignment, s: &str,
                                              color: ColorT, zerocolor: ZeroColorT) {
        // resolve the alignment here
        let x = match align {
            LeftAligned  => x,
            Centered     => x - s.char_len() as f32 * (NCOLUMNS as f32 * zoom) / 2.0,
            RightAligned => x - s.char_len() as f32 * (NCOLUMNS as f32 * zoom),
        };
        let mut zeroes = s.find(|c: char| c != '0').unwrap_or(s.len());
        if zeroes == s.len() || !s.char_at(zeroes).is_digit() {
            zeroes -= 1; // keep at least one zero
        }
        if zeroes > 0 {
            self.string(x, y, zoom, LeftAligned, s.slice_to(zeroes), zerocolor);
        }
        self.string(x + zeroes as f32 * (8.0 * zoom), y, zoom, LeftAligned,
                    s.slice_from(zeroes), color);
    }
}
