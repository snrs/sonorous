// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Bitmap font.

use std::{uint, vec};
use util::gfx::*;

/// Bit vector which represents one row of zoomed font.
type ZoomedFontRow = u32;

/// 8x16 resizable bitmap font.
pub struct Font {
    /**
     * Font data used for zoomed font reconstruction. This is actually an array of `u32`
     * elements, where the first `u16` element forms upper 16 bits and the second forms lower
     * 16 bits. It is reinterpreted for better compression. (C: `fontdata`)
     *
     * One glyph has 16 `u32` elements for each row from the top to the bottom. One `u32`
     * element contains eight four-bit groups for each column from the left (lowermost group)
     * to the right (uppermost group). Each group is a bitwise OR of following bits:
     *
     * - 1: the lower right triangle of the zoomed pixel should be drawn.
     * - 2: the lower left triangle of the zoomed pixel should be drawn.
     * - 4: the upper left triangle of the zoomed pixel should be drawn.
     * - 8: the upper right triangle of the zoomed pixel should be drawn.
     *
     * So for example, if the group bits read 3 (1+2), the zoomed pixel would be drawn
     * as follows (in the zoom factor 5):
     *
     *     .....
     *     #...#
     *     ##.##
     *     #####
     *     #####
     *
     * The group bits 15 (1+2+4+8) always draw the whole square, so in the zoom factor 1 only
     * pixels with group bits 15 will be drawn.
     */
    glyphs: ~[u16],

    /// Precalculated zoomed font per zoom factor. It is three-dimensional array which indices
    /// are zoom factor, glyph number and row respectively. Assumes that each element has
    /// at least zoom factor times 8 (columns per row) bits. (C: `zoomfont`)
    pixels: ~[~[~[ZoomedFontRow]]]
}

/// An alignment mode of `Font::print_string`.
pub enum Alignment {
    /// Coordinates specify the top-left corner of the bounding box.
    LeftAligned,
    /// Coordinates specify the top-center point of the bounding box.
    Centered,
    /// Coordinates specify the top-right corner of the bounding box.
    RightAligned
}

/// Decompresses a bitmap font data. `Font::create_zoomed_font` is required for the actual use.
pub fn Font() -> Font {
    // Delta-coded code words. (C: `words`)
    let dwords = [0, 2, 6, 2, 5, 32, 96, 97, 15, 497, 15, 1521, 15, 1537,
        16, 48, 176, 1, 3, 1, 3, 7, 1, 4080, 4096, 3, 1, 8, 3, 4097, 4080,
        16, 16128, 240, 1, 2, 9, 3, 8177, 15, 16385, 240, 15, 1, 47, 721,
        143, 2673, 2, 6, 7, 1, 31, 17, 16, 63, 64, 33, 0, 1, 2, 1, 8, 3];

    // LZ77-compressed indices to code words:
    // - Byte 33..97 encodes a literal code word 0..64;
    // - Byte 98..126 encodes an LZ77 length distance pair with length 3..31;
    //   the following byte 33..126 encodes a distance 1..94.
    // (C: `indices`)
    let indices =
        ~"!!7a/&/&s$7a!f!'M*Q*Qc$(O&J!!&J&Jc(e!2Q2Qc$-Bg2m!2bB[Q7Q2[e&2Q!Qi>&!&!>UT2T2&2>WT!c*\
          T2GWc8icM2U2D!.8(M$UQCQ-jab!'U*2*2*2TXbZ252>9ZWk@*!*!*8(J$JlWi@cxQ!Q!d$#Q'O*?k@e2dfe\
          jcNl!&JTLTLG_&J>]c*&Jm@cB&J&J7[e(o>pJM$Qs<7[{Zj`Jm40!3!.8(M$U!C!-oR>UQ2U2]2a9Y[S[QCQ\
          2GWk@*M*Q*B*!*!g$aQs`G8.M(U$[!Ca[o@Q2Q!IJQ!Q!c,GWk@787M6U2C2d!a[2!2k?!bnc32>[u`>Uc4d\
          @b(q@abXU!D!.8(J&J&d$q`Q2IXu`g@Q2aWQ!q@!!ktk,x@M$Qk@3!.8(M$U!H#W'O,?4m_f!7[i&n!:eX5g\
          hCk=>UQ2Q2U2Dc>J!!&J&b&k@J)LKg!GK!)7Wk@'8,M=UWCcfa[c&Q2l`f4If(Q2G[l@MSUQC!2!2c$Q:RWG\
          Ok@,[<2WfZQ2U2D2.l`a[eZ7f(!2b2|@b$j!>MSUQCc6[2W2Q:RWGOk@Q2Q2c$a[g*Ql`7[&J&Jk$7[l`!Qi\
          $d^GWk@U2D2.9([$[#['[,@<2W2k@!2!2m$a[l`:^[a[a[T2Td~c$k@d2:R[V[a@_b|o@,M=UWCgZU:EW.Ok\
          @>[g<G[!2!2d$k@Ug@Q2V2a2IW_!Wt`Ih*q`!2>WQ!Q!c,Gk_!7[&J&Jm$k@gti$m`k:U:EW.O(?s@T2Tb$a\
          [CW2Qk@M+U:^[GbX,M>U`[WCO-l@'U,D<.W(O&J&Je$k@a[Q!U!]!G8.M(U$[!Ca[k@*Q!Q!l$b2m!+!:#W'\
          O,?4!1n;c`*!*!l$h`'8,M=UWCO-pWz!a[i,#Q'O,?4~R>QQ!Q!aUQ2Q2Q2aWl=2!2!2>[e<c$G[p`dZcHd@\
          l`czi|c$al@i`b:[!2Un`>8TJTJ&J7[&b&e$o`i~aWQ!c(hd2!2!2>[g@e$k]epi|e0i!bph(d$dbGWhA2!2\
          U2D2.9(['[,@<2W2k`*J*?*!*!k$o!;[a[T2T2c$c~o@>[c6i$p@Uk>GW}`G[!2!2b$h!al`aWQ!Q!Qp`fVl\
          Zf@UWb6>eX:GWk<&J&J7[c&&JTJTb$G?o`c~i$m`k@U:EW.O(v`T2Tb$a[Fp`M+eZ,M=UWCO-u`Q:RWGO.A(\
          M$U!Ck@a[]!G8.M(U$[!Ca[i:78&J&Jc$%[g*7?e<g0w$cD#iVAg*$[g~dB]NaaPGft~!f!7[.W(O";

    /// Decompresses a font data from `dwords` and `indices`. (C: `fontdecompress`)
    fn decompress(dwords: &[u16], indices: &str) -> ~[u16] {
        let mut words = ~[0];
        for dwords.iter().advance |&delta| {
            let last = *words.last();
            words.push(last + delta);
        }

        let nindices = indices.len();
        let mut i = 0;
        let mut glyphs = ~[];
        while i < nindices {
            let code = indices[i] as uint;
            i += 1;
            match code {
                33..97 => { glyphs.push(words[code - 33]); }
                98..126 => {
                    let length = code - 95; // code=98 -> length=3
                    let distance = indices[i] as uint - 32;
                    i += 1;
                    let start = glyphs.len() - distance;
                    for uint::range(start, start + length) |i| {
                        glyphs.push(glyphs[i]);
                    }
                }
                _ => fail!(~"unexpected codeword")
            }
        }
        glyphs
    }

    let glyphs = decompress(dwords, indices);
    assert!(glyphs.len() == 3072);
    Font { glyphs: glyphs, pixels: ~[] }
}

impl Font {
    /// Creates a zoomed font of scale `zoom`. (C: `fontprocess`)
    pub fn create_zoomed_font(&mut self, zoom: uint) {
        assert!(zoom > 0);
        assert!(zoom <= (8 * ::std::sys::size_of::<ZoomedFontRow>()) / 8);
        if zoom < self.pixels.len() && !self.pixels[zoom].is_empty() { return; }

        let nrows = 16;
        let nglyphs = self.glyphs.len() / nrows / 2;
        let mut pixels = vec::from_elem(nglyphs, vec::from_elem(zoom*nrows, 0));

        let put_zoomed_pixel = |glyph: uint, row: uint, col: uint, v: u32| {
            let zoomrow = row * zoom;
            let zoomcol = col * zoom;
            for uint::range(0, zoom) |r| {
                for uint::range(0, zoom) |c| {
                    let mut mask = 0;
                    if r + c >= zoom    { mask |= 1; } // lower right
                    if r > c            { mask |= 2; } // lower left
                    if r < c            { mask |= 4; } // upper right
                    if r + c < zoom - 1 { mask |= 8; } // upper left

                    // if `zoom` is odd, drawing four corner triangles leaves one center pixel
                    // intact since we don't draw diagonals for aesthetic reason. such case
                    // must be specially handled.
                    if (v & mask) != 0 || v == 15 {
                        pixels[glyph][zoomrow+r] |= 1 << (zoomcol+c);
                    }
                }
            }
        };

        let mut i = 0;
        for uint::range(0, nglyphs) |glyph| {
            for uint::range(0, nrows) |row| {
                let data = (self.glyphs[i] as u32 << 16) | (self.glyphs[i+1] as u32);
                i += 2;
                for uint::range(0, 8) |col| {
                    let v = (data >> (4 * col)) & 15;
                    put_zoomed_pixel(glyph, row, col, v);
                }
            }
        }
        self.pixels.grow_set(zoom, &~[], pixels);
    }

    /// Prints a glyph with given position and color (possibly gradient). This method is
    /// distinct from `print_glyph` since the glyph #95 is used for the tick marker
    /// (character code -1 in C). (C: `printchar`)
    pub fn print_glyph<ColorT:Blend+Copy>(&self, pixels: &mut SurfacePixels, x: uint, y: uint,
                                          zoom: uint, glyph: uint, color: ColorT) { // XXX #3984
        assert!(!self.pixels[zoom].is_empty());
        for uint::range(0, 16 * zoom) |iy| {
            let row = self.pixels[zoom][glyph][iy];
            let rowcolor = color.blend(iy as int, 16 * zoom as int);
            for uint::range(0, 8 * zoom) |ix| {
                if ((row >> ix) & 1) != 0 {
                    put_pixel(pixels, x + ix, y + iy, rowcolor); // XXX incorrect lifetime
                }
            }
        }
    }

    /// Prints a character with given position and color.
    pub fn print_char<ColorT:Blend+Copy>(&self, pixels: &mut SurfacePixels, x: uint, y: uint,
                                         zoom: uint, c: char, color: ColorT) { // XXX #3984
        if !c.is_whitespace() {
            let c = c as uint;
            let glyph = if 32 <= c && c < 126 {c-32} else {0};
            self.print_glyph(pixels, x, y, zoom, glyph, color);
        }
    }

    /// Prints a string with given position, alignment and color. (C: `printstr`)
    pub fn print_string<ColorT:Blend+Copy>(&self, pixels: &mut SurfacePixels, x: uint, y: uint,
                                           zoom: uint, align: Alignment, s: &str,
                                           color: ColorT) { // XXX #3984
        let mut x = match align {
            LeftAligned  => x,
            Centered     => x - s.char_len() * (8 * zoom) / 2,
            RightAligned => x - s.char_len() * (8 * zoom),
        };
        for s.iter().advance |c| {
            let nextx = x + 8 * zoom;
            if nextx >= pixels.width { break; }
            self.print_char(pixels, x, y, zoom, c, copy color);
            x = nextx;
        }
    }
}

