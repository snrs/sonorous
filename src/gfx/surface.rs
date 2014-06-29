// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Utilities for SDL surfaces.

use gfx::color::{Color, Gradient, RGB, RGBA, Blend};

pub use sdl::Rect;
pub use sdl::video::Surface;
use sdl::video::ll::SDL_PixelFormat;

/// A trait that can be translated to point coordinates (`x` and `y` fields in `sdl::Rect`,
/// hence the name). Also contains `()`.
pub trait XyOpt {
    /// Returns point coordinates if any.
    fn xy_opt(&self) -> Option<(i16,i16)>;
}

/// Same as `XyOpt` but does not contain `()`.
pub trait Xy: XyOpt {
    /// Returns point coordinates.
    fn xy(&self) -> (i16,i16);
}

/// A trait that can be translated to a rectangular area (`w` and `h` fields in `sdl::Rect`,
/// hence the name). Also contains `()`.
pub trait WhOpt {
    /// Returns a rectangular area if any.
    fn wh_opt(&self) -> Option<(u16,u16)>;
}

/// Same as `WhOpt` but does not contain `()`.
pub trait Wh {
    /// Returns a rectangular area.
    fn wh(&self) -> (u16,u16);
}

impl XyOpt for () {
    #[inline(always)]
    fn xy_opt(&self) -> Option<(i16,i16)> { None }
}

// Rust: we can't define these with `impl<T:Xy> XyOpt for T` due to the ambiguity.
impl XyOpt for Rect {
    #[inline(always)]
    fn xy_opt(&self) -> Option<(i16,i16)> { Some((self.x, self.y)) }
}

impl<'r,T:XyOpt> XyOpt for &'r T {
    #[inline(always)]
    fn xy_opt(&self) -> Option<(i16,i16)> { (*self).xy_opt() }
}

impl Xy for Rect {
    #[inline(always)]
    fn xy(&self) -> (i16,i16) { (self.x, self.y) }
}

impl<'r,T:Xy> Xy for &'r T {
    #[inline(always)]
    fn xy(&self) -> (i16,i16) { (*self).xy() }
}

impl WhOpt for () {
    #[inline(always)]
    fn wh_opt(&self) -> Option<(u16,u16)> { None }
}

impl WhOpt for Rect {
    #[inline(always)]
    fn wh_opt(&self) -> Option<(u16,u16)> { Some((self.w, self.h)) }
}

impl WhOpt for Surface {
    #[inline(always)]
    fn wh_opt(&self) -> Option<(u16,u16)> { Some(self.get_size()) }
}

impl<'r,T:WhOpt> WhOpt for &'r T {
    #[inline(always)]
    fn wh_opt(&self) -> Option<(u16,u16)> { (*self).wh_opt() }
}

impl Wh for Rect {
    #[inline(always)]
    fn wh(&self) -> (u16,u16) { (self.w, self.h) }
}

impl Wh for Surface {
    #[inline(always)]
    fn wh(&self) -> (u16,u16) { self.get_size() }
}

impl<'r,T:Wh> Wh for &'r T {
    #[inline(always)]
    fn wh(&self) -> (u16,u16) { (*self).wh() }
}

/// A helper trait for defining every implementations for types `(T1,T2)` where `T1` and `T2` is
/// convertible to an integer.
trait ToInt16 {
    /// Converts to `i16`.
    fn to_i16(&self) -> i16;
    /// Converts to `u16`.
    fn to_u16(&self) -> u16;
}

macro_rules! define_ToInt16(
    ($t:ty) => (impl ToInt16 for $t {
                    #[inline(always)]
                    fn to_i16(&self) -> i16 { *self as i16 }
                    #[inline(always)]
                    fn to_u16(&self) -> u16 { *self as u16 }
                })
)

define_ToInt16!(int)
define_ToInt16!(uint)
define_ToInt16!(i8)
define_ToInt16!(i16)
define_ToInt16!(i32)
define_ToInt16!(i64)
define_ToInt16!(u8)
define_ToInt16!(u16)
define_ToInt16!(u32)
define_ToInt16!(u64)

impl<X:ToInt16+Clone,Y:ToInt16+Clone> XyOpt for (X,Y) {
    #[inline(always)]
    fn xy_opt(&self) -> Option<(i16,i16)> {
        let (x, y) = self.clone();
        Some((x.to_i16(), y.to_i16()))
    }
}

impl<X:ToInt16+Clone,Y:ToInt16+Clone> Xy for (X,Y) {
    #[inline(always)]
    fn xy(&self) -> (i16,i16) {
        let (x, y) = self.clone();
        (x.to_i16(), y.to_i16())
    }
}

impl<W:ToInt16+Clone,H:ToInt16+Clone> WhOpt for (W,H) {
    #[inline(always)]
    fn wh_opt(&self) -> Option<(u16,u16)> {
        let (w, h) = self.clone();
        Some((w.to_u16(), h.to_u16()))
    }
}

impl<W:ToInt16+Clone,H:ToInt16+Clone> Wh for (W,H) {
    #[inline(always)]
    fn wh(&self) -> (u16,u16) {
        let (w, h) = self.clone();
        (w.to_u16(), h.to_u16())
    }
}

/// Constructs an `sdl::Rect` from given point coordinates. Fills `w` and `h` fields to 0
/// as expected by the second `sdl::Rect` argument from `SDL_BlitSurface`.
#[inline(always)]
pub fn rect_from_xy<XY:Xy>(xy: XY) -> Rect {
    let (x, y) = xy.xy();
    Rect { x: x, y: y, w: 0, h: 0 }
}

/// Constructs an `sdl::Rect` from given point coordinates and optional rectangular area.
/// `rect_from_xywh(xy, ())` equals to `rect_from_xy(xy)`.
#[inline(always)]
pub fn rect_from_xywh<XY:Xy,WH:WhOpt>(xy: XY, wh: WH) -> Rect {
    let (x, y) = xy.xy();
    let (w, h) = wh.wh_opt().unwrap_or((0, 0));
    Rect { x: x, y: y, w: w, h: h }
}

/// Additions to `sdl::video::Surface`. They replace their `_rect` suffixed counterparts,
/// which are generally annoying to work with.
pub trait SurfaceAreaUtil {
    /// An alternative interface to `set_clip_rect`.
    fn set_clip_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH);
    /// An alternative interface to `blit_rect`.
    fn blit_area<SrcXY:Xy,DstXY:XyOpt,WH:WhOpt>(&self, src: &Surface,
                                                srcxy: SrcXY, dstxy: DstXY, wh: WH) -> bool;
    /// An alternative interface to `fill_rect`.
    fn fill_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH, color: Color) -> bool;
}

impl SurfaceAreaUtil for Surface {
    #[inline(always)]
    fn set_clip_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH) {
        let rect = rect_from_xywh(xy, wh);
        self.set_clip_rect(&rect)
    }

    #[inline(always)]
    fn blit_area<SrcXY:Xy,DstXY:XyOpt,WH:WhOpt>(&self, src: &Surface,
                                                srcxy: SrcXY, dstxy: DstXY, wh: WH) -> bool {
        let srcrect = rect_from_xywh(srcxy, wh);
        let dstrect = dstxy.xy_opt().map(|xy| rect_from_xywh(xy, &srcrect));
        self.blit_rect(src, Some(srcrect), dstrect)
    }

    #[inline(always)]
    fn fill_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH, color: Color) -> bool {
        let rect = rect_from_xywh(xy, wh);
        self.fill_rect(Some(rect), color)
    }
}

/// A proxy to `sdl::video::Surface` for the direct access to pixels. For now, it is for 32 bits
/// per pixel only.
pub struct SurfacePixels<'r> {
    fmt: *mut SDL_PixelFormat,
    width: uint,
    height: uint,
    pitch: uint,
    pixels: &'r mut [u32]
}

/// A trait for the direct access to pixels.
pub trait SurfacePixelsUtil {
    /// Grants the direct access to pixels. Also locks the surface as needed, so you can't blit
    /// during working with pixels.
    fn with_pixels<R>(&self, f: |pixels: &mut SurfacePixels| -> R) -> R;
}

impl SurfacePixelsUtil for Surface {
    fn with_pixels<R>(&self, f: |pixels: &mut SurfacePixels| -> R) -> R {
        self.with_lock(|pixels| {
            let fmt = unsafe {(*self.raw).format};
            let pitch = unsafe {((*self.raw).pitch / 4) as uint};
            let pixels = unsafe {::std::mem::transmute(pixels)};
            let mut proxy = SurfacePixels { fmt: fmt, width: self.get_width() as uint,
                                            height: self.get_height() as uint,
                                            pitch: pitch, pixels: pixels };
            f(&mut proxy)
        })
    }
}

impl<'r> SurfacePixels<'r> {
    /// Returns a pixel at given position. (C: `getpixel`)
    pub fn get_pixel(&self, x: uint, y: uint) -> Color {
        Color::from_mapped(self.pixels[x + y * self.pitch], self.fmt as *const _)
    }

    /// Returns a pixel at given position, only when the position is valid.
    pub fn get_pixel_checked(&self, x: uint, y: uint) -> Option<Color> {
        if x < self.width && y < self.height {
            Some(self.get_pixel(x, y))
        } else {
            None
        }
    }

    /// Sets a pixel to given position. (C: `putpixel`)
    pub fn put_pixel(&mut self, x: uint, y: uint, c: Color) {
        self.pixels[x + y * self.pitch] = c.to_mapped(self.fmt as *const _);
    }

    /// Sets a pixel to given position, only when the position is valid.
    /// Returns true when the pixel has really been set.
    pub fn put_pixel_checked(&mut self, x: uint, y: uint, c: Color) -> bool {
        if x < self.width && y < self.height {
            self.put_pixel(x, y, c);
            true
        } else {
            false
        }
    }

    /// Sets or blends (if `c` is `RGBA`) a pixel to given position. (C: `putblendedpixel`)
    pub fn put_blended_pixel(&mut self, x: uint, y: uint, c: Color) {
        match c {
            RGB(..) => self.put_pixel(x, y, c),
            RGBA(r,g,b,a) => match self.get_pixel(x, y) {
                RGB(r2,g2,b2) | RGBA(r2,g2,b2,_) => {
                    let grad = Gradient { zero: RGB(r,g,b), one: RGB(r2,g2,b2) };
                    self.put_pixel(x, y, grad.blend(a as int, 255));
                }
            }
        }
    }

    /// Sets or blends (if `c` is `RGBA`) a pixel to given position,
    /// only when the position is valid.
    /// Returns true when the pixel has really been set.
    pub fn put_blended_pixel_checked(&mut self, x: uint, y: uint, c: Color) -> bool {
        if x < self.width && y < self.height {
            self.put_blended_pixel(x, y, c);
            true
        } else {
            false
        }
    }

}

