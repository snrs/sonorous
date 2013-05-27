// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Graphic utilities.

use sdl::Rect;
pub use sdl::video::*;

//----------------------------------------------------------------------------------------------
// `Rect` additions

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

impl<'self,T:XyOpt> XyOpt for &'self T {
    #[inline(always)]
    fn xy_opt(&self) -> Option<(i16,i16)> { (*self).xy_opt() }
}

impl Xy for Rect {
    #[inline(always)]
    fn xy(&self) -> (i16,i16) { (self.x, self.y) }
}

impl<'self,T:Xy> Xy for &'self T {
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

impl<'self,T:WhOpt> WhOpt for &'self T {
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

impl<'self,T:Wh> Wh for &'self T {
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

impl<X:ToInt16+Copy,Y:ToInt16+Copy> XyOpt for (X,Y) {
    #[inline(always)]
    fn xy_opt(&self) -> Option<(i16,i16)> { let (x, y) = *self; Some((x.to_i16(), y.to_i16())) }
}

impl<X:ToInt16+Copy,Y:ToInt16+Copy> Xy for (X,Y) {
    #[inline(always)]
    fn xy(&self) -> (i16,i16) { let (x, y) = *self; (x.to_i16(), y.to_i16()) }
}

impl<W:ToInt16+Copy,H:ToInt16+Copy> WhOpt for (W,H) {
    #[inline(always)]
    fn wh_opt(&self) -> Option<(u16,u16)> { let (w, h) = *self; Some((w.to_u16(), h.to_u16())) }
}

impl<W:ToInt16+Copy,H:ToInt16+Copy> Wh for (W,H) {
    #[inline(always)]
    fn wh(&self) -> (u16,u16) { let (w, h) = *self; (w.to_u16(), h.to_u16()) }
}

/// Constructs an `sdl::Rect` from given point coordinates. Fills `w` and `h` fields to 0
/// as expected by the second `sdl::Rect` argument from `SDL_BlitSurface`.
#[inline(always)]
fn rect_from_xy<XY:Xy>(xy: XY) -> Rect {
    let (x, y) = xy.xy();
    Rect { x: x, y: y, w: 0, h: 0 }
}

/// Constructs an `sdl::Rect` from given point coordinates and optional rectangular area.
/// `rect_from_xywh(xy, ())` equals to `rect_from_xy(xy)`.
#[inline(always)]
fn rect_from_xywh<XY:Xy,WH:WhOpt>(xy: XY, wh: WH) -> Rect {
    let (x, y) = xy.xy();
    let (w, h) = wh.wh_opt().get_or_default((0, 0));
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
        let dstrect = dstxy.xy_opt().map(|&xy| rect_from_xywh(xy, &srcrect));
        self.blit_rect(src, Some(srcrect), dstrect)
    }

    #[inline(always)]
    fn fill_area<XY:Xy,WH:WhOpt>(&self, xy: XY, wh: WH, color: Color) -> bool {
        let rect = rect_from_xywh(xy, wh);
        self.fill_rect(Some(rect), color)
    }
}

//----------------------------------------------------------------------------------------------
// color

/// Extracts red, green, blue components from given color.
fn to_rgb(c: Color) -> (u8, u8, u8) {
    match c { RGB(r, g, b) | RGBA(r, g, b, _) => (r, g, b) }
}

/// Linear color gradient.
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
//
// Rust: `Copy` can't be inherited even when it's specified. (#3984)
pub trait Blend {
    /// Calculates the color at the position `num/denom`. (C: `blend`)
    fn blend(&self, num: int, denom: int) -> Color;
}

impl Blend for Color {
    fn blend(&self, _num: int, _denom: int) -> Color { *self }
}

impl Blend for Gradient {
    fn blend(&self, num: int, denom: int) -> Color {
        fn mix(x: u8, y: u8, num: int, denom: int) -> u8 {
            let x = x as int, y = y as int;
            (y + ((x - y) * num / denom)) as u8
        }

        let (r0, g0, b0) = to_rgb(self.zero);
        let (r1, g1, b1) = to_rgb(self.one);
        RGB(mix(r1, r0, num, denom), mix(g1, g0, num, denom), mix(b1, b0, num, denom))
    }
}

//----------------------------------------------------------------------------------------------
// surface utilities

/// Creates a new RAM-backed surface. By design, Angolmois does not use a VRAM-backed surface
/// except for the screen. (C: `newsurface`)
pub fn new_surface(w: uint, h: uint) -> Result<~Surface,~str> {
    Surface::new([SWSurface], w as int, h as int, 32, 0xff0000, 0xff00, 0xff, 0)
}

/// A proxy to `sdl::video::Surface` for the direct access to pixels. For now, it is for 32 bits
/// per pixel only.
pub struct SurfacePixels<'self> {
    fmt: *ll::SDL_PixelFormat,
    width: uint,
    height: uint,
    pitch: uint,
    pixels: &'self mut [u32]
}

/// A trait for the direct access to pixels.
pub trait SurfacePixelsUtil {
    /// Grants the direct access to pixels. Also locks the surface as needed, so you can't blit
    /// during working with pixels.
    fn with_pixels<R>(&self, f: &fn(pixels: &mut SurfacePixels) -> R) -> R;
}

impl SurfacePixelsUtil for Surface {
    fn with_pixels<R>(&self, f: &fn(pixels: &mut SurfacePixels) -> R) -> R {
        do self.with_lock |pixels| {
            let fmt = unsafe {(*self.raw).format};
            let pitch = unsafe {(*self.raw).pitch / 4 as uint};
            let pixels = unsafe {cast::transmute(pixels)};
            let mut proxy = SurfacePixels { fmt: fmt, width: self.get_width() as uint,
                                            height: self.get_height() as uint,
                                            pitch: pitch, pixels: pixels };
            f(&mut proxy)
        }
    }
}

/// Returns a pixel at given position. (C: `getpixel`)
//
// Rust: this and subsequent `*_pixel` functions are required due to the incorrect lifetime
//       inference in 0.6 borrowck algorithm. This problem has been fixed in 0.7 with a new
//       flow-sensitive borrowck, but for now we keep them.
pub fn get_pixel(surface: &SurfacePixels, x: uint, y: uint) -> Color {
    Color::from_mapped(surface.pixels[x + y * surface.pitch], surface.fmt)
}

/// Sets a pixel to given position. (C: `putpixel`)
pub fn put_pixel(surface: &mut SurfacePixels, x: uint, y: uint, c: Color) {
    surface.pixels[x + y * surface.pitch] = c.to_mapped(surface.fmt);
}

/// Sets or blends (if `c` is `RGBA`) a pixel to given position. (C: `putblendedpixel`)
pub fn put_blended_pixel(surface: &mut SurfacePixels, x: uint, y: uint, c: Color) {
    match c {
        RGB(*) => put_pixel(surface, x, y, c),
        RGBA(r,g,b,a) => match get_pixel(surface, x, y) {
            RGB(r2,g2,b2) | RGBA(r2,g2,b2,_) => {
                let grad = Gradient { zero: RGB(r,g,b), one: RGB(r2,g2,b2) };
                put_pixel(surface, x, y, grad.blend(a as int, 255));
            }
        }
    }
}

impl<'self> SurfacePixels<'self> {
    /// Returns a pixel at given position. (C: `getpixel`)
    pub fn get_pixel(&self, x: uint, y: uint) -> Color { get_pixel(self, x, y) }

    /// Sets a pixel to given position. (C: `putpixel`)
    pub fn put_pixel(&mut self, x: uint, y: uint, c: Color) { put_pixel(self, x, y, c) }

    /// Sets or blends (if `c` is `RGBA`) a pixel to given position. (C: `putblendedpixel`)
    pub fn put_blended_pixel(&mut self, x: uint, y: uint, c: Color) {
        put_blended_pixel(self, x, y, c)
    }
}

/// A scaling factor for the calculation of convolution kernel.
static FP_SHIFT1: int = 11;
/// A scaling factor for the summation of weighted pixels.
static FP_SHIFT2: int = 16;

/// Returns `2^FP_SHIFT * W(x/y)` where `W(x)` is a bicubic kernel function. `y` should be
/// positive. (C: `bicubic_kernel`)
fn bicubic_kernel(x: int, y: int) -> int {
    let x = num::abs(x);
    if x < y {
        // W(x/y) = 1/2 (2 - 5(x/y)^2 + 3(x/y)^3)
        ((2*y*y - 5*x*x + 3*x*x/y*x) << (FP_SHIFT1-1)) / (y*y)
    } else if x < y * 2 {
        // W(x/y) = 1/2 (4 - 8(x/y) + 5(x/y)^2 - (x/y)^3)
        ((4*y*y - 8*x*y + 5*x*x - x*x/y*x) << (FP_SHIFT1-1)) / (y*y)
    } else {
        0
    }
}

/**
 * Performs the bicubic interpolation. `dest` should be initialized to the target dimension
 * before calling this function. This function should be used only for the upscaling; it can do
 * the downscaling somehow but technically its result is incorrect. (C: `bicubic_interpolation`)
 *
 * Well, this function is one of the ugliest functions in Angolmois, especially since it is
 * a complicated (in terms of code complexity) and still poor (we normally use the matrix form
 * instead) implementation of the algorithm. In fact, the original version of `bicubic_kernel`
 * had even a slightly incorrect curve (`1/2 - x^2 + 1/2 x^3` instead of `1 - 5/2 x^2 +
 * 3/2 x^3`). This function still remains here only because we don't use OpenGL...
 */
pub fn bicubic_interpolation(src: &SurfacePixels, dest: &mut SurfacePixels) {
    let w = dest.width as int - 1;
    let h = dest.height as int - 1;
    let ww = src.width as int - 1;
    let hh = src.height as int - 1;

    let mut dx = 0, x = 0;
    for int::range(0, w + 1) |i| {
        let mut dy = 0, y = 0;
        for int::range(0, h + 1) |j| {
            let mut r = 0, g = 0, b = 0;
            let a0 = [bicubic_kernel((x-1) * w - i * ww, w),
                      bicubic_kernel( x    * w - i * ww, w),
                      bicubic_kernel((x+1) * w - i * ww, w),
                      bicubic_kernel((x+2) * w - i * ww, w)];
            let a1 = [bicubic_kernel((y-1) * h - j * hh, h),
                      bicubic_kernel( y    * h - j * hh, h),
                      bicubic_kernel((y+1) * h - j * hh, h),
                      bicubic_kernel((y+2) * h - j * hh, h)];
            for int::range(0, 4) |k0| {
                for int::range(0, 4) |k1| {
                    let xx = x + k0 - 1, yy = y + k1 - 1;
                    if 0 <= xx && xx <= ww && 0 <= yy && yy <= hh {
                        let (r2,g2,b2) = to_rgb(src.get_pixel(xx as uint, yy as uint));
                        let d = (a0[k0] * a1[k1]) >> (FP_SHIFT1*2 - FP_SHIFT2);
                        r += r2 as int * d;
                        g += g2 as int * d;
                        b += b2 as int * d;
                    }
                }
            }

            let r = ::util::core::cmp::clamp(0, r >> FP_SHIFT2, 255) as u8;
            let g = ::util::core::cmp::clamp(0, g >> FP_SHIFT2, 255) as u8;
            let b = ::util::core::cmp::clamp(0, b >> FP_SHIFT2, 255) as u8;
            put_pixel(dest, i as uint, j as uint, RGB(r, g, b)); // XXX incorrect lifetime

            dy += hh;
            if dy > h {
                y += 1;
                dy -= h;
            }
        }

        dx += ww;
        if dx > w {
            x += 1;
            dx -= w;
        }
    }
}

//----------------------------------------------------------------------------------------------

