// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! In-skin scalar value.

use std::fmt;
use std::rc::Rc;
use std::str::CowString;

use gfx::color::{Color, RGB, RGBA};
use gfx::ratio_num::RatioNum;
use gfx::gl::Texture2D;

/// The image reference in the scalar value.
pub enum ImageSource<'a> {
    /**
     * A reference to the texture, contained in the `Rc` box.
     *
     * `Rc` is required because the renderer tries to delay the draw calls as long as possible,
     * so the reference to the texture may be kept indefinitely.
     * The renderer does try not to touch the `Rc` box itself until strictly required,
     * thus it requires the *reference* to the `Rc` box containing the texture.
     */
    Texture(&'a Rc<Texture2D>),
    /// A reference to the image file.
    /// The renderer is free to cache the resulting texture per the path.
    Path(Path),
}

impl<'a> Clone for ImageSource<'a> {
    fn clone(&self) -> ImageSource<'a> {
        match *self {
            ImageSource::Texture(tex) => ImageSource::Texture(tex),
            ImageSource::Path(ref path) => ImageSource::Path(path.clone()),
        }
    }
}

/// The clipping rectangle.
#[deriving(Clone, Show)]
pub struct ImageClip {
    pub x: RatioNum<f32>,
    pub y: RatioNum<f32>,
    pub w: RatioNum<f32>,
    pub h: RatioNum<f32>,
}

impl ImageClip {
    /// Returns a new, full-screen clipping rectangle.
    pub fn new() -> ImageClip {
        ImageClip { x: RatioNum::zero(), w: RatioNum::one(),
                    y: RatioNum::zero(), h: RatioNum::one() }
    }

    /// Substitutes the current base rectangle with another clipping rectangle:
    ///
    /// ```notrust
    /// +---------------+
    /// | +-------+     |
    /// | |       | <-------- self
    /// | +-------+     | <-- other
    /// |               |
    /// +---------------+
    /// ```
    pub fn subst(&self, other: &ImageClip) -> ImageClip {
        let x = other.x + self.x.subst(&other.w);
        let y = other.y + self.y.subst(&other.h);
        let w = self.w.subst(&other.w);
        let h = self.h.subst(&other.h);
        ImageClip { x: x, y: y, w: w, h: h }
    }
}

/// The scalar value.
pub enum Scalar<'a> {
    /// An owned string. Analogous to `std::str::CowString`.
    OwnedStr(String),
    /// A borrowed string slice. Analogous to `std::str::CowString`.
    BorrowedStr(&'a str),
    /// An image reference and the clipping rectangle in pixels.
    Image(ImageSource<'a>, ImageClip),
    /// A signed integer.
    Int(int),
    /// An unsigned integer.
    Uint(uint),
    /// A 32-bit floating point number.
    F32(f32),
    /// A 64-bit floating point number.
    F64(f64),
    /// A color.
    Color(Color),
}

impl<'a> Scalar<'a> {
    /// Extracts the string slice if any.
    pub fn as_slice(&'a self) -> Option<&'a str> {
        match *self {
            Scalar::OwnedStr(ref s) => Some(s[]),
            Scalar::BorrowedStr(s) => Some(s),
            _ => None,
        }
    }

    /// Extracts the image reference if any.
    pub fn as_image_source(&'a self) -> Option<&'a ImageSource<'a>> {
        match *self {
            Scalar::Image(ref src, _clip) => Some(src),
            _ => None,
        }
    }
}

/// A trait for `as_scalar` convenience method.
pub trait AsScalar<'a> {
    /// Converts the value to the scalar value with no copying.
    fn as_scalar(&'a self) -> Scalar<'a>;
}

/// A trait for `into_scalar` convenience method.
pub trait IntoScalar<'a> {
    /// Converts the value to the scalar value while giving the ownership up.
    fn into_scalar(self) -> Scalar<'a>;
}

impl<'a> IntoScalar<'a> for Scalar<'a> {
    #[inline] fn into_scalar(self) -> Scalar<'a> { self }
}

impl<'a> IntoScalar<'a> for &'a str {
    #[inline] fn into_scalar(self) -> Scalar<'a> { Scalar::BorrowedStr(self) }
}

impl<'a> AsScalar<'a> for String {
    #[inline] fn as_scalar(&'a self) -> Scalar<'a> { Scalar::BorrowedStr(self[]) }
}

impl IntoScalar<'static> for String {
    #[inline] fn into_scalar(self) -> Scalar<'static> { Scalar::OwnedStr(self) }
}

impl<'a> AsScalar<'a> for Rc<Texture2D> {
    #[inline]
    fn as_scalar(&'a self) -> Scalar<'a> {
        Scalar::Image(ImageSource::Texture(self), ImageClip::new())
    }
}

macro_rules! scalar_conv_impls(
    ($t:ty -> |$v:ident| $e:expr) => (
        impl<'a> AsScalar<'a> for $t {
            #[inline] fn as_scalar(&'a self) -> Scalar<'a> { let $v: $t = *self; $e }
        }
        impl IntoScalar<'static> for $t {
            #[inline] fn into_scalar(self) -> Scalar<'static> { let $v: $t = self; $e }
        }
    )
)

scalar_conv_impls!(int   -> |v| Scalar::Int(v))
scalar_conv_impls!(i8    -> |v| Scalar::Int(v as int))
scalar_conv_impls!(i16   -> |v| Scalar::Int(v as int))
scalar_conv_impls!(i64   -> |v| Scalar::Int(v as int))
scalar_conv_impls!(i32   -> |v| Scalar::Int(v as int))
scalar_conv_impls!(uint  -> |v| Scalar::Uint(v))
scalar_conv_impls!(u8    -> |v| Scalar::Uint(v as uint))
scalar_conv_impls!(u16   -> |v| Scalar::Uint(v as uint))
scalar_conv_impls!(u64   -> |v| Scalar::Uint(v as uint))
scalar_conv_impls!(u32   -> |v| Scalar::Uint(v as uint))
scalar_conv_impls!(f32   -> |v| Scalar::F32(v))
scalar_conv_impls!(f64   -> |v| Scalar::F64(v))
scalar_conv_impls!(Color -> |v| Scalar::Color(v))

impl<'a> fmt::Show for ImageSource<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ImageSource::Texture(tex) => {
                let tex = tex.deref();
                write!(f, "<texture {}x{}>", tex.width, tex.height)
            },
            ImageSource::Path(ref path) => write!(f, "<external {}>", path.display()),
        }
    }
}

impl<'a> fmt::Show for Scalar<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Scalar::OwnedStr(ref s) => s.fmt(f),
            Scalar::BorrowedStr(s) => s.fmt(f),
            Scalar::Image(ref src, ref clip) => write!(f, "Scalar::Image({}, {})", *src, *clip),
            Scalar::Int(v) => v.fmt(f),
            Scalar::Uint(v) => v.fmt(f),
            Scalar::F32(v) => v.fmt(f),
            Scalar::F64(v) => v.fmt(f),
            Scalar::Color(RGB(r, g, b)) => write!(f, "#{:02x}{:02x}{:02x}", r, g, b),
            Scalar::Color(RGBA(r, g, b, a)) => write!(f, "#{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
        }
    }
}

impl<'a> IntoCow<'a, String, str> for Scalar<'a> {
    fn into_cow(self) -> CowString<'a> {
        match self {
            Scalar::OwnedStr(s) => s.into_cow(),
            Scalar::BorrowedStr(s) => s.into_cow(),
            scalar => scalar.to_string().into_cow(),
        }
    }
}

macro_rules! scalar_to_prim_impl(
    ($($f:ident -> $t:ty);*) => (
        impl<'a> ToPrimitive for Scalar<'a> {
            $(
                fn $f(&self) -> Option<$t> {
                    match *self {
                        Scalar::OwnedStr(..) => None,
                        Scalar::BorrowedStr(..) => None,
                        Scalar::Image(..) => None,
                        Scalar::Int(v) => v.$f(),
                        Scalar::Uint(v) => v.$f(),
                        Scalar::F32(v) => v.$f(),
                        Scalar::F64(v) => v.$f(),
                        Scalar::Color(..) => None,
                    }
                }
            )*
        }
    )
)

scalar_to_prim_impl!(
    to_int  -> int;  to_i8 -> i8; to_i16 -> i16; to_i32 -> i32; to_i64 -> i64;
    to_uint -> uint; to_u8 -> u8; to_u16 -> u16; to_u32 -> u32; to_u64 -> u64;
                                                 to_f32 -> f32; to_f64 -> f64
)

impl<'a> Clone for Scalar<'a> {
    fn clone(&self) -> Scalar<'a> {
        match *self {
            Scalar::OwnedStr(ref s) => Scalar::OwnedStr(s.to_string()),
            Scalar::BorrowedStr(s) => Scalar::BorrowedStr(s),
            Scalar::Image(ref src, ref clip) => Scalar::Image(src.clone(), clip.clone()),
            Scalar::Int(v) => Scalar::Int(v),
            Scalar::Uint(v) => Scalar::Uint(v),
            Scalar::F32(v) => Scalar::F32(v),
            Scalar::F64(v) => Scalar::F64(v),
            Scalar::Color(v) => Scalar::Color(v),
        }
    }
}

