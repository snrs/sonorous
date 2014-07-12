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
use std::str::MaybeOwned;

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
    TextureSource(&'a Rc<Texture2D>),
    /// A reference to the image file.
    /// The renderer is free to cache the resulting texture per the path.
    PathSource(Path),
}

impl<'a> Clone for ImageSource<'a> {
    fn clone(&self) -> ImageSource<'a> {
        match *self {
            TextureSource(tex) => TextureSource(tex),
            PathSource(ref path) => PathSource(path.clone()),
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
    /// An owned string. Analogous to `std::str::MaybeOwned`.
    OwnedStrScalar(String),
    /// A borrowed string slice. Analogous to `std::str::MaybeOwned`.
    BorrowedStrScalar(&'a str),
    /// An image reference and the clipping rectangle in pixels.
    ImageScalar(ImageSource<'a>, ImageClip),
    /// A signed integer.
    IntScalar(int),
    /// An unsigned integer.
    UintScalar(uint),
    /// A 32-bit floating point number.
    F32Scalar(f32),
    /// A 64-bit floating point number.
    F64Scalar(f64),
    /// A color.
    ColorScalar(Color),
}

impl<'a> Scalar<'a> {
    /// Extracts the string slice if any.
    pub fn as_slice(&'a self) -> Option<&'a str> {
        match *self {
            OwnedStrScalar(ref s) => Some(s.as_slice()),
            BorrowedStrScalar(s) => Some(s),
            _ => None,
        }
    }

    /// Extracts the image reference if any.
    pub fn as_image_source(&'a self) -> Option<&'a ImageSource<'a>> {
        match *self {
            ImageScalar(ref src, _clip) => Some(src),
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
    #[inline] fn into_scalar(self) -> Scalar<'a> { BorrowedStrScalar(self) }
}

impl<'a> AsScalar<'a> for String {
    #[inline] fn as_scalar(&'a self) -> Scalar<'a> { BorrowedStrScalar(self.as_slice()) }
}

impl IntoScalar<'static> for String {
    #[inline] fn into_scalar(self) -> Scalar<'static> { OwnedStrScalar(self) }
}

impl<'a> AsScalar<'a> for Rc<Texture2D> {
    #[inline]
    fn as_scalar(&'a self) -> Scalar<'a> {
        ImageScalar(TextureSource(self), ImageClip::new())
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

scalar_conv_impls!(int   -> |v| IntScalar(v))
scalar_conv_impls!(i8    -> |v| IntScalar(v as int))
scalar_conv_impls!(i16   -> |v| IntScalar(v as int))
scalar_conv_impls!(i64   -> |v| IntScalar(v as int))
scalar_conv_impls!(i32   -> |v| IntScalar(v as int))
scalar_conv_impls!(uint  -> |v| UintScalar(v))
scalar_conv_impls!(u8    -> |v| UintScalar(v as uint))
scalar_conv_impls!(u16   -> |v| UintScalar(v as uint))
scalar_conv_impls!(u64   -> |v| UintScalar(v as uint))
scalar_conv_impls!(u32   -> |v| UintScalar(v as uint))
scalar_conv_impls!(f32   -> |v| F32Scalar(v))
scalar_conv_impls!(f64   -> |v| F64Scalar(v))
scalar_conv_impls!(Color -> |v| ColorScalar(v))

impl<'a> fmt::Show for ImageSource<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TextureSource(tex) => {
                let tex = tex.deref();
                write!(f, "<texture {}x{}>", tex.width, tex.height)
            },
            PathSource(ref path) => write!(f, "<external {}>", path.display()),
        }
    }
}

impl<'a> fmt::Show for Scalar<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OwnedStrScalar(ref s) => s.fmt(f),
            BorrowedStrScalar(s) => s.fmt(f),
            ImageScalar(ref src, ref clip) => write!(f, "ImageScalar({}, {})", *src, *clip),
            IntScalar(v) => v.fmt(f),
            UintScalar(v) => v.fmt(f),
            F32Scalar(v) => v.fmt(f),
            F64Scalar(v) => v.fmt(f),
            ColorScalar(RGB(r, g, b)) => write!(f, "#{:02x}{:02x}{:02x}", r, g, b),
            ColorScalar(RGBA(r, g, b, a)) => write!(f, "#{:02x}{:02x}{:02x}{:02x}", r, g, b, a),
        }
    }
}

impl<'a> IntoMaybeOwned<'a> for Scalar<'a> {
    fn into_maybe_owned(self) -> MaybeOwned<'a> {
        match self {
            OwnedStrScalar(s) => s.into_maybe_owned(),
            BorrowedStrScalar(s) => s.into_maybe_owned(),
            scalar => scalar.to_string().into_maybe_owned(),
        }
    }
}

macro_rules! scalar_to_prim_impl(
    ($($f:ident -> $t:ty);*) => (
        impl<'a> ToPrimitive for Scalar<'a> {
            $(
                fn $f(&self) -> Option<$t> {
                    match *self {
                        OwnedStrScalar(..) => None,
                        BorrowedStrScalar(..) => None,
                        ImageScalar(..) => None,
                        IntScalar(v) => v.$f(),
                        UintScalar(v) => v.$f(),
                        F32Scalar(v) => v.$f(),
                        F64Scalar(v) => v.$f(),
                        ColorScalar(..) => None,
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
            OwnedStrScalar(ref s) => OwnedStrScalar(s.to_string()),
            BorrowedStrScalar(s) => BorrowedStrScalar(s),
            ImageScalar(ref src, ref clip) => ImageScalar(src.clone(), clip.clone()),
            IntScalar(v) => IntScalar(v),
            UintScalar(v) => UintScalar(v),
            F32Scalar(v) => F32Scalar(v),
            F64Scalar(v) => F64Scalar(v),
            ColorScalar(v) => ColorScalar(v),
        }
    }
}

