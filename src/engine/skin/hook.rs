// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

/*!
 * Skin hooks.
 *
 * There are currently three kinds of hooks available:
 *
 * - **Scalar hooks** return a text, a reference to the texture,
 *   or a scalar value that can be converted to the text,
 * - **Block hooks** calls the block (represented as a closure) zero or more times.
 *   It can optionally supply the alternative name so that the matching alternative (if any)
 *   gets called. The `parent` parameter is used for the hook delegation (see below).
 *   The block may return `false`, which requests the hook to stop the iteration.
 *
 * Normally objects implement the hooks via overriding corresponding methods
 * or delegating hooks to other objects.
 * It is normal that the same name is shared for different kinds of hooks,
 * and such technique is often used for optionally available scalars.
 *
 * Block hooks deserve some additional restrictions due to the current hook design.
 * The renderer does *not* (or rather, can't) keep the references to the parent hooks.
 * Consequently it is up to the hooks to ensure that
 * **the parent hook is called when the search on the current hook has failed**.
 * Doing this incorrectly would give bugs very hard to debug or trace, naturally.
 *
 * The hook interface provides a convenience method, `delegate`, to simplify this matter:
 * Whenever the block hook wants to give a new hook `new_hook` to the closure,
 * it should give `&parent.delegate(new_hook)` instead
 * which automatically searchs `parent` when the search on `new_hook` fails.
 * (It cannot be `&self.delegate(new_hook)` since this won't work for multiple delegations.)
 * Also, whenever the block hook wants to delegate the *current* block hook to others,
 * it should call the delegated hooks' `run_block_hook` method instead of the direct `block_hook`;
 * this ensures that the delegated hook will continue to search on the parent hooks.
 */

use std::fmt;
use std::rc::Rc;
use std::str::MaybeOwned;

use gfx::gl::Texture2D;

/// The scalar value returned by the hook.
pub enum Scalar<'a> {
    /// An owned string. Analogous to `std::str::MaybeOwned`.
    OwnedStrScalar(~str),
    /// A borrowed string slice. Analogous to `std::str::MaybeOwned`.
    BorrowedStrScalar(&'a str),
    /**
     * A reference to the texture, contanied in the `Rc` box.
     *
     * `Rc` is required because the renderer tries to delay the draw calls as long as possible,
     * so the reference to the texture may be kept indefinitely.
     * The renderer does try not to touch the `Rc` box itself until strictly required,
     * thus it requires the *reference* to the `Rc` box containing the texture.
     */
    TextureScalar(&'a Rc<Texture2D>),
    /// A signed integer.
    IntScalar(int),
    /// An unsigned integer.
    UintScalar(uint),
    /// A 32-bit floating point number.
    F32Scalar(f32),
    /// A 64-bit floating point number.
    F64Scalar(f64),
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

impl<'a> IntoScalar<'a> for &'a str {
    #[inline] fn into_scalar(self) -> Scalar<'a> { BorrowedStrScalar(self) }
}

impl<'a> AsScalar<'a> for ~str {
    #[inline] fn as_scalar(&'a self) -> Scalar<'a> { BorrowedStrScalar(self.as_slice()) }
}

impl IntoScalar<'static> for ~str {
    #[inline] fn into_scalar(self) -> Scalar<'static> { OwnedStrScalar(self) }
}

impl<'a> AsScalar<'a> for Rc<Texture2D> {
    #[inline] fn as_scalar(&'a self) -> Scalar<'a> { TextureScalar(self) }
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

scalar_conv_impls!(int  -> |v| IntScalar(v))
scalar_conv_impls!(i8   -> |v| IntScalar(v as int))
scalar_conv_impls!(i16  -> |v| IntScalar(v as int))
scalar_conv_impls!(i64  -> |v| IntScalar(v as int))
scalar_conv_impls!(i32  -> |v| IntScalar(v as int))
scalar_conv_impls!(uint -> |v| UintScalar(v))
scalar_conv_impls!(u8   -> |v| UintScalar(v as uint))
scalar_conv_impls!(u16  -> |v| UintScalar(v as uint))
scalar_conv_impls!(u64  -> |v| UintScalar(v as uint))
scalar_conv_impls!(u32  -> |v| UintScalar(v as uint))
scalar_conv_impls!(f32  -> |v| F32Scalar(v))
scalar_conv_impls!(f64  -> |v| F64Scalar(v))

impl<'a> fmt::Show for Scalar<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            OwnedStrScalar(ref s) => s.fmt(f),
            BorrowedStrScalar(s) => s.fmt(f),
            TextureScalar(tex) => {
                let tex = tex.borrow();
                write!(f.buf, "<texture {}x{}>", tex.width, tex.height)
            },
            IntScalar(v) => v.fmt(f),
            UintScalar(v) => v.fmt(f),
            F32Scalar(v) => v.fmt(f),
            F64Scalar(v) => v.fmt(f),
        }
    }
}

impl<'a> IntoMaybeOwned<'a> for Scalar<'a> {
    fn into_maybe_owned(self) -> MaybeOwned<'a> {
        match self {
            OwnedStrScalar(s) => s.into_maybe_owned(),
            BorrowedStrScalar(s) => s.into_maybe_owned(),
            scalar => scalar.to_str().into_maybe_owned(),
        }
    }
}

macro_rules! scalar_to_prim_impl(
    ($($f:ident -> $t:ty);*) => (
        impl<'a> ToPrimitive for Scalar<'a> {
            $(
                fn $f(&self) -> Option<$t> {
                    match *self {
                        OwnedStrScalar(..) | BorrowedStrScalar(..) | TextureScalar(..) => None,
                        IntScalar(v) => v.$f(),
                        UintScalar(v) => v.$f(),
                        F32Scalar(v) => v.$f(),
                        F64Scalar(v) => v.$f(),
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

/// The hook interface.
pub trait Hook {
    /// The scalar hook. The hook should return a scalar value or `None` if the search has failed.
    fn scalar_hook<'a>(&'a self, _id: &str) -> Option<Scalar<'a>> {
        None
    }

    /**
     * The block hook. The hook should call `body` with the newly generated hooks,
     * which should be either `parent` or `&parent.delegate(other_hook)`, zero or more times.
     * `Body` can return `false` to request the hook to stop the iteration.
     * The hook should return `true` when the search succeeded,
     * even when it didn't actually call the `body` at all.
     *
     * Do not call `block_hook` methods directly from other `block_hook`s;
     * this wrecks the delegation chain. Use `run_block_hook` instead.
     */
    fn block_hook(&self, _id: &str, _parent: &Hook,
                  _body: |newhook: &Hook, alt: &str| -> bool) -> bool {
        false
    }

    /// Runs the block hook from other block hooks.
    /// Note that `body` is now a reference to the closure (easier to call it in this way).
    /// Same as `block_hook` but does not wreck the delegation chain.
    fn run_block_hook(&self, id: &str, parent: &Hook, body: &|&Hook, &str| -> bool) -> bool {
        self.block_hook(id, parent, |hook,alt| (*body)(&parent.delegate(hook),alt))
    }

    /// Returns a delegated hook that tries `delegated` first and `self` later.
    fn delegate<'a>(&'a self, delegated: &'a Hook) -> Delegate<'a> {
        Delegate { base: self, delegated: delegated }
    }

    /// Returns a delegated hook that gives `value` for `id` scalar hook first and
    /// tries `self` later.
    fn add_text<'a>(&'a self, id: &'a str, value: &'a str) -> AddText<'a> {
        AddText { base: self, id: id, value: value }
    }
}

impl<'a,T:Hook> Hook for &'a T {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        (**self).scalar_hook(id)
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        (**self).block_hook(id, parent, body)
    }
}

impl<T:Hook> Hook for ~T {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        (**self).scalar_hook(id)
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        (**self).block_hook(id, parent, body)
    }
}

impl<T:Hook> Hook for Option<T> {
    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        match *self {
            Some(ref hook) => hook.block_hook(id, parent, body),
            None => false
        }
    }
}

/// A delegated hook with the order.
pub struct Delegate<'a> {
    base: &'a Hook,
    delegated: &'a Hook,
}

impl<'a> Hook for Delegate<'a> {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        self.delegated.scalar_hook(id)
            .or_else(|| self.base.scalar_hook(id))
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        self.delegated.run_block_hook(id, parent, &body) ||
            self.base.run_block_hook(id, parent, &body)
    }
}

/// A delegated hook with a single scalar hook added.
pub struct AddText<'a> {
    base: &'a Hook,
    id: &'a str,
    value: &'a str,
}

impl<'a> Hook for AddText<'a> {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        if self.id == id {
            Some(self.value.into_scalar())
        } else {
            self.base.scalar_hook(id)
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        self.base.run_block_hook(id, parent, &body)
    }
}

