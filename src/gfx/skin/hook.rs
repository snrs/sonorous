// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

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

use gfx::skin::scalar::{Scalar, IntoScalar};

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
    fn run_block_hook(&self, id: &str, parent: &Hook, body: &mut |&Hook, &str| -> bool) -> bool {
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

    fn block_hook(&self, id: &str, parent: &Hook, mut body: |&Hook, &str| -> bool) -> bool {
        self.delegated.run_block_hook(id, parent, &mut body) ||
            self.base.run_block_hook(id, parent, &mut body)
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

    fn block_hook(&self, id: &str, parent: &Hook, mut body: |&Hook, &str| -> bool) -> bool {
        self.base.run_block_hook(id, parent, &mut body)
    }
}

