// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

// XXX should really be in `format::pointer` instead...

use format::obj::{Lane, Damage, ObjData, ObjQueryOps};
use format::bms::{Bms, BmsObj, ImageRef, SoundRef};
use compat::core::iter;

/// A pointer to the object. A pointer is used to implement common operations, e.g. iterating
/// until given position, or finding the closest object with given condition. A pointer can also
/// be used like an object when it points to the valid object.
pub struct Pointer {
    /// A BMS data holding objects.
    bms: @mut ~Bms,
    /// The current position. Can be the past-the-end value.
    pos: uint
}

/// Returns true if two pointers share the common BMS data.
fn has_same_bms(lhs: &Pointer, rhs: &Pointer) -> bool {
    ::core::managed::mut_ptr_eq(lhs.bms, rhs.bms)
}

impl Eq for Pointer {
    fn eq(&self, other: &Pointer) -> bool {
        has_same_bms(self, other) && self.pos == other.pos
    }
    fn ne(&self, other: &Pointer) -> bool {
        !has_same_bms(self, other) || self.pos != other.pos
    }
}

impl Ord for Pointer {
    fn lt(&self, other: &Pointer) -> bool {
        assert!(has_same_bms(self, other));
        self.pos < other.pos
    }
    fn le(&self, other: &Pointer) -> bool {
        assert!(has_same_bms(self, other));
        self.pos <= other.pos
    }
    fn ge(&self, other: &Pointer) -> bool {
        assert!(has_same_bms(self, other));
        self.pos >= other.pos
    }
    fn gt(&self, other: &Pointer) -> bool {
        assert!(has_same_bms(self, other));
        self.pos > other.pos
    }
}


impl Clone for Pointer {
    pub fn clone(&self) -> Pointer {
        Pointer { bms: self.bms, pos: self.pos }
    }
}

impl ObjQueryOps<SoundRef,ImageRef> for Pointer {
    pub fn is_visible(self) -> bool { self.bms.objs[self.pos].is_visible() }
    pub fn is_invisible(self) -> bool { self.bms.objs[self.pos].is_invisible() }
    pub fn is_lnstart(self) -> bool { self.bms.objs[self.pos].is_lnstart() }
    pub fn is_lndone(self) -> bool { self.bms.objs[self.pos].is_lndone() }
    pub fn is_ln(self) -> bool { self.bms.objs[self.pos].is_ln() }
    pub fn is_bomb(self) -> bool { self.bms.objs[self.pos].is_bomb() }
    pub fn is_soundable(self) -> bool { self.bms.objs[self.pos].is_soundable() }
    pub fn is_gradable(self) -> bool { self.bms.objs[self.pos].is_gradable() }
    pub fn is_renderable(self) -> bool { self.bms.objs[self.pos].is_renderable() }
    pub fn is_object(self) -> bool { self.bms.objs[self.pos].is_object() }
    pub fn is_bgm(self) -> bool { self.bms.objs[self.pos].is_bgm() }
    pub fn is_setbga(self) -> bool { self.bms.objs[self.pos].is_setbga() }
    pub fn is_setbpm(self) -> bool { self.bms.objs[self.pos].is_setbpm() }
    pub fn is_stop(self) -> bool { self.bms.objs[self.pos].is_stop() }

    pub fn object_lane(self) -> Option<Lane> { self.bms.objs[self.pos].object_lane() }
    pub fn sounds(self) -> ~[SoundRef] { self.bms.objs[self.pos].sounds() }
    pub fn keydown_sound(self) -> Option<SoundRef> { self.bms.objs[self.pos].keydown_sound() }
    pub fn keyup_sound(self) -> Option<SoundRef> { self.bms.objs[self.pos].keyup_sound() }
    pub fn through_sound(self) -> Option<SoundRef> { self.bms.objs[self.pos].through_sound() }
    pub fn images(self) -> ~[ImageRef] { self.bms.objs[self.pos].images() }
    pub fn through_damage(self) -> Option<Damage> { self.bms.objs[self.pos].through_damage() }
}

pub impl Pointer {
    /// Returns the time of pointed object.
    fn time(&self) -> float { self.bms.objs[self.pos].time }

    /// Returns the associated game data of pointed object.
    fn data(&self) -> ObjData<SoundRef,ImageRef> { self.bms.objs[self.pos].data }

    /// Seeks to the first object which time is past the limit, if any.
    fn seek_until(&mut self, limit: float) {
        let bms = &*self.bms;
        let nobjs = bms.objs.len();
        while self.pos < nobjs {
            if bms.objs[self.pos].time >= limit { break; }
            self.pos += 1;
        }
    }

    /// Iterates over objects starting from the current object, until the first object which
    /// time is past the limit is reached.
    fn iter_until(&mut self, limit: float, f: &fn(&BmsObj) -> bool) -> iter::Ret {
        let bms = &*self.bms;
        let nobjs = bms.objs.len();
        while self.pos < nobjs {
            let current = &bms.objs[self.pos];
            if current.time >= limit { return iter::EarlyExit; }
            if !f(current) { return iter::EarlyExit; }
            self.pos += 1;
        }
        iter::Finished
    }

    /// Seeks to the object pointed by the other pointer.
    fn seek_to(&mut self, limit: Pointer) {
        assert!(has_same_bms(self, &limit));
        let bms = &*self.bms;
        assert!(limit.pos <= bms.objs.len());
        self.pos = limit.pos;
    }

    /// Iterates over objects starting from the current object, until the object pointed by
    /// the other pointer is reached.
    fn iter_to(&mut self, limit: Pointer, f: &fn(&BmsObj) -> bool) -> iter::Ret {
        assert!(has_same_bms(self, &limit));
        let bms = &*self.bms;
        assert!(limit.pos <= bms.objs.len());
        while self.pos < limit.pos {
            let current = &bms.objs[self.pos];
            if !f(current) { return iter::EarlyExit; }
            self.pos += 1;
        }
        iter::Finished
    }

    /// Seeks to the end of objects.
    fn seek_to_end(&mut self) {
        let bms = &*self.bms;
        self.pos = bms.objs.len();
    }

    /// Iterates over objects starting from the current object.
    fn iter_to_end(&mut self, f: &fn(&BmsObj) -> bool) -> iter::Ret {
        let bms = &*self.bms;
        let nobjs = bms.objs.len();
        while self.pos < nobjs {
            let current = &bms.objs[self.pos];
            if !f(current) { return iter::EarlyExit; }
            self.pos += 1;
        }
        iter::Finished
    }

    /// Finds the next object that satisfies given condition if any, without updating itself.
    fn find_next_of_type(&self, cond: &fn(&BmsObj) -> bool) -> Option<Pointer> {
        let bms = &*self.bms;
        let nobjs = bms.objs.len();
        let mut i = self.pos;
        while i < nobjs {
            let current = &bms.objs[i];
            if cond(current) {
                return Some(Pointer { bms: self.bms, pos: i });
            }
            i += 1;
        }
        None
    }

    /// Finds the previous object that satisfies given condition if any, without updating
    /// itself.
    fn find_previous_of_type(&self, cond: &fn(&BmsObj) -> bool) -> Option<Pointer> {
        let bms = &*self.bms;
        let mut i = self.pos;
        while i > 0 {
            i -= 1;
            let current = &bms.objs[i];
            if cond(current) {
                return Some(Pointer { bms: self.bms, pos: i });
            }
        }
        None
    }

    /// Finds the closest object from the virtual time `base` that satisfies given condition
    /// if any. `base` should lie between the pointed object and the previous object.
    /// The proximity is measured in terms of virtual time, which can differ from actual time.
    fn find_closest_of_type(&self, base: float, cond: &fn(&BmsObj) -> bool) -> Option<Pointer> {
        let previous = self.find_previous_of_type(cond);
        let next = self.find_next_of_type(cond);
        match (previous, next) {
            (None, None) => None,
            (None, Some(next)) => Some(next),
            (Some(previous), None) => Some(previous),
            (Some(previous), Some(next)) =>
                if num::abs(previous.time() - base) <
                   num::abs(next.time() - base) { Some(previous) }
                else { Some(next) }
        }
    }
}

/// Returns a pointer pointing the first object in `bms`.
pub fn Pointer(bms: @mut ~Bms) -> Pointer {
    Pointer { bms: bms, pos: 0 }
}

/// Returns a pointer pointing given object in `bms`.
pub fn pointer_with_pos(bms: @mut ~Bms, pos: uint) -> Pointer {
    Pointer { bms: bms, pos: pos }
}

