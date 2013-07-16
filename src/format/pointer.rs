// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Pointer interface, which provides a reference to the position or object in the timeline.

use std::{num, managed};
use format::obj::{ObjData, ToObjData, Obj};
use format::timeline::Timeline;

/// A pointer to the object. A pointer is used to implement common operations, e.g. iterating
/// until given position, or finding the closest object with given condition. A pointer can also
/// be used like an object when it points to the valid object.
pub struct Pointer<SoundRef,ImageRef> {
    /// A timeline holding objects.
    timeline: @Timeline<SoundRef,ImageRef>,
    /// The current position. Can be the past-the-end value.
    pos: uint
}

/// Returns true if two pointers share the common timeline.
fn has_same_timeline<S,I>(lhs: &Pointer<S,I>, rhs: &Pointer<S,I>) -> bool {
    managed::ptr_eq(lhs.timeline, rhs.timeline)
}

impl<S,I> Eq for Pointer<S,I> {
    fn eq(&self, other: &Pointer<S,I>) -> bool {
        has_same_timeline(self, other) && self.pos == other.pos
    }
    fn ne(&self, other: &Pointer<S,I>) -> bool {
        !has_same_timeline(self, other) || self.pos != other.pos
    }
}

impl<S,I> Ord for Pointer<S,I> {
    fn lt(&self, other: &Pointer<S,I>) -> bool {
        assert!(has_same_timeline(self, other));
        self.pos < other.pos
    }
    fn le(&self, other: &Pointer<S,I>) -> bool {
        assert!(has_same_timeline(self, other));
        self.pos <= other.pos
    }
    fn ge(&self, other: &Pointer<S,I>) -> bool {
        assert!(has_same_timeline(self, other));
        self.pos >= other.pos
    }
    fn gt(&self, other: &Pointer<S,I>) -> bool {
        assert!(has_same_timeline(self, other));
        self.pos > other.pos
    }
}

impl<S,I> Clone for Pointer<S,I> {
    pub fn clone(&self) -> Pointer<S,I> {
        Pointer { timeline: self.timeline, pos: self.pos }
    }
}

impl<S:Copy,I:Copy> ToObjData<S,I> for Pointer<S,I> {
    pub fn to_obj_data(&self) -> ObjData<S,I> { copy self.timeline.objs[self.pos].data }
}

impl<S:Copy,I:Copy> Pointer<S,I> {
    /// Returns the time of pointed object.
    pub fn time(&self) -> float { self.timeline.objs[self.pos].time }

    /// Returns the associated game data of pointed object.
    pub fn data(&self) -> ObjData<S,I> { copy self.timeline.objs[self.pos].data }

    /// Seeks to the first object which time is past the limit, if any.
    pub fn seek_until(&mut self, limit: float) {
        let nobjs = self.timeline.objs.len();
        while self.pos < nobjs {
            if self.timeline.objs[self.pos].time >= limit { break; }
            self.pos += 1;
        }
    }

    /// Iterates over objects starting from the current object, until the first object which
    /// time is past the limit is reached.
    pub fn iter_until(&mut self, limit: float, f: &fn(&Obj<S,I>) -> bool) -> bool {
        let nobjs = self.timeline.objs.len();
        while self.pos < nobjs {
            let current = &self.timeline.objs[self.pos];
            if current.time >= limit { return false; }
            if !f(current) { return false; }
            self.pos += 1;
        }
        true
    }

    /// Seeks to the object pointed by the other pointer.
    pub fn seek_to(&mut self, limit: Pointer<S,I>) {
        assert!(has_same_timeline(self, &limit));
        assert!(limit.pos <= self.timeline.objs.len());
        self.pos = limit.pos;
    }

    /// Iterates over objects starting from the current object, until the object pointed by
    /// the other pointer is reached.
    pub fn iter_to(&mut self, limit: Pointer<S,I>, f: &fn(&Obj<S,I>) -> bool) -> bool {
        assert!(has_same_timeline(self, &limit));
        assert!(limit.pos <= self.timeline.objs.len());
        while self.pos < limit.pos {
            let current = &self.timeline.objs[self.pos];
            if !f(current) { return false; }
            self.pos += 1;
        }
        true
    }

    /// Seeks to the end of objects.
    pub fn seek_to_end(&mut self) {
        self.pos = self.timeline.objs.len();
    }

    /// Iterates over objects starting from the current object.
    pub fn iter_to_end(&mut self, f: &fn(&Obj<S,I>) -> bool) -> bool {
        let nobjs = self.timeline.objs.len();
        while self.pos < nobjs {
            let current = &self.timeline.objs[self.pos];
            if !f(current) { return false; }
            self.pos += 1;
        }
        true
    }

    /// Finds the next object that satisfies given condition if any, without updating itself.
    pub fn find_next_of_type(&self, cond: &fn(&Obj<S,I>) -> bool) -> Option<Pointer<S,I>> {
        let nobjs = self.timeline.objs.len();
        let mut i = self.pos;
        while i < nobjs {
            let current = &self.timeline.objs[i];
            if cond(current) {
                return Some(Pointer { timeline: self.timeline, pos: i });
            }
            i += 1;
        }
        None
    }

    /// Finds the previous object that satisfies given condition if any, without updating itself.
    pub fn find_previous_of_type(&self, cond: &fn(&Obj<S,I>) -> bool) -> Option<Pointer<S,I>> {
        let mut i = self.pos;
        while i > 0 {
            i -= 1;
            let current = &self.timeline.objs[i];
            if cond(current) {
                return Some(Pointer { timeline: self.timeline, pos: i });
            }
        }
        None
    }

    /// Finds the closest object from the virtual time `base` that satisfies given condition
    /// if any. `base` should lie between the pointed object and the previous object.
    /// The proximity is measured in terms of virtual time, which can differ from actual time.
    pub fn find_closest_of_type(&self, base: float,
                                cond: &fn(&Obj<S,I>) -> bool) -> Option<Pointer<S,I>> {
        let previous = self.find_previous_of_type(|obj| cond(obj)); // XXX #7363
        let next = self.find_next_of_type(|obj| cond(obj)); // XXX #7363
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

/// Timeline additions for pointers.
pub trait TimelinePointerUtil<SoundRef,ImageRef> {
    /// Returns a pointer pointing the first object in the timeline.
    pub fn pointer(@self) -> Pointer<SoundRef,ImageRef>;
    /// Returns a pointer pointing given object in the timeline.
    pub fn pointer_with_pos(@self, pos: uint) -> Pointer<SoundRef,ImageRef>;
}

impl<S,I> TimelinePointerUtil<S,I> for Timeline<S,I> {
    pub fn pointer(@self) -> Pointer<S,I> {
        Pointer { timeline: self, pos: 0 }
    }

    pub fn pointer_with_pos(@self, pos: uint) -> Pointer<S,I> {
        Pointer { timeline: self, pos: pos }
    }
}

