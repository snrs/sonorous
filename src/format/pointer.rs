// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Pointer interface, which provides a reference to the position or object in the timeline.

use std::{uint, num, managed};
use format::obj::{Obj, ObjAxis, ObjLoc, ObjData, ToObjData};
use format::timeline::Timeline;

/// A pointer to the object. A pointer is used to implement common operations, e.g. iterating
/// until given position, or finding the closest object with given condition. A pointer can also
/// be used like an object when it points to the valid object.
pub struct Pointer<SoundRef,ImageRef> {
    /// A timeline holding objects.
    timeline: @Timeline<SoundRef,ImageRef>,
    /**
     * The position of an object next to the current pointer. `Pointer` implements a `ToObjData`
     * trait, and it will return the data of the object pointed by `index`.
     *
     * Any pointer should satisfy that `objs[ptr.index-1].vpos <= ptr.vpos <= objs[ptr.index].vpos`
     * and so on, unless `index` is 0 (in that case there is no lower bound) or `index` equals to
     * `objs.len()` (in that case there is no upper bound). 
     *
     * The treatment of a group of objects in the same position is complicated. Basically, seeking
     * into the particular position yields a pointer pointing at the first object of the group
     * (`index` is set to the index of that object, so that the next iteration will yield it).
     * The pointer also may point to other objects in the group by, for example, breaking the loop.
     * The iteration properly handles both cases, but you should be aware of the fact that seeking
     * alone does not yield all available objects.
     */
    index: uint,
    /// The current location per axis, not necessarily equal to that of any object.
    loc: ObjLoc<float>,
}

/// Returns true if two pointers share the common timeline.
fn has_same_timeline<S,I>(lhs: &Pointer<S,I>, rhs: &Pointer<S,I>) -> bool {
    managed::ptr_eq(lhs.timeline, rhs.timeline)
}

/// Returns the first index `i` such that `v[i]` is no `Less` than the target, or `v.len()` if
/// there is no such `i`. Similar to `vec::bsearch` but `v[i]` (if any) needs not be `Equal` to
/// the target.
fn bsearch_no_less<T>(v: &[T], f: &fn(&T) -> Ordering) -> uint {
    let mut base = 0;
    let mut limit = v.len();
    while limit != 0 { // invariant: v[base-1] (if any) < target <= v[base+limit] (if any)
        let ix = base + (limit >> 1);
        if f(&v[ix]) == Less {
            base = ix + 1;
            limit -= 1;
        }
        limit >>= 1;
    }
    base
}

impl<S,I> Eq for Pointer<S,I> {
    fn eq(&self, other: &Pointer<S,I>) -> bool {
        has_same_timeline(self, other) && self.loc == other.loc
    }
    fn ne(&self, other: &Pointer<S,I>) -> bool {
        !has_same_timeline(self, other) || self.loc != other.loc
    }
}

impl<S,I> Ord for Pointer<S,I> {
    fn lt(&self, other: &Pointer<S,I>) -> bool {
        assert!(has_same_timeline(self, other));
        self.loc < other.loc
    }
    fn le(&self, other: &Pointer<S,I>) -> bool {
        assert!(has_same_timeline(self, other));
        self.loc <= other.loc
    }
    fn ge(&self, other: &Pointer<S,I>) -> bool {
        assert!(has_same_timeline(self, other));
        self.loc >= other.loc
    }
    fn gt(&self, other: &Pointer<S,I>) -> bool {
        assert!(has_same_timeline(self, other));
        self.loc > other.loc
    }
}

impl<S,I> Clone for Pointer<S,I> {
    pub fn clone(&self) -> Pointer<S,I> {
        Pointer { timeline: self.timeline, index: self.index, loc: self.loc.clone() }
    }
}

impl<S:Copy,I:Copy> ToObjData<S,I> for Pointer<S,I> {
    pub fn to_obj_data(&self) -> ObjData<S,I> { copy self.timeline.objs[self.index].data }
}

/// Given one axis, interpolates remaining axes from the difference of `objs[index-1]` and
/// `objs[index]` (or similar generalization if one of them is out of bounds). Used by
/// `Pointer::locate` and `Pointer::seek` methods.
fn interpolate_loc<S,I>(timeline: &Timeline<S,I>, index: uint,
                        axis: ObjAxis, pos: float) -> ObjLoc<float> {
    let objs: &[Obj<S,I>] = timeline.objs;
    assert!(index == 0 || index == objs.len() ||
            objs[index-1].loc[axis] < objs[index].loc[axis]);

    let starts;
    let diffs;
    if index == 0 || objs.len() < 2 {
        // timeline doesn't have a sufficient information to interpolate.
        // diffs have to be reconstructed from `initbpm`.
        let measuresize = timeline.initbpm.measure_to_sec(1.0);
        starts = ObjLoc { vpos: 0.0, pos: 0.0, vtime: 0.0, time: 0.0 };
        diffs = ObjLoc { vpos: 1.0, pos: 1.0, vtime: measuresize, time: measuresize };
    } else {
        // at the end, use the differences between second-to-last and last objects instead.
        // this is why we need an explicit `End` object...
        let index = if index == objs.len() {objs.len()-1} else {index};
        let prev = &objs[index-1].loc;
        let next = &objs[index].loc;
        starts = prev.clone();
        diffs = ObjLoc { vpos: next.vpos - prev.vpos, pos: next.pos - prev.pos,
                         vtime: next.vtime - prev.vtime, time: next.time - prev.time };
    }
    assert!(diffs[axis] > 0.0);

    let frac = (pos - starts[axis]) / diffs[axis];
    ObjLoc { vpos: starts.vpos + diffs.vpos * frac, pos: starts.pos + diffs.pos * frac,
             vtime: starts.vtime + diffs.vtime * frac, time: starts.time + diffs.time * frac }
}

impl<S:Copy,I:Copy> Pointer<S,I> {
    /// Returns true if the pointer invariant holds.
    pub fn invariant(&self) -> bool {
        if self.index > 0 {
            let prev = &self.timeline.objs[self.index-1];
            if !(prev.loc.vpos  <= self.loc.vpos ) { return false; }
            if !(prev.loc.pos   <= self.loc.pos  ) { return false; }
            if !(prev.loc.vtime <= self.loc.vtime) { return false; }
            if !(prev.loc.time  <= self.loc.time ) { return false; }
        }
        if self.index < self.timeline.objs.len() {
            let next = &self.timeline.objs[self.index-1];
            if !(self.loc.vpos  <= next.loc.vpos ) { return false; }
            if !(self.loc.pos   <= next.loc.pos  ) { return false; }
            if !(self.loc.vtime <= next.loc.vtime) { return false; }
            if !(self.loc.time  <= next.loc.time ) { return false; }
        }
        true
    }

    /// Returns the associated game data of pointed object. Short for `to_obj_data`.
    pub fn data(&self) -> ObjData<S,I> { self.to_obj_data() }

    /// Returns a pointer to the first object which position is no less than `pos`.
    /// It takes `O(log n)` time, and should be preferred when there is no more information about
    /// `pos` (i.e. random access).
    pub fn from_axis(timeline: @Timeline<S,I>, axis: ObjAxis, pos: float) -> Pointer<S,I> {
        // we seek to the first object no less than given position. with an exception of the very
        // first and last object, an object right before this object should be less than given
        // position therefore.
        let index = do bsearch_no_less(timeline.objs) |obj| {
            let pos_ = obj.loc[axis];
            if pos_ < pos {Less} else if pos_ > pos {Greater} else {Equal}
        };

        // the previous object (`objs[index-1]`) is guaranteed to be before `pos`, so we first
        // seek to the previous object then interpolate locations.
        let loc = interpolate_loc(&*timeline, index, axis, pos);
        Pointer { timeline: timeline, index: index, loc: loc }
    }

    /// Returns a pointer to the object pointed by `index`.
    pub fn from_index(timeline: @Timeline<S,I>, index: uint) -> Pointer<S,I> {
        assert!(index < timeline.objs.len());
        Pointer { timeline: timeline, index: index, loc: timeline.objs[index].loc.clone() }
    }

    /// Returns a pointer pointing at the end of the timeline (actually, an `End` object).
    pub fn from_end(timeline: @Timeline<S,I>) -> Pointer<S,I> {
        Pointer::from_index(timeline, timeline.objs.len() - 1)
    }

    /// Locates the first object which position is no less than the current position plus `delta`.
    /// It can move in both forward and backward motion. It takes `O(n)` time, and should be
    /// preferred when `pos` is close enough to the current pointer (i.e. linear access).
    pub fn seek(&mut self, axis: ObjAxis, delta: float) {
        // do nothing, as the following algorithm can't guarantee that it will locate the first
        // object when the pointer already points the object which is not the first object of
        // given position and `delta` is zero.
        if delta == 0.0 { return; }
        let pos = self.loc[axis] + delta;

        let objs: &[Obj<S,I>] = self.timeline.objs;
        let mut index = self.index;
        if delta > 0.0 { // forward
            while index < objs.len() && objs[index].loc[axis] < pos {
                index += 1;
            }
        } else { // backward
            while index > 0 && objs[index-1].loc[axis] >= pos {
                index -= 1;
            }
        }
        self.index = index;
        self.loc = interpolate_loc(&*self.timeline, index, axis, pos);
    }

    /// Returns a new pointer that points to the first object which position is no less than
    /// the current position plus `delta`.
    pub fn find(&self, axis: ObjAxis, delta: float) -> Pointer<S,I> {
        let mut ptr = self.clone();
        ptr.seek(axis, delta);
        ptr
    }

    /// Returns a pointer to the object pointed by `index`.
    pub fn find_index(&self, index: uint) -> Pointer<S,I> {
        Pointer::from_index(self.timeline, index)
    }

    /// Returns a pointer pointing at the end of the timeline (actually, an `End` object).
    pub fn find_end(&self) -> Pointer<S,I> {
        Pointer::from_end(self.timeline)
    }

    /// Iterates up to given pointer (not including it).
    pub fn upto(&self, end: &Pointer<S,I>, f: &fn(Pointer<S,I>) -> bool) -> bool {
        assert!(has_same_timeline(self, end));
        let timeline = self.timeline;
        uint::range(self.index, end.index, |i| f(Pointer::from_index(timeline, i)))
    }

    /// Iterates objects until the object is no less than `delta` measures/seconds away from
    /// the current position.
    pub fn until(&self, axis: ObjAxis, delta: float, f: &fn(Pointer<S,I>) -> bool) -> bool {
        let pos = self.loc[axis] + delta;
        let timeline = self.timeline;
        let objs: &[Obj<S,I>] = self.timeline.objs;
        let nobjs = objs.len();
        let mut i = self.index;
        while i < nobjs && objs[i].loc[axis] < pos {
            if !f(Pointer::from_index(timeline, i)) { return false; }
            i += 1;
        }
        true
    }

    /// Iterates up to given pointer (not including it) while updating the current pointer.
    /// If the caller breaks the loop, the pointer points to the last returned object. Otherwise
    /// the pointer is set to given pointer. In any case the pointer doesn't change during the loop.
    pub fn mut_upto(&mut self, end: &Pointer<S,I>, f: &fn(Pointer<S,I>) -> bool) -> bool {
        assert!(has_same_timeline(self, end));
        if self.index > end.index || self.loc > end.loc { return true; }

        let timeline = self.timeline;
        let objs: &[Obj<S,I>] = self.timeline.objs;
        let limit = end.index;
        let mut i = self.index;
        while i < limit {
            if !f(Pointer::from_index(timeline, i)) {
                self.index = i;
                self.loc = objs[i].loc.clone();
                return false;
            }
            i += 1;
        }
        self.index = i;
        self.loc = end.loc.clone();
        true
    }

    /// Iterates objects until the object is no less than `delta` measures/seconds away from
    /// the current position, while updating the current pointer. If the caller breaks the loop,
    /// the pointer points to the last returned object. Otherwise the location is set to given
    /// position. In any case the pointer doesn't change during the loop.
    pub fn mut_until(&mut self, axis: ObjAxis, delta: float, f: &fn(Pointer<S,I>) -> bool) -> bool {
        if delta <= 0.0 { return true; }
        let pos = self.loc[axis] + delta;

        let timeline = self.timeline;
        let objs: &[Obj<S,I>] = self.timeline.objs;
        let nobjs = objs.len();
        let mut i = self.index;
        while i < nobjs && objs[i].loc[axis] < pos {
            if !f(Pointer::from_index(timeline, i)) {
                self.index = i;
                self.loc = objs[i].loc.clone();
                return false;
            }
            i += 1;
        }
        self.index = i;
        self.loc = interpolate_loc(&*self.timeline, i, axis, pos);
        true
    }

    /// Finds the next object that satisfies given condition if any, without updating itself.
    pub fn find_next_of_type(&self, cond: &fn(&Obj<S,I>) -> bool) -> Option<Pointer<S,I>> {
        let objs: &[Obj<S,I>] = self.timeline.objs;
        let nobjs = objs.len();
        let mut i = self.index;
        while i < nobjs {
            let current = &objs[i];
            if cond(current) { return Some(Pointer::from_index(self.timeline, i)); }
            i += 1;
        }
        None
    }

    /// Finds the previous object that satisfies given condition if any, without updating itself.
    pub fn find_previous_of_type(&self, cond: &fn(&Obj<S,I>) -> bool) -> Option<Pointer<S,I>> {
        let mut i = self.index;
        while i > 0 {
            i -= 1;
            let current = &self.timeline.objs[i];
            if cond(current) { return Some(Pointer::from_index(self.timeline, i)); }
        }
        None
    }

    /// Finds the closest object from the curernt pointer that satisfies given condition if any.
    /// The proximity is measured in terms of given axis.
    pub fn find_closest_of_type(&self, axis: ObjAxis,
                                cond: &fn(&Obj<S,I>) -> bool) -> Option<Pointer<S,I>> {
        let prev = self.find_previous_of_type(|obj| cond(obj)); // XXX #7363
        let next = self.find_next_of_type(|obj| cond(obj)); // XXX #7363
        match (prev, next) {
            (None, None) => None,
            (None, Some(next)) => Some(next),
            (Some(prev), None) => Some(prev),
            (Some(prev), Some(next)) =>
                if num::abs(prev.loc[axis] - self.loc[axis]) <
                   num::abs(next.loc[axis] - self.loc[axis]) { Some(prev) }
                else { Some(next) }
        }
    }
}

/// Timeline additions for pointers.
pub trait TimelinePointerUtil<SoundRef,ImageRef> {
    /// Returns a pointer pointing at given position in the timeline.
    pub fn pointer(@self, axis: ObjAxis, pos: float) -> Pointer<SoundRef,ImageRef>;
    /// Returns a pointer pointing at given object in the timeline.
    pub fn pointer_with_index(@self, index: uint) -> Pointer<SoundRef,ImageRef>;
    /// Returns a pointer pointing at the end of the timeline.
    pub fn pointer_with_end(@self) -> Pointer<SoundRef,ImageRef>;
}

impl<S:Copy,I:Copy> TimelinePointerUtil<S,I> for Timeline<S,I> {
    pub fn pointer(@self, axis: ObjAxis, pos: float) -> Pointer<S,I> {
        Pointer::from_axis(self, axis, pos)
    }

    pub fn pointer_with_index(@self, index: uint) -> Pointer<S,I> {
        Pointer::from_index(self, index)
    }

    pub fn pointer_with_end(@self) -> Pointer<S,I> {
        Pointer::from_end(self)
    }
}

