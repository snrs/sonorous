// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Pointer interface, which provides a reference to the position or object in the timeline.

use std::num;
use std::rc::Rc;
use format::obj::{Obj, ObjAxis, ObjLoc, ObjData, ToObjData};
use format::timeline::Timeline;

/// A pointer to the object. A pointer is used to implement common operations, e.g. iterating
/// until given position, or finding the closest object with given condition. A pointer can also
/// be used like an object when it points to the valid object.
pub struct Pointer<SoundRef,ImageRef> {
    /// A timeline holding objects.
    pub timeline: Rc<Timeline<SoundRef,ImageRef>>,
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
    pub index: uint,
    /// The current location per axis, not necessarily equal to that of any object.
    pub loc: ObjLoc<f64>,
}

/// Returns true if two pointers share the common timeline.
fn has_same_timeline<S,I>(lhs: &Pointer<S,I>, rhs: &Pointer<S,I>) -> bool {
    lhs.timeline.deref() as *const Timeline<S,I> == rhs.timeline.deref() as *const Timeline<S,I>
}

/// Returns the first index `i` such that `v[i]` is no `Less` than the target, or `v.len()` if
/// there is no such `i`. Similar to `vec::bsearch` but `v[i]` (if any) needs not be `Equal` to
/// the target.
fn bsearch_no_less<T>(v: &[T], f: |&T| -> Ordering) -> uint {
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

impl<S,I> PartialEq for Pointer<S,I> {
    fn eq(&self, other: &Pointer<S,I>) -> bool {
        has_same_timeline(self, other) && self.loc == other.loc
    }
    fn ne(&self, other: &Pointer<S,I>) -> bool {
        !has_same_timeline(self, other) || self.loc != other.loc
    }
}

impl<S,I> PartialOrd for Pointer<S,I> {
    fn partial_cmp(&self, other: &Pointer<S,I>) -> Option<Ordering> {
        assert!(has_same_timeline(self, other));
        self.loc.partial_cmp(&other.loc)
    }
}

impl<S,I> Clone for Pointer<S,I> {
    fn clone(&self) -> Pointer<S,I> {
        Pointer { timeline: self.timeline.clone(), index: self.index, loc: self.loc.clone() }
    }
}

impl<S:Clone,I:Clone> ToObjData<S,I> for Pointer<S,I> {
    fn to_obj_data(&self) -> ObjData<S,I> {
        self.timeline.objs[self.index].data.clone()
    }
}

/// Given one axis, interpolates remaining axes from the difference of `objs[index-1]` and
/// `objs[index]` (or similar generalization if one of them is out of bounds). Used by
/// `Pointer::locate` and `Pointer::seek` methods.
fn interpolate_loc<S,I>(timeline: &Timeline<S,I>, index: uint,
                        axis: ObjAxis, pos: f64) -> ObjLoc<f64> {
    let objs = timeline.objs[];
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

/// An iterator object for `Pointer::upto`.
#[deriving(Clone)]
pub struct UptoIterator<SoundRef,ImageRef> {
    timeline: Rc<Timeline<SoundRef,ImageRef>>,
    cur: uint,
    end: uint,
}

impl<S,I> Iterator<Pointer<S,I>> for UptoIterator<S,I> {
    fn next(&mut self) -> Option<Pointer<S,I>> {
        if self.cur < self.end {
            let i = self.cur;
            self.cur += 1;
            Some(Pointer::from_index(self.timeline.clone(), i))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        let remaining = self.end - self.cur;
        (remaining, Some(remaining))
    }
}

/// An iterator object for `Pointer::until`.
#[deriving(Clone)]
pub struct UntilIterator<SoundRef,ImageRef> {
    timeline: Rc<Timeline<SoundRef,ImageRef>>,
    cur: uint,
    axis: ObjAxis,
    endpos: f64,
}

impl<S,I> Iterator<Pointer<S,I>> for UntilIterator<S,I> {
    fn next(&mut self) -> Option<Pointer<S,I>> {
        if self.cur < self.timeline.objs.len() {
            let i = self.cur;
            if self.timeline.objs[i].loc[self.axis] < self.endpos {
                self.cur += 1;
                Some(Pointer::from_index(self.timeline.clone(), i))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        (0, Some(self.timeline.objs.len() - self.cur))
    }
}

/// An iterator object for `Pointer::mut_upto`.
pub struct MutUptoIterator<'r, SoundRef:'r, ImageRef:'r> {
    ptr: &'r mut Pointer<SoundRef,ImageRef>,
    finished: bool,
    lastloc: Option<ObjLoc<f64>>,
    cur: uint,
    end: uint,
    endloc: ObjLoc<f64>,
}

impl<'r,S,I> Iterator<Pointer<S,I>> for MutUptoIterator<'r,S,I> {
    fn next(&mut self) -> Option<Pointer<S,I>> {
        if self.finished { return None; }

        if self.cur < self.end {
            let i = self.cur;
            self.lastloc = Some(self.ptr.timeline.objs[i].loc.clone());
            self.cur += 1;
            return Some(Pointer::from_index(self.ptr.timeline.clone(), i));
        }

        self.ptr.index = self.cur;
        self.ptr.loc = self.endloc.clone();
        self.finished = true;
        None
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        if self.finished {
            (0, Some(0))
        } else {
            let remaining = self.end - self.cur;
            (remaining, Some(remaining))
        }
    }
}

#[unsafe_destructor]
impl<'r,S,I> Drop for MutUptoIterator<'r,S,I> {
    fn drop(&mut self) {
        if !self.finished {
            self.ptr.index = self.cur - 1;
            self.ptr.loc = self.lastloc.expect("next() was never called").clone();
        }
    }
}

/// An iterator object for `Pointer::mut_until`.
pub struct MutUntilIterator<'r, SoundRef:'r, ImageRef:'r> {
    ptr: &'r mut Pointer<SoundRef,ImageRef>,
    finished: bool,
    lastloc: Option<ObjLoc<f64>>,
    cur: uint,
    axis: ObjAxis,
    endpos: f64,
}

impl<'r,S,I> Iterator<Pointer<S,I>> for MutUntilIterator<'r,S,I> {
    fn next(&mut self) -> Option<Pointer<S,I>> {
        if self.finished { return None; }

        if self.cur < self.ptr.timeline.objs.len() {
            let i = self.cur;
            if self.ptr.timeline.objs[i].loc[self.axis] < self.endpos {
                self.lastloc = Some(self.ptr.timeline.objs[i].loc.clone());
                self.cur += 1;
                return Some(Pointer::from_index(self.ptr.timeline.clone(), i));
            }
        }

        self.ptr.index = self.cur;
        self.ptr.loc = interpolate_loc(self.ptr.timeline.deref(),
                                       self.cur, self.axis, self.endpos);
        self.finished = true;
        None
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        if self.finished {
            (0, Some(0))
        } else {
            (0, Some(self.ptr.timeline.objs.len() - self.cur))
        }
    }
}

#[unsafe_destructor]
impl<'r,S,I> Drop for MutUntilIterator<'r,S,I> {
    fn drop(&mut self) {
        if !self.finished {
            self.ptr.index = self.cur - 1;
            self.ptr.loc = self.lastloc.expect("next() was never called").clone();
        }
    }
}

impl<S:Clone,I:Clone> Pointer<S,I> {
    /// Returns the associated game data of pointed object. Short for `to_obj_data`.
    pub fn data(&self) -> ObjData<S,I> { self.to_obj_data() }
}

impl<S,I> Pointer<S,I> {
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

    /// Returns a pointer to the first object which position is no less than `pos`.
    /// It takes `O(log n)` time, and should be preferred when there is no more information about
    /// `pos` (i.e. random access).
    pub fn from_axis(timeline: Rc<Timeline<S,I>>, axis: ObjAxis, pos: f64) -> Pointer<S,I> {
        // we seek to the first object no less than given position. with an exception of the very
        // first and last object, an object right before this object should be less than given
        // position therefore.
        let index = bsearch_no_less(timeline.objs[], |obj| {
            let pos_ = obj.loc[axis];
            if pos_ < pos {Less} else if pos_ > pos {Greater} else {Equal}
        });

        // the previous object (`objs[index-1]`) is guaranteed to be before `pos`, so we first
        // seek to the previous object then interpolate locations.
        let loc = interpolate_loc(timeline.deref(), index, axis, pos);
        Pointer { timeline: timeline, index: index, loc: loc }
    }

    /// Returns a pointer to the object pointed by `index`.
    pub fn from_index(timeline: Rc<Timeline<S,I>>, index: uint) -> Pointer<S,I> {
        assert!(index < timeline.objs.len());
        let loc = timeline.objs[index].loc.clone();
        Pointer { timeline: timeline, index: index, loc: loc }
    }

    /// Returns a pointer pointing at the end of the timeline (actually, an `End` object).
    pub fn from_end(timeline: Rc<Timeline<S,I>>) -> Pointer<S,I> {
        let index = timeline.objs.len() - 1;
        Pointer::from_index(timeline, index)
    }

    /// Locates the first object which position is no less than the current position plus `delta`.
    /// It can move in both forward and backward motion. It takes `O(n)` time, and should be
    /// preferred when `pos` is close enough to the current pointer (i.e. linear access).
    pub fn seek(&mut self, axis: ObjAxis, delta: f64) {
        // do nothing, as the following algorithm can't guarantee that it will locate the first
        // object when the pointer already points the object which is not the first object of
        // given position and `delta` is zero.
        if delta == 0.0 { return; }
        let pos = self.loc[axis] + delta;

        let objs = self.timeline.objs[];
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
        self.loc = interpolate_loc(self.timeline.deref(), index, axis, pos);
    }

    /// Returns a new pointer that points to the first object which position is no less than
    /// the current position plus `delta`.
    pub fn find(&self, axis: ObjAxis, delta: f64) -> Pointer<S,I> {
        let mut ptr = self.clone();
        ptr.seek(axis, delta);
        ptr
    }

    /// Returns a pointer to the object pointed by `index`.
    pub fn find_index(&self, index: uint) -> Pointer<S,I> {
        Pointer::from_index(self.timeline.clone(), index)
    }

    /// Returns a pointer pointing at the end of the timeline (actually, an `End` object).
    pub fn find_end(&self) -> Pointer<S,I> {
        Pointer::from_end(self.timeline.clone())
    }

    /// Iterates up to given pointer (not including it).
    pub fn upto(&self, end: &Pointer<S,I>) -> UptoIterator<S,I> {
        assert!(has_same_timeline(self, end));
        UptoIterator { timeline: self.timeline.clone(), cur: self.index, end: end.index }
    }

    /// Iterates objects until the object is no less than `delta` measures/seconds away from
    /// the current position.
    pub fn until(&self, axis: ObjAxis, delta: f64) -> UntilIterator<S,I> {
        let pos = self.loc[axis] + delta;
        UntilIterator { timeline: self.timeline.clone(), cur: self.index, axis: axis, endpos: pos }
    }

    /// Iterates up to given pointer (not including it) while updating the current pointer.
    /// If the caller breaks the loop, the pointer points to the last returned object. Otherwise
    /// the pointer is set to given pointer. In any case the pointer doesn't change during the loop.
    pub fn mut_upto<'r>(&'r mut self, end: &Pointer<S,I>) -> MutUptoIterator<'r,S,I> {
        assert!(has_same_timeline(self, end));
        let donotupdate = self.index > end.index || self.loc > end.loc;
        let index = self.index; // XXX #6268
        MutUptoIterator { ptr: self, finished: donotupdate, lastloc: None,
                          cur: index, end: end.index, endloc: end.loc.clone() }
    }

    /// Iterates objects until the object is no less than `delta` measures/seconds away from
    /// the current position, while updating the current pointer. If the caller breaks the loop,
    /// the pointer points to the last returned object. Otherwise the location is set to given
    /// position. In any case the pointer doesn't change during the loop.
    pub fn mut_until<'r>(&'r mut self, axis: ObjAxis, delta: f64) -> MutUntilIterator<'r,S,I> {
        let donotupdate = delta <= 0.0;
        let pos = self.loc[axis] + delta;
        let index = self.index; // XXX #6268
        MutUntilIterator { ptr: self, finished: donotupdate, lastloc: None,
                           cur: index, axis: axis, endpos: pos }
    }

    /// Finds the next object that satisfies given condition if any, without updating itself.
    pub fn find_next_of_type(&self, cond: |&Obj<S,I>| -> bool) -> Option<Pointer<S,I>> {
        let objs = self.timeline.objs[];
        let nobjs = objs.len();
        let mut i = self.index;
        while i < nobjs {
            let current = &objs[i];
            if cond(current) { return Some(Pointer::from_index(self.timeline.clone(), i)); }
            i += 1;
        }
        None
    }

    /// Finds the previous object that satisfies given condition if any, without updating itself.
    pub fn find_previous_of_type(&self, cond: |&Obj<S,I>| -> bool) -> Option<Pointer<S,I>> {
        let mut i = self.index;
        while i > 0 {
            i -= 1;
            let current = &self.timeline.objs[i];
            if cond(current) { return Some(Pointer::from_index(self.timeline.clone(), i)); }
        }
        None
    }

    /// Finds the closest object from the curernt pointer that satisfies given condition if any.
    /// The proximity is measured in terms of given axis.
    pub fn find_closest_of_type(&self, axis: ObjAxis,
                                cond: |&Obj<S,I>| -> bool) -> Option<Pointer<S,I>> {
        let prev = self.find_previous_of_type(|obj| cond(obj));
        let next = self.find_next_of_type(|obj| cond(obj));
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
    fn pointer(&self, axis: ObjAxis, pos: f64) -> Pointer<SoundRef,ImageRef>;
    /// Returns a pointer pointing at given object in the timeline.
    fn pointer_with_index(&self, index: uint) -> Pointer<SoundRef,ImageRef>;
    /// Returns a pointer pointing at the end of the timeline.
    fn pointer_with_end(&self) -> Pointer<SoundRef,ImageRef>;
}

impl<S:Clone,I:Clone> TimelinePointerUtil<S,I> for Rc<Timeline<S,I>> {
    fn pointer(&self, axis: ObjAxis, pos: f64) -> Pointer<S,I> {
        Pointer::from_axis(self.clone(), axis, pos)
    }

    fn pointer_with_index(&self, index: uint) -> Pointer<S,I> {
        Pointer::from_index(self.clone(), index)
    }

    fn pointer_with_end(&self) -> Pointer<S,I> {
        Pointer::from_end(self.clone())
    }
}

