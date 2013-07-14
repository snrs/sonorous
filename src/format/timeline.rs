// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Timeline interface.

use std::int;
use format::obj::Obj;

/// A portion of game data which is not associated to resources and other metadata. Timelines are
/// immutable by design (except for `modf` module), and should be built by `TimelineBuilder`
/// in order to satisfy the invariant.
pub struct Timeline<SoundRef,ImageRef> {
    /// List of objects sorted by the position. (C: `objs`)
    objs: ~[Obj<SoundRef,ImageRef>],
    /// The scaling factor of measures. Defaults to 1.0. (C: `shortens`)
    shortens: ~[float],
}

impl<SoundRef,ImageRef> Timeline<SoundRef,ImageRef> {
    /// Returns a scaling factor of given measure number. The default scaling factor is 1.0, and
    /// that value applies to any out-of-bound measures. (C: `shorten`)
    pub fn shorten(&self, measure: int) -> float {
        if measure < 0 || measure as uint >= self.shortens.len() {
            1.0
        } else {
            self.shortens[measure as uint]
        }
    }

    /// Calculates the virtual time that is `offset` measures away from the virtual time `base`.
    /// This takes account of the scaling factor, so if first four measures are scaled by 1/4,
    /// then `adjust_object_time(0.0, 2.0)` results in `5.0`. (C: `adjust_object_time`)
    pub fn adjust_object_time(&self, base: float, offset: float) -> float {
        let basemeasure = base.floor() as int;
        let baseshorten = self.shorten(basemeasure);
        let basefrac = base - basemeasure as float;
        let tonextmeasure = (1.0 - basefrac) * baseshorten;
        if offset < tonextmeasure {
            base + offset / baseshorten
        } else {
            let mut offset = offset - tonextmeasure;
            let mut i = basemeasure + 1;
            let mut curshorten = self.shorten(i);
            while offset >= curshorten {
                offset -= curshorten;
                i += 1;
                curshorten = self.shorten(i);
            }
            i as float + offset / curshorten
        }
    }

    /// Calculates an adjusted offset between the virtual time `base` and `base + offset`.
    /// This takes account of the measure scaling factor, so for example, the adjusted offset
    /// between the virtual time 0.0 and 2.0 is, if the measure #000 is scaled by 1.2x,
    /// 2.2 measures instead of 2.0 measures. (C: `adjust_object_position`)
    pub fn adjust_object_position(&self, base: float, time: float) -> float {
        let basemeasure = base.floor() as int;
        let timemeasure = time.floor() as int;
        let basefrac = base - basemeasure as float;
        let timefrac = time - timemeasure as float;
        let mut pos = timefrac * self.shorten(timemeasure) -
                      basefrac * self.shorten(basemeasure);
        for int::range(basemeasure, timemeasure) |i| {
            pos += self.shorten(i);
        }
        pos
    }
}

/// A timeline builder.
pub mod builder {
    use std::uint;
    use format::obj::*;
    use super::Timeline;

    /// Fixes a problematic data. (C: `sanitize_bms`)
    fn sanitize_objs<S:Copy,I:Copy>(objs: &mut [Obj<S,I>]) {
        /// Abstracted sanitization algorithm. Basically, objects are categorized into several types
        /// and the algorithm removes or replaces objects which have the same or conflicting type
        /// in the same position. Conflicting types are specified by `merge_types` function.
        fn sanitize<S:Copy,I:Copy>(objs: &mut [Obj<S,I>], to_type: &fn(&Obj<S,I>) -> Option<uint>,
                                   merge_types: &fn(uint) -> uint) {
            let len = objs.len();
            let mut i = 0;
            while i < len {
                let cur = objs[i].time;
                let mut types = 0;
                let mut j = i;
                while j < len && objs[j].time <= cur {
                    let ty = to_type(&objs[j]); // XXX #3511
                    for ty.iter().advance |&t| {
                        if (types & (1 << t)) != 0 {
                            // duplicate type
                            objs[j] = (copy objs[j]).to_effect();
                        } else {
                            types |= 1 << t;
                        }
                    }
                    j += 1;
                }

                types = merge_types(types);

                while i < j {
                    let ty = to_type(&objs[i]); // XXX #3511
                    for ty.iter().advance |&t| {
                        if (types & (1 << t)) == 0 {
                            objs[i] = (copy objs[i]).to_effect();
                        }
                    }
                    i += 1;
                }
            }
        }

        // notes are processed in the per-lane basis (as we need to keep the internal status).
        for uint::range(0, NLANES) |lane| {
            let lane0 = Lane(lane);

            static LNDONE: uint = 0;
            static LNSTART: uint = 1;
            static VISIBLE: uint = 2;
            static INVISIBLE: uint = 3;
            static BOMB: uint = 4;
            let to_type = |obj: &Obj<S,I>| -> Option<uint> {
                match obj.data {
                    Visible(lane,_) if lane == lane0 => Some(VISIBLE),
                    Invisible(lane,_) if lane == lane0 => Some(INVISIBLE),
                    LNStart(lane,_) if lane == lane0 => Some(LNSTART),
                    LNDone(lane,_) if lane == lane0 => Some(LNDONE),
                    Bomb(lane,_,_) if lane == lane0 => Some(BOMB),
                    _ => None,
                }
            };

            let mut inside = false;
            do sanitize(objs, |obj| to_type(obj)) |mut types| { // XXX #7363
                static LNMASK: uint = (1 << LNSTART) | (1 << LNDONE);

                // remove overlapping LN endpoints altogether
                if (types & LNMASK) == LNMASK { types &= !LNMASK; }

                // remove prohibited types according to inside
                if inside {
                    types &= !((1 << LNSTART) | (1 << VISIBLE) | (1 << BOMB));
                } else {
                    types &= !(1 << LNDONE);
                }

                // invisible note cannot overlap with long note endpoints
                if (types & LNMASK) != 0 { types &= !(1 << INVISIBLE); }

                // keep the most important (lowest) type, except for
                // BOMB/INVISIBLE combination
                let lowest = types & -types;
                if lowest == (1 << INVISIBLE) {
                    types = lowest | (types & (1 << BOMB));
                } else {
                    types = lowest;
                }

                if (types & (1 << LNSTART)) != 0 {
                    inside = true;
                } else if (types & (1 << LNDONE)) != 0 {
                    inside = false;
                }

                types
            }

            if inside {
                // remove last starting longnote which is unfinished
                match objs.rposition(|obj| to_type(obj).is_some()) {
                    Some(pos) if objs[pos].is_lnstart() => { objs[pos] = objs[pos].to_effect(); }
                    _ => {}
                }
            }
        }

        // object-like effects (except for BGMs) are simpler, any conflicting objects are removed.
        sanitize(objs,
                 |&obj| match obj.data {
                            SetBGA(Layer1,_) => Some(0),
                            SetBGA(Layer2,_) => Some(1),
                            SetBGA(Layer3,_) => Some(2),
                            SetBGA(PoorBGA,_) => Some(3),
                            SetBPM(*) => Some(4),
                            Stop(*) => Some(5),
                            _ => None,
                        },
                 |types| types);
    }

    /// An unprocessed game data which will eventually produce `Timeline` value.
    pub struct TimelineBuilder<SoundRef,ImageRef> {
        /// Same as `Timeline::objs` but not yet sorted.
        objs: ~[Obj<SoundRef,ImageRef>],
        /// Same as `Timeline::shortens`.
        shortens: ~[float],
    }

    /// A mark to the existing object. The caller is recommended not to directly mutate the builder
    /// contents and to use this and `TimelineBuilder::{mutate,delete}` methods.
    pub struct Mark(uint);

    impl<S:Copy,I:Copy> TimelineBuilder<S,I> {
        /// Creates a new timeline builder.
        pub fn new() -> ~TimelineBuilder<S,I> {
            ~TimelineBuilder { objs: ~[], shortens: ~[] }
        }

        /// Sets a shorten factor for given measure.
        pub fn set_shorten(&mut self, measure: uint, factor: float) {
            assert!(factor > 0.0);
            self.shortens.grow_set(measure, &1.0, factor);
        }

        /// Adds an object at given position.
        pub fn add(&mut self, pos: float, data: ObjData<S,I>) {
            self.objs.push(Obj { time: pos, data: data });
        }

        /// Adds an object at given position and returns a mark to that object for later usage.
        pub fn add_and_mark(&mut self, pos: float, data: ObjData<S,I>) -> Mark {
            let index = self.objs.len();
            self.objs.push(Obj { time: pos, data: data });
            Mark(index)
        }

        /// Mutates an existing object.
        pub fn mutate(&mut self, mark: Mark, f: &fn(float,ObjData<S,I>) -> (float,ObjData<S,I>)) {
            assert!(*mark < self.objs.len());
            let Obj { time: pos, data: data } = copy self.objs[*mark];
            let (pos, data) = f(pos, data);
            self.objs[*mark] = Obj { time: pos, data: data };
        }

        /// Deletes an existing object (or rather, marks it as deleted).
        pub fn delete(&mut self, mark: Mark) {
            self.mutate(mark, |pos,_data| (pos, Deleted));
        }

        /// Builds an actual timeline.
        //
        // Rust: what is happening? I can't get `build(~self)` signature working...
        pub fn build(self) -> ~Timeline<S,I> {
            let TimelineBuilder { objs: objs, shortens: shortens } = self;
            let mut objs = objs;
            ::extra::sort::tim_sort(objs);
            sanitize_objs(objs);
            objs.retain(|&obj| !obj.is_deleted());
            ~Timeline { objs: objs, shortens: shortens }
        }
    }
}

/// Modifiers available to timelines. They are safe to run through the existing timeline, as long as
/// the original timeline is no longer used.
pub mod modf {
    use std::{vec, float};
    use std::rand::*;
    use format::obj::*;
    use super::Timeline;

    /// Removes objects not in given lanes by replacing them to BGMs. (C: `analyze_and_compact_bms`)
    pub fn filter_lanes<S:Copy,I:Copy>(timeline: &mut Timeline<S,I>, lanes: &[Lane]) {
        let mut keep = vec::from_elem(NLANES, false);
        for lanes.iter().advance |&Lane(lane)| {
            keep[lane] = true;
        }

        for timeline.objs.mut_iter().advance |obj| {
            match obj.object_lane() {
                Some(lane) if !keep[*lane] => { obj.data = obj.data.to_effect(); }
                _ => {}
            }
        }
        timeline.objs.retain(|&obj| !obj.is_deleted());
    }

    /// Applies a mapping to the object lane if any. This is used to shuffle the lanes without
    /// modifying the relative time position.
    fn map_object_lane<S:Copy,I:Copy>(obj: &mut Obj<S,I>, map: &[Lane]) {
        match obj.object_lane() {
            Some(lane) => { *obj = obj.with_object_lane(map[*lane]); }
            None => {}
        }
    }

    /// Swaps given lanes in the reverse order. (C: `shuffle_bms` with `MIRROR_MODF`)
    pub fn mirror<S:Copy,I:Copy>(timeline: &mut Timeline<S,I>, lanes: &[Lane]) {
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));
        let rlanes = vec::reversed(lanes);
        let assocs = vec::zip_slice(lanes, rlanes); // XXX #3511
        for assocs.iter().advance |&(Lane(from), to)| {
            map[from] = to;
        }

        for timeline.objs.mut_iter().advance |obj| {
            map_object_lane(obj, map);
        }
    }

    /// Swaps given lanes in the random order. (C: `shuffle_bms` with
    /// `SHUFFLE_MODF`/`SHUFFLEEX_MODF`)
    pub fn shuffle<S:Copy,I:Copy,R:RngUtil>(timeline: &mut Timeline<S,I>, r: &mut R,
                                            lanes: &[Lane]) {
        let shuffled = r.shuffle(lanes);
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));
        let assocs = vec::zip_slice(lanes, shuffled); // XXX #3511
        for assocs.iter().advance |&(Lane(from), to)| {
            map[from] = to;
        }

        for timeline.objs.mut_iter().advance |obj| {
            map_object_lane(obj, map);
        }
    }

    /// Swaps given lanes in the random order, where the order is determined per object. It does not
    /// cause objects to move within another LN object, or place two objects in the same or very
    /// close time position to the same lane. (C: `shuffle_bms` with `RANDOM_MODF`/`RANDOMEX_MODF`)
    pub fn randomize<S:Copy,I:Copy,R:RngUtil>(timeline: &mut Timeline<S,I>, r: &mut R,
                                              lanes: &[Lane]) {
        let mut movable = lanes.to_owned();
        let mut map = vec::from_fn(NLANES, |lane| Lane(lane));

        let mut lasttime = float::neg_infinity;
        for timeline.objs.mut_iter().advance |obj| {
            if obj.is_lnstart() {
                let lane = obj.object_lane().get();
                match movable.position_elem(&lane) {
                    Some(i) => { movable.swap_remove(i); }
                    None => fail!(~"non-sanitized timeline")
                }
            }
            if lasttime < obj.time { // reshuffle required
                lasttime = obj.time + 1e-4;
                let shuffled = r.shuffle(movable);
                let assocs = vec::zip_slice(movable, shuffled); // XXX #3511
                for assocs.iter().advance |&(Lane(from), to)| {
                    map[from] = to;
                }
            }
            if obj.is_lnstart() {
                let lane = obj.object_lane().get();
                movable.push(lane);
            }
            map_object_lane(obj, map);
        }
    }
}

