// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Timeline interface.

use std::{int, cmp};
use format::obj::*;

/// A portion of game data which is not associated to resources and other metadata. Timelines are
/// immutable by design (except for `modf` module), and should be built by `TimelineBuilder`
/// in order to satisfy the invariant.
pub struct Timeline<SoundRef,ImageRef> {
    /// Initial BPM. (C: `initbpm`)
    initbpm: BPM,
    /// List of objects sorted by the position. (C: `objs`)
    objs: ~[Obj<SoundRef,ImageRef>],
}

/// Derived Timeline information.
pub struct TimelineInfo {
    /// The start position of the timeline. This is either -1.0 or 0.0 depending on the first
    /// measure has any visible objects or not. (C: `originoffset`)
    originoffset: float,
    /// Set to true if the timeline has a BPM change. (C: `hasbpmchange`)
    hasbpmchange: bool,
    /// Set to true if the timeline has long note objects. (C: `haslongnote`)
    haslongnote: bool,
    /// The number of visible objects in the timeline. A long note object counts as one object.
    /// (C: `nnotes`)
    nnotes: int,
    /// The maximum possible score. (C: `maxscore`)
    maxscore: int
}

impl<S:Copy,I:Copy> Timeline<S,I> {
    /// Returns the position of the last object in the chart.
    pub fn end(&self) -> ObjLoc<float> {
        let last = self.objs.last();
        assert!(last.data.is_end());
        last.loc.clone()
    }

    /// Similar to `self.end().time`, but also takes account of `sound_length` which should return
    /// the length of sound resources in seconds or 0.0. It also handles the non-zero origin (by
    /// `originoffset`) and the chart scrolling backwards. (C: `get_bms_duration`)
    pub fn duration(&self, originoffset: float, sound_length: &fn(S) -> float) -> float {
        assert!(originoffset <= 0.0);
        let mut maxtime = 0.0;
        for self.objs.iter().advance |obj| {
            match obj.data {
                Visible(_,Some(ref sref)) | LNStart(_,Some(ref sref)) | BGM(ref sref) => {
                    maxtime = cmp::max(maxtime, obj.loc.time + sound_length(copy *sref));
                }
                SetBPM(BPM(0.0)) => {
                    return obj.loc.time;
                }
                SetBPM(BPM(newbpm)) if newbpm < 0.0 => {
                    // we explicitly ignore `self.end().time` since it will be +inf.
                    let backtime = BPM(-newbpm).measure_to_sec(obj.loc.pos - originoffset);
                    return cmp::max(maxtime, obj.loc.time + backtime);
                }
                _ => {}
            }
        }
        // except for sounds past the end, we normally ends at the position of `End` object.
        cmp::max(maxtime, self.end().time)
    }

    /// Analyzes the timeline. (C: `analyze_and_compact_bms`)
    pub fn analyze(&self) -> TimelineInfo {
        let mut infos = TimelineInfo { originoffset: 0.0, hasbpmchange: false, haslongnote: false,
                                       nnotes: 0, maxscore: 0 };

        for self.objs.iter().advance |&obj| {
            infos.haslongnote |= obj.is_lnstart();
            infos.hasbpmchange |= obj.is_setbpm();

            if obj.is_lnstart() || obj.is_visible() {
                infos.nnotes += 1;
                if obj.loc.time < 1.0 { infos.originoffset = -1.0; }
            }
        }

        for int::range(0, infos.nnotes) |i| {
            let ratio = (i as float) / (infos.nnotes as float);
            infos.maxscore += (300.0 * (1.0 + ratio)) as int;
        }

        infos
    }
}

impl<S:ToStr,I:ToStr> Timeline<S,I> {
    /// Dumps the timeline to the writer for the debugging purpose.
    pub fn dump(&self, writer: @Writer) {
        writer.write_line(
            fmt!("********  ********  ********  ********  SetBPM(%f)", *self.initbpm));
        for self.objs.iter().advance |obj| {
            writer.write_line(
                fmt!("%8.3f  %8.3f  %8.3f  %8.3f  %s",
                     obj.loc.vpos, obj.loc.pos, obj.loc.vtime, obj.loc.time, obj.data.to_str()));
        }
    }
}

/// A timeline builder.
pub mod builder {
    use std::{uint, float};
    use format::obj::*;
    use super::Timeline;

    /// Converts objects to an integral "class". Normally objects have no ordering, but sometimes
    /// some kind of objects should be placed ahead of other kind of objects (e.g. `Stop` after
    /// `SetBPM`) so we need to define a custom ordering.
    fn classify<S,I>(data: &ObjData<S,I>) -> int {
        match *data {
            Deleted | StopEnd | End => -1, // should be removed
            SetMeasureFactor(*) | MeasureBar => 0,
            Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) | Bomb(*) | BGM(*) | SetBGA(*) => 1,
            SetBPM(*) => 2,
            Stop(*) => 3,
        }
    }

    impl<S:Copy,I:Copy> Ord for ObjData<S,I> {
        fn lt(&self, other: &ObjData<S,I>) -> bool { classify(self) < classify(other) }
        fn le(&self, other: &ObjData<S,I>) -> bool { classify(self) <= classify(other) }
        fn ge(&self, other: &ObjData<S,I>) -> bool { classify(self) >= classify(other) }
        fn gt(&self, other: &ObjData<S,I>) -> bool { classify(self) > classify(other) }
    }

    /// Fixes a problematic data. (C: `sanitize_bms`)
    fn sanitize_objs<S:Copy,I:Copy>(objs: &mut [(float,ObjData<S,I>)]) {
        /// Abstracted sanitization algorithm. Basically, objects are categorized into several types
        /// and the algorithm removes or replaces objects which have the same or conflicting type
        /// in the same position. Conflicting types are specified by `merge_types` function.
        fn sanitize<S:Copy,I:Copy>(objs: &mut [(float,ObjData<S,I>)],
                                   to_type: &fn(&ObjData<S,I>) -> Option<uint>,
                                   merge_types: &fn(uint) -> uint) {
            let len = objs.len();
            let mut i = 0;
            while i < len {
                let &(vpos, _data) = &objs[i];
                let mut types = 0;
                let mut j = i;
                while j < len {
                    let &(vpos_, data_) = &objs[j];
                    if vpos_ > vpos { break; }
                    let ty = to_type(&data_); // XXX #3511
                    for ty.iter().advance |&t| {
                        if (types & (1 << t)) != 0 {
                            // duplicate type
                            objs[j] = (vpos_, data_.to_effect());
                        } else {
                            types |= 1 << t;
                        }
                    }
                    j += 1;
                }

                types = merge_types(types);

                while i < j {
                    let &(vpos, data) = &objs[i];
                    let ty = to_type(&data); // XXX #3511
                    for ty.iter().advance |&t| {
                        if (types & (1 << t)) == 0 {
                            objs[i] = (vpos, data.to_effect());
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
            let to_type = |obj: &ObjData<S,I>| -> Option<uint> {
                match *obj {
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
                match objs.rposition(|&(_,data)| to_type(&data).is_some()) {
                    Some(index) => {
                        let &(vpos, data) = &objs[index];
                        if data.is_lnstart() { objs[index] = (vpos, data.to_effect()); }
                    }
                    None => {}
                }
            }
        }

        // object-like effects (except for BGMs) are simpler, any conflicting objects are removed.
        sanitize(objs,
                 |&obj| match obj {
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

    /// Derives other three axes from the virtual position.
    fn precalculate_time<S:Copy,I:Copy>(initbpm: BPM, objs: &[(float,ObjData<S,I>)],
                                        endvpos: float) -> ~[Obj<S,I>] {
        let mut ret = ~[];

        // last discontinuity for vpos-pos relation
        let mut shorten = 1.0;
        let mut shorten_vpos = 0.0;
        let mut shorten_pos = 0.0;

        // last discontinuity for pos-vtime relation
        let mut bpm = initbpm;
        let mut bpm_pos = 0.0;
        let mut bpm_vtime = 0.0;

        // vtime-time relation does not have a factor other than 1 (currently), but vtime can
        // accumulate the `Stop` duration.
        let mut stop_delay = 0.0;

        for objs.iter().advance |&(vpos,data)| {
            // these objects should not be inserted manually
            if classify(&data) < 0 { loop; }

            let pos = (vpos - shorten_vpos) * shorten + shorten_pos;
            let vtime = bpm.measure_to_sec(pos - bpm_pos) + bpm_vtime;
            let time = vtime + stop_delay;

            let loc = ObjLoc { vpos: vpos, pos: pos, vtime: vtime, time: time };
            ret.push(Obj { loc: loc, data: copy data });

            match data {
                SetBPM(BPM(newbpm)) => {
                    if newbpm > 0.0 {
                        bpm = BPM(newbpm);
                        bpm_pos = pos;
                        bpm_vtime = vtime;
                    } else {
                        // the chart scrolls backwards or terminates immediately,
                        // vtime and time should be +inf from now on.
                        bpm_vtime = float::infinity;
                    }
                }
                Stop(duration) => {
                    let delay = duration.to_sec(bpm);
                    stop_delay += delay;
                    ret.push(Obj { loc: ObjLoc { time: time + delay, ..loc }, data: StopEnd });
                }
                SetMeasureFactor(newshorten) => {
                    shorten = newshorten;
                    shorten_vpos = vpos;
                    shorten_pos = pos;
                }
                _ => {}
            }
        }

        // insert a final `End` object at the end
        assert!(ret.is_empty() || ret.last().loc.vpos < endvpos);
        let endpos = (endvpos - shorten_vpos) * shorten + shorten_pos;
        let endvtime = bpm.measure_to_sec(endpos - bpm_pos) + bpm_vtime;
        let endtime = endvtime + stop_delay;
        ret.push(Obj { loc: ObjLoc { vpos: endvpos, pos: endpos, vtime: endvtime, time: endtime},
                       data: End });

        ret
    }

    /// An unprocessed game data which will eventually produce `Timeline` value.
    pub struct TimelineBuilder<SoundRef,ImageRef> {
        /// Same as `Timeline::initbpm`.
        initbpm: Option<BPM>,
        /// Same as `Timeline::objs` but not yet sorted and only has a virtual position.
        objs: ~[(float,ObjData<SoundRef,ImageRef>)],
        /// End position. Every object must have the virtual position less than this value.
        endvpos: float,
    }

    /// A mark to the existing object. The caller is recommended not to directly mutate the builder
    /// contents and to use this and `TimelineBuilder::{mutate,delete}` methods.
    pub struct Mark(uint);

    impl<S:Copy,I:Copy> TimelineBuilder<S,I> {
        /// Creates a new timeline builder.
        pub fn new() -> ~TimelineBuilder<S,I> {
            ~TimelineBuilder { initbpm: None, objs: ~[], endvpos: 0.0 }
        }

        /// Sets an initial BPM.
        pub fn set_initbpm(&mut self, bpm: BPM) {
            self.initbpm = Some(bpm); 
        }

        /// Sets the end position.
        pub fn set_end(&mut self, vpos: float) {
            assert!(vpos >= 0.0);
            self.endvpos = vpos;
        }

        /// Adds an object at given position.
        pub fn add(&mut self, vpos: float, data: ObjData<S,I>) {
            assert!(vpos >= 0.0);
            self.objs.push((vpos, data));
        }

        /// Adds an object at given position and returns a mark to that object for later usage.
        pub fn add_and_mark(&mut self, vpos: float, data: ObjData<S,I>) -> Mark {
            assert!(vpos >= 0.0);
            let index = self.objs.len();
            self.objs.push((vpos, data));
            Mark(index)
        }

        /// Mutates an existing object.
        pub fn mutate(&mut self, mark: Mark, f: &fn(float,ObjData<S,I>) -> (float,ObjData<S,I>)) {
            assert!(*mark < self.objs.len());
            let (vpos, data) = copy self.objs[*mark];
            let (vpos, data) = f(vpos, data);
            assert!(vpos >= 0.0);
            self.objs[*mark] = (vpos, data);
        }

        /// Deletes an existing object (or rather, marks it as deleted).
        pub fn delete(&mut self, mark: Mark) {
            self.mutate(mark, |vpos,_data| (vpos, Deleted));
        }

        /// Builds an actual timeline.
        pub fn build(~self) -> Timeline<S,I> {
            let ~TimelineBuilder { initbpm: initbpm, objs: objs, endvpos: endvpos } = self;
            let initbpm = initbpm.expect("initial BPM should have been set");
            let mut objs = objs;
            ::extra::sort::tim_sort(objs);
            sanitize_objs(objs);
            let objs = precalculate_time(initbpm, objs, endvpos);
            Timeline { initbpm: initbpm, objs: objs }
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
            match (*obj).object_lane() {
                Some(lane) if !keep[*lane] => { obj.data = obj.data.to_effect(); }
                _ => {}
            }
        }
        timeline.objs.retain(|&obj| !obj.is_deleted());
    }

    /// Applies a mapping to the object lane if any. This is used to shuffle the lanes without
    /// modifying the relative time position.
    fn map_object_lane<S:Copy,I:Copy>(obj: &mut Obj<S,I>, map: &[Lane]) {
        match (*obj).object_lane() {
            Some(lane) => { *obj = (*obj).with_object_lane(map[*lane]); }
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
            if (*obj).is_lnstart() {
                let lane = (*obj).object_lane().get();
                match movable.position_elem(&lane) {
                    Some(i) => { movable.swap_remove(i); }
                    None => fail!(~"non-sanitized timeline")
                }
            }
            if lasttime < obj.loc.time { // reshuffle required
                lasttime = obj.loc.time; // XXX should we use this restriction on very quick notes?
                let shuffled = r.shuffle(movable);
                let assocs = vec::zip_slice(movable, shuffled); // XXX #3511
                for assocs.iter().advance |&(Lane(from), to)| {
                    map[from] = to;
                }
            }
            if (*obj).is_lnstart() {
                let lane = (*obj).object_lane().get();
                movable.push(lane);
            }
            map_object_lane(obj, map);
        }
    }
}

