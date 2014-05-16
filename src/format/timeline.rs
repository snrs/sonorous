// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Timeline interface.

use std::{fmt, io};
use format::obj::*;

/// A portion of game data which is not associated to resources and other metadata. Timelines are
/// immutable by design (except for `modf` module), and should be built by `TimelineBuilder`
/// in order to satisfy the invariant.
pub struct Timeline<SoundRef,ImageRef> {
    /// Initial BPM.
    pub initbpm: BPM,
    /// List of objects sorted by the position.
    pub objs: Vec<Obj<SoundRef,ImageRef>>,
}

/// Derived Timeline information.
pub struct TimelineInfo {
    /// The start position of the timeline. This is either -1.0 or 0.0 depending on the first
    /// measure has any visible objects or not.
    pub originoffset: f64,
    /// Set to true if the timeline has a BPM change.
    pub hasbpmchange: bool,
    /// Set to true if the timeline has long note objects.
    pub haslongnote: bool,
    /// The number of visible objects in the timeline. A long note object counts as one object.
    pub nnotes: uint,
    /// The maximum possible score.
    pub maxscore: int
}

impl<S:Clone,I:Clone> Timeline<S,I> {
    /// Returns the position of the last object in the chart.
    pub fn end(&self) -> ObjLoc<f64> {
        let last = self.objs.last().unwrap();
        assert!(last.data.is_end());
        last.loc.clone()
    }

    /// Similar to `self.end().time`, but also takes account of `sound_length` which should return
    /// the length of sound resources in seconds or 0.0. It also handles the non-zero origin (by
    /// `originoffset`) and the chart scrolling backwards.
    pub fn duration(&self, originoffset: f64, sound_length: |S| -> f64) -> f64 {
        assert!(originoffset <= 0.0);
        let mut maxtime = 0.0;
        for obj in self.objs.iter() {
            match obj.data {
                Visible(_,Some(ref sref)) | LNStart(_,Some(ref sref)) | BGM(ref sref) => {
                    let sndend = obj.loc.time + sound_length(sref.clone());
                    if maxtime < sndend { maxtime = sndend; }
                }
                SetBPM(BPM(0.0)) => {
                    return obj.loc.time;
                }
                SetBPM(BPM(newbpm)) if newbpm < 0.0 => {
                    // we explicitly ignore `self.end().time` since it will be +inf.
                    let backtime = BPM(-newbpm).measure_to_sec(obj.loc.pos - originoffset);
                    let backend = obj.loc.time + backtime;
                    return if maxtime > backend {maxtime} else {backend};
                }
                _ => {}
            }
        }
        // except for sounds past the end, we normally ends at the position of `End` object.
        if maxtime > self.end().time {maxtime} else {self.end().time}
    }

    /// Analyzes the timeline.
    pub fn analyze(&self) -> TimelineInfo {
        let mut infos = TimelineInfo { originoffset: 0.0, hasbpmchange: false, haslongnote: false,
                                       nnotes: 0, maxscore: 0 };

        for obj in self.objs.iter() {
            infos.haslongnote |= obj.is_lnstart();
            infos.hasbpmchange |= obj.is_setbpm();

            if obj.is_lnstart() || obj.is_visible() {
                infos.nnotes += 1;
                if obj.loc.time < 1.0 { infos.originoffset = -1.0; }
            }
        }

        for i in range(0, infos.nnotes) {
            let ratio = (i as f64) / (infos.nnotes as f64);
            infos.maxscore += (300.0 * (1.0 + ratio)) as int;
        }

        infos
    }
}

impl<S:fmt::Show,I:fmt::Show> Timeline<S,I> {
    /// Dumps the timeline to the writer for the debugging purpose.
    pub fn dump(&self, writer: &mut Writer) -> io::IoResult<()> {
        try!(writeln!(writer, "********  ********  ********  ********  SetBPM({})", *self.initbpm));
        for obj in self.objs.iter() {
            try!(writeln!(writer, "{:8.3}  {:8.3}  {:8.3}  {:8.3}  {}",
                          obj.loc.vpos, obj.loc.pos, obj.loc.vtime, obj.loc.time, obj.data));
        }
        Ok(())
    }
}

/// A timeline builder.
pub mod builder {
    use std::{f64, cmp};
    use format::obj::*;
    use super::Timeline;

    /// An object data with virtual position. Easier than a tuple to deal with.
    #[deriving(Clone)]
    pub struct ObjDataWithVpos<SoundRef,ImageRef> {
        pub vpos: f64,
        pub data: ObjData<SoundRef,ImageRef>
    }

    /// Converts objects to an integral "class". Normally objects have no ordering, but sometimes
    /// some kind of objects should be placed ahead of other kind of objects (e.g. `Stop` after
    /// `SetBPM`) so we need to define a custom ordering.
    fn classify<S,I>(obj: &ObjData<S,I>) -> int {
        match *obj {
            Deleted | StopEnd | End => -1, // should be removed
            SetMeasureFactor(..) | MeasureBar => 0,
            Visible(..) | Invisible(..) | LNStart(..) | LNDone(..) | Bomb(..) |
                BGM(..) | SetBGA(..) => 1,
            SetBPM(..) => 2,
            Stop(..) => 3,
        }
    }

    /// Sorts the objects with the position and internal ordering as per `classify`.
    fn sort_objs<S,I>(objs: &mut [ObjDataWithVpos<S,I>]) {
        objs.sort_by(|a, b| {
            let posord = if a.vpos < b.vpos {Less} else if a.vpos > b.vpos {Greater} else {Equal};
            cmp::lexical_ordering(posord, classify(&a.data).cmp(&classify(&b.data)))
        });
    }

    /// Fixes a problematic data.
    fn sanitize_objs<S:Clone,I:Clone>(objs: &mut [ObjDataWithVpos<S,I>]) {
        /// Abstracted sanitization algorithm. Basically, objects are categorized into several types
        /// and the algorithm removes or replaces objects which have the same or conflicting type
        /// in the same position. Conflicting types are specified by `merge_types` function.
        fn sanitize<S:Clone,I:Clone>(objs: &mut [ObjDataWithVpos<S,I>],
                                     to_type: |&ObjData<S,I>| -> Option<int>,
                                     merge_types: |int| -> int) {
            let len = objs.len();
            let mut i = 0;
            while i < len {
                let vpos = objs[i].vpos;
                let mut types = 0;
                let mut j = i;
                while j < len {
                    let obj = &mut objs[j];
                    if obj.vpos > vpos { break; }
                    for &t in to_type(&obj.data).iter() {
                        if (types & (1 << t)) != 0 {
                            // duplicate type
                            obj.data = obj.data.to_effect();
                        } else {
                            types |= 1 << t;
                        }
                    }
                    j += 1;
                }

                types = merge_types(types);

                while i < j {
                    let obj = &mut objs[i];
                    for &t in to_type(&obj.data).iter() {
                        if (types & (1 << t)) == 0 {
                            obj.data = obj.data.to_effect();
                        }
                    }
                    i += 1;
                }
            }
        }

        // notes are processed in the per-lane basis (as we need to keep the internal status).
        for lane in range(0, NLANES) {
            let lane0 = Lane(lane);

            static LNDONE: int = 0;
            static LNSTART: int = 1;
            static VISIBLE: int = 2;
            static INVISIBLE: int = 3;
            static BOMB: int = 4;
            let to_type = |obj: &ObjData<S,I>| -> Option<int> {
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
            sanitize(objs, |obj| to_type(obj), |mut types| {
                static LNMASK: int = (1 << LNSTART) | (1 << LNDONE);

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
            });

            if inside {
                // remove last starting longnote which is unfinished
                match objs.iter().rposition(|obj| to_type(&obj.data).is_some()) {
                    Some(index) => {
                        let obj = &mut objs[index];
                        if obj.data.is_lnstart() {
                            obj.data = obj.data.to_effect();
                        }
                    }
                    None => {}
                }
            }
        }

        // object-like effects (except for BGMs) are simpler, any conflicting objects are removed.
        sanitize(objs,
                 |obj| match *obj {
                           SetBGA(Layer1,_) => Some(0),
                           SetBGA(Layer2,_) => Some(1),
                           SetBGA(Layer3,_) => Some(2),
                           SetBGA(PoorBGA,_) => Some(3),
                           SetBPM(..) => Some(4),
                           Stop(..) => Some(5),
                           _ => None,
                       },
                 |types| types);
    }

    /// Derives other three axes from the virtual position.
    fn precalculate_time<S:Clone,I:Clone>(initbpm: BPM, objs: &[ObjDataWithVpos<S,I>],
                                          endvpos: f64) -> Vec<Obj<S,I>> {
        let mut ret = Vec::new();

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

        for obj in objs.iter() {
            // these objects should not be inserted manually
            if classify(&obj.data) < 0 { continue; }

            let vpos = obj.vpos;
            let pos = (vpos - shorten_vpos) * shorten + shorten_pos;
            let vtime = bpm.measure_to_sec(pos - bpm_pos) + bpm_vtime;
            let time = vtime + stop_delay;

            let loc = ObjLoc { vpos: vpos, pos: pos, vtime: vtime, time: time };
            ret.push(Obj { loc: loc, data: obj.data.clone() });

            match obj.data {
                SetBPM(BPM(newbpm)) => {
                    if newbpm > 0.0 {
                        bpm = BPM(newbpm);
                        bpm_pos = pos;
                        bpm_vtime = vtime;
                    } else {
                        // the chart scrolls backwards or terminates immediately,
                        // vtime and time should be +inf from now on.
                        bpm_vtime = f64::INFINITY;
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
        assert!(ret.is_empty() || ret.last().unwrap().loc.vpos < endvpos);
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
        pub initbpm: Option<BPM>,
        /// Same as `Timeline::objs` but not yet sorted and only has a virtual position.
        pub objs: Vec<ObjDataWithVpos<SoundRef,ImageRef>>,
        /// End position. Every object must have the virtual position less than this value.
        pub endvpos: f64,
    }

    /// A mark to the existing object. The caller is recommended not to directly mutate the builder
    /// contents and to use this and `TimelineBuilder::{mutate,delete}` methods.
    pub struct Mark(uint);

    impl<S:Eq+Clone,I:Eq+Clone> TimelineBuilder<S,I> {
        /// Creates a new timeline builder.
        pub fn new() -> TimelineBuilder<S,I> {
            TimelineBuilder { initbpm: None, objs: Vec::new(), endvpos: 0.0 }
        }

        /// Sets an initial BPM.
        pub fn set_initbpm(&mut self, bpm: BPM) {
            self.initbpm = Some(bpm); 
        }

        /// Sets the end position.
        pub fn set_end(&mut self, vpos: f64) {
            assert!(vpos >= 0.0);
            self.endvpos = vpos;
        }

        /// Adds an object at given position.
        pub fn add(&mut self, vpos: f64, data: ObjData<S,I>) {
            assert!(vpos >= 0.0);
            self.objs.push(ObjDataWithVpos { vpos: vpos, data: data });
        }

        /// Adds an object at given position and returns a mark to that object for later usage.
        pub fn add_and_mark(&mut self, vpos: f64, data: ObjData<S,I>) -> Mark {
            assert!(vpos >= 0.0);
            let index = self.objs.len();
            self.objs.push(ObjDataWithVpos { vpos: vpos, data: data });
            Mark(index)
        }

        /// Mutates an existing object.
        pub fn mutate(&mut self, mark: Mark, f: |f64,ObjData<S,I>| -> (f64,ObjData<S,I>)) {
            let Mark(mark) = mark;
            assert!(mark < self.objs.len());
            let obj = &mut self.objs.as_mut_slice()[mark];
            let (vpos, data) = f(obj.vpos, obj.data.clone()); // XXX can we just move the data?
            assert!(vpos >= 0.0);
            obj.vpos = vpos;
            obj.data = data;
        }

        /// Deletes an existing object (or rather, marks it as deleted).
        pub fn delete(&mut self, mark: Mark) {
            self.mutate(mark, |vpos,_data| (vpos, Deleted));
        }

        /// Builds an actual timeline.
        pub fn build(self) -> Timeline<S,I> {
            let TimelineBuilder { initbpm: initbpm, objs: objs, endvpos: endvpos } = self;
            let initbpm = initbpm.expect("initial BPM should have been set");
            let mut objs = objs;
            sort_objs(objs.as_mut_slice());
            sanitize_objs(objs.as_mut_slice());
            let objs = precalculate_time(initbpm, objs.as_slice(), endvpos);
            Timeline { initbpm: initbpm, objs: objs }
        }
    }
}

/// Modifiers available to timelines. They are safe to run through the existing timeline, as long as
/// the original timeline is no longer used.
pub mod modf {
    use std::f64;
    use rand::Rng;
    use format::obj::*;
    use super::Timeline;

    /// Removes objects not in given lanes by replacing them to BGMs.
    pub fn filter_lanes<S:Clone,I:Clone>(timeline: &mut Timeline<S,I>, lanes: &[Lane]) {
        let mut keep = Vec::from_elem(NLANES, false);
        for &Lane(lane) in lanes.iter() {
            keep.as_mut_slice()[lane] = true;
        }

        for obj in timeline.objs.mut_iter() {
            match (*obj).object_lane() {
                Some(lane) if !keep.as_slice()[*lane] => { obj.data = obj.data.to_effect(); }
                _ => {}
            }
        }
        timeline.objs.retain(|obj| !obj.is_deleted());
    }

    /// Applies a mapping to the object lane if any. This is used to shuffle the lanes without
    /// modifying the relative time position.
    fn map_object_lane<S:Clone,I:Clone>(obj: &mut Obj<S,I>, map: &[Lane]) {
        match (*obj).object_lane() {
            Some(lane) => { *obj = (*obj).with_object_lane(map[*lane]); }
            None => {}
        }
    }

    /// Swaps given lanes in the reverse order.
    pub fn mirror<S:Clone,I:Clone>(timeline: &mut Timeline<S,I>, lanes: &[Lane]) {
        let mut map = Vec::from_fn(NLANES, |lane| Lane(lane));
        for (&Lane(from), &to) in lanes.iter().zip(lanes.iter().rev()) {
            map.as_mut_slice()[from] = to;
        }

        for obj in timeline.objs.mut_iter() {
            map_object_lane(obj, map.as_slice());
        }
    }

    /// Swaps given lanes in the random order.
    pub fn shuffle<S:Clone,I:Clone,R:Rng>(timeline: &mut Timeline<S,I>, r: &mut R, lanes: &[Lane]) {
        let mut shuffled = Vec::from_slice(lanes);
        r.shuffle(shuffled.as_mut_slice());
        let mut map = Vec::from_fn(NLANES, |lane| Lane(lane));
        for (&Lane(from), &to) in lanes.iter().zip(shuffled.iter()) {
            map.as_mut_slice()[from] = to;
        }

        for obj in timeline.objs.mut_iter() {
            map_object_lane(obj, map.as_slice());
        }
    }

    /// Swaps given lanes in the random order, where the order is determined per object. It does not
    /// cause objects to move within another LN object, or place two objects in the same or very
    /// close time position to the same lane.
    pub fn randomize<S:Clone,I:Clone,R:Rng>(timeline: &mut Timeline<S,I>, r: &mut R,
                                            lanes: &[Lane]) {
        let mut movable = Vec::from_slice(lanes);
        let mut map = Vec::from_fn(NLANES, |lane| Lane(lane));

        let mut lasttime = f64::NEG_INFINITY;
        for obj in timeline.objs.mut_iter() {
            if (*obj).is_lnstart() {
                let lane = (*obj).object_lane().unwrap();
                match movable.iter().position(|&i| i == lane) {
                    Some(i) => { movable.swap_remove(i); }
                    None => fail!("non-sanitized timeline")
                }
            }
            if lasttime < obj.loc.time { // reshuffle required
                lasttime = obj.loc.time; // XXX should we use this restriction on very quick notes?
                let mut shuffled = movable.clone();
                r.shuffle(shuffled.as_mut_slice());
                for (&Lane(from), &to) in movable.iter().zip(shuffled.iter()) {
                    map.as_mut_slice()[from] = to;
                }
            }
            if (*obj).is_lnstart() {
                let lane = (*obj).object_lane().unwrap();
                movable.push(lane);
            }
            map_object_lane(obj, map.as_slice());
        }
    }
}

