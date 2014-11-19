// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Game play elements ("objects") and object-like effects.

use std::fmt;

pub use self::ObjData::{Deleted, Visible, Invisible, LNStart, LNDone, Bomb};
pub use self::ObjData::{BGM, SetBGA, SetBPM, Stop, StopEnd, SetMeasureFactor, MeasureBar, End};

/// A game play element mapped to the single input element (for example, button) and the screen
/// area (henceforth "lane").
#[deriving(PartialEq,Eq,Clone)]
pub struct Lane(pub uint);

/// The maximum number of lanes.
pub const NLANES: uint = 72;

impl Deref<uint> for Lane {
    fn deref<'a>(&'a self) -> &'a uint {
        let Lane(ref v) = *self;
        v
    }
}

/// BGA layers.
#[deriving(PartialEq,Eq,Show,Clone)]
pub enum BGALayer {
    /// The lowest layer. BMS channel #04.
    Layer1 = 0,
    /// The middle layer. BMS channel #07.
    Layer2 = 1,
    /// The highest layer. BMS channel #0A.
    Layer3 = 2,
    /// The layer only displayed shortly after the MISS grade. It is technically not over
    /// `Layer3`, but several extensions to BMS assumes it. BMS channel #06.
    PoorBGA = 3
}

/// The number of BGA layers.
pub const NLAYERS: uint = 4;

/// Beats per minute. Used as a conversion factor between the time position and actual time
/// in BMS.
#[deriving(PartialEq,Show,Clone)]
pub struct BPM(pub f64);

impl Deref<f64> for BPM {
    fn deref<'a>(&'a self) -> &'a f64 {
        let BPM(ref v) = *self;
        v
    }
}

impl BPM {
    /// Converts a measure to a second.
    pub fn measure_to_sec(&self, measure: f64) -> f64 { measure * 240.0 / **self }

    /// Converts a second to a measure.
    pub fn sec_to_measure(&self, sec: f64) -> f64 { sec * **self / 240.0 }
}

/// A duration from the particular point. It may be specified in measures or seconds. Used in
/// the `Stop` object.
#[deriving(PartialEq,Show,Clone)]
pub enum Duration { Seconds(f64), Measures(f64) }

impl Duration {
    /// Returns the sign of the duration.
    pub fn sign(&self) -> int {
        let v = match *self {
            Duration::Seconds(secs) => secs,
            Duration::Measures(measures) => measures
        };
        if v < 0.0 {-1} else if v > 0.0 {1} else {0}
    }

    /// Calculates the actual seconds from the current BPM.
    pub fn to_sec(&self, bpm: BPM) -> f64 {
        match *self {
            Duration::Seconds(secs) => secs,
            Duration::Measures(measures) => bpm.measure_to_sec(measures)
        }
    }
}

/// A damage value upon the MISS grade. Normally it is specified in percents of the full gauge
/// (as in `MAXGAUGE`), but sometimes it may cause an instant death. Used in the `Bomb` object
/// (normal note objects have a fixed value).
#[deriving(PartialEq,Show,Clone)]
pub enum Damage { Gauge(f64), InstantDeath }

/**
 * A slice of the image. BMS #BGA command can override the existing #BMP command with this slice.
 *
 * Blitting occurs from the region `(sx,sy)-(sx+w,sy+h)` in the source surface to the region
 * `(dx,dy)-(dx+w,dy+h)` in the screen. The rectangular region contains the upper-left corner
 * but not the lower-right corner. The region is clipped to make the upper-left corner has
 * non-negative coordinates and the size of the region doesn't exceed the canvas dimension.
 */
#[deriving(PartialEq,Show,Clone)]
pub struct ImageSlice {
    pub sx: int, pub sy: int, pub dx: int, pub dy: int, pub w: int, pub h: int,
}

/// A reference to the BGA target, i.e. something that can be displayed in a single BGA layer.
#[deriving(PartialEq,Show,Clone)]
pub enum BGARef<ImageRef> {
    /// Fully transparent image.
    Blank,
    /// Static image.
    Image(ImageRef),
    /// A portion of static image.
    SlicedImage(ImageRef, Box<ImageSlice>),
}

impl<I> BGARef<I> {
    /// Returns a reference to the underlying image resource if any.
    pub fn as_image_ref<'r>(&'r self) -> Option<&'r I> {
        match *self {
            BGARef::Blank => None,
            BGARef::Image(ref iref) | BGARef::SlicedImage(ref iref, _) => Some(iref)
        }
    }
}

/// A data for objects (or object-like effects). Does not include the time information.
#[deriving(PartialEq,Clone)]
pub enum ObjData<SoundRef,ImageRef> {
    /// Deleted object. Only used during various processing.
    Deleted,
    /// Visible object. Sound is played when the key is input inside the associated grading area.
    Visible(Lane, Option<SoundRef>),
    /// Invisible object. Sound is played when the key is input inside the associated grading
    /// area. No render nor grading performed.
    Invisible(Lane, Option<SoundRef>),
    /// Start of long note (LN). Sound is played when the key is down inside the associated
    /// grading area.
    LNStart(Lane, Option<SoundRef>),
    /// End of LN. Sound is played when the start of LN is graded, the key was down and now up
    /// inside the associated grading area.
    LNDone(Lane, Option<SoundRef>),
    /// Bomb. Pressing the key down at the moment that the object is on time causes
    /// the specified damage; sound is played in this case. No associated grading area.
    Bomb(Lane, Option<SoundRef>, Damage),
    /// Plays associated sound.
    BGM(SoundRef),
    /**
     * Sets the virtual BGA layer to given image (or anything available in `BGA`).
     * The layer itself may not be displayed depending on the current game status.
     *
     * If the reference points to a movie, the movie starts playing; if the other layer had
     * the same movie started, it rewinds to the beginning. The resulting image from the movie
     * can be shared among multiple layers.
     */
    SetBGA(BGALayer, BGARef<ImageRef>),
    /// Sets the BPM. Negative BPM causes the chart scrolls backwards. Zero BPM causes the chart
    /// immediately terminates. In both cases, the chart is considered unfinished if there are
    /// remaining gradable objects.
    SetBPM(BPM),
    /// Stops the scroll of the chart for given duration ("scroll stopper" hereafter). The duration,
    /// if specified in measures, is not affected by the measure scaling factor.
    Stop(Duration),
    /// Restarts the scroll of the chart. This object is a no-op, but it is used to keep
    /// the linear relation between time and position axes.
    StopEnd,
    /// Sets the measure scaling factor, which is a ratio of the interval in the virtual position
    /// (e.g. as specified by the BMS creators) and the interval in the actual position.
    /// This can be ignored for the game play, but we still keep this relation since the virtual
    /// position is often directly used to refer certain point in the chart.
    SetMeasureFactor(f64),
    /// Start of the measure, where the measure bar is drawn. This is derived from
    /// `SetMeasureFactor` but made into the separate object as an optimization.
    MeasureBar,
    /// Marks the logical end of the chart. This is also useful to extend the chart without
    /// inserting any dummy object after the end of the song. This object is otherwise a no-op,
    /// but it should be the last object in the chart and should be placed in the different
    /// position from the next-to-last object (see `format::pointer` for rationale).
    End,
}

impl<S:fmt::Show,I:fmt::Show> fmt::Show for ObjData<S,I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        #![allow(non_camel_case_types)]

        struct fmt_lane(Lane);
        impl fmt::Show for fmt_lane {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let fmt_lane(lane) = *self;
                write!(f, "{}:{:02}", *lane / 36 + 1, *lane % 36)
            }
        }

        struct fmt_or_default<'r, T:'r>(&'r Option<T>, &'r str);
        impl<'r,T:fmt::Show> fmt::Show for fmt_or_default<'r,T> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    fmt_or_default(&Some(ref v), _) => v.fmt(f),
                    fmt_or_default(&None, default) => default.fmt(f),
                }
            }
        }

        match *self {
            Deleted => write!(f, "Deleted"),
            Visible(lane, ref sref) =>
                write!(f, "Visible({},{})", fmt_lane(lane), fmt_or_default(sref, "--")),
            Invisible(lane, ref sref) =>
                write!(f, "Invisible({},{})", fmt_lane(lane), fmt_or_default(sref, "--")),
            LNStart(lane, ref sref) =>
                write!(f, "LNStart({},{})", fmt_lane(lane), fmt_or_default(sref, "--")),
            LNDone(lane, ref sref) =>
                write!(f, "LNDone({},{})", fmt_lane(lane), fmt_or_default(sref, "--")),
            Bomb(lane, ref sref, damage) =>
                write!(f, "Bomb({},{},{})", fmt_lane(lane), fmt_or_default(sref, "--"), damage),
            BGM(ref sref) =>
                write!(f, "BGM({})", sref),
            SetBGA(layer, BGARef::Blank) =>
                write!(f, "SetBGA({},--)", layer),
            SetBGA(layer, BGARef::Image(ref iref)) =>
                write!(f, "SetBGA({},{})", layer, iref),
            SetBGA(layer, BGARef::SlicedImage(ref iref, ref slice)) =>
                write!(f, "SetBGA({},{}:{}+{}+{}x{}:{}+{}+{}x{})",
                          layer, iref, slice.sx, slice.sy, slice.w, slice.h,
                          slice.dx, slice.dy, slice.w, slice.h),
            SetBPM(BPM(bpm)) =>
                write!(f, "SetBPM({})", bpm),
            Stop(Duration::Seconds(secs)) =>
                write!(f, "Stop({}s)", secs),
            Stop(Duration::Measures(measures)) =>
                write!(f, "Stop({})", measures),
            StopEnd =>
                write!(f, "StopEnd"),
            SetMeasureFactor(factor) =>
                write!(f, "SetMeasureFactor({})", factor),
            MeasureBar =>
                write!(f, "MeasureBar"),
            End =>
                write!(f, "End"),
        }
    }
}

/// Any type that contains `ObjData`.
pub trait ToObjData<SoundRef:Clone,ImageRef:Clone> {
    fn to_obj_data(&self) -> ObjData<SoundRef,ImageRef>;
}

/// Any type that contains and can individually update `ObjData`.
pub trait WithObjData<SoundRef:Clone,ImageRef:Clone>: ToObjData<SoundRef,ImageRef> {
    fn with_obj_data(&self, data: ObjData<SoundRef,ImageRef>) -> Self;
}

impl<S:Clone,I:Clone> ToObjData<S,I> for ObjData<S,I> {
    fn to_obj_data(&self) -> ObjData<S,I> { self.clone() }
}

impl<S:Clone,I:Clone> WithObjData<S,I> for ObjData<S,I> {
    fn with_obj_data(&self, data: ObjData<S,I>) -> ObjData<S,I> { data }
}

/// Query operations for objects. Implicitly defined in terms of `ToObjData` trait.
pub trait ObjQueryOps<SoundRef:Clone,ImageRef:Clone> {
    /// Returns true if the object is deleted (`Deleted`).
    fn is_deleted(&self) -> bool;
    /// Returns true if the object is a visible object (`Visible`).
    fn is_visible(&self) -> bool;
    /// Returns true if the object is an invisible object (`Invisible`).
    fn is_invisible(&self) -> bool;
    /// Returns true if the object is a start of LN object (`LNStart`).
    fn is_lnstart(&self) -> bool;
    /// Returns true if the object is an end of LN object (`LNDone`).
    fn is_lndone(&self) -> bool;
    /// Returns true if the object is either a start or an end of LN object.
    fn is_ln(&self) -> bool;
    /// Returns true if the object is a bomb (`Bomb`).
    fn is_bomb(&self) -> bool;
    /// Returns true if the object is soundable when it is the closest soundable object from
    /// the current position and the player pressed the key. Named "soundable" since it may
    /// choose not to play the associated sound. Note that not every object with sound is soundable.
    fn is_soundable(&self) -> bool;
    /// Returns true if the object is subject to grading.
    fn is_gradable(&self) -> bool;
    /// Returns true if the object has a visible representation.
    fn is_renderable(&self) -> bool;
    /// Returns true if the data is an object.
    fn is_object(&self) -> bool;
    /// Returns true if the data is a BGM.
    fn is_bgm(&self) -> bool;
    /// Returns true if the data is a BGA.
    fn is_setbga(&self) -> bool;
    /// Returns true if the data is a BPM change.
    fn is_setbpm(&self) -> bool;
    /// Returns true if the data is a scroll stopper.
    fn is_stop(&self) -> bool;
    /// Returns true if the data is the end of a scroll stopper.
    fn is_stopend(&self) -> bool;
    /// Returns true if the data is a change in the measure scaling factor.
    fn is_setmeasurefactor(&self) -> bool;
    /// Returns true if the data is a measure bar.
    fn is_measurebar(&self) -> bool;
    /// Returns true if the data is an end mark.
    fn is_end(&self) -> bool;

    /// Returns an associated lane if the data is an object.
    fn object_lane(&self) -> Option<Lane>;
    /// Returns all sounds associated to the data.
    fn sounds(&self) -> Vec<SoundRef>;
    /// Returns all sounds played when key is pressed.
    fn keydown_sound(&self) -> Option<SoundRef>;
    /// Returns all sounds played when key is unpressed.
    fn keyup_sound(&self) -> Option<SoundRef>;
    /// Returns all sounds played when the object is activated while the corresponding key is
    /// currently pressed. Bombs are the only instance of this kind of sounds.
    fn through_sound(&self) -> Option<SoundRef>;
    /// Returns all images associated to the data.
    fn images(&self) -> Vec<ImageRef>;
    /// Returns an associated damage value when the object is activated.
    fn through_damage(&self) -> Option<Damage>;
}

/// Conversion operations for objects. Implicitly defined in terms of `WithObjData` trait.
pub trait ObjConvOps<SoundRef:Clone,ImageRef:Clone>: ObjQueryOps<SoundRef,ImageRef> {
    /// Returns a visible object with the same time, lane and sound as given object.
    fn to_visible(&self) -> Self;
    /// Returns an invisible object with the same time, lane and sound as given object.
    fn to_invisible(&self) -> Self;
    /// Returns a start of LN object with the same time, lane and sound as given object.
    fn to_lnstart(&self) -> Self;
    /// Returns an end of LN object with the same time, lane and sound as given object.
    fn to_lndone(&self) -> Self;
    /// Returns a non-object version of given object. May return `Deleted` if it should be deleted.
    fn to_effect(&self) -> Self;
    /// Returns an object with lane replaced with given lane. No effect on object-like effects.
    fn with_object_lane(&self, lane: Lane) -> Self;
}

impl<S:Clone,I:Clone,T:ToObjData<S,I>> ObjQueryOps<S,I> for T {
    fn is_deleted(&self) -> bool {
        match self.to_obj_data() { Deleted => true, _ => false }
    }

    fn is_visible(&self) -> bool {
        match self.to_obj_data() { Visible(..) => true, _ => false }
    }

    fn is_invisible(&self) -> bool {
        match self.to_obj_data() { Invisible(..) => true, _ => false }
    }

    fn is_lnstart(&self) -> bool {
        match self.to_obj_data() { LNStart(..) => true, _ => false }
    }

    fn is_lndone(&self) -> bool {
        match self.to_obj_data() { LNDone(..) => true, _ => false }
    }

    fn is_ln(&self) -> bool {
        match self.to_obj_data() { LNStart(..) | LNDone(..) => true, _ => false }
    }

    fn is_bomb(&self) -> bool {
        match self.to_obj_data() { Bomb(..) => true, _ => false }
    }

    fn is_soundable(&self) -> bool {
        match self.to_obj_data() {
            Visible(..) | Invisible(..) | LNStart(..) | LNDone(..) => true,
            _ => false
        }
    }

    fn is_gradable(&self) -> bool {
        match self.to_obj_data() {
            Visible(..) | LNStart(..) | LNDone(..) => true,
            _ => false
        }
    }

    fn is_renderable(&self) -> bool {
        match self.to_obj_data() {
            Visible(..) | LNStart(..) | LNDone(..) | Bomb(..) => true,
            _ => false
        }
    }

    fn is_object(&self) -> bool {
        match self.to_obj_data() {
            Visible(..) | Invisible(..) | LNStart(..) | LNDone(..) | Bomb(..) => true,
            _ => false
        }
    }

    fn is_bgm(&self) -> bool {
        match self.to_obj_data() { BGM(..) => true, _ => false }
    }

    fn is_setbga(&self) -> bool {
        match self.to_obj_data() { SetBGA(..) => true, _ => false }
    }

    fn is_setbpm(&self) -> bool {
        match self.to_obj_data() { SetBPM(..) => true, _ => false }
    }

    fn is_stop(&self) -> bool {
        match self.to_obj_data() { Stop(..) => true, _ => false }
    }

    fn is_stopend(&self) -> bool {
        match self.to_obj_data() { StopEnd => true, _ => false }
    }

    fn is_setmeasurefactor(&self) -> bool {
        match self.to_obj_data() { SetMeasureFactor(..) => true, _ => false }
    }

    fn is_measurebar(&self) -> bool {
        match self.to_obj_data() { MeasureBar => true, _ => false }
    }

    fn is_end(&self) -> bool {
        match self.to_obj_data() { End => true, _ => false }
    }

    fn object_lane(&self) -> Option<Lane> {
        match self.to_obj_data() {
            Visible(lane,_) | Invisible(lane,_) | LNStart(lane,_) |
            LNDone(lane,_) | Bomb(lane,_,_) => Some(lane),
            _ => None
        }
    }

    fn sounds(&self) -> Vec<S> {
        match self.to_obj_data() {
            Visible(_,Some(ref sref)) |
            Invisible(_,Some(ref sref)) |
            LNStart(_,Some(ref sref)) |
            LNDone(_,Some(ref sref)) |
            Bomb(_,Some(ref sref),_) |
            BGM(ref sref) => vec![sref.clone()],
            _ => Vec::new()
        }
    }

    fn keydown_sound(&self) -> Option<S> {
        match self.to_obj_data() {
            Visible(_,ref sref) | Invisible(_,ref sref) | LNStart(_,ref sref) => sref.clone(),
            _ => None
        }
    }

    fn keyup_sound(&self) -> Option<S> {
        match self.to_obj_data() { LNDone(_,ref sref) => sref.clone(), _ => None }
    }

    fn through_sound(&self) -> Option<S> {
        match self.to_obj_data() { Bomb(_,ref sref,_) => sref.clone(), _ => None }
    }

    fn images(&self) -> Vec<I> {
        match self.to_obj_data() {
            SetBGA(_,BGARef::Image(ref iref)) |
            SetBGA(_,BGARef::SlicedImage(ref iref,_)) => vec![iref.clone()],
            _ => Vec::new()
        }
    }

    fn through_damage(&self) -> Option<Damage> {
        match self.to_obj_data() { Bomb(_,_,damage) => Some(damage), _ => None }
    }
}

impl<S:Clone,I:Clone,T:WithObjData<S,I>+Clone> ObjConvOps<S,I> for T {
    fn to_visible(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => Visible(lane,snd.clone()),
            _ => panic!("to_visible for non-object")
        };
        self.with_obj_data(data)
    }

    fn to_invisible(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => Invisible(lane,snd.clone()),
            _ => panic!("to_invisible for non-object")
        };
        self.with_obj_data(data)
    }

    fn to_lnstart(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => LNStart(lane,snd.clone()),
            _ => panic!("to_lnstart for non-object")
        };
        self.with_obj_data(data)
    }

    fn to_lndone(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => LNDone(lane,snd.clone()),
            _ => panic!("to_lndone for non-object")
        };
        self.with_obj_data(data)
    }

    fn to_effect(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(_,Some(ref snd)) | Invisible(_,Some(ref snd)) |
            LNStart(_,Some(ref snd)) | LNDone(_,Some(ref snd)) => BGM(snd.clone()),
            Visible(_,None) | Invisible(_,None) |
            LNStart(_,None) | LNDone(_,None) | Bomb(_,_,_) => Deleted,
            _ => { return self.clone(); }
        };
        self.with_obj_data(data)
    }

    fn with_object_lane(&self, lane: Lane) -> T {
        let data = match self.to_obj_data() {
            Visible(_,ref snd) => Visible(lane,snd.clone()),
            Invisible(_,ref snd) => Invisible(lane,snd.clone()),
            LNStart(_,ref snd) => LNStart(lane,snd.clone()),
            LNDone(_,ref snd) => LNDone(lane,snd.clone()),
            Bomb(_,ref snd,damage) => Bomb(lane,snd.clone(),damage),
            _ => { return self.clone(); }
        };
        self.with_obj_data(data)
    }
}

/// Axes available to the objects. See `Obj` for more information.
#[deriving(PartialEq,Eq,Show,Clone)]
pub enum ObjAxis {
    /// Virtual position.
    VirtualPos  = 0,
    /// Actual position.
    ActualPos   = 1,
    /// Virtual time.
    VirtualTime = 2,
    /// Actual time.
    ActualTime  = 3,
}

/// Object location per axis.
#[deriving(PartialEq,Show,Clone)]
pub struct ObjLoc<T> {
    /// Virtual position in measures.
    pub vpos: T,
    /// Actual position in measures.
    pub pos: T,
    /// Virtual time in seconds. Can be a positive infinity if the chart scrolls backwards prior to
    /// this object and this object should not be graded.
    pub vtime: T,
    /// Actual time in seconds. Can be a positive infinity if the chart scrolls backwards prior to
    /// this object and this object should not be activated.
    pub time: T,
}

impl<T:Clone+PartialOrd> PartialOrd for ObjLoc<T> {
    fn partial_cmp(&self, other: &ObjLoc<T>) -> Option<Ordering> {
        self.time.partial_cmp(&other.time)
    }
}

impl<T:Clone> Index<ObjAxis,T> for ObjLoc<T> {
    fn index<'a>(&'a self, axis: &ObjAxis) -> &'a T {
        match *axis { ObjAxis::VirtualPos  => &self.vpos,  ObjAxis::ActualPos  => &self.pos,
                      ObjAxis::VirtualTime => &self.vtime, ObjAxis::ActualTime => &self.time }
    }
}

/**
 * An object with precalculated position and time information.
 *
 * Sonorous has four distinct axes: virtual position, actual position, virtual time and actual time.
 * Positions have a unit of measures, times have a unit of seconds. Specifically:
 *
 * - Virtual position is what the chart file originally specified, and also what the player and
 *   creator actually perceive as the "measure". This is used purely for user convenience.
 * - Actual position is a position after the measure scaling factor is applied. It linearly relates
 *   to the relative position of game elements (the proportional factor being the "play speed").
 * - Virtual time is related to the actual position by BPM. The grading procedure uses a distance
 *   between the gradable object's virtual time and the current virtual time (and so-called "grading
 *   area" is also defined in terms of virtual time) so the rapid change of BPM does not affect
 *   the grading. Virtual time literally *stops* when `Stop` is activated, so objects close to
 *   `Stop` have natural grading areas based on the chart appearance.
 * - Actual time is when the object is actually activated (played or overlapped with the grading
 *   line), and related to the virtual time by `Stop` objects.
 *
 * First three axes can *stop* while the actual time progresses. Axes are linearly related between
 * consecutive objects. The proportional factor should be non-negative; `SetBPM` with a negative
 * BPM seems to be an exception, but it actually makes the proportional factor infinite.
 *
 * The following table illustrates various situations possible with this model.
 *
 * ~~~~ {.notrust}
 * vpos    pos     vtime   time    data
 * ------  ------  ------  ------  ----------------
 * -inf    -inf    -inf    -inf    SetBPM(120) (not actual object, derived from `timeline.initbpm`)
 * 0.00    0.00    0.00    0.00    MeasureBar
 * 1.00    1.00    2.00    2.00    MeasureBar, SetMeasureFactor(0.5)
 * 2.00    1.50    3.00    3.00    MeasureBar, SetMeasureFactor(1)
 * 2.50    2.00    4.00    4.00    SetBPM(240)
 * 3.00    2.50    4.50    4.50    MeasureBar, SetBPM(24000)
 * 4.00    3.50    4.51    4.51    MeasureBar, Stop(Seconds(1.48))
 * 4.00    3.50    4.51    5.99    StopEnd
 * 5.00    4.50    4.52    6.00    MeasureBar, SetBPM(-120)
 * 6.00    5.50    +inf    +inf    MeasureBar
 * ~~~~
 */
#[deriving(Show,Clone)]
pub struct Obj<SoundRef,ImageRef> {
    /// Object location.
    pub loc: ObjLoc<f64>,
    /// Associated object data.
    pub data: ObjData<SoundRef,ImageRef>
}

impl<S:Clone,I:Clone> PartialEq for Obj<S,I> {
    fn eq(&self, other: &Obj<S,I>) -> bool { self.loc == other.loc }
    fn ne(&self, other: &Obj<S,I>) -> bool { self.loc != other.loc }
}

impl<S:Clone,I:Clone> PartialOrd for Obj<S,I> {
    fn partial_cmp(&self, other: &Obj<S,I>) -> Option<Ordering> {
        self.loc.partial_cmp(&other.loc)
    }
}

impl<S:Clone,I:Clone> ToObjData<S,I> for Obj<S,I> {
    fn to_obj_data(&self) -> ObjData<S,I> { self.data.clone() }
}

impl<S:Clone,I:Clone> WithObjData<S,I> for Obj<S,I> {
    fn with_obj_data(&self, data: ObjData<S,I>) -> Obj<S,I> {
        Obj { loc: self.loc.clone(), data: data }
    }
}

