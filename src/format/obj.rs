// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Game play elements ("objects") and object-like effects.

/// A game play element mapped to the single input element (for example, button) and the screen
/// area (henceforth "lane").
#[deriving(Eq)]
pub struct Lane(uint);

/// The maximum number of lanes. (C: `NNOTECHANS`)
pub static NLANES: uint = 72;

/// BGA layers. (C: `enum BGA_type`)
#[deriving(Eq,ToStr)]
pub enum BGALayer {
    /// The lowest layer. BMS channel #04. (C: `BGA_LAYER`)
    Layer1 = 0,
    /// The middle layer. BMS channel #07. (C: `BGA2_LAYER`)
    Layer2 = 1,
    /// The highest layer. BMS channel #0A. (C: `BGA3_LAYER`)
    Layer3 = 2,
    /// The layer only displayed shortly after the MISS grade. It is technically not over
    /// `Layer3`, but several extensions to BMS assumes it. BMS channel #06.
    /// (C: `POORBGA_LAYER`)
    PoorBGA = 3
}

/// The number of BGA layers.
pub static NLAYERS: uint = 4;

/// Beats per minute. Used as a conversion factor between the time position and actual time
/// in BMS.
#[deriving(Eq,ToStr)]
pub struct BPM(float);

impl BPM {
    /// Converts a measure to a millisecond. (C: `MEASURE_TO_MSEC`)
    pub fn measure_to_msec(self, measure: float) -> float { measure * 240000.0 / *self }

    /// Converts a millisecond to a measure. (C: `MSEC_TO_MEASURE`)
    pub fn msec_to_measure(self, msec: float) -> float { msec * *self / 240000.0 }
}

/// A duration from the particular point. It may be specified in measures or seconds. Used in
/// the `Stop` object.
#[deriving(Eq,ToStr)]
pub enum Duration { Seconds(float), Measures(float) }

impl Duration {
    /// Calculates the actual milliseconds from the current BPM.
    pub fn to_msec(&self, bpm: BPM) -> float {
        match *self {
            Seconds(secs) => secs * 1000.0,
            Measures(measures) => bpm.measure_to_msec(measures)
        }
    }
}

/// A damage value upon the MISS grade. Normally it is specified in percents of the full gauge
/// (as in `MAXGAUGE`), but sometimes it may cause an instant death. Used in the `Bomb` object
/// (normal note objects have a fixed value).
#[deriving(Eq,ToStr)]
pub enum Damage { GaugeDamage(float), InstantDeath }

/// A data for objects (or object-like effects). Does not include the time information.
#[deriving(Eq)]
pub enum ObjData<SoundRef,ImageRef> {
    /// Deleted object. Only used during various processing.
    Deleted,
    /// Visible object. Sound is played when the key is input inside the associated grading
    /// area. (C: `NOTE`)
    Visible(Lane, Option<SoundRef>),
    /// Invisible object. Sound is played when the key is input inside the associated grading
    /// area. No render nor grading performed. (C: `INVNOTE`)
    Invisible(Lane, Option<SoundRef>),
    /// Start of long note (LN). Sound is played when the key is down inside the associated
    /// grading area. (C: `LNSTART`)
    LNStart(Lane, Option<SoundRef>),
    /// End of LN. Sound is played when the start of LN is graded, the key was down and now up
    /// inside the associated grading area. (C: `LNDONE`)
    LNDone(Lane, Option<SoundRef>),
    /// Bomb. Pressing the key down at the moment that the object is on time causes
    /// the specified damage; sound is played in this case. No associated grading area.
    /// (C: `BOMB`)
    Bomb(Lane, Option<SoundRef>, Damage),
    /// Plays associated sound. (C: `BGM_CHANNEL`)
    BGM(SoundRef),
    /**
     * Sets the virtual BGA layer to given image. The layer itself may not be displayed
     * depending on the current game status. (C: `BGA_CHANNEL`)
     *
     * If the reference points to a movie, the movie starts playing; if the other layer had
     * the same movie started, it rewinds to the beginning. The resulting image from the movie
     * can be shared among multiple layers.
     */
    SetBGA(BGALayer, Option<ImageRef>),
    /// Sets the BPM. Negative BPM causes the chart scrolls backwards (and implicitly signals
    /// the end of the chart). (C: `BPM_CHANNEL`)
    SetBPM(BPM),
    /// Stops the scroll of the chart for given duration ("scroll stopper" hereafter).
    /// (C: `STOP_CHANNEL`)
    Stop(Duration),
    /// Marks the logical end of the chart. This is also useful to extend the chart without
    /// inserting any dummy object after the end of the song. This object is otherwise a no-op,
    /// and expected to be the last object in the chart if present.
    End,

    // XXX
    //SetShortenFactor(float),
    //MeasureBar, // ???
}

impl<S:ToStr,I:ToStr> ToStr for ObjData<S,I> {
    fn to_str(&self) -> ~str {
        fn lane_to_str(lane: Lane) -> ~str {
            fmt!("%u:%02u", *lane / 36 + 1, *lane % 36)
        }
        fn to_str_or_default<T:ToStr>(v: &Option<T>, default: &str) -> ~str {
            match *v { Some(ref v) => v.to_str(), None => default.to_owned() }
        }

        match *self {
            Deleted => ~"Deleted",
            Visible(lane,ref sref) =>
                fmt!("Visible(%s,%s)", lane_to_str(lane), to_str_or_default(sref, "--")),
            Invisible(lane,ref sref) =>
                fmt!("Invisible(%s,%s)", lane_to_str(lane), to_str_or_default(sref, "--")),
            LNStart(lane,ref sref) =>
                fmt!("LNStart(%s,%s)", lane_to_str(lane), to_str_or_default(sref, "--")),
            LNDone(lane,ref sref) =>
                fmt!("LNDone(%s,%s)", lane_to_str(lane), to_str_or_default(sref, "--")),
            Bomb(lane,ref sref,damage) =>
                fmt!("Bomb(%s,%s,%s)", lane_to_str(lane), to_str_or_default(sref, "--"),
                                       damage.to_str()),
            BGM(ref sref) =>
                fmt!("BGM(%s)", sref.to_str()),
            SetBGA(layer,ref iref) =>
                fmt!("SetBGA(%s,%s)", layer.to_str(), to_str_or_default(iref, "--")),
            SetBPM(BPM(bpm)) =>
                fmt!("SetBPM(%f)", bpm),
            Stop(Seconds(secs)) =>
                fmt!("Stop(%fs)", secs),
            Stop(Measures(measures)) =>
                fmt!("Stop(%f)", measures),
            End => ~"End",
        }
    }
}

/// Any type that contains `ObjData`.
pub trait ToObjData<SoundRef:Copy,ImageRef:Copy> {
    pub fn to_obj_data(&self) -> ObjData<SoundRef,ImageRef>;
}

/// Any type that contains and can individually update `ObjData`.
pub trait WithObjData<SoundRef:Copy,ImageRef:Copy>: ToObjData<SoundRef,ImageRef> {
    pub fn with_obj_data(&self, data: ObjData<SoundRef,ImageRef>) -> Self;
}

impl<S:Copy,I:Copy> ToObjData<S,I> for ObjData<S,I> {
    pub fn to_obj_data(&self) -> ObjData<S,I> { copy *self }
}

impl<S:Copy,I:Copy> WithObjData<S,I> for ObjData<S,I> {
    pub fn with_obj_data(&self, data: ObjData<S,I>) -> ObjData<S,I> { data }
}

/// Query operations for objects. Implicitly defined in terms of `ToObjData` trait.
pub trait ObjQueryOps<SoundRef:Copy,ImageRef:Copy> {
    /// Returns true if the object is deleted (`Deleted`).
    pub fn is_deleted(&self) -> bool;
    /// Returns true if the object is a visible object (`Visible`). (C: `obj->type == NOTE`)
    pub fn is_visible(&self) -> bool;
    /// Returns true if the object is an invisible object (`Invisible`).
    /// (C: `obj->type == INVNOTE`)
    pub fn is_invisible(&self) -> bool;
    /// Returns true if the object is a start of LN object (`LNStart`).
    /// (C: `obj->type == LNSTART`)
    pub fn is_lnstart(&self) -> bool;
    /// Returns true if the object is an end of LN object (`LNEnd`). (C: `obj->type == LNDONE`)
    pub fn is_lndone(&self) -> bool;
    /// Returns true if the object is either a start or an end of LN object.
    /// (C: `obj->type < NOTE`)
    pub fn is_ln(&self) -> bool;
    /// Returns true if the object is a bomb (`Bomb`). (C: `obj->type == BOMB`)
    pub fn is_bomb(&self) -> bool;
    /// Returns true if the object is soundable when it is the closest soundable object from
    /// the current position and the player pressed the key. Named "soundable" since it may
    /// choose not to play the associated sound. Note that not every object with sound is
    /// soundable. (C: `obj->type <= INVNOTE`)
    pub fn is_soundable(&self) -> bool;
    /// Returns true if the object is subject to grading. (C: `obj->type < INVNOTE`)
    pub fn is_gradable(&self) -> bool;
    /// Returns true if the object has a visible representation. (C: `obj->type != INVNOTE`)
    pub fn is_renderable(&self) -> bool;
    /// Returns true if the data is an object. (C: `IS_NOTE_CHANNEL(obj->chan)`)
    pub fn is_object(&self) -> bool;
    /// Returns true if the data is a BGM. (C: `obj->chan == BGM_CHANNEL`)
    pub fn is_bgm(&self) -> bool;
    /// Returns true if the data is a BGA. (C: `obj->chan == BGA_CHANNEL`)
    pub fn is_setbga(&self) -> bool;
    /// Returns true if the data is a BPM change. (C: `obj->chan == BPM_CHANNEL`)
    pub fn is_setbpm(&self) -> bool;
    /// Returns true if the data is a scroll stopper. (C: `obj->chan == STOP_CHANNEL`)
    pub fn is_stop(&self) -> bool;
    /// Returns true if the data is an end mark.
    pub fn is_end(&self) -> bool;

    /// Returns an associated lane if the data is an object.
    pub fn object_lane(&self) -> Option<Lane>;
    /// Returns all sounds associated to the data.
    pub fn sounds(&self) -> ~[SoundRef];
    /// Returns all sounds played when key is pressed.
    pub fn keydown_sound(&self) -> Option<SoundRef>;
    /// Returns all sounds played when key is unpressed.
    pub fn keyup_sound(&self) -> Option<SoundRef>;
    /// Returns all sounds played when the object is activated while the corresponding key is
    /// currently pressed. Bombs are the only instance of this kind of sounds.
    pub fn through_sound(&self) -> Option<SoundRef>;
    /// Returns all images associated to the data.
    pub fn images(&self) -> ~[ImageRef];
    /// Returns an associated damage value when the object is activated.
    pub fn through_damage(&self) -> Option<Damage>;
}

/// Conversion operations for objects. Implicitly defined in terms of `WithObjData` trait.
pub trait ObjConvOps<SoundRef:Copy,ImageRef:Copy>: ObjQueryOps<SoundRef,ImageRef> {
    /// Returns a visible object with the same time, lane and sound as given object.
    pub fn to_visible(&self) -> Self;
    /// Returns an invisible object with the same time, lane and sound as given object.
    pub fn to_invisible(&self) -> Self;
    /// Returns a start of LN object with the same time, lane and sound as given object.
    pub fn to_lnstart(&self) -> Self;
    /// Returns an end of LN object with the same time, lane and sound as given object.
    pub fn to_lndone(&self) -> Self;
    /// Returns a non-object version of given object. May return `Deleted` if it should be deleted.
    pub fn to_effect(&self) -> Self;
    /// Returns an object with lane replaced with given lane. No effect on object-like effects.
    pub fn with_object_lane(&self, lane: Lane) -> Self;
}

impl<S:Copy,I:Copy,T:ToObjData<S,I>> ObjQueryOps<S,I> for T {
    pub fn is_deleted(&self) -> bool {
        match self.to_obj_data() { Deleted => true, _ => false }
    }

    pub fn is_visible(&self) -> bool {
        match self.to_obj_data() { Visible(*) => true, _ => false }
    }

    pub fn is_invisible(&self) -> bool {
        match self.to_obj_data() { Invisible(*) => true, _ => false }
    }

    pub fn is_lnstart(&self) -> bool {
        match self.to_obj_data() { LNStart(*) => true, _ => false }
    }

    pub fn is_lndone(&self) -> bool {
        match self.to_obj_data() { LNDone(*) => true, _ => false }
    }

    pub fn is_ln(&self) -> bool {
        match self.to_obj_data() { LNStart(*) | LNDone(*) => true, _ => false }
    }

    pub fn is_bomb(&self) -> bool {
        match self.to_obj_data() { Bomb(*) => true, _ => false }
    }

    pub fn is_soundable(&self) -> bool {
        match self.to_obj_data() {
            Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) => true,
            _ => false
        }
    }

    pub fn is_gradable(&self) -> bool {
        match self.to_obj_data() {
            Visible(*) | LNStart(*) | LNDone(*) => true,
            _ => false
        }
    }

    pub fn is_renderable(&self) -> bool {
        match self.to_obj_data() {
            Visible(*) | LNStart(*) | LNDone(*) | Bomb(*) => true,
            _ => false
        }
    }

    pub fn is_object(&self) -> bool {
        match self.to_obj_data() {
            Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) | Bomb(*) => true,
            _ => false
        }
    }

    pub fn is_bgm(&self) -> bool {
        match self.to_obj_data() { BGM(*) => true, _ => false }
    }

    pub fn is_setbga(&self) -> bool {
        match self.to_obj_data() { SetBGA(*) => true, _ => false }
    }

    pub fn is_setbpm(&self) -> bool {
        match self.to_obj_data() { SetBPM(*) => true, _ => false }
    }

    pub fn is_stop(&self) -> bool {
        match self.to_obj_data() { Stop(*) => true, _ => false }
    }

    pub fn is_end(&self) -> bool {
        match self.to_obj_data() { End => true, _ => false }
    }

    pub fn object_lane(&self) -> Option<Lane> {
        match self.to_obj_data() {
            Visible(lane,_) | Invisible(lane,_) | LNStart(lane,_) |
            LNDone(lane,_) | Bomb(lane,_,_) => Some(lane),
            _ => None
        }
    }

    pub fn sounds(&self) -> ~[S] {
        match self.to_obj_data() {
            Visible(_,Some(ref sref)) => ~[copy *sref],
            Invisible(_,Some(ref sref)) => ~[copy *sref],
            LNStart(_,Some(ref sref)) => ~[copy *sref],
            LNDone(_,Some(ref sref)) => ~[copy *sref],
            Bomb(_,Some(ref sref),_) => ~[copy *sref],
            BGM(ref sref) => ~[copy *sref],
            _ => ~[]
        }
    }

    pub fn keydown_sound(&self) -> Option<S> {
        match self.to_obj_data() {
            Visible(_,ref sref) | Invisible(_,ref sref) | LNStart(_,ref sref) => copy *sref,
            _ => None
        }
    }

    pub fn keyup_sound(&self) -> Option<S> {
        match self.to_obj_data() { LNDone(_,ref sref) => copy *sref, _ => None }
    }

    pub fn through_sound(&self) -> Option<S> {
        match self.to_obj_data() { Bomb(_,ref sref,_) => copy *sref, _ => None }
    }

    pub fn images(&self) -> ~[I] {
        match self.to_obj_data() { SetBGA(_,Some(ref iref)) => ~[copy *iref], _ => ~[] }
    }

    pub fn through_damage(&self) -> Option<Damage> {
        match self.to_obj_data() { Bomb(_,_,damage) => Some(damage), _ => None }
    }
}

impl<S:Copy,I:Copy,T:WithObjData<S,I>+Copy> ObjConvOps<S,I> for T {
    pub fn to_visible(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => Visible(lane,copy *snd),
            _ => fail!(~"to_visible for non-object")
        };
        self.with_obj_data(data)
    }

    pub fn to_invisible(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => Invisible(lane,copy *snd),
            _ => fail!(~"to_invisible for non-object")
        };
        self.with_obj_data(data)
    }

    pub fn to_lnstart(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => LNStart(lane,copy *snd),
            _ => fail!(~"to_lnstart for non-object")
        };
        self.with_obj_data(data)
    }

    pub fn to_lndone(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => LNDone(lane,copy *snd),
            _ => fail!(~"to_lndone for non-object")
        };
        self.with_obj_data(data)
    }

    pub fn to_effect(&self) -> T {
        let data = match self.to_obj_data() {
            Visible(_,Some(ref snd)) | Invisible(_,Some(ref snd)) |
            LNStart(_,Some(ref snd)) | LNDone(_,Some(ref snd)) => BGM(copy *snd),
            Visible(_,None) | Invisible(_,None) |
            LNStart(_,None) | LNDone(_,None) | Bomb(_,_,_) => Deleted,
            _ => { return copy *self; }
        };
        self.with_obj_data(data)
    }

    pub fn with_object_lane(&self, lane: Lane) -> T {
        let data = match self.to_obj_data() {
            Visible(_,ref snd) => Visible(lane,copy *snd),
            Invisible(_,ref snd) => Invisible(lane,copy *snd),
            LNStart(_,ref snd) => LNStart(lane,copy *snd),
            LNDone(_,ref snd) => LNDone(lane,copy *snd),
            Bomb(_,ref snd,damage) => Bomb(lane,copy *snd,damage),
            _ => { return copy *self; }
        };
        self.with_obj_data(data)
    }
}

/// Game play data associated to the time axis. It contains both objects (which are also
/// associated to lanes) and object-like effects.
#[deriving(Eq,ToStr)]
pub struct Obj<SoundRef,ImageRef> {
    /// Time position in measures.
    time: float,
    /// Actual data.
    data: ObjData<SoundRef,ImageRef>
}

impl<S:Copy,I:Copy> Ord for Obj<S,I> {
    fn lt(&self, other: &Obj<S,I>) -> bool { self.time < other.time }
    fn le(&self, other: &Obj<S,I>) -> bool { self.time <= other.time }
    fn ge(&self, other: &Obj<S,I>) -> bool { self.time >= other.time }
    fn gt(&self, other: &Obj<S,I>) -> bool { self.time > other.time }
}

impl<S:Copy,I:Copy> ToObjData<S,I> for Obj<S,I> {
    pub fn to_obj_data(&self) -> ObjData<S,I> { copy self.data }
}

impl<S:Copy,I:Copy> WithObjData<S,I> for Obj<S,I> {
    pub fn with_obj_data(&self, data: ObjData<S,I>) -> Obj<S,I> {
        Obj { time: self.time, data: data }
    }
}

