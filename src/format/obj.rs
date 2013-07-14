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
#[deriving(Eq)]
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
#[deriving(Eq)]
pub struct BPM(float);

impl BPM {
    /// Converts a measure to a millisecond. (C: `MEASURE_TO_MSEC`)
    pub fn measure_to_msec(self, measure: float) -> float { measure * 240000.0 / *self }

    /// Converts a millisecond to a measure. (C: `MSEC_TO_MEASURE`)
    pub fn msec_to_measure(self, msec: float) -> float { msec * *self / 240000.0 }
}

/// A duration from the particular point. It may be specified in measures or seconds. Used in
/// the `Stop` object.
#[deriving(Eq)]
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
#[deriving(Eq)]
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

    // XXX
    //SetShortenFactor(float),
    //End,
}

/// Query operations for objects.
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

/// Conversion operations for objects.
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

impl<SoundRef:Copy,ImageRef:Copy> ObjQueryOps<SoundRef,ImageRef> for ObjData<SoundRef,ImageRef> {
    pub fn is_deleted(&self) -> bool {
        match *self { Deleted => true, _ => false }
    }

    pub fn is_visible(&self) -> bool {
        match *self { Visible(*) => true, _ => false }
    }

    pub fn is_invisible(&self) -> bool {
        match *self { Invisible(*) => true, _ => false }
    }

    pub fn is_lnstart(&self) -> bool {
        match *self { LNStart(*) => true, _ => false }
    }

    pub fn is_lndone(&self) -> bool {
        match *self { LNDone(*) => true, _ => false }
    }

    pub fn is_ln(&self) -> bool {
        match *self { LNStart(*) | LNDone(*) => true, _ => false }
    }

    pub fn is_bomb(&self) -> bool {
        match *self { Bomb(*) => true, _ => false }
    }

    pub fn is_soundable(&self) -> bool {
        match *self { Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) => true, _ => false }
    }

    pub fn is_gradable(&self) -> bool {
        match *self { Visible(*) | LNStart(*) | LNDone(*) => true, _ => false }
    }

    pub fn is_renderable(&self) -> bool {
        match *self { Visible(*) | LNStart(*) | LNDone(*) | Bomb(*) => true, _ => false }
    }

    pub fn is_object(&self) -> bool {
        match *self { Visible(*) | Invisible(*) | LNStart(*) | LNDone(*) | Bomb(*) => true,
                     _ => false }
    }

    pub fn is_bgm(&self) -> bool {
        match *self { BGM(*) => true, _ => false }
    }

    pub fn is_setbga(&self) -> bool {
        match *self { SetBGA(*) => true, _ => false }
    }

    pub fn is_setbpm(&self) -> bool {
        match *self { SetBPM(*) => true, _ => false }
    }

    pub fn is_stop(&self) -> bool {
        match *self { Stop(*) => true, _ => false }
    }

    pub fn object_lane(&self) -> Option<Lane> {
        match *self {
            Visible(lane,_) | Invisible(lane,_) | LNStart(lane,_) |
            LNDone(lane,_) | Bomb(lane,_,_) => Some(lane),
            _ => None
        }
    }

    pub fn sounds(&self) -> ~[SoundRef] {
        match *self {
            Visible(_,Some(ref sref)) => ~[copy *sref],
            Invisible(_,Some(ref sref)) => ~[copy *sref],
            LNStart(_,Some(ref sref)) => ~[copy *sref],
            LNDone(_,Some(ref sref)) => ~[copy *sref],
            Bomb(_,Some(ref sref),_) => ~[copy *sref],
            BGM(ref sref) => ~[copy *sref],
            _ => ~[]
        }
    }

    pub fn keydown_sound(&self) -> Option<SoundRef> {
        match *self {
            Visible(_,ref sref) | Invisible(_,ref sref) | LNStart(_,ref sref) => copy *sref,
            _ => None
        }
    }

    pub fn keyup_sound(&self) -> Option<SoundRef> {
        match *self { LNDone(_,ref sref) => copy *sref, _ => None }
    }

    pub fn through_sound(&self) -> Option<SoundRef> {
        match *self { Bomb(_,ref sref,_) => copy *sref, _ => None }
    }

    pub fn images(&self) -> ~[ImageRef] {
        match *self { SetBGA(_,Some(ref iref)) => ~[copy *iref], _ => ~[] }
    }

    pub fn through_damage(&self) -> Option<Damage> {
        match *self { Bomb(_,_,damage) => Some(damage), _ => None }
    }
}

impl<SoundRef:Copy,ImageRef:Copy> ObjConvOps<SoundRef,ImageRef> for ObjData<SoundRef,ImageRef> {
    pub fn to_visible(&self) -> ObjData<SoundRef,ImageRef> {
        match *self {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => Visible(lane,copy *snd),
            _ => fail!(~"to_visible for non-object")
        }
    }

    pub fn to_invisible(&self) -> ObjData<SoundRef,ImageRef> {
        match *self {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => Invisible(lane,copy *snd),
            _ => fail!(~"to_invisible for non-object")
        }
    }

    pub fn to_lnstart(&self) -> ObjData<SoundRef,ImageRef> {
        match *self {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => LNStart(lane,copy *snd),
            _ => fail!(~"to_lnstart for non-object")
        }
    }

    pub fn to_lndone(&self) -> ObjData<SoundRef,ImageRef> {
        match *self {
            Visible(lane,ref snd) | Invisible(lane,ref snd) |
            LNStart(lane,ref snd) | LNDone(lane,ref snd) => LNDone(lane,copy *snd),
            _ => fail!(~"to_lndone for non-object")
        }
    }

    pub fn to_effect(&self) -> ObjData<SoundRef,ImageRef> {
        match *self {
            Visible(_,Some(ref snd)) | Invisible(_,Some(ref snd)) |
            LNStart(_,Some(ref snd)) | LNDone(_,Some(ref snd)) => BGM(copy *snd),
            Visible(_,None) | Invisible(_,None) |
            LNStart(_,None) | LNDone(_,None) | Bomb(_,_,_) => Deleted,
            _ => copy *self
        }
    }

    pub fn with_object_lane(&self, lane: Lane) -> ObjData<SoundRef,ImageRef> {
        match *self {
            Visible(_,ref snd) => Visible(lane,copy *snd),
            Invisible(_,ref snd) => Invisible(lane,copy *snd),
            LNStart(_,ref snd) => LNStart(lane,copy *snd),
            LNDone(_,ref snd) => LNDone(lane,copy *snd),
            Bomb(_,ref snd,damage) => Bomb(lane,copy *snd,damage),
            _ => copy *self
        }
    }
}

/// Game play data associated to the time axis. It contains both objects (which are also
/// associated to lanes) and object-like effects.
#[deriving(Eq)]
pub struct Obj<SoundRef,ImageRef> {
    /// Time position in measures.
    time: float,
    /// Actual data.
    data: ObjData<SoundRef,ImageRef>
}

impl<SoundRef:Copy,ImageRef:Copy> Obj<SoundRef,ImageRef> {
    /// Creates a `Visible` object.
    pub fn Visible(time: float, lane: Lane, sref: Option<SoundRef>) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: Visible(lane, sref) }
    }

    /// Creates an `Invisible` object.
    pub fn Invisible(time: float, lane: Lane, sref: Option<SoundRef>) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: Invisible(lane, sref) }
    }

    /// Creates an `LNStart` object.
    pub fn LNStart(time: float, lane: Lane, sref: Option<SoundRef>) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: LNStart(lane, sref) }
    }

    /// Creates an `LNDone` object.
    pub fn LNDone(time: float, lane: Lane, sref: Option<SoundRef>) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: LNDone(lane, sref) }
    }

    /// Creates a `Bomb` object.
    pub fn Bomb(time: float, lane: Lane, sref: Option<SoundRef>,
            damage: Damage) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: Bomb(lane, sref, damage) }
    }

    /// Creates a `BGM` object.
    pub fn BGM(time: float, sref: SoundRef) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: BGM(sref) }
    }

    /// Creates a `SetBGA` object.
    pub fn SetBGA(time: float, layer: BGALayer, iref: Option<ImageRef>) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: SetBGA(layer, iref) }
    }

    /// Creates a `SetBPM` object.
    pub fn SetBPM(time: float, bpm: BPM) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: SetBPM(bpm) }
    }

    /// Creates a `Stop` object.
    pub fn Stop(time: float, duration: Duration) -> Obj<SoundRef,ImageRef> {
        Obj { time: time, data: Stop(duration) }
    }

    /// Returns the number of a measure containing this object.
    pub fn measure(&self) -> int { self.time.floor() as int }
}

impl<SoundRef:Copy,ImageRef:Copy> Ord for Obj<SoundRef,ImageRef> {
    fn lt(&self, other: &Obj<SoundRef,ImageRef>) -> bool { self.time < other.time }
    fn le(&self, other: &Obj<SoundRef,ImageRef>) -> bool { self.time <= other.time }
    fn ge(&self, other: &Obj<SoundRef,ImageRef>) -> bool { self.time >= other.time }
    fn gt(&self, other: &Obj<SoundRef,ImageRef>) -> bool { self.time > other.time }
}

impl<SoundRef:Copy,ImageRef:Copy> ObjQueryOps<SoundRef,ImageRef> for Obj<SoundRef,ImageRef> {
    pub fn is_deleted(&self) -> bool { self.data.is_deleted() }
    pub fn is_visible(&self) -> bool { self.data.is_visible() }
    pub fn is_invisible(&self) -> bool { self.data.is_invisible() }
    pub fn is_lnstart(&self) -> bool { self.data.is_lnstart() }
    pub fn is_lndone(&self) -> bool { self.data.is_lndone() }
    pub fn is_ln(&self) -> bool { self.data.is_ln() }
    pub fn is_bomb(&self) -> bool { self.data.is_bomb() }
    pub fn is_soundable(&self) -> bool { self.data.is_soundable() }
    pub fn is_gradable(&self) -> bool { self.data.is_gradable() }
    pub fn is_renderable(&self) -> bool { self.data.is_renderable() }
    pub fn is_object(&self) -> bool { self.data.is_object() }
    pub fn is_bgm(&self) -> bool { self.data.is_bgm() }
    pub fn is_setbga(&self) -> bool { self.data.is_setbga() }
    pub fn is_setbpm(&self) -> bool { self.data.is_setbpm() }
    pub fn is_stop(&self) -> bool { self.data.is_stop() }

    pub fn object_lane(&self) -> Option<Lane> { self.data.object_lane() }
    pub fn sounds(&self) -> ~[SoundRef] { self.data.sounds() }
    pub fn keydown_sound(&self) -> Option<SoundRef> { self.data.keydown_sound() }
    pub fn keyup_sound(&self) -> Option<SoundRef> { self.data.keyup_sound() }
    pub fn through_sound(&self) -> Option<SoundRef> { self.data.through_sound() }
    pub fn images(&self) -> ~[ImageRef] { self.data.images() }
    pub fn through_damage(&self) -> Option<Damage> { self.data.through_damage() }
}

impl<SoundRef:Copy,ImageRef:Copy> ObjConvOps<SoundRef,ImageRef> for Obj<SoundRef,ImageRef> {
    pub fn to_visible(&self) -> Obj<SoundRef,ImageRef> {
        Obj { time: self.time, data: self.data.to_visible() }
    }
    pub fn to_invisible(&self) -> Obj<SoundRef,ImageRef> {
        Obj { time: self.time, data: self.data.to_invisible() }
    }
    pub fn to_lnstart(&self) -> Obj<SoundRef,ImageRef> {
        Obj { time: self.time, data: self.data.to_lnstart() }
    }
    pub fn to_lndone(&self) -> Obj<SoundRef,ImageRef> {
        Obj { time: self.time, data: self.data.to_lndone() }
    }
    pub fn to_effect(&self) -> Obj<SoundRef,ImageRef> {
        Obj { time: self.time, data: self.data.to_effect() }
    }
    pub fn with_object_lane(&self, lane: Lane) -> Obj<SoundRef,ImageRef> {
        Obj { time: self.time, data: self.data.with_object_lane(lane) }
    }
}

