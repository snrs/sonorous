// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Game play logics.

use std::{int, uint, vec, cmp, num};

use sdl::*;
use ext::sdl::{mixer, mpeg};
use format::obj::*;
use format::timeline::TimelineInfo;
use format::pointer::*;
use format::bms::*;
use util::gl::{Texture, ShadedDrawing, TexturedDrawing};
use util::gfx::*;
use util::bmfont::{Font, LeftAligned, Centered};
use engine::keyspec::*;
use engine::input::{Input, KeyInput, JoyAxisInput, JoyButtonInput, QuitInput};
use engine::input::{LaneInput, SpeedUpInput, SpeedDownInput};
use engine::input::{InputState, Positive, Neutral, Negative};
use engine::input::{KeyMap};
use engine::resource::{BGAW, BGAH};
use engine::resource::{SoundResource, ImageResource, NoImage, Image, Movie};
use ui::common::{Ticker, update_line};
use ui::screen::Screen;
use ui::options::*;
use ui::init::{SCREENW, SCREENH};

//----------------------------------------------------------------------------------------------
// bms utilities

/// Applies given modifier to the game data. The target lanes of the modifier is determined
/// from given key specification. This function should be called twice for the Couple Play,
/// since 1P and 2P should be treated separately.
pub fn apply_modf<R: ::std::rand::RngUtil>(bms: &mut Bms, modf: Modf, r: &mut R,
                                           keyspec: &KeySpec, begin: uint, end: uint) {
    use timeline_modf = format::timeline::modf;

    let mut lanes = ~[];
    for uint::range(begin, end) |i| {
        let lane = keyspec.order[i];
        let kind = keyspec.kinds[*lane];
        if modf == ShuffleExModf || modf == RandomExModf ||
                kind.map_default(false, |&kind| kind.counts_as_key()) {
            lanes.push(lane);
        }
    }

    match modf {
        MirrorModf => timeline_modf::mirror(&mut bms.timeline, lanes),
        ShuffleModf | ShuffleExModf => timeline_modf::shuffle(&mut bms.timeline, r, lanes),
        RandomModf | RandomExModf => timeline_modf::randomize(&mut bms.timeline, r, lanes)
    };
}

//----------------------------------------------------------------------------------------------
// BGA states

/// A list of image references displayed in BGA layers (henceforth the BGA state). Not all image
/// referenced here is directly rendered, but the references themselves are kept.
type BGAState = [Option<ImageRef>, ..NLAYERS];

/// Similar to `BGAState` but also has a set of textures used to render the BGA.
struct BGARenderState {
    state: BGAState,
    textures: ~[Texture]
}

/// Returns the initial BGA state. Note that merely setting a particular layer doesn't start
/// the movie playback; `poorbgafix` in `parser::parse` function handles it.
fn initial_bga_state() -> BGAState {
    [None, None, None, Some(ImageRef(Key(0)))]
}

trait Uploadable {
    /// Uploads an associated surface to the texture if any.
    pub fn upload_to_texture(&self, texture: &Texture);
    /// Returns true if the resource should be updated continuously (i.e. movies or animations).
    pub fn should_always_upload(&self) -> bool;
}

impl Uploadable for ImageResource {
    pub fn upload_to_texture(&self, texture: &Texture) {
        match *self {
            NoImage => {}
            Image(surface) | Movie(surface,_) => {
                texture.upload_surface(surface, false, false);
            }
        }
    }

    pub fn should_always_upload(&self) -> bool {
        match *self {
            NoImage | Image(_) => false,
            Movie(_,mpeg) => mpeg.status() == mpeg::SMPEG_PLAYING
        }
    }
}

impl BGARenderState {
    /// Creates an initial state and textures.
    pub fn new(imgres: &[ImageResource]) -> BGARenderState {
        let state = initial_bga_state();
        let textures = do state.map |&iref| {
            let texture = match Texture::new(BGAW, BGAH) {
                Ok(texture) => texture,
                Err(err) => die!("Texture::new failed: %s", err)
            };
            for iref.iter().advance |&iref| {
                imgres[**iref].upload_to_texture(&texture);
            }
            texture
        };
        BGARenderState { state: state, textures: textures }
    }

    /// Updates the BGA state. This method prepares given image resources for the next rendering,
    /// notably by starting and stopping the movie playback and uploading textures as needed.
    pub fn update(&mut self, current: &BGAState, imgres: &[ImageResource]) {
        for uint::range(0, NLAYERS) |layer| {
            // TODO this design can't handle the case that a BGA layer is updated to the same
            // image reference, which should rewind the movie playback.
            if self.state[layer] != current[layer] {
                for self.state[layer].iter().advance |&iref| {
                    imgres[**iref].stop_movie();
                }
                for current[layer].iter().advance |&iref| {
                    imgres[**iref].start_movie();
                    imgres[**iref].upload_to_texture(&self.textures[layer]);
                }
            } else {
                for self.state[layer].iter().advance |&iref| {
                    if imgres[**iref].should_always_upload() {
                        imgres[**iref].upload_to_texture(&self.textures[layer]);
                    }
                }
            }
        }
        self.state = *current;
    }

    /// Renders the image resources for the specified layers to the specified region of `screen`.
    pub fn render(&self, screen: &Screen, layers: &[BGALayer], x: f32, y: f32) {
        // the BGA area should have been already filled to black.
        for layers.iter().advance |&layer| {
            if self.state[layer as uint].is_some() {
                do screen.draw_textured(&self.textures[layer as uint]) |d| {
                    d.rect(x, y, x + BGAW as f32, y + BGAH as f32);
                }
            }
        }
    }
}

//----------------------------------------------------------------------------------------------
// game play logics

/// Grades. Sonorous performs the time-based grading as long as possible (it can go wrong when
/// the object is near the discontinuity due to the current implementation strategy).
#[deriving(Eq)]
enum Grade {
    /**
     * Issued when the player did not input the object at all, the player was pressing the key
     * while a bomb passes through the corresponding lane, or failed to unpress the key within
     * the grading area for the end of LN. Resets the combo number, decreases the gauge
     * by severe amount (`MISS_DAMAGE` unless specified by the bomb) and displays the POOR BGA
     * for moments.
     *
     * Several games also use separate grading areas for empty lanes next to the object,
     * in order to avoid continuing the consecutive run ("combo") of acceptable grades by
     * just pressing every keys in the correct timing instead of pressing only lanes containing
     * objects. While this system is not a bad thing (and there are several BMS implementations
     * that use it), it is tricky to implement for the all situations. Sonorous currently
     * does not use this system due to the complexity.
     */
    MISS = 0,
    /// Issued when the player inputed the object and the normalized time difference (that is,
    /// the time difference multiplied by `Player::gradefactor`) between the input point and
    /// the object is between `GOOD_CUTOFF` and `BAD_CUTOFF` seconds. Resets the combo number,
    /// decreases the gauge by moderate amount (`BAD_DAMAGE`) and displays the POOR BGA for moments.
    BAD  = 1,
    /// Issued when the player inputed the object and the normalized time difference is between
    /// `GREAT_CUTOFF` and `GOOD_CUTOFF` seconds. Both the combo number and gauge is left unchanged.
    GOOD = 2,
    /// Issued when the player inputed the object and the normalized time difference is between
    /// `COOL_CUTOFF` and `GREAT_CUTOFF` . The combo number is increased by one and the gauge is
    /// replenished by small amount.
    GREAT = 3,
    /// Issued when the player inputed the object and the normalized time difference is less
    /// than `COOL_CUTOFF` . The combo number is increased by one and the gauge is replenished by
    /// large amount.
    COOL = 4,
}

/// Required time difference in seconds to get at least COOL grade.
static COOL_CUTOFF: float = 0.0144;
/// Required time difference in seconds to get at least GREAT grade.
static GREAT_CUTOFF: float = 0.048;
/// Required time difference in seconds to get at least GOOD grade.
static GOOD_CUTOFF: float = 0.084;
/// Required time difference in seconds to get at least BAD grade.
static BAD_CUTOFF: float = 0.144;

/// The number of available grades.
static NGRADES: uint = 5;

/// The maximum (internal) value for the gauge.
static MAXGAUGE: int = 512;
/// A base score per exact input. Actual score can increase by the combo (up to 2x) or decrease
/// by the larger time difference.
static SCOREPERNOTE: float = 300.0;

/// A damage due to the MISS grading. Only applied when the grading is not due to the bomb.
static MISS_DAMAGE: Damage = GaugeDamage(0.059);
/// A damage due to the BAD grading.
static BAD_DAMAGE: Damage = GaugeDamage(0.030);

/// Game play states independent to the display.
pub struct Player {
    /// The game play options.
    opts: ~Options,
    /// The current BMS metadata.
    meta: BmsMeta,
    /// The current timeline. This is a managed pointer so that `Pointer` can be created for it.
    timeline: @BmsTimeline,
    /// The derived timeline information.
    infos: TimelineInfo,
    /// The length of BMS file in seconds as calculated by `bms_duration`.
    duration: float,
    /// The key specification.
    keyspec: ~KeySpec,
    /// The input mapping.
    keymap: ~KeyMap,

    /// Set to true if the corresponding object in `bms.objs` had graded and should not be
    /// graded twice. Its length equals to that of `bms.objs`.
    nograding: ~[bool],
    /// Sound resources.
    sndres: ~[SoundResource],
    /// A sound chunk used for beeps. It always plays on the channel #0.
    beep: ~mixer::Chunk,
    /// Last channels in which the corresponding sound in `sndres` was played.
    sndlastch: ~[Option<uint>],
    /// Indices to last sounds which the channel has played. For every `x`, if `sndlastch[x] ==
    /// Some(y)` then `sndlastchmap[y] == Some(x)` and vice versa.
    lastchsnd: ~[Option<uint>],
    /// Currently active BGA layers.
    bga: BGAState,

    /// The chart expansion rate, or "play speed". One measure has the length of 400 pixels
    /// times the play speed, so higher play speed means that objects will fall much more
    /// quickly (hence the name).
    playspeed: float,
    /// The play speed targeted for speed change if any. It is also the value displayed while
    /// the play speed is changing.
    targetspeed: Option<float>,
    /// The current BPM. Can be negative, in that case the chart will scroll backwards.
    bpm: BPM,
    /// The timestamp at the last tick. It is a return value from `sdl::get_ticks` and measured
    /// in milliseconds.
    now: uint,
    /// The timestamp at the first tick.
    origintime: uint,

    /// A pointer to the point where the game play starts (and not necessarily equal to zero point
    /// in the chart). Corresponds to `origintime` and `TimelineInfo::originoffset`.
    origin: BmsPointer,
    /// A pointer to the grading line.
    cur: BmsPointer,
    /// A pointer to the lower bound of the grading area containing `cur`.
    checked: BmsPointer,
    /// A pointer to objects for the start of LN which grading is in progress.
    thru: [Option<BmsPointer>, ..NLANES],
    /// The pointer to the first encountered `SetBPM` object with a negative value. This is
    /// a special casing for negative BPMs (ugh!); when this is set, the normal timeline routine is
    /// disabled and `cur` etc. always move backwards with given BPM. Everything except for
    /// `MeasureLine` is not rendered.
    reverse: Option<BmsPointer>,

    /// The scale factor for grading area. The factor less than 1 causes the grading area
    /// shrink.
    gradefactor: float,
    /// The last grade and time when the grade is issued.
    lastgrade: Option<(Grade,uint)>,
    /// The numbers of each grades.
    gradecounts: [uint, ..NGRADES],
    /// The last combo number, i.e. the number of objects graded at least GREAT. GOOD doesn't
    /// cause the combo number reset; BAD and MISS do.
    lastcombo: uint,
    /// The best combo number so far. If the player manages to get no BADs and MISSes, then
    /// the combo number should end up with the number of note and LN objects
    /// (`BMSInfo::nnotes`).
    bestcombo: uint,
    /// The current score.
    score: uint,
    /// The current health gauge. Should be no larger than `MAXGAUGE`. This can go negative
    /// (not displayed directly), which will require players much more efforts to survive.
    gauge: int,
    /// The health gauge required to survive at the end of the song. Note that the gaugex
    /// less than this value (or even zero) doesn't cause the instant game over;
    /// only `InstantDeath` value from `Damage` does.
    survival: int,

    /// The number of keyboard or joystick keys, mapped to each lane and and currently pressed.
    keymultiplicity: [uint, ..NLANES],
    /// The state of joystick axes.
    joystate: [InputState, ..NLANES],
}

/// A list of play speed marks. `SpeedUpInput` and `SpeedDownInput` changes the play speed to
/// the next/previous nearest mark.
static SPEED_MARKS: &'static [float] = &[0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.5, 2.0, 2.5, 3.0,
    3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 7.0, 8.0, 10.0, 15.0, 25.0, 40.0, 60.0, 99.0];

/// Finds the next nearest play speed mark if any.
fn next_speed_mark(current: float) -> Option<float> {
    let mut prev = None;
    for SPEED_MARKS.iter().advance |&speed| {
        if speed < current - 0.001 {
            prev = Some(speed);
        } else {
            return prev;
        }
    }
    None
}

/// Finds the previous nearest play speed mark if any.
fn previous_speed_mark(current: float) -> Option<float> {
    let mut next = None;
    for SPEED_MARKS.rev_iter().advance |&speed| {
        if speed > current + 0.001 {
            next = Some(speed);
        } else {
            return next;
        }
    }
    None
}

/// Creates a beep sound played on the play speed change.
fn create_beep() -> ~mixer::Chunk {
    let samples = vec::from_fn::<i32>(12000, // approx. 0.14 seconds
        // sawtooth wave at 3150 Hz, quadratic decay after 0.02 seconds.
        |i| { let i = i as i32; (i%28-14) * cmp::min(2000, (12000-i)*(12000-i)/50000) });
    mixer::Chunk::new(unsafe { ::std::cast::transmute(samples) }, 128)
}

/// Creates a new player object. The player object owns other related structures, including
/// the options, BMS file, key specification, input mapping and sound resources.
pub fn Player(opts: ~Options, bms: Bms, infos: TimelineInfo, duration: float, keyspec: ~KeySpec,
              keymap: ~KeyMap, sndres: ~[SoundResource]) -> Player {
    // we no longer need the full `Bms` structure.
    let Bms { bmspath: _, meta: meta, timeline: timeline } = bms;
    let timeline = @timeline;

    let now = get_ticks();
    let initplayspeed = opts.playspeed;
    let originoffset = infos.originoffset;
    let gradefactor = 1.5 - cmp::min(meta.rank, 5) as float * 0.25;
    let initialgauge = MAXGAUGE * 500 / 1000;
    let survival = MAXGAUGE * 293 / 1000;
    let initbpm = timeline.initbpm;
    let nobjs = timeline.objs.len();
    let nsounds = sndres.len();

    // we initially set all pointers to the origin, and let the `tick` do the initial calculation.
    let origin = timeline.pointer(VirtualPos, originoffset);
    let mut player = Player {
        opts: opts, meta: meta, timeline: timeline, infos: infos, duration: duration,
        keyspec: keyspec, keymap: keymap,

        nograding: vec::from_elem(nobjs, false), sndres: sndres, beep: create_beep(),
        sndlastch: vec::from_elem(nsounds, None), lastchsnd: ~[], bga: initial_bga_state(),

        playspeed: initplayspeed, targetspeed: None, bpm: initbpm, now: now, origintime: now,

        origin: origin.clone(), cur: origin.clone(), checked: origin.clone(),
        thru: [None, ..NLANES], reverse: None,

        gradefactor: gradefactor, lastgrade: None, gradecounts: [0, ..NGRADES],
        lastcombo: 0, bestcombo: 0, score: 0, gauge: initialgauge, survival: survival,

        keymultiplicity: [0, ..NLANES], joystate: [Neutral, ..NLANES],
    };

    player.allocate_more_channels(64);
    mixer::reserve_channels(1); // so that the beep won't be affected
    player
}

impl Player {
    /// Returns true if the specified lane is being pressed, either by keyboard, joystick
    /// buttons or axes.
    pub fn key_pressed(&self, lane: Lane) -> bool {
        self.keymultiplicity[*lane] > 0 || self.joystate[*lane] != Neutral
    }

    /// Returns the play speed displayed. Can differ from the actual play speed
    /// (`self.playspeed`) when the play speed is changing.
    pub fn nominal_playspeed(&self) -> float {
        self.targetspeed.get_or_default(self.playspeed)
    }

    /// Updates the score and associated statistics according to grading. `scoredelta` is
    /// an weight normalized to [0,1] that is calculated from the distance between the object
    /// and the input time, and `damage` is an optionally associated `Damage` value for bombs.
    /// May return true when `Damage` resulted in the instant death.
    pub fn update_grade(&mut self, grade: Grade, scoredelta: float,
                        damage: Option<Damage>) -> bool {
        self.gradecounts[grade as uint] += 1;
        self.lastgrade = Some((grade, self.now));
        self.score += (scoredelta * SCOREPERNOTE *
                       (1.0 + (self.lastcombo as float) /
                              (self.infos.nnotes as float))) as uint;

        match grade {
            MISS | BAD => { self.lastcombo = 0; }
            GOOD => {}
            GREAT | COOL => {
                // at most 5/512(1%) recover when the combo is topped
                let weight = if grade == GREAT {2} else {3};
                let cmbbonus = cmp::min(self.lastcombo as int, 100) / 50;
                self.lastcombo += 1;
                self.gauge = cmp::min(self.gauge + weight + cmbbonus, MAXGAUGE);
            }
        }
        self.bestcombo = cmp::max(self.bestcombo, self.lastcombo);

        match damage {
            Some(GaugeDamage(ratio)) => {
                self.gauge -= (MAXGAUGE as float * ratio) as int; true
            }
            Some(InstantDeath) => {
                self.gauge = cmp::min(self.gauge, 0); false
            }
            None => true
        }
    }

    /// Same as `update_grade`, but the grade is calculated from the normalized difference
    /// between the object and input time in seconds. The normalized distance equals to
    /// the actual time difference when `gradefactor` is 1.0.
    pub fn update_grade_from_distance(&mut self, dist: float) {
        let dist = num::abs(dist);
        let (grade, damage) = if      dist <  COOL_CUTOFF {(COOL,None)}
                              else if dist < GREAT_CUTOFF {(GREAT,None)}
                              else if dist <  GOOD_CUTOFF {(GOOD,None)}
                              else if dist <   BAD_CUTOFF {(BAD,Some(BAD_DAMAGE))}
                              else                        {(MISS,Some(MISS_DAMAGE))};
        let scoredelta = cmp::max(1.0 - dist / BAD_CUTOFF, 0.0);
        let keepgoing = self.update_grade(grade, scoredelta, damage);
        assert!(keepgoing);
    }

    /// Same as `update_grade`, but with the predetermined damage value. Always results in MISS
    /// grade. May return true when the damage resulted in the instant death.
    pub fn update_grade_from_damage(&mut self, damage: Damage) -> bool {
        self.update_grade(MISS, 0.0, Some(damage))
    }

    /// Same as `update_grade`, but always results in MISS grade with the standard damage value.
    pub fn update_grade_to_miss(&mut self) {
        let keepgoing = self.update_grade(MISS, 0.0, Some(MISS_DAMAGE));
        assert!(keepgoing);
    }

    /// Allocate more SDL_mixer channels without stopping already playing channels.
    pub fn allocate_more_channels(&mut self, howmany: uint) {
        let howmany = howmany as ::std::libc::c_int;
        let nchannels = mixer::allocate_channels(-1 as ::std::libc::c_int);
        let nchannels = mixer::allocate_channels(nchannels + howmany) as uint;
        if self.lastchsnd.len() < nchannels {
            self.lastchsnd.grow(nchannels, &None);
        }
    }

    /// Plays a given sound referenced by `sref`. `bgm` indicates that the sound is a BGM and
    /// should be played with the lower volume and should in the different channel group from
    /// key sounds.
    pub fn play_sound(&mut self, sref: SoundRef, bgm: bool) {
        let sref = **sref as uint;
        let chunk = match self.sndres[sref].chunk() {
            Some(chunk) => chunk,
            None => { return; }
        };
        let lastch = self.sndlastch[sref].map(|&ch| ch as ::std::libc::c_int);

        // try to play on the last channel if it is not occupied by other sounds (in this case
        // the last channel info is removed)
        let mut ch;
        loop {
            ch = chunk.play(lastch, 0);
            if ch >= 0 { break; }
            self.allocate_more_channels(32);
        }

        let group = if bgm {1} else {0};
        mixer::set_channel_volume(Some(ch), if bgm {96} else {128});
        mixer::group_channel(Some(ch), Some(group));

        let ch = ch as uint;
        for self.lastchsnd[ch].iter().advance |&idx| { self.sndlastch[idx] = None; }
        self.sndlastch[sref] = Some(ch);
        self.lastchsnd[ch] = Some(sref as uint);
    }

    /// Plays a given sound if `sref` is not zero. This reflects the fact that an alphanumeric
    /// key `00` is normally a placeholder.
    pub fn play_sound_if_nonzero(&mut self, sref: SoundRef, bgm: bool) {
        if **sref > 0 { self.play_sound(sref, bgm); }
    }

    /// Plays a beep. The beep is always played in the channel 0, which is excluded from
    /// the uniform key sound and BGM management.
    pub fn play_beep(&self) {
        self.beep.play(Some(0), 0);
    }

    /// Updates the player state.
    pub fn tick(&mut self) -> bool {
        // smoothly change the play speed
        if self.targetspeed.is_some() {
            let target = self.targetspeed.get();
            let delta = target - self.playspeed;
            if num::abs(delta) < 0.001 {
                self.playspeed = target;
                self.targetspeed = None;
            } else {
                self.playspeed += delta * 0.1;
            }
        }

        self.now = get_ticks();
        let mut cur = self.cur;
        let mut checked = self.checked;
        let mut thru = self.thru;
        let prev = self.cur.clone();

        let curtime = (self.now - self.origintime) as float / 1000.0 + self.origin.loc.time;
        match self.reverse {
            Some(reverse) => {
                assert!(*self.bpm < 0.0 && curtime >= reverse.loc.time);
                let newpos = reverse.loc.pos + self.bpm.sec_to_measure(curtime - reverse.loc.time);
                cur.seek(ActualPos, newpos - cur.loc.pos);
            }
            None => {
                // apply object-like effects while advancing `self.cur`
                for cur.mut_until(ActualTime, curtime - cur.loc.time) |p| {
                    match p.data() {
                        BGM(sref) => {
                            self.play_sound_if_nonzero(sref, true);
                        }
                        SetBGA(layer, iref) => {
                            self.bga[layer as uint] = iref;
                        }
                        SetBPM(newbpm) => {
                            self.bpm = newbpm;
                            if *newbpm == 0.0 {
                                return false; // finish immediately
                            } else if *newbpm < 0.0 {
                                self.reverse = Some(p.clone()); // activate reverse motion
                            }
                        }
                        Visible(_,sref) | LNStart(_,sref) => {
                            if self.opts.is_autoplay() {
                                for sref.iter().advance |&sref| {
                                    self.play_sound_if_nonzero(sref, false);
                                }
                                self.update_grade_from_distance(0.0);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        // grade objects that have escaped the grading area
        if !self.opts.is_autoplay() {
            for checked.mut_upto(&cur) |p| {
                let dist = (cur.loc.vtime - p.loc.vtime) * self.gradefactor;
                if dist < BAD_CUTOFF { break; }
                if !self.nograding[p.index] {
                    let lane = p.object_lane(); // XXX #3511
                    for lane.iter().advance |&Lane(lane)| {
                        let missable =
                            match p.data() {
                                Visible(*) | LNStart(*) => true,
                                LNDone(*) => thru[lane].is_some(),
                                _ => false,
                            };
                        if missable {
                            self.update_grade_to_miss();
                            thru[lane] = None;
                        }
                    }
                }
            }
        }

        // process inputs
        loop {
            // map to the virtual input. results in `vkey` (virtual key), `state` (input state)
            // and `continuous` (true if the input is not discrete and `Negative` input state
            // matters).
            let (key, state) = match event::poll_event() {
                event::NoEvent => { break; }
                ev => match Input::from_event(ev) {
                    Some((QuitInput,_)) => { return false; },
                    Some(key_and_state) => key_and_state,
                    None => loop
                }
            };
            let vkey = match self.keymap.find(&key) {
                Some(&vkey) => vkey,
                None => loop
            };
            let continuous = match key {
                KeyInput(*) | JoyButtonInput(*) | QuitInput => false,
                JoyAxisInput(*) => true
            };

            if self.opts.is_exclusive() { loop; }

            // Returns true if the given lane is previously pressed and now unpressed.
            // When the virtual input is mapped to multiple actual inputs it can update
            // the internal state but still return false.
            let is_unpressed = |lane: Lane, continuous: bool, state: InputState| {
                if state == Neutral || (continuous && self.joystate[*lane] != state) {
                    if continuous {
                        self.joystate[*lane] = state; true
                    } else {
                        if self.keymultiplicity[*lane] > 0 {
                            self.keymultiplicity[*lane] -= 1;
                        }
                        (self.keymultiplicity[*lane] == 0)
                    }
                } else {
                    false
                }
            };

            // Returns true if the given lane is previously unpressed and now pressed.
            // When the virtual input is mapped to multiple actual inputs it can update
            // the internal state but still return false.
            let is_pressed = |lane: Lane, continuous: bool, state: InputState| {
                if state != Neutral {
                    if continuous {
                        self.joystate[*lane] = state; true
                    } else {
                        self.keymultiplicity[*lane] += 1;
                        (self.keymultiplicity[*lane] == 1)
                    }
                } else {
                    false
                }
            };

            let process_unpress = |lane: Lane| {
                // if LN grading is in progress and it is not within the threshold then
                // MISS grade is issued
                for thru[*lane].iter().advance |&thru| {
                    let nextlndone = do thru.find_next_of_type |&obj| {
                        obj.object_lane() == Some(lane) && obj.is_lndone()
                    };
                    for nextlndone.iter().advance |&p| {
                        let delta = (p.loc.vtime - cur.loc.vtime) * self.gradefactor;
                        if num::abs(delta) < BAD_CUTOFF {
                            self.nograding[p.index] = true;
                        } else {
                            self.update_grade_to_miss();
                        }
                    }
                }
                thru[*lane] = None;
            };

            let process_press = |lane: Lane| {
                // plays the closest key sound
                let soundable =
                    do cur.find_closest_of_type(VirtualTime) |&obj| {
                        obj.object_lane() == Some(lane) && obj.is_soundable()
                    };
                for soundable.iter().advance |&p| {
                    let sounds = p.sounds(); // XXX #3511
                    for sounds.iter().advance |&sref| {
                        self.play_sound(sref, false);
                    }
                }

                // tries to grade the closest gradable object in the grading area
                let gradable =
                    do cur.find_closest_of_type(VirtualTime) |&obj| {
                        obj.object_lane() == Some(lane) && obj.is_gradable()
                    };
                for gradable.iter().advance |&p| {
                    if p.index >= checked.index && !self.nograding[p.index] && !p.is_lndone() {
                        let dist = (p.loc.vtime - cur.loc.vtime) * self.gradefactor;
                        if num::abs(dist) < BAD_CUTOFF {
                            if p.is_lnstart() { thru[*lane] = Some(p.clone()); }
                            self.nograding[p.index] = true;
                            self.update_grade_from_distance(dist);
                        }
                    }
                }
                true
            };

            match (vkey, state) {
                (SpeedDownInput, Positive) | (SpeedDownInput, Negative) => {
                    let current = self.targetspeed.get_or_default(self.playspeed);
                    let newspeed = next_speed_mark(current); // XXX #3511
                    for newspeed.iter().advance |&newspeed| {
                        self.targetspeed = Some(newspeed);
                        self.play_beep();
                    }
                }
                (SpeedUpInput, Positive) | (SpeedUpInput, Negative) => {
                    let current = self.targetspeed.get_or_default(self.playspeed);
                    let newspeed = previous_speed_mark(current); // XXX #3511
                    for newspeed.iter().advance |&newspeed| {
                        self.targetspeed = Some(newspeed);
                        self.play_beep();
                    }
                }
                (LaneInput(lane), state) => {
                    if !self.opts.is_autoplay() {
                        if is_unpressed(lane, continuous, state) {
                            process_unpress(lane);
                        }
                        if is_pressed(lane, continuous, state) {
                            process_press(lane);
                        }
                    }
                }
                (_, _) => {}
            }

        }

        // process bombs
        if !self.opts.is_autoplay() {
            for prev.upto(&cur) |p| {
                match p.data() {
                    Bomb(lane,sref,damage) if self.key_pressed(lane) => {
                        // ongoing long note is not graded twice
                        thru[*lane] = None;
                        for sref.iter().advance |&sref| {
                            self.play_sound(sref, false);
                        }
                        if !self.update_grade_from_damage(damage) {
                            // instant death
                            self.cur = cur.find_end();
                            return false;
                        }
                    }
                    _ => {}
                }
            }
        }

        self.cur = cur;
        self.checked = checked;
        self.thru = thru;

        // determines if we should keep playing
        if self.cur.index == self.timeline.objs.len() {
            if self.opts.is_autoplay() {
                mixer::num_playing(None) != mixer::num_playing(Some(0))
            } else {
                mixer::newest_in_group(Some(1)).is_some()
            }
        } else if self.cur.loc.vpos < self.infos.originoffset {
            false // special casing the negative BPM
        } else {
            true
        }
    }
}

/// Display interface.
pub trait Display {
    /// Renders the current information from `player` to the screen or console. Called after
    /// each call to `Player::tick`.
    pub fn render(&mut self, player: &Player);
    /// Shows the game play result from `player` to the screen or console. Called only once.
    pub fn show_result(&self, player: &Player);
}

//----------------------------------------------------------------------------------------------
// graphic display

/// An appearance for each lane.
struct LaneStyle {
    /// The left position of the lane in the final screen.
    left: uint,
    /// The left position of the lane in the object sprite.
    spriteleft: uint,
    /// The left position of the lane in the bomb sprite.
    spritebombleft: uint,
    /// The width of lane.
    width: uint,
    /// The base color of object. The actual `Gradient` for drawing is derived from this color.
    basecolor: Color
}

impl LaneStyle {
    /// Constructs a new `LaneStyle` object from given key kind and the left (`Left(pos)`) or
    /// right (`Right(pos)`) position.
    pub fn from_kind(kind: KeyKind, pos: Either<uint,uint>) -> LaneStyle {
        let (spriteleft, spritebombleft, width, color) = match kind {
            WhiteKey    => ( 25,   0, 25, RGB(0x80,0x80,0x80)),
            WhiteKeyAlt => ( 50,   0, 25, RGB(0xf0,0xe0,0x80)),
            BlackKey    => ( 75,   0, 25, RGB(0x80,0x80,0xff)),
            Button1     => (130, 100, 30, RGB(0xe0,0xe0,0xe0)),
            Button2     => (160, 100, 30, RGB(0xff,0xff,0x40)),
            Button3     => (190, 100, 30, RGB(0x80,0xff,0x80)),
            Button4     => (220, 100, 30, RGB(0x80,0x80,0xff)),
            Button5     => (250, 100, 30, RGB(0xff,0x40,0x40)),
            Scratch     => (320, 280, 40, RGB(0xff,0x80,0x80)),
            FootPedal   => (360, 280, 40, RGB(0x80,0xff,0x80)),
        };
        let left = pos.either(|&left| left, |&right| right - width);
        LaneStyle { left: left, spriteleft: spriteleft, spritebombleft: spritebombleft,
                    width: width, basecolor: color }
    }

    /// Renders required object and bomb images to the sprite.
    pub fn render_to_sprite(&self, sprite: &Surface) {
        let left = self.spriteleft;
        let noteleft = self.spriteleft + SCREENW;
        let bombleft = self.spritebombleft + SCREENW;
        assert!(sprite.get_width() as uint >= cmp::max(noteleft, bombleft) + self.width);

        // render a background sprite (0 at top, <1 at bottom)
        let backcolor = Gradient { zero: RGB(0,0,0), one: self.basecolor };
        for uint::range(140, SCREENH - 80) |i| {
            sprite.fill_area((left, i), (self.width, 1), backcolor.blend(i as int - 140, 1000));
        }

        // render note and bomb sprites (1/2 at middle, 1 at border)
        let denom = self.width as int;
        let notecolor = Gradient { zero: RGB(0xff,0xff,0xff), one: self.basecolor };
        let bombcolor = Gradient { zero: RGB(0,0,0),          one: RGB(0xc0,0,0) };
        for uint::range(0, self.width / 2) |i| {
            let num = (self.width - i) as int;
            sprite.fill_area((noteleft+i, 0), (self.width-i*2, SCREENH),
                             notecolor.blend(num, denom));
            sprite.fill_area((bombleft+i, 0), (self.width-i*2, SCREENH),
                             bombcolor.blend(num, denom));
        }
    }

    /// Clears the lane background.
    pub fn clear_back(&self, d: &mut ShadedDrawing) {
        d.rect(self.left as f32, 30.0,
               (self.left + self.width) as f32, SCREENH as f32 - 80.0, RGB(0,0,0));
    }

    /// Renders the key-pressed lane background to the screen from the sprite.
    pub fn render_pressed_back(&self, d: &mut TexturedDrawing) {
        d.rect_area(self.left as f32, 140.0,
                    (self.left + self.width) as f32, SCREENH as f32 - 80.0,
                    self.spriteleft as f32, 140.0,
                    (self.spriteleft + self.width) as f32, SCREENH as f32 - 80.0);
    }

    /// Renders an object to the screen from the sprite.
    pub fn render_note(&self, d: &mut TexturedDrawing, top: uint, bottom: uint) {
        d.rect_area(self.left as f32, top as f32,
                    (self.left + self.width) as f32, bottom as f32,
                    (self.spriteleft + SCREENW) as f32, 0.0,
                    (self.spriteleft + self.width + SCREENW) as f32, bottom as f32);
    }

    /// Renders a bomb object to the screen from the sprite.
    pub fn render_bomb(&self, d: &mut TexturedDrawing, top: uint, bottom: uint) {
        d.rect_area(self.left as f32, top as f32,
                    (self.left + self.width) as f32, bottom as f32,
                    (self.spritebombleft + SCREENW) as f32, 0.0,
                    (self.spritebombleft + self.width + SCREENW) as f32, bottom as f32);
    }
}

/// Builds a list of `LaneStyle`s from the key specification.
fn build_lane_styles(keyspec: &KeySpec) ->
                                Result<(uint, Option<uint>, ~[(Lane,LaneStyle)]), ~str> {
    let mut leftmost = 0;
    let mut rightmost = SCREENW;
    let mut styles = ~[];
    for keyspec.each_left_lanes |&lane| {
        let kind = keyspec.kinds[*lane];
        assert!(kind.is_some());
        let kind = kind.get();
        let style = LaneStyle::from_kind(kind, Left(leftmost));
        styles.push((lane, style));
        leftmost += style.width + 1;
        if leftmost > SCREENW - 20 {
            return Err(~"The screen can't hold that many lanes");
        }
    }
    for keyspec.each_right_lanes |&lane| {
        let kind = keyspec.kinds[*lane];
        assert!(kind.is_some());
        let kind = kind.get();
        let style = LaneStyle::from_kind(kind, Right(rightmost));
        styles.push((lane, style));
        if rightmost < leftmost + 40 {
            return Err(~"The screen can't hold that many lanes");
        }
        rightmost -= style.width + 1;
    }
    let mut rightmost = if rightmost == SCREENW {None} else {Some(rightmost)};

    // move lanes to the center if there are too small number of lanes
    let cutoff = 165;
    if leftmost < cutoff {
        for uint::range(0, keyspec.split) |i| {
            let (lane, style) = styles[i];
            let mut style = style;
            style.left += (cutoff - leftmost) / 2;
            styles[i] = (lane, style);
        }
        leftmost = cutoff;
    }
    if rightmost.map_default(false, |&x| x > SCREENW - cutoff) {
        for uint::range(keyspec.split, styles.len()) |i| {
            let (lane, style) = styles[i];
            let mut style = style;
            style.left -= (rightmost.get() - (SCREENW - cutoff)) / 2;
            styles[i] = (lane, style);
        }
        rightmost = Some(SCREENW - cutoff);
    }

    Ok((leftmost, rightmost, styles))
}

/// Creates a sprite.
fn create_sprite(opts: &Options, leftmost: uint, rightmost: Option<uint>,
                 styles: &[(Lane,LaneStyle)]) -> Texture {
    let sprite = match new_surface(SCREENW + 400, SCREENH) {
        Ok(surface) => surface,
        Err(err) => die!("new_surface failed: %s", err)
    };
    let black = RGB(0,0,0);
    let gray = RGB(0x40,0x40,0x40); // gray used for separators

    // render notes and lane backgrounds
    for styles.iter().advance |&(_lane,style)| {
        style.render_to_sprite(sprite);
    }

    // render panels
    do sprite.with_pixels |pixels| {
        let topgrad = Gradient { zero: RGB(0x60,0x60,0x60), one: RGB(0xc0,0xc0,0xc0) };
        let botgrad = Gradient { zero: RGB(0x40,0x40,0x40), one: RGB(0xc0,0xc0,0xc0) };
        for int::range(-244, 556) |j| {
            for int::range(-10, 20) |i| {
                let c = (i*2+j*3+750) % 2000;
                put_pixel(pixels, (j+244) as uint, (i+10) as uint, // XXX incorrect lifetime
                          topgrad.blend(850 - num::abs(c-1000), 700));
            }
            for int::range(-20, 60) |i| {
                let c = (i*3+j*2+750) % 2000;
                let bottom = (SCREENH - 60) as int;
                put_pixel(pixels, (j+244) as uint, (i+bottom) as uint, // XXX incorrect lifetime
                          botgrad.blend(850 - num::abs(c-1000), 700));
            }
        }
    }
    sprite.fill_area((10, SCREENH-36), (leftmost, 1), gray);

    // erase portions of panels left unused
    let leftgap = leftmost + 20;
    let rightgap = rightmost.map_default(SCREENW, |x| x - 20);
    let gapwidth = rightgap - leftgap;
    sprite.fill_area((leftgap, 0), (gapwidth, 30), black);
    sprite.fill_area((leftgap, SCREENH-80), (gapwidth, 80), black);
    do sprite.with_pixels |pixels| {
        for uint::range(0, 20) |i| {
            for uint::range_rev(20, 0) |j| {
                if i*i + j*j <= 400 { break; } // circled border
                put_pixel(pixels, leftmost + j, 10 + i, black); // XXX incorrect lifetime
                put_pixel(pixels, leftmost + j, (SCREENH-61) - i, black); // XXX
                for rightmost.iter().advance |&right| {
                    put_pixel(pixels, (right-j) - 1, 10 + i, black); // XXX incorrect lifetime
                    put_pixel(pixels, (right-j) - 1, (SCREENH-61) - i, black); // XXX
                }
            }
        }
    }

    // draw the gauge bar if needed
    if !opts.is_autoplay() {
        sprite.fill_area((0, SCREENH-16), (368, 16), gray);
        sprite.fill_area((4, SCREENH-12), (360, 8), black);
    }

    match Texture::from_owned_surface(sprite, false, false) {
        Ok(tex) => tex,
        Err(err) => die!("Texture::from_owned_surface failed: %s", err)
    }
}

/// Full-featured graphic display. Used for the normal game play and automatic play mode.
pub struct GraphicDisplay {
    /// Sprite texture generated by `create_sprite`.
    sprite: Texture,
    /// Display screen.
    screen: Screen,
    /// Bitmap font.
    font: ~Font,
    /// Image resources.
    imgres: ~[ImageResource],

    /// The leftmost X coordinate of the area next to the lanes, that is, the total width of
    /// left-hand-side lanes.
    leftmost: uint,
    /// The rightmost X coordinate of the area next to the lanes, that is, the screen width
    /// minus the total width of right-hand-side lanes if any. `None` indicates the absence of
    /// right-hand-side lanes.
    rightmost: Option<uint>,
    /// The order and appearance of lanes.
    lanestyles: ~[(Lane,LaneStyle)],
    /// The left coordinate of the BGA.
    bgax: uint,
    /// The top coordinate of the BGA.
    bgay: uint,

    /// If not `None`, indicates that the POOR BGA should be displayed until this timestamp.
    poorlimit: Option<uint>,
    /// If not `None`, indicates that the grading information should be displayed until
    /// this timestamp.
    gradelimit: Option<uint>,
    /// Currently known state of BGAs.
    lastbga: BGARenderState,
}

/// Creates a new graphic display from the options, key specification, pre-allocated (usually
/// by `init_video`) screen, pre-created bitmap fonts and pre-loaded image resources. The last
/// three are owned by the display, others are not (in fact, should be owned by `Player`).
pub fn GraphicDisplay(opts: &Options, keyspec: &KeySpec, screen: Screen, font: ~Font,
                      imgres: ~[ImageResource]) -> Result<GraphicDisplay,~str> {
    let (leftmost, rightmost, styles) = match build_lane_styles(keyspec) {
        Ok(styles) => styles,
        Err(err) => { return Err(err); }
    };
    let centerwidth = rightmost.get_or_default(SCREENW) - leftmost;
    let bgax = leftmost + (centerwidth - BGAW) / 2;
    let bgay = (SCREENH - BGAH) / 2;
    let sprite = create_sprite(opts, leftmost, rightmost, styles);
    let bgastate = BGARenderState::new(imgres);

    let display = GraphicDisplay {
        sprite: sprite, screen: screen, font: font, imgres: imgres,
        leftmost: leftmost, rightmost: rightmost, lanestyles: styles, bgax: bgax, bgay: bgay,
        poorlimit: None, gradelimit: None, lastbga: bgastate,
    };

    Ok(display)
}

/// The list of grade names and corresponding color scheme.
static GRADES: &'static [(&'static str,Gradient)] = &[
    // Rust: can we just use `Gradient()`???
    ("MISS",  Gradient { zero: RGB(0xff,0xc0,0xc0), one: RGB(0xff,0x40,0x40) }),
    ("BAD",   Gradient { zero: RGB(0xff,0xc0,0xff), one: RGB(0xff,0x40,0xff) }),
    ("GOOD",  Gradient { zero: RGB(0xff,0xff,0xc0), one: RGB(0xff,0xff,0x40) }),
    ("GREAT", Gradient { zero: RGB(0xc0,0xff,0xc0), one: RGB(0x40,0xff,0x40) }),
    ("COOL",  Gradient { zero: RGB(0xc0,0xc0,0xff), one: RGB(0x40,0x40,0xff) }),
];

impl Display for GraphicDisplay {
    fn render(&mut self, player: &Player) {
        let font = &*self.font;

        let W = SCREENW as f32;
        let H = SCREENH as f32;

        // update display states
        let mut poorlimit = self.poorlimit;
        let mut gradelimit = self.gradelimit;
        for player.lastgrade.iter().advance |&(grade,when)| {
            if grade == MISS {
                // switches to the normal BGA after 600ms
                poorlimit = poorlimit.merge(Some(when + 600), cmp::max);
            }
            // grade disappears after 700ms
            gradelimit = gradelimit.merge(Some(when + 700), cmp::max);
        }
        if poorlimit < Some(player.now) { poorlimit = None; }
        if gradelimit < Some(player.now) { gradelimit = None; }
        self.lastbga.update(&player.bga, self.imgres);
        *&mut self.poorlimit = poorlimit;
        *&mut self.gradelimit = gradelimit;

        self.screen.clear();

        // render BGAs (should render before the lanes since lanes can overlap with BGAs)
        if player.opts.has_bga() {
            let layers = if poorlimit.is_some() {&[PoorBGA]} else {&[Layer1, Layer2, Layer3]};
            self.lastbga.render(&self.screen, layers, self.bgax as f32, self.bgay as f32);
        }

        do self.screen.draw_shaded |d| {
            // fill the lanes to the border color
            d.rect(0.0, 30.0, self.leftmost as f32, H-80.0, RGB(0x40,0x40,0x40));
            for self.rightmost.iter().advance |&rightmost| {
                d.rect(rightmost as f32, 30.0, H, 520.0, RGB(0x40,0x40,0x40));
            }

            // clear the lanes to the background color
            for self.lanestyles.iter().advance |&(_lane,style)| {
                style.clear_back(d);
            }
        }

        //let bottom = player.cur.find(ActualPos, -0.03 / player.playspeed);
        //let top = player.cur.find(ActualPos, 1.22 / player.playspeed);
        let bottom = player.cur.clone();
        let top = player.cur.find(ActualPos, 1.25 / player.playspeed);

        let loc_to_y = |loc: &ObjLoc<float>| {
            let offset = loc.pos - bottom.loc.pos;
            (SCREENH-70) - (400.0 * player.playspeed * offset) as uint
        };

        do self.screen.draw_textured(&self.sprite) |d| {
            // if we are in the reverse motion, do not draw objects before the motion start.
            let localbottom = match player.reverse {
                Some(reverse) => reverse.clone(),
                None => bottom
            };

            // render objects
            for self.lanestyles.iter().advance |&(lane,style)| {
                if player.key_pressed(lane) { style.render_pressed_back(d); }

                let front = do localbottom.find_next_of_type |&obj| {
                    obj.object_lane() == Some(lane) && obj.is_renderable()
                };
                if front.is_none() { loop; }
                let front = front.get();

                // LN starting before the bottom and ending after the top
                if front.loc.vpos > top.loc.vpos && front.is_lndone() {
                    style.render_note(d, 30, SCREENH - 80);
                } else {
                    let mut nextbottom = None;
                    for front.upto(&top) |ptr| {
                        let y = loc_to_y(&ptr.loc);
                        match ptr.data() {
                            LNStart(lane0,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                nextbottom = Some(y);
                            }
                            LNDone(lane0,_) if lane0 == lane => {
                                let bottom = SCREENH-80;
                                style.render_note(d, y, nextbottom.get_or_default(bottom));
                                nextbottom = None;
                            }
                            Visible(lane0,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                style.render_note(d, y-5, y);
                            }
                            Bomb(lane0,_,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                style.render_bomb(d, y-5, y);
                            }
                            _ => {}
                        }
                    }

                    for nextbottom.iter().advance |&y| {
                        style.render_note(d, 30, y);
                    }
                }
            }
        }

        do self.screen.draw_shaded |d| {
            // render non-note objects (currently, measure bars)
            for bottom.upto(&top) |ptr| {
                match ptr.data() {
                    MeasureBar => {
                        let y = loc_to_y(&ptr.loc) as f32;
                        d.rect(0.0, y, self.leftmost as f32, y + 1.0, RGB(0xc0,0xc0,0xc0));
                        for self.rightmost.iter().advance |&rightmost| {
                            d.rect(rightmost as f32, y,
                                   (SCREENW - rightmost) as f32, y + 1.0, RGB(0xc0,0xc0,0xc0));
                        }
                    }
                    _ => {}
                }
            }

            // render grading text
            if gradelimit.is_some() && player.lastgrade.is_some() {
                let gradelimit = gradelimit.get();
                let (lastgrade,_) = player.lastgrade.get();
                let (gradename,gradecolor) = GRADES[lastgrade as uint];
                let delta = (cmp::max(gradelimit - player.now, 400) as f32 - 400.0) / 15.0;
                let cx = (self.leftmost / 2) as f32; // avoids half-pixels
                let cy = H / 2.0 - delta; // offseted center
                font.draw_string(d, cx, cy - 40.0, 2.0, Centered, gradename, gradecolor);
                if player.lastcombo > 1 {
                    font.draw_string(d, cx, cy - 12.0, 1.0, Centered,
                                     fmt!("%u COMBO", player.lastcombo),
                                     Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80)));
                }
                if player.opts.is_autoplay() {
                    font.draw_string(d, cx, cy + 2.0, 1.0, Centered, "(AUTO)",
                                     Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x40,0x40,0x40)));
                }
            }
        }

        do self.screen.draw_textured(&self.sprite) |d| {
            // restore panel from the sprite
            d.rect_area(0.0, 0.0, W, 30.0, 0.0, 0.0, W, 30.0);
            d.rect_area(0.0, H-80.0, W, H, 0.0, H-80.0, W, H);
        }
        do self.screen.draw_shaded |d| {
            let elapsed = (player.now - player.origintime) / 1000;
            let duration = player.duration as uint;
            let durationmsec = (player.duration * 1000.0) as uint;

            // render panel text
            let black = RGB(0,0,0);
            font.draw_string(d, 10.0, 8.0, 1.0, LeftAligned,
                             fmt!("SCORE %07u", player.score), black);
            let nominalplayspeed = player.nominal_playspeed();
            font.draw_string(d, 5.0, H-78.0, 2.0, LeftAligned,
                             fmt!("%4.1fx", nominalplayspeed), black);
            font.draw_string(d, (self.leftmost-94) as f32, H-35.0, 1.0, LeftAligned,
                             fmt!("%02u:%02u / %02u:%02u", elapsed/60, elapsed%60,
                                                           duration/60, duration%60), black);
            font.draw_string(d, 95.0, H-62.0, 1.0, LeftAligned,
                             fmt!("@%9.4f", player.cur.loc.vpos), black);
            font.draw_string(d, 95.0, H-78.0, 1.0, LeftAligned,
                             fmt!("BPM %6.2f", *player.bpm), black);
            let timetick = cmp::min(self.leftmost, (player.now - player.origintime) *
                                                   self.leftmost / durationmsec);
            font.draw_glyph(d, 6.0 + timetick as f32, H-52.0, 1.0, 95, RGB(0x40,0x40,0x40));

            // render gauge
            if !player.opts.is_autoplay() {
                // cycles four times per measure, [0,40)
                let cycle = (160.0 * player.cur.loc.vpos).floor() % 40.0;
                let width = if player.gauge < 0 {0}
                            else {player.gauge * 400 / MAXGAUGE - (cycle as int)};
                let width = ::util::std::cmp::clamp(5, width, 360);
                let color = if player.gauge >= player.survival {RGB(0xc0,0,0)}
                            else {RGB(0xc0 - ((cycle * 4.0) as u8), 0, 0)};
                d.rect(4.0, H-12.0, 4.0 + width as f32, H-4.0, color);
            }
        }

        self.screen.swap_buffers();
    }

    fn show_result(&self, player: &Player) {
        if player.opts.is_autoplay() { return; }

        // check if the song reached the last gradable object (otherwise the game play was
        // terminated by the user)
        let nextgradable = player.cur.find_next_of_type(|obj| obj.is_gradable());
        if nextgradable.is_some() { return; }

        if player.gauge >= player.survival {
            println(fmt!("*** CLEARED! ***\n\
                          COOL  %4u    GREAT %4u    GOOD  %4u\n\
                          BAD   %4u    MISS  %4u    MAX COMBO %u\n\
                          SCORE %07u (max %07d)",
                         player.gradecounts[4], player.gradecounts[3],
                         player.gradecounts[2], player.gradecounts[1],
                         player.gradecounts[0], player.bestcombo,
                         player.score, player.infos.maxscore));
        } else {
            println("YOU FAILED!");
        }
    }
}

//----------------------------------------------------------------------------------------------
// text display

/// Text-only display. Used for the exclusive mode with BGA disabled.
pub struct TextDisplay {
    /// Ticker used for printing to the console.
    ticker: Ticker
}

/// Creates a new text-only display.
pub fn TextDisplay() -> TextDisplay {
    TextDisplay { ticker: Ticker() }
}

impl Display for TextDisplay {
    fn render(&mut self, player: &Player) {
        if !player.opts.showinfo { return; }

        do self.ticker.on_tick(player.now) {
            let elapsed = (player.now - player.origintime) / 100;
            let duration = (player.duration * 10.0) as uint;
            update_line(fmt!("%02u:%02u.%u / %02u:%02u.%u (@%9.4f) | BPM %6.2f | %u / %d notes",
                             elapsed/600, elapsed/10%60, elapsed%10,
                             duration/600, duration/10%60, duration%10,
                             player.cur.loc.vpos, *player.bpm,
                             player.lastcombo, player.infos.nnotes));
        }
    }

    fn show_result(&self, _player: &Player) {
        update_line("");
    }
}

//----------------------------------------------------------------------------------------------
// BGA-only display

/// BGA-only display. Used for the exclusive mode with BGA enabled.
pub struct BGAOnlyDisplay {
    /// The underlying text-only display (as the BGA-only display lacks the on-screen display).
    textdisplay: TextDisplay,
    /// Display screen.
    screen: Screen,
    /// Image resources.
    imgres: ~[ImageResource],
    /// Currently known state of BGAs.
    lastbga: BGARenderState,
}

/// Creates a new BGA-only display from the pre-created screen (usually by `init_video`) and
/// pre-loaded image resources.
pub fn BGAOnlyDisplay(screen: Screen, imgres: ~[ImageResource]) -> BGAOnlyDisplay {
    let bgastate = BGARenderState::new(imgres);
    BGAOnlyDisplay { textdisplay: TextDisplay(), screen: screen, imgres: imgres, lastbga: bgastate }
}

impl Display for BGAOnlyDisplay {
    fn render(&mut self, player: &Player) {
        self.screen.clear();
        self.lastbga.update(&player.bga, self.imgres);

        let layers = &[Layer1, Layer2, Layer3];
        self.lastbga.render(&self.screen, layers, 0.0, 0.0);
        self.screen.swap_buffers();

        self.textdisplay.render(player);
    }

    fn show_result(&self, player: &Player) {
        self.textdisplay.show_result(player);
    }
}

//----------------------------------------------------------------------------------------------

