// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

/*!
 * BMS format implementation.
 *
 * # Structure
 *
 * The BMS format is a plain text format with most directives start with optional whitespace
 * followed by `#`. Besides the metadata (title, artist etc.), a BMS file is a map from the time
 * position to various game play elements (henceforth "objects") and other object-like effects
 * including BGM and BGA changes. It also contains preprocessor directives used to randomize some or
 * all parts of the BMS file, which would only make sense in the loading time.
 *
 * The time position is a virtual time divided by an unit of (musical) measure. It is related to
 * the actual time by the current Beats Per Minute (BPM) value which can, well, also change during
 * the game play. Consequently it is convenient to refer the position in terms of measures, which
 * the BMS format does: the lines `#xxxyy:AABBCC...` indicates that the measure number `xxx`
 * contains objects or object-like effects (of the type specified by `yy`, henceforth "channels"),
 * evenly spaced throughout the measure and which data values are `AA`, `BB`, `CC` respectively.
 *
 * An alphanumeric identifier (henceforth "alphanumeric key") like `AA` or `BB` may mean that
 * the actual numeric value interpreted as base 16 or 36 (depending on the channel), or a reference
 * to other assets (e.g. `#BMPAA foo.png`) or complex values specified by other commands (e.g.
 * `#BPMBB 192.0`). In most cases, an identifier `00` indicates an absence of objects or object-like
 * effects at that position.
 *
 * More detailed information about BMS format, including surveys about how different implementations
 * (so called BMS players) react to underspecified features or edge cases, can be found at [BMS
 * command memo](http://hitkey.nekokan.dyndns.info/cmds.htm).
 */

use std::fmt;

use format::obj::*;
use format::timeline::Timeline;
use format::pointer::Pointer;

pub use format::bms::types::{Key, MAXKEY};

pub mod types;
pub mod diag;
pub mod encoding;
pub mod preproc;
pub mod parse;
pub mod load;

/// Sound reference.
#[deriving(PartialEq,Eq,Clone)]
pub struct SoundRef(pub Key);

/// Image reference.
#[deriving(PartialEq,Eq,Clone)]
pub struct ImageRef(pub Key);

impl Deref<Key> for SoundRef {
    fn deref<'a>(&'a self) -> &'a Key {
        let SoundRef(ref key) = *self;
        key
    }
}

impl Deref<Key> for ImageRef {
    fn deref<'a>(&'a self) -> &'a Key {
        let ImageRef(ref key) = *self;
        key
    }
}

impl fmt::Show for SoundRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.deref().fmt(f) }
}

impl fmt::Show for ImageRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.deref().fmt(f) }
}

/// Default BPM. This value comes from the original BMS specification.
pub static DefaultBPM: BPM = BPM(130.0);

/// Play mode specified in the BMS file. This maps to BMS #PLAYER command. Over the course of
/// the evolution of the BMS format, this value became highly ambiguous and the client is advised
/// not to solely rely on this value.
#[deriving(PartialEq,Eq,Clone)]
pub enum PlayMode {
    /// Single Play (SP), where only channels #1x are used for the game play.
    SinglePlay = 1,
    /// Couple Play, where channels #1x and #2x renders to the different panels. They are originally
    /// meant to be played by different players with separate gauges and scores, but this mode of
    /// game play is increasingly unsupported by modern implementations. Sonorous has only a limited
    /// support for Couple Play.
    CouplePlay = 2,
    /// Double Play (DP), where both channels #1x and #2x renders to a single wide panel. The chart
    /// is still meant to be played by one person.
    DoublePlay = 3,
    /// Battle Play, where channels #1x are copied to channels #2x and both renders to
    /// the different panels. This was a temporary measure for the two-player game mode and
    /// has been completely replaced by automatic support for two-player mode (or a lack thereof)
    /// in modern implementations. Sonorous does not support Battle Play, but it does parse and
    /// treats Battle Play as Single Play.
    BattlePlay = 4,
}

/// Difficulty level specified by the author. This maps to BMS #DIFFICULTY command. Does not affect
/// the actual game play but affects the selection screen by grouping related BMSes.
#[deriving(PartialEq,Eq,PartialOrd,Clone)]
pub struct Difficulty(pub int);

impl Difficulty {
    /// Returns a string for representing the difficulty if any. This is used only for convenience.
    pub fn name(&self) -> Option<&'static str> {
        // this set of strings is designed to be unique in the first character and compatible to
        // existing subtitle detection rules in other implementations.
        match *self {
            Difficulty(1) => Some("BEGINNER"),
            Difficulty(2) => Some("NORMAL"),
            Difficulty(3) => Some("HARD"),
            Difficulty(4) => Some("EXTRA"),
            Difficulty(5) => Some("INSANE"),
            _ => None,
        }
    }
}

/// Loaded BMS metadata and resources.
pub struct BmsMeta {
    /// The name of character encoding used by the BMS file, and its confidence between 0 and 1.
    /// Confidence is set to infinity when it is forced by the loader.
    pub encoding: (&'static str, f64),

    /// Title. Maps to BMS #TITLE command.
    pub title: Option<String>,
    /// Subtitle(s). Maps to BMS #SUBTITLE command.
    pub subtitles: Vec<String>,
    /// Genre. Maps to BMS #GENRE command.
    pub genre: Option<String>,
    /// Artist. Maps to BMS #ARTIST command.
    pub artist: Option<String>,
    /// Secondary artist(s). Maps to BMS #SUBARTIST command.
    pub subartists: Vec<String>,
    /// Comment(s). Maps to BMS #COMMENT command.
    pub comments: Vec<String>,
    /// Path to an image for loading screen. Maps to BMS #STAGEFILE command.
    pub stagefile: Option<String>,
    /// Path to an image for banner image for the selection screen. Maps to BMS #BANNER command.
    pub banner: Option<String>,
    /// A base path used for loading all other resources. Maps to BMS #PATH_WAV command.
    pub basepath: Option<Path>,

    /// Game mode. Maps to BMS #PLAYER command.
    pub mode: PlayMode,
    /// Game level specified by the author. Does not affect the actual game play. Maps to BMS
    /// #PLAYLEVEL command.
    pub playlevel: int,
    /// Difficulty level specified by the author. Maps to BMS #DIFFICULTY command.
    pub difficulty: Option<Difficulty>,
    /// Gauge difficulty. Higher is easier. Maps to BMS #RANK command.
    pub rank: int,

    /// Paths to sound file relative to `basepath` or BMS file.
    pub sndpath: Vec<Option<String>>,
    /// Paths to image/movie file relative to `basepath` or BMS file.
    pub imgpath: Vec<Option<String>>,
}

/// Timeline for the BMS file.
pub type BmsTimeline = Timeline<SoundRef,ImageRef>;

/// Pointer for the BMS file. Provided for the convenience.
pub type BmsPointer = Pointer<SoundRef,ImageRef>;

/// Loaded BMS data. This is an intermediate structure for metadata, resources and actual chart
/// data, and cannot be used for actual game play (since `Pointer` requires `Timeline` to be
/// a managed pointer).
pub struct Bms {
    /// A path to the BMS file if any. Also used for finding the resource when `meta.basepath`
    /// is not set.
    pub bmspath: Option<Path>,
    /// Metadata and resources.
    pub meta: BmsMeta,
    /// Timeline.
    pub timeline: BmsTimeline,
}

impl Bms {
    /// Sets a path to the BMS file, which affects some metadata.
    pub fn with_bmspath(self, bmspath: &Path) -> Bms {
        let Bms { bmspath: bmspath0, meta, timeline } = self;
        if bmspath0.is_some() {
            Bms { bmspath: bmspath0, meta: meta, timeline: timeline }
        } else {
            let bmspath = bmspath.clone();
            let mut meta = meta;
            if meta.basepath.is_none() {
                let basepath = bmspath.dir_path();
                // Rust: `""` is not same as `"."` but `Path::dir_path` may produce it!
                let basepath = if basepath == Path::new("") {Path::new(".")} else {basepath};
                meta.basepath = Some(basepath);
            }
            Bms { bmspath: Some(bmspath), meta: meta, timeline: timeline }
        }
    }
}

