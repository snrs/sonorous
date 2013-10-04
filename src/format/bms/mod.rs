// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
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

use std::rand::*;

use format::obj::*;
use format::timeline::Timeline;
use format::pointer::Pointer;

pub use format::bms::types::{Key, MAXKEY};

pub mod types;
pub mod diag;
pub mod parse;
pub mod load;

/// Sound reference.
#[deriving(Eq,Clone)]
pub struct SoundRef(Key);

/// Image reference.
#[deriving(Eq,Clone)]
pub struct ImageRef(Key);

impl ToStr for SoundRef {
    fn to_str(&self) -> ~str { (**self).to_str() }
}

impl ToStr for ImageRef {
    fn to_str(&self) -> ~str { (**self).to_str() }
}

/// Default BPM. This value comes from the original BMS specification.
pub static DefaultBPM: BPM = BPM(130.0);

/**
 * Blit commands, which manipulate the image after the image had been loaded. This maps to BMS
 * #BGA command.
 *
 * Blitting occurs from the region `(x1,y1)-(x2,y2)` in the source surface to the region
 * `(dx,dy)-(dx+(x2-x1),dy+(y2-y1))` in the destination surface. The rectangular region contains
 * the upper-left corner but not the lower-right corner. The region is clipped to make
 * the upper-left corner has non-negative coordinates and the size of the region doesn't exceed
 * 256 by 256 pixels.
 */
pub struct BlitCmd {
    dst: ImageRef, src: ImageRef,
    x1: int, y1: int, x2: int, y2: int, dx: int, dy: int
}

/// A value of BMS #PLAYER command signifying Single Play (SP), where only channels #1x are used
/// for the game play.
pub static SinglePlay: int = 1;
/// A value of BMS #PLAYER command signifying Couple Play, where channels #1x and #2x renders to
/// the different panels. They are originally meant to be played by different players with
/// separate gauges and scores, but this mode of game play is increasingly unsupported by modern
/// implementations. Sonorous has only a limited support for Couple Play.
pub static CouplePlay: int = 2;
/// A value of BMS #PLAYER command signifying Double Play (DP), where both channels #1x and #2x
/// renders to a single wide panel. The chart is still meant to be played by one person.
pub static DoublePlay: int = 3;

/// Loaded BMS metadata and resources.
pub struct BmsMeta {
    /// Title. Maps to BMS #TITLE command.
    title: Option<~str>,
    /// Genre. Maps to BMS #GENRE command.
    genre: Option<~str>,
    /// Artist. Maps to BMS #ARTIST command.
    artist: Option<~str>,
    /// Path to an image for loading screen. Maps to BMS #STAGEFILE command.
    stagefile: Option<~str>,
    /// A base path used for loading all other resources. Maps to BMS #PATH_WAV command.
    basepath: Option<~str>,

    /// Game mode. One of `SinglePlay`(1), `CouplePlay`(2) or `DoublePlay`(3). Maps to BMS
    /// #PLAYER command.
    player: int,
    /// Game level. Does not affect the actual game play. Maps to BMS #PLAYLEVEL command.
    playlevel: int,
    /// Gauge difficulty. Higher is easier. Maps to BMS #RANK command.
    rank: int,

    /// Paths to sound file relative to `basepath` or BMS file.
    sndpath: ~[Option<~str>],
    /// Paths to image/movie file relative to `basepath` or BMS file.
    imgpath: ~[Option<~str>],
    /// List of blit commands to be executed after `imgpath` is loaded.
    blitcmd: ~[BlitCmd],
}

/// Timeline for the BMS file.
pub type BmsTimeline = Timeline<SoundRef,ImageRef>;

/// Pointer for the BMS file. Provided for the convenience.
pub type BmsPointer = Pointer<SoundRef,ImageRef>;

/// Loaded BMS data. This is an intermediate structure for metadata, resources and actual chart
/// data, and cannot be used for actual game play (since `Pointer` requires `Timeline` to be
/// a managed pointer).
pub struct Bms {
    /// A path to the BMS file. Also used for finding the resource when `meta.basepath` is not set.
    bmspath: ~str,
    /// Metadata and resources.
    meta: BmsMeta,
    /// Timeline.
    timeline: BmsTimeline,
}

