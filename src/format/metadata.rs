// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Common metadata.

/// A numerical rating of the chart specified by the author.
///
/// Due to the long history of coexisting different rating systems (BMS, for example, has
/// at least five of them), this is a mostly freeform number associated with the rating system.
/// Levels in different rating systems are incompatible to others.
#[deriving(Clone, PartialEq)]
pub struct Level {
    /// The numeric rating.
    pub value: int,
    /// The rating system.
    pub system: LevelSystem,
}

/// A unique identifier for the rating system.
#[deriving(PartialEq, Eq, FromPrimitive, Clone)]
pub enum LevelSystem {
    // FIXME this is a temporary, ambiguous rating system used by BMS
    Bms = 1,
}

/// Difficulty group specified by the author.
///
/// Does not affect the actual game play but affects the selection screen
/// by grouping related charts.
#[deriving(PartialEq, Eq, PartialOrd, Ord, Clone)]
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

/// Common metadata for music data formats.
#[deriving(Clone)]
pub struct Meta {
    /// True when the metadata is subject to the randomness.
    /// This distinction is important since this would mean that we can't easily cache
    /// the metadata for listing purpose.
    pub random: bool,

    /// Title.
    pub title: Option<String>,
    /// Subtitle(s). Intended to be rendered in small print or only in certain cases.
    pub subtitles: Vec<String>,
    /// Purported genre. Many such "genres" are made up and may not match the actual genre.
    pub genre: Option<String>,
    /// Purported artist description. Normally includes composer, arranger, remixer and/or singer.
    /// Again, many such "artists" may not match the actual artist.
    pub artist: Option<String>,
    /// Secondary artist(s). Normally includes game data creator, BGA designer and so on.
    pub subartists: Vec<String>,
    /// Comment(s).
    pub comments: Vec<String>,

    /// The numerical chart rating specified by the author. Does not affect the actual game play.
    pub level: Option<Level>,
    /// Difficulty group specified by the author.
    pub difficulty: Option<Difficulty>,
}

