// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Global game options.

/// Game play modes. (C: `enum mode`)
#[deriving(Eq)]
pub enum Mode {
    /// Normal game play. The graphical display and input is enabled. (C: `PLAY_MODE`)
    PlayMode,
    /// Automatic game play. The graphical display is enabled but the input is mostly ignored
    /// except for the play speed change. (C: `AUTOPLAY_MODE`)
    AutoPlayMode,
    /// Exclusive (headless) mode. The graphical display is reduced to the BGA or absent at all
    /// (when `NoBga` is also set). (C: `EXCLUSIVE_MODE`)
    ExclusiveMode
}

/// Modifiers that affect the game data. (C: `enum modf`)
#[deriving(Eq)]
pub enum Modf {
    /// Swaps all "key" (i.e. `KeyKind::counts_as_key` returns true) lanes in the reverse order.
    /// See `player::apply_mirror_modf` for the detailed algorithm. (C: `MIRROR_MODF`)
    MirrorModf,
    /// Swaps all "key" lanes in the random order. See `player::apply_shuffle_modf` for
    /// the detailed algorithm. (C: `SHUFFLE_MODF`)
    ShuffleModf,
    /// Swaps all lanes in the random order. (C: `SHUFFLEEX_MODF`)
    ShuffleExModf,
    /// Swaps all "key" lanes in the random order, where the order is determined per object.
    /// See `player::apply_random_modf` for the detailed algorithm. (C: `RANDOM_MODF`)
    RandomModf,
    /// Swaps all lanes in the random order, where the order is determined per object.
    /// (C: `RANDOMEX_MODF`)
    RandomExModf
}

/// Specifies how the BGA is displayed. (C: `enum bga`)
#[deriving(Eq)]
pub enum Bga {
    /// Both the BGA image and movie is displayed. (C: `BGA_AND_MOVIE`)
    BgaAndMovie,
    /// The BGA is displayed but the movie is not loaded. (C: `BGA_BUT_NO_MOVIE`)
    BgaButNoMovie,
    /// The BGA is not displayed. When used with `ExclusiveMode` it also disables the graphical
    /// display entirely. (C: `NO_BGA`)
    NoBga
}

/// Global options set from the command line and environment variables.
pub struct Options {
    /// Game play mode. (C: `opt_mode`)
    mode: Mode,
    /// Modifiers that affect the game data. (C: `opt_modf`)
    modf: Option<Modf>,
    /// Specifies how the BGA is displayed. (C: `opt_bga`)
    bga: Bga,
    /// True if the metadata (either overlaid in the loading screen or printed separately
    /// in the console) is displayed. (C: `opt_showinfo`)
    showinfo: bool,
    /// True if the full screen is enabled. (C: `opt_fullscreen`)
    fullscreen: bool,
    /// An index to the joystick device if any. (C: `opt_joystick`)
    joystick: Option<uint>,
    /// A key specification preset name if any. (C: `preset`)
    preset: Option<~str>,
    /// A left-hand-side key specification if any. (C: `leftkeys`)
    leftkeys: Option<~str>,
    /// A right-hand-side key specification if any. Can be an empty string. (C: `rightkeys`)
    rightkeys: Option<~str>,
    /// An initial play speed. (C: `playspeed`)
    playspeed: float,
}

impl Options {
    /// Returns true if the exclusive mode is enabled. This enables a text-based interface.
    /// (C: `opt_mode >= EXCLUSIVE_MODE`)
    pub fn is_exclusive(&self) -> bool { self.mode == ExclusiveMode }

    /// Returns true if the input is ignored. Escape key or speed-changing keys are still
    /// available as long as the graphical screen is enabled. (C: `!!opt_mode`)
    pub fn is_autoplay(&self) -> bool { self.mode != PlayMode }

    /// Returns true if the BGA is displayed. (C: `opt_bga < NO_BGA`)
    pub fn has_bga(&self) -> bool { self.bga != NoBga }

    /// Returns true if the BGA movie is enabled. (C: `opt_bga < BGA_BUT_NO_MOVIE`)
    pub fn has_movie(&self) -> bool { self.bga == BgaAndMovie }

    /// Returns true if the graphical screen is enabled.
    /// (C: `opt_mode < EXCLUSIVE_MODE || opt_bga < NO_BGA`)
    pub fn has_screen(&self) -> bool { !self.is_exclusive() || self.has_bga() }
}

