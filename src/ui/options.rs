// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Global game options.

use std::{char, uint, float, str};

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

/// A return value from `parse_opts`.
pub enum ParsingResult {
    /// The caller is expected to show the version information.
    ShowVersion,
    /// The caller is expected to show the usage.
    ShowUsage,
    /// The caller is given a path to BMS file and options.
    PathAndOptions(~str,~Options),
    /// The caller should stop the program with given error message.
    Error(~str),
}

/// Parses given arguments (excluding the program name) and returns a parsed path to BMS file and
/// options. `get_path` is called only when arguments do not contain the path.
pub fn parse_opts(args: &[~str], get_path: &fn() -> Option<~str>) -> ParsingResult {
    use util::std::str::StrUtil;
    use util::std::hashmap::map_from_vec;

    let longargs = map_from_vec([
        (~"--help", 'h'), (~"--version", 'V'), (~"--speed", 'a'),
        (~"--autoplay", 'v'), (~"--exclusive", 'x'), (~"--sound-only", 'X'),
        (~"--windowed", 'w'), (~"--no-fullscreen", 'w'),
        (~"--fullscreen", ' '), (~"--info", ' '), (~"--no-info", 'q'),
        (~"--mirror", 'm'), (~"--shuffle", 's'), (~"--shuffle-ex", 'S'),
        (~"--random", 'r'), (~"--random-ex", 'R'), (~"--preset", 'k'),
        (~"--key-spec", 'K'), (~"--bga", ' '), (~"--no-bga", 'B'),
        (~"--movie", ' '), (~"--no-movie", 'M'), (~"--joystick", 'j'),
    ]);

    let nargs = args.len();

    let mut bmspath = None;
    let mut mode = PlayMode;
    let mut modf = None;
    let mut bga = BgaAndMovie;
    let mut showinfo = true;
    let mut fullscreen = true;
    let mut joystick = None;
    let mut preset = None;
    let mut leftkeys = None;
    let mut rightkeys = None;
    let mut playspeed = 1.0;

    let mut i = 0;
    while i < nargs {
        if !args[i].starts_with("-") {
            if bmspath.is_none() {
                bmspath = Some(args[i].clone());
            }
        } else if args[i] == ~"--" {
            i += 1;
            if bmspath.is_none() && i < nargs {
                bmspath = Some(args[i].clone());
            }
            break;
        } else {
            let shortargs =
                if args[i].starts_with("--") {
                    match longargs.find(&args[i]) {
                        Some(&c) => str::from_char(c),
                        None => { return Error(fmt!("Invalid option: %s", args[i])); }
                    }
                } else {
                    args[i].slice_to_end(1).to_owned()
                };
            let nshortargs = shortargs.len();

            let mut inside = true;
            for shortargs.iter().enumerate().advance |(j, c)| {
                // Reads the argument of the option. Option string should be consumed first.
                macro_rules! fetch_arg(($opt:expr) => ({
                    let off = if inside {j+1} else {j};
                    let nextarg =
                        if inside && off < nshortargs {
                            // remaining portion of `args[i]` is an argument
                            shortargs.slice_to_end(off)
                        } else {
                            // `args[i+1]` is an argument as a whole
                            i += 1;
                            if i < nargs {
                                let arg: &str = args[i];
                                arg
                            } else {
                                return Error(fmt!("No argument to the option -%c", $opt));
                            }
                        };
                    inside = false;
                    nextarg
                }));

                match c {
                    'h' => { return ShowUsage; }
                    'V' => { return ShowVersion; }
                    'v' => { mode = AutoPlayMode; }
                    'x' => { mode = ExclusiveMode; }
                    'X' => { mode = ExclusiveMode; bga = NoBga; }
                    'w' => { fullscreen = false; }
                    'q' => { showinfo = false; }
                    'm' => { modf = Some(MirrorModf); }
                    's' => { modf = Some(ShuffleModf); }
                    'S' => { modf = Some(ShuffleExModf); }
                    'r' => { modf = Some(RandomModf); }
                    'R' => { modf = Some(RandomExModf); }
                    'k' => { preset = Some(fetch_arg!('k').to_owned()); }
                    'K' => { leftkeys = Some(fetch_arg!('K').to_owned());
                             rightkeys = Some(fetch_arg!('K').to_owned()); }
                    'a' => {
                        match float::from_str(fetch_arg!('a')) {
                            Some(speed) if speed > 0.0 => {
                                playspeed = if speed < 0.1 {0.1}
                                            else if speed > 99.0 {99.0}
                                            else {speed};
                            }
                            _ => { return Error(fmt!("Invalid argument to option -a")); }
                        }
                    }
                    'B' => { bga = NoBga; }
                    'M' => { bga = BgaButNoMovie; }
                    'j' => {
                        match uint::from_str(fetch_arg!('j')) {
                            Some(n) => { joystick = Some(n); }
                            _ => { return Error(fmt!("Invalid argument to option -j")); }
                        }
                    }
                    ' ' => {} // for ignored long options
                    '1'..'9' => { playspeed = char::to_digit(c, 10).get() as float; }
                    _ => { return Error(fmt!("Invalid option: -%c", c)); }
                }
                if !inside { break; }
            }
        }
        i += 1;
    }

    // shows a file dialog if the path to the BMS file is missing and the system supports it
    if bmspath.is_none() {
        bmspath = get_path();
    }

    match bmspath {
        None => ShowUsage,
        Some(bmspath) => PathAndOptions(bmspath, ~Options {
            mode: mode, modf: modf, bga: bga, showinfo: showinfo, fullscreen: fullscreen,
            joystick: joystick, preset: preset, leftkeys: leftkeys, rightkeys: rightkeys,
            playspeed: playspeed
        })
    }
}

