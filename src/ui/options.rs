// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Global game options.

use std::{char, str, hashmap};

/// Game play modes.
#[deriving(Eq,Clone)]
pub enum Mode {
    /// Normal game play. The graphical display and input is enabled.
    PlayMode,
    /// Automatic game play. The graphical display is enabled but the input is mostly ignored
    /// except for the play speed change.
    AutoPlayMode,
    /// Exclusive (headless) mode. The graphical display is reduced to the BGA or absent at all
    /// (when `NoBga` is also set).
    ExclusiveMode
}

/// Modifiers that affect the game data.
#[deriving(Eq,Clone)]
pub enum Modf {
    /// Swaps all "key" (i.e. `KeyKind::counts_as_key` returns true) lanes in the reverse order.
    /// See `player::apply_mirror_modf` for the detailed algorithm.
    MirrorModf,
    /// Swaps all "key" lanes in the random order. See `player::apply_shuffle_modf` for
    /// the detailed algorithm.
    ShuffleModf,
    /// Swaps all lanes in the random order.
    ShuffleExModf,
    /// Swaps all "key" lanes in the random order, where the order is determined per object.
    /// See `player::apply_random_modf` for the detailed algorithm.
    RandomModf,
    /// Swaps all lanes in the random order, where the order is determined per object.
    RandomExModf
}

/// Specifies how the BGA is displayed.
#[deriving(Eq,Clone)]
pub enum Bga {
    /// Both the BGA image and movie is displayed.
    BgaAndMovie,
    /// The BGA is displayed but the movie is not loaded.
    BgaButNoMovie,
    /// The BGA is not displayed. When used with `ExclusiveMode` it also disables the graphical
    /// display entirely.
    NoBga
}

/// Global options set from the command line and environment variables.
#[deriving(Eq,Clone)]
pub struct Options {
    /// Game play mode.
    mode: Mode,
    /// Modifiers that affect the game data.
    modf: Option<Modf>,
    /// Specifies how the BGA is displayed.
    bga: Bga,
    /// True if the metadata (either overlaid in the loading screen or printed separately
    /// in the console) is displayed.
    showinfo: bool,
    /// True if the full screen is enabled.
    fullscreen: bool,
    /// An index to the joystick device if any.
    joystick: Option<uint>,
    /// A key specification preset name if any.
    preset: Option<~str>,
    /// A left-hand-side key specification if any.
    leftkeys: Option<~str>,
    /// A right-hand-side key specification if any. Can be an empty string.
    rightkeys: Option<~str>,
    /// An initial play speed.
    playspeed: f64,

    /// If set, prints the recognized BMS commands after parsing and exits.
    debug_dumpbmscommandfull: bool,
    /// If set, prints the recognized BMS commands after parsing and preprocessing and exits.
    debug_dumpbmscommand: bool,
    /// If set, prints the fully calculated timeline and exits.
    debug_dumptimeline: bool,
}

impl Options {
    /// Returns true if the exclusive mode is enabled. This enables a text-based interface.
    pub fn is_exclusive(&self) -> bool { self.mode == ExclusiveMode }

    /// Returns true if the input is ignored. Escape key or speed-changing keys are still
    /// available as long as the graphical screen is enabled.
    pub fn is_autoplay(&self) -> bool { self.mode != PlayMode }

    /// Returns true if the BGA is displayed.
    pub fn has_bga(&self) -> bool { self.bga != NoBga }

    /// Returns true if the BGA movie is enabled.
    pub fn has_movie(&self) -> bool { self.bga == BgaAndMovie }

    /// Returns true if the graphical screen is enabled.
    pub fn has_screen(&self) -> bool { !self.is_exclusive() || self.has_bga() }
}

/// A return value from `parse_opts`.
#[deriving(Eq,Clone)]
pub enum ParsingResult {
    /// The caller is expected to show the version information.
    ShowVersion,
    /// The caller is expected to show the usage.
    ShowUsage,
    /// The caller is given a path and options.
    PathAndOptions(Path,~Options),
    /// The caller should stop the program with given error message.
    Error(~str),
}

/// Parses given arguments (excluding the program name) and returns a parsed path to BMS file and
/// options. `get_path` is called only when arguments do not contain the path.
pub fn parse_opts(args: &[~str], get_path: &fn() -> Option<Path>) -> ParsingResult {
    let longargs = (~[
        (~"--help", 'h'), (~"--version", 'V'), (~"--speed", 'a'),
        (~"--autoplay", 'v'), (~"--exclusive", 'x'), (~"--sound-only", 'X'),
        (~"--windowed", 'w'), (~"--no-fullscreen", 'w'),
        (~"--fullscreen", ' '), (~"--info", ' '), (~"--no-info", 'q'),
        (~"--mirror", 'm'), (~"--shuffle", 's'), (~"--shuffle-ex", 'S'),
        (~"--random", 'r'), (~"--random-ex", 'R'), (~"--preset", 'k'),
        (~"--key-spec", 'K'), (~"--bga", ' '), (~"--no-bga", 'B'),
        (~"--movie", ' '), (~"--no-movie", 'M'), (~"--joystick", 'j'),
        (~"--debug", 'Z'),
    ]).move_iter().collect::<hashmap::HashMap<~str,char>>();

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
    let mut debug_dumpbmscommandfull = false;
    let mut debug_dumpbmscommand = false;
    let mut debug_dumptimeline = false;

    let mut i = 0;
    while i < nargs {
        if !args[i].starts_with("-") {
            if bmspath.is_none() {
                bmspath = Some(Path(args[i]));
            }
        } else if args[i] == ~"--" {
            i += 1;
            if bmspath.is_none() && i < nargs {
                bmspath = Some(Path(args[i]));
            }
            break;
        } else {
            let shortargs =
                if args[i].starts_with("--") {
                    match longargs.find(&args[i]) {
                        Some(&c) => str::from_char(c),
                        None => { return Error(format!("Invalid option: {}", args[i])); }
                    }
                } else {
                    args[i].slice_from(1).to_owned()
                };
            let nshortargs = shortargs.len();

            let mut inside = true;
            for (j, c) in shortargs.iter().enumerate() {
                // Reads the argument of the option. Option string should be consumed first.
                macro_rules! fetch_arg(($opt:expr) => ({
                    let off = if inside {j+1} else {j};
                    let nextarg =
                        if inside && off < nshortargs {
                            // remaining portion of `args[i]` is an argument
                            shortargs.slice_from(off)
                        } else {
                            // `args[i+1]` is an argument as a whole
                            i += 1;
                            if i < nargs {
                                let arg: &str = args[i];
                                arg
                            } else {
                                return Error(format!("No argument to the option -{}", $opt));
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
                        match from_str::<f64>(fetch_arg!('a')) {
                            Some(speed) if speed > 0.0 => {
                                playspeed = if speed < 0.1 {0.1}
                                            else if speed > 99.0 {99.0}
                                            else {speed};
                            }
                            _ => { return Error(format!("Invalid argument to option -a")); }
                        }
                    }
                    'B' => { bga = NoBga; }
                    'M' => { bga = BgaButNoMovie; }
                    'j' => {
                        match from_str::<uint>(fetch_arg!('j')) {
                            Some(n) => { joystick = Some(n); }
                            _ => { return Error(format!("Invalid argument to option -j")); }
                        }
                    }
                    'Z' => match fetch_arg!('Z') {
                        &"dump-bmscommand-full" => { debug_dumpbmscommandfull = true; }
                        &"dump-bmscommand" => { debug_dumpbmscommand = true; }
                        &"dump-timeline" => { debug_dumptimeline = true; }
                        arg => { return Error(format!("Unknown debugging option: -Z {}", arg)); }
                    },
                    ' ' => {} // for ignored long options
                    '1'..'9' => { playspeed = char::to_digit(c, 10).unwrap() as f64; }
                    _ => { return Error(format!("Invalid option: -{}", c)); }
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
            mode: mode,
            modf: modf,
            bga: bga,
            showinfo: showinfo,
            fullscreen: fullscreen,
            joystick: joystick,
            preset: preset,
            leftkeys: leftkeys, rightkeys: rightkeys,
            playspeed: playspeed,
            debug_dumpbmscommandfull: debug_dumpbmscommandfull,
            debug_dumpbmscommand: debug_dumpbmscommand,
            debug_dumptimeline: debug_dumptimeline,
        })
    }
}

