// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Global game options.

use std::{char, io, os};
use std::collections::HashMap;
use encoding::label::encoding_from_whatwg_label;

use format::bms::load::LoaderOptions;
use gfx::skin::ast::Skin;
use gfx::skin::parse::load_skin;
use engine::cache::MetadataCache;

/// Game play modes.
#[deriving(PartialEq,Eq,Clone)]
pub enum Mode {
    /// Normal game play. The graphical display and input is enabled.
    Play,
    /// Automatic game play. The graphical display is enabled but the input is mostly ignored
    /// except for the play speed change.
    AutoPlay,
    /// Exclusive (headless) mode. The graphical display is reduced to the BGA or absent at all
    /// (when `Bga::None` is also set).
    Exclusive
}

/// Modifiers that affect the game data.
#[deriving(PartialEq,Eq,Clone)]
pub enum Modifier {
    /// Swaps all "key" (i.e. `KeyKind::counts_as_key` returns true) lanes in the reverse order.
    /// See `player::apply_mirror_modf` for the detailed algorithm.
    Mirror,
    /// Swaps all "key" lanes in the random order. See `player::apply_shuffle_modf` for
    /// the detailed algorithm.
    Shuffle,
    /// Swaps all lanes in the random order.
    ShuffleEx,
    /// Swaps all "key" lanes in the random order, where the order is determined per object.
    /// See `player::apply_random_modf` for the detailed algorithm.
    Random,
    /// Swaps all lanes in the random order, where the order is determined per object.
    RandomEx
}

/// Specifies how the BGA is displayed.
#[deriving(PartialEq,Eq,Clone)]
pub enum Bga {
    /// Both the BGA image and movie is displayed.
    WithMovie,
    /// The BGA is displayed but the movie is not loaded.
    WithoutMovie,
    /// The BGA is not displayed. When used with `PlayMode::Exclusive` it also disables
    /// the graphical display entirely.
    None
}

/// Global options set from the command line and environment variables.
#[deriving(PartialEq,Clone)]
pub struct Options {
    /// Game play mode.
    pub mode: Mode,
    /// Modifiers that affect the game data.
    pub modf: Option<Modifier>,
    /// Specifies how the BGA is displayed.
    pub bga: Bga,
    /// True if the metadata (either overlaid in the loading screen or printed separately
    /// in the console) is displayed.
    pub showinfo: bool,
    /// True if the full screen is enabled.
    pub fullscreen: bool,
    /// An index to the joystick device if any.
    pub joystick: Option<uint>,
    /// A key specification preset name if any.
    pub preset: Option<String>,
    /// A left-hand-side key specification if any.
    pub leftkeys: Option<String>,
    /// A right-hand-side key specification if any. Can be an empty string.
    pub rightkeys: Option<String>,
    /// An initial play speed.
    pub playspeed: f64,
    /// A character encoding *name* forced to the loader.
    pub encoding: Option<String>,
    /// A root path to the data files. This is used to normalize the cached path.
    pub dataroot: Path,
    /// A root path to the skin.
    pub skinroot: Path,
    /// A path to the metadata cache file. When omitted it redirects to the in-memory database.
    pub metadatacache: Option<Path>,

    /// If set, prints the recognized BMS commands after parsing and exits.
    pub debug_dumpbmscommandfull: bool,
    /// If set, prints the recognized BMS commands after parsing and preprocessing and exits.
    pub debug_dumpbmscommand: bool,
    /// If set, prints the fully calculated timeline and exits.
    pub debug_dumptimeline: bool,
}

impl Options {
    /// Returns true if the exclusive mode is enabled. This enables a text-based interface.
    pub fn is_exclusive(&self) -> bool { self.mode == Mode::Exclusive }

    /// Returns true if the input is ignored. Escape key or speed-changing keys are still
    /// available as long as the graphical screen is enabled.
    pub fn is_autoplay(&self) -> bool { self.mode != Mode::Play }

    /// Returns true if the BGA is displayed.
    pub fn has_bga(&self) -> bool { self.bga != Bga::None }

    /// Returns true if the BGA movie is enabled.
    pub fn has_movie(&self) -> bool { self.bga == Bga::WithMovie }

    /// Returns true if the graphical screen is enabled.
    pub fn has_screen(&self) -> bool { !self.is_exclusive() || self.has_bga() }

    /// Returns loader options.
    pub fn loader_options(&self) -> LoaderOptions {
        let mut loaderopts = LoaderOptions::new();
        loaderopts.parser.force_encoding =
            self.encoding.as_ref().and_then(|s| encoding_from_whatwg_label(s[]));
        loaderopts
    }

    /// Parses and returns the skin data.
    pub fn load_skin(&self, name: &str) -> Result<Skin,String> {
        match io::File::open(&self.skinroot.join(name)) {
            Ok(f) => {
                let mut buffered = io::BufferedReader::new(f);
                load_skin(&mut buffered).map(|skin| skin.make_absolute(&self.skinroot))
            },
            Err(err) => Err(err.to_string()),
        }
    }

    /// Opens a metadata cache. If the path is left unspecified, the cache is backed by RAM.
    pub fn open_metadata_cache(&self) -> io::IoResult<MetadataCache> {
        let dataroot = self.dataroot.clone();
        match self.metadatacache {
            Some(ref cache) => MetadataCache::open(dataroot, cache),
            None => MetadataCache::open_in_memory(dataroot),
        }
    }
}

/// A return value from `parse_opts`.
#[deriving(PartialEq,Clone)]
pub enum ParsingResult {
    /// The caller is expected to show the version information.
    ShowVersion,
    /// The caller is expected to show the usage.
    ShowUsage,
    /// The caller is given a path and options.
    PathAndOptions(Path,Options),
    /// The caller should stop the program with given error message.
    Error(String),
}

/// Parses given arguments (excluding the program name) and returns a parsed path to BMS file and
/// options. `get_path` is called only when arguments do not contain the path.
pub fn parse_opts(args: &[String], get_path: || -> Option<Path>) -> ParsingResult {
    let longargs: HashMap<&str,char> = vec![
        ("--help", 'h'), ("--version", 'V'), ("--speed", 'a'),
        ("--autoplay", 'v'), ("--exclusive", 'x'), ("--sound-only", 'X'),
        ("--windowed", 'w'), ("--no-fullscreen", 'w'),
        ("--fullscreen", ' '), ("--info", ' '), ("--no-info", 'q'),
        ("--mirror", 'm'), ("--shuffle", 's'), ("--shuffle-ex", 'S'),
        ("--random", 'r'), ("--random-ex", 'R'), ("--preset", 'k'),
        ("--key-spec", 'K'), ("--bga", ' '), ("--no-bga", 'B'),
        ("--movie", ' '), ("--no-movie", 'M'), ("--joystick", 'j'),
        ("--encoding", 'E'), ("--database-root", 'D'), ("--skin-root", 'Y'),
        ("--debug", 'Z')
    ].into_iter().collect();

    let nargs = args.len();
    let selforcwd = os::self_exe_path().unwrap_or(Path::new("."));

    let mut bmspath = None;
    let mut mode = Mode::Play;
    let mut modf = None;
    let mut bga = Bga::WithMovie;
    let mut showinfo = true;
    let mut fullscreen = true;
    let mut joystick = None;
    let mut preset = None;
    let mut leftkeys = None;
    let mut rightkeys = None;
    let mut playspeed = 1.0;
    let mut encoding = None;
    let mut skinroot = selforcwd.join_many(["res", "skin"][]);
    let mut dataroot = selforcwd.clone();
    let mut metadatacache = None;
    let mut debug_dumpbmscommandfull = false;
    let mut debug_dumpbmscommand = false;
    let mut debug_dumptimeline = false;

    macro_rules! error(
        ($($e:tt)*) => (return ParsingResult::Error(format!($($e)*)))
    )

    let mut i = 0;
    while i < nargs {
        let arg = args[i][];
        if !arg.starts_with("-") {
            if bmspath.is_none() {
                bmspath = Some(Path::new(arg));
            }
        } else if arg == "--" {
            i += 1;
            if bmspath.is_none() && i < nargs {
                bmspath = Some(Path::new(arg));
            }
            break;
        } else {
            let shortargs =
                if arg.starts_with("--") {
                    match longargs.get(&arg) {
                        Some(&c) => c.to_string(),
                        None => error!("Invalid option: {}", arg)
                    }
                } else {
                    arg[1..].to_string()
                };
            let nshortargs = shortargs.len();

            let mut inside = true;
            for (j, c) in shortargs[].chars().enumerate() {
                // Reads the argument of the option. Option string should be consumed first.
                macro_rules! fetch_arg(($opt:expr) => ({
                    let off = if inside {j+1} else {j};
                    let nextarg =
                        if inside && off < nshortargs {
                            // remaining portion of `args[i]` is an argument
                            shortargs[off..]
                        } else {
                            // `args[i+1]` is an argument as a whole
                            i += 1;
                            if i < nargs {
                                args[i][]
                            } else {
                                error!("No argument to the option -{}", $opt);
                            }
                        };
                    inside = false;
                    nextarg
                }));

                match c {
                    'h' => { return ParsingResult::ShowUsage; }
                    'V' => { return ParsingResult::ShowVersion; }
                    'v' => { mode = Mode::AutoPlay; }
                    'x' => { mode = Mode::Exclusive; }
                    'X' => { mode = Mode::Exclusive; bga = Bga::None; }
                    'w' => { fullscreen = false; }
                    'q' => { showinfo = false; }
                    'm' => { modf = Some(Modifier::Mirror); }
                    's' => { modf = Some(Modifier::Shuffle); }
                    'S' => { modf = Some(Modifier::ShuffleEx); }
                    'r' => { modf = Some(Modifier::Random); }
                    'R' => { modf = Some(Modifier::RandomEx); }
                    'k' => { preset = Some(fetch_arg!('k').to_string()); }
                    'K' => { leftkeys = Some(fetch_arg!('K').to_string());
                             rightkeys = Some(fetch_arg!('K').to_string()); }
                    'a' => {
                        match from_str::<f64>(fetch_arg!('a')) {
                            Some(speed) if speed > 0.0 => {
                                playspeed = if speed < 0.1 {0.1}
                                            else if speed > 99.0 {99.0}
                                            else {speed};
                            }
                            _ => error!("Invalid argument to option -a")
                        }
                    }
                    'B' => { bga = Bga::None; }
                    'M' => { bga = Bga::WithoutMovie; }
                    'j' => {
                        match from_str::<uint>(fetch_arg!('j')) {
                            Some(n) => { joystick = Some(n); }
                            _ => error!("Invalid argument to option -j")
                        }
                    }
                    'E' => {
                        let arg = fetch_arg!('E');
                        // since `Encoding`s are not `Send`able we keep only the encoding name
                        match encoding_from_whatwg_label(arg) {
                            Some(..) => { encoding = Some(arg.to_string()); }
                            None => error!("Invalid encoding name: {}", arg)
                        }
                    }
                    'D' => {
                        let arg = fetch_arg!('D');
                        match Path::new_opt(arg[]) {
                            Some(path) => {
                                metadatacache = Some(path.join("metadata.db"));
                                dataroot = path;
                            }
                            None => error!("Invalid database path: {}", arg)
                        }
                    }
                    'Y' => {
                        let arg = fetch_arg!('Y');
                        match Path::new_opt(arg[]) {
                            Some(path) => { skinroot = path; }
                            None => error!("Invalid skin path: {}", arg)
                        }
                    }
                    'Z' => match fetch_arg!('Z') {
                        "dump-bmscommand-full" => { debug_dumpbmscommandfull = true; }
                        "dump-bmscommand" => { debug_dumpbmscommand = true; }
                        "dump-timeline" => { debug_dumptimeline = true; }
                        arg => error!("Unknown debugging option: -Z {}", arg)
                    },
                    ' ' => {} // for ignored long options
                    '1'...'9' => { playspeed = char::to_digit(c, 10).unwrap() as f64; }
                    _ => error!("Invalid option: -{}", c)
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
        None => ParsingResult::ShowUsage,
        Some(bmspath) => ParsingResult::PathAndOptions(bmspath, Options {
            mode: mode,
            modf: modf,
            bga: bga,
            showinfo: showinfo,
            fullscreen: fullscreen,
            joystick: joystick,
            preset: preset,
            leftkeys: leftkeys, rightkeys: rightkeys,
            playspeed: playspeed,
            encoding: encoding,
            dataroot: dataroot,
            skinroot: skinroot,
            metadatacache: metadatacache,
            debug_dumpbmscommandfull: debug_dumpbmscommandfull,
            debug_dumpbmscommand: debug_dumpbmscommand,
            debug_dumptimeline: debug_dumptimeline,
        })
    }
}

