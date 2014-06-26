// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

/*!
 * This is Sonorous, a free music video game written in Rust programming language that supports
 * the [BMS format](http://en.wikipedia.org/wiki/Be-Music_Source) for playing.
 *
 * Sonorous is a continuation of prior attempts to the free music video games: Angolmois (2005--),
 * theseit (2007--2008) and Angolmois Rust edition (2012--). Unlike prior attempts, however,
 * Sonorous aims at the full-featured game with relatively liberal decisions (e.g. languages).
 *
 * At the moment Sonorous is highly experimental, so do not expect high stabillity nor features.
 * Still, you are greatly welcomed to make any suggestions or file any issues: please use 
 * [Github page](https://github.com/snrs/sonorous) for now.
 */

#![crate_id = "sonorous#0.1.0"]
#![crate_type = "bin"]

#![no_start]
#![feature(macro_rules, phase, struct_variant, globs, link_args, unsafe_destructor)]

#![comment = "Sonorous"]
#![license = "GPLv2+"]

#[phase(plugin, link)] extern crate log;
extern crate libc;
extern crate serialize;
extern crate sync;
extern crate native;

extern crate sdl;
extern crate sdl_image;
extern crate sdl_mixer;
extern crate opengles;
extern crate encoding;
extern crate sqlite3;

#[doc(hidden)] mod _linkhacks {
    // this forces `-lc` to be appended to the end of linker arguments.
    // it is important since `-lc` is no longer implied by recent rustc due to `-nodefaultlibs`,
    // thus `-lsqlite3` should be before `-lc` in order to use libc's symbols.
    #[cfg(not(target_os = "win32"))] #[link_args="-lc"] extern {}
}

#[macro_escape] pub mod util {
    /*!
     * General utilities. These modules are usable outside Sonorous, and may split into
     * individual projects in the future.
     */
    pub mod std;
    pub mod macros;
    pub mod lex;
    pub mod filesearch;
    pub mod maybe_owned;
    pub mod envelope;
    pub mod chardet;
    pub mod console;
    pub mod md5;
}

pub mod ext {
    //! Bindings to external libraries or APIs.
    pub mod sdl;
    pub mod smpeg;
    pub mod win32;
}

pub mod gfx {
    //! Various graphics routines.
    pub mod color;
    pub mod ratio_num;
    pub mod surface;
    pub mod gl;
    pub mod draw;
    pub mod bmfont;
    pub mod screen;
    pub mod skin;
}

pub mod format {
    //! Data structures and operations for formats used by music video games.
    pub mod obj;
    pub mod metadata;
    pub mod timeline;
    pub mod pointer;
    pub mod bms;
}

pub mod engine {
    //! Shared game engine modules.
    pub mod keyspec;
    pub mod input;
    pub mod resource;
    pub mod cache;
    pub mod player;
}

#[macro_escape] pub mod ui {
    //! User interface. Most user interaction is implemented via the `Scene` interface.
    pub mod common;
    pub mod hooks;
    pub mod options;
    pub mod init;
    pub mod scene;
    pub mod selecting;
    pub mod loading;
    pub mod viewing;
    pub mod playing;
    pub mod playresult;
}

/// Returns a version string.
pub fn version() -> String { "Sonorous 0.1.0-pre".to_string() }

/// Returns an executable name used in the command line if any.
pub fn exename() -> String {
    let args = std::os::args();
    if args.is_empty() {"sonorous".to_string()} else {args.as_slice()[0].clone()}
}

/// Dumps the recognized BMS commands. This is used by `-Z dump-bmscommand[-full]` debug options.
pub fn dump_bmscommand(bmspath: &Path, full: bool,
                       parseropts: &format::bms::parse::ParserOptions,
                       callback: |line: Option<uint>, msg: format::bms::diag::BmsMessage| -> bool) {
    use format::bms::diag::BmsMessage;
    use format::bms::parse::{BmsUnknown, Parsed, Command, Message, Encoding};
    use format::bms::parse::{Parser, PreprocessingParser};

    let mut f = match std::io::File::open(bmspath) {
        Ok(f) => f,
        Err(err) => die!("Couldn't load BMS file: {}", err)
    };

    // Rust: this cannot be iter_command that receives Iterator<Parsed<'r>> due to the lifetime
    //       problem. I don't know why...
    fn print_command<'r>(parsed: Parsed<'r>, callback: |Option<uint>, BmsMessage| -> bool) {
        match parsed {
            Command(_lineno, BmsUnknown(..)) => {}
            Command(_lineno, cmd) => { util::console::printoutln(cmd.to_str().as_slice()); }
            Message(lineno, msg) => { callback(lineno, msg); }
            Encoding(..) => {}
        }
    }

    if full {
        for parsed in Parser::new(&mut f, parseropts).iter() {
            print_command(parsed, |line, msg| callback(line, msg));
        }
    } else {
        let mut r = std::rand::task_rng();
        for parsed in PreprocessingParser::new(&mut f, &mut r, parseropts).iter() {
            print_command(parsed, |line, msg| callback(line, msg));
        }
    }
}

/// Parses the BMS file, initializes the display, shows the loading screen and runs the game play
/// loop.
pub fn play(bmspath: &Path, opts: ui::options::Options) {
    use std::rc::Rc;
    use std::cell::RefCell;
    use std::collections::HashMap;

    use ui::init::{init_audio, init_video, init_joystick};
    use ui::scene::{Scene, run_scene};
    use ui::selecting::{preprocess_bms, PreprocessedBms, print_diag, SelectingScene};
    use ui::loading::{LoadingScene, TextualLoadingScene};

    fn wrap_opts(opts: ui::options::Options) -> Rc<ui::options::Options> { Rc::new(opts) }

    let scene;
    if bmspath.is_dir() {
        if opts.is_exclusive() {
            die!("Exclusive mode is not usable with the directory path.");
        }

        init_audio();
        for &joyidx in opts.joystick.iter() { init_joystick(joyidx); }
        let screen = Rc::new(RefCell::new(init_video(false, opts.fullscreen)));
        scene = SelectingScene::new(screen, bmspath, wrap_opts(opts)) as Box<Scene>
    } else {
        if opts.debug_dumpbmscommand || opts.debug_dumpbmscommandfull {
            dump_bmscommand(bmspath, opts.debug_dumpbmscommandfull,
                            &opts.loader_options().parser, print_diag);
            ui::common::exit(0);
        }

        // parses the file and sanitizes it
        let mut r = std::rand::task_rng();
        let preproc = match std::io::File::open(bmspath) {
            Ok(mut f) => preprocess_bms(bmspath, &mut f, &opts,
                                        &mut r, &opts.loader_options(), print_diag),
            Err(err) => Err(err.to_str()),
        };
        let PreprocessedBms { bms, infos, keyspec } = match preproc {
            Ok(preproc) => preproc,
            Err(err) => die!("Couldn't load BMS file: {}", err)
        };

        if opts.debug_dumptimeline {
            let _ = bms.timeline.dump(&mut std::io::stdout());
            ui::common::exit(0);
        }

        // initialize SDL
        init_audio();
        for &joyidx in opts.joystick.iter() { init_joystick(joyidx); }

        // initialize the screen if required
        let screen;
        let keymap;
        if opts.has_screen() {
            screen = Some(Rc::new(RefCell::new(init_video(opts.is_exclusive(), opts.fullscreen))));
            // this requires a video subsystem.
            keymap = match engine::input::read_keymap(&keyspec, std::os::getenv) {
                Ok(map) => map,
                Err(err) => die!("{}", err)
            };
        } else {
            screen = None;
            keymap = HashMap::new(); // in this case we explicitly ignore keymaps
        }

        scene = if opts.is_exclusive() {
            TextualLoadingScene::new(screen, bms, infos,
                                     keyspec, keymap, wrap_opts(opts)) as Box<Scene>
        } else {
            LoadingScene::new(screen.unwrap(), bms, infos,
                              keyspec, keymap, wrap_opts(opts)) as Box<Scene>
        };
    }

    run_scene(scene);
    ui::common::exit(0);
}

/// Prints the usage.
pub fn usage() {
    // Rust: this is actually a good use case of `include_str!`...
    let _ = write!(&mut std::io::stderr(), "\
{version}
https://github.com/snrs/sonorous/

Usage: {prog} <options> <path>
  Accepts any BMS, BME, BML or PMS file or the directory path.
  The directory path enables the song/pattern selection mode,
  where game play options are used as the initial settings.

Options:
  -h, --help              This help
  -V, --version           Shows the version
  -a X.X, --speed X.X     Sets the initial play speed (default: 1.0x)
  -1, .., -9              Same as '-a 1.0', .., '-a 9.0'
  -v, --autoplay          Enables AUTO PLAY (viewer) mode
  -x, --exclusive         Enables exclusive (BGA and sound only) mode
  -X, --sound-only        Enables sound only mode, equivalent to -xB
  --fullscreen            Enables the fullscreen mode (default)
  -w, --no-fullscreen     Disables the fullscreen mode
  --info                  Shows a brief information about the song (default)
  -q, --no-info           Do not show an information about the song
  -m, --mirror            Uses a mirror modifier
  -s, --shuffle           Uses a shuffle modifier
  -S, --shuffle-ex        Uses a shuffle modifier, even for scratches
  -r, --random            Uses a random modifier
  -R, --random-ex         Uses a random modifier, even for scratches
  -k NAME, --preset NAME  Forces a use of given key preset (default: bms)
  -K LEFT RIGHT, --key-spec LEFT RIGHT
                          Sets a custom key specification (see the manual)
  --bga                   Loads and shows the BGA (default)
  -B, --no-bga            Do not load and show the BGA
  -M, --no-movie          Do not load and show the BGA movie
  -j N, --joystick N      Enables the joystick with index N (normally 0)
  -E ENCODING             Forces the use of specified encoding
  -D PATH, --database-root PATH
                          Sets the database path which should be writable
  -Y PATH, --skin-root PATH
                          Sets the skin lookup path (default: <root>/res/skin)
  -Z OPTION               Enables the specified debugging option

Environment Variables:
  SNRS_1P_KEYS=<scratch>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<pedal>
  SNRS_2P_KEYS=<pedal>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<scratch>
  SNRS_PMS_KEYS=<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<8>|<9>
  SNRS_SPEED_KEYS=<speed down>|<speed up>
  SNRS_XXy_KEY=<keys for channel XX and channel kind y>
    Sets keys used for game play. Use either SDL key names or joystick names
    like 'button N' or 'axis N' can be used. Separate multiple keys by '%'.
    See the manual for more information.

Available debugging options:
  -Z dump-bmscommand      Dumps recognized BMS commands and exit
  -Z dump-bmscommand-full Same as above but also dumps skipped flow commands
  -Z dump-timeline        Dumps precalculated timeline and exit

", version = version(), prog = exename());
    ui::common::exit(1);
}

/// The entry point for subprograms. Only enabled with `--cfg subprogram`.
#[cfg(subprogram)]
pub fn subprogram(args: &[String]) -> ! {
    let ret: int = match args.head().map(|s| s.as_slice()) {
        None => {
            let _ = write!(&mut std::io::stderr(), "\
The list of available subprograms:
  chardet-train         Trains a character encoding detection algorithm.

");
            0
        }
        Some("chardet-train") => util::chardet::chardet_train(args.tail()),
        Some(prog) => {
            let _ = write!(&mut std::io::stderr(), "Subprogram {} is unknown.", prog);
            1
        }
    };
    ui::common::exit(ret);
}

/// The entry point for subprograms. Only enabled with `--cfg subprogram`.
#[cfg(not(subprogram))]
pub fn subprogram(_args: &[String]) -> ! {
    let _ = write!(&mut std::io::stderr(), "Subprograms are not supported in this build.\n");
    ui::common::exit(1);
}

#[start]
#[doc(hidden)]
#[allow(dead_code)]
fn start(argc: int, argv: **u8) -> int {
    native::start(argc, argv, main)
}

/// The entry point. Parses the command line options and delegates other things to `play`.
pub fn main() {
    use ui::options::*;
    let args = std::os::args();
    let args = args.slice(1, args.len());
    if !args.is_empty() && "--subprogram".equiv(&args[0]) {
        subprogram(args.slice(1, args.len()));
    }
    match parse_opts(args, ui::common::get_path_from_dialog) {
        ShowVersion => { println!("{}", version()); }
        ShowUsage => { usage(); }
        PathAndOptions(bmspath, opts) => { play(&bmspath, opts); }
        Error(err) => { die!("{}", err); }
    }
}

