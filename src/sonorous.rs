// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

/*!
 * This is Sonorous, a free music video game written in Rust programming language that supports
 * the [BMS format](http://en.wikipedia.org/wiki/Be-Music_Source) for playing.
 *
 * Sonorous is a continuation of prior attempts to the free music video games: Angolmois (2005--),
 * theseit (2007--2008) and Angolmois Rust edition (2012--2013). Unlike prior attempts, however,
 * Sonorous aims at the full-featured game with relatively liberal decisions (e.g. languages).
 *
 * At the moment Sonorous is highly experimental, so do not expect high stabillity nor features.
 * Still, you are greatly welcomed to make any suggestions or file any issues: please use 
 * [Github page](https://github.com/snrs/sonorous) for now.
 *
 * # Notes
 *
 * - This code is known to compile with the following combinations of rustc and rust-sdl:
 *     - rustc 0.8 + rust-sdl `5598e68` 2013-10-03
 *
 * - There are several comments with Rust issue numbers like #1234. They are intended to be fixed
 *   after corresponding issues are resolved. The following issues are common enough that we put
 *   the explanation here:
 *     - \#3511 - iterator needs to ensure its underlying object available but rvalue lifetime is
 *       too short for it. rooting the underlying object is necessary for now.
 *     - \#7363 - implicit borrowing of stack closures is currently disabled due to the soundness
 *       issue. can be worked around by wrapping a reference to the closure to another closure.
 */

#[link(name = "sonorous",
       vers = "0.1.0",
       uuid = "6DD4E626-EE80-4D82-AB67-846634B74A19",
       url = "https://github.com/snrs/sonorous/")];

#[comment = "Sonorous"];
#[license = "GPLv2+"];

extern mod extra;
extern mod sdl;
extern mod opengles;
extern mod encoding;

// see below for specifics.
use self::util::std::str::*;
use self::util::std::option::*;

#[macro_escape] pub mod util {
    /*!
     * General utilities. These modules are usable outside Sonorous, and may split into
     * individual projects in the future.
     */
    pub mod std;
    pub mod macros;
    pub mod lex;
    pub mod gfx;
    pub mod gl;
    pub mod bmfont;
    pub mod filesearch;
    pub mod opt_owned;
    pub mod envelope;
    pub mod chardet;
}
pub mod ext {
    //! Bindings to external libraries or APIs.
    pub mod sdl;
    pub mod win32;
}
pub mod format {
    //! Data structures and operations for formats used by music video games.
    pub mod obj;
    pub mod timeline;
    pub mod pointer;
    #[path="bms/mod.rs"] pub mod bms;
}
pub mod engine {
    //! Shared game engine modules.
    pub mod keyspec;
    pub mod input;
    pub mod resource;
    pub mod skin;
    pub mod player;
}
#[macro_escape] pub mod ui {
    //! User interface. Most user interaction is implemented via the `Scene` interface.
    pub mod common;
    pub mod options;
    pub mod screen;
    pub mod init;
    pub mod scene;
    pub mod selecting;
    pub mod loading;
    pub mod viewing;
    pub mod playing;
    pub mod playresult;
}

/// Returns a version string.
pub fn version() -> ~str { ~"Sonorous 0.1.0-pre" }

/// Returns an executable name used in the command line if any.
pub fn exename() -> ~str {
    let args = std::os::args();
    if args.is_empty() {~"sonorous"} else {args[0].clone()}
}

/// Dumps the recognized BMS commands. This is used by `-Z dump-bmscommand[-full]` debug options.
pub fn dump_bmscommand<Listener:format::bms::diag::BmsMessageListener>(
                                bmspath: &Path, full: bool,
                                parseropts: &format::bms::parse::BmsParserOptions,
                                callback: &mut Listener) {
    use std::{io, rand};
    use format::bms::parse::{BmsCommand, BmsUnknown};
    use format::bms::parse::{each_bms_command_with_flow, each_bms_command};

    let f = match io::file_reader(bmspath) {
        Ok(f) => f,
        Err(err) => die!("Couldn't load BMS file: {}", err)
    };
    let blk = |_lineno: uint, cmd: BmsCommand| {
        match cmd {
            BmsUnknown(*) => {}
            cmd => { println(cmd.to_str()); }
        }
        true
    };
    if full {
        each_bms_command_with_flow(f, parseropts, callback, blk);
    } else {
        let mut r = rand::rng();
        each_bms_command(f, &mut r, parseropts, callback, blk);
    }
}

/// Parses the BMS file, initializes the display, shows the loading screen and runs the game play
/// loop.
pub fn play(bmspath: &Path, opts: @ui::options::Options) {
    use std::{rand, os};
    use ui::init::{init_audio, init_video, init_joystick};
    use ui::scene::{Scene, run_scene};
    use ui::selecting::{preprocess_bms, PreprocessedBms, print_diag, SelectingScene};
    use ui::loading::{LoadingScene, TextualLoadingScene};

    let scene;
    if os::path_is_dir(bmspath) {
        if opts.is_exclusive() {
            die!("Exclusive mode is not usable with the directory path.");
        }

        init_audio();
        for &joyidx in opts.joystick.iter() { init_joystick(joyidx); }
        let screen = init_video(false, opts.fullscreen);
        scene = SelectingScene::new(screen, bmspath, opts) as ~Scene:
    } else {
        let mut callback = |line, msg| print_diag(line, msg);

        if opts.debug_dumpbmscommand || opts.debug_dumpbmscommandfull {
            dump_bmscommand(bmspath, opts.debug_dumpbmscommandfull,
                            &opts.loader_options().parser, &mut callback);
            ui::common::exit(0);
        }

        // parses the file and sanitizes it
        let mut r = rand::rng();
        let PreprocessedBms { bms, infos, keyspec } =
            match preprocess_bms(bmspath, opts, &mut r, &opts.loader_options(), &mut callback) {
                Ok(preproc) => *preproc,
                Err(err) => die!("Couldn't load BMS file: {}", err)
            };

        if opts.debug_dumptimeline {
            bms.timeline.dump(std::io::stdout());
            ui::common::exit(0);
        }

        // initialize SDL
        init_audio();
        for &joyidx in opts.joystick.iter() { init_joystick(joyidx); }

        // initialize the screen if required
        let screen;
        let keymap;
        if opts.has_screen() {
            screen = Some(init_video(opts.is_exclusive(), opts.fullscreen));
            // this requires a video subsystem.
            keymap = match engine::input::read_keymap(keyspec, std::os::getenv) {
                Ok(map) => ~map,
                Err(err) => die!("{}", err)
            };
        } else {
            screen = None;
            // in this case we explicitly ignore keymaps
            keymap = ~std::hashmap::HashMap::new();
        }

        scene = if opts.is_exclusive() {
            TextualLoadingScene::new(screen, bms, infos, keyspec, keymap, opts) as ~Scene:
        } else {
            LoadingScene::new(screen.unwrap(), bms, infos, keyspec, keymap, opts) as ~Scene:
        };
    }

    run_scene(scene);
}

/// Prints the usage.
pub fn usage() {
    // Rust: this is actually a good use case of `include_str!`...
    std::io::stderr().write_str(format!("\
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

", version = version(), prog = exename()));
    ui::common::exit(1);
}

#[cfg(subprogram)]
pub fn subprogram(args: &[~str]) -> ! {
    let ret: int = match args.head_opt() {
        None => {
            std::io::stderr().write_str("\
The list of available subprograms:
  chardet-train         Trains a character encoding detection algorithm.

");
            0
        }
        Some(&~"chardet-train") => util::chardet::chardet_train(args.tail()),
        Some(prog) => {
            std::io::stderr().write_str(format!("Subprogram {} is unknown.", *prog));
            1
        }
    };
    ui::common::exit(ret);
}

#[cfg(not(subprogram))]
pub fn subprogram(_args: &[~str]) -> ! {
    std::io::stderr().write_str("Subprograms are not supported in this build.\n");
    ui::common::exit(1);
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
        ShowVersion => { println(version()); }
        ShowUsage => { usage(); }
        PathAndOptions(bmspath, opts) => { play(&bmspath, @*opts); }
        Error(err) => { die!("{}", err); }
    }
}

