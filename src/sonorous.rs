// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

/*!
 * This is a direct, one-to-one translation of Angolmois to Rust programming language.
 * [Angolmois](http://mearie.org/projects/angolmois/) is
 * a [BM98](http://bm98.yaneu.com/bm98/)-like minimalistic music video game which supports
 * the [BMS format](http://en.wikipedia.org/wiki/Be-Music_Source) for playing.
 * 
 * Angolmois is a combination of string parsing, path manipulation, two-dimensional graphics and
 * complex game play carefully packed into some thousand lines of code. This translation is intended
 * to provide an example of translating a moderately-sized C code to Rust, and also to investigate
 * additional library supports required for such moderately-sized programs.
 * 
 * Angolmois is distributed under GNU GPL version 2+, so is this translation. The portions of it is
 * intended to be sent as a patch to Rust, so those portions are licensed under Apache License 2.0
 * and MIT license. Also note that:
 *
 * - This code is known to compile with the following combinations of rustc and rust-sdl:
 *     - rustc 0.7 + rust-sdl `48cb490` 2013-07-02 (an unmerged branch from rossmurray/rust-sdl)
 *
 * - Unlike the original Angolmois code (which sacrifices most comments due to code size concerns),
 *   the Rust version has much more comments which can be beneficial for understanding Angolmois
 *   itself too.
 *
 * # Key
 * 
 * The following notations are used in the comments and documentations.
 * 
 * * (C: ...) - variable/function corresponds to given name in the C code.
 * * Rust: ... - suboptimal translation with a room for improvement in Rust. often contains a Rust
 *   issue number like #1234.
 * * XXX - should be fixed as soon as Rust issue is gone.
 * * TODO - other problems unrelated to Rust.
 *
 * # Common Issues
 *
 * Those issues are common enough that they have to be discussed before the source code.
 *
 * * #3511 - iterator needs to ensure its underlying object available but rvalue lifetime is too
 *           short for it. rooting the underlying object is necessary for now.
 * * #7363 - implicit borrowing of stack closures is currently disabled due to the soundness issue.
 *           can be worked around by wrapping a reference to the closure to another closure.
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

use std::{char, uint, float, str};
use std::num::Round;

// see below for specifics.
use self::util::std::str::*;
use self::util::std::option::*;
use self::util::std::io::*;

#[macro_escape] pub mod util {
    pub mod std;
    pub mod macros;
    pub mod lex;
    pub mod gfx;
    pub mod gl;
    pub mod bmfont;
    pub mod filesearch;
}
pub mod ext {
    pub mod sdl;
    pub mod win32;
}
pub mod format {
    pub mod obj;
    #[path="bms/mod.rs"] pub mod bms;
}
pub mod engine {
    pub mod keyspec;
    pub mod input;
}
#[macro_escape] pub mod ui {
    pub mod common;
    pub mod options;
    pub mod screen;
    pub mod player;
}

/// Returns a version string. (C: `VERSION`)
pub fn version() -> ~str { ~"Angolmois 2.0.0 alpha 2 (rust edition)" }

/// Returns an executable name used in the command line if any. (C: `argv0`)
pub fn exename() -> ~str {
    let args = std::os::args();
    if args.is_empty() {~"angolmois"} else {args[0].clone()}
}

/// Parses the BMS file, initializes the display, shows the loading screen and runs the game play
/// loop. (C: `play`)
pub fn play(bmspath: ~str, opts: ~ui::options::Options) {
    use util::gfx;
    use format::bms;
    use ui::player;

    // parses the file and sanitizes it
    let mut r = std::rand::rng();
    let mut bms = match bms::parse_bms(bmspath, &mut r) {
        Ok(bms) => ~bms,
        Err(err) => die!("Couldn't load BMS file: %s", err)
    };
    bms::sanitize_bms(bms);

    // parses the key specification and further sanitizes `bms` with it
    let keyspec = match engine::keyspec::key_spec(bms, opts.preset.clone(), opts.leftkeys.clone(),
                                                  opts.rightkeys.clone()) {
        Ok(keyspec) => keyspec,
        Err(err) => die!("%s", err)
    };
    engine::keyspec::compact_bms(bms, keyspec);
    let infos = ~bms::analyze_bms(bms);

    // applies the modifier if any
    for opts.modf.iter().advance |&modf| {
        player::apply_modf(bms, modf, &mut r, keyspec, 0, keyspec.split);
        if keyspec.split < keyspec.order.len() {
            player::apply_modf(bms, modf, &mut r, keyspec, keyspec.split, keyspec.order.len());
        }
    }

    let (port, chan) = std::comm::stream();
    chan.send(~(opts, bms, infos, keyspec));

    do ::sdl::start {
        let ~(opts, bms, infos, keyspec) = port.recv();

        // initialize SDL
        player::init_sdl();
        for opts.joystick.iter().advance |&joyidx| { player::init_joystick(joyidx); }

        // read the input mapping (dependent to the SDL initialization)
        let keymap = match engine::input::read_keymap(keyspec, std::os::getenv) {
            Ok(map) => ~map,
            Err(err) => die!("%s", err)
        };

        // uncompress and populate the bitmap font.
        let mut font = ~util::bmfont::Font();
        font.create_zoomed_font(1);
        font.create_zoomed_font(2);
        let font = font;

        // initialize the screen if required
        let mut screen = None;
        if opts.has_screen() {
            screen = Some(player::init_video(opts.is_exclusive(), opts.fullscreen));
        }

        let atexit = || { if opts.is_exclusive() { player::update_line(""); } };

        // render the loading screen
        let mut ticker = ui::common::Ticker();
        let mut saved_screen = None; // XXX should be in a trait actually
        let _ = saved_screen; // Rust: avoids incorrect warning. (#3796)
        let update_status;
        if !opts.is_exclusive() {
            let screen_: &gfx::Surface = screen.get_ref().sdl_surface;
            player::show_stagefile_screen(bms, infos, keyspec, opts, screen_, font);
            if opts.showinfo {
                saved_screen = Some(player::save_screen_for_loading(screen_));
                update_status = |path| {
                    let screen: &gfx::Surface = screen.get_ref().sdl_surface;
                    let saved_screen: &gfx::Surface = *saved_screen.get_ref();
                    player::graphic_update_status(path, screen, saved_screen,
                                                  font, &mut ticker, || atexit()) // XXX #7363
                };
            } else {
                update_status = |_path| {};
            }
        } else if opts.showinfo {
            player::show_stagefile_noscreen(bms, infos, keyspec, opts);
            update_status = |path| {
                player::text_update_status(path, &mut ticker, || atexit()) // XXX #7363
            };
        } else {
            update_status = |_path| {};
        }

        // wait for resources
        let start = ::sdl::get_ticks() + 3000;
        let (sndres, imgres) =
            player::load_resource(bms, opts, |msg| update_status(msg)); // XXX #7363
        if opts.showinfo {
            ticker.reset(); // force update
            update_status(None);
        }
        while ::sdl::get_ticks() < start { player::check_exit(|| atexit()); } // XXX #7363

        // create the player and transfer ownership of other resources to it
        let duration = bms::bms_duration(bms, infos.originoffset,
                                         |sref| sndres[**sref].duration());
        let player = player::Player(opts, bms, infos, duration, keyspec, keymap, sndres);

        // Rust: `@mut` upcasting is unsupported as of 0.6. (#5725)
        fn loop_with_display<T:player::Display>(mut player: player::Player, mut display: T) {
            while player.tick() {
                display.render(&player);
            }
            display.show_result(&player);

            // remove all channels before sound resources are deallocated.
            // halting alone is not sufficient due to rust-sdl's bug.
            ::sdl::mixer::allocate_channels(0);
        }

        // create the display and runs the actual game play loop
        match screen {
            Some(screen) => {
                if player.opts.is_exclusive() {
                    loop_with_display(player, player::BGAOnlyDisplay(screen, imgres));
                } else {
                    let display_ = player::GraphicDisplay(player.opts, player.keyspec,
                                                          screen, font, imgres);
                    match display_ {
                        Ok(display) => loop_with_display(player, display),
                        Err(err) => die!("%s", err)
                    }
                }
            }
            None => {
                loop_with_display(player, player::TextDisplay());
            }
        }

        // it's done!
        atexit();
    }
}

/// Prints the usage. (C: `usage`)
pub fn usage() {
    // Rust: this is actually a good use case of `include_str!`...
    std::io::stderr().write_str(fmt!("\
%s -- the simple BMS player
http://mearie.org/projects/angolmois/
https://github.com/lifthrasiir/angolmois-rust/

Usage: %s <options> <path>
  Accepts any BMS, BME, BML or PMS file.
  Resources should be in the same directory as the BMS file.

Options:
  -h, --help              This help
  -V, --version           Shows the version
  -a #.#, --speed #.#     Sets the initial play speed (default: 1.0x)
  -#                      Same as '-a #.0'
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
  -j #, --joystick #      Enable the joystick with index # (normally 0)

Environment Variables:
  ANGOLMOIS_1P_KEYS=<scratch>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<pedal>
  ANGOLMOIS_2P_KEYS=<pedal>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<scratch>
  ANGOLMOIS_PMS_KEYS=<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<8>|<9>
  ANGOLMOIS_SPEED_KEYS=<speed down>|<speed up>
  ANGOLMOIS_XXy_KEY=<keys for channel XX and channel kind y>
    Sets keys used for game play. Use either SDL key names or joystick names
    like 'button #' or 'axis #' can be used. Separate multiple keys by '%%'.
    See the manual for more information.

", version(), exename()));
    ui::common::exit(1);
}

/// The entry point. Parses the command line options and delegates other things to `play`.
/// (C: `main`)
pub fn main() {
    use ui::options::*;
    use util::std::str::StrUtil;

    let longargs = util::std::hashmap::map_from_vec([
        (~"--help", 'h'), (~"--version", 'V'), (~"--speed", 'a'),
        (~"--autoplay", 'v'), (~"--exclusive", 'x'), (~"--sound-only", 'X'),
        (~"--windowed", 'w'), (~"--no-fullscreen", 'w'),
        (~"--fullscreen", ' '), (~"--info", ' '), (~"--no-info", 'q'),
        (~"--mirror", 'm'), (~"--shuffle", 's'), (~"--shuffle-ex", 'S'),
        (~"--random", 'r'), (~"--random-ex", 'R'), (~"--preset", 'k'),
        (~"--key-spec", 'K'), (~"--bga", ' '), (~"--no-bga", 'B'),
        (~"--movie", ' '), (~"--no-movie", 'M'), (~"--joystick", 'j'),
    ]);

    let args = std::os::args();
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

    let mut i = 1;
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
                        None => die!("Invalid option: %s", args[i])
                    }
                } else {
                    args[i].slice_to_end(1).to_owned()
                };
            let nshortargs = shortargs.len();

            let mut inside = true;
            for shortargs.iter().enumerate().advance |(j, c)| {
                // Reads the argument of the option. Option string should be consumed first.
                let fetch_arg = |opt| {
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
                                die!("No argument to the option -%c", opt);
                            }
                        };
                    inside = false;
                    nextarg
                };

                match c {
                    'h' => { usage(); }
                    'V' => { println(version()); return; }
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
                    'k' => { preset = Some(fetch_arg('k').to_owned()); }
                    'K' => { leftkeys = Some(fetch_arg('K').to_owned());
                             rightkeys = Some(fetch_arg('K').to_owned()); }
                    'a' => {
                        match float::from_str(fetch_arg('a')) {
                            Some(speed) if speed > 0.0 => {
                                playspeed = if speed < 0.1 {0.1}
                                            else if speed > 99.0 {99.0}
                                            else {speed};
                            }
                            _ => die!("Invalid argument to option -a")
                        }
                    }
                    'B' => { bga = NoBga; }
                    'M' => { bga = BgaButNoMovie; }
                    'j' => {
                        match uint::from_str(fetch_arg('j')) {
                            Some(n) => { joystick = Some(n); }
                            _ => die!("Invalid argument to option -j")
                        }
                    }
                    ' ' => {} // for ignored long options
                    '1'..'9' => { playspeed = char::to_digit(c, 10).get() as float; }
                    _ => die!("Invalid option: -%c", c)
                }
                if !inside { break; }
            }
        }
        i += 1;
    }

    // shows a file dialog if the path to the BMS file is missing and the system supports it
    if bmspath.is_none() {
        bmspath = ui::common::get_path_from_dialog();
    }

    match bmspath {
        None => { usage(); }
        Some(bmspath) => {
            let opts = ~Options {
                mode: mode, modf: modf, bga: bga, showinfo: showinfo, fullscreen: fullscreen,
                joystick: joystick, preset: preset, leftkeys: leftkeys, rightkeys: rightkeys,
                playspeed: playspeed
            };
            play(bmspath, opts);
        }
    }
}
