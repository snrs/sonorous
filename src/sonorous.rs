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
 *     - rustc 0.7 + rust-sdl `48cb490` 2013-07-02 (an unmerged branch from rossmurray/rust-sdl)
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
    pub mod timeline;
    pub mod pointer;
    #[path="bms/mod.rs"] pub mod bms;
}
pub mod engine {
    pub mod keyspec;
    pub mod input;
    pub mod resource;
    pub mod player;
}
#[macro_escape] pub mod ui {
    pub mod common;
    pub mod options;
    pub mod screen;
    pub mod init;
    pub mod loading;
    pub mod playing;
}

/// Returns a version string.
pub fn version() -> ~str { ~"Sonorous 0.1.0-pre" }

/// Returns an executable name used in the command line if any.
pub fn exename() -> ~str {
    let args = std::os::args();
    if args.is_empty() {~"sonorous"} else {args[0].clone()}
}

/// Parses the BMS file, initializes the display, shows the loading screen and runs the game play
/// loop.
pub fn play(bmspath: ~str, opts: ~ui::options::Options) {
    use format::bms;
    use engine::player;
    use ui::{init, loading, playing};
    use ui::common::{check_exit, update_line};
    use ui::screen::Screen;

    let callback: bms::diag::BmsMessageCallback = |line, msg| {
        let atline = match line { Some(line) => fmt!(" at line %u", line), None => ~"" };
        warn!("[%s%s] %s", msg.severity().to_str(), atline, msg.to_str());
        true
    };

    if opts.debug_dumpbmscommand {
        let f = match std::io::file_reader(&Path(bmspath)) {
            Ok(f) => f,
            Err(err) => die!("Couldn't load BMS file: %s", err)
        };
        let parseropts = bms::parse::BmsParserOptions { callback: Some(callback) };
        for bms::parse::each_bms_command(f, &parseropts) |cmd| {
            match cmd {
                bms::parse::BmsUnknown(*) => {}
                cmd => { println(cmd.to_str()); }
            }
        }
        ui::common::exit(0);
    }

    // parses the file and sanitizes it
    let mut r = std::rand::rng();
    let loaderopts = bms::load::BmsLoaderOptions { callback: Some(callback) };
    let mut bms = match bms::load::load_bms(bmspath, &mut r, &loaderopts) {
        Ok(bms) => bms,
        Err(err) => die!("Couldn't load BMS file: %s", err)
    };

    // parses the key specification and further sanitizes `bms` with it
    let keyspec = match engine::keyspec::key_spec(&bms, opts.preset.clone(),
                                                  opts.leftkeys.clone(), opts.rightkeys.clone()) {
        Ok(keyspec) => keyspec,
        Err(err) => die!("%s", err)
    };
    keyspec.filter_timeline(&mut bms.timeline);
    let infos = bms.timeline.analyze();

    // applies the modifier if any
    for opts.modf.iter().advance |&modf| {
        player::apply_modf(&mut bms, modf, &mut r, keyspec, 0, keyspec.split);
        if keyspec.split < keyspec.order.len() {
            player::apply_modf(&mut bms, modf, &mut r, keyspec, keyspec.split, keyspec.order.len());
        }
    }

    if opts.debug_dumptimeline {
        bms.timeline.dump(std::io::stdout());
        ui::common::exit(0);
    }

    let (port, chan) = std::comm::stream();
    chan.send(~(opts, bms, infos, keyspec));

    do ::sdl::start {
        let ~(opts, bms, infos, keyspec) = port.recv();

        // initialize SDL
        init::init_audio();
        for opts.joystick.iter().advance |&joyidx| { init::init_joystick(joyidx); }

        // uncompress and populate the bitmap font.
        let mut font = ~util::bmfont::Font();
        font.create_zoomed_font(1);
        font.create_zoomed_font(2);
        let font = font;

        // initialize the screen if required
        let mut screen = None;
        let keymap;
        if opts.has_screen() {
            screen = Some(init::init_video(opts.is_exclusive(), opts.fullscreen));
            // this requires a video subsystem.
            keymap = match engine::input::read_keymap(keyspec, std::os::getenv) {
                Ok(map) => ~map,
                Err(err) => die!("%s", err)
            };
        } else {
            // in this case we explicitly ignore keymaps
            keymap = ~std::hashmap::HashMap::new();
        }

        // Rust: `|| { if opts.is_exclusive() { update_line(~""); } }` segfaults due to
        //       the moved `opts`. (#2202)
        let atexit = if opts.is_exclusive() { || update_line("") } else { || {} };

        // render the loading screen
        let mut ticker = ui::common::Ticker();
        let mut loading_scene = None; // XXX should be in a trait actually
        let _ = loading_scene; // Rust: avoids incorrect warning. (#3796)
        let update_status;
        if !opts.is_exclusive() {
            let screen_: &Screen = screen.get_ref();
            let mut scene = loading::LoadingScene::new(&bms, &infos, keyspec, opts);
            scene.render(screen_, font, None);
            scene.load_stagefile();
            scene.render(screen_, font, None);
            if opts.showinfo {
                loading_scene = Some(scene);
                update_status = |path| {
                    let screen: &Screen = screen.get_ref();
                    let loading_scene: &loading::LoadingScene = loading_scene.get_ref();
                    loading::graphic_update_status(path, screen, loading_scene,
                                                   font, &mut ticker, || atexit()) // XXX #7363
                };
            } else {
                update_status = |_path| {};
            }
        } else if opts.showinfo {
            loading::show_stagefile_noscreen(&bms, &infos, keyspec, opts);
            update_status = |path| {
                loading::text_update_status(path, &mut ticker, || atexit()) // XXX #7363
            };
        } else {
            update_status = |_path| {};
        }

        // wait for resources
        let start = ::sdl::get_ticks() + 3000;
        let (sndres, imgres) =
            loading::load_resource(&bms, opts, |msg| update_status(msg)); // XXX #7363
        if opts.showinfo {
            ticker.reset(); // force update
            update_status(None);
        }
        while ::sdl::get_ticks() < start { check_exit(|| atexit()); } // XXX #7363

        // create the player and transfer ownership of other resources to it
        let duration = bms.timeline.duration(infos.originoffset, |sref| sndres[**sref].duration());
        let mut player = player::Player(opts, bms, infos, duration, keyspec, keymap, sndres);

        // create the display and runs the actual game play loop
        let mut display = match screen {
            Some(screen) => {
                if player.opts.is_exclusive() {
                    @mut playing::BGAOnlyDisplay(screen, imgres) as @mut playing::Display
                } else {
                    match playing::GraphicDisplay(player.opts, player.keyspec,
                                                  screen, font, imgres) {
                        Ok(display) => @mut display as @mut playing::Display,
                        Err(err) => die!("%s", err)
                    }
                }
            }
            None => @mut playing::TextDisplay() as @mut playing::Display
        };

        while player.tick() {
            display.render(&player);
        }
        display.show_result(&player);

        // remove all channels before sound resources are deallocated.
        // halting alone is not sufficient due to rust-sdl's bug.
        ::sdl::mixer::allocate_channels(0);

        // it's done!
        atexit();
    }
}

/// Prints the usage.
pub fn usage() {
    // Rust: this is actually a good use case of `include_str!`...
    std::io::stderr().write_str(fmt!("\
%s
https://github.com/snrs/sonorous/

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
  -j #, --joystick #      Enables the joystick with index # (normally 0)
  -Z OPTION               Enables the specified debugging option

Environment Variables:
  SNRS_1P_KEYS=<scratch>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<pedal>
  SNRS_2P_KEYS=<pedal>|<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<scratch>
  SNRS_PMS_KEYS=<key 1>|<2>|<3>|<4>|<5>|<6>|<7>|<8>|<9>
  SNRS_SPEED_KEYS=<speed down>|<speed up>
  SNRS_XXy_KEY=<keys for channel XX and channel kind y>
    Sets keys used for game play. Use either SDL key names or joystick names
    like 'button #' or 'axis #' can be used. Separate multiple keys by '%%'.
    See the manual for more information.

Available debugging options:
  -Z dump-bmscommand      Dumps recognized BMS commands and exit
  -Z dump-timeline        Dumps precalculated timeline and exit

", version(), exename()));
    ui::common::exit(1);
}

/// The entry point. Parses the command line options and delegates other things to `play`.
pub fn main() {
    use ui::options::*;
    let args = std::os::args();
    let args = args.slice(1, args.len());
    match parse_opts(args, ui::common::get_path_from_dialog) {
        ShowVersion => { println(version()); }
        ShowUsage => { usage(); }
        PathAndOptions(bmspath, opts) => { play(bmspath, opts); }
        Error(err) => { die!("%s", err); }
    }
}

