// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

/*!
 * Game play logics. This module contains whooping 2000+ lines of code, reflecting the fact that
 * Angolmois is not well refactored. (In fact, the game logic is usually hard to refactor, right?)
 */

use sdl::*;
use sdl::video::*;
use sdl::event::*;
use sdl::mixer::*;
use ext::sdl::mixer::*;
use ext::sdl::mpeg::*;
use format::bms::*;
use util::gfx::*;
use util::bmfont::*;
use util::filesearch::resolve_relative_path;
use engine::keyspec::*;
use engine::input::{Input, KeyInput, JoyAxisInput, JoyButtonInput, QuitInput};
use engine::input::{LaneInput, SpeedUpInput, SpeedDownInput};
use engine::input::{InputState, Positive, Neutral, Negative};
use engine::input::{KeyMap};
use ui::common::{Ticker};
use ui::options::*;

/// The width of screen, unless the exclusive mode.
pub static SCREENW: uint = 800;
/// The height of screen, unless the exclusive mode.
pub static SCREENH: uint = 600;
/// The width of BGA, or the width of screen for the exclusive mode.
pub static BGAW: uint = 256;
/// The height of BGA, or the height of screen for the exclusive mode.
pub static BGAH: uint = 256;

//----------------------------------------------------------------------------------------------
// bms utilities

/// Applies given modifier to the game data. The target lanes of the modifier is determined
/// from given key specification. This function should be called twice for the Couple Play,
/// since 1P and 2P should be treated separately. (C: `shuffle_bms`)
pub fn apply_modf<R: ::core::rand::RngUtil>(bms: &mut Bms, modf: Modf, r: &mut R,
                                            keyspec: &KeySpec, begin: uint, end: uint) {
    let mut lanes = ~[];
    for uint::range(begin, end) |i| {
        let lane = keyspec.order[i];
        let kind = keyspec.kinds[*lane];
        if modf == ShuffleExModf || modf == RandomExModf ||
                kind.map_default(false, |&kind| kind.counts_as_key()) {
            lanes.push(lane);
        }
    }

    match modf {
        MirrorModf => apply_mirror_modf(bms, lanes),
        ShuffleModf | ShuffleExModf => apply_shuffle_modf(bms, r, lanes),
        RandomModf | RandomExModf => apply_random_modf(bms, r, lanes)
    }
}

//----------------------------------------------------------------------------------------------
// utilities

/// Checks if the user pressed the escape key or the quit button. `atexit` is called before
/// the program is terminated. (C: `check_exit`)
pub fn check_exit(atexit: &fn()) {
    loop {
        match poll_event() {
            KeyEvent(EscapeKey,_,_,_) | QuitEvent => {
                atexit();
                ::ui::common::exit(0);
            },
            NoEvent => { break; },
            _ => {}
        }
    }
}

/// Writes a line to the console without advancing to the next line. `s` should be short enough
/// to be replaced (currently up to 72 bytes).
pub fn update_line(s: &str) {
    io::stderr().write_str(fmt!("\r%s\r%s", str::repeat(" ", 72), s));
}

//----------------------------------------------------------------------------------------------
// initialization

/// An internal sampling rate for SDL_mixer. Every chunk loaded is first converted to
/// this sampling rate for the purpose of mixing.
static SAMPLERATE: i32 = 44100;

/// The number of bytes in the chunk converted to an internal sampling rate.
static BYTESPERSEC: i32 = SAMPLERATE * 2 * 2; // stereo, 16 bits/sample

/// Creates a small screen for BGAs (`BGAW` by `BGAH` pixels) if `exclusive` is set,
/// or a full-sized screen (`SCREENW` by `SCREENH` pixels) otherwise. `fullscreen` is ignored
/// when `exclusive` is set. (C: `init_video`)
pub fn init_video(exclusive: bool, fullscreen: bool) -> ~Surface {
    let result =
        if exclusive {
            set_video_mode(BGAW as int, BGAH as int, 32, [SWSurface], [DoubleBuf])
        } else if !fullscreen {
            set_video_mode(SCREENW as int, SCREENH as int, 32, [SWSurface], [DoubleBuf])
        } else {
            set_video_mode(SCREENW as int, SCREENH as int, 32, [], [Fullscreen])
        };
    let screen =
        match result {
            Ok(screen) => screen,
            Err(err) => die!("SDL Video Initialization Failure: %s", err)
        };
    if !exclusive {
        mouse::set_cursor_visible(false);
    }
    wm::set_caption(::version(), "");
    screen
}

/// Initializes an SDL, SDL_image and SDL_mixer. (C: `init_ui`)
pub fn init_sdl() {
    if !init([InitVideo, InitAudio, InitJoystick]) {
        die!("SDL Initialization Failure: %s", get_error());
    }
    img::init([img::InitJPG, img::InitPNG]);
    //mixer::init([mixer::InitOGG, mixer::InitMP3]); // TODO
    if mixer::open(SAMPLERATE, audio::S16AudioFormat, audio::Stereo, 2048).is_err() {
        die!("SDL Mixer Initialization Failure");
    }
}

/// Initializes a joystick with given index.
pub fn init_joystick(joyidx: uint) -> ~joy::Joystick {
    joy::ll::SDL_JoystickEventState(1); // TODO rust-sdl patch
    match joy::Joystick::open(joyidx as int) {
        Ok(joy) => joy,
        Err(err) => die!("SDL Joystick Initialization Failure: %s", err)
    }
}

//----------------------------------------------------------------------------------------------
// resource management

/// Alternative file extensions for sound resources. (C: `SOUND_EXTS`)
static SOUND_EXTS: &'static [&'static str] = &[".WAV", ".OGG", ".MP3"];
/// Alternative file extensions for image resources. (C: `IMAGE_EXTS`)
static IMAGE_EXTS: &'static [&'static str] = &[".BMP", ".PNG", ".JPG", ".JPEG", ".GIF"];

/// Returns a specified or implied resource directory from the BMS file.
fn get_basedir(bms: &Bms) -> Path {
    // TODO this logic assumes that #PATH_WAV is always interpreted as a native path, which
    // the C version doesn't assume. this difference barely makes the practical issue though.
    match bms.basepath {
        Some(ref basepath) => { let basepath: &str = *basepath; Path(basepath) }
        None => Path(bms.bmspath).dir_path()
    }
}

/// Sound resource associated to `SoundRef`. It contains the actual SDL_mixer chunk that can be
/// readily played. (C: the type of `sndres`)
enum SoundResource {
    /// No sound resource is associated, or error occurred while loading.
    NoSound,
    /// Sound resource is associated.
    //
    // Rust: ideally this should be just a ~-ptr, but the current borrowck is very constrained
    //       in this aspect. after several attempts I finally sticked to delegate the ownership
    //       to a managed box.
    Sound(@~Chunk) // XXX borrowck
}

pub impl SoundResource {
    /// Returns the associated chunk if any.
    fn chunk(&self) -> Option<@~Chunk> {
        match *self {
            NoSound => None,
            Sound(chunk) => Some(chunk)
        }
    }

    /// Returns the length of associated sound chunk in seconds. This is used for determining
    /// the actual duration of the song in presence of key and background sounds, so it may
    /// return 0.0 if no sound is present.
    fn duration(&self) -> float {
        match *self {
            NoSound => 0.0,
            Sound(chunk) => {
                let chunk = chunk.to_ll_chunk();
                (unsafe {(*chunk).alen} as float) / (BYTESPERSEC as float)
            }
        }
    }
}

/// Loads a sound resource.
fn load_sound(key: Key, path: &str, basedir: &Path) -> SoundResource {
    let res = match resolve_relative_path(basedir, path, SOUND_EXTS) {
        Some(fullpath) => Chunk::from_wav(&fullpath),
        None => Err(~"not found")
    };
    match res {
        Ok(res) => Sound(@res),
        Err(_) => {
            warn!("failed to load sound #WAV%s (%s)", key.to_str(), path);
            NoSound
        }
    }
}

/// Image resource associated to `ImageRef`. It can be either a static image or a movie, and
/// both contains an SDL surface that can be blitted to the screen. (C: the type of `imgres`)
enum ImageResource {
    /// No image resource is associated, or error occurred while loading.
    NoImage,
    /// A static image is associated. The surface may have a transparency which is already
    /// handled by `load_image`.
    Image(@~Surface), // XXX borrowck
    /// A movie is associated. A playback starts when `start_movie` method is called, and stops
    /// when `stop_movie` is called. An associated surface is updated from the separate thread
    /// during the playback.
    Movie(@~Surface, @~MPEG) // XXX borrowck
}

pub impl ImageResource {
    /// Returns an associated surface if any.
    fn surface(&self) -> Option<@~Surface> {
        match *self {
            NoImage => None,
            Image(surface) | Movie(surface,_) => Some(surface)
        }
    }

    /// Stops the movie playback if possible.
    fn stop_movie(&self) {
        match *self {
            NoImage | Image(_) => {}
            Movie(_,mpeg) => { mpeg.stop(); }
        }
    }

    /// Starts (or restarts, if the movie was already being played) the movie playback
    /// if possible.
    fn start_movie(&self) {
        match *self {
            NoImage | Image(_) => {}
            Movie(_,mpeg) => { mpeg.rewind(); mpeg.play(); }
        }
    }
}

/// Loads an image resource.
fn load_image(key: Key, path: &str, opts: &Options, basedir: &Path) -> ImageResource {
    /// Converts a surface to the native display format, while preserving a transparency or
    /// setting a color key if required.
    fn to_display_format(surface: ~Surface) -> Result<~Surface,~str> {
        if unsafe {(*(*surface.raw).format).Amask} != 0 {
            let res = surface.display_format_alpha();
            // Rust: `|&surface|` causes an unchecked copy. (#3224)
            do res.iter |surface| { // XXX #3224
                (*&surface).set_alpha([SrcAlpha, RLEAccel], 255);
            }
            res
        } else {
            let res = surface.display_format();
            do res.iter |surface| { // XXX #3224
                (*&surface).set_color_key([SrcColorKey, RLEAccel], RGB(0,0,0));
            }
            res
        }
    }

    if path.to_lower().ends_with(".mpg") {
        if opts.has_movie() {
            let res = match resolve_relative_path(basedir, path, []) {
                Some(fullpath) => MPEG::from_path(&fullpath),
                None => Err(~"not found")
            };
            match res {
                Ok(movie) => {
                    let surface = match new_surface(BGAW, BGAH) {
                        Ok(surface) => @surface,
                        Err(err) => die!("new_surface failed: %s", err)
                    };
                    movie.enable_video(true);
                    movie.set_loop(true);
                    movie.set_display(*surface);
                    return Movie(surface, @movie);
                }
                Err(_) => { warn!("failed to load image #BMP%s (%s)", key.to_str(), path); }
            }
        }
    } else if opts.has_bga() {
        let res = match resolve_relative_path(basedir, path, IMAGE_EXTS) {
            Some(fullpath) =>
                do img::load(&fullpath).chain |surface| {
                    do to_display_format(surface).chain |surface| {
                        Ok(Image(@surface))
                    }
                },
            None => Err(~"not found")
        };
        match res {
            Ok(res) => { return res; },
            Err(_) => { warn!("failed to load image #BMP%s (%s)", key.to_str(), path); }
        }
    }
    NoImage
}

/// Applies the blit command to given list of image resources. (C: a part of `load_resource`)
fn apply_blitcmd(imgres: &mut [ImageResource], bc: &BlitCmd) {
    let origin: @~Surface = match imgres[**bc.src] {
        Image(src) => src,
        _ => { return; }
    };
    let target: @~Surface = match imgres[**bc.dst] {
        Image(dst) => dst,
        NoImage => {
            let surface = match new_surface(BGAW, BGAH) {
                Ok(surface) => @surface,
                Err(err) => die!("new_surface failed: %s", err)
            };
            surface.fill(RGB(0, 0, 0));
            surface.set_color_key([SrcColorKey, RLEAccel], RGB(0, 0, 0));
            imgres[**bc.dst] = Image(surface);
            surface
        },
        _ => { return; }
    };

    let x1 = cmp::max(bc.x1, 0);
    let y1 = cmp::max(bc.y1, 0);
    let x2 = cmp::min(bc.x2, bc.x1 + BGAW as int);
    let y2 = cmp::min(bc.y2, bc.y1 + BGAH as int);
    target.blit_area(*origin, (x1,y1), (x2,y2), (x2-x1,y2-y1));
}

/// A list of image references displayed in BGA layers (henceforth the BGA state). Not all image
/// referenced here is directly rendered, but the references themselves are kept.
type BGAState = [Option<ImageRef>, ..NLAYERS];

/// Returns the initial BGA state. Note that merely setting a particular layer doesn't start
/// the movie playback; `poorbgafix` in `parser::parse` function handles it.
fn initial_bga_state() -> BGAState {
    [None, None, None, Some(ImageRef(Key(0)))]
}

/// A trait for BGA state.
trait BGAStateOps {
    /// Updates the BGA state. This method prepares given image resources for the next
    /// rendering, notably by starting and stopping the movie playback.
    fn update(&mut self, current: &BGAState, imgres: &[ImageResource]);
    /// Renders the image resources for the specified layers to the specified region of
    /// `screen`.
    fn render(&self, screen: &Surface, layers: &[BGALayer], imgres: &[ImageResource],
              x: uint, y: uint);
}

impl BGAStateOps for BGAState {
    fn update(&mut self, current: &BGAState, imgres: &[ImageResource]) {
        for uint::range(0, NLAYERS) |layer| {
            // TODO this design can't handle the case that a BGA layer is updated to the same
            // image reference, which should rewind the movie playback. the original Angolmois
            // does handle it.
            if self[layer] != current[layer] {
                for self[layer].each |&iref| {
                    imgres[**iref].stop_movie();
                }
                for current[layer].each |&iref| {
                    imgres[**iref].start_movie();
                }
            }
        }
        *self = *current;
    }

    fn render(&self, screen: &Surface, layers: &[BGALayer], imgres: &[ImageResource],
              x: uint, y: uint) {
        screen.fill_area((x,y), (256,256), RGB(0,0,0));
        for layers.each |&layer| {
            for self[layer as uint].each |&iref| {
                for imgres[**iref].surface().each |&surface| {
                    screen.blit_area(&**surface, (0,0), (x,y), (256,256));
                }
            }
        }
    }
}

//----------------------------------------------------------------------------------------------
// loading

/// Returns the interface string common to the graphical and textual loading screen.
fn displayed_info(bms: &Bms, infos: &BmsInfo, keyspec: &KeySpec) -> (~str, ~str, ~str, ~str) {
    let meta = fmt!("Level %d | BPM %.2f%s | %d note%s [%uKEY%s]",
                    bms.playlevel, *bms.initbpm, if infos.hasbpmchange {~"?"} else {~""},
                    infos.nnotes, if infos.nnotes == 1 {~""} else {~"s"}, keyspec.nkeys(),
                    if infos.haslongnote {~"-LN"} else {~""});
    let title = bms.title.clone().get_or_default(~"");
    let genre = bms.genre.clone().get_or_default(~"");
    let artist = bms.artist.clone().get_or_default(~"");
    (meta, title, genre, artist)
}

/// Renders the graphical loading screen by blitting BMS #STAGEFILE image (if any) and showing
/// the metadata. (C: `play_show_stagefile` when `opt_mode < EXCLUSIVE_MODE`)
pub fn show_stagefile_screen(bms: &Bms, infos: &BmsInfo, keyspec: &KeySpec, opts: &Options,
                             screen: &Surface, font: &Font) {
    let (meta, title, genre, artist) = displayed_info(bms, infos, keyspec);

    do screen.with_pixels |pixels| {
        font.print_string(pixels, SCREENW/2, SCREENH/2-16, 2, Centered, "loading bms file...",
                          Gradient(RGB(0x80,0x80,0x80), RGB(0x20,0x20,0x20)));
    }
    screen.flip();

    do screen.with_pixels |pixels| {
        for bms.stagefile.each |&path| {
            let basedir = get_basedir(bms);
            for resolve_relative_path(&basedir, path, IMAGE_EXTS).each |&path| {
                match img::load(&path).chain(|s| s.display_format()) {
                    Ok(surface) => {
                        do surface.with_pixels |srcpixels| {
                            bicubic_interpolation(srcpixels, pixels);
                        }
                    }
                    Err(_) => {}
                }
            }
        }

        if opts.showinfo {
            let bg = RGBA(0x10,0x10,0x10,0x40);
            let fg = Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80));
            for uint::range(0, SCREENW) |i| {
                for uint::range(0, 42) |j| {
                    put_blended_pixel(pixels, i, j, bg); // XXX incorrect lifetime
                }
                for uint::range(SCREENH-20, SCREENH) |j| {
                    put_blended_pixel(pixels, i, j, bg); // XXX incorrect lifetime
                }
            }
            font.print_string(pixels, 6, 4, 2, LeftAligned, title, fg);
            font.print_string(pixels, SCREENW-8, 4, 1, RightAligned, genre, fg);
            font.print_string(pixels, SCREENW-8, 20, 1, RightAligned, artist, fg);
            font.print_string(pixels, 3, SCREENH-18, 1, LeftAligned, meta, fg);
        }
    }

    screen.flip();
}

/// Renders the textual loading screen by printing the metadata.
/// (C: `play_show_stagefile` when `opt_mode >= EXCLUSIVE_MODE`)
pub fn show_stagefile_noscreen(bms: &Bms, infos: &BmsInfo, keyspec: &KeySpec, opts: &Options) {
    if opts.showinfo {
        let (meta, title, genre, artist) = displayed_info(bms, infos, keyspec);
        io::stderr().write_line(fmt!("\
----------------------------------------------------------------------------------------------
Title:    %s\nGenre:    %s\nArtist:   %s\n%s
----------------------------------------------------------------------------------------------",
            title, genre, artist, meta));
    }
}

/// Loads the image and sound resources and calls a callback whenever a new resource has been
/// loaded. (C: `load_resource`)
pub fn load_resource(bms: &Bms, opts: &Options,
                     callback: &fn(Option<~str>)) -> (~[SoundResource], ~[ImageResource]) {
    let basedir = get_basedir(bms);

    let sndres = do bms.sndpath.mapi |i, &path| {
        match path {
            Some(path) => {
                callback(Some(path.clone()));
                load_sound(Key(i as int), path, &basedir)
            },
            None => NoSound
        }
    };
    let mut imgres = do bms.imgpath.mapi |i, &path| {
        match path {
            Some(path) => {
                callback(Some(path.clone()));
                load_image(Key(i as int), path, opts, &basedir)
            },
            None => NoImage
        }
    };

    for bms.blitcmd.each |bc| {
        apply_blitcmd(imgres, bc);
    }
    (sndres, imgres)
}

/// Saves a portion of the screen for the use in `graphic_update_status`.
pub fn save_screen_for_loading(screen: &Surface) -> ~Surface {
    let saved_screen = match new_surface(SCREENW, 20) {
        Ok(surface) => surface,
        Err(err) => die!("new_surface failed: %s", err)
    };
    saved_screen.blit_area(screen, (0,SCREENH-20), (0,0), (SCREENW,20));
    saved_screen
}

/// A callback template for `load_resource` with the graphical loading screen.
/// (C: `resource_loaded`)
pub fn graphic_update_status(path: Option<~str>, screen: &Surface, saved_screen: &Surface,
                             font: &Font, ticker: &mut Ticker, atexit: &fn()) {
    // Rust: `on_tick` calls the closure at most once so `path` won't be referenced twice,
    //       but the analysis can't reason that. (#4654) an "option dance" via
    //       `Option<T>::swap_unwrap` is not helpful here since `path` can be `None`.
    let mut path = path; // XXX #4654
    do ticker.on_tick(get_ticks()) {
        let path = ::core::util::replace(&mut path, None); // XXX #4654
        let msg = path.get_or_default(~"loading...");
        screen.blit_at(saved_screen, 0, (SCREENH-20) as i16);
        do screen.with_pixels |pixels| {
            font.print_string(pixels, SCREENW-3, SCREENH-18, 1, RightAligned, msg,
                              Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x80,0x80,0x80)));
        }
        screen.flip();
    }
    check_exit(atexit);
}

/// A callback template for `load_resource` with the textual loading screen.
/// (C: `resource_loaded`)
pub fn text_update_status(path: Option<~str>, ticker: &mut Ticker, atexit: &fn()) {
    let mut path = path; // XXX #4654
    do ticker.on_tick(get_ticks()) {
        match ::core::util::replace(&mut path, None) { // XXX #4654
            Some(path) => {
                let path = if path.len() < 63 {path} else {path.slice(0, 63).to_owned()};
                update_line(~"Loading: " + path);
            }
            None => { update_line("Loading done."); }
        }
    }
    check_exit(atexit);
}

//----------------------------------------------------------------------------------------------
// game play logics

/// Grades. Angolmois performs the time-based grading as long as possible (it can go wrong when
/// the object is near the discontinuity due to the current implementation strategy).
#[deriving(Eq)]
enum Grade {
    /**
     * Issued when the player did not input the object at all, the player was pressing the key
     * while a bomb passes through the corresponding lane, or failed to unpress the key within
     * the grading area for the end of LN. Resets the combo number, decreases the gauge
     * by severe amount (`MISS_DAMAGE` unless specified by the bomb) and displays the POOR BGA
     * for moments.
     *
     * Several games also use separate grading areas for empty lanes next to the object,
     * in order to avoid continuing the consecutive run ("combo") of acceptable grades by
     * just pressing every keys in the correct timing instead of pressing only lanes containing
     * objects. While this system is not a bad thing (and there are several BMS implementations
     * that use it), it is tricky to implement for the all situations. Angolmois currently
     * does not use this system due to the complexity.
     */
    MISS = 0,
    /// Issued when the player inputed the object and the normalized time difference (that is,
    /// the time difference multiplied by `Player::gradefactor`) between the input point and
    /// the object is between `GOOD_CUTOFF` and `BAD_CUTOFF` milliseconds. Resets the combo
    /// number, decreases the gauge by moderate amount (`BAD_DAMAGE`) and displays the POOR BGA
    /// for moments.
    BAD  = 1,
    /// Issued when the player inputed the object and the normalized time difference is between
    /// `GREAT_CUTOFF` and `GOOD_CUTOFF` milliseconds. Both the combo number and gauge is
    /// left unchanged.
    GOOD = 2,
    /// Issued when the player inputed the object and the normalized time difference is between
    /// `COOL_CUTOFF` and `GREAT_CUTOFF` milliseconds. The combo number is increased by one and
    /// the gauge is replenished by small amount.
    GREAT = 3,
    /// Issued when the player inputed the object and the normalized time difference is less
    /// than `COOL_CUTOFF` milliseconds. The combo number is increased by one and the gauge is
    /// replenished by large amount.
    COOL = 4,
}

/// Required time difference in milliseconds to get at least COOL grade.
static COOL_CUTOFF: float = 14.4;
/// Required time difference in milliseconds to get at least GREAT grade.
static GREAT_CUTOFF: float = 48.0;
/// Required time difference in milliseconds to get at least GOOD grade.
static GOOD_CUTOFF: float = 84.0;
/// Required time difference in milliseconds to get at least BAD grade.
static BAD_CUTOFF: float = 144.0;

/// The number of available grades.
static NGRADES: uint = 5;

/// The maximum (internal) value for the gauge.
static MAXGAUGE: int = 512;
/// A base score per exact input. Actual score can increase by the combo (up to 2x) or decrease
/// by the larger time difference.
static SCOREPERNOTE: float = 300.0;

/// A damage due to the MISS grading. Only applied when the grading is not due to the bomb.
static MISS_DAMAGE: Damage = GaugeDamage(0.059);
/// A damage due to the BAD grading.
static BAD_DAMAGE: Damage = GaugeDamage(0.030);

/// Game play states independent to the display.
pub struct Player {
    /// The game play options.
    opts: ~Options,
    /// The current BMS data.
    //
    // Rust: this should have been just `~Bms`, and `Pointer` should have received a lifetime
    //       parameter (for `&'self Bms` things). in reality, though, a lifetime parameter made
    //       borrowck much stricter and I ended up with wrapping `bms` to a mutable managed box.
    bms: @mut ~Bms,
    /// The derived BMS information.
    infos: ~BmsInfo,
    /// The length of BMS file in seconds as calculated by `bms_duration`. (C: `duration`)
    duration: float,
    /// The key specification.
    keyspec: ~KeySpec,
    /// The input mapping.
    keymap: ~KeyMap,

    /// Set to true if the corresponding object in `bms.objs` had graded and should not be
    /// graded twice. Its length equals to that of `bms.objs`. (C: `nograding` field in
    /// `struct obj`)
    nograding: ~[bool],
    /// Sound resources. (C: `res` field in `sndres`)
    sndres: ~[SoundResource],
    /// A sound chunk used for beeps. It always plays on the channel #0. (C: `beep`)
    beep: ~Chunk,
    /// Last channels in which the corresponding sound in `sndres` was played.
    /// (C: `lastch` field in `sndres`)
    sndlastch: ~[Option<uint>],
    /// Indices to last sounds which the channel has played. For every `x`, if `sndlastch[x] ==
    /// Some(y)` then `sndlastchmap[y] == Some(x)` and vice versa. (C: `sndlastchmap`)
    lastchsnd: ~[Option<uint>],
    /// Currently active BGA layers. (C: `bga`)
    bga: BGAState,

    /// The chart expansion rate, or "play speed". One measure has the length of 400 pixels
    /// times the play speed, so higher play speed means that objects will fall much more
    /// quickly (hence the name). (C: `playspeed`)
    playspeed: float,
    /// The play speed targeted for speed change if any. It is also the value displayed while
    /// the play speed is changing. (C: `targetspeed`)
    targetspeed: Option<float>,
    /// The current BPM. Can be negative, in that case the chart will scroll backwards.
    /// (C: `bpm`)
    bpm: BPM,
    /// The timestamp at the last tick. It is a return value from `sdl::get_ticks` and measured
    /// in milliseconds. (C: `now`)
    now: uint,
    /// The timestamp at the first tick. (C: `origintime`)
    origintime: uint,
    /**
     * The timestamp at the last discontinuity that breaks a linear relationship between
     * the virtual time and actual time. (C: `starttime`) Currently the following are
     * considered a discontinuity:
     *
     * - `origintime`
     * - A change in BPM
     * - A change in scaling factor of measure
     * - A scroll stopper (in this case, `stoptime` is first updated and `starttime` is updated
     *   at the end of stop)
     */
    starttime: uint,
    /// The timestamp at the end of ongoing scroll stopper, if any. (C: `stoptime`)
    stoptime: Option<uint>,
    /// The virtual time at the last discontinuity. (C: `startoffset`)
    startoffset: float,
    /// The current scaling factor of measure. (C: `startshorten`)
    startshorten: float,

    /// The virtual time at the bottom of the visible chart. (C: `bottom`)
    bottom: float,
    /// The virtual time at the grading line. Currently same as `bottom`. (C: `line`)
    line: float,
    /// The virtual time at the top of the visible chart. (C: `top`)
    top: float,
    /// A pointer to the first `Obj` after `bottom`. (C: `pfront`)
    pfront: pointer::Pointer,
    /// A pointer to the first `Obj` after `line`. (C: `pcur`)
    pcur: pointer::Pointer,
    /// A pointer to the first `Obj` that haven't escaped the grading area. It is possible that
    /// this `Obj` haven't reached the grading area either. (C: `pcheck`)
    pcheck: pointer::Pointer,
    /// Pointers to `Obj`s for the start of LN which grading is in progress. (C: `pthru`)
    //
    // Rust: this is intended to be `[Option<pointer::Pointer>, ..NLANES]` but a fixed-size vector
    //       cannot be cloned.
    pthru: ~[Option<pointer::Pointer>],

    /// The scale factor for grading area. The factor less than 1 causes the grading area
    /// shrink. (C: `gradefactor`)
    gradefactor: float,
    /// (C: `grademode` and `gradetime`)
    lastgrade: Option<(Grade,uint)>,
    /// The numbers of each grades. (C: `scocnt`)
    gradecounts: [uint, ..NGRADES],
    /// The last combo number, i.e. the number of objects graded at least GREAT. GOOD doesn't
    /// cause the combo number reset; BAD and MISS do. (C: `scombo`)
    lastcombo: uint,
    /// The best combo number so far. If the player manages to get no BADs and MISSes, then
    /// the combo number should end up with the number of note and LN objects
    /// (`BMSInfo::nnotes`). (C: `smaxcombo`)
    bestcombo: uint,
    /// The current score. (C: `score`)
    score: uint,
    /// The current health gauge. Should be no larger than `MAXGAUGE`. This can go negative
    /// (not displayed directly), which will require players much more efforts to survive.
    /// (C: `gauge`)
    gauge: int,
    /// The health gauge required to survive at the end of the song. Note that the gaugex
    /// less than this value (or even zero) doesn't cause the instant game over;
    /// only `InstantDeath` value from `Damage` does. (C: `survival`)
    survival: int,

    /// The number of keyboard or joystick keys, mapped to each lane and and currently pressed.
    /// (C: `keypressed[0]`)
    keymultiplicity: [uint, ..NLANES],
    /// The state of joystick axes. (C: `keypressed[1]`)
    joystate: [InputState, ..NLANES],
}

/// A list of play speed marks. `SpeedUpInput` and `SpeedDownInput` changes the play speed to
/// the next/previous nearest mark. (C: `speeds`)
static SPEED_MARKS: &'static [float] = &[0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.5, 2.0, 2.5, 3.0,
    3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 7.0, 8.0, 10.0, 15.0, 25.0, 40.0, 60.0, 99.0];

/// Finds the next nearest play speed mark if any.
fn next_speed_mark(current: float) -> Option<float> {
    let mut prev = None;
    for SPEED_MARKS.each |&speed| {
        if speed < current - 0.001 {
            prev = Some(speed);
        } else {
            return prev;
        }
    }
    None
}

/// Finds the previous nearest play speed mark if any.
fn previous_speed_mark(current: float) -> Option<float> {
    let mut next = None;
    for SPEED_MARKS.each_reverse |&speed| {
        if speed > current + 0.001 {
            next = Some(speed);
        } else {
            return next;
        }
    }
    None
}

/// Creates a beep sound played on the play speed change. (C: `create_beep`)
fn create_beep() -> ~Chunk {
    let samples = vec::from_fn::<i32>(12000, // approx. 0.14 seconds
        // sawtooth wave at 3150 Hz, quadratic decay after 0.02 seconds.
        |i| { let i = i as i32; (i%28-14) * cmp::min(2000, (12000-i)*(12000-i)/50000) });
    Chunk::new(unsafe { cast::transmute(samples) }, 128)
}

/// Creates a new player object. The player object owns other related structures, including
/// the options, BMS file, key specification, input mapping and sound resources.
pub fn Player(opts: ~Options, bms: ~Bms, infos: ~BmsInfo, duration: float, keyspec: ~KeySpec,
              keymap: ~KeyMap, sndres: ~[SoundResource]) -> Player {
    let now = get_ticks();
    let initplayspeed = opts.playspeed;
    let originoffset = infos.originoffset;
    let startshorten = bms.shorten(originoffset as int);
    let gradefactor = 1.5 - cmp::min(bms.rank, 5) as float * 0.25;
    let initialgauge = MAXGAUGE * 500 / 1000;
    let survival = MAXGAUGE * 293 / 1000;
    let initbpm = bms.initbpm;
    let nobjs = bms.objs.len();
    let nsounds = sndres.len();

    let bms = @mut bms;
    let initptr = pointer::Pointer(bms);
    let mut player = Player {
        opts: opts, bms: bms, infos: infos, duration: duration,
        keyspec: keyspec, keymap: keymap,

        nograding: vec::from_elem(nobjs, false), sndres: sndres, beep: create_beep(),
        sndlastch: vec::from_elem(nsounds, None), lastchsnd: ~[], bga: initial_bga_state(),

        playspeed: initplayspeed, targetspeed: None, bpm: initbpm, now: now, origintime: now,
        starttime: now, stoptime: None, startoffset: originoffset, startshorten: startshorten,

        bottom: originoffset, line: originoffset, top: originoffset,
        pfront: initptr, pcur: initptr, pcheck: initptr, pthru: ~[None, ..NLANES],

        gradefactor: gradefactor, lastgrade: None, gradecounts: [0, ..NGRADES],
        lastcombo: 0, bestcombo: 0, score: 0, gauge: initialgauge, survival: survival,

        keymultiplicity: [0, ..NLANES], joystate: [Neutral, ..NLANES],
    };

    player.allocate_more_channels(64);
    reserve_channels(1); // so that the beep won't be affected
    player
}

pub impl Player {
    /// Returns true if the specified lane is being pressed, either by keyboard, joystick
    /// buttons or axes.
    fn key_pressed(&self, lane: Lane) -> bool {
        self.keymultiplicity[*lane] > 0 || self.joystate[*lane] != Neutral
    }

    /// Returns the play speed displayed. Can differ from the actual play speed
    /// (`self.playspeed`) when the play speed is changing.
    fn nominal_playspeed(&self) -> float {
        self.targetspeed.get_or_default(self.playspeed)
    }

    /// Updates the score and associated statistics according to grading. `scoredelta` is
    /// an weight normalized to [0,1] that is calculated from the distance between the object
    /// and the input time, and `damage` is an optionally associated `Damage` value for bombs.
    /// May return true when `Damage` resulted in the instant death. (C: `update_grade`)
    fn update_grade(&mut self, grade: Grade, scoredelta: float,
                    damage: Option<Damage>) -> bool {
        self.gradecounts[grade as uint] += 1;
        self.lastgrade = Some((grade, self.now));
        self.score += (scoredelta * SCOREPERNOTE *
                       (1.0 + (self.lastcombo as float) /
                              (self.infos.nnotes as float))) as uint;

        match grade {
            MISS | BAD => { self.lastcombo = 0; }
            GOOD => {}
            GREAT | COOL => {
                // at most 5/512(1%) recover when the combo is topped
                let weight = if grade == GREAT {2} else {3};
                let cmbbonus = cmp::min(self.lastcombo as int, 100) / 50;
                self.lastcombo += 1;
                self.gauge = cmp::min(self.gauge + weight + cmbbonus, MAXGAUGE);
            }
        }
        self.bestcombo = cmp::max(self.bestcombo, self.lastcombo);

        match damage {
            Some(GaugeDamage(ratio)) => {
                self.gauge -= (MAXGAUGE as float * ratio) as int; true
            }
            Some(InstantDeath) => {
                self.gauge = cmp::min(self.gauge, 0); false
            }
            None => true
        }
    }

    /// Same as `update_grade`, but the grade is calculated from the normalized difference
    /// between the object and input time in milliseconds. The normalized distance equals to
    /// the actual time difference when `gradefactor` is 1.0. (C: `update_grade(grade,
    /// scoredelta, 0)` where `grade` and `scoredelta` are pre-calculated from `dist`)
    fn update_grade_from_distance(&mut self, dist: float) {
        let dist = num::abs(dist);
        let (grade, damage) = if      dist <  COOL_CUTOFF {(COOL,None)}
                              else if dist < GREAT_CUTOFF {(GREAT,None)}
                              else if dist <  GOOD_CUTOFF {(GOOD,None)}
                              else if dist <   BAD_CUTOFF {(BAD,Some(BAD_DAMAGE))}
                              else                        {(MISS,Some(MISS_DAMAGE))};
        let scoredelta = cmp::max(1.0 - dist / BAD_CUTOFF, 0.0);
        let keepgoing = self.update_grade(grade, scoredelta, damage);
        assert!(keepgoing);
    }

    /// Same as `update_grade`, but with the predetermined damage value. Always results in MISS
    /// grade. May return true when the damage resulted in the instant death.
    /// (C: `update_grade(0, 0, damage)`)
    fn update_grade_from_damage(&mut self, damage: Damage) -> bool {
        self.update_grade(MISS, 0.0, Some(damage))
    }

    /// Same as `update_grade`, but always results in MISS grade with the standard damage value.
    /// (C: `update_grade(0, 0, 0)`)
    fn update_grade_to_miss(&mut self) {
        let keepgoing = self.update_grade(MISS, 0.0, Some(MISS_DAMAGE));
        assert!(keepgoing);
    }

    /// Allocate more SDL_mixer channels without stopping already playing channels.
    /// (C: `allocate_more_channels`)
    fn allocate_more_channels(&mut self, howmany: uint) {
        let howmany = howmany as libc::c_int;
        let nchannels = allocate_channels(-1 as libc::c_int);
        let nchannels = allocate_channels(nchannels + howmany) as uint;
        if self.lastchsnd.len() < nchannels {
            self.lastchsnd.grow(nchannels, &None);
        }
    }

    /// Plays a given sound referenced by `sref`. `bgm` indicates that the sound is a BGM and
    /// should be played with the lower volume and should in the different channel group from
    /// key sounds. (C: `play_sound`)
    fn play_sound(&mut self, sref: SoundRef, bgm: bool) {
        let sref = **sref as uint;
        let chunk = match self.sndres[sref].chunk() {
            Some(chunk) => chunk,
            None => { return; }
        };
        let lastch = self.sndlastch[sref].map(|&ch| ch as libc::c_int);

        // try to play on the last channel if it is not occupied by other sounds (in this case
        // the last channel info is removed)
        let mut ch;
        loop {
            ch = chunk.play(lastch, 0);
            if ch >= 0 { break; }
            self.allocate_more_channels(32);
        }

        let group = if bgm {1} else {0};
        set_channel_volume(Some(ch), if bgm {96} else {128});
        group_channel(Some(ch), Some(group));

        let ch = ch as uint;
        for self.lastchsnd[ch].each |&idx| { self.sndlastch[idx] = None; }
        self.sndlastch[sref] = Some(ch);
        self.lastchsnd[ch] = Some(sref as uint);
    }

    /// Plays a given sound if `sref` is not zero. This reflects the fact that an alphanumeric
    /// key `00` is normally a placeholder.
    fn play_sound_if_nonzero(&mut self, sref: SoundRef, bgm: bool) {
        if **sref > 0 { self.play_sound(sref, bgm); }
    }

    /// Plays a beep. The beep is always played in the channel 0, which is excluded from
    /// the uniform key sound and BGM management. (C: `Mix_PlayChannel(0, beep, 0)`)
    fn play_beep(&self) {
        self.beep.play(Some(0), 0);
    }

    /// Updates the player state. (C: `play_process`)
    fn tick(&mut self) -> bool {
        // Rust: this is very extreme case of loan conflict. (#4666)
        let bms = &*self.bms;
        let mut pfront = self.pfront.clone();
        let mut pcur = self.pcur.clone();
        let mut pcheck = self.pcheck.clone();
        let mut pthru = self.pthru.clone();

        // smoothly change the play speed
        if self.targetspeed.is_some() {
            let target = self.targetspeed.get();
            let delta = target - self.playspeed;
            if num::abs(delta) < 0.001 {
                self.playspeed = target;
                self.targetspeed = None;
            } else {
                self.playspeed += delta * 0.1;
            }
        }

        // process the ongoing scroll stopper if any
        self.now = get_ticks();
        self.bottom = match self.stoptime {
            Some(t) => {
                if self.now >= t {
                    self.starttime = t;
                    self.stoptime = None;
                }
                self.startoffset
            }
            None => {
                let msecdiff = (self.now - self.starttime) as float;
                let measurediff = self.bpm.msec_to_measure(msecdiff);
                self.startoffset + measurediff / self.startshorten
            }
        };

        // Breaks a continuity at given virtual time.
        let break_continuity = |at: float| {
            assert!(at >= self.startoffset);
            self.starttime += (self.bpm.measure_to_msec(at - self.startoffset) *
                               self.startshorten) as uint;
            self.startoffset = at;
        };

        // process the measure scale factor change
        let bottommeasure = self.bottom.floor();
        let curshorten = bms.shorten(bottommeasure as int);
        if bottommeasure >= -1.0 && self.startshorten != curshorten {
            break_continuity(bottommeasure);
            self.startshorten = curshorten;
        }

        //self.line = bms.adjust_object_time(self.bottom, 0.03 / self.playspeed);
        self.line = self.bottom;
        self.top = bms.adjust_object_time(self.bottom, 1.25 / self.playspeed);
        let lineshorten = bms.shorten(self.line.floor() as int);

        // apply object-like effects while advancing to new `pcur`
        pfront.seek_until(self.bottom);
        let mut prevpcur = pointer::pointer_with_pos(self.bms, pcur.pos);
        for pcur.iter_until(self.line) |&obj| {
            match obj.data {
                BGM(sref) => {
                    self.play_sound_if_nonzero(sref, true);
                }
                SetBGA(layer, iref) => {
                    self.bga[layer as uint] = iref;
                }
                SetBPM(newbpm) => {
                    break_continuity(obj.time);
                    self.bpm = newbpm;
                }
                Stop(duration) => {
                    let msecs = duration.to_msec(self.bpm);
                    let newstoptime = Some(msecs as uint + self.now);
                    self.stoptime = self.stoptime.merge(newstoptime, cmp::max);
                    self.startoffset = obj.time;
                }
                Visible(_,sref) | LNStart(_,sref) => {
                    if self.opts.is_autoplay() {
                        for sref.each |&sref| {
                            self.play_sound_if_nonzero(sref, false);
                        }
                        self.update_grade_from_distance(0.0);
                    }
                }
                _ => {}
            }
        }

        // grade objects that have escaped the grading area
        if !self.opts.is_autoplay() {
            for pcheck.iter_to(pcur) |&obj| {
                let dist = self.bpm.measure_to_msec(self.line - obj.time) *
                           bms.shorten(obj.measure()) * self.gradefactor;
                if dist < BAD_CUTOFF { break; }
                if !self.nograding[pcheck.pos] {
                    for obj.object_lane().each |&Lane(lane)| {
                        let missable =
                            match obj.data {
                                Visible(*) | LNStart(*) => true,
                                LNDone(*) => pthru[lane].is_some(),
                                _ => false,
                            };
                        if missable {
                            self.update_grade_to_miss();
                            pthru[lane] = None;
                        }
                    }
                }
            }
        }

        // process inputs
        loop {
            // map to the virtual input. results in `vkey` (virtual key), `state` (input state)
            // and `continuous` (true if the input is not discrete and `Negative` input state
            // matters).
            let (key, state) = match poll_event() {
                event::NoEvent => { break; }
                ev => match Input::from_event(ev) {
                    Some((QuitInput,_)) => { return false; },
                    Some(key_and_state) => key_and_state,
                    None => loop
                }
            };
            let vkey = match self.keymap.find(&key) {
                Some(&vkey) => vkey,
                None => loop
            };
            let continuous = match key {
                KeyInput(*) | JoyButtonInput(*) | QuitInput => false,
                JoyAxisInput(*) => true
            };

            if self.opts.is_exclusive() { loop; }

            // Returns true if the given lane is previously pressed and now unpressed.
            // When the virtual input is mapped to multiple actual inputs it can update
            // the internal state but still return false.
            let is_unpressed = |lane: Lane, continuous: bool, state: InputState| {
                if state == Neutral || (continuous && self.joystate[*lane] != state) {
                    if continuous {
                        self.joystate[*lane] = state; true
                    } else {
                        if self.keymultiplicity[*lane] > 0 {
                            self.keymultiplicity[*lane] -= 1;
                        }
                        (self.keymultiplicity[*lane] == 0)
                    }
                } else {
                    false
                }
            };

            // Returns true if the given lane is previously unpressed and now pressed.
            // When the virtual input is mapped to multiple actual inputs it can update
            // the internal state but still return false.
            let is_pressed = |lane: Lane, continuous: bool, state: InputState| {
                if state != Neutral {
                    if continuous {
                        self.joystate[*lane] = state; true
                    } else {
                        self.keymultiplicity[*lane] += 1;
                        (self.keymultiplicity[*lane] == 1)
                    }
                } else {
                    false
                }
            };

            let process_unpress = |lane: Lane| {
                // if LN grading is in progress and it is not within the threshold then
                // MISS grade is issued
                for pthru[*lane].each |&thru| {
                    let nextlndone = do thru.find_next_of_type |&obj| {
                        obj.object_lane() == Some(lane) &&
                        obj.is_lndone()
                    };
                    for nextlndone.each |&p| {
                        let delta = self.bpm.measure_to_msec(p.time() - self.line) *
                                    lineshorten * self.gradefactor;
                        if num::abs(delta) < BAD_CUTOFF {
                            self.nograding[p.pos] = true;
                        } else {
                            self.update_grade_to_miss();
                        }
                    }
                }
                pthru[*lane] = None;
            };

            let process_press = |lane: Lane| {
                // plays the closest key sound
                let soundable =
                    do pcur.find_closest_of_type(self.line) |&obj| {
                        obj.object_lane() == Some(lane) && obj.is_soundable()
                    };
                for soundable.each |&p| {
                    for p.sounds().each |&sref| {
                        self.play_sound(sref, false);
                    }
                }

                // tries to grade the closest gradable object in
                // the grading area
                let gradable =
                    do pcur.find_closest_of_type(self.line) |&obj| {
                        obj.object_lane() == Some(lane) && obj.is_gradable()
                    };
                for gradable.each |&p| {
                    if p.pos >= pcheck.pos && !self.nograding[p.pos] && !p.is_lndone() {
                        let dist = self.bpm.measure_to_msec(p.time() - self.line) *
                                   lineshorten * self.gradefactor;
                        if num::abs(dist) < BAD_CUTOFF {
                            if p.is_lnstart() {
                                pthru[*lane] = Some(pointer::pointer_with_pos(self.bms, p.pos));
                            }
                            self.nograding[p.pos] = true;
                            self.update_grade_from_distance(dist);
                        }
                    }
                }
                true
            };

            match (vkey, state) {
                (SpeedDownInput, Positive) | (SpeedDownInput, Negative) => {
                    let current = self.targetspeed.get_or_default(self.playspeed);
                    for next_speed_mark(current).each |&newspeed| {
                        self.targetspeed = Some(newspeed);
                        self.play_beep();
                    }
                }
                (SpeedUpInput, Positive) | (SpeedUpInput, Negative) => {
                    let current = self.targetspeed.get_or_default(self.playspeed);
                    for previous_speed_mark(current).each |&newspeed| {
                        self.targetspeed = Some(newspeed);
                        self.play_beep();
                    }
                }
                (LaneInput(lane), state) => {
                    if !self.opts.is_autoplay() {
                        if is_unpressed(lane, continuous, state) {
                            process_unpress(lane);
                        }
                        if is_pressed(lane, continuous, state) {
                            process_press(lane);
                        }
                    }
                }
                (_, _) => {}
            }

        }

        // process bombs
        if !self.opts.is_autoplay() {
            for prevpcur.iter_to(pcur) |&obj| {
                match obj.data {
                    Bomb(lane,sref,damage) if self.key_pressed(lane) => {
                        // ongoing long note is not graded twice
                        pthru[*lane] = None;
                        for sref.each |&sref| {
                            self.play_sound(sref, false);
                        }
                        if !self.update_grade_from_damage(damage) {
                            // instant death
                            pcur.seek_to_end();
                            return false;
                        }
                    }
                    _ => {}
                }
            }
        }

        self.pfront = pfront;
        self.pcur = pcur;
        self.pcheck = pcheck;
        self.pthru = pthru;

        // determines if we should keep playing
        if self.bottom > (bms.nmeasures + 1) as float {
            if self.opts.is_autoplay() {
                num_playing(None) != num_playing(Some(0))
            } else {
                newest_in_group(Some(1)).is_some()
            }
        } else if self.bottom < self.infos.originoffset {
            false // special casing the negative BPM
        } else {
            true
        }
    }
}

/// Display interface.
pub trait Display {
    /// Renders the current information from `player` to the screen or console. Called after
    /// each call to `Player::tick`.
    pub fn render(&mut self, player: &Player);
    /// Shows the game play result from `player` to the screen or console. Called only once.
    pub fn show_result(&self, player: &Player);
}

//----------------------------------------------------------------------------------------------
// graphic display

/// An appearance for each lane. (C: `struct tkeykind` and `tkeyleft`)
struct LaneStyle {
    /// The left position of the lane in the final screen. (C: `tkeyleft`)
    left: uint,
    /// The left position of the lane in the object sprite. (C: `spriteleft` field)
    spriteleft: uint,
    /// The left position of the lane in the bomb sprite. (C: `spritebombleft` field)
    spritebombleft: uint,
    /// The width of lane. (C: `width` field)
    width: uint,
    /// The base color of object. The actual `Gradient` for drawing is derived from this color.
    /// (C: `basecolor` field)
    basecolor: Color
}

pub impl LaneStyle {
    /// Constructs a new `LaneStyle` object from given key kind and the left (`Left(pos)`) or
    /// right (`Right(pos)`) position. (C: `tkeykinds`)
    fn from_kind(kind: KeyKind, pos: Either<uint,uint>) -> LaneStyle {
        let (spriteleft, spritebombleft, width, color) = match kind {
            WhiteKey    => ( 25,   0, 25, RGB(0x80,0x80,0x80)),
            WhiteKeyAlt => ( 50,   0, 25, RGB(0xf0,0xe0,0x80)),
            BlackKey    => ( 75,   0, 25, RGB(0x80,0x80,0xff)),
            Button1     => (130, 100, 30, RGB(0xe0,0xe0,0xe0)),
            Button2     => (160, 100, 30, RGB(0xff,0xff,0x40)),
            Button3     => (190, 100, 30, RGB(0x80,0xff,0x80)),
            Button4     => (220, 100, 30, RGB(0x80,0x80,0xff)),
            Button5     => (250, 100, 30, RGB(0xff,0x40,0x40)),
            Scratch     => (320, 280, 40, RGB(0xff,0x80,0x80)),
            FootPedal   => (360, 280, 40, RGB(0x80,0xff,0x80)),
        };
        let left = pos.either(|&left| left, |&right| right - width);
        LaneStyle { left: left, spriteleft: spriteleft, spritebombleft: spritebombleft,
                    width: width, basecolor: color }
    }

    /// Renders required object and bomb images to the sprite.
    fn render_to_sprite(&self, sprite: &Surface) {
        let left = self.spriteleft;
        let noteleft = self.spriteleft + SCREENW;
        let bombleft = self.spritebombleft + SCREENW;
        assert!(sprite.get_width() as uint >= cmp::max(noteleft, bombleft) + self.width);

        // render a background sprite (0 at top, <1 at bottom)
        let backcolor = Gradient { zero: RGB(0,0,0), one: self.basecolor };
        for uint::range(140, SCREENH - 80) |i| {
            sprite.fill_area((left, i), (self.width, 1), backcolor.blend(i as int - 140, 1000));
        }

        // render note and bomb sprites (1/2 at middle, 1 at border)
        let denom = self.width as int;
        let notecolor = Gradient { zero: RGB(0xff,0xff,0xff), one: self.basecolor };
        let bombcolor = Gradient { zero: RGB(0,0,0),          one: RGB(0xc0,0,0) };
        for uint::range(0, self.width / 2) |i| {
            let num = (self.width - i) as int;
            sprite.fill_area((noteleft+i, 0), (self.width-i*2, SCREENH),
                             notecolor.blend(num, denom));
            sprite.fill_area((bombleft+i, 0), (self.width-i*2, SCREENH),
                             bombcolor.blend(num, denom));
        }
    }

    /// Renders the lane background to the screen from the sprite.
    fn render_back(&self, screen: &Surface, sprite: &Surface, pressed: bool) {
        screen.fill_area((self.left, 30), (self.width, SCREENH-110), RGB(0,0,0));
        if pressed {
            screen.blit_area(sprite, (self.spriteleft, 140), (self.left, 140),
                             (self.width, SCREENH-220));
        }
    }

    /// Renders an object to the screen from the sprite.
    fn render_note(&self, screen: &Surface, sprite: &Surface, top: uint, bottom: uint) {
        screen.blit_area(sprite, (self.spriteleft + SCREENW, 0),
                         (self.left, top), (self.width, bottom - top));
    }

    /// Renders a bomb object to the screen from the sprite.
    fn render_bomb(&self, screen: &Surface, sprite: &Surface, top: uint, bottom: uint) {
        screen.blit_area(sprite, (self.spritebombleft + SCREENW, 0),
                         (self.left, top), (self.width, bottom - top));
    }
}

/// Builds a list of `LaneStyle`s from the key specification.
fn build_lane_styles(keyspec: &KeySpec) ->
                                Result<(uint, Option<uint>, ~[(Lane,LaneStyle)]), ~str> {
    let mut leftmost = 0, rightmost = SCREENW;
    let mut styles = ~[];
    for keyspec.each_left_lanes |&lane| {
        let kind = keyspec.kinds[*lane];
        assert!(kind.is_some());
        let kind = kind.get();
        let style = LaneStyle::from_kind(kind, Left(leftmost));
        styles.push((lane, style));
        leftmost += style.width + 1;
        if leftmost > SCREENW - 20 {
            return Err(~"The screen can't hold that many lanes");
        }
    }
    for keyspec.each_right_lanes |&lane| {
        let kind = keyspec.kinds[*lane];
        assert!(kind.is_some());
        let kind = kind.get();
        let style = LaneStyle::from_kind(kind, Right(rightmost));
        styles.push((lane, style));
        if rightmost < leftmost + 40 {
            return Err(~"The screen can't hold that many lanes");
        }
        rightmost -= style.width + 1;
    }
    let mut rightmost = if rightmost == SCREENW {None} else {Some(rightmost)};

    // move lanes to the center if there are too small number of lanes
    let cutoff = 165;
    if leftmost < cutoff {
        for uint::range(0, keyspec.split) |i| {
            let mut (lane, style) = styles[i];
            style.left += (cutoff - leftmost) / 2;
            styles[i] = (lane, style);
        }
        leftmost = cutoff;
    }
    if rightmost.map_default(false, |&x| x > SCREENW - cutoff) {
        for uint::range(keyspec.split, styles.len()) |i| {
            let mut (lane, style) = styles[i];
            style.left -= (rightmost.get() - (SCREENW - cutoff)) / 2;
            styles[i] = (lane, style);
        }
        rightmost = Some(SCREENW - cutoff);
    }

    Ok((leftmost, rightmost, styles))
}

/// Creates a sprite. (C: sprite construction portion of `play_prepare`)
fn create_sprite(opts: &Options, leftmost: uint, rightmost: Option<uint>,
                 styles: &[(Lane,LaneStyle)]) -> ~Surface {
    let sprite = match new_surface(SCREENW + 400, SCREENH) {
        Ok(surface) => surface,
        Err(err) => die!("new_surface failed: %s", err)
    };
    let black = RGB(0,0,0);
    let gray = RGB(0x40,0x40,0x40); // gray used for separators

    // render notes and lane backgrounds
    for styles.each |&(_lane,style)| {
        style.render_to_sprite(sprite);
    }

    // render panels
    do sprite.with_pixels |pixels| {
        let topgrad = Gradient { zero: RGB(0x60,0x60,0x60), one: RGB(0xc0,0xc0,0xc0) };
        let botgrad = Gradient { zero: RGB(0x40,0x40,0x40), one: RGB(0xc0,0xc0,0xc0) };
        for int::range(-244, 556) |j| {
            for int::range(-10, 20) |i| {
                let c = (i*2+j*3+750) % 2000;
                put_pixel(pixels, (j+244) as uint, (i+10) as uint, // XXX incorrect lifetime
                          topgrad.blend(850 - num::abs(c-1000), 700));
            }
            for int::range(-20, 60) |i| {
                let c = (i*3+j*2+750) % 2000;
                let bottom = (SCREENH - 60) as int;
                put_pixel(pixels, (j+244) as uint, (i+bottom) as uint, // XXX incorrect lifetime
                          botgrad.blend(850 - num::abs(c-1000), 700));
            }
        }
    }
    sprite.fill_area((10, SCREENH-36), (leftmost, 1), gray);

    // erase portions of panels left unused
    let leftgap = leftmost + 20;
    let rightgap = rightmost.map_default(SCREENW, |x| x - 20);
    let gapwidth = rightgap - leftgap;
    sprite.fill_area((leftgap, 0), (gapwidth, 30), black);
    sprite.fill_area((leftgap, SCREENH-80), (gapwidth, 80), black);
    do sprite.with_pixels |pixels| {
        for uint::range(0, 20) |i| {
            for uint::range_rev(20, 0) |j| {
                if i*i + j*j <= 400 { break; } // circled border
                put_pixel(pixels, leftmost + j, 10 + i, black); // XXX incorrect lifetime
                put_pixel(pixels, leftmost + j, (SCREENH-61) - i, black); // XXX
                for rightmost.each |&right| {
                    put_pixel(pixels, (right-j) - 1, 10 + i, black); // XXX incorrect lifetime
                    put_pixel(pixels, (right-j) - 1, (SCREENH-61) - i, black); // XXX
                }
            }
        }
    }

    // draw the gauge bar if needed
    if !opts.is_autoplay() {
        sprite.fill_area((0, SCREENH-16), (368, 16), gray);
        sprite.fill_area((4, SCREENH-12), (360, 8), black);
    }

    sprite
}

/// Full-featured graphic display. Used for the normal game play and automatic play mode.
pub struct GraphicDisplay {
    /// Sprite surface generated by `create_sprite`. (C: `sprite`)
    sprite: ~Surface,
    /// Display screen. (C: `screen`)
    screen: ~Surface,
    /// Bitmap font.
    font: ~Font,
    /// Image resources. (C: `imgres`)
    imgres: ~[ImageResource],

    /// The leftmost X coordinate of the area next to the lanes, that is, the total width of
    /// left-hand-side lanes. (C: `tpanel1`)
    leftmost: uint,
    /// The rightmost X coordinate of the area next to the lanes, that is, the screen width
    /// minus the total width of right-hand-side lanes if any. `None` indicates the absence of
    /// right-hand-side lanes. (C: `tpanel2`)
    rightmost: Option<uint>,
    /// The order and appearance of lanes. (C: `tkey` and `tkeyleft`)
    lanestyles: ~[(Lane,LaneStyle)],
    /// The left coordinate of the BGA. (C: `tbgax`)
    bgax: uint,
    /// The top coordinate of the BGA. (C: `tbgay`)
    bgay: uint,

    /// If not `None`, indicates that the POOR BGA should be displayed until this timestamp.
    /// (C: `poorlimit`)
    poorlimit: Option<uint>,
    /// If not `None`, indicates that the grading information should be displayed until
    /// this timestamp. (C: `gradetime`)
    gradelimit: Option<uint>,
    /// Currently known state of BGAs.
    lastbga: BGAState,
}

/// Creates a new graphic display from the options, key specification, pre-allocated (usually
/// by `init_video`) screen, pre-created bitmap fonts and pre-loaded image resources. The last
/// three are owned by the display, others are not (in fact, should be owned by `Player`).
pub fn GraphicDisplay(opts: &Options, keyspec: &KeySpec, screen: ~Surface, font: ~Font,
                      imgres: ~[ImageResource]) -> Result<GraphicDisplay,~str> {
    let (leftmost, rightmost, styles) = match build_lane_styles(keyspec) {
        Ok(styles) => styles,
        Err(err) => { return Err(err); }
    };
    let centerwidth = rightmost.get_or_default(SCREENW) - leftmost;
    let bgax = leftmost + (centerwidth - BGAW) / 2;
    let bgay = (SCREENH - BGAH) / 2;
    let sprite = create_sprite(opts, leftmost, rightmost, styles);

    let display = GraphicDisplay {
        sprite: sprite, screen: screen, font: font, imgres: imgres,
        leftmost: leftmost, rightmost: rightmost, lanestyles: styles, bgax: bgax, bgay: bgay,
        poorlimit: None, gradelimit: None, lastbga: initial_bga_state(),
    };

    display.screen.fill(RGB(0,0,0));
    display.restore_panel();
    display.screen.flip();

    Ok(display)
}

/// The list of grade names and corresponding color scheme. (C: `tgradestr` and `tgradecolor`)
static GRADES: &'static [(&'static str,Gradient)] = &[
    // Rust: can we just use `Gradient()`???
    ("MISS",  Gradient { zero: RGB(0xff,0xc0,0xc0), one: RGB(0xff,0x40,0x40) }),
    ("BAD",   Gradient { zero: RGB(0xff,0xc0,0xff), one: RGB(0xff,0x40,0xff) }),
    ("GOOD",  Gradient { zero: RGB(0xff,0xff,0xc0), one: RGB(0xff,0xff,0x40) }),
    ("GREAT", Gradient { zero: RGB(0xc0,0xff,0xc0), one: RGB(0x40,0xff,0x40) }),
    ("COOL",  Gradient { zero: RGB(0xc0,0xc0,0xff), one: RGB(0x40,0x40,0xff) }),
];

impl GraphicDisplay {
    /// Restores the panels by blitting upper and bottom panels to the screen.
    fn restore_panel(&self) {
        let screen: &Surface = self.screen;
        let sprite: &Surface = self.sprite;
        screen.blit_area(sprite, (0,0), (0,0), (SCREENW,30));
        screen.blit_area(sprite, (0,SCREENH-80), (0,SCREENH-80), (SCREENW,80));
    }
}

impl Display for GraphicDisplay {
    fn render(&mut self, player: &Player) {
        let screen = &*self.screen;
        let sprite = &*self.sprite;
        let font = &*self.font;
        let bms = &*player.bms;

        // update display states
        let mut poorlimit = self.poorlimit;
        let mut gradelimit = self.gradelimit;
        for player.lastgrade.each |&(grade,when)| {
            if grade == MISS {
                // switches to the normal BGA after 600ms
                poorlimit = poorlimit.merge(Some(when + 600), cmp::max);
            }
            // grade disappears after 700ms
            gradelimit = gradelimit.merge(Some(when + 700), cmp::max);
        }
        if poorlimit < Some(player.now) { poorlimit = None; }
        if gradelimit < Some(player.now) { gradelimit = None; }
        self.lastbga.update(&player.bga, self.imgres);
        *&mut self.poorlimit = poorlimit;
        *&mut self.gradelimit = gradelimit;

        // render BGAs (should render before the lanes since lanes can overlap with BGAs)
        if player.opts.has_bga() {
            let layers = if poorlimit.is_some() {&[PoorBGA]} else {&[Layer1, Layer2, Layer3]};
            self.lastbga.render(self.screen, layers, self.imgres, self.bgax, self.bgay);
        }

        // fill the lanes to the border color
        screen.fill_area((0, 30), (self.leftmost, SCREENH-110), RGB(0x40,0x40,0x40));
        for self.rightmost.each |&rightmost| {
            screen.fill_area((rightmost, 30), (SCREENH-rightmost, 490), RGB(0x40,0x40,0x40));
        }
        for self.lanestyles.each |&(lane,style)| {
            style.render_back(screen, sprite, player.key_pressed(lane));
        }

        // set the clip area to avoid drawing on the panels
        screen.set_clip_area((0, 30), (SCREENW, SCREENH-110));

        // render objects
        let time_to_y = |time| {
            let adjusted = bms.adjust_object_position(player.bottom, time);
            (SCREENH-70) - (400.0 * player.playspeed * adjusted) as uint
        };
        for self.lanestyles.each |&(lane,style)| {
            let front = do player.pfront.find_next_of_type |&obj| {
                obj.object_lane() == Some(lane) && obj.is_renderable()
            };
            if front.is_none() { loop; }
            let front = front.get();

            // LN starting before the bottom and ending after the top
            if front.time() > player.top && front.is_lndone() {
                style.render_note(screen, sprite, 30, SCREENH - 80);
            } else {
                let mut i = front.pos;
                let mut nextbottom = None;
                let nobjs = bms.objs.len(), top = player.top;
                while i < nobjs && bms.objs[i].time <= top {
                    let y = time_to_y(bms.objs[i].time);
                    match bms.objs[i].data {
                        LNStart(lane0,_) if lane0 == lane => {
                            assert!(nextbottom.is_none());
                            nextbottom = Some(y);
                        }
                        LNDone(lane0,_) if lane0 == lane => {
                            let bottom = SCREENH-80;
                            style.render_note(screen, sprite, y,
                                              nextbottom.get_or_default(bottom));
                            nextbottom = None;
                        }
                        Visible(lane0,_) if lane0 == lane => {
                            assert!(nextbottom.is_none());
                            style.render_note(screen, sprite, y-5, y);
                        }
                        Bomb(lane0,_,_) if lane0 == lane => {
                            assert!(nextbottom.is_none());
                            style.render_bomb(screen, sprite, y-5, y);
                        }
                        _ => {}
                    }
                    i += 1;
                }

                for nextbottom.each |&y| {
                    style.render_note(screen, sprite, 30, y);
                }
            }
        }

        // render measure bars
        for int::range(player.bottom.floor() as int, player.top.floor() as int + 1) |i| {
            let y = time_to_y(i as float);
            screen.fill_area((0, y), (self.leftmost, 1), RGB(0xc0,0xc0,0xc0));
            for self.rightmost.each |&rightmost| {
                screen.fill_area((rightmost, y), (800-rightmost, 1), RGB(0xc0,0xc0,0xc0));
            }
        }

        // render grading text
        if gradelimit.is_some() && player.lastgrade.is_some() {
            let gradelimit = gradelimit.get();
            let (lastgrade,_) = player.lastgrade.get();
            let (gradename,gradecolor) = GRADES[lastgrade as uint];
            let delta = (cmp::max(gradelimit - player.now, 400) - 400) / 15;
            do screen.with_pixels |pixels| {
                font.print_string(pixels, self.leftmost/2, SCREENH/2 - 40 - delta, 2,
                                  Centered, gradename, gradecolor);
                if player.lastcombo > 1 {
                    font.print_string(pixels, self.leftmost/2, SCREENH/2 - 12 - delta, 1,
                                      Centered, fmt!("%u COMBO", player.lastcombo),
                                      Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80)));
                }
                if player.opts.is_autoplay() {
                    font.print_string(pixels, self.leftmost/2, SCREENH/2 + 2 - delta, 1,
                                      Centered, "(AUTO)",
                                      Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x40,0x40,0x40)));
                }
            }
        }

        screen.set_clip_rect(&screen.get_rect());

        self.restore_panel();

        // render panel
        let elapsed = (player.now - player.origintime) / 1000;
        let duration = player.duration as uint;
        let durationmsec = (player.duration * 1000.0) as uint;
        do screen.with_pixels |pixels| {
            let black = RGB(0,0,0);
            font.print_string(pixels, 10, 8, 1, LeftAligned,
                              fmt!("SCORE %07u", player.score), black);
            let nominalplayspeed = player.nominal_playspeed();
            font.print_string(pixels, 5, SCREENH-78, 2, LeftAligned,
                              fmt!("%4.1fx", nominalplayspeed), black);
            font.print_string(pixels, self.leftmost-94, SCREENH-35, 1, LeftAligned,
                              fmt!("%02u:%02u / %02u:%02u", elapsed/60, elapsed%60,
                                                            duration/60, duration%60), black);
            font.print_string(pixels, 95, SCREENH-62, 1, LeftAligned,
                              fmt!("@%9.4f", player.bottom), black);
            font.print_string(pixels, 95, SCREENH-78, 1, LeftAligned,
                              fmt!("BPM %6.2f", *player.bpm), black);
            let timetick =
                cmp::min(self.leftmost, (player.now - player.origintime) *
                                        self.leftmost / durationmsec);
            font.print_glyph(pixels, 6 + timetick, SCREENH-52, 1,
                             95, RGB(0x40,0x40,0x40)); // glyph #95: tick
        }

        // render gauge
        if !player.opts.is_autoplay() {
            // cycles four times per measure, [0,40)
            let cycle = (160.0 * player.startshorten * player.bottom).floor() % 40.0;
            let width = if player.gauge < 0 {0}
                        else {player.gauge * 400 / MAXGAUGE - (cycle as int)};
            let width = ::util::core::cmp::clamp(5, width, 360);
            let color = if player.gauge >= player.survival {RGB(0xc0,0,0)}
                        else {RGB(0xc0 - ((cycle * 4.0) as u8), 0, 0)};
            screen.fill_area((4, SCREENH-12), (width, 8), color);
        }

        screen.flip();
    }

    fn show_result(&self, player: &Player) {
        if player.opts.is_autoplay() { return; }

        // check if the song reached the last gradable object (otherwise the game play was
        // terminated by the user)
        let nextgradable = player.pcur.find_next_of_type(|obj| obj.is_gradable());
        if nextgradable.is_some() { return; }

        if player.gauge >= player.survival {
            io::println(fmt!("*** CLEARED! ***\n\
                              COOL  %4u    GREAT %4u    GOOD  %4u\n\
                              BAD   %4u    MISS  %4u    MAX COMBO %u\n\
                              SCORE %07u (max %07d)",
                             player.gradecounts[4], player.gradecounts[3],
                             player.gradecounts[2], player.gradecounts[1],
                             player.gradecounts[0], player.bestcombo,
                             player.score, player.infos.maxscore));
        } else {
            io::println("YOU FAILED!");
        }
    }
}

//----------------------------------------------------------------------------------------------
// text display

/// Text-only display. Used for the exclusive mode with BGA disabled.
pub struct TextDisplay {
    /// Ticker used for printing to the console.
    ticker: Ticker
}

/// Creates a new text-only display.
pub fn TextDisplay() -> TextDisplay {
    TextDisplay { ticker: Ticker() }
}

impl Display for TextDisplay {
    fn render(&mut self, player: &Player) {
        if !player.opts.showinfo { return; }

        do self.ticker.on_tick(player.now) {
            let elapsed = (player.now - player.origintime) / 100;
            let duration = (player.duration / 100.0) as uint;
            update_line(fmt!("%02u:%02u.%u / %02u:%02u.%u (@%9.4f) | BPM %6.2f | %u / %d notes",
                             elapsed/600, elapsed/10%60, elapsed%10,
                             duration/600, duration/10%60, duration%10,
                             player.bottom, *player.bpm,
                             player.lastcombo, player.infos.nnotes));
        }
    }

    fn show_result(&self, _player: &Player) {
        update_line("");
    }
}

//----------------------------------------------------------------------------------------------
// BGA-only display

/// BGA-only display. Used for the exclusive mode with BGA enabled.
pub struct BGAOnlyDisplay {
    /// The underlying text-only display (as the BGA-only display lacks the on-screen display).
    textdisplay: TextDisplay,
    /// Display screen. (C: `screen`)
    screen: ~Surface,
    /// Image resources. (C: `imgres`)
    imgres: ~[ImageResource],
    /// Currently known state of BGAs.
    lastbga: BGAState,
}

/// Creates a new BGA-only display from the pre-created screen (usually by `init_video`) and
/// pre-loaded image resources.
pub fn BGAOnlyDisplay(screen: ~Surface, imgres: ~[ImageResource]) -> BGAOnlyDisplay {
    BGAOnlyDisplay { textdisplay: TextDisplay(), screen: screen,
                     imgres: imgres, lastbga: initial_bga_state() }
}

impl Display for BGAOnlyDisplay {
    fn render(&mut self, player: &Player) {
        self.lastbga.update(&player.bga, self.imgres);

        let layers = &[Layer1, Layer2, Layer3];
        self.lastbga.render(self.screen, layers, self.imgres, 0, 0);
        self.screen.flip();

        self.textdisplay.render(player);
    }

    fn show_result(&self, player: &Player) {
        self.textdisplay.show_result(player);
    }
}

//----------------------------------------------------------------------------------------------

