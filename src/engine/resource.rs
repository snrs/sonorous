// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Resource management.

use std::cmp;
use sdl::{img, video, mixer};
use sdl::video::{Surface, RGB};
use ext::sdl::mpeg;
use format::bms::{Bms, BlitCmd};
use util::gfx::SurfaceAreaUtil;
use util::gl::PreparedSurface;
use util::filesearch::resolve_relative_path;

/// The width of BGA, or the width of screen for the exclusive mode.
pub static BGAW: uint = 256;
/// The height of BGA, or the height of screen for the exclusive mode.
pub static BGAH: uint = 256;

/// An internal sampling rate for SDL_mixer. Every chunk loaded is first converted to
/// this sampling rate for the purpose of mixing.
pub static SAMPLERATE: i32 = 44100;
/// The number of bytes in the chunk converted to an internal sampling rate.
pub static BYTESPERSEC: i32 = SAMPLERATE * 2 * 2; // stereo, 16 bits/sample

/// Alternative file extensions for sound resources.
static SOUND_EXTS: &'static [&'static str] = &[".WAV", ".OGG", ".MP3"];
/// Alternative file extensions for image resources.
static IMAGE_EXTS: &'static [&'static str] = &[".BMP", ".PNG", ".JPG", ".JPEG", ".GIF"];

/// Returns a specified or implied resource directory from the BMS file.
pub fn get_basedir(bms: &Bms) -> Path {
    // TODO this logic assumes that #PATH_WAV is always interpreted as a native path, which
    // the C version doesn't assume. this difference barely makes the practical issue though.
    match bms.meta.basepath {
        Some(ref basepath) => { let basepath: &str = *basepath; Path(basepath) }
        None => Path(bms.bmspath).dir_path()
    }
}

/// A wrapper for `util::filesearch::resolve_relative_path` which returns `Result`.
fn resolve_relative_path_result(basedir: &Path, path: &str, exts: &[&str]) -> Result<Path,~str> {
    match resolve_relative_path(basedir, path, exts) {
        Some(path) => Ok(path),
        None => Err(~"file not found"),
    }
}

/// Sound resource associated to `SoundRef`. It contains the actual SDL_mixer chunk that can be
/// readily played.
#[deriving(Clone)]
pub enum SoundResource {
    /// No sound resource is associated, or error occurred while loading.
    NoSound,
    /// Sound resource is associated.
    //
    // Rust: ideally this should be just a ~-ptr, but the current borrowck is very constrained
    //       in this aspect. after several attempts I finally sticked to delegate the ownership
    //       to a managed box.
    Sound(@~mixer::Chunk) // XXX borrowck
}

impl SoundResource {
    /// Returns the associated chunk if any.
    pub fn chunk(&self) -> Option<@~mixer::Chunk> {
        match *self {
            NoSound => None,
            Sound(chunk) => Some(chunk)
        }
    }

    /// Returns the length of associated sound chunk in seconds. This is used for determining
    /// the actual duration of the song in presence of key and background sounds, so it may
    /// return 0.0 if no sound is present.
    pub fn duration(&self) -> f64 {
        match *self {
            NoSound => 0.0,
            Sound(chunk) => {
                let chunk = chunk.to_ll_chunk();
                (unsafe {(*chunk).alen} as f64) / (BYTESPERSEC as f64)
            }
        }
    }
}

/// Loads a sound resource.
pub fn load_sound(path: &str, basedir: &Path) -> Result<SoundResource,~str> {
    let fullpath = earlyexit!(resolve_relative_path_result(basedir, path, SOUND_EXTS));
    let res = earlyexit!(mixer::Chunk::from_wav(&fullpath));
    Ok(Sound(@res))
}

/// Image resource associated to `ImageRef`. It can be either a static image or a movie, and
/// both contains an SDL surface that can be blitted to the screen.
#[deriving(Clone)]
pub enum ImageResource {
    /// No image resource is associated, or error occurred while loading.
    NoImage,
    /// A static image is associated. The surface may have a transparency which is already
    /// handled by `load_image`.
    Image(@PreparedSurface), // XXX borrowck
    /// A movie is associated. A playback starts when `start_movie` method is called, and stops
    /// when `stop_movie` is called. An associated surface is updated from the separate thread
    /// during the playback.
    Movie(@PreparedSurface, @~mpeg::MPEG) // XXX borrowck
}

impl ImageResource {
    /// Returns an associated surface if any.
    pub fn surface(&self) -> Option<@PreparedSurface> {
        match *self {
            NoImage => None,
            Image(surface) | Movie(surface,_) => Some(surface)
        }
    }

    /// Stops the movie playback if possible.
    pub fn stop_movie(&self) {
        match *self {
            NoImage | Image(_) => {}
            Movie(_,mpeg) => { mpeg.stop(); }
        }
    }

    /// Starts (or restarts, if the movie was already being played) the movie playback
    /// if possible.
    pub fn start_movie(&self) {
        match *self {
            NoImage | Image(_) => {}
            Movie(_,mpeg) => { mpeg.rewind(); mpeg.play(); }
        }
    }
}

/// Loads an image resource.
pub fn load_image(path: &str, basedir: &Path, load_movie: bool) -> Result<ImageResource,~str> {
    use util::std::str::StrUtil;

    /// Converts a surface to the native display format, while preserving a transparency or
    /// setting a color key if required.
    fn to_display_format(surface: ~Surface) -> Result<PreparedSurface,~str> {
        let surface = if unsafe {(*(*surface.raw).format).Amask} != 0 {
            let surface = earlyexit!(surface.display_format_alpha());
            surface.set_alpha([video::SrcAlpha, video::RLEAccel], 255);
            surface
        } else {
            let surface = earlyexit!(surface.display_format());
            surface.set_color_key([video::SrcColorKey, video::RLEAccel], RGB(0,0,0));
            surface
        };
        match PreparedSurface::from_owned_surface(surface) {
            Ok(prepared) => Ok(prepared),
            Err((_surface,err)) => Err(err)
        }
    }

    if path.to_ascii_lower().ends_with(".mpg") {
        if load_movie {
            let fullpath = earlyexit!(resolve_relative_path_result(basedir, path, []));
            let movie = earlyexit!(mpeg::MPEG::from_path(&fullpath));
            let surface = earlyexit!(PreparedSurface::new(BGAW, BGAH, false));
            movie.enable_video(true);
            movie.set_loop(true);
            movie.set_display(*surface);
            Ok(Movie(@surface, @movie))
        } else {
            Ok(NoImage)
        }
    } else {
        let fullpath = earlyexit!(resolve_relative_path_result(basedir, path, IMAGE_EXTS));
        let surface = earlyexit!(img::load(&fullpath));
        let prepared = earlyexit!(to_display_format(surface));
        Ok(Image(@prepared))
    }
}

/// Applies the blit command to given list of image resources.
pub fn apply_blitcmd(imgres: &mut [ImageResource], bc: &BlitCmd) {
    let origin: @PreparedSurface = match imgres[**bc.src] {
        Image(src) => src,
        _ => { return; }
    };
    let target: @PreparedSurface = match imgres[**bc.dst] {
        Image(dst) => dst,
        NoImage => {
            let surface = match PreparedSurface::new(BGAW, BGAH, false) {
                Ok(surface) => @surface,
                Err(err) => fail!(format!("PreparedSurface::new failed: {}", err))
            };
            surface.fill(RGB(0, 0, 0));
            surface.set_color_key([video::SrcColorKey, video::RLEAccel], RGB(0, 0, 0));
            imgres[**bc.dst] = Image(surface);
            surface
        },
        _ => { return; }
    };

    let x1 = cmp::max(bc.x1, 0);
    let y1 = cmp::max(bc.y1, 0);
    let x2 = cmp::min(bc.x2, bc.x1 + BGAW as int);
    let y2 = cmp::min(bc.y2, bc.y1 + BGAH as int);
    target.blit_area(**origin, (x1,y1), (bc.dx,bc.dy), (x2-x1,y2-y1));
}

