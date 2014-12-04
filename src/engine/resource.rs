// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Resource management.

use std::str;

use sdl::video::{Surface, RGB, SurfaceFlag};
use sdl_image;
use sdl_mixer::Chunk;
use ext::smpeg::MPEG;
use util::filesearch::SearchContext;
use gfx::gl::PreparedSurface;

/// The width of BGA, or the width of screen for the exclusive mode.
pub const BGAW: uint = 256;
/// The height of BGA, or the height of screen for the exclusive mode.
pub const BGAH: uint = 256;

/// An internal sampling rate for SDL_mixer. Every chunk loaded is first converted to
/// this sampling rate for the purpose of mixing.
pub const SAMPLERATE: i32 = 44100;
/// The number of bytes in the chunk converted to an internal sampling rate.
pub const BYTESPERSEC: i32 = SAMPLERATE * 2 * 2; // stereo, 16 bits/sample

/// Alternative file extensions for sound resources.
static SOUND_EXTS: &'static [&'static str] = &[".WAV", ".OGG", ".MP3"];
/// Alternative file extensions for image resources.
static IMAGE_EXTS: &'static [&'static str] = &[".BMP", ".PNG", ".JPG", ".JPEG", ".GIF"];

/// A wrapper for `SearchContext::resolve_relative_path` which returns `Result`.
fn resolve_relative_path_result(search: &mut SearchContext, basedir: &Path, path: &str,
                                exts: &[&str]) -> Result<Path,String> {
    match search.resolve_relative_path(basedir, path, exts) {
        Some(path) => Ok(path),
        None => Err(format!("file not found")),
    }
}

pub trait SearchContextAdditions {
    fn resolve_relative_path_for_sound(&mut self, path: &str,
                                       basedir: &Path) -> Result<Path,String>;
    fn resolve_relative_path_for_image(&mut self, path: &str,
                                       basedir: &Path) -> Result<Path,String>;
}

impl SearchContextAdditions for SearchContext {
    /// Resolves the relative path for the use by `LoadedSoundlike::new`.
    fn resolve_relative_path_for_sound(&mut self, path: &str,
                                       basedir: &Path) -> Result<Path,String> {
        resolve_relative_path_result(self, basedir, path, SOUND_EXTS)
    }

    /// Resolves the relative path for the use by `LoadedImagelike::new`.
    fn resolve_relative_path_for_image(&mut self, path: &str,
                                       basedir: &Path) -> Result<Path,String> {
        use std::ascii::AsciiExt;
        // preserve extensions for the movie files
        if path.to_ascii_lower()[].ends_with(".mpg") {
            resolve_relative_path_result(self, basedir, path, [][])
        } else {
            resolve_relative_path_result(self, basedir, path, IMAGE_EXTS)
        }
    }
}

/// Sound resource associated to `SoundRef`. It contains the actual SDL_mixer chunk that can be
/// readily played.
pub enum Soundlike {
    /// No sound resource is associated, or error occurred while loading.
    None,
    /// Sound resource is associated.
    Sound(Chunk)
}

impl Soundlike {
    /// Returns the associated chunk if any.
    pub fn chunk<'r>(&'r self) -> Option<&'r Chunk> {
        match *self {
            Soundlike::None => None,
            Soundlike::Sound(ref chunk) => Some(chunk)
        }
    }

    /// Returns the associated chunk if any.
    pub fn chunk_mut<'r>(&'r mut self) -> Option<&'r mut Chunk> {
        match *self {
            Soundlike::None => None,
            Soundlike::Sound(ref mut chunk) => Some(chunk)
        }
    }

    /// Returns the length of associated sound chunk in seconds. This is used for determining
    /// the actual duration of the song in presence of key and background sounds, so it may
    /// return 0.0 if no sound is present.
    pub fn duration(&self) -> f64 {
        match *self {
            Soundlike::None => 0.0,
            Soundlike::Sound(ref chunk) => {
                let chunk = chunk.to_ll_chunk();
                (unsafe {(*chunk).alen} as f64) / (BYTESPERSEC as f64)
            }
        }
    }
}

/// Same as `Soundlike` but no managed pointer. This version of sound resource can be
/// transferred across tasks and thus used for the worker model.
//
// Rust: the very existence of this enum and `LoadedImagelike` is due to the problem in
//       cross-task owned pointer containing a managed pointer. (#8983)
pub enum LoadedSoundlike {
    None,
    Sound(Chunk)
}

impl LoadedSoundlike {
    /// Loads a sound resource.
    pub fn new(path: &Path) -> Result<LoadedSoundlike,String> {
        let res = try!(Chunk::from_wav(path));
        Ok(LoadedSoundlike::Sound(res))
    }

    /// Creates a `Soundlike` instance. There is no turning back.
    pub fn wrap(self) -> Soundlike {
        match self {
            LoadedSoundlike::None => Soundlike::None,
            LoadedSoundlike::Sound(chunk) => Soundlike::Sound(chunk)
        }
    }
}

/// Image resource associated to `ImageRef`. It can be either a static image or a movie, and
/// both contains an SDL surface that can be blitted to the screen.
pub enum Imagelike {
    /// No image resource is associated, or error occurred while loading.
    None,
    /// A static image is associated. The surface may have a transparency which is already
    /// handled by `LoadedImagelike::new`.
    Image(PreparedSurface),
    /// A movie is associated. A playback starts when `start_movie` method is called, and stops
    /// when `stop_animating` is called. An associated surface is updated from the separate thread
    /// during the playback.
    Movie(PreparedSurface, MPEG)
}

impl Imagelike {
    /// Returns an associated surface if any.
    pub fn surface<'r>(&'r self) -> Option<&'r PreparedSurface> {
        match *self {
            Imagelike::None => None,
            Imagelike::Image(ref surface) | Imagelike::Movie(ref surface,_) => Some(surface)
        }
    }

    /// Stops the animation/movie playback if possible.
    pub fn stop_animating(&self) {
        match *self {
            Imagelike::None | Imagelike::Image(_) => {}
            Imagelike::Movie(_,ref mpeg) => { mpeg.stop(); }
        }
    }

    /// Starts (or restarts, if the movie was already being played) the animation/movie playback
    /// if possible.
    pub fn start_animating(&self) {
        match *self {
            Imagelike::None | Imagelike::Image(_) => {}
            Imagelike::Movie(_,ref mpeg) => { mpeg.rewind(); mpeg.play(); }
        }
    }
}

/// Same as `Imagelike` but no managed pointer. This version of image resource can be
/// transferred across tasks and thus used for the worker model.
pub enum LoadedImagelike {
    None,
    Image(PreparedSurface),
    Movie(PreparedSurface, MPEG)
}

impl LoadedImagelike {
    /// Loads an image resource.
    pub fn new(path: &Path, load_movie: bool) -> Result<LoadedImagelike,String> {
        use std::ascii::AsciiExt;

        /// Converts a surface to the native display format, while preserving a transparency or
        /// setting a color key if required.
        fn to_display_format(surface: Surface) -> Result<PreparedSurface,String> {
            let surface = if unsafe {(*(*surface.raw).format).Amask} != 0 {
                let surface = try!(surface.display_format_alpha());
                surface.set_alpha([SurfaceFlag::SrcAlpha, SurfaceFlag::RLEAccel][], 255);
                surface
            } else {
                let surface = try!(surface.display_format());
                surface.set_color_key([SurfaceFlag::SrcColorKey, SurfaceFlag::RLEAccel][],
                                      RGB(0, 0, 0));
                surface
            };
            match PreparedSurface::from_owned_surface(surface) {
                Ok(prepared) => Ok(prepared),
                Err((_surface,err)) => Err(err)
            }
        }

        let ext = path.extension().and_then(str::from_utf8);
        if ext.unwrap_or_default().eq_ignore_ascii_case("mpg") {
            if load_movie {
                let movie = try!(MPEG::from_path(path));
                let surface = try!(PreparedSurface::new(BGAW, BGAH, false));
                movie.enable_video(true);
                movie.set_loop(true);
                movie.set_display(surface.as_surface());
                Ok(LoadedImagelike::Movie(surface, movie))
            } else {
                Ok(LoadedImagelike::None)
            }
        } else {
            let surface = try!(sdl_image::load(path));
            let prepared = try!(to_display_format(surface));

            // PreparedSurface may destroy SDL_SRCALPHA, which is still required for alpha blitting.
            // for RGB images, it is effectively no-op as per-surface alpha is fully opaque.
            prepared.as_surface().set_alpha([SurfaceFlag::SrcAlpha, SurfaceFlag::RLEAccel][], 255);
            Ok(LoadedImagelike::Image(prepared))
        }
    }

    /// Creates an `Imagelike` instance. Again, there is no turning back.
    pub fn wrap(self) -> Imagelike {
        match self {
            LoadedImagelike::None => Imagelike::None,
            LoadedImagelike::Image(surface) => Imagelike::Image(surface),
            LoadedImagelike::Movie(surface, mpeg) => Imagelike::Movie(surface, mpeg)
        }
    }
}

