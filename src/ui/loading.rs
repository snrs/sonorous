// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Loading screen. Displays the STAGEFILE image and metadata while loading resources.

use sdl::*;
use format::timeline::TimelineInfo;
use format::bms::{Bms, Key};
use util::gl::Texture;
use util::gfx::*;
use util::bmfont::{Font, LeftAligned, Centered, RightAligned};
use engine::keyspec::KeySpec;
use engine::resource::{SoundResource, NoSound, ImageResource, NoImage, Image};
use engine::resource::{get_basedir, load_sound, load_image, apply_blitcmd};
use ui::common::{Ticker, check_exit, update_line};
use ui::screen::Screen;
use ui::options::Options;
use ui::init::{SCREENW, SCREENH};

/// Loading scene context.
pub struct LoadingScene {
    /// Same as `Options::showinfo`.
    showinfo: bool,
    /// Metadata string (level, BPM, number of notes etc).
    brief: ~str,
    /// Title string.
    title: ~str,
    /// Genre string.
    genre: ~str,
    /// Artist string.
    artist: ~str,
    /// A path to #STAGEFILE image. Since the resource engine performs its own path resolution
    /// we just keep basedir and let the engine resolve it.
    stagefile_path: Option<(Path,~str)>, // basedir, path
    /// A loaded OpenGL texture for #STAGEFILE image.
    stagefile_tex: Option<Texture>,
}

/// Returns the interface string common to the graphical and textual loading screen.
fn displayed_info(bms: &Bms, infos: &TimelineInfo, keyspec: &KeySpec) -> (~str, ~str, ~str, ~str) {
    let brief = fmt!("Level %d | BPM %.2f%s | %d note%s [%uKEY%s]",
                     bms.meta.playlevel, *bms.timeline.initbpm,
                     if infos.hasbpmchange {~"?"} else {~""}, infos.nnotes,
                     if infos.nnotes == 1 {~""} else {~"s"}, keyspec.nkeys(),
                     if infos.haslongnote {~"-LN"} else {~""});
    let title = bms.meta.title.clone().get_or_default(~"");
    let genre = bms.meta.genre.clone().get_or_default(~"");
    let artist = bms.meta.artist.clone().get_or_default(~"");
    (brief, title, genre, artist)
}

impl LoadingScene {
    /// Creates a state required for rendering the graphical loading screen.
    pub fn new(bms: &Bms, infos: &TimelineInfo, keyspec: &KeySpec, opts: &Options) -> LoadingScene {
        let (brief, title, genre, artist) = displayed_info(bms, infos, keyspec);
        let stagefile_path = do bms.meta.stagefile.map |&path| { (get_basedir(bms), path) };
        LoadingScene { showinfo: opts.showinfo, brief: brief, title: title, genre: genre,
                       artist: artist, stagefile_path: stagefile_path, stagefile_tex: None }
    }

    /// Loads the BMS #STAGEFILE image and creates an OpenGL texture for it.
    pub fn load_stagefile(&mut self) {
        let (basedir, path) = match self.stagefile_path {
            Some((ref basedir, ref path)) => (basedir, path),
            None => { return; }
        };

        let tex_or_err = do load_image(*path, basedir, false).chain |res| {
            match res {
                Image(surface) => {
                    // in principle we don't need this, but some STAGEFILEs mistakenly uses alpha
                    // channel or SDL_image fails to read them so we need to force STAGEFILEs to
                    // ignore alpha channels. (cf. http://bugzilla.libsdl.org/show_bug.cgi?id=1943)
                    do surface.display_format().chain |surface| {
                        Texture::from_owned_surface(surface, false, false)
                    }
                },
                _ => Err(~"unexpected")
            }
        };
        match tex_or_err {
            Ok(tex) => { self.stagefile_tex = Some(tex); }
            Err(_err) => { warn!("failed to load image #STAGEFILE (%s)", path.to_str()); }
        }
    }

    /// Renders the graphical loading screen by blitting BMS #STAGEFILE image (if any) and showing
    /// the metadata.
    pub fn render(&self, screen: &Screen, font: &Font, msg: Option<~str>) {
        let msg = msg.get_or_default(~"loading...");

        screen.clear();

        let W = SCREENW as f32;
        let H = SCREENH as f32;

        do screen.draw_shaded() |d| {
            font.draw_string(d, W/2.0, H/2.0-16.0, 2.0, Centered, "loading bms file...",
                             Gradient(RGB(0x80,0x80,0x80), RGB(0x20,0x20,0x20)));
        }

        if self.stagefile_tex.is_some() {
            let tex = self.stagefile_tex.get_ref();
            do screen.draw_textured(tex) |d| {
                d.rect(0.0, 0.0, W, H);
            }
        }

        if self.showinfo {
            let bg = RGBA(0x10,0x10,0x10,0xc0);
            let fg = Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80));
            do screen.draw_shaded() |d| {
                d.rect(0.0, 0.0, W, 42.0, bg);
                d.rect(0.0, H-20.0, W, H, bg);
                font.draw_string(d, 6.0, 4.0, 2.0, LeftAligned, self.title, fg);
                font.draw_string(d, W-8.0, 4.0, 1.0, RightAligned, self.genre, fg);
                font.draw_string(d, W-8.0, 20.0, 1.0, RightAligned, self.artist, fg);
                font.draw_string(d, 3.0, H-18.0, 1.0, LeftAligned, self.brief, fg);
                font.draw_string(d, W-3.0, H-18.0, 1.0, RightAligned, msg,
                                 Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x80,0x80,0x80)));
            }
        }

        screen.swap_buffers();
    }
}

/// Renders the textual loading screen by printing the metadata.
pub fn show_stagefile_noscreen(bms: &Bms, infos: &TimelineInfo, keyspec: &KeySpec, opts: &Options) {
    if opts.showinfo {
        let (brief, title, genre, artist) = displayed_info(bms, infos, keyspec);
        ::std::io::stderr().write_line(fmt!("\
----------------------------------------------------------------------------------------------
Title:    %s\nGenre:    %s\nArtist:   %s\n%s
----------------------------------------------------------------------------------------------",
            title, genre, artist, brief));
    }
}

/// Loads the image and sound resources and calls a callback whenever a new resource has been
/// loaded.
pub fn load_resource(bms: &Bms, opts: &Options,
                     callback: &fn(Option<~str>)) -> (~[SoundResource], ~[ImageResource]) {
    let basedir = get_basedir(bms);

    let sndres: ~[SoundResource] =
        do bms.meta.sndpath.iter().enumerate().transform |(i, &path)| {
            match path {
                Some(path) => {
                    callback(Some(path.clone()));
                    match load_sound(path, &basedir) {
                        Ok(res) => res,
                        Err(_) => {
                            warn!("failed to load sound #WAV%s (%s)", Key(i as int).to_str(), path);
                            NoSound
                        }
                    }
                },
                None => NoSound
            }
        }.collect();
    let mut imgres: ~[ImageResource] =
        do bms.meta.imgpath.iter().enumerate().transform |(i, &path)| {
            let path = if opts.has_bga() {path} else {None}; // skip loading BGAs if requested
            match path {
                Some(path) => {
                    callback(Some(path.clone()));
                    match load_image(path, &basedir, opts.has_movie()) {
                        Ok(res) => res,
                        Err(_) => {
                            warn!("failed to load image #BMP%s (%s)", Key(i as int).to_str(), path);
                            NoImage
                        }
                    }
                },
                None => NoImage
            }
        }.collect();

    for bms.meta.blitcmd.iter().advance |bc| {
        apply_blitcmd(imgres, bc);
    }
    (sndres, imgres)
}

/// A callback template for `load_resource` with the graphical loading screen.
pub fn graphic_update_status(path: Option<~str>, screen: &Screen, scene: &LoadingScene,
                             font: &Font, ticker: &mut Ticker, atexit: &fn()) {
    // Rust: `on_tick` calls the closure at most once so `path` won't be referenced twice,
    //       but the analysis can't reason that. (#4654) an "option dance" via
    //       `Option<T>::swap_unwrap` is not helpful here since `path` can be `None`.
    let mut path = path; // XXX #4654
    do ticker.on_tick(get_ticks()) {
        let path = ::std::util::replace(&mut path, None); // XXX #4654
        scene.render(screen, font, path);
    }
    check_exit(atexit);
}

/// A callback template for `load_resource` with the textual loading screen.
pub fn text_update_status(path: Option<~str>, ticker: &mut Ticker, atexit: &fn()) {
    let mut path = path; // XXX #4654
    do ticker.on_tick(get_ticks()) {
        match ::std::util::replace(&mut path, None) { // XXX #4654
            Some(path) => {
                let path = if path.len() < 63 {path} else {path.slice(0, 63).to_owned()};
                update_line(~"Loading: " + path);
            }
            None => { update_line("Loading done."); }
        }
    }
    check_exit(atexit);
}

