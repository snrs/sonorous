// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Loading screen. Displays the STAGEFILE image and metadata while loading resources.

use std::vec;
use extra::container::Deque;
use extra::dlist::DList;

use sdl::*;
use format::timeline::TimelineInfo;
use format::bms::{Bms, Key};
use util::gl::{Texture, ShadedDrawingTraits, TexturedDrawingTraits};
use util::gfx::*;
use util::bmfont::{LeftAligned, Centered, RightAligned};
use util::filesearch::SearchContext;
use engine::input::KeyMap;
use engine::keyspec::KeySpec;
use engine::resource::{SoundResource, NoSound, ImageResource, NoImage, Image};
use engine::resource::{get_basedir, load_sound, load_image, apply_blitcmd};
use engine::player::Player;
use ui::common::{update_line};
use ui::screen::Screen;
use ui::options::Options;
use ui::init::{SCREENW, SCREENH};
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PopScene, ReplaceScene, Exit};
use ui::playing::PlayingScene;
use ui::viewing::{ViewingScene, TextualViewingScene};

/// Jobs to be executed.
pub enum LoadingJob {
    LoadStageFile,
    LoadSound(uint),
    LoadImage(uint),
}

/// Scene-independent loading context.
pub struct LoadingContext {
    /// Game play options.
    opts: ~Options,
    /// Yet-to-be-loaded BMS data.
    bms: Bms,
    /// The derived timeline information.
    infos: TimelineInfo,
    /// The key specification.
    keyspec: ~KeySpec,
    /// The input mapping.
    keymap: ~KeyMap,

    /// The latest message from the resource loader.
    message: Option<~str>,
    /// Context for searching files.
    search: SearchContext,
    /// A list of jobs to be executed.
    jobs: DList<LoadingJob>,
    /// A number of total jobs queued.
    ntotaljobs: uint,

    /// The resource directory associated to the BMS data.
    basedir: Path,
    /// A loaded OpenGL texture for #STAGEFILE image.
    stagefile: Option<Texture>,
    /// A list of loaded sound resources. Initially populated with `NoSound`.
    sndres: ~[SoundResource],
    /// A list of loaded image resources. Initially populated with `NoImage`.
    imgres: ~[ImageResource],

    /// Metadata string (level, BPM, number of notes etc).
    brief: ~str,
    /// Title string.
    title: ~str,
    /// Genre string.
    genre: ~str,
    /// Artist string.
    artist: ~str,
}

/// Returns the interface string common to the graphical and textual loading screen.
fn displayed_info(bms: &Bms, infos: &TimelineInfo, keyspec: &KeySpec) -> (~str, ~str, ~str, ~str) {
    let brief = format!("Level {level} | BPM {bpm:.2}{hasbpmchange} | \
                         {nnotes, plural, =1{# note} other{# notes}} [{nkeys}KEY{haslongnote}]",
                        level = bms.meta.playlevel, bpm = *bms.timeline.initbpm,
                        hasbpmchange = if infos.hasbpmchange {"?"} else {""},
                        nnotes = infos.nnotes, nkeys = keyspec.nkeys(),
                        haslongnote = if infos.haslongnote {"-LN"} else {""});
    let title = bms.meta.title.clone().unwrap_or(~"");
    let genre = bms.meta.genre.clone().unwrap_or(~"");
    let artist = bms.meta.artist.clone().unwrap_or(~"");
    (brief, title, genre, artist)
}

impl LoadingContext {
    /// Creates a loading context.
    pub fn new(bms: Bms, infos: TimelineInfo, keyspec: ~KeySpec, keymap: ~KeyMap,
               opts: ~Options) -> LoadingContext {
        let basedir = get_basedir(&bms);
        let sndres = vec::from_elem(bms.meta.sndpath.len(), NoSound);
        let imgres = vec::from_elem(bms.meta.imgpath.len(), NoImage);
        let (brief, title, genre, artist) = displayed_info(&bms, &infos, keyspec);

        let mut jobs = DList::new();
        if opts.has_bga() { // should go first
            jobs.push_back(LoadStageFile);
        }
        for (i, path) in bms.meta.sndpath.iter().enumerate() {
            if path.is_some() { jobs.push_back(LoadSound(i)); }
        }
        if opts.has_bga() {
            for (i, path) in bms.meta.imgpath.iter().enumerate() {
                if path.is_some() { jobs.push_back(LoadImage(i)); }
            }
        }
        let njobs = jobs.len();

        LoadingContext {
            opts: opts, bms: bms, infos: infos, keyspec: keyspec, keymap: keymap,
            message: None, search: SearchContext::new(), jobs: jobs, ntotaljobs: njobs,
            basedir: basedir, stagefile: None, sndres: sndres, imgres: imgres,
            brief: brief, title: title, genre: genre, artist: artist,
        }
    }

    /// Loads the BMS #STAGEFILE image and creates an OpenGL texture for it.
    pub fn load_stagefile(&mut self) {
        let path = match self.bms.meta.stagefile {
            Some(ref path) => path,
            None => { return; }
        };

        let res = load_image(&mut self.search, *path, &self.basedir, false);
        let tex_or_err = do res.and_then |res| {
            match res {
                Image(surface) => {
                    // in principle we don't need this, but some STAGEFILEs mistakenly uses alpha
                    // channel or SDL_image fails to read them so we need to force STAGEFILEs to
                    // ignore alpha channels. (cf. http://bugzilla.libsdl.org/show_bug.cgi?id=1943)
                    do surface.display_format().and_then |surface| {
                        Texture::from_owned_surface(surface, false, false)
                    }
                },
                _ => Err(~"unexpected")
            }
        };
        match tex_or_err {
            Ok(tex) => { self.stagefile = Some(tex); }
            Err(_err) => { warn!("failed to load image \\#STAGEFILE ({})", path.to_str()); }
        }
    }

    /// Loads the sound and creates a `Chunk` for it.
    pub fn load_sound(&mut self, i: uint) {
        let path = self.bms.meta.sndpath[i].get_ref().clone();
        self.message = Some(path.clone());

        match load_sound(&mut self.search, path, &self.basedir) {
            Ok(res) => { self.sndres[i] = res; }
            Err(_) => { warn!("failed to load sound \\#WAV{} ({})", Key(i as int).to_str(), path); }
        }
    }

    /// Loads the image or movie and creates an OpenGL texture for it.
    pub fn load_image(&mut self, i: uint) {
        let path = self.bms.meta.imgpath[i].get_ref().clone();
        self.message = Some(path.clone());

        match load_image(&mut self.search, path, &self.basedir, self.opts.has_movie()) {
            Ok(res) => { self.imgres[i] = res; }
            Err(_) => { warn!("failed to load image \\#BMP{} ({})", Key(i as int).to_str(), path); }
        }
    }

    /// Processes pending jobs. Returns true if there are any processed jobs.
    pub fn process_jobs(&mut self) -> bool {
        match self.jobs.pop_front() {
            Some(LoadStageFile) => { self.load_stagefile(); true  }
            Some(LoadSound(i))  => { self.load_sound(i);    true  }
            Some(LoadImage(i))  => { self.load_image(i);    true  }
            None                => { self.message = None;   false }
        }
    }

    /// Returns a ratio of processed jobs over all jobs.
    pub fn processed_jobs_ratio(&self) -> f64 {
        1.0 - (self.jobs.len() as f64) / (self.ntotaljobs as f64)
    }

    /// Returns completely loaded `Player` and `ImageResource`s.
    pub fn to_player(self) -> (Player,~[ImageResource]) {
        let LoadingContext {
            opts: opts, bms: bms, infos: infos, keyspec: keyspec, keymap: keymap,
            message: _, search: _, jobs: jobs, ntotaljobs: _,
            basedir: _, stagefile: _, sndres: sndres, imgres: imgres,
            brief: _, title: _, genre: _, artist: _
        } = self;
        assert!(jobs.is_empty());

        let mut imgres = imgres;
        for bc in bms.meta.blitcmd.iter() {
            apply_blitcmd(imgres, bc);
        }

        let duration = bms.timeline.duration(infos.originoffset,
                                             |sref| sndres[**sref].duration());
        let player = Player::new(opts, bms, infos, duration, keyspec, keymap, sndres);
        (player, imgres)
    }
}

/// Graphical loading scene context.
pub struct LoadingScene {
    /// Loading context.
    context: LoadingContext,
    /// Display screen.
    screen: Screen,
    /// The minimum number of ticks (as per `sdl::get_ticks()`) until the scene proceeds.
    /// It is currently 3 seconds after the beginning of the scene.
    waituntil: uint,
}

impl LoadingScene {
    /// Creates a scene context required for rendering the graphical loading screen.
    pub fn new(screen: Screen, bms: Bms, infos: TimelineInfo,
               keyspec: ~KeySpec, keymap: ~KeyMap, opts: ~Options) -> ~LoadingScene {
        ~LoadingScene { context: LoadingContext::new(bms, infos, keyspec, keymap, opts),
                        screen: screen, waituntil: 0 }
    }
}

impl Scene for LoadingScene {
    fn activate(&mut self) -> SceneCommand {
        self.waituntil = get_ticks() + 3000;
        Continue
    }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new().fpslimit(20) }

    fn tick(&mut self) -> SceneCommand {
        loop {
            match event::poll_event() {
                event::KeyEvent(event::EscapeKey,_,_,_) => { return PopScene; }
                event::QuitEvent => { return Exit; }
                event::NoEvent => { break; },
                _ => {}
            }
        }

        if !self.context.process_jobs() {
            if get_ticks() > self.waituntil { return ReplaceScene; }
        }
        Continue
    }

    fn render(&self) {
        let msg = match self.context.message {
            None => ~"loading...",
            Some(ref msg) =>
                format!("{} ({:.0}%)", *msg, 100.0 * self.context.processed_jobs_ratio()),
        };

        self.screen.clear();

        let W = SCREENW as f32;
        let H = SCREENH as f32;

        do self.screen.draw_shaded_with_font |d| {
            d.string(W/2.0, H/2.0-16.0, 2.0, Centered, "loading bms file...",
                     Gradient(RGB(0x80,0x80,0x80), RGB(0x20,0x20,0x20)));
        }

        if self.context.stagefile.is_some() {
            let tex = self.context.stagefile.get_ref();
            do self.screen.draw_textured(tex) |d| {
                d.rect(0.0, 0.0, W, H);
            }
        }

        if self.context.opts.showinfo {
            let bg = RGBA(0x10,0x10,0x10,0xc0);
            let fg = Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80));
            do self.screen.draw_shaded_with_font |d| {
                d.rect(0.0, 0.0, W, 42.0, bg);
                d.rect(0.0, H-20.0, W, H, bg);
                d.string(6.0, 4.0, 2.0, LeftAligned, self.context.title, fg);
                d.string(W-8.0, 4.0, 1.0, RightAligned, self.context.genre, fg);
                d.string(W-8.0, 20.0, 1.0, RightAligned, self.context.artist, fg);
                d.string(3.0, H-18.0, 1.0, LeftAligned, self.context.brief, fg);
                d.string(W-3.0, H-18.0, 1.0, RightAligned, msg,
                         Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x80,0x80,0x80)));
            }
        }

        self.screen.swap_buffers();
    }

    fn deactivate(&mut self) {}

    fn consume(~self) -> ~Scene: {
        // Rust: do not destruct owned box in 0.7, it is seriously broken.
        let LoadingScene { context: context, screen: screen, waituntil: _ } = *self;
        let (player, imgres) = context.to_player();
        match PlayingScene::new(player, screen, imgres) {
            Ok(scene) => scene as ~Scene:,
            Err(err) => die!("{}", err),
        }
    }
}

/// Textual loading scene context.
pub struct TextualLoadingScene {
    /// Loading context.
    context: LoadingContext,
    /// Display screen. This is not used by this scene but sent to the viewing scene.
    screen: Option<Screen>,
}

impl TextualLoadingScene {
    /// Creates a scene context required for rendering the textual loading screen.
    pub fn new(screen: Option<Screen>, bms: Bms, infos: TimelineInfo,
               keyspec: ~KeySpec, keymap: ~KeyMap, opts: ~Options) -> ~TextualLoadingScene {
        ~TextualLoadingScene { context: LoadingContext::new(bms, infos, keyspec, keymap, opts),
                               screen: screen }
    }
}

impl Scene for TextualLoadingScene {
    fn activate(&mut self) -> SceneCommand {
        if self.context.opts.showinfo {
            ::std::io::stderr().write_line(format!("\
----------------------------------------------------------------------------------------------
Title:    {title}
Genre:    {genre}
Artist:   {artist}
{brief}
----------------------------------------------------------------------------------------------",
                title = self.context.title, genre = self.context.genre,
                artist = self.context.artist, brief = self.context.brief));
        }
        Continue
    }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new().fpslimit(20) }

    fn tick(&mut self) -> SceneCommand {
        if !self.context.process_jobs() {ReplaceScene} else {Continue}
    }

    fn render(&self) {
        if self.context.opts.showinfo {
            match self.context.message {
                Some(ref msg) => {
                    use util::std::str::StrUtil;
                    let msg: &str = *msg;
                    let msg = if msg.len() < 63 {msg} else {msg.slice_upto(0, 63)};
                    update_line(format!("Loading: {} ({:.0}%)", msg,
                                        100.0 * self.context.processed_jobs_ratio()));
                }
                None => { update_line("Loading done."); }
            };
        }
    }

    fn deactivate(&mut self) {
        if self.context.opts.showinfo {
            update_line("");
        }
    }

    fn consume(~self) -> ~Scene: {
        // Rust: do not destruct owned box in 0.7, it is seriously broken.
        let TextualLoadingScene { context: context, screen: screen } = *self;
        let (player, imgres) = context.to_player();
        match screen {
            Some(screen) => ViewingScene::new(screen, imgres, player) as ~Scene:,
            None => TextualViewingScene::new(player) as ~Scene:,
        }
    }
}

