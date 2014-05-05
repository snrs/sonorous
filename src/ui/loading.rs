// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Loading screen. Displays the STAGEFILE image and metadata while loading resources.

use std::rc::Rc;
use std::cell::RefCell;
use collections::{Deque, DList};

use sdl::{event, get_ticks};
use format::timeline::TimelineInfo;
use format::bms::{Bms, Key};
use util::filesearch::SearchContext;
use util::console::{printerr, printerrln};
use gfx::gl::Texture2D;
use gfx::screen::Screen;
use gfx::skin::scalar::{Scalar, AsScalar, IntoScalar};
use gfx::skin::hook::Hook;
use gfx::skin::render::Renderer;
use engine::input::KeyMap;
use engine::keyspec::KeySpec;
use engine::resource::{SoundResource, LoadedSoundResource, NoSound};
use engine::resource::{ImageResource, LoadedImageResource, NoImage, LoadedImage};
use engine::resource::{SearchContextAdditions};
use engine::player::Player;
use ui::common::{update_line};
use ui::options::Options;
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
    pub opts: Rc<Options>,
    /// Yet-to-be-loaded BMS data.
    pub bms: Bms,
    /// The derived timeline information.
    pub infos: TimelineInfo,
    /// The key specification.
    pub keyspec: ~KeySpec,
    /// The input mapping.
    pub keymap: ~KeyMap,

    /// The most recently loaded file name from the resource loader.
    pub lastpath: Option<~str>,
    /// Context for searching files.
    pub search: SearchContext,
    /// A list of jobs to be executed.
    pub jobs: DList<LoadingJob>,
    /// A number of total jobs queued.
    pub ntotaljobs: uint,

    /// The resource directory associated to the BMS data.
    pub basedir: Path,
    /// A loaded OpenGL texture for #STAGEFILE image.
    pub stagefile: Option<Rc<Texture2D>>,
    /// A list of loaded sound resources. Initially populated with `NoSound`.
    pub sndres: Vec<SoundResource>,
    /// A list of loaded image resources. Initially populated with `NoImage`.
    pub imgres: Vec<ImageResource>,
}

impl LoadingContext {
    /// Creates a loading context.
    pub fn new(bms: Bms, infos: TimelineInfo, keyspec: ~KeySpec, keymap: ~KeyMap,
               opts: Rc<Options>) -> LoadingContext {
        let basedir = bms.meta.basepath.clone().unwrap_or(Path::new("."));
        let sndres = Vec::from_fn(bms.meta.sndpath.len(), |_| NoSound);
        let imgres = Vec::from_fn(bms.meta.imgpath.len(), |_| NoImage);

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
            lastpath: None, search: SearchContext::new(), jobs: jobs, ntotaljobs: njobs,
            basedir: basedir, stagefile: None, sndres: sndres, imgres: imgres,
        }
    }

    /// Loads the BMS #STAGEFILE image and creates an OpenGL texture for it.
    pub fn load_stagefile(&mut self) {
        let path = match self.bms.meta.stagefile {
            Some(ref path) => path,
            None => { return; }
        };
        let fullpath = self.search.resolve_relative_path_for_image(*path, &self.basedir);

        let res = fullpath.and_then(|path| LoadedImageResource::new(&path, false));
        let tex_or_err = res.and_then(|res| {
            match res {
                LoadedImage(surface) => {
                    // in principle we don't need this, but some STAGEFILEs mistakenly uses alpha
                    // channel or SDL_image fails to read them so we need to force STAGEFILEs to
                    // ignore alpha channels. (cf. http://bugzilla.libsdl.org/show_bug.cgi?id=1943)
                    surface.as_surface().display_format().and_then(|surface| {
                        Texture2D::from_owned_surface(surface, false, false)
                    })
                },
                _ => fail!("unexpected")
            }
        });
        match tex_or_err {
            Ok(tex) => { self.stagefile = Some(Rc::new(tex)); }
            Err(_err) => { warn!("failed to load image \\#STAGEFILE ({})", path.to_str()); }
        }
    }

    /// Loads the sound and creates a `Chunk` for it.
    pub fn load_sound(&mut self, i: uint) {
        let path = self.bms.meta.sndpath.as_slice()[i].get_ref().clone();
        self.lastpath = Some(path.clone());
        let fullpath = self.search.resolve_relative_path_for_sound(path, &self.basedir);

        match fullpath.and_then(|path| LoadedSoundResource::new(&path)) {
            Ok(res) => {
                self.sndres.as_mut_slice()[i] = res.wrap();
            }
            Err(_) => {
                warn!("failed to load sound \\#WAV{} ({})", Key(i as int).to_str(), path);
            }
        }
    }

    /// Loads the image or movie and creates an OpenGL texture for it.
    pub fn load_image(&mut self, i: uint) {
        let path = self.bms.meta.imgpath.as_slice()[i].get_ref().clone();
        self.lastpath = Some(path.clone());
        let fullpath = self.search.resolve_relative_path_for_image(path, &self.basedir);

        let has_movie = self.opts.has_movie();
        match fullpath.and_then(|path| LoadedImageResource::new(&path, has_movie)) {
            Ok(res) => {
                self.imgres.as_mut_slice()[i] = res.wrap();
            }
            Err(_) => {
                warn!("failed to load image \\#BMP{} ({})", Key(i as int).to_str(), path);
            }
        }
    }

    /// Processes pending jobs. Returns true if there are any processed jobs.
    pub fn process_jobs(&mut self) -> bool {
        match self.jobs.pop_front() {
            Some(LoadStageFile) => { self.load_stagefile(); true  }
            Some(LoadSound(i))  => { self.load_sound(i);    true  }
            Some(LoadImage(i))  => { self.load_image(i);    true  }
            None                => { self.lastpath = None;  false }
        }
    }

    /// Returns a ratio of processed jobs over all jobs.
    pub fn processed_jobs_ratio(&self) -> f64 {
        1.0 - (self.jobs.len() as f64) / (self.ntotaljobs as f64)
    }

    /// Returns completely loaded `Player` and `ImageResource`s.
    pub fn to_player(self) -> (Player,Vec<ImageResource>) {
        let LoadingContext { opts, bms, infos, keyspec, keymap, jobs, sndres, imgres, .. } = self;
        assert!(jobs.is_empty());
        (Player::new(opts, bms, infos, keyspec, keymap, sndres), imgres)
    }
}

/// Graphical loading scene context.
pub struct LoadingScene {
    /// Loading context.
    pub context: LoadingContext,
    /// Display screen.
    pub screen: Rc<RefCell<Screen>>,
    /// Skin renderer.
    pub skin: RefCell<Renderer>,
    /// The minimum number of ticks (as per `sdl::get_ticks()`) until the scene proceeds.
    /// It is currently 3 seconds after the beginning of the scene.
    pub waituntil: uint,
}

impl LoadingScene {
    /// Creates a scene context required for rendering the graphical loading screen.
    pub fn new(screen: Rc<RefCell<Screen>>, bms: Bms, infos: TimelineInfo,
               keyspec: ~KeySpec, keymap: ~KeyMap, opts: Rc<Options>) -> ~LoadingScene {
        let skin = match opts.load_skin("loading.json") {
            Ok(skin) => skin,
            Err(err) => die!("{}", err),
        };
        ~LoadingScene { context: LoadingContext::new(bms, infos, keyspec, keymap, opts),
                        screen: screen, skin: RefCell::new(Renderer::new(skin)), waituntil: 0 }
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
        let mut screen = self.screen.borrow_mut();

        screen.clear();
        self.skin.borrow_mut().render(screen.deref_mut(), &self.context);
        screen.swap_buffers();
    }

    fn deactivate(&mut self) {}

    fn consume(~self) -> ~Scene: {
        let LoadingScene { context, screen, .. } = *self;
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
    pub context: LoadingContext,
    /// Display screen. This is not used by this scene but sent to the viewing scene.
    pub screen: Option<Rc<RefCell<Screen>>>,
}

impl TextualLoadingScene {
    /// Creates a scene context required for rendering the textual loading screen.
    pub fn new(screen: Option<Rc<RefCell<Screen>>>, bms: Bms, infos: TimelineInfo,
               keyspec: ~KeySpec, keymap: ~KeyMap, opts: Rc<Options>) -> ~TextualLoadingScene {
        ~TextualLoadingScene { context: LoadingContext::new(bms, infos, keyspec, keymap, opts),
                               screen: screen }
    }
}

impl Scene for TextualLoadingScene {
    fn activate(&mut self) -> SceneCommand {
        if self.context.opts.showinfo {
            printerr(format!("\
----------------------------------------------------------------------------------------------
Title:    {title}",
                title = self.context.bms.meta.title.as_ref().map_or("", |s| s.as_slice())));
            for subtitle in self.context.bms.meta.subtitles.iter() {
                printerr(format!("
          {subtitle}", subtitle = *subtitle));
            }
            printerr(format!("
Genre:    {genre}
Artist:   {artist}",
                genre = self.context.bms.meta.genre.as_ref().map_or("", |s| s.as_slice()),
                artist = self.context.bms.meta.artist.as_ref().map_or("", |s| s.as_slice())));
            for subartist in self.context.bms.meta.subartists.iter() {
                printerr(format!("
          {subartist}", subartist = *subartist));
            }
            for comment in self.context.bms.meta.comments.iter() {
                printerr(format!("
\"{comment}\"", comment = *comment));
            }
            printerrln(format!("
Level {level} | BPM {bpm:.2}{hasbpmchange} | \
{nnotes, plural, =1{# note} other{# notes}} [{nkeys}KEY{haslongnote}{difficulty}]
----------------------------------------------------------------------------------------------",
                level = self.context.bms.meta.playlevel,
                bpm = *self.context.bms.timeline.initbpm,
                hasbpmchange = if self.context.infos.hasbpmchange {"?"} else {""},
                nnotes = self.context.infos.nnotes, nkeys = self.context.keyspec.nkeys(),
                haslongnote = if self.context.infos.haslongnote {"-LN"} else {""},
                difficulty = self.context.bms.meta.difficulty.and_then(|d| d.name())
                                                             .map_or("".to_owned(),
                                                                     |name| " " + name)));
        }
        Continue
    }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new().fpslimit(20) }

    fn tick(&mut self) -> SceneCommand {
        if !self.context.process_jobs() {ReplaceScene} else {Continue}
    }

    fn render(&self) {
        if self.context.opts.showinfo {
            match self.context.lastpath {
                Some(ref path) => {
                    use util::std::str::StrUtil;
                    let path = if path.len() < 63 {path.as_slice()} else {path.slice_upto(0, 63)};
                    update_line(format!("Loading: {} ({:.0}%)", path,
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
        let TextualLoadingScene { context, screen } = *self;
        let (player, imgres) = context.to_player();
        match screen {
            Some(screen) => ViewingScene::new(screen, imgres, player) as ~Scene:,
            None => TextualViewingScene::new(player) as ~Scene:,
        }
    }
}

////////////////////////////////////////////////////////////////////////
// hooks

impl Hook for LoadingContext {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "loading.path" => self.lastpath.as_ref().map(|s| s.as_scalar()),
            "loading.ratio" => Some(self.processed_jobs_ratio().into_scalar()),
            "meta.stagefile" => self.stagefile.as_ref().map(|s| s.as_scalar()),
            _ => {
                self.opts.scalar_hook(id)
                    .or_else(|| self.bms.scalar_hook(id))
                    .or_else(|| self.infos.scalar_hook(id))
                    .or_else(|| self.keyspec.scalar_hook(id))
            }
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, mut body: |&Hook, &str| -> bool) -> bool {
        match id {
            "loading" => { self.lastpath.is_some() && body(parent, ""); }
            "meta.stagefile" => { self.stagefile.is_some() && body(parent, ""); }
            _ => {
                return self.opts.run_block_hook(id, parent, &mut body) ||
                       self.bms.run_block_hook(id, parent, &mut body) ||
                       self.infos.run_block_hook(id, parent, &mut body) ||
                       self.keyspec.run_block_hook(id, parent, &mut body);
            }
        }
        true
    }
}

