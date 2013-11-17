// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Song and pattern selection screen.

use std::{cmp, os, comm, task, util};
use std::rand::{rng, Rng};
use extra::arc::RWArc;

use sdl::{event, get_ticks};
use gl = opengles::gl2;
use format::timeline::TimelineInfo;
use format::bms;
use format::bms::Bms;
use util::filesearch::SearchContext;
use util::envelope::Envelope;
use gfx::color::{Color, RGB};
use gfx::gl::{PreparedSurface, Texture};
use gfx::draw::{ShadedDrawingTraits, TexturedDrawingTraits};
use gfx::bmfont::{LeftAligned, RightAligned};
use engine::input::read_keymap;
use engine::keyspec::{KeySpec, key_spec};
use engine::resource::{SearchContextAdditions, LoadedImageResource, LoadedImage};
use engine::player::apply_modf;
use ui::screen::Screen;
use ui::init::{SCREENW, SCREENH};
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PushScene, PopScene, Exit};
use ui::options::Options;
use ui::loading::LoadingScene;

/// The BMS data that has been preprocessed for modifiers and analyzed but yet to be loaded.
pub struct PreprocessedBms {
    /// Yet-to-be-loaded BMS data.
    bms: Bms,
    /// The derived timeline information.
    infos: TimelineInfo,
    /// The key specification.
    keyspec: ~KeySpec,
}

/// Loads and preprocesses the BMS file from given options. Frontend routines should use this.
pub fn preprocess_bms<R:Rng,Listener:bms::diag::BmsMessageListener>(
                                bmspath: &Path, opts: @Options, r: &mut R,
                                loaderopts: &bms::load::BmsLoaderOptions,
                                callback: &mut Listener) -> Result<~PreprocessedBms,~str> {
    let mut bms = earlyexit!(bms::load::load_bms(bmspath, r, loaderopts, callback));
    let keyspec = earlyexit!(key_spec(&bms, opts.preset.clone(),
                                      opts.leftkeys.clone(), opts.rightkeys.clone()));
    keyspec.filter_timeline(&mut bms.timeline);
    let infos = bms.timeline.analyze();
    for &modf in opts.modf.iter() {
        apply_modf(&mut bms, modf, r, keyspec);
    }
    Ok(~PreprocessedBms { bms: bms, infos: infos, keyspec: keyspec })
}

/// Internal message from the worker task to the main task.
enum Message {
    /// The worker has scanned more files.
    PushFiles(~[Path]),
    /// The worker has finished scanning files.
    NoMoreFiles,
    /// The worker has loaded the BMS file or failed to do so. Since this message can be delayed,
    /// the main task should ignore the message with non-current paths.
    BmsLoaded(Path,Result<~PreprocessedBms,~str>,~[(Option<uint>,bms::diag::BmsMessage)]),
    /// The worker has loaded the banner image (the second `~str`) for the BMS file (the first
    /// `Path`). This may be sent after `BmsLoaded` message. Due to the same reason as above
    /// the main task should ignore the message with non-current paths.
    BmsBannerLoaded(Path,~str,Envelope<PreparedSurface>),
}

/// Preloaded game data.
struct PreloadedData {
    /// A part of the game data necessary for initializing `Player`. Anything else is used for
    /// the selection screen only.
    preproc: ~PreprocessedBms,
    /// A banner texture, if any.
    banner: Option<Texture>,
    /// A tentatively calculated duration of the BMS file in seconds. This is tentative because
    /// we don't know lengths of key sounds and BGMs yet.
    duration: f64,
    /// A list of diagnostic messages returned during the loading.
    messages: ~[(Option<uint>,bms::diag::BmsMessage)],
    /// Metadata string.
    brief: ~str,
}

impl PreloadedData {
    /// Creates and calculates some necessary informations from given processed BMS and messages.
    pub fn new(preproc: ~PreprocessedBms,
               messages: ~[(Option<uint>,bms::diag::BmsMessage)]) -> PreloadedData {
        let originoffset = preproc.infos.originoffset;
        let duration = preproc.bms.timeline.duration(originoffset, |_| 0.0);

        let duration_ = (duration * 10.0) as uint;
        let difficultyname = preproc.bms.meta.difficulty.and_then(|d| d.name());
        let brief = format!("{:02}:{:02}.{} | Level {level} | BPM {bpm:.2}{hasbpmchange} | \
                             {nnotes, plural, =1{# note} other{# notes}} \
                             [{nkeys}KEY{haslongnote}{difficulty}]",
                            duration_/600, duration_/10%60, duration_%10,
                            level = preproc.bms.meta.playlevel, bpm = *preproc.bms.timeline.initbpm,
                            hasbpmchange = if preproc.infos.hasbpmchange {"?"} else {""},
                            nnotes = preproc.infos.nnotes, nkeys = preproc.keyspec.nkeys(),
                            haslongnote = if preproc.infos.haslongnote {"-LN"} else {""},
                            difficulty = difficultyname.map_default(~"", |name| ~" " + *name));

        PreloadedData { preproc: preproc, banner: None, duration: duration, messages: messages,
                        brief: brief }
    }
}

/// The state of preloaded game data.
enum PreloadingState {
    /// The selected entry cannot be used for preloading or the entry does not exist.
    NothingToPreload,
    /// The selected entry should be preloaded after given timestamp (as per `sdl::get_ticks()`).
    PreloadAfter(uint),
    /// Preloading is in progress, the result of the worker task will be delivered with this port.
    Preloading(comm::Port<task::TaskResult>),
    /// Preloading finished with a success.
    Preloaded(PreloadedData),
    /// Preloading finished with a failure. Error message is available.
    PreloadFailed(~str),
}

/// Song/pattern selection scene context. Used when the directory path is specified.
pub struct SelectingScene {
    /// Display screen.
    screen: @Screen,
    /// Game play options.
    opts: @Options,
    /// The root path of the scanner.
    root: Path,
    /// A list of paths to loaded files.
    files: ~[Path],
    /// Set to true when the scanner finished scanning.
    filesdone: bool,
    /// The index of the topmost entry visible on the screen.
    scrolloffset: uint,
    /// The index of the selected entry.
    offset: uint,
    /// Preloaded game data or preloading state if any.
    preloaded: PreloadingState,
    /// A port for receiving worker messages.
    port: comm::Port<Message>,
    /// A shared channel to which workers send messages.
    chan: comm::SharedChan<Message>,
    /// A shared cell, set to false when the scene is deactivated.
    keepgoing: RWArc<bool>,
}

/// Returns true if the path should be parsed as a BMS file.
fn is_bms_file(path: &Path) -> bool {
    use std::ascii::StrAsciiExt;
    match path.filetype() {
        Some(ext) => match ext.to_ascii_lower() {
            ~".bms" | ~".bme" | ~".bml" | ~".pms" => true,
            _ => false
        },
        _ => false
    }
}

/// Returns a task builder for worker tasks. It seems that the SDL event loop does not go well with
/// libuv loop, so we need to run workers in separate threads for now.
fn worker_task(name: ~str) -> task::TaskBuilder {
    let mut builder = task::task();
    builder.name(name);
    builder.sched_mode(task::SingleThreaded); // XXX
    builder.supervised();
    builder
}

/// Prints a diagnostic message to the screen.
pub fn print_diag(line: Option<uint>, msg: bms::diag::BmsMessage) {
    if msg.severity() == bms::diag::Internal { return; }
    let atline = match line {
        Some(line) => format!(" at line {}", line),
        None => ~"",
    };
    warn!("[{}{}] {}", msg.severity().to_str(), atline, msg.to_str());
}

/// The maximum number of entries displayed in one screen.
static NUMENTRIES: uint = 15;
/// The number of milliseconds after `update_offset` before the actual preloading starts,
/// in order to avoid the worker congestion.
static PRELOAD_DELAY: uint = 300;

impl SelectingScene {
    /// Creates a new selection scene from the screen, the root path and initial options.
    pub fn new(screen: @Screen, root: &Path, opts: @Options) -> ~SelectingScene {
        let root = os::make_absolute(root);
        let (port, chan) = comm::stream();
        let chan = comm::SharedChan::new(chan);
        ~SelectingScene {
            screen: screen, opts: opts, root: root,
            files: ~[], filesdone: false, scrolloffset: 0, offset: 0, preloaded: NothingToPreload,
            port: port, chan: chan, keepgoing: RWArc::new(true),
        }
    }

    /// Spawns a new scanning task.
    pub fn spawn_scanning_task(&self) {
        let root = self.root.clone();
        let chan = self.chan.clone();
        let keepgoing = self.keepgoing.clone();
        do worker_task(~"scanner").spawn {
            fn recur(search: &mut SearchContext, root: Path,
                     chan: &comm::SharedChan<Message>, keepgoing: &RWArc<bool>) -> bool {
                if !keepgoing.read(|v| *v) { return false; }

                let (dirs, files) = search.get_entries(&root);
                let files: ~[Path] =
                    files.iter().map(|e| root.push(*e)).filter(is_bms_file).collect();
                chan.send(PushFiles(files));
                for dir in dirs.iter() {
                    if !recur(search, root.push(*dir), chan, keepgoing) { return false; }
                }
                true
            }
            let mut search = SearchContext::new();
            if recur(&mut search, root.clone(), &chan, &keepgoing) {
                chan.send(NoMoreFiles);
            }
        }
    }

    /// Clears the current scanned files and restarts the scanning task.
    pub fn refresh(&mut self) {
        // terminates prior tasks
        assert!(self.keepgoing.read(|&v| v));
        do self.keepgoing.write |v| { *v = false; }
        self.keepgoing = RWArc::new(true);
        let (port, chan) = comm::stream();
        self.port = port;
        self.chan = comm::SharedChan::new(chan);

        self.files.clear();
        self.filesdone = false;
        self.scrolloffset = 0;
        self.offset = 0;
        self.preloaded = NothingToPreload;
        self.spawn_scanning_task();
    }

    /// Spawns a new preloading task and returns the port for its result.
    pub fn spawn_preloading_task(&self, path: &Path) -> comm::Port<task::TaskResult> {
        let bmspath = path.clone();
        let opts = (*self.opts).clone();
        let chan = self.chan.clone();
        let mut future = None;
        let mut builder = worker_task(~"preloader");
        do builder.future_result |f| { future = Some(f); }
        do builder.spawn_with(opts) |opts| {
            let mut r = rng();
            let mut diags = ~[];
            let mut callback = |line: Option<uint>, msg: bms::diag::BmsMessage| {
                diags.push((line, msg));
            };
            let loaderopts = opts.loader_options();
            let result = preprocess_bms(&bmspath, @opts, &mut r, &loaderopts, &mut callback);

            let (banner, basepath) = match result {
                Ok(ref preproc) =>
                    (preproc.bms.meta.banner.clone(), preproc.bms.meta.basepath.clone()),
                Err(_) => (None, None),
            };
            chan.send(BmsLoaded(bmspath.clone(), result, diags));

            for bannerpath in banner.iter() {
                let mut search = SearchContext::new();
                let basedir = basepath.clone().unwrap_or(Path("."));
                let fullpath = search.resolve_relative_path_for_image(*bannerpath, &basedir);
                let res = fullpath.and_then(|path| LoadedImageResource::new(&path, false));
                match res {
                    Ok(LoadedImage(surface)) => {
                        chan.send(BmsBannerLoaded(bmspath.clone(), bannerpath.clone(),
                                                  Envelope::new(surface)));
                    }
                    _ => {}
                }
            }
        }
        future.unwrap()
    }

    /// Returns a `Path` to the currently selected entry if any.
    pub fn current<'r>(&'r self) -> Option<&'r Path> {
        if self.offset < self.files.len() {Some(&self.files[self.offset])} else {None}
    }

    /// Checks if a given `Path` indeed points to the current entry.
    pub fn is_current(&self, path: &Path) -> bool {
        match self.current() { Some(current) => current == path, None => false }
    }

    /// Updates the selected entry. `offset` may be out of the range.
    pub fn update_offset(&mut self, offset: uint) {
        self.offset = offset;
        if offset < self.files.len() {
            self.preloaded = PreloadAfter(get_ticks() + PRELOAD_DELAY);
        } else {
            self.preloaded = NothingToPreload;
        }

        if self.scrolloffset > offset {
            self.scrolloffset = offset;
        } else if self.scrolloffset + (NUMENTRIES-1) < offset {
            self.scrolloffset = offset - (NUMENTRIES-1);
        }
    }

    /// Creates a new `LoadingScene` from the currently selected entry. It will load the BMS file
    /// or use the preloaded data if possible.
    pub fn create_loading_scene(&mut self) -> Option<~Scene:> {
        let preloaded = util::replace(&mut self.preloaded, PreloadAfter(0));
        let PreprocessedBms { bms, infos, keyspec } =
            match preloaded {
                Preloaded(data) => *data.preproc, // use the preloaded data if possible
                _ => {
                    let path = match self.current() {
                        Some(path) => path,
                        None => { return None; }
                    };
                    let mut r = rng();
                    let mut callback = |line, msg| print_diag(line, msg);
                    let ret = preprocess_bms(path, self.opts, &mut r,
                                             &self.opts.loader_options(), &mut callback);
                    match ret {
                        Ok(preproc) => *preproc,
                        Err(err) => { warn!("{}", err); return None; }
                    }
                }
            };
        let keymap = match read_keymap(keyspec, os::getenv) {
            Ok(map) => ~map,
            Err(err) => die!("{}", err)
        };
        Some(LoadingScene::new(self.screen, bms, infos, keyspec, keymap, self.opts) as ~Scene:)
    }
}

impl Scene for SelectingScene {
    fn activate(&mut self) -> SceneCommand {
        do self.keepgoing.write |v| { *v = true; }
        if !self.filesdone { self.refresh(); }
        event::enable_key_repeat(event::DefaultRepeatDelay, event::DefaultRepeatInterval);
        Continue
    }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new().tpslimit(50).fpslimit(20) }

    fn tick(&mut self) -> SceneCommand {
        loop {
            match event::poll_event() {
                event::KeyEvent(event::EscapeKey,true,_,_) => { return PopScene; }
                event::QuitEvent => { return Exit; }
                event::NoEvent => { break; },

                // navigation
                event::KeyEvent(event::UpKey,true,_,_) => {
                    if self.offset > 0 {
                        self.update_offset(self.offset - 1);
                    }
                }
                event::KeyEvent(event::DownKey,true,_,_) => {
                    if self.offset + 1 < self.files.len() {
                        self.update_offset(self.offset + 1);
                    }
                }
                event::KeyEvent(event::PageUpKey,true,_,_) => {
                    if self.offset > (NUMENTRIES-1) - 1 {
                        self.update_offset(self.offset - (NUMENTRIES-1));
                    } else if self.offset > 0 {
                        self.update_offset(0);
                    }
                }
                event::KeyEvent(event::PageDownKey,true,_,_) => {
                    let nfiles = self.files.len();
                    if self.offset + (NUMENTRIES-1) < nfiles {
                        self.update_offset(self.offset + (NUMENTRIES-1));
                    } else if self.offset + 1 < nfiles {
                        self.update_offset(nfiles - 1);
                    }
                }
                event::KeyEvent(event::HomeKey,true,_,_) => {
                    if self.offset > 0 {
                        self.update_offset(0);
                    }
                }
                event::KeyEvent(event::EndKey,true,_,_) => {
                    let nfiles = self.files.len();
                    if self.offset + 1 < nfiles {
                        self.update_offset(nfiles - 1);
                    }
                }

                // refresh
                event::KeyEvent(event::F5Key,true,_,_) => {
                    self.refresh();
                }

                // (auto)play
                event::KeyEvent(event::ReturnKey,true,_,_) => {
                    match self.create_loading_scene() {
                        Some(scene) => { return PushScene(scene); }
                        None => {}
                    }
                }

                _ => {}
            }
        }

        while self.port.peek() {
            match self.port.recv() {
                PushFiles(paths) => {
                    if self.files.is_empty() { // immediately preloads the first entry
                        self.preloaded = PreloadAfter(0);
                    }
                    self.files.push_all_move(paths);
                }
                NoMoreFiles => {
                    self.filesdone = true;
                }
                BmsLoaded(bmspath,preproc,messages) => {
                    if !self.is_current(&bmspath) { loop; }
                    self.preloaded = match preproc {
                        Ok(preproc) => Preloaded(PreloadedData::new(preproc, messages)),
                        Err(err) => PreloadFailed(err),
                    };
                }
                BmsBannerLoaded(bmspath,imgpath,prepared) => {
                    if !self.is_current(&bmspath) { loop; }
                    match self.preloaded {
                        Preloaded(ref mut data) if data.banner.is_none() => {
                            let prepared = prepared.unwrap();
                            match Texture::from_prepared_surface(&prepared, false, false) {
                                Ok(tex) => { data.banner = Some(tex); }
                                Err(_err) => { warn!("failed to load image \\#BANNER ({})",
                                                     imgpath.to_str()); }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        if self.offset < self.files.len() {
            let newpreloaded = match self.preloaded {
                PreloadAfter(timeout) if timeout < get_ticks() => {
                    // preload the current entry after some delay
                    match self.current() {
                        Some(path) => Some(Preloading(self.spawn_preloading_task(path))),
                        None => Some(NothingToPreload), // XXX wait what happened?!
                    }
                },
                Preloading(ref port) if port.peek() && port.recv() == task::Failure => {
                    // port.recv() may also return `task::Success` when the message is sent
                    // but being delayed. in this case we can safely ignore it.
                    Some(PreloadFailed(~"unexpected failure"))
                },
                _ => None,
            };
            if newpreloaded.is_some() {
                self.preloaded = newpreloaded.unwrap();
            }
        }

        Continue
    }

    fn render(&self) {
        self.screen.clear();

        static META_TOP: f32 = NUMENTRIES as f32 * 20.0;

        static WHITE:     Color = RGB(0xff,0xff,0xff);
        static LIGHTGRAY: Color = RGB(0xc0,0xc0,0xc0);
        static GRAY:      Color = RGB(0x80,0x80,0x80);
        static BLACK:     Color = RGB(0,0,0);

        // prints the banner first, so that the overflowing title etc. can overlap (for now)
        match self.preloaded {
            Preloaded(ref data) => {
                let x1 = SCREENW as f32 - 302.0; let y1 = META_TOP + 24.0;
                let x2 = SCREENW as f32 - 2.0;   let y2 = META_TOP + 104.0;
                if data.banner.is_some() {
                    do self.screen.draw_textured(data.banner.get_ref()) |d| {
                        d.rect(x1, y1, x2, y2);
                    }
                } else {
                    do self.screen.draw_shaded_prim(gl::LINES) |d| {
                        d.line(x1, y1, x1, y2, WHITE); d.line(x1, y1, x2, y1, WHITE);
                        d.line(x1, y1, x2, y2, WHITE); d.line(x1, y2, x2, y1, WHITE);
                        d.line(x1, y2, x2, y2, WHITE); d.line(x2, y1, x2, y2, WHITE);
                    }
                }
            }
            _ => {}
        }

        let root = self.root.push("_"); // for the calculation of `Path::get_relative_to`
        do self.screen.draw_shaded_with_font |d| {
            let top = cmp::min(self.scrolloffset, self.files.len());
            let bottom = cmp::min(self.scrolloffset + NUMENTRIES, self.files.len());
            let windowedfiles = self.files.slice(top, bottom);

            let mut y = 0.0;
            for (i, path) in windowedfiles.iter().enumerate() {
                let path = root.get_relative_to(&path.push("_"));
                if self.scrolloffset + i == self.offset { // inverted
                    d.rect(10.0, y, SCREENW as f32, y + 20.0, WHITE);
                    d.string(12.0, y + 2.0, 1.0, LeftAligned, path.to_str(), BLACK);
                } else {
                    d.string(12.0, y + 2.0, 1.0, LeftAligned, path.to_str(), WHITE);
                }
                y += 20.0;
            }
            let pixelsperslot = (META_TOP - 4.0) / cmp::max(NUMENTRIES, self.files.len()) as f32;
            d.rect(2.0, 2.0 + top as f32 * pixelsperslot,
                   7.0, 2.0 + (top + NUMENTRIES) as f32 * pixelsperslot, LIGHTGRAY);
            d.rect(0.0, META_TOP + 1.0, SCREENW as f32, META_TOP + 2.0, WHITE);

            // preloaded data if any
            match self.preloaded {
                NothingToPreload | PreloadAfter(*) => {}
                Preloading(*) => {
                    d.string(4.0, META_TOP + 4.0, 1.0, LeftAligned,
                             "loading...", LIGHTGRAY);
                }
                Preloaded(ref data) => {
                    let meta = &data.preproc.bms.meta;

                    let mut top = META_TOP + 4.0;
                    let genre = meta.genre.map_default("", |s| s.as_slice());
                    d.string(4.0, top, 1.0, LeftAligned, genre, LIGHTGRAY);
                    top += 18.0;
                    let title = meta.title.map_default("", |s| s.as_slice());
                    if !title.is_empty() {
                        d.string(6.0, top + 2.0, 2.0, LeftAligned, title, GRAY);
                        d.string(4.0, top, 2.0, LeftAligned, title, WHITE);
                    } else {
                        d.string(4.0, top, 2.0, LeftAligned, "(no title)", GRAY);
                    }
                    top += 36.0;
                    for subtitle in meta.subtitles.iter() {
                        if subtitle.is_empty() { loop; }
                        d.string(21.0, top + 1.0, 1.0, LeftAligned, *subtitle, GRAY);
                        d.string(20.0, top, 1.0, LeftAligned, *subtitle, WHITE);
                        top += 18.0;
                    }
                    let artist = meta.artist.map_default("", |s| s.as_slice());
                    if !artist.is_empty() {
                        d.string(4.0, top, 1.0, LeftAligned, artist, WHITE);
                        top += 18.0;
                    }
                    for subartist in meta.subartists.iter() {
                        if subartist.is_empty() { loop; }
                        d.string(20.0, top, 1.0, LeftAligned, *subartist, WHITE);
                        top += 18.0;
                    }
                    for comment in meta.comments.iter() {
                        if comment.is_empty() { loop; }
                        d.string(4.0, top, 1.0, LeftAligned,
                                 format!("> {}", *comment), RGB(0x80,0xff,0x80));
                        top += 18.0;
                    }
                    for &(line, msg) in data.messages.iter() {
                        if top > SCREENH as f32 { break; }

                        let atline = match line {
                            Some(line) => format!(" (line {})", line),
                            None => ~"",
                        };
                        let text = format!("* {}: {}{}", msg.severity().to_str(),
                                           msg.to_str(), atline);
                        let color = match msg.severity() {
                            bms::diag::Fatal => RGB(0xff,0x40,0x40),
                            bms::diag::Warning => RGB(0xff,0xff,0x40),
                            bms::diag::Note => RGB(0x40,0xff,0xff),
                            bms::diag::Internal => loop, // impossible
                        };
                        d.string(4.0, top, 1.0, LeftAligned, text, color);
                        top += 18.0;

                        if msg == bms::diag::BmsUsesLegacyEncoding {
                            let (encname, confidence) = meta.encoding;
                            d.string(4.0, top, 1.0, LeftAligned,
                                     format!("  (Detected \"{}\" encoding with confidence {:.2}%)",
                                             encname, confidence * 100.0),
                                     RGB(0x20,0x80,0x80));
                            top += 18.0;
                        }
                    }

                    d.string(SCREENW as f32 - 2.0, META_TOP + 4.0, 1.0, RightAligned,
                             data.brief, GRAY);
                }
                PreloadFailed(ref err) => {
                    d.string(4.0, META_TOP + 4.0, 1.0, LeftAligned,
                             format!("error: {}", *err), LIGHTGRAY);
                }
            }

            // status bar will overwrite any overflowing messages
            d.rect(0.0, SCREENH as f32 - 20.0, SCREENW as f32, SCREENH as f32, WHITE);
            d.string(2.0, SCREENH as f32 - 18.0, 1.0, LeftAligned,
                     format!("Up/Down/PgUp/PgDn/Home/End: Select   \
                              Enter: {}   F5: Refresh   Esc: Quit",
                             if self.opts.is_autoplay() {"Autoplay"} else {"Play"}),
                     RGB(0,0,0));
        }

        self.screen.swap_buffers();
    }

    fn deactivate(&mut self) {
        event::enable_key_repeat(event::CustomRepeatDelay(0), event::DefaultRepeatInterval);
        do self.keepgoing.write |v| { *v = false; }
    }

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}

