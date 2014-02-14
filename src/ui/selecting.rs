// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Song and pattern selection screen.

use std::{str, cmp, io, os, comm, task};
use std::rc::Rc;
use std::cell::RefCell;
use std::rand::{rng, Rng};
use std::comm::{Chan, Port};
use sync::RWArc;

use sdl::{event, get_ticks};
use gl = opengles::gl2;
use format::timeline::TimelineInfo;
use format::bms;
use format::bms::Bms;
use util::filesearch::SearchContext;
use util::envelope::Envelope;
use util::md5::{MD5, ToHex};
use gfx::color::{Color, RGB};
use gfx::gl::{PreparedSurface, Texture2D};
use gfx::draw::{ShadedDrawingTraits, TexturedDrawingTraits};
use gfx::bmfont::{LeftAligned, RightAligned};
use gfx::screen::Screen;
use engine::input::read_keymap;
use engine::keyspec::{KeySpec, key_spec};
use engine::resource::{SearchContextAdditions, LoadedImageResource, LoadedImage};
use engine::player::apply_modf;
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
pub fn preprocess_bms<'r,R:Rng>(
        bmspath: &Path, f: &mut Reader, opts: &Options, r: &mut R,
        loaderopts: &bms::load::LoaderOptions, callback: bms::load::Callback<'r>)
                                -> Result<~PreprocessedBms,~str> {
    let bms = if_ok!(bms::load::load_bms(f, r, loaderopts, callback));
    let mut bms = bms.with_bmspath(bmspath);
    let keyspec = if_ok!(key_spec(&bms, opts.preset.clone(),
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
    /// The worker has failed to read and/or load the BMS file. Error message follows.
    BmsFailed(Path,~str),
    /// The worker has read the BMS file and calculated its MD5 hash.
    BmsHashRead(Path,[u8, ..16]),
    /// The worker has loaded the BMS file or failed to do so. Since this message can be delayed,
    /// the main task should ignore the message with non-current paths.
    BmsLoaded(Path,~PreprocessedBms,~[(Option<uint>,bms::diag::BmsMessage)]),
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
    banner: Option<Texture2D>,
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
                            level = preproc.bms.meta.playlevel,
                            bpm = preproc.bms.timeline.initbpm.to_f64(),
                            hasbpmchange = if preproc.infos.hasbpmchange {"?"} else {""},
                            nnotes = preproc.infos.nnotes, nkeys = preproc.keyspec.nkeys(),
                            haslongnote = if preproc.infos.haslongnote {"-LN"} else {""},
                            difficulty = difficultyname.map_or(~"", |name| ~" " + name));

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
    /// Preloading is in progress.
    Preloading,
    /// Preloading finished with a success.
    Preloaded(PreloadedData),
    /// Preloading finished with a failure. Error message is available.
    PreloadFailed(~str),
}

/// Song/pattern selection scene context. Used when the directory path is specified.
pub struct SelectingScene {
    /// Display screen.
    screen: Rc<RefCell<Screen>>,
    /// Game play options.
    opts: Rc<Options>,
    /// The root path of the scanner.
    root: Path,
    /// A list of paths to loaded files, and MD5 hashes of them if read.
    files: ~[(Path, Option<[u8, ..16]>)],
    /// Set to true when the scanner finished scanning.
    filesdone: bool,
    /// The index of the topmost entry visible on the screen.
    scrolloffset: uint,
    /// The index of the selected entry.
    offset: uint,
    /// Preloaded game data or preloading state if any.
    preloaded: PreloadingState,
    /// A port for receiving worker messages.
    port: Port<Message>,
    /// A shared channel to which workers send messages.
    chan: Chan<Message>,
    /// A shared cell, set to false when the scene is deactivated.
    keepgoing: RWArc<bool>,
}

/// Returns true if the path should be parsed as a BMS file.
fn is_bms_file(path: &Path) -> bool {
    use std::ascii::StrAsciiExt;
    match path.extension().and_then(str::from_utf8) {
        Some(ext) => match ext.to_ascii_lower() {
            ~"bms" | ~"bme" | ~"bml" | ~"pms" => true,
            _ => false
        },
        _ => false
    }
}

/// Spawns an worker task. We are required to use `libnative` due to the SDL event loop.
/// Also we need to use our own wrapper to avoid "sending on a closed channel" error from
/// the default `future_result` wrapper.
fn spawn_worker_task(name: ~str, body: proc(), on_error: proc()) {
    let mut builder = task::task();
    builder.name(name + " wrapper");
    builder.watched();
    builder.spawn(proc() {
        let mut builder = task::task();
        builder.name(name);
        builder.watched();
        let ret = builder.try(body);
        if ret.is_err() { on_error(); }
    });
}

/// Prints a diagnostic message to the screen.
/// This can be directly used as a parser message callback.
pub fn print_diag(line: Option<uint>, msg: bms::diag::BmsMessage) -> bool {
    let atline = match line {
        Some(line) => format!(" at line {}", line),
        None => ~"",
    };
    warn!("[{}{}] {}", msg.severity().to_str(), atline, msg.to_str());
    true
}

/// The maximum number of entries displayed in one screen.
static NUMENTRIES: uint = 15;
/// The number of milliseconds after `update_offset` before the actual preloading starts,
/// in order to avoid the worker congestion.
static PRELOAD_DELAY: uint = 300;

impl SelectingScene {
    /// Creates a new selection scene from the screen, the root path and initial options.
    pub fn new(screen: Rc<RefCell<Screen>>, root: &Path, opts: Rc<Options>) -> ~SelectingScene {
        let root = os::make_absolute(root);
        let (port, chan) = Chan::new();
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
        let chan_ = self.chan.clone();
        let keepgoing = self.keepgoing.clone();
        spawn_worker_task(~"scanner", proc() {
            fn recur(search: &mut SearchContext, root: Path,
                     chan: &Chan<Message>, keepgoing: &RWArc<bool>) -> bool {
                if !keepgoing.read(|v| *v) { return false; }

                let (dirs, files) = {
                    let (dirs, files) = search.get_entries(&root);
                    (dirs.to_owned(), files.iter().map(|e| e.clone()).filter(is_bms_file).collect())
                }; // so that we can reborrow `search`
                chan.try_send(PushFiles(files));
                for dir in dirs.move_iter() {
                    if !recur(search, dir, chan, keepgoing) { return false; }
                }
                true
            }
            let mut search = SearchContext::new();
            if recur(&mut search, root.clone(), &chan, &keepgoing) {
                chan.try_send(NoMoreFiles);
            }
        }, proc() {
            chan_.try_send(NoMoreFiles);
        });
    }

    /// Clears the current scanned files and restarts the scanning task.
    pub fn refresh(&mut self) {
        // terminates prior tasks
        assert!(self.keepgoing.read(|&v| v));
        self.keepgoing.write(|v| { *v = false; });
        self.keepgoing = RWArc::new(true);
        let (port, chan) = Chan::new();
        self.port = port;
        self.chan = chan;

        self.files.clear();
        self.filesdone = false;
        self.scrolloffset = 0;
        self.offset = 0;
        self.preloaded = NothingToPreload;
        self.spawn_scanning_task();
    }

    /// Spawns a new preloading task.
    pub fn spawn_preloading_task(&self, path: &Path) {
        let bmspath = path.clone();
        let bmspath_ = path.clone();
        let opts = self.opts.borrow().clone();
        let chan = self.chan.clone();
        let chan_ = self.chan.clone();
        spawn_worker_task(~"preloader", proc() {
            let opts = Rc::new(opts);

            let load_banner = |bannerpath: ~str, basepath: Option<Path>| {
                let mut search = SearchContext::new();
                let basedir = basepath.clone().unwrap_or(Path::new("."));
                let fullpath = search.resolve_relative_path_for_image(bannerpath, &basedir);
                let res = fullpath.and_then(|path| LoadedImageResource::new(&path, false));
                match res {
                    Ok(LoadedImage(surface)) => {
                        chan.try_send(BmsBannerLoaded(bmspath.clone(), bannerpath,
                                                      Envelope::new(surface)));
                    }
                    _ => {}
                }
            };

            let load_with_reader = |mut f| {
                let mut r = rng();
                let mut diags = ~[];
                let loaderopts = opts.borrow().loader_options();

                let preproc = {
                    let callback = |line: Option<uint>, msg: bms::diag::BmsMessage| {
                        diags.push((line, msg));
                        true
                    };
                    preprocess_bms(&bmspath, &mut f, opts.borrow(), &mut r, &loaderopts, callback)
                };
                match preproc {
                    Ok(preproc) => {
                        let banner = preproc.bms.meta.banner.clone();
                        let basepath = preproc.bms.meta.basepath.clone();
                        chan.try_send(BmsLoaded(bmspath.clone(), preproc, diags));
                        if banner.is_some() {
                            load_banner(banner.unwrap(), basepath);
                        }
                    }
                    Err(err) => {
                        chan.try_send(BmsFailed(bmspath.clone(), err));
                    }
                }
            };

            match io::File::open(&bmspath) {
                Ok(mut f) => {
                    let buf = f.read_to_end().ok().unwrap_or_else(|| ~[]);
                    let hash = MD5::from_buffer(buf).final();
                    chan.try_send(BmsHashRead(bmspath.clone(), hash));

                    // since we have read the file we don't want the parser to read it again.
                    load_with_reader(io::MemReader::new(buf));
                }
                Err(err) => {
                    chan.try_send(BmsFailed(bmspath.clone(), err.to_str()));
                }
            }
        }, proc() {
            // the task failed to send the error message, so the wrapper sends it instead
            chan_.try_send(BmsFailed(bmspath_, ~"unexpected error"));
        });
    }

    /// Returns a `Path` to the currently selected entry if any.
    pub fn current<'r>(&'r self) -> Option<&'r Path> {
        if self.offset < self.files.len() {
            let (ref path, _) = self.files[self.offset];
            Some(path)
        } else {
            None
        }
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
        #[cfg(rust_nightly_20140206)] use std::util::replace;
        #[cfg(not(rust_nightly_20140206))] use std::mem::replace;

        let preloaded = replace(&mut self.preloaded, PreloadAfter(0));
        let PreprocessedBms { bms, infos, keyspec } =
            match preloaded {
                Preloaded(data) => *data.preproc, // use the preloaded data if possible
                _ => {
                    let path = match self.current() {
                        Some(path) => path,
                        None => { return None; }
                    };
                    let mut f = match io::File::open(path) {
                        Ok(f) => f,
                        Err(err) => {
                            warn!("Failed to open {}: {}", path.display(), err);
                            return None;
                        }
                    };
                    let mut r = rng();
                    let opts = self.opts.borrow();
                    let ret = preprocess_bms(path, &mut f as &mut Reader, opts, &mut r,
                                             &opts.loader_options(), print_diag);
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
        Some(LoadingScene::new(self.screen.clone(), bms, infos,
                               keyspec, keymap, self.opts.clone()) as ~Scene:)
    }
}

impl Scene for SelectingScene {
    fn activate(&mut self) -> SceneCommand {
        self.keepgoing.write(|v| { *v = true; });
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

        loop {
            match self.port.try_recv() {
                comm::Empty | comm::Disconnected => { break; }

                comm::Data(PushFiles(paths)) => {
                    if self.files.is_empty() { // immediately preloads the first entry
                        self.preloaded = PreloadAfter(0);
                    }
                    for path in paths.move_iter() {
                        self.files.push((path, None));
                    }
                }
                comm::Data(NoMoreFiles) => {
                    self.filesdone = true;
                }
                comm::Data(BmsFailed(bmspath,err)) => {
                    if !self.is_current(&bmspath) { continue; }
                    self.preloaded = PreloadFailed(err);
                }
                comm::Data(BmsHashRead(bmspath,hash)) => {
                    if !self.is_current(&bmspath) { continue; }
                    let (_, ref mut hash0) = self.files[self.offset];
                    *hash0 = Some(hash);
                }
                comm::Data(BmsLoaded(bmspath,preproc,messages)) => {
                    if !self.is_current(&bmspath) { continue; }
                    self.preloaded = Preloaded(PreloadedData::new(preproc, messages));
                }
                comm::Data(BmsBannerLoaded(bmspath,imgpath,prepared)) => {
                    if !self.is_current(&bmspath) { continue; }
                    match self.preloaded {
                        Preloaded(ref mut data) if data.banner.is_none() => {
                            let prepared = prepared.unwrap();
                            match Texture2D::from_prepared_surface(&prepared, false, false) {
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
            match self.preloaded {
                PreloadAfter(timeout) if timeout < get_ticks() => {
                    // preload the current entry after some delay
                    self.preloaded = match self.current() {
                        Some(path) => { self.spawn_preloading_task(path); Preloading },
                        None => NothingToPreload, // XXX wait what happened?!
                    };
                }
                _ => {}
            }
        }

        Continue
    }

    fn render(&self) {
        let screen__ = self.screen.borrow();
        let mut screen_ = screen__.borrow_mut();
        let screen = screen_.get();

        screen.clear();

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
                    screen.draw_textured(data.banner.get_ref(), |d| {
                        d.rect(x1, y1, x2, y2);
                    });
                } else {
                    screen.draw_shaded_prim(gl::LINES, |d| {
                        d.line(x1, y1, x1, y2, WHITE); d.line(x1, y1, x2, y1, WHITE);
                        d.line(x1, y1, x2, y2, WHITE); d.line(x1, y2, x2, y1, WHITE);
                        d.line(x1, y2, x2, y2, WHITE); d.line(x2, y1, x2, y2, WHITE);
                    });
                }
            }
            _ => {}
        }

        screen.draw_shaded_with_font(|d| {
            let top = cmp::min(self.scrolloffset, self.files.len());
            let bottom = cmp::min(self.scrolloffset + NUMENTRIES, self.files.len());
            let windowedfiles = self.files.slice(top, bottom);

            let mut y = 0.0;
            for (i, &(ref path, ref hash)) in windowedfiles.iter().enumerate() {
                let path = path.path_relative_from(&self.root).unwrap_or_else(|| path.clone());
                if self.scrolloffset + i == self.offset { // inverted
                    d.rect(10.0, y, SCREENW as f32, y + 20.0, WHITE);
                    d.string(12.0, y + 2.0, 1.0, LeftAligned, path.display().to_str(), BLACK);
                } else {
                    d.string(12.0, y + 2.0, 1.0, LeftAligned, path.display().to_str(), WHITE);
                }
                for hash in hash.iter() {
                    d.string(SCREENW as f32 - 4.0, y + 2.0, 1.0, RightAligned, hash.to_hex(), GRAY);
                }
                y += 20.0;
            }
            let pixelsperslot = (META_TOP - 4.0) / cmp::max(NUMENTRIES, self.files.len()) as f32;
            d.rect(2.0, 2.0 + top as f32 * pixelsperslot,
                   7.0, 2.0 + (top + NUMENTRIES) as f32 * pixelsperslot, LIGHTGRAY);
            d.rect(0.0, META_TOP + 1.0, SCREENW as f32, META_TOP + 2.0, WHITE);

            // preloaded data if any
            match self.preloaded {
                NothingToPreload | PreloadAfter(..) => {}
                Preloading(..) => {
                    d.string(4.0, META_TOP + 4.0, 1.0, LeftAligned,
                             "loading...", LIGHTGRAY);
                }
                Preloaded(ref data) => {
                    let meta = &data.preproc.bms.meta;

                    let mut top = META_TOP + 4.0;
                    let genre = meta.genre.as_ref().map_or("", |s| s.as_slice());
                    d.string(4.0, top, 1.0, LeftAligned, genre, LIGHTGRAY);
                    top += 18.0;
                    let title = meta.title.as_ref().map_or("", |s| s.as_slice());
                    if !title.is_empty() {
                        d.string(6.0, top + 2.0, 2.0, LeftAligned, title, GRAY);
                        d.string(4.0, top, 2.0, LeftAligned, title, WHITE);
                    } else {
                        d.string(4.0, top, 2.0, LeftAligned, "(no title)", GRAY);
                    }
                    top += 36.0;
                    for subtitle in meta.subtitles.iter() {
                        if subtitle.is_empty() { continue; }
                        d.string(21.0, top + 1.0, 1.0, LeftAligned, *subtitle, GRAY);
                        d.string(20.0, top, 1.0, LeftAligned, *subtitle, WHITE);
                        top += 18.0;
                    }
                    let artist = meta.artist.as_ref().map_or("", |s| s.as_slice());
                    if !artist.is_empty() {
                        d.string(4.0, top, 1.0, LeftAligned, artist, WHITE);
                        top += 18.0;
                    }
                    for subartist in meta.subartists.iter() {
                        if subartist.is_empty() { continue; }
                        d.string(20.0, top, 1.0, LeftAligned, *subartist, WHITE);
                        top += 18.0;
                    }
                    for comment in meta.comments.iter() {
                        if comment.is_empty() { continue; }
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
                             if self.opts.borrow().is_autoplay() {"Autoplay"} else {"Play"}),
                     RGB(0,0,0));
        });

        screen.swap_buffers();
    }

    fn deactivate(&mut self) {
        event::enable_key_repeat(event::CustomRepeatDelay(0), event::DefaultRepeatInterval);
        self.keepgoing.write(|v| { *v = false; });
    }

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}

