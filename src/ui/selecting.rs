// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Song and pattern selection screen.

use std::{str, cmp, io, os, comm, task};
use std::rc::Rc;
use std::cell::RefCell;
use std::comm::{Sender, Receiver};
use std::rand::{task_rng, Rng};
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RWLock, TaskPool};

use sdl::{event, get_ticks};
use format::timeline::TimelineInfo;
use format::metadata::Meta;
use format::bms;
use format::bms::Bms;
use util::filesearch::SearchContext;
use util::envelope::Envelope;
use util::md5::MD5Hash;
use gfx::gl::{PreparedSurface, Texture2D};
use gfx::screen::Screen;
use gfx::skin::scalar::{Scalar, IntoScalar};
use gfx::skin::hook::Hook;
use gfx::skin::render::Renderer;
use engine::input::read_keymap;
use engine::keyspec::{KeySpec, key_spec};
use engine::resource::{SearchContextAdditions, LoadedImageResource, LoadedImage};
use engine::cache::MetadataCache;
use engine::player::apply_modf;
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PushScene, PopScene, Exit};
use ui::options::Options;
use ui::loading::LoadingScene;

/// The BMS data that has been preprocessed for modifiers and analyzed but yet to be loaded.
pub struct PreprocessedBms {
    /// Yet-to-be-loaded BMS data.
    pub bms: Bms,
    /// The derived timeline information.
    pub infos: TimelineInfo,
    /// The key specification.
    pub keyspec: KeySpec,
}

/// Loads and preprocesses the BMS file from given options. Frontend routines should use this.
pub fn preprocess_bms<'r,R:Rng>(
        bmspath: &Path, f: &mut Reader, opts: &Options, r: &mut R,
        loaderopts: &bms::load::LoaderOptions, callback: bms::load::Callback<'r>)
                                -> Result<PreprocessedBms,String> {
    let bms = try!(bms::load::load_bms(f, r, loaderopts, callback));
    let mut bms = bms.with_bmspath(bmspath);
    let keyspec = try!(key_spec(&bms, opts.preset.clone(),
                                opts.leftkeys.clone(), opts.rightkeys.clone()));
    keyspec.filter_timeline(&mut bms.timeline);
    let infos = bms.timeline.analyze();
    for &modf in opts.modf.iter() {
        apply_modf(&mut bms, modf, r, &keyspec);
    }
    Ok(PreprocessedBms { bms: bms, infos: infos, keyspec: keyspec })
}

/// Internal message from the worker task to the main task.
enum Message {
    /// The worker has scanned more files, some of them with their MD5 hashes.
    PushFiles(Vec<(Path, Option<MD5Hash>)>),
    /// The worker has finished scanning files.
    NoMoreFiles,
    /// The worker has failed to read and/or load the BMS file. Error message follows.
    BmsFailed(Path, String),
    /// The worker has read the BMS file and calculated its MD5 hash.
    BmsHashRead(Path, MD5Hash),
    /// The worker has read the cached metadata.
    BmsCacheLoaded(Path, Meta),
    /// The worker has loaded the BMS file or failed to do so. Since this message can be delayed,
    /// the main task should ignore the message with non-current paths.
    BmsLoaded(Path, PreprocessedBms, Vec<(Option<uint>,bms::diag::BmsMessage)>),
    /// The worker has loaded the banner image (the second `String`) for the BMS file (the first
    /// `Path`). This may be sent after `BmsLoaded` message. Due to the same reason as above
    /// the main task should ignore the message with non-current paths.
    BmsBannerLoaded(Path, String, Envelope<PreparedSurface>),
}

/// Preloaded game data.
pub struct PreloadedData {
    /// A part of the game data necessary for initializing `Player`. Anything else is used for
    /// the selection screen only.
    pub preproc: PreprocessedBms,
    /// A banner texture, if any.
    pub banner: Option<Rc<Texture2D>>,
    /// A tentatively calculated duration of the BMS file in seconds. This is tentative because
    /// we don't know lengths of key sounds and BGMs yet.
    pub duration: f64,
    /// A list of diagnostic messages returned during the loading.
    pub messages: Vec<(Option<uint>,bms::diag::BmsMessage)>,
}

impl PreloadedData {
    /// Creates and calculates some necessary informations from given processed BMS and messages.
    pub fn new(preproc: PreprocessedBms,
               messages: Vec<(Option<uint>,bms::diag::BmsMessage)>) -> PreloadedData {
        let originoffset = preproc.infos.originoffset;
        let duration = preproc.bms.timeline.duration(originoffset, |_| 0.0);
        PreloadedData { preproc: preproc, banner: None, duration: duration, messages: messages }
    }
}

/// The state of preloaded game data.
pub enum PreloadingState {
    /// The selected entry cannot be used for preloading or the entry does not exist.
    NothingToPreload,
    /// The selected entry should be preloaded after given timestamp (as per `sdl::get_ticks()`).
    PreloadAfter(uint),
    /// Preloading is in progress.
    Preloading,
    /// Preloading finished with a success.
    Preloaded(PreloadedData),
    /// Preloading finished with a failure. Error message is available.
    PreloadFailed(String),
}

/// The scanned entry.
pub struct Entry {
    /// A path to loaded file.
    pub path: Path,
    /// MD5 hash. Only present when it has been read.
    pub hash: Option<MD5Hash>,
    /// Loaded metadata if any.
    pub meta: Option<Meta>,
}

/// Song/pattern selection scene context. Used when the directory path is specified.
pub struct SelectingScene {
    /// Display screen.
    pub screen: Rc<RefCell<Screen>>,
    /// Game play options.
    pub opts: Rc<Options>,
    /// Skin renderer.
    pub skin: RefCell<Renderer>,
    /// Shared metadata cache.
    ///
    /// This has to be shared, otherwise each task would try to write to the same database file
    /// simultaneously and SQLite would give up for most cases.
    cache: Arc<Mutex<MetadataCache>>,

    /// The root path of the scanner.
    pub root: Path,
    /// A list of scanned entries.
    pub files: Vec<Entry>,
    /// A mapping from the path to scanned entries.
    fileindices: HashMap<Path, uint>,
    /// Set to true when the scanner finished scanning.
    pub filesdone: bool,
    /// The index of the topmost entry visible on the screen.
    pub scrolloffset: uint,
    /// The index of the selected entry.
    pub offset: uint,
    /// Preloaded game data or preloading state if any.
    pub preloaded: PreloadingState,

    /// A task pool for various purposes.
    pool: TaskPool<()>,
    /// A receiver for receiving worker messages.
    receiver: Receiver<Message>,
    /// A shared sender to which workers send messages.
    sender: Sender<Message>,
    /// A shared cell, set to false when the scene is deactivated.
    keepgoing: Arc<RWLock<bool>>,
}

/// Returns true if the path should be parsed as a BMS file.
fn is_bms_file(path: &Path) -> bool {
    use std::ascii::StrAsciiExt;
    match path.extension().and_then(str::from_utf8) {
        Some(ext) => match ext.to_ascii_lower()[] {
            "bms" | "bme" | "bml" | "pms" => true,
            _ => false
        },
        _ => false
    }
}

/// Spawns an worker task. We are required to use `libnative` due to the SDL event loop.
/// Also we need to use our own wrapper to avoid "sending on a closed channel" error from
/// the default `future_result` wrapper.
fn spawn_worker_task(name: &'static str, body: proc():Send, on_error: proc():Send) {
    task::TaskBuilder::new().named(name.to_string() + " wrapper").spawn(proc() {
        let ret = task::TaskBuilder::new().named(name).try(body);
        if ret.is_err() { on_error(); }
    });
}

/// Prints a diagnostic message to the screen.
/// This can be directly used as a parser message callback.
pub fn print_diag(line: Option<uint>, msg: bms::diag::BmsMessage) -> bool {
    use util::std::option::StrOption;

    let atline = line.map(|line| format!(" at line {}", line));
    warn!("[{}{}] {}", msg.severity, atline.as_ref_slice_or(""), msg);
    true
}

/// The maximum number of entries displayed in one screen.
const NUMENTRIES: uint = 15;
/// The number of milliseconds after `update_offset` before the actual preloading starts,
/// in order to avoid the worker congestion.
const PRELOAD_DELAY: uint = 300;

impl SelectingScene {
    /// Creates a new selection scene from the screen, the root path and initial options.
    pub fn new(screen: Rc<RefCell<Screen>>, root: &Path, opts: Rc<Options>) -> Box<SelectingScene> {
        let skin = match opts.load_skin("selecting.cson") {
            Ok(skin) => skin,
            Err(err) => die!("{}", err),
        };
        let cache = match opts.open_metadata_cache() {
            Ok(cache) => cache,
            Err(err) => die!("can't open metadata cache: {}", err),
        };
        let root = os::make_absolute(root);
        let (sender, receiver) = comm::channel();
        let pool = TaskPool::new(os::num_cpus(), || proc(_) ());
        box SelectingScene {
            screen: screen, opts: opts, skin: RefCell::new(Renderer::new(skin)),
            cache: Arc::new(Mutex::new(cache)),
            root: root, files: Vec::new(), fileindices: HashMap::new(),
            filesdone: false, scrolloffset: 0, offset: 0, preloaded: NothingToPreload,
            pool: pool, receiver: receiver, sender: sender,
            keepgoing: Arc::new(RWLock::new(true)),
        }
    }

    /// Spawns a new scanning task.
    pub fn spawn_scanning_task(&self) {
        let root = self.root.clone();
        let sender = self.sender.clone();
        let sender_ = self.sender.clone();
        let keepgoing = self.keepgoing.clone();
        let cache = self.cache.clone();
        spawn_worker_task("scanner", proc() {
            debug!("scanner: start");

            fn recur(cache: &Mutex<MetadataCache>, root: Path,
                     sender: &Sender<Message>, keepgoing: &Arc<RWLock<bool>>) -> bool {
                if !*keepgoing.read() { return false; }

                let ret = cache.lock().get_entries(&root);
                let (dirs, files) = match ret {
                    Ok((dirs, files)) => {
                        (dirs, files.into_iter().filter(|&(ref p, _)| is_bms_file(p)).collect())
                    }
                    Err(err) => {
                        warn!("scanner failed to read {}: {}", root.display(), err);
                        (Vec::new(), Vec::new())
                    }
                };
                sender.send(PushFiles(files));
                for dir in dirs.into_iter() {
                    if !recur(cache, dir, sender, keepgoing) { return false; }
                }
                true
            }

            if recur(cache.deref(), root.clone(), &sender, &keepgoing) {
                sender.send(NoMoreFiles);
            }

            debug!("scanner: done");
        }, proc() {
            sender_.send(NoMoreFiles);
        });
    }

    /// Makes a new preloading task. This can run on a separate task or a task pool.
    fn make_preloading_task(&self, path: &Path) -> proc(): Send {
        let bmspath = path.clone();
        let opts = self.opts.deref().clone();
        let sender = self.sender.clone();
        let cache = self.cache.clone();
        proc() {
            debug!("preloader for {}: start", bmspath.display());

            let opts = Rc::new(opts);

            let load_banner = |bannerpath: String, basepath: Option<Path>| {
                let mut search = SearchContext::new();
                let basedir = basepath.clone().unwrap_or(Path::new("."));
                let fullpath =
                    search.resolve_relative_path_for_image(bannerpath[], &basedir);
                let res = fullpath.and_then(|path| LoadedImageResource::new(&path, false));
                match res {
                    Ok(LoadedImage(surface)) => {
                        sender.send(BmsBannerLoaded(bmspath.clone(), bannerpath,
                                                    Envelope::new(surface)));
                    }
                    _ => {}
                }
            };

            let load_with_reader = |(hash, mut f): (MD5Hash, io::File)| -> Result<(), String> {
                let mut r = task_rng();
                let mut diags = Vec::new();
                let loaderopts = opts.loader_options();

                let preproc = {
                    let callback = |line: Option<uint>, msg: bms::diag::BmsMessage| {
                        diags.push((line, msg));
                        true
                    };
                    try!(preprocess_bms(&bmspath, &mut f, opts.deref(),
                                        &mut r, &loaderopts, callback))
                };

                let banner = preproc.bms.meta.banner.clone();
                let basepath = preproc.bms.meta.basepath.clone();
                let meta = preproc.bms.meta.common.clone();
                sender.send(BmsCacheLoaded(bmspath.clone(), meta.clone()));
                sender.send(BmsLoaded(bmspath.clone(), preproc, diags));

                let _ = cache.lock().put_metadata(&hash, meta);
                if banner.is_some() {
                    load_banner(banner.unwrap(), basepath);
                }
                Ok(())
            };

            let get_hash_and_reader = || -> Result<(MD5Hash, io::File), String> {
                // we have read the file so we don't want the parser to read it again.
                let (hash, f) = try!(cache.lock().get_hash(&bmspath).map_err(|e| e.to_string()));
                sender.send(BmsHashRead(bmspath.clone(), hash));
                let f = match f {
                    Some(f) => f,
                    None => try!(io::File::open(&bmspath).map_err(|e| e.to_string())),
                };
                Ok((hash, f))
            };

            match get_hash_and_reader().and_then(load_with_reader) {
                Ok(()) => {}
                Err(err) => { sender.send(BmsFailed(bmspath, err)); }
            }

            debug!("preloader: done");
        }
    }

    /// Makes a new cached preloading task.
    /// This task tries to read the cache first, and falls back to the preloading task if needed.
    fn make_cached_preloading_task(&self, hash: &MD5Hash, path: &Path) -> proc(): Send {
        let hash = *hash;
        let bmspath = path.clone();
        let cache = self.cache.clone();
        let sender = self.sender.clone();
        let job = self.make_preloading_task(path);
        proc() {
            debug!("cached preloader for {} ({}): start", bmspath.display(), hash);
            let meta = cache.lock().get_metadata(&hash);
            match meta {
                Ok(Some(meta)) => {
                    sender.send(BmsCacheLoaded(bmspath, meta.clone()));
                    let _ = cache.lock().put_metadata(&hash, meta);
                    debug!("cached preloader: done");
                }
                Ok(None) | Err(..) => {
                    job();
                }
            }
        }
    }

    /// Spawns a new preloading task.
    pub fn spawn_preloading_task(&self, path: &Path) {
        let bmspath = path.clone();
        let sender = self.sender.clone();
        spawn_worker_task("preloader", self.make_preloading_task(path), proc() {
            // the task failed to send the error message, so the wrapper sends it instead
            sender.send(BmsFailed(bmspath, format!("unexpected error")));
        });
    }

    /// Reads a cached metadata if possible.
    pub fn get_cached_metadata(&self, hash: &MD5Hash) -> Option<Meta> {
        match self.cache.lock().get_metadata(hash) {
            Ok(Some(meta)) => Some(meta),
            Ok(None) | Err(..) => None,
        }
    }

    /// Clears the current scanned files and restarts the scanning task.
    pub fn refresh(&mut self) {
        // terminates prior tasks
        assert!(*self.keepgoing.read());
        *self.keepgoing.write() = false;
        self.keepgoing = Arc::new(RWLock::new(true));
        let (sender, receiver) = comm::channel();
        self.receiver = receiver;
        self.sender = sender;

        self.files.clear();
        self.filesdone = false;
        self.scrolloffset = 0;
        self.offset = 0;
        self.preloaded = NothingToPreload;
        self.spawn_scanning_task();
    }

    /// Returns a `Path` to the currently selected entry if any.
    pub fn current<'r>(&'r self) -> Option<&'r Path> {
        if self.offset < self.files.len() {
            Some(&self.files[self.offset].path)
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
    pub fn create_loading_scene(&mut self) -> Option<Box<Scene+'static>> {
        use std::mem::replace;

        let preloaded = replace(&mut self.preloaded, PreloadAfter(0));
        let PreprocessedBms { bms, infos, keyspec } =
            match preloaded {
                Preloaded(data) => data.preproc, // use the preloaded data if possible
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
                    let mut r = task_rng();
                    let opts = self.opts.deref();
                    let ret = preprocess_bms(path, &mut f as &mut Reader, opts, &mut r,
                                             &opts.loader_options(), print_diag);
                    match ret {
                        Ok(preproc) => preproc,
                        Err(err) => { warn!("{}", err); return None; }
                    }
                }
            };
        let keymap = match read_keymap(&keyspec, os::getenv) {
            Ok(map) => map,
            Err(err) => die!("{}", err)
        };
        Some(LoadingScene::new(self.screen.clone(), bms, infos,
                               keyspec, keymap, self.opts.clone()) as Box<Scene+'static>)
    }
}

impl Scene for SelectingScene {
    fn activate(&mut self) -> SceneCommand {
        *self.keepgoing.write() = true;
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
                        let offset = self.offset - 1; // XXX #6268
                        self.update_offset(offset);
                    }
                }
                event::KeyEvent(event::DownKey,true,_,_) => {
                    if self.offset + 1 < self.files.len() {
                        let offset = self.offset + 1; // XXX #6268
                        self.update_offset(offset);
                    }
                }
                event::KeyEvent(event::PageUpKey,true,_,_) => {
                    if self.offset > (NUMENTRIES-1) - 1 {
                        let offset = self.offset - (NUMENTRIES-1); // XXX #6268
                        self.update_offset(offset);
                    } else if self.offset > 0 {
                        self.update_offset(0);
                    }
                }
                event::KeyEvent(event::PageDownKey,true,_,_) => {
                    let nfiles = self.files.len();
                    if self.offset + (NUMENTRIES-1) < nfiles {
                        let offset = self.offset + (NUMENTRIES-1); // XXX #6268
                        self.update_offset(offset);
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
            match self.receiver.try_recv() {
                Ok(PushFiles(paths)) => {
                    if self.files.is_empty() { // immediately preloads the first entry
                        self.preloaded = PreloadAfter(0);
                    }
                    for (path, hash) in paths.into_iter() {
                        let job = match hash {
                            Some(hash) => self.make_cached_preloading_task(&hash, &path),
                            None => self.make_preloading_task(&path),
                        };

                        let index = self.files.len();
                        self.fileindices.insert(path.clone(), index);
                        self.files.push(Entry { path: path, hash: hash, meta: None });
                        self.pool.execute(proc(_) job());
                    }
                }
                Ok(NoMoreFiles) => {
                    self.filesdone = true;
                }
                Ok(BmsFailed(bmspath, err)) => {
                    if !self.is_current(&bmspath) { continue; }
                    self.preloaded = PreloadFailed(err);
                }
                Ok(BmsHashRead(bmspath, hash)) => {
                    match self.fileindices.find(&bmspath) {
                        Some(&offset) => { self.files[mut][offset].hash = Some(hash); }
                        None => {}
                    }
                }
                Ok(BmsCacheLoaded(bmspath, meta)) => {
                    match self.fileindices.find(&bmspath) {
                        Some(&offset) => { self.files[mut][offset].meta = Some(meta); }
                        None => {}
                    }
                }
                Ok(BmsLoaded(bmspath, preproc, messages)) => {
                    if !self.is_current(&bmspath) { continue; }
                    self.preloaded = Preloaded(PreloadedData::new(preproc, messages));
                }
                Ok(BmsBannerLoaded(bmspath, imgpath, prepared)) => {
                    if !self.is_current(&bmspath) { continue; }
                    match self.preloaded {
                        Preloaded(ref mut data) if data.banner.is_none() => {
                            let prepared = prepared.unwrap();
                            match Texture2D::from_prepared_surface(&prepared, false, false) {
                                Ok(tex) => { data.banner = Some(Rc::new(tex)); }
                                Err(_err) => {
                                    warn!("failed to load image #BANNER ({})", imgpath);
                                }
                            }
                        }
                        _ => {}
                    }
                }
                Err(_) => { break; }
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
        let mut screen = self.screen.borrow_mut();

        screen.clear();
        self.skin.borrow_mut().render(screen.deref_mut(), self);
        screen.swap_buffers();
    }

    fn deactivate(&mut self) {
        event::enable_key_repeat(event::CustomRepeatDelay(0), event::DefaultRepeatInterval);
        *self.keepgoing.write() = false;
    }

    fn consume(self) -> Box<Scene+'static> { fail!("unreachable"); }
}

define_hooks! {
    for PreprocessedBms |preproc, id, parent, body| {
        delegate preproc.bms;
        delegate preproc.infos;
        delegate preproc.keyspec;
    }

    for (Option<uint>, bms::diag::BmsMessage) |msg, id, parent, body| {
        scalar "msg.line" => return msg.val0().map(|v| v.into_scalar());
        scalar "msg.text" => msg.ref1().message.into_scalar();

        block "msg" => body(parent, msg.ref1().id);
        block "msg.line" => msg.val0().is_some() && body(parent, "");
        block "msg.severity" => match msg.ref1().severity {
            bms::diag::Note    => { body(parent, "note"); }
            bms::diag::Warning => { body(parent, "warning"); }
            bms::diag::Fatal   => { body(parent, "fatal"); }
        };
    }

    for PreloadedData |data, id, parent, body| {
        delegate data.preproc;

        scalar "meta.duration" => data.duration.into_scalar();
        scalar "meta.banner" => return data.banner.as_ref().map(|tex| tex.as_scalar());

        block "messages" => data.messages.iter().all(|msg| body(&parent.delegate(msg), ""));
        block "meta.banner" => data.banner.is_some() && body(parent, "");
    }

    for SelectingScene |scene, id, parent, body| {
        delegate scene.opts;

        scalar "entries.scrollstart" => {
            if scene.files.is_empty() {
                0.0f32.into_scalar()
            } else {
                let top = cmp::min(scene.scrolloffset, scene.files.len());
                (top as f32 / scene.files.len() as f32).into_scalar()
            }
        };
        scalar "entries.scrollend" => {
            if scene.files.is_empty() {
                1.0f32.into_scalar()
            } else {
                let bottom = cmp::min(scene.scrolloffset + NUMENTRIES, scene.files.len());
                (bottom as f32 / scene.files.len() as f32).into_scalar()
            }
        };

        block "scanning" => scene.filesdone || body(parent, "");
        block "entries" => {
            let top = cmp::min(scene.scrolloffset, scene.files.len());
            scene.files[top..].iter().enumerate().all(|(i, entry)| {
                let inverted = scene.scrolloffset + i == scene.offset;
                body(&(scene, inverted, entry), "")
            });
        };
        block "entries.before" => {
            let top = cmp::min(scene.scrolloffset, scene.files.len());
            scene.files[..top].iter().rev().all(|entry| {
                body(&(scene, false, entry), "")
            });
        };
        block "preload" => match scene.preloaded {
            NothingToPreload | PreloadAfter(..) => {}
            Preloading => { body(parent, "loading"); }
            Preloaded(ref data) => { body(&parent.delegate(data), "loaded"); }
            PreloadFailed(ref err) => {
                body(&parent.add_text("preload.error", err[]), "failed");
            }
        };
    }
}

impl<'a> Hook for (&'a SelectingScene, bool, &'a Entry) {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        let (scene, _inverted, entry) = *self;
        match id {
            "entry.path" => {
                let path = entry.path.path_relative_from(&scene.root)
                                     .unwrap_or_else(|| entry.path.clone());
                Some(path.display().to_string().into_scalar())
            },
            "entry.hash" => entry.hash.map(|h| h.to_string().into_scalar()),
            _ => entry.meta.as_ref().and_then(|meta| meta.scalar_hook(id))
                      .or_else(|| scene.scalar_hook(id))
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, mut body: |&Hook, &str| -> bool) -> bool {
        let (scene, inverted, entry) = *self;
        match id {
            "entry.hash" => { entry.hash.is_some() && body(parent, ""); }
            "entry.meta" => { entry.meta.is_some() && body(parent, ""); }
            "entry.inverted" => { inverted && body(parent, ""); }
            _ => { return entry.meta.run_block_hook(id, parent, &mut body) ||
                          scene.run_block_hook(id, parent, &mut body); }
        }
        true
    }
}

