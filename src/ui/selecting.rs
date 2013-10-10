// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Song and pattern selection screen.

use std::{cmp, os, comm, task};
use std::rand::{rng, Rng};
use extra::arc::RWArc;

use sdl::*;
use format::timeline::TimelineInfo;
use format::bms;
use format::bms::Bms;
use util::gfx::*;
use util::gl::ShadedDrawingTraits;
use util::bmfont::{LeftAligned, RightAligned};
use util::filesearch::SearchContext;
use engine::keyspec::{KeySpec, key_spec};
use engine::player::apply_modf;
use ui::screen::Screen;
use ui::init::{SCREENW, SCREENH};
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PopScene, Exit};
use ui::options::Options;

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

enum Message {
    PushFiles(~[Path]),
    NoMoreFiles,
    BmsLoaded(Path,Result<~PreprocessedBms,~str>),
}

enum PreloadingState {
    NeverPreloaded,
    PreloadAfter(uint),
    Preloading(comm::Port<task::TaskResult>),
    Preloaded(Result<~PreprocessedBms,~str>),
}

pub struct SelectingScene {
    screen: @Screen,
    opts: @Options,
    root: Path,
    files: ~[Path],
    filesdone: bool,
    scrolloffset: uint,
    offset: uint,
    preloaded: PreloadingState,
    port: comm::Port<Message>,
    chan: comm::SharedChan<Message>,
    keepgoing: RWArc<bool>,
}

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

fn worker_task(name: ~str) -> task::TaskBuilder {
    let mut builder = task::task();
    builder.name(name);
    // XXX why does SDL ignores events when using the default scheduler?!
    builder.sched_mode(task::SingleThreaded);
    builder.supervised();
    builder
}

static NUMENTRIES: uint = 15;
static PRELOAD_DELAY: uint = 300;

impl SelectingScene {
    pub fn new(screen: @Screen, root: &Path, opts: @Options) -> ~SelectingScene {
        let root = os::make_absolute(root);
        let (port, chan) = comm::stream();
        let chan = comm::SharedChan::new(chan);
        ~SelectingScene {
            screen: screen, opts: opts, root: root,
            files: ~[], filesdone: false, scrolloffset: 0, offset: 0, preloaded: NeverPreloaded,
            port: port, chan: chan, keepgoing: RWArc::new(true),
        }
    }

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
        self.preloaded = NeverPreloaded;
        self.spawn_scanning_task();
    }

    pub fn spawn_preloading_task(&self, path: &Path) -> comm::Port<task::TaskResult> {
        let path = path.clone();
        let opts = (*self.opts).clone();
        let chan = self.chan.clone();
        let mut future = None;
        let mut builder = worker_task(~"preloader");
        do builder.future_result |f| { future = Some(f); }
        do builder.spawn_with(opts) |opts| {
            let mut r = rng();
            let loaderopts = bms::load::BmsLoaderOptions::new();
            let mut callback = bms::diag::IgnoringMessageListener;
            let result = preprocess_bms(&path, @opts, &mut r, &loaderopts, &mut callback);
            chan.send(BmsLoaded(path.clone(), result));
        }
        future.unwrap()
    }

    pub fn update_offset(&mut self, offset: uint) {
        self.offset = offset;
        if offset < self.files.len() {
            self.preloaded = PreloadAfter(get_ticks() + PRELOAD_DELAY);
        } else {
            self.preloaded = NeverPreloaded;
        }

        if self.scrolloffset > offset {
            self.scrolloffset = offset;
        } else if self.scrolloffset + (NUMENTRIES-1) < offset { 
            self.scrolloffset = offset - (NUMENTRIES-1);
        }
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
                event::KeyEvent(event::EscapeKey,_,_,_) => { return PopScene; }
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
                BmsLoaded(bmspath,preloaded) => {
                    // the loading jobs may lag, we need to verify the data is indeed current
                    if self.offset < self.files.len() && bmspath == self.files[self.offset] {
                        self.preloaded = Preloaded(preloaded);
                    }
                }
            }
        }

        if self.offset < self.files.len() {
            let newpreloaded = match self.preloaded {
                PreloadAfter(timeout) if timeout < get_ticks() => {
                    // preload the current entry after some delay
                    let future = self.spawn_preloading_task(&self.files[self.offset]);
                    Some(Preloading(future))
                },
                Preloading(ref port) if port.peek() && port.recv() == task::Failure => {
                    // port.recv() may also return `task::Success` when the message is sent
                    // but being delayed. in this case we can safely ignore it.
                    Some(Preloaded(Err(~"unexpected failure")))
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

        let root = self.root.push("_"); // for the calculation of `Path::get_relative_to`
        do self.screen.draw_shaded_with_font |d| {
            let top = cmp::min(self.scrolloffset, self.files.len());
            let bottom = cmp::min(self.scrolloffset + NUMENTRIES, self.files.len());
            let windowedfiles = self.files.slice(top, bottom);

            static META_TOP: f32 = NUMENTRIES as f32 * 20.0;

            let mut y = 0.0;
            for (i, path) in windowedfiles.iter().enumerate() {
                let path = root.get_relative_to(&path.push("_"));
                if self.scrolloffset + i == self.offset { // inverted
                    d.rect(10.0, y, SCREENW as f32, y + 20.0, RGB(0xff,0xff,0xff));
                    d.string(12.0, y + 2.0, 1.0, LeftAligned, path.to_str(), RGB(0,0,0));
                } else {
                    d.string(12.0, y + 2.0, 1.0, LeftAligned, path.to_str(), RGB(0xff,0xff,0xff));
                }
                y += 20.0;
            }
            let pixelsperslot = (META_TOP - 4.0) / cmp::max(NUMENTRIES, self.files.len()) as f32;
            d.rect(2.0, 2.0 + top as f32 * pixelsperslot,
                   7.0, 2.0 + (top + NUMENTRIES) as f32 * pixelsperslot, RGB(0xc0,0xc0,0xc0));
            d.rect(0.0, META_TOP + 1.0, SCREENW as f32, META_TOP + 2.0, RGB(0xff,0xff,0xff));

            match self.preloaded {
                NeverPreloaded | PreloadAfter(*) => {}
                Preloading(*) => {
                    d.string(2.0, META_TOP + 4.0, 1.0, LeftAligned,
                             "loading...", RGB(0xc0,0xc0,0xc0));
                }
                Preloaded(Ok(ref preloaded)) => {
                    let title = preloaded.bms.meta.title.map_default("", |s| s.as_slice());
                    let genre = preloaded.bms.meta.genre.map_default("", |s| s.as_slice());
                    let artist = preloaded.bms.meta.artist.map_default("", |s| s.as_slice());
                    d.string(2.0, META_TOP + 4.0, 2.0, LeftAligned, title, RGB(0xff,0xff,0xff));
                    d.string(2.0, META_TOP + 40.0, 1.0, LeftAligned, artist, RGB(0xff,0xff,0xff));
                    d.string(SCREENW as f32 - 2.0, META_TOP + 40.0, 1.0, RightAligned,
                             genre, RGB(0xff,0xff,0xff));
                }
                Preloaded(Err(ref err)) => {
                    d.string(2.0, META_TOP + 4.0, 1.0, LeftAligned,
                             format!("error: {}", *err), RGB(0xc0,0xc0,0xc0));
                }
            }

            d.rect(0.0, SCREENH as f32 - 21.0,
                   SCREENW as f32, SCREENH as f32 - 20.0, RGB(0xff,0xff,0xff));
            d.string(2.0, SCREENH as f32 - 18.0, 1.0, LeftAligned,
                     format!("Up/Down/PgUp/PgDown/Home/End: Select   \
                              Enter: {}   F5: Refresh   Esc: Quit",
                             if self.opts.is_autoplay() {"Autoplay"} else {"Play"}),
                     RGB(0xff,0xff,0xff));
        }

        self.screen.swap_buffers();
    }

    fn deactivate(&mut self) {
        event::enable_key_repeat(event::CustomRepeatDelay(0), event::DefaultRepeatInterval);
        do self.keepgoing.write |v| { *v = false; }
    }

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}

