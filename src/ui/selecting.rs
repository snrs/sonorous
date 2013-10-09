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
use util::bmfont::LeftAligned;
use util::filesearch::SearchContext;
use engine::keyspec::{KeySpec, key_spec};
use engine::player::apply_modf;
use ui::screen::Screen;
use ui::init::{SCREENW};
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
                                bmspath: &Path, opts: &Options, r: &mut R,
                                loaderopts: &bms::load::BmsLoaderOptions,
                                callback: &mut Listener) -> Result<PreprocessedBms,~str> {
    let mut bms = earlyexit!(bms::load::load_bms(bmspath, r, loaderopts, callback));
    let keyspec = earlyexit!(key_spec(&bms, opts.preset.clone(),
                                      opts.leftkeys.clone(), opts.rightkeys.clone()));
    keyspec.filter_timeline(&mut bms.timeline);
    let infos = bms.timeline.analyze();
    for &modf in opts.modf.iter() {
        apply_modf(&mut bms, modf, r, keyspec);
    }
    Ok(PreprocessedBms { bms: bms, infos: infos, keyspec: keyspec })
}

enum Message {
    PushFiles(~[Path]),
    NoMoreFiles,
    BmsMessageCallback(Option<uint>,bms::diag::BmsMessage),
    BmsLoaded(Path,Result<PreprocessedBms,~str>),
}

pub struct SelectingScene {
    screen: Screen,
    opts: ~Options,
    root: Path,
    files: ~[Path],
    filesdone: bool,
    scrolloffset: uint,
    offset: uint,
    preloaded: Option<Result<PreprocessedBms,~str>>,
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

// XXX why does SDL ignores events when using the default scheduler?!
fn single_threaded_task() -> task::TaskBuilder {
    let mut builder = task::task();
    builder.sched_mode(task::SingleThreaded);
    builder
}

static NUMENTRIES: uint = 15;

impl SelectingScene {
    pub fn new(screen: Screen, root: &Path, opts: ~Options) -> ~SelectingScene {
        let root = os::make_absolute(root);
        let (port, chan) = comm::stream();
        let chan = comm::SharedChan::new(chan);
        ~SelectingScene {
            screen: screen, opts: opts, root: root,
            files: ~[], filesdone: false, scrolloffset: 0, offset: 0, preloaded: None,
            port: port, chan: chan, keepgoing: RWArc::new(true),
        }
    }

    pub fn spawn_scanning_task(&self) {
        let root = self.root.clone();
        let chan = self.chan.clone();
        let keepgoing = self.keepgoing.clone();
        do single_threaded_task().spawn {
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

    pub fn spawn_preloading_task(&self, path: &Path) {
        let path = path.clone();
        let opts = self.opts.clone();
        let chan = self.chan.clone();
        do single_threaded_task().spawn {
            let mut r = rng();
            let loaderopts = bms::load::BmsLoaderOptions::new();
            let mut callback = |line: Option<uint>, msg: bms::diag::BmsMessage| {
                chan.send(BmsMessageCallback(line, msg));
            };
            let result = preprocess_bms(&path, opts, &mut r, &loaderopts, &mut callback);
            chan.send(BmsLoaded(path.clone(), result));
        }
    }

    pub fn update_offset(&mut self, offset: uint) {
        self.offset = offset;
        if offset < self.files.len() {
            self.preloaded = None;
            self.spawn_preloading_task(&self.files[offset]);
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
        self.files.clear();
        self.filesdone = false;
        self.spawn_scanning_task();
        event::enable_key_repeat(event::DefaultRepeatDelay, event::DefaultRepeatInterval);
        Continue
    }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new().fpslimit(20) }

    fn tick(&mut self) -> SceneCommand {
        loop {
            match event::poll_event() {
                event::KeyEvent(event::EscapeKey,_,_,_) => { return PopScene; }
                event::QuitEvent => { return Exit; }
                event::NoEvent => { break; },

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
                    }
                }
                event::KeyEvent(event::PageDownKey,true,_,_) => {
                    if self.offset + (NUMENTRIES-1) < self.files.len() {
                        self.update_offset(self.offset + (NUMENTRIES-1));
                    }
                }

                _ => {}
            }
        }

        while self.port.peek() {
            match self.port.recv() {
                PushFiles(paths) => {
                    self.files.push_all_move(paths);
                }
                NoMoreFiles => {
                    self.filesdone = true;
                }
                BmsMessageCallback(line,msg) => {
                    let atline = match line {
                        Some(line) => format!(" at line {}", line),
                        None => ~"",
                    };
                    warn!("[{}{}] {}", msg.severity().to_str(), atline, msg.to_str());
                }
                BmsLoaded(bmspath,preloaded) => {
                    // the loading jobs may lag, we need to verify the data is indeed current
                    if self.offset < self.files.len() && bmspath == self.files[self.offset] {
                        self.preloaded = Some(preloaded);
                    }
                }
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

            let mut y = 0.0;
            for (i, path) in windowedfiles.iter().enumerate() {
                let path = root.get_relative_to(&path.push("_"));
                if self.scrolloffset + i == self.offset { // inverted
                    d.rect(0.0, y, SCREENW as f32, y + 20.0, RGB(0xff,0xff,0xff));
                    d.string(2.0, y + 2.0, 1.0, LeftAligned, path.to_str(), RGB(0,0,0));
                } else {
                    d.string(2.0, y + 2.0, 1.0, LeftAligned, path.to_str(), RGB(0xff,0xff,0xff));
                }
                y += 20.0;
            }

            d.string(2.0, 302.0, 1.0, LeftAligned,
                     format!("(scanned {} files{})", self.files.len(),
                             if self.filesdone {""} else {" and counting"}),
                     RGB(0x80,0x80,0x80));

            if !self.files.is_empty() {
                match self.preloaded {
                    None => {
                        d.string(2.0, 330.0, 1.0, LeftAligned,
                                 "preloading BMS", RGB(0xc0,0xc0,0xc0));
                    }
                    Some(Ok(ref preloaded)) => {
                        d.string(2.0, 330.0, 1.0, LeftAligned,
                                 format!("preloaded, title: {:?}", preloaded.bms.meta.title),
                                 RGB(0xc0,0xc0,0xc0));
                    }
                    Some(Err(ref err)) => {
                        d.string(2.0, 330.0, 1.0, LeftAligned,
                                 format!("preloading error: {}", *err),
                                 RGB(0xc0,0xc0,0xc0));
                    }
                }
            }
        }

        self.screen.swap_buffers();
    }

    fn deactivate(&mut self) {
        event::enable_key_repeat(event::CustomRepeatDelay(0), event::DefaultRepeatInterval);
        do self.keepgoing.write |v| { *v = false; }
    }

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}

