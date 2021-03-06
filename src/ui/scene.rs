// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Scene management.

use std::io::timer::sleep;
use std::time::Duration;
use sdl::get_ticks;
use ui::common::Ticker;

/// Options used by the scene to customize the scene loop.
#[deriving(Clone)]
pub struct SceneOptions {
    /// If specified, limits the number of `Scene::tick` calls per second to this value.
    /// `run_scene` ensures this limitation by sleeping after each tick as needed.
    pub tpslimit: Option<uint>,
    /// If specified, limits the number of `Scene::render` calls per second to this value.
    /// Due to the implementation strategy `tpslimit` takes precedence over this if specified.
    pub fpslimit: Option<uint>,
}

impl SceneOptions {
    /// Creates default options for the scene.
    pub fn new() -> SceneOptions {
        SceneOptions { tpslimit: None, fpslimit: None }
    }

    /// Replaces `tpslimit` field with given value.
    pub fn tpslimit(self, tps: uint) -> SceneOptions {
        SceneOptions { tpslimit: Some(tps), ..self }
    }

    /// Replaces `fpslimit` field with given value.
    pub fn fpslimit(self, fps: uint) -> SceneOptions {
        SceneOptions { fpslimit: Some(fps), ..self }
    }
}

/// A command returned by `Scene`'s `tick` method.
pub enum SceneCommand {
    /// Continues displaying this scene.
    Continue,
    /// Pushes a new `Scene` to the scene stack, making it the active scene. The current scene is
    /// stopped (after calling `deactivate`) until the new scene returns `PopScene` command.
    Push(Box<Scene+'static>),
    /// Replaces the current scene with a new `Scene` that will be returned by `consume` method.
    /// The command itself does not have a `Scene` argument since new scene may have to be
    /// constructured out of the existing scene. Therefore the scene should be prepared for
    /// multiple next scenes possible if any.
    Replace,
    /// Pops the current scene from the scene stack. The program exits if it was the only remaining
    /// scene in the stack.
    Pop,
    /// Clears the scene stack, effectively ending the program.
    Exit,
}

/// Scene interface.
pub trait Scene {
    /// Called when the scene is to be activated, prior to the first `tick` call. May return
    /// a non-`Continue` command to immediately deactivate the scene.
    fn activate(&mut self) -> SceneCommand;

    /// Returns the options for this scene. It is called *after* the `activate` call.
    fn scene_options(&self) -> SceneOptions;

    /// Does the event handling and internal logics, and returns a command to instruct the caller.
    fn tick(&mut self) -> SceneCommand;

    /// Does the rendering jobs. It may get called once after the `tick` call (but not mandatory,
    /// for example, due to the frame drop).
    fn render(&self);

    /// Called when the scene is to be deactivated by the latest `tick` call. It is not called
    /// when `activate` returns a non-`Continue` command and the scene becomes deactivated.
    fn deactivate(&mut self);

    /// Called when the scene is to be replaced by a new `Scene` due to the `ReplaceScene` command.
    /// When called due to the `tick` call, this is called after `deactivate` call.
    fn consume(self: Box<Self>) -> Box<Scene+'static>;
}

/// Runs given scene and other additionally spawned scenes.
pub fn run_scene(scene: Box<Scene+'static>) {
    let mut current = scene;
    let mut stack = Vec::new();
    loop {
        let mut result = current.activate();
        match result {
            SceneCommand::Continue => {
                let opts = current.scene_options();
                let mintickdelay = opts.tpslimit.map_or(0, |tps| 1000 / tps);
                let interval = opts.fpslimit.map_or(0, |fps| 1000 / fps);
                let mut ticker = Ticker::with_interval(interval);
                loop {
                    let ticklimit = get_ticks() + mintickdelay;
                    result = current.tick();
                    match result {
                        SceneCommand::Continue => {
                            ticker.on_tick(get_ticks(), || { current.render(); });
                        }
                        _ => { break; }
                    }
                    let now = get_ticks();
                    if now < ticklimit { sleep(Duration::milliseconds((ticklimit - now) as i64)); }
                }
                current.deactivate();
            }
            _ => {}
        }
        match result {
            SceneCommand::Continue => {
                panic!("impossible");
            }
            SceneCommand::Push(newscene) => {
                stack.push(current);
                current = newscene;
            }
            SceneCommand::Replace => {
                current = current.consume();
            }
            SceneCommand::Pop => {
                if stack.is_empty() { break; }
                current = stack.pop().unwrap();
            }
            SceneCommand::Exit => {
                break;
            }
        }
    }
}

