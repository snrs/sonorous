// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Scene management.

use sdl::get_ticks;
use ui::common::Ticker;

/// Options used by the scene to customize the scene loop.
#[deriving(Clone)]
pub struct SceneOptions {
    /// If specified, limits the number of `Scene::render` calls per second to this value.
    fpslimit: Option<uint>,
}

impl SceneOptions {
    /// Creates default options for the scene.
    pub fn new() -> SceneOptions {
        SceneOptions { fpslimit: None }
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
    PushScene(~Scene:),
    /// Replaces the current scene with a new `Scene` that will be returned by `consume` method.
    /// The command itself does not have a `Scene` argument since new scene may have to be
    /// constructured out of the existing scene. Therefore the scene should be prepared for
    /// multiple next scenes possible if any.
    ReplaceScene,
    /// Pops the current scene from the scene stack. The program exits if it was the only remaining
    /// scene in the stack.
    PopScene,
    /// Clears the scene stack, effectively ending the program.
    Exit,
}

/// Scene interface.
pub trait Scene {
    /// Called when the scene is to be activated, prior to the first `tick` call. May return
    /// a non-`Continue` command to immediately deactivate the scene.
    pub fn activate(&mut self) -> SceneCommand;

    /// Returns the options for this scene. It is called *after* the `activate` call.
    pub fn scene_options(&self) -> SceneOptions;

    /// Does the event handling and internal logics, and returns a command to instruct the caller.
    pub fn tick(&mut self) -> SceneCommand;

    /// Does the rendering jobs. It may get called once after the `tick` call (but not mandatory,
    /// for example, due to the frame drop).
    pub fn render(&self);

    /// Called when the scene is to be deactivated by the latest `tick` call. It is not called
    /// when `activate` returns a non-`Continue` command and the scene becomes deactivated.
    pub fn deactivate(&mut self);

    /// Called when the scene is to be replaced by a new `Scene` due to the `ReplaceScene` command.
    /// When called due to the `tick` call, this is called after `deactivate` call.
    /// And yes, this has to be `~Scene:` without no `Send` kind.
    pub fn consume(~self) -> ~Scene:;
}

/// Runs given scene and other additionally spawned scenes.
pub fn run_scene(scene: ~Scene:) {
    let mut current = scene;
    let mut stack = ~[];
    loop {
        let mut result = current.activate();
        match result {
            Continue => {
                let opts = current.scene_options();
                let interval = opts.fpslimit.map_default(0, |&fps| 1000 / fps);
                let mut ticker = Ticker::with_interval(interval);
                loop {
                    result = current.tick();
                    match result {
                        Continue => { do ticker.on_tick(get_ticks()) { current.render(); } }
                        _ => { break; }
                    }
                }
                current.deactivate();
            }
            _ => {}
        }
        match result {
            Continue => {
                fail!("impossible");
            }
            PushScene(newscene) => {
                stack.push(current);
                current = newscene;
            }
            ReplaceScene => {
                current = current.consume();
            }
            PopScene => {
                if stack.is_empty() { break; }
                current = stack.pop();
            }
            Exit => {
                break;
            }
        }
    }
}

