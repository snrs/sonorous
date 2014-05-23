// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Play result screen. Only used when the graphical `PlayingScene` finishes.

use std::rc::Rc;
use std::cell::RefCell;

use sdl::event;
use gfx::color::{Color, RGB};
use gfx::draw::ShadedDrawingTraits;
use gfx::bmfont::{LeftAligned, Centered};
use gfx::screen::Screen;
use gfx::skin::ShadedFontDrawingAdditions;
use engine::player::Player;
use ui::init::{SCREENW, SCREENH};
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PopScene, Exit};
use ui::playing;

/// Play result scene.
pub struct PlayResultScene {
    /// Display screen.
    pub screen: Rc<RefCell<Screen>>,
    /// Game play state after playing.
    pub player: Player,
}

impl PlayResultScene {
    /// Creates a new play result scene from the game play state after `PlayingScene`.
    pub fn new(screen: Rc<RefCell<Screen>>, player: Player) -> Box<PlayResultScene> {
        box PlayResultScene { screen: screen, player: player }
    }
}

impl Scene for PlayResultScene {
    fn activate(&mut self) -> SceneCommand { Continue }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new().tpslimit(20).fpslimit(1) }

    fn tick(&mut self) -> SceneCommand {
        loop {
            match event::poll_event() {
                event::KeyEvent(event::EscapeKey,true,_,_) |
                event::KeyEvent(event::ReturnKey,true,_,_) => { return PopScene; }
                event::QuitEvent => { return Exit; }
                event::NoEvent => { break; }
                _ => {}
            }
        }
        Continue
    }

    fn render(&self) {
        let mut screen = self.screen.borrow_mut();

        screen.clear();

        screen.draw_shaded_with_font(|d| {
            static WHITE: Color = RGB(0xff,0xff,0xff);
            static GRAY:  Color = RGB(0x80,0x80,0x80);

            let cleared = self.player.gauge >= self.player.survival;
            d.rect(0.0, 20.0, SCREENW as f32, 120.0, WHITE);
            d.string(SCREENW as f32 * 0.5, 38.0, 4.0, Centered,
                     if cleared {"CLEARED!"} else {"FAILED..."}, RGB(0,0,0));

            static GRADEGAP: f32 = 25.0;
            static GRADEWIDTH: f32 = (SCREENW as f32 - GRADEGAP * 6.0) / 5.0 + GRADEGAP;
            for i in range(0, 5) {
                let i = i as uint;
                let (name, color) = playing::GRADES[4-i];
                let count = self.player.gradecounts[4-i];
                let x = GRADEGAP + i as f32 * GRADEWIDTH;
                d.string(x, 160.0, 2.0, LeftAligned, name, color);
                d.numeral(x, 195.0, 3.0, LeftAligned,
                          format!("{:04}", count).as_slice(), WHITE, GRAY);
            }

            d.string(GRADEGAP, 270.0, 2.0, LeftAligned, "SCORE", WHITE);
            d.numeral(GRADEGAP, 305.0, 3.0, LeftAligned,
                      format!("{:07}/", self.player.score).as_slice(), WHITE, GRAY);
            d.numeral(GRADEGAP + 8.0 * 24.0, 305.0, 3.0, LeftAligned,
                      format!("{:07}", self.player.infos.maxscore).as_slice(), WHITE, GRAY);

            static COMBOLEFT: f32 = GRADEGAP + 3.0 * GRADEWIDTH;
            d.string(COMBOLEFT, 270.0, 2.0, LeftAligned, "MAX COMBO", WHITE);
            d.numeral(COMBOLEFT, 305.0, 3.0, LeftAligned,
                      format!("{:04}/", self.player.bestcombo).as_slice(), WHITE, GRAY);
            d.numeral(COMBOLEFT + 5.0 * 24.0, 305.0, 3.0, LeftAligned,
                      format!("{:04}", self.player.infos.nnotes).as_slice(), WHITE, GRAY);

            d.string(SCREENW as f32 / 2.0, SCREENH as f32 - 40.0, 1.0, Centered,
                     "Press Return key to continue.", WHITE);
        });

        screen.swap_buffers();
    }

    fn deactivate(&mut self) {}

    fn consume(~self) -> Box<Scene>: { fail!("unreachable"); }
}

