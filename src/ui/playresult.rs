// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Play result screen. Only used when the graphical `PlayingScene` finishes.

use sdl::*;
use util::gfx::*;
use util::gl::ShadedDrawingTraits;
use util::bmfont::{LeftAligned, Centered};
use engine::player::Player;
use ui::screen::Screen;
use ui::init::{SCREENW, SCREENH};
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PopScene, Exit};
use ui::playing;

/// Play result scene.
pub struct PlayResultScene {
    /// Display screen.
    screen: @Screen,
    /// Game play state after playing.
    player: Player,
}

impl PlayResultScene {
    /// Creates a new play result scene from the game play state after `PlayingScene`.
    pub fn new(screen: @Screen, player: Player) -> ~PlayResultScene {
        ~PlayResultScene { screen: screen, player: player }
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
        self.screen.clear();

        do self.screen.draw_shaded_with_font |d| {
            let cleared = (self.player.gauge >= self.player.survival);
            d.rect(0.0, 20.0, SCREENW as f32, 120.0, RGB(0xff,0xff,0xff));
            d.string(SCREENW as f32 * 0.5, 38.0, 4.0, Centered,
                     if cleared {"CLEARED!"} else {"FAILED..."}, RGB(0,0,0));

            let print_number = |x: f32, y: f32, zoom: f32, numstr: &str| {
                let mut zeroes = numstr.find(|c: char| c != '0').unwrap_or(numstr.len());
                if zeroes == numstr.len() || !numstr.char_at(zeroes).is_digit() {
                    zeroes -= 1; // keep at least one zero
                }
                if zeroes > 0 {
                    d.string(x, y, zoom, LeftAligned,
                             numstr.slice_to(zeroes), RGB(0x80,0x80,0x80));
                }
                d.string(x + zeroes as f32 * (8.0 * zoom), y, zoom, LeftAligned,
                         numstr.slice_from(zeroes), RGB(0xff,0xff,0xff));
            };

            static GRADEGAP: f32 = 25.0;
            static GRADEWIDTH: f32 = (SCREENW as f32 - GRADEGAP * 6.0) / 5.0 + GRADEGAP;
            for i in range(0, 5) {
                let i = i as uint;
                let (name, color) = playing::GRADES[4-i];
                let count = self.player.gradecounts[4-i];
                let x = GRADEGAP + i as f32 * GRADEWIDTH;
                d.string(x, 160.0, 2.0, LeftAligned, name, color);
                print_number(x, 195.0, 3.0, format!("{:04}", count));
            }

            d.string(GRADEGAP, 270.0, 2.0, LeftAligned, "SCORE", RGB(0xff,0xff,0xff));
            print_number(GRADEGAP, 305.0, 3.0, format!("{:07}/", self.player.score));
            print_number(GRADEGAP + 8.0 * 24.0, 305.0, 3.0,
                         format!("{:07}", self.player.infos.maxscore));

            static COMBOLEFT: f32 = GRADEGAP + 3.0 * GRADEWIDTH;
            d.string(COMBOLEFT, 270.0, 2.0, LeftAligned, "MAX COMBO", RGB(0xff,0xff,0xff));
            print_number(COMBOLEFT, 305.0, 3.0, format!("{:04}/", self.player.bestcombo));
            print_number(COMBOLEFT + 5.0 * 24.0, 305.0, 3.0,
                         format!("{:04}", self.player.infos.nnotes));

            d.string(SCREENW as f32 / 2.0, SCREENH as f32 - 40.0, 1.0, Centered,
                     "Press Return key to continue.", RGB(0xff,0xff,0xff));
        }

        self.screen.swap_buffers();
    }

    fn deactivate(&mut self) {}

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}

