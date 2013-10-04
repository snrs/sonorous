// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Play result screen.

use engine::player::Player;
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PopScene};

/// Play result scene. For now it is a placeholder.
struct PlayResultScene {
    player: Player
}

impl PlayResultScene {
    /// Creates a new play result scene from the game play state after `PlayingScene`.
    pub fn new(player: Player) -> ~PlayResultScene {
        ~PlayResultScene { player: player }
    }
}

impl Scene for PlayResultScene {
    fn activate(&mut self) -> SceneCommand { Continue }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new() }

    fn tick(&mut self) -> SceneCommand {
        // print the message and immediately terminates
        if self.player.gauge >= self.player.survival {
            println(format!("*** CLEARED! ***\n\
                             COOL  {:4}    GREAT {:4}    GOOD  {:4}\n\
                             BAD   {:4}    MISS  {:4}    MAX COMBO {}\n\
                             SCORE {:07} (max {:07})",
                            self.player.gradecounts[4], self.player.gradecounts[3],
                            self.player.gradecounts[2], self.player.gradecounts[1],
                            self.player.gradecounts[0], self.player.bestcombo,
                            self.player.score, self.player.infos.maxscore));
        } else {
            println("YOU FAILED!");
        }

        PopScene
    }

    fn render(&self) { fail!("unreachable"); }

    fn deactivate(&mut self) {}

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}

