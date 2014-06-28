// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Common skin hooks for various types.

use format::{timeline, metadata, bms};
use engine::{keyspec, player};
use ui::options;

use gfx::skin::scalar::{Scalar, AsScalar, IntoScalar};
use gfx::skin::hook::Hook;

impl Hook for options::Options {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "opts.playspeed" => Some(self.playspeed.into_scalar()),
            _ => None,
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        match id {
            "opts.autoplay" => { self.is_autoplay() && body(parent, ""); }
            "opts.modifier" => match self.modf {
                Some(options::MirrorModf)    => { body(parent, "mirror"); }
                Some(options::ShuffleModf)   => { body(parent, "shuffle"); }
                Some(options::ShuffleExModf) => { body(parent, "shuffle-ex"); }
                Some(options::RandomModf)    => { body(parent, "random"); }
                Some(options::RandomExModf)  => { body(parent, "random-ex"); }
                None => {}
            },
            "opts.hasbga" => { self.has_bga() && body(parent, ""); }
            "opts.hasmovie" => { self.has_movie() && body(parent, ""); }
            "opts.showinfo" => { self.showinfo && body(parent, ""); }
            "opts.fullscreen" => { self.fullscreen && body(parent, ""); }
            _ => { return false; }
        }
        true
    }
}

impl<S,I> Hook for timeline::Timeline<S,I> {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "timeline.initbpm" => Some(self.initbpm.into_scalar()),
            _ => None,
        }
    }
}

impl Hook for timeline::TimelineInfo {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "timeline.#notes" => Some(self.nnotes.into_scalar()),
            "timeline.maxscore" => Some(self.maxscore.into_scalar()),
            _ => None,
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        match id {
            "timeline.bpmchange" => { self.hasbpmchange && body(parent, ""); }
            "timeline.longnote" => { self.haslongnote && body(parent, ""); }
            _ => { return false; }
        }
        true
    }
}

impl Hook for metadata::Meta {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "meta.title" => self.title.as_ref().map(|s| s.as_scalar()),
            "meta.genre" => self.genre.as_ref().map(|s| s.as_scalar()),
            "meta.artist" => self.artist.as_ref().map(|s| s.as_scalar()),
            "meta.level" => self.level.as_ref().map(|lv| lv.value.into_scalar()),
            "meta.difficulty" => match self.difficulty {
                Some(metadata::Difficulty(diff)) => Some(diff.into_scalar()),
                None => None,
            },
            _ => None,
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        match id {
            "meta.title" => { self.title.is_some() && body(parent, ""); }
            "meta.subtitle" => {
                self.subtitles.iter().advance(|s|
                    body(&parent.add_text("meta.subtitle", s.as_slice()), ""));
            }
            "meta.genre" => { self.genre.is_some() && body(parent, ""); }
            "meta.artist" => { self.artist.is_some() && body(parent, ""); }
            "meta.subartist" => {
                self.subartists.iter().advance(|s|
                    body(&parent.add_text("meta.subartist", s.as_slice()), ""));
            }
            "meta.comment" => {
                self.comments.iter().advance(|s|
                    body(&parent.add_text("meta.comment", s.as_slice()), ""));
            }
            "meta.level" => { self.level.is_some() && body(parent, ""); }
            "meta.levelsystem" => match self.level.as_ref().map(|lv| lv.system) {
                Some(metadata::LevelSystemBms) => { body(parent, "bms"); }
                None => {}
            },
            "meta.difficulty" => match self.difficulty {
                Some(metadata::Difficulty(1)) => { body(parent, "beginner"); }
                Some(metadata::Difficulty(2)) => { body(parent, "normal"); }
                Some(metadata::Difficulty(3)) => { body(parent, "hard"); }
                Some(metadata::Difficulty(4)) => { body(parent, "extra"); }
                Some(metadata::Difficulty(5)) => { body(parent, "insane"); }
                Some(metadata::Difficulty(_)) => { body(parent, "???"); }
                None => {}
            },
            _ => { return false; }
        }
        true
    }
}

impl Hook for bms::BmsMeta {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "meta.encoding" => Some(self.encoding.val0().into_scalar()),
            "meta.encoding.confidence" => {
                let mut confidence = self.encoding.val1();
                if confidence > 1.0 { confidence = 1.0; }
                Some(confidence.into_scalar())
            },
            _ => self.common.scalar_hook(id)
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, mut body: |&Hook, &str| -> bool) -> bool {
        match id {
            "meta.playmode" => match self.mode {
                bms::SinglePlay => { body(parent, "single"); }
                bms::CouplePlay => { body(parent, "couple"); }
                bms::DoublePlay => { body(parent, "double"); }
                bms::BattlePlay => { body(parent, "battle"); }
            },
            _ => { return self.common.run_block_hook(id, parent, &mut body); }
        }
        true
    }
}

impl Hook for bms::Bms {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        self.meta.scalar_hook(id)
            .or_else(|| self.timeline.scalar_hook(id))
    }

    fn block_hook(&self, id: &str, parent: &Hook, mut body: |&Hook, &str| -> bool) -> bool {
        self.meta.run_block_hook(id, parent, &mut body) ||
            self.timeline.run_block_hook(id, parent, &mut body)
    }
}

impl Hook for keyspec::KeySpec {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "meta.#keys" =>
                Some(self.nkeys().into_scalar()),
            _ => None,
        }
    }
}

struct GradeInfo {
    name: &'static str,
    count: uint,
}

impl Hook for GradeInfo {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "grade.name" => Some(self.name.into_scalar()),
            "grade.count" => Some(self.count.into_scalar()),
            _ => None,
        }
    }
}

impl Hook for player::Player {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "meta.duration" => Some(self.duration.into_scalar()),
            "player.playspeed" => Some(self.nominal_playspeed().into_scalar()),
            "player.bpm" => Some(self.bpm.into_scalar()),
            "player.now.time" => Some(self.now.into_scalar()),
            "player.now.vpos" => Some(self.cur.loc.vpos.into_scalar()),
            "player.ratio" => {
                let starttime = (self.now - self.origintime) as f64;
                let duration = self.duration * 1000.0;
                Some((starttime / duration).into_scalar())
            },
            "player.score" => Some(self.score.into_scalar()),
            "player.lastcombo" => Some(self.lastcombo.into_scalar()),
            "player.bestcombo" => Some(self.bestcombo.into_scalar()),
            "player.gauge" =>
                Some((self.gauge as f64 / player::MAXGAUGE as f64).into_scalar()),
            "player.survival" =>
                Some((self.survival as f64 / player::MAXGAUGE as f64).into_scalar()),
            _ => {
                self.opts.scalar_hook(id)
                    .or_else(|| self.meta.scalar_hook(id))
                    .or_else(|| self.timeline.scalar_hook(id))
                    .or_else(|| self.infos.scalar_hook(id))
                    .or_else(|| self.keyspec.scalar_hook(id))
            }
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, mut body: |&Hook, &str| -> bool) -> bool {
        match id {
            "player.survival" => { self.gauge >= self.survival && body(parent, ""); }
            "player.grades" => {
                static GRADENAMES: [&'static str, ..5] = ["cool", "great", "good", "bad", "miss"];
                GRADENAMES.iter().zip(self.gradecounts.iter().rev()).advance(|(&name, &count)|
                    body(&parent.delegate(&GradeInfo { name: name, count: count }), name));
            },
            _ => {
                return self.opts.run_block_hook(id, parent, &mut body) ||
                       self.meta.run_block_hook(id, parent, &mut body) ||
                       self.timeline.run_block_hook(id, parent, &mut body) ||
                       self.infos.run_block_hook(id, parent, &mut body) ||
                       self.keyspec.run_block_hook(id, parent, &mut body);
            }
        }
        true
    }
}

