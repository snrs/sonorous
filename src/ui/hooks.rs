// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Common skin hooks for various types.

use format::{timeline, bms};
use gfx::skin::hook::{Scalar, AsScalar, IntoScalar, Hook};
use engine::keyspec;

impl<S,I> Hook for timeline::Timeline<S,I> {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "timeline.initbpm" => Some(self.initbpm.to_f64().into_scalar()),
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

impl Hook for bms::BmsMeta {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        match id {
            "meta.encoding" => Some(self.encoding.val0().into_scalar()),
            "meta.encoding.confidence" => {
                let mut confidence = self.encoding.val1();
                if confidence > 1.0 { confidence = 1.0; }
                Some(confidence.into_scalar())
            },
            "meta.title" => self.title.as_ref().map(|s| s.as_scalar()),
            "meta.genre" => self.genre.as_ref().map(|s| s.as_scalar()),
            "meta.artist" => self.artist.as_ref().map(|s| s.as_scalar()),
            "meta.playlevel" => Some(self.playlevel.into_scalar()),
            "meta.difficulty" => match self.difficulty {
                Some(bms::Difficulty(diff)) => Some(diff.into_scalar()),
                None => None,
            },
            _ => None,
        }
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        match id {
            "meta.title" => { self.title.is_some() && body(parent, ""); }
            "meta.subtitle" => {
                self.subtitles.iter().advance(|s| body(&parent.add_text("meta.subtitle", *s), ""));
            }
            "meta.genre" => { self.genre.is_some() && body(parent, ""); }
            "meta.artist" => { self.artist.is_some() && body(parent, ""); }
            "meta.subartist" => {
                self.subartists.iter()
                               .advance(|s| body(&parent.add_text("meta.subartist", *s), ""));
            }
            "meta.comment" => {
                self.comments.iter().advance(|s| body(&parent.add_text("meta.comment", *s), ""));
            }
            "meta.playmode" => match self.mode {
                bms::SinglePlay => { body(parent, "single"); }
                bms::CouplePlay => { body(parent, "couple"); }
                bms::DoublePlay => { body(parent, "double"); }
                bms::BattlePlay => { body(parent, "battle"); }
            },
            "meta.playlevel" => { self.playlevel > 0 && body(parent, ""); }
            "meta.difficulty" => match self.difficulty {
                Some(bms::Difficulty(1)) => { body(parent, "beginner"); }
                Some(bms::Difficulty(2)) => { body(parent, "normal"); }
                Some(bms::Difficulty(3)) => { body(parent, "hard"); }
                Some(bms::Difficulty(4)) => { body(parent, "extra"); }
                Some(bms::Difficulty(5)) => { body(parent, "insane"); }
                Some(bms::Difficulty(_)) => { body(parent, "???"); }
                None => {}
            },
            _ => { return false; }
        }
        true
    }
}

impl Hook for bms::Bms {
    fn scalar_hook<'a>(&'a self, id: &str) -> Option<Scalar<'a>> {
        self.meta.scalar_hook(id)
            .or_else(|| self.timeline.scalar_hook(id))
    }

    fn block_hook(&self, id: &str, parent: &Hook, body: |&Hook, &str| -> bool) -> bool {
        self.meta.run_block_hook(id, parent, &body) ||
            self.timeline.run_block_hook(id, parent, &body)
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

