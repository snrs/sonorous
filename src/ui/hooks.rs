// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Common skin hooks for various types.

use format::{timeline, metadata, bms};
use engine::{keyspec, player};
use ui::options;

use gfx::skin::scalar::{Scalar, IntoScalar};
use gfx::skin::hook::Hook;

define_hooks! {
    for options::Options {
        scalar "opts.playspeed" => self.playspeed.into_scalar();

        block "opts.autoplay" => self.is_autoplay() && body(parent, "");
        block "opts.modifier" => match self.modf {
            Some(options::MirrorModf)    => { body(parent, "mirror"); }
            Some(options::ShuffleModf)   => { body(parent, "shuffle"); }
            Some(options::ShuffleExModf) => { body(parent, "shuffle-ex"); }
            Some(options::RandomModf)    => { body(parent, "random"); }
            Some(options::RandomExModf)  => { body(parent, "random-ex"); }
            None => {}
        };
        block "opts.hasbga" => self.has_bga() && body(parent, "");
        block "opts.hasmovie" => self.has_movie() && body(parent, "");
        block "opts.showinfo" => self.showinfo && body(parent, "");
        block "opts.fullscreen" => self.fullscreen && body(parent, "");
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

define_hooks! {
    for timeline::TimelineInfo {
        scalar "timeline.#notes" => self.nnotes.into_scalar();
        scalar "timeline.maxscore" => self.maxscore.into_scalar();

        block "timeline.bpmchange" => self.hasbpmchange && body(parent, "");
        block "timeline.longnote" => self.haslongnote && body(parent, "");
    }
}

define_hooks! {
    for metadata::Meta {
        scalar "meta.title" => return self.title.as_ref().map(|s| s.as_scalar());
        scalar "meta.genre" => return self.genre.as_ref().map(|s| s.as_scalar());
        scalar "meta.artist" => return self.artist.as_ref().map(|s| s.as_scalar());
        scalar "meta.level" => return self.level.as_ref().map(|lv| lv.value.into_scalar());
        scalar "meta.difficulty" =>
            return self.difficulty.map(|metadata::Difficulty(diff)| diff.into_scalar());

        block "meta.title" => self.title.is_some() && body(parent, "");
        block "meta.subtitle" =>
            self.subtitles.iter().advance(|s|
                body(&parent.add_text("meta.subtitle", s.as_slice()), ""));
        block "meta.genre" => self.genre.is_some() && body(parent, "");
        block "meta.artist" => self.artist.is_some() && body(parent, "");
        block "meta.subartist" =>
            self.subartists.iter().advance(|s|
                body(&parent.add_text("meta.subartist", s.as_slice()), ""));
        block "meta.comment" =>
            self.comments.iter().advance(|s|
                body(&parent.add_text("meta.comment", s.as_slice()), ""));
        block "meta.level" => self.level.is_some() && body(parent, "");
        block "meta.levelsystem" => match self.level.as_ref().map(|lv| lv.system) {
            Some(metadata::LevelSystemBms) => { body(parent, "bms"); }
            None => {}
        };
        block "meta.difficulty" => match self.difficulty {
            Some(metadata::Difficulty(1)) => { body(parent, "beginner"); }
            Some(metadata::Difficulty(2)) => { body(parent, "normal"); }
            Some(metadata::Difficulty(3)) => { body(parent, "hard"); }
            Some(metadata::Difficulty(4)) => { body(parent, "extra"); }
            Some(metadata::Difficulty(5)) => { body(parent, "insane"); }
            Some(metadata::Difficulty(_)) => { body(parent, "???"); }
            None => {}
        };
    }

    for bms::BmsMeta {
        delegate self.common;

        scalar "meta.encoding" => self.encoding.val0().into_scalar();
        scalar "meta.encoding.confidence" => {
            let mut confidence = self.encoding.val1();
            if confidence > 1.0 { confidence = 1.0; }
            confidence.into_scalar()
        };

        block "meta.playmode" => match self.mode {
            bms::SinglePlay => { body(parent, "single"); }
            bms::CouplePlay => { body(parent, "couple"); }
            bms::DoublePlay => { body(parent, "double"); }
            bms::BattlePlay => { body(parent, "battle"); }
        };
    }

    for bms::Bms {
        delegate self.meta;
        delegate self.timeline;
    }

    for keyspec::KeySpec {
        scalar "meta.#keys" => self.nkeys().into_scalar();
    }
}

struct GradeInfo {
    name: &'static str,
    count: uint,
}

define_hooks! {
    for GradeInfo {
        scalar "grade.name" => self.name.into_scalar();
        scalar "grade.count" => self.count.into_scalar();
    }

    for player::Player {
        delegate self.opts;
        delegate self.meta;
        delegate self.timeline;
        delegate self.infos;
        delegate self.keyspec;

        scalar "meta.duration" => self.duration.into_scalar();
        scalar "player.playspeed" => self.nominal_playspeed().into_scalar();
        scalar "player.bpm" => self.bpm.into_scalar();
        scalar "player.now.time" => self.now.into_scalar();
        scalar "player.now.vpos" => self.cur.loc.vpos.into_scalar();
        scalar "player.ratio" => {
            let starttime = (self.now - self.origintime) as f64;
            let duration = self.duration * 1000.0;
            (starttime / duration).into_scalar()
        };
        scalar "player.score" => self.score.into_scalar();
        scalar "player.lastcombo" => self.lastcombo.into_scalar();
        scalar "player.bestcombo" => self.bestcombo.into_scalar();
        scalar "player.gauge" => (self.gauge as f64 / player::MAXGAUGE as f64).into_scalar();
        scalar "player.survival" => (self.survival as f64 / player::MAXGAUGE as f64).into_scalar();
        block "player.survival" => self.gauge >= self.survival && body(parent, "");
        block "player.grades" => {
            static GRADENAMES: [&'static str, ..5] = ["cool", "great", "good", "bad", "miss"];
            GRADENAMES.iter().zip(self.gradecounts.iter().rev()).advance(|(&name, &count)|
                body(&parent.delegate(&GradeInfo { name: name, count: count }), name));
        };
    }
}

