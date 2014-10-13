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
    for options::Options |opts, id, parent, body| {
        scalar "opts.playspeed" => opts.playspeed.into_scalar();

        block "opts.autoplay" => opts.is_autoplay() && body(parent, "");
        block "opts.modifier" => match opts.modf {
            Some(options::MirrorModf)    => { body(parent, "mirror"); }
            Some(options::ShuffleModf)   => { body(parent, "shuffle"); }
            Some(options::ShuffleExModf) => { body(parent, "shuffle-ex"); }
            Some(options::RandomModf)    => { body(parent, "random"); }
            Some(options::RandomExModf)  => { body(parent, "random-ex"); }
            None => {}
        };
        block "opts.hasbga" => opts.has_bga() && body(parent, "");
        block "opts.hasmovie" => opts.has_movie() && body(parent, "");
        block "opts.showinfo" => opts.showinfo && body(parent, "");
        block "opts.fullscreen" => opts.fullscreen && body(parent, "");
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
    for timeline::TimelineInfo |infos, id, parent, body| {
        scalar "timeline.nnotes" => infos.nnotes.into_scalar();
        scalar "timeline.maxscore" => infos.maxscore.into_scalar();

        block "timeline.bpmchange" => infos.hasbpmchange && body(parent, "");
        block "timeline.longnote" => infos.haslongnote && body(parent, "");
    }
}

define_hooks! {
    for metadata::Meta |meta, id, parent, body| {
        scalar "meta.title" => return meta.title.as_ref().map(|s| s.as_scalar());
        scalar "meta.genre" => return meta.genre.as_ref().map(|s| s.as_scalar());
        scalar "meta.artist" => return meta.artist.as_ref().map(|s| s.as_scalar());
        scalar "meta.level" => return meta.level.as_ref().map(|lv| lv.value.into_scalar());
        scalar "meta.difficulty" =>
            return meta.difficulty.map(|metadata::Difficulty(diff)| diff.into_scalar());

        block "meta.title" => meta.title.is_some() && body(parent, "");
        block "meta.subtitle" =>
            meta.subtitles.iter().all(|s|
                body(&parent.add_text("meta.subtitle", s[]), ""));
        block "meta.genre" => meta.genre.is_some() && body(parent, "");
        block "meta.artist" => meta.artist.is_some() && body(parent, "");
        block "meta.subartist" =>
            meta.subartists.iter().all(|s|
                body(&parent.add_text("meta.subartist", s[]), ""));
        block "meta.comment" =>
            meta.comments.iter().all(|s|
                body(&parent.add_text("meta.comment", s[]), ""));
        block "meta.level" => meta.level.is_some() && body(parent, "");
        block "meta.levelsystem" => match meta.level.as_ref().map(|lv| lv.system) {
            Some(metadata::LevelSystemBms) => { body(parent, "bms"); }
            None => {}
        };
        block "meta.difficulty" => match meta.difficulty {
            Some(metadata::Difficulty(1)) => { body(parent, "beginner"); }
            Some(metadata::Difficulty(2)) => { body(parent, "normal"); }
            Some(metadata::Difficulty(3)) => { body(parent, "hard"); }
            Some(metadata::Difficulty(4)) => { body(parent, "extra"); }
            Some(metadata::Difficulty(5)) => { body(parent, "insane"); }
            Some(metadata::Difficulty(_)) => { body(parent, "???"); }
            None => {}
        };
    }

    for bms::BmsMeta |meta, id, parent, body| {
        delegate meta.common;

        scalar "meta.encoding" => meta.encoding.val0().into_scalar();
        scalar "meta.encoding.confidence" => {
            let mut confidence = meta.encoding.val1();
            if confidence > 1.0 { confidence = 1.0; }
            confidence.into_scalar()
        };

        block "meta.playmode" => match meta.mode {
            bms::SinglePlay => { body(parent, "single"); }
            bms::CouplePlay => { body(parent, "couple"); }
            bms::DoublePlay => { body(parent, "double"); }
            bms::BattlePlay => { body(parent, "battle"); }
        };
    }

    for bms::Bms |bms, id, parent, body| {
        delegate bms.meta;
        delegate bms.timeline;
    }

    for keyspec::KeySpec |keyspec, id, parent, body| {
        scalar "meta.nkeys" => keyspec.nkeys().into_scalar();
    }
}

struct GradeInfo {
    name: &'static str,
    count: uint,
}

define_hooks! {
    for GradeInfo |grade, id, parent, body| {
        scalar "grade.name" => grade.name.into_scalar();
        scalar "grade.count" => grade.count.into_scalar();
    }

    for player::Player |player, id, parent, body| {
        delegate player.opts;
        delegate player.meta;
        delegate player.timeline;
        delegate player.infos;
        delegate player.keyspec;

        scalar "meta.duration" => player.duration.into_scalar();
        scalar "player.playspeed" => player.nominal_playspeed().into_scalar();
        scalar "player.bpm" => player.bpm.into_scalar();
        scalar "player.now.time" => player.now.into_scalar();
        scalar "player.now.vpos" => player.cur.loc.vpos.into_scalar();
        scalar "player.ratio" => {
            let starttime = (player.now - player.origintime) as f64;
            let duration = player.duration * 1000.0;
            (starttime / duration).into_scalar()
        };
        scalar "player.score" => player.score.into_scalar();
        scalar "player.lastcombo" => player.lastcombo.into_scalar();
        scalar "player.bestcombo" => player.bestcombo.into_scalar();
        scalar "player.gauge" =>
            (player.gauge as f64 / player::MAXGAUGE as f64).into_scalar();
        scalar "player.survival" =>
            (player.survival as f64 / player::MAXGAUGE as f64).into_scalar();
        block "player.survival" => player.gauge >= player.survival && body(parent, "");
        block "player.grades" => {
            static GRADENAMES: [&'static str, ..5] = ["cool", "great", "good", "bad", "miss"];
            GRADENAMES.iter().zip(player.gradecounts.iter().rev()).all(|(&name, &count)|
                body(&parent.delegate(&GradeInfo { name: name, count: count }), name));
        };
    }
}

