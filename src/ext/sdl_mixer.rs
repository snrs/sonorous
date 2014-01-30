// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Additional bindings for SDL_mixer.

use std::libc::c_int;
pub use sdl_mixer::*;

pub mod ll {
    use std::libc::c_int;
    extern {
        pub fn Mix_Volume(channel: c_int, volume: c_int) -> c_int;
        pub fn Mix_ReserveChannels(num: c_int) -> c_int;
        pub fn Mix_GroupChannel(which: c_int, tag: c_int) -> c_int;
        pub fn Mix_GroupNewer(tag: c_int) -> c_int;
    }
}

pub fn num_playing(channel: Option<c_int>) -> c_int {
    use sdl_mixer;
    unsafe {
        match channel {
            Some(channel) => sdl_mixer::ll::Mix_Playing(channel),
            None => sdl_mixer::ll::Mix_Playing(-1)
        }
    }
}

pub fn get_channel_volume(channel: Option<c_int>) -> c_int {
    unsafe {
        let ll_channel = channel.unwrap_or(-1);
        ll::Mix_Volume(ll_channel, -1)
    }
}

pub fn set_channel_volume(channel: Option<c_int>, volume: c_int) {
    unsafe {
        let ll_channel = channel.unwrap_or(-1);
        ll::Mix_Volume(ll_channel, volume);
    }
}

pub fn reserve_channels(num: c_int) -> c_int {
    unsafe { ll::Mix_ReserveChannels(num) }
}

pub fn group_channel(which: Option<c_int>, tag: Option<c_int>) -> bool {
    unsafe {
        let ll_which = which.unwrap_or(-1);
        let ll_tag = tag.unwrap_or(-1);
        ll::Mix_GroupChannel(ll_which, ll_tag) != 0
    }
}

pub fn newest_in_group(tag: Option<c_int>) -> Option<c_int> {
    unsafe {
        let ll_tag = tag.unwrap_or(-1);
        let channel = ll::Mix_GroupNewer(ll_tag);
        if channel == -1 {None} else {Some(channel)}
    }
}

