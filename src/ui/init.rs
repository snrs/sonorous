// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Initialization.

use sdl::*;
use engine::resource::{BGAW, BGAH, SAMPLERATE};
use ui::screen::Screen;

/// The width of screen, unless the exclusive mode.
pub static SCREENW: uint = 800;
/// The height of screen, unless the exclusive mode.
pub static SCREENH: uint = 600;

/// Creates a small screen for BGAs (`BGAW` by `BGAH` pixels) if `exclusive` is set,
/// or a full-sized screen (`SCREENW` by `SCREENH` pixels) otherwise. `fullscreen` is ignored
/// when `exclusive` is set.
pub fn init_video(exclusive: bool, fullscreen: bool) -> Screen {
    let (width, height, fullscreen) = if exclusive {
        (BGAW, BGAH, false)
    } else {
        (SCREENW, SCREENH, fullscreen)
    };
    let screen = match Screen::new(width, height, fullscreen) {
        Ok(screen) => screen,
        Err(err) => die!("Failed to initialize screen: %s", err)
    };
    if !exclusive {
        mouse::set_cursor_visible(false);
    }
    wm::set_caption(::version(), "");
    screen
}

/// Initializes an SDL, SDL_image and SDL_mixer.
pub fn init_sdl() {
    if !init([InitVideo, InitAudio, InitJoystick]) {
        die!("SDL Initialization Failure: %s", get_error());
    }
    img::init([img::InitJPG, img::InitPNG]);
    //mixer::init([mixer::InitOGG, mixer::InitMP3]); // TODO
    if mixer::open(SAMPLERATE, audio::S16AudioFormat, audio::Stereo, 2048).is_err() {
        die!("SDL Mixer Initialization Failure");
    }
}

/// Initializes a joystick with given index.
pub fn init_joystick(joyidx: uint) -> ~joy::Joystick {
    unsafe {
        joy::ll::SDL_JoystickEventState(1); // TODO rust-sdl patch
    }
    match joy::Joystick::open(joyidx as int) {
        Ok(joy) => joy,
        Err(err) => die!("SDL Joystick Initialization Failure: %s", err)
    }
}

