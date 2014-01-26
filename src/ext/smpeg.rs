// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! The minimal but functional binding for SMPEG.

use std::libc::{c_int, c_float};
use std::ptr::null;
use sdl::video::Surface;
pub use self::ll::{SMPEGstatus, SMPEG_ERROR, SMPEG_STOPPED, SMPEG_PLAYING};

pub mod ll {
    use std::libc::{c_void, c_int, c_char, c_float, c_double};
    use sdl::video::ll::{SDL_RWops, SDL_Surface};
    use sdl::audio::ll::SDL_AudioSpec;
    pub struct SMPEG { priv opaque: () }
    pub struct SMPEG_Info {
        has_audio: c_int,
        has_video: c_int,
        width: c_int,
        height: c_int,
        current_frame: c_int,
        current_fps: c_double,
        audio_string: [c_char, ..80],
        audio_current_frame: c_int,
        current_offset: u32,
        total_size: u32,
        current_time: c_double,
        total_time: c_double
    }
    #[deriving(Eq, Clone)]
    #[repr(C)]
    pub enum SMPEGstatus {
        SMPEG_ERROR = -1,
        SMPEG_STOPPED = 0,
        SMPEG_PLAYING =1
    }
    #[link(name = "smpeg")]
    extern {
        pub fn SMPEG_new(file: *c_char, info: *SMPEG_Info,
                         sdl_audio: c_int) -> *SMPEG;
        pub fn SMPEG_new_descr(file: c_int, info: *SMPEG_Info,
                               sdl_audio: c_int) -> *SMPEG;
        pub fn SMPEG_new_data(data: *c_void, size: c_int, info: *SMPEG_Info,
                              sdl_audio: c_int) -> *SMPEG;
        pub fn SMPEG_new_rwops(src: *SDL_RWops, info: *SMPEG_Info,
                               sdl_audio: c_int) -> *SMPEG;
        pub fn SMPEG_getinfo(mpeg: *SMPEG, info: *SMPEG_Info);
        pub fn SMPEG_enableaudio(mpeg: *SMPEG, enable: c_int);
        pub fn SMPEG_enablevideo(mpeg: *SMPEG, enable: c_int);
        pub fn SMPEG_delete(mpeg: *SMPEG);
        pub fn SMPEG_status(mpeg: *SMPEG) -> SMPEGstatus;
        pub fn SMPEG_setvolume(mpeg: *SMPEG, volume: c_int);
        // XXX SDL_Mutex and SMPEG_DisplayCallback unimplemented
        pub fn SMPEG_setdisplay(mpeg: *SMPEG, dst: *SDL_Surface,
                                surfLock: *c_void, callback: *c_void);
        pub fn SMPEG_loop(mpeg: *SMPEG, repeat: c_int);
        pub fn SMPEG_scaleXY(mpeg: *SMPEG, width: c_int, height: c_int);
        pub fn SMPEG_scale(mpeg: *SMPEG, scale: c_int);
        pub fn SMPEG_move(mpeg: *SMPEG, x: c_int, y: c_int);
        pub fn SMPEG_setdisplayregion(mpeg: *SMPEG, x: c_int, y: c_int,
                                      w: c_int, h: c_int);
        pub fn SMPEG_play(mpeg: *SMPEG);
        pub fn SMPEG_pause(mpeg: *SMPEG);
        pub fn SMPEG_stop(mpeg: *SMPEG);
        pub fn SMPEG_rewind(mpeg: *SMPEG);
        pub fn SMPEG_seek(mpeg: *SMPEG, bytes: c_int);
        pub fn SMPEG_skip(mpeg: *SMPEG, seconds: c_float);
        pub fn SMPEG_renderFrame(mpeg: *SMPEG, framenum: c_int);
        pub fn SMPEG_renderFinal(mpeg: *SMPEG, dst: *SDL_Surface, x: c_int, y: c_int);
        // XXX SMPEG_Filter unimplemented
        pub fn SMPEG_filter(mpeg: *SMPEG, filter: *c_void) -> *c_void;
        pub fn SMPEG_error(mpeg: *SMPEG) -> *c_char;
        pub fn SMPEG_playAudio(mpeg: *SMPEG, stream: *u8, len: c_int) -> c_int;
        pub fn SMPEG_playAudioSDL(mpeg: *c_void, stream: *u8, len: c_int) -> c_int;
        pub fn SMPEG_wantedSpec(mpeg: *SMPEG, wanted: *SDL_AudioSpec) -> c_int;
        pub fn SMPEG_actualSpec(mpeg: *SMPEG, spec: *SDL_AudioSpec);
    }
}

pub struct MPEG {
    raw: *ll::SMPEG
}

fn wrap_mpeg(raw: *ll::SMPEG) -> ~MPEG {
    ~MPEG { raw: raw }
}

impl Drop for MPEG {
    fn drop(&mut self) {
        unsafe { ll::SMPEG_delete(self.raw); }
    }
}

impl MPEG {
    pub fn from_path(path: &Path) -> Result<~MPEG, ~str> {
        let raw = unsafe {
            path.to_c_str().with_ref(|buf| {
                ll::SMPEG_new(buf, null(), 0)
            })
        };

        if raw.is_null() { Err(::sdl::get_error()) }
        else { Ok(wrap_mpeg(raw)) }
    }

    pub fn status(&self) -> SMPEGstatus {
        unsafe { ll::SMPEG_status(self.raw) }
    }

    pub fn set_volume(&self, volume: int) {
        unsafe { ll::SMPEG_setvolume(self.raw, volume as c_int); }
    }

    pub fn set_display(&self, surface: &Surface) {
        unsafe {
            ll::SMPEG_setdisplay(self.raw, surface.raw, null(), null());
        }
    }

    pub fn enable_video(&self, enable: bool) {
        unsafe { ll::SMPEG_enablevideo(self.raw, enable as c_int); }
    }

    pub fn enable_audio(&self, enable: bool) {
        unsafe { ll::SMPEG_enableaudio(self.raw, enable as c_int); }
    }

    pub fn set_loop(&self, repeat: bool) {
        unsafe { ll::SMPEG_loop(self.raw, repeat as c_int); }
    }

    pub fn resize(&self, width: int, height: int) {
        unsafe { ll::SMPEG_scaleXY(self.raw, width as c_int, height as c_int); }
    }

    pub fn scale_by(&self, scale: int) {
        unsafe { ll::SMPEG_scale(self.raw, scale as c_int); }
    }

    pub fn move(&self, x: int, y: int) {
        unsafe { ll::SMPEG_move(self.raw, x as c_int, y as c_int); }
    }

    pub fn set_display_region(&self, x: int, y: int, w: int, h: int) {
        unsafe {
            ll::SMPEG_setdisplayregion(self.raw, x as c_int, y as c_int,
                                       w as c_int, h as c_int);
        }
    }

    pub fn play(&self) {
        unsafe { ll::SMPEG_play(self.raw); }
    }

    pub fn pause(&self) {
        unsafe { ll::SMPEG_pause(self.raw); }
    }

    pub fn stop(&self) {
        unsafe { ll::SMPEG_stop(self.raw); }
    }

    pub fn rewind(&self) {
        unsafe { ll::SMPEG_rewind(self.raw); }
    }

    pub fn seek(&self, bytes: int) {
        unsafe { ll::SMPEG_seek(self.raw, bytes as c_int); }
    }

    pub fn skip(&self, seconds: f64) {
        unsafe { ll::SMPEG_skip(self.raw, seconds as c_float); }
    }

    pub fn get_error(&self) -> ~str {
        unsafe {
            let cstr = ll::SMPEG_error(self.raw);
            ::std::str::raw::from_c_str(::std::cast::transmute(&cstr))
        }
    }
}

