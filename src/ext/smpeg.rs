// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! The minimal but functional binding for SMPEG.

use libc::{c_int, c_float};
use std::ptr::mut_null;
use std::mem::transmute;
use std::string::raw::from_buf;
use sdl::video::Surface;
pub use self::ll::{SMPEGstatus, SMPEG_ERROR, SMPEG_STOPPED, SMPEG_PLAYING};

pub mod ll {
    #![allow(non_camel_case_types)]

    use libc::{c_void, c_int, c_char, c_float, c_double};
    use sdl::video::ll::{SDL_RWops, SDL_Surface};
    use sdl::audio::ll::SDL_AudioSpec;
    #[repr(C)]
    pub struct SMPEG { _opaque: () }
    #[repr(C)]
    pub struct SMPEG_Info {
        pub has_audio: c_int,
        pub has_video: c_int,
        pub width: c_int,
        pub height: c_int,
        pub current_frame: c_int,
        pub current_fps: c_double,
        pub audio_string: [c_char, ..80],
        pub audio_current_frame: c_int,
        pub current_offset: u32,
        pub total_size: u32,
        pub current_time: c_double,
        pub total_time: c_double
    }
    #[deriving(PartialEq, Eq, Clone)]
    #[repr(C)]
    pub enum SMPEGstatus {
        SMPEG_ERROR = -1,
        SMPEG_STOPPED = 0,
        SMPEG_PLAYING =1
    }
    #[link(name = "smpeg")]
    extern {
        pub fn SMPEG_new(file: *const c_char, info: *mut SMPEG_Info,
                         sdl_audio: c_int) -> *mut SMPEG;
        pub fn SMPEG_new_descr(file: c_int, info: *mut SMPEG_Info,
                               sdl_audio: c_int) -> *mut SMPEG;
        pub fn SMPEG_new_data(data: *mut c_void, size: c_int, info: *mut SMPEG_Info,
                              sdl_audio: c_int) -> *mut SMPEG;
        pub fn SMPEG_new_rwops(src: *mut SDL_RWops, info: *mut SMPEG_Info,
                               sdl_audio: c_int) -> *mut SMPEG;
        pub fn SMPEG_getinfo(mpeg: *mut SMPEG, info: *mut SMPEG_Info);
        pub fn SMPEG_enableaudio(mpeg: *mut SMPEG, enable: c_int);
        pub fn SMPEG_enablevideo(mpeg: *mut SMPEG, enable: c_int);
        pub fn SMPEG_delete(mpeg: *mut SMPEG);
        pub fn SMPEG_status(mpeg: *mut SMPEG) -> SMPEGstatus;
        pub fn SMPEG_setvolume(mpeg: *mut SMPEG, volume: c_int);
        // XXX SDL_Mutex and SMPEG_DisplayCallback unimplemented
        pub fn SMPEG_setdisplay(mpeg: *mut SMPEG, dst: *mut SDL_Surface,
                                surfLock: *mut c_void, callback: *mut c_void);
        pub fn SMPEG_loop(mpeg: *mut SMPEG, repeat: c_int);
        pub fn SMPEG_scaleXY(mpeg: *mut SMPEG, width: c_int, height: c_int);
        pub fn SMPEG_scale(mpeg: *mut SMPEG, scale: c_int);
        pub fn SMPEG_move(mpeg: *mut SMPEG, x: c_int, y: c_int);
        pub fn SMPEG_setdisplayregion(mpeg: *mut SMPEG, x: c_int, y: c_int, w: c_int, h: c_int);
        pub fn SMPEG_play(mpeg: *mut SMPEG);
        pub fn SMPEG_pause(mpeg: *mut SMPEG);
        pub fn SMPEG_stop(mpeg: *mut SMPEG);
        pub fn SMPEG_rewind(mpeg: *mut SMPEG);
        pub fn SMPEG_seek(mpeg: *mut SMPEG, bytes: c_int);
        pub fn SMPEG_skip(mpeg: *mut SMPEG, seconds: c_float);
        pub fn SMPEG_renderFrame(mpeg: *mut SMPEG, framenum: c_int);
        pub fn SMPEG_renderFinal(mpeg: *mut SMPEG, dst: *mut SDL_Surface, x: c_int, y: c_int);
        // XXX SMPEG_Filter unimplemented
        pub fn SMPEG_filter(mpeg: *mut SMPEG, filter: *mut c_void) -> *mut c_void;
        pub fn SMPEG_error(mpeg: *mut SMPEG) -> *mut c_char;
        pub fn SMPEG_playAudio(mpeg: *mut SMPEG, stream: *mut u8, len: c_int) -> c_int;
        pub fn SMPEG_playAudioSDL(mpeg: *mut c_void, stream: *mut u8, len: c_int) -> c_int;
        pub fn SMPEG_wantedSpec(mpeg: *mut SMPEG, wanted: *mut SDL_AudioSpec) -> c_int;
        pub fn SMPEG_actualSpec(mpeg: *mut SMPEG, spec: *mut SDL_AudioSpec);
    }
}

pub struct MPEG {
    pub raw: *mut ll::SMPEG
}

fn wrap_mpeg(raw: *mut ll::SMPEG) -> MPEG {
    MPEG { raw: raw }
}

impl Drop for MPEG {
    fn drop(&mut self) {
        unsafe { ll::SMPEG_delete(self.raw); }
    }
}

impl MPEG {
    pub fn from_path(path: &Path) -> Result<MPEG, String> {
        let raw = unsafe {
            let path = path.to_c_str();
            ll::SMPEG_new(path.as_ptr(), mut_null(), 0)
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
            ll::SMPEG_setdisplay(self.raw, surface.raw, mut_null(), mut_null());
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

    pub fn move_by(&self, x: int, y: int) {
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

    pub fn get_error(&self) -> String {
        unsafe {
            let cstr = ll::SMPEG_error(self.raw);
            from_buf(transmute(&cstr))
        }
    }
}

