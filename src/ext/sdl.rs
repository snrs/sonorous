// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

/*!
 * Extensions to rust-sdl. This comprises of additional bindings for SDL_mixer and a minimal
 * but functional binding for SMPEG.
 *
 * NOTE: Some of these additions will be eventually sent to rust-sdl and are not subject to
 * the above copyright notice.
 */

#[cfg(target_os = "win32")]
pub mod syswm {
    use std::libc::{HANDLE, INVALID_HANDLE_VALUE};

    pub mod ll {
        use std::libc::{HANDLE, c_int};

        pub struct SDL_version {
            major: u8,
            minor: u8,
            patch: u8
        }

        impl SDL_version {
            pub fn new() -> SDL_version {
                SDL_version { major: 1, minor: 2, patch: 14 }
            }
        }

        pub struct SDL_SysWMinfo {
            version: SDL_version,
            window: HANDLE,
            hglrc: HANDLE
        }

        extern {
            pub fn SDL_GetWMInfo(info: *mut SDL_SysWMinfo) -> c_int;
        }
    }

    pub fn get_wm_info() -> Option<ll::SDL_SysWMinfo> {
        #[fixed_stack_segment]; #[inline(never)];
        let mut wminfo = ll::SDL_SysWMinfo {
            version: ll::SDL_version::new(),
            window: INVALID_HANDLE_VALUE as HANDLE,
            hglrc: INVALID_HANDLE_VALUE as HANDLE,
        };
        unsafe {
            if ll::SDL_GetWMInfo(::std::ptr::to_mut_unsafe_ptr(&mut wminfo)) == 0 {
                None
            } else {
                Some(wminfo)
            }
        }
    }
}

pub mod mixer {
    use std::libc::c_int;
    pub use sdl::mixer::*;

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
        #[fixed_stack_segment]; #[inline(never)];
        use sdl::mixer;
        unsafe {
            match channel {
                Some(channel) => mixer::ll::Mix_Playing(channel),
                None => mixer::ll::Mix_Playing(-1)
            }
        }
    }

    pub fn get_channel_volume(channel: Option<c_int>) -> c_int {
        #[fixed_stack_segment]; #[inline(never)];
        unsafe {
            let ll_channel = channel.unwrap_or(-1);
            ll::Mix_Volume(ll_channel, -1)
        }
    }

    pub fn set_channel_volume(channel: Option<c_int>, volume: c_int) {
        #[fixed_stack_segment]; #[inline(never)];
        unsafe {
            let ll_channel = channel.unwrap_or(-1);
            ll::Mix_Volume(ll_channel, volume);
        }
    }

    pub fn reserve_channels(num: c_int) -> c_int {
        #[fixed_stack_segment]; #[inline(never)];
        unsafe { ll::Mix_ReserveChannels(num) }
    }

    pub fn group_channel(which: Option<c_int>, tag: Option<c_int>) -> bool {
        #[fixed_stack_segment]; #[inline(never)];
        unsafe {
            let ll_which = which.unwrap_or(-1);
            let ll_tag = tag.unwrap_or(-1);
            ll::Mix_GroupChannel(ll_which, ll_tag) != 0
        }
    }

    pub fn newest_in_group(tag: Option<c_int>) -> Option<c_int> {
        #[fixed_stack_segment]; #[inline(never)];
        unsafe {
            let ll_tag = tag.unwrap_or(-1);
            let channel = ll::Mix_GroupNewer(ll_tag);
            if channel == -1 {None} else {Some(channel)}
        }
    }
}

pub mod mpeg {
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
        pub enum SMPEGstatus {
            SMPEG_ERROR = -1,
            SMPEG_STOPPED = 0,
            SMPEG_PLAYING =1
        }
        #[link_args = "-lsmpeg"]
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
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_delete(self.raw); }
        }
    }

    impl MPEG {
        pub fn from_path(path: &Path) -> Result<~MPEG, ~str> {
            #[fixed_stack_segment]; #[inline(never)];
            let raw = unsafe {
                do path.to_c_str().with_ref |buf| {
                    ll::SMPEG_new(buf, null(), 0)
                }
            };

            if raw.is_null() { Err(::sdl::get_error()) }
            else { Ok(wrap_mpeg(raw)) }
        }

        pub fn status(&self) -> SMPEGstatus {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_status(self.raw) }
        }

        pub fn set_volume(&self, volume: int) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_setvolume(self.raw, volume as c_int); }
        }

        pub fn set_display(&self, surface: &Surface) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe {
                ll::SMPEG_setdisplay(self.raw, surface.raw, null(), null());
            }
        }

        pub fn enable_video(&self, enable: bool) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_enablevideo(self.raw, enable as c_int); }
        }

        pub fn enable_audio(&self, enable: bool) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_enableaudio(self.raw, enable as c_int); }
        }

        pub fn set_loop(&self, repeat: bool) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_loop(self.raw, repeat as c_int); }
        }

        pub fn resize(&self, width: int, height: int) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_scaleXY(self.raw, width as c_int, height as c_int); }
        }

        pub fn scale_by(&self, scale: int) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_scale(self.raw, scale as c_int); }
        }

        pub fn move(&self, x: int, y: int) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_move(self.raw, x as c_int, y as c_int); }
        }

        pub fn set_display_region(&self, x: int, y: int, w: int, h: int) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe {
                ll::SMPEG_setdisplayregion(self.raw, x as c_int, y as c_int,
                                           w as c_int, h as c_int);
            }
        }

        pub fn play(&self) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_play(self.raw); }
        }

        pub fn pause(&self) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_pause(self.raw); }
        }

        pub fn stop(&self) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_stop(self.raw); }
        }

        pub fn rewind(&self) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_rewind(self.raw); }
        }

        pub fn seek(&self, bytes: int) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_seek(self.raw, bytes as c_int); }
        }

        pub fn skip(&self, seconds: f64) {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe { ll::SMPEG_skip(self.raw, seconds as c_float); }
        }

        pub fn get_error(&self) -> ~str {
            #[fixed_stack_segment]; #[inline(never)];
            unsafe {
                let cstr = ll::SMPEG_error(self.raw);
                ::std::str::raw::from_c_str(::std::cast::transmute(&cstr))
            }
        }
    }
}

