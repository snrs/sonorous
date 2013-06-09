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
    use core::libc::{HANDLE, INVALID_HANDLE_VALUE};

    pub mod ll {
        use core::libc::{HANDLE, c_int};

        pub struct SDL_version {
            major: u8,
            minor: u8,
            patch: u8
        }

        pub impl SDL_version {
            fn new() -> SDL_version {
                SDL_version { major: 1, minor: 2, patch: 14 }
            }
        }

        pub struct SDL_SysWMinfo {
            version: SDL_version,
            window: HANDLE,
            hglrc: HANDLE
        }

        pub extern {
            fn SDL_GetWMInfo(info: *mut SDL_SysWMinfo) -> c_int;
        }
    }

    pub fn get_wm_info() -> Option<ll::SDL_SysWMinfo> {
        let mut wminfo = ll::SDL_SysWMinfo {
            version: ll::SDL_version::new(),
            window: INVALID_HANDLE_VALUE as HANDLE,
            hglrc: INVALID_HANDLE_VALUE as HANDLE,
        };
        unsafe {
            if ll::SDL_GetWMInfo(ptr::to_mut_unsafe_ptr(&mut wminfo)) == 0 {
                None
            } else {
                Some(wminfo)
            }
        }
    }
}

pub mod mixer {
    use core::libc::c_int;

    pub mod ll {
        use core::libc::c_int;
        pub extern {
            fn Mix_Volume(channel: c_int, volume: c_int) -> c_int;
            fn Mix_ReserveChannels(num: c_int) -> c_int;
            fn Mix_GroupChannel(which: c_int, tag: c_int) -> c_int;
            fn Mix_GroupNewer(tag: c_int) -> c_int;
        }
    }

    pub fn num_playing(channel: Option<c_int>) -> c_int {
        use sdl::mixer;
        match channel {
            Some(channel) => mixer::ll::Mix_Playing(channel),
            None => mixer::ll::Mix_Playing(-1)
        }
    }

    pub fn get_channel_volume(channel: Option<c_int>) -> c_int {
        unsafe {
            let ll_channel = channel.get_or_default(-1);
            ll::Mix_Volume(ll_channel, -1)
        }
    }

    pub fn set_channel_volume(channel: Option<c_int>, volume: c_int) {
        unsafe {
            let ll_channel = channel.get_or_default(-1);
            ll::Mix_Volume(ll_channel, volume);
        }
    }

    pub fn reserve_channels(num: c_int) -> c_int {
        unsafe { ll::Mix_ReserveChannels(num) }
    }

    pub fn group_channel(which: Option<c_int>, tag: Option<c_int>) -> bool {
        unsafe {
            let ll_which = which.get_or_default(-1);
            let ll_tag = tag.get_or_default(-1);
            ll::Mix_GroupChannel(ll_which, ll_tag) != 0
        }
    }

    pub fn newest_in_group(tag: Option<c_int>) -> Option<c_int> {
        unsafe {
            let ll_tag = tag.get_or_default(-1);
            let channel = ll::Mix_GroupNewer(ll_tag);
            if channel == -1 {None} else {Some(channel)}
        }
    }
}

pub mod mpeg {
    use core::libc::{c_int, c_float};
    use sdl::video::Surface;
    use self::ll::SMPEGstatus;

    pub mod ll {
        use core::libc::{c_void, c_int, c_char, c_float, c_double};
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
        pub enum SMPEGstatus {
            SMPEG_ERROR = -1,
            SMPEG_STOPPED = 0,
            SMPEG_PLAYING =1
        }
        #[link_args = "-lsmpeg"]
        pub extern {
            fn SMPEG_new(file: *c_char, info: *SMPEG_Info, sdl_audio: c_int) -> *SMPEG;
            fn SMPEG_new_descr(file: c_int, info: *SMPEG_Info, sdl_audio: c_int) -> *SMPEG;
            fn SMPEG_new_data(data: *c_void, size: c_int, info: *SMPEG_Info,
                              sdl_audio: c_int) -> *SMPEG;
            fn SMPEG_new_rwops(src: *SDL_RWops, info: *SMPEG_Info,
                               sdl_audio: c_int) -> *SMPEG;
            fn SMPEG_getinfo(mpeg: *SMPEG, info: *SMPEG_Info);
            fn SMPEG_enableaudio(mpeg: *SMPEG, enable: c_int);
            fn SMPEG_enablevideo(mpeg: *SMPEG, enable: c_int);
            fn SMPEG_delete(mpeg: *SMPEG);
            fn SMPEG_status(mpeg: *SMPEG) -> SMPEGstatus;
            fn SMPEG_setvolume(mpeg: *SMPEG, volume: c_int);
            // XXX SDL_Mutex and SMPEG_DisplayCallback unimplemented
            fn SMPEG_setdisplay(mpeg: *SMPEG, dst: *SDL_Surface,
                                surfLock: *c_void, callback: *c_void);
            fn SMPEG_loop(mpeg: *SMPEG, repeat: c_int);
            fn SMPEG_scaleXY(mpeg: *SMPEG, width: c_int, height: c_int);
            fn SMPEG_scale(mpeg: *SMPEG, scale: c_int);
            fn SMPEG_move(mpeg: *SMPEG, x: c_int, y: c_int);
            fn SMPEG_setdisplayregion(mpeg: *SMPEG, x: c_int, y: c_int, w: c_int, h: c_int);
            fn SMPEG_play(mpeg: *SMPEG);
            fn SMPEG_pause(mpeg: *SMPEG);
            fn SMPEG_stop(mpeg: *SMPEG);
            fn SMPEG_rewind(mpeg: *SMPEG);
            fn SMPEG_seek(mpeg: *SMPEG, bytes: c_int);
            fn SMPEG_skip(mpeg: *SMPEG, seconds: c_float);
            fn SMPEG_renderFrame(mpeg: *SMPEG, framenum: c_int);
            fn SMPEG_renderFinal(mpeg: *SMPEG, dst: *SDL_Surface, x: c_int, y: c_int);
            // XXX SMPEG_Filter unimplemented
            fn SMPEG_filter(mpeg: *SMPEG, filter: *c_void) -> *c_void;
            fn SMPEG_error(mpeg: *SMPEG) -> *c_char;
            fn SMPEG_playAudio(mpeg: *SMPEG, stream: *u8, len: c_int) -> c_int;
            fn SMPEG_playAudioSDL(mpeg: *c_void, stream: *u8, len: c_int) -> c_int;
            fn SMPEG_wantedSpec(mpeg: *SMPEG, wanted: *SDL_AudioSpec) -> c_int;
            fn SMPEG_actualSpec(mpeg: *SMPEG, spec: *SDL_AudioSpec);
        }
    }

    pub struct MPEG {
        pub raw: *ll::SMPEG
    }

    fn wrap_mpeg(raw: *ll::SMPEG) -> ~MPEG {
        ~MPEG { raw: raw }
    }

    impl Drop for MPEG {
        pub fn finalize(&self) {
            unsafe { ll::SMPEG_delete(self.raw); }
        }
    }

    pub impl MPEG {
        fn from_path(path: &Path) -> Result<~MPEG, ~str> {
            let raw = unsafe {
                do str::as_c_str(path.to_str()) |buf| {
                    ll::SMPEG_new(buf, ptr::null(), 0)
                }
            };

            if raw.is_null() { Err(::sdl::get_error()) }
            else { Ok(wrap_mpeg(raw)) }
        }

        fn status(&self) -> SMPEGstatus {
            unsafe { ll::SMPEG_status(self.raw) }
        }

        fn set_volume(&self, volume: int) {
            unsafe { ll::SMPEG_setvolume(self.raw, volume as c_int); }
        }

        fn set_display(&self, surface: &Surface) {
            unsafe {
                ll::SMPEG_setdisplay(self.raw, surface.raw, ptr::null(), ptr::null());
            }
        }

        fn enable_video(&self, enable: bool) {
            unsafe { ll::SMPEG_enablevideo(self.raw, enable as c_int); }
        }

        fn enable_audio(&self, enable: bool) {
            unsafe { ll::SMPEG_enableaudio(self.raw, enable as c_int); }
        }

        fn set_loop(&self, repeat: bool) {
            unsafe { ll::SMPEG_loop(self.raw, repeat as c_int); }
        }

        fn resize(&self, width: int, height: int) {
            unsafe { ll::SMPEG_scaleXY(self.raw, width as c_int, height as c_int); }
        }

        fn scale_by(&self, scale: int) {
            unsafe { ll::SMPEG_scale(self.raw, scale as c_int); }
        }

        fn move(&self, x: int, y: int) {
            unsafe { ll::SMPEG_move(self.raw, x as c_int, y as c_int); }
        }

        fn set_display_region(&self, x: int, y: int, w: int, h: int) {
            unsafe {
                ll::SMPEG_setdisplayregion(self.raw, x as c_int, y as c_int,
                                           w as c_int, h as c_int);
            }
        }

        fn play(&self) {
            unsafe { ll::SMPEG_play(self.raw); }
        }

        fn pause(&self) {
            unsafe { ll::SMPEG_pause(self.raw); }
        }

        fn stop(&self) {
            unsafe { ll::SMPEG_stop(self.raw); }
        }

        fn rewind(&self) {
            unsafe { ll::SMPEG_rewind(self.raw); }
        }

        fn seek(&self, bytes: int) {
            unsafe { ll::SMPEG_seek(self.raw, bytes as c_int); }
        }

        fn skip(&self, seconds: float) {
            unsafe { ll::SMPEG_skip(self.raw, seconds as c_float); }
        }

        fn get_error(&self) -> ~str {
            unsafe {
                let cstr = ll::SMPEG_error(self.raw);
                str::raw::from_c_str(cast::transmute(&cstr))
            }
        }
    }
}

