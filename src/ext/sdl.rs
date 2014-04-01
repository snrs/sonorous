// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Extensions to rust-sdl.

#[cfg(target_os = "win32")]
pub mod syswm {
    use std::libc::{HANDLE, INVALID_HANDLE_VALUE};

    pub mod ll {
        #![allow(non_camel_case_types)]

        use std::libc::{HANDLE, c_int};

        pub struct SDL_version {
            pub major: u8,
            pub minor: u8,
            pub patch: u8
        }

        impl SDL_version {
            pub fn new() -> SDL_version {
                SDL_version { major: 1, minor: 2, patch: 14 }
            }
        }

        pub struct SDL_SysWMinfo {
            pub version: SDL_version,
            pub window: HANDLE,
            pub hglrc: HANDLE
        }

        extern {
            pub fn SDL_GetWMInfo(info: *mut SDL_SysWMinfo) -> c_int;
        }
    }

    pub fn get_wm_info() -> Option<ll::SDL_SysWMinfo> {
        let mut wminfo = ll::SDL_SysWMinfo {
            version: ll::SDL_version::new(),
            window: INVALID_HANDLE_VALUE as HANDLE,
            hglrc: INVALID_HANDLE_VALUE as HANDLE,
        };
        unsafe {
            if ll::SDL_GetWMInfo(&mut wminfo) == 0 {
                None
            } else {
                Some(wminfo)
            }
        }
    }
}

