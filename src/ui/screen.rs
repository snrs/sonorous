// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Abstracted graphical screen.

use sdl::video;

#[cfg(target_os="win32")] use opengles::egl;
#[cfg(target_os="win32")] use ext::sdl::syswm;

/// OpenGL state. This corresponds to EGL context in Windows; in the other platforms the global SDL
/// context handles this so there is no additional state.
#[cfg(target_os="win32")]
pub struct GLState {
    egl_display: egl::Display,
    egl_surface: egl::Surface,
    egl_context: egl::Context,
}

#[cfg(target_os="win32")]
pub impl GLState {
    /// Creates a new OpenGL state from the current SDL window.
    fn new() -> Result<GLState,~str> {
        use ext::win32::ll::*;

        macro_rules! return_on_err(
            ($e:expr) => {
                match $e {
                    Ok(v) => v,
                    Err(err) => { return Err(fmt!("EGL error 0x%x", err as uint)); }
                }
            }
        )

        // we need to preload this before initializing EGL
        do str::as_c_str("d3dcompiler_43.dll") |dllname| {
            unsafe { LoadLibraryA(dllname); }
        }

        let hwnd = match syswm::get_wm_info() {
            Some(wminfo) => wminfo.window,
            None => { return Err(~"SDL_GetWMInfo failed"); }
        };
        let hdc = unsafe { GetDC(hwnd) };
        let display = return_on_err!(egl::get_display(hdc));
        return_on_err!(egl::initialize(display));

        let configattrs = [
            (egl::RED_SIZE, 5),
            (egl::GREEN_SIZE, 6),
            (egl::BLUE_SIZE, 5),
            (egl::ALPHA_SIZE, egl::DONT_CARE),
            (egl::DEPTH_SIZE, egl::DONT_CARE),
            (egl::STENCIL_SIZE, egl::DONT_CARE),
            (egl::SAMPLE_BUFFERS, 0),
        ];
        let configs = return_on_err!(egl::get_configs(display, configattrs, Some(1)));
        if configs.is_empty() {
            return Err(~"no suitable EGL configs available");
        }
        let config = configs[0];

        let surfaceattrs = [
            // none
        ];
        let surface = return_on_err!(egl::create_window_surface(display, config, hwnd,
                                                                surfaceattrs));

        let contextattrs = [
            (egl::CONTEXT_CLIENT_VERSION, 2),
        ];
        let context = return_on_err!(egl::create_context(display, config, None, contextattrs));

        return_on_err!(egl::make_current(display, surface, surface, context));
        Ok(GLState { egl_display: display, egl_surface: surface, egl_context: context })
    }

    /// Swap the buffers if the double buffering is enabled. Do nothing otherwise.
    fn swap_buffers(&self) {
        egl::swap_buffers(self.egl_display, self.egl_surface);
    }
}

/// OpenGL state. This corresponds to EGL context in Windows; in the other platforms the global SDL
/// context handles this so there is no additional state.
#[cfg(not(target_os="win32"))]
pub struct GLState;

#[cfg(not(target_os="win32"))]
pub impl GLState {
    /// Creates a new OpenGL state from the current SDL window.
    fn new() -> Result<GLState,~str> { Ok(GLState) }
    /// Swap the buffers if the double buffering is enabled. Do nothing otherwise.
    fn swap_buffers(&self) { video::swap_buffers(); }
}

/// Abstracted graphical screen.
pub struct Screen {
    /// Screen width.
    width: uint,
    /// Screen height.
    height: uint,
    /// SDL surface returned by `sdl::video::set_video_mode`.
    sdl_surface: ~video::Surface,
    /// OpenGL state if required.
    glstate: GLState
}

pub impl Screen {
    /// Creates a new screen with given parameters.
    fn new(width: uint, height: uint, fullscreen: bool) -> Result<Screen,~str> {
        let surfaceflags, videoflags;
        if fullscreen {
            surfaceflags = &[];
            videoflags = &[video::Fullscreen];
        } else {
            surfaceflags = &[video::SWSurface];
            videoflags = &[video::DoubleBuf];
        }

        do video::set_video_mode(width as int, height as int, 32,
                                 surfaceflags, videoflags).chain |screen| {
            // Rust: double `chain` doesn't work here. (#4654?)
            match GLState::new() {
                Ok(glstate) => Ok(Screen { width: width, height: height,
                                           sdl_surface: screen, glstate: glstate }),
                Err(err) => Err(err)
            }
        }
    }

    /// Swap the buffers if the double buffering is enabled. Do nothing otherwise.
    fn swap_buffers(&self) { self.glstate.swap_buffers(); }
}

