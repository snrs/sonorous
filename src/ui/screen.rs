// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Abstracted graphical screen.

use sdl::video;
use gl = opengles::gl2;
use glutil = util::gl;

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
            (egl::RED_SIZE, 8),
            (egl::GREEN_SIZE, 8),
            (egl::BLUE_SIZE, 8),
            (egl::ALPHA_SIZE, 8),
            (egl::DEPTH_SIZE, 16),
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

    /// Returns true if SDL's OpenGL support is in use.
    fn uses_sdl_ogl_support() -> bool { false }

    /// Swap the buffers if the double buffering is enabled. Do nothing otherwise.
    fn swap_buffers(&self) {
        egl::swap_buffers(self.egl_display, self.egl_surface);
    }
}

#[cfg(target_os="win32")]
impl Drop for GLState {
    fn finalize(&self) {
        match egl::terminate(self.egl_display) {
            Ok(()) => {}
            Err(err) => fail!(fmt!("EGL error 0x%x", err as uint))
        }
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
    /// Returns true if SDL's OpenGL support is in use.
    fn uses_sdl_ogl_support() -> bool { true }
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
    glstate: GLState,
    /// Shared vertex buffer object for drawing.
    buffer: glutil::Buffer,
    /// OpenGL program for non-textured triangles.
    program_for_shades: glutil::ProgramForShades,
    /// OpenGL program for textured triangles.
    program_for_textures: glutil::ProgramForTextures,
}

pub impl Screen {
    /// Creates a new screen with given parameters.
    fn new(width: uint, height: uint, fullscreen: bool) -> Result<Screen,~str> {
        let mut surfaceflags;
        let mut videoflags;
        if fullscreen {
            surfaceflags = ~[];
            videoflags = ~[video::Fullscreen];
        } else {
            surfaceflags = ~[video::SWSurface];
            videoflags = ~[video::DoubleBuf];
        }
        if GLState::uses_sdl_ogl_support() {
            // SDL_OPENGL flag cannot be used in Windows as ANGLE should own the screen context.
            videoflags.push(video::OpenGL);
        }

        let screen = earlyexit!(video::set_video_mode(width as int, height as int, 32,
                                                      surfaceflags, videoflags));
        let glstate = earlyexit!(GLState::new());

        gl::enable(gl::BLEND);
        gl::blend_func(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        gl::clear_color(0.0, 0.0, 0.0, 0.0);

        let l = 0.0, r = width as f32, t = 0.0, b = height as f32, f = 1.0, n = -1.0;
        let projection = [
            2.0/(r-l),    0.0,          0.0,          0.0,
            0.0,          2.0/(t-b),    0.0,          0.0,
            0.0,          0.0,          -2.0/(f-n),   0.0,
            -(r+l)/(r-l), -(t+b)/(t-b), -(f+n)/(f-n), 1.0,
        ];
        let local_transform = [
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0,
        ];

        // initialize shaders
        let program_for_shades = earlyexit!(glutil::ProgramForShades::new());
        let program_for_textures = earlyexit!(glutil::ProgramForTextures::new());
        program_for_shades.bind();
        program_for_shades.projection.set_matrix_4f(false, projection);
        program_for_shades.local_transform.set_matrix_3f(false, local_transform);
        program_for_textures.bind();
        program_for_textures.projection.set_matrix_4f(false, projection);
        program_for_textures.local_transform.set_matrix_3f(false, local_transform);
        program_for_textures.sampler.set_1i(0);

        Ok(Screen { width: width, height: height, sdl_surface: screen,
                    glstate: glstate, buffer: glutil::Buffer::new(),
                    program_for_shades: program_for_shades,
                    program_for_textures: program_for_textures })
    }

    /// Swap the buffers if the double buffering is enabled. Do nothing otherwise.
    fn swap_buffers(&self) { self.glstate.swap_buffers(); }

    /// Clears the whole screen.
    fn clear(&self) {
        gl::clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);
    }

    /// Draws shaded triangles to the screen. The block receives a mutable reference to
    /// `util::gl::ShadedDrawing`, to which it should add points.
    fn draw_shaded(&self, prim: gl::GLenum, f: &fn(&mut glutil::ShadedDrawing)) {
        let mut drawing = ~glutil::ShadedDrawing::new();
        f(&mut *drawing);
        drawing.draw_prim(&self.program_for_shades, &self.buffer, prim);
    }

    /// Draws textured triangles to the screen. The block receives a mutable reference to
    /// `util::gl::TexturedDrawing`, to which it should add points.
    fn draw_textured(&self, prim: gl::GLenum, texture: &glutil::Texture,
                     f: &fn(&mut glutil::TexturedDrawing)) {
        let mut drawing = ~glutil::TexturedDrawing::new(texture);
        f(&mut *drawing);
        drawing.draw_prim(&self.program_for_textures, &self.buffer, texture, prim);
    }
}

