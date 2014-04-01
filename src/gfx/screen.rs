// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Abstracted graphical screen.

use std::rc::Rc;
use std::slice::MutableCloneableVector;
use sdl::video;
use gl = opengles::gl2;

use gfx::color::Blend;
use gfx::gl::{VertexBuffer, Texture2D, FrameBuffer};
use gfx::draw::{ProgramForShades, ShadedDrawing, ShadedDrawingTraits};
use gfx::draw::{ProgramForTextures, TexturedDrawing, TexturedDrawingTraits};
use gfx::bmfont::{Font, FontDrawingUtils, Alignment};

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
impl GLState {
    /// Creates a new OpenGL state from the current SDL window.
    pub fn new() -> Result<GLState,~str> {
        use ext::win32::ll::*;

        macro_rules! return_on_err(
            ($e:expr) => {
                match $e {
                    Ok(v) => v,
                    Err(err) => { return Err(format!("EGL error 0x{:x}", err)); }
                }
            }
        )

        // we need to preload this before initializing EGL
        "d3dcompiler_43.dll".to_c_str().with_ref(|dllname| {
            unsafe { LoadLibraryA(dllname); }
        });

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
        let config = configs.as_slice()[0];

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
    pub fn uses_sdl_ogl_support() -> bool { false }

    /// Swap the buffers if the double buffering is enabled. Do nothing otherwise.
    pub fn swap_buffers(&self) {
        let _ = egl::swap_buffers(self.egl_display, self.egl_surface);
    }
}

#[cfg(target_os="win32")]
impl Drop for GLState {
    fn drop(&mut self) {
        match egl::terminate(self.egl_display) {
            Ok(()) => {}
            Err(err) => fail!(format!("EGL error 0x{:x}", err))
        }
    }
}

/// OpenGL state. This corresponds to EGL context in Windows; in the other platforms the global SDL
/// context handles this so there is no additional state.
#[cfg(not(target_os="win32"))]
pub struct GLState;

#[cfg(not(target_os="win32"))]
impl GLState {
    /// Creates a new OpenGL state from the current SDL window.
    pub fn new() -> Result<GLState,~str> { Ok(GLState) }
    /// Returns true if SDL's OpenGL support is in use.
    pub fn uses_sdl_ogl_support() -> bool { true }
    /// Swap the buffers if the double buffering is enabled. Do nothing otherwise.
    pub fn swap_buffers(&self) { video::swap_buffers(); }
}

/// Abstracted graphical screen.
pub struct Screen {
    /// Screen width.
    pub width: uint,
    /// Screen height.
    pub height: uint,
    /// SDL surface returned by `sdl::video::set_video_mode`.
    pub sdl_surface: ~video::Surface,
    /// OpenGL state if required.
    glstate: GLState,
    /// Shared vertex buffer object for drawing.
    pub vertexbuf: VertexBuffer,
    /// OpenGL program for non-textured triangles.
    pub program_for_shades: ProgramForShades,
    /// OpenGL program for textured triangles.
    pub program_for_textures: ProgramForTextures,
    /// The last viewport set via `set_viewport`.
    last_viewport: (gl::GLint, gl::GLint, gl::GLsizei, gl::GLsizei),
    /// The last projection matrix set via `set_projection_*`.
    last_projection: [gl::GLfloat, ..16],
    /// The last local transform matrix set via `set_local_transform`.
    last_local_transform: [gl::GLfloat, ..9],
    /// Shared bitmap font.
    pub font: Rc<Font>,
}

impl Screen {
    /// Creates a new screen with given parameters.
    pub fn new(width: uint, height: uint, fullscreen: bool) -> Result<Screen,~str> {
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

        let screen = try!(video::set_video_mode(width as int, height as int, 32,
                                                surfaceflags, videoflags));
        let glstate = try!(GLState::new());

        gl::enable(gl::BLEND);
        gl::blend_func(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
        gl::clear_color(0.0, 0.0, 0.0, 1.0);

        // initialize shaders
        let program_for_shades = try!(ProgramForShades::new());
        let program_for_textures = try!(ProgramForTextures::new());
        program_for_textures.bind();
        program_for_textures.sampler.set_1i(0);

        let mut screen = Screen { width: width, height: height, sdl_surface: screen,
                                  glstate: glstate, vertexbuf: VertexBuffer::new(),
                                  program_for_shades: program_for_shades,
                                  program_for_textures: program_for_textures,
                                  last_viewport: (0, 0, 0, 0), last_projection: [0.0, ..16],
                                  last_local_transform: [0.0, ..9],
                                  font: Rc::new(Font::new()) };
        screen.set_local_transform([1.0, 0.0, 0.0,
                                    0.0, 1.0, 0.0,
                                    0.0, 0.0, 1.0]);
        screen.set_viewport(0, 0, width as gl::GLsizei, height as gl::GLsizei);
        screen.set_projection_ortho(0.0, width as f32, 0.0, height as f32);
        Ok(screen)
    }

    /// Sets the local transform matrix. `matrix` should be a 3x3 row-major matrix.
    pub fn set_local_transform(&mut self, matrix: &[gl::GLfloat]) {
        assert!(matrix.len() == 9);
        self.program_for_shades.bind();
        self.program_for_shades.local_transform.set_matrix_3f(false, matrix);
        self.program_for_textures.bind();
        self.program_for_textures.local_transform.set_matrix_3f(false, matrix);
        self.last_local_transform.mut_slice(0, 9).copy_from(matrix);
    }

    /// Sets the viewport, which translates [-1,1]x[-1,1] coordinates into the window coordinates.
    pub fn set_viewport(&mut self, left: gl::GLint, top: gl::GLint,
                        width: gl::GLsizei, height: gl::GLsizei) {
        gl::viewport(left, top, width, height);
        self.last_viewport = (left, top, width, height);
    }

    /// Sets the projection matrix. `matrix` should be a 4x4 row-major matrix.
    pub fn set_projection(&mut self, matrix: &[gl::GLfloat]) {
        assert!(matrix.len() == 16);
        self.program_for_shades.bind();
        self.program_for_shades.projection.set_matrix_4f(false, matrix);
        self.program_for_textures.bind();
        self.program_for_textures.projection.set_matrix_4f(false, matrix);
        self.last_projection.mut_slice(0, 16).copy_from(matrix);
    }

    /// Sets the orthographic projection matrix with given 2D bounds.
    /// Far and near bounds (z-index) are implicitly set to [1,-1].
    pub fn set_projection_ortho(&mut self, left: f32, right: f32, top: f32, bottom: f32) {
        let (l, r, t, b, f, n) = (left, right, top, bottom, 1.0, -1.0);
        self.set_projection([
            2.0/(r-l),    0.0,          0.0,          0.0,
            0.0,          2.0/(t-b),    0.0,          0.0,
            0.0,          0.0,          -2.0/(f-n),   0.0,
            -(r+l)/(r-l), -(t+b)/(t-b), -(f+n)/(f-n), 1.0,
        ]);
    }

    /// Temporarily saves the current viewport, projection and local transform matrixes
    /// before the block, and restores them at the end of the block.
    pub fn locally(&mut self, f: |&mut Screen|) {
        let (saved_vl, saved_vt, saved_vw, saved_vh) = self.last_viewport;
        let saved_projection = self.last_projection;
        let saved_local_transform = self.last_local_transform;
        f(self);
        self.set_viewport(saved_vl, saved_vt, saved_vw, saved_vh);
        self.set_projection(saved_projection.as_slice());
        self.set_local_transform(saved_local_transform.as_slice());
    }

    /// Swap the buffers if the double buffering is enabled. Do nothing otherwise.
    pub fn swap_buffers(&self) {
        self.glstate.swap_buffers();
    }

    /// Clears the whole screen.
    pub fn clear(&self) {
        gl::clear(gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT);
    }

    /// Sets the scissor box within given block.
    /// Any draw operations inside the block will be clipped according to given scissor box.
    pub fn scissor(&self, x: int, y: int, w: uint, h: uint, f: ||) {
        assert!(!gl::is_enabled(gl::SCISSOR_TEST));
        gl::enable(gl::SCISSOR_TEST);
        gl::scissor(x as gl::GLint, y as gl::GLint, w as gl::GLsizei, h as gl::GLsizei);
        f();
        gl::disable(gl::SCISSOR_TEST);
    }

    /// Draws shaded primitives to the screen. The block receives a mutable reference to
    /// `util::gl::ShadedDrawing`, to which it should add points.
    pub fn draw_shaded_prim(&mut self, prim: gl::GLenum, f: |&mut ShadedDrawing|) {
        let mut drawing = ShadedDrawing::new(prim);
        f(&mut drawing);
        drawing.draw_to(self);
    }

    /// Same as `draw_shaded_prim` but with a default font.
    pub fn draw_shaded_prim_with_font(&mut self, prim: gl::GLenum, f: |&mut ShadedFontDrawing|) {
        let mut drawing = ShadedFontDrawing::new(prim, self.font.clone());
        f(&mut drawing);
        drawing.draw_to(self);
    }

    /// Draws shaded triangles to the screen. The block receives a mutable reference to
    /// `util::gl::ShadedDrawing`, to which it should add points.
    pub fn draw_shaded(&mut self, f: |&mut ShadedDrawing|) {
        self.draw_shaded_prim(gl::TRIANGLES, f)
    }

    /// Same as `draw_shaded` but with a default font.
    pub fn draw_shaded_with_font(&mut self, f: |&mut ShadedFontDrawing|) {
        self.draw_shaded_prim_with_font(gl::TRIANGLES, f)
    }

    /// Draws textured primitives to the screen. The block receives a mutable reference to
    /// `util::gl::TexturedDrawing`, to which it should add points.
    pub fn draw_textured_prim(&mut self, prim: gl::GLenum, texture: &Texture2D,
                              f: |&mut TexturedDrawing|) {
        let mut drawing = TexturedDrawing::new(prim, texture);
        f(&mut drawing);
        drawing.draw_texture_to(self, texture);
    }

    /// Draws textured triangles to the screen. The block receives a mutable reference to
    /// `util::gl::TexturedDrawing`, to which it should add points.
    pub fn draw_textured(&mut self, texture: &Texture2D, f: |&mut TexturedDrawing|) {
        self.draw_textured_prim(gl::TRIANGLES, texture, f)
    }

    /// Renders to given frame buffer. The frame buffer should be complete.
    /// The viewport is temporarily reset to dimensions of the frame buffer in given block.
    pub fn render_to_framebuffer(&mut self, framebuf: &FrameBuffer, f: |&mut Screen|) {
        assert!(framebuf.complete());
        self.locally(|screen| {
            framebuf.bind();
            screen.set_viewport(0, 0, framebuf.width as gl::GLsizei,
                                      framebuf.height as gl::GLsizei);
            screen.set_projection_ortho(0.0, framebuf.width as f32, framebuf.height as f32, 0.0);
            f(screen);
            FrameBuffer::unbind();
        })
    }
}

/// Things that can be drawn to the screen at a time.
/// This trait implies the knowledge about the internals of screen (e.g. shaders).
pub trait ScreenDraw {
    /// Draws the thing into the screen.
    fn draw_to(self, screen: &mut Screen);
}

/// Same to `ScreenDraw` but expects the texture.
pub trait ScreenTexturedDraw {
    /// Draws the thing into the screen with given texture.
    fn draw_texture_to(self, screen: &mut Screen, texture: &Texture2D);
}

/// Analogue to `ShadedDrawing` with font drawing interfaces.
pub struct ShadedFontDrawing {
    drawing: ShadedDrawing,
    font: Rc<Font>,
}

impl ShadedFontDrawing {
    /// Creates a new state.
    pub fn new(prim: gl::GLenum, font: Rc<Font>) -> ShadedFontDrawing {
        assert!(prim == gl::TRIANGLES, "only triangles are supported for ShadedFontDrawing");
        ShadedFontDrawing { drawing: ShadedDrawing::new(prim), font: font }
    }

    /// Draws a glyph with given position and color (possibly gradient). This method is
    /// distinct from `glyph` since the glyph #95 is used for the tick marker
    /// (character code -1 in C).
    pub fn glyph<ColorT:Blend>(&mut self, x: f32, y: f32, zoom: f32, glyph: uint, color: ColorT) {
        self.drawing.glyph(self.font.deref(), x, y, zoom, glyph, color)
    }

    /// Draws a character with given position and color.
    pub fn char<ColorT:Blend>(&mut self, x: f32, y: f32, zoom: f32, c: char, color: ColorT) {
        self.drawing.char(self.font.deref(), x, y, zoom, c, color)
    }

    /// Draws a string with given position, alignment and color.
    pub fn string<ColorT:Blend>(&mut self, x: f32, y: f32, zoom: f32,
                                align: Alignment, s: &str, color: ColorT) {
        self.drawing.string(self.font.deref(), x, y, zoom, align, s, color)
    }
}

impl ShadedDrawingTraits for ShadedFontDrawing {
    fn point_rgba(&mut self, x: f32, y: f32, rgba: (u8,u8,u8,u8)) {
        self.drawing.point_rgba(x, y, rgba)
    }
    fn line_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                 rgba1: (u8,u8,u8,u8), rgba2: (u8,u8,u8,u8)) {
        self.drawing.line_rgba(x1, y1, x2, y2, rgba1, rgba2)
    }
    fn triangle_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32,
                     rgba1: (u8,u8,u8,u8), rgba2: (u8,u8,u8,u8), rgba3: (u8,u8,u8,u8)) {
        self.drawing.triangle_rgba(x1, y1, x2, y2, x3, y3, rgba1, rgba2, rgba3)
    }
    fn rect_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, rgba11: (u8,u8,u8,u8),
                 rgba12: (u8,u8,u8,u8), rgba21: (u8,u8,u8,u8), rgba22: (u8,u8,u8,u8)) {
        self.drawing.rect_rgba(x1, y1, x2, y2, rgba11, rgba12, rgba21, rgba22)
    }
    fn draw_prim(self, program: &ProgramForShades, vertexbuf: &VertexBuffer) {
        self.drawing.draw_prim(program, vertexbuf);
    }
}

impl<T:ShadedDrawingTraits> ScreenDraw for T {
    fn draw_to(self, screen: &mut Screen) {
        self.draw_prim(&screen.program_for_shades, &screen.vertexbuf);
    }
}

impl<T:TexturedDrawingTraits> ScreenTexturedDraw for T {
    fn draw_texture_to(self, screen: &mut Screen, texture: &Texture2D) {
        self.draw_prim(&screen.program_for_textures, &screen.vertexbuf, texture);
    }
}

