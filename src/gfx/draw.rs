// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! 2D drawing routines for OpenGL.

use gfx::color::{Color, to_rgba};
use gfx::gl::{Texture, Buffer};
use gfx::gl::{Shader, VertexShader, FragmentShader};
use gfx::gl::{Program, UniformLoc, AttribLoc};
use gl = opengles::gl2;
use opengles::gl2::{GLenum, GLint, GLuint, GLsizei};

/// OpenGL program for shaded triangles. See also `ShadedDrawing`.
pub struct ProgramForShades {
    /// Compiled and linked program.
    program: Program,
    /// Vertex position attribute.
    vertex_position: AttribLoc,
    /// Color attribute.
    color: AttribLoc,
    /// Local transform (i.e. three-dimensional Affine transform) uniform.
    local_transform: UniformLoc,
    /// Projection (i.e. four-dimensional 2D projection) uniform.
    projection: UniformLoc,
}

impl ProgramForShades {
    /// Compiles and links the program.
    pub fn new() -> Result<ProgramForShades,~str> {
        let vertex_code = stringify!(
            attribute vec2 vertex_position_in;
            attribute vec4 color_in;
            uniform mat3 local_transform;
            uniform mat4 projection;
            varying vec4 color;
            void main(void) {
                gl_Position =
                    projection * vec4(local_transform * vec3(vertex_position_in, 1.0), 1.0);
                color = color_in;
            }
        );
        let fragment_code = stringify!(
            precision mediump float;
            varying vec4 color;
            void main(void) {
                gl_FragColor = color;
            }
        );

        let vertex_shader = earlyexit!(Shader::from_str(VertexShader, vertex_code));
        let fragment_shader = earlyexit!(Shader::from_str(FragmentShader, fragment_code));
        let program = earlyexit!(Program::new(vertex_shader, fragment_shader));

        let vertex_position = earlyexit!(program.attrib_location("vertex_position_in"));
        let color = earlyexit!(program.attrib_location("color_in"));
        let local_transform = earlyexit!(program.uniform_location("local_transform"));
        let projection = earlyexit!(program.uniform_location("projection"));

        vertex_position.enable_array();
        color.enable_array();

        Ok(ProgramForShades {
            program: program,
            vertex_position: vertex_position,
            color: color,
            local_transform: local_transform,
            projection: projection,
        })
    }

    /// Binds the program to the current OpenGL context.
    pub fn bind(&self) {
        self.program.bind();
    }
}

/// OpenGL program for textured triangles. See also `TexturedDrawing`.
pub struct ProgramForTextures {
    /// Compiled and linked program.
    program: Program,
    /// Vertex position attribute.
    vertex_position: AttribLoc,
    /// Texture coordinate attribute.
    texture_coord: AttribLoc,
    /// Color attribute.
    color: AttribLoc,
    /// Local transform (i.e. three-dimensional Affine transform) uniform.
    local_transform: UniformLoc,
    /// Projection (i.e. four-dimensional 2D projection) uniform.
    projection: UniformLoc,
    /// Texture uniform.
    sampler: UniformLoc,
}

impl ProgramForTextures {
    /// Compiles and links the program.
    pub fn new() -> Result<ProgramForTextures,~str> {
        let vertex_code = stringify!(
            attribute vec2 vertex_position_in;
            attribute vec2 texture_coord_in;
            attribute vec4 color_in;
            uniform mat3 local_transform;
            uniform mat4 projection;
            varying vec2 texture_coord;
            varying vec4 color;
            void main(void) {
                gl_Position =
                    projection * vec4(local_transform * vec3(vertex_position_in, 1.0), 1.0);
                texture_coord = texture_coord_in;
                color = color_in;
            }
        );
        let fragment_code = stringify!(
            precision mediump float;
            varying vec2 texture_coord;
            varying vec4 color;
            uniform sampler2D sampler;
            void main(void) {
                gl_FragColor = texture2D(sampler, vec2(texture_coord.s, texture_coord.t)) * color;
            }
        );

        let vertex_shader = earlyexit!(Shader::from_str(VertexShader, vertex_code));
        let fragment_shader = earlyexit!(Shader::from_str(FragmentShader, fragment_code));
        let program = earlyexit!(Program::new(vertex_shader, fragment_shader));

        let vertex_position = earlyexit!(program.attrib_location("vertex_position_in"));
        let texture_coord = earlyexit!(program.attrib_location("texture_coord_in"));
        let color = earlyexit!(program.attrib_location("color_in"));
        let local_transform = earlyexit!(program.uniform_location("local_transform"));
        let projection = earlyexit!(program.uniform_location("projection"));
        let sampler = earlyexit!(program.uniform_location("sampler"));

        vertex_position.enable_array();
        texture_coord.enable_array();
        color.enable_array();

        Ok(ProgramForTextures {
            program: program,
            vertex_position: vertex_position,
            texture_coord: texture_coord,
            color: color,
            local_transform: local_transform,
            projection: projection,
            sampler: sampler,
        })
    }

    /// Binds the program to the current OpenGL context.
    pub fn bind(&self) {
        self.program.bind();
    }
}

/// A state for non-immediate shaded rendering. This is not intended for the general use, see
/// `ui::screen::Screen::draw_shaded` for the initial invocation.
pub struct ShadedDrawing {
    prim: GLenum,
    vertices: ~[(f32,f32,u8,u8,u8,u8)],
}

impl ShadedDrawing {
    /// Creates a new state.
    pub fn new(prim: GLenum) -> ShadedDrawing {
        ShadedDrawing { prim: prim, vertices: ~[] }
    }

    /// Draws specified primitives. The suitable program and scratch VBO should be supplied.
    pub fn draw_prim(~self, program: &ProgramForShades, buffer: &Buffer) {
        program.bind();
        buffer.bind();

        // TODO there is no `offsetof` or similar
        let rowsize = ::std::sys::size_of::<(f32,f32,u8,u8,u8,u8)>() as GLint;
        let coloroffset = ::std::sys::size_of::<(f32,f32)>() as GLuint;
        program.vertex_position.define_pointer_f32(2, false, rowsize, 0);
        program.color.define_pointer_u8(4, true, rowsize, coloroffset);

        buffer.upload(self.vertices, gl::DYNAMIC_DRAW);
        gl::draw_arrays(self.prim, 0, self.vertices.len() as GLsizei);
    }
}

/// Common trait for shaded rendering interface.
pub trait ShadedDrawingTraits {
    /// Adds a point to the state, with a direct RGBA color.
    fn point_rgba(&mut self, x: f32, y: f32, rgba: (u8,u8,u8,u8));

    /// Adds a point to the state.
    fn point(&mut self, x: f32, y: f32, c: Color) {
        self.point_rgba(x, y, to_rgba(c));
    }

    /// Adds two endpoints of a line segment to the state, with direct RGBA colors.
    fn line_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                 rgba1: (u8,u8,u8,u8), rgba2: (u8,u8,u8,u8));

    /// Adds two endpoints of a line segment to the state, with an uniform color.
    fn line(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, c: Color) {
        let rgba = to_rgba(c);
        self.line_rgba(x1, y1, x2, y2, rgba, rgba);
    }

    /// Adds three corner points of a triangle to the state, with direct RGBA colors.
    fn triangle_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32,
                     rgba1: (u8,u8,u8,u8), rgba2: (u8,u8,u8,u8), rgba3: (u8,u8,u8,u8));

    /// Adds three corner points of a triangle to the state, with an uniform color.
    fn triangle(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32, c: Color) {
        let rgba = to_rgba(c);
        self.triangle_rgba(x1, y1, x2, y2, x3, y3, rgba, rgba, rgba);
    }

    /// Adds four points of a rectangle to the state, with direct RGBA colors.
    fn rect_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, rgba11: (u8,u8,u8,u8),
                 rgba12: (u8,u8,u8,u8), rgba21: (u8,u8,u8,u8), rgba22: (u8,u8,u8,u8));

    /// Adds four points of a rectangle to the state, with a horizontal gradient.
    fn rect_horiz(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, cx1: Color, cx2: Color) {
        let rgbax1 = to_rgba(cx1);
        let rgbax2 = to_rgba(cx2);
        self.rect_rgba(x1, y1, x2, y2, rgbax1, rgbax2, rgbax1, rgbax2);
    }

    /// Adds four points of a rectangle to the state, with a vertical gradient.
    fn rect_vert(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, cy1: Color, cy2: Color) {
        let rgbay1 = to_rgba(cy1);
        let rgbay2 = to_rgba(cy2);
        self.rect_rgba(x1, y1, x2, y2, rgbay1, rgbay1, rgbay2, rgbay2);
    }

    /// Adds four points of a rectangle to the state, with an uniform color.
    fn rect(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, c: Color) {
        let rgba = to_rgba(c);
        self.rect_rgba(x1, y1, x2, y2, rgba, rgba, rgba, rgba);
    }
}

impl ShadedDrawingTraits for ShadedDrawing {
    fn point_rgba(&mut self, x: f32, y: f32, (r, g, b, a): (u8,u8,u8,u8)) {
        self.vertices.push((x, y, r, g, b, a));
    }

    fn line_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                 (r1, g1, b1, a1): (u8,u8,u8,u8), (r2, g2, b2, a2): (u8,u8,u8,u8)) {
        self.vertices.push_all([(x1, y1, r1, g1, b1, a1), (x2, y2, r2, g2, b2, a2)]);
    }

    fn triangle_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32,
                     (r1, g1, b1, a1): (u8,u8,u8,u8), (r2, g2, b2, a2): (u8,u8,u8,u8),
                     (r3, g3, b3, a3): (u8,u8,u8,u8)) {
        self.vertices.push_all([(x1, y1, r1, g1, b1, a1), (x2, y2, r2, g2, b2, a2),
                                (x3, y3, r3, g3, b3, a3)]);
    }

    fn rect_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                 (r11, g11, b11, a11): (u8,u8,u8,u8), (r12, g12, b12, a12): (u8,u8,u8,u8),
                 (r21, g21, b21, a21): (u8,u8,u8,u8), (r22, g22, b22, a22): (u8,u8,u8,u8)) {
        if self.prim == gl::LINES || self.prim == gl::TRIANGLES {
            self.vertices.push_all([(x1, y1, r11, g11, b11, a11), (x1, y2, r12, g12, b12, a12),
                                    (x2, y2, r22, g22, b22, a22), (x1, y1, r11, g11, b11, a11),
                                    (x2, y2, r22, g22, b22, a22), (x2, y1, r21, g21, b21, a21)]);
        } else {
            self.vertices.push_all([(x1, y1, r11, g11, b11, a11), (x1, y2, r12, g12, b12, a12),
                                    (x2, y2, r22, g22, b22, a22), (x2, y1, r21, g21, b21, a21)]);
        }
    }
}

/// A state for non-immediate textured rendering. This is not intended for the general use, see
/// `ui::screen::Screen::draw_textured` for the initial invocation.
pub struct TexturedDrawing {
    width_recip: f32,
    height_recip: f32,
    prim: GLenum,
    vertices: ~[(f32,f32,f32,f32,u8,u8,u8,u8)],
}

impl TexturedDrawing {
    /// Creates a new state. A texture is required for pixel specification.
    pub fn new(prim: GLenum, texture: &Texture) -> TexturedDrawing {
        let width_recip = 1.0 / (texture.width as f32);
        let height_recip = 1.0 / (texture.height as f32);
        TexturedDrawing { width_recip: width_recip, height_recip: height_recip,
                          prim: prim, vertices: ~[] }
    }

    /// Draws specified primitives with given texture. The suitable program and scratch VBO
    /// should be supplied.
    pub fn draw_prim(~self, program: &ProgramForTextures, buffer: &Buffer, texture: &Texture) {
        program.bind();
        buffer.bind();

        // TODO there is no `offsetof` or similar
        let rowsize = ::std::sys::size_of::<(f32,f32,f32,f32,u8,u8,u8,u8)>() as GLint;
        let texoffset = ::std::sys::size_of::<(f32,f32)>() as GLuint;
        let coloroffset = texoffset + ::std::sys::size_of::<(f32,f32)>() as GLuint;
        program.vertex_position.define_pointer_f32(2, false, rowsize, 0);
        program.texture_coord.define_pointer_f32(2, false, rowsize, texoffset);
        program.color.define_pointer_u8(4, true, rowsize, coloroffset);
        program.sampler.set_1i(0);
        texture.bind(0);

        buffer.upload(self.vertices, gl::DYNAMIC_DRAW);
        gl::draw_arrays(self.prim, 0, self.vertices.len() as GLsizei);
    }
}

/// Common trait for textured rendering interface.
pub trait TexturedDrawingTraits {
    /// Returns a reciprocal of texture sizes, for normalizing texture coordinates to [0,1].
    fn size_recip(&self) -> (f32, f32);

    /// Adds a point to the state, with direct texture coordinates and a color.
    fn point_st_rgba(&mut self, x: f32, y: f32, st: (f32,f32), rgba: (u8,u8,u8,u8));

    /// Adds a point to the state, with direct texture coordinates.
    fn point_st(&mut self, x: f32, y: f32, st: (f32,f32)) {
        self.point_st_rgba(x, y, st, (255,255,255,255));
    }

    /// Adds a point to the state.
    fn point(&mut self, x: f32, y: f32, s: f32, t: f32) {
        let (width_recip, height_recip) = self.size_recip();
        self.point_st(x, y, (s * width_recip, t * height_recip));
    }

    /// Adds four points of a rectangle to the state, with direct texture coordinates and a color.
    fn rect_st_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, st1: (f32,f32), st2: (f32,f32),
                    rgba: (u8,u8,u8,u8));

    /// Adds four points of a rectangle to the state, with direct texture coordinates.
    fn rect_st(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, st1: (f32,f32), st2: (f32,f32)) {
        self.rect_st_rgba(x1, y1, x2, y2, st1, st2, (255,255,255,255));
    }

    /// Adds four points of a rectangle to the state, with a specified pixel area and color.
    fn rect_area_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                      s1: f32, t1: f32, s2: f32, t2: f32, rgba: (u8,u8,u8,u8)) {
        let (width_recip, height_recip) = self.size_recip();
        self.rect_st_rgba(x1, y1, x2, y2, (s1 * width_recip, t1 * height_recip),
                          (s2 * width_recip, t2 * height_recip), rgba);
    }

    /// Adds four points of a rectangle to the state, with a specified pixel area.
    fn rect_area(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                 s1: f32, t1: f32, s2: f32, t2: f32) {
        let (width_recip, height_recip) = self.size_recip();
        self.rect_st(x1, y1, x2, y2, (s1 * width_recip, t1 * height_recip),
                     (s2 * width_recip, t2 * height_recip));
    }

    /// Adds four points of a rectangle to the state, where the whole texture is used for that
    /// rectangle. A color still applies.
    fn rect_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, rgba: (u8,u8,u8,u8)) {
        self.rect_st_rgba(x1, y1, x2, y2, (0.0, 0.0), (1.0, 1.0), rgba);
    }

    /// Adds four points of a rectangle to the state, where the whole texture is used for that
    /// rectangle.
    fn rect(&mut self, x1: f32, y1: f32, x2: f32, y2: f32) {
        self.rect_st(x1, y1, x2, y2, (0.0, 0.0), (1.0, 1.0));
    }
}

impl TexturedDrawingTraits for TexturedDrawing {
    fn size_recip(&self) -> (f32, f32) {
        (self.width_recip, self.height_recip)
    }

    fn point_st_rgba(&mut self, x: f32, y: f32, (s, t): (f32,f32), (r, g, b, a): (u8,u8,u8,u8)) {
        self.vertices.push((x, y, s, t, r, g, b, a));
    }

    fn rect_st_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                    (s1, t1): (f32,f32), (s2, t2): (f32,f32), (r, g, b, a): (u8,u8,u8,u8)) {
        if self.prim == gl::LINES || self.prim == gl::TRIANGLES {
            self.vertices.push_all([(x1, y1, s1, t1, r, g, b, a), (x1, y2, s1, t2, r, g, b, a),
                                    (x2, y2, s2, t2, r, g, b, a), (x1, y1, s1, t1, r, g, b, a),
                                    (x2, y2, s2, t2, r, g, b, a), (x2, y1, s2, t1, r, g, b, a)]);
        } else {
            self.vertices.push_all([(x1, y1, s1, t1, r, g, b, a), (x1, y2, s1, t2, r, g, b, a),
                                    (x2, y2, s2, t2, r, g, b, a), (x2, y1, s2, t1, r, g, b, a)]);
        }
    }
}

