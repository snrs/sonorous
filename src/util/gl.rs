// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! OpenGL helpers.

use sdl::video;
use gl = opengles::gl2;
use opengles::gl2::{GLenum, GLint, GLuint, GLsizei, GLfloat};
use util::gfx::to_rgba;

/// OpenGL shader types. This closely follows OpenGL ES API, which means that geometry shaders are
/// absent.
#[deriving(Eq)]
pub enum ShaderType {
    /// Vertex shader.
    VertexShader = gl::VERTEX_SHADER as int,
    /// Fragment shader.
    FragmentShader = gl::FRAGMENT_SHADER as int,
}

/// Compiled shader. Automatically deleted when finalized.
#[deriving(Eq)]
pub struct Shader {
    /// Shader type.
    shader_type: ShaderType,
    /// OpenGL reference to the shader.
    index: GLuint,
}

/// Location for attributes (per-vertex additional data used by the shader).
pub struct AttribLoc(GLint);

impl AttribLoc {
    /// Enables the use of vertex attribute array.
    pub fn enable_array(&self) {
        gl::enable_vertex_attrib_array(**self as GLuint);
    }

    /// Disables the use of vertex attribute array.
    pub fn disable_array(&self) {
        gl::disable_vertex_attrib_array(**self as GLuint);
    }

    /// Specifies that the vertex attribute is an array of `size` f32 values, which start at
    /// the byte offset of `offset`, `offset+stride`, `offset+stride*2` etc. (`stride` is set to
    /// the size of each value when given 0.)
    pub fn define_pointer_f32(&self, size: GLint, normalized: bool,
                              stride: GLsizei, offset: GLuint) {
        gl::vertex_attrib_pointer_f32(**self as GLuint, size, normalized, stride, offset);
    }

    /// Same as `define_pointer_f32` but the array consists of `size` i8 values instead.
    pub fn define_pointer_i8(&self, size: GLint, normalized: bool,
                             stride: GLsizei, offset: GLuint) {
        gl::vertex_attrib_pointer_i8(**self as GLuint, size, normalized, stride, offset);
    }

    /// Same as `define_pointer_f32` but the array consists of `size` i32 values instead.
    pub fn define_pointer_i32(&self, size: GLint, normalized: bool,
                              stride: GLsizei, offset: GLuint) {
        gl::vertex_attrib_pointer_i32(**self as GLuint, size, normalized, stride, offset);
    }

    /// Same as `define_pointer_f32` but the array consists of `size` u8 values instead.
    pub fn define_pointer_u8(&self, size: GLint, normalized: bool,
                             stride: GLsizei, offset: GLuint) {
        gl::vertex_attrib_pointer_u8(**self as GLuint, size, normalized, stride, offset);
    }
}

/// Location for uniform variables (shared additional data used by the shader).
pub struct UniformLoc(GLint);

impl UniformLoc {
    /// Sets a uniform variable to a float.
    pub fn set_1f(&self, x: GLfloat) { gl::uniform_1f(**self, x); }
    /// Sets a uniform variable to a 2D vector of floats. (e.g. coordinates)
    pub fn set_2f(&self, x: GLfloat, y: GLfloat) { gl::uniform_2f(**self, x, y); }
    /// Sets a uniform variable to a 3D vector of floats. (e.g. RGB colors)
    pub fn set_3f(&self, x: GLfloat, y: GLfloat, z: GLfloat) { gl::uniform_3f(**self, x, y, z); }
    /// Sets a uniform variable to a 4D vector of floats. (e.g. RGBA colors)
    pub fn set_4f(&self, x: GLfloat, y: GLfloat, z: GLfloat, w: GLfloat) {
        gl::uniform_4f(**self, x, y, z, w);
    }

    /// Sets a uniform variable to an integer. (e.g. samplers)
    pub fn set_1i(&self, x: GLint) { gl::uniform_1i(**self, x); }
    /// Sets a uniform variable to a 2D vector of integers.
    pub fn set_2i(&self, x: GLint, y: GLint) { gl::uniform_2i(**self, x, y); }
    /// Sets a uniform variable to a 3D vector of integers.
    pub fn set_3i(&self, x: GLint, y: GLint, z: GLint) { gl::uniform_3i(**self, x, y, z); }
    /// Sets a uniform variable to a 4D vector of integers.
    pub fn set_4i(&self, x: GLint, y: GLint, z: GLint, w: GLint) {
        gl::uniform_4i(**self, x, y, z, w);
    }

    /// Sets a uniform variable to a 2x2 matrix of floats. If `transpose` is set the matrix is row
    /// major, otherwise it's column major.
    pub fn set_matrix_2f(&self, transpose: bool, value: &[GLfloat]) {
        gl::uniform_matrix_2fv(**self, transpose, value);
    }
    /// Sets a uniform variable to a 3x3 matrix of floats. If `transpose` is set the matrix is row
    /// major, otherwise it's column major.
    pub fn set_matrix_3f(&self, transpose: bool, value: &[GLfloat]) {
        gl::uniform_matrix_3fv(**self, transpose, value);
    }
    /// Sets a uniform variable to a 4x4 matrix of floats. If `transpose` is set the matrix is row
    /// major, otherwise it's column major.
    pub fn set_matrix_4f(&self, transpose: bool, value: &[GLfloat]) {
        gl::uniform_matrix_4fv(**self, transpose, value);
    }
}

impl Shader {
    /// Creates a shader from given string.
    pub fn from_str(shader_type: ShaderType, code: &str) -> Result<Shader,~str> {
        let shader = gl::create_shader(shader_type as GLenum);
        if shader == 0 {
            return Err(fmt!("GL error 0x%x", gl::get_error() as uint));
        }

        gl::shader_source(shader, ["#version 100\n".as_bytes().to_owned(),
                                   code.as_bytes().to_owned()]);
        gl::compile_shader(shader);
        if gl::get_shader_iv(shader, gl::COMPILE_STATUS) != 0 {
            Ok(Shader { shader_type: shader_type, index: shader })
        } else {
            let ret = Err(gl::get_shader_info_log(shader));
            gl::delete_shader(shader);
            ret
        }
    }

    /// Creates a shader from given file path.
    pub fn from_file(shader_type: ShaderType, path: &Path) -> Result<Shader,~str> {
        do ::std::io::file_reader(path).chain |f| {
            Shader::from_str(shader_type, f.read_c_str())
        }
    }
}

impl Drop for Shader {
    fn drop(&self) {
        gl::delete_shader(self.index);
    }
}

/// Compiled and linked program, which contains one vertex shader and one fragment shader.
/// A program owns and manages both kinds of shaders.
#[deriving(Eq)]
pub struct Program {
    vertex: Shader,
    fragment: Shader,
    index: GLuint,
}

impl Program {
    /// Creates a new program from given vertex and fragment shader.
    pub fn new(vertex_shader: Shader, fragment_shader: Shader) -> Result<Program,~str> {
        assert!(vertex_shader.shader_type == VertexShader);
        assert!(fragment_shader.shader_type == FragmentShader);

        let program = gl::create_program();
        gl::attach_shader(program, vertex_shader.index);
        gl::attach_shader(program, fragment_shader.index);

        gl::link_program(program);
        if gl::get_program_iv(program, gl::LINK_STATUS) != 0 {
            Ok(Program { vertex: vertex_shader, fragment: fragment_shader, index: program })
        } else {
            let ret = Err(gl::get_program_info_log(program));
            gl::detach_shader(program, vertex_shader.index);
            gl::detach_shader(program, fragment_shader.index);
            gl::delete_program(program);
            ret
        }
    }

    /// Returns a location for given attribute name.
    pub fn attrib_location(&self, attr: &str) -> Result<AttribLoc,~str> {
        let loc = gl::get_attrib_location(self.index, attr.to_owned());
        if loc == -1 { return Err(fmt!("no attrib location: %s", attr)); }
        Ok(AttribLoc(loc))
    }

    /// Returns a location for given uniform variable name.
    pub fn uniform_location(&self, uniform: &str) -> Result<UniformLoc,~str> {
        let loc = gl::get_uniform_location(self.index, uniform.to_owned());
        if loc == -1 { return Err(fmt!("no uniform location: %s", uniform)); }
        Ok(UniformLoc(loc))
    }

    /// Activates the use of given program. This replaces the currently active program if any.
    pub fn bind(&self) {
        gl::use_program(self.index);
    }
}

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
            uniform mat3 local_transform;
            uniform mat4 projection;
            varying vec2 texture_coord;
            void main(void) {
                gl_Position =
                    projection * vec4(local_transform * vec3(vertex_position_in, 1.0), 1.0);
                texture_coord = texture_coord_in;
            }
        );
        let fragment_code = stringify!(
            precision mediump float;
            varying vec2 texture_coord;
            uniform sampler2D sampler;
            void main(void) {
                gl_FragColor = texture2D(sampler, vec2(texture_coord.s, texture_coord.t));
            }
        );

        let vertex_shader = earlyexit!(Shader::from_str(VertexShader, vertex_code));
        let fragment_shader = earlyexit!(Shader::from_str(FragmentShader, fragment_code));
        let program = earlyexit!(Program::new(vertex_shader, fragment_shader));

        let vertex_position = earlyexit!(program.attrib_location("vertex_position_in"));
        let texture_coord = earlyexit!(program.attrib_location("texture_coord_in"));
        let local_transform = earlyexit!(program.uniform_location("local_transform"));
        let projection = earlyexit!(program.uniform_location("projection"));
        let sampler = earlyexit!(program.uniform_location("sampler"));

        vertex_position.enable_array();
        texture_coord.enable_array();

        Ok(ProgramForTextures {
            program: program,
            vertex_position: vertex_position,
            texture_coord: texture_coord,
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

/// Internally used pixel format (three bytes R, G, B for one pixel) for opaque textures.
#[cfg(target_endian="little")]
static RGB_PIXEL_FORMAT: &'static video::PixelFormat = &video::PixelFormat {
    palette: None, bpp: 24,
    r_loss: 0, g_loss: 0, b_loss: 0, a_loss: 0,
    r_shift: 0, g_shift: 8, b_shift: 16, a_shift: 0,
    r_mask: 0x000000ff, g_mask: 0x0000ff00, b_mask: 0x00ff0000, a_mask: 0,
    color_key: 0, alpha: 255,
};

/// Internally used pixel format (three bytes R, G, B for one pixel) for opaque textures.
#[cfg(target_endian="big")]
static RGB_PIXEL_FORMAT: &'static video::PixelFormat = &video::PixelFormat {
    palette: None, bpp: 24,
    r_loss: 0, g_loss: 0, b_loss: 0, a_loss: 0,
    r_shift: 16, g_shift: 8, b_shift: 0, a_shift: 0,
    r_mask: 0x00ff0000, g_mask: 0x0000ff00, b_mask: 0x000000ff, a_mask: 0,
    color_key: 0, alpha: 255,
};

/// Internally used pixel format (four bytes R, G, B, A for one pixel) for transparent textures.
#[cfg(target_endian="little")]
static RGBA_PIXEL_FORMAT: &'static video::PixelFormat = &video::PixelFormat {
    palette: None, bpp: 32,
    r_loss: 0, g_loss: 0, b_loss: 0, a_loss: 0,
    r_shift: 0, g_shift: 8, b_shift: 16, a_shift: 24,
    r_mask: 0x000000ff, g_mask: 0x0000ff00, b_mask: 0x00ff0000, a_mask: 0xff000000,
    color_key: 0, alpha: 255,
};

/// Internally used pixel format (four bytes R, G, B, A for one pixel) for transparent textures.
#[cfg(target_endian="big")]
static RGBA_PIXEL_FORMAT: &'static video::PixelFormat = &video::PixelFormat {
    palette: None, bpp: 32,
    r_loss: 0, g_loss: 0, b_loss: 0, a_loss: 0,
    r_shift: 24, g_shift: 16, b_shift: 8, a_shift: 0,
    r_mask: 0xff000000, g_mask: 0x00ff0000, b_mask: 0x0000ff00, a_mask: 0x000000ff,
    color_key: 0, alpha: 255,
};

/// Converts the surface to the suitable format for OpenGL. In particular, color key is converted
/// to alpha channel. Deallocates the original surface as needed.
fn prepare_surface_for_texture(surface: ~video::Surface)
                            -> Result<~video::Surface,(~video::Surface,~str)> {
    let format = unsafe { &*(*surface.raw).format };

    let prepared =
        if format.BitsPerPixel == 24 {
            // we still have to ensure that it has a proper byte order
            format.Rmask == RGB_PIXEL_FORMAT.r_mask && format.Gmask == RGB_PIXEL_FORMAT.g_mask &&
            format.Bmask == RGB_PIXEL_FORMAT.b_mask && format.Amask == RGB_PIXEL_FORMAT.a_mask
        } else if format.BitsPerPixel == 32 {
            format.Rmask == RGBA_PIXEL_FORMAT.r_mask && format.Gmask == RGBA_PIXEL_FORMAT.g_mask &&
            format.Bmask == RGBA_PIXEL_FORMAT.b_mask && format.Amask == RGBA_PIXEL_FORMAT.a_mask
        } else {
            false
        };
    if prepared { return Ok(surface); }

    let hasalpha = unsafe { ((*surface.raw).flags & video::SrcColorKey as u32) != 0 ||
                            format.Amask != 0 };
    let newfmt = if hasalpha {RGBA_PIXEL_FORMAT} else {RGB_PIXEL_FORMAT};
    match surface.convert(newfmt, [video::SWSurface]) {
        Ok(surface_) => Ok(surface_),
        Err(err) => Err((surface, err))
    }
}

/// OpenGL texture.
pub struct Texture {
    /// OpenGL reference to the texture.
    index: GLuint,
    /// Intrinsic width of the texture. Not affected by calls to `upload_surface`.
    width: uint,
    /// Intrinsic height of the texture. Not affected by calls to `upload_surface`.
    height: uint,
}

impl Drop for Texture {
    fn drop(&self) {
        gl::delete_textures(&[self.index]);
    }
}

impl Texture {
    /// Creates a new texture with given intrinsic dimension, which is only used for convenience
    /// in `*Drawing` interfaces.
    pub fn new(width: uint, height: uint) -> Result<Texture,~str> {
        let texture = gl::gen_textures(1)[0];
        Ok(Texture { index: texture, width: width, height: height })
    }

    /// Creates a new texture from an SDL surface, which is destroyed after the upload. `xwrap` and
    /// `ywrap` specifies whether the texture should wrap in horizontal or vertical directions or
    /// not.
    pub fn from_owned_surface(surface: ~video::Surface,
                              xwrap: bool, ywrap: bool) -> Result<Texture,~str> {
        let (width, height) = surface.get_size();
        let texture = earlyexit!(Texture::new(width as uint, height as uint));
        match texture.upload_surface(surface, xwrap, ywrap) {
            Ok(_surface) => Ok(texture),
            Err((_surface,err)) => Err(err)
        }
    }

    /// Binds the texture to the current OpenGL context.
    pub fn bind(&self, target: int) {
        let texenum = gl::TEXTURE0 + target as GLenum;
        gl::active_texture(texenum);
        gl::bind_texture(gl::TEXTURE_2D, self.index);
    }

    /// Uploads an SDL surface, which may be converted to the suitable format. If the conversion
    /// fails the original surface is returned with an error string. `xwrap` and `ywrap` specifies
    /// whether the texture should wrap in horizontal or vertical directions or not.
    pub fn upload_surface(&self, surface: ~video::Surface, xwrap: bool, ywrap: bool) ->
                                    Result<~video::Surface,(~video::Surface,~str)> {
        let surface = earlyexit!(prepare_surface_for_texture(surface));
        let bpp = unsafe { (*(*surface.raw).format).BitsPerPixel };
        let glpixfmt = if bpp == 24 {gl::RGB} else {gl::RGBA};
        let (width, height) = surface.get_size();

        gl::bind_texture(gl::TEXTURE_2D, self.index);
        do surface.with_lock |pixels| {
            let pixels: &[u8] = unsafe { ::std::cast::transmute(pixels) };
            gl::tex_image_2d(gl::TEXTURE_2D, 0, glpixfmt as GLint, width as GLsizei,
                             height as GLsizei, 0, glpixfmt, gl::UNSIGNED_BYTE, Some(pixels));
        }
        gl::tex_parameter_i(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S,
                            if xwrap {gl::REPEAT} else {gl::CLAMP_TO_EDGE} as GLint);
        gl::tex_parameter_i(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T,
                            if ywrap {gl::REPEAT} else {gl::CLAMP_TO_EDGE} as GLint);
        gl::tex_parameter_i(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as GLint);
        gl::tex_parameter_i(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as GLint);

        Ok(surface)
    }
}

/// OpenGL vertex buffer object.
pub struct Buffer {
    /// OpenGL reference to the VBO.
    index: GLuint,
}

impl Drop for Buffer {
    fn drop(&self) {
        gl::delete_buffers(&[self.index]);
    }
}

impl Buffer {
    /// Creates a new vertex buffer object.
    pub fn new() -> Buffer {
        let vbo = gl::gen_buffers(1)[0];
        Buffer { index: vbo }
    }

    /// Binds the VBO to the current OpenGL context.
    pub fn bind(&self) {
        gl::bind_buffer(gl::ARRAY_BUFFER, self.index);
    }

    /// Uploads a data to the VBO.
    pub fn upload<T>(&self, data: &[T], usage: GLenum) {
        gl::buffer_data(gl::ARRAY_BUFFER, data, usage);
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

    /// Adds a point to the state, with a direct RGBA color.
    pub fn point_rgba(&mut self, x: f32, y: f32, (r, g, b, a): (u8,u8,u8,u8)) {
        self.vertices.push((x, y, r, g, b, a));
    }

    /// Adds a point to the state.
    pub fn point(&mut self, x: f32, y: f32, c: video::Color) {
        self.point_rgba(x, y, to_rgba(c));
    }

    /// Adds a triangle to the state, with an uniform color.
    pub fn triangle(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32,
                    c: video::Color) {
        let (r, g, b, a) = to_rgba(c);
        self.vertices.push_all([(x1, y1, r, g, b, a), (x2, y2, r, g, b, a), (x3, y3, r, g, b, a)]);
    }

    /// Adds four points of a rectangle to the state, with direct RGBA colors.
    pub fn rect_rgba(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
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

    /// Adds four points of a rectangle to the state, with a horizontal gradient.
    pub fn rect_horiz(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                      cx1: video::Color, cx2: video::Color) {
        let rgbax1 = to_rgba(cx1);
        let rgbax2 = to_rgba(cx2);
        self.rect_rgba(x1, y1, x2, y2, rgbax1, rgbax2, rgbax1, rgbax2);
    }

    /// Adds four points of a rectangle to the state, with a vertical gradient.
    pub fn rect_vert(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                     cy1: video::Color, cy2: video::Color) {
        let rgbay1 = to_rgba(cy1);
        let rgbay2 = to_rgba(cy2);
        self.rect_rgba(x1, y1, x2, y2, rgbay1, rgbay1, rgbay2, rgbay2);
    }

    /// Adds four points of a rectangle to the state, with an uniform color.
    pub fn rect(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, c: video::Color) {
        let rgba = to_rgba(c);
        self.rect_rgba(x1, y1, x2, y2, rgba, rgba, rgba, rgba);
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

/// A state for non-immediate textured rendering. This is not intended for the general use, see
/// `ui::screen::Screen::draw_textured` for the initial invocation.
pub struct TexturedDrawing {
    width_recip: f32,
    height_recip: f32,
    prim: GLenum,
    vertices: ~[(f32,f32,f32,f32)],
}

impl TexturedDrawing {
    /// Creates a new state. A texture is required for pixel specification.
    pub fn new(prim: GLenum, texture: &Texture) -> TexturedDrawing {
        let width_recip = 1.0 / (texture.width as f32);
        let height_recip = 1.0 / (texture.height as f32);
        TexturedDrawing { width_recip: width_recip, height_recip: height_recip,
                          prim: prim, vertices: ~[] }
    }

    /// Adds a point to the state, with direct texture coordinates.
    pub fn point_st(&mut self, x: f32, y: f32, (s, t): (f32,f32)) {
        self.vertices.push((x, y, s, t));
    }

    /// Adds a point to the state.
    pub fn point(&mut self, x: f32, y: f32, s: f32, t: f32) {
        self.point_st(x, y, (s * self.width_recip, t * self.height_recip));
    }

    /// Adds four points of a rectangle to the state, with direct texture coordinates.
    pub fn rect_st(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                   (s1, t1): (f32,f32), (s2, t2): (f32,f32)) {
        if self.prim == gl::LINES || self.prim == gl::TRIANGLES {
            self.vertices.push_all([(x1, y1, s1, t1), (x1, y2, s1, t2), (x2, y2, s2, t2),
                                    (x1, y1, s1, t1), (x2, y2, s2, t2), (x2, y1, s2, t1)]);
        } else {
            self.vertices.push_all([(x1, y1, s1, t1), (x1, y2, s1, t2),
                                    (x2, y2, s2, t2), (x2, y1, s2, t1)]);
        }
    }

    /// Adds four points of a rectangle to the state, with a specified pixel area.
    pub fn rect_area(&mut self, x1: f32, y1: f32, x2: f32, y2: f32,
                     s1: f32, t1: f32, s2: f32, t2: f32) {
        self.rect_st(x1, y1, x2, y2,
                     (s1 * self.width_recip, t1 * self.height_recip),
                     (s2 * self.width_recip, t2 * self.height_recip));
    }

    /// Adds four points of a rectangle to the state, where the whole texture is used for that
    /// rectangle.
    pub fn rect(&mut self, x1: f32, y1: f32, x2: f32, y2: f32) {
        self.rect_st(x1, y1, x2, y2, (0.0, 0.0), (1.0, 1.0));
    }

    /// Draws specified primitives with given texture. The suitable program and scratch VBO
    /// should be supplied.
    pub fn draw_prim(~self, program: &ProgramForTextures, buffer: &Buffer, texture: &Texture) {
        program.bind();
        buffer.bind();

        // TODO there is no `offsetof` or similar
        let rowsize = ::std::sys::size_of::<(f32,f32,f32,f32)>() as GLint;
        let texoffset = ::std::sys::size_of::<(f32,f32)>() as GLuint;
        program.vertex_position.define_pointer_f32(2, false, rowsize, 0);
        program.texture_coord.define_pointer_f32(2, false, rowsize, texoffset);
        program.sampler.set_1i(0);
        texture.bind(0);

        buffer.upload(self.vertices, gl::DYNAMIC_DRAW);
        gl::draw_arrays(self.prim, 0, self.vertices.len() as GLsizei);
    }
}

