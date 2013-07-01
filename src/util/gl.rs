// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! OpenGL helpers.

use sdl::video;
use gl = opengles::gl2;
use opengles::gl2::{GLenum, GLint, GLuint, GLsizei, GLfloat};

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

pub impl AttribLoc {
    /// Enables the use of vertex attribute array.
    fn enable_array(&self) {
        gl::enable_vertex_attrib_array(**self as GLuint);
    }

    /// Disables the use of vertex attribute array.
    fn disable_array(&self) {
        gl::disable_vertex_attrib_array(**self as GLuint);
    }

    /// Specifies that the vertex attribute is an array of `size` f32 values, which start at
    /// the byte offset of `offset`, `offset+stride`, `offset+stride*2` etc. (`stride` is set to
    /// the size of each value when given 0.)
    fn define_pointer_f32(&self, size: GLint, normalized: bool, stride: GLsizei, offset: GLuint) {
        gl::vertex_attrib_pointer_f32(**self as GLuint, size, normalized, stride, offset);
    }

    /// Same as `define_pointer_f32` but the array consists of `size` i8 values instead.
    fn define_pointer_i8(&self, size: GLint, normalized: bool, stride: GLsizei, offset: GLuint) {
        gl::vertex_attrib_pointer_i8(**self as GLuint, size, normalized, stride, offset);
    }

    /// Same as `define_pointer_f32` but the array consists of `size` i32 values instead.
    fn define_pointer_i32(&self, size: GLint, normalized: bool, stride: GLsizei, offset: GLuint) {
        gl::vertex_attrib_pointer_i32(**self as GLuint, size, normalized, stride, offset);
    }

    /// Same as `define_pointer_f32` but the array consists of `size` u8 values instead.
    fn define_pointer_u8(&self, size: GLint, normalized: bool, stride: GLsizei, offset: GLuint) {
        gl::vertex_attrib_pointer_u8(**self as GLuint, size, normalized, stride, offset);
    }
}

/// Location for uniform variables (shared additional data used by the shader).
pub struct UniformLoc(GLint);

pub impl UniformLoc {
    /// Sets a uniform variable to a float.
    fn set_1f(&self, x: GLfloat) { gl::uniform_1f(**self, x); }
    /// Sets a uniform variable to a 2D vector of floats. (e.g. coordinates)
    fn set_2f(&self, x: GLfloat, y: GLfloat) { gl::uniform_2f(**self, x, y); }
    /// Sets a uniform variable to a 3D vector of floats. (e.g. RGB colors)
    fn set_3f(&self, x: GLfloat, y: GLfloat, z: GLfloat) { gl::uniform_3f(**self, x, y, z); }
    /// Sets a uniform variable to a 4D vector of floats. (e.g. RGBA colors)
    fn set_4f(&self, x: GLfloat, y: GLfloat, z: GLfloat, w: GLfloat) {
        gl::uniform_4f(**self, x, y, z, w);
    }

    /// Sets a uniform variable to an integer. (e.g. samplers)
    fn set_1i(&self, x: GLint) { gl::uniform_1i(**self, x); }
    /// Sets a uniform variable to a 2D vector of integers.
    fn set_2i(&self, x: GLint, y: GLint) { gl::uniform_2i(**self, x, y); }
    /// Sets a uniform variable to a 3D vector of integers.
    fn set_3i(&self, x: GLint, y: GLint, z: GLint) { gl::uniform_3i(**self, x, y, z); }
    /// Sets a uniform variable to a 4D vector of integers.
    fn set_4i(&self, x: GLint, y: GLint, z: GLint, w: GLint) { gl::uniform_4i(**self, x, y, z, w); }

    /// Sets a uniform variable to a 2x2 matrix of floats. If `transpose` is set the matrix is row
    /// major, otherwise it's column major.
    fn set_matrix_2f(&self, transpose: bool, value: &[GLfloat]) {
        gl::uniform_matrix_2fv(**self, transpose, value);
    }
    /// Sets a uniform variable to a 3x3 matrix of floats. If `transpose` is set the matrix is row
    /// major, otherwise it's column major.
    fn set_matrix_3f(&self, transpose: bool, value: &[GLfloat]) {
        gl::uniform_matrix_3fv(**self, transpose, value);
    }
    /// Sets a uniform variable to a 4x4 matrix of floats. If `transpose` is set the matrix is row
    /// major, otherwise it's column major.
    fn set_matrix_4f(&self, transpose: bool, value: &[GLfloat]) {
        gl::uniform_matrix_4fv(**self, transpose, value);
    }
}

pub impl Shader {
    /// Creates a shader from given string.
    fn from_str(shader_type: ShaderType, code: &str) -> Result<Shader,~str> {
        let shader = gl::create_shader(shader_type as GLenum);
        if shader == 0 {
            return Err(fmt!("GL error 0x%x", gl::get_error() as uint));
        }

        gl::shader_source(shader, ["#version 100\n".to_bytes(), code.to_bytes()]);
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
    fn from_file(shader_type: ShaderType, path: &Path) -> Result<Shader,~str> {
        do io::file_reader(path).chain |f| {
            Shader::from_str(shader_type, f.read_c_str())
        }
    }
}

impl Drop for Shader {
    fn finalize(&self) {
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

pub impl Program {
    /// Creates a new program from given vertex and fragment shader.
    fn new(vertex_shader: Shader, fragment_shader: Shader) -> Result<Program,~str> {
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
    fn attrib_location(&self, attr: &str) -> Result<AttribLoc,~str> {
        let loc = gl::get_attrib_location(self.index, attr.to_owned());
        if loc == -1 { return Err(fmt!("no attrib location: %s", attr)); }
        Ok(AttribLoc(loc))
    }

    /// Returns a location for given uniform variable name.
    fn uniform_location(&self, uniform: &str) -> Result<UniformLoc,~str> {
        let loc = gl::get_uniform_location(self.index, uniform.to_owned());
        if loc == -1 { return Err(fmt!("no uniform location: %s", uniform)); }
        Ok(UniformLoc(loc))
    }

    /// Activates the use of given program. This replaces the currently active program if any.
    fn bind(&self) {
        gl::use_program(self.index);
    }
}

pub struct ProgramForShades {
    program: Program,
    vertex_position: AttribLoc,
    color: AttribLoc,
    local_transform: UniformLoc,
    projection: UniformLoc,
}

pub impl ProgramForShades {
    fn new() -> Result<ProgramForShades,~str> {
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

    fn bind(&self) {
        self.program.bind();
    }
}

pub struct ProgramForTextures {
    program: Program,
    vertex_position: AttribLoc,
    texture_coord: AttribLoc,
    local_transform: UniformLoc,
    projection: UniformLoc,
    sampler: UniformLoc,
}

pub impl ProgramForTextures {
    fn new() -> Result<ProgramForTextures,~str> {
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

    fn bind(&self) {
        self.program.bind();
    }
}

/// Internally used pixel format for opaque textures.
static RGB_PIXEL_FORMAT: &'static video::PixelFormat = &video::PixelFormat {
    palette: None, bpp: 24,
    r_loss: 0, g_loss: 0, b_loss: 0, a_loss: 0,
    r_shift: 0, g_shift: 8, b_shift: 16, a_shift: 0,
    r_mask: 0x000000ff, g_mask: 0x0000ff00, b_mask: 0x00ff0000, a_mask: 0,
    color_key: 0, alpha: 255,
};

/// Internally used pixel format for transparent textures.
static RGBA_PIXEL_FORMAT: &'static video::PixelFormat = &video::PixelFormat {
    palette: None, bpp: 32,
    r_loss: 0, g_loss: 0, b_loss: 0, a_loss: 0,
    r_shift: 0, g_shift: 8, b_shift: 16, a_shift: 24,
    r_mask: 0x000000ff, g_mask: 0x0000ff00, b_mask: 0x00ff0000, a_mask: 0xff000000,
    color_key: 0, alpha: 255,
};

/// Converts the surface to the suitable format for OpenGL. In particular, color key is converted
/// to alpha channel. Deallocates the original surface as needed.
fn prepare_surface_for_texture(surface: ~video::Surface)
                            -> Result<~video::Surface,(~video::Surface,~str)> {
    let format = unsafe { &*(*surface.raw).format };
    if format.BitsPerPixel != 24 && format.BitsPerPixel != 32 {
        let hasalpha = unsafe { ((*surface.raw).flags & video::SrcColorKey as u32) != 0 ||
                                format.Amask != 0 };

        let newfmt = if hasalpha {RGBA_PIXEL_FORMAT} else {RGB_PIXEL_FORMAT};
        match surface.convert(newfmt, [video::SWSurface]) {
            Ok(surface_) => Ok(surface_),
            Err(err) => Err((surface, err))
        }
    } else {
        Ok(surface)
    }
}

/// Texture.
pub struct Texture {
    /// OpenGL reference to the texture.
    index: GLuint,
    /// Intrinsic width of the texture. Not affected by calls to `upload_surface`.
    width: uint,
    /// Intrinsic height of the texture. Not affected by calls to `upload_surface`.
    height: uint,
}

impl Drop for Texture {
    fn finalize(&self) {
        gl::delete_textures(&[self.index]);
    }
}

pub impl Texture {
    fn new(width: uint, height: uint) -> Result<Texture,~str> {
        let texture = gl::gen_textures(1)[0];
        Ok(Texture { index: texture, width: width, height: height })
    }

    fn from_owned_surface(surface: ~video::Surface,
                          xwrap: bool, ywrap: bool) -> Result<Texture,~str> {
        let (width, height) = surface.get_size();
        let texture = earlyexit!(Texture::new(width as uint, height as uint));
        match texture.upload_surface(surface, xwrap, ywrap) {
            Ok(_surface) => Ok(texture),
            Err((_surface,err)) => Err(err)
        }
    }

    fn bind(&self, target: int) {
        let texenum = gl::TEXTURE0 + target as GLenum;
        gl::active_texture(texenum);
        gl::bind_texture(gl::TEXTURE_2D, self.index);
    }

    fn upload_surface(&self, surface: ~video::Surface, xwrap: bool, ywrap: bool) ->
                                    Result<~video::Surface,(~video::Surface,~str)> {
        let surface = earlyexit!(prepare_surface_for_texture(surface));
        let bpp = unsafe { (*(*surface.raw).format).BitsPerPixel };
        let glpixfmt = if bpp == 24 {gl::RGB} else {gl::RGBA};
        let (width, height) = surface.get_size();

        gl::bind_texture(gl::TEXTURE_2D, self.index);
        do surface.with_lock |pixels| {
            let pixels: &[u8] = unsafe { cast::transmute(pixels) };
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

pub struct Drawing {
    vbo: GLuint,
}

pub impl Drawing {
    fn new() -> Drawing {
        let vbo = gl::gen_buffers(1)[0];
        Drawing { vbo: vbo }
    }

    fn shaded(&self, program: &ProgramForShades, prim: GLenum,
              f: &fn(add: &fn(x: f32, y: f32, c: video::Color))) {
        let mut vertices: ~[(f32,f32,u8,u8,u8,u8)] = ~[];
        let add = |x: f32, y: f32, c: video::Color| {
            let (r, g, b, a) = match c {
                video::RGB(r,g,b) => (r, g, b, 255),
                video::RGBA(r,g,b,a) => (r, g, b, a)
            };
            vertices.push((x, y, r, g, b, a));
        };

        f(add);

        program.bind();
        gl::bind_buffer(gl::ARRAY_BUFFER, self.vbo);

        // TODO there is no `offsetof` or similar
        let rowsize = sys::size_of::<(f32,f32,u8,u8,u8,u8)>() as GLint;
        let coloroffset = sys::size_of::<(f32,f32)>() as GLuint;
        program.vertex_position.define_pointer_f32(2, false, rowsize, 0);
        program.color.define_pointer_u8(4, true, rowsize, coloroffset);

        gl::buffer_data(gl::ARRAY_BUFFER, vertices, gl::DYNAMIC_DRAW);
        gl::draw_arrays(prim, 0, vertices.len() as GLsizei);
    }

    fn textured(&self, program: &ProgramForTextures, texture: &Texture, prim: GLenum,
                f: &fn(add: &fn(x: f32, y: f32, s: f32, t: f32))) {
        let mut vertices: ~[(f32,f32,f32,f32)] = ~[];
        let width_recip = 1.0 / (texture.width as f32);
        let height_recip = 1.0 / (texture.height as f32);
        let add = |x: f32, y: f32, s: f32, t: f32| {
            vertices.push((x, y, s * width_recip, t * height_recip));
        };

        f(add);

        program.bind();
        gl::bind_buffer(gl::ARRAY_BUFFER, self.vbo);

        // TODO there is no `offsetof` or similar
        let rowsize = sys::size_of::<(f32,f32,f32,f32)>() as GLint;
        let texoffset = sys::size_of::<(f32,f32)>() as GLuint;
        program.vertex_position.define_pointer_f32(2, false, rowsize, 0);
        program.texture_coord.define_pointer_f32(2, false, rowsize, texoffset);
        program.sampler.set_1i(0);
        texture.bind(0);

        gl::buffer_data(gl::ARRAY_BUFFER, vertices, gl::DYNAMIC_DRAW);
        gl::draw_arrays(prim, 0, vertices.len() as GLsizei);
    }
}

impl Drop for Drawing {
    fn finalize(&self) {
        gl::delete_buffers([self.vbo]);
    }
}

