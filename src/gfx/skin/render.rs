// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Skin renderer.

use std::{f32, num, str, mem};
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::io::{IoResult, MemWriter};
use std::collections::{HashMap, hashmap};

use opengles::gl2 as gl;
use sdl_image;

use gfx::color::{Color, Gradient, RGB, to_rgba};
use gfx::gl::Texture2D;
use gfx::draw::{ShadedDrawing, ShadedDrawingTraits, TexturedDrawing, TexturedDrawingTraits};
use gfx::bmfont::{NCOLUMNS, NROWS, FontDrawingUtils, LeftAligned};
use gfx::screen::{Screen, ShadedFontDrawing, ScreenDraw, ScreenTexturedDraw};
use gfx::skin::scalar::{Scalar, ImageScalar, TextureSource, PathSource, ImageClip, ColorScalar};
use gfx::skin::ast::{Expr, ENum, EScalar, ENeg, EAdd, ESub, EMul, EDiv, Pos, Rect};
use gfx::skin::ast::{Gen, HookGen, TextGen, TextLenGen};
use gfx::skin::ast::{Block, CondBlock, MultiBlock};
use gfx::skin::ast::{ScalarFormat, NoFormat, NumFormat, MsFormat, HmsFormat};
use gfx::skin::ast::{TextSource, ScalarText, StaticText, TextBlock, TextConcat};
use gfx::skin::ast::{ColorSource, ScalarColor, StaticColor, ColorBlock};
use gfx::skin::ast::{GradientSource, FlatColorGradient, ColorGradient, GradientBlock};
use gfx::skin::ast::{Node, Nothing, Debug, ColoredLine, ColoredRect, TexturedRect,
                     Text, Group, Clip, NodeBlock};
use gfx::skin::ast::{Skin};
use gfx::skin::hook::Hook;

/// The currently active draw call.
enum ActiveDraw<'a> {
    /// No draw call active.
    ToBeDrawn,
    /// Shaded drawing with `GL_LINES` primitive.
    ShadedLines(ShadedDrawing),
    /// Shaded drawing with `GL_TRIANGLES` primitive.
    Shaded(ShadedFontDrawing), // TODO it will eventually need font refs
    /// Textured drawing with `GL_TRIANGLES` primitive.
    /// It also stores the reference to the texture for commit.
    Textured(TexturedDrawing, Rc<Texture2D>),
}

/// The skin renderer.
pub struct Renderer {
    /// The current skin data.
    skin: Rc<Skin>,
    /// The cached textures for image scalars. `None` indicates the error without further retries.
    imagecache: HashMap<Path,Option<Rc<Texture2D>>>,
}

impl Renderer {
    /// Creates a new renderer out of the skin data.
    /// The renderer maintains the global cache, thus should be kept as long as possible.
    pub fn new(skin: Skin) -> Renderer {
        Renderer { skin: Rc::new(skin), imagecache: HashMap::new() }
    }

    /// Renders the skin with supplied hook to given screen.
    /// This overwrites to the current screen, no `clear` is called.
    pub fn render(&mut self, screen: &mut Screen, hook: &Hook) {
        let skin = self.skin.clone();
        let state = State::new(self, screen);
        state.nodes(hook, skin.nodes[]);
        state.finish();
    }
}

/// The clipping region used to clip the render and calculate the relative coordinates.
struct ClipRect {
    /// Left coordinate of the clipping region.
    dx: f32,
    /// Top coordinate of the clipping region.
    dy: f32,
    /// The width of the clipping region.
    w: f32,
    /// The height of the clipping region.
    h: f32,
}

/// The internal state for single render.
struct State<'a> {
    /// Parent renderer.
    renderer: RefCell<&'a mut Renderer>,
    /// Screen reference.
    screen: RefCell<&'a mut Screen>,
    /// The active draw call.
    draw: RefCell<ActiveDraw<'a>>,
    /// The current clipping region.
    clip: Cell<ClipRect>,
}

impl<'a> State<'a> {
    /// Creates a new render state, with a clipping region of the whole screen.
    fn new(renderer: &'a mut Renderer, screen: &'a mut Screen) -> State<'a> {
        let initclip = ClipRect { dx: 0.0, dy: 0.0,
                                  w: screen.width as f32, h: screen.height as f32 };
        State { renderer: RefCell::new(renderer), screen: RefCell::new(screen),
                draw: RefCell::new(ToBeDrawn), clip: Cell::new(initclip) }
    }

    /// Evaluates an expression.
    fn expr(&self, hook: &Hook, e: &Expr, reference: f32) -> f32 {
        match *e {
            ENum(ref v) => v.to_num(&reference),
            EScalar(ref id) => match hook.scalar_hook(id.as_slice()) {
                Some(ref scalar) => match scalar.to_f32() {
                    Some(v) => v,
                    None => {
                        warn!("skin: `{id}` is not a numeric scalar hook ({scalar}).",
                              id = id.as_slice(), scalar = *scalar);
                        f32::NAN
                    }
                },
                None => {
                    warn!("skin: `{id}` is not a scalar hook.", id = id.as_slice());
                    f32::NAN
                },
            },
            ENeg(box ref e_) => -self.expr(hook, e_, reference),
            EAdd(box ref lhs, box ref rhs) =>
                self.expr(hook, lhs, reference) + self.expr(hook, rhs, reference),
            ESub(box ref lhs, box ref rhs) =>
                self.expr(hook, lhs, reference) - self.expr(hook, rhs, reference),
            EMul(box ref lhs, box ref rhs) =>
                self.expr(hook, lhs, reference) * self.expr(hook, rhs, reference),
            EDiv(box ref lhs, box ref rhs) =>
                self.expr(hook, lhs, reference) / self.expr(hook, rhs, reference),
        }
    }

    /// Evaluates a position.
    fn pos(&self, hook: &Hook, pos: &Pos) -> (f32, f32) {
        let clip = self.clip.get();
        (clip.dx + self.expr(hook, &pos.x, clip.w),
         clip.dy + self.expr(hook, &pos.y, clip.h))
    }

    /// Evaluates a rectangle.
    fn rect(&self, hook: &Hook, rect: &Rect) -> ((f32, f32), (f32, f32)) {
        (self.pos(hook, &rect.p), self.pos(hook, &rect.q))
    }

    /// Evaluates a color source.
    fn color_source(&self, hook: &Hook, color: &ColorSource) -> Option<Color> {
        match *color {
            ScalarColor(ref id) => match hook.scalar_hook(id.as_slice()) {
                Some(ColorScalar(color)) => Some(color),
                Some(ref scalar) => {
                    warn!("skin: `{id}` is not a color scalar hook ({scalar}).",
                          id = id.as_slice(), scalar = *scalar);
                    None
                },
                _ => {
                    warn!("skin: `{id}` is not a scalar hook.", id = id.as_slice());
                    None
                },
            },
            StaticColor(color) => Some(color),
            ColorBlock(ref block) => {
                let mut ret = None;
                self.block(hook, block, |hook_, color_| {
                    ret = self.color_source(hook_, &**color_);
                    false
                });
                ret
            },
        }
    }

    /// Evaluates a gradient source.
    fn gradient_source(&self, hook: &Hook, gradient: &GradientSource) -> Option<Gradient> {
        match *gradient {
            FlatColorGradient(ref color) => {
                match self.color_source(hook, color) {
                    Some(color) => Some(Gradient { zero: color, one: color }),
                    None => None,
                }
            }
            ColorGradient(ref zero, ref one) => {
                match (self.color_source(hook, zero), self.color_source(hook, one)) {
                    (Some(zero), Some(one)) => Some(Gradient { zero: zero, one: one }),
                    (_, _) => None,
                }
            }
            GradientBlock(ref block) => {
                let mut ret = None;
                self.block(hook, block, |hook_, gradient_| {
                    ret = self.gradient_source(hook_, &**gradient_);
                    false
                });
                ret
            }
        }
    }

    /// Combines a (possibly transparent) color with the opacity.
    fn rgba(&self, color: Color, opacity: f32) -> (u8, u8, u8, u8) {
        let (r, g, b, a) = to_rgba(color);
        (r, g, b, (a as f32 * opacity) as u8)
    }

    /// Given a block call generator, calls `body` zero or more times.
    /// Returns true if there were a matching identifier (no matter `body` has been called or not).
    ///
    /// The `body` receives the inner hook object and the optional variant string
    /// (defaults to an empty string), and can return `false` to stop the iteration.
    fn gen(&self, hook: &Hook, gen: &Gen, body: |&Hook, &str| -> bool) -> bool {
        match *gen {
            HookGen(ref id) => hook.block_hook(id.as_slice(), hook, body),
            TextGen(ref id) => match hook.scalar_hook(id.as_slice()) {
                Some(scalar) => {
                    let text = scalar.into_maybe_owned();
                    body(hook, text.as_slice());
                    true
                },
                None => false
            },
            TextLenGen(ref id) => match hook.scalar_hook(id.as_slice()) {
                Some(scalar) => {
                    let text = scalar.into_maybe_owned();
                    if !text.is_empty() {
                        body(hook, text.as_slice().char_len().to_string()[]);
                    }
                    true
                },
                None => false
            },
        }
    }

    /// Processes block nodes (`{"$$": "id", ...}`) by calling `body` zero or more times.
    /// Returns true if there were a matching identifier (no matter `body` has been called or not).
    ///
    /// The `body` receives the inner hook object and the optional variant string
    /// (defaults to an empty string), and can return `false` to stop the iteration.
    fn block<T>(&self, hook: &Hook, block: &Block<T>, body: |&Hook, &T| -> bool) {
        match *block {
            CondBlock { ref gen, ref then, ref else_ } => {
                let mut called = false;
                self.gen(hook, gen, |hook_, _alt| {
                    called = true;
                    match *then {
                        Some(ref then) => body(hook_, then),
                        None => true,
                    }
                });
                if !called {
                    match *else_ {
                        Some(ref else_) => { body(hook, else_); }
                        None => {}
                    }
                }
            }
            MultiBlock { ref gen, ref map, ref default, ref else_ } => {
                let mut called = false;
                self.gen(hook, gen, |hook_, alt| {
                    called = true;
                    match map.find_equiv(&alt) {
                        Some(then) => body(hook_, then),
                        None => match *default {
                            Some(ref default) => body(hook_, default),
                            None => {
                                warn!("skin: `{id}` gave an unknown alternative `{alt}`, \
                                       will ignore", id = gen.id(), alt = alt);
                                true
                            }
                        }
                    }
                });
                if !called {
                    match *else_ {
                        Some(ref else_) => { body(hook, else_); }
                        None => {}
                    }
                }
            }
        }
    }

    /// Formats the scalar value with given format specification (`{"$": ..., "format": "..."}`).
    fn scalar_format<'a>(&self, scalar: Scalar<'a>, fmt: &ScalarFormat,
                         out: &mut Writer) -> IoResult<()> {
        fn to_f64<'a>(scalar: Scalar<'a>) -> Option<f64> {
            let v = scalar.to_f64();
            if v.is_none() {
                warn!("skin: scalar_format received a non-number `{}`, will ignore", scalar);
            }
            v
        }

        fn fill_and_clip(out: &mut Writer, sign: bool, minwidth: u8, maxwidth: u8,
                         precision: u8, v: f64) -> IoResult<()> {
            let precision = precision as uint;
            if maxwidth == 255 && minwidth == 0 {
                // no need to construct a temporary buffer
                if sign {
                    write!(out, "{:+.*}", precision, v)
                } else {
                    write!(out, "{:.*}", precision, v)
                }
            } else {
                let maxwidth = maxwidth as uint;
                let minwidth = minwidth as uint;
                let s = if sign {
                    format!("{:+.*}", precision, v)
                } else {
                    format!("{:.*}", precision, v)
                };
                let ss = if s.len() > maxwidth {
                    s[s.len() - maxwidth..]
                } else {
                    s[]
                };
                if ss.len() < minwidth {
                    let signidx = if ss.starts_with("+") || ss.starts_with("-") {1} else {0};
                    if signidx > 0 {
                        try!(write!(out, "{}", ss[..signidx]));
                    }
                    for _ in range(0, minwidth - ss.len()) {
                        try!(write!(out, "0"));
                    }
                    write!(out, "{}", ss[signidx..])
                } else {
                    write!(out, "{}", ss)
                }
            }
        }

        match *fmt {
            NoFormat => write!(out, "{}", scalar),
            NumFormat { sign, minwidth, maxwidth, precision, multiplier } => {
                let v = match to_f64(scalar) { Some(v) => v, None => return Ok(()) };
                let v = v * multiplier as f64;
                fill_and_clip(out, sign, minwidth, maxwidth, precision, v)
            },
            MsFormat { sign, minwidth, maxwidth, precision, multiplier } => {
                let v = match to_f64(scalar) { Some(v) => v, None => return Ok(()) };
                let v = v * multiplier as f64;
                let (min, sec) = num::div_rem(v, 60.0);
                try!(fill_and_clip(out, sign, minwidth, maxwidth, 0, min));
                let (sec10, sec1) = num::div_rem(sec.abs(), 10.0);
                write!(out, ":{:01.0}{:.*}", sec10, precision as uint, sec1)
            },
            HmsFormat { sign, minwidth, maxwidth, precision, multiplier } => {
                let v = match to_f64(scalar) { Some(v) => v, None => return Ok(()) };
                let v = v * multiplier as f64;
                let (min, sec) = num::div_rem(v, 60.0);
                let (hour, min) = num::div_rem(min, 60.0);
                try!(fill_and_clip(out, sign, minwidth, maxwidth, 0, hour));
                let (sec10, sec1) = num::div_rem(sec.abs(), 10.0);
                write!(out, ":{:02}:{:01.0}{:.*}", min.abs(), sec10, precision as uint, sec1)
            },
        }
    }

    /// Processes text fragments (`"foo"`, `{"$": "id"}` etc.) and renders it to given `Writer`.
    fn text_source<'a>(&self, hook: &'a Hook, text: &'a TextSource,
                       out: &mut Writer) -> IoResult<()> {
        match *text {
            ScalarText(ref id, ref format) => match hook.scalar_hook(id.as_slice()) {
                Some(scalar) => self.scalar_format(scalar, format, out),
                _ => {
                    warn!("skin: `{id}` is not a scalar hook, will use an empty string",
                          id = id.as_slice());
                    Ok(())
                },
            },
            StaticText(ref text) => write!(out, "{}", *text),
            TextBlock(ref block) => {
                let mut ret = Ok(());
                self.block(hook, block, |hook_, text_| {
                    match self.text_source(hook_, &**text_, out) {
                        Ok(()) => true,
                        Err(err) => { ret = Err(err); false }
                    }
                });
                ret
            },
            TextConcat(ref nodes) => {
                for node in nodes.iter() {
                    try!(self.text_source(hook, node, out));
                }
                Ok(())
            },
        }
    }

    /// Processes texts (`{"$text": ..., ...}`) and renders it as a `MaybeOwned` string.
    fn text<'a>(&self, hook: &'a Hook, text: &'a TextSource) -> str::MaybeOwned<'a> {
        let mut out = MemWriter::new();
        match self.text_source(hook, text, &mut out) {
            Ok(()) => {
                str::from_utf8(out.unwrap()[]).unwrap().to_string().into_maybe_owned()
            },
            Err(err) => {
                warn!("skin: I/O error on text_source({}), will ignore", err);
                "".into_maybe_owned()
            }
        }
    }

    /// Processes texture references (`{"$rect": "tex", ...}`) and calls the callback with
    /// an appropriate reference-counted reference to the texture and clipping rectangle.
    /// `callback` gets called at most once.
    fn texture<'a,T>(&'a self, hook: &'a Hook, id: &str,
                     callback: |Option<(&Rc<Texture2D>, &ImageClip)>| -> T) -> T {
        let mut rendererref = self.renderer.borrow_mut();
        let renderer = rendererref.deref_mut();

        let mut scalar = hook.scalar_hook(id);
        if scalar.is_none() {
            scalar = renderer.skin.scalars.find_equiv(&id).map(|v| v.clone());
        }
        match scalar {
            Some(ImageScalar(TextureSource(tex), ref clip)) => callback(Some((tex, clip))),
            Some(ImageScalar(PathSource(ref path), ref clip)) => {
                let tex = match renderer.imagecache.entry(path.clone()) {
                    hashmap::Occupied(tex) => tex.into_mut(),
                    hashmap::Vacant(entry) => {
                        let surface = sdl_image::load(path).ok();
                        let tex = surface.and_then(
                            |s| Texture2D::from_owned_surface(s, false, false).ok());
                        entry.set(tex.map(Rc::new))
                    }
                };
                callback(tex.as_ref().map(|tex| (tex, clip)))
            },
            Some(ref scalar) => {
                warn!("skin: `{id}` is not a texture scalar hook ({scalar}).",
                      id = id, scalar = *scalar);
                callback(None)
            },
            _ => {
                warn!("skin: `{id}` is not a scalar hook.", id = id);
                callback(None)
            },
        }
    }

    /// Commits the current draw call to given screen.
    fn commit_to(&self, draw: ActiveDraw, screen: &mut Screen) {
        match draw {
            ToBeDrawn => {}
            ShadedLines(d) => { d.draw_to(screen); }
            Shaded(d) => { d.draw_to(screen); }
            Textured(d, tex) => { d.draw_texture_to(screen, tex.deref()); }
        }
    }

    /// Draws to the screen by committing when only needed.
    /// `try_to_draw` should return `None` when the current draw call has to be committed;
    /// `new_draw` should return a new draw call from given screen.
    fn draw_or_commit<T>(&self, try_to_draw: |&mut ActiveDraw| -> Option<T>,
                         new_draw: |&mut Screen| -> ActiveDraw) -> T {
        let mut drawref = self.draw.borrow_mut();
        let draw = drawref.deref_mut(); // don't borrow multiple times
        match try_to_draw(draw) {
            Some(v) => v,
            None => { // commit the current drawing and create a new one
                let mut screenref = self.screen.borrow_mut();
                let screen = screenref.deref_mut();
                let newdraw = new_draw(*screen);
                let olddraw = mem::replace(draw, newdraw);
                self.commit_to(olddraw, *screen);
                match try_to_draw(draw) {
                    Some(v) => v,
                    None => unreachable!() // this is an error
                }
            }
        }
    }

    /// Commits any remaining active draw call.
    fn finish(&self) {
        self.draw_or_commit(
            |draw| match draw { &ToBeDrawn => Some(()), _ => None },
            |_screen| ToBeDrawn);
    }

    /// Forces a `ShadedLines` draw call and calls a callback with the associated drawing.
    fn shaded_lines<T>(&self, f: |&mut ShadedDrawing| -> T) -> T {
        self.draw_or_commit(
            |draw| match draw { &ShadedLines(ref mut d) => Some(f(d)), _ => None },
            |_screen| ShadedLines(ShadedDrawing::new(gl::LINES)))
    }

    /// Forces a `Shaded` draw call and calls a callback with the associated drawing.
    fn shaded<T>(&self, f: |&mut ShadedFontDrawing| -> T) -> T {
        self.draw_or_commit(
            |draw| match draw { &Shaded(ref mut d) => Some(f(d)), _ => None },
            |screen| Shaded(ShadedFontDrawing::new(gl::TRIANGLES, screen.font.clone())))
    }

    /// Forces a `Textured` draw call with given texture
    /// and calls a callback with the associated drawing.
    fn textured<T>(&self, tex: &Rc<Texture2D>, f: |&mut TexturedDrawing| -> T) -> T {
        self.draw_or_commit(
            |draw| match draw {
                // keep the current drawing only when the texture is exactly identical.
                &Textured(ref mut d, ref tex_) if tex.deref() as *const Texture2D ==
                                                  tex_.deref() as *const Texture2D => Some(f(d)),
                _ => None,
            },
            |_screen| {
                let tex = tex.clone();
                let d = TexturedDrawing::new(gl::TRIANGLES, tex.deref());
                Textured(d, tex)
            })
    }

    /// Processes and renders nodes.
    /// Returns `false` when the current render should jump out of the innermost clipping group.
    fn nodes(&self, hook: &Hook, nodes: &[Node]) -> bool {
        for node in nodes.iter() {
            match *node {
                Nothing => {}

                Debug(ref msg) => {
                    let clip = self.clip.get();
                    debug!("skin debug: dx={} dy={} w={} y={} msg={}",
                           clip.dx, clip.dy, clip.w, clip.h, *msg);
                }

                ColoredLine { ref from, ref to, ref color, opacity } => {
                    let (x1, y1) = self.pos(hook, from);
                    let (x2, y2) = self.pos(hook, to);
                    let color = self.color_source(hook, color).unwrap_or(RGB(255, 255, 255));
                    let rgba = self.rgba(color, opacity);
                    self.shaded_lines(|d| d.line_rgba(x1, y1, x2, y2, rgba, rgba));
                }

                ColoredRect { ref at, ref color, opacity } => {
                    let ((x1, y1), (x2, y2)) = self.rect(hook, at);
                    let color = self.color_source(hook, color).unwrap_or(RGB(255, 255, 255));
                    let rgba = self.rgba(color, opacity);
                    self.shaded(|d| d.rect_rgba(x1, y1, x2, y2, rgba, rgba, rgba, rgba));
                }

                TexturedRect { ref tex, ref at, ref colormod, opacity, ref clip } => {
                    let ((x1, y1), (x2, y2)) = self.rect(hook, at);
                    let colormod = self.color_source(hook, colormod).unwrap_or(RGB(255, 255, 255));
                    let rgba = self.rgba(colormod, opacity);

                    self.texture(hook, tex.as_slice(), |ret| match ret {
                        Some((tex, texclip)) => {
                            let tl = texclip.x.to_num(&(tex.width as f32));
                            let tt = texclip.y.to_num(&(tex.height as f32));
                            let tw = texclip.w.to_num(&(tex.width as f32));
                            let th = texclip.h.to_num(&(tex.height as f32));
                            let tx1 = tl + self.expr(hook, &clip.p.x, tw);
                            let ty1 = tt + self.expr(hook, &clip.p.y, th);
                            let tx2 = tl + self.expr(hook, &clip.q.x, tw);
                            let ty2 = tt + self.expr(hook, &clip.q.y, th);
                            self.textured(tex, |d| d.rect_area_rgba(x1, y1, x2, y2,
                                                                    tx1, ty1, tx2, ty2, rgba));
                        }

                        _ => {
                            warn!("skin: `{id}` is not a texture hook, will ignore",
                                  id = tex.as_slice());
                        }
                    });
                }

                Text { ref at, size, anchor: (ax,ay), ref color, ref zerocolor, ref text } => {
                    let (x, y) = self.pos(hook, at);

                    let text = self.text(hook, text);
                    let mut zeroes;
                    if zerocolor.is_some() {
                        zeroes = text.as_slice().find(|c: char| c != '0').unwrap_or(text.len());
                        if zeroes == text.len() || !text.as_slice().char_at(zeroes).is_digit() {
                            zeroes -= 1; // keep at least one zero
                        }
                    } else {
                        zeroes = 0;
                    }

                    static DEFAULT_GRADIENT: Gradient =
                        Gradient { zero: RGB(255, 255, 255), one: RGB(255, 255, 255) };
                    let gradient = self.gradient_source(hook, color).unwrap_or(DEFAULT_GRADIENT);
                    let gradient0 = match *zerocolor {
                        Some(ref color0) => self.gradient_source(hook, color0)
                                                .unwrap_or(DEFAULT_GRADIENT),
                        None => gradient
                    };

                    let zoom = size / NROWS as f32;
                    let w = zoom * (text.as_slice().char_len() * NCOLUMNS) as f32;
                    let h = size as f32;
                    self.shaded(|d| {
                        let left = x - w * ax;
                        let left0 = left + zoom * (zeroes * NCOLUMNS) as f32;
                        let top = y - h * ay;
                        if zeroes > 0 {
                            d.string(left, top, zoom, LeftAligned,
                                     text.as_slice()[..zeroes], gradient0);
                        }
                        d.string(left0, top, zoom, LeftAligned,
                                 text.as_slice()[zeroes..], gradient);
                    });
                }

                Group(ref nodes) => {
                    let savedclip = self.clip.get();
                    self.nodes(hook, nodes[]);
                    self.clip.set(savedclip);
                }

                Clip { ref at } => {
                    let ((x1, y1), (x2, y2)) = self.rect(hook, at);
                    let newclip = ClipRect { dx: x1, dy: y1, w: x2 - x1, h: y2 - y1 };
                    self.clip.set(newclip);
                    if newclip.w <= 0.0 || newclip.h <= 0.0 {
                        // no need to render past this node.
                        return false;
                    }
                }

                NodeBlock(ref block) => {
                    let mut keepgoing = true;
                    self.block(hook, block, |hook_, nodes_| {
                        keepgoing = self.nodes(hook_, nodes_[]);
                        keepgoing
                    });
                    if !keepgoing { return false; }
                }
            }
        }
        true
    }
}

