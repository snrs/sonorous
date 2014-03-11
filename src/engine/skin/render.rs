// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Skin renderer.

use std::mem;
use std::rc::Rc;
use std::str::MaybeOwned;

use gl = opengles::gl2;

use gfx::gl::Texture2D;
use gfx::draw::{ShadedDrawing, ShadedDrawingTraits, TexturedDrawing, TexturedDrawingTraits};
use gfx::bmfont::{NCOLUMNS, NROWS, FontDrawingUtils, LeftAligned};
use gfx::screen::{Screen, ShadedFontDrawing, ScreenDraw, ScreenTexturedDraw};
use engine::skin::ast::{Expr, ENum, ERatioNum, Pos, Rect};
use engine::skin::ast::{Gen, HookGen, TextGen, TextLenGen};
use engine::skin::ast::{Block, CondBlock, MultiBlock};
use engine::skin::ast::{TextSource, ScalarText, StaticText, TextBlock, TextConcat};
use engine::skin::ast::{Node, Nothing, Debug, ColoredLine, ColoredRect, TexturedRect,
                        Text, Group, Clip};
use engine::skin::ast::{Skin};
use engine::skin::hook::Hook;

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
pub struct Renderer<'a> {
    /// Screen reference.
    priv screen: &'a mut Screen,
    /// The active draw call.
    priv draw: ActiveDraw<'a>,
    /// Left coordinate of the clipping region.
    priv dx: f32,
    /// Top coordinate of the clipping region.
    priv dy: f32,
    /// The width of the clipping region.
    priv w: f32,
    /// The height of the clipping region.
    priv h: f32,
}

impl<'a> Renderer<'a> {
    /// Renders the skin with supplied hook to given screen.
    /// This overwrites to the current screen, no `clear` is called.
    pub fn render(screen: &mut Screen, skin: &Skin, hook: &Hook) {
        let mut renderer = Renderer::new(screen);
        renderer.nodes(hook, skin.nodes);
        renderer.finish();
    }

    fn new(screen: &'a mut Screen) -> Renderer<'a> {
        Renderer { screen: screen, draw: ToBeDrawn,
                   dx: 0.0, dy: 0.0, w: screen.width as f32, h: screen.height as f32 }
    }

    fn expr(_hook: &Hook, pos: &Expr, reference: f32) -> f32 {
        match *pos {
            ENum(v) => v,
            ERatioNum(r, v) => r * reference + v,
        }
    }

    fn pos(&self, hook: &Hook, pos: &Pos) -> (f32, f32) {
        (self.dx + Renderer::expr(hook, &pos.x, self.w),
         self.dy + Renderer::expr(hook, &pos.y, self.h))
    }

    fn rect(&self, hook: &Hook, rect: &Rect) -> ((f32, f32), (f32, f32)) {
        (self.pos(hook, &rect.p), self.pos(hook, &rect.q))
    }

    fn gen(hook: &Hook, gen: &Gen, body: |&Hook, &str| -> bool) -> bool {
        match *gen {
            HookGen(ref id) => hook.block_hook(id.as_slice(), hook, body),
            TextGen(ref id) => match hook.scalar_hook(id.as_slice()) {
                Some(text) => { body(hook, text.as_slice()); true },
                None => false
            },
            TextLenGen(ref id) => match hook.scalar_hook(id.as_slice()) {
                Some(text) => { if !text.is_empty() { body(hook, text.len().to_str()); } true },
                None => false
            },
        }
    }

    fn block<T>(hook: &Hook, block: &Block<T>, body: |&Hook, &T| -> bool) {
        match *block {
            CondBlock { ref gen, ref then, ref else_ } => {
                let mut called = false;
                Renderer::gen(hook, gen, |hook_, _alt| {
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
                Renderer::gen(hook, gen, |hook_, alt| {
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

    fn text_source<'a>(hook: &'a Hook, text: &'a TextSource) -> MaybeOwned<'a> {
        match *text {
            ScalarText(ref id) => match hook.scalar_hook(id.as_slice()) {
                Some(text) => text,
                _ => {
                    warn!("skin: `{id}` is not a scalar hook, will use an empty string",
                          id = id.as_slice());
                    "".into_maybe_owned()
                },
            },
            StaticText(ref text) => {
                let text: &str = *text;
                text.into_maybe_owned()
            },
            TextBlock(ref block) => {
                let mut text = ~"";
                Renderer::block(hook, block, |hook_, text_| {
                    text.push_str(Renderer::text_source(hook_, *text_).as_slice());
                    true
                });
                text.into_maybe_owned()
            }
            TextConcat(ref nodes) => {
                let mut text = ~"";
                for node in nodes.iter() {
                    text.push_str(Renderer::text_source(hook, node).as_slice());
                }
                text.into_maybe_owned()
            }
        }
    }

    fn commit(&mut self, draw: ActiveDraw) {
        match draw {
            ToBeDrawn => {}
            ShadedLines(d) => { d.draw_to(self.screen); }
            Shaded(d) => { d.draw_to(self.screen); }
            Textured(d, tex) => { d.draw_texture_to(self.screen, tex.borrow()); }
        }
    }

    fn finish(&mut self) {
        let draw = mem::replace(&mut self.draw, ToBeDrawn);
        self.commit(draw);
    }

    fn shaded_lines<'a>(&'a mut self) -> &'a mut ShadedDrawing {
        match self.draw {
            ShadedLines(ref mut d) => { return d; }
            _ => {
                let newd = ShadedDrawing::new(gl::LINES);
                let draw = mem::replace(&mut self.draw, ShadedLines(newd));
                self.commit(draw);
                match self.draw {
                    ShadedLines(ref mut d) => d,
                    _ => unreachable!()
                }
            }
        }
    }

    fn shaded<'a>(&'a mut self) -> &'a mut ShadedFontDrawing {
        match self.draw {
            Shaded(ref mut d) => { return d; }
            _ => {
                let newd = ShadedFontDrawing::new(gl::TRIANGLES, self.screen.font.clone());
                let draw = mem::replace(&mut self.draw, Shaded(newd));
                self.commit(draw);
                match self.draw {
                    Shaded(ref mut d) => d,
                    _ => unreachable!()
                }
            }
        }
    }

    fn textured<'a>(&'a mut self, tex: &Rc<Texture2D>) -> &'a mut TexturedDrawing {
        match self.draw {
            // keep the current drawing only when the texture is exactly identical.
            Textured(ref mut d, ref tex_)
                    if tex.borrow() as *Texture2D == tex_.borrow() as *Texture2D => {
                return d;
            }
            _ => {
                let tex = tex.clone();
                let newd = TexturedDrawing::new(gl::TRIANGLES, tex.borrow());
                let draw = mem::replace(&mut self.draw, Textured(newd, tex));
                self.commit(draw);
                match self.draw {
                    Textured(ref mut d, _) => d,
                    _ => unreachable!()
                }
            }
        }
    }

    fn nodes(&mut self, hook: &Hook, nodes: &[Node]) -> bool {
        for node in nodes.iter() {
            match *node {
                Nothing => {}
                Debug(ref msg) => {
                    debug!("skin debug: dx={} dy={} w={} y={} msg={}",
                           self.dx, self.dy, self.w, self.h, *msg);
                }
                ColoredLine { ref from, ref to, ref color } => {
                    let (x1, y1) = self.pos(hook, from);
                    let (x2, y2) = self.pos(hook, to);
                    self.shaded_lines().line(x1, y1, x2, y2, *color);
                }
                ColoredRect { ref at, ref color } => {
                    let ((x1, y1), (x2, y2)) = self.rect(hook, at);
                    self.shaded().rect(x1, y1, x2, y2, *color);
                }
                TexturedRect { ref tex, ref at, ref rgba, ref clip } => {
                    let ((x1, y1), (x2, y2)) = self.rect(hook, at);
                    match (hook.texture_hook(tex.as_slice()), *clip) {
                        (Some(tex), Some(ref clip)) => {
                            let (tw, th) = {
                                let tex = tex.borrow();
                                (tex.width as f32, tex.height as f32)
                            };
                            let tx1 = Renderer::expr(hook, &clip.p.x, tw);
                            let ty1 = Renderer::expr(hook, &clip.p.y, th);
                            let tx2 = Renderer::expr(hook, &clip.q.x, tw);
                            let ty2 = Renderer::expr(hook, &clip.q.y, th);
                            self.textured(tex).rect_area_rgba(x1, y1, x2, y2,
                                                              tx1, ty1, tx2, ty2, *rgba);
                        }
                        (Some(tex), None) => {
                            self.textured(tex).rect_rgba(x1, y1, x2, y2, *rgba);
                        }
                        (None, _) => {
                            warn!("skin: `{id}` is not a texture hook, will ignore",
                                  id = tex.as_slice());
                        }
                    }
                }
                Text { ref at, size, anchor: (ax,ay), ref color, ref text } => {
                    let (x, y) = self.pos(hook, at);
                    let text = Renderer::text_source(hook, text);
                    let zoom = size / NROWS as f32;
                    let w = zoom * (text.as_slice().char_len() * NCOLUMNS) as f32;
                    let h = size as f32;
                    self.shaded().string(x - w * ax, y - h * ay, zoom,
                                         LeftAligned, text.as_slice(), *color);
                }
                Group(ref nodes) => {
                    let dx = self.dx;
                    let dy = self.dy;
                    let w = self.w;
                    let h = self.h;
                    self.nodes(hook, *nodes);
                    self.dx = dx;
                    self.dy = dy;
                    self.w = w;
                    self.h = h;
                }
                Clip { ref at } => {
                    let ((x1, y1), (x2, y2)) = self.rect(hook, at);
                    self.dx = x1;
                    self.dy = y1;
                    self.w = x2 - x1;
                    self.h = y2 - y1;
                    if self.w <= 0.0 || self.h <= 0.0 {
                        // no need to render past this node.
                        // XXX this should propagate to parent nodes up to the innermost group
                        return false;
                    }
                }
                Block(ref block) => {
                    Renderer::block(hook, block, |hook_, nodes_| {
                        self.nodes(hook_, *nodes_)
                    });
                }
            }
        }
        true
    }
}

