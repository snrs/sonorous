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
use engine::skin::ast::{Gen, HookGen, TextGen, TextLenGen};
use engine::skin::ast::{Block, CondBlock, MultiBlock};
use engine::skin::ast::{TextSource, ScalarText, StaticText, TextBlock, TextConcat};
use engine::skin::ast::{Node, Nothing, Debug, ColoredLine, ColoredRect, TexturedRect,
                        Text, Group, Displace};
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
    /// Left coordinate of the clipping region.
    priv dx: f32,
    /// Top coordinate of the clipping region.
    priv dy: f32,
    /// The active draw call.
    priv draw: ActiveDraw<'a>,
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
        Renderer { screen: screen, dx: 0.0, dy: 0.0, draw: ToBeDrawn }
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

    fn nodes(&mut self, hook: &Hook, nodes: &[Node]) {
        for node in nodes.iter() {
            match *node {
                Nothing => {}
                Debug(ref msg) => {
                    debug!("skin debug: dx={} dy={} msg={}", self.dx, self.dy, *msg);
                }
                ColoredLine { ref from, ref to, ref color } => {
                    self.shaded_lines().line(from.x + self.dx, from.y + self.dy,
                                             to.x + self.dx, to.y + self.dy, *color);
                }
                ColoredRect { ref at, ref color } => {
                    self.shaded().rect(at.p.x + self.dx, at.p.y + self.dy,
                                       at.q.x + self.dx, at.q.y + self.dy, *color);
                }
                TexturedRect { ref tex, ref at, ref rgba, ref clip } => {
                    match (hook.texture_hook(tex.as_slice()), *clip) {
                        (Some(tex), Some(ref clip)) => {
                            self.textured(tex).rect_area_rgba(at.p.x + self.dx, at.p.y + self.dy,
                                                              at.q.x + self.dx, at.q.y + self.dy,
                                                              clip.p.x, clip.p.y,
                                                              clip.q.x, clip.q.y, *rgba);
                        }
                        (Some(tex), None) => {
                            self.textured(tex).rect_rgba(at.p.x + self.dx, at.p.y + self.dy,
                                                         at.q.x + self.dx, at.q.y + self.dy, *rgba);
                        }
                        (None, _) => {
                            warn!("skin: `{id}` is not a texture hook, will ignore",
                                  id = tex.as_slice());
                        }
                    }
                }
                Text { ref at, size, anchor: (ax,ay), ref color, ref text } => {
                    let text = Renderer::text_source(hook, text);
                    let zoom = size / NROWS as f32;
                    let w = zoom * (text.as_slice().char_len() * NCOLUMNS) as f32;
                    let h = size as f32;
                    self.shaded().string(at.x - w * ax + self.dx, at.y - h * ay + self.dy,
                                         zoom, LeftAligned, text.as_slice(), *color);
                }
                Group { ref clip, ref nodes } => {
                    assert!(clip.is_none(), "clip not implemented");
                    let dx = self.dx;
                    let dy = self.dy;
                    self.nodes(hook, *nodes);
                    self.dx = dx;
                    self.dy = dy;
                }
                Displace { ref by } => {
                    self.dx += by.x;
                    self.dy += by.y;
                }
                Block(ref block) => {
                    Renderer::block(hook, block, |hook_, nodes_| {
                        self.nodes(hook_, *nodes_);
                        true
                    });
                }
            }
        }
    }
}

