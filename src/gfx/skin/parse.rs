// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A (sort of) parser for Scene description language.

use std::{num, result};
use collections::{TreeMap, HashMap};
use serialize::json::{Json, Null, Boolean, Number, String, List, Object};
use serialize::json::from_reader;

use gfx::color::{Color, Gradient, RGB, RGBA};
use gfx::bmfont::{NROWS};
use gfx::skin::scalar::{Scalar, ImageScalar, IntoScalar};
use gfx::skin::ast::{Expr, ENum, ERatioNum};
use gfx::skin::ast::{Pos, Rect, Id};
use gfx::skin::ast::{HookGen, TextGen, TextLenGen};
use gfx::skin::ast::{Block, CondBlock, MultiBlock};
use gfx::skin::ast::{ScalarFormat, NoFormat, NumFormat, MsFormat, HmsFormat};
use gfx::skin::ast::{TextSource, ScalarText, StaticText, TextBlock, TextConcat};
use gfx::skin::ast::{Node, Nothing, Debug, ColoredLine, ColoredRect, TexturedRect,
                     Text, Group, Clip};
use gfx::skin::ast::{Skin};

/// Values that can be constructed from JSON.
pub trait FromJson {
    /// Constructs a value by consuming JSON, returns None if impossible.
    fn from_json(json: Json) -> Result<Self,~str>;
}

impl<T:FromJson> FromJson for ~T {
    fn from_json(json: Json) -> Result<~T,~str> {
        from_json(json).map(|e| ~e)
    }
}

impl<T:FromJson> FromJson for ~[T] {
    fn from_json(json: Json) -> Result<~[T],~str> {
        match json {
            List(l) => result::collect(l.move_iter().map(from_json::<T>)),
            _ => Err(~"expected a list"),
        }
    }
}

/// A wrapper around `FromJson::from_json(json)`.
pub fn from_json<T:FromJson>(json: Json) -> Result<T,~str> {
    FromJson::from_json(json)
}

/// Clamps and converts the value in the range `[0,1]` into the range `[0,255]`.
fn from_0_1(x: f64) -> u8 {
    if x < 0.0 {0} else if x > 1.0 {255} else {(x * 255.0) as u8}
}

/// Clamps the value in the range `[0,255]` into the range `[0,255]`.
fn from_0_255(x: f64) -> u8 {
    if x < 0.0 {0} else if x > 255.0 {255} else {x as u8}
}

/// Returns a string representation of remaining keys in the tree map.
fn treemap_keys(map: &TreeMap<~str,Json>) -> ~str {
    map.iter().map(|(k,_)| k.as_slice()).collect::<Vec<&str>>().connect(", ")
}

macro_rules! ensure_empty(
    ($map:expr, $i:expr) => (
        if !$map.is_empty() {
            return Err(format!("{} has {, plural, =1{an unexpected key} \
                                               other{unexpected keys}}: {}",
                               $i, $map.len(), treemap_keys($map)));
        }
    )
)

macro_rules! fail_with_json(
    ($json:expr, $me:expr) => ({
        let jsontype = match $json {
            Null => "null",
            Boolean(true) => "true",
            Boolean(false) => "false",
            Number(..) => "a number",
            String(..) => "a string",
            List(..) => "a list",
            Object(..) => "an object",
        };
        return Err(format!("expected {}, got {}", $me, jsontype));
    })
)

macro_rules! pop(
    ($map:expr, $key:expr, $me:expr) => (
        match $map.pop(&$key.to_owned()) {
            Some(v) => v,
            None => { return Err(format!("expected `{}` in {}", $key, $me)); }
        }
    )
)

impl FromJson for Scalar<'static> {
    fn from_json(json: Json) -> Result<Scalar<'static>,~str> {
        match json {
            Number(v) => Ok(v.into_scalar()),
            String(s) => Ok(s.into_scalar()),

            Object(mut map) => {
                // {"$image": "path/to/image.png"}
                let image = map.pop(&~"$image");
                if image.is_some() {
                    let path = match image.unwrap() {
                        String(path) => Path::new(path),
                        image => fail_with_json!(image, "a path for the image scalar")
                    };
                    ensure_empty!(map, "the image scalar");
                    return Ok(ImageScalar(path));
                }

                Err(~"expected a scalar, got an unrecognized object")
            },

            _ => fail_with_json!(json, "a scalar")
        }
    }
}

impl FromJson for Color {
    fn from_json(json: Json) -> Result<Color,~str> {
        match json {
            // "#rgb", "#rgba" (preferred)
            // "#rrggbb", "#rrggbbaa" (preferred)
            // "white", "transparent", ...
            String(s) => if s.starts_with("#") {
                match (s.len(), num::from_str_radix::<u32>(s.slice_from(1), 16)) {
                    (4, Some(v)) => Ok(RGB((((v >> 8) & 0xf) * 0x11) as u8,
                                           (((v >> 4) & 0xf) * 0x11) as u8,
                                           (( v       & 0xf) * 0x11) as u8)),
                    (5, Some(v)) => Ok(RGBA((((v >> 12) & 0xf) * 0x11) as u8,
                                            (((v >>  8) & 0xf) * 0x11) as u8,
                                            (((v >>  4) & 0xf) * 0x11) as u8,
                                            (( v        & 0xf) * 0x11) as u8)),
                    (7, Some(v)) => Ok(RGB(((v >> 16) & 0xff) as u8,
                                           ((v >>  8) & 0xff) as u8,
                                           ( v        & 0xff) as u8)),
                    (9, Some(v)) => Ok(RGBA(((v >> 24) & 0xff) as u8,
                                            ((v >> 16) & 0xff) as u8,
                                            ((v >>  8) & 0xff) as u8,
                                            ( v        & 0xff) as u8)),
                    _ => Err(format!("unrecognized color code `{}`", s)),
                }
            } else {
                // predefined colors
                match s.as_slice() {
                    "transparent"   => Ok(RGBA(0, 0, 0, 0)),
                    "white"         => Ok(RGB(255, 255, 255)),
                    "silver"        => Ok(RGB(192, 192, 192)),
                    "gray" | "grey" => Ok(RGB(128, 128, 128)),
                    "black"         => Ok(RGB(  0,   0,   0)),
                    "red"           => Ok(RGB(255,   0,   0)),
                    "maroon"        => Ok(RGB(128,   0,   0)),
                    "yellow"        => Ok(RGB(255, 255,   0)),
                    "orange"        => Ok(RGB(255, 165,   0)),
                    "olive"         => Ok(RGB(128, 128,   0)),
                    "lime"          => Ok(RGB(  0, 255,   0)),
                    "green"         => Ok(RGB(  0, 128,   0)),
                    "aqua"          => Ok(RGB(  0, 255, 255)),
                    "teal"          => Ok(RGB(  0, 128, 128)),
                    "blue"          => Ok(RGB(  0,   0, 255)),
                    "navy"          => Ok(RGB(  0,   0, 128)),
                    "fuchsia"       => Ok(RGB(255,   0, 255)),
                    "purple"        => Ok(RGB(128,   0, 128)),
                    _               => Err(format!("unrecognized color name `{}`", s)),
                }
            },

            List(l) => match l.as_slice() {
                [List(ref l)] => match l.as_slice() {
                    // [[r/255, g/255, b/255]]
                    // [[r/255, g/255, b/255, a/255]]
                    [Number(r), Number(g), Number(b)] =>
                        Ok(RGB(from_0_1(r), from_0_1(g), from_0_1(b))),
                    [Number(r), Number(g), Number(b), Number(a)] =>
                        Ok(RGBA(from_0_1(r), from_0_1(g), from_0_1(b), from_0_1(a))),

                    _ => Err(~"expected a color triple/quadruple in the list")
                },

                // [r, g, b]
                // [r, g, b, a]
                [Number(r), Number(g), Number(b)] =>
                    Ok(RGB(from_0_255(r), from_0_255(g), from_0_255(b))),
                [Number(r), Number(g), Number(b), Number(a)] =>
                    Ok(RGBA(from_0_255(r), from_0_255(g), from_0_255(b), from_0_255(a))),

                _ => Err(~"expected a color triple/quadruple")
            },

            _ => fail_with_json!(json, "a color")
        }
    }
}

impl FromJson for Gradient {
    fn from_json(mut json: Json) -> Result<Gradient,~str> {
        match json {
            // ["#rgb", [r,g,b]] etc.
            List(ref mut l) => if l.len() == 2 {
                let one = l.pop().unwrap();
                let zero = l.pop().unwrap();
                let zero = try!(from_json(zero));
                let one = try!(from_json(one));
                return Ok(Gradient { zero: zero, one: one });
            },

            // {"zero": x, "one": y}
            Object(mut map) => {
                let zero = try!(from_json(pop!(map, "zero", "the gradient")));
                let one = try!(from_json(pop!(map, "one", "the gradient")));
                ensure_empty!(map, "the gradient");
                return Ok(Gradient { zero: zero, one: one });
            },

            _ => {}
        }

        // otherwise same as color
        let color = try!(from_json(json));
        Ok(Gradient { zero: color, one: color })
    }
}

impl FromJson for Expr {
    fn from_json(json: Json) -> Result<Expr,~str> {
        use util::std::str::StrUtil;

        let expr = match json {
            String(s) => s,
            Number(v) => { return Ok(ENum(v as f32)); }
            _ => fail_with_json!(json, "an expression")
        };

        macro_rules! invalid(
            ($msg:expr) => (return Err(format!("invalid expression `{}`: {}", expr, $msg)))
        )

        // parse <num> ['%'] {('+' | '-') <num> ['%']}
        let mut ratio = None;
        let mut num = 0.0;
        let mut s = expr.as_slice();
        let mut minus = false;
        loop {
            let mut v = 0.0;
            if !lex!(s; ws*, f64 -> v, str* -> s, !) { invalid!("expected a number"); }
            if minus { v = -v; }
            if s.starts_with("%") {
                v /= 100.0;
                ratio.mutate_or_set(v, |r| r + v);
                s = s.slice_from(1);
            } else {
                num += v;
            }
            s = s.trim_left();
            if s.is_empty() { break; }
            if s.starts_with("+") {
                minus = false;
                s = s.slice_from(1);
            } else if s.starts_with("-") {
                minus = true;
                s = s.slice_from(1);
            } else {
                invalid!("expected `+` or `-`");
            }
        }

        match ratio {
            Some(r) => Ok(ERatioNum(r, num)),
            None => Ok(ENum(num)),
        }
    }
}

impl FromJson for Pos {
    fn from_json(json: Json) -> Result<Pos,~str> {
        match json {
            // [x, y]
            List(mut l) => match l.len() {
                2 => {
                    let y = l.pop().unwrap();
                    let x = l.pop().unwrap();
                    let x = try!(from_json(x));
                    let y = try!(from_json(y));
                    Ok(Pos { x: x, y: y })
                },
                _ => Err(~"expected a position pair"),
            },

            // {"x": x, "y": y}
            Object(mut map) => {
                let x = try!(from_json(pop!(map, "x", "the position")));
                let y = try!(from_json(pop!(map, "y", "the position")));
                ensure_empty!(map, "the position");
                Ok(Pos { x: x, y: y })
            },

            _ => fail_with_json!(json, "a position")
        }
    }
}

impl FromJson for Rect {
    fn from_json(json: Json) -> Result<Rect,~str> {
        match json {
            List(mut l) => match l.len() {
                // [[px, py], [qx, qy]] (preferred)
                2 => {
                    let q = l.pop().unwrap();
                    let p = l.pop().unwrap();
                    let p = try!(from_json(p));
                    let q = try!(from_json(q));
                    Ok(Rect { p: p, q: q })
                },

                // [px, py, qx, qy]
                4 => {
                    let qy = l.pop().unwrap();
                    let qx = l.pop().unwrap();
                    let py = l.pop().unwrap();
                    let px = l.pop().unwrap();
                    let px = try!(from_json(px));
                    let py = try!(from_json(py));
                    let qx = try!(from_json(qx));
                    let qy = try!(from_json(qy));
                    Ok(Rect { p: Pos { x: px, y: py }, q: Pos { x: qx, y: qy } })
                },

                _ => Err(~"expected a rectangle position pair/quadruple"),
            },

            // {"p": [px, py], "q": [qx, qy]}
            // {"p": {"x": px, "y": py}, "q": {"x": qx, "y": qy}}
            Object(mut map) => {
                let p = try!(from_json(pop!(map, "p", "the rectangle position")));
                let q = try!(from_json(pop!(map, "q", "the rectangle position")));
                ensure_empty!(map, "the rectangle position");
                Ok(Rect { p: p, q: q })
            },

            _ => fail_with_json!(json, "a rectangle position")
        }
    }
}

impl FromJson for Id {
    fn from_json(json: Json) -> Result<Id,~str> {
        match json {
            String(s) => Ok(Id(s)),
            _ => fail_with_json!(json, "an identifier")
        }
    }
}

impl<T:FromJson> FromJson for Block<T> {
    fn from_json(json: Json) -> Result<Block<T>,~str> {
        let mut map = match json {
            Object(map) => map,
            _ => fail_with_json!(json, "a block")
        };

        let gen = match map.pop(&~"$$") {
            Some(hook) => HookGen(try!(from_json(hook))),
            None => match map.pop(&~"$$text") {
                Some(hook) => TextGen(try!(from_json(hook))),
                None => match map.pop(&~"$$len") {
                    Some(hook) => TextLenGen(try!(from_json(hook))),
                    None => { return Err(~"expected a block (text source or node), \
                                           got an unrecognized object"); }
                }
            }
        };

        let then = map.pop(&~"$then");
        let default = map.pop(&~"$default");
        let else_ = map.pop(&~"$else");
        if then.is_some() && !map.is_empty() {
            return Err(~"`$then` cannot be used with other alternatives in the block");
        }
        if map.iter().any(|(k,_)| k.starts_with("$")) {
            return Err(format!("some alternatives in the block start with `$`: {}",
                               treemap_keys(map)));
        }

        fn lift<T,E>(x: Option<Result<T,E>>) -> Result<Option<T>,E> {
            match x { Some(Ok(x)) => Ok(Some(x)), Some(Err(x)) => Err(x), None => Ok(None) }
        }

        let then = try!(lift(then.map(from_json)));
        let default = try!(lift(default.map(from_json)));
        let else_ = try!(lift(else_.map(from_json)));
        match (then, default, else_, map.is_empty()) {
            // {"$$": "even?", "$then": ..., "$else": ...}
            (then, None, else_, true) =>
                Ok(CondBlock { gen: gen, then: then, else_: else_ }),

            // {"$$": "even?", "alt1": ..., "alt2": ..., "$default": ..., "$else": ...}
            (None, default, else_, false) => {
                let map = result::collect(map.move_iter().map(|(k,v)|
                    match from_json::<T>(v) { Ok(v) => Ok((k,v)), Err(err) => Err(err) }
                ));
                Ok(MultiBlock { gen: gen, map: try!(map), default: default, else_: else_ })
            },

            _ => Err(~"no alternatives nor `$then` in the block"),
        }
    }
}

impl FromJson for ScalarFormat {
    fn from_json(json: Json) -> Result<ScalarFormat,~str> {
        let fmt = match json {
            Null => { return Ok(NoFormat); }
            String(s) => s,
            _ => fail_with_json!(json, "a scalar format")
        };

        macro_rules! invalid(
            ($msg:expr) => (return Err(format!("invalid scalar format `{}`: {}", fmt, $msg)))
        )

        let mut s = fmt.as_slice();
        let sign = if s.starts_with("+") { s = s.slice_from(1); true } else { false };
        let unbounded = if s.starts_with("..") { s = s.slice_from(2); true } else { false };
        let nhashes = s.find(|c: char| c != '#').unwrap_or(s.len());
        s = s.slice_from(nhashes);
        let nzeroes = s.find(|c: char| c != '0').unwrap_or(s.len());
        if nzeroes == 0 { invalid!("no `0` in the integer part"); }
        s = s.slice_from(nzeroes);
        let nsixties = if s.starts_with(":00") {
            s = s.slice_from(3);
            if s.starts_with(":00") { s = s.slice_from(3); 2 } else { 1 }
        } else {
            0
        };
        let precision = if s.starts_with(".0") {
            s = s.slice_from(1);
            let prec = s.find(|c: char| c != '0').unwrap_or(s.len());
            assert!(prec > 0);
            s = s.slice_from(prec);
            prec
        } else {
            0
        };
        s = s.trim_left();
        let mult = if s.starts_with("*") {
            from_str::<f64>(s.slice_from(1).trim())
        } else if s.starts_with("/") {
            from_str::<f64>(s.slice_from(1).trim()).map(|v| 1.0 / v)
        } else {
            if !s.is_empty() { invalid!("unknown trailing characters"); }
            Some(1.0)
        };
        let mult = match mult {
            Some(mult) => mult,
            None => { invalid!("expected a multiplier"); }
        };

        let signwidth = if sign {1} else {0};
        let fracwidth = if precision > 0 && nsixties == 0 {precision+1} else {0};
        let maxwidth = signwidth + nhashes + nzeroes + fracwidth;
        let minwidth = signwidth + nzeroes + fracwidth;
        if !unbounded && maxwidth > 254 { invalid!("too long format string"); }
        if minwidth > 254 { invalid!("too long format string"); }

        let maxwidth = if unbounded {255u8} else {maxwidth as u8};
        let minwidth = if nzeroes == 1 {0} else {minwidth as u8}; // easy to test later
        let precision = precision as u8;

        match nsixties {
            0 => Ok(NumFormat { sign: sign, minwidth: minwidth, maxwidth: maxwidth,
                                precision: precision, multiplier: mult }),
            1 => Ok(MsFormat { sign: sign, minwidth: minwidth, maxwidth: maxwidth + 3,
                               precision: precision, multiplier: mult }),
            2 => Ok(HmsFormat { sign: sign, minwidth: minwidth, maxwidth: maxwidth + 6,
                                precision: precision, multiplier: mult }),
            _ => unreachable!(),
        }
    }
}

impl FromJson for TextSource {
    fn from_json(json: Json) -> Result<TextSource,~str> {
        match json {
            // "static text"
            String(s) => Ok(StaticText(s)),

            // ["concat", "enated ", "text"]
            List(l) => Ok(TextConcat(try!(from_json(List(l))))),

            Object(mut map) => {
                // {"$": "number"}
                // {"$": "number", "format": "##00.00"}
                let dynamic = map.pop(&~"$");
                if dynamic.is_some() {
                    let id = try!(from_json(dynamic.unwrap()));
                    let format = match map.pop(&~"format") {
                        None => NoFormat,
                        Some(fmt) => try!(from_json(fmt)),
                    };
                    ensure_empty!(map, "the text source");
                    return Ok(ScalarText(id, format));
                }

                // {"$$": "even?", ...} etc. (delegated to Block)
                Ok(TextBlock(try!(from_json(Object(map)))))
            },

            _ => fail_with_json!(json, "a text source")
        }
    }
}

impl FromJson for Node {
    fn from_json(json: Json) -> Result<Node,~str> {
        match json {
            // "some comment" (temporary)
            String(_) => Ok(Nothing),

            // ["simple group", [{"$clip": [p, q]}], "clipping region will be reset"]
            List(l) => Ok(Group(try!(from_json(List(l))))),

            Object(mut map) => {
                // {"$debug": "message"}
                let debug = map.pop(&~"$debug");
                if debug.is_some() {
                    let msg = match debug.unwrap() {
                        String(msg) => msg,
                        debug => fail_with_json!(debug, "a debug message")
                    };
                    ensure_empty!(map, "the debug message");
                    return Ok(Debug(msg));
                }

                // {"$line": null, "from": [x,y], "to": [x,y], "color": "#rgb"}
                let line = map.pop(&~"$line");
                if line.is_some() {
                    match line.unwrap() {
                        Null => {}
                        line => fail_with_json!(line, "`\"$line\": null`")
                    }
                    let from = try!(from_json(pop!(map, "from", "the line")));
                    let to = try!(from_json(pop!(map, "to", "the line")));
                    let color = try!(from_json(pop!(map, "color", "the line")));
                    ensure_empty!(map, "the line");
                    return Ok(ColoredLine { from: from, to: to, color: color });
                }

                // {"$rect": null, "at": [[px,py], [qx,qy]], "color": "#rgb", "alpha": 0.5}
                // {"$rect": "tex", "at": [[px,py], [qx,qy]], "clip": [[tpx,tpy], [tqx,tqy]]}
                let rect = map.pop(&~"$rect");
                if rect.is_some() {
                    let tex = match rect.unwrap() {
                        Null => None,
                        tex => Some(try!(from_json(tex))),
                    };
                    let at = try!(from_json(pop!(map, "at", "the rectangle")));
                    let color = match map.pop(&~"color") {
                        Some(color) => Some(try!(from_json::<Color>(color))),
                        None => None,
                    };
                    let opacity = match map.pop(&~"opacity") {
                        Some(Number(a)) => Some(a),
                        Some(opacity) =>
                            fail_with_json!(opacity, "a number in the rectangle opacity"),
                        None => None,
                    };
                    let (color, opacity) = match (color, opacity) {
                        (Some(RGB(r,g,b)), Some(a)) => (Some(RGBA(r,g,b,from_0_1(a))), None),
                        (color, opacity) => (color, opacity),
                    };
                    ensure_empty!(map, "the rectangle");
                    return match (tex, color, opacity) {
                        (None, Some(color), None) =>
                            Ok(ColoredRect { at: at, color: color }),
                        (None, Some(_), Some(_)) =>
                            Err(~"the colored rectangle cannot have both \
                                  the RGBA `color` and `opacity`"),
                        (None, None, _) =>
                            Err(~"expected `color` in the colored rectangle"),
                        (Some(tex), color, opacity) => {
                            let rgba = match (color, opacity) {
                                (Some(RGB(r,g,b)), None) => (r,g,b,255),
                                (Some(RGBA(r,g,b,a)), None) => (r,g,b,a),
                                (None, Some(a)) => (255,255,255,from_0_1(a)),
                                (None, None) => (255,255,255,255),
                                (_, _) => {
                                    return Err(~"the textured rectangle cannot have \
                                                 both the RGBA `color` and `opacity`");
                                }
                            };
                            let clip = match map.pop(&~"clip") {
                                Some(clip) => Some(try!(from_json(clip))),
                                None => None,
                            };
                            Ok(TexturedRect { tex: tex, at: at, rgba: rgba, clip: clip })
                        },
                    };
                }

                // {"$text": "some text", "at": [x,y], "size": h, "anchor": "left", "color": "#rgb"}
                let text = map.pop(&~"$text");
                if text.is_some() {
                    let text = try!(from_json(text.unwrap()));
                    let at = try!(from_json(pop!(map, "at", "the text")));
                    let size = match map.pop(&~"size") {
                        Some(Number(size)) if size > 0.0 => size as f32,
                        Some(Number(_)) => { return Err(~"nonpositive `size` in the text"); }
                        Some(size) => fail_with_json!(size, "a text size"),
                        None => NROWS as f32,
                    };
                    let anchor = match map.pop(&~"anchor") {
                        Some(String(s)) => match s.as_slice() {
                            "left"   => (0.0, 0.0),
                            "center" => (0.5, 0.0),
                            "right"  => (1.0, 0.0),
                            _ => {
                                return Err(format!("unrecognized anchor type `{}` in the text", s));
                            }
                        },
                        Some(List(l)) => match l.as_slice() {
                            [Number(x), Number(y)] => (x as f32, y as f32),
                            _ => { return Err(~"expected an anchor pair in the list"); }
                        },
                        Some(anchor) => fail_with_json!(anchor, "an anchor"),
                        None => (0.0, 0.0),
                    };
                    let color = try!(from_json(pop!(map, "color", "the text")));
                    ensure_empty!(map, "the text");
                    return Ok(Text { at: at, size: size, anchor: anchor,
                                     color: color, text: text });
                }

                // {"$clip": [[px,py],[qx,qy]]}
                let clip = map.pop(&~"$clip");
                if clip.is_some() {
                    let at = try!(from_json(clip.unwrap()));
                    ensure_empty!(map, "the reclipping command");
                    return Ok(Clip { at: at });
                }

                // {"$cliptop":    h} = {"$clip": [[0,h],["100%","100%"]]}
                // {"$clipbottom": h} = {"$clip": [[0,0],["100%","100%-h"]]}
                // {"$clipleft":   w} = {"$clip": [[w,0],["100%","100%"]]}
                // {"$clipright":  w} = {"$clip": [[0,0],["100%-w","100%"]]}
                macro_rules! clip_to(
                    ($key:expr, $num_to_rect:expr) => ({
                        let clipto = map.pop(&~$key);
                        if clipto.is_some() {
                            let v = try!(from_json(clipto.unwrap()));
                            ensure_empty!(map, "the reclipping command");
                            return Ok(Clip { at: $num_to_rect(v) });
                        }
                    })
                )
                fn invert(e: Expr) -> Expr {
                    match e {
                        ENum(v) => ERatioNum(1.0, -v),
                        ERatioNum(r, v) => ERatioNum(1.0-r, -v),
                    }
                }
                clip_to!("$cliptop",
                         |h| Rect { p: Pos { x: ENum(0.0), y: h },
                                    q: Pos { x: ERatioNum(1.0,0.0), y: ERatioNum(1.0,0.0) } })
                clip_to!("$clipbottom",
                         |h| Rect { p: Pos { x: ENum(0.0), y: ENum(0.0) },
                                    q: Pos { x: ERatioNum(1.0,0.0), y: invert(h) } })
                clip_to!("$clipleft",
                         |w| Rect { p: Pos { x: w, y: ENum(0.0) },
                                    q: Pos { x: ERatioNum(1.0,0.0), y: ERatioNum(1.0,0.0) } })
                clip_to!("$clipright",
                         |w| Rect { p: Pos { x: ENum(0.0), y: ENum(0.0) },
                                    q: Pos { x: invert(w), y: ERatioNum(1.0,0.0) } })

                // {"$$": "even?", ...} etc. (delegated to Block)
                Ok(Block(try!(from_json(Object(map)))))
            },

            _ => fail_with_json!(json, "a node")
        }
    }
}

impl FromJson for Skin {
    fn from_json(json: Json) -> Result<Skin,~str> {
        match json {
            // {"scalars": {...}, "nodes": [...]}
            Object(mut map) => {
                let mut scalars = HashMap::new();
                match map.pop(&~"scalars") {
                    Some(Object(scalarmap)) => {
                        for (k, v) in scalarmap.move_iter() {
                            scalars.insert(k, try!(from_json(v)));
                        }
                    }
                    Some(scalarmap) => fail_with_json!(scalarmap, "an object with scalars"),
                    None => {}
                }
                let nodes = try!(from_json(pop!(map, "nodes", "the skin")));
                ensure_empty!(map, "the skin");
                Ok(Skin { scalars: scalars, nodes: nodes })
            },

            _ => fail_with_json!(json, "a skin")
        }
    }
}

/// Parses and returns the skin data.
pub fn load_skin(f: &mut Reader) -> Result<Skin,~str> {
    let json = match from_reader(f) {
        Ok(json) => json,
        Err(err) => { return Err(err.to_str()); }
    };
    from_json(json)
}

