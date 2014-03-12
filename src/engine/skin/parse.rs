// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! A (sort of) parser for Scene description language.

use std::{num, option};
use serialize::json::{Json, Null, Number, String, List, Object};
use serialize::json::from_reader;

use gfx::color::{Color, Gradient, RGB, RGBA};
use gfx::bmfont::{NROWS};
use engine::skin::ast::{Expr, ENum, ERatioNum};
use engine::skin::ast::{Pos, Rect, Id};
use engine::skin::ast::{HookGen, TextGen, TextLenGen};
use engine::skin::ast::{Block, CondBlock, MultiBlock};
use engine::skin::ast::{ScalarFormat, NoFormat, NumFormat, MsFormat, HmsFormat};
use engine::skin::ast::{TextSource, ScalarText, StaticText, TextBlock, TextConcat};
use engine::skin::ast::{Node, Nothing, Debug, ColoredLine, ColoredRect, TexturedRect,
                        Text, Group, Clip};
use engine::skin::ast::{Skin};

/// Values that can be constructed from JSON.
pub trait FromJson {
    /// Constructs a value by consuming JSON, returns None if impossible.
    fn from_json(json: Json) -> Option<Self>;
}

impl<T:FromJson> FromJson for ~T {
    fn from_json(json: Json) -> Option<~T> {
        from_json(json).map(|e| ~e)
    }
}

impl<T:FromJson> FromJson for ~[T] {
    fn from_json(json: Json) -> Option<~[T]> {
        match json {
            List(l) => option::collect(l.move_iter().map(from_json::<T>)),
            _ => None,
        }
    }
}

/// A wrapper around `FromJson::from_json(json)`.
pub fn from_json<T:FromJson>(json: Json) -> Option<T> {
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

impl FromJson for Color {
    fn from_json(json: Json) -> Option<Color> {
        match json {
            // "#rgb", "#rgba" (preferred)
            // "#rrggbb", "#rrggbbaa" (preferred)
            // "white", "transparent", ...
            String(s) => if s.starts_with("#") {
                match (s.len(), num::from_str_radix::<u32>(s.slice_from(1), 16)) {
                    (4, Some(v)) => Some(RGB((((v >> 8) & 0xf) * 0x11) as u8,
                                             (((v >> 4) & 0xf) * 0x11) as u8,
                                             (( v       & 0xf) * 0x11) as u8)),
                    (5, Some(v)) => Some(RGBA((((v >> 12) & 0xf) * 0x11) as u8,
                                              (((v >>  8) & 0xf) * 0x11) as u8,
                                              (((v >>  4) & 0xf) * 0x11) as u8,
                                              (( v        & 0xf) * 0x11) as u8)),
                    (7, Some(v)) => Some(RGB(((v >> 16) & 0xff) as u8,
                                             ((v >>  8) & 0xff) as u8,
                                             ( v        & 0xff) as u8)),
                    (9, Some(v)) => Some(RGBA(((v >> 24) & 0xff) as u8,
                                              ((v >> 16) & 0xff) as u8,
                                              ((v >>  8) & 0xff) as u8,
                                              ( v        & 0xff) as u8)),
                    _ => None,
                }
            } else {
                // predefined colors
                match s.as_slice() {
                    "transparent"   => Some(RGBA(0, 0, 0, 0)),
                    "white"         => Some(RGB(255, 255, 255)),
                    "silver"        => Some(RGB(192, 192, 192)),
                    "gray" | "grey" => Some(RGB(128, 128, 128)),
                    "black"         => Some(RGB(  0,   0,   0)),
                    "red"           => Some(RGB(255,   0,   0)),
                    "maroon"        => Some(RGB(128,   0,   0)),
                    "yellow"        => Some(RGB(255, 255,   0)),
                    "orange"        => Some(RGB(255, 165,   0)),
                    "olive"         => Some(RGB(128, 128,   0)),
                    "lime"          => Some(RGB(  0, 255,   0)),
                    "green"         => Some(RGB(  0, 128,   0)),
                    "aqua"          => Some(RGB(  0, 255, 255)),
                    "teal"          => Some(RGB(  0, 128, 128)),
                    "blue"          => Some(RGB(  0,   0, 255)),
                    "navy"          => Some(RGB(  0,   0, 128)),
                    "fuchsia"       => Some(RGB(255,   0, 255)),
                    "purple"        => Some(RGB(128,   0, 128)),
                    _               => None,
                }
            },

            List(l) => match l.as_slice() {
                [List(ref l)] => match l.as_slice() {
                    // [[r/255, g/255, b/255]]
                    // [[r/255, g/255, b/255, a/255]]
                    [Number(r), Number(g), Number(b)] =>
                        Some(RGB(from_0_1(r), from_0_1(g), from_0_1(b))),
                    [Number(r), Number(g), Number(b), Number(a)] =>
                        Some(RGBA(from_0_1(r), from_0_1(g), from_0_1(b), from_0_1(a))),

                    _ => None,
                },

                // [r, g, b]
                // [r, g, b, a]
                [Number(r), Number(g), Number(b)] =>
                    Some(RGB(from_0_255(r), from_0_255(g), from_0_255(b))),
                [Number(r), Number(g), Number(b), Number(a)] =>
                    Some(RGBA(from_0_255(r), from_0_255(g), from_0_255(b), from_0_255(a))),

                _ => None,
            },

            _ => None
        }
    }
}

impl FromJson for Gradient {
    fn from_json(mut json: Json) -> Option<Gradient> {
        fn from_zero_one(zero: Color, one: Color) -> Option<Gradient> {
            Some(Gradient { zero: zero, one: one })
        }

        match json {
            // ["#rgb", [r,g,b]] etc.
            List(ref mut l) => if l.len() == 2 {
                let one = l.pop().unwrap();
                let zero = l.pop().unwrap();
                return match (from_json(zero), from_json(one)) {
                    (Some(zero), Some(one)) => from_zero_one(zero, one),
                    _ => None,
                };
            },

            // {"zero": x, "one": y}
            Object(mut map) => {
                let zero = map.pop(&~"zero").and_then(from_json);
                let one = map.pop(&~"one").and_then(from_json);
                if !map.is_empty() { return None; }
                return match (zero, one) {
                    (Some(zero), Some(one)) => from_zero_one(zero, one),
                    _ => None,
                };
            },

            _ => {}
        }

        // otherwise same as color
        match from_json(json) {
            Some(color) => Some(Gradient { zero: color, one: color }),
            None => None,
        }
    }
}

impl FromJson for Expr {
    fn from_json(json: Json) -> Option<Expr> {
        use util::std::str::StrUtil;

        let expr = match json {
            String(s) => s,
            Number(v) => { return Some(ENum(v as f32)); }
            _ => { return None; }
        };

        // parse <num> ['%'] {('+' | '-') <num> ['%']}
        let mut ratio = None;
        let mut num = 0.0;
        let mut s = expr.as_slice();
        let mut minus = false;
        loop {
            let mut v = 0.0;
            if !lex!(s; ws*, f64 -> v, str* -> s, !) { return None; }
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
                return None;
            }
        }

        match ratio {
            Some(r) => Some(ERatioNum(r, num)),
            None => Some(ENum(num)),
        }
    }
}

impl FromJson for Pos {
    fn from_json(json: Json) -> Option<Pos> {
        fn from_xy(x: Json, y: Json) -> Option<Pos> {
            match (from_json(x), from_json(y)) {
                (Some(x), Some(y)) => Some(Pos { x: x, y: y }),
                _ => None,
            }
        }

        match json {
            // [x, y]
            List(mut l) => match l.len() {
                2 => {
                    let y = l.pop().unwrap();
                    let x = l.pop().unwrap();
                    from_xy(x, y)
                },
                _ => None,
            },

            // {"x": x, "y": y}
            Object(mut map) => {
                let x = map.pop(&~"x");
                let y = map.pop(&~"y");
                if !map.is_empty() { return None; }
                match (x, y) {
                    (Some(x), Some(y)) => from_xy(x, y),
                    _ => None,
                }
            },

            _ => None,
        }
    }
}

impl FromJson for Rect {
    fn from_json(json: Json) -> Option<Rect> {
        fn from_pq(p: Pos, q: Pos) -> Option<Rect> { Some(Rect { p: p, q: q }) }

        match json {
            List(mut l) => match l.len() {
                // [[px, py], [qx, qy]] (preferred)
                2 => {
                    let q = l.pop().unwrap();
                    let p = l.pop().unwrap();
                    match (from_json(p), from_json(q)) {
                        (Some(p), Some(q)) => from_pq(p, q),
                        _ => None,
                    }
                },

                // [px, py, qx, qy]
                4 => {
                    let qy = l.pop().unwrap();
                    let qx = l.pop().unwrap();
                    let py = l.pop().unwrap();
                    let px = l.pop().unwrap();
                    match (from_json(px), from_json(py), from_json(qx), from_json(qy)) {
                        (Some(px), Some(py), Some(qx), Some(qy)) =>
                            from_pq(Pos { x: px, y: py }, Pos { x: qx, y: qy }),
                        _ => None,
                    }
                },

                _ => None,
            },

            // {"p": [px, py], "q": [qx, qy]}
            // {"p": {"x": px, "y": py}, "q": {"x": qx, "y": qy}}
            Object(mut map) => {
                let p = map.pop(&~"p").and_then(from_json);
                let q = map.pop(&~"q").and_then(from_json);
                if !map.is_empty() { return None; }
                match (p, q) {
                    (Some(p), Some(q)) => from_pq(p, q),
                    _ => None,
                }
            },

            _ => None
        }
    }
}

impl FromJson for Id {
    fn from_json(json: Json) -> Option<Id> {
        match json {
            String(s) => Some(Id(s)),
            _ => None
        }
    }
}

impl<T:FromJson> FromJson for Block<T> {
    fn from_json(json: Json) -> Option<Block<T>> {
        let mut map = match json {
            Object(map) => map,
            _ => { return None; }
        };

        let gen = match map.pop(&~"$$") {
            Some(hook) => from_json(hook).map(HookGen),
            None => match map.pop(&~"$$text") {
                Some(hook) => from_json(hook).map(TextGen),
                None => match map.pop(&~"$$len") {
                    Some(hook) => from_json(hook).map(TextLenGen),
                    None => None
                }
            }
        };

        let then = map.pop(&~"$then");
        let default = map.pop(&~"$default");
        let else_ = map.pop(&~"$else");
        if then.is_some() && !map.is_empty() {
            // $then cannot be used with other alternatives
            return None;
        }
        if map.iter().any(|(k,_)| k.starts_with("$")) {
            // alternatives cannot start with $
            return None;
        }

        let then = then.and_then(from_json);
        let default = default.and_then(from_json);
        let else_ = else_.and_then(from_json);
        match (gen, then, default, else_, map.is_empty()) {
            // {"$$": "even?", "$then": ..., "$else": ...}
            (Some(gen), then, None, else_, true) =>
                Some(CondBlock { gen: gen, then: then, else_: else_ }),

            // {"$$": "even?", "alt1": ..., "alt2": ..., "$default": ..., "$else": ...}
            (Some(gen), None, default, else_, false) => {
                let map = option::collect(map.move_iter().map(|(k,v)|
                    match from_json::<T>(v) { Some(v) => Some((k,v)), None => None }
                ));
                match map {
                    Some(map) => Some(MultiBlock { gen: gen, map: map,
                                                   default: default, else_: else_ }),
                    None => None,
                }
            },

            _ => None,
        }
    }
}

impl FromJson for ScalarFormat {
    fn from_json(json: Json) -> Option<ScalarFormat> {
        let fmt = match json {
            Null => { return Some(NoFormat); }
            String(s) => s,
            _ => { return None; }
        };

        let mut s = fmt.as_slice();
        let sign = if s.starts_with("+") { s = s.slice_from(1); true } else { false };
        let unbounded = if s.starts_with("..") { s = s.slice_from(2); true } else { false };
        let nhashes = s.find(|c: char| c != '#').unwrap_or(s.len());
        s = s.slice_from(nhashes);
        let nzeroes = s.find(|c: char| c != '0').unwrap_or(s.len());
        if nzeroes == 0 { return None; } // no `#.00`, only `0.00` is valid
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
            Some(1.0)
        };
        let mult = match mult {
            Some(mult) => mult,
            None => { return None; }
        };

        let signwidth = if sign {1} else {0};
        let fracwidth = if precision > 0 && nsixties == 0 {precision+1} else {0};
        let maxwidth = signwidth + nhashes + nzeroes + fracwidth;
        let minwidth = signwidth + nzeroes + fracwidth;
        if !unbounded && maxwidth > 254 { return None; }
        if minwidth > 254 { return None; }

        let maxwidth = if unbounded {255u8} else {maxwidth as u8};
        let minwidth = if nzeroes == 1 {0} else {minwidth as u8}; // easy to test later
        let precision = precision as u8;

        match nsixties {
            0 => Some(NumFormat { sign: sign, minwidth: minwidth, maxwidth: maxwidth,
                                  precision: precision, multiplier: mult }),
            1 => Some(MsFormat { sign: sign, minwidth: minwidth, maxwidth: maxwidth + 3,
                                 precision: precision, multiplier: mult }),
            2 => Some(HmsFormat { sign: sign, minwidth: minwidth, maxwidth: maxwidth + 6,
                                  precision: precision, multiplier: mult }),
            _ => unreachable!(),
        }
    }
}

impl FromJson for TextSource {
    fn from_json(json: Json) -> Option<TextSource> {
        match json {
            // "static text"
            String(s) => Some(StaticText(s)),

            // ["concat", "enated ", "text"]
            List(l) => from_json(List(l)).map(TextConcat),

            Object(mut map) => {
                // {"$": "number"}
                // {"$": "number", "format": "##00.00"}
                let dynamic = map.pop(&~"$");
                if dynamic.is_some() {
                    let id = from_json(dynamic.unwrap());
                    let format = match map.pop(&~"format") {
                        None => Some(NoFormat),
                        Some(fmt) => from_json(fmt),
                    };
                    if !map.is_empty() { return None; }
                    return match (id, format) {
                        (Some(id), Some(fmt)) => Some(ScalarText(id, fmt)),
                        _ => None,
                    };
                }

                // {"$$": "even?", ...} etc. (delegated to Block)
                from_json(Object(map)).map(TextBlock)
            },

            _ => None,
        }
    }
}

impl FromJson for Node {
    fn from_json(json: Json) -> Option<Node> {
        match json {
            // "some comment" (temporary)
            String(_) => Some(Nothing),

            // ["simple group", [{"$clip": [p, q]}], "clipping region will be reset"]
            List(l) => from_json(List(l)).map(Group),

            Object(mut map) => {
                // {"$debug": "message"}
                let debug = map.pop(&~"$debug");
                if debug.is_some() {
                    if !map.is_empty() { return None; }
                    return match debug.unwrap() {
                        String(msg) => Some(Debug(msg)),
                        _ => None,
                    };
                }

                // {"$line": null, "from": [x,y], "to": [x,y], "color": "#rgb"}
                let line = map.pop(&~"$line");
                if line.is_some() {
                    let line = line.unwrap();
                    let from = map.pop(&~"from").and_then(from_json);
                    let to = map.pop(&~"to").and_then(from_json);
                    let color = map.pop(&~"color").and_then(from_json);
                    if !map.is_empty() { return None; }
                    return match (line, from, to, color) {
                        (Null, Some(from), Some(to), Some(color)) =>
                            Some(ColoredLine { from: from, to: to, color: color }),
                        _ => None,
                    };
                }

                // {"$rect": null, "at": [[px,py], [qx,qy]], "color": "#rgb", "alpha": 0.5}
                // {"$rect": "tex", "at": [[px,py], [qx,qy]], "clip": [[tpx,tpy], [tqx,tqy]]}
                let rect = map.pop(&~"$rect");
                if rect.is_some() {
                    let tex = match rect.unwrap() {
                        Null => Some(None),
                        tex => from_json(tex).map(Some),
                    };
                    let at = map.pop(&~"at").and_then(from_json);
                    let color = map.pop(&~"color").and_then(from_json::<Color>);
                    let opacity = map.pop(&~"opacity");
                    let (color, opacity) = match (color, opacity) {
                        (Some(RGB(r,g,b)), Some(Number(a))) =>
                            (Some(RGBA(r,g,b,from_0_1(a))), None),
                        (color, opacity) => (color, opacity),
                    };
                    let clip = map.pop(&~"clip").and_then(from_json);
                    if !map.is_empty() { return None; }
                    return match (tex, at, color, opacity, clip) {
                        (Some(None), Some(at), Some(color), None, None) =>
                            Some(ColoredRect { at: at, color: color }),
                        (Some(Some(tex)), Some(at), color, opacity, clip) => {
                            let rgba = match (color, opacity) {
                                (Some(RGB(r,g,b)), None) => Some((r,g,b,255)),
                                (Some(RGBA(r,g,b,a)), None) => Some((r,g,b,a)),
                                (None, Some(Number(a))) => Some((255,255,255,from_0_1(a))),
                                (None, None) => Some((255,255,255,255)),
                                (_, _) => None,
                            };
                            match rgba {
                                Some(rgba) =>
                                    Some(TexturedRect { tex: tex, at: at, rgba: rgba, clip: clip }),
                                None => None,
                            }
                        },
                        _ => None,
                    };
                }

                // {"$text": "some text", "at": [x,y], "size": h, "anchor": "left", "color": "#rgb"}
                let text = map.pop(&~"$text");
                if text.is_some() {
                    let text = from_json(text.unwrap());
                    let at = map.pop(&~"at").and_then(from_json);
                    let size = match map.pop(&~"size") {
                        Some(Number(size)) if size > 0.0 => Some(size as f32),
                        Some(_) => None,
                        None => Some(NROWS as f32),
                    };
                    let anchor = match map.pop(&~"anchor") {
                        Some(String(s)) => match s.as_slice() {
                            "left"   => Some((0.0, 0.0)),
                            "center" => Some((0.5, 0.0)),
                            "right"  => Some((1.0, 0.0)),
                            _        => None,
                        },
                        Some(List(l)) => match l.as_slice() {
                            [Number(x), Number(y)] => Some((x as f32, y as f32)),
                            _ => None,
                        },
                        Some(_) => None,
                        None => Some((0.0, 0.0)),
                    };
                    let color = map.pop(&~"color").and_then(from_json);
                    if !map.is_empty() { return None; }
                    return match (text, at, size, anchor, color) {
                        (Some(text), Some(at), Some(size), Some(anchor), Some(color)) =>
                            Some(Text { at: at, size: size, anchor: anchor,
                                        color: color, text: text }),
                        _ => None,
                    };
                }

                // {"$clip": [[px,py],[qx,qy]]}
                let clip = map.pop(&~"$clip");
                if clip.is_some() {
                    let at = from_json(clip.unwrap());
                    if !map.is_empty() { return None; }
                    return match at {
                        Some(at) => Some(Clip { at: at }),
                        _ => None,
                    };
                }

                // {"$cliptop":    h} = {"$clip": [[0,h],["100%","100%"]]}
                // {"$clipbottom": h} = {"$clip": [[0,0],["100%","100%-h"]]}
                // {"$clipleft":   w} = {"$clip": [[w,0],["100%","100%"]]}
                // {"$clipright":  w} = {"$clip": [[0,0],["100%-w","100%"]]}
                macro_rules! clip_to(
                    ($key:expr, $num_to_rect:expr) => ({
                        let clipto = map.pop(&~$key);
                        if clipto.is_some() {
                            let v = from_json(clipto.unwrap());
                            if !map.is_empty() { return None; }
                            return match v {
                                Some(v) => Some(Clip { at: $num_to_rect(v) }),
                                _ => None,
                            };
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
                from_json(Object(map)).map(Block)
            },

            _ => None
        }
    }
}

impl FromJson for Skin {
    fn from_json(json: Json) -> Option<Skin> {
        match json {
            // {"nodes": [...]}
            Object(mut map) => {
                let nodes = map.pop(&~"nodes").and_then(from_json);
                if !map.is_empty() { return None; }
                match nodes {
                    Some(nodes) => Some(Skin { nodes: nodes }),
                    None => None,
                }
            },

            _ => None
        }
    }
}

/// Parses and returns the skin data.
pub fn load_skin(f: &mut Reader) -> Result<Skin,~str> {
    let json = match from_reader(f) {
        Ok(json) => json,
        Err(err) => { return Err(err.to_str()); }
    };
    match from_json(json) {
        Some(skin) => Ok(skin),
        None => Err(~"skin parsing failed"),
    }
}

