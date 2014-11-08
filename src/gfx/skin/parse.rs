// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! A (sort of) parser for Scene description language.

use std::{num, mem};
use std::collections::{TreeMap, HashMap};
use cson;
use serialize::json::{Json, ToJson, Null, Boolean, I64, U64, F64, String, List, Object};

use gfx::color::{Color, RGB, RGBA};
use gfx::ratio_num::RatioNum;
use gfx::bmfont::{NROWS};
use gfx::skin::scalar::{Scalar, ImageScalar, IntoScalar};
use gfx::skin::scalar::{PathSource, ImageClip};
use gfx::skin::ast::{Expr, ENum, EScalar};
use gfx::skin::ast::{Pos, Rect, Id};
use gfx::skin::ast::{HookGen, TextGen, TextLenGen};
use gfx::skin::ast::{Block, CondBlock, MultiBlock};
use gfx::skin::ast::{ScalarFormat, NoFormat, NumFormat, MsFormat, HmsFormat};
use gfx::skin::ast::{TextSource, ScalarText, StaticText, TextBlock, TextConcat};
use gfx::skin::ast::{ColorSource, ScalarColor, StaticColor, ColorBlock};
use gfx::skin::ast::{GradientSource, FlatColorGradient, ColorGradient, GradientBlock};
use gfx::skin::ast::{Node, Nothing, Debug, ColoredLine, ColoredRect, TexturedRect,
                     Text, Group, Clip, NodeBlock};
use gfx::skin::ast::{Skin};

/// Values that can be constructed from JSON.
pub trait FromJson {
    /// Constructs a value by consuming JSON, returns None if impossible.
    fn from_json(json: Json) -> Result<Self,String>;
}

impl<T:FromJson> FromJson for Box<T> {
    fn from_json(json: Json) -> Result<Box<T>,String> {
        from_json(json).map(|e| box e)
    }
}

impl<T:FromJson> FromJson for Vec<T> {
    fn from_json(json: Json) -> Result<Vec<T>,String> {
        match json {
            List(l) => l.into_iter().map(from_json::<T>).collect(),
            _ => Err(format!("expected a list")),
        }
    }
}

/// A wrapper around `FromJson::from_json(json)`.
pub fn from_json<T:FromJson>(json: Json) -> Result<T,String> {
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

/// Extracts a number from `json` if possible.
fn num(json: &Json) -> Option<f64> {
    match *json {
        I64(v) => Some(v as f64),
        U64(v) => Some(v as f64),
        F64(v) => Some(v),
        _ => None,
    }
}

/// Returns a string representation of remaining keys in the tree map.
fn treemap_keys(map: &TreeMap<String,Json>) -> String {
    map.iter().map(|(k,_)| k[]).collect::<Vec<&str>>().connect(", ")
}

macro_rules! ensure_empty(
    ($map:expr, $i:expr) => (
        if !$map.is_empty() {
            return Err(format!("{} has {}: {}",
                               $i,
                               if $map.len() == 1 {"an unexpected key"} else {"unexpected keys"},
                               treemap_keys(&$map)));
        }
    )
)

macro_rules! fail_with_json(
    ($json:expr, $me:expr) => ({
        let jsontype = match $json {
            Null => "null",
            Boolean(true) => "true",
            Boolean(false) => "false",
            I64(..) | U64(..) | F64(..) => "a number",
            String(..) => "a string",
            List(..) => "a list",
            Object(..) => "an object",
        };
        return Err(format!("expected {}, got {}", $me, jsontype));
    })
)

macro_rules! pop(
    ($map:expr, $key:expr, $me:expr) => (
        match $map.remove(&$key.to_string()) {
            Some(v) => v,
            None => { return Err(format!("expected `{}` in {}", $key, $me)); }
        }
    )
)

impl FromJson for Scalar<'static> {
    fn from_json(json: Json) -> Result<Scalar<'static>,String> {
        match json {
            I64(v) => Ok(v.into_scalar()),
            U64(v) => Ok(v.into_scalar()),
            F64(v) => Ok(v.into_scalar()),
            String(s) => Ok(s.into_scalar()),

            Object(mut map) => {
                // {"$image": "path/to/image.png"}
                let image = map.remove(&"$image".to_string());
                if image.is_some() {
                    let path = match image.unwrap() {
                        String(path) => Path::new(path),
                        image => fail_with_json!(image, "a path for the image scalar")
                    };
                    let clip = match map.remove(&"clip".to_string()) {
                        Some(clip) => try!(from_json(clip)),
                        None => ImageClip::new(),
                    };
                    ensure_empty!(map, "the image scalar");
                    return Ok(ImageScalar(PathSource(path), clip));
                }

                Err(format!("expected a scalar, got an unrecognized object"))
            },

            _ => fail_with_json!(json, "a scalar")
        }
    }
}

impl FromJson for ImageClip {
    fn from_json(json: Json) -> Result<ImageClip,String> {
        let rect = try!(from_json::<Rect>(json));
        match rect {
            Rect { p: Pos { x: ENum(px), y: ENum(py) }, q: Pos { x: ENum(qx), y: ENum(qy) } } =>
                Ok(ImageClip { x: px, y: py, w: qx - px, h: qy - py }),
            _ => Err("clip coordinates for images should be a simple number".to_string()),
        }
    }
}

impl FromJson for Color {
    fn from_json(json: Json) -> Result<Color,String> {
        match json {
            // "#rgb", "#rgba" (preferred)
            // "#rrggbb", "#rrggbbaa" (preferred)
            // "white", "transparent", ...
            String(s) => if s[].starts_with("#") {
                match (s.len(), num::from_str_radix::<u32>(s[1..], 16)) {
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
                match s[] {
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

            List(l) => match l[] {
                [List(ref l)] => match l[] {
                    // [[r/255, g/255, b/255]]
                    [ref r, ref g, ref b] => match (num(r), num(g), num(b)) {
                        (Some(r), Some(g), Some(b)) =>
                            Ok(RGB(from_0_1(r), from_0_1(g), from_0_1(b))),
                        _ => Err(format!("expected a color triple/quadruple in the list"))
                    },

                    // [[r/255, g/255, b/255, a/255]]
                    [ref r, ref g, ref b, ref a] => match (num(r), num(g), num(b), num(a)) {
                        (Some(r), Some(g), Some(b), Some(a)) =>
                            Ok(RGBA(from_0_1(r), from_0_1(g), from_0_1(b), from_0_1(a))),
                        _ => Err(format!("expected a color triple/quadruple in the list"))
                    },

                    _ => Err(format!("expected a color triple/quadruple in the list"))
                },

                // [r, g, b]
                [ref r, ref g, ref b] => match (num(r), num(g), num(b)) {
                    (Some(r), Some(g), Some(b)) =>
                        Ok(RGB(from_0_255(r), from_0_255(g), from_0_255(b))),
                    _ => Err(format!("expected a color triple/quadruple"))
                },

                // [r, g, b, a]
                [ref r, ref g, ref b, ref a] => match (num(r), num(g), num(b), num(a)) {
                    (Some(r), Some(g), Some(b), Some(a)) =>
                        Ok(RGBA(from_0_255(r), from_0_255(g), from_0_255(b), from_0_255(a))),
                    _ => Err(format!("expected a color triple/quadruple"))
                },

                _ => Err(format!("expected a color triple/quadruple"))
            },

            _ => fail_with_json!(json, "a color")
        }
    }
}

impl FromJson for ColorSource {
    fn from_json(json: Json) -> Result<ColorSource,String> {
        match json {
            // "white", "#fff", [255,255,255] etc.
            String(..) | List(..) => Ok(StaticColor(try!(from_json(json)))),

            Object(mut map) => {
                // {"$": "color"}
                let dynamic = map.remove(&"$".to_string());
                if dynamic.is_some() {
                    let id = try!(from_json(dynamic.unwrap()));
                    ensure_empty!(map, "the color");
                    return Ok(ScalarColor(id));
                }

                // {"$$": "transparent?", ...} etc. (delegated to Block)
                Ok(ColorBlock(try!(from_json(Object(map)))))
            }

            _ => fail_with_json!(json, "a color")
        }
    }
}

impl FromJson for GradientSource {
    fn from_json(json: Json) -> Result<GradientSource,String> {
        match json {
            // "white", "#fff" etc.
            String(..) => Ok(FlatColorGradient(StaticColor(try!(from_json(json))))),

            List(mut l) => if l.len() == 2 {
                // [x, y]
                let mut it = mem::replace(&mut l, Vec::new()).into_iter();
                let zero = try!(from_json(it.next().unwrap()));
                let one = try!(from_json(it.next().unwrap()));
                Ok(ColorGradient(zero, one))
            } else {
                // [r, g, b] or [r, g, b, a]
                Ok(FlatColorGradient(StaticColor(try!(from_json(List(l))))))
            },

            Object(mut map) => {
                // {"zero": x, "one": y}
                if map.len() == 2 && map.contains_key(&"zero".to_string())
                                  && map.contains_key(&"one".to_string()) {
                    let zero = try!(from_json(pop!(map, "zero", "the gradient")));
                    let one = try!(from_json(pop!(map, "one", "the gradient")));
                    assert!(map.is_empty());
                    return Ok(ColorGradient(zero, one));
                }

                // {"$": "color"}
                let dynamic = map.remove(&"$".to_string());
                if dynamic.is_some() {
                    let id = try!(from_json(dynamic.unwrap()));
                    ensure_empty!(map, "the gradient");
                    return Ok(FlatColorGradient(ScalarColor(id)));
                }

                // {"$$": "transparent?", ...} etc. (delegated to Block)
                Ok(GradientBlock(try!(from_json(Object(map)))))
            },

            _ => fail_with_json!(json, "a gradient")
        }
    }
}

impl FromJson for Expr {
    fn from_json(json: Json) -> Result<Expr,String> {
        #[deriving(PartialEq, Eq)]
        enum Type { TNum, TRatioNum }

        // <atom> ::= <num> | <num> '%' | <id> | '(' <additive> ')'
        fn atom<'a>(s: &'a str) -> Result<(&'a str, Expr, Type), String> {
            let mut s = s.trim_left();
            let mut v = 0.0;
            if lex!(s; f32 -> v, ws*, str* -> s, !) {
                if s.starts_with("%") {
                    let e = ENum(RatioNum::from_ratio(v / 100.0));
                    Ok((s[1..], e, TRatioNum))
                } else {
                    let e = ENum(RatioNum::from_num(v));
                    Ok((s, e, TNum))
                }
            } else if s.starts_with("(") {
                let (s, e, t) = try!(additive(s[1..]));
                let s = s.trim_left();
                if s.starts_with(")") {
                    Ok((s[1..], e, t))
                } else {
                    Err(format!("parsing failed at `{}`: expected `)`", s))
                }
            } else { // <id> ::= [a-zA-Z_] [a-zA-Z0-9_]* ('.' [a-zA-Z_] [a-zA-Z0-9_]*)*
                let mut first = true;
                let mut idlen = s.len();
                for (i, c) in s.char_indices() {
                    match c {
                        'a'...'z' | 'A'...'Z' | '_' => { first = false; }
                        '0'...'9' if !first => { first = false; }
                        '.' if !first => { first = true; }
                        _ => { idlen = i; break; }
                    }
                }
                if idlen == 0 {
                    Err(format!("parsing failed at `{}`: expected an id", s))
                } else {
                    let e = EScalar(Id(s[..idlen].to_string()));
                    Ok((s[idlen..], e, TNum)) // yes, no dimensional scalar for now.
                }
            }
        }

        // <unary> ::= <atom> | '-' <atom>
        fn unary<'a>(s: &'a str) -> Result<(&'a str, Expr, Type), String> {
            let s = s.trim_left();
            if s.starts_with("-") {
                let (s, e, t) = try!(atom(s[1..]));
                Ok((s, Expr::neg(e), t))
            } else {
                atom(s)
            }
        }

        // <multiplicative> ::= <unary> | <multiplicative> '*' <unary>
        //                              | <multiplicative> '/' <unary>
        fn multiplicative<'a>(s: &'a str) -> Result<(&'a str, Expr, Type), String> {
            let s = s.trim_left();
            let (mut s, mut e, mut t) = try!(unary(s));
            loop {
                s = s.trim_left();
                if s.starts_with("*") {
                    let (s_, e_, t_) = try!(unary(s[1..]));
                    s = s_;
                    e = Expr::mul(e, e_);
                    t = match (t, t_) {
                        (TNum, TNum) => TNum,
                        (TNum, TRatioNum) | (TRatioNum, TNum) => TRatioNum,
                        (TRatioNum, TRatioNum) => {
                            return Err("invalid type: ratio * ratio".to_string());
                        }
                    };
                } else if s.starts_with("/") {
                    let (s_, e_, t_) = try!(unary(s[1..]));
                    s = s_;
                    e = Expr::div(e, e_);
                    t = match (t, t_) {
                        (TNum, TNum) => TNum,
                        (TRatioNum, TNum) => TRatioNum,
                        (_, TRatioNum) => {
                            return Err("invalid type: number or ratio / ratio".to_string());
                        }
                    };
                } else {
                    break;
                }
            }
            Ok((s, e, t))
        }

        // <additive> ::= <multiplicative> | <additive> '+' <unary> | <additive> '-' <unary>
        fn additive<'a>(s: &'a str) -> Result<(&'a str, Expr, Type), String> {
            let s = s.trim_left();
            let (mut s, mut e, mut t) = try!(multiplicative(s));
            loop {
                s = s.trim_left();
                if s.starts_with("+") {
                    let (s_, e_, t_) = try!(multiplicative(s[1..]));
                    s = s_;
                    e = Expr::add(e, e_);
                    t = if t == TNum && t_ == TNum {TNum} else {TRatioNum};
                } else if s.starts_with("-") {
                    let (s_, e_, t_) = try!(multiplicative(s[1..]));
                    s = s_;
                    e = Expr::sub(e, e_);
                    t = if t == TNum && t_ == TNum {TNum} else {TRatioNum};
                } else {
                    break;
                }
            }
            Ok((s, e, t))
        }

        // <expr> ::= <additive> EOF
        fn expr<'a>(s: &'a str) -> Result<Expr, String> {
            let (s, e, _t) = try!(additive(s));
            if !s.trim_left().is_empty() {
                Err(format!("parsing failed at `{}`: expected the end of string", s))
            } else {
                Ok(e)
            }
        }

        match json {
            String(s) => expr(s[]),
            I64(v) => Ok(ENum(RatioNum::from_num(v as f32))),
            U64(v) => Ok(ENum(RatioNum::from_num(v as f32))),
            F64(v) => Ok(ENum(RatioNum::from_num(v as f32))),
            _ => fail_with_json!(json, "an expression"),
        }
    }
}

impl FromJson for Pos {
    fn from_json(mut json: Json) -> Result<Pos,String> {
        match json {
            // [x, y]
            List(ref mut l) => match l.len() {
                2 => {
                    let mut it = mem::replace(l, Vec::new()).into_iter();
                    let x = try!(from_json(it.next().unwrap()));
                    let y = try!(from_json(it.next().unwrap()));
                    Ok(Pos { x: x, y: y })
                },
                _ => Err(format!("expected a position pair")),
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
    fn from_json(mut json: Json) -> Result<Rect,String> {
        match json {
            List(ref mut l) => match l.len() {
                // [[px, py], [qx, qy]] (preferred)
                2 => {
                    let mut it = mem::replace(l, Vec::new()).into_iter();
                    let p = try!(from_json(it.next().unwrap()));
                    let q = try!(from_json(it.next().unwrap()));
                    Ok(Rect { p: p, q: q })
                },

                // [px, py, qx, qy]
                4 => {
                    let mut it = mem::replace(l, Vec::new()).into_iter();
                    let px = try!(from_json(it.next().unwrap()));
                    let py = try!(from_json(it.next().unwrap()));
                    let qx = try!(from_json(it.next().unwrap()));
                    let qy = try!(from_json(it.next().unwrap()));
                    Ok(Rect { p: Pos { x: px, y: py }, q: Pos { x: qx, y: qy } })
                },

                _ => Err(format!("expected a rectangle position pair/quadruple")),
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
    fn from_json(json: Json) -> Result<Id,String> {
        match json {
            String(s) => Ok(Id(s.into_string())),
            _ => fail_with_json!(json, "an identifier")
        }
    }
}

impl<T:FromJson> FromJson for Block<T> {
    fn from_json(json: Json) -> Result<Block<T>,String> {
        let mut map = match json {
            Object(map) => map,
            _ => fail_with_json!(json, "a block")
        };

        let gen = match map.remove(&"$$".to_string()) {
            Some(hook) => HookGen(try!(from_json(hook))),
            None => match map.remove(&"$$text".to_string()) {
                Some(hook) => TextGen(try!(from_json(hook))),
                None => match map.remove(&"$$len".to_string()) {
                    Some(hook) => TextLenGen(try!(from_json(hook))),
                    None => { return Err(format!("expected a block (text source or node), \
                                                  got an unrecognized object")); }
                }
            }
        };

        let then = map.remove(&"$then".to_string());
        let default = map.remove(&"$default".to_string());
        let else_ = map.remove(&"$else".to_string());
        if then.is_some() && !map.is_empty() {
            return Err(format!("`$then` cannot be used with other alternatives in the block"));
        }
        if map.iter().any(|(k,_)| k[].starts_with("$")) {
            return Err(format!("some alternatives in the block start with `$`: {}",
                               treemap_keys(&map)));
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
                let map: Result<HashMap<String,T>, String> = map.into_iter().map(|(k,v)|
                    match from_json::<T>(v) {
                        Ok(v) => Ok((k.into_string(), v)),
                        Err(err) => Err(err),
                    }
                ).collect();
                Ok(MultiBlock { gen: gen, map: try!(map), default: default, else_: else_ })
            },

            _ => Err(format!("no alternatives nor `$then` in the block")),
        }
    }
}

impl FromJson for ScalarFormat {
    fn from_json(json: Json) -> Result<ScalarFormat,String> {
        let fmt = match json {
            Null => { return Ok(NoFormat); }
            String(s) => s,
            _ => fail_with_json!(json, "a scalar format")
        };

        macro_rules! invalid(
            ($msg:expr) => (return Err(format!("invalid scalar format `{}`: {}", fmt, $msg)))
        )

        let mut s = fmt[];
        let sign = if s.starts_with("+") { s = s[1..]; true } else { false };
        let unbounded = if s.starts_with("..") { s = s[2..]; true } else { false };
        let nhashes = s.find(|c: char| c != '#').unwrap_or(s.len());
        s = s[nhashes..];
        let nzeroes = s.find(|c: char| c != '0').unwrap_or(s.len());
        if nzeroes == 0 { invalid!("no `0` in the integer part"); }
        s = s[nzeroes..];
        let nsixties = if s.starts_with(":00") {
            s = s[3..];
            if s.starts_with(":00") { s = s[3..]; 2u } else { 1u }
        } else {
            0u
        };
        let precision = if s.starts_with(".0") {
            s = s[1..];
            let prec = s.find(|c: char| c != '0').unwrap_or(s.len());
            assert!(prec > 0);
            s = s[prec..];
            prec
        } else {
            0
        };
        s = s.trim_left();
        let mult = if s.starts_with("*") {
            from_str::<f64>(s[1..].trim())
        } else if s.starts_with("/") {
            from_str::<f64>(s[1..].trim()).map(|v| 1.0 / v)
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
    fn from_json(json: Json) -> Result<TextSource,String> {
        match json {
            // "static text"
            String(s) => Ok(StaticText(s.into_string())),

            // ["concat", "enated ", "text"]
            List(l) => Ok(TextConcat(try!(from_json(List(l))))),

            Object(mut map) => {
                // {"$": "number"}
                // {"$": "number", "format": "##00.00"}
                let dynamic = map.remove(&"$".to_string());
                if dynamic.is_some() {
                    let id = try!(from_json(dynamic.unwrap()));
                    let format = match map.remove(&"format".to_string()) {
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
    fn from_json(json: Json) -> Result<Node,String> {
        match json {
            // "some comment" (temporary)
            String(_) => Ok(Nothing),

            // ["simple group", [{"$clip": [p, q]}], "clipping region will be reset"]
            List(l) => Ok(Group(try!(from_json(List(l))))),

            Object(mut map) => {
                // {"$debug": "message"}
                let debug = map.remove(&"$debug".to_string());
                if debug.is_some() {
                    let msg = match debug.unwrap() {
                        String(msg) => msg,
                        debug => fail_with_json!(debug, "a debug message")
                    };
                    ensure_empty!(map, "the debug message");
                    return Ok(Debug(msg.into_string()));
                }

                // {"$line": null, "from": [x,y], "to": [x,y], "color": "#rgb"}
                let line = map.remove(&"$line".to_string());
                if line.is_some() {
                    match line.unwrap() {
                        Null => {}
                        line => fail_with_json!(line, "`\"$line\": null`")
                    }
                    let from = try!(from_json(pop!(map, "from", "the line")));
                    let to = try!(from_json(pop!(map, "to", "the line")));
                    let color = try!(from_json(pop!(map, "color", "the line")));
                    let opacity = match map.remove(&"opacity".to_string()) {
                        Some(opacity) => match num(&opacity) {
                            Some(a) => a as f32,
                            None => fail_with_json!(opacity, "a number in the line opacity"),
                        },
                        None => 1.0,
                    };
                    ensure_empty!(map, "the line");
                    return Ok(ColoredLine { from: from, to: to, color: color, opacity: opacity });
                }

                // {"$rect": null, "at": [[px,py], [qx,qy]], "color": "#rgb", "alpha": 0.5}
                // {"$rect": "tex", "at": [[px,py], [qx,qy]], "clip": [[tpx,tpy], [tqx,tqy]]}
                let rect = map.remove(&"$rect".to_string());
                if rect.is_some() {
                    let tex = match rect.unwrap() {
                        Null => None,
                        tex => Some(try!(from_json(tex))),
                    };
                    let at = try!(from_json(pop!(map, "at", "the rectangle")));
                    let color = match map.remove(&"color".to_string()) {
                        Some(color) => Some(try!(from_json::<ColorSource>(color))),
                        None => None,
                    };
                    let opacity = match map.remove(&"opacity".to_string()) {
                        Some(opacity) => match num(&opacity) {
                            Some(a) => a as f32,
                            None => fail_with_json!(opacity, "a number in the rectangle opacity"),
                        },
                        None => 1.0,
                    };
                    let clip = match map.remove(&"clip".to_string()) {
                        Some(clip) => Some(try!(from_json(clip))),
                        None => None,
                    };
                    ensure_empty!(map, "the rectangle");
                    return match (tex, color, clip) {
                        (None, Some(color), None) =>
                            Ok(ColoredRect { at: at, color: color, opacity: opacity }),
                        (None, None, _) =>
                            Err(format!("expected `color` in the colored rectangle")),
                        (None, _, Some(..)) =>
                            Err(format!("the colored rectangle cannot have `clip`")),
                        (Some(tex), color, clip) => {
                            let color = color.unwrap_or(StaticColor(RGBA(255,255,255,255)));
                            let clip = clip.unwrap_or(Rect::new());
                            Ok(TexturedRect { tex: tex, at: at, colormod: color,
                                              opacity: opacity, clip: clip })
                        },
                    };
                }

                // {"$text": "some text", "at": [x,y], "size": h, "anchor": "left", "color": "#rgb"}
                let text = map.remove(&"$text".to_string());
                if text.is_some() {
                    let text = try!(from_json(text.unwrap()));
                    let at = try!(from_json(pop!(map, "at", "the text")));
                    let size = match map.remove(&"size".to_string()) {
                        Some(size) => match num(&size) {
                            Some(size) if size > 0.0 => size as f32,
                            Some(_) => return Err(format!("nonpositive `size` in the text")),
                            None => fail_with_json!(size, "a text size"),
                        },
                        None => NROWS as f32,
                    };
                    let anchor = match map.remove(&"anchor".to_string()) {
                        Some(String(s)) => match s[] {
                            "left"   => (0.0, 0.0),
                            "center" => (0.5, 0.0),
                            "right"  => (1.0, 0.0),
                            _ => {
                                return Err(format!("unrecognized anchor type `{}` in the text", s));
                            }
                        },
                        Some(List(l)) => match l[] {
                            [ref x, ref y] => match (num(x), num(y)) {
                                (Some(x), Some(y)) => (x as f32, y as f32),
                                _ => return Err(format!("expected an anchor pair in the list")),
                            },
                            _ => { return Err(format!("expected an anchor pair in the list")); }
                        },
                        Some(anchor) => fail_with_json!(anchor, "an anchor"),
                        None => (0.0, 0.0),
                    };
                    let color = try!(from_json(pop!(map, "color", "the text")));
                    let zerocolor = match map.remove(&"zerocolor".to_string()) {
                        Some(zerocolor) => Some(try!(from_json(zerocolor))),
                        None => None,
                    };
                    ensure_empty!(map, "the text");
                    return Ok(Text { at: at, size: size, anchor: anchor,
                                     color: color, zerocolor: zerocolor, text: text });
                }

                // {"$clip": [[px,py],[qx,qy]]}
                let clip = map.remove(&"$clip".to_string());
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
                        let clipto = map.remove(&$key.to_string());
                        if clipto.is_some() {
                            let v = try!(from_json(clipto.unwrap()));
                            ensure_empty!(map, "the reclipping command");
                            return Ok(Clip { at: $num_to_rect(v) });
                        }
                    })
                )
                fn invert(e: Expr) -> Expr { Expr::sub(ENum(RatioNum::one()), e) }
                clip_to!("$cliptop",
                         |h| Rect { p: Pos { x: Expr::zero(), y: h },
                                    q: Pos { x: Expr::one(), y: Expr::one() } })
                clip_to!("$clipbottom",
                         |h| Rect { p: Pos { x: Expr::zero(), y: Expr::zero() },
                                    q: Pos { x: Expr::one(), y: invert(h) } })
                clip_to!("$clipleft",
                         |w| Rect { p: Pos { x: w, y: Expr::zero() },
                                    q: Pos { x: Expr::one(), y: Expr::one() } })
                clip_to!("$clipright",
                         |w| Rect { p: Pos { x: Expr::zero(), y: Expr::zero() },
                                    q: Pos { x: invert(w), y: Expr::one() } })

                // {"$$": "even?", ...} etc. (delegated to Block)
                Ok(NodeBlock(try!(from_json(Object(map)))))
            },

            _ => fail_with_json!(json, "a node")
        }
    }
}

impl FromJson for Skin {
    fn from_json(json: Json) -> Result<Skin,String> {
        match json {
            // {"scalars": {...}, "nodes": [...]}
            Object(mut map) => {
                let mut scalars = HashMap::new();
                match map.remove(&"scalars".to_string()) {
                    Some(Object(scalarmap)) => {
                        for (k, v) in scalarmap.into_iter() {
                            scalars.insert(k.into_string(), try!(from_json(v)));
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
pub fn load_skin<T: Buffer>(f: &mut T) -> Result<Skin,String> {
    let json = match cson::reader::Reader::new(f).parse_document() {
        Ok(cson) => cson.to_json(),
        Err(err) => { return Err(err.to_string()); }
    };
    from_json(json)
}

