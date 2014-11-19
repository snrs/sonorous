// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

/*!
 * Internal representation of Sonorous scene description language.
 *
 * The current scene description language is based on JSON serialization format.
 * Some examples and basic elements:
 *
 * ~~~~ {.notrust}
 * {"nodes": [
 *     "comments are (currently) written in the raw string.",
 *     "this is a temporary measure and will be replaced with the proper comments.",
 *
 *     "this renders a static rectangle.",
 *     {"$rect": null, "at": [[8,8], ["100%-8",44]], "color": "#808080"},
 *
 *     "this renders a static text.",
 *     {"$text": "Gallery", "at": [10,10], "size": 32, "color": "black"},
 *
 *     "the renderer has a clipping region, and this delimits the changes on it.",
 *     [
 *         "this changes the clipping region.",
 *         {"$clip": [[10,50],["100%","100%"]]},
 *
 *         "this loops over the hook. the hook provider may freely call each block.",
 *         {"$$": "images", "$then": [
 *             "this renders a dynamic textured rectangle.",
 *             "the hook provider can supply different images for `image` texture hook each time.",
 *             {"$rect": "image.thumb", "at": [[0,0], [32,32]]},
 *
 *             "this renders a dynamic text.",
 *             "again, the hook provider can supply different strings for scalar hook each time.",
 *             {"$text": {"$": "image.filename"}, "at": [40,2], "size": 16, "color": "white"},
 *
 *             "the block does not group the clipping region changes,",
 *             "so this command will apply to the next iteration.",
 *             {"$cliptop": 36}
 *         ], "$else": [
 *             "this gets rendered only when the `$then` block is never called.",
 *             {"$text": "Oops, no images.", "at": [0,0], "size": 16, "color": "gray"}
 *         ]},
 *
 *         "the block can be used in the `$text` command, and it can also be used as ",
 *         "the limited switching construct. no real conditional exists, though.",
 *         {"$text": ["Total ", {"$": "nimages"},
 *                    {"$$text": "nimages", "1": " image", "$default": " images"}],
 *          "at": [10,46], "size": 16, "color": [255,255,255]}
 *     ],
 *
 *     "due to the current format quirks, commas are NOT allowed at the end."
 * ]}
 * ~~~~
 */

use std::collections::HashMap;

use gfx::color::Color;
use gfx::ratio_num::RatioNum;
use gfx::skin::scalar::{Scalar, ImageSource};

/// An identifier to the hook. Different kind of hooks can share the same identifier.
#[deriving(PartialEq, Eq, Hash, Clone, Show)]
pub struct Id(pub String);

impl Id {
    /// Returns a slice of the identifier for a convenient matching.
    pub fn as_slice<'a>(&'a self) -> &'a str {
        let &Id(ref id) = self;
        id[]
    }
}

/// Numerical expression.
#[deriving(Clone, Show)]
pub enum Expr {
    Num(RatioNum<f32>),
    Scalar(Id),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

impl Expr {
    /// Returns a value of zero.
    pub fn zero() -> Expr {
        Expr::Num(RatioNum::zero())
    }

    /// Returns a value equal to the base value.
    pub fn one() -> Expr {
        Expr::Num(RatioNum::one())
    }

    /// Constructs an `Expr::Neg` node or any equivalent but possibly simpler expression.
    pub fn neg(e: Expr) -> Expr {
        match e {
            Expr::Num(v) => Expr::Num(-v),
            Expr::Neg(box e) => e,
            e => Expr::Neg(box e),
        }
    }

    /// Constructs an `Expr::Add` node or any equivalent but possibly simpler expression.
    pub fn add(lhs: Expr, rhs: Expr) -> Expr {
        match (lhs, rhs) {
            (Expr::Num(lhs), Expr::Num(rhs)) =>
                Expr::Num(lhs + rhs),
            (Expr::Add(box lhs, box Expr::Num(lhsv)), Expr::Num(rhs)) |
            (Expr::Add(box Expr::Num(lhsv), box lhs), Expr::Num(rhs)) =>
                Expr::Add(box lhs, box Expr::Num(lhsv + rhs)),
            (Expr::Sub(box lhs, box Expr::Num(lhsv)), Expr::Num(rhs)) =>
                Expr::Add(box lhs, box Expr::Num(rhs - lhsv)),
            (lhs, rhs) =>
                Expr::Add(box lhs, box rhs),
        }
    }

    /// Constructs an `Expr::Sub` node or any equivalent but possibly simpler expression.
    pub fn sub(lhs: Expr, rhs: Expr) -> Expr {
        match (lhs, rhs) {
            (Expr::Num(lhs), Expr::Num(rhs)) =>
                Expr::Num(lhs - rhs),
            (Expr::Add(box lhs, box Expr::Num(lhsv)), Expr::Num(rhs)) |
            (Expr::Add(box Expr::Num(lhsv), box lhs), Expr::Num(rhs)) =>
                Expr::Sub(box lhs, box Expr::Num(rhs - lhsv)),
            (Expr::Sub(box lhs, box Expr::Num(lhsv)), Expr::Num(rhs)) =>
                Expr::Sub(box lhs, box Expr::Num(lhsv + rhs)),
            (lhs, rhs) =>
                Expr::Sub(box lhs, box rhs),
        }
    }

    /// Constructs an `Expr::Mul` node or any equivalent but possibly simpler expression.
    pub fn mul(lhs: Expr, rhs: Expr) -> Expr {
        match (lhs, rhs) {
            (Expr::Num(lhs), Expr::Num(RatioNum { ratio: 0.0, num: rhs })) => Expr::Num(lhs * rhs),
            (Expr::Num(RatioNum { ratio: 0.0, num: lhs }), Expr::Num(rhs)) => Expr::Num(rhs * lhs),
            // other combinations of `Expr::Num`s are type errors, but we defer the error here
            (lhs, rhs) => Expr::Mul(box lhs, box rhs),
        }
    }

    /// Constructs an `Expr::Div` node or any equivalent but possibly simpler expression.
    pub fn div(lhs: Expr, rhs: Expr) -> Expr {
        match (lhs, rhs) {
            (Expr::Num(lhs), Expr::Num(RatioNum { ratio: 0.0, num: rhs })) => Expr::Num(lhs / rhs),
            // other combinations of `Expr::Num`s are type errors, but we defer the error here
            (lhs, rhs) => Expr::Div(box lhs, box rhs),
        }
    }
}

/// Two-dimensional position.
#[deriving(Clone, Show)]
pub struct Pos { pub x: Expr, pub y: Expr }

/// Two-dimensional rectangle.
#[deriving(Clone, Show)]
pub struct Rect { pub p: Pos, pub q: Pos }

impl Rect {
    /// Returns a new, full-screen clipping rectangle.
    pub fn new() -> Rect {
        Rect { p: Pos { x: Expr::Num(RatioNum::zero()), y: Expr::Num(RatioNum::zero()) },
               q: Pos { x: Expr::Num(RatioNum::one()), y: Expr::Num(RatioNum::one()) } }
    }
}

/// A block call generator.
#[deriving(Clone)]
pub enum Gen {
    // {"$$": "id", ...} maps to `block_hook`
    Hook(Id),
    // {"$$text": "id", ...} maps to `scalar_hook`
    // - the alternative with the exact string is called once
    // - due to the internal structure, a string starting with $ will only match to `$default`
    Text(Id),
    // {"$$len": "id", ...} maps to `scalar_hook`
    // - for non-empty string the alternative with the length (as a string) is called once
    // - for non-existant or empty string `$else` is called
    TextLen(Id),
}

impl Gen {
    /// Returns a slice of the identifier (if any) for the inspection.
    pub fn id<'a>(&'a self) -> &'a str {
        match *self {
            Gen::Hook(ref id) | Gen::Text(ref id) | Gen::TextLen(ref id) => id.as_slice()
        }
    }
}

/// The universal flow structure.
#[deriving(Clone)]
pub enum Block<T> {
    // conditional
    // - `then` is called multiple times with a fresh mapping
    // - `else_` is called once with an original mapping when `then` is never called
    Cond { gen: Gen, then: Option<T>, else_: Option<T> },
    // multi
    // - the hook can call other alternatives multiple times
    // - if there is no recognized alternative `default` gets called instead
    // - `else_` is called once with an original mapping when no alternative is called
    Multi { gen: Gen, map: HashMap<String,T>, default: Option<T>, else_: Option<T> },
}

/// The formatting specification for scalar text.
#[deriving(Clone)]
pub enum ScalarFormat {
    None,
    // ['+'] [".."] {'#'} {'0'} '0' [".0" {'0'}] [('*' <mult> | '/' <div>)]
    // e.g. `##000.00` prints 3.147 as `003.15`, 1234.5 as `1234.50` and -987654 as `87654.00`
    Num { sign: bool, minwidth: u8, maxwidth: u8, precision: u8, multiplier: f64 },
    // ['+'] [".."] {'#'} {'0'} '0' [":00" [":00"]] [".0" {'0'}] [('*' <mult> | '/' <div>)]
    // e.g. `0:00:00.0` prints 23456.7 as `6:30:56.7`, `##00:00.0` prints 23456.7 as `390:56.7`
    // note that `minwidth`/`maxwidth` here indicates those of most significant parts.
    Ms { sign: bool, minwidth: u8, maxwidth: u8, precision: u8, multiplier: f64 },
    Hms { sign: bool, minwidth: u8, maxwidth: u8, precision: u8, multiplier: f64 },
}

/// The text source for the `$text` node.
#[deriving(Clone)]
pub enum TextSource {
    Scalar(Id, ScalarFormat),
    Static(String),
    Block(Block<Box<TextSource>>),
    Concat(Vec<TextSource>),
}

/// The color source for the `color` field in `$rect`, `$line` and a part of `$text` nodes.
/// Maps to `gfx::color::Color`.
pub enum ColorSource {
    Scalar(Id),
    Static(Color),
    Block(Block<Box<ColorSource>>),
}

impl Clone for ColorSource {
    fn clone(&self) -> ColorSource {
        match *self {
            ColorSource::Scalar(ref id) => ColorSource::Scalar(id.clone()),
            ColorSource::Static(color) => ColorSource::Static(color),
            ColorSource::Block(ref block) => ColorSource::Block(block.clone()),
        }
    }
}

/// The linear color gradient source for the `color` field in the `$text` node.
/// Maps to `gfx::color::Gradient`.
#[deriving(Clone)]
pub enum GradientSource {
    FlatColor(ColorSource),
    Color(/* zero */ ColorSource, /* one */ ColorSource),
    Block(Block<Box<GradientSource>>),
}

/// The main skin commands.
#[deriving(Clone)]
pub enum Node {
    Nothing,
    Debug(String),
    ColoredLine { from: Pos, to: Pos, color: ColorSource, opacity: f32 },
    // colored rect at given position
    ColoredRect { at: Rect, color: ColorSource, opacity: f32 },
    // textured rect at given position, optionally clipped
    TexturedRect { tex: Id, at: Rect, colormod: ColorSource, opacity: f32, clip: Rect },
    // text with fixed anchor
    Text { at: Pos, size: f32, anchor: (f32,f32), color: GradientSource,
           zerocolor: Option<GradientSource>, text: TextSource }, // XXX no opacity yet
    // clipping group, resets the clipping region after the group
    Group(Vec<Node>),
    // reclipping command
    Clip { at: Rect },
    // block
    Block(Block<Vec<Node>>),
}

/// The top-level parsed skin data.
#[deriving(Clone)]
pub struct Skin {
    /// The predefined scalar values.
    pub scalars: HashMap<String,Scalar<'static>>,
    /// The list of commands.
    pub nodes: Vec<Node>,
}

impl Skin {
    /// Converts the relative paths in the scalar data into the absolute ones.
    pub fn make_absolute(self, base: &Path) -> Skin {
        let Skin { mut scalars, nodes } = self;
        for (_, v) in scalars.iter_mut() {
            match *v {
                Scalar::Image(ImageSource::Path(ref mut path), _clip) => {
                    *path = base.join(&*path);
                }
                _ => {}
            }
        }
        Skin { scalars: scalars, nodes: nodes }
    }
}

