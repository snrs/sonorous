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

use collections::HashMap;

use gfx::color::{Color, Gradient};
use gfx::skin::scalar::{Scalar, ImageScalar};

/// Numerical expression.
#[deriving(Eq,Show)]
pub enum Expr {
    ENum(f32),
    ERatioNum(f32, f32),
}

/// Two-dimensional position.
#[deriving(Show)]
pub struct Pos { pub x: Expr, pub y: Expr }

/// Two-dimensional rectangle.
#[deriving(Show)]
pub struct Rect { pub p: Pos, pub q: Pos }

/// An identifier to the hook. Different kind of hooks can share the same identifier.
#[deriving(Eq,TotalEq,Hash,Show)]
pub struct Id(pub ~str);

impl Id {
    /// Returns a slice of the identifier for a convenient matching.
    pub fn as_slice<'a>(&'a self) -> &'a str {
        let &Id(ref id) = self;
        id.as_slice()
    }
}

/// A block call generator.
pub enum Gen {
    // {"$$": "id", ...} maps to `block_hook`
    HookGen(Id),
    // {"$$text": "id", ...} maps to `scalar_hook`
    // - the alternative with the exact string is called once
    // - due to the internal structure, a string starting with $ will only match to `$default`
    TextGen(Id),
    // {"$$len": "id", ...} maps to `scalar_hook`
    // - for non-empty string the alternative with the length (as a string) is called once
    // - for non-existant or empty string `$else` is called
    TextLenGen(Id),
}

impl Gen {
    /// Returns a slice of the identifier (if any) for the inspection.
    pub fn id<'a>(&'a self) -> &'a str {
        match *self {
            HookGen(ref id) | TextGen(ref id) | TextLenGen(ref id) => id.as_slice()
        }
    }
}

/// The universal flow structure.
pub enum Block<T> {
    // conditional
    // - `then` is called multiple times with a fresh mapping
    // - `else_` is called once with an original mapping when `then` is never called
    CondBlock { pub gen: Gen, pub then: Option<T>, pub else_: Option<T> },
    // multi
    // - the hook can call other alternatives multiple times
    // - if there is no recognized alternative `default` gets called instead
    // - `else_` is called once with an original mapping when no alternative is called
    MultiBlock { pub gen: Gen, pub map: HashMap<~str,T>,
                 pub default: Option<T>, pub else_: Option<T> },
}

/// The formatting specification for scalar text.
pub enum ScalarFormat {
    NoFormat,
    // ['+'] [".."] {'#'} {'0'} '0' [".0" {'0'}] [('*' <mult> | '/' <div>)]
    // e.g. `##000.00` prints 3.147 as `003.15`, 1234.5 as `1234.50` and -987654 as `87654.00`
    NumFormat { pub sign: bool, pub minwidth: u8, pub maxwidth: u8,
                pub precision: u8, pub multiplier: f64 },
    // ['+'] [".."] {'#'} {'0'} '0' [":00" [":00"]] [".0" {'0'}] [('*' <mult> | '/' <div>)]
    // e.g. `0:00:00.0` prints 23456.7 as `6:30:56.7`, `##00:00.0` prints 23456.7 as `390:56.7`
    // note that `minwidth`/`maxwidth` here indicates those of most significant parts.
    MsFormat { pub sign: bool, pub minwidth: u8, pub maxwidth: u8,
               pub precision: u8, pub multiplier: f64 },
    HmsFormat { pub sign: bool, pub minwidth: u8, pub maxwidth: u8,
                pub precision: u8, pub multiplier: f64 },
}

/// The text source for the `$text` node.
pub enum TextSource {
    ScalarText(Id, ScalarFormat),
    StaticText(~str),
    TextBlock(Block<Box<TextSource>>),
    TextConcat(Vec<TextSource>),
}

/// The main skin commands.
pub enum Node {
    Nothing,
    Debug(~str),
    ColoredLine { pub from: Pos, pub to: Pos, pub color: Color },
    // colored rect at given position
    ColoredRect { pub at: Rect, pub color: Color },
    // textured rect at given position, optionally clipped
    TexturedRect { pub tex: Id, pub at: Rect, pub rgba: (u8,u8,u8,u8), pub clip: Option<Rect> },
    // text with fixed anchor
    Text { pub at: Pos, pub size: f32, pub anchor: (f32,f32),
           pub color: Gradient, pub text: TextSource },
    // clipping group, resets the clipping region after the group
    Group(Vec<Node>),
    // reclipping command
    Clip { pub at: Rect },
    // block
    Block(Block<Vec<Node>>),
}

/// The top-level parsed skin data.
pub struct Skin {
    /// The predefined scalar values.
    pub scalars: HashMap<~str,Scalar<'static>>,
    /// The list of commands.
    pub nodes: Vec<Node>,
}

impl Skin {
    /// Converts the relative paths in the scalar data into the absolute ones.
    pub fn make_absolute(self, base: &Path) -> Skin {
        let Skin { mut scalars, nodes } = self;
        for (_, v) in scalars.mut_iter() {
            match *v {
                ImageScalar(ref mut path) => { *path = base.join(&*path); }
                _ => {}
            }
        }
        Skin { scalars: scalars, nodes: nodes }
    }
}

