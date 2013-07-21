// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Game play screen. Renders the screen from `engine::player::Player` state.

use std::{int, uint, cmp, num};

use ext::sdl::mpeg;
use format::obj::*;
use util::gl::{Texture, ShadedDrawing, TexturedDrawing};
use util::gfx::*;
use util::bmfont::{Font, LeftAligned, Centered};
use engine::keyspec::*;
use engine::resource::{BGAW, BGAH};
use engine::resource::{ImageResource, NoImage, Image, Movie};
use engine::player::{BGAState, initial_bga_state, MISS, MAXGAUGE, Player};
use ui::common::{Ticker, update_line};
use ui::screen::Screen;
use ui::options::*;
use ui::init::{SCREENW, SCREENH};

/// Similar to `BGAState` but also has a set of textures used to render the BGA.
struct BGARenderState {
    state: BGAState,
    textures: ~[Texture]
}

trait Uploadable {
    /// Uploads an associated surface to the texture if any.
    pub fn upload_to_texture(&self, texture: &Texture);
    /// Returns true if the resource should be updated continuously (i.e. movies or animations).
    pub fn should_always_upload(&self) -> bool;
}

impl Uploadable for ImageResource {
    pub fn upload_to_texture(&self, texture: &Texture) {
        match *self {
            NoImage => {}
            Image(surface) | Movie(surface,_) => {
                texture.upload_surface(surface, false, false);
            }
        }
    }

    pub fn should_always_upload(&self) -> bool {
        match *self {
            NoImage | Image(_) => false,
            Movie(_,mpeg) => mpeg.status() == mpeg::SMPEG_PLAYING
        }
    }
}

impl BGARenderState {
    /// Creates an initial state and textures.
    pub fn new(imgres: &[ImageResource]) -> BGARenderState {
        let state = initial_bga_state();
        let textures = do state.map |&iref| {
            let texture = match Texture::new(BGAW, BGAH) {
                Ok(texture) => texture,
                Err(err) => die!("Texture::new failed: %s", err)
            };
            for iref.iter().advance |&iref| {
                imgres[**iref].upload_to_texture(&texture);
            }
            texture
        };
        BGARenderState { state: state, textures: textures }
    }

    /// Updates the BGA state. This method prepares given image resources for the next rendering,
    /// notably by starting and stopping the movie playback and uploading textures as needed.
    pub fn update(&mut self, current: &BGAState, imgres: &[ImageResource]) {
        for uint::range(0, NLAYERS) |layer| {
            // TODO this design can't handle the case that a BGA layer is updated to the same
            // image reference, which should rewind the movie playback.
            if self.state[layer] != current[layer] {
                for self.state[layer].iter().advance |&iref| {
                    imgres[**iref].stop_movie();
                }
                for current[layer].iter().advance |&iref| {
                    imgres[**iref].start_movie();
                    imgres[**iref].upload_to_texture(&self.textures[layer]);
                }
            } else {
                for self.state[layer].iter().advance |&iref| {
                    if imgres[**iref].should_always_upload() {
                        imgres[**iref].upload_to_texture(&self.textures[layer]);
                    }
                }
            }
        }
        self.state = *current;
    }

    /// Renders the image resources for the specified layers to the specified region of `screen`.
    pub fn render(&self, screen: &Screen, layers: &[BGALayer], x: f32, y: f32) {
        // the BGA area should have been already filled to black.
        for layers.iter().advance |&layer| {
            if self.state[layer as uint].is_some() {
                do screen.draw_textured(&self.textures[layer as uint]) |d| {
                    d.rect(x, y, x + BGAW as f32, y + BGAH as f32);
                }
            }
        }
    }
}

/// Display interface.
pub trait Display {
    /// Renders the current information from `player` to the screen or console. Called after
    /// each call to `Player::tick`.
    pub fn render(&mut self, player: &Player);
    /// Shows the game play result from `player` to the screen or console. Called only once.
    pub fn show_result(&self, player: &Player);
}

//----------------------------------------------------------------------------------------------
// graphic display

/// An appearance for each lane.
struct LaneStyle {
    /// The left position of the lane in the final screen.
    left: uint,
    /// The left position of the lane in the object sprite.
    spriteleft: uint,
    /// The left position of the lane in the bomb sprite.
    spritebombleft: uint,
    /// The width of lane.
    width: uint,
    /// The base color of object. The actual `Gradient` for drawing is derived from this color.
    basecolor: Color
}

impl LaneStyle {
    /// Constructs a new `LaneStyle` object from given key kind and the left (`Left(pos)`) or
    /// right (`Right(pos)`) position.
    pub fn from_kind(kind: KeyKind, pos: Either<uint,uint>) -> LaneStyle {
        let (spriteleft, spritebombleft, width, color) = match kind {
            WhiteKey    => ( 25,   0, 25, RGB(0x80,0x80,0x80)),
            WhiteKeyAlt => ( 50,   0, 25, RGB(0xf0,0xe0,0x80)),
            BlackKey    => ( 75,   0, 25, RGB(0x80,0x80,0xff)),
            Button1     => (130, 100, 30, RGB(0xe0,0xe0,0xe0)),
            Button2     => (160, 100, 30, RGB(0xff,0xff,0x40)),
            Button3     => (190, 100, 30, RGB(0x80,0xff,0x80)),
            Button4     => (220, 100, 30, RGB(0x80,0x80,0xff)),
            Button5     => (250, 100, 30, RGB(0xff,0x40,0x40)),
            Scratch     => (320, 280, 40, RGB(0xff,0x80,0x80)),
            FootPedal   => (360, 280, 40, RGB(0x80,0xff,0x80)),
        };
        let left = pos.either(|&left| left, |&right| right - width);
        LaneStyle { left: left, spriteleft: spriteleft, spritebombleft: spritebombleft,
                    width: width, basecolor: color }
    }

    /// Renders required object and bomb images to the sprite.
    pub fn render_to_sprite(&self, sprite: &Surface) {
        let left = self.spriteleft;
        let noteleft = self.spriteleft + SCREENW;
        let bombleft = self.spritebombleft + SCREENW;
        assert!(sprite.get_width() as uint >= cmp::max(noteleft, bombleft) + self.width);

        // render a background sprite (0 at top, <1 at bottom)
        let backcolor = Gradient { zero: RGB(0,0,0), one: self.basecolor };
        for uint::range(140, SCREENH - 80) |i| {
            sprite.fill_area((left, i), (self.width, 1), backcolor.blend(i as int - 140, 1000));
        }

        // render note and bomb sprites (1/2 at middle, 1 at border)
        let denom = self.width as int;
        let notecolor = Gradient { zero: RGB(0xff,0xff,0xff), one: self.basecolor };
        let bombcolor = Gradient { zero: RGB(0,0,0),          one: RGB(0xc0,0,0) };
        for uint::range(0, self.width / 2) |i| {
            let num = (self.width - i) as int;
            sprite.fill_area((noteleft+i, 0), (self.width-i*2, SCREENH),
                             notecolor.blend(num, denom));
            sprite.fill_area((bombleft+i, 0), (self.width-i*2, SCREENH),
                             bombcolor.blend(num, denom));
        }
    }

    /// Clears the lane background.
    pub fn clear_back(&self, d: &mut ShadedDrawing) {
        d.rect(self.left as f32, 30.0,
               (self.left + self.width) as f32, SCREENH as f32 - 80.0, RGB(0,0,0));
    }

    /// Renders the key-pressed lane background to the screen from the sprite.
    pub fn render_pressed_back(&self, d: &mut TexturedDrawing) {
        d.rect_area(self.left as f32, 140.0,
                    (self.left + self.width) as f32, SCREENH as f32 - 80.0,
                    self.spriteleft as f32, 140.0,
                    (self.spriteleft + self.width) as f32, SCREENH as f32 - 80.0);
    }

    /// Renders an object to the screen from the sprite.
    pub fn render_note(&self, d: &mut TexturedDrawing, top: uint, bottom: uint) {
        d.rect_area(self.left as f32, top as f32,
                    (self.left + self.width) as f32, bottom as f32,
                    (self.spriteleft + SCREENW) as f32, 0.0,
                    (self.spriteleft + self.width + SCREENW) as f32, bottom as f32);
    }

    /// Renders a bomb object to the screen from the sprite.
    pub fn render_bomb(&self, d: &mut TexturedDrawing, top: uint, bottom: uint) {
        d.rect_area(self.left as f32, top as f32,
                    (self.left + self.width) as f32, bottom as f32,
                    (self.spritebombleft + SCREENW) as f32, 0.0,
                    (self.spritebombleft + self.width + SCREENW) as f32, bottom as f32);
    }
}

/// Builds a list of `LaneStyle`s from the key specification.
fn build_lane_styles(keyspec: &KeySpec) ->
                                Result<(uint, Option<uint>, ~[(Lane,LaneStyle)]), ~str> {
    let mut leftmost = 0;
    let mut rightmost = SCREENW;
    let mut styles = ~[];
    for keyspec.each_left_lanes |&lane| {
        let kind = keyspec.kinds[*lane];
        assert!(kind.is_some());
        let kind = kind.get();
        let style = LaneStyle::from_kind(kind, Left(leftmost));
        styles.push((lane, style));
        leftmost += style.width + 1;
        if leftmost > SCREENW - 20 {
            return Err(~"The screen can't hold that many lanes");
        }
    }
    for keyspec.each_right_lanes |&lane| {
        let kind = keyspec.kinds[*lane];
        assert!(kind.is_some());
        let kind = kind.get();
        let style = LaneStyle::from_kind(kind, Right(rightmost));
        styles.push((lane, style));
        if rightmost < leftmost + 40 {
            return Err(~"The screen can't hold that many lanes");
        }
        rightmost -= style.width + 1;
    }
    let mut rightmost = if rightmost == SCREENW {None} else {Some(rightmost)};

    // move lanes to the center if there are too small number of lanes
    let cutoff = 165;
    if leftmost < cutoff {
        for uint::range(0, keyspec.split) |i| {
            let (lane, style) = styles[i];
            let mut style = style;
            style.left += (cutoff - leftmost) / 2;
            styles[i] = (lane, style);
        }
        leftmost = cutoff;
    }
    if rightmost.map_default(false, |&x| x > SCREENW - cutoff) {
        for uint::range(keyspec.split, styles.len()) |i| {
            let (lane, style) = styles[i];
            let mut style = style;
            style.left -= (rightmost.get() - (SCREENW - cutoff)) / 2;
            styles[i] = (lane, style);
        }
        rightmost = Some(SCREENW - cutoff);
    }

    Ok((leftmost, rightmost, styles))
}

/// Creates a sprite.
fn create_sprite(opts: &Options, leftmost: uint, rightmost: Option<uint>,
                 styles: &[(Lane,LaneStyle)]) -> Texture {
    let sprite = match new_surface(SCREENW + 400, SCREENH) {
        Ok(surface) => surface,
        Err(err) => die!("new_surface failed: %s", err)
    };
    let black = RGB(0,0,0);
    let gray = RGB(0x40,0x40,0x40); // gray used for separators

    // render notes and lane backgrounds
    for styles.iter().advance |&(_lane,style)| {
        style.render_to_sprite(sprite);
    }

    // render panels
    do sprite.with_pixels |pixels| {
        let topgrad = Gradient { zero: RGB(0x60,0x60,0x60), one: RGB(0xc0,0xc0,0xc0) };
        let botgrad = Gradient { zero: RGB(0x40,0x40,0x40), one: RGB(0xc0,0xc0,0xc0) };
        for int::range(-244, 556) |j| {
            for int::range(-10, 20) |i| {
                let c = (i*2+j*3+750) % 2000;
                put_pixel(pixels, (j+244) as uint, (i+10) as uint, // XXX incorrect lifetime
                          topgrad.blend(850 - num::abs(c-1000), 700));
            }
            for int::range(-20, 60) |i| {
                let c = (i*3+j*2+750) % 2000;
                let bottom = (SCREENH - 60) as int;
                put_pixel(pixels, (j+244) as uint, (i+bottom) as uint, // XXX incorrect lifetime
                          botgrad.blend(850 - num::abs(c-1000), 700));
            }
        }
    }
    sprite.fill_area((10, SCREENH-36), (leftmost, 1), gray);

    // erase portions of panels left unused
    let leftgap = leftmost + 20;
    let rightgap = rightmost.map_default(SCREENW, |x| x - 20);
    let gapwidth = rightgap - leftgap;
    sprite.fill_area((leftgap, 0), (gapwidth, 30), black);
    sprite.fill_area((leftgap, SCREENH-80), (gapwidth, 80), black);
    do sprite.with_pixels |pixels| {
        for uint::range(0, 20) |i| {
            for uint::range_rev(20, 0) |j| {
                if i*i + j*j <= 400 { break; } // circled border
                put_pixel(pixels, leftmost + j, 10 + i, black); // XXX incorrect lifetime
                put_pixel(pixels, leftmost + j, (SCREENH-61) - i, black); // XXX
                for rightmost.iter().advance |&right| {
                    put_pixel(pixels, (right-j) - 1, 10 + i, black); // XXX incorrect lifetime
                    put_pixel(pixels, (right-j) - 1, (SCREENH-61) - i, black); // XXX
                }
            }
        }
    }

    // draw the gauge bar if needed
    if !opts.is_autoplay() {
        sprite.fill_area((0, SCREENH-16), (368, 16), gray);
        sprite.fill_area((4, SCREENH-12), (360, 8), black);
    }

    match Texture::from_owned_surface(sprite, false, false) {
        Ok(tex) => tex,
        Err(err) => die!("Texture::from_owned_surface failed: %s", err)
    }
}

/// Full-featured graphic display. Used for the normal game play and automatic play mode.
pub struct GraphicDisplay {
    /// Sprite texture generated by `create_sprite`.
    sprite: Texture,
    /// Display screen.
    screen: Screen,
    /// Bitmap font.
    font: ~Font,
    /// Image resources.
    imgres: ~[ImageResource],

    /// The leftmost X coordinate of the area next to the lanes, that is, the total width of
    /// left-hand-side lanes.
    leftmost: uint,
    /// The rightmost X coordinate of the area next to the lanes, that is, the screen width
    /// minus the total width of right-hand-side lanes if any. `None` indicates the absence of
    /// right-hand-side lanes.
    rightmost: Option<uint>,
    /// The order and appearance of lanes.
    lanestyles: ~[(Lane,LaneStyle)],
    /// The left coordinate of the BGA.
    bgax: uint,
    /// The top coordinate of the BGA.
    bgay: uint,

    /// If not `None`, indicates that the POOR BGA should be displayed until this timestamp.
    poorlimit: Option<uint>,
    /// If not `None`, indicates that the grading information should be displayed until
    /// this timestamp.
    gradelimit: Option<uint>,
    /// Currently known state of BGAs.
    lastbga: BGARenderState,
}

/// Creates a new graphic display from the options, key specification, pre-allocated (usually
/// by `init_video`) screen, pre-created bitmap fonts and pre-loaded image resources. The last
/// three are owned by the display, others are not (in fact, should be owned by `Player`).
pub fn GraphicDisplay(opts: &Options, keyspec: &KeySpec, screen: Screen, font: ~Font,
                      imgres: ~[ImageResource]) -> Result<GraphicDisplay,~str> {
    let (leftmost, rightmost, styles) = match build_lane_styles(keyspec) {
        Ok(styles) => styles,
        Err(err) => { return Err(err); }
    };
    let centerwidth = rightmost.get_or_default(SCREENW) - leftmost;
    let bgax = leftmost + (centerwidth - BGAW) / 2;
    let bgay = (SCREENH - BGAH) / 2;
    let sprite = create_sprite(opts, leftmost, rightmost, styles);
    let bgastate = BGARenderState::new(imgres);

    let display = GraphicDisplay {
        sprite: sprite, screen: screen, font: font, imgres: imgres,
        leftmost: leftmost, rightmost: rightmost, lanestyles: styles, bgax: bgax, bgay: bgay,
        poorlimit: None, gradelimit: None, lastbga: bgastate,
    };

    Ok(display)
}

/// The list of grade names and corresponding color scheme.
static GRADES: &'static [(&'static str,Gradient)] = &[
    // Rust: can we just use `Gradient()`???
    ("MISS",  Gradient { zero: RGB(0xff,0xc0,0xc0), one: RGB(0xff,0x40,0x40) }),
    ("BAD",   Gradient { zero: RGB(0xff,0xc0,0xff), one: RGB(0xff,0x40,0xff) }),
    ("GOOD",  Gradient { zero: RGB(0xff,0xff,0xc0), one: RGB(0xff,0xff,0x40) }),
    ("GREAT", Gradient { zero: RGB(0xc0,0xff,0xc0), one: RGB(0x40,0xff,0x40) }),
    ("COOL",  Gradient { zero: RGB(0xc0,0xc0,0xff), one: RGB(0x40,0x40,0xff) }),
];

impl Display for GraphicDisplay {
    fn render(&mut self, player: &Player) {
        let font = &*self.font;

        let W = SCREENW as f32;
        let H = SCREENH as f32;

        // update display states
        let mut poorlimit = self.poorlimit;
        let mut gradelimit = self.gradelimit;
        for player.lastgrade.iter().advance |&(grade,when)| {
            if grade == MISS {
                // switches to the normal BGA after 600ms
                poorlimit = poorlimit.merge(Some(when + 600), cmp::max);
            }
            // grade disappears after 700ms
            gradelimit = gradelimit.merge(Some(when + 700), cmp::max);
        }
        if poorlimit < Some(player.now) { poorlimit = None; }
        if gradelimit < Some(player.now) { gradelimit = None; }
        self.lastbga.update(&player.bga, self.imgres);
        *&mut self.poorlimit = poorlimit;
        *&mut self.gradelimit = gradelimit;

        self.screen.clear();

        // render BGAs (should render before the lanes since lanes can overlap with BGAs)
        if player.opts.has_bga() {
            let layers = if poorlimit.is_some() {&[PoorBGA]} else {&[Layer1, Layer2, Layer3]};
            self.lastbga.render(&self.screen, layers, self.bgax as f32, self.bgay as f32);
        }

        do self.screen.draw_shaded |d| {
            // fill the lanes to the border color
            d.rect(0.0, 30.0, self.leftmost as f32, H-80.0, RGB(0x40,0x40,0x40));
            for self.rightmost.iter().advance |&rightmost| {
                d.rect(rightmost as f32, 30.0, H, 520.0, RGB(0x40,0x40,0x40));
            }

            // clear the lanes to the background color
            for self.lanestyles.iter().advance |&(_lane,style)| {
                style.clear_back(d);
            }
        }

        //let bottom = player.cur.find(ActualPos, -0.03 / player.playspeed);
        //let top = player.cur.find(ActualPos, 1.22 / player.playspeed);
        let bottom = player.cur.clone();
        let top = player.cur.find(ActualPos, 1.25 / player.playspeed);

        let loc_to_y = |loc: &ObjLoc<float>| {
            let offset = loc.pos - bottom.loc.pos;
            (SCREENH-70) - (400.0 * player.playspeed * offset) as uint
        };

        do self.screen.draw_textured(&self.sprite) |d| {
            // if we are in the reverse motion, do not draw objects before the motion start.
            let localbottom = match player.reverse {
                Some(reverse) => reverse.clone(),
                None => bottom
            };

            // render objects
            for self.lanestyles.iter().advance |&(lane,style)| {
                if player.key_pressed(lane) { style.render_pressed_back(d); }

                let front = do localbottom.find_next_of_type |&obj| {
                    obj.object_lane() == Some(lane) && obj.is_renderable()
                };
                if front.is_none() { loop; }
                let front = front.get();

                // LN starting before the bottom and ending after the top
                if front.loc.vpos > top.loc.vpos && front.is_lndone() {
                    style.render_note(d, 30, SCREENH - 80);
                } else {
                    let mut nextbottom = None;
                    for front.upto(&top) |ptr| {
                        let y = loc_to_y(&ptr.loc);
                        match ptr.data() {
                            LNStart(lane0,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                nextbottom = Some(y);
                            }
                            LNDone(lane0,_) if lane0 == lane => {
                                let bottom = SCREENH-80;
                                style.render_note(d, y, nextbottom.get_or_default(bottom));
                                nextbottom = None;
                            }
                            Visible(lane0,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                style.render_note(d, y-5, y);
                            }
                            Bomb(lane0,_,_) if lane0 == lane => {
                                assert!(nextbottom.is_none());
                                style.render_bomb(d, y-5, y);
                            }
                            _ => {}
                        }
                    }

                    for nextbottom.iter().advance |&y| {
                        style.render_note(d, 30, y);
                    }
                }
            }
        }

        do self.screen.draw_shaded |d| {
            // render non-note objects (currently, measure bars)
            for bottom.upto(&top) |ptr| {
                match ptr.data() {
                    MeasureBar => {
                        let y = loc_to_y(&ptr.loc) as f32;
                        d.rect(0.0, y, self.leftmost as f32, y + 1.0, RGB(0xc0,0xc0,0xc0));
                        for self.rightmost.iter().advance |&rightmost| {
                            d.rect(rightmost as f32, y,
                                   (SCREENW - rightmost) as f32, y + 1.0, RGB(0xc0,0xc0,0xc0));
                        }
                    }
                    _ => {}
                }
            }

            // render grading text
            if gradelimit.is_some() && player.lastgrade.is_some() {
                let gradelimit = gradelimit.get();
                let (lastgrade,_) = player.lastgrade.get();
                let (gradename,gradecolor) = GRADES[lastgrade as uint];
                let delta = (cmp::max(gradelimit - player.now, 400) as f32 - 400.0) / 15.0;
                let cx = (self.leftmost / 2) as f32; // avoids half-pixels
                let cy = H / 2.0 - delta; // offseted center
                font.draw_string(d, cx, cy - 40.0, 2.0, Centered, gradename, gradecolor);
                if player.lastcombo > 1 {
                    font.draw_string(d, cx, cy - 12.0, 1.0, Centered,
                                     fmt!("%u COMBO", player.lastcombo),
                                     Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80)));
                }
                if player.opts.is_autoplay() {
                    font.draw_string(d, cx, cy + 2.0, 1.0, Centered, "(AUTO)",
                                     Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x40,0x40,0x40)));
                }
            }
        }

        do self.screen.draw_textured(&self.sprite) |d| {
            // restore panel from the sprite
            d.rect_area(0.0, 0.0, W, 30.0, 0.0, 0.0, W, 30.0);
            d.rect_area(0.0, H-80.0, W, H, 0.0, H-80.0, W, H);
        }
        do self.screen.draw_shaded |d| {
            let elapsed = (player.now - player.origintime) / 1000;
            let duration = player.duration as uint;
            let durationmsec = (player.duration * 1000.0) as uint;

            // render panel text
            let black = RGB(0,0,0);
            font.draw_string(d, 10.0, 8.0, 1.0, LeftAligned,
                             fmt!("SCORE %07u", player.score), black);
            let nominalplayspeed = player.nominal_playspeed();
            font.draw_string(d, 5.0, H-78.0, 2.0, LeftAligned,
                             fmt!("%4.1fx", nominalplayspeed), black);
            font.draw_string(d, (self.leftmost-94) as f32, H-35.0, 1.0, LeftAligned,
                             fmt!("%02u:%02u / %02u:%02u", elapsed/60, elapsed%60,
                                                           duration/60, duration%60), black);
            font.draw_string(d, 95.0, H-62.0, 1.0, LeftAligned,
                             fmt!("@%9.4f", player.cur.loc.vpos), black);
            font.draw_string(d, 95.0, H-78.0, 1.0, LeftAligned,
                             fmt!("BPM %6.2f", *player.bpm), black);
            let timetick = cmp::min(self.leftmost, (player.now - player.origintime) *
                                                   self.leftmost / durationmsec);
            font.draw_glyph(d, 6.0 + timetick as f32, H-52.0, 1.0, 95, RGB(0x40,0x40,0x40));

            // render gauge
            if !player.opts.is_autoplay() {
                // cycles four times per measure, [0,40)
                let cycle = (160.0 * player.cur.loc.vpos).floor() % 40.0;
                let width = if player.gauge < 0 {0}
                            else {player.gauge * 400 / MAXGAUGE - (cycle as int)};
                let width = ::util::std::cmp::clamp(5, width, 360);
                let color = if player.gauge >= player.survival {RGB(0xc0,0,0)}
                            else {RGB(0xc0 - ((cycle * 4.0) as u8), 0, 0)};
                d.rect(4.0, H-12.0, 4.0 + width as f32, H-4.0, color);
            }
        }

        self.screen.swap_buffers();
    }

    fn show_result(&self, player: &Player) {
        if player.opts.is_autoplay() { return; }

        // check if the song reached the last gradable object (otherwise the game play was
        // terminated by the user)
        let nextgradable = player.cur.find_next_of_type(|obj| obj.is_gradable());
        if nextgradable.is_some() { return; }

        if player.gauge >= player.survival {
            println(fmt!("*** CLEARED! ***\n\
                          COOL  %4u    GREAT %4u    GOOD  %4u\n\
                          BAD   %4u    MISS  %4u    MAX COMBO %u\n\
                          SCORE %07u (max %07d)",
                         player.gradecounts[4], player.gradecounts[3],
                         player.gradecounts[2], player.gradecounts[1],
                         player.gradecounts[0], player.bestcombo,
                         player.score, player.infos.maxscore));
        } else {
            println("YOU FAILED!");
        }
    }
}

/// Text-only display. Used for the exclusive mode with BGA disabled.
pub struct TextDisplay {
    /// Ticker used for printing to the console.
    ticker: Ticker
}

/// Creates a new text-only display.
pub fn TextDisplay() -> TextDisplay {
    TextDisplay { ticker: Ticker() }
}

impl Display for TextDisplay {
    fn render(&mut self, player: &Player) {
        if !player.opts.showinfo { return; }

        do self.ticker.on_tick(player.now) {
            let elapsed = (player.now - player.origintime) / 100;
            let duration = (player.duration * 10.0) as uint;
            update_line(fmt!("%02u:%02u.%u / %02u:%02u.%u (@%9.4f) | BPM %6.2f | %u / %d notes",
                             elapsed/600, elapsed/10%60, elapsed%10,
                             duration/600, duration/10%60, duration%10,
                             player.cur.loc.vpos, *player.bpm,
                             player.lastcombo, player.infos.nnotes));
        }
    }

    fn show_result(&self, _player: &Player) {
        update_line("");
    }
}

/// BGA-only display. Used for the exclusive mode with BGA enabled.
pub struct BGAOnlyDisplay {
    /// The underlying text-only display (as the BGA-only display lacks the on-screen display).
    textdisplay: TextDisplay,
    /// Display screen.
    screen: Screen,
    /// Image resources.
    imgres: ~[ImageResource],
    /// Currently known state of BGAs.
    lastbga: BGARenderState,
}

/// Creates a new BGA-only display from the pre-created screen (usually by `init_video`) and
/// pre-loaded image resources.
pub fn BGAOnlyDisplay(screen: Screen, imgres: ~[ImageResource]) -> BGAOnlyDisplay {
    let bgastate = BGARenderState::new(imgres);
    BGAOnlyDisplay { textdisplay: TextDisplay(), screen: screen, imgres: imgres, lastbga: bgastate }
}

impl Display for BGAOnlyDisplay {
    fn render(&mut self, player: &Player) {
        self.screen.clear();
        self.lastbga.update(&player.bga, self.imgres);

        let layers = &[Layer1, Layer2, Layer3];
        self.lastbga.render(&self.screen, layers, 0.0, 0.0);
        self.screen.swap_buffers();

        self.textdisplay.render(player);
    }

    fn show_result(&self, player: &Player) {
        self.textdisplay.show_result(player);
    }
}

