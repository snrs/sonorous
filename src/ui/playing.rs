// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Game play screen. Renders the screen from `engine::player::Player` state.

use std::{int, uint, cmp, num};

use format::obj::*;
use util::gl::{Texture, ShadedDrawing, ShadedDrawingTraits, TexturedDrawing, TexturedDrawingTraits};
use util::gfx::*;
use util::bmfont::{FontDrawingUtils, LeftAligned, Centered};
use engine::keyspec::*;
use engine::resource::{BGAW, BGAH};
use engine::resource::ImageResource;
use engine::player::{MISS, MAXGAUGE, Player};
use ui::screen::Screen;
use ui::init::{SCREENW, SCREENH};
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PopScene, ReplaceScene};
use ui::viewing::BGARenderState;
use ui::playresult::PlayResultScene;

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
fn create_sprite(leftmost: uint, rightmost: Option<uint>, styles: &[(Lane,LaneStyle)]) -> Texture {
    let sprite = match new_surface(SCREENW + 400, SCREENH) {
        Ok(surface) => surface,
        Err(err) => die!("new_surface failed: %s", err)
    };

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
    sprite.fill_area((10, SCREENH-36), (leftmost, 1), RGB(0x40,0x40,0x40));

    // erase portions of panels left unused
    let leftgap = leftmost + 20;
    let rightgap = rightmost.map_default(SCREENW, |x| x - 20);
    let gapwidth = rightgap - leftgap;
    let black = RGB(0,0,0);
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

    match Texture::from_owned_surface(sprite, false, false) {
        Ok(tex) => tex,
        Err(err) => die!("Texture::from_owned_surface failed: %s", err)
    }
}

/// Game play scene context. Used for the normal game play and automatic play mode.
pub struct PlayingScene {
    /// Game play state with various non-graphic resources.
    player: Player,
    /// Sprite texture generated by `create_sprite`.
    sprite: Texture,
    /// Display screen.
    screen: Screen,
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

impl PlayingScene {
    /// Creates a new game play scene from the player, pre-allocated (usually by `init_video`)
    /// screen and pre-loaded image resources. Other resources including pre-loaded sound resources
    /// are included in the `player`.
    pub fn new(player: Player, screen: Screen,
               imgres: ~[ImageResource]) -> Result<~PlayingScene,~str> {
        let (leftmost, rightmost, styles) = match build_lane_styles(player.keyspec) {
            Ok(styles) => styles,
            Err(err) => { return Err(err); }
        };
        let centerwidth = rightmost.get_or_default(SCREENW) - leftmost;
        let bgax = leftmost + (centerwidth - BGAW) / 2;
        let bgay = (SCREENH - BGAH) / 2;
        let sprite = create_sprite(leftmost, rightmost, styles);
        let bgastate = BGARenderState::new(imgres);

        Ok(~PlayingScene {
            player: player, sprite: sprite, screen: screen, imgres: imgres,
            leftmost: leftmost, rightmost: rightmost, lanestyles: styles, bgax: bgax, bgay: bgay,
            poorlimit: None, gradelimit: None, lastbga: bgastate,
        })
    }
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

impl Scene for PlayingScene {
    fn activate(&mut self) -> SceneCommand { Continue }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new() }

    fn tick(&mut self) -> SceneCommand {
        // TODO `QuitEvent` should be handled by the scene and not the player!
        if self.player.tick() {
            // update display states
            let mut poorlimit = self.poorlimit;
            let mut gradelimit = self.gradelimit;
            for self.player.lastgrade.iter().advance |&(grade,when)| {
                if grade == MISS {
                    // switches to the normal BGA after 600ms
                    poorlimit = poorlimit.merge(Some(when + 600), cmp::max);
                }
                // grade disappears after 700ms
                gradelimit = gradelimit.merge(Some(when + 700), cmp::max);
            }
            if poorlimit < Some(self.player.now) { poorlimit = None; }
            if gradelimit < Some(self.player.now) { gradelimit = None; }
            self.lastbga.update(&self.player.bga, self.imgres);
            *&mut self.poorlimit = poorlimit;
            *&mut self.gradelimit = gradelimit;

            Continue
        } else {
            if self.player.opts.is_autoplay() { return PopScene; }

            // check if the song reached the last gradable object (otherwise the game play was
            // terminated by the user)
            let nextgradable = self.player.cur.find_next_of_type(|obj| obj.is_gradable());
            if nextgradable.is_some() { return PopScene; }

            // otherwise move to the result screen
            ReplaceScene
        }
    }

    fn render(&self) {
        let W = SCREENW as f32;
        let H = SCREENH as f32;

        self.screen.clear();

        // render BGAs (should render before the lanes since lanes can overlap with BGAs)
        if self.player.opts.has_bga() {
            let layers = if self.poorlimit.is_some() {&[PoorBGA]} else {&[Layer1, Layer2, Layer3]};
            self.lastbga.render(&self.screen, layers, self.bgax as f32, self.bgay as f32,
                                BGAW as f32, BGAH as f32);
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

        //let bottom = self.player.cur.find(ActualPos, -0.03 / self.player.playspeed);
        //let top = self.player.cur.find(ActualPos, 1.22 / self.player.playspeed);
        let bottom = self.player.cur.clone();
        let top = self.player.cur.find(ActualPos, 1.25 / self.player.playspeed);

        let loc_to_y = |loc: &ObjLoc<float>| {
            let offset = loc.pos - bottom.loc.pos;
            (SCREENH-70) - (400.0 * self.player.playspeed * offset) as uint
        };

        do self.screen.draw_textured(&self.sprite) |d| {
            // if we are in the reverse motion, do not draw objects before the motion start.
            let localbottom = match self.player.reverse {
                Some(reverse) => reverse.clone(),
                None => bottom
            };

            // render objects
            for self.lanestyles.iter().advance |&(lane,style)| {
                if self.player.key_pressed(lane) { style.render_pressed_back(d); }

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

        do self.screen.draw_shaded_with_font |d| {
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
            if self.gradelimit.is_some() && self.player.lastgrade.is_some() {
                let gradelimit = self.gradelimit.get();
                let (lastgrade,_) = self.player.lastgrade.get();
                let (gradename,gradecolor) = GRADES[lastgrade as uint];
                let delta = (cmp::max(gradelimit - self.player.now, 400) as f32 - 400.0) / 15.0;
                let cx = (self.leftmost / 2) as f32; // avoids half-pixels
                let cy = H / 2.0 - delta; // offseted center
                d.string(cx, cy - 40.0, 2.0, Centered, gradename, gradecolor);
                if self.player.lastcombo > 1 {
                    d.string(cx, cy - 12.0, 1.0, Centered, fmt!("%u COMBO", self.player.lastcombo),
                             Gradient(RGB(0xff,0xff,0xff), RGB(0x80,0x80,0x80)));
                }
                if self.player.opts.is_autoplay() {
                    d.string(cx, cy + 2.0, 1.0, Centered, "(AUTO)",
                             Gradient(RGB(0xc0,0xc0,0xc0), RGB(0x40,0x40,0x40)));
                }
            }
        }

        do self.screen.draw_textured(&self.sprite) |d| {
            // restore panel from the sprite
            d.rect_area(0.0, 0.0, W, 30.0, 0.0, 0.0, W, 30.0);
            d.rect_area(0.0, H-80.0, W, H, 0.0, H-80.0, W, H);
        }
        do self.screen.draw_shaded_with_font |d| {
            let elapsed = (self.player.now - self.player.origintime) / 1000;
            let duration = self.player.duration as uint;
            let durationmsec = (self.player.duration * 1000.0) as uint;

            // render panel text
            let black = RGB(0,0,0);
            d.string(10.0, 8.0, 1.0, LeftAligned, fmt!("SCORE %07u", self.player.score), black);
            let nominalplayspeed = self.player.nominal_playspeed();
            d.string(5.0, H-78.0, 2.0, LeftAligned, fmt!("%4.1fx", nominalplayspeed), black);
            d.string((self.leftmost-94) as f32, H-35.0, 1.0, LeftAligned,
                     fmt!("%02u:%02u / %02u:%02u", elapsed/60, elapsed%60,
                                                   duration/60, duration%60), black);
            d.string(95.0, H-62.0, 1.0, LeftAligned,
                     fmt!("@%9.4f", self.player.cur.loc.vpos), black);
            d.string(95.0, H-78.0, 1.0, LeftAligned,
                     fmt!("BPM %6.2f", *self.player.bpm), black);
            let timetick = cmp::min(self.leftmost, (self.player.now - self.player.origintime) *
                                                   self.leftmost / durationmsec);
            d.glyph(6.0 + timetick as f32, H-52.0, 1.0, 95, RGB(0x40,0x40,0x40));

            // render gauge
            if !self.player.opts.is_autoplay() {
                // draw the gauge bar
                let gray = RGB(0x40,0x40,0x40);
                d.rect(0.0, H-16.0, 368.0, H, gray);
                d.rect(4.0, H-12.0, 360.0, H-4.0, black);

                // cycles four times per measure, [0,40)
                let cycle = (160.0 * self.player.cur.loc.vpos).floor() % 40.0;
                let width = if self.player.gauge < 0 {0}
                            else {self.player.gauge * 400 / MAXGAUGE - (cycle as int)};
                let width = ::util::std::cmp::clamp(5, width, 360);
                let color = if self.player.gauge >= self.player.survival {RGB(0xc0,0,0)}
                            else {RGB(0xc0 - ((cycle * 4.0) as u8), 0, 0)};
                d.rect(4.0, H-12.0, 4.0 + width as f32, H-4.0, color);
            }
        }

        self.screen.swap_buffers();
    }

    fn deactivate(&mut self) {}

    fn consume(~self) -> ~Scene: {
        PlayResultScene::new(self.player) as ~Scene:
    }
}

