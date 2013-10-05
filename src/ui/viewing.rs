// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Viewer portion of the game play screen. Also shared by exclusive modes.

use ext::sdl::mpeg;
use format::obj::{NLAYERS, BGALayer, Layer1, Layer2, Layer3};
use util::gl::{Texture, TexturedDrawingTraits};
use engine::resource::{BGAW, BGAH};
use engine::resource::{ImageResource, NoImage, Image, Movie};
use engine::player::{BGAState, initial_bga_state, Player};
use ui::common::{Ticker, update_line};
use ui::screen::Screen;
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PopScene};

/// Similar to `BGAState` but also has a set of textures used to render the BGA.
struct BGARenderState {
    state: BGAState,
    textures: ~[Texture]
}

trait Uploadable {
    /// Uploads an associated surface to the texture if any.
    fn upload_to_texture(&self, texture: &Texture);
    /// Returns true if the resource should be updated continuously (i.e. movies or animations).
    fn should_always_upload(&self) -> bool;
}

impl Uploadable for ImageResource {
    fn upload_to_texture(&self, texture: &Texture) {
        match *self {
            NoImage => {}
            Image(surface) | Movie(surface,_) => {
                texture.upload_surface(surface, false, false);
            }
        }
    }

    fn should_always_upload(&self) -> bool {
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
                Err(err) => die!("Texture::new failed: {}", err)
            };
            for &iref in iref.iter() {
                imgres[**iref].upload_to_texture(&texture);
            }
            texture
        };
        BGARenderState { state: state, textures: textures }
    }

    /// Updates the BGA state. This method prepares given image resources for the next rendering,
    /// notably by starting and stopping the movie playback and uploading textures as needed.
    pub fn update(&mut self, current: &BGAState, imgres: &[ImageResource]) {
        for layer in range(0, NLAYERS) {
            // TODO this design can't handle the case that a BGA layer is updated to the same
            // image reference, which should rewind the movie playback.
            if self.state[layer] != current[layer] {
                for &iref in self.state[layer].iter() {
                    imgres[**iref].stop_movie();
                }
                for &iref in current[layer].iter() {
                    imgres[**iref].start_movie();
                    imgres[**iref].upload_to_texture(&self.textures[layer]);
                }
            } else {
                for &iref in self.state[layer].iter() {
                    if imgres[**iref].should_always_upload() {
                        imgres[**iref].upload_to_texture(&self.textures[layer]);
                    }
                }
            }
        }
        self.state = *current;
    }

    /// Renders the image resources for the specified layers to the specified region of `screen`.
    pub fn render(&self, screen: &Screen, layers: &[BGALayer], x: f32, y: f32, w: f32, h: f32) {
        // the BGA area should have been already filled to black.
        for &layer in layers.iter() {
            if self.state[layer as uint].is_some() {
                do screen.draw_textured(&self.textures[layer as uint]) |d| {
                    d.rect(x, y, x + w, y + h);
                }
            }
        }
    }
}

/// Text-only viewing scene context. Used for the exclusive mode with BGA disabled.
pub struct TextualViewingScene {
    /// Current game play states.
    player: Player,
    /// Ticker used for printing to the console.
    ticker: Ticker,
}

impl TextualViewingScene {
    /// Creates a new text-only viewing scene.
    pub fn new(player: Player) -> ~TextualViewingScene {
        ~TextualViewingScene { player: player, ticker: Ticker() }
    }
}

impl Scene for TextualViewingScene {
    fn activate(&mut self) -> SceneCommand { Continue }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new().fpslimit(20) }

    fn tick(&mut self) -> SceneCommand {
        if self.player.tick() {Continue} else {PopScene}
    }

    fn render(&self) {
        if !self.player.opts.showinfo { return; }

        let elapsed = (self.player.now - self.player.origintime) / 100;
        let duration = (self.player.duration * 10.0) as uint;
        update_line(format!("{:02}:{:02}.{} / {:02}:{:02}.{} (@{pos:9.4}) | \
                             BPM {bpm:6.2} | {lastcombo} / {nnotes} notes",
                            elapsed/600, elapsed/10%60, elapsed%10,
                            duration/600, duration/10%60, duration%10,
                            pos = self.player.cur.loc.vpos, bpm = *self.player.bpm,
                            lastcombo = self.player.lastcombo, nnotes = self.player.infos.nnotes));
    }

    fn deactivate(&mut self) {
        update_line("");
    }

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}

/// BGA-only viewing scene context. Used for the exclusive mode with BGA enabled.
pub struct ViewingScene {
    /// The underlying text-only viewing scene context (as the BGA-only viewing scene lacks
    /// the on-screen display).
    parent: ~TextualViewingScene,
    /// Display screen.
    screen: Screen,
    /// Image resources.
    imgres: ~[ImageResource],
    /// Currently known state of BGAs.
    lastbga: BGARenderState,
}

impl ViewingScene {
    /// Creates a new BGA-only scene context from the pre-created screen (usually by `init_video`)
    /// and pre-loaded image resources.
    pub fn new(screen: Screen, imgres: ~[ImageResource], player: Player) -> ~ViewingScene {
        let bgastate = BGARenderState::new(imgres);
        ~ViewingScene { parent: TextualViewingScene::new(player),
                        screen: screen, imgres: imgres, lastbga: bgastate }
    }
}

impl Scene for ViewingScene {
    fn activate(&mut self) -> SceneCommand { self.parent.activate() }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new() }

    fn tick(&mut self) -> SceneCommand {
        let cmd = self.parent.tick();
        self.lastbga.update(&self.parent.player.bga, self.imgres);
        cmd
    }

    fn render(&self) {
        self.screen.clear();

        let layers = &[Layer1, Layer2, Layer3];
        self.lastbga.render(&self.screen, layers, 0.0, 0.0, BGAW as f32, BGAH as f32);
        self.screen.swap_buffers();

        self.parent.render();
    }

    fn deactivate(&mut self) { self.parent.deactivate() }

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}
