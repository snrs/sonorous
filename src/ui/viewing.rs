// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Viewer portion of the game play screen. Also shared by exclusive modes.

use std::rc::Rc;
use std::cell::RefCell;

use opengles::gl2 as gl;
use ext::smpeg::SMPEG_PLAYING;
use format::obj::{NLAYERS, BGALayer, BGARef};
use format::bms::ImageRef;
use gfx::color::RGBA;
use gfx::surface::SurfaceAreaUtil;
use gfx::gl::{Texture2D, PreparedSurface, FrameBuffer};
use gfx::draw::TexturedDrawingTraits;
use gfx::screen::Screen;
use engine::resource::{BGAW, BGAH, Imagelike};
use engine::player::{BGAState, initial_bga_state, Player};
use ui::common::{Ticker, update_line};
use ui::scene::{Scene, SceneOptions, SceneCommand};

trait Uploadable {
    /// Uploads an associated surface to the texture if any.
    fn upload_to_texture(&self, texture: &Texture2D);
    /// Returns true if the resource should be updated continuously (i.e. movies or animations).
    fn should_always_upload(&self) -> bool;
}

impl Uploadable for Imagelike {
    fn upload_to_texture(&self, texture: &Texture2D) {
        match *self {
            Imagelike::None => {}
            Imagelike::Image(ref surface) | Imagelike::Movie(ref surface,_) => {
                texture.upload_surface(surface, false, false);
            }
        }
    }

    fn should_always_upload(&self) -> bool {
        match *self {
            Imagelike::None | Imagelike::Image(_) => false,
            Imagelike::Movie(_,ref mpeg) => mpeg.status() == SMPEG_PLAYING
        }
    }
}

/// The canvas to which the BGA is drawn.
pub struct BGACanvas {
    /// The current BGA states.
    state: BGAState,
    /// Per-layer textures.
    textures: Vec<Texture2D>,
    /// The internal canvas texture. This is what the caller should render.
    canvas: Texture2D,
    /// The frame buffer associated to the canvas.
    framebuf: FrameBuffer,
    /// The scratch surface for partial blitting.
    scratch: PreparedSurface,
}

/// Uploads the image pointed by the BGA reference to the texture.
/// It performs a necessary clipping for `BGARef::SlicedImage`.
/// `force` should be set to true when the image has to be updated immediately.
fn upload_bga_ref_to_texture(bgaref: &BGARef<ImageRef>, imgres: &[Imagelike],
                             texture: &Texture2D, scratch: &PreparedSurface, force: bool) {
    match *bgaref {
        BGARef::Image(iref) if force || imgres[**iref as uint].should_always_upload() => {
            imgres[**iref as uint].upload_to_texture(texture);
        }
        BGARef::SlicedImage(iref, ref slice)
                if force || imgres[**iref as uint].should_always_upload() => {
            scratch.as_surface().fill(RGBA(0, 0, 0, 0));
            for surface in imgres[**iref as uint].surface().into_iter() {
                // this requires SDL_SRCALPHA flags in `surface` (and not `scratch`).
                // see `LoadedImagelike::new` for relevant codes.
                scratch.as_surface().blit_area(surface.as_surface(), (slice.sx, slice.sy),
                                               (slice.dx, slice.dy), (slice.w, slice.h));
            }
            texture.upload_surface(scratch, false, false);
        }
        _ => {}
    }
}

impl BGACanvas {
    /// Creates an initial canvas state and resources.
    pub fn new(imgres: &[Imagelike]) -> BGACanvas {
        let state = initial_bga_state();

        let scratch = match PreparedSurface::new(BGAW, BGAH, true) {
            Ok(surface) => surface,
            Err(err) => die!("PreparedSurface::new failed: {}", err)
        };

        let textures = state.iter().map(|iref| {
            let texture = match Texture2D::new(BGAW, BGAH) {
                Ok(texture) => texture,
                Err(err) => die!("Texture2D::new failed: {}", err)
            };
            upload_bga_ref_to_texture(iref, imgres, &texture, &scratch, true);
            texture
        }).collect();

        let canvas = match Texture2D::new(BGAW, BGAH) {
            Ok(texture) => texture,
            Err(err) => die!("Texture2D::new failed: {}", err)
        };
        canvas.create_storage(gl::RGB, gl::UNSIGNED_BYTE, false, false);

        let framebuf = FrameBuffer::from_texture(&canvas);

        BGACanvas { state: state, textures: textures, canvas: canvas, framebuf: framebuf,
                    scratch: scratch }
    }

    /// Updates the BGA state. This method prepares given image resources for the next rendering,
    /// notably by starting and stopping the movie playback and uploading textures as needed.
    pub fn update(&mut self, current: &BGAState, imgres: &[Imagelike]) {
        for layer in range(0, NLAYERS) {
            if self.state[layer] != current[layer] {
                // TODO this design can't handle the case that a BGA layer is updated to the same
                // image reference, which should rewind the movie playback.
                if self.state[layer].as_image_ref() != current[layer].as_image_ref() {
                    for &iref in self.state[layer].as_image_ref().into_iter() {
                        imgres[**iref as uint].stop_animating();
                    }
                    for &iref in current[layer].as_image_ref().into_iter() {
                        imgres[**iref as uint].start_animating();
                    }
                }
                upload_bga_ref_to_texture(&current[layer], imgres,
                                          &self.textures[layer], &self.scratch, true);
            } else {
                upload_bga_ref_to_texture(&self.state[layer], imgres,
                                          &self.textures[layer], &self.scratch, false);
            }
            self.state[layer] = current[layer].clone();
        }
    }

    /// Renders the image resources to the internal canvas texture.
    pub fn render_to_texture(&self, screen: &mut Screen, layers: &[BGALayer]) {
        screen.render_to_framebuffer(&self.framebuf, |buf| {
            buf.clear();
            for &layer in layers.iter() {
                match self.state[layer as uint] {
                    BGARef::Blank => {}
                    _ => {
                        buf.draw_textured(&self.textures[layer as uint], |d| {
                            d.rect(0.0, 0.0, BGAW as f32, BGAH as f32);
                        });
                    }
                }
            }
        });
    }

    /// Returns the internal canvas texture.
    pub fn as_texture<'r>(&'r self) -> &'r Texture2D { &self.canvas }
}

/// Text-only viewing scene context. Used for the exclusive mode with BGA disabled.
pub struct TextualViewingScene {
    /// Current game play states.
    pub player: Player,
    /// Ticker used for printing to the console.
    pub ticker: Ticker,
}

impl TextualViewingScene {
    /// Creates a new text-only viewing scene.
    pub fn new(player: Player) -> Box<TextualViewingScene> {
        box TextualViewingScene { player: player, ticker: Ticker::new() }
    }
}

impl Scene for TextualViewingScene {
    fn activate(&mut self) -> SceneCommand { SceneCommand::Continue }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new().fpslimit(20) }

    fn tick(&mut self) -> SceneCommand {
        if self.player.tick() {SceneCommand::Continue} else {SceneCommand::Pop}
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
                            lastcombo = self.player.lastcombo,
                            nnotes = self.player.infos.nnotes)[]);
    }

    fn deactivate(&mut self) {
        update_line("");
    }

    fn consume(self: Box<TextualViewingScene>) -> Box<Scene+'static> { panic!("unreachable"); }
}

/// BGA-only viewing scene context. Used for the exclusive mode with BGA enabled.
pub struct ViewingScene {
    /// The underlying text-only viewing scene context (as the BGA-only viewing scene lacks
    /// the on-screen display).
    pub parent: Box<TextualViewingScene>,
    /// Display screen.
    pub screen: Rc<RefCell<Screen>>,
    /// Image resources.
    pub imgres: Vec<Imagelike>,
    /// BGA canvas.
    pub bgacanvas: BGACanvas,
}

impl ViewingScene {
    /// Creates a new BGA-only scene context from the pre-created screen (usually by `init_video`)
    /// and pre-loaded image resources.
    pub fn new(screen: Rc<RefCell<Screen>>, imgres: Vec<Imagelike>,
               player: Player) -> Box<ViewingScene> {
        let bgacanvas = BGACanvas::new(imgres[]);
        box ViewingScene { parent: TextualViewingScene::new(player),
                           screen: screen, imgres: imgres, bgacanvas: bgacanvas }
    }
}

impl Scene for ViewingScene {
    fn activate(&mut self) -> SceneCommand { self.parent.activate() }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new() }

    fn tick(&mut self) -> SceneCommand {
        let cmd = self.parent.tick();
        self.bgacanvas.update(&self.parent.player.bga, self.imgres[]);
        cmd
    }

    fn render(&self) {
        let mut screen = self.screen.borrow_mut();

        screen.clear();

        let layers = &[BGALayer::Layer1, BGALayer::Layer2, BGALayer::Layer3];
        self.bgacanvas.render_to_texture(screen.deref_mut(), layers);
        screen.draw_textured(self.bgacanvas.as_texture(), |d| {
            d.rect(0.0, 0.0, BGAW as f32, BGAH as f32);
        });
        screen.swap_buffers();

        self.parent.render();
    }

    fn deactivate(&mut self) { self.parent.deactivate() }

    fn consume(self: Box<ViewingScene>) -> Box<Scene+'static> { panic!("unreachable"); }
}

