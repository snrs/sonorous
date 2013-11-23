// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Viewer portion of the game play screen. Also shared by exclusive modes.

use gl = opengles::gl2;
use ext::sdl::mpeg;
use format::obj::{NLAYERS, BGALayer, Layer1, Layer2, Layer3};
use format::obj::{ImageSlice, BGARef, BlankBGA, ImageBGA, SlicedImageBGA};
use format::bms::ImageRef;
use gfx::color::RGBA;
use gfx::surface::SurfaceAreaUtil;
use gfx::gl::{Texture2D, PreparedSurface, FrameBuffer};
use gfx::draw::TexturedDrawingTraits;
use gfx::screen::Screen;
use engine::resource::{BGAW, BGAH};
use engine::resource::{ImageResource, NoImage, Image, Movie};
use engine::player::{BGAState, initial_bga_state, Player};
use ui::common::{Ticker, update_line};
use ui::scene::{Scene, SceneOptions, SceneCommand, Continue, PopScene};

trait Uploadable {
    /// Uploads an associated surface to the texture if any.
    fn upload_to_texture(&self, texture: &Texture2D);
    /// Returns true if the resource should be updated continuously (i.e. movies or animations).
    fn should_always_upload(&self) -> bool;
}

impl Uploadable for ImageResource {
    fn upload_to_texture(&self, texture: &Texture2D) {
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

/// The canvas to which the BGA is drawn.
struct BGACanvas {
    /// The current BGA states.
    state: BGAState,
    /// Per-layer textures.
    textures: ~[Texture2D],
    /// The internal canvas texture. This is what the caller should render.
    canvas: Texture2D,
    /// The frame buffer associated to the canvas.
    framebuf: FrameBuffer,
    /// The scratch surface for partial blitting.
    scratch: PreparedSurface,
}

/// Uploads the image pointed by the BGA reference to the texture.
/// It performs a necessary clipping for `SlicedImageBGA`.
/// `force` should be set to true when the image has to be updated immediately.
fn upload_bga_ref_to_texture(bgaref: &BGARef<ImageRef>, imgres: &[ImageResource],
                             texture: &Texture2D, scratch: &PreparedSurface, force: bool) {
    match *bgaref {
        ImageBGA(ref iref) if force || imgres[***iref].should_always_upload() => {
            imgres[***iref].upload_to_texture(texture);
        }
        SlicedImageBGA(ref iref, ~ImageSlice {sx,sy,dx,dy,w,h})
                if force || imgres[***iref].should_always_upload() => {
            scratch.fill(RGBA(0, 0, 0, 0));
            for surface in imgres[***iref].surface().move_iter() {
                // this requires SDL_SRCALPHA flags in `surface` (and not `scratch`).
                // see `LoadedImageResource::new` for relevant codes.
                scratch.blit_area(**surface, (sx, sy), (dx, dy), (w, h));
            }
            texture.upload_surface(scratch, false, false);
        }
        _ => {}
    }
}

impl BGACanvas {
    /// Creates an initial canvas state and resources.
    pub fn new(imgres: &[ImageResource]) -> BGACanvas {
        let state = initial_bga_state();

        let scratch = match PreparedSurface::new(BGAW, BGAH, true) {
            Ok(surface) => surface,
            Err(err) => die!("PreparedSurface::new failed: {}", err)
        };

        let textures = do state.map |iref| {
            let texture = match Texture2D::new(BGAW, BGAH) {
                Ok(texture) => texture,
                Err(err) => die!("Texture2D::new failed: {}", err)
            };
            upload_bga_ref_to_texture(iref, imgres, &texture, &scratch, true);
            texture
        };

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
    pub fn update(&mut self, current: &BGAState, imgres: &[ImageResource]) {
        for layer in range(0, NLAYERS) {
            if self.state[layer] != current[layer] {
                // TODO this design can't handle the case that a BGA layer is updated to the same
                // image reference, which should rewind the movie playback.
                if self.state[layer].as_image_ref() != current[layer].as_image_ref() {
                    for &iref in self.state[layer].as_image_ref().move_iter() {
                        imgres[**iref].stop_animating();
                    }
                    for &iref in current[layer].as_image_ref().move_iter() {
                        imgres[**iref].start_animating();
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
        do screen.render_to_framebuffer(&self.framebuf) |buf| {
            buf.clear();
            for &layer in layers.iter() {
                match self.state[layer as uint] {
                    BlankBGA => {}
                    _ => {
                        do buf.draw_textured(&self.textures[layer as uint]) |d| {
                            d.rect(0.0, 0.0, BGAW as f32, BGAH as f32);
                        }
                    }
                }
            }
        }
    }

    /// Returns the internal canvas texture.
    pub fn as_texture<'r>(&'r self) -> &'r Texture2D { &self.canvas }
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
    screen: @mut Screen,
    /// Image resources.
    imgres: ~[ImageResource],
    /// BGA canvas.
    bgacanvas: BGACanvas,
}

impl ViewingScene {
    /// Creates a new BGA-only scene context from the pre-created screen (usually by `init_video`)
    /// and pre-loaded image resources.
    pub fn new(screen: @mut Screen, imgres: ~[ImageResource], player: Player) -> ~ViewingScene {
        let bgacanvas = BGACanvas::new(imgres);
        ~ViewingScene { parent: TextualViewingScene::new(player),
                        screen: screen, imgres: imgres, bgacanvas: bgacanvas }
    }
}

impl Scene for ViewingScene {
    fn activate(&mut self) -> SceneCommand { self.parent.activate() }

    fn scene_options(&self) -> SceneOptions { SceneOptions::new() }

    fn tick(&mut self) -> SceneCommand {
        let cmd = self.parent.tick();
        self.bgacanvas.update(&self.parent.player.bga, self.imgres);
        cmd
    }

    fn render(&self) {
        self.screen.clear();

        let layers = &[Layer1, Layer2, Layer3];
        self.bgacanvas.render_to_texture(&mut *self.screen, layers);
        do self.screen.draw_textured(self.bgacanvas.as_texture()) |d| {
            d.rect(0.0, 0.0, BGAW as f32, BGAH as f32);
        }
        self.screen.swap_buffers();

        self.parent.render();
    }

    fn deactivate(&mut self) { self.parent.deactivate() }

    fn consume(~self) -> ~Scene: { fail!("unreachable"); }
}

