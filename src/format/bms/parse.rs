// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

use compat::core::iter;

use format::obj::{BPM, Duration, Seconds, Measures};
use format::bms::{Key, SoundRef, ImageRef, BlitCmd};

pub enum BmsCommand<'self> {
    BmsUnknown(&'self str),             // starting with `#` but unknown otherwise
    BmsTitle(&'self str),               // #TITLE
    BmsGenre(&'self str),               // #GENRE
    BmsArtist(&'self str),              // #ARTIST
    BmsStageFile(&'self str),           // #STAGEFILE
    BmsPathWAV(&'self str),             // #PATH_WAV
    BmsBPM(BPM),                        // #BPM (without a following alphanumeric key)
    BmsExBPM(Key, BPM),                 // #EXBPM or #BPMxx
    BmsPlayer(int),                     // #PLAYER
    BmsPlayLevel(int),                  // #PLAYLEVEL
    BmsRank(int),                       // #RANK
    BmsLNType(int),                     // #LNTYPE
    BmsLNObj(Key),                      // #LNOBJ
    BmsWAV(SoundRef, &'self str),       // #WAV
    BmsBMP(ImageRef, &'self str),       // #BMP
    BmsBGA(BlitCmd),                    // #BGA
    BmsStop(Key, Duration),             // #STOP
    BmsStp(float, Duration),            // #STP
    BmsRandom(int),                     // #RANDOM
    BmsSetRandom(int),                  // #SETRANDOM
    BmsEndRandom,                       // #ENDRANDOM
    BmsIf(int),                         // #IF
    BmsElseIf(int),                     // #ELSEIF
    BmsElse,                            // #ELSE
    BmsEndIf,                           // #END(IF)
    BmsData(uint, Key, &'self str)      // #xxxyy:...
}

pub fn each_bms_command(f: @io::Reader, blk: &fn(BmsCommand) -> bool) -> iter::Ret {
    let lines = vec::split(f.read_whole_stream(), |&ch| ch == 10u8);
    for lines.each |&line0| {
        let line0 = ::util::core::str::from_fixed_utf8_bytes(line0, |_| ~"\ufffd");
        let line: &str = line0;

        // skip non-command lines
        let line = line.trim_left();
        if !line.starts_with("#") { loop; }
        let line = line.slice_to_end(1);

        let upperline = line.to_upper();

        macro_rules! if_prefix(
            ($prefix:expr -> $constr:ident string) => (
                if_prefix!($prefix {
                    let mut text = ~"";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        if !blk($constr(text)) { return iter::EarlyExit; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident value) => (
                if_prefix!($prefix {
                    let mut value = 0;
                    if lex!(line; ws, int -> value) {
                        if !blk($constr(value)) { return iter::EarlyExit; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident path($keyconstr:ident)) => (
                if_prefix!($prefix {
                    let mut key = Key(-1), path = ~"";
                    if lex!(line; Key -> key, ws, str -> path, ws*, !) {
                        if !blk($constr($keyconstr(key), path)) { return iter::EarlyExit; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident) => (
                if_prefix!($prefix {
                    if !blk($constr) { return iter::EarlyExit; }
                })
            );
            ($prefix:expr $blk:expr) => ({
                let prefix: &'static str = $prefix;
                if upperline.starts_with(prefix) {
                    let line = line.slice_to_end(prefix.len());
                    let _ = line; // removes warning
                    $blk
                }
            })
        )

        if_prefix!("TITLE"     -> BmsTitle     string)
        if_prefix!("GENRE"     -> BmsGenre     string)
        if_prefix!("ARTIST"    -> BmsArtist    string)
        if_prefix!("STAGEFILE" -> BmsStageFile string)
        if_prefix!("PATH_WAV"  -> BmsPathWAV   string)

        if_prefix!("BPM" { // #BPM <float> or #BPMxx <float>
            let mut key = Key(-1), bpm = 0.0;
            if lex!(line; Key -> key, ws, float -> bpm) {
                if !blk(BmsExBPM(key, BPM(bpm))) { return iter::EarlyExit; }
            } else if lex!(line; ws, float -> bpm) {
                if !blk(BmsBPM(BPM(bpm))) { return iter::EarlyExit; }
            }
        })

        if_prefix!("PLAYER"    -> BmsPlayer    value)
        if_prefix!("PLAYLEVEL" -> BmsPlayLevel value)
        if_prefix!("RANK"      -> BmsRank      value)
        if_prefix!("LNTYPE"    -> BmsLNType    value)

        if_prefix!("LNOBJ" { // #LNOBJ <key>
            let mut key = Key(-1);
            if lex!(line; ws, Key -> key) {
                if !blk(BmsLNObj(key)) { return iter::EarlyExit; }
            }
        })

        if_prefix!("WAV" -> BmsWAV path(SoundRef))
        if_prefix!("BMP" -> BmsBMP path(ImageRef))

        if_prefix!("BGA" { // #BGAxx yy <int> <int> <int> <int> <int> <int>
            let mut dst = Key(0), src = Key(0);
            let mut bc = BlitCmd { dst: ImageRef(Key(0)), src: ImageRef(Key(0)),
                                   x1: 0, y1: 0, x2: 0, y2: 0, dx: 0, dy: 0 };
            if lex!(line; Key -> dst, ws, Key -> src, ws, int -> bc.x1, ws, int -> bc.y1, ws,
                    int -> bc.x2, ws, int -> bc.y2, ws, int -> bc.dx, ws, int -> bc.dy) {
                bc.src = ImageRef(src);
                bc.dst = ImageRef(dst);
                if !blk(BmsBGA(bc)) { return iter::EarlyExit; }
            }
        })

        if_prefix!("STOP" { // #STOPxx <int>
            let mut key = Key(-1), duration = 0;
            if lex!(line; Key -> key, ws, int -> duration) {
                let duration = Measures(duration as float / 192.0);
                if !blk(BmsStop(key, duration)) { return iter::EarlyExit; }
            }
        })

        if_prefix!("STP" { // #STP<int>.<int> <int>
            let mut measure = 0, frac = 0, duration = 0;
            if lex!(line; Measure -> measure, '.', uint -> frac, ws,
                    int -> duration) && duration > 0 {
                let pos = measure as float + frac as float * 0.001;
                let duration = Seconds(duration as float * 0.001);
                if !blk(BmsStp(pos, duration)) { return iter::EarlyExit; }
            }
        })

        if_prefix!("ENDSW" { loop; })

        if_prefix!("RANDOM"    -> BmsRandom    value)
        if_prefix!("SETRANDOM" -> BmsSetRandom value)
        if_prefix!("ENDRANDOM" -> BmsEndRandom      )
        if_prefix!("IF"        -> BmsIf        value)
        if_prefix!("ELSEIF"    -> BmsElseIf    value)
        if_prefix!("ELSE"      -> BmsElse           )
        if_prefix!("END"       -> BmsEndIf          )

        let mut measure = 0, chan = Key(0), data = ~"";
        if lex!(line; Measure -> measure, Key -> chan, ':', ws*, str -> data, ws*, !) {
            if !blk(BmsData(measure, chan, data)) { return iter::EarlyExit; }
        }

        if !blk(BmsUnknown(line)) { return iter::EarlyExit; }
    }
    iter::Finished
}

