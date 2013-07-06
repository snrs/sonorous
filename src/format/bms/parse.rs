// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS parser.

use std::vec;
use format::obj::{BPM, Duration, Seconds, Measures};
use format::bms::types::{Key};
use format::bms::{ImageRef, BlitCmd};

/// Represents one line of BMS file.
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
    BmsWAV(Key, &'self str),            // #WAV
    BmsBMP(Key, &'self str),            // #BMP
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
    BmsShorten(uint, float),            // #xxx02
    BmsData(uint, Key, &'self str)      // #xxxyy:...
}

impl<'self> ToStr for BmsCommand<'self> {
    /// Returns a reconstructed line for given BMS command.
    fn to_str(&self) -> ~str {
        match *self {
            BmsUnknown(s) => fmt!("#%s", s),
            BmsTitle(s) => fmt!("#TITLE %s", s),
            BmsGenre(s) => fmt!("#GENRE %s", s),
            BmsArtist(s) => fmt!("#ARTIST %s", s),
            BmsStageFile(s) => fmt!("#STAGEFILE %s", s),
            BmsPathWAV(s) => fmt!("#PATH_WAV %s", s),
            BmsBPM(BPM(bpm)) => fmt!("#BPM %f", bpm),
            BmsExBPM(key, BPM(bpm)) => fmt!("#BPM%s %f", key.to_str(), bpm),
            BmsPlayer(v) => fmt!("#PLAYER %d", v),
            BmsPlayLevel(v) => fmt!("#PLAYLEVEL %d", v),
            BmsRank(v) => fmt!("#RANK %d", v),
            BmsLNType(lntype) => fmt!("#LNTYPE %d", lntype),
            BmsLNObj(key) => fmt!("#LNOBJ %s", key.to_str()),
            BmsWAV(key, s) => fmt!("#WAV%s %s", key.to_str(), s),
            BmsBMP(key, s) => fmt!("#BMP%s %s", key.to_str(), s),
            BmsBGA(BlitCmd { dst, src, x1, y1, x2, y2, dx, dy }) =>
                fmt!("#BGA%s %s %d %d %d %d %d %d",
                     dst.to_str(), src.to_str(), x1, y1, x2, y2, dx, dy),
            BmsStop(key, Measures(dur)) => fmt!("#STOP%s %d", key.to_str(), (dur * 192.0) as int),
            BmsStop(*) => fail!(~"unsupported"),
            BmsStp(pos, Seconds(dur)) => fmt!("#STP %07.3f %d", pos, (dur * 1000.0) as int),
            BmsStp(*) => fail!(~"unsupported"),
            BmsRandom(val) => fmt!("#RANDOM %d", val),
            BmsSetRandom(val) => fmt!("#SETRANDOM %d", val),
            BmsEndRandom => ~"#ENDRANDOM",
            BmsIf(val) => fmt!("#IF %d", val),
            BmsElseIf(val) => fmt!("#ELSEIF %d", val),
            BmsElse => ~"#ELSE",
            BmsEndIf => ~"#ENDIF",
            BmsShorten(measure, shorten) => fmt!("#%03u02:%f", measure, shorten),
            BmsData(measure, chan, data) => fmt!("#%03u%s:%s", measure, chan.to_str(), data),
        }
    }
}

pub fn each_bms_command(f: @::std::io::Reader, blk: &fn(BmsCommand) -> bool) -> bool {
    use util::std::str::StrUtil;

    let lines = vec::split(f.read_whole_stream(), |&ch| ch == 10u8);
    for lines.iter().advance |&line0| {
        let line0 = ::util::std::str::from_fixed_utf8_bytes(line0, |_| ~"\ufffd");
        let line: &str = line0;

        // skip non-command lines
        let line = line.trim_left();
        if !line.starts_with("#") { loop; }
        let line = line.slice_to_end(1);

        let upperline = line.to_ascii_upper();

        macro_rules! if_prefix(
            ($prefix:expr -> $constr:ident string) => (
                if_prefix!($prefix {
                    let mut text = ~"";
                    if lex!(line; ws, str* -> text, ws*, !) {
                        if !blk($constr(text)) { return false; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident value) => (
                if_prefix!($prefix {
                    let mut value = 0;
                    if lex!(line; ws, int -> value) {
                        if !blk($constr(value)) { return false; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident path) => (
                if_prefix!($prefix {
                    let mut key = Key(-1);
                    let mut path = ~"";
                    if lex!(line; Key -> key, ws, str -> path, ws*, !) {
                        if !blk($constr(key, path)) { return false; }
                    }
                })
            );
            ($prefix:expr -> $constr:ident) => (
                if_prefix!($prefix {
                    if !blk($constr) { return false; }
                })
            );
            ($prefix:expr $blk:expr) => ({
                let prefix: &'static str = $prefix;
                if upperline.starts_with(prefix) {
                    let line = line.slice_to_end(prefix.len());
                    let _ = line; // removes warning
                    $blk;
                    loop;
                }
            })
        )

        if_prefix!("TITLE"     -> BmsTitle     string)
        if_prefix!("GENRE"     -> BmsGenre     string)
        if_prefix!("ARTIST"    -> BmsArtist    string)
        if_prefix!("STAGEFILE" -> BmsStageFile string)
        if_prefix!("PATH_WAV"  -> BmsPathWAV   string)

        if_prefix!("BPM" { // #BPM <float> or #BPMxx <float>
            let mut key = Key(-1);
            let mut bpm = 0.0;
            if lex!(line; Key -> key, ws, float -> bpm) {
                if !blk(BmsExBPM(key, BPM(bpm))) { return false; }
            } else if lex!(line; ws, float -> bpm) {
                if !blk(BmsBPM(BPM(bpm))) { return false; }
            }
        })

        if_prefix!("PLAYER"    -> BmsPlayer    value)
        if_prefix!("PLAYLEVEL" -> BmsPlayLevel value)
        if_prefix!("RANK"      -> BmsRank      value)
        if_prefix!("LNTYPE"    -> BmsLNType    value)

        if_prefix!("LNOBJ" { // #LNOBJ <key>
            let mut key = Key(-1);
            if lex!(line; ws, Key -> key) {
                if !blk(BmsLNObj(key)) { return false; }
            }
        })

        if_prefix!("WAV" -> BmsWAV path)
        if_prefix!("BMP" -> BmsBMP path)

        if_prefix!("BGA" { // #BGAxx yy <int> <int> <int> <int> <int> <int>
            let mut dst = Key(0);
            let mut src = Key(0);
            let mut bc = BlitCmd { dst: ImageRef(Key(0)), src: ImageRef(Key(0)),
                                   x1: 0, y1: 0, x2: 0, y2: 0, dx: 0, dy: 0 };
            if lex!(line; Key -> dst, ws, Key -> src, ws, int -> bc.x1, ws, int -> bc.y1, ws,
                    int -> bc.x2, ws, int -> bc.y2, ws, int -> bc.dx, ws, int -> bc.dy) {
                bc.src = ImageRef(src);
                bc.dst = ImageRef(dst);
                if !blk(BmsBGA(bc)) { return false; }
            }
        })

        if_prefix!("STOP" { // #STOPxx <int>
            let mut key = Key(-1);
            let mut duration = 0;
            if lex!(line; Key -> key, ws, int -> duration) {
                let duration = Measures(duration as float / 192.0);
                if !blk(BmsStop(key, duration)) { return false; }
            }
        })

        if_prefix!("STP" { // #STP<int>.<int> <int>
            let mut measure = 0;
            let mut frac = 0;
            let mut duration = 0;
            if lex!(line; Measure -> measure, '.', uint -> frac, ws,
                    int -> duration) && duration > 0 {
                let pos = measure as float + frac as float * 0.001;
                let duration = Seconds(duration as float * 0.001);
                if !blk(BmsStp(pos, duration)) { return false; }
            }
        })

        if_prefix!("ENDSW" { }) // avoids #ENDSW being parsed as #ENDIF

        if_prefix!("RANDOM"    -> BmsRandom    value)
        if_prefix!("SETRANDOM" -> BmsSetRandom value)
        if_prefix!("ENDRANDOM" -> BmsEndRandom      )
        if_prefix!("IF"        -> BmsIf        value)
        if_prefix!("ELSEIF"    -> BmsElseIf    value)
        if_prefix!("ELSE"      -> BmsElse           )
        if_prefix!("END"       -> BmsEndIf          )

        let mut measure = 0;
        let mut chan = Key(0);
        let mut data = ~"";
        if lex!(line; Measure -> measure, Key -> chan, ':', ws*, str -> data, ws*, !) {
            if chan == Key(2) { // #xxx02:<float>
                let mut shorten = 0.0;
                if lex!(data; ws*, float -> shorten) {
                    if !blk(BmsShorten(measure, shorten)) { return false; }
                    loop;
                }
            } else {
                if !blk(BmsData(measure, chan, data)) { return false; }
                loop;
            }
        }

        if !blk(BmsUnknown(line)) { return false; }
    }
    true
}

