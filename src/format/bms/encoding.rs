// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Character encoding detection for BMS format.

use std::cmp;
use encoding::{Encoding, EncodingRef, DecodeStrict, DecodeReplace};
use encoding::all::{ASCII, UTF_8, WINDOWS_949, WINDOWS_31J};

use util::chardet::{Classifier, CharClassKo, CharClassJa, convert_raw_confidence};

static LOG_PROBS_KO: &'static [i32] = &[
     552483, -251065, -187207, -163086,  -88603, -130451,   -2906,  -18512,  -35744,  -77761,
    -439522, -493587,  -63872,       0,       0,       0, -447903, -450588, -192957, -424931,
    -428366, -439371, -381774, -437472, -464612, -440834, -430816, -412963, -443252, -455960,
    -439033, -465512, -481607, -452974, -295339, -394243, -417433, -436318, -424640, -453085,
    -408190,       0, -337120, -342559, -340045, -390810, -367378, -362360, -350409, -358721,
    -344365, -386048, -378418, -300543, -324063, -357782, -341811, -375471, -358808, -352643,
    -373660, -346715, -368680, -406217, -266907, -246069, -217794, -229556, -214754, -264366,
    -123848, -163253, -170876, -213532, -311807, -327884, -383674, -309190, -267631, -408071,
    -359482, -385177, -361882, -331612, -356844, -362450, -307037, -358975, -343735, -369816,
    -354754, -353362, -333292, -283308,       0,  476786,  464244,  551795,  158798,  154085,
     148055,  157507,  423537,  474691,  465626,  470458,  480994,  483912,  483317,  472684,
     486736,  467066,  464216,  494842,  442136,  456360,  458081,  462483,  384185,  186504,
     188436,  207318,  206198,  202216,  266763,  202678,  151882,  153399,  150949,  149908,
     150722,  148568,  149770,  150994,  151085,  145990,  149908,  151311,  151677,  149999,
     151131,  150448,  149953,  150539,  149509,  140982,  152696,  150994,  151993,  151268,
     148568,  151177,  144499,  151940,  151932,  150858,  146445,  151268,  150948,  149418,
     151177,  151131,  149463,  151131,  150311,  150902,  150902,
];

static LOG_PROBS_JA: &'static [i32] = &[
     578168,  595533, -519562,  -81564,    -910, -463761, -551809, -129774,  -58469,   -2822,
          0,       0,       0,       0,  -25268,       0,       0, -423239, -428369, -452137,
    -433665, -413771, -435877, -437062, -426341, -459655, -469202, -412439, -466691, -428227,
    -427952, -439672, -435672, -462985, -396282, -455093, -430901, -417250, -460709, -457244,
    -406040, -396471, -409790, -427335, -425017, -434123, -419376, -421745, -389832, -145468,
    -119354, -123541,  -69588, -178492, -145996, -139950, -155965, -114733, -116872, -122880,
    -248896, -136961, -134743, -151998,  -24858,  289652,   33155,  270933,  159049,  290606,
      29510,  248662,   94216,  256017,  141011,  299433,   64481,  236950,  150402,  275372,
     116927,  228013,   92697,  312102,  131422,  106948,       0,       0,       0,       0,
     245725,  378555,  243699,  354404,       0,       0,       0,       0,       0,       0,
          0,       0,       0,       0,       0,       0,       0,       0,       0,       0,
          0,       0,       0,       0,       0,       0,       0,       0,       0,       0,
          0,       0,
];

/// Reads the whole stream with given encoding. Any error would be substituted with U+FFFD.
pub fn decode_stream(f: &mut Reader, encoding: EncodingRef) -> ~str {
    // TODO use incremental decoding when available
    let s = f.read_to_end();
    encoding.decode(s, DecodeReplace).unwrap()
}

/// Tries to guess the encoding of the stream and reads it accordingly.
/// Any error would be substituted with U+FFFD.
/// Returns a guessed encoding and confidence (probability) in addition to the decoded string.
/// Currently recognizes `ASCII`, `UTF_8`, `WINDOWS_949` and `WINDOWS_31J` encodings.
//
// Rust: cannot change this to return `EncodingRef`, it seems to "infect" other uses of
//       `UTF_8.decode(...)` etc. to fail to resolve.
pub fn guess_decode_stream(f: &mut Reader) -> (~str, &'static Encoding, f64) {
    let s: ~[u8] = f.read_to_end();

    // check for BOM (Sonorous proposal #1)
    if s.len() >= 3 && [0xef, 0xbb, 0xbf].equiv(&s.slice_to(3)) {
        return (UTF_8.decode(s, DecodeReplace).unwrap(), UTF_8 as &'static Encoding, 1.0);
    }

    // check for UTF-8 first line (Sonorous proposal #2)
    let first1k = s.slice_to(cmp::min(s.len(), 1024));
    let first1keol = first1k.iter().position(|&c| c == 0x0a).unwrap_or(first1k.len());
    let firstline = first1k.slice_to(first1keol);
    if firstline.iter().any(|&c| c >= 0x80) && UTF_8.decode(firstline, DecodeStrict).is_ok() {
        return (UTF_8.decode(s, DecodeReplace).unwrap(), UTF_8 as &'static Encoding, 1.0);
    }

    // ASCII: do we have to decode at all?
    if s.iter().all(|&c| c < 0x80) {
        return (ASCII.decode(s, DecodeReplace).unwrap(), ASCII as &'static Encoding, 1.0);
    }

    // Windows-949/31J: guess
    let ko = WINDOWS_949.decode(s, DecodeReplace).unwrap();
    let ja = WINDOWS_31J.decode(s, DecodeReplace).unwrap();
    let koconfidence = Classifier::new(CharClassKo, LOG_PROBS_KO).raw_confidence(ko);
    let jaconfidence = Classifier::new(CharClassJa, LOG_PROBS_JA).raw_confidence(ja);
    let (s, encoding, confidence) =
        if koconfidence < jaconfidence {
            (ko, WINDOWS_949 as &'static Encoding, koconfidence)
        } else {
            (ja, WINDOWS_31J as &'static Encoding, jaconfidence)
        };
    (s, encoding, convert_raw_confidence(confidence))
}

