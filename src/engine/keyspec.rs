// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Key kinds and specification.

use format::obj::{Lane, NLANES, ObjQueryOps};
use format::timeline::Timeline;
use format::timeline::modf::filter_lanes;
use format::bms::{Bms, Key, DoublePlay, CouplePlay};

/**
 * Key kinds. They define an appearance of particular lane, but otherwise ignored for the game
 * play. Angolmois supports several key kinds in order to cover many potential uses.
 * (C: `KEYKIND_MNEMONICS`)
 *
 * # Defaults
 *
 * For BMS/BME, channels #11/13/15/19 and #21/23/25/29 use `WhiteKey`, #12/14/18 and #22/24/28
 * use `BlackKey`, #16 and #26 use `Scratch`, #17 and #27 use `FootPedal`.
 *
 * For PMS, channels #11/17/25 use `Button1`, #12/16/24 use `Button2`, #13/19/23 use `Button3`,
 * #14/18/22 use `Button4`, #15 uses `Button5`.
 */
#[deriving(Eq)]
pub enum KeyKind {
    /// White key, which mimics a real white key in the musical keyboard.
    WhiteKey,
    /// White key, but rendered yellow. This is used for simulating the O2Jam interface which
    /// has one yellow lane (mapped to spacebar) in middle of six other lanes (mapped to normal
    /// keys).
    WhiteKeyAlt,
    /// Black key, which mimics a real black key in the keyboard but rendered light blue as in
    /// Beatmania and other games.
    BlackKey,
    /// Scratch, rendered red. Scratch lane is wider than other "keys" and normally doesn't
    /// count as a key.
    Scratch,
    /// Foot pedal, rendered green. Otherwise has the same properties as scratch. The choice of
    /// color follows that of EZ2DJ, one of the first games that used this game element.
    FootPedal,
    /// White button. This and following "buttons" come from Pop'n Music, which has nine colored
    /// buttons. (White buttons constitute 1st and 9th of Pop'n Music buttons.) The "buttons"
    /// are wider than aforementioned "keys" but narrower than scratch and foot pedal.
    Button1,
    /// Yellow button (2nd and 8th of Pop'n Music buttons).
    Button2,
    /// Green button (3rd and 7th of Pop'n Music buttons).
    Button3,
    /// Navy button (4th and 6th of Pop'n Music buttons).
    Button4,
    /// Red button (5th of Pop'n Music buttons).
    Button5,
}

impl KeyKind {
    /// Returns a list of all supported key kinds.
    //
    // Rust: can this method be generated on the fly?
    pub fn all() -> &'static [KeyKind] {
        &[WhiteKey, WhiteKeyAlt, BlackKey, Scratch, FootPedal,
          Button1, Button2, Button3, Button4, Button5]
    }

    /// Converts a mnemonic character to an appropriate key kind. Used for parsing a key
    /// specification (see also `KeySpec`).
    pub fn from_char(c: char) -> Option<KeyKind> {
        match c {
            'a' => Some(WhiteKey),
            'y' => Some(WhiteKeyAlt),
            'b' => Some(BlackKey),
            's' => Some(Scratch),
            'p' => Some(FootPedal),
            'q' => Some(Button1),
            'w' => Some(Button2),
            'e' => Some(Button3),
            'r' => Some(Button4),
            't' => Some(Button5),
            _   => None
        }
    }

    /// Converts an appropriate key kind to a mnemonic character. Used for environment variables
    /// (see also `read_keymap`).
    pub fn to_char(self) -> char {
        match self {
            WhiteKey    => 'a',
            WhiteKeyAlt => 'y',
            BlackKey    => 'b',
            Scratch     => 's',
            FootPedal   => 'p',
            Button1     => 'w',
            Button2     => 'e',
            Button3     => 'r',
            Button4     => 't',
            Button5     => 's'
        }
    }

    /**
     * Returns true if a kind counts as a "key". (C: `KEYKIND_IS_KEY`)
     *
     * This affects the number of keys displayed in the loading screen, and reflects a common
     * practice of counting "keys" in many games (e.g. Beatmania IIDX has 8 lanes including one
     * scratch but commonly said to have 7 "keys").
     */
    pub fn counts_as_key(self) -> bool {
        self != Scratch && self != FootPedal
    }
}

/// The key specification. Specifies the order and apperance of lanes. Once determined from
/// the options and BMS file, the key specification is fixed and independent of other data
/// (e.g. `#PLAYER` value).
pub struct KeySpec {
    /// The number of lanes on the left side. This number is significant only when Couple Play
    /// is used. (C: `nleftkeys`)
    split: uint,
    /// The order of significant lanes. The first `nleftkeys` lanes go to the left side and
    /// the remaining lanes (C: `nrightkeys`) go to the right side. (C: `keyorder`)
    order: ~[Lane],
    /// The type of lanes. (C: `keykind`)
    kinds: ~[Option<KeyKind>]
}

impl KeySpec {
    /// Returns a number of lanes that count towards "keys". Notably scratches and pedals do not
    /// count as keys. (C: `nkeys`)
    pub fn nkeys(&self) -> uint {
        let mut nkeys = 0;
        for self.kinds.iter().filter_map(|kind| *kind).advance |kind| {
            if kind.counts_as_key() { nkeys += 1; }
        }
        nkeys
    }

    /// Iterates over lanes on the left side, from left to right.
    pub fn each_left_lanes(&self, f: &fn(&Lane) -> bool) -> bool {
        assert!(self.split <= self.order.len());
        self.order.slice(0, self.split).iter().advance(f)
    }

    /// Iterates over lanes on the right side if any, from left to right.
    pub fn each_right_lanes(&self, f: &fn(&Lane) -> bool) -> bool {
        assert!(self.split <= self.order.len());
        self.order.slice(self.split, self.order.len()).iter().advance(f)
    }

    /// Removes insignificant lanes. (C: `analyze_and_compact_bms`)
    pub fn filter_timeline<S:Copy,I:Copy>(&self, timeline: &mut Timeline<S,I>) {
        filter_lanes(timeline, self.order);
    }
}

/// Parses the key specification from the string. (C: `parse_key_spec`)
pub fn parse_key_spec(s: &str) -> Option<~[(Lane, KeyKind)]> {
    use util::std::str::StrUtil;

    let mut specs = ~[];
    let mut s = s.trim_left().to_owned();
    while !s.is_empty() {
        let mut chan = Key(0);
        let mut kind = '\x00';
        let mut s2 = ~"";
        if !lex!(s; Key -> chan, char -> kind, ws*, str* -> s2, !) {
            return None;
        }
        s = s2;
        match (chan, KeyKind::from_char(kind)) {
            (Key(36/*1*36*/..107/*3*36-1*/), Some(kind)) => {
                specs.push((Lane(*chan as uint - 1*36), kind));
            }
            (_, _) => { return None; }
        }
    }
    Some(specs)
}

/// A list of well-known key specifications. (C: `presets`)
static PRESETS: &'static [(&'static str, &'static str, &'static str)] = &[
    // 5-key BMS, SP/DP
    ("5",     "16s 11a 12b 13a 14b 15a", ""),
    ("10",    "16s 11a 12b 13a 14b 15a", "21a 22b 23a 24b 25a 26s"),
    // 5-key BMS with a foot pedal, SP/DP
    ("5/fp",  "16s 11a 12b 13a 14b 15a 17p", ""),
    ("10/fp", "16s 11a 12b 13a 14b 15a 17p", "27p 21a 22b 23a 24b 25a 26s"),
    // 7-key BME, SP/DP
    ("7",     "16s 11a 12b 13a 14b 15a 18b 19a", ""),
    ("14",    "16s 11a 12b 13a 14b 15a 18b 19a", "21a 22b 23a 24b 25a 28b 29a 26s"),
    // 7-key BME with a foot pedal, SP/DP
    ("7/fp",  "16s 11a 12b 13a 14b 15a 18b 19a 17p", ""),
    ("14/fp", "16s 11a 12b 13a 14b 15a 18b 19a 17p", "27p 21a 22b 23a 24b 25a 28b 29a 26s"),
    // 9-key PMS (#PLAYER 3)
    ("9",     "11q 12w 13e 14r 15t 22r 23e 24w 25q", ""),
    // 9-key PMS (BME-compatible)
    ("9-bme", "11q 12w 13e 14r 15t 18r 19e 16w 17q", ""),
];

/**
 * Determines the key specification from the preset name, in the absence of explicit key
 * specification with `-K` option. (C: `detect_preset`)
 *
 * Besides from presets specified in `PRESETS`, this function also allows the following
 * pseudo-presets inferred from the BMS file:
 *
 * - `bms`, `bme`, `bml` or no preset: Selects one of eight presets `{5,7,10,14}[/fp]`.
 * - `pms`: Selects one of two presets `9` and `9-bme`.
 */
pub fn preset_to_key_spec(bms: &Bms, preset: Option<~str>) -> Option<(~str, ~str)> {
    use util::std::str::StrUtil;

    let mut present = [false, ..NLANES];
    for bms.timeline.objs.iter().advance |&obj| {
        let lane = obj.object_lane(); // XXX #3511
        for lane.iter().advance |&Lane(lane)| {
            present[lane] = true;
        }
    }

    let preset = match preset.map(|s| s.to_ascii_lower()) {
        None | Some(~"bms") | Some(~"bme") | Some(~"bml") => {
            let isbme = (present[8] || present[9] || present[36+8] || present[36+9]);
            let haspedal = (present[7] || present[36+7]);
            let nkeys = match bms.meta.player {
                CouplePlay | DoublePlay => if isbme {~"14"} else {~"10"},
                _                       => if isbme {~"7" } else {~"5" }
            };
            if haspedal {nkeys + "/fp"} else {nkeys}
        },
        Some(~"pms") => {
            let isbme = (present[6] || present[7] || present[8] || present[9]);
            if isbme {~"9-bme"} else {~"9"}
        },
        Some(preset) => preset
    };

    for PRESETS.iter().advance |&(name, leftkeys, rightkeys)| {
        if name == preset {
            return Some((leftkeys.to_owned(), rightkeys.to_owned()));
        }
    }
    None
}

/// Parses a key specification from the options.
pub fn key_spec(bms: &Bms, preset: Option<~str>,
                leftkeys: Option<~str>, rightkeys: Option<~str>) -> Result<~KeySpec,~str> {
    use util::std::str::StrUtil;

    let (leftkeys, rightkeys) =
        if leftkeys.is_none() && rightkeys.is_none() {
            let preset =
                if preset.is_none() && bms.bmspath.to_ascii_lower().ends_with(".pms") {
                    Some(~"pms")
                } else {
                    preset
                };
            match preset_to_key_spec(bms, preset.clone()) {
                Some(leftright) => leftright,
                None => {
                    return Err(fmt!("Invalid preset name: %s",
                                    preset.map_default(~"", |&v| v.clone())));
                }
            }
        } else {
            // Rust: `Option` of managed pointer is not easy to use due to
            //       implicit move. `Option<T>::clone_default` maybe?
            (leftkeys.map_default(~"", |&v| v.clone()),
             rightkeys.map_default(~"", |&v| v.clone()))
        };

    let mut keyspec = ~KeySpec { split: 0, order: ~[], kinds: ~[None, ..NLANES] };
    let parse_and_add = |keys: &str| -> Option<uint> {
        match parse_key_spec(keys) {
            None | Some([]) => None,
            Some(left) => {
                for left.iter().advance |&(lane,kind)| {
                    if keyspec.kinds[*lane].is_some() { return None; }
                    keyspec.order.push(lane);
                    keyspec.kinds[*lane] = Some(kind);
                }
                Some(left.len())
            }
        }
    };

    if !leftkeys.is_empty() {
        match parse_and_add(leftkeys) {
            None => { return Err(fmt!("Invalid key spec for left hand side: %s", leftkeys)); }
            Some(nkeys) => { keyspec.split += nkeys; }
        }
    } else {
        return Err(fmt!("No key model is specified using -k or -K"));
    }
    if !rightkeys.is_empty() {
        match parse_and_add(rightkeys) {
            None => { return Err(fmt!("Invalid key spec for right hand side: %s", rightkeys)); }
            Some(nkeys) => { // no split panes except for #PLAYER 2
                if bms.meta.player != CouplePlay { keyspec.split += nkeys; }
            }
        }
    }
    Ok(keyspec)
}

