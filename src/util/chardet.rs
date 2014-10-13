// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md for details.
//
// Licensed under the Apache License, Version 2.0 <http://www.apache.org/licenses/LICENSE-2.0> or
// the MIT license <http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

/*!
 * Approximate character encoding detection, for some unfortunate formats like BMS.
 *
 * # Algorithm
 *
 * Theoretically, this module implements a multiclass classifier using one naive Bayesian
 * classifier per one character encoding. This algorithm assumes that a certain class of characters
 * *in a particular encoding* exhibits a common (beta) distribution, and multiple occurrences of
 * the same character class do not affect the confidence This is because we deal with very short
 * multibyte sequences, sometimes of one or two characters and at most some tens of them, and
 * they exhibit wildly different distributions compared to mid- or large-sized texts. It makes most
 * existing open-source solutions (including Mozilla's chardet library) unusable in this regard.
 *
 * Since Bayesian classifier naturally supports multiclass classifier, using multiple classifiers
 * seems absurd. But we cannot easily extract the common set of orthogonal features from a single
 * multibyte sequence (notorious example being the half-width Katakana in Shift_JIS), and we need
 * the common set of *classes* with the same distribution in order to use a multiclass classifier!
 * As such thing does not exist in practice, this is not quite bad as it sounds.
 *
 * The classes of "related" characters are calculated from the row indices of corresponding CJK
 * character sets. This is good for several reasons: legacy CJK character sets have several regions
 * containing related characters (e.g. Hangul and Hanja is clearly grouped in KS X 1001),
 * so using the row indices exploits the existing useful property of character sets.
 * We use additional classes for characters not in the particular character set (e.g. Unified
 * Hangul Code not in KS X 1001) and, obviously, for U+FFFD.
 *
 * On a side note, ruv-it clearly seems to use a detection algorithm using the "first byte of
 * multibyte sequence". While the exact algorithm remains elusive (does ruv-it use a pairwise
 * distribution comparison, for example?) ruv-it's algorithm bears some similarity with
 * the algorithm implemented by Sonorous, as the first byte of multibyte sequence is generally
 * correlated with the row index of character sets.
 */

use util::maybe_owned::{MaybeOwnedVec, IntoMaybeOwnedVec};

/// An iterator returned by `CharClass::classes`.
pub struct CharClasses {
    seen: Vec<u64>,
    index: uint,
}

impl Iterator<uint> for CharClasses {
    fn next(&mut self) -> Option<uint> {
        let limit = self.seen.len() * 64;
        let mut i = self.index;
        while i < limit {
            if (self.seen.as_slice()[i/64] >> (i%64)) & 1 == 1 {
                self.index = i + 1;
                return Some(i);
            }
            i += 1;
        }
        self.index = i;
        None
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        (0, Some(64 * self.seen.len() - self.index))
    }
}

/// Character classes definitions for each language.
pub trait CharClass {
    /// Number of character classes. The class #0 is always reserved for U+FFFD.
    fn num_classes(&self) -> uint;

    /// Returns an index to the character class of given character if any.
    /// None means a neutral character, like whitespaces.
    fn from_char(&self, c: char) -> Option<uint>;

    /// Returns default "positive" and "negative" frequencies for given character class.
    /// See also `Trainer::add_default`.
    fn default_freq(&self, cc: uint) -> (f64,f64);

    /// Returns an iterator over character classes present in given string.
    /// Currently returns distinct classes.
    fn classes(&self, s: &str) -> CharClasses {
        let mut seen = Vec::from_elem((self.num_classes() + 63) / 64, 0u64);
        for c in s.chars() {
            match self.from_char(c) {
                Some(cls) if (seen.as_slice()[cls/64] >> (cls%64)) & 1 == 0 => {
                    seen.as_mut_slice()[cls/64] |= 1 << (cls%64);
                }
                _ => {}
            }
        }
        CharClasses { seen: seen, index: 0 }
    }
}

/// Character classes for Korean.
#[deriving(Clone)]
pub struct CharClassKo;

impl CharClass for CharClassKo {
    // 0:      U+FFFD
    // 1-94:   first byte 0xa1-0xfe, KS X 1001
    // 95-166: first byte 0x81-0xc7, UHC

    fn num_classes(&self) -> uint { 167 }

    fn from_char(&self, c: char) -> Option<uint> {
        use encoding::index::euc_kr;
        if c < '\u007f' {
            None
        } else {
            let ptr = euc_kr::backward(c as u32);
            if ptr == 0xffff {
                Some(0)
            } else if ptr < (26 + 26 + 126) * (0xc7 - 0x81) {
                let lead = ptr / (26 + 26 + 126);
                let trail = ptr % (26 + 26 + 126);
                if lead < 0xa1 - 0x81 || trail < 26 + 26 + 32 {
                    Some((1 + (0xff - 0xa1) + lead) as uint)
                } else {
                    Some((1 + lead - (0xa1 - 0x81)) as uint)
                }
            } else {
                let ptr = ptr - (26 + 26 + 126) * (0xc7 - 0x81);
                let lead = ptr / 94 + (0xc7 - 0xa1);
                Some((1 + lead) as uint)
            }
        }
    }

    fn default_freq(&self, cc: uint) -> (f64,f64) {
        if cc == 0 {
            (0.0, 99.0)
        } else if cc >= 1 + (0xff - 0xa1) {
            (0.0, 9.0)
        } else {
            (0.0, 0.0)
        }
    }
}

/// Character classes for Japanese.
#[deriving(Clone)]
pub struct CharClassJa;

impl CharClass for CharClassJa {
    // 0:      U+FFFD
    // 1:      first byte 0xa1-0xdf, JIS X 0201
    // 2-63:   first byte 0x81-0x9f, JIS X 0208 (even: second byte < 0xf9, odd: >= 0xf9)
    // 64-95:  first byte 0xe0-0xef, JIS X 0208 (ditto)
    // 96-121: first byte 0xf0-0xfc, Shift_JIS extension (ditto)

    fn num_classes(&self) -> uint { 122 }

    fn from_char(&self, c: char) -> Option<uint> {
        use encoding::index::jis0208;
        match c {
            '\u0000'...'\u0080'|'\u00a5'|'\u203e' => None,
            '\uff61'...'\uff9f' => Some(1),
            _ => {
                let ptr = jis0208::backward(c as u32);
                if ptr == 0xffff {
                    Some(0)
                } else {
                    Some((2 + ptr / 94) as uint)
                }
            }
        }
    }

    fn default_freq(&self, cc: uint) -> (f64,f64) {
        if cc == 0 {
            (0.0, 99.0)
        } else if cc == 1 {
            (0.0, 9.0)
        } else {
            (0.0, 0.0)
        }
    }
}

/// The trainer for encoding detector.
#[deriving(Clone)]
pub struct Trainer<CC> {
    /// Character classes definition.
    cc: CC,
    /// Weighted "positive" and "negative" frequencies for each character class.
    weights: Vec<(f64,f64)>,
}

impl<CC:CharClass> Trainer<CC> {
    /// Returns a trainer without any data.
    pub fn new(cc: CC) -> Trainer<CC> {
        let nclasses = cc.num_classes();
        Trainer { cc: cc, weights: Vec::from_elem(nclasses, (0.0f64, 0.0f64)) }
    }

    /// Adds a set of default frequencies to the trainer.
    pub fn add_default(&mut self) {
        for cls in range(0, self.weights.len()) {
            let (ref mut pos, ref mut neg) = self.weights.as_mut_slice()[cls];
            let (pos_, neg_) = self.cc.default_freq(cls);
            *pos += pos_;
            *neg += neg_;
        }
    }

    /// Trains the trainer using given string in given encoding (if `positive` is true)
    /// or not in given encoding (if false).
    pub fn train(&mut self, s: &str, positive: bool) {
        let posw = if positive {1.0} else {0.0};
        let negw = if positive {0.0} else {1.0};
        for cls in self.cc.classes(s) {
            let (ref mut pos, ref mut neg) = self.weights.as_mut_slice()[cls];
            *pos += posw;
            *neg += negw;
        }
    }

    /// Scales the frequencies with given factor.
    pub fn scale(&mut self, scale: f64) {
        for cls in range(0, self.weights.len()) {
            let (ref mut pos, ref mut neg) = self.weights.as_mut_slice()[cls];
            *pos *= scale;
            *neg *= scale;
        }
    }

    /// Merges the frquencies of two trainers.
    pub fn merge(&mut self, trainer: Trainer<CC>) {
        assert!(self.weights.len() == trainer.weights.len());
        for cls in range(0, self.weights.len()) {
            let (ref mut pos, ref mut neg) = self.weights.as_mut_slice()[cls];
            let (pos_, neg_) = trainer.weights.as_slice()[cls];
            *pos += pos_;
            *neg += neg_;
        }
    }

    /// Calculates the probabilities for the classifier. The original trainer is destroyed.
    pub fn into_classifier(self) -> Classifier<CC> {
        let Trainer { weights, cc } = self;
        let logprobs: Vec<i32> = weights.move_iter().map(|(pos, neg)| {
            let p = (pos + 1.0) / (pos + neg + 2.0);
            (((1.0 - p).ln() - p.ln()) * 65536.0).round() as i32
        }).collect();
        Classifier::new(cc, logprobs)
    }
}

/// The encoding detector.
#[deriving(Clone)]
pub struct Classifier<CC> {
    /// Character classes definition.
    cc: CC,
    /// Precalculated probabilities for each class. For the ease of calculation, it is not an exact
    /// probability `p` but rather `round((log(1-p)-log(p))*2^16)`.
    logprobs: MaybeOwnedVec<'static,i32>
}

/// Converts a raw confidence value from `Classifier::raw_confidence`
/// to the true probability from `Classifier::confidence`.
pub fn convert_raw_confidence(v: i32) -> f64 {
    1.0 / (1.0 + (v as f64 / 65536.0).exp())
}

impl<CC:CharClass> Classifier<CC> {
    /// Creates a classifier from given precalculated probabilities.
    pub fn new<T:IntoMaybeOwnedVec<'static,i32>>(cc: CC, logprobs: T) -> Classifier<CC> {
        let logprobs = logprobs.into_maybe_owned_vec();
        assert!(logprobs.len() == cc.num_classes());
        Classifier { cc: cc, logprobs: logprobs }
    }

    /// Returns a raw value (i.e. `2^16 log(1/p-1)`) of the confidence for given string.
    /// Bigger the value, unlikely the string being encoded in given encoding.
    pub fn raw_confidence(&self, s: &str) -> i32 {
        let mut confidence = 0;
        for cls in self.cc.classes(s) {
            confidence += self.logprobs.as_slice()[cls];
        }
        confidence
    }

    /// Returns a probability [0..1] of given string being encoded in given encoding.
    /// Use `raw_confidence` if you only want to compare confidences from multiple detectors.
    pub fn confidence(&self, s: &str) -> f64 {
        convert_raw_confidence(self.raw_confidence(s))
    }
}

/**
 * An entry point for `chardet-train` subprogram.
 * It receives one argument, rescaling factor, which is used to normalize the frequencies
 * for each encoding. Smaller factor creates a forgiving detector, and vice versa.
 *
 * This subprogram does not prefer the particular language during the training.
 * Rather, it assumes that one string is likely to appear in some encoding
 * if it can be encoded in that encoding, even when that string is not in the prefered language.
 * It's because many artists tried to use as many characters available as possible;
 * for example, there are instances of Cyrillic characters in both encodings.
 */
#[cfg(not(no_subprogram))]
pub fn chardet_train(args: &[String]) -> int {
    use std::io::{stdin, stderr, BufferedReader};
    use encoding::{Encoding, EncodeStrict, DecodeReplace};
    use encoding::all::{WINDOWS_949, WINDOWS_31J};

    if args.len() != 1 {
        let _ = write!(&mut stderr(),
                       "Usage: {prog} --subprogram chardet-train <rescale> \
                               < per-line-corpus-in-utf-8 > code.rs\n", prog = ::exename());
        return 1;
    }

    let rescale = match from_str::<f64>(args[0].as_slice()) {
        Some(v) if v > 0.0 => v,
        _ => {
            let _ = write!(&mut stderr(), "Invalid rescale argument: {}\n", args[1]);
            return 1;
        }
    };

    // `<lang1>trainer<lang2>` is a trainer using character classes for <lang1> and
    // weights scaled as <lang2>. this makes the training a bit faster.
    let mut kotrainerko = Trainer::new(CharClassKo);
    let mut kotrainerja = Trainer::new(CharClassKo);
    let mut jatrainerko = Trainer::new(CharClassJa);
    let mut jatrainerja = Trainer::new(CharClassJa);
    let mut nkowords = 0u;
    let mut njawords = 0u;

    let mut stream = BufferedReader::new(stdin());
    let words: Vec<String> =
        stream.lines().map(|s| s.unwrap()).filter(|s| !s.as_slice().trim().is_empty()).collect();
    let nwords = words.len();
    for (i, w) in words.move_iter().enumerate() {
        if (i + 1) % 10000 == 0 {
            let _ = write!(&mut stderr(), "Processing {} out of {} words...\n", i + 1, nwords);
        }
        match WINDOWS_949.encode(w.as_slice(), EncodeStrict) {
            Ok(w_) => {
                let w_ = WINDOWS_31J.decode(w_.as_slice(), DecodeReplace).unwrap();
                nkowords += 1;
                kotrainerko.train(w.as_slice(), true);
                jatrainerko.train(w_.as_slice(), false);
            }
            Err(..) => {}
        }
        match WINDOWS_31J.encode(w.as_slice(), EncodeStrict) {
            Ok(w_) => {
                let w_ = WINDOWS_949.decode(w_.as_slice(), DecodeReplace).unwrap();
                njawords += 1;
                jatrainerja.train(w.as_slice(), true);
                kotrainerja.train(w_.as_slice(), false);
            }
            Err(..) => {}
        }
    }

    if nkowords == 0 {
        let _ = write!(&mut stderr(), "There are no Korean words available.\n");
        return 1;
    }
    if njawords == 0 {
        let _ = write!(&mut stderr(), "There are no Japanese words available.\n");
        return 1;
    }

    kotrainerko.scale(rescale / nkowords as f64);
    jatrainerko.scale(rescale / nkowords as f64);
    kotrainerja.scale(rescale / njawords as f64);
    jatrainerja.scale(rescale / njawords as f64);

    kotrainerko.merge(kotrainerja);
    jatrainerja.merge(jatrainerko);

    fn process_trainer<CC:CharClass+Clone>(mut trainer: Trainer<CC>, varname: &str) {
        trainer.add_default();
        let classifier = trainer.into_classifier();
        print!("static {}: &'static [i32] = &[", varname);
        for (i, &v) in classifier.logprobs.as_slice().iter().enumerate() {
            if i % 10 == 0 { print!("\n   "); }
            print!(" {:7i},", v);
        }
        println!("\n];\n");
    }

    process_trainer(kotrainerko, "LOG_PROBS_KO");
    process_trainer(jatrainerja, "LOG_PROBS_JA");
    return 0;
}

