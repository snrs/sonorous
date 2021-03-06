// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Diagnostics for BMS format.

#![allow(non_upper_case_globals)]

use std::fmt;

/// The severity of messages. Every error message has one of the severity assigned.
#[deriving(PartialEq,Eq,PartialOrd,Show,Clone)]
pub enum Severity {
    /// Various notes. This kind of diagnostics does not affect the game play at all but indicates
    /// possible incompatibilities or deprecated features.
    Note,
    /// Warning message. This kind of diagnostics does not affect the game play a lot but has
    /// visible and generally unwanted effects. The caller is recommended to show these messages
    /// when the BMS file was played for the first time.
    Warning,
    /// Fatal error message. This kind of diagnostics means that it's impossible to play this BMS
    /// file without massively affecting the game play. The caller is recommended to abort
    /// the loading when received this message.
    Fatal,
}

/// Messages for BMS format.
#[deriving(PartialEq,Eq,Clone)]
pub struct BmsMessage {
    /// The severity of message.
    pub severity: Severity,
    /// The internal identifier.
    pub id: &'static str,
    /// The string representation of the message.
    pub message: &'static str,
}

impl fmt::Show for BmsMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub static BmsHasNegativeInitBPM: BmsMessage = BmsMessage {
    severity: Severity::Fatal,
    id: "neg-init-bpm",
    message: "Initial #BPM cannot be negative. This line will be ignored.",
};

pub static BmsHasZeroInitBPM: BmsMessage = BmsMessage {
    severity: Severity::Fatal,
    id: "zero-init-bpm",
    message: "Initial #BPM cannot be zero. This line will be ignored.",
};

pub static BmsHasNegativeSTOPDuration: BmsMessage = BmsMessage {
    severity: Severity::Fatal,
    id: "neg-stop-duration",
    message: "#STOP duration cannot be negative. This #STOP will be ignored.",
};

pub static BmsHasNegativeSTPDuration: BmsMessage = BmsMessage {
    severity: Severity::Fatal,
    id: "neg-stp-duration",
    message: "#STP duration cannot be negative. This line will be ignored.",
};

pub static BmsHasNoTITLE: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "no-title",
    message: "#TITLE is missing.",
};

pub static BmsHasEmptyTITLE: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "empty-title",
    message: "#TITLE is empty.",
};

pub static BmsHasMultipleTITLEs: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "multiple-title",
    message: "There are multiple #TITLE commands. Only the last such line will be used.",
};

pub static BmsUsesCouplePlay: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "couple-play",
    message: "Support for Couple Play (#PLAYER 2) is limited.",
};

pub static BmsUsesBattlePlay: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "battle-play",
    message: "Battle Play (#PLAYER 4) is not supported, and will be treated as Single Play.",
};

pub static BmsHasInvalidLNTYPE: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "invalid-lntype",
    message: "Invalid #LNTYPE value will be ignored.",
};

pub static BmsHasZeroLNOBJ: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "zero-lnobj",
    message: "#LNOBJ 00 is invalid and will be ignored.",
};

pub static BmsHasMultipleLNOBJs: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "multiple-lnobj",
    message: "There are multiple #LNOBJ commands. Only the last such line will be used.",
};

pub static BmsHasUnimplementedFlow: BmsMessage = BmsMessage {
    severity: Severity::Warning,
    id: "unimplemented-flow",
    message: "#SWITCH and related flow commands are not yet implemented and may malfunction.",
};

pub static BmsUsesLegacyEncoding: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "legacy-encoding",
    message: "The file is encoded in the legacy CJK encodings. Their continued use is discouraged.",
};

pub static BmsHasFullWidthSharp: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "full-width-sharp",
    message: "# should be a half-width letter for the compatibility.",
};

pub static BmsHasOneDigitAlphanumericKey: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "one-digit-key",
    message: "One-digit alphanumeric key is assumed to be prepended by 0 digit.",
};

pub static BmsHasNoARTIST: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "no-artist",
    message: "#ARTIST is missing.",
};

pub static BmsHasEmptyARTIST: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "empty-artist",
    message: "#ARTIST is empty.",
};

pub static BmsHasMultipleARTISTs: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "multiple-artist",
    message: "There are multiple #ARTIST commands. Only the last such line will be used.",
};

pub static BmsHasNoGENRE: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "no-genre",
    message: "#GENRE is missing.",
};

pub static BmsHasEmptyGENRE: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "empty-genre",
    message: "#GENRE is empty.",
};

pub static BmsHasMultipleGENREs: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "multiple-genre",
    message: "There are multiple #GENRE commands. Only the last such line will be used.",
};

pub static BmsHasGENLE: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "genle",
    message: "#GENLE [sic] will be interpreted as #GENRE.",
};

pub static BmsHasEmptyPath: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "empty-path",
    message: "Empty path will be ignored.",
};

pub static BmsHasInvalidPLAYER: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "invalid-player",
    message: "Invalid #PLAYER value will be ignored.",
};

pub static BmsHasNegativePLAYLEVEL: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "neg-playlevel",
    message: "#PLAYLEVEL should be non-negative for the compatibility.",
};

pub static BmsHasDIFFICULTYOutOfRange: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "difficulty-out-of-range",
    message: "#DIFFICULTY should be between 1 and 5 for the compatibility.",
};

pub static BmsHasEXBPM: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "exbpm",
    message: "#EXBPMxx is a temporary measure, you should use #BPMxx instead.",
};

pub static BmsHasNonpositiveBPM: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "nonpos-bpm",
    message: "Non-positive BPM is not portable and its use is discouraged.",
};
pub static BmsUsesLNTYPE2: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "lntype2",
    message: "#LNTYPE 2 is deprecated, you should use #LNTYPE 1 (implied) or #LNOBJ instead.",
};

pub static BmsHasUnknownWAVCMD: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "unknown-wavcmd",
    message: "Invalid #WAVCMD command will be ignored.",
};

pub static BmsHasSONG: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "song",
    message: "#SONG is deprecated, you should use #TEXT instead.",
};

pub static BmsHasRONDAM: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "rondam",
    message: "#RONDAM [sic] will be interpreted as #RANDOM.",
};

pub static BmsHasRANDOMWithoutWhitespace: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "rondam-no-ws",
    message: "#RANDOM should be followed by one or more whitespace.",
};

pub static BmsHasIFWithoutWhitespace: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "if-no-ws",
    message: "#IF should be followed by one or more whitespace.",
};

pub static BmsHasIFEND: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "ifend",
    message: "#IFEND [sic] will be interpreted as #ENDIF.",
};

pub static BmsHasENDNotFollowedByIF: BmsMessage = BmsMessage {
    severity: Severity::Note,
    id: "end-without-if",
    message: "#END not followed by IF will be interpreted as #ENDIF.",
};

