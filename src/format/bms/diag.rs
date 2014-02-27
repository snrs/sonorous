// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Diagnostics for BMS format.

use std::fmt;

/// The severity of messages. Every error message has one of the severity assigned.
#[deriving(Eq,Ord,Show,Clone)]
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
#[deriving(Eq,Clone)]
pub enum BmsMessage {
    BmsHasNegativeInitBPM,
    BmsHasZeroInitBPM,
    BmsHasNegativeSTOPDuration,
    BmsHasNegativeSTPDuration,

    BmsHasNoTITLE,
    BmsHasEmptyTITLE,
    BmsHasMultipleTITLEs,
    BmsUsesCouplePlay,
    BmsUsesBattlePlay,
    BmsHasInvalidLNTYPE,
    BmsHasZeroLNOBJ,
    BmsHasMultipleLNOBJs,
    BmsHasUnimplementedFlow,

    BmsUsesLegacyEncoding,
    BmsHasFullWidthSharp,
    BmsHasOneDigitAlphanumericKey,
    BmsHasNoARTIST,
    BmsHasEmptyARTIST,
    BmsHasMultipleARTISTs,
    BmsHasNoGENRE,
    BmsHasEmptyGENRE,
    BmsHasMultipleGENREs,
    BmsHasGENLE,
    BmsHasEmptyPath,
    BmsHasInvalidPLAYER,
    BmsHasNegativePLAYLEVEL,
    BmsHasDIFFICULTYOutOfRange,
    BmsHasEXBPM,
    BmsHasNonpositiveBPM,
    BmsUsesLNTYPE2,
    BmsHasUnknownWAVCMD,
    BmsHasSONG,
    BmsHasRONDAM,
    BmsHasRANDOMWithoutWhitespace,
    BmsHasIFWithoutWhitespace,
    BmsHasIFEND,
    BmsHasENDNotFollowedByIF,
}

impl BmsMessage {
    /// Returns a tuple of severity and string representation of the message.
    pub fn severity_and_message(&self) -> (Severity, &'static str) {
        match *self {
            BmsHasNegativeInitBPM =>
                (Fatal, "Initial #BPM cannot be negative. This line will be ignored."),
            BmsHasZeroInitBPM =>
                (Fatal, "Initial #BPM cannot be zero. This line will be ignored."),
            BmsHasNegativeSTOPDuration =>
                (Fatal, "#STOP duration cannot be negative. This #STOP will be ignored."),
            BmsHasNegativeSTPDuration =>
                (Fatal, "#STP duration cannot be negative. This line will be ignored."),

            BmsHasNoTITLE =>
                (Warning, "#TITLE is missing."),
            BmsHasEmptyTITLE =>
                (Warning, "#TITLE is empty."),
            BmsHasMultipleTITLEs =>
                (Warning, "There are multiple #TITLE commands. Only the last such line will \
                           be used."),
            BmsUsesCouplePlay =>
                (Warning, "Support for Couple Play (#PLAYER 2) is limited."),
            BmsUsesBattlePlay =>
                (Warning, "Battle Play (#PLAYER 4) is not supported, and will be treated as \
                           Single Play."),
            BmsHasInvalidLNTYPE =>
                (Warning, "Invalid #LNTYPE value will be ignored."),
            BmsHasZeroLNOBJ =>
                (Warning, "#LNOBJ 00 is invalid and will be ignored."),
            BmsHasMultipleLNOBJs =>
                (Warning, "There are multiple #LNOBJ commands. Only the last such line will \
                           be used."),
            BmsHasUnimplementedFlow =>
                (Warning, "#SWITCH and related flow commands are not yet implemented \
                           and may malfunction."),

            BmsUsesLegacyEncoding =>
                (Note, "The file is encoded in the legacy CJK encodings. \
                        Their continued use is discouraged."),
            BmsHasFullWidthSharp =>
                (Note, "# should be a half-width letter for the compatibility."),
            BmsHasOneDigitAlphanumericKey =>
                (Note, "One-digit alphanumeric key is assumed to be prepended by 0 digit."),
            BmsHasNoARTIST =>
                (Note, "#ARTIST is missing."),
            BmsHasEmptyARTIST =>
                (Note, "#ARTIST is empty."),
            BmsHasMultipleARTISTs =>
                (Note, "There are multiple #ARTIST commands. Only the last such line will \
                        be used."),
            BmsHasNoGENRE =>
                (Note, "#GENRE is missing."),
            BmsHasEmptyGENRE =>
                (Note, "#GENRE is empty."),
            BmsHasMultipleGENREs =>
                (Note, "There are multiple #GENRE commands. Only the last such line will \
                        be used."),
            BmsHasGENLE =>
                (Note, "#GENLE [sic] will be interpreted as #GENRE."),
            BmsHasEmptyPath =>
                (Note, "Empty path will be ignored."),
            BmsHasInvalidPLAYER =>
                (Note, "Invalid #PLAYER value will be ignored."),
            BmsHasNegativePLAYLEVEL =>
                (Note, "#PLAYLEVEL should be non-negative for the compatibility."),
            BmsHasDIFFICULTYOutOfRange =>
                (Note, "#DIFFICULTY should be between 1 and 5 for the compatibility."),
            BmsHasEXBPM =>
                (Note, "#EXBPMxx is a temporary measure, you should use #BPMxx instead."),
            BmsHasNonpositiveBPM =>
                (Note, "Non-positive BPM is not portable and its use is discouraged."),
            BmsUsesLNTYPE2 =>
                (Note, "#LNTYPE 2 is deprecated, you should use #LNTYPE 1 (implied) or \
                        #LNOBJ instead."),
            BmsHasUnknownWAVCMD =>
                (Note, "Invalid #WAVCMD command will be ignored."),
            BmsHasSONG =>
                (Note, "#SONG is deprecated, you should use #TEXT instead."),
            BmsHasRONDAM =>
                (Note, "#RONDAM [sic] will be interpreted as #RANDOM."),
            BmsHasRANDOMWithoutWhitespace =>
                (Note, "#RANDOM should be followed by one or more whitespace."),
            BmsHasIFWithoutWhitespace =>
                (Note, "#IF should be followed by one or more whitespace."),
            BmsHasIFEND =>
                (Note, "#IFEND [sic] will be interpreted as #ENDIF."),
            BmsHasENDNotFollowedByIF =>
                (Note, "#END not followed by IF will be interpreted as #ENDIF."),
        }
    }

    /// Returns the severity of the message.
    pub fn severity(&self) -> Severity {
        let (severity, _) = self.severity_and_message();
        severity
    }
}

impl fmt::Show for BmsMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (_, msg) = self.severity_and_message();
        write!(f.buf, "{}", msg)
    }
}

