// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! Diagnostics for BMS format.

/// The severity of messages. Every error message has one of the severity assigned.
#[deriving(Eq,Ord,ToStr,Clone)]
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
    BmsUsesCouplePlay,
    BmsUsesBattlePlay,
    BmsHasInvalidLNTYPE,
    BmsHasZeroLNOBJ,
    BmsHasMultipleLNOBJs,

    BmsHasFullWidthSharp,
    BmsHasNoARTIST,
    BmsHasEmptyARTIST,
    BmsHasNoGENRE,
    BmsHasEmptyGENRE,
    BmsHasGENLE,
    BmsHasEmptyPath,
    BmsHasInvalidPLAYER,
    BmsHasNegativePLAYLEVEL,
    BmsHasEXBPM,
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
            BmsUsesCouplePlay =>
                (Warning, "Support for Couple Play (#PLAYER 2) is limited."),
            BmsUsesBattlePlay =>
                (Warning, "Battle Play (#PLAYER 4) is not supported, and will be treated as \
                           Couple Play."),
            BmsHasInvalidLNTYPE =>
                (Warning, "Invalid #LNTYPE value will be ignored."),
            BmsHasZeroLNOBJ =>
                (Warning, "#LNOBJ 00 is invalid and will be ignored."),
            BmsHasMultipleLNOBJs =>
                (Warning, "There are multiple #LNOBJ commands. Only the last such line will \
                           be used."),

            BmsHasFullWidthSharp =>
                (Note, "# should be a half-width letter for the compatibility."),
            BmsHasNoARTIST =>
                (Note, "#ARTIST is missing."),
            BmsHasEmptyARTIST =>
                (Note, "#ARTIST is empty."),
            BmsHasNoGENRE =>
                (Note, "#GENRE is missing."),
            BmsHasEmptyGENRE =>
                (Note, "#GENRE is empty."),
            BmsHasGENLE =>
                (Note, "#GENLE [sic] will be interpreted as #GENRE."),
            BmsHasEmptyPath =>
                (Note, "Empty path will be ignored."),
            BmsHasInvalidPLAYER =>
                (Note, "Invalid #PLAYER value will be ignored."),
            BmsHasNegativePLAYLEVEL =>
                (Note, "#PLAYLEVEL should be non-negative for the compatibility."),
            BmsHasEXBPM =>
                (Note, "#EXBPMxx is a temporary measure, you should use #BPMxx instead."),
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

impl ToStr for BmsMessage {
    fn to_str(&self) -> ~str {
        let (_, msg) = self.severity_and_message();
        msg.to_owned()
    }
}

/// A standard callback for BMS messages. The line is optional for global messages.
pub type BmsMessageCallback<'self> =
    &'self fn:Copy(line: Option<uint>, message: BmsMessage) -> bool;

