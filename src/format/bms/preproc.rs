// This is a part of Sonorous.
// Copyright (c) 2005, 2007, 2009, 2012, 2013, 2014, Kang Seonghoon.
// See README.md and LICENSE.txt for details.

//! BMS preprocessor.

use std::fmt;
use rand::Rng;

use format::bms::diag::*;

/// Represents one line of BMS command that may affect the control flow.
#[deriving(Clone)]
pub enum BmsFlowCommand {
    BmsRandom(int),                             // #RANDOM
    BmsSetRandom(int),                          // #SETRANDOM
    BmsEndRandom,                               // #ENDRANDOM
    BmsIf(int),                                 // #IF
    BmsElseIf(int),                             // #ELSEIF
    BmsElse,                                    // #ELSE
    BmsEndIf,                                   // #ENDIF
    BmsSwitch(int),                             // #SWITCH
    BmsSetSwitch(int),                          // #SETSWITCH
    BmsEndSw,                                   // #ENDSW
    BmsCase(int),                               // #CASE
    BmsSkip,                                    // #SKIP
    BmsDef,                                     // #DEF
}

impl fmt::Show for BmsFlowCommand {
    /// Returns a reconstructed line for given BMS flow command.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BmsRandom(val) => write!(f.buf, "\\#RANDOM {}", val),
            BmsSetRandom(val) => write!(f.buf, "\\#SETRANDOM {}", val),
            BmsEndRandom => write!(f.buf, "\\#ENDRANDOM"),
            BmsIf(val) => write!(f.buf, "\\#IF {}", val),
            BmsElseIf(val) => write!(f.buf, "\\#ELSEIF {}", val),
            BmsElse => write!(f.buf, "\\#ELSE"),
            BmsEndIf => write!(f.buf, "\\#ENDIF"),
            BmsSwitch(val) => write!(f.buf, "\\#SWITCH {}", val),
            BmsSetSwitch(val) => write!(f.buf, "\\#SETSWITCH {}", val),
            BmsEndSw => write!(f.buf, "\\#ENDSW"),
            BmsCase(val) => write!(f.buf, "\\#CASE {}", val),
            BmsSkip => write!(f.buf, "\\#SKIP"),
            BmsDef => write!(f.buf, "\\#DEF"),
        }
    }
}

/// The state of the block, for determining which lines should be processed.
#[deriving(Eq)]
enum BlockState {
    /// Not contained in the #IF block.
    Outside,
    /// Active.
    Process,
    /// Inactive, but (for the purpose of #IF/#ELSEIF/#ELSE/#ENDIF structure) can move to
    /// `Process` state when matching clause appears.
    Ignore,
    /// Inactive and won't be processed until the end of block.
    NoFurther
}

impl BlockState {
    /// Returns true if lines should be ignored in the current block given that the parent
    /// block was active.
    fn inactive(self) -> bool {
        match self { Outside | Process => false, Ignore | NoFurther => true }
    }
}

/**
 * Block information. The parser keeps a list of nested blocks and determines if
 * a particular line should be processed or not.
 *
 * Sonorous actually recognizes only one kind of blocks, starting with #RANDOM or
 * #SETRANDOM and ending with #ENDRANDOM or #END(IF) outside an #IF block. An #IF block is
 * a state within #RANDOM, so it follows that #RANDOM/#SETRANDOM blocks can nest but #IF
 * can't nest unless its direct parent is #RANDOM/#SETRANDOM.
 */
#[deriving(Eq)]
struct Block {
    /// A generated value if any. It can be `None` if this block is the topmost one (which
    /// is actually not a block but rather a sentinel) or the last `#RANDOM` or `#SETRANDOM`
    /// command was invalid, and #IF in that case will always evaluates to false.
    val: Option<int>,
    /// The state of the block.
    state: BlockState,
    /// True if the parent block is already ignored so that this block should be ignored
    /// no matter what `state` is.
    skip: bool
}

/// A generic BMS preprocessor. `T` is normally a BMS command, but there is no restriction.
pub struct Preprocessor<'r,T,R> {
    /// The current block informations.
    priv blocks: ~[Block],
    /// Random number generator.
    priv r: &'r mut R,
}

impl<'r,T:Send+Clone,R:Rng> Preprocessor<'r,T,R> {
    /// Creates a new preprocessor with given RNG.
    pub fn new(r: &'r mut R) -> Preprocessor<'r,T,R> {
        let blocks = ~[Block { val: None, state: Outside, skip: false }];
        Preprocessor { blocks: blocks, r: r }
    }

    /// Returns true if any command which appears at this position should be ignored.
    pub fn inactive(&self) -> bool {
        let last = self.blocks.last().unwrap();
        last.skip || last.state.inactive()
    }

    /// Adds the non-flow command (or any appropriate data) into the preprocessor.
    /// `messages` will have zero or more messages inserted.
    /// `result` will have zero or more preprocessed commands (or any appropriate data) inserted.
    pub fn feed_other(&mut self, cmd: T, _messages: &mut ~[BmsMessage], result: &mut ~[T]) {
        if !self.inactive() {
            result.push(cmd);
        }
    }

    /// Adds the flow command into the preprocessor.
    /// `messages` will have zero or more messages inserted.
    /// `result` will have zero or more preprocessed commands (or any appropriate data) inserted.
    pub fn feed_flow(&mut self, _lineno: Option<uint>, flow: &BmsFlowCommand,
                     messages: &mut ~[BmsMessage], _result: &mut ~[T]) {
        let inactive = self.inactive();
        match *flow {
            BmsRandom(val) | BmsSetRandom(val) => {
                let val = if val <= 0 {None} else {Some(val)};
                let setrandom = match *flow { BmsSetRandom(..) => true, _ => false };

                // do not generate a random value if the entire block is skipped (but it
                // still marks the start of block)
                let generated = val.and_then(|val| {
                    if setrandom {
                        Some(val)
                    } else if !inactive {
                        Some(self.r.gen_range(1, val + 1))
                    } else {
                        None
                    }
                });
                self.blocks.push(Block { val: generated, state: Outside, skip: inactive });
            }
            BmsEndRandom => {
                if self.blocks.len() > 1 { self.blocks.pop(); }
            }
            BmsIf(val) | BmsElseIf(val) => {
                let val = if val <= 0 {None} else {Some(val)};
                let haspriorelse = match *flow { BmsElseIf(..) => true, _ => false };

                let last = &mut self.blocks[self.blocks.len() - 1];
                last.state =
                    if (!haspriorelse && !last.state.inactive()) || last.state == Ignore {
                        if val.is_none() || val != last.val {Ignore} else {Process}
                    } else {
                        NoFurther
                    };
            }
            BmsElse => {
                let last = &mut self.blocks[self.blocks.len() - 1];
                last.state = if last.state == Ignore {Process} else {NoFurther};
            }
            BmsEndIf => {
                for &idx in self.blocks.iter().rposition(|&i| i.state != Outside).iter() {
                    if idx > 0 { self.blocks.truncate(idx + 1); }
                }

                let last = &mut self.blocks[self.blocks.len() - 1];
                last.state = Outside;
            }
            BmsSwitch(..) | BmsSetSwitch(..) | BmsEndSw | BmsCase(..) | BmsSkip | BmsDef => {
                messages.push(BmsHasUnimplementedFlow);
            }
        }
    }

    /// Terminates the preprocessor.
    /// `messages` will have zero or more messages inserted.
    /// `result` will have zero or more preprocessed commands (or any appropriate data) inserted.
    pub fn finish(&mut self, _messages: &mut ~[BmsMessage], _result: &mut ~[T]) {
    }
}

#[cfg(test)]
mod tests {
    use rand::task_rng;
    use super::Preprocessor;

    macro_rules! with_pp(
        (|$pp:ident| $blk:expr) => ({
            let mut r = task_rng();
            let mut $pp = Preprocessor::new(&mut r);
            $blk;
        })
    )

    #[test]
    fn test_no_flow() {
        with_pp!(|pp| {
            let mut messages = ~[];
            let mut out = ~[];
            pp.feed_other(42, &mut messages, &mut out);
            assert!(messages.as_slice() == []);
            assert!(out.as_slice() == [42]);
            messages.clear();
            out.clear();
            pp.finish(&mut messages, &mut out);
            assert!(messages.as_slice() == []);
            assert!(out.as_slice() == []);
        })
    }

    // TODO add more tests
}

