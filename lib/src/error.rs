use std::cmp::PartialEq;

#[derive(thiserror::Error, Copy, Clone, Debug, PartialEq)]
pub enum Error {
    #[error("assertion failed")]
    Assert,
    #[error("value cannot be applied")]
    CannotApply,
    #[error("cannot convert type: value out of range")]
    Convert,
    #[error("attempt to divide by zero")]
    DivideByZero,
    #[error("duplicate argument")]
    DuplicateArg,
    #[error("unexpected argument")]
    ExtraArgument,
    #[error("general IO error")]
    IO,
    #[error("format 0 must have only 1 track")]
    InvalidFmt0,
    #[error("invalid MIDI format")]
    InvalidFormat,
    #[error("invalid macro definition")]
    InvalidMacro,
    #[error("unexpected right parenthesis")]
    InvalidParen,
    #[error("invalid include path")]
    InvalidPath,
    #[error("invalid time division")]
    InvalidTimeDiv,
    #[error("missing argument")]
    NilArgument,
    #[error("expected a MIDI event")]
    NotMidi,
    #[error("expected an identifier")]
    NotAnIdent,
    #[error("cannot convert type: not a number")]
    NotANumber,
    #[error("expected a quoted expression")]
    NotAQuote,
    #[error("integer overflow")]
    Overflow,
    #[error("invalid number literal")]
    ParseNum,
    #[error("undefined variable")]
    Undefined,
    #[error("integer underflow")]
    Underflow,
    #[error("too many strings")]
    TooManyStrings,
    #[error("too many MIDI tracks")]
    TooManyTracks,
    #[error("too long MIDI track")]
    TrackTooLong,
    #[error("incompatible types")]
    TypeErr,
    #[error("unclosed parenthesis")]
    UnclosedParen,
    #[error("unclosed string literal")]
    UnclosedStr,
}

pub type Result<T> = std::result::Result<T, Error>;
