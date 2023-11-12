use nom::{
    combinator::fail,
    error::{ErrorKind as NomErrorKind, FromExternalError, ParseError},
};

use crate::{Input, Res};

#[derive(Debug, PartialEq)]
pub(crate) struct Error<'a> {
    pub kind: ErrorKind,
    pub input: Input<'a>,
}

impl<'a> Error<'a> {
    pub fn new(kind: ErrorKind, input: Input) -> Error {
        Error { kind, input }
    }
}

impl<'a> ParseError<Input<'a>> for Error<'a> {
    fn from_error_kind(input: Input<'a>, kind: NomErrorKind) -> Self {
        Self {
            kind: ErrorKind::Nom(kind),
            input,
        }
    }

    fn append(_input: Input<'a>, _kind: NomErrorKind, other: Self) -> Self {
        other
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    Nom(NomErrorKind),
    AddressExpected,
    InvalidIdentifierCharacter,
    InvalidValueLiteral,
    InvalidAddressLiteral,
    InvalidOperationTarget,
    ExpectedEndOfValue,
    InvalidStatement,
    InvalidMoveSource,
    InvalidMoveTarget,
    UnsupportedMove,
}

pub(crate) trait MapError<'a, T> {
    fn map_error(self, kind: ErrorKind) -> Res<'a, T>;
}

impl<'a, T> MapError<'a, T> for Res<'a, T> {
    fn map_error(self, kind: ErrorKind) -> Res<'a, T> {
        self.map_err(|err| err.map(|e| Error::new(kind, e.input)))
    }
}

impl<'a, E> FromExternalError<Input<'a>, E> for Error<'a> {
    fn from_external_error(input: Input<'a>, kind: NomErrorKind, _e: E) -> Self {
        Error {
            kind: ErrorKind::Nom(kind),
            input,
        }
    }
}

pub(crate) fn fail_with<'a, O>(kind: ErrorKind) -> impl FnMut(Input<'a>) -> Res<'a, O> {
    move |input| fail(input).map_error(kind)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BuildErrorKind {
    Syntax(ErrorKind),
    NonexistentLabel,
    DuplicatedLabel,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BuildError {
    kind: BuildErrorKind,
    location: Option<usize>, // TODO: remove Option and make every error source know its location
}

impl BuildError {
    pub(crate) fn new(kind: BuildErrorKind, location: Option<usize>) -> Self {
        Self { kind, location }
    }

    pub(crate) fn from_syntax_error(error: Error, complete_input: Input) -> Self {
        let location = complete_input.len() - error.input.len();
        Self {
            kind: BuildErrorKind::Syntax(error.kind),
            location: Some(location),
        }
    }
}
