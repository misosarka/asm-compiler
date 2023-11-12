use crate::{
    error::{ErrorKind::InvalidValueLiteral, MapError},
    identifier::last,
    number::literal_u32,
    Input, Res,
};

use nom::Parser;

/// A numeric value.
#[derive(PartialEq, Debug, Clone)]
pub(crate) enum Value32 {
    Literal(u32),
}

/// Parses a numeric value.
///
/// All of its subparsers should use [`last`].
///
/// # Errors
/// * [`InvalidValueLiteral`] (from [`value32_literal`])
pub(crate) fn value32(input: Input) -> Res<Value32> {
    value32_literal(input)
}

/// Parses a u32 literal.
///
/// Uses [`last`].
///
/// On error, returns [`InvalidValueLiteral`].
fn value32_literal(input: Input) -> Res<Value32> {
    last(literal_u32)
        .map(Value32::Literal)
        .parse(input)
        .map_error(InvalidValueLiteral)
}
