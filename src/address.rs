use crate::error::{
    ErrorKind::{AddressExpected, InvalidAddressLiteral, InvalidIdentifierCharacter},
    MapError,
};
use crate::identifier::{identifier, last};
use crate::number::literal_u32;
use crate::{Input, Res};

use nom::{
    character::complete::{anychar, char},
    combinator::{cut, fail, peek},
    sequence::preceded,
    Parser,
};

/// Representation of an address.
#[derive(PartialEq, Debug, Clone)]
pub(crate) enum Address {
    Literal(u32),
    Label(String),
}

/// Parses an address.
///
/// All of its subparsers should use [`last`].
///
/// # Errors
/// * [`AddressExpected`] (when there is no '@' or there is an invalid character or EOF after it)
/// * [`InvalidAddressLiteral`] (from [`address_literal`])
/// * [`InvalidIdentifierCharacter`] (from [`address_label`])
pub(crate) fn address(input: Input) -> Res<Address> {
    let (input, _) = char('@')(input).map_error(AddressExpected)?;
    let (input, c) = cut(peek(anychar))(input).map_error(AddressExpected)?;
    if c.is_ascii_digit() {
        cut(address_literal)(input)
    } else if c.is_ascii_alphabetic() || c == '_' {
        cut(address_label)(input)
    } else {
        cut(fail)(input).map_error(AddressExpected)
    }
}

/// Parses an address literal (without the '@').
///
/// Uses [`last`].
///
/// On error, returns [`InvalidAddressLiteral`].
fn address_literal(input: Input) -> Res<Address> {
    last(literal_u32)
        .map(Address::Literal)
        .parse(input)
        .map_error(InvalidAddressLiteral)
}

/// Parses an address specified by a label (without the '@').
///
/// Uses [`last`].
///
/// On error, returns [`InvalidIdentifierCharacter`].
fn address_label(input: Input) -> Res<Address> {
    last(identifier)
        .map(Address::Label)
        .parse(input)
        .map_error(InvalidIdentifierCharacter)
}

/// Parses a label (prefixed by '$').
///
/// Uses [`last`].
///
/// On error, returns [`InvalidIdentifierCharacter`].
pub(crate) fn label(input: Input) -> Res<String> {
    preceded(char('$'), cut(last(identifier)))(input).map_error(InvalidIdentifierCharacter)
}
