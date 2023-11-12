use crate::{Input, Res};

use nom::{
    branch::alt,
    bytes::complete::{is_a, tag},
    character::complete::{digit1, hex_digit1, oct_digit1},
    combinator::{cut, map_res},
    sequence::preceded,
};

/// Parses an integer literal (dec, hex, oct or bin).
///
/// Does not use [`last`](crate::identifier::last).
///
/// Returns a [`Nom`](crate::error::ErrorKind::Nom) error.
pub(crate) fn literal_u32(input: Input) -> Res<u32> {
    alt((hex_u32, oct_u32, bin_u32, dec_u32))(input)
}

fn hex_u32(input: Input) -> Res<u32> {
    map_res(
        preceded(alt((tag("0x"), tag("0X"))), cut(hex_digit1)),
        |x| u32::from_str_radix(x, 16),
    )(input)
}

fn oct_u32(input: Input) -> Res<u32> {
    map_res(
        preceded(alt((tag("0o"), tag("0O"))), cut(oct_digit1)),
        |x| u32::from_str_radix(x, 8),
    )(input)
}

fn bin_u32(input: Input) -> Res<u32> {
    map_res(
        preceded(alt((tag("0b"), tag("0B"))), cut(is_a("01"))),
        |x| u32::from_str_radix(x, 2),
    )(input)
}

fn dec_u32(input: Input) -> Res<u32> {
    map_res(digit1, |x: Input| x.parse())(input)
}
