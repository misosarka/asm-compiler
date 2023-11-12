use crate::{identifier::last, Input, Res};
use nom::Parser;
use nom::{bytes::complete::take_till, character::complete::char, sequence::preceded};

/// Parses a comment.
///
/// Uses [`last`].
///
/// Returns a [`Nom`](crate::error::ErrorKind::Nom) error.
pub(crate) fn comment(input: Input) -> Res<()> {
    preceded(char('#'), last(take_till(|c| c == '\n')))
        .map(|_| ())
        .parse(input)
}
