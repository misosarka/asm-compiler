use crate::{
    comment::comment,
    error::{Error, ErrorKind::ExpectedEndOfValue, MapError},
    Input, Res,
};

use nom::{
    branch::alt,
    character::complete::{multispace1, one_of},
    combinator::{eof, verify},
    multi::{many0, many1_count},
    sequence::{pair, terminated},
    Parser,
};

const KEYWORDS: &[&str] = &[
    "end", "mov", "psh", "pul", "val", "adr", "psv", "get", "adf", "set", "tra", "sea", "psr",
    "tar", "psa", "gea", "ada", "tfr", "tfa", "psf", "plr", "pla", "add", "sub", "mul", "and",
    "orb", "xor", "shl", "shr", "aad", "asb", "aml", "aan", "aor", "axr", "asl", "asr", "inc",
    "dec", "not", "jmp", "jfl", "jnf", "jze", "jnz", "jma", "cal", "ret", "ipa", "ipb", "ipc",
    "out", "slp", "rnd", "utc", "tim", "reg", "ar", "fp", "stk",
];

/// Parses an identifier containing letters (a-zA-Z), numbers (0-9) and underscores (_).
/// The first character must not be a number.
///
/// The identifier is also checked against a list of keywords.
///
/// Does not use [`last`].
///
/// Returns a [`Nom`](crate::error::ErrorKind::Nom) error.
pub(crate) fn identifier(input: Input) -> Res<String> {
    let first_char = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_");
    let other_char = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789");
    let parser = pair(first_char, many0(other_char)).map(|(c, v)| {
        let mut s = c.to_string();
        s.extend(v);
        s
    });
    verify(parser, |r| !KEYWORDS.contains(&r))(input)
}

/// Consumes an end of file or any amount of whitespace and comments.
///
/// On error, returns [`ExpectedEndOfValue`].
pub(crate) fn end_of_value(input: Input) -> Res<()> {
    alt((
        eof.map(|_| ()),
        many1_count(alt((multispace1.map(|_| ()), comment))).map(|_| ()),
    ))
    .parse(input)
    .map_error(ExpectedEndOfValue)
}

/// Ensures the result of the provided `parser` is followed by whitespace, comment or end of file.
/// Also consumes all of this whitespace/comments.
///
/// # Errors
/// * Errors from `parser`
/// * [`ExpectedEndOfValue`] (when no whitespace, comment or end of file is found)
pub(crate) fn last<'a, O>(
    parser: impl Parser<Input<'a>, O, Error<'a>>,
) -> impl FnMut(Input<'a>) -> Res<'a, O> {
    terminated(parser, end_of_value)
}

/*
/// Returns true if the character is whitespace or '#'.
fn is_whitespace_comment(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '#'
}

/// Matches everything until whitespace or start of comment.
pub(crate) fn word(input: Input) -> Res<Input> {
    take_till(is_whitespace_comment)(input)
}
*/
