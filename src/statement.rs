use crate::address::label;
use crate::error::{fail_with, BuildError, Error, ErrorKind::InvalidStatement};
use crate::identifier::end_of_value;
use crate::instructions::{instruction, Instruction};
use crate::{Input, Res};

use nom::branch::alt;
use nom::combinator::{eof, opt};
use nom::multi::many0;
use nom::sequence::preceded;
use nom::{Finish, Parser};

/// A single statement in the file.
#[derive(Debug)]
pub(crate) enum Statement {
    Instr(Instruction),
    Label(String),
}

// !!! TODO: operation() returns an error that is not cut(), leading to it being overwritten by InvalidStatement

/// Parses a statement and returns it as a [`Statement`].
///
/// All of its subparsers should use [`last`](crate::identifier::last).
///
/// # Errors
/// * Errors from [`instruction`] (except the [`Nom`](crate::error::ErrorKind::Nom) error)
/// * [`InvalidIdentifierCharacter`](crate::error::ErrorKind::InvalidIdentifierCharacter) (from [`label`])
/// * [`InvalidStatement`] (when no statement matched)
fn statement(input: Input) -> Res<Statement> {
    alt((
        instruction.map(Statement::Instr),
        label.map(Statement::Label),
        fail_with(InvalidStatement),
    ))(input)
}

/// Parses a string input as a file and returns a [`Vec`] of [`Statement`]s.
///
/// Needs to consume all of its input to succeed.
fn file(input: Input) -> Res<Vec<Statement>> {
    let (input, vec) = preceded(opt(end_of_value), many0(statement))(input)?;
    if eof::<_, Error>(input).is_ok() {
        Ok((input, vec))
    } else {
        // A statement has errored, parse it again to get the error (do not care about the ok value)
        statement.map(|_| vec![]).parse(input)
    }
}

pub(crate) fn parse(input: Input) -> Result<Vec<Statement>, BuildError> {
    file(input)
        .finish()
        .map(|(_, vec)| vec)
        .map_err(|e| BuildError::from_syntax_error(e, input))
}
