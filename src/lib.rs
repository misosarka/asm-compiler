mod address;
mod bytecode;
mod comment;
mod error;
mod identifier;
mod instructions;
mod intermediate;
mod moves;
mod number;
mod ops;
mod statement;
#[cfg(test)]
mod tests;
mod value;

use crate::bytecode::to_bytecode;
use crate::error::{BuildError, Error};
use crate::intermediate::to_intermediate;
use crate::statement::parse;

use nom::IResult;

pub(crate) type Input<'a> = &'a str;
pub(crate) type Res<'a, Output> = IResult<Input<'a>, Output, Error<'a>>;

pub fn compile(code: &str) -> Result<Box<[u8]>, BuildError> {
    parse(code).and_then(to_intermediate).and_then(to_bytecode)
}
