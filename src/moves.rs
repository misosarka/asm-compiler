use crate::address::{address, Address};
use crate::error::{
    fail_with,
    ErrorKind::{InvalidMoveSource, InvalidMoveTarget, UnsupportedMove},
    MapError,
};
use crate::identifier::last;
use crate::instructions::Instruction;
use crate::value::{value32, Value32};
use crate::{Input, Res};

use nom::{branch::alt, bytes::complete::tag, combinator::fail, Parser};

/// A source of a move (location or value to be moved from).
enum MoveSource {
    Lit(Value32),
    Addr(Address),
    Reg,
    Ar,
    AtAr,
    Fp,
    Stack,
}

/// A target of a move (location to be moved into).
enum MoveTarget {
    Addr(Address),
    Reg,
    Ar,
    AtAr,
    Stack,
}

/// Parses the source of a move and returns it as a [`MoveSource`].
///
/// All of its subparsers should use [`last`].
///
/// # Errors
/// * Errors from [`address`]
/// * Errors from [`value32`]
/// * [`InvalidMoveSource`] (when no source matched)
fn move_source(input: Input) -> Res<MoveSource> {
    use MoveSource as Src;

    alt((
        last(tag("reg")).map(|_| Src::Reg),
        last(tag("ar")).map(|_| Src::Ar),
        last(tag("@ar")).map(|_| Src::AtAr),
        last(tag("fp")).map(|_| Src::Fp),
        last(tag("stk")).map(|_| Src::Stack),
        address.map(Src::Addr),
        value32.map(Src::Lit),
        fail_with(InvalidMoveSource),
    ))(input)
}

/// Parses the target of a move and returns it as a [`MoveTarget`].
///
/// All of its subparsers should use [`last`].
///
/// # Errors
/// * Errors from [`address`]
/// * Errors from [`value32`]
/// * [`InvalidMoveTarget`] (when no target matched)
fn move_target(input: Input) -> Res<MoveTarget> {
    use MoveTarget as Tar;

    alt((
        last(tag("reg")).map(|_| Tar::Reg),
        last(tag("ar")).map(|_| Tar::Ar),
        last(tag("@ar")).map(|_| Tar::AtAr),
        last(tag("stk")).map(|_| Tar::Stack),
        address.map(Tar::Addr),
        fail_with(InvalidMoveTarget),
    ))(input)
}

/// Parses the [source](MoveSource) and [target](MoveTarget) of a `mov` instruction and returns an [`Instruction`].
///
/// Uses [`last`].
///
/// # Errors
/// * Errors from [`address`]
/// * Errors from [`value32`]
/// * [`InvalidMoveSource`] (when no source matched)
/// * [`InvalidMoveTarget`] (when no target matched)
/// * [`UnsupportedMove`] (when there is no instruction for the specified source and target)
pub(crate) fn move_instruction(input: Input) -> Res<Instruction> {
    use MoveSource as Src;
    use MoveTarget as Tar;

    let (input, source) = move_source(input)?;
    let (input, target) = move_target(input)?;
    let instr = match (source, target) {
        (Src::Lit(val), Tar::Reg) => Instruction::MovLitReg(val),
        (Src::Lit(val), Tar::Ar) => Instruction::MovLitAr(val),
        (Src::Lit(val), Tar::Stack) => Instruction::PshLit(val),

        (Src::Addr(addr), Tar::Reg) => Instruction::MovMemReg(addr),
        (Src::Addr(addr), Tar::Ar) => Instruction::MovMemAr(addr),

        (Src::Reg, Tar::Addr(addr)) => Instruction::MovRegMem(addr),
        (Src::Reg, Tar::Ar) => Instruction::MovRegAr,
        (Src::Reg, Tar::AtAr) => Instruction::MovRegAtAr,
        (Src::Reg, Tar::Stack) => Instruction::PshReg,

        (Src::Ar, Tar::Reg) => Instruction::MovArReg,
        (Src::Ar, Tar::Stack) => Instruction::PshAr,

        (Src::AtAr, Tar::Reg) => Instruction::MovAtArReg,
        (Src::AtAr, Tar::Ar) => Instruction::MovAtArAr,

        (Src::Fp, Tar::Reg) => Instruction::MovFpReg,
        (Src::Fp, Tar::Ar) => Instruction::MovFpAr,
        (Src::Fp, Tar::Stack) => Instruction::PshFp,

        (Src::Stack, Tar::Reg) => Instruction::PulReg,
        (Src::Stack, Tar::Ar) => Instruction::PulAr,

        _ => return fail(input).map_error(UnsupportedMove),
    };
    Ok((input, instr))
}

/// Parses the [source](MoveSource) of a `psh` instruction and returns an [`Instruction`].
///
/// Uses [`last`].
///
/// # Errors
/// * Errors from [`address`]
/// * Errors from [`value32`]
/// * [`InvalidMoveSource`] (when no source matched)
/// * [`UnsupportedMove`] (when there is no instruction for the specified source and target)
pub(crate) fn push_instruction(input: Input) -> Res<Instruction> {
    use MoveSource as Src;

    let (input, source) = move_source(input)?;
    let instr = match source {
        Src::Lit(val) => Instruction::PshLit(val),
        Src::Reg => Instruction::PshReg,
        Src::Ar => Instruction::PshAr,
        Src::Fp => Instruction::PshFp,
        _ => return fail(input).map_error(UnsupportedMove),
    };
    Ok((input, instr))
}

/// Parses the [target](MoveTarget) of a `pul` instruction and returns an [`Instruction`].
///
/// Uses [`last`].
///
/// # Errors
/// * Errors from [`address`]
/// * Errors from [`value32`]
/// * [`InvalidMoveTarget`] (when no target matched)
/// * [`UnsupportedMove`] (when there is no instruction for the specified source and target)
pub(crate) fn pull_instruction(input: Input) -> Res<Instruction> {
    use MoveTarget as Tar;

    let (input, target) = move_target(input)?;
    let instr = match target {
        Tar::Reg => Instruction::PulReg,
        Tar::Ar => Instruction::PulAr,
        _ => return fail(input).map_error(UnsupportedMove),
    };
    Ok((input, instr))
}
