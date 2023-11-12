use crate::address::{address, Address};
use crate::error::{fail_with, ErrorKind::InvalidOperationTarget};
use crate::identifier::last;
use crate::instructions::Instruction;
use crate::value::{value32, Value32};
use crate::{Input, Res};

use nom::{branch::alt, bytes::complete::tag, Parser};

/// A primitive operation.
pub(crate) enum Operation {
    Add,
    Sub,
    Mul,
    And,
    Orb,
    Xor,
    Shl,
    Shr,
}

/// A source of an operation (location of the left value and the result).
/// Either the register or the address register.
pub(crate) enum OpSource {
    Reg,
    Ar,
}

/// A target of an operation (location of the right value) whose [source](OpSource) is the register.
enum RegOpTarget {
    Lit(Value32),
    Mem(Address),
    Aar,
    Stk,
}

/// A target of an operation (location of the right value) whose [source](OpSource) is the address register.
enum ArOpTarget {
    Lit(Value32),
    Mem(Address),
    Reg,
    Stk,
}

/// Complete information about the source and target of an operation.
enum OpData {
    Reg(RegOpTarget),
    Ar(ArOpTarget),
}

/// Parses the target of an operation and returns an [`Instruction`].
///
/// Uses [`last`].
///
/// # Errors
/// * Errors from [`address`]
/// * Errors from [`value32`]
/// * [`InvalidOperationTarget`] (when no target matched)
pub(crate) fn operation(op: Operation, source: OpSource, input: Input) -> Res<Instruction> {
    use ArOpTarget as AT;
    use OpData::*;
    use Operation::*;
    use RegOpTarget as RT;

    let (input, data) = op_data(source, input)?;

    let instr = match (op, data) {
        (Add, Reg(RT::Lit(val))) => Instruction::AddRegLit(val),
        (Sub, Reg(RT::Lit(val))) => Instruction::SubRegLit(val),
        (Mul, Reg(RT::Lit(val))) => Instruction::MulRegLit(val),
        (And, Reg(RT::Lit(val))) => Instruction::AndRegLit(val),
        (Orb, Reg(RT::Lit(val))) => Instruction::OrbRegLit(val),
        (Xor, Reg(RT::Lit(val))) => Instruction::XorRegLit(val),
        (Shl, Reg(RT::Lit(val))) => Instruction::ShlRegLit(val),
        (Shr, Reg(RT::Lit(val))) => Instruction::ShrRegLit(val),

        (Add, Reg(RT::Mem(addr))) => Instruction::AddRegMem(addr),
        (Sub, Reg(RT::Mem(addr))) => Instruction::SubRegMem(addr),
        (Mul, Reg(RT::Mem(addr))) => Instruction::MulRegMem(addr),
        (And, Reg(RT::Mem(addr))) => Instruction::AndRegMem(addr),
        (Orb, Reg(RT::Mem(addr))) => Instruction::OrbRegMem(addr),
        (Xor, Reg(RT::Mem(addr))) => Instruction::XorRegMem(addr),
        (Shl, Reg(RT::Mem(addr))) => Instruction::ShlRegMem(addr),
        (Shr, Reg(RT::Mem(addr))) => Instruction::ShrRegMem(addr),

        (Add, Reg(RT::Aar)) => Instruction::AddRegAtAr,
        (Sub, Reg(RT::Aar)) => Instruction::SubRegAtAr,
        (Mul, Reg(RT::Aar)) => Instruction::MulRegAtAr,
        (And, Reg(RT::Aar)) => Instruction::AndRegAtAr,
        (Orb, Reg(RT::Aar)) => Instruction::OrbRegAtAr,
        (Xor, Reg(RT::Aar)) => Instruction::XorRegAtAr,
        (Shl, Reg(RT::Aar)) => Instruction::ShlRegAtAr,
        (Shr, Reg(RT::Aar)) => Instruction::ShrRegAtAr,

        (Add, Reg(RT::Stk)) => Instruction::AddRegPul,
        (Sub, Reg(RT::Stk)) => Instruction::SubRegPul,
        (Mul, Reg(RT::Stk)) => Instruction::MulRegPul,
        (And, Reg(RT::Stk)) => Instruction::AndRegPul,
        (Orb, Reg(RT::Stk)) => Instruction::OrbRegPul,
        (Xor, Reg(RT::Stk)) => Instruction::XorRegPul,
        (Shl, Reg(RT::Stk)) => Instruction::ShlRegPul,
        (Shr, Reg(RT::Stk)) => Instruction::ShrRegPul,

        (Add, Ar(AT::Lit(val))) => Instruction::AddArLit(val),
        (Sub, Ar(AT::Lit(val))) => Instruction::SubArLit(val),
        (Mul, Ar(AT::Lit(val))) => Instruction::MulArLit(val),
        (And, Ar(AT::Lit(val))) => Instruction::AndArLit(val),
        (Orb, Ar(AT::Lit(val))) => Instruction::OrbArLit(val),
        (Xor, Ar(AT::Lit(val))) => Instruction::XorArLit(val),
        (Shl, Ar(AT::Lit(val))) => Instruction::ShlArLit(val),
        (Shr, Ar(AT::Lit(val))) => Instruction::ShrArLit(val),

        (Add, Ar(AT::Mem(addr))) => Instruction::AddArMem(addr),
        (Sub, Ar(AT::Mem(addr))) => Instruction::SubArMem(addr),
        (Mul, Ar(AT::Mem(addr))) => Instruction::MulArMem(addr),
        (And, Ar(AT::Mem(addr))) => Instruction::AndArMem(addr),
        (Orb, Ar(AT::Mem(addr))) => Instruction::OrbArMem(addr),
        (Xor, Ar(AT::Mem(addr))) => Instruction::XorArMem(addr),
        (Shl, Ar(AT::Mem(addr))) => Instruction::ShlArMem(addr),
        (Shr, Ar(AT::Mem(addr))) => Instruction::ShrArMem(addr),

        (Add, Ar(AT::Reg)) => Instruction::AddArReg,
        (Sub, Ar(AT::Reg)) => Instruction::SubArReg,
        (Mul, Ar(AT::Reg)) => Instruction::MulArReg,
        (And, Ar(AT::Reg)) => Instruction::AndArReg,
        (Orb, Ar(AT::Reg)) => Instruction::OrbArReg,
        (Xor, Ar(AT::Reg)) => Instruction::XorArReg,
        (Shl, Ar(AT::Reg)) => Instruction::ShlArReg,
        (Shr, Ar(AT::Reg)) => Instruction::ShrArReg,

        (Add, Ar(AT::Stk)) => Instruction::AddArPul,
        (Sub, Ar(AT::Stk)) => Instruction::SubArPul,
        (Mul, Ar(AT::Stk)) => Instruction::MulArPul,
        (And, Ar(AT::Stk)) => Instruction::AndArPul,
        (Orb, Ar(AT::Stk)) => Instruction::OrbArPul,
        (Xor, Ar(AT::Stk)) => Instruction::XorArPul,
        (Shl, Ar(AT::Stk)) => Instruction::ShlArPul,
        (Shr, Ar(AT::Stk)) => Instruction::ShrArPul,
    };

    Ok((input, instr))
}

/// Parses the target of an operation and returns it inside [`OpData`].
///
/// All of its subparsers should use [`last`].
///
/// # Errors
/// * Errors from [`address`]
/// * Errors from [`value32`]
/// * [`InvalidOperationTarget`] (when no target matched)
fn op_data(source: OpSource, input: Input) -> Res<OpData> {
    use ArOpTarget as AT;
    use RegOpTarget as RT;
    match source {
        OpSource::Reg => alt((
            last(tag("@ar")).map(|_| OpData::Reg(RT::Aar)),
            last(tag("stk")).map(|_| OpData::Reg(RT::Stk)),
            address.map(|addr| OpData::Reg(RT::Mem(addr))),
            value32.map(|val| OpData::Reg(RT::Lit(val))),
            fail_with(InvalidOperationTarget),
        ))(input),
        OpSource::Ar => alt((
            last(tag("reg")).map(|_| OpData::Ar(AT::Reg)),
            last(tag("stk")).map(|_| OpData::Ar(AT::Stk)),
            address.map(|addr| OpData::Ar(AT::Mem(addr))),
            value32.map(|val| OpData::Ar(AT::Lit(val))),
            fail_with(InvalidOperationTarget),
        ))(input),
    }
}
