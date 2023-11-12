use crate::address::{address, Address};
use crate::identifier::last;
use crate::moves::{move_instruction, pull_instruction, push_instruction};
use crate::ops::{operation, OpSource, Operation};
use crate::value::{value32, Value32};
use crate::{Input, Res};

use nom::{
    character::complete::alpha1,
    combinator::{cut, fail},
    Parser,
};

/// Representation of a single instruction.
///
/// Some instructions contain a parameter (either [`Value32`] or [`Address`]).
#[derive(PartialEq, Debug, Clone)]
pub(crate) enum Instruction {
    End,

    MovLitReg(Value32),
    MovLitAr(Value32),
    PshLit(Value32),

    MovMemReg(Address),
    MovMemAr(Address),

    MovRegMem(Address),
    MovRegAr,
    MovRegAtAr,
    PshReg,

    MovArReg,
    PshAr,

    MovAtArReg,
    MovAtArAr,

    MovFpReg,
    MovFpAr,
    PshFp,

    PulReg,
    PulAr,

    AddRegLit(Value32),
    SubRegLit(Value32),
    MulRegLit(Value32),
    AndRegLit(Value32),
    OrbRegLit(Value32),
    XorRegLit(Value32),
    ShlRegLit(Value32),
    ShrRegLit(Value32),

    AddRegMem(Address),
    SubRegMem(Address),
    MulRegMem(Address),
    AndRegMem(Address),
    OrbRegMem(Address),
    XorRegMem(Address),
    ShlRegMem(Address),
    ShrRegMem(Address),

    AddRegAtAr,
    SubRegAtAr,
    MulRegAtAr,
    AndRegAtAr,
    OrbRegAtAr,
    XorRegAtAr,
    ShlRegAtAr,
    ShrRegAtAr,

    AddRegPul,
    SubRegPul,
    MulRegPul,
    AndRegPul,
    OrbRegPul,
    XorRegPul,
    ShlRegPul,
    ShrRegPul,

    AddArLit(Value32),
    SubArLit(Value32),
    MulArLit(Value32),
    AndArLit(Value32),
    OrbArLit(Value32),
    XorArLit(Value32),
    ShlArLit(Value32),
    ShrArLit(Value32),

    AddArMem(Address),
    SubArMem(Address),
    MulArMem(Address),
    AndArMem(Address),
    OrbArMem(Address),
    XorArMem(Address),
    ShlArMem(Address),
    ShrArMem(Address),

    AddArReg,
    SubArReg,
    MulArReg,
    AndArReg,
    OrbArReg,
    XorArReg,
    ShlArReg,
    ShrArReg,

    AddArPul,
    SubArPul,
    MulArPul,
    AndArPul,
    OrbArPul,
    XorArPul,
    ShlArPul,
    ShrArPul,

    Inc,
    Dec,
    Not,

    Jmp(Address),
    Jfl(Address),
    Jnf(Address),
    Jze(Address),
    Jnz(Address),
    Jma,

    Cal(Address),
    Ret,

    Ipa,
    Ipb,
    Ipc,
    Out,
    Slp,
    Rnd,
    Utc,
    Tim,
}

impl Instruction {
    /// Returns the number of bytes needed to encode this instruction.
    ///
    /// This is either 1 for instructions without parameters or 5 for parametrized ones.
    pub fn byte_size(&self) -> u32 {
        use Instruction::*;
        match self {
            End | MovRegAr | MovRegAtAr | PshReg | MovArReg | PshAr | MovAtArReg | MovAtArAr
            | MovFpReg | MovFpAr | PshFp | PulReg | PulAr | AddRegAtAr | SubRegAtAr
            | MulRegAtAr | AndRegAtAr | OrbRegAtAr | XorRegAtAr | ShlRegAtAr | ShrRegAtAr
            | AddRegPul | SubRegPul | MulRegPul | AndRegPul | OrbRegPul | XorRegPul | ShlRegPul
            | ShrRegPul | AddArReg | SubArReg | MulArReg | AndArReg | OrbArReg | XorArReg
            | ShlArReg | ShrArReg | AddArPul | SubArPul | MulArPul | AndArPul | OrbArPul
            | XorArPul | ShlArPul | ShrArPul | Inc | Dec | Not | Jma | Ret | Ipa | Ipb | Ipc
            | Out | Slp | Rnd | Utc | Tim => 1,
            MovLitReg(_) | MovLitAr(_) | PshLit(_) | MovMemReg(_) | MovMemAr(_) | MovRegMem(_)
            | AddRegLit(_) | SubRegLit(_) | MulRegLit(_) | AndRegLit(_) | OrbRegLit(_)
            | XorRegLit(_) | ShlRegLit(_) | ShrRegLit(_) | AddRegMem(_) | SubRegMem(_)
            | MulRegMem(_) | AndRegMem(_) | OrbRegMem(_) | XorRegMem(_) | ShlRegMem(_)
            | ShrRegMem(_) | AddArLit(_) | SubArLit(_) | MulArLit(_) | AndArLit(_)
            | OrbArLit(_) | XorArLit(_) | ShlArLit(_) | ShrArLit(_) | AddArMem(_) | SubArMem(_)
            | MulArMem(_) | AndArMem(_) | OrbArMem(_) | XorArMem(_) | ShlArMem(_) | ShrArMem(_)
            | Jmp(_) | Jfl(_) | Jnf(_) | Jze(_) | Jnz(_) | Cal(_) => 5,
        }
    }
}

/// Parses an instruction code (a single word).
///
/// Uses [`last`].
///
/// Returns a [`Nom`](crate::error::ErrorKind::Nom) error.
fn instruction_code(input: Input) -> Res<Input> {
    last(alpha1)(input)
}

/// Parses a complete instruction and returns it as an [`Instruction`].
///
/// All of its subparsers should use [`last`].
///
/// # Errors
/// * Errors from [`address`]
/// * Errors from [`value32`]
/// * Errors from [`operation`]
/// * Errors from [`move_instruction`], [`push_instruction`] and [`pull_instruction`]
/// * A [`Nom`](crate::error::ErrorKind::Nom) error (when an instruction code is invalid)
pub(crate) fn instruction(input: Input) -> Res<Instruction> {
    use Instruction::*;
    let (input, instr) = instruction_code(input)?;
    let (input, instr) = match instr {
        "end" => (input, End),

        "mov" => cut(move_instruction)(input)?,
        "psh" => cut(push_instruction)(input)?,
        "pul" => cut(pull_instruction)(input)?,

        "val" => cut(value32).map(MovLitReg).parse(input)?,
        "adr" => cut(value32).map(MovLitAr).parse(input)?,
        "psv" => cut(value32).map(PshLit).parse(input)?,

        "get" => cut(address).map(MovMemReg).parse(input)?,
        "adf" => cut(address).map(MovMemAr).parse(input)?,

        "set" => cut(address).map(MovRegMem).parse(input)?,
        "tra" => (input, MovRegAr),
        "sea" => (input, MovRegAtAr),
        "psr" => (input, PshReg),

        "tar" => (input, MovArReg),
        "psa" => (input, PshAr),

        "gea" => (input, MovAtArReg),
        "ada" => (input, MovAtArAr),

        "tfr" => (input, MovFpReg),
        "tfa" => (input, MovFpAr),
        "psf" => (input, PshFp),

        "plr" => (input, PulReg),
        "pla" => (input, PulAr),

        "add" => operation(Operation::Add, OpSource::Reg, input)?,
        "sub" => operation(Operation::Sub, OpSource::Reg, input)?,
        "mul" => operation(Operation::Mul, OpSource::Reg, input)?,
        "and" => operation(Operation::And, OpSource::Reg, input)?,
        "orb" => operation(Operation::Orb, OpSource::Reg, input)?,
        "xor" => operation(Operation::Xor, OpSource::Reg, input)?,
        "shl" => operation(Operation::Shl, OpSource::Reg, input)?,
        "shr" => operation(Operation::Shr, OpSource::Reg, input)?,

        "aad" => operation(Operation::Add, OpSource::Ar, input)?,
        "asb" => operation(Operation::Sub, OpSource::Ar, input)?,
        "aml" => operation(Operation::Mul, OpSource::Ar, input)?,
        "aan" => operation(Operation::And, OpSource::Ar, input)?,
        "aor" => operation(Operation::Orb, OpSource::Ar, input)?,
        "axr" => operation(Operation::Xor, OpSource::Ar, input)?,
        "asl" => operation(Operation::Shl, OpSource::Ar, input)?,
        "asr" => operation(Operation::Shr, OpSource::Ar, input)?,

        "inc" => (input, Inc),
        "dec" => (input, Dec),
        "not" => (input, Not),

        "jmp" => cut(address).map(Jmp).parse(input)?,
        "jfl" => cut(address).map(Jfl).parse(input)?,
        "jnf" => cut(address).map(Jnf).parse(input)?,
        "jze" => cut(address).map(Jze).parse(input)?,
        "jnz" => cut(address).map(Jnz).parse(input)?,
        "jma" => (input, Jma),

        "cal" => cut(address).map(Cal).parse(input)?,
        "ret" => (input, Ret),

        "ipa" => (input, Ipa),
        "ipb" => (input, Ipb),
        "ipc" => (input, Ipc),
        "out" => (input, Out),
        "slp" => (input, Slp),
        "rnd" => (input, Rnd),
        "utc" => (input, Utc),
        "tim" => (input, Tim),

        _ => return fail(input),
    };
    Ok((input, instr))
}
