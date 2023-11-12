use crate::address::Address;
use crate::error::{BuildError, BuildErrorKind::NonexistentLabel};
use crate::instructions::Instruction::{self, *};
use crate::intermediate::Intermediate;
use crate::value::Value32;

/// Representation of bytecode for a single instruction.
enum Bytecode {
    Single(u8),
    Param(u8, [u8; 4]),
}

/// Converts from the [intermediate representation](Intermediate) to the bytecode.
///
/// The produced bytecode has the form of an owned byte slice.
pub(crate) fn to_bytecode(mut input: Intermediate) -> Result<Box<[u8]>, BuildError> {
    let mut result = vec![];
    input.instructions.push(End);
    for instr in &input.instructions {
        match instruction_bytecode(instr, &input)? {
            Bytecode::Single(val) => result.push(val),
            Bytecode::Param(val, arr) => {
                result.push(val);
                result.extend(arr);
            }
        }
    }
    Ok(result.into_boxed_slice())
}

/// Converts a [`Value32`] to [`Bytecode`].
///
/// This operation is infallible.
fn val_to_bytecode(val: &Value32, _inter: &Intermediate) -> [u8; 4] {
    let Value32::Literal(val) = val;
    val.to_be_bytes()
}

/// Converts an [`Address`] to [`Bytecode`].
///
/// # Errors
/// [`NonexistentLabel`] (when referring to a label that is not defined)
fn addr_to_bytecode(addr: &Address, inter: &Intermediate) -> Result<[u8; 4], BuildError> {
    let addr = match addr {
        Address::Literal(a) => a,
        Address::Label(l) => inter
            .labels
            .get(l)
            .ok_or(BuildError::new(NonexistentLabel, None))?,
    };
    Ok(addr.to_be_bytes())
}

/// Converts an [`Instruction`] to [`Bytecode`].
///
/// # Errors
/// [`NonexistentLabel`] (from [`addr_to_bytecode`])
fn instruction_bytecode(instr: &Instruction, inter: &Intermediate) -> Result<Bytecode, BuildError> {
    use Bytecode::*;
    Ok(match instr {
        End => Single(0x00),
        MovLitReg(val) => Param(0x10, val_to_bytecode(val, inter)),
        MovLitAr(val) => Param(0x11, val_to_bytecode(val, inter)),
        PshLit(val) => Param(0x12, val_to_bytecode(val, inter)),
        MovMemReg(addr) => Param(0x20, addr_to_bytecode(addr, inter)?),
        MovMemAr(addr) => Param(0x21, addr_to_bytecode(addr, inter)?),
        MovRegMem(addr) => Param(0x30, addr_to_bytecode(addr, inter)?),
        MovRegAr => Single(0x31),
        MovRegAtAr => Single(0x32),
        PshReg => Single(0x33),
        MovArReg => Single(0x40),
        PshAr => Single(0x41),
        MovAtArReg => Single(0x50),
        MovAtArAr => Single(0x51),
        MovFpReg => Single(0x60),
        MovFpAr => Single(0x61),
        PshFp => Single(0x62),
        PulReg => Single(0x70),
        PulAr => Single(0x71),
        AddRegLit(val) => Param(0x80, val_to_bytecode(val, inter)),
        SubRegLit(val) => Param(0x81, val_to_bytecode(val, inter)),
        MulRegLit(val) => Param(0x82, val_to_bytecode(val, inter)),
        AndRegLit(val) => Param(0x83, val_to_bytecode(val, inter)),
        OrbRegLit(val) => Param(0x84, val_to_bytecode(val, inter)),
        XorRegLit(val) => Param(0x85, val_to_bytecode(val, inter)),
        ShlRegLit(val) => Param(0x86, val_to_bytecode(val, inter)),
        ShrRegLit(val) => Param(0x87, val_to_bytecode(val, inter)),
        AddRegMem(addr) => Param(0x88, addr_to_bytecode(addr, inter)?),
        SubRegMem(addr) => Param(0x89, addr_to_bytecode(addr, inter)?),
        MulRegMem(addr) => Param(0x8a, addr_to_bytecode(addr, inter)?),
        AndRegMem(addr) => Param(0x8b, addr_to_bytecode(addr, inter)?),
        OrbRegMem(addr) => Param(0x8c, addr_to_bytecode(addr, inter)?),
        XorRegMem(addr) => Param(0x8d, addr_to_bytecode(addr, inter)?),
        ShlRegMem(addr) => Param(0x8e, addr_to_bytecode(addr, inter)?),
        ShrRegMem(addr) => Param(0x8f, addr_to_bytecode(addr, inter)?),
        AddRegAtAr => Single(0x90),
        SubRegAtAr => Single(0x91),
        MulRegAtAr => Single(0x92),
        AndRegAtAr => Single(0x93),
        OrbRegAtAr => Single(0x94),
        XorRegAtAr => Single(0x95),
        ShlRegAtAr => Single(0x96),
        ShrRegAtAr => Single(0x97),
        AddRegPul => Single(0x98),
        SubRegPul => Single(0x99),
        MulRegPul => Single(0x9a),
        AndRegPul => Single(0x9b),
        OrbRegPul => Single(0x9c),
        XorRegPul => Single(0x9d),
        ShlRegPul => Single(0x9e),
        ShrRegPul => Single(0x9f),
        AddArLit(val) => Param(0xa0, val_to_bytecode(val, inter)),
        SubArLit(val) => Param(0xa1, val_to_bytecode(val, inter)),
        MulArLit(val) => Param(0xa2, val_to_bytecode(val, inter)),
        AndArLit(val) => Param(0xa3, val_to_bytecode(val, inter)),
        OrbArLit(val) => Param(0xa4, val_to_bytecode(val, inter)),
        XorArLit(val) => Param(0xa5, val_to_bytecode(val, inter)),
        ShlArLit(val) => Param(0xa6, val_to_bytecode(val, inter)),
        ShrArLit(val) => Param(0xa7, val_to_bytecode(val, inter)),
        AddArMem(addr) => Param(0xa8, addr_to_bytecode(addr, inter)?),
        SubArMem(addr) => Param(0xa9, addr_to_bytecode(addr, inter)?),
        MulArMem(addr) => Param(0xaa, addr_to_bytecode(addr, inter)?),
        AndArMem(addr) => Param(0xab, addr_to_bytecode(addr, inter)?),
        OrbArMem(addr) => Param(0xac, addr_to_bytecode(addr, inter)?),
        XorArMem(addr) => Param(0xad, addr_to_bytecode(addr, inter)?),
        ShlArMem(addr) => Param(0xae, addr_to_bytecode(addr, inter)?),
        ShrArMem(addr) => Param(0xaf, addr_to_bytecode(addr, inter)?),
        AddArReg => Single(0xb0),
        SubArReg => Single(0xb1),
        MulArReg => Single(0xb2),
        AndArReg => Single(0xb3),
        OrbArReg => Single(0xb4),
        XorArReg => Single(0xb5),
        ShlArReg => Single(0xb6),
        ShrArReg => Single(0xb7),
        AddArPul => Single(0xb8),
        SubArPul => Single(0xb9),
        MulArPul => Single(0xba),
        AndArPul => Single(0xbb),
        OrbArPul => Single(0xbc),
        XorArPul => Single(0xbd),
        ShlArPul => Single(0xbe),
        ShrArPul => Single(0xbf),
        Inc => Single(0xc0),
        Dec => Single(0xc1),
        Not => Single(0xc2),
        Jmp(addr) => Param(0xd0, addr_to_bytecode(addr, inter)?),
        Jfl(addr) => Param(0xd1, addr_to_bytecode(addr, inter)?),
        Jnf(addr) => Param(0xd2, addr_to_bytecode(addr, inter)?),
        Jze(addr) => Param(0xd3, addr_to_bytecode(addr, inter)?),
        Jnz(addr) => Param(0xd4, addr_to_bytecode(addr, inter)?),
        Jma => Single(0xd5),
        Cal(addr) => Param(0xe0, addr_to_bytecode(addr, inter)?),
        Ret => Single(0xe1),
        Ipa => Single(0xf0),
        Ipb => Single(0xf1),
        Ipc => Single(0xf2),
        Out => Single(0xf3),
        Slp => Single(0xf4),
        Rnd => Single(0xf5),
        Utc => Single(0xf6),
        Tim => Single(0xf7),
    })
}
