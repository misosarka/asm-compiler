use crate::address::Address::Literal as AddrLit;
use crate::error::{BuildErrorKind::*, ErrorKind::*};
use crate::instructions::{instruction, Instruction::*};
use crate::value::Value32::Literal as ValLit;

/// Asserts that the test file compiles successfully to the specified instructions.
///
/// The first parameter is the name of a text file (as a string literal, without `.txt`) located in `src/test/`
/// whose contents are the test code.
///
/// The second parameter is an array of [`Instruction`](crate::instructions::Instruction) values.
///
/// This macro uses the fact that [`Intermediate`](crate::intermediate::Intermediate) compares its instances by
/// [converting them into bytecode](crate::bytecode::to_bytecode) and comparing that, meaning the test will pass
/// even if the intermediates are not exactly the same but produce the identical bytecode.
macro_rules! assert_ok {
    ($file:literal, [ $( $item:expr ),* ] ) => {
        let text = include_str!(concat!("test/", $file, ".txt"));
        assert_eq!(
            $crate::statement::parse(text).and_then($crate::intermediate::to_intermediate),
            $crate::intermediate::Intermediate::from_instr_vec(vec![$($item, )*])
        )
    };
}

/// Asserts that the test file produces a specific compilation error.
///
/// The first parameter is the name of a text file (as a string literal, without `.txt`) located in `src/test/`
/// whose contents are the test code.
///
/// The second parameter is a [`BuildErrorKind`](crate::error::BuildErrorKind) which the code is supposed to produce.
///
/// The third parameter is either a location where the error should occur, or # followed by the expected remaining input.
macro_rules! assert_err {
    ($file:literal, $error:expr, #$remaining:expr) => {
        let text = include_str!(concat!("test/", $file, ".txt"));
        assert_eq!(
            $crate::compile(text),
            Err($crate::error::BuildError::new(
                $error,
                Some(text.len() - $remaining.len())
            ))
        )
    };
    ($file:literal, $error:expr, $location:expr) => {
        let text = include_str!(concat!("test/", $file, ".txt"));
        assert_eq!(
            $crate::compile(text),
            Err($crate::error::BuildError::new($error, $location))
        )
    };
}

#[test]
fn parse_inc() {
    assert_eq!(instruction("inc").unwrap().1, Inc);
}

#[test]
fn parse_jmp() {
    assert_eq!(instruction("jmp @453057").unwrap().1, Jmp(AddrLit(453057)));
}

#[test]
fn parse_invalid_code() {
    assert!(instruction("abc").is_err());
}

#[test]
fn basic_instr() {
    assert_ok!(
        "basic_instr",
        [
            Inc,
            Jmp(AddrLit(0x4aB53057)),
            Jfl(AddrLit(0x3273863f)),
            Dec,
            Dec
        ]
    );
}

#[test]
fn formatting() {
    assert_ok!(
        "formatting",
        [
            Inc,
            Jmp(AddrLit(16)),
            Dec,
            AddArReg,
            MovLitAr(ValLit(0o100))
        ]
    );
}

#[test]
fn multiple_on_one_line() {
    assert_ok!("multiple_on_one_line", [Inc, Jmp(AddrLit(16)), Dec]);
}

#[test]
fn instructions_with_values() {
    assert_ok!(
        "instructions_with_values",
        [
            Cal(AddrLit(255)),
            Ret,
            Jma,
            PshLit(ValLit(0x2367327a)),
            MovLitAr(ValLit(0xabcd1234)),
            Not,
            MovLitReg(ValLit(0))
        ]
    );
}

#[test]
fn moves() {
    assert_ok!(
        "moves",
        [
            MovLitReg(ValLit(0xabcd1234)),
            MovLitAr(ValLit(0x1234abcd)),
            PshLit(ValLit(0xabcdabcd)),
            MovMemReg(AddrLit(0xabcd1234)),
            MovMemAr(AddrLit(0x1234abcd)),
            MovRegMem(AddrLit(0x12341234)),
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
            PulAr
        ]
    );
}

#[test]
fn moves_shorthand() {
    assert_ok!(
        "moves_shorthand",
        [
            MovLitReg(ValLit(0xabcd1234)),
            MovLitAr(ValLit(0x1234abcd)),
            PshLit(ValLit(0xabcdabcd)),
            MovMemReg(AddrLit(0xabcd1234)),
            MovMemAr(AddrLit(0x1234abcd)),
            MovRegMem(AddrLit(0x12341234)),
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
            PulAr
        ]
    );
}

#[test]
fn push_pull() {
    assert_ok!(
        "push_pull",
        [
            PshLit(ValLit(0x098765ef)),
            PshReg,
            PshAr,
            PshFp,
            PulReg,
            PulAr
        ]
    );
}

#[test]
fn ops() {
    assert_ok!(
        "ops",
        [
            AddRegLit(ValLit(0x12345678)),
            SubRegLit(ValLit(0xabcdabcd)),
            MulRegLit(ValLit(0x12345678)),
            AndRegLit(ValLit(0xabcdabcd)),
            OrbRegLit(ValLit(0x12345678)),
            XorRegLit(ValLit(0xabcdabcd)),
            ShlRegLit(ValLit(0x12345678)),
            ShrRegLit(ValLit(0xabcdabcd)),
            AddRegMem(AddrLit(0x12345678)),
            SubRegMem(AddrLit(0xabcdabcd)),
            MulRegMem(AddrLit(0x12345678)),
            AndRegMem(AddrLit(0xabcdabcd)),
            OrbRegMem(AddrLit(0x12345678)),
            XorRegMem(AddrLit(0xabcdabcd)),
            ShlRegMem(AddrLit(0x12345678)),
            ShrRegMem(AddrLit(0xabcdabcd)),
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
            AddArLit(ValLit(0x12345678)),
            SubArLit(ValLit(0xabcdabcd)),
            MulArLit(ValLit(0x12345678)),
            AndArLit(ValLit(0xabcdabcd)),
            OrbArLit(ValLit(0x12345678)),
            XorArLit(ValLit(0xabcdabcd)),
            ShlArLit(ValLit(0x12345678)),
            ShrArLit(ValLit(0xabcdabcd)),
            AddArMem(AddrLit(0x12345678)),
            SubArMem(AddrLit(0xabcdabcd)),
            MulArMem(AddrLit(0x12345678)),
            AndArMem(AddrLit(0xabcdabcd)),
            OrbArMem(AddrLit(0x12345678)),
            XorArMem(AddrLit(0xabcdabcd)),
            ShlArMem(AddrLit(0x12345678)),
            ShrArMem(AddrLit(0xabcdabcd)),
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
            ShrArPul
        ]
    );
}

#[test]
fn comments() {
    assert_ok!(
        "comments",
        [
            MovLitReg(ValLit(0)),
            MovRegMem(AddrLit(1)),
            AddRegPul,
            MovAtArReg
        ]
    );
}

#[test]
fn labels() {
    assert_ok!(
        "labels",
        [
            MovLitReg(ValLit(0)),
            Jmp(AddrLit(5)),
            Jnf(AddrLit(10)),
            Cal(AddrLit(5))
        ]
    );
}

#[test]
fn invalid_address_literal() {
    assert_err!("invalid_address_literal", Syntax(InvalidAddressLiteral), #"Q\r\ninc");
}

#[test]
fn invalid_label_character() {
    assert_err!("invalid_label_character", Syntax(InvalidIdentifierCharacter), #"%\r\ninc");
}

#[test]
fn io() {
    assert_ok!("io", [Ipa, Ipb, Ipc, Out, Slp, Rnd, Utc, Tim]);
}

#[test]
fn nonexistent_label() {
    assert_err!("nonexistent_label", NonexistentLabel, None);
}

#[test]
fn duplicated_label() {
    assert_err!("duplicated_label", DuplicatedLabel, None);
}
