use std::collections::HashMap;

use crate::bytecode::to_bytecode;
use crate::error::{BuildError, BuildErrorKind::DuplicatedLabel};
use crate::instructions::Instruction;
use crate::statement::Statement;

/// An intermediate representation of the code.
///
/// Contains a [`Vec`] of [`Instruction`]s and a [`HashMap`] of labels and their respective addresses.
///
/// The [`PartialEq`] implementation compares two instances of `Intermediate`
/// by [converting them into bytecode](to_bytecode) and comparing that.
#[derive(Debug, Clone)]
pub(crate) struct Intermediate {
    pub instructions: Vec<Instruction>,
    pub labels: HashMap<String, u32>,
}

impl Intermediate {
    #[cfg(test)]
    pub fn from_instr_vec(instructions: Vec<Instruction>) -> Result<Self, BuildError> {
        Ok(Self {
            instructions,
            labels: HashMap::new(),
        })
    }
}

impl PartialEq<Intermediate> for Intermediate {
    fn eq(&self, other: &Intermediate) -> bool {
        to_bytecode(self.clone()) == to_bytecode(other.clone())
    }
}

/// Converts from a [`Vec`] of [`Statement`]s to an [intermediate representation](Intermediate).
///
/// This includes assigning labels their address.
pub(crate) fn to_intermediate(code: Vec<Statement>) -> Result<Intermediate, BuildError> {
    let mut instructions = vec![];
    let mut current_addr = 0u32;
    let mut labels = HashMap::new();
    for stmt in code {
        match stmt {
            Statement::Instr(i) => {
                current_addr += i.byte_size();
                instructions.push(i);
            }
            Statement::Label(l) => {
                if labels.insert(l, current_addr).is_some() {
                    return Err(BuildError::new(DuplicatedLabel, None));
                }
            }
        }
    }
    Ok(Intermediate {
        instructions,
        labels,
    })
}
