use crate::parser::{BinaryOp, ClassMemberKind, Identifier, Pattern, UnaryOp};

#[derive(Debug, Clone)]
pub enum Instruction {
    Load(Identifier),
    Store(Identifier),
    Define(Pattern, bool), // bool indicates if it's a static define
    LoadProperty(String),
    StoreProperty(String),
    LoadIndex,
    StoreIndex,
    Pop,
    PushNull,
    PushBool(bool),
    PushInteger(String),
    PushNumber(f64),
    PushString(String),
    PushArray(Vec<bool>), // bool indicates if the element is a spread element
    PushObject(Vec<Option<String>>), // Option indicates if the value is a spread element
    Raise,
    PushFunction {
        parameters: Vec<String>,
        body_pointer: usize,
    },
    PushClass {
        parent: Option<Identifier>,
        initializer_pointer: usize,
        properties: Vec<(String, ClassMemberKind)>,
        constructor_pointer: Option<usize>,
    },
    Break,
    Continue,
    Return,
    Jump(usize),
    JumpIfFalse(usize),
    EnterBlock,
    EnterLoop {
        break_target: usize,
        continue_target: usize,
    },
    EnterTryCatch {
        catch_target: usize,
    },
    ExitFrame,
    CallFunction(usize),                 // number of arguments
    CallPropertyFunction(String, usize), // number of arguments
    TryShortCircuit(BinaryOp, usize),    // jump target
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    MakeInstance(Vec<String>), // field names
    CallInitializer,
    CheckCatch(usize), // jump target
}
