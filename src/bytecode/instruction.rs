use crate::{
    CanonicalPath,
    parser::{BinaryOp, Identifier, Pattern, UnaryOp},
};

pub type CodePointer = usize;

#[derive(Debug, Clone)]
pub enum Instruction {
    Exit,
    Load(Identifier),
    Store(Identifier),
    InitializeLazyEnd,
    Define(Pattern),
    DefineCanonic(CanonicalPath),
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
    PushLazy(CodePointer),
    Raise,
    PushFunction {
        parameters: Vec<String>,
        body_pointer: CodePointer,
    },
    PushClass {
        parent: bool,
        methods: Vec<(String, Vec<String>, CodePointer)>,
        static_methods: Vec<(String, Vec<String>, CodePointer)>,
        constructor: Option<(Vec<String>, CodePointer)>,
    },
    Break,
    Continue,
    Return,
    Jump(CodePointer),
    JumpIfFalse(CodePointer),
    EnterBlock,
    EnterLoop {
        break_target: CodePointer,
        continue_target: CodePointer,
    },
    EnterTryCatch {
        catch_target: CodePointer,
    },
    ExitFrame,
    CallFunction(usize),                    // number of arguments
    CallPropertyFunction(String, usize),    // number of arguments
    TryShortCircuit(BinaryOp, CodePointer), // jump target
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    CallConstructor(usize),    // number of arguments
    MakeInstance(Vec<String>), // list of property names
    CheckCatch(CodePointer),   // jump target
}
