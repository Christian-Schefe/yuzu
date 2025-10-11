use crate::{
    CanonicalPath, ModulePath,
    parser::{BinaryOp, Identifier, Pattern, UnaryOp},
};

pub type CodePointer = usize;

#[derive(Debug, Clone)]
pub enum Instruction {
    Exit,
    DuplicateTopN(usize), // number of number or elements to duplicate
    Load(Identifier),
    Store(Identifier),
    InitializeLazy(CanonicalPath),
    Define(Pattern),
    DefineCanonic(CanonicalPath, CodePointer),
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
        body_pointer: CodePointer,
    },
    PushClass {
        parent: bool,
        methods: Vec<(String, Vec<String>, CodePointer)>,
        static_methods: Vec<(String, Vec<String>, CodePointer)>,
        constructor: Option<(Vec<String>, (CodePointer, CodePointer))>,
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
        filtered: bool,
    },
    ExitFrame,
    CallFunction(usize),                    // number of arguments
    CallPropertyFunction(String, usize),    // number of arguments
    TryShortCircuit(BinaryOp, CodePointer), // jump target
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    CallConstructor(usize),           // number of arguments
    MakeInstance(usize, Vec<String>), // number of constructor args, list of property names
    EnterModule(ModulePath),
}
