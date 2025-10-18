use crate::{
    CanonicalPath, ModulePath,
    parser::{BinaryOp, FunctionParameters, Identifier, Pattern, UnaryOp},
};

pub type CodePointer = usize;

#[derive(Debug, Clone)]
pub enum Instruction {
    Exit,
    DuplicateTopN(usize), // number of number or elements to duplicate
    Load(Identifier),
    Store(Identifier),
    StartInitializeLazy(CanonicalPath),
    InitializeLazy(CanonicalPath),
    InitializeModule(ModulePath),
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
        parameters: FunctionParameters,
        body_pointer: CodePointer,
    },
    PushClass {
        parent: bool,
        methods: Vec<(String, FunctionParameters, CodePointer)>,
        static_methods: Vec<(String, FunctionParameters, CodePointer)>,
        constructor: Option<(FunctionParameters, CodePointer)>,
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
    CallFunction(Vec<bool>), // bool indicates if the element is a spread element
    TryShortCircuit(BinaryOp, CodePointer), // jump target
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    CallConstructor(usize), // number of arguments
    MakeInstance,
}
