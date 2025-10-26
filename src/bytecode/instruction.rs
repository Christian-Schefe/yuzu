use crate::parser::{BinaryOp, FunctionParameters, Pattern, UnaryOp};

pub type CodePointer = usize;

#[derive(Debug, Clone)]
pub enum Instruction {
    Exit,
    DuplicateTopN(usize), // number of number or elements to duplicate
    Load(String),
    Store(String),
    StartInitializeLazy,
    InitializeLazy,
    Define(Pattern, bool),
    DefineLazy(String, CodePointer),
    InitModule,
    SetRootModule,
    LoadProperty(String),
    StoreProperty(String),
    LoadIndex,
    StoreIndex,
    Pop,
    PushNull,
    PushModule(CodePointer),
    PushBool(bool),
    PushInteger(String),
    PushNumber(f64),
    PushString(String),
    PushArray(Vec<bool>), // bool indicates if the element is a spread element
    PushObject(Vec<Option<String>>), // Option indicates if the value is a spread element
    Raise,
    PushFunction(FunctionData),
    PushClass {
        parent: bool,
        methods: Vec<(String, FunctionData)>,
        static_methods: Vec<(String, FunctionData)>,
        constructor: Option<FunctionData>,
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
    Await,
    Yield,
}

#[derive(Debug, Clone)]
pub struct FunctionData {
    pub parameters: FunctionParameters,
    pub body_pointer: CodePointer,
    pub is_async: bool,
}
