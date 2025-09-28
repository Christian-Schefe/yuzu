use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::tree_interpreter::{Environment, LocatedControlFlow, LocatedExpression, Location};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Integer(i64),
    Bool(bool),
    String(Vec<char>),
    Null,
    Array(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<ObjectValue>>),
    Function(Rc<FunctionValue>),
    BuiltinFunction(Rc<BuiltinFunctionValue>),
    Prototype(Rc<RefCell<PrototypeValue>>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Integer(a), Value::Number(b)) => (*a as f64) == *b,
            (Value::Number(a), Value::Integer(b)) => *a == (*b as f64),

            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Array(a), Value::Array(b)) => Rc::ptr_eq(a, b),
            (Value::Object(a), Value::Object(b)) => Rc::ptr_eq(a, b),
            (Value::Function(a), Value::Function(b)) => Rc::ptr_eq(a, b),
            (Value::BuiltinFunction(a), Value::BuiltinFunction(b)) => Rc::ptr_eq(a, b),
            (Value::Prototype(a), Value::Prototype(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

pub struct ObjectValue {
    pub properties: HashMap<String, Value>,
    pub prototype: Rc<RefCell<PrototypeValue>>,
}

pub struct BuiltinFunctionValue {
    pub func:
        Box<dyn Fn(Vec<Value>, &Location, Rc<Environment>) -> Result<Value, LocatedControlFlow>>,
    pub is_static: bool,
}

pub struct FunctionValue {
    pub parameters: Vec<String>,
    pub is_static: bool,
    pub body: LocatedExpression,
    pub env: Rc<Environment>,
}

pub struct PrototypeValue {
    pub properties: HashMap<String, Value>,
    pub parent: Option<Rc<RefCell<PrototypeValue>>>,
}
