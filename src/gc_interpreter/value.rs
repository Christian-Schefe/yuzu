use std::collections::HashMap;

use gc_arena::{
    Collect, Gc, Mutation, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    gc_interpreter::{LocatedExpression, MyRoot},
    tree_interpreter::{Located, Location},
};

#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub enum Value<'a> {
    Number(f64),
    Integer(i64),
    Bool(bool),
    String(Vec<char>),
    Null,
    Array(GcRefLock<'a, Vec<Value<'a>>>),
    Object(GcRefLock<'a, ObjectValue<'a>>),
    Function(GcRefLock<'a, FunctionValue<'a>>),
    BuiltinFunction(Gc<'a, BuiltinFunctionValue<'a>>),
    Prototype(GcRefLock<'a, PrototypeValue<'a>>),
    Resource(GcRefLock<'a, Box<dyn Resource>>),
    Buffer(GcRefLock<'a, Vec<u8>>),
}

impl PartialEq for Value<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Integer(a), Value::Number(b)) => (*a as f64) == *b,
            (Value::Number(a), Value::Integer(b)) => *a == (*b as f64),

            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Array(a), Value::Array(b)) => Gc::ptr_eq(*a, *b),
            (Value::Object(a), Value::Object(b)) => Gc::ptr_eq(*a, *b),
            (Value::Function(a), Value::Function(b)) => Gc::ptr_eq(*a, *b),
            (Value::BuiltinFunction(a), Value::BuiltinFunction(b)) => Gc::ptr_eq(*a, *b),
            (Value::Prototype(a), Value::Prototype(b)) => Gc::ptr_eq(*a, *b),
            (Value::Buffer(a), Value::Buffer(b)) => Gc::ptr_eq(*a, *b),
            (Value::Resource(a), Value::Resource(b)) => Gc::ptr_eq(*a, *b),
            _ => false,
        }
    }
}

pub trait Resource: Collect {
    fn close(&mut self) -> Result<(), String>;
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, String>;
    fn write(&mut self, buf: &[u8]) -> Result<usize, String>;
}

impl std::fmt::Debug for dyn Resource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<resource>")
    }
}

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct ObjectValue<'a> {
    pub properties: HashMap<String, Value<'a>>,
    pub prototype: GcRefLock<'a, PrototypeValue<'a>>,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct BuiltinFunctionValue<'a> {
    pub func: StaticCollect<
        Box<
            dyn for<'b> Fn(
                &Mutation<'b>,
                &MyRoot<'b>,
                Vec<Value<'b>>,
                &Location,
                Gc<'b, Environment<'b>>,
            ) -> Result<Value<'b>, LocatedControlFlow<'b>>,
        >,
    >,
    pub kind: FunctionKind<'a>,
}

impl std::fmt::Debug for BuiltinFunctionValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunctionValue").finish()
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct FunctionValue<'a> {
    pub parameters: Vec<String>,
    pub body: Gc<'a, StaticCollect<LocatedExpression>>,
    pub env: Gc<'a, Environment<'a>>,
    pub kind: FunctionKind<'a>,
}
impl std::fmt::Debug for FunctionValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionValue")
            .field("parameters", &self.parameters)
            .field("body", &self.body)
            .finish()
    }
}

#[derive(Collect, Debug)]
#[collect(no_drop)]
pub struct PrototypeValue<'a> {
    pub properties: HashMap<String, Value<'a>>,
    pub parent: Option<GcRefLock<'a, PrototypeValue<'a>>>,
}

#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub enum FunctionKind<'a> {
    Function,
    Method,
    Constructor(GcRefLock<'a, PrototypeValue<'a>>),
}

impl std::fmt::Display for FunctionKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Function => write!(f, "function"),
            FunctionKind::Method => write!(f, "method"),
            FunctionKind::Constructor(_) => write!(f, "constructor"),
        }
    }
}

#[derive(Clone, Collect)]
#[collect(no_drop)]
pub struct Environment<'a> {
    values: GcRefLock<'a, HashMap<String, Value<'a>>>,
    parent: Option<Gc<'a, Environment<'a>>>,
    pub module_path: String,
}

impl<'a> Environment<'a> {
    pub fn new_global(mutation: &Mutation<'a>, module_path: String) -> Self {
        Self {
            values: Gc::new(mutation, RefLock::new(HashMap::new())),
            parent: None,
            module_path,
        }
    }

    pub fn root(&self) -> &Environment<'a> {
        let mut cur = self;
        while let Some(parent) = &cur.parent {
            cur = parent;
        }
        cur
    }

    pub fn new(mutation: &Mutation<'a>, parent: Gc<'a, Environment<'a>>) -> Self {
        Self {
            values: Gc::new(mutation, RefLock::new(HashMap::new())),
            module_path: parent.module_path.clone(),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value<'a>> {
        if let Some(val) = self.values.borrow().get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&self, mutation: &Mutation<'a>, name: &str, value: Value<'a>) -> bool {
        if self.values.borrow().contains_key(name) {
            self.values
                .borrow_mut(mutation)
                .insert(name.to_string(), value);
            true
        } else if let Some(parent) = &self.parent {
            parent.set(mutation, name, value)
        } else {
            false
        }
    }

    pub fn define(&self, mutation: &Mutation<'a>, name: &str, value: Value<'a>) -> bool {
        let mut map = self.values.borrow_mut(mutation);
        if map.contains_key(name) {
            return false;
        }
        map.insert(name.to_string(), value);
        true
    }
}

pub fn variable_to_string(var: &Value) -> String {
    match var {
        Value::Number(n) => n.to_string(),
        Value::Integer(i) => i.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::String(s) => s.iter().collect(),
        Value::Null => "null".to_string(),
        Value::Resource(_) => "<resource>".to_string(),
        Value::Object(entries) => {
            let entries = entries
                .borrow()
                .properties
                .iter()
                .map(|(k, v)| format!("{}: {}", k, variable_to_string(v)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", entries)
        }
        Value::Array(elements) => {
            let elements = elements
                .borrow()
                .iter()
                .map(|e| variable_to_string(e))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", elements)
        }
        Value::BuiltinFunction { .. } => "<builtin function>".to_string(),
        Value::Function(f) => format!("<{}>", f.borrow().kind),
        Value::Prototype(p) => {
            let entries = p
                .borrow()
                .properties
                .iter()
                .map(|(k, v)| format!("{}: {}", k, variable_to_string(&v)))
                .collect::<Vec<_>>()
                .join(", ");
            let parent = if let Some(parent) = &p.borrow().parent {
                format!(
                    " extends {}",
                    variable_to_string(&Value::Prototype(parent.clone()))
                )
            } else {
                "".to_string()
            };
            format!("<prototype {{{}}}{}>", entries, parent)
        }
        Value::Buffer(buf) => {
            let buf = buf.borrow();
            let display = if buf.len() > 10 {
                let mut s = buf[..10]
                    .iter()
                    .map(|b| format!("{:02x}", b))
                    .collect::<Vec<_>>()
                    .join(" ");
                s.push_str(" ...");
                s
            } else {
                buf.iter()
                    .map(|b| format!("{:02x}", b))
                    .collect::<Vec<_>>()
                    .join(" ")
            };
            format!("<buffer [{}]>", display)
        }
    }
}

#[derive(Clone, Collect, Debug)]
#[collect(no_drop)]
pub enum ControlFlow<'a> {
    Break,
    Continue,
    Error(Value<'a>),
}

pub type LocatedControlFlow<'a> = Located<ControlFlow<'a>>;
