use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::{BinaryOp, TypeHint};

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Number,
    Integer,
    Bool,
    String,
    Null,
    Continue,
    Break,
    Return(Box<ValueType>),
    Array(Option<Box<ValueType>>),
    Object(HashMap<String, ValueType>),
    Class(ClassType),
    Function(FunctionType),
    ClassInstance(ClassInstanceType),
    Any,
    Union(Vec<ValueType>),
}

impl ValueType {
    pub fn from_type_hint(
        hint: &TypeHint,
        env: Rc<RefCell<Environment>>,
    ) -> Result<ValueType, TypeError> {
        Ok(match hint {
            TypeHint::Number => ValueType::Number,
            TypeHint::Integer => ValueType::Integer,
            TypeHint::Bool => ValueType::Bool,
            TypeHint::String => ValueType::String,
            TypeHint::Null => ValueType::Null,
            TypeHint::Array(elem_type) => {
                let elem_value_type = elem_type
                    .as_ref()
                    .map(|et| ValueType::from_type_hint(et, env.clone()))
                    .transpose()?;
                ValueType::Array(elem_value_type.map(Box::new))
            }
            TypeHint::Object(fields) => {
                let mut value_fields = HashMap::new();
                for (key, val_type) in fields {
                    value_fields.insert(
                        key.clone(),
                        ValueType::from_type_hint(val_type, env.clone())?,
                    );
                }
                ValueType::Object(value_fields)
            }
            TypeHint::Class(name) => {
                let Some(class_type) = env.borrow().get(name) else {
                    return Err(TypeError::UndefinedVariable(name.clone()));
                };
                let ValueType::Class(class_type) = class_type else {
                    return Err(TypeError::NotAClass);
                };
                ValueType::Class(class_type)
            }
            TypeHint::Function {
                parameters,
                return_type,
            } => {
                let param_types = parameters
                    .iter()
                    .map(|p| ValueType::from_type_hint(p, env.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                let ret_type = Box::new(ValueType::from_type_hint(return_type, env.clone())?);
                ValueType::Function(FunctionType {
                    parameters: param_types,
                    return_type: ret_type,
                })
            }
            TypeHint::ClassInstance(name) => {
                let Some(class_type) = env.borrow().get(name) else {
                    return Err(TypeError::UndefinedVariable(name.clone()));
                };
                let ValueType::Class(class_type) = class_type else {
                    return Err(TypeError::NotAClass);
                };
                let instance = ClassInstanceType {
                    properties: class_type.instance_fields.clone(),
                    class: class_type,
                };
                ValueType::ClassInstance(instance)
            }
            TypeHint::Any => ValueType::Any,
            TypeHint::Union(types) => {
                let value_types = types
                    .iter()
                    .map(|t| ValueType::from_type_hint(t, env.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                ValueType::union(value_types)
            }
        })
    }
    pub fn union(types: Vec<ValueType>) -> ValueType {
        let mut flat_types = Vec::new();
        for t in types {
            if let ValueType::Any = t {
                return ValueType::Any;
            }
            match t {
                ValueType::Union(inner) => {
                    for it in inner {
                        if !flat_types.contains(&it) {
                            flat_types.push(it);
                        }
                    }
                }
                other => {
                    if !flat_types.contains(&other) {
                        flat_types.push(other);
                    }
                }
            }
        }
        if flat_types.len() == 1 {
            flat_types.into_iter().next().unwrap()
        } else {
            ValueType::Union(flat_types)
        }
    }
}

pub enum TypeError {
    Mismatch {
        expected: ValueType,
        found: ValueType,
    },
    UnsupportedBinaryOp {
        op: BinaryOp,
        left: ValueType,
        right: ValueType,
    },
    UndefinedVariable(String),
    Redefinition(String),
    InvalidAssignmentTarget,
    NotAClass,
    FieldNotFound(String),
    NotIndexable,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::Mismatch { expected, found } => {
                write!(
                    f,
                    "Type mismatch: expected {:?}, found {:?}",
                    expected, found
                )
            }
            TypeError::UnsupportedBinaryOp { op, left, right } => write!(
                f,
                "Unsupported binary operation: {:?} between {:?} and {:?}",
                op, left, right
            ),
            TypeError::UndefinedVariable(name) => write!(f, "Unknown identifier: {}", name),
            TypeError::InvalidAssignmentTarget => write!(f, "Invalid assignment target"),
            TypeError::NotAClass => write!(f, "Not a class type"),
            TypeError::Redefinition(name) => write!(f, "Redefinition of variable: {}", name),
            TypeError::FieldNotFound(name) => write!(f, "Field not found: {}", name),
            TypeError::NotIndexable => write!(f, "Type is not indexable"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInstanceType {
    pub properties: HashMap<String, ValueType>,
    pub class: ClassType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassType {
    pub instance_fields: HashMap<String, ValueType>,
    pub constructor: Option<FunctionType>,
    pub static_fields: HashMap<String, ValueType>,
    pub methods: HashMap<String, FunctionType>,
    pub static_methods: HashMap<String, FunctionType>,
    pub parent: Option<Box<ClassType>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<ValueType>,
    pub return_type: Box<ValueType>,
}

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, ValueType>,
    parent: Option<Rc<RefCell<Environment>>>,
    pub module_path: String,
}

impl Environment {
    pub fn new_global(module_path: String) -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
            module_path,
        }
    }

    pub fn root(env: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let mut current = env;
        loop {
            let borrow = current.borrow();
            let Some(parent) = borrow.parent.clone() else {
                break;
            };
            drop(borrow);
            current = parent;
        }
        current
    }

    pub fn new(parent: Rc<RefCell<Environment>>) -> Self {
        let mod_path = parent.borrow().module_path.clone();
        Self {
            values: HashMap::new(),
            module_path: mod_path,
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<ValueType> {
        if let Some(val) = self.values.get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &str, value: ValueType) -> bool {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            true
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set(name, value)
        } else {
            false
        }
    }

    pub fn define(&mut self, name: &str, value: ValueType) -> bool {
        if self.values.contains_key(name) {
            return false;
        }
        self.values.insert(name.to_string(), value);
        true
    }
}
