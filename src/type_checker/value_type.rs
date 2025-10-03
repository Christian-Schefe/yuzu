use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::{BinaryOp, TypeHint};

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Placeholder,
    Number,
    Integer,
    Bool,
    String,
    Null,
    Array(TypeId),
    Object(HashMap<String, TypeId>),
    Class(ClassType),
    Function(FunctionType),
    ClassInstance(ClassInstanceType),
    Any,
    Union(Vec<TypeId>),
    Unreachable(UnreachableReason),
}

pub type TypeId = usize;
pub struct TypeArena {
    types: Vec<ValueType>,
}

impl TypeArena {
    pub fn new() -> Self {
        Self {
            types: vec![
                ValueType::Null,
                ValueType::Number,
                ValueType::Integer,
                ValueType::Bool,
                ValueType::String,
                ValueType::Any,
                ValueType::Unreachable(UnreachableReason::Return),
                ValueType::Unreachable(UnreachableReason::LoopControlFlow),
                ValueType::Object(HashMap::new()),
                ValueType::Array(5), // Any array
            ],
        }
    }
    pub fn get(&self, id: TypeId) -> ValueType {
        self.types[id].clone()
    }
    pub fn new_type(&mut self, t: ValueType) -> TypeId {
        let id = self.types.len();
        self.types.push(t);
        id
    }
    pub fn replace(&mut self, id: TypeId, t: ValueType) {
        self.types[id] = t;
    }
    pub fn type_null(&self) -> TypeId {
        0
    }
    pub fn type_number(&self) -> TypeId {
        1
    }
    pub fn type_integer(&self) -> TypeId {
        2
    }
    pub fn type_bool(&self) -> TypeId {
        3
    }
    pub fn type_string(&self) -> TypeId {
        4
    }
    pub fn type_any(&self) -> TypeId {
        5
    }
    pub fn type_unreachable_return(&self) -> TypeId {
        6
    }
    pub fn type_unreachable_loop_control_flow(&self) -> TypeId {
        7
    }
    pub fn empty_object(&self) -> TypeId {
        8
    }
    pub fn any_array(&self) -> TypeId {
        9
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnreachableReason {
    Return,
    LoopControlFlow,
}

impl ValueType {
    pub fn from_type_hint(
        arena: &mut TypeArena,
        hint: &TypeHint,
        env: Rc<RefCell<Environment>>,
    ) -> Result<TypeId, TypeError> {
        Ok(match hint {
            TypeHint::Number => arena.type_number(),
            TypeHint::Integer => arena.type_integer(),
            TypeHint::Bool => arena.type_bool(),
            TypeHint::String => arena.type_string(),
            TypeHint::Null => arena.type_null(),
            TypeHint::Array(elem_type) => {
                let elem_id = if let Some(et) = elem_type {
                    ValueType::from_type_hint(arena, et, env.clone())?
                } else {
                    arena.type_any()
                };
                let val = ValueType::Array(elem_id);
                arena.new_type(val)
            }
            TypeHint::Object(fields) => {
                let mut value_fields = HashMap::new();
                for (key, val_type) in fields {
                    value_fields.insert(
                        key.clone(),
                        ValueType::from_type_hint(arena, val_type, env.clone())?,
                    );
                }
                let val = ValueType::Object(value_fields);
                arena.new_type(val)
            }
            TypeHint::Class(name) => {
                let Some(class_type) = env.borrow().get(name) else {
                    return Err(TypeError::UndefinedVariable(name.clone()));
                };
                let ValueType::Class(_) = arena.get(class_type) else {
                    return Err(TypeError::NotAClass);
                };
                class_type
            }
            TypeHint::Function {
                parameters,
                return_type,
            } => {
                let param_types = parameters
                    .iter()
                    .map(|p| ValueType::from_type_hint(arena, p, env.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                let ret_type =
                    Box::new(ValueType::from_type_hint(arena, return_type, env.clone())?);
                let val = ValueType::Function(FunctionType {
                    parameters: param_types,
                    return_type: ret_type,
                });
                arena.new_type(val)
            }
            TypeHint::ClassInstance(name) => {
                let Some(class_type) = env.borrow().get(name) else {
                    return Err(TypeError::UndefinedVariable(name.clone()));
                };
                let ValueType::Class(class) = arena.get(class_type) else {
                    return Err(TypeError::NotAClass);
                };
                let instance = ClassInstanceType {
                    properties: class.instance_fields.clone(),
                    class,
                };
                let val = ValueType::ClassInstance(instance);
                arena.new_type(val)
            }
            TypeHint::Any => arena.type_any(),
            TypeHint::Union(types) => {
                let value_types = types
                    .iter()
                    .map(|t| ValueType::from_type_hint(arena, t, env.clone()))
                    .collect::<Result<Vec<_>, _>>()?;
                let val = ValueType::union(arena, value_types);
                arena.new_type(val)
            }
            TypeHint::This => {
                let env_ref = env.borrow();
                if let Some(class_type) = env_ref.in_class {
                    class_type
                } else {
                    return Err(TypeError::NotAClass);
                }
            }
        })
    }
    pub fn union(arena: &mut TypeArena, types: Vec<TypeId>) -> ValueType {
        let mut flat_types = Vec::new();
        let mut flat_type_ids = Vec::new();
        for t in types {
            if let ValueType::Any = arena.get(t) {
                return ValueType::Any;
            }
            match arena.get(t) {
                ValueType::Union(inner) => {
                    for it in inner {
                        let v = arena.get(it);
                        if !flat_types.contains(&v) {
                            flat_types.push(v);
                            flat_type_ids.push(it);
                        }
                    }
                }
                other => {
                    if !flat_types.contains(&other) {
                        flat_types.push(other);
                        flat_type_ids.push(t);
                    }
                }
            }
        }
        if flat_types.len() == 1 {
            flat_types.into_iter().next().unwrap()
        } else {
            ValueType::Union(flat_type_ids)
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
    ContinueOrBreakOutsideLoop,
    ReturnOutsideFunction,
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
            TypeError::ContinueOrBreakOutsideLoop => {
                write!(f, "Continue or break statement outside of loop")
            }
            TypeError::ReturnOutsideFunction => write!(f, "Return statement outside of function"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInstanceType {
    pub properties: HashMap<String, TypeId>,
    pub class: ClassType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassType {
    pub instance_fields: HashMap<String, TypeId>,
    pub constructor: Option<FunctionType>,
    pub static_fields: HashMap<String, TypeId>,
    pub methods: HashMap<String, FunctionType>,
    pub static_methods: HashMap<String, FunctionType>,
    pub parent: Option<Box<ClassType>>,
}

impl ClassType {
    pub fn get_class_instance_type(&self) -> ClassInstanceType {
        let mut properties = self.instance_fields.clone();
        if let Some(parent) = &self.parent {
            for (k, v) in &parent.instance_fields {
                if !properties.contains_key(k) {
                    properties.insert(k.clone(), v.clone());
                }
            }
        }
        ClassInstanceType {
            properties,
            class: self.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<TypeId>,
    pub return_type: Box<TypeId>,
}

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, TypeId>,
    parent: Option<Rc<RefCell<Environment>>>,
    pub module_path: String,
    pub in_loop: bool,
    pub return_type: Option<TypeId>,
    pub in_class: Option<TypeId>,
}

impl Environment {
    pub fn new_global(module_path: String) -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
            module_path,
            in_loop: false,
            return_type: None,
            in_class: None,
        }
    }

    pub fn new(parent: Rc<RefCell<Environment>>) -> Self {
        let p_ref = parent.borrow();
        let mod_path = p_ref.module_path.clone();
        let in_loop = p_ref.in_loop;
        let return_type = p_ref.return_type.clone();
        drop(p_ref);
        Self {
            values: HashMap::new(),
            module_path: mod_path,
            in_loop,
            return_type,
            in_class: None,
            parent: Some(parent),
        }
    }

    pub fn new_loop(parent: Rc<RefCell<Environment>>) -> Self {
        let p_ref = parent.borrow();
        let mod_path = p_ref.module_path.clone();
        let return_type = p_ref.return_type.clone();
        drop(p_ref);
        Self {
            values: HashMap::new(),
            module_path: mod_path,
            in_loop: true,
            return_type,
            in_class: None,
            parent: Some(parent),
        }
    }

    pub fn new_function(parent: Rc<RefCell<Environment>>, return_type: TypeId) -> Self {
        let p_ref = parent.borrow();
        let mod_path = p_ref.module_path.clone();
        drop(p_ref);
        Self {
            values: HashMap::new(),
            module_path: mod_path,
            in_loop: false,
            return_type: Some(return_type),
            in_class: None,
            parent: Some(parent),
        }
    }

    pub fn new_class(parent: Rc<RefCell<Environment>>, placeholder: usize) -> Self {
        let p_ref = parent.borrow();
        let mod_path = p_ref.module_path.clone();
        let return_type = p_ref.return_type.clone();
        drop(p_ref);
        Self {
            values: HashMap::new(),
            module_path: mod_path,
            in_loop: false,
            return_type,
            in_class: Some(placeholder),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<TypeId> {
        if let Some(val) = self.values.get(name) {
            Some(*val)
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    pub fn can_define(&self, name: &str) -> bool {
        !self.values.contains_key(name)
    }

    pub fn define(&mut self, name: &str, value: TypeId) {
        self.values.insert(name.to_string(), value);
    }
}
