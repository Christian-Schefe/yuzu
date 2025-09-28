use std::{
    cell::{OnceCell, RefCell},
    collections::{HashMap, HashSet},
    fmt::{self},
    io::Read,
    path::PathBuf,
    rc::Rc,
};

use crate::{
    parse_string,
    parser::{BinaryOp, ClassMemberType, Expression, Extra, UnaryOp},
    tree_interpreter::standard::define_globals,
};

pub mod standard;

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Integer(i64),
    Bool(bool),
    String(Vec<char>),
    Null,
    Array(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<HashMap<String, Value>>>),
    Function(Rc<FunctionValue>),
    BuiltinFunction(BuiltinFunctionType),
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

type BuiltinFunctionType =
    Rc<dyn Fn(Vec<Value>, &Location, Rc<Environment>) -> Result<Value, LocatedControlFlow>>;

#[derive(Clone)]
pub struct Location {
    pub span: core::ops::Range<usize>,
    pub module: String,
}

impl Location {
    pub fn new(range: core::ops::Range<usize>, module: String) -> Self {
        Self {
            span: range,
            module,
        }
    }
}

pub struct FunctionValue {
    parameters: Vec<String>,
    body: LocatedExpression,
    env: Rc<Environment>,
}

pub struct PrototypeValue {
    properties: HashMap<String, Value>,
    static_properties: HashSet<String>,
}

impl Value {
    fn get_prototype_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "Number",
            Value::Integer(_) => "Integer",
            Value::Bool(_) => "Bool",
            Value::String(_) => "String",
            Value::Null => "Null",
            Value::Array(_) => "Array",
            Value::Object(_) => "Object",
            Value::Function { .. } => "Function",
            Value::BuiltinFunction { .. } => "BuiltinFunction",
            Value::Prototype { .. } => "Prototype",
        }
    }
}

#[derive(Clone)]
pub struct Environment {
    values: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Rc<Environment>>,
    module_path: String,
}

impl Environment {
    pub fn new_global(module_path: String) -> Self {
        Self {
            values: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
            module_path,
        }
    }

    pub fn new(parent: Rc<Environment>) -> Self {
        Self {
            values: Rc::new(RefCell::new(HashMap::new())),
            module_path: parent.module_path.clone(),
            parent: Some(parent),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.values.borrow().get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&self, name: &str, value: Value) -> bool {
        if self.values.borrow().contains_key(name) {
            self.values.borrow_mut().insert(name.to_string(), value);
            true
        } else if let Some(parent) = &self.parent {
            parent.set(name, value)
        } else {
            false
        }
    }

    pub fn define(&self, name: &str, value: Value) -> bool {
        let mut map = self.values.borrow_mut();
        if map.contains_key(name) {
            return false;
        }
        map.insert(name.to_string(), value);
        true
    }
}

pub enum RuntimeError {
    TypeError(String),
    UndefinedVariable(String),
    FieldAccessError(String),
    ArrayIndexOutOfBounds(i64),
    FunctionArgumentError(String),
    DuplicateVariableDefinition(String),
    ImportError(String),
    Custom(Value),
    UnhandledControlFlow,
}

fn type_error<T>(msg: &str, expr: &LocatedExpression) -> Result<T, LocatedControlFlow> {
    Err(RuntimeError::TypeError(msg.to_string()).at(expr))
}

pub enum ControlFlow {
    Return(Value),
    Break,
    Continue,
    Error(RuntimeError),
}

pub type LocatedControlFlow = Located<ControlFlow>;

pub struct Located<T> {
    pub data: T,
    pub location: Location,
}

impl RuntimeError {
    pub fn at(self, expr: &LocatedExpression) -> LocatedControlFlow {
        LocatedControlFlow {
            data: ControlFlow::Error(self),
            location: expr.extra.clone(),
        }
    }
    pub fn span(self, location: &Location) -> LocatedControlFlow {
        LocatedControlFlow {
            data: ControlFlow::Error(self),
            location: location.clone(),
        }
    }
    pub fn from_to(self, from: &LocatedExpression, to: &LocatedExpression) -> LocatedControlFlow {
        LocatedControlFlow {
            data: ControlFlow::Error(self),
            location: Location {
                span: from.extra.span.start..to.extra.span.end,
                module: from.extra.module.clone(),
            },
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TypeError(msg) => write!(f, "TypeError: {}", msg),
            Self::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            Self::FieldAccessError(msg) => write!(f, "Field does not exist: {}", msg),
            Self::ArrayIndexOutOfBounds(index) => {
                write!(f, "Array index out of bounds: {}", index)
            }
            Self::FunctionArgumentError(msg) => write!(f, "Function argument error: {}", msg),
            Self::DuplicateVariableDefinition(name) => {
                write!(f, "Variable already defined in this scope: {}", name)
            }
            Self::UnhandledControlFlow => {
                write!(
                    f,
                    "Unhandled control flow (return, break, continue) outside of function or loop"
                )
            }
            Self::Custom(val) => write!(f, "Error: {}", variable_to_string(val)),
            Self::ImportError(msg) => write!(f, "ImportError: {}", msg),
        }
    }
}

fn variable_to_string(var: &Value) -> String {
    match var {
        Value::Number(n) => n.to_string(),
        Value::Integer(i) => i.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::String(s) => s.iter().collect(),
        Value::Null => "null".to_string(),
        Value::Object(entries) => {
            let entries = entries
                .borrow()
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
        Value::Function { .. } => "<function>".to_string(),
        Value::Prototype { .. } => "<prototype>".to_string(),
    }
}

thread_local! {
    pub static GLOBAL_STATE: OnceCell<RefCell<GlobalState>> = OnceCell::new();
}

pub struct GlobalState {
    modules: HashMap<String, Option<Value>>,
    pwd: PathBuf,
}

pub fn init_global_state(pwd: PathBuf) {
    GLOBAL_STATE.with(|state| {
        state.get_or_init(|| {
            RefCell::new(GlobalState {
                modules: HashMap::new(),
                pwd,
            })
        });
    });
}

type LocatedExpression = Extra<Location>;

pub fn interpret_global(
    expr: &LocatedExpression,
    module_path: String,
    with_std: bool,
) -> Result<Value, Located<RuntimeError>> {
    let global_env = Rc::new(Environment::new_global(module_path));
    define_globals(&*global_env, with_std);
    match interpret(expr, global_env) {
        Ok(v) => Ok(v),
        Err(LocatedControlFlow {
            data,
            location: span,
        }) => match data {
            ControlFlow::Error(e) => Err(Located {
                data: e,
                location: span,
            }),
            ControlFlow::Return(v) => Ok(v),
            _ => Err(Located {
                data: RuntimeError::UnhandledControlFlow,
                location: span,
            }),
        },
    }
}

fn interpret(expr: &LocatedExpression, env: Rc<Environment>) -> Result<Value, LocatedControlFlow> {
    match &expr.expr {
        Expression::Number(n) => Ok(Value::Number(*n)),
        Expression::Integer(i) => Ok(Value::Integer(*i)),
        Expression::Bool(b) => Ok(Value::Bool(*b)),
        Expression::Null => Ok(Value::Null),
        Expression::String(s) => Ok(Value::String(s.chars().collect())),
        Expression::ArrayLiteral(elements) => {
            let mut vals = Vec::new();
            for elem in elements {
                let val = interpret(elem, env.clone())?;
                vals.push(val);
            }
            Ok(Value::Array(Rc::new(RefCell::new(vals))))
        }
        Expression::ObjectLiteral(pairs) => {
            let mut map = HashMap::new();
            for (key, value_expr) in pairs {
                let value = interpret(value_expr, env.clone())?;
                map.insert(key.clone(), value);
            }
            Ok(Value::Object(Rc::new(RefCell::new(map))))
        }
        Expression::FunctionLiteral { parameters, body } => {
            Ok(Value::Function(Rc::new(FunctionValue {
                parameters: parameters.clone(),
                body: *body.clone(),
                env,
            })))
        }
        Expression::PrototypeLiteral {
            properties,
            superclass,
        } => {
            let mut map = HashMap::new();
            let mut static_properties = HashSet::new();

            if let Some(superclass) = superclass {
                let super_val = interpret(superclass, env.clone())?;
                let Value::Prototype(p) = super_val else {
                    return type_error("Superclass must be a prototype", superclass);
                };
                let p = p.borrow();
                map = p.properties.clone();
                static_properties = p.static_properties.clone();
            }

            let mut constructors = Vec::new();
            for (name, member_type, property) in properties {
                let val = interpret(property, env.clone())?;
                if let ClassMemberType::Constructor = member_type {
                    let Value::Function(f) = val else {
                        return type_error("Constructor must be a function", property);
                    };
                    constructors.push((name, f));
                } else {
                    map.insert(name.clone(), val);
                }
            }
            let mut no_statics_map = map.clone();
            for static_property in static_properties.iter() {
                no_statics_map.remove(static_property);
            }
            for (name, constructor) in constructors {
                map.insert(
                    name.to_string(),
                    Value::BuiltinFunction(instantiate_prototype(
                        no_statics_map.clone(),
                        constructor,
                    )),
                );
                static_properties.insert(name.to_string());
            }
            let prototype = Rc::new(RefCell::new(PrototypeValue {
                properties: map,
                static_properties,
            }));
            Ok(Value::Prototype(prototype))
        }
        Expression::Break => Err(LocatedControlFlow {
            data: ControlFlow::Break,
            location: expr.extra.clone(),
        }),
        Expression::Continue => Err(LocatedControlFlow {
            data: ControlFlow::Continue,
            location: expr.extra.clone(),
        }),
        Expression::Return(ret_expr) => {
            let val = if let Some(expr) = ret_expr {
                interpret(expr, env)?
            } else {
                Value::Null
            };
            Err(LocatedControlFlow {
                data: ControlFlow::Return(val),
                location: expr.extra.clone(),
            })
        }
        Expression::Raise(err_expr) => {
            let val = interpret(err_expr, env.clone())?;
            Err(RuntimeError::Custom(val).at(expr))
        }
        Expression::Ident(name) => {
            if let Some(val) = env.get(name) {
                Ok(val)
            } else {
                Err(RuntimeError::UndefinedVariable(name.clone()).at(expr))
            }
        }
        Expression::Define(name, value) => {
            let val = interpret(value, env.clone())?;
            if env.define(name, val) {
                Ok(Value::Null)
            } else {
                Err(RuntimeError::DuplicateVariableDefinition(name.to_string()).at(expr))
            }
        }
        Expression::Block(exprs, ret) => {
            let inner_env = Rc::new(Environment::new(env));
            for e in exprs {
                interpret(e, inner_env.clone())?;
            }
            if let Some(r) = ret {
                interpret(r, inner_env.clone())
            } else {
                Ok(Value::Null)
            }
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let cond = interpret(condition, env.clone())?;
            match cond {
                Value::Bool(true) => interpret(then_branch, env.clone()),
                Value::Bool(false) => {
                    if let Some(else_b) = else_branch {
                        interpret(else_b, env.clone())
                    } else {
                        Ok(Value::Null)
                    }
                }
                _ => Err(
                    RuntimeError::TypeError("Condition of if must be a boolean".to_string())
                        .at(&condition),
                ),
            }
        }
        Expression::Loop {
            init,
            condition,
            increment,
            body,
        } => interpret_for_loop(env, init, condition, increment, body),
        Expression::UnaryOp { op, expr } => {
            let val = interpret(expr, env.clone())?;
            match (op, val) {
                (UnaryOp::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
                (UnaryOp::Negate, Value::Integer(i)) => Ok(Value::Integer(-i)),
                (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                _ => Err(RuntimeError::TypeError(format!(
                    "Invalid operand type for unary operator {:?}",
                    op
                ))
                .at(expr)),
            }
        }
        Expression::IterLoop {
            item,
            iterable,
            body,
        } => {
            let iter_val = interpret(iterable, env.clone())?;
            let iter = do_property_function_call(
                &env,
                &iter_val,
                &iterable.extra,
                &"iter".to_string(),
                &vec![],
            )?;
            loop {
                let next = do_property_function_call(
                    &env,
                    &iter,
                    &iterable.extra,
                    &"next".to_string(),
                    &vec![],
                )?;
                let next_item = match next {
                    Value::Object(next_map) => next_map.borrow().get("value").cloned(),
                    _ => None,
                };
                let Some(next_item) = next_item else {
                    break;
                };
                let loop_env = Rc::new(Environment::new(env.clone()));
                loop_env.define(item, next_item);
                if let Err(LocatedControlFlow {
                    data,
                    location: span,
                }) = interpret(body, loop_env.clone())
                {
                    match data {
                        ControlFlow::Break => break,
                        ControlFlow::Continue => continue,
                        other => {
                            return Err(LocatedControlFlow {
                                data: other,
                                location: span,
                            });
                        }
                    }
                }
            }
            Ok(Value::Null)
        }
        Expression::BinaryOp { op, left, right } => interpret_binary_op(&env, op, left, right),
        Expression::FunctionCall {
            function,
            arguments,
        } => {
            let func_val = interpret(function, env.clone())?;
            let arg_vals = interpret_args(arguments, env.clone())?;
            do_function_call(func_val, &function.extra, arg_vals, env.clone())
        }
        Expression::PropertyFunctionCall {
            object,
            function,
            arguments,
        } => do_property_function_call(
            &env,
            &interpret(object, env.clone())?,
            &object.extra,
            function,
            arguments,
        ),
        Expression::FieldAccess { object, field } => do_property_access(env, object, field),
        Expression::ArrayIndex { array, index } => {
            let arr_val = interpret(array, env.clone())?;
            let idx_val = interpret(index, env.clone())?;
            match arr_val {
                Value::Array(elements) => {
                    let Value::Integer(i) = idx_val else {
                        return Err(RuntimeError::TypeError(
                            "Array index must be an integer".to_string(),
                        )
                        .at(&index));
                    };
                    let elements = elements.borrow();
                    if let Ok(i) = TryInto::<usize>::try_into(i)
                        && i < elements.len()
                    {
                        Ok(elements[i].clone())
                    } else {
                        Err(RuntimeError::ArrayIndexOutOfBounds(i).at(&index))
                    }
                }
                Value::String(string) => {
                    let Value::Integer(i) = idx_val else {
                        return Err(RuntimeError::TypeError(
                            "Array index must be an integer".to_string(),
                        )
                        .at(&index));
                    };
                    if let Ok(i) = TryInto::<usize>::try_into(i)
                        && i < string.len()
                    {
                        Ok(Value::String(vec![string[i]]))
                    } else {
                        Err(RuntimeError::ArrayIndexOutOfBounds(i).at(&index))
                    }
                }
                Value::Object(object_map) => {
                    let Value::String(key_chars) = idx_val else {
                        return Err(RuntimeError::TypeError(
                            "Object index must be a string".to_string(),
                        )
                        .at(&index));
                    };
                    let key: String = key_chars.iter().collect();
                    let map = object_map.borrow();
                    if let Some(val) = map.get(&key) {
                        Ok(val.clone())
                    } else {
                        Err(RuntimeError::FieldAccessError(key).at(&index))
                    }
                }
                _ => {
                    return Err(RuntimeError::TypeError(
                        "Attempted to index a non-array value".to_string(),
                    )
                    .at(&array));
                }
            }
        }
    }
}

fn do_property_function_call(
    env: &Rc<Environment>,
    obj_val: &Value,
    span: &Location,
    function: &str,
    arguments: &Vec<LocatedExpression>,
) -> Result<Value, Located<ControlFlow>> {
    let arg_vals = interpret_args(arguments, env.clone())?;
    match &obj_val {
        Value::Object(object_map) => {
            let function = object_map.borrow().get(function).cloned();
            if let Some(f) = function {
                return do_target_function_call(Some(&obj_val), &span, f, arg_vals, env.clone());
            }
        }
        Value::Prototype(p) => {
            let function = p.borrow().properties.get(function).cloned();
            if let Some(f) = function {
                return do_target_function_call(None, &span, f, arg_vals, env.clone());
            }
        }
        _ => {}
    };
    let prototype = obj_val.get_prototype_name();
    if let Some(Value::Prototype(p)) = env.get(prototype) {
        if let Some(val) = p.borrow().properties.get(function).cloned() {
            do_target_function_call(Some(&obj_val), &span, val, arg_vals, env.clone())
        } else {
            Err(RuntimeError::FieldAccessError(function.to_string()).span(&span))
        }
    } else {
        Err(RuntimeError::FieldAccessError(function.to_string()).span(&span))
    }
}

fn do_property_access(
    env: Rc<Environment>,
    object: &Box<LocatedExpression>,
    field: &String,
) -> Result<Value, Located<ControlFlow>> {
    let obj_val = interpret(object, env.clone())?;
    let res = match &obj_val {
        Value::Object(object_map) => object_map.borrow().get(field).cloned(),
        Value::Prototype(p) => p.borrow().properties.get(field).cloned(),
        _ => None,
    };
    if let Some(v) = res {
        Ok(v)
    } else {
        let prototype = obj_val.get_prototype_name();
        if let Some(Value::Prototype(p)) = env.get(prototype) {
            if let Some(val) = p.borrow().properties.get(field) {
                Ok(val.clone())
            } else {
                Err(RuntimeError::FieldAccessError(field.clone()).at(&object))
            }
        } else {
            Err(RuntimeError::FieldAccessError(field.clone()).at(&object))
        }
    }
}

fn interpret_for_loop(
    env: Rc<Environment>,
    init: &Option<Box<LocatedExpression>>,
    condition: &Box<LocatedExpression>,
    increment: &Option<Box<LocatedExpression>>,
    body: &Box<LocatedExpression>,
) -> Result<Value, Located<ControlFlow>> {
    let loop_env = Rc::new(Environment::new(env));
    if let Some(init) = init {
        interpret(init, loop_env.clone())?;
    }
    loop {
        let cond = match interpret(condition, loop_env.clone()) {
            Err(LocatedControlFlow {
                data,
                location: span,
            }) => match data {
                ControlFlow::Break => break,
                ControlFlow::Continue => continue,
                other => {
                    return Err(LocatedControlFlow {
                        data: other,
                        location: span,
                    });
                }
            },
            Ok(v) => v,
        };
        let Value::Bool(cond) = cond else {
            return Err(RuntimeError::TypeError(
                "Condition of for loop must be a boolean".to_string(),
            )
            .at(&condition));
        };
        if !cond {
            break;
        }

        if let Err(LocatedControlFlow {
            data,
            location: span,
        }) = interpret(body, loop_env.clone())
        {
            match data {
                ControlFlow::Break => break,
                ControlFlow::Continue => continue,
                other => {
                    return Err(LocatedControlFlow {
                        data: other,
                        location: span,
                    });
                }
            }
        }
        if let Some(increment) = increment {
            if let Err(LocatedControlFlow {
                data,
                location: span,
            }) = interpret(increment, loop_env.clone())
            {
                match data {
                    ControlFlow::Break => break,
                    ControlFlow::Continue => continue,
                    other => {
                        return Err(LocatedControlFlow {
                            data: other,
                            location: span,
                        });
                    }
                }
            }
        }
    }
    Ok(Value::Null)
}

fn interpret_binary_op(
    env: &Rc<Environment>,
    op: &BinaryOp,
    left: &Box<LocatedExpression>,
    right: &Box<LocatedExpression>,
) -> Result<Value, LocatedControlFlow> {
    if op == &BinaryOp::Assign
        || op == &BinaryOp::AddAssign
        || op == &BinaryOp::SubtractAssign
        || op == &BinaryOp::MultiplyAssign
        || op == &BinaryOp::DivideAssign
    {
        fn compute_result(
            op: &BinaryOp,
            left: impl FnOnce() -> Result<Value, RuntimeError>,
            right: Value,
        ) -> Result<Value, RuntimeError> {
            match op {
                BinaryOp::Assign => Ok(right),
                BinaryOp::AddAssign
                | BinaryOp::SubtractAssign
                | BinaryOp::MultiplyAssign
                | BinaryOp::DivideAssign => {
                    let actual_op = match op {
                        BinaryOp::AddAssign => BinaryOp::Add,
                        BinaryOp::SubtractAssign => BinaryOp::Subtract,
                        BinaryOp::MultiplyAssign => BinaryOp::Multiply,
                        BinaryOp::DivideAssign => BinaryOp::Divide,
                        _ => unreachable!(),
                    };
                    interpret_simple_binary_op(&actual_op, left()?, right)
                }
                _ => unreachable!(),
            }
        }
        match &left.expr {
            Expression::Ident(name) => {
                let val = interpret(right, env.clone())?;
                let result = compute_result(
                    op,
                    || {
                        env.get(name)
                            .ok_or_else(|| RuntimeError::UndefinedVariable(name.clone()))
                    },
                    val,
                )
                .map_err(|e| e.at(left))?;
                if env.set(name, result.clone()) {
                    Ok(result)
                } else {
                    Err(RuntimeError::UndefinedVariable(name.clone()).at(left))
                }
            }
            Expression::FieldAccess { object, field } => {
                let obj_val = interpret(object, env.clone())?;
                match obj_val {
                    Value::Object(object_map) => {
                        let val = interpret(right, env.clone())?;
                        let result = compute_result(
                            op,
                            || {
                                let map = object_map.borrow();
                                map.get(field)
                                    .cloned()
                                    .ok_or_else(|| RuntimeError::FieldAccessError(field.clone()))
                            },
                            val,
                        )
                        .map_err(|e| e.at(left))?;
                        let mut map = object_map.borrow_mut();
                        map.insert(field.clone(), result.clone());
                        Ok(result)
                    }
                    _ => Err(RuntimeError::TypeError(
                        "Field access must be on an object".to_string(),
                    )
                    .at(object)),
                }
            }
            Expression::ArrayIndex { array, index } => {
                let arr_val = interpret(array, env.clone())?;
                let idx_val = interpret(index, env.clone())?;
                let Value::Array(elements) = arr_val else {
                    return Err(RuntimeError::TypeError(
                        "Attempted to index a non-array value".to_string(),
                    )
                    .at(&array));
                };
                let Value::Integer(i) = idx_val else {
                    return Err(RuntimeError::TypeError(
                        "Array index must be an integer".to_string(),
                    )
                    .at(&index));
                };
                if let Ok(i) = TryInto::<usize>::try_into(i) {
                    let val = interpret(right, env.clone())?;
                    let result = compute_result(
                        op,
                        || {
                            let elements = elements.borrow();
                            if i >= elements.len() {
                                return Err(RuntimeError::ArrayIndexOutOfBounds(i as i64));
                            }
                            Ok(elements[i].clone())
                        },
                        val,
                    )
                    .map_err(|e| e.at(left))?;
                    let mut elements = elements.borrow_mut();
                    elements[i] = result.clone();
                    Ok(result)
                } else {
                    Err(RuntimeError::ArrayIndexOutOfBounds(i).at(&index))
                }
            }
            _ => Err(RuntimeError::TypeError(
                "Left side of assignment must be assignable".to_string(),
            )
            .at(left)),
        }
    } else {
        let left_val = interpret(left, env.clone())?;
        match (op, &left_val) {
            (BinaryOp::And, Value::Bool(false)) => Ok(Value::Bool(false)),
            (BinaryOp::Or, Value::Bool(true)) => Ok(Value::Bool(true)),
            _ => {
                let right_val = interpret(right, env.clone())?;
                interpret_simple_binary_op(op, left_val, right_val)
                    .map_err(|e| e.from_to(&left, &right))
            }
        }
    }
}

fn interpret_simple_binary_op(
    op: &BinaryOp,
    left: Value,
    right: Value,
) -> Result<Value, RuntimeError> {
    match (op, left, right) {
        (BinaryOp::Add, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
        (BinaryOp::Add, Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
        (BinaryOp::Add, Value::Integer(l), Value::Number(r)) => Ok(Value::Number(l as f64 + r)),
        (BinaryOp::Add, Value::Number(l), Value::Integer(r)) => Ok(Value::Number(l + r as f64)),

        (BinaryOp::Subtract, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
        (BinaryOp::Subtract, Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l - r)),
        (BinaryOp::Subtract, Value::Integer(l), Value::Number(r)) => {
            Ok(Value::Number(l as f64 - r))
        }
        (BinaryOp::Subtract, Value::Number(l), Value::Integer(r)) => {
            Ok(Value::Number(l - r as f64))
        }

        (BinaryOp::Multiply, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
        (BinaryOp::Multiply, Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
        (BinaryOp::Multiply, Value::Integer(l), Value::Number(r)) => {
            Ok(Value::Number(l as f64 * r))
        }
        (BinaryOp::Multiply, Value::Number(l), Value::Integer(r)) => {
            Ok(Value::Number(l * r as f64))
        }

        (BinaryOp::Divide, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
        (BinaryOp::Divide, Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l / r)),
        (BinaryOp::Divide, Value::Integer(l), Value::Number(r)) => Ok(Value::Number(l as f64 / r)),
        (BinaryOp::Divide, Value::Number(l), Value::Integer(r)) => Ok(Value::Number(l / r as f64)),

        (BinaryOp::And, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
        (BinaryOp::Or, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),

        (BinaryOp::Equal, l, r) => Ok(Value::Bool(l == r)),
        (BinaryOp::NotEqual, l, r) => Ok(Value::Bool(l != r)),

        (BinaryOp::Less, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
        (BinaryOp::Less, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l < r)),
        (BinaryOp::LessEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
        (BinaryOp::LessEqual, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l <= r)),

        (BinaryOp::Greater, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
        (BinaryOp::Greater, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l > r)),
        (BinaryOp::GreaterEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
        (BinaryOp::GreaterEqual, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l >= r)),

        _ => Err(RuntimeError::TypeError(format!(
            "Invalid operand types for binary operator {:?}",
            op
        ))),
    }
}

fn do_function_call(
    function_val: Value,
    span: &Location,
    arguments: Vec<Value>,
    env: Rc<Environment>,
) -> Result<Value, LocatedControlFlow> {
    match function_val {
        Value::Function(f) => {
            let FunctionValue {
                parameters,
                body,
                env: func_env,
            } = &*f;
            if parameters.len() != arguments.len() {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected {} arguments, got {}",
                    parameters.len(),
                    arguments.len()
                ))
                .span(span));
            }
            let call_env = Rc::new(Environment::new(func_env.clone()));
            for (param, arg) in parameters.iter().zip(arguments.into_iter()) {
                if !call_env.define(param, arg) {
                    unreachable!("Parameter name should not be duplicated: {}", param);
                }
            }
            handle_return_control_flow(interpret(&body, call_env))
        }
        Value::BuiltinFunction(func) => handle_return_control_flow(func(arguments, span, env)),
        _ => Err(
            RuntimeError::TypeError("Attempted to call a non-function value".to_string())
                .span(span),
        ),
    }
}

fn do_target_function_call(
    target_val: Option<&Value>,
    target: &Location,
    function_val: Value,
    arguments: Vec<Value>,
    env: Rc<Environment>,
) -> Result<Value, LocatedControlFlow> {
    match function_val {
        Value::Function(f) => {
            let FunctionValue {
                parameters,
                body,
                env: func_env,
            } = &*f;
            let arg_count = arguments.len() + if target_val.is_some() { 1 } else { 0 };
            if parameters.len() != arg_count {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected {} arguments, got {}",
                    parameters.len(),
                    arg_count
                ))
                .span(target));
            }
            let call_env = Rc::new(Environment::new(func_env.clone()));
            let args_iter = target_val.cloned().into_iter().chain(arguments.into_iter());
            for (param, arg) in parameters.iter().zip(args_iter) {
                if !call_env.define(param, arg) {
                    unreachable!("Parameter name should not be duplicated: {}", param);
                }
            }
            handle_return_control_flow(interpret(&body, call_env))
        }
        Value::BuiltinFunction(func) => {
            let arg_vals = target_val
                .cloned()
                .into_iter()
                .chain(arguments.into_iter())
                .collect::<Vec<_>>();
            handle_return_control_flow(func(arg_vals, target, env))
        }
        _ => Err(
            RuntimeError::TypeError("Attempted to call a non-function value".to_string())
                .span(target),
        ),
    }
}

fn handle_return_control_flow(
    res: Result<Value, LocatedControlFlow>,
) -> Result<Value, LocatedControlFlow> {
    match res {
        Ok(v) => Ok(v),
        Err(LocatedControlFlow {
            data,
            location: span,
        }) => match data {
            ControlFlow::Error(e) => Err(Located {
                data: ControlFlow::Error(e),
                location: span,
            }),
            ControlFlow::Return(v) => Ok(v),
            _ => Err(RuntimeError::UnhandledControlFlow.span(&span)),
        },
    }
}

fn interpret_args(
    args: &Vec<LocatedExpression>,
    env: Rc<Environment>,
) -> Result<Vec<Value>, LocatedControlFlow> {
    let mut arg_vals = Vec::new();
    for arg in args {
        let val = interpret(arg, env.clone())?;
        arg_vals.push(val);
    }
    Ok(arg_vals)
}

fn instantiate_prototype(
    p: HashMap<String, Value>,
    constructor: Rc<FunctionValue>,
) -> BuiltinFunctionType {
    Rc::new(move |args, span, _| {
        let property_map = p.clone();
        let this = Value::Object(Rc::new(RefCell::new(property_map)));
        let FunctionValue {
            parameters,
            body,
            env: func_env,
        } = &*constructor;
        let arg_count = args.len() + 1;
        if parameters.len() != arg_count {
            return Err(RuntimeError::FunctionArgumentError(format!(
                "Expected {} arguments, got {}",
                parameters.len(),
                arg_count
            ))
            .span(&span));
        }
        let args_iter = std::iter::once(this.clone()).chain(args.into_iter());
        let call_env = Rc::new(Environment::new(func_env.clone()));
        for (param, arg_val) in parameters.iter().zip(args_iter) {
            if !call_env.define(param, arg_val) {
                unreachable!("Parameter name should not be duplicated: {}", param);
            }
        }
        interpret(&body, call_env)?;
        Ok(this)
    })
}

fn import_module(
    module_relative_path: &str,
    span: &Location,
    env: Rc<Environment>,
) -> Result<Value, LocatedControlFlow> {
    let parts = module_relative_path.split('/').collect::<Vec<_>>();
    let mut module_path = env
        .module_path
        .split('/')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>();
    module_path.pop();
    for part in parts.into_iter() {
        match part {
            "." => continue,
            ".." => {
                module_path.pop();
                continue;
            }
            "" => continue,
            _ => module_path.push(part),
        }
    }
    let module_path = module_path.join("/");

    let prev_val = GLOBAL_STATE.with(|state| {
        let mut state = state.get().unwrap().borrow_mut();
        let prev = state.modules.get(&module_path).cloned();
        match prev {
            Some(Some(v)) => return Ok(Some(v)),
            Some(None) => {
                return Err(RuntimeError::ImportError(format!(
                    "Cyclic import detected for module '{}'",
                    module_path
                ))
                .span(&span));
            }
            None => {}
        }
        state.modules.insert(module_path.clone(), None);
        Ok(None)
    })?;
    if let Some(v) = prev_val {
        return Ok(v);
    }

    let pwd = GLOBAL_STATE.with(|state| state.get().unwrap().borrow().pwd.clone());
    let path = pwd.join(format!("{}.yuzu", module_path));
    let mut file = match std::fs::File::open(&path) {
        Ok(f) => f,
        Err(e) => {
            return Err(RuntimeError::ImportError(format!(
                "Failed to open file {}: {}",
                path.display(),
                e
            ))
            .span(&span));
        }
    };
    let mut buf = String::new();
    if let Err(e) = file.read_to_string(&mut buf) {
        eprintln!("Error reading file {}: {}", path.display(), e);
        return Err(RuntimeError::ImportError("Failed to read file".to_string()).span(&span));
    }

    let val = interpret_string(&buf, span, module_path.clone(), true)?;
    GLOBAL_STATE.with(|state| {
        let mut state = state.get().unwrap().borrow_mut();
        state.modules.insert(module_path, Some(val.clone()));
    });
    Ok(val)
}

fn interpret_string(
    input: &str,
    span: &Location,
    module_path: String,
    with_std: bool,
) -> Result<Value, LocatedControlFlow> {
    let parsed = parse_string(input)
        .map_err(|_| RuntimeError::ImportError("Failed to parse input".to_string()).span(&span))?;
    let add_file_info = |extra| Location::new(extra, module_path.clone());
    let parsed = parsed.map_extra(&add_file_info);
    let interpreted = interpret_global(&parsed, module_path, with_std);
    interpreted.map_err(|e| e.data.span(&e.location))
}
