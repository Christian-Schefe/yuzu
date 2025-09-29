use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    io::Read,
    path::PathBuf,
    rc::Rc,
};

use crate::{
    parse_string,
    parser::{BinaryOp, Expression, Extra, MemberKind, UnaryOp},
    tree_interpreter::standard::{define_globals, root_prototypes},
};

pub mod resource;
pub mod standard;
mod value;

pub use value::*;

#[derive(Clone, Debug)]
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

    pub fn root(&self) -> &Environment {
        let mut cur = self;
        while let Some(parent) = &cur.parent {
            cur = parent;
        }
        cur
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

fn make_exception(msg: &str, variant: &str, env: &Environment) -> Value {
    let p = if let Some(Value::Prototype(p)) = env.root().get(variant) {
        p
    } else {
        GLOBAL_STATE.with(|state| {
            state
                .get()
                .unwrap()
                .borrow()
                .root_prototypes
                .exception
                .clone()
        })
    };

    let mut map = HashMap::new();
    map.insert("message".to_string(), Value::String(msg.chars().collect()));
    Value::Object(Rc::new(RefCell::new(ObjectValue {
        properties: map,
        prototype: p,
    })))
}

pub fn runtime_error<T>(
    msg: &str,
    variant: &str,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    let exception = make_exception(msg, variant, env);
    Err(Located {
        data: ControlFlow::Error(exception),
        location: expr.location().clone(),
    })
}

pub fn type_error<T>(
    msg: &str,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error(msg, "TypeError", env, expr)
}

pub fn array_index_out_of_bounds<T>(
    index: i64,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error(
        &format!("Array index out of bounds: {}", index),
        "ArrayIndexOutOfBounds",
        env,
        expr,
    )
}

pub fn function_argument_error<T>(
    msg: &str,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error(msg, "FunctionArgumentError", env, expr)
}

pub fn io_error<T>(
    msg: &str,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error(msg, "IOError", env, expr)
}

pub fn unhandled_control_flow<T>(
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error("Unhandled control flow", "RuntimeError", env, expr)
}

pub fn field_access_error<T>(
    field: String,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error(
        &format!("Field access error: {}", field),
        "FieldAccessError",
        env,
        expr,
    )
}

pub fn undefined_variable<T>(
    name: &str,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error(
        &format!("Undefined variable: {}", name),
        "UndefinedVariable",
        env,
        expr,
    )
}

pub fn duplicate_variable_definition<T>(
    name: &str,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error(
        &format!("Duplicate variable definition: {}", name),
        "DuplicateVariableDefinition",
        env,
        expr,
    )
}

pub fn import_error<T>(
    msg: &str,
    env: &Environment,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow> {
    runtime_error(msg, "ImportError", env, expr)
}

pub enum ControlFlow {
    Return(Value),
    Break,
    Continue,
    Error(Value),
}

pub type LocatedControlFlow = Located<ControlFlow>;

#[derive(Clone, Debug)]
pub struct Located<T> {
    pub data: T,
    pub location: Location,
}

pub trait HasLocation {
    fn location(&self) -> &Location;
}

impl<T> HasLocation for Located<T> {
    fn location(&self) -> &Location {
        &self.location
    }
}

impl HasLocation for Location {
    fn location(&self) -> &Location {
        self
    }
}

impl<T> HasLocation for &T
where
    T: HasLocation,
{
    fn location(&self) -> &Location {
        (*self).location()
    }
}

impl HasLocation for Extra<Location> {
    fn location(&self) -> &Location {
        &self.extra
    }
}

impl<T> HasLocation for Box<T>
where
    T: HasLocation,
{
    fn location(&self) -> &Location {
        self.as_ref().location()
    }
}

fn variable_to_string(var: &Value) -> String {
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
        Value::Function { .. } => "<function>".to_string(),
        Value::Prototype(p) => {
            let entries = p
                .borrow()
                .properties
                .iter()
                .map(|(k, v)| format!("{}: {}", k, variable_to_string(&v.0)))
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

thread_local! {
    pub static GLOBAL_STATE: OnceCell<RefCell<GlobalState>> = OnceCell::new();
}

pub struct GlobalState {
    modules: HashMap<String, Option<Value>>,
    root_prototypes: RootPrototypes,
    pwd: PathBuf,
}

pub struct RootPrototypes {
    pub number: Rc<RefCell<PrototypeValue>>,
    pub string: Rc<RefCell<PrototypeValue>>,
    pub array: Rc<RefCell<PrototypeValue>>,
    pub object: Rc<RefCell<PrototypeValue>>,
    pub function: Rc<RefCell<PrototypeValue>>,
    pub builtin_function: Rc<RefCell<PrototypeValue>>,
    pub integer: Rc<RefCell<PrototypeValue>>,
    pub bool: Rc<RefCell<PrototypeValue>>,
    pub null: Rc<RefCell<PrototypeValue>>,
    pub prototype: Rc<RefCell<PrototypeValue>>,
    pub exception: Rc<RefCell<PrototypeValue>>,
    pub resource: Rc<RefCell<PrototypeValue>>,
    pub buffer: Rc<RefCell<PrototypeValue>>,
}

pub fn init_global_state(pwd: PathBuf) {
    GLOBAL_STATE.with(|state| {
        state.get_or_init(|| {
            RefCell::new(GlobalState {
                modules: HashMap::new(),
                root_prototypes: root_prototypes(),
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
) -> Result<Value, Located<Value>> {
    let global_env = Rc::new(Environment::new_global(module_path));
    define_globals(&*global_env, with_std);
    match interpret(expr, global_env.clone()) {
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
                data: make_exception("Unhandled control flow", "RuntimeError", &global_env),
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
            Ok(Value::Object(Rc::new(RefCell::new(ObjectValue {
                properties: map,
                prototype: GLOBAL_STATE
                    .with(|state| state.get().unwrap().borrow().root_prototypes.object.clone()),
            }))))
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
            let parent = if let Some(superclass) = superclass {
                let super_val = interpret(superclass, env.clone())?;
                let Value::Prototype(p) = super_val else {
                    return type_error("Superclass must be a prototype", &env, superclass);
                };
                Some(p)
            } else {
                None
            };

            let prototype = Rc::new(RefCell::new(PrototypeValue {
                properties: HashMap::new(),
                parent,
            }));
            let mut prototype_map = prototype.borrow_mut();
            for (name, kind, property) in properties {
                let val = interpret(property, env.clone())?;
                let kind = match kind {
                    MemberKind::Field => PropertyKind::Field,
                    MemberKind::Method => PropertyKind::Method,
                    MemberKind::Constructor => PropertyKind::Constructor(prototype.clone()),
                    MemberKind::StaticMethod => PropertyKind::StaticMethod,
                };
                prototype_map.properties.insert(name.clone(), (val, kind));
            }
            drop(prototype_map);
            Ok(Value::Prototype(prototype))
        }
        Expression::New(expr) => {
            let (obj_expr, field_name, args) = if let Expression::PropertyFunctionCall {
                object,
                function,
                arguments,
            } = &expr.expr
            {
                (object, function.as_str(), arguments)
            } else if let Expression::FunctionCall {
                function,
                arguments,
            } = &expr.expr
            {
                (function, "$constructor", arguments)
            } else {
                return type_error("Invalid constructor call", &env, expr);
            };

            let obj_val = interpret(obj_expr, env.clone())?;
            let Some((val, kind)) = do_property_access(obj_val.clone(), field_name) else {
                return field_access_error(
                    match field_name {
                        "$constructor" => "Default constructor".to_string(),
                        other => other.to_string(),
                    },
                    &env,
                    expr,
                );
            };
            let PropertyKind::Constructor(p) = kind else {
                return type_error("Constructor must be a constructor property", &env, expr);
            };
            let object_value = ObjectValue {
                properties: HashMap::new(),
                prototype: p.clone(),
            };
            let instance = Value::Object(Rc::new(RefCell::new(object_value)));
            let args = interpret_args(args, env.clone())?;
            do_function_call(val, &expr.extra, args, env, Some(instance.clone()))?;
            Ok(instance)
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
            Err(LocatedControlFlow {
                data: ControlFlow::Error(val),
                location: expr.extra.clone(),
            })
        }
        Expression::Ident(name) => {
            if let Some(val) = env.get(name) {
                Ok(val)
            } else {
                undefined_variable(name, &env, expr)
            }
        }
        Expression::Define(name, value) => {
            let val = interpret(value, env.clone())?;
            if env.define(name, val) {
                Ok(Value::Null)
            } else {
                duplicate_variable_definition(name, &env, expr)
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
                _ => type_error("Condition of if must be a boolean", &env, condition),
            }
        }
        Expression::TryCatch {
            try_block,
            exception_prototype,
            exception_var,
            catch_block,
        } => match interpret(try_block, env.clone()) {
            Ok(v) => Ok(v),
            Err(LocatedControlFlow {
                data: ControlFlow::Error(e),
                location,
            }) => {
                let catch_env = Rc::new(Environment::new(env.clone()));
                let matching = if let Some(proto) = exception_prototype {
                    let proto_val = interpret(proto, env.clone())?;
                    let Value::Prototype(target) = proto_val else {
                        return type_error("Exception prototype must be a prototype", &env, proto);
                    };
                    let mut current = get_prototype(&e);
                    loop {
                        if Rc::ptr_eq(&current, &target) {
                            break true;
                        }
                        let cur_ref = current.borrow();
                        if let Some(parent_ref) = &cur_ref.parent {
                            let parent = parent_ref.clone();
                            drop(cur_ref);
                            current = parent;
                        } else {
                            break false;
                        }
                    }
                } else {
                    true
                };
                if !matching {
                    return Err(LocatedControlFlow {
                        data: ControlFlow::Error(e),
                        location,
                    });
                }
                catch_env.define(exception_var, e);
                interpret(catch_block, catch_env)
            }
            Err(other) => Err(other),
        },
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
                _ => type_error("Invalid operand type for unary operator", &env, expr),
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
                let next_item = do_property_access(next, "value");
                let Some((next_item, _)) = next_item else {
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
            do_function_call(func_val, &function.extra, arg_vals, env.clone(), None)
        }
        Expression::PropertyFunctionCall {
            object,
            function,
            arguments,
        } => do_property_function_call(
            &env,
            &interpret(object, env.clone())?,
            &expr.extra,
            function,
            arguments,
        ),
        Expression::FieldAccess { object, field } => {
            do_property_access(interpret(object, env.clone())?, field)
                .ok_or_else(|| field_access_error::<()>(field.to_string(), &env, expr).unwrap_err())
                .map(|(val, _)| val)
        }
        Expression::ArrayIndex { array, index } => {
            let arr_val = interpret(array, env.clone())?;
            let idx_val = interpret(index, env.clone())?;
            match arr_val {
                Value::Array(elements) => {
                    let Value::Integer(i) = idx_val else {
                        return type_error("Array index must be an integer", &env, index);
                    };
                    let elements = elements.borrow();
                    if let Ok(i) = TryInto::<usize>::try_into(i)
                        && i < elements.len()
                    {
                        Ok(elements[i].clone())
                    } else {
                        array_index_out_of_bounds(i, &env, index)
                    }
                }
                Value::String(string) => {
                    let Value::Integer(i) = idx_val else {
                        return type_error("String index must be an integer", &env, index);
                    };
                    if let Ok(i) = TryInto::<usize>::try_into(i)
                        && i < string.len()
                    {
                        Ok(Value::String(vec![string[i]]))
                    } else {
                        array_index_out_of_bounds(i, &env, index)
                    }
                }
                Value::Object(_) | Value::Prototype(_) => {
                    let Value::String(key_chars) = idx_val else {
                        return type_error("Object index must be a string", &env, index);
                    };
                    let key = key_chars.iter().collect::<String>();
                    do_property_access(arr_val, key.as_str())
                        .ok_or_else(|| field_access_error::<()>(key, &env, expr).unwrap_err())
                        .map(|(val, _)| val)
                }
                _ => type_error("Cannot index into this value", &env, array),
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
    let Some((property_val, kind)) = do_property_access(obj_val.clone(), function) else {
        return field_access_error(function.to_string(), env, span);
    };
    let target = if let Value::Prototype(_) = obj_val {
        None
    } else if let PropertyKind::StaticMethod = kind {
        return type_error("Cannot call instance method on static", env, span);
    } else {
        Some(obj_val.clone())
    };
    do_function_call(property_val, &span, arg_vals, env.clone(), target)
}

fn do_property_access(object: Value, field: &str) -> Option<(Value, PropertyKind)> {
    let proto = match &object {
        Value::Object(object_map) => {
            let object_ref = object_map.borrow();
            if let Some(val) = object_ref.properties.get(field).cloned() {
                return Some((val, PropertyKind::Field));
            }
            Some(object_ref.prototype.clone())
        }
        Value::Prototype(p) => Some(p.clone()),
        _ => None,
    };
    if let Some(mut cur) = proto {
        loop {
            let cur_ref = cur.borrow();
            if let Some(val) = cur_ref.properties.get(field).cloned() {
                return Some(val);
            } else if let Some(parent) = &cur_ref.parent {
                let parent = parent.clone();
                drop(cur_ref);
                cur = parent;
            } else {
                break;
            }
        }
    }
    GLOBAL_STATE.with(|state| {
        let root = &state.get().unwrap().borrow().root_prototypes;
        let root_prot = match object {
            Value::Number(_) => &root.number,
            Value::Integer(_) => &root.integer,
            Value::Bool(_) => &root.bool,
            Value::String(_) => &root.string,
            Value::Null => &root.null,
            Value::Function { .. } => &root.function,
            Value::BuiltinFunction { .. } => &root.builtin_function,
            Value::Array { .. } => &root.array,
            Value::Object { .. } => &root.object,
            Value::Prototype { .. } => &root.prototype,
            Value::Resource(_) => &root.resource,
            Value::Buffer(_) => &root.buffer,
        };
        let root_ref = root_prot.borrow();
        root_ref.properties.get(field).cloned()
    })
}

fn get_prototype(object: &Value) -> Rc<RefCell<PrototypeValue>> {
    match &object {
        Value::Object(object_map) => return object_map.borrow().prototype.clone(),
        _ => {}
    };
    GLOBAL_STATE.with(|state| {
        let root = &state.get().unwrap().borrow().root_prototypes;
        let prot = match object {
            Value::Number(_) => &root.number,
            Value::Integer(_) => &root.integer,
            Value::Bool(_) => &root.bool,
            Value::String(_) => &root.string,
            Value::Null => &root.null,
            Value::Function { .. } => &root.function,
            Value::BuiltinFunction { .. } => &root.builtin_function,
            Value::Array { .. } => &root.array,
            Value::Object { .. } => &root.object,
            Value::Prototype { .. } => &root.prototype,
            Value::Resource(_) => &root.resource,
            Value::Buffer(_) => &root.buffer,
        };
        prot.clone()
    })
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
            return type_error(
                "Condition of for loop must be a boolean",
                &loop_env,
                condition,
            );
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
            left: impl FnOnce() -> Result<Value, LocatedControlFlow>,
            right: Value,
            env: &Rc<Environment>,
            location: &Location,
        ) -> Result<Value, LocatedControlFlow> {
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
                    let left = left()?;
                    interpret_simple_binary_op(&actual_op, left, right, location, env)
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
                            .ok_or_else(|| undefined_variable::<()>(name, env, left).unwrap_err())
                    },
                    val,
                    env,
                    left.location(),
                )?;
                if env.set(name, result.clone()) {
                    Ok(result)
                } else {
                    undefined_variable(name, env, left)
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
                                map.properties.get(field).cloned().ok_or_else(|| {
                                    field_access_error::<()>(field.clone(), env, left).unwrap_err()
                                })
                            },
                            val,
                            env,
                            left.location(),
                        )?;
                        let mut map = object_map.borrow_mut();
                        map.properties.insert(field.clone(), result.clone());
                        Ok(result)
                    }
                    _ => type_error("Left side of assignment must be an object", env, object),
                }
            }
            Expression::ArrayIndex { array, index } => {
                let arr_val = interpret(array, env.clone())?;
                let idx_val = interpret(index, env.clone())?;
                let Value::Array(elements) = arr_val else {
                    return type_error("Attempted to index a non-array value", env, array);
                };
                let Value::Integer(i) = idx_val else {
                    return type_error("Array index must be an integer", env, index);
                };
                if let Ok(i) = TryInto::<usize>::try_into(i) {
                    let val = interpret(right, env.clone())?;
                    let result = compute_result(
                        op,
                        || {
                            let elements = elements.borrow();
                            if i >= elements.len() {
                                return array_index_out_of_bounds(i as i64, env, left);
                            }
                            Ok(elements[i].clone())
                        },
                        val,
                        env,
                        left.location(),
                    )?;
                    let mut elements = elements.borrow_mut();
                    elements[i] = result.clone();
                    Ok(result)
                } else {
                    array_index_out_of_bounds(i, env, index)
                }
            }
            _ => type_error(
                "Left side of assignment must be a variable, field, or array index",
                env,
                left,
            ),
        }
    } else {
        let left_val = interpret(left, env.clone())?;
        match (op, &left_val) {
            (BinaryOp::And, Value::Bool(false)) => Ok(Value::Bool(false)),
            (BinaryOp::Or, Value::Bool(true)) => Ok(Value::Bool(true)),
            _ => {
                let right_val = interpret(right, env.clone())?;
                interpret_simple_binary_op(op, left_val, right_val, &left.location(), env)
            }
        }
    }
}

fn interpret_simple_binary_op(
    op: &BinaryOp,
    left: Value,
    right: Value,
    location: &Location,
    env: &Environment,
) -> Result<Value, LocatedControlFlow> {
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

        (BinaryOp::NullCoalesce, l, r) => match l {
            Value::Null => Ok(r),
            _ => Ok(l),
        },

        (BinaryOp::Less, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
        (BinaryOp::Less, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l < r)),
        (BinaryOp::LessEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
        (BinaryOp::LessEqual, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l <= r)),

        (BinaryOp::Greater, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
        (BinaryOp::Greater, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l > r)),
        (BinaryOp::GreaterEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
        (BinaryOp::GreaterEqual, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l >= r)),

        _ => type_error(
            &format!("Invalid operand types for binary operator {:?}", op),
            env,
            location,
        ),
    }
}

fn do_function_call(
    function_val: Value,
    span: &Location,
    arguments: Vec<Value>,
    env: Rc<Environment>,
    target: Option<Value>,
) -> Result<Value, LocatedControlFlow> {
    match function_val {
        Value::Function(f) => {
            let FunctionValue {
                parameters,
                body,
                env: func_env,
            } = &*f;

            let args_len = arguments.len() + if target.is_some() { 1 } else { 0 };
            if parameters.len() != args_len {
                return function_argument_error(
                    &format!("Expected {} arguments, got {}", parameters.len(), args_len),
                    &env,
                    span,
                );
            }
            let call_env = Rc::new(Environment::new(func_env.clone()));
            let args_iter = target.into_iter().chain(arguments.into_iter());
            for (param, arg) in parameters.iter().zip(args_iter) {
                if !call_env.define(param, arg) {
                    unreachable!("Parameter name should not be duplicated: {}", param);
                }
            }
            let result = interpret(&body, call_env);
            handle_return_control_flow(result, &env)
        }
        Value::BuiltinFunction(f) => {
            let BuiltinFunctionValue { func } = &*f;
            let args = target
                .into_iter()
                .chain(arguments.into_iter())
                .collect::<Vec<_>>();
            let result = func(args, span, env.clone());
            handle_return_control_flow(result, &env)
        }
        _ => type_error("Attempted to call a non-function value", &env, span),
    }
}

fn handle_return_control_flow(
    res: Result<Value, LocatedControlFlow>,
    env: &Environment,
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
            _ => unhandled_control_flow(&env, span),
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
                return import_error(
                    &format!("Cyclic import detected for module '{}'", module_path),
                    &env,
                    span,
                );
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
            return import_error(
                &format!("Failed to open file {}: {}", path.display(), e),
                &env,
                span,
            );
        }
    };
    let mut buf = String::new();
    if let Err(e) = file.read_to_string(&mut buf) {
        return import_error(&format!("Failed to read file: {}", e), &env, span);
    }
    let file_path = path.to_string_lossy().to_string();

    let val = interpret_string(
        &buf,
        span,
        module_path.clone(),
        true,
        Some(&file_path),
        env.clone(),
    )?;
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
    file_path: Option<&str>,
    env: Rc<Environment>,
) -> Result<Value, LocatedControlFlow> {
    let parsed = parse_string(input, file_path)
        .map_err(|_| import_error::<()>("Failed to parse input", &env, span).unwrap_err())?;
    let add_file_info = |extra| Location::new(extra, module_path.clone());
    let parsed = parsed.map_extra(&add_file_info);
    let interpreted = interpret_global(&parsed, module_path, with_std);
    interpreted.map_err(|e| Located {
        data: ControlFlow::Error(e.data),
        location: span.clone(),
    })
}
