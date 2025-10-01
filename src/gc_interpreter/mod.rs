use std::{collections::HashMap, io::Read, vec};

use gc_arena::{
    Arena, Collect, Gc, Mutation, Rootable, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    gc_interpreter::{
        exception::{
            array_index_out_of_bounds, duplicate_variable_definition, field_access_error,
            import_error, type_error, undefined_variable, unhandled_control_flow,
        },
        standard::{define_globals, root_prototypes},
        value::{
            ClassInstanceValue, ClassValue, ControlFlow, Environment, FunctionValue,
            LocatedControlFlow, StringVariant, Value, ValueClasses, variable_to_string,
        },
    },
    location::{Located, Location},
    parse_string,
    parser::{BinaryOp, Expression, Extra, MemberKind, UnaryOp},
};

mod exception;
mod resource;
pub mod standard;
mod value;

#[derive(Collect)]
#[collect(no_drop)]
pub struct MyRoot<'a> {
    modules: GcRefLock<'a, HashMap<String, ModuleRoot<'a>>>,
    module_values: GcRefLock<'a, HashMap<String, Option<Value<'a>>>>,
    value_classes: ValueClasses<'a>,
    std_export: GcRefLock<'a, Option<GcRefLock<'a, HashMap<String, Value<'a>>>>>,
    pwd: std::path::PathBuf,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ModuleRoot<'a> {
    env: Environment<'a>,
    expr: Gc<'a, StaticCollect<LocatedExpression>>,
}

type LocatedExpression = Extra<Location>;

pub fn interpret_global(
    expr: LocatedExpression,
    module_path: String,
    pwd: std::path::PathBuf,
) -> Result<(), Located<String>> {
    let arena = Arena::<Rootable![MyRoot<'_>]>::new(|mc| MyRoot {
        modules: Gc::new(mc, RefLock::new(HashMap::new())),
        module_values: Gc::new(mc, RefLock::new(HashMap::new())),
        value_classes: root_prototypes(mc),
        std_export: Gc::new(mc, RefLock::new(None)),
        pwd,
    });
    arena.mutate(
        |mc, root| match interpret(mc, root, expr, module_path, false) {
            Ok(_) => Ok(()),
            Err(e) => Err(Located {
                data: format!(
                    "Uncaught exception: {:?}",
                    match e.data {
                        ControlFlow::Break => "<break>".to_string(),
                        ControlFlow::Continue => "<continue>".to_string(),
                        ControlFlow::Error(v) => variable_to_string(&v),
                    }
                ),
                location: e.location.clone(),
            }),
        },
    )?;
    Ok(())
}

fn interpret_string<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    input: &str,
    span: &Location,
    module_path: String,
    file_path: Option<&str>,
    env: Gc<'a, Environment<'a>>,
    no_std: bool,
) -> Result<Value<'a>, LocatedControlFlow<'a>> {
    let parsed = parse_string(input, file_path).map_err(|_| {
        import_error::<()>(mc, root, "Failed to parse input", &env, span).unwrap_err()
    })?;
    let add_file_info = |extra| Location::new(extra, module_path.clone());
    let parsed = parsed.map_extra(&add_file_info);
    interpret(mc, root, parsed, module_path, no_std)
}

pub enum Frame<'a> {
    Eval(&'a LocatedExpression),
    Kont(Located<Kont<'a>>),
}

pub struct EnvFrame<'a> {
    pub frame: Frame<'a>,
    pub env: Gc<'a, Environment<'a>>,
}

impl<'a> Frame<'a> {
    pub fn kont(kont: Kont<'a>, env: Gc<'a, Environment<'a>>, location: Location) -> EnvFrame<'a> {
        EnvFrame {
            frame: Frame::Kont(Located {
                data: kont,
                location,
            }),
            env,
        }
    }
    pub fn eval(expr: &'a LocatedExpression, env: Gc<'a, Environment<'a>>) -> EnvFrame<'a> {
        EnvFrame {
            frame: Frame::Eval(expr),
            env,
        }
    }
}

pub enum Kont<'a> {
    Return,
    Raise,
    Define(String),
    Block {
        exprs_remaining: Vec<&'a LocatedExpression>,
        last_value: Option<&'a LocatedExpression>,
    },
    Binary {
        op: &'a BinaryOp,
        right: &'a LocatedExpression,
    },
    Unary {
        op: &'a UnaryOp,
    },
    BinaryCombine {
        op: &'a BinaryOp,
        left: Value<'a>,
    },
    FunctionCall {
        args_remaining: Vec<&'a LocatedExpression>,
        args: Vec<Value<'a>>,
        function: Option<Value<'a>>,
    },
    ObjectLiteral {
        entries_remaining: Vec<(String, &'a LocatedExpression)>,
        map: HashMap<String, Value<'a>>,
        current_key: String,
    },
    ArrayLiteral {
        entries_remaining: Vec<&'a LocatedExpression>,
        elements: Vec<Value<'a>>,
    },
    ClassLiteral {
        properties_remaining: Vec<((String, &'a MemberKind), &'a LocatedExpression)>,
        map: HashMap<String, (&'a MemberKind, Value<'a>)>,
        current_key: Option<(String, &'a MemberKind)>,
        parent: Option<Value<'a>>,
    },
    FieldAccess {
        field: String,
    },
    ArrayAccess {
        index: &'a LocatedExpression,
        array: Option<Value<'a>>,
    },
    PropertyFunctionCall {
        args_remaining: Vec<&'a LocatedExpression>,
        args: Vec<Value<'a>>,
        function_field: String,
        target: Option<Value<'a>>,
    },
    ConstructorCall {
        args_remaining: Vec<&'a LocatedExpression>,
        args: Vec<Value<'a>>,
        function: Option<Value<'a>>,
    },
    ReturnBarrier,
    Assign(&'a LocatedExpression),
    FieldAssign(Value<'a>, String),
    ArrayAssign {
        value: Value<'a>,
        index: &'a LocatedExpression,
        array: Option<Value<'a>>,
    },
    Set(ExecResult<'a>),
    IfElse {
        then_branch: &'a LocatedExpression,
        else_branch: Option<&'a LocatedExpression>,
    },
    Loop {
        condition: &'a LocatedExpression,
        body: &'a LocatedExpression,
        increment: Option<&'a LocatedExpression>,
        state: LoopState,
    },
}

pub enum LoopState {
    Init,
    Condition,
    Body,
    Increment,
}

pub enum ExecResult<'a> {
    Value(Value<'a>),
    Return(Value<'a>),
    Error(Value<'a>, Location),
    Break(Location),
    Continue(Location),
}

impl Default for ExecResult<'_> {
    fn default() -> Self {
        ExecResult::Value(Value::Null)
    }
}

fn convert_err<'a>(val: Result<Value<'a>, LocatedControlFlow<'a>>) -> ExecResult<'a> {
    match val {
        Ok(v) => ExecResult::Value(v),
        Err(LocatedControlFlow { data, location }) => match data {
            ControlFlow::Error(e) => ExecResult::Error(e, location),
            ControlFlow::Break => ExecResult::Break(location),
            ControlFlow::Continue => ExecResult::Continue(location),
        },
    }
}

pub fn interpret<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    expr: LocatedExpression,
    module_path: String,
    no_std: bool,
) -> Result<Value<'a>, LocatedControlFlow<'a>> {
    let env = Gc::new(mc, Environment::new_global(mc, module_path.clone()));
    define_globals(mc, root, env, no_std);
    let expr = Gc::new(mc, StaticCollect(expr));
    let module_root = ModuleRoot {
        env: env.as_ref().clone(),
        expr,
    };
    let mut modules = root.modules.borrow_mut(mc);
    modules.insert(module_path.clone(), module_root);
    let do_processing = || {
        let mut stack: Vec<EnvFrame<'a>> = vec![Frame::eval(expr.as_ref(), env)];
        let mut last: ExecResult<'a> = ExecResult::Value(Value::Null);
        while let Some(frame) = stack.pop() {
            match frame.frame {
                Frame::Eval(expr) => {
                    interpret_frame_eval(mc, root, &mut stack, &mut last, &expr, frame.env);
                }
                Frame::Kont(kont) => {
                    interpret_kont_eval(
                        mc,
                        root,
                        &mut stack,
                        &mut last,
                        kont.data,
                        kont.location,
                        frame.env,
                    );
                }
            }
        }
        match last {
            ExecResult::Value(v) => Ok(v),
            ExecResult::Return(v) => Ok(v),
            ExecResult::Error(v, location) => Err(LocatedControlFlow {
                data: ControlFlow::Error(v),
                location,
            }),
            ExecResult::Break(location) => Err(LocatedControlFlow {
                data: ControlFlow::Break,
                location,
            }),
            ExecResult::Continue(location) => Err(LocatedControlFlow {
                data: ControlFlow::Continue,
                location,
            }),
        }
    };
    handle_return_control_flow(mc, root, do_processing(), &env)
}

fn interpret_frame_eval<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    stack: &mut Vec<EnvFrame<'a>>,
    last: &mut ExecResult<'a>,
    expr: &'a LocatedExpression,
    env: Gc<'a, Environment<'a>>,
) {
    let location = &expr.extra;
    match &expr.expr {
        Expression::Null => {
            *last = ExecResult::Value(Value::Null);
        }
        Expression::Bool(b) => {
            *last = ExecResult::Value(Value::Bool(*b));
        }
        Expression::Number(n) => {
            *last = ExecResult::Value(Value::Number(*n));
        }
        Expression::Integer(i) => {
            *last = ExecResult::Value(Value::Integer(*i));
        }
        Expression::String(s) => {
            *last = ExecResult::Value(Value::String(Gc::new(mc, StringVariant::from_string(s))));
        }
        Expression::Break => {
            *last = ExecResult::Break(location.clone());
        }
        Expression::Continue => {
            *last = ExecResult::Continue(location.clone());
        }
        Expression::Return(inner_expr) => {
            if let Some(inner_expr) = inner_expr {
                stack.push(Frame::kont(Kont::Return, env, location.clone()));
                stack.push(Frame::eval(inner_expr, env));
            } else {
                *last = ExecResult::Return(Value::Null);
            }
        }
        Expression::Define(name, expr) => {
            stack.push(Frame::kont(
                Kont::Define(name.clone()),
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(expr, env));
        }
        Expression::Ident(name) => {
            if let Some(val) = env.get(name) {
                *last = ExecResult::Value(val);
            } else {
                *last = convert_err(undefined_variable(mc, root, name, &env, expr));
            }
        }
        Expression::FunctionLiteral { parameters, body } => {
            *last = ExecResult::Value(Value::Function(Gc::new(
                mc,
                RefLock::new(FunctionValue::Function {
                    parameters: parameters.clone(),
                    body: Gc::new(mc, StaticCollect(*body.clone())),
                    env,
                }),
            )));
        }
        Expression::ObjectLiteral(entries) => {
            if entries.is_empty() {
                *last = ExecResult::Value(Value::Object(Gc::new(mc, RefLock::new(HashMap::new()))));
            } else {
                let mut entries_remaining = entries
                    .iter()
                    .map(|(k, v)| (k.to_string(), v))
                    .rev()
                    .collect::<Vec<_>>();
                let (first_key, first_entry) = entries_remaining
                    .pop()
                    .expect("Entries should never be empty");
                stack.push(Frame::kont(
                    Kont::ObjectLiteral {
                        entries_remaining,
                        map: HashMap::new(),
                        current_key: first_key,
                    },
                    env,
                    location.clone(),
                ));
                stack.push(Frame::eval(first_entry, env));
            }
        }
        Expression::ClassLiteral { parent, properties } => {
            if properties.is_empty() && parent.is_none() {
                *last = ExecResult::Value(Value::Class(Gc::new(
                    mc,
                    RefLock::new(ClassValue {
                        static_fields: HashMap::new(),
                        instance_fields: HashMap::new(),
                        methods: HashMap::new(),
                        static_methods: HashMap::new(),
                        constructor: None,
                        parent: None,
                    }),
                )));
            } else {
                let mut entries_remaining = properties
                    .iter()
                    .map(|(k, kind, v)| ((k.to_string(), kind), v))
                    .rev()
                    .collect::<Vec<_>>();
                if let Some(parent) = parent {
                    stack.push(Frame::kont(
                        Kont::ClassLiteral {
                            properties_remaining: entries_remaining,
                            map: HashMap::new(),
                            current_key: None,
                            parent: None,
                        },
                        env.clone(),
                        location.clone(),
                    ));
                    stack.push(Frame::eval(parent, env));
                } else {
                    let (first_key, first_entry) = entries_remaining
                        .pop()
                        .expect("Entries should never be empty");
                    stack.push(Frame::kont(
                        Kont::ClassLiteral {
                            properties_remaining: entries_remaining,
                            map: HashMap::new(),
                            current_key: Some(first_key),
                            parent: None,
                        },
                        env.clone(),
                        location.clone(),
                    ));
                    stack.push(Frame::eval(first_entry, env));
                }
            }
        }
        Expression::Block(exprs, last_expr) => {
            if exprs.is_empty() && last_expr.is_none() {
                *last = ExecResult::Value(Value::Null);
            } else {
                let mut exprs_remaining = exprs.iter().rev().collect::<Vec<_>>();
                let block_env = Gc::new(mc, Environment::new(mc, env));
                let first_expr = exprs_remaining
                    .pop()
                    .unwrap_or_else(|| last_expr.as_ref().unwrap());
                // No Kont here if the only expression is the last one, as the result of the block is the result of that expression
                if !exprs.is_empty() {
                    stack.push(Frame::kont(
                        Kont::Block {
                            exprs_remaining,
                            last_value: last_expr.as_deref(),
                        },
                        block_env,
                        location.clone(),
                    ));
                }
                stack.push(Frame::eval(first_expr, block_env));
            }
        }
        Expression::BinaryOp { op, left, right } => {
            if let BinaryOp::Assign = op {
                stack.push(Frame::kont(Kont::Assign(left), env, location.clone()));
                stack.push(Frame::eval(right, env));
                return;
            }
            let op = if let BinaryOp::AddAssign
            | BinaryOp::SubtractAssign
            | BinaryOp::MultiplyAssign
            | BinaryOp::DivideAssign = op
            {
                stack.push(Frame::kont(Kont::Assign(left), env, location.clone()));
                match op {
                    BinaryOp::AddAssign => &BinaryOp::Add,
                    BinaryOp::SubtractAssign => &BinaryOp::Subtract,
                    BinaryOp::MultiplyAssign => &BinaryOp::Multiply,
                    BinaryOp::DivideAssign => &BinaryOp::Divide,
                    _ => unreachable!(),
                }
            } else {
                op
            };
            stack.push(Frame::kont(
                Kont::Binary { op, right },
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(left, env));
        }
        Expression::UnaryOp { op, expr } => {
            stack.push(Frame::kont(Kont::Unary { op }, env, location.clone()));
            stack.push(Frame::eval(expr, env));
        }
        Expression::FunctionCall {
            function,
            arguments,
        } => {
            stack.push(Frame::kont(
                Kont::FunctionCall {
                    args_remaining: arguments.iter().rev().collect(),
                    args: vec![],
                    function: None,
                },
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(function, env));
        }
        Expression::FieldAccess { object, field } => {
            stack.push(Frame::kont(
                Kont::FieldAccess {
                    field: field.clone(),
                },
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(object, env));
        }
        Expression::ArrayIndex { array, index } => {
            stack.push(Frame::kont(
                Kont::ArrayAccess { index, array: None },
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(array, env));
        }
        Expression::PropertyFunctionCall {
            object,
            function,
            arguments,
        } => {
            stack.push(Frame::kont(
                Kont::PropertyFunctionCall {
                    args_remaining: arguments.iter().rev().collect(),
                    args: vec![],
                    target: None,
                    function_field: function.clone(),
                },
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(object, env));
        }
        Expression::New { expr, arguments } => {
            stack.push(Frame::kont(
                Kont::ConstructorCall {
                    args_remaining: arguments.iter().rev().collect(),
                    args: vec![],
                    function: None,
                },
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(expr, env));
        }
        Expression::ArrayLiteral(entries) => {
            if entries.is_empty() {
                *last = ExecResult::Value(Value::Array(Gc::new(mc, RefLock::new(vec![]))));
            } else {
                let mut entries_remaining = entries.iter().rev().collect::<Vec<_>>();
                let first_entry = entries_remaining
                    .pop()
                    .expect("Entries should never be empty");
                stack.push(Frame::kont(
                    Kont::ArrayLiteral {
                        entries_remaining,
                        elements: vec![],
                    },
                    env,
                    location.clone(),
                ));
                stack.push(Frame::eval(first_entry, env));
            }
        }
        Expression::Raise(expr) => {
            stack.push(Frame::kont(Kont::Raise, env, location.clone()));
            stack.push(Frame::eval(expr, env));
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => {
            stack.push(Frame::kont(
                Kont::IfElse {
                    then_branch,
                    else_branch: else_branch.as_deref(),
                },
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(condition, env));
        }
        Expression::Loop {
            init,
            condition,
            increment,
            body,
        } => {
            let loop_env = Gc::new(mc, Environment::new(mc, env));
            stack.push(Frame::kont(
                Kont::Loop {
                    state: if init.is_none() {
                        LoopState::Condition
                    } else {
                        LoopState::Init
                    },
                    condition,
                    increment: increment.as_deref(),
                    body,
                },
                loop_env,
                location.clone(),
            ));
            if let Some(init) = init {
                stack.push(Frame::eval(init, loop_env));
            } else {
                stack.push(Frame::eval(condition, loop_env));
            }
        }
        _ => unimplemented!("Expression not implemented: {:?}", expr.expr),
    }
}

fn maybe_take_last_value<'a>(last: &mut ExecResult<'a>) -> Option<Value<'a>> {
    let ExecResult::Value(_) = last else {
        return None;
    };
    let ExecResult::Value(val) = std::mem::take(last) else {
        unreachable!();
    };
    Some(val)
}

fn interpret_kont_eval<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    stack: &mut Vec<EnvFrame<'a>>,
    last: &mut ExecResult<'a>,
    kont: Kont<'a>,
    location: Location,
    env: Gc<'a, Environment<'a>>,
) {
    match kont {
        Kont::Set(value) => {
            let Some(_) = maybe_take_last_value(last) else {
                return;
            };
            *last = value;
        }
        // If result of expr is not a value, propagate it upwards, otherwise convert to return
        Kont::Return => {
            let Some(last_value) = maybe_take_last_value(last) else {
                return;
            };
            *last = ExecResult::Return(last_value);
        }
        Kont::Raise => {
            let Some(last_value) = maybe_take_last_value(last) else {
                return;
            };
            *last = ExecResult::Error(last_value, location);
        }
        Kont::ReturnBarrier => {
            let ExecResult::Return(_) = last else {
                return;
            };
            let ExecResult::Return(val) = std::mem::take(last) else {
                unreachable!();
            };
            *last = ExecResult::Value(val);
        }
        // If last expr was not a value, propagate it upwards, otherwise continue with next expr
        Kont::Block {
            mut exprs_remaining,
            last_value,
        } => {
            // Ignore values of exprs that are not the last one
            let Some(_) = maybe_take_last_value(last) else {
                return;
            };
            if let Some(next_expr) = exprs_remaining.pop() {
                stack.push(Frame::kont(
                    Kont::Block {
                        exprs_remaining,
                        last_value,
                    },
                    env,
                    location,
                ));
                stack.push(Frame::eval(next_expr, env));
            } else if let Some(last_expr) = last_value {
                // No Kont here as result of last expr is result of block
                stack.push(Frame::eval(last_expr, env));
            } else {
                *last = ExecResult::Value(Value::Null);
            }
        }
        Kont::ObjectLiteral {
            mut entries_remaining,
            mut map,
            current_key,
        } => {
            let Some(last_value) = maybe_take_last_value(last) else {
                return;
            };
            map.insert(current_key.clone(), last_value);

            if let Some((next_key, next_expr)) = entries_remaining.pop() {
                stack.push(Frame::kont(
                    Kont::ObjectLiteral {
                        entries_remaining,
                        map,
                        current_key: next_key,
                    },
                    env,
                    location,
                ));
                stack.push(Frame::eval(next_expr, env));
            } else {
                *last = ExecResult::Value(Value::Object(Gc::new(mc, RefLock::new(map))));
            }
        }
        Kont::ClassLiteral {
            mut properties_remaining,
            mut map,
            current_key,
            parent,
        } => {
            let Some(last_value) = maybe_take_last_value(last) else {
                return;
            };
            let parent = if let Some((current_key, kind)) = current_key {
                map.insert(current_key, (kind, last_value));
                parent
            } else {
                let Value::Class(_) = last_value else {
                    *last = convert_err(type_error(
                        mc,
                        root,
                        "Superclass must be a prototype",
                        &env,
                        &location,
                    ));
                    return;
                };
                Some(last_value)
            };

            if let Some((next_key, next_expr)) = properties_remaining.pop() {
                stack.push(Frame::kont(
                    Kont::ClassLiteral {
                        properties_remaining,
                        map,
                        current_key: Some(next_key),
                        parent,
                    },
                    env,
                    location,
                ));
                stack.push(Frame::eval(next_expr, env));
            } else {
                let prototype = Gc::new(
                    mc,
                    RefLock::new(ClassValue {
                        static_fields: HashMap::new(),
                        instance_fields: HashMap::new(),
                        constructor: None,
                        methods: HashMap::new(),
                        static_methods: HashMap::new(),
                        parent: parent.map(|x| {
                            if let Value::Class(p) = x {
                                p
                            } else {
                                unreachable!();
                            }
                        }),
                    }),
                );
                map_prototype_functions(mc, map, prototype);
                *last = ExecResult::Value(Value::Class(prototype));
            }
        }
        Kont::ArrayLiteral {
            mut entries_remaining,
            mut elements,
        } => {
            let Some(last_value) = maybe_take_last_value(last) else {
                return;
            };
            elements.push(last_value);

            if let Some(next_expr) = entries_remaining.pop() {
                stack.push(Frame::kont(
                    Kont::ArrayLiteral {
                        entries_remaining,
                        elements,
                    },
                    env,
                    location,
                ));
                stack.push(Frame::eval(next_expr, env));
            } else {
                *last = ExecResult::Value(Value::Array(Gc::new(mc, RefLock::new(elements))));
            }
        }
        // If last expr was not a value, propagate it upwards, otherwise define variable and return null or error
        Kont::Define(name) => {
            let Some(last_value) = maybe_take_last_value(last) else {
                return;
            };
            *last = if env.define(mc, &name, last_value) {
                ExecResult::Value(Value::Null)
            } else {
                convert_err(duplicate_variable_definition(
                    mc, root, &name, &env, location,
                ))
            };
        }
        Kont::Binary { op, right } => {
            let Some(left) = maybe_take_last_value(last) else {
                return;
            };
            match interpret_short_circuit(op, left) {
                Ok(result) => {
                    *last = result;
                }
                Err(left) => {
                    stack.push(Frame::kont(Kont::BinaryCombine { op, left }, env, location));
                    stack.push(Frame::eval(right, env));
                }
            }
        }
        Kont::BinaryCombine { op, left } => {
            let Some(right) = maybe_take_last_value(last) else {
                return;
            };
            if let Some(result) = interpret_simple_binary_op(op, left, right) {
                *last = ExecResult::Value(result);
            } else {
                *last = convert_err(type_error(
                    mc,
                    root,
                    &format!("Invalid operand types for binary operator {:?}", op),
                    &env,
                    location,
                ));
            }
        }
        Kont::Unary { op } => {
            let Some(value) = maybe_take_last_value(last) else {
                return;
            };
            if let Some(result) = interpret_simple_unary_op(op, value) {
                *last = ExecResult::Value(result);
            } else {
                *last = convert_err(type_error(
                    mc,
                    root,
                    &format!("Invalid operand type for unary operator {:?}", op),
                    &env,
                    location,
                ));
            }
        }
        Kont::FunctionCall {
            mut args_remaining,
            mut args,
            function,
        } => {
            let Some(last_value) = maybe_take_last_value(last) else {
                return;
            };
            let function = if let Some(f) = function {
                args.push(last_value);
                f
            } else {
                last_value
            };

            if let Some(next_arg) = args_remaining.pop() {
                stack.push(Frame::kont(
                    Kont::FunctionCall {
                        args_remaining,
                        args,
                        function: Some(function),
                    },
                    env,
                    location,
                ));
                stack.push(Frame::eval(next_arg, env));
            } else {
                call_function(mc, root, stack, last, &location, env, args, &function);
            }
        }
        Kont::FieldAccess { field } => {
            let Some(object) = maybe_take_last_value(last) else {
                return;
            };
            *last = if let Some((value, _)) = get_property(root, &object, &field) {
                ExecResult::Value(value)
            } else {
                convert_err(field_access_error(mc, root, field, &env, location))
            };
        }
        Kont::ArrayAccess { index, array } => {
            let Some(val) = maybe_take_last_value(last) else {
                return;
            };
            let Some(array) = array else {
                stack.push(Frame::kont(
                    Kont::ArrayAccess {
                        index,
                        array: Some(val),
                    },
                    env,
                    location.clone(),
                ));
                stack.push(Frame::eval(index, env));
                return;
            };

            match array {
                Value::Array(arr) => {
                    let Value::Integer(i) = val else {
                        *last = convert_err(type_error(
                            mc,
                            root,
                            "Array index must be an integer",
                            &env,
                            &location,
                        ));
                        return;
                    };
                    let arr_ref = arr.borrow();
                    if let Ok(index) = TryInto::<usize>::try_into(i)
                        && index < arr_ref.len()
                    {
                        let value = arr_ref[index].clone();
                        *last = ExecResult::Value(value);
                    } else {
                        *last =
                            convert_err(array_index_out_of_bounds(mc, root, i, &env, &location));
                        return;
                    }
                }
                Value::TypedSlice {
                    buffer,
                    start,
                    length,
                    buffer_type,
                } => {
                    let Value::Integer(i) = val else {
                        *last = convert_err(type_error(
                            mc,
                            root,
                            "Array index must be an integer",
                            &env,
                            &location,
                        ));
                        return;
                    };
                    let byte_start = start * buffer_type.byte_size();
                    let byte_end = byte_start + length * buffer_type.byte_size();
                    let arr_ref = buffer.borrow();
                    if let Ok(index) = TryInto::<usize>::try_into(i)
                        && index < arr_ref.len()
                    {
                        let byte_offset = byte_start + index * buffer_type.byte_size();
                        let Some(value) =
                            buffer_type.try_read_value(&arr_ref[byte_offset..byte_end])
                        else {
                            *last = convert_err(type_error(
                                mc,
                                root,
                                "Failed to read value from typed slice",
                                &env,
                                &location,
                            ));
                            return;
                        };
                        *last = ExecResult::Value(value);
                    } else {
                        *last =
                            convert_err(array_index_out_of_bounds(mc, root, i, &env, &location));
                        return;
                    }
                }
                _ => {
                    *last = convert_err(type_error(
                        mc,
                        root,
                        "Attempted to index a non-array value",
                        &env,
                        &location,
                    ));
                    return;
                }
            }
        }
        Kont::PropertyFunctionCall {
            mut args_remaining,
            mut args,
            function_field,
            target,
        } => {
            let Some(val) = maybe_take_last_value(last) else {
                return;
            };

            let target = if let Some(target) = target {
                args.push(val);
                target
            } else {
                val
            };
            if let Some(next_arg) = args_remaining.pop() {
                stack.push(Frame::kont(
                    Kont::PropertyFunctionCall {
                        args_remaining,
                        args,
                        function_field: function_field,
                        target: Some(target),
                    },
                    env,
                    location,
                ));
                stack.push(Frame::eval(next_arg, env));
            } else {
                let Some((func, kind)) = get_property(root, &target, &function_field) else {
                    *last = convert_err(field_access_error(
                        mc,
                        root,
                        function_field,
                        &env,
                        &location,
                    ));
                    return;
                };
                if let MemberKind::Method = kind {
                    args.insert(0, target);
                }
                call_function(mc, root, stack, last, &location, env, args, &func);
            }
        }
        Kont::ConstructorCall {
            mut args_remaining,
            mut args,
            function,
        } => {
            let Some(last_value) = maybe_take_last_value(last) else {
                return;
            };
            let function = if let Some(f) = function {
                args.push(last_value);
                f
            } else {
                last_value
            };

            if let Some(next_arg) = args_remaining.pop() {
                stack.push(Frame::kont(
                    Kont::ConstructorCall {
                        args_remaining,
                        args,
                        function: Some(function),
                    },
                    env,
                    location,
                ));
                stack.push(Frame::eval(next_arg, env));
            } else {
                let Value::Class(class) = function else {
                    *last = convert_err(type_error(
                        mc,
                        root,
                        "Attempted to call a non-prototype value with 'new'",
                        &env,
                        &location,
                    ));
                    return;
                };
                let class_ref = class.borrow();
                let function = if let Some(constructor) = class_ref.constructor {
                    Value::Function(constructor)
                } else {
                    *last = convert_err(type_error(
                        mc,
                        root,
                        "Attempted to call a prototype without a constructor with 'new'",
                        &env,
                        &location,
                    ));
                    return;
                };

                let instance = create_class_instance(mc, class);
                args.insert(0, instance.clone());

                stack.push(Frame::kont(
                    Kont::Set(ExecResult::Value(instance)),
                    env.clone(),
                    location.clone(),
                ));
                call_function(mc, root, stack, last, &location, env, args, &function);
            }
        }
        Kont::Assign(target) => {
            let Some(value) = maybe_take_last_value(last) else {
                return;
            };
            match &target.expr {
                Expression::Ident(name) => {
                    if env.set(mc, &name, value.clone()) {
                        *last = ExecResult::Value(value);
                    } else {
                        *last = convert_err(undefined_variable(mc, root, &name, &env, &location));
                    }
                }
                Expression::FieldAccess { object, field } => {
                    stack.push(Frame::kont(
                        Kont::FieldAssign(value, field.clone()),
                        env,
                        location.clone(),
                    ));
                    stack.push(Frame::eval(object, env));
                }
                Expression::ArrayIndex { array, index } => {
                    stack.push(Frame::kont(
                        Kont::ArrayAssign {
                            value: value,
                            index,
                            array: None,
                        },
                        env,
                        location.clone(),
                    ));
                    stack.push(Frame::eval(array, env));
                }
                _ => {
                    *last = convert_err(type_error(
                        mc,
                        root,
                        "Invalid assignment target",
                        &env,
                        &location,
                    ));
                }
            }
        }
        Kont::FieldAssign(value, field) => {
            let Some(object) = maybe_take_last_value(last) else {
                return;
            };
            match object {
                Value::Object(obj) => {
                    obj.borrow_mut(mc).insert(field, value.clone());
                    *last = ExecResult::Value(value);
                }
                Value::ClassInstance(obj) => {
                    if let Some(field) = obj.borrow_mut(mc).fields.get_mut(&field) {
                        *field = value.clone();
                        *last = ExecResult::Value(value);
                    } else {
                        *last = convert_err(type_error(
                            mc,
                            root,
                            &format!("Field '{}' does not exist on class instance", field),
                            &env,
                            &location,
                        ));
                    }
                }
                _ => {
                    *last = convert_err(type_error(
                        mc,
                        root,
                        "Attempted to assign field on non-object value",
                        &env,
                        &location,
                    ));
                }
            }
        }
        Kont::ArrayAssign {
            value,
            index,
            array,
        } => {
            let Some(val) = maybe_take_last_value(last) else {
                return;
            };
            let Some(array) = array else {
                stack.push(Frame::kont(
                    Kont::ArrayAssign {
                        value,
                        index,
                        array: Some(val),
                    },
                    env,
                    location.clone(),
                ));
                stack.push(Frame::eval(index, env));
                return;
            };

            match array {
                Value::Array(arr) => {
                    let Value::Integer(i) = val else {
                        *last = convert_err(type_error(
                            mc,
                            root,
                            "Array index must be an integer",
                            &env,
                            &location,
                        ));
                        return;
                    };
                    let mut arr_ref = arr.borrow_mut(mc);
                    if let Ok(index) = TryInto::<usize>::try_into(i)
                        && index < arr_ref.len()
                    {
                        arr_ref[index] = value.clone();
                        *last = ExecResult::Value(value);
                    } else {
                        *last =
                            convert_err(array_index_out_of_bounds(mc, root, i, &env, &location));
                        return;
                    }
                }
                Value::TypedSlice {
                    buffer,
                    start,
                    length,
                    buffer_type,
                } => {
                    let Value::Integer(i) = val else {
                        *last = convert_err(type_error(
                            mc,
                            root,
                            "Array index must be an integer",
                            &env,
                            &location,
                        ));
                        return;
                    };
                    let mut buf_ref = buffer.borrow_mut(mc);
                    let byte_start = start * buffer_type.byte_size();
                    let byte_end = byte_start + length * buffer_type.byte_size();
                    if let Ok(index) = TryInto::<usize>::try_into(i)
                        && index < buf_ref.len()
                    {
                        let byte_offset = byte_start + index * buffer_type.byte_size();
                        if !buffer_type.try_write_value(&mut buf_ref[byte_offset..byte_end], &value)
                        {
                            *last = convert_err(type_error(
                                mc,
                                root,
                                "Failed to write value to typed slice",
                                &env,
                                &location,
                            ));
                            return;
                        }
                        *last = ExecResult::Value(value);
                    } else {
                        *last =
                            convert_err(array_index_out_of_bounds(mc, root, i, &env, &location));
                        return;
                    }
                }
                _ => {
                    *last = convert_err(type_error(
                        mc,
                        root,
                        "Attempted to index a non-array value",
                        &env,
                        &location,
                    ));
                    return;
                }
            }
        }
        Kont::IfElse {
            then_branch,
            else_branch,
        } => {
            let Some(condition) = maybe_take_last_value(last) else {
                return;
            };
            match condition {
                Value::Bool(true) => {
                    stack.push(Frame::eval(then_branch, env));
                }
                Value::Bool(false) => {
                    if let Some(else_branch) = else_branch {
                        stack.push(Frame::eval(else_branch, env));
                    } else {
                        *last = ExecResult::Value(Value::Null);
                    }
                }
                _ => {
                    *last = convert_err(type_error(
                        mc,
                        root,
                        "Condition in if expression must be a boolean",
                        &env,
                        &location,
                    ));
                }
            }
        }
        Kont::Loop {
            state,
            condition,
            increment,
            body,
        } => {
            let next_state = 'm: {
                match last {
                    ExecResult::Break(_) => {
                        *last = ExecResult::Value(Value::Null);
                        return;
                    }
                    ExecResult::Continue(_) => break 'm LoopState::Body,
                    _ => {}
                }
                match state {
                    LoopState::Init => {
                        let Some(_) = maybe_take_last_value(last) else {
                            return;
                        };
                        LoopState::Condition
                    }
                    LoopState::Condition => {
                        let Some(condition) = maybe_take_last_value(last) else {
                            return;
                        };
                        match condition {
                            Value::Bool(true) => LoopState::Body,
                            Value::Bool(false) => {
                                *last = ExecResult::Value(Value::Null);
                                return;
                            }
                            _ => {
                                *last = convert_err(type_error(
                                    mc,
                                    root,
                                    "Condition in loop expression must be a boolean",
                                    &env,
                                    &location,
                                ));
                                return;
                            }
                        }
                    }
                    LoopState::Body | LoopState::Increment => {
                        let Some(_) = maybe_take_last_value(last) else {
                            return;
                        };
                        if let LoopState::Body = state
                            && let Some(_) = increment
                        {
                            LoopState::Increment
                        } else {
                            LoopState::Condition
                        }
                    }
                }
            };
            let next_expr = match next_state {
                LoopState::Condition => condition,
                LoopState::Body => body,
                LoopState::Increment => increment.unwrap(),
                LoopState::Init => unreachable!(),
            };
            stack.push(Frame::kont(
                Kont::Loop {
                    state: next_state,
                    condition,
                    increment,
                    body,
                },
                env,
                location,
            ));
            stack.push(Frame::eval(next_expr, env));
        }
    }
}

fn call_function<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    stack: &mut Vec<EnvFrame<'a>>,
    last: &mut ExecResult<'a>,
    location: &Location,
    env: Gc<'a, Environment<'a>>,
    args: Vec<Value<'a>>,
    function: &Value<'a>,
) {
    let Value::Function(f) = function else {
        *last = convert_err(type_error(
            mc,
            root,
            "Attempted to call a non-function value",
            &env,
            location,
        ));
        return;
    };
    match *f.borrow() {
        FunctionValue::Builtin { ref func } => {
            // No call environment for builtin functions, as they won't define variables
            *last = convert_err(func(mc, root, args, location, env));
        }
        FunctionValue::Function {
            ref parameters,
            body,
            env: func_env,
        } => {
            let args_len = args.len();
            if args_len != parameters.len() {
                *last = convert_err(type_error(
                    mc,
                    root,
                    &format!("Expected {} arguments, got {}", parameters.len(), args_len),
                    &env,
                    location,
                ));
            } else {
                let call_env = Gc::new(mc, Environment::new(mc, func_env));
                for (param, arg) in parameters.iter().zip(args.into_iter()) {
                    call_env.define(mc, param, arg);
                }
                let body_ref = body.as_ref();
                stack.push(Frame::kont(
                    Kont::ReturnBarrier,
                    call_env.clone(),
                    location.clone(),
                ));
                stack.push(Frame::eval(body_ref, call_env));
            }
        }
    }
}

fn import_module<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    module_relative_path: &str,
    span: &Location,
    env: Gc<'a, Environment<'a>>,
) -> Result<Value<'a>, LocatedControlFlow<'a>> {
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

    let prev_val = {
        let mut state = root.module_values.borrow_mut(mc);
        match state.get(&module_path) {
            Some(Some(v)) => Some(v.clone()),
            Some(None) => {
                return import_error(
                    mc,
                    root,
                    &format!("Cyclic import detected for module '{}'", module_path),
                    &env,
                    span,
                );
            }
            None => {
                state.insert(module_path.clone(), None);
                None
            }
        }
    };

    if let Some(v) = prev_val {
        return Ok(v);
    }

    let path = root.pwd.join(format!("{}.yuzu", module_path));
    let mut file = match std::fs::File::open(&path) {
        Ok(f) => f,
        Err(e) => {
            return import_error(
                mc,
                root,
                &format!("Failed to open file {}: {}", path.display(), e),
                &env,
                span,
            );
        }
    };
    let mut buf = String::new();
    if let Err(e) = file.read_to_string(&mut buf) {
        return import_error(mc, root, &format!("Failed to read file: {}", e), &env, span);
    }
    let file_path = path.to_string_lossy().to_string();

    let val = interpret_string(
        mc,
        root,
        &buf,
        span,
        module_path.clone(),
        Some(&file_path),
        env,
        false,
    )?;

    let mut state = root.module_values.borrow_mut(mc);
    state.insert(module_path, Some(val.clone()));

    Ok(val)
}

fn handle_return_control_flow<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    res: Result<Value<'a>, LocatedControlFlow<'a>>,
    env: &Environment<'a>,
) -> Result<Value<'a>, LocatedControlFlow<'a>> {
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
            _ => unhandled_control_flow(mc, root, &env, span),
        },
    }
}

fn interpret_short_circuit<'a>(
    op: &BinaryOp,
    left: Value<'a>,
) -> Result<ExecResult<'a>, Value<'a>> {
    Ok(match (op, &left) {
        (BinaryOp::And, Value::Bool(false)) => ExecResult::Value(Value::Bool(false)),
        (BinaryOp::Or, Value::Bool(true)) => ExecResult::Value(Value::Bool(true)),
        (BinaryOp::NullCoalesce, Value::Null) => return Err(left),
        (BinaryOp::NullCoalesce, _) => ExecResult::Value(left),
        _ => return Err(left),
    })
}

fn interpret_simple_unary_op<'a>(op: &UnaryOp, value: Value<'a>) -> Option<Value<'a>> {
    Some(match (op, value) {
        (UnaryOp::Negate, Value::Number(v)) => Value::Number(-v),
        (UnaryOp::Negate, Value::Integer(v)) => Value::Integer(-v),
        (UnaryOp::Not, Value::Integer(v)) => Value::Integer(!v),
        (UnaryOp::Not, Value::Bool(v)) => Value::Bool(!v),
        _ => return None,
    })
}

fn interpret_simple_binary_op<'a>(
    op: &BinaryOp,
    left: Value<'a>,
    right: Value<'a>,
) -> Option<Value<'a>> {
    Some(match (op, left, right) {
        (BinaryOp::Add, Value::Number(l), Value::Number(r)) => Value::Number(l + r),
        (BinaryOp::Add, Value::Integer(l), Value::Integer(r)) => Value::Integer(l + r),
        (BinaryOp::Add, Value::Integer(l), Value::Number(r)) => Value::Number(l as f64 + r),
        (BinaryOp::Add, Value::Number(l), Value::Integer(r)) => Value::Number(l + r as f64),

        (BinaryOp::Subtract, Value::Number(l), Value::Number(r)) => Value::Number(l - r),
        (BinaryOp::Subtract, Value::Integer(l), Value::Integer(r)) => Value::Integer(l - r),
        (BinaryOp::Subtract, Value::Integer(l), Value::Number(r)) => Value::Number(l as f64 - r),
        (BinaryOp::Subtract, Value::Number(l), Value::Integer(r)) => Value::Number(l - r as f64),

        (BinaryOp::Multiply, Value::Number(l), Value::Number(r)) => Value::Number(l * r),
        (BinaryOp::Multiply, Value::Integer(l), Value::Integer(r)) => Value::Integer(l * r),
        (BinaryOp::Multiply, Value::Integer(l), Value::Number(r)) => Value::Number(l as f64 * r),
        (BinaryOp::Multiply, Value::Number(l), Value::Integer(r)) => Value::Number(l * r as f64),

        (BinaryOp::Divide, Value::Number(l), Value::Number(r)) => Value::Number(l / r),
        (BinaryOp::Divide, Value::Integer(l), Value::Integer(r)) => Value::Integer(l / r),
        (BinaryOp::Divide, Value::Integer(l), Value::Number(r)) => Value::Number(l as f64 / r),
        (BinaryOp::Divide, Value::Number(l), Value::Integer(r)) => Value::Number(l / r as f64),

        (BinaryOp::And, Value::Bool(l), Value::Bool(r)) => Value::Bool(l && r),
        (BinaryOp::Or, Value::Bool(l), Value::Bool(r)) => Value::Bool(l || r),

        (BinaryOp::NullCoalesce, l, r) => {
            if let Value::Null = l {
                r
            } else {
                l
            }
        }

        (BinaryOp::Equal, l, r) => Value::Bool(l == r),
        (BinaryOp::NotEqual, l, r) => Value::Bool(l != r),

        (BinaryOp::Less, Value::Number(l), Value::Number(r)) => Value::Bool(l < r),
        (BinaryOp::Less, Value::Integer(l), Value::Integer(r)) => Value::Bool(l < r),
        (BinaryOp::LessEqual, Value::Number(l), Value::Number(r)) => Value::Bool(l <= r),
        (BinaryOp::LessEqual, Value::Integer(l), Value::Integer(r)) => Value::Bool(l <= r),

        (BinaryOp::Greater, Value::Number(l), Value::Number(r)) => Value::Bool(l > r),
        (BinaryOp::Greater, Value::Integer(l), Value::Integer(r)) => Value::Bool(l > r),
        (BinaryOp::GreaterEqual, Value::Number(l), Value::Number(r)) => Value::Bool(l >= r),
        (BinaryOp::GreaterEqual, Value::Integer(l), Value::Integer(r)) => Value::Bool(l >= r),

        _ => return None,
    })
}

fn get_class<'a>(root: &MyRoot<'a>, target: &Value<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    if let Value::ClassInstance(instance) = target {
        instance.borrow().class
    } else if let Value::Class(class) = target {
        *class
    } else {
        let classes = &root.value_classes;
        let value_class = match target {
            Value::Number(_) => classes.number,
            Value::Integer(_) => classes.integer,
            Value::Bool(_) => classes.bool,
            Value::String(_) => classes.string,
            Value::Null => classes.null,
            Value::Function { .. } => classes.function,
            Value::Array { .. } => classes.array,
            Value::Object { .. } => classes.object,
            Value::ClassInstance { .. } => classes.class_instance,
            Value::Class { .. } => classes.class,
            Value::Resource(_) => classes.resource,
            Value::Buffer(_) => classes.buffer,
            Value::TypedSlice { .. } => classes.typed_slice,
        };
        value_class
    }
}

fn get_property<'a>(
    root: &MyRoot<'a>,
    target: &Value<'a>,
    field: &str,
) -> Option<(Value<'a>, MemberKind)> {
    match target {
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            if let Some(val) = obj_ref.get(field).cloned() {
                return Some((val, MemberKind::Field));
            }
        }
        Value::ClassInstance(obj) => {
            let obj_ref = obj.borrow();
            if let Some(val) = obj_ref.fields.get(field).cloned() {
                return Some((val, MemberKind::Field));
            }
        }
        _ => {}
    }

    let class = get_class(root, target);

    fn look_for_property_in_class<'a>(
        mut class: GcRefLock<'a, ClassValue<'a>>,
        field: &str,
    ) -> Option<(Value<'a>, MemberKind)> {
        loop {
            let cur_ref = class.borrow();
            if let Some(val) = cur_ref.static_fields.get(field).cloned() {
                return Some((val, MemberKind::StaticField));
            } else if let Some(val) = cur_ref.methods.get(field) {
                return Some((Value::Function(*val), MemberKind::Method));
            } else if let Some(val) = cur_ref.static_methods.get(field) {
                return Some((Value::Function(*val), MemberKind::StaticMethod));
            }

            if let Some(parent) = &cur_ref.parent {
                let parent = parent.clone();
                drop(cur_ref);
                class = parent;
            } else {
                return None;
            }
        }
    }
    if let Some(res) = look_for_property_in_class(class, field) {
        Some(res)
    } else if let Value::ClassInstance(_) = target {
        look_for_property_in_class(root.value_classes.class_instance, field)
    } else if let Value::Class(_) = target {
        look_for_property_in_class(root.value_classes.class, field)
    } else {
        None
    }
}

fn map_prototype_functions<'a>(
    mc: &Mutation<'a>,
    map: HashMap<String, (&'a MemberKind, Value<'a>)>,
    prototype: GcRefLock<'a, ClassValue<'a>>,
) {
    let mut prot_ref = prototype.borrow_mut(mc);
    for (key, (kind, v)) in map.into_iter() {
        match kind {
            MemberKind::Field => {
                prot_ref.instance_fields.insert(key, v);
            }
            MemberKind::StaticField => {
                prot_ref.static_fields.insert(key, v);
            }
            MemberKind::Method => {
                let Value::Function(f) = v else {
                    //This shouldn't be possible because the parser only assigns functions as methods
                    eprintln!("Warning: Attempted to add non-function as method to prototype");
                    continue;
                };
                prot_ref.methods.insert(key.clone(), f);
            }
            MemberKind::StaticMethod => {
                let Value::Function(f) = v else {
                    //This shouldn't be possible because the parser only assigns functions as static methods
                    eprintln!(
                        "Warning: Attempted to add non-function as static method to prototype"
                    );
                    continue;
                };
                prot_ref.static_methods.insert(key.clone(), f);
            }
            MemberKind::Constructor => {
                let Value::Function(f) = v else {
                    //This shouldn't be possible because the parser only assigns functions as constructors
                    eprintln!("Warning: Attempted to add non-function as constructor to prototype");
                    continue;
                };
                prot_ref.constructor = Some(f);
            }
        }
    }
}

fn create_class_instance<'a>(mc: &Mutation<'a>, class: GcRefLock<'a, ClassValue<'a>>) -> Value<'a> {
    let mut fields = HashMap::new();
    let mut current_class = Some(class);
    while let Some(class) = current_class {
        let class_ref = class.borrow();
        for (key, value) in class_ref.instance_fields.iter() {
            if !fields.contains_key(key) {
                fields.insert(key.clone(), value.clone());
            }
        }
        current_class = class_ref.parent.clone();
    }
    Value::ClassInstance(Gc::new(
        mc,
        RefLock::new(ClassInstanceValue { class, fields }),
    ))
}
