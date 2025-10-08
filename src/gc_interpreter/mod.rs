use std::{collections::HashMap, vec};

use gc_arena::{
    Arena, Collect, Gc, Mutation, Rootable, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    ModulePath, ParsedProgram,
    gc_interpreter::{
        exception::{
            array_index_out_of_bounds, cyclic_static_initialization, division_by_zero,
            duplicate_variable_definition, field_access_error, type_error, undefined_variable,
            unhandled_control_flow, unsupported_binary_operation,
        },
        resolver::resolve_canonical_paths,
        value::{
            ClassInstanceValue, ClassValue, Environment, FunctionValue, IntVariant, LocatedError,
            ModuleTree, StaticValue, StringVariant, Value, variable_to_string,
        },
    },
    location::{Located, Location},
    parser::{BinaryOp, Expression, Identifier, LocatedExpression, UnaryOp},
};

mod exception;
mod resolver;
mod resource;
pub mod standard;
mod value;

#[derive(Collect)]
#[collect(no_drop)]
pub struct MyRoot<'a> {
    root_module: GcRefLock<'a, ModuleTree<'a>>,
    pwd: std::path::PathBuf,
    program: StaticCollect<ParsedProgram>,
    args: Value<'a>,
}

pub fn interpret_global(
    program: ParsedProgram,
    pwd: std::path::PathBuf,
    args: Vec<String>,
) -> Result<(), Located<String>> {
    let arena = Arena::<Rootable![MyRoot<'_>]>::new(|mc| {
        let root_module = ModuleTree::new(mc);
        let args_value = Value::Array(Gc::new(
            mc,
            RefLock::new(
                args.iter()
                    .map(|s| Value::String(Gc::new(mc, StringVariant::from_string(s))))
                    .collect(),
            ),
        ));
        MyRoot {
            root_module: Gc::new(mc, RefLock::new(root_module)),
            pwd,
            program: StaticCollect(program),
            args: args_value,
        }
    });
    arena.mutate(move |mc, root| {
        resolve_canonical_paths(mc, root);
        let root_module = ModuleTree::get(root.root_module, &ModulePath::root()).unwrap();
        let env = root_module.borrow().env;
        let root_tree = &root.program.children["root"];
        for expr in &root_tree.expressions {
            if let Expression::Define { .. } = expr.data {
                continue; // handled in resolver
            }
            if let Err(e) = interpret(mc, root, expr, env) {
                let msg = match &e.data {
                    Value::ClassInstance(ci) => {
                        let ci_ref = ci.borrow();
                        ci_ref
                            .fields
                            .get("message")
                            .and_then(|v| {
                                if let Value::String(s) = v {
                                    Some(s.to_string())
                                } else {
                                    None
                                }
                            })
                            .unwrap_or_else(|| variable_to_string(&e.data))
                    }
                    _ => variable_to_string(&e.data),
                };
                return Err(Located {
                    data: format!("Uncaught exception: {}", msg),
                    location: e.location,
                });
            }
        }
        Ok(())
    })?;
    Ok(())
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
    ConstructorMakeInstance {
        args: Vec<Value<'a>>,
        class: GcRefLock<'a, ClassValue<'a>>,
        function: Value<'a>,
        fields_remaining: Vec<(String, &'a LocatedExpression)>,
        fields: HashMap<String, Value<'a>>,
        current_field: Option<String>,
    },
    ReturnBarrier,
    Assign {
        target: &'a LocatedExpression,
        op: Option<&'a BinaryOp>,
    },
    FieldAssign(Value<'a>, Option<&'a BinaryOp>, String),
    ArrayAssign {
        value: Value<'a>,
        index: &'a LocatedExpression,
        array: Option<Value<'a>>,
        op: Option<&'a BinaryOp>,
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
    TryCatch {
        exception_prototype: Option<Value<'a>>,
        has_prototype: bool,
        try_block: &'a LocatedExpression,
        exception_var: String,
        catch_block: &'a LocatedExpression,
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

fn convert_err<'a>(val: Result<Value<'a>, LocatedError<'a>>) -> ExecResult<'a> {
    match val {
        Ok(v) => ExecResult::Value(v),
        Err(LocatedError { data, location }) => ExecResult::Error(data, location),
    }
}

pub fn interpret<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    expr: &'a LocatedExpression,
    env: Gc<'a, Environment<'a>>,
) -> Result<Value<'a>, LocatedError<'a>> {
    let mut stack: Vec<EnvFrame<'a>> = vec![Frame::eval(expr, env)];
    let mut last: ExecResult<'a> = ExecResult::Value(Value::Null);
    while let Some(frame) = stack.pop() {
        match frame.frame {
            Frame::Eval(expr) => {
                interpret_frame_eval(mc, root, &mut stack, &mut last, &expr, frame.env);
            }
            Frame::Kont(kont) => {
                interpret_frame_kont(
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
        ExecResult::Error(v, location) => Err(LocatedError { data: v, location }),
        ExecResult::Break(location) => unhandled_control_flow(mc, root, location),
        ExecResult::Continue(location) => unhandled_control_flow(mc, root, location),
    }
}

fn interpret_frame_eval<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    stack: &mut Vec<EnvFrame<'a>>,
    last: &mut ExecResult<'a>,
    expr: &'a LocatedExpression,
    env: Gc<'a, Environment<'a>>,
) {
    let location = &expr.location;
    match &expr.data {
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
            *last = ExecResult::Value(Value::Integer(IntVariant::from_digit_string(i)));
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
        Expression::StaticDefine { name, value } => {
            env.define_static(
                mc,
                name,
                StaticValue::Uninitialized(Gc::new(mc, StaticCollect(*value.clone()))),
            );
        }
        Expression::Return(inner_expr) => {
            if let Some(inner_expr) = inner_expr {
                stack.push(Frame::kont(Kont::Return, env, location.clone()));
                stack.push(Frame::eval(inner_expr, env));
            } else {
                *last = ExecResult::Return(Value::Null);
            }
        }
        Expression::Define { name, value: expr } => {
            stack.push(Frame::kont(
                Kont::Define(name.clone()),
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(expr, env));
        }
        Expression::Ident(name) => *last = eval_identifier(mc, root, name, env, &expr.location),
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
        Expression::ClassLiteral { .. } => {
            *last = make_class_literal(mc, root, expr, env);
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
        Expression::Assign { target, value, op } => {
            stack.push(Frame::kont(
                Kont::Assign {
                    target,
                    op: op.as_ref(),
                },
                env,
                location.clone(),
            ));
            stack.push(Frame::eval(value, env));
        }
        Expression::BinaryOp { op, left, right } => {
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
        Expression::TryCatch {
            try_block,
            exception_prototype,
            exception_var,
            catch_block,
        } => {
            stack.push(Frame::kont(
                Kont::TryCatch {
                    exception_prototype: None,
                    has_prototype: exception_prototype.is_some(),
                    exception_var: exception_var.clone(),
                    try_block,
                    catch_block,
                },
                env,
                location.clone(),
            ));
            if let Some(exception_prototype) = exception_prototype {
                stack.push(Frame::eval(exception_prototype, env));
            } else {
                stack.push(Frame::eval(try_block, env));
            }
        }
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

fn interpret_frame_kont<'a>(
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
                convert_err(duplicate_variable_definition(mc, root, &name, location))
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
            let result = match interpret_simple_binary_op(mc, root, op, left, right, &location) {
                Ok(v) => v,
                Err(e) => {
                    *last = ExecResult::Error(e.data, e.location);
                    return;
                }
            };
            *last = ExecResult::Value(result);
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
            *last = match get_property(mc, root, &object, &field, &location, env) {
                Ok((val, _)) => ExecResult::Value(val),
                Err(e) => ExecResult::Error(e.data, e.location),
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
            *last = get_at_index(mc, root, &location, array, val);
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
                let (func, is_method) =
                    match get_property(mc, root, &target, &function_field, &location, env) {
                        Ok(v) => v,
                        Err(e) => {
                            *last = ExecResult::Error(e.data, e.location);
                            return;
                        }
                    };
                if is_method {
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
                        &location,
                    ));
                    return;
                };
                let fields = get_class_fields(class);
                stack.push(Frame::kont(
                    Kont::ConstructorMakeInstance {
                        args,
                        class,
                        fields_remaining: fields
                            .into_iter()
                            .map::<(String, &'a LocatedExpression), _>(|(k, v)| {
                                (k.clone(), v.as_ref())
                            })
                            .rev()
                            .collect(),
                        fields: HashMap::new(),
                        function,
                        current_field: None,
                    },
                    env,
                    location.clone(),
                ));
            }
        }
        Kont::ConstructorMakeInstance {
            mut args,
            class,
            mut fields_remaining,
            mut fields,
            function,
            current_field,
        } => {
            if let Some(current_field) = current_field {
                let Some(last_value) = maybe_take_last_value(last) else {
                    return;
                };
                fields.insert(current_field, last_value);
            }
            if let Some((next_field, next_expr)) = fields_remaining.pop() {
                stack.push(Frame::kont(
                    Kont::ConstructorMakeInstance {
                        args,
                        class,
                        fields_remaining,
                        fields,
                        function,
                        current_field: Some(next_field),
                    },
                    env,
                    location.clone(),
                ));
                stack.push(Frame::eval(next_expr, env));
                return;
            }
            let instance = Value::ClassInstance(Gc::new(
                mc,
                RefLock::new(ClassInstanceValue {
                    class: class.clone(),
                    fields,
                }),
            ));
            args.insert(0, instance.clone());

            stack.push(Frame::kont(
                Kont::Set(ExecResult::Value(instance)),
                env.clone(),
                location.clone(),
            ));
            call_function(mc, root, stack, last, &location, env, args, &function);
        }
        Kont::Assign { target, op } => {
            let Some(value) = maybe_take_last_value(last) else {
                return;
            };
            match &target.data {
                Expression::Ident(name) => {
                    let result = if let Some(op) = op {
                        let left_val = match eval_identifier(mc, root, name, env, &location) {
                            ExecResult::Value(v) => v,
                            other => {
                                *last = other;
                                return;
                            }
                        };
                        match interpret_simple_binary_op(mc, root, op, left_val, value, &location) {
                            Ok(v) => v,
                            Err(e) => {
                                *last = ExecResult::Error(e.data, e.location);
                                return;
                            }
                        }
                    } else {
                        value
                    };
                    *last = match assign_identifier(mc, root, name, env, &location, result.clone())
                    {
                        Ok(_) => ExecResult::Value(result),
                        Err(e) => ExecResult::Error(e.data, e.location),
                    };
                }
                Expression::FieldAccess { object, field } => {
                    stack.push(Frame::kont(
                        Kont::FieldAssign(value, op, field.clone()),
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
                            op,
                        },
                        env,
                        location.clone(),
                    ));
                    stack.push(Frame::eval(array, env));
                }
                _ => {
                    *last =
                        convert_err(type_error(mc, root, "Invalid assignment target", &location));
                }
            }
        }
        Kont::FieldAssign(value, op, field) => {
            let Some(object) = maybe_take_last_value(last) else {
                return;
            };
            let result = if let Some(op) = op {
                let left_val = match get_property(mc, root, &object, &field, &location, env) {
                    Ok((v, _)) => v,
                    Err(e) => {
                        *last = ExecResult::Error(e.data, e.location);
                        return;
                    }
                };
                match interpret_simple_binary_op(mc, root, op, left_val, value, &location) {
                    Ok(v) => v,
                    Err(e) => {
                        *last = ExecResult::Error(e.data, e.location);
                        return;
                    }
                }
            } else {
                value
            };
            *last = set_property(root, mc, &location, object, field, result);
        }
        Kont::ArrayAssign {
            value,
            index,
            array,
            op,
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
                        op,
                    },
                    env,
                    location.clone(),
                ));
                stack.push(Frame::eval(index, env));
                return;
            };

            let result = if let Some(op) = op {
                let left_val = match get_at_index(mc, root, &location, array.clone(), val.clone()) {
                    ExecResult::Value(v) => v,
                    other => {
                        *last = other;
                        return;
                    }
                };
                match interpret_simple_binary_op(mc, root, op, left_val, value, &location) {
                    Ok(v) => v,
                    Err(e) => {
                        *last = ExecResult::Error(e.data, e.location);
                        return;
                    }
                }
            } else {
                value
            };
            *last = set_at_index(mc, root, &location, val, array, result);
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
                // Handle break/continue, propagate error and return upwards
                match last {
                    ExecResult::Break(_) => {
                        *last = ExecResult::Value(Value::Null);
                        return;
                    }
                    ExecResult::Continue(_) => break 'm LoopState::Condition,
                    _ => {}
                }
                let Some(val) = maybe_take_last_value(last) else {
                    return;
                };
                match state {
                    LoopState::Init => LoopState::Condition,
                    LoopState::Condition => match val {
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
                                &location,
                            ));
                            return;
                        }
                    },
                    LoopState::Body | LoopState::Increment => {
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
        Kont::TryCatch {
            has_prototype,
            exception_prototype,
            try_block,
            exception_var,
            catch_block,
        } => {
            if has_prototype && exception_prototype.is_none() {
                let Some(last) = maybe_take_last_value(last) else {
                    return;
                };
                stack.push(Frame::kont(
                    Kont::TryCatch {
                        exception_prototype: Some(last),
                        has_prototype,
                        exception_var: exception_var.clone(),
                        try_block,
                        catch_block,
                    },
                    env,
                    location.clone(),
                ));
                stack.push(Frame::eval(try_block, env));
                return;
            }
            let ExecResult::Error(exception, _) = last else {
                return;
            };
            if let Some(Value::Class(c)) = exception_prototype
                && !is_instance_of(&exception, c)
            {
                return;
            }
            let ExecResult::Error(exception, _) = std::mem::take(last) else {
                unreachable!();
            };
            let catch_env = Gc::new(mc, Environment::new(mc, env));
            catch_env.define(mc, &exception_var, exception);
            stack.push(Frame::eval(catch_block, catch_env));
        }
    }
}

fn set_at_index<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    location: &Location,
    index: Value<'a>,
    array: Value<'a>,
    result: Value<'a>,
) -> ExecResult<'a> {
    match array {
        Value::Array(arr) => {
            let Value::Integer(i) = index else {
                return convert_err(type_error(
                    mc,
                    root,
                    "Array index must be an integer",
                    location,
                ));
            };
            let mut arr_ref = arr.borrow_mut(mc);
            if let Some(index) = i.try_to_usize()
                && index < arr_ref.len()
            {
                arr_ref[index] = result.clone();
                ExecResult::Value(result)
            } else {
                convert_err(array_index_out_of_bounds(mc, root, i, location))
            }
        }
        Value::String(_) => convert_err(type_error(
            mc,
            root,
            "Strings are immutable; cannot assign to index",
            location,
        )),
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            let Value::String(s) = index else {
                return convert_err(type_error(
                    mc,
                    root,
                    "Object index must be an string",
                    location,
                ));
            };
            let s = s.to_string();
            if obj_ref.contains_key(&s) {
                drop(obj_ref);
                let mut obj_ref = obj.borrow_mut(mc);
                obj_ref.insert(s, result.clone());
                ExecResult::Value(result)
            } else {
                convert_err(field_access_error(mc, root, &s, location))
            }
        }
        Value::TypedSlice {
            buffer,
            start,
            length,
            buffer_type,
        } => {
            let Value::Integer(i) = index else {
                return convert_err(type_error(
                    mc,
                    root,
                    "Array index must be an integer",
                    location,
                ));
            };
            let mut buf_ref = buffer.borrow_mut(mc);
            let byte_start = start * buffer_type.byte_size();
            let byte_end = byte_start + length * buffer_type.byte_size();
            if let Some(index) = i.try_to_usize()
                && index < buf_ref.len()
            {
                let byte_offset = byte_start + index * buffer_type.byte_size();
                if buffer_type
                    .try_write_value(&mut buf_ref[byte_offset..byte_end], &result)
                    .is_some()
                {
                    ExecResult::Value(result)
                } else {
                    convert_err(type_error(
                        mc,
                        root,
                        "Failed to write value to typed slice",
                        location,
                    ))
                }
            } else {
                convert_err(array_index_out_of_bounds(mc, root, i, location))
            }
        }
        _ => convert_err(type_error(
            mc,
            root,
            "Attempted to index a non-array value",
            location,
        )),
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
        (UnaryOp::Negate, Value::Integer(v)) => Value::Integer(v.neg()),
        (UnaryOp::Not, Value::Integer(v)) => Value::Integer(v.invert()),
        (UnaryOp::Not, Value::Bool(v)) => Value::Bool(!v),
        _ => return None,
    })
}

fn interpret_simple_binary_op<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    op: &BinaryOp,
    left: Value<'a>,
    right: Value<'a>,
    location: &Location,
) -> Result<Value<'a>, LocatedError<'a>> {
    Ok(match (op, left, right) {
        (BinaryOp::Add, Value::Number(l), Value::Number(r)) => Value::Number(l + r),
        (BinaryOp::Add, Value::Integer(l), Value::Integer(r)) => Value::Integer(l.add(r)),
        (BinaryOp::Add, Value::Integer(l), Value::Number(r)) => Value::Number(l.to_f64() + r),
        (BinaryOp::Add, Value::Number(l), Value::Integer(r)) => Value::Number(l + r.to_f64()),

        (BinaryOp::Subtract, Value::Number(l), Value::Number(r)) => Value::Number(l - r),
        (BinaryOp::Subtract, Value::Integer(l), Value::Integer(r)) => Value::Integer(l.sub(r)),
        (BinaryOp::Subtract, Value::Integer(l), Value::Number(r)) => Value::Number(l.to_f64() - r),
        (BinaryOp::Subtract, Value::Number(l), Value::Integer(r)) => Value::Number(l - r.to_f64()),

        (BinaryOp::Multiply, Value::Number(l), Value::Number(r)) => Value::Number(l * r),
        (BinaryOp::Multiply, Value::Integer(l), Value::Integer(r)) => Value::Integer(l.mul(r)),
        (BinaryOp::Multiply, Value::Integer(l), Value::Number(r)) => Value::Number(l.to_f64() * r),
        (BinaryOp::Multiply, Value::Number(l), Value::Integer(r)) => Value::Number(l * r.to_f64()),

        (BinaryOp::Divide, Value::Number(l), Value::Number(r)) => Value::Number(l / r),
        (BinaryOp::Divide, Value::Integer(l), Value::Integer(r)) => Value::Integer(
            l.div(r)
                .ok_or_else(|| division_by_zero::<Value<'a>>(mc, root, location).unwrap_err())?,
        ),
        (BinaryOp::Divide, Value::Integer(l), Value::Number(r)) => Value::Number(l.to_f64() / r),
        (BinaryOp::Divide, Value::Number(l), Value::Integer(r)) => Value::Number(l / r.to_f64()),

        (BinaryOp::Modulo, Value::Number(l), Value::Number(r)) => Value::Number(l % r),
        (BinaryOp::Modulo, Value::Integer(l), Value::Integer(r)) => Value::Integer(
            l.rem(r)
                .ok_or_else(|| division_by_zero::<Value<'a>>(mc, root, location).unwrap_err())?,
        ),
        (BinaryOp::Modulo, Value::Integer(l), Value::Number(r)) => Value::Number(l.to_f64() % r),
        (BinaryOp::Modulo, Value::Number(l), Value::Integer(r)) => Value::Number(l % r.to_f64()),

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
        (BinaryOp::Less, Value::Integer(l), Value::Integer(r)) => Value::Bool(l.less(&r)),
        (BinaryOp::LessEqual, Value::Number(l), Value::Number(r)) => Value::Bool(l <= r),
        (BinaryOp::LessEqual, Value::Integer(l), Value::Integer(r)) => {
            Value::Bool(l.less_equal(&r))
        }

        (BinaryOp::Greater, Value::Number(l), Value::Number(r)) => Value::Bool(l > r),
        (BinaryOp::Greater, Value::Integer(l), Value::Integer(r)) => Value::Bool(l.greater(&r)),
        (BinaryOp::GreaterEqual, Value::Number(l), Value::Number(r)) => Value::Bool(l >= r),
        (BinaryOp::GreaterEqual, Value::Integer(l), Value::Integer(r)) => {
            Value::Bool(l.greater_equal(&r))
        }

        (_, left, right) => {
            return unsupported_binary_operation(mc, root, op, &left, &right, location);
        }
    })
}

pub fn get_std_env<'a>(root: &MyRoot<'a>) -> Gc<'a, Environment<'a>> {
    let std_tree = ModuleTree::get(root.root_module, &ModulePath::from_root("std")).unwrap();
    let std_env = std_tree.borrow().env;
    std_env
}

fn get_classes<'a>(
    root: &MyRoot<'a>,
    target: &Value<'a>,
) -> (
    Option<GcRefLock<'a, ClassValue<'a>>>,
    GcRefLock<'a, ClassValue<'a>>,
) {
    let class_name = target.get_type();
    let std_env = get_std_env(root);
    let Value::Class(value_class) = std_env.get_simple(class_name).unwrap() else {
        panic!("Standard class {} not found", class_name);
    };

    let extra_class = if let Value::ClassInstance(instance) = target {
        Some(instance.borrow().class)
    } else if let Value::Class(class) = target {
        Some(*class)
    } else {
        None
    };
    (extra_class, value_class)
}

fn get_property<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    target: &Value<'a>,
    field: &str,
    location: &Location,
    env: Gc<'a, Environment<'a>>,
) -> Result<(Value<'a>, bool), LocatedError<'a>> {
    match target {
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            if let Some(val) = obj_ref.get(field).cloned() {
                return Ok((val, false));
            }
        }
        Value::ClassInstance(obj) => {
            let obj_ref = obj.borrow();
            if let Some(val) = obj_ref.fields.get(field).cloned() {
                return Ok((val, false));
            }
        }
        _ => {}
    }

    let (extra_class, value_class) = get_classes(root, target);

    fn look_for_property_in_class<'a>(
        mc: &Mutation<'a>,
        root: &MyRoot<'a>,
        mut class: GcRefLock<'a, ClassValue<'a>>,
        field: &str,
        location: &Location,
        env: Gc<'a, Environment<'a>>,
    ) -> Option<Result<(Value<'a>, bool), LocatedError<'a>>> {
        loop {
            let cur_ref = class.borrow();
            if let Some(val) = cur_ref.static_fields.get(field).cloned() {
                match val {
                    StaticValue::BeingInitialized => {
                        return Some(cyclic_static_initialization(mc, root, field, location));
                    }
                    StaticValue::Initialized(v) => {
                        return Some(Ok((v, false)));
                    }
                    StaticValue::Uninitialized(expr) => {
                        drop(cur_ref);
                        let mut class_mut = class.borrow_mut(mc);
                        class_mut
                            .static_fields
                            .insert(field.to_string(), StaticValue::BeingInitialized);
                        drop(class_mut);
                        let result = interpret(mc, root, expr.as_ref(), env);
                        match result {
                            Ok(v) => {
                                class
                                    .borrow_mut(mc)
                                    .static_fields
                                    .insert(field.to_string(), StaticValue::Initialized(v.clone()));
                                return Some(Ok((v, false)));
                            }
                            Err(err) => {
                                return Some(Err(err));
                            }
                        }
                    }
                }
            } else if let Some(val) = cur_ref.methods.get(field) {
                return Some(Ok((Value::Function(*val), true)));
            } else if let Some(val) = cur_ref.static_methods.get(field) {
                return Some(Ok((Value::Function(*val), false)));
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
    if let Some(extra_class) = extra_class
        && let Some(res) = look_for_property_in_class(mc, root, extra_class, field, location, env)
    {
        res
    } else if let Some(res) =
        look_for_property_in_class(mc, root, value_class, field, location, env)
    {
        res
    } else {
        field_access_error(mc, root, field, location)
    }
}

fn get_class_fields<'a>(
    class: GcRefLock<'a, ClassValue<'a>>,
) -> Vec<(String, Gc<'a, StaticCollect<LocatedExpression>>)> {
    let mut fields = Vec::new();
    let mut current_class = Some(class);
    while let Some(class) = current_class {
        let class_ref = class.borrow();
        for (key, value) in class_ref.instance_fields.iter().rev() {
            fields.push((key.clone(), *value));
        }
        current_class = class_ref.parent.clone();
    }
    fields.reverse();
    fields
}

fn get_at_index<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    location: &Location,
    array: Value<'a>,
    index: Value<'a>,
) -> ExecResult<'a> {
    match array {
        Value::Array(arr) => {
            let Value::Integer(i) = index else {
                return convert_err(type_error(
                    mc,
                    root,
                    "Array index must be an integer",
                    location,
                ));
            };
            let arr_ref = arr.borrow();
            if let Some(index) = i.try_to_usize()
                && index < arr_ref.len()
            {
                let value = arr_ref[index].clone();
                ExecResult::Value(value)
            } else {
                convert_err(array_index_out_of_bounds(mc, root, i, location))
            }
        }
        Value::String(s) => {
            let Value::Integer(i) = index else {
                return convert_err(type_error(
                    mc,
                    root,
                    "String index must be an integer",
                    location,
                ));
            };
            if let Some(index) = i.try_to_usize() {
                let Some(value) = StringVariant::slice(mc, s, index, 1) else {
                    return convert_err(array_index_out_of_bounds(mc, root, i, location));
                };
                ExecResult::Value(Value::String(value))
            } else {
                convert_err(array_index_out_of_bounds(mc, root, i, location))
            }
        }
        Value::TypedSlice {
            buffer,
            start,
            length,
            buffer_type,
        } => {
            let Value::Integer(i) = index else {
                return convert_err(type_error(
                    mc,
                    root,
                    "Array index must be an integer",
                    location,
                ));
            };
            let byte_start = start * buffer_type.byte_size();
            let byte_end = byte_start + length * buffer_type.byte_size();
            let arr_ref = buffer.borrow();
            if let Some(index) = i.try_to_usize()
                && index < arr_ref.len()
            {
                let byte_offset = byte_start + index * buffer_type.byte_size();
                let Some(value) = buffer_type.try_read_value(&arr_ref[byte_offset..byte_end])
                else {
                    return convert_err(type_error(
                        mc,
                        root,
                        "Failed to read value from typed slice",
                        &location,
                    ));
                };
                ExecResult::Value(value)
            } else {
                convert_err(array_index_out_of_bounds(mc, root, i, &location))
            }
        }
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            let Value::String(s) = index else {
                return convert_err(type_error(
                    mc,
                    root,
                    "Object index must be an string",
                    location,
                ));
            };
            let s = s.to_string();
            if let Some(value) = obj_ref.get(&s).cloned() {
                ExecResult::Value(value)
            } else {
                convert_err(field_access_error(mc, root, &s, location))
            }
        }
        _ => convert_err(type_error(
            mc,
            root,
            "Attempted to index a non-array value",
            &location,
        )),
    }
}

fn set_property<'a>(
    root: &MyRoot<'a>,
    mc: &Mutation<'a>,
    location: &Location,
    object: Value<'a>,
    field: String,
    result: Value<'a>,
) -> ExecResult<'a> {
    match object {
        Value::Object(obj) => {
            obj.borrow_mut(mc).insert(field, result.clone());
            ExecResult::Value(result)
        }
        Value::ClassInstance(obj) => {
            if let Some(field) = obj.borrow_mut(mc).fields.get_mut(&field) {
                *field = result.clone();
                ExecResult::Value(result)
            } else {
                convert_err(type_error(
                    mc,
                    root,
                    &format!("Field '{}' does not exist on class instance", field),
                    &location,
                ))
            }
        }
        Value::Class(class) => {
            let mut cur = class;
            loop {
                let mut class_ref = cur.borrow_mut(mc);
                if let Some(field_val) = class_ref.static_fields.get_mut(&field) {
                    *field_val = StaticValue::Initialized(result.clone());
                    return ExecResult::Value(result);
                } else if let Some(parent) = &class_ref.parent {
                    let parent = *parent;
                    drop(class_ref);
                    cur = parent;
                } else {
                    return convert_err(type_error(
                        mc,
                        root,
                        &format!("Static field '{}' does not exist on class", field),
                        &location,
                    ));
                }
            }
        }
        _ => convert_err(type_error(
            mc,
            root,
            "Attempted to assign field on non-object value",
            &location,
        )),
    }
}

fn is_instance_of<'a>(object: &Value<'a>, class: GcRefLock<'a, ClassValue<'a>>) -> bool {
    let Value::ClassInstance(instance) = object else {
        return false;
    };
    let mut current_class = Some(instance.borrow().class);
    while let Some(class_ref) = current_class {
        if Gc::ptr_eq(class_ref, class) {
            return true;
        }
        current_class = class_ref.borrow().parent;
    }
    false
}

fn expression_to_function<'a>(
    mc: &Mutation<'a>,
    expr: &'a LocatedExpression,
    env: Gc<'a, Environment<'a>>,
) -> GcRefLock<'a, FunctionValue<'a>> {
    if let Expression::FunctionLiteral { parameters, body } = &expr.data {
        let func_env = Gc::new(mc, Environment::new(mc, env));
        Gc::new(
            mc,
            RefLock::new(FunctionValue::Function {
                parameters: parameters.clone(),
                body: Gc::new(mc, StaticCollect(*body.clone())),
                env: func_env,
            }),
        )
    } else {
        panic!("Expected function literal");
    }
}

fn make_class_literal<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    expr: &'a LocatedExpression,
    env: Gc<'a, Environment<'a>>,
) -> ExecResult<'a> {
    match &expr.data {
        Expression::ClassLiteral {
            parent,
            fields,
            static_fields,
            methods,
            static_methods,
            constructor,
        } => {
            let parent_val = match parent {
                None => None,
                Some(p) => match eval_identifier(mc, root, p, env, &expr.location) {
                    ExecResult::Value(v) => Some(v),
                    other => {
                        return other;
                    }
                },
            };
            let parent_class = match &parent_val {
                Some(Value::Class(c)) => Some(c.clone()),
                Some(other) => {
                    return convert_err(type_error(
                        mc,
                        root,
                        &format!("Expected class as parent, got {:?}", other),
                        &expr.location,
                    ));
                }
                None => None,
            };
            let instance_fields = fields
                .iter()
                .map(|(name, expr)| (name.clone(), Gc::new(mc, StaticCollect(*expr.clone()))))
                .collect();
            let methods = methods
                .iter()
                .map(|(name, expr)| (name.clone(), expression_to_function(mc, expr, env)))
                .collect();
            let static_methods = static_methods
                .iter()
                .map(|(name, expr)| (name.clone(), expression_to_function(mc, expr, env)))
                .collect();
            let constructor = constructor
                .as_ref()
                .map(|expr| expression_to_function(mc, expr, env));
            let static_fields = static_fields
                .iter()
                .map(|(name, expr)| {
                    (
                        name.clone(),
                        StaticValue::Uninitialized(Gc::new(mc, StaticCollect(*expr.clone()))),
                    )
                })
                .collect();
            let class_val = Gc::new(
                mc,
                RefLock::new(ClassValue {
                    parent: parent_class,
                    instance_fields,
                    constructor,
                    static_fields,
                    static_methods,
                    methods,
                }),
            );
            ExecResult::Value(Value::Class(class_val))
        }
        _ => unreachable!(),
    }
}

fn eval_identifier<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    ident: &Identifier,
    env: Gc<'a, Environment<'a>>,
    location: &Location,
) -> ExecResult<'a> {
    match ident {
        Identifier::Simple(name) => convert_err(Environment::get(env, mc, root, name, location)),
        Identifier::Scoped(path) => {
            let module_tree = ModuleTree::get(root.root_module, &path.path);
            if let Some(module_tree) = module_tree {
                let env = module_tree.borrow().env;
                convert_err(Environment::get(env, mc, root, &path.item, location))
            } else {
                convert_err(undefined_variable(mc, root, &path.to_string(), location))
            }
        }
    }
}

fn assign_identifier<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    ident: &Identifier,
    env: Gc<'a, Environment<'a>>,
    location: &Location,
    value: Value<'a>,
) -> Result<(), LocatedError<'a>> {
    match ident {
        Identifier::Simple(name) => Environment::set(env, mc, root, name, value, &location),
        Identifier::Scoped(path) => {
            let module_tree = ModuleTree::get(root.root_module, &path.path);
            if let Some(module_tree) = module_tree {
                let env = module_tree.borrow().env;
                Environment::set(env, mc, root, &path.item, value, &location)
            } else {
                undefined_variable(mc, root, &path.to_string(), location)
            }
        }
    }
}
