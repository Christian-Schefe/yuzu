use std::{collections::HashMap, vec};

use gc_arena::{
    Arena, Collect, Gc, Mutation, Rootable, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    ModulePath, ParsedProgram,
    bytecode::{Instruction, compile, compile_expr},
    gc_interpreter::{
        exception::{
            array_index_out_of_bounds, cyclic_static_initialization, division_by_zero,
            duplicate_variable_definition, field_access_error, index_out_of_bounds, type_error,
            undefined_variable, unhandled_control_flow, unsupported_binary_operation,
        },
        resolver::resolve_canonical_paths,
        value::{
            ClassInstanceValue, ClassValue, Environment, FunctionValue, IntVariant, LocatedError,
            ModuleTree, StaticValue, StringVariant, Value, variable_to_string,
        },
    },
    location::{Located, Location},
    parser::{
        BinaryOp, ClassMemberKind, Expression, Identifier, LocatedExpression, Pattern, UnaryOp,
    },
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
            let code = compile_expr(expr);
            if let Err(e) = interpret(mc, root, &code, env) {
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

pub enum Frame {
    Block,
    Catch {
        target: usize,
        stack_height: usize,
    },
    Loop {
        break_target: usize,
        continue_target: usize,
        stack_height: usize,
    },
    Function {
        return_ip: usize,
        stack_height: usize,
    },
}

pub struct EnvFrame<'a> {
    pub frame: Frame,
    pub env: Gc<'a, Environment<'a>>,
}

impl<'a> EnvFrame<'a> {
    fn new(frame: Frame, env: Gc<'a, Environment<'a>>) -> Self {
        Self { frame, env }
    }
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
    code: &[Located<Instruction>],
    env: Gc<'a, Environment<'a>>,
) -> Result<Value<'a>, LocatedError<'a>> {
    let mut frame_stack: Vec<EnvFrame<'a>> = vec![EnvFrame::new(Frame::Block, env)];
    let mut stack: Vec<Value<'a>> = Vec::new();
    let mut ip = 0;

    fn bubble_error<'a>(
        err: Result<Value<'a>, LocatedError<'a>>,
        frame_stack: &mut Vec<EnvFrame<'a>>,
        stack: &mut Vec<Value<'a>>,
        ip: &mut usize,
    ) -> Result<(), LocatedError<'a>> {
        let err = err.unwrap_err();
        while let Some(frame) = frame_stack.pop() {
            match frame.frame {
                Frame::Catch {
                    target,
                    stack_height,
                } => {
                    *ip = target;
                    stack.truncate(stack_height);
                    stack.push(err.data);
                    return Ok(());
                }
                _ => continue,
            }
        }
        return Err(err);
    }
    fn bubble_control_flow<'a>(
        mc: &Mutation<'a>,
        root: &MyRoot<'a>,
        is_break: bool,
        location: &Location,
        frame_stack: &mut Vec<EnvFrame<'a>>,
        stack: &mut Vec<Value<'a>>,
        ip: &mut usize,
    ) -> Result<(), LocatedError<'a>> {
        while let Some(frame) = frame_stack.pop() {
            match frame.frame {
                Frame::Loop {
                    break_target,
                    continue_target,
                    stack_height,
                } => {
                    *ip = if is_break {
                        break_target
                    } else {
                        continue_target
                    };
                    stack.truncate(stack_height);
                    return Ok(());
                }
                _ => continue,
            }
        }
        return unhandled_control_flow(mc, root, location);
    }
    fn bubble_return<'a>(
        mc: &Mutation<'a>,
        root: &MyRoot<'a>,
        ret: Value<'a>,
        location: &Location,
        frame_stack: &mut Vec<EnvFrame<'a>>,
        stack: &mut Vec<Value<'a>>,
        ip: &mut usize,
    ) -> Result<(), LocatedError<'a>> {
        while let Some(frame) = frame_stack.pop() {
            match frame.frame {
                Frame::Function {
                    return_ip,
                    stack_height,
                } => {
                    *ip = return_ip;
                    stack.truncate(stack_height);
                    stack.push(ret);
                    return Ok(());
                }
                _ => continue,
            }
        }
        return unhandled_control_flow(mc, root, location);
    }

    while ip < code.len() {
        let instruction = &code[ip];
        let env = frame_stack.last().expect("Frame stack empty").env;
        match &instruction.data {
            Instruction::Load(identifier) => todo!(),
            Instruction::Store(identifier) => todo!(),
            Instruction::Define(pattern, _) => todo!(),
            Instruction::LoadProperty(_) => todo!(),
            Instruction::StoreProperty(_) => todo!(),
            Instruction::LoadIndex => todo!(),
            Instruction::StoreIndex => todo!(),
            Instruction::Pop => {
                stack.pop().expect("Stack empty");
            }
            Instruction::PushNull => stack.push(Value::Null),
            Instruction::PushBool(b) => stack.push(Value::Bool(*b)),
            Instruction::PushInteger(i) => {
                stack.push(Value::Integer(IntVariant::from_digit_string(i)))
            }
            Instruction::PushNumber(num) => stack.push(Value::Number(*num)),
            Instruction::PushString(s) => {
                stack.push(Value::String(Gc::new(mc, StringVariant::from_string(s))))
            }
            Instruction::PushArray(items) => {
                let mut items: Vec<(Value<'a>, bool)> = items
                    .iter()
                    .rev()
                    .map(|is_spread| (stack.pop().expect("Stack empty"), *is_spread))
                    .collect();
                items.reverse();
                let mut item_vec = Vec::new();
                for (item, is_spread) in items {
                    if is_spread {
                        if let Value::Array(arr) = item {
                            let arr_ref = arr.borrow();
                            item_vec.extend_from_slice(&arr_ref);
                        } else {
                            bubble_error(
                                type_error(
                                    mc,
                                    root,
                                    "Spread operator can only be applied to arrays",
                                    &instruction.location,
                                ),
                                &mut frame_stack,
                                &mut stack,
                                &mut ip,
                            )?;
                            continue;
                        }
                    } else {
                        item_vec.push(item);
                    }
                }
                stack.push(Value::Array(Gc::new(mc, RefLock::new(item_vec))));
            }
            Instruction::PushObject(items) => todo!(),
            Instruction::Raise => {
                let err = stack.pop().expect("Stack empty");
                bubble_error(
                    Err(LocatedError {
                        data: err,
                        location: instruction.location.clone(),
                    }),
                    &mut frame_stack,
                    &mut stack,
                    &mut ip,
                )?;
                continue;
            }
            Instruction::PushFunction {
                parameters,
                body_pointer,
            } => {
                let func = Value::Function(Gc::new(
                    mc,
                    RefLock::new(FunctionValue::Function {
                        parameters: parameters.clone(),
                        body: *body_pointer,
                        env,
                    }),
                ));
                stack.push(func);
            }
            Instruction::PushClass {
                parent,
                initializer_pointer,
                properties,
                constructor_pointer,
            } => todo!(),
            Instruction::Break => {
                bubble_control_flow(
                    mc,
                    root,
                    true,
                    &instruction.location,
                    &mut frame_stack,
                    &mut stack,
                    &mut ip,
                )?;
                continue;
            }
            Instruction::Continue => {
                bubble_control_flow(
                    mc,
                    root,
                    false,
                    &instruction.location,
                    &mut frame_stack,
                    &mut stack,
                    &mut ip,
                )?;
                continue;
            }
            Instruction::Return => todo!(),
            Instruction::Jump(target) => {
                ip = *target;
                continue;
            }
            Instruction::JumpIfFalse(target) => {
                let condition = stack.pop().expect("Stack empty");
                let Value::Bool(b) = condition else {
                    bubble_error(
                        type_error(
                            mc,
                            root,
                            "Condition must be a boolean",
                            &instruction.location,
                        ),
                        &mut frame_stack,
                        &mut stack,
                        &mut ip,
                    )?;
                    continue;
                };
                if b {
                    ip = *target;
                    continue;
                }
            }
            Instruction::EnterBlock => {
                let block_env = Gc::new(mc, Environment::new(mc, env));
                frame_stack.push(EnvFrame::new(Frame::Block, block_env));
            }
            Instruction::EnterLoop {
                break_target,
                continue_target,
            } => {
                frame_stack.push(EnvFrame::new(
                    Frame::Loop {
                        break_target: *break_target,
                        continue_target: *continue_target,
                        stack_height: stack.len(),
                    },
                    env,
                ));
            }
            Instruction::EnterTryCatch { catch_target } => {
                frame_stack.push(EnvFrame::new(
                    Frame::Catch {
                        target: *catch_target,
                        stack_height: stack.len(),
                    },
                    env,
                ));
            }
            Instruction::ExitFrame => match frame_stack.pop().expect("Frame stack empty").frame {
                Frame::Function { return_ip, .. } => {
                    ip = return_ip;
                    continue;
                }
                _ => {}
            },
            Instruction::CallFunction(_) => todo!(),
            Instruction::CallPropertyFunction(_, _) => todo!(),
            Instruction::TryShortCircuit(binary_op, _) => todo!(),
            Instruction::BinaryOp(binary_op) => todo!(),
            Instruction::UnaryOp(unary_op) => todo!(),
            Instruction::MakeInstance(items) => todo!(),
            Instruction::CallInitializer => todo!(),
            Instruction::CheckCatch(_) => todo!(),
        };
        ip += 1;
    }
    let last = stack
        .pop()
        .expect("Stack should have at least one value at the end of execution");
    Ok(last)
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
    code: &mut Vec<Located<Instruction>>,
) -> GcRefLock<'a, FunctionValue<'a>> {
    if let Expression::FunctionLiteral { parameters, body } = &expr.data {
        let func_env = Gc::new(mc, Environment::new(mc, env));
        compile(body, code);
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
    code: &mut Vec<Located<Instruction>>,
) -> ExecResult<'a> {
    match &expr.data {
        Expression::ClassLiteral {
            parent,
            properties,
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
            let instance_fields = properties
                .iter()
                .filter(|(_, _, kind)| matches!(kind, ClassMemberKind::Field))
                .map(|(name, expr, _)| (name.clone(), Gc::new(mc, StaticCollect(*expr.clone()))))
                .collect();
            let methods = properties
                .iter()
                .filter(|(_, _, kind)| matches!(kind, ClassMemberKind::Method))
                .map(|(name, expr, _)| (name.clone(), expression_to_function(mc, expr, env)))
                .collect();
            let static_methods = properties
                .iter()
                .filter(|(_, _, kind)| matches!(kind, ClassMemberKind::StaticMethod))
                .map(|(name, expr, _)| (name.clone(), expression_to_function(mc, expr, env)))
                .collect();
            let static_fields = properties
                .iter()
                .filter(|(_, _, kind)| matches!(kind, ClassMemberKind::StaticField))
                .map(|(name, expr, _)| {
                    (
                        name.clone(),
                        StaticValue::Uninitialized(Gc::new(mc, StaticCollect(*expr.clone()))),
                    )
                })
                .collect();
            let constructor = constructor
                .as_ref()
                .map(|expr| expression_to_function(mc, expr, env));
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

fn resolve_pattern<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    pattern: &'a Located<Pattern>,
    value: Value<'a>,
) -> Result<Vec<(&'a String, Value<'a>)>, LocatedError<'a>> {
    let mut stack = vec![(pattern, value)];
    let mut result = Vec::new();
    while let Some((pattern, value)) = stack.pop() {
        let location = &pattern.location;
        match &pattern.data {
            Pattern::Wildcard => continue,
            Pattern::Ident(name) => result.push((name, value.clone())),
            Pattern::Object { entries, rest } => {
                let Value::Object(obj) = &value else {
                    return type_error(
                        mc,
                        root,
                        "Expected object value for object pattern",
                        location,
                    );
                };
                let obj_ref = obj.borrow();
                let mut remaining_fields = obj_ref.clone();
                for (item, pattern) in entries {
                    let Some(val) = remaining_fields.remove(item) else {
                        return field_access_error(mc, root, item, location);
                    };
                    stack.push((pattern, val.clone()));
                }
                if let Some(rest) = rest {
                    let rest_obj = Value::Object(Gc::new(mc, RefLock::new(remaining_fields)));
                    result.push((rest, rest_obj));
                }
            }
            Pattern::Array { items, rest } => {
                let Value::Array(arr) = &value else {
                    return type_error(
                        mc,
                        root,
                        "Expected array value for array pattern",
                        location,
                    );
                };
                let arr_ref = arr.borrow();
                for (i, pattern) in items.iter().enumerate() {
                    let Some(val) = arr_ref.get(i) else {
                        return index_out_of_bounds(mc, root, i, location);
                    };
                    stack.push((pattern, val.clone()));
                }
                if let Some(rest) = rest
                    && items.len() <= arr_ref.len()
                {
                    let rest_obj =
                        Value::Array(Gc::new(mc, RefLock::new(arr_ref[items.len()..].to_vec())));
                    result.push((rest, rest_obj));
                }
            }
        }
    }
    Ok(result)
}
