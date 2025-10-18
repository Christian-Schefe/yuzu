use std::{collections::HashMap, vec};

use gc_arena::{
    Arena, Collect, Gc, Mutation, Rootable, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    ModulePath, ParsedProgram,
    bytecode::Instruction,
    gc_interpreter::{
        exception::{
            array_index_out_of_bounds, cannot_instantiate, cyclic_static_initialization,
            division_by_zero, duplicate_variable_definition, field_access_error,
            index_out_of_bounds, type_error, undefined_identifier, undefined_variable,
            unhandled_control_flow, unsupported_binary_operation, unsupported_unary_operation,
        },
        resolver::compile_and_setup,
        value::{
            ClassInstanceValue, ClassValue, CodePointer, Environment, FunctionValue, IntVariant,
            LazyValue, LocatedError, ModuleTree, StringVariant, Value, value_to_string,
        },
    },
    location::{Located, Location},
    parser::{BinaryOp, Identifier, Pattern, UnaryOp},
};

mod exception;
mod resolver;
mod resource;
pub mod standard;
pub mod value;

#[derive(Collect)]
#[collect(no_drop)]
pub struct MyRoot<'a> {
    pub root_module: GcRefLock<'a, ModuleTree<'a>>,
    pub pwd: std::path::PathBuf,
    pub args: Value<'a>,
    pub code: GcRefLock<'a, StaticCollect<Vec<Located<Instruction>>>>,
    pub global_env: Gc<'a, Environment<'a>>,
    pub exec_ctx: GcRefLock<'a, ExecContext<'a>>,
}

pub struct Context<'a> {
    pub mc: &'a Mutation<'a>,
    pub root: &'a MyRoot<'a>,
}

impl<'a> Context<'a> {
    pub fn gc_lock<T>(&self, val: T) -> GcRefLock<'a, T>
    where
        T: Collect,
    {
        GcRefLock::new(self.mc, RefLock::new(val))
    }

    pub fn gc<T>(&self, val: T) -> Gc<'a, T>
    where
        T: Collect,
    {
        Gc::new(self.mc, val)
    }
}

pub fn interpret_global(
    mut program: ParsedProgram,
    pwd: std::path::PathBuf,
    args: Vec<String>,
    main_module: String,
) -> Result<(), Located<String>> {
    let arena = Arena::<Rootable![MyRoot<'_>]>::new(|mc| {
        let global_env = Gc::new(mc, Environment::new_global(mc));
        let root_module = ModuleTree::new(mc, global_env);
        let args_value = Value::Array(Gc::new(
            mc,
            RefLock::new(
                args.iter()
                    .map(|s| Value::String(Gc::new(mc, StringVariant::from_string(s))))
                    .collect(),
            ),
        ));
        let code = Vec::new();
        let mut exec_ctx = ExecContext::new();
        exec_ctx.push_frame(Frame::Simple, global_env);
        MyRoot {
            root_module: Gc::new(mc, RefLock::new(root_module)),
            pwd,
            args: args_value,
            code: GcRefLock::new(mc, RefLock::new(StaticCollect(code))),
            global_env,
            exec_ctx: GcRefLock::new(mc, RefLock::new(exec_ctx)),
        }
    });
    arena.mutate(move |mc, root| {
        let ctx = Context { mc, root };
        let mut code = root.code.borrow_mut(mc);
        compile_and_setup(&ctx, &mut program, &mut code, main_module);
    });
    interpret(arena)
}

fn convert_error<'a>(ctx: &Context<'a>, e: LocatedError<'a>) -> Located<String> {
    let msg = match &e.data {
        Value::ClassInstance(ci) => {
            let ci_ref = ci.borrow();
            let msg = get_property(ctx, &ci_ref.inner, "message", &e.location);
            match msg {
                Ok(v) => value_to_string(&v),
                _ => value_to_string(&e.data),
            }
        }
        _ => value_to_string(&e.data),
    };
    return Located {
        data: format!("Uncaught exception: {}", msg),
        location: e.location,
    };
}

pub fn interpret(mut arena: Arena<Rootable![MyRoot<'_>]>) -> Result<(), Located<String>> {
    let instruction_batch_size = 1000;
    loop {
        for _ in 0..instruction_batch_size {
            if let Some(res) = arena.mutate(|mc, root| {
                let ctx = Context { mc, root };
                let code = ctx.root.code.borrow();
                let mut exec_ctx = ctx.root.exec_ctx.borrow_mut(mc);

                match eval_instruction(&ctx, &mut exec_ctx, &code) {
                    Ok(false) => return Some(Ok(())),
                    Ok(true) => {}
                    Err(e) => {
                        return Some(
                            bubble_error(&mut exec_ctx, Err(e)).map_err(|e| convert_error(&ctx, e)),
                        );
                    }
                }
                exec_ctx.advance();
                None
            }) {
                return res;
            }
        }
        arena.collect_debt();
    }
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Frame<'a> {
    Simple,
    Catch {
        target: usize,
        stack_height: usize,
        filter_class: Option<GcRefLock<'a, ClassValue<'a>>>,
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

#[derive(Collect)]
#[collect(no_drop)]
pub struct EnvFrame<'a> {
    pub frame: Frame<'a>,
    pub env: Gc<'a, Environment<'a>>,
}

impl<'a> EnvFrame<'a> {
    fn new(frame: Frame<'a>, env: Gc<'a, Environment<'a>>) -> Self {
        Self { frame, env }
    }
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ExecContext<'a> {
    pub stack: Vec<Value<'a>>,
    pub frame_stack: Vec<EnvFrame<'a>>,
    pub ip: usize,
    pub jumped: bool,
}

impl<'a> ExecContext<'a> {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            frame_stack: Vec::new(),
            ip: 0,
            jumped: false,
        }
    }
    pub fn push_frame(&mut self, frame: Frame<'a>, env: Gc<'a, Environment<'a>>) {
        self.frame_stack.push(EnvFrame::new(frame, env));
    }
    pub fn pop_frame(&mut self) -> EnvFrame<'a> {
        self.frame_stack.pop().expect("Frame stack empty")
    }
    pub fn push(&mut self, val: Value<'a>) {
        self.stack.push(val);
    }
    pub fn pop(&mut self) -> Value<'a> {
        self.stack.pop().expect("Stack empty")
    }
    pub fn jump(&mut self, target: usize) {
        self.ip = target;
        self.jumped = true;
    }
    pub fn advance(&mut self) {
        if !self.jumped {
            self.ip += 1;
        } else {
            self.jumped = false;
        }
    }
    pub fn call_fn(&mut self, env: Gc<'a, Environment<'a>>, func: CodePointer) {
        let return_ip = self.ip + 1;
        self.push_frame(
            Frame::Function {
                return_ip,
                stack_height: self.stack.len(),
            },
            env,
        );
        self.jump(func);
    }
    pub fn enter_loop(
        &mut self,
        env: Gc<'a, Environment<'a>>,
        break_target: usize,
        continue_target: usize,
    ) {
        self.push_frame(
            Frame::Loop {
                break_target,
                continue_target,
                stack_height: self.stack.len(),
            },
            env,
        );
    }
    pub fn enter_catch(
        &mut self,
        env: Gc<'a, Environment<'a>>,
        target: usize,
        filter_class: Option<GcRefLock<'a, ClassValue<'a>>>,
    ) {
        self.push_frame(
            Frame::Catch {
                target,
                filter_class,
                stack_height: self.stack.len(),
            },
            env,
        );
    }
}

fn bubble_error<'a>(
    exec_ctx: &mut ExecContext<'a>,
    err: Result<Value<'a>, LocatedError<'a>>,
) -> Result<(), LocatedError<'a>> {
    let err = err.unwrap_err();
    while !exec_ctx.frame_stack.is_empty() {
        let frame = exec_ctx.pop_frame();
        if let Frame::Catch {
            target,
            stack_height,
            filter_class,
        } = frame.frame
        {
            if let Some(filter_class) = filter_class {
                if !is_instance_of(&err.data, filter_class) {
                    continue; // Keep bubbling
                }
            }
            exec_ctx.stack.truncate(stack_height);
            exec_ctx.jump(target);
            exec_ctx.push(err.data);
            return Ok(());
        }
    }
    return Err(err);
}
fn bubble_control_flow<'a>(
    ctx: &Context<'a>,
    exec_ctx: &mut ExecContext<'a>,
    is_break: bool,
    location: &Location,
) -> Result<(), LocatedError<'a>> {
    while !exec_ctx.frame_stack.is_empty() {
        let frame = exec_ctx.pop_frame();
        match frame.frame {
            Frame::Loop {
                break_target,
                continue_target,
                stack_height,
            } => {
                exec_ctx.stack.truncate(stack_height);
                let target = if is_break {
                    break_target
                } else {
                    continue_target
                };
                exec_ctx.frame_stack.push(frame);
                exec_ctx.jump(target);
                return Ok(());
            }
            Frame::Function { .. } => {
                return unhandled_control_flow(ctx, location);
            }
            _ => {}
        }
    }
    return unhandled_control_flow(ctx, location);
}

fn bubble_return<'a>(
    ctx: &Context<'a>,
    exec_ctx: &mut ExecContext<'a>,
    ret: Value<'a>,
    location: &Location,
) -> Result<(), LocatedError<'a>> {
    while !exec_ctx.frame_stack.is_empty() {
        let frame = exec_ctx.pop_frame();
        if let Frame::Function {
            return_ip,
            stack_height,
        } = frame.frame
        {
            exec_ctx.stack.truncate(stack_height);
            let target = return_ip;
            exec_ctx.jump(target);
            exec_ctx.push(ret);
            return Ok(());
        }
    }
    return unhandled_control_flow(ctx, location);
}

pub fn eval_instruction<'a>(
    ctx: &Context<'a>,
    exec_ctx: &mut ExecContext<'a>,
    code: &Vec<Located<Instruction>>,
) -> Result<bool, LocatedError<'a>> {
    let instruction = &code[exec_ctx.ip];
    let location = &instruction.location;
    let env = exec_ctx.frame_stack.last().expect("Frame stack empty").env;

    /*
    println!(
        "IP {}: {:?} at {}:{}",
        exec_ctx.ip, instruction.data, instruction.location.module, instruction.location.span.start
    );
    println!(
        "Frames: {:?}",
        exec_ctx
            .frame_stack
            .iter()
            .map(|f| &f.frame)
            .collect::<Vec<_>>()
    );
    */

    match &instruction.data {
        Instruction::Exit => return Ok(false),
        Instruction::InitializeModule(path) => {
            let module = ModuleTree::get(ctx.root.root_module, path);
            let Some(module) = module else {
                return undefined_variable(ctx, &path.to_string(), location);
            };
            let mut mod_ref = module.borrow_mut(ctx.mc);
            if !mod_ref.is_initialized
                && let Some(pointer) = mod_ref.initializer
            {
                mod_ref.is_initialized = true;
                exec_ctx.call_fn(mod_ref.env, pointer);
            } else {
                exec_ctx.push(Value::Null);
            }
        }
        Instruction::DuplicateTopN(n) => {
            let stack_len = exec_ctx.stack.len();
            let val = exec_ctx.stack[stack_len - n..].to_vec();
            exec_ctx.stack.extend(val);
        }
        Instruction::Load(ident) => eval_identifier(ctx, exec_ctx, ident, env, location)?,
        Instruction::Store(ident) => {
            let val = exec_ctx.pop();
            assign_identifier(ctx, ident, env, location, &val)?;
            exec_ctx.push(val);
        }
        Instruction::Define(pattern) => {
            let value = exec_ctx.pop();
            let pattern = resolve_pattern(ctx, pattern, location, value)?;
            for (name, val) in pattern {
                if !env.define(ctx, name, val) {
                    return duplicate_variable_definition(ctx, name, location);
                }
            }
        }
        Instruction::DefineCanonic(canonical_path, initializer) => {
            let Some(module) = ModuleTree::get(ctx.root.root_module, &canonical_path.path) else {
                panic!("Module not found: {}", canonical_path.path)
            };
            let mod_env = module.borrow().env;
            let item = Value::Lazy(ctx.gc_lock(LazyValue::Uninitialized {
                body: *initializer,
                env: mod_env,
            }));
            if !mod_env.define_const(ctx, &canonical_path.item, item) {
                return duplicate_variable_definition(ctx, &canonical_path.item, location);
            }
        }
        Instruction::LoadProperty(field) => {
            let object = exec_ctx.pop();
            let val = get_property(ctx, &object, field, location)?;
            exec_ctx.push(val);
        }
        Instruction::StoreProperty(field) => {
            let val = exec_ctx.pop();
            let object = exec_ctx.pop();
            set_property(ctx, location, &object, field, &val)?;
            exec_ctx.push(val);
        }
        Instruction::LoadIndex => {
            let index = exec_ctx.pop();
            let array = exec_ctx.pop();
            let res = get_at_index(ctx, location, &array, index)?;
            exec_ctx.push(res);
        }
        Instruction::StoreIndex => {
            let val = exec_ctx.pop();
            let index = exec_ctx.pop();
            let array = exec_ctx.pop();
            set_at_index(ctx, location, &array, index, &val)?;
            exec_ctx.push(val);
        }
        Instruction::Pop => {
            exec_ctx.pop();
        }
        Instruction::PushNull => exec_ctx.push(Value::Null),
        Instruction::PushBool(b) => exec_ctx.push(Value::Bool(*b)),
        Instruction::PushInteger(i) => {
            exec_ctx.push(Value::Integer(IntVariant::from_digit_string(i)))
        }
        Instruction::PushNumber(num) => exec_ctx.push(Value::Number(*num)),
        Instruction::PushString(s) => {
            exec_ctx.push(Value::String(ctx.gc(StringVariant::from_string(s))))
        }
        Instruction::PushArray(items) => {
            let item_vec = pop_spread_args(ctx, exec_ctx, location, items)?;
            exec_ctx.push(Value::Array(ctx.gc_lock(item_vec)));
        }
        Instruction::PushObject(items) => {
            let entries: Vec<(Option<String>, Value<'a>)> = items
                .iter()
                .rev()
                .map(|key| (key.clone(), exec_ctx.pop()))
                .collect();
            let mut entry_map = HashMap::new();
            for (key, value) in entries.into_iter().rev() {
                if let Some(key) = key {
                    entry_map.insert(key, value);
                } else {
                    if let Value::Object(obj) = value {
                        let obj_ref = obj.borrow();
                        for (k, v) in obj_ref.iter() {
                            entry_map.insert(k.clone(), v.clone());
                        }
                    } else {
                        return type_error(
                            ctx,
                            "Spread operator can only be applied to objects",
                            &location,
                        );
                    };
                }
            }
            exec_ctx.push(Value::Object(ctx.gc_lock(entry_map)));
        }
        Instruction::Raise => {
            let err = exec_ctx.pop();
            return Err(LocatedError {
                data: err,
                location: location.clone(),
            });
        }
        Instruction::PushFunction {
            parameters,
            body_pointer,
        } => {
            let func = Value::Function(ctx.gc_lock(FunctionValue::Function {
                parameters: StaticCollect(parameters.clone()),
                body: *body_pointer,
                env,
            }));
            exec_ctx.push(func);
        }
        Instruction::PushClass {
            parent,
            methods,
            static_methods,
            constructor,
        } => {
            let parent = if *parent {
                let Value::Class(parent) = exec_ctx.pop() else {
                    return type_error(ctx, "Parent must be a class", &location);
                };
                Some(parent)
            } else {
                None
            };
            let class = Value::Class(
                ctx.gc_lock(ClassValue {
                    constructor: constructor.as_ref().map(|(params, constructor)| {
                        ctx.gc_lock(FunctionValue::Function {
                            parameters: StaticCollect(params.clone()),
                            body: *constructor,
                            env,
                        })
                    }),
                    methods: methods
                        .iter()
                        .map(|(name, params, body)| {
                            (
                                name.clone(),
                                ctx.gc_lock(FunctionValue::Function {
                                    parameters: StaticCollect(params.clone()),
                                    body: *body,
                                    env,
                                }),
                            )
                        })
                        .collect(),
                    static_methods: static_methods
                        .iter()
                        .map(|(name, params, body)| {
                            (
                                name.clone(),
                                ctx.gc_lock(FunctionValue::Function {
                                    parameters: StaticCollect(params.clone()),
                                    body: *body,
                                    env,
                                }),
                            )
                        })
                        .collect(),
                    parent,
                }),
            );
            exec_ctx.push(class);
        }
        Instruction::MakeInstance => {
            let inner = exec_ctx.pop();
            let Value::Class(class) = exec_ctx.pop() else {
                return type_error(ctx, "Attempted to instantiate a non-class value", &location);
            };
            let instance = Value::ClassInstance(ctx.gc_lock(ClassInstanceValue { inner, class }));
            exec_ctx.push(instance);
        }
        Instruction::Break => {
            bubble_control_flow(ctx, exec_ctx, true, &location)?;
        }
        Instruction::Continue => {
            bubble_control_flow(ctx, exec_ctx, false, &location)?;
        }
        Instruction::Return => {
            let ret = exec_ctx.pop();
            bubble_return(ctx, exec_ctx, ret, &location)?;
        }
        Instruction::Jump(target) => {
            exec_ctx.jump(*target);
        }
        Instruction::JumpIfFalse(target) => {
            let condition = exec_ctx.pop();
            if let Value::Bool(b) = condition {
                if !b {
                    exec_ctx.jump(*target);
                }
            } else {
                return type_error(ctx, "Condition must be a boolean", &location);
            };
        }
        Instruction::EnterBlock => {
            let block_env = ctx.gc(Environment::new(ctx, env));
            exec_ctx.push_frame(Frame::Simple, block_env);
        }
        Instruction::EnterLoop {
            break_target,
            continue_target,
        } => {
            exec_ctx.enter_loop(env, *break_target, *continue_target);
        }
        Instruction::EnterTryCatch {
            catch_target,
            filtered,
        } => {
            let filter_class = if *filtered {
                let Value::Class(class) = exec_ctx.pop() else {
                    return type_error(ctx, "Expected class for exception filter", location);
                };
                Some(class)
            } else {
                None
            };
            exec_ctx.enter_catch(env, *catch_target, filter_class);
        }
        Instruction::ExitFrame => match exec_ctx.pop_frame().frame {
            Frame::Function {
                return_ip,
                stack_height,
            } => {
                //Truncation is only necessary for control flow jumps, not normal function exits
                //as the stack should be at the correct height already
                debug_assert_eq!(exec_ctx.stack.len(), stack_height + 1); // Plus return value
                exec_ctx.jump(return_ip);
            }
            Frame::Catch { stack_height, .. } => {
                debug_assert_eq!(exec_ctx.stack.len(), stack_height + 1);
            }
            Frame::Loop { stack_height, .. } => {
                debug_assert_eq!(exec_ctx.stack.len(), stack_height + 1);
            }
            Frame::Simple => {}
        },
        Instruction::CallFunction(args) => {
            let args = pop_spread_args(ctx, exec_ctx, location, args)?;
            let function = exec_ctx.pop();
            call_function(ctx, exec_ctx, location, env, args, &function)?;
        }
        Instruction::TryShortCircuit(op, target) => {
            if interpret_short_circuit(op, exec_ctx.stack.last().expect("Stack empty")) {
                exec_ctx.jump(*target);
            }
        }
        Instruction::BinaryOp(op) => {
            let right = exec_ctx.pop();
            let left = exec_ctx.pop();
            let res = interpret_simple_binary_op(ctx, op, left, right, location)?;
            exec_ctx.push(res);
        }
        Instruction::UnaryOp(op) => {
            let val = exec_ctx.pop();
            let res = interpret_simple_unary_op(ctx, op, val, location)?;
            exec_ctx.push(res);
        }
        Instruction::CallConstructor(arg_count) => {
            let mut args = Vec::with_capacity(*arg_count);
            for _ in 0..*arg_count {
                args.push(exec_ctx.pop());
            }
            args.reverse();
            let Value::Class(class) = exec_ctx.pop() else {
                return type_error(
                    ctx,
                    "Attempted to call constructor on a non-class value",
                    location,
                );
            };
            let class_ref = class.borrow();
            let Some(constructor) = &class_ref.constructor else {
                return cannot_instantiate(ctx, "Class does not have a constructor", location);
            };

            exec_ctx.push(Value::Class(class));

            call_function(
                ctx,
                exec_ctx,
                location,
                env,
                args,
                &Value::Function(*constructor),
            )?;
        }
        Instruction::InitializeLazy(path) => {
            let module_tree = ModuleTree::get(ctx.root.root_module, &path.path);
            let lazy = if let Some(module_tree) = module_tree {
                let env = module_tree.borrow().env;
                env.get(&path.item)
            } else {
                return undefined_variable(ctx, &path.to_string(), location);
            };
            let Some(lazy) = lazy else {
                return undefined_variable(ctx, &path.to_string(), location);
            };
            let Value::Lazy(lazy_ref) = lazy else {
                return type_error(ctx, "Attempted to initialize a non-lazy value", location);
            };
            let value = exec_ctx.pop();
            let mut lazy_borrow = lazy_ref.borrow_mut(ctx.mc);
            match &*lazy_borrow {
                LazyValue::Initialized(_) => {
                    return type_error(
                        ctx,
                        "Attempted to initialize an already initialized lazy value",
                        location,
                    );
                }
                _ => {
                    *lazy_borrow = LazyValue::Initialized(value.clone());
                    exec_ctx.push(value);
                }
            }
        }
    };
    Ok(true)
}

fn pop_spread_args<'a>(
    ctx: &Context<'a>,
    exec_ctx: &mut ExecContext<'a>,
    location: &Location,
    items: &Vec<bool>,
) -> Result<Vec<Value<'a>>, LocatedError<'a>> {
    let mut items: Vec<(Value<'a>, bool)> = items
        .iter()
        .rev()
        .map(|is_spread| (exec_ctx.pop(), *is_spread))
        .collect();
    items.reverse();
    let mut item_vec = Vec::new();
    for (item, is_spread) in items {
        if is_spread {
            if let Value::Array(arr) = item {
                let arr_ref = arr.borrow();
                item_vec.extend_from_slice(&arr_ref);
            } else {
                return type_error(
                    ctx,
                    "Spread operator can only be applied to arrays",
                    &location,
                );
            }
        } else {
            item_vec.push(item);
        }
    }
    Ok(item_vec)
}

fn set_at_index<'a>(
    ctx: &Context<'a>,
    location: &Location,
    array: &Value<'a>,
    index: Value<'a>,
    result: &Value<'a>,
) -> Result<(), LocatedError<'a>> {
    let array = if let Value::ClassInstance(instance) = array {
        let mut cur = *instance;
        while let Value::ClassInstance(ci) = &cur.borrow().inner {
            cur = *ci;
        }
        &cur.borrow().inner
    } else {
        array
    };
    match array {
        Value::Array(arr) => {
            let Value::Integer(i) = index else {
                return type_error(ctx, "Array index must be an integer", location);
            };
            let mut arr_ref = arr.borrow_mut(ctx.mc);
            if let Some(index) = i.try_to_usize()
                && index < arr_ref.len()
            {
                arr_ref[index] = result.clone();
                Ok(())
            } else {
                array_index_out_of_bounds(ctx, i, location)
            }
        }
        Value::String(_) => type_error(
            ctx,
            "Strings are immutable; cannot assign to index",
            location,
        ),
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            let Value::String(s) = index else {
                return type_error(ctx, "Object index must be an string", location);
            };
            let s = s.to_string();
            if obj_ref.contains_key(&s) {
                drop(obj_ref);
                let mut obj_ref = obj.borrow_mut(ctx.mc);
                obj_ref.insert(s, result.clone());
                Ok(())
            } else {
                field_access_error(ctx, &s, location)
            }
        }
        Value::TypedBuffer {
            buffer,
            buffer_type,
        } => {
            let Value::Integer(i) = index else {
                return type_error(ctx, "Array index must be an integer", location);
            };
            if let Some(index) = i.try_to_usize() {
                if let Some(_) = buffer_type.try_write_buffer(ctx, *buffer, index, result) {
                    Ok(())
                } else {
                    type_error(ctx, "Failed to write value to typed buffer", location)
                }
            } else {
                array_index_out_of_bounds(ctx, i, location)
            }
        }
        _ => type_error(ctx, "Attempted to index a non-array value", location),
    }
}

fn call_function<'a>(
    ctx: &Context<'a>,
    exec_ctx: &mut ExecContext<'a>,
    location: &Location,
    env: Gc<'a, Environment<'a>>,
    args: Vec<Value<'a>>,
    function: &Value<'a>,
) -> Result<(), LocatedError<'a>> {
    let Value::Function(f) = function else {
        return type_error(ctx, "Attempted to call a non-function value", location);
    };
    match *f.borrow() {
        FunctionValue::Builtin { ref func } => {
            // No call environment for builtin functions, as they won't define variables
            exec_ctx.push(func(ctx, args, location, env)?);
            Ok(())
        }
        FunctionValue::Curried {
            func,
            ref bound_args,
        } => {
            let args = bound_args.iter().cloned().chain(args.into_iter()).collect();
            call_function(ctx, exec_ctx, location, env, args, &Value::Function(func))
        }
        FunctionValue::Function {
            ref parameters,
            body,
            env: func_env,
        } => {
            let args_len = args.len();
            let param_len = parameters.parameters.len();
            if let Some(rest) = &parameters.rest_parameter {
                if args_len < param_len {
                    type_error(
                        ctx,
                        &format!(
                            "Expected at least {} arguments, got {}",
                            param_len, args_len
                        ),
                        location,
                    )
                } else {
                    let call_env = ctx.gc(Environment::new(ctx, func_env));
                    let mut args_iter = args.into_iter();
                    for (param, arg) in parameters
                        .parameters
                        .iter()
                        .zip(args_iter.by_ref().take(param_len))
                    {
                        call_env.define(ctx, param, arg);
                    }
                    call_env.define(ctx, rest, Value::Array(ctx.gc_lock(args_iter.collect())));
                    exec_ctx.call_fn(call_env, body);
                    Ok(())
                }
            } else {
                if args_len != param_len {
                    type_error(
                        ctx,
                        &format!("Expected {} arguments, got {}", param_len, args_len),
                        location,
                    )
                } else {
                    let call_env = ctx.gc(Environment::new(ctx, func_env));
                    for (param, arg) in parameters.parameters.iter().zip(args.into_iter()) {
                        call_env.define(ctx, param, arg);
                    }
                    exec_ctx.call_fn(call_env, body);
                    Ok(())
                }
            }
        }
    }
}

fn interpret_short_circuit<'a>(op: &BinaryOp, left: &Value<'a>) -> bool {
    match (op, &left) {
        (BinaryOp::And, Value::Bool(false)) => true,
        (BinaryOp::Or, Value::Bool(true)) => true,
        (BinaryOp::NullCoalesce, Value::Null) => false,
        (BinaryOp::NullCoalesce, _) => true,
        _ => return false,
    }
}

fn interpret_simple_unary_op<'a>(
    ctx: &Context<'a>,
    op: &UnaryOp,
    value: Value<'a>,
    location: &Location,
) -> Result<Value<'a>, LocatedError<'a>> {
    Ok(match (op, value) {
        (UnaryOp::Negate, Value::Number(v)) => Value::Number(-v),
        (UnaryOp::Negate, Value::Integer(v)) => Value::Integer(v.neg()),
        (UnaryOp::Not, Value::Integer(v)) => Value::Integer(v.invert()),
        (UnaryOp::Not, Value::Bool(v)) => Value::Bool(!v),
        (_, v) => return unsupported_unary_operation(ctx, op, &v, location),
    })
}

fn interpret_simple_binary_op<'a>(
    ctx: &Context<'a>,
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
                .ok_or_else(|| division_by_zero::<Value<'a>>(ctx, location).unwrap_err())?,
        ),
        (BinaryOp::Divide, Value::Integer(l), Value::Number(r)) => Value::Number(l.to_f64() / r),
        (BinaryOp::Divide, Value::Number(l), Value::Integer(r)) => Value::Number(l / r.to_f64()),

        (BinaryOp::Modulo, Value::Number(l), Value::Number(r)) => Value::Number(l % r),
        (BinaryOp::Modulo, Value::Integer(l), Value::Integer(r)) => Value::Integer(
            l.rem(r)
                .ok_or_else(|| division_by_zero::<Value<'a>>(ctx, location).unwrap_err())?,
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
            return unsupported_binary_operation(ctx, op, &left, &right, location);
        }
    })
}

pub fn get_std_env<'a>(ctx: &Context<'a>) -> Gc<'a, Environment<'a>> {
    let std_tree = ModuleTree::get(ctx.root.root_module, &ModulePath::std()).unwrap();
    let std_env = std_tree.borrow().env;
    std_env
}

fn get_class_maybe_lazy<'a>(
    env: Gc<'a, Environment<'a>>,
    name: &str,
) -> Option<GcRefLock<'a, ClassValue<'a>>> {
    let val = env.get(name)?;
    match val {
        Value::Class(c) => Some(c),
        Value::Lazy(c) => match &*c.borrow() {
            LazyValue::Initialized(Value::Class(c)) => Some(*c),
            _ => None,
        },
        _ => None,
    }
}

fn get_classes<'a>(
    ctx: &Context<'a>,
    target: &Value<'a>,
) -> (
    Option<GcRefLock<'a, ClassValue<'a>>>,
    GcRefLock<'a, ClassValue<'a>>,
) {
    let class_name = target.get_type();
    let std_env = get_std_env(ctx);
    let Some(value_class) = get_class_maybe_lazy(std_env, class_name) else {
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
    ctx: &Context<'a>,
    target: &Value<'a>,
    field: &str,
    location: &Location,
) -> Result<Value<'a>, LocatedError<'a>> {
    let inner_target = if let Value::ClassInstance(instance) = target {
        let mut cur = *instance;
        while let Value::ClassInstance(inner_instance) = &cur.borrow().inner {
            cur = *inner_instance;
        }
        &cur.borrow().inner
    } else {
        target
    };

    match inner_target {
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            if let Some(val) = obj_ref.get(field).cloned() {
                return Ok(val);
            }
        }
        _ => {}
    }

    let (extra_class, value_class) = get_classes(ctx, target);

    fn look_for_property_in_class<'a>(
        ctx: &Context<'a>,
        target: &Value<'a>,
        mut class: GcRefLock<'a, ClassValue<'a>>,
        field: &str,
        is_class: bool,
    ) -> Option<Result<Value<'a>, LocatedError<'a>>> {
        loop {
            let cur_ref = class.borrow();
            if let Some(val) = cur_ref.methods.get(field) {
                if is_class {
                    return Some(Ok(Value::Function(*val)));
                } else {
                    let func_val = Value::Function(
                        ctx.gc_lock(FunctionValue::curry(*val, vec![target.clone()])),
                    );
                    return Some(Ok(func_val));
                }
            } else if is_class && let Some(val) = cur_ref.static_methods.get(field) {
                return Some(Ok(Value::Function(*val)));
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

    let is_class = matches!(target, Value::Class(_));
    if let Some(extra_class) = extra_class
        && let Some(res) = look_for_property_in_class(ctx, target, extra_class, field, is_class)
    {
        res
    } else if let Some(res) = look_for_property_in_class(ctx, target, value_class, field, is_class)
    {
        res
    } else {
        field_access_error(ctx, field, location)
    }
}

fn get_at_index<'a>(
    ctx: &Context<'a>,
    location: &Location,
    array: &Value<'a>,
    index: Value<'a>,
) -> Result<Value<'a>, LocatedError<'a>> {
    let array = if let Value::ClassInstance(instance) = array {
        let mut cur = *instance;
        while let Value::ClassInstance(inner_instance) = &cur.borrow().inner {
            cur = *inner_instance;
        }
        &cur.borrow().inner
    } else {
        &array
    };
    match array {
        Value::Array(arr) => {
            let Value::Integer(i) = index else {
                return type_error(ctx, "Array index must be an integer", location);
            };
            let arr_ref = arr.borrow();
            if let Some(index) = i.try_to_usize()
                && index < arr_ref.len()
            {
                let value = arr_ref[index].clone();
                Ok(value)
            } else {
                array_index_out_of_bounds(ctx, i, location)
            }
        }
        Value::String(s) => {
            let Value::Integer(i) = index else {
                return type_error(ctx, "String index must be an integer", location);
            };
            if let Some(index) = i.try_to_usize() {
                let Some(value) = StringVariant::slice(ctx, *s, index, 1) else {
                    return array_index_out_of_bounds(ctx, i, location);
                };
                Ok(Value::String(value))
            } else {
                array_index_out_of_bounds(ctx, i, location)
            }
        }
        Value::TypedBuffer {
            buffer,
            buffer_type,
        } => {
            let Value::Integer(i) = index else {
                return type_error(ctx, "Array index must be an integer", location);
            };
            if let Some(index) = i.try_to_usize() {
                if let Some(value) = buffer_type.try_read_buffer(*buffer, index) {
                    Ok(value)
                } else {
                    type_error(ctx, "Failed to read value from typed slice", location)
                }
            } else {
                array_index_out_of_bounds(ctx, i, &location)
            }
        }
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            let Value::String(s) = index else {
                return type_error(ctx, "Object index must be an string", location);
            };
            let s = s.to_string();
            if let Some(value) = obj_ref.get(&s).cloned() {
                Ok(value)
            } else {
                field_access_error(ctx, &s, location)
            }
        }
        _ => type_error(
            ctx,
            &format!("Attempted to index a value of type: {}", array.get_type()),
            &location,
        ),
    }
}

fn set_property<'a>(
    ctx: &Context<'a>,
    location: &Location,
    object: &Value<'a>,
    field: &String,
    result: &Value<'a>,
) -> Result<(), LocatedError<'a>> {
    let object = if let Value::ClassInstance(instance) = object {
        let mut cur = *instance;
        while let Value::ClassInstance(inner_instance) = &cur.borrow().inner {
            cur = *inner_instance;
        }
        &cur.borrow().inner
    } else {
        object
    };
    match object {
        Value::Object(obj) => {
            obj.borrow_mut(ctx.mc).insert(field.clone(), result.clone());
            Ok(())
        }
        _ => type_error(
            ctx,
            "Attempted to assign field on non-object value",
            &location,
        ),
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

fn eval_identifier<'a>(
    ctx: &Context<'a>,
    exec_ctx: &mut ExecContext<'a>,
    ident: &Identifier,
    env: Gc<'a, Environment<'a>>,
    location: &Location,
) -> Result<(), LocatedError<'a>> {
    let val = match ident {
        Identifier::Simple(name) => env.get(name),
        Identifier::Scoped(path) => {
            let module_tree = ModuleTree::get(ctx.root.root_module, &path.path);
            if let Some(module_tree) = module_tree {
                let env = module_tree.borrow().env;
                env.get(&path.item)
            } else {
                return undefined_variable(ctx, &path.to_string(), location);
            }
        }
    };
    let Some(val) = val else {
        let mut idents = Vec::new();
        env.collect_known_identifiers(&mut idents);
        return undefined_identifier(ctx, ident, location);
    };
    match val {
        Value::Lazy(lazy) => {
            let mut lazy_ref = lazy.borrow_mut(ctx.mc);
            match &*lazy_ref {
                LazyValue::BeingInitialized => {
                    return cyclic_static_initialization(ctx, ident, location);
                }
                LazyValue::Initialized(v) => {
                    exec_ctx.push(v.clone());
                }
                LazyValue::Uninitialized { body, env } => {
                    let body = *body;
                    let env = *env;
                    *lazy_ref = LazyValue::BeingInitialized;
                    drop(lazy_ref);
                    let call_env = ctx.gc(Environment::new(ctx, env));
                    exec_ctx.call_fn(call_env, body);
                }
            }
        }
        _ => exec_ctx.push(val.clone()),
    }
    Ok(())
}

fn assign_identifier<'a>(
    ctx: &Context<'a>,
    ident: &Identifier,
    env: Gc<'a, Environment<'a>>,
    location: &Location,
    value: &Value<'a>,
) -> Result<(), LocatedError<'a>> {
    match ident {
        Identifier::Simple(name) => Environment::set(ctx, env, name, value.clone(), &location),
        Identifier::Scoped(path) => {
            let module_tree = ModuleTree::get(ctx.root.root_module, &path.path);
            if let Some(module_tree) = module_tree {
                let env = module_tree.borrow().env;
                Environment::set(ctx, env, &path.item, value.clone(), &location)
            } else {
                undefined_variable(ctx, &path.to_string(), location)
            }
        }
    }
}

fn resolve_pattern<'a, 'b>(
    ctx: &Context<'a>,
    pattern: &'b Pattern,
    location: &Location,
    value: Value<'a>,
) -> Result<Vec<(&'b String, Value<'a>)>, LocatedError<'a>> {
    let mut stack = vec![(pattern, location, value)];
    let mut result = Vec::new();
    while let Some((pattern, location, value)) = stack.pop() {
        match &pattern {
            Pattern::Wildcard => continue,
            Pattern::Ident(name) => result.push((name, value.clone())),
            Pattern::Object { entries, rest } => {
                let Value::Object(obj) = &value else {
                    return type_error(ctx, "Expected object value for object pattern", location);
                };
                let obj_ref = obj.borrow();
                let mut remaining_fields = obj_ref.clone();
                for (item, pattern) in entries {
                    let Some(val) = remaining_fields.remove(item) else {
                        return field_access_error(ctx, item, location);
                    };
                    stack.push((&pattern.data, &pattern.location, val.clone()));
                }
                if let Some(rest) = rest {
                    let rest_obj = Value::Object(ctx.gc_lock(remaining_fields));
                    result.push((rest, rest_obj));
                }
            }
            Pattern::Array { items, rest } => {
                let Value::Array(arr) = &value else {
                    return type_error(ctx, "Expected array value for array pattern", location);
                };
                let arr_ref = arr.borrow();
                for (i, pattern) in items.iter().enumerate() {
                    let Some(val) = arr_ref.get(i) else {
                        return index_out_of_bounds(ctx, i, location);
                    };
                    stack.push((&pattern.data, &pattern.location, val.clone()));
                }
                if let Some(rest) = rest
                    && items.len() <= arr_ref.len()
                {
                    let rest_obj = Value::Array(ctx.gc_lock(arr_ref[items.len()..].to_vec()));
                    result.push((rest, rest_obj));
                }
            }
        }
    }
    Ok(result)
}
