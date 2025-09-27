use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::tree_interpreter::{
    BuiltinFunctionType, Environment, FunctionValue, PrototypeValue, RuntimeError, Value,
    interpret, variable_to_string,
};

pub fn instantiate_prototype(
    p: HashMap<String, Value>,
    constructor: Option<Rc<FunctionValue>>,
) -> BuiltinFunctionType {
    Rc::new(move |args, expr| {
        let this = Value::Object(Rc::new(RefCell::new(p.clone())));
        if let Some(constructor) = &constructor {
            let FunctionValue {
                parameters,
                body,
                env: func_env,
            } = &**constructor;
            let arg_count = args.len() + 1;
            if parameters.len() != arg_count {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected {} arguments, got {}",
                    parameters.len(),
                    arg_count
                ))
                .at(&expr));
            }
            let args_iter = std::iter::once(this.clone()).chain(args.into_iter());
            let call_env = Rc::new(Environment::new(Some(func_env.clone())));
            for (param, arg_val) in parameters.iter().zip(args_iter) {
                if !call_env.define(param, arg_val) {
                    unreachable!("Parameter name should not be duplicated: {}", param);
                }
            }
            interpret(&body, call_env)?;
        }
        Ok(this)
    })
}

pub fn define_globals(env: &Environment) {
    env.define(
        "print",
        Value::BuiltinFunction(Rc::new(|args, _| {
            let str = args
                .into_iter()
                .map(|arg| variable_to_string(&arg))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", str);
            Ok(Value::Null)
        })),
    );
    env.define(
        "typeof",
        Value::BuiltinFunction(Rc::new(|args, expr| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .at(expr));
            }
            let type_str = match &args[0] {
                Value::Null => "null",
                Value::Bool(_) => "boolean",
                Value::Number(_) => "number",
                Value::Integer(_) => "integer",
                Value::String(_) => "string",
                Value::Object(_) => "object",
                Value::Array(_) => "array",
                Value::Prototype { .. } => "prototype",
                Value::Function { .. } => "function",
                Value::BuiltinFunction { .. } => "builtin_function",
            };
            Ok(Value::String(type_str.chars().collect()))
        })),
    );
    env.define("Integer", integer_prototype());
    env.define("String", string_prototype());
    env.define("Array", array_prototype());
}

fn integer_prototype() -> Value {
    let mut map = HashMap::new();
    map.insert(
        "to_string".to_string(),
        Value::BuiltinFunction(Rc::new(|args, expr| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .at(expr));
            }
            match &args[0] {
                Value::Integer(i) => Ok(Value::String(i.to_string().chars().collect())),
                _ => Err(RuntimeError::TypeError(
                    "to_string can only be called on Integer".to_string(),
                )
                .at(expr)),
            }
        })),
    );
    map.insert(
        "new".to_string(),
        Value::BuiltinFunction(Rc::new(|args, expr| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 0 arguments, got {}",
                    args.len()
                ))
                .at(expr));
            }
            Ok(Value::Integer(0))
        })),
    );
    Value::Prototype(Rc::new(PrototypeValue { properties: map }))
}

fn string_prototype() -> Value {
    let mut map = HashMap::new();
    map.insert(
        "concat".to_string(),
        Value::BuiltinFunction(Rc::new(|args, expr| {
            if args.len() < 3 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected at least 2 arguments, got {}",
                    args.len()
                ))
                .at(expr));
            }
            let strings = args
                .iter()
                .skip(1)
                .map(|arg| match arg {
                    Value::String(s) => Ok(s.iter().collect::<String>()),
                    _ => Err(RuntimeError::TypeError(
                        "concat can only be called on String".to_string(),
                    )
                    .at(expr)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::String(strings.concat().chars().collect()))
        })),
    );
    Value::Prototype(Rc::new(PrototypeValue { properties: map }))
}

fn array_prototype() -> Value {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        Value::BuiltinFunction(Rc::new(|args, expr| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .at(expr));
            }
            match &args[0] {
                Value::Array(arr) => Ok(Value::Integer(arr.borrow().len() as i64)),
                _ => Err(
                    RuntimeError::TypeError("length can only be called on Array".to_string())
                        .at(expr),
                ),
            }
        })),
    );
    map.insert(
        "push".to_string(),
        Value::BuiltinFunction(Rc::new(|args, expr| {
            if args.len() < 2 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected at least 1 argument, got {}",
                    args.len() - 1
                ))
                .at(expr));
            }
            match &args[0] {
                Value::Array(arr) => {
                    let mut arr = arr.borrow_mut();
                    for arg in args.iter().skip(1) {
                        arr.push(arg.clone());
                    }
                    Ok(Value::Null)
                }
                _ => Err(
                    RuntimeError::TypeError("push can only be called on Array".to_string())
                        .at(expr),
                ),
            }
        })),
    );
    map.insert(
        "_iter".to_string(),
        Value::BuiltinFunction(Rc::new(|args, expr| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .at(expr));
            }
            match &args[0] {
                Value::Array(_) => {
                    let mut map = HashMap::new();
                    map.insert("target".to_string(), args[0].clone());
                    map.insert("current".to_string(), Value::Integer(-1));
                    map.insert(
                        "next".to_string(),
                        Value::BuiltinFunction(Rc::new(|args, expr| {
                            if args.len() != 1 {
                                return Err(RuntimeError::FunctionArgumentError(format!(
                                    "Expected 1 argument, got {}",
                                    args.len()
                                ))
                                .at(expr));
                            }
                            match &args[0] {
                                Value::Object(obj) => {
                                    let obj = obj.borrow();
                                    let target = obj.get("target").ok_or_else(|| {
                                        RuntimeError::FieldAccessError("target".to_string())
                                            .at(expr)
                                    })?;
                                    let current = obj.get("current").ok_or_else(|| {
                                        RuntimeError::FieldAccessError("current".to_string())
                                            .at(expr)
                                    })?;
                                    match (target, current) {
                                        (Value::Array(arr), Value::Integer(i)) => {
                                            let arr = arr.borrow();
                                            let next_index = *i + 1;
                                            if (next_index as usize) < arr.len() {
                                                let value = arr[next_index as usize].clone();
                                                let mut new_obj = obj.clone();
                                                new_obj.insert(
                                                    "current".to_string(),
                                                    Value::Integer(next_index),
                                                );
                                                Ok(value)
                                            } else {
                                                Ok(Value::Null)
                                            }
                                        }
                                        _ => Err(RuntimeError::TypeError(
                                            "Invalid iterator state".to_string(),
                                        )
                                        .at(expr)),
                                    }
                                }
                                _ => Err(RuntimeError::TypeError(
                                    "next can only be called on iterator object".to_string(),
                                )
                                .at(expr)),
                            }
                        })),
                    );
                    Ok(Value::Object(Rc::new(RefCell::new(map))))
                }
                _ => Err(
                    RuntimeError::TypeError("_iter can only be called on Array".to_string())
                        .at(expr),
                ),
            }
        })),
    );
    Value::Prototype(Rc::new(PrototypeValue { properties: map }))
}
