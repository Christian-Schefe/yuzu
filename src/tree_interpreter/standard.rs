use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::tree_interpreter::{
    Environment, PrototypeValue, RuntimeError, Value, variable_to_string,
};

pub fn instantiate_prototype(
    p: HashMap<String, Value>,
) -> impl Fn(Vec<Value>) -> Result<Value, RuntimeError> {
    move |_| Ok(Value::Object(Rc::new(RefCell::new(p.clone()))))
}

pub fn define_globals(env: &Environment) {
    env.define(
        "print",
        Value::BuiltinFunction(Rc::new(|args| {
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
        Value::BuiltinFunction(Rc::new(|args| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                )));
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
}

fn integer_prototype() -> Value {
    let mut map = HashMap::new();
    map.insert(
        "to_string".to_string(),
        Value::BuiltinFunction(Rc::new(|args| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                )));
            }
            match &args[0] {
                Value::Integer(i) => Ok(Value::String(i.to_string().chars().collect())),
                _ => Err(RuntimeError::TypeError(
                    "to_string can only be called on Integer".to_string(),
                )),
            }
        })),
    );
    map.insert(
        "new".to_string(),
        Value::BuiltinFunction(Rc::new(|args| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 0 arguments, got {}",
                    args.len()
                )));
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
        Value::BuiltinFunction(Rc::new(|args| {
            if args.len() < 3 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected at least 2 arguments, got {}",
                    args.len()
                )));
            }
            let strings = args
                .iter()
                .skip(1)
                .map(|arg| match arg {
                    Value::String(s) => Ok(s.iter().collect::<String>()),
                    _ => Err(RuntimeError::TypeError(
                        "concat can only be called on String".to_string(),
                    )),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::String(strings.concat().chars().collect()))
        })),
    );
    Value::Prototype(Rc::new(PrototypeValue { properties: map }))
}
