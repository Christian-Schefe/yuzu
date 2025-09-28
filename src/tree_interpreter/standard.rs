use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::tree_interpreter::{
    BuiltinFunctionValue, Environment, GLOBAL_STATE, LocatedControlFlow, Location, PrototypeValue,
    RootPrototypes, RuntimeError, Value, import_module, interpret_string, variable_to_string,
};

fn make_builtin_function(
    func: impl Fn(Vec<Value>, &Location, Rc<Environment>) -> Result<Value, LocatedControlFlow> + 'static,
) -> Value {
    Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
        func: Box::new(func),
        is_static: false,
    }))
}

fn make_static_builtin_function(
    func: impl Fn(Vec<Value>, &Location, Rc<Environment>) -> Result<Value, LocatedControlFlow> + 'static,
) -> Value {
    Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
        func: Box::new(func),
        is_static: true,
    }))
}

pub fn define_globals(env: &Environment, with_std: bool) {
    env.define(
        "print",
        make_builtin_function(|args, _, _| {
            let str = args
                .into_iter()
                .map(|arg| variable_to_string(&arg))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", str);
            Ok(Value::Null)
        }),
    );
    env.define(
        "import",
        make_builtin_function(|args, span, env| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .span(&span));
            }
            let path = match &args[0] {
                Value::String(s) => s.iter().collect::<String>(),
                _ => {
                    return Err(RuntimeError::TypeError(
                        "import argument must be a string".to_string(),
                    )
                    .span(span));
                }
            };
            import_module(&path, &span, env)
        }),
    );
    env.define(
        "typeof",
        make_builtin_function(|args, span, _| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .span(span));
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
        }),
    );

    GLOBAL_STATE.with(|state| {
        let root_prototypes = &state.get().unwrap().borrow().root_prototypes;
        env.define("Integer", Value::Prototype(root_prototypes.integer.clone()));
        env.define("String", Value::Prototype(root_prototypes.string.clone()));
        env.define("Array", Value::Prototype(root_prototypes.array.clone()));
        env.define("Object", Value::Prototype(root_prototypes.object.clone()));
        env.define("Bool", Value::Prototype(root_prototypes.bool.clone()));
        env.define("Number", Value::Prototype(root_prototypes.number.clone()));
        env.define("Null", Value::Prototype(root_prototypes.null.clone()));
        env.define(
            "Function",
            Value::Prototype(root_prototypes.function.clone()),
        );
        env.define(
            "BuiltinFunction",
            Value::Prototype(root_prototypes.builtin_function.clone()),
        );
        env.define(
            "Prototype",
            Value::Prototype(root_prototypes.prototype.clone()),
        );
    });

    if !with_std {
        return;
    }

    let Value::Object(std) = STD_PROTOTYPE.with(|cell| {
        cell.get_or_init(|| {
            interpret_string(
                STD,
                &Location::new(0..STD.len(), "std".to_string()),
                "std".to_string(),
                false,
            )
            .ok()
            .expect("Failed to interpret standard library")
        })
        .clone()
    }) else {
        panic!("Failed to interpret standard library");
    };

    for (key, value) in std.borrow().properties.iter() {
        if !env.define(key, value.clone()) {
            let prev = env.get(key).unwrap();
            let Value::Prototype(real_p) = prev else {
                panic!("Global {} in std is not a prototype, cannot merge", key);
            };
            let Value::Prototype(new_p) = value else {
                panic!("Global {} in std is not a prototype, cannot merge", key);
            };
            let mut real_p = real_p.borrow_mut();
            let new_p = new_p.borrow();
            for (k, v) in new_p.properties.iter() {
                real_p.properties.insert(k.clone(), v.clone());
            }
        }
    }
}

pub fn root_prototypes() -> RootPrototypes {
    RootPrototypes {
        integer: integer_prototype(),
        string: string_prototype(),
        array: array_prototype(),
        object: object_prototype(),
        bool: Rc::new(RefCell::new(PrototypeValue {
            properties: HashMap::new(),
            parent: None,
        })),
        number: Rc::new(RefCell::new(PrototypeValue {
            properties: HashMap::new(),
            parent: None,
        })),
        null: Rc::new(RefCell::new(PrototypeValue {
            properties: HashMap::new(),
            parent: None,
        })),
        function: Rc::new(RefCell::new(PrototypeValue {
            properties: HashMap::new(),
            parent: None,
        })),
        builtin_function: Rc::new(RefCell::new(PrototypeValue {
            properties: HashMap::new(),
            parent: None,
        })),
        prototype: prototype_prototype(),
    }
}

fn object_prototype() -> Rc<RefCell<PrototypeValue>> {
    let mut map = HashMap::new();
    map.insert(
        "prototype".to_string(),
        make_builtin_function(|args, expr, _| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .span(expr));
            }
            match &args[0] {
                Value::Object(obj) => {
                    let proto = obj.borrow().prototype.clone();
                    Ok(Value::Prototype(proto))
                }
                _ => Err(RuntimeError::TypeError(
                    "prototype can only be called on Object".to_string(),
                )
                .span(expr)),
            }
        }),
    );
    Rc::new(RefCell::new(PrototypeValue {
        properties: map,
        parent: None,
    }))
}

fn prototype_prototype() -> Rc<RefCell<PrototypeValue>> {
    let mut map = HashMap::new();
    map.insert(
        "super".to_string(),
        make_static_builtin_function(|args, expr, _| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .span(expr));
            }
            match &args[0] {
                Value::Prototype(obj) => {
                    let proto = obj.borrow().parent.clone();
                    Ok(proto.map_or(Value::Null, Value::Prototype))
                }
                _ => Err(
                    RuntimeError::TypeError("super can only be called on Object".to_string())
                        .span(expr),
                ),
            }
        }),
    );
    Rc::new(RefCell::new(PrototypeValue {
        properties: map,
        parent: None,
    }))
}

fn integer_prototype() -> Rc<RefCell<PrototypeValue>> {
    let mut map = HashMap::new();
    map.insert(
        "to_string".to_string(),
        make_builtin_function(|args, expr, _| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .span(expr));
            }
            match &args[0] {
                Value::Integer(i) => Ok(Value::String(i.to_string().chars().collect())),
                _ => Err(RuntimeError::TypeError(
                    "to_string can only be called on Integer".to_string(),
                )
                .span(expr)),
            }
        }),
    );
    map.insert(
        "new".to_string(),
        make_builtin_function(|args, expr, _| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 0 arguments, got {}",
                    args.len()
                ))
                .span(expr));
            }
            Ok(Value::Integer(0))
        }),
    );
    Rc::new(RefCell::new(PrototypeValue {
        properties: map,
        parent: None,
    }))
}

fn string_prototype() -> Rc<RefCell<PrototypeValue>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        make_builtin_function(|args, expr, _| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .span(expr));
            }
            match &args[0] {
                Value::String(arr) => Ok(Value::Integer(arr.len() as i64)),
                _ => Err(RuntimeError::TypeError(
                    "length can only be called on String".to_string(),
                )
                .span(expr)),
            }
        }),
    );
    map.insert(
        "concat".to_string(),
        make_builtin_function(|args, expr, _| {
            if args.len() < 3 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected at least 2 arguments, got {}",
                    args.len()
                ))
                .span(expr));
            }
            let strings = args
                .iter()
                .skip(1)
                .map(|arg| match arg {
                    Value::String(s) => Ok(s.iter().collect::<String>()),
                    _ => Err(RuntimeError::TypeError(
                        "concat can only be called on String".to_string(),
                    )
                    .span(expr)),
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::String(strings.concat().chars().collect()))
        }),
    );
    Rc::new(RefCell::new(PrototypeValue {
        properties: map,
        parent: None,
    }))
}

fn array_prototype() -> Rc<RefCell<PrototypeValue>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        make_builtin_function(|args, expr, _| {
            if args.len() != 1 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected 1 argument, got {}",
                    args.len()
                ))
                .span(expr));
            }
            match &args[0] {
                Value::Array(arr) => Ok(Value::Integer(arr.borrow().len() as i64)),
                _ => Err(
                    RuntimeError::TypeError("length can only be called on Array".to_string())
                        .span(expr),
                ),
            }
        }),
    );
    map.insert(
        "push".to_string(),
        make_builtin_function(|args, expr, _| {
            if args.len() < 2 {
                return Err(RuntimeError::FunctionArgumentError(format!(
                    "Expected at least 1 argument, got {}",
                    args.len() - 1
                ))
                .span(expr));
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
                        .span(expr),
                ),
            }
        }),
    );
    Rc::new(RefCell::new(PrototypeValue {
        properties: map,
        parent: None,
    }))
}

pub const STD: &str = include_str!("../../reference/std.yuzu");
thread_local! {
    pub static STD_PROTOTYPE: OnceCell<Value> = OnceCell::new();
}
