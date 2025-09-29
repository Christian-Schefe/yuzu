use std::{
    cell::{OnceCell, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::tree_interpreter::{
    BuiltinFunctionValue, Environment, GLOBAL_STATE, LocatedControlFlow, Location, PropertyKind,
    PrototypeValue, RootPrototypes, Value, function_argument_error, import_module,
    interpret_string, io_error,
    resource::{FileResource, SocketResource},
    type_error, variable_to_string,
};

fn make_builtin_function(
    func: impl Fn(Vec<Value>, &Location, Rc<Environment>) -> Result<Value, LocatedControlFlow> + 'static,
) -> Value {
    Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
        func: Box::new(func),
    }))
}

fn make_builtin_method(
    func: impl Fn(Vec<Value>, &Location, Rc<Environment>) -> Result<Value, LocatedControlFlow> + 'static,
) -> (Value, PropertyKind) {
    (
        Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
            func: Box::new(func),
        })),
        PropertyKind::Method,
    )
}

fn make_builtin_static_method(
    func: impl Fn(Vec<Value>, &Location, Rc<Environment>) -> Result<Value, LocatedControlFlow> + 'static,
) -> (Value, PropertyKind) {
    (
        Value::BuiltinFunction(Rc::new(BuiltinFunctionValue {
            func: Box::new(func),
        })),
        PropertyKind::StaticMethod,
    )
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
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    span,
                );
            }
            let path = match &args[0] {
                Value::String(s) => s.iter().collect::<String>(),
                _ => return type_error("Import argument must be a string", &env, span),
            };
            import_module(&path, &span, env)
        }),
    );

    env.define(
        "File",
        Value::Prototype(Rc::new(RefCell::new(PrototypeValue {
            properties: {
                let mut map = HashMap::new();
                map.insert(
                    "open".to_string(),
                    make_builtin_static_method(|args, span, env| {
                        if args.len() != 1 {
                            return function_argument_error(
                                &format!("Expected 1 argument, got {}", args.len()),
                                &env,
                                span,
                            );
                        }
                        let path = match &args[0] {
                            Value::String(s) => s.iter().collect::<String>(),
                            _ => return type_error("Open argument must be a string", &env, span),
                        };
                        let file = FileResource::open(&path).map_err(|e| {
                            io_error::<()>(
                                &format!("Failed to open file {}: {}", path, e),
                                &env,
                                span,
                            )
                            .unwrap_err()
                        })?;
                        Ok(Value::Resource(Rc::new(RefCell::new(file))))
                    }),
                );
                map
            },
            parent: None,
        }))),
    );

    env.define(
        "Socket",
        Value::Prototype(Rc::new(RefCell::new(PrototypeValue {
            properties: {
                let mut map = HashMap::new();
                map.insert(
                    "connect".to_string(),
                    make_builtin_static_method(|args, span, env| {
                        if args.len() != 2 {
                            return function_argument_error(
                                &format!("Expected 2 arguments, got {}", args.len()),
                                &env,
                                span,
                            );
                        }
                        let host = match &args[0] {
                            Value::String(s) => s.iter().collect::<String>(),
                            _ => return type_error("Host argument must be a string", &env, span),
                        };
                        let port = match &args[1] {
                            Value::Integer(i) => *i,
                            _ => return type_error("Port argument must be an integer", &env, span),
                        };
                        let port = if let Some(port) = TryInto::<u16>::try_into(port).ok() {
                            port
                        } else {
                            return type_error("Port argument must be a valid u16", &env, span);
                        };
                        let socket = SocketResource::new(host.clone(), port).map_err(|e| {
                            io_error::<()>(
                                &format!("Failed to connect to {}:{}: {}", host, port, e),
                                &env,
                                span,
                            )
                            .unwrap_err()
                        })?;
                        Ok(Value::Resource(Rc::new(RefCell::new(socket))))
                    }),
                );
                map
            },
            parent: None,
        }))),
    );

    env.define(
        "typeof",
        make_builtin_function(|args, span, env| {
            if args.len() != 1 {
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    span,
                );
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
                Value::Resource(_) => "resource",
                Value::Buffer(_) => "buffer",
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
        env.define(
            "Exception",
            Value::Prototype(root_prototypes.exception.clone()),
        );
        env.define(
            "Resource",
            Value::Prototype(root_prototypes.resource.clone()),
        );
        env.define("Buffer", Value::Prototype(root_prototypes.buffer.clone()));
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
                Some("std"),
                Rc::new(env.clone()),
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
            println!("Warning: Global {} in std shadows existing global", key);
            let prev = env.get(key).unwrap();
            let Value::Prototype(real_p) = prev else {
                panic!("Global {} in std is not a prototype, cannot merge", key);
            };
            let Value::Prototype(new_p) = value else {
                panic!("Global {} in std is not a prototype, cannot merge", key);
            };
            let mut real_p_ref = real_p.borrow_mut();
            let new_p = new_p.borrow();
            for (k, (v, kind)) in new_p.properties.iter() {
                let new_kind = match kind {
                    PropertyKind::Constructor(_) => PropertyKind::Constructor(real_p.clone()),
                    _ => kind.clone(),
                };
                real_p_ref
                    .properties
                    .insert(k.clone(), (v.clone(), new_kind));
            }
        }
    }
}

pub fn root_prototypes() -> RootPrototypes {
    let object_proto = object_prototype();
    RootPrototypes {
        integer: integer_prototype(),
        string: string_prototype(),
        array: array_prototype(),
        object: object_proto.clone(),
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
        exception: exception_prototype(object_proto),
        resource: resource_prototype(),
        buffer: buffer_prototype(),
    }
}

fn buffer_prototype() -> Rc<RefCell<PrototypeValue>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        make_builtin_method(|args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Buffer(buf) => Ok(Value::Integer(buf.borrow().len() as i64)),
                _ => return type_error("length can only be called on Buffer", &env, expr),
            }
        }),
    );
    map.insert(
        "to_string".to_string(),
        make_builtin_method(|args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Buffer(buf) => {
                    let s = String::from_utf8_lossy(&buf.borrow()).to_string();
                    Ok(Value::String(s.chars().collect()))
                }
                _ => return type_error("to_string can only be called on Buffer", &env, expr),
            }
        }),
    );
    Rc::new(RefCell::new(PrototypeValue {
        properties: map,
        parent: None,
    }))
}

fn resource_prototype() -> Rc<RefCell<PrototypeValue>> {
    let mut map = HashMap::new();
    map.insert(
        "close".to_string(),
        make_builtin_method(|args, span, env| {
            if args.len() != 1 {
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    span,
                );
            }
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut();
                    res_ref.close().map_err(|e| {
                        io_error::<()>(&format!("Failed to close resource: {}", e), &env, span)
                            .unwrap_err()
                    })?;
                    Ok(Value::Null)
                }
                _ => return type_error("Close argument must be a resource", &env, span),
            }
        }),
    );
    map.insert(
        "read".to_string(),
        make_builtin_method(|args, span, env| {
            if args.len() != 2 {
                return function_argument_error(
                    &format!("Expected 2 arguments, got {}", args.len()),
                    &env,
                    span,
                );
            }
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut();
                    let buf_size = match &args[1] {
                        Value::Integer(i) if *i > 0 => *i as usize,
                        _ => {
                            return type_error(
                                "Read size argument must be a positive integer",
                                &env,
                                span,
                            );
                        }
                    };
                    let mut buf = vec![0u8; buf_size];
                    let bytes_read = res_ref.read(&mut buf).map_err(|e| {
                        io_error::<()>(&format!("Failed to read from resource: {}", e), &env, span)
                            .unwrap_err()
                    })?;
                    buf.truncate(bytes_read);
                    Ok(Value::Buffer(Rc::new(RefCell::new(buf))))
                }
                _ => return type_error("Read argument must be a resource", &env, span),
            }
        }),
    );
    Rc::new(RefCell::new(PrototypeValue {
        properties: map,
        parent: None,
    }))
}

fn exception_prototype(parent: Rc<RefCell<PrototypeValue>>) -> Rc<RefCell<PrototypeValue>> {
    let map = HashMap::new();
    Rc::new(RefCell::new(PrototypeValue {
        properties: map,
        parent: Some(parent),
    }))
}

fn object_prototype() -> Rc<RefCell<PrototypeValue>> {
    let mut map = HashMap::new();
    map.insert(
        "prototype".to_string(),
        make_builtin_method(|args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Object(obj) => {
                    let proto = obj.borrow().prototype.clone();
                    Ok(Value::Prototype(proto))
                }
                _ => return type_error("prototype can only be called on Object", &env, expr),
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
        (
            make_builtin_function(|args, expr, env| {
                if args.len() != 1 {
                    return function_argument_error(
                        &format!("Expected 1 argument, got {}", args.len()),
                        &env,
                        expr,
                    );
                }
                match &args[0] {
                    Value::Prototype(obj) => {
                        let proto = obj.borrow().parent.clone();
                        Ok(proto.map_or(Value::Null, Value::Prototype))
                    }
                    _ => return type_error("super can only be called on Prototype", &env, expr),
                }
            }),
            PropertyKind::StaticMethod,
        ),
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
        make_builtin_method(|args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Integer(i) => Ok(Value::String(i.to_string().chars().collect())),
                _ => return type_error("to_string can only be called on Integer", &env, expr),
            }
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
        make_builtin_method(|args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::String(arr) => Ok(Value::Integer(arr.len() as i64)),
                _ => return type_error("length can only be called on String", &env, expr),
            }
        }),
    );
    map.insert(
        "concat".to_string(),
        make_builtin_method(|args, expr, env| {
            if args.len() < 2 {
                return function_argument_error(
                    &format!("Expected at least 2 arguments, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            let strings = args
                .iter()
                .map(|arg| match arg {
                    Value::String(s) => Ok(s.iter().collect::<String>()),
                    _ => return type_error("concat arguments must be strings", &env, expr),
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
        make_builtin_method(|args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Array(arr) => Ok(Value::Integer(arr.borrow().len() as i64)),
                _ => return type_error("length can only be called on Array", &env, expr),
            }
        }),
    );
    map.insert(
        "push".to_string(),
        make_builtin_method(|args, expr, env| {
            if args.len() < 2 {
                return function_argument_error(
                    &format!("Expected at least 1 argument, got {}", args.len() - 1),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Array(arr) => {
                    let mut arr = arr.borrow_mut();
                    for arg in args.iter().skip(1) {
                        arr.push(arg.clone());
                    }
                    Ok(Value::Null)
                }
                _ => return type_error("push can only be called on Array", &env, expr),
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
