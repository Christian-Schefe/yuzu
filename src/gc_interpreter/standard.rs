use std::collections::HashMap;

use gc_arena::{
    Gc, Mutation, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    gc_interpreter::{
        MyRoot, RootPrototypes,
        exception::{function_argument_error, io_error, type_error},
        import_module, interpret_string,
        resource::{FileResource, SocketResource},
        value::{
            BuiltinFunctionValue, Environment, FunctionKind, LocatedControlFlow, PrototypeValue,
            Value, variable_to_string,
        },
    },
    tree_interpreter::Location,
};

fn make_builtin_function<'a>(
    mc: &Mutation<'a>,
    func: impl for<'b> Fn(
        &Mutation<'b>,
        &MyRoot<'b>,
        Vec<Value<'b>>,
        &Location,
        Gc<'b, Environment<'b>>,
    ) -> Result<Value<'b>, LocatedControlFlow<'b>>
    + 'static,
) -> Value<'a> {
    Value::BuiltinFunction(Gc::new(
        mc,
        BuiltinFunctionValue {
            func: StaticCollect(Box::new(func)),
            kind: FunctionKind::Function,
        },
    ))
}

fn make_builtin_method<'a>(
    mc: &Mutation<'a>,
    func: impl for<'b> Fn(
        &Mutation<'b>,
        &MyRoot<'b>,
        Vec<Value<'b>>,
        &Location,
        Gc<'b, Environment<'b>>,
    ) -> Result<Value<'b>, LocatedControlFlow<'b>>
    + 'static,
) -> Value<'a> {
    Value::BuiltinFunction(Gc::new(
        mc,
        BuiltinFunctionValue {
            func: StaticCollect(Box::new(func)),
            kind: FunctionKind::Method,
        },
    ))
}

pub fn define_globals<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    env: Gc<'a, Environment<'a>>,
    no_std: bool,
) {
    env.define(
        mc,
        "print",
        make_builtin_function(mc, |_, _, args, _, _| {
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
        mc,
        "import",
        make_builtin_function(mc, |mc, root, args, span, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    span,
                );
            }
            let path = match &args[0] {
                Value::String(s) => s.iter().collect::<String>(),
                _ => return type_error(mc, root, "Import argument must be a string", &env, span),
            };
            import_module(mc, root, &path, &span, env)
        }),
    );

    env.define(
        mc,
        "File",
        Value::Prototype(Gc::new(
            mc,
            RefLock::new(PrototypeValue {
                properties: {
                    let mut map = HashMap::new();
                    map.insert(
                        "open".to_string(),
                        make_builtin_function(mc, |mc, root, args, span, env| {
                            if args.len() != 1 {
                                return function_argument_error(
                                    mc,
                                    root,
                                    &format!("Expected 1 argument, got {}", args.len()),
                                    &env,
                                    span,
                                );
                            }
                            let path = match &args[0] {
                                Value::String(s) => s.iter().collect::<String>(),
                                _ => {
                                    return type_error(
                                        mc,
                                        root,
                                        "Open argument must be a string",
                                        &env,
                                        span,
                                    );
                                }
                            };
                            let file = FileResource::open(&path).map_err(|e| {
                                io_error::<()>(
                                    mc,
                                    root,
                                    &format!("Failed to open file {}: {}", path, e),
                                    &env,
                                    span,
                                )
                                .unwrap_err()
                            })?;
                            Ok(Value::Resource(Gc::new(mc, RefLock::new(Box::new(file)))))
                        }),
                    );
                    map
                },
                parent: None,
            }),
        )),
    );

    env.define(
        mc,
        "Socket",
        Value::Prototype(Gc::new(
            mc,
            RefLock::new(PrototypeValue {
                properties: {
                    let mut map = HashMap::new();
                    map.insert(
                        "connect".to_string(),
                        make_builtin_function(mc, |mc, root, args, span, env| {
                            if args.len() != 2 {
                                return function_argument_error(
                                    mc,
                                    root,
                                    &format!("Expected 2 arguments, got {}", args.len()),
                                    &env,
                                    span,
                                );
                            }
                            let host = match &args[0] {
                                Value::String(s) => s.iter().collect::<String>(),
                                _ => {
                                    return type_error(
                                        mc,
                                        root,
                                        "Host argument must be a string",
                                        &env,
                                        span,
                                    );
                                }
                            };
                            let port = match &args[1] {
                                Value::Integer(i) => *i,
                                _ => {
                                    return type_error(
                                        mc,
                                        root,
                                        "Port argument must be an integer",
                                        &env,
                                        span,
                                    );
                                }
                            };
                            let port = if let Some(port) = TryInto::<u16>::try_into(port).ok() {
                                port
                            } else {
                                return type_error(
                                    mc,
                                    root,
                                    "Port argument must be a valid u16",
                                    &env,
                                    span,
                                );
                            };
                            let socket = SocketResource::new(host.clone(), port).map_err(|e| {
                                io_error::<()>(
                                    mc,
                                    root,
                                    &format!("Failed to connect to {}:{}: {}", host, port, e),
                                    &env,
                                    span,
                                )
                                .unwrap_err()
                            })?;
                            Ok(Value::Resource(Gc::new(mc, RefLock::new(Box::new(socket)))))
                        }),
                    );
                    map
                },
                parent: None,
            }),
        )),
    );

    env.define(
        mc,
        "typeof",
        make_builtin_function(mc, |mc, root, args, span, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
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

    let root_prototypes = &root.root_prototypes;
    env.define(mc, "Integer", Value::Prototype(root_prototypes.integer));
    env.define(mc, "String", Value::Prototype(root_prototypes.string));
    env.define(mc, "Array", Value::Prototype(root_prototypes.array));
    env.define(mc, "Object", Value::Prototype(root_prototypes.object));
    env.define(mc, "Bool", Value::Prototype(root_prototypes.bool));
    env.define(mc, "Number", Value::Prototype(root_prototypes.number));
    env.define(mc, "Null", Value::Prototype(root_prototypes.null));
    env.define(mc, "Function", Value::Prototype(root_prototypes.function));
    env.define(
        mc,
        "BuiltinFunction",
        Value::Prototype(root_prototypes.builtin_function),
    );
    env.define(mc, "Prototype", Value::Prototype(root_prototypes.prototype));
    env.define(mc, "Exception", Value::Prototype(root_prototypes.exception));
    env.define(mc, "Resource", Value::Prototype(root_prototypes.resource));
    env.define(mc, "Buffer", Value::Prototype(root_prototypes.buffer));

    if no_std {
        return;
    }

    let std_ref = root.std_prototype.borrow_mut(mc);
    let std_val = if let Some(&v) = std_ref.as_ref() {
        v
    } else {
        let std_val = interpret_string(
            mc,
            root,
            STD,
            &Location::new(0..STD.len(), "std".to_string()),
            "std".to_string(),
            Some("std"),
            env,
            true,
        )
        .expect("Failed to interpret standard library");

        let Value::Object(std) = std_val else {
            panic!(
                "Failed to interpret standard library: not an object: {:?}",
                std_val
            );
        };
        std
    };

    for (key, value) in std_val.borrow().properties.iter() {
        if !env.define(mc, key, value.clone()) {
            let prev = env.get(key).unwrap();
            let Value::Prototype(real_p) = prev else {
                panic!("Global {} in std is not a prototype, cannot merge", key);
            };
            let Value::Prototype(new_p) = value else {
                panic!("Global {} in std is not a prototype, cannot merge", key);
            };
            let mut real_p_ref = real_p.borrow_mut(mc);
            let new_p = new_p.borrow();
            for (k, v) in new_p.properties.iter() {
                if let Value::Function(f) = v {
                    let mut f = f.borrow_mut(mc);
                    if let FunctionKind::Constructor(_) = f.kind {
                        f.kind = FunctionKind::Constructor(real_p);
                    }
                };
                real_p_ref.properties.insert(k.clone(), v.clone());
            }
        }
    }
}

pub fn root_prototypes<'a>(mc: &Mutation<'a>) -> RootPrototypes<'a> {
    let object_proto = object_prototype(mc);
    RootPrototypes {
        integer: integer_prototype(mc),
        string: string_prototype(mc),
        array: array_prototype(mc),
        object: object_proto,
        bool: Gc::new(
            mc,
            RefLock::new(PrototypeValue {
                properties: HashMap::new(),
                parent: None,
            }),
        ),
        number: Gc::new(
            mc,
            RefLock::new(PrototypeValue {
                properties: HashMap::new(),
                parent: None,
            }),
        ),
        null: Gc::new(
            mc,
            RefLock::new(PrototypeValue {
                properties: HashMap::new(),
                parent: None,
            }),
        ),
        function: Gc::new(
            mc,
            RefLock::new(PrototypeValue {
                properties: HashMap::new(),
                parent: None,
            }),
        ),
        builtin_function: Gc::new(
            mc,
            RefLock::new(PrototypeValue {
                properties: HashMap::new(),
                parent: None,
            }),
        ),
        prototype: prototype_prototype(mc),
        exception: exception_prototype(mc, object_proto),
        resource: resource_prototype(mc),
        buffer: buffer_prototype(mc),
    }
}

fn buffer_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, PrototypeValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Buffer(buf) => Ok(Value::Integer(buf.borrow().len() as i64)),
                _ => {
                    return type_error(mc, root, "length can only be called on Buffer", &env, expr);
                }
            }
        }),
    );
    map.insert(
        "to_string".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
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
                _ => {
                    return type_error(
                        mc,
                        root,
                        "to_string can only be called on Buffer",
                        &env,
                        expr,
                    );
                }
            }
        }),
    );
    map.insert(
        "from_string".to_string(),
        make_builtin_function(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::String(s) => {
                    let bytes = s.iter().map(|c| *c as u8).collect::<Vec<u8>>();
                    Ok(Value::Buffer(Gc::new(mc, RefLock::new(bytes))))
                }
                _ => {
                    return type_error(
                        mc,
                        root,
                        "from_string can only be called on String",
                        &env,
                        expr,
                    );
                }
            }
        }),
    );
    map.insert(
        "with_size".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Integer(size) if *size >= 0 => {
                    let buf = vec![0u8; *size as usize];
                    Ok(Value::Buffer(Gc::new(mc, RefLock::new(buf))))
                }
                _ => {
                    return type_error(
                        mc,
                        root,
                        "with_size argument must be a non-negative integer",
                        &env,
                        expr,
                    );
                }
            }
        }),
    );
    Gc::new(
        mc,
        RefLock::new(PrototypeValue {
            properties: map,
            parent: None,
        }),
    )
}

fn resource_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, PrototypeValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "close".to_string(),
        make_builtin_method(mc, |mc, root, args, span, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    span,
                );
            }
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(mc);
                    res_ref.close().map_err(|e| {
                        io_error::<()>(
                            mc,
                            root,
                            &format!("Failed to close resource: {}", e),
                            &env,
                            span,
                        )
                        .unwrap_err()
                    })?;
                    Ok(Value::Null)
                }
                _ => return type_error(mc, root, "Close argument must be a resource", &env, span),
            }
        }),
    );
    map.insert(
        "read".to_string(),
        make_builtin_method(mc, |mc, root, args, span, env| {
            if args.len() != 4 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 4 arguments, got {}", args.len()),
                    &env,
                    span,
                );
            }
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(mc);
                    let read_amount = match &args[2] {
                        Value::Integer(i) if *i > 0 => *i as usize,
                        _ => {
                            return type_error(
                                mc,
                                root,
                                "Read size argument must be a positive integer",
                                &env,
                                span,
                            );
                        }
                    };
                    let read_offset = match &args[3] {
                        Value::Integer(i) if *i >= 0 => *i as usize,
                        _ => {
                            return type_error(
                                mc,
                                root,
                                "Read offset argument must be a non-negative integer",
                                &env,
                                span,
                            );
                        }
                    };
                    let mut buf = match &args[1] {
                        Value::Buffer(b) => {
                            &mut b.borrow_mut(mc)[read_offset..read_offset + read_amount]
                        }
                        _ => {
                            return type_error(
                                mc,
                                root,
                                "Read buffer argument must be a buffer",
                                &env,
                                span,
                            );
                        }
                    };
                    let bytes_read = res_ref.read(&mut buf).map_err(|e| {
                        io_error::<()>(
                            mc,
                            root,
                            &format!("Failed to read from resource: {}", e),
                            &env,
                            span,
                        )
                        .unwrap_err()
                    })?;
                    Ok(Value::Integer(bytes_read as i64))
                }
                _ => return type_error(mc, root, "Read argument must be a resource", &env, span),
            }
        }),
    );
    map.insert(
        "write".to_string(),
        make_builtin_method(mc, |mc, root, args, span, env| {
            if args.len() != 4 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 4 arguments, got {}", args.len()),
                    &env,
                    span,
                );
            }
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(mc);
                    let write_amount = match &args[2] {
                        Value::Integer(i) if *i > 0 => *i as usize,
                        _ => {
                            return type_error(
                                mc,
                                root,
                                "Write size argument must be a positive integer",
                                &env,
                                span,
                            );
                        }
                    };
                    let write_offset = match &args[3] {
                        Value::Integer(i) if *i >= 0 => *i as usize,
                        _ => {
                            return type_error(
                                mc,
                                root,
                                "Write offset argument must be a non-negative integer",
                                &env,
                                span,
                            );
                        }
                    };
                    let buf = match &args[1] {
                        Value::Buffer(b) => &b.borrow()[write_offset..write_offset + write_amount],
                        _ => {
                            return type_error(
                                mc,
                                root,
                                "Write buffer argument must be a buffer",
                                &env,
                                span,
                            );
                        }
                    };

                    let bytes_written = res_ref.write(&buf).map_err(|e| {
                        io_error::<()>(
                            mc,
                            root,
                            &format!("Failed to write to resource: {}", e),
                            &env,
                            span,
                        )
                        .unwrap_err()
                    })?;
                    Ok(Value::Integer(bytes_written as i64))
                }
                _ => return type_error(mc, root, "Write argument must be a resource", &env, span),
            }
        }),
    );
    Gc::new(
        mc,
        RefLock::new(PrototypeValue {
            properties: map,
            parent: None,
        }),
    )
}

fn exception_prototype<'a>(
    mc: &Mutation<'a>,
    parent: GcRefLock<'a, PrototypeValue<'a>>,
) -> GcRefLock<'a, PrototypeValue<'a>> {
    let map = HashMap::new();
    Gc::new(
        mc,
        RefLock::new(PrototypeValue {
            properties: map,
            parent: Some(parent),
        }),
    )
}

fn object_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, PrototypeValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "prototype".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Object(obj) => {
                    let proto = obj.borrow().prototype;
                    Ok(Value::Prototype(proto))
                }
                _ => {
                    return type_error(
                        mc,
                        root,
                        "prototype can only be called on Object",
                        &env,
                        expr,
                    );
                }
            }
        }),
    );
    Gc::new(
        mc,
        RefLock::new(PrototypeValue {
            properties: map,
            parent: None,
        }),
    )
}

fn prototype_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, PrototypeValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "super".to_string(),
        make_builtin_function(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Prototype(obj) => {
                    let proto = obj.borrow().parent;
                    Ok(proto.map_or(Value::Null, Value::Prototype))
                }
                _ => {
                    return type_error(
                        mc,
                        root,
                        "super can only be called on Prototype",
                        &env,
                        expr,
                    );
                }
            }
        }),
    );
    Gc::new(
        mc,
        RefLock::new(PrototypeValue {
            properties: map,
            parent: None,
        }),
    )
}

fn integer_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, PrototypeValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "to_string".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Integer(i) => Ok(Value::String(i.to_string().chars().collect())),
                _ => {
                    return type_error(
                        mc,
                        root,
                        "to_string can only be called on Integer",
                        &env,
                        expr,
                    );
                }
            }
        }),
    );
    Gc::new(
        mc,
        RefLock::new(PrototypeValue {
            properties: map,
            parent: None,
        }),
    )
}

fn string_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, PrototypeValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::String(arr) => Ok(Value::Integer(arr.len() as i64)),
                _ => {
                    return type_error(mc, root, "length can only be called on String", &env, expr);
                }
            }
        }),
    );
    map.insert(
        "concat".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() < 2 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected at least 2 arguments, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            let strings = args
                .iter()
                .map(|arg| match arg {
                    Value::String(s) => Ok(s.iter().collect::<String>()),
                    _ => {
                        return type_error(
                            mc,
                            root,
                            "concat arguments must be strings",
                            &env,
                            expr,
                        );
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::String(strings.concat().chars().collect()))
        }),
    );
    Gc::new(
        mc,
        RefLock::new(PrototypeValue {
            properties: map,
            parent: None,
        }),
    )
}

fn array_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, PrototypeValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() != 1 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected 1 argument, got {}", args.len()),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Array(arr) => Ok(Value::Integer(arr.borrow().len() as i64)),
                _ => return type_error(mc, root, "length can only be called on Array", &env, expr),
            }
        }),
    );
    map.insert(
        "push".to_string(),
        make_builtin_method(mc, |mc, root, args, expr, env| {
            if args.len() < 2 {
                return function_argument_error(
                    mc,
                    root,
                    &format!("Expected at least 1 argument, got {}", args.len() - 1),
                    &env,
                    expr,
                );
            }
            match &args[0] {
                Value::Array(arr) => {
                    let mut arr = arr.borrow_mut(mc);
                    for arg in args.iter().skip(1) {
                        arr.push(arg.clone());
                    }
                    Ok(Value::Null)
                }
                _ => return type_error(mc, root, "push can only be called on Array", &env, expr),
            }
        }),
    );
    Gc::new(
        mc,
        RefLock::new(PrototypeValue {
            properties: map,
            parent: None,
        }),
    )
}

pub const STD: &str = include_str!("../../reference/std.yuzu");
