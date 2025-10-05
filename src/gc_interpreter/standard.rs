use std::collections::HashMap;

use gc_arena::{
    Gc, Mutation, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    gc_interpreter::{
        MyRoot, ValueClasses,
        exception::{function_argument_error, io_error, type_error},
        resource::{FileResource, SocketResource},
        value::{
            ClassInstanceValue, ClassValue, Environment, FunctionValue, LocatedError,
            StringVariant, TypedBufferType, Value, variable_to_string,
        },
    },
    location::Location,
};

fn expect_arg_len<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    args: &Vec<Value<'a>>,
    expected: usize,
    env: &Environment<'a>,
    expr: &Location,
) -> Result<(), LocatedError<'a>> {
    if args.len() != expected {
        function_argument_error(
            mc,
            root,
            &format!("Expected {} arguments, got {}", expected, args.len()),
            env,
            expr,
        )
    } else {
        Ok(())
    }
}

fn expect_class_arg<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    value: &Value<'a>,
    env: &Environment<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, ClassValue<'a>>, LocatedError<'a>> {
    if let Value::Class(c) = value {
        Ok(c.clone())
    } else {
        type_error(
            mc,
            root,
            &format!("Expected argument to be a Class, got {}", value.get_type()),
            env,
            expr,
        )
    }
}

fn expect_class_instance_arg<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    value: &Value<'a>,
    env: &Environment<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, ClassInstanceValue<'a>>, LocatedError<'a>> {
    if let Value::ClassInstance(c) = value {
        Ok(c.clone())
    } else {
        type_error(
            mc,
            root,
            &format!(
                "Expected argument to be a ClassInstance, got {}",
                value.get_type()
            ),
            env,
            expr,
        )
    }
}

fn exprect_buffer_arg<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    value: &Value<'a>,
    env: &Environment<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, Vec<u8>>, LocatedError<'a>> {
    if let Value::Buffer(b) = value {
        Ok(b.clone())
    } else {
        type_error(
            mc,
            root,
            &format!("Expected argument to be a Buffer, got {}", value.get_type()),
            env,
            expr,
        )
    }
}

fn make_builtin_function<'a>(
    mc: &Mutation<'a>,
    func: impl for<'b> Fn(
        &Mutation<'b>,
        &MyRoot<'b>,
        Vec<Value<'b>>,
        &Location,
        Gc<'b, Environment<'b>>,
    ) -> Result<Value<'b>, LocatedError<'b>>
    + 'static,
) -> GcRefLock<'a, FunctionValue<'a>> {
    Gc::new(
        mc,
        RefLock::new(FunctionValue::Builtin {
            func: StaticCollect(Box::new(func)),
        }),
    )
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
        Value::Function(make_builtin_function(mc, |_, _, args, _, _| {
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
        mc,
        "File",
        Value::Class(Gc::new(
            mc,
            RefLock::new(ClassValue {
                instance_fields: HashMap::new(),
                constructor: None,
                static_fields: HashMap::new(),
                methods: HashMap::new(),
                static_methods: {
                    let mut map = HashMap::new();
                    map.insert(
                        "open".to_string(),
                        make_builtin_function(mc, |mc, root, args, span, env| {
                            expect_arg_len(mc, root, &args, 1, &env, span)?;
                            let path = match &args[0] {
                                Value::String(s) => s.to_string(),
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
        Value::Class(Gc::new(
            mc,
            RefLock::new(ClassValue {
                instance_fields: HashMap::new(),
                constructor: None,
                static_fields: HashMap::new(),
                methods: HashMap::new(),
                static_methods: {
                    let mut map = HashMap::new();
                    map.insert(
                        "connect".to_string(),
                        make_builtin_function(mc, |mc, root, args, span, env| {
                            expect_arg_len(mc, root, &args, 2, &env, span)?;
                            let host = match &args[0] {
                                Value::String(s) => s.to_string(),
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
        Value::Function(make_builtin_function(mc, |mc, root, args, span, env| {
            expect_arg_len(mc, root, &args, 1, &env, span)?;
            let type_str = args[0].get_type();
            Ok(Value::String(Gc::new(
                mc,
                StringVariant::from_string(type_str),
            )))
        })),
    );

    let root_prototypes = &root.value_classes;
    env.define(mc, "Integer", Value::Class(root_prototypes.integer));
    env.define(mc, "String", Value::Class(root_prototypes.string));
    env.define(mc, "Array", Value::Class(root_prototypes.array));
    env.define(mc, "Object", Value::Class(root_prototypes.object));
    env.define(mc, "Bool", Value::Class(root_prototypes.bool));
    env.define(mc, "Number", Value::Class(root_prototypes.number));
    env.define(mc, "Null", Value::Class(root_prototypes.null));
    env.define(mc, "Function", Value::Class(root_prototypes.function));
    env.define(
        mc,
        "ClassInstance",
        Value::Class(root_prototypes.class_instance),
    );
    env.define(mc, "Prototype", Value::Class(root_prototypes.class));
    env.define(mc, "Exception", Value::Class(root_prototypes.exception));
    env.define(mc, "Resource", Value::Class(root_prototypes.resource));
    env.define(mc, "Buffer", Value::Class(root_prototypes.buffer));
    env.define(mc, "TypedSlice", Value::Class(root_prototypes.typed_slice));

    let typed_slice_variants = [
        TypedBufferType::Uint8,
        TypedBufferType::Int8,
        TypedBufferType::Uint16,
        TypedBufferType::Int16,
        TypedBufferType::Uint32,
        TypedBufferType::Int32,
        TypedBufferType::Uint64,
        TypedBufferType::Int64,
        TypedBufferType::Float32,
        TypedBufferType::Float64,
    ];

    for buffer_type in typed_slice_variants {
        let name = match buffer_type {
            TypedBufferType::Uint8 => "Uint8Array",
            TypedBufferType::Int8 => "Int8Array",
            TypedBufferType::Uint16 => "Uint16Array",
            TypedBufferType::Int16 => "Int16Array",
            TypedBufferType::Uint32 => "Uint32Array",
            TypedBufferType::Int32 => "Int32Array",
            TypedBufferType::Uint64 => "Uint64Array",
            TypedBufferType::Int64 => "Int64Array",
            TypedBufferType::Float32 => "Float32Array",
            TypedBufferType::Float64 => "Float64Array",
        };
        let mut static_methods = HashMap::new();
        static_methods.insert(
            "of".to_string(),
            make_builtin_function(mc, move |mc, root, args, span, env| {
                expect_arg_len(mc, root, &args, 1, &env, span)?;
                let buf = exprect_buffer_arg(mc, root, &args[0], &env, span)?;
                let length = buf.borrow().len() / buffer_type.byte_size();
                Ok(Value::TypedSlice {
                    buffer: buf.clone(),
                    start: 0,
                    length,
                    buffer_type: buffer_type.clone(),
                })
            }),
        );
        env.define(
            mc,
            name,
            Value::Class(Gc::new(
                mc,
                RefLock::new(ClassValue {
                    instance_fields: HashMap::new(),
                    constructor: None,
                    static_fields: HashMap::new(),
                    methods: HashMap::new(),
                    static_methods,
                    parent: Some(root_prototypes.typed_slice),
                }),
            )),
        );
    }

    if no_std {
        return;
    }

    let std_ref = root.std_export.borrow_mut(mc);
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

    for (key, value) in std_val.borrow().iter() {
        if !env.define(mc, key, value.clone()) {
            let prev = env.get(key).unwrap();
            let Value::Class(real_p) = prev else {
                panic!("Global {} in std is not a prototype, cannot merge", key);
            };
            let Value::Class(new_p) = value else {
                panic!("Global {} in std is not a prototype, cannot merge", key);
            };
            let mut real_p_ref = real_p.borrow_mut(mc);
            let new_p = new_p.borrow();
            for (k, v) in new_p.static_fields.iter() {
                real_p_ref.static_fields.insert(k.clone(), v.clone());
            }
            for (k, v) in new_p.instance_fields.iter() {
                real_p_ref.instance_fields.insert(k.clone(), v.clone());
            }
            for (k, v) in new_p.static_methods.iter() {
                real_p_ref.static_methods.insert(k.clone(), v.clone());
            }
            for (k, v) in new_p.methods.iter() {
                real_p_ref.methods.insert(k.clone(), v.clone());
            }
            real_p_ref.constructor = new_p.constructor.clone();
            if let Some(parent) = new_p.parent
                && !Gc::ptr_eq(parent, real_p)
            {
                real_p_ref.parent = Some(parent.clone());
            }
        }
    }
}

pub fn root_prototypes<'a>(mc: &Mutation<'a>) -> ValueClasses<'a> {
    let empty_class = || {
        Gc::new(
            mc,
            RefLock::new(ClassValue {
                instance_fields: HashMap::new(),
                constructor: None,
                static_fields: HashMap::new(),
                methods: HashMap::new(),
                static_methods: HashMap::new(),
                parent: None,
            }),
        )
    };
    ValueClasses {
        integer: integer_prototype(mc),
        string: string_prototype(mc),
        array: array_prototype(mc),
        object: object_prototype(mc),
        bool: empty_class(),
        number: empty_class(),
        null: empty_class(),
        function: empty_class(),
        class_instance: class_instance_prototype(mc),
        class: class_prototype(mc),
        exception: exception_prototype(mc),
        resource: resource_prototype(mc),
        buffer: buffer_prototype(mc),
        typed_slice: typed_slice_prototype(mc),
    }
}

fn typed_slice_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut methods = HashMap::new();
    methods.insert(
        "length".to_string(),
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
                Value::TypedSlice { length, .. } => Ok(Value::Integer((*length) as i64)),
                _ => {
                    return type_error(
                        mc,
                        root,
                        "length can only be called on TypedSlice",
                        &env,
                        expr,
                    );
                }
            }
        }),
    );
    Gc::new(
        mc,
        RefLock::new(ClassValue {
            methods,
            instance_fields: HashMap::new(),
            constructor: None,
            static_fields: HashMap::new(),
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

fn class_instance_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut methods = HashMap::new();
    methods.insert(
        "get_class".to_string(),
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
            let obj = expect_class_instance_arg(mc, root, &args[0], &env, expr)?;
            Ok(Value::Class(obj.borrow().class))
        }),
    );
    Gc::new(
        mc,
        RefLock::new(ClassValue {
            static_fields: HashMap::new(),
            instance_fields: HashMap::new(),
            constructor: None,
            methods,
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

fn buffer_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut methods = HashMap::new();
    let mut static_methods = HashMap::new();
    methods.insert(
        "length".to_string(),
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
                Value::Buffer(buf) => Ok(Value::Integer(buf.borrow().len() as i64)),
                _ => {
                    return type_error(mc, root, "length can only be called on Buffer", &env, expr);
                }
            }
        }),
    );
    methods.insert(
        "to_string".to_string(),
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
                Value::Buffer(buf) => {
                    let buf_ref = buf.borrow();
                    let s = String::from_utf8_lossy(&buf_ref);
                    Ok(Value::String(Gc::new(mc, StringVariant::from_string(&s))))
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
    static_methods.insert(
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
                    let bytes = s.to_string().into_bytes();
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
    static_methods.insert(
        "with_size".to_string(),
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
        RefLock::new(ClassValue {
            methods,
            instance_fields: HashMap::new(),
            constructor: None,
            static_fields: HashMap::new(),
            static_methods,
            parent: None,
        }),
    )
}

fn resource_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "close".to_string(),
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
        make_builtin_function(mc, |mc, root, args, span, env| {
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
        make_builtin_function(mc, |mc, root, args, span, env| {
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
        RefLock::new(ClassValue {
            static_fields: HashMap::new(),
            instance_fields: HashMap::new(),
            constructor: None,
            methods: map,
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

fn exception_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let map = HashMap::new();
    Gc::new(
        mc,
        RefLock::new(ClassValue {
            static_fields: map,
            instance_fields: HashMap::new(),
            constructor: None,
            methods: HashMap::new(),
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

fn object_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let map = HashMap::new();
    Gc::new(
        mc,
        RefLock::new(ClassValue {
            static_fields: map,
            instance_fields: HashMap::new(),
            constructor: None,
            methods: HashMap::new(),
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

fn class_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
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
            let class = expect_class_arg(mc, root, &args[0], &env, expr)?;
            let Some(parent) = class.borrow().parent else {
                return type_error(mc, root, "Class has no parent", &env, expr);
            };
            let Some(ctor) = parent.borrow().constructor else {
                return type_error(mc, root, "Parent class has no constructor", &env, expr);
            };
            Ok(Value::Function(ctor))
        }),
    );
    Gc::new(
        mc,
        RefLock::new(ClassValue {
            methods: map,
            instance_fields: HashMap::new(),
            constructor: None,
            static_fields: HashMap::new(),
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

fn integer_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "to_string".to_string(),
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
                Value::Integer(i) => Ok(Value::String(Gc::new(
                    mc,
                    StringVariant::from_string(&i.to_string()),
                ))),
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
        RefLock::new(ClassValue {
            methods: map,
            instance_fields: HashMap::new(),
            constructor: None,
            static_fields: HashMap::new(),
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

fn string_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
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
                Value::String(arr) => Ok(Value::Integer(arr.len() as i64)),
                _ => {
                    return type_error(mc, root, "length can only be called on String", &env, expr);
                }
            }
        }),
    );
    map.insert(
        "concat".to_string(),
        make_builtin_function(mc, |mc, root, args, expr, env| {
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
                    Value::String(s) => Ok(s.to_string()),
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
            Ok(Value::String(Gc::new(
                mc,
                StringVariant::from_string(&strings.join("")),
            )))
        }),
    );
    Gc::new(
        mc,
        RefLock::new(ClassValue {
            methods: map,
            instance_fields: HashMap::new(),
            constructor: None,
            static_fields: HashMap::new(),
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

fn array_prototype<'a>(mc: &Mutation<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
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
                Value::Array(arr) => Ok(Value::Integer(arr.borrow().len() as i64)),
                _ => return type_error(mc, root, "length can only be called on Array", &env, expr),
            }
        }),
    );
    map.insert(
        "push".to_string(),
        make_builtin_function(mc, |mc, root, args, expr, env| {
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
        RefLock::new(ClassValue {
            methods: map,
            instance_fields: HashMap::new(),
            constructor: None,
            static_fields: HashMap::new(),
            static_methods: HashMap::new(),
            parent: None,
        }),
    )
}

pub const STD: &str = include_str!("../../reference/std.yuzu");
