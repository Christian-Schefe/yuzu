use std::collections::HashMap;

use gc_arena::{
    Gc, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    gc_interpreter::{
        Context,
        exception::{function_argument_error, io_error, type_error},
        resource::SocketResource,
        standard::{
            fs::define_fs_globals, rand::define_rand_globals, system::define_system_globals,
        },
        value::{
            ClassInstanceValue, ClassValue, Environment, FunctionValue, IntVariant, LocatedError,
            StringVariant, TypedBufferType, Value, variable_to_string,
        },
    },
    location::Location,
};

mod fs;
mod rand;
mod system;

fn expect_arg_len<'a>(
    ctx: &Context<'a>,
    args: &Vec<Value<'a>>,
    expected: usize,
    expr: &Location,
) -> Result<(), LocatedError<'a>> {
    if args.len() != expected {
        function_argument_error(
            ctx,
            &format!("Expected {} arguments, got {}", expected, args.len()),
            expr,
        )
    } else {
        Ok(())
    }
}

fn expect_class_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, ClassValue<'a>>, LocatedError<'a>> {
    if let Value::Class(c) = value {
        Ok(c.clone())
    } else {
        type_error(
            ctx,
            &format!("Expected argument to be a Class, got {}", value.get_type()),
            expr,
        )
    }
}

fn expect_class_instance_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, ClassInstanceValue<'a>>, LocatedError<'a>> {
    if let Value::ClassInstance(c) = value {
        Ok(c.clone())
    } else {
        type_error(
            ctx,
            &format!(
                "Expected argument to be a ClassInstance, got {}",
                value.get_type()
            ),
            expr,
        )
    }
}

fn expect_object_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, HashMap<String, Value<'a>>>, LocatedError<'a>> {
    if let Value::Object(o) = value {
        Ok(o.clone())
    } else {
        type_error(
            ctx,
            &format!(
                "Expected argument to be an Object, got {}",
                value.get_type()
            ),
            expr,
        )
    }
}

fn expect_buffer_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, Vec<u8>>, LocatedError<'a>> {
    if let Value::Buffer(b) = value {
        Ok(b.clone())
    } else {
        type_error(
            ctx,
            &format!("Expected argument to be a Buffer, got {}", value.get_type()),
            expr,
        )
    }
}

fn expect_string_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<Gc<'a, StringVariant<'a>>, LocatedError<'a>> {
    if let Value::String(s) = value {
        Ok(s.clone())
    } else {
        type_error(
            ctx,
            &format!("Expected argument to be a String, got {}", value.get_type()),
            expr,
        )
    }
}

fn expect_integer_arg<'a, 'b>(
    ctx: &Context<'a>,
    value: &'b Value<'a>,
    expr: &Location,
) -> Result<&'b IntVariant, LocatedError<'a>> {
    if let Value::Integer(i) = value {
        Ok(i)
    } else {
        type_error(
            ctx,
            &format!(
                "Expected argument to be an Integer, got {}",
                value.get_type()
            ),
            expr,
        )
    }
}

fn expect_usize_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<usize, LocatedError<'a>> {
    if let Value::Integer(i) = value {
        i.try_to_usize().ok_or_else(|| {
            type_error::<usize>(ctx, "Integer too large to fit in usize", expr).unwrap_err()
        })
    } else {
        type_error(
            ctx,
            &format!(
                "Expected argument to be an Integer, got {}",
                value.get_type()
            ),
            expr,
        )
    }
}

fn make_builtin_function<'a>(
    ctx: &Context<'a>,
    func: impl for<'b> Fn(
        &Context<'b>,
        Vec<Value<'b>>,
        &Location,
        Gc<'b, Environment<'b>>,
    ) -> Result<Value<'b>, LocatedError<'b>>
    + 'static,
) -> GcRefLock<'a, FunctionValue<'a>> {
    ctx.gc_lock(FunctionValue::Builtin {
        func: StaticCollect(Box::new(func)),
    })
}

pub fn define_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define(
        ctx,
        "print",
        Value::Function(make_builtin_function(ctx, |_, args, _, _| {
            let str = args
                .into_iter()
                .map(|arg| variable_to_string(&arg))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", str);
            Ok(Value::Null)
        })),
    );

    define_system_globals(ctx, env);
    define_fs_globals(ctx, env);
    define_rand_globals(ctx, env);

    env.define(
        ctx,
        "Socket",
        Value::Class(ctx.gc_lock(ClassValue {
            constructor: None,
            methods: HashMap::new(),
            static_methods: {
                let mut map = HashMap::new();
                map.insert(
                    "connect".to_string(),
                    make_builtin_function(ctx, |ctx, args, span, _| {
                        expect_arg_len(ctx, &args, 2, span)?;
                        let host = expect_string_arg(ctx, &args[0], span)?.to_string();
                        let port = expect_usize_arg(ctx, &args[1], span)?;
                        let port = if let Some(port) = TryInto::<u16>::try_into(port).ok() {
                            port
                        } else {
                            return type_error(ctx, "Port argument must be a valid u16", span);
                        };
                        let socket = SocketResource::new(host.clone(), port).map_err(|e| {
                            io_error::<()>(
                                ctx,
                                &format!("Failed to connect to {}:{}: {}", host, port, e),
                                span,
                            )
                            .unwrap_err()
                        })?;
                        Ok(Value::Resource(ctx.gc_lock(Box::new(socket))))
                    }),
                );
                map
            },
            parent: None,
        })),
    );

    env.define(
        ctx,
        "typeof",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 1, span)?;
            let type_str = args[0].get_type();
            Ok(Value::String(ctx.gc(StringVariant::from_string(type_str))))
        })),
    );

    let root_prototypes = root_prototypes(ctx);
    for (name, class) in root_prototypes {
        env.define(ctx, &name, Value::Class(class));
    }
    let Some(Value::Class(typed_slice_prototype)) = env.get("TypedSlice") else {
        panic!("TypedSlice prototype not found");
    };

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
            make_builtin_function(ctx, move |ctx, args, span, _| {
                expect_arg_len(ctx, &args, 1, span)?;
                let buf = expect_buffer_arg(ctx, &args[0], span)?;
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
            ctx,
            name,
            Value::Class(ctx.gc_lock(ClassValue {
                constructor: None,
                methods: HashMap::new(),
                static_methods,
                parent: Some(typed_slice_prototype),
            })),
        );
    }
}

pub fn root_prototypes<'a>(ctx: &Context<'a>) -> Vec<(String, GcRefLock<'a, ClassValue<'a>>)> {
    let empty_class = || {
        ctx.gc_lock(ClassValue {
            constructor: None,
            methods: HashMap::new(),
            static_methods: HashMap::new(),
            parent: None,
        })
    };
    vec![
        ("Integer".to_string(), integer_prototype(ctx)),
        ("String".to_string(), string_prototype(ctx)),
        ("Array".to_string(), array_prototype(ctx)),
        ("Object".to_string(), object_prototype(ctx)),
        ("Bool".to_string(), empty_class()),
        ("Number".to_string(), empty_class()),
        ("Null".to_string(), empty_class()),
        ("Function".to_string(), empty_class()),
        ("Class".to_string(), class_prototype(ctx)),
        ("ClassInstance".to_string(), class_instance_prototype(ctx)),
        ("Exception".to_string(), exception_prototype(ctx)),
        ("Resource".to_string(), resource_prototype(ctx)),
        ("Buffer".to_string(), buffer_prototype(ctx)),
        ("TypedSlice".to_string(), typed_slice_prototype(ctx)),
    ]
}

fn typed_slice_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut methods = HashMap::new();
    methods.insert(
        "length".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            match &args[0] {
                Value::TypedSlice { length, .. } => {
                    Ok(Value::Integer(IntVariant::from_u64(*length as u64)))
                }
                _ => {
                    return type_error(ctx, "length can only be called on TypedSlice", expr);
                }
            }
        }),
    );
    ctx.gc_lock(ClassValue {
        methods,
        constructor: None,
        static_methods: HashMap::new(),
        parent: None,
    })
}

fn class_instance_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut methods = HashMap::new();
    methods.insert(
        "get_class".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            let obj = expect_class_instance_arg(ctx, &args[0], expr)?;
            Ok(Value::Class(obj.borrow().class))
        }),
    );
    ctx.gc_lock(ClassValue {
        constructor: None,
        methods,
        static_methods: HashMap::new(),
        parent: None,
    })
}

fn buffer_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut methods = HashMap::new();
    let mut static_methods = HashMap::new();
    methods.insert(
        "length".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            match &args[0] {
                Value::Buffer(buf) => Ok(Value::Integer(IntVariant::from_u64(
                    buf.borrow().len() as u64
                ))),
                _ => {
                    return type_error(ctx, "length can only be called on Buffer", expr);
                }
            }
        }),
    );
    methods.insert(
        "to_string".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            match &args[0] {
                Value::Buffer(buf) => {
                    let buf_ref = buf.borrow();
                    let s = String::from_utf8_lossy(&buf_ref);
                    Ok(Value::String(ctx.gc(StringVariant::from_string(&s))))
                }
                _ => {
                    return type_error(ctx, "to_string can only be called on Buffer", expr);
                }
            }
        }),
    );
    static_methods.insert(
        "from_string".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            match &args[0] {
                Value::String(s) => {
                    let bytes = s.to_string().into_bytes();
                    Ok(Value::Buffer(ctx.gc(RefLock::new(bytes))))
                }
                _ => {
                    return type_error(ctx, "from_string can only be called on String", expr);
                }
            }
        }),
    );
    static_methods.insert(
        "with_size".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 1, expr)?;
            let size = expect_usize_arg(ctx, &args[0], expr)?;
            let buf = vec![0u8; size];
            Ok(Value::Buffer(ctx.gc(RefLock::new(buf))))
        }),
    );
    ctx.gc_lock(ClassValue {
        methods,
        constructor: None,
        static_methods,
        parent: None,
    })
}

fn resource_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "close".to_string(),
        make_builtin_function(ctx, |ctx, args, span, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    span,
                );
            }
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(ctx.mc);
                    res_ref.close().map_err(|e| {
                        io_error::<()>(ctx, &format!("Failed to close resource: {}", e), span)
                            .unwrap_err()
                    })?;
                    Ok(Value::Null)
                }
                _ => return type_error(ctx, "Close argument must be a resource", span),
            }
        }),
    );
    map.insert(
        "read".to_string(),
        make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 4, span)?;
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(ctx.mc);
                    let read_amount = expect_usize_arg(ctx, &args[2], span)?;
                    let read_offset = expect_usize_arg(ctx, &args[3], span)?;
                    let mut buf = match &args[1] {
                        Value::Buffer(b) => {
                            &mut b.borrow_mut(ctx.mc)[read_offset..read_offset + read_amount]
                        }
                        _ => {
                            return type_error(ctx, "Read buffer argument must be a buffer", span);
                        }
                    };
                    let bytes_read = res_ref.read(&mut buf).map_err(|e| {
                        io_error::<()>(ctx, &format!("Failed to read from resource: {}", e), span)
                            .unwrap_err()
                    })?;
                    Ok(Value::Integer(IntVariant::from_u64(bytes_read as u64)))
                }
                _ => return type_error(ctx, "Read argument must be a resource", span),
            }
        }),
    );
    map.insert(
        "write".to_string(),
        make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 4, span)?;
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(ctx.mc);
                    let write_amount = expect_usize_arg(ctx, &args[2], span)?;
                    let write_offset = expect_usize_arg(ctx, &args[3], span)?;
                    let buf = match &args[1] {
                        Value::Buffer(b) => &b.borrow()[write_offset..write_offset + write_amount],
                        _ => {
                            return type_error(ctx, "Write buffer argument must be a buffer", span);
                        }
                    };

                    let bytes_written = res_ref.write(&buf).map_err(|e| {
                        io_error::<()>(ctx, &format!("Failed to write to resource: {}", e), span)
                            .unwrap_err()
                    })?;
                    Ok(Value::Integer(IntVariant::from_u64(bytes_written as u64)))
                }
                _ => return type_error(ctx, "Write argument must be a resource", span),
            }
        }),
    );
    ctx.gc_lock(ClassValue {
        constructor: None,
        methods: map,
        static_methods: HashMap::new(),
        parent: None,
    })
}

fn exception_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    ctx.gc_lock(ClassValue {
        constructor: None,
        methods: HashMap::new(),
        static_methods: HashMap::new(),
        parent: None,
    })
}

fn object_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "keys".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 1, expr)?;
            let obj = expect_object_arg(ctx, &args[0], expr)?;
            let keys = obj
                .borrow()
                .keys()
                .cloned()
                .map(|k| Value::String(ctx.gc(StringVariant::from_string(&k))))
                .collect::<Vec<_>>();
            Ok(Value::Array(ctx.gc(RefLock::new(keys))))
        }),
    );
    ctx.gc_lock(ClassValue {
        constructor: None,
        methods: HashMap::new(),
        static_methods: map,
        parent: None,
    })
}

fn class_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "super".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            let class = expect_class_arg(ctx, &args[0], expr)?;
            let Some(parent) = class.borrow().parent else {
                return type_error(ctx, "Class has no parent", expr);
            };
            let Some(ctor) = parent.borrow().constructor else {
                return type_error(ctx, "Parent class has no constructor", expr);
            };
            Ok(Value::Function(ctor))
        }),
    );
    ctx.gc_lock(ClassValue {
        methods: map,
        constructor: None,
        static_methods: HashMap::new(),
        parent: None,
    })
}

fn integer_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "to_string".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            match &args[0] {
                Value::Integer(i) => Ok(Value::String(
                    ctx.gc(StringVariant::from_string(&i.to_string())),
                )),
                _ => {
                    return type_error(ctx, "to_string can only be called on Integer", expr);
                }
            }
        }),
    );
    ctx.gc_lock(ClassValue {
        methods: map,
        constructor: None,
        static_methods: HashMap::new(),
        parent: None,
    })
}

fn string_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 1, expr)?;
            let string = expect_string_arg(ctx, &args[0], expr)?;
            Ok(Value::Integer(IntVariant::from_u64(string.len() as u64)))
        }),
    );
    map.insert(
        "concat".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() < 2 {
                return function_argument_error(
                    ctx,
                    &format!("Expected at least 2 arguments, got {}", args.len()),
                    expr,
                );
            }
            let strings = args
                .iter()
                .map(|arg| match arg {
                    Value::String(s) => Ok(s.to_string()),
                    _ => {
                        return type_error(ctx, "concat arguments must be strings", expr);
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::String(
                ctx.gc(StringVariant::from_string(&strings.join(""))),
            ))
        }),
    );
    map.insert(
        "slice".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 3, expr)?;
            let s = expect_string_arg(ctx, &args[0], expr)?;
            let start = expect_usize_arg(ctx, &args[1], expr)?;
            let length = expect_usize_arg(ctx, &args[2], expr)?;
            if start + length > s.len() {
                return type_error(ctx, "Slice out of bounds", expr);
            }
            let Some(result) = StringVariant::slice(ctx, s, start, length) else {
                return type_error(ctx, "Slice out of bounds", expr);
            };
            Ok(Value::String(result))
        }),
    );
    ctx.gc_lock(ClassValue {
        methods: map,
        constructor: None,
        static_methods: HashMap::new(),
        parent: None,
    })
}

fn array_prototype<'a>(ctx: &Context<'a>) -> GcRefLock<'a, ClassValue<'a>> {
    let mut map = HashMap::new();
    map.insert(
        "length".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 1, expr)?;
            match &args[0] {
                Value::Array(arr) => Ok(Value::Integer(IntVariant::from_u64(
                    arr.borrow().len() as u64
                ))),
                _ => return type_error(ctx, "length can only be called on Array", expr),
            }
        }),
    );
    map.insert(
        "push".to_string(),
        make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() < 2 {
                return function_argument_error(
                    ctx,
                    &format!("Expected at least 1 argument, got {}", args.len() - 1),
                    expr,
                );
            }
            match &args[0] {
                Value::Array(arr) => {
                    let mut arr = arr.borrow_mut(ctx.mc);
                    for arg in args.iter().skip(1) {
                        arr.push(arg.clone());
                    }
                    Ok(Value::Null)
                }
                _ => return type_error(ctx, "push can only be called on Array", expr),
            }
        }),
    );
    ctx.gc_lock(ClassValue {
        methods: map,
        constructor: None,
        static_methods: HashMap::new(),
        parent: None,
    })
}
