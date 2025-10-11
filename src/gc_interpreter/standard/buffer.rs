use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    exception::{function_argument_error, type_error},
    standard::{expect_arg_len, expect_buffer_arg, expect_usize_arg, make_builtin_function},
    value::{Environment, IntVariant, StringVariant, TypedBufferType, Value},
};

pub fn define_typed_buffer_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
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
            TypedBufferType::Uint8 => "uint8_array",
            TypedBufferType::Int8 => "int8_array",
            TypedBufferType::Uint16 => "uint16_array",
            TypedBufferType::Int16 => "int16_array",
            TypedBufferType::Uint32 => "uint32_array",
            TypedBufferType::Int32 => "int32_array",
            TypedBufferType::Uint64 => "uint64_array",
            TypedBufferType::Int64 => "int64_array",
            TypedBufferType::Float32 => "float32_array",
            TypedBufferType::Float64 => "float64_array",
        };
        env.define_const(
            ctx,
            &format!("{}_of", name),
            Value::Function(make_builtin_function(ctx, move |ctx, args, span, _| {
                expect_arg_len(ctx, &args, 1, span)?;
                let buf = expect_buffer_arg(ctx, &args[0], span)?;
                let length = buf.borrow().len() / buffer_type.byte_size();
                Ok(Value::TypedSlice {
                    buffer: buf.clone(),
                    start: 0,
                    length,
                    buffer_type: buffer_type.clone(),
                })
            })),
        );
    }

    env.define_const(
        ctx,
        "typed_buffer_length",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
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
        })),
    );

    env.define_const(
        ctx,
        "buffer_length",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            let buf = expect_buffer_arg(ctx, &args[0], expr)?;
            Ok(Value::Integer(IntVariant::from_u64(
                buf.borrow().len() as u64
            )))
        })),
    );
    env.define_const(
        ctx,
        "buffer_to_string",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
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
        })),
    );
    env.define_const(
        ctx,
        "buffer_from_string",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
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
                    Ok(Value::Buffer(ctx.gc_lock(bytes)))
                }
                _ => {
                    return type_error(ctx, "from_string can only be called on String", expr);
                }
            }
        })),
    );
    env.define_const(
        ctx,
        "buffer_with_size",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 1, expr)?;
            let size = expect_usize_arg(ctx, &args[0], expr)?;
            let buf = vec![0u8; size];
            Ok(Value::Buffer(ctx.gc_lock(buf)))
        })),
    );
}
