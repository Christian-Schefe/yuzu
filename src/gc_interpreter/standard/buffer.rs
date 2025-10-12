use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    exception::{function_argument_error, index_out_of_bounds, type_error},
    standard::{expect_arg_len, expect_buffer_arg, expect_usize_arg, make_builtin_function},
    value::{BufferValue, Environment, IntVariant, StringVariant, TypedBufferType, Value},
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
                Ok(Value::TypedBuffer {
                    buffer: buf.clone(),
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
                Value::TypedBuffer {
                    buffer,
                    buffer_type,
                } => {
                    let length = buffer.length / buffer_type.byte_size();
                    Ok(Value::Integer(IntVariant::from_u64(length as u64)))
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
            Ok(Value::Integer(IntVariant::from_u64(buf.length as u64)))
        })),
    );
    env.define_const(
        ctx,
        "buffer_slice",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 3, expr)?;
            let buf = expect_buffer_arg(ctx, &args[0], expr)?;
            let start = expect_usize_arg(ctx, &args[1], expr)?;
            let length = expect_usize_arg(ctx, &args[2], expr)?;
            let Some(slice) = buf.slice(start, length) else {
                return index_out_of_bounds(ctx, start + length, expr);
            };
            Ok(Value::Buffer(ctx.gc(slice)))
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
                Value::Buffer(buf) => buf.with_slice(|slice| {
                    let s = String::from_utf8_lossy(&slice);
                    Ok(Value::String(ctx.gc(StringVariant::from_string(&s))))
                }),
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
                    Ok(Value::Buffer(ctx.gc(BufferValue {
                        length: bytes.len(),
                        buffer: ctx.gc_lock(bytes),
                        start: 0,
                    })))
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
            Ok(Value::Buffer(ctx.gc(BufferValue {
                buffer: ctx.gc_lock(buf),
                start: 0,
                length: size,
            })))
        })),
    );
    env.define_const(
        ctx,
        "buffer_copy",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 2, expr)?;
            let src_buf = expect_buffer_arg(ctx, &args[0], expr)?;
            let dst_buf = expect_buffer_arg(ctx, &args[1], expr)?;
            if src_buf.length != dst_buf.length {
                return type_error(
                    ctx,
                    "Source and destination buffers must be the same length",
                    expr,
                );
            }
            dst_buf.with_mut_slice(ctx, |dst_slice| {
                src_buf.with_slice(|src_slice| {
                    dst_slice.copy_from_slice(&src_slice);
                })
            });
            Ok(Value::Null)
        })),
    );
}
