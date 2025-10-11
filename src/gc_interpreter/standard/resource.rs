use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    exception::{function_argument_error, io_error, type_error},
    resource::{FileResource, SocketResource},
    standard::{expect_arg_len, expect_string_arg, expect_usize_arg, make_builtin_function},
    value::{Environment, IntVariant, Value},
};

pub fn define_resource_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "fs_open",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 1, span)?;
            let path = expect_string_arg(ctx, &args[0], span)?;
            let path_str = path.to_string();
            let file = FileResource::open(&path_str).map_err(|e| {
                io_error::<()>(
                    ctx,
                    &format!("Failed to open file {}: {}", path_str, e),
                    span,
                )
                .unwrap_err()
            })?;
            Ok(Value::Resource(ctx.gc_lock(Box::new(file))))
        })),
    );
    env.define_const(
        ctx,
        "fs_mkdir",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 1, span)?;
            let path = expect_string_arg(ctx, &args[0], span)?;
            let path_str = path.to_string();
            let file = FileResource::open(&path_str).map_err(|e| {
                io_error::<()>(
                    ctx,
                    &format!("Failed to open file {}: {}", path_str, e),
                    span,
                )
                .unwrap_err()
            })?;
            Ok(Value::Resource(ctx.gc_lock(Box::new(file))))
        })),
    );
    env.define_const(
        ctx,
        "socket_connect",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
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
        })),
    );
    env.define_const(
        ctx,
        "resource_close",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
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
        })),
    );
    env.define_const(
        ctx,
        "resource_read",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
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
        })),
    );
    env.define_const(
        ctx,
        "resource_write",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
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
        })),
    );
}
