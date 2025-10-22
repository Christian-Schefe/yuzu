use gc_arena::{Gc, StaticCollect};

use crate::gc_interpreter::{
    Context,
    exception::{function_argument_error, io_error, type_error},
    resource::{FileResource, TcpListenerResource, TcpStreamResource},
    standard::{
        expect_arg_len, expect_buffer_arg, expect_string_arg, expect_usize_arg,
        make_builtin_function,
    },
    value::{Environment, IntVariant, Value},
};

pub fn define_resource_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "fs_open",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            let path = expect_string_arg(ctx, exec_ctx, &args[0])?;
            let path_str = path.to_string();
            let file = FileResource::open(&path_str).map_err(|e| {
                io_error(
                    ctx,
                    exec_ctx,
                    &format!("Failed to open file {}: {}", path_str, e),
                )
            })?;
            Ok(Value::Resource(ctx.gc_lock(Box::new(file))))
        })),
    );
    env.define_const(
        ctx,
        "fs_mkdir",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            let path = expect_string_arg(ctx, exec_ctx, &args[0])?;
            let path_str = path.to_string();
            let file = FileResource::open(&path_str).map_err(|e| {
                io_error(
                    ctx,
                    exec_ctx,
                    &format!("Failed to open file {}: {}", path_str, e),
                )
            })?;
            Ok(Value::Resource(ctx.gc_lock(Box::new(file))))
        })),
    );
    env.define_const(
        ctx,
        "tcp_connect",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            let host = expect_string_arg(ctx, exec_ctx, &args[0])?.to_string();
            let port = expect_usize_arg(ctx, exec_ctx, &args[1])?;
            let port = if let Some(port) = TryInto::<u16>::try_into(port).ok() {
                port
            } else {
                return Err(type_error(
                    ctx,
                    exec_ctx,
                    "Port argument must be a valid u16",
                ));
            };
            let socket = TcpStreamResource::connect(&host, port).map_err(|e| {
                io_error(
                    ctx,
                    exec_ctx,
                    &format!("Failed to connect to {}:{}: {}", host, port, e),
                )
            })?;
            Ok(Value::Resource(ctx.gc_lock(Box::new(socket))))
        })),
    );
    env.define_const(
        ctx,
        "tcp_listen",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            let addr = expect_string_arg(ctx, exec_ctx, &args[0])?.to_string();
            let port = expect_usize_arg(ctx, exec_ctx, &args[1])?;
            let port = if let Some(port) = TryInto::<u16>::try_into(port).ok() {
                port
            } else {
                return Err(type_error(
                    ctx,
                    exec_ctx,
                    "Port argument must be a valid u16",
                ));
            };
            let socket = TcpListenerResource::bind(&addr, port).map_err(|e| {
                io_error(
                    ctx,
                    exec_ctx,
                    &format!("Failed to listen on {}:{}: {}", addr, port, e),
                )
            })?;
            Ok(Value::Resource(ctx.gc_lock(Box::new(socket))))
        })),
    );
    env.define_const(
        ctx,
        "tcp_accept",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(ctx.mc);
                    match res_ref.as_any_mut().downcast_mut::<TcpListenerResource>() {
                        Some(listener) => {
                            let Some((stream, _)) = listener.accept().map_err(|e| {
                                io_error(
                                    ctx,
                                    exec_ctx,
                                    &format!("Failed to accept connection: {}", e),
                                )
                            })?
                            else {
                                return Ok(Value::Null);
                            };
                            let socket = TcpStreamResource {
                                stream: StaticCollect(Some(stream)),
                            };
                            Ok(Value::Resource(ctx.gc_lock(Box::new(socket))))
                        }
                        None => {
                            return Err(type_error(
                                ctx,
                                exec_ctx,
                                "Accept argument must be a TcpListener resource",
                            ));
                        }
                    }
                }
                _ => {
                    return Err(type_error(
                        ctx,
                        exec_ctx,
                        "Accept argument must be a resource",
                    ));
                }
            }
        })),
    );
    env.define_const(
        ctx,
        "resource_close",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            if args.len() != 1 {
                return Err(function_argument_error(
                    ctx,
                    exec_ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                ));
            }
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(ctx.mc);
                    res_ref.close().map_err(|e| {
                        io_error(ctx, exec_ctx, &format!("Failed to close resource: {}", e))
                    })?;
                    Ok(Value::Null)
                }
                _ => {
                    return Err(type_error(
                        ctx,
                        exec_ctx,
                        "Close argument must be a resource",
                    ));
                }
            }
        })),
    );
    env.define_const(
        ctx,
        "resource_read",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(ctx.mc);
                    let buf = expect_buffer_arg(ctx, exec_ctx, &args[1])?;
                    buf.with_mut_slice(ctx, |slice| {
                        let bytes_read = res_ref.read(slice).map_err(|e| {
                            io_error(
                                ctx,
                                exec_ctx,
                                &format!("Failed to read from resource: {}", e),
                            )
                        })?;
                        Ok(Value::Integer(IntVariant::from_u64(bytes_read as u64)))
                    })
                }
                _ => {
                    return Err(type_error(
                        ctx,
                        exec_ctx,
                        "Read argument must be a resource",
                    ));
                }
            }
        })),
    );
    env.define_const(
        ctx,
        "resource_try_read",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(ctx.mc);
                    let buf = expect_buffer_arg(ctx, exec_ctx, &args[1])?;
                    buf.with_mut_slice(ctx, |slice| {
                        let bytes_read = res_ref
                            .try_read(slice)
                            .map_err(|e| {
                                io_error(
                                    ctx,
                                    exec_ctx,
                                    &format!("Failed to read from resource: {}", e),
                                )
                            })?
                            .map_or(Value::Null, |n| {
                                Value::Integer(IntVariant::from_u64(n as u64))
                            });
                        Ok(bytes_read)
                    })
                }
                _ => {
                    return Err(type_error(
                        ctx,
                        exec_ctx,
                        "Read argument must be a resource",
                    ));
                }
            }
        })),
    );
    env.define_const(
        ctx,
        "resource_write",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            match &args[0] {
                Value::Resource(res) => {
                    let mut res_ref = res.borrow_mut(ctx.mc);
                    let buf = expect_buffer_arg(ctx, exec_ctx, &args[1])?;
                    buf.with_slice(|slice| {
                        let bytes_written = res_ref.write(slice).map_err(|e| {
                            io_error(
                                ctx,
                                exec_ctx,
                                &format!("Failed to write to resource: {}", e),
                            )
                        })?;
                        Ok(Value::Integer(IntVariant::from_u64(bytes_written as u64)))
                    })
                }
                _ => {
                    return Err(type_error(
                        ctx,
                        exec_ctx,
                        "Write argument must be a resource",
                    ));
                }
            }
        })),
    );
}
