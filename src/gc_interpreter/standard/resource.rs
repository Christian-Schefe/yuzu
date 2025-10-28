use gc_arena::{Gc, StaticCollect};

use crate::gc_interpreter::{
    Context,
    exception::{function_argument_error, io_error, type_error},
    resource::{AsyncFileResource, FileResource, TcpListenerResource, TcpStreamResource},
    standard::{
        do_async, expect_arg_len, expect_buffer_arg, expect_string_arg, expect_usize_arg,
        make_builtin_function,
    },
    value::{Environment, IntVariant, Value, get_async_resource},
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
                    &format!("Failed to open file {path_str}: {e}"),
                )
            })?;
            Ok(Value::Resource(ctx.gc_lock(Box::new(file))))
        })),
    );
    env.define_const(
        ctx,
        "fs_open_async",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            let path = expect_string_arg(ctx, exec_ctx, &args[0])?;
            let path_str = path.to_string();

            let fut = AsyncFileResource::open(path_str.clone());

            do_async(
                ctx,
                exec_ctx,
                fut,
                Vec::new(),
                move |ctx, exec_ctx, res, _| match res {
                    Err(e) => Err(io_error(
                        ctx,
                        exec_ctx,
                        &format!("Failed to open file asynchronously {path_str}: {e}"),
                    )),
                    Ok(res) => Ok(Value::AsyncResource(ctx.gc(StaticCollect(Box::new(res))))),
                },
            )
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
                    &format!("Failed to open file {path_str}: {e}"),
                )
            })?;
            Ok(Value::Resource(ctx.gc_lock(Box::new(file))))
        })),
    );

    env.define_const(
        ctx,
        "tcp_connect_async",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            let host = expect_string_arg(ctx, exec_ctx, &args[0])?.to_string();
            let port = expect_usize_arg(ctx, exec_ctx, &args[1])?;
            let port = if let Ok(port) = TryInto::<u16>::try_into(port) {
                port
            } else {
                return Err(type_error(
                    ctx,
                    exec_ctx,
                    "Port argument must be a valid u16",
                ));
            };
            let fut = TcpStreamResource::connect(host.clone(), port);
            do_async(
                ctx,
                exec_ctx,
                fut,
                Vec::new(),
                move |ctx, exec_ctx, res, _| match res {
                    Err(e) => Err(io_error(
                        ctx,
                        exec_ctx,
                        &format!("Failed to connect to {host}:{port}: {e}"),
                    )),
                    Ok(res) => Ok(Value::AsyncResource(ctx.gc(StaticCollect(Box::new(res))))),
                },
            )
        })),
    );
    env.define_const(
        ctx,
        "tcp_listen_async",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            let addr = expect_string_arg(ctx, exec_ctx, &args[0])?.to_string();
            let port = expect_usize_arg(ctx, exec_ctx, &args[1])?;
            let port = if let Ok(port) = TryInto::<u16>::try_into(port) {
                port
            } else {
                return Err(type_error(
                    ctx,
                    exec_ctx,
                    "Port argument must be a valid u16",
                ));
            };
            let fut = TcpListenerResource::bind(addr.clone(), port);

            do_async(
                ctx,
                exec_ctx,
                fut,
                Vec::new(),
                move |ctx, exec_ctx, res, _| match res {
                    Err(e) => Err(io_error(
                        ctx,
                        exec_ctx,
                        &format!("Failed to bind to {addr}:{port}: {e}"),
                    )),
                    Ok(res) => Ok(Value::AsyncResource(ctx.gc(StaticCollect(Box::new(res))))),
                },
            )
        })),
    );
    env.define_const(
        ctx,
        "tcp_accept_async",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            match &args[0] {
                Value::AsyncResource(res) => {
                    let Some(listener) = get_async_resource::<TcpListenerResource>(&*res.0) else {
                        return Err(type_error(
                            ctx,
                            exec_ctx,
                            "Accept argument must be a TcpListener resource",
                        ));
                    };
                    let fut = listener.accept();

                    do_async(
                        ctx,
                        exec_ctx,
                        fut,
                        Vec::new(),
                        |ctx, exec_ctx, res, _| match res {
                            Err(e) => Err(io_error(
                                ctx,
                                exec_ctx,
                                &format!("Failed to accept TCP connection: {e}"),
                            )),
                            Ok(res) => {
                                Ok(Value::AsyncResource(ctx.gc(StaticCollect(Box::new(res)))))
                            }
                        },
                    )
                }
                _ => Err(type_error(
                    ctx,
                    exec_ctx,
                    "Accept argument must be a resource",
                )),
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
                        io_error(ctx, exec_ctx, &format!("Failed to close resource: {e}"))
                    })?;
                    Ok(Value::Null)
                }
                _ => Err(type_error(
                    ctx,
                    exec_ctx,
                    "Close argument must be a resource",
                )),
            }
        })),
    );
    env.define(
        ctx,
        "resource_close_async",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            match &args[0] {
                Value::AsyncResource(res) => {
                    let resource = res.0.clone();
                    let fut = async move { resource.close().await };
                    do_async(
                        ctx,
                        exec_ctx,
                        fut,
                        vec![],
                        move |ctx, exec_ctx, res, _| match res {
                            Err(e) => Err(io_error(
                                ctx,
                                exec_ctx,
                                &format!("Failed to close resource: {e}"),
                            )),
                            Ok(_) => Ok(Value::Null),
                        },
                    )
                }
                _ => Err(type_error(
                    ctx,
                    exec_ctx,
                    "Close argument must be an async resource",
                )),
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
                            io_error(ctx, exec_ctx, &format!("Failed to read from resource: {e}"))
                        })?;
                        Ok(Value::Integer(IntVariant::from_u64(bytes_read as u64)))
                    })
                }
                _ => Err(type_error(
                    ctx,
                    exec_ctx,
                    "Read argument must be a resource",
                )),
            }
        })),
    );
    env.define_const(
        ctx,
        "resource_read_async",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            match &args[0] {
                Value::AsyncResource(res) => {
                    let resource = res.0.clone();
                    let buf = expect_buffer_arg(ctx, exec_ctx, &args[1])?;
                    let len = buf.length;
                    let fut = async move {
                        let mut buffer = vec![0u8; len];
                        let read_fut = resource.read(&mut buffer);
                        let bytes_read = read_fut.await;
                        bytes_read.map(|n| (n, buffer))
                    };
                    do_async(
                        ctx,
                        exec_ctx,
                        fut,
                        vec![Value::Buffer(buf)],
                        move |ctx, exec_ctx, res, args| {
                            let Value::Buffer(buf) = &args[0] else {
                                unreachable!()
                            };

                            buf.with_mut_slice(ctx, |slice| match res {
                                Err(e) => Err(io_error(
                                    ctx,
                                    exec_ctx,
                                    &format!("Failed to read from resource: {e}"),
                                )),
                                Ok((n, buffer)) => {
                                    slice[..n].copy_from_slice(&buffer[..n]);
                                    Ok(Value::Integer(IntVariant::from_u64(n as u64)))
                                }
                            })
                        },
                    )
                }
                _ => Err(type_error(
                    ctx,
                    exec_ctx,
                    "Read argument must be an async resource",
                )),
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
                            io_error(ctx, exec_ctx, &format!("Failed to write to resource: {e}"))
                        })?;
                        Ok(Value::Integer(IntVariant::from_u64(bytes_written as u64)))
                    })
                }
                _ => Err(type_error(
                    ctx,
                    exec_ctx,
                    "Write argument must be a resource",
                )),
            }
        })),
    );
    env.define(
        ctx,
        "resource_write_async",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            match &args[0] {
                Value::AsyncResource(res) => {
                    let resource = res.0.clone();
                    let buf = expect_buffer_arg(ctx, exec_ctx, &args[1])?;
                    let data = buf.with_slice(|slice| slice.to_vec());
                    let fut = async move {
                        let write_fut = resource.write(&data);
                        let bytes_written = write_fut.await;
                        bytes_written
                    };
                    do_async(
                        ctx,
                        exec_ctx,
                        fut,
                        vec![Value::Buffer(buf)],
                        move |ctx, exec_ctx, res, _| match res {
                            Err(e) => Err(io_error(
                                ctx,
                                exec_ctx,
                                &format!("Failed to write to resource: {e}"),
                            )),
                            Ok(n) => Ok(Value::Integer(IntVariant::from_u64(n as u64))),
                        },
                    )
                }
                _ => Err(type_error(
                    ctx,
                    exec_ctx,
                    "Write argument must be an async resource",
                )),
            }
        })),
    );
}
