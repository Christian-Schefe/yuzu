use std::collections::HashMap;

use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    exception::io_error,
    resource::FileResource,
    standard::{expect_arg_len, expect_string_arg, make_builtin_function},
    value::{ClassValue, Environment, Value},
};

pub fn define_fs_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define(
        ctx,
        "FileSystem",
        Value::Class(ctx.gc_lock(ClassValue {
            constructor: None,
            methods: HashMap::new(),
            static_methods: {
                let mut map = HashMap::new();
                map.insert(
                    "open".to_string(),
                    make_builtin_function(ctx, |ctx, args, span, _| {
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
                    }),
                );
                map.insert(
                    "mkdir".to_string(),
                    make_builtin_function(ctx, |ctx, args, span, _| {
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
                    }),
                );
                map
            },
            parent: None,
        })),
    );
}
