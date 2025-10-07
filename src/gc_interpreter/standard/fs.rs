use std::collections::HashMap;

use gc_arena::{Gc, Mutation, lock::RefLock};

use crate::gc_interpreter::{
    exception::io_error,
    resource::FileResource,
    standard::{expect_arg_len, expect_string_arg, make_builtin_function},
    value::{ClassValue, Environment, Value},
};

pub fn define_fs_globals<'a>(mc: &Mutation<'a>, env: Gc<'a, Environment<'a>>) {
    env.define(
        mc,
        "FileSystem",
        Value::Class(Gc::new(
            mc,
            RefLock::new(ClassValue {
                instance_fields: Vec::new(),
                constructor: None,
                static_fields: HashMap::new(),
                methods: HashMap::new(),
                static_methods: {
                    let mut map = HashMap::new();
                    map.insert(
                        "open".to_string(),
                        make_builtin_function(mc, |mc, root, args, span, _| {
                            expect_arg_len(mc, root, &args, 1, span)?;
                            let path = expect_string_arg(mc, root, &args[0], span)?;
                            let path_str = path.to_string();
                            let file = FileResource::open(&path_str).map_err(|e| {
                                io_error::<()>(
                                    mc,
                                    root,
                                    &format!("Failed to open file {}: {}", path_str, e),
                                    span,
                                )
                                .unwrap_err()
                            })?;
                            Ok(Value::Resource(Gc::new(mc, RefLock::new(Box::new(file)))))
                        }),
                    );
                    map.insert(
                        "mkdir".to_string(),
                        make_builtin_function(mc, |mc, root, args, span, _| {
                            expect_arg_len(mc, root, &args, 1, span)?;
                            let path = expect_string_arg(mc, root, &args[0], span)?;
                            let path_str = path.to_string();
                            let file = FileResource::open(&path_str).map_err(|e| {
                                io_error::<()>(
                                    mc,
                                    root,
                                    &format!("Failed to open file {}: {}", path_str, e),
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
}
