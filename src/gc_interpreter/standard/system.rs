use std::collections::HashMap;

use gc_arena::{Gc, Mutation, lock::RefLock};

use crate::gc_interpreter::{
    standard::{expect_arg_len, make_builtin_function},
    value::{ClassValue, Environment, Value},
};

pub fn define_system_globals<'a>(mc: &Mutation<'a>, env: Gc<'a, Environment<'a>>) {
    env.define(
        mc,
        "System",
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
                        "args".to_string(),
                        make_builtin_function(mc, |mc, root, args, span, _| {
                            expect_arg_len(mc, root, &args, 0, span)?;
                            Ok(root.args.clone())
                        }),
                    );
                    map
                },
                parent: None,
            }),
        )),
    );
}
