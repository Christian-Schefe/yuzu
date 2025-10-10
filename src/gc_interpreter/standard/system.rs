use std::collections::HashMap;

use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    standard::{expect_arg_len, make_builtin_function},
    value::{ClassValue, Environment, Value},
};

pub fn define_system_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define(
        ctx,
        "System",
        Value::Class(ctx.gc_lock(ClassValue {
            constructor: None,
            methods: HashMap::new(),
            static_methods: {
                let mut map = HashMap::new();
                map.insert(
                    "args".to_string(),
                    make_builtin_function(ctx, |ctx, args, span, _| {
                        expect_arg_len(ctx, &args, 0, span)?;
                        Ok(ctx.root.args.clone())
                    }),
                );
                map
            },
            parent: None,
        })),
    );
}
