use std::collections::HashMap;

use gc_arena::{Gc, Mutation, lock::RefLock};
use rand::Rng;

use crate::gc_interpreter::{
    standard::{expect_arg_len, expect_integer_arg, make_builtin_function},
    value::{ClassValue, Environment, IntVariant, Value},
};

pub fn define_rand_globals<'a>(mc: &Mutation<'a>, env: Gc<'a, Environment<'a>>) {
    env.define(
        mc,
        "Random",
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
                        "int".to_string(),
                        make_builtin_function(mc, |mc, root, args, span, _| {
                            expect_arg_len(mc, root, &args, 2, span)?;
                            let min = expect_integer_arg(mc, root, &args[0], span)?;
                            let max = expect_integer_arg(mc, root, &args[1], span)?;
                            match (min, max) {
                                (IntVariant::Small(min), IntVariant::Small(max)) => {
                                    let random_int = rand::thread_rng().gen_range(*min..=*max);
                                    Ok(Value::Integer(IntVariant::Small(random_int)))
                                }
                                _ => {
                                    let min = min.as_big();
                                    let max = max.as_big();
                                    let random_int = rand::thread_rng().gen_range(min..=max);
                                    Ok(Value::Integer(IntVariant::Big(gc_arena::StaticCollect(
                                        random_int,
                                    ))))
                                }
                            }
                        }),
                    );
                    map
                },
                parent: None,
            }),
        )),
    );
}
