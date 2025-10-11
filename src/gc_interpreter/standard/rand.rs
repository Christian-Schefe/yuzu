use gc_arena::Gc;
use rand::Rng;

use crate::gc_interpreter::{
    Context,
    standard::{expect_arg_len, expect_integer_arg, make_builtin_function},
    value::{Environment, IntVariant, Value},
};

pub fn define_rand_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "rand_int",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 2, span)?;
            let min = expect_integer_arg(ctx, &args[0], span)?;
            let max = expect_integer_arg(ctx, &args[1], span)?;
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
        })),
    );
}
