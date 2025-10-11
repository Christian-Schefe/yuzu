use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    standard::{expect_arg_len, make_builtin_function},
    value::{Environment, Value},
};

pub fn define_system_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "sys_args",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 0, span)?;
            Ok(ctx.root.args.clone())
        })),
    );
}
