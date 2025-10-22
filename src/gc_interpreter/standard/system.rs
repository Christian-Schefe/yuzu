use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    standard::{expect_arg_len, expect_usize_arg, make_builtin_function},
    value::{Environment, Value},
};

pub fn define_system_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "sys_args",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 0)?;
            Ok(ctx.root.args.clone())
        })),
    );
    env.define_const(
        ctx,
        "sys_sleep",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            let duration = expect_usize_arg(ctx, exec_ctx, &args[0])?;
            std::thread::sleep(std::time::Duration::from_millis(duration as u64));
            Ok(Value::Null)
        })),
    );
}
