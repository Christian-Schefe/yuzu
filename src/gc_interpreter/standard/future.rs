use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    standard::{expect_arg_len, expect_future_arg, make_builtin_function},
    value::{Environment, FutureValue, Task, Value},
};

pub fn define_future_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "future_new",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 0)?;
            Ok(Value::Future(
                ctx.gc_lock(FutureValue::Pending(Task::Never)),
            ))
        })),
    );
    env.define_const(
        ctx,
        "future_resolve",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, mut args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            let future = expect_future_arg(ctx, exec_ctx, &args[0])?;
            let mut fut = future.borrow_mut(ctx.mc);
            *fut = FutureValue::Completed(args.pop().unwrap());
            Ok(Value::Null)
        })),
    );
    env.define_const(
        ctx,
        "future_reject",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, mut args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 2)?;
            let future = expect_future_arg(ctx, exec_ctx, &args[0])?;
            let mut fut = future.borrow_mut(ctx.mc);
            *fut = FutureValue::Failed(args.pop().unwrap());
            Ok(Value::Null)
        })),
    );
}
