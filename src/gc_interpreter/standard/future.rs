use gc_arena::{Gc, StaticCollect};

use crate::gc_interpreter::{
    Context,
    standard::{expect_arg_len, expect_future_arg, make_builtin_function},
    value::{Environment, FutureValue, Task, Value},
};

pub fn define_future_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "future_new",
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 0, span)?;
            Ok(Value::Future(
                ctx.gc_lock(FutureValue::Pending(Task::Never)),
            ))
        })),
    );
    env.define_const(
        ctx,
        "future_resolve",
        Value::Function(make_builtin_function(ctx, |ctx, mut args, span, _| {
            expect_arg_len(ctx, &args, 2, span)?;
            let future = expect_future_arg(ctx, &args[0], span)?;
            let mut fut = future.borrow_mut(ctx.mc);
            *fut = FutureValue::Completed(args.pop().unwrap());
            Ok(Value::Null)
        })),
    );
    env.define_const(
        ctx,
        "future_reject",
        Value::Function(make_builtin_function(ctx, |ctx, mut args, span, _| {
            expect_arg_len(ctx, &args, 2, span)?;
            let future = expect_future_arg(ctx, &args[0], span)?;
            let mut fut = future.borrow_mut(ctx.mc);
            *fut = FutureValue::Failed(args.pop().unwrap(), StaticCollect(span.clone()));
            Ok(Value::Null)
        })),
    );
}
