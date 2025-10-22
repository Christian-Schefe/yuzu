use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    exception::function_argument_error,
    standard::{expect_arg_len, expect_array_arg, make_builtin_function},
    value::{Environment, IntVariant, Value},
};

pub fn define_array_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "array_length",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            let arr = expect_array_arg(ctx, exec_ctx, &args[0])?;
            Ok(Value::Integer(IntVariant::from_u64(
                arr.borrow().len() as u64
            )))
        })),
    );
    env.define_const(
        ctx,
        "array_push",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            if args.len() < 2 {
                return Err(function_argument_error(
                    ctx,
                    exec_ctx,
                    &format!("Expected at least 1 argument, got {}", args.len() - 1),
                ));
            }
            let arr = expect_array_arg(ctx, exec_ctx, &args[0])?;
            let mut arr = arr.borrow_mut(ctx.mc);
            for arg in args.iter().skip(1) {
                arr.push(arg.clone());
            }
            Ok(Value::Null)
        })),
    );
}
