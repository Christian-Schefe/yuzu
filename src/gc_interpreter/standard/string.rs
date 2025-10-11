use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    exception::{function_argument_error, type_error},
    standard::{expect_arg_len, expect_string_arg, expect_usize_arg, make_builtin_function},
    value::{Environment, IntVariant, StringVariant, Value, value_to_string},
};

pub fn define_string_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "to_string",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            let str = value_to_string(&args[0]);
            Ok(Value::String(ctx.gc(StringVariant::from_string(&str))))
        })),
    );
    env.define_const(
        ctx,
        "string_length",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 1, expr)?;
            let string = expect_string_arg(ctx, &args[0], expr)?;
            Ok(Value::Integer(IntVariant::from_u64(string.len() as u64)))
        })),
    );
    env.define_const(
        ctx,
        "string_concat",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() < 2 {
                return function_argument_error(
                    ctx,
                    &format!("Expected at least 2 arguments, got {}", args.len()),
                    expr,
                );
            }
            let strings = args
                .iter()
                .map(|arg| match arg {
                    Value::String(s) => Ok(s.to_string()),
                    _ => {
                        return type_error(ctx, "concat arguments must be strings", expr);
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Value::String(
                ctx.gc(StringVariant::from_string(&strings.join(""))),
            ))
        })),
    );
    env.define_const(
        ctx,
        "string_slice",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 3, expr)?;
            let s = expect_string_arg(ctx, &args[0], expr)?;
            let start = expect_usize_arg(ctx, &args[1], expr)?;
            let length = expect_usize_arg(ctx, &args[2], expr)?;
            if start + length > s.len() {
                return type_error(ctx, "Slice out of bounds", expr);
            }
            let Some(result) = StringVariant::slice(ctx, s, start, length) else {
                return type_error(ctx, "Slice out of bounds", expr);
            };
            Ok(Value::String(result))
        })),
    );
}
