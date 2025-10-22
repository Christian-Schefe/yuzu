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
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            if args.len() != 1 {
                return Err(function_argument_error(
                    ctx,
                    exec_ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                ));
            }
            let str = value_to_string(&args[0]);
            Ok(Value::String(ctx.gc(StringVariant::from_string(&str))))
        })),
    );
    env.define_const(
        ctx,
        "string_length",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            let string = expect_string_arg(ctx, exec_ctx, &args[0])?;
            Ok(Value::Integer(IntVariant::from_u64(string.len() as u64)))
        })),
    );
    env.define_const(
        ctx,
        "string_concat",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            if args.len() < 2 {
                return Err(function_argument_error(
                    ctx,
                    exec_ctx,
                    &format!("Expected at least 2 arguments, got {}", args.len()),
                ));
            }
            let strings = args
                .iter()
                .map(|arg| match arg {
                    Value::String(s) => Ok(s.to_string()),
                    _ => Err(type_error(
                        ctx,
                        exec_ctx,
                        "concat arguments must be strings",
                    )),
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
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 3)?;
            let s = expect_string_arg(ctx, exec_ctx, &args[0])?;
            let start = expect_usize_arg(ctx, exec_ctx, &args[1])?;
            let length = expect_usize_arg(ctx, exec_ctx, &args[2])?;
            if start + length > s.len() {
                return Err(type_error(ctx, exec_ctx, "Slice out of bounds"));
            }
            let Some(result) = StringVariant::slice(ctx, s, start, length) else {
                return Err(type_error(ctx, exec_ctx, "Slice out of bounds"));
            };
            Ok(Value::String(result))
        })),
    );
}
