use gc_arena::Gc;

use crate::gc_interpreter::{
    Context,
    exception::{function_argument_error, type_error},
    standard::{
        expect_arg_len, expect_class_arg, expect_class_instance_arg, expect_object_arg,
        make_builtin_function,
    },
    value::{Environment, StringVariant, Value},
};

pub fn define_object_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "object_get_class",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            if args.len() != 1 {
                return Err(function_argument_error(
                    ctx,
                    exec_ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                ));
            }
            let obj = expect_class_instance_arg(ctx, exec_ctx, &args[0])?;
            Ok(Value::Class(obj.borrow().class))
        })),
    );
    env.define_const(
        ctx,
        "object_keys",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            let obj = expect_object_arg(ctx, exec_ctx, &args[0])?;
            let keys = obj
                .borrow()
                .keys()
                .cloned()
                .map(|k| Value::String(ctx.gc(StringVariant::from_string(&k))))
                .collect::<Vec<_>>();
            Ok(Value::Array(ctx.gc_lock(keys)))
        })),
    );
    env.define_const(
        ctx,
        "class_constructor",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            if args.len() != 1 {
                return Err(function_argument_error(
                    ctx,
                    exec_ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                ));
            }
            let class = expect_class_arg(ctx, exec_ctx, &args[0])?;
            let Some(ctor) = class.borrow().constructor else {
                return Err(type_error(ctx, exec_ctx, "Class has no constructor"));
            };
            Ok(Value::Function(ctor))
        })),
    );
}
