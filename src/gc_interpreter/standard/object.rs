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
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            let obj = expect_class_instance_arg(ctx, &args[0], expr)?;
            Ok(Value::Class(obj.borrow().class))
        })),
    );
    env.define_const(
        ctx,
        "object_keys",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            expect_arg_len(ctx, &args, 1, expr)?;
            let obj = expect_object_arg(ctx, &args[0], expr)?;
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
        "class_super",
        Value::Function(make_builtin_function(ctx, |ctx, args, expr, _| {
            if args.len() != 1 {
                return function_argument_error(
                    ctx,
                    &format!("Expected 1 argument, got {}", args.len()),
                    expr,
                );
            }
            let class = expect_class_arg(ctx, &args[0], expr)?;
            let Some(parent) = class.borrow().parent else {
                return type_error(ctx, "Class has no parent", expr);
            };
            let Some((_, ctor)) = parent.borrow().constructor else {
                return type_error(ctx, "Parent class has no constructor", expr);
            };
            Ok(Value::Function(ctor))
        })),
    );
}
