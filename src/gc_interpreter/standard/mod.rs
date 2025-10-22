use std::collections::HashMap;

use gc_arena::{Gc, StaticCollect, lock::GcRefLock};

use crate::gc_interpreter::{
    Context, ExecContext,
    exception::{function_argument_error, type_error},
    standard::{
        array::define_array_globals, buffer::define_typed_buffer_globals,
        future::define_future_globals, object::define_object_globals, rand::define_rand_globals,
        resource::define_resource_globals, string::define_string_globals,
        system::define_system_globals,
    },
    value::{
        BufferValue, ClassInstanceValue, ClassValue, Environment, FunctionValue, FunctionValueType,
        FutureValue, IntVariant, LocatedError, StringVariant, Value, value_to_string,
    },
};

mod array;
mod buffer;
mod future;
mod object;
mod rand;
mod resource;
mod string;
mod system;

pub fn define_intrinsics<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    define_system_globals(ctx, env);
    define_resource_globals(ctx, env);
    define_rand_globals(ctx, env);
    define_string_globals(ctx, env);
    define_typed_buffer_globals(ctx, env);
    define_array_globals(ctx, env);
    define_object_globals(ctx, env);
    define_future_globals(ctx, env);
}

pub fn define_globals<'a>(ctx: &Context<'a>, env: Gc<'a, Environment<'a>>) {
    env.define_const(
        ctx,
        "print",
        Value::Function(make_builtin_function(ctx, |_, _, args, _| {
            let str = args
                .into_iter()
                .map(|arg| value_to_string(&arg))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", str);
            Ok(Value::Null)
        })),
    );
    env.define_const(
        ctx,
        "typeof",
        Value::Function(make_builtin_function(ctx, |ctx, exec_ctx, args, _| {
            expect_arg_len(ctx, exec_ctx, &args, 1)?;
            let type_str = args[0].get_type();
            Ok(Value::String(ctx.gc(StringVariant::from_string(type_str))))
        })),
    );
}

fn expect_arg_len<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    args: &Vec<Value<'a>>,
    expected: usize,
) -> Result<(), LocatedError<'a>> {
    if args.len() != expected {
        Err(function_argument_error(
            ctx,
            exec_ctx,
            &format!("Expected {} arguments, got {}", expected, args.len()),
        ))
    } else {
        Ok(())
    }
}

fn expect_class_arg<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &Value<'a>,
) -> Result<GcRefLock<'a, ClassValue<'a>>, LocatedError<'a>> {
    if let Value::Class(c) = value {
        Ok(c.clone())
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!("Expected argument to be a Class, got {}", value.get_type()),
        ))
    }
}

fn expect_class_instance_arg<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &Value<'a>,
) -> Result<GcRefLock<'a, ClassInstanceValue<'a>>, LocatedError<'a>> {
    if let Value::ClassInstance(c) = value {
        Ok(c.clone())
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!(
                "Expected argument to be a ClassInstance, got {}",
                value.get_type()
            ),
        ))
    }
}

fn expect_object_arg<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &Value<'a>,
) -> Result<GcRefLock<'a, HashMap<String, Value<'a>>>, LocatedError<'a>> {
    if let Value::Object(o) = value {
        Ok(*o)
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!(
                "Expected argument to be an Object, got {}",
                value.get_type()
            ),
        ))
    }
}

fn expect_future_arg<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &Value<'a>,
) -> Result<GcRefLock<'a, FutureValue<'a>>, LocatedError<'a>> {
    if let Value::Future(f) = value {
        Ok(f.clone())
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!("Expected argument to be a Future, got {}", value.get_type()),
        ))
    }
}

fn expect_array_arg<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &Value<'a>,
) -> Result<GcRefLock<'a, Vec<Value<'a>>>, LocatedError<'a>> {
    if let Value::Array(a) = value {
        Ok(*a)
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!("Expected argument to be an Array, got {}", value.get_type()),
        ))
    }
}

fn expect_buffer_arg<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &Value<'a>,
) -> Result<Gc<'a, BufferValue<'a>>, LocatedError<'a>> {
    if let Value::Buffer(b) = value {
        Ok(*b)
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!("Expected argument to be a Buffer, got {}", value.get_type()),
        ))
    }
}

fn expect_string_arg<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &Value<'a>,
) -> Result<Gc<'a, StringVariant<'a>>, LocatedError<'a>> {
    if let Value::String(s) = value {
        Ok(s.clone())
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!("Expected argument to be a String, got {}", value.get_type()),
        ))
    }
}

fn expect_integer_arg<'a, 'b>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &'b Value<'a>,
) -> Result<&'b IntVariant, LocatedError<'a>> {
    if let Value::Integer(i) = value {
        Ok(i)
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!(
                "Expected argument to be an Integer, got {}",
                value.get_type()
            ),
        ))
    }
}

fn expect_usize_arg<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    value: &Value<'a>,
) -> Result<usize, LocatedError<'a>> {
    if let Value::Integer(i) = value {
        i.try_to_usize()
            .ok_or_else(|| type_error(ctx, exec_ctx, "Integer too large to fit in usize"))
    } else {
        Err(type_error(
            ctx,
            exec_ctx,
            &format!(
                "Expected argument to be an Integer, got {}",
                value.get_type()
            ),
        ))
    }
}

fn make_builtin_function<'a>(
    ctx: &Context<'a>,
    func: impl for<'b> Fn(
        &Context<'b>,
        &ExecContext<'b>,
        Vec<Value<'b>>,
        Gc<'b, Environment<'b>>,
    ) -> Result<Value<'b>, LocatedError<'b>>
    + 'static,
) -> GcRefLock<'a, FunctionValue<'a>> {
    ctx.gc_lock(FunctionValue::new(
        ctx,
        FunctionValueType::Builtin {
            func: StaticCollect(Box::new(func)),
        },
        false,
    ))
}
