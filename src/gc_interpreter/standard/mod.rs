use std::collections::HashMap;

use gc_arena::{Gc, StaticCollect, lock::GcRefLock};

use crate::{
    gc_interpreter::{
        Context,
        exception::{function_argument_error, type_error},
        standard::{
            array::define_array_globals, buffer::define_typed_buffer_globals,
            future::define_future_globals, object::define_object_globals,
            rand::define_rand_globals, resource::define_resource_globals,
            string::define_string_globals, system::define_system_globals,
        },
        value::{
            BufferValue, ClassInstanceValue, ClassValue, Environment, FunctionValue,
            FunctionValueType, FutureValue, IntVariant, LocatedError, StringVariant, Value,
            value_to_string,
        },
    },
    location::Location,
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
        Value::Function(make_builtin_function(ctx, |_, args, _, _| {
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
        Value::Function(make_builtin_function(ctx, |ctx, args, span, _| {
            expect_arg_len(ctx, &args, 1, span)?;
            let type_str = args[0].get_type();
            Ok(Value::String(ctx.gc(StringVariant::from_string(type_str))))
        })),
    );
}

fn expect_arg_len<'a>(
    ctx: &Context<'a>,
    args: &Vec<Value<'a>>,
    expected: usize,
    expr: &Location,
) -> Result<(), LocatedError<'a>> {
    if args.len() != expected {
        function_argument_error(
            ctx,
            &format!("Expected {} arguments, got {}", expected, args.len()),
            expr,
        )
    } else {
        Ok(())
    }
}

fn expect_class_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, ClassValue<'a>>, LocatedError<'a>> {
    if let Value::Class(c) = value {
        Ok(c.clone())
    } else {
        type_error(
            ctx,
            &format!("Expected argument to be a Class, got {}", value.get_type()),
            expr,
        )
    }
}

fn expect_class_instance_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, ClassInstanceValue<'a>>, LocatedError<'a>> {
    if let Value::ClassInstance(c) = value {
        Ok(c.clone())
    } else {
        type_error(
            ctx,
            &format!(
                "Expected argument to be a ClassInstance, got {}",
                value.get_type()
            ),
            expr,
        )
    }
}

fn expect_object_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, HashMap<String, Value<'a>>>, LocatedError<'a>> {
    if let Value::Object(o) = value {
        Ok(*o)
    } else {
        type_error(
            ctx,
            &format!(
                "Expected argument to be an Object, got {}",
                value.get_type()
            ),
            expr,
        )
    }
}

fn expect_future_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, FutureValue<'a>>, LocatedError<'a>> {
    if let Value::Future(f) = value {
        Ok(f.clone())
    } else {
        type_error(
            ctx,
            &format!("Expected argument to be a Future, got {}", value.get_type()),
            expr,
        )
    }
}

fn expect_array_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<GcRefLock<'a, Vec<Value<'a>>>, LocatedError<'a>> {
    if let Value::Array(a) = value {
        Ok(*a)
    } else {
        type_error(
            ctx,
            &format!("Expected argument to be an Array, got {}", value.get_type()),
            expr,
        )
    }
}

fn expect_buffer_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<Gc<'a, BufferValue<'a>>, LocatedError<'a>> {
    if let Value::Buffer(b) = value {
        Ok(*b)
    } else {
        type_error(
            ctx,
            &format!("Expected argument to be a Buffer, got {}", value.get_type()),
            expr,
        )
    }
}

fn expect_string_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<Gc<'a, StringVariant<'a>>, LocatedError<'a>> {
    if let Value::String(s) = value {
        Ok(s.clone())
    } else {
        type_error(
            ctx,
            &format!("Expected argument to be a String, got {}", value.get_type()),
            expr,
        )
    }
}

fn expect_integer_arg<'a, 'b>(
    ctx: &Context<'a>,
    value: &'b Value<'a>,
    expr: &Location,
) -> Result<&'b IntVariant, LocatedError<'a>> {
    if let Value::Integer(i) = value {
        Ok(i)
    } else {
        type_error(
            ctx,
            &format!(
                "Expected argument to be an Integer, got {}",
                value.get_type()
            ),
            expr,
        )
    }
}

fn expect_usize_arg<'a>(
    ctx: &Context<'a>,
    value: &Value<'a>,
    expr: &Location,
) -> Result<usize, LocatedError<'a>> {
    if let Value::Integer(i) = value {
        i.try_to_usize().ok_or_else(|| {
            type_error::<usize>(ctx, "Integer too large to fit in usize", expr).unwrap_err()
        })
    } else {
        type_error(
            ctx,
            &format!(
                "Expected argument to be an Integer, got {}",
                value.get_type()
            ),
            expr,
        )
    }
}

fn make_builtin_function<'a>(
    ctx: &Context<'a>,
    func: impl for<'b> Fn(
        &Context<'b>,
        Vec<Value<'b>>,
        &Location,
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
