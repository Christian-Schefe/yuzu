use std::collections::HashMap;

use gc_arena::{Gc, Mutation, lock::RefLock};

use crate::{
    gc_interpreter::{
        Environment, MyRoot, Value,
        value::{ControlFlow, LocatedControlFlow, ObjectValue},
    },
    tree_interpreter::{HasLocation, Located},
};

fn make_exception<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    variant: &str,
    env: &Environment<'a>,
) -> Value<'a> {
    let p = if let Some(Value::Prototype(p)) = env.root().get(variant) {
        p
    } else {
        root.root_prototypes.exception.clone()
    };

    let mut map = HashMap::new();
    map.insert("message".to_string(), Value::String(msg.chars().collect()));
    Value::Object(Gc::new(
        mc,
        RefLock::new(ObjectValue {
            properties: map,
            prototype: p,
        }),
    ))
}

pub fn runtime_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    variant: &str,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    let exception = make_exception(mc, root, msg, variant, env);
    Err(Located {
        data: ControlFlow::Error(exception),
        location: expr.location().clone(),
    })
}

pub fn type_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(mc, root, msg, "TypeError", env, expr)
}

pub fn array_index_out_of_bounds<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    index: i64,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Array index out of bounds: {}", index),
        "ArrayIndexOutOfBounds",
        env,
        expr,
    )
}

pub fn function_argument_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(mc, root, msg, "FunctionArgumentError", env, expr)
}

pub fn io_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(mc, root, msg, "IOError", env, expr)
}

pub fn unhandled_control_flow<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(
        mc,
        root,
        "Unhandled control flow",
        "RuntimeError",
        env,
        expr,
    )
}

pub fn field_access_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    field: String,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Field access error: {}", field),
        "FieldAccessError",
        env,
        expr,
    )
}

pub fn undefined_variable<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    name: &str,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Undefined variable: {}", name),
        "UndefinedVariable",
        env,
        expr,
    )
}

pub fn duplicate_variable_definition<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    name: &str,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Duplicate variable definition: {}", name),
        "DuplicateVariableDefinition",
        env,
        expr,
    )
}

pub fn import_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    env: &Environment<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedControlFlow<'a>> {
    runtime_error(mc, root, msg, "ImportError", env, expr)
}
