use std::collections::HashMap;

use crate::{
    gc_interpreter::{
        Context, Value, get_std_env,
        value::{ClassInstanceValue, IntVariant, LocatedError, StringVariant},
    },
    location::HasLocation,
    parser::{BinaryOp, Identifier, UnaryOp},
};

fn make_exception<'a>(ctx: &Context<'a>, msg: &str, variant: &str) -> Value<'a> {
    let std_env = get_std_env(ctx);
    let p = if let Some(Value::Class(p)) = std_env.get(variant) {
        p
    } else {
        let Some(Value::Class(p)) = std_env.get("Exception") else {
            panic!("Exception class not found");
        };
        p
    };

    let mut map = HashMap::new();
    map.insert(
        "message".to_string(),
        Value::String(ctx.gc(StringVariant::from_string(msg))),
    );
    Value::ClassInstance(ctx.gc_lock(ClassInstanceValue {
        fields: map,
        class: p,
    }))
}

pub fn runtime_error<'a, T>(
    ctx: &Context<'a>,
    msg: &str,
    variant: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    let exception = make_exception(ctx, msg, variant);
    Err(LocatedError {
        data: exception,
        location: expr.location().clone(),
    })
}

pub fn type_error<'a, T>(
    ctx: &Context<'a>,
    msg: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(ctx, msg, "TypeError", expr)
}

pub fn array_index_out_of_bounds<'a, T>(
    ctx: &Context<'a>,
    index: IntVariant,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Array index out of bounds: {}", index),
        "ArrayIndexOutOfBounds",
        expr,
    )
}

pub fn index_out_of_bounds<'a, T>(
    ctx: &Context<'a>,
    index: usize,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Array index out of bounds: {}", index),
        "ArrayIndexOutOfBounds",
        expr,
    )
}

pub fn function_argument_error<'a, T>(
    ctx: &Context<'a>,
    msg: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(ctx, msg, "FunctionArgumentError", expr)
}

pub fn io_error<'a, T>(
    ctx: &Context<'a>,
    msg: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(ctx, msg, "IOError", expr)
}

pub fn unhandled_control_flow<'a, T>(
    ctx: &Context<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(ctx, "Unhandled control flow", "RuntimeError", expr)
}

pub fn field_access_error<'a, T>(
    ctx: &Context<'a>,
    field: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Field access error: {}", field),
        "FieldAccessError",
        expr,
    )
}

pub fn cyclic_static_initialization<'a, T>(
    ctx: &Context<'a>,
    name: &Identifier,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Cyclic static initialization: {}", name),
        "CyclicStaticInitialization",
        expr,
    )
}

pub fn undefined_variable<'a, T>(
    ctx: &Context<'a>,
    name: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Undefined variable: {}", name),
        "UndefinedVariable",
        expr,
    )
}
pub fn undefined_identifier<'a, T>(
    ctx: &Context<'a>,
    name: &Identifier,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Undefined variable: {}", name),
        "UndefinedVariable",
        expr,
    )
}

pub fn duplicate_variable_definition<'a, T>(
    ctx: &Context<'a>,
    name: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Duplicate variable definition: {}", name),
        "DuplicateVariableDefinition",
        expr,
    )
}

pub fn division_by_zero<'a, T>(
    ctx: &Context<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(ctx, "Division by zero", "DivisionByZero", expr)
}

pub fn unsupported_binary_operation<'a, T>(
    ctx: &Context<'a>,
    op: &BinaryOp,
    left: &Value<'a>,
    right: &Value<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!(
            "Unsupported operation: {} between {} and {}",
            op,
            left.get_type(),
            right.get_type()
        ),
        "UnsupportedOperation",
        expr,
    )
}
pub fn unsupported_unary_operation<'a, T>(
    ctx: &Context<'a>,
    op: &UnaryOp,
    val: &Value<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Unsupported operation: {} on {}", op, val.get_type()),
        "UnsupportedOperation",
        expr,
    )
}

pub fn cannot_assign_to_constant<'a, T>(
    ctx: &Context<'a>,
    name: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        ctx,
        &format!("Cannot assign to constant: {}", name),
        "AssignmentToConstant",
        expr,
    )
}

pub fn cannot_instantiate<'a, T>(
    ctx: &Context<'a>,
    msg: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(ctx, msg, "CannotInstantiate", expr)
}
