use std::collections::HashMap;

use crate::{
    gc_interpreter::{
        Context, ExecContext, Value, get_class_maybe_lazy, get_std_env,
        value::{ClassInstanceValue, IntVariant, StringVariant},
    },
    parser::{BinaryOp, UnaryOp},
};

pub fn runtime_error<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    msg: &str,
    variant: &str,
) -> Value<'a> {
    let std_env = get_std_env(ctx);
    let p = if let Some(c) = get_class_maybe_lazy(std_env, variant) {
        c
    } else {
        let Some(p) = get_class_maybe_lazy(std_env, "Exception") else {
            panic!("Exception class not found or not initialized");
        };
        p
    };

    let stack_trace = exec_ctx.get_stack_trace();
    let mut map = HashMap::new();
    map.insert(
        "message".to_string(),
        Value::String(ctx.gc(StringVariant::from_string(msg))),
    );
    map.insert("stack_trace".to_string(), Value::StackTrace(stack_trace));
    Value::ClassInstance(ctx.gc_lock(ClassInstanceValue {
        inner: Value::Object(ctx.gc_lock(map)),
        class: p,
    }))
}

pub fn type_error<'a>(ctx: &Context<'a>, exec_ctx: &ExecContext<'a>, msg: &str) -> Value<'a> {
    runtime_error(ctx, exec_ctx, msg, "TypeError")
}

pub fn array_index_out_of_bounds<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    index: IntVariant,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        &format!("Array index out of bounds: {index}"),
        "ArrayIndexOutOfBounds",
    )
}

pub fn index_out_of_bounds<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    index: usize,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        &format!("Array index out of bounds: {index}"),
        "ArrayIndexOutOfBounds",
    )
}

pub fn function_argument_error<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    msg: &str,
) -> Value<'a> {
    runtime_error(ctx, exec_ctx, msg, "FunctionArgumentError")
}

pub fn io_error<'a>(ctx: &Context<'a>, exec_ctx: &ExecContext<'a>, msg: &str) -> Value<'a> {
    runtime_error(ctx, exec_ctx, msg, "IOError")
}

pub fn unhandled_control_flow<'a>(ctx: &Context<'a>, exec_ctx: &ExecContext<'a>) -> Value<'a> {
    runtime_error(ctx, exec_ctx, "Unhandled control flow", "RuntimeError")
}

pub fn field_access_error<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    field: &str,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        &format!("Field access error: {field}"),
        "FieldAccessError",
    )
}

pub fn cyclic_static_initialization<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        "Cyclic static initialization",
        "CyclicStaticInitialization",
    )
}

pub fn undefined_variable<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    name: &str,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        &format!("Undefined variable: {name}"),
        "UndefinedVariable",
    )
}

pub fn duplicate_variable_definition<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    name: &str,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        &format!("Duplicate variable definition: {name}"),
        "DuplicateVariableDefinition",
    )
}

pub fn division_by_zero<'a>(ctx: &Context<'a>, exec_ctx: &ExecContext<'a>) -> Value<'a> {
    runtime_error(ctx, exec_ctx, "Division by zero", "DivisionByZero")
}

pub fn unsupported_binary_operation<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    op: &BinaryOp,
    left: &Value<'a>,
    right: &Value<'a>,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        &format!(
            "Unsupported operation: {} between {} and {}",
            op,
            left.get_type(),
            right.get_type()
        ),
        "UnsupportedOperation",
    )
}
pub fn unsupported_unary_operation<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    op: &UnaryOp,
    val: &Value<'a>,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        &format!("Unsupported operation: {} on {}", op, val.get_type()),
        "UnsupportedOperation",
    )
}

pub fn cannot_assign_to_constant<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    name: &str,
) -> Value<'a> {
    runtime_error(
        ctx,
        exec_ctx,
        &format!("Cannot assign to constant: {name}"),
        "AssignmentToConstant",
    )
}

pub fn cannot_instantiate<'a>(
    ctx: &Context<'a>,
    exec_ctx: &ExecContext<'a>,
    msg: &str,
) -> Value<'a> {
    runtime_error(ctx, exec_ctx, msg, "CannotInstantiate")
}
