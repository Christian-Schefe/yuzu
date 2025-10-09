use std::collections::HashMap;

use gc_arena::{Gc, Mutation, lock::RefLock};

use crate::{
    gc_interpreter::{
        MyRoot, Value, get_std_env,
        value::{ClassInstanceValue, IntVariant, LocatedError, StringVariant},
    },
    location::HasLocation,
    parser::BinaryOp,
};

fn make_exception<'a>(mc: &Mutation<'a>, root: &MyRoot<'a>, msg: &str, variant: &str) -> Value<'a> {
    let std_env = get_std_env(root);
    let p = if let Some(Value::Class(p)) = std_env.get_simple(variant) {
        p
    } else {
        let Some(Value::Class(p)) = std_env.get_simple("Exception") else {
            panic!("Exception class not found");
        };
        p
    };

    let mut map = HashMap::new();
    map.insert(
        "message".to_string(),
        Value::String(Gc::new(mc, StringVariant::from_string(msg))),
    );
    Value::ClassInstance(Gc::new(
        mc,
        RefLock::new(ClassInstanceValue {
            fields: map,
            class: p,
        }),
    ))
}

pub fn runtime_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    variant: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    let exception = make_exception(mc, root, msg, variant);
    Err(LocatedError {
        data: exception,
        location: expr.location().clone(),
    })
}

pub fn type_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(mc, root, msg, "TypeError", expr)
}

pub fn array_index_out_of_bounds<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    index: IntVariant,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Array index out of bounds: {}", index),
        "ArrayIndexOutOfBounds",
        expr,
    )
}

pub fn index_out_of_bounds<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    index: usize,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Array index out of bounds: {}", index),
        "ArrayIndexOutOfBounds",
        expr,
    )
}

pub fn function_argument_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(mc, root, msg, "FunctionArgumentError", expr)
}

pub fn io_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    msg: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(mc, root, msg, "IOError", expr)
}

pub fn unhandled_control_flow<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(mc, root, "Unhandled control flow", "RuntimeError", expr)
}

pub fn field_access_error<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    field: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Field access error: {}", field),
        "FieldAccessError",
        expr,
    )
}

pub fn cyclic_static_initialization<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    name: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Cyclic static initialization: {}", name),
        "CyclicStaticInitialization",
        expr,
    )
}

pub fn undefined_variable<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    name: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Undefined variable: {}", name),
        "UndefinedVariable",
        expr,
    )
}

pub fn duplicate_variable_definition<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    name: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Duplicate variable definition: {}", name),
        "DuplicateVariableDefinition",
        expr,
    )
}

pub fn division_by_zero<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(mc, root, "Division by zero", "DivisionByZero", expr)
}

pub fn unsupported_binary_operation<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    op: &BinaryOp,
    left: &Value<'a>,
    right: &Value<'a>,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        mc,
        root,
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

pub fn cannot_assign_to_constant<'a, T>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    name: &str,
    expr: impl HasLocation,
) -> Result<T, LocatedError<'a>> {
    runtime_error(
        mc,
        root,
        &format!("Cannot assign to constant: {}", name),
        "AssignmentToConstant",
        expr,
    )
}
