mod value_type;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use value_type::*;

use crate::{
    location::{Located, Location},
    parser::{BinaryOp, Expression, LocatedExpression, MemberKind, TypeHint},
};

pub enum Frame<'a> {
    Eval(&'a LocatedExpression),
    Kont(Located<Kont<'a>>),
}

impl<'a> Frame<'a> {
    pub fn eval(expr: &'a LocatedExpression, env: &Rc<RefCell<Environment>>) -> EnvFrame<'a> {
        EnvFrame {
            frame: Frame::Eval(expr),
            env: env.clone(),
        }
    }

    pub fn kont(
        kont: Kont<'a>,
        env: &Rc<RefCell<Environment>>,
        location: Location,
    ) -> EnvFrame<'a> {
        EnvFrame {
            frame: Frame::Kont(Located {
                data: kont,
                location,
            }),
            env: env.clone(),
        }
    }
}

pub enum Kont<'a> {
    Block {
        remaining_exprs: Vec<&'a LocatedExpression>,
        has_last: bool,
    },
    ObjectLiteral {
        properties: Vec<(&'a String, &'a LocatedExpression)>,
        collected: HashMap<String, TypeId>,
        current_key: String,
    },
    ArrayLiteral {
        elements: Vec<&'a LocatedExpression>,
        collected: Vec<TypeId>,
    },
    FunctionLiteral {
        function_type: TypeId,
    },
    ClassLiteralShell {
        class_type: TypeId,
        instance_type: TypeId,
        parent: Option<&'a LocatedExpression>,
        properties: Vec<(
            &'a String,
            &'a MemberKind,
            &'a Option<TypeHint>,
            &'a LocatedExpression,
        )>,
    },
    ClassLiteralFunctionBodies {
        properties: Vec<(
            &'a String,
            &'a MemberKind,
            &'a Option<TypeHint>,
            &'a LocatedExpression,
        )>,
        collected: HashMap<String, (TypeId, &'a MemberKind, &'a Location)>,
        current_key: Option<(&'a String, &'a MemberKind, &'a Location)>,
        class_type: TypeId,
    },
    IfElse {
        cond_type: Option<TypeId>,
        then_branch: &'a LocatedExpression,
        then_type: Option<TypeId>,
        else_branch: Option<&'a LocatedExpression>,
    },
    Binary {
        op: &'a BinaryOp,
        right: &'a LocatedExpression,
    },
    BinaryCombine {
        op: &'a BinaryOp,
        left: TypeId,
    },
    Assign {
        op: &'a Option<BinaryOp>,
        target: &'a LocatedExpression,
    },
    FieldAssign {
        value: TypeId,
        op: &'a Option<BinaryOp>,
        field: &'a String,
    },
    Define {
        name: &'a String,
        type_hint: Option<&'a TypeHint>,
    },
    Set {
        value: TypeId,
    },
    FieldAccess {
        field: String,
    },
    ArrayIndex {
        index: &'a LocatedExpression,
        array: Option<TypeId>,
    },
    Return(bool),
}

pub struct EnvFrame<'a> {
    pub frame: Frame<'a>,
    pub env: Rc<RefCell<Environment>>,
}

impl Default for ValueType {
    fn default() -> Self {
        ValueType::Null
    }
}

pub fn check_types(expr: &LocatedExpression, module_path: String) -> Vec<Located<TypeError>> {
    let env = Environment::new_global(module_path.clone());

    let mut stack: Vec<EnvFrame> = vec![Frame::eval(expr, &Rc::new(RefCell::new(env)))];
    let mut arena = TypeArena::new();
    let mut last: TypeId = arena.type_any();
    let mut errors = Vec::new();
    while let Some(frame) = stack.pop() {
        match frame.frame {
            Frame::Eval(expr) => {
                check_types_eval(
                    &mut arena,
                    &mut stack,
                    &mut last,
                    &mut errors,
                    &expr,
                    frame.env,
                );
            }
            Frame::Kont(kont) => {
                check_types_kont(
                    &mut arena,
                    &mut stack,
                    &mut last,
                    &mut errors,
                    kont.data,
                    kont.location,
                    frame.env,
                );
            }
        }
    }
    errors
}

fn check_types_eval<'a>(
    arena: &mut TypeArena,
    stack: &mut Vec<EnvFrame<'a>>,
    last: &mut TypeId,
    errors: &mut Vec<Located<TypeError>>,
    expr: &'a LocatedExpression,
    env: Rc<RefCell<Environment>>,
) {
    let location = &expr.location;
    match &expr.data {
        Expression::Number(_) => {
            *last = arena.type_number();
        }
        Expression::Integer(_) => {
            *last = arena.type_integer();
        }
        Expression::Bool(_) => {
            *last = arena.type_bool();
        }
        Expression::String(_) => {
            *last = arena.type_string();
        }
        Expression::Null => {
            *last = arena.type_null();
        }
        Expression::Continue | Expression::Break => {
            if !env.borrow().in_loop {
                errors.push(Located::new(
                    TypeError::ContinueOrBreakOutsideLoop,
                    location.clone(),
                ));
            }
            *last = arena.type_loop_control_flow();
        }
        Expression::Return(ret_expr) => {
            stack.push(Frame::kont(
                Kont::Return(ret_expr.is_some()),
                &env,
                location.clone(),
            ));
            if let Some(ret_expr) = ret_expr {
                stack.push(Frame::eval(ret_expr, &env));
            }
        }
        Expression::Ident(name) => {
            let Some(var_type) = env.borrow().get(name) else {
                errors.push(Located::new(
                    TypeError::UndefinedVariable(name.clone()),
                    location.clone(),
                ));
                *last = arena.type_any();
                return;
            };
            *last = var_type;
        }
        Expression::ObjectLiteral(properties) => {
            if properties.is_empty() {
                *last = arena.empty_object();
            } else {
                let mut remaining: Vec<(&String, &LocatedExpression)> =
                    properties.iter().rev().map(|(k, v)| (k, v)).collect();
                let (first_key, first_expr) = remaining.pop().unwrap();
                stack.push(Frame::kont(
                    Kont::ObjectLiteral {
                        properties: remaining,
                        collected: HashMap::new(),
                        current_key: first_key.clone(),
                    },
                    &env,
                    location.clone(),
                ));
                stack.push(Frame::eval(first_expr, &env));
            }
        }
        Expression::ArrayLiteral(elements) => {
            if elements.is_empty() {
                *last = arena.any_array();
            } else {
                let mut remaining: Vec<&LocatedExpression> = elements.iter().rev().collect();
                let first_elem = remaining.pop().unwrap();
                stack.push(Frame::kont(
                    Kont::ArrayLiteral {
                        elements: remaining,
                        collected: Vec::new(),
                    },
                    &env,
                    location.clone(),
                ));
                stack.push(Frame::eval(first_elem, &env));
            }
        }
        Expression::Block(exprs, last_expr) => {
            if exprs.is_empty() && last_expr.is_none() {
                *last = arena.type_null();
            } else {
                let mut remaining: Vec<&LocatedExpression> = exprs
                    .iter()
                    .chain(last_expr.as_deref().into_iter())
                    .rev()
                    .collect();
                let block_env = Rc::new(RefCell::new(Environment::new(env)));
                let first_expr = remaining.pop().unwrap();

                stack.push(Frame::kont(
                    Kont::Block {
                        remaining_exprs: remaining,
                        has_last: last_expr.is_some(),
                    },
                    &block_env,
                    location.clone(),
                ));
                stack.push(Frame::eval(first_expr, &block_env));
            }
        }
        Expression::FunctionLiteral {
            parameters,
            return_type,
            body,
        } => {
            let function_type = parse_function_type(
                arena,
                parameters,
                return_type,
                env.clone(),
                errors,
                location,
            );
            let func_env = Rc::new(RefCell::new(Environment::new_function(
                env.clone(),
                function_type.return_type,
            )));
            let mut func_env_mut = func_env.borrow_mut();
            for ((param_name, _), param_type) in
                parameters.iter().zip(function_type.parameters.iter())
            {
                func_env_mut.define(param_name, *param_type);
            }
            drop(func_env_mut);
            let function_type = arena.new_type(ValueType::Function(function_type));
            stack.push(Frame::kont(
                Kont::FunctionLiteral { function_type },
                &func_env,
                location.clone(),
            ));
            stack.push(Frame::eval(body, &func_env));
        }
        Expression::ClassLiteral { .. } => {
            unreachable!("ClassLiteral should be handled in Define expression");
        }
        Expression::BinaryOp { op, left, right } => {
            stack.push(Frame::kont(
                Kont::Binary { op, right },
                &env,
                location.clone(),
            ));
            stack.push(Frame::eval(left, &env));
        }
        Expression::Assign { target, value, op } => {
            stack.push(Frame::kont(
                Kont::Assign { op, target },
                &env,
                location.clone(),
            ));
            stack.push(Frame::eval(value, &env));
        }
        Expression::Define {
            name,
            value,
            type_hint,
        } => {
            // insert stub for recursive definitions
            match &value.data {
                Expression::FunctionLiteral {
                    parameters,
                    return_type,
                    body: _,
                } => {
                    let function_type = parse_function_type(
                        arena,
                        parameters,
                        return_type,
                        env.clone(),
                        errors,
                        location,
                    );
                    let function_type = arena.new_type(ValueType::Function(function_type));
                    env.borrow_mut().define(name, function_type);
                    stack.push(Frame::kont(
                        Kont::Set {
                            value: arena.type_null(),
                        },
                        &env,
                        location.clone(),
                    ));
                    stack.push(Frame::eval(value, &env));
                    return;
                }
                Expression::ClassLiteral { properties, parent } => {
                    let class_type = arena.new_type(ValueType::Placeholder);
                    let instance_type = arena.new_type(ValueType::Placeholder);
                    env.borrow_mut().define(name, class_type);
                    let properties: Vec<(
                        &String,
                        &MemberKind,
                        &Option<TypeHint>,
                        &LocatedExpression,
                    )> = properties
                        .iter()
                        .rev()
                        .map(|(name, kind, hint, expr)| (name, kind, hint, expr))
                        .collect();
                    let class_env = Rc::new(RefCell::new(Environment::new_class(
                        env.clone(),
                        class_type,
                        instance_type,
                    )));
                    stack.push(Frame::kont(
                        Kont::ClassLiteralFunctionBodies {
                            properties: properties.clone(),
                            class_type,
                            collected: HashMap::new(),
                            current_key: None,
                        },
                        &class_env,
                        location.clone(),
                    ));
                    stack.push(Frame::kont(
                        Kont::ClassLiteralShell {
                            class_type,
                            instance_type,
                            parent: parent.as_deref(),
                            properties,
                        },
                        &class_env,
                        location.clone(),
                    ));
                    if let Some(parent) = parent {
                        stack.push(Frame::eval(parent, &class_env));
                    }
                    return;
                }
                _ => {}
            };
            stack.push(Frame::kont(
                Kont::Define {
                    name,
                    type_hint: type_hint.as_ref(),
                },
                &env,
                location.clone(),
            ));
            stack.push(Frame::eval(value, &env));
        }
        Expression::FieldAccess { object, field } => {
            stack.push(Frame::kont(
                Kont::FieldAccess {
                    field: field.clone(),
                },
                &env,
                location.clone(),
            ));
            stack.push(Frame::eval(object, &env));
        }
        Expression::ArrayIndex { array, index } => {
            stack.push(Frame::kont(
                Kont::ArrayIndex { index, array: None },
                &env,
                location.clone(),
            ));
            stack.push(Frame::eval(array, &env));
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => {
            stack.push(Frame::kont(
                Kont::IfElse {
                    then_branch,
                    else_branch: else_branch.as_deref(),
                    then_type: None,
                    cond_type: None,
                },
                &env,
                location.clone(),
            ));
            stack.push(Frame::eval(condition, &env));
        }
        _ => *last = arena.type_any(),
    }
}

fn maybe_take_last(arena: &mut TypeArena, last: &mut TypeId) -> Option<TypeId> {
    Some(std::mem::replace(last, arena.type_null()))
}

fn check_types_kont<'a>(
    arena: &mut TypeArena,
    stack: &mut Vec<EnvFrame<'a>>,
    last: &mut TypeId,
    errors: &mut Vec<Located<TypeError>>,
    kont: Kont<'a>,
    location: Location,
    env: Rc<RefCell<Environment>>,
) {
    match kont {
        Kont::Return(has_ret_type) => {
            let val = if has_ret_type {
                let Some(val) = maybe_take_last(arena, last) else {
                    return;
                };
                val
            } else {
                arena.type_null()
            };
            let env_ref = env.borrow();
            let Some(ret_type) = env_ref.return_type.clone() else {
                errors.push(Located::new(
                    TypeError::ReturnOutsideFunction,
                    location.clone(),
                ));
                *last = arena.type_return();
                return;
            };
            if !check_is_assignable(arena, ret_type, val) {
                errors.push(Located::new(
                    TypeError::Mismatch {
                        expected: arena.get(ret_type),
                        found: arena.get(val),
                    },
                    location.clone(),
                ));
            }
            *last = arena.type_return();
        }
        Kont::ObjectLiteral {
            mut properties,
            mut collected,
            current_key,
        } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            collected.insert(current_key, val);
            if let Some((next_key, next_expr)) = properties.pop() {
                stack.push(Frame::kont(
                    Kont::ObjectLiteral {
                        properties,
                        collected,
                        current_key: next_key.clone(),
                    },
                    &env,
                    location,
                ));
                stack.push(Frame::eval(next_expr, &env));
            } else {
                *last = arena.new_type(ValueType::Object(collected));
            }
        }
        Kont::ArrayLiteral {
            mut elements,
            mut collected,
        } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            collected.push(val);
            if let Some(next_elem) = elements.pop() {
                stack.push(Frame::kont(
                    Kont::ArrayLiteral {
                        elements,
                        collected,
                    },
                    &env,
                    location,
                ));
                stack.push(Frame::eval(next_elem, &env));
            } else {
                let union = ValueType::union(arena, collected);
                let union = arena.new_type(union);
                *last = arena.new_type(ValueType::Array(union));
            }
        }
        Kont::FunctionLiteral { function_type } => {
            let body = std::mem::take(last);
            let ValueType::Function(f_type) = arena.get(function_type) else {
                unreachable!()
            };

            if !matches!(arena.get(body), ValueType::Return)
                && !check_is_assignable(arena, f_type.return_type, body)
            {
                errors.push(Located::new(
                    TypeError::Mismatch {
                        expected: arena.get(f_type.return_type),
                        found: arena.get(body),
                    },
                    location.clone(),
                ));
            }
            *last = function_type;
        }
        Kont::ClassLiteralShell {
            parent,
            properties,
            class_type,
            instance_type,
        } => {
            let parent = if let Some(p) = parent {
                let Some(val) = maybe_take_last(arena, last) else {
                    return;
                };
                let parent = arena.get(val);
                let ValueType::Class(class) = parent else {
                    if !matches!(parent, ValueType::Any) {
                        errors.push(Located::new(
                            TypeError::NotAClass(parent),
                            p.location.clone(),
                        ));
                    }
                    arena.replace(class_type, ValueType::Any);
                    arena.replace(instance_type, ValueType::Any);
                    *last = arena.type_null();
                    return;
                };
                Some(class)
            } else {
                None
            };

            let collected = properties
                .into_iter()
                .map(|(next_key, next_kind, next_type_hint, next_expr)| {
                    let type_id = match &next_expr.data {
                        Expression::FunctionLiteral {
                            parameters,
                            return_type,
                            ..
                        } => {
                            let function_type = parse_function_type(
                                arena,
                                parameters,
                                return_type,
                                env.clone(),
                                errors,
                                &location,
                            );
                            arena.new_type(ValueType::Function(function_type))
                        }
                        _ => {
                            if let Some(type_hint) = next_type_hint {
                                match ValueType::from_type_hint(arena, type_hint, env.clone()) {
                                    Ok(t) => t,
                                    Err(err) => {
                                        errors.push(Located::new(err, location.clone()));
                                        arena.type_any()
                                    }
                                }
                            } else {
                                arena.type_any()
                            }
                        }
                    };
                    (next_key.clone(), (type_id, next_kind))
                })
                .collect();

            let mut class = ClassType {
                instance_fields: HashMap::new(),
                constructor: None,
                static_fields: HashMap::new(),
                methods: HashMap::new(),
                static_methods: HashMap::new(),
                parent: parent.map(Box::new),
            };

            map_prototype_functions(collected, &mut class);

            arena.replace(
                instance_type,
                ValueType::ClassInstance(class.get_class_instance_type()),
            );
            arena.replace(class_type, ValueType::Class(class));
            *last = arena.type_null();
        }
        Kont::ClassLiteralFunctionBodies {
            mut properties,
            mut collected,
            class_type,
            current_key,
        } => {
            if let Some((current_key, current_kind, current_location)) = current_key {
                let Some(val) = maybe_take_last(arena, last) else {
                    return;
                };
                collected.insert(current_key.clone(), (val, current_kind, current_location));
            }
            if let Some((next_key, next_kind, _, next_expr)) = properties.pop() {
                stack.push(Frame::kont(
                    Kont::ClassLiteralFunctionBodies {
                        properties,
                        collected,
                        current_key: Some((next_key, next_kind, &next_expr.location)),
                        class_type,
                    },
                    &env,
                    next_expr.location.clone(),
                ));
                stack.push(Frame::eval(next_expr, &env));
                return;
            }
            let class_type = arena.get(class_type);
            if let ValueType::Any = class_type {
                *last = arena.type_null();
                return;
            }
            let ValueType::Class(class) = class_type else {
                unreachable!()
            };
            for (key, (v, kind, location)) in collected.into_iter() {
                let Some(expected) = (match kind {
                    MemberKind::Field => class.instance_fields.get(&key),
                    MemberKind::StaticField => class.static_fields.get(&key),
                    MemberKind::Method => class.methods.get(&key),
                    MemberKind::StaticMethod => class.static_methods.get(&key),
                    MemberKind::Constructor => class.constructor.as_ref(),
                }) else {
                    unreachable!()
                };
                if !check_is_assignable(arena, *expected, v) {
                    errors.push(Located::new(
                        TypeError::Mismatch {
                            expected: arena.get(*expected),
                            found: arena.get(v),
                        },
                        location.clone(),
                    ));
                    continue;
                }
            }
            *last = arena.type_null();
        }
        Kont::IfElse {
            then_branch,
            else_branch,
            then_type,
            cond_type,
        } => {
            if cond_type.is_none() {
                let Some(cond) = maybe_take_last(arena, last) else {
                    return;
                };
                let cond_type = arena.get(cond);
                match cond_type {
                    ValueType::Bool | ValueType::Any => {}
                    _ => {
                        errors.push(Located::new(
                            TypeError::Mismatch {
                                expected: ValueType::Bool,
                                found: cond_type,
                            },
                            location.clone(),
                        ));
                        *last = arena.type_any();
                        return;
                    }
                }
                stack.push(Frame::kont(
                    Kont::IfElse {
                        then_branch,
                        else_branch,
                        then_type,
                        cond_type: Some(cond),
                    },
                    &env,
                    location.clone(),
                ));
                stack.push(Frame::eval(then_branch, &env));
                return;
            }
            let then_type = if let Some(then_type) = then_type {
                then_type
            } else {
                let then_type = std::mem::take(last);
                if let Some(else_branch) = else_branch {
                    stack.push(Frame::kont(
                        Kont::IfElse {
                            then_branch,
                            else_branch: None,
                            then_type: Some(then_type),
                            cond_type,
                        },
                        &env,
                        location.clone(),
                    ));
                    stack.push(Frame::eval(else_branch, &env));
                    return;
                }
                then_type
            };
            let else_type = if else_branch.is_some() {
                std::mem::take(last)
            } else {
                arena.type_null()
            };
            let res = ValueType::union(arena, vec![then_type, else_type]);
            *last = arena.new_type(res);
        }
        Kont::Block {
            mut remaining_exprs,
            has_last,
        } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            if let Some(next_expr) = remaining_exprs.pop() {
                stack.push(Frame::kont(
                    Kont::Block {
                        remaining_exprs,
                        has_last,
                    },
                    &env,
                    location,
                ));
                stack.push(Frame::eval(next_expr, &env));
                return;
            }
            *last = if has_last { val } else { arena.type_null() };
        }
        Kont::Set { value } => {
            *last = value;
        }
        Kont::Define { name, type_hint } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            let val_type = if let Some(type_hint) = type_hint {
                let expected_type = match ValueType::from_type_hint(arena, type_hint, env.clone()) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(Located::new(err, location.clone()));
                        *last = arena.type_any();
                        return;
                    }
                };
                if !check_is_assignable(arena, expected_type, val) {
                    errors.push(Located::new(
                        TypeError::Mismatch {
                            expected: arena.get(expected_type),
                            found: arena.get(val),
                        },
                        location.clone(),
                    ));
                    *last = arena.type_any();
                    return;
                }
                expected_type
            } else {
                val
            };
            if !env.borrow().can_define(name) {
                errors.push(Located::new(
                    TypeError::Redefinition(name.clone()),
                    location.clone(),
                ));
                *last = arena.type_any();
                return;
            }
            env.borrow_mut().define(name, val_type);
            *last = arena.type_null();
        }
        Kont::Binary { op, right } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            stack.push(Frame::kont(
                Kont::BinaryCombine { op, left: val },
                &env,
                location,
            ));
            stack.push(Frame::eval(right, &env));
        }
        Kont::BinaryCombine { op, left } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            match check_simple_binary_op(arena, op, left, val) {
                Ok(result) => {
                    *last = result;
                }
                Err(err) => {
                    errors.push(Located::new(err, location));
                    *last = arena.type_any();
                }
            }
        }
        Kont::Assign { op, target } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            match &target.data {
                Expression::Ident(name) => {
                    let Some(current_type) = env.borrow().get(name) else {
                        errors.push(Located::new(
                            TypeError::UndefinedVariable(name.clone()),
                            location.clone(),
                        ));
                        *last = arena.type_any();
                        return;
                    };
                    let var_type = if let Some(op) = op {
                        match check_simple_binary_op(arena, op, current_type.clone(), val) {
                            Ok(t) => t,
                            Err(err) => {
                                errors.push(Located::new(err, location.clone()));
                                arena.type_any()
                            }
                        }
                    } else {
                        val
                    };
                    if !check_is_assignable(arena, current_type, var_type) {
                        errors.push(Located::new(
                            TypeError::Mismatch {
                                expected: arena.get(current_type),
                                found: arena.get(var_type),
                            },
                            location.clone(),
                        ));
                        *last = arena.type_any();
                        return;
                    }
                    *last = var_type;
                }
                Expression::FieldAccess { object, field } => {
                    stack.push(Frame::kont(
                        Kont::FieldAssign {
                            value: val,
                            op,
                            field,
                        },
                        &env,
                        location.clone(),
                    ));
                    stack.push(Frame::eval(object, &env));
                }
                Expression::ArrayIndex { .. } => {} //todo!()
                _ => {
                    errors.push(Located::new(
                        TypeError::InvalidAssignmentTarget,
                        location.clone(),
                    ));
                    *last = arena.type_any();
                }
            }
        }
        Kont::FieldAssign { value, op, field } => {
            let Some(object) = maybe_take_last(arena, last) else {
                return;
            };
            let object_type = arena.get(object);
            if let ValueType::Any = object_type {
                *last = arena.type_any();
                return;
            }
            let Some((field_type, _)) = get_property(arena, object, field) else {
                errors.push(Located::new(
                    TypeError::FieldNotFound(field.clone()),
                    location,
                ));
                *last = arena.type_any();
                return;
            };
            let res = if let Some(op) = op {
                match check_simple_binary_op(arena, op, field_type, value) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(Located::new(err, location.clone()));
                        arena.type_any()
                    }
                }
            } else {
                value
            };
            if !check_is_assignable(arena, field_type, res) {
                errors.push(Located::new(
                    TypeError::Mismatch {
                        expected: arena.get(field_type),
                        found: arena.get(res),
                    },
                    location,
                ));
            }
            *last = field_type;
        }
        Kont::FieldAccess { field } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            if let ValueType::Any = arena.get(val) {
                *last = arena.type_any();
                return;
            }
            match get_property(arena, val, &field) {
                Some((field_type, _)) => {
                    *last = field_type;
                }
                None => {
                    errors.push(Located::new(TypeError::FieldNotFound(field), location));
                    *last = arena.type_any();
                }
            }
        }
        Kont::ArrayIndex { index, array } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            let Some(array) = array else {
                stack.push(Frame::kont(
                    Kont::ArrayIndex {
                        index,
                        array: Some(val),
                    },
                    &env,
                    location.clone(),
                ));
                stack.push(Frame::eval(index, &env));
                return;
            };
            match get_index_type(arena, array, val) {
                Ok(index_type) => {
                    *last = index_type;
                }
                Err(err) => {
                    errors.push(Located::new(err, location));
                    *last = arena.type_any();
                }
            }
        }
    }
}

fn check_simple_binary_op(
    arena: &mut TypeArena,
    op: &BinaryOp,
    left_id: TypeId,
    right_id: TypeId,
) -> Result<TypeId, TypeError> {
    let Some(left) = ValueType::filter_loop_control_flow(arena, left_id) else {
        return Ok(arena.type_loop_control_flow());
    };
    let Some(right) = ValueType::filter_loop_control_flow(arena, right_id) else {
        return Ok(arena.type_loop_control_flow());
    };
    let left = arena.get(left);
    let right = arena.get(right);
    if let ValueType::Any = left {
        return Ok(left_id);
    }
    if let ValueType::Any = right {
        return Ok(right_id);
    }
    if let ValueType::Union(left) = left {
        let mapped = left
            .into_iter()
            .map(|t| check_simple_binary_op(arena, op, t, right_id))
            .collect::<Result<Vec<_>, _>>()?;
        let union = ValueType::union(arena, mapped);
        return Ok(arena.new_type(union));
    }
    Ok(match op {
        BinaryOp::Add
        | BinaryOp::Subtract
        | BinaryOp::Divide
        | BinaryOp::Multiply
        | BinaryOp::Modulo => match (&left, &right) {
            (ValueType::Number, ValueType::Number) => arena.type_number(),
            (ValueType::Number, ValueType::Integer) => arena.type_number(),
            (ValueType::Integer, ValueType::Number) => arena.type_number(),
            (ValueType::Integer, ValueType::Integer) => arena.type_integer(),
            _ => {
                return Err(TypeError::UnsupportedBinaryOp {
                    op: op.clone(),
                    left,
                    right,
                });
            }
        },
        BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
            match (&left, &right) {
                (ValueType::Number, ValueType::Number) => arena.type_bool(),
                (ValueType::Number, ValueType::Integer) => arena.type_bool(),
                (ValueType::Integer, ValueType::Number) => arena.type_bool(),
                (ValueType::Integer, ValueType::Integer) => arena.type_bool(),
                _ => {
                    return Err(TypeError::UnsupportedBinaryOp {
                        op: op.clone(),
                        left,
                        right,
                    });
                }
            }
        }
        BinaryOp::Equal | BinaryOp::NotEqual => arena.type_bool(),
        _ => {
            return Err(TypeError::UnsupportedBinaryOp {
                op: op.clone(),
                left,
                right,
            });
        }
    })
}

fn check_is_assignable(arena: &mut TypeArena, to_id: TypeId, from_id: TypeId) -> bool {
    let Some(from_id) = ValueType::filter_loop_control_flow(arena, from_id) else {
        return true;
    };
    let Some(to_id) = ValueType::filter_loop_control_flow(arena, to_id) else {
        return true;
    };
    if to_id == from_id {
        return true;
    }
    let from = arena.get(from_id);
    let to = arena.get(to_id);
    if let ValueType::Any = from {
        return true;
    }
    if let ValueType::Any = to {
        return true;
    }
    if to == from {
        return true;
    }
    match to {
        ValueType::Union(types) => types
            .into_iter()
            .any(|t| check_is_assignable(arena, t, from_id)),
        ValueType::Object(to_fields) => {
            if let ValueType::Object(from_fields) = from {
                for (key, to_field_type) in to_fields {
                    let Some(from_field_type) = from_fields.get(&key) else {
                        return false;
                    };
                    if !check_is_assignable(arena, to_field_type, *from_field_type) {
                        return false;
                    }
                }
                true
            } else {
                false
            }
        }
        ValueType::Array(to_elem_type) => {
            if let ValueType::Array(from_elem_type) = from {
                check_is_assignable(arena, to_elem_type, from_elem_type)
            } else {
                false
            }
        }
        _ => match from {
            ValueType::Union(types) => types
                .into_iter()
                .all(|t| check_is_assignable(arena, to_id, t)),
            _ => false,
        },
    }
}

fn get_index_type(
    arena: &mut TypeArena,
    array: TypeId,
    index: TypeId,
) -> Result<TypeId, TypeError> {
    let array = arena.get(array);
    let index = arena.get(index);
    if let ValueType::Any = array {
        return Ok(arena.type_any());
    }
    match array {
        ValueType::Array(element_type) => {
            if let ValueType::Any = index {
                return Ok(arena.type_any());
            }
            if let ValueType::Integer = index {
                Ok(element_type)
            } else {
                Err(TypeError::Mismatch {
                    expected: ValueType::Integer,
                    found: index.clone(),
                })
            }
        }
        _ => Err(TypeError::NotIndexable),
    }
}

fn map_prototype_functions(map: HashMap<String, (TypeId, &MemberKind)>, class: &mut ClassType) {
    for (key, (v, kind)) in map.into_iter() {
        match kind {
            MemberKind::Field => {
                class.instance_fields.insert(key, v);
            }
            MemberKind::StaticField => {
                class.static_fields.insert(key, v);
            }
            MemberKind::Method => {
                class.methods.insert(key, v);
            }
            MemberKind::StaticMethod => {
                class.static_methods.insert(key, v);
            }
            MemberKind::Constructor => {
                class.constructor = Some(v);
            }
        }
    }
}

fn parse_function_type(
    arena: &mut TypeArena,
    parameters: &Vec<(String, Option<TypeHint>)>,
    return_type: &Option<TypeHint>,
    env: Rc<RefCell<Environment>>,
    errors: &mut Vec<Located<TypeError>>,
    location: &Location,
) -> FunctionType {
    let mut param_types = Vec::new();
    for (_, type_hint) in parameters {
        let param_type = match type_hint {
            Some(hint) => match ValueType::from_type_hint(arena, hint, env.clone()) {
                Ok(t) => t,
                Err(err) => {
                    errors.push(Located::new(err, location.clone()));
                    arena.type_any()
                }
            },
            None => arena.type_any(),
        };
        param_types.push(param_type);
    }
    let return_type = if let Some(type_hint) = return_type {
        match ValueType::from_type_hint(arena, type_hint, env.clone()) {
            Ok(t) => t,
            Err(err) => {
                errors.push(Located::new(err, location.clone()));
                arena.type_any()
            }
        }
    } else {
        arena.type_any()
    };
    FunctionType {
        parameters: param_types,
        return_type,
    }
}

fn get_class(target: &ValueType) -> &ClassType {
    if let ValueType::ClassInstance(instance) = target {
        &instance.class
    } else if let ValueType::Class(class) = target {
        class
    } else {
        todo!();
    }
}

fn get_property(
    arena: &mut TypeArena,
    target_id: usize,
    field: &str,
) -> Option<(usize, MemberKind)> {
    let target = arena.get(target_id);
    match &target {
        ValueType::Object(obj) => {
            if let Some(val) = obj.get(field) {
                return Some((*val, MemberKind::Field));
            }
        }
        ValueType::ClassInstance(obj) => {
            if let Some(val) = obj.properties.get(field) {
                return Some((*val, MemberKind::Field));
            }
        }
        _ => {}
    }

    let class = get_class(&target);

    fn look_for_property_in_class(
        mut class: &ClassType,
        field: &str,
    ) -> Option<(usize, MemberKind)> {
        loop {
            if let Some(val) = class.static_fields.get(field) {
                return Some((*val, MemberKind::StaticField));
            } else if let Some(val) = class.methods.get(field) {
                return Some((*val, MemberKind::Method));
            } else if let Some(val) = class.static_methods.get(field) {
                return Some((*val, MemberKind::StaticMethod));
            }

            if let Some(parent) = &class.parent {
                class = parent.as_ref();
            } else {
                return None;
            }
        }
    }
    if let Some(res) = look_for_property_in_class(&class, field) {
        Some(res)
    } else if let ValueType::ClassInstance(_) = target {
        //look_for_property_in_class(root.value_classes.class_instance, field)
        None
    } else if let ValueType::Class(_) = target {
        //look_for_property_in_class(root.value_classes.class, field)
        None
    } else {
        None
    }
}
