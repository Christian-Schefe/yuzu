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
        parameters: Vec<(&'a String, Option<&'a TypeHint>)>,
        return_type: Option<&'a TypeHint>,
    },
    ClassLiteral {
        has_parent: bool,
        parent: Option<ClassType>,
        properties: Vec<(&'a String, &'a MemberKind, &'a LocatedExpression)>,
        collected: HashMap<String, (TypeId, &'a MemberKind)>,
        current_key: Option<(&'a String, &'a MemberKind)>,
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
    Define {
        name: &'a String,
        type_hint: Option<&'a TypeHint>,
    },
    OverridePlaceholder {
        placeholder: TypeId,
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
            *last = arena.type_unreachable_loop_control_flow();
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
            let func_env = Rc::new(RefCell::new(Environment::new_function(
                env.clone(),
                arena.type_any(),
            )));
            for (param, type_hint) in parameters {
                let param_type = if let Some(type_hint) = type_hint {
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
                let mut func_env_mut = func_env.borrow_mut();
                if !func_env_mut.can_define(param) {
                    errors.push(Located::new(
                        TypeError::Redefinition(param.clone()),
                        location.clone(),
                    ));
                }
                func_env_mut.define(param, param_type);
            }
            let parameters = parameters
                .iter()
                .map(|(name, type_hint)| (name, type_hint.as_ref()))
                .rev()
                .collect();
            stack.push(Frame::kont(
                Kont::FunctionLiteral {
                    parameters,
                    return_type: return_type.as_ref(),
                },
                &func_env,
                location.clone(),
            ));
            stack.push(Frame::eval(body, &func_env));
        }
        Expression::ClassLiteral { parent, properties } => {
            let has_parent = parent.is_some();
            let properties = properties
                .iter()
                .rev()
                .map(|(name, kind, expr)| (name, kind, expr))
                .collect();
            stack.push(Frame::kont(
                Kont::ClassLiteral {
                    has_parent,
                    parent: None,
                    properties,
                    collected: HashMap::new(),
                    current_key: None,
                },
                &env,
                location.clone(),
            ));
            if let Some(parent) = parent {
                stack.push(Frame::eval(parent, &env));
            }
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
                        Kont::OverridePlaceholder {
                            placeholder: function_type,
                        },
                        &env,
                        location.clone(),
                    ));
                    stack.push(Frame::eval(value, &env));
                    return;
                }
                Expression::ClassLiteral { .. } => {
                    let class_type = arena.new_type(ValueType::Placeholder);
                    env.borrow_mut().define(name, class_type);
                    let class_env = Rc::new(RefCell::new(Environment::new_class(
                        env.clone(),
                        class_type,
                    )));
                    stack.push(Frame::kont(
                        Kont::OverridePlaceholder {
                            placeholder: class_type,
                        },
                        &class_env,
                        location.clone(),
                    ));
                    stack.push(Frame::eval(value, &class_env));
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
        _ => *last = arena.type_any(),
    }
}

fn maybe_take_last(arena: &mut TypeArena, last: &mut TypeId) -> Option<TypeId> {
    if let ValueType::Unreachable(_) = arena.get(*last) {
        return None;
    }
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
                *last = arena.type_unreachable_return();
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
            *last = arena.type_unreachable_return();
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
        Kont::FunctionLiteral {
            parameters,
            return_type,
        } => {
            let val = std::mem::take(last);
            let param_types = parameters
                .into_iter()
                .map(|(_, type_hint)| {
                    if let Some(type_hint) = type_hint {
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
                })
                .collect::<Vec<_>>();
            let ret_type = if let Some(type_hint) = return_type {
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
            if !matches!(
                arena.get(val),
                ValueType::Unreachable(UnreachableReason::Return)
            ) && !check_is_assignable(arena, ret_type, val)
            {
                errors.push(Located::new(
                    TypeError::Mismatch {
                        expected: arena.get(ret_type),
                        found: arena.get(val),
                    },
                    location.clone(),
                ));
            }
            *last = arena.new_type(ValueType::Function(FunctionType {
                parameters: param_types,
                return_type: Box::new(ret_type),
            }));
        }
        Kont::ClassLiteral {
            has_parent,
            parent,
            mut properties,
            mut collected,
            current_key,
        } => {
            if has_parent && parent.is_none() {
                let Some(val) = maybe_take_last(arena, last) else {
                    return;
                };
                let ValueType::Class(class_type) = arena.get(val) else {
                    errors.push(Located::new(TypeError::NotAClass, location.clone()));
                    *last = arena.type_any();
                    return;
                };
                stack.push(Frame::kont(
                    Kont::ClassLiteral {
                        has_parent,
                        parent: Some(class_type),
                        properties,
                        collected,
                        current_key,
                    },
                    &env,
                    location,
                ));
                return;
            }

            if let Some((current_key, current_kind)) = current_key {
                let Some(val) = maybe_take_last(arena, last) else {
                    return;
                };
                collected.insert(current_key.clone(), (val, current_kind));
            }

            if let Some((next_key, next_kind, next_expr)) = properties.pop() {
                stack.push(Frame::kont(
                    Kont::ClassLiteral {
                        has_parent,
                        parent,
                        properties,
                        collected,
                        current_key: Some((next_key, next_kind)),
                    },
                    &env,
                    location,
                ));
                stack.push(Frame::eval(next_expr, &env));
                return;
            }

            let mut class = ClassType {
                instance_fields: HashMap::new(),
                constructor: None,
                static_fields: HashMap::new(),
                methods: HashMap::new(),
                static_methods: HashMap::new(),
                parent: None,
            };

            map_prototype_functions(arena, collected, &mut class);

            *last = arena.new_type(ValueType::Class(class));
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
        Kont::OverridePlaceholder { placeholder } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };

            arena.replace(placeholder, arena.get(val));
            *last = arena.type_null();
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
                Expression::FieldAccess { object, field } => {} //todo!()
                Expression::ArrayIndex { array, index } => {}   //todo!()
                _ => {
                    errors.push(Located::new(
                        TypeError::InvalidAssignmentTarget,
                        location.clone(),
                    ));
                    *last = arena.type_any();
                }
            }
        }
        Kont::FieldAccess { field } => {
            let Some(val) = maybe_take_last(arena, last) else {
                return;
            };
            match get_field_type(arena, val, &field) {
                Some(field_type) => {
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
    let left = arena.get(left_id);
    let right = arena.get(right_id);
    if let ValueType::Any = left {
        return Ok(arena.type_any());
    }
    if let ValueType::Any = right {
        return Ok(arena.type_any());
    }
    Ok(match op {
        BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Divide | BinaryOp::Multiply => {
            match (&left, &right) {
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
            }
        }
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
        _ => to == from,
    }
}

fn get_field_type(arena: &mut TypeArena, object: TypeId, field: &str) -> Option<TypeId> {
    let object = arena.get(object);
    if let ValueType::Any = object {
        return Some(arena.type_any());
    }
    match object {
        ValueType::Object(fields) => fields.get(field).copied(),
        ValueType::ClassInstance(instance) => instance.properties.get(field).copied(),
        _ => None,
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

fn map_prototype_functions(
    arena: &mut TypeArena,
    map: HashMap<String, (TypeId, &MemberKind)>,
    class: &mut ClassType,
) {
    for (key, (v, kind)) in map.into_iter() {
        match kind {
            MemberKind::Field => {
                class.instance_fields.insert(key, v);
            }
            MemberKind::StaticField => {
                class.static_fields.insert(key, v);
            }
            MemberKind::Method => {
                let ValueType::Function(f) = arena.get(v) else {
                    //This shouldn't be possible because the parser only assigns functions as methods
                    eprintln!("Warning: Attempted to add non-function as method to prototype");
                    continue;
                };
                class.methods.insert(key.clone(), f);
            }
            MemberKind::StaticMethod => {
                let ValueType::Function(f) = arena.get(v) else {
                    //This shouldn't be possible because the parser only assigns functions as static methods
                    eprintln!(
                        "Warning: Attempted to add non-function as static method to prototype"
                    );
                    continue;
                };
                class.static_methods.insert(key.clone(), f);
            }
            MemberKind::Constructor => {
                let ValueType::Function(f) = arena.get(v) else {
                    //This shouldn't be possible because the parser only assigns functions as constructors
                    eprintln!("Warning: Attempted to add non-function as constructor to prototype");
                    continue;
                };
                class.constructor = Some(f);
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
        return_type: Box::new(return_type),
    }
}
