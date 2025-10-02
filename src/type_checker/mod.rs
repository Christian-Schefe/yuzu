mod value_type;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub use value_type::*;

use crate::{
    location::{Located, Location},
    parser::{BinaryOp, Expression, LocatedExpression, TypeHint},
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
        collected: HashMap<String, ValueType>,
        current_key: String,
    },
    ArrayLiteral {
        elements: Vec<&'a LocatedExpression>,
        collected: Vec<ValueType>,
    },
    Binary {
        op: &'a BinaryOp,
        right: &'a LocatedExpression,
    },
    BinaryCombine {
        op: &'a BinaryOp,
        left: ValueType,
    },
    Assign {
        op: &'a Option<BinaryOp>,
        target: &'a LocatedExpression,
    },
    Define(&'a String, Option<&'a TypeHint>),
    FieldAccess {
        field: String,
    },
    ArrayIndex {
        index: &'a LocatedExpression,
        array: Option<ValueType>,
    },
    Return,
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
    let mut last: ValueType = ValueType::Null;
    let mut errors = Vec::new();
    while let Some(frame) = stack.pop() {
        match frame.frame {
            Frame::Eval(expr) => {
                check_types_eval(&mut stack, &mut last, &mut errors, &expr, frame.env);
            }
            Frame::Kont(kont) => {
                check_types_kont(
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
    stack: &mut Vec<EnvFrame<'a>>,
    last: &mut ValueType,
    errors: &mut Vec<Located<TypeError>>,
    expr: &'a LocatedExpression,
    env: Rc<RefCell<Environment>>,
) {
    let location = &expr.location;
    match &expr.data {
        Expression::Number(_) => {
            *last = ValueType::Number;
        }
        Expression::Integer(_) => {
            *last = ValueType::Integer;
        }
        Expression::Bool(_) => {
            *last = ValueType::Bool;
        }
        Expression::String(_) => {
            *last = ValueType::String;
        }
        Expression::Null => {
            *last = ValueType::Null;
        }
        Expression::Continue => {
            *last = ValueType::Continue;
        }
        Expression::Break => {
            *last = ValueType::Break;
        }
        Expression::Return(ret_expr) => {
            if let Some(ret_expr) = ret_expr {
                stack.push(Frame::kont(Kont::Return, &env, location.clone()));
                stack.push(Frame::eval(ret_expr, &env));
            } else {
                *last = ValueType::Return(Box::new(ValueType::Null));
            }
        }
        Expression::Ident(name) => {
            let Some(var_type) = env.borrow().get(name) else {
                errors.push(Located::new(
                    TypeError::UndefinedVariable(name.clone()),
                    location.clone(),
                ));
                *last = ValueType::Any;
                return;
            };
            *last = var_type;
        }
        Expression::ObjectLiteral(properties) => {
            if properties.is_empty() {
                *last = ValueType::Object(HashMap::new());
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
                *last = ValueType::Array(None);
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
                *last = ValueType::Null;
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
            stack.push(Frame::kont(
                Kont::Define(name, type_hint.as_ref()),
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
        _ => *last = ValueType::Any, //todo!()
    }
}

fn maybe_take_last(last: &mut ValueType) -> Option<ValueType> {
    if let ValueType::Continue | ValueType::Break | ValueType::Return(_) = last {
        return None;
    }
    Some(std::mem::replace(last, ValueType::Null))
}

fn check_types_kont<'a>(
    stack: &mut Vec<EnvFrame<'a>>,
    last: &mut ValueType,
    errors: &mut Vec<Located<TypeError>>,
    kont: Kont<'a>,
    location: Location,
    env: Rc<RefCell<Environment>>,
) {
    match kont {
        Kont::Return => {
            let Some(val) = maybe_take_last(last) else {
                return;
            };
            *last = ValueType::Return(Box::new(val));
        }
        Kont::ObjectLiteral {
            mut properties,
            mut collected,
            current_key,
        } => {
            let Some(val) = maybe_take_last(last) else {
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
                *last = ValueType::Object(collected);
            }
        }
        Kont::ArrayLiteral {
            mut elements,
            mut collected,
        } => {
            let Some(val) = maybe_take_last(last) else {
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
                *last = ValueType::Array(Some(Box::new(ValueType::union(collected))));
            }
        }
        Kont::Block {
            mut remaining_exprs,
            has_last,
        } => {
            let Some(val) = maybe_take_last(last) else {
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
            *last = if has_last { val } else { ValueType::Null }
        }
        Kont::Define(name, type_hint) => {
            let Some(val) = maybe_take_last(last) else {
                return;
            };
            let val_type = if let Some(type_hint) = type_hint {
                let expected_type = match ValueType::from_type_hint(type_hint, env.clone()) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(Located::new(err, location.clone()));
                        *last = ValueType::Any;
                        return;
                    }
                };
                if !check_is_assignable(&expected_type, &val) {
                    errors.push(Located::new(
                        TypeError::Mismatch {
                            expected: expected_type,
                            found: val.clone(),
                        },
                        location.clone(),
                    ));
                    *last = ValueType::Any;
                    return;
                }
                expected_type
            } else {
                val
            };
            if !env.borrow_mut().define(name, val_type) {
                errors.push(Located::new(
                    TypeError::Redefinition(name.clone()),
                    location.clone(),
                ));
                *last = ValueType::Any;
                return;
            }
            *last = ValueType::Null;
        }
        Kont::Binary { op, right } => {
            let Some(val) = maybe_take_last(last) else {
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
            let Some(val) = maybe_take_last(last) else {
                return;
            };
            match check_simple_binary_op(op, left, val) {
                Ok(result) => {
                    *last = result;
                }
                Err(err) => {
                    errors.push(Located::new(err, location));
                    *last = ValueType::Any;
                }
            }
        }
        Kont::Assign { op, target } => {
            let Some(val) = maybe_take_last(last) else {
                return;
            };
            match &target.data {
                Expression::Ident(name) => {
                    let Some(current_type) = env.borrow().get(name) else {
                        errors.push(Located::new(
                            TypeError::UndefinedVariable(name.clone()),
                            location.clone(),
                        ));
                        *last = ValueType::Any;
                        return;
                    };
                    let var_type = if let Some(op) = op {
                        match check_simple_binary_op(op, current_type.clone(), val) {
                            Ok(t) => t,
                            Err(err) => {
                                errors.push(Located::new(err, location.clone()));
                                ValueType::Any
                            }
                        }
                    } else {
                        val
                    };
                    if !check_is_assignable(&current_type, &var_type) {
                        errors.push(Located::new(
                            TypeError::Mismatch {
                                expected: current_type,
                                found: var_type.clone(),
                            },
                            location.clone(),
                        ));
                        *last = ValueType::Any;
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
                    *last = ValueType::Any;
                }
            }
        }
        Kont::FieldAccess { field } => {
            let Some(val) = maybe_take_last(last) else {
                return;
            };
            match get_field_type(&val, &field) {
                Some(field_type) => {
                    *last = field_type;
                }
                None => {
                    errors.push(Located::new(TypeError::FieldNotFound(field), location));
                    *last = ValueType::Any;
                }
            }
        }
        Kont::ArrayIndex { index, array } => {
            let Some(val) = maybe_take_last(last) else {
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
            match get_index_type(&array, &val) {
                Ok(index_type) => {
                    *last = index_type;
                }
                Err(err) => {
                    errors.push(Located::new(err, location));
                    *last = ValueType::Any;
                }
            }
        }
    }
}

fn check_simple_binary_op(
    op: &BinaryOp,
    left: ValueType,
    right: ValueType,
) -> Result<ValueType, TypeError> {
    if let ValueType::Any = left {
        return Ok(left);
    }
    if let ValueType::Any = right {
        return Ok(right);
    }
    Ok(match op {
        BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Divide | BinaryOp::Multiply => {
            match (&left, &right) {
                (ValueType::Number, ValueType::Number) => ValueType::Number,
                (ValueType::Number, ValueType::Integer) => ValueType::Number,
                (ValueType::Integer, ValueType::Number) => ValueType::Number,
                (ValueType::Integer, ValueType::Integer) => ValueType::Integer,
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
                (ValueType::Number, ValueType::Number) => ValueType::Bool,
                (ValueType::Number, ValueType::Integer) => ValueType::Bool,
                (ValueType::Integer, ValueType::Number) => ValueType::Bool,
                (ValueType::Integer, ValueType::Integer) => ValueType::Bool,
                _ => {
                    return Err(TypeError::UnsupportedBinaryOp {
                        op: op.clone(),
                        left,
                        right,
                    });
                }
            }
        }
        BinaryOp::Equal | BinaryOp::NotEqual => ValueType::Bool,
        _ => {
            return Err(TypeError::UnsupportedBinaryOp {
                op: op.clone(),
                left,
                right,
            });
        }
    })
}

fn check_is_assignable(to: &ValueType, from: &ValueType) -> bool {
    if let ValueType::Any = from {
        return true;
    }
    if let ValueType::Any = to {
        return true;
    }
    match to {
        ValueType::Union(types) => types.iter().any(|t| check_is_assignable(t, from)),
        ValueType::Object(to_fields) => {
            if let ValueType::Object(from_fields) = from {
                for (key, to_field_type) in to_fields {
                    let Some(from_field_type) = from_fields.get(key) else {
                        return false;
                    };
                    if !check_is_assignable(to_field_type, from_field_type) {
                        return false;
                    }
                }
                true
            } else {
                false
            }
        }
        _ => to == from,
    }
}

fn get_field_type(object: &ValueType, field: &str) -> Option<ValueType> {
    if let ValueType::Any = object {
        return Some(ValueType::Any);
    }
    match object {
        ValueType::Object(fields) => fields.get(field).cloned(),
        ValueType::ClassInstance(instance) => instance.properties.get(field).cloned(),
        _ => None,
    }
}

fn get_index_type(array: &ValueType, index: &ValueType) -> Result<ValueType, TypeError> {
    if let ValueType::Any = array {
        return Ok(ValueType::Any);
    }
    match array {
        ValueType::Array(element_type) => {
            if let ValueType::Integer = index {
                Ok(element_type.clone().map(|x| *x).unwrap_or(ValueType::Any))
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
