use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::parser::{BinaryOp, Expression, UnaryOp};

#[derive(Clone)]
pub enum Value {
    Number(f64),
    Integer(i64),
    Bool(bool),
    String(String),
    Null,
    Array(Rc<RefCell<Vec<Value>>>),
    Object(Rc<RefCell<HashMap<String, Value>>>),
    Function {
        parameters: Vec<String>,
        body: Box<Expression>,
        env: Rc<Environment>,
    },
    BuiltinFunction {
        func: fn(Vec<Value>) -> Result<Value, RuntimeError>,
    },
}

#[derive(Clone)]
pub struct Environment {
    values: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new(parent: Option<Rc<Environment>>) -> Self {
        Self {
            values: Rc::new(RefCell::new(HashMap::new())),
            parent,
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.values.borrow().get(name) {
            Some(val.clone())
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn set(&self, name: &str, value: Value) -> bool {
        if self.values.borrow().contains_key(name) {
            self.values.borrow_mut().insert(name.to_string(), value);
            true
        } else if let Some(parent) = &self.parent {
            parent.set(name, value)
        } else {
            false
        }
    }

    pub fn define(&self, name: &str, value: Value) {
        self.values.borrow_mut().insert(name.to_string(), value);
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeError(String),
    UndefinedVariable(String),
    FieldAccessError(String),
    ArrayIndexOutOfBounds(usize),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TypeError(msg) => write!(f, "TypeError: {}", msg),
            Self::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            Self::FieldAccessError(msg) => write!(f, "Field does not exist: {}", msg),
            Self::ArrayIndexOutOfBounds(index) => {
                write!(f, "Array index out of bounds: {}", index)
            }
        }
    }
}

fn variable_to_string(var: &Value) -> String {
    match var {
        Value::Number(n) => n.to_string(),
        Value::Integer(i) => i.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::String(s) => s.clone(),
        Value::Null => "null".to_string(),
        Value::Object(_) => "<object>".to_string(),
        Value::Array(elements) => {
            let elements = elements
                .borrow()
                .iter()
                .map(|e| variable_to_string(e))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", elements)
        }
        Value::BuiltinFunction { .. } => "<builtin function>".to_string(),
        Value::Function { .. } => "<function>".to_string(),
    }
}

pub fn interpret_global(expr: &Expression) -> Result<Value, RuntimeError> {
    let global_env = Rc::new(Environment::new(None));
    global_env.define(
        "print",
        Value::BuiltinFunction {
            func: |args| {
                let str = args
                    .into_iter()
                    .map(|arg| variable_to_string(&arg))
                    .collect::<Vec<_>>()
                    .join(" ");
                println!("{}", str);
                Ok(Value::Null)
            },
        },
    );
    interpret(expr, global_env)
}

fn interpret(expr: &Expression, env: Rc<Environment>) -> Result<Value, RuntimeError> {
    match expr {
        Expression::Number(n) => Ok(Value::Number(*n)),
        Expression::Integer(i) => Ok(Value::Integer(*i)),
        Expression::Bool(b) => Ok(Value::Bool(*b)),
        Expression::Null => Ok(Value::Null),
        Expression::String(s) => Ok(Value::String(s.clone())),
        Expression::ArrayLiteral(elements) => {
            let mut vals = Vec::new();
            for elem in elements {
                let val = interpret(elem, env.clone())?;
                vals.push(val);
            }
            Ok(Value::Array(Rc::new(RefCell::new(vals))))
        }
        Expression::ObjectLiteral(pairs) => {
            let mut map = HashMap::new();
            for (key, value_expr) in pairs {
                let value = interpret(value_expr, env.clone())?;
                map.insert(key.clone(), value);
            }
            Ok(Value::Object(Rc::new(RefCell::new(map))))
        }
        Expression::Ident(name) => {
            if let Some(val) = env.get(name) {
                Ok(val)
            } else {
                Err(RuntimeError::UndefinedVariable(name.clone()))
            }
        }
        Expression::Define(name, value) => {
            let val = interpret(value, env.clone())?;
            env.define(name, val);
            Ok(Value::Null)
        }
        Expression::Block(exprs, ret) => {
            let inner_env = Rc::new(Environment::new(Some(env)));
            for e in exprs {
                interpret(e, inner_env.clone())?;
            }
            if let Some(r) = ret {
                interpret(r, inner_env.clone())
            } else {
                Ok(Value::Null)
            }
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let cond = interpret(condition, env.clone())?;
            match cond {
                Value::Bool(true) => interpret(then_branch, env.clone()),
                Value::Bool(false) => {
                    if let Some(else_b) = else_branch {
                        interpret(else_b, env.clone())
                    } else {
                        Ok(Value::Null)
                    }
                }
                _ => Err(RuntimeError::TypeError(
                    "Condition of if must be a boolean".to_string(),
                )),
            }
        }
        Expression::UnaryOp { op, expr } => {
            let val = interpret(expr, env.clone())?;
            match (op, val) {
                (UnaryOp::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
                (UnaryOp::Negate, Value::Integer(i)) => Ok(Value::Integer(-i)),
                (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                _ => Err(RuntimeError::TypeError(format!(
                    "Invalid operand type for unary operator {:?}",
                    op
                ))),
            }
        }
        Expression::BinaryOp { op, left, right } => interpret_binary_op(&env, op, left, right),
        Expression::FunctionCall {
            function,
            arguments,
        } => {
            let func_val = interpret(function, env.clone())?;
            match func_val {
                Value::Function {
                    parameters,
                    body,
                    env: func_env,
                } => {
                    if parameters.len() != arguments.len() {
                        return Err(RuntimeError::TypeError(format!(
                            "Expected {} arguments, got {}",
                            parameters.len(),
                            arguments.len()
                        )));
                    }
                    let call_env = Rc::new(Environment::new(Some(func_env)));
                    for (param, arg) in parameters.iter().zip(arguments.iter()) {
                        let arg_val = interpret(arg, env.clone())?;
                        call_env.define(param, arg_val);
                    }
                    interpret(&body, call_env)
                }
                Value::BuiltinFunction { func } => {
                    let mut arg_vals = Vec::new();
                    for arg in arguments {
                        let val = interpret(arg, env.clone())?;
                        arg_vals.push(val);
                    }
                    func(arg_vals)
                }
                _ => Err(RuntimeError::TypeError(
                    "Attempted to call a non-function value".to_string(),
                )),
            }
        }
        Expression::ArrayIndex { array, index } => {
            let arr_val = interpret(array, env.clone())?;
            let idx_val = interpret(index, env.clone())?;
            let Value::Array(elements) = arr_val else {
                return Err(RuntimeError::TypeError(
                    "Attempted to index a non-array value".to_string(),
                ));
            };
            let Value::Integer(i) = idx_val else {
                return Err(RuntimeError::TypeError(
                    "Array index must be an integer".to_string(),
                ));
            };
            let i = i as usize;
            let elements = elements.borrow();
            if i < elements.len() {
                Ok(elements[i].clone())
            } else {
                Err(RuntimeError::ArrayIndexOutOfBounds(i))
            }
        }
        Expression::FieldAccess { object, field } => {
            let obj_val = interpret(object, env.clone())?;
            match obj_val {
                Value::Object(object_map) => {
                    let map = object_map.borrow();
                    if let Some(val) = map.get(field) {
                        Ok(val.clone())
                    } else {
                        Err(RuntimeError::FieldAccessError(field.clone()))
                    }
                }
                _ => Err(RuntimeError::TypeError(
                    "Field access must be on an object".to_string(),
                )),
            }
        }
    }
}

fn interpret_binary_op(
    env: &Rc<Environment>,
    op: &BinaryOp,
    left: &Box<Expression>,
    right: &Box<Expression>,
) -> Result<Value, RuntimeError> {
    if let BinaryOp::Assign = op {
        match left.as_ref() {
            Expression::Ident(name) => {
                let val = interpret(right, env.clone())?;
                if env.set(name, val.clone()) {
                    Ok(val)
                } else {
                    Err(RuntimeError::UndefinedVariable(name.clone()))
                }
            }
            Expression::FieldAccess { object, field } => {
                let obj_val = interpret(object, env.clone())?;
                match obj_val {
                    Value::Object(object_map) => {
                        let val = interpret(right, env.clone())?;
                        let mut map = object_map.borrow_mut();
                        map.insert(field.clone(), val.clone());
                        Ok(val)
                    }
                    _ => Err(RuntimeError::TypeError(
                        "Field access must be on an object".to_string(),
                    )),
                }
            }
            Expression::ArrayIndex { array, index } => {
                let arr_val = interpret(array, env.clone())?;
                let idx_val = interpret(index, env.clone())?;
                let Value::Array(elements) = arr_val else {
                    return Err(RuntimeError::TypeError(
                        "Attempted to index a non-array value".to_string(),
                    ));
                };
                let Value::Integer(i) = idx_val else {
                    return Err(RuntimeError::TypeError(
                        "Array index must be an integer".to_string(),
                    ));
                };
                let i = i as usize;
                let mut elements = elements.borrow_mut();
                if i < elements.len() {
                    let val = interpret(right, env.clone())?;
                    elements[i] = val.clone();
                    Ok(val)
                } else {
                    Err(RuntimeError::ArrayIndexOutOfBounds(i))
                }
            }
            _ => Err(RuntimeError::TypeError(
                "Left side of assignment must be assignable".to_string(),
            )),
        }
    } else {
        let left = interpret(left, env.clone())?;
        match (op, &left) {
            (BinaryOp::And, Value::Bool(false)) => Ok(Value::Bool(false)),
            (BinaryOp::Or, Value::Bool(true)) => Ok(Value::Bool(true)),
            _ => {
                let right = interpret(right, env.clone())?;
                interpret_simple_binary_op(op, left, right)
            }
        }
    }
}

fn interpret_simple_binary_op(
    op: &BinaryOp,
    left: Value,
    right: Value,
) -> Result<Value, RuntimeError> {
    match (op, left, right) {
        (BinaryOp::Add, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
        (BinaryOp::Add, Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),

        (BinaryOp::Subtract, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
        (BinaryOp::Subtract, Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l - r)),

        (BinaryOp::Multiply, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
        (BinaryOp::Multiply, Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),

        (BinaryOp::Divide, Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
        (BinaryOp::Divide, Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l / r)),

        (BinaryOp::And, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
        (BinaryOp::Or, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),

        (BinaryOp::Equal, l, r) => Ok(Value::Bool(is_value_equal(&l, &r))),
        (BinaryOp::NotEqual, l, r) => Ok(Value::Bool(!is_value_equal(&l, &r))),

        (BinaryOp::Less, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
        (BinaryOp::Less, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l < r)),
        (BinaryOp::LessEqual, Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
        (BinaryOp::LessEqual, Value::Integer(l), Value::Integer(r)) => Ok(Value::Bool(l <= r)),

        _ => Err(RuntimeError::TypeError(format!(
            "Invalid operand types for binary operator {:?}",
            op
        ))),
    }
}

fn is_value_equal(l: &Value, r: &Value) -> bool {
    match (l, r) {
        (Value::Number(l), Value::Number(r)) => l == r,
        (Value::Integer(l), Value::Integer(r)) => l == r,
        (Value::Bool(l), Value::Bool(r)) => l == r,
        (Value::Null, Value::Null) => true,
        _ => false,
    }
}
