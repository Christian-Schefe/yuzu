use std::fmt;

use crate::parser::{BinaryOp, ClassMemberType, UnaryOp};

#[derive(Debug, Clone)]
pub enum Expression<T> {
    Number(f64),
    Integer(i64),
    Bool(bool),
    String(String),
    ArrayLiteral(Vec<Extra<T>>),
    FunctionLiteral {
        parameters: Vec<String>,
        body: Box<Extra<T>>,
    },
    PrototypeLiteral {
        superclass: Option<Box<Extra<T>>>,
        properties: Vec<(String, ClassMemberType, Extra<T>)>,
    },
    ObjectLiteral(Vec<(String, Extra<T>)>),
    Null,

    BinaryOp {
        op: BinaryOp,
        left: Box<Extra<T>>,
        right: Box<Extra<T>>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Extra<T>>,
    },

    Define(String, Box<Extra<T>>),
    Block(Vec<Extra<T>>, Option<Box<Extra<T>>>),
    If {
        condition: Box<Extra<T>>,
        then_branch: Box<Extra<T>>,
        else_branch: Option<Box<Extra<T>>>,
    },
    Loop {
        init: Option<Box<Extra<T>>>,
        condition: Box<Extra<T>>,
        increment: Option<Box<Extra<T>>>,
        body: Box<Extra<T>>,
    },
    IterLoop {
        item: String,
        iterable: Box<Extra<T>>,
        body: Box<Extra<T>>,
    },
    Ident(String),
    ArrayIndex {
        array: Box<Extra<T>>,
        index: Box<Extra<T>>,
    },
    FieldAccess {
        object: Box<Extra<T>>,
        field: String,
    },
    FunctionCall {
        function: Box<Extra<T>>,
        arguments: Vec<Extra<T>>,
    },
    PropertyFunctionCall {
        object: Box<Extra<T>>,
        function: String,
        arguments: Vec<Extra<T>>,
    },
    Break,
    Continue,
    Return(Option<Box<Extra<T>>>),
    Raise(Box<Extra<T>>),
}

impl<T> Extra<T> {
    pub fn map_extra<K>(self, f: &impl Fn(T) -> K) -> Extra<K> {
        Extra {
            expr: match self.expr {
                Expression::Number(n) => Expression::Number(n),
                Expression::Integer(i) => Expression::Integer(i),
                Expression::Bool(b) => Expression::Bool(b),
                Expression::String(s) => Expression::String(s),
                Expression::ArrayLiteral(elements) => {
                    Expression::ArrayLiteral(elements.into_iter().map(|e| e.map_extra(f)).collect())
                }
                Expression::ObjectLiteral(fields) => Expression::ObjectLiteral(
                    fields
                        .into_iter()
                        .map(|(k, v)| (k, v.map_extra(f)))
                        .collect(),
                ),
                Expression::FunctionLiteral { parameters, body } => Expression::FunctionLiteral {
                    parameters,
                    body: Box::new(body.map_extra(f)),
                },
                Expression::PrototypeLiteral {
                    superclass,
                    properties,
                } => Expression::PrototypeLiteral {
                    superclass: superclass.map(|sc| Box::new(sc.map_extra(f))),
                    properties: properties
                        .into_iter()
                        .map(|(name, member_type, expr)| (name, member_type, expr.map_extra(f)))
                        .collect(),
                },
                Expression::Null => Expression::Null,
                Expression::BinaryOp { op, left, right } => Expression::BinaryOp {
                    op,
                    left: Box::new(left.map_extra(f)),
                    right: Box::new(right.map_extra(f)),
                },
                Expression::UnaryOp { op, expr } => Expression::UnaryOp {
                    op,
                    expr: Box::new(expr.map_extra(f)),
                },
                Expression::Define(name, value) => {
                    Expression::Define(name, Box::new(value.map_extra(f)))
                }
                Expression::Block(exprs, ret) => Expression::Block(
                    exprs.into_iter().map(|e| e.map_extra(f)).collect(),
                    ret.map(|r| Box::new(r.map_extra(f))),
                ),
                Expression::If {
                    condition,
                    then_branch,
                    else_branch,
                } => Expression::If {
                    condition: Box::new(condition.map_extra(f)),
                    then_branch: Box::new(then_branch.map_extra(f)),
                    else_branch: else_branch.map(|eb| Box::new(eb.map_extra(f))),
                },
                Expression::Loop {
                    init,
                    condition,
                    increment,
                    body,
                } => Expression::Loop {
                    init: init.map(|i| Box::new(i.map_extra(f))),
                    condition: Box::new(condition.map_extra(f)),
                    increment: increment.map(|inc| Box::new(inc.map_extra(f))),
                    body: Box::new(body.map_extra(f)),
                },
                Expression::IterLoop {
                    item,
                    iterable,
                    body,
                } => Expression::IterLoop {
                    item,
                    iterable: Box::new(iterable.map_extra(f)),
                    body: Box::new(body.map_extra(f)),
                },
                Expression::Ident(name) => Expression::Ident(name),
                Expression::ArrayIndex { array, index } => Expression::ArrayIndex {
                    array: Box::new(array.map_extra(f)),
                    index: Box::new(index.map_extra(f)),
                },
                Expression::FieldAccess { object, field } => Expression::FieldAccess {
                    object: Box::new(object.map_extra(f)),
                    field,
                },
                Expression::FunctionCall {
                    function,
                    arguments,
                } => Expression::FunctionCall {
                    function: Box::new(function.map_extra(f)),
                    arguments: arguments.into_iter().map(|arg| arg.map_extra(f)).collect(),
                },
                Expression::PropertyFunctionCall {
                    object,
                    function,
                    arguments,
                } => Expression::PropertyFunctionCall {
                    object: Box::new(object.map_extra(f)),
                    function,
                    arguments: arguments.into_iter().map(|arg| arg.map_extra(f)).collect(),
                },
                Expression::Break => Expression::Break,
                Expression::Continue => Expression::Continue,
                Expression::Return(expr) => {
                    Expression::Return(expr.map(|e| Box::new(e.map_extra(f))))
                }
                Expression::Raise(expr) => Expression::Raise(Box::new(expr.map_extra(f))),
            },
            extra: f(self.extra),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Extra<T> {
    pub expr: Expression<T>,
    pub extra: T,
}

impl<T> fmt::Display for Expression<Extra<T>>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "Number({})", n),
            Self::Integer(i) => write!(f, "Integer({})", i),
            Self::Bool(b) => write!(f, "Bool({})", b),
            Self::String(s) => write!(f, "String({:?})", s),
            Self::ArrayLiteral(elements) => {
                write!(f, "ArrayLiteral([")?;
                for elem in elements {
                    write!(f, "{}, ", elem.expr)?;
                }
                write!(f, "])")
            }
            Self::ObjectLiteral(fields) => {
                write!(f, "ObjectLiteral({{")?;
                for (key, value) in fields {
                    write!(f, "{}: {}, ", key, value.expr)?;
                }
                write!(f, "}})")
            }
            Self::FunctionLiteral { parameters, body } => {
                write!(f, "FunctionLiteral(")?;
                write!(f, "params: [")?;
                for param in parameters {
                    write!(f, "{}, ", param)?;
                }
                write!(f, "], body: {})", body.expr)
            }
            Self::PrototypeLiteral {
                properties,
                superclass,
            } => {
                write!(
                    f,
                    "PrototypeLiteral(superclass: {:?}, properties: [",
                    superclass
                )?;
                for property in properties {
                    write!(
                        f,
                        "{} ({:?}): {}, ",
                        property.0, property.1, property.2.expr
                    )?;
                }
                write!(f, "])")
            }
            Self::Null => write!(f, "Null"),
            Self::BinaryOp { op, left, right } => {
                write!(f, "BinaryOp({}, {}, {})", op, left.expr, right.expr)
            }
            Self::UnaryOp { op, expr } => write!(f, "UnaryOp({}, {})", op, expr.expr),
            Self::Define(name, value) => write!(f, "Define({}, {})", name, value.expr),
            Self::Block(exprs, ret) => {
                write!(f, "Block(")?;
                for expr in exprs {
                    write!(f, "{};", expr.expr)?;
                }
                if let Some(ret) = ret {
                    write!(f, "{}", ret.expr)?;
                }
                write!(f, ")")
            }
            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "If({}, {}, ", condition.expr, then_branch.expr)?;
                if let Some(else_branch) = else_branch {
                    write!(f, "{})", else_branch.expr)
                } else {
                    write!(f, "None)")
                }
            }
            Self::Loop {
                init,
                condition,
                increment,
                body,
            } => {
                write!(f, "Loop(")?;
                if let Some(init) = init {
                    write!(f, "init: {}, ", init.expr)?;
                } else {
                    write!(f, "init: None, ")?;
                }
                write!(f, "condition: {}, ", condition.expr)?;
                if let Some(increment) = increment {
                    write!(f, "increment: {}, ", increment.expr)?;
                } else {
                    write!(f, "increment: None, ")?;
                }
                write!(f, "body: {})", body.expr)
            }
            Self::IterLoop {
                item,
                iterable,
                body,
            } => {
                write!(
                    f,
                    "IterLoop(item: {}, iterable: {}, body: {})",
                    item, iterable.expr, body.expr
                )
            }
            Self::Ident(name) => write!(f, "Ident({})", name),
            Self::ArrayIndex { array, index } => {
                write!(f, "ArrayIndex({}, {})", array.expr, index.expr)
            }
            Self::FieldAccess { object, field } => {
                write!(f, "FieldAccess({}, {})", object.expr, field)
            }
            Self::FunctionCall {
                function,
                arguments,
            } => {
                write!(f, "FunctionCall({}, [", function.expr)?;
                for arg in arguments {
                    write!(f, "{}, ", arg.expr)?;
                }
                write!(f, "])")
            }
            Self::PropertyFunctionCall {
                object,
                function,
                arguments,
            } => {
                write!(f, "PropertyFunctionCall({}, {}, [", object.expr, function)?;
                for arg in arguments {
                    write!(f, "{}, ", arg.expr)?;
                }
                write!(f, "])")
            }
            Self::Break => write!(f, "Break"),
            Self::Continue => write!(f, "Continue"),
            Self::Return(expr) => {
                if let Some(expr) = expr {
                    write!(f, "Return({})", expr.expr)
                } else {
                    write!(f, "Return(None)")
                }
            }
            Self::Raise(expr) => write!(f, "Raise({})", expr.expr),
        }
    }
}
