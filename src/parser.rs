use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::{
    Parser,
    input::{Stream, ValueInput},
    pratt::{Associativity, infix, postfix, prefix},
    prelude::*,
};

use crate::lexer::{LocatedToken, Token};

#[derive(Debug, Clone)]
pub enum Expression {
    Number(f64),
    Integer(i64),
    Bool(bool),
    String(String),
    ArrayLiteral(Vec<LocatedExpression>),
    FunctionLiteral {
        parameters: Vec<String>,
        body: Box<LocatedExpression>,
    },
    PrototypeLiteral {
        properties: Vec<(String, LocatedExpression)>,
    },
    ObjectLiteral(Vec<(String, LocatedExpression)>),
    Null,

    BinaryOp {
        op: BinaryOp,
        left: Box<LocatedExpression>,
        right: Box<LocatedExpression>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<LocatedExpression>,
    },

    Define(String, Box<LocatedExpression>),
    Block(Vec<LocatedExpression>, Option<Box<LocatedExpression>>),
    If {
        condition: Box<LocatedExpression>,
        then_branch: Box<LocatedExpression>,
        else_branch: Option<Box<LocatedExpression>>,
    },
    ForLoop {
        init: Box<LocatedExpression>,
        condition: Box<LocatedExpression>,
        increment: Box<LocatedExpression>,
        body: Box<LocatedExpression>,
    },
    Ident(String),
    ArrayIndex {
        array: Box<LocatedExpression>,
        index: Box<LocatedExpression>,
    },
    FieldAccess {
        object: Box<LocatedExpression>,
        field: String,
    },
    FunctionCall {
        function: Box<LocatedExpression>,
        arguments: Vec<LocatedExpression>,
    },
    PropertyFunctionCall {
        object: Box<LocatedExpression>,
        function: String,
        arguments: Vec<LocatedExpression>,
    },
}

impl fmt::Display for Expression {
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
            Self::PrototypeLiteral { properties } => {
                write!(f, "PrototypeLiteral(properties: [")?;
                for property in properties {
                    write!(f, "{}: {}, ", property.0, property.1.expr)?;
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
            Self::ForLoop {
                init,
                condition,
                increment,
                body,
            } => write!(
                f,
                "ForLoop({}, {}, {}, {})",
                init.expr, condition.expr, increment.expr, body.expr
            ),
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
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocatedExpression {
    pub expr: Expression,
    pub span: core::ops::Range<usize>,
}

fn located(expr: Expression, span: SimpleSpan) -> LocatedExpression {
    LocatedExpression {
        expr,
        span: span.into_range(),
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,

    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    And,
    Or,

    Assign,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEqual => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEqual => write!(f, ">="),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Negate,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Not => write!(f, "not"),
            Self::Negate => write!(f, "-"),
        }
    }
}

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, LocatedExpression, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let mut expr = Recursive::declare();

    let atom = select! {
        Token::Number(n) => Expression::Number(n),
        Token::Integer(i) => Expression::Integer(i),
        Token::Bool(b) => Expression::Bool(b),
        Token::String(s) => Expression::String(s),
        Token::Char(c) => Expression::String(c.to_string()),
        Token::Ident(name) => Expression::Ident(name.to_string()),
        Token::Null => Expression::Null,
    }
    .map_with(|expr, extra| located(expr, extra.span()));

    let array_literal = expr
        .clone()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map(Expression::ArrayLiteral)
        .recover_with(chumsky::recovery::via_parser(
            chumsky::recovery::nested_delimiters(Token::LBracket, Token::RBracket, [], |_| {
                Expression::ArrayLiteral(vec![])
            }),
        ))
        .map_with(|expr, extra| located(expr, extra.span()));

    let object_literal = select! { Token::Ident(name) => name.to_string() }
        .then_ignore(just(Token::Colon))
        .then(expr.clone())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map(Expression::ObjectLiteral)
        .map_with(|expr, extra| located(expr, extra.span()));

    let block = expr
        .clone()
        .then_ignore(just(Token::Semicolon))
        .repeated()
        .collect::<Vec<_>>()
        .then(expr.clone().or_not())
        .map(|exprs| Expression::Block(exprs.0, exprs.1.map(Box::new)))
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map_with(|expr, extra| located(expr, extra.span()));

    let brace_start = choice((object_literal, block)).recover_with(chumsky::recovery::via_parser(
        chumsky::recovery::nested_delimiters(Token::LBrace, Token::RBrace, [], |_| {
            Expression::Block(vec![], None)
        })
        .map_with(|expr, extra| located(expr, extra.span())),
    ));

    let parenthesized = expr
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .recover_with(chumsky::recovery::via_parser(
            chumsky::recovery::nested_delimiters(Token::LParen, Token::RParen, [], |_| {
                Expression::Null
            })
            .map_with(|expr, extra| located(expr, extra.span())),
        ));

    let define = just(Token::Let)
        .ignore_then(select! { Token::Ident(name) => name.to_string() })
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|(name, value)| Expression::Define(name, Box::new(value)))
        .map_with(|expr, extra| located(expr, extra.span()));

    let define_fn = just(Token::Fn)
        .ignore_then(select! { Token::Ident(name) => name.to_string() })
        .then(
            select! { Token::Ident(name) => name.to_string() }
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(expr.clone())
        .try_map_with(|((name, params), body), extra| {
            if params
                .iter()
                .collect::<std::collections::HashSet<_>>()
                .len()
                != params.len()
            {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    "Function parameters must be unique",
                ));
            }
            Ok(located(
                Expression::Define(
                    name,
                    Box::new(located(
                        Expression::FunctionLiteral {
                            parameters: params,
                            body: Box::new(body),
                        },
                        extra.span(),
                    )),
                ),
                extra.span(),
            ))
        });

    let if_else = just(Token::If)
        .ignore_then(just(Token::LParen))
        .ignore_then(expr.clone())
        .then_ignore(just(Token::RParen))
        .then(expr.clone())
        .then(just(Token::Else).ignore_then(expr.clone()).or_not())
        .map_with(|((condition, then_branch), else_branch), extra| {
            located(
                Expression::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: else_branch.map(Box::new),
                },
                extra.span(),
            )
        });

    let for_loop = just(Token::For)
        .ignore_then(just(Token::LParen))
        .ignore_then(expr.clone())
        .then_ignore(just(Token::Semicolon))
        .then(expr.clone())
        .then_ignore(just(Token::Semicolon))
        .then(expr.clone())
        .then_ignore(just(Token::RParen))
        .then(expr.clone())
        .map_with(|(((init, condition), increment), body), extra| {
            located(
                Expression::ForLoop {
                    init: Box::new(init),
                    condition: Box::new(condition),
                    increment: Box::new(increment),
                    body: Box::new(body),
                },
                extra.span(),
            )
        });

    let define_class = just(Token::Class)
        .ignore_then(select! { Token::Ident(name) => name.to_string() })
        .then(
            choice((
                define_fn
                    .clone()
                    .then_ignore(just(Token::Semicolon))
                    .map(|lf| {
                        if let Expression::Define(name, expr) = lf.expr {
                            (name, *expr)
                        } else {
                            panic!("Expected function definition in class body")
                        }
                    }),
                define
                    .clone()
                    .then_ignore(just(Token::Semicolon))
                    .map(|lf| {
                        if let Expression::Define(name, expr) = lf.expr {
                            (name, *expr)
                        } else {
                            panic!("Expected field definition in class body")
                        }
                    }),
            ))
            .repeated()
            .collect::<Vec<(String, LocatedExpression)>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .try_map_with(|(name, body), extra| {
            if body
                .iter()
                .map(|(n, _)| n)
                .collect::<std::collections::HashSet<_>>()
                .len()
                != body.len()
            {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    "Duplicate field or method names in class definition",
                ));
            }
            Ok(located(
                Expression::Define(
                    name,
                    Box::new(located(
                        Expression::PrototypeLiteral { properties: body },
                        extra.span(),
                    )),
                ),
                extra.span(),
            ))
        });

    let basic = choice((
        atom,
        array_literal,
        brace_start,
        define,
        define_fn,
        define_class,
        for_loop,
        if_else,
        parenthesized,
    ));

    let op = |c| just(c);
    let bin_infix = |assoc, token, binary_op: BinaryOp| {
        infix(assoc, op(token), move |lhs, _, rhs, extra| {
            located(
                Expression::BinaryOp {
                    op: binary_op.clone(),
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                },
                extra.span(),
            )
        })
    };

    let operators = basic.pratt((
        prefix(14, op(Token::Minus), |_, expr, extra| {
            located(
                Expression::UnaryOp {
                    op: UnaryOp::Negate,
                    expr: Box::new(expr),
                },
                extra.span(),
            )
        }),
        prefix(14, op(Token::Not), |_, expr, extra| {
            located(
                Expression::UnaryOp {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                },
                extra.span(),
            )
        }),
        bin_infix(Associativity::Left(13), Token::Star, BinaryOp::Multiply),
        bin_infix(Associativity::Left(13), Token::Slash, BinaryOp::Divide),
        bin_infix(Associativity::Left(12), Token::Plus, BinaryOp::Add),
        bin_infix(Associativity::Left(12), Token::Minus, BinaryOp::Subtract),
        bin_infix(Associativity::Left(10), Token::Less, BinaryOp::Less),
        bin_infix(
            Associativity::Left(10),
            Token::LessEqual,
            BinaryOp::LessEqual,
        ),
        bin_infix(Associativity::Left(10), Token::Greater, BinaryOp::Greater),
        bin_infix(
            Associativity::Left(10),
            Token::GreaterEqual,
            BinaryOp::GreaterEqual,
        ),
        bin_infix(Associativity::Left(9), Token::Equal, BinaryOp::Equal),
        bin_infix(Associativity::Left(9), Token::NotEqual, BinaryOp::NotEqual),
        bin_infix(Associativity::Left(6), Token::And, BinaryOp::And),
        bin_infix(Associativity::Left(5), Token::Or, BinaryOp::Or),
        bin_infix(Associativity::Right(3), Token::Assign, BinaryOp::Assign),
        postfix(
            16,
            just(Token::Dot)
                .ignore_then(select! { Token::Ident(name) => name.to_string() })
                .then_ignore(just(Token::LParen))
                .then(
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|v| v.unwrap_or_default()),
                )
                .then_ignore(just(Token::RParen)),
            |lhs, (name, op), extra| {
                located(
                    Expression::PropertyFunctionCall {
                        object: Box::new(lhs),
                        function: name,
                        arguments: op,
                    },
                    extra.span(),
                )
            },
        ),
        postfix(
            15,
            just(Token::Dot).ignore_then(select! { Token::Ident(name) => name.to_string() }),
            |lhs, op, extra| {
                located(
                    Expression::FieldAccess {
                        object: Box::new(lhs),
                        field: op,
                    },
                    extra.span(),
                )
            },
        ),
        postfix(
            15,
            just(Token::LParen)
                .ignore_then(
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|v| v.unwrap_or_default()),
                )
                .then_ignore(just(Token::RParen)),
            |lhs, op, extra| {
                located(
                    Expression::FunctionCall {
                        function: Box::new(lhs),
                        arguments: op,
                    },
                    extra.span(),
                )
            },
        ),
        postfix(
            15,
            just(Token::LBracket)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::RBracket)),
            |lhs, op, extra| {
                located(
                    Expression::ArrayIndex {
                        array: Box::new(lhs),
                        index: Box::new(op),
                    },
                    extra.span(),
                )
            },
        ),
    ));

    expr.define(operators);

    expr.then_ignore(just(Token::Semicolon))
        .repeated()
        .collect::<Vec<_>>()
        .map_with(|exprs, extra| located(Expression::Block(exprs, None), extra.span()))
}

pub fn parse(source: &str, tokens: Vec<LocatedToken>) -> Option<LocatedExpression> {
    let token_iter = tokens.into_iter().map(|lt| (lt.token, lt.span.into()));
    let token_stream =
        Stream::from_iter(token_iter).map((0..source.len()).into(), |(t, s): (_, _)| (t, s));
    match parser().parse(token_stream).into_result() {
        Ok(expr) => Some(expr),
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, ((), err.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(((), err.span().into_range()))
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(source))
                    .unwrap();
            }
            None
        }
    }
}
