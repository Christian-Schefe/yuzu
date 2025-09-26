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
    ArrayLiteral(Vec<Expression>),
    ObjectLiteral(Vec<(String, Expression)>),
    Null,

    BinaryOp {
        op: BinaryOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expression>,
    },

    Define(String, Box<Expression>),
    Block(Vec<Expression>, Option<Box<Expression>>),
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    Ident(String),
    ArrayIndex {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    FieldAccess {
        object: Box<Expression>,
        field: String,
    },
    FunctionCall {
        function: Box<Expression>,
        arguments: Vec<Expression>,
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
                    write!(f, "{}, ", elem)?;
                }
                write!(f, "])")
            }
            Self::ObjectLiteral(fields) => {
                write!(f, "ObjectLiteral({{")?;
                for (key, value) in fields {
                    write!(f, "{}: {}, ", key, value)?;
                }
                write!(f, "}})")
            }
            Self::Null => write!(f, "Null"),
            Self::BinaryOp { op, left, right } => {
                write!(f, "BinaryOp({}, {}, {})", op, left, right)
            }
            Self::UnaryOp { op, expr } => write!(f, "UnaryOp({}, {})", op, expr),
            Self::Define(name, value) => write!(f, "Define({}, {})", name, value),
            Self::Block(exprs, ret) => {
                write!(f, "Block(")?;
                for expr in exprs {
                    write!(f, "{};", expr)?;
                }
                if let Some(ret) = ret {
                    write!(f, "{}", ret)?;
                }
                write!(f, ")")
            }
            Self::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "If({}, {}, ", condition, then_branch)?;
                if let Some(else_branch) = else_branch {
                    write!(f, "{})", else_branch)
                } else {
                    write!(f, "None)")
                }
            }
            Self::Ident(name) => write!(f, "Ident({})", name),
            Self::ArrayIndex { array, index } => {
                write!(f, "ArrayIndex({}, {})", array, index)
            }
            Self::FieldAccess { object, field } => {
                write!(f, "FieldAccess({}, {})", object, field)
            }
            Self::FunctionCall {
                function,
                arguments,
            } => {
                write!(f, "FunctionCall({}, [", function)?;
                for arg in arguments {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, "])")
            }
        }
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
-> impl Parser<'tokens, I, Expression, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let mut expr = Recursive::declare();

    let atom = select! {
        Token::Number(n) => Expression::Number(n),
        Token::Integer(i) => Expression::Integer(i),
        Token::Bool(b) => Expression::Bool(b),
        Token::String(s) => Expression::String(s),
        Token::Ident(name) => Expression::Ident(name.to_string()),
        Token::Null => Expression::Null,
    };

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
        ));

    let object_literal = select! { Token::Ident(name) => name.to_string() }
        .then_ignore(just(Token::Colon))
        .then(expr.clone())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map(Expression::ObjectLiteral);

    let block = expr
        .clone()
        .then_ignore(just(Token::Semicolon))
        .repeated()
        .collect::<Vec<_>>()
        .then(expr.clone().or_not())
        .map(|exprs| Expression::Block(exprs.0, exprs.1.map(Box::new)))
        .delimited_by(just(Token::LBrace), just(Token::RBrace));

    let brace_start = choice((object_literal, block)).recover_with(chumsky::recovery::via_parser(
        chumsky::recovery::nested_delimiters(Token::LBrace, Token::RBrace, [], |_| {
            Expression::Block(vec![], None)
        }),
    ));

    let parenthesized = expr
        .clone()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .recover_with(chumsky::recovery::via_parser(
            chumsky::recovery::nested_delimiters(Token::LParen, Token::RParen, [], |_| {
                Expression::Null
            }),
        ));

    let define = just(Token::Let)
        .ignore_then(select! { Token::Ident(name) => name.to_string() })
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|(name, value)| Expression::Define(name, Box::new(value)));

    let if_else = just(Token::If)
        .ignore_then(just(Token::LParen))
        .ignore_then(expr.clone())
        .then_ignore(just(Token::RParen))
        .then(expr.clone())
        .then(just(Token::Else).ignore_then(expr.clone()).or_not())
        .map(|((condition, then_branch), else_branch)| Expression::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        });

    let basic = choice((
        atom,
        array_literal,
        brace_start,
        define,
        if_else,
        parenthesized,
    ));

    let op = |c| just(c);
    let bin_infix = |assoc, token, binary_op: BinaryOp| {
        infix(assoc, op(token), move |lhs, _, rhs, _| {
            Expression::BinaryOp {
                op: binary_op.clone(),
                left: Box::new(lhs),
                right: Box::new(rhs),
            }
        })
    };

    let operators = basic.pratt((
        prefix(14, op(Token::Minus), |_, expr, _| Expression::UnaryOp {
            op: UnaryOp::Negate,
            expr: Box::new(expr),
        }),
        prefix(14, op(Token::Not), |_, expr, _| Expression::UnaryOp {
            op: UnaryOp::Not,
            expr: Box::new(expr),
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
            15,
            just(Token::Dot).ignore_then(select! { Token::Ident(name) => name.to_string() }),
            |lhs, op, _| Expression::FieldAccess {
                object: Box::new(lhs),
                field: op,
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
            |lhs, op, _| Expression::FunctionCall {
                function: Box::new(lhs),
                arguments: op,
            },
        ),
        postfix(
            15,
            just(Token::LBracket)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::RBracket)),
            |lhs, op, _| Expression::ArrayIndex {
                array: Box::new(lhs),
                index: Box::new(op),
            },
        ),
    ));

    expr.define(operators);

    expr.then_ignore(just(Token::Semicolon))
        .recover_with(chumsky::recovery::skip_until(
            none_of([Token::Semicolon, Token::RBrace, Token::RParen]).ignored(),
            one_of([Token::Semicolon, Token::RBrace, Token::RParen]).ignored(),
            || Expression::Null,
        ))
        .repeated()
        .collect::<Vec<_>>()
        .map(|exprs| Expression::Block(exprs, None))
}

pub fn parse(source: &str, tokens: Vec<LocatedToken>) -> Option<Expression> {
    let token_iter = tokens.into_iter().map(|lt| (lt.token, lt.span.into()));
    let token_stream =
        Stream::from_iter(token_iter).map((0..source.len()).into(), |(t, s): (_, _)| (t, s));
    match parser().parse(token_stream).into_result() {
        Ok(expr) => Some(expr),
        // If parsing was unsuccessful, generate a nice user-friendly diagnostic with ariadne. You could also use
        // codespan, or whatever other diagnostic library you care about. You could even just display-print the errors
        // with Rust's built-in `Display` trait, but it's a little crude
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, ((), err.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_code(3)
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
