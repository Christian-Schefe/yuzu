use std::{collections::HashMap, fmt};

use chumsky::{
    Parser,
    input::{Stream, ValueInput},
    pratt::{Associativity, infix, postfix, prefix},
    prelude::*,
};

use crate::{
    lexer::{LocatedToken, Token},
    location::{Located, Location},
};

mod expression;
mod types;

pub use expression::*;
pub use types::*;

fn located<T>(expr: T, span: SimpleSpan) -> Located<T> {
    Located::new(
        expr,
        Location {
            span: span.into_range(),
            module: "".to_string(),
        },
    )
}

#[derive(Debug, Clone, PartialEq)]
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

    NullCoalesce,
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
            Self::NullCoalesce => write!(f, "??"),
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

fn type_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, TypeHint, extra::Err<Rich<'tokens, Token<'src>>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let mut type_hint = Recursive::declare();
    let simple_type = select! {Token::Ident(name) => match name {
            "number" => TypeHint::Union(vec![TypeHint::Number, TypeHint::Integer]),
            "float" => TypeHint::Number,
            "int" => TypeHint::Integer,
            "bool" => TypeHint::Bool,
            "string" => TypeHint::String,
            "any" => TypeHint::Any,
            name => TypeHint::ClassInstance(name.to_string()),
        },
        Token::Null => TypeHint::Null,
    };

    let obj_type = just(Token::LBrace)
        .ignore_then(
            select! { Token::Ident(name) => name.to_string() }
                .then_ignore(just(Token::Colon))
                .then(type_hint.clone())
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .then_ignore(just(Token::Comma).or_not()),
        )
        .then_ignore(just(Token::RBrace))
        .map(|fields| TypeHint::Object(fields.into_iter().collect::<HashMap<String, TypeHint>>()));

    let basic = choice((obj_type, simple_type));

    let operators = basic.pratt((
        postfix(
            15,
            just(Token::LBracket).ignore_then(just(Token::RBracket)),
            |lhs, _, _| TypeHint::Array(Some(Box::new(lhs))),
        ),
        infix(
            Associativity::Left(10),
            just(Token::Pipe),
            move |lhs, _, rhs, _| TypeHint::union(lhs, rhs),
        ),
    ));
    type_hint.define(operators);
    type_hint
}

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, LocatedExpression, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let mut expr = Recursive::declare();

    let colon_type_hint = just(Token::Colon).ignore_then(type_parser()).or_not();

    let atom = select! {
        Token::Number(n) => Expression::Number(n),
        Token::Integer(i) => Expression::Integer(i),
        Token::Bool(b) => Expression::Bool(b),
        Token::String(s) => Expression::String(s),
        Token::Char(c) => Expression::String(c.to_string()),
        Token::Ident(name) => Expression::Ident(name.to_string()),
        Token::Null => Expression::Null,
        Token::Break => Expression::Break,
        Token::Continue => Expression::Continue,
    }
    .map_with(|expr, extra| located(expr, extra.span()));

    let return_expr = just(Token::Return)
        .ignore_then(expr.clone().or_not())
        .map_with(|expr, extra| located(Expression::Return(expr.map(Box::new)), extra.span()));

    let raise_expr = just(Token::Raise)
        .ignore_then(expr.clone())
        .map_with(|expr, extra| located(Expression::Raise(Box::new(expr)), extra.span()));

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
        .then(just(Token::Colon).ignore_then(expr.clone()).or_not())
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .then_ignore(just(Token::Comma).or_not())
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
        .map_with(|expr, extra| {
            located(
                Expression::ObjectLiteral(
                    expr.into_iter()
                        .map(|(k, v)| {
                            (
                                k.clone(),
                                v.unwrap_or(located(Expression::Ident(k), extra.span())),
                            )
                        })
                        .collect(),
                ),
                extra.span(),
            )
        });

    let block = expr
        .clone()
        .then(just(Token::Semicolon).or_not())
        .try_map_with(|(expr, semicolon), extra| {
            if needs_semi(&expr.data) && semicolon.is_none() {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    "Expected ';' after expression in block",
                ));
            };
            Ok(expr)
        })
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

    let lambda_literal = select! { Token::Ident(name) => name.to_string() }
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .then_ignore(just(Token::DoubleArrow))
        .then(expr.clone())
        .try_map_with(|(params, body), extra| {
            if !is_unique(&params) {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    "Function parameters must be unique",
                ));
            }
            Ok(located(
                Expression::FunctionLiteral {
                    parameters: params,
                    body: Box::new(body),
                },
                extra.span(),
            ))
        });

    let define_let = just(Token::Let)
        .ignore_then(select! { Token::Ident(name) => name.to_string() })
        .then(colon_type_hint)
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|((name, type_hint), value)| Expression::Define {
            name,
            value: Box::new(value),
            type_hint,
        })
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
            if !is_unique(&params) {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    "Function parameters must be unique",
                ));
            }
            Ok(located(
                Expression::Define {
                    name,
                    value: Box::new(located(
                        Expression::FunctionLiteral {
                            parameters: params,
                            body: Box::new(body),
                        },
                        extra.span(),
                    )),
                    type_hint: None,
                },
                extra.span(),
            ))
        });

    let define_constructor_fn = just(Token::Constructor)
        .ignore_then(
            select! { Token::Ident(name) => name.to_string() }
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(expr.clone())
        .try_map_with(|(params, body), extra| {
            if !is_unique(&params) {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    "Function parameters must be unique",
                ));
            }
            Ok(located(
                Expression::Define {
                    name: "$constructor".to_string(),
                    value: Box::new(located(
                        Expression::FunctionLiteral {
                            parameters: params,
                            body: Box::new(body),
                        },
                        extra.span(),
                    )),
                    type_hint: None,
                },
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
                Expression::Loop {
                    init: Some(Box::new(init)),
                    condition: Box::new(condition),
                    increment: Some(Box::new(increment)),
                    body: Box::new(body),
                },
                extra.span(),
            )
        });

    let while_loop = just(Token::While)
        .ignore_then(
            expr.clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(expr.clone())
        .map_with(|(condition, body), extra| {
            located(
                Expression::Loop {
                    init: None,
                    condition: Box::new(condition),
                    increment: None,
                    body: Box::new(body),
                },
                extra.span(),
            )
        });

    let iter_loop = just(Token::For)
        .ignore_then(
            select! { Token::Ident(name) => name.to_string() }
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(expr.clone())
        .map_with(|((item, iterable), body), extra| {
            located(
                Expression::IterLoop {
                    item,
                    iterable: Box::new(iterable),
                    body: Box::new(body),
                },
                extra.span(),
            )
        });

    let define_class = just(Token::Class)
        .ignore_then(select! { Token::Ident(name) => name.to_string() })
        .then(just(Token::Colon).ignore_then(expr.clone()).or_not())
        .then(
            choice((
                define_fn.clone().map(|lf| {
                    if let Expression::Define {
                        name,
                        value: mut expr,
                        type_hint: _,
                    } = lf.data
                    {
                        let Expression::FunctionLiteral { parameters, .. } = &mut expr.data else {
                            panic!("Expected function definition in class body")
                        };
                        parameters.insert(0, "this".to_string());
                        (name, MemberKind::Method, *expr)
                    } else {
                        panic!("Expected function definition in class body")
                    }
                }),
                just(Token::Static)
                    .ignore_then(define_fn.clone())
                    .map(|lf| {
                        if let Expression::Define {
                            name,
                            value: expr,
                            type_hint: _,
                        } = lf.data
                        {
                            (name, MemberKind::StaticMethod, *expr)
                        } else {
                            panic!("Expected static method definition in class body")
                        }
                    }),
                define_constructor_fn.clone().map(|lf| {
                    if let Expression::Define {
                        name,
                        value: mut expr,
                        type_hint: _,
                    } = lf.data
                    {
                        let Expression::FunctionLiteral { parameters, .. } = &mut expr.data else {
                            panic!("Expected function definition in class body")
                        };
                        parameters.insert(0, "this".to_string());
                        (name, MemberKind::Constructor, *expr)
                    } else {
                        panic!("Expected constructor definition in class body")
                    }
                }),
                define_let.clone().map(|lf| {
                    if let Expression::Define {
                        name,
                        value: expr,
                        type_hint: _,
                    } = lf.data
                    {
                        (name, MemberKind::Field, *expr)
                    } else {
                        panic!("Expected field definition in class body")
                    }
                }),
            ))
            .then(just(Token::Semicolon).or_not())
            .try_map_with(|(data, semicolon), extra| {
                if needs_semi(&data.2.data) && semicolon.is_none() {
                    return Err(chumsky::error::Rich::custom(
                        extra.span(),
                        "Expected ';' after expression in block",
                    ));
                };
                Ok(data)
            })
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .try_map_with(|((name, superclass), body), extra| {
            if !is_unique_iter(body.iter().map(|(n, _, _)| n)) {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    "Duplicate field or method names in class definition",
                ));
            }
            Ok(located(
                Expression::Define {
                    name,
                    value: Box::new(located(
                        Expression::ClassLiteral {
                            properties: body,
                            parent: superclass.map(Box::new),
                        },
                        extra.span(),
                    )),
                    type_hint: None,
                },
                extra.span(),
            ))
        });

    let new_expr = just(Token::New)
        .ignore_then(choice((
            parenthesized.clone(),
            select! { Token::Ident(name) => Expression::Ident(name.to_string()) }
                .map_with(|expr, extra| located(expr, extra.span())),
        )))
        .then(
            expr.clone()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .map_with(|(expr, args), extra| {
            located(
                Expression::New {
                    expr: Box::new(expr),
                    arguments: args,
                },
                extra.span(),
            )
        });

    let try_catch = just(Token::Try)
        .ignore_then(expr.clone())
        .then(
            just(Token::Catch)
                .ignore_then(expr.clone().then_ignore(just(Token::As)).or_not())
                .then(select! { Token::Ident(name) => name.to_string() })
                .then(expr.clone()),
        )
        .map_with(
            |(try_block, ((exception_type, exception_name), catch_block)), extra| {
                located(
                    Expression::TryCatch {
                        try_block: Box::new(try_block),
                        catch_block: Box::new(catch_block),
                        exception_var: exception_name,
                        exception_prototype: exception_type.map(Box::new),
                    },
                    extra.span(),
                )
            },
        );

    let basic = choice((
        atom,
        array_literal,
        brace_start,
        define_let,
        define_fn,
        define_class,
        new_expr,
        for_loop,
        while_loop,
        iter_loop,
        if_else,
        lambda_literal, //must come before parenthesized
        parenthesized,
        return_expr,
        raise_expr,
        try_catch,
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
    let bin_infix_assign = |assoc, token, binary_op: Option<BinaryOp>| {
        infix(assoc, op(token), move |lhs, _, rhs, extra| {
            located(
                Expression::Assign {
                    op: binary_op.clone(),
                    target: Box::new(lhs),
                    value: Box::new(rhs),
                },
                extra.span(),
            )
        })
    };

    let unary_prefix = |token, unary_op: UnaryOp| {
        prefix(14, op(token), move |_, expr, extra| {
            located(
                Expression::UnaryOp {
                    op: unary_op.clone(),
                    expr: Box::new(expr),
                },
                extra.span(),
            )
        })
    };

    let operators = basic.pratt((
        unary_prefix(Token::Minus, UnaryOp::Negate),
        unary_prefix(Token::Not, UnaryOp::Not),
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
        bin_infix(
            Associativity::Left(4),
            Token::NullCoalesce,
            BinaryOp::NullCoalesce,
        ),
        bin_infix_assign(Associativity::Right(3), Token::Assign, None),
        bin_infix_assign(
            Associativity::Right(3),
            Token::PlusAssign,
            Some(BinaryOp::Add),
        ),
        bin_infix_assign(
            Associativity::Right(3),
            Token::MinusAssign,
            Some(BinaryOp::Subtract),
        ),
        bin_infix_assign(
            Associativity::Right(3),
            Token::StarAssign,
            Some(BinaryOp::Multiply),
        ),
        bin_infix_assign(
            Associativity::Right(3),
            Token::SlashAssign,
            Some(BinaryOp::Divide),
        ),
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
                .then_ignore(just(Token::RParen))
                .map_with(|expr, extra| (expr, extra.span())),
            |lhs, ((name, op), span): ((String, Vec<_>), SimpleSpan), _| {
                located(
                    Expression::PropertyFunctionCall {
                        object: Box::new(lhs),
                        function: name,
                        arguments: op,
                    },
                    span,
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

    expr.then(just(Token::Semicolon).or_not())
        .try_map_with(|(data, semicolon), extra| {
            if needs_semi(&data.data) && semicolon.is_none() {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    "Expected ';' after expression in block",
                ));
            };
            Ok(data)
        })
        .repeated()
        .collect::<Vec<_>>()
        .map_with(|exprs, extra| located(Expression::Block(exprs, None), extra.span()))
}

pub fn parse<'a>(
    source: &str,
    tokens: Vec<LocatedToken<'a>>,
) -> Result<LocatedExpression, Vec<Rich<'a, Token<'a>>>> {
    let token_iter = tokens.into_iter().map(|lt| (lt.token, lt.span.into()));
    let token_stream =
        Stream::from_iter(token_iter).map((0..source.len()).into(), |(t, s): (_, _)| (t, s));
    parser().parse(token_stream).into_result()
}

fn needs_semi(expr: &Expression) -> bool {
    match expr {
        Expression::Block { .. } => false,
        Expression::Define { value, .. } => needs_semi(&value.data),
        Expression::If {
            then_branch,
            else_branch,
            ..
        } => needs_semi(&else_branch.as_ref().unwrap_or(then_branch).data),
        Expression::FunctionLiteral { body, .. } => needs_semi(&body.data),
        Expression::Loop { body, .. } => needs_semi(&body.data),
        Expression::IterLoop { body, .. } => needs_semi(&body.data),
        Expression::ClassLiteral { .. } => false,
        _ => true,
    }
}

fn is_unique<T: Eq + std::hash::Hash>(items: &[T]) -> bool {
    let set: std::collections::HashSet<_> = items.iter().collect();
    set.len() == items.len()
}

fn is_unique_iter<T: Eq + std::hash::Hash, I: Iterator<Item = T>>(items: I) -> bool {
    let mut set = std::collections::HashSet::new();
    for item in items {
        if !set.insert(item) {
            return false;
        }
    }
    true
}
