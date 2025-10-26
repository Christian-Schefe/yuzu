use std::{collections::HashSet, fmt};

use chumsky::{
    Parser,
    input::{Stream, ValueInput},
    pratt::{Associativity, infix, postfix, prefix},
    prelude::*,
};

use crate::{
    lexer::{LocatedToken, Token},
    location::{LineIndex, Located, Location, Position},
};

mod expression;

pub use expression::*;

fn located<T>(expr: T, span: SimpleSpan) -> Located<T> {
    let start = Position {
        index: span.start,
        line: 0,
        column: 0,
    };
    let end = Position {
        index: span.end,
        line: 0,
        column: 0,
    };
    Located::new(
        expr,
        Location::new(start, end, String::new(), String::new()),
    )
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

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

impl BinaryOp {
    pub fn can_short_circuit(&self) -> bool {
        matches!(self, Self::And | Self::Or | Self::NullCoalesce)
    }
    pub fn method_name(&self) -> &'static str {
        match self {
            Self::Add => "$add",
            Self::Subtract => "$sub",
            Self::Multiply => "$mul",
            Self::Divide => "$div",
            Self::Modulo => "$mod",
            Self::Equal => "$eq",
            Self::NotEqual => "$neq",
            Self::Less => "$lt",
            Self::LessEqual => "$le",
            Self::Greater => "$gt",
            Self::GreaterEqual => "$ge",
            Self::And => "$and",
            Self::Or => "$or",
            Self::NullCoalesce => "$coalesce",
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
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

impl UnaryOp {
    pub fn method_name(&self) -> &'static str {
        match self {
            Self::Not => "$not",
            Self::Negate => "$negate",
        }
    }
}

fn statements_to_block(
    mut statements: Vec<(LocatedExpression, Option<Token>)>,
    last_expr: Option<LocatedExpression>,
) -> Expression {
    let last = if let Some(last) = last_expr {
        Some(Box::new(last))
    } else if statements.last().is_some_and(|x| x.1.is_none()) {
        let last = statements.pop().unwrap().0;
        Some(Box::new(last))
    } else {
        None
    };
    let statements = statements.into_iter().map(|(s, _)| s).collect();
    Expression::Block(statements, last)
}

fn pattern_parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, Located<Pattern>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let pattern = recursive(|pattern| {
        let ident = select! { Token::Ident(name) => match name {
            "_" => Pattern::Wildcard,
            s => Pattern::Ident(s.to_string())
        }}
        .map_with(|p, extra| located(p, extra.span()))
        .boxed();

        let object = select! { Token::Ident(name) => name.to_string() }
            .clone()
            .then(just(Token::Colon).ignore_then(pattern.clone()).or_not())
            .map_with(|(name, pattern), extra| {
                let pattern =
                    pattern.unwrap_or_else(|| located(Pattern::Ident(name.clone()), extra.span()));
                PatternObjectItem::Pattern(name, pattern)
            })
            .or(just(Token::ThreeDots)
                .ignore_then(select! { Token::Ident(name) => name.to_string() })
                .map(PatternObjectItem::Rest))
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .try_map_with(|pattern_items, extra| {
                let mut entries = Vec::new();
                let mut rest = None;
                for item in pattern_items {
                    if rest.is_some() {
                        return Err(chumsky::error::Rich::custom(
                            extra.span(),
                            "Rest pattern must be the last item in array pattern",
                        ));
                    }
                    match item {
                        PatternObjectItem::Pattern(name, pattern) => {
                            entries.push((name, pattern));
                        }
                        PatternObjectItem::Rest(name) => {
                            rest = Some(name);
                        }
                    }
                }
                Ok(located(
                    Pattern::Object {
                        entries: entries,
                        rest,
                    },
                    extra.span(),
                ))
            })
            .boxed();

        let array = pattern
            .clone()
            .map(PatternArrayItem::Pattern)
            .or(just(Token::ThreeDots)
                .ignore_then(select! { Token::Ident(name) => name.to_string() })
                .map(PatternArrayItem::Rest))
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBracket), just(Token::RBracket))
            .try_map_with(|pattern_items, extra| {
                let mut items = Vec::new();
                let mut rest = None;
                for item in pattern_items {
                    if rest.is_some() {
                        return Err(chumsky::error::Rich::custom(
                            extra.span(),
                            "Rest pattern must be the last item in array pattern",
                        ));
                    }
                    match item {
                        PatternArrayItem::Pattern(p) => items.push(p),
                        PatternArrayItem::Rest(name) => {
                            rest = Some(name);
                        }
                    }
                }
                Ok(located(Pattern::Array { items, rest }, extra.span()))
            })
            .boxed();

        choice((object, array, ident))
    });
    pattern
}

fn parser<'tokens, 'src: 'tokens, I>(
    root_name: String,
) -> impl Parser<'tokens, I, Vec<ParsedModuleItem>, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! { Token::Ident(name) => name.to_string() };

    let double_colon_identifier_with_name = ident
        .clone()
        .separated_by(just(Token::DoubleColon))
        .at_least(1)
        .collect::<Vec<_>>()
        .map_with(move |mut parts, extra| {
            let name = parts.last().unwrap().clone();
            parts.reverse();
            let Some(mut top_level) = parts.pop() else {
                panic!("Unreachable: parts should have at least one element");
            };
            if top_level == "root" {
                top_level = root_name.clone();
            }

            let mut cur_expr = Expression::Ident(top_level.clone());
            for part in parts.into_iter().rev() {
                cur_expr = Expression::FieldAccess {
                    object: Box::new(located(cur_expr, extra.span())),
                    field: part,
                };
            }
            (located(cur_expr, extra.span()), name)
        })
        .boxed();

    let double_colon_identifier = double_colon_identifier_with_name
        .clone()
        .map(|(expr, _)| expr)
        .boxed();

    let pattern = pattern_parser().boxed();

    let parameter_list = ident
        .clone()
        .map(|name| (name, false))
        .or(just(Token::ThreeDots)
            .ignore_then(ident.clone())
            .map(|name| (name, true)))
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .try_map_with(|params, extra| {
            let duplicates = get_duplicates(params.iter().map(|(name, _)| name).cloned());
            if !duplicates.is_empty() {
                return Err(chumsky::error::Rich::custom(
                    extra.span(),
                    format!(
                        "Duplicate function parameter names: {}",
                        duplicates.into_iter().collect::<Vec<_>>().join(", ")
                    ),
                ));
            }
            let mut items = Vec::new();
            let mut rest = None;
            for (item, is_rest) in params {
                if rest.is_some() {
                    return Err(chumsky::error::Rich::custom(
                        extra.span(),
                        "Rest pattern must be the last item in parameter list",
                    ));
                }
                if is_rest {
                    rest = Some(item);
                } else {
                    items.push(item);
                }
            }
            Ok(FunctionParameters::new(items, rest))
        })
        .boxed();

    let expr = recursive(move |expr| {
        let function_literal = parameter_list
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then(expr.clone())
            .map_with(|(params, body), extra| {
                Box::new(located(
                    Expression::FunctionLiteral {
                        parameters: params,
                        body: Box::new(body),
                        is_async: false
                    },
                    extra.span(),
                ))
            })
            .boxed();

        let let_ = just(Token::Let).or(just(Token::Const))
            .then(pattern.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map_with(|((is_const, pattern), value), extra| {
                located(
                    Expression::Define {
                        pattern,
                        value: Box::new(value),
                        is_const: is_const == Token::Const,
                    },
                    extra.span(),
                )
            })
            .boxed();

        let parenthesized = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with(|expr, extra| located(expr.data, extra.span()));

        let inline_expr = {
            let val = select! {
                Token::Number(n) => Expression::Number(n),
                Token::Integer(i) => Expression::Integer(i.to_string()),
                Token::Bool(b) => Expression::Bool(b),
                Token::String(s) => Expression::String(s),
                Token::Char(c) => Expression::String(c.to_string()),
                Token::Null => Expression::Null,
                Token::Break => Expression::Break,
                Token::Continue => Expression::Continue,
            }
            .map_with(|expr, extra| located(expr, extra.span()));

            let expression_spread_list = just(Token::ThreeDots)
                .or_not()
                .then(expr.clone())
                .map(|(spread, expr)| (expr, spread.is_some()))
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();

            let pair_list = choice((
                ident
                    .clone()
                    .then_ignore(just(Token::Colon))
                    .map(Option::Some),
                just(Token::ThreeDots).map(|_| None),
            ))
            .then(expr.clone())
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<_>>()
            .boxed();

            let new_expr = just(Token::New)
                .ignore_then(choice((parenthesized.clone(), double_colon_identifier.clone())))
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
                })
                .boxed();

            let import_expr = just(Token::Use)
                .ignore_then(
                    double_colon_identifier_with_name
                        .map_with(|(expr, name), extra| {
                            located(
                                Expression::Define {
                                    pattern: located(Pattern::Ident(name.clone()), extra.span()),
                                    value: Box::new(expr),
                                    is_const: true,
                                },
                                extra.span(),
                            )
                        }),
                )
                .boxed();

            let atom = choice((
                double_colon_identifier,
                val,
                new_expr,
                import_expr,
                just(Token::Return)
                    .ignore_then(expr.clone().or_not())
                    .map_with(|expr, extra| {
                        located(Expression::Return(expr.map(Box::new)), extra.span())
                    }),
                just(Token::Raise)
                    .ignore_then(expr.clone())
                    .map_with(|expr, extra| {
                        located(Expression::Raise(Box::new(expr)), extra.span())
                    }),
                just(Token::Await)
                    .ignore_then(expr.clone().or_not())
                    .map_with(|expr, extra| {
                        located(Expression::Await(expr.map(Box::new)), extra.span())
                    }),
                let_.clone(),
                just(Token::Async)
                    .or_not()
                    .then(parameter_list)
                    .delimited_by(just(Token::LParen), just(Token::RParen))
                    .then_ignore(just(Token::DoubleArrow))
                    .then(expr.clone())
                    .map_with(|((is_async, params), body), extra| {
                        located(
                            Expression::FunctionLiteral {
                                parameters: params,
                                body: Box::new(body),
                                is_async: is_async.is_some(),
                            },
                            extra.span(),
                        )
                    }),
                expression_spread_list
                    .clone()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket))
                    .map(Expression::ArrayLiteral)
                    .map_with(|expr, extra| located(expr, extra.span())),
                parenthesized.clone(),
                pair_list
                    .clone()
                    .delimited_by(just(Token::LBrace), just(Token::RBrace))
                    .map_with(|pairs, extra| {
                        located(Expression::ObjectLiteral(pairs), extra.span())
                    }),
            ))
            .labelled("expression")
            .boxed();

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

            let operators = atom.pratt((
                unary_prefix(Token::Minus, UnaryOp::Negate),
                unary_prefix(Token::Not, UnaryOp::Not),
                bin_infix(Associativity::Left(13), Token::Star, BinaryOp::Multiply),
                bin_infix(Associativity::Left(13), Token::Slash, BinaryOp::Divide),
                bin_infix(Associativity::Left(13), Token::Percent, BinaryOp::Modulo),
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
                    15,
                    just(Token::Dot).or(just(Token::DoubleColon))
                        .ignore_then(select! { Token::Ident(name) => name.to_string() }),
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
                    expression_spread_list
                        .clone()
                        .delimited_by(just(Token::LParen), just(Token::RParen))
                        .boxed(),
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

            operators.labelled("expression").boxed()
        };
        let for_iter_loop = just(Token::For)
            .ignore_then(just(Token::LParen))
            .ignore_then(
                ident
                    .clone()
                    .then_ignore(just(Token::In))
                    .then(expr.clone()),
            )
            .then_ignore(just(Token::RParen))
            .then(expr.clone())
            .map_with(|((item, iterable), body), extra| {
                located(desugar_iter_loop(item, iterable, body), extra.span())
            })
            .boxed();

        let for_loop = just(Token::For)
            .ignore_then(
                expr.clone()
                    .then_ignore(just(Token::Semicolon))
                    .then(expr.clone())
                    .then_ignore(just(Token::Semicolon))
                    .then(expr.clone())
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
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
            })
            .boxed();

        let while_loop = just(Token::While)
            .ignore_then(parenthesized.clone())
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
            })
            .boxed();

        let try_catch = just(Token::Try)
            .ignore_then(expr.clone())
            .then(
                just(Token::Catch)
                    .ignore_then(
                        expr.clone()
                            .then_ignore(just(Token::As))
                            .or_not()
                            .then(select! { Token::Ident(name) => name.to_string() })
                            .delimited_by(just(Token::LParen), just(Token::RParen)),
                    )
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
            )
            .boxed();

        let if_else = just(Token::If)
            .ignore_then(parenthesized.clone())
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
            })
            .boxed();

        let function =  just(Token::Async)
            .or_not()
            .then_ignore(just(Token::Fn))
            .then(ident.clone())
            .map_with(|(is_async , name), extra| (is_async.is_some(), located(Pattern::Ident(name), extra.span())))
            .then(function_literal.clone())
            .map_with(|((is_async, pattern), mut func), extra| {
                set_function_literal_async(&mut func, is_async);
                located(
                    Expression::Define {
                        pattern,
                        value: func,
                        is_const: true,
                    },
                    extra.span(),
                )
            })
            .boxed();

        let class = just(Token::Class)
            .ignore_then(select! { Token::Ident(name) => name.to_string() })
            .map_with(|name, extra| located(Pattern::Ident(name), extra.span()))
            .then(just(Token::Colon).ignore_then(expr.clone()).or_not())
            .then(
                choice((
                    just(Token::Async).or_not()
                        .then(just(Token::Static).or_not())
                        .then_ignore(just(Token::Fn))
                        .then(ident.clone())
                        .then(function_literal.clone())
                        .try_map(|(((is_async, is_static), name), mut func), span| {
                            set_function_literal_async(&mut func, is_async.is_some());
                            let Expression::FunctionLiteral { parameters, .. } = &mut func.data
                            else {
                                panic!("Expected function definition in class body")
                            };
                            let is_static = is_static.is_some();
                            if !is_static {
                                if parameters.parameters.contains(&"this".to_string()) {
                                    return Err(chumsky::error::Rich::custom(
                                        span,
                                        "Instance method parameter list cannot contain 'this' parameter",
                                    ));
                                }
                                parameters.parameters.insert(0, "this".to_string());
                            }
                            Ok(ClassMember::Method {
                                name,
                                value: *func,
                                is_static,
                            })
                        }),
                    just(Token::Constructor)
                        .ignore_then(function_literal.clone())
                        .map(|mut func| {
                            let Expression::FunctionLiteral { .. } = &mut func.data else {
                                panic!("Expected function definition in class body")
                            };
                            ClassMember::Constructor { value: *func }
                        }),
                ))
                .then(just(Token::Semicolon).or_not())
                .try_map_with(|(data, semicolon), extra| {
                    if needs_semi(&data.expr().data) && semicolon.is_none() {
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
            .try_map_with(|((pattern, parent), body), extra| {
                let duplicates = get_duplicates(body.iter().map(|member| match member {
                    ClassMember::Method { name, .. } => name.clone(),
                    ClassMember::Constructor { .. } => "$constructor".to_string(),
                }));
                if !duplicates.is_empty() {
                    return Err(chumsky::error::Rich::custom(
                        extra.span(),
                        format!(
                            "Duplicate class member names: {}",
                            duplicates.into_iter().collect::<Vec<_>>().join(", ")
                        ),
                    ));
                }
                Ok(located(
                    Expression::Define {
                        pattern,
                        value: Box::new(located(
                            ClassMember::make_class_expr(body, parent.map(Box::new)),
                            extra.span(),
                        )),
                        is_const: true,
                    },
                    extra.span(),
                ))
            })
            .boxed();
        let block = recursive(|block| {
            let no_semi_statements = choice((
                function.clone(),
                class.clone(),
                if_else.clone(),
                try_catch.clone(),
                for_loop.clone(),
                for_iter_loop.clone(),
                while_loop.clone(),
                block.clone(),
            ))
            .boxed();
            // Note the order here matters: we want to try parsing no-semi statements first,
            // so that an empty block {} is not interpreted as an object literal.
            let statement = choice((
                no_semi_statements
                    .clone()
                    .then(just(Token::Semicolon).or_not()),
                inline_expr
                    .clone()
                    .then(just(Token::Semicolon).map(|x| Some(x))),
            ))
            .boxed();
            let last_statement = choice((no_semi_statements, inline_expr.clone()));
            statement
                .repeated()
                .collect::<Vec<_>>()
                .then(last_statement.or_not())
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map_with(|(statements, last_statement), extra| {
                    located(
                        statements_to_block(statements, last_statement),
                        extra.span(),
                    )
                })
        })
        .boxed();

        choice((
            function,
            class,
            if_else,
            try_catch,
            for_loop,
            for_iter_loop,
            while_loop,
            block,
            inline_expr,
        ))
        .boxed()
    })
    .boxed();

    let module = just(Token::Mod)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Semicolon))
        .map_with(|name, extra| ParsedModuleItem::Module(located(name.to_string(), extra.span())))
        .boxed();

    let module_item = choice((
        just(Token::Export)
            .or_not()
            .then(expr.clone())
            .then(just(Token::Semicolon).or_not())
            .try_map_with(|((is_export, data), semicolon), extra| {
                if needs_semi(&data.data) && semicolon.is_none() {
                    return Err(chumsky::error::Rich::custom(
                        extra.span(),
                        "Expected ';' after expression in block",
                    ));
                };
                if is_export.is_some() {
                    let Expression::Define {
                        pattern,
                        value,
                        is_const: _,
                    } = data.data
                    else {
                        return Err(chumsky::error::Rich::custom(
                            extra.span(),
                            "Only definitions can be exported",
                        ));
                    };
                    let name = match pattern.data {
                        Pattern::Ident(name) => name,
                        _ => {
                            return Err(chumsky::error::Rich::custom(
                                extra.span(),
                                "Only simple identifiers can be exported",
                            ));
                        }
                    };
                    let expr = Expression::CanonicDefine { name, value };
                    Ok(ParsedModuleItem::Expression(Located::new(
                        expr,
                        data.location,
                    )))
                } else {
                    Ok(ParsedModuleItem::Expression(data))
                }
            }),
        module.clone(),
    ));

    module_item.repeated().collect::<Vec<_>>()
}

pub fn parse<'a>(
    source: &str,
    line_index: &LineIndex,
    root_name: String,
    module_path: &str,
    file_path: &str,
    tokens: Vec<LocatedToken<'a>>,
) -> Result<Vec<ParsedModuleItem>, Vec<Rich<'a, Token<'a>>>> {
    let token_iter = tokens
        .into_iter()
        .map(|lt| (lt.data, lt.location.span().into()));
    let source_len = source.len();
    let token_stream =
        Stream::from_iter(token_iter).map((source_len..source_len).into(), |(t, s): (_, _)| (t, s));
    parser(root_name)
        .parse(token_stream)
        .into_result()
        .map(|mut pm| {
            for item in &mut pm {
                match item {
                    ParsedModuleItem::Expression(e) => {
                        e.set_module(module_path, file_path, line_index)
                    }
                    ParsedModuleItem::Module(m) => {
                        m.location.module = module_path.to_string();
                        m.location.file_path = file_path.to_string();
                        m.location.update_position(line_index);
                    }
                }
            }
            pm
        })
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
        Expression::ClassLiteral { .. } => false,
        _ => true,
    }
}

fn get_duplicates(items: impl Iterator<Item = String>) -> HashSet<String> {
    let mut seen = HashSet::new();
    let mut duplicates = HashSet::new();
    for item in items {
        if !seen.insert(item.clone()) {
            duplicates.insert(item);
        }
    }
    duplicates
}

fn set_function_literal_async(expr: &mut Box<Located<Expression>>, set_async: bool) {
    let Expression::FunctionLiteral { is_async, .. } = &mut expr.data else {
        panic!("Expected function literal")
    };
    *is_async = set_async;
}
