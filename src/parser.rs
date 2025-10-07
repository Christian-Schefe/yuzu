use std::{
    collections::{HashMap, HashSet},
    fmt,
};

use chumsky::{
    Parser,
    input::{Stream, ValueInput},
    pratt::{Associativity, infix, postfix, prefix},
    prelude::*,
};

use crate::{
    CanonicalPath, ModulePath,
    lexer::{LocatedToken, Token},
    location::{Located, Location},
};

mod expression;
mod types;

pub use expression::*;
pub use types::*;

fn located<T>(expr: T, span: SimpleSpan) -> Located<T> {
    Located::new(expr, Location::new(span.into_range(), "".to_string()))
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

    let class_type = just(Token::Class)
        .ignore_then(
            select! { Token::Ident(name) => name.to_string() }
                .delimited_by(just(Token::Less), just(Token::Greater)),
        )
        .map(TypeHint::Class);

    let parenthesized = just(Token::LParen)
        .ignore_then(type_hint.clone())
        .then_ignore(just(Token::RParen));

    let function_type = just(Token::LParen)
        .ignore_then(
            type_hint
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::RParen))
        .then_ignore(just(Token::DoubleArrow))
        .then(type_hint.clone())
        .map(|(parameters, return_type)| TypeHint::Function {
            parameters,
            return_type: Box::new(return_type),
        });

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

    let basic = choice((
        function_type,
        parenthesized,
        class_type,
        obj_type,
        simple_type,
    ))
    .boxed();

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

fn parser<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, ParsedModule, extra::Err<Rich<'tokens, Token<'src>>>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! { Token::Ident(name) => name.to_string() };
    let colon_type_hint = just(Token::Colon).ignore_then(type_parser()).or_not();

    let type_hint_list = ident
        .clone()
        .then(colon_type_hint.clone())
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>();

    let expr = recursive(move |expr| {
        let function_literal = type_hint_list
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .then(colon_type_hint.clone())
            .then(expr.clone())
            .try_map_with(|((params, return_type), body), extra| {
                let duplicate_params =
                    get_duplicates(params.iter().map(|(name, _)| name.to_string()));
                if !duplicate_params.is_empty() {
                    return Err(chumsky::error::Rich::custom(
                        extra.span(),
                        format!(
                            "Duplicate function parameter names: {}",
                            duplicate_params.into_iter().collect::<Vec<_>>().join(", ")
                        ),
                    ));
                }
                Ok(Box::new(located(
                    Expression::FunctionLiteral {
                        parameters: params,
                        return_type,
                        body: Box::new(body),
                    },
                    extra.span(),
                )))
            });

        let let_ = just(Token::Let)
            .ignore_then(ident.clone())
            .then(colon_type_hint.clone())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .map_with(|((name, type_hint), value), extra| {
                located(
                    Expression::Define {
                        name,
                        value: Box::new(value),
                        type_hint,
                    },
                    extra.span(),
                )
            });

        let parenthesized = expr
            .clone()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map_with(|expr, extra| located(expr.data, extra.span()));

        let identifier = ident
            .clone()
            .separated_by(just(Token::DoubleColon))
            .at_least(1)
            .collect::<Vec<_>>()
            .map_with(|mut parts, extra| {
                let ident = if parts.len() == 1 {
                    Identifier::Simple(parts[0].clone())
                } else {
                    let name = parts.pop().unwrap();
                    Identifier::Scoped(CanonicalPath::new(ModulePath::new(parts), name))
                };
                located(ident, extra.span())
            });

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

            let list = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();

            let pair_list = ident
                .clone()
                .then(just(Token::Colon).ignore_then(expr.clone()))
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>();

            let identifier_expr = identifier
                .clone()
                .map_with(|id, extra| located(Expression::Ident(id.data), extra.span()));

            let new_expr = just(Token::New)
                .ignore_then(choice((parenthesized.clone(), identifier_expr.clone())))
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

            let atom = choice((
                identifier_expr,
                val,
                new_expr,
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
                let_.clone(),
                type_hint_list
                    .delimited_by(just(Token::LParen), just(Token::RParen))
                    .then_ignore(just(Token::DoubleArrow))
                    .then(expr.clone())
                    .map_with(|(params, body), extra| {
                        located(
                            Expression::FunctionLiteral {
                                parameters: params,
                                return_type: None,
                                body: Box::new(body),
                            },
                            extra.span(),
                        )
                    }),
                list.clone()
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
                    just(Token::Dot)
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
            });

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
            });

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
            });

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
            );

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

        let function = just(Token::Fn)
            .ignore_then(ident.clone())
            .then(function_literal.clone())
            .map_with(|(name, func), extra| {
                located(
                    Expression::Define {
                        name,
                        value: func,
                        type_hint: None,
                    },
                    extra.span(),
                )
            })
            .boxed();

        let class = just(Token::Class)
            .ignore_then(select! { Token::Ident(name) => name.to_string() })
            .then(just(Token::Colon).ignore_then(identifier.clone()).or_not())
            .then(
                choice((
                    just(Token::Fn)
                        .ignore_then(ident.clone())
                        .then(function_literal.clone())
                        .map(|(name, mut func)| {
                            let Expression::FunctionLiteral { parameters, .. } = &mut func.data
                            else {
                                panic!("Expected function definition in class body")
                            };
                            parameters.insert(0, ("this".to_string(), Some(TypeHint::This)));
                            ClassMember::Method {
                                name,
                                value: *func,
                                is_static: false,
                            }
                        }),
                    just(Token::Static)
                        .ignore_then(ident.clone())
                        .then(function_literal.clone())
                        .map(|(name, func)| {
                            let Expression::FunctionLiteral { .. } = &func.data else {
                                panic!("Expected function definition in class body")
                            };
                            ClassMember::Method {
                                name,
                                value: *func,
                                is_static: true,
                            }
                        }),
                    just(Token::Constructor)
                        .ignore_then(function_literal.clone())
                        .map(|mut func| {
                            let Expression::FunctionLiteral { parameters, .. } = &mut func.data
                            else {
                                panic!("Expected function definition in class body")
                            };
                            parameters.insert(0, ("this".to_string(), Some(TypeHint::This)));
                            ClassMember::Constructor { value: *func }
                        }),
                    let_.clone().map(|lf| {
                        if let Expression::Define {
                            name,
                            value,
                            type_hint,
                        } = lf.data
                        {
                            ClassMember::Field {
                                name,
                                value: *value,
                                type_hint,
                                is_static: false,
                            }
                        } else {
                            panic!("Expected field definition in class body")
                        }
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
            .try_map_with(|((name, parent), body), extra| {
                let duplicates = get_duplicates(body.iter().map(|member| match member {
                    ClassMember::Field { name, .. } | ClassMember::Method { name, .. } => {
                        name.clone()
                    }
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
                        name,
                        value: Box::new(located(
                            ClassMember::make_class_expr(body, parent.map(|p| p.data)),
                            extra.span(),
                        )),
                        type_hint: None,
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
            ));
            let statement = choice((
                inline_expr
                    .clone()
                    .then(just(Token::Semicolon).map(|x| Some(x))),
                no_semi_statements
                    .clone()
                    .then(just(Token::Semicolon).or_not()),
            ));
            let last_statement = choice((inline_expr.clone(), no_semi_statements));
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
            inline_expr,
            block,
        ))
        .boxed()
    })
    .boxed();

    let module = just(Token::Mod)
        .ignore_then(ident.clone())
        .then_ignore(just(Token::Semicolon))
        .map_with(|name, extra| located(name.to_string(), extra.span()));

    let import = just(Token::Use).ignore_then(
        ident
            .clone()
            .separated_by(just(Token::DoubleColon))
            .at_least(2)
            .collect::<Vec<_>>()
            .then_ignore(just(Token::Semicolon))
            .map_with(|mut parts, extra| {
                let name = parts.pop().unwrap();
                located(
                    CanonicalPath::new(ModulePath::new(parts), name),
                    extra.span(),
                )
            }),
    );

    module
        .repeated()
        .collect::<Vec<_>>()
        .then(import.repeated().collect::<Vec<_>>())
        .then(
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
                .collect::<Vec<_>>(),
        )
        .map(|((modules, imports), exprs)| ParsedModule {
            expressions: exprs,
            children: modules,
            imports,
        })
}

pub fn parse<'a>(
    source: &str,
    file_path: &str,
    tokens: Vec<LocatedToken<'a>>,
) -> Result<ParsedModule, Vec<Rich<'a, Token<'a>>>> {
    let token_iter = tokens.into_iter().map(|lt| (lt.token, lt.span.into()));
    let token_stream =
        Stream::from_iter(token_iter).map((0..source.len()).into(), |(t, s): (_, _)| (t, s));
    parser().parse(token_stream).into_result().map(|mut pm| {
        for expr in &mut pm.expressions {
            expr.set_module(file_path);
        }
        for child in &mut pm.children {
            child.location.module = file_path.to_string();
        }
        for import in &mut pm.imports {
            import.location.module = file_path.to_string();
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
