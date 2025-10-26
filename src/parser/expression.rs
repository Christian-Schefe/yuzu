use std::fmt::Display;

use crate::{
    location::{LineIndex, Located},
    parser::{BinaryOp, UnaryOp},
};

pub type LocatedExpression = Located<Expression>;

#[derive(Debug, Clone)]
pub enum Expression {
    Null,
    Number(f64),
    Integer(String),
    Bool(bool),
    String(String),
    Ident(String),
    Break,
    Continue,
    Return(Option<Box<LocatedExpression>>),
    ArrayLiteral(Vec<(LocatedExpression, bool)>), // bool indicates if the element is a spread
    CanonicDefine {
        name: String,
        value: Box<LocatedExpression>,
    },
    FunctionLiteral {
        parameters: FunctionParameters,
        body: Box<LocatedExpression>,
        is_async: bool,
    },
    ClassLiteral {
        parent: Option<Box<LocatedExpression>>,
        constructor: Option<Box<LocatedExpression>>,
        properties: Vec<(String, Box<LocatedExpression>, ClassMemberKind)>,
    },
    ObjectLiteral(Vec<(Option<String>, LocatedExpression)>), // if string is None, it's a spread
    ModuleLiteral {
        expressions: Vec<LocatedExpression>,
    },
    Assign {
        target: Box<LocatedExpression>,
        value: Box<LocatedExpression>,
        op: Option<BinaryOp>,
    },
    BinaryOp {
        op: BinaryOp,
        left: Box<LocatedExpression>,
        right: Box<LocatedExpression>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<LocatedExpression>,
    },

    Define {
        pattern: Located<Pattern>,
        value: Box<LocatedExpression>,
        is_const: bool,
    },
    Block(Vec<LocatedExpression>, Option<Box<LocatedExpression>>),
    If {
        condition: Box<LocatedExpression>,
        then_branch: Box<LocatedExpression>,
        else_branch: Option<Box<LocatedExpression>>,
    },
    TryCatch {
        try_block: Box<LocatedExpression>,
        exception_prototype: Option<Box<LocatedExpression>>,
        exception_var: String,
        catch_block: Box<LocatedExpression>,
    },
    Loop {
        init: Option<Box<LocatedExpression>>,
        condition: Box<LocatedExpression>,
        increment: Option<Box<LocatedExpression>>,
        body: Box<LocatedExpression>,
    },
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
        arguments: Vec<(LocatedExpression, bool)>, // bool indicates if the argument is a spread
    },
    Raise(Box<LocatedExpression>),
    Await(Option<Box<LocatedExpression>>),
    New {
        expr: Box<LocatedExpression>,
        arguments: Vec<LocatedExpression>,
    },
}

pub enum ClassMember {
    Method {
        name: String,
        value: LocatedExpression,
        is_static: bool,
    },
    Constructor {
        value: LocatedExpression,
    },
}

#[derive(Debug, Clone)]
pub struct ClassMemberKind {
    pub is_static: bool,
}

impl ClassMember {
    pub fn expr(&self) -> &LocatedExpression {
        match self {
            ClassMember::Method { value, .. } => value,
            ClassMember::Constructor { value } => value,
        }
    }
    pub fn make_class_expr(
        members: Vec<ClassMember>,
        parent: Option<Box<LocatedExpression>>,
    ) -> Expression {
        let mut properties = Vec::new();
        let mut constructor = None;
        for member in members {
            match member {
                ClassMember::Method {
                    name,
                    value,
                    is_static,
                } => {
                    properties.push((name, Box::new(value), ClassMemberKind { is_static }));
                }
                ClassMember::Constructor { value } => {
                    constructor = Some(Box::new(value));
                }
            }
        }
        Expression::ClassLiteral {
            parent,
            constructor,
            properties,
        }
    }
}

pub enum ParsedModuleItem {
    Expression(LocatedExpression),
    Module(Located<String>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Ident(String),
    Object {
        entries: Vec<(String, Located<Pattern>)>,
        rest: Option<String>,
    },
    Array {
        items: Vec<Located<Pattern>>,
        rest: Option<String>,
    },
}

pub enum PatternObjectItem {
    Pattern(String, Located<Pattern>),
    Rest(String),
}

pub enum PatternArrayItem {
    Pattern(Located<Pattern>),
    Rest(String),
}

/// Desugars:
/// ```
/// for (item in iterable) [body]
/// ```
/// into:
/// ```
/// {
///     let #iterable = iterable.iter();
///     let #item = #iterable.next();
///     while (#item != null) {
///         let item = #item.value;
///         [body]
///         #item = #iterable.next();
///     }
/// }
/// ```
pub fn desugar_iter_loop(
    item: String,
    iterable: LocatedExpression,
    body: LocatedExpression,
) -> Expression {
    let get_iterable_call = LocatedExpression::new(
        Expression::FunctionCall {
            function: Box::new(LocatedExpression::new(
                Expression::FieldAccess {
                    object: Box::new(iterable.clone()),
                    field: "iter".to_string(),
                },
                iterable.location.clone(),
            )),
            arguments: vec![],
        },
        iterable.location.clone(),
    );
    let iter_init = LocatedExpression::new(
        Expression::Define {
            pattern: Located::new(
                Pattern::Ident("#iterable".to_string()),
                iterable.location.clone(),
            ),
            value: Box::new(get_iterable_call),
            is_const: true,
        },
        iterable.location.clone(),
    );

    let get_item_call = LocatedExpression::new(
        Expression::FunctionCall {
            function: Box::new(LocatedExpression::new(
                Expression::FieldAccess {
                    object: Box::new(LocatedExpression::new(
                        Expression::Ident("#iterable".to_string()),
                        iterable.location.clone(),
                    )),
                    field: "next".to_string(),
                },
                iterable.location.clone(),
            )),
            arguments: vec![],
        },
        iterable.location.clone(),
    );
    let outer_item_init = LocatedExpression::new(
        Expression::Define {
            pattern: Located::new(
                Pattern::Ident("#item".to_string()),
                iterable.location.clone(),
            ),
            value: Box::new(get_item_call.clone()),
            is_const: false,
        },
        iterable.location.clone(),
    );

    let condition = LocatedExpression::new(
        Expression::BinaryOp {
            op: BinaryOp::NotEqual,
            left: Box::new(LocatedExpression::new(
                Expression::Ident("#item".to_string()),
                iterable.location.clone(),
            )),
            right: Box::new(LocatedExpression::new(
                Expression::Null,
                iterable.location.clone(),
            )),
        },
        iterable.location.clone(),
    );

    let get_inner_item_value = LocatedExpression::new(
        Expression::FieldAccess {
            object: Box::new(LocatedExpression::new(
                Expression::Ident("#item".to_string()),
                iterable.location.clone(),
            )),
            field: "value".to_string(),
        },
        iterable.location.clone(),
    );
    let inner_item_init = LocatedExpression::new(
        Expression::Define {
            pattern: Located::new(Pattern::Ident(item.clone()), iterable.location.clone()),
            value: Box::new(get_inner_item_value.clone()),
            is_const: true,
        },
        iterable.location.clone(),
    );
    let outer_item_update = LocatedExpression::new(
        Expression::Assign {
            target: Box::new(LocatedExpression::new(
                Expression::Ident("#item".to_string()),
                iterable.location.clone(),
            )),
            value: Box::new(get_item_call),
            op: None,
        },
        iterable.location.clone(),
    );

    let body = LocatedExpression::new(
        Expression::Block(vec![inner_item_init, body], None),
        iterable.location.clone(),
    );

    let loop_expr = LocatedExpression::new(
        Expression::Loop {
            init: None,
            condition: Box::new(condition),
            increment: Some(Box::new(outer_item_update)),
            body: Box::new(body),
        },
        iterable.location.clone(),
    );
    Expression::Block(vec![iter_init, outer_item_init, loop_expr], None)
}

impl Located<Pattern> {
    pub fn set_module(&mut self, module_path: &str, file_path: &str, line_index: &LineIndex) {
        self.location.module = module_path.to_string();
        self.location.file_path = file_path.to_string();
        self.location.update_position(line_index);

        match &mut self.data {
            Pattern::Wildcard => {}
            Pattern::Ident(_) => {}
            Pattern::Object { entries, rest: _ } => entries
                .iter_mut()
                .for_each(|(_, p)| p.set_module(module_path, file_path, line_index)),
            Pattern::Array { items, rest: _ } => items
                .iter_mut()
                .for_each(|e| e.set_module(module_path, file_path, line_index)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionParameters {
    pub parameters: Vec<String>,
    pub rest_parameter: Option<String>,
}

impl FunctionParameters {
    pub fn new(parameters: Vec<String>, rest_parameter: Option<String>) -> Self {
        Self {
            parameters,
            rest_parameter,
        }
    }
}

impl Display for FunctionParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self.parameters.join(", ");
        if let Some(rest) = &self.rest_parameter {
            if !self.parameters.is_empty() {
                write!(f, "{}, ...{}", params, rest)
            } else {
                write!(f, "...{}", rest)
            }
        } else {
            write!(f, "{}", params)
        }
    }
}

impl LocatedExpression {
    pub fn set_module(&mut self, module_path: &str, file_path: &str, line_index: &LineIndex) {
        self.location.module = module_path.to_string();
        self.location.file_path = file_path.to_string();
        self.location.update_position(line_index);

        match &mut self.data {
            Expression::Number(_) => {}
            Expression::Integer(_) => {}
            Expression::Bool(_) => {}
            Expression::String(_) => {}
            Expression::ArrayLiteral(elements) => elements
                .iter_mut()
                .for_each(|e| e.0.set_module(module_path, file_path, line_index)),
            Expression::ObjectLiteral(fields) => fields
                .iter_mut()
                .for_each(|(_, v)| v.set_module(module_path, file_path, line_index)),
            Expression::CanonicDefine { value, .. } => {
                value.set_module(module_path, file_path, line_index)
            }
            Expression::FunctionLiteral {
                body,
                parameters: _,
                is_async: _,
            } => body.set_module(module_path, file_path, line_index),
            Expression::ClassLiteral {
                parent: _,
                constructor,
                properties,
            } => {
                properties
                    .iter_mut()
                    .for_each(|(_, v, _)| v.set_module(module_path, file_path, line_index));
                if let Some(constructor) = constructor {
                    constructor.set_module(module_path, file_path, line_index);
                }
            }
            Expression::ModuleLiteral { expressions } => {
                expressions
                    .iter_mut()
                    .for_each(|e| e.set_module(module_path, file_path, line_index));
            }
            Expression::Null => {}
            Expression::Assign {
                target,
                value,
                op: _,
            } => {
                target.set_module(module_path, file_path, line_index);
                value.set_module(module_path, file_path, line_index);
            }
            Expression::BinaryOp { op: _, left, right } => {
                left.set_module(module_path, file_path, line_index);
                right.set_module(module_path, file_path, line_index);
            }
            Expression::UnaryOp { op: _, expr } => {
                expr.set_module(module_path, file_path, line_index)
            }
            Expression::Define {
                pattern,
                value,
                is_const: _,
            } => {
                pattern.set_module(module_path, file_path, line_index);
                value.set_module(module_path, file_path, line_index);
            }
            Expression::Block(exprs, ret) => {
                exprs
                    .iter_mut()
                    .for_each(|e| e.set_module(module_path, file_path, line_index));
                if let Some(ret) = ret {
                    ret.set_module(module_path, file_path, line_index);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.set_module(module_path, file_path, line_index);
                then_branch.set_module(module_path, file_path, line_index);
                if let Some(else_branch) = else_branch {
                    else_branch.set_module(module_path, file_path, line_index);
                }
            }
            Expression::TryCatch {
                try_block,
                exception_prototype,
                exception_var: _,
                catch_block,
            } => {
                try_block.set_module(module_path, file_path, line_index);
                if let Some(exception_prototype) = exception_prototype {
                    exception_prototype.set_module(module_path, file_path, line_index);
                }
                catch_block.set_module(module_path, file_path, line_index);
            }
            Expression::Loop {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(init) = init {
                    init.set_module(module_path, file_path, line_index);
                }
                condition.set_module(module_path, file_path, line_index);
                if let Some(increment) = increment {
                    increment.set_module(module_path, file_path, line_index);
                }
                body.set_module(module_path, file_path, line_index);
            }
            Expression::Ident(_) => {}
            Expression::ArrayIndex { array, index } => {
                array.set_module(module_path, file_path, line_index);
                index.set_module(module_path, file_path, line_index);
            }
            Expression::FieldAccess { object, field: _ } => {
                object.set_module(module_path, file_path, line_index);
            }
            Expression::FunctionCall {
                function,
                arguments,
            } => {
                function.set_module(module_path, file_path, line_index);
                arguments.into_iter().for_each(|(arg, _)| {
                    arg.set_module(module_path, file_path, line_index);
                });
            }
            Expression::Break => {}
            Expression::Continue => {}
            Expression::Return(expr) => {
                if let Some(expr) = expr {
                    expr.set_module(module_path, file_path, line_index);
                }
            }
            Expression::Raise(expr) => expr.set_module(module_path, file_path, line_index),
            Expression::Await(expr) => {
                if let Some(expr) = expr {
                    expr.set_module(module_path, file_path, line_index);
                }
            }
            Expression::New { expr, arguments } => {
                expr.set_module(module_path, file_path, line_index);
                arguments
                    .into_iter()
                    .for_each(|arg| arg.set_module(module_path, file_path, line_index));
            }
        }
    }
}
