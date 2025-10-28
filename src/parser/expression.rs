use std::fmt::Display;

use crate::{
    location::{LineIndex, Located, Location},
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
    ExternalModule(String),
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
    pub fn set_module(&mut self, file_path: &str, line_index: &LineIndex) {
        self.location.file_path = file_path.to_string();
        self.location.update_position(line_index);

        match &mut self.data {
            Pattern::Wildcard => {}
            Pattern::Ident(_) => {}
            Pattern::Object { entries, rest: _ } => entries
                .iter_mut()
                .for_each(|(_, p)| p.set_module(file_path, line_index)),
            Pattern::Array { items, rest: _ } => items
                .iter_mut()
                .for_each(|e| e.set_module(file_path, line_index)),
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
                write!(f, "{params}, ...{rest}")
            } else {
                write!(f, "...{rest}")
            }
        } else {
            write!(f, "{params}")
        }
    }
}

impl LocatedExpression {
    pub fn set_module(&mut self, file_path: &str, line_index: &LineIndex) {
        self.visit(|e| {
            e.location.file_path = file_path.to_string();
            e.location.update_position(line_index);
            match &mut e.data {
                Expression::Define { pattern, .. } => {
                    pattern.set_module(file_path, line_index);
                }
                _ => {}
            }
        });
    }

    pub fn replace_external_modules<F>(&mut self, mut handler: F) -> bool
    where
        F: FnMut(&String, &Location) -> Option<LocatedExpression>,
    {
        let mut success = true;
        self.visit(|e| {
            if let Expression::CanonicDefine { name: _, value }
            | Expression::Define {
                pattern: _,
                value,
                is_const: _,
            } = &mut e.data
            {
                if let Expression::ExternalModule(name) = &value.data {
                    let Some(res) = handler(name, &value.location) else {
                        success = false;
                        return;
                    };
                    *value = Box::new(res);
                }
            }
        });
        success
    }

    pub fn visit<F>(&mut self, mut accept: F)
    where
        F: FnMut(&mut LocatedExpression),
    {
        let mut stack = vec![self];
        while let Some(current) = stack.pop() {
            accept(current);
            match &mut current.data {
                Expression::Block(statements, last_expr) => {
                    stack.extend(statements.iter_mut());
                    if let Some(last) = last_expr {
                        stack.push(last);
                    }
                }
                Expression::Loop {
                    init,
                    condition,
                    increment,
                    body,
                } => {
                    if let Some(init) = init {
                        stack.push(init);
                    }
                    stack.push(condition);
                    if let Some(inc) = increment {
                        stack.push(inc);
                    }
                    stack.push(body);
                }
                Expression::Null => {}
                Expression::Number(_) => {}
                Expression::Integer(_) => {}
                Expression::Bool(_) => {}
                Expression::String(_) => {}
                Expression::Ident(_) => {}
                Expression::Break => {}
                Expression::Continue => {}
                Expression::Return(located) => {
                    if let Some(expr) = located {
                        stack.push(expr);
                    }
                }
                Expression::ArrayLiteral(items) => {
                    for (item, _) in items.iter_mut() {
                        stack.push(item);
                    }
                }
                Expression::CanonicDefine { name: _, value } => {
                    stack.push(value);
                }
                Expression::FunctionLiteral {
                    parameters: _,
                    body,
                    is_async: _,
                } => {
                    stack.push(body);
                }
                Expression::ClassLiteral {
                    parent,
                    constructor,
                    properties,
                } => {
                    if let Some(parent) = parent {
                        stack.push(parent);
                    }
                    if let Some(constructor) = constructor {
                        stack.push(constructor);
                    }
                    for (_, value, _) in properties.iter_mut() {
                        stack.push(value);
                    }
                }
                Expression::ObjectLiteral(items) => {
                    for (_, value) in items.iter_mut() {
                        stack.push(value);
                    }
                }
                Expression::ModuleLiteral { expressions } => {
                    for expr in expressions.iter_mut() {
                        stack.push(expr);
                    }
                }
                Expression::ExternalModule(_) => {}
                Expression::Assign {
                    target,
                    value,
                    op: _,
                } => {
                    stack.push(target);
                    stack.push(value);
                }
                Expression::BinaryOp { op: _, left, right } => {
                    stack.push(left);
                    stack.push(right);
                }
                Expression::UnaryOp { op: _, expr } => {
                    stack.push(expr);
                }
                Expression::Define {
                    pattern: _,
                    value,
                    is_const: _,
                } => {
                    stack.push(value);
                }
                Expression::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    stack.push(condition);
                    stack.push(then_branch);
                    if let Some(else_branch) = else_branch {
                        stack.push(else_branch);
                    }
                }
                Expression::TryCatch {
                    try_block,
                    exception_prototype,
                    exception_var: _,
                    catch_block,
                } => {
                    stack.push(try_block);
                    if let Some(proto) = exception_prototype {
                        stack.push(proto);
                    }
                    stack.push(catch_block);
                }
                Expression::ArrayIndex { array, index } => {
                    stack.push(array);
                    stack.push(index);
                }
                Expression::FieldAccess { object, field: _ } => {
                    stack.push(object);
                }
                Expression::FunctionCall {
                    function,
                    arguments,
                } => {
                    stack.push(function);
                    for (arg, _) in arguments.iter_mut() {
                        stack.push(arg);
                    }
                }
                Expression::Raise(located) => {
                    stack.push(located);
                }
                Expression::Await(located) => {
                    if let Some(expr) = located {
                        stack.push(expr);
                    }
                }
                Expression::New { expr, arguments } => {
                    stack.push(expr);
                    for arg in arguments.iter_mut() {
                        stack.push(arg);
                    }
                }
            }
        }
    }
}
