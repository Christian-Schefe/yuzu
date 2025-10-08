use crate::{
    CanonicalPath,
    location::Located,
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
    Ident(Identifier),
    Break,
    Continue,
    Return(Option<Box<LocatedExpression>>),
    ArrayLiteral(Vec<LocatedExpression>),
    StaticDefine {
        name: String,
        value: Box<LocatedExpression>,
    },
    FunctionLiteral {
        parameters: Vec<String>,
        body: Box<LocatedExpression>,
    },
    ClassLiteral {
        parent: Option<Identifier>,
        fields: Vec<(String, Box<LocatedExpression>)>,
        static_fields: Vec<(String, Box<LocatedExpression>)>,
        constructor: Option<Box<LocatedExpression>>,
        methods: Vec<(String, Box<LocatedExpression>)>,
        static_methods: Vec<(String, Box<LocatedExpression>)>,
    },
    ObjectLiteral(Vec<(String, LocatedExpression)>),
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
        name: String,
        value: Box<LocatedExpression>,
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
        arguments: Vec<LocatedExpression>,
    },
    PropertyFunctionCall {
        object: Box<LocatedExpression>,
        function: String,
        arguments: Vec<LocatedExpression>,
    },
    Raise(Box<LocatedExpression>),
    New {
        expr: Box<LocatedExpression>,
        arguments: Vec<LocatedExpression>,
    },
}

pub enum ClassMember {
    Field {
        name: String,
        value: LocatedExpression,
        is_static: bool,
    },
    Method {
        name: String,
        value: LocatedExpression,
        is_static: bool,
    },
    Constructor {
        value: LocatedExpression,
    },
}

impl ClassMember {
    pub fn expr(&self) -> &LocatedExpression {
        match self {
            ClassMember::Field { value, .. } => value,
            ClassMember::Method { value, .. } => value,
            ClassMember::Constructor { value } => value,
        }
    }
    pub fn make_class_expr(members: Vec<ClassMember>, parent: Option<Identifier>) -> Expression {
        let mut fields = Vec::new();
        let mut static_fields = Vec::new();
        let mut methods = Vec::new();
        let mut static_methods = Vec::new();
        let mut constructor = None;
        for member in members {
            match member {
                ClassMember::Field {
                    name,
                    value,
                    is_static,
                } => {
                    if is_static {
                        static_fields.push((name, Box::new(value)));
                    } else {
                        fields.push((name, Box::new(value)));
                    }
                }
                ClassMember::Method {
                    name,
                    value,
                    is_static,
                } => {
                    if is_static {
                        static_methods.push((name, Box::new(value)));
                    } else {
                        methods.push((name, Box::new(value)));
                    }
                }
                ClassMember::Constructor { value } => {
                    constructor = Some(Box::new(value));
                }
            }
        }
        Expression::ClassLiteral {
            parent,
            fields,
            static_fields,
            constructor,
            methods,
            static_methods,
        }
    }
}

pub enum ParsedModuleItem {
    Expression(LocatedExpression),
    Import(Located<CanonicalPath>),
    Module(Located<String>),
}

#[derive(Debug, Clone)]
pub enum Identifier {
    Simple(String),
    Scoped(CanonicalPath),
}

/// Desugars:
/// ```
/// for (item in iterable) [body]
/// ```
/// into:
/// ```
/// {
///     let $iterable = iterable.iter();
///     let $item = $iterable.next();
///     while ($item != null) {
///         let item = $item.value;
///         [body]
///         $item = $iterable.next();
///     }
/// }
/// ```
pub fn desugar_iter_loop(
    item: String,
    iterable: LocatedExpression,
    body: LocatedExpression,
) -> Expression {
    let get_iterable_call = LocatedExpression::new(
        Expression::PropertyFunctionCall {
            object: Box::new(iterable.clone()),
            function: "iter".to_string(),
            arguments: vec![],
        },
        iterable.location.clone(),
    );
    let iter_init = LocatedExpression::new(
        Expression::Define {
            name: "$iterable".to_string(),
            value: Box::new(get_iterable_call),
        },
        iterable.location.clone(),
    );

    let get_item_call = LocatedExpression::new(
        Expression::PropertyFunctionCall {
            object: Box::new(LocatedExpression::new(
                Expression::Ident(Identifier::Simple("$iterable".to_string())),
                iterable.location.clone(),
            )),
            function: "next".to_string(),
            arguments: vec![],
        },
        iterable.location.clone(),
    );
    let outer_item_init = LocatedExpression::new(
        Expression::Define {
            name: "$item".to_string(),
            value: Box::new(get_item_call.clone()),
        },
        iterable.location.clone(),
    );

    let condition = LocatedExpression::new(
        Expression::BinaryOp {
            op: BinaryOp::NotEqual,
            left: Box::new(LocatedExpression::new(
                Expression::Ident(Identifier::Simple("$item".to_string())),
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
                Expression::Ident(Identifier::Simple("$item".to_string())),
                iterable.location.clone(),
            )),
            field: "value".to_string(),
        },
        iterable.location.clone(),
    );
    let inner_item_init = LocatedExpression::new(
        Expression::Define {
            name: item.clone(),
            value: Box::new(get_inner_item_value.clone()),
        },
        iterable.location.clone(),
    );
    let outer_item_update = LocatedExpression::new(
        Expression::Assign {
            target: Box::new(LocatedExpression::new(
                Expression::Ident(Identifier::Simple("$item".to_string())),
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

impl LocatedExpression {
    pub fn set_module(&mut self, module_path: &str) {
        self.location.module = module_path.to_string();
        match &mut self.data {
            Expression::Number(_) => {}
            Expression::Integer(_) => {}
            Expression::Bool(_) => {}
            Expression::String(_) => {}
            Expression::ArrayLiteral(elements) => {
                elements.iter_mut().for_each(|e| e.set_module(module_path))
            }
            Expression::ObjectLiteral(fields) => fields
                .iter_mut()
                .for_each(|(_, v)| v.set_module(module_path)),
            Expression::StaticDefine { value, .. } => value.set_module(module_path),
            Expression::FunctionLiteral {
                body,
                parameters: _,
            } => body.set_module(module_path),
            Expression::ClassLiteral {
                parent: _,
                fields,
                static_fields,
                constructor,
                methods,
                static_methods,
            } => {
                fields
                    .iter_mut()
                    .for_each(|(_, v)| v.set_module(module_path));
                static_fields
                    .iter_mut()
                    .for_each(|(_, v)| v.set_module(module_path));
                if let Some(constructor) = constructor {
                    constructor.set_module(module_path);
                }
                methods
                    .iter_mut()
                    .for_each(|(_, v)| v.set_module(module_path));
                static_methods
                    .iter_mut()
                    .for_each(|(_, v)| v.set_module(module_path));
            }
            Expression::Null => {}
            Expression::Assign {
                target,
                value,
                op: _,
            } => {
                target.set_module(module_path);
                value.set_module(module_path);
            }
            Expression::BinaryOp { op: _, left, right } => {
                left.set_module(module_path);
                right.set_module(module_path);
            }
            Expression::UnaryOp { op: _, expr } => expr.set_module(module_path),
            Expression::Define { name: _, value } => {
                value.set_module(module_path);
            }
            Expression::Block(exprs, ret) => {
                exprs.iter_mut().for_each(|e| e.set_module(module_path));
                if let Some(ret) = ret {
                    ret.set_module(module_path);
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.set_module(module_path);
                then_branch.set_module(module_path);
                if let Some(else_branch) = else_branch {
                    else_branch.set_module(module_path);
                }
            }
            Expression::TryCatch {
                try_block,
                exception_prototype,
                exception_var: _,
                catch_block,
            } => {
                try_block.set_module(module_path);
                if let Some(exception_prototype) = exception_prototype {
                    exception_prototype.set_module(module_path);
                }
                catch_block.set_module(module_path);
            }
            Expression::Loop {
                init,
                condition,
                increment,
                body,
            } => {
                if let Some(init) = init {
                    init.set_module(module_path);
                }
                condition.set_module(module_path);
                if let Some(increment) = increment {
                    increment.set_module(module_path);
                }
                body.set_module(module_path);
            }
            Expression::Ident(_) => {}
            Expression::ArrayIndex { array, index } => {
                array.set_module(module_path);
                index.set_module(module_path);
            }
            Expression::FieldAccess { object, field: _ } => {
                object.set_module(module_path);
            }
            Expression::FunctionCall {
                function,
                arguments,
            } => {
                function.set_module(module_path);
                arguments
                    .into_iter()
                    .for_each(|arg| arg.set_module(module_path));
            }
            Expression::PropertyFunctionCall {
                object,
                function: _,
                arguments,
            } => {
                object.set_module(module_path);
                arguments
                    .into_iter()
                    .for_each(|arg| arg.set_module(module_path));
            }
            Expression::Break => {}
            Expression::Continue => {}
            Expression::Return(expr) => {
                if let Some(expr) = expr {
                    expr.set_module(module_path);
                }
            }
            Expression::Raise(expr) => expr.set_module(module_path),
            Expression::New { expr, arguments } => {
                expr.set_module(module_path);
                arguments
                    .into_iter()
                    .for_each(|arg| arg.set_module(module_path));
            }
        }
    }
}
