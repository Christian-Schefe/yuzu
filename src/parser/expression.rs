use crate::{
    location::Located,
    parser::{BinaryOp, UnaryOp, types::TypeHint},
};

pub type LocatedExpression = Located<Expression>;

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
    ClassLiteral {
        parent: Option<Box<LocatedExpression>>,
        properties: Vec<(String, MemberKind, LocatedExpression)>,
    },
    ObjectLiteral(Vec<(String, LocatedExpression)>),
    Null,
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
        type_hint: Option<TypeHint>,
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
    IterLoop {
        item: String,
        iterable: Box<LocatedExpression>,
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
    Break,
    Continue,
    Return(Option<Box<LocatedExpression>>),
    Raise(Box<LocatedExpression>),
    New {
        expr: Box<LocatedExpression>,
        arguments: Vec<LocatedExpression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemberKind {
    Field,
    StaticField,
    Method,
    StaticMethod,
    Constructor,
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

            Expression::FunctionLiteral {
                body,
                parameters: _,
            } => body.set_module(module_path),
            Expression::ClassLiteral {
                parent: superclass,
                properties,
            } => {
                if let Some(superclass) = superclass {
                    superclass.set_module(module_path);
                }
                properties
                    .iter_mut()
                    .for_each(|(_, _, expr)| expr.set_module(module_path))
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
            Expression::Define {
                name: _,
                value,
                type_hint: _,
            } => {
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
            Expression::IterLoop {
                item: _,
                iterable,
                body,
            } => {
                iterable.set_module(module_path);
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
