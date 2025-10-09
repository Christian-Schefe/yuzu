use crate::{
    location::{Located, located},
    parser::{ClassMemberKind, Expression, LocatedExpression, Pattern},
};

mod instruction;
pub use instruction::*;

pub fn compile_expr(expression: &LocatedExpression) -> Vec<Located<Instruction>> {
    let mut code = Vec::new();
    compile(expression, &mut code);
    code
}

pub fn compile(expression: &LocatedExpression, code: &mut Vec<Located<Instruction>>) {
    match &expression.data {
        Expression::Null => code.push(located(Instruction::PushNull, expression)),
        Expression::Number(num) => code.push(located(Instruction::PushNumber(*num), expression)),
        Expression::Integer(num) => code.push(located(
            Instruction::PushInteger(num.to_string()),
            expression,
        )),
        Expression::Bool(value) => code.push(located(Instruction::PushBool(*value), expression)),
        Expression::String(value) => {
            code.push(located(Instruction::PushString(value.clone()), expression))
        }
        Expression::Ident(identifier) => {
            code.push(located(Instruction::Load(identifier.clone()), expression))
        }
        Expression::Break => code.push(located(Instruction::Break, expression)),
        Expression::Continue => code.push(located(Instruction::Continue, expression)),
        Expression::Return(inner) => {
            if let Some(inner) = inner {
                compile(inner, code);
                code.push(located(Instruction::Return, expression));
            } else {
                code.push(located(Instruction::Return, expression));
            }
        }
        Expression::ArrayLiteral(items) => {
            let mut spread_flags = Vec::new();
            for (item, is_spread) in items {
                compile(item, code);
                spread_flags.push(*is_spread);
            }
            code.push(located(Instruction::PushArray(spread_flags), expression));
        }
        Expression::ObjectLiteral(items) => {
            let mut entries = Vec::new();
            for (key, value) in items {
                compile(value, code);
                entries.push(key.clone());
            }
            code.push(located(Instruction::PushObject(entries), expression));
        }
        Expression::FunctionLiteral { parameters, body } => {
            let body_pointer = code.len() + 2;
            code.push(located(
                Instruction::PushFunction {
                    parameters: parameters.clone(),
                    body_pointer,
                },
                expression,
            ));
            let jump_index = code.len();
            code.push(located(Instruction::Jump(0), expression));
            compile(body, code);
            code.push(located(Instruction::ExitFrame, expression));
            let cur_index = code.len();
            let Instruction::Jump(target) = &mut code[jump_index].data else {
                unreachable!();
            };
            *target = cur_index;
        }
        Expression::Block(expressions, last_expr) => {
            let mut code = vec![located(Instruction::EnterBlock, expression)];
            for expr in expressions {
                compile(expr, &mut code);
                code.push(located(Instruction::Pop, expr));
            }
            if let Some(last_expr) = last_expr {
                compile(last_expr, &mut code);
            } else {
                code.push(located(Instruction::PushNull, expression));
            }
            code.push(located(Instruction::ExitFrame, expression));
        }
        Expression::If {
            condition,
            then_branch,
            else_branch,
        } => {
            compile(condition, code);
            let jump_if_false_index = code.len();
            code.push(located(Instruction::JumpIfFalse(0), expression));
            compile(then_branch, code);
            let jump_index = code.len();
            code.push(located(Instruction::Jump(0), expression));
            let cur_index = code.len();
            let Instruction::JumpIfFalse(target) = &mut code[jump_if_false_index].data else {
                unreachable!();
            };
            *target = cur_index;
            if let Some(else_branch) = else_branch {
                compile(else_branch, code);
            } else {
                code.push(located(Instruction::PushNull, expression));
            }
            let cur_index = code.len();
            let Instruction::Jump(target) = &mut code[jump_index].data else {
                unreachable!();
            };
            *target = cur_index;
        }
        Expression::Loop {
            init,
            condition,
            increment,
            body,
        } => {
            let loop_enter_index = code.len();
            code.push(located(
                Instruction::EnterLoop {
                    break_target: 0,
                    continue_target: 0,
                },
                expression,
            ));
            if let Some(init) = init {
                compile(init, code);
                code.push(located(Instruction::Pop, expression));
            }
            let condition_index = code.len();
            let Instruction::EnterLoop {
                continue_target, ..
            } = &mut code[loop_enter_index].data
            else {
                unreachable!();
            };
            *continue_target = condition_index;
            compile(condition, code);
            let jump_if_false_index = code.len();
            code.push(located(Instruction::JumpIfFalse(0), expression));
            compile(body, code);
            code.push(located(Instruction::Pop, expression));
            if let Some(increment) = increment {
                compile(increment, code);
                code.push(located(Instruction::Pop, expression));
            }
            code.push(located(Instruction::Jump(condition_index), expression));
            let cur_index = code.len();
            let Instruction::JumpIfFalse(target) = &mut code[jump_if_false_index].data else {
                unreachable!();
            };
            *target = cur_index;
            let Instruction::EnterLoop { break_target, .. } = &mut code[loop_enter_index].data
            else {
                unreachable!();
            };
            *break_target = cur_index;
            code.push(located(Instruction::PushNull, expression));
            code.push(located(Instruction::ExitFrame, expression));
        }
        Expression::Define { pattern, value } => {
            compile(value, code);
            code.push(located(
                Instruction::Define(pattern.data.clone(), false),
                pattern,
            ));
        }
        Expression::FunctionCall {
            function,
            arguments,
        } => {
            compile(function, code);
            for arg in arguments {
                compile(arg, code);
            }
            code.push(located(
                Instruction::CallFunction(arguments.len()),
                expression,
            ));
        }
        Expression::BinaryOp { op, left, right } => {
            compile(left, code);
            let try_short_circuit_index = code.len();
            code.push(located(
                Instruction::TryShortCircuit(op.clone(), 0),
                expression,
            ));
            compile(right, code);
            code.push(located(Instruction::BinaryOp(op.clone()), expression));
            let cur_index = code.len();
            let Instruction::TryShortCircuit(_, target) = &mut code[try_short_circuit_index].data
            else {
                unreachable!();
            };
            *target = cur_index;
        }
        Expression::FieldAccess { object, field } => {
            compile(object, code);
            code.push(located(
                Instruction::LoadProperty(field.clone()),
                expression,
            ));
        }
        Expression::PropertyFunctionCall {
            object,
            function,
            arguments,
        } => {
            compile(object, code);
            for arg in arguments {
                compile(arg, code);
            }
            code.push(located(
                Instruction::CallPropertyFunction(function.clone(), arguments.len()),
                expression,
            ));
        }
        Expression::StaticDefine { name, value } => {
            compile(value, code);
            code.push(located(
                Instruction::Define(Pattern::Ident(name.clone()), true),
                expression,
            ));
        }
        Expression::Assign { target, value, op } => match &target.data {
            Expression::Ident(name) => {
                if let Some(op) = op {
                    code.push(located(Instruction::Load(name.clone()), expression));
                    let try_short_circuit_index = code.len();
                    code.push(located(
                        Instruction::TryShortCircuit(op.clone(), 0),
                        expression,
                    ));
                    compile(value, code);
                    code.push(located(Instruction::BinaryOp(op.clone()), expression));
                    code.push(located(Instruction::Store(name.clone()), expression));
                    let cur_index = code.len();
                    let Instruction::TryShortCircuit(_, target) =
                        &mut code[try_short_circuit_index].data
                    else {
                        unreachable!();
                    };
                    *target = cur_index;
                } else {
                    compile(value, code);
                    code.push(located(Instruction::Store(name.clone()), expression));
                }
            }
            Expression::FieldAccess { object, field } => {
                compile(object, code);
                if let Some(op) = op {
                    code.push(located(
                        Instruction::LoadProperty(field.clone()),
                        expression,
                    ));
                    let try_short_circuit_index = code.len();
                    code.push(located(
                        Instruction::TryShortCircuit(op.clone(), 0),
                        expression,
                    ));
                    compile(value, code);
                    code.push(located(Instruction::BinaryOp(op.clone()), expression));
                    code.push(located(
                        Instruction::StoreProperty(field.clone()),
                        expression,
                    ));
                    let cur_index = code.len();
                    let Instruction::TryShortCircuit(_, target) =
                        &mut code[try_short_circuit_index].data
                    else {
                        unreachable!();
                    };
                    *target = cur_index;
                } else {
                    compile(value, code);
                    code.push(located(
                        Instruction::StoreProperty(field.clone()),
                        expression,
                    ));
                }
            }
            Expression::ArrayIndex { array, index } => {
                compile(array, code);
                compile(index, code);
                if let Some(op) = op {
                    code.push(located(Instruction::LoadIndex, expression));
                    let try_short_circuit_index = code.len();
                    code.push(located(
                        Instruction::TryShortCircuit(op.clone(), 0),
                        expression,
                    ));
                    compile(value, code);
                    code.push(located(Instruction::BinaryOp(op.clone()), expression));
                    code.push(located(Instruction::StoreIndex, expression));
                    let cur_index = code.len();
                    let Instruction::TryShortCircuit(_, target) =
                        &mut code[try_short_circuit_index].data
                    else {
                        unreachable!();
                    };
                    *target = cur_index;
                } else {
                    compile(value, code);
                    code.push(located(Instruction::StoreIndex, expression));
                }
            }
            _ => {
                panic!("Invalid assignment target");
            }
        },
        Expression::ClassLiteral {
            parent,
            constructor,
            properties,
        } => {
            let jump_index = code.len();
            code.push(located(Instruction::Jump(0), expression));
            let initializer_pointer = code.len();
            let mut fields = Vec::new();
            for (name, property, kind) in properties {
                if !matches!(kind, ClassMemberKind::Field) {
                    continue;
                }
                fields.push(name.clone());
                compile(property, code);
            }
            code.push(located(Instruction::MakeInstance(fields), expression));
            let cur_index = code.len();
            let Instruction::Jump(target) = &mut code[jump_index].data else {
                unreachable!();
            };
            *target = cur_index;
            for (_, property, kind) in properties {
                if matches!(kind, ClassMemberKind::Field) {
                    continue;
                }
                compile(property, code);
            }
            let constructor_pointer = if let Some(constructor) = constructor {
                let cur_index = code.len();
                compile(constructor, code);
                Some(cur_index)
            } else {
                None
            };
            let property_infos = properties
                .iter()
                .map(|(name, _, kind)| (name.clone(), kind.clone()))
                .collect();
            code.push(located(
                Instruction::PushClass {
                    parent: parent.clone(),
                    initializer_pointer,
                    properties: property_infos,
                    constructor_pointer,
                },
                expression,
            ));
        }
        Expression::UnaryOp { op, expr } => {
            compile(expr, code);
            code.push(located(Instruction::UnaryOp(op.clone()), expression));
        }
        Expression::TryCatch {
            try_block,
            exception_prototype,
            exception_var,
            catch_block,
        } => {
            let try_catch_index = code.len();
            code.push(located(
                Instruction::EnterTryCatch { catch_target: 0 },
                expression,
            ));
            compile(try_block, code);
            let jump_index = code.len();
            code.push(located(Instruction::Jump(0), expression));
            let cur_index = code.len();
            let Instruction::EnterTryCatch { catch_target } = &mut code[try_catch_index].data
            else {
                unreachable!();
            };
            *catch_target = cur_index;
            let mut check_catch_index = None;
            if let Some(exception_prototype) = exception_prototype {
                compile(exception_prototype, code);
                check_catch_index = Some(code.len());
                code.push(located(Instruction::CheckCatch(0), expression));
            }
            code.push(located(
                Instruction::Define(Pattern::Ident(exception_var.clone()), false),
                expression,
            ));
            compile(catch_block, code);
            let cur_index = code.len();
            let Instruction::Jump(target) = &mut code[jump_index].data else {
                unreachable!();
            };
            *target = cur_index;
            if let Some(check_catch_index) = check_catch_index {
                let Instruction::CheckCatch(target) = &mut code[check_catch_index].data else {
                    unreachable!();
                };
                *target = cur_index;
            }
        }
        Expression::ArrayIndex { array, index } => {
            compile(array, code);
            compile(index, code);
            code.push(located(Instruction::LoadIndex, expression));
        }
        Expression::Raise(expr) => {
            compile(expr, code);
            code.push(located(Instruction::Raise, expression));
        }
        Expression::New { expr, arguments } => {
            compile(expr, code);
            code.push(located(Instruction::CallInitializer, expression));
            for arg in arguments {
                compile(arg, code);
            }
            code.push(located(
                Instruction::CallFunction(arguments.len()),
                expression,
            ));
        }
    }
}
