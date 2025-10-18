use crate::{
    location::{Located, located},
    parser::{ClassMemberKind, Expression, LocatedExpression, Pattern},
};

mod instruction;
pub use instruction::*;

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
                code.push(located(Instruction::PushNull, expression));
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
            let jump_index = code.len();
            code.push(located(Instruction::Jump(0), expression));
            let body_pointer = code.len();
            compile(body, code);
            code.push(located(Instruction::ExitFrame, expression));
            let cur_index = code.len();
            let Instruction::Jump(target) = &mut code[jump_index].data else {
                unreachable!();
            };
            *target = cur_index;
            code.push(located(
                Instruction::PushFunction {
                    parameters: parameters.clone(),
                    body_pointer,
                },
                expression,
            ));
        }
        Expression::Block(expressions, last_expr) => {
            code.push(located(Instruction::EnterBlock, expression));
            for expr in expressions {
                compile(expr, code);
                code.push(located(Instruction::Pop, expr));
            }
            if let Some(last_expr) = last_expr {
                compile(last_expr, code);
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
            code.push(located(Instruction::Define(pattern.data.clone()), pattern));
            code.push(located(Instruction::PushNull, expression));
        }
        Expression::FunctionCall {
            function,
            arguments,
        } => {
            compile(function, code);
            for (arg, _) in arguments {
                compile(arg, code);
            }
            code.push(located(
                Instruction::CallFunction(arguments.iter().map(|(_, spread)| *spread).collect()),
                expression,
            ));
        }
        Expression::BinaryOp { op, left, right } => {
            compile(left, code);
            let can_short_circuit = op.can_short_circuit();
            let try_short_circuit_index = code.len();
            if can_short_circuit {
                code.push(located(
                    Instruction::TryShortCircuit(op.clone(), 0),
                    expression,
                ));
            }
            compile(right, code);
            code.push(located(Instruction::BinaryOp(op.clone()), expression));
            if can_short_circuit {
                let cur_index = code.len();
                let Instruction::TryShortCircuit(_, target) =
                    &mut code[try_short_circuit_index].data
                else {
                    unreachable!();
                };
                *target = cur_index;
            }
        }
        Expression::FieldAccess { object, field } => {
            compile(object, code);
            code.push(located(
                Instruction::LoadProperty(field.clone()),
                expression,
            ));
        }
        Expression::CanonicDefine { name, value } => {
            let jump_index = code.len();
            code.push(located(Instruction::Jump(0), expression));
            let body_pointer = code.len();
            code.push(located(
                Instruction::InitializeModule(name.path.clone()),
                expression,
            ));
            code.push(located(Instruction::Pop, expression));
            code.push(located(
                Instruction::StartInitializeLazy(name.clone()),
                expression,
            ));
            compile(value, code);
            code.push(located(
                Instruction::InitializeLazy(name.clone()),
                expression,
            ));
            code.push(located(Instruction::ExitFrame, expression));
            let cur_index = code.len();
            let Instruction::Jump(target) = &mut code[jump_index].data else {
                unreachable!();
            };
            *target = cur_index;
            code.push(located(
                Instruction::DefineCanonic(name.clone(), body_pointer),
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
                    code.push(located(Instruction::DuplicateTopN(1), expression));
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
                    code.push(located(Instruction::DuplicateTopN(2), expression)); // duplicate array and index
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

            let mut methods = Vec::new();
            let mut static_methods = Vec::new();

            for (name, property, kind) in properties {
                let Expression::FunctionLiteral { parameters, body } = &property.data else {
                    panic!("Only functions are allowed as class methods");
                };
                let body_pointer = code.len();
                compile(body, code);
                code.push(located(Instruction::ExitFrame, expression));
                match kind {
                    ClassMemberKind::Method => {
                        methods.push((name.clone(), parameters.clone(), body_pointer))
                    }
                    ClassMemberKind::StaticMethod => {
                        static_methods.push((name.clone(), parameters.clone(), body_pointer))
                    }
                };
            }

            let constructor = if let Some(constructor) = constructor {
                let Expression::FunctionLiteral { parameters, body } = &constructor.data else {
                    panic!("Only functions are allowed as constructors");
                };
                let constructor_pointer = code.len();
                compile(body, code);
                code.push(located(Instruction::ExitFrame, expression));

                Some((parameters.clone(), constructor_pointer))
            } else {
                None
            };

            let cur_index = code.len();
            let Instruction::Jump(target) = &mut code[jump_index].data else {
                unreachable!();
            };
            *target = cur_index;
            if let Some(parent) = parent {
                code.push(located(Instruction::Load(parent.clone()), expression));
            }
            code.push(located(
                Instruction::PushClass {
                    parent: parent.is_some(),
                    methods,
                    static_methods,
                    constructor,
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
            if let Some(exception_prototype) = exception_prototype {
                compile(exception_prototype, code);
            }
            let try_catch_index = code.len();
            code.push(located(
                Instruction::EnterTryCatch {
                    catch_target: 0,
                    filtered: exception_prototype.is_some(),
                },
                expression,
            ));
            compile(try_block, code);
            code.push(located(Instruction::ExitFrame, expression));
            let jump_index = code.len();
            code.push(located(Instruction::Jump(0), expression));
            let cur_index = code.len();
            let Instruction::EnterTryCatch { catch_target, .. } = &mut code[try_catch_index].data
            else {
                unreachable!();
            };
            *catch_target = cur_index;
            code.push(located(Instruction::EnterBlock, expression)); // new scope for catch variable
            code.push(located(
                Instruction::Define(Pattern::Ident(exception_var.clone())),
                expression,
            ));
            compile(catch_block, code);
            code.push(located(Instruction::ExitFrame, expression));
            let cur_index = code.len();
            let Instruction::Jump(target) = &mut code[jump_index].data else {
                unreachable!();
            };
            *target = cur_index;
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
            for arg in arguments {
                compile(arg, code);
            }
            code.push(located(
                Instruction::CallConstructor(arguments.len()),
                expression,
            ));
            code.push(located(Instruction::MakeInstance, expression));
        }
    }
}
