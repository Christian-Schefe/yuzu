use std::{collections::HashMap, io::Read, vec};

use gc_arena::{
    Arena, Collect, Gc, Mutation, Rootable, StaticCollect,
    lock::{GcRefLock, RefLock},
};

use crate::{
    gc_interpreter::{
        exception::{import_error, unhandled_control_flow},
        standard::{define_globals, root_prototypes},
        value::{
            ControlFlow, Environment, FunctionValue, LocatedControlFlow, ObjectValue,
            PrototypeValue, Value, variable_to_string,
        },
    },
    parse_string,
    parser::{Expression, Extra},
    tree_interpreter::{Located, Location},
};

mod exception;
mod resource;
mod standard;
mod value;

#[derive(Collect)]
#[collect(no_drop)]
pub struct MyRoot<'a> {
    modules: GcRefLock<'a, HashMap<String, ModuleRoot<'a>>>,
    module_values: GcRefLock<'a, HashMap<String, Option<Value<'a>>>>,
    root_prototypes: RootPrototypes<'a>,
    std_prototype: GcRefLock<'a, Option<GcRefLock<'a, ObjectValue<'a>>>>,
    pwd: std::path::PathBuf,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct ModuleRoot<'a> {
    env: Environment<'a>,
    expr: Gc<'a, StaticCollect<LocatedExpression>>,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct RootPrototypes<'a> {
    pub number: GcRefLock<'a, PrototypeValue<'a>>,
    pub string: GcRefLock<'a, PrototypeValue<'a>>,
    pub array: GcRefLock<'a, PrototypeValue<'a>>,
    pub object: GcRefLock<'a, PrototypeValue<'a>>,
    pub function: GcRefLock<'a, PrototypeValue<'a>>,
    pub builtin_function: GcRefLock<'a, PrototypeValue<'a>>,
    pub integer: GcRefLock<'a, PrototypeValue<'a>>,
    pub bool: GcRefLock<'a, PrototypeValue<'a>>,
    pub null: GcRefLock<'a, PrototypeValue<'a>>,
    pub prototype: GcRefLock<'a, PrototypeValue<'a>>,
    pub exception: GcRefLock<'a, PrototypeValue<'a>>,
    pub resource: GcRefLock<'a, PrototypeValue<'a>>,
    pub buffer: GcRefLock<'a, PrototypeValue<'a>>,
}

type LocatedExpression = Extra<Location>;

pub fn interpret_global(
    expr: LocatedExpression,
    module_path: String,
    pwd: std::path::PathBuf,
) -> Result<(), Located<String>> {
    let arena = Arena::<Rootable![MyRoot<'_>]>::new(|mc| MyRoot {
        modules: Gc::new(mc, RefLock::new(HashMap::new())),
        module_values: Gc::new(mc, RefLock::new(HashMap::new())),
        root_prototypes: root_prototypes(mc),
        std_prototype: Gc::new(mc, RefLock::new(None)),
        pwd,
    });
    arena.mutate(
        |mc, root| match interpret(mc, root, expr, module_path, false) {
            Ok(_) => Ok(()),
            Err(e) => Err(Located {
                data: format!(
                    "Uncaught exception: {:?}",
                    match e.data {
                        ControlFlow::Break => "<break>".to_string(),
                        ControlFlow::Continue => "<continue>".to_string(),
                        ControlFlow::Error(v) => variable_to_string(&v),
                        ControlFlow::Return(v) => variable_to_string(&v),
                    }
                ),
                location: e.location.clone(),
            }),
        },
    )?;
    Ok(())
}

pub fn interpret<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    expr: LocatedExpression,
    module_path: String,
    no_std: bool,
) -> Result<Value<'a>, LocatedControlFlow<'a>> {
    let env = Gc::new(mc, Environment::new_global(mc, module_path.clone()));
    define_globals(mc, root, env.clone(), no_std);
    let expr = Gc::new(mc, StaticCollect(expr));
    let module_root = ModuleRoot {
        env: env.as_ref().clone(),
        expr: expr.clone(),
    };
    let mut modules = root.modules.borrow_mut(mc);
    modules.insert(module_path.clone(), module_root);
    let do_processing = || {
        let mut processing_stack: Vec<(&LocatedExpression, usize)> = vec![(expr.as_ref(), 0)];
        let mut result_stack: Vec<Result<Value<'a>, LocatedControlFlow<'a>>> = Vec::new();
        let mut process_expr = |exp: &LocatedExpression,
                                step|
         -> Result<Option<Value<'a>>, LocatedControlFlow<'a>> {
            let expr: &'a Expression<Location> = &exp.expr;
            let res = match expr {
                Expression::Null => Some(Value::Null),
                Expression::Bool(b) => Some(Value::Bool(*b)),
                Expression::Number(n) => Some(Value::Number(*n)),
                Expression::Integer(i) => Some(Value::Integer(*i)),
                Expression::String(s) => Some(Value::String(s.chars().collect())),
                Expression::ObjectLiteral(o) => {
                    if step == 0 {
                        for (_, v) in o.iter().rev() {
                            processing_stack.push((v, 0));
                        }
                        None
                    } else {
                        let mut map = HashMap::new();
                        for (k, _) in o.iter() {
                            let val = result_stack
                                .pop()
                                .expect("Not enough values on result stack")?;
                            map.insert(k.clone(), val);
                        }
                        Some(Value::Object(Gc::new(
                            mc,
                            RefLock::new(ObjectValue {
                                properties: map,
                                prototype: root.root_prototypes.object.clone(),
                            }),
                        )))
                    }
                }
                Expression::FunctionLiteral { parameters, body } => Some(Value::Function(Gc::new(
                    mc,
                    FunctionValue {
                        parameters: parameters.clone(),
                        body: Gc::new(mc, StaticCollect(*body.clone())),
                        env: env.clone(),
                    },
                ))),
                Expression::Return(v) => {
                    if step == 0
                        && let Some(v) = v
                    {
                        processing_stack.push((v, 0));
                        None
                    } else {
                        let val = if step == 1 {
                            result_stack
                                .pop()
                                .expect("Not enough values on result stack")?
                        } else {
                            Value::Null
                        };
                        return Err(Located {
                            data: ControlFlow::Return(val),
                            location: exp.extra.clone(),
                        });
                    }
                }
                Expression::Block(expressions, last_expr) => {
                    if step == 0 {
                        if let Some(last_expr) = last_expr {
                            processing_stack.push((&last_expr, 0));
                        }
                        processing_stack.push((&expressions[step], 0));
                        None
                    } else {
                        let expr_results = processing_stack.split_off(processing_stack.len() - expressions.len());
                        for 
                        if last_expr.is_some() {
                            Some(Ok(result_stack
                                .pop()
                                .expect("Not enough values on result stack")?))
                        } else {
                            Some(Ok(Value::Null))
                        }
                    }
                }
                v => todo!("{:?}", v),
            };
            Ok(res)
        };
        while let Some((exp, step)) = processing_stack.pop() {
            match process_expr(exp, step) {
                Ok(Some(res)) => result_stack.push(Ok(res)),
                Ok(None) => processing_stack.push((exp, step + 1)),
                Err(err) => {
                    result_stack.push(Err(err));
                }
            }
        }
        result_stack.pop().expect("No result")
    };
    handle_return_control_flow(mc, root, do_processing(), &env)
}

fn interpret_string<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    input: &str,
    span: &Location,
    module_path: String,
    file_path: Option<&str>,
    env: Gc<'a, Environment<'a>>,
    no_std: bool,
) -> Result<Value<'a>, LocatedControlFlow<'a>> {
    let parsed = parse_string(input, file_path).map_err(|_| {
        import_error::<()>(mc, root, "Failed to parse input", &env, span).unwrap_err()
    })?;
    let add_file_info = |extra| Location::new(extra, module_path.clone());
    let parsed = parsed.map_extra(&add_file_info);
    interpret(mc, root, parsed, module_path, no_std)
}

fn import_module<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    module_relative_path: &str,
    span: &Location,
    env: Gc<'a, Environment<'a>>,
) -> Result<Value<'a>, LocatedControlFlow<'a>> {
    let parts = module_relative_path.split('/').collect::<Vec<_>>();
    let mut module_path = env
        .module_path
        .split('/')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>();
    module_path.pop();
    for part in parts.into_iter() {
        match part {
            "." => continue,
            ".." => {
                module_path.pop();
                continue;
            }
            "" => continue,
            _ => module_path.push(part),
        }
    }
    let module_path = module_path.join("/");

    let prev_val = {
        let mut state = root.module_values.borrow_mut(mc);
        match state.get(&module_path) {
            Some(Some(v)) => Some(v.clone()),
            Some(None) => {
                return import_error(
                    mc,
                    root,
                    &format!("Cyclic import detected for module '{}'", module_path),
                    &env,
                    span,
                );
            }
            None => {
                state.insert(module_path.clone(), None);
                None
            }
        }
    };

    if let Some(v) = prev_val {
        return Ok(v);
    }

    let path = root.pwd.join(format!("{}.yuzu", module_path));
    let mut file = match std::fs::File::open(&path) {
        Ok(f) => f,
        Err(e) => {
            return import_error(
                mc,
                root,
                &format!("Failed to open file {}: {}", path.display(), e),
                &env,
                span,
            );
        }
    };
    let mut buf = String::new();
    if let Err(e) = file.read_to_string(&mut buf) {
        return import_error(mc, root, &format!("Failed to read file: {}", e), &env, span);
    }
    let file_path = path.to_string_lossy().to_string();

    let val = interpret_string(
        mc,
        root,
        &buf,
        span,
        module_path.clone(),
        Some(&file_path),
        env.clone(),
        false,
    )?;

    let mut state = root.module_values.borrow_mut(mc);
    state.insert(module_path, Some(val.clone()));

    Ok(val)
}

fn handle_return_control_flow<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    res: Result<Value<'a>, LocatedControlFlow<'a>>,
    env: &Environment<'a>,
) -> Result<Value<'a>, LocatedControlFlow<'a>> {
    match res {
        Ok(v) => Ok(v),
        Err(LocatedControlFlow {
            data,
            location: span,
        }) => match data {
            ControlFlow::Error(e) => Err(Located {
                data: ControlFlow::Error(e),
                location: span,
            }),
            ControlFlow::Return(v) => Ok(v),
            _ => unhandled_control_flow(mc, root, &env, span),
        },
    }
}
