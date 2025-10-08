use std::collections::{HashMap, HashSet};

use gc_arena::{Gc, Mutation, StaticCollect, lock::GcRefLock};

use crate::{
    CanonicalPath, ModulePath, ParsedModuleTree,
    gc_interpreter::{
        ExecResult, ModuleTree, MyRoot, expression_to_function, make_class_literal,
        standard::define_globals,
        value::{ClassValue, Environment, StaticValue, Value},
    },
    parser::{Expression, Identifier, LocatedExpression},
};

pub struct CanonicalItem<'a> {
    expr: &'a LocatedExpression,
    parent: Option<CanonicalPath>,
}

pub enum CanonicalItemType<'a> {
    Import(CanonicalPath),
    StaticVariable(String, &'a LocatedExpression),
    Item(CanonicalItem<'a>),
}

pub fn resolve_canonical_paths<'a>(mc: &Mutation<'a>, root: &'a MyRoot<'a>) {
    let mut items = Vec::new();

    for (name, child) in &root.program.children {
        let root_path = ModulePath::from_root(name);
        collect_canonical_items(mc, root, &mut items, &root_path, child);
    }

    let item_map = items
        .into_iter()
        .map(|(k, v)| (k.to_string(), (k, v)))
        .collect();
    let order = match topo_sort(&item_map) {
        Ok(order) => order,
        Err(cycle) => {
            panic!("Cycle detected: {:?}", cycle);
        }
    };

    let global_env = Gc::new(mc, Environment::new_global(mc));
    define_globals(mc, global_env);
    let std_child = &root.program.children["std"];
    let std_path = ModulePath::from_root("std");
    transfer_env_recursive(mc, root, &std_path, std_child, global_env);

    for path in &order {
        if path.path.get_root() != "std" {
            continue;
        }
        let Some((_, item)) = item_map.get(&path.to_string()) else {
            panic!("Item not found for {}", path);
        };
        define_item(mc, root, path, item, true);
    }

    let std_tree = ModuleTree::get(root.root_module, &std_path).unwrap();
    let std_env = std_tree.borrow().env;

    for (name, child) in &root.program.children {
        if name == "std" {
            continue;
        }
        let root_path = ModulePath::from_root(name);
        transfer_env_recursive(mc, root, &root_path, child, std_env);
    }

    for path in &order {
        if path.path.get_root() == "std" {
            continue;
        }
        let module_tree = ModuleTree::get(root.root_module, &path.path)
            .expect(&format!("Module {} not found", path.path.to_string()));
        let env = module_tree.borrow().env;
        if env.exists(&path.item) {
            println!("Skipping already defined item: {}", path);
            continue;
        }
        let Some((_, item)) = item_map.get(&path.to_string()) else {
            panic!("Item not found for {}", path);
        };
        define_item(mc, root, path, item, false);
    }
}

fn define_item<'a>(
    mc: &Mutation<'a>,
    root: &'a MyRoot<'a>,
    path: &CanonicalPath,
    item: &CanonicalItemType<'a>,
    allow_merge: bool,
) {
    let module_tree = ModuleTree::get(root.root_module, &path.path)
        .expect(&format!("Module {} not found", path.path.to_string()));
    let env = module_tree.borrow().env;
    match item {
        CanonicalItemType::Import(import) => {
            let source_module_tree = ModuleTree::get(root.root_module, &import.path);
            if let Some(source_module_tree) = source_module_tree {
                let source_env = source_module_tree.borrow().env;
                if !env.import_from(mc, source_env, &import.item) {
                    panic!("Failed to define imported item: {}", import.item);
                }
            } else {
                panic!("Imported module not found: {}", import.path);
            }
        }
        CanonicalItemType::Item(item) => {
            let value = match &item.expr.data {
                Expression::FunctionLiteral { .. } => {
                    Value::Function(expression_to_function(mc, item.expr, env))
                }
                Expression::ClassLiteral { .. } => {
                    let ExecResult::Value(class_val) =
                        make_class_literal(mc, root, &item.expr, env)
                    else {
                        panic!("Failed to create class literal for {}", path);
                    };
                    class_val
                }
                expr => unreachable!("Unexpected expression in canonical items: {:?}", expr),
            };
            let tree = module_tree.borrow();
            if tree.env.exists(&path.item) {
                if !allow_merge {
                    panic!("Failed to define item: {}", path.item);
                }
                let Some(Value::Class(existing_class)) = tree.env.get_simple(&path.item) else {
                    panic!("Failed to define item: {}", path.item);
                };
                if let Value::Class(new_class) = value {
                    merge_std_classes(mc, new_class, existing_class);
                } else {
                    panic!("Failed to define item: {}", path.item);
                }
            } else {
                tree.env.define_const(mc, &path.item, value);
            }
        }
        CanonicalItemType::StaticVariable(name, expr) => {
            if !env.define_static(
                mc,
                name,
                StaticValue::Uninitialized(Gc::new(mc, StaticCollect((*expr).clone()))),
            ) {
                panic!("Failed to define static variable: {}", name);
            }
        }
    }
}

fn merge_std_classes<'a>(
    mc: &Mutation<'a>,
    from_class: GcRefLock<'a, ClassValue<'a>>,
    to_class: GcRefLock<'a, ClassValue<'a>>,
) {
    let from_ref = from_class.borrow();
    let mut to_ref = to_class.borrow_mut(mc);
    for (name, method) in from_ref.methods.iter() {
        to_ref.methods.insert(name.clone(), *method);
    }
    for (name, method) in from_ref.static_methods.iter() {
        to_ref.static_methods.insert(name.clone(), *method);
    }
    for (name, field) in from_ref.instance_fields.iter() {
        to_ref.instance_fields.push((name.clone(), *field));
    }
    for (name, field) in from_ref.static_fields.iter() {
        to_ref.static_fields.insert(name.clone(), field.clone());
    }
    if from_ref.constructor.is_some() {
        to_ref.constructor = from_ref.constructor;
    }
}

fn collect_canonical_items<'a>(
    mc: &Mutation<'a>,
    root: &'a MyRoot<'a>,
    items: &mut Vec<(CanonicalPath, CanonicalItemType<'a>)>,
    path: &ModulePath,
    tree: &'a ParsedModuleTree,
) {
    let _ = ModuleTree::get_or_insert(mc, root.root_module, path);
    for import in tree.imports.iter() {
        items.push((
            CanonicalPath::new(path.clone(), import.data.item.clone()),
            CanonicalItemType::Import(import.data.clone()),
        ));
    }
    for expr in tree.expressions.iter() {
        match &expr.data {
            Expression::Define { name, value } => match &value.data {
                Expression::FunctionLiteral { .. } => {
                    items.push((
                        CanonicalPath::new(path.clone(), name.clone()),
                        CanonicalItemType::Item(CanonicalItem {
                            expr: value,
                            parent: None,
                        }),
                    ));
                }
                Expression::ClassLiteral { parent, .. } => {
                    items.push((
                        CanonicalPath::new(path.clone(), name.clone()),
                        CanonicalItemType::Item(CanonicalItem {
                            expr: value,
                            parent: parent.clone().map(|p| match p {
                                Identifier::Simple(name) => {
                                    CanonicalPath::new(path.clone(), name.clone())
                                }
                                Identifier::Scoped(path) => path,
                            }),
                        }),
                    ));
                }
                _ => {}
            },
            Expression::StaticDefine { name, value } => {
                items.push((
                    CanonicalPath::new(path.clone(), name.clone()),
                    CanonicalItemType::StaticVariable(name.clone(), value),
                ));
            }
            _ => {}
        }
    }
    for (name, child) in &tree.children {
        let child_path = path.push(name.clone());
        collect_canonical_items(mc, root, items, &child_path, child);
    }
}

fn topo_sort<'a>(
    items: &'a HashMap<String, (CanonicalPath, CanonicalItemType<'a>)>,
) -> Result<Vec<&'a CanonicalPath>, Vec<String>> {
    let mut visited = HashSet::new();
    let mut temp_mark = HashSet::new();
    let mut output = Vec::new();

    fn visit<'a>(
        node: &str,
        node_path: &'a CanonicalPath,
        items: &'a HashMap<String, (CanonicalPath, CanonicalItemType<'a>)>,
        visited: &mut HashSet<String>,
        temp_mark: &mut HashSet<String>,
        output: &mut Vec<&'a CanonicalPath>,
    ) -> Result<(), Vec<String>> {
        if visited.contains(node) {
            return Ok(());
        }
        if temp_mark.contains(node) {
            // cycle detected
            return Err(vec![node.to_string()]);
        }

        temp_mark.insert(node.to_string());
        if let Some((_, item)) = items.get(node) {
            match item {
                CanonicalItemType::Import(import) => {
                    let key = import.to_string();
                    visit(&key, import, items, visited, temp_mark, output)?;
                }
                CanonicalItemType::Item(canonical_item) => {
                    if let Some(parent) = canonical_item.parent.as_ref() {
                        let key = parent.to_string();
                        visit(&key, parent, items, visited, temp_mark, output)?;
                    }
                }
                CanonicalItemType::StaticVariable(_, _) => {}
            }
        }
        temp_mark.remove(node);
        visited.insert(node.to_string());
        output.push(node_path);
        Ok(())
    }

    for (key, (path, _)) in items.iter() {
        if !visited.contains(key) {
            visit(key, path, &items, &mut visited, &mut temp_mark, &mut output)?;
        }
    }

    Ok(output)
}

pub fn transfer_env_recursive<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    path: &ModulePath,
    tree: &'a ParsedModuleTree,
    std_env: Gc<'a, Environment<'a>>,
) {
    let current_module_tree = ModuleTree::get(root.root_module, path)
        .expect(&format!("Module {} not found", path.to_string()));
    let current_env = current_module_tree.borrow().env;

    Environment::transfer(mc, std_env, current_env);

    for (name, child) in &tree.children {
        let child_path = path.push(name.clone());
        transfer_env_recursive(mc, root, &child_path, child, std_env);
    }
}
