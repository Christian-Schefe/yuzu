use std::collections::{HashMap, HashSet};

use gc_arena::{Gc, Mutation};

use crate::{
    CanonicalPath, ModulePath, ParsedModuleTree,
    gc_interpreter::{
        ExecResult, ModuleTree, MyRoot, expression_to_function, make_class_literal,
        standard::define_globals,
        value::{Environment, Value},
    },
    parser::{Expression, Identifier, LocatedExpression},
};

pub struct CanonicalItem<'a> {
    expr: &'a LocatedExpression,
    parent: Option<CanonicalPath>,
}

pub enum CanonicalItemType<'a> {
    Import(CanonicalPath),
    Item(CanonicalItem<'a>),
}

pub fn resolve_canonical_paths<'a>(mc: &Mutation<'a>, root: &'a MyRoot<'a>) {
    let mut items = Vec::new();

    for (name, child) in &root.program.children {
        let root_path = ModulePath::from_root(name);
        collect_canonical_items(&mut items, &root_path, child);
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
    let std_child = &root.program.children["root"];
    let std_path = ModulePath::from_root("std");
    transfer_env_recursive(mc, root, &std_path, std_child, global_env);

    for path in &order {
        if path.path.get_root() != "std" {
            continue;
        }
        let Some((_, item)) = item_map.get(&path.to_string()) else {
            panic!("Item not found for {}", path);
        };
        define_item(mc, root, path, item);
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
        let Some((_, item)) = item_map.get(&path.to_string()) else {
            panic!("Item not found for {}", path);
        };
        define_item(mc, root, path, item);
    }
}

fn define_item<'a>(
    mc: &Mutation<'a>,
    root: &'a MyRoot<'a>,
    path: &CanonicalPath,
    item: &CanonicalItemType<'a>,
) {
    let module_tree = ModuleTree::get(root.root_module, &path.path).unwrap();
    let env = module_tree.borrow().env;
    match item {
        CanonicalItemType::Import(import) => {
            let source_module_tree = ModuleTree::get(root.root_module, &import.path);
            if let Some(source_module_tree) = source_module_tree {
                let source_env = source_module_tree.borrow().env;
                if let Some(val) = source_env.get(&import.item) {
                    env.define(mc, &import.item, val);
                    println!("Imported {} in {}", import.item, path.path.to_string());
                } else {
                    panic!("Imported item not found: {}", import.item);
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
            tree.env.define(mc, &path.item, value);
            println!("Defined {} in {}", path.item, path.path.to_string());
        }
    }
}

fn collect_canonical_items<'a>(
    items: &mut Vec<(CanonicalPath, CanonicalItemType<'a>)>,
    path: &ModulePath,
    tree: &'a ParsedModuleTree,
) {
    for import in tree.imports.iter() {
        items.push((
            CanonicalPath::new(path.clone(), import.data.item.clone()),
            CanonicalItemType::Import(import.data.clone()),
        ));
    }
    for expr in tree.expressions.iter() {
        match &expr.data {
            Expression::Define {
                name,
                value,
                type_hint: _,
            } => match &value.data {
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
            _ => {}
        }
    }
    for (name, child) in &tree.children {
        let child_path = path.push(name.clone());
        collect_canonical_items(items, &child_path, child);
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
    println!("Loading std into {}", path);
    let current_module_tree = ModuleTree::get_or_insert(mc, root.root_module, path);
    let current_env = current_module_tree.borrow().env;

    Environment::transfer(mc, std_env, current_env);

    for (name, child) in &tree.children {
        let child_path = path.push(name.clone());
        transfer_env_recursive(mc, root, &child_path, child, std_env);
    }
}
