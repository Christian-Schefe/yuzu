use std::collections::{HashMap, HashSet};

use gc_arena::Mutation;

use crate::{
    ModulePath, ParsedModuleTree,
    gc_interpreter::{
        ModuleTree, MyRoot, expression_to_function, make_class_literal, value::Value,
    },
    parser::{Expression, Identifier, LocatedExpression},
};

pub struct CanonicalPath {
    pub path: ModulePath,
    pub item: String,
}

impl CanonicalPath {
    pub fn to_string(&self) -> String {
        format!("{}::{}", self.path, self.item)
    }
}

impl CanonicalPath {
    pub fn new(path: ModulePath, item: String) -> Self {
        Self { path, item }
    }
}

pub struct CanonicalItem<'a> {
    expr: &'a LocatedExpression,
    parent: Option<CanonicalPath>,
}

pub fn resolve_canonical_paths<'a>(
    mc: &Mutation<'a>,
    root: &MyRoot<'a>,
    tree: &'a ParsedModuleTree,
) {
    let mut items = Vec::new();
    let root_path = ModulePath::root();
    collect_canonical_items(&mut items, &root_path, tree);
    let item_map = items
        .into_iter()
        .map(|(k, v)| (k.to_string(), (k, v)))
        .collect();
    let order = match topo_sort(&item_map) {
        Ok(order) => order,
        Err(cycle) => {
            panic!("Cycle detected in class inheritance: {:?}", cycle);
        }
    };
    for path in order {
        let (_, item) = item_map.get(&path.to_string()).unwrap();
        let module_tree = ModuleTree::get_or_insert(mc, root.root_module, &path.path);
        let env = module_tree.borrow().env;
        let value = match &item.expr.data {
            Expression::FunctionLiteral { .. } => {
                Value::Function(expression_to_function(mc, item.expr, env))
            }
            Expression::ClassLiteral { parent, .. } => {
                let parent_val = parent.as_ref().map(|p| match p {
                    Identifier::Scoped(path, name) => ModuleTree::get(root.root_module, path)
                        .unwrap()
                        .borrow()
                        .env
                        .get(name)
                        .unwrap(),
                    Identifier::Simple(name) => env.get(name).unwrap(),
                });
                Value::Class(make_class_literal(mc, root, &item.expr, env, parent_val))
            }
            _ => unreachable!(),
        };
        let tree = module_tree.borrow();
        tree.env.define(mc, &path.item, value);
    }
}

fn collect_canonical_items<'a>(
    items: &mut Vec<(CanonicalPath, CanonicalItem<'a>)>,
    path: &ModulePath,
    tree: &'a ParsedModuleTree,
) {
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
                        CanonicalItem { expr, parent: None },
                    ));
                }
                Expression::ClassLiteral { parent, .. } => {
                    items.push((
                        CanonicalPath::new(path.clone(), name.clone()),
                        CanonicalItem {
                            expr,
                            parent: parent.clone().map(|p| match p {
                                Identifier::Simple(name) => CanonicalPath::new(path.clone(), name),
                                Identifier::Scoped(path, name) => CanonicalPath::new(path, name),
                            }),
                        },
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
    items: &'a HashMap<String, (CanonicalPath, CanonicalItem)>,
) -> Result<Vec<&'a CanonicalPath>, Vec<String>> {
    let mut visited = HashSet::new();
    let mut temp_mark = HashSet::new();
    let mut output = Vec::new();

    fn visit<'a>(
        node: &str,
        node_path: &'a CanonicalPath,
        items: &'a HashMap<String, (CanonicalPath, CanonicalItem)>,
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
            if let Some(parent) = item.parent.as_ref() {
                let key = parent.to_string();
                visit(&key, parent, items, visited, temp_mark, output)?;
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
