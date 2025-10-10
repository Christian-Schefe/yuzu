use std::collections::{HashMap, HashSet};

use gc_arena::{Gc, lock::GcRefLock};

use crate::{
    CanonicalPath, ModulePath, ParsedModuleTree, ParsedProgram,
    bytecode::{Instruction, compile},
    gc_interpreter::{
        Context, ModuleTree,
        standard::define_globals,
        value::{ClassValue, Environment},
    },
    location::Located,
    parser::{Expression, Identifier, LocatedExpression, Pattern},
};

pub struct CanonicalItem {
    expr: LocatedExpression,
    parent: Option<CanonicalPath>,
}

pub fn resolve_canonical_paths<'a>(
    ctx: &Context<'a>,
    program: &mut ParsedProgram,
    code: &mut Vec<Located<Instruction>>,
) {
    let mut items = Vec::new();

    for (name, child) in &mut program.children {
        let root_path = ModulePath::from_root(name);
        collect_canonical_items(ctx, &mut items, &root_path, child);
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

    let global_env = ctx.gc(Environment::new_global(ctx.mc));
    define_globals(ctx, global_env);

    for (name, child) in &program.children {
        let root_path = ModulePath::from_root(name);
        transfer_env_recursive(ctx, &root_path, child, global_env);
    }

    let root_expr_tree = &program.children["root"];
    let expressions = &root_expr_tree.expressions;
    let last_location = expressions.last().map(|e| e.location.clone()).unwrap();

    for path in &order {
        let Some((_, item)) = item_map.get(&path.to_string()) else {
            panic!("Item not found for {}", path);
        };
        compile(&item.expr, code);
    }

    for expr in expressions.iter() {
        compile(expr, code);
    }
    code.push(Located::new(Instruction::Exit, last_location));
}

fn merge_std_classes<'a>(
    ctx: &Context<'a>,
    from_class: GcRefLock<'a, ClassValue<'a>>,
    to_class: GcRefLock<'a, ClassValue<'a>>,
) {
    let from_ref = from_class.borrow();
    let mut to_ref = to_class.borrow_mut(ctx.mc);
    for (name, method) in from_ref.methods.iter() {
        to_ref.methods.insert(name.clone(), *method);
    }
    for (name, method) in from_ref.static_methods.iter() {
        to_ref.static_methods.insert(name.clone(), *method);
    }
    if from_ref.constructor.is_some() {
        to_ref.constructor = from_ref.constructor;
    }
}

fn collect_canonical_items<'a>(
    ctx: &Context<'a>,
    items: &mut Vec<(CanonicalPath, CanonicalItem)>,
    path: &ModulePath,
    tree: &mut ParsedModuleTree,
) {
    let _ = ModuleTree::get_or_insert(ctx, ctx.root.root_module, path);

    let expressions = std::mem::take(&mut tree.expressions);
    let mut unused_expressions = Vec::new();
    for expr in expressions.into_iter() {
        match expr.data {
            Expression::Define { pattern, value } => {
                let name = match pattern.data {
                    Pattern::Ident(name) => name,
                    _ => panic!("Only simple identifiers are allowed in top-level definitions"),
                };
                let full_path = CanonicalPath::new(path.clone(), name);
                let parent = match &value.data {
                    Expression::FunctionLiteral { .. } => None,
                    Expression::ClassLiteral { parent, .. } => parent.clone().map(|p| match p {
                        Identifier::Simple(name) => CanonicalPath::new(path.clone(), name.clone()),
                        Identifier::Scoped(path) => path,
                    }),
                    _ => None,
                };
                let expr = Located::new(
                    Expression::CanonicDefine {
                        name: full_path.clone(),
                        value,
                    },
                    expr.location,
                );
                items.push((full_path, CanonicalItem { expr, parent }));
            }
            Expression::CanonicDefine { ref name, .. } => {
                items.push((name.clone(), CanonicalItem { expr, parent: None }));
            }
            other => {
                unused_expressions.push(Located::new(other, expr.location));
            }
        }
    }
    tree.expressions = unused_expressions;
    for (name, child) in &mut tree.children {
        let child_path = path.push(name.clone());
        collect_canonical_items(ctx, items, &child_path, child);
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

pub fn transfer_env_recursive<'a>(
    ctx: &Context<'a>,
    path: &ModulePath,
    tree: &ParsedModuleTree,
    std_env: Gc<'a, Environment<'a>>,
) {
    let current_module_tree = ModuleTree::get(ctx.root.root_module, path)
        .expect(&format!("Module {} not found", path.to_string()));
    let current_env = current_module_tree.borrow().env;

    Environment::transfer(ctx, std_env, current_env);

    for (name, child) in &tree.children {
        let child_path = path.push(name.clone());
        transfer_env_recursive(ctx, &child_path, child, std_env);
    }
}
