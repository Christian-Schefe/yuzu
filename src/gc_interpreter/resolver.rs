use crate::{
    CanonicalPath, ModulePath, ParsedModuleTree, ParsedProgram,
    bytecode::{Instruction, compile},
    gc_interpreter::{
        Context, ModuleTree,
        standard::{define_globals, define_intrinsics},
    },
    location::Located,
    parser::{Expression, LocatedExpression, Pattern},
};

pub fn compile_and_setup<'a>(
    ctx: &Context<'a>,
    program: &mut ParsedProgram,
    code: &mut Vec<Located<Instruction>>,
    main_module: String,
) {
    let mut items = Vec::new();

    for (name, child) in &mut program.children {
        let root_path = ModulePath::from_root(name);
        collect_canonical_items(ctx, &mut items, &root_path, child);
    }

    // global functions available in all modules
    let global_env = ctx.root.global_env;
    define_globals(ctx, global_env);

    // instrinsic functions available as a separate module, used to build standard library
    let intrinsic_module =
        ModuleTree::get_or_insert(ctx, ctx.root.root_module, &ModulePath::intrinsics());
    define_intrinsics(ctx, intrinsic_module.borrow().env);

    for item in items {
        compile(&item, code);
    }

    let root_expressions = &program.children[&main_module].expressions;
    let first_location = root_expressions
        .first()
        .map(|e| e.location.clone())
        .unwrap();
    let last_location = root_expressions.last().map(|e| e.location.clone()).unwrap();

    code.push(Located::new(
        Instruction::InitializeModule(ModulePath::std()),
        first_location.clone(),
    ));

    code.push(Located::new(
        Instruction::InitializeModule(ModulePath::from_root(&main_module)),
        first_location.clone(),
    ));

    code.push(Located::new(Instruction::Exit, last_location));

    for (name, child) in &mut program.children {
        let root_path = ModulePath::from_root(name);
        compile_module_initializers(ctx, &root_path, child, code);
    }
}

fn collect_canonical_items<'a>(
    ctx: &Context<'a>,
    items: &mut Vec<LocatedExpression>,
    path: &ModulePath,
    tree: &mut ParsedModuleTree,
) {
    let _ = ModuleTree::get_or_insert(ctx, ctx.root.root_module, path);

    let expressions = std::mem::take(&mut tree.exported_expressions);
    for expr in expressions.into_iter() {
        match expr.data {
            Expression::Define { pattern, value } => {
                let name = match pattern.data {
                    Pattern::Ident(name) => name,
                    _ => panic!("Only simple identifiers are allowed in top-level definitions"),
                };
                let full_path = CanonicalPath::new(path.clone(), name);
                let expr = Located::new(
                    Expression::CanonicDefine {
                        name: full_path.clone(),
                        value,
                    },
                    expr.location,
                );
                items.push(expr);
            }
            _ => {
                panic!("Expression can't be exported");
            }
        }
    }
    for (name, child) in &mut tree.children {
        let child_path = path.push(name.clone());
        collect_canonical_items(ctx, items, &child_path, child);
    }
}

fn compile_module_initializers<'a>(
    ctx: &Context<'a>,
    path: &ModulePath,
    tree: &mut ParsedModuleTree,
    code: &mut Vec<Located<Instruction>>,
) {
    let module = ModuleTree::get(ctx.root.root_module, path).unwrap();

    let expressions = std::mem::take(&mut tree.expressions);
    if !expressions.is_empty() {
        let first_location = expressions.first().map(|e| e.location.clone()).unwrap();
        let last_location = expressions.last().map(|e| e.location.clone()).unwrap();
        let block = Located::new(
            Expression::Block(expressions.clone(), None),
            first_location.clone(),
        );
        let initializer = code.len();
        compile(&block, code);
        code.push(Located::new(Instruction::ExitFrame, last_location.clone()));
        module.borrow_mut(ctx.mc).initializer = Some(initializer);
    }

    for (name, child) in &mut tree.children {
        let child_path = path.push(name.clone());
        compile_module_initializers(ctx, &child_path, child, code);
    }
}
