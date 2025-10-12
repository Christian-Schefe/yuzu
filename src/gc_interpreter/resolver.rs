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

    let std_expressions = &program.children["std"].expressions;
    let root_expressions = &program.children[&main_module].expressions;
    let first_location = root_expressions
        .first()
        .map(|e| e.location.clone())
        .unwrap();
    let last_location = root_expressions.last().map(|e| e.location.clone()).unwrap();

    for item in items {
        compile(&item, code);
    }

    code.push(Located::new(
        Instruction::EnterModule(ModulePath::std()),
        first_location.clone(),
    ));
    let std_block = Located::new(
        Expression::Block(std_expressions.clone(), None),
        first_location.clone(),
    );
    compile(&std_block, code);
    code.push(Located::new(Instruction::ExitFrame, first_location.clone()));

    code.push(Located::new(
        Instruction::EnterModule(ModulePath::from_root(&main_module)),
        first_location.clone(),
    ));
    let root_block = Located::new(
        Expression::Block(root_expressions.clone(), None),
        first_location.clone(),
    );
    compile(&root_block, code);
    code.push(Located::new(Instruction::ExitFrame, first_location.clone()));

    code.push(Located::new(Instruction::Exit, last_location));
}

fn collect_canonical_items<'a>(
    ctx: &Context<'a>,
    items: &mut Vec<LocatedExpression>,
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
                let expr = Located::new(
                    Expression::CanonicDefine {
                        name: full_path.clone(),
                        value,
                    },
                    expr.location,
                );
                items.push(expr);
            }
            Expression::CanonicDefine { .. } => {
                panic!("CanonicDefine should not appear in source code");
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
