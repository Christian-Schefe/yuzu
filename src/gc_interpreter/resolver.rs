use crate::{
    bytecode::{Instruction, compile},
    gc_interpreter::{
        Context,
        standard::{define_globals, define_intrinsics},
        value::{ModuleValue, Value},
    },
    location::Located,
    parser::LocatedExpression,
};

pub fn compile_and_setup<'a>(
    ctx: &Context<'a>,
    program: LocatedExpression,
    code: &mut Vec<Located<Instruction>>,
    main_module_name: &str,
) {
    // global functions available in all modules
    let global_env = ctx.root.global_env;
    define_globals(ctx, global_env);

    // instrinsic functions available as a separate module, used to build standard library
    let intrinsic_module = ModuleValue::new(ctx.mc, ctx.root.global_env);
    define_intrinsics(ctx, intrinsic_module.env);
    if !ctx.root.global_env.define_const(
        ctx,
        "intrinsics",
        Value::Module(ctx.gc_lock(intrinsic_module)),
    ) {
        panic!("Failed to create intrinsics module");
    }

    compile(&program, code);
    code.push(Located::new(
        Instruction::SetRootModule,
        program.location.clone(),
    ));
    code.push(Located::new(
        Instruction::InitModule,
        program.location.clone(),
    ));
    code.push(Located::new(Instruction::Pop, program.location.clone()));

    for module in ["std", main_module_name] {
        code.push(Located::new(
            Instruction::Load(module.to_string()),
            program.location.clone(),
        ));
        code.push(Located::new(
            Instruction::InitModule,
            program.location.clone(),
        ));
        code.push(Located::new(Instruction::Pop, program.location.clone()));
    }

    code.push(Located::new(
        Instruction::PushNull,
        program.location.clone(),
    ));
    code.push(Located::new(Instruction::Exit, program.location.clone()));
}
