use std::{collections::HashMap, path::PathBuf};

use clap::Parser;

use crate::{
    location::Located,
    package::{parse_fake_package, parse_package_root},
    parser::Expression,
};

mod bytecode;
mod gc_interpreter;
mod lexer;
mod location;
mod package;
mod parser;

#[derive(Debug, Parser)]
#[clap(trailing_var_arg = true)]
struct Args {
    input: PathBuf,
    other: Vec<String>,
}

#[tokio::main]
async fn main() {
    let args = match Args::try_parse() {
        Ok(a) => a,
        Err(e) => {
            e.print().expect("Error writing error");
            std::process::exit(1);
        }
    };

    if !args.input.exists() {
        eprintln!(
            "Input path does not exist: {}",
            args.input.to_string_lossy()
        );
        std::process::exit(1);
    }

    let mut expressions = Vec::new();
    let mut sources = HashMap::new();

    //We parse std first so its initializer is run before user code
    let std_path = PathBuf::from("./example/std");
    let Ok(_) = parse_package_root(&std_path, &mut expressions, &mut sources) else {
        std::process::exit(1);
    };

    let (pwd, main_module_name) = if args.input.is_file() {
        let pwd = args.input.parent().unwrap().to_path_buf();
        let Ok(main_module_name) = parse_fake_package(&args.input, &mut expressions, &mut sources)
        else {
            std::process::exit(1);
        };
        (pwd, main_module_name)
    } else {
        let pwd = args.input.clone();
        let Ok(main_module_name) = parse_package_root(&args.input, &mut expressions, &mut sources)
        else {
            std::process::exit(1);
        };
        (pwd, main_module_name)
    };

    let loc = expressions.first().unwrap().location.clone();
    let root_module = Located::new(Expression::ModuleLiteral { expressions }, loc);

    gc_interpreter::interpret_global(
        root_module,
        sources.into_iter().collect(),
        pwd,
        args.other,
        &main_module_name,
    )
    .await;
}
