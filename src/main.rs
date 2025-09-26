use std::io::Read;

use clap::Parser;
use clio::Input;

mod lexer;
mod parser;
mod tree_interpreter;

#[derive(Debug, Parser)]
struct Args {
    #[clap(value_parser)]
    input: Input,
}

fn main() {
    let mut args = match Args::try_parse() {
        Ok(a) => a,
        Err(e) => {
            e.print().expect("Error writing error");
            std::process::exit(1);
        }
    };
    let mut buf = Vec::new();
    let res = args.input.read_to_end(&mut buf);
    if let Err(e) = res {
        eprintln!("Error reading input: {}", e);
        std::process::exit(1);
    }
    let input = String::from_utf8_lossy(&buf);
    println!("{}", input);
    let lexed = lexer::lex(&input);
    println!("{:?}", lexed);
    let Some(parsed) = parser::parse(&input, lexed.expect("Lexing failed")) else {
        std::process::exit(1);
    };
    println!("{}", parsed);
    let interpreted = tree_interpreter::interpret_global(&parsed);
    if let Err(e) = interpreted {
        eprintln!("Runtime error: {:?}", e);
        std::process::exit(1);
    } else {
        println!("Program executed successfully");
    }
}
