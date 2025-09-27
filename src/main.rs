use std::io::Read;

use ariadne::{Color, Label, Report, ReportKind, Source};
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
    if let Err(err) = lexed {
        Report::build(ReportKind::Error, ((), err.span.clone()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(err.error.to_string())
            .with_label(
                Label::new(((), err.span))
                    .with_message(err.error.to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(Source::from(&input))
            .unwrap();
        std::process::exit(1);
    }
    let Some(parsed) = parser::parse(&input, lexed.unwrap()) else {
        std::process::exit(1);
    };
    println!("{}", parsed.expr);
    let interpreted = tree_interpreter::interpret_global(&parsed);
    if let Err(err) = interpreted {
        Report::build(ReportKind::Error, ((), err.span.clone()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(err.error.to_string())
            .with_label(
                Label::new(((), err.span))
                    .with_message(err.error.to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(Source::from(&input))
            .unwrap();
        std::process::exit(1);
    } else {
        println!("Program executed successfully");
    }
}
