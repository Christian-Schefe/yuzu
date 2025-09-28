use std::io::Read;

use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use clio::Input;

use crate::{parser::LocatedExpression, tree_interpreter::Location};

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

    let path = args
        .input
        .path()
        .to_path_buf()
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .to_path_buf();
    tree_interpreter::init_global_state(path);

    let mut buf = Vec::new();
    let res = args.input.read_to_end(&mut buf);
    if let Err(e) = res {
        eprintln!("Error reading input: {}", e);
        std::process::exit(1);
    }
    let input = String::from_utf8_lossy(&buf);
    let Ok(parsed) = parse_string(&input) else {
        std::process::exit(1);
    };

    let file_location_str = args.input.path().to_string_lossy().to_string();

    let add_file_info = |extra| Location::new(extra, file_location_str.clone());

    let parsed = parsed.map_extra(&add_file_info);
    let interpreted = tree_interpreter::interpret_global(&parsed, "root".to_string(), true);
    if let Err(err) = interpreted {
        Report::build(
            ReportKind::Error,
            (err.location.module.clone(), err.location.span.clone()),
        )
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(err.data.to_string())
        .with_label(
            Label::new((err.location.module, err.location.span))
                .with_message(err.data.to_string())
                .with_color(Color::Red),
        )
        .finish()
        .eprint(ariadne::sources(vec![
            (file_location_str.to_string(), input.as_ref()),
            ("std".to_string(), tree_interpreter::standard::STD),
        ]))
        .unwrap();
        std::process::exit(1);
    }
}

fn parse_string(input: &str) -> Result<LocatedExpression, ()> {
    let lexed = lexer::lex(input);
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
            .eprint(Source::from(input))
            .unwrap();
        return Err(());
    }
    let Some(parsed) = parser::parse(input, lexed.unwrap()) else {
        return Err(());
    };
    Ok(parsed)
}
