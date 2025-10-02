use std::io::Read;

use ariadne::{Color, Label, Report, ReportKind};
use clap::Parser;
use clio::Input;

use crate::parser::LocatedExpression;

mod gc_interpreter;
mod lexer;
mod location;
mod parser;
mod type_checker;

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

    let mut buf = Vec::new();
    let res = args.input.read_to_end(&mut buf);
    if let Err(e) = res {
        eprintln!("Error reading input: {}", e);
        std::process::exit(1);
    }
    let input = String::from_utf8_lossy(&buf);
    let file_location_str = args.input.path().to_string_lossy().to_string();
    let Ok(mut parsed) = parse_string(&input, Some(&file_location_str)) else {
        std::process::exit(1);
    };
    parsed.set_module(&file_location_str);

    let errors = type_checker::check_types(&parsed, "root".to_string());
    if !errors.is_empty() {
        for err in errors {
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
            .eprint(ariadne::sources(vec![(
                file_location_str.to_string(),
                input.as_ref(),
            )]))
            .unwrap();
        }
        std::process::exit(1);
    }

    let interpreted = gc_interpreter::interpret_global(parsed, "root".to_string(), path);
    if let Err(err) = interpreted {
        let err_message = err.data;
        Report::build(
            ReportKind::Error,
            (err.location.module.clone(), err.location.span.clone()),
        )
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(err_message.clone())
        .with_label(
            Label::new((err.location.module, err.location.span))
                .with_message(err_message)
                .with_color(Color::Red),
        )
        .finish()
        .eprint(ariadne::sources(vec![
            (file_location_str.to_string(), input.as_ref()),
            ("std".to_string(), gc_interpreter::standard::STD),
        ]))
        .unwrap();
        std::process::exit(1);
    }
}

fn parse_string(input: &str, location: Option<&str>) -> Result<LocatedExpression, ()> {
    let lexed = lexer::lex(input);
    let location = location.unwrap_or("<input>").to_string();
    if let Err(err) = lexed {
        Report::build(ReportKind::Error, (location.clone(), err.span.clone()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(err.error.to_string())
            .with_label(
                Label::new((location.clone(), err.span))
                    .with_message(err.error.to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(ariadne::sources(vec![(location.clone(), input)]))
            .unwrap();
        return Err(());
    }
    match parser::parse(input, lexed.unwrap()) {
        Err(errs) => {
            for err in errs {
                Report::build(
                    ReportKind::Error,
                    (location.clone(), err.span().into_range()),
                )
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(err.to_string())
                .with_label(
                    Label::new((location.clone(), err.span().into_range()))
                        .with_message(err.reason().to_string())
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(ariadne::sources(vec![(location.clone(), input)]))
                .unwrap();
            }
            Err(())
        }
        Ok(parsed) => Ok(parsed),
    }
}
