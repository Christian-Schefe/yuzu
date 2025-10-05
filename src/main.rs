use std::collections::{HashMap, HashSet};

use ariadne::{Color, Label, Report, ReportKind};
use clap::Parser;
use clio::Input;

use crate::{
    location::Located,
    parser::{LocatedExpression, ParsedModule},
};

mod gc_interpreter;
mod lexer;
mod location;
mod parser;

#[derive(Debug, Parser)]
struct Args {
    #[clap(value_parser)]
    input: Input,
}

fn main() {
    let args = match Args::try_parse() {
        Ok(a) => a,
        Err(e) => {
            e.print().expect("Error writing error");
            std::process::exit(1);
        }
    };

    let pwd = args
        .input
        .path()
        .to_path_buf()
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .to_path_buf();

    let file_location_str = args.input.path().to_string_lossy().to_string();

    let mut visited_files = HashSet::new();
    let Ok(parsed) = parse_module_tree(&mut visited_files, ModulePath::root(), &file_location_str)
    else {
        std::process::exit(1);
    };
    println!("Parsed successfully: {:#?}", parsed);
    /*
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
    } */
}

fn parse_module_tree(
    visited_files: &mut HashSet<String>,
    path: ModulePath,
    file_path: &str,
) -> Result<ParsedModuleTree, ()> {
    let contents = std::fs::read_to_string(&file_path).unwrap();
    let pm = parse_string(&contents, file_path.to_string())?;
    let mut children = HashMap::new();
    let mut success = true;
    for child in pm.children {
        let child_module_path = path.push(child.data.clone());
        let file_parent = std::path::Path::new(&file_path).parent().unwrap();
        let child_file_path = file_parent.join(format!("{}.yuzu", child.data));
        let child_file_path = child_file_path.to_string_lossy().to_string();

        if visited_files.contains(&child_file_path) {
            Report::build(
                ReportKind::Error,
                (child.location.module.clone(), child.location.span.clone()),
            )
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(format!(
                "Module imported multiple times: {}",
                child_module_path
            ))
            .with_label(
                Label::new((child.location.module, child.location.span))
                    .with_message("Module imported multiple times here")
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(ariadne::sources(vec![(file_path.to_string(), &contents)]))
            .unwrap();
            success = false;
            continue;
        }
        visited_files.insert(child_file_path.clone());

        let child_tree = parse_module_tree(visited_files, child_module_path, &child_file_path);
        let Ok(child_tree) = child_tree else {
            success = false;
            continue;
        };
        children.insert(child.data.clone(), child_tree);
    }
    if !success {
        return Err(());
    }
    Ok(ParsedModuleTree {
        children,
        expressions: pm.expressions,
        imports: pm.imports,
    })
}

fn parse_string(input: &str, location: String) -> Result<ParsedModule, ()> {
    let lexed = lexer::lex(input);
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
    match parser::parse(input, &location, lexed.unwrap()) {
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

#[derive(Clone, Debug)]
pub struct ModulePath {
    path: Vec<String>,
    root: Option<String>,
}

impl std::fmt::Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let path_str = self
            .path
            .iter()
            .map(|s| format!("::{}", s))
            .collect::<String>();
        if let Some(root) = &self.root {
            write!(f, "{}{}", root, path_str)
        } else {
            write!(f, "root{}", path_str)
        }
    }
}

impl ModulePath {
    pub fn root() -> Self {
        Self {
            path: Vec::new(),
            root: None,
        }
    }
    pub fn from_root(root: &str, path: Vec<String>) -> Self {
        Self {
            path,
            root: if root == "root" {
                None
            } else {
                Some(root.to_string())
            },
        }
    }
    pub fn new(path: Vec<String>) -> Self {
        let mut path = path.into_iter();
        let root = path.next().unwrap();
        let path = path.collect::<Vec<_>>();
        Self::from_root(&root, path)
    }
    pub fn push(&self, segment: String) -> Self {
        let mut new_path = self.path.clone();
        new_path.push(segment);
        Self {
            path: new_path,
            root: self.root.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ParsedModuleTree {
    children: std::collections::HashMap<String, ParsedModuleTree>,
    expressions: Vec<LocatedExpression>,
    imports: Vec<Located<ModulePath>>,
}
