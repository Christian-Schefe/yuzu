use std::{
    collections::{HashMap, HashSet},
    vec,
};

use ariadne::{Color, Label, Report, ReportKind};
use clap::Parser;
use clio::Input;

use crate::parser::{LocatedExpression, ParsedModuleItem};

mod bytecode;
mod gc_interpreter;
mod lexer;
mod location;
mod parser;

#[derive(Debug, Parser)]
#[clap(trailing_var_arg = true)]
struct Args {
    #[clap(value_parser)]
    input: Input,
    other: Vec<String>,
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
    let Ok(parsed_std) = parse_module_tree(
        &mut visited_files,
        ModulePath::from_root("std"),
        "./data2/std.yuzu",
    ) else {
        std::process::exit(1);
    };

    let parsed = ParsedProgram {
        children: HashMap::from([
            ("root".to_string(), parsed),
            ("std".to_string(), parsed_std),
        ]),
    };

    let sources = parsed.get_sources();

    let interpreted = gc_interpreter::interpret_global(parsed, pwd, args.other);
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
        .eprint(ariadne::sources(sources))
        .unwrap();
        std::process::exit(1);
    }
}

fn parse_module_tree(
    visited_files: &mut HashSet<String>,
    path: ModulePath,
    file_path: &str,
) -> Result<ParsedModuleTree, ()> {
    let contents = std::fs::read_to_string(&file_path).unwrap();
    let pm = parse_string(&contents, path.clone(), path.to_string())?;
    let mut children = HashMap::new();
    let mut expressions = Vec::new();

    let mut success = true;
    for item in pm {
        let child = match item {
            ParsedModuleItem::Expression(e) => {
                expressions.push(e);
                continue;
            }
            ParsedModuleItem::Module(m) => m,
        };
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
        expressions,
        source: contents,
    })
}

fn parse_string(
    input: &str,
    module_path: ModulePath,
    location: String,
) -> Result<Vec<ParsedModuleItem>, ()> {
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
    match parser::parse(input, module_path, &location, lexed.unwrap()) {
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
}

impl std::fmt::Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path.join("::"))
    }
}

impl ModulePath {
    pub fn root() -> Self {
        Self {
            path: vec!["root".to_string()],
        }
    }
    pub fn get_root(&self) -> &str {
        &self.path[0]
    }
    pub fn from_root(root: &str) -> Self {
        Self {
            path: vec![root.to_string()],
        }
    }
    pub fn new(path: Vec<String>) -> Self {
        if path.is_empty() {
            panic!("ModulePath cannot be empty");
        }
        Self { path }
    }
    pub fn push(&self, segment: String) -> Self {
        let mut new_path = self.path.clone();
        new_path.push(segment);
        Self { path: new_path }
    }
}

#[derive(Clone, Debug)]
pub struct CanonicalPath {
    pub path: ModulePath,
    pub item: String,
}

impl std::fmt::Display for CanonicalPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.path, self.item)
    }
}

impl CanonicalPath {
    pub fn to_string(&self) -> String {
        format!("{}::{}", self.path, self.item)
    }
}

impl CanonicalPath {
    pub fn new(path: ModulePath, item: String) -> Self {
        Self { path, item }
    }
}

#[derive(Debug)]
pub struct ParsedModuleTree {
    source: String,
    children: HashMap<String, ParsedModuleTree>,
    expressions: Vec<LocatedExpression>,
}

impl ParsedModuleTree {
    pub fn get_sources(&self, path: &ModulePath, sources: &mut Vec<(String, String)>) {
        sources.push((path.to_string(), self.source.clone()));
        for (name, child) in &self.children {
            child.get_sources(&path.push(name.clone()), sources);
        }
    }
}

pub struct ParsedProgram {
    children: HashMap<String, ParsedModuleTree>,
}

impl ParsedProgram {
    pub fn get_sources(&self) -> Vec<(String, String)> {
        let mut sources = Vec::new();
        for (name, child) in &self.children {
            child.get_sources(&ModulePath::from_root(name), &mut sources);
        }
        sources
    }
}
