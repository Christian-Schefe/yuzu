use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use ariadne::{Color, Label, Report, ReportKind};
use serde::{Deserialize, Serialize};

use crate::{
    lexer::{self},
    location::{LineIndex, Located},
    parser::{self, Expression, LocatedExpression},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageConfig {
    pub name: String,
    pub version: String,
    pub dependencies: Vec<String>,
}

pub fn try_load_config(folder: &Path) -> Result<PackageConfig, String> {
    let config_path = folder.join("yuzuconf.json");
    if !config_path.exists() {
        return Err(format!(
            "No yuzuconf.json found in folder: {}",
            folder.to_string_lossy()
        ));
    }
    let content = std::fs::read_to_string(config_path).map_err(|e| e.to_string())?;
    let config: PackageConfig = serde_json::from_str(&content).map_err(|e| e.to_string())?;
    Ok(config)
}

pub fn parse_package_root(
    folder: &Path,
    module_tree: &mut Vec<LocatedExpression>,
    sources: &mut HashMap<String, String>,
) -> Result<String, ()> {
    let config = try_load_config(folder).map_err(|e| {
        eprintln!("Failed to load package config: {e}");
    })?;
    let mut visited_files = HashSet::new();
    let file_path = folder.join("src").join("main.yuzu");
    if !file_path.exists() {
        eprintln!(
            "No main.yuzu file found in package root: {}",
            folder.to_string_lossy()
        );
        return Err(());
    }
    visited_files.insert(
        file_path
            .canonicalize()
            .unwrap()
            .to_string_lossy()
            .to_string(),
    );
    let tree = parse_module_tree(&mut visited_files, &config.name, &file_path, sources)?;
    let loc = tree.location.clone();
    module_tree.push(Located::new(
        Expression::CanonicDefine {
            name: config.name.clone(),
            value: Box::new(tree),
        },
        loc,
    ));
    for dep_path in config.dependencies {
        let dep_folder = folder.join(dep_path);
        parse_package_root(&dep_folder, module_tree, sources)?;
    }
    Ok(config.name)
}

pub fn parse_fake_package(
    main_file: &Path,
    module_tree: &mut Vec<LocatedExpression>,
    sources: &mut HashMap<String, String>,
) -> Result<String, ()> {
    let mut visited_files = HashSet::new();
    if !main_file.exists() {
        eprintln!(
            "No main.yuzu file found in path: {}",
            main_file.to_string_lossy()
        );
        return Err(());
    }
    visited_files.insert(
        main_file
            .canonicalize()
            .unwrap()
            .to_string_lossy()
            .to_string(),
    );
    let tree = parse_module_tree(&mut visited_files, "root", main_file, sources)?;
    let loc = tree.location.clone();
    module_tree.push(Located::new(
        Expression::CanonicDefine {
            name: "root".to_string(),
            value: Box::new(tree),
        },
        loc,
    ));
    Ok("root".to_string())
}

fn parse_module_tree(
    visited_files: &mut HashSet<String>,
    root_name: &str,
    file_path: &Path,
    sources: &mut HashMap<String, String>,
) -> Result<LocatedExpression, ()> {
    let contents = std::fs::read_to_string(file_path).unwrap();
    let canonic_path_str = file_path
        .canonicalize()
        .unwrap()
        .to_string_lossy()
        .to_string();

    let pm = parse_string(
        &contents,
        root_name.to_string(),
        canonic_path_str.to_string(),
    )?;

    let mut expressions = Vec::new();

    let mut success = true;
    for mut item in pm {
        success &= item.replace_external_modules(|child, child_location| {
            let file_parent = file_path.parent().unwrap();
            let child_file_path = file_parent.join(format!("{}.yuzu", child.clone()));
            let child_file_path_str = child_file_path
                .canonicalize()
                .unwrap()
                .to_string_lossy()
                .to_string();

            if visited_files.contains(&child_file_path_str) {
                Report::build(
                    ReportKind::Error,
                    (child_location.file_path.clone(), child_location.span()),
                )
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(format!(
                    "Module imported multiple times: {child_file_path_str}"
                ))
                .with_label(
                    Label::new((child_location.file_path.clone(), child_location.span()))
                        .with_message("Module imported multiple times here")
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(ariadne::sources(vec![(
                    child_location.file_path.clone(),
                    &contents,
                )]))
                .unwrap();
                return None;
            }
            visited_files.insert(child_file_path_str);

            let child_tree = parse_module_tree(visited_files, root_name, &child_file_path, sources);
            let Ok(child_tree) = child_tree else {
                return None;
            };
            Some(child_tree)
        });
        expressions.push(item);
    }
    if !success {
        return Err(());
    }
    let first_loc = expressions.first().unwrap().location.clone();
    let last_loc = expressions.last().unwrap().location.clone();
    let module = Located::new(
        Expression::ModuleLiteral { expressions },
        first_loc.merge(&last_loc),
    );
    if sources.insert(canonic_path_str.clone(), contents).is_some() {
        panic!("Duplicate module path detected: {canonic_path_str}");
    }
    Ok(module)
}

fn parse_string(
    input: &str,
    root_name: String,
    file_path: String,
) -> Result<Vec<LocatedExpression>, ()> {
    let line_index = LineIndex::new(input);
    let lexed = lexer::lex(input, &line_index);
    if let Err(err) = lexed {
        Report::build(ReportKind::Error, (file_path.clone(), err.span.clone()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(err.error.to_string())
            .with_label(
                Label::new((file_path.clone(), err.span))
                    .with_message(err.error.to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(ariadne::sources(vec![(file_path.clone(), input)]))
            .unwrap();
        return Err(());
    }
    match parser::parse(input, root_name, lexed.unwrap()) {
        Err(errs) => {
            for err in errs {
                Report::build(
                    ReportKind::Error,
                    (file_path.clone(), err.span().into_range()),
                )
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(err.to_string())
                .with_label(
                    Label::new((file_path.clone(), err.span().into_range()))
                        .with_message(err.reason().to_string())
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(ariadne::sources(vec![(file_path.clone(), input)]))
                .unwrap();
            }
            Err(())
        }
        Ok(mut parsed) => {
            for expr in &mut parsed {
                expr.set_module(&file_path, &line_index);
            }
            Ok(parsed)
        }
    }
}
