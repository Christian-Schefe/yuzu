use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use ariadne::{Color, Label, Report, ReportKind};
use serde::{Deserialize, Serialize};

use crate::{
    ModulePath,
    lexer::{self},
    location::{LineIndex, Located},
    parser::{self, Expression, LocatedExpression, ParsedModuleItem},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageConfig {
    pub name: String,
    pub version: String,
    pub dependencies: Vec<String>,
}

pub fn try_load_config(folder: &PathBuf) -> Result<PackageConfig, String> {
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
    folder: &PathBuf,
    module_tree: &mut Vec<LocatedExpression>,
    sources: &mut HashMap<String, String>,
) -> Result<String, ()> {
    let config = try_load_config(folder).map_err(|e| {
        eprintln!("Failed to load package config: {}", e);
    })?;
    let mut visited_files = HashSet::new();
    let root_path = ModulePath::from_root(&config.name);
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
    let tree = parse_module_tree(&mut visited_files, root_path.clone(), &file_path, sources)?;
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
    main_file: &PathBuf,
    module_tree: &mut Vec<LocatedExpression>,
    sources: &mut HashMap<String, String>,
) -> Result<String, ()> {
    let mut visited_files = HashSet::new();
    let root_path = ModulePath::from_root("root");
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
    let tree = parse_module_tree(&mut visited_files, root_path.clone(), main_file, sources)?;
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
    path: ModulePath,
    file_path: &PathBuf,
    sources: &mut HashMap<String, String>,
) -> Result<LocatedExpression, ()> {
    let contents = std::fs::read_to_string(&file_path).unwrap();
    let canonic_path_str = file_path.to_string_lossy().to_string();
    let pm = parse_string(
        &contents,
        path.get_root().to_string(),
        path.to_string(),
        &canonic_path_str,
    )?;

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
        let file_parent = file_path.parent().unwrap();
        let child_file_path = file_parent.join(format!("{}.yuzu", child.data.clone()));
        let child_file_path_str = child_file_path
            .canonicalize()
            .unwrap()
            .to_string_lossy()
            .to_string();

        if visited_files.contains(&child_file_path_str) {
            Report::build(
                ReportKind::Error,
                (child.location.module.clone(), child.location.span()),
            )
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(format!(
                "Module imported multiple times: {}",
                child_module_path
            ))
            .with_label(
                Label::new((child.location.module.clone(), child.location.span()))
                    .with_message("Module imported multiple times here")
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(ariadne::sources(vec![(child.location.module, &contents)]))
            .unwrap();
            success = false;
            continue;
        }
        visited_files.insert(child_file_path_str);

        let child_tree =
            parse_module_tree(visited_files, child_module_path, &child_file_path, sources);
        let Ok(child_tree) = child_tree else {
            success = false;
            continue;
        };
        let module_expr = Located::new(
            Expression::CanonicDefine {
                name: child.data,
                value: Box::new(child_tree),
            },
            child.location.clone(),
        );
        expressions.push(module_expr);
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
    if sources.insert(path.to_string(), contents).is_some() {
        panic!("Duplicate module path detected: {}", path);
    }
    Ok(module)
}

fn parse_string(
    input: &str,
    root_name: String,
    module_path: String,
    file_path: &str,
) -> Result<Vec<ParsedModuleItem>, ()> {
    let line_index = LineIndex::new(input);
    let lexed = lexer::lex(input, &line_index);
    if let Err(err) = lexed {
        Report::build(ReportKind::Error, (module_path.clone(), err.span.clone()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(err.error.to_string())
            .with_label(
                Label::new((module_path.clone(), err.span))
                    .with_message(err.error.to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .eprint(ariadne::sources(vec![(module_path.clone(), input)]))
            .unwrap();
        return Err(());
    }
    match parser::parse(
        input,
        &line_index,
        root_name,
        &module_path,
        file_path,
        lexed.unwrap(),
    ) {
        Err(errs) => {
            for err in errs {
                Report::build(
                    ReportKind::Error,
                    (module_path.clone(), err.span().into_range()),
                )
                .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                .with_message(err.to_string())
                .with_label(
                    Label::new((module_path.clone(), err.span().into_range()))
                        .with_message(err.reason().to_string())
                        .with_color(Color::Red),
                )
                .finish()
                .eprint(ariadne::sources(vec![(module_path.clone(), input)]))
                .unwrap();
            }
            Err(())
        }
        Ok(parsed) => Ok(parsed),
    }
}
