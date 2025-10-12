use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use ariadne::{Color, Label, Report, ReportKind};
use serde::{Deserialize, Serialize};

use crate::{
    ModulePath, ParsedModuleTree, lexer,
    parser::{self, ParsedModuleItem},
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
    module_tree: &mut HashMap<String, ParsedModuleTree>,
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
    let tree = parse_module_tree(&mut visited_files, root_path.clone(), &file_path)?;
    module_tree.insert(config.name.clone(), tree);
    for dep_path in config.dependencies {
        let dep_folder = folder.join(dep_path);
        parse_package_root(&dep_folder, module_tree)?;
    }
    Ok(config.name)
}

pub fn parse_fake_package(
    main_file: &PathBuf,
    module_tree: &mut HashMap<String, ParsedModuleTree>,
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
    let tree = parse_module_tree(&mut visited_files, root_path.clone(), main_file)?;
    module_tree.insert("root".to_string(), tree);
    Ok("root".to_string())
}

fn parse_module_tree(
    visited_files: &mut HashSet<String>,
    path: ModulePath,
    file_path: &PathBuf,
) -> Result<ParsedModuleTree, ()> {
    let contents = std::fs::read_to_string(&file_path).unwrap();
    let pm = parse_string(&contents, path.get_root().to_string(), path.to_string())?;
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
        let file_parent = file_path.parent().unwrap();
        let child_file_path = file_parent.join(format!("{}.yuzu", child.data));
        let child_file_path_str = child_file_path
            .canonicalize()
            .unwrap()
            .to_string_lossy()
            .to_string();

        if visited_files.contains(&child_file_path_str) {
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
                Label::new((child.location.module.clone(), child.location.span))
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
    root_name: String,
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
    match parser::parse(input, root_name, &location, lexed.unwrap()) {
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
