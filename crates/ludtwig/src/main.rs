extern crate core;

use std::path::PathBuf;
use std::sync::mpsc::Sender;
use std::sync::{mpsc, Arc};
use std::thread;

use crate::check::rule::{RuleDefinition, Severity};
use crate::check::rules::get_config_active_rule_definitions;
use clap::Parser;
use walkdir::{DirEntry, WalkDir};

use crate::config::Config;
use crate::output::ProcessingEvent;

mod check;
mod config;
mod error;
mod output;
mod process;

/// A CLI tool for template files (Twig + HTML) with focus on formatting and detecting mistakes.
#[derive(Parser, Debug, Clone)]
#[clap(author = env!("CARGO_PKG_AUTHORS"))]
pub struct Opts {
    /// Files or directories to scan
    #[clap(
        value_name = "FILE",
        min_values = 1,
        required = true,
        conflicts_with = "create_config",
        parse(from_os_str),
        name = "files"
    )]
    files: Vec<PathBuf>,

    /// Apply all code suggestions automatically. This changes the original files!
    #[clap(short = 'f', long)]
    fix: bool,

    /// Print out the parsed syntax tree for each file
    #[clap(short = 'i', long)]
    inspect: bool,

    /// Specify where the ludtwig configuration file is. Ludtwig looks in the current directory for a 'ludtwig-config.toml' by default.
    #[clap(short = 'c', long, parse(from_os_str))]
    config_path: Option<PathBuf>,

    /// Create the default configuration file in the config path. Defaults to the current directory.
    #[clap(short = 'C', long, name = "create_config")]
    create_config: bool,
}

/// Context to pass to every processing thead (can be cloned)
#[derive(Debug)]
pub struct CliContext {
    /// Channel sender for transmitting messages back to the CLI.
    pub output_tx: Sender<ProcessingEvent>,
    /// Shared Data
    pub data: Arc<CliSharedData>,
}

#[derive(Debug)]
pub struct CliSharedData {
    /// Apply all code suggestions automatically. This changes the original files!
    pub fix: bool,
    /// Print out the parsed syntax tree for each file
    pub inspect: bool,
    /// The config values to use.
    pub config: Config,
    /// Config active rule definitions
    pub rule_definitions: Vec<Arc<RuleDefinition>>,
}

impl Clone for CliContext {
    fn clone(&self) -> Self {
        Self {
            output_tx: self.output_tx.clone(),
            data: Arc::clone(&self.data),
        }
    }
}

impl CliContext {
    pub fn send_processing_output(&self, event: ProcessingEvent) {
        self.output_tx
            .send(event)
            .expect("output should still receive ProcessingEvents");
    }
}

/// Parse the CLI arguments and bootstrap the application.
fn main() {
    let opts: Opts = Opts::from_args();
    let config = config::handle_config_or_exit(&opts);

    let process_code = app(opts, config);
    std::process::exit(process_code);
}

/// The entry point of the async application.
fn app(opts: Opts, config: Config) -> i32 {
    println!("Scanning files...");

    // sender and receiver channels for the communication between tasks and the user.
    let (tx, rx) = mpsc::channel();

    // construct active rules
    let active_rules = match get_config_active_rule_definitions(&config) {
        Ok(rules) => rules,
        Err(e) => {
            println!("Error: {}", e);
            return 1;
        }
    };

    let cli_context = CliContext {
        output_tx: tx,
        data: Arc::new(CliSharedData {
            fix: opts.fix,
            inspect: opts.inspect,
            config,
            rule_definitions: active_rules,
        }),
    };

    let output_handler = thread::spawn(|| output::handle_processing_output(rx));

    // work on each user specified file / directory path concurrently
    opts.files.into_iter().for_each(|path| {
        handle_input_path(path, cli_context.clone());
    });

    drop(cli_context); // drop this tx channel

    // the output_handler will finish execution if all the tx (sending channel) ends are closed.
    output_handler
        .join()
        .expect("Error: can't join output_handler thread")
}

/// filters out hidden directories or files
/// (that start with '.').
fn is_not_hidden(entry: &DirEntry) -> bool {
    let name = entry.file_name().to_string_lossy();

    // '.' and './' is a valid path for the current working directory and not an hidden file / dir
    // otherwise anything that starts with a '.' is considered hidden for ludtwig
    !name.starts_with('.') || name == "." || name == "./"
}

/// Process a directory path.
fn handle_input_path(path: PathBuf, cli_context: CliContext) {
    let walker = WalkDir::new(path).into_iter();

    // synchronous directory traversal but move the work for each file to a different thread in the thread pool.
    rayon::scope(move |s| {
        for entry in walker.filter_entry(is_not_hidden) {
            let entry = match entry {
                Ok(e) => e,
                Err(e) => {
                    println!("Error: walking over the file path: {}", e);
                    cli_context.send_processing_output(ProcessingEvent::Report(Severity::Error));
                    continue;
                }
            };

            if !entry.file_type().is_file() {
                continue;
            }

            let name = entry.file_name().to_string_lossy();

            if !name.ends_with(".twig") && !name.ends_with(".html") {
                continue;
            }

            let clone = cli_context.clone();
            let tx_clone = cli_context.output_tx.clone();
            s.spawn(
                move |_s1| match process::process_file(entry.path().into(), clone) {
                    Ok(_) => {}
                    Err(e) => {
                        tx_clone
                            .send(ProcessingEvent::Report(Severity::Error))
                            .expect("output should still receive ProcessingEvents");
                        println!("Error: {}", e)
                    }
                },
            );
        }
    });
}
