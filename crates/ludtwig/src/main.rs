#![warn(clippy::pedantic)]
#![allow(clippy::module_name_repetitions)]

use crate::check::rule::{Rule, Severity};
use crate::check::rules::get_config_active_rule_definitions;
use crate::config::Config;
use crate::output::ProcessingEvent;
use clap::Parser;
use ignore::types::TypesBuilder;
use ignore::{WalkBuilder, WalkState};
use std::path::PathBuf;
use std::sync::mpsc::Sender;
use std::sync::{mpsc, Arc};
use std::thread;

mod check;
mod config;
mod error;
mod output;
mod process;

// uses author, version and description from Cargo.toml
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None)]
pub struct Opts {
    /// Files or directories to scan
    #[arg(
        value_name = "FILE",
        num_args = 1..,
        required = true,
        conflicts_with = "create_config",
        name = "files"
    )]
    files: Vec<PathBuf>,

    /// Apply all code suggestions automatically. This changes the original files!
    #[arg(short = 'f', long)]
    fix: bool,

    /// Print out the parsed syntax tree for each file
    #[arg(short = 'i', long)]
    inspect: bool,

    /// Specify where the ludtwig configuration file is. Ludtwig looks in the current directory for a 'ludtwig-config.toml' by default.
    #[arg(short = 'c', long)]
    config_path: Option<PathBuf>,

    /// Create the default configuration file in the config path. Defaults to the current directory.
    #[arg(short = 'C', long, name = "create_config")]
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
    pub rule_definitions: Vec<&'static dyn Rule>,
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
    let opts: Opts = Opts::parse();
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

    let output_handler = thread::spawn(move || output::handle_processing_output(&rx));

    // work on each user specified file / directory path concurrently
    handle_input_paths(opts.files, cli_context.clone());

    drop(cli_context); // drop this tx channel

    // the output_handler will finish execution if all the tx (sending channel) ends are closed.
    output_handler
        .join()
        .expect("Error: can't join output_handler thread")
}

/// Process a directory path.
fn handle_input_paths(paths: Vec<PathBuf>, cli_context: CliContext) {
    let types = TypesBuilder::new()
        .add_defaults()
        .select("twig")
        .select("html")
        .build()
        .unwrap();

    // create walker over all the user specified paths
    let mut walker = WalkBuilder::new(&paths[0]);
    for path in paths.into_iter().skip(1) {
        walker.add(path);
    }

    let walker = walker
        .add_custom_ignore_filename(".ludtwig-ignore")
        .types(types)
        .build_parallel();

    // parallel directory traversal but move the work for each file to a different thread in the thread pool.
    rayon::scope(move |s| {
        walker.run(|| {
            let cli_context = cli_context.clone();

            Box::new(move |entry| {
                let entry = match entry {
                    Ok(e) => e,
                    Err(e) => {
                        println!("Error: walking over the file path: {}", e);
                        cli_context
                            .send_processing_output(ProcessingEvent::Report(Severity::Error));
                        return WalkState::Continue;
                    }
                };

                // filter out directories
                if entry.file_type().map_or(true, |t| t.is_dir()) {
                    return WalkState::Continue;
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
                            println!("Error: {}", e);
                        }
                    },
                );

                WalkState::Continue
            })
        });
    });
}
