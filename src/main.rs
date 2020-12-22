mod analyzer;
mod output;
mod process;
mod writer;

use crate::output::OutputMessage;
use clap::{crate_authors, crate_version, Clap, ValueHint};
use std::boxed::Box;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs::{self};
use tokio::sync::mpsc;
use walkdir::WalkDir;

/// A CLI tool for '.twig' files with focus on formatting and detecting mistakes.
#[derive(Clap, Debug, Clone)]
#[clap(version = crate_version!(), author = crate_authors!())]
struct Opts {
    /// Files or directories to scan and format
    #[clap(value_name = "FILE", min_values = 1, required = true, value_hint = ValueHint::AnyPath)]
    files: Vec<PathBuf>,

    /// Disable the analysis of the syntax tree. There will still be parsing errors.
    #[clap(short = 'A', long)]
    no_analysis: bool,

    /// Disable the formatted writing of the syntax tree to disk. With this option the tool will not write to any files.
    #[clap(short = 'W', long)]
    no_writing: bool,

    /// Specify a custom output directory instead of modifying the files in place.
    #[clap(short, long, value_hint = ValueHint::AnyPath)]
    output_path: Option<PathBuf>,
}

#[derive(Debug)]
pub struct CliContext {
    /// Channel sender for transmitting messages back to the user.
    pub output_tx: mpsc::Sender<OutputMessage>,
    /// Disable the analysis of the syntax tree. There will still be parsing errors.
    pub no_analysis: bool,
    /// Disable the formatted writing of the syntax tree to disk. With this option the tool will not write to any files.
    pub no_writing: bool,
    /// Specify a custom output directory instead of modifying the files in place.
    pub output_path: Option<PathBuf>,
}

impl CliContext {
    /// Helper function to send a [OutputMessage] back to the user.
    pub async fn send_output(&self, msg: OutputMessage) {
        self.output_tx.send(msg).await.unwrap();
    }
}

/// Parse the CLI arguments and bootstrap the async application.
fn main() {
    let opts: Opts = Opts::parse();

    let runtime = tokio::runtime::Runtime::new().expect("can't create tokio runtime");

    let process_code = runtime.block_on(app(opts)).unwrap();

    drop(runtime);

    std::process::exit(process_code);
}

/// The entry point of the async application.
async fn app(opts: Opts) -> Result<i32, Box<dyn std::error::Error>> {
    println!("Parsing files...");

    // sender and receiver channels for the communication between tasks and the user.
    let (tx, rx) = mpsc::channel(128);

    let cli_context = Arc::new(CliContext {
        output_tx: tx,
        no_analysis: opts.no_analysis,
        no_writing: opts.no_writing,
        output_path: opts.output_path,
    });

    let output_handler = tokio::spawn(output::handle_processing_output(rx));

    let mut futures = Vec::with_capacity(opts.files.len());
    for path in opts.files {
        let context = Arc::clone(&cli_context);
        futures.push(tokio::task::spawn(handle_input_path(path, context)));
    }
    drop(cli_context);

    for t in futures {
        t.await.unwrap();
    }

    let process_code = output_handler.await.unwrap();

    Ok(process_code)
}

/// Process one input path (CLI file argument).
async fn handle_input_path(path: PathBuf, cli_context: Arc<CliContext>) {
    let meta = fs::metadata(&path).await.unwrap();
    if meta.is_file() {
        if let Some(file_type) = path.extension() {
            if file_type == "twig" {
                process::process_file(path, cli_context).await;
            }
        }

        return;
    }

    handle_input_dir(path, cli_context).await;
}

/// Process a directory path.
async fn handle_input_dir(path: PathBuf, cli_context: Arc<CliContext>) {
    let processes = tokio::task::spawn_blocking(move || {
        let mut futures_processes = Vec::new();

        for entry in WalkDir::new(path) {
            let entry = entry.unwrap();
            let path = entry.path();

            if !path.is_file() {
                continue;
            }

            if let Some(file_type) = path.extension() {
                if file_type == "twig" {
                    futures_processes.push(tokio::spawn(process::process_file(
                        path.into(),
                        Arc::clone(&cli_context),
                    )));
                }
            }
        }

        futures_processes
    })
    .await
    .unwrap();

    for f in processes {
        f.await.unwrap();
    }
}
