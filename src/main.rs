mod analyzer;
mod output;
mod process;
mod writer;

use crate::output::OutputMessage;
use clap::{crate_authors, crate_version, Clap, ValueHint};
use std::boxed::Box;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::fs::{self};
use tokio::sync::mpsc;
use walkdir::WalkDir;

/// Tools for '.twig' files. Mostly scanning for errors and formatting.
#[derive(Clap, Debug, Clone)]
#[clap(version = crate_version!(), author = crate_authors!())]
struct Opts {
    /// Files or directories to scan and format
    #[clap(value_name = "FILE", min_values = 1, required = true, value_hint = ValueHint::AnyPath)]
    files: Vec<PathBuf>,

    /// Disable the analysis of the syntax tree. There will still be parsing errors.
    #[clap(short = 'a', long)]
    no_analysis: bool,

    /// Disable the formatted printing of the syntax tree to disk. With this option the tool will not write to any files.
    #[clap(short = 'w', long)]
    no_writing: bool,

    /// Specify a custom output directory instead of modifying the files in place.
    #[clap(short, long, value_hint = ValueHint::AnyPath)]
    output_path: Option<PathBuf>,
    // Sets a custom config file.
    // TODO: reimplement with working config file
    //#[clap(, long, default_value = "default.conf")]
    //config: Option<PathBuf>,
}

#[derive(Debug)]
pub struct CliContext {
    pub output_tx: mpsc::Sender<OutputMessage>,
    /// Disable the analysis of the syntax tree. There will still be parsing errors.
    pub no_analysis: bool,
    /// Disable the formatted printing of the syntax tree to disk. With this option the tool will not write to any files.
    pub no_writing: bool,
    /// Specify a custom output directory instead of modifying the files in place.
    pub output_path: Option<PathBuf>,
}

impl CliContext {
    pub async fn send_output(&self, msg: OutputMessage) {
        self.output_tx.send(msg).await.unwrap();
    }
}

fn main() {
    let opts: Opts = Opts::parse();

    let runtime = tokio::runtime::Runtime::new().expect("can't create tokio runtime");

    let process_code = runtime.block_on(app(opts)).unwrap();

    drop(runtime);

    std::process::exit(process_code);
}

async fn app(opts: Opts) -> Result<i32, Box<dyn std::error::Error>> {
    println!("Parsing files...");

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
        //let tx = cli_context.output_tx.clone();
        let context = Arc::clone(&cli_context);
        futures.push(tokio::task::spawn(handle_input_path(path, context)));
    }
    drop(cli_context);

    for t in futures {
        t.await.unwrap();
    }

    let process_code = output_handler.await.unwrap();

    Ok(process_code)

    // Gets a value for config if supplied by user, or defaults to "default.conf"
    // println!("Value for config: {}", opts.config);
    // TODO: implement configuration.
}

async fn handle_input_path<P>(path: P, cli_context: Arc<CliContext>)
where
    P: AsRef<Path> + 'static + Send,
{
    let meta = fs::metadata(&path).await.unwrap();
    if meta.is_file() {
        process::process_file(path.as_ref().into(), cli_context).await;
        return;
    }

    handle_input_dir(path, cli_context).await;
}

async fn handle_input_dir<P>(path: P, cli_context: Arc<CliContext>)
where
    P: AsRef<Path> + 'static + Send,
{
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
