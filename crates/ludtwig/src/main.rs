mod attribute;
mod config;
mod output;
mod process;

use crate::attribute::LudtwigRegex;
use crate::config::Config;
use crate::output::OutputMessage;
use rayon::prelude::*;
use std::boxed::Box;
use std::path::PathBuf;
use std::sync::mpsc::SyncSender;
use std::sync::{mpsc, Arc};
use std::thread;
use walkdir::{DirEntry, WalkDir};
use clap::Parser;

/// A CLI tool for '.twig' files with focus on formatting and detecting mistakes.
#[derive(Parser, Debug, Clone)]
#[clap(author = env!("CARGO_PKG_AUTHORS"))]
pub struct Opts {
    /// Files or directories to scan and format
    #[clap(
        value_name = "FILE",
        min_values = 1,
        required = true,
        conflicts_with = "create_config",
        parse(from_os_str),
        name = "files"
    )]
    files: Vec<PathBuf>,

    /// Disable the analysis of the syntax tree. There will still be parsing errors.
    #[clap(short = 'A', long)]
    no_analysis: bool,

    /// Disable the formatted writing of the syntax tree to disk. With this option the tool will not write to any files.
    #[clap(short = 'W', long)]
    no_writing: bool,

    /// Specify a custom output directory instead of modifying the files in place.
    #[clap(short, long, parse(from_os_str))]
    output_path: Option<PathBuf>,

    /// Specify where the ludtwig configuration file is. Ludtwig looks in the current directory for a 'ludtwig-config.toml' by default.
    #[clap(short = 'c', long, parse(from_os_str))]
    config_path: Option<PathBuf>,

    /// Create the default configuration file in the config path. Defaults to the current directory.
    #[clap(short = 'C', long, name = "create_config")]
    create_config: bool,
}

#[derive(Debug)]
pub struct CliContext {
    /// Channel sender for transmitting messages back to the user.
    pub output_tx: SyncSender<OutputMessage>,
    /// Disable the analysis of the syntax tree. There will still be parsing errors.
    pub no_analysis: bool,
    /// Disable the formatted writing of the syntax tree to disk. With this option the tool will not write to any files.
    pub no_writing: bool,
    /// Specify a custom output directory instead of modifying the files in place.
    pub output_path: Option<PathBuf>,
    /// The config values to use.
    pub config: Config,
    /// vector of compiled regular expressions for attribute sorting
    pub attribute_regex: Vec<LudtwigRegex>,
}

impl CliContext {
    /// Helper function to send a [OutputMessage] back to the user.
    pub fn send_output(&self, msg: OutputMessage) {
        self.output_tx.send(msg).unwrap();
    }
}

/// Parse the CLI arguments and bootstrap the application.
fn main() {
    let opts: Opts = Opts::from_args();
    let config = config::handle_config_or_exit(&opts);
    let attribute_regex = match config.get_compiled_attribute_regex() {
        Ok(v) => v,
        Err(e) => {
            println!("Error reading configuration (One value of the attribute-ordering-regex array can not be compiled to a regex):");
            println!("{}", e);
            std::process::exit(1);
        }
    };

    let process_code = app(opts, config, attribute_regex).unwrap();
    std::process::exit(process_code);
}

/// The entry point of the async application.
fn app(
    opts: Opts,
    config: Config,
    attribute_regex: Vec<LudtwigRegex>,
) -> Result<i32, Box<dyn std::error::Error>> {
    println!("Parsing files...");

    // sender and receiver channels for the communication between tasks and the user.
    // the channel is bounded to buffer 32 messages before sending will block.
    // this limit should be fine for one thread continuously processing the incoming messages from the channel.
    let (tx, rx) = mpsc::sync_channel(32);

    let cli_context = Arc::new(CliContext {
        output_tx: tx,
        no_analysis: opts.no_analysis,
        no_writing: opts.no_writing,
        output_path: opts.output_path,
        config,
        attribute_regex,
    });

    let output_handler = thread::spawn(|| output::handle_processing_output(rx));

    // work on each user specified file / directory path concurrently
    opts.files.into_par_iter().for_each(|path| {
        handle_input_path(path, Arc::clone(&cli_context));
    });

    drop(cli_context);

    // the output_handler will finish execution if all the sending channel ends are closed.
    let process_code = output_handler.join().unwrap();

    Ok(process_code)
}

/// filters out hidden directories or files
/// (that start with '.').
fn is_hidden(entry: &DirEntry) -> bool {
    !entry
        .file_name()
        .to_str()
        // '.' and './' is a valid path for the current working directory and not an hidden file / dir
        // otherwise anything that starts with a '.' is considered hidden for ludtwig
        .map(|s| s.starts_with('.') && s != "." && s != "./")
        .unwrap_or(false)
}

/// Process a directory path.
fn handle_input_path(path: PathBuf, cli_context: Arc<CliContext>) {
    let walker = WalkDir::new(path).into_iter();

    // synchronous directory traversal but move the work for each file to a different thread in the thread pool.
    rayon::scope(move |s| {
        for entry in walker.filter_entry(|e| is_hidden(e)) {
            let entry = entry.unwrap();

            if !entry.file_type().is_file() {
                continue;
            }

            if !entry
                .file_name()
                .to_str() // also skips non utf-8 file names!
                .map(|s| s.ends_with(".twig"))
                .unwrap_or(false)
            {
                continue;
            }

            let clone = Arc::clone(&cli_context);
            s.spawn(move |_s1| process::process_file(entry.path().into(), clone));
        }
    });
}
