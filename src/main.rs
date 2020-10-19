mod process;
mod writer;

use clap::{crate_authors, crate_version, Clap, ValueHint};
use std::boxed::Box;
use std::future::Future;
use std::path::Path;
use std::pin::Pin;
use tokio::fs;
use tokio::stream::StreamExt;

/// Tools for '.twig' files. Mostly scanning for errors and formatting.
#[derive(Clap)]
#[clap(version = crate_version!(), author = crate_authors!())]
struct Opts {
    /// Files or directories to scan and format
    #[clap(value_name = "FILE", min_values = 1, required = true, value_hint = ValueHint::AnyPath)]
    files: Vec<String>,
    // Sets a custom config file.
    // TODO: reimplement config file
    //#[clap(short, long, default_value = "default.conf")]
    //config: String,

    // A level of verbosity, and can be used multiple times
    // TODO: reimplement verbose levels.
    //#[clap(short, long, parse(from_occurrences))]
    //verbose: i32,
}

fn main() {
    let opts: Opts = Opts::parse();
    //println!("working on files: {:#?}", opts.files);

    let runtime = tokio::runtime::Runtime::new().expect("can't create tokio runtime");

    runtime.block_on(async {
        let mut futures = Vec::with_capacity(opts.files.len());
        for path in &opts.files {
            futures.push(handle_input_path(path.clone()));
        }

        for t in futures {
            t.await;
        }
    });

    // Gets a value for config if supplied by user, or defaults to "default.conf"
    // println!("Value for config: {}", opts.config);
    // TODO: implement configuration.

    // Vary the output based on how many times the user used the "verbose" flag
    // (i.e. 'myprog -v -v -v' or 'myprog -vvv' vs 'myprog -v'
    // TODO: maybe implement verbose levels.
    /*
    match opts.verbose {
        0 => println!("No verbose info"),
        1 => println!("Some verbose info"),
        2 => println!("Tons of verbose info"),
        3 | _ => println!("Don't be crazy"),
    }
     */
}

async fn handle_input_path<P>(path: P)
where
    P: AsRef<Path> + 'static,
{
    let meta = fs::metadata(&path).await.unwrap();
    if meta.is_file() {
        process::process_file(&path).await;
        return;
    }

    handle_input_dir(path).await;
}

fn handle_input_dir<P>(path: P) -> Pin<Box<dyn Future<Output = ()>>>
where
    P: AsRef<Path> + 'static,
{
    Box::pin(async move {
        let mut entries = fs::read_dir(path).await.unwrap();
        let mut futures_dirs = Vec::new();
        let mut futures_processes = Vec::new();

        while let Some(entry_result) = entries.next().await {
            if entry_result.is_err() {
                continue;
            }

            let entry = entry_result.unwrap();
            let path = entry.path();

            if path.is_dir() {
                futures_dirs.push(handle_input_dir(path));
                //handle_input_dir(path).await;
                continue;
            }

            if let Some(file_type) = path.extension() {
                if file_type == "twig" {
                    futures_processes.push(process::process_file(path));
                }
            }
        }

        for f in futures_dirs {
            f.await;
        }

        for f in futures_processes {
            f.await;
        }

        ()
    })
}
