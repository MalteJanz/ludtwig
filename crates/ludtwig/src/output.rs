use crate::check::rule::Severity;
use ansi_term::Colour::*;
use ansi_term::Style;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::mpsc::Receiver;

/// The user output message which is rendered in the command line interface.
#[derive(Debug, Clone)]
pub struct CliOutput {
    pub severity: Severity,
    pub message: String,
}

/// This single output message is send through a channel from other threads and
/// associates a [CliOutput] with a file path.
#[derive(Debug, Clone)]
pub struct CliOutputMessage {
    /// The file path that is associated with this output message.
    pub file: PathBuf,

    pub output: CliOutput,
}

/// This function receives all the [CliOutputMessage] instances from the receiver channel and
/// prints information to the command line interface.
pub fn handle_processing_output(rx: Receiver<CliOutputMessage>) -> i32 {
    let mut map = HashMap::new();
    let mut file_count = 0;
    let mut error_count = 0;
    let mut warning_count = 0;
    let mut info_count = 0;

    // receive all incoming messages until all sending ends are closed.
    while let Ok(msg) = rx.recv() {
        let entry = map.entry(msg.file).or_insert_with(Vec::new);
        entry.push(msg.output);
    }

    // enable ansi codes support on windows 10 if the environment supports it.
    #[cfg(windows)]
    let _ansi_enabled = ansi_term::enable_ansi_support().is_ok();

    // iterate through the messages for each file and print them out.
    for (file_path, output_list) in map {
        file_count += 1;

        if output_list.is_empty() {
            continue;
        }

        for output in output_list {
            if output.message == "" {
                continue;
            }

            match output.severity {
                Severity::Error => {
                    error_count += 1;
                }
                Severity::Warning => {
                    warning_count += 1;
                }
                Severity::Info => {
                    info_count += 1;
                }
            }

            println!("{}", output.message);
        }
    }

    println!(
        "\nFiles scanned: {}, Errors: {}, Warnings: {}, Info: {}",
        file_count, error_count, warning_count, info_count
    );

    if file_count > 0 && (error_count > 0 || warning_count > 0) {
        println!(
            "{}",
            Style::new()
                .on(White)
                .fg(Black)
                .paint("Happy bug fixing ;)")
        );
        return 1; // return exit code 1 if there were errors or warnings.
    } else if file_count > 0 {
        println!("{}", Style::new().on(Green).paint("Good job! o.O"));
        return 0;
    }

    0
}
