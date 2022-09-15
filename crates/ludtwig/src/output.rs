use std::io;
use std::io::Write;
use std::sync::mpsc::Receiver;

use crate::check::rule::Severity;

pub enum ProcessingEvent {
    FileProcessed,
    Report(Severity),
}

/// This function receives all the [CliOutputMessage] instances from the receiver channel and
/// prints information to the command line interface.
pub fn handle_processing_output(rx: Receiver<ProcessingEvent>) -> i32 {
    let mut file_count = 0;
    let mut error_count = 0;
    let mut warning_count = 0;
    let mut info_count = 0;

    // receive all incoming messages until all sending ends are closed.
    while let Ok(msg) = rx.recv() {
        match msg {
            ProcessingEvent::FileProcessed => {
                file_count += 1;
            }
            ProcessingEvent::Report(severity) => match severity {
                Severity::Error => {
                    error_count += 1;
                }
                Severity::Warning => {
                    warning_count += 1;
                }
                Severity::Info => {
                    info_count += 1;
                }
            },
        }
    }

    let conclusion_msg = format!(
        "\nFiles scanned: {}, Errors: {}, Warnings: {}, Info: {}\n",
        file_count, error_count, warning_count, info_count
    );

    if file_count > 0 && (error_count > 0 || warning_count > 0) {
        io::stderr().write_all(conclusion_msg.as_bytes()).unwrap();
        println!("Happy bug fixing ;)");
        return 1; // return exit code 1 if there were errors or warnings.
    } else if file_count > 0 {
        print!("{}", conclusion_msg);
        println!("Good job! o.O");
        return 0;
    }

    0
}
