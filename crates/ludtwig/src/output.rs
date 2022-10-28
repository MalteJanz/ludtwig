use codespan_reporting::term::termcolor::{Buffer, BufferWriter, ColorChoice};
use std::io;
use std::io::Write;
use std::sync::mpsc::Receiver;

use crate::check::rule::Severity;

pub enum ProcessingEvent {
    FileProcessed,
    Report(Severity),
    OutputStderrMessage(Buffer),
}

/// This function receives all the [CliOutputMessage] instances from the receiver channel and
/// prints information to the command line interface.
pub fn handle_processing_output(rx: Receiver<ProcessingEvent>) -> i32 {
    let mut file_count = 0;
    let mut error_count = 0;
    let mut warning_count = 0;
    let mut help_count = 0;
    let mut info_count = 0;

    let stderr_writer = BufferWriter::stderr(ColorChoice::Always);

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
                Severity::Help => {
                    help_count += 1;
                }
                Severity::Info => {
                    info_count += 1;
                }
            },
            ProcessingEvent::OutputStderrMessage(buffer) => {
                stderr_writer.print(&buffer).unwrap();
            }
        }
    }

    drop(stderr_writer); // finish writing to stderr

    let conclusion_msg = format!(
        "\nFiles scanned: {}, Errors: {}, Warnings: {}, Helps: {}, Info: {}, Total: {}\n",
        file_count,
        error_count,
        warning_count,
        help_count,
        info_count,
        (error_count + warning_count + help_count + info_count)
    );

    if file_count > 0 && (error_count > 0 || warning_count > 0 || help_count > 0) {
        io::stderr().write_all(conclusion_msg.as_bytes()).unwrap();
        1 // return exit code 1 if there were errors, warnings or help.
    } else {
        print!("{}", conclusion_msg);
        0
    }
}
