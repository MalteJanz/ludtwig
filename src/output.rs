use ansi_term::Colour::*;
use ansi_term::Style;
use async_std::channel::Receiver;
use async_std::path::PathBuf;
use std::collections::HashMap;

/// The user output has different variants.
#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    /// only to notify the output processing about a file, that was processed (does not show in CLI, only for counting purposes)
    None,
    Error(String),
    Warning(String),
}

/// This is a single output message which is associated with a file path.
#[derive(Debug, Clone, PartialEq)]
pub struct OutputMessage {
    /// The file path that is associated with this output message.
    pub file: PathBuf,

    pub output: Output,
}

/// This function receives all the [OutputMessage] instances from the receiver channel and
/// prints information to the console / user.
pub async fn handle_processing_output(rx: Receiver<OutputMessage>) -> i32 {
    let mut map = HashMap::new();
    let mut file_count = 0;
    let mut warning_count = 0;
    let mut error_count = 0;

    // receive all incoming messages until all sending ends are closed.
    while let Ok(msg) = rx.recv().await {
        let entry = map.entry(msg.file).or_insert_with(Vec::new);

        match msg.output {
            Output::None => {}
            _ => {
                entry.push(msg.output);
            }
        }
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

        println!("\nFile: {:?}", file_path.as_os_str());

        for output in output_list {
            match output {
                Output::Error(message) => {
                    error_count += 1;
                    println!("[Error] {}", Red.paint(message));
                }
                Output::Warning(message) => {
                    warning_count += 1;
                    println!("[Warning] {}", Yellow.paint(message));
                }
                Output::None => {}
            }
        }
    }

    println!(
        "\nFiles scanned: {}, Errors: {}, Warnings: {}",
        file_count, error_count, warning_count
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

    return 0;
}
