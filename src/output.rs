use ansi_term::Colour::*;
use ansi_term::Style;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::mpsc;

#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    /// only to notify the output processing about a file, that was processed (does not show in CLI)
    None,
    Error(String),
    Warning(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct OutputMessage {
    pub file: Arc<PathBuf>,
    pub output: Output,
}

pub async fn handle_processing_output(mut rx: mpsc::Receiver<OutputMessage>) -> i32 {
    let mut map = HashMap::new();
    let mut file_count = 0;
    let mut warning_count = 0;
    let mut error_count = 0;

    while let Some(msg) = rx.recv().await {
        let entry = map.entry(msg.file).or_insert(vec![]);

        match msg.output {
            Output::None => {}
            _ => {
                entry.push(msg.output);
            }
        }
    }

    for (file_path, output_list) in map {
        file_count += 1;

        if output_list.is_empty() {
            continue;
        }

        println!("\nFile: {:?}", file_path);

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
        return 1;
    } else if file_count > 0 {
        println!("{}", Style::new().on(Green).paint("Good job! o.O"));
        return 0;
    }

    return 0;
}
