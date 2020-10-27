use colored::*;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;
use tokio::sync::mpsc;

#[derive(Debug, Clone, PartialEq)]
pub enum OutputType {
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OutputMessage {
    pub file: PathBuf,
    pub message: String,
    pub output_type: OutputType,
}

impl Display for OutputType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputType::Error => write!(f, "Error"),
            OutputType::Warning => write!(f, "Warning"),
        }
    }
}

pub async fn handle_processing_output(mut rx: mpsc::Receiver<OutputMessage>) -> i32 {
    let mut map = HashMap::new();
    let mut file_count = 0;
    let mut warning_count = 0;
    let mut error_count = 0;

    while let Some(msg) = rx.recv().await {
        file_count += 1;

        match msg.output_type {
            OutputType::Error => {
                error_count += 1;
            }
            OutputType::Warning => {
                warning_count += 1;
            }
        }

        map.entry(msg.file)
            .or_insert(vec![])
            .push((msg.output_type, msg.message));
    }

    for (k, v) in map {
        println!("\nFile: {:?}", k);

        for (output_type, message) in v {
            match output_type {
                OutputType::Error => {
                    println!("[{}] {}", output_type, message.red());
                }
                OutputType::Warning => {
                    println!("[{}] {}", output_type, message.yellow());
                }
            }
        }
    }

    println!(
        "\nFiles scanned: {}, Errors: {}, Warnings: {}",
        file_count, error_count, warning_count
    );

    if file_count > 0 && (error_count > 0 || warning_count > 0) {
        println!("{}", "Happy bug fixing ;)".black().on_white());
        return 1;
    } else if file_count > 0 {
        println!("{}", "Good job! o.O".on_green());
        return 0;
    }

    return 0;
}
