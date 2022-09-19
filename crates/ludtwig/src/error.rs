use std::error::Error;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

#[derive(Debug)]
pub enum FileProcessingError {
    FileRead {
        path: PathBuf,
        io_error: std::io::Error,
    },
    FileWrite {
        path: PathBuf,
        io_error: std::io::Error,
    },
    MaxApplyIteration,
    OverlappingSuggestionInSingleRule {
        rule_name: String,
    },
}

impl Display for FileProcessingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FileProcessingError::FileRead { path, .. } => {
                write!(f, "file {} can't be read", path.to_string_lossy())
            }
            FileProcessingError::FileWrite { path, .. } => {
                write!(f, "file {} can't be written", path.to_string_lossy())
            }
            FileProcessingError::MaxApplyIteration => {
                write!(f, "max suggestion apply iteration encountered. This may be caused by fighting rules (programmer error) or too many conflicting suggestions at once")
            }
            FileProcessingError::OverlappingSuggestionInSingleRule { rule_name } => {
                write!(f, "Suggestion collision inside the same rule, check rule {} or write bug report - this is a programmer error", rule_name)
            }
        }
    }
}

impl Error for FileProcessingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            FileProcessingError::FileRead { io_error, .. } => Some(io_error),
            FileProcessingError::FileWrite { io_error, .. } => Some(io_error),
            FileProcessingError::MaxApplyIteration => None,
            FileProcessingError::OverlappingSuggestionInSingleRule { .. } => None,
        }
    }
}
