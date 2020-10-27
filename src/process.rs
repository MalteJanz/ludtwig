use crate::analyzer::analyze;
use crate::output::OutputMessage;
use crate::output::OutputType;
use crate::writer::write_tree;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::fs;
use tokio::sync::mpsc;

pub async fn process_file<P>(path: P, tx: mpsc::Sender<OutputMessage>)
where
    P: AsRef<Path>,
{
    let path = path.as_ref();

    let file_content = match fs::read_to_string(path).await {
        Ok(f) => f,
        Err(_) => {
            tx.send(OutputMessage {
                file: path.into(),
                message: "Can't read file".to_string(),
                output_type: OutputType::Error,
            })
            .await
            .unwrap();
            return;
        }
    };

    let tree = tokio::task::spawn_blocking(move || {
        let tree = match twig::parse(&file_content) {
            Ok(r) => r,
            Err(e) => {
                return Err(e.pretty_helpful_error_string(&file_content));
            }
        };

        Ok(tree)
    })
    .await
    .unwrap();

    let tree = match tree {
        Ok(t) => t,
        Err(e) => {
            tx.send(OutputMessage {
                file: path.into(),
                message: e,
                output_type: OutputType::Error,
            })
            .await
            .unwrap();

            return;
        }
    };

    let original_path = PathBuf::from(path);
    let raw_path = path.parent().unwrap_or_else(|| Path::new(""));

    let stem = path.file_stem().unwrap_or_else(|| OsStr::new(""));

    let mut filename = stem.to_os_string();
    filename.push(".formatted.");

    if let Some(extension) = path.extension() {
        filename.push(extension);
    }

    let file_path = raw_path.join(filename);

    let tree = Arc::new(tree);
    let mut futs = vec![];

    let clone = Arc::clone(&tree);
    futs.push(tokio::spawn(async move {
        analyze(original_path, clone, tx).await;
    }));

    futs.push(tokio::spawn(async move {
        write_tree(file_path, &tree).await;
    }));

    for fut in futs {
        fut.await.unwrap();
    }
}
