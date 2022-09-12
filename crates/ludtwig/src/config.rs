use crate::Opts;
use figment::providers::{Env, Format as FigFormat, Toml};
use figment::Figment;
use serde::Deserialize;
use std::path::{Path, PathBuf};

// TODO: refactor config to fit the needs of the new rules

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct Config {
    pub general: General,
    pub format: Format,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct General {
    pub active_rules: Vec<String>,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct Format {
    pub line_ending: LineEnding,
    pub indentation_mode: IndentationMode,
    pub indentation_count: u8,
    pub preferred_max_line_length: u16,
    pub attribute_inline_max_count: u8,
    pub indent_children_of_blocks: bool,
    pub linebreaks_around_blocks: bool,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "kebab-case")]
pub enum IndentationMode {
    Space,
    Tab,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
pub enum LineEnding {
    #[serde(rename = "unix_LF")]
    UnixLF,
    #[serde(rename = "windows_CRLF")]
    WindowsCRLF,
}

pub const DEFAULT_CONFIG_PATH: &str = "./ludtwig-config.toml";
pub const DEFAULT_RAW_CONFIG: &str = include_str!("../ludtwig-config.toml");

impl Config {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, figment::Error> {
        let config: Config = Figment::new()
            // first read the raw config from memory (for default values)
            .merge(Toml::string(DEFAULT_RAW_CONFIG))
            // then read the config in the file system (if it exists)
            .merge(Toml::file(path))
            // last read from the environment
            .merge(
                Env::prefixed("LUDTWIG_")
                    .split("__")
                    .map(|key| key.as_str().replace('_', "-").into()),
            )
            .extract()?;

        Ok(config)
    }
}

pub fn handle_config_or_exit(opts: &Opts) -> Config {
    let config_path = opts
        .config_path
        .clone()
        .unwrap_or_else(|| PathBuf::from(DEFAULT_CONFIG_PATH));

    if opts.create_config {
        if Path::exists(config_path.as_ref()) {
            println!("The configuration file already exists at that location. \
            Try choosing a different location with '-c my-path' or make a backup of your current config file (rename it).");
            std::process::exit(1);
        }

        std::fs::write(&config_path, DEFAULT_RAW_CONFIG).expect("can't write default config");
        println!(
            "Default config was written to {}",
            config_path.to_str().unwrap_or("'non utf8 file path'")
        );

        std::process::exit(0);
    }

    match Config::new(config_path) {
        Ok(c) => c,
        Err(e) => {
            println!("Error reading configuration:");
            println!("{}", e);
            std::process::exit(1);
        }
    }
}
