use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

use figment::providers::{Env, Format as FigFormat, Toml};
use figment::Figment;
use serde::Deserialize;

use crate::Opts;

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
    pub indent_children_of_blocks: bool,
    pub linebreaks_around_blocks: bool,
    pub twig_quotation: Quotation,
    pub html_quotation: Quotation,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "kebab-case")]
pub enum IndentationMode {
    Space,
    Tab,
}

impl Display for IndentationMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IndentationMode::Space => {
                write!(f, "spaces")
            }
            IndentationMode::Tab => {
                write!(f, "tabs")
            }
        }
    }
}

impl IndentationMode {
    pub fn corresponding_char(&self) -> char {
        match self {
            IndentationMode::Space => ' ',
            IndentationMode::Tab => '\t',
        }
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
pub enum LineEnding {
    #[serde(rename = "unix_LF")]
    UnixLF,
    #[serde(rename = "windows_CRLF")]
    WindowsCRLF,
}

impl Display for LineEnding {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LineEnding::UnixLF => {
                write!(f, "UnixLF (\\n)")
            }
            LineEnding::WindowsCRLF => {
                write!(f, "WindowsCRLF (\\r\\n)")
            }
        }
    }
}

impl LineEnding {
    pub fn corresponding_string(&self) -> &'static str {
        match self {
            LineEnding::UnixLF => "\n",
            LineEnding::WindowsCRLF => "\r\n",
        }
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
pub enum Quotation {
    #[serde(rename = "single")]
    Single,
    #[serde(rename = "double")]
    Double,
}

impl Display for Quotation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Quotation::Single => {
                write!(f, "single quotes (')")
            }
            Quotation::Double => {
                write!(f, "double quotes (\")")
            }
        }
    }
}

impl Quotation {
    pub fn corresponding_char(&self) -> char {
        match self {
            Quotation::Single => '\'',
            Quotation::Double => '"',
        }
    }
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
            config_path.to_string_lossy()
        );

        std::process::exit(0);
    }

    match Config::new(config_path) {
        Ok(c) => c,
        Err(e) => {
            println!("Error reading configuration:");
            println!("{}", e);
            std::process::exit(1)
        }
    }
}
