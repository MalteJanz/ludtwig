use quick_xml::de;
use serde::Deserialize;
use std::convert::TryFrom;
use std::fs;

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
pub struct Config {
    pub indentation_mode: IndentationMode,
    pub indentation_count: u8,
    pub indent_children_of_blocks: bool,
    pub linebreaks_around_blocks: bool,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case", try_from = "String")]
pub enum IndentationMode {
    Space,
    Tab,
}

impl TryFrom<String> for IndentationMode {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "space" => Ok(IndentationMode::Space),
            "tab" => Ok(IndentationMode::Tab),
            _ => Err(String::from(
                "invalid indentation mode. Valid ones are 'space' and 'tab'.",
            )),
        }
    }
}

pub fn read_or_default() -> Config {
    // TODO
    let raw_xml = fs::read_to_string("./.ludtwig-conf.xml")
        .expect("can't find config file '.ludtwig-conf.xml'");
    let config = de::from_str(&raw_xml).expect("can't parse config file '.ludtwig-conf.xml'");

    config
}
