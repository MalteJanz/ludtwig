use regex::Regex;

#[derive(Debug, Clone)]
pub struct LudtwigRegex {
    inverted: bool,
    regex: Regex,
}

impl LudtwigRegex {
    pub fn from_raw_config_string(raw: &str) -> Result<LudtwigRegex, regex::Error> {
        let inverted = raw.chars().next().map_or(false, |c| c == '?');
        let raw_regex = if inverted { &raw[1..] } else { &raw };
        let regex = Regex::new(raw_regex)?;

        Ok(LudtwigRegex { inverted, regex })
    }

    /// matches the given string against the regex and applies inverted logic
    pub fn is_match(&self, s: &str) -> bool {
        let is_match = self.regex.is_match(s);

        if self.inverted {
            !is_match
        } else {
            is_match
        }
    }
}
