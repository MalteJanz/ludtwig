use ludtwig_parser::ast::TagAttribute;
use regex::Regex;
use std::borrow::Cow;

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

/// check if the attributes can be sorted
/// if they contain any twig syntax they will keep they current order
/// otherwise they will be sorted.
pub fn apply_attribute_ordering<'a, 'b>(
    attributes: &'a [TagAttribute],
    attribute_regex: &'b [LudtwigRegex],
) -> Cow<'a, [TagAttribute]> {
    let mut attributes = Cow::Borrowed(attributes);

    let can_be_sorted = !attributes.iter().any(|a| match a {
        TagAttribute::TwigComment(_) => true,
        TagAttribute::TwigStructure(_) => true,
        TagAttribute::HtmlAttribute(_) => false,
    });

    if !can_be_sorted {
        return attributes;
    }

    attributes.to_mut().sort_by(|a, b| {
        let score_a = calculate_attribute_score(attribute_regex, a);
        let score_b = calculate_attribute_score(attribute_regex, b);

        score_a.cmp(&score_b)
    });

    attributes
}

/// try matching each of the LudtwigRegex one by one until one succeeds
/// then return the index of the regex as the score for ordering
/// in case nothing matches or it is not a HtmlAttribute it will return usize::MAX.
fn calculate_attribute_score(attribute_regex: &[LudtwigRegex], a: &TagAttribute) -> usize {
    attribute_regex
        .iter()
        .enumerate()
        .filter_map(|(i, r)| match a {
            TagAttribute::HtmlAttribute(a) => {
                if r.is_match(&a.name) {
                    Some(i)
                } else {
                    None
                }
            }
            _ => None,
        })
        .next()
        .unwrap_or(usize::MAX)
}
