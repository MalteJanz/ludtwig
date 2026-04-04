pub fn is_valid_ascii_alpha_snake_case(s: &str, screaming: bool) -> bool {
    let is_ascii_case = if screaming {
        char::is_ascii_uppercase
    } else {
        char::is_ascii_lowercase
    };

    let mut iter = s.chars().enumerate().peekable();
    while let Some((idx, c)) = iter.next() {
        let next = iter.peek();

        // first or last should not be an underline
        if (idx == 0 || next.is_none()) && c == '_' {
            return false;
        }

        // chars must match the required case (lowercase or uppercase), or be a digit or underline
        if !is_ascii_case(&c) && !c.is_ascii_digit() && c != '_' {
            return false;
        }

        // no two underlines next to each other
        if let Some((_, next)) = next {
            if c == '_' && *next == '_' {
                return false;
            }
        }
    }

    true
}

pub fn try_make_snake_case(original: &str, screaming: bool) -> Option<String> {
    let to_ascii_case = if screaming {
        char::to_ascii_uppercase
    } else {
        char::to_ascii_lowercase
    };

    let mut iter = original.chars().enumerate().peekable();
    let mut attempt = String::new();
    while let Some((idx, c)) = iter.next() {
        let next = iter.peek();

        // special first or last character
        if idx == 0 || next.is_none() {
            // skip minus or underline at the start and end
            if c == '-' || c == '_' {
                continue;
            }

            // first uppercase should not be pretended with underline
            if idx == 0 && c.is_ascii_uppercase() {
                attempt.push(to_ascii_case(&c));
                continue;
            }
        }

        if let Some((_, next)) = next {
            if (c == '-' || c == '_') && (*next == '_' || *next == '-' || next.is_ascii_uppercase())
            {
                continue; // next will already place an underline
            }
        }

        // replace all minus with underline
        if c == '-' {
            attempt.push('_');
            continue;
        }

        // make an underline before each uppercase and replace it with lowercase
        if c.is_ascii_uppercase() {
            attempt.push('_');
        }

        attempt.push(to_ascii_case(&c));
    }

    // validate suggestion
    if is_valid_ascii_alpha_snake_case(&attempt, screaming) {
        return Some(attempt);
    }

    None
}

pub fn is_valid_alphanumeric_kebab_case(s: &str) -> bool {
    let mut iter = s.chars().enumerate().peekable();
    while let Some((idx, c)) = iter.next() {
        let next = iter.peek();

        // first or last should not be an minus
        if (idx == 0 || next.is_none()) && c == '-' {
            return false;
        }

        if idx == 0 {
            // special rule for first characters
            if !(c.is_ascii_lowercase() || c.is_ascii_digit() || [':', '@', '#'].contains(&c)) {
                return false;
            }
        } else {
            // everything else must be ascii lowercase or minus
            if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-') {
                return false;
            }
        }

        // no two minus next to each other
        if let Some((_, next)) = next {
            if c == '-' && *next == '-' {
                return false;
            }
        }
    }

    true
}

pub fn try_make_kebab_case(original: &str) -> Option<String> {
    let mut iter = original.chars().enumerate().peekable();
    let mut attempt = String::new();
    while let Some((idx, c)) = iter.next() {
        let next = iter.peek();

        // special first or last character
        if idx == 0 || next.is_none() {
            // skip minus or underline at the start and end
            if ['-', '_'].contains(&c) {
                continue;
            }

            // first uppercase should not be pretended with minus
            if idx == 0 && c.is_ascii_uppercase() {
                attempt.push(c.to_ascii_lowercase());
                continue;
            }
        }

        if let Some((_, next)) = next {
            if (c == '-' || c == '_') && (*next == '_' || *next == '-' || next.is_ascii_uppercase())
            {
                continue; // next will already place an underline
            }
        }

        // replace all underline with minus
        if c == '_' {
            attempt.push('-');
            continue;
        }

        // make an minus before each uppercase and replace it with lowercase
        if c.is_ascii_uppercase() {
            attempt.push('-');
        }

        attempt.push(c.to_ascii_lowercase());
    }

    // validate suggestion
    if is_valid_alphanumeric_kebab_case(&attempt) {
        return Some(attempt);
    }

    None
}

/// Check if a name is valid camelCase.
/// Allows leading underscores (e.g. `_private`).
/// First non-underscore character must be lowercase letter.
/// No underscores or hyphens after the initial prefix.
pub fn is_camel_case(name: &str) -> bool {
    let stripped = name.trim_start_matches('_');
    if stripped.is_empty() {
        return true; // just underscores, allow it
    }

    // First character must be lowercase
    let first = stripped.chars().next().unwrap();
    if !first.is_ascii_lowercase() {
        return false;
    }

    // No underscores or hyphens in the rest
    !stripped.contains('_') && !stripped.contains('-')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_ascii_alpha_snake_case() {
        assert!(is_valid_ascii_alpha_snake_case("my_block", false));
        assert!(is_valid_ascii_alpha_snake_case("my_b_l_o_c_k", false));
        assert!(is_valid_ascii_alpha_snake_case(
            "page_account_register_advantages_entry1",
            false
        ));
        assert!(is_valid_ascii_alpha_snake_case("b2b_something", false));
        assert!(is_valid_ascii_alpha_snake_case("something_v1_child", false));

        assert!(!is_valid_ascii_alpha_snake_case("my-block", false));
        assert!(!is_valid_ascii_alpha_snake_case("myBlock", false));
        assert!(!is_valid_ascii_alpha_snake_case("MyBlock", false));
        assert!(!is_valid_ascii_alpha_snake_case("my__block", false));
        assert!(!is_valid_ascii_alpha_snake_case("_my_block", false));
        assert!(!is_valid_ascii_alpha_snake_case("__my_block", false));
        assert!(!is_valid_ascii_alpha_snake_case("my_block_", false));
        assert!(!is_valid_ascii_alpha_snake_case("my_block__", false));
        assert!(!is_valid_ascii_alpha_snake_case("__my_block__", false));
        assert!(!is_valid_ascii_alpha_snake_case("innerA", false));
        assert!(!is_valid_ascii_alpha_snake_case(
            "pageAccountRegister",
            false
        ));
        assert!(!is_valid_ascii_alpha_snake_case(
            "page-account-register",
            false
        ));
        assert!(!is_valid_ascii_alpha_snake_case("b2bSomething", false));
        assert!(!is_valid_ascii_alpha_snake_case(
            "something_v1-child",
            false
        ));
    }

    #[test]
    fn test_is_ascii_alpha_snake_case_screaming() {
        assert!(is_valid_ascii_alpha_snake_case("MY_BLOCK", true));
        assert!(is_valid_ascii_alpha_snake_case("MY_B_L_O_C_K", true));
        assert!(is_valid_ascii_alpha_snake_case(
            "PAGE_ACCOUNT_REGISTER_ADVANTAGES_ENTRY1",
            true
        ));
        assert!(is_valid_ascii_alpha_snake_case("B2B_SOMETHING", true));
        assert!(is_valid_ascii_alpha_snake_case("SOMETHING_V1_CHILD", true));

        assert!(!is_valid_ascii_alpha_snake_case("MY-BLOCK", true));
        assert!(!is_valid_ascii_alpha_snake_case("myBlock", true));
        assert!(!is_valid_ascii_alpha_snake_case("MyBlock", true));
        assert!(!is_valid_ascii_alpha_snake_case("MY__BLOCK", true));
        assert!(!is_valid_ascii_alpha_snake_case("_MY_BLOCK", true));
        assert!(!is_valid_ascii_alpha_snake_case("__MY_BLOCK", true));
        assert!(!is_valid_ascii_alpha_snake_case("MY_BLOCK_", true));
        assert!(!is_valid_ascii_alpha_snake_case("MY_BLOCK__", true));
        assert!(!is_valid_ascii_alpha_snake_case("__MY_BLOCK__", true));
        assert!(!is_valid_ascii_alpha_snake_case("INNERa", true));
        assert!(!is_valid_ascii_alpha_snake_case(
            "pageAccountRegister",
            true
        ));
        assert!(!is_valid_ascii_alpha_snake_case(
            "PAGE-ACCOUNT-REGISTER",
            true
        ));
        assert!(!is_valid_ascii_alpha_snake_case("b2bSomething", true));
        assert!(!is_valid_ascii_alpha_snake_case("SOMETHING_V1-CHILD", true));
    }

    #[test]
    fn test_try_make_snake_case() {
        assert_eq!(
            try_make_snake_case("my-kebab-case-block", false),
            Some("my_kebab_case_block".to_string())
        );
        assert_eq!(
            try_make_snake_case("myCamelCaseBlock", false),
            Some("my_camel_case_block".to_string())
        );
        assert_eq!(
            try_make_snake_case("MyPascalCaseBlock", false),
            Some("my_pascal_case_block".to_string())
        );
        assert_eq!(
            try_make_snake_case("myStrange-Block", false),
            Some("my_strange_block".to_string())
        );
        assert_eq!(
            try_make_snake_case("innerA", false),
            Some("inner_a".to_string())
        );
        assert_eq!(
            try_make_snake_case("inner-", false),
            Some("inner".to_string())
        );
        assert_eq!(
            try_make_snake_case("inner_", false),
            Some("inner".to_string())
        );
        assert_eq!(
            try_make_snake_case("-inner", false),
            Some("inner".to_string())
        );
        assert_eq!(
            try_make_snake_case("_inner", false),
            Some("inner".to_string())
        );
        assert_eq!(
            try_make_snake_case("pageAccountRegister", false),
            Some("page_account_register".to_string())
        );
        assert_eq!(
            try_make_snake_case("page-account-register", false),
            Some("page_account_register".to_string())
        );
        assert_eq!(
            try_make_snake_case("b2bSomething", false),
            Some("b2b_something".to_string())
        );
        assert_eq!(
            try_make_snake_case("something_v1-child", false),
            Some("something_v1_child".to_string())
        );
        assert_eq!(try_make_snake_case("_My-Broken_-Block_", false), None);
    }

    #[test]
    fn test_try_make_snake_case_screaming() {
        assert_eq!(
            try_make_snake_case("my-kebab-case-block", true),
            Some("MY_KEBAB_CASE_BLOCK".to_string())
        );
        assert_eq!(
            try_make_snake_case("myCamelCaseBlock", true),
            Some("MY_CAMEL_CASE_BLOCK".to_string())
        );
        assert_eq!(
            try_make_snake_case("MyPascalCaseBlock", true),
            Some("MY_PASCAL_CASE_BLOCK".to_string())
        );
        assert_eq!(
            try_make_snake_case("myStrange-Block", true),
            Some("MY_STRANGE_BLOCK".to_string())
        );
        assert_eq!(
            try_make_snake_case("innerA", true),
            Some("INNER_A".to_string())
        );
        assert_eq!(
            try_make_snake_case("inner-", true),
            Some("INNER".to_string())
        );
        assert_eq!(
            try_make_snake_case("inner_", true),
            Some("INNER".to_string())
        );
        assert_eq!(
            try_make_snake_case("-inner", true),
            Some("INNER".to_string())
        );
        assert_eq!(
            try_make_snake_case("_inner", true),
            Some("INNER".to_string())
        );
        assert_eq!(
            try_make_snake_case("pageAccountRegister", true),
            Some("PAGE_ACCOUNT_REGISTER".to_string())
        );
        assert_eq!(
            try_make_snake_case("page-account-register", true),
            Some("PAGE_ACCOUNT_REGISTER".to_string())
        );
        assert_eq!(
            try_make_snake_case("b2bSomething", true),
            Some("B2B_SOMETHING".to_string())
        );
        assert_eq!(
            try_make_snake_case("something_v1-child", true),
            Some("SOMETHING_V1_CHILD".to_string())
        );
        assert_eq!(try_make_snake_case("_My-Broken_-Block_", true), None);
    }

    #[test]
    fn test_is_valid_alphanumeric_kebab_case() {
        assert!(is_valid_alphanumeric_kebab_case("my-attribute"));
        assert!(is_valid_alphanumeric_kebab_case("h1"));
        assert!(is_valid_alphanumeric_kebab_case(":vue-bound"));
        assert!(is_valid_alphanumeric_kebab_case("@vue-event"));
        assert!(is_valid_alphanumeric_kebab_case("#vue-slot"));
        assert!(is_valid_alphanumeric_kebab_case("my-attribute2"));

        assert!(!is_valid_alphanumeric_kebab_case("my--attribute"));
        assert!(!is_valid_alphanumeric_kebab_case("-my-attribute"));
        assert!(!is_valid_alphanumeric_kebab_case("my-attribute-"));
        assert!(!is_valid_alphanumeric_kebab_case("my-attribute--"));
        assert!(!is_valid_alphanumeric_kebab_case("--my-attribute"));
        assert!(!is_valid_alphanumeric_kebab_case("myAttribute"));
        assert!(!is_valid_alphanumeric_kebab_case("my_attribute"));
        assert!(!is_valid_alphanumeric_kebab_case("myA"));
    }

    #[test]
    fn test_try_make_kebab_case() {
        assert_eq!(
            try_make_kebab_case("my_snake_case_block"),
            Some("my-snake-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("myCamelCaseBlock"),
            Some("my-camel-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("MyPascalCaseBlock"),
            Some("my-pascal-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case(":myVueCamelCaseBlock"),
            Some(":my-vue-camel-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("@myVueCamelCaseBlock"),
            Some("@my-vue-camel-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("#myVueCamelCaseBlock"),
            Some("#my-vue-camel-case-block".to_string())
        );
        assert_eq!(
            try_make_kebab_case("myStrange-Block"),
            Some("my-strange-block".to_string())
        );
        assert_eq!(try_make_kebab_case("myA"), Some("my-a".to_string()));
        assert_eq!(try_make_kebab_case("my-"), Some("my".to_string()));
        assert_eq!(try_make_kebab_case("my_"), Some("my".to_string()));
        assert_eq!(try_make_kebab_case("-my"), Some("my".to_string()));
        assert_eq!(try_make_kebab_case("_my"), Some("my".to_string()));
        assert_eq!(
            try_make_kebab_case("my_attribute2_is_valid3"),
            Some("my-attribute2-is-valid3".to_string())
        );
        assert_eq!(try_make_kebab_case("_My-Broken_-Block_"), None);
    }

    #[test]
    fn test_is_camel_case() {
        assert!(is_camel_case("camelCase"));
        assert!(is_camel_case("camelCase2Long"));
        assert!(is_camel_case("_privateVar")); // leading underscore is allowed
        assert!(is_camel_case("_")); // just underscores, allowed by convention

        assert!(!is_camel_case("PascalCase"));
        assert!(!is_camel_case("_PrivateVar")); // leading underscore + PascalCase
        assert!(!is_camel_case("_snake_var")); // leading underscore + snake_case
        assert!(!is_camel_case("snake_case"));
        assert!(!is_camel_case("kebab-case"));
        assert!(!is_camel_case("UPPERCASE"));
    }
}
