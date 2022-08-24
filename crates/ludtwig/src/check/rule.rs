use ludtwig_parser::syntax::untyped::{SyntaxNode, SyntaxToken, TextRange};

pub trait Rule {
    /// A unique, kebab-case name for the rule.
    fn name(&self) -> &'static str;

    /// Check an individual untyped node in the syntax tree.
    /// The conversion to a typed AST node can be made at any time with a simple call to cast.
    /// Defaults to doing nothing.
    ///
    /// The return type is `Option<()>` to allow usage of the `?` (early return if not found) on the properties of the AST nodes
    /// which are all optional.
    fn check_node(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        None
    }

    /// Check an individual untyped token (which doesn't have children) in the syntax tree.
    /// The conversion to a typed AST node can be made at any time with a simple call to cast.
    /// Defaults to doing nothing.
    ///
    /// The return type is `Option<()>` to allow usage of the `?` (early return if not found) on the properties of the AST nodes
    /// which are all optional.
    fn check_token(&self, token: SyntaxToken, ctx: &mut RuleContext) -> Option<()> {
        None
    }

    /// Called once with the root untyped node in the syntax tree.
    /// The conversion to a typed AST node can be made at any time with a simple call to cast.
    /// Defaults to doing nothing.
    ///
    /// The return type is `Option<()>` to allow usage of the `?` (early return if not found) on the properties of the AST nodes
    /// which are all optional.
    fn check_root(&self, node: SyntaxNode, ctx: &mut RuleContext) -> Option<()> {
        None
    }
}

#[derive(Debug)]
pub struct RuleContext {
    // file_id
    // source_text
    pub(super) check_results: Vec<CheckResult>,
}

impl RuleContext {
    pub fn create_result<S: Into<String>>(
        &self,
        rule_name: S,
        severity: Severity,
        message: S,
    ) -> CheckResult {
        CheckResult {
            rule_name: rule_name.into(),
            severity,
            message: message.into(),
            primary: None,
            secondary: vec![],
        }
    }

    pub fn add_result(&mut self, result: CheckResult) {
        self.check_results.push(result);
    }
}

#[derive(Debug)]
pub struct CheckResult {
    // file_id
    pub(super) rule_name: String,
    pub(super) severity: Severity,
    pub(super) message: String,
    pub(super) primary: Option<CheckNote>,
    pub(super) secondary: Vec<CheckNote>,
}

impl CheckResult {
    // TODO: enforce only one primary_note call via type builder pattern
    /// The primary (red) label and location of the error, there should be only one of these per check result.
    /// Further context can be provided with multiple secondary notes.
    pub fn primary_note<S: Into<String>>(mut self, syntax_range: TextRange, message: S) -> Self {
        self.primary = Some(CheckNote {
            syntax_range,
            message: message.into(),
        });
        self
    }

    /// The secondary (blue) label which can provide more context and explain the error to a user.
    pub fn secondary_note<S: Into<String>>(mut self, syntax_range: TextRange, message: S) -> Self {
        self.secondary.push(CheckNote {
            syntax_range,
            message: message.into(),
        });
        self
    }

    pub fn severity(mut self, severity: Severity) -> Self {
        self.severity = severity;
        self
    }
}

#[derive(Debug)]
pub struct CheckNote {
    pub(super) syntax_range: TextRange,
    pub(super) message: String,
}

#[derive(Debug, Clone)]
pub enum Severity {
    Error,
    Warning,
    Info,
}
