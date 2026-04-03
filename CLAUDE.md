# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Ludtwig is a linter and formatter for Twig template files (Shopware/Symfony) that parses both Twig and HTML into a single lossless syntax tree using the `rowan` library. Written in Rust (edition 2024, MSRV 1.85.0).

## Build & Development Commands

```bash
cargo build                          # Build all crates
cargo test --all-features            # Run all tests
cargo test -p ludtwig-parser         # Test parser crate only
cargo test -p ludtwig                # Test CLI crate only
cargo test <test_name>               # Run a single test by name
cargo fmt -- --check                 # Check formatting
cargo clippy --locked                # Lint (CI uses RUSTFLAGS="-Dwarnings")
cargo bench -p ludtwig-parser        # Run parser benchmarks (criterion)
```

Fuzzing (requires nightly): `cd fuzz && cargo +nightly fuzz run main`

## Workspace Structure

Two crates in `crates/`:

- **ludtwig-parser** — Lexer (`logos`), recursive-descent parser, and syntax tree (`rowan`). Produces a lossless `GreenNode` preserving all input including errors.
- **ludtwig** — CLI application. File discovery (`ignore` + rayon parallel walking), configuration (`figment` with TOML/env), rule execution, suggestion application, and diagnostic output (`codespan-reporting`).

## Architecture

### Parser pipeline (ludtwig-parser)

`Lexer` → `Parser` (event-based, marker pattern) → `Sink` (events → `GreenNode`)

- **Lexer** (`lexer.rs`): `logos::Logos` derive produces `Token` with `SyntaxKind`
- **Grammar** (`grammar.rs`, `grammar/`): `root()` → `parse_any_element()` dispatches to HTML or Twig sub-grammars. Twig grammar split into `tags.rs`, `expression.rs`, `literal.rs`, `shopware.rs`
- **Syntax** (`syntax/untyped.rs`): `SyntaxKind` enum (~120 token + many node variants). `syntax/typed.rs`: strongly-typed AST wrappers (e.g. `HtmlTag`, `TwigBlock`)

### Linting pipeline (ludtwig)

`main.rs` (CLI/config) → `process.rs` (per-file orchestration) → `check.rs` (tree traversal + rule dispatch) → `output.rs` (result aggregation)

- **Rule trait** (`check/rule.rs`): `check_node()`, `check_token()`, `check_root()` methods. Each rule returns `CheckResult` with severity, message, labels, and optional `Suggestion` fixes.
- **Rule implementations**: one file per rule in `check/rules/`. Rules are registered in a static `RULE_DEFINITIONS` array in `check/rules.rs`.
- **Suggestion application** (`process.rs`): iterative loop (max 10 passes) — apply non-overlapping suggestions, re-parse, re-check until stable.

### Changelog

When making changes, add a changelog entry under `# NEXT-VERSION` in the affected crate's `CHANGELOG.md`. Link a GitHub issue or PR number when available, preferring issues over PRs if both exist (e.g. `[#149](https://github.com/MalteJanz/ludtwig/issues/149)`). See existing entries for style reference.

### Adding a new rule

1. Create `crates/ludtwig/src/check/rules/your_rule.rs` implementing the `Rule` trait
2. Register it in the `RULE_DEFINITIONS` array in `check/rules.rs`
3. Add it to the `active-rules` list in `ludtwig-config.toml`
4. Write tests using `test_rule()` and `test_rule_fix()` helpers from `check/rules/test`

#### `check_root` vs `check_node`/`check_token`

Rules using `check_node` or `check_token` get ignore-directive handling for free from the framework in `check.rs`. Rules using `check_root` must handle it themselves by calling the shared helpers `check_for_rule_ignore_enter` and `check_for_rule_ignore_leave` from `RuleExt` during tree traversal. They must also skip `SyntaxKind::ERROR` nodes manually. See `html_duplicate_id.rs` or `twig_block_duplicate.rs` for the pattern.

Every `check_root` rule should include tests for ignore-directive handling:
- Specific ignore (`{# ludtwig-ignore <rule-name> #}`) suppresses the diagnostic
- Blanket ignore (`{# ludtwig-ignore #}`) skips the subtree
- Diagnostics are still reported after the ignore directive's scope ends
- Ignoring a different rule name does not suppress this rule

### Testing patterns

- **Snapshot tests**: uses `expect-test` crate — call `.assert_eq()` on expected strings, run tests with `UPDATE_EXPECT=1` to auto-update snapshots
- **Rule tests**: `test_rule(name, code, expected_diagnostics)` and `test_rule_fix(name, code, expected_fixed_code)` in `check/rules/test` module
- **Parser tests**: inline snapshot tests in grammar modules verifying tree structure

### Threading model

Files processed in parallel via `rayon::scope` + `WalkBuilder::run_parallel`. Each file is parsed and checked single-threaded. Results sent to a dedicated output thread via MPSC channel.

### Configuration

Three-tier precedence: env vars (`LUDTWIG_` prefix) > `ludtwig-config.toml` file > embedded defaults. Config struct in `config.rs`, default template in `crates/ludtwig/ludtwig-config.toml`.
