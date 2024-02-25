# NEXT-VERSION

- The used ludtwig version is now printed out every time ludtwig is executed
- [#90](https://github.com/MalteJanz/ludtwig/issues/90) Print out the used config file or environment variable overrides
- [#90](https://github.com/MalteJanz/ludtwig/issues/90) Added `--verbose` CLI flag to print out all used config values
  and maybe more details in the future

# v0.5.2

- Updated dependencies to latest versions

# v0.5.1

- [#79](https://github.com/MalteJanz/ludtwig/issues/79) Fix two failing tests when running `cargo test --release`
- [#82](https://github.com/MalteJanz/ludtwig/issues/82) Fix parsing of unquoted twig hash syntax, when it only consists
  of a single underscore

# v0.5.0

- Fix HTML Tag name parser token collisions, which caused tags like `source` to not parse correctly
- Fix Twig accessor parser to allow array indexing by dot operator
- Fix lexer and parsing of colon word syntax like `:special-attribute` as HTML attribute and now support `array[:upper]`
  as Twig slice

# v0.4.0

- Rewrite of the whole parser to make it lossless and implemented error recovery

# v0.3.3

- Hotfix: fixed another panic (attempt to subtract with overflow) for the parsing error line and column numbers
  reconstruction.

# v0.3.2

- Hotfix: fixed a panic (array index out of bounds) for the parsing error line and column numbers reconstruction.

# v0.3.1

- [Bugfix #2](https://github.com/MalteJanz/ludtwig-parser/issues/2):
  Line and column numbers for the human-readable errors should now be correct regardless of the line ending.
  They now also work correctly with utf-8 files.

# v0.3.0

- Mostly fixed a performance regression:
  Average template parsing performance is increased by ~295% since last version \[266us -> 87us\].
  It is still ~10% \[from 78us\] behind the performance before the advanced html tag attribute parsing was implemented
  (but this is expected).
- Added iterator implementations for the AST structures.
  There is one basic iterator which visits all nodes in the AST and a advanced iterator which visits all nodes together
  with a context.
  This context contains information about it's neighbor nodes and so on.
- Removed `HasChildren` Trait because it was not as useful as it seems and was not implemented for all AST types.

# v0.2.1

Initial release as a separate crate from ludtwig.
