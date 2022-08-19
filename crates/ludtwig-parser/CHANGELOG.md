# NEXT-VERSION

# v0.3.3
- Hotfix: fixed another panic (attempt to subtract with overflow) for the parsing error line and column numbers reconstruction.

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
  There is one basic iterator which visits all nodes in the AST and a advanced iterator which visits all nodes together with a context.
  This context contains information about it's neighbor nodes and so on.
- Removed `HasChildren` Trait because it was not as useful as it seems and was not implemented for all AST types.

# v0.2.1
Initial release as a separate crate from ludtwig.
