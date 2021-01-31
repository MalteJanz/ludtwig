# NEXT-VERSION

# v0.3.0
- Performance increase of around 91% (almost doubled) in some cases.
  This was made possible by switching the async runtime from tokio to async-std and doing
  some optimizations of the directory traversal (which was blocking code).
- \[BREAKING\] Improved directory traversal by skipping hidden directories and files

# v0.2.1
- Bump version for crates.io release.
- Extracted parser code into it's own repository and public crate.

# v0.2.0
- [FEATURE #4](https://github.com/MalteJanz/ludtwig/issues/4):
  Implemented twig syntax support for `if` / `elseif` / `else` / `for` / `apply` / `set` (without `=`) blocks.
  Also any other non hierarchical twig statement `{% ... %}` will now be parsed (but not validated)
- [FEATURE #16](https://github.com/MalteJanz/ludtwig/issues/16):
  Implemented support for twig comments and structures like `if` / `block` / ... in the attributes of an html tag.
  Tag attributes are only a subset of the AST structure and their children only can contain other Tag attributes (for example no html tag as an html attribute is possible).
- [FEATURE #19](https://github.com/MalteJanz/ludtwig/issues/19):
  Enabled parsing support for output expressions as html tag attribute names and allow any twig structure or output expression inside attribute values.
  This also allows nested quotes `"` that are inside twig syntax inside of a value like  
  `alt="{{ "something.other"|trans|striptags }}"`
- [REMOVED #21](https://github.com/MalteJanz/ludtwig/issues/21)
  Removed example analyzer that has checked that twig block names contain their parent block name.
  The reasoning behind this is that it looks not very useful in practice and clutters the output with warnings.
- [FEATURE]
  Added new analyzer that checks for duplicate twig block names.
- \[BUGFIX\]: 
  Improved readability for parsing errors by displaying the error and its context in a way that is readable by the user.
  For example missing closing tags will give the context with each attribute to identify the right tag.
  The attributes were not displayed in a user readable way before this change.
  