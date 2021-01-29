# NEXT-VERSION
- [FEATURE #4](https://github.com/MalteJanz/ludtwig/issues/4):
  Implemented twig syntax support for `if` / `elseif` / `else` / `for` / `apply` / `set` (without `=`) blocks.
  Also any other non hierarchical twig statement `{% ... %}` will now be parsed (but not validated)
- [FEATURE #16](https://github.com/MalteJanz/ludtwig/issues/16):
  Implemented support for twig comments and structures like `if` / `block` / ... in the attributes of an html tag.
  Tag attributes are only a subset of the AST structure and their children only can contain other Tag attributes (for example no html tag as an html attribute is possible).
- \[BUG\]: 
  Improved readability for parsing errors by displaying the error and its context in a way that is readable by the user.
  For example missing closing tags will give the context with each attribute to identify the right tag.
  The attributes were not displayed in a user readable way before this change.
