#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // panics if parser did not consume all tokens
        let parse = ludtwig_parser::parse(s);

        // just to be sure the parsed tree text and input text are equal
        let root = ludtwig_parser::syntax::untyped::SyntaxNode::new_root(parse.green_node);
        assert_eq!(root.text(), s);
    }
});
