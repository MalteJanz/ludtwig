use crate::parser::Parser;
use crate::syntax::untyped::SyntaxKind;
use crate::T;

pub(super) fn parse_twig_block(parser: &mut Parser) {
    debug_assert!(parser.at(T!["{%"]));
    let m = parser.start();

    parser.bump();
    parser.expect(T!["block"]);
    parser.expect(T![word]);
    parser.expect(T!["%}"]);

    parser.complete(m, SyntaxKind::TWIG_STARTING_BLOCK);
}

#[cfg(test)]
mod tests {
    use crate::parser::check_tree;
    use expect_test::expect;

    #[test]
    fn parse_twig_block_opening() {
        check_tree(
            "{% block block_name %}",
            expect![[r#"
            ROOT@0..22
              TWIG_STARTING_BLOCK@0..22
                TK_CURLY_PERCENT@0..2 "{%"
                TK_WHITESPACE@2..3 " "
                TK_BLOCK@3..8 "block"
                TK_WHITESPACE@8..9 " "
                TK_WORD@9..19 "block_name"
                TK_WHITESPACE@19..20 " "
                TK_PERCENT_CURLY@20..22 "%}""#]],
        );
    }
}
