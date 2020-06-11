//! JsDoc parser

use swc_ecma_ast::Str;

pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{jsdoc::lexer::Lexer, with_test_sess};
    use swc_common::DUMMY_SP;
    use swc_ecma_ast::JsDoc;

    fn parse(s: &str) -> JsDoc {
        with_test_sess(s, |session, input| {
            let mut p = parser::Parser::new(&session.handler, Lexer::new(&session.handler, input));

            p.parse().map_err(|mut e| {
                e.emit();
                ()
            })
        })
        .unwrap()
    }

    fn s(s: &str) -> Str {
        Str {
            span: DUMMY_SP,
            value: s.into(),
            has_escape: false,
        }
    }

    #[test]
    fn functions() {
        let res = parse(
            "/**
 * This is a function.
 *
 * @param {string} n - A string param
 * @return {string} A good string
 *
 * @example
 *
 *     foo('hello')
 */",
        );

        assert_eq!(
            res,
            JsDoc {
                span: Default::default(),
                description: s("This is a function."),
                tags: vec![]
            }
        )
    }
}
