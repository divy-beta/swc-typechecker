//! JsDoc parser

use crate::{
    error::{Eof, Error, ErrorToDiag, SyntaxError},
    token::Token,
    PResult,
};
use swc_common::{comments::Comment, errors::Handler, BytePos, Span};
use swc_ecma_ast::Str;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct JsDoc {
    pub description: Str,
    pub items: Vec<JsDocItem>,
}

/// Starts with '@'
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum JsDocItem {
    Simple { ty: Str, key: Str, value: Str },
}

#[derive(Clone)]
pub struct JsDocParser<'a, 'b> {
    handler: &'a Handler,
    span: Span,
    text: &'b str,
    group_fin: Option<char>,
}

impl<'a, 'b> JsDocParser<'a, 'b> {
    pub fn new(handler: &'a Handler, span: Span, text: &'b str) -> Self {
        Self {
            handler,
            span,
            text,
            group_fin: None,
        }
    }
}

impl<'a, 'b> JsDocParser<'a, 'b> {
    pub fn parse(&mut self) -> PResult<'a, JsDoc> {
        let description = self.parse_description()?;
        let items = self.parse_items()?;

        Ok(JsDoc { description, items })
    }

    pub fn parse_description(&mut self) -> PResult<'a, Str> {
        let mut last = 0;
        for (i, c) in self.text.char_indices() {
            if c == '@' {
                break;
            }

            last = i;
        }

        Ok(self.bump(last))
    }

    pub fn parse_item(&mut self) -> PResult<'a, JsDocItem> {
        println!("parse_item: {}", self.text);

        if self.text.starts_with('@') {
            self.bump(1);

            let ty = self.parse_str(false)?;

            let key = self.parse_str(false)?;
            self.skip_ws_and_line_break();
            let value = self.parse_str(true)?;
            return Ok(JsDocItem::Simple { ty, key, value });
        }

        Err(ErrorToDiag {
            handler: self.handler,
            span: self.span,
            error: SyntaxError::Expected(&Token::At, self.text.to_string()),
        })?
    }

    fn parse_str(&mut self, allow_empty: bool) -> PResult<'a, Str> {
        println!("parse_str: {}", self.text);

        self.skip_ws_and_line_break();

        if self.text.is_empty() {
            if allow_empty {
                return Ok(Str {
                    span: self.span,
                    value: Default::default(),
                    has_escape: false,
                });
            } else {
                Err(Eof {
                    last: self.span,
                    handler: self.handler,
                })?;
            };
        }

        if self.text.starts_with('(') {
            return self.parse_group(')');
        }
        if self.text.starts_with('[') {
            return self.parse_group(']');
        }
        if self.text.starts_with('{') {
            return self.parse_group('}');
        }

        for (i, c) in self.text.char_indices() {
            if let Some(fin) = self.group_fin {
                if c == fin {
                    return Ok(self.bump(i));
                }
            }

            if self.group_fin.is_none() && c.is_ascii_whitespace() {
                return Ok(self.bump(i));
            }
        }

        Err(Eof {
            last: self.span,
            handler: self.handler,
        })?
    }

    fn parse_group(&mut self, fin: char) -> PResult<'a, Str> {
        self.bump(1);
        let old = self.group_fin;
        self.group_fin = Some(fin);

        let ret = self.parse_str(true)?;

        self.group_fin = old;

        Ok(ret)
    }

    pub fn parse_items(&mut self) -> PResult<'a, Vec<JsDocItem>> {
        let mut items = vec![];
        loop {
            self.skip_ws_and_line_break();
            items.push(self.parse_item()?);
        }

        Ok(items)
    }

    fn skip_ws_and_line_break(&mut self) {
        let mut remove_star = false;
        for (i, c) in self.text.char_indices() {
            if c.is_ascii_whitespace() {
                continue;
            }

            self.bump(i);
            return;
        }
    }

    fn bump(&mut self, n: usize) -> Str {
        let value = &self.text[..n];
        self.text = &self.text[n..];

        let span = if self.span.is_dummy() {
            self.span
        } else {
            let span = self.span.with_hi(self.span.lo() + BytePos(n as _));
            self.span = self.span.with_lo(self.span.lo() + BytePos(n as _));
            span
        };

        Str {
            span,
            value: value.into(),
            has_escape: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use swc_common::DUMMY_SP;

    fn parse(s: &str) -> JsDoc {
        ::testing::run_test2(false, |cm, handler| {
            let mut p = JsDocParser::new(&handler, DUMMY_SP, s);

            Ok(p.parse().unwrap())
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
                description: s("This is a function."),
                items: vec![]
            }
        )
    }
}
