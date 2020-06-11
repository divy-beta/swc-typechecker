use crate::{parser::input::Buffer, PResult, Tokens};
use swc_common::errors::Handler;
use swc_ecma_ast::*;
use swc_ecma_parser_macros::parser;

#[derive(Clone)]
pub struct Parser<'a, I: Tokens> {
    handler: &'a Handler,
    input: Buffer<I>,
}

impl<'a, I> Parser<'a, I>
where
    I: Tokens,
{
    pub fn new(handler: &'a Handler, input: I) -> Self {
        Self {
            handler,
            input: Buffer::new(input),
        }
    }
}

#[parser]
impl<'a, I> Parser<'a, I>
where
    I: Tokens,
{
    pub fn parse(&mut self) -> PResult<'a, JsDoc> {}

    fn parse_tags(&mut self) -> PResult<'a, Vec<JsDocTag>> {}

    fn parse_tag(&mut self) -> PResult<'a, Vec<JsDocTag>> {}

    fn parse_type(&mut self) -> PResult<'a, JsDocType> {
        let start = self.input.cur_pos();
    }
}
