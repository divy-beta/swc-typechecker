use crate::{lexer::TokenContexts, token::TokenAndSpan, Context, JscTarget, Syntax, Tokens};
use swc_common::{errors::Handler, input::Input, BytePos, Span, SpanData, SyntaxContext};

#[derive(Clone, Default)]
struct State {}

#[derive(Clone)]
pub struct Lexer<'a, I: Input> {
    handler: &'a Handler,
    input: I,
    state: State,
}

impl<'a, I> Lexer<'a, I>
where
    I: Input,
{
    pub fn new(handler: &'a Handler, input: I) -> Self {
        Self {
            handler,
            input,
            state: Default::default(),
        }
    }
}

impl<I> Iterator for Lexer<'_, I>
where
    I: Input,
{
    type Item = TokenAndSpan;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.input.cur()?;

        match c {
            '{' | '}' | '(' | ')' | '[' | ']' | '@' | '<' | '>' | '=' | ',' | '.' | '`' => {
                let token = match c {
                    '{' => tok!('{'),
                    '}' => tok!('}'),
                    '[' => tok!('['),
                    ']' => tok!(']'),
                    '(' => tok!('('),
                    ')' => tok!(')'),
                    '@' => tok!('@'),
                    '<' => tok!('<'),
                    '>' => tok!('>'),
                    '=' => tok!('='),
                    ',' => tok!(','),
                    '.' => tok!('.'),
                    '`' => tok!('`'),
                    _ => unreachable!(),
                };
                let lo = self.input.cur_pos();
                self.input.bump();
                let hi = lo + BytePos(1);
                return Some(TokenAndSpan {
                    token,
                    had_line_break: false,
                    span: SpanData {
                        lo,
                        hi,
                        ctxt: SyntaxContext::empty(),
                    },
                });
            }
        }

        None
    }
}

impl<I> Tokens for Lexer<'_, I>
where
    I: Input,
{
    fn set_ctx(&mut self, ctx: Context) {
        unreachable!("JsDoc::Lexer.set_ctx()")
    }

    fn ctx(&self) -> Context {
        unreachable!("JsDoc::Lexer.ctx()")
    }

    fn syntax(&self) -> Syntax {
        unreachable!("JsDoc::Lexer.syntax()")
    }

    fn target(&self) -> JscTarget {
        unreachable!("JsDoc::Lexer.target()")
    }

    fn set_expr_allowed(&mut self, _: bool) {
        unreachable!("JsDoc::Lexer.set_expr_allowed()")
    }

    fn token_context(&self) -> &TokenContexts {
        unreachable!("JsDoc::Lexer.token_context()")
    }

    fn token_context_mut(&mut self) -> &mut TokenContexts {
        unreachable!("JsDoc::Lexer.token_context_mut()")
    }

    fn set_token_context(&mut self, _c: TokenContexts) {
        unreachable!("JsDoc::Lexer.set_token_context()")
    }
}
