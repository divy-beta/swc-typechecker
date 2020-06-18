use crate::{parser::input::Buffer, token::Token, PResult, Session, Tokens};
use swc_common::{errors::Handler, BytePos};
use swc_ecma_ast::*;
use swc_ecma_parser_macros::parser;

#[derive(Clone)]
pub struct Parser<'a, I: Tokens> {
    session: Session<'a>,
    input: Buffer<I>,
}

impl<'a, I> Parser<'a, I>
where
    I: Tokens,
{
    pub fn new(handler: &'a Handler, input: I) -> Self {
        Self {
            session: Session { handler },
            input: Buffer::new(input),
        }
    }
}

#[parser]
impl<'a, I> Parser<'a, I>
where
    I: Tokens,
{
    pub fn parse(&mut self) -> PResult<'a, JsDoc> {
        let start = cur_pos!();
        let description = self.parse_str_until(true, |_| true)?;
        let tags = self.parse_tags()?;

        Ok(JsDoc {
            span: span!(start),
            description,
            tags,
        })
    }

    fn parse_tags(&mut self) -> PResult<'a, Vec<JsDocTagItem>> {
        let mut tags = vec![];

        while is!('@') {
            let tag = self.parse_tag()?;
            tags.push(tag);
        }

        Ok(tags)
    }

    fn parse_tag(&mut self) -> PResult<'a, JsDocTagItem> {
        let start = cur_pos!();
        expect!('@');

        if !is!(IdentName) {
            unexpected!()
        }

        let tag_name = bump!();
        let tag_name = match tag_name {
            Token::Word(word) => Ident::new(word.into(), span!(start)),
            _ => unreachable!(),
        };

        let tag = match &*tag_name.sym {
            "abstract" | "virtual" => self.parse_unknown_tag(start)?,

            "access" => self.parse_unknown_tag(start)?,

            "async" => self.parse_unknown_tag(start)?,

            "augments" | "extends" => {
                let class = self.parse_expr_with_type_args()?;

                JsDocTag::Augments(JsDocAugmentsTag {
                    class,
                    span: span!(start),
                })
            }

            "author" => {
                let author = self.parse_str_until(false, |_| true)?;

                JsDocTag::Author(JsDocAuthorTag {
                    span: span!(start),
                    author,
                })
            }

            "borrow" => self.parse_unknown_tag(start)?,

            // "callback" => unimplemented!("jsdoc parser: @callback"),
            "constructor" | "class" => JsDocTag::Class(JsDocClassTag { span: span!(start) }),

            "classdesc" => self.parse_unknown_tag(start)?,

            "constant" | "const" => self.parse_unknown_tag(start)?,

            "constructs" => self.parse_unknown_tag(start)?,

            "copyright" => self.parse_unknown_tag(start)?,

            "defaultvalue" | "default" => self.parse_unknown_tag(start)?,

            "deprecated" => self.parse_unknown_tag(start)?,

            "description" | "desc" => self.parse_unknown_tag(start)?,

            "enum" => JsDocTag::Enum(JsDocEnumTag { span: span!(start) }),

            "event" => self.parse_unknown_tag(start)?,

            "example" => self.parse_unknown_tag(start)?,

            "exports" => self.parse_unknown_tag(start)?,

            "external" | "host" => self.parse_unknown_tag(start)?,

            "file" | "fileoverview" | "overview" => self.parse_unknown_tag(start)?,

            "fires" | "emits" => self.parse_unknown_tag(start)?,

            "function" | "func" | "method" => self.parse_unknown_tag(start)?,

            "generator" => self.parse_unknown_tag(start)?,

            "global" => self.parse_unknown_tag(start)?,

            "hideconstructor" => self.parse_unknown_tag(start)?,

            "ignore" => self.parse_unknown_tag(start)?,

            "implements" => {
                let class = self.parse_expr_with_type_args()?;

                JsDocTag::Implements(JsDocImplementsTag {
                    span: span!(start),
                    class,
                })
            }

            "inheritdoc" => self.parse_unknown_tag(start)?,

            "inner" => self.parse_unknown_tag(start)?,

            "instance" => self.parse_unknown_tag(start)?,

            "interface" => self.parse_unknown_tag(start)?,

            "kind" => self.parse_unknown_tag(start)?,

            "lends" => self.parse_unknown_tag(start)?,

            "license" => self.parse_unknown_tag(start)?,

            "listens" => self.parse_unknown_tag(start)?,

            "var" | "member" => self.parse_unknown_tag(start)?,

            "memberof" => self.parse_unknown_tag(start)?,

            "mixes" => self.parse_unknown_tag(start)?,

            "mixin" => self.parse_unknown_tag(start)?,

            "module" => self.parse_unknown_tag(start)?,

            "name" => self.parse_unknown_tag(start)?,

            "namespace" => self.parse_unknown_tag(start)?,

            "override" => self.parse_unknown_tag(start)?,

            "package" => self.parse_unknown_tag(start)?,

            "param" | "arg" | "argument" => {
                let name = self.parse_entity_name()?;
                let type_expr = self.parse_opt_type_expr()?;

                JsDocTag::Parameter(JsDocParameterTag {
                    span: span!(start),
                    name,
                    type_expr,
                    // TODO
                    is_name_first: false,
                    // TODO
                    is_bracketed: false,
                })
            }

            "private" => JsDocTag::Private(JsDocPrivateTag { span: span!(start) }),

            "property" | "prop" => {
                let name = self.parse_entity_name()?;
                let type_expr = self.parse_opt_type_expr()?;

                JsDocTag::Property(JsDocPropertyTag {
                    span: span!(start),
                    name,
                    type_expr,
                    // TODO
                    is_name_first: false,
                    // TODO
                    is_bracketed: false,
                })
            }

            "protected" => JsDocTag::Protected(JsDocProtectedTag { span: span!(start) }),

            "public" => JsDocTag::Public(JsDocPublicTag { span: span!(start) }),

            "readonly" => JsDocTag::Readonly(JsDocReadonlyTag { span: span!(start) }),

            "requires" => self.parse_unknown_tag(start)?,

            "returns" | "return" => {
                let type_expr = self.parse_opt_type_expr()?;
                JsDocTag::Return(JsDocReturnTag {
                    span: span!(start),
                    type_expr,
                })
            }

            "see" => self.parse_unknown_tag(start)?,

            "since" => self.parse_unknown_tag(start)?,

            "static" => self.parse_unknown_tag(start)?,

            "summary" => self.parse_unknown_tag(start)?,

            "this" => JsDocTag::This(JsDocThisTag { span: span!(start) }),

            "throws" | "exception" => self.parse_unknown_tag(start)?,

            "todo" => self.parse_unknown_tag(start)?,

            "tutorial" => self.parse_unknown_tag(start)?,

            "type" => {
                let type_expr = self.parse_type_expr()?;

                JsDocTag::Type(JsDocTypeTag {
                    span: span!(start),
                    type_expr,
                })
            }

            "typedef" => {
                let type_expr = self.parse_opt_type_expr_or_type_lit()?;

                let name = self.parse_opt_ident()?;

                let full_name = self.parse_opt_namespace_body()?;
                JsDocTag::Typedef(JsDocTypedefTag {
                    span: span!(start),
                    full_name,
                    name,
                    type_expr,
                })
            }

            "variation" => self.parse_unknown_tag(start)?,

            "version" => self.parse_unknown_tag(start)?,

            "yields" | "yield" => self.parse_unknown_tag(start)?,

            _ => self.parse_unknown_tag(start)?,
        };

        let span = span!(start);
        Ok(JsDocTagItem {
            span,
            tag_name,
            tag,
        })
    }

    fn parse_entity_name(&mut self) -> PResult<'a, TsEntityName> {
        let mut obj = match self.parse_opt_ident()? {
            None => unexpected!(),
            Some(v) => TsEntityName::Ident(v),
        };

        while is!(IdentName) {
            match self.parse_opt_ident()? {
                Some(v) => {
                    obj = TsEntityName::TsQualifiedName(Box::new(TsQualifiedName {
                        left: obj,
                        right: v,
                    }));
                }
                None => break,
            }
        }

        Ok(obj)
    }

    fn parse_unknown_tag(&mut self, start: BytePos) -> PResult<'a, JsDocTag> {
        let extras = self.parse_str_until(false, |_| true)?;
        Ok(JsDocTag::Unknown(JsDocUnknownTag {
            span: span!(start),
            extras,
        }))
    }

    fn parse_str_until<F>(&mut self, include_new_line: bool, op: F) -> PResult<'a, Str>
    where
        F: Fn(&Str) -> bool,
    {
    }

    fn parse_type(&mut self) -> PResult<'a, JsDocType> {
        let start = cur_pos!();
        match cur!(true)? {
            tok!('*') => {
                bump!();
                return Ok(JsDocType::All(JsDocAllType { span: span!(start) }));
            }
            tok!('?') => {
                bump!();
                return Ok(JsDocType::Unknown(JsDocUnknownType { span: span!(start) }));
            }
            _ => {}
        }

        //
    }

    fn parse_opt_type_expr(&mut self) -> PResult<'a, Option<JsDocTypeExpr>> {}

    fn parse_type_expr(&mut self) -> PResult<'a, JsDocTypeExpr> {}

    fn parse_expr_with_type_args(&mut self) -> PResult<'a, JsDocExprWithTypeArgs> {}

    fn parse_opt_namespace_body(&mut self) -> PResult<'a, Option<JsDocNamespaceBody>> {}

    fn parse_opt_type_expr_or_type_lit(&mut self) -> PResult<'a, Option<JsDocTypeExprOrTypeLit>> {}

    fn parse_opt_ident(&mut self) -> PResult<'a, Option<Ident>> {
        let span = self.input.cur_span();
        if is!(IdentName) {
            let sym = match bump!() {
                Token::Word(w) => w.into(),
                _ => unreachable!(),
            };
            Ok(Some(Ident::new(sym, span)))
        } else {
            Ok(None)
        }
    }
}
