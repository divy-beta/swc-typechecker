macro_rules! tok {
    ('`') => {
        crate::token::Token::BackQuote
    };
    // (';') => { Token::Semi };
    ('@') => {
        crate::token::Token::At
    };
    ('#') => {
        crate::token::Token::Hash
    };

    ('&') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::BitAnd)
    };
    ('+') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Add)
    };
    ('-') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Sub)
    };
    ("??") => {
        crate::token::Token::BinOp(crate::token::BinOpToken::NullishCoalescing)
    };
    ('~') => {
        crate::token::Token::Tilde
    };
    ('!') => {
        crate::token::Token::Bang
    };

    ('|') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::BitOr)
    };

    (',') => {
        crate::token::Token::Comma
    };
    ('?') => {
        crate::token::Token::QuestionMark
    };
    (':') => {
        crate::token::Token::Colon
    };
    ("::") => {
        crate::token::Token::ColonColon
    };
    ('.') => {
        crate::token::Token::Dot
    };
    ("=>") => {
        crate::token::Token::Arrow
    };
    ("...") => {
        crate::token::Token::DotDotDot
    };
    ("${") => {
        crate::token::Token::DollarLBrace
    };

    ('+') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Add)
    };
    ('-') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Sub)
    };
    ('*') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Mul)
    };
    ('/') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Div)
    };
    ("/=") => {
        crate::token::Token::AssignOp(DivAssign)
    };
    ('%') => {
        crate::token::Token::BinOp(Mod)
    };
    ('~') => {
        crate::token::Token::Tilde
    };
    ('<') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Lt)
    };
    ('>') => {
        crate::token::Token::BinOp(crate::token::BinOpToken::Gt)
    };

    ("++") => {
        crate::token::Token::PlusPlus
    };
    ("--") => {
        crate::token::Token::MinusMinus
    };

    ('=') => {
        crate::token::Token::AssignOp(crate::token::AssignOpToken::Assign)
    };

    ('(') => {
        crate::token::Token::LParen
    };
    (')') => {
        crate::token::Token::RParen
    };
    ('{') => {
        crate::token::Token::LBrace
    };
    ('}') => {
        crate::token::Token::RBrace
    };
    ('[') => {
        crate::token::Token::LBracket
    };
    (']') => {
        crate::token::Token::RBracket
    };

    ("async") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("async")))
    };
    ("as") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("as")))
    };
    ("await") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Await))
    };
    ("break") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Break))
    };
    ("case") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Case))
    };
    ("catch") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Catch))
    };
    ("class") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Class))
    };
    ("const") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Const))
    };
    ("continue") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Continue))
    };
    ("debugger") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Debugger))
    };
    ("default") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Default_))
    };
    ("delete") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Delete))
    };
    ("do") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Do))
    };
    ("else") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Else))
    };
    ("export") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Export))
    };
    ("extends") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Extends))
    };
    ("false") => {
        crate::token::Token::Word(crate::token::Word::False)
    };
    ("finally") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Finally))
    };
    ("for") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::For))
    };
    ("from") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("from")))
    };
    ("function") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Function))
    };
    ("if") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::If))
    };
    ("in") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::In))
    };
    ("import") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Import))
    };
    ("let") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Let))
    };
    ("new") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::New))
    };
    ("null") => {
        crate::token::Token::Word(crate::token::Word::Null)
    };
    ("of") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("of")))
    };
    ("return") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Return))
    };
    ("super") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Super))
    };
    ("static") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("static")))
    };
    ("switch") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Switch))
    };
    ("target") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("target")))
    };
    ("this") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::This))
    };
    ("throw") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Throw))
    };
    ("true") => {
        crate::token::Token::Word(crate::token::Word::True)
    };
    ("try") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Try))
    };
    ("typeof") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::TypeOf))
    };
    ("var") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Var))
    };
    ("void") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Void))
    };
    ("while") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::While))
    };
    ("with") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::With))
    };
    ("yield") => {
        crate::token::Token::Word(crate::token::Word::Keyword(crate::token::Keyword::Yield))
    };

    // ----------
    // JSX
    // ----------
    (JSXTagStart) => {
        crate::token::Token::JSXTagStart
    };

    (JSXTagEnd) => {
        crate::token::Token::JSXTagEnd
    };

    // ----------
    // Typescript
    // ----------
    ("asserts") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("asserts")))
    };
    ("implements") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("implements")))
    };
    ("is") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("is")))
    };
    ("new") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("new")))
    };
    ("keyof") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("keyof")))
    };
    ("unique") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("unique")))
    };
    ("object") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("object")))
    };
    ("global") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("global")))
    };
    ("require") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("require")))
    };
    ("enum") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("enum")))
    };
    ("readonly") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("readonly")))
    };
    ("as") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("as")))
    };
    ("namespace") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("namespace")))
    };
    ("abstract") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("abstract")))
    };
    ("infer") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("infer")))
    };
    ("any") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("any")))
    };
    ("boolean") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("boolean")))
    };
    ("bigint") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("bigint")))
    };
    ("never") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("never")))
    };
    ("number") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("number")))
    };
    ("string") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("string")))
    };
    ("symbol") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("symbol")))
    };
    ("unknown") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("unknown")))
    };
    ("require") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("require")))
    };
    ("interface") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("interface")))
    };
    ("declare") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("declare")))
    };
    ("undefined") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("undefined")))
    };
    ("meta") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("meta")))
    };
    ("type") => {
        crate::token::Token::Word(crate::token::Word::Ident(swc_atoms::js_word!("type")))
    };
}

macro_rules! token_including_semi {
    (';') => {
        Token::Semi
    };
    ($t:tt) => {
        tok!($t)
    };
}

macro_rules! unexpected {
    ($p:expr) => {{
        let got = format!("{:?}", cur!($p, false).ok());
        syntax_error!(
            $p,
            $p.input.cur_span(),
            $crate::error::SyntaxError::Unexpected { got }
        )
    }};
}

/// This handles automatic semicolon insertion.
///
/// Returns bool.
macro_rules! is {
    ($p:expr, BindingIdent) => {{
        let ctx = $p.ctx();
        match cur!($p, false) {
            Ok(&$crate::token::Token::Word(ref w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($p:expr, IdentRef) => {{
        let ctx = $p.ctx();
        match cur!($p, false) {
            Ok(&$crate::token::Token::Word(ref w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($p:expr,IdentName) => {{
        match cur!($p, false) {
            Ok(&$crate::token::Token::Word(..)) => true,
            _ => false,
        }
    }};

    ($p:expr,';') => {{
        $p.input.is(&Token::Semi)
            || eof!($p)
            || is!($p, '}')
            || $p.input.had_line_break_before_cur()
    }};

    ($p:expr, $t:tt) => {
        $p.input.is(&tok!($t))
    };
}

macro_rules! peeked_is {
    ($p:expr, BindingIdent) => {{
        let ctx = $p.ctx();
        match peek!($p) {
            Ok(&$crate::token::Token::Word(ref w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($p:expr, IdentRef) => {{
        let ctx = $p.ctx();
        match peek!($p) {
            Ok(&Word(ref w)) => !ctx.is_reserved_word(&w.cow()),
            _ => false,
        }
    }};

    ($p:expr,IdentName) => {{
        match peek!($p) {
            Ok(&Word(..)) => true,
            _ => false,
        }
    }};

    ($p:expr, ';') => {{
        compile_error!("peeked_is!(';') is invalid");
    }};

    ($p:expr, $t:tt) => {
        $p.input.peeked_is(&tok!($t))
    };
}

/// Returns true on eof.
macro_rules! eof {
    ($p:expr) => {
        cur!($p, false).is_err()
    };
}

macro_rules! is_one_of {
    ($p:expr, $($t:tt),+) => {{
        false
        $(
            || is!($p, $t)
        )*
    }};
}

// This will panic if current != token
macro_rules! assert_and_bump {
    ($p:expr, $t:tt) => {{
        const TOKEN: &Token = &tok!($t);
        if cfg!(debug_assertions) && !$p.input.is(TOKEN) {
            unreachable!(
                "assertion failed: expected {:?}, got {:?}",
                TOKEN,
                $p.input.cur()
            );
        }
        let _ = cur!($p, true)?;
        bump!($p);
    }};
}

/// This handles automatic semicolon insertion.
///
/// Returns bool if token is static, and Option<Token>
///     if token has data like string.
macro_rules! eat {
    ($p:expr, ';') => {{
        log::trace!("eat(';'): cur={:?}", cur!($p, true));
        $p.input.eat(&Token::Semi)
            || eof!($p)
            || is!($p, '}')
            || $p.input.had_line_break_before_cur()
    }};

    ($p:expr, $t:tt) => {{
        if is!($p, $t) {
            bump!($p);
            true
        } else {
            false
        }
    }};
}

macro_rules! eat_exact {
    ($p:expr, $t:tt) => {{
        const TOKEN: &Token = &token_including_semi!($t);
        if $p.input.is(TOKEN) {
            bump!($p);
            true
        } else {
            false
        }
    }};
}

macro_rules! is_exact {
    ($p:expr, $t:tt) => {{
        const TOKEN: &Token = &token_including_semi!($t);
        $p.input.is(TOKEN)
    }};
}

/// This handles automatic semicolon insertion.
macro_rules! expect {
    ($p:expr, $t:tt) => {{
        const TOKEN: &Token = &token_including_semi!($t);
        if !eat!($p, $t) {
            let cur = format!("{:?}", cur!($p, false).ok());
            syntax_error!(
                $p,
                $p.input.cur_span(),
                $crate::error::SyntaxError::Expected(TOKEN, cur)
            )
        }
    }};
}

macro_rules! expect_exact {
    ($p:expr, $t:tt) => {{
        const TOKEN: &Token = &token_including_semi!($t);
        if !eat_exact!($p, $t) {
            let cur = format!("{:?}", cur!($p, false).ok());
            syntax_error!($p, $p.input.cur_span(), SyntaxError::Expected(TOKEN, cur))
        }
    }};
}

macro_rules! store {
    ($p:expr, $t:tt) => {{
        const TOKEN: Token = token_including_semi!($t);

        $p.input.store(TOKEN);
    }};
}

/// cur!($parser, required:bool)
macro_rules! cur {
    ($p:expr, $required:expr) => {{
        let pos = $p.input.last_pos();
        let last = swc_common::Span::new(pos, pos, Default::default());
        let is_err_token = match $p.input.cur() {
            Some(&$crate::token::Token::Error(..)) => true,
            _ => false,
        };
        if is_err_token {
            match $p.input.bump() {
                $crate::token::Token::Error(e) => {
                    let err =
                        ::swc_common::errors::DiagnosticBuilder::from($crate::error::ErrorToDiag {
                            handler: &$p.session.handler,
                            span: e.span,
                            error: e.error,
                        });
                    return Err(err.into());
                }
                _ => unreachable!(),
            }
        }

        match $p.input.cur() {
            Some(c) => Ok(c),
            None => {
                if $required {
                    let err = ::swc_common::errors::DiagnosticBuilder::from($crate::error::Eof {
                        last,
                        handler: &$p.session.handler,
                    });
                    return Err(err.into());
                }
                Err($crate::error::Eof {
                    last,
                    handler: &$p.session.handler,
                })
            }
        }
    }};
}

macro_rules! peek {
    ($p:expr) => {{
        debug_assert!(
            $p.input.knows_cur(),
            "parser should not call peek() without knowing current token.
Current token is {:?}",
            cur!($p, false),
        );

        let pos = cur_pos!($p);
        let last = Span::new(pos, pos, Default::default());
        match $p.input.peek() {
            Some(c) => Ok(c),
            None => {
                let err = ::swc_common::errors::DiagnosticBuilder::from($crate::error::Eof {
                    //TODO: Use whole span
                    last,
                    handler: &$p.session.handler,
                });
                Err(err)
            }
        }
    }};
}

macro_rules! bump {
    ($p:expr) => {{
        debug_assert!(
            $p.input.knows_cur(),
            "parser should not call bump() without knowing current token"
        );
        $p.input.bump()
    }};
}

macro_rules! cur_pos {
    ($p:expr) => {{
        $p.input.cur_pos()
    }};
}

macro_rules! last_pos {
    ($p:expr) => {
        $p.input.prev_span().hi
    };
}

macro_rules! return_if_arrow {
    ($p:expr, $expr:expr) => {{
        // FIXME:
        //
        //

        // let is_cur = match $p.state.potential_arrow_start {
        //     Some(start) => $expr.span.lo() == start,
        //     None => false
        // };
        // if is_cur {
        if let Expr::Arrow { .. } = *$expr {
            return Ok($expr);
        }
        // }
    }};
}

macro_rules! trace_cur {
    ($p:expr, $name:ident) => {{
        // println!("{}: {:?}", stringify!($name), cur!($p, false));
    }};
}

/// This macro requires macro named 'last_pos' to be in scope.
macro_rules! span {
    ($p:expr, $start:expr) => {{
        let start: ::swc_common::BytePos = $start;
        let end: ::swc_common::BytePos = last_pos!($p);
        if cfg!(debug_assertions) && start > end {
            unreachable!(
                "assertion failed: (span.start <= span.end).
 start = {}, end = {}",
                start.0, end.0
            )
        }
        ::swc_common::Span::new(start, end, ::swc_common::SyntaxContext::empty())
    }};
}

macro_rules! make_error {
    ($p:expr, $span:expr, $err:expr) => {{
        ::swc_common::errors::DiagnosticBuilder::from($crate::error::ErrorToDiag {
            handler: $p.session.handler,
            span: $span,
            error: $err,
        })
    }};
}

macro_rules! syntax_error {
    ($p:expr, $err:expr) => {
        syntax_error!($p, $p.input.cur_span(), $err)
    };

    ($p:expr, $span:expr, $err:expr) => {{
        let err = make_error!($p, $span, $err);
        return Err(err.into());
    }};
}
