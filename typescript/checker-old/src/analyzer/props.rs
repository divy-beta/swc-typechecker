use super::{scope::ScopeKind, Analyzer};
use crate::{
    analyzer::{expr::TypeOfMode, util::ResultExt, Ctx},
    errors::{Error, Errors},
    ty::{MethodSignature, Operator, PropertySignature, Type, TypeElement},
    validator::{Validate, ValidateWith},
    ValidationResult,
};
use macros::{validator, validator_method};
use swc_atoms::js_word;
use swc_common::{Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

#[derive(Debug, Clone, Copy)]
pub(super) enum ComputedPropMode {
    Class {
        has_body: bool,
    },
    /// Object literal
    Object,

    Interface,
}

impl Visit<PropName> for Analyzer<'_, '_> {
    fn visit(&mut self, node: &PropName) {
        self.record(node);

        node.visit_children(self);
    }
}

impl Visit<ComputedPropName> for Analyzer<'_, '_> {
    #[validator_method]
    fn visit(&mut self, node: &ComputedPropName) {
        self.record(node);

        let mode = self.ctx.computed_prop_mode;

        let span = node.span;

        let is_symbol_access = match *node.expr {
            Expr::Member(MemberExpr {
                obj:
                    ExprOrSuper::Expr(box Expr::Ident(Ident {
                        sym: js_word!("Symbol"),
                        ..
                    })),
                ..
            }) => true,
            _ => false,
        };

        let mut errors = Errors::default();
        let ty = match self.validate(&node.expr) {
            Ok(ty) => ty,
            Err(err) => {
                match err {
                    Error::TS2585 { span } => Err(Error::TS2585 { span })?,
                    _ => {}
                }

                errors.push(err);
                // TODO: Change this to something else (maybe any)
                Type::unknown(span)
            }
        };

        match mode {
            ComputedPropMode::Class { .. } | ComputedPropMode::Interface => {
                let ty = self
                    .expand(node.span, ty.clone())
                    .store(&mut self.info.errors);

                if let Some(ref ty) = ty {
                    match *ty {
                        Type::Lit(..) => {}
                        _ if ty.is_kwd(TsKeywordTypeKind::TsSymbolKeyword)
                            || ty.is_unique_symbol() => {}
                        _ if is_symbol_access => {
                            errors.push(Error::NonSymbolTypedFieldFromSymbol {
                                span: node.expr.span(),
                            })
                        }
                        _ => match mode {
                            ComputedPropMode::Class { .. } => {
                                errors.push(Error::TS1168 { span: node.span })
                            }
                            ComputedPropMode::Interface => {
                                errors.push(Error::TS1169 { span: node.span })
                            }
                            _ => {}
                        },
                    }
                }
            }

            _ => {}
        }

        if match mode {
            ComputedPropMode::Class { has_body } => !has_body,
            ComputedPropMode::Object => errors.is_empty(),
            // TODO:
            ComputedPropMode::Interface => errors.is_empty(),
        } {
            let ty = ty.generalize_lit();
            match *ty.normalize() {
                Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsAnyKeyword,
                    ..
                })
                | Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsStringKeyword,
                    ..
                })
                | Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsNumberKeyword,
                    ..
                })
                | Type::Keyword(TsKeywordType {
                    kind: TsKeywordTypeKind::TsSymbolKeyword,
                    ..
                })
                | Type::Operator(Operator {
                    op: TsTypeOperatorOp::Unique,
                    ty:
                        box Type::Keyword(TsKeywordType {
                            kind: TsKeywordTypeKind::TsSymbolKeyword,
                            ..
                        }),
                    ..
                }) => {}
                _ if is_symbol_access => {}
                _ => errors.push(Error::TS2464 { span }),
            }
        }
        if !errors.is_empty() {
            Err(Error::Errors {
                span,
                errors: errors.into(),
            })?
        }
    }
}

#[validator]
impl Validate<Prop> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeElement>;

    fn validate(&mut self, prop: &Prop) -> Self::Output {
        self.record(prop);

        let ctx = Ctx {
            computed_prop_mode: ComputedPropMode::Object,
            ..self.ctx
        };
        self.with_ctx(ctx).validate_prop(prop)
    }
}

impl Analyzer<'_, '_> {
    fn validate_prop(&mut self, prop: &Prop) -> ValidationResult<TypeElement> {
        // TODO: Validate prop key

        match prop {
            Prop::Shorthand(ref i) => {
                // TODO: Check if RValue is correct
                self.type_of_ident(&i, TypeOfMode::RValue, None)
                    .store(&mut self.info.errors);
            }
            _ => {}
        }

        let span = prop.span();

        Ok(match *prop {
            Prop::Shorthand(..) => PropertySignature {
                span: prop.span(),
                key: prop_key_to_expr(&prop),
                params: Default::default(),
                optional: false,
                readonly: false,
                computed: false,
                type_ann: Default::default(),
                type_params: Default::default(),
            }
            .into(),

            Prop::KeyValue(ref kv) => {
                let ty = kv.value.validate_with(self)?;

                PropertySignature {
                    span: prop.span(),
                    key: prop_key_to_expr(&prop),
                    params: Default::default(),
                    optional: false,
                    readonly: false,
                    computed: false,
                    type_ann: Some(ty),
                    type_params: Default::default(),
                }
                .into()
            }

            Prop::Assign(ref p) => unimplemented!("type_of_prop(AssignProperty): {:?}", p),
            Prop::Getter(ref p) => p.validate_with(self)?,
            Prop::Setter(ref p) => self.with_child(
                ScopeKind::Fn,
                Default::default(),
                |child| -> ValidationResult<_> {
                    Ok(PropertySignature {
                        span: prop.span(),
                        key: prop_key_to_expr(&prop),
                        params: vec![p.param.validate_with(child)?],
                        optional: false,
                        readonly: false,
                        computed: false,
                        type_ann: None,
                        type_params: Default::default(),
                    }
                    .into())
                },
            )?,

            Prop::Method(ref p) => MethodSignature {
                span,
                readonly: false,
                key: prop_key_to_expr(&prop),
                computed: false,
                optional: false,
                params: p.function.params.validate_with(self)?,
                ret_ty: try_opt!(p.function.return_type.validate_with(self)),
                type_params: try_opt!(p.function.type_params.validate_with(self)),
            }
            .into(),
        })
    }
}

#[validator]
impl Validate<GetterProp> for Analyzer<'_, '_> {
    type Output = ValidationResult<TypeElement>;

    fn validate(&mut self, n: &GetterProp) -> Self::Output {
        self.record(n);

        let type_ann = self
            .with_child(ScopeKind::Fn, Default::default(), |child| {
                n.key.visit_with(child);

                if let Some(body) = &n.body {
                    let ret_ty = child.visit_stmts_for_return(&body.stmts)?;
                    if let None = ret_ty {
                        // getter property must have return statements.
                        child.info.errors.push(Error::TS2378 { span: n.key.span() });
                    }

                    return Ok(ret_ty);
                }

                Ok(None)
            })
            .store(&mut self.info.errors)
            .flatten();

        Ok(PropertySignature {
            span: n.span(),
            key: prop_name_to_expr(&n.key),
            params: Default::default(),
            optional: false,
            readonly: false,
            computed: false,
            type_ann,
            type_params: Default::default(),
        }
        .into())
    }
}

fn prop_key_to_expr(p: &Prop) -> Box<Expr> {
    match *p {
        Prop::Shorthand(ref i) => box Expr::Ident(i.clone()),
        Prop::Assign(AssignProp { ref key, .. }) => box Expr::Ident(key.clone()),
        Prop::Getter(GetterProp { ref key, .. })
        | Prop::KeyValue(KeyValueProp { ref key, .. })
        | Prop::Method(MethodProp { ref key, .. })
        | Prop::Setter(SetterProp { ref key, .. }) => prop_name_to_expr(key),
    }
}

pub(super) fn prop_name_to_expr(key: &PropName) -> Box<Expr> {
    match *key {
        PropName::Computed(ref p) => p.expr.clone(),
        PropName::Ident(ref ident) => box Expr::Ident(ident.clone()),
        PropName::Str(ref s) => box Expr::Lit(Lit::Str(Str { ..s.clone() })),
        PropName::Num(ref s) => box Expr::Lit(Lit::Num(Number { ..s.clone() })),
    }
}
