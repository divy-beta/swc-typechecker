use super::Type;
use crate::{
    ty,
    ty::{FnParam, ImportType, QueryExpr, TypeElement, TypeParamDecl, TypeParamInstantiation},
    util::PatExt,
};
use swc_common::{util::map::Map, Spanned, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_utils::prop_name_to_expr;

impl From<Type> for TsType {
    fn from(t: Type) -> Self {
        match t {
            Type::This(t) => t.into(),
            Type::Lit(t) => t.into(),
            Type::Query(t) => t.into(),
            Type::Infer(t) => t.into(),
            Type::Import(t) => t.into(),
            Type::Predicate(t) => t.into(),
            Type::IndexedAccessType(t) => t.into(),
            Type::Ref(t) => t.into(),
            Type::TypeLit(t) => t.into(),
            Type::Keyword(t) => t.into(),
            Type::Conditional(t) => t.into(),
            Type::Tuple(t) => t.into(),
            Type::Array(t) => t.into(),
            Type::Union(t) => t.into(),
            Type::Intersection(t) => t.into(),
            Type::Function(t) => t.into(),
            Type::Constructor(t) => t.into(),
            Type::Method(t) => t.into(),
            Type::Operator(t) => t.into(),
            Type::Param(t) => t.into(),
            Type::EnumVariant(t) => t.into(),
            Type::Interface(t) => t.into(),
            Type::Enum(t) => t.into(),
            Type::Mapped(t) => t.into(),
            Type::Alias(t) => t.into(),
            Type::Namespace(t) => {
                unimplemented!("TsNamespaceDecl should be handled before converting to TsType")
            }
            Type::Module(t) => t.into(),
            Type::Class(t) => t.into(),
            Type::ClassInstance(t) => t.into(),
            Type::Static(t) => (*t.ty).clone().into(),
            Type::Arc(t) => (*t).clone().into(),
        }
    }
}

impl From<ty::QueryType> for TsType {
    fn from(t: ty::QueryType) -> Self {
        TsType::TsTypeQuery(TsTypeQuery {
            span: t.span,
            expr_name: t.expr.into(),
        })
    }
}

impl From<ty::QueryExpr> for TsTypeQueryExpr {
    fn from(t: ty::QueryExpr) -> Self {
        match t {
            QueryExpr::TsEntityName(t) => TsTypeQueryExpr::TsEntityName(t),
            QueryExpr::Import(t) => TsTypeQueryExpr::Import(t.into()),
        }
    }
}

impl From<ty::ImportType> for TsImportType {
    fn from(t: ImportType) -> Self {
        TsImportType {
            span: t.span,
            arg: t.arg,
            qualifier: t.qualifier,
            type_args: t.type_params.map(From::from),
        }
    }
}

impl From<ty::InferType> for TsType {
    fn from(t: ty::InferType) -> Self {
        TsType::TsInferType(TsInferType {
            span: t.span,
            type_param: t.type_param.into(),
        })
    }
}

impl From<ty::ImportType> for TsType {
    fn from(t: ty::ImportType) -> Self {
        TsType::TsImportType(TsImportType {
            span: t.span,
            arg: t.arg,
            qualifier: t.qualifier,
            type_args: t.type_params.map(From::from),
        })
    }
}

impl From<ty::Predicate> for TsType {
    fn from(t: ty::Predicate) -> Self {
        TsType::TsTypePredicate(TsTypePredicate {
            span: t.span,
            asserts: t.asserts,
            param_name: t.param_name,
            type_ann: t.ty.map(From::from),
        })
    }
}

impl From<ty::IndexedAccessType> for TsType {
    fn from(t: ty::IndexedAccessType) -> Self {
        TsType::TsIndexedAccessType(TsIndexedAccessType {
            span: t.span,
            readonly: t.readonly,
            obj_type: t.obj_type.into(),
            index_type: t.index_type.into(),
        })
    }
}

impl From<ty::Ref> for TsType {
    fn from(t: ty::Ref) -> Self {
        TsType::TsTypeRef(TsTypeRef {
            span: t.span,
            type_name: t.type_name,
            type_params: t.type_args.map(From::from),
        })
    }
}

impl From<ty::TypeLit> for TsType {
    fn from(t: ty::TypeLit) -> Self {
        TsType::TsTypeLit(TsTypeLit {
            span: t.span,
            members: t.members.into_iter().map(From::from).collect(),
        })
    }
}

impl From<ty::Conditional> for TsType {
    fn from(t: ty::Conditional) -> Self {
        TsType::TsConditionalType(TsConditionalType {
            span: t.span,
            check_type: box (*t.check_type).into(),
            extends_type: box (*t.extends_type).into(),
            true_type: box (*t.true_type).into(),
            false_type: box (*t.false_type).into(),
        })
    }
}

impl From<ty::Tuple> for TsType {
    fn from(t: ty::Tuple) -> Self {
        TsType::TsTupleType(TsTupleType {
            span: t.span,
            elem_types: t.types.into_iter().map(|v| box v.into()).collect(),
        })
    }
}

impl From<ty::Array> for TsType {
    fn from(t: ty::Array) -> Self {
        TsType::TsArrayType(TsArrayType {
            span: t.span,
            elem_type: box (*t.elem_type).into(),
        })
    }
}

impl From<ty::Union> for TsType {
    fn from(t: ty::Union) -> Self {
        TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsUnionType(TsUnionType {
            span: t.span,
            types: t.types.into_iter().map(From::from).collect(),
        }))
    }
}

impl From<ty::Intersection> for TsType {
    fn from(t: ty::Intersection) -> Self {
        TsType::TsUnionOrIntersectionType(TsUnionOrIntersectionType::TsIntersectionType(
            TsIntersectionType {
                span: t.span,
                types: t.types.into_iter().map(From::from).collect(),
            },
        ))
    }
}

impl From<ty::Function> for TsType {
    fn from(t: ty::Function) -> Self {
        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
            type_params: t.type_params.map(From::from),
            type_ann: t.ret_ty.into(),
        }))
    }
}

impl From<ty::Constructor> for TsType {
    fn from(t: ty::Constructor) -> Self {
        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsConstructorType(
            TsConstructorType {
                span: t.span,
                params: t.params.into_iter().map(From::from).collect(),
                type_params: t.type_params.map(From::from),
                type_ann: t.type_ann.into(),
            },
        ))
    }
}

impl From<ty::Method> for TsType {
    fn from(t: ty::Method) -> Self {
        TsType::TsFnOrConstructorType(TsFnOrConstructorType::TsFnType(TsFnType {
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
            type_params: t.type_params.map(From::from),
            type_ann: t.ret_ty.into(),
        }))
    }
}

impl From<ty::TypeParamDecl> for TsTypeParamDecl {
    fn from(t: TypeParamDecl) -> Self {
        TsTypeParamDecl {
            span: t.span,
            params: t.params.into_iter().map(From::from).collect(),
        }
    }
}

impl From<ty::Type> for TsTypeAnn {
    fn from(t: ty::Type) -> Self {
        TsTypeAnn {
            span: t.span(),
            type_ann: box t.into(),
        }
    }
}

impl From<Box<ty::Type>> for TsTypeAnn {
    fn from(t: Box<ty::Type>) -> Self {
        (*t).into()
    }
}

impl From<Box<ty::Type>> for Box<TsType> {
    fn from(t: Box<ty::Type>) -> Self {
        box (*t).into()
    }
}

impl From<ty::TypeParam> for TsTypeParam {
    fn from(t: ty::TypeParam) -> Self {
        TsTypeParam {
            span: t.span,
            // TODO
            name: t.name.into(),
            constraint: t.constraint.map(From::from),
            default: t.default.map(From::from),
        }
    }
}

impl From<ty::Operator> for TsType {
    fn from(t: ty::Operator) -> Self {
        TsTypeOperator {
            span: t.span,
            op: t.op,
            type_ann: t.ty.into(),
        }
        .into()
    }
}

impl From<ty::TypeParam> for TsType {
    fn from(t: ty::TypeParam) -> Self {
        TsType::TsTypeRef(TsTypeRef {
            span: t.span,
            // TODO
            type_name: t.name.into(),
            type_params: None,
        })
    }
}

impl From<ty::EnumVariant> for TsType {
    fn from(t: ty::EnumVariant) -> Self {
        TsType::TsTypeRef(TsTypeRef {
            span: t.span,
            type_name: TsEntityName::TsQualifiedName(box TsQualifiedName {
                left: t.enum_name.into(),
                right: Ident::new(t.name, DUMMY_SP),
            }),
            type_params: None,
        })
    }
}

impl From<ty::Enum> for TsType {
    fn from(t: ty::Enum) -> Self {
        TsType::TsTypeRef(TsTypeRef {
            span: t.span,
            // TODO
            type_name: t.id.into(),
            type_params: None,
        })
    }
}

impl From<ty::Interface> for TsType {
    fn from(t: ty::Interface) -> Self {
        TsTypeRef {
            span: t.span,
            // TODO
            type_name: TsEntityName::Ident(t.name.into()),
            type_params: None,
        }
        .into()
    }
}

impl From<ty::Mapped> for TsType {
    fn from(t: ty::Mapped) -> Self {
        TsMappedType {
            span: t.span,

            readonly: t.readonly,
            type_param: t.type_param.into(),
            optional: t.optional.map(From::from),
            type_ann: t.ty.map(From::from),
        }
        .into()
    }
}

impl From<ty::Alias> for TsType {
    fn from(t: ty::Alias) -> Self {
        (*t.ty).into()
    }
}

impl From<ty::Module> for TsType {
    fn from(_: ty::Module) -> Self {
        unreachable!("ty::Module should be handled before converting to TsType")
    }
}

impl From<TypeParamInstantiation> for TsTypeParamInstantiation {
    fn from(t: TypeParamInstantiation) -> Self {
        TsTypeParamInstantiation {
            span: t.span,
            params: t.params.into_iter().map(|v| box v.into()).collect(),
        }
    }
}

impl From<ty::Operator> for TsTypeOperator {
    fn from(t: ty::Operator) -> Self {
        TsTypeOperator {
            span: t.span,
            op: t.op,
            type_ann: t.ty.into(),
        }
    }
}

impl From<ty::Class> for TsType {
    fn from(t: ty::Class) -> Self {
        // TODO: Handle generics
        TsTypeLit {
            span: t.span,
            members: t.body.into_iter().map(From::from).collect(),
        }
        .into()
    }
}

impl From<ty::ClassInstance> for TsType {
    fn from(t: ty::ClassInstance) -> Self {
        unimplemented!("From<ty::ClassInstance> for TsType")
    }
}

impl From<ty::ClassMember> for TsTypeElement {
    fn from(m: ty::ClassMember) -> Self {
        match m {
            ty::ClassMember::Constructor(c) => {
                TsTypeElement::TsConstructSignatureDecl(TsConstructSignatureDecl {
                    span: c.span,
                    params: c.params.into_iter().map(From::from).collect(),
                    type_ann: c.ret_ty.map(From::from),
                    type_params: c.type_params.map(From::from),
                })
            }
            ty::ClassMember::Method(m) => TsTypeElement::TsMethodSignature(TsMethodSignature {
                span: m.span,
                readonly: false,
                computed: match m.key {
                    PropName::Computed(_) => true,
                    _ => false,
                },
                key: box prop_name_to_expr(m.key),
                optional: m.is_optional,
                params: m.params.into_iter().map(From::from).collect(),
                type_ann: Some(TsTypeAnn {
                    span: DUMMY_SP,
                    type_ann: box (*m.ret_ty).into(),
                }),
                type_params: m.type_params.map(From::from),
            }),
            ty::ClassMember::Property(p) => {
                TsTypeElement::TsPropertySignature(TsPropertySignature {
                    span: p.span,
                    readonly: p.readonly,
                    key: p.key,
                    computed: p.computed,
                    optional: p.is_optional,
                    init: None,
                    params: vec![],
                    type_ann: p.value.map(|ty| TsTypeAnn {
                        span: DUMMY_SP,
                        type_ann: box ty.into(),
                    }),
                    type_params: None,
                })
            }
            ty::ClassMember::IndexSignature(s) => {
                TsTypeElement::TsIndexSignature(TsIndexSignature {
                    span: s.span,
                    params: s.params.into_iter().map(From::from).collect(),
                    type_ann: s.type_ann.map(|ty| TsTypeAnn {
                        span: DUMMY_SP,
                        type_ann: box ty.into(),
                    }),
                    readonly: s.readonly,
                })
            }
        }
    }
}

impl From<ty::TypeElement> for TsTypeElement {
    fn from(e: ty::TypeElement) -> Self {
        match e {
            TypeElement::Call(e) => TsTypeElement::TsCallSignatureDecl(TsCallSignatureDecl {
                span: e.span,
                params: e.params.into_iter().map(|v| v.into()).collect(),
                type_ann: e.ret_ty.map(From::from),
                type_params: e.type_params.map(From::from),
            }),
            TypeElement::Constructor(e) => {
                TsTypeElement::TsConstructSignatureDecl(TsConstructSignatureDecl {
                    span: e.span,
                    params: e.params.into_iter().map(|v| v.into()).collect(),
                    type_ann: e.ret_ty.map(From::from),
                    type_params: e.type_params.map(From::from),
                })
            }
            TypeElement::Property(e) => TsTypeElement::TsPropertySignature(TsPropertySignature {
                span: e.span,
                readonly: e.readonly,
                key: e.key,
                computed: e.computed,
                optional: e.optional,
                init: None,
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.type_ann.map(From::from),
                type_params: e.type_params.map(From::from),
            }),
            TypeElement::Method(e) => TsTypeElement::TsMethodSignature(TsMethodSignature {
                span: e.span,
                readonly: e.readonly,
                key: e.key,
                computed: e.computed,
                optional: e.optional,
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.ret_ty.map(From::from),
                type_params: e.type_params.map(From::from),
            }),
            TypeElement::Index(e) => TsTypeElement::TsIndexSignature(TsIndexSignature {
                params: e.params.into_iter().map(From::from).collect(),
                type_ann: e.type_ann.map(From::from),
                readonly: e.readonly,
                span: e.span,
            }),
        }
    }
}

impl From<ty::FnParam> for TsFnParam {
    fn from(t: FnParam) -> Self {
        let ty = t.ty;
        let type_ann = Some(TsTypeAnn {
            span: DUMMY_SP,
            type_ann: ty.into(),
        });

        match t.pat {
            Pat::Ident(i) => TsFnParam::Ident(Ident {
                span: t.span,
                sym: i.sym,
                type_ann,
                optional: !t.required,
            }),
            Pat::Array(a) => TsFnParam::Array(ArrayPat {
                span: t.span,
                type_ann,
                elems: a.elems,
                optional: !t.required,
            }),
            Pat::Rest(r) => TsFnParam::Rest(RestPat {
                span: t.span,
                dot3_token: r.dot3_token,
                arg: r.arg,
                type_ann,
            }),
            Pat::Object(o) => TsFnParam::Object(ObjectPat {
                span: t.span,
                type_ann,
                props: o.props,
                optional: o.optional,
            }),
            _ => unimplemented!("From<ty::FnParam> for TsFnParam"),
        }
    }
}

impl From<ty::Type> for Box<TsType> {
    fn from(t: Type) -> Self {
        box t.into()
    }
}
