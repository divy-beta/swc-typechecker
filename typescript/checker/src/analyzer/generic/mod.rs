use self::remover::TypeParamRemover;
use super::Analyzer;
use crate::{
    analyzer::scope::Scope,
    builtin_types,
    debug::print_backtrace,
    ty::{
        self, Alias, Array, CallSignature, Conditional, FnParam, IndexedAccessType, Interface,
        Mapped, Operator, PropertySignature, Ref, Tuple, Type, Type::Param, TypeElement, TypeLit,
        TypeOrSpread, TypeParam, TypeParamDecl, TypeParamInstantiation, Union,
    },
    ValidationResult,
};
use bitflags::_core::mem::take;
use fxhash::{FxHashMap, FxHashSet};
use itertools::{EitherOrBoth, Itertools};
use std::collections::hash_map::Entry;
use swc_atoms::{js_word, JsWord, JsWordStaticSet};
use swc_common::{
    Fold, FoldWith, Span, Spanned, Visit, VisitMut, VisitMutWith, VisitWith, DUMMY_SP,
};
use swc_ecma_ast::*;
use swc_ecma_parser::token::Keyword::TypeOf;
use swc_ecma_utils::Id;

mod remover;

/// Type inference for arguments.
impl Analyzer<'_, '_> {
    /// TODO: implement
    pub(super) fn infer_arg_types(
        &mut self,
        span: Span,
        type_params: &[TypeParam],
        params: &[FnParam],
        args: &[TypeOrSpread],
    ) -> ValidationResult<TypeParamInstantiation> {
        log::debug!(
            "infer_arg_types: {:?}",
            type_params.iter().map(|p| &p.name).collect::<Vec<_>>()
        );

        let mut inferred = FxHashMap::default();

        // TODO: Handle optional parameters
        // TODO: Convert this to error.
        assert!(args.len() <= params.len());

        for (p, arg) in params.iter().zip(args) {
            assert_eq!(
                arg.spread, None,
                "argument inference for spread argument in a function / method call is not \
                 implemented yet"
            );

            self.infer_type(&mut inferred, &p.ty, &arg.ty)?;
        }

        let mut params = Vec::with_capacity(type_params.len());
        for type_param in type_params {
            if let Some(ty) = inferred.remove(&type_param.name) {
                params.push(ty);
            } else {
                log::debug!("type param = {:?}", type_param.constraint);

                match type_param.constraint {
                    Some(box Type::Param(ref p)) => {
                        params.push(Type::Param(p.clone()));
                        continue;
                    }
                    _ => {}
                }

                if type_param.constraint.is_some()
                    && is_literals(&type_param.constraint.as_ref().unwrap())
                {
                    params.push(*type_param.constraint.clone().unwrap());
                    continue;
                }

                if type_param.constraint.is_some()
                    && match **type_param.constraint.as_ref().unwrap() {
                        Type::Interface(..)
                        | Type::Keyword(..)
                        | Type::Ref(..)
                        | Type::TypeLit(..) => true,
                        _ => false,
                    }
                {
                    let ty =
                        self.expand_fully(span, *type_param.constraint.clone().unwrap(), false)?;
                    params.push(ty);
                    continue;
                }

                log::warn!(
                    "infer: A type parameter {} defaults to {{}}",
                    type_param.name
                );

                // Defaults to {}
                params.push(Type::TypeLit(TypeLit {
                    span,
                    members: vec![],
                }));
            }
        }

        Ok(TypeParamInstantiation {
            span: DUMMY_SP,
            params,
        })
    }

    fn infer_type(
        &mut self,
        inferred: &mut FxHashMap<JsWord, Type>,
        param: &Type,
        arg: &Type,
    ) -> ValidationResult<()> {
        let param = param.normalize();
        let arg = arg.normalize();

        match param {
            Type::Param(TypeParam {
                ref name,
                ref constraint,
                ..
            }) => {
                log::debug!("infer_type: type parameter: {} = {:?}", name, constraint);

                if constraint.is_some() && is_literals(&constraint.as_ref().unwrap()) {
                    log::debug!("infer: {} = {:?}", name, constraint);
                    inferred.insert(name.clone(), *constraint.clone().unwrap());
                    return Ok(());
                }

                if constraint.is_some()
                    && match **constraint.as_ref().unwrap() {
                        Type::Interface(..)
                        | Type::Keyword(..)
                        | Type::Ref(..)
                        | Type::TypeLit(..) => true,
                        _ => false,
                    }
                {
                    log::debug!("infer: {} = {:?}", name, constraint);
                    inferred.insert(name.clone(), *constraint.clone().unwrap());
                    return Ok(());
                }

                log::info!("infer: {} = {:?}", name, arg);
                match inferred.entry(name.clone()) {
                    Entry::Occupied(e) => {
                        match e.get() {
                            Type::Param(..) => return Ok(()),
                            _ => {}
                        }

                        // Use this for type inference.
                        let param_ty = e.get().clone();

                        // We pass in inverse order to infer type of arg from the type information
                        // of parameter
                        self.infer_type(inferred, &arg, &param_ty)?;
                    }
                    Entry::Vacant(e) => {
                        e.insert(arg.clone());
                    }
                }

                return Ok(());
            }

            Type::Array(Array { elem_type, .. }) => match arg {
                Type::Array(Array {
                    elem_type: arg_elem_type,
                    ..
                }) => self.infer_type(inferred, &elem_type, &arg_elem_type)?,

                _ => {}
            },

            Type::Function(p) => match arg {
                Type::Function(a) => {
                    self.infer_type_of_fn_params(inferred, &p.params, &a.params)?;
                    self.infer_type(inferred, &p.ret_ty, &a.ret_ty)?;
                    if let Some(arg_type_params) = &a.type_params {
                        self.rename_inferred(inferred, arg_type_params)?;
                    }
                }
                _ => {}
            },

            Type::TypeLit(param) => match arg {
                Type::TypeLit(arg) => self.infer_type_lit(inferred, param, arg)?,
                _ => {}
            },

            Type::Tuple(param) => match arg {
                Type::Tuple(arg) => self.infer_tuple(inferred, param, arg)?,
                _ => {}
            },

            Type::Keyword(..) => {}

            Type::Ref(param) => match arg {
                Type::Ref(arg) => {}
                _ => {
                    let param = self.expand(param.span(), Type::Ref(param.clone()))?;
                    return self.infer_type(inferred, &param, arg);
                }
            },

            Type::Lit(..) => match arg {
                Type::Lit(..) => {}
                _ => {}
            },

            // TODO: implement
            Type::Union(..) => match arg {
                Type::Union(..) => {}
                _ => {}
            },

            // Handled by generic expander, so let's return it as-is.
            Type::Mapped(..) => {}

            Type::Alias(param) => self.infer_type(inferred, &param.ty, arg)?,

            _ => {}
        }

        match arg {
            // Handled by generic expander, so let's return it as-is.
            Type::Mapped(..) => {}
            Type::Keyword(..) => {}
            Type::Ref(..) => {
                let arg = self.expand(arg.span(), arg.clone())?;
                match arg {
                    Type::Ref(..) => {}
                    _ => {
                        return self.infer_type(inferred, param, &arg);
                    }
                }
            }
            Type::Alias(arg) => self.infer_type(inferred, param, &arg.ty)?,
            _ => log::error!("infer_arg_type: \narg = {:?}\nparam = {:?}", arg, param),
        }

        Ok(())
    }

    fn infer_type_lit(
        &mut self,
        inferred: &mut FxHashMap<JsWord, Type>,
        param: &TypeLit,
        arg: &TypeLit,
    ) -> ValidationResult<()> {
        // TODO: implement
        Ok(())
    }

    fn infer_tuple(
        &mut self,
        inferred: &mut FxHashMap<JsWord, Type>,
        param: &Tuple,
        arg: &Tuple,
    ) -> ValidationResult<()> {
        for item in param.types.iter().zip_longest(&arg.types) {
            match item {
                EitherOrBoth::Both(param, arg) => self.infer_type(inferred, param, arg)?,
                EitherOrBoth::Left(_) => {}
                EitherOrBoth::Right(_) => {}
            }
        }

        Ok(())
    }

    fn infer_type_of_fn_param(
        &mut self,
        inferred: &mut FxHashMap<JsWord, Type>,
        param: &FnParam,
        arg: &FnParam,
    ) -> ValidationResult<()> {
        self.infer_type(inferred, &param.ty, &arg.ty)
    }

    fn infer_type_of_fn_params(
        &mut self,
        inferred: &mut FxHashMap<JsWord, Type>,
        params: &[FnParam],
        args: &[FnParam],
    ) -> ValidationResult<()> {
        for (param, arg) in params.iter().zip(args) {
            self.infer_type_of_fn_param(inferred, param, arg)?
        }

        Ok(())
    }

    fn rename_inferred(
        &mut self,
        inferred: &mut FxHashMap<JsWord, Type>,
        arg_type_params: &TypeParamDecl,
    ) -> ValidationResult<()> {
        struct Renamer<'a> {
            fixed: &'a FxHashMap<JsWord, Type>,
        }

        impl VisitMut<Type> for Renamer<'_> {
            fn visit_mut(&mut self, node: &mut Type) {
                match node {
                    Type::Param(p) if self.fixed.contains_key(&p.name) => {
                        *node = (*self.fixed.get(&p.name).unwrap()).clone();
                    }
                    _ => node.visit_mut_children(self),
                }
            }
        }

        //
        let mut fixed = FxHashMap::default();

        inferred.iter().for_each(|(param_name, ty)| {
            // Ignore unrelated type parameters
            if arg_type_params.params.iter().all(|v| *param_name != v.name) {
                return;
            }
            fixed.insert(param_name.clone(), ty.clone());
        });

        let mut v = Renamer { fixed: &fixed };
        inferred.iter_mut().for_each(|(_, ty)| {
            ty.visit_mut_with((&mut v));
        });

        Ok(())
    }
}

/// Handles renaming of the type parameters.
impl Analyzer<'_, '_> {
    pub(super) fn rename_type_params(
        &mut self,
        span: Span,
        mut ty: Type,
        type_ann: Option<&Type>,
    ) -> ValidationResult {
        if self.is_builtin {
            return Ok(ty);
        }
        log::trace!("rename_type_params");

        // ty = self.expand(span, ty)?;

        let mut inferred = FxHashMap::default();

        let mut usage_visitor = TypeParamUsageFinder::default();
        ty.normalize().visit_with(&mut usage_visitor);
        if usage_visitor.params.is_empty() {
            log::debug!("rename_type_param: No type parameter is used in type");
            match ty {
                Type::Function(ref mut f) => {
                    f.type_params = None;
                }

                _ => {}
            }

            return Ok(ty);
        }

        if let Some(type_ann) = type_ann {
            self.infer_type(&mut inferred, &ty, type_ann)?;
            return Ok(ty
                .into_owned()
                .fold_with(&mut TypeParamRenamer { inferred }));
        }

        let decl = Some(TypeParamDecl {
            span: DUMMY_SP,
            params: usage_visitor.params,
        });

        match ty {
            Type::Function(ref mut f) => {
                f.type_params = decl;
            }

            _ => {}
        }

        Ok(ty.fold_with(&mut TypeParamRemover::new()))
    }
}

#[derive(Debug)]
struct TypeParamRenamer {
    inferred: FxHashMap<JsWord, Type>,
}

impl Fold<Type> for TypeParamRenamer {
    fn fold(&mut self, mut ty: Type) -> Type {
        ty = ty.fold_children(self);

        match ty {
            Type::Param(ref param) => {
                if let Some(ty) = self.inferred.get(&param.name) {
                    return ty.clone();
                }
            }
            _ => {}
        }

        ty
    }
}

#[derive(Debug, Default)]
struct TypeParamUsageFinder {
    params: Vec<TypeParam>,
}

impl Visit<TypeParamDecl> for TypeParamUsageFinder {
    #[inline]
    fn visit(&mut self, _: &TypeParamDecl) {}
}

impl Visit<TypeParam> for TypeParamUsageFinder {
    fn visit(&mut self, node: &TypeParam) {
        for p in &self.params {
            if node.name == p.name {
                return;
            }
        }

        log::info!("Found type parameter({})", node.name);

        self.params.push(node.clone());
    }
}

/// Generic expander.
impl Analyzer<'_, '_> {
    pub(super) fn expand_type_params(
        &mut self,
        i: &TypeParamInstantiation,
        params: &[TypeParam],
        ty: Type,
    ) -> ValidationResult {
        self.expand_type_params_inner(i, params, ty, false)
    }

    ///
    ///
    ///  This methods handle special types like mapped type.
    ///
    ///  e.g.
    ///      type BadNested<T> = {
    ///          x: T extends number ? T : string;
    ///      };
    ///      T extends {
    ///          [K in keyof BadNested<infer P>]: BadNested<infer P>[K];
    ///      } ? P : never;
    ///
    ///
    ///z     T extends {
    ///          x: infer P extends number ? infer P : string;
    ///      } ? P : never
    fn expand_type_params_inner(
        &mut self,
        type_args: &TypeParamInstantiation,
        params: &[TypeParam],
        ty: Type,
        fully: bool,
    ) -> ValidationResult {
        let mut ty = ty.fold_with(&mut GenericExpander {
            params,
            i: type_args,
            fully,
            dejavu: Default::default(),
        });

        Ok(ty)
    }

    /// Returns `Some(true)` if `child` extends `parent`.
    fn extends(&self, child: &Type, parent: &Type) -> Option<bool> {
        match child {
            Type::Ref(..) => return None,
            _ => {}
        }
        match parent {
            Type::Ref(..) => return None,
            _ => {}
        }

        let span = child.span();

        match self.assign(parent, child, span) {
            Ok(()) => Some(true),
            _ => None,
        }
    }
}

/// This struct does not expands ref to other thpe. See Analyzer.expand to do
/// such operation.
struct GenericExpander<'a> {
    params: &'a [TypeParam],
    i: &'a TypeParamInstantiation,
    /// Expand fully?
    fully: bool,
    dejavu: FxHashSet<JsWord>,
}

impl Fold<Type> for GenericExpander<'_> {
    fn fold(&mut self, mut ty: Type) -> Type {
        let old_fully = self.fully;
        self.fully |= match ty {
            Type::Mapped(..) => true,
            _ => false,
        };

        match ty {
            Type::Ref(Ref {
                span,
                type_name: TsEntityName::Ident(Ident { ref sym, .. }),
                ref type_args,
                ..
            }) => {
                if *sym == js_word!("Array") {
                    return Type::Array(Array {
                        span,
                        elem_type: box type_args
                            .as_ref()
                            .and_then(|args| args.params.iter().next().cloned())
                            .unwrap_or_else(|| Type::any(span)),
                    });
                }

                if self.dejavu.contains(sym) {
                    log::debug!("Dejavu: {}", sym);
                    return ty;
                }

                log::info!("Ref: {}", sym);

                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == *sym {
                        assert_eq!(*type_args, None);

                        return self.i.params[idx].clone();
                    }
                }

                return ty.fold_children(self);
            }

            Type::Ref(..) => return ty.fold_children(self),

            Type::Param(mut param) => {
                param = param.fold_with(self);

                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == param.name {
                        match self.i.params[idx].clone().normalize() {
                            Type::Param(..) => {}
                            _ => return self.i.params[idx].clone(),
                        }
                    }
                }

                for (idx, p) in self.params.iter().enumerate() {
                    if p.name == param.name {
                        match self.i.params[idx].clone().normalize() {
                            Type::Param(..) => return self.i.params[idx].clone(),
                            _ => {}
                        }
                    }
                }

                return Type::Param(param);
            }

            // Alias returns other than self.
            Type::Alias(mut alias) => {
                alias = alias.fold_with(self);
                //
                if let Some(..) = &alias.type_params {
                    // TODO: Handle unresolved type parameter
                    log::warn!("An type alias has type parameters. It may not be fully expanded.");
                }
                return *alias.ty;
            }

            Type::Interface(mut i) if self.fully => {
                i = i.fold_with(self);

                if let Some(..) = &i.type_params {
                    log::error!("An interface has type parameters. It may not be fully expanded.");
                }

                // TODO: Handle super
                if !i.extends.is_empty() {
                    log::error!("not yet implemented: expanding interface which has a parent");
                    return Type::Interface(i);
                }

                return Type::TypeLit(TypeLit {
                    span: i.span,
                    members: i.body,
                });
            }
            Type::Class(mut c) => {
                c = c.fold_with(self);

                if let Some(..) = &c.type_params {
                    log::error!("A class has type parameters. It may not be fully expanded.");
                }

                return Type::Class(c);
            }

            Type::Conditional(mut c) => {
                c = c.fold_with(self);

                // if let Some(v) = self.analyzer.extends(&c.check_type, &c.extends_type) {
                //     return if v { *c.true_type } else { *c.false_type };
                // }

                return Type::Conditional(c);
            }

            Type::Mapped(mut m @ Mapped { ty: Some(..), .. }) => {
                m = m.fold_with(self);

                match m.type_param.constraint {
                    Some(box Type::Operator(Operator {
                        op: TsTypeOperatorOp::KeyOf,
                        ty: box Type::Union(ref u),
                        ..
                    })) => {
                        log::error!("Union!");
                    }
                    _ => {}
                }

                m.ty = match m.ty {
                    Some(box Type::IndexedAccessType(IndexedAccessType {
                        span,
                        readonly,
                        obj_type,
                        index_type,
                    })) => {
                        match *obj_type {
                            Type::TypeLit(TypeLit { span, members, .. })
                                if members.iter().all(|m| match m {
                                    TypeElement::Property(_) => true,
                                    _ => false,
                                }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for m in members {
                                    match m {
                                        ty::TypeElement::Property(p) => {
                                            //
                                            new_members.push(ty::TypeElement::Property(p));
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                return Type::TypeLit(TypeLit {
                                    span,
                                    members: new_members,
                                });
                            }

                            _ => Some(box Type::IndexedAccessType(IndexedAccessType {
                                span,
                                readonly,
                                obj_type,
                                index_type,
                            })),
                        }
                    }
                    _ => m.ty,
                };

                if let Some(constraint) = &m.type_param.constraint {
                    match &**constraint {
                        Type::Operator(Operator {
                            span,
                            op: TsTypeOperatorOp::KeyOf,
                            ty,
                        }) => match &**ty {
                            Type::Keyword(..) => return *ty.clone(),
                            Type::TypeLit(TypeLit { span, members, .. })
                                if members.iter().all(|m| match m {
                                    TypeElement::Property(_) => true,
                                    TypeElement::Method(_) => true,
                                    _ => false,
                                }) =>
                            {
                                let mut new_members = Vec::with_capacity(members.len());
                                for member in members {
                                    match member {
                                        ty::TypeElement::Method(method) => {
                                            new_members.push(ty::TypeElement::Property(
                                                PropertySignature {
                                                    span: method.span,
                                                    readonly: method.readonly,
                                                    key: method.key.clone(),
                                                    computed: method.computed,
                                                    optional: method.optional,
                                                    params: vec![],
                                                    type_ann: m.ty.clone().map(|v| *v),
                                                    type_params: None,
                                                },
                                            ));
                                        }
                                        ty::TypeElement::Property(p) => {
                                            let mut p = p.clone();
                                            if let Some(ty) = &m.ty {
                                                p.type_ann = Some(*ty.clone());
                                            }
                                            //
                                            new_members.push(ty::TypeElement::Property(p));
                                        }
                                        _ => unreachable!(),
                                    }
                                }

                                return Type::TypeLit(TypeLit {
                                    span: *span,
                                    members: new_members,
                                });
                            }
                            _ => {}
                        },

                        _ => {}
                    }
                }

                return Type::Mapped(m);
            }

            Type::This(..) | Type::Keyword(..) | Type::TypeLit(..) | Type::Lit(..) => {
                return ty.fold_children(self)
            }

            Type::Query(..)
            | Type::Operator(..)
            | Type::Tuple(..)
            | Type::Infer(..)
            | Type::Import(..)
            | Type::Predicate(..)
            | Type::Array(..)
            | Type::Union(..)
            | Type::Intersection(..)
            | Type::IndexedAccessType(..)
            | Type::Function(..)
            | Type::Constructor(..)
            | Type::Method(..)
            | Type::Enum(..)
            | Type::EnumVariant(..)
            | Type::Interface(..)
            | Type::Namespace(..)
            | Type::Module(..)
            | Type::ClassInstance(..)
            | Type::Mapped(..) => return ty.fold_children(self),

            Type::Static(s) => return s.ty.clone().fold_with(self),
            Type::Arc(a) => return (*a).clone().fold_with(self),
        }

        ty
    }
}

/// This method returns true for types like `'foo'` and `'foo' | 'bar'`.
pub(super) fn is_literals(ty: &Type) -> bool {
    match ty.normalize() {
        Type::Lit(_) => true,
        Type::Union(Union { ref types, .. }) => types.iter().all(|v| is_literals(v)),
        _ => false,
    }
}
