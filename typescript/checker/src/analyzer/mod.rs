pub use self::name::Name;
use self::{
    scope::{Scope, ScopeKind},
    util::PatExt,
};
use super::Checker;
use crate::{
    builtin_types::Lib,
    errors::Error,
    loader::Load,
    ty::{self, Type, TypeRefExt},
    Rule,
};
use fxhash::{FxHashMap, FxHashSet};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{borrow::Cow, cell::RefCell, path::PathBuf, sync::Arc};
use swc_atoms::{js_word, JsWord};
use swc_common::{Span, Spanned, Visit, VisitWith};
use swc_ecma_ast::*;

mod control_flow;
pub mod export;
mod expr;
mod name;
mod scope;
mod type_facts;
mod util;

struct Analyzer<'a, 'b> {
    info: Info,
    resolved_imports: FxHashMap<JsWord, Arc<Type<'static>>>,
    errored_imports: FxHashSet<JsWord>,
    pending_exports: Vec<((JsWord, Span), Box<Expr>)>,
    inferred_return_types: RefCell<Vec<Type<'static>>>,
    scope: Scope<'a>,
    path: Arc<PathBuf>,
    loader: &'b dyn Load,
    libs: &'b [Lib],
    rule: Rule,
}

impl<T> Visit<Vec<T>> for Analyzer<'_, '_>
where
    T: VisitWith<Self> + for<'any> VisitWith<ImportFinder<'any>> + Send + Sync,
    Vec<T>: VisitWith<Self>,
{
    fn visit(&mut self, items: &Vec<T>) {
        // We first load imports.

        let mut imports: Vec<ImportInfo> = vec![];

        items.iter().for_each(|item| {
            // EXtract imports
            item.visit_with(&mut ImportFinder { to: &mut imports });
            // item.visit_with(self);
        });

        let loader = self.loader;
        let path = self.path.clone();
        let import_results = imports
            .par_iter()
            .map(|import| {
                loader.load(path.clone(), &*import).map_err(|err| {
                    //
                    (import, err)
                })
            })
            .collect::<Vec<_>>();

        for res in import_results {
            match res {
                Ok(import) => {
                    self.resolved_imports.extend(import);
                }
                Err((import, mut err)) => {
                    match err {
                        Error::ModuleLoadFailed { ref mut errors, .. } => {
                            self.info.errors.append(errors);
                        }
                        _ => {}
                    }
                    // Mark errored imported types as any to prevent useless errors
                    self.errored_imports.extend(
                        import
                            .items
                            .iter()
                            .map(|&Specifier { ref local, .. }| local.0.clone()),
                    );

                    self.info.errors.push(err);
                }
            }
        }

        items.visit_children(self);

        self.handle_pending_exports();
    }
}

impl Visit<TsModuleDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, decl: &TsModuleDecl) {
        // TODO(kdy1): Uncomment the line below.
        // Uncommenting the line somehow returns without excuting subsequent codes.
        // decl.visit_children(self);

        // println!("after: visit<TsModuleDecl>: {:?}", decl.id);

        self.scope.register_type(
            match decl.id {
                TsModuleName::Ident(ref i) => i.sym.clone(),
                TsModuleName::Str(ref s) => s.value.clone(),
            },
            decl.clone().into(),
        );
    }
}

impl Visit<TsInterfaceDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, decl: &TsInterfaceDecl) {
        self.scope
            .register_type(decl.id.sym.clone(), decl.clone().into());
    }
}

impl Visit<TsTypeAliasDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, decl: &TsTypeAliasDecl) {
        self.scope
            .register_type(decl.id.sym.clone(), decl.clone().into());

        // TODO(kdy1): Validate type
    }
}

#[derive(Debug)]
struct ImportFinder<'a> {
    to: &'a mut Vec<ImportInfo>,
}

/// Extracts require('foo')
impl Visit<CallExpr> for ImportFinder<'_> {
    fn visit(&mut self, expr: &CallExpr) {
        let span = expr.span();

        match expr.callee {
            ExprOrSuper::Expr(box Expr::Ident(ref i)) if i.sym == js_word!("require") => {
                let src = expr
                    .args
                    .iter()
                    .map(|v| match *v.expr {
                        Expr::Lit(Lit::Str(Str { ref value, .. })) => value.clone(),
                        _ => unimplemented!("error reporting for dynamic require"),
                    })
                    .next()
                    .unwrap();
                self.to.push(ImportInfo {
                    span,
                    all: true,
                    items: vec![],
                    src,
                });
            }
            _ => return,
        }
    }
}

impl Visit<ImportDecl> for ImportFinder<'_> {
    fn visit(&mut self, import: &ImportDecl) {
        let span = import.span();
        let mut items = vec![];
        let mut all = false;

        for s in &import.specifiers {
            match *s {
                ImportSpecifier::Default(ref default) => items.push(Specifier {
                    export: (js_word!("default"), default.span),
                    local: (default.local.sym.clone(), default.local.span),
                }),
                ImportSpecifier::Specific(ref s) => {
                    items.push(Specifier {
                        export: (
                            s.imported
                                .clone()
                                .map(|v| v.sym)
                                .unwrap_or_else(|| s.local.sym.clone()),
                            s.span,
                        ),
                        local: (s.local.sym.clone(), s.local.span),
                    });
                }
                ImportSpecifier::Namespace(..) => all = true,
            }
        }

        if !items.is_empty() {
            self.to.push(ImportInfo {
                span,
                items,
                all,
                src: import.src.value.clone(),
            });
        }
    }
}

impl<'a, 'b> Analyzer<'a, 'b> {
    pub fn new(
        libs: &'b [Lib],
        rule: Rule,
        scope: Scope<'a>,
        path: Arc<PathBuf>,
        loader: &'b dyn Load,
    ) -> Self {
        Analyzer {
            libs,
            rule,
            scope,
            info: Default::default(),
            inferred_return_types: Default::default(),
            path,
            resolved_imports: Default::default(),
            errored_imports: Default::default(),
            pending_exports: Default::default(),
            loader,
        }
    }
}

#[derive(Debug, Default)]
pub struct Info {
    pub exports: FxHashMap<JsWord, Arc<Type<'static>>>,
    pub errors: Vec<Error>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportInfo {
    pub span: Span,
    pub items: Vec<Specifier>,
    pub all: bool,
    pub src: JsWord,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Specifier {
    pub local: (JsWord, Span),
    pub export: (JsWord, Span),
}

impl Visit<TsEnumDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, e: &TsEnumDecl) {
        e.visit_children(self);

        self.scope.register_type(e.id.sym.clone(), e.clone().into());
    }
}

impl Visit<ClassDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, c: &ClassDecl) {
        c.visit_children(self);

        let ty = match self.type_of_class(&c.class) {
            Ok(ty) => ty,
            Err(err) => {
                self.info.errors.push(err);
                Type::any(c.span()).into()
            }
        };
        self.scope
            .register_type(c.ident.sym.clone(), c.class.clone().into());
        self.scope.declare_var(
            VarDeclKind::Var,
            c.ident.sym.clone(),
            Some(ty),
            // initialized = true
            true,
            // declare Class does not allow multiple declarations.
            false,
        );
    }
}

impl Analyzer<'_, '_> {
    /// TODO: Handle recursive funciton
    fn visit_fn(&mut self, f: &Function) -> Type<'static> {
        let fn_ty = self.with_child(ScopeKind::Fn, Default::default(), |child| {
            f.params
                .iter()
                .for_each(|pat| child.scope.declare_vars(VarDeclKind::Let, pat));

            match f.body {
                Some(ref body) => body.visit_children(child),
                None => {}
            }

            f.visit_children(child);

            dbg!(&child.inferred_return_types);

            let fn_ty = child.type_of_fn(f)?;

            Ok(fn_ty)
        });

        match fn_ty {
            Ok(ty) => ty.to_static(),
            Err(err) => {
                self.info.errors.push(err);
                Type::any(f.span)
            }
        }
    }
}

impl Visit<FnDecl> for Analyzer<'_, '_> {
    /// NOTE: This method **should not call f.visit_children(self)**
    fn visit(&mut self, f: &FnDecl) {
        let fn_ty = self.visit_fn(&f.function);

        self.scope.declare_var(
            VarDeclKind::Var,
            f.ident.sym.clone(),
            Some(fn_ty),
            // initialized
            true,
            // allow_multiple
            f.declare,
        );
    }
}

impl Visit<Function> for Analyzer<'_, '_> {
    fn visit(&mut self, f: &Function) {
        self.visit_fn(f);
    }
}

impl Visit<ArrowExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, f: &ArrowExpr) {
        self.with_child(ScopeKind::Fn, Default::default(), |analyzer| {
            f.params
                .iter()
                .for_each(|pat| analyzer.scope.declare_vars(VarDeclKind::Let, pat));

            match f.body {
                BlockStmtOrExpr::Expr(ref expr) => expr.visit_with(analyzer),
                BlockStmtOrExpr::BlockStmt(ref stmt) => stmt.visit_children(analyzer),
            }
        })
    }
}

impl Visit<BlockStmt> for Analyzer<'_, '_> {
    fn visit(&mut self, stmt: &BlockStmt) {
        self.with_child(ScopeKind::Block, Default::default(), |analyzer| {
            stmt.visit_children(analyzer);
        })
    }
}

impl Visit<AssignExpr> for Analyzer<'_, '_> {
    fn visit(&mut self, expr: &AssignExpr) {
        let span = expr.span();

        let rhs_ty = match self
            .type_of(&expr.right)
            .and_then(|ty| self.fix_type(span, ty))
        {
            Ok(rhs_ty) => rhs_ty.to_static(),
            Err(err) => {
                self.info.errors.push(err);
                return;
            }
        };
        if expr.op == op!("=") {
            self.try_assign(&expr.left, &rhs_ty);
        }
    }
}

impl Visit<VarDecl> for Analyzer<'_, '_> {
    fn visit(&mut self, var: &VarDecl) {
        let kind = var.kind;

        var.decls.iter().for_each(|v| {
            v.visit_with(self);

            if let Some(ref init) = v.init {
                let span = init.span();

                //  Check if v_ty is assignable to ty
                let value_ty = match self.type_of(&init).and_then(|ty| self.fix_type(span, ty)) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.info.errors.push(err);
                        return;
                    }
                };

                match v.name.get_ty() {
                    Some(ty) => {
                        let ty = Type::from(ty.clone());
                        let ty = match self.fix_type(span, Cow::Owned(ty)) {
                            Ok(ty) => ty,
                            Err(err) => {
                                self.info.errors.push(err);
                                return;
                            }
                        };
                        let error = value_ty.assign_to(&ty);
                        match error {
                            Ok(()) => {
                                self.scope.declare_vars(kind, &v.name);
                                return;
                            }
                            Err(err) => {
                                self.info.errors.push(err);
                            }
                        }
                    }
                    None => {
                        // infer type from value.

                        let ty = value_ty.generalize_lit().to_static();

                        self.scope.declare_var(
                            kind,
                            match v.name {
                                Pat::Ident(ref i) => i.sym.clone(),
                                _ => unimplemented!("declare_var with complex type inference"),
                            },
                            Some(ty),
                            // initialized
                            true,
                            // Variable declarations does not allow multiple declarations with same
                            // name
                            false,
                        );
                        return;
                    }
                }
            } else {
                if !var.declare {
                    let (sym, ty) = match v.name {
                        Pat::Ident(Ident {
                            ref sym,
                            ref type_ann,
                            ..
                        }) => (
                            sym.clone(),
                            type_ann.as_ref().map(|t| Type::from(t.type_ann.clone())),
                        ),
                        _ => unreachable!(
                            "complex pattern without initializer is invalid syntax and parser \
                             should handle it"
                        ),
                    };
                    self.scope.declare_var(
                        kind, sym, ty, false, // initialized
                        false, // allow_multiple
                    );
                    return;
                }
            }

            self.scope.declare_vars(kind, &v.name);
        });
    }
}

impl Analyzer<'_, '_> {
    fn try_assign(&mut self, lhs: &PatOrExpr, ty: Cow<TsType>) {
        match *lhs {
            PatOrExpr::Expr(ref expr) | PatOrExpr::Pat(box Pat::Expr(ref expr)) => match **expr {
                // TODO(kdy1): Validate
                Expr::Member(MemberExpr { .. }) => return,
                _ => unimplemented!(
                    "assign: {:?} = {:?}\nFile: {}",
                    expr,
                    ty,
                    self.path.display()
                ),
            },

            PatOrExpr::Pat(ref pat) => {
                // Update variable's type
                match **pat {
                    Pat::Ident(ref i) => {
                        if let Some(var_info) = self.scope.vars.get_mut(&i.sym) {
                            // Variable is declared.

                            let var_ty = if let Some(ref var_ty) = var_info.ty {
                                // let foo: string;
                                // let foo = 'value';

                                let errors = ty.assign_to(&var_ty);
                                if errors.is_none() {
                                    Some(ty.into_owned())
                                } else {
                                    self.info.errors.extend(errors);
                                    None
                                }
                            } else {
                                // let v = foo;
                                // v = bar;
                                None
                            };
                            if let Some(var_ty) = var_ty {
                                if var_info.ty.is_none() || !var_info.ty.as_ref().unwrap().is_any()
                                {
                                    var_info.ty = Some(var_ty);
                                }
                            }
                        } else {
                            let var_info = if let Some(var_info) = self.scope.search_parent(&i.sym)
                            {
                                VarInfo {
                                    ty: if var_info.ty.is_some()
                                        && var_info.ty.as_ref().unwrap().is_any()
                                    {
                                        Some(any(var_info.ty.as_ref().unwrap().span()))
                                    } else {
                                        Some(ty.into_owned())
                                    },
                                    copied: true,
                                    ..var_info.clone()
                                }
                            } else {
                                // undefined symbol
                                self.info
                                    .errors
                                    .push(Error::UndefinedSymbol { span: i.span });
                                return;
                            };
                            // Variable is defined on parent scope.
                            //
                            // We copy varinfo with enhanced type.
                            self.scope.vars.insert(i.sym.clone(), var_info);
                        }
                    }

                    _ => unimplemented!("assignment with complex pattern"),
                }
            }
        }
    }
}

/// Analyzes a module.
///
/// Constants are propagated, and
impl Checker<'_> {
    pub fn analyze_module(&self, rule: Rule, path: Arc<PathBuf>, m: &Module) -> Info {
        ::swc_common::GLOBALS.set(&self.globals, || {
            let mut a = Analyzer::new(&self.libs, rule, Scope::root(), path, &self);
            m.visit_with(&mut a);

            a.info
        })
    }
}

#[test]
fn assert_types() {
    fn is_sync<T: Sync>() {}
    fn is_send<T: Send>() {}
    is_sync::<Info>();
    is_send::<Info>();
}
