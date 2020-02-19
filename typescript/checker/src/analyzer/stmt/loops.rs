use super::super::Analyzer;
use crate::{
    analyzer::{expr::TypeOfMode, ScopeKind},
    errors::Error,
    ty::{Array, Type},
    validator::Validate,
    ValidationResult,
};
use macros::validator_method;
use swc_common::{Span, Spanned, VisitWith};
use swc_ecma_ast::*;

impl Analyzer<'_, '_> {
    #[validator_method]
    fn check_lhs_of_for_loop(&mut self, e: &VarDeclOrPat) {
        match *e {
            VarDeclOrPat::VarDecl(ref v) => {
                // Store variables
                v.visit_with(self);
            }
            VarDeclOrPat::Pat(ref mut pat) => match *pat {
                Pat::Expr(ref mut e) => {
                    self.validate_expr(&mut e, TypeOfMode::LValue, None)?;
                }
                Pat::Ident(ref mut i) => {
                    // TODO: verify
                    self.type_of_ident(i, TypeOfMode::LValue, None)?;
                }
                _ => {}
            },
        }
    }

    fn check_rhs_of_for_loop(&mut self, e: &mut Expr) -> ValidationResult {
        // Check iterable
        self.validate(e)
    }

    fn validate_for_loop(&mut self, span: Span, lhs: &VarDeclOrPat, rty: Type) {
        match lhs {
            VarDeclOrPat::Pat(Pat::Expr(ref l)) => {
                let lty = match self.validate_expr(&mut **l, TypeOfMode::LValue, None) {
                    Ok(ty) => ty,
                    Err(..) => return,
                };

                println!("FOO\nL: {:?}", lty);
                match self.assign(
                    &Type::Array(Array {
                        span,
                        elem_type: box lty,
                    }),
                    &rty,
                    lhs.span(),
                ) {
                    Ok(..) => {}
                    Err(err) => self.info.errors.push(err),
                }
            }
            _ => {}
        }
    }

    #[validator_method]
    fn check_for_of_in_loop(&mut self, span: Span, left: &VarDeclOrPat, rhs: &mut Expr) {
        self.with_child(
            ScopeKind::Flow,
            Default::default(),
            |child| -> ValidationResult<()> {
                child.check_lhs_of_for_loop(left);
                let rty = if match left {
                    VarDeclOrPat::VarDecl(VarDecl { ref decls, .. }) => !decls.is_empty(),
                    _ => true,
                } {
                    child.check_rhs_of_for_loop(rhs)?
                } else {
                    return Ok(());
                };

                child.validate_for_loop(span, &left, rty);

                Ok(())
            },
        )?;
    }
}

impl Validate<ForInStmt> for Analyzer<'_, '_> {
    type Output = ();

    fn validate(&mut self, s: &mut ForInStmt) {
        self.check_for_of_in_loop(s.span, &mut s.left, &mut s.right);
    }
}

impl Validate<ForOfStmt> for Analyzer<'_, '_> {
    type Output = ();

    fn validate(&mut self, s: &mut ForOfStmt) {
        self.check_for_of_in_loop(s.span, &mut s.left, &mut s.right);
    }
}
