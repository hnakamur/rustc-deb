//! Checks for needless address of operations (`&`)
//!
//! This lint is **warn** by default

use crate::utils::{in_macro, snippet_opt, span_lint_and_then};
use if_chain::if_chain;
use rustc::hir::{BindingAnnotation, Expr, ExprKind, Item, MutImmutable, Pat, PatKind};
use rustc::lint::{LateContext, LateLintPass, LintArray, LintPass};
use rustc::ty;
use rustc::ty::adjustment::{Adjust, Adjustment};
use rustc::{declare_tool_lint, lint_array};
use rustc_errors::Applicability;
use syntax::ast::NodeId;

/// **What it does:** Checks for address of operations (`&`) that are going to
/// be dereferenced immediately by the compiler.
///
/// **Why is this bad?** Suggests that the receiver of the expression borrows
/// the expression.
///
/// **Example:**
/// ```rust
/// let x: &i32 = &&&&&&5;
/// ```
///
/// **Known problems:** None.
declare_clippy_lint! {
    pub NEEDLESS_BORROW,
    nursery,
    "taking a reference that is going to be automatically dereferenced"
}

#[derive(Default)]
pub struct NeedlessBorrow {
    derived_item: Option<NodeId>,
}

impl LintPass for NeedlessBorrow {
    fn get_lints(&self) -> LintArray {
        lint_array!(NEEDLESS_BORROW)
    }
}

impl<'a, 'tcx> LateLintPass<'a, 'tcx> for NeedlessBorrow {
    fn check_expr(&mut self, cx: &LateContext<'a, 'tcx>, e: &'tcx Expr) {
        if in_macro(e.span) || self.derived_item.is_some() {
            return;
        }
        if let ExprKind::AddrOf(MutImmutable, ref inner) = e.node {
            if let ty::Ref(..) = cx.tables.expr_ty(inner).sty {
                for adj3 in cx.tables.expr_adjustments(e).windows(3) {
                    if let [Adjustment {
                        kind: Adjust::Deref(_), ..
                    }, Adjustment {
                        kind: Adjust::Deref(_), ..
                    }, Adjustment {
                        kind: Adjust::Borrow(_),
                        ..
                    }] = *adj3
                    {
                        span_lint_and_then(
                            cx,
                            NEEDLESS_BORROW,
                            e.span,
                            "this expression borrows a reference that is immediately dereferenced \
                             by the compiler",
                            |db| {
                                if let Some(snippet) = snippet_opt(cx, inner.span) {
                                    db.span_suggestion_with_applicability(
                                        e.span,
                                        "change this to",
                                        snippet,
                                        Applicability::MachineApplicable,
                                    );
                                }
                            },
                        );
                    }
                }
            }
        }
    }
    fn check_pat(&mut self, cx: &LateContext<'a, 'tcx>, pat: &'tcx Pat) {
        if in_macro(pat.span) || self.derived_item.is_some() {
            return;
        }
        if_chain! {
            if let PatKind::Binding(BindingAnnotation::Ref, _, name, _) = pat.node;
            if let ty::Ref(_, tam, mutbl) = cx.tables.pat_ty(pat).sty;
            if mutbl == MutImmutable;
            if let ty::Ref(_, _, mutbl) = tam.sty;
            // only lint immutable refs, because borrowed `&mut T` cannot be moved out
            if mutbl == MutImmutable;
            then {
                span_lint_and_then(
                    cx,
                    NEEDLESS_BORROW,
                    pat.span,
                    "this pattern creates a reference to a reference",
                    |db| {
                        if let Some(snippet) = snippet_opt(cx, name.span) {
                            db.span_suggestion_with_applicability(
                                pat.span,
                                "change this to",
                                snippet,
                                Applicability::MachineApplicable,
                            );
                        }
                    }
                )
            }
        }
    }

    fn check_item(&mut self, _: &LateContext<'a, 'tcx>, item: &'tcx Item) {
        if item.attrs.iter().any(|a| a.check_name("automatically_derived")) {
            debug_assert!(self.derived_item.is_none());
            self.derived_item = Some(item.id);
        }
    }

    fn check_item_post(&mut self, _: &LateContext<'a, 'tcx>, item: &'tcx Item) {
        if let Some(id) = self.derived_item {
            if item.id == id {
                self.derived_item = None;
            }
        }
    }
}
