use crate::utils::{get_trait_def_id, implements_trait, snippet_opt, span_lint_and_then, SpanlessEq};
use crate::utils::{higher, sugg};
use if_chain::if_chain;
use rustc::hir;
use rustc::hir::intravisit::{walk_expr, NestedVisitorMap, Visitor};
use rustc::lint::{LateContext, LateLintPass, LintArray, LintPass};
use rustc::{declare_tool_lint, lint_array};
use rustc_errors::Applicability;
use syntax::ast;

/// **What it does:** Checks for `a = a op b` or `a = b commutative_op a`
/// patterns.
///
/// **Why is this bad?** These can be written as the shorter `a op= b`.
///
/// **Known problems:** While forbidden by the spec, `OpAssign` traits may have
/// implementations that differ from the regular `Op` impl.
///
/// **Example:**
/// ```rust
/// let mut a = 5;
/// ...
/// a = a + b;
/// ```
declare_clippy_lint! {
    pub ASSIGN_OP_PATTERN,
    style,
    "assigning the result of an operation on a variable to that same variable"
}

/// **What it does:** Checks for `a op= a op b` or `a op= b op a` patterns.
///
/// **Why is this bad?** Most likely these are bugs where one meant to write `a
/// op= b`.
///
/// **Known problems:** Clippy cannot know for sure if `a op= a op b` should have
/// been `a = a op a op b` or `a = a op b`/`a op= b`. Therefore it suggests both.
/// If `a op= a op b` is really the correct behaviour it should be
/// written as `a = a op a op b` as it's less confusing.
///
/// **Example:**
/// ```rust
/// let mut a = 5;
/// ...
/// a += a + b;
/// ```
declare_clippy_lint! {
    pub MISREFACTORED_ASSIGN_OP,
    complexity,
    "having a variable on both sides of an assign op"
}

#[derive(Copy, Clone, Default)]
pub struct AssignOps;

impl LintPass for AssignOps {
    fn get_lints(&self) -> LintArray {
        lint_array!(ASSIGN_OP_PATTERN, MISREFACTORED_ASSIGN_OP)
    }
}

impl<'a, 'tcx> LateLintPass<'a, 'tcx> for AssignOps {
    fn check_expr(&mut self, cx: &LateContext<'a, 'tcx>, expr: &'tcx hir::Expr) {
        match &expr.node {
            hir::ExprKind::AssignOp(op, lhs, rhs) => {
                if let hir::ExprKind::Binary(binop, l, r) = &rhs.node {
                    if op.node == binop.node {
                        let lint = |assignee: &hir::Expr, rhs_other: &hir::Expr| {
                            span_lint_and_then(
                                cx,
                                MISREFACTORED_ASSIGN_OP,
                                expr.span,
                                "variable appears on both sides of an assignment operation",
                                |db| {
                                    if let (Some(snip_a), Some(snip_r)) =
                                        (snippet_opt(cx, assignee.span), snippet_opt(cx, rhs_other.span))
                                    {
                                        let a = &sugg::Sugg::hir(cx, assignee, "..");
                                        let r = &sugg::Sugg::hir(cx, rhs, "..");
                                        let long =
                                            format!("{} = {}", snip_a, sugg::make_binop(higher::binop(op.node), a, r));
                                        db.span_suggestion_with_applicability(
                                            expr.span,
                                            &format!(
                                                "Did you mean {} = {} {} {} or {}? Consider replacing it with",
                                                snip_a,
                                                snip_a,
                                                op.node.as_str(),
                                                snip_r,
                                                long
                                            ),
                                            format!("{} {}= {}", snip_a, op.node.as_str(), snip_r),
                                            Applicability::MachineApplicable,
                                        );
                                        db.span_suggestion_with_applicability(
                                            expr.span,
                                            "or",
                                            long,
                                            Applicability::MachineApplicable, // snippet
                                        );
                                    }
                                },
                            );
                        };
                        // lhs op= l op r
                        if SpanlessEq::new(cx).ignore_fn().eq_expr(lhs, l) {
                            lint(lhs, r);
                        }
                        // lhs op= l commutative_op r
                        if is_commutative(op.node) && SpanlessEq::new(cx).ignore_fn().eq_expr(lhs, r) {
                            lint(lhs, l);
                        }
                    }
                }
            },
            hir::ExprKind::Assign(assignee, e) => {
                if let hir::ExprKind::Binary(op, l, r) = &e.node {
                    #[allow(clippy::cyclomatic_complexity)]
                    let lint = |assignee: &hir::Expr, rhs: &hir::Expr| {
                        let ty = cx.tables.expr_ty(assignee);
                        let rty = cx.tables.expr_ty(rhs);
                        macro_rules! ops {
                            ($op:expr,
                             $cx:expr,
                             $ty:expr,
                             $rty:expr,
                             $($trait_name:ident),+) => {
                                match $op {
                                    $(hir::BinOpKind::$trait_name => {
                                        let [krate, module] = crate::utils::paths::OPS_MODULE;
                                        let path = [krate, module, concat!(stringify!($trait_name), "Assign")];
                                        let trait_id = if let Some(trait_id) = get_trait_def_id($cx, &path) {
                                            trait_id
                                        } else {
                                            return; // useless if the trait doesn't exist
                                        };
                                        // check that we are not inside an `impl AssignOp` of this exact operation
                                        let parent_fn = cx.tcx.hir().get_parent(e.id);
                                        let parent_impl = cx.tcx.hir().get_parent(parent_fn);
                                        // the crate node is the only one that is not in the map
                                        if_chain! {
                                            if parent_impl != ast::CRATE_NODE_ID;
                                            if let hir::Node::Item(item) = cx.tcx.hir().get(parent_impl);
                                            if let hir::ItemKind::Impl(_, _, _, _, Some(trait_ref), _, _) =
                                                &item.node;
                                            if trait_ref.path.def.def_id() == trait_id;
                                            then { return; }
                                        }
                                        implements_trait($cx, $ty, trait_id, &[$rty])
                                    },)*
                                    _ => false,
                                }
                            }
                        }
                        if ops!(
                            op.node,
                            cx,
                            ty,
                            rty.into(),
                            Add,
                            Sub,
                            Mul,
                            Div,
                            Rem,
                            And,
                            Or,
                            BitAnd,
                            BitOr,
                            BitXor,
                            Shr,
                            Shl
                        ) {
                            span_lint_and_then(
                                cx,
                                ASSIGN_OP_PATTERN,
                                expr.span,
                                "manual implementation of an assign operation",
                                |db| {
                                    if let (Some(snip_a), Some(snip_r)) =
                                        (snippet_opt(cx, assignee.span), snippet_opt(cx, rhs.span))
                                    {
                                        db.span_suggestion_with_applicability(
                                            expr.span,
                                            "replace it with",
                                            format!("{} {}= {}", snip_a, op.node.as_str(), snip_r),
                                            Applicability::MachineApplicable,
                                        );
                                    }
                                },
                            );
                        }
                    };

                    let mut visitor = ExprVisitor {
                        assignee,
                        counter: 0,
                        cx,
                    };

                    walk_expr(&mut visitor, e);

                    if visitor.counter == 1 {
                        // a = a op b
                        if SpanlessEq::new(cx).ignore_fn().eq_expr(assignee, l) {
                            lint(assignee, r);
                        }
                        // a = b commutative_op a
                        // Limited to primitive type as these ops are know to be commutative
                        if SpanlessEq::new(cx).ignore_fn().eq_expr(assignee, r)
                            && cx.tables.expr_ty(assignee).is_primitive_ty()
                        {
                            match op.node {
                                hir::BinOpKind::Add
                                | hir::BinOpKind::Mul
                                | hir::BinOpKind::And
                                | hir::BinOpKind::Or
                                | hir::BinOpKind::BitXor
                                | hir::BinOpKind::BitAnd
                                | hir::BinOpKind::BitOr => {
                                    lint(assignee, l);
                                },
                                _ => {},
                            }
                        }
                    }
                }
            },
            _ => {},
        }
    }
}

fn is_commutative(op: hir::BinOpKind) -> bool {
    use rustc::hir::BinOpKind::*;
    match op {
        Add | Mul | And | Or | BitXor | BitAnd | BitOr | Eq | Ne => true,
        Sub | Div | Rem | Shl | Shr | Lt | Le | Ge | Gt => false,
    }
}

struct ExprVisitor<'a, 'tcx: 'a> {
    assignee: &'a hir::Expr,
    counter: u8,
    cx: &'a LateContext<'a, 'tcx>,
}

impl<'a, 'tcx: 'a> Visitor<'tcx> for ExprVisitor<'a, 'tcx> {
    fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
        if SpanlessEq::new(self.cx).ignore_fn().eq_expr(self.assignee, expr) {
            self.counter += 1;
        }

        walk_expr(self, expr);
    }
    fn nested_visit_map<'this>(&'this mut self) -> NestedVisitorMap<'this, 'tcx> {
        NestedVisitorMap::None
    }
}
