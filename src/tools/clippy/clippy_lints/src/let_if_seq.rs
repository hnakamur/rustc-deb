use crate::utils::{snippet, span_lint_and_then};
use if_chain::if_chain;
use rustc::hir;
use rustc::hir::def::Def;
use rustc::hir::BindingAnnotation;
use rustc::lint::{LateContext, LateLintPass, LintArray, LintPass};
use rustc::{declare_tool_lint, lint_array};
use rustc_errors::Applicability;
use syntax::ast;

/// **What it does:** Checks for variable declarations immediately followed by a
/// conditional affectation.
///
/// **Why is this bad?** This is not idiomatic Rust.
///
/// **Known problems:** None.
///
/// **Example:**
/// ```rust,ignore
/// let foo;
///
/// if bar() {
///     foo = 42;
/// } else {
///     foo = 0;
/// }
///
/// let mut baz = None;
///
/// if bar() {
///     baz = Some(42);
/// }
/// ```
///
/// should be written
///
/// ```rust,ignore
/// let foo = if bar() {
///     42
/// } else {
///     0
/// };
///
/// let baz = if bar() {
///     Some(42)
/// } else {
///     None
/// };
/// ```
declare_clippy_lint! {
    pub USELESS_LET_IF_SEQ,
    style,
    "unidiomatic `let mut` declaration followed by initialization in `if`"
}

#[derive(Copy, Clone)]
pub struct LetIfSeq;

impl LintPass for LetIfSeq {
    fn get_lints(&self) -> LintArray {
        lint_array!(USELESS_LET_IF_SEQ)
    }
}

impl<'a, 'tcx> LateLintPass<'a, 'tcx> for LetIfSeq {
    fn check_block(&mut self, cx: &LateContext<'a, 'tcx>, block: &'tcx hir::Block) {
        let mut it = block.stmts.iter().peekable();
        while let Some(stmt) = it.next() {
            if_chain! {
                if let Some(expr) = it.peek();
                if let hir::StmtKind::Decl(ref decl, _) = stmt.node;
                if let hir::DeclKind::Local(ref decl) = decl.node;
                if let hir::PatKind::Binding(mode, canonical_id, ident, None) = decl.pat.node;
                if let hir::StmtKind::Expr(ref if_, _) = expr.node;
                if let hir::ExprKind::If(ref cond, ref then, ref else_) = if_.node;
                if !used_in_expr(cx, canonical_id, cond);
                if let hir::ExprKind::Block(ref then, _) = then.node;
                if let Some(value) = check_assign(cx, canonical_id, &*then);
                if !used_in_expr(cx, canonical_id, value);
                then {
                    let span = stmt.span.to(if_.span);

                    let (default_multi_stmts, default) = if let Some(ref else_) = *else_ {
                        if let hir::ExprKind::Block(ref else_, _) = else_.node {
                            if let Some(default) = check_assign(cx, canonical_id, else_) {
                                (else_.stmts.len() > 1, default)
                            } else if let Some(ref default) = decl.init {
                                (true, &**default)
                            } else {
                                continue;
                            }
                        } else {
                            continue;
                        }
                    } else if let Some(ref default) = decl.init {
                        (false, &**default)
                    } else {
                        continue;
                    };

                    let mutability = match mode {
                        BindingAnnotation::RefMut | BindingAnnotation::Mutable => "<mut> ",
                        _ => "",
                    };

                    // FIXME: this should not suggest `mut` if we can detect that the variable is not
                    // use mutably after the `if`

                    let sug = format!(
                        "let {mut}{name} = if {cond} {{{then} {value} }} else {{{else} {default} }};",
                        mut=mutability,
                        name=ident.name,
                        cond=snippet(cx, cond.span, "_"),
                        then=if then.stmts.len() > 1 { " ..;" } else { "" },
                        else=if default_multi_stmts { " ..;" } else { "" },
                        value=snippet(cx, value.span, "<value>"),
                        default=snippet(cx, default.span, "<default>"),
                    );
                    span_lint_and_then(cx,
                                       USELESS_LET_IF_SEQ,
                                       span,
                                       "`if _ { .. } else { .. }` is an expression",
                                       |db| {
                                           db.span_suggestion_with_applicability(
                                                span,
                                                "it is more idiomatic to write",
                                                sug,
                                                Applicability::HasPlaceholders,
                                            );
                                           if !mutability.is_empty() {
                                               db.note("you might not need `mut` at all");
                                           }
                                       });
                }
            }
        }
    }
}

struct UsedVisitor<'a, 'tcx: 'a> {
    cx: &'a LateContext<'a, 'tcx>,
    id: ast::NodeId,
    used: bool,
}

impl<'a, 'tcx> hir::intravisit::Visitor<'tcx> for UsedVisitor<'a, 'tcx> {
    fn visit_expr(&mut self, expr: &'tcx hir::Expr) {
        if_chain! {
            if let hir::ExprKind::Path(ref qpath) = expr.node;
            if let Def::Local(local_id) = self.cx.tables.qpath_def(qpath, expr.hir_id);
            if self.id == local_id;
            then {
                self.used = true;
                return;
            }
        }
        hir::intravisit::walk_expr(self, expr);
    }
    fn nested_visit_map<'this>(&'this mut self) -> hir::intravisit::NestedVisitorMap<'this, 'tcx> {
        hir::intravisit::NestedVisitorMap::None
    }
}

fn check_assign<'a, 'tcx>(
    cx: &LateContext<'a, 'tcx>,
    decl: ast::NodeId,
    block: &'tcx hir::Block,
) -> Option<&'tcx hir::Expr> {
    if_chain! {
        if block.expr.is_none();
        if let Some(expr) = block.stmts.iter().last();
        if let hir::StmtKind::Semi(ref expr, _) = expr.node;
        if let hir::ExprKind::Assign(ref var, ref value) = expr.node;
        if let hir::ExprKind::Path(ref qpath) = var.node;
        if let Def::Local(local_id) = cx.tables.qpath_def(qpath, var.hir_id);
        if decl == local_id;
        then {
            let mut v = UsedVisitor {
                cx,
                id: decl,
                used: false,
            };

            for s in block.stmts.iter().take(block.stmts.len()-1) {
                hir::intravisit::walk_stmt(&mut v, s);

                if v.used {
                    return None;
                }
            }

            return Some(value);
        }
    }

    None
}

fn used_in_expr<'a, 'tcx: 'a>(cx: &LateContext<'a, 'tcx>, id: ast::NodeId, expr: &'tcx hir::Expr) -> bool {
    let mut v = UsedVisitor { cx, id, used: false };
    hir::intravisit::walk_expr(&mut v, expr);
    v.used
}
