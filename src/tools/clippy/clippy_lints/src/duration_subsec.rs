use if_chain::if_chain;
use rustc::hir::*;
use rustc::lint::{LateContext, LateLintPass, LintArray, LintPass};
use rustc::{declare_tool_lint, lint_array};
use rustc_errors::Applicability;
use syntax::source_map::Spanned;

use crate::consts::{constant, Constant};
use crate::utils::paths;
use crate::utils::{match_type, snippet_with_applicability, span_lint_and_sugg, walk_ptrs_ty};

/// **What it does:** Checks for calculation of subsecond microseconds or milliseconds
/// from other `Duration` methods.
///
/// **Why is this bad?** It's more concise to call `Duration::subsec_micros()` or
/// `Duration::subsec_millis()` than to calculate them.
///
/// **Known problems:** None.
///
/// **Example:**
/// ```rust
/// let dur = Duration::new(5, 0);
/// let _micros = dur.subsec_nanos() / 1_000;
/// let _millis = dur.subsec_nanos() / 1_000_000;
/// ```
declare_clippy_lint! {
    pub DURATION_SUBSEC,
    complexity,
    "checks for calculation of subsecond microseconds or milliseconds"
}

#[derive(Copy, Clone)]
pub struct DurationSubsec;

impl LintPass for DurationSubsec {
    fn get_lints(&self) -> LintArray {
        lint_array!(DURATION_SUBSEC)
    }
}

impl<'a, 'tcx> LateLintPass<'a, 'tcx> for DurationSubsec {
    fn check_expr(&mut self, cx: &LateContext<'a, 'tcx>, expr: &'tcx Expr) {
        if_chain! {
            if let ExprKind::Binary(Spanned { node: BinOpKind::Div, .. }, ref left, ref right) = expr.node;
            if let ExprKind::MethodCall(ref method_path, _ , ref args) = left.node;
            if match_type(cx, walk_ptrs_ty(cx.tables.expr_ty(&args[0])), &paths::DURATION);
            if let Some((Constant::Int(divisor), _)) = constant(cx, cx.tables, right);
            then {
                let suggested_fn = match (method_path.ident.as_str().as_ref(), divisor) {
                    ("subsec_micros", 1_000) | ("subsec_nanos", 1_000_000) => "subsec_millis",
                    ("subsec_nanos", 1_000) => "subsec_micros",
                    _ => return,
                };
                let mut applicability = Applicability::MachineApplicable;
                span_lint_and_sugg(
                    cx,
                    DURATION_SUBSEC,
                    expr.span,
                    &format!("Calling `{}()` is more concise than this calculation", suggested_fn),
                    "try",
                    format!(
                        "{}.{}()",
                        snippet_with_applicability(cx, args[0].span, "_", &mut applicability),
                        suggested_fn
                    ),
                    applicability,
                );
            }
        }
    }
}
