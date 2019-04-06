use crate::utils::{in_macro, match_path_ast, snippet_opt, span_lint_and_then, span_note_and_lint};
use if_chain::if_chain;
use rustc::lint::{in_external_macro, EarlyContext, EarlyLintPass, LintArray, LintContext, LintPass};
use rustc::{declare_tool_lint, lint_array};
use rustc_errors::Applicability;
use syntax::ast;
use syntax::source_map::Span;
use syntax::visit::FnKind;
use syntax_pos::BytePos;

/// **What it does:** Checks for return statements at the end of a block.
///
/// **Why is this bad?** Removing the `return` and semicolon will make the code
/// more rusty.
///
/// **Known problems:** If the computation returning the value borrows a local
/// variable, removing the `return` may run afoul of the borrow checker.
///
/// **Example:**
/// ```rust
/// fn foo(x: usize) {
///     return x;
/// }
/// ```
/// simplify to
/// ```rust
/// fn foo(x: usize) {
///     x
/// }
/// ```
declare_clippy_lint! {
    pub NEEDLESS_RETURN,
    style,
    "using a return statement like `return expr;` where an expression would suffice"
}

/// **What it does:** Checks for `let`-bindings, which are subsequently
/// returned.
///
/// **Why is this bad?** It is just extraneous code. Remove it to make your code
/// more rusty.
///
/// **Known problems:** None.
///
/// **Example:**
/// ```rust
/// fn foo() -> String {
///     let x = String::new();
///     x
/// }
/// ```
/// instead, use
/// ```
/// fn foo() -> String {
///     String::new()
/// }
/// ```
declare_clippy_lint! {
    pub LET_AND_RETURN,
    style,
    "creating a let-binding and then immediately returning it like `let x = expr; x` at the end of a block"
}

/// **What it does:** Checks for unit (`()`) expressions that can be removed.
///
/// **Why is this bad?** Such expressions add no value, but can make the code
/// less readable. Depending on formatting they can make a `break` or `return`
/// statement look like a function call.
///
/// **Known problems:** The lint currently misses unit return types in types,
/// e.g. the `F` in `fn generic_unit<F: Fn() -> ()>(f: F) { .. }`.
///
/// **Example:**
/// ```rust
/// fn return_unit() -> () {
///     ()
/// }
/// ```
declare_clippy_lint! {
    pub UNUSED_UNIT,
    style,
    "needless unit expression"
}

#[derive(Copy, Clone)]
pub struct ReturnPass;

impl ReturnPass {
    // Check the final stmt or expr in a block for unnecessary return.
    fn check_block_return(&mut self, cx: &EarlyContext<'_>, block: &ast::Block) {
        if let Some(stmt) = block.stmts.last() {
            match stmt.node {
                ast::StmtKind::Expr(ref expr) | ast::StmtKind::Semi(ref expr) => {
                    self.check_final_expr(cx, expr, Some(stmt.span));
                },
                _ => (),
            }
        }
    }

    // Check a the final expression in a block if it's a return.
    fn check_final_expr(&mut self, cx: &EarlyContext<'_>, expr: &ast::Expr, span: Option<Span>) {
        match expr.node {
            // simple return is always "bad"
            ast::ExprKind::Ret(Some(ref inner)) => {
                // allow `#[cfg(a)] return a; #[cfg(b)] return b;`
                if !expr.attrs.iter().any(attr_is_cfg) {
                    self.emit_return_lint(cx, span.expect("`else return` is not possible"), inner.span);
                }
            },
            // a whole block? check it!
            ast::ExprKind::Block(ref block, _) => {
                self.check_block_return(cx, block);
            },
            // an if/if let expr, check both exprs
            // note, if without else is going to be a type checking error anyways
            // (except for unit type functions) so we don't match it
            ast::ExprKind::If(_, ref ifblock, Some(ref elsexpr)) => {
                self.check_block_return(cx, ifblock);
                self.check_final_expr(cx, elsexpr, None);
            },
            // a match expr, check all arms
            ast::ExprKind::Match(_, ref arms) => {
                for arm in arms {
                    self.check_final_expr(cx, &arm.body, Some(arm.body.span));
                }
            },
            _ => (),
        }
    }

    fn emit_return_lint(&mut self, cx: &EarlyContext<'_>, ret_span: Span, inner_span: Span) {
        if in_external_macro(cx.sess(), inner_span) || in_macro(inner_span) {
            return;
        }
        span_lint_and_then(cx, NEEDLESS_RETURN, ret_span, "unneeded return statement", |db| {
            if let Some(snippet) = snippet_opt(cx, inner_span) {
                db.span_suggestion_with_applicability(
                    ret_span,
                    "remove `return` as shown",
                    snippet,
                    Applicability::MachineApplicable,
                );
            }
        });
    }

    // Check for "let x = EXPR; x"
    fn check_let_return(&mut self, cx: &EarlyContext<'_>, block: &ast::Block) {
        let mut it = block.stmts.iter();

        // we need both a let-binding stmt and an expr
        if_chain! {
            if let Some(retexpr) = it.next_back();
            if let ast::StmtKind::Expr(ref retexpr) = retexpr.node;
            if let Some(stmt) = it.next_back();
            if let ast::StmtKind::Local(ref local) = stmt.node;
            // don't lint in the presence of type inference
            if local.ty.is_none();
            if !local.attrs.iter().any(attr_is_cfg);
            if let Some(ref initexpr) = local.init;
            if let ast::PatKind::Ident(_, ident, _) = local.pat.node;
            if let ast::ExprKind::Path(_, ref path) = retexpr.node;
            if match_path_ast(path, &[&ident.as_str()]);
            if !in_external_macro(cx.sess(), initexpr.span);
            then {
                    span_note_and_lint(cx,
                                       LET_AND_RETURN,
                                       retexpr.span,
                                       "returning the result of a let binding from a block. \
                                       Consider returning the expression directly.",
                                       initexpr.span,
                                       "this expression can be directly returned");
            }
        }
    }
}

impl LintPass for ReturnPass {
    fn get_lints(&self) -> LintArray {
        lint_array!(NEEDLESS_RETURN, LET_AND_RETURN, UNUSED_UNIT)
    }
}

impl EarlyLintPass for ReturnPass {
    fn check_fn(&mut self, cx: &EarlyContext<'_>, kind: FnKind<'_>, decl: &ast::FnDecl, span: Span, _: ast::NodeId) {
        match kind {
            FnKind::ItemFn(.., block) | FnKind::Method(.., block) => self.check_block_return(cx, block),
            FnKind::Closure(body) => self.check_final_expr(cx, body, Some(body.span)),
        }
        if_chain! {
            if let ast::FunctionRetTy::Ty(ref ty) = decl.output;
            if let ast::TyKind::Tup(ref vals) = ty.node;
            if vals.is_empty() && !in_macro(ty.span) && get_def(span) == get_def(ty.span);
            then {
                let (rspan, appl) = if let Ok(fn_source) =
                        cx.sess().source_map()
                                 .span_to_snippet(span.with_hi(ty.span.hi())) {
                    if let Some(rpos) = fn_source.rfind("->") {
                        #[allow(clippy::cast_possible_truncation)]
                        (ty.span.with_lo(BytePos(span.lo().0 + rpos as u32)),
                            Applicability::MachineApplicable)
                    } else {
                        (ty.span, Applicability::MaybeIncorrect)
                    }
                } else {
                    (ty.span, Applicability::MaybeIncorrect)
                };
                span_lint_and_then(cx, UNUSED_UNIT, rspan, "unneeded unit return type", |db| {
                    db.span_suggestion_with_applicability(
                        rspan,
                        "remove the `-> ()`",
                        String::new(),
                        appl,
                    );
                });
            }
        }
    }

    fn check_block(&mut self, cx: &EarlyContext<'_>, block: &ast::Block) {
        self.check_let_return(cx, block);
        if_chain! {
            if let Some(ref stmt) = block.stmts.last();
            if let ast::StmtKind::Expr(ref expr) = stmt.node;
            if is_unit_expr(expr) && !in_macro(expr.span);
            then {
                let sp = expr.span;
                span_lint_and_then(cx, UNUSED_UNIT, sp, "unneeded unit expression", |db| {
                    db.span_suggestion_with_applicability(
                        sp,
                        "remove the final `()`",
                        String::new(),
                        Applicability::MachineApplicable,
                    );
                });
            }
        }
    }

    fn check_expr(&mut self, cx: &EarlyContext<'_>, e: &ast::Expr) {
        match e.node {
            ast::ExprKind::Ret(Some(ref expr)) | ast::ExprKind::Break(_, Some(ref expr)) => {
                if is_unit_expr(expr) && !in_macro(expr.span) {
                    span_lint_and_then(cx, UNUSED_UNIT, expr.span, "unneeded `()`", |db| {
                        db.span_suggestion_with_applicability(
                            expr.span,
                            "remove the `()`",
                            String::new(),
                            Applicability::MachineApplicable,
                        );
                    });
                }
            },
            _ => (),
        }
    }
}

fn attr_is_cfg(attr: &ast::Attribute) -> bool {
    attr.meta_item_list().is_some() && attr.name() == "cfg"
}

// get the def site
fn get_def(span: Span) -> Option<Span> {
    span.ctxt().outer().expn_info().and_then(|info| info.def_site)
}

// is this expr a `()` unit?
fn is_unit_expr(expr: &ast::Expr) -> bool {
    if let ast::ExprKind::Tup(ref vals) = expr.node {
        vals.is_empty()
    } else {
        false
    }
}
