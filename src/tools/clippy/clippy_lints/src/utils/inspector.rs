//! checks for attributes

use crate::utils::get_attr;
use rustc::hir;
use rustc::hir::print;
use rustc::lint::{LateContext, LateLintPass, LintArray, LintPass};
use rustc::{declare_tool_lint, lint_array};
use syntax::ast::Attribute;

/// **What it does:** Dumps every ast/hir node which has the `#[clippy::dump]`
/// attribute
///
/// **Example:**
/// ```rust
/// #[clippy::dump]
/// extern crate foo;
/// ```
///
/// prints
///
/// ```
/// item `foo`
/// visibility inherited from outer item
/// extern crate dylib source: "/path/to/foo.so"
/// ```
declare_clippy_lint! {
    pub DEEP_CODE_INSPECTION,
    internal_warn,
    "helper to dump info about code"
}

pub struct Pass;

impl LintPass for Pass {
    fn get_lints(&self) -> LintArray {
        lint_array!(DEEP_CODE_INSPECTION)
    }
}

impl<'a, 'tcx> LateLintPass<'a, 'tcx> for Pass {
    fn check_item(&mut self, cx: &LateContext<'a, 'tcx>, item: &'tcx hir::Item) {
        if !has_attr(&item.attrs) {
            return;
        }
        print_item(cx, item);
    }

    fn check_impl_item(&mut self, cx: &LateContext<'a, 'tcx>, item: &'tcx hir::ImplItem) {
        if !has_attr(&item.attrs) {
            return;
        }
        println!("impl item `{}`", item.ident.name);
        match item.vis.node {
            hir::VisibilityKind::Public => println!("public"),
            hir::VisibilityKind::Crate(_) => println!("visible crate wide"),
            hir::VisibilityKind::Restricted { ref path, .. } => println!(
                "visible in module `{}`",
                print::to_string(print::NO_ANN, |s| s.print_path(path, false))
            ),
            hir::VisibilityKind::Inherited => println!("visibility inherited from outer item"),
        }
        if item.defaultness.is_default() {
            println!("default");
        }
        match item.node {
            hir::ImplItemKind::Const(_, body_id) => {
                println!("associated constant");
                print_expr(cx, &cx.tcx.hir().body(body_id).value, 1);
            },
            hir::ImplItemKind::Method(..) => println!("method"),
            hir::ImplItemKind::Type(_) => println!("associated type"),
            hir::ImplItemKind::Existential(_) => println!("existential type"),
        }
    }
    // fn check_trait_item(&mut self, cx: &LateContext<'a, 'tcx>, item: &'tcx
    // hir::TraitItem) {
    // if !has_attr(&item.attrs) {
    // return;
    // }
    // }
    //
    // fn check_variant(&mut self, cx: &LateContext<'a, 'tcx>, var: &'tcx
    // hir::Variant, _:
    // &hir::Generics) {
    // if !has_attr(&var.node.attrs) {
    // return;
    // }
    // }
    //
    // fn check_struct_field(&mut self, cx: &LateContext<'a, 'tcx>, field: &'tcx
    // hir::StructField) {
    // if !has_attr(&field.attrs) {
    // return;
    // }
    // }
    //

    fn check_expr(&mut self, cx: &LateContext<'a, 'tcx>, expr: &'tcx hir::Expr) {
        if !has_attr(&expr.attrs) {
            return;
        }
        print_expr(cx, expr, 0);
    }

    fn check_arm(&mut self, cx: &LateContext<'a, 'tcx>, arm: &'tcx hir::Arm) {
        if !has_attr(&arm.attrs) {
            return;
        }
        for pat in &arm.pats {
            print_pat(cx, pat, 1);
        }
        if let Some(ref guard) = arm.guard {
            println!("guard:");
            print_guard(cx, guard, 1);
        }
        println!("body:");
        print_expr(cx, &arm.body, 1);
    }

    fn check_stmt(&mut self, cx: &LateContext<'a, 'tcx>, stmt: &'tcx hir::Stmt) {
        if !has_attr(stmt.node.attrs()) {
            return;
        }
        match stmt.node {
            hir::StmtKind::Decl(ref decl, _) => print_decl(cx, decl),
            hir::StmtKind::Expr(ref e, _) | hir::StmtKind::Semi(ref e, _) => print_expr(cx, e, 0),
        }
    }
    // fn check_foreign_item(&mut self, cx: &LateContext<'a, 'tcx>, item: &'tcx
    // hir::ForeignItem) {
    // if !has_attr(&item.attrs) {
    // return;
    // }
    // }
    //
}

fn has_attr(attrs: &[Attribute]) -> bool {
    get_attr(attrs, "dump").count() > 0
}

fn print_decl(cx: &LateContext<'_, '_>, decl: &hir::Decl) {
    match decl.node {
        hir::DeclKind::Local(ref local) => {
            println!("local variable of type {}", cx.tables.node_id_to_type(local.hir_id));
            println!("pattern:");
            print_pat(cx, &local.pat, 0);
            if let Some(ref e) = local.init {
                println!("init expression:");
                print_expr(cx, e, 0);
            }
        },
        hir::DeclKind::Item(_) => println!("item decl"),
    }
}

#[allow(clippy::similar_names)]
fn print_expr(cx: &LateContext<'_, '_>, expr: &hir::Expr, indent: usize) {
    let ind = "  ".repeat(indent);
    println!("{}+", ind);
    println!("{}ty: {}", ind, cx.tables.expr_ty(expr));
    println!("{}adjustments: {:?}", ind, cx.tables.adjustments().get(expr.hir_id));
    match expr.node {
        hir::ExprKind::Box(ref e) => {
            println!("{}Box", ind);
            print_expr(cx, e, indent + 1);
        },
        hir::ExprKind::Array(ref v) => {
            println!("{}Array", ind);
            for e in v {
                print_expr(cx, e, indent + 1);
            }
        },
        hir::ExprKind::Call(ref func, ref args) => {
            println!("{}Call", ind);
            println!("{}function:", ind);
            print_expr(cx, func, indent + 1);
            println!("{}arguments:", ind);
            for arg in args {
                print_expr(cx, arg, indent + 1);
            }
        },
        hir::ExprKind::MethodCall(ref path, _, ref args) => {
            println!("{}MethodCall", ind);
            println!("{}method name: {}", ind, path.ident.name);
            for arg in args {
                print_expr(cx, arg, indent + 1);
            }
        },
        hir::ExprKind::Tup(ref v) => {
            println!("{}Tup", ind);
            for e in v {
                print_expr(cx, e, indent + 1);
            }
        },
        hir::ExprKind::Binary(op, ref lhs, ref rhs) => {
            println!("{}Binary", ind);
            println!("{}op: {:?}", ind, op.node);
            println!("{}lhs:", ind);
            print_expr(cx, lhs, indent + 1);
            println!("{}rhs:", ind);
            print_expr(cx, rhs, indent + 1);
        },
        hir::ExprKind::Unary(op, ref inner) => {
            println!("{}Unary", ind);
            println!("{}op: {:?}", ind, op);
            print_expr(cx, inner, indent + 1);
        },
        hir::ExprKind::Lit(ref lit) => {
            println!("{}Lit", ind);
            println!("{}{:?}", ind, lit);
        },
        hir::ExprKind::Cast(ref e, ref target) => {
            println!("{}Cast", ind);
            print_expr(cx, e, indent + 1);
            println!("{}target type: {:?}", ind, target);
        },
        hir::ExprKind::Type(ref e, ref target) => {
            println!("{}Type", ind);
            print_expr(cx, e, indent + 1);
            println!("{}target type: {:?}", ind, target);
        },
        hir::ExprKind::If(ref e, _, ref els) => {
            println!("{}If", ind);
            println!("{}condition:", ind);
            print_expr(cx, e, indent + 1);
            if let Some(ref els) = *els {
                println!("{}else:", ind);
                print_expr(cx, els, indent + 1);
            }
        },
        hir::ExprKind::While(ref cond, _, _) => {
            println!("{}While", ind);
            println!("{}condition:", ind);
            print_expr(cx, cond, indent + 1);
        },
        hir::ExprKind::Loop(..) => {
            println!("{}Loop", ind);
        },
        hir::ExprKind::Match(ref cond, _, ref source) => {
            println!("{}Match", ind);
            println!("{}condition:", ind);
            print_expr(cx, cond, indent + 1);
            println!("{}source: {:?}", ind, source);
        },
        hir::ExprKind::Closure(ref clause, _, _, _, _) => {
            println!("{}Closure", ind);
            println!("{}clause: {:?}", ind, clause);
        },
        hir::ExprKind::Yield(ref sub) => {
            println!("{}Yield", ind);
            print_expr(cx, sub, indent + 1);
        },
        hir::ExprKind::Block(_, _) => {
            println!("{}Block", ind);
        },
        hir::ExprKind::Assign(ref lhs, ref rhs) => {
            println!("{}Assign", ind);
            println!("{}lhs:", ind);
            print_expr(cx, lhs, indent + 1);
            println!("{}rhs:", ind);
            print_expr(cx, rhs, indent + 1);
        },
        hir::ExprKind::AssignOp(ref binop, ref lhs, ref rhs) => {
            println!("{}AssignOp", ind);
            println!("{}op: {:?}", ind, binop.node);
            println!("{}lhs:", ind);
            print_expr(cx, lhs, indent + 1);
            println!("{}rhs:", ind);
            print_expr(cx, rhs, indent + 1);
        },
        hir::ExprKind::Field(ref e, ident) => {
            println!("{}Field", ind);
            println!("{}field name: {}", ind, ident.name);
            println!("{}struct expr:", ind);
            print_expr(cx, e, indent + 1);
        },
        hir::ExprKind::Index(ref arr, ref idx) => {
            println!("{}Index", ind);
            println!("{}array expr:", ind);
            print_expr(cx, arr, indent + 1);
            println!("{}index expr:", ind);
            print_expr(cx, idx, indent + 1);
        },
        hir::ExprKind::Path(hir::QPath::Resolved(ref ty, ref path)) => {
            println!("{}Resolved Path, {:?}", ind, ty);
            println!("{}path: {:?}", ind, path);
        },
        hir::ExprKind::Path(hir::QPath::TypeRelative(ref ty, ref seg)) => {
            println!("{}Relative Path, {:?}", ind, ty);
            println!("{}seg: {:?}", ind, seg);
        },
        hir::ExprKind::AddrOf(ref muta, ref e) => {
            println!("{}AddrOf", ind);
            println!("mutability: {:?}", muta);
            print_expr(cx, e, indent + 1);
        },
        hir::ExprKind::Break(_, ref e) => {
            println!("{}Break", ind);
            if let Some(ref e) = *e {
                print_expr(cx, e, indent + 1);
            }
        },
        hir::ExprKind::Continue(_) => println!("{}Again", ind),
        hir::ExprKind::Ret(ref e) => {
            println!("{}Ret", ind);
            if let Some(ref e) = *e {
                print_expr(cx, e, indent + 1);
            }
        },
        hir::ExprKind::InlineAsm(_, ref input, ref output) => {
            println!("{}InlineAsm", ind);
            println!("{}inputs:", ind);
            for e in input {
                print_expr(cx, e, indent + 1);
            }
            println!("{}outputs:", ind);
            for e in output {
                print_expr(cx, e, indent + 1);
            }
        },
        hir::ExprKind::Struct(ref path, ref fields, ref base) => {
            println!("{}Struct", ind);
            println!("{}path: {:?}", ind, path);
            for field in fields {
                println!("{}field \"{}\":", ind, field.ident.name);
                print_expr(cx, &field.expr, indent + 1);
            }
            if let Some(ref base) = *base {
                println!("{}base:", ind);
                print_expr(cx, base, indent + 1);
            }
        },
        hir::ExprKind::Repeat(ref val, ref anon_const) => {
            println!("{}Repeat", ind);
            println!("{}value:", ind);
            print_expr(cx, val, indent + 1);
            println!("{}repeat count:", ind);
            print_expr(cx, &cx.tcx.hir().body(anon_const.body).value, indent + 1);
        },
        hir::ExprKind::Err => {
            println!("{}Err", ind);
        },
    }
}

fn print_item(cx: &LateContext<'_, '_>, item: &hir::Item) {
    let did = cx.tcx.hir().local_def_id(item.id);
    println!("item `{}`", item.ident.name);
    match item.vis.node {
        hir::VisibilityKind::Public => println!("public"),
        hir::VisibilityKind::Crate(_) => println!("visible crate wide"),
        hir::VisibilityKind::Restricted { ref path, .. } => println!(
            "visible in module `{}`",
            print::to_string(print::NO_ANN, |s| s.print_path(path, false))
        ),
        hir::VisibilityKind::Inherited => println!("visibility inherited from outer item"),
    }
    match item.node {
        hir::ItemKind::ExternCrate(ref _renamed_from) => {
            let def_id = cx.tcx.hir().local_def_id(item.id);
            if let Some(crate_id) = cx.tcx.extern_mod_stmt_cnum(def_id) {
                let source = cx.tcx.used_crate_source(crate_id);
                if let Some(ref src) = source.dylib {
                    println!("extern crate dylib source: {:?}", src.0);
                }
                if let Some(ref src) = source.rlib {
                    println!("extern crate rlib source: {:?}", src.0);
                }
            } else {
                println!("weird extern crate without a crate id");
            }
        },
        hir::ItemKind::Use(ref path, ref kind) => println!("{:?}, {:?}", path, kind),
        hir::ItemKind::Static(..) => println!("static item of type {:#?}", cx.tcx.type_of(did)),
        hir::ItemKind::Const(..) => println!("const item of type {:#?}", cx.tcx.type_of(did)),
        hir::ItemKind::Fn(..) => {
            let item_ty = cx.tcx.type_of(did);
            println!("function of type {:#?}", item_ty);
        },
        hir::ItemKind::Mod(..) => println!("module"),
        hir::ItemKind::ForeignMod(ref fm) => println!("foreign module with abi: {}", fm.abi),
        hir::ItemKind::GlobalAsm(ref asm) => println!("global asm: {:?}", asm),
        hir::ItemKind::Ty(..) => {
            println!("type alias for {:?}", cx.tcx.type_of(did));
        },
        hir::ItemKind::Existential(..) => {
            println!("existential type with real type {:?}", cx.tcx.type_of(did));
        },
        hir::ItemKind::Enum(..) => {
            println!("enum definition of type {:?}", cx.tcx.type_of(did));
        },
        hir::ItemKind::Struct(..) => {
            println!("struct definition of type {:?}", cx.tcx.type_of(did));
        },
        hir::ItemKind::Union(..) => {
            println!("union definition of type {:?}", cx.tcx.type_of(did));
        },
        hir::ItemKind::Trait(..) => {
            println!("trait decl");
            if cx.tcx.trait_is_auto(did) {
                println!("trait is auto");
            } else {
                println!("trait is not auto");
            }
        },
        hir::ItemKind::TraitAlias(..) => {
            println!("trait alias");
        },
        hir::ItemKind::Impl(_, _, _, _, Some(ref _trait_ref), _, _) => {
            println!("trait impl");
        },
        hir::ItemKind::Impl(_, _, _, _, None, _, _) => {
            println!("impl");
        },
    }
}

#[allow(clippy::similar_names)]
fn print_pat(cx: &LateContext<'_, '_>, pat: &hir::Pat, indent: usize) {
    let ind = "  ".repeat(indent);
    println!("{}+", ind);
    match pat.node {
        hir::PatKind::Wild => println!("{}Wild", ind),
        hir::PatKind::Binding(ref mode, _, ident, ref inner) => {
            println!("{}Binding", ind);
            println!("{}mode: {:?}", ind, mode);
            println!("{}name: {}", ind, ident.name);
            if let Some(ref inner) = *inner {
                println!("{}inner:", ind);
                print_pat(cx, inner, indent + 1);
            }
        },
        hir::PatKind::Struct(ref path, ref fields, ignore) => {
            println!("{}Struct", ind);
            println!(
                "{}name: {}",
                ind,
                print::to_string(print::NO_ANN, |s| s.print_qpath(path, false))
            );
            println!("{}ignore leftover fields: {}", ind, ignore);
            println!("{}fields:", ind);
            for field in fields {
                println!("{}  field name: {}", ind, field.node.ident.name);
                if field.node.is_shorthand {
                    println!("{}  in shorthand notation", ind);
                }
                print_pat(cx, &field.node.pat, indent + 1);
            }
        },
        hir::PatKind::TupleStruct(ref path, ref fields, opt_dots_position) => {
            println!("{}TupleStruct", ind);
            println!(
                "{}path: {}",
                ind,
                print::to_string(print::NO_ANN, |s| s.print_qpath(path, false))
            );
            if let Some(dot_position) = opt_dots_position {
                println!("{}dot position: {}", ind, dot_position);
            }
            for field in fields {
                print_pat(cx, field, indent + 1);
            }
        },
        hir::PatKind::Path(hir::QPath::Resolved(ref ty, ref path)) => {
            println!("{}Resolved Path, {:?}", ind, ty);
            println!("{}path: {:?}", ind, path);
        },
        hir::PatKind::Path(hir::QPath::TypeRelative(ref ty, ref seg)) => {
            println!("{}Relative Path, {:?}", ind, ty);
            println!("{}seg: {:?}", ind, seg);
        },
        hir::PatKind::Tuple(ref pats, opt_dots_position) => {
            println!("{}Tuple", ind);
            if let Some(dot_position) = opt_dots_position {
                println!("{}dot position: {}", ind, dot_position);
            }
            for field in pats {
                print_pat(cx, field, indent + 1);
            }
        },
        hir::PatKind::Box(ref inner) => {
            println!("{}Box", ind);
            print_pat(cx, inner, indent + 1);
        },
        hir::PatKind::Ref(ref inner, ref muta) => {
            println!("{}Ref", ind);
            println!("{}mutability: {:?}", ind, muta);
            print_pat(cx, inner, indent + 1);
        },
        hir::PatKind::Lit(ref e) => {
            println!("{}Lit", ind);
            print_expr(cx, e, indent + 1);
        },
        hir::PatKind::Range(ref l, ref r, ref range_end) => {
            println!("{}Range", ind);
            print_expr(cx, l, indent + 1);
            print_expr(cx, r, indent + 1);
            match *range_end {
                hir::RangeEnd::Included => println!("{} end included", ind),
                hir::RangeEnd::Excluded => println!("{} end excluded", ind),
            }
        },
        hir::PatKind::Slice(ref first_pats, ref range, ref last_pats) => {
            println!("{}Slice [a, b, ..i, y, z]", ind);
            println!("[a, b]:");
            for pat in first_pats {
                print_pat(cx, pat, indent + 1);
            }
            println!("i:");
            if let Some(ref pat) = *range {
                print_pat(cx, pat, indent + 1);
            }
            println!("[y, z]:");
            for pat in last_pats {
                print_pat(cx, pat, indent + 1);
            }
        },
    }
}

fn print_guard(cx: &LateContext<'_, '_>, guard: &hir::Guard, indent: usize) {
    let ind = "  ".repeat(indent);
    println!("{}+", ind);
    match guard {
        hir::Guard::If(expr) => {
            println!("{}If", ind);
            print_expr(cx, expr, indent + 1);
        },
    }
}
