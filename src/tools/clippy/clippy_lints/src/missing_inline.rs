use crate::utils::span_lint;
use rustc::hir;
use rustc::lint::{LateContext, LateLintPass, LintArray, LintPass};
use rustc::{declare_tool_lint, lint_array};
use syntax::ast;
use syntax::source_map::Span;

/// **What it does:** it lints if an exported function, method, trait method with default impl,
/// or trait method impl is not `#[inline]`.
///
/// **Why is this bad?** In general, it is not. Functions can be inlined across
/// crates when that's profitable as long as any form of LTO is used. When LTO is disabled,
/// functions that are not `#[inline]` cannot be inlined across crates. Certain types of crates
/// might intend for most of the methods in their public API to be able to be inlined across
/// crates even when LTO is disabled. For these types of crates, enabling this lint might make
/// sense. It allows the crate to require all exported methods to be `#[inline]` by default, and
/// then opt out for specific methods where this might not make sense.
///
/// **Known problems:** None.
///
/// **Example:**
/// ```rust
/// pub fn foo() {} // missing #[inline]
/// fn ok() {} // ok
/// #[inline] pub fn bar() {} // ok
/// #[inline(always)] pub fn baz() {} // ok
///
/// pub trait Bar {
///   fn bar(); // ok
///   fn def_bar() {} // missing #[inline]
/// }
///
/// struct Baz;
/// impl Baz {
///    fn priv() {} // ok
/// }
///
/// impl Bar for Baz {
///   fn bar() {} // ok - Baz is not exported
/// }
///
/// pub struct PubBaz;
/// impl PubBaz {
///    fn priv() {} // ok
///    pub not_ptriv() {} // missing #[inline]
/// }
///
/// impl Bar for PubBaz {
///    fn bar() {} // missing #[inline]
///    fn def_bar() {} // missing #[inline]
/// }
/// ```
declare_clippy_lint! {
    pub MISSING_INLINE_IN_PUBLIC_ITEMS,
    restriction,
    "detects missing #[inline] attribute for public callables (functions, trait methods, methods...)"
}

pub struct MissingInline;

fn check_missing_inline_attrs(cx: &LateContext<'_, '_>, attrs: &[ast::Attribute], sp: Span, desc: &'static str) {
    let has_inline = attrs.iter().any(|a| a.name() == "inline");
    if !has_inline {
        span_lint(
            cx,
            MISSING_INLINE_IN_PUBLIC_ITEMS,
            sp,
            &format!("missing `#[inline]` for {}", desc),
        );
    }
}

fn is_executable<'a, 'tcx>(cx: &LateContext<'a, 'tcx>) -> bool {
    use rustc::session::config::CrateType;

    cx.tcx.sess.crate_types.get().iter().any(|t: &CrateType| match t {
        CrateType::Executable => true,
        _ => false,
    })
}

impl LintPass for MissingInline {
    fn get_lints(&self) -> LintArray {
        lint_array![MISSING_INLINE_IN_PUBLIC_ITEMS]
    }
}

impl<'a, 'tcx> LateLintPass<'a, 'tcx> for MissingInline {
    fn check_item(&mut self, cx: &LateContext<'a, 'tcx>, it: &'tcx hir::Item) {
        if is_executable(cx) {
            return;
        }

        if !cx.access_levels.is_exported(it.id) {
            return;
        }
        match it.node {
            hir::ItemKind::Fn(..) => {
                let desc = "a function";
                check_missing_inline_attrs(cx, &it.attrs, it.span, desc);
            },
            hir::ItemKind::Trait(ref _is_auto, ref _unsafe, ref _generics, ref _bounds, ref trait_items) => {
                // note: we need to check if the trait is exported so we can't use
                // `LateLintPass::check_trait_item` here.
                for tit in trait_items {
                    let tit_ = cx.tcx.hir().trait_item(tit.id);
                    match tit_.node {
                        hir::TraitItemKind::Const(..) | hir::TraitItemKind::Type(..) => {},
                        hir::TraitItemKind::Method(..) => {
                            if tit.defaultness.has_value() {
                                // trait method with default body needs inline in case
                                // an impl is not provided
                                let desc = "a default trait method";
                                let item = cx.tcx.hir().expect_trait_item(tit.id.node_id);
                                check_missing_inline_attrs(cx, &item.attrs, item.span, desc);
                            }
                        },
                    }
                }
            },
            hir::ItemKind::Const(..)
            | hir::ItemKind::Enum(..)
            | hir::ItemKind::Mod(..)
            | hir::ItemKind::Static(..)
            | hir::ItemKind::Struct(..)
            | hir::ItemKind::TraitAlias(..)
            | hir::ItemKind::GlobalAsm(..)
            | hir::ItemKind::Ty(..)
            | hir::ItemKind::Union(..)
            | hir::ItemKind::Existential(..)
            | hir::ItemKind::ExternCrate(..)
            | hir::ItemKind::ForeignMod(..)
            | hir::ItemKind::Impl(..)
            | hir::ItemKind::Use(..) => {},
        };
    }

    fn check_impl_item(&mut self, cx: &LateContext<'a, 'tcx>, impl_item: &'tcx hir::ImplItem) {
        use rustc::ty::{ImplContainer, TraitContainer};
        if is_executable(cx) {
            return;
        }

        // If the item being implemented is not exported, then we don't need #[inline]
        if !cx.access_levels.is_exported(impl_item.id) {
            return;
        }

        let desc = match impl_item.node {
            hir::ImplItemKind::Method(..) => "a method",
            hir::ImplItemKind::Const(..) | hir::ImplItemKind::Type(_) | hir::ImplItemKind::Existential(_) => return,
        };

        let def_id = cx.tcx.hir().local_def_id(impl_item.id);
        let trait_def_id = match cx.tcx.associated_item(def_id).container {
            TraitContainer(cid) => Some(cid),
            ImplContainer(cid) => cx.tcx.impl_trait_ref(cid).map(|t| t.def_id),
        };

        if let Some(trait_def_id) = trait_def_id {
            if let Some(n) = cx.tcx.hir().as_local_node_id(trait_def_id) {
                if !cx.access_levels.is_exported(n) {
                    // If a trait is being implemented for an item, and the
                    // trait is not exported, we don't need #[inline]
                    return;
                }
            }
        }

        check_missing_inline_attrs(cx, &impl_item.attrs, impl_item.span, desc);
    }
}
