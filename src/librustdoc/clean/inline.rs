// Copyright 2012-2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Support for inlining external documentation into the current AST.

use std::collections::BTreeMap;
use std::io;
use std::iter::once;
use std::rc::Rc;

use syntax::ast;
use rustc::hir;

use rustc::hir::def::{Def, CtorKind};
use rustc::hir::def_id::DefId;
use rustc::ty;
use rustc::util::nodemap::FxHashSet;

use core::{DocContext, DocAccessLevels};
use doctree;
use clean::{self, GetDefId};

use super::Clean;

/// Attempt to inline a definition into this AST.
///
/// This function will fetch the definition specified, and if it is
/// from another crate it will attempt to inline the documentation
/// from the other crate into this crate.
///
/// This is primarily used for `pub use` statements which are, in general,
/// implementation details. Inlining the documentation should help provide a
/// better experience when reading the documentation in this use case.
///
/// The returned value is `None` if the definition could not be inlined,
/// and `Some` of a vector of items if it was successfully expanded.
pub fn try_inline(cx: &DocContext, def: Def, name: ast::Name)
                  -> Option<Vec<clean::Item>> {
    if def == Def::Err { return None }
    let did = def.def_id();
    if did.is_local() { return None }
    let mut ret = Vec::new();
    let inner = match def {
        Def::Trait(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Trait);
            ret.extend(build_impls(cx, did));
            clean::TraitItem(build_external_trait(cx, did))
        }
        Def::Fn(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Function);
            clean::FunctionItem(build_external_function(cx, did))
        }
        Def::Struct(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Struct);
            ret.extend(build_impls(cx, did));
            clean::StructItem(build_struct(cx, did))
        }
        Def::Union(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Union);
            ret.extend(build_impls(cx, did));
            clean::UnionItem(build_union(cx, did))
        }
        Def::TyAlias(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Typedef);
            ret.extend(build_impls(cx, did));
            clean::TypedefItem(build_type_alias(cx, did), false)
        }
        Def::Enum(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Enum);
            ret.extend(build_impls(cx, did));
            clean::EnumItem(build_enum(cx, did))
        }
        Def::TyForeign(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Foreign);
            ret.extend(build_impls(cx, did));
            clean::ForeignTypeItem
        }
        // Never inline enum variants but leave them shown as reexports.
        Def::Variant(..) => return None,
        // Assume that enum variants and struct types are reexported next to
        // their constructors.
        Def::VariantCtor(..) |
        Def::StructCtor(..) => return Some(Vec::new()),
        Def::Mod(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Module);
            clean::ModuleItem(build_module(cx, did))
        }
        Def::Static(did, mtbl) => {
            record_extern_fqn(cx, did, clean::TypeKind::Static);
            clean::StaticItem(build_static(cx, did, mtbl))
        }
        Def::Const(did) => {
            record_extern_fqn(cx, did, clean::TypeKind::Const);
            clean::ConstantItem(build_const(cx, did))
        }
        _ => return None,
    };
    cx.renderinfo.borrow_mut().inlined.insert(did);
    ret.push(clean::Item {
        source: cx.tcx.def_span(did).clean(cx),
        name: Some(name.clean(cx)),
        attrs: load_attrs(cx, did),
        inner,
        visibility: Some(clean::Public),
        stability: cx.tcx.lookup_stability(did).clean(cx),
        deprecation: cx.tcx.lookup_deprecation(did).clean(cx),
        def_id: did,
    });
    Some(ret)
}

pub fn load_attrs(cx: &DocContext, did: DefId) -> clean::Attributes {
    cx.tcx.get_attrs(did).clean(cx)
}

/// Record an external fully qualified name in the external_paths cache.
///
/// These names are used later on by HTML rendering to generate things like
/// source links back to the original item.
pub fn record_extern_fqn(cx: &DocContext, did: DefId, kind: clean::TypeKind) {
    let crate_name = cx.tcx.crate_name(did.krate).to_string();
    let relative = cx.tcx.def_path(did).data.into_iter().filter_map(|elem| {
        // extern blocks have an empty name
        let s = elem.data.to_string();
        if !s.is_empty() {
            Some(s)
        } else {
            None
        }
    });
    let fqn = once(crate_name).chain(relative).collect();
    cx.renderinfo.borrow_mut().external_paths.insert(did, (fqn, kind));
}

pub fn build_external_trait(cx: &DocContext, did: DefId) -> clean::Trait {
    let trait_items = cx.tcx.associated_items(did).map(|item| item.clean(cx)).collect();
    let predicates = cx.tcx.predicates_of(did);
    let generics = (cx.tcx.generics_of(did), &predicates).clean(cx);
    let generics = filter_non_trait_generics(did, generics);
    let (generics, supertrait_bounds) = separate_supertrait_bounds(generics);
    clean::Trait {
        unsafety: cx.tcx.trait_def(did).unsafety,
        generics,
        items: trait_items,
        bounds: supertrait_bounds,
    }
}

fn build_external_function(cx: &DocContext, did: DefId) -> clean::Function {
    let sig = cx.tcx.fn_sig(did);

    let constness = if cx.tcx.is_const_fn(did) {
        hir::Constness::Const
    } else {
        hir::Constness::NotConst
    };

    let predicates = cx.tcx.predicates_of(did);
    clean::Function {
        decl: (did, sig).clean(cx),
        generics: (cx.tcx.generics_of(did), &predicates).clean(cx),
        unsafety: sig.unsafety(),
        constness,
        abi: sig.abi(),
    }
}

fn build_enum(cx: &DocContext, did: DefId) -> clean::Enum {
    let predicates = cx.tcx.predicates_of(did);

    clean::Enum {
        generics: (cx.tcx.generics_of(did), &predicates).clean(cx),
        variants_stripped: false,
        variants: cx.tcx.adt_def(did).variants.clean(cx),
    }
}

fn build_struct(cx: &DocContext, did: DefId) -> clean::Struct {
    let predicates = cx.tcx.predicates_of(did);
    let variant = cx.tcx.adt_def(did).struct_variant();

    clean::Struct {
        struct_type: match variant.ctor_kind {
            CtorKind::Fictive => doctree::Plain,
            CtorKind::Fn => doctree::Tuple,
            CtorKind::Const => doctree::Unit,
        },
        generics: (cx.tcx.generics_of(did), &predicates).clean(cx),
        fields: variant.fields.clean(cx),
        fields_stripped: false,
    }
}

fn build_union(cx: &DocContext, did: DefId) -> clean::Union {
    let predicates = cx.tcx.predicates_of(did);
    let variant = cx.tcx.adt_def(did).struct_variant();

    clean::Union {
        struct_type: doctree::Plain,
        generics: (cx.tcx.generics_of(did), &predicates).clean(cx),
        fields: variant.fields.clean(cx),
        fields_stripped: false,
    }
}

fn build_type_alias(cx: &DocContext, did: DefId) -> clean::Typedef {
    let predicates = cx.tcx.predicates_of(did);

    clean::Typedef {
        type_: cx.tcx.type_of(did).clean(cx),
        generics: (cx.tcx.generics_of(did), &predicates).clean(cx),
    }
}

pub fn build_impls(cx: &DocContext, did: DefId) -> Vec<clean::Item> {
    let tcx = cx.tcx;
    let mut impls = Vec::new();

    for &did in tcx.inherent_impls(did).iter() {
        build_impl(cx, did, &mut impls);
    }

    // If this is the first time we've inlined something from another crate, then
    // we inline *all* impls from all the crates into this crate. Note that there's
    // currently no way for us to filter this based on type, and we likely need
    // many impls for a variety of reasons.
    //
    // Primarily, the impls will be used to populate the documentation for this
    // type being inlined, but impls can also be used when generating
    // documentation for primitives (no way to find those specifically).
    if cx.populated_all_crate_impls.get() {
        return impls;
    }

    cx.populated_all_crate_impls.set(true);

    for &cnum in tcx.crates().iter() {
        for did in tcx.all_trait_implementations(cnum).iter() {
            build_impl(cx, *did, &mut impls);
        }
    }

    // Also try to inline primitive impls from other crates.
    let lang_items = tcx.lang_items();
    let primitive_impls = [
        lang_items.isize_impl(),
        lang_items.i8_impl(),
        lang_items.i16_impl(),
        lang_items.i32_impl(),
        lang_items.i64_impl(),
        lang_items.i128_impl(),
        lang_items.usize_impl(),
        lang_items.u8_impl(),
        lang_items.u16_impl(),
        lang_items.u32_impl(),
        lang_items.u64_impl(),
        lang_items.u128_impl(),
        lang_items.f32_impl(),
        lang_items.f64_impl(),
        lang_items.char_impl(),
        lang_items.str_impl(),
        lang_items.slice_impl(),
        lang_items.const_ptr_impl(),
        lang_items.mut_ptr_impl(),
    ];

    for def_id in primitive_impls.iter().filter_map(|&def_id| def_id) {
        if !def_id.is_local() {
            build_impl(cx, def_id, &mut impls);
        }
    }

    impls
}

pub fn build_impl(cx: &DocContext, did: DefId, ret: &mut Vec<clean::Item>) {
    if !cx.renderinfo.borrow_mut().inlined.insert(did) {
        return
    }

    let attrs = load_attrs(cx, did);
    let tcx = cx.tcx;
    let associated_trait = tcx.impl_trait_ref(did);

    // Only inline impl if the implemented trait is
    // reachable in rustdoc generated documentation
    if let Some(traitref) = associated_trait {
        if !cx.access_levels.borrow().is_doc_reachable(traitref.def_id) {
            return
        }
    }

    // If this is an auto impl, then bail out early here
    if tcx.is_auto_impl(did) {
        return ret.push(clean::Item {
            inner: clean::AutoImplItem(clean::AutoImpl {
                // FIXME: this should be decoded
                unsafety: hir::Unsafety::Normal,
                trait_: match associated_trait.as_ref().unwrap().clean(cx) {
                    clean::TraitBound(polyt, _) => polyt.trait_,
                    clean::RegionBound(..) => unreachable!(),
                },
            }),
            source: tcx.def_span(did).clean(cx),
            name: None,
            attrs,
            visibility: Some(clean::Inherited),
            stability: tcx.lookup_stability(did).clean(cx),
            deprecation: tcx.lookup_deprecation(did).clean(cx),
            def_id: did,
        });
    }

    let for_ = tcx.type_of(did).clean(cx);

    // Only inline impl if the implementing type is
    // reachable in rustdoc generated documentation
    if let Some(did) = for_.def_id() {
        if !cx.access_levels.borrow().is_doc_reachable(did) {
            return
        }
    }

    let predicates = tcx.predicates_of(did);
    let trait_items = tcx.associated_items(did).filter_map(|item| {
        match item.kind {
            ty::AssociatedKind::Const => {
                let default = if item.defaultness.has_value() {
                    Some(print_inlined_const(cx, item.def_id))
                } else {
                    None
                };
                Some(clean::Item {
                    name: Some(item.name.clean(cx)),
                    inner: clean::AssociatedConstItem(
                        tcx.type_of(item.def_id).clean(cx),
                        default,
                    ),
                    source: tcx.def_span(item.def_id).clean(cx),
                    attrs: clean::Attributes::default(),
                    visibility: None,
                    stability: tcx.lookup_stability(item.def_id).clean(cx),
                    deprecation: tcx.lookup_deprecation(item.def_id).clean(cx),
                    def_id: item.def_id
                })
            }
            ty::AssociatedKind::Method => {
                if item.vis != ty::Visibility::Public && associated_trait.is_none() {
                    return None
                }
                let mut cleaned = item.clean(cx);
                cleaned.inner = match cleaned.inner.clone() {
                    clean::TyMethodItem(clean::TyMethod {
                        unsafety, decl, generics, abi
                    }) => {
                        let constness = if tcx.is_const_fn(item.def_id) {
                            hir::Constness::Const
                        } else {
                            hir::Constness::NotConst
                        };

                        clean::MethodItem(clean::Method {
                            unsafety,
                            constness,
                            decl,
                            generics,
                            abi,
                        })
                    }
                    ref r => panic!("not a tymethod: {:?}", r),
                };
                Some(cleaned)
            }
            ty::AssociatedKind::Type => {
                let typedef = clean::Typedef {
                    type_: tcx.type_of(item.def_id).clean(cx),
                    generics: clean::Generics {
                        lifetimes: vec![],
                        type_params: vec![],
                        where_predicates: vec![]
                    }
                };
                Some(clean::Item {
                    name: Some(item.name.clean(cx)),
                    inner: clean::TypedefItem(typedef, true),
                    source: tcx.def_span(item.def_id).clean(cx),
                    attrs: clean::Attributes::default(),
                    visibility: None,
                    stability: tcx.lookup_stability(item.def_id).clean(cx),
                    deprecation: tcx.lookup_deprecation(item.def_id).clean(cx),
                    def_id: item.def_id
                })
            }
        }
    }).collect::<Vec<_>>();
    let polarity = tcx.impl_polarity(did);
    let trait_ = associated_trait.clean(cx).map(|bound| {
        match bound {
            clean::TraitBound(polyt, _) => polyt.trait_,
            clean::RegionBound(..) => unreachable!(),
        }
    });
    if trait_.def_id() == tcx.lang_items().deref_trait() {
        super::build_deref_target_impls(cx, &trait_items, ret);
    }

    let provided = trait_.def_id().map(|did| {
        tcx.provided_trait_methods(did)
            .into_iter()
            .map(|meth| meth.name.to_string())
            .collect()
    }).unwrap_or(FxHashSet());

    ret.push(clean::Item {
        inner: clean::ImplItem(clean::Impl {
            unsafety: hir::Unsafety::Normal, // FIXME: this should be decoded
            provided_trait_methods: provided,
            trait_,
            for_,
            generics: (tcx.generics_of(did), &predicates).clean(cx),
            items: trait_items,
            polarity: Some(polarity.clean(cx)),
        }),
        source: tcx.def_span(did).clean(cx),
        name: None,
        attrs,
        visibility: Some(clean::Inherited),
        stability: tcx.lookup_stability(did).clean(cx),
        deprecation: tcx.lookup_deprecation(did).clean(cx),
        def_id: did,
    });
}

fn build_module(cx: &DocContext, did: DefId) -> clean::Module {
    let mut items = Vec::new();
    fill_in(cx, did, &mut items);
    return clean::Module {
        items,
        is_crate: false,
    };

    fn fill_in(cx: &DocContext, did: DefId, items: &mut Vec<clean::Item>) {
        // If we're reexporting a reexport it may actually reexport something in
        // two namespaces, so the target may be listed twice. Make sure we only
        // visit each node at most once.
        let mut visited = FxHashSet();
        for &item in cx.tcx.item_children(did).iter() {
            let def_id = item.def.def_id();
            if cx.tcx.visibility(def_id) == ty::Visibility::Public {
                if !visited.insert(def_id) { continue }
                if let Some(i) = try_inline(cx, item.def, item.ident.name) {
                    items.extend(i)
                }
            }
        }
    }
}

struct InlinedConst {
    nested_bodies: Rc<BTreeMap<hir::BodyId, hir::Body>>
}

impl hir::print::PpAnn for InlinedConst {
    fn nested(&self, state: &mut hir::print::State, nested: hir::print::Nested)
              -> io::Result<()> {
        if let hir::print::Nested::Body(body) = nested {
            state.print_expr(&self.nested_bodies[&body].value)
        } else {
            Ok(())
        }
    }
}

pub fn print_inlined_const(cx: &DocContext, did: DefId) -> String {
    let body = cx.tcx.extern_const_body(did).body;
    let inlined = InlinedConst {
        nested_bodies: cx.tcx.item_body_nested_bodies(did).nested_bodies
    };
    hir::print::to_string(&inlined, |s| s.print_expr(&body.value))
}

fn build_const(cx: &DocContext, did: DefId) -> clean::Constant {
    clean::Constant {
        type_: cx.tcx.type_of(did).clean(cx),
        expr: print_inlined_const(cx, did)
    }
}

fn build_static(cx: &DocContext, did: DefId, mutable: bool) -> clean::Static {
    clean::Static {
        type_: cx.tcx.type_of(did).clean(cx),
        mutability: if mutable {clean::Mutable} else {clean::Immutable},
        expr: "\n\n\n".to_string(), // trigger the "[definition]" links
    }
}

/// A trait's generics clause actually contains all of the predicates for all of
/// its associated types as well. We specifically move these clauses to the
/// associated types instead when displaying, so when we're generating the
/// generics for the trait itself we need to be sure to remove them.
/// We also need to remove the implied "recursive" Self: Trait bound.
///
/// The inverse of this filtering logic can be found in the `Clean`
/// implementation for `AssociatedType`
fn filter_non_trait_generics(trait_did: DefId, mut g: clean::Generics)
                             -> clean::Generics {
    for pred in &mut g.where_predicates {
        match *pred {
            clean::WherePredicate::BoundPredicate {
                ty: clean::Generic(ref s),
                ref mut bounds
            } if *s == "Self" => {
                bounds.retain(|bound| {
                    match *bound {
                        clean::TyParamBound::TraitBound(clean::PolyTrait {
                            trait_: clean::ResolvedPath { did, .. },
                            ..
                        }, _) => did != trait_did,
                        _ => true
                    }
                });
            }
            _ => {}
        }
    }

    g.where_predicates.retain(|pred| {
        match *pred {
            clean::WherePredicate::BoundPredicate {
                ty: clean::QPath {
                    self_type: box clean::Generic(ref s),
                    trait_: box clean::ResolvedPath { did, .. },
                    name: ref _name,
                }, ref bounds
            } => !(*s == "Self" && did == trait_did) && !bounds.is_empty(),
            _ => true,
        }
    });
    g
}

/// Supertrait bounds for a trait are also listed in the generics coming from
/// the metadata for a crate, so we want to separate those out and create a new
/// list of explicit supertrait bounds to render nicely.
fn separate_supertrait_bounds(mut g: clean::Generics)
                              -> (clean::Generics, Vec<clean::TyParamBound>) {
    let mut ty_bounds = Vec::new();
    g.where_predicates.retain(|pred| {
        match *pred {
            clean::WherePredicate::BoundPredicate {
                ty: clean::Generic(ref s),
                ref bounds
            } if *s == "Self" => {
                ty_bounds.extend(bounds.iter().cloned());
                false
            }
            _ => true,
        }
    });
    (g, ty_bounds)
}
