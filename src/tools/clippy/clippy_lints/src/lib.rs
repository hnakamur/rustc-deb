// error-pattern:cargo-clippy

#![feature(box_syntax)]
#![feature(rustc_private)]
#![feature(slice_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(range_contains)]
#![feature(str_escape)]
#![allow(clippy::missing_docs_in_private_items)]
#![recursion_limit = "256"]
#![warn(rust_2018_idioms, trivial_casts, trivial_numeric_casts)]
#![feature(crate_visibility_modifier)]
#![feature(try_from)]

// FIXME: switch to something more ergonomic here, once available.
// (currently there is no way to opt into sysroot crates w/o `extern crate`)
#[allow(unused_extern_crates)]
extern crate fmt_macros;
#[allow(unused_extern_crates)]
extern crate rustc;
#[allow(unused_extern_crates)]
extern crate rustc_data_structures;
#[allow(unused_extern_crates)]
extern crate rustc_errors;
#[allow(unused_extern_crates)]
extern crate rustc_plugin;
#[allow(unused_extern_crates)]
extern crate rustc_target;
#[allow(unused_extern_crates)]
extern crate rustc_typeck;
#[allow(unused_extern_crates)]
extern crate syntax;
#[allow(unused_extern_crates)]
extern crate syntax_pos;

use toml;

// Currently, categories "style", "correctness", "complexity" and "perf" are enabled by default,
// as said in the README.md of this repository. If this changes, please update README.md.
#[macro_export]
macro_rules! declare_clippy_lint {
    { pub $name:tt, style, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Warn, $description, report_in_external_macro: true }
    };
    { pub $name:tt, correctness, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Deny, $description, report_in_external_macro: true }
    };
    { pub $name:tt, complexity, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Warn, $description, report_in_external_macro: true }
    };
    { pub $name:tt, perf, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Warn, $description, report_in_external_macro: true }
    };
    { pub $name:tt, pedantic, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Allow, $description, report_in_external_macro: true }
    };
    { pub $name:tt, restriction, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Allow, $description, report_in_external_macro: true }
    };
    { pub $name:tt, cargo, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Allow, $description, report_in_external_macro: true }
    };
    { pub $name:tt, nursery, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Allow, $description, report_in_external_macro: true }
    };
    { pub $name:tt, internal, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Allow, $description, report_in_external_macro: true }
    };
    { pub $name:tt, internal_warn, $description:tt } => {
        declare_tool_lint! { pub clippy::$name, Warn, $description, report_in_external_macro: true }
    };
}

mod consts;
#[macro_use]
mod utils;

// begin lints modules, do not remove this comment, it’s used in `update_lints`
pub mod approx_const;
pub mod arithmetic;
pub mod assign_ops;
pub mod attrs;
pub mod bit_mask;
pub mod blacklisted_name;
pub mod block_in_if_condition;
pub mod booleans;
pub mod bytecount;
pub mod cargo_common_metadata;
pub mod collapsible_if;
pub mod const_static_lifetime;
pub mod copies;
pub mod copy_iterator;
pub mod cyclomatic_complexity;
pub mod default_trait_access;
pub mod derive;
pub mod doc;
pub mod double_comparison;
pub mod double_parens;
pub mod drop_forget_ref;
pub mod duration_subsec;
pub mod else_if_without_else;
pub mod empty_enum;
pub mod entry;
pub mod enum_clike;
pub mod enum_glob_use;
pub mod enum_variants;
pub mod eq_op;
pub mod erasing_op;
pub mod escape;
pub mod eta_reduction;
pub mod eval_order_dependence;
pub mod excessive_precision;
pub mod explicit_write;
pub mod fallible_impl_from;
pub mod format;
pub mod formatting;
pub mod functions;
pub mod identity_conversion;
pub mod identity_op;
pub mod if_not_else;
pub mod implicit_return;
pub mod indexing_slicing;
pub mod infallible_destructuring_match;
pub mod infinite_iter;
pub mod inherent_impl;
pub mod inline_fn_without_body;
pub mod int_plus_one;
pub mod invalid_ref;
pub mod items_after_statements;
pub mod large_enum_variant;
pub mod len_zero;
pub mod let_if_seq;
pub mod lifetimes;
pub mod literal_representation;
pub mod loops;
pub mod map_clone;
pub mod map_unit_fn;
pub mod matches;
pub mod mem_discriminant;
pub mod mem_forget;
pub mod mem_replace;
pub mod methods;
pub mod minmax;
pub mod misc;
pub mod misc_early;
pub mod missing_doc;
pub mod missing_inline;
pub mod multiple_crate_versions;
pub mod mut_mut;
pub mod mut_reference;
pub mod mutex_atomic;
pub mod needless_bool;
pub mod needless_borrow;
pub mod needless_borrowed_ref;
pub mod needless_continue;
pub mod needless_pass_by_value;
pub mod needless_update;
pub mod neg_cmp_op_on_partial_ord;
pub mod neg_multiply;
pub mod new_without_default;
pub mod no_effect;
pub mod non_copy_const;
pub mod non_expressive_names;
pub mod ok_if_let;
pub mod open_options;
pub mod overflow_check_conditional;
pub mod panic_unimplemented;
pub mod partialeq_ne_impl;
pub mod precedence;
pub mod ptr;
pub mod ptr_offset_with_cast;
pub mod question_mark;
pub mod ranges;
pub mod redundant_clone;
pub mod redundant_field_names;
pub mod redundant_pattern_matching;
pub mod reference;
pub mod regex;
pub mod replace_consts;
pub mod returns;
pub mod serde_api;
pub mod shadow;
pub mod slow_vector_initialization;
pub mod strings;
pub mod suspicious_trait_impl;
pub mod swap;
pub mod temporary_assignment;
pub mod transmute;
pub mod trivially_copy_pass_by_ref;
pub mod types;
pub mod unicode;
pub mod unsafe_removed_from_name;
pub mod unused_io_amount;
pub mod unused_label;
pub mod unwrap;
pub mod use_self;
pub mod vec;
pub mod wildcard_dependencies;
pub mod write;
pub mod zero_div_zero;
// end lints modules, do not remove this comment, it’s used in `update_lints`

pub use crate::utils::conf::Conf;

mod reexport {
    crate use syntax::ast::{Name, NodeId};
}

pub fn register_pre_expansion_lints(
    session: &rustc::session::Session,
    store: &mut rustc::lint::LintStore,
    conf: &Conf,
) {
    store.register_pre_expansion_pass(Some(session), box write::Pass);
    store.register_pre_expansion_pass(Some(session), box redundant_field_names::RedundantFieldNames);
    store.register_pre_expansion_pass(
        Some(session),
        box non_expressive_names::NonExpressiveNames {
            single_char_binding_names_threshold: conf.single_char_binding_names_threshold,
        },
    );
    store.register_pre_expansion_pass(Some(session), box attrs::CfgAttrPass);
}

pub fn read_conf(reg: &rustc_plugin::Registry<'_>) -> Conf {
    match utils::conf::file_from_args(reg.args()) {
        Ok(file_name) => {
            // if the user specified a file, it must exist, otherwise default to `clippy.toml` but
            // do not require the file to exist
            let file_name = if let Some(file_name) = file_name {
                Some(file_name)
            } else {
                match utils::conf::lookup_conf_file() {
                    Ok(path) => path,
                    Err(error) => {
                        reg.sess
                            .struct_err(&format!("error finding Clippy's configuration file: {}", error))
                            .emit();
                        None
                    },
                }
            };

            let file_name = file_name.map(|file_name| {
                if file_name.is_relative() {
                    reg.sess
                        .local_crate_source_file
                        .as_ref()
                        .and_then(|file| std::path::Path::new(&file).parent().map(std::path::Path::to_path_buf))
                        .unwrap_or_default()
                        .join(file_name)
                } else {
                    file_name
                }
            });

            let (conf, errors) = utils::conf::read(file_name.as_ref().map(|p| p.as_ref()));

            // all conf errors are non-fatal, we just use the default conf in case of error
            for error in errors {
                reg.sess
                    .struct_err(&format!(
                        "error reading Clippy's configuration file `{}`: {}",
                        file_name.as_ref().and_then(|p| p.to_str()).unwrap_or(""),
                        error
                    ))
                    .emit();
            }

            conf
        },
        Err((err, span)) => {
            reg.sess
                .struct_span_err(span, err)
                .span_note(span, "Clippy will use default configuration")
                .emit();
            toml::from_str("").expect("we never error on empty config files")
        },
    }
}

#[rustfmt::skip]
pub fn register_plugins(reg: &mut rustc_plugin::Registry<'_>, conf: &Conf) {
    let mut store = reg.sess.lint_store.borrow_mut();
    // begin deprecated lints, do not remove this comment, it’s used in `update_lints`
    store.register_removed(
        "should_assert_eq",
        "`assert!()` will be more flexible with RFC 2011",
    );
    store.register_removed(
        "extend_from_slice",
        "`.extend_from_slice(_)` is a faster way to extend a Vec by a slice",
    );
    store.register_removed(
        "range_step_by_zero",
        "`iterator.step_by(0)` panics nowadays",
    );
    store.register_removed(
        "unstable_as_slice",
        "`Vec::as_slice` has been stabilized in 1.7",
    );
    store.register_removed(
        "unstable_as_mut_slice",
        "`Vec::as_mut_slice` has been stabilized in 1.7",
    );
    store.register_removed(
        "str_to_string",
        "using `str::to_string` is common even today and specialization will likely happen soon",
    );
    store.register_removed(
        "string_to_string",
        "using `string::to_string` is common even today and specialization will likely happen soon",
    );
    store.register_removed(
        "misaligned_transmute",
        "this lint has been split into cast_ptr_alignment and transmute_ptr_to_ptr",
    );
    store.register_removed(
        "assign_ops",
        "using compound assignment operators (e.g. `+=`) is harmless",
    );
    store.register_removed(
        "if_let_redundant_pattern_matching",
        "this lint has been changed to redundant_pattern_matching",
    );
    store.register_removed(
        "unsafe_vector_initialization",
        "the replacement suggested by this lint had substantially different behavior",
    );
    // end deprecated lints, do not remove this comment, it’s used in `update_lints`

    reg.register_late_lint_pass(box serde_api::Serde);
    reg.register_early_lint_pass(box utils::internal_lints::Clippy);
    reg.register_late_lint_pass(box utils::internal_lints::CompilerLintFunctions::new());
    reg.register_early_lint_pass(box utils::internal_lints::DefaultHashTypes::default());
    reg.register_late_lint_pass(box utils::internal_lints::LintWithoutLintPass::default());
    reg.register_late_lint_pass(box utils::inspector::Pass);
    reg.register_late_lint_pass(box utils::author::Pass);
    reg.register_late_lint_pass(box types::TypePass);
    reg.register_late_lint_pass(box booleans::NonminimalBool);
    reg.register_late_lint_pass(box eq_op::EqOp);
    reg.register_early_lint_pass(box enum_variants::EnumVariantNames::new(conf.enum_variant_name_threshold));
    reg.register_late_lint_pass(box enum_glob_use::EnumGlobUse);
    reg.register_late_lint_pass(box enum_clike::UnportableVariant);
    reg.register_late_lint_pass(box excessive_precision::ExcessivePrecision);
    reg.register_late_lint_pass(box bit_mask::BitMask::new(conf.verbose_bit_mask_threshold));
    reg.register_late_lint_pass(box ptr::PointerPass);
    reg.register_late_lint_pass(box needless_bool::NeedlessBool);
    reg.register_late_lint_pass(box needless_bool::BoolComparison);
    reg.register_late_lint_pass(box approx_const::Pass);
    reg.register_late_lint_pass(box misc::Pass);
    reg.register_early_lint_pass(box precedence::Precedence);
    reg.register_early_lint_pass(box needless_continue::NeedlessContinue);
    reg.register_late_lint_pass(box eta_reduction::EtaPass);
    reg.register_late_lint_pass(box identity_op::IdentityOp);
    reg.register_late_lint_pass(box erasing_op::ErasingOp);
    reg.register_early_lint_pass(box items_after_statements::ItemsAfterStatements);
    reg.register_late_lint_pass(box mut_mut::MutMut);
    reg.register_late_lint_pass(box mut_reference::UnnecessaryMutPassed);
    reg.register_late_lint_pass(box len_zero::LenZero);
    reg.register_late_lint_pass(box attrs::AttrPass);
    reg.register_early_lint_pass(box collapsible_if::CollapsibleIf);
    reg.register_late_lint_pass(box block_in_if_condition::BlockInIfCondition);
    reg.register_late_lint_pass(box unicode::Unicode);
    reg.register_late_lint_pass(box strings::StringAdd);
    reg.register_early_lint_pass(box returns::ReturnPass);
    reg.register_late_lint_pass(box implicit_return::Pass);
    reg.register_late_lint_pass(box methods::Pass);
    reg.register_late_lint_pass(box map_clone::Pass);
    reg.register_late_lint_pass(box shadow::Pass);
    reg.register_late_lint_pass(box types::LetPass);
    reg.register_late_lint_pass(box types::UnitCmp);
    reg.register_late_lint_pass(box loops::Pass);
    reg.register_late_lint_pass(box lifetimes::LifetimePass);
    reg.register_late_lint_pass(box entry::HashMapLint);
    reg.register_late_lint_pass(box ranges::Pass);
    reg.register_late_lint_pass(box types::CastPass);
    reg.register_late_lint_pass(box types::TypeComplexityPass::new(conf.type_complexity_threshold));
    reg.register_late_lint_pass(box matches::MatchPass);
    reg.register_late_lint_pass(box minmax::MinMaxPass);
    reg.register_late_lint_pass(box open_options::NonSensical);
    reg.register_late_lint_pass(box zero_div_zero::Pass);
    reg.register_late_lint_pass(box mutex_atomic::MutexAtomic);
    reg.register_late_lint_pass(box needless_update::Pass);
    reg.register_late_lint_pass(box needless_borrow::NeedlessBorrow::default());
    reg.register_late_lint_pass(box needless_borrowed_ref::NeedlessBorrowedRef);
    reg.register_late_lint_pass(box no_effect::Pass);
    reg.register_late_lint_pass(box temporary_assignment::Pass);
    reg.register_late_lint_pass(box transmute::Transmute);
    reg.register_late_lint_pass(
        box cyclomatic_complexity::CyclomaticComplexity::new(conf.cyclomatic_complexity_threshold)
    );
    reg.register_late_lint_pass(box escape::Pass{too_large_for_stack: conf.too_large_for_stack});
    reg.register_early_lint_pass(box misc_early::MiscEarly);
    reg.register_late_lint_pass(box panic_unimplemented::Pass);
    reg.register_late_lint_pass(box strings::StringLitAsBytes);
    reg.register_late_lint_pass(box derive::Derive);
    reg.register_late_lint_pass(box types::CharLitAsU8);
    reg.register_late_lint_pass(box vec::Pass);
    reg.register_late_lint_pass(box drop_forget_ref::Pass);
    reg.register_late_lint_pass(box empty_enum::EmptyEnum);
    reg.register_late_lint_pass(box types::AbsurdExtremeComparisons);
    reg.register_late_lint_pass(box types::InvalidUpcastComparisons);
    reg.register_late_lint_pass(box regex::Pass::default());
    reg.register_late_lint_pass(box copies::CopyAndPaste);
    reg.register_late_lint_pass(box copy_iterator::CopyIterator);
    reg.register_late_lint_pass(box format::Pass);
    reg.register_early_lint_pass(box formatting::Formatting);
    reg.register_late_lint_pass(box swap::Swap);
    reg.register_early_lint_pass(box if_not_else::IfNotElse);
    reg.register_early_lint_pass(box else_if_without_else::ElseIfWithoutElse);
    reg.register_early_lint_pass(box int_plus_one::IntPlusOne);
    reg.register_late_lint_pass(box overflow_check_conditional::OverflowCheckConditional);
    reg.register_late_lint_pass(box unused_label::UnusedLabel);
    reg.register_late_lint_pass(box new_without_default::NewWithoutDefault::default());
    reg.register_late_lint_pass(box blacklisted_name::BlackListedName::new(
            conf.blacklisted_names.iter().cloned().collect()
    ));
    reg.register_late_lint_pass(box functions::Functions::new(conf.too_many_arguments_threshold));
    reg.register_early_lint_pass(box doc::Doc::new(conf.doc_valid_idents.iter().cloned().collect()));
    reg.register_late_lint_pass(box neg_multiply::NegMultiply);
    reg.register_early_lint_pass(box unsafe_removed_from_name::UnsafeNameRemoval);
    reg.register_late_lint_pass(box mem_discriminant::MemDiscriminant);
    reg.register_late_lint_pass(box mem_forget::MemForget);
    reg.register_late_lint_pass(box mem_replace::MemReplace);
    reg.register_late_lint_pass(box arithmetic::Arithmetic::default());
    reg.register_late_lint_pass(box assign_ops::AssignOps);
    reg.register_late_lint_pass(box let_if_seq::LetIfSeq);
    reg.register_late_lint_pass(box eval_order_dependence::EvalOrderDependence);
    reg.register_late_lint_pass(box missing_doc::MissingDoc::new());
    reg.register_late_lint_pass(box missing_inline::MissingInline);
    reg.register_late_lint_pass(box ok_if_let::Pass);
    reg.register_late_lint_pass(box redundant_pattern_matching::Pass);
    reg.register_late_lint_pass(box partialeq_ne_impl::Pass);
    reg.register_early_lint_pass(box reference::Pass);
    reg.register_early_lint_pass(box reference::DerefPass);
    reg.register_early_lint_pass(box double_parens::DoubleParens);
    reg.register_late_lint_pass(box unused_io_amount::UnusedIoAmount);
    reg.register_late_lint_pass(box large_enum_variant::LargeEnumVariant::new(conf.enum_variant_size_threshold));
    reg.register_late_lint_pass(box explicit_write::Pass);
    reg.register_late_lint_pass(box needless_pass_by_value::NeedlessPassByValue);
    reg.register_late_lint_pass(box trivially_copy_pass_by_ref::TriviallyCopyPassByRef::new(
            conf.trivial_copy_size_limit,
            &reg.sess.target,
    ));
    reg.register_early_lint_pass(box literal_representation::LiteralDigitGrouping);
    reg.register_early_lint_pass(box literal_representation::LiteralRepresentation::new(
            conf.literal_representation_threshold
    ));
    reg.register_late_lint_pass(box use_self::UseSelf);
    reg.register_late_lint_pass(box bytecount::ByteCount);
    reg.register_late_lint_pass(box infinite_iter::Pass);
    reg.register_late_lint_pass(box inline_fn_without_body::Pass);
    reg.register_late_lint_pass(box invalid_ref::InvalidRef);
    reg.register_late_lint_pass(box identity_conversion::IdentityConversion::default());
    reg.register_late_lint_pass(box types::ImplicitHasher);
    reg.register_early_lint_pass(box const_static_lifetime::StaticConst);
    reg.register_late_lint_pass(box fallible_impl_from::FallibleImplFrom);
    reg.register_late_lint_pass(box replace_consts::ReplaceConsts);
    reg.register_late_lint_pass(box types::UnitArg);
    reg.register_late_lint_pass(box double_comparison::Pass);
    reg.register_late_lint_pass(box question_mark::Pass);
    reg.register_late_lint_pass(box suspicious_trait_impl::SuspiciousImpl);
    reg.register_early_lint_pass(box cargo_common_metadata::Pass);
    reg.register_early_lint_pass(box multiple_crate_versions::Pass);
    reg.register_early_lint_pass(box wildcard_dependencies::Pass);
    reg.register_late_lint_pass(box map_unit_fn::Pass);
    reg.register_late_lint_pass(box infallible_destructuring_match::Pass);
    reg.register_late_lint_pass(box inherent_impl::Pass::default());
    reg.register_late_lint_pass(box neg_cmp_op_on_partial_ord::NoNegCompOpForPartialOrd);
    reg.register_late_lint_pass(box unwrap::Pass);
    reg.register_late_lint_pass(box duration_subsec::DurationSubsec);
    reg.register_late_lint_pass(box default_trait_access::DefaultTraitAccess);
    reg.register_late_lint_pass(box indexing_slicing::IndexingSlicing);
    reg.register_late_lint_pass(box non_copy_const::NonCopyConst);
    reg.register_late_lint_pass(box ptr_offset_with_cast::Pass);
    reg.register_late_lint_pass(box redundant_clone::RedundantClone);
    reg.register_late_lint_pass(box slow_vector_initialization::Pass);
    reg.register_late_lint_pass(box types::RefToMut);

    reg.register_lint_group("clippy::restriction", Some("clippy_restriction"), vec![
        arithmetic::FLOAT_ARITHMETIC,
        arithmetic::INTEGER_ARITHMETIC,
        else_if_without_else::ELSE_IF_WITHOUT_ELSE,
        implicit_return::IMPLICIT_RETURN,
        indexing_slicing::INDEXING_SLICING,
        inherent_impl::MULTIPLE_INHERENT_IMPL,
        literal_representation::DECIMAL_LITERAL_REPRESENTATION,
        mem_forget::MEM_FORGET,
        methods::CLONE_ON_REF_PTR,
        methods::OPTION_UNWRAP_USED,
        methods::RESULT_UNWRAP_USED,
        methods::WRONG_PUB_SELF_CONVENTION,
        misc::FLOAT_CMP_CONST,
        missing_doc::MISSING_DOCS_IN_PRIVATE_ITEMS,
        missing_inline::MISSING_INLINE_IN_PUBLIC_ITEMS,
        panic_unimplemented::UNIMPLEMENTED,
        shadow::SHADOW_REUSE,
        shadow::SHADOW_SAME,
        strings::STRING_ADD,
        write::PRINT_STDOUT,
        write::USE_DEBUG,
    ]);

    reg.register_lint_group("clippy::pedantic", Some("clippy_pedantic"), vec![
        attrs::INLINE_ALWAYS,
        copies::MATCH_SAME_ARMS,
        copy_iterator::COPY_ITERATOR,
        default_trait_access::DEFAULT_TRAIT_ACCESS,
        derive::EXPL_IMPL_CLONE_ON_COPY,
        doc::DOC_MARKDOWN,
        empty_enum::EMPTY_ENUM,
        enum_glob_use::ENUM_GLOB_USE,
        enum_variants::MODULE_NAME_REPETITIONS,
        enum_variants::PUB_ENUM_VARIANT_NAMES,
        if_not_else::IF_NOT_ELSE,
        infinite_iter::MAYBE_INFINITE_ITER,
        items_after_statements::ITEMS_AFTER_STATEMENTS,
        literal_representation::LARGE_DIGIT_GROUPS,
        loops::EXPLICIT_INTO_ITER_LOOP,
        loops::EXPLICIT_ITER_LOOP,
        matches::SINGLE_MATCH_ELSE,
        methods::FILTER_MAP,
        methods::MAP_FLATTEN,
        methods::OPTION_MAP_UNWRAP_OR,
        methods::OPTION_MAP_UNWRAP_OR_ELSE,
        methods::RESULT_MAP_UNWRAP_OR_ELSE,
        misc::USED_UNDERSCORE_BINDING,
        misc_early::UNSEPARATED_LITERAL_SUFFIX,
        mut_mut::MUT_MUT,
        needless_continue::NEEDLESS_CONTINUE,
        needless_pass_by_value::NEEDLESS_PASS_BY_VALUE,
        non_expressive_names::SIMILAR_NAMES,
        replace_consts::REPLACE_CONSTS,
        shadow::SHADOW_UNRELATED,
        strings::STRING_ADD_ASSIGN,
        types::CAST_POSSIBLE_TRUNCATION,
        types::CAST_POSSIBLE_WRAP,
        types::CAST_PRECISION_LOSS,
        types::CAST_SIGN_LOSS,
        types::INVALID_UPCAST_COMPARISONS,
        types::LINKEDLIST,
        unicode::NON_ASCII_LITERAL,
        unicode::UNICODE_NOT_NFC,
        use_self::USE_SELF,
    ]);

    reg.register_lint_group("clippy::internal", Some("clippy_internal"), vec![
        utils::internal_lints::CLIPPY_LINTS_INTERNAL,
        utils::internal_lints::COMPILER_LINT_FUNCTIONS,
        utils::internal_lints::DEFAULT_HASH_TYPES,
        utils::internal_lints::LINT_WITHOUT_LINT_PASS,
    ]);

    reg.register_lint_group("clippy::all", Some("clippy"), vec![
        approx_const::APPROX_CONSTANT,
        assign_ops::ASSIGN_OP_PATTERN,
        assign_ops::MISREFACTORED_ASSIGN_OP,
        attrs::DEPRECATED_CFG_ATTR,
        attrs::DEPRECATED_SEMVER,
        attrs::UNKNOWN_CLIPPY_LINTS,
        attrs::USELESS_ATTRIBUTE,
        bit_mask::BAD_BIT_MASK,
        bit_mask::INEFFECTIVE_BIT_MASK,
        bit_mask::VERBOSE_BIT_MASK,
        blacklisted_name::BLACKLISTED_NAME,
        block_in_if_condition::BLOCK_IN_IF_CONDITION_EXPR,
        block_in_if_condition::BLOCK_IN_IF_CONDITION_STMT,
        booleans::LOGIC_BUG,
        booleans::NONMINIMAL_BOOL,
        bytecount::NAIVE_BYTECOUNT,
        collapsible_if::COLLAPSIBLE_IF,
        const_static_lifetime::CONST_STATIC_LIFETIME,
        copies::IFS_SAME_COND,
        copies::IF_SAME_THEN_ELSE,
        cyclomatic_complexity::CYCLOMATIC_COMPLEXITY,
        derive::DERIVE_HASH_XOR_EQ,
        double_comparison::DOUBLE_COMPARISONS,
        double_parens::DOUBLE_PARENS,
        drop_forget_ref::DROP_COPY,
        drop_forget_ref::DROP_REF,
        drop_forget_ref::FORGET_COPY,
        drop_forget_ref::FORGET_REF,
        duration_subsec::DURATION_SUBSEC,
        entry::MAP_ENTRY,
        enum_clike::ENUM_CLIKE_UNPORTABLE_VARIANT,
        enum_variants::ENUM_VARIANT_NAMES,
        enum_variants::MODULE_INCEPTION,
        eq_op::EQ_OP,
        eq_op::OP_REF,
        erasing_op::ERASING_OP,
        escape::BOXED_LOCAL,
        eta_reduction::REDUNDANT_CLOSURE,
        eval_order_dependence::DIVERGING_SUB_EXPRESSION,
        eval_order_dependence::EVAL_ORDER_DEPENDENCE,
        excessive_precision::EXCESSIVE_PRECISION,
        explicit_write::EXPLICIT_WRITE,
        format::USELESS_FORMAT,
        formatting::POSSIBLE_MISSING_COMMA,
        formatting::SUSPICIOUS_ASSIGNMENT_FORMATTING,
        formatting::SUSPICIOUS_ELSE_FORMATTING,
        functions::NOT_UNSAFE_PTR_ARG_DEREF,
        functions::TOO_MANY_ARGUMENTS,
        identity_conversion::IDENTITY_CONVERSION,
        identity_op::IDENTITY_OP,
        indexing_slicing::OUT_OF_BOUNDS_INDEXING,
        infallible_destructuring_match::INFALLIBLE_DESTRUCTURING_MATCH,
        infinite_iter::INFINITE_ITER,
        inline_fn_without_body::INLINE_FN_WITHOUT_BODY,
        int_plus_one::INT_PLUS_ONE,
        invalid_ref::INVALID_REF,
        large_enum_variant::LARGE_ENUM_VARIANT,
        len_zero::LEN_WITHOUT_IS_EMPTY,
        len_zero::LEN_ZERO,
        let_if_seq::USELESS_LET_IF_SEQ,
        lifetimes::EXTRA_UNUSED_LIFETIMES,
        lifetimes::NEEDLESS_LIFETIMES,
        literal_representation::INCONSISTENT_DIGIT_GROUPING,
        literal_representation::MISTYPED_LITERAL_SUFFIXES,
        literal_representation::UNREADABLE_LITERAL,
        loops::EMPTY_LOOP,
        loops::EXPLICIT_COUNTER_LOOP,
        loops::FOR_KV_MAP,
        loops::FOR_LOOP_OVER_OPTION,
        loops::FOR_LOOP_OVER_RESULT,
        loops::ITER_NEXT_LOOP,
        loops::MANUAL_MEMCPY,
        loops::MUT_RANGE_BOUND,
        loops::NEEDLESS_COLLECT,
        loops::NEEDLESS_RANGE_LOOP,
        loops::NEVER_LOOP,
        loops::REVERSE_RANGE_LOOP,
        loops::UNUSED_COLLECT,
        loops::WHILE_IMMUTABLE_CONDITION,
        loops::WHILE_LET_LOOP,
        loops::WHILE_LET_ON_ITERATOR,
        map_clone::MAP_CLONE,
        map_unit_fn::OPTION_MAP_UNIT_FN,
        map_unit_fn::RESULT_MAP_UNIT_FN,
        matches::MATCH_AS_REF,
        matches::MATCH_BOOL,
        matches::MATCH_OVERLAPPING_ARM,
        matches::MATCH_REF_PATS,
        matches::MATCH_WILD_ERR_ARM,
        matches::SINGLE_MATCH,
        mem_discriminant::MEM_DISCRIMINANT_NON_ENUM,
        mem_replace::MEM_REPLACE_OPTION_WITH_NONE,
        methods::CHARS_LAST_CMP,
        methods::CHARS_NEXT_CMP,
        methods::CLONE_DOUBLE_REF,
        methods::CLONE_ON_COPY,
        methods::EXPECT_FUN_CALL,
        methods::FILTER_NEXT,
        methods::GET_UNWRAP,
        methods::INTO_ITER_ON_ARRAY,
        methods::INTO_ITER_ON_REF,
        methods::ITER_CLONED_COLLECT,
        methods::ITER_NTH,
        methods::ITER_SKIP_NEXT,
        methods::NEW_RET_NO_SELF,
        methods::OK_EXPECT,
        methods::OPTION_MAP_OR_NONE,
        methods::OR_FUN_CALL,
        methods::SEARCH_IS_SOME,
        methods::SHOULD_IMPLEMENT_TRAIT,
        methods::SINGLE_CHAR_PATTERN,
        methods::STRING_EXTEND_CHARS,
        methods::TEMPORARY_CSTRING_AS_PTR,
        methods::UNNECESSARY_FILTER_MAP,
        methods::UNNECESSARY_FOLD,
        methods::USELESS_ASREF,
        methods::WRONG_SELF_CONVENTION,
        minmax::MIN_MAX,
        misc::CMP_NAN,
        misc::CMP_OWNED,
        misc::FLOAT_CMP,
        misc::MODULO_ONE,
        misc::REDUNDANT_PATTERN,
        misc::SHORT_CIRCUIT_STATEMENT,
        misc::TOPLEVEL_REF_ARG,
        misc::ZERO_PTR,
        misc_early::BUILTIN_TYPE_SHADOW,
        misc_early::DOUBLE_NEG,
        misc_early::DUPLICATE_UNDERSCORE_ARGUMENT,
        misc_early::MIXED_CASE_HEX_LITERALS,
        misc_early::REDUNDANT_CLOSURE_CALL,
        misc_early::UNNEEDED_FIELD_PATTERN,
        misc_early::ZERO_PREFIXED_LITERAL,
        mut_reference::UNNECESSARY_MUT_PASSED,
        mutex_atomic::MUTEX_ATOMIC,
        needless_bool::BOOL_COMPARISON,
        needless_bool::NEEDLESS_BOOL,
        needless_borrowed_ref::NEEDLESS_BORROWED_REFERENCE,
        needless_update::NEEDLESS_UPDATE,
        neg_cmp_op_on_partial_ord::NEG_CMP_OP_ON_PARTIAL_ORD,
        neg_multiply::NEG_MULTIPLY,
        new_without_default::NEW_WITHOUT_DEFAULT,
        no_effect::NO_EFFECT,
        no_effect::UNNECESSARY_OPERATION,
        non_copy_const::BORROW_INTERIOR_MUTABLE_CONST,
        non_copy_const::DECLARE_INTERIOR_MUTABLE_CONST,
        non_expressive_names::JUST_UNDERSCORES_AND_DIGITS,
        non_expressive_names::MANY_SINGLE_CHAR_NAMES,
        ok_if_let::IF_LET_SOME_RESULT,
        open_options::NONSENSICAL_OPEN_OPTIONS,
        overflow_check_conditional::OVERFLOW_CHECK_CONDITIONAL,
        panic_unimplemented::PANIC_PARAMS,
        partialeq_ne_impl::PARTIALEQ_NE_IMPL,
        precedence::PRECEDENCE,
        ptr::CMP_NULL,
        ptr::MUT_FROM_REF,
        ptr::PTR_ARG,
        ptr_offset_with_cast::PTR_OFFSET_WITH_CAST,
        question_mark::QUESTION_MARK,
        ranges::ITERATOR_STEP_BY_ZERO,
        ranges::RANGE_MINUS_ONE,
        ranges::RANGE_PLUS_ONE,
        ranges::RANGE_ZIP_WITH_LEN,
        redundant_field_names::REDUNDANT_FIELD_NAMES,
        redundant_pattern_matching::REDUNDANT_PATTERN_MATCHING,
        reference::DEREF_ADDROF,
        reference::REF_IN_DEREF,
        regex::INVALID_REGEX,
        regex::REGEX_MACRO,
        regex::TRIVIAL_REGEX,
        returns::LET_AND_RETURN,
        returns::NEEDLESS_RETURN,
        returns::UNUSED_UNIT,
        serde_api::SERDE_API_MISUSE,
        slow_vector_initialization::SLOW_VECTOR_INITIALIZATION,
        strings::STRING_LIT_AS_BYTES,
        suspicious_trait_impl::SUSPICIOUS_ARITHMETIC_IMPL,
        suspicious_trait_impl::SUSPICIOUS_OP_ASSIGN_IMPL,
        swap::ALMOST_SWAPPED,
        swap::MANUAL_SWAP,
        temporary_assignment::TEMPORARY_ASSIGNMENT,
        transmute::CROSSPOINTER_TRANSMUTE,
        transmute::TRANSMUTE_BYTES_TO_STR,
        transmute::TRANSMUTE_INT_TO_BOOL,
        transmute::TRANSMUTE_INT_TO_CHAR,
        transmute::TRANSMUTE_INT_TO_FLOAT,
        transmute::TRANSMUTE_PTR_TO_PTR,
        transmute::TRANSMUTE_PTR_TO_REF,
        transmute::USELESS_TRANSMUTE,
        transmute::WRONG_TRANSMUTE,
        trivially_copy_pass_by_ref::TRIVIALLY_COPY_PASS_BY_REF,
        types::ABSURD_EXTREME_COMPARISONS,
        types::BORROWED_BOX,
        types::BOX_VEC,
        types::CAST_LOSSLESS,
        types::CAST_PTR_ALIGNMENT,
        types::CAST_REF_TO_MUT,
        types::CHAR_LIT_AS_U8,
        types::FN_TO_NUMERIC_CAST,
        types::FN_TO_NUMERIC_CAST_WITH_TRUNCATION,
        types::IMPLICIT_HASHER,
        types::LET_UNIT_VALUE,
        types::OPTION_OPTION,
        types::TYPE_COMPLEXITY,
        types::UNIT_ARG,
        types::UNIT_CMP,
        types::UNNECESSARY_CAST,
        types::VEC_BOX,
        unicode::ZERO_WIDTH_SPACE,
        unsafe_removed_from_name::UNSAFE_REMOVED_FROM_NAME,
        unused_io_amount::UNUSED_IO_AMOUNT,
        unused_label::UNUSED_LABEL,
        vec::USELESS_VEC,
        write::PRINTLN_EMPTY_STRING,
        write::PRINT_LITERAL,
        write::PRINT_WITH_NEWLINE,
        write::WRITELN_EMPTY_STRING,
        write::WRITE_LITERAL,
        write::WRITE_WITH_NEWLINE,
        zero_div_zero::ZERO_DIVIDED_BY_ZERO,
    ]);

    reg.register_lint_group("clippy::style", Some("clippy_style"), vec![
        assign_ops::ASSIGN_OP_PATTERN,
        attrs::UNKNOWN_CLIPPY_LINTS,
        bit_mask::VERBOSE_BIT_MASK,
        blacklisted_name::BLACKLISTED_NAME,
        block_in_if_condition::BLOCK_IN_IF_CONDITION_EXPR,
        block_in_if_condition::BLOCK_IN_IF_CONDITION_STMT,
        collapsible_if::COLLAPSIBLE_IF,
        const_static_lifetime::CONST_STATIC_LIFETIME,
        enum_variants::ENUM_VARIANT_NAMES,
        enum_variants::MODULE_INCEPTION,
        eq_op::OP_REF,
        eta_reduction::REDUNDANT_CLOSURE,
        excessive_precision::EXCESSIVE_PRECISION,
        formatting::SUSPICIOUS_ASSIGNMENT_FORMATTING,
        formatting::SUSPICIOUS_ELSE_FORMATTING,
        infallible_destructuring_match::INFALLIBLE_DESTRUCTURING_MATCH,
        len_zero::LEN_WITHOUT_IS_EMPTY,
        len_zero::LEN_ZERO,
        let_if_seq::USELESS_LET_IF_SEQ,
        literal_representation::INCONSISTENT_DIGIT_GROUPING,
        literal_representation::UNREADABLE_LITERAL,
        loops::EMPTY_LOOP,
        loops::FOR_KV_MAP,
        loops::NEEDLESS_RANGE_LOOP,
        loops::WHILE_LET_ON_ITERATOR,
        map_clone::MAP_CLONE,
        matches::MATCH_BOOL,
        matches::MATCH_OVERLAPPING_ARM,
        matches::MATCH_REF_PATS,
        matches::MATCH_WILD_ERR_ARM,
        matches::SINGLE_MATCH,
        mem_replace::MEM_REPLACE_OPTION_WITH_NONE,
        methods::CHARS_LAST_CMP,
        methods::GET_UNWRAP,
        methods::INTO_ITER_ON_REF,
        methods::ITER_CLONED_COLLECT,
        methods::ITER_SKIP_NEXT,
        methods::NEW_RET_NO_SELF,
        methods::OK_EXPECT,
        methods::OPTION_MAP_OR_NONE,
        methods::SHOULD_IMPLEMENT_TRAIT,
        methods::STRING_EXTEND_CHARS,
        methods::UNNECESSARY_FOLD,
        methods::WRONG_SELF_CONVENTION,
        misc::REDUNDANT_PATTERN,
        misc::TOPLEVEL_REF_ARG,
        misc::ZERO_PTR,
        misc_early::BUILTIN_TYPE_SHADOW,
        misc_early::DOUBLE_NEG,
        misc_early::DUPLICATE_UNDERSCORE_ARGUMENT,
        misc_early::MIXED_CASE_HEX_LITERALS,
        misc_early::UNNEEDED_FIELD_PATTERN,
        mut_reference::UNNECESSARY_MUT_PASSED,
        neg_multiply::NEG_MULTIPLY,
        new_without_default::NEW_WITHOUT_DEFAULT,
        non_expressive_names::JUST_UNDERSCORES_AND_DIGITS,
        non_expressive_names::MANY_SINGLE_CHAR_NAMES,
        ok_if_let::IF_LET_SOME_RESULT,
        panic_unimplemented::PANIC_PARAMS,
        ptr::CMP_NULL,
        ptr::PTR_ARG,
        question_mark::QUESTION_MARK,
        redundant_field_names::REDUNDANT_FIELD_NAMES,
        redundant_pattern_matching::REDUNDANT_PATTERN_MATCHING,
        regex::REGEX_MACRO,
        regex::TRIVIAL_REGEX,
        returns::LET_AND_RETURN,
        returns::NEEDLESS_RETURN,
        returns::UNUSED_UNIT,
        strings::STRING_LIT_AS_BYTES,
        types::FN_TO_NUMERIC_CAST,
        types::FN_TO_NUMERIC_CAST_WITH_TRUNCATION,
        types::IMPLICIT_HASHER,
        types::LET_UNIT_VALUE,
        unsafe_removed_from_name::UNSAFE_REMOVED_FROM_NAME,
        write::PRINTLN_EMPTY_STRING,
        write::PRINT_LITERAL,
        write::PRINT_WITH_NEWLINE,
        write::WRITELN_EMPTY_STRING,
        write::WRITE_LITERAL,
        write::WRITE_WITH_NEWLINE,
    ]);

    reg.register_lint_group("clippy::complexity", Some("clippy_complexity"), vec![
        assign_ops::MISREFACTORED_ASSIGN_OP,
        attrs::DEPRECATED_CFG_ATTR,
        booleans::NONMINIMAL_BOOL,
        cyclomatic_complexity::CYCLOMATIC_COMPLEXITY,
        double_comparison::DOUBLE_COMPARISONS,
        double_parens::DOUBLE_PARENS,
        duration_subsec::DURATION_SUBSEC,
        eval_order_dependence::DIVERGING_SUB_EXPRESSION,
        eval_order_dependence::EVAL_ORDER_DEPENDENCE,
        explicit_write::EXPLICIT_WRITE,
        format::USELESS_FORMAT,
        functions::TOO_MANY_ARGUMENTS,
        identity_conversion::IDENTITY_CONVERSION,
        identity_op::IDENTITY_OP,
        int_plus_one::INT_PLUS_ONE,
        lifetimes::EXTRA_UNUSED_LIFETIMES,
        lifetimes::NEEDLESS_LIFETIMES,
        loops::EXPLICIT_COUNTER_LOOP,
        loops::MUT_RANGE_BOUND,
        loops::WHILE_LET_LOOP,
        map_unit_fn::OPTION_MAP_UNIT_FN,
        map_unit_fn::RESULT_MAP_UNIT_FN,
        matches::MATCH_AS_REF,
        methods::CHARS_NEXT_CMP,
        methods::CLONE_ON_COPY,
        methods::FILTER_NEXT,
        methods::SEARCH_IS_SOME,
        methods::UNNECESSARY_FILTER_MAP,
        methods::USELESS_ASREF,
        misc::SHORT_CIRCUIT_STATEMENT,
        misc_early::REDUNDANT_CLOSURE_CALL,
        misc_early::ZERO_PREFIXED_LITERAL,
        needless_bool::BOOL_COMPARISON,
        needless_bool::NEEDLESS_BOOL,
        needless_borrowed_ref::NEEDLESS_BORROWED_REFERENCE,
        needless_update::NEEDLESS_UPDATE,
        neg_cmp_op_on_partial_ord::NEG_CMP_OP_ON_PARTIAL_ORD,
        no_effect::NO_EFFECT,
        no_effect::UNNECESSARY_OPERATION,
        overflow_check_conditional::OVERFLOW_CHECK_CONDITIONAL,
        partialeq_ne_impl::PARTIALEQ_NE_IMPL,
        precedence::PRECEDENCE,
        ptr_offset_with_cast::PTR_OFFSET_WITH_CAST,
        ranges::RANGE_MINUS_ONE,
        ranges::RANGE_PLUS_ONE,
        ranges::RANGE_ZIP_WITH_LEN,
        reference::DEREF_ADDROF,
        reference::REF_IN_DEREF,
        swap::MANUAL_SWAP,
        temporary_assignment::TEMPORARY_ASSIGNMENT,
        transmute::CROSSPOINTER_TRANSMUTE,
        transmute::TRANSMUTE_BYTES_TO_STR,
        transmute::TRANSMUTE_INT_TO_BOOL,
        transmute::TRANSMUTE_INT_TO_CHAR,
        transmute::TRANSMUTE_INT_TO_FLOAT,
        transmute::TRANSMUTE_PTR_TO_PTR,
        transmute::TRANSMUTE_PTR_TO_REF,
        transmute::USELESS_TRANSMUTE,
        types::BORROWED_BOX,
        types::CAST_LOSSLESS,
        types::CHAR_LIT_AS_U8,
        types::OPTION_OPTION,
        types::TYPE_COMPLEXITY,
        types::UNIT_ARG,
        types::UNNECESSARY_CAST,
        types::VEC_BOX,
        unused_label::UNUSED_LABEL,
        zero_div_zero::ZERO_DIVIDED_BY_ZERO,
    ]);

    reg.register_lint_group("clippy::correctness", Some("clippy_correctness"), vec![
        approx_const::APPROX_CONSTANT,
        attrs::DEPRECATED_SEMVER,
        attrs::USELESS_ATTRIBUTE,
        bit_mask::BAD_BIT_MASK,
        bit_mask::INEFFECTIVE_BIT_MASK,
        booleans::LOGIC_BUG,
        copies::IFS_SAME_COND,
        copies::IF_SAME_THEN_ELSE,
        derive::DERIVE_HASH_XOR_EQ,
        drop_forget_ref::DROP_COPY,
        drop_forget_ref::DROP_REF,
        drop_forget_ref::FORGET_COPY,
        drop_forget_ref::FORGET_REF,
        enum_clike::ENUM_CLIKE_UNPORTABLE_VARIANT,
        eq_op::EQ_OP,
        erasing_op::ERASING_OP,
        formatting::POSSIBLE_MISSING_COMMA,
        functions::NOT_UNSAFE_PTR_ARG_DEREF,
        indexing_slicing::OUT_OF_BOUNDS_INDEXING,
        infinite_iter::INFINITE_ITER,
        inline_fn_without_body::INLINE_FN_WITHOUT_BODY,
        invalid_ref::INVALID_REF,
        literal_representation::MISTYPED_LITERAL_SUFFIXES,
        loops::FOR_LOOP_OVER_OPTION,
        loops::FOR_LOOP_OVER_RESULT,
        loops::ITER_NEXT_LOOP,
        loops::NEVER_LOOP,
        loops::REVERSE_RANGE_LOOP,
        loops::WHILE_IMMUTABLE_CONDITION,
        mem_discriminant::MEM_DISCRIMINANT_NON_ENUM,
        methods::CLONE_DOUBLE_REF,
        methods::INTO_ITER_ON_ARRAY,
        methods::TEMPORARY_CSTRING_AS_PTR,
        minmax::MIN_MAX,
        misc::CMP_NAN,
        misc::FLOAT_CMP,
        misc::MODULO_ONE,
        non_copy_const::BORROW_INTERIOR_MUTABLE_CONST,
        non_copy_const::DECLARE_INTERIOR_MUTABLE_CONST,
        open_options::NONSENSICAL_OPEN_OPTIONS,
        ptr::MUT_FROM_REF,
        ranges::ITERATOR_STEP_BY_ZERO,
        regex::INVALID_REGEX,
        serde_api::SERDE_API_MISUSE,
        suspicious_trait_impl::SUSPICIOUS_ARITHMETIC_IMPL,
        suspicious_trait_impl::SUSPICIOUS_OP_ASSIGN_IMPL,
        swap::ALMOST_SWAPPED,
        transmute::WRONG_TRANSMUTE,
        types::ABSURD_EXTREME_COMPARISONS,
        types::CAST_PTR_ALIGNMENT,
        types::CAST_REF_TO_MUT,
        types::UNIT_CMP,
        unicode::ZERO_WIDTH_SPACE,
        unused_io_amount::UNUSED_IO_AMOUNT,
    ]);

    reg.register_lint_group("clippy::perf", Some("clippy_perf"), vec![
        bytecount::NAIVE_BYTECOUNT,
        entry::MAP_ENTRY,
        escape::BOXED_LOCAL,
        large_enum_variant::LARGE_ENUM_VARIANT,
        loops::MANUAL_MEMCPY,
        loops::NEEDLESS_COLLECT,
        loops::UNUSED_COLLECT,
        methods::EXPECT_FUN_CALL,
        methods::ITER_NTH,
        methods::OR_FUN_CALL,
        methods::SINGLE_CHAR_PATTERN,
        misc::CMP_OWNED,
        mutex_atomic::MUTEX_ATOMIC,
        slow_vector_initialization::SLOW_VECTOR_INITIALIZATION,
        trivially_copy_pass_by_ref::TRIVIALLY_COPY_PASS_BY_REF,
        types::BOX_VEC,
        vec::USELESS_VEC,
    ]);

    reg.register_lint_group("clippy::cargo", Some("clippy_cargo"), vec![
        cargo_common_metadata::CARGO_COMMON_METADATA,
        multiple_crate_versions::MULTIPLE_CRATE_VERSIONS,
        wildcard_dependencies::WILDCARD_DEPENDENCIES,
    ]);

    reg.register_lint_group("clippy::nursery", Some("clippy_nursery"), vec![
        attrs::EMPTY_LINE_AFTER_OUTER_ATTR,
        fallible_impl_from::FALLIBLE_IMPL_FROM,
        mutex_atomic::MUTEX_INTEGER,
        needless_borrow::NEEDLESS_BORROW,
        redundant_clone::REDUNDANT_CLONE,
        unwrap::PANICKING_UNWRAP,
        unwrap::UNNECESSARY_UNWRAP,
    ]);
}

pub fn register_renamed(ls: &mut rustc::lint::LintStore) {
    ls.register_renamed("clippy::stutter", "clippy::module_name_repetitions");
    ls.register_renamed("clippy::new_without_default_derive", "clippy::new_without_default");
}

// only exists to let the dogfood integration test works.
// Don't run clippy as an executable directly
#[allow(dead_code)]
fn main() {
    panic!("Please use the cargo-clippy executable");
}
