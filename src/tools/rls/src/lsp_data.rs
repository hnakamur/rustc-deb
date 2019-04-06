// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Types, helpers, and conversions to and from LSP and `racer` types.

use std::error::Error;
use std::fmt;
use std::path::PathBuf;

use languageserver_types as ls_types;
use racer;
use rls_analysis::DefKind;
use rls_span as span;
use serde_derive::{Deserialize, Serialize};
use url::Url;

use crate::config;
use crate::actions::hover;

pub use languageserver_types::notification::Notification as LSPNotification;
pub use languageserver_types::request::Request as LSPRequest;
pub use languageserver_types::*;

/// Errors that can occur when parsing a file URI.
#[derive(Debug)]
pub enum UrlFileParseError {
    /// The URI scheme is not `file`.
    InvalidScheme,
    /// Invalid file path in the URI.
    InvalidFilePath,
}

impl Error for UrlFileParseError {
    fn description(&self) -> &str {
        match *self {
            UrlFileParseError::InvalidScheme => "URI scheme is not `file`",
            UrlFileParseError::InvalidFilePath => "Invalid file path in URI",
        }
    }
}

impl fmt::Display for UrlFileParseError
where
    UrlFileParseError: Error,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

/// Parse the given URI into a `PathBuf`.
pub fn parse_file_path(uri: &Url) -> Result<PathBuf, UrlFileParseError> {
    if uri.scheme() == "file" {
        uri.to_file_path()
            .map_err(|_err| UrlFileParseError::InvalidFilePath)
    } else {
        Err(UrlFileParseError::InvalidScheme)
    }
}

/// Create an edit for the given location and text.
pub fn make_workspace_edit(location: Location, new_text: String) -> WorkspaceEdit {
    let changes = vec![(
        location.uri,
        vec![TextEdit {
            range: location.range,
            new_text,
        }],
    )].into_iter()
    .collect();

    WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
    }
}

/// Utilities for working with the language server protocol.
pub mod ls_util {
    use super::*;
    use crate::Span;

    /// Convert a language server protocol range into an RLS range.
    /// NOTE: This does not translate LSP UTF-16 code units offsets into Unicode
    /// Scalar Value offsets as expected by RLS/Rust.
    pub fn range_to_rls(r: Range) -> span::Range<span::ZeroIndexed> {
        span::Range::from_positions(position_to_rls(r.start), position_to_rls(r.end))
    }

    /// Convert a language server protocol position into an RLS position.
    pub fn position_to_rls(p: Position) -> span::Position<span::ZeroIndexed> {
        span::Position::new(
            span::Row::new_zero_indexed(p.line as u32),
            span::Column::new_zero_indexed(p.character as u32),
        )
    }

    /// Convert a language server protocol location into an RLS span.
    pub fn location_to_rls(
        l: &Location,
    ) -> Result<span::Span<span::ZeroIndexed>, UrlFileParseError> {
        parse_file_path(&l.uri).map(|path| Span::from_range(range_to_rls(l.range), path))
    }

    /// Convert an RLS span into a language server protocol location.
    pub fn rls_to_location(span: &Span) -> Location {
        // An RLS span has the same info as an LSP Location
        Location {
            uri: Url::from_file_path(&span.file).unwrap(),
            range: rls_to_range(span.range),
        }
    }

    /// Convert an RLS location into a language server protocol location.
    pub fn rls_location_to_location(l: &span::Location<span::ZeroIndexed>) -> Location {
        Location {
            uri: Url::from_file_path(&l.file).unwrap(),
            range: rls_to_range(span::Range::from_positions(l.position, l.position)),
        }
    }

    /// Convert an RLS range into a language server protocol range.
    pub fn rls_to_range(r: span::Range<span::ZeroIndexed>) -> Range {
        Range {
            start: rls_to_position(r.start()),
            end: rls_to_position(r.end()),
        }
    }

    /// Convert an RLS position into a language server protocol range.
    pub fn rls_to_position(p: span::Position<span::ZeroIndexed>) -> Position {
        Position {
            line: p.row.0.into(),
            character: p.col.0.into(),
        }
    }

    /// Creates a `Range` spanning the whole file as currently known by `Vfs`
    ///
    /// Panics if `Vfs` cannot load the file.
    pub fn range_from_file_string(content: impl AsRef<str>) -> Range {
        let content = content.as_ref();

        if content.is_empty() {
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 0),
            }
        } else {
            let mut line_count = content.lines().count() as u64 - 1;
            let col = if content.ends_with('\n') {
                line_count += 1;
                0
            } else {
                content
                    .lines()
                    .last()
                    .expect("String is not empty.")
                    .chars()
                    // LSP uses UTF-16 code units offset
                    .map(|chr| chr.len_utf16() as u64)
                    .sum()
            };
            // range is zero-based and the end position is exclusive
            Range {
                start: Position::new(0, 0),
                end: Position::new(line_count, col),
            }
        }
    }
}

/// Convert an RLS def-kind to a language server protocol symbol-kind.
pub fn source_kind_from_def_kind(k: DefKind) -> SymbolKind {
    match k {
        DefKind::Enum | DefKind::Union => SymbolKind::Enum,
        DefKind::Static | DefKind::Const | DefKind::ForeignStatic => SymbolKind::Constant,
        DefKind::Tuple => SymbolKind::Array,
        DefKind::Struct => SymbolKind::Struct,
        DefKind::Function | DefKind::Macro | DefKind::ForeignFunction => SymbolKind::Function,
        DefKind::Method => SymbolKind::Method,
        DefKind::Mod => SymbolKind::Module,
        DefKind::Trait => SymbolKind::Interface,
        DefKind::Type | DefKind::ExternType => SymbolKind::TypeParameter,
        DefKind::Local => SymbolKind::Variable,
        DefKind::Field => SymbolKind::Field,
        DefKind::TupleVariant | DefKind::StructVariant => SymbolKind::EnumMember,
    }
}

/// What kind of completion is this racer match type?
pub fn completion_kind_from_match_type(m: racer::MatchType) -> CompletionItemKind {
    match m {
        racer::MatchType::Crate | racer::MatchType::Module => CompletionItemKind::Module,
        racer::MatchType::Struct(_) => CompletionItemKind::Class,
        racer::MatchType::Enum(_) => CompletionItemKind::Enum,
        racer::MatchType::StructField | racer::MatchType::EnumVariant(_) => {
            CompletionItemKind::Field
        }
        racer::MatchType::Macro
        | racer::MatchType::Function
        | racer::MatchType::Method(_)
        | racer::MatchType::FnArg(_) => CompletionItemKind::Function,
        racer::MatchType::Type | racer::MatchType::Trait => {
            CompletionItemKind::Interface
        }
        racer::MatchType::Let(_)
        | racer::MatchType::IfLet(_)
        | racer::MatchType::WhileLet(_)
        | racer::MatchType::For(_)
        | racer::MatchType::MatchArm
        | racer::MatchType::Const
        | racer::MatchType::Static => CompletionItemKind::Variable,
        racer::MatchType::TypeParameter(_) => CompletionItemKind::TypeParameter,
        racer::MatchType::Builtin(_) => CompletionItemKind::Keyword,
        racer::MatchType::UseAlias(m) => match m.mtype {
            racer::MatchType::UseAlias(_) => unreachable!("Nested use aliases"),
            typ => completion_kind_from_match_type(typ),
        }
        racer::MatchType::AssocType => CompletionItemKind::TypeParameter,
    }
}

/// Convert a racer match into an RLS completion.
pub fn completion_item_from_racer_match(m: &racer::Match) -> CompletionItem {
    let mut item = CompletionItem::new_simple(m.matchstr.clone(), m.contextstr.clone());
    item.kind = Some(completion_kind_from_match_type(m.mtype.clone()));

    if !m.docs.is_empty() {
        item.documentation = Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover::process_docs(&m.docs),
        }));
    }

    item
}

/* ------  Extension methods for JSON-RPC protocol types ------ */

/// Provide additional methods for the remote `Range` type
pub trait RangeExt {
    /// Do both Ranges overlap?
    fn overlaps(&self, other: &Self) -> bool;
}

impl RangeExt for Range {
    fn overlaps(&self, other: &Self) -> bool {
        self.start <= other.end && other.start <= self.end
    }
}

/// `DidChangeConfigurationParams.settings` payload reading the { rust: {...} } bit.
#[derive(Debug, Deserialize)]
pub struct ChangeConfigSettings {
    pub rust: config::Config,
}

/* -----------------  JSON-RPC protocol types ----------------- */

/// Supported initialization options that can be passed in the `initialize`
/// request, under `initialization_options` key. These are specific to the RLS.
#[derive(Debug, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct InitializationOptions {
    /// Should the build not be triggered immediately after receiving `initialize`
    pub omit_init_build: bool,
    pub cmd_run: bool,
    /// `DidChangeConfigurationParams.settings` payload for upfront configuration.
    pub settings: Option<ChangeConfigSettings>,
}

impl Default for InitializationOptions {
    fn default() -> Self {
        InitializationOptions {
            omit_init_build: false,
            cmd_run: false,
            settings: None,
        }
    }
}

// Subset of flags from ls_types::ClientCapabilities that affects this RLS.
// Passed in the `initialize` request under `capabilities`.
#[derive(Debug, PartialEq, Deserialize, Serialize, Clone, Copy, Default)]
#[serde(default)]
pub struct ClientCapabilities {
    pub code_completion_has_snippet_support: bool,
    pub related_information_support: bool,
}

impl ClientCapabilities {
    pub fn new(params: &ls_types::InitializeParams) -> ClientCapabilities {
        // ls_types::ClientCapabilities is a rather awkward object to use internally
        // (for instance it doesn't Clone). Instead we pick out the bits of it that we
        // are going to handle into ClientCapabilities. The upside of
        // using this very simple struct is that it can be kept thread safe
        // without mutex locking it on every request.
        let code_completion_has_snippet_support = params
            .capabilities
            .text_document
            .as_ref()
            .and_then(|doc| doc.completion.as_ref())
            .and_then(|comp| comp.completion_item.as_ref())
            .and_then(|item| item.snippet_support.as_ref())
            .unwrap_or(&false)
            .to_owned();

        let related_information_support = params
            .capabilities
            .text_document
            .as_ref()
            .and_then(|doc| doc.publish_diagnostics.as_ref())
            .and_then(|diag| diag.related_information.as_ref())
            .unwrap_or(&false)
            .to_owned();

        ClientCapabilities {
            code_completion_has_snippet_support,
            related_information_support,
        }
    }
}

/* --------------- Custom JSON-RPC notifications -------------- */

/// Custom LSP notification sent to client indicating that the server is currently
/// processing data and may publish new diagnostics on `rustDocument/diagnosticsEnd`.
#[derive(Debug)]
pub enum DiagnosticsBegin {}

impl LSPNotification for DiagnosticsBegin {
    type Params = ();
    const METHOD: &'static str = "rustDocument/diagnosticsBegin";
}

/// Custom LSP notification sent to client indicating that data processing started
/// by a `rustDocument`/diagnosticsBegin` has ended.
/// For each `diagnosticsBegin` message, there is a single `diagnosticsEnd` message.
/// This means that for multiple active `diagnosticsBegin` messages, there will
/// be sent multiple `diagnosticsEnd` notifications.
#[derive(Debug)]
pub enum DiagnosticsEnd {}

impl LSPNotification for DiagnosticsEnd {
    type Params = ();
    const METHOD: &'static str = "rustDocument/diagnosticsEnd";
}

/// Custom LSP notification sent to client indicating that a build process has begun.
#[derive(Debug)]
pub enum BeginBuild {}

impl LSPNotification for BeginBuild {
    type Params = ();
    const METHOD: &'static str = "rustDocument/beginBuild";
}

/* ----------  Temporary LSP type until window/progress proposal is done --------- */

// Notification from server to client for build progress.
#[derive(Debug)]
pub struct Progress;

impl notification::Notification for Progress {
    type Params = ProgressParams;
    const METHOD: &'static str = NOTIFICATION__Progress;
}

/**
 * The progress notification is sent from the server to the client to ask the client
 * to indicate progress.
 */
#[allow(non_upper_case_globals)]
pub const NOTIFICATION__Progress: &str = "window/progress";

#[derive(Debug, PartialEq, Deserialize, Serialize, Clone)]
pub struct ProgressParams {
    // A unique identifier to associate multiple progress notifications with the same progress.
    pub id: String,

    // The title of the progress.
    // This should be the same for all ProgressParams with the same id.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,

    // Optional progress message to display.
    // If unset, the previous progress message (if any) is still valid.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,

    // Optional progress percentage to display.
    // If unset, the previous progress percentage (if any) is still valid.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub percentage: Option<f64>,

    // Set to true on the final progress update.
    // No more progress notifications with the same ID should be sent.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub done: Option<bool>,
}
