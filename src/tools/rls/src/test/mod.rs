// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
#[macro_use]
mod harness;
mod lens;

use crate::actions::{notifications, requests};
use crate::config::{Config, Inferrable};
use crate::server::{self as ls_server, Notification, Request, RequestId, ShutdownRequest};
use jsonrpc_core;
use rls_analysis::{AnalysisHost, Target};
use rls_vfs::Vfs;
use serde_json::Value;

use self::harness::{
    compare_json, expect_message, expect_series, src, Environment, ExpectedMessage, RecordOutput,
};

use languageserver_types::*;

use env_logger;
use serde_json;
use std::marker::PhantomData;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::time::Instant;
use url::Url;

pub use self::harness::FIXTURES_DIR;

fn initialize(id: usize, root_path: Option<String>) -> Request<ls_server::InitializeRequest> {
    initialize_with_opts(id, root_path, None)
}

fn initialize_with_opts(
    id: usize,
    root_path: Option<String>,
    initialization_options: Option<serde_json::Value>,
) -> Request<ls_server::InitializeRequest> {
    let params = InitializeParams {
        process_id: None,
        root_path,
        root_uri: None,
        initialization_options,
        capabilities: ClientCapabilities {
            workspace: None,
            text_document: None,
            experimental: None,
        },
        trace: Some(TraceOption::Off),
        workspace_folders: None,
    };
    Request {
        id: RequestId::Num(id as u64),
        params,
        received: Instant::now(),
        _action: PhantomData,
    }
}

fn blocking_request<T: ls_server::BlockingRequestAction>(
    id: usize,
    params: T::Params,
) -> Request<T> {
    Request {
        id: RequestId::Num(id as u64),
        params,
        received: Instant::now(),
        _action: PhantomData,
    }
}

fn request<T: ls_server::RequestAction>(id: usize, params: T::Params) -> Request<T> {
    Request {
        id: RequestId::Num(id as u64),
        params,
        received: Instant::now(),
        _action: PhantomData,
    }
}

fn notification<A: ls_server::BlockingNotificationAction>(params: A::Params) -> Notification<A> {
    Notification {
        params,
        _action: PhantomData,
    }
}

#[test]
fn test_shutdown() {
    let mut env = Environment::generate_from_fixture("common");

    let root_path = env.cache.abs_path(Path::new("."));

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        blocking_request::<ShutdownRequest>(1, ()).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);;

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(&mut server, results, &ExpectedMessage::new(Some(1)));
}

#[test]
fn test_goto_def() {
    let mut env = Environment::generate_from_fixture("common");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::Definition>(
            11,
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(url),
                position: env
                    .cache
                    .mk_ls_position(src(&source_file_path, 22, "world")),
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);;

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    // TODO structural checking of result, rather than looking for a string - src(&source_file_path, 12, "world")
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(11)).expect_contains(r#""start":{"line":20,"character":8}"#),
    );
}

#[test]
fn test_hover() {
    let mut env = Environment::generate_from_fixture("common");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::Hover>(
            11,
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(url),
                position: env
                    .cache
                    .mk_ls_position(src(&source_file_path, 22, "world")),
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);;

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(11))
                .expect_contains(r#"[{"language":"rust","value":"&str"},{"language":"rust","value":"let world = \"world\";"}]"#)
    );
}

/// Test hover continues to work after the source has moved line
#[test]
fn test_hover_after_src_line_change() {
    let mut env = Environment::generate_from_fixture("common");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    let world_src_pos = env
        .cache
        .mk_ls_position(src(&source_file_path, 21, "world"));
    let world_src_pos_after = Position {
        line: world_src_pos.line + 1,
        ..world_src_pos
    };

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::Hover>(
            11,
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(url.clone()),
                position: world_src_pos,
            },
        ).to_string(),
        notification::<notifications::DidChangeTextDocument>(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: url.clone(),
                version: Some(2),
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: Some(Range {
                    start: Position {
                        line: 19,
                        character: 15,
                    },
                    end: Position {
                        line: 19,
                        character: 15,
                    },
                }),
                range_length: Some(0),
                text: "\n    ".into(),
            }],
        }).to_string(),
        request::<requests::Hover>(
            13,
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(url),
                position: world_src_pos_after,
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    // first hover over unmodified
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(11)).expect_contains(r#"[{"language":"rust","value":"&str"}]"#),
    );

    // handle didChange notification and wait for rebuild
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    // hover after line change should work at the new line
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(None).expect_contains(r#"[{"language":"rust","value":"&str"}]"#),
    );
}

#[test]
fn test_workspace_symbol() {
    let mut env = Environment::generate_from_fixture("workspace_symbol");

    let root_path = env.cache.abs_path(Path::new("."));

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::WorkspaceSymbol>(
            42,
            WorkspaceSymbolParams {
                query: "nemo".to_owned(),
            },
        ).to_string(),
    ];

    env.with_config(|c| c.cfg_test = true);
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(42)).expect_contains(r#""id":42"#)
            // in main.rs
            .expect_contains(r#"main.rs"#)
            .expect_contains(r#""name":"nemo""#)
            .expect_contains(r#""kind":12"#)
            .expect_contains(r#""range":{"start":{"line":11,"character":11},"end":{"line":11,"character":15}}"#)
            .expect_contains(r#""containerName":"x""#)

            // in foo.rs
            .expect_contains(r#"foo.rs"#)
            .expect_contains(r#""name":"nemo""#)
            .expect_contains(r#""kind":2"#)
            .expect_contains(r#""range":{"start":{"line":0,"character":4},"end":{"line":0,"character":8}}"#)
            .expect_contains(r#""containerName":"foo""#),
    );
}

#[test]
fn test_workspace_symbol_duplicates() {
    let mut env = Environment::generate_from_fixture("workspace_symbol_duplicates");

    let root_path = env.cache.abs_path(Path::new("."));

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::WorkspaceSymbol>(
            42,
            WorkspaceSymbolParams {
                query: "Frobnicator".to_owned(),
            },
        ).to_string(),
    ];

    env.with_config(|c| c.cfg_test = true);
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    server.wait_for_concurrent_jobs();
    let result: serde_json::Value =
        serde_json::from_str(&results.lock().unwrap().remove(0)).unwrap();
    let mut result = result.get("result").unwrap().clone();
    *result.pointer_mut("/0/location/uri").unwrap() = "shared.rs".into();
    compare_json(
        &result,
        r#"[{
            "containerName": "a",
            "kind": 23,
            "location": {
              "range": {
                "end": { "line": 11, "character": 18 },
                "start": { "line": 11, "character": 7 }
              },
              "uri": "shared.rs"
            },
            "name": "Frobnicator"
        }]"#,
    )
}

#[test]
fn test_find_all_refs() {
    let mut env = Environment::generate_from_fixture("common");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::References>(
            42,
            ReferenceParams {
                text_document: TextDocumentIdentifier::new(url),
                position: env.cache.mk_ls_position(src(&source_file_path, 10, "Bar")),
                context: ReferenceContext {
                    include_declaration: true,
                },
            },
        ).to_string(),
    ];

    env.with_config(|c| c.cfg_test = true);
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(42))
            .expect_contains(
                r#"{"start":{"line":9,"character":7},"end":{"line":9,"character":10}}"#,
            ).expect_contains(
                r#"{"start":{"line":15,"character":14},"end":{"line":15,"character":17}}"#,
            ).expect_contains(
                r#"{"start":{"line":23,"character":15},"end":{"line":23,"character":18}}"#,
            ),
    );
}

#[test]
fn test_find_all_refs_no_cfg_test() {
    let mut env = Environment::generate_from_fixture("find_all_refs_no_cfg_test");
    env.with_config(|c| c.all_targets = false);

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::References>(
            42,
            ReferenceParams {
                text_document: TextDocumentIdentifier::new(url),
                position: env.cache.mk_ls_position(src(&source_file_path, 10, "Bar")),
                context: ReferenceContext {
                    include_declaration: true,
                },
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(42))
            .expect_contains(
                r#"{"start":{"line":9,"character":7},"end":{"line":9,"character":10}}"#,
            ).expect_contains(
                r#"{"start":{"line":22,"character":15},"end":{"line":22,"character":18}}"#,
            ),
    );
}

#[test]
fn test_borrow_error() {
    let mut env = Environment::generate_from_fixture("borrow_error");

    let root_path = env.cache.abs_path(Path::new("."));
    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None)
            .expect_contains(r#""message":"cannot borrow `x` as mutable more than once at a time"#),
    );

    expect_series(&mut server, results, vec!["progress"]);
}

#[test]
fn test_highlight() {
    let mut env = Environment::generate_from_fixture("common");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::DocumentHighlight>(
            42,
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(url),
                position: env
                    .cache
                    .mk_ls_position(src(&source_file_path, 22, "world")),
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(42))
            .expect_contains(
                r#"{"start":{"line":20,"character":8},"end":{"line":20,"character":13}}"#,
            ).expect_contains(
                r#"{"start":{"line":21,"character":27},"end":{"line":21,"character":32}}"#,
            ),
    );
}

#[test]
fn test_rename() {
    let mut env = Environment::generate_from_fixture("common");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");
    let text_doc = TextDocumentIdentifier::new(url);
    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::Rename>(
            42,
            RenameParams {
                text_document: text_doc,
                position: env
                    .cache
                    .mk_ls_position(src(&source_file_path, 22, "world")),
                new_name: "foo".to_owned(),
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(42))
            .expect_contains(
                r#"{"start":{"line":20,"character":8},"end":{"line":20,"character":13}}"#,
            ).expect_contains(
                r#"{"start":{"line":21,"character":27},"end":{"line":21,"character":32}}"#,
            ).expect_contains(r#"{"changes""#),
    );
}

#[test]
fn test_reformat() {
    let mut env = Environment::generate_from_fixture("reformat");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");
    let text_doc = TextDocumentIdentifier::new(url);
    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::Formatting>(
            42,
            DocumentFormattingParams {
                text_document: text_doc,
                options: FormattingOptions {
                    tab_size: 4,
                    insert_spaces: true,
                    properties: ::std::collections::HashMap::new(),
                },
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(42))
            .expect_contains(r#"{"start":{"line":0,"character":0},"end":{"line":12,"character":0}}"#)
            .expect_contains(r#"newText":"// Copyright 2017 The Rust Project Developers. See the COPYRIGHT\n// file at the top-level directory of this distribution and at\n// http://rust-lang.org/COPYRIGHT.\n//\n// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or\n// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license\n// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your\n// option. This file may not be copied, modified, or distributed\n// except according to those terms.\n\npub mod foo;\npub fn main() {\n    let world = \"world\";\n    println!(\"Hello, {}!\", world);\n}"#)
        );
}

#[test]
fn test_reformat_with_range() {
    let mut env = Environment::generate_from_fixture("reformat_with_range");
    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");
    let text_doc = TextDocumentIdentifier::new(url);
    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::RangeFormatting>(
            42,
            DocumentRangeFormattingParams {
                text_document: text_doc,
                range: Range {
                    start: Position {
                        line: 12,
                        character: 0,
                    },
                    end: Position {
                        line: 13,
                        character: 0,
                    },
                },
                options: FormattingOptions {
                    tab_size: 4,
                    insert_spaces: true,
                    properties: ::std::collections::HashMap::new(),
                },
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);

    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    let newline = if cfg!(windows) { r#"\r\n"# } else { r#"\n"# };
    let formatted = r#"newText":"// Copyright 2017 The Rust Project Developers. See the COPYRIGHT\n// file at the top-level directory of this distribution and at\n// http://rust-lang.org/COPYRIGHT.\n//\n// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or\n// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license\n// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your\n// option. This file may not be copied, modified, or distributed\n// except according to those terms.\n\npub fn main() {\n    let world1 = \"world\";\n    println!(\"Hello, {}!\", world1);\n    let world2 = \"world\";\n    println!(\"Hello, {}!\", world2);\n    let world3 = \"world\";\n    println!(\"Hello, {}!\", world3);\n}\n"#;
    expect_message(&mut server, results,
        ExpectedMessage::new(Some(42)).expect_contains(r#"{"start":{"line":0,"character":0},"end":{"line":15,"character":5}}"#)
            .expect_contains(&formatted.replace(r#"\n"#, newline))
    );
}

#[test]
fn test_multiple_binaries() {
    let mut env = Environment::generate_from_fixture("multiple_bins");

    let root_path = env.cache.abs_path(Path::new("."));
    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    env.with_config(|c| c.build_bin = Inferrable::Specified(Some("bin2".to_owned())));
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    // These messages should be about bin_name1 and bin_name2, but the order is
    // not deterministic FIXME(#606)
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None).expect_contains("unused variable: `bin_name"),
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None).expect_contains("unused variable: `bin_name"),
    );
    expect_series(&mut server, results, vec!["progress"]);
}

// FIXME Requires rust-src component, which would break Rust CI
// #[test]
// fn test_completion() {
//     let mut env = Environment::generate_from_fixture("common");

//     let source_file_path = Path::new("src").join("main.rs");

//     let root_path = env.cache.abs_path(Path::new("."));
//     let url = Url::from_file_path(env.cache.abs_path(&source_file_path)).expect("couldn't convert file path to URL");
//     let text_doc = TextDocumentIdentifier::new(url);

//     let messages = vec![
//         initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
//         request::<requests::Completion>(11, TextDocumentPositionParams {
//             text_document: text_doc.clone(),
//             position: env.cache.mk_ls_position(src(&source_file_path, 22, "rld"))
//         }).to_string(),
//         request::<requests::Completion>(22, TextDocumentPositionParams {
//             text_document: text_doc.clone(),
//             position: env.cache.mk_ls_position(src(&source_file_path, 25, "x)"))
//         }).to_string(),
//     ];

//     let (mut server, results, ..) = env.mock_server(messages);
//     // Initialize and build.
//     assert_eq!(ls_server::LsService::handle_message(&mut server),
//                ls_server::ServerStateChange::Continue);
//     expect_message(results.clone(), &[ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
//                                        ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#"title":"Indexing""#),
//                                        ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#""done":true"#)]);

//     assert_eq!(ls_server::LsService::handle_message(&mut server),
//                ls_server::ServerStateChange::Continue);
//     expect_message(results.clone(), &[ExpectedMessage::new(Some(11)).expect_contains(r#"[{"label":"world","kind":6,"detail":"let world = \"world\";"}]"#)]);

//     assert_eq!(ls_server::LsService::handle_message(&mut server),
//                ls_server::ServerStateChange::Continue);
//     expect_message(results.clone(), &[ExpectedMessage::new(Some(22)).expect_contains(r#"{"label":"x","kind":5,"detail":"u64"#)]);
// }

#[test]
fn test_bin_lib_project() {
    let mut env = Environment::generate_from_fixture("bin_lib");

    let root_path = env.cache.abs_path(Path::new("."));

    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    env.with_config(|c| {
        c.cfg_test = true;
        c.build_bin = Inferrable::Specified(Some("bin_lib".into()));
    });
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None)
            .expect_contains(r#"bin_lib/tests/tests.rs"#)
            .expect_contains(r#"unused variable: `unused_var`"#),
    );

    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(None)
            .expect_contains("progress")
            .expect_contains(r#""done":true"#),
    );
}

// FIXME(#524) timing issues when run concurrently with `test_bin_lib_project`
// #[test]
// fn test_bin_lib_project_no_cfg_test() {
//     let mut env = Environment::generate_from_fixture("bin_lib");

//     let root_path = env.cache.abs_path(Path::new("."));

//     let messages = vec![
//         initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
//     ];

//     env.with_config(|c| {
//         c.build_lib = Inferrable::Specified(false);
//         c.build_bin = Inferrable::Specified(Some("bin_lib".into()));
//     });
//     let (mut server, results, ..) = env.mock_server(messages);
//     // Initialize and build.
//     assert_eq!(ls_server::LsService::handle_message(&mut server),
//                ls_server::ServerStateChange::Continue);
//     expect_message(results.clone(), &[ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
//                                        ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#"title":"Indexing""#),
//                                        ExpectedMessage::new(None).expect_contains("cannot find struct, variant or union type `LibCfgTestStruct` in module `bin_lib`"),
//                                        ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#""done":true"#)]);
// }

// FIXME(#455) reinstate this test
// #[test]
// fn test_simple_workspace() {
//     let mut env = Environment::generate_from_fixture("simple_workspace");

//     let root_path = env.cache.abs_path(Path::new("."));

//     let messages = vec![
//         initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
//     ];

//     let (mut server, results, ..) = env.mock_server(messages);
//     // Initialize and build.
//     assert_eq!(ls_server::LsService::handle_message(&mut server),
//                ls_server::ServerStateChange::Continue);
//     expect_message(results.clone(), &[ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
//                                        ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#"title":"Indexing""#),
//                                        // TODO: Ideally we should check for message contents for different crates/targets,
//                                        // however order of received messages is non-deterministic and this
//                                        // would require implementing something like `or_expect_contains`
//                                        ExpectedMessage::new(None).expect_contains("publishIndexing"),
//                                        ExpectedMessage::new(None).expect_contains("publishIndexing"),
//                                        ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#""done":true"#)]);
// }

#[test]
fn test_infer_lib() {
    let mut env = Environment::generate_from_fixture("infer_lib");

    let root_path = env.cache.abs_path(Path::new("."));
    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None).expect_contains("struct is never constructed: `UnusedLib`"),
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(None)
            .expect_contains("progress")
            .expect_contains(r#""done":true"#),
    );
}

#[test]
fn test_infer_bin() {
    let mut env = Environment::generate_from_fixture("infer_bin");

    let root_path = env.cache.abs_path(Path::new("."));
    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None).expect_contains("struct is never constructed: `UnusedBin`"),
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(None)
            .expect_contains("progress")
            .expect_contains(r#""done":true"#),
    );
}

#[test]
fn test_infer_custom_bin() {
    let mut env = Environment::generate_from_fixture("infer_custom_bin");

    let root_path = env.cache.abs_path(Path::new("."));
    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None)
            .expect_contains("struct is never constructed: `UnusedCustomBin`"),
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(None)
            .expect_contains("progress")
            .expect_contains(r#""done":true"#),
    );
}

#[test]
fn test_omit_init_build() {
    use serde_json::json;

    let mut env = Environment::generate_from_fixture("common");

    let root_path = env.cache.abs_path(Path::new("."));
    let root_path = root_path.as_os_str().to_str().map(|x| x.to_owned());
    let init_options = json!({
        "omitInitBuild": true,
        "cmdRun": true
    });
    let initialize = initialize_with_opts(0, root_path, Some(init_options));

    let (mut server, results, ..) = env.mock_server(vec![initialize.to_string()]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );
}

#[test]
fn test_init_with_configuration() {
    use serde_json::json;

    let mut env = Environment::generate_from_fixture("common");

    let root_path = env.cache.abs_path(Path::new("."));
    let root_path = root_path.as_os_str().to_str().map(|x| x.to_owned());
    let init_options = json!({
        "settings": {
            "rust": {
                "features": ["some_feature"],
                "all_targets": false
            }
        }
    });

    let initialize = initialize_with_opts(0, root_path, Some(init_options));

    let (mut server, results, config) = env.mock_server(vec![initialize.to_string()]);

    assert!(
        &config.lock().unwrap().features.is_empty(),
        "Default config should have no explicit features enabled"
    );

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    assert_eq!(&config.lock().unwrap().features, &["some_feature"]);
    assert_eq!(config.lock().unwrap().all_targets, false);
}

#[test]
fn test_parse_error_on_malformed_input() {
    let _ = env_logger::try_init();
    struct NoneMsgReader;

    impl ls_server::MessageReader for NoneMsgReader {
        fn read_message(&self) -> Option<String> {
            None
        }
    }

    let analysis = Arc::new(AnalysisHost::new(Target::Debug));
    let vfs = Arc::new(Vfs::new());
    let reader = Box::new(NoneMsgReader);
    let output = RecordOutput::new();
    let results = output.output.clone();
    let mut server = ls_server::LsService::new(
        analysis,
        vfs,
        Arc::new(Mutex::new(Config::default())),
        reader,
        output,
    );

    let result = ls_server::LsService::handle_message(&mut server);
    assert_eq!(
        result,
        ls_server::ServerStateChange::Break { exit_code: 101 }
    );

    let error = results.lock().unwrap().pop().expect("no error response");

    let failure: jsonrpc_core::Failure =
        serde_json::from_str(&error).expect("Couldn't parse json failure response");

    assert!(failure.error.code == jsonrpc_core::ErrorCode::ParseError);
}

#[test]
fn test_find_impls() {
    let mut env = Environment::generate_from_fixture("find_impls");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    // This test contains code for testing implementations of `Eq`. However, `rust-analysis` is not
    // installed on Travis making rls-analysis fail why retrieving the typeid. Installing
    // `rust-analysis` is also not an option, because this makes other test timeout.
    // e.g., https://travis-ci.org/rust-lang/rls/jobs/265339002

    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        request::<requests::Implementation>(
            1,
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(url.clone()),
                position: env.cache.mk_ls_position(src(&source_file_path, 13, "Bar")),
            },
        ).to_string(),
        request::<requests::Implementation>(
            2,
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(url),
                position: env
                    .cache
                    .mk_ls_position(src(&source_file_path, 16, "Super")),
            },
        ).to_string(),
        // FIXME Does not work on Travis
        // request::<requests::Implementation>(
        //     3,
        //     TextDocumentPositionParams {
        //         text_document: TextDocumentIdentifier::new(url),
        //         position: env.cache.mk_ls_position(src(&source_file_path, 20, "Eq")),
        //     },
        //     ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    // TODO structural checking of result, rather than looking for a string - src(&source_file_path, 12, "world")
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(1))
            .expect_contains(
                r#""range":{"start":{"line":18,"character":15},"end":{"line":18,"character":18}}"#,
            ).expect_contains(
                r#""range":{"start":{"line":19,"character":12},"end":{"line":19,"character":15}}"#,
            ),
    );
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(2))
            .expect_contains(
                r#""range":{"start":{"line":18,"character":15},"end":{"line":18,"character":18}}"#,
            ).expect_contains(
                r#""range":{"start":{"line":22,"character":15},"end":{"line":22,"character":18}}"#,
            ),
    );
    // FIXME Does not work on Travis
    // assert_eq!(ls_server::LsService::handle_message(&mut server),
    //            ls_server::ServerStateChange::Continue);
    // expect_message(results.clone(), &[
    //     // TODO assert that only one position is returned
    //     ExpectedMessage::new(Some(3))
    //         .expect_contains(r#""range":{"start":{"line":19,"character":12},"end":{"line":19,"character":15}}"#)
    // ]);
}

#[test]
fn test_features() {
    let mut env = Environment::generate_from_fixture("features");

    let root_path = env.cache.abs_path(Path::new("."));
    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    env.with_config(|c| c.features = vec!["foo".to_owned()]);
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None).expect_contains(
            r#""message":"cannot find struct, variant or union type `Bar` in this scope"#,
        ),
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(None)
            .expect_contains("progress")
            .expect_contains(r#""done":true"#),
    );
}

#[test]
fn test_all_features() {
    let mut env = Environment::generate_from_fixture("features");

    let root_path = env.cache.abs_path(Path::new("."));
    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    env.with_config(|c| c.all_features = true);
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results, vec!["progress"]);
}

#[test]
fn test_no_default_features() {
    let mut env = Environment::generate_from_fixture("features");

    let root_path = env.cache.abs_path(Path::new("."));
    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    env.with_config(|c| {
        c.no_default_features = true;
        c.features = vec!["foo".to_owned(), "bar".to_owned()]
    });
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None).expect_contains(
            r#""message":"cannot find struct, variant or union type `Baz` in this scope"#,
        ),
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(None)
            .expect_contains("progress")
            .expect_contains(r#""done":true"#),
    );
}

// #[test]
// fn test_handle_utf8_directory() {
//     let mut env = Environment::generate_from_fixture("unicødë");
//
//     let root_path = env.cache.abs_path(Path::new("."));
//     let root_url = Url::from_directory_path(&root_path).unwrap();
//     let messages = vec![
//         initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()
//     ];
//
//     let (mut server, results, ..) = env.mock_server(messages);
//     // Initialize and build.
//     assert_eq!(ls_server::LsService::handle_message(&mut server),
//                ls_server::ServerStateChange::Continue);
//     expect_message(results.clone(), &[ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
//                                        ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#"title":"Indexing""#),
//                                        ExpectedMessage::new(None)
//                                            .expect_contains(root_url.path())
//                                            .expect_contains("struct is never used: `Unused`"),
//                                        ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#""done":true"#)]);
// }

#[test]
fn test_deglob() {
    let mut env = Environment::generate_from_fixture("deglob");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");
    let text_doc = TextDocumentIdentifier::new(url.clone());
    let messages = vec![
        initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
        // request deglob for single wildcard
        request::<requests::CodeAction>(
            100,
            CodeActionParams {
                text_document: text_doc.clone(),
                range: env.cache.mk_ls_range_from_line(12),
                context: CodeActionContext {
                    diagnostics: vec![],
                    only: None,
                },
            },
        ).to_string(),
        // deglob single
        request::<requests::ExecuteCommand>(
            200,
            ExecuteCommandParams {
                command: format!("rls.deglobImports-{}", ::std::process::id()),
                arguments: vec![
                    serde_json::to_value(&requests::DeglobResult {
                        location: Location {
                            uri: url.clone(),
                            range: Range::new(Position::new(12, 13), Position::new(12, 14)),
                        },
                        new_text: "{Stdout, Stdin}".into(),
                    }).unwrap(),
                ],
            },
        ).to_string(),
        // request deglob for double wildcard
        request::<requests::CodeAction>(
            1100,
            CodeActionParams {
                text_document: text_doc,
                range: env.cache.mk_ls_range_from_line(15),
                context: CodeActionContext {
                    diagnostics: vec![],
                    only: None,
                },
            },
        ).to_string(),
        // deglob two wildcards
        request::<requests::ExecuteCommand>(
            1200,
            ExecuteCommandParams {
                command: format!("rls.deglobImports-{}", ::std::process::id()),
                arguments: vec![
                    serde_json::to_value(&requests::DeglobResult {
                        location: Location {
                            uri: url.clone(),
                            range: Range::new(Position::new(15, 14), Position::new(15, 15)),
                        },
                        new_text: "size_of".into(),
                    }).unwrap(),
                    serde_json::to_value(&requests::DeglobResult {
                        location: Location {
                            uri: url,
                            range: Range::new(Position::new(15, 31), Position::new(15, 32)),
                        },
                        new_text: "max".into(),
                    }).unwrap(),
                ],
            },
        ).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("rls.deglobImports-"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    {
        server.wait_for_concurrent_jobs();
        let response: Value = serde_json::from_str(&results.lock().unwrap().remove(0)).unwrap();
        assert_eq!(response["id"], 100);
        assert_eq!(response["result"][0]["title"], "Deglob import");
        assert_eq!(
            response["result"][0]["command"],
            &*format!("rls.deglobImports-{}", ::std::process::id())
        );
        let deglob = &response["result"][0]["arguments"][0];
        assert!(
            deglob["location"]["uri"]
                .as_str()
                .unwrap()
                .ends_with("deglob/src/main.rs")
        );
        let deglob_loc = &deglob["location"]["range"];
        assert_eq!(deglob_loc["start"]["line"], 12);
        assert_eq!(deglob_loc["start"]["character"], 13);
        assert_eq!(deglob_loc["end"]["line"], 12);
        assert_eq!(deglob_loc["end"]["character"], 14);
        let mut imports: Vec<_> = deglob["new_text"]
            .as_str()
            .unwrap()
            .trim_matches('{')
            .trim_matches('}')
            .split(", ")
            .collect();
        imports.sort();
        assert_eq!(imports, vec!["Stdin", "Stdout"]);
    }

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    {
        server.wait_for_concurrent_jobs();
        let response: Value = serde_json::from_str(&results.lock().unwrap().remove(0)).unwrap();
        assert_eq!(response["id"], 0x0100_0001);
        assert_eq!(response["method"], "workspace/applyEdit");
        let (key, changes) = response["params"]["edit"]["changes"]
            .as_object()
            .unwrap()
            .iter()
            .next()
            .unwrap();
        assert!(key.ends_with("deglob/src/main.rs"));
        let change = &changes[0];
        assert_eq!(change["range"]["start"]["line"], 12);
        assert_eq!(change["range"]["start"]["character"], 13);
        assert_eq!(change["range"]["end"]["line"], 12);
        assert_eq!(change["range"]["end"]["character"], 14);
        let mut imports: Vec<_> = change["newText"]
            .as_str()
            .expect("newText missing")
            .trim_matches('{')
            .trim_matches('}')
            .split(", ")
            .collect();
        imports.sort();
        assert_eq!(imports, vec!["Stdin", "Stdout"]);

        let response: Value = serde_json::from_str(&results.lock().unwrap().remove(0)).unwrap();
        assert_eq!(response["id"], 200);
        assert!(response["result"].is_null());
    }

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(1100))
            .expect_contains(r#""title":"Deglob imports""#)
            .expect_contains(r#""command":"rls.deglobImports-"#)
            .expect_contains(r#"{"location":{"range":{"end":{"character":15,"line":15},"start":{"character":14,"line":15}},"uri":"#)
            .expect_contains(r#"deglob/src/main.rs"}"#)
            .expect_contains(r#""new_text":"size_of""#)
            .expect_contains(r#"{"location":{"range":{"end":{"character":32,"line":15},"start":{"character":31,"line":15}},"uri":"#)
            .expect_contains(r#"deglob/src/main.rs"}"#)
            .expect_contains(r#""new_text":"max""#)
    );

    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    {
        server.wait_for_concurrent_jobs();
        let response: Value = serde_json::from_str(&results.lock().unwrap().remove(0)).unwrap();
        assert_eq!(response["id"], 0x0100_0002);
        assert_eq!(response["method"], "workspace/applyEdit");
        let (key, changes) = response["params"]["edit"]["changes"]
            .as_object()
            .unwrap()
            .iter()
            .next()
            .unwrap();
        assert!(key.ends_with("deglob/src/main.rs"));
        let change = &changes[0];
        assert_eq!(change["range"]["start"]["line"], 15);
        assert_eq!(change["range"]["start"]["character"], 14);
        assert_eq!(change["range"]["end"]["line"], 15);
        assert_eq!(change["range"]["end"]["character"], 15);
        assert_eq!(change["newText"], "size_of");
        let change = &changes[1];
        assert_eq!(change["range"]["start"]["line"], 15);
        assert_eq!(change["range"]["start"]["character"], 31);
        assert_eq!(change["range"]["end"]["line"], 15);
        assert_eq!(change["range"]["end"]["character"], 32);
        assert_eq!(change["newText"], "max");
    }

    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(Some(1200)).expect_contains(r#"null"#),
    );
}

#[test]
fn test_all_targets() {
    let mut env = Environment::generate_from_fixture("bin_lib");

    let root_path = env.cache.abs_path(Path::new("."));

    let messages =
        vec![initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string()];

    env.with_config(|c| {
        c.all_targets = true;
        c.cfg_test = true;
    });
    let (mut server, results, ..) = env.mock_server(messages);
    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results.clone(), vec!["progress"]);

    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(None)
            .expect_contains(r#"bin_lib/tests/tests.rs"#)
            .expect_contains(r#"unused variable: `unused_var`"#),
    );
    expect_message(
        &mut server,
        results,
        ExpectedMessage::new(None)
            .expect_contains("progress")
            .expect_contains(r#""done":true"#),
    );
}

/// Handle receiving a notification before the `initialize` request by ignoring and
/// continuing to run
#[test]
fn ignore_uninitialized_notification() {
    let mut env = Environment::generate_from_fixture("common");

    let source_file_path = Path::new("src").join("main.rs");

    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    let messages = vec![
        notification::<notifications::DidChangeTextDocument>(DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: url,
                version: Some(2),
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: Some(Range {
                    start: Position {
                        line: 19,
                        character: 15,
                    },
                    end: Position {
                        line: 19,
                        character: 15,
                    },
                }),
                range_length: Some(0),
                text: "\n    ".into(),
            }],
        }).to_string(),
        initialize(1, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);

    // Ignore notification
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );

    // Initialize and build
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(1)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results, vec!["progress"]);
}

/// Handle receiving requests before the `initialize` request by returning an error response
/// and continuing to run
#[test]
fn fail_uninitialized_request() {
    let mut env = Environment::generate_from_fixture("common");

    let source_file_path = Path::new("src").join("main.rs");
    let root_path = env.cache.abs_path(Path::new("."));
    let url = Url::from_file_path(env.cache.abs_path(&source_file_path))
        .expect("couldn't convert file path to URL");

    let messages = vec![
        request::<requests::Definition>(
            0,
            TextDocumentPositionParams {
                text_document: TextDocumentIdentifier::new(url),
                position: env
                    .cache
                    .mk_ls_position(src(&source_file_path, 22, "world")),
            },
        ).to_string(),
        initialize(1, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
    ];

    let (mut server, results, ..) = env.mock_server(messages);

    // Return error response to pre `initialize` request, keep running.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    {
        server.wait_for_concurrent_jobs();
        let response: Value = serde_json::from_str(&results.lock().unwrap().remove(0)).unwrap();
        assert_eq!(response["id"], 0);
        assert_eq!(response["error"]["code"], -32002);
        let message = response["error"]["message"].as_str().unwrap();
        assert!(
            message.to_lowercase().contains("initialize"),
            "Unexpected error.message `{}`",
            message,
        );
    }

    // Initialize and build.
    assert_eq!(
        ls_server::LsService::handle_message(&mut server),
        ls_server::ServerStateChange::Continue
    );
    expect_message(
        &mut server,
        results.clone(),
        ExpectedMessage::new(Some(1)).expect_contains("capabilities"),
    );

    expect_series(&mut server, results, vec!["progress"]);
}

// FIXME disabled since it is failing in the Rust repo.
// #[test]
// fn test_dep_fail() {
//     let mut env = Environment::generate_from_fixture("dep_fail");

//     let root_path = env.cache.abs_path(Path::new("."));

//     let messages = vec![
//         initialize(0, root_path.as_os_str().to_str().map(|x| x.to_owned())).to_string(),
//     ];

//     let (mut server, results, ..) = env.mock_server(messages);
//     // Initialize and build.
//     assert_eq!(
//         ls_server::LsService::handle_message(&mut server),
//         ls_server::ServerStateChange::Continue
//     );
//     expect_message(
//         results.clone(),
//         &[
//             ExpectedMessage::new(Some(0)).expect_contains("capabilities"),
//             ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#"title":"Building""#),
//             ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#"title":"Building""#),
//             ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#""done":true"#),
//             ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#"title":"Indexing""#),
//             ExpectedMessage::new(None).expect_contains("message").expect_contains("Cargo failed: Error compiling dependent crate"),
//             ExpectedMessage::new(None).expect_contains("progress").expect_contains(r#"title":"Indexing""#),
//         ],
//     );
// }
