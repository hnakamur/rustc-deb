// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Implementation of the server loop, and traits for extending server
//! interactions (for example, to add support for handling new types of
//! requests).

use crate::actions::{notifications, requests, ActionContext};
use crate::config::Config;
use crate::lsp_data;
use crate::lsp_data::{InitializationOptions, LSPNotification, LSPRequest};
use crate::server::dispatch::Dispatcher;
pub use crate::server::dispatch::{RequestAction, DEFAULT_REQUEST_TIMEOUT};
pub use crate::server::io::{MessageReader, Output};
use crate::server::io::{StdioMsgReader, StdioOutput};
use crate::server::message::RawMessage;
pub use crate::server::message::{
    Ack, BlockingNotificationAction, BlockingRequestAction, NoResponse, Notification, Request,
    RequestId, Response, ResponseError, ResponseWithMessage,
};
use crate::version;
use jsonrpc_core::{self as jsonrpc, types::error::ErrorCode, Id};
pub use languageserver_types::notification::Exit as ExitNotification;
pub use languageserver_types::request::Initialize as InitializeRequest;
pub use languageserver_types::request::Shutdown as ShutdownRequest;
use languageserver_types::{
    CodeActionProviderCapability, CodeLensOptions, CompletionOptions, ExecuteCommandOptions,
    ImplementationProviderCapability, InitializeParams, InitializeResult, RenameProviderCapability,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use log::{debug, error, trace, warn};
use rls_analysis::AnalysisHost;
use rls_vfs::Vfs;
use serde_json;
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

mod dispatch;
mod io;
mod message;

const NOT_INITIALIZED_CODE: ErrorCode = ErrorCode::ServerError(-32002);

/// Run the Rust Language Server.
pub fn run_server(analysis: Arc<AnalysisHost>, vfs: Arc<Vfs>) -> i32 {
    debug!("Language Server starting up. Version: {}", version());
    let service = LsService::new(
        analysis,
        vfs,
        Arc::new(Mutex::new(Config::default())),
        Box::new(StdioMsgReader),
        StdioOutput::new(),
    );
    let exit_code = LsService::run(service);
    debug!("Server shutting down");
    exit_code
}

impl BlockingRequestAction for ShutdownRequest {
    type Response = Ack;

    fn handle<O: Output>(
        _id: RequestId,
        _params: Self::Params,
        ctx: &mut ActionContext,
        _out: O,
    ) -> Result<Self::Response, ResponseError> {
        if let Ok(ctx) = ctx.inited() {
            // Currently we don't perform an explicit cleanup, other than storing state
            ctx.shut_down.store(true, Ordering::SeqCst);
            Ok(Ack)
        } else {
            Err(ResponseError::Message(
                NOT_INITIALIZED_CODE,
                "not yet received `initialize` request".to_owned(),
            ))
        }
    }
}

impl BlockingRequestAction for InitializeRequest {
    type Response = NoResponse;

    fn handle<O: Output>(
        id: RequestId,
        params: Self::Params,
        ctx: &mut ActionContext,
        out: O,
    ) -> Result<NoResponse, ResponseError> {
        let init_options: InitializationOptions = params
            .initialization_options
            .as_ref()
            .and_then(|options| {
                let de = serde_json::from_value(options.to_owned());
                if let Err(ref e) = de {
                    warn!("initialization_options: {}", e);
                }
                de.ok()
            })
            .unwrap_or_default();

        trace!("init: {:?} -> {:?}", params.initialization_options, init_options);

        if ctx.inited().is_ok() {
            return Err(ResponseError::Message(
                // No code in the spec, just use some number
                ErrorCode::ServerError(123),
                "Already received an initialize request".to_owned(),
            ));
        }

        let result = InitializeResult {
            capabilities: server_caps(ctx),
        };

        // send response early before `ctx.init` to enforce
        // initialize-response-before-all-other-messages constraint
        result.send(id, &out);

        let capabilities = lsp_data::ClientCapabilities::new(&params);
        ctx.init(get_root_path(&params), init_options, capabilities, &out)
            .unwrap();

        Ok(NoResponse)
    }
}

/// A service implementing a language server.
pub struct LsService<O: Output> {
    msg_reader: Box<dyn MessageReader + Send + Sync>,
    output: O,
    ctx: ActionContext,
    dispatcher: Dispatcher,
}

impl<O: Output> LsService<O> {
    /// Construct a new language server service.
    pub fn new(
        analysis: Arc<AnalysisHost>,
        vfs: Arc<Vfs>,
        config: Arc<Mutex<Config>>,
        reader: Box<dyn MessageReader + Send + Sync>,
        output: O,
    ) -> LsService<O> {
        let dispatcher = Dispatcher::new(output.clone());

        LsService {
            msg_reader: reader,
            output,
            ctx: ActionContext::new(analysis, vfs, config),
            dispatcher,
        }
    }

    /// Run this language service.
    pub fn run(mut self) -> i32 {
        loop {
            match self.handle_message() {
                ServerStateChange::Continue => (),
                ServerStateChange::Break { exit_code } => return exit_code,
            }
        }
    }

    fn dispatch_message(&mut self, msg: RawMessage) -> Result<(), jsonrpc::Error> {
        macro_rules! match_action {
            (
                $method: expr;
                notifications: $($n_action: ty),*;
                blocking_requests: $($br_action: ty),*;
                requests: $($request: ty),*;
            ) => {
                trace!("Handling `{}`", $method);

                match $method.as_str() {
                $(
                    <$n_action as LSPNotification>::METHOD => {
                        let notification: Notification<$n_action> = msg.parse_as_notification()?;
                        if let Ok(mut ctx) = self.ctx.inited() {
                            if notification.dispatch(&mut ctx, self.output.clone()).is_err() {
                                debug!("Error handling notification: {:?}", msg);
                            }
                        }
                        else {
                            warn!(
                                "Server has not yet received an `initialize` request, ignoring {}", $method,
                            );
                        }
                    }
                )*

                $(
                    <$br_action as LSPRequest>::METHOD => {
                        let request: Request<$br_action> = msg.parse_as_request()?;

                        // block until all nonblocking requests have been handled ensuring ordering
                        self.wait_for_concurrent_jobs();

                        let req_id = request.id.clone();
                        match request.blocking_dispatch(&mut self.ctx, &self.output) {
                            Ok(res) => res.send(req_id, &self.output),
                            Err(ResponseError::Empty) => {
                                debug!("error handling {}", $method);
                                self.output.failure_message(
                                    req_id,
                                    ErrorCode::InternalError,
                                    "An unknown error occurred"
                                )
                            }
                            Err(ResponseError::Message(code, msg)) => {
                                debug!("error handling {}: {}", $method, msg);
                                self.output.failure_message(req_id, code, msg)
                            }
                        }
                    }
                )*

                $(
                    <$request as LSPRequest>::METHOD => {
                        let request: Request<$request> = msg.parse_as_request()?;
                        if let Ok(ctx) = self.ctx.inited() {
                            self.dispatcher.dispatch(request, ctx);
                        }
                        else {
                            warn!(
                                "Server has not yet received an `initialize` request, cannot handle {}", $method,
                            );
                            self.output.failure_message(
                                request.id,
                                NOT_INITIALIZED_CODE,
                                "not yet received `initialize` request".to_owned(),
                            );
                        }
                    }
                )*
                    _ => debug!("Method not found: {}", $method)
                }
            }
        }

        // Notifications and blocking requests are handled immediately on the
        // main thread. They will never be dropped.
        // Blocking requests wait for all non-blocking requests to complete,
        // notifications do not.
        // Other requests are read and then forwarded to a worker thread, they
        // might timeout and will return an error but should not be dropped.
        // Some requests might block again when executing due to waiting for a
        // build or access to the VFS or real file system.
        // Requests must not mutate RLS state, but may ask the client to mutate
        // the client state.
        match_action!(
            msg.method;
            notifications:
                notifications::Initialized,
                notifications::DidOpenTextDocument,
                notifications::DidChangeTextDocument,
                notifications::DidSaveTextDocument,
                notifications::DidChangeConfiguration,
                notifications::DidChangeWatchedFiles,
                notifications::Cancel;
            blocking_requests:
                ShutdownRequest,
                InitializeRequest;
            requests:
                requests::ExecuteCommand,
                requests::Formatting,
                requests::RangeFormatting,
                requests::ResolveCompletion,
                requests::Rename,
                requests::CodeAction,
                requests::DocumentHighlight,
                requests::Implementation,
                requests::Symbols,
                requests::Hover,
                requests::WorkspaceSymbol,
                requests::Definition,
                requests::References,
                requests::Completion,
                requests::CodeLensRequest;
        );
        Ok(())
    }

    /// Read a message from the language server reader input and handle it with
    /// the appropriate action. Returns a `ServerStateChange` that describes how
    /// the service should proceed now that the message has been handled.
    pub fn handle_message(&mut self) -> ServerStateChange {
        let msg_string = match self.msg_reader.read_message() {
            Some(m) => m,
            None => {
                error!("Can't read message");
                self.output.failure(Id::Null, jsonrpc::Error::parse_error());
                return ServerStateChange::Break { exit_code: 101 };
            }
        };

        trace!("Read message `{}`", msg_string);

        let raw_message = match RawMessage::try_parse(&msg_string) {
            Ok(Some(rm)) => rm,
            Ok(None) => return ServerStateChange::Continue,
            Err(e) => {
                error!("parsing error, {:?}", e);
                self.output.failure(Id::Null, jsonrpc::Error::parse_error());
                return ServerStateChange::Break { exit_code: 101 };
            }
        };

        trace!("Parsed message `{:?}`", raw_message);

        // If we're in shutdown mode, ignore any messages other than 'exit'.
        // This is not actually in the spec, I'm not sure we should do this,
        // but it kinda makes sense.
        {
            let shutdown_mode = match self.ctx {
                ActionContext::Init(ref ctx) => ctx.shut_down.load(Ordering::SeqCst),
                _ => false,
            };
            if raw_message.method == <ExitNotification as LSPNotification>::METHOD {
                let exit_code = if shutdown_mode { 0 } else { 1 };
                return ServerStateChange::Break { exit_code };
            }
            if shutdown_mode {
                trace!("In shutdown mode, ignoring {:?}!", raw_message);
                return ServerStateChange::Continue;
            }
        }

        // Workaround https://github.com/rust-lang/rust/pull/55937 by moving
        // raw_message instead of borrowing.
        let id = raw_message.id.clone();
        if let Err(e) = self.dispatch_message(raw_message) {
            error!("dispatch error: {:?}, message: `{}`", e, msg_string);
            self.output.failure(id, e);
            return ServerStateChange::Break { exit_code: 101 };
        }

        ServerStateChange::Continue
    }

    pub fn wait_for_concurrent_jobs(&mut self) {
        match &self.ctx {
            ActionContext::Init(ctx) => ctx.wait_for_concurrent_jobs(),
            ActionContext::Uninit(_) => {}
        }
    }
}

/// How should the server proceed?
#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum ServerStateChange {
    /// Continue serving responses to requests and sending notifications to the
    /// client.
    Continue,
    /// Stop the server.
    Break { exit_code: i32 },
}

fn server_caps(ctx: &ActionContext) -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::Incremental,
        )),
        hover_provider: Some(true),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
        }),
        definition_provider: Some(true),
        type_definition_provider: None,
        implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
        references_provider: Some(true),
        document_highlight_provider: Some(true),
        document_symbol_provider: Some(true),
        workspace_symbol_provider: Some(true),
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
        document_formatting_provider: Some(true),
        execute_command_provider: Some(ExecuteCommandOptions {
            // We append our pid to the command so that if there are multiple
            // instances of the RLS then they will have unique names for the
            // commands.
            commands: vec![
                format!("rls.applySuggestion-{}", ctx.pid()),
                format!("rls.deglobImports-{}", ctx.pid()),
            ],
        }),
        rename_provider: Some(RenameProviderCapability::Simple(true)),
        color_provider: None,

        // These are supported if the `unstable_features` option is set.
        // We'll update these capabilities dynamically when we get config
        // info from the client.
        document_range_formatting_provider: Some(false),

        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
        }),
        document_on_type_formatting_provider: None,
        signature_help_provider: None,

        folding_range_provider: None,
        workspace: None,
    }
}

fn get_root_path(params: &InitializeParams) -> PathBuf {
    params
        .root_uri
        .as_ref()
        .map(|uri| {
            assert!(uri.scheme() == "file");
            uri.to_file_path().expect("Could not convert URI to path")
        })
        .unwrap_or_else(|| {
            params
                .root_path
                .as_ref()
                .map(PathBuf::from)
                .expect("No root path or URI")
        })
}

#[cfg(test)]
mod test {
    use super::*;
    use url::Url;

    fn get_default_params() -> InitializeParams {
        InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: languageserver_types::ClientCapabilities {
                workspace: None,
                text_document: None,
                experimental: None,
            },
            trace: Some(languageserver_types::TraceOption::Off),
            workspace_folders: None,
        }
    }

    fn make_platform_path(path: &'static str) -> PathBuf {
        if cfg!(windows) {
            PathBuf::from(format!("C:/{}", path))
        } else {
            PathBuf::from(format!("/{}", path))
        }
    }

    #[test]
    fn test_use_root_uri() {
        let mut params = get_default_params();

        let root_path = make_platform_path("path/a");
        let root_uri = make_platform_path("path/b");
        params.root_path = Some(root_path.to_str().unwrap().to_owned());
        params.root_uri = Some(Url::from_directory_path(&root_uri).unwrap());

        assert_eq!(get_root_path(&params), root_uri);
    }

    #[test]
    fn test_use_root_path() {
        let mut params = get_default_params();

        let root_path = make_platform_path("path/a");
        params.root_path = Some(root_path.to_str().unwrap().to_owned());
        params.root_uri = None;

        assert_eq!(get_root_path(&params), root_path);
    }

    /// Some clients send empty object params for void params requests (see #1038)
    #[test]
    fn parse_shutdown_object_params() {
        let raw = RawMessage::try_parse(
            r#"{"jsonrpc": "2.0", "id": 2, "method": "shutdown", "params": {}}"#,
        )
        .ok()
        .and_then(|x| x)
        .expect("raw parse failed");

        let _request: Request<ShutdownRequest> = raw
            .parse_as_request()
            .expect("Boring validation is happening");
    }
}
