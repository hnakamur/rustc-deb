// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! The Rust Language Server.
//!
//! The RLS provides a server that runs in the background, providing IDEs,
//! editors, and other tools with information about Rust programs. It supports
//! functionality such as 'goto definition', symbol search, reformatting, and
//! code completion, and enables renaming and refactorings.

#![feature(rustc_private, integer_atomics, drain_filter)]
#![feature(crate_visibility_modifier)] // needed for edition 2018
#![allow(unknown_lints)]
#![warn(clippy::all, rust_2018_idioms)]
#![allow(
    clippy::cyclomatic_complexity,
    clippy::needless_pass_by_value,
    clippy::too_many_arguments
)]
// See rustc/rustc.rs in rust repo for explanation of stack adjustments.
#![feature(link_args)]
#[allow(unused_attributes)]
#[cfg_attr(
    all(windows, target_env = "msvc"),
    link_args = "/STACK:16777216"
)]
#[cfg_attr(
    all(windows, not(target_env = "msvc")),
    link_args = "-Wl,--stack,16777216"
)]
extern "C" {}

use log::warn;
use env_logger;
use rustc_tools_util::*;

use std::env;
use std::sync::Arc;

use rls_analysis::{AnalysisHost, Target};
use rls_rustc as rustc_shim;
use rls_vfs::Vfs;

pub mod actions;
pub mod build;
pub mod cmd;
pub mod concurrency;
pub mod config;
pub mod lsp_data;
pub mod project_model;
pub mod server;

#[cfg(test)]
mod test;

const RUSTC_SHIM_ENV_VAR_NAME: &str = "RLS_RUSTC_SHIM";
const RUSTC_WRAPPER_ENV_VAR: &str = "RUSTC_WRAPPER";

type Span = rls_span::Span<rls_span::ZeroIndexed>;

/// The main entry point to the RLS. Parses CLI arguments and then runs the
/// server.
pub fn main() {
    let exit_code = main_inner();
    ::std::process::exit(exit_code);
}

fn main_inner() -> i32 {
    env_logger::init();

    // [workaround]
    // Currently sccache breaks RLS with obscure error messages.
    // Until it's actually fixed disable the wrapper completely
    // in the current process tree.
    //
    // See https://github.com/rust-lang/rls/issues/703
    // and https://github.com/mozilla/sccache/issues/303
    if env::var_os(RUSTC_WRAPPER_ENV_VAR).is_some() {
        warn!("The {} environment variable is incompatible with RLS, \
               removing it from the process environment", RUSTC_WRAPPER_ENV_VAR);
        env::remove_var(RUSTC_WRAPPER_ENV_VAR);
    }

    if env::var(RUSTC_SHIM_ENV_VAR_NAME)
        .map(|v| v != "0")
        .unwrap_or(false)
    {
        rustc_shim::run();
        return 0;
    }

    if let Some(first_arg) = ::std::env::args().nth(1) {
        return match first_arg.as_str() {
            "--version" | "-V" => {
                println!("{}", version().replace("rls", "rls-preview"));
                0
            }
            "--help" | "-h" => {
                println!("{}", help());
                0
            }
            "--cli" => {
                cmd::run();
                0
            }
            unknown => {
                println!(
                    "Unknown argument '{}'. Supported arguments:\n{}",
                    unknown,
                    help()
                );
                101
            }
        };
    }

    let analysis = Arc::new(AnalysisHost::new(Target::Debug));
    let vfs = Arc::new(Vfs::new());

    server::run_server(analysis, vfs)
}

fn version() -> String {
    let version = rustc_tools_util::get_version_info!();
    version.to_string()
}

fn help() -> &'static str {
    r#"
    --version or -V to print the version and commit info
    --help or -h for this message
    --cli starts the RLS in command line mode
    No input starts the RLS as a language server
    "#
}
