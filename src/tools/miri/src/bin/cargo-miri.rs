#![feature(inner_deref)]

extern crate cargo_metadata;

use std::path::{PathBuf, Path};
use std::io::{self, Write, BufRead};
use std::process::Command;
use std::fs::{self, File};

const CARGO_MIRI_HELP: &str = r#"Interprets bin crates

Usage:
    cargo miri [subcommand] [options] [--] [<opts>...]

Subcommands:
    run                      Run binaries (default)
    test                     Run tests
    setup                    Only perform automatic setup, but without asking questions (for getting a proper libstd)

Common options:
    -h, --help               Print this message
    --features               Features to compile for the package
    -V, --version            Print version info and exit

Other options are the same as `cargo rustc`.

The feature `cargo-miri` is automatically defined for convenience. You can use
it to configure the resource limits

    #![cfg_attr(feature = "cargo-miri", memory_size = 42)]

available resource limits are `memory_size`, `step_limit`, `stack_limit`
"#;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MiriCommand {
    Run,
    Test,
    Setup,
}

fn show_help() {
    println!("{}", CARGO_MIRI_HELP);
}

fn show_version() {
    println!("miri {} ({} {})",
        env!("CARGO_PKG_VERSION"), env!("VERGEN_SHA_SHORT"), env!("VERGEN_COMMIT_DATE"));
}

fn show_error(msg: String) -> ! {
    eprintln!("fatal error: {}", msg);
    std::process::exit(1)
}

fn get_arg_flag_value(name: &str) -> Option<String> {
    // stop searching at `--`
    let mut args = std::env::args().skip_while(|val| !(val.starts_with(name) || val == "--"));

    match args.next() {
        Some(ref p) if p == "--" => None,
        Some(ref p) if p == name => args.next(),
        Some(p) => {
            // Make sure this really starts with `$name=`, we didn't test for the `=` yet.
            let v = &p[name.len()..]; // strip leading `$name`
            if v.starts_with('=') {
                Some(v[1..].to_owned()) // strip leading `=`
            } else {
                None
            }
        },
        None => None,
    }
}

fn list_targets() -> impl Iterator<Item=cargo_metadata::Target> {
    // We need to get the manifest, and then the metadata, to enumerate targets.
    let manifest_path = get_arg_flag_value("--manifest-path").map(|m|
        Path::new(&m).canonicalize().unwrap()
    );

    let mut metadata = if let Ok(metadata) = cargo_metadata::metadata(
        manifest_path.as_ref().map(AsRef::as_ref),
    )
    {
        metadata
    } else {
        show_error(format!("error: Could not obtain cargo metadata."));
    };

    let current_dir = std::env::current_dir();

    let package_index = metadata
        .packages
        .iter()
        .position(|package| {
            let package_manifest_path = Path::new(&package.manifest_path);
            if let Some(ref manifest_path) = manifest_path {
                package_manifest_path == manifest_path
            } else {
                let current_dir = current_dir.as_ref().expect(
                    "could not read current directory",
                );
                let package_manifest_directory = package_manifest_path.parent().expect(
                    "could not find parent directory of package manifest",
                );
                package_manifest_directory == current_dir
            }
        })
        .expect("could not find matching package");
    let package = metadata.packages.remove(package_index);

    // Finally we got the list of targets to build
    package.targets.into_iter()
}

fn xargo_version() -> Option<(u32, u32, u32)> {
    let out = Command::new("xargo").arg("--version").output().ok()?;
    if !out.status.success() {
        return None;
    }
    // Parse output. The first line looks like "xargo 0.3.12 (b004f1c 2018-12-13)".
    let line = out.stderr.lines().nth(0)
        .expect("malformed `xargo --version` output: not at least one line")
        .expect("malformed `xargo --version` output: error reading first line");
    let (name, version) = {
        let mut split = line.split(' ');
        (split.next().expect("malformed `xargo --version` output: empty"),
         split.next().expect("malformed `xargo --version` output: not at least two words"))
    };
    if name != "xargo" {
        panic!("malformed `xargo --version` output: application name is not `xargo`");
    }
    let mut version_pieces = version.split('.');
    let major = version_pieces.next()
        .expect("malformed `xargo --version` output: not a major version piece")
        .parse()
        .expect("malformed `xargo --version` output: major version is not an integer");
    let minor = version_pieces.next()
        .expect("malformed `xargo --version` output: not a minor version piece")
        .parse()
        .expect("malformed `xargo --version` output: minor version is not an integer");
    let patch = version_pieces.next()
        .expect("malformed `xargo --version` output: not a patch version piece")
        .parse()
        .expect("malformed `xargo --version` output: patch version is not an integer");
    if !version_pieces.next().is_none() {
        panic!("malformed `xargo --version` output: more than three pieces in version");
    }
    Some((major, minor, patch))
}

fn ask(question: &str) {
    let mut buf = String::new();
    print!("{} [Y/n] ", question);
    io::stdout().flush().unwrap();
    io::stdin().read_line(&mut buf).unwrap();
    match buf.trim().to_lowercase().as_ref() {
        "" | "y" | "yes" => {}, // proceed
        "n" | "no" => show_error(format!("Aborting as per your request")),
        a => show_error(format!("I do not understand `{}`", a))
    };
}

/// Perform the setup requires to make `cargo miri` work: Getting a custom-built libstd. Then sets MIRI_SYSROOT.
/// Skipped if MIRI_SYSROOT is already set, in that case we expect the user has done all this already.
fn setup(ask_user: bool) {
    if std::env::var("MIRI_SYSROOT").is_ok() {
        return;
    }

    // First, we need xargo
    let xargo = xargo_version();
    if xargo.map_or(true, |v| v < (0, 3, 13)) {
        if ask_user {
            ask("It seems you do not have a recent enough xargo installed. I will run `cargo install xargo -f`. Proceed?");
        } else {
            println!("Installing xargo: `cargo install xargo -f`");
        }
        if !Command::new("cargo").args(&["install", "xargo", "-f"]).status().unwrap().success() {
            show_error(format!("Failed to install xargo"));
        }
    }

    // Then, we also need rust-src.  Let's see if it is already installed.
    let sysroot = Command::new("rustc").args(&["--print", "sysroot"]).output().unwrap().stdout;
    let sysroot = std::str::from_utf8(&sysroot[..]).unwrap();
    let src = Path::new(sysroot.trim_end_matches('\n')).join("lib").join("rustlib").join("src");
    if !src.exists() {
        if ask_user {
            ask("It seems you do not have the rust-src component installed. I will run `rustup component add rust-src`. Proceed?");
        } else {
            println!("Installing rust-src component: `rustup component add rust-src`");
        }
        if !Command::new("rustup").args(&["component", "add", "rust-src"]).status().unwrap().success() {
            show_error(format!("Failed to install rust-src component"));
        }
    }

    // Next, we need our own libstd. We will do this work in whatever is a good cache dir for this platform.
    let dirs = directories::ProjectDirs::from("miri", "miri", "miri").unwrap();
    let dir = dirs.cache_dir();
    if !dir.exists() {
        fs::create_dir_all(&dir).unwrap();
    }
    // The interesting bit: Xargo.toml
    File::create(dir.join("Xargo.toml")).unwrap()
        .write_all(br#"
[dependencies.std]
default_features = false
# We need the `panic_unwind` feature because we use the `unwind` panic strategy.
# Using `abort` works for libstd, but then libtest will not compile.
features = ["panic_unwind"]

[dependencies.test]
stage = 1
        "#).unwrap();
    // The boring bits: A dummy project for xargo
    File::create(dir.join("Cargo.toml")).unwrap()
        .write_all(br#"
[package]
name = "miri-xargo"
description = "A dummy project for building libstd with xargo."
version = "0.0.0"

[lib]
path = "lib.rs"
        "#).unwrap();
    File::create(dir.join("lib.rs")).unwrap();
    // Run xargo
    let target = get_arg_flag_value("--target");
    let mut command = Command::new("xargo");
    command.arg("build").arg("-q")
        .current_dir(&dir)
        .env("RUSTFLAGS", miri::miri_default_args().join(" "))
        .env("XARGO_HOME", dir.to_str().unwrap());
    if let Some(ref target) = target {
        command.arg("--target").arg(&target);
    }
    if !command.status().unwrap().success()
    {
        show_error(format!("Failed to run xargo"));
    }

    // That should be it!  But we need to figure out where xargo built stuff.
    // Unfortunately, it puts things into a different directory when the
    // architecture matches the host.
    let is_host = match target {
        None => true,
        Some(target) => target == rustc_version::version_meta().unwrap().host,
    };
    let sysroot = if is_host { dir.join("HOST") } else { PathBuf::from(dir) };
    std::env::set_var("MIRI_SYSROOT", &sysroot);
    if !ask_user {
        println!("A libstd for miri is now available in `{}`", sysroot.display());
    }
}

fn main() {
    // Check for version and help flags even when invoked as 'cargo-miri'
    if std::env::args().any(|a| a == "--help" || a == "-h") {
        show_help();
        return;
    }
    if std::env::args().any(|a| a == "--version" || a == "-V") {
        show_version();
        return;
    }

    if let Some("miri") = std::env::args().nth(1).as_ref().map(AsRef::as_ref) {
        // this arm is when `cargo miri` is called.  We call `cargo rustc` for
        // each applicable target, but with the RUSTC env var set to the `cargo-miri`
        // binary so that we come back in the other branch, and dispatch
        // the invocations to rustc and miri, respectively.

        let (subcommand, skip) = match std::env::args().nth(2).deref() {
            Some("test") => (MiriCommand::Test, 3),
            Some("run") => (MiriCommand::Run, 3),
            Some("setup") => (MiriCommand::Setup, 3),
            // Default command, if there is an option or nothing
            Some(s) if s.starts_with("-") => (MiriCommand::Run, 2),
            None => (MiriCommand::Run, 2),
            // Unvalid command
            Some(s) => {
                show_error(format!("Unknown command `{}`", s))
            }
        };

        // We always setup
        let ask = subcommand != MiriCommand::Setup;
        setup(ask);
        if subcommand == MiriCommand::Setup {
            // Stop here.
            return;
        }

        // Now run the command.
        for target in list_targets() {
            let args = std::env::args().skip(skip);
            let kind = target.kind.get(0).expect(
                "badly formatted cargo metadata: target::kind is an empty array",
            );
            match (subcommand, &kind[..]) {
                (MiriCommand::Test, "test") => {
                    // For test binaries we call `cargo rustc --test target -- <rustc args>`
                    if let Err(code) = process(
                        vec!["--test".to_string(), target.name].into_iter().chain(
                            args,
                        ),
                    )
                    {
                        std::process::exit(code);
                    }
                }
                (MiriCommand::Test, "lib") => {
                    // For libraries we call `cargo rustc -- --test <rustc args>`
                    // Notice now that `--test` is a rustc arg rather than a cargo arg. This tells
                    // rustc to build a test harness which calls all #[test] functions.
                    // We then execute that harness just like any other binary.
                    if let Err(code) = process(
                        vec!["--".to_string(), "--test".to_string()].into_iter().chain(
                            args,
                        ),
                    )
                    {
                        std::process::exit(code);
                    }
                }
                (MiriCommand::Run, "bin") => {
                    // For ordinary binaries we call `cargo rustc --bin target -- <rustc args>`
                    if let Err(code) = process(
                        vec!["--bin".to_string(), target.name].into_iter().chain(
                            args,
                        ),
                    )
                    {
                        std::process::exit(code);
                    }
                }
                _ => {}
            }
        }
    } else if let Some("rustc") = std::env::args().nth(1).as_ref().map(AsRef::as_ref) {
        // This arm is executed when cargo-miri runs `cargo rustc` with the `RUSTC_WRAPPER` env var set to itself:
        // Dependencies get dispatched to rustc, the final test/binary to miri.

        let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
        let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
        let sys_root = if let Ok(sysroot) = ::std::env::var("MIRI_SYSROOT") {
            sysroot
        } else if let (Some(home), Some(toolchain)) = (home, toolchain) {
            format!("{}/toolchains/{}", home, toolchain)
        } else {
            option_env!("RUST_SYSROOT")
                .map(|s| s.to_owned())
                .or_else(|| {
                    Command::new("rustc")
                        .arg("--print")
                        .arg("sysroot")
                        .output()
                        .ok()
                        .and_then(|out| String::from_utf8(out.stdout).ok())
                        .map(|s| s.trim().to_owned())
                })
                .expect("need to specify RUST_SYSROOT env var during miri compilation, or use rustup or multirust")
        };

        // this conditional check for the --sysroot flag is there so users can call `cargo-miri` directly
        // without having to pass --sysroot or anything
        let rustc_args = std::env::args().skip(2);
        let mut args: Vec<String> = if std::env::args().any(|s| s == "--sysroot") {
            rustc_args.collect()
        } else {
            rustc_args
                .chain(Some("--sysroot".to_owned()))
                .chain(Some(sys_root))
                .collect()
        };
        args.splice(0..0, miri::miri_default_args().iter().map(ToString::to_string));
        args.extend_from_slice(&["--cfg".to_owned(), r#"feature="cargo-miri""#.to_owned()]);

        // this check ensures that dependencies are built but not interpreted and the final crate is
        // interpreted but not built
        let miri_enabled = std::env::args().any(|s| s == "--emit=dep-info,metadata");
        let mut command = if miri_enabled {
            let mut path = std::env::current_exe().expect("current executable path invalid");
            path.set_file_name("miri");
            Command::new(path)
        } else {
            Command::new("rustc")
        };
        command.args(&args);

        match command.status() {
            Ok(exit) => {
                if !exit.success() {
                    std::process::exit(exit.code().unwrap_or(42));
                }
            }
            Err(ref e) if miri_enabled => panic!("error during miri run: {:?}", e),
            Err(ref e) => panic!("error during rustc call: {:?}", e),
        }
    } else {
        show_error(format!("Must be called with either `miri` or `rustc` as first argument."))
    }
}

fn process<I>(old_args: I) -> Result<(), i32>
where
    I: Iterator<Item = String>,
{
    let mut args = vec!["rustc".to_owned()];

    let mut found_dashes = false;
    for arg in old_args {
        found_dashes |= arg == "--";
        args.push(arg);
    }
    if !found_dashes {
        args.push("--".to_owned());
    }
    args.push("--emit=dep-info,metadata".to_owned());

    let path = std::env::current_exe().expect("current executable path invalid");
    let exit_status = Command::new("cargo")
        .args(&args)
        .env("RUSTC_WRAPPER", path)
        .spawn()
        .expect("could not run cargo")
        .wait()
        .expect("failed to wait for cargo?");

    if exit_status.success() {
        Ok(())
    } else {
        Err(exit_status.code().unwrap_or(-1))
    }
}
