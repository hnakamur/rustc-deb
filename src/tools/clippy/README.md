# Clippy

[![Build Status](https://travis-ci.com/rust-lang/rust-clippy.svg?branch=master)](https://travis-ci.com/rust-lang/rust-clippy)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/id677xpw1dguo7iw?svg=true)](https://ci.appveyor.com/project/rust-lang-libs/rust-clippy)
[![Current Version](https://meritbadge.herokuapp.com/clippy)](https://crates.io/crates/clippy)
[![License: MIT/Apache-2.0](https://img.shields.io/crates/l/clippy.svg)](#license)

A collection of lints to catch common mistakes and improve your [Rust](https://github.com/rust-lang/rust) code.

[There are 291 lints included in this crate!](https://rust-lang.github.io/rust-clippy/master/index.html)

We have a bunch of lint categories to allow you to choose how much Clippy is supposed to ~~annoy~~ help you:

* `clippy::all` (everything that has no false positives)
* `clippy::pedantic` (everything)
* `clippy::nursery` (new lints that aren't quite ready yet)
* `clippy::style` (code that should be written in a more idiomatic way)
* `clippy::complexity` (code that does something simple but in a complex way)
* `clippy::perf` (code that can be written in a faster way)
* `clippy::cargo` (checks against the cargo manifest)
* **`clippy::correctness`** (code that is just outright wrong or very very useless)

More to come, please [file an issue](https://github.com/rust-lang/rust-clippy/issues) if you have ideas!

Only the following of those categories are enabled by default:

* `clippy::style`
* `clippy::correctness`
* `clippy::complexity`
* `clippy::perf`

Other categories need to be enabled in order for their lints to be executed.

Table of contents:

*   [Usage instructions](#usage)
*   [Configuration](#configuration)
*   [License](#license)

## Usage

Since this is a tool for helping the developer of a library or application
write better code, it is recommended not to include Clippy as a hard dependency.
Options include using it as an optional dependency, as a cargo subcommand, or
as an included feature during build. These options are detailed below.

### As a cargo subcommand (`cargo clippy`)

One way to use Clippy is by installing Clippy through rustup as a cargo
subcommand.

#### Step 1: Install rustup

You can install [rustup](http://rustup.rs/) on supported platforms. This will help
us install Clippy and its dependencies.

If you already have rustup installed, update to ensure you have the latest
rustup and compiler:

```terminal
rustup update
```

#### Step 2: Install Clippy

Once you have rustup and the latest stable release (at least Rust 1.29) installed, run the following command:

```terminal
rustup component add clippy
```
If it says that it can't find the `clippy` component, please run `rustup self update`.

#### Step 3: Run Clippy

Now you can run Clippy by invoking the following command:

```terminal
cargo clippy
```

### Running Clippy from the command line without installing it

To have cargo compile your crate with Clippy without Clippy installation
in your code, you can use:

```terminal
cargo run --bin cargo-clippy --manifest-path=path_to_clippys_Cargo.toml
```

*[Note](https://github.com/rust-lang/rust-clippy/wiki#a-word-of-warning):*
Be sure that Clippy was compiled with the same version of rustc that cargo invokes here!

### Travis CI

You can add Clippy to Travis CI in the same way you use it locally:

```yml
language: rust
rust:
  - stable
  - beta
before_script:
  - rustup component add clippy
script:
  - cargo clippy
  # if you want the build job to fail when encountering warnings, use
  - cargo clippy -- -D warnings
  # in order to also check tests and none-default crate features, use
  - cargo clippy --all-targets --all-features -- -D warnings
  - cargo test
  # etc.
```

It might happen that Clippy is not available for a certain nightly release.
In this case you can try to conditionally install Clippy from the git repo.

```yaml
language: rust
rust:
  - nightly
before_script:
   - rustup component add clippy --toolchain=nightly || cargo install --git https://github.com/rust-lang/rust-clippy/ --force clippy
   # etc
```

## Configuration

Some lints can be configured in a TOML file named `clippy.toml` or `.clippy.toml`. It contains a basic `variable = value` mapping eg.

```toml
blacklisted-names = ["toto", "tata", "titi"]
cyclomatic-complexity-threshold = 30
```

See the [list of lints](https://rust-lang.github.io/rust-clippy/master/index.html) for more information about which lints can be configured and the
meaning of the variables.

To deactivate the “for further information visit *lint-link*” message you can
define the `CLIPPY_DISABLE_DOCS_LINKS` environment variable.

### Allowing/denying lints

You can add options to your code to `allow`/`warn`/`deny` Clippy lints:

*   the whole set of `Warn` lints using the `clippy` lint group (`#![deny(clippy::all)]`)

*   all lints using both the `clippy` and `clippy::pedantic` lint groups (`#![deny(clippy::all)]`,
    `#![deny(clippy::pedantic)]`). Note that `clippy::pedantic` contains some very aggressive
    lints prone to false positives.

*   only some lints (`#![deny(clippy::single_match, clippy::box_vec)]`, etc)

*   `allow`/`warn`/`deny` can be limited to a single function or module using `#[allow(...)]`, etc

Note: `deny` produces errors instead of warnings.

If you do not want to include your lint levels in your code, you can globally enable/disable lints by passing extra flags to Clippy during the run: `cargo clippy -- -A clippy::lint_name` will run Clippy with `lint_name` disabled and `cargo clippy -- -W clippy::lint_name` will run it with that enabled. This also works with lint groups. For example you can run Clippy with warnings for all lints enabled: `cargo clippy -- -W clippy::pedantic`

## Contributing

If you want to contribute to Clippy, you can find more information in [CONTRIBUTING.md](https://github.com/rust-lang/rust-clippy/blob/master/CONTRIBUTING.md).

## License

Copyright 2014-2019 The Rust Project Developers

Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
<LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
option. All files in the project carrying such notice may not be
copied, modified, or distributed except according to those terms.
