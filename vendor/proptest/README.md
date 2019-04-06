# Proptest

[![Build Status](https://travis-ci.org/AltSysrq/proptest.svg?branch=master)](https://travis-ci.org/AltSysrq/proptest)
[![Build status](https://ci.appveyor.com/api/projects/status/ofe98xfthbx1m608/branch/master?svg=true)](https://ci.appveyor.com/project/AltSysrq/proptest/branch/master)
[![](http://meritbadge.herokuapp.com/proptest)](https://crates.io/crates/proptest)

Proptest is a property testing framework (i.e., the QuickCheck family)
inspired by the [Hypothesis](http://hypothesis.works/) framework for
Python. It allows to test that certain properties of your code hold for
arbitrary inputs, and if a failure is found, automatically finds the
minimal test case to reproduce the problem. Unlike QuickCheck, generation
and shrinking is defined on a per-value basis instead of per-type, which
makes it more flexible and simplifies composition.

If you have dependencies which provide QuickCheck `Arbitrary`
implementations, see also the related
[`proptest-quickcheck-interop`](https://crates.io/crates/proptest-quickcheck-interop)
crates which enables reusing those implementations with proptest.

## Status of this crate

The majority of the functionality offered by proptest is in active use and
is known to work well.

The API is unlikely to see drastic breaking changes, but there may still be
minor breaking changes here and there, particularly when "impl Trait"
becomes stable and after the upcoming redesign of the `rand` crate.

See the [changelog](https://github.com/AltSysrq/proptest/blob/master/CHANGELOG.md)
for a full list of substantial historical changes, breaking and otherwise.

## Introduction

_Property testing_ is a system of testing code by checking that certain
properties of its output or behaviour are fulfilled for all inputs. These
inputs are generated automatically, and, critically, when a failing input
is found, the input is automatically reduced to a _minimal_ test case.

Property testing is best used to compliment traditional unit testing (i.e.,
using specific inputs chosen by hand). Traditional tests can test specific
known edge cases, simple inputs, and inputs that were known in the past to
reveal bugs, whereas property tests will search for more complicated inputs
that cause problems.

## Getting Started

Let's say we want to make a function that parses dates of the form
`YYYY-MM-DD`. We're not going to worry about _validating_ the date, any
triple of integers is fine. So let's bang something out real quick.

```rust
fn parse_date(s: &str) -> Option<(u32, u32, u32)> {
    if 10 != s.len() { return None; }
    if "-" != &s[4..5] || "-" != &s[7..8] { return None; }

    let year = &s[0..4];
    let month = &s[6..7];
    let day = &s[8..10];

    year.parse::<u32>().ok().and_then(
        |y| month.parse::<u32>().ok().and_then(
            |m| day.parse::<u32>().ok().map(
                |d| (y, m, d))))
}
```

It compiles, that means it works, right? Maybe not, let's add some tests.

```rust
#[test]
fn test_parse_date() {
    assert_eq!(None, parse_date("2017-06-1"));
    assert_eq!(None, parse_date("2017-06-170"));
    assert_eq!(None, parse_date("2017006-17"));
    assert_eq!(None, parse_date("2017-06017"));
    assert_eq!(Some((2017, 06, 17)), parse_date("2017-06-17"));
}
```

Tests pass, deploy to production! But now your application starts crashing,
and people are upset that you moved Christmas to February. Maybe we need to
be a bit more thorough.

In `Cargo.toml`, add

```toml
[dev-dependencies]
proptest = "0.8.7"
```

and at the top of `main.rs` or `lib.rs`:

```rust
#[macro_use] extern crate proptest;
```

Now we can add some property tests to our date parser. But how do we test
the date parser for arbitrary inputs, without making another date parser in
the test to validate it? We won't need to as long as we choose our inputs
and properties correctly. But before correctness, there's actually an even
simpler property to test: _The function should not crash._ Let's start
there.

```rust
proptest! {
    #[test]
    fn doesnt_crash(s in "\\PC*") {
        parse_date(s);
    }
}
```

What this does is take a literally random `&String` (ignore `\\PC*` for the
moment, we'll get back to that — if you've already figured it out, contain
your excitement for a bit) and give it to `parse_date()` and then throw the
output away.

When we run this, we get a bunch of scary-looking output, eventually ending
with

```text
thread 'main' panicked at 'Test failed: byte index 4 is not a char boundary; it is inside 'ௗ' (bytes 2..5) of `aAௗ0㌀0`; minimal failing input: s = "aAௗ0㌀0"
	successes: 102
	local rejects: 0
	global rejects: 0
'
```

If we look at the top directory after the test fails, we'll see a new
`proptest-regressions` directory, which contains some files corresponding
to source files containing failing test cases. These are [_failure
persistence_](#failure-persistence) files. The first thing we should do is
add these to source control.

```text
$ git add proptest-regressions
```

The next thing we should do is copy the failing case to a traditional unit
test since it has exposed a bug not similar to what we've tested in the
past.

```rust
#[test]
fn test_unicode_gibberish() {
    assert_eq!(None, parse_date("aAௗ0㌀0"));
}
```

Now, let's see what happened... we forgot about UTF-8! You can't just
blindly slice strings since you could split a character, in this case that
Tamil diacritic placed atop other characters in the string.

In the interest of making the code changes as small as possible, we'll just
check that the string is ASCII and reject anything that isn't.

```rust
fn parse_date(s: &str) -> Option<(u32, u32, u32)> {
    if 10 != s.len() { return None; }

    // NEW: Ignore non-ASCII strings so we don't need to deal with Unicode.
    if !s.is_ascii() { return None; }

    if "-" != &s[4..5] || "-" != &s[7..8] { return None; }

    let year = &s[0..4];
    let month = &s[6..7];
    let day = &s[8..10];

    year.parse::<u32>().ok().and_then(
        |y| month.parse::<u32>().ok().and_then(
            |m| day.parse::<u32>().ok().map(
                |d| (y, m, d))))
}
```

The tests pass now! But we know there are still more problems, so let's
test more properties.

Another property we want from our code is that it parses every valid date.
We can add another test to the `proptest!` section:

```rust
proptest! {
    // snip...

    #[test]
    fn parses_all_valid_dates(s in "[0-9]{4}-[0-9]{2}-[0-9]{2}") {
        parse_date(s).unwrap();
    }
}
```

The thing to the right-hand side of `in` is actually a *regular
expression*, and `s` is chosen from strings which match it. So in our
previous test, `"\\PC*"` was generating arbitrary strings composed of
arbitrary non-control characters. Now, we generate things in the YYYY-MM-DD
format.

The new test passes, so let's move on to something else.

The final property we want to check is that the dates are actually parsed
_correctly_. Now, we can't do this by generating strings — we'd end up just
reimplementing the date parser in the test! Instead, we start from the
expected output, generate the string, and check that it gets parsed back.

```rust
proptest! {
    // snip...

    #[test]
    fn parses_date_back_to_original(y in 0u32..10000,
                                    m in 1u32..13, d in 1u32..32) {
        let (y2, m2, d2) = parse_date(
            &format!("{:04}-{:02}-{:02}", y, m, d)).unwrap();
        // prop_assert_eq! is basically the same as assert_eq!, but doesn't
        // cause a bunch of panic messages to be printed on intermediate
        // test failures. Which one to use is largely a matter of taste.
        prop_assert_eq!((y, m, d), (y2, m2, d2));
    }
}
```

Here, we see that besides regexes, we can use any expression which is a
`proptest::strategy::Strategy`, in this case, integer ranges.

The test fails when we run it. Though there's not much output this time.

```text
thread 'main' panicked at 'Test failed: assertion failed: `(left == right)` (left: `(0, 10, 1)`, right: `(0, 0, 1)`) at examples/dateparser_v2.rs:46; minimal failing input: y = 0, m = 10, d = 1
	successes: 2
	local rejects: 0
	global rejects: 0
', examples/dateparser_v2.rs:33
note: Run with `RUST_BACKTRACE=1` for a backtrace.
```

The failing input is `(y, m, d) = (0, 10, 1)`, which is a rather specific
output. Before thinking about why this breaks the code, let's look at what
proptest did to arrive at this value. At the start of our test function,
insert

```rust
    println!("y = {}, m = {}, d = {}", y, m, d);
```

Running the test again, we get something like this:

```text
y = 2497, m = 8, d = 27
y = 9641, m = 8, d = 18
y = 7360, m = 12, d = 20
y = 3680, m = 12, d = 20
y = 1840, m = 12, d = 20
y = 920, m = 12, d = 20
y = 460, m = 12, d = 20
y = 230, m = 12, d = 20
y = 115, m = 12, d = 20
y = 57, m = 12, d = 20
y = 28, m = 12, d = 20
y = 14, m = 12, d = 20
y = 7, m = 12, d = 20
y = 3, m = 12, d = 20
y = 1, m = 12, d = 20
y = 0, m = 12, d = 20
y = 0, m = 6, d = 20
y = 0, m = 9, d = 20
y = 0, m = 11, d = 20
y = 0, m = 10, d = 20
y = 0, m = 10, d = 10
y = 0, m = 10, d = 5
y = 0, m = 10, d = 3
y = 0, m = 10, d = 2
y = 0, m = 10, d = 1
```

The test failure message said there were two successful cases; we see these
at the very top, `2497-08-27` and `9641-08-18`. The next case,
`7360-12-20`, failed. There's nothing immediately obviously special about
this date. Fortunately, proptest reduced it to a much simpler case. First,
it rapidly reduced the `y` input to `0` at the beginning, and similarly
reduced the `d` input to the minimum allowable value of `1` at the end.
Between those two, though, we see something different: it tried to shrink
`12` to `6`, but then ended up raising it back up to `10`. This is because
the `0000-06-20` and `0000-09-20` test cases _passed_.

In the end, we get the date `0000-10-01`, which apparently gets parsed as
`0000-00-01`. Again, this failing case was added to the failure persistence
file, and we should add this as its own unit test:

```text
$ git add proptest-regressions
```

```rust
#[test]
fn test_october_first() {
    assert_eq!(Some(0, 10, 1), parse_date("0000-10-01"));
}
```

Now to figure out what's broken in the code. Even without the intermediate
input, we can say with reasonable confidence that the year and day parts
don't come into the picture since both were reduced to the minimum
allowable input. The month input was _not_, but was reduced to `10`. This
means we can infer that there's something special about `10` that doesn't
hold for `9`. In this case, that "special something" is being two digits
wide. In our code:

```rust
    let month = &s[6..7];
```

We were off by one, and need to use the range `5..7`. After fixing this,
the test passes.

The `proptest!` macro has some additional syntax, including for setting
configuration for things like the number of test cases to generate. See its
[documentation](https://altsysrq.github.io/rustdoc/proptest/latest/proptest/macro.proptest.html)
for more details.

There is a more in-depth tutorial
[in the crate documentation](https://altsysrq.github.io/rustdoc/proptest/latest/proptest/#in-depth-tutorial).

## Differences between QuickCheck and Proptest

QuickCheck and Proptest are similar in many ways: both generate random
inputs for a function to check certain properties, and automatically shrink
inputs to minimal failing cases.

The one big difference is that QuickCheck generates and shrinks values
based on type alone, whereas Proptest uses explicit `Strategy` objects. The
QuickCheck approach has a lot of disadvantages in comparison:

- QuickCheck can only define one generator and shrinker per type. If you
need a custom generation strategy, you need to wrap it in a newtype and
implement traits on that by hand. In Proptest, you can define arbitrarily
many different strategies for the same type, and there are plenty built-in.

- For the same reason, QuickCheck has a single "size" configuration that
tries to define the range of values generated. If you need an integer
between 0 and 100 and another between 0 and 1000, you probably need to do
another newtype. In Proptest, you can directly just express that you want a
`0..100` integer and a `0..1000` integer.

- Types in QuickCheck are not easily composable. Defining `Arbitrary` and
`Shrink` for a new struct which is simply produced by the composition of
its fields requires implementing both by hand, including a bidirectional
mapping between the struct and a tuple of its fields. In Proptest, you can
make a tuple of the desired components and then `prop_map` it into the
desired form. Shrinking happens automatically in terms of the input types.

- Because constraints on values cannot be expressed in QuickCheck,
generation and shrinking may lead to a lot of input rejections. Strategies
in Proptest are aware of simple constraints and do not generate or shrink
to values that violate them.

The author of Hypothesis also has an [article on this
topic](http://hypothesis.works/articles/integrated-shrinking/).

Of course, there's also some relative downsides that fall out of what
Proptest does differently:

- Generating complex values in Proptest can be up to an order of magnitude
slower than in QuickCheck. This is because QuickCheck performs stateless
shrinking based on the output value, whereas Proptest must hold on to all
the intermediate states and relationships in order for its richer shrinking
model to work.

## Limitations of Property Testing

Given infinite time, property testing will eventually explore the whole
input space to a test. However, time is not infinite, so only a randomly
sampled portion of the input space can be explored. This means that
property testing is extremely unlikely to find single-value edge cases in a
large space. For example, the following test will virtually always pass:

```rust
#[macro_use] extern crate proptest;
use proptest::prelude::*;

proptest! {
    #[test]
    fn i64_abs_is_never_negative(a: i64) {
        // This actually fails if a == i64::MIN, but randomly picking one
        // specific value out of 2⁶⁴ is overwhelmingly unlikely.
        assert!(a.abs() >= 0);
    }
}
```

Because of this, traditional unit testing with intelligently selected cases
is still necessary for many kinds of problems.

Similarly, in some cases it can be hard or impossible to define a strategy
which actually produces useful inputs. A strategy of `.{1,4096}` may be
great to fuzz a C parser, but is highly unlikely to produce anything that
makes it to a code generator.

## Failure Persistence

By default, when Proptest finds a failing test case, it _persists_ that
failing case in a file named after the source containing the failing test,
but in a separate directory tree rooted at `proptest-regressions`† . Later
runs of tests will replay those test cases before generating novel cases.
This ensures that the test will not fail on one run and then spuriously
pass on the next, and also exposes similar tests to the same
known-problematic input.

(†  If you do not have an obvious source directory, you may instead find
files next to the source files, with a different extension.)

It is recommended to check these files in to your source control so that
other test runners (e.g., collaborators or a CI system) also replay these
cases.

Note that, by default, all tests in the same crate will share that one
persistence file. If you have a very large number of tests, it may be
desirable to separate them into smaller groups so the number of extra test
cases that get run is reduced. This can be done by adjusting the
`failure_persistence` flag on `Config`.

There are two ways this persistence could theoretically be done.

The immediately obvious option is to persist a representation of the value
itself, for example by using Serde. While this has some advantages,
particularly being resistant to changes like tweaking the input strategy,
it also has a lot of problems. Most importantly, there is no way to
determine whether any given value is actually within the domain of the
strategy that produces it. Thus, some (likely extremely fragile) mechanism
to ensure that the strategy that produced the value exactly matches the one
in use in a test case would be required.

The other option is to store the _seed_ that was used to produce the
failing test case. This approach requires no support from the strategy or
the produced value. If the strategy in use differs from the one used to
produce failing case that was persisted, the seed may or may not produce
the problematic value, but nonetheless produces a valid value. Due to these
advantages, this is the approach Proptest uses.

## Forking and Timeouts

By default, proptest tests are run in-process and are allowed to run for
however long it takes them. This is resource-efficient and produces the
nicest test output, and for many use cases is sufficient. However, problems
like overflowing the stack, aborting the process, or getting stuck in an
infinite will simply break the entire test process and prevent proptest
from determining a minimal reproducible case.

As of version 0.7.1, proptest has optional "fork" and "timeout" features
(both enabled by default), which make it possible to run your test cases in
a subprocess and limit how long they may run. This is generally slower,
may make using a debugger more difficult, and makes test output harder to
interpret, but allows proptest to find and minimise test cases for these
situations as well.

To enable these features, simply set the `fork` and/or `timeout` fields on
the `Config`. (Setting `timeout` implies `fork`.)

Here is a simple example of using both features:

```rust
#[macro_use] extern crate proptest;
use proptest::prelude::*;

// The worst possible way to calculate Fibonacci numbers
fn fib(n: u64) -> u64 {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

proptest! {
    #![proptest_config(ProptestConfig {
        // Setting both fork and timeout is redundant since timeout implies
        // fork, but both are shown for clarity.
        fork: true,
        timeout: 1000,
        .. ProptestConfig::default()
    })]

    #[test]
    fn test_fib(n: u64) {
        // For large n, this will variously run for an extremely long time,
        // overflow the stack, or panic due to integer overflow.
        assert!(fib(n) >= n);
    }
}
```

The exact value of the test failure depends heavily on the performance of
the host system, the rust version, and compiler flags, but on the system
where it was originally tested, it found that the maximum value that
`fib()` could handle was 39, despite having dozens of processes dump core
due to stack overflow or time out along the way.

If you just want to run tests in subprocesses or with a timeout every now
and then, you can do that by setting the `PROPTEST_FORK` or
`PROPTEST_TIMEOUT` environment variables to alter the default
configuration. For example, on Unix,

```sh
# Run all the proptest tests in subprocesses with no timeout.
# Individual tests can still opt out by setting `fork: false` in their
# own configuration.
PROPTEST_FORK=true cargo test
# Run all the proptest tests in subprocesses with a 1 second timeout.
# Tests can still opt out or use a different timeout by setting `timeout: 0`
# or another timeout in their own configuration.
PROPTEST_TIMEOUT=1000 cargo test
```


# Acknowledgements

This crate wouldn't have come into existence had it not been for the [Rust port
of QuickCheck](https://github.com/burntsushi/quickcheck) and the
[`regex_generate`](https://github.com/CryptArchy/regex_generate) crate which
gave wonderful examples of what is possible.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
