## 0.8.7

### New Additions

- Add `max_shrink_iters` and `max_shrink_time` options to test configuration to
  allow capping the resources expended on shrinking test cases.

- Add `verbose` option to make proptest give details about what is happening as
  the test executes.

- When a failure is saved to the persistence file, the message now also
  includes the seed that was saved so that it can manually be added to the
  appropriate file should the test have run somewhere where the updated file is
  not accessible (for example, on a CI system).

### Bug Fixes

- `any::<SystemTime>()` now generates random values centred on the UNIX epoch
  rather than always producing the current time.

### Other Notes

- When using forking, proptest will now detect conditions which cause the child
  process to crash without running any tests, and will fail quickly instead of
  respawning child processes.

## 0.8.6

### New Additions

- `Vec<S> where S: Strategy` is now itself a `Strategy` for producing
  fixed-size `Vec`s whose values are derived from the respective strategies.

- It is now possible to configure the test runner to cache test results to
  avoid spending time running identical tests. See `Config.result_cache`.

- Add `sample::Index`, a type for generating indices into runtime-sized slices
  and vectors.

- Add `sample::Selector`, a type for picking items out of dynamically-created
  iterables.

### Bug Fixes

- Fix panic when using `sample::subsequence` with an empty vector.

- Fix panic when using `sample::subsequence` with a size equal to the size of
  the input vector.

- Fix sampled bitset strategies on integers not allowing to generate exactly
  the same number of bits as the integer is wide.

### Other Notes

- Passing empty size ranges to functions requiring a non-empty size range now
  panic with an explicit message immediately rather than causing an arithmetic
  error when generating input values.

- There were a few cases where proptest would accept a `SizeRange` with an
  inclusive maximum value of `usize::MAX`. Size ranges are now always clamped
  to `usize::MAX - 1`.

## 0.8.5

### Bug Fixes

- Fix build when nightly features are enabled.

## 0.8.4

### Bug Fixes

- Nightly and no_std support work against latest nightly once again.

### New Additions

- Added `bits::bool_vec` for generating `Vec<bool>` as a bit set.

### Nightly-only breakage

- `impl Arbitrary for CollectionAllocErr` is temporarily removed pending it
  being available outside the `alloc` crate again.

- `bits::bitset` is no longer available without the `bit-set` feature (enabled
  by default), which is [not compatible with `#[no_std]`
  environments](https://github.com/contain-rs/bit-vec/pull/51).

## 0.8.3

### Bug Fixes

- Fix that regex-based string generation could transpose the order of a literal
  and a non-literal component.

## 0.8.2

### New Additions

- Macros which previously accepted `pattern in strategy` syntax to specify
  arguments now also accept `pattern: type` syntax as shorthand for
  `pattern in any::<type>()`.

- Closure-style `proptest!` invocation no longer requires the body to use block
  syntax.

- Closure-style `proptest!` invocation now accepts custom configurations.

## 0.8.1

### New Additions

- `proptest!` now has form that accepts a closure. See the documentation for
  the macro for more details.

### Bug Fixes

- Fix spurious warning about corrupt regression files. The files were not
  corrupt but the parser was failing to handle the blank line at the end.

- The `multiplex_alloc!` and `multiplex_core!` macros which were
  unintentionally exported in 0.8.0 are no longer exported. This is not
  considered a breaking change since they were not supposed to be accessible,
  and in any case would not have expanded into valid code in most other crates.

## 0.8.0

### New Additions

- A combinator `.prop_filter_map` has been added to `Strategy`.
  It is similar to `.filter_map` for `Iterator` in that it is the
  combination of `.prop_filter` and `.prop_map`.

- `i128` and `u128` are now supported without any feature flags and on stable.

- More implementations of `Arbitrary` are supported for `alloc` + `no_std` users.

- `size_range` now accepts inclusive ranges of form `low..=high` and `..=high`.
  Thus, you can construct a `vec` strategy as: `vec(elt_strategy, low..=high)`
  and `vec(elt_strategy, ..=high)`. This also applies to other functions
  accepting `Into<SizeRange>`.

- `..= high` is now a valid strategy. Please note that `..= 1` will naturally
  include numbers lower than `0` for sized types.

- `low..=high` is also a valid strategy.

- `Arbitrary` is implemented for `RangeInclusive<Idx>`, `RangeToInclusive`,
  and `DecodeUtf16` on stable.

- Bitset strategies and `subsequence` now accept all range syntaxes.

### Bug Fixes

- Fix a race condition where a test failing due to running ever so slightly
  over the set timeout could cause the test harness to converge to the
  incorrect failing value, a non-failing value, or panic.

### Deprecations

- The type alias `ValueFor<S>` is now deprecated and will be removed in
  version 0.9. You should just use `S::Value` instead.

### Breaking changes

- A minimum version of 1.27 of Rust is now required.

- `regex-syntax` version 0.6 is now used.

- `rand` version 0.5 is now used.

- As a consequence, the `FailurePersistence` trait will now use `[u8; 16]` seeds
  instead of `[u32; 4]`. However, the stored failure persistence files using
  the default `FileFailurePersistence` will still use `[u32; 4]` so your old
  failure persistence files should still work.

- The RNG used by proptest has been changed to a PRNG `TestRng` which proptest
  exposes. This is currently a simple new-type wrapper around `XorShiftRng`.
  In the future, this will give us more freedom to make changes without breakage.

- The feature flag `i128_support` has been removed. The features it added are
  now always supported.

- The associated type `Value` of `Strategy` has been renamed to `Tree`.
  A new associated type `Value` has been added to `Strategy` which always refers
  to the same type as `<S::Tree as ValueTree>::Value` for some strategy `S`.
  This change allows you to write `-> impl Strategy<Value = T>` for functions
  returning a `Strategy` generating `T`s. This is more ergonomic to use than
  `-> impl Strategy<Value = impl ValueTree<Value = T>>`.

- The method `new_value` in `Strategy` has been renamed to `new_tree` to mirror
  the renaming of `Value` to `Tree`.

- As a consequence change, the associated type `ValueTree` has been removed from
  `Arbitrary`.

- The methods `run` and `run_one` on `TestRunner` now takes a function-under-test
  that accepts the generated type by value instead of by reference instead.
  This means that you don't need to write `ref value in my_strategy` and can
  write `value in my_strategy` instead even if `typeof(value)` doesn't implement
  `Copy`. This is also a step in the direction of allowing strategies to generate
  references when generic associated types (GATs) land.
  However, `ref value in my_strategy` will still be accepted, so not a lot of
  breakage should come of this if you've used `proptest! { .. }`.

- `prop_compose!` no longer applies `.boxed()` to the strategy produced.
  Therefore, `-> BoxedStrategy<T>` is no longer the correct type.
  The new return type is `-> impl Strategy<Value = T>`.
  If you want the old behaviour, you can use `.boxed()` yourself.

- `Arbitrary` for `SizeRange` changed its associated type to use `RangeInclusive`.
  Same applies for `CString`.

- Many APIs now use `impl Trait` in argument position, which could affect code
  using turbofishes to specify types explicitly.

- `char` APIs which formerly represented a range as `(start, end)` now require
  `start..=end`.

### Nightly-only breakage

- As `std::io::{Chars, CharsError}` have been deprecated on nightly,
  their `Arbitrary` implementations have been removed.

## 0.7.2

### Bug Fixes

- Fix that `bool` would not shrink correctly, leading to hangs when tests
  taking `bool` parameters would fail in certain circumstances.

## 0.7.1

### New Additions

- It is now possible to run test cases in sub-processes. This allows using
  proptest to test functions which may cause the test process to terminate
  abruptly, such as by calling `abort()` or even suffering a segmentation
  fault. This requires the "fork" feature, enabled by default.

- Added support for setting a timeout which applies on a per-test-case (i.e.,
  single input rather than the whole test) basis. This allows using proptest to
  find inputs which cause code to get stuck in infinite loops or exhibit other
  pathological performance behaviour. This requires the "timeout" feature (and
  transitively, the "fork" feature), enabled by default.

See also [the documentation](README.md#forking-and-timeouts) for these
features.

### Bug Fixes

- Fix that failure persistence file would be written to the incorrect location
  in projects using workspaces. See
  [#24](https://github.com/AltSysrq/proptest/issues/24) for more details and
  instructions on how to migrate any persistence files that had been written to
  the wrong location.

- Fix a case where `any::<ArgsOs>()` or `any::<VarsOs>()` could panic on
  Windows.

### Nightly-only breakage

- Support for the `hashmap_core` crate is removed pending
  https://github.com/Amanieu/hashmap_core/issues/3.

## 0.7.0

### Potential Breaking Changes

- The persistence system has been refactored to allow for
  non-file-system based persistence. `FailurePersistence`
  is now a trait, and the prior file-based enum which fulfilled
  that purpose is now called `FileFailurePersistence` and implements
  the generic trait. The default behavior has not changed.

- Reflecting the change to persistence, `Config.failure_persistence`
  is now of type `Option<Box<FailurePersistence>>`.

- The `source_file` used as an optional reference point to the location of the
  calling test is now tracked on the `Config` struct rather than the
  `TestRunner`.

### New Additions

- Experimental support on nightly for working in `#![no_std]` environments has
  been added. To use it, one must disable the default-features for proptest and
  use the new "alloc" and "nightly" features. Currently access to a heap
  allocator is still required.

## 0.6.0

### Potential Breaking Changes

- There is a small change of breakage if you've relied on `Recursive` using an
  `Arc<BoxedStrategy<T>>` as `Recursive` now internally uses `BoxedStrategy<T>`
  instead as well as expecting a `Fn(BoxedStrategy<T>) -> R` instead of
  `Fn(Arc<BoxedStrategy<T>>) -> R`. In addition, the type of recursive
  strategies has changed from `Recursive<BoxedStrategy<T>, F>` to just
  `Recursive<T, F>`.

### Minor changes

- Reduced indirections and heap allocations inside `Recursive<T, F>` somewhat.

- `BoxedStrategy<T>` and `SBoxedStrategy<T>` now use `Arc` internally instead of
  using `Box`. While this has marginal overhead, it also reduces the overhead
  in `Recursive<T, F>`. The upside to this change is also that you can very
  cheaply clone strategies.

- `Filter` is marginally faster.

### Bug Fixes

- Removed `impl Arbitrary for LocalKeyState` since `LocalKeyState` no longer
  exists in the nightly compiler.

- Unstable features compile on latest nightly again.

## 0.5.1

### New Additions

- `proptest::strategy::Union` and `proptest::strategy::TupleUnion` now work
  with weighted strategies even if the sum of the weights overflows a `u32`.

- Added `SIGNALING_NAN` strategy to generate signalling NaNs if supported by
  the platform. Note that this is _not_ included in `ANY`.

### Bug Fixes

- Fixed values produced via `prop_recursive()` not shrinking from the recursive
  to the non-recursive case.

- Fix that `QUIET_NAN` would generate signalling NaNs on most platforms on Rust
  1.24.0 and later.

## 0.5.0

### Potential Breaking Changes

- There is a small chance of breakage if you've relied on the constraints put
  on type inference by the closure in `leaf.prop_recursive(..)` having a fixed
  output type. The output type is now any strategy that generates the same type
  as `leaf`. This change is intended to make working with recursive types a bit
  easier as you no longer have to use `.boxed()` inside the closure you pass to
  `.prop_recursive(..)`.

- There is a small chance of breakage wrt. type inference due to the
  introduction of `SizeRange`.

- There is a small chance of breakage wrt. type inference due to the
  introduction of `Probability`.

- `BoxedStrategy` and `SBoxedStrategy` are now newtypes instead of being type
  aliases. You will only experience breaking changes if you've directly
  used `.boxed()` and not `(S)BoxedStrategy<T>` but rather
  `Box<Strategy<Value = Box<ValueTree<Value = T>>>>`. The probability of
  breakage is very small, but still possible. The benefit of this change
  is that calling `.boxed()` or `.sboxed()` twice only boxes once. This can
  happen in situations where you have functions `Strategy -> BoxedStrategy` or
  with code generation.

- `proptest::char::ANY` has been removed. Any remaining uses must be replaced
  by `proptest::char::any()`.

- `proptest::strategy::Singleton` has been removed. Any remaining uses must be
  replaced by `proptest::strategy::Just`.

### New Additions

- Proptest now has an `Arbitrary` trait in `proptest::arbitrary` and re-exported
  in the `proptest::prelude`. `Arbitrary` has also been `impl`emented for most
  of the standard library. The trait provides a mechanism to define a canonical
  `Strategy` for a given type just like `Arbitrary` in Haskell's QuickCheck.
  Deriving for this trait will also be provided soon in the crate
  `proptest_derive`. To use the canonical strategy for a certain type `T`,
  you can simply use `any::<T>()`. This is the major new addition of this release.

- The `any_with`, `arbitrary`, `arbitrary_with` free functions in
  the module `proptest::arbitrary`.

- The `ArbitraryF1` and `ArbitraryF2` traits in `proptest::arbitrary::functor`.
  These are "higher order" `Arbitrary` traits that correspond to the `Arbitrary1`
  and `Arbitrary2` type classes in Haskell's QuickCheck. They are mainly provided
  to support a common set of container-like types in custom deriving self-recursive
  types in  `proptest_derive`. More on this later releases.

- The strategies in `proptest::option` and `proptest::result` now accept a type
  `Probability` which is a wrapper around `f64`. Convertions from types such as
  `f64` are provided to make the interface ergonomic to use. Users may also use
  the `proptest::option::prob` function to explicitly construct the type.

- The strategies in `proptest::collections` now accept a type `SizeRange`
  which is a wrapper around `Range<usize>`. Convertions from types
  such as `usize` and `Range<usize>` are provided to make the interface
  ergonomic to use. Users may also use the `proptest::collections::size_bounds`
  function to explicitly construct the type.

- A `.prop_map_into()` operation on all strategies that map
  using `Into<OutputType>`. This is a clerarer and cheaper
  operation than using `.prop_map(OutputType::from)`.

- A nonshrinking `LazyJust` strategy that can be used instead of `Just` when you
  have non-`Clone` types.

- Anything that can be coerced to `fn() -> T` where `T: Debug` is a `Strategy`
  where `ValueFor<fn() -> T> == T`. This is intended to make it easier to reuse
  proptest for unit tests with manual input space partition where `fn() -> T`
  provides fixtures.

### Minor changes

- Relaxed the constraints of `btree_map` removing `'static`.

- Reduced the heap allocation inside `Recursive` somewhat.

## 0.4.2

### Bug Fixes

- The `unstable` feature now works again.

## 0.4.1

### New Additions

- The `proptest::num::f32` and `proptest::num::f64` modules now have additional
  constants (e.g., `POSITIVE`, `SUBNORMAL`, `INFINITE`) which can be used to
  generate subsets of the floating-point domain by class and sign.

### Bug Fixes

- `proptest::num::f32::ANY` and `proptest::num::f64::ANY` now actually produce
  arbitrary values. Previously, they had the same effect as `0.0..1.0`. While
  this fix is a very substantial change in behaviour, it was not considered a
  breaking change since (a) the new behaviour is consistent with the
  documentation and expectations, (b) it's quite unlikely anyone was depending
  on the old behaviour since anyone who wanted that range would have written it
  out, and (c) Proptest isn't generally a transitive dependency so the chance
  of this update happening "by surprise" is low.

## 0.4.0

### Deprecations and Potential Breaking Changes

- `proptest::char::ANY` replaced with `proptest::char::any()`.
  `proptest::char::ANY` is present but deprecated, and will be removed in
  proptest 0.5.0.

- Instead of returning `-> Result<Self::Value, String>`, strategies are
  expected to return `-> Result<Self::Value, Reason>` instead. `Reason` reduces
  the amount of heap allocations, especially for `.prop_filter(..)` where you
  may now also pass in `&'static str`. You will only experience breaks if
  you've written your own strategy types or if you've used
  `TestCaseError::Reject` or `TestCaseError::Fail` explicitly.

- Update of externally-visible crate `rand` to `0.4.2`.

### New Additions

- Added `proptest::test_runner::Reason` which allows you to avoid heap
  allocation in some places and may be used to make the API richer in the
  future without incurring more breaking changes.

- Added a type alias `proptest::strategy::NewTree<S>` where `S: Strategy`
  defined as: `type NewTree<S> = Result<<S as Strategy>::Value, Rejection>`.

## 0.3.4

### Bug Fixes

- Cases where `file!()` returns a relative path, such as on Windows, are now
  handled more reasonably. See
  [#24](https://github.com/AltSysrq/proptest/issues/24) for more details and
  instructions on how to migrate any persistence files that had been written to
  the wrong location.

## 0.3.3

Boxing Day Special

### New Additions

- Added support for `i128` and `u128`. Since this is an unstable feature in
  Rust, this is hidden behind the feature `unstable` which you have to
  explicitly opt into in your `Cargo.toml` file.

- Failing case persistence. By default, when a test fails, Proptest will now
  save the seed for the failing test to a file, and later runs will test the
  persisted failing cases before generating new ones.

- Added `UniformArrayStrategy` and helper functions to simplify generating
  homogeneous arrays with non-`Copy` inner strategies.

- Trait `rand::Rng` and struct `rand::XorShiftRng` are now included in
  `proptest::prelude`.

### Bug Fixes

- Fix a case where certain combinations of strategies, like two
  `prop_shuffle()`s in close proximity, could result in low-quality randomness.

## 0.3.2

### New Additions

- Added `SampledBitSetStrategy` to generate bit sets based on size
  distribution.

- Added `Strategy::sboxed()` and `SBoxedStrategy` to make `Send + Sync` boxed
  strategies.

- `RegexGeneratorStrategy` is now `Send` and `Sync`.

- Added a type alias `ValueFor<S>` where `S: Strategy`. This is a shorter way
  to refer to: `<<S as Strategy>::Value as ValueTree>::Value`.

- Added a type alias `type W<T> = (u32, T)` for a weighted strategy `T` in the
  context of union strategies.

- `TestRunner` now implements `Default`.

- Added `Config::with_cases(number_of_cases: u32) -> Config` for simpler
  construction of a `Config` that only differs by the number of test cases.

- All default fields of `Config` can now be overridden by setting environment
  variables. See the docs of that struct for more details.

- Bumped dependency `rand = "0.3.18"`.

- Added `proptest::sample::subsequence` which returns a strategy generating
  subsequences, of the source `Vec`, with a size within the given `Range`.

- Added `proptest::sample::select` which returns a strategy selecting exactly
  one value from another collection.

- Added `prop_perturb` strategy combinator.

- Added `strategy::check_strategy_sanity()` function to do sanity checks on the
  shrinking implementation of a strategy.

- Added `prop_shuffle` strategy combinator.

- Added `strategy::Fuse` adaptor.

### Bug Fixes

- Fix bug where `Vec`, array and tuple shrinking could corrupt the state of
  their inner values, for example leading to out-of-range integers.

- Fix bug where `Flatten` (a.k.a. the `prop_flat_map` combinator) could fail to
  converge to a failing test case during shrinking.

- Fix `TupleUnion` sometimes panicking during shrinking if there were more than
  two choices.

## 0.3.1

### New Additions

- Added `CharStrategy::new_borrowed`.

## 0.3.0

### New Additions

- `Union` now supports weighting via `Union::new_weighted`. Corresponding
  syntax to specify weights is also available in `prop_oneof!`.

- Added `TupleUnion`, which works like `Union` but permits doing static
  dispatch even with heterogeneous delegate strategies.

- `prop_oneof!` is smarter about how it combines the input strategies.

- Added `option` module to generate weighted or unweighted `Option` types.

- Added `result` module to generate weighted or unweighted `Result` types.

- All `bits` submodules now have a `masked` function to create a strategy for
  generating subsets of an arbitrary bitmask.

### Potential Breaking Changes

- `Union::new` now has a generic argument type which could impact type
  inference.

- The concrete types produced by `prop_oneof!` have changed.

- API functions which used to return `BoxedStrategy` now return a specific
  type.

- `BitSetStrategy<T>` is no longer `Copy` for non-`Copy` types `T` nor `Debug`
  for non-`Debug` types `T`.

- `BitSetLike::max` has been renamed to `BitSetLike::len`.

## 0.2.1

### New Additions

- Added `prop_assert!` macro family to assert without panicking, for quieter
  test failure modes.

- New `prelude` module for easier importing of important things.

- Renamed `Singleton` to `Just`. (The old name is still available.)

- Failure messages produced by `proptest!` are now much more readable.

- Added in-depth tutorial.

## 0.2.0

### Breaking Changes

- `Strategy` now requires `std::fmt::Debug`.

### New Additions

- `Strategy` now has a family of `prop_flat_map()` combinators for producing
  dynamic and higher-order strategies.

- `Strategy` has a `prop_recursive()` combinator which allows generating
  recursive structures.

- Added `proptest::bool::weighted()` to pull booleans from a weighted
  distribution.

- New `prop_oneof!` macro makes it easier to select from one of several
  strategies.

- New `prop_compose!` macro to simplify writing most types of custom
  strategies.

## 0.1.1

### New Additions

Add `strategy::NoShrink`, `Strategy::no_shrink()`.
