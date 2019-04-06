//-
// Copyright 2017, 2018 The proptest developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use core::any::Any;
use std_facade::{fmt, Box, Vec};

use test_runner::failure_persistence::FailurePersistence;
use test_runner::Seed;

/// Failure persistence option that loads and saves nothing at all.
#[derive(Debug, Default, PartialEq)]
struct NoopFailurePersistence;

impl FailurePersistence for NoopFailurePersistence {
    fn load_persisted_failures(&self, _source_file: Option<&'static str>) -> Vec<Seed> {
        Vec::new()
    }

    fn save_persisted_failure(&mut self,
        _source_file: Option<&'static str>,
        _seed: Seed,
        _shrunken_value: &dyn fmt::Debug,
    ) {
    }

    fn box_clone(&self) -> Box<dyn FailurePersistence> {
        Box::new(NoopFailurePersistence)
    }

    fn eq(&self, other: &dyn FailurePersistence) -> bool {
        other.as_any().downcast_ref::<Self>().map_or(false, |x| x == self)
    }

    fn as_any(&self) -> &dyn Any { self }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_runner::failure_persistence::tests::*;

    #[test]
    fn default_load_is_empty() {
        assert!(NoopFailurePersistence::default()
                    .load_persisted_failures(None).is_empty());
        assert!(NoopFailurePersistence::default()
                    .load_persisted_failures(HI_PATH).is_empty());
    }

    #[test]
    fn seeds_not_recoverable() {
        let mut p = NoopFailurePersistence::default();
        p.save_persisted_failure(HI_PATH, INC_SEED, &"");
        assert!(p.load_persisted_failures(HI_PATH).is_empty());
        assert!(p.load_persisted_failures(None).is_empty());
        assert!(p.load_persisted_failures(UNREL_PATH).is_empty());
    }
}
