//-
// Copyright 2017, 2018 The proptest developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use core::any::Any;
use std_facade::{fmt, Box, Vec, BTreeMap, BTreeSet};

use test_runner::failure_persistence::FailurePersistence;
use test_runner::Seed;

/// Failure persistence option that loads and saves seeds in memory
/// on the heap. This may be useful when accumulating test failures
/// across multiple `TestRunner` instances for external reporting
/// or batched persistence.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct MapFailurePersistence {
    /// Backing map, keyed by source_file.
    pub map: BTreeMap<&'static str, BTreeSet<Seed>>
}

impl FailurePersistence for MapFailurePersistence {
    fn load_persisted_failures(&self, source_file: Option<&'static str>)
                               -> Vec<Seed> {
        source_file
            .and_then(|source| self.map.get(source))
            .map(|seeds| seeds.iter().cloned().collect::<Vec<_>>())
            .unwrap_or_default()
    }

    fn save_persisted_failure(
        &mut self,
        source_file: Option<&'static str>,
        seed: Seed,
        _shrunken_value: &dyn fmt::Debug,
    ) {
        let s = match source_file {
            Some(sf) => sf,
            None => return
        };
        let set = self.map.entry(s).or_insert_with(BTreeSet::new);
        set.insert(seed);
    }

    fn box_clone(&self) -> Box<dyn FailurePersistence> {
        Box::new(self.clone())
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
    fn initial_map_is_empty() {
        assert!(MapFailurePersistence::default()
                    .load_persisted_failures(HI_PATH).is_empty())
    }

    #[test]
    fn seeds_recoverable() {
        let mut p = MapFailurePersistence::default();
        p.save_persisted_failure(HI_PATH, INC_SEED, &"");
        let restored = p.load_persisted_failures(HI_PATH);
        assert_eq!(1, restored.len());
        assert_eq!(INC_SEED, *restored.first().unwrap());

        assert!(p.load_persisted_failures(None).is_empty());
        assert!(p.load_persisted_failures(UNREL_PATH).is_empty());
    }

    #[test]
    fn seeds_deduplicated() {
        let mut p = MapFailurePersistence::default();
        p.save_persisted_failure(HI_PATH, INC_SEED, &"");
        p.save_persisted_failure(HI_PATH, INC_SEED, &"");
        let restored = p.load_persisted_failures(HI_PATH);
        assert_eq!(1, restored.len());
    }
}
