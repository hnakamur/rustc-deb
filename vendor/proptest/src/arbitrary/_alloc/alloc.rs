//-
// Copyright 2017, 2018 The proptest developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Arbitrary implementations for `std::hash`.

use core::cmp;
use core::ops::Range;
use core::usize;

multiplex_alloc!(alloc::alloc, ::std::alloc);

use strategy::*;
use strategy::statics::static_map;
use arbitrary::*;

arbitrary!(alloc::CannotReallocInPlace; alloc::CannotReallocInPlace);
arbitrary!(alloc::Global; alloc::Global);

// Not Debug.
//lazy_just!(System, || System);

arbitrary!(alloc::Layout, SFnPtrMap<(Range<u8>, StrategyFor<usize>), Self>;
    // 1. align must be a power of two and <= (1 << 31):
    // 2. "when rounded up to the nearest multiple of align, must not overflow".
    static_map((0u8..32u8, any::<usize>()), |(align_power, size)| {
        let align = 1usize << align_power;
        let max_size = 0usize.wrapping_sub(align);
        // Not quite a uniform distribution due to clamping,
        // but probably good enough
        alloc::Layout::from_size_align(cmp::min(max_size, size), align).unwrap()
    })
);

arbitrary!(alloc::AllocErr, Just<Self>; Just(alloc::AllocErr));
/* 2018-07-28 CollectionAllocErr is not currently available outside of using
 * the `alloc` crate, which would require a different nightly feature. For now,
 * disable.
arbitrary!(alloc::collections::CollectionAllocErr, TupleUnion<(W<Just<Self>>, W<Just<Self>>)>;
           prop_oneof![Just(alloc::collections::CollectionAllocErr::AllocErr),
                       Just(alloc::collections::CollectionAllocErr::CapacityOverflow)]);
 */

#[cfg(test)]
mod test {
    no_panic_test!(
        layout => alloc::Layout,
        alloc_err => alloc::AllocErr
        //collection_alloc_err => alloc::collections::CollectionAllocErr
    );
}
