//-
// Copyright 2017, 2018 The proptest developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Arbitrary implementations for `std::ascii`.

use core::ascii::{EscapeDefault, escape_default};

use strategy::statics::static_map;
use arbitrary::*;

arbitrary!(EscapeDefault, SMapped<u8, Self>;
    static_map(any::<u8>(), escape_default));

#[cfg(test)]
mod test {
    no_panic_test!(escape_default => EscapeDefault);
}
