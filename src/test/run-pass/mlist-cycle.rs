// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// xfail-test
// -*- rust -*-
extern mod std;

type cell = {c: @list};

enum list { link(@mut cell), nil, }

pub fn main() {
    let first: @cell = @mut {c: @nil()};
    let second: @cell = @mut {c: @link(first)};
    first._0 = @link(second);
    sys.rustrt.gc();
    let third: @cell = @mut {c: @nil()};
}
