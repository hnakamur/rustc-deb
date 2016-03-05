// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Can't use unit struct as enum pattern

// aux-build:empty-struct.rs

#![feature(rustc_attrs)]
// remove prior feature after warning cycle and promoting warnings to errors
#![feature(braced_empty_structs)]

extern crate empty_struct;
use empty_struct::*;

struct Empty2;

enum E {
    Empty4
}

// remove attribute after warning cycle and promoting warnings to errors
#[rustc_error]
fn main() { //~ ERROR: compilation successful
    let e2 = Empty2;
    let e4 = E::Empty4;
    let xe2 = XEmpty2;
    let xe4 = XE::XEmpty4;

    // Rejected by parser as yet
    // match e2 {
    //     Empty2() => () // ERROR `Empty2` does not name a tuple variant or a tuple struct
    // }
    // match xe2 {
    //     XEmpty2() => () // ERROR `XEmpty2` does not name a tuple variant or a tuple struct
    // }
    match e2 {
        Empty2(..) => () //~ WARN `Empty2` does not name a tuple variant or a tuple struct
            //~^ WARN hard error
    }
    match xe2 {
        XEmpty2(..) => () //~ WARN `XEmpty2` does not name a tuple variant or a tuple struct
            //~^ WARN hard error
    }
    // Rejected by parser as yet
    // match e4 {
    //     E::Empty4() => () // ERROR `E::Empty4` does not name a tuple variant or a tuple struct
    // }
    // match xe4 {
    //     XE::XEmpty4() => (), // ERROR `XE::XEmpty4` does not name a tuple variant or a tuple
    //     _ => {},
    // }
    match e4 {
        E::Empty4(..) => () //~ WARN `E::Empty4` does not name a tuple variant or a tuple struct
            //~^ WARN hard error
    }
    match xe4 {
        XE::XEmpty4(..) => (), //~ WARN `XE::XEmpty4` does not name a tuple variant or a tuple
            //~^ WARN hard error
        _ => {},
    }
}
