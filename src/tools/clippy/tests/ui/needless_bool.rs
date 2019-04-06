#![warn(clippy::needless_bool)]

use std::cell::Cell;

macro_rules! bool_comparison_trigger {
    ($($i:ident: $def:expr, $stb:expr );+  $(;)*) => (

        #[derive(Clone)]
        pub struct Trigger {
            $($i: (Cell<bool>, bool, bool)),+
        }

        #[allow(dead_code)]
        impl Trigger {
            pub fn trigger(&self, key: &str) -> bool {
                $(
                    if let stringify!($i) = key {
                        return self.$i.1 && self.$i.2 == $def;
                    }
                 )+
                false
            }
        }
    )
}

#[allow(clippy::if_same_then_else)]
fn main() {
    let x = true;
    let y = false;
    if x {
        true
    } else {
        true
    };
    if x {
        false
    } else {
        false
    };
    if x {
        true
    } else {
        false
    };
    if x {
        false
    } else {
        true
    };
    if x && y {
        false
    } else {
        true
    };
    if x {
        x
    } else {
        false
    }; // would also be questionable, but we don't catch this yet
    bool_ret(x);
    bool_ret2(x);
    bool_ret3(x);
    bool_ret5(x, x);
    bool_ret4(x);
    bool_ret6(x, x);
    needless_bool(x);
    needless_bool2(x);
    needless_bool3(x);
}

#[allow(clippy::if_same_then_else, clippy::needless_return)]
fn bool_ret(x: bool) -> bool {
    if x {
        return true;
    } else {
        return true;
    };
}

#[allow(clippy::if_same_then_else, clippy::needless_return)]
fn bool_ret2(x: bool) -> bool {
    if x {
        return false;
    } else {
        return false;
    };
}

#[allow(clippy::needless_return)]
fn bool_ret3(x: bool) -> bool {
    if x {
        return true;
    } else {
        return false;
    };
}

#[allow(clippy::needless_return)]
fn bool_ret5(x: bool, y: bool) -> bool {
    if x && y {
        return true;
    } else {
        return false;
    };
}

#[allow(clippy::needless_return)]
fn bool_ret4(x: bool) -> bool {
    if x {
        return false;
    } else {
        return true;
    };
}

#[allow(clippy::needless_return)]
fn bool_ret6(x: bool, y: bool) -> bool {
    if x && y {
        return false;
    } else {
        return true;
    };
}

fn needless_bool(x: bool) {
    if x == true {};
}

fn needless_bool2(x: bool) {
    if x == false {};
}

fn needless_bool3(x: bool) {
    bool_comparison_trigger! {
        test_one:   false, false;
        test_three: false, false;
        test_two:   true, true;
    }

    if x == true {};
    if x == false {};
}
