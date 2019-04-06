#![warn(clippy::implicit_return)]

fn test_end_of_fn() -> bool {
    if true {
        // no error!
        return true;
    }
    true
}

#[allow(clippy::needless_bool)]
fn test_if_block() -> bool {
    if true {
        true
    } else {
        false
    }
}

#[allow(clippy::match_bool)]
#[rustfmt::skip]
fn test_match(x: bool) -> bool {
    match x {
        true => false,
        false => { true },
    }
}

#[allow(clippy::never_loop)]
fn test_loop() -> bool {
    loop {
        break true;
    }
}

#[allow(clippy::never_loop)]
fn test_loop_with_block() -> bool {
    loop {
        {
            break true;
        }
    }
}

#[allow(clippy::never_loop)]
fn test_loop_with_nests() -> bool {
    loop {
        if true {
            break true;
        } else {
            let _ = true;
        }
    }
}

fn test_closure() {
    #[rustfmt::skip]
    let _ = || { true };
    let _ = || true;
}

fn main() {
    let _ = test_end_of_fn();
    let _ = test_if_block();
    let _ = test_match(true);
    let _ = test_loop();
    let _ = test_loop_with_block();
    let _ = test_loop_with_nests();
    test_closure();
}
