#![feature(generators, generator_trait)]

use std::ops::Generator;

pub fn foo() -> impl Generator<Yield = (), Return = ()> {
    || {
        if false {
            yield;
        }
    }
}

pub fn bar<T: 'static>(t: T) -> Box<Generator<Yield = T, Return = ()>> {
    Box::new(|| {
        yield t;
    })
}
