#![feature(generic_associated_types)]
//~^ WARNING the feature `generic_associated_types` is incomplete

trait Foo {
    type Bar<,>;
    //~^ ERROR expected one of `>`, identifier, or lifetime, found `,`
}

fn main() {}
