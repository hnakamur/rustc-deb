// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Based on Isaac Gouy's fannkuchredux.csharp
extern mod std;

fn fannkuch(n: int) -> int {
    fn perm1init(i: uint) -> int { return i as int; }

    let mut perm = vec::from_elem(n as uint, 0);
    let mut perm1 = vec::from_fn(n as uint, |i| perm1init(i));
    let mut count = vec::from_elem(n as uint, 0);
    let mut f = 0;
    let mut i = 0;
    let mut k = 0;
    let mut r = 0;
    let mut flips = 0;
    let mut nperm = 0;
    let mut checksum = 0;
    r = n;
    while r > 0 {
        i = 0;
        while r != 1 { count[r - 1] = r; r -= 1; }
        while i < n { perm[i] = perm1[i]; i += 1; }
        // Count flips and update max and checksum

        f = 0;
        k = perm[0];
        while k != 0 {
            i = 0;
            while 2 * i < k {
                let t = perm[i];
                perm[i] = perm[k - i];
                perm[k - i] = t;
                i += 1;
            }
            k = perm[0];
            f += 1;
        }
        if f > flips { flips = f; }
        if nperm & 0x1 == 0 { checksum += f; } else { checksum -= f; }
        // Use incremental change to generate another permutation

        let mut go = true;
        while go {
            if r == n {
                io::println(fmt!("%d", checksum));
                return flips;
            }
            let p0 = perm1[0];
            i = 0;
            while i < r { let j = i + 1; perm1[i] = perm1[j]; i = j; }
            perm1[r] = p0;
            count[r] -= 1;
            if count[r] > 0 { go = false; } else { r += 1; }
        }
        nperm += 1;
    }
    return flips;
}

fn main() {
    let args = os::args();
    let args = if os::getenv(~"RUST_BENCH").is_some() {
        ~[~"", ~"10"]
    } else if args.len() <= 1u {
        ~[~"", ~"8"]
    } else {
        args
    };

    let n = int::from_str(args[1]).get();
    io::println(fmt!("Pfannkuchen(%d) = %d", n, fannkuch(n)));
}
