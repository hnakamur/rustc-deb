// A callee may not read the destination of our `&mut` without
// us noticing.

fn main() {
    let mut x = 15;
    let xraw = &mut x as *mut _;
    let xref = unsafe { &mut *xraw }; // derived from raw, so using raw is still okay...
    callee(xraw);
    let _val = *xref; // ...but any use of raw will invalidate our ref.
    //~^ ERROR: does not exist on the stack
}

fn callee(xraw: *mut i32) {
    // We are a bit sneaky: We first create a shared ref, exploiting the reborrowing rules,
    // and then we read through that.
    let shr = unsafe { &*xraw };
    let _val = *shr;
}
