//! Channels with send and receive operations implemented using strictly non-blocking `select!`.
//!
//! Such `select!` invocations will often try to optimize the macro invocations by converting them
//! into special non-blocking method calls like `try_recv()`.

use std::ops::Deref;
use std::thread;
use std::time::{Duration, Instant};

use channel;

pub struct Sender<T>(pub channel::Sender<T>);

pub struct Receiver<T>(pub channel::Receiver<T>);

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Sender<T> {
        Sender(self.0.clone())
    }
}

impl<T> Clone for Receiver<T> {
    fn clone(&self) -> Receiver<T> {
        Receiver(self.0.clone())
    }
}

impl<T> Deref for Receiver<T> {
    type Target = channel::Receiver<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Deref for Sender<T> {
    type Target = channel::Sender<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Sender<T> {
    pub fn send(&self, msg: T) {
        if self.0.capacity() == Some(0) {
            // Zero-capacity channel are an exception - they need truly blocking operations.
            select! {
                send(self.0, msg) => {}
            }
        } else {
            loop {
                select! {
                    send(self.0, msg) => break,
                    default => thread::yield_now(),
                }
            }
        }
    }
}

impl<T> Receiver<T> {
    pub fn try_recv(&self) -> Option<T> {
        select! {
            recv(self.0, msg) => msg,
            default => None,
        }
    }

    pub fn recv(&self) -> Option<T> {
        if self.0.capacity() == Some(0) {
            // Zero-capacity channel are an exception - they need truly blocking operations.
            select! {
                recv(self.0, msg) => msg,
            }
        } else {
            loop {
                select! {
                    recv(self.0, msg) => break msg,
                    default => thread::yield_now(),
                }
            }
        }
    }
}

pub fn bounded<T>(cap: usize) -> (Sender<T>, Receiver<T>) {
    let (s, r) = channel::bounded(cap);
    (Sender(s), Receiver(r))
}

pub fn unbounded<T>() -> (Sender<T>, Receiver<T>) {
    let (s, r) = channel::unbounded();
    (Sender(s), Receiver(r))
}

pub fn after(dur: Duration) -> Receiver<Instant> {
    let r = channel::after(dur);
    Receiver(r)
}

pub fn tick(dur: Duration) -> Receiver<Instant> {
    let r = channel::tick(dur);
    Receiver(r)
}
