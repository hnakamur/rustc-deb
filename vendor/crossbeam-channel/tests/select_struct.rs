//! Tests for the `Select` struct.

extern crate crossbeam;
extern crate crossbeam_channel as channel;

use std::any::Any;
use std::cell::Cell;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::time::Duration;

use channel::Select;

fn ms(ms: u64) -> Duration {
    Duration::from_millis(ms)
}

#[test]
fn smoke1() {
    let (s1, r1) = channel::unbounded::<usize>();
    let (s2, r2) = channel::unbounded::<usize>();

    s1.send(1);

    Select::new()
        .recv(&r1, |v| assert_eq!(v, Some(1)))
        .recv(&r2, |_| panic!())
        .wait();

    s2.send(2);

    Select::new()
        .recv(&r1, |_| panic!())
        .recv(&r2, |v| assert_eq!(v, Some(2)))
        .wait();
}

#[test]
fn smoke2() {
    let (_s1, r1) = channel::unbounded::<i32>();
    let (_s2, r2) = channel::unbounded::<i32>();
    let (_s3, r3) = channel::unbounded::<i32>();
    let (_s4, r4) = channel::unbounded::<i32>();
    let (s5, r5) = channel::unbounded::<i32>();

    s5.send(5);

    Select::new()
        .recv(&r1, |_| panic!())
        .recv(&r2, |_| panic!())
        .recv(&r3, |_| panic!())
        .recv(&r4, |_| panic!())
        .recv(&r5, |v| assert_eq!(v, Some(5)))
        .wait();
}

#[test]
fn closed() {
    let (s1, r1) = channel::unbounded::<i32>();
    let (s2, r2) = channel::unbounded::<i32>();

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            drop(s1);
            thread::sleep(ms(500));
            s2.send(5);
        });

        let after = channel::after(ms(1000));
        Select::new()
            .recv(&r1, |v| assert!(v.is_none()))
            .recv(&r2, |_| panic!())
            .recv(&after, |_| panic!())
            .wait();

        r2.recv().unwrap();
    });

    let after = channel::after(ms(1000));
    Select::new()
        .recv(&r1, |v| assert!(v.is_none()))
        .recv(&r2, |_| panic!())
        .recv(&after, |_| panic!())
        .wait();

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            thread::sleep(ms(500));
            drop(s2);
        });

        let after = channel::after(ms(1000));
        Select::new()
            .recv(&r2, |v| assert!(v.is_none()))
            .recv(&after, |_| panic!())
            .wait();
    });
}

#[test]
fn default() {
    let (s1, r1) = channel::unbounded::<i32>();
    let (s2, r2) = channel::unbounded::<i32>();

    Select::new()
        .recv(&r1, |_| panic!())
        .recv(&r2, |_| panic!())
        .default(|| ())
        .wait();

    drop(s1);

    Select::new()
        .recv(&r1, |v| assert!(v.is_none()))
        .recv(&r2, |_| panic!())
        .default(|| panic!())
        .wait();

    s2.send(2);

    Select::new()
        .recv(&r2, |v| assert_eq!(v, Some(2)))
        .default(|| panic!())
        .wait();

    Select::new()
        .recv(&r2, |_| panic!())
        .default(|| ())
        .wait();

    Select::new()
        .default(|| ())
        .wait();
}

#[test]
fn timeout() {
    let (_s1, r1) = channel::unbounded::<i32>();
    let (s2, r2) = channel::unbounded::<i32>();

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            thread::sleep(ms(1500));
            s2.send(2);
        });

        let after = channel::after(ms(1000));
        Select::new()
            .recv(&r1, |_| panic!())
            .recv(&r2, |_| panic!())
            .recv(&after, |_| ())
            .wait();

        let after = channel::after(ms(1000));
        Select::new()
            .recv(&r1, |_| panic!())
            .recv(&r2, |v| assert_eq!(v, Some(2)))
            .recv(&after, |_| panic!())
            .wait();
    });

    crossbeam::scope(|scope| {
        let (s, r) = channel::unbounded::<i32>();

        scope.spawn(move || {
            thread::sleep(ms(500));
            drop(s);
        });

        let after = channel::after(ms(1000));
        Select::new()
            .recv(&after, |_| {
                Select::new()
                    .recv(&r, |v| assert!(v.is_none()))
                    .default(|| panic!())
                    .wait();
            })
            .wait();
    });
}

#[test]
fn default_when_closed() {
    let (_, r) = channel::unbounded::<i32>();

    Select::new()
        .recv(&r, |v| assert!(v.is_none()))
        .default(|| panic!())
        .wait();

    let (_, r) = channel::unbounded::<i32>();

    let after = channel::after(ms(1000));
    Select::new()
        .recv(&r, |v| assert!(v.is_none()))
        .recv(&after, |_| panic!())
        .wait();
}

#[test]
fn unblocks() {
    let (s1, r1) = channel::bounded::<i32>(0);
    let (s2, r2) = channel::bounded::<i32>(0);

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            thread::sleep(ms(500));
            s2.send(2);
        });

        let after = channel::after(ms(1000));
        Select::new()
            .recv(&r1, |_| panic!())
            .recv(&r2, |v| assert_eq!(v, Some(2)))
            .recv(&after, |_| panic!())
            .wait();
    });

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            thread::sleep(ms(500));
            assert_eq!(r1.recv().unwrap(), 1);
        });

        let after = channel::after(ms(1000));
        Select::new()
            .send(&s1, || 1, || ())
            .send(&s2, || 2, || panic!())
            .recv(&after, |_| panic!())
            .wait();
    });
}

#[test]
fn both_ready() {
    let (s1, r1) = channel::bounded(0);
    let (s2, r2) = channel::bounded(0);

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            thread::sleep(ms(500));
            s1.send(1);
            assert_eq!(r2.recv().unwrap(), 2);
        });

        for _ in 0..2 {
            Select::new()
                .recv(&r1, |v| assert_eq!(v, Some(1)))
                .send(&s2, || 2, || ())
                .wait();
        }
    });
}

#[test]
fn loop_try() {
    const RUNS: usize = 20;

    for _ in 0..RUNS {
        let (s1, r1) = channel::bounded::<i32>(0);
        let (s2, r2) = channel::bounded::<i32>(0);
        let (s_end, r_end) = channel::bounded::<()>(0);

        crossbeam::scope(|scope| {
            scope.spawn(|| {
                loop {
                    let mut done = false;

                    Select::new()
                        .send(&s1, || 1, || done = true)
                        .default(|| ())
                        .wait();
                    if done {
                        break;
                    }

                    Select::new()
                        .recv(&r_end, |_| done = true)
                        .default(|| ())
                        .wait();
                    if done {
                        break;
                    }
                }
            });

            scope.spawn(|| {
                loop {
                    if let Some(x) = r2.try_recv() {
                        assert_eq!(x, 2);
                        break;
                    }

                    let mut done = false;
                    Select::new()
                        .recv(&r_end, |_| done = true)
                        .default(|| ())
                        .wait();
                    if done {
                        break;
                    }
                }
            });

            scope.spawn(|| {
                thread::sleep(ms(500));

                let after = channel::after(ms(1000));
                Select::new()
                    .recv(&r1, |v| assert_eq!(v, Some(1)))
                    .send(&s2, || 2, || ())
                    .recv(&after, |_| panic!())
                    .wait();

                drop(s_end);
            });
        });
    }
}

#[test]
fn cloning1() {
    crossbeam::scope(|scope| {
        let (s1, r1) = channel::unbounded::<i32>();
        let (_s2, r2) = channel::unbounded::<i32>();
        let (s3, r3) = channel::unbounded::<()>();

        scope.spawn(move || {
            r3.recv().unwrap();
            drop(s1.clone());
            assert_eq!(r3.try_recv(), None);
            s1.send(1);
            r3.recv().unwrap();
        });

        s3.send(());

        Select::new()
            .recv(&r1, |_| ())
            .recv(&r2, |_| ())
            .wait();

        s3.send(());
    });
}

#[test]
fn cloning2() {
    let (s1, r1) = channel::unbounded::<()>();
    let (s2, r2) = channel::unbounded::<()>();
    let (_s3, _r3) = channel::unbounded::<()>();

    crossbeam::scope(|scope| {
        scope.spawn(move || {
            Select::new()
                .recv(&r1, |_| panic!())
                .recv(&r2, |_| ())
                .wait();
        });

        thread::sleep(ms(500));
        drop(s1.clone());
        s2.send(());
    })
}

#[test]
fn preflight1() {
    let (s, r) = channel::unbounded();
    s.send(());

    Select::new()
        .recv(&r, |_| ())
        .wait();
}

#[test]
fn preflight2() {
    let (s, r) = channel::unbounded();
    drop(s.clone());
    s.send(());
    drop(s);

    Select::new()
        .recv(&r, |v| assert!(v.is_some()))
        .wait();
    assert_eq!(r.try_recv(), None);
}

#[test]
fn preflight3() {
    let (s, r) = channel::unbounded();
    drop(s.clone());
    s.send(());
    drop(s);
    r.recv().unwrap();

    Select::new()
        .recv(&r, |v| assert!(v.is_none()))
        .wait();
}

#[test]
fn duplicate_cases() {
    let (s, r) = channel::unbounded::<i32>();
    let hit = vec![Cell::new(false); 4];

    while hit.iter().map(|h| h.get()).any(|hit| !hit) {
        Select::new()
            .recv(&r, |_| hit[0].set(true))
            .recv(&r, |_| hit[1].set(true))
            .send(&s, || 0, || hit[2].set(true))
            .send(&s, || 0, || hit[3].set(true))
            .wait();
    }
}

#[test]
fn nesting() {
    let (s, r) = channel::unbounded::<i32>();

    Select::new()
        .send(&s, || 0, || {
            Select::new()
                .recv(&r, |v| {
                    assert_eq!(v, Some(0));
                    Select::new()
                        .send(&s, || 1, || {
                            Select::new()
                                .recv(&r, |v| assert_eq!(v, Some(1)))
                                .wait();
                        })
                        .wait();
                })
                .wait();
        })
        .wait();
}

#[test]
fn stress_recv() {
    const COUNT: usize = 10_000;

    let (s1, r1) = channel::unbounded();
    let (s2, r2) = channel::bounded(5);
    let (s3, r3) = channel::bounded(100);

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            for i in 0..COUNT {
                s1.send(i);
                r3.recv().unwrap();

                s2.send(i);
                r3.recv().unwrap();
            }
        });

        for i in 0..COUNT {
            for _ in 0..2 {
                Select::new()
                    .recv(&r1, |v| assert_eq!(v, Some(i)))
                    .recv(&r2, |v| assert_eq!(v, Some(i)))
                    .wait();

                s3.send(());
            }
        }
    });
}

#[test]
fn stress_send() {
    const COUNT: usize = 10_000;

    let (s1, r1) = channel::bounded(0);
    let (s2, r2) = channel::bounded(0);
    let (s3, r3) = channel::bounded(100);

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            for i in 0..COUNT {
                assert_eq!(r1.recv().unwrap(), i);
                assert_eq!(r2.recv().unwrap(), i);
                r3.recv().unwrap();
            }
        });

        for i in 0..COUNT {
            for _ in 0..2 {
                Select::new()
                    .send(&s1, || i, || ())
                    .send(&s2, || i, || ())
                    .wait();
            }
            s3.send(());
        }
    });
}

#[test]
fn stress_mixed() {
    const COUNT: usize = 10_000;

    let (s1, r1) = channel::bounded(0);
    let (s2, r2) = channel::bounded(0);
    let (s3, r3) = channel::bounded(100);

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            for i in 0..COUNT {
                s1.send(i);
                assert_eq!(r2.recv().unwrap(), i);
                r3.recv().unwrap();
            }
        });

        for i in 0..COUNT {
            for _ in 0..2 {
                Select::new()
                    .recv(&r1, |v| assert_eq!(v, Some(i)))
                    .send(&s2, || i, || ())
                    .wait();
            }
            s3.send(());
        }
    });
}

#[test]
fn stress_timeout_two_threads() {
    const COUNT: usize = 20;

    let (s, r) = channel::bounded(2);

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            for i in 0..COUNT {
                if i % 2 == 0 {
                    thread::sleep(ms(500));
                }

                let mut done = false;
                while !done {
                    let after = channel::after(ms(100));
                    Select::new()
                        .send(&s, || i, || done = true)
                        .recv(&after, |_| ())
                        .wait();
                }
            }
        });

        scope.spawn(|| {
            for i in 0..COUNT {
                if i % 2 == 0 {
                    thread::sleep(ms(500));
                }

                let mut done = false;
                while !done {
                    let after = channel::after(ms(100));
                    Select::new()
                        .recv(&r, |v| {
                            assert_eq!(v, Some(i));
                            done = true;
                        })
                        .recv(&after, |_| ())
                        .wait();
                }
            }
        });
    });
}

#[test]
fn send_recv_same_channel() {
    let (s, r) = channel::bounded::<i32>(0);
    let after = channel::after(ms(500));
    Select::new()
        .send(&s, || 0, || panic!())
        .recv(&r, |_| panic!())
        .recv(&after, |_| ())
        .wait();

    let (s, r) = channel::unbounded::<i32>();
    let after = channel::after(ms(500));
    Select::new()
        .send(&s, || 0, || ())
        .recv(&r, |_| panic!())
        .recv(&after, |_| panic!())
        .wait();
}

#[test]
fn matching() {
    const THREADS: usize = 44;

    let (s, r) = &channel::bounded::<usize>(0);

    crossbeam::scope(|scope| {
        for i in 0..THREADS {
            scope.spawn(move || {
                Select::new()
                    .recv(&r, |v| assert_ne!(v.unwrap(), i))
                    .send(&s, || i, || ())
                    .wait();
            });
        }
    });

    assert_eq!(r.try_recv(), None);
}

#[test]
fn matching_with_leftover() {
    const THREADS: usize = 55;

    let (s, r) = &channel::bounded::<usize>(0);

    crossbeam::scope(|scope| {
        for i in 0..THREADS {
            scope.spawn(move || {
                Select::new()
                    .recv(&r, |v| assert_ne!(v.unwrap(), i))
                    .send(&s, || i, || ())
                    .wait();
            });
        }
        s.send(!0);
    });

    assert_eq!(r.try_recv(), None);
}

#[test]
fn channel_through_channel() {
    const COUNT: usize = 1000;

    type T = Box<Any + Send>;

    for cap in 0..3 {
        let (s, r) = channel::bounded::<T>(cap);

        crossbeam::scope(|scope| {
            scope.spawn(move || {
                let mut s = s;

                for _ in 0..COUNT {
                    let (new_s, new_r) = channel::bounded(cap);
                    let mut new_r: T = Box::new(Some(new_r));

                    Select::new()
                        .send(&s, || new_r, || ())
                        .wait();

                    s = new_s;
                }
            });

            scope.spawn(move || {
                let mut r = r;

                for _ in 0..COUNT {
                    let new = Select::new()
                        .recv(&r, |msg| {
                            msg.unwrap()
                                .downcast_mut::<Option<channel::Receiver<T>>>()
                                .unwrap()
                                .take()
                                .unwrap()
                        })
                        .wait();
                    r = new;
                }
            });
        });
    }
}

#[test]
fn linearizable() {
    const COUNT: usize = 100_000;

    for step in 0..2 {
        let (start_s, start_r) = channel::bounded::<()>(0);
        let (end_s, end_r) = channel::bounded::<()>(0);

        let ((s1, r1), (s2, r2)) = if step == 0 {
            (channel::bounded::<i32>(1), channel::bounded::<i32>(1))
        } else {
            (channel::unbounded::<i32>(), channel::unbounded::<i32>())
        };

        crossbeam::scope(|scope| {
            scope.spawn(|| {
                for _ in 0..COUNT {
                    start_s.send(());

                    s1.send(1);
                    Select::new()
                        .recv(&r1, |_| ())
                        .recv(&r2, |_| ())
                        .default(|| unreachable!())
                        .wait();

                    end_s.send(());
                    r2.try_recv();
                }
            });

            for _ in 0..COUNT {
                start_r.recv();

                s2.send(1);
                r1.try_recv();

                end_r.recv();
            }
        });
    }
}

#[test]
fn fairness1() {
    const COUNT: usize = 10_000;

    let (s1, r1) = channel::bounded::<()>(COUNT);
    let (s2, r2) = channel::unbounded::<()>();

    for _ in 0..COUNT {
        s1.send(());
        s2.send(());
    }

    let hits = vec![Cell::new(0usize); 4];
    while hits[0].get() + hits[1].get() < 2 * COUNT {
        let after = channel::after(ms(0));
        let tick = channel::tick(ms(0));
        Select::new()
            .recv(&r1, |_| hits[0].set(hits[0].get() + 1))
            .recv(&r2, |_| hits[1].set(hits[1].get() + 1))
            .recv(&after, |_| hits[2].set(hits[2].get() + 1))
            .recv(&tick, |_| hits[3].set(hits[3].get() + 1))
            .wait();
    }

    assert!(r1.is_empty());
    assert!(r2.is_empty());

    let sum: usize = hits.iter().map(|h| h.get()).sum();
    assert!(hits.iter().all(|x| x.get() >= sum / hits.len() / 2));
}

#[test]
fn fairness2() {
    const COUNT: usize = 10_000;

    let (s1, r1) = channel::unbounded::<()>();
    let (s2, r2) = channel::bounded::<()>(1);
    let (s3, r3) = channel::bounded::<()>(0);

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            for _ in 0..COUNT {
                let mut sel = Select::new();
                if s1.is_empty() {
                    sel = sel.send(&s1, || (), || ());
                }
                if s2.is_empty() {
                    sel = sel.send(&s2, || (), || ());
                }
                sel = sel.send(&s3, || (), || ());
                sel.wait();
            }
        });

        let hits = vec![Cell::new(0usize); 3];
        for _ in 0..COUNT {
            Select::new()
                .recv(&r1, |_| hits[0].set(hits[0].get() + 1))
                .recv(&r2, |_| hits[1].set(hits[1].get() + 1))
                .recv(&r3, |_| hits[2].set(hits[2].get() + 1))
                .wait();
        }
        assert!(hits.iter().all(|x| x.get() >= COUNT / hits.len() / 10));
    });
}

#[test]
fn drop_callbacks() {
    static DROPS: AtomicUsize = AtomicUsize::new(0);

    struct Foo;
    impl Drop for Foo {
        fn drop(&mut self) {
            DROPS.fetch_add(1, Ordering::SeqCst);
        }
    }

    let (s, r) = channel::unbounded::<()>();

    DROPS.store(0, Ordering::SeqCst);
    let foo1 = Foo;
    let foo2 = Foo;
    let foo3 = Foo;
    let foo4 = Foo;
    let sel = Select::new()
        .recv(&r, |_| drop(foo1))
        .send(&s, || drop(foo2), || drop(foo3))
        .default(|| drop(foo4));
    assert_eq!(DROPS.load(Ordering::SeqCst), 0);
    sel.wait();
    assert_eq!(DROPS.load(Ordering::SeqCst), 4);

    DROPS.store(0, Ordering::SeqCst);
    let foo1 = Foo;
    let foo2 = Foo;
    let foo3 = Foo;
    let mut sel = Select::new();
    sel = sel.default(|| drop(foo1));
    assert_eq!(DROPS.load(Ordering::SeqCst), 0);
    sel = sel.default(|| drop(foo2));
    assert_eq!(DROPS.load(Ordering::SeqCst), 1);
    sel = sel.default(|| drop(foo3));
    assert_eq!(DROPS.load(Ordering::SeqCst), 2);
    sel.wait();
    assert_eq!(DROPS.load(Ordering::SeqCst), 3);

    DROPS.store(0, Ordering::SeqCst);
    let foo1 = Foo;
    let foo2 = Foo;
    let foo3 = Foo;
    let foo4 = Foo;
    Select::new()
        .recv(&r, |_| drop(foo1))
        .send(&s, || drop(foo2), || drop(foo3))
        .default(|| {
            assert_eq!(DROPS.load(Ordering::SeqCst), 3);
            drop(foo4);
        })
        .wait();
    assert_eq!(DROPS.load(Ordering::SeqCst), 4);

    s.send(());

    DROPS.store(0, Ordering::SeqCst);
    let foo1 = Foo;
    let foo2 = Foo;
    let foo3 = Foo;
    let foo4 = Foo;
    Select::new()
        .recv(&r, |_| {
            assert_eq!(DROPS.load(Ordering::SeqCst), 3);
            drop(foo1);
        })
        .send(&s, || drop(foo2), || drop(foo3))
        .default(|| drop(foo4))
        .wait();
    assert_eq!(DROPS.load(Ordering::SeqCst), 4);
}
