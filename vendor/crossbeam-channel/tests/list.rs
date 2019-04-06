//! Tests for the list channel flavor.

extern crate crossbeam;
#[macro_use]
extern crate crossbeam_channel as channel;
extern crate rand;

mod wrappers;

macro_rules! tests {
    ($channel:path) => {
        use std::sync::atomic::AtomicUsize;
        use std::sync::atomic::Ordering;
        use std::thread;
        use std::time::Duration;

        use $channel as channel;
        use crossbeam;
        use rand::{thread_rng, Rng};

        fn ms(ms: u64) -> Duration {
            Duration::from_millis(ms)
        }

        #[test]
        fn smoke() {
            let (s, r) = channel::unbounded();
            s.send(7);
            assert_eq!(r.try_recv(), Some(7));

            s.send(8);
            assert_eq!(r.recv(), Some(8));

            assert_eq!(r.try_recv(), None);
            select! {
                recv(r) => panic!(),
                recv(channel::after(ms(1000))) => {}
            }

            assert_eq!(s.capacity(), None);
            assert_eq!(r.capacity(), None);
        }

        #[test]
        fn capacity() {
            let (s, r) = channel::unbounded::<()>();
            assert_eq!(s.capacity(), None);
            assert_eq!(r.capacity(), None);
        }

        #[test]
        fn len_empty_full() {
            let (s, r) = channel::unbounded();

            assert_eq!(s.len(), 0);
            assert_eq!(s.is_empty(), true);
            assert_eq!(s.is_full(), false);
            assert_eq!(r.len(), 0);
            assert_eq!(r.is_empty(), true);
            assert_eq!(r.is_full(), false);

            s.send(());

            assert_eq!(s.len(), 1);
            assert_eq!(s.is_empty(), false);
            assert_eq!(s.is_full(), false);
            assert_eq!(r.len(), 1);
            assert_eq!(r.is_empty(), false);
            assert_eq!(r.is_full(), false);

            r.recv().unwrap();

            assert_eq!(s.len(), 0);
            assert_eq!(s.is_empty(), true);
            assert_eq!(s.is_full(), false);
            assert_eq!(r.len(), 0);
            assert_eq!(r.is_empty(), true);
            assert_eq!(r.is_full(), false);
        }

        #[test]
        fn recv() {
            let (s, r) = channel::unbounded();

            crossbeam::scope(|scope| {
                scope.spawn(move || {
                    assert_eq!(r.recv(), Some(7));
                    thread::sleep(ms(1000));
                    assert_eq!(r.recv(), Some(8));
                    thread::sleep(ms(1000));
                    assert_eq!(r.recv(), Some(9));
                    assert_eq!(r.recv(), None);
                });
                scope.spawn(move || {
                    thread::sleep(ms(1500));
                    s.send(7);
                    s.send(8);
                    s.send(9);
                });
            });
        }

        #[test]
        fn recv_timeout() {
            let (s, r) = channel::unbounded::<i32>();

            crossbeam::scope(|scope| {
                scope.spawn(move || {
                    select! {
                        recv(r) => panic!(),
                        recv(channel::after(ms(1000))) => {}
                    }
                    select! {
                        recv(r, v) => assert_eq!(v, Some(7)),
                        recv(channel::after(ms(1000))) => panic!(),
                    }
                    select! {
                        recv(r, v) => assert_eq!(v, None),
                        recv(channel::after(ms(1000))) => panic!(),
                    }
                });
                scope.spawn(move || {
                    thread::sleep(ms(1500));
                    s.send(7);
                });
            });
        }

        #[test]
        fn try_recv() {
            let (s, r) = channel::unbounded();

            crossbeam::scope(|scope| {
                scope.spawn(move || {
                    assert_eq!(r.try_recv(), None);
                    thread::sleep(ms(1500));
                    assert_eq!(r.try_recv(), Some(7));
                    thread::sleep(ms(500));
                    assert_eq!(r.try_recv(), None);
                });
                scope.spawn(move || {
                    thread::sleep(ms(1000));
                    s.send(7);
                });
            });
        }

        #[test]
        fn recv_after_close() {
            let (s, r) = channel::unbounded();

            s.send(1);
            s.send(2);
            s.send(3);

            drop(s);

            assert_eq!(r.recv(), Some(1));
            assert_eq!(r.recv(), Some(2));
            assert_eq!(r.recv(), Some(3));
            assert_eq!(r.recv(), None);
        }

        #[test]
        fn len() {
            let (s, r) = channel::unbounded();

            assert_eq!(s.len(), 0);
            assert_eq!(r.len(), 0);

            for i in 0..50 {
                s.send(i);
                assert_eq!(s.len(), i + 1);
            }

            for i in 0..50 {
                r.recv().unwrap();
                assert_eq!(r.len(), 50 - i - 1);
            }

            assert_eq!(s.len(), 0);
            assert_eq!(r.len(), 0);
        }

        #[test]
        fn close_wakes_receiver() {
            let (s, r) = channel::unbounded::<()>();

            crossbeam::scope(|scope| {
                scope.spawn(move || {
                    assert_eq!(r.recv(), None);
                });
                scope.spawn(move || {
                    thread::sleep(ms(1000));
                    drop(s);
                });
            });
        }

        #[test]
        fn spsc() {
            const COUNT: usize = 100_000;

            let (s, r) = channel::unbounded();

            crossbeam::scope(|scope| {
                scope.spawn(move || {
                    for i in 0..COUNT {
                        assert_eq!(r.recv(), Some(i));
                    }
                    assert_eq!(r.recv(), None);
                });
                scope.spawn(move || {
                    for i in 0..COUNT {
                        s.send(i);
                    }
                });
            });
        }

        #[test]
        fn mpmc() {
            const COUNT: usize = 25_000;
            const THREADS: usize = 4;

            let (s, r) = channel::unbounded::<usize>();
            let v = (0..COUNT).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();

            crossbeam::scope(|scope| {
                for _ in 0..THREADS {
                    scope.spawn(|| {
                        for _ in 0..COUNT {
                            let n = r.recv().unwrap();
                            v[n].fetch_add(1, Ordering::SeqCst);
                        }
                    });
                }
                for _ in 0..THREADS {
                    scope.spawn(|| {
                        for i in 0..COUNT {
                            s.send(i);
                        }
                    });
                }
            });

            assert_eq!(r.try_recv(), None);

            for c in v {
                assert_eq!(c.load(Ordering::SeqCst), THREADS);
            }
        }

        #[test]
        fn stress_timeout_two_threads() {
            const COUNT: usize = 100;

            let (s, r) = channel::unbounded();

            crossbeam::scope(|scope| {
                scope.spawn(|| {
                    for i in 0..COUNT {
                        if i % 2 == 0 {
                            thread::sleep(ms(50));
                        }
                        s.send(i);
                    }
                });

                scope.spawn(|| {
                    for i in 0..COUNT {
                        if i % 2 == 0 {
                            thread::sleep(ms(50));
                        }
                        loop {
                            select! {
                                recv(r, v) => {
                                    assert_eq!(v, Some(i));
                                    break;
                                }
                                recv(channel::after(ms(10))) => {}
                            }
                        }
                    }
                });
            });
        }

        #[test]
        fn drops() {
            static DROPS: AtomicUsize = AtomicUsize::new(0);

            #[derive(Debug, PartialEq)]
            struct DropCounter;

            impl Drop for DropCounter {
                fn drop(&mut self) {
                    DROPS.fetch_add(1, Ordering::SeqCst);
                }
            }

            let mut rng = thread_rng();

            for _ in 0..100 {
                let steps = rng.gen_range(0, 10_000);
                let additional = rng.gen_range(0, 1000);

                DROPS.store(0, Ordering::SeqCst);
                let (s, r) = channel::unbounded::<DropCounter>();

                crossbeam::scope(|scope| {
                    scope.spawn(|| {
                        for _ in 0..steps {
                            r.recv().unwrap();
                        }
                    });

                    scope.spawn(|| {
                        for _ in 0..steps {
                            s.send(DropCounter);
                        }
                    });
                });

                for _ in 0..additional {
                    s.send(DropCounter);
                }

                assert_eq!(DROPS.load(Ordering::SeqCst), steps);
                drop(s);
                drop(r);
                assert_eq!(DROPS.load(Ordering::SeqCst), steps + additional);
            }
        }

        #[test]
        fn linearizable() {
            const COUNT: usize = 25_000;
            const THREADS: usize = 4;

            let (s, r) = channel::unbounded();

            crossbeam::scope(|scope| {
                for _ in 0..THREADS {
                    scope.spawn(|| {
                        for _ in 0..COUNT {
                            s.send(0);
                            r.try_recv().unwrap();
                        }
                    });
                }
            });
        }

        #[test]
        fn fairness() {
            const COUNT: usize = 10_000;

            let (s1, r1) = channel::unbounded::<()>();
            let (s2, r2) = channel::unbounded::<()>();

            for _ in 0..COUNT {
                s1.send(());
                s2.send(());
            }

            let mut hits = [0usize; 2];
            for _ in 0..COUNT {
                select! {
                    recv(r1) => hits[0] += 1,
                    recv(r2) => hits[1] += 1,
                }
            }
            assert!(hits.iter().all(|x| *x >= COUNT / hits.len() / 2));
        }

        #[test]
        fn fairness_duplicates() {
            const COUNT: usize = 10_000;

            let (s, r) = channel::unbounded();

            for _ in 0..COUNT {
                s.send(());
            }

            let mut hits = [0usize; 5];
            for _ in 0..COUNT {
                select! {
                    recv(r) => hits[0] += 1,
                    recv(r) => hits[1] += 1,
                    recv(r) => hits[2] += 1,
                    recv(r) => hits[3] += 1,
                    recv(r) => hits[4] += 1,
                }
            }
            assert!(hits.iter().all(|x| *x >= COUNT / hits.len() / 2));
        }

        #[test]
        fn recv_in_send() {
            let (s, r) = channel::unbounded();
            s.send(());

            select! {
                send(s, assert_eq!(r.recv(), Some(()))) => {}
            }
        }
    };
}

mod normal {
    tests!(wrappers::normal);
}

mod cloned {
    tests!(wrappers::cloned);
}

mod select {
    tests!(wrappers::select);
}

mod select_spin {
    tests!(wrappers::select_spin);
}

mod select_multi {
    tests!(wrappers::select_multi);
}
