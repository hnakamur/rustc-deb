use dox::mem;

pub type rlim_t = c_ulong;
pub type sa_family_t = u16;
pub type pthread_key_t = ::c_uint;
pub type speed_t = ::c_uint;
pub type tcflag_t = ::c_uint;

pub enum timezone {}

s! {
    pub struct sockaddr {
        pub sa_family: sa_family_t,
        pub sa_data: [::c_char; 14],
    }

    pub struct sockaddr_in {
        pub sin_family: sa_family_t,
        pub sin_port: ::in_port_t,
        pub sin_addr: ::in_addr,
        pub sin_zero: [u8; 8],
    }

    pub struct sockaddr_in6 {
        pub sin6_family: sa_family_t,
        pub sin6_port: ::in_port_t,
        pub sin6_flowinfo: u32,
        pub sin6_addr: ::in6_addr,
        pub sin6_scope_id: u32,
    }

    pub struct sockaddr_un {
        pub sun_family: sa_family_t,
        pub sun_path: [::c_char; 108]
    }

    pub struct sockaddr_storage {
        pub ss_family: sa_family_t,
        __ss_align: ::size_t,
        #[cfg(target_pointer_width = "32")]
        __ss_pad2: [u8; 128 - 2 * 4],
        #[cfg(target_pointer_width = "64")]
        __ss_pad2: [u8; 128 - 2 * 8],
    }

    pub struct addrinfo {
        pub ai_flags: ::c_int,
        pub ai_family: ::c_int,
        pub ai_socktype: ::c_int,
        pub ai_protocol: ::c_int,
        pub ai_addrlen: socklen_t,

        #[cfg(any(target_os = "linux", target_os = "emscripten"))]
        pub ai_addr: *mut ::sockaddr,

        pub ai_canonname: *mut c_char,

        #[cfg(target_os = "android")]
        pub ai_addr: *mut ::sockaddr,

        pub ai_next: *mut addrinfo,
    }

    pub struct sockaddr_ll {
        pub sll_family: ::c_ushort,
        pub sll_protocol: ::c_ushort,
        pub sll_ifindex: ::c_int,
        pub sll_hatype: ::c_ushort,
        pub sll_pkttype: ::c_uchar,
        pub sll_halen: ::c_uchar,
        pub sll_addr: [::c_uchar; 8]
    }

    pub struct fd_set {
        fds_bits: [::c_ulong; FD_SETSIZE / ULONG_SIZE],
    }

    pub struct tm {
        pub tm_sec: ::c_int,
        pub tm_min: ::c_int,
        pub tm_hour: ::c_int,
        pub tm_mday: ::c_int,
        pub tm_mon: ::c_int,
        pub tm_year: ::c_int,
        pub tm_wday: ::c_int,
        pub tm_yday: ::c_int,
        pub tm_isdst: ::c_int,
        pub tm_gmtoff: ::c_long,
        pub tm_zone: *const ::c_char,
    }

    pub struct sched_param {
        pub sched_priority: ::c_int,
        #[cfg(target_env = "musl")]
        pub sched_ss_low_priority: ::c_int,
        #[cfg(target_env = "musl")]
        pub sched_ss_repl_period: ::timespec,
        #[cfg(target_env = "musl")]
        pub sched_ss_init_budget: ::timespec,
        #[cfg(target_env = "musl")]
        pub sched_ss_max_repl: ::c_int,
    }

    pub struct Dl_info {
        pub dli_fname: *const ::c_char,
        pub dli_fbase: *mut ::c_void,
        pub dli_sname: *const ::c_char,
        pub dli_saddr: *mut ::c_void,
    }

    #[cfg_attr(any(target_arch = "x86", target_arch = "x86_64"),
               repr(packed))]
    pub struct epoll_event {
        pub events: ::uint32_t,
        pub u64: ::uint64_t,
    }

    pub struct utsname {
        pub sysname: [::c_char; 65],
        pub nodename: [::c_char; 65],
        pub release: [::c_char; 65],
        pub version: [::c_char; 65],
        pub machine: [::c_char; 65],
        pub domainname: [::c_char; 65]
    }
}

// intentionally not public, only used for fd_set
#[cfg(target_pointer_width = "32")]
const ULONG_SIZE: usize = 32;
#[cfg(target_pointer_width = "64")]
const ULONG_SIZE: usize = 64;

pub const EXIT_FAILURE: ::c_int = 1;
pub const EXIT_SUCCESS: ::c_int = 0;
pub const RAND_MAX: ::c_int = 2147483647;
pub const EOF: ::c_int = -1;
pub const SEEK_SET: ::c_int = 0;
pub const SEEK_CUR: ::c_int = 1;
pub const SEEK_END: ::c_int = 2;
pub const _IOFBF: ::c_int = 0;
pub const _IONBF: ::c_int = 2;
pub const _IOLBF: ::c_int = 1;

pub const F_DUPFD: ::c_int = 0;
pub const F_GETFD: ::c_int = 1;
pub const F_SETFD: ::c_int = 2;
pub const F_GETFL: ::c_int = 3;
pub const F_SETFL: ::c_int = 4;

pub const SIGTRAP: ::c_int = 5;

pub const PTHREAD_CREATE_JOINABLE: ::c_int = 0;
pub const PTHREAD_CREATE_DETACHED: ::c_int = 1;

pub const CLOCK_REALTIME: ::c_int = 0;
pub const CLOCK_MONOTONIC: ::c_int = 1;

pub const RLIMIT_CPU: ::c_int = 0;
pub const RLIMIT_FSIZE: ::c_int = 1;
pub const RLIMIT_DATA: ::c_int = 2;
pub const RLIMIT_STACK: ::c_int = 3;
pub const RLIMIT_CORE: ::c_int = 4;
pub const RLIMIT_LOCKS: ::c_int = 10;
pub const RLIMIT_SIGPENDING: ::c_int = 11;
pub const RLIMIT_MSGQUEUE: ::c_int = 12;
pub const RLIMIT_NICE: ::c_int = 13;
pub const RLIMIT_RTPRIO: ::c_int = 14;

pub const RUSAGE_SELF: ::c_int = 0;

pub const O_RDONLY: ::c_int = 0;
pub const O_WRONLY: ::c_int = 1;
pub const O_RDWR: ::c_int = 2;
pub const O_TRUNC: ::c_int = 512;
pub const O_CLOEXEC: ::c_int = 0x80000;
pub const S_IFIFO: ::mode_t = 4096;
pub const S_IFCHR: ::mode_t = 8192;
pub const S_IFBLK: ::mode_t = 24576;
pub const S_IFDIR: ::mode_t = 16384;
pub const S_IFREG: ::mode_t = 32768;
pub const S_IFLNK: ::mode_t = 40960;
pub const S_IFSOCK: ::mode_t = 49152;
pub const S_IFMT: ::mode_t = 61440;
pub const S_IRWXU: ::mode_t = 448;
pub const S_IXUSR: ::mode_t = 64;
pub const S_IWUSR: ::mode_t = 128;
pub const S_IRUSR: ::mode_t = 256;
pub const S_IRWXG: ::mode_t = 56;
pub const S_IXGRP: ::mode_t = 8;
pub const S_IWGRP: ::mode_t = 16;
pub const S_IRGRP: ::mode_t = 32;
pub const S_IRWXO: ::mode_t = 7;
pub const S_IXOTH: ::mode_t = 1;
pub const S_IWOTH: ::mode_t = 2;
pub const S_IROTH: ::mode_t = 4;
pub const F_OK: ::c_int = 0;
pub const R_OK: ::c_int = 4;
pub const W_OK: ::c_int = 2;
pub const X_OK: ::c_int = 1;
pub const STDIN_FILENO: ::c_int = 0;
pub const STDOUT_FILENO: ::c_int = 1;
pub const STDERR_FILENO: ::c_int = 2;
pub const SIGHUP: ::c_int = 1;
pub const SIGINT: ::c_int = 2;
pub const SIGQUIT: ::c_int = 3;
pub const SIGILL: ::c_int = 4;
pub const SIGABRT: ::c_int = 6;
pub const SIGFPE: ::c_int = 8;
pub const SIGKILL: ::c_int = 9;
pub const SIGSEGV: ::c_int = 11;
pub const SIGPIPE: ::c_int = 13;
pub const SIGALRM: ::c_int = 14;
pub const SIGTERM: ::c_int = 15;

pub const PROT_NONE: ::c_int = 0;
pub const PROT_READ: ::c_int = 1;
pub const PROT_WRITE: ::c_int = 2;
pub const PROT_EXEC: ::c_int = 4;

pub const MAP_FILE: ::c_int = 0x0000;
pub const MAP_SHARED: ::c_int = 0x0001;
pub const MAP_PRIVATE: ::c_int = 0x0002;
pub const MAP_FIXED: ::c_int = 0x0010;

pub const MAP_FAILED: *mut ::c_void = !0 as *mut ::c_void;

pub const MCL_CURRENT: ::c_int = 0x0001;
pub const MCL_FUTURE: ::c_int = 0x0002;

pub const MS_ASYNC: ::c_int = 0x0001;
pub const MS_INVALIDATE: ::c_int = 0x0002;
pub const MS_SYNC: ::c_int = 0x0004;
pub const MS_RDONLY: ::c_ulong = 0x01;
pub const MS_NOSUID: ::c_ulong = 0x02;
pub const MS_NODEV: ::c_ulong = 0x04;
pub const MS_NOEXEC: ::c_ulong = 0x08;
pub const MS_SYNCHRONOUS: ::c_ulong = 0x10;
pub const MS_REMOUNT: ::c_ulong = 0x20;
pub const MS_MANDLOCK: ::c_ulong = 0x40;
pub const MS_DIRSYNC: ::c_ulong = 0x80;
pub const MS_NOATIME: ::c_ulong = 0x0400;
pub const MS_NODIRATIME: ::c_ulong = 0x0800;
pub const MS_BIND: ::c_ulong = 0x1000;
pub const MS_MOVE: ::c_ulong = 0x2000;
pub const MS_REC: ::c_ulong = 0x4000;
pub const MS_SILENT: ::c_ulong = 0x8000;
pub const MS_POSIXACL: ::c_ulong = 0x010000;
pub const MS_UNBINDABLE: ::c_ulong = 0x020000;
pub const MS_PRIVATE: ::c_ulong = 0x040000;
pub const MS_SLAVE: ::c_ulong = 0x080000;
pub const MS_SHARED: ::c_ulong = 0x100000;
pub const MS_ACTIVE: ::c_ulong = 0x40000000;
pub const MS_NOUSER: ::c_ulong = 0x80000000;
pub const MS_MGC_VAL: ::c_ulong = 0xc0ed0000;
pub const MS_MGC_MSK: ::c_ulong = 0xffff0000;
pub const MS_RMT_MASK: ::c_ulong = 0x800051;

pub const EPERM: ::c_int = 1;
pub const ENOENT: ::c_int = 2;
pub const ESRCH: ::c_int = 3;
pub const EINTR: ::c_int = 4;
pub const EIO: ::c_int = 5;
pub const ENXIO: ::c_int = 6;
pub const E2BIG: ::c_int = 7;
pub const ENOEXEC: ::c_int = 8;
pub const EBADF: ::c_int = 9;
pub const ECHILD: ::c_int = 10;
pub const EAGAIN: ::c_int = 11;
pub const ENOMEM: ::c_int = 12;
pub const EACCES: ::c_int = 13;
pub const EFAULT: ::c_int = 14;
pub const ENOTBLK: ::c_int = 15;
pub const EBUSY: ::c_int = 16;
pub const EEXIST: ::c_int = 17;
pub const EXDEV: ::c_int = 18;
pub const ENODEV: ::c_int = 19;
pub const ENOTDIR: ::c_int = 20;
pub const EISDIR: ::c_int = 21;
pub const EINVAL: ::c_int = 22;
pub const ENFILE: ::c_int = 23;
pub const EMFILE: ::c_int = 24;
pub const ENOTTY: ::c_int = 25;
pub const ETXTBSY: ::c_int = 26;
pub const EFBIG: ::c_int = 27;
pub const ENOSPC: ::c_int = 28;
pub const ESPIPE: ::c_int = 29;
pub const EROFS: ::c_int = 30;
pub const EMLINK: ::c_int = 31;
pub const EPIPE: ::c_int = 32;
pub const EDOM: ::c_int = 33;
pub const ERANGE: ::c_int = 34;
pub const EWOULDBLOCK: ::c_int = EAGAIN;

pub const EBFONT: ::c_int = 59;
pub const ENOSTR: ::c_int = 60;
pub const ENODATA: ::c_int = 61;
pub const ETIME: ::c_int = 62;
pub const ENOSR: ::c_int = 63;
pub const ENONET: ::c_int = 64;
pub const ENOPKG: ::c_int = 65;
pub const EREMOTE: ::c_int = 66;
pub const ENOLINK: ::c_int = 67;
pub const EADV: ::c_int = 68;
pub const ESRMNT: ::c_int = 69;
pub const ECOMM: ::c_int = 70;
pub const EPROTO: ::c_int = 71;
pub const EDOTDOT: ::c_int = 73;

pub const AF_PACKET: ::c_int = 17;
pub const IPPROTO_RAW: ::c_int = 255;

pub const PROT_GROWSDOWN: ::c_int = 0x1000000;
pub const PROT_GROWSUP: ::c_int = 0x2000000;

pub const MAP_TYPE: ::c_int = 0x000f;

pub const MADV_NORMAL: ::c_int = 0;
pub const MADV_RANDOM: ::c_int = 1;
pub const MADV_SEQUENTIAL: ::c_int = 2;
pub const MADV_WILLNEED: ::c_int = 3;
pub const MADV_DONTNEED: ::c_int = 4;
pub const MADV_REMOVE: ::c_int = 9;
pub const MADV_DONTFORK: ::c_int = 10;
pub const MADV_DOFORK: ::c_int = 11;
pub const MADV_MERGEABLE: ::c_int = 12;
pub const MADV_UNMERGEABLE: ::c_int = 13;
pub const MADV_HWPOISON: ::c_int = 100;

pub const IFF_LOOPBACK: ::c_int = 0x8;

pub const AF_UNIX: ::c_int = 1;
pub const AF_INET: ::c_int = 2;
pub const AF_INET6: ::c_int = 10;
pub const SOCK_RAW: ::c_int = 3;
pub const IPPROTO_TCP: ::c_int = 6;
pub const IPPROTO_IP: ::c_int = 0;
pub const IPPROTO_IPV6: ::c_int = 41;
pub const IP_MULTICAST_TTL: ::c_int = 33;
pub const IP_MULTICAST_LOOP: ::c_int = 34;
pub const IP_TTL: ::c_int = 2;
pub const IP_HDRINCL: ::c_int = 3;
pub const IP_ADD_MEMBERSHIP: ::c_int = 35;
pub const IP_DROP_MEMBERSHIP: ::c_int = 36;
pub const IPV6_ADD_MEMBERSHIP: ::c_int = 20;
pub const IPV6_DROP_MEMBERSHIP: ::c_int = 21;

pub const TCP_NODELAY: ::c_int = 1;
pub const TCP_MAXSEG: ::c_int = 2;
pub const TCP_CORK: ::c_int = 3;
pub const TCP_KEEPIDLE: ::c_int = 4;
pub const TCP_KEEPINTVL: ::c_int = 5;
pub const TCP_KEEPCNT: ::c_int = 6;
pub const TCP_SYNCNT: ::c_int = 7;
pub const TCP_LINGER2: ::c_int = 8;
pub const TCP_DEFER_ACCEPT: ::c_int = 9;
pub const TCP_WINDOW_CLAMP: ::c_int = 10;
pub const TCP_INFO: ::c_int = 11;
pub const TCP_QUICKACK: ::c_int = 12;
pub const TCP_CONGESTION: ::c_int = 13;

pub const IPV6_MULTICAST_LOOP: ::c_int = 19;
pub const IPV6_V6ONLY: ::c_int = 26;

pub const SO_DEBUG: ::c_int = 1;

pub const SHUT_RD: ::c_int = 0;
pub const SHUT_WR: ::c_int = 1;
pub const SHUT_RDWR: ::c_int = 2;

pub const LOCK_SH: ::c_int = 1;
pub const LOCK_EX: ::c_int = 2;
pub const LOCK_NB: ::c_int = 4;
pub const LOCK_UN: ::c_int = 8;

pub const SIGSTKSZ: ::size_t = 8192;

pub const SA_NODEFER: ::c_int = 0x40000000;
pub const SA_RESETHAND: ::c_int = 0x80000000;
pub const SA_RESTART: ::c_int = 0x10000000;
pub const SA_NOCLDSTOP: ::c_int = 0x00000001;

pub const PATH_MAX: ::c_int = 4096;

pub const FD_SETSIZE: usize = 1024;

pub const EPOLLIN: ::c_int = 0x1;
pub const EPOLLPRI: ::c_int = 0x2;
pub const EPOLLOUT: ::c_int = 0x4;
pub const EPOLLRDNORM: ::c_int = 0x40;
pub const EPOLLRDBAND: ::c_int = 0x80;
pub const EPOLLWRNORM: ::c_int = 0x100;
pub const EPOLLWRBAND: ::c_int = 0x200;
pub const EPOLLMSG: ::c_int = 0x400;
pub const EPOLLERR: ::c_int = 0x8;
pub const EPOLLHUP: ::c_int = 0x10;
pub const EPOLLET: ::c_int = 0x80000000;

pub const EPOLL_CTL_ADD: ::c_int = 1;
pub const EPOLL_CTL_MOD: ::c_int = 3;
pub const EPOLL_CTL_DEL: ::c_int = 2;

pub const MNT_DETACH: ::c_int = 0x2;
pub const MNT_EXPIRE: ::c_int = 0x4;

pub const Q_GETFMT: ::c_int = 0x800004;
pub const Q_GETINFO: ::c_int = 0x800005;
pub const Q_SETINFO: ::c_int = 0x800006;
pub const QIF_BLIMITS: ::uint32_t = 1;
pub const QIF_SPACE: ::uint32_t = 2;
pub const QIF_ILIMITS: ::uint32_t = 4;
pub const QIF_INODES: ::uint32_t = 8;
pub const QIF_BTIME: ::uint32_t = 16;
pub const QIF_ITIME: ::uint32_t = 32;
pub const QIF_LIMITS: ::uint32_t = 5;
pub const QIF_USAGE: ::uint32_t = 10;
pub const QIF_TIMES: ::uint32_t = 48;
pub const QIF_ALL: ::uint32_t = 63;

pub const CBAUD: ::tcflag_t = 0o0010017;

pub const EFD_CLOEXEC: ::c_int = 0x80000;

pub const F_SETLK: ::c_int = 6;
pub const F_SETLKW: ::c_int = 7;

pub const MNT_FORCE: ::c_int = 0x1;

pub const Q_SYNC: ::c_int = 0x800001;
pub const Q_QUOTAON: ::c_int = 0x800002;
pub const Q_QUOTAOFF: ::c_int = 0x800003;
pub const Q_GETQUOTA: ::c_int = 0x800007;
pub const Q_SETQUOTA: ::c_int = 0x800008;

pub const TCIOFF: ::c_int = 2;
pub const TCION: ::c_int = 3;
pub const TCOOFF: ::c_int = 0;
pub const TCOON: ::c_int = 1;
pub const TCIFLUSH: ::c_int = 0;
pub const TCOFLUSH: ::c_int = 1;
pub const TCIOFLUSH: ::c_int = 2;
pub const NL0: ::c_int  = 0x00000000;
pub const NL1: ::c_int  = 0x00000100;
pub const TAB0: ::c_int = 0x00000000;
pub const TAB1: ::c_int = 0x00000800;
pub const TAB2: ::c_int = 0x00001000;
pub const TAB3: ::c_int = 0x00001800;
pub const CR0: ::c_int  = 0x00000000;
pub const CR1: ::c_int  = 0x00000200;
pub const CR2: ::c_int  = 0x00000400;
pub const CR3: ::c_int  = 0x00000600;
pub const FF0: ::c_int  = 0x00000000;
pub const FF1: ::c_int  = 0x00008000;
pub const BS0: ::c_int  = 0x00000000;
pub const BS1: ::c_int  = 0x00002000;
pub const VT0: ::c_int  = 0x00000000;
pub const VT1: ::c_int  = 0x00004000;
pub const VERASE: usize = 2;
pub const VWERASE: usize = 14;
pub const VKILL: usize = 3;
pub const VREPRINT: usize = 12;
pub const VINTR: usize = 0;
pub const VQUIT: usize = 1;
pub const VSUSP: usize = 10;
pub const VSTART: usize = 8;
pub const VSTOP: usize = 9;
pub const VLNEXT: usize = 15;
pub const VDISCARD: usize = 13;
pub const VTIME: usize = 5;
pub const IGNBRK: ::tcflag_t = 0x00000001;
pub const BRKINT: ::tcflag_t = 0x00000002;
pub const IGNPAR: ::tcflag_t = 0x00000004;
pub const PARMRK: ::tcflag_t = 0x00000008;
pub const INPCK: ::tcflag_t = 0x00000010;
pub const ISTRIP: ::tcflag_t = 0x00000020;
pub const INLCR: ::tcflag_t = 0x00000040;
pub const IGNCR: ::tcflag_t = 0x00000080;
pub const ICRNL: ::tcflag_t = 0x00000100;
pub const IXON: ::tcflag_t = 0x00000400;
pub const IXOFF: ::tcflag_t = 0x00001000;
pub const IXANY: ::tcflag_t = 0x00000800;
pub const IMAXBEL: ::tcflag_t = 0x00002000;
pub const OPOST: ::tcflag_t = 0x1;
pub const ONLCR: ::tcflag_t = 0x4;
pub const CSIZE: ::tcflag_t = 0x00000030;
pub const CS5: ::tcflag_t = 0x00000000;
pub const CS6: ::tcflag_t = 0x00000010;
pub const CS7: ::tcflag_t = 0x00000020;
pub const CS8: ::tcflag_t = 0x00000030;
pub const CSTOPB: ::tcflag_t = 0x00000040;
pub const CREAD: ::tcflag_t = 0x00000080;
pub const PARENB: ::tcflag_t = 0x00000100;
pub const PARODD: ::tcflag_t = 0x00000200;
pub const HUPCL: ::tcflag_t = 0x00000400;
pub const CLOCAL: ::tcflag_t = 0x00000800;
pub const CRTSCTS: ::tcflag_t = 0x80000000;
pub const ECHOKE: ::tcflag_t = 0x00000800;
pub const ECHOE: ::tcflag_t = 0x00000010;
pub const ECHOK: ::tcflag_t = 0x00000020;
pub const ECHO: ::tcflag_t = 0x00000008;
pub const ECHONL: ::tcflag_t = 0x00000040;
pub const ECHOPRT: ::tcflag_t = 0x00000400;
pub const ECHOCTL: ::tcflag_t = 0x00000200;
pub const ISIG: ::tcflag_t = 0x00000001;
pub const ICANON: ::tcflag_t = 0x00000002;
pub const PENDIN: ::tcflag_t = 0x00004000;
pub const NOFLSH: ::tcflag_t = 0x00000080;

pub const CLONE_VM: ::c_int = 0x100;
pub const CLONE_FS: ::c_int = 0x200;
pub const CLONE_FILES: ::c_int = 0x400;
pub const CLONE_SIGHAND: ::c_int = 0x800;
pub const CLONE_PTRACE: ::c_int = 0x2000;
pub const CLONE_VFORK: ::c_int = 0x4000;
pub const CLONE_PARENT: ::c_int = 0x8000;
pub const CLONE_THREAD: ::c_int = 0x10000;
pub const CLONE_NEWNS: ::c_int = 0x20000;
pub const CLONE_SYSVSEM: ::c_int = 0x40000;
pub const CLONE_SETTLS: ::c_int = 0x80000;
pub const CLONE_PARENT_SETTID: ::c_int = 0x100000;
pub const CLONE_CHILD_CLEARTID: ::c_int = 0x200000;
pub const CLONE_DETACHED: ::c_int = 0x400000;
pub const CLONE_UNTRACED: ::c_int = 0x800000;
pub const CLONE_CHILD_SETTID: ::c_int = 0x01000000;

f! {
    pub fn FD_CLR(fd: ::c_int, set: *mut fd_set) -> () {
        let fd = fd as usize;
        let size = mem::size_of_val(&(*set).fds_bits[0]) * 8;
        (*set).fds_bits[fd / size] &= !(1 << (fd % size));
        return
    }

    pub fn FD_ISSET(fd: ::c_int, set: *mut fd_set) -> bool {
        let fd = fd as usize;
        let size = mem::size_of_val(&(*set).fds_bits[0]) * 8;
        return ((*set).fds_bits[fd / size] & (1 << (fd % size))) != 0
    }

    pub fn FD_SET(fd: ::c_int, set: *mut fd_set) -> () {
        let fd = fd as usize;
        let size = mem::size_of_val(&(*set).fds_bits[0]) * 8;
        (*set).fds_bits[fd / size] |= 1 << (fd % size);
        return
    }

    pub fn FD_ZERO(set: *mut fd_set) -> () {
        for slot in (*set).fds_bits.iter_mut() {
            *slot = 0;
        }
    }

    pub fn WIFEXITED(status: ::c_int) -> bool {
        (status & 0xff) == 0
    }

    pub fn WEXITSTATUS(status: ::c_int) -> ::c_int {
        (status >> 8) & 0xff
    }

    pub fn WTERMSIG(status: ::c_int) -> ::c_int {
        status & 0x7f
    }
}

extern {
    pub fn fdatasync(fd: ::c_int) -> ::c_int;
    pub fn mincore(addr: *mut ::c_void, len: ::size_t,
                   vec: *mut ::c_uchar) -> ::c_int;
    pub fn clock_gettime(clk_id: ::c_int, tp: *mut ::timespec) -> ::c_int;
    pub fn prctl(option: ::c_int, ...) -> ::c_int;
    pub fn pthread_getattr_np(native: ::pthread_t,
                              attr: *mut ::pthread_attr_t) -> ::c_int;
    pub fn pthread_attr_getguardsize(attr: *const ::pthread_attr_t,
                                     guardsize: *mut ::size_t) -> ::c_int;
    pub fn pthread_attr_getstack(attr: *const ::pthread_attr_t,
                                 stackaddr: *mut *mut ::c_void,
                                 stacksize: *mut ::size_t) -> ::c_int;
    pub fn memalign(align: ::size_t, size: ::size_t) -> *mut ::c_void;
    pub fn setgroups(ngroups: ::size_t,
                     ptr: *const ::gid_t) -> ::c_int;
    pub fn sched_setscheduler(pid: ::pid_t, policy: ::c_int, param: *const sched_param) -> ::c_int;
    pub fn sched_getscheduler(pid: ::pid_t) -> ::c_int;
    pub fn sched_get_priority_max(policy: ::c_int) -> ::c_int;
    pub fn sched_get_priority_min(policy: ::c_int) -> ::c_int;
    pub fn epoll_create(size: ::c_int) -> ::c_int;
    pub fn epoll_ctl(epfd: ::c_int,
                     op: ::c_int,
                     fd: ::c_int,
                     event: *mut epoll_event) -> ::c_int;
    pub fn epoll_wait(epfd: ::c_int,
                      events: *mut epoll_event,
                      maxevents: ::c_int,
                      timeout: ::c_int) -> ::c_int;
    pub fn pipe2(fds: *mut ::c_int, flags: ::c_int) -> ::c_int;
    pub fn mount(src: *const ::c_char,
                 target: *const ::c_char,
                 fstype: *const ::c_char,
                 flags: ::c_ulong,
                 data: *const ::c_void) -> ::c_int;
    pub fn umount(target: *const ::c_char) -> ::c_int;
    pub fn umount2(target: *const ::c_char, flags: ::c_int) -> ::c_int;
    pub fn clone(cb: extern fn(*mut ::c_void) -> ::c_int,
                 child_stack: *mut ::c_void,
                 flags: ::c_int,
                 arg: *mut ::c_void, ...) -> ::c_int;
    pub fn statfs(path: *const ::c_char, buf: *mut statfs) -> ::c_int;
    pub fn fstatfs(fd: ::c_int, buf: *mut statfs) -> ::c_int;
    pub fn memrchr(cx: *const ::c_void, c: ::c_int, n: ::size_t) -> *mut ::c_void;
    pub fn syscall(num: ::c_long, ...) -> ::c_long;
}

cfg_if! {
    if #[cfg(any(target_os = "linux",
                 target_os = "emscripten"))] {
        mod linux;
        pub use self::linux::*;
    } else if #[cfg(target_os = "android")] {
        mod android;
        pub use self::android::*;
    } else {
        // ...
    }
}
