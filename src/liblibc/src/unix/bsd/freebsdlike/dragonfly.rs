pub const PTHREAD_STACK_MIN: ::size_t = 1024;
pub const KERN_PROC_PATHNAME: ::c_int = 9;
pub const SIGSTKSZ: ::size_t = 8192 /* MINSIGSTKSZ */ + 32768;

extern {
    pub fn __dfly_error() -> *const ::c_int;
}
