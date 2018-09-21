use std::env;
use std::process::Command;
use std::path::{Path, PathBuf};

fn main() {
    assert_eq!(env::args_os().len(), 2);
    let test = PathBuf::from(env::args_os().nth(1).unwrap());
    let dst = Path::new("/data/local/tmp").join(test.file_name().unwrap());

    let status = Command::new("adb")
        .arg("wait-for-device")
        .status()
        .expect("failed to run: adb wait-for-device");
    assert!(status.success());

    let status = Command::new("adb")
        .arg("push")
        .arg(&test)
        .arg(&dst)
        .status()
        .expect("failed to run: adb pushr");
    assert!(status.success());

    let output = Command::new("adb")
        .arg("shell")
        .arg(&dst)
        .output()
        .expect("failed to run: adb shell");
    assert!(status.success());

    println!("status: {}\nstdout ---\n{}\nstderr ---\n{}",
             output.status,
             String::from_utf8_lossy(&output.stdout),
             String::from_utf8_lossy(&output.stderr));

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut lines = stdout.lines().filter(|l| l.starts_with("test result"));
    if !lines.all(|l| l.contains("test result: ok") && l.contains("0 failed")) {
        panic!("failed to find successful test run");
    }
}
