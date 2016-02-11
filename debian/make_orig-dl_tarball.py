#!/usr/bin/python

import os
import subprocess
import sys
import time

deb_dir = os.path.dirname(os.path.realpath(sys.argv[0]))
src_root_dir = os.path.realpath(os.path.join(deb_dir, '..'))
src_root_parent_dir = os.path.realpath(os.path.join(src_root_dir, '..'))
snapshot_dir = os.path.join(src_root_dir, 'src', 'etc')

os.environ['CFG_SRC_DIR'] = src_root_dir

os.chdir(src_root_dir)
ps = subprocess.Popen(['dpkg-parsechangelog','-SVersion'], stdout=subprocess.PIPE)
version = ps.stdout.read()
assert version is not None
upstream_version = version.split('-')[0]

sys.path.append(snapshot_dir)
from snapshot import determine_curr_snapshot

snapshots = {}
for arch in ('i386', 'x86_64'):
    snapshots[arch] = determine_curr_snapshot(arch + '-unknown-linux')

def create_dl_tarball():
    dl_tarfile = 'rustc_' + upstream_version  + '.orig-dl.tar.gz'
    url_base = 'https://static.rust-lang.org/stage0-snapshots/'
    out_paths = []
    for arch, snapshot in snapshots.iteritems():
        url = url_base + snapshot
        out_path = os.path.join(src_root_parent_dir, snapshot)
        subprocess.check_call(["wget", "-N", url], cwd=src_root_parent_dir)
        out_paths.append(out_path)
    print "Building %s" % dl_tarfile
    # extra flags for reproducibility
    subprocess.check_call([
        "tar", "--mtime=@%s" % int(time.time()), "--clamp-mtime",
        "-czf", dl_tarfile
    ] + snapshots.values(), cwd=src_root_parent_dir, env={"GZIP":"-n"})

create_dl_tarball()
