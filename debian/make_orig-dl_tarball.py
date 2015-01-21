#!/usr/bin/python

import os
import re
import StringIO
import subprocess
import sys
import tarfile
import urllib3

deb_dir = os.path.dirname(os.path.realpath(sys.argv[0]))
src_root_dir = os.path.realpath(os.path.join(deb_dir, '..'))
src_root_parent_dir = os.path.realpath(os.path.join(src_root_dir, '..'))
snapshot_dir = os.path.join(src_root_dir, 'src', 'etc')

os.environ['CFG_SRC_DIR'] = src_root_dir

os.chdir(src_root_dir)
ps = subprocess.Popen('dpkg-parsechangelog', stdout=subprocess.PIPE)
version = ps.stdout.read()
regex = re.compile('^Version:\s*(\S+)\s*$', re.MULTILINE)
mo = regex.search(version)
assert mo is not None
version = mo.group(1)
upstream_version = version.split('-')[0]

sys.path.append(snapshot_dir)
from snapshot import determine_curr_snapshot

snapshots = {}
for arch in ('i386', 'x86_64'):
    snapshots[arch] = determine_curr_snapshot(arch + '-unknown-linux')

http = urllib3.PoolManager()

def create_dl_tarball():
    dl_tarfile = 'rust_' + upstream_version  + '.orig-dl.tar.gz'
    dl_tarfile = os.path.join(src_root_parent_dir, dl_tarfile)
    tar = tarfile.open(dl_tarfile, 'w:gz')
    url_base = 'https://static.rust-lang.org/stage0-snapshots/'
    for arch in snapshots.iterkeys():
        snapshot = snapshots[arch]
        url = url_base + snapshot
        print 'Downloading', snapshot, '...',
        sys.stdout.flush()
        r = http.request('GET', url)
        print
        assert(r.status == 200)
        filelike = StringIO.StringIO(r.data)
        tarinfo = tarfile.TarInfo(snapshot)
        tarinfo.size = len(filelike.buf)
        print 'Writing to', dl_tarfile, '...',
        tar.addfile(tarinfo, filelike)
        filelike.close()
        print
    tar.close()

create_dl_tarball()
