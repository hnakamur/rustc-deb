#!/bin/sh
# For versions 1.10 and later.
# ONLY FOR BOOTSTRAPPING with DEB_BUILD_PROFILES=dlstage0
#
# Will create rustc_1.10.0+dfsg1.orig-dl.tar.xz; you should include this
# with your source upload.

set -e

upstream_version="$(dpkg-parsechangelog -SVersion | sed -e 's/\(.*\)-.*/\1/g')"
supported_arch="x86_64 i686"

rm -rf dl/*.sha256
for arch in $supported_arch; do
	triplet="${arch}-unknown-linux-gnu"
	python src/etc/get-stage0.py "$triplet"
done

tar --mtime=@"$(date +%s)" --clamp-mtime \
  -cJf "../rustc_${upstream_version}.orig-dl.tar.xz" \
  --transform "s/^dl\///" \
  dl/*
