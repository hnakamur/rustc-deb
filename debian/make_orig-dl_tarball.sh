#!/bin/sh
# See README.Debian "Bootstrapping a new distro" for details.
#
# You should probably use `debian/rules source_orig-dl` instead of calling this
# directly.

set -e

upstream_version="$(dpkg-parsechangelog -SVersion | sed -e 's/\(.*\)-.*/\1/g')"
supported_arch="x86_64 i686"

rm -f dl/*.sha256
for arch in $supported_arch; do
	triplet="${arch}-unknown-linux-gnu"
	python src/etc/get-stage0.py "$triplet"
	rm -rf "$triplet"
done

tar --mtime=@"$(date +%s)" --clamp-mtime \
  --owner=root --group=root \
  -cJf "../rustc_${upstream_version}.orig-dl.tar.xz" \
  --transform "s/^dl\///" \
  dl/*

rm -f src/bootstrap/bootstrap.pyc
