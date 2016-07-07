#!/bin/bash
ORIG=1.9
NEW=1.10

sed -i -e "s|libstd-rust-$ORIG|libstd-rust-$NEW|g" control

git mv libstd-rust-$ORIG.lintian-overrides libstd-rust-$NEW.lintian-overrides
sed -i -e "s|libstd-rust-$ORIG|libstd-rust-$NEW|g" libstd-rust-$NEW.lintian-overrides

