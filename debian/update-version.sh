#!/bin/bash
ORIG=1.8
NEW=1.9

sed -i -e "s|libstd-rust-$ORIG|libstd-rust-$NEW|g" control

git mv libstd-rust-$ORIG.lintian-overrides libstd-rust-$NEW.lintian-overrides
sed -i -e "s|libstd-rust-$ORIG|libstd-rust-$NEW|g" libstd-rust-$NEW.lintian-overrides

