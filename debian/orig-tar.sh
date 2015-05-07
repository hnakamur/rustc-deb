#!/bin/sh -ev

VERSION=$2
DIR=rustc-$(echo $VERSION|sed -e "s|~beta|-beta|" -e "s|~alpha|-alpha|")
TAR=rustc_$VERSION.orig.tar.bz2

# clean up the upstream tarball
tar -zxvf $3
# remove file with incompatible license
rm $DIR/src/llvm/cmake/modules/LLVMParseArguments.cmake
tar jcvf ../$TAR $DIR
rm -rf $DIR $3

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

exit 0

