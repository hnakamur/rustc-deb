# Entry point for all travis builds, this will set up the Travis environment by
# downloading any dependencies. It will then execute the `run.sh` script to
# build and execute all tests.

set -ex

if [ "$TRAVIS_OS_NAME" = "linux" ]; then
  OS=unknown-linux-gnu
else
  OS=apple-darwin
fi

export HOST=$ARCH-$OS
if [ "$TARGET" = "" ]; then
  TARGET=$HOST
fi

MAIN_TARGETS=https://static.rust-lang.org/dist
DATE=$(echo $TRAVIS_RUST_VERSION | sed s/nightly-//)
EXTRA_TARGETS=https://people.mozilla.org/~acrichton/libc-test/$DATE

install() {
  if [ "$TRAVIS" = "true" ]; then
    sudo apt-get update
    sudo apt-get install -y $@
  fi
}

mkdir -p .cargo
cp ci/cargo-config .cargo/config

if [ "$TRAVIS" = "true" ]; then
  case "$TARGET" in
    *-apple-ios | *-rumprun-*)
      curl -s $EXTRA_TARGETS/$TARGET.tar.gz | \
       tar xzf - -C `rustc --print sysroot`/lib/rustlib
      ;;

    *)
      # Download the rustlib folder from the relevant portion of main
      # distribution's tarballs.
      dir=rust-std-$TARGET
      pkg=rust-std
      if [ "$TRAVIS_RUST_VERSION" = "1.0.0" ]; then
        pkg=rust
        dir=rustc
      fi
      curl -s $MAIN_TARGETS/$pkg-$TRAVIS_RUST_VERSION-$TARGET.tar.gz | \
        tar xzf - -C $HOME/rust/lib/rustlib --strip-components=4 \
          $pkg-$TRAVIS_RUST_VERSION-$TARGET/$dir/lib/rustlib/$TARGET
      ;;

  esac
fi

# Pull a pre-built docker image for testing android, then run tests entirely
# within that image. Note that this is using the same rustc installation that
# travis has (sharing it via `-v`) and otherwise the tests run entirely within
# the container.
if [ "$DOCKER" != "" ]; then
  args=""

  case "$TARGET" in
    mips-unknown-linux-gnu)
      args="$args -e CC=mips-linux-gnu-gcc-5"
      ;;

    *)
      ;;
  esac

  exec docker run \
    --entrypoint bash \
    -v `rustc --print sysroot`:/usr/local:ro \
    -v `pwd`:/checkout \
    -e LD_LIBRARY_PATH=/usr/local/lib \
    -e CARGO_TARGET_DIR=/tmp \
    $args \
    -w /checkout \
    -it $DOCKER \
    ci/run.sh $TARGET
fi

case "$TARGET" in
  x86_64-unknown-linux-musl)
    install musl-tools
    export CC=musl-gcc
    ;;

  arm-unknown-linux-gnueabihf)
    install gcc-4.7-arm-linux-gnueabihf qemu-user
    export CC=arm-linux-gnueabihf-gcc-4.7
    ;;

  aarch64-unknown-linux-gnu)
    install gcc-aarch64-linux-gnu qemu-user
    export CC=aarch64-linux-gnu-gcc
    ;;

  *-apple-ios)
    ;;

  *)
    # clang has better error messages and implements alignof more broadly
    export CC=clang

    if [ "$TARGET" = "i686-unknown-linux-gnu" ]; then
      install gcc-multilib
    fi
    ;;

esac

sh ci/run.sh $TARGET

if [ "$TARGET" = "x86_64-unknown-linux-gnu" ] && \
   [ "$TRAVIS_RUST_VERSION" = "nightly" ] && \
   [ "$TRAVIS_OS_NAME" = "linux" ]; then
  sh ci/dox.sh
fi
