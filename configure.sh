# configure.sh --

set -ex

prefix=/usr/local

../configure \
    --enable-maintainer-mode                    \
    --config-cache                              \
    --cache-file=../config.cache                \
    --prefix="$prefix"                          \
    --enable-time-tests                         \
    VFLAGS=-O0					\
    "$@"

### end of file
