# configure.sh --

set -ex

prefix=/usr/local
if test -d /lib64
then libdir=${prefix}/lib64
else libdir=${prefix}/lib
fi

../configure \
    --enable-maintainer-mode                    \
    --config-cache                              \
    --cache-file=../config.cache                \
    --prefix="$prefix"                          \
    --libdir="$libdir"                          \
    --enable-time-tests                         \
    VFLAGS=-O0					\
    "$@"

### end of file
