#!/bin/sh
./unix_common.sh freebsd gtk2 amd64 builds/freebsd-x64-gtk2 ~/cudatext_build
./unix_common.sh freebsd gtk2 i386 builds/freebsd-x32-gtk2 ~/cudatext_build

./unix_common.sh netbsd gtk2 amd64 builds/netbsd-x64-gtk2 ~/cudatext_build
./unix_common.sh openbsd gtk2 amd64 builds/openbsd-x64-gtk2 ~/cudatext_build
./unix_common.sh dragonflybsd gtk2 amd64 builds/dragonflybsd-x64-gtk2 ~/cudatext_build
