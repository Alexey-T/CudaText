#!/bin/sh
./unix_common.sh freebsd gtk2 amd64 bin/freebsd-x64-gtk2 ~/cudatext_build
#./unix_common.sh freebsd gtk2 i386 bin/freebsd-x32-gtk2 ~/cudatext_build

./unix_common.sh netbsd gtk2 amd64 bin/netbsd-x64-gtk2 ~/cudatext_build
./unix_common.sh netbsd gtk2 i386 bin/netbsd-x32-gtk2 ~/cudatext_build

./unix_common.sh openbsd gtk2 amd64 bin/openbsd-x64-gtk2 ~/cudatext_build
./unix_common.sh openbsd gtk2 i386 bin/openbsd-x32-gtk2 ~/cudatext_build

./unix_common.sh dragonflybsd gtk2 amd64 bin/dragonflybsd-x64-gtk2 ~/cudatext_build