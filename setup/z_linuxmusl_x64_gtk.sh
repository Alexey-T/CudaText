#!/bin/sh
./unix_common.sh linux_musl gtk2 i386 builds/linux_musl-x64-gtk2 ~/cudatext_build
xz -d -k ~/cudatext_build/cudatext-linux_musl-gtk2-i386-1.88.3.1.tar.xz
