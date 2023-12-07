#!/bin/sh
./unix_common.sh linux_musl gtk2 amd64 bin/linux_musl-x64-gtk2 ~/cudatext_build
xz -d -k ~/cudatext_build/cudatext-linux_musl-*.xz