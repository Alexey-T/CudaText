#!/bin/sh
./unix_common.sh linux gtk2 amd64 builds/linux-x64-gtk2 ~/cudatext_build
./unix_common.sh linux qt5 amd64 builds/linux-x64-qt5 ~/cudatext_build

./unix_common.sh linux gtk2 aarch64 builds/linux-aarch64 ~/cudatext_build
./unix_common.sh linux qt5 aarch64 builds/linux-aarch64-qt5 ~/cudatext_build

./unix_common.sh linux gtk2 arm builds/linux-arm-gtk2 ~/cudatext_build
./unix_common.sh linux qt5 arm builds/linux-arm-qt5 ~/cudatext_build

#./unix_common.sh linux gtk2 i386 builds/linux-x32-gtk2 ~/cudatext_build
#./unix_common.sh linux qt5 i386 builds/linux-x32-qt5 ~/cudatext_build

./unix_common_deb.sh gtk2_amd64 x64-gtk2
./unix_common_deb.sh qt5_amd64 x64-qt5
