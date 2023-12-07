#!/bin/sh
./unix_common.sh linux gtk2 aarch64 bin/linux-aarch64 ~/cudatext_build
./unix_common.sh linux qt5 aarch64 bin/linux-aarch64-qt5 ~/cudatext_build

./unix_common.sh linux gtk2 arm bin/linux-arm-gtk2 ~/cudatext_build
./unix_common.sh linux qt5 arm bin/linux-arm-qt5 ~/cudatext_build