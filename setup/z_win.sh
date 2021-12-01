#!/bin/sh
mkdir ~/cudatext_build
./win_common.sh x64 ~/cudatext_build ../app/builds/win64 ~/cudatext_build/lib_win_x64
./win_common.sh x32 ~/cudatext_build ../app/builds/win32 ~/cudatext_build/lib_win_x32
