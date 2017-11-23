#!/bin/bash

mkdir ~/cudatext_build
cd ~/cudatext_build

curl -L https://sourceforge.net/projects/cudatext/files/release/Windows/Python_engine/python35_x64.zip/download >py_dll_x64.zip
curl -L https://sourceforge.net/projects/cudatext/files/release/Windows/Python_engine/python35_x32.zip/download >py_dll_x32.zip

unzip py_dll_x64.zip -d ./py_dll_x64
unzip py_dll_x32.zip -d ./py_dll_x32
