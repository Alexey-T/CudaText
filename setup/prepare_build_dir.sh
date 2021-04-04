#!/bin/bash
CUDDIR=~/cudatext_build
mkdir $CUDDIR
cd $CUDDIR

curl -L https://sourceforge.net/projects/cudatext/files/release_libs/cudatext-windows-libs.zip/download >winlibs.zip
unzip winlibs.zip -d .

mkdir lib_qt
cd lib_qt
curl -L https://sourceforge.net/projects/cudatext/files/release_libs/libQt5Pas.zip/download >libQt5Pas.zip
unzip libQt5Pas.zip -d .
