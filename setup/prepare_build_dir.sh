#!/bin/bash
CUDDIR=~/cudatext_build
mkdir $CUDDIR
cd $CUDDIR

curl -L https://sourceforge.net/projects/cudatext/files/libs_for_release_making/cudatext-windows-libs.zip/download >winlibs.zip
unzip winlibs.zip -d .
