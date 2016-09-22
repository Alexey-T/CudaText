#!/bin/sh

. ./cuda_ver.sh

outdir=~/Public
mkdir -p $outdir
dname=cudatext_$cuda_ver-1_gtk2_amd64

dd=~/tmp/$dname
sudo rm -rf $dd

mkdir -p $dd
mkdir $dd/DEBIAN
cp debfiles/control $dd/DEBIAN

mkdir $dd/usr
mkdir $dd/usr/bin
mkdir $dd/usr/share
mkdir $dd/usr/share/cudatext
mkdir $dd/usr/share/cudatext/py
mkdir $dd/usr/share/cudatext/py/cuda_addonman
mkdir $dd/usr/share/cudatext/py/cuda_comments
mkdir $dd/usr/share/cudatext/py/cuda_make_plugin
mkdir $dd/usr/share/cudatext/py/cuda_insert_time
mkdir $dd/usr/share/cudatext/py/cudax_lib
mkdir $dd/usr/share/cudatext/py/requests
mkdir $dd/usr/share/pixmaps
mkdir $dd/usr/share/applications

cp ../app/cudatext $dd/usr/bin
cp debfiles/cudatext.desktop $dd/usr/share/applications
cp debfiles/cudatext-256.png $dd/usr/share/pixmaps
cp -r ../app/data $dd/usr/share/cudatext
cp -r ../app/readme $dd/usr/share/cudatext
cp -r ../app/settings_default $dd/usr/share/cudatext

###py
cp -r ../app/py/*.py $dd/usr/share/cudatext/py
cp -r ../app/py/cuda_addonman/*.py $dd/usr/share/cudatext/py/cuda_addonman
cp -r ../app/py/cuda_addonman/*.inf $dd/usr/share/cudatext/py/cuda_addonman
cp -r ../app/py/cuda_comments/*.py $dd/usr/share/cudatext/py/cuda_comments
cp -r ../app/py/cuda_comments/*.inf $dd/usr/share/cudatext/py/cuda_comments
cp -r ../app/py/cuda_make_plugin/*.py $dd/usr/share/cudatext/py/cuda_make_plugin
cp -r ../app/py/cuda_make_plugin/*.inf $dd/usr/share/cudatext/py/cuda_make_plugin
cp -r ../app/py/cuda_insert_time/*.py $dd/usr/share/cudatext/py/cuda_insert_time
cp -r ../app/py/cuda_insert_time/*.inf $dd/usr/share/cudatext/py/cuda_insert_time
cp -r ../app/py/cudax_lib/*.py $dd/usr/share/cudatext/py/cudax_lib
cp -r ../app/py/requests/* $dd/usr/share/cudatext/py/requests


sudo chmod -R 755 $dd
sudo chown -R root:root $dd
dpkg-deb --build $dd $outdir/$dname.deb
