#!/bin/bash

. ./cuda_ver.sh
debsuffix=$1
binsuffix=$2

outdir=~/cudatext_build
mkdir -p $outdir
debname=cudatext_$cuda_ver-1_$debsuffix

dd=~/tmp/$debname
sudo rm -rf $dd

mkdir -p $dd
mkdir $dd/DEBIAN
cp debfiles/$debsuffix/control $dd/DEBIAN

mkdir $dd/usr
mkdir $dd/usr/bin
mkdir $dd/usr/share
mkdir $dd/usr/share/cudatext
mkdir $dd/usr/share/cudatext/py
mkdir $dd/usr/share/cudatext/py/cuda_addonman
mkdir $dd/usr/share/cudatext/py/cuda_comments
mkdir $dd/usr/share/cudatext/py/cuda_lexer_detecter
mkdir $dd/usr/share/cudatext/py/cuda_make_plugin
mkdir $dd/usr/share/cudatext/py/cuda_multi_installer
mkdir $dd/usr/share/cudatext/py/cuda_insert_time
mkdir $dd/usr/share/cudatext/py/cuda_new_file
mkdir $dd/usr/share/cudatext/py/cuda_prefs
mkdir $dd/usr/share/cudatext/py/cuda_prefs/icons
mkdir $dd/usr/share/cudatext/py/cuda_palette
mkdir $dd/usr/share/cudatext/py/cuda_project_man
mkdir $dd/usr/share/cudatext/py/cuda_snippet_panel
mkdir $dd/usr/share/cudatext/py/cuda_sort
mkdir $dd/usr/share/cudatext/py/cuda_tabs_list
mkdir $dd/usr/share/cudatext/py/sys
mkdir $dd/usr/share/pixmaps
mkdir $dd/usr/share/applications
mkdir $dd/usr/share/doc
mkdir $dd/usr/share/doc/cudatext

cp ../app/bin/linux-$binsuffix/cudatext $dd/usr/bin
cp debfiles/cudatext.desktop $dd/usr/share/applications
cp debfiles/cudatext-512.png $dd/usr/share/pixmaps
cp -r ../app/data $dd/usr/share/cudatext
cp -r ../app/settings_default $dd/usr/share/cudatext
cp debfiles/copyright $dd/usr/share/doc/cudatext

###py
cp -r ../app/py/*.py $dd/usr/share/cudatext/py
cp -r ../app/py/cuda_addonman/*.py $dd/usr/share/cudatext/py/cuda_addonman
cp -r ../app/py/cuda_addonman/*.inf $dd/usr/share/cudatext/py/cuda_addonman
cp -r ../app/py/cuda_comments/*.py $dd/usr/share/cudatext/py/cuda_comments
cp -r ../app/py/cuda_comments/*.inf $dd/usr/share/cudatext/py/cuda_comments
cp -r ../app/py/cuda_lexer_detecter/* $dd/usr/share/cudatext/py/cuda_lexer_detecter
cp -r ../app/py/cuda_make_plugin/*.py $dd/usr/share/cudatext/py/cuda_make_plugin
cp -r ../app/py/cuda_make_plugin/*.inf $dd/usr/share/cudatext/py/cuda_make_plugin
cp -r ../app/py/cuda_multi_installer/*.py $dd/usr/share/cudatext/py/cuda_multi_installer
cp -r ../app/py/cuda_multi_installer/*.inf $dd/usr/share/cudatext/py/cuda_multi_installer
cp -r ../app/py/cuda_new_file/*.py $dd/usr/share/cudatext/py/cuda_new_file
cp -r ../app/py/cuda_new_file/*.inf $dd/usr/share/cudatext/py/cuda_new_file
cp -r ../app/py/cuda_prefs/*.py $dd/usr/share/cudatext/py/cuda_prefs
cp -r ../app/py/cuda_prefs/*.inf $dd/usr/share/cudatext/py/cuda_prefs
cp -r ../app/py/cuda_prefs/icons/* $dd/usr/share/cudatext/py/cuda_prefs/icons
rm -rf ../app/py/cuda_palette/__pycache__
cp -r ../app/py/cuda_palette/* $dd/usr/share/cudatext/py/cuda_palette
rm -rf ../app/py/cuda_insert_time/__pycache__
cp -r ../app/py/cuda_insert_time $dd/usr/share/cudatext/py
rm -rf ../app/py/cuda_project_man/__pycache__
cp -r ../app/py/cuda_project_man $dd/usr/share/cudatext/py
rm -rf ../app/py/cuda_snippet_panel/__pycache__
cp -r ../app/py/cuda_snippet_panel $dd/usr/share/cudatext/py
rm -rf ../app/py/cuda_sort/__pycache__
cp -r ../app/py/cuda_sort $dd/usr/share/cudatext/py
rm -rf ../app/py/cuda_tabs_list/__pycache__
cp -r ../app/py/cuda_tabs_list $dd/usr/share/cudatext/py
cp -r ../app/py/sys $dd/usr/share/cudatext/py

cp debfiles/changelog $dd/usr/share/doc/cudatext
gzip --best -n $dd/usr/share/doc/cudatext/changelog
mv $dd/usr/share/doc/cudatext/changelog.gz $dd/usr/share/doc/cudatext/changelog.Debian.gz

sudo chmod -R 755 $dd
sudo chmod 644 $dd/usr/share/applications/cudatext.desktop
sudo chmod 644 $dd/usr/share/doc/cudatext/*
sudo chmod 644 $dd/usr/share/pixmaps/cudatext*.png
sudo chown -R root:root $dd
sudo rm -rf $dd/usr/share/cudatext/py/cuda_lexer_detecter/__pycache__

cd $dd/usr/share/cudatext
sudo find . -type f -exec chmod 644 {} +
sudo find . -type d -exec chmod 755 {} +

dpkg-deb --build $dd $outdir/$debname.deb