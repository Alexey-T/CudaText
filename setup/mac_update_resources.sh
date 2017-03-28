#!/bin/sh
from=~/cuda/cuda/app
app=~/cuda/cuda/app/cudatext.app

rm -rf $app/Contents/Resources/data
rm -rf $app/Contents/Resources/readme
rm -rf $app/Contents/Resources/sett*

cp -rf $from/data $app/Contents/Resources
cp -rf $from/readme $app/Contents/Resources
cp -rf $from/settings_default $app/Contents/Resources

mkdir $app/Contents/Resources/py
mkdir $app/Contents/Resources/py/cuda_addonman
mkdir $app/Contents/Resources/py/cuda_insert_time
mkdir $app/Contents/Resources/py/cuda_make_plugin
mkdir $app/Contents/Resources/py/cuda_comments
mkdir $app/Contents/Resources/py/cuda_new_file
mkdir $app/Contents/Resources/py/cuda_palette
mkdir $app/Contents/Resources/py/cudax_lib
mkdir $app/Contents/Resources/py/requests

cp $from/py/*.py $app/Contents/Resources/py
cp $from/py/cuda_addonman/*.inf $app/Contents/Resources/py/cuda_addonman
cp $from/py/cuda_addonman/*.py $app/Contents/Resources/py/cuda_addonman
cp $from/py/cuda_insert_time/*.py $app/Contents/Resources/py/cuda_insert_time
cp $from/py/cuda_insert_time/*.inf $app/Contents/Resources/py/cuda_insert_time
cp $from/py/cuda_make_plugin/*.py $app/Contents/Resources/py/cuda_make_plugin
cp $from/py/cuda_make_plugin/*.inf $app/Contents/Resources/py/cuda_make_plugin
cp $from/py/cuda_comments/*.py $app/Contents/Resources/py/cuda_comments
cp $from/py/cuda_comments/*.inf $app/Contents/Resources/py/cuda_comments
cp $from/py/cuda_new_file/*.py $app/Contents/Resources/py/cuda_new_file
cp $from/py/cuda_new_file/*.inf $app/Contents/Resources/py/cuda_new_file
cp -rf $from/py/cuda_palette/* $app/Contents/Resources/py/cuda_palette
cp $from/py/cudax_lib/*.py $app/Contents/Resources/py/cudax_lib
cp -rf $from/py/requests $app/Contents/Resources/py

echo Done
