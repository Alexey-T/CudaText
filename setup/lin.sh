#!/bin/sh
cd ../app

mkdir -p ~/Public
name=~/Public/cudatext-linux-gtk2-amd64.tar

rm ~/Public/*gtk*.xz
tar --exclude=*.pyc -cf $name cudatext readme data settings_default py/*.py py/cuda_addonman py/cuda_make_plugin py/cuda_insert_time py/cudax_lib py/requests py/cuda_comments

cd ../setup/debfiles
tar -rf $name cudatext-256.png

xz -z $name
