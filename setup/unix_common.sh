#!/bin/sh
. ./cuda_ver.sh
cd ../app

os=$1
widgets=$2
cpu=$3
exedir=$4
outdir=$5

mkdir -p $outdir
zipfile=$outdir/cudatext-$os-$widgets-$cpu-$cuda_ver.tar

rm $outdir/cudatext-$os-$widgets-$cpu-*.xz
tar --exclude=*.pyc -cf $zipfile readme data settings_default py/*.py py/cuda_addonman py/cuda_project_man py/cuda_tabs_list py/cuda_make_plugin py/cuda_insert_time py/sys py/cuda_comments py/cuda_new_file py/cuda_palette py/cuda_prefs py/cuda_multi_installer py/cuda_sort py/cuda_snippet_panel py/cuda_lexer_detecter

cd ../setup/debfiles
tar -rf $zipfile cudatext-512.png

cd ../../app
cd $exedir
tar -rf $zipfile cudatext

xz -z $zipfile
