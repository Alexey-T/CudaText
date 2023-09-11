#!/bin/bash
. ./cuda_ver.sh
cd ../app

os=$1
widgets=$2
cpu=$3
exedir=$4
outdir=$5

if [[ "$outdir" == "" ]]; then
    echo "Need script params"
    exit
fi

mkdir -p $outdir
tarfile2=$outdir/cudatext-$os-$widgets-$cpu-$cuda_ver.tar
tarfile=$outdir/_cud.tar

rm $outdir/cudatext-$os-$widgets-$cpu-*.xz
rm $tarfile

tar --exclude=*.pyc -cf $tarfile readme data settings_default py/*.py py/cuda_addonman py/cuda_project_man py/cuda_tabs_list py/cuda_make_plugin py/cuda_insert_time py/sys py/cuda_comments py/cuda_new_file py/cuda_palette py/cuda_prefs py/cuda_multi_installer py/cuda_sort py/cuda_snippet_panel py/cuda_lexer_detecter

cd ../setup/debfiles
tar -rf $tarfile cudatext-512.png

cd ../../app
cd $exedir
tar -rf $tarfile cudatext

cd $outdir
rm -rf cudatext/
mkdir cudatext
cd cudatext
tar -xf $tarfile
cd ..
tar -rf $tarfile2 cudatext/
rm $tarfile
rm -rf cudatext/

xz -z $tarfile2
