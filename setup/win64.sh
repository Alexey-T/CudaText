#!/bin/sh
. ./cuda_ver.sh
cd ../app

cpu=x64
zipname=cudatext-win-$cpu-$cuda_ver.zip
outdir=~/Public
exedir=~/Public/win_exe/x64
mkdir -p $outdir
mkdir $outdir/src 

rm $outdir/$zipname
zip -r -x*.pyc $outdir/$zipname data readme settings_default *.manifest py/*.py py/cuda_addonman py/cuda_make_plugin py/cuda_insert_time py/cudax_lib py/requests py/cuda_comments py/cuda_new_file py/cuda_palette

# Take dll
cd ../app64
zip $outdir/$zipname dlls/* *.dll *.zip

# Take exe
cd $exedir
zip $outdir/$zipname cudatext.exe
