#!/bin/sh
. ./cuda_ver.sh
cd ../app

cpu=$1
outdir=$2
exedir=$3
pydir=$4

zipname=cudatext-windows-$cpu-$cuda_ver.zip
mkdir -p $outdir
mkdir $outdir/src 

rm $outdir/$zipname
zip -r -x*.pyc $outdir/$zipname data readme settings_default *.manifest py/*.py py/cuda_addonman py/cuda_project_man py/cuda_tabs_list py/cuda_make_plugin py/cuda_insert_time py/sys py/cuda_comments py/cuda_new_file py/cuda_palette py/cuda_prefs py/cuda_multi_installer py/cuda_sort py/cuda_snippet_panel py/cuda_lexer_detecter

# Take exe
cd $exedir
zip $outdir/$zipname cudatext.exe

cd $pydir
zip -r $outdir/$zipname *
