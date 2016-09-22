#!/bin/sh
cd ../app

dname=cudatext-win-x32
outdir=~/Public
mkdir -p $outdir 

rm $outdir/$dname.zip
zip -r -x*.pyc $outdir/$dname.zip data readme settings_default *.manifest *.dll py*.zip dlls/* py/*.py py/cuda_addonman py/cuda_make_plugin py/cuda_insert_time py/cudax_lib py/requests py/cuda_comments

#
#Exe file must be in ~/Public/win_exe
#
cd $outdir/win_exe
zip $outdir/$dname.zip cudatext.exe

#
#This zips sources, must be in ~/Downloads/cud/src
#
cd ~/Downloads/cud
zip -r $outdir/$dname.zip src/
