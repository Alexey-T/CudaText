#!/bin/sh
. ./cuda_ver.sh
cd ../app

cpu=$1
outdir=$2
exedir=$3
pydir=$4

zipname=cudatext-win-$cpu-$cuda_ver.zip
mkdir -p $outdir
mkdir $outdir/src 

rm $outdir/$zipname
zip -r -x*.pyc $outdir/$zipname data readme settings_default *.manifest py/*.py py/cuda_addonman py/cuda_project_man py/cuda_show_unsaved py/cuda_tabs_list py/cuda_make_plugin py/cuda_insert_time py/cudax_lib py/requests py/cuda_comments py/cuda_new_file py/cuda_palette

# Take exe
cd $exedir
zip $outdir/$zipname cudatext.exe

# Take sources
#cd $outdir/src
#curl -L https://github.com/Alexey-T/CudaText/archive/master.zip >CudaText.zip
#curl -L https://github.com/Alexey-T/ATFlatControls/archive/master.zip >ATFlatControls.zip
#curl -L https://github.com/Alexey-T/ATFileNotif-Lazarus/archive/master.zip >ATFileNotif.zip
#curl -L https://github.com/Alexey-T/ATGroups/archive/master.zip >ATGroups.zip
#curl -L https://github.com/Alexey-T/ATSynEdit/archive/master.zip >ATSynEdit.zip
#curl -L https://github.com/Alexey-T/ATSynEdit_Ex/archive/master.zip >ATSynEdit_Ex.zip
#curl -L https://github.com/Alexey-T/ATTabs/archive/master.zip >ATTabs.zip
#curl -L https://github.com/Alexey-T/EControl/archive/master.zip >EControl.zip
#curl -L https://github.com/Alexey-T/Python-for-Lazarus/archive/master.zip >Python.zip
#
#cd ..
#zip -r $outdir/$zipname src/

cd $pydir
zip -r $outdir/$zipname *
