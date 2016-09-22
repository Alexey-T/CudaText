#!/bin/sh
cd ../app

zipname=cudatext-win-x32
outdir=~/Public
exedir=~/Public/win_exe
mkdir -p $outdir
mkdir $outdir/src 

rm $outdir/$zipname.zip
zip -r -x*.pyc $outdir/$zipname.zip data readme settings_default *.manifest *.dll py*.zip dlls/* py/*.py py/cuda_addonman py/cuda_make_plugin py/cuda_insert_time py/cudax_lib py/requests py/cuda_comments

# Take exe
cd $exedir
zip $outdir/$zipname.zip cudatext.exe

# Take sources
cd $outdir/src
curl -L https://github.com/Alexey-T/CudaText/archive/master.zip >CudaText.zip
curl -L https://github.com/Alexey-T/ATButtons/archive/master.zip >ATButtons.zip
curl -L https://github.com/Alexey-T/ATFileNotif-Lazarus/archive/master.zip >ATFileNotif.zip
curl -L https://github.com/Alexey-T/ATGauge/archive/master.zip >ATGauge.zip
curl -L https://github.com/Alexey-T/ATGroups/archive/master.zip >ATGroups.zip
curl -L https://github.com/Alexey-T/ATScrollBar/archive/master.zip >ATScrollBar.zip
curl -L https://github.com/Alexey-T/ATStatus/archive/master.zip >ATStatus.zip
curl -L https://github.com/Alexey-T/ATSynEdit/archive/master.zip >ATSynEdit.zip
curl -L https://github.com/Alexey-T/ATTabs/archive/master.zip >ATTabs.zip
curl -L https://github.com/Alexey-T/EControl/archive/master.zip >EControl.zip
curl -L https://github.com/Alexey-T/Python-for-Lazarus/archive/master.zip >Python.zip

cd ..
zip -r $outdir/$zipname.zip src/
