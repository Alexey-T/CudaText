#!/bin/bash
ws=$1
cud=$2
exe=$3
bundle=$4
app=$cud/app

strip $exe
rm $bundle/Contents/MacOS/cudatext
cp $exe $bundle/Contents/MacOS/cudatext

. $cud/setup/cuda_ver.sh

#copy bundle to folder
mkdir $app/cudatext-mac-$ws
cp -rf $app/cudatext.app $app/cudatext-mac-$ws/
mv $app/cudatext-mac-$ws/cudatext.app $app/cudatext-mac-$ws/CudaText.app

if [ -d "$app/cudatext-mac-$ws/Applications" ]
then
  echo App link exists
else
  ln -s /Applications $app/cudatext-mac-$ws/Applications
fi

#make .dmg
/usr/bin/hdiutil create ~/cudatext-macos-$ws-$cuda_ver.dmg -ov -srcfolder $app/cudatext-mac-$ws/
