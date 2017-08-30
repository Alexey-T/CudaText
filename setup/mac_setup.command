#!/bin/bash
app=~/cuda/cuda/app
strip $app/cudatext

. $app/../setup/cuda_ver.sh

makelink=$app/../setup/mac_files/create_symlink_cudatext.command

#copy bundle to folder
mkdir $app/cudatext-mac
cp -rf $app/cudatext.app $app/cudatext-mac/
cp $makelink $app/cudatext-mac/
mv $app/cudatext-mac/cudatext.app $app/cudatext-mac/CudaText.app

if [ -d "$app/cudatext-mac/Applications" ]
then
  echo App link exists
else
  ln -s /Applications $app/cudatext-mac/Applications
fi

#make .dmg
/usr/bin/hdiutil create ~/cudatext-mac-carbon-$cuda_ver.dmg -ov -srcfolder $app/cudatext-mac/
