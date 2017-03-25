#!/bin/bash
app=~/cuda/cuda/app

strip $app/cudatext

#copy bundle to folder
mkdir $app/cudatext-mac
cp -rf $app/cudatext.app $app/cudatext-mac/

#make .dmg
/usr/bin/hdiutil create ~/cudatext-mac-carbon.dmg -ov -srcfolder $app/cudatext-mac/
