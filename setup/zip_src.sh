#!/bin/bash
. ./cuda_ver.sh

mkdir ~/cudatext_build/src
cd ~/cudatext_build/src
curl -L https://github.com/Alexey-T/ATBinHex-Lazarus/archive/master.zip >ATBinHex.zip
curl -L https://github.com/Alexey-T/ATFileNotif-Lazarus/archive/master.zip >ATFileNotif.zip
curl -L https://github.com/Alexey-T/ATFlatControls/archive/master.zip >ATFlatControls.zip
curl -L https://github.com/Alexey-T/ATSynEdit/archive/master.zip >ATSynEdit.zip
curl -L https://github.com/Alexey-T/ATSynEdit_Ex_v1/archive/master.zip >ATSynEdit_Ex_v1.zip
curl -L https://github.com/Alexey-T/ATTabs/archive/master.zip >ATTabs.zip
curl -L https://github.com/Alexey-T/CudaText/archive/master.zip >CudaText.zip
curl -L https://github.com/Alexey-T/EControl_v1/archive/master.zip >EControl_v1.zip
curl -L https://github.com/Alexey-T/Python-for-Lazarus/archive/master.zip >Python.zip

cd ..
zip -r cudatext-src-$cuda_ver.zip src/
