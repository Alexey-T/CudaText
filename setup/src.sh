#!/bin/bash
. ./cuda_ver.sh

mkdir ~/cudatext_build/src
cd ~/cudatext_build/src
curl -L https://github.com/Alexey-T/ATBinHex-Lazarus/archive/master.zip >ATBinHex.zip
curl -L https://github.com/Alexey-T/ATFlatControls/archive/master.zip >ATFlatControls.zip
curl -L https://github.com/Alexey-T/EncConv/archive/master.zip >EncConv.zip
curl -L https://github.com/Alexey-T/ATSynEdit/archive/master.zip >ATSynEdit.zip
curl -L https://github.com/Alexey-T/ATSynEdit_Cmp/archive/master.zip >ATSynEdit_Cmp.zip
curl -L https://github.com/Alexey-T/ATSynEdit_Ex/archive/master.zip >ATSynEdit_Ex.zip
#curl -L https://github.com/Alexey-T/ATSynEdit_Ex/archive/v2.zip >ATSynEdit_Ex-v2.zip
curl -L https://github.com/Alexey-T/CudaText/archive/master.zip >CudaText.zip
curl -L https://github.com/Alexey-T/EControl/archive/master.zip >EControl.zip
#curl -L https://github.com/Alexey-T/EControl/archive/v2.zip >EControl-v2.zip
curl -L https://github.com/Alexey-T/Python-for-Lazarus/archive/master.zip >Python.zip
curl -L https://github.com/Alexey-T/Emmet-Pascal/archive/master.zip >Emmet.zip
curl -L https://github.com/dinkumoil/cuda_shell_extension/archive/master.zip >ShellExtension.zip

cd ..
zip -r cudatext-src-$cuda_ver.zip src/
