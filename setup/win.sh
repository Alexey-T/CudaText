#!/bin/sh
cd ~/cuda/cuda/app

dname=cudatext-win-x32

rm ~/Public/$dname.zip
zip -r -x*.pyc ~/Public/$dname.zip data readme settings_default *.manifest *.dll py*.zip dlls/* py/*.py py/cuda_addonman py/cuda_make_plugin py/cuda_insert_time py/cudax_lib py/requests py/cuda_comments

cd ~/Public/win_exe
zip ~/Public/$dname.zip cudatext.exe

cd ~/Downloads/cud
zip -r ~/Public/$dname.zip src/
