#!/bin/bash

comps="\
bgrabitmap/bgrabitmap/bgrabitmappack.lpk
EncConv/encconv/encconv_package.lpk
ATBinHex-Lazarus/atbinhex/atbinhex_package.lpk
ATFlatControls/atflatcontrols/atflatcontrols_package.lpk
ATSynEdit/atsynedit/atsynedit_package.lpk
ATSynEdit_Cmp/atsynedit_cmp/atsynedit_cmp_package.lpk
EControl/econtrol/econtrol_package.lpk
ATSynEdit_Ex/atsynedit_ex/atsynedit_ex_package.lpk
Python-for-Lazarus/python4lazarus/python4lazarus_package.lpk
Emmet-Pascal/emmet/emmet_package.lpk"

# update submodules
git submodule foreach git pull origin master

# update upstream
git pull upstream master

# FIXME: fix Linux compile - remove GTK2_IME_CODE flag from atsynedit_package.lpk
sed -i '/<Other>/,/<\/Other>/d' comp/ATSynEdit/atsynedit/atsynedit_package.lpk

# compile components 
#find . -name *.lpk -exec lazbuild '{}' \;
(cd comp && echo "$comps" | xargs lazbuild)

# build CudaText
lazbuild app/cudatext.lpr

# install components to IDE
#find . -name *.lpk -exec lazbuild --add-package '{}' \;
for comp in $comps;do lazbuild --add-package comp/$comp; done

# rebuild IDE with new components
lazbuild --build-ide=

# FIXME: restore atsynedit_package.lpk
(cd comp/ATSynEdit && git checkout atsynedit/atsynedit_package.lpk)

