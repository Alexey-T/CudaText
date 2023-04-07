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

# ensure we have lazarus config
cfg=~/.lazarus/miscellaneousoptions.xml
[[ -f $cfg ]] || { echo "Could not find $cfg to patch, aborting"; exit; }

# patch config if needed and add WITH_GTK2_IM flag
grep -q WITH_GTK2_IM $cfg || sed -i '/<\/Profile0>/ s/.*/<Defines Count="1"><Item1 Value="WITH_GTK2_IM"\/><\/Defines>\n&/' $cfg

# update submodules
git submodule foreach git pull origin master

# update upstream
git pull upstream master

# add WITH_GTK2_IM to lazarus config if does not exists
sed -i -n -e '/-dWITH_GTK2_IM/!p' -e '$a-dWITH_GTK2_IM' ~/.lazarus/idemake.cfg

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
