#!/bin/bash

# components to install - order is important (dependencies hell)
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

# default config needed to build lazarus IDE
config='<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <MiscellaneousOptions>
    <Version Value="3"/>
    <BuildLazarusOptions>
      <Profiles Count="4">
        <Profile0 Name="Normal IDE">
          <LCLPlatform Value="gtk2"/>
          <IdeBuildMode Value="Build"/>
        </Profile0>
      </Profiles>
    </BuildLazarusOptions>
  </MiscellaneousOptions>
</CONFIG>'

# default lazarus build config file
cfgfile=~/.lazarus/miscellaneousoptions.xml

# create default lazarus config if non exists
[[ -f $cfgfile ]] || echo "$config" > $cfgfile

# patch config if needed and add WITH_GTK2_IM flag
grep -q WITH_GTK2_IM $cfgfile || sed -i '/<\/Profile0>/ s/.*/<Defines Count="1"><Item1 Value="WITH_GTK2_IM"\/><\/Defines>\n&/' $cfgfile

# update from git, including submodules
git pull --recurse-submodules

# compile components 
(cd comp && echo "$comps" | xargs lazbuild -qq)

# build CudaText, abort script on error
lazbuild app/cudatext.lpr || exit

# install components to IDE
#find . -name *.lpk -exec lazbuild --add-package '{}' \;
for comp in $comps;do lazbuild --add-package comp/$comp; done

# rebuild IDE with new components
lazbuild -qq --build-ide=
