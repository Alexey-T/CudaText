; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "8.70"
!define MAX_ITEMS 664

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "4.30"
!define MAX_ITEMS 126

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "6.96"
!define MAX_ITEMS 21

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "7.37"
!define MAX_ITEMS 325

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "1.24"
!define MAX_ITEMS 39

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "2.85"
!define MAX_ITEMS 143

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "4.77"
!define MAX_ITEMS 537

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "3.93"
!define MAX_ITEMS 88

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "3.93"
!define MAX_ITEMS 579

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "4.19"
!define MAX_ITEMS 340

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "6.53"
!define MAX_ITEMS 438

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "7.48"
!define MAX_ITEMS 597

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "9.27"
!define MAX_ITEMS 791

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

; Sample NSIS script for lexer benchmarking
!include "MUI2.nsh"
!include "LogicLib.nsh"

!define APPNAME "Benchmark"
!define VERSION "2.62"
!define MAX_ITEMS 866

Name "${APPNAME} ${VERSION}"
OutFile "bench-setup.exe"
InstallDir "$PROGRAMFILES64\${APPNAME}"

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

Section "Main" SEC_MAIN
    SetOutPath "$INSTDIR"
    File "app.exe"
    WriteUninstaller "$INSTDIR\uninstall.exe"

; generated section 0
Section "Extra 0" SEC_0
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 0"
        SetOutPath "$INSTDIR\extra0"
        File "extra0.dat"
    ${EndIf}
SectionEnd

; generated section 1
Section "Extra 1" SEC_1
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 1"
        SetOutPath "$INSTDIR\extra1"
        File "extra1.dat"
    ${EndIf}
SectionEnd

; generated section 2
Section "Extra 2" SEC_2
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 2"
        SetOutPath "$INSTDIR\extra2"
        File "extra2.dat"
    ${EndIf}
SectionEnd

; generated section 3
Section "Extra 3" SEC_3
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 3"
        SetOutPath "$INSTDIR\extra3"
        File "extra3.dat"
    ${EndIf}
SectionEnd

; generated section 4
Section "Extra 4" SEC_4
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 4"
        SetOutPath "$INSTDIR\extra4"
        File "extra4.dat"
    ${EndIf}
SectionEnd

; generated section 5
Section "Extra 5" SEC_5
    ${If} ${SectionIsSelected} ${SEC_MAIN}
        DetailPrint "installing extra 5"
        SetOutPath "$INSTDIR\extra5"
        File "extra5.dat"
    ${EndIf}
SectionEnd

Section "Uninstall"
    Delete "$INSTDIR\app.exe"
    RMDir "$INSTDIR"
SectionEnd

