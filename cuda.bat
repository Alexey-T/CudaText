@ECHO OFF
:: cuda.bat v2.0
:: A wrapper for DOS command line (github.com/Alexey-T/CudaText/issues/2484)

SET ct_fullpath=C:\wherever\you\have\the\program\cudatext.exe

IF %1d == d "%ct_fullpath%"
for %%* in (%1) do "%ct_fullpath%" "%%~dpF*"
