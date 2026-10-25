; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "4.18"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "2.91"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "7.37"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "0.8"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "0.88"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "7.19"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "4.50"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "0.63"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "7.52"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "1.13"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "0.79"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "0.17"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "7.11"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

; Sample Inno Setup script for lexer benchmarking
#define AppName "Benchmark"
#define AppVersion "1.28"
#define AppPublisher "Bench Corp"

[Setup]
AppId={{7B3A5F0E-1234-5678-9ABC-DEF012345678}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputBaseFilename=bench-setup
Compression=lzma2
SolidCompression=yes
ArchitecturesInstallIn64BitMode=x64compatible

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "app.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "data\file0.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file1.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file2.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file3.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file4.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file5.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file6.dat"; DestDir: "{app}\data"; Flags: ignoreversion
Source: "data\file7.dat"; DestDir: "{app}\data"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\app.exe"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\app.exe"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Icons:"

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 0'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 1'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 2'), mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    MsgBox(ExpandConstant('step 3'), mbInformation, MB_OK);
end;

