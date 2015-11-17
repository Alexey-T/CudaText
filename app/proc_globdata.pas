(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_globdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  FileUtil, Dialogs, Graphics, ExtCtrls,
  jsonConf,
  Process,
  ATSynEdit,
  ATSynEdit_Keymap,
  ATSynEdit_Keymap_Init,
  ATStringProc,
  ATButtons,
  proc_cmd,
  proc_lexer,
  proc_msg,
  ecSyntAnal;

var
  AppBookmarkSetup: array[1..255] of
    record ImageIndex: integer; Color: TColor; end;
  AppBookmarkImagelist: TImageList = nil;

type
  TAppPathId = (
    cDirSettings,
    cDirSettingsDef,
    cDirData,
    cDirDataLexlib,
    cDirDataNewdoc,
    cDirDataThemes,
    cDirDataAcp,
    cDirDataAcpSpec,
    cDirReadme,
    cDirPy,
    cFileLexlib,
    cFileOptHistory,
    cFileOptDefault,
    cFileOptUser,
    cFileOptFiletypes,
    cFileOptKeymap,
    cFileOptPlugins,
    cFileHistoryList,
    cFileHistorySession,
    cFileLexerStyles,
    cFileReadmeHist,
    cFileReadmeMouse,
    cFileReadmeLexerInst
    );

type
  TUiOps = record
    VarFontName: string;
    VarFontSize: integer;

    PyLibrary: string;
    LexlibFilename: string;

    ListboxWidth: integer;
    ListboxItemCountCmd: integer;
    ListboxItemCountBm: integer;
    ListboxItemCountTabs: integer;
    ListboxCompleteSizeX: integer;
    ListboxCompleteSizeY: integer;
    ListboxFuzzySearch: boolean;

    TabSizeX: integer;
    TabSizeY: integer;
    TabIndentX: integer;
    TabIndentY: integer;
    TabAngle: integer;
    TabBottom: boolean;
    TabColorFull: boolean;
    TabShowX: boolean;
    TabShowPlus: boolean;
    TabDblClickClose: boolean;

    MaxHistoryMenu: integer;
    MaxHistoryFiles: integer;

    FindSuggestSel: boolean;
    FindSuggestWord: boolean;
    FindSelCase: integer;
    FindShowFindfirst: boolean;
    FindIndentVert: integer;
    FindIndentHorz: integer;

    EscapeClose: boolean;
    EscapeCloseConsole: boolean;
    InitialDir: string;

    ExportHtmlNumbers: boolean;
    ExportHtmlFontName: string;
    ExportHtmlFontSize: integer;

    TreeAutoSync: boolean;
    TreeTimeFill: integer;
    TreeTimeFocus: integer;
    PyChangeSlow: integer;

    NewdocLexer: string;
    NewdocEnc: string;
    NewdocEnds: integer;

    StatusNoSel: string;
    StatusSmallSel: string;
    StatusStreamSel: string;
    StatusColSel: string;
    StatusCarets: string;
    StatusSizeX: integer;
    StatusSizeY: integer;
    StatusCenter: boolean;
    StatusTime: integer;

    ShowTitlePath: boolean;
    ShowLastFiles: boolean;
    OneInstance: boolean;
    NotifEnabled: boolean;
    NotifTimeSec: integer;
  end;
var
  UiOps: TUiOps;

type
  TEditorOps = record
    OpFontName: string;
    OpFontSize: integer;
    OpSpaceX: integer;
    OpSpaceY: integer;
    OpTabSize: integer;
    OpTabSpaces: boolean;

    OpOvrSel: boolean;
    OpOvrOnPaste: boolean;
    OpUnderlineColor: boolean;
    OpUnderlineColorSize: integer;

    //view
    OpGutterShow: boolean;
    OpGutterFold: boolean;
    OpGutterFoldAlways: boolean;
    OpGutterBookmk: boolean;
    OpNumbersShow: boolean;
    OpNumbersFontSize: integer;
    OpNumbersStyle: integer;
    OpNumbersForCarets: boolean;
    OpNumbersCenter: boolean;
    OpRulerShow: boolean;
    OpRulerFontSize: integer;
    OpRulerSize: integer;
    OpRulerTextIndent: integer;
    OpMinimapShow: boolean;
    OpMinimapShowSelAlways: boolean;
    OpMinimapShowSelBorder: boolean;
    OpMinimapCharWidth: integer;
    OpMargin: integer;
    OpMarginString: string;

    //unprinted
    OpUnprintedShow: boolean;
    OpUnprintedSpaces: boolean;
    OpUnprintedEnds: boolean;
    OpUnprintedEndDetails: boolean;
    OpUnprintedReplaceSpec: boolean;

    OpUnprintedEndArrow: boolean;
    OpUnprintedTabArrowLen: integer;
    OpUnprintedSpaceDotScale: integer;
    OpUnprintedEndDotScale: integer;
    OpUnprintedEndFontScale: integer;
    OpUnprintedTabPointerScale: integer;

    //wrap
    OpWrapMode: integer;
    OpWrapIndented: boolean;

    //undo
    OpUndoLimit: integer;
    OpUndoGrouped: boolean;
    OpUndoAfterSave: boolean;

    //caret
    OpCaretBlinkTime: integer;
    OpCaretBlinkEn: boolean;
    OpCaretShapeNorm: integer;
    OpCaretShapeOvr: integer;
    OpCaretShapeRO: integer;
    OpCaretVirtual: boolean;
    OpCaretMulti: boolean;

    //general
    OpShowCurLine: boolean;
    OpShowCurLineMin: boolean;
    OpShowCurCol: boolean;
    OpShowLastLineOnTop: boolean;
    OpShowSelectBgFull: boolean;
    OpShowSyntaxBgFull: boolean;
    OpCopyLineIfNoSel: boolean;
    OpCutLineIfNoSel: boolean;
    OpSavingTrimSpaces: boolean;
    OpSavingForceFinalEol: boolean;
    OpShowHintOnVertScroll: boolean;
    OpDynHilite: boolean;

    OpWordChars: UnicodeString;
    OpHexChars: UnicodeString;

    //indent
    OpIndentAuto: boolean;
    OpIndentAutoKind: integer;
    OpIndentSize: integer;
    OpUnIndentKeepsAlign: boolean;

    //mouse
    OpMouse2ClickDragSelectsWords: boolean;
    OpMouseDragDrop: boolean;
    OpMouseNiceScroll: boolean;
    OpMouseRightClickMovesCaret: boolean;
    OpMouseEnableColumnSelection: boolean;
    OpMouseHideCursorOnType: boolean; //don't work on lin
    OpMouseGutterClickSelectedLine: boolean;

    //keys
    OpKeyBackspaceUnindent: boolean;
    OpKeyTabIndents: boolean;
    OpKeyHomeToNonSpace: boolean;
    OpKeyHomeEndNavigateWrapped: boolean;
    OpKeyEndToNonSpace: boolean;
    OpKeyPageKeepsRelativePos: boolean;
    OpKeyPageUpDownSize: integer;
    OpKeyUpDownKeepColumn: boolean;
    OpKeyUpDownNavigateWrapped: boolean;
    OpKeyLeftRightSwapSel: boolean;
    OpKeyLeftRightSwapSelAndSelect: boolean;
  end;
var
  EditorOps: TEditorOps;

function GetAppPath(id: TAppPathId): string;
function GetLexerOverrideFN(AName: string): string;
function GetActiveControl(Form: TWinControl): TWinControl;
function GetDefaultListItemHeight: integer;
function GetBitmapX(AColor: TColor): TBitmap;
function MsgBox(const Str: string; Flags: integer): integer;
function AppFindLexer(const fn: string): TecSyntAnalyzer;
procedure DoSaveKeyItem(K: TATKeymapItem);
procedure DoEnumLexers(L: TStringList; AlsoDisabled: boolean = false);

var
  Manager: TecSyntaxManager = nil;
  Keymap: TATKeymap = nil;

type TStrEvent = procedure(Sender: TObject; const ARes: string) of object;

const
  cmdFirstLexerCommand = 6000;
  cmdLastLexerCommand = 6400-1;
  cmdFirstPluginCommand = 6400;
  cmdLastPluginCommand = 6800-1;

const
  cEncNameUtf8 = 'UTF-8';
  cEncNameUtf8NoBom = 'UTF-8 no bom';
  cEncNameUtf16LE = 'UTF-16 LE';
  cEncNameUtf16BE = 'UTF-16 BE';
  cEncNameAnsi = 'ANSI';

  cEncNameCP1250 = 'CP1250';
  cEncNameCP1251 = 'CP1251';
  cEncNameCP1252 = 'CP1252';
  cEncNameCP1253 = 'CP1253';
  cEncNameCP1254 = 'CP1254';
  cEncNameCP1255 = 'CP1255';
  cEncNameCP1256 = 'CP1256';
  cEncNameCP1257 = 'CP1257';
  cEncNameCP1258 = 'CP1258';
  cEncNameCP437 = 'CP437';
  cEncNameCP850 = 'CP850';
  cEncNameCP852 = 'CP852';
  cEncNameCP866 = 'CP866';
  cEncNameCP874 = 'CP874';
  cEncNameISO1 = 'ISO-8859-1';
  cEncNameISO2 = 'ISO-8859-2';
  cEncNameMac = 'Macintosh';
  cEncNameCP932 = 'CP932';
  cEncNameCP936 = 'CP936';
  cEncNameCP949 = 'CP949';
  cEncNameCP950 = 'CP950';

type
  TAppPyEvent = (
    cEventOnOpen,
    cEventOnSaveAfter,
    cEventOnSaveBefore,
    cEventOnKey,
    cEventOnChange,
    cEventOnChangeSlow,
    cEventOnCaret,
    cEventOnNumber,
    cEventOnState,
    cEventOnFocus,
    cEventOnLexer,
    cEventOnComplete,
    cEventOnFuncHint,
    cEventOnGotoDef,
    cEventOnConsole,
    cEventOnCompare,
    cEventOnStart
    );
  TAppPyEvents = set of TAppPyEvent;

const
  cAppPyEvent: array[TAppPyEvent] of string = (
    'on_open',
    'on_save',
    'on_save_pre',
    'on_key',
    'on_change',
    'on_change_slow',
    'on_caret',
    'on_num',
    'on_state',
    'on_focus',
    'on_lexer',
    'on_complete',
    'on_func_hint',
    'on_goto_def',
    'on_console',
    'on_compare',
    'on_start'
    );


implementation

var
  bmpX: TBitmap = nil;

function InitPyLibraryPath: string;
begin
  {$ifdef windows}
    Result:= 'python33.dll';
  {$endif}
  {$ifdef linux}
    Result:= 'libpython3.4m.so.1.0';
  {$endif}
  {$ifdef darwin}
    Result:= '/Library/Frameworks/Python.framework/Versions/3.3/lib/libpython3.3.dylib';
    if FileExists(Result) then exit;
    Result:= '/Library/Frameworks/Python.framework/Versions/3.4/lib/libpython3.4.dylib';
    if FileExists(Result) then exit;
    Result:= '/Library/Frameworks/Python.framework/Versions/3.5/lib/libpython3.5.dylib';
  {$endif} ;
end;

var
  OpDirExe: string = '';
  OpDirLocal: string = '';
  OpDirPrecopy: string = '';

function GetDirPrecopy: string;
begin
  Result:=
  {$ifdef windows} '' {$endif}
  {$ifdef linux} '/usr/share/cudatext' {$endif}
  {$ifdef darwin} ExtractFileDir(OpDirExe)+'/Resources' {$endif}
end;

function MsgBox(const Str: string; Flags: integer): integer;
begin
  Result:= Application.MessageBox(PChar(Str), PChar(msgTitle), Flags);
end;

function GetAppPath(id: TAppPathId): string;
begin
  case id of
    cDirSettings:
      begin
        Result:= OpDirLocal+DirectorySeparator+'settings';
        CreateDirUTF8(Result);
      end;
    cDirSettingsDef:
      begin
        Result:= OpDirLocal+DirectorySeparator+'settings_default';
      end;
    cDirData:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data';
      end;
    cDirDataLexlib:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'lexlib';
      end;
    cDirDataNewdoc:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'newdoc';
      end;
    cDirDataThemes:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'themes';
      end;
    cDirDataAcp:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'autocomplete';
      end;
    cDirDataAcpSpec:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'autocompletespec';
      end;
    cDirReadme:
      begin
        Result:= OpDirLocal+DirectorySeparator+'readme';
      end;
    cDirPy:
      begin
        Result:= OpDirLocal+DirectorySeparator+'py';
      end;
    cFileLexlib:
      begin
        Result:= GetAppPath(cDirDataLexlib)+DirectorySeparator+UiOps.LexlibFilename;
      end;

    cFileOptDefault:
      begin
        Result:= GetAppPath(cDirSettingsDef)+DirectorySeparator+'default.json';
      end;
    cFileOptHistory:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'history.json';
      end;
    cFileOptUser:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'user.json';
      end;
    cFileOptFiletypes:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'filetypes.json';
      end;
    cFileOptKeymap:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'keys.json';
      end;
    cFileOptPlugins:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'plugins.json';
      end;
    cFileHistoryList:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'history files.json';
      end;
    cFileHistorySession:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'history session.json';
      end;
    cFileLexerStyles:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'lexer styles backup.ini';
      end;

    cFileReadmeHist:
      begin
        Result:= GetAppPath(cDirReadme)+DirectorySeparator+'history.txt';
      end;
    cFileReadmeMouse:
      begin
        Result:= GetAppPath(cDirReadme)+DirectorySeparator+'help mouse.txt';
      end;
    cFileReadmeLexerInst:
      begin
        Result:= GetAppPath(cDirReadme)+DirectorySeparator+'help lexers install.txt';
      end;
  end;
end;

procedure InitDirs;
var
  S: string;
begin
  OpDirExe:= ExtractFileDir(ParamStrUTF8(0));
  OpDirPrecopy:= GetDirPrecopy;

  if DirectoryExistsUTF8(
      OpDirExe+
      DirectorySeparator+'data'+
      DirectorySeparator+'lexlib') then
    OpDirLocal:= OpDirExe
  else
  begin
    {$ifdef windows}
    OpDirLocal:= GetEnvironmentVariableUTF8('appdata')+'\CudaText';
    {$else}
    OpDirLocal:= GetEnvironmentVariableUTF8('HOME')+'/.cudatext';
    {$endif}
    CreateDirUTF8(OpDirLocal);

    if DirectoryExistsUTF8(OpDirPrecopy) then
    begin
      {$ifdef linux}
      RunCommand(Format('cp -R -u -t %s /usr/share/cudatext/py /usr/share/cudatext/data /usr/share/cudatext/readme /usr/share/cudatext/settings_default', [OpDirLocal]), S);
      {$endif}
      {$ifdef darwin}
      RunCommand(Format('rsync -av "%s/" "%s"', [OpDirPrecopy, OpDirLocal]), S);
      {$endif}
    end;
  end;
end;

procedure InitEditorOps(var Op: TEditorOps);
begin
  with Op do
  begin
    OpFontName:= {$ifndef darwin} 'Courier New' {$else} 'Monaco' {$endif};
    OpFontSize:= {$ifndef darwin} 9 {$else} 11 {$endif};

    OpSpaceX:= 0;
    OpSpaceY:= 0;

    OpTabSize:= 8;
    OpTabSpaces:= false;

    OpOvrSel:= true;
    OpOvrOnPaste:= false;
    OpUnderlineColor:= true;
    OpUnderlineColorSize:= 3;

    OpGutterShow:= true;
    OpGutterFold:= true;
    OpGutterFoldAlways:= true;
    OpGutterBookmk:= true;

    OpNumbersShow:= true;
    OpNumbersFontSize:= 0;
    OpNumbersStyle:= Ord(cNumbersEach10th);
    OpNumbersForCarets:= false;
    OpNumbersCenter:= true;

    OpRulerShow:= false;
    OpRulerFontSize:= 8;
    OpRulerSize:= 20;
    OpRulerTextIndent:= 0;

    OpMinimapShow:= false;
    OpMinimapShowSelAlways:= false;
    OpMinimapShowSelBorder:= false;
    OpMinimapCharWidth:= 0;

    OpMargin:= cInitMarginRight;
    OpMarginString:= '';

    OpUnprintedShow:= false;
    OpUnprintedSpaces:= true;
    OpUnprintedEnds:= true;
    OpUnprintedEndDetails:= false;
    OpUnprintedReplaceSpec:= true;

    OpUnprintedEndArrow:= true;
    OpUnprintedTabArrowLen:= 1;
    OpUnprintedSpaceDotScale:= 15;
    OpUnprintedEndDotScale:= 30;
    OpUnprintedEndFontScale:= 80;
    OpUnprintedTabPointerScale:= 22;

    OpWrapMode:= 0;
    OpWrapIndented:= true;

    OpUndoLimit:= 5000;
    OpUndoGrouped:= true;
    OpUndoAfterSave:= false;

    OpCaretBlinkTime:= cInitTimerBlink;
    OpCaretBlinkEn:= true;
    OpCaretShapeNorm:= Ord(cInitCaretShapeIns);
    OpCaretShapeOvr:= Ord(cInitCaretShapeOvr);
    OpCaretShapeRO:= Ord(cInitCaretShapeRO);
    OpCaretVirtual:= true;
    OpCaretMulti:= true;

    OpShowCurLine:= false;
    OpShowCurLineMin:= true;
    OpShowCurCol:= false;
    OpShowLastLineOnTop:= false;
    OpShowSelectBgFull:= false;
    OpShowSyntaxBgFull:= true;
    OpCopyLineIfNoSel:= true;
    OpCutLineIfNoSel:= false;
    OpSavingTrimSpaces:= false;
    OpSavingForceFinalEol:= false;
    OpShowHintOnVertScroll:= false;
    OpDynHilite:= true;

    OpWordChars:= '';
    OpHexChars:= '';

    OpIndentAuto:= true;
    OpIndentAutoKind:= Ord(cIndentAsIs);
    OpIndentSize:= 2;
    OpUnIndentKeepsAlign:= true;

    OpMouse2ClickDragSelectsWords:= true;
    OpMouseDragDrop:= true;
    OpMouseNiceScroll:= true;
    OpMouseRightClickMovesCaret:= false;
    OpMouseEnableColumnSelection:= true;
    OpMouseHideCursorOnType:= false;
    OpMouseGutterClickSelectedLine:= true;

    OpKeyBackspaceUnindent:= true;
    OpKeyTabIndents:= true;
    OpKeyHomeToNonSpace:= true;
    OpKeyHomeEndNavigateWrapped:= true;
    OpKeyEndToNonSpace:= true;
    OpKeyPageKeepsRelativePos:= true;
    OpKeyPageUpDownSize:= Ord(cPageSizeFullMinus1);
    OpKeyUpDownKeepColumn:= true;
    OpKeyUpDownNavigateWrapped:= true;
    OpKeyLeftRightSwapSel:= true;
    OpKeyLeftRightSwapSelAndSelect:= false;
  end;
end;


procedure InitUiOps(var Op: TUiOps);
begin
  with Op do
  begin
    VarFontName:= 'default';
    VarFontSize:= {$ifdef windows} 9 {$else} 10 {$endif};

    LexlibFilename:= 'lib.lxl';
    PyLibrary:= InitPyLibraryPath;

    ListboxWidth:= 450;
    ListboxItemCountCmd:= 15;
    ListboxItemCountBm:= 10;
    ListboxItemCountTabs:= 30;
    ListboxCompleteSizeX:= 550;
    ListboxCompleteSizeY:= 200;
    ListboxFuzzySearch:= true;

    TabSizeX:= 170;
    TabSizeY:= 24;
    TabIndentX:= 5;
    TabIndentY:= 5;
    TabAngle:= 3;
    TabBottom:= false;
    TabColorFull:= false;
    TabShowX:= true;
    TabShowPlus:= true;
    TabDblClickClose:= false;

    MaxHistoryMenu:= 10;
    MaxHistoryFiles:= 25;

    FindSuggestSel:= false;
    FindSuggestWord:= true;
    FindSelCase:= 2;
    FindShowFindfirst:= true;
    FindIndentVert:= -5;
    FindIndentHorz:= 10;

    EscapeClose:= false;
    EscapeCloseConsole:= true;
    InitialDir:= '';

    ExportHtmlNumbers:= false;
    ExportHtmlFontSize:= 12;
    ExportHtmlFontName:= 'Courier New';

    TreeAutoSync:= true;
    TreeTimeFill:= 2000;
    TreeTimeFocus:= 2000;
    PyChangeSlow:= 2000;

    NewdocLexer:= '';
    NewdocEnc:= cEncNameUtf8;
    NewdocEnds:= {$ifdef windows} Ord(cEndWin) {$else} Ord(cEndUnix) {$endif};

    StatusNoSel:= 'Ln {y}, Col {x}';
    StatusSmallSel:= 'Ln {y}, Col {x}, sel';
    StatusStreamSel:= 'Ln {y}, Col {x}, {sel} lines sel';
    StatusColSel:= '{sel}x{cols} column';
    StatusCarets:= '{carets} carets, {sel} lines sel';
    StatusSizeX:= 200;
    StatusSizeY:= TabSizeY;
    StatusCenter:= true;
    StatusTime:= 5;

    ShowTitlePath:= false;
    ShowLastFiles:= true;
    OneInstance:= false;
    NotifEnabled:= false;
    NotifTimeSec:= 1;
  end;
end;

function GetLexerOverrideFN(AName: string): string;
begin
  AName:= StringReplace(AName, '/', '_', [rfReplaceAll]);
  AName:= StringReplace(AName, '\', '_', [rfReplaceAll]);
  AName:= StringReplace(AName, '*', '_', [rfReplaceAll]);
  Result:= GetAppPath(cDirSettings)+DirectorySeparator+'lexer '+AName+'.json';
end;

function AppFindLexer(const fn: string): TecSyntAnalyzer;
var
  c: TJsonConfig;
  fn_opt, s, ext: string;
begin
  fn_opt:= GetAppPath(cFileOptFiletypes);
  if FileExistsUTF8(fn_opt) then
  begin
    c:= TJsonConfig.Create(nil);
    try
      c.FileName:= fn_opt;

      //by filename
      s:= c.GetValue(ExtractFileName(fn), '');
      if s<>'' then
      begin
        Result:= Manager.FindAnalyzer(s);
        Exit
      end;

      //by extention
      ext:= ExtractFileExt(fn);
      if ext<>'' then
      begin
        s:= c.GetValue('*'+ext, '');
        if s<>'' then
        begin
          Result:= Manager.FindAnalyzer(s);
          Exit
        end;
      end;
    finally
      c.Free;
    end;
  end;

  Result:= DoFindLexerForFilename(Manager, fn);
end;


procedure DoSaveKeyItem(K: TATKeymapItem);
var
  c: TJSONConfig;
  path: string;
  i: integer;
  sl: tstringlist;
begin
  path:= IntToStr(K.Command);
  c:= TJSONConfig.Create(nil);
  sl:= TStringlist.create;
  try
    c.Formatted:= true;
    c.Filename:= GetAppPath(cFileOptKeymap);
    c.SetValue(path+'/name', K.Name);

    sl.clear;
    for i:= 0 to High(TATKeyArray) do
      if K.Keys1[i]<>0 then
        sl.Add(Inttostr(K.Keys1[i]));
    c.SetValue(path+'/k1', sl);

    sl.clear;
    for i:= 0 to High(TATKeyArray) do
      if K.Keys2[i]<>0 then
        sl.Add(Inttostr(K.Keys2[i]));
    c.SetValue(path+'/k2', sl);
  finally
    c.Free;
    sl.Free;
  end;
end;

function GetActiveControl(Form: TWinControl): TWinControl;
var
  Ctl: TControl;
  i, j: integer;
begin
  Result:= nil;
  for i:= 0 to Form.ControlCount-1 do
  begin
    Ctl:= Form.Controls[i];
    if (Ctl is TWinControl) then
      if (Ctl as TWinControl).Focused then
        begin Result:= Ctl as TWinControl; exit end;
    if Ctl is TPanel then
    begin
      Result:= GetActiveControl(Ctl as TPanel);
      if Result<>nil then exit;
    end;
  end;
end;

function GetDefaultListItemHeight: integer;
var
  bmp: TBitmap;
begin
  bmp:= TBitmap.Create;
  bmp.Canvas.Font.Name:= UiOps.VarFontName;
  bmp.Canvas.Font.Size:= UiOps.VarFontSize;
  Result:= bmp.Canvas.TextHeight('Pyj')+3;
  bmp.Free;
end;

function GetBitmapX(AColor: TColor): TBitmap;
const
  size=7;
  colBack=clWhite;
begin
  if not Assigned(bmpX) then
  begin
    bmpX:= TBitmap.Create;
    bmpX.SetSize(size, size);
    bmpX.Transparent:= true;
    bmpX.TransparentColor:= colBack;
  end;

  with bmpX.Canvas do
  begin
    Brush.Color:= colBack;
    FillRect(0, 0, size, size);
    Pen.Color:= AColor;
    Line(1, 1, size, size);
    Line(size, 0, 0, size);
  end;

  Result:= bmpX;
end;


procedure DoEnumLexers(L: TStringList; AlsoDisabled: boolean = false);
var
  i: Integer;
begin
  with Manager do
    for i:= 0 to AnalyzerCount-1 do
      if AlsoDisabled or not Analyzers[i].Internal then
        L.Add(Analyzers[i].LexerName);
end;


initialization
  InitDirs;
  InitEditorOps(EditorOps);
  InitUiOps(UiOps);

  Keymap:= TATKeymap.Create;
  InitKeymapFull(Keymap);
  InitKeymapApp(Keymap);

  FillChar(AppBookmarkSetup, SizeOf(AppBookmarkSetup), 0);
  AppBookmarkImagelist:= TImageList.Create(nil);

finalization
  FreeAndNil(Keymap);
  FreeAndNil(AppBookmarkImagelist);

end.

