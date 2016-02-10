(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit FormMain;

{$mode objfpc}{$H+}
{$define import_cudatext_py}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus,
  Clipbrd, StrUtils, Variants, IniFiles,
  FileUtil, LazFileUtils, LazUTF8, LclType, LclProc, LclIntf,
  jsonConf,
  PythonEngine,
  UniqueInstance,
  ecSyntAnal,
  ATButtons,
  ATListbox,
  ATSynEdit,
  ATSynEdit_Keymap,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Commands,
  ATSynEdit_Finder,
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_Export_HTML,
  ATSynEdit_Ranges,
  ATSynEdit_CanvasProc,
  ATSynEdit_Adapter_EControl,
  ATTabs,
  ATGroups,
  ATStatusBar,
  ATStrings,
  ATStringProc,
  atsynedit_form_complete,
  atsynedit_form_complete_synwrite,
  atsynedit_form_complete_css,
  atsynedit_form_complete_html,
  Gauges,
  ColorPalette,
  proc_str,
  proc_py,
  proc_py_const,
  proc_files,
  proc_globdata,
  proc_colors,
  proc_cmd,
  proc_editor,
  proc_miscutils,
  proc_msg,
  proc_install_zip,
  proc_lexer_styles,
  proc_keysdialog,
  proc_customdialog,
  formconsole,
  formframe,
  form_menu_commands,
  form_menu_list,
  form_menu_py,
  formgoto,
  formfind,
  formsavetabs,
  formconfirmrep,
  formlexerprop,
  formlexerlib,
  formlexerstylesload,
  formpalette,
  formcolorsetup,
  formabout,
  formcharmaps,
  math;


type
  { TfmMain }
  TfmMain = class(TForm)
    AppProps: TApplicationProperties;
    ListboxOut: TATListbox;
    ListboxVal: TATListbox;
    ButtonCancel: TATButton;
    FontDlg: TFontDialog;
    Gauge: TGauge;
    ImageListBm: TImageList;
    ImageListBar: TImageList;
    ImageListTree: TImageList;
    MainMenu: TMainMenu;
    mnuGr1p2H: TMenuItem;
    mnuEditSpToTab: TMenuItem;
    SepEd7: TMenuItem;
    mnuEditTabToSp: TMenuItem;
    mnuEditCharmap: TMenuItem;
    SepV2: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem14: TMenuItem;
    SepHelp1: TMenuItem;
    SepHelp2: TMenuItem;
    SepFile1: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    SepEd6: TMenuItem;
    mnuFileEndUn: TMenuItem;
    mnuFileEndMac: TMenuItem;
    mnuFileEnds: TMenuItem;
    SepFile4: TMenuItem;
    mnuFileEndWin: TMenuItem;
    mnuFileEnc: TMenuItem;
    mnuTextUndo: TMenuItem;
    mnuTextRedo: TMenuItem;
    MenuItem32: TMenuItem;
    mnuTextCut: TMenuItem;
    mnuTextCopy: TMenuItem;
    mnuTextPaste: TMenuItem;
    mnuTextDelete: TMenuItem;
    MenuItem37: TMenuItem;
    mnuTextSel: TMenuItem;
    mnuTextGotoDef: TMenuItem;
    mnuCmtToggleStr: TMenuItem;
    mnuPlug: TMenuItem;
    mnuFileHtml: TMenuItem;
    mnuTreeFold9: TMenuItem;
    mnuTreeFold7: TMenuItem;
    mnuTreeFold8: TMenuItem;
    mnuTreeFold2: TMenuItem;
    mnuTreeFold5: TMenuItem;
    mnuTreeFold6: TMenuItem;
    mnuTreeFold3: TMenuItem;
    mnuTreeFold4: TMenuItem;
    mnuTreeUnfoldAll: TMenuItem;
    mnuTreeFoldAll: TMenuItem;
    mnuTreeFoldLevel: TMenuItem;
    mnuViewSide: TMenuItem;
    mnuOpKeys: TMenuItem;
    mnuHelpWiki: TMenuItem;
    mnuOpColors: TMenuItem;
    mnuEditTrimL: TMenuItem;
    mnuEditTrimR: TMenuItem;
    mnuEditTrim: TMenuItem;
    mnuHelpLexers: TMenuItem;
    mnuTabColor: TMenuItem;
    mnuThemes: TMenuItem;
    mnuTabsize1: TMenuItem;
    mnuTabsize2: TMenuItem;
    mnuTabsize4: TMenuItem;
    mnuTabsize8: TMenuItem;
    MenuItem29: TMenuItem;
    mnuTabsizeSpace: TMenuItem;
    mnuFind2Prev: TMenuItem;
    mnuTabSaveAs: TMenuItem;
    mnuTabSave: TMenuItem;
    mnuOpFileTypes: TMenuItem;
    mnuFindPrev: TMenuItem;
    mnuOpLexLib: TMenuItem;
    mnuOpLexSub: TMenuItem;
    mnuOpLexProp: TMenuItem;
    mnuCmtAdd: TMenuItem;
    mnuCmtRemove: TMenuItem;
    mnuCmtToggle: TMenuItem;
    mnuEditCmtSub: TMenuItem;
    mnuFileCloseDel: TMenuItem;
    mnuOpLexer: TMenuItem;
    mnuOpMore: TMenuItem;
    MenuItem21: TMenuItem;
    mnuViewStatus: TMenuItem;
    mnuViewFullscr: TMenuItem;
    mnuFindWordNext: TMenuItem;
    mnuFindWordPrev: TMenuItem;
    SepSr2: TMenuItem;
    mnuFind2Next: TMenuItem;
    MenuItem20: TMenuItem;
    mnuFind2WordNext: TMenuItem;
    mnuFind2WordPrev: TMenuItem;
    mnuHelpChangelog: TMenuItem;
    mnuHelpMouse: TMenuItem;
    mnuHelpForum: TMenuItem;
    mnuViewToolbar: TMenuItem;
    mnuFontText: TMenuItem;
    mnuFontUi: TMenuItem;
    mnuFonts: TMenuItem;
    mnuFileReopen: TMenuItem;
    mnuOpUser: TMenuItem;
    SepOp1: TMenuItem;
    mnuOp: TMenuItem;
    mnuOpDefault: TMenuItem;
    mnuFileOpenSub: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuBookmarksSub: TMenuItem;
    mnuFindRepDialog: TMenuItem;
    mnuFindNext: TMenuItem;
    mnuFindDlg: TMenuItem;
    SepSr1: TMenuItem;
    mnuSortSub: TMenuItem;
    mnuSortAsc: TMenuItem;
    mnuSortDesc: TMenuItem;
    mnuBmPrev: TMenuItem;
    mnuBmNext: TMenuItem;
    mnuGotoBm: TMenuItem;
    mnuBmToggle: TMenuItem;
    mnuBmInvert: TMenuItem;
    mnuBmClear: TMenuItem;
    mnuCaseSent: TMenuItem;
    mnuCaseLow: TMenuItem;
    mnuCaseUp: TMenuItem;
    mnuCaseTitle: TMenuItem;
    mnuCaseInvert: TMenuItem;
    mnuCaseSub: TMenuItem;
    mnuSelExtLine: TMenuItem;
    mnuSelInvert: TMenuItem;
    mnuSelSplit: TMenuItem;
    SepSel1: TMenuItem;
    mnuSel: TMenuItem;
    mnuFileSaveAll: TMenuItem;
    mnuEditCopyLine: TMenuItem;
    mnuEditCopyAppend: TMenuItem;
    SepEd4: TMenuItem;
    mnuEditCopyFFull: TMenuItem;
    mnuEditCopyFName: TMenuItem;
    mnuEditCopyFDir: TMenuItem;
    mnuEditClipSub: TMenuItem;
    mnuGotoLine: TMenuItem;
    mnuSr: TMenuItem;
    mnuTabMove1: TMenuItem;
    mnuTabMove2: TMenuItem;
    mnuTabMove3: TMenuItem;
    mnuTabMove4: TMenuItem;
    mnuTabMove5: TMenuItem;
    mnuTabMove6: TMenuItem;
    MenuItem19: TMenuItem;
    mnuTabMoveNext: TMenuItem;
    mnuTabMovePrev: TMenuItem;
    mnuTabMoveSub: TMenuItem;
    mnuTabCloseSub: TMenuItem;
    MenuItem9: TMenuItem;
    mnuTabCloseOtherAll: TMenuItem;
    mnuTabCloseAllAll: TMenuItem;
    mnuTabCloseAllSame: TMenuItem;
    mnuTabCloseLeft: TMenuItem;
    mnuTabCloseRight: TMenuItem;
    mnuTabCloseThis: TMenuItem;
    mnuTabCloseOtherSame: TMenuItem;
    mnuCaretsUp1Page: TMenuItem;
    mnuCaretsUpBegin: TMenuItem;
    mnuCaretsDown1Line: TMenuItem;
    mnuCaretsDown1Page: TMenuItem;
    mnuCaretsDownEnd: TMenuItem;
    SepEd5: TMenuItem;
    mnuCaretsCancel: TMenuItem;
    mnuCaretsUp1Line: TMenuItem;
    mnuCaretsExtSub: TMenuItem;
    mnuEditLineMoveUp: TMenuItem;
    mnuEditLineMoveDown: TMenuItem;
    mnuEditLineDel: TMenuItem;
    mnuEditLineDup: TMenuItem;
    mnuEditIndent: TMenuItem;
    mnuEditUnindent: TMenuItem;
    mnuEditIndentSub: TMenuItem;
    mnuEditLineOp: TMenuItem;
    mnuHelpCmd: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuEditDel: TMenuItem;
    mnuSelAll: TMenuItem;
    SepEd2: TMenuItem;
    mnuEditRedo: TMenuItem;
    SepEd1: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuViewNums: TMenuItem;
    mnuViewRuler: TMenuItem;
    mnuViewFold: TMenuItem;
    mnuViewWrap: TMenuItem;
    mnuViewMinimap: TMenuItem;
    mnuViewSplitSub: TMenuItem;
    MenuItem10: TMenuItem;
    SepV1: TMenuItem;
    mnuViewUnpriShow: TMenuItem;
    mnuViewUnpriSpaces: TMenuItem;
    mnuViewUnpriEnds: TMenuItem;
    mnuViewUnpriEndsDet: TMenuItem;
    mnuViewUnpri: TMenuItem;
    mnuEndsWin: TMenuItem;
    mnuEndsUnix: TMenuItem;
    mnuEndsMac: TMenuItem;
    mnuViewSplitDo: TMenuItem;
    mnuViewSplitHorz: TMenuItem;
    mnuHelp: TMenuItem;
    mnuView: TMenuItem;
    mnuViewBottom: TMenuItem;
    mnuFileCloseAll: TMenuItem;
    mnuFileCloseOther: TMenuItem;
    SepFile2: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuGr1p2V: TMenuItem;
    mnuGr6: TMenuItem;
    mnuGr4G: TMenuItem;
    mnuGr4V: TMenuItem;
    mnuGr4H: TMenuItem;
    mnuGroups: TMenuItem;
    mnuGr1: TMenuItem;
    mnuGr3V: TMenuItem;
    mnuGr3H: TMenuItem;
    mnuGr2V: TMenuItem;
    mnuGr2H: TMenuItem;
    mnuFile: TMenuItem;
    SepFile3: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuFileClose: TMenuItem;
    OpenDlg: TOpenDialog;
    PanelBottom: TPanel;
    PanelLeft: TPanel;
    PanelMain: TPanel;
    PanelAll: TPanel;
    PopupEnc: TPopupMenu;
    PopupEnds: TPopupMenu;
    PopupLex: TPopupMenu;
    PopupFind: TPopupMenu;
    PopupText: TPopupMenu;
    PopupTree: TPopupMenu;
    PopupTabSize: TPopupMenu;
    PopupNewdoc: TPopupMenu;
    PopupRecents: TPopupMenu;
    PopupTab: TPopupMenu;
    PythonEngine: TPythonEngine;
    PythonIO: TPythonInputOutput;
    PythonMod: TPythonModule;
    SaveDlg: TSaveDialog;
    SplitterVert: TSplitter;
    SplitterHorz: TSplitter;
    TimerStatusAlt: TTimer;
    TimerTreeFill: TTimer;
    TimerCmd: TTimer;
    TimerStatus: TTimer;
    TimerTreeFocus: TTimer;
    ToolbarMain: TToolBar;
    tbNew: TToolButton;
    tbCopy: TToolButton;
    tbSelAll: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    tbDel: TToolButton;
    tbMinimap: TToolButton;
    tbSidePanel: TToolButton;
    tbBtmPanel: TToolButton;
    ToolButton15: TToolButton;
    tbUnpri: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    tbFind: TToolButton;
    ToolButton5: TToolButton;
    tbGoto: TToolButton;
    tbCut: TToolButton;
    tbPaste: TToolButton;
    ToolButton9: TToolButton;
    Tree: TTreeView;
    UniqInstance: TUniqueInstance;
    procedure ButtonCancelClick(Sender: TObject);
    procedure DoOnTabOver(Sender: TObject; ATabIndex: Integer);
    procedure DoOnTabsLeftClick(Sender: TObject);
    procedure DoOnTabsBottomClick(Sender: TObject);
    procedure FinderFound(Sender: TObject; APos1, APos2: TPoint);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormColorsApply(const AColors: TAppTheme);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FrameAddRecent(Sender: TObject);
    procedure FrameOnChangeCaretPos(Sender: TObject);
    procedure FrameOnSetLexer(Sender: TObject);
    procedure FrameParseBegin(Sender: TObject);
    procedure FrameParseDone(Sender: TObject);
    procedure ListboxOutClick(Sender: TObject);
    procedure ListboxOutDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
    procedure DoHelpWiki;
    procedure ListboxOutKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuThemesClick(Sender: TObject);
    procedure DoHelpLexers;
    procedure mnuOpKeysClick(Sender: TObject);
    procedure mnuTabColorClick(Sender: TObject);
    procedure mnuTabsize1Click(Sender: TObject);
    procedure mnuTabsize2Click(Sender: TObject);
    procedure mnuTabsize4Click(Sender: TObject);
    procedure mnuTabsize8Click(Sender: TObject);
    procedure MenuNewdocClick(Sender: TObject);
    procedure MenuRecentsClear(Sender: TObject);
    procedure mnuFind2NextClick(Sender: TObject);
    procedure mnuFind2PrevClick(Sender: TObject);
    procedure mnuFind2WordNextClick(Sender: TObject);
    procedure mnuFind2WordPrevClick(Sender: TObject);
    procedure DoHelpAbout;
    procedure DoHelpForum;
    procedure DoHelpChangelog;
    procedure DoHelpMouse;
    procedure MenuWindowClick(Sender: TObject);
    procedure mnuEndsMacClick(Sender: TObject);
    procedure mnuEndsUnixClick(Sender: TObject);
    procedure mnuEndsWinClick(Sender: TObject);
    procedure mnuTabCloseAllAllClick(Sender: TObject);
    procedure mnuTabCloseAllSameClick(Sender: TObject);
    procedure mnuTabCloseLeftClick(Sender: TObject);
    procedure mnuTabCloseOtherAllClick(Sender: TObject);
    procedure mnuTabCloseOtherSameClick(Sender: TObject);
    procedure mnuTabCloseRightClick(Sender: TObject);
    procedure mnuTabCloseThisClick(Sender: TObject);
    procedure mnuTabMove1Click(Sender: TObject);
    procedure mnuTabMove2Click(Sender: TObject);
    procedure mnuTabMove3Click(Sender: TObject);
    procedure mnuTabMove4Click(Sender: TObject);
    procedure mnuTabMove5Click(Sender: TObject);
    procedure mnuTabMove6Click(Sender: TObject);
    procedure mnuTabMoveNextClick(Sender: TObject);
    procedure mnuTabMovePrevClick(Sender: TObject);
    procedure mnuTabSaveAsClick(Sender: TObject);
    procedure mnuTabSaveClick(Sender: TObject);
    procedure mnuTabsizeSpaceClick(Sender: TObject);
    procedure mnuTreeFold2Click(Sender: TObject);
    procedure mnuTreeFold3Click(Sender: TObject);
    procedure mnuTreeFold4Click(Sender: TObject);
    procedure mnuTreeFold5Click(Sender: TObject);
    procedure mnuTreeFold6Click(Sender: TObject);
    procedure mnuTreeFold7Click(Sender: TObject);
    procedure mnuTreeFold8Click(Sender: TObject);
    procedure mnuTreeFold9Click(Sender: TObject);
    procedure mnuTreeFoldAllClick(Sender: TObject);
    procedure mnuTreeUnfoldAllClick(Sender: TObject);
    procedure PopupNewdocPopup(Sender: TObject);
    procedure PopupTabPopup(Sender: TObject);
    procedure PopupTextPopup(Sender: TObject);
    procedure PythonEngineAfterInit(Sender: TObject);
    procedure PythonIOSendData(Sender: TObject; const Data: AnsiString);
    procedure PythonIOSendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PythonModInitialization(Sender: TObject);
    procedure StatusPanelClick(Sender: TObject; AIndex: Integer);
    procedure tbBtmPanelClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbCutClick(Sender: TObject);
    procedure tbDelClick(Sender: TObject);
    procedure tbFindClick(Sender: TObject);
    procedure tbGotoClick(Sender: TObject);
    procedure tbMinimapClick(Sender: TObject);
    procedure tbNewClick(Sender: TObject);
    procedure tbNewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure tbOpenClick(Sender: TObject);
    procedure tbPasteClick(Sender: TObject);
    procedure tbRedoClick(Sender: TObject);
    procedure tbSaveClick(Sender: TObject);
    procedure tbSelAllClick(Sender: TObject);
    procedure tbSidePanelClick(Sender: TObject);
    procedure tbUndoClick(Sender: TObject);
    procedure tbUnpriClick(Sender: TObject);
    procedure TimerCmdTimer(Sender: TObject);
    procedure TimerStatusAltTimer(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure TimerTreeFillTimer(Sender: TObject);
    procedure TimerTreeFocusTimer(Sender: TObject);
    procedure TreeClick(Sender: TObject);
    procedure TreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure UniqInstanceOtherInstance(Sender: TObject; ParamCount: Integer;
      Parameters: array of String);
  private
    { private declarations }
    FListRecents: TStringList;
    FListNewdoc: TStringList;
    FListThemes: TStringList;
    FListOut: TStringlist;
    FListVal: TStringlist;
    FThemeName: string;
    FSessionFilename: string;
    FColorDialog: TColorDialog;
    Status: TATStatus;
    StatusAlt: TATStatus;
    Groups: TATGroups;
    TabsLeft: TATTabs;
    TabsBottom: TATTabs;
    FFinder: TATEditorFinder;
    FFindStop: boolean;
    FFindConfirmAll: TModalResult;
    FFindMark: boolean;
    FFullScreen: boolean;
    FOrigBounds: TRect;
    FOrigWndState: TWindowState;
    FOrigToolbar: boolean;
    FOrigStatus: boolean;
    FHandledOnShow: boolean;
    FTreeClick: boolean;
    FNewClickedEditor: TATSynEdit;
    FPyComplete_Text: string;
    FPyComplete_CharsLeft: integer;
    FPyComplete_CharsRight: integer;
    FPyComplete_CaretPos: TPoint;

    procedure CharmapOnInsert(const AStr: string);
    procedure DoInvalidateEditors;
    procedure DoPanel_Event(const AEvent: string);
    procedure DoPanel_OnSelChanged(Sender: TObject);
    function DoSidebar_ActivateTab(const ACaption: string): boolean;
    function DoSidebar_AddTab(const ACaption, AControlType: string; ATabIndex: integer): boolean;
    procedure DoApplyThemeToTreeview(C: TTreeview);
    procedure DoAutoComplete;
    procedure DoCudaLibAction(const AMethod: string);
    procedure DoDialogCharMap;
    procedure DoFindActionFromString(AStr: string);
    procedure DoFindOptionsFromString(const S: string);
    function DoFindOptionsToString: string;
    procedure DoGetSplitInfo(const Id: string; out BoolVert, BoolVisible: boolean;
      out NPos, NTotal: integer);
    function DoSidebar_GetControlHandle(const ACaption: string): PtrInt;
    function DoSidebar_GetTabIndexOfCaption(const Str: string): integer;
    procedure DoGotoDefinition;
    procedure DoShowFuncHint;
    procedure DoApplyFrameOps(F: TEditorFrame; const Op: TEditorOps;
      AForceApply: boolean);
    procedure DoApplyFontFixed;
    procedure DoApplyFontVar;
    procedure DoApplyAllOps;
    procedure DoApplyTheme;
    procedure DoClearRecentFileHistory;
    function DoOnConsole(const Str: string): boolean;
    function DoOnConsoleNav(const Str: string): boolean;
    function DoOnMacro(const Str: string): boolean;
    procedure DoOps_ShowEventPlugins;
    function DoDialogConfColors(var AColors: TAppTheme): boolean;
    function DoDialogMenuApi(const AText: string; AMultiline: boolean; AInitIndex: integer): integer;
    procedure DoFileExportHtml;
    procedure DoFileInstallZip(const fn: string);
    procedure DoFileCloseAndDelete;
    procedure DoFileNewFrom(const fn: string);
    function DoPyPanelAdd(AParams: string): boolean;
    function DoPyPanelDelete(const ACaption: string): boolean;
    function DoPyPanelFocus(const ACaption: string): boolean;
    procedure DoPyRunLastPlugin;
    procedure DoPyResetPlugins;
    procedure DoPyStringToEvents(const AEventStr: string; var AEvents: TAppPyEvents);
    procedure DoPyUpdateEvents(const AModuleName, AEventStr, ALexerStr, AKeyStr: string);
    procedure DoSetSplitInfo(const Id: string; NPos: integer);
    procedure DoPanel_OnClick(Sender: TObject);
    procedure DoPanel_OnDblClick(Sender: TObject);
    procedure DoToolbarAddButtom(AStr: string);
    procedure DoToolbarClick(Sender: TObject);
    procedure FrameLexerChange(Sender: TObject);
    procedure FrameOnEditorClickEndSelect(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure FrameOnEditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure MenuEncWithReloadClick(Sender: TObject);
    procedure MsgStatusAlt(const S: string; const NSeconds: integer);
    function SFindOptionsToTextHint: string;
    procedure UpdateMenuPlugins;
    procedure DoOps_LoadLexlib;
    procedure DoOps_SaveLexlib(Cfm: boolean);
    procedure DoOps_SaveHistory;
    procedure DoOps_SaveHistory_GroupView(c: TJsonConfig);
    procedure DoOps_LoadHistory;
    procedure DoOps_LoadHistory_GroupView(c: TJsonConfig);
    procedure DoOps_SaveSession(fn_session: string);
    procedure DoOps_LoadSession(fn_session: string);
    procedure DoOps_LoadOptionsAndApplyAll;
    procedure DoOps_LoadOptionsOverride(F: TEditorFrame);
    procedure DoOps_OpenFile_FileTypes;
    procedure DoOps_OpenFile_LexerOvr;
    procedure DoOps_PreinstallPlugins;
    procedure DoOps_LoadPlugins;
    procedure DoOps_DlgFont(var OpName: string; var OpSize: integer;
      const ConfStrName, ConfStrSize: string);
    procedure DoOps_DlgFontText;
    procedure DoOps_DlgFontUi;
    procedure DoOps_OpenFile_Default;
    procedure DoOps_OpenFile_User;
    procedure DoOps_LoadOptions(const fn: string; var Op: TEditorOps);
    procedure DoOps_LoadKeymap;
    procedure DoEditorsLock(ALock: boolean);
    procedure DoFindCurrentWordOrSel(ANext: boolean; AWordOrSel: boolean);
    procedure DoCopyFilenameDir;
    procedure DoCopyFilenameFull;
    procedure DoCopyFilenameName;
    procedure DoCopyLine;
    procedure DoDialogCommands;
    procedure DoDialogGoto;
    procedure DoDialogGotoBookmk;
    function DoDialogSaveTabs: boolean;
    procedure DoDialogLexerProp(an: TecSyntAnalyzer);
    procedure DoDialogLexerLib;
    procedure DoDialogLoadLexerStyles;
    procedure DoDialogColors;
    procedure DoShowConsole;
    procedure DoShowOutput;
    procedure DoShowValidate;
    procedure DoShowSidePanel(const ATabCaption: string);
    procedure DoTreeCollapseLevel(ALevel: integer);
    function FrameOfPopup: TEditorFrame;
    procedure FrameOnCommand(Sender: TObject; ACommand: integer; const AText: string;
      var AHandled: boolean);
    function DoFileCloseAll: boolean;
    procedure DoDialogFind(AReplaceMode: boolean);
    procedure DoFindResult(ok: boolean);
    procedure DoFindNext(ANext: boolean);
    procedure DoMoveTabTo(Num: Integer);
    procedure DoOnTabPopup(Sender: TObject);
    function DoFileOpen(AFilename: string): TEditorFrame;
    procedure DoFileOpenDialog;
    procedure DoFileSaveAll;
    procedure DoFileReopen;
    procedure DoLoadParamstr;
    procedure DoSortSel(ed: TATSynEdit; Asc, ANocase: boolean);
    procedure DoTabUntitled(D: TATTabData);
    procedure DoToggleFullScreen;
    procedure DoToggleSidePanel;
    procedure DoToggleBottomPanel;
    procedure DoToggleFindDialog;
    procedure DoToggleToolbar;
    procedure DoToggleStatusbar;
    procedure DoCommentAction(Act: TATCommentAction);
    procedure FindDialogDone(Sender: TObject; const Res: string);
    procedure FinderBadRegex(Sender: TObject);
    procedure FinderConfirmReplace(Sender: TObject; APos1, APos2: TPoint;
      AForMany: boolean; var AConfirm, AContinue: boolean);
    procedure FinderProgress(Sender: TObject; ACurPos, AMaxPos: integer;
      var AContinue: boolean);
    procedure FinderUpdateEditor(AUpdateText: boolean);
    procedure FrameOnSaveFile(Sender: TObject);
    procedure GetEditorIndexes(Ed: TATSynEdit; out AGroupIndex, ATabIndex: Integer);
    function GetModifiedCount: integer;
    function GetShowSidePanel: boolean;
    function GetShowStatus: boolean;
    function GetShowToolbar: boolean;
    function GetShowBottom: boolean;
    procedure GotoDialogDone(Sender: TObject; const Res: string);
    procedure InitFormFind;
    function IsFocusedBottom: boolean;
    function IsFocusedFind: boolean;
    function IsLexerMatches(const ANameList: string): boolean;
    procedure MenuPluginClick(Sender: TObject);
    procedure MenuThemeDefClick(Sender: TObject);
    procedure PyCompletionOnGetProp(Sender: TObject; out AText,
      ASuffix: string; out ACharsLeft, ACharsRight: integer);
    procedure Py_RunPlugin_Index(Num: integer);
    procedure SetFrameEncoding(Frame: TEditorFrame; const AEnc: string;
      AAlsoReloadFile: boolean);
    procedure SetLexerIndex(N: integer);
    procedure SetShowBottom(Value: boolean);
    procedure SetShowSidePanel(AValue: boolean);
    procedure SplitterOnPaint_Gr(Sender: TObject);
    procedure SplitterOnPaint_Main(Sender: TObject);
    procedure UpdateEditorTabsize(N: integer);
    procedure UpdateKeymapDynamicItems;
    procedure UpdateMenuChecked;
    procedure UpdateMenuEnc(AMenu: TMenuItem);
    procedure DoApplyUiOps;
    procedure InitPyEngine;
    procedure FrameOnChangeCaption(Sender: TObject);
    procedure FrameOnUpdateStatus(Sender: TObject);
    function DoTabAdd(Pages: TATPages): TATTabData;
    procedure DoOnTabFocus(Sender: TObject);
    procedure DoOnTabAdd(Sender: TObject);
    procedure DoOnTabClose(Sender: TObject; ATabIndex: Integer;
      var ACanClose, ACanContinue: boolean);
    procedure FrameOnFocus(Sender: TObject);
    function GetFrame(N: integer): TEditorFrame;
    procedure MenuEncNoReloadClick(Sender: TObject);
    procedure MenuLexClick(Sender: TObject);
    procedure MenuMainClick(Sender: TObject);
    procedure MenuRecentsClick(Sender: TObject);
    procedure SetFrame(Frame: TEditorFrame);
    procedure SetFullscreen(AValue: boolean);
    procedure SetLineEnds(Val: TATLineEnds);
    procedure MsgStatus(const S: string);
    procedure SetShowStatus(AValue: boolean);
    procedure SetShowToolbar(AValue: boolean);
    procedure UpdateMenuThemes(sub: TMenuItem);
    procedure UpdateStatusbarPanelsFromString(AStr: string);
    procedure UpdateTabsActiveColor(F: TEditorFrame);
    procedure UpdateTree(AFill: boolean; AConsiderTreeVisible: boolean=true);
    procedure UpKey(mi: TMenuItem; cmd: integer);
    procedure UpdateCaption;
    procedure UpdateEnabledAll(b: boolean);
    procedure InitFrameEvents(F: TEditorFrame);
    procedure UpdateInputForm(Form: TForm; APreferHeight: integer);
    procedure UpdateFrame(AUpdatedText: boolean= false);
    procedure UpdateMenuHotkeys;
    procedure UpdateMenuLexers;
    procedure UpdateAppForSearch(AStart: boolean);
    procedure UpdateMenuNewdoc;
    procedure UpdateStatus;
    procedure UpdateMenuRecent(F: TEditorFrame);
    procedure InitStatusButton;
  public
    { public declarations }
    FPanelCaptions: TStringlist;
    function FrameCount: integer;
    property Frames[N: integer]: TEditorFrame read GetFrame;
    function CurrentFrame: TEditorFrame;
    function CurrentEditor: TATSynEdit;
    function GetEditorFrame(Ed: TATSynEdit): TEditorFrame;
    function GetEditorBrother(Ed: TATSynEdit): TATSynEdit;
    property ShowFullscreen: boolean read FFullScreen write SetFullscreen;
    property ShowSidePanel: boolean read GetShowSidePanel write SetShowSidePanel;
    property ShowToolbar: boolean read GetShowToolbar write SetShowToolbar;
    property ShowStatus: boolean read GetShowStatus write SetShowStatus;
    property ShowBottom: boolean read GetShowBottom write SetShowBottom;
    function DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: array of string): string;
    procedure DoPyCommand(const AModule, AMethod: string; const AParam: string='');
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

var
  cStatusCaret: integer = 0;
  cStatusEnc: integer = 1;
  cStatusEnds: integer = 2;
  cStatusLexer: integer = 3;
  cStatusTabsize: integer = 4;
  cStatusInsOvr: integer = -1;
  cStatusMsg: integer = 5;

{ TfmMain }
{$I formmain_py.inc}

procedure TfmMain.StatusPanelClick(Sender: TObject; AIndex: Integer);
begin
  if not CurrentFrame.IsText then exit;

  if AIndex=cStatusEnc then
  begin
    if not CurrentFrame.ReadOnly then
      PopupEnc.PopUp;
  end
  else
  if AIndex=cStatusEnds then
  begin
    if not CurrentFrame.ReadOnly then
      PopupEnds.PopUp;
  end
  else
  if AIndex=cStatusLexer then
  begin
    PopupLex.PopUp;
  end
  else
  if AIndex=cStatusTabsize then
  begin
    PopupTabSize.Popup;
  end;
end;

procedure TfmMain.tbBtmPanelClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_ToggleBottomPanel);
end;

procedure TfmMain.tbCopyClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_ClipboardCopy);
  UpdateFrame();
  UpdateStatus;
end;

procedure TfmMain.tbCutClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_ClipboardCut);
  UpdateFrame(true);
  UpdateStatus;
end;

procedure TfmMain.tbDelClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_TextDeleteSelection);
  UpdateFrame(true);
  UpdateStatus;
end;

procedure TfmMain.tbFindClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_DialogFind);
  UpdateStatus;
end;

procedure TfmMain.tbGotoClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_DialogGoto);
  UpdateStatus;
end;

procedure TfmMain.tbMinimapClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_ToggleMinimap);
end;

procedure TfmMain.tbNewClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_FileNew);
  UpdateStatus;
end;

procedure TfmMain.tbNewContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
end;

procedure TfmMain.tbOpenClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_FileOpen);
  UpdateStatus;
end;

procedure TfmMain.tbPasteClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_ClipboardPaste);
  UpdateFrame(true);
  UpdateStatus;
end;

procedure TfmMain.tbRedoClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_Redo);
  UpdateFrame(true);
  UpdateStatus;
end;

procedure TfmMain.tbSaveClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_FileSave);
  UpdateStatus;
end;

procedure TfmMain.tbSelAllClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_SelectAll);
  UpdateFrame();
  UpdateStatus;
end;

procedure TfmMain.tbSidePanelClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_ToggleSidePanel);
end;

procedure TfmMain.tbUndoClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_Undo);
  UpdateFrame(true);
  UpdateStatus;
end;

procedure TfmMain.tbUnpriClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cCommand_ToggleUnprinted);
end;

procedure TfmMain.TimerStatusTimer(Sender: TObject);
begin
  MsgStatus('');
  TimerStatus.Enabled:= false;
end;

procedure TfmMain.TimerTreeFillTimer(Sender: TObject);
begin
  TimerTreeFill.Enabled:= false;
  UpdateTree(true);
end;

procedure TfmMain.TimerTreeFocusTimer(Sender: TObject);
begin
  TimerTreeFocus.Enabled:= false;
  UpdateTree(false);
end;

procedure TfmMain.TreeClick(Sender: TObject);
var
  R: TecTextRange;
  P: TPoint;
begin
  if Tree.Selected=nil then exit;
  if Tree.Selected.Data=nil then exit;
  R:= TecTextRange(Tree.Selected.Data);
  P:= CurrentFrame.Adapter.TreeGetPositionOfRange(R);
  FTreeClick:= true;
  CurrentEditor.DoGotoPos_AndUnfold(P, UiOps.FindIndentHorz, UiOps.FindIndentVert);
  CurrentEditor.SetFocus;
  FTreeClick:= false;
end;

procedure TfmMain.TreeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  //fix to hide parts on Tree's hints on editor canvas (Win32, moving mouse from
  //long hint to shorter)
  DoInvalidateEditors;
end;

procedure TfmMain.UniqInstanceOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
var
  i: integer;
  FStyle: TFormStyle;
begin
  for i:= 0 to ParamCount-1 do
    if FileExistsUTF8(Parameters[i]) then
      DoFileOpen(Parameters[i]);

  if WindowState=wsMinimized then
  begin
    WindowState:= wsNormal;
    Application.ProcessMessages;
  end;
  Application.BringToFront;

  //hack to show form
  FStyle:= FormStyle;
  FormStyle:= fsStayOnTop;
  FormStyle:= FStyle;
end;


procedure TfmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  AppBookmarkImagelist.AddImages(ImageListBm);
  for i:= 0 to 9 do
  begin
    AppBookmarkSetup[240+i].Color:= clMoneyGreen;
    AppBookmarkSetup[240+i].ImageIndex:= i;
  end;

  PanelAll.Align:= alClient;
  Manager:= TecSyntaxManager.Create(Self);
  FSessionFilename:= GetAppPath(cFileHistorySession);

  FPanelCaptions:= TStringList.Create;
  FListRecents:= TStringList.Create;
  FListNewdoc:= TStringList.Create;
  FListThemes:= TStringlist.Create;
  FListOut:= TStringlist.Create;
  FListVal:= TStringlist.Create;

  FillChar(AppPanelProp_Out, SizeOf(AppPanelProp_Out), 0);
  FillChar(AppPanelProp_Val, SizeOf(AppPanelProp_Val), 0);
  AppPanelProp_Out.Listbox:= ListboxOut;
  AppPanelProp_Out.Items:= FListOut;
  AppPanelProp_Val.Listbox:= ListboxVal;
  AppPanelProp_Val.Items:= FListVal;

  Status:= TATStatus.Create(Self);
  Status.Parent:= Self;
  Status.Align:= alBottom;
  Status.Top:= Height;
  Status.Height:= 23;
  Status.OnPanelClick:= @StatusPanelClick;

  Status.AddPanel(170, saMiddle, '?');
  Status.AddPanel(105, saMiddle, '?');
  Status.AddPanel(50, saMiddle, '?');
  Status.AddPanel(140, saMiddle, '?');
  Status.AddPanel(80, saMiddle, '?');
  Status.AddPanel(4000, saLeft, '');

  StatusAlt:= TATStatus.Create(Self);
  StatusAlt.Parent:= Self;
  StatusAlt.Align:= alBottom;
  StatusAlt.Top:= Status.Top-4;
  StatusAlt.Height:= Status.Height;
  StatusAlt.AddPanel(5000, saLeft, '?');
  StatusAlt.Hide;

  fmConsole:= TfmConsole.Create(Self);
  fmConsole.Parent:= PanelBottom;
  fmConsole.Align:= alClient;
  fmConsole.OnConsole:= @DoOnConsole;
  fmConsole.OnConsoleNav:= @DoOnConsoleNav;

  ListboxOut.Align:= alClient;
  ListboxVal.Align:= alClient;

  Groups:= TATGroups.Create(Self);
  Groups.Parent:= PanelMain;
  Groups.Align:= alClient;
  Groups.Mode:= gmOne;
  Groups.OnTabFocus:= @DoOnTabFocus;
  Groups.OnTabAdd:= @DoOnTabAdd;
  Groups.OnTabClose:= @DoOnTabClose;
  Groups.OnTabPopup:= @DoOnTabPopup;
  Groups.OnTabOver:= @DoOnTabOver;

  TabsBottom:= TATTabs.Create(Self);
  TabsBottom.Parent:= PanelBottom;
  TabsBottom.Align:= alBottom;

  TabsBottom.AddTab(-1, 'Console', nil);
  TabsBottom.AddTab(-1, 'Output', nil);
  TabsBottom.AddTab(-1, 'Validate', nil);
  TabsBottom.OnTabClick:= @DoOnTabsBottomClick;

  TabsLeft:= TATTabs.Create(Self);
  TabsLeft.Parent:= PanelLeft;
  TabsLeft.Align:= alTop;

  TabsLeft.AddTab(-1, 'Tree', nil);
  TabsLeft.OnTabClick:= @DoOnTabsLeftClick;

  with FAppSidePanels[0] do
  begin
    ItemCaption:= 'Tree';
    ItemTreeview:= Tree;
    ItemImagelist:= ImageListTree;
    ItemMenu:= PopupTree;
  end;

  FFinder:= TATEditorFinder.Create;
  FFinder.OptRegex:= true;
  FFinder.OnConfirmReplace:= @FinderConfirmReplace;
  FFinder.OnProgress:= @FinderProgress;
  FFinder.OnBadRegex:= @FinderBadRegex;
  FFinder.OnFound:=@FinderFound;

  UpdateMenuEnc(PopupEnc.Items);
  UpdateMenuEnc(mnuFileEnc);
  InitStatusButton;

  FFindStop:= false;
  FFindConfirmAll:= mrNone;

  Groups.Splitter1.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter2.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter3.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter4.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter5.OnPaint:= @SplitterOnPaint_Gr;
  SplitterVert.OnPaint:= @SplitterOnPaint_Main;
  SplitterHorz.OnPaint:= @SplitterOnPaint_Main;
end;

procedure TfmMain.DoOnTabsBottomClick(Sender: TObject);
var
  N: integer;
  Data: TATTabData;
  Ctl: TWinControl;
begin
  fmConsole.Hide;
  ListboxOut.Hide;
  ListboxVal.Hide;
  for N:= 0 to FPanelCaptions.Count-1 do
    (FPanelCaptions.Objects[N] as TAppPanelPropsClass).Data.Listbox.Hide;

  case TabsBottom.TabIndex of
    0:
      begin
        fmConsole.Show;
        fmConsole.Ed.SetFocus;
      end;
    1:
      begin
        ListboxOut.Show;
        ListboxOut.SetFocus;
      end;
    2:
      begin
        ListboxVal.Show;
        ListboxVal.SetFocus;
      end;
    else
      begin
        Data:= TabsBottom.GetTabData(TabsBottom.TabIndex);
        if Data=nil then exit;
        N:= FPanelCaptions.IndexOf(Data.TabCaption);
        if N<0 then exit;
        Ctl:= (FPanelCaptions.Objects[N] as TAppPanelPropsClass).Data.Listbox;
        Ctl.Show;
        if Ctl.CanFocus and Ctl.CanSetFocus then
          Ctl.SetFocus;
      end;
  end;
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
    UpdateMenuRecent(Frames[i]);

  DoOps_SaveHistory;
end;

procedure TfmMain.ButtonCancelClick(Sender: TObject);
begin
  FFindStop:= true;
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  cfm: boolean;
begin
  if GetModifiedCount>0 then
    cfm:= DoDialogSaveTabs
  else
    cfm:= true;
  CanClose:= cfm;
  //old code
  //CanClose:= DoFileCloseAll;
end;

procedure TfmMain.FormColorsApply(const AColors: TAppTheme);
begin
  Theme:= AColors;
  DoApplyTheme;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FListRecents);
  FreeAndNil(FListNewdoc);
  FreeAndNil(FListThemes);
  FreeAndNil(FListOut);
  FreeAndNil(FListVal);
  FreeAndNil(FPanelCaptions);
end;

procedure TfmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: integer;
begin
  for i:= 0 to Length(Filenames)-1 do
    if FileExistsUTF8(FileNames[i]) and
      not DirectoryExistsUTF8(FileNames[i]) then
      DoFileOpen(FileNames[i]);
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ANext: boolean;
begin
  if (Key=vk_tab) and (ssCtrl in Shift) then
  begin
    ANext:= not (ssShift in Shift);
    Groups.PagesCurrent.Tabs.SwitchTab(ANext);
    Key:= 0;
    exit
  end;

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    PyEscapeFlag:= true;
    if PyCommandRunning then
    begin
      Key:= 0;
      exit
    end;

    if fmConsole.ed.Focused or fmConsole.memo.Focused then
    begin
      if UiOps.EscapeCloseConsole then
        ShowBottom:= false
      else
        CurrentEditor.SetFocus;
      Key:= 0;
      exit
    end;

    if UiOps.EscapeClose then
    begin
      Close;
      Key:= 0;
      exit
    end;

    exit
  end;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  if FHandledOnShow then exit;
  FHandledOnShow:= true;
  TabsBottom.TabIndex:= 0;

  DoOps_LoadOptions(GetAppPath(cFileOptUser), EditorOps);
  DoApplyFontFixed;
  DoApplyFontVar;
  DoOps_LoadLexlib;
  DoApplyUiOps;
  InitPyEngine;

  DoFileOpen('');
  DoOps_LoadPlugins;
  DoOps_LoadHistory;
  DoOps_LoadKeymap;
  DoOps_PreinstallPlugins;

  UpdateMenuPlugins;
  UpdateMenuThemes(mnuThemes);
  UpdateMenuHotkeys;

  DoPyEvent(CurrentEditor, cEventOnFocus, []);
  DoPyEvent(CurrentEditor, cEventOnStart, []);

  ActiveControl:= CurrentEditor;
  UpdateStatus;
  DoLoadParamstr;
end;

procedure TfmMain.FrameAddRecent(Sender: TObject);
begin
  UpdateMenuRecent(Sender as TEditorFrame);
end;

procedure TfmMain.FrameOnChangeCaretPos(Sender: TObject);
begin
  if FTreeClick then exit;
  TimerTreeFocus.Enabled:= false;
  TimerTreeFocus.Enabled:= true;
end;

procedure TfmMain.MenuNewdocClick(Sender: TObject);
var
  N: integer;
begin
  N:= (Sender as TComponent).Tag;
  if (N>=0) and (N<FListNewdoc.Count) then
    DoFileNewFrom(FListNewdoc[N]);
end;

procedure TfmMain.MenuRecentsClear(Sender: TObject);
begin
  DoClearRecentFileHistory;
end;

procedure TfmMain.mnuFind2NextClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_FindNext);
end;

procedure TfmMain.mnuFind2PrevClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_FindPrev);
end;

procedure TfmMain.mnuFind2WordNextClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_FindCurWordNext);
end;

procedure TfmMain.mnuFind2WordPrevClick(Sender: TObject);
begin
  CurrentEditor.DoCommand(cmd_FindCurWordPrev);
end;

procedure TfmMain.DoClearRecentFileHistory;
begin
  FListRecents.Clear;
  UpdateMenuRecent(nil);
  //
  DeleteFileUTF8(GetAppPath(cFileHistoryList));
end;

procedure TfmMain.DoFileInstallZip(const fn: string);
var
  msg: string;
begin
  if DoInstallAddonFromZip(fn, Manager, GetAppPath(cDirDataAcp), msg) then
  begin
    DoOps_SaveLexlib(false);
    UpdateMenuLexers;
    MsgBox('Installed:'#13+msg, MB_OK or MB_ICONINFORMATION);
  end;
end;


procedure TfmMain.DoLoadParamstr;
var
  fn: string;
  cnt, i: integer;
begin
  cnt:= ParamCount;
  for i:= 1 to cnt do
  begin
    fn:= ParamStrUTF8(i);

    //OSX 10.8 gives "-psn**"
    if SBeginsWith(fn, '-') then Continue;

    if FileExistsUTF8(fn) then
      DoFileOpen(fn)
    else
    if MsgBox(
      Format(msgConfirmCreateNewFile, [fn]),
      MB_OKCANCEL or MB_ICONQUESTION) = id_ok then
    begin
      FCreateFile(fn);
      if FileExistsUTF8(fn) then
        DoFileOpen(fn);
    end;
  end;
end;

function TfmMain.GetModifiedCount: integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to FrameCount-1 do
    if Frames[i].Modified then
      Inc(Result);
end;

function TfmMain.GetShowSidePanel: boolean;
begin
  Result:= PanelLeft.Visible;
end;

function TfmMain.GetShowStatus: boolean;
begin
  Result:= Status.Visible;
end;

function TfmMain.GetShowToolbar: boolean;
begin
  Result:= ToolbarMain.Visible;
end;

function TfmMain.GetShowBottom: boolean;
begin
  Result:= PanelBottom.Visible;
end;

function TfmMain.DoDialogSaveTabs: boolean;
var
  i: integer;
  F: TEditorFrame;
  res: TModalResult;
begin
  Result:= false;
  with TfmSaveTabs.Create(nil) do
  try
    List.Clear;
    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      List.Items.Add(F.TabCaption+IfThen(F.Filename<>'', '  ('+ExtractFileDir(F.Filename)+')'));
      List.Checked[List.Count-1]:= F.Modified;
    end;

    res:= ShowModal;
    case res of
      mrClose:
        Result:= true;
      mrCancel:
        Result:= false;
      mrOk:
        begin
          Result:= true;
          for i:= 0 to List.Count-1 do
          begin
            F:= Frames[i];
            if List.Checked[i] then
              F.DoFileSave(false, SaveDlg);
          end;
        end;
    end;
  finally
    Free
  end;
end;

procedure TfmMain.DoDialogLexerProp(an: TecSyntAnalyzer);
begin
  if DoShowDialogLexerProp(an,
    EditorOps.OpFontName,
    EditorOps.OpFontSize,
    GetAppPath(cFileLexerStyles)) then
  begin
    UpdateMenuLexers;
    UpdateStatus;
    UpdateFrame;
    DoOps_SaveLexlib(true);
  end;
end;

procedure TfmMain.DoDialogLexerLib;
begin
  if DoShowDialogLexerLib(Manager,
    GetAppPath(cDirDataAcp),
    EditorOps.OpFontName,
    EditorOps.OpFontSize,
    GetAppPath(cFileLexerStyles)) then
  begin
    UpdateMenuLexers;
    UpdateStatus;
    UpdateFrame;
    DoOps_SaveLexlib(true);
  end;
end;

procedure TfmMain.DoCopyFilenameFull;
begin
  Clipboard.AsText:= CurrentFrame.FileName;
end;

procedure TfmMain.DoCopyFilenameDir;
begin
  Clipboard.AsText:= ExtractFileDir(CurrentFrame.FileName);
end;

procedure TfmMain.DoCopyFilenameName;
begin
  Clipboard.AsText:= ExtractFileName(CurrentFrame.FileName);
end;


procedure TfmMain.DoCopyLine;
var
  Str: atString;
  Ed: TATSynEdit;
  N: integer;
begin
  Ed:= CurrentEditor;
  N:= Ed.Carets[0].PosY;
  if not Ed.Strings.IsIndexValid(N) then exit;
  Str:= Ed.Strings.Lines[N];
  Clipboard.AsText:= Str;
end;

procedure TfmMain.DoHelpAbout;
begin
  with TfmAbout.Create(Self) do
  try
    labelVer.Caption:= cAppExeVersion;
    ShowModal;
  finally
    Free
  end;
end;

procedure TfmMain.DoHelpForum;
begin
  OpenURL('http://synwrite.sourceforge.net/forums/viewforum.php?f=20');
end;

procedure TfmMain.DoHelpChangelog;
begin
  DoFileOpen(GetAppPath(cFileReadmeHist));
end;

procedure TfmMain.DoHelpMouse;
begin
  DoFileOpen(GetAppPath(cFileReadmeMouse));
end;

procedure TfmMain.MenuWindowClick(Sender: TObject);
begin
  SetFrame(Frames[(Sender as TMenuItem).Tag]);
end;

procedure TfmMain.mnuEndsWinClick(Sender: TObject);
begin
  SetLineEnds(cEndWin);
end;

procedure TfmMain.mnuEndsUnixClick(Sender: TObject);
begin
  SetLineEnds(cEndUnix);
end;

procedure TfmMain.mnuEndsMacClick(Sender: TObject);
begin
  SetLineEnds(cEndMac);
end;

procedure TfmMain.SetLineEnds(Val: TATLineEnds);
begin
  CurrentFrame.LineEnds:= Val;
  UpdateStatus;
  MsgStatus(msgStatusEndsChanged);
end;

type
  TUniqInstanceHack = class(TUniqueInstance);

procedure TfmMain.DoApplyUiOps;
var
  i: integer;
begin
  UpdateStatusbarPanelsFromString(UiOps.StatusPanels);

  TimerTreeFill.Interval:= UiOps.TreeTimeFill;
  TimerTreeFocus.Interval:= UiOps.TreeTimeFocus;

  fmConsole.memo.OptCaretShapeRO:= TATSynCaretShape(EditorOps.OpCaretShapeRO);

  for i:= Low(FAppSidePanels) to High(FAppSidePanels) do
    with FAppSidePanels[i] do
    begin
      if ItemCaption='' then break;
      if Assigned(ItemTreeview) then
        ItemTreeview.ShowLines:= UiOps.TreeShowLines;
    end;

  TabsBottom.TabBottom:= true;
  TabsBottom.TabShowPlus:= false;
  TabsBottom.TabShowMenu:= false;
  TabsBottom.TabShowClose:= tbShowNone;
  TabsBottom.TabDoubleClickClose:= false;
  TabsBottom.TabMiddleClickClose:= false;
  TabsBottom.TabAngle:= UiOps.TabAngle;
  TabsBottom.TabIndentTop:= 0;
  TabsBottom.TabIndentInit:= UiOps.TabIndentX;
  TabsBottom.Height:= UiOps.TabSizeY;
  TabsBottom.TabHeight:= UiOps.TabSizeY-1;
  TabsBottom.TabWidthMax:= UiOps.TabSizeX;

  TabsLeft.TabBottom:= UiOps.TabBottom;
  TabsLeft.TabShowPlus:= false;
  TabsLeft.TabShowMenu:= false;
  TabsLeft.TabShowClose:= tbShowNone;
  TabsLeft.TabDoubleClickClose:= false;
  TabsLeft.TabMiddleClickClose:= false;
  TabsLeft.TabAngle:= UiOps.TabAngle;
  TabsLeft.TabIndentTop:= 0;
  TabsLeft.TabIndentInit:= UiOps.TabIndentX;
  TabsLeft.Height:= UiOps.TabSizeY;
  TabsLeft.TabHeight:= UiOps.TabSizeY-1;
  TabsLeft.TabWidthMax:= UiOps.TabSizeX;
  if UiOps.TabBottom then
    TabsLeft.Align:= alBottom
  else
    Tabsleft.Align:= alTop;

  Groups.SetTabOption(tabOptionBottomTabs, Ord(UiOps.TabBottom));
  Groups.SetTabOption(tabOptionShowXButtons, Ord(UiOps.TabShowX));
  Groups.SetTabOption(tabOptionShowPlus, Ord(UiOps.TabShowPlus));
  Groups.SetTabOption(tabOptionShowEntireColor, Ord(UiOps.TabColorFull));
  Groups.SetTabOption(tabOptionDoubleClickClose, Ord(UiOps.TabDblClickClose));
  Groups.SetTabOption(tabOptionAngle, UiOps.TabAngle);
  Groups.SetTabOption(tabOptionWidthMax, UiOps.TabSizeX);
  Groups.SetTabOption(tabOptionHeight1, UiOps.TabSizeY);
  Groups.SetTabOption(tabOptionHeight2, UiOps.TabSizeY-2);
  Groups.SetTabOption(tabOptionIndentInit, UiOps.TabIndentX);
  Groups.SetTabOption(tabOptionIndentColor, 4);
  Groups.SetTabOption(tabOptionWidecharModified, Ord('*'));
  Groups.SetTabOption(tabOptionShowNums, Ord(UiOps.TabNumbers));

  Status.Height:= UiOps.StatusHeight;
  ButtonCancel.Height:= UiOps.StatusHeight-2;
  TimerStatus.Interval:= UiOps.StatusTime*1000;

  ATButtonTheme.FontName:= UiOps.VarFontName;
  ATButtonTheme.FontSize:= UiOps.VarFontSize;

  cCompleteFormSizeX:= UiOps.ListboxCompleteSizeX;
  cCompleteFormSizeY:= UiOps.ListboxCompleteSizeY;

  if UiOps.OneInstance then
    if not UniqInstance.Enabled then
    begin
      UniqInstance.Enabled:= true;
      TUniqInstanceHack(UniqInstance).Loaded;

      if UniqInstance.PriorInstanceRunning then
        Application.Terminate;
        //note: app still works and will get DoFileOpen calls (e.g. on session opening)
        //so later need to check Application.Terminated
    end;

  DoApplyTheme;
end;


function TfmMain.DoFileOpen(AFilename: string): TEditorFrame;
var
  Pages: TATPages;
  D: TATTabData;
  F: TEditorFrame;
  i: integer;
  isOem: boolean;
begin
  Result:= nil;
  if Application.Terminated then exit;

  if AFilename='' then
  begin
    Pages:= Groups.PagesCurrent;
    D:= DoTabAdd(Pages);
    DoTabUntitled(D);
    Result:= D.TabObject as TEditorFrame;
    Exit
  end;

  //expand "./name"
  AFilename:= ExpandFileNameUTF8(AFilename);

  if not FileExistsUTF8(AFilename) then
  begin
    MsgBox(msgCannotFindFile+#13+AFilename, mb_ok or mb_iconerror);
    Exit
  end;

  //zip files
  if ExtractFileExt(AFilename)='.zip' then
  begin
    DoFileInstallZip(AFilename);
    exit
  end;

  //NonTextFiles: 0: prompt, 1: open, 2: don't open
  if not IsFilenameListedInExtensionList(AFilename, UiOps.PictureTypes) then
  if UiOps.NonTextFiles<>1 then
    if not IsFileContentText(AFilename, UiOps.NonTextFilesBufferKb, false, IsOem) then
      case UiOps.NonTextFiles of
        0:
          begin
            if MsgBox(Format(msgConfirmOpenNotText, [AFilename]),
              MB_OKCANCEL or MB_ICONWARNING)<>id_ok then Exit;
          end;
        2:
          Exit;
      end;

  //is file already opened? activate frame
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    if CompareFilenames(F.FileName, AFilename)=0 then
    begin
      SetFrame(F);
      Result:= F;
      UpdateStatus;
      UpdateTree(true);
      Exit
    end;
  end;

  //is current frame empty? use it
  F:= CurrentFrame;
  if F.IsEmpty then
  begin
    F.DoFileOpen(AFilename);
    Result:= F;
    UpdateStatus;
    MsgStatus('Opened: '+ExtractFileName(AFilename));
    DoPyEvent(F.Editor, cEventOnOpen, []);
    Exit
  end;

  Pages:= Groups.PagesCurrent;
  D:= DoTabAdd(Pages);
  F:= D.TabObject as TEditorFrame;
  F.DoFileOpen(AFilename);
  Result:= F;

  UpdateStatus;
  MsgStatus('Opened: '+ExtractFileName(AFilename));
  DoPyEvent(F.Editor, cEventOnOpen, []);
end;

procedure TfmMain.DoFileOpenDialog;
var
  i: integer;
begin
  with OpenDlg do
  begin
    FileName:= '';

    if CurrentFrame.FileName<>'' then
      InitialDir:= ExtractFileDir(CurrentFrame.FileName)
    else
      InitialDir:= UiOps.InitialDir;

    if not Execute then Exit;

    if Files.Count>1 then
    begin
      for i:= 0 to Files.Count-1 do
        DoFileOpen(Files[i]);
    end
    else
    begin
      if FileExistsUTF8(FileName) then
        DoFileOpen(FileName)
      else
      if MsgBox(
        Format(msgConfirmCreateNewFile, [FileName]),
        MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
      begin
        FCreateFile(FileName);
        DoFileOpen(FileName);
      end;
    end;
  end;
end;

procedure TfmMain.DoDialogCommands;
var
  Form: TfmCommands;
  Cmd: integer;
begin
  MsgStatus('Commands: F9 to configure keys; "@key" to find hotkey');

  Form:= TfmCommands.Create(Self);
  try
    UpdateInputForm(Form,
      Form.edit.Height+
      Form.edit.BorderSpacing.Around*3+
      Form.list.ItemHeight*UiOps.ListboxItemCountCmd);
    Form.keymap:= CurrentEditor.Keymap;
    Form.ShowModal;
    Cmd:= Form.ResultNum;
  finally
    FreeAndNil(Form);
  end;

  if Cmd>0 then
  begin
    CurrentEditor.DoCommand(Cmd);
    UpdateFrame;
  end;
end;

procedure TfmMain.DoDialogGoto;
begin
  if not Assigned(fmGoto) then
  begin
    fmGoto:= TfmGoto.Create(Self);
    fmGoto.OnDone:= @GotoDialogDone;
    fmGoto.Parent:= PanelMain;
    fmGoto.Align:= alBottom;
    fmGoto.Color:= GetAppColor('TabBg');
  end;

  with fmGoto do
  begin
    Show;
    edInput.Text:= '';
    edInput.SetFocus;
    UpdateState;
  end;
end;

procedure TfmMain.GotoDialogDone(Sender: TObject; const Res: string);
var
  Ed: TATSynEdit;
  Num: integer;
begin
  Ed:= CurrentEditor;

  if Res=cOpGotoClose then
  begin
    fmGoto.Hide;
    Ed.SetFocus;
    Exit;
  end;

  if Res=cOpGotoLine then
  begin
    Num:= StrToIntDef(fmGoto.edInput.Text, 0)-1;
    if Num<0 then
    begin
      MsgStatus(msgStatusBadNum);
      Exit
    end;
    Num:= Min(Num, Ed.Strings.Count-1);

    fmGoto.Hide;
    MsgStatus(Format(msgStatusGotoLine, [Num+1]));

    Ed.DoGotoPos_AndUnfold(Point(0, Num), UiOps.FindIndentHorz, UiOps.FindIndentVert);
    Ed.Update;
    Ed.SetFocus;
  end;
end;

procedure TfmMain.DoDialogGotoBookmk;
var
  ed: TATSynEdit;
  Form: TfmGotoList;
  Num, NumMax: integer;
  items: TStringlist;
  str: atString;
  i: integer;
begin
  ed:= CurrentEditor;
  items:= TStringlist.Create;
  try
    for i:= 0 to ed.Strings.Count-1 do
      if ed.Strings.LinesBm[i]>0 then
      begin
        str:= 'Line '+Inttostr(i+1)+': '+ed.Strings.Lines[i];
        items.AddObject(Utf8Encode(str), TObject(ptrint(i)));
      end;

    Num:= -1;
    Form:= TfmGotoList.Create(Self);
    try
      UpdateInputForm(Form,
        Form.List.ItemHeight*UiOps.ListboxItemCountBm +
        Form.List.BorderSpacing.Around*2);
      Form.Items:= items;
      Form.ShowModal;
      if Form.ResultIndex>=0 then
        Num:= ptrint(items.Objects[Form.ResultIndex]);
    finally
      FreeAndNil(Form);
    end;
  finally
    FreeAndNil(items);
  end;

  if Num<0 then
  begin
    MsgStatus(msgStatusCancel);
    Exit
  end;

  NumMax:= CurrentEditor.Strings.Count-1;
  if Num>NumMax then Num:= NumMax;

  CurrentEditor.DoGotoPos_AndUnfold(
    Point(0, Num),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert);
  MsgStatus(Format(msgStatusGotoLine, [Num+1]));
end;


procedure TfmMain.DoDialogColors;
const
  cDef = 'test';
var
  str: string;
begin
  if DoDialogConfColors(Theme) then
  begin
    DoApplyTheme;
    if Msgbox(msgConfirmSaveColors, MB_OKCANCEL or MB_ICONQUESTION)=id_ok then
    begin
      str:= Trim(InputBox(msgTitle, msgThemeName, cDef));
      if str='' then exit;
      str:= GetAppPath(cDirDataThemes)+DirectorySeparator+str+'.json';
      DoSaveTheme(str, Theme);
      UpdateMenuThemes(mnuThemes);
    end;
  end;
end;

function TfmMain.IsFocusedBottom: boolean;
begin
  Result:=
    fmConsole.ed.Focused or
    fmConsole.memo.Focused or
    ListboxOut.Focused or
    ListboxVal.Focused;
end;

function TfmMain.IsFocusedFind: boolean;
begin
  Result:= Assigned(fmFind) and
    (
    fmFind.Focused or
    fmFind.edFind.Focused or
    fmFind.edRep.Focused
    );
end;


procedure TfmMain.SetShowBottom(Value: boolean);
var
  bBottom: boolean;
begin
  if GetShowBottom=Value then exit;
  bBottom:= IsFocusedBottom;

  PanelBottom.Visible:= Value;
  SplitterHorz.Visible:= Value;
  SplitterHorz.Top:= 0;

  if not Value then
    if bBottom then
      CurrentEditor.SetFocus;

  UpdateStatus;
end;

procedure TfmMain.SetShowSidePanel(AValue: boolean);
begin
  if GetShowSidePanel=AValue then Exit;
  PanelLeft.Visible:= AValue;
  SplitterVert.Visible:= AValue;
  SplitterVert.Left:= PanelLeft.Width;
  if AValue then
    UpdateTree(true);
end;

procedure TfmMain.PopupNewdocPopup(Sender: TObject);
begin
  UpdateMenuNewdoc;
end;


procedure TfmMain.PopupTabPopup(Sender: TObject);
var
  Cnt, N: Integer;
begin
  Cnt:= Groups.PagesVisibleCount; //visible groups
  N:= Groups.PagesIndexOf(Groups.PopupPages); //current group

  mnuTabMove1.Enabled:= (Cnt>=2) and (N<>1);
  mnuTabMove2.Enabled:= {(Cnt>=2) and} (N<>2);
  mnuTabMove3.Enabled:= (Cnt>=3) and (N<>3);
  mnuTabMove4.Enabled:= (Cnt>=4) and (N<>4);
  mnuTabMove5.Enabled:= (Cnt>=5) and (N<>5);
  mnuTabMove6.Enabled:= (Cnt>=6) and (N<>6);
  mnuTabMoveNext.Enabled:= Cnt>=2;
  mnuTabMovePrev.Enabled:= Cnt>=2;
end;

procedure TfmMain.PythonEngineAfterInit(Sender: TObject);
var
  dir: string;
begin
  dir:= ExtractFileDir(Application.ExeName)+DirectorySeparator;
  {$ifdef windows}
  Py_SetSysPath([dir+'dlls', dir+ ChangeFileExt(UiOps.PyLibrary, '.zip')], false);
  {$endif}
  Py_SetSysPath([GetAppPath(cDirPy)], true);

  {$ifdef import_cudatext_py}
  try
    GetPythonEngine.ExecString('from cudatext import *');
  except
  end;
  {$endif}
end;

procedure TfmMain.InitPyEngine;
begin
  PythonEngine.DllPath:= ExtractFileDir(UiOps.PyLibrary);
  PythonEngine.DllName:= ExtractFileName(UiOps.PyLibrary);
  PythonEngine.LoadDll;
end;

procedure TfmMain.MenuEncNoReloadClick(Sender: TObject);
begin
  SetFrameEncoding(CurrentFrame, (Sender as TMenuItem).Caption, false);
end;

procedure TfmMain.MenuEncWithReloadClick(Sender: TObject);
begin
  SetFrameEncoding(CurrentFrame, (Sender as TMenuItem).Caption, true);
end;


procedure TfmMain.SetFrameEncoding(Frame: TEditorFrame; const AEnc: string; AAlsoReloadFile: boolean);
begin
  Frame.EncodingName:= AEnc;

  if AAlsoReloadFile then
  begin
    if Frame.FileName<>'' then
      Frame.DoFileReload(false)
    else
      MsgBox('Cannot reload untitled tab', MB_OK);
  end;

  UpdateFrame;
  UpdateStatus;
  MsgStatus(msgStatusEncChanged);
end;

procedure TfmMain.MenuLexClick(Sender: TObject);
begin
  CurrentFrame.Lexer:= TecSyntAnalyzer(pointer((Sender as TComponent).Tag));
  UpdateFrame;
  UpdateStatus;
end;

procedure TfmMain.DoOps_LoadLexlib;
var
  fn: string;
begin
  fn:= GetAppPath(cFileLexlib);
  if not FileExistsUTF8(fn) then
  begin
    MsgBox(msgCannotFindLexlib+#13+fn, mb_ok or mb_iconerror);
    Exit
  end;
  Manager.LoadFromFile(fn);
  UpdateMenuLexers;
end;

procedure TfmMain.DoOps_SaveLexlib(Cfm: boolean);
var
  fn: string;
begin
  if Cfm then
    if MsgBox(msgConfirmSaveLib, MB_OKCANCEL or MB_ICONWARNING)<>id_ok then exit;

  fn:= GetAppPath(cFileLexlib);
  if not FileExistsUTF8(fn) then exit;
  Manager.SaveToFile(fn);
  MsgStatus(msgStatusLexlibSave);
end;


procedure TfmMain.UpdateMenuLexers;
var
  i: integer;
  sl: tstringlist;
  an: TecSyntAnalyzer;
  mi, mi0: tmenuitem;
  ch: char;
  ch0: char;
begin
  UpdateKeymapDynamicItems;
  DoOps_LoadKeymap;

  PopupLex.Items.Clear;

  ch0:= '?';
  mi0:= nil;

  mi:= TMenuItem.create(self);
  mi.caption:= msgNoLexer;
  mi.OnClick:= @MenuLexClick;
  PopupLex.Items.Add(mi);

  sl:= tstringlist.create;
  try
    //make stringlist of all lexers
    for i:= 0 to Manager.AnalyzerCount-1 do
    begin
      an:= Manager.Analyzers[i];
      if not an.Internal then
        sl.AddObject(an.LexerName, an);
    end;
    sl.sort;

    //put stringlist to menu
    if not UiOps.LexerMenuGrouped then
    begin
      for i:= 0 to sl.count-1 do
      begin
        if sl[i]='' then Continue;
        mi:= TMenuItem.create(self);
        mi.caption:= sl[i];
        mi.tag:= ptrint(sl.Objects[i]);
        mi.OnClick:= @MenuLexClick;
        PopupLex.Items.Add(mi);
      end;
    end
    else
    //grouped view
    for i:= 0 to sl.count-1 do
    begin
      if sl[i]='' then Continue;
      ch:= UpCase(sl[i][1]);
      if ch<>ch0 then
      begin
        ch0:= ch;
        mi0:= TMenuItem.create(self);
        mi0.Caption:= ch;
        PopupLex.Items.Add(mi0);
      end;

      mi:= TMenuItem.create(self);
      mi.caption:= sl[i];
      mi.tag:= ptrint(sl.Objects[i]);
      mi.OnClick:= @MenuLexClick;
      if assigned(mi0) then
        mi0.add(mi)
      else
        PopupLex.Items.Add(mi);
    end;
  finally
    sl.free;
  end;
end;

procedure TfmMain.MsgStatus(const S: string);
var
  Frame: TEditorFrame;
  msg: string;
begin
  Frame:= CurrentFrame;
  msg:= s;

  if Frame.IsText then
  begin
    if Frame.ReadOnly then
      msg:= msgStatusReadonly + ' ' +msg;
    if Frame.MacroRecord then
      msg:= msgStatusMacroRec + ' ' +msg;
  end;

  Status[cStatusMsg]:= msg;

  if S='' then exit;
  TimerStatus.Enabled:= false;
  TimerStatus.Enabled:= true;
end;

procedure TfmMain.MsgStatusAlt(const S: string; const NSeconds: integer);
begin
  StatusAlt[0]:= S;
  StatusAlt.Show;
  StatusAlt.Top:= Status.Top-4;
  TimerStatusAlt.Interval:= Max(1, Min(30, NSeconds))*1000;
  TimerStatusAlt.Enabled:= false;
  TimerStatusAlt.Enabled:= true;
end;

procedure TfmMain.SetShowStatus(AValue: boolean);
begin
  Status.Visible:= AValue;
end;

procedure TfmMain.SetShowToolbar(AValue: boolean);
begin
  ToolbarMain.Visible:= AValue;
end;

procedure TfmMain.DoFileSaveAll;
var
  F: TEditorFrame;
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    if F.Modified then
      F.DoFileSave(false, SaveDlg);
  end;
end;

procedure TfmMain.DoFileReopen;
var
  F: TEditorFrame;
  bRO: boolean;
begin
  F:= CurrentFrame;
  if F.FileName='' then exit;
  if F.Modified then
    if MsgBox(
      Format(msgConfirmReopenModifiedTab, [F.FileName]),
      MB_OKCANCEL or MB_ICONQUESTION)<>id_ok then exit;

  bRO:= F.ReadOnly;
  F.ReadOnly:= false;;
  F.DoSaveHistory; //save hist to reopen at same scrollpos
  F.DoFileOpen(F.FileName, true);
  F.ReadOnly:= bRO;

  MsgStatus('Re-opened: '+ExtractFileName(F.Filename));
end;

function TfmMain.DoFileCloseAll: boolean;
var
  i: integer;
begin
  Result:= true;
  Groups.CloseTabs(tabCloseAll, false);
  for i:= 0 to FrameCount-1 do
    if Frames[i].Modified then
      begin Result:= false; Break end;
end;

procedure TfmMain.DoFileCloseAndDelete;
var
  fn: string;
begin
  if not CurrentFrame.IsText then exit;
  fn:= CurrentFrame.FileName;
  if fn='' then exit;

  if MsgBox(msgConfirmCloseDel+#13+fn, MB_OKCANCEL or MB_ICONWARNING)=id_ok then
    if Groups.CloseTabs(tabCloseCurrent, false) then
      DeleteFileUTF8(fn);
end;


procedure TfmMain.MenuRecentsClick(Sender: TObject);
var
  n: integer;
  fn: string;
begin
  n:= (Sender as TComponent).Tag;
  fn:= FListRecents[n];
  if FileExistsUTF8(fn) then
    DoFileOpen(fn)
  else
  begin
    MsgBox(msgCannotFindFile+#13+fn, MB_OK or MB_ICONERROR);
    FListRecents.Delete(n);
    UpdateMenuRecent(nil);
  end;
end;

procedure TfmMain.DoToggleFullScreen;
begin
  ShowFullscreen:= not ShowFullscreen;
end;

procedure TfmMain.DoToggleSidePanel;
begin
  ShowSidePanel:= not ShowSidePanel;
end;

procedure TfmMain.DoToggleBottomPanel;
begin
  ShowBottom:= not ShowBottom;
end;

procedure TfmMain.DoToggleFindDialog;
var
  bBottom: boolean;
begin
  bBottom:= IsFocusedFind;

  InitFormFind;
  fmFind.Visible:= not fmFind.Visible;

  if not fmFind.Visible then
    if bBottom then
      CurrentEditor.SetFocus;
end;

procedure TfmMain.DoToggleToolbar;
begin
  ShowToolbar:= not ShowToolbar;
end;

procedure TfmMain.DoToggleStatusbar;
begin
  ShowStatus:= not ShowStatus;
end;

procedure TfmMain.DoCudaLibAction(const AMethod: string);
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  Ed.Strings.BeginUndoGroup;
  try
    DoPyCommand('cudax_lib', AMethod);
  finally
    Ed.Strings.EndUndoGroup;
  end;
end;


procedure TfmMain.DoShowConsole;
begin
  ShowBottom:= true;
  TabsBottom.TabIndex:= 0;
  fmConsole.ed.SetFocus;
end;

procedure TfmMain.DoShowOutput;
begin
  ShowBottom:= true;
  TabsBottom.TabIndex:= 1;
end;

procedure TfmMain.DoShowValidate;
begin
  ShowBottom:= true;
  TabsBottom.TabIndex:= 2;
end;

procedure TfmMain.DoShowSidePanel(const ATabCaption: string);
begin
  if ATabCaption='-' then
  begin
    if PanelLeft.Visible then DoToggleSidePanel;
  end
  else
  begin
    if not PanelLeft.Visible then DoToggleSidePanel;
    if ATabCaption<>'' then
      DoSidebar_ActivateTab(ATabCaption);
  end;
end;

procedure TfmMain.SetFullscreen(AValue: boolean);
begin
  if FFullScreen=AValue then Exit;
  FFullScreen:=AValue;

  if FFullScreen then
  begin
    FOrigWndState:= WindowState;
    FOrigBounds:= BoundsRect;
    FOrigToolbar:= ShowToolbar;
    FOrigStatus:= ShowStatus;

    BorderStyle:= bsNone;
    BoundsRect:= Monitor.BoundsRect;
    ShowToolbar:= false;
    ShowStatus:= false;
  end
  else
  begin
    WindowState:= FOrigWndState;
    BoundsRect:= FOrigBounds;
    BorderStyle:= bsSizeable;
    ShowToolbar:= FOrigToolbar;
    ShowStatus:= FOrigStatus;
    BoundsRect:= FOrigBounds; //again
  end;
end;

procedure TfmMain.DoEditorsLock(ALock: boolean);
var
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
    Frames[i].Locked:= ALock;
end;

procedure TfmMain.DoFileNewFrom(const fn: string);
var
  F: TEditorFrame;
begin
  F:= DoFileOpen('');
  if F=nil then exit;
  F.Editor.Strings.LoadFromFile(fn);
  F.Lexer:= AppFindLexer(fn);
  UpdateFrame(true);
  UpdateStatus;
end;

procedure TfmMain.DoOps_OpenFile_Default;
var
  fn: string;
begin
  fn:= GetAppPath(cFileOptDefault);
  DoFileOpen(fn);
end;

procedure TfmMain.DoOps_OpenFile_User;
var
  fn: string;
begin
  fn:= GetAppPath(cFileOptUser);
  if not FileExistsUTF8(fn) then
  begin
    FCreateFile(fn, true);
    if not FileExistsUTF8(fn) then Exit;
  end;

  DoFileOpen(fn);
end;

procedure TfmMain.DoOps_OpenFile_FileTypes;
var
  fn: string;
begin
  fn:= GetAppPath(cFileOptFiletypes);
  if not FileExistsUTF8(fn) then
  begin
    FCreateFile(fn, true);
    if not FileExistsUTF8(fn) then Exit;
  end;

  DoFileOpen(fn);
end;

procedure TfmMain.DoOps_OpenFile_LexerOvr;
var
  fn: string;
  an: TecSyntAnalyzer;
begin
  an:= CurrentFrame.Lexer;
  if an=nil then exit;
  if an.LexerName='' then exit;

  fn:= GetLexerOverrideFN(an.LexerName);
  if not FileExistsUTF8(fn) then
  begin
    FCreateFile(fn, true);
    if not FileExistsUTF8(fn) then exit;
  end;

  DoFileOpen(fn);
end;

procedure TfmMain.MenuMainClick(Sender: TObject);
var
  F: TEditorFrame;
  EdFocus: boolean;
  Cmd: integer;
  SHint, SModule, SMethod, SParam: string;
begin
  Cmd:= (Sender as TComponent).Tag;
  SHint:= (Sender as TMenuItem).Hint;

  //dont do editor commands here if ed not focused
  F:= CurrentFrame;
  EdFocus:=
    F.Editor.Focused or
    F.Editor2.Focused or
    fmConsole.ed.Focused or
    fmConsole.memo.Focused;
  if not EdFocus then
    if (Cmd>0) and (Cmd<cmdFirstAppCommand) then exit;

  //-1 means run plugin: Hint='module,method,param'
  if (Cmd=-1) then
  begin
    SModule:= SGetItem(SHint);
    SMethod:= SGetItem(SHint);
    SParam:= SHint; //not SGetItem, allows to use ","
    DoPyCommand(SModule, SMethod, SParam);
  end
  else
    CurrentEditor.DoCommand(Cmd);

  UpdateFrame;
  UpdateStatus;
end;

procedure TfmMain.SetLexerIndex(N: integer);
begin
  if (N>=0) and (N<Manager.AnalyzerCount) then
  begin
    CurrentFrame.Lexer:= Manager.Analyzers[N];
    UpdateFrame;
    UpdateStatus;
  end;
end;


procedure TfmMain.DoAutoComplete;
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  LexName: string;
  IsPascal, IsCss, IsHtml, IsCaseSens: boolean;
  FileHtml, FileCss, FileAcp: string;
begin
  F:= CurrentFrame;
  Ed:= CurrentEditor;

  if DoPyEvent(Ed, cEventOnComplete, [])=cPyTrue then exit;

  if F.Lexer=nil then exit;
  if Ed.Carets.Count<>1 then exit;

  LexName:= F.LexerNameAtPos(Point(Ed.Carets[0].PosX, Ed.Carets[0].PosY));
  MsgStatus('Trying auto-complete for: '+LexName);
  if LexName='' then exit;

  //'php_'->'php'
  if LexName[Length(LexName)]='_' then
    Delete(LexName, Length(Lexname), 1);

  IsPascal:= Pos('Pascal', LexName)>0;
  IsHtml:= UiOps.AutocompleteHtml and (Pos('HTML', LexName)>0);
  IsCss:= UiOps.AutocompleteCss and (LexName='CSS');
  IsCaseSens:= false; //cannot detect it yet
  FileCss:= GetAppPath(cDirDataAcpSpec)+DirectorySeparator+'css_list.ini';
  FileHtml:= GetAppPath(cDirDataAcpSpec)+DirectorySeparator+'html_list.ini';
  FileAcp:= GetAppPath(cDirDataAcp)+DirectorySeparator+LexName+'.acp';

  if IsHtml then
  begin
    if EditorHasCssAtCaret(Ed) then
      DoEditorCompletionCss(Ed, FileCss)
    else
      DoEditorCompletionHtml(Ed, FileHtml);
  end
  else
  if IsCss then
    DoEditorCompletionCss(Ed, FileCss)
  else
    DoEditorCompletionAcp(Ed, FileAcp, IsCaseSens, IsPascal);
end;

procedure TfmMain.mnuTreeFold2Click(Sender: TObject);
begin
  DoTreeCollapseLevel(2);
end;

procedure TfmMain.mnuTreeFold3Click(Sender: TObject);
begin
  DoTreeCollapseLevel(3);
end;

procedure TfmMain.mnuTreeFold4Click(Sender: TObject);
begin
  DoTreeCollapseLevel(4);
end;

procedure TfmMain.mnuTreeFold5Click(Sender: TObject);
begin
  DoTreeCollapseLevel(5);
end;

procedure TfmMain.mnuTreeFold6Click(Sender: TObject);
begin
  DoTreeCollapseLevel(6);
end;

procedure TfmMain.mnuTreeFold7Click(Sender: TObject);
begin
  DoTreeCollapseLevel(7);
end;

procedure TfmMain.mnuTreeFold8Click(Sender: TObject);
begin
  DoTreeCollapseLevel(8);
end;

procedure TfmMain.mnuTreeFold9Click(Sender: TObject);
begin
  DoTreeCollapseLevel(9);
end;

procedure TfmMain.mnuTreeFoldAllClick(Sender: TObject);
begin
  Tree.FullCollapse;
end;

procedure TfmMain.mnuTreeUnfoldAllClick(Sender: TObject);
begin
  Tree.FullExpand;
end;


procedure TfmMain.DoTreeCollapseLevel(ALevel: integer);
var
  Node: TTreeNode;
  i: integer;
begin
  Tree.Items.BeginUpdate;
  Tree.FullExpand;
  try
    for i:= 0 to Tree.Items.Count-1 do
    begin
      Node:= Tree.Items[i];
      if Node.Level>=ALevel-1 then
        Node.Collapse(true);
    end;
  finally
    Tree.Items.EndUpdate;
  end;
end;

procedure TfmMain.DoFileExportHtml;
var
  Ed: TATSynEdit;
  STitle: string;
  Opt: TOpenOptions;
begin
  STitle:= ExtractFileName(CurrentFrame.FileName);
  if STitle='' then STitle:= 'untitled';
  SaveDlg.Filename:= STitle+'.html';
  SaveDlg.InitialDir:= GetTempDir(false);

  Opt:= SaveDlg.Options;
  try
    SaveDlg.Options:= SaveDlg.Options-[ofOverwritePrompt];
    SaveDlg.Filter:= 'HTML files|*.htm;*.html';
    if not SaveDlg.Execute then exit;
  finally
    SaveDlg.Options:= Opt;
  end;

  Ed:= CurrentEditor;
  Ed.DoCommand(cCommand_SelectNone);
  DoEditorExportToHTML(Ed, SaveDlg.FileName, STitle,
    UiOps.ExportHtmlFontName,
    UiOps.ExportHtmlFontSize,
    UiOps.ExportHtmlNumbers,
    GetAppColor('ExportHtmlBg'),
    GetAppColor('ExportHtmlNumbers')
    );

  if MsgBox('Open created document?', MB_OKCANCEL or MB_ICONQUESTION)=id_ok then
    OpenDocument(SaveDlg.FileName);
end;


function TfmMain.DoDialogMenuApi(const AText: string; AMultiline: boolean;
  AInitIndex: integer): integer;
var
  Form: TfmMenuApi;
  S, SItem: string;
begin
  Form:= TfmMenuApi.Create(nil);
  try
    S:= AText;
    repeat
      SItem:= SGetItem(S, #10);
      if SItem='' then Break;
      Form.listItems.Add(SItem);
    until false;

    UpdateInputForm(Form,
      Form.edit.Height+
      Form.edit.BorderSpacing.Around*3+
      Form.list.ItemHeight*UiOps.ListboxItemCountCmd);

    Form.Multiline:= AMultiline;
    Form.InitItemIndex:= AInitIndex;
    Form.ShowModal;
    Result:= Form.ResultCode;
  finally
    Form.Free;
  end;
end;

function TfmMain.DoDialogConfColors(var AColors: TAppTheme): boolean;
begin
  with TfmColorSetup.Create(nil) do
  try
    OnApply:= @FormColorsApply;
    Data:= AColors;
    Result:= ShowModal=mrOk;
    if Result then AColors:= Data;
  finally
    Free
  end;
end;

procedure TfmMain.SplitterOnPaint_Gr(Sender: TObject);
var
  Sp: TSplitter;
begin
  Sp:= Sender as TSplitter;
  Sp.Canvas.Brush.Color:= GetAppColor('SplitGroups');
  Sp.Canvas.FillRect(Sp.ClientRect);
end;

procedure TfmMain.SplitterOnPaint_Main(Sender: TObject);
var
  Sp: TSplitter;
begin
  Sp:= Sender as TSplitter;
  Sp.Canvas.Brush.Color:= GetAppColor('SplitMain');
  Sp.Canvas.FillRect(Sp.ClientRect);
end;


procedure TfmMain.GetEditorIndexes(Ed: TATSynEdit;
  out AGroupIndex, ATabIndex: Integer);
begin
  Groups.PagesAndTabIndexOfControl(GetEditorFrame(Ed), AGroupIndex, ATabIndex);
  Dec(AGroupIndex); //was 1-based
end;

procedure TfmMain.DoHelpWiki;
begin
  OpenURL('http://wiki.freepascal.org/CudaText');
end;

procedure TfmMain.ListboxOutKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Prop: ^TAppPanelProps;
  List: TATListbox;
begin
  //Esc
  if (Key=VK_ESCAPE) then
  begin
    if CurrentEditor.CanSetFocus then
      CurrentEditor.SetFocus;
    Key:= 0;
    exit
  end;

  List:= Sender as TATListbox;
  if Sender=ListboxOut then
    Prop:= @AppPanelProp_Out
  else
    Prop:= @AppPanelProp_Val;

  if not ((List.ItemIndex>=0) and
          (List.ItemIndex<Prop^.Items.Count)) then exit;

  //Ctrl+C
  if (Key=Ord('C')) and (Shift=[ssCtrl]) then
  begin
    Clipboard.AsText:= Prop^.Items.Text;
    Key:= 0;
    exit
  end;

  //Ctrl+D
  if (Key=Ord('D')) and (Shift=[ssCtrl]) then
  begin
    Clipboard.AsText:= Prop^.Items[List.ItemIndex];
    Key:= 0;
    exit
  end;

  //Ctrl+Del
  if Key=VK_DELETE then
  begin
    ////don't enable Del for Output panel
    //if Shift=[] then
    //  Prop^.Items.Delete(List.ItemIndex);

    if Shift=[ssCtrl] then
      Prop^.Items.Clear;

    List.ItemCount:= Prop^.Items.Count;
    if List.ItemCount=0 then
      List.ItemIndex:= -1
    else
    if List.ItemIndex>=List.ItemCount then
      List.ItemIndex:= List.ItemCount-1;

    List.Invalidate;
  end;
end;

procedure TfmMain.MenuThemesClick(Sender: TObject);
var
  fn: string;
begin
  fn:= FListThemes[(Sender as TComponent).Tag];
  FThemeName:= ExtractFileNameOnly(fn);
  DoLoadTheme(fn, Theme);
  DoApplyTheme;
end;

procedure TfmMain.DoHelpLexers;
begin
  DoFileOpen(GetAppPath(cFileReadmeLexerInst));
end;

procedure TfmMain.mnuOpKeysClick(Sender: TObject);
begin
  MsgBox('To customize hotkeys, call Commands dialog, focus any command, and press F9, you''ll see additional dialog', mb_ok);
end;

procedure TfmMain.mnuTabColorClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  with TfmPalette.Create(Self) do
  try
    ResColor:= F.TabColor;
    case ShowModal of
      mrOk: F.TabColor:= ResColor;
      mrNo: F.TabColor:= clNone;
    end;
  finally
    Free
  end;
end;

procedure TfmMain.MenuThemeDefClick(Sender: TObject);
begin
  FThemeName:= '';
  DoInitTheme(Theme);
  DoApplyTheme;
end;

procedure TfmMain.mnuTabsize1Click(Sender: TObject);
begin
  UpdateEditorTabsize(1);
end;

procedure TfmMain.mnuTabsize2Click(Sender: TObject);
begin
  UpdateEditorTabsize(2);
end;

procedure TfmMain.mnuTabsize4Click(Sender: TObject);
begin
  UpdateEditorTabsize(4);
end;

procedure TfmMain.mnuTabsize8Click(Sender: TObject);
begin
  UpdateEditorTabsize(8);
end;

procedure DoParseOutputLine(const AProp: TAppPanelProps;
  const AStr: string;
  out AFilename: string;
  out ALine, ACol: integer);
var
  Parts: TRegexParts;
begin
  AFilename:= AProp.DefFilename;
  ALine:= -1;
  ACol:= 0;

  if AProp.RegexStr='' then exit;
  if AProp.RegexIdLine=0 then exit;

  if not SRegexFindParts(AProp.RegexStr, AStr, Parts) then exit;
  if AProp.RegexIdName>0 then
    AFilename:= Parts[AProp.RegexIdName];
  if AProp.RegexIdLine>0 then
    ALine:= StrToIntDef(Parts[AProp.RegexIdLine], -1);
  if AProp.RegexIdCol>0 then
    ACol:= StrToIntDef(Parts[AProp.RegexIdCol], 0);

  if not AProp.ZeroBase then
  begin
    if ALine>0 then Dec(ALine);
    if ACol>0 then Dec(ACol);
  end;
end;

procedure TfmMain.ListboxOutClick(Sender: TObject);
var
  Prop: ^TAppPanelProps;
  ResFilename: string;
  ResLine, ResCol: integer;
  NIndex, NTag: integer;
  SText: string;
begin
  if Sender=ListboxOut then
    Prop:= @AppPanelProp_Out
  else
    Prop:= @AppPanelProp_Val;

  NIndex:= Prop^.Listbox.ItemIndex;
  if NIndex<0 then exit;
  if NIndex>=Prop^.Items.Count then exit;

  SText:= Prop^.Items[NIndex];
  NTag:= PtrInt(Prop^.Items.Objects[NIndex]);

  DoParseOutputLine(Prop^, SText, ResFilename, ResLine, ResCol);
  if (ResFilename<>'') and (ResLine>=0) then
  begin
    MsgStatus(Format('file "%s", line %d, col %d', [ResFilename, ResLine, ResCol]));
    if FileExists(ResFilename) then
    begin
      DoFileOpen(ResFilename);
      CurrentFrame.Editor.DoCaretSingle(ResCol, ResLine);
      CurrentFrame.Editor.DoGotoCaret(cEdgeTop);
      CurrentFrame.Editor.Update;
      UpdateStatus;
    end;
  end
  else
  begin
    MsgStatus('Clicking log line');
    DoPyEvent(CurrentEditor, cEventOnOutputNav,
      [SStringToPythonString(SText), IntToStr(NTag)] );
  end;
end;


procedure TfmMain.ListboxOutDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
const
  cDx=4; cDy=1;
var
  Prop: PAppPanelProps;
  ResFilename: string;
  ResLine, ResCol: integer;
begin
  Prop:= GetAppPanelProps_ByListbox(Sender as TATListbox);
  if Prop=nil then exit;

  DoParseOutputLine(Prop^, Prop^.Items[AIndex], ResFilename, ResLine, ResCol);
  if (ResFilename<>'') and (ResLine>=0) then
  begin
    C.Font.Color:= GetAppColor('ListFontHotkey');
    C.Brush.Color:= GetAppColor('ListBg');
  end
  else
  begin
    C.Font.Color:= GetAppColor('ListFont');
    C.Brush.Color:= GetAppColor('ListBg');
  end;

  if AIndex=Prop^.Listbox.ItemIndex then
  begin
    C.Brush.Color:= GetAppColor('ListSelBg');
    C.FillRect(ARect);
  end;

  C.TextOut(ARect.Left+cDx, ARect.Top+cDy, Prop^.Items[AIndex]);
end;


procedure TfmMain.DoGotoDefinition;
begin
  if DoPyEvent(CurrentEditor, cEventOnGotoDef, [])<>cPyTrue then
    MsgStatus('No goto-definition plugins installed for this lexer');
end;

procedure TfmMain.DoShowFuncHint;
var
  S: string;
begin
  S:= DoPyEvent(CurrentEditor, cEventOnFuncHint, []);
  if (S='') or (S='None') then exit;

  MsgStatusAlt(S, UiOps.StatusAltTime);
end;

procedure TfmMain.PopupTextPopup(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  UpKey(mnuTextUndo, cCommand_Undo);
  UpKey(mnuTextRedo, cCommand_Redo);
  UpKey(mnuTextCut, cCommand_ClipboardCut);
  UpKey(mnuTextCopy, cCommand_ClipboardCopy);
  UpKey(mnuTextPaste, cCommand_ClipboardPaste);
  UpKey(mnuTextDelete, cCommand_TextDeleteSelection);
  UpKey(mnuTextSel, cCommand_SelectAll);
  UpKey(mnuTextGotoDef, cmd_GotoDefinition);

  Ed:= CurrentEditor;
  if assigned(mnuTextCut) then mnuTextCut.Enabled:= not Ed.ModeReadOnly;
  if assigned(mnuTextPaste) then mnuTextPaste.Enabled:= not Ed.ModeReadOnly and Clipboard.HasFormat(CF_Text);
  if assigned(mnuTextDelete) then mnuTextDelete.Enabled:= not Ed.ModeReadOnly and Ed.Carets.IsSelection;
  if assigned(mnuTextUndo) then mnuTextUndo.Enabled:= not Ed.ModeReadOnly and (Ed.UndoCount>0);
  if assigned(mnuTextRedo) then mnuTextRedo.Enabled:= not Ed.ModeReadOnly and (Ed.RedoCount>0);
end;


procedure TfmMain.DoDialogLoadLexerStyles;
var
  Form: TfmLexerStylesRestore;
  An: TecSyntAnalyzer;
  i: integer;
begin
  Form:= TfmLexerStylesRestore.Create(nil);
  try
    Form.StylesFilename:= GetAppPath(cFileLexerStyles);
    if Form.ShowModal=mrOk then
    begin
      for i:= 0 to Form.List.Count-1 do
        if Form.List.Checked[i] then
        begin
          An:= Manager.FindAnalyzer(Form.List.Items[i]);
          if Assigned(An) then
            LoadLexerStylesFromFile(An, Form.StylesFilename)
          else
            MsgBox('Cannot find lexer in library: '+Form.List.Items[i], MB_OK);
        end;

      DoOps_SaveLexlib(false);
      UpdateFrame;
    end;
  finally
    FreeAndNil(Form);
  end;
end;


procedure TfmMain.DoPyUpdateEvents(const AModuleName, AEventStr, ALexerStr, AKeyStr: string);
var
  i, N: integer;
begin
  //find index of plugin (get first empty index if not listed)
  N:= -1;
  for i:= Low(FPluginsEvents) to High(FPluginsEvents) do
    with FPluginsEvents[i] do
      if (ItemModule=AModuleName) or (ItemModule='') then
        begin N:= i; Break end;
  if N<0 then Exit;

  //update record
  with FPluginsEvents[N] do
  begin
    if ItemModule='' then
      ItemModule:= AModuleName;
    DoPyStringToEvents(AEventStr, ItemEvents);
    ItemLexers:= ALexerStr;
    ItemKeys:= AKeyStr;
  end;
end;



procedure TfmMain.CharmapOnInsert(const AStr: string);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  Str: atString;
  Shift, PosAfter: TPoint;
begin
  Ed:= CurrentEditor;
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];
  Str:= Utf8Decode(AStr);

  Ed.Strings.TextInsert(Caret.PosX, Caret.PosY, Str,
    Ed.ModeOverwrite, Shift, PosAfter);
  Ed.DoCaretSingle(Caret.PosX+Length(Str), Caret.PosY);

  Ed.Strings.Modified:= true;
  Ed.DoEventChange;

  UpdateFrame(true);
  UpdateStatus;
end;


procedure TfmMain.DoDialogCharMap;
begin
  if fmCharmaps=nil then
  begin
    fmCharmaps:= TfmCharmaps.Create(nil);
    fmCharmaps.OnInsert:= @CharmapOnInsert;
  end;

  fmCharmaps.InitialStr:= Utf8Encode(Widestring(EditorGetCurrentChar(CurrentEditor)));
  fmCharmaps.Show;
end;

function TfmMain.DoOnConsole(const Str: string): boolean;
begin
  Result:= DoPyEvent(CurrentEditor, cEventOnConsole,
    [SStringToPythonString(Str)]) <> cPyFalse;
end;

function TfmMain.DoOnConsoleNav(const Str: string): boolean;
begin
  Result:= DoPyEvent(CurrentEditor, cEventOnConsoleNav,
    [SStringToPythonString(Str)]) <> cPyFalse;
end;

function TfmMain.DoOnMacro(const Str: string): boolean;
begin
  Result:= DoPyEvent(CurrentEditor, cEventOnMacro,
    [SStringToPythonString(Str)]) <> cPyFalse;
end;

procedure TfmMain.DoPyCommand(const AModule, AMethod: string; const AParam: string='');
begin
  PyLastCommandModule:= AModule;
  PyLastCommandMethod:= AMethod;
  PyLastCommandParam:= AParam;

  with CurrentFrame do
    if MacroRecord then
      MacroString:= MacroString+ ('py:'+AModule+','+AMethod+','+AParam+#10);

  CurrentEditor.Strings.BeginUndoGroup;
  PyCommandRunning:= true;
  try
    Py_RunPlugin_Command(AModule, AMethod, AParam);
  finally
    PyCommandRunning:= false;
    CurrentEditor.Strings.EndUndoGroup;
  end;
end;


function TfmMain.DoPyPanelAdd(AParams: string): boolean;
var
  SCaption: string;
  Listbox: TATListbox;
  Props: TAppPanelPropsClass;
begin
  Result:= false;
  SCaption:= SGetItem(AParams, ';');

  if (SCaption='Console') or
     (SCaption='Output') or
     (SCaption='Validate') then exit;
  if FPanelCaptions.IndexOf(SCaption)>=0 then exit;

  Listbox:= TATListbox.Create(Self);
  Listbox.Hide;
  Listbox.Parent:= PanelBottom;
  Listbox.Align:= alClient;
  Listbox.OnClick:= @ListboxOutClick;
  Listbox.OnDrawItem:= @ListboxOutDrawItem;
  Listbox.OnKeyDown:= @ListboxOutKeyDown;
  Listbox.Color:= GetAppColor('ListBg');
  Listbox.ItemHeight:= ListboxOut.ItemHeight;
  Listbox.CanGetFocus:= true;

  Props:= TAppPanelPropsClass.Create;
  Props.Data.Listbox:= Listbox;
  Props.Data.Items:= TStringList.Create;

  FPanelCaptions.AddObject(SCaption, Props);
  TabsBottom.AddTab(-1, SCaption, nil);
  Result:= true;
end;


function TfmMain.DoPyPanelDelete(const ACaption: string): boolean;
var
  PropObject: TAppPanelPropsClass;
  Data: TATTabData;
  N: integer;
begin
  Result:= false;

  N:= FPanelCaptions.IndexOf(ACaption);
  if N<0 then exit;
  PropObject:= fmMain.FPanelCaptions.Objects[N] as TAppPanelPropsClass;
  PropObject.Data.Listbox.Free;
  PropObject.Data.Items.Free;
  PropObject.Free;
  FPanelCaptions.Delete(N);

  for N:= TabsBottom.TabCount-1 downto 0 do
  begin
    Data:= TabsBottom.GetTabData(N);
    if Assigned(Data) and (Data.TabCaption=ACaption) then
    begin
      TabsBottom.DeleteTab(N, false, false);
      break
    end;
  end;

  Result:= true;
end;


function TfmMain.DoPyPanelFocus(const ACaption: string): boolean;
var
  Data: TATTabData;
  i: integer;
begin
  Result:= false;
  for i:= 0 to TabsBottom.TabCount-1 do
  begin
    Data:= TabsBottom.GetTabData(i);
    if Assigned(Data) and (Data.TabCaption=ACaption) then
    begin
      TabsBottom.TabIndex:= i;
      exit(true);
    end;
  end;
end;


procedure TfmMain.DoGetSplitInfo(const Id: string;
  out BoolVert, BoolVisible: boolean; out NPos, NTotal: integer);
  //----
  procedure GetSp(Sp: TSplitter);
  begin
    BoolVert:= (Sp.Align=alLeft) or (Sp.Align=alRight);
    BoolVisible:= Sp.Visible;
    NPos:= Sp.GetSplitterPosition;
    if BoolVert then NTotal:= Sp.Parent.Width else NTotal:= Sp.Parent.Height;
  end;
  //----
begin
  BoolVert:= false;
  BoolVisible:= true;
  NPos:= 0;
  NTotal:= 0;

  if Id='L' then GetSp(SplitterVert) else
  if Id='B' then GetSp(SplitterHorz) else
  if Id='G1' then GetSp(Groups.Splitter1) else
  if Id='G2' then GetSp(Groups.Splitter2) else
  if Id='G3' then GetSp(Groups.Splitter3) else
  ;
end;


procedure TfmMain.DoSetSplitInfo(const Id: string; NPos: integer);
  procedure SetSp(Sp: TSplitter);
  begin
    Sp.SetSplitterPosition(NPos);
    if Assigned(Sp.OnMoved) then
      Sp.OnMoved(Self);
  end;
begin
  if NPos<0 then exit;
  if Id='L' then SetSp(SplitterVert) else
  if Id='B' then SetSp(SplitterHorz) else
  if Id='G1' then SetSp(Groups.Splitter1) else
  if Id='G2' then SetSp(Groups.Splitter2) else
  if Id='G3' then SetSp(Groups.Splitter3) else
  ;
end;

procedure TfmMain.FrameLexerChange(Sender: TObject);
begin
  DoPyEvent(CurrentEditor, cEventOnLexer, []);
end;

procedure TfmMain.DoToolbarAddButtom(AStr: string);
var
  SHint, SIcon, SCmd: string;
  btn: TToolButton;
begin
  if AStr='' then
  begin
    btn:= TToolButton.Create(Self);
    btn.Parent:= ToolbarMain;
    btn.Left:= ToolbarMain.ClientWidth;
    btn.Style:= tbsDivider;
    exit
  end;

  SHint:= SGetItem(AStr, ';');
  SIcon:= SGetItem(AStr, ';');
  SCmd:= SGetItem(AStr, ';');

  btn:= TToolButton.Create(Self);
  btn.Hint:= SHint;
  btn.Caption:= SCmd;
  btn.OnClick:= @DoToolbarClick;
  if UpdateImagelistWithIconFromFile(ImageListBar, SIcon) then
    btn.ImageIndex:= ImageListBar.Count-1;
  btn.Parent:= ToolbarMain;
  btn.Left:= ToolbarMain.ClientWidth;
end;


procedure TfmMain.DoToolbarClick(Sender: TObject);
var
  SHint, SModule, SMethod, SParam: string;
  NCmd: integer;
begin
  //'module,method,param' or 'NN'
  SHint:= (Sender as TToolButton).Caption;
  NCmd:= StrToIntDef(SHint, 0);

  if NCmd=0 then
  begin
    SModule:= SGetItem(SHint);
    SMethod:= SGetItem(SHint);
    SParam:= SHint; //not SGetItem, allows to use ","
    DoPyCommand(SModule, SMethod, SParam);
  end
  else
    CurrentEditor.DoCommand(NCmd);

  UpdateFrame;
  UpdateStatus;
end;


//----------------------------
{$I formmain_loadsave.inc}
{$I formmain_updates_proc.inc}
{$I formmain_frame_proc.inc}
{$I formmain_tab_proc.inc}
{$I formmain_find.inc}
{$I formmain_cmd.inc}
{$I formmain_editing.inc}
{$I formmain_plugins.inc}


end.

