(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit FormMain;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ScopedEnums on}

{$IFDEF DEBUG}
{$INLINE OFF}
{$ENDIF}
//{$define debug_on_lexer}

interface

uses
  {$ifdef windows}
  Windows,
  win32menustyler,
  win32titlestyler,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus,
  Clipbrd, StrUtils, Variants, IniFiles,
  LclType, LclProc, LclIntf,
  InterfaceBase,
  LazFileUtils, LazUTF8, FileUtil,
  syncobjs,
  EncConv,
  TreeFilterEdit,
  {$ifdef LCLGTK2}
  fix_gtk_clipboard,
  {$endif}
  fix_focus_window,
  at__jsonconf, at__fpjson, proc_json_ex,
  PythonEngine,
  ec_LexerList,
  ec_SyntAnal,
  ec_syntax_format,
  ATButtons,
  ATFlatToolbar,
  ATFlatThemes,
  ATListbox,
  ATScrollBar,
  ATPanelSimple,
  ATCanvasPrimitives,
  ATSynEdit,
  ATSynEdit_Globals,
  ATSynEdit_Keymap,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Commands,
  ATSynEdit_Finder,
  ATSynEdit_Carets,
  ATSynEdit_Bookmarks,
  ATSynEdit_Markers,
  ATSynEdit_WrapInfo,
  ATSynEdit_Ranges,
  ATSynEdit_DimRanges,
  ATSynEdit_Gaps,
  ATSynEdit_Hotspots,
  ATSynEdit_Gutter_Decor,
  ATSynEdit_LineParts,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Adapter_LiteLexer,
  ATSynEdit_Export_HTML,
  ATSynEdit_Edits,
  ATSynEdit_Cmp_Form,
  ATSynEdit_Cmp_AcpFiles,
  ATSynEdit_Cmp_CSS,
  ATSynEdit_Cmp_HTML,
  ATSynEdit_Cmp_FileURI,
  ATTabs,
  ATGroups,
  ATStatusBar,
  ATStrings,
  ATStringProc,
  ATStringProc_Separator,
  ATGauge,
  ATBinHex,
  proc_inittick,
  proc_str,
  proc_py,
  proc_py_const,
  proc_appvariant,
  proc_files,
  proc_globdata,
  proc_panelhost,
  proc_colors,
  proc_cmd,
  proc_editor,
  proc_miscutils,
  proc_keybackupclass,
  proc_msg,
  proc_install_zip,
  proc_lexer_styles,
  proc_keysdialog,
  proc_customdialog,
  proc_customdialog_dummy,
  proc_scrollbars,
  proc_cssprovider,
  formconsole,
  formframe,
  formgoto,
  formfind,
  formsavetabs,
  formconfirmrep,
  formlexerprop,
  formlexerlib,
  formlexerstylemap,
  formcolorsetup,
  formabout,
  formcharmaps,
  formkeyinput,
  formconfirmbinary,
  form_menu_commands,
  form_menu_list,
  form_menu_py,
  form_addon_report,
  form_choose_theme,
  form_unprinted,
  Math;

type
  { TAppNotifThread }

  TAppNotifThread = class(TThread)
  private
    CurFrame: TEditorFrame;
    procedure HandleOneFrame;
    procedure NotifyFrame1;
    procedure NotifyFrame2;
    //procedure ModifyFrame1;
  protected
    procedure Execute; override;
  end;

var
  AppNotifThread: TAppNotifThread = nil;

type

  { TAppFormWithEditor }

  TAppFormWithEditor = class(TFormDummy)
  public
    Ed: TATSynEdit;
    Popup: TPopupMenu;
    RegexStr: string;
    RegexIdLine,
    RegexIdCol,
    RegexIdName: integer;
    DefFilename: string;
    ZeroBase: boolean;
    Encoding: string;
    procedure Clear;
    procedure Add(const AText: string);
    function IsIndexValid(AIndex: integer): boolean;
  end;

type
  TAppFinderMarking = (
    None,
    Selections,
    Markers,
    Bookmarks
    );

  TAppMenuItemsAlt = record
    item0: TMenuItem;
    item1: TMenuItem;
    active0: boolean;
  end;

  TDlgMenuProps = record
    ItemsText: string;
    Caption: string;
    InitialIndex: integer;
    Multiline: boolean;
    NoFuzzy: boolean;
    NoFullFilter: boolean;
    ShowCentered: boolean;
    UseEditorFont: boolean;
    Collapse: TATCollapseStringMode;
    W, H: integer;
  end;

  TDlgCommandsProps = record
    Caption: string;
    LexerName: string;
    ShowUsual,
    ShowPlugins,
    ShowLexers,
    ShowFiles,
    ShowRecents,
    AllowConfig,
    AllowConfigForLexer,
    ShowCentered: boolean;
    FocusedCommand: integer;
    W, H: integer;
  end;

  { TAppCompletionApiProps }

  TAppCompletionApiProps = record
    Editor: TATSynEdit;
    Text: string;
    CharsLeft: integer;
    CharsRight: integer;
    CaretPos: TPoint;
    procedure Clear;
  end;

  { TFrameEditState }

  TFrameEditState = record
    Ed1_FileName: string;
    Ed2_FileName: string;
    Ed1_ModifiedVersion: Int64;
    Ed2_ModifiedVersion: Int64;
    procedure Assign(AFrame: TEditorFrame);
  end;

operator =(constref a, b: TFrameEditState): boolean;

const
  cMenuTabsizeMin = 1;
  cMenuTabsizeMax = 10;

type
  TAppAllowSomething = (
    Enable,
    Disable,
    NotGood
    );

  TAppConfigHistoryElement = (
    RecentFiles,
    Search,
    Console
    );
  TAppConfigHistoryElements = set of TAppConfigHistoryElement;

  TAppTooltipPos = (
    WindowTop,
    WindowBottom,
    EditorCaret,
    CustomTextPos
    );

type
  { TfmMain }
  TfmMain = class(TForm)
    AppProps: TApplicationProperties;
    mnuFile: TMenuItem;
    mnuFileReopen: TMenuItem;
    mnuFileSaveAll: TMenuItem;
    mnuFileCloseAll: TMenuItem;
    mnuFileCloseOther: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuFileNewMenu: TMenuItem;
    mnuFileSep1: TMenuItem;
    mnuFileSep2: TMenuItem;
    mnuFileSep3: TMenuItem;
    mnuFileSep4: TMenuItem;
    mnuFileEnds: TMenuItem;
    mnuFileEnc: TMenuItem;
    mnuFileCloseDel: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFileOpenDir: TMenuItem;
    mnuFileOpenSub: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuFileClose: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditPasteHist: TMenuItem;
    mnuEditPasteIndent: TMenuItem;
    mnuEditSpToTab: TMenuItem;
    mnuEditTabToSp: TMenuItem;
    mnuEditCharmap: TMenuItem;
    mnuEditTrimL: TMenuItem;
    mnuEditTrimR: TMenuItem;
    mnuEditTrim: TMenuItem;
    mnuEditCopyLine: TMenuItem;
    mnuEditCopyAppend: TMenuItem;
    mnuEditCopyFFull: TMenuItem;
    mnuEditCopyFName: TMenuItem;
    mnuEditCopyFDir: TMenuItem;
    mnuEditCopySub: TMenuItem;
    mnuEditLineOp: TMenuItem;
    mnuEditLineMoveUp: TMenuItem;
    mnuEditLineMoveDown: TMenuItem;
    mnuEditLineDel: TMenuItem;
    mnuEditLineDup: TMenuItem;
    mnuEditIndent: TMenuItem;
    mnuEditUnindent: TMenuItem;
    mnuEditIndentSub: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuEditRedo: TMenuItem;
    mnuEditSep1: TMenuItem;
    mnuEditSep2: TMenuItem;
    mnuEditSep4: TMenuItem;
    mnuEditSep5: TMenuItem;
    mnuEditSep6: TMenuItem;
    mnuEditSep7: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuCaseSent: TMenuItem;
    mnuCaseLow: TMenuItem;
    mnuCaseUp: TMenuItem;
    mnuCaseTitle: TMenuItem;
    mnuCaseInvert: TMenuItem;
    mnuCaseSub: TMenuItem;
    mnuSel: TMenuItem;
    mnuSelExtLine: TMenuItem;
    mnuSelInvert: TMenuItem;
    mnuSelSplit: TMenuItem;
    mnuSelExtWord: TMenuItem;
    mnuSelAll: TMenuItem;
    mnuSelSep1: TMenuItem;
    mnuSelSep2: TMenuItem;
    mnuCaretsUpBegin: TMenuItem;
    mnuCaretsDown1Line: TMenuItem;
    mnuCaretsDown1Page: TMenuItem;
    mnuCaretsDownEnd: TMenuItem;
    mnuCaretsCancel: TMenuItem;
    mnuCaretsUp1Line: TMenuItem;
    mnuCaretsExtSub: TMenuItem;
    mnuCaretsSep1: TMenuItem;
    mnuCaretsUp1Page: TMenuItem;
    mnuFind: TMenuItem;
    mnuFindNext: TMenuItem;
    mnuFindPrev: TMenuItem;
    mnuFindDlg: TMenuItem;
    mnuFindRepDialog: TMenuItem;
    mnuFindWordNext: TMenuItem;
    mnuFindWordPrev: TMenuItem;
    mnuFindSep1: TMenuItem;
    mnuFindSep2: TMenuItem;
    mnuGotoLine: TMenuItem;
    mnuGotoBm: TMenuItem;
    mnuBmPlaceOnCarets: TMenuItem;
    mnuBmSep1: TMenuItem;
    mnuBmSep2: TMenuItem;
    mnuBmDeleteLines: TMenuItem;
    mnuBmCopyLines: TMenuItem;
    mnuBmPlaceCarets: TMenuItem;
    mnuBmSub: TMenuItem;
    mnuBmToggle: TMenuItem;
    mnuBmInvert: TMenuItem;
    mnuBmClear: TMenuItem;
    mnuBmPrev: TMenuItem;
    mnuBmNext: TMenuItem;
    mnuView: TMenuItem;
    mnuViewSplitNo: TMenuItem;
    mnuViewSplitV: TMenuItem;
    mnuViewSplitH: TMenuItem;
    mnuViewHint2: TMenuItem;
    mnuViewHint1: TMenuItem;
    mnuViewSidebar: TMenuItem;
    mnuViewOnTop: TMenuItem;
    mnuViewFloatSide: TMenuItem;
    mnuViewFloatBottom: TMenuItem;
    mnuViewMicromap: TMenuItem;
    mnuViewDistFree: TMenuItem;
    mnuViewSide: TMenuItem;
    mnuViewStatus: TMenuItem;
    mnuViewFullscr: TMenuItem;
    mnuViewToolbar: TMenuItem;
    mnuViewNums: TMenuItem;
    mnuViewRuler: TMenuItem;
    mnuViewFold: TMenuItem;
    mnuViewWrap: TMenuItem;
    mnuViewMinimap: TMenuItem;
    mnuViewSplitSub: TMenuItem;
    mnuViewBottom: TMenuItem;
    mnuViewSep1: TMenuItem;
    mnuViewSep2: TMenuItem;
    mnuLexers: TMenuItem;
    mnuPlugins: TMenuItem;
    mnuOp: TMenuItem;
    mnuOpUser: TMenuItem;
    mnuOpDefaultUser: TMenuItem;
    mnuOpPlugins: TMenuItem;
    mnuOpThemeUi: TMenuItem;
    mnuOpThemeSyntax: TMenuItem;
    mnuOpThemes: TMenuItem;
    mnuOpUnprinted: TMenuItem;
    mnuOpLangs: TMenuItem;
    mnuOpLexMap: TMenuItem;
    mnuOpSep1: TMenuItem;
    mnuOpLexLib: TMenuItem;
    mnuOpLexSub: TMenuItem;
    mnuOpLexProp: TMenuItem;
    mnuOpSep2: TMenuItem;
    mnuOpDefault: TMenuItem;
    mnuOpLexer: TMenuItem;
    mnuFontOutput: TMenuItem;
    mnuFontText: TMenuItem;
    mnuFontUi: TMenuItem;
    mnuFontSub: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpCmd: TMenuItem;
    mnuHelpCheckUpd: TMenuItem;
    mnuHelpIssues: TMenuItem;
    mnuHelpSep1: TMenuItem;
    mnuHelpSep2: TMenuItem;
    mnuHelpWiki: TMenuItem;
    mnuHelpForum: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuGroups: TMenuItem;
    mnuGr1: TMenuItem;
    mnuGr1p2V: TMenuItem;
    mnuGr1p2H: TMenuItem;
    mnuGr2V: TMenuItem;
    mnuGr2H: TMenuItem;
    mnuGr3V: TMenuItem;
    mnuGr3H: TMenuItem;
    mnuGr4G: TMenuItem;
    mnuGr4V: TMenuItem;
    mnuGr4H: TMenuItem;
    mnuGr6: TMenuItem;
    mnuGr6H: TMenuItem;
    mnuGr6V: TMenuItem;
    mnuTextOpenUrl: TMenuItem;
    mnuTextUndo: TMenuItem;
    mnuTextRedo: TMenuItem;
    mnuTextSep1: TMenuItem;
    mnuTextCut: TMenuItem;
    mnuTextCopy: TMenuItem;
    mnuTextPaste: TMenuItem;
    mnuTextDelete: TMenuItem;
    mnuTextSep2: TMenuItem;
    mnuTextSel: TMenuItem;
    mnuTextGotoDef: TMenuItem;
    TimerMouseStop: TTimer;
    TimerStatusWork: TTimer;
    TimerAppIdle: TIdleTimer;
    ImageListTabs: TImageList;
    ImageListSide: TImageList;
    ImageListBm: TImageList;
    MainMenu: TMainMenu;
    PanelAll: TATPanelSimple;
    PanelMain: TATPanelSimple;
    PanelEditors: TATPanelSimple;
    PanelSide: TATPanelSimple;
    PopupText: TPopupMenu;
    PopupRecents: TPopupMenu;
    TimerTooltip: TTimer;
    TimerTreeFill: TTimer;
    TimerCmd: TTimer;
    TimerStatusClear: TTimer;
    ToolbarMain: TATFlatToolbar;
    ToolbarSideMid: TATFlatToolbar;
    ToolbarSideLow: TATFlatToolbar;
    ToolbarSideTop: TATFlatToolbar;
    procedure AppPropsActivate(Sender: TObject);
    procedure AppPropsDeactivate(Sender: TObject);
    procedure AppPropsDropFiles(Sender: TObject;
      const FileNames: array of string);
    procedure AppPropsEndSession(Sender: TObject);
    procedure AppPropsModalBegin(Sender: TObject);
    procedure AppPropsQueryEndSession(var Cancel: Boolean);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var ACanClose: boolean);
    procedure FormColorsApply(const AColors: TAppTheme; AThemeUI: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormWindowStateChange(Sender: TObject);
    procedure FrameAddRecent(Sender: TObject);
    procedure FrameOnMsgStatus(Sender: TObject; const AStr: string);
    procedure FrameOnEditorChangeCaretPos(Sender: TObject);
    procedure FrameOnEditorScroll(Sender: TObject);
    procedure FrameOnEditorPaint(Sender: TObject);
    procedure FrameOnInitAdapter(Sender: TObject);
    procedure FrameOnChangeSlow(Sender: TObject);
    procedure FrameParseDone(Sender: TObject);
    procedure EditorOutput_OnClickDbl(Sender: TObject; var AHandled: boolean);
    procedure EditorOutput_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuTabColorClick(Sender: TObject);
    procedure mnuTabPinnedClick(Sender: TObject);
    procedure mnuTabCopyDirClick(Sender: TObject);
    procedure mnuTabCopyFullPathClick(Sender: TObject);
    procedure mnuTabCopyNameClick(Sender: TObject);
    procedure mnuTabMoveF2Click(Sender: TObject);
    procedure mnuTabMoveF3Click(Sender: TObject);
    procedure DoHelpAbout;
    procedure DoHelpForum;
    procedure DoHelpWiki;
    procedure DoHelpIssues;

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
    procedure mnuTabMoveF1Click(Sender: TObject);
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
    procedure mnuTreeSortedClick(Sender: TObject);
    procedure mnuTreeUnfoldAllClick(Sender: TObject);
    procedure PopupRecentsPopup(Sender: TObject);
    procedure PopupTabPopup(Sender: TObject);
    procedure PopupTextPopup(Sender: TObject);
    procedure StatusPanelClick(Sender: TObject; AIndex: Integer);
    procedure TimerAppIdleTimer(Sender: TObject);
    procedure TimerCmdTimer(Sender: TObject);
    procedure TimerMouseStopTimer(Sender: TObject);
    procedure TimerTooltipTimer(Sender: TObject);
    procedure TimerStatusWorkTimer(Sender: TObject);
    procedure TimerStatusClearTimer(Sender: TObject);
    procedure TimerTreeFillTimer(Sender: TObject);
    procedure HandleOtherInstance(Sender: TObject; AParamCount: Integer;
      const AParameters: array of String);
    {$ifdef windows}
    procedure SecondInstance(const Msg: TBytes);
    {$endif}
  private
    { private declarations }
    SaveDlg: TSaveDialog;
    ImageListTree: TImageList;
    PopupTab: TPopupMenu;
    PopupTree: TPopupMenu;
    PopupTabSize: TPopupMenu;
    PopupViewerMode: TPopupMenu;
    PopupPicScale: TPopupMenu;
    PopupStatusbarMsg: TPopupMenu;
    mnuTabCloseAllAll: TMenuItem;
    mnuTabCloseAllSame: TMenuItem;
    mnuTabCloseLeft: TMenuItem;
    mnuTabCloseOtherAll: TMenuItem;
    mnuTabCloseOtherSame: TMenuItem;
    mnuTabCloseRight: TMenuItem;
    mnuTabCloseSub: TMenuItem;
    mnuTabCloseThis: TMenuItem;
    mnuTabColor: TMenuItem;
    mnuTabPinned: TMenuItem;
    mnuTabCopyDir: TMenuItem;
    mnuTabCopyFullPath: TMenuItem;
    mnuTabCopyName: TMenuItem;
    mnuTabCopySub: TMenuItem;
    mnuTabMove1: TMenuItem;
    mnuTabMove2: TMenuItem;
    mnuTabMove3: TMenuItem;
    mnuTabMove4: TMenuItem;
    mnuTabMove5: TMenuItem;
    mnuTabMove6: TMenuItem;
    mnuTabMoveF1: TMenuItem;
    mnuTabMoveF2: TMenuItem;
    mnuTabMoveF3: TMenuItem;
    mnuTabMoveNext: TMenuItem;
    mnuTabMovePrev: TMenuItem;
    mnuTabMoveSub: TMenuItem;
    mnuTabSave: TMenuItem;
    mnuTabSaveAs: TMenuItem;
    mnuTreeFoldAll: TMenuItem;
    mnuTreeUnfoldAll: TMenuItem;
    mnuTreeFoldLevel: TMenuItem;
    mnuTreeFold2: TMenuItem;
    mnuTreeFold3: TMenuItem;
    mnuTreeFold4: TMenuItem;
    mnuTreeFold5: TMenuItem;
    mnuTreeFold6: TMenuItem;
    mnuTreeFold7: TMenuItem;
    mnuTreeFold8: TMenuItem;
    mnuTreeFold9: TMenuItem;
    mnuTreeSorted: TMenuItem;
    mnuTabsizeSpace: TMenuItem;
    mnuTabsizeConvTabs: TMenuItem;
    mnuTabsizeConvSpaces: TMenuItem;
    mnuTabsizesValue: array[cMenuTabsizeMin..cMenuTabsizeMax] of TMenuItem;
    mnuToolbarCaseLow: TMenuItem;
    mnuToolbarCaseUp: TMenuItem;
    mnuToolbarCaseTitle: TMenuItem;
    mnuToolbarCaseInvert: TMenuItem;
    mnuToolbarCaseSent: TMenuItem;
    mnuToolbarCommentLineAdd: TMenuItem;
    mnuToolbarCommentLineDel: TMenuItem;
    mnuToolbarCommentLineToggle: TMenuItem;
    mnuToolbarCommentStream: TMenuItem;
    PopupToolbarCase: TPopupMenu;
    PopupToolbarComment: TPopupMenu;
    PopupSidebarClone: TPopupMenu;
    PaintTest: TPaintBox;
    FFormFloatGroups1: TForm;
    FFormFloatGroups2: TForm;
    FFormFloatGroups3: TForm;
    FBoundsMain: TRect;
    FBoundsFloatGroups1: TRect;
    FBoundsFloatGroups2: TRect;
    FBoundsFloatGroups3: TRect;
    FConsoleMustShow: boolean;
    FColorDialog: TColorDialog;
    Status: TATStatus;
    Groups: TATGroups;
    GroupsCtx: TATGroups;
    GroupsCtxIndex: integer;
    GroupsF1: TATGroups;
    GroupsF2: TATGroups;
    GroupsF3: TATGroups;
    FFormTooltip: TForm;
    FTooltipPanel: TAppPanelEx;

    mnuApple: TMenuItem;
    mnuApple_About: TMenuItem;
    //mnuApple_Quit: TMenuItem;

    mnuViewWrap_Alt,
    mnuViewNums_Alt,
    mnuViewFold_Alt,
    mnuViewRuler_Alt,
    mnuViewMinimap_Alt,
    mnuViewMicromap_Alt,
    mnuViewUnpriShow_Alt,
    mnuViewUnpriSpaces_Alt,
    mnuViewUnpriSpacesTail_Alt,
    mnuViewUnpriEnds_Alt,
    mnuViewUnpriEndsDet_Alt,
    mnuViewSplitNo_Alt,
    mnuViewSplitV_Alt,
    mnuViewSplitH_Alt,
    mnuViewToolbar_Alt,
    mnuViewStatus_Alt,
    mnuViewFullscr_Alt,
    mnuViewDistFree_Alt,
    mnuViewSidebar_Alt,
    mnuViewSide_Alt,
    mnuViewBottom_Alt,
    mnuViewFloatSide_Alt,
    mnuViewFloatBottom_Alt,
    mnuViewOnTop_Alt,
    mnuGr1_Alt,
    mnuGr2H_Alt,
    mnuGr2V_Alt,
    mnuGr3H_Alt,
    mnuGr3V_Alt,
    mnuGr1p2V_Alt,
    mnuGr1p2H_Alt,
    mnuGr4H_Alt,
    mnuGr4V_Alt,
    mnuGr4G_Alt,
    mnuGr6H_Alt,
    mnuGr6V_Alt,
    mnuGr6_Alt: TAppMenuItemsAlt;

    FFinder: TATEditorFinder;
    FFindStop: boolean;
    FFindConfirmAll: TModalResult;
    FFindMarkingMode: TAppFinderMarking;
    FFindMarkingCaret1st: boolean;
    FShowFullScreen: boolean;
    FShowFullScreen_DisFree: boolean;
    FOrigBounds: TRect;
    FOrigWndState: TWindowState;
    FOrigShowToolbar: boolean;
    FOrigShowBottom: boolean;
    FOrigShowStatusbar: boolean;
    FOrigShowSidePanel: boolean;
    FOrigShowSideBar: boolean;
    FOrigShowTabs: boolean;
    FHandledUntilFirstFocus: boolean;
    FHandledOnShowPartly: boolean;
    FHandledOnShowFully: boolean;
    FHandledOnStart2: boolean;
    FHandledMakeCaretVisible: boolean;
    FCodetreeBuffer: TTreeView;
    FCfmPanel: TPanel;
    FCfmLink: string;
    FMenuVisible: boolean;
    FNewClickedEditor: TATSynEdit;
    FPyCompletionProps: TAppCompletionApiProps;
    FNeedUpdateStatuses: boolean;
    FNeedUpdateMenuPlugins: boolean;
    FNeedUpdateMenuChecks: boolean;
    FNeedUpdateMenuShortcuts: boolean;
    FNeedUpdateMenuShortcuts_Force: boolean;
    FNeedAppState_SubCommands: boolean;
    FNeedAppState_MenuAdd: boolean;
    FNeedAppState_MenuRemove: boolean;
    FNeedAppState_MenuChange: boolean;
    FLastDirOfOpenDlg: string;
    FLastLexerForPluginsMenu: string;
    FLastStatusbarMessage: string;
    FLastSelectedCommand: integer;
    FLastMousePos: TPoint;
    FLastMaximized: boolean;
    FLastMaximizedMonitor: integer;
    FLastFocusedFrame: TComponent;
    FLastTooltipLine: integer;
    FLastAppActivate: QWord;
    FLastSaveSessionTick: QWord;
    FLastLoadedConfig: string;
    FLastLoadedEditorOps: TEditorOps;
    FDisableTreeClearing: boolean;
    FLexerProgressIndex: integer;
    FOption_WindowPos: string;
    FOption_AllowSessionLoad: TAppAllowSomething;
    FOption_AllowSessionSave: TAppAllowSomething;
    FOption_StartupCommand: string;
    FOption_GroupMode: TATGroupsMode;
    FOption_GroupSizes: TATGroupsPoints;
    FOption_GroupPanelSize: TPoint;
    FOption_SidebarTab: string;
    FOption_BottomTab: string;
    FCmdlineFileCount: integer;
    FPrevJsonObj: TJSONData;
    FPrevFramesEditState: array of TFrameEditState;

    function CodeTreeFilter_OnFilterNode(ItemNode: TTreeNode; out Done: Boolean): Boolean;
    function ConfirmAllFramesAreSaved(AWithCancel: boolean): boolean;
    procedure FindAndStop(ABack: boolean);
    procedure FindAndReplaceAll(var NCounter: integer);
    procedure FindAndReplaceOneMatch(AndStop: boolean);
    procedure FindAndCountAll(var NCounter: integer);
    procedure FindAndSelectAll(var NCounter: integer);
    procedure FindAndMarkAll(var NCounter: integer);
    procedure FindAndReplaceInAllFrames(FramePrev: TEditorFrame; var NCounter: integer);
    procedure FindAndExtractRegexMatches;
    procedure DoFocusUsualGroup(AIndex: integer);
    procedure DoFocusFloatingGroup(AIndex: integer);
    procedure DoFocusNextGroup(ANext: boolean);
    function GetFileOpenOptionsString(AFileCount: integer): string;
    procedure HandleTimerCommand(Ed: TATSynEdit; CmdCode: integer; CmdInvoke: TATCommandInvoke);
    procedure InvalidateMouseoverDependantControls;
    function IsTooManyTabsOpened: boolean;
    function GetUntitledNumberedCaption: string;
    procedure PopupBottomOnPopup(Sender: TObject);
    procedure PopupBottomClearClick(Sender: TObject);
    procedure PopupBottomCopyClick(Sender: TObject);
    procedure PopupBottomSelectAllClick(Sender: TObject);
    procedure PopupBottomWrapClick(Sender: TObject);
    procedure ConfirmButtonOkClick(Sender: TObject);
    procedure ConfirmPanelMouseLeave(Sender: TObject);
    procedure FindDialogFocusEditor(Sender: TObject);
    procedure FindDialogGetMainEditor(out AEditor: TATSynEdit);
    procedure FrameConfirmLink(Sender: TObject; const ALink: string);
    procedure FormEnter(Sender: TObject);
    function GetShowDistractionFree: boolean;
    procedure PythonEngineAfterInit(Sender: TObject);
    procedure PythonIOSendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PythonModuleInitialization(Sender: TObject);
    procedure CodeTreeFilter_OnChange(Sender: TObject);
    procedure CodeTreeFilter_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CodeTreeFilter_ResetOnClick(Sender: TObject);
    procedure CodeTreeFilter_OnCommand(Sender: TObject; ACmd: integer; AInvoke: TATCommandInvoke;
      const AText: string; var AHandled: boolean);
    procedure DisablePluginMenuItems(AddFindLibraryItem: boolean);
    procedure DoShowForVisibleFrames;
    procedure DoLocalizeTabTitles;
    procedure DoApplyCli(const ACliModule: string; const ACliParams: TAppStringArray);
    procedure DoApplyNewdocLexer(F: TEditorFrame);
    procedure DoApplyLexerStylesMapsToFrames(AndApplyTheme: boolean);
    procedure DoApplyTranslationToGroups(G: TATGroups);
    procedure DoClearSingleFirstTab;
    function DoCloseAllTabs(AClosePinned: boolean): boolean;
    procedure DoCloseAllTabs_Old;
    procedure DoDialogUnprinted(Ed: TATSynEdit);
    procedure DoDialogMenuThemes_ThemeSetter(const AThemeUi, AThemeSyntax: string);
    procedure DoFileDialog_PrepareDir(Dlg: TFileDialog);
    procedure DoFileDialog_SaveDir(Dlg: TFileDialog);
    procedure DoCommands_OnMsg(Sender: TObject; const ARes: string);
    procedure DoFindFirst;
    procedure DoFindNext(ANext: boolean);
    procedure DoFindMarkAll(AMode: TAppFinderMarking);
    procedure DoFindMarkingInit(AMode: TAppFinderMarking);
    procedure DoFindOptions_OnChange(Sender: TObject);
    procedure DoFindOptions_ApplyDict(const AText: string);
    function DoFindOptions_GetDict: PPyObject;
    procedure DoFindActionFromString(const AStr: string);
    procedure DoFindCurrentWordOrSel(Ed: TATSynEdit; ANext, AWordOrSel: boolean);
    procedure DoFolderOpen(const ADirName: string; ANewProject: boolean; AInvoke: TATCommandInvoke);
    procedure DoFolderAdd(AInvoke: TATCommandInvoke);
    procedure DoGetSaveDialog(var ASaveDlg: TSaveDialog);
    procedure DoGroupsChangeMode(Sender: TObject);
    procedure DoOnLexerParseProgress(Sender: TObject; AProgress: integer);
    //procedure DoOnLexerParseProgress(Sender: TObject; ALineIndex, ALineCount: integer);
    procedure DoOnLexerParseProgress_Sync();
    procedure DoOps_AddPluginMenuItem(const ACaption: string; ASubMenu: TMenuItem;
      ALangFile: TIniFile; ACommandListIndex: integer);
    procedure DoOps_LexersBackupSave;
    procedure DoOps_LexersBackupRestore;
    procedure DoOps_LoadOptions_Editor(cfg: TJSONConfig; var Op: TEditorOps);
    procedure DoOps_LoadOptions_Global(cfg: TJSONConfig);
    procedure DoOps_LoadOptions_Ui(cfg: TJSONConfig);
    procedure ShowWelcomeInfo;
    procedure DoOps_OnCreate;
    function FindFrameOfFilename(const AFileName: string; AllowEmptyPath: boolean=false): TEditorFrame;
    function FindFrameOfPreviewTab: TEditorFrame;
    procedure FixMainLayout;
    procedure FormFloatGroups1_OnEmpty(Sender: TObject);
    procedure FormFloatGroups2_OnEmpty(Sender: TObject);
    procedure FormFloatGroups3_OnEmpty(Sender: TObject);
    procedure FormFloatGroups1_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups2_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups3_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormFloatGroups_OnUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormFloatGroups_OnDropFiles(Sender: TObject; const FileNames: array of String);
    procedure CharmapOnInsert(const AStr: string);
    procedure DoLocalize;
    procedure DoLocalizePopupTab;
    function DoCheckFilenameOpened(const AName: string): boolean;
    procedure DoInvalidateEditors;
    function DoMenuAdd_Params(const AMenuId, AMenuCmd, AMenuCaption, AMenuHotkey, AMenuTagString: string; AIndex: integer): string;
    procedure DoMenu_Remove(const AMenuId: string);
    procedure DoMenuClear(const AMenuId: string);
    function DoMenu_GetPyProps(mi: TMenuItem): PPyObject;
    function DoMenu_PyEnum(const AMenuId: string): PPyObject;
    procedure DoOnTabFocus(Sender: TObject);
    procedure DoOnTabFocusFinalization(F: TEditorFrame;
      AAllowEventOnTabChange: boolean);
    procedure DoOnTabAdd(Sender: TObject);
    procedure DoOnTabClose(Sender: TObject; ATabIndex: Integer; var ACanClose, ACanContinue: boolean);
    procedure DoOnTabMove(Sender: TObject; NFrom, NTo: Integer);
    procedure DoOnTabPopup(Sender: TObject; APages: TATPages; ATabIndex: integer);
    procedure DoOnTabDblClick(Sender: TObject; AIndex: integer);
    function DoOnTabGetTick(Sender: TObject; ATabObject: TObject): Int64;
    procedure DoCodetree_UpdateVersion(Ed: TATSynEdit);
    procedure DoCodetree_Clear;
    procedure DoCodetree_PanelOnEnter(Sender: TObject);
    procedure DoCodetree_StopUpdate;
    procedure DoCodetree_OnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DoCodetree_GetSyntaxRange(ANode: TTreeNode; out APosBegin, APosEnd: TPoint);
    procedure DoCodetree_SetSyntaxRange(ANode: TTreeNode; const APosBegin, APosEnd: TPoint);
    procedure DoCodetree_OnClick(Sender: TObject);
    procedure DoCodetree_OnDblClick(Sender: TObject);
    procedure DoCodetree_OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoCodetree_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoCodetree_GotoBlockForCurrentNode(AndSelect: boolean);
    procedure DoCodetree_ApplyTreeHelperResults(Tree: TTreeView; Data: PPyObject);
    function DoCodetree_ApplyTreeHelperInPascal(Ed, EdPair: TATSynEdit; ATree: TTreeView; const ALexer: string): boolean;
    procedure DoCodetree_OnAdvDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure DoSidebar_OnContextPopup(const ACaption: string);
    procedure DoSidebar_OnCloseFloatForm(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoSidebar_OnBeforeToggle(Sender: TObject);
    procedure DoSidebar_OnAfterToggle(Sender: TObject);
    function DoSidebar_GetFormTitle(const ACaption: string): string;
    procedure DoSidebar_OnPythonCall(const ACallback: string);
    procedure DoSidebar_OnShowCodeTree(Sender: TObject);
    function DoSidebar_FilenameToImageIndex(ATabCaption, AFilename: string): integer;
    procedure DoSidebar_ListboxDrawItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure DoSidebar_MainMenuClick(Sender: TObject);
    procedure DoSidebar_FocusCodetreeFilter;
    procedure DoSidebar_FocusCodetree;
    procedure DoBottom_OnBeforeToggle(Sender: TObject);
    procedure DoBottom_OnAfterToggle(Sender: TObject);
    procedure DoBottom_OnCloseFloatForm(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoBottom_FindClick(Sender: TObject);
    function DoAutoComplete_FromPlugins(Ed: TATSynEdit): boolean;
    function DoAutoComplete_PosOnBadToken(Ed: TATSynEdit; AX, AY: integer): boolean;
    procedure DoAutoComplete_Callback(Ed: TATSynEdit; AActivate: boolean);
    procedure DoAutoComplete(Ed: TATSynEdit);
    procedure DoPyCommand_CommandLineParam(const AModuleAndMethod: string);
    procedure DoPyCommand_Cudaxlib(Ed: TATSynEdit; const AMethod: string; AInvoke: TATCommandInvoke);
    procedure DoDialogCharMap;
    procedure DoGotoFromInput(Frame: TEditorFrame; const AInput: string);
    procedure DoGotoDefinition(Ed: TATSynEdit);
    procedure DoShowFuncHint(Ed: TATSynEdit);
    procedure DoApplyGutterVisible(AValue: boolean);
    procedure DoApplyFrameOps(F: TEditorFrame; const Op: TEditorOps; AForceApply: boolean);
    procedure DoApplyFont_Text;
    procedure DoApplyFont_Ui;
    procedure DoApplyFont_UiStatusbar;
    procedure DoApplyFont_Output;
    procedure DoApplyAllOps;
    procedure DoApplyTheme;
    procedure DoApplyTheme_ThemedMainMenu;
    procedure DoApplyThemeToGroups(G: TATGroups);
    procedure DoOnConsoleNumberChange(Sender: TObject);
    procedure DoOnConsoleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function DoDialogConfigTheme(AThemeUI: boolean): boolean;
    function DoDialogMenuApi(const AProps: TDlgMenuProps): integer;
    procedure DoDialogMenuTranslations;
    procedure DoDialogMenuThemes;
    procedure DoDialogMenuEncodings;
    procedure DoDialogMenuEnds;
    procedure DoFileExportHtml(Ed: TATSynEdit);
    function DoFileInstallZip(const AFileName: string; out DirTarget: string;
      ASilent, AAllowUpdateAddons: boolean): boolean;
    procedure DoFileCloseAndDelete(Ed: TATSynEdit);
    procedure DoFileNew;
    procedure DoFileNewMenu_ToolbarClick(Sender: TObject);
    procedure DoFileNewMenu(Sender: TObject; AInvoke: TATCommandInvoke);
    procedure DoFileNewFrom(const fn: string);
    procedure DoFileSave(Frame: TEditorFrame; Ed: TATSynEdit);
    procedure DoFileSaveAs(Frame: TEditorFrame; Ed: TATSynEdit);
    procedure DoFocusFrame(F: TEditorFrame);
    procedure DoFocusEditor(Ed: TATSynEdit);
    procedure DoSwitchTab(ANext: boolean);
    procedure DoSwitchTabSimply(ANext: boolean);
    procedure DoSwitchTabToRecent;
    procedure DoPyTimerTick(Sender: TObject);
    procedure DoPyRunLastPlugin(AInvoke: TATCommandInvoke);
    procedure DoPyResetPlugins;
    procedure DoPyRescanPlugins;
    function DoSplitter_StringToId(const AStr: string): integer;
    procedure DoSplitter_GetInfo(const Id: integer; out BoolVert, BoolVisible: boolean; out NPos, NTotal: integer);
    procedure DoSplitter_SetInfo(const Id: integer; NPos: integer);
    procedure DoToolbarClick(Sender: TObject);
    procedure FrameLexerChange(Sender: TATSynEdit);
    function GetFloatGroups: boolean;
    function GetShowFloatGroup1: boolean;
    function GetShowFloatGroup2: boolean;
    function GetShowFloatGroup3: boolean;
    function GetShowOnTop: boolean;
    function GetShowSidebarOnRight: boolean;
    procedure InitStatusProgress;
    procedure InitButtonCancel;
    procedure InitAppleMenu;
    procedure InitImageListCodetree;
    procedure InitPaintTest;
    procedure InitPopupTree;
    procedure InitPopupPicScale;
    procedure InitPopupBottom(var AMenu: TPopupMenu; AEditor: TATSynEdit);
    procedure InitPopupViewerMode;
    procedure InitPopupTab;
    procedure InitPopupTabSize;
    procedure InitBottomEditor(var Form: TAppFormWithEditor);
    procedure InitFloatGroup(var F: TForm; var G: TATGroups; AIndexOfGroup: integer;
      const ARect: TRect; AOnClose: TCloseEvent; AOnGroupEmpty: TNotifyEvent);
    procedure InitFloatGroups;
    procedure InitSaveDlg;
    procedure InitSidebar;
    procedure InitToolbar;
    procedure InitCodeTree;
    function IsWindowMaximizedOrFullscreen: boolean;
    function IsAllowedToOpenFileNow: boolean;
    function IsThemeNameExist(const AName: string; AThemeUI: boolean): boolean;
    procedure PopupToolbarCaseOnPopup(Sender: TObject);
    procedure PopupToolbarCommentOnPopup(Sender: TObject);
    procedure MenuRecent_RemoveFilename(const fn: string);
    procedure MenuRecentsClear(Sender: TObject);
    procedure MenuRecentsPopup(Sender: TObject);
    procedure MenuRecentItemClick(Sender: TObject);
    procedure MenuitemClick_CommandFromTag(Sender: TObject);
    procedure MenuitemClick_CommandFromHint(Sender: TObject);
    procedure MenuPicScaleClick(Sender: TObject);
    procedure MenuTabsizeClick(Sender: TObject);
    procedure MenuViewerModeClick(Sender: TObject);
    procedure MenuLexerClick(Sender: TObject);
    procedure MenuMainClick(Sender: TObject);
    procedure MsgStatus(AText: string; AFinderMessage: boolean=false);
    procedure MsgStatusErrorInRegex;
    procedure MsgStatusFileOpened(const AFileName1, AFileName2: string);
    function GetStatusbarPrefix(Frame: TEditorFrame): string;
    procedure SearcherDirectoryEnter(FileIterator: TFileIterator);
    procedure SetShowFloatGroup1(AValue: boolean);
    procedure SetShowFloatGroup2(AValue: boolean);
    procedure SetShowFloatGroup3(AValue: boolean);
    procedure SetShowMenu(AValue: boolean);
    procedure SetShowOnTop(AValue: boolean);
    procedure SetShowSidebarOnRight(AValue: boolean);
    procedure SetSidebarPanel(const ACaption: string);
    procedure SetShowDistractionFree(AValue: boolean);
    procedure SetShowDistractionFree_Forced;
    procedure SetShowFullScreen(AValue: boolean);
    procedure SetFullScreen_Ex(AValue: boolean; AHideAll: boolean);
    procedure SetFullScreen_Universal(AValue: boolean);
    procedure SetFullScreen_Win32(AValue: boolean);
    procedure DoOps_ShowEventPlugins;
    procedure DoOps_LoadPluginFromInf(const fn_inf: string; IniPlugins: TMemIniFile);
    procedure DoOps_LoadSidebarIcons;
    procedure DoOps_LoadCodetreeIcons;
    procedure DoOps_LoadToolbarIcons;
    procedure DoOps_LoadLexerLib(AOnCreate: boolean);
    procedure DoOps_SaveHistory(ASaveModifiedTabs: boolean);
    procedure DoOps_ClearConfigHistory(AMode: TAppConfigHistoryElements);
    procedure DoOps_SaveHistory_GroupView(cfg: TJsonConfig; const AJsonPath: string);
    procedure DoOps_SaveOptionBool(const APath: string; AValue: boolean);
    procedure DoOps_SaveOptionString(const APath, AValue: string);
    procedure DoOps_SaveThemes;
    procedure DoOps_LoadHistory;
    procedure DoOps_LoadHistory_GroupView(cfg: TJsonConfig; const AJsonPath: string);
    function DoOps_SaveSession(const ASessionId: string; ASaveModifiedFiles, ASaveUntitledTabs, AByTimer: boolean): boolean;
    function DoOps_LoadSession(const ASessionId: string; AllowShowPanels: boolean): boolean;
    procedure DoOps_SaveSessionsBackups(const ASessionFilename: string);
    procedure DoOps_LoadOptionsAndApplyAll;
    procedure DoOps_LoadOptionsLexerSpecific(F: TEditorFrame; Ed: TATSynEdit);
    procedure DoOps_OpenFile_LexerSpecific;
    procedure DoOps_LoadPlugins(AKeepHotkeys: boolean);
    procedure DoOps_DialogFont(var OpName: string; var OpSize: integer; const AConfigStrName, AConfigStrSize: string);
    procedure DoOps_DialogFont_Text;
    procedure DoOps_DialogFont_Ui;
    procedure DoOps_DialogFont_Output;
    procedure DoOps_FontSizeChange(AIncrement: integer);
    procedure DoOps_FontSizeReset;
    procedure DoOps_OpenFile_Default;
    procedure DoOps_OpenFile_User;
    procedure DoOps_OpenFile_DefaultAndUser;
    procedure DoOps_LoadOptions(const AFileName: string; var Ops: TEditorOps; AllowGlobalOps: boolean);
    procedure DoOps_LoadOptionsFromString(const AString: string);
    procedure DoOps_FindPythonLib(Sender: TObject);
    procedure DoDialogCommands;
    function DoDialogCommands_Custom(Ed: TATSynEdit; const AProps: TDlgCommandsProps): integer;
    function DoDialogCommands_Py(var AProps: TDlgCommandsProps): string;
    procedure DoDialogGoto;
    function DoDialogMenuList(const ACaption: string; AItems: TStringList; AInitItemIndex: integer;
      ACloseOnCtrlRelease: boolean= false; AOnListSelect: TAppListSelectEvent=nil): integer;
    procedure DoDialogMenuTabSwitcher;
    function DoDialogMenuLexerChoose(const AFilename: string; ANames: TStringList): integer;
    procedure DoDialogGotoBookmark;
    function DoDialogSaveTabs: boolean;
    procedure DoDialogLexerProp(an: TecSyntAnalyzer);
    procedure DoDialogLexerLib;
    procedure DoDialogLexerMap;
    procedure DoDialogTheme(AThemeUI: boolean);
    procedure DoDialogLexerMenu;
    procedure DoShowConsole(AndFocus: boolean);
    procedure DoShowOutput(AndFocus: boolean);
    procedure DoShowValidate(AndFocus: boolean);
    function FrameOfPopup: TEditorFrame;
    procedure FrameOnEditorCommand(Sender: TObject; ACommand: integer; AInvoke: TATCommandInvoke;
      const AText: string; var AHandled: boolean);
    function DoFileCloseAll(AWithCancel, AClosePinned: boolean): boolean;
    procedure DoDialogFind(AReplaceMode: boolean);
    procedure DoDialogFind_Hide;
    procedure DoDialogFind_Toggle(AReplaceMode, AAndFocus: boolean);
    procedure FinderFormChangeVisible(Sender: TObject);
    procedure FinderShowResult(ok, AIsReplace: boolean; AFinder: TATEditorFinder);
    procedure FinderShowResultSimple(ok: boolean; AFinder: TATEditorFinder);
    procedure FinderShowMatchesCount(AMatchCount, ATime: integer);
    function FinderHandleKeyDown(AKey: word; AShiftState: TShiftState): boolean;
    procedure DoMoveTabToGroup(AGroupIndex: Integer; AFromCommandPalette: boolean=false);
    function DoFileOpen(AFileName, AFileName2: string; APages: TATPages=nil; const AOptions: string=''): TEditorFrame;
    procedure DoFileOpenDialog(const AOptions: string='');
    procedure DoFileOpenDialog_NoPlugins;
    function DoFileSaveAll: boolean;
    procedure DoFileReopen(F: TEditorFrame; Ed: TATSynEdit);
    procedure DoFileReopenRecent;
    procedure DoLoadCommandParams(const AParams: array of string; AOpenOptions: string);
    procedure DoLoadCommandLine;
    procedure DoLoadCommandLine_FromString(const AText: string);
    //procedure DoToggleMenu;
    procedure DoToggleFloatSide;
    procedure DoToggleFloatBottom;
    procedure DoToggleOnTop;
    procedure DoToggleFullScreen;
    procedure DoToggleDistractionFree;
    procedure DoToggleSidePanel;
    procedure DoToggleBottomPanel;
    procedure DoToggleSidebar;
    procedure DoToggleToolbar;
    procedure DoToggleStatusbar;
    procedure DoToggleUiTabs;
    procedure FinderGetHiAllIndexes(AFinder: TATEditorFinder; out AIndex, ACount: integer);
    function FinderGetHiAllIndexesString(AFinder: TATEditorFinder): string;
    function FinderOptionsToHint(AFinder: TATEditorFinder; AIsReplace: boolean): string;
    function FinderReplaceAll(Ed: TATSynEdit; AResetCaret: boolean): integer;
    procedure FinderShowReplaceReport(ACounter, ATime: integer);
    procedure FindDialogDone(Sender: TObject; Res: TAppFinderOperation; AEnableUpdateAll: boolean);
    procedure FindDialogDone2(Sender: TObject; Res: TAppFinderOperation);
    procedure FinderOnFound(Sender: TObject; APos1, APos2: TPoint);
    procedure FinderOnProgress(Sender: TObject; const ACurPos, AMaxPos: Int64; var AContinue: boolean);
    procedure FinderOnWrapAtEdge(Sender: TObject);
    procedure FinderUpdateEditor(AUpdateText: boolean; AUpdateStatusbar: boolean=true);
    procedure FrameOnSaveFile(Sender: TObject; const fn: string);
    procedure GetEditorIndexes(Ed: TATSynEdit; out AGroupIndex, ATabIndex: Integer);
    function GetModifiedCount: integer;
    function GetShowSideBar: boolean;
    function GetShowStatus: boolean;
    function GetShowToolbar: boolean;
    function GetShowTabsMain: boolean;
    procedure InitFormFind;
    function IsFocusedBottom: boolean;
    function IsFocusedFind: boolean;
    procedure TimerConsoleCompletionTick(Sender: TObject);
    procedure PyCompletionOnGetProp(Sender: TObject; AContent: TStringList; out ACharsLeft, ACharsRight: integer);
    procedure PyCompletionOnResult(Sender: TObject; const ASnippetId: string; ASnippetIndex: integer);
    procedure DoPyCommand_ByCommandInfo(CmdItem: TAppCommandInfo; AInvoke: TATCommandInvoke);
    procedure SetFrameEncoding(Ed: TATSynEdit; const AEnc: string; AAlsoReloadFile: boolean);
    procedure SetFrameLexerByIndex(Ed: TATSynEdit; AIndex: integer);
    procedure SetShowStatus(AValue: boolean);
    procedure SetShowToolbar(AValue: boolean);
    procedure SetShowSideBar(AValue: boolean);
    procedure SetShowTabsMain(AValue: boolean);
    procedure SplitterOnPaintDummy(Sender: TObject);
    procedure StopAllTimers;
    procedure UpdateGroupsOfContextMenu;
    procedure UpdatePlugins_AfterInstallingZip;
    procedure UpdateEditorShowCaret;
    procedure UpdateFindDialogFromSuggestions;
    procedure UpdateFindDialogParent;
    procedure UpdateFindDialogOnTabFocusing(F: TEditorFrame);
    procedure UpdateFindDialogEnabled(Frame: TEditorFrame);
    procedure UpdateGlobalProgressbar(AValue: integer; AVisible: boolean; AMaxValue: integer=100);
    procedure UpdateLexerProgressbar(AValue: integer; AVisible: boolean; AMaxValue: integer=100);
    procedure UpdateGroupsMode(AMode: TATGroupsMode);
    procedure UpdateMenuTheming(AMenu: TPopupMenu);
    procedure UpdateMenuTheming_MainMenu;
    procedure UpdateMenuTheming_WhiteLine;
    procedure UpdateMenuRecents(sub: TMenuItem);
    procedure UpdateMenuSidebarButton(AWhenAutoShow: boolean);
    procedure UpdateSidebarButtonOverlay;
    procedure UpdateEditorTabsize(AValue: integer);
    procedure UpdateMenuItemAltObject(mi: TMenuItem; ACmd: integer);
    procedure UpdateMenuItemChecked(mi: TMenuItem; saved: TAppMenuItemsAlt; AValue: boolean);
    procedure UpdateMenuItemHint(mi: TMenuItem; const AHint: string);
    procedure UpdateMenuItemHotkey(mi: TMenuItem; ACmd: integer; AllowSetShortcut: boolean=true);
    procedure UpdateMenuItem_SetShortcutFromProps(mi: TMenuItem);
    procedure UpdateMenuItem_SetShortcutsRecursively(AMenuItem: TMenuItem; AMaxMenuLevel: integer);
    procedure UpdateMenuLexersTo(AMenu: TMenuItem);
    procedure UpdateMenuRecent(Ed: TATSynEdit);
    procedure UpdateMenuHotkeys;
    procedure UpdateMenuPlugins;
    procedure UpdateMenuPlugins_Shortcuts(AForceUpdate: boolean=false);
    procedure UpdateMenuPlugins_Shortcuts_Work(AForceUpdate: boolean);
    procedure UpdateMenuChecks_Frame(F: TEditorFrame);
    procedure UpdateMenuChecks_FrameSplit(F: TEditorFrame);
    procedure UpdateMenuChecks_Global;
    procedure UpdateFrameLineEnds(Frame: TEditorFrame; AValue: TATLineEnds);
    procedure UpdateEditorCaretLineEnds(Frame: TEditorFrame; Ed: TATSynEdit; AValue: TATLineEnds);
    procedure UpdateStatusbarPanelsFromString(const AText: string);
    procedure UpdateStatusbarHints;
    procedure UpdateStatusbar_ForFrame(AStatus: TATStatus; F: TEditorFrame);
    procedure UpdateStatusbar_RealWork;
    procedure UpdateToolbarButtons(F: TEditorFrame);
    procedure UpdateToolbarButton(AToolbar: TATFlatToolbar; ACmd: integer; AChecked, AEnabled: boolean);
    procedure UpdateSidebarButtonFind;
    procedure UpdateTabCaptionsFromFolders;
    procedure UpdateTabsActiveColor(F: TEditorFrame);
    procedure UpdateTree(AFill: boolean; AConsiderTreeVisible: boolean=true);
    procedure UpdateTreeByTimer;
    procedure UpdateTreeSelection(Ed: TATSynEdit);
    procedure UpdateTreeImagelistActivity;
    procedure UpdateCaption;
    procedure UpdateCaption_RealWork;
    procedure UpdateEnabledAll(b: boolean);
    procedure UpdateInputForm(Form: TForm; AndHeight: boolean= true);
    procedure UpdateFrameEx(F: TEditorFrame; AUpdatedText: boolean);
    procedure UpdateCurrentFrame(AUpdatedText: boolean= false);
    procedure UpdateAppForSearch(AStart, AEdLock, AFindMode, AUpdateEnableAll, AFocusFindDlg: boolean);
    procedure UpdateStatusbar;
    procedure UpdateTreeFilter;

    procedure DoApplyUiOps;
    procedure DoApplyUiOpsToGroups(G: TATGroups);
    procedure DoApplyInitialGroupSizes;
    procedure DoApplyInitialSidebarPanel;
    procedure DoApplyInitialWindowPos;
    procedure InitConfirmPanel;
    procedure InitPyEngine;
    procedure InitFrameEvents(F: TEditorFrame);
    procedure InitStatusbar;
    procedure InitGroups;
    procedure InitFinder;
    procedure InitBookmarkSetup;
    procedure FrameOnChangeCaption(Sender: TObject);
    procedure FrameOnUpdateStatusbar(Sender: TObject; AReason: TAppStatusbarUpdateReason);
    procedure FrameOnUpdateState(Sender: TObject);
    function CreateTab(APages: TATPages; const ACaption: string;
      AndActivate: boolean=true;
      AAllowNearCurrent: TAppNewTabNearCurrent=TAppNewTabNearCurrent.ByOption): TATTabData;
    procedure FrameOnEditorFocus(Sender: TObject);
    function GetFrame(AIndex: integer): TEditorFrame;
    procedure SetFrame(Frame: TEditorFrame);
    procedure DoTooltipHide;
    procedure DoTooltipShow(const AText: string; ASeconds: integer;
      APosition: TAppTooltipPos; AGotoBracket: boolean; APosX, APosY: integer);
    procedure DoOnDeleteLexer(Sender: TObject; const ALexerName: string);
  public
    { public declarations }
    CodeTree: TAppTreeContainer;
    CodeTreeFilter: TTreeFilterEdit;
    CodeTreeFilterInput: TATComboEdit;
    PanelCodeTreeAll: TFormDummy;
    PanelCodeTreeTop: TPanel;
    StatusProgress: TATGauge;
    ButtonCancel: TATButton;
    TimerConsoleCmp: TTimer;
    TimerFinderWrapped: TTimer;
    function FrameCount: integer;
    property Frames[N: integer]: TEditorFrame read GetFrame;
    function CurrentGroups: TATGroups;
    function CurrentFrame: TEditorFrame;
    function CurrentEditor: TATSynEdit;
    property FloatGroups: boolean read GetFloatGroups;
    property ShowFloatGroup1: boolean read GetShowFloatGroup1 write SetShowFloatGroup1;
    property ShowFloatGroup2: boolean read GetShowFloatGroup2 write SetShowFloatGroup2;
    property ShowFloatGroup3: boolean read GetShowFloatGroup3 write SetShowFloatGroup3;
    property ShowMenu: boolean read FMenuVisible write SetShowMenu;
    property ShowOnTop: boolean read GetShowOnTop write SetShowOnTop;
    property ShowFullscreen: boolean read FShowFullScreen write SetShowFullScreen;
    property ShowDistractionFree: boolean read GetShowDistractionFree write SetShowDistractionFree;
    property ShowSideBar: boolean read GetShowSideBar write SetShowSideBar;
    property ShowSideBarOnRight: boolean read GetShowSidebarOnRight write SetShowSidebarOnRight;
    property ShowToolbar: boolean read GetShowToolbar write SetShowToolbar;
    property ShowStatus: boolean read GetShowStatus write SetShowStatus;
    property ShowTabsMain: boolean read GetShowTabsMain write SetShowTabsMain;
    function DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: TAppVariantArray): TAppPyEventResult;
    function DoPyEvent_ConsoleNav(const AText: string): boolean;
    procedure DoPyEvent_ConsoleComplete(Sender: TObject);
    function DoPyEvent_Message(const AText: string): boolean;
    procedure DoPyEvent_AppState(AState: integer);
    procedure DoPyEvent_EdState(Ed: TATSynEdit; AState: integer);
    procedure DoPyEvent_AppActivate(AEvent: TAppPyEvent);
    procedure DoPyEvent_Open(Ed: TATSynEdit);
    procedure DoPyEvent_OpenNone(Ed: TATSynEdit);
    procedure DoPyCommand(const AModule, AMethod: string; const AParams: TAppVariantArray; AInvoke: TATCommandInvoke);
    function RunTreeHelper(Frame: TEditorFrame; ATree: TTreeView;
      AllowPascalHelpers, AllowPythonHelpers: boolean): boolean;
    function DoPyLexerDetection(const Filename: string; Lexers: TStringList): integer;
    procedure FinderOnGetToken(Sender: TObject; AX, AY: integer; out AKind: TATTokenKind);
    procedure FinderOnConfirmReplace(Sender: TObject; APos1, APos2: TPoint;
      AForMany: boolean; var AConfirm, AContinue: boolean; var AReplacement: UnicodeString);
    procedure FinderOnConfirmReplace_API(Sender: TObject; APos1, APos2: TPoint;
      AForMany: boolean; var AConfirm, AContinue: boolean; var AReplacement: UnicodeString);
    procedure PyStatusbarPanelClick(Sender: TObject; const ATag: Int64);
    procedure UpdateThemes(const AThemeUi, AThemeSyntax: string);
  end;

var
  fmMain: TfmMain;

var
  fmOutput: TAppFormWithEditor = nil;
  fmValidate: TAppFormWithEditor = nil;

implementation

uses
  Emmet,
  EmmetHelper,
  TreeHelpers_Base,
  TreeHelpers_Proc,
  ATSynEdit_ClipRecents,
  ATStringProc_HtmlColor;

{$R *.lfm}

var
  PythonEng: TPythonEngine = nil;
  PythonModule: TPythonModule = nil;
  PythonIO: TPythonInputOutput = nil;

type
  TAppCommandGetStatus = (
    NoCommands,
    BadCommand,
    OkCommand
    );

const
  cThreadSleepTime = 50;
  cThreadSleepCount = 20;
  //SleepTime*SleepCount ~= 1 sec

const
  StatusbarTag_Caret = 10;
  StatusbarTag_Enc = 11;
  StatusbarTag_LineEnds = 12;
  StatusbarTag_Lexer = 13;
  StatusbarTag_TabSize = 14;
  StatusbarTag_InsOvr = 15;
  StatusbarTag_SelMode = 16;
  StatusbarTag_WrapMode = 17;
  StatusbarTag_Zoom = 18;
  StatusbarTag_Msg = 20;

function GetAppColorOfStatusbarFont: TColor;
begin
  Result:= GetAppColor(TAppThemeColor.StatusFont);
  if Result=clNone then
    Result:= ATFlatTheme.ColorFont;
end;

function GetAppColorOfStatusbarDimmed: TColor;
var
  NColorFont, NColorBg: TColor;
begin
  NColorFont:= GetAppColorOfStatusbarFont;

  NColorBg:= GetAppColor(TAppThemeColor.StatusBg);
  if NColorBg=clNone then
    NColorBg:= ATFlatTheme.ColorBgPassive;

  Result:= ColorBlendHalf(NColorFont, NColorBg);
end;


procedure UpdateThemeStatusbar;
var
  NColor: TColor;
begin
  AppThemeStatusbar:= ATFlatTheme;

  if UiOps.StatusbarFontName<>'' then
    AppThemeStatusbar.FontName:= UiOps.StatusbarFontName
  else
    AppThemeStatusbar.FontName:= UiOps.VarFontName;

  if UiOps.StatusbarFontSize>0 then
    AppThemeStatusbar.FontSize:= UiOps.StatusbarFontSize
  else
    AppThemeStatusbar.FontSize:= UiOps.VarFontSize;

  NColor:= GetAppColor(TAppThemeColor.StatusFont);
  if NColor<>clNone then
    AppThemeStatusbar.ColorFont:= NColor;
end;


{$ifdef windows}
const
  PrevWndProc: WNDPROC = nil;

function WndCallback(AHWnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
const
  OBJID_MENU = LONG($FFFFFFFD);
var
  dc: HDC;
  brush: TBrush;
  clientRect, windowRect: TRect;
  mbi: MENUBARINFO;
begin
  case uMsg of
     WM_NCPAINT,
     WM_NCACTIVATE:
       begin
         Result := Windows.DefWindowProc(AHWnd, uMsg, WParam, LParam);

         FillChar(mbi{%H-}, SizeOf(mbi), 0);
         mbi.cbSize := SizeOf(mbi);
         if not GetMenuBarInfo(AHWnd, OBJID_MENU, 0, @mbi) then
           exit;

         dc := GetWindowDC(AHWnd);
         try
           GetClientRect(AHWnd, clientRect);
           MapWindowPoints(AHWnd, 0, clientRect, 2);
           GetWindowRect(AHWnd, windowRect);
           OffsetRect(clientRect, -windowRect.Left, -windowRect.Top);
           clientRect.bottom := clientRect.Top;
           Dec(clientRect.Top);

           brush := TBrush.Create;
           brush.Color := MenuStylerTheme.ColorBk;
           FillRect(dc, clientRect, brush.Reference.Handle);
           FreeAndNil(brush);
         finally
           ReleaseDC(AHWnd, dc);
         end;
         exit;
       end;
  end;

  if Assigned(PrevWndProc) then
    Result := CallWindowProc(PrevWndProc, AHWnd, uMsg, WParam, LParam);
end;
{$endif}


type
  { TGroupsHelper }

  TGroupsHelper = class
  public
    class function GetEditorFrame(Ed: TATSynEdit): TEditorFrame;
    class function GetEditorBrother(Ed: TATSynEdit): TATSynEdit;
    class function GetEditorFirstSecond(Ed: TATSynEdit; AFirst: boolean): TATSynEdit;
    class function GetPagesOfGroupIndex(AIndex: integer): TATPages;
    class function GetEditorActiveInGroup(AIndex: integer): TATSynEdit;
    class procedure ForceFrameVisible(Frame: TEditorFrame);
    class function FindPagesUnderCursorPos(ACursorPos: TPoint; AGroups: TATGroups): TATPages;
  end;

class function TGroupsHelper.GetEditorFrame(Ed: TATSynEdit): TEditorFrame;
var
  F: TCustomFrame;
begin
  F:= Ed.ParentFrameObject;
  if Assigned(F) then
    Result:= F as TEditorFrame
  else
    Result:= nil;
end;

(*
function GetEditorFrame(Ed: TATSynEdit): TEditorFrame;
//1st parent is TFormDummy, 2nd parent is TEditorFrame
var
  Ctl: TWinControl;
begin
  Result:= nil;
  if Ed=nil then exit;
  Ctl:= Ed.Parent;
  if Ctl=nil then exit;
  Ctl:= Ctl.Parent;
  if Ctl is TEditorFrame then
    Result:= TEditorFrame(Ctl);
end;
*)

class function TGroupsHelper.GetEditorBrother(Ed: TATSynEdit): TATSynEdit;
var
  F: TEditorFrame;
begin
  F:= GetEditorFrame(Ed);
  if F=nil then exit(nil);
  if Ed=F.Ed1 then
    Result:= F.Ed2
  else
    Result:= F.Ed1;
end;

class function TGroupsHelper.GetEditorFirstSecond(Ed: TATSynEdit; AFirst: boolean): TATSynEdit;
var
  F: TEditorFrame;
begin
  F:= GetEditorFrame(Ed);
  if F=nil then exit(nil);
  if AFirst then
    Result:= F.Ed1
  else
    Result:= F.Ed2;
end;


class function TGroupsHelper.GetPagesOfGroupIndex(AIndex: integer): TATPages;
begin
  Result:= nil;
  case AIndex of
    0..5:
      Result:= fmMain.Groups.Pages[AIndex];
    6:
      begin
        if Assigned(fmMain.GroupsF1) then
          Result:= fmMain.GroupsF1.Pages[0]
      end;
    7:
      begin
        if Assigned(fmMain.GroupsF2) then
          Result:= fmMain.GroupsF2.Pages[0]
      end;
    8:
      begin
        if Assigned(fmMain.GroupsF3) then
          Result:= fmMain.GroupsF3.Pages[0]
      end;
  end;
end;

class function TGroupsHelper.GetEditorActiveInGroup(AIndex: integer): TATSynEdit;
var
  Pages: TATPages;
  Data: TATTabData;
begin
  Result:= nil;
  Pages:= GetPagesOfGroupIndex(AIndex);
  if Pages=nil then exit;
  Data:= Pages.Tabs.GetTabData(Pages.Tabs.TabIndex);
  if Assigned(Data) then
    Result:= (Data.TabObject as TEditorFrame).Editor;
end;

class procedure TGroupsHelper.ForceFrameVisible(Frame: TEditorFrame);
var
  Grp: TATGroups;
  Pages: TATPages;
  NLocalGrpIndex, NGlobalGrpIndex, NTabIndex: integer;
begin
  if Frame=nil then exit;
  if Frame.Visible then exit;
  GetFrameLocation(Frame, Grp, Pages, NLocalGrpIndex, NGlobalGrpIndex, NTabIndex);
  if Assigned(Pages) and (NTabIndex>=0) then
    Pages.Tabs.TabIndex:= NTabIndex;
end;

class function TGroupsHelper.FindPagesUnderCursorPos(ACursorPos: TPoint; AGroups: TATGroups): TATPages;
var
  i: integer;
begin
  Result:= nil;
  for i in [Low(TATGroupsNums)..High(TATGroupsNums)] do
    if AGroups.Pages[i].Visible then
      if PtInControl(AGroups.Pages[i], ACursorPos) then
      begin
        Result:= AGroups.Pages[i];
        exit;
      end;
end;


procedure AppCommandPut(Ed: TATSynEdit; ACommand: integer; AInvoke: TATCommandInvoke; AForceTimer: boolean);
var
  Frame: TEditorFrame;
  Item: TAppCommandDelayed;
  D: TATTabData;
  N: integer;
begin
  Item:= Default(TAppCommandDelayed);
  Item.Code:= ACommand;
  Item.EdAddress:= Ed;
  Item.EdIndex:= 0;
  Item.Tabs:= nil;
  Item.TabIndex:= -1;
  Item.Invoke:= AInvoke;

  Frame:= TGroupsHelper.GetEditorFrame(Ed);
  if Assigned(Frame) then
  begin
    Item.EdIndex:= Frame.EditorObjToIndex(Ed);
    Item.Tabs:= Frame.GetTabPages.Tabs;
    //is it active tab?
    N:= Item.Tabs.TabIndex;
    D:= Item.Tabs.GetTabData(N);
    if Assigned(D) and (D.TabObject=Frame) then
      Item.TabIndex:= N
    else
      //it's passive tab
      Item.TabIndex:= Item.Tabs.FindTabByObject(Frame);
  end;

  AppCommandsDelayed.Push(Item);

  if AForceTimer or IsCommandNeedTimer(ACommand) then
  begin
    fmMain.TimerCmd.Enabled:= true;
  end
  else
  begin
    fmMain.TimerCmd.Enabled:= false;
    fmMain.TimerCmdTimer(nil);
  end;
end;


function AppCommandGet(out AEditor: TATSynEdit; out ACommand: integer;
  out AInvoke: TATCommandInvoke): TAppCommandGetStatus;
var
  Item: TAppCommandDelayed;
  TabData: TATTabData;
  Frame: TEditorFrame;
  EdTemp: TATSynEdit;
begin
  AEditor:= nil;
  ACommand:= 0;
  AInvoke:= TATCommandInvoke.AppInternal;
  if AppCommandsDelayed.IsEmpty() then
    exit(TAppCommandGetStatus.NoCommands);

  if AppCommandHandlerIsBusy then
    exit(TAppCommandGetStatus.BadCommand);

  Result:= TAppCommandGetStatus.BadCommand;
  Item:= AppCommandsDelayed.Front();
  AppCommandsDelayed.Pop();

  if Item.Tabs=nil then exit;
  TabData:= Item.Tabs.GetTabData(Item.TabIndex);
  if TabData=nil then exit;
  if TabData.TabObject=nil then exit;
  Frame:= TabData.TabObject as TEditorFrame;
  EdTemp:= Frame.EditorIndexToObj(Item.EdIndex);
  if EdTemp=nil then exit;

  //Item.EdAddress is like CRC here.
  //we don't read the memory from this pointer!
  //why? avoid AV from deleted frames.
  if pointer(EdTemp)<>Item.EdAddress then exit;

  AEditor:= EdTemp;
  ACommand:= Item.Code;
  AInvoke:= Item.Invoke;
  Result:= TAppCommandGetStatus.OkCommand;

  if ACommand=cmd_FileCloseAll then
    AppCommandHandlerIsBusy:= true;
end;

type
  { TKeymapHelperMain }

  TKeymapHelperMain = class
  public
    class procedure DeleteCategoryWithHotkeyBackup(AKeymap: TATKeymap; ABackup: TAppHotkeyBackup; ACategory: TAppCommandCategory);
    class procedure AddPluginsWithHotkeyBackup(AKeymap: TATKeymap; ABackup: TAppHotkeyBackup; AItemsFromAPI: boolean);
    class procedure UpdateDynamicEx(AKeymap: TATKeymap; ACategory: TAppCommandCategory);
    class procedure UpdateDynamic(ACategory: TAppCommandCategory);
    class function Debug_PluginCommands(AKeymap: TATKeymap; const AText: string): string;
    class function Debug_PluginCommands(const AText: string): string;
  end;

class procedure TKeymapHelperMain.DeleteCategoryWithHotkeyBackup(AKeymap: TATKeymap; ABackup: TAppHotkeyBackup; ACategory: TAppCommandCategory);
var
  MapItem: TATKeymapItem;
  Cmd, i: integer;
begin
  for i:= AKeymap.Count-1 downto 0 do
  begin
    MapItem:= AKeymap[i];
    Cmd:= MapItem.Command;
    if Cmd<cmdFirstAppCommand then Break;
    if TPluginHelper.CommandCategory(Cmd)=ACategory then
    begin
      //backup hotkeys of plugins
      //this function must not loose any hotkeys!
      if ACategory in [TAppCommandCategory.Plugin, TAppCommandCategory.PluginSub] then
        ABackup.Add(MapItem, MapItem.Description);

      AKeymap.Delete(i);
    end;
  end;
end;

class procedure TKeymapHelperMain.AddPluginsWithHotkeyBackup(AKeymap: TATKeymap; ABackup: TAppHotkeyBackup; AItemsFromAPI: boolean);
var
  CmdItem: TAppCommandInfo;
  SCommandText: string;
  i: integer;
begin
  {
  SCommandText:= ABackup.DebugText;
  if SCommandText<>'' then
    MsgBox(SCommandText, MB_OK);
    }

  if not AItemsFromAPI then
  for i:= 0 to AppCommandList.Count-1 do
  begin
    CmdItem:= TAppCommandInfo(AppCommandList[i]);
    if CmdItem.ItemModule='' then Break;
    if SEndsWith(CmdItem.ItemCaption, '-') then Continue;
    SCommandText:= CmdItem.CommaStr;

    AKeymap.Add(
      cmdFirstPluginCommand+i,
      'plugin: '+AppNicePluginCaption(CmdItem.ItemCaption),
      [], [],
      SCommandText);

    ABackup.Get(AKeymap[AKeymap.Count-1], SCommandText);
  end
  else
  for i:= 0 to AppCommand2List.Count-1 do
  begin
    CmdItem:= TAppCommandInfo(AppCommand2List[i]);
    if CmdItem.ItemModule='' then Break;
    SCommandText:= CmdItem.CommaStr;

    AKeymap.Add(
      cmdFirstPluginSubCommand+i,
      'plugin: '+AppNicePluginCaption(CmdItem.ItemCaption),
      [], [],
      SCommandText);

    ABackup.Get(AKeymap[AKeymap.Count-1], SCommandText);
  end;
end;

class procedure TKeymapHelperMain.UpdateDynamicEx(AKeymap: TATKeymap; ACategory: TAppCommandCategory);
var
  Frame: TEditorFrame;
  An: TecSyntAnalyzer;
  KeysBackup: TAppHotkeyBackup;
  sl: TStringList;
  i: integer;
begin
  KeysBackup:= TAppHotkeyBackup.Create;
  DeleteCategoryWithHotkeyBackup(AKeymap, KeysBackup, ACategory);

  //if ACategory in [categ_Plugin, categ_PluginSub] then
  //  MsgBox('1'#10+TKeymapHelperMain.Debug_PluginCommands(AKeymap, 'Macro'), MB_OK); ///////debug

  case ACategory of
    TAppCommandCategory.Lexer:
      begin
        sl:= TStringList.Create;
        try
          //usual lexers
          for i:= 0 to AppManager.LexerCount-1 do
          begin
            An:= AppManager.Lexers[i];
            if An.Deleted then Continue;
            if An.Internal then Continue;
            sl.AddObject(An.LexerName,
                         TObject(cmdFirstLexerCommand+i));
          end;

          //lite lexers
          for i:= 0 to AppManagerLite.LexerCount-1 do
            sl.AddObject(AppManagerLite.Lexers[i].LexerName+msgLiteLexerSuffix,
                         TObject(cmdFirstLexerCommand+i+AppManager.LexerCount));

          sl.Sort;

          //insert "none" at list begin
          sl.InsertObject(0, msgNoLexer, TObject(cmdLastLexerCommand));

          for i:= 0 to sl.count-1 do
            AKeymap.Add(
              PtrInt(sl.Objects[i]),
              'lexer: '+sl[i],
              [], []);
        finally
          FreeAndNil(sl);
        end;
      end;

    TAppCommandCategory.Plugin,
    TAppCommandCategory.PluginSub:
      begin
        AddPluginsWithHotkeyBackup(AKeymap, KeysBackup, ACategory=TAppCommandCategory.PluginSub);
      end;

    TAppCommandCategory.OpenedFile:
      for i:= 0 to AppFrameList1.Count-1 do
      begin
        Frame:= TEditorFrame(AppFrameList1[i]);
        if Frame.FileName<>'' then
          AKeymap.Add(
            cmdFirstFileCommand+i,
            'opened file: '+FormatFilenameForMenu(Frame.FileName),
            [], []);
      end;

    TAppCommandCategory.RecentFile:
      begin
        for i:= 0 to AppListRecents.Count-1 do
        begin
          AKeymap.Add(
            cmdFirstRecentCommand+i,
            'recent file: '+FormatFilenameForMenu(AppListRecents[i]),
            [], []);
        end;
      end;
  end;

  FreeAndNil(KeysBackup);
end;

class procedure TKeymapHelperMain.UpdateDynamic(ACategory: TAppCommandCategory);
var
  Map: TATKeymap;
  i: integer;
begin
  UpdateDynamicEx(AppKeymapMain, ACategory);

  for i:= 0 to AppKeymapLexers.Count-1 do
  begin
    Map:= TATKeymap(AppKeymapLexers.Objects[i]);
    UpdateDynamicEx(Map, ACategory);
  end;
end;

class function TKeymapHelperMain.Debug_PluginCommands(AKeymap: TATKeymap;
  const AText: string): string;
var
  Cmd, i: integer;
begin
  Result:= '';
  for i:= 0 to AKeymap.Count-1 do
  begin
    Cmd:= AKeymap.Items[i].Command;
    if TPluginHelper.CommandCategory(Cmd) in [TAppCommandCategory.Plugin, TAppCommandCategory.PluginSub] then
      if Pos(AText, AKeymap.Items[i].Name)>0 then
        Result+= AKeymap.Items[i].Name+#10;
  end;
end;

class function TKeymapHelperMain.Debug_PluginCommands(const AText: string): string;
var
  Map: TATKeymap;
  i: integer;
begin
  Result:= Debug_PluginCommands(AppKeymapMain, AText);

  for i:= 0 to AppKeymapLexers.Count-1 do
  begin
    Map:= TATKeymap(AppKeymapLexers.Objects[i]);
    Result+= Debug_PluginCommands(Map, AText);
  end;
end;

{ TAppFormWithEditor }

procedure TAppFormWithEditor.Clear;
begin
  EditorClear(Ed);
  Ed.Update(true);
end;

procedure TAppFormWithEditor.Add(const AText: string);
begin
  Ed.ModeReadOnly:= false;
  Ed.Strings.LineAdd(AText);
  Ed.ModeReadOnly:= true;
  Ed.Update(true);
end;

function TAppFormWithEditor.IsIndexValid(AIndex: integer): boolean;
begin
  Result:= Ed.Strings.IsIndexValid(AIndex);
end;

{ TAppCompletionApiProps }

procedure TAppCompletionApiProps.Clear;
begin
  Editor:= nil;
  Text:= '';
  CharsLeft:= 0;
  CharsRight:= 0;
  CaretPos:= Point(0, 0);
end;

{ TAppNotifThread }

procedure TAppNotifThread.NotifyFrame1;
begin
  CurFrame.NotifyAboutChange(CurFrame.Ed1);
end;

procedure TAppNotifThread.NotifyFrame2;
begin
  CurFrame.NotifyAboutChange(CurFrame.Ed2);
end;

{
procedure TAppNotifThread.ModifyFrame1;
begin
  CurFrame.Ed1.Modified:= true;
  CurFrame.UpdateModified(CurFrame.Ed1);
end;
}

procedure TAppNotifThread.HandleOneFrame;
  //
  function IsEditorChanged(EdIndex: integer): boolean;
  var
    SFileName: string;
    Props: TAppFileProps;
  begin
    Result:= false;
    if EdIndex=0 then
      SFileName:= CurFrame.FileName
    else
      SFileName:= CurFrame.FileName2;
    if SFileName='' then exit;

    Props.Init(SFileName);
    if not CurFrame.FileProps[EdIndex].Inited then
      CurFrame.FileProps[EdIndex]:= Props
    else
    if CurFrame.FileProps[EdIndex]<>Props then
    begin
      CurFrame.FileProps[EdIndex]:= Props;
      Result:= true;
    end;
  end;
  //
begin
  if IsEditorChanged(0) then
    Synchronize(@NotifyFrame1);

  if not CurFrame.EditorsLinked then
    if IsEditorChanged(1) then
      Synchronize(@NotifyFrame2);
end;

procedure TAppNotifThread.Execute;
var
  i: integer;
begin
  repeat
    for i:= 1 to cThreadSleepCount*UiOps.NotificationTimeSeconds do
    begin
      if Application.Terminated then exit;
      if Terminated then exit;
      Sleep(cThreadSleepTime);
    end;

    if not UiOps.NotificationEnabled then Continue;

    AppEventLister.WaitFor(INFINITE);
    AppEventWatcher.ResetEvent;

    try
      for i:= 0 to AppFrameList2.Count-1 do
      begin
        CurFrame:= TEditorFrame(AppFrameList2[i]);
        if CurFrame.FileName='' then Continue;
        if not CurFrame.NotifEnabled then Continue;
        if CurFrame.FrameKind<>TAppFrameKind.Editor then Continue;
        HandleOneFrame;
      end;
    finally
      AppEventWatcher.SetEvent;
    end;
  until false;
end;

{ TFrameEditState }

operator =(constref a, b: TFrameEditState): boolean;
begin
  Result:=
    (a.Ed1_FileName = b.Ed1_FileName) and
    (a.Ed2_FileName = b.Ed2_FileName) and
    (a.Ed1_ModifiedVersion = b.Ed1_ModifiedVersion) and
    (a.Ed2_ModifiedVersion = b.Ed2_ModifiedVersion);
end;

procedure TFrameEditState.Assign(AFrame: TEditorFrame);
begin
  Ed1_FileName:= AFrame.Ed1.FileName;
  Ed2_FileName:= AFrame.Ed2.FileName;
  Ed1_ModifiedVersion:= AFrame.Ed1.Strings.ModifiedVersion;
  Ed2_ModifiedVersion:= AFrame.Ed2.Strings.ModifiedVersion;
end;

{ TfmMain }

{$I formmain_py_toolbars.inc}
{$I formmain_py_statusbars.inc}
{$I formmain_py_api.inc}
{$I formmain_py_helpers.inc}
{$I formmain_py_pluginwork.inc}

procedure TfmMain.MenuViewerModeClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F=nil then exit;
  if F.FrameKind<>TAppFrameKind.BinaryViewer then exit;
  F.Binary.Mode:= TATBinHexMode((Sender as TComponent).Tag);
  UpdateStatusbar;
end;

procedure TfmMain.InitPopupBottom(var AMenu: TPopupMenu; AEditor: TATSynEdit);
var
  mi: TMenuItem;
begin
  if AMenu=nil then
  begin
    AMenu:= TPopupMenu.Create(Self);
    AMenu.OnPopup:=@PopupBottomOnPopup;

    mi:= TMenuItem.Create(AEditor);
    mi.Caption:= 'copy';
    mi.Tag:= 100;
    mi.OnClick:=@PopupBottomCopyClick;
    AMenu.Items.Add(mi);

    mi:= TMenuItem.Create(AEditor);
    mi.Caption:= 'select all';
    mi.Tag:= 101;
    mi.OnClick:=@PopupBottomSelectAllClick;
    AMenu.Items.Add(mi);

    mi:= TMenuItem.Create(AEditor);
    mi.Caption:= 'clear';
    mi.Tag:= 102;
    mi.OnClick:=@PopupBottomClearClick;
    AMenu.Items.Add(mi);

    mi:= TMenuItem.Create(AEditor);
    mi.Caption:= 'toggle word wrap';
    mi.Tag:= 103;
    mi.OnClick:=@PopupBottomWrapClick;
    AMenu.Items.Add(mi);
  end;
end;

procedure TfmMain.InitPopupViewerMode;
var
  mi: TMenuItem;
  mode: TATBinHexMode;
begin
  if PopupViewerMode=nil then
  begin
    PopupViewerMode:= TPopupMenu.Create(Self);
    for mode:= Low(mode) to High(mode) do
    begin
      mi:= TMenuItem.Create(Self);
      mi.Caption:= msgViewer+': '+msgViewerModes[mode];
      mi.OnClick:= @MenuViewerModeClick;
      mi.Tag:= Ord(mode);
      PopupViewerMode.Items.Add(mi);
    end;
  end;
end;

procedure TfmMain.InitPopupTab;
var
  mi: TMenuItem;
begin
  if PopupTab=nil then
  begin
    PopupTab:= TPopupMenu.Create(Self);
    PopupTab.OnPopup:= @PopupTabPopup;

    mnuTabCloseThis:= TMenuItem.Create(Self);
    mnuTabCloseThis.Caption:= 'Close tab';
    mnuTabCloseThis.OnClick:= @mnuTabCloseThisClick;
    PopupTab.Items.Add(mnuTabCloseThis);

    mnuTabCloseSub:= TMenuItem.Create(Self);
    mnuTabCloseSub.Caption:= 'Close';
    PopupTab.Items.Add(mnuTabCloseSub);

    mnuTabCloseOtherSame:= TMenuItem.Create(Self);
    mnuTabCloseOtherSame.Caption:= 'Others (same group)';
    mnuTabCloseOtherSame.OnClick:= @mnuTabCloseOtherSameClick;
    mnuTabCloseSub.Add(mnuTabCloseOtherSame);

    mnuTabCloseOtherAll:= TMenuItem.Create(Self);
    mnuTabCloseOtherAll.Caption:= 'Others (all groups)';
    mnuTabCloseOtherAll.OnClick:= @mnuTabCloseOtherAllClick;
    mnuTabCloseSub.Add(mnuTabCloseOtherAll);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    mnuTabCloseSub.Add(mi);

    mnuTabCloseAllSame:= TMenuItem.Create(Self);
    mnuTabCloseAllSame.Caption:= 'All (same group)';
    mnuTabCloseAllSame.OnClick:= @mnuTabCloseAllSameClick;
    mnuTabCloseSub.Add(mnuTabCloseAllSame);

    mnuTabCloseAllAll:= TMenuItem.Create(Self);
    mnuTabCloseAllAll.Caption:= 'All (all groups)';
    mnuTabCloseAllAll.OnClick:= @mnuTabCloseAllAllClick;
    mnuTabCloseSub.Add(mnuTabCloseAllAll);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    mnuTabCloseSub.Add(mi);

    mnuTabCloseLeft:= TMenuItem.Create(Self);
    mnuTabCloseLeft.Caption:= 'Left tabs (same group)';
    mnuTabCloseLeft.OnClick:= @mnuTabCloseLeftClick;
    mnuTabCloseSub.Add(mnuTabCloseLeft);

    mnuTabCloseRight:= TMenuItem.Create(Self);
    mnuTabCloseRight.Caption:= 'Right tabs (same group)';
    mnuTabCloseRight.OnClick:= @mnuTabCloseRightClick;
    mnuTabCloseSub.Add(mnuTabCloseRight);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    PopupTab.Items.Add(mi);

    mnuTabSave:= TMenuItem.Create(Self);
    mnuTabSave.Caption:= 'Save';
    mnuTabSave.OnClick:= @mnuTabSaveClick;
    PopupTab.Items.Add(mnuTabSave);

    mnuTabSaveAs:= TMenuItem.Create(Self);
    mnuTabSaveAs.Caption:= 'Save as...';
    mnuTabSaveAs.OnClick:= @mnuTabSaveAsClick;
    PopupTab.Items.Add(mnuTabSaveAs);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    PopupTab.Items.Add(mi);

    mnuTabCopySub:= TMenuItem.Create(Self);
    mnuTabCopySub.Caption:= 'Copy to clipboard';
    PopupTab.Items.Add(mnuTabCopySub);

    mnuTabCopyFullPath:= TMenuItem.Create(Self);
    mnuTabCopyFullPath.Caption:= 'Copy full filepath';
    mnuTabCopyFullPath.OnClick:= @mnuTabCopyFullPathClick;
    mnuTabCopySub.Add(mnuTabCopyFullPath);

    mnuTabCopyDir:= TMenuItem.Create(Self);
    mnuTabCopyDir.Caption:= 'Copy filepath only';
    mnuTabCopyDir.OnClick:= @mnuTabCopyDirClick;
    mnuTabCopySub.Add(mnuTabCopyDir);

    mnuTabCopyName:= TMenuItem.Create(Self);
    mnuTabCopyName.Caption:= 'Copy filename only';
    mnuTabCopyName.OnClick:= @mnuTabCopyNameClick;
    mnuTabCopySub.Add(mnuTabCopyName);

    mnuTabMoveSub:= TMenuItem.Create(Self);
    mnuTabMoveSub.Caption:= 'Move tab to group';
    PopupTab.Items.Add(mnuTabMoveSub);

    mnuTabMove1:= TMenuItem.Create(Self);
    mnuTabMove1.Caption:= '1';
    mnuTabMove1.OnClick:= @mnuTabMove1Click;
    mnuTabMoveSub.Add(mnuTabMove1);

    mnuTabMove2:= TMenuItem.Create(Self);
    mnuTabMove2.Caption:= '2';
    mnuTabMove2.OnClick:= @mnuTabMove2Click;
    mnuTabMoveSub.Add(mnuTabMove2);

    mnuTabMove3:= TMenuItem.Create(Self);
    mnuTabMove3.Caption:= '3';
    mnuTabMove3.OnClick:= @mnuTabMove3Click;
    mnuTabMoveSub.Add(mnuTabMove3);

    mnuTabMove4:= TMenuItem.Create(Self);
    mnuTabMove4.Caption:= '4';
    mnuTabMove4.OnClick:= @mnuTabMove4Click;
    mnuTabMoveSub.Add(mnuTabMove4);

    mnuTabMove5:= TMenuItem.Create(Self);
    mnuTabMove5.Caption:= '5';
    mnuTabMove5.OnClick:= @mnuTabMove5Click;
    mnuTabMoveSub.Add(mnuTabMove5);

    mnuTabMove6:= TMenuItem.Create(Self);
    mnuTabMove6.Caption:= '6';
    mnuTabMove6.OnClick:= @mnuTabMove6Click;
    mnuTabMoveSub.Add(mnuTabMove6);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    mnuTabMoveSub.Add(mi);

    mnuTabMoveF1:= TMenuItem.Create(Self);
    mnuTabMoveF1.Caption:= 'Floating 1';
    mnuTabMoveF1.OnClick:= @mnuTabMoveF1Click;
    mnuTabMoveSub.Add(mnuTabMoveF1);

    mnuTabMoveF2:= TMenuItem.Create(Self);
    mnuTabMoveF2.Caption:= 'Floating 2';
    mnuTabMoveF2.OnClick:= @mnuTabMoveF2Click;
    mnuTabMoveSub.Add(mnuTabMoveF2);

    mnuTabMoveF3:= TMenuItem.Create(Self);
    mnuTabMoveF3.Caption:= 'Floating 3';
    mnuTabMoveF3.OnClick:= @mnuTabMoveF3Click;
    mnuTabMoveSub.Add(mnuTabMoveF3);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    mnuTabMoveSub.Add(mi);

    mnuTabMoveNext:= TMenuItem.Create(Self);
    mnuTabMoveNext.Caption:= 'Next';
    mnuTabMoveNext.OnClick:= @mnuTabMoveNextClick;
    mnuTabMoveSub.Add(mnuTabMoveNext);

    mnuTabMovePrev:= TMenuItem.Create(Self);
    mnuTabMovePrev.Caption:= 'Previous';
    mnuTabMovePrev.OnClick:= @mnuTabMovePrevClick;
    mnuTabMoveSub.Add(mnuTabMovePrev);

    mnuTabPinned:= TMenuItem.Create(Self);
    mnuTabPinned.Caption:= 'Pinned';
    mnuTabPinned.OnClick:= @mnuTabPinnedClick;
    PopupTab.Items.Add(mnuTabPinned);

    mnuTabColor:= TMenuItem.Create(Self);
    mnuTabColor.Caption:= 'Set tab color...';
    mnuTabColor.OnClick:= @mnuTabColorClick;
    PopupTab.Items.Add(mnuTabColor);
  end;

  DoLocalizePopupTab;
end;

procedure TfmMain.StatusPanelClick(Sender: TObject; AIndex: Integer);
var
  Frame: TEditorFrame;
  FrameKind: TAppFrameKind;
  Data: TATStatusData;
  Pnt: TPoint;
  mi: TMenuItem;
  i: integer;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  FrameKind:= Frame.FrameKind;

  Data:= Status.GetPanelData(AIndex);
  if Data=nil then exit;

  if FrameKind=TAppFrameKind.ImageViewer then
  begin
    case Data.Tag of
      StatusbarTag_TabSize:
        begin
          InitPopupPicScale;
          PopupPicScale.Popup;
        end;
    end;
    exit;
  end;

  //cell of plugin
  if Data.Tag>20 then
  begin
    PyStatusbarPanelClick(Sender, Data.Tag);
  end
  else
  //standard cell in viewer mode
  if FrameKind=TAppFrameKind.BinaryViewer then
  begin
    case Data.Tag of
      StatusbarTag_Caret:
        begin
          DoDialogGoto;
        end;
      StatusbarTag_Enc:
        begin
          with Mouse.CursorPos do
            Frame.Binary.TextEncodingsMenu(X, Y);
        end;
      StatusbarTag_Lexer:
        begin
          InitPopupViewerMode;
          PopupViewerMode.PopUp;
        end;
      StatusbarTag_WrapMode:
        begin
          Frame.Binary.TextWrap:= not Frame.Binary.TextWrap;
          UpdateStatusbar;
          UpdateMenuChecks_Frame(Frame);
        end;
    end;
  end
  else
  //standard cell in editor mode
  case Data.Tag of
    StatusbarTag_Caret:
      begin
        DoDialogGoto;
      end;
    StatusbarTag_Enc:
      begin
        if not Frame.ReadOnly[Frame.Editor] then
          DoDialogMenuEncodings;
      end;
    StatusbarTag_LineEnds:
      begin
        if not Frame.ReadOnly[Frame.Editor] then
          DoDialogMenuEnds;
      end;
    StatusbarTag_Lexer:
      begin
        DoDialogLexerMenu;
      end;
    StatusbarTag_TabSize:
      begin
        InitPopupTabSize;
        PopupTabSize.Popup;
      end;
    StatusbarTag_SelMode:
      begin
        with Frame.Editor do
          OptMouseColumnSelectionWithoutKey:= not OptMouseColumnSelectionWithoutKey;
        UpdateStatusbar_RealWork;
      end;
    StatusbarTag_WrapMode:
      begin
        //toggle values: no wrap / wrap at window / wrap at margin
        with Frame.Editor do
        begin
          if OptWrapMode=High(OptWrapMode) then
            OptWrapMode:= Low(OptWrapMode)
          else
            OptWrapMode:= Succ(OptWrapMode);
        end;
        UpdateStatusbar_RealWork;
      end;
    StatusbarTag_Msg:
      begin
        if PopupStatusbarMsg=nil then
          PopupStatusbarMsg:= TPopupMenu.Create(Self);
        PopupStatusbarMsg.Items.Clear;
        for i:= 0 to AppStatusbarMessages.Count-1 do
        begin
          mi:= TMenuItem.Create(Self);
          mi.Caption:= AppStatusbarMessages[i];
          mi.Enabled:= false;
          PopupStatusbarMsg.Items.Add(mi);
        end;
        Pnt:= Mouse.CursorPos;
        PopupStatusbarMsg.PopUp(Pnt.X, Pnt.Y);
      end;
  end;
end;

procedure TfmMain.TimerAppIdleTimer(Sender: TObject);
var
  STemp: string;
  Frame: TEditorFrame;
  NTick: QWord;
begin
  //in Lazarus 2.1 trunk on Linux x64 gtk2/qt5, TimerAppIdle.Timer is called too early,
  //when Handle is not created
  if not HandleAllocated then
  begin
    //debug
    exit;
  end;

  if AppClosingTabs then exit;
  if AppDroppingFiles then exit;

  if FOption_StartupCommand<>'' then
  begin
    STemp:= FOption_StartupCommand;
    FOption_StartupCommand:= '';
    DoPyCommand_CommandLineParam(STemp);
  end;

  TimerMouseStop.Enabled:= TPluginHelper.EventIsUsed(TAppPyEvent.OnMouseStop);

  if not FHandledMakeCaretVisible and AppFormShowCompleted then
  begin
    FHandledMakeCaretVisible:= true;
    Frame:= CurrentFrame;
    if Assigned(Frame) then
    begin
      EditorCaretToView(Frame.Ed1, true, {$ifdef windows}false{$else}true{$endif});
      if Frame.Splitted then
        EditorCaretToView(Frame.Ed2, true, {$ifdef windows}false{$else}true{$endif});
    end;
  end;

  //flush saved Python "print" results to console
  if Assigned(fmConsole) then
    fmConsole.FlushConsole;

  AppUpdateWatcherFrames;
  if AppCommandHandlerIsBusy then exit;

  Frame:= CurrentFrame;

  UpdateToolbarButtons(Frame);

  if not PtInRect(BoundsRect, Mouse.CursorPos) then
    InvalidateMouseoverDependantControls;

  //frame requested to update statusbar
  if FNeedUpdateStatuses then
  begin
    FNeedUpdateStatuses:= false;
    TimerStatusWork.Enabled:= false;
    UpdateTabCaptionsFromFolders;
    UpdateMenuChecks_Frame(Frame);
    UpdateMenuChecks_FrameSplit(Frame);
    UpdateMenuChecks_Global;
    UpdateStatusbar_RealWork;
  end;

  if FNeedUpdateMenuPlugins then
  begin
    FNeedUpdateMenuPlugins:= false;
    UpdateMenuPlugins; //takes ~30 msec, so it is now in TimerAppIdle
    UpdateMenuPlugins_Shortcuts(true);
    UpdateMenuHotkeys; //takes ~3 msec
    DoPyEvent(nil, TAppPyEvent.OnInitPluginsMenu, []);
  end;

  if FNeedUpdateMenuChecks then
  begin
    FNeedUpdateMenuChecks:= false;
    UpdateMenuChecks_Frame(Frame);
    UpdateMenuChecks_FrameSplit(Frame);
    UpdateMenuChecks_Global;
  end;

  if Assigned(Frame) and not (Frame.IsTreeBusy or Frame.IsParsingBusy) then
    if AppCodetreeState.NeedsSelJump then
    begin
      AppCodetreeState.NeedsSelJump:= false;
      UpdateTree(false);
    end;

  //fire on_change_idle
  if Assigned(Frame) then
  begin
    if Frame.TextChangeSlow[0] then
    begin
      Frame.TextChangeSlow[0]:= false;
      DoPyEvent(Frame.Ed1, TAppPyEvent.OnChangeSlow, []);
    end;
    if Frame.TextChangeSlow[1] then
    begin
      Frame.TextChangeSlow[1]:= false;
      DoPyEvent(Frame.Ed2, TAppPyEvent.OnChangeSlow, []);
    end;
  end;

  if FNeedUpdateMenuShortcuts then
  begin
    FNeedUpdateMenuShortcuts:= false;
    UpdateMenuPlugins_Shortcuts_Work(FNeedUpdateMenuShortcuts_Force);
  end;

  if FNeedAppState_MenuAdd then
  begin
    FNeedAppState_MenuAdd:= false;
    DoPyEvent_AppState(APPSTATE_API_MENU_ADD);
  end;
  if FNeedAppState_MenuRemove then
  begin
    FNeedAppState_MenuRemove:= false;
    DoPyEvent_AppState(APPSTATE_API_MENU_REMOVE);
  end;
  if FNeedAppState_MenuChange then
  begin
    FNeedAppState_MenuChange:= false;
    DoPyEvent_AppState(APPSTATE_API_MENU_CHANGE);
  end;
  if FNeedAppState_SubCommands then
  begin
    FNeedAppState_SubCommands:= false;
    DoPyEvent_AppState(APPSTATE_API_SUBCOMMANDS);
  end;

  //auto-save session (and text of modified tabs) each N seconds
  if ((UiOps.SessionSaveInterval>0) and UiOps.ReopenSession) or
     (UiOps.SessionSaveInterval<0) then
  begin
    NTick:= GetTickCount64;
    if FLastSaveSessionTick=0 then
      FLastSaveSessionTick:= NTick
    else
    if NTick-FLastSaveSessionTick>=Abs(UiOps.SessionSaveInterval)*1000 then
    begin
      FLastSaveSessionTick:= NTick;
      DoOps_SaveSession(AppFile_Session, true{ASaveModifiedTabs}, true{ASaveUntitledTabs}, true{AByTimer});
    end;
  end;
end;

procedure TfmMain.TimerStatusClearTimer(Sender: TObject);
begin
  DoStatusbarColorByTag(Status, StatusbarTag_Msg, GetAppColorOfStatusbarDimmed);
  TimerStatusClear.Enabled:= false;
end;

procedure TfmMain.TimerTooltipTimer(Sender: TObject);
begin
  DoTooltipHide;
end;

procedure TfmMain.TimerStatusWorkTimer(Sender: TObject);
begin
  TimerStatusWork.Enabled:= false;
  UpdateStatusbar_RealWork;
end;

procedure TfmMain.TimerTreeFillTimer(Sender: TObject);
var
  Frame: TEditorFrame;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  if Frame.IsTreeBusy or Frame.IsParsingBusy then exit;

  TimerTreeFill.Enabled:= false;
  UpdateTree(true);
end;

procedure TfmMain.DoCodetree_UpdateVersion(Ed: TATSynEdit);
begin
  Inc(AppCodetreeState.Version);
  AppCodetreeState.Editor:= Ed;
  if Assigned(Ed) then
    AppCodetreeState.Lexer:= EditorLexerNameAtPos(Ed, Point(-1, -1))
  else
    AppCodetreeState.Lexer:= '';
end;

procedure TfmMain.DoCodetree_OnClick(Sender: TObject);
begin
  CloseFormAutoCompletion;
end;

procedure TfmMain.DoCodetree_OnDblClick(Sender: TObject);
var
  Ed: TATSynEdit;
  PntBegin, PntEnd: TPoint;
begin
  DoCodetree_GetSyntaxRange(CodeTree.Tree.Selected, PntBegin, PntEnd);

  AppCodetreeState.DblClicking:= true;
  try
    Ed:= CurrentEditor;
    Ed.DoGotoPos(
      PntBegin,
      Point(-1, -1),
      UiOps.FindIndentHorz,
      UiOps.FindIndentVert,
      true,
      TATEditorActionIfFolded.Unfold
      );
    DoFocusEditor(Ed);

    if (AppCodetreeState.SelLine<>PntBegin.Y) or
      (AppCodetreeState.Editor<>Ed) then
    begin
      AppCodetreeState.Editor:= Ed;
      AppCodetreeState.SelLine:= PntBegin.Y;
      if AppCodetreeState.SelLine>=0 then
        DoPyEvent_AppState(APPSTATE_CODETREE_SET_SELECTION);
    end;
  finally
    AppCodetreeState.DblClicking:= false;
  end;
end;

procedure TfmMain.DoCodetree_GetSyntaxRange(ANode: TTreeNode; out APosBegin, APosEnd: TPoint);
var
  DataObj: TObject;
  Range: TATRangeInCodeTree;
begin
  APosBegin:= Point(-1, -1);
  APosEnd:= Point(-1, -1);
  if ANode=nil then exit;
  if ANode.Data=nil then exit;

  DataObj:= TObject(ANode.Data);
  if not (DataObj is TATRangeInCodeTree) then
  begin
    MsgLogConsole('ERROR: tree_proc(... TREE_ITEM_GET_RANGE ...) gets wrong node type: '+DataObj.ClassName);
    exit;
  end;
  Range:= DataObj as TATRangeInCodeTree;
  APosBegin:= Range.PosBegin;
  APosEnd:= Range.PosEnd;
end;

procedure TfmMain.DoCodetree_SetSyntaxRange(ANode: TTreeNode; const APosBegin, APosEnd: TPoint);
var
  DataObj: TObject;
  Range: TATRangeInCodeTree;
begin
  if ANode=nil then exit;
  if ANode.Data=nil then
  begin
    DataObj:= TATRangeInCodeTree.Create;
    ANode.Data:= Pointer(DataObj);
  end
  else
    DataObj:= TObject(ANode.Data);

  if DataObj is TATRangeInCodeTree then
  begin
    Range:= DataObj as TATRangeInCodeTree;
    Range.PosBegin:= APosBegin;
    Range.PosEnd:= APosEnd;
  end
  else
  begin
    MsgLogConsole('ERROR: tree_proc(... TREE_ITEM_SET_RANGE ...) gets wrong node type: '+DataObj.ClassName);
  end;
end;

procedure TfmMain.DoCodetree_Clear;
begin
  { //is it needed to fix #4459 ?
  if Assigned(FCodetreeBuffer) then
    FCodetreeBuffer.Items.Clear;
    }

  if CodeTree.Tree.Items.Count>0 then
  begin
    CodeTree.Tree.Items.Clear;
    DoCodetree_UpdateVersion(nil);
    AppCodetreeState.NeedsSelJump:= false;
    DoPyEvent_AppState(APPSTATE_CODETREE_CLEAR);
  end;
end;


procedure TfmMain.DoCodetree_OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  //fix to hide parts on Tree's hints on editor canvas (Win32, moving mouse from
  //long hint to shorter)
  DoInvalidateEditors;
end;


function TfmMain.IsAllowedToOpenFileNow: boolean;
begin
  Result:= true;
  if IsDialogCustomShown then exit(false);
  if Assigned(fmCommands) and fmCommands.Visible then fmCommands.Close;
end;


procedure TfmMain.InitAppleMenu;
var
  cAppleString: string;
begin
  {$ifndef darwin}
  //don't run this procedure on Win/Linux
  exit;
  {$endif}

  cAppleString:= UTF8Encode(WideChar($F8FF));

  mnuApple:= TMenuItem.Create(Self);
  mnuApple.Caption:= cAppleString;
  Menu.Items.Insert(0, mnuApple);

  mnuApple_About:= TMenuItem.Create(Self);
  mnuApple_About.Caption:= 'About CudaText';
  mnuApple.Add(mnuApple_About);
  mnuHelpAbout.Visible:= false;

  //macOS adds "Quit" item in Apple menu, "Exit" not needed
  mnuFileExit.Visible:= false;
end;


procedure TfmMain.InitCodeTree;
begin
  PanelCodeTreeAll:= TFormDummy.Create(Self);
  PanelCodeTreeAll.Name:= 'PanelCodeTreeAll';
  PanelCodeTreeAll.BorderStyle:= bsNone;
  PanelCodeTreeAll.OnEnter:= @DoCodetree_PanelOnEnter;

  CodeTree:= TAppTreeContainer.Create(PanelCodeTreeAll);
  CodeTree.Name:= 'CodeTree';
  DoControl_InitPropsObject(CodeTree, PanelCodeTreeAll, 'treeview');
  CodeTree.Parent:= PanelCodeTreeAll;
  CodeTree.Align:= alClient;
  CodeTree.Themed:= true;
  CodeTree.Tree.OnClick:= @DoCodetree_OnClick;
  CodeTree.Tree.OnDblClick:= @DoCodetree_OnDblClick;
  CodeTree.Tree.OnMouseMove:= @DoCodetree_OnMouseMove;
  CodeTree.Tree.OnKeyDown:= @DoCodetree_OnKeyDown;
  CodeTree.Tree.OnContextPopup:= @DoCodetree_OnContextPopup;
  CodeTree.Tree.OnAdvancedCustomDrawItem:=@DoCodetree_OnAdvDrawItem;

  PanelCodeTreeTop:= TPanel.Create(PanelCodeTreeAll);
  PanelCodeTreeTop.Name:= 'PanelCodeTreeTop';
  DoControl_InitPropsObject(PanelCodeTreeTop, PanelCodeTreeAll, 'panel');
  PanelCodeTreeTop.Parent:= PanelCodeTreeAll;
  PanelCodeTreeTop.Align:= alTop;
  PanelCodeTreeTop.BorderStyle:= bsNone;
  PanelCodeTreeTop.BevelInner:= bvNone;
  PanelCodeTreeTop.BevelOuter:= bvNone;
  PanelCodeTreeTop.Height:= UiOps.InputHeight;

  CodeTreeFilter:= TTreeFilterEdit.Create(PanelCodeTreeAll);
  CodeTreeFilter.Name:= 'CodeTreeFilter';
  CodeTreeFilter.OnFilterNode:= @CodeTreeFilter_OnFilterNode;
  DoControl_InitPropsObject(CodeTreeFilter, PanelCodeTreeAll, 'tree_filter_edit');
  CodeTreeFilter.Hide;

  {
  CodeTreeFilterReset:= TATButton.Create(PanelCodeTreeAll);
  CodeTreeFilterReset.Name:= 'CodeTreeFilterReset';
  DoControl_InitPropsObject(CodeTreeFilterReset, PanelCodeTreeAll, 'button_ex');
  CodeTreeFilterReset.Parent:= PanelCodeTreeTop;
  CodeTreeFilterReset.Align:= alRight;
  CodeTreeFilterReset.Width:= ATScrollbarTheme.InitialSize;
  CodeTreeFilterReset.Caption:= '';
  CodeTreeFilterReset.Arrow:= true;
  CodeTreeFilterReset.ArrowKind:= abakCross;
  CodeTreeFilterReset.Focusable:= false;
  CodeTreeFilterReset.ShowHint:= true;
  CodeTreeFilterReset.Hint:= msgTooltipClearFilter;
  CodeTreeFilterReset.OnClick:= @CodeTreeFilter_ResetOnClick;
  }

  CodeTreeFilterInput:= TATComboEdit.Create(PanelCodeTreeAll);
  CodeTreeFilterInput.Name:= 'CodeTreeFilterInput';
  DoControl_InitPropsObject(CodeTreeFilterInput, PanelCodeTreeAll, 'editor_combo');
  CodeTreeFilterInput.Parent:= PanelCodeTreeTop;
  CodeTreeFilterInput.Align:= alClient;
  CodeTreeFilterInput.Keymap:= AppKeymapMain;
  CodeTreeFilterInput.OnChange:= @CodeTreeFilter_OnChange;
  CodeTreeFilterInput.OnCommand:= @CodeTreeFilter_OnCommand;
  CodeTreeFilterInput.OnKeyDown:= @CodeTreeFilter_OnKeyDown;
end;

procedure TfmMain.InitStatusbar;
begin
  Status:= TATStatus.Create(Self);
  Status.Parent:= Self;
  Status.Align:= alBottom;
  Status.Top:= Height;
  Status.ScaleFromFont:= true; //statusbar is autosized via its font size
  Status.OnPanelClick:= @StatusPanelClick;
  Status.ShowHint:= true;
  Status.Theme:= @AppThemeStatusbar;
end;

procedure TfmMain.InitGroups;
begin
  Groups:= TATGroups.Create(Self);
  Groups.Parent:= PanelEditors;
  Groups.Align:= alClient;
  Groups.Mode:= gmOne;
  Groups.Images:= ImageListTabs;
  Groups.OnChangeMode:=@DoGroupsChangeMode;
  Groups.OnTabFocus:= @DoOnTabFocus;
  Groups.OnTabAdd:= @DoOnTabAdd;
  Groups.OnTabClose:= @DoOnTabClose;
  Groups.OnTabMove:= @DoOnTabMove;
  Groups.OnTabPopup:= @DoOnTabPopup;
  //Groups.OnTabOver:= @DoOnTabOver;
  Groups.OnTabGetTick:= @DoOnTabGetTick;
  Groups.OnTabDblClick:= @DoOnTabDblClick;
end;

procedure TfmMain.InitFinder;
begin
  FFinder:= TATEditorFinder.Create;
  FFinder.OnConfirmReplace:= @FinderOnConfirmReplace;
  FFinder.OnProgress:= @FinderOnProgress;
  FFinder.OnFound:=@FinderOnFound;
  FFinder.OnGetToken:= @FinderOnGetToken;
  FFinder.OnWrapAtEdge:= @FinderOnWrapAtEdge;
end;

procedure TfmMain.InitBookmarkSetup;
var
  i: integer;
begin
  AppBookmarkImagelist.AddImages(ImageListBm);
  for i:= 2 to 9 do
  begin
    AppBookmarkSetup[i].Color:= clDefault;
    AppBookmarkSetup[i].ImageIndex:= i-1;
  end;
end;

procedure TfmMain.UpdateMenuTheming_WhiteLine;
{$ifdef windows}
//fix white line under the menubar, with MenuStyler on
var
  Prev: WNDPROC;
{$endif}
begin
  {$ifdef windows}
  Prev:= Windows.WNDPROC(SetWindowLongPtrW(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback)));
  if not Assigned(PrevWndProc) then
    PrevWndProc:= Prev;
  {$endif}
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  UpdateMenuTheming_WhiteLine;

  //default "ui_scale":0 must be converted to Screen's DPI
  ATEditorScalePercents:= Max(100, 100*Screen.PixelsPerInch div 96);
  ATSynEdit_Finder.MsgBox_InFinder:= @MsgBox;

  OnEnter:= @FormEnter;
  TimerCmd.Interval:= UiOps.CommandTimerInterval;
  mnuHelpCheckUpd.Enabled:= UiOps.AllowProgramUpdates;
  AppRunAutocomplete:= @DoAutoComplete_Callback;

  with AppPanels[TAppPanelId.Side] do
  begin
    PanelRoot:= Self.PanelMain;
    Toolbar:= ToolbarSideTop;
    DefaultPanel:= msgPanelTree_Init;
    OnBeforeToggle:= @DoSidebar_OnBeforeToggle;
    OnAfterToggle:= @DoSidebar_OnAfterToggle;
    OnCommand:= @DoSidebar_OnPythonCall;
    OnCloseFloatForm:= @DoSidebar_OnCloseFloatForm;
    OnGetTranslatedTitle:= @DoSidebar_GetFormTitle;
    Init(Self, alLeft);
    Splitter.OnPaint:= @SplitterOnPaintDummy;
  end;

  with AppPanels[TAppPanelId.Btm] do
  begin
    PanelRoot:= Self.PanelAll;
    Toolbar:= ToolbarSideLow;
    OnBeforeToggle:= @DoBottom_OnBeforeToggle;
    OnAfterToggle:= @DoBottom_OnAfterToggle;
    OnCommand:= @DoSidebar_OnPythonCall;
    OnCloseFloatForm:= @DoBottom_OnCloseFloatForm;
    OnGetTranslatedTitle:= @DoSidebar_GetFormTitle;
    Init(Self, alBottom);
    Splitter.OnPaint:= @SplitterOnPaintDummy;
  end;

  {
  LexerProgress:= TATGauge.Create(Self);
  LexerProgress.Parent:= Status;
  }

  EControlOptions.OnLexerParseProgress:= @DoOnLexerParseProgress;
  AppCustomDialog_DoPyCallback:= @DoPyCallbackFromAPI;
  AppCustomDialog_OnEditorCommand:= @FrameOnEditorCommand;

  DoMenuitemEllipsis(mnuOpThemeUi);
  DoMenuitemEllipsis(mnuOpThemeSyntax);
  //DoMenuitemEllipsis(mnuOpKeys);
  DoMenuitemEllipsis(mnuOpThemes);
  DoMenuitemEllipsis(mnuOpLangs);

  PopupToolbarCase:= TPopupMenu.Create(Self);
  PopupToolbarCase.OnPopup:= @PopupToolbarCaseOnPopup;

  PopupToolbarComment:= TPopupMenu.Create(Self);
  PopupToolbarComment.OnPopup:= @PopupToolbarCommentOnPopup;

  {$ifdef windows}
  if not AppAlwaysNewInstance and IsSetToOneInstance then
    with TInstanceManage.GetInstance do
      case Status of
        isFirst:
          begin
            SetFormHandleForActivate(Self.Handle);
            OnSecondInstanceSentData := @SecondInstance;
          end;
      end;
  {$endif}

  FBoundsMain:= Rect(100, 100, 900, 700);;
  AppPanels[TAppPanelId.Side].FormFloatBounds:= Rect(650, 50, 900, 700);
  AppPanels[TAppPanelId.Btm].FormFloatBounds:= Rect(50, 480, 900, 700);
  FBoundsFloatGroups1:= Rect(300, 100, 800, 700);
  FBoundsFloatGroups2:= Rect(320, 120, 820, 720);
  FBoundsFloatGroups3:= Rect(340, 140, 840, 740);

  InitAppleMenu;
  InitToolbar;
  InitCodeTree;
  InitBottomEditor(fmOutput);
  InitBottomEditor(fmValidate);
  InitConsole(Self, @FindDialogGetMainEditor);
  fmConsole.OnConsoleNav:= @DoPyEvent_ConsoleNav;
  fmConsole.OnConsoleComplete:= @DoPyEvent_ConsoleComplete;
  fmConsole.OnNumberChange:= @DoOnConsoleNumberChange;
  fmConsole.OnKeyDown:=@DoOnConsoleKeyDown;
  InitSidebar; //after initing PanelCodeTreeAll, EditorOutput, EditorValidate, fmConsole
  InitBookmarkSetup;

  FMenuVisible:= true;

  InitStatusbar;
  InitGroups;
  InitFinder;

  FFindStop:= false;
  FFindConfirmAll:= mrNone;

  FLastDirOfOpenDlg:= '';
  FLastLexerForPluginsMenu:= '-';
  FLastTooltipLine:= -1;

  UpdateMenuItemHint(mnuFile, 'top-file');
  UpdateMenuItemHint(mnuEdit, 'top-edit');
  UpdateMenuItemHint(mnuSel, 'top-sel');
  UpdateMenuItemHint(mnuFind, 'top-sr');
  UpdateMenuItemHint(mnuView, 'top-view');
  UpdateMenuItemHint(mnuOp, 'top-op');
  UpdateMenuItemHint(mnuHelp, 'top-help');
  UpdateMenuItemHint(mnuGroups, 'top-groups');
  UpdateMenuItemHint(mnuPlugins, 'plugins');
  UpdateMenuItemHint(mnuFileOpenSub, '_recents');
  UpdateMenuItemHint(mnuFileEnds, '_ends');
  UpdateMenuItemHint(mnuOpPlugins, '_oplugins');
  mnuFileOpenSub.Parent.OnClick:= @MenuRecentsPopup;

  DoOps_OnCreate;

  //option is applied only once at app start
  if not UiOps.ShowMenubar then
    ShowMenu:= false;
end;

procedure InitAdditionalCommandLineOptions(
  out AWindowPos: string;
  out AAllowSessionLoad, AAllowSessionSave: TAppAllowSomething;
  out AStartupCommand: string;
  out AFileFolderCount: integer);
  forward;

procedure TfmMain.DoOps_OnCreate;
begin
  //must load window position in OnCreate to fix flickering with maximized window, Win10
  InitAdditionalCommandLineOptions(
    FOption_WindowPos,
    FOption_AllowSessionLoad,
    FOption_AllowSessionSave,
    FOption_StartupCommand,
    FCmdlineFileCount);
  FLastLoadedConfig:= 'user';
  DoOps_LoadOptions(AppFile_OptionsUser, EditorOps, true); //before LoadHistory
  DoFileOpen('', '', nil, '/noevent /noopenedevent /nononeevent'); //before LoadHistory

  DoOps_LoadToolbarIcons;
  DoOps_LoadSidebarIcons; //before LoadPlugins (for sidebar icons)

  InitPyEngine; //before LoadPlugins

  if not AppManagerThread.Finished then
    AppManagerThread.WaitFor; //before LoadPlugins (for LexerDetecter)

  DoOps_LoadPlugins(false); //before LoadHistory (for on_open for restored session)
  DoOps_LoadHistory;
end;

procedure TfmMain.DoCloseAllTabs_Old;
//deprecated in Cud 1.178, to delete later
var
  Pages: TATPages;
  Tabs: TATTabs;
  nGroup, nTab: integer;
begin
  for nGroup:= 0 to cAppMaxGroup do
  begin
    Pages:= TGroupsHelper.GetPagesOfGroupIndex(nGroup);
    if Pages=nil then Continue;
    if not Pages.Visible then Continue;
    Tabs:= Pages.Tabs;
    for nTab:= Tabs.TabCount-1 downto 0 do
      Tabs.DeleteTab(nTab, true{AllowEvent}, false{AWithCancelBtn}, aocNone);
  end;
end;

function TfmMain.DoCloseAllTabs(AClosePinned: boolean): boolean;
var
  List: array of TATGroups;
  i: integer;
begin
  Result:= false;

  AppClosingTabs:= true;

  List:= [Groups, GroupsF1, GroupsF2, GroupsF3];
  for i:= High(List) downto 0 do
    if Assigned(List[i]) then
      if not List[i].CloseTabs(tabCloseAll, false, AClosePinned) then
      begin
        AppClosingTabs:= false;
        exit;
      end;

  //better free deleted frames sooner, #4632
  AppUpdateWatcherFrames(8000);

  AppClosingTabs:= false;
  Result:= true;
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  F: TEditorFrame;
  bAddTabsToRecents: boolean;
  i: integer;
begin
  if Assigned(AppNotifThread) then
  begin
    AppNotifThread.Terminate;
    Sleep(cThreadSleepTime+10);
  end;

  bAddTabsToRecents:= not UiOps.ReopenSession;

  //save history of all frames,
  //update recent-files list
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];

    if bAddTabsToRecents then
    begin
      UpdateMenuRecent(F.Ed1);
      if not F.EditorsLinked then
        UpdateMenuRecent(F.Ed2);
    end;

    //on_close are not fired automatically on app exit
    //(because we don't really close tabs on exit), so fire it here
    DoPyEvent(F.Ed1, TAppPyEvent.OnClose, []);
    if not F.EditorsLinked then
      DoPyEvent(F.Ed2, TAppPyEvent.OnClose, []);
  end;

  DoPyEvent(nil, TAppPyEvent.OnExit, []);

  //after UpdateMenuRecent
  //and after on_exit, so plugin can close its side-panels in on_exit, and panel will be hidden on next app start
  DoOps_SaveHistory(UiOps.SaveModifiedTabsOnClose);

  {
  //seems doing DoCloseAllTabs in FormClose is bad idea:
  //app asks to save modified tabs, even with UiOps.SessionSaveOnExit.
  AppSessionIsClosing:= true; //to avoid asking "Close pinned tab?"
  DoCloseAllTabs;
  }

  {$ifdef LCLGTK2}
  FixClipboardFinalization;
  {$endif}
end;

procedure TfmMain.ButtonCancelClick(Sender: TObject);
begin
  FFindStop:= true;
end;

procedure TfmMain.FormActivate(Sender: TObject);
begin
  AppActiveForm:= Sender;
  UpdateTree(true);
end;

procedure TfmMain.FormChangeBounds(Sender: TObject);
begin
  CloseFormAutoCompletion;
  DoTooltipHide;
end;

procedure TfmMain.DoPyEvent_AppState(AState: integer);
begin
  DoPyEvent(nil, TAppPyEvent.OnState, [AppVariant(AState)]);
end;

procedure TfmMain.DoPyEvent_EdState(Ed: TATSynEdit; AState: integer);
begin
  DoPyEvent(Ed, TAppPyEvent.OnStateEd, [AppVariant(AState)]);
end;

procedure TfmMain.DoPyEvent_AppActivate(AEvent: TAppPyEvent);
var
  Tick: QWord;
begin
  Tick:= GetTickCount64;
  if (Tick-FLastAppActivate)>500 then //workaround for too many calls in LCL, on Ubuntu 20.04
  begin
    FLastAppActivate:= Tick;
    DoPyEvent(nil, AEvent, []);
  end;
end;

procedure TfmMain.DoPyEvent_Open(Ed: TATSynEdit);
begin
  DoPyEvent(Ed, TAppPyEvent.OnOpen, []);
end;

procedure TfmMain.DoPyEvent_OpenNone(Ed: TATSynEdit);
begin
  DoPyEvent(Ed, TAppPyEvent.OnOpenNone, []);
end;

procedure TfmMain.AppPropsActivate(Sender: TObject);
var
  F: TEditorFrame;
begin
  //Caption:= 'act '+TimeToStr(Now);

  if EditorOps.OpDimUnfocused<>0 then
  begin
    F:= CurrentFrame;
    if Assigned(F) then
      F.Editor.Update;
  end;

  DoPyEvent_AppActivate(TAppPyEvent.OnAppActivate);
end;

procedure TfmMain.AppPropsDeactivate(Sender: TObject);
var
  F: TEditorFrame;
begin
  if EditorOps.OpDimUnfocused<>0 then
  begin
    F:= CurrentFrame;
    if Assigned(F) then
      F.Editor.Update;
  end;

  {
  //it was needed when autocomplete was non-docked window, to hide autocomplete from Alt+Tab
  CloseFormAutoCompletion;
  }

  DoPyEvent_AppActivate(TAppPyEvent.OnAppDeactivate);
end;

procedure TfmMain.AppPropsDropFiles(Sender: TObject;
  const FileNames: array of string);
{$ifdef darwin}
var
  i: integer;
{$endif}
begin
  {$ifdef darwin}
  SetLength(AppDroppedFiles, Length(FileNames));
  for i:= 0 to Length(FileNames)-1 do
    AppDroppedFiles[i]:= FileNames[i];
  {$endif}
end;

procedure TfmMain.AppPropsEndSession(Sender: TObject);
begin
  //
end;

procedure TfmMain.AppPropsModalBegin(Sender: TObject);
begin
  CloseFormAutoCompletion;
end;

procedure TfmMain.AppPropsQueryEndSession(var Cancel: Boolean);
var
  FAction: TCloseAction;
begin
  FAction:= caFree;
  FormClose(Self, FAction);
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var ACanClose: boolean);
var
  F: TEditorFrame;
  i: integer;
begin
  //call on_close_pre for all tabs, it's needed to save all
  //tabs by AutoSave plugin
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    DoPyEvent(F.Ed1, TAppPyEvent.OnCloseBefore, []);
    if not F.EditorsLinked then
      DoPyEvent(F.Ed2, TAppPyEvent.OnCloseBefore, []);
  end;

  if GetModifiedCount>0 then
    ACanClose:= (
      UiOps.ReopenSession and
      UiOps.SessionSaveOnExit and
      UiOps.HistoryItems[TAppHistoryElement.Text]
      )
      or DoDialogSaveTabs
  else
    ACanClose:= true;

  if ACanClose then
    StopAllTimers;
end;

procedure TfmMain.StopAllTimers;
begin
  TimerAppIdle.AutoEnabled:=false;
  TimerStatusClear.Enabled:= false;
  TimerStatusWork.Enabled:= false;
  TimerTooltip.Enabled:= false;
  TimerTreeFill.Enabled:= false;
  TimerAppIdle.Enabled:= false;
  TimerCmd.Enabled:= false;
end;

procedure TfmMain.UpdateSidebarButtonOverlay;
var
  Btn: TATButton;
  NCount, i: integer;
begin
  for i:= 0 to ToolbarSideLow.ButtonCount-1 do
  begin
    Btn:= ToolbarSideLow.Buttons[i];
    if Btn.Caption=msgPanelValidate_Init then
    begin
      NCount:= fmValidate.Ed.Strings.Count-1;
      if NCount>0 then
        Btn.TextOverlay:= IntToStr(NCount)
      else
        Btn.TextOverlay:= '';
    end
    else
    if Btn.Caption=msgPanelOutput_Init then
    begin
      NCount:= fmOutput.Ed.Strings.Count-1;
      if NCount>0 then
        Btn.TextOverlay:= IntToStr(NCount)
      else
        Btn.TextOverlay:= '';
    end
    else
    if Btn.Caption=msgPanelConsole_Init then
    begin
      if Assigned(fmConsole) then
        NCount:= fmConsole.ErrorCounter
      else
        NCount:= 0;
      if NCount>0 then
        Btn.TextOverlay:= IntToStr(NCount)
      else
        Btn.TextOverlay:= '';
    end;
  end;
end;

procedure TfmMain.FormColorsApply(const AColors: TAppTheme; AThemeUI: boolean);
begin
  AppTheme:= AColors;
  DoClearLexersAskedList;
  DoApplyTheme;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  CloseFormAutoCompletion;

  AppStopListTimers;

  FreeAndNil(FPrevJsonObj);

  if Assigned(FFinder) then
    FreeAndNil(FFinder);
end;

procedure TfmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
const
  cStepsForProgress = 40;
var
  SName, SOpenOptions, SOpenOptionsAll, SOpenOptionsActive: string;
  Pages: TATPages;
  i: integer;
begin
  if not IsAllowedToOpenFileNow then exit;
  AppDroppingFiles:= true;

  //MS WordPad, Notepad++ - they get focus on drag-drop from Explorer
  Application.BringToFront;

  //set group according to mouse cursor
  Pages:= TGroupsHelper.FindPagesUnderCursorPos(Mouse.CursorPos, Groups);

  SOpenOptionsActive:= GetFileOpenOptionsString(1); //options without '/passive'
  SOpenOptionsAll:= GetFileOpenOptionsString(Length(FileNames)); //options with '/passive' if count>1

  for i:= 0 to Length(FileNames)-1 do
  begin
    if Application.Terminated then exit;
    SName:= FileNames[i];

    //if dropped N>1 files, open last file as active
    if (i>0) and (i=Length(FileNames)-1) then
    begin
      SOpenOptions:= SOpenOptionsActive;
      Application.ProcessMessages; //required to make visible last ui-tab after opening last file
    end
    else
      SOpenOptions:= SOpenOptionsAll;

    if DirectoryExists(SName) then
      DoFolderOpen(SName, False, TATCommandInvoke.AppDragDrop)
    else
    if FileExists(SName) then
      DoFileOpen(SName, '', Pages, SOpenOptions);

    if i mod cStepsForProgress = cStepsForProgress-1 then
      Application.ProcessMessages;
  end;

  AppDroppingFiles:= false;
end;

procedure TfmMain.FormFloatGroups_OnDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  SName: string;
  CurForm: TCustomForm;
  Gr: TATGroups;
  i: integer;
begin
  if not IsAllowedToOpenFileNow then exit;

  //MS WordPad, Notepad++ - they get focus on drag-drop from Explorer
  Application.BringToFront;

  CurForm:= Sender as TForm;
  if CurForm=FFormFloatGroups1 then
  begin
    Gr:= GroupsF1;
  end
  else
  if CurForm=FFormFloatGroups2 then
  begin
    Gr:= GroupsF2;
  end
  else
  if CurForm=FFormFloatGroups3 then
  begin
    Gr:= GroupsF3;
  end
  else
  raise Exception.Create('Unknown floating group form');

  for i:= 0 to Length(Filenames)-1 do
  begin
    SName:= FileNames[i];
    if DirectoryExists(SName) then
      DoFolderOpen(SName, False, TATCommandInvoke.AppDragDrop)
    else
    if FileExists(SName) then
      DoFileOpen(SName, '', Gr.Pages[0]);
  end;
end;

procedure TfmMain.FormFloatGroups_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FormAutoCompletion) and FormAutoCompletion.Visible then
  begin
    FormAutoCompletion.FormKeyDown(Sender, Key, Shift);
    exit;
  end;
end;

procedure TfmMain.FormFloatGroups_OnUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if Assigned(FormAutoCompletion) and FormAutoCompletion.Visible then
  begin
    FormAutoCompletion.FormUTF8KeyPress(Sender, UTF8Key);
    exit;
  end;
end;

procedure TfmMain.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if Assigned(FormAutoCompletion) and FormAutoCompletion.Visible then
  begin
    FormAutoCompletion.FormUTF8KeyPress(Sender, UTF8Key);
    exit;
  end;
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  bEditorActive,
  bConsoleActive,
  bFindDockedAndVisible: boolean;
  Ed: TATSynEdit;
  Ctl: TWinControl;
  KeyArray: TATKeyArray;
  N: integer;
begin
  if Assigned(FormAutoCompletion) and FormAutoCompletion.Visible then
  begin
    FormAutoCompletion.FormKeyDown(Sender, Key, Shift);
    exit;
  end;

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    PyEscapeFlag:= true;
    if AppPython.IsRunning then
    begin
      Key:= 0;
      exit
    end;

    Ctl:= ActiveControl;
    bEditorActive:=
      (Ctl is TATSynEdit) and
      Assigned(TATSynEdit(Ctl).ParentFrameObject);
    bConsoleActive:=
      Assigned(fmConsole) and
      fmConsole.Visible and
      (fmConsole.EdInput.Focused or
       fmConsole.EdMemo.Focused);
    bFindDockedAndVisible:=
      Assigned(fmFind) and
      fmFind.Visible and
      Assigned(fmFind.Parent);

    DoTooltipHide;

    if not bEditorActive or bConsoleActive then
    begin
      DoFocusEditor(CurrentEditor);
      if bConsoleActive then
      begin
        if UiOps.EscapeCloseConsole then
        begin
          AppPanels[TAppPanelId.Btm].Visible:= false;
          UpdateMenuChecks_Global;
        end;
      end
      else
      Key:= 0;
    end
    else
    if bFindDockedAndVisible then
    begin
      Ed:= Ctl as TATSynEdit;
      if UiOps.EscapeCloseFinder then
      begin
        EditorClearHiAllMarkers(Ed);
        fmFind.Hide;
      end
      else
        Ed.SetFocus;
      Key:= 0;
    end
    else
    if bEditorActive and UiOps.EscapeClose then
    begin
      //Esc pressed when it's assigned to 'cancel selection'? don't close
      if bEditorActive then
      begin
        Ed:= Ctl as TATSynEdit;
        if (Ed.Carets.Count>1) or Ed.Carets.IsSelection then
        begin
          KeyArray.Clear;
          N:= Ed.Keymap.GetCommandFromShortcut(ShortCut(VK_ESCAPE, []), KeyArray);
          case N of
            cCommand_Cancel,
            cCommand_CancelKeepLast,
            cCommand_SelectNone:
              exit;
          end;
        end;
      end;

      Close;
      Key:= 0;
    end;

    exit
  end;

  if (Key=VK_TAB) and (Shift<>[]) then
  begin
    Ed:= CurrentEditor;
    if Assigned(Ed) then
    begin
      KeyArray.Clear;
      N:= Ed.Keymap.GetCommandFromShortcut(ShortCut(Key, Shift), KeyArray);
      if N>=0 then
        Ed.DoCommand(N, TATCommandInvoke.Hotkey);
    end;
    Key:= 0;
    exit;
  end;

  //allow F12 keypress from Project Manager when main menu is hidden
  //check Sender=nil to allow F-keys only when called from another form, to not block key-combos with F-keys
  if (Key>=VK_F1) and (Key<=VK_F24) and (Sender=nil) then
  begin
    Ed:= CurrentEditor;
    if Assigned(Ed) then //we may here check Ed.Focused to not block key-combos with F-keys
    begin
      KeyArray.Clear;
      N:= Ed.Keymap.GetCommandFromShortcut(ShortCut(Key, Shift), KeyArray);
      if N>=0 then
      begin
        Ed.DoCommand(N, TATCommandInvoke.Hotkey);
        Key:= 0;
      end;
    end;
    exit;
  end;
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  FixMainLayout;
end;

procedure TfmMain.FixMainLayout;
begin
  //issue #1814
  AppPanels[TAppPanelId.Side].UpdateSplitter;
  AppPanels[TAppPanelId.Btm].UpdateSplitter;

  //issue #4249
  if AppPanels[TAppPanelId.Btm].Visible then
    Constraints.MinHeight:= Min(Height, AppPanels[TAppPanelId.Btm].PanelSize+200)
  else
    Constraints.MinHeight:= 200; //like in form designer
end;

procedure TfmMain.DoApplyCli(const ACliModule: string; const ACliParams: TAppStringArray);
var
  Params: TAppVariantArray;
  i: integer;
begin
  if ACliModule<>'' then
  begin
    Params:= nil;
    SetLength(Params, Length(ACliParams));
    for i:= 0 to High(ACliParams) do
      Params[i]:= AppVariant(ACliParams[i]);

    MsgStdout(Format('Calling on_cli for "%s" with %d params', [ACliModule, Length(ACliParams)]));
    DoPyCommand(ACliModule, cAppPyEvent[TAppPyEvent.OnCLI], Params, TATCommandInvoke.AppAPI);
  end;
end;

procedure TfmMain.FormShow(Sender: TObject);
  //
  procedure _Init_FixSplitters;
  // https://bugs.freepascal.org/view.php?id=35599
  // it's needed for macOS and Win10
  var
    id: TAppPanelId;
    ResizeStyle: TResizeStyle;
  begin
    if UiOps.SplittersUsePoorStyle then
      ResizeStyle:= rsPattern
    else
      ResizeStyle:= rsUpdate;

    for id:= Low(id) to High(id) do
      if id<>TAppPanelId.None then
        with AppPanels[id] do
          Splitter.ResizeStyle:= ResizeStyle;

    Groups.Splitter1.ResizeStyle:= ResizeStyle;
    Groups.Splitter2.ResizeStyle:= ResizeStyle;
    Groups.Splitter3.ResizeStyle:= ResizeStyle;
    Groups.Splitter4.ResizeStyle:= ResizeStyle;
    Groups.Splitter5.ResizeStyle:= ResizeStyle;
  end;
  //
  procedure _Init_DisableSomeMenuItems;
  begin
    {$ifdef LCLGTK2}
    if Assigned(mnuViewFloatSide) then
      mnuViewFloatSide.Enabled:= false;
    if Assigned(mnuViewFloatBottom) then
      mnuViewFloatBottom.Enabled:= false;
    {$endif}
  end;
  //
  procedure _Init_SidebarEvents;
  var
    id: TAppPanelId;
  begin
    for id in TAppPanelId do
      if id<>TAppPanelId.None then
        AppPanels[id].OnContextPopup:= @DoSidebar_OnContextPopup;
  end;
  //
  procedure _Init_WindowMaximized;
  begin
    if FLastMaximized then
    begin
      FLastMaximized:= false;
      {
      //code is Ok for Win32/Qt5, but gives fail on Gtk2: form is maximized incorrectly, form caption is invisible,
      //also see issue #5259
      if (FLastMaximizedMonitor>=0) and (FLastMaximizedMonitor<Screen.MonitorCount) then
        BoundsRect:= Screen.Monitors[FLastMaximizedMonitor].BoundsRect;
      }
      WindowState:= wsMaximized;
    end;
  end;
  //
  procedure _Init_ApiOnStart;
  begin
    DoPyEvent(nil, TAppPyEvent.OnStart, []);
    AppApiOnStartActivated:= true;
  end;
  //
  procedure _Init_KeymapMain;
  begin
    //load keymap-main
    //after loading plugins (to apply plugins keys)
    TKeymapHelper.SetHotkey(AppKeymapMain, 'cuda_comments,cmt_toggle_line_body|Ctrl+/|', false);
    TKeymapHelper.LoadConfig(AppKeymapMain, AppFile_Hotkeys, false);
  end;
  //
  procedure _Init_KeymapNoneForEmpty;
  var
    Frame: TEditorFrame;
  begin
    //load keymap for none-lexer (to initial empty frame)
    if FrameCount=1 then //session was not loaded
    begin
      Frame:= Frames[0];
      if Frame.IsEmpty then
        FrameLexerChange(Frame.Ed1);
    end;
  end;
  //
  procedure _Init_StartupSession;
  begin
    //load session
    //after on_start (so HTML Tooltips with on_open can work)
    //after loading keymap-main and keymap for none-lexer
    if //fixing #4239, don't check here IsDefaultSessionActive ////avoid if on_start runs ProjectManager which loaded project-session
       UiOps.ReopenSession and
       (FOption_AllowSessionLoad=TAppAllowSomething.Enable) then
      DoOps_LoadSession(AppFile_Session, false);
  end;
  //
  procedure _Init_FrameFocus;
  var
    Frame: TEditorFrame;
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
      Frame.SetFocus;
  end;
  //
  procedure _Init_ShowStartupTimes;
  var
    NTick: QWord;
  begin
    NTick:= GetTickCount64;
    MsgLogConsole(Format(
      'Startup: %dms, plugins: %s', [
      (NTick-AppTickInitial) div 10 * 10,
      AppPython.GetTimingReport
      ]));

    {
    MsgLogConsole('Toolbar updates: '+
      IntToStr(AppPanels[cPaneSide].ToolbarUpdateCount + AppPanels[cPaneOut].ToolbarUpdateCount)+' times, '+
      IntToStr(AppPanels[cPaneSide].ToolbarUpdateTime + AppPanels[cPaneOut].ToolbarUpdateTime)+'ms');
      }
  end;
  //
  procedure _Init_CheckExePath;
  {$ifdef windows}
  const
    BadStr: PChar = 'c:\Program Files';
  {$endif}
  begin
    {$ifdef windows}
    if strlicomp(PChar(Application.ExeName), BadStr, Length(BadStr))=0 then
      MsgLogConsole('ERROR: CudaText cannot save configs if it''s copied to "Program Files" folder');
    {$endif}
  end;
  //
  procedure _Init_ForceRepaintEditor;
  var
    Frame: TEditorFrame;
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
    begin
      EditorForceUpdateIfWrapped(Frame.Ed1);
      if Frame.Splitted then
        EditorForceUpdateIfWrapped(Frame.Ed2);
    end;
  end;
  //
  procedure _Init_ShortcutsForCustomizedMainMenu;
  //var
  //  tick: QWord;
  begin
    //tick:= GetTickCount64;

    //main menu is not customized?
    if (MainMenu.Items.Count>3) and
      (MainMenu.Items[0]=mnuFile) and
      (MainMenu.Items[1]=mnuEdit) and
      (MainMenu.Items[2]=mnuSel) and
      (MainMenu.Items[MainMenu.Items.Count-1]=mnuGroups) then
      exit;

    UpdateMenuItem_SetShortcutsRecursively(MainMenu.Items, 2);

    //tick:= GetTickCount64-tick;
    //MsgLogConsole('Init top menu shortcuts: '+IntToStr(tick)+'ms');
  end;
  //
var
  Frame: TEditorFrame;
begin
  _Init_FixSplitters;
  _Init_DisableSomeMenuItems;
  _Init_SidebarEvents;

  if FHandledOnShowPartly then exit;

  DoApplyInitialGroupSizes; //before FormLock to solve bad group-splitters pos, issue #3067
  FormLock(Self);

  DoApplyFont_Text;
  DoApplyFont_Ui;

  fmConsole.EdInput.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);
  if Assigned(CodeTreeFilterInput) then
  begin
    CodeTreeFilterInput.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);
    EditorCaretShapeFromString(CodeTreeFilterInput.CaretShapeNormal, EditorOps.OpCaretViewNormal);
    EditorCaretShapeFromString(CodeTreeFilterInput.CaretShapeOverwrite, EditorOps.OpCaretViewOverwrite);
  end;

  if FConsoleMustShow then
    DoShowConsole(false);

  FHandledOnShowPartly:= true;

  _Init_ApiOnStart;
  _Init_KeymapMain;
  _Init_KeymapNoneForEmpty;
  _Init_StartupSession;
  _Init_WindowMaximized; //after StartupSession to fix #4219

  //after on_start, ConfigToolbar is slow with visible toolbar
  DoApplyUiOps;
  DoApplyInitialSidebarPanel;

  AppPanels[TAppPanelId.Side].UpdateButtons;
  AppPanels[TAppPanelId.Btm].UpdateButtons;
  UpdateStatusbar;
  DoApplyInitialWindowPos;

  if AppPanels[TAppPanelId.Btm].Visible then
    if AppPanels[TAppPanelId.Btm].LastActivePanel='' then
      DoShowConsole(false);
  FormUnlock(Self);
  DoLoadCommandLine; //after FormUnlock, to fix #4445

  //postpone parsing until frames are shown
  AppAllowFrameParsing:= true;
  DoShowForVisibleFrames;

  FHandledUntilFirstFocus:= true;

  _Init_FrameFocus;
  _Init_ShowStartupTimes;

  AppPython.DisableTiming;
  ShowWelcomeInfo;

  if UiOps.NewdocLexer<>'' then
    if FrameCount=1 then
    begin
      Frame:= Frames[0];
      if Frame.IsEmpty and (Frame.LexerName[Frame.Ed1]='') then
        DoApplyNewdocLexer(Frame);
    end;

  if UiOps.NotificationEnabled then
  begin
    AppNotifThread:= TAppNotifThread.Create(false);
    AppNotifThread.Priority:= tpLower;
  end;

  FHandledOnShowFully:= true;
  FNeedUpdateMenuChecks:= true;
  FNeedUpdateMenuPlugins:= true;

  _Init_CheckExePath;

  //fix wrong caret/staples pos, #4559
  _Init_ForceRepaintEditor;

  _Init_ShortcutsForCustomizedMainMenu;

  if not FHandledOnStart2 then
  begin
    FHandledOnStart2:= true;
    DoPyEvent(nil, TAppPyEvent.OnStart2, []);
  end;

  AppFormShowCompleted:= true;
  if Assigned(fmConsole) then
    fmConsole.FlushConsole;
end;

procedure TfmMain.FormWindowStateChange(Sender: TObject);
begin
  DoPyEvent_AppState(APPSTATE_WINDOW);
end;

procedure TfmMain.ShowWelcomeInfo;
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  SText: string;
begin
  if not FileExists(AppFile_History) then
  begin
    Frame:= Frames[0];
    if Frame.IsEmpty then
    begin
      Ed:= Frame.Ed1;
      Frame.TabCaption:= msgWelcomeTabTitle;
      Frame.TabCaptionReason:= TAppTabCaptionReason.UnsavedSpecial;
      SText:= msgFirstStartInfo;
      if not AppPython.Inited then
        SText+= #10+msgCannotInitPython1+#10+msgCannotInitPython2+#10+msgCannotInitPython2b;
      Ed.Strings.LoadFromString(SText);
      Ed.Modified:= false;
      Frame.InSession:= false;
      Frame.InHistory:= false;
    end;
  end;
end;

procedure TfmMain.FrameAddRecent(Sender: TObject);
begin
  UpdateMenuRecent(Sender as TATSynEdit);
end;

procedure TfmMain.FrameOnEditorChangeCaretPos(Sender: TObject);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  if AppCodetreeState.DblClicking then exit;
  AppCodetreeState.NeedsSelJump:= true;

  Ed:= Sender as TATSynEdit;
  if Ed.Carets.Count>0 then
  begin
    Caret:= Ed.Carets[0];
    if (FLastTooltipLine>=0) and (Caret.PosY<>FLastTooltipLine) then
      DoTooltipHide;
  end;
end;

procedure TfmMain.FrameOnEditorScroll(Sender: TObject);
begin
  DoTooltipHide;
end;

procedure TfmMain.FrameOnMsgStatus(Sender: TObject; const AStr: string);
begin
  MsgStatus(AStr);
end;


procedure TfmMain.FrameOnEditorPaint(Sender: TObject);
//why we set Ed.OptTextCenteringCharWidth via OnPaint event?
//before it was as usual, but buggy - sometimes option was reset to 0 (without stable repro)
var
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  CurGrp: TATGroups;
  SLexer: string;
begin
  Ed:= TATSynEdit(Sender);
  Frame:= TGroupsHelper.GetEditorFrame(Ed);
  if Frame=nil then exit;
  CurGrp:= Frame.GetTabGroups;

  if CurGrp.Mode in [gmOne, gm2h, gm3h, gm4h, gm6h] then
  begin
    SLexer:= Frame.LexerName[Ed];
    if ShowDistractionFree then
    begin
      Ed.OptTextCenteringCharWidth:=
        AppOption_LoadFromStringlist(EditorOps_CenteringDistFree, SLexer, EditorOps.OpCenteringForDistractionFree);
    end
    else
    begin
      Ed.OptTextCenteringCharWidth:=
        AppOption_LoadFromStringlist(EditorOps_CenteringWidth, SLexer, EditorOps.OpCenteringWidth);
    end;
  end
  else
  begin
    Ed.OptTextCenteringCharWidth:= 0;
  end;
end;

procedure TfmMain.MenuRecentsClear(Sender: TObject);
begin
  DoOps_ClearConfigHistory([TAppConfigHistoryElement.RecentFiles]);
end;

procedure TfmMain.UpdatePlugins_AfterInstallingZip;
begin
  DoOps_LoadPlugins(true);
  UpdateMenuPlugins;
  UpdateMenuPlugins_Shortcuts(true);
  DoPyEvent(nil, TAppPyEvent.OnInitPluginsMenu, []);
end;

function TfmMain.DoFileInstallZip(const AFileName: string; out DirTarget: string;
  ASilent, AAllowUpdateAddons: boolean): boolean;
var
  msg, msg2: string;
  AddonType: TAppAddonType;
  bKeepFrameLexers: boolean;
  bNeedRestart: boolean;
begin
  bNeedRestart:= false;
  bKeepFrameLexers:= true;
  if bKeepFrameLexers then
    DoOps_LexersBackupSave;

  DoInstallAddonFromZip(AFileName, AppDir_DataAutocomplete, msg, msg2,
    Result, AddonType, DirTarget, bNeedRestart, ASilent);

  if Result then
  begin
    case AddonType of
      TAppAddonType.Lexer,
      TAppAddonType.LexerLite:
        begin
          if AAllowUpdateAddons then
            DoOps_LoadLexerLib(true); //AOnCreate=true - don't backup lexers
        end;

      TAppAddonType.Plugin:
        begin
          if AAllowUpdateAddons then
            UpdatePlugins_AfterInstallingZip;
        end;
    end;

    if not ASilent then
      DoDialogAddonInstalledReport(msg, msg2, bNeedRestart);
  end;

  if bKeepFrameLexers then
    DoOps_LexersBackupRestore;
end;


function TfmMain.GetModifiedCount: integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to FrameCount-1 do
    with Frames[i] do
      if Modified then
        Inc(Result);
end;

function TfmMain.GetShowStatus: boolean;
begin
  Result:= Status.Visible;
end;

function TfmMain.GetShowToolbar: boolean;
begin
  Result:= ToolbarMain.Visible;
end;

function TfmMain.GetShowSideBar: boolean;
begin
  Result:= PanelSide.Visible;
end;

function TfmMain.DoDialogSaveTabs: boolean;
var
  F: TEditorFrame;
  Form: TfmSaveTabs;
  SCaption: string;
  i: integer;
begin
  Result:= false;
  Form:= TfmSaveTabs.Create(nil);
  try
    Form.List.Clear;
    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      if not F.Modified then Continue;
      SCaption:= F.TabCaption;
      if F.FileName<>'' then
      begin
        if F.EditorsLinked then
          SCaption+= '  ('+AppCollapseHomeDirInFilename(ExtractFileDir(F.FileName))+')'
        else
          SCaption+= '  ('+AppCollapseHomeDirInFilename(ExtractFileDir(F.FileName))+' | '+
                           AppCollapseHomeDirInFilename(ExtractFileDir(F.FileName2))+')'
      end;
      Form.List.Items.AddObject(SCaption, F);
      Form.List.Checked[Form.List.Count-1]:= true;
    end;

    case Form.ShowModal of
      //"Don't save/ Keep in session"
      mrClose:
        begin
          Result:= true;
          //set true, because user can call this dialog via CmdPalette,
          //where he can choose "Don't save" before
          UiOps.SaveModifiedTabsOnClose:= true;
        end;
      //"Cancel"
      mrCancel:
        begin
          Result:= false;
        end;
      //"Don't save"
      mrNoToAll:
        begin
          Result:= true; //like for mrClose
          UiOps.SaveModifiedTabsOnClose:= false;
        end;
      //"Save"
      mrOk:
        begin
          Result:= true;
          for i:= 0 to Form.List.Count-1 do
            if Form.List.Checked[i] then
            begin
              F:= Form.List.Items.Objects[i] as TEditorFrame;
              if not F.DoFileSave(false, true) then
                exit(false);
            end;
        end;
    end;
  finally
    Form.Free
  end;
end;

procedure TfmMain.DoDialogLexerProp(an: TecSyntAnalyzer);
begin
  if DoShowDialogLexerProp(an,
    EditorOps.OpFontName,
    EditorOps.OpFontSize) then
  begin
    UpdateStatusbar;
    UpdateCurrentFrame;
  end;
end;

procedure TfmMain.DoDialogLexerLib;
begin
  if DoShowDialogLexerLib(
    AppDir_DataAutocomplete,
    EditorOps.OpFontName,
    EditorOps.OpFontSize,
    @DoOnDeleteLexer
    ) then
  begin
    UpdateStatusbar;
    UpdateCurrentFrame;
  end;
end;

procedure TfmMain.DoApplyLexerStylesMapsToFrames(AndApplyTheme: boolean);
var
  F: TEditorFrame;
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    F.ApplyLexerStyleMap;
    if AndApplyTheme then
    begin
      F.ApplyTheme;
      //update coloring of Markdown fenced-code-blocks, on applying syntax theme
      F.LexerReparse;
    end;
  end;
end;

procedure TfmMain.DoDialogLexerMap;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F=nil then exit;

  if DoDialogLexerStylesMap(F.Lexer[F.Editor]) then
    DoApplyLexerStylesMapsToFrames(false);
end;

procedure TfmMain.DoHelpAbout;
var
  Form: TfmAbout;
begin
  Form:= TfmAbout.Create(Self);
  try
    Form.labelVersion.Caption:= cAppExeVersion;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TfmMain.DoHelpForum;
begin
  OpenURL('http://synwrite.sourceforge.net/forums/viewforum.php?f=20');
end;

procedure TfmMain.UpdateFrameLineEnds(Frame: TEditorFrame; AValue: TATLineEnds);
begin
  if Assigned(Frame) then
  begin
    Frame.LineEnds[Frame.Editor]:= AValue;
    Frame.UpdateModified(Frame.Editor);
  end;
  UpdateStatusbar;
  MsgStatus(msgStatusEndsChanged);
end;

procedure TfmMain.UpdateEditorCaretLineEnds(Frame: TEditorFrame; Ed: TATSynEdit; AValue: TATLineEnds);
begin
  EditorChangeLineEndsForSelection(Ed, AValue);
  if Assigned(Frame) then
  begin
    Frame.UpdateModified(Ed);
    MsgStatus(msgStatusEndsChanged);
  end;
end;


procedure TfmMain.DoApplyUiOpsToGroups(G: TATGroups);
begin
  G.SetTabFont(Self.Font);
  G.SetTabOption(tabOptionScalePercents, ATEditorScalePercents);
  G.SetTabOption(tabOptionShowHint, 1);
  G.SetTabOption(tabOptionVarWidth, Ord(UiOps.TabVarWidth));
  G.SetTabOption(tabOptionMultiline, Ord(UiOps.TabMultiline));
  G.SetTabOption(tabOptionSpaceSide, IfThen(UiOps.TabAngled, 10, 0));
  G.SetTabOption(tabOptionSpaceInitial, IfThen(UiOps.TabAngled, 10, 4));
  G.SetTabOption(tabOptionSpaceBetweenTabs, IfThen(UiOps.TabAngled, 4, 0));
  G.SetTabOption(tabOptionShowFlat, Ord(UiOps.TabFlat));
  G.SetTabOption(tabOptionShowXButtons, UiOps.TabShowX);
  G.SetTabOption(tabOptionShowXRounded, Ord(UiOps.TabShowXRounded));
  G.SetTabOption(tabOptionShowPlus, Ord(UiOps.TabShowPlus and not UiOps.TabsDisabled));
  G.SetTabOption(tabOptionShowEntireColor, Ord(UiOps.TabColorFull));
  G.SetTabOption(tabOptionFontScale, UiOps.TabFontScale);
  G.SetTabOption(tabOptionDoubleClickClose, Ord(UiOps.TabDblClickClose));
  G.SetTabOption(tabOptionWidthNormal, UiOps.TabWidth);
  G.SetTabOption(tabOptionWidthMin, UiOps.TabWidthMin);
  G.SetTabOption(tabOptionWidthMax, UiOps.TabWidthMax);
  G.SetTabOption(tabOptionHeightInner, UiOps.TabHeightInner);
  G.SetTabOption(tabOptionSpacer, IfThen(UiOps.TabPosition=0, UiOps.TabSpacer));
  G.SetTabOption(tabOptionSpacer2, 1); //for multiline mode
  G.SetTabOption(tabOptionSpaceBeforeText, UiOps.TabSpaceBeforeText);
  G.SetTabOption(tabOptionSpaceAfterText, UiOps.TabSpaceAfterText);
  G.SetTabOption(tabOptionColoredBandSize, _InitOptColoredBandSize);
  G.SetTabOption(tabOptionActiveMarkSize, _InitOptActiveMarkSize);
  G.SetTabOption(tabOptionScrollMarkSizeX, _InitOptScrollMarkSizeX);
  G.SetTabOption(tabOptionScrollMarkSizeY, _InitOptScrollMarkSizeY);
  G.SetTabOption(tabOptionShowNums, Ord(UiOps.TabNumbers));
  G.SetTabOption(tabOptionSpaceXRight, 10);
  G.SetTabOption(tabOptionSpaceXSize, UiOps.TabShowXSize);
  G.SetTabOption(tabOptionArrowSize, 4);
  G.SetTabOption(tabOptionButtonSize, 16);
  G.SetTabOption(tabOptionShowArrowsNear, Ord(Pos('<>', UiOps.TabButtonLayout)>0));
  G.SetTabOption(tabOptionWhichActivateOnClose, IfThen(UiOps.TabRecentOnClose, Ord(aocRecent), Ord(aocRight)));
  G.SetTabOption(tabOptionPosition, UiOps.TabPosition);

  G.SetTabOptionString(tabOptionButtonLayout, UiOps.TabButtonLayout);
  DoApplyTranslationToGroups(G);
end;

procedure TfmMain.DoApplyTranslationToGroups(G: TATGroups);
begin
  G.SetTabOptionString(tabOptionHintForX, msgTooltipCloseTab);
  G.SetTabOptionString(tabOptionHintForPlus, msgTooltipAddTab);
  G.SetTabOptionString(tabOptionHintForArrowLeft, msgTooltipArrowLeft);
  G.SetTabOptionString(tabOptionHintForArrowRight, msgTooltipArrowRight);
  G.SetTabOptionString(tabOptionHintForArrowMenu, msgTooltipArrowMenu);
end;



procedure TfmMain.DoApplyUiOps;
var
  id: TAppPanelId;
  Pages: TATPages;
  Ed: TATSynEdit;
  F: TEditorFrame;
  i: integer;
begin
  AppScaleSplitter(AppPanels[TAppPanelId.Side].Splitter);
  AppScaleSplitter(AppPanels[TAppPanelId.Btm].Splitter);
  AppScaleSplitter(Groups.Splitter1);
  AppScaleSplitter(Groups.Splitter2);
  AppScaleSplitter(Groups.Splitter3);
  AppScaleSplitter(Groups.Splitter4);
  AppScaleSplitter(Groups.Splitter5);

  //apply DoubleBuffered
  //no need for ToolbarMain and buttons
  for i:= 0 to cAppMaxGroup do
  begin
    Pages:= TGroupsHelper.GetPagesOfGroupIndex(i);
    if Pages=nil then Continue;
    Pages.Tabs.DoubleBuffered:= UiOps.DoubleBuffered;
  end;

  for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      F.Ed1.DoubleBuffered:= UiOps.DoubleBuffered;
      F.Ed2.DoubleBuffered:= UiOps.DoubleBuffered;
      F.Ed1.Font.Size:= EditorOps.OpFontSize;
      F.Ed2.Font.Size:= EditorOps.OpFontSize;
      if Assigned(F.Binary) then
      begin
        F.Binary.DoubleBuffered:= UiOps.DoubleBuffered;
        F.Binary.Font.Size:= EditorOps.OpFontSize;
      end;
    end;

  Status.DoubleBuffered:= UiOps.DoubleBuffered;
  //LexerProgress.DoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmConsole) then
    fmConsole.IsDoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmFind) then
    fmFind.IsDoubleBuffered:= UiOps.DoubleBuffered;
  //end apply DoubleBuffered

  if Assigned(fmFind) then
  begin
    EditorApplyOpsCommon(fmFind.edFind);
    EditorApplyOpsCommon(fmFind.edRep);
    Ed:= CurrentEditor;
    fmFind.chkHiAll.Enabled:= Assigned(Ed) and (Ed.Strings.Count<UiOps.FindHiAll_MaxLines);
  end;

  UpdateStatusbarPanelsFromString(UiOps.StatusPanels);
  UpdateStatusbarHints;

  TimerTreeFill.Interval:= UiOps.TreeTimeFill;
  CodeTree.Tree.ToolTips:= UiOps.TreeShowTooltips;
  CodeTree.Invalidate;

  EditorApplyOpsCommon(CodeTreeFilterInput);
  //CodeTreeFilterReset.Width:= ATEditorScale(ATScrollbarTheme.InitialSize);

  if Assigned(fmConsole) then
  begin
    EditorApplyOpsCommon(fmConsole.EdMemo);
    EditorApplyOpsCommon(fmConsole.EdInput);
    fmConsole.EdInput.Height:= ATEditorScale(UiOps.InputHeight);
    fmConsole.MemoWordWrap:= UiOps.ConsoleWordWrap;
    fmConsole.ApplyCaretView;
    fmConsole.ShortCutForAutoCompletion:= AppKeymapMain.GetShortcutFromCommand(cmd_AutoComplete);
  end;

  EditorApplyOpsCommon(fmOutput.Ed);
  EditorApplyOpsCommon(fmValidate.Ed);

  DoApplyFont_Output;

  DoApplyUiOpsToGroups(Groups);
  if FloatGroups then
  begin
    DoApplyUiOpsToGroups(GroupsF1);
    DoApplyUiOpsToGroups(GroupsF2);
    DoApplyUiOpsToGroups(GroupsF3);
  end;

  ShowStatus:= UiOps.ShowStatusbar;
  ShowToolbar:= UiOps.ShowToolbar;

  PanelSide.Visible:= UiOps.SidebarShow;
  ShowSideBarOnRight:= UiOps.SidebarOnRight;

  for id in TAppPanelId do
    if id<>TAppPanelId.None then
      with AppPanels[id] do
      begin
        PanelTitle.Height:= Groups.GetTabSingleRowHeight-1;
        if UiOps.TabPosition=1 then
          PanelTitle.Align:= alBottom
        else
          PanelTitle.Align:= alTop;
      end;

  case UiOps.TreeFilterLayout of
    0:
      PanelCodeTreeTop.Hide;
    1:
      begin
        PanelCodeTreeTop.Align:= alTop;
        PanelCodeTreeTop.Show;
      end;
    2:
      begin
        PanelCodeTreeTop.Align:= alBottom;
        PanelCodeTreeTop.Show;
      end;
  end;

  PanelCodeTreeTop.Height:= ATEditorScale(UiOps.InputHeight);

  TimerStatusClear.Interval:= UiOps.StatusTime*1000;

  ATFlatTheme.FontName:= UiOps.VarFontName;
  ATFlatTheme.FontSize:= UiOps.VarFontSize;
  ATFlatTheme.MonoFontName:= EditorOps.OpFontName;
  ATFlatTheme.MonoFontSize:= EditorOps.OpFontSize;
  ATFlatTheme.ScalePercents:= ATEditorScalePercents;
  ATFlatTheme.ScaleFontPercents:= ATEditorScaleFontPercents;

  ATScrollbar.ATScrollbarTheme.ScalePercents:= ATEditorScalePercents;
  ATScrollbar.ATScrollbarTheme.BorderSize:= 1;

  CompletionOps.FormWidth:= ATEditorScale(UiOps.ListboxAutoCompleteWidth);
  CompletionOps.FormMaxVisibleItems:= UiOps.ListboxAutoCompleteMaxItems;

  {$ifdef unix}
  if not AppAlwaysNewInstance and UiOps.OneInstance then
  begin
    if Assigned(AppUniqInst) then
      AppUniqInst.OnOtherInstance:= @fmMain.HandleOtherInstance;
  end;
  {$endif}

  //apply UiOps.Scale to sidebar
  ToolbarSideTop.UpdateControls;
  ToolbarSideLow.UpdateControls;
  ToolbarSideMid.UpdateControls;

  DoApplyTheme;
end;

procedure TfmMain.DoFolderOpen(const ADirName: string; ANewProject: boolean;
  AInvoke: TATCommandInvoke);
begin
  DoPyCommand('cuda_project_man', 'open_dir',
    [AppVariant(ADirName), AppVariant(ANewProject)],
    AInvoke);
end;

procedure TfmMain.DoFolderAdd(AInvoke: TATCommandInvoke);
begin
  if not AppPython.Inited then
  begin
    MsgBox(msgCommandNeedsPython, MB_OK or MB_ICONWARNING);
    exit;
  end;

  DoPyCommand('cuda_project_man', 'new_project_open_dir', [], AInvoke);
end;

procedure TfmMain.DoGroupsChangeMode(Sender: TObject);
begin
  DoPyEvent_AppState(APPSTATE_GROUPS);
end;

procedure TfmMain.CodeTreeFilter_OnChange(Sender: TObject);
var
  F: TEditorFrame;
  S: string;
begin
  S:= UTF8Encode(CodeTreeFilterInput.Text);

  F:= CurrentFrame;
  if Assigned(F) then
    F.CodetreeFilter:= S;

  if S='' then
  begin
    CodeTreeFilter.ForceFilter('');
    CodeTreeFilter.FilteredTreeview:= nil;
    exit;
  end;

  CodeTreeFilter.FilteredTreeview:= CodeTree.Tree;
  CodeTreeFilter.Text:= S;
end;

procedure TfmMain.CodeTreeFilter_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Frame: TEditorFrame;
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
      Frame.SetFocus;
    Key:= 0;
    exit
  end;

  //handle Tab-key, because LCL by default can jump to bottom-panel form
  if (Key=VK_TAB) and (Shift=[]) then
  begin
    if CodeTree.Tree.CanFocus then
      CodeTree.Tree.SetFocus;
    Key:= 0;
    exit;
  end;

  if ((Key=VK_UP) or (Key=VK_DOWN) or (Key=VK_RETURN)) and (Shift=[]) then
  begin
    CodeTree.Tree.KeyDown(Key, Shift);
    Key:= 0;
    exit;
  end;
end;

procedure TfmMain.CodeTreeFilter_ResetOnClick(Sender: TObject);
begin
  CodeTreeFilterInput.Text:= '';
  CodeTreeFilterInput.OnChange(nil);
end;

procedure TfmMain.MsgStatusFileOpened(const AFileName1, AFileName2: string);
var
  S: string;
begin
  S:= msgStatusOpened+' "'+ExtractFileName(AFileName1)+'"';
  if AFileName2<>'' then
    S+= ', "'+ExtractFileName(AFileName2)+'"';
  MsgStatus(S);
end;

function SubInString(const sub, s: string): boolean;
begin
  Result:= System.Pos(sub, s)>0;
end;

function TfmMain.DoFileOpen(AFileName, AFileName2: string; APages: TATPages;
  const AOptions: string): TEditorFrame;
var
  D: TATTabData;
  F: TEditorFrame;
  bSilent, bPreviewTab, bEnableHistory, bEnableLoadUndo, bEnableLoadBookmarks,
  bEnableEventPre, bEnableEventOpened, bEnableEventOpenedNone,
  bAllowZip, bAllowPics, bAllowLexerDetect, bDetectedPics,
  bAllowUpdateAddons, bAndActivate: boolean;
  bFileTooBig, bFileTooBig2: boolean;
  AllowNear: TAppNewTabNearCurrent;
  OpenMode, NonTextMode: TAppOpenMode;
  CurGroups: TATGroups;
  //tick: QWord;
  //msg: string;
begin
  Result:= nil;
  AppDir_LastInstalledAddon:= '';
  if Application.Terminated then exit;
  if IsTooManyTabsOpened then exit;
  AppOpeningFile:= true;

 try
  bFileTooBig:= IsFileTooBigForOpening(AFileName);
  bFileTooBig2:= IsFileTooBigForOpening(AFileName2);

  //we cannot open too big _second_ file, viewer is for first file
  if bFileTooBig2 then
  begin
    MsgBox(
      msgFileTooBig+#10+AFileName2+#10+Format('(%d M)', [FileSize(AFileName2) div (1024*1024)]),
      MB_OK+MB_ICONERROR);
    AFileName2:= '';
  end;

  CurGroups:= CurrentGroups;

  bSilent:= SubInString('/silent', AOptions);
  bPreviewTab:= SubInString('/preview', AOptions);
  bEnableHistory:= not SubInString('/nohistory', AOptions);
  bEnableLoadBookmarks:= true;
  bEnableLoadUndo:= not SubInString('/noloadundo', AOptions);
  bEnableEventPre:= not SubInString('/noevent', AOptions);
  bEnableEventOpened:= not SubInString('/noopenedevent', AOptions);
  bEnableEventOpenedNone:= not SubInString('/nononeevent', AOptions);
  bAndActivate:= not SubInString('/passive', AOptions);
  bAllowLexerDetect:= not SubInString('/nolexerdetect', AOptions);
  bAllowZip:= not SubInString('/nozip', AOptions);
  bAllowPics:= not SubInString('/nopictures', AOptions);
  bAllowUpdateAddons:= not SubInString('/noupdateaddons', AOptions);

  AllowNear:= TAppNewTabNearCurrent.ByOption;
  if SubInString('/donear', AOptions) then
    AllowNear:= TAppNewTabNearCurrent.Enabled
  else
  if SubInString('/nonear', AOptions) then
    AllowNear:= TAppNewTabNearCurrent.Disabled;

  if SubInString('/view-text', AOptions) then
    OpenMode:= TAppOpenMode.ViewText
  else
  if SubInString('/view-binary', AOptions) then
    OpenMode:= TAppOpenMode.ViewBinary
  else
  if SubInString('/view-hex', AOptions) then
    OpenMode:= TAppOpenMode.ViewHex
  else
  if SubInString('/view-unicode', AOptions) then
    OpenMode:= TAppOpenMode.ViewUnicode
  else
  if SubInString('/view-uhex', AOptions) then
    OpenMode:= TAppOpenMode.ViewUHex
  else
    OpenMode:= TAppOpenMode.Editor;

  if SubInString('/nontext-view-text', AOptions) then
    NonTextMode:= TAppOpenMode.ViewText
  else
  if SubInString('/nontext-view-binary', AOptions) then
    NonTextMode:= TAppOpenMode.ViewBinary
  else
  if SubInString('/nontext-view-hex', AOptions) then
    NonTextMode:= TAppOpenMode.ViewHex
  else
  if SubInString('/nontext-view-unicode', AOptions) then
    NonTextMode:= TAppOpenMode.ViewUnicode
  else
  if SubInString('/nontext-view-uhex', AOptions) then
    NonTextMode:= TAppOpenMode.ViewUHex
  else
  if SubInString('/nontext-cancel', AOptions) then
    NonTextMode:= TAppOpenMode.None
  else
    NonTextMode:= TAppOpenMode.Editor;

  if APages=nil then
    APages:= CurGroups.PagesCurrent;

  if AFileName='' then
  begin
    D:= CreateTab(APages, '', bAndActivate, AllowNear);
    if not Assigned(D) then
    begin
      D:= Groups.Pages1.Tabs.GetTabData(0);
      DoClearSingleFirstTab;
    end;
    Result:= D.TabObject as TEditorFrame;
    Result.SetFocus;
    Exit
  end;

  //expand "./name"
  //note: ExpandFileNameUTF8 has bug in Laz 1.9-
  if AFileName<>'' then
  begin
    AFileName:= AppExpandFileName(AFileName);
    if not FileExists(AFileName) then
    begin
      MsgBox(msgCannotFindFile+#10+AppCollapseHomeDirInFilename(AFileName), MB_OK or MB_ICONERROR);
      Exit
    end;
  end;

  if AFileName2<>'' then
  begin
    AFileName2:= AppExpandFileName(AFileName2);
    if not FileExists(AFileName2) then
    begin
      MsgBox(msgCannotFindFile+#10+AppCollapseHomeDirInFilename(AFileName2), MB_OK or MB_ICONERROR);
      Exit
    end;
  end;

  if not FileIsReadable(AFileName) then
  begin
    MsgBox(msgCannotOpenFile+#10+AFileName+#10#10+msgCannotOpenNoReadPermissions, MB_OK or MB_ICONERROR);
    exit;
  end;

  if not FileIsReadable(AFileName2) then
  begin
    AFileName2:= '';
  end;

  if OpenMode=TAppOpenMode.Editor then
  begin
    //zip files
    if bAllowZip and (ExtractFileExt(AFileName)='.zip') then
    begin
      if DoFileInstallZip(AFileName, AppDir_LastInstalledAddon, bSilent, bAllowUpdateAddons) then
        Result:= CurrentFrame;
      exit
    end;

    //session files
    if ExtractFileExt(AFileName)='.cuda-session' then
    begin
      DoOps_LoadSession(AFileName, true);
      Result:= CurrentFrame;
      exit
    end;

    //py event
    if bEnableEventPre then
    begin
      if DoPyEvent(CurrentEditor, TAppPyEvent.OnOpenBefore, [AppVariant(AFileName)]).Val = TAppPyEventValue.False then exit;
    end;

    bDetectedPics:= bAllowPics and IsFilenameListedInExtensionList(AFileName, UiOps.PictureTypes);

    //non-text option
    if not bFileTooBig then
    if not bDetectedPics then
    if not AppSessionIsLoading then
    if UiOps.NonTextFiles<>1 then
      if not AppIsFileContentText(
               AFileName,
               UiOps.NonTextFilesBufferKb,
               ATEditorOptions.DetectUTF16BufferWords
               ) then
      begin
        if NonTextMode=TAppOpenMode.None then
          Exit;
        if NonTextMode<>TAppOpenMode.Editor then
          OpenMode:= NonTextMode
        else
        case UiOps.NonTextFiles of
          0:
            case DoDialogConfirmBinaryFile(AFileName, bFileTooBig) of
              ConfirmBinaryViewText:
                OpenMode:= TAppOpenMode.ViewText;
              ConfirmBinaryViewBinary:
                OpenMode:= TAppOpenMode.ViewBinary;
              ConfirmBinaryViewHex:
                OpenMode:= TAppOpenMode.ViewHex;
              ConfirmBinaryViewUnicode:
                OpenMode:= TAppOpenMode.ViewUnicode;
              ConfirmBinaryViewUHex:
                OpenMode:= TAppOpenMode.ViewUHex;
              ConfirmBinaryCancel:
                Exit;
            end;
          2:
            Exit;
          3:
            OpenMode:= TAppOpenMode.ViewBinary;
          4:
            OpenMode:= TAppOpenMode.ViewHex;
          else
            Exit;
        end;
      end;

    //too big size?
    if (OpenMode=TAppOpenMode.Editor) and bFileTooBig then
    begin
      case DoDialogConfirmBinaryFile(AFileName, bFileTooBig) of
        ConfirmBinaryViewText:
          OpenMode:= TAppOpenMode.ViewText;
        ConfirmBinaryViewBinary:
          OpenMode:= TAppOpenMode.ViewBinary;
        ConfirmBinaryViewHex:
          OpenMode:= TAppOpenMode.ViewHex;
        ConfirmBinaryViewUnicode:
          OpenMode:= TAppOpenMode.ViewUnicode;
        ConfirmBinaryViewUHex:
          OpenMode:= TAppOpenMode.ViewUHex;
        ConfirmBinaryCancel:
          Exit;
      end;
    end;
  end; //not binary

  //file already opened? activate its frame
  F:= FindFrameOfFilename(AFileName);
  if F=nil then
    if AFileName2<>'' then
      F:= FindFrameOfFilename(AFileName2);
  if Assigned(F) then
  begin
    //don't work, if need to open 2 files
    if AFileName2<>'' then
    begin
      if not SameFileName(F.FileName, AFileName) then exit;
      if not SameFileName(F.FileName2, AFileName2) then exit;
    end;

    SetFrame(F);
    Result:= F;
    Result.SetFocus;
    UpdateStatusbar;
    UpdateTreeByTimer;
    Exit
  end;

  //get preview-tab, create it if not yet opened
  if bPreviewTab then
  begin
    Result:= FindFrameOfPreviewTab;
    if Result=nil then
    begin
      APages:= Groups.Pages1; //open preview tab in 1st group
      if UiOps.TabsDisabled then
        D:= APages.Tabs.GetTabData(0)
      else
        D:= CreateTab(APages, 'pre', true, TAppNewTabNearCurrent.Disabled);
      if not Assigned(D) then exit;
      UpdateTabPreviewStyle(D, true);
      Result:= D.TabObject as TEditorFrame;
    end;

    if bAndActivate then
    begin
      SetFrame(Result);
      DoFocusFrame(Result);
    end;

    Result.DoSaveHistory(Result.Ed1);
    if not Result.EditorsLinked then
      Result.DoSaveHistory(Result.Ed2);

    Result.Ed1.BookmarkDeleteAll();
    Result.Ed2.BookmarkDeleteAll();

    Result.DoFileOpen(AFileName, AFileName2,
      bEnableHistory,
      bEnableLoadBookmarks,
      bAllowLexerDetect,
      true,
      bEnableLoadUndo,
      OpenMode);
    MsgStatusFileOpened(AFileName, AFileName2);

    if bEnableEventOpened then
    begin
      DoPyEvent_Open(Result.Ed1);
    end;

    exit;
  end;

  //is current frame empty? use it
  if APages=CurGroups.PagesCurrent then
  begin
    F:= CurrentFrame;
    if Assigned(F) then
    if F.IsEmpty then
    begin
      //tick:= GetTickCount64;
      F.DoFileOpen(AFileName, AFileName2,
        bEnableHistory,
        bEnableLoadBookmarks,
        bAllowLexerDetect,
        true,
        bEnableLoadUndo,
        OpenMode);
      Result:= F;
      //tick:= (GetTickCount64-tick) div 1000;

      UpdateStatusbar;
      UpdateFindDialogEnabled(F);
      //if tick>2 then
      //  msg:= msg+' ('+IntToStr(tick)+'s)';
      MsgStatusFileOpened(AFileName, AFileName2);

      if bEnableEventOpened then
        DoPyEvent_Open(F.Ed1);

      if bEnableEventOpenedNone and IsFilenameForLexerDetecter(AFileName) then
        if (F.FrameKind=TAppFrameKind.Editor) and (F.LexerName[F.Ed1]='') then
        begin
          DoPyEvent_OpenNone(F.Ed1);
          UpdateStatusbar;
        end;

      if AFileName2<>'' then
      begin
        if bEnableEventOpened then
          DoPyEvent_Open(F.Ed2);

        if bEnableEventOpenedNone and IsFilenameForLexerDetecter(AFileName2) then
          if (F.FrameKind=TAppFrameKind.Editor) and (F.LexerName[F.Ed2]='') then
            DoPyEvent_OpenNone(F.Ed2);

        UpdateStatusbar;
      end;

      //for _lite_ lexer, update code-tree (for _normal_ lexer it will run on parsing-done)
      if F.Ed1.AdapterForHilite is TATLiteLexer then
        UpdateTreeByTimer;

      Exit
    end;
  end;

  //did not find frame to reuse, create new frame
  D:= CreateTab(APages, ExtractFileName(AFileName), false{AndActivate}, AllowNear);
  if not Assigned(D) then
  begin
    D:= Groups.Pages1.Tabs.GetTabData(0);
    DoClearSingleFirstTab;
  end;
  F:= D.TabObject as TEditorFrame;

  F.DoFileOpen(AFileName, AFileName2,
    bEnableHistory,
    bEnableLoadBookmarks,
    bAllowLexerDetect,
    true,
    bEnableLoadUndo,
    OpenMode);
  Result:= F;

  //use AndActivate=false in CreateTab() and focus here manually,
  //to avoid setting None-lexer + applying lexer-specific config for it; issue #5320
  if bAndActivate then
  begin
    SetFrame(Result);
    DoFocusFrame(Result);
  end;

  UpdateStatusbar;
  UpdateFindDialogEnabled(F);
  MsgStatusFileOpened(AFileName, AFileName2);

  if bEnableEventOpened then
    DoPyEvent_Open(F.Ed1);

  if bEnableEventOpenedNone then
    if IsFilenameForLexerDetecter(AFileName) then
      if (F.FrameKind=TAppFrameKind.Editor) and (F.LexerName[F.Ed1]='') then
        DoPyEvent_OpenNone(F.Ed1);

  if bEnableEventOpened then
    if AFileName2<>'' then
      DoPyEvent_Open(F.Ed2);

  if bAndActivate then
    DoFocusFrame(Result);
 finally
   AppOpeningFile:= false;
 end;
end;


procedure TfmMain.DoFileOpenDialog_NoPlugins;
begin
  DoFileOpenDialog('/noevent');
end;

procedure TfmMain.DoFileDialog_PrepareDir(Dlg: TFileDialog);
var
  Dir: string;
  iChar: integer;
begin
  Dir:= '';
  for iChar:= 1 to Length(UiOps.OpenDir) do
  begin
    case UiOps.OpenDir[iChar] of
      'p':
        Dir:= PyCurrentProjectFolder;
      'q':
        Dir:= ExtractFileDir(PyCurrentProject);
      'f':
        Dir:= ExtractFileDir(CurrentFrame.FileName);
      'l':
        Dir:= FLastDirOfOpenDlg;
      'i':
        begin
          Dir:= UiOps.InitialDir;
          if Pos('{', Dir)>0 then
          begin
            Dir:= StringReplace(Dir, '{AppDir}', ExtractFileDir(Application.ExeName), [rfReplaceAll]);
            Dir:= StringReplace(Dir, '{AppDrive}', {$ifdef windows} ExtractFileDrive(Application.ExeName) {$else} '' {$endif}, [rfReplaceAll]);
          end;
        end;
      'h':
        Dir:= AppDir_Home;
      else
        Dir:= '';
    end;
    if (Dir<>'') and DirectoryExists(Dir) then
      Break;
  end;
  Dlg.InitialDir:= Dir;
end;

procedure TfmMain.DoFileDialog_SaveDir(Dlg: TFileDialog);
begin
  FLastDirOfOpenDlg:= ExtractFileDir(Dlg.FileName);
end;


procedure TfmMain.DoFileOpenDialog(const AOptions: string='');
const
  //passive option used only for many files
  SOptionPassive = '/passive /nonear';
  SOptionSilent = '/silent /noupdateaddons';
var
  dlg: TOpenDialog;
  NFileCount, NCountZip, i: integer;
  fn: string;
  bZip, bZipAllowed: boolean;
begin
  bZipAllowed:= not SubInString('/nozip', AOptions);

  dlg:= TOpenDialog.Create(nil);
  try
    dlg.Title:= msgDialogTitleOpen;
    dlg.Options:= [
      ofAllowMultiSelect,
      ofPathMustExist,
      ofEnableSizing
      ];
    dlg.FileName:= '';

    DoFileDialog_PrepareDir(dlg);
    if not dlg.Execute then exit;
    DoFileDialog_SaveDir(dlg);

    NFileCount:= dlg.Files.Count;
    NCountZip:= 0;

    if NFileCount>1 then
    begin
      UpdateGlobalProgressbar(0, true, NFileCount);

      for i:= 0 to NFileCount-1 do
      begin
        fn:= dlg.Files[i];
        if not FileExists(fn) then Continue;

        bZip:= bZipAllowed and (ExtractFileExt(fn)='.zip');
        if bZip then
          Inc(NCountZip);

        DoFileOpen(fn, '', nil, AOptions + SOptionPassive + IfThen(bZip, SOptionSilent));

        if bZip or (i mod 10 = 0) then
        begin
          UpdateGlobalProgressbar(i+1, true, NFileCount);
          Application.ProcessMessages;
        end;
      end;

      UpdateGlobalProgressbar(0, false);
      if NCountZip>0 then
      begin
        UpdatePlugins_AfterInstallingZip;
        MsgBox(
          Format(msgStatusAddonsInstalled, [NCountZip]),
          MB_OK or MB_ICONINFORMATION);
      end;
    end
    else
    begin
      if FileExists(dlg.FileName) then
        DoFileOpen(dlg.FileName, '', nil, AOptions)
      else
      if MsgBox(
        Format(msgConfirmCreateNewFile, [dlg.FileName]),
        MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
      begin
        AppCreateFile(dlg.FileName);
        DoFileOpen(dlg.FileName, '', nil, AOptions);
      end;
    end;
  finally
    FreeAndNil(dlg);
  end;
end;

procedure TfmMain.DoDialogCommands;
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  NCmd: integer;
  Props: TDlgCommandsProps;
begin
  F:= CurrentFrame;
  Ed:= F.Editor;

  TKeymapHelperMain.UpdateDynamic(TAppCommandCategory.Lexer);
  TKeymapHelperMain.UpdateDynamic(TAppCommandCategory.OpenedFile);
  TKeymapHelperMain.UpdateDynamic(TAppCommandCategory.RecentFile);

  Props:= Default(TDlgCommandsProps);
  Props.Caption:= msgCmdPaletteCaption;
  Props.LexerName:= F.LexerName[Ed];
  Props.ShowUsual:= true;
  Props.ShowPlugins:= true;
  Props.ShowLexers:= true;
  Props.ShowFiles:= true;
  Props.ShowRecents:= true;
  Props.AllowConfig:= true;
  Props.AllowConfigForLexer:= true;
  Props.ShowCentered:= false;
  Props.FocusedCommand:= FLastSelectedCommand;

  NCmd:= DoDialogCommands_Custom(Ed, Props);
  if NCmd>0 then
  begin
    FLastSelectedCommand:= NCmd;
    Ed.DoCommand(NCmd, TATCommandInvoke.AppPalette);
    UpdateCurrentFrame;
  end;

  if Assigned(fmConsole) then
    fmConsole.ShortCutForAutoCompletion:= AppKeymapMain.GetShortcutFromCommand(cmd_AutoComplete);
end;


function TfmMain.DoDialogCommands_Py(var AProps: TDlgCommandsProps): string;
  //
  function _GetPyCommand(Cmd: TAppCommandInfo): string;
  begin
    if Cmd.ItemProcParam<>'' then
      Result:= Format('p:module=%s;cmd=%s;info=%s;', [Cmd.ItemModule, Cmd.ItemProc, Cmd.ItemProcParam])
    else
      Result:= Format('p:%s.%s', [Cmd.ItemModule, Cmd.ItemProc]);
  end;
  //
var
  F: TEditorFrame;
  NCmd, NIndex: integer;
  Category: TAppCommandCategory;
begin
  Result:= '';

  F:= CurrentFrame;
  if F=nil then exit;

  AProps.LexerName:= F.LexerName[F.Editor];
  NCmd:= DoDialogCommands_Custom(F.Editor, AProps);
  if NCmd<=0 then exit;
  Category:= TPluginHelper.CommandCategory(NCmd);

  case Category of
    TAppCommandCategory.Plugin:
      Result:= _GetPyCommand(TAppCommandInfo(AppCommandList[NCmd-cmdFirstPluginCommand]));

    //PluginSub is needed here, e.g. for ExtTools plugin with its subcommands
    TAppCommandCategory.PluginSub:
      Result:= _GetPyCommand(TAppCommandInfo(AppCommand2List[NCmd-cmdFirstPluginSubCommand]));

    TAppCommandCategory.Lexer:
      begin
        NIndex:= NCmd-cmdFirstLexerCommand;
        if NIndex<AppManager.LexerCount then
          Result:= 'l:'+AppManager.Lexers[NIndex].LexerName
        else
        begin
          Dec(NIndex, AppManager.LexerCount);
          if NIndex<AppManagerLite.LexerCount then
            Result:= 'l:'+AppManagerLite.Lexers[NIndex].LexerName+msgLiteLexerSuffix
          else
            Result:= 'c:'+IntToStr(NCmd);
        end;
      end;

    TAppCommandCategory.OpenedFile:
      begin
        NIndex:= NCmd-cmdFirstFileCommand;
        if NIndex<AppFrameList1.Count then
          Result:= 'f:'+TEditorFrame(AppFrameList1[NIndex]).FileName
        else
          Result:= 'c:'+IntToStr(NCmd);
      end;

    TAppCommandCategory.RecentFile:
      begin
        NIndex:= NCmd-cmdFirstRecentCommand;
        if NIndex<AppListRecents.Count then
          Result:= 'r:'+AppListRecents[NIndex]
        else
          Result:= 'c:'+IntToStr(NCmd);
      end;

    else
      Result:= 'c:'+IntToStr(NCmd);
  end;
end;


function TfmMain.DoDialogCommands_Custom(Ed: TATSynEdit; const AProps: TDlgCommandsProps): integer;
var
  bKeysChanged: boolean;
begin
  Result:= 0;
  fmCommands:= TfmCommands.Create(Self);
  try
    UpdateInputForm(fmCommands);
    fmCommands.OptShowUsual:= AProps.ShowUsual;
    fmCommands.OptShowPlugins:= AProps.ShowPlugins;
    fmCommands.OptShowLexers:= AProps.ShowLexers;
    fmCommands.OptShowFiles:= AProps.ShowFiles;
    fmCommands.OptShowRecents:= AProps.ShowRecents;
    fmCommands.OptAllowConfig:= AProps.AllowConfig;
    fmCommands.OptAllowConfigForLexer:= AProps.AllowConfigForLexer;
    fmCommands.OptFocusedCommand:= AProps.FocusedCommand;
    fmCommands.OnMsg:= @DoCommands_OnMsg;
    fmCommands.CurrentLexerName:= AProps.LexerName;
    fmCommands.Keymap:= Ed.Keymap;
    fmCommands.ListCaption:= AProps.Caption;

    if UiOps.CmdPaletteFilterText_Forced<>'' then
    begin
      fmCommands.CurrentFilterText:= UiOps.CmdPaletteFilterText_Forced;
      UiOps.CmdPaletteFilterText_Forced:= '';
    end
    else
    if UiOps.CmdPaletteFilterKeep then
      fmCommands.CurrentFilterText:= UiOps.CmdPaletteFilterText;

    if AProps.ShowCentered then
      fmCommands.Position:= poScreenCenter;

    if AProps.W>0 then
      fmCommands.Width:= AProps.W;
    if AProps.H>0 then
      fmCommands.Height:= AProps.H;

    fmCommands.ShowModal;

    UiOps.CmdPaletteFilterText:= fmCommands.CurrentFilterText;
    Result:= fmCommands.ResultCommand;
    bKeysChanged:= fmCommands.ResultHotkeysChanged;
  finally
    FreeAndNil(fmCommands);
  end;

  if bKeysChanged then
  begin
    UpdateMenuHotkeys;
    UpdateMenuPlugins_Shortcuts(true);
  end;
end;


procedure TfmMain.DoDialogGoto;
var
  Str: string;
  Frame: TEditorFrame;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;

  if not Assigned(fmGoto) then
    fmGoto:= TfmGoto.Create(Self);

  fmGoto.IsDoubleBuffered:= UiOps.DoubleBuffered;
  fmGoto.Width:= ATEditorScale(UiOps.ListboxSizeX);
  UpdateInputForm(fmGoto, false);

  fmGoto.edInput.Text:= Frame.GotoInput;
  fmGoto.edInput.DoSelect_All;

  if fmGoto.ShowModal=mrOk then
  begin
    Frame.GotoInput:= fmGoto.edInput.Text;
    Str:= UTF8Encode(Frame.GotoInput);
    if DoPyEvent(Frame.Editor, TAppPyEvent.OnGotoEnter, [AppVariant(Str)]).Val = TAppPyEventValue.False then exit;
    DoGotoFromInput(Frame, Str);
  end;
end;

function TfmMain.DoDialogMenuList(const ACaption: string; AItems: TStringList;
  AInitItemIndex: integer;
  ACloseOnCtrlRelease: boolean=false;
  AOnListSelect: TAppListSelectEvent=nil): integer;
var
  Form: TfmMenuList;
begin
  Result:= -1;
  if AItems.Count=0 then exit;
  Form:= TfmMenuList.Create(Self);
  try
    UpdateInputForm(Form);
    Form.Caption:= ACaption;
    Form.Items:= AItems;
    Form.CloseOnCtrlRelease:= ACloseOnCtrlRelease;
    Form.InitialItemIndex:= AInitItemIndex;
    Form.OnListSelect:= AOnListSelect;
    Form.ShowModal;
    Result:= Form.ResultIndex;
  finally
    FreeAndNil(Form);
  end;
end;

function TfmMain.DoDialogMenuLexerChoose(const AFilename: string; ANames: TStringList): integer;
begin
  Result:= DoDialogMenuList(
    Format(msgMenuLexersForFile, [ExtractFileName(AFilename)]),
    ANames, 0);
end;

procedure TfmMain.DoGotoFromInput(Frame: TEditorFrame; const AInput: string);
begin
  case Frame.FrameKind of
    TAppFrameKind.BinaryViewer:
    begin
      if ViewerGotoFromString(Frame.Binary, AInput) then
        MsgStatus('')
      else
        MsgStatus(msgStatusBadLineNum);
    end;
    TAppFrameKind.Editor:
    begin
      if EditorGotoFromString(Frame.Editor, AInput) then
        MsgStatus('')
      else
        MsgStatus(msgStatusBadLineNum);
    end;
  end;

  Frame.SetFocus;
end;

type
  TAppBookmarkProp = class
  public
    Frame: TEditorFrame;
    Ed: TATSynEdit;
    LineIndex: integer;
    MenuCaption: string;
  end;

procedure TfmMain.DoDialogGotoBookmark;
var
  ListItems: TStringList;
  //
  function ShrinkLineIndentation(const S: string; ATabSize: integer): string;
  var
    N: integer;
    SBegin, SEnd: string;
  begin
    N:= SGetIndentChars(S);
    if N<2 then exit(S);
    SBegin:= Copy(S, 1, N);
    SEnd:= Copy(S, N+1, MaxInt);
    SBegin:= StringReplace(SBegin, #9, StringOfChar(' ', ATabSize), [rfReplaceAll]);
    if ATabSize>=4 then
      N:= 4
    else
      N:= 2;
    SetLength(SBegin, Length(SBegin) div N);
    Result:= SBegin+SEnd;
  end;
  //
  function NiceBookmarkKind(NKind: integer): string;
  begin
    //paint prefix [N] for numbered bookmarks (kind=2..10)
    if (NKind>=2) and (NKind<=10) then
      Result:= '['+IntToStr(NKind-1)+'] '
    else
      Result:= '';
  end;
  //
  procedure AddItemsOfFrame(Frame: TEditorFrame);
  var
    Ed: TATSynEdit;
    SCaption: string;
    Prop: TAppBookmarkProp;
    Mark: PATBookmarkItem;
    NLine, i: integer;
  const
    cMaxLen = 150;
  begin
    Ed:= Frame.Editor;
    for i:= 0 to Ed.Strings.Bookmarks.Count-1 do
    begin
      Mark:= Ed.Strings.Bookmarks[i];
      if not Mark^.Data.ShowInBookmarkList then Continue;

      NLine:= Mark^.Data.LineNum;
      if not Ed.Strings.IsIndexValid(NLine) then Continue;

      SCaption:= Copy(Ed.Strings.Lines[NLine], 1, cMaxLen);
      SCaption:= ShrinkLineIndentation(SCaption, Ed.OptTabSize);

      Prop:= TAppBookmarkProp.Create;
      Prop.Frame:= Frame;
      Prop.Ed:= Ed;
      Prop.LineIndex:= NLine;
      Prop.MenuCaption:=
        SCaption+
        #9'  '+
        Frame.TabCaption+': '+
        NiceBookmarkKind(Mark^.Data.Kind)+
        IntToStr(NLine+1);

      ListItems.AddObject(Prop.MenuCaption, Prop);
    end;
  end;
  //
var
  Form: TfmMenuApi;
  CurFrame, Frame: TEditorFrame;
  Prop: TAppBookmarkProp;
  MenuCaption: string;
  CurLineIndex, SelIndex, i: integer;
begin
  CurFrame:= CurrentFrame;
  CurLineIndex:= CurFrame.Editor.Carets[0].PosY;
  SelIndex:= 0;

  with TIniFile.Create(AppFile_Language) do
  try
    MenuCaption:= ReadString('m_sr', 'b_', 'Bookmarks');
    MenuCaption:= StringReplace(MenuCaption, '&', '', [rfReplaceAll]);
  finally
    Free;
  end;

  ListItems:= TStringList.Create;
  try
    ListItems.OwnsObjects:= true;

    AddItemsOfFrame(CurFrame);

    for i:= ListItems.Count-1 downto 0 do
      if TAppBookmarkProp(ListItems.Objects[i]).LineIndex <= CurLineIndex then
      begin
        SelIndex:= i;
        Break;
      end;

    //add bookmarks of all other frames
    for i:= 0 to FrameCount-1 do
    begin
      Frame:= Frames[i];
      if Frame<>CurFrame then
        AddItemsOfFrame(Frame);
    end;

    if ListItems.Count=0 then
    begin
      MsgStatus(msgCannotFindBookmarks);
      Exit;
    end;

    Form:= TfmMenuApi.Create(nil);
    try
      for i:= 0 to ListItems.Count-1 do
        Form.listItems.Add(ListItems[i]);

      UpdateInputForm(Form);

      Form.ListCaption:= MenuCaption;
      Form.Multiline:= false;
      Form.InitItemIndex:= SelIndex;
      Form.DisableFuzzy:= not UiOps.ListboxFuzzySearch;
      Form.DisableFullFilter:= true;

      Form.ShowModal;
      SelIndex:= Form.ResultCode;
    finally
      Form.Free;
    end;

    if SelIndex<0 then
    begin
      MsgStatus(msgStatusCancelled);
      Exit
    end;

    Prop:= TAppBookmarkProp(ListItems.Objects[SelIndex]);
    SetFrame(Prop.Frame);
    Prop.Frame.SetFocus;
    Prop.Ed.DoGotoPos(
      Point(0, Prop.LineIndex),
      Point(-1, -1),
      UiOps.FindIndentHorz,
      UiOps.FindIndentVert,
      true,
      TATEditorActionIfFolded.Unfold
      );
  finally
    FreeAndNil(ListItems);
  end;
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


procedure UpdateMenuEnabled(AItem: TMenuItem; AValue: boolean); inline;
begin
  if Assigned(AItem) then
    AItem.Enabled:= AValue;
end;

procedure UpdateMenuChecked(AItem: TMenuItem; AValue: boolean); inline;
begin
  if Assigned(AItem) then
    AItem.Checked:= AValue;
end;

procedure TfmMain.UpdateGroupsOfContextMenu;
var
  CurForm: TForm;
begin
  GroupsCtx:= Groups;
  GroupsCtxIndex:= GroupsCtx.FindPages(GroupsCtx.PopupPages);

  if FloatGroups then
  begin
    CurForm:= Screen.ActiveForm;
    if CurForm=FFormFloatGroups1 then
    begin
      GroupsCtx:= GroupsF1;
      GroupsCtxIndex:= 6;
    end
    else
    if CurForm=FFormFloatGroups2 then
    begin
      GroupsCtx:= GroupsF2;
      GroupsCtxIndex:= 7;
    end
    else
    if CurForm=FFormFloatGroups3 then
    begin
      GroupsCtx:= GroupsF3;
      GroupsCtxIndex:= 8;
    end;
  end;
end;

procedure TfmMain.PopupTabPopup(Sender: TObject);
var
  Frame: TEditorFrame;
  NVis, NCur: Integer;
begin
  UpdateGroupsOfContextMenu;

  Frame:= FrameOfPopup;
  NCur:= GroupsCtxIndex;
  NVis:= Groups.PagesVisibleCount; //visible groups

  UpdateMenuEnabled(mnuTabMove1, ((NVis>=2) and (NCur<>0)) or (NCur>5));
  UpdateMenuEnabled(mnuTabMove2, {(NVis>=2) and} (NCur<>1));
  UpdateMenuEnabled(mnuTabMove3, (NVis>=3) and (NCur<>2));
  UpdateMenuEnabled(mnuTabMove4, (NVis>=4) and (NCur<>3));
  UpdateMenuEnabled(mnuTabMove5, (NVis>=5) and (NCur<>4));
  UpdateMenuEnabled(mnuTabMove6, (NVis>=6) and (NCur<>5));
  UpdateMenuEnabled(mnuTabMoveF1, (NCur<>6));
  UpdateMenuEnabled(mnuTabMoveF2, (NCur<>7));
  UpdateMenuEnabled(mnuTabMoveF3, (NCur<>8));
  UpdateMenuEnabled(mnuTabMoveNext, (NVis>=2) and (NCur<6));
  UpdateMenuEnabled(mnuTabMovePrev, (NVis>=2) and (NCur<6));
  UpdateMenuChecked(mnuTabPinned, Assigned(Frame) and Frame.TabPinned);
end;

procedure TfmMain.PythonEngineAfterInit(Sender: TObject);
var
  Dirs: array of string;
  {$ifdef windows}
  dir: string;
  {$endif}
  PathAppend: boolean;
begin
  AppPython.Initialize;
  AppVariantInitializePython;
  Dirs:= nil;

  {$ifdef windows}
  PathAppend:= false;
  dir:= ExtractFileDir(Application.ExeName)+DirectorySeparator;
  SetLength(Dirs, 2);
  Dirs[0]:= dir+ChangeFileExt(UiOps.PyLibrary, 'dlls');
  Dirs[1]:= dir+ChangeFileExt(UiOps.PyLibrary, '.zip');
  {$else}
  PathAppend:= true;
  {$endif}

  //add to sys.path folders py/, py/sys/
  Dirs:= Concat(Dirs, [AppDir_Py, AppDir_Py+DirectorySeparator+'sys']);

  AppPython.SetPath(Dirs, PathAppend);

  AppPython.Exec('import os');
  AppPython.Exec('for k in os.environ.keys():k.upper().startswith("GIT_") and os.environ.pop(k)');
end;

procedure TfmMain.InitPyEngine;
var
  NTick: QWord;
begin
  NTick:= GetTickCount64;

  {$ifdef windows}
  Windows.SetEnvironmentVariable('PYTHONIOENCODING', 'UTF-8');
  {$endif}

  PythonIO:= TPythonInputOutput.Create(Self);
  PythonIO.MaxLineLength:= 2000;
  PythonIO.OnSendUniData:= @PythonIOSendUniData;
  PythonIO.UnicodeIO:= True;
  PythonIO.RawOutput:= False;

  PythonEng:= TPythonEngine.Create(Self);
  {$ifdef LCLGTK2}
  PythonEng.AutoFinalize:= false; //fix by @z4ziggy: quitting the app causes hanging and crash report
  {$endif}
  PythonEng.AutoLoad:= false;
  PythonEng.FatalAbort:= false;
  PythonEng.FatalMsgDlg:= false;
  PythonEng.PyFlags:= [pfIgnoreEnvironmentFlag];
  PythonEng.OnAfterInit:= @PythonEngineAfterInit;
  PythonEng.IO:= PythonIO;

  PythonModule:= TPythonModule.Create(Self);
  PythonModule.Engine:= PythonEng;
  PythonModule.ModuleName:= 'cudatext_api';
  PythonModule.OnInitialization:= @PythonModuleInitialization;

  if (UiOps.PyLibrary='') or
    ((Pos('/', UiOps.PyLibrary)>0) and not FileExists(UiOps.PyLibrary)) then
  begin
    MsgLogConsole(msgCannotInitPython1);
    MsgLogConsole(msgCannotInitPython2);
    if msgCannotInitPython2b<>'' then
      MsgLogConsole(msgCannotInitPython2b);
    DisablePluginMenuItems(
      {$if defined(windows) or defined(darwin)}
      false
      {$else}
      true //UiOps.PyLibrary<>''
      {$endif});
    exit;
  end;

  PythonEng.UseLastKnownVersion:= False;
  PythonEng.DllPath:= ExtractFilePath(UiOps.PyLibrary);
  PythonEng.DllName:= ExtractFileName(UiOps.PyLibrary);
  PythonEng.LoadDll;

  if not AppPython.Inited then
  begin
    FConsoleMustShow:= true;
    MsgLogConsole(msgCannotInitPython1);
    MsgLogConsole(msgCannotInitPython2);
    if msgCannotInitPython2b<>'' then
      MsgLogConsole(msgCannotInitPython2b);
    DisablePluginMenuItems(true);
  end
  else
  if UiOps.LogConsoleDetailedStartupTime then
  begin
    NTick:= GetTickCount64-NTick;
    MsgLogConsole(Format('Loaded Python library: %dms', [NTick]));
  end;
end;

procedure TfmMain.DisablePluginMenuItems(AddFindLibraryItem: boolean);
{$ifndef windows}
var
  mi: TMenuItem;
{$endif}
begin
  if Assigned(mnuOpPlugins) then
    mnuOpPlugins.Enabled:= false;

  if AddFindLibraryItem then
    if Assigned(mnuPlugins) then
    begin
      {$ifdef windows}
      mnuPlugins.Enabled:= false;
      {$else}
      mi:= TMenuItem.Create(Self);
      mi.Caption:= msgPythonFindCaptionLong;
      mi.OnClick:= @DoOps_FindPythonLib;
      mnuPlugins.Clear;
      mnuPlugins.Add(mi);
      {$endif}
    end;
end;


procedure TfmMain.SetFrameEncoding(Ed: TATSynEdit; const AEnc: string;
  AAlsoReloadFile: boolean);
var
  Frame: TEditorFrame;
  bBadUTF8: boolean;
begin
  Frame:= TGroupsHelper.GetEditorFrame(Ed);
  if Frame=nil then exit;

  if SameText(Ed.EncodingName, AEnc) then exit;
  Ed.EncodingName:= AEnc;
  bBadUTF8:= false;

  if AAlsoReloadFile then
  begin
    if Frame.GetFileName(Ed)<>'' then
    begin
      Frame.DoFileReload_DisableDetectEncoding(Ed);
      bBadUTF8:= Ed.Strings.LoadingForcedANSI and (Ed.Strings.Encoding=TATFileEncoding.ANSI);
    end
    else
      MsgBox(msgCannotReloadUntitledTab, MB_OK or MB_ICONWARNING);
  end
  else
  begin
    //set modified to allow save
    Ed.Modified:= true;
  end;

  Ed.DoEventChange(0); //reanalyze all file
  UpdateFrameEx(Frame, false);
  UpdateStatusbar;

  if bBadUTF8 then
    MsgStatus(msgCannotLoadFileInUTF8)
  else
    MsgStatus(msgStatusEncChanged);
end;

procedure TfmMain.MenuLexerClick(Sender: TObject);
var
  F: TEditorFrame;
  obj: TObject;
  SName: string;
begin
  F:= CurrentFrame;
  obj:= TObject((Sender as TComponent).Tag);

  if obj is TecSyntAnalyzer then
    SName:= (obj as TecSyntAnalyzer).LexerName
  else
  if obj is TATLiteLexer then
    SName:= (obj as TATLiteLexer).LexerName+msgLiteLexerSuffix
  else
    SName:= '';

  F.LexerName[F.Editor]:= SName;

  //if some lexer selected, OnParseDone will update the tree
  //if (none) lexer selected, update tree manually
  if SName='' then
    UpdateTreeByTimer;

  UpdateFrameEx(F, false);
  UpdateStatusbar;
end;


procedure TfmMain.DoOps_LexersBackupSave;
var
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
    Frames[i].LexerBackupSave;
end;

procedure TfmMain.DoOps_LexersBackupRestore;
var
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
    Frames[i].LexerBackupRestore;
end;


procedure TfmMain.DoOps_LoadLexerLib(AOnCreate: boolean);
var
  bKeepFrameLexers: boolean;
begin
  bKeepFrameLexers:= not AOnCreate;
  if bKeepFrameLexers then
    DoOps_LexersBackupSave;

  AppLoadLexers;

  if bKeepFrameLexers then
    DoOps_LexersBackupRestore;
end;


procedure TfmMain.UpdateMenuLexersTo(AMenu: TMenuItem);
var
  sl: TStringList;
  an: TecSyntAnalyzer;
  an_lite: TATLiteLexer;
  mi, mi0: TMenuItem;
  ch, ch0: char;
  i: integer;
begin
  if AMenu=nil then exit;
  AMenu.Clear;

  ch0:= '?';
  mi0:= nil;

  mi:= TMenuItem.Create(self);
  mi.Caption:= msgNoLexer;
  mi.OnClick:= @MenuLexerClick;
  AMenu.Add(mi);

  sl:= TStringList.Create;
  try
    //make stringlist of all lexers
    for i:= 0 to AppManager.LexerCount-1 do
    begin
      an:= AppManager.Lexers[i];
      if an.Deleted then Continue;
      if not an.Internal then
        sl.AddObject(an.LexerName, an);
    end;

    for i:= 0 to AppManagerLite.LexerCount-1 do
    begin
      an_lite:= AppManagerLite.Lexers[i];
      sl.AddObject(an_lite.LexerName+msgLiteLexerSuffix, an_lite);
    end;
    sl.Sort;

    //put stringlist to menu
    if not UiOps.LexerMenuGrouped then
    begin
      for i:= 0 to sl.Count-1 do
      begin
        if sl[i]='' then Continue;
        mi:= TMenuItem.Create(self);
        mi.Caption:= sl[i];
        mi.Tag:= PtrInt(sl.Objects[i]);
        mi.OnClick:= @MenuLexerClick;
        AMenu.Add(mi);
      end;
    end
    else
    //grouped view
    for i:= 0 to sl.Count-1 do
    begin
      if sl[i]='' then Continue;
      ch:= UpCase(sl[i][1]);
      if ch<>ch0 then
      begin
        ch0:= ch;
        mi0:= TMenuItem.Create(self);
        mi0.Caption:= ch;
        AMenu.Add(mi0);
      end;

      mi:= TMenuItem.Create(self);
      mi.Caption:= sl[i];
      mi.Tag:= PtrInt(sl.Objects[i]);
      mi.OnClick:= @MenuLexerClick;
      if Assigned(mi0) then
        mi0.Add(mi)
      else
        AMenu.Add(mi);
    end;
  finally
    FreeAndNil(sl);
  end;
end;

function TfmMain.GetStatusbarPrefix(Frame: TEditorFrame): string;
begin
  Result:= '';
  if Frame=nil then exit;
  if Frame.FrameKind=TAppFrameKind.Editor then
  begin
    //if Frame.ReadOnly[Frame.Editor] then
    //  Result+= msgStatusReadonly+' ';

    //if Frame.MacroRecord then
    //  Result+= msgStatusMacroRec+' ';
  end;
end;

procedure TfmMain.MsgStatus(AText: string; AFinderMessage: boolean=false);
var
  STime: string;
begin
  SReplaceAll(AText, #10, ' ');
  SReplaceAll(AText, #13, ' ');

  if DoPyEvent_Message(AText) then
  begin
    if AText='' then
    begin
      FLastStatusbarMessage:= '';
      DoStatusbarTextByTag(Status, StatusbarTag_Msg, '');
      exit;
    end;

    STime:= FormatDateTime('[HH:mm] ', Now);
    AppStatusbarMessages.Add(STime+AText);
    while AppStatusbarMessages.Count>UiOps.MaxMaxStatusbarMessages do
      AppStatusbarMessages.Delete(0);
    FLastStatusbarMessage:= AText;

    DoStatusbarTextByTag(Status, StatusbarTag_Msg, {STime+}GetStatusbarPrefix(CurrentFrame)+AText);
    DoStatusbarColorByTag(Status, StatusbarTag_Msg, GetAppColorOfStatusbarFont);
    DoStatusbarHintByTag(Status, StatusbarTag_Msg, StringsTrailingText(AppStatusbarMessages, UiOps.MaxStatusbarMessages));

    TimerStatusClear.Enabled:= false;
    TimerStatusClear.Enabled:= true;
  end;

  if AFinderMessage then
    if Assigned(fmFind) then
      fmFind.UpdateCaption(AText);
end;

procedure TfmMain.DoTooltipShow(const AText: string; ASeconds: integer;
  APosition: TAppTooltipPos; AGotoBracket: boolean; APosX, APosY: integer);
var
  Ed: TATSynEdit;
  WorkRect: TRect;
  NCellSize, NSizeX, NSizeY: integer;
  TempX, TempY: integer;
  P: TPoint;
  PntCoord: TATPoint;
begin
  if ASeconds<=0 then
  begin
    DoTooltipHide;
    Exit
  end;
  ASeconds:= Min(ASeconds, UiOps.AltTooltipTimeMax);

  //not Screen.WorkAreaRect, see issue #3866
  WorkRect:= Screen.DesktopRect;

  if FFormTooltip=nil then
  begin
    FFormTooltip:= TForm.CreateNew(nil);
    FFormTooltip.BorderStyle:= bsNone;
    FFormTooltip.ShowInTaskBar:= stNever;
    FFormTooltip.FormStyle:= fsSystemStayOnTop;
    FFormTooltip.ControlStyle:= FFormTooltip.ControlStyle+[csNoFocus]; //recommended by Zeljko in Laz bugtracker

    FTooltipPanel:= TAppPanelEx.Create(FFormTooltip);
    FTooltipPanel.Align:= alClient;
    FTooltipPanel.Parent:= FFormTooltip;
    FTooltipPanel.PaddingX:= UiOps.AltTooltipPaddingX;
    FTooltipPanel.PaddingY:= UiOps.AltTooltipPaddingY;
  end;

  FTooltipPanel.Font.Name:= EditorOps.OpFontName;
  FTooltipPanel.Font.Size:= ATEditorScaleFont(EditorOps.OpFontSize);
  FTooltipPanel.Font.Color:= clInfoText;
  FTooltipPanel.Color:= clInfoBk;
  FTooltipPanel.ColorFrame:= ColorBlendHalf(ColorToRGB(clInfoBk), ColorToRGB(clInfoText));
  FTooltipPanel.Caption:= AText;

  P:= Canvas_TextMultilineExtent(FTooltipPanel.Canvas, AText);

  NSizeX:= P.X + 2*UiOps.AltTooltipPaddingX;
  NSizeY:= P.Y + 2*UiOps.AltTooltipPaddingY;
  FFormTooltip.ClientWidth:= NSizeX;
  FFormTooltip.ClientHeight:= NSizeY;

  case APosition of
    TAppTooltipPos.WindowTop:
      begin
        P:= Self.ClientToScreen(Point(0, 0));
      end;
    TAppTooltipPos.WindowBottom:
      begin
        P:= Status.ClientToScreen(Point(0, 0));
      end;
    TAppTooltipPos.EditorCaret,
    TAppTooltipPos.CustomTextPos:
      begin
        Ed:= CurrentEditor;
        NCellSize:= Ed.TextCharSize.Y;
        if APosition=TAppTooltipPos.EditorCaret then
        begin
          if Ed.Carets.Count=0 then exit;
          P.X:= Ed.Carets[0].PosX;
          P.Y:= Ed.Carets[0].PosY;
        end
        else
        begin
          P.X:= APosX;
          P.Y:= APosY;
        end;
        FLastTooltipLine:= P.Y;
        if AGotoBracket then
        begin
          EditorBracket_FindOpeningBracketBackward(Ed,
            P.X, P.Y,
            '()',
            EditorOps.OpBracketDistance,
            TempX, TempY);
          if TempX>=0 then
            P.X:= TempX+1;
          if TempY>=0 then
            P.Y:= TempY;
        end;
        PntCoord:= Ed.CaretPosToClientPos(P);
        if PntCoord.Y<0 then exit;
        if not ATPointInRect(Ed.ClientRect, PntCoord) then exit;
        P:= Point(PntCoord.X, PntCoord.Y);
        P:= Ed.ClientToScreen(P);
        Dec(P.Y, NSizeY);
        if P.Y<=WorkRect.Top then
          Inc(P.Y, NSizeY+NCellSize);
      end;
  end;

  P.X:= Min(P.X, WorkRect.Right-NSizeX);
  P.Y:= Min(P.Y, WorkRect.Bottom-NSizeY);
  FFormTooltip.Left:= P.X;
  FFormTooltip.Top:= P.Y;

  FFormTooltip.Show;
  //get focus back from FFormTooltip
  LCLIntf.SetForegroundWindow(Self.Handle);

  TimerTooltip.Interval:= ASeconds*1000;
  TimerTooltip.Enabled:= false;
  TimerTooltip.Enabled:= true;
end;

procedure TfmMain.SetShowMenu(AValue: boolean);
begin
  if FMenuVisible=AValue then exit;
  FMenuVisible:= AValue;

  if AValue then
    Menu:= MainMenu
  else
    Menu:= nil;

  {$ifdef windows}
  //workaround for LCL strange bug, when hiding MainMenu causes app hang on pressing Alt
  if AValue then
    Windows.SetMenu(Handle, MainMenu.Handle)
  else
    Windows.SetMenu(Handle, 0);
  {$endif}

  UpdateMenuSidebarButton(not FMenuVisible);
end;

function TfmMain.GetShowOnTop: boolean;
begin
  Result:= UiOps.ShowFormsOnTop;
end;

procedure TfmMain.SetShowOnTop(AValue: boolean);
begin
  UiOps.ShowFormsOnTop:= AValue;
  UpdateFormOnTop(Self);
  UpdateStatusbar;
end;

procedure TfmMain.SetSidebarPanel(const ACaption: string);
begin
  if (ACaption<>'-') and (ACaption<>'') then
    if AppPanels[TAppPanelId.Side].Visible then
      AppPanels[TAppPanelId.Side].UpdatePanels(ACaption, true, true);
end;

procedure TfmMain.SetShowSideBar(AValue: boolean);
begin
  PanelSide.Visible:= AValue;
end;

function TfmMain.GetShowSidebarOnRight: boolean;
begin
  Result:= PanelSide.Align=alRight;
end;

procedure TfmMain.SetShowSidebarOnRight(AValue: boolean);
const
  cVal: array[boolean] of TAlign = (alLeft, alRight);
begin
  if AValue=GetShowSidebarOnRight then exit;
  PanelSide.Align:= cVal[AValue];
  AppPanels[TAppPanelId.Side].Align:= cVal[AValue];
end;

procedure TfmMain.SetShowStatus(AValue: boolean);
begin
  Status.Visible:= AValue;
end;

procedure TfmMain.SetShowToolbar(AValue: boolean);
begin
  if AValue=GetShowToolbar then exit;

  if AValue then
    ToolbarMain.UpdateControls;

  ToolbarMain.Visible:= AValue;
end;

function TfmMain.DoFileSaveAll: boolean;
var
  F: TEditorFrame;
  i: integer;
begin
  Result:= true;
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    if F.Editor.Modified then
      if not F.DoFileSave(false, true) then
        Result:= false;
  end;
end;

procedure TfmMain.DoFileReopenRecent;
var
  fn: string;
  i: integer;
begin
  //prevent crash
  if AppOpeningFile then exit;

  for i:= 0 to AppListRecents.Count-1 do
  begin
    fn:= AppExpandHomeDirInFilename(AppListRecents[i]);
    if FileExists(fn) then
      if FindFrameOfFilename(fn)=nil then
      begin
        DoFileOpen(fn, '');
        exit;
      end;
  end;
end;

procedure TfmMain.DoFileReopen(F: TEditorFrame; Ed: TATSynEdit);
var
  fn: string;
  bPrevRO, bChangedRO: boolean;
  PrevLexer: string;
begin
  if F=nil then exit;

  fn:= F.GetFileName(Ed);
  if fn='' then exit;

  if not FileExists(fn) then
  begin
    MsgStatus(msgCannotFindFile+' '+ExtractFileName(fn));
    exit;
  end;

  if Ed.Modified and UiOps.ReloadUnsavedConfirm then
    if MsgBox(
      Format(msgConfirmReopenModifiedTab, [AppCollapseHomeDirInFilename(fn)]),
      MB_OKCANCEL or MB_ICONQUESTION
      ) <> ID_OK then exit;

  bChangedRO:= TATEditorModifiedOption.ReadOnly in Ed.ModifiedOptions;
  bPrevRO:= F.ReadOnly[Ed];
  PrevLexer:= F.LexerName[Ed];
  F.ReadOnly[Ed]:= false;
  F.DoFileReload(Ed);
  F.LexerName[Ed]:= PrevLexer;
  if bChangedRO then
    F.ReadOnly[Ed]:= bPrevRO;
  Ed.Modified:= false;

  UpdateStatusbar;
  MsgStatus(msgStatusReopened+' '+ExtractFileName(fn));
end;

function TfmMain.ConfirmAllFramesAreSaved(AWithCancel: boolean): boolean;
var
  MsgFlags: integer;
  F: TEditorFrame;
  ListNoSave: TFPList;
  bModified: boolean;
  NCount, i: integer;
begin
  NCount:= FrameCount;
  if (NCount=1) and (Frames[0].IsEmpty) then
    exit(true);

  if AWithCancel then
    MsgFlags:= MB_YESNOCANCEL or MB_ICONQUESTION
  else
    MsgFlags:= MB_YESNO or MB_ICONQUESTION;

  ListNoSave:= TFPList.Create;
  try
    for i:= 0 to NCount-1 do
    begin
      F:= Frames[i];
      bModified:= F.Modified;
      if bModified then
        case MsgBox(
               Format(msgConfirmSaveModifiedTab, [F.TabCaption]),
               MsgFlags) of
          ID_YES:
            begin
              //Cancel in "Save as" dlg must be global cancel
              if not F.DoFileSave(false, true) then
                exit(false);
            end;
          ID_NO:
            ListNoSave.Add(F);
          ID_CANCEL:
            exit(false);
        end;
    end;

    for i:= 0 to ListNoSave.Count-1 do
    begin
      F:= TEditorFrame(ListNoSave[i]);
      F.Ed1.Modified:= false;
      if not F.EditorsLinked then
        F.Ed2.Modified:= false;
    end;
  finally
    FreeAndNil(ListNoSave);
  end;

  Result:= true;
end;

function TfmMain.DoFileCloseAll(AWithCancel, AClosePinned: boolean): boolean;
var
  F: TEditorFrame;
begin
  if not ConfirmAllFramesAreSaved(AWithCancel) then
    exit(false);

  //focus 1st tab (fixes appearing of empty tab on loading session with active group 2)
  if FrameCount>0 then
  begin
    F:= Frames[0];
    F.SetFocus;
    Groups.PagesCurrent:= Groups.Pages1;
  end;

  DoCloseAllTabs(AClosePinned);
  Result:= true;
end;


procedure TfmMain.DoFileCloseAndDelete(Ed: TATSynEdit);
var
  Frame: TEditorFrame;
  fn, fnPic: string;
begin
  Frame:= TGroupsHelper.GetEditorFrame(Ed);
  if Frame=nil then exit;

  CloseFormAutoCompletion;

  if not Frame.EditorsLinked then
  begin
    MsgStatus(msgCannotHandleSplittedTab);
    exit;
  end;

  fn:= Frame.GetFileName(Ed);
  if fn='' then
  begin
    MsgStatus(msgCannotHandleUntitledTab);
    exit;
  end;

  if Ed.Modified then
    Ed.Modified:= false;

  if MsgBox(
       msgConfirmCloseAndDeleteFile+#10+AppCollapseHomeDirInFilename(fn),
       MB_OKCANCEL or MB_ICONWARNING)=ID_OK then
    if Groups.CloseTabs(tabCloseCurrent, false, true) then
    begin
      DeleteFileUTF8(fn);

      //delete helper file from 'Insert Pics' plugin
      fnPic:= fn+'.cuda-pic';
      if FileExists(fnPic) then
        DeleteFileUTF8(fnPic);

      MenuRecent_RemoveFilename(fn);

      DoPyEvent(nil, TAppPyEvent.OnDeleteFile, [AppVariant(fn)]);
    end;
end;

procedure TfmMain.DoFileNew;
var
  Frame: TEditorFrame;
begin
  Frame:= DoFileOpen('', '');
  DoApplyNewdocLexer(Frame);
end;

procedure TfmMain.DoApplyNewdocLexer(F: TEditorFrame);
var
  Ed: TATSynEdit;
begin
  //call this for empty NewdocLexer too: to apply lexer-specific config for none-lexer
  if Assigned(F) then
  begin
    Ed:= F.Ed1;
    F.LexerName[Ed]:= UiOps.NewdocLexer;

    if UiOps.NewdocLexer<>'' then
      if (F.Lexer[Ed]=nil) and (F.LexerLite[Ed]=nil) then
        MsgLogConsole(Format(msgBadLexerName, [UiOps.NewdocLexer]));
  end;
end;

procedure TfmMain.MenuRecentItemClick(Sender: TObject);
var
  fn: string;
  n: integer;
begin
  n:= (Sender as TComponent).Tag;
  fn:= AppExpandHomeDirInFilename(AppListRecents[n]);
  if FileExists(fn) then
    DoFileOpen(fn, '')
  else
  begin
    MsgBox(msgCannotFindFile+#10+AppCollapseHomeDirInFilename(fn), MB_OK or MB_ICONERROR);
    AppListRecents.Delete(n);
    UpdateMenuRecent(nil);
  end;
end;

procedure TfmMain.MenuRecent_RemoveFilename(const fn: string);
var
  n: integer;
begin
  n:= AppListRecents.IndexOf(AppCollapseHomeDirInFilename(fn));
  if n>=0 then
  begin
    AppListRecents.Delete(n);
    UpdateMenuRecent(nil);
  end;
end;

{
procedure TfmMain.DoToggleMenu;
begin
  ShowMenu:= not ShowMenu;
end;
}

procedure TfmMain.DoToggleFloatSide;
begin
  with AppPanels[TAppPanelId.Side] do
    Floating:= not Floating;
end;

procedure TfmMain.DoToggleFloatBottom;
begin
  with AppPanels[TAppPanelId.Btm] do
    Floating:= not Floating;
end;

procedure TfmMain.DoToggleOnTop;
var
  Form: TCustomForm;
begin
  //support floating groups
  Form:= GetParentForm(CurrentGroups);
  if Form=Self then
    ShowOnTop:= not ShowOnTop
  else
  begin
    if Form.FormStyle=fsNormal then
      Form.FormStyle:= fsSystemStayOnTop
    else
      Form.FormStyle:= fsNormal;
  end;
end;

procedure TfmMain.DoToggleFullScreen;
begin
  ShowFullscreen:= not ShowFullscreen;
end;

procedure TfmMain.DoToggleDistractionFree;
begin
  ShowDistractionFree:= not ShowDistractionFree;
end;

procedure TfmMain.DoToggleSidePanel;
begin
  with AppPanels[TAppPanelId.Side] do
  begin
    Visible:= not Visible;
    if not Visible then
      if ActiveControl=nil then
        DoFocusEditor(CurrentEditor);
  end;
end;

procedure TfmMain.DoToggleBottomPanel;
begin
  with AppPanels[TAppPanelId.Btm] do
  begin
    Visible:= not Visible;
    if not Visible then
      if ActiveControl=nil then
        DoFocusEditor(CurrentEditor);
  end;
end;

procedure TfmMain.DoToggleSidebar;
begin
  ShowSideBar:= not ShowSideBar;
  DoOps_SaveOptionBool('/ui_sidebar_show', ShowSideBar);
end;

procedure TfmMain.DoToggleToolbar;
begin
  ShowToolbar:= not ShowToolbar;
  DoOps_SaveOptionBool('/ui_toolbar_show', ShowToolbar);
end;

procedure TfmMain.DoToggleStatusbar;
begin
  ShowStatus:= not ShowStatus;
  DoOps_SaveOptionBool('/ui_statusbar_show', ShowStatus);
end;

procedure TfmMain.DoToggleUiTabs;
begin
  ShowTabsMain:= not ShowTabsMain;
  DoOps_SaveOptionBool('/ui_tab_show', ShowTabsMain);
end;

procedure TfmMain.DoPyCommand_Cudaxlib(Ed: TATSynEdit; const AMethod: string;
  AInvoke: TATCommandInvoke);
begin
  Ed.Strings.BeginUndoGroup;
  try
    DoPyCommand('cudax_lib', AMethod, [], AInvoke);
  finally
    Ed.Strings.EndUndoGroup;
  end;
end;


procedure TfmMain.DoShowConsole(AndFocus: boolean);
begin
  AppPanels[TAppPanelId.Btm].UpdatePanels(msgPanelConsole_Init, AndFocus, true);
end;

procedure TfmMain.DoShowOutput(AndFocus: boolean);
begin
  AppPanels[TAppPanelId.Btm].UpdatePanels(msgPanelOutput_Init, AndFocus, true);
end;

procedure TfmMain.DoShowValidate(AndFocus: boolean);
begin
  AppPanels[TAppPanelId.Btm].UpdatePanels(msgPanelValidate_Init, AndFocus, true);
end;

procedure TfmMain.SetShowFullScreen(AValue: boolean);
begin
  if FShowFullScreen=AValue then Exit;
  FShowFullScreen:= AValue;
  FShowFullScreen_DisFree:= false;
  SetFullScreen_Ex(AValue, false);
  DoPyEvent_AppState(APPSTATE_WINDOW);
end;

procedure TfmMain.SetShowDistractionFree(AValue: boolean);
begin
  if GetShowDistractionFree=AValue then Exit;
  FShowFullScreen:= AValue;
  FShowFullScreen_DisFree:= AValue;
  SetFullScreen_Ex(AValue, true);
  DoPyEvent_AppState(APPSTATE_WINDOW);
end;

procedure TfmMain.SetShowDistractionFree_Forced;
begin
  SetFullScreen_Ex(true, true);
end;

function TfmMain.GetShowDistractionFree: boolean;
begin
  Result:= FShowFullScreen and FShowFullScreen_DisFree;
end;


procedure TfmMain.DoApplyGutterVisible(AValue: boolean);
var
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
    with Frames[i] do
    begin
      Ed1.OptGutterVisible:= AValue;
      Ed2.OptGutterVisible:= AValue;
    end;
end;

procedure TfmMain.SetFullScreen_Ex(AValue: boolean; AHideAll: boolean);
  //
  procedure FullScreen_ForDistractionFree(Ed: TATSynEdit);
  begin
    Ed.OptMinimapVisible:= false;
    Ed.OptMicromapVisible:= false;
    Ed.Update;
  end;
  //
  procedure FullScreen_Restore(Ed: TATSynEdit);
  begin
    Ed.OptMinimapVisible:= EditorOps.OpMinimapShow;
    Ed.OptMicromapVisible:= EditorOps.OpMicromapShow;
    Ed.OptMicromapOnScrollbar:= EditorOps.OpMicromapOnScrollbar;
    Ed.Update;
  end;
  //
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if AValue then
  begin
    FOrigShowToolbar:= ShowToolbar;
    FOrigShowStatusbar:= ShowStatus;
    FOrigShowBottom:= AppPanels[TAppPanelId.Btm].Visible;
    FOrigShowSidePanel:= AppPanels[TAppPanelId.Side].Visible;
    FOrigShowSideBar:= ShowSideBar;
    FOrigShowTabs:= ShowTabsMain;

    if AHideAll then
    begin
      FullScreen_ForDistractionFree(F.Ed1);
      FullScreen_ForDistractionFree(F.Ed2);
    end;

    if AHideAll or (Pos('t', UiOps.FullScreen)>0) then ShowToolbar:= false;
    if AHideAll or (Pos('b', UiOps.FullScreen)>0) then AppPanels[TAppPanelId.Btm].Visible:= false;
    if AHideAll or (Pos('i', UiOps.FullScreen)>0) then ShowStatus:= false;
    if AHideAll or (Pos('p', UiOps.FullScreen)>0) then AppPanels[TAppPanelId.Side].Visible:= false;
    if AHideAll or (Pos('a', UiOps.FullScreen)>0) then ShowSideBar:= false;
    if AHideAll or (Pos('u', UiOps.FullScreen)>0) then ShowTabsMain:= false;
    if AHideAll or (Pos('g', UiOps.FullScreen)>0) then DoApplyGutterVisible(false);
  end
  else
  begin
    ShowToolbar:= FOrigShowToolbar;
    ShowStatus:= FOrigShowStatusbar;
    AppPanels[TAppPanelId.Btm].Visible:= FOrigShowBottom;
    AppPanels[TAppPanelId.Side].Visible:= FOrigShowSidePanel;
    ShowSideBar:= FOrigShowSideBar;
    ShowTabsMain:= FOrigShowTabs;

    FullScreen_Restore(F.Ed1);
    FullScreen_Restore(F.Ed2);

    DoApplyGutterVisible(EditorOps.OpGutterShow);
  end;

  {$ifdef windows}
  SetFullScreen_Win32(AValue);

  if not AValue then
    ApplyFormDarkTitle(Self, IsColorDark(GetAppColor(TAppThemeColor.TabBg)), true);
  {$else}
  SetFullScreen_Universal(AValue);
  {$endif}
end;

procedure TfmMain.SetFullScreen_Universal(AValue: boolean);
begin
  //works ok on GTK2 and macOS
  //WindowState:=wsFullScreen works ok, but WindowState:=wsNormal don't restore form size on GTK2
  if AValue then
    ShowWindow(Handle, SW_SHOWFULLSCREEN)
  else
    ShowWindow(Handle, SW_SHOWNORMAL);
end;

procedure TfmMain.SetFullScreen_Win32(AValue: boolean);
begin
  if AValue then
  begin
    //hide MainMenu in full-screen, to fix issues #5090 and #4857
    //do it only on Windows (Linux gtk2 will have a problem - app cannot fully return from full-screen mode)
    ShowMenu:= false;

    FOrigWndState:= WindowState;
    FOrigBounds:= BoundsRect;
    BorderStyle:= bsNone;
    BoundsRect:= Monitor.BoundsRect;
  end
  else
  begin
    DoControlLock(Self); //reduces flickering with dark ui-theme
    ShowMenu:= UiOps.ShowMenuBar;
    WindowState:= FOrigWndState;
    BoundsRect:= FOrigBounds;
    BorderStyle:= bsSizeable;
    BoundsRect:= FOrigBounds; //again
    Application.ProcessMessages; //reduces flickering with dark ui-theme
    DoControlUnlock(Self);
  end;

  UpdateMenuTheming_MainMenu;
  UpdateMenuTheming_WhiteLine;
end;

function TfmMain.GetShowTabsMain: boolean;
begin
  Result:= Groups.Pages1.Tabs.Visible;
end;

procedure TfmMain.SetShowTabsMain(AValue: boolean);
begin
  Groups.SetTabOption(tabOptionShowTabs, Ord(AValue));
end;

procedure TfmMain.DoFileNewFrom(const fn: string);
var
  F: TEditorFrame;
begin
  F:= DoFileOpen('', '');
  if F=nil then exit;
  F.Ed1.Strings.LoadFromFile(fn, []);
  F.DoLexerFromFilename(F.Ed1, fn);
  UpdateFrameEx(F, true);
  UpdateStatusbar;
end;

procedure TfmMain.DoFileSave(Frame: TEditorFrame; Ed: TATSynEdit);
var
  bSaveAs, bUntitled, bFileExists: boolean;
  SFilename: string;
begin
  if Frame=nil then exit;

  InitSaveDlg;
  DoFileDialog_PrepareDir(SaveDlg);

  bSaveAs:= false;
  SFilename:= Frame.GetFileName(Ed);
  bUntitled:= SFilename='';
  if bUntitled then
    bSaveAs:= true;
  bFileExists:= (SFilename<>'') and FileExists(SFilename);

  //if file not exists, it's moved during Cud work, we must recreate it (like ST3)
  if UiOps.AllowSaveOfUnmodifiedFile or
    (Ed.Modified or bUntitled or not bFileExists) then
  begin
    if Frame.DoFileSave_Ex(Ed, bSaveAs) then
      DoFileDialog_SaveDir(SaveDlg);
  end
  else
    MsgStatus(msgStatusSaveIsIgnored);
end;

procedure TfmMain.DoFileSaveAs(Frame: TEditorFrame; Ed: TATSynEdit);
begin
  if Frame=nil then exit;

  InitSaveDlg;
  DoFileDialog_PrepareDir(SaveDlg);

  if Frame.DoFileSave_Ex(Ed, true) then
    DoFileDialog_SaveDir(SaveDlg);
end;

procedure TfmMain.DoFocusEditor(Ed: TATSynEdit);
begin
  if Ed=nil then exit;
  if Ed.Visible and Ed.Enabled then
    Ed.SetFocus;
end;

procedure TfmMain.DoFocusFrame(F: TEditorFrame);
begin
  if Assigned(F) then
    if F.Visible and F.Enabled then
    begin
      SetFrame(F);
      F.SetFocus;
    end;
end;

procedure TfmMain.DoSwitchTabSimply(ANext: boolean);
begin
  CurrentGroups.PagesCurrent.Tabs.SwitchTab(ANext);
end;

procedure TfmMain.DoSwitchTab(ANext: boolean);
begin
  if UiOps.TabSwitcherDialog then
    DoDialogMenuTabSwitcher
  else
    DoSwitchTabSimply(ANext);
end;

procedure TfmMain.DoSwitchTabToRecent;
var
  Frame, CurFrame, NewFrame: TEditorFrame;
  Time: Int64;
  i: integer;
begin
  CurFrame:= CurrentFrame;
  if CurFrame=nil then exit;
  NewFrame:= nil;
  Time:= 0;

  for i:= 0 to FrameCount-1 do
  begin
    Frame:= Frames[i];
    if Frame=CurFrame then Continue;
    if Frame.Editor.ActivationTime>Time then
    begin
      Time:= Frame.Editor.ActivationTime;
      NewFrame:= Frame;
    end;
  end;

  if Assigned(NewFrame) then
  begin
    SetFrame(NewFrame);
    NewFrame.SetFocus;
  end;
end;


function TfmMain.FindFrameOfFilename(const AFileName: string; AllowEmptyPath: boolean=false): TEditorFrame;
var
  Frame: TEditorFrame;
  bEmptyPath, bOK: boolean;
  i: integer;
begin
  Result:= nil;
  if AFileName='' then exit;

  bEmptyPath:= ExtractFileDir(AFileName)='';
  if bEmptyPath and not AllowEmptyPath then exit;

  for i:= 0 to FrameCount-1 do
  begin
    Frame:= Frames[i];
    if bEmptyPath then
    begin
      bOK:= SameFileName(AFileName, ExtractFileName(Frame.FileName));
      if not bOK then
        if not Frame.EditorsLinked then
          bOK:= SameFileName(AFileName, ExtractFileName(Frame.FileName2));
    end
    else
    begin
      bOK:= SameFileName(AFileName, Frame.FileName);
      if not bOK then
        if not Frame.EditorsLinked then
          bOK:= SameFileName(AFileName, Frame.FileName2);
    end;
    if bOK then
      exit(Frame);
  end;
end;

function TfmMain.FindFrameOfPreviewTab: TEditorFrame;
var
  F: TEditorFrame;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    if F.TabIsPreview then exit(F);
  end;
end;


function TfmMain.DoCheckFilenameOpened(const AName: string): boolean;
begin
  Result:= Assigned(FindFrameOfFilename(AName));
end;

procedure TfmMain.DoOps_OpenFile_Default;
var
  F: TEditorFrame;
begin
  F:= DoFileOpen(AppFile_OptionsDefault, '');
  if Assigned(F) then
    F.ReadOnly[F.Ed1]:= true;
end;

procedure TfmMain.DoOps_OpenFile_User;
var
  fn: string;
begin
  fn:= AppFile_OptionsUser;
  if not FileExists(fn) then
  begin
    AppCreateFileJSON(fn);
    if not FileExists(fn) then Exit;
  end;

  DoFileOpen(fn, '');
end;

procedure TfmMain.DoOps_OpenFile_DefaultAndUser;
var
  NameDef, NameUser: string;
  F: TEditorFrame;
begin
  NameDef:= AppFile_OptionsDefault;
  NameUser:= AppFile_OptionsUser;

  if not FileExists(NameUser) then
  begin
    AppCreateFileJSON(NameUser);
    if not FileExists(NameUser) then exit;
  end;

  F:= DoFileOpen(NameDef, NameUser);
  if Assigned(F) then
  begin
    F.ReadOnly[F.Ed1]:= true;
    F.ReadOnly[F.Ed2]:= false;
  end
  else
    MsgStatus(msgCannotOpenFile+' default.json/user.json');
end;

procedure TfmMain.DoOps_OpenFile_LexerSpecific;
var
  F: TEditorFrame;
  CurLexer, fn, fn_def: string;
begin
  F:= CurrentFrame;
  if F=nil then exit;

  CurLexer:= F.LexerName[F.Editor];

  fn:= AppFile_LexerSpecificConfig(CurLexer, false);
  fn_def:= AppFile_LexerSpecificConfig(CurLexer, true);

  if not FileExists(fn) then
  begin
    AppCreateFileJSON(fn);
    if not FileExists(fn) then exit;
  end;

  if FileExists(fn_def) then
    DoFileOpen(fn_def, fn)
  else
    DoFileOpen(fn, '');
end;

procedure TfmMain.MenuMainClick(Sender: TObject);
var
  F: TEditorFrame;
  bFindFocused, bPanelFocused: boolean;
  NTag: PtrInt;
  NCommand: integer;
  SCaption, SCallback: string;
  mi: TMenuItem;
begin
  NTag:= (Sender as TComponent).Tag;
  if NTag=0 then exit;

  SCaption:= '';
  if Sender is TMenuItem then
  begin
    mi:= Sender as TMenuItem;
    SCaption:= mi.Caption;
    repeat
      mi:= mi.Parent;
      if mi=nil then Break;
      if mi.Caption='' then Break;
      SCaption:= mi.Caption+'>'+SCaption;
    until false;
    SCaption:= 'm='+SCaption+';';
  end;

  NCommand:= TAppMenuProps(NTag).CommandCode;
  SCallback:= TAppMenuProps(NTag).CommandString;

  F:= CurrentFrame;

  //note: F can be Nil here in some cases
  //(e.g. loaded session with bad focused tab in group-2, and group-2 is empty)
  if F=nil then
    F:= Frames[0];

  if IsCommandForClipboardAction(NCommand) then
  //if (NCommand>0) and (NCommand<cmdFirstAppCommand) then
  begin
    //dont do editor commands here if ed not focused
    bFindFocused:= Assigned(fmFind) and
      (fmFind.edFind.Focused or
      fmFind.edRep.Focused);
    bPanelFocused:=
      CodeTree.Tree.Focused or
      CodeTreeFilterInput.Focused or
      (Assigned(fmConsole) and (fmConsole.EdInput.Focused or fmConsole.EdMemo.Focused));

    if bFindFocused or bPanelFocused then
    begin
      MsgStatus(msgIgnoredCommandIfNotFocused);
      exit;
    end;
  end;

  //-1 means run callback
  if NCommand=-1 then
  begin
    if SCallback<>'' then
    begin
      F.Editor.CommandLog.Add(cmd_PluginRun, TATCommandInvoke.MenuAPI, SCallback+SCaption);
      DoPyCallbackFromAPI(SCallback, [], []);
      if not PyEditorMaybeDeleted then
        F.Editor.CommandLog.Add(cmd_PluginEnd, TATCommandInvoke.MenuAPI, SCallback+SCaption);
    end;
  end
  else
    F.Editor.DoCommand(NCommand, TATCommandInvoke.MenuMain);

  if (NCommand<>-1)
    and (NCommand<>cmd_FileClose)
    and (NCommand<>cmd_FileCloseAndDelete)
    and (NCommand<>cmd_FileCloseAll) then
    UpdateFrameEx(F, false);

  UpdateStatusbar;
end;

procedure TfmMain.SetFrameLexerByIndex(Ed: TATSynEdit; AIndex: integer);
var
  F: TEditorFrame;
  CountUsual, CountLite: integer;
begin
  F:= TGroupsHelper.GetEditorFrame(Ed);
  if F=nil then exit;

  CountUsual:= AppManager.LexerCount;
  CountLite:= AppManagerLite.LexerCount;

  if (AIndex>=0) and (AIndex<CountUsual+CountLite) then
  begin
    if AIndex<CountUsual then
      F.Lexer[Ed]:= AppManager.Lexers[AIndex]
    else
      F.LexerLite[Ed]:= AppManagerLite.Lexers[AIndex-CountUsual];
  end
  else
  begin
    F.Lexer[Ed]:= nil;
    F.LexerLite[Ed]:= nil;
  end;

  UpdateFrameEx(F, false);
  UpdateStatusbar;
end;


function TfmMain.DoAutoComplete_FromPlugins(Ed: TATSynEdit): boolean;
begin
  Result:= DoPyEvent(Ed, TAppPyEvent.OnComplete, []).Val = TAppPyEventValue.True;
end;

function TfmMain.DoAutoComplete_PosOnBadToken(Ed: TATSynEdit; AX, AY: integer): boolean;
var
  TokenKind: TATTokenKind;
  bLexerHTML: boolean;
begin
  Result:= false;

  if not UiOps.AutocompleteInComments or
    not UiOps.AutocompleteInCommentsHTML or
    not UiOps.AutocompleteInStrings then
  begin
    TokenKind:= EditorGetTokenKind(Ed, AX, AY, false{ADocCommentIsAlsoComment});
    case TokenKind of
      TATTokenKind.Comment:
        begin
          bLexerHTML:= false;
          if Assigned(Ed.AdapterForHilite) then
            bLexerHTML:= SubInString('HTML', Ed.AdapterForHilite.GetLexerName);

          if bLexerHTML then
            Result:= not UiOps.AutocompleteInCommentsHTML
          else
            Result:= not UiOps.AutocompleteInComments;
        end;
      TATTokenKind.Str:
        begin
          Result:= not UiOps.AutocompleteInStrings;
        end;
    end;
  end;
end;


procedure TfmMain.DoAutoComplete_Callback(Ed: TATSynEdit; AActivate: boolean);
{
AActivate param is for future maybe, for _delayed_ autocomplete.
AActivate=False means: cancel next planned _delayed_ autocomplete.
if we will need _delayed_ acp, we will
a) rewrite this callback to just set/clear the flag,
b) add reaction to flag in TimerAppIdle handler.
}
var
  Frame: TEditorFrame;
begin
  if AActivate then
  begin
    AppAutocompleteInvoke:= 'a';
    Frame:= TGroupsHelper.GetEditorFrame(Ed);
    if Assigned(Frame) then
      Frame.TextCharsTyped:= 0;
    DoAutoComplete(Ed);
  end;
end;

procedure TfmMain.DoAutoComplete(Ed: TATSynEdit);
var
  Frame: TEditorFrame;
  SLexer: string;
  Caret: TATCaretItem;
  bNeedCss, bNeedHtml, bNeedAcp: boolean;
  bWithLexer: boolean;
begin
  Frame:= TGroupsHelper.GetEditorFrame(Ed);
  if Frame=nil then exit;

  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  //disable completion in comments/strings
  if DoAutoComplete_PosOnBadToken(Ed, Caret.PosX, Caret.PosY) then exit;

  CompletionOps.AppendOpeningBracket:= Ed.OptAutocompleteAddOpeningBracket;
  CompletionOps.UpDownAtEdge:= TATCompletionUpDownAtEdge(Ed.OptAutocompleteUpDownAtEdge);
  CompletionOps.CommitChars:= Ed.OptAutocompleteCommitChars; //before DoPyEvent
  CompletionOps.CommitOnEnter:= Ed.OptAutocompleteCommitOnEnter;
  CompletionOps.CloseChars:= Ed.OptAutocompleteCloseChars; //before DoPyEvent
  CompletionOps.CommitIfSingleItem:= Ed.OptAutocompleteCommitIfSingleItem; //before DoPyEvent
  CompletionOps.CommandForShitchTab:= cmd_SwitchTab_HotkeyNext;
  CompletionOps.ShortcutForAutocomplete:= Ed.Keymap.GetShortcutFromCommand(cmd_AutoComplete);
  CompletionOps.ClosingTimerInverval:= UiOps.AutocompleteClosingDelay;

  //auto-completion for file:///, before plugins
  if UiOps.AutocompleteFileURI and
    DoEditorCompletionFileURI(Ed) then exit;

  //auto-completion plugins
  if DoAutoComplete_FromPlugins(Ed) then exit;

  bNeedHtml:= false;
  bNeedCss:= false;
  bNeedAcp:= false;
  SLexer:= '';
  bWithLexer:= Frame.Lexer[Ed]<>nil;

  if bWithLexer then
  begin
    SLexer:= EditorLexerNameAtPos(Ed, Point(Caret.PosX, Caret.PosY));
    if SLexer='' then exit;

    bNeedHtml:= UiOps.AutocompleteHtml and SRegexMatchesString(SLexer, UiOps.AutocompleteHtml_Lexers, false);
    bNeedCss:= UiOps.AutocompleteCss and SRegexMatchesString(SLexer, UiOps.AutocompleteCss_Lexers, false);
    bNeedAcp:= UiOps.AutocompleteAcpFiles;

    CompletionOpsCss.FilenameCssList:= AppDir_DataAutocompleteSpec+DirectorySeparator+'css_list.ini';
    CompletionOpsCss.FilenameCssColors:= AppDir_DataAutocompleteSpec+DirectorySeparator+'css_colors.ini';
    CompletionOpsCss.FilenameCssSelectors:= AppDir_DataAutocompleteSpec+DirectorySeparator+'css_sel.ini';
    CompletionOpsHtml.FilenameHtmlList:= AppDir_DataAutocompleteSpec+DirectorySeparator+'html_list.ini';
    CompletionOpsHtml.FilenameHtmlGlobals:= AppDir_DataAutocompleteSpec+DirectorySeparator+'html_globals.ini';
    CompletionOpsHtml.FilenameHtmlEntities:= AppDir_DataAutocompleteSpec+DirectorySeparator+'html_entities.ini';
    CompletionOpsHtml.FilenameHtmlMediaTypes:= AppDir_DataAutocompleteSpec+DirectorySeparator+'html_mediatypes.ini';

    //allow autocompletion with multi-carets only in HTML
    if Ed.Carets.Count>1 then
      if not bNeedHtml then
      begin
        MsgStatus(msgCannotAutocompleteMultiCarets);
        exit;
      end;
    MsgStatus(msgStatusTryingAutocomplete+' '+SLexer);
  end;

  //completion for HTML, CSS, .acp - is available only with lexer
  if bNeedHtml then
    DoEditorCompletionHtml(Ed)
  else
  if bNeedCss then
  begin
    if CompletionOpsCss.Provider=nil then
      if AppPython.Inited then
        CompletionOpsCss.Provider:= TATCssPythonProvider.Create;
    DoEditorCompletionCss(Ed);
  end
  else
  if bNeedAcp then
    DoEditorCompletionAcp(Ed, AppFile_LexerAcp(SLexer), false{CaseSens});
end;

procedure TfmMain.mnuTreeFold2Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(CodeTree.Tree, 2);
end;

procedure TfmMain.mnuTreeFold3Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(CodeTree.Tree, 3);
end;

procedure TfmMain.mnuTreeFold4Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(CodeTree.Tree, 4);
end;

procedure TfmMain.mnuTreeFold5Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(CodeTree.Tree, 5);
end;

procedure TfmMain.mnuTreeFold6Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(CodeTree.Tree, 6);
end;

procedure TfmMain.mnuTreeFold7Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(CodeTree.Tree, 7);
end;

procedure TfmMain.mnuTreeFold8Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(CodeTree.Tree, 8);
end;

procedure TfmMain.mnuTreeFold9Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(CodeTree.Tree, 9);
end;

procedure TfmMain.mnuTreeFoldAllClick(Sender: TObject);
begin
  CodeTree.Tree.FullCollapse;
end;

procedure TfmMain.mnuTreeSortedClick(Sender: TObject);
var
  Frame: TEditorFrame;
begin
  if CodeTree.Tree.SortType=stNone then
    CodeTree.Tree.SortType:= stText
  else
    CodeTree.Tree.SortType:= stNone;

  Frame:= TGroupsHelper.GetEditorFrame(AppCodetreeState.Editor);
  if Assigned(Frame) then
    Frame.CodetreeSortType:= CodeTree.Tree.SortType;
end;

procedure TfmMain.mnuTreeUnfoldAllClick(Sender: TObject);
begin
  CodeTree.Tree.FullExpand;
end;

procedure TfmMain.DoFileExportHtml(Ed: TATSynEdit);
var
  Dlg: TSaveDialog;
  List: TStringList;
  Frame: TEditorFrame;
  SFileName, STitle: string;
  SaveCarets: TATCarets;
begin
  if EditorIsEmpty(Ed) then exit;

  STitle:= ExtractFileName(Ed.FileName);
  if STitle='' then
  begin
    Frame:= TGroupsHelper.GetEditorFrame(Ed);
    if Assigned(Frame) then
      STitle:= Frame.TabCaption
    else
      STitle:= msgUntitledEnglish;
  end;

  Dlg:= TSaveDialog.Create(Self);
  try
    Dlg.Title:= msgDialogTitleSaveAs;
    Dlg.Filename:= STitle+'.html';
    Dlg.InitialDir:= GetTempDir(false);
    Dlg.Options:= [ofPathMustExist, ofEnableSizing, ofDontAddToRecent];
    Dlg.Filter:= 'HTML files|*.htm;*.html';
    if not Dlg.Execute then exit;
    SFileName:= Dlg.FileName;
  finally
    FreeAndNil(Dlg);
  end;

  //hide caret, so HTML won't contain dynamic lexer highlights
  SaveCarets:= TATCarets.Create;
  SaveCarets.Assign(Ed.Carets);

  Ed.DoCaretSingle(-1, -1);
  Ed.DoEventCarets;
  Ed.Update;

  //Application.ProcessMessages; //crashes, dont do it

  List:= TStringList.Create;
  try
    EditorExportToHTML(Ed,
      List,
      Point(0, 0),
      Point(0, Ed.Strings.Count),
      STitle,
      UiOps.ExportHtmlFontName,
      UiOps.ExportHtmlFontSize,
      UiOps.ExportHtmlNumbers,
      GetAppColor(TAppThemeColor.ExportHtmlBg),
      GetAppColor(TAppThemeColor.ExportHtmlNumbers)
      );
    List.SaveToFile(SFileName);
  finally
    FreeAndNil(List);
  end;

  //restore carets
  Ed.Carets.Assign(SaveCarets);
  FreeAndNil(SaveCarets);
  Ed.DoEventCarets;
  Ed.Update;
  //UpdateFrameEx(F, true);

  if not FileExists(SFileName) then
    MsgBox(msgCannotSaveFile+#10+SFileName, MB_OK or MB_ICONERROR)
  else
  if MsgBox(msgConfirmOpenCreatedDoc, MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
    OpenDocument(SFileName);
end;


function TfmMain.DoDialogMenuApi(const AProps: TDlgMenuProps): integer;
var
  Form: TfmMenuApi;
  Sep: TATStringSeparator;
  SItem: string;
  DeskRect: TRect;
begin
  Form:= TfmMenuApi.Create(nil);
  try
    Sep.Init(AProps.ItemsText, #10);
    repeat
      if not Sep.GetItemStr(SItem) then Break;
      Form.listItems.Add(SItem);
    until false;

    UpdateInputForm(Form);
    if AProps.ShowCentered then
      Form.Position:= poScreenCenter;

    Form.ListCaption:= AProps.Caption;
    Form.Multiline:= AProps.Multiline;
    Form.InitItemIndex:= AProps.InitialIndex;
    Form.DisableFuzzy:= AProps.NoFuzzy;
    Form.DisableFullFilter:= AProps.NoFullFilter;
    Form.CollapseMode:= AProps.Collapse;
    Form.UseEditorFont:= AProps.UseEditorFont;

    DeskRect:= Screen.WorkAreaRect;

    if AProps.W>0 then
      Form.Width:= Min(AProps.W, DeskRect.Width);
    if AProps.H>0 then
      Form.Height:= Min(AProps.H, DeskRect.Height);

    Form.ShowModal;
    Result:= Form.ResultCode;
  finally
    Form.Free;
  end;
end;

procedure TfmMain.DoDialogMenuTranslations;
const
  cEnLang = 'en (built-in)';
var
  ListFiles, ListNames: TStringList;
  NResult, NItemIndex, i: integer;
  S: string;
begin
  ListFiles:= TStringList.Create;
  ListNames:= TStringList.Create;
  try
    FindAllFiles(ListFiles, AppDir_DataLang, '*.ini', false);
    if ListFiles.Count=0 then exit;
    ListFiles.Sort;

    ListNames.Add(cEnLang);
    for i:= 0 to ListFiles.Count-1 do
    begin
      S:= ExtractFileNameOnly(ListFiles[i]);
      if S='translation template' then Continue;
      ListNames.Add(S);
    end;

    NItemIndex:= ListNames.IndexOf(UiOps.LangName);
    if NItemIndex<0 then
      NItemIndex:= 0;

    NResult:= DoDialogMenuList(msgMenuTranslations, ListNames, NItemIndex);
    if NResult<0 then exit;

    if ListNames[NResult]=cEnLang then
    begin
      UiOps.LangName:= '';
      MsgBox(msgStatusI18nEnglishAfterRestart, MB_OK or MB_ICONINFORMATION);
    end
    else
    begin
      UiOps.LangName:= ListNames[NResult];
      DoLocalize;

      if DirectoryExists(AppDir_Data+DirectorySeparator+'langmenu') then
        MsgBox(msgStatusI18nPluginsMenuAfterRestart, MB_OK or MB_ICONINFORMATION);
    end;

    DoPyEvent_AppState(APPSTATE_LANG);
  finally
    FreeAndNil(ListNames);
    FreeAndNil(ListFiles);
  end;
end;

procedure TfmMain.DoDialogMenuEnds;
var
  List: TStringList;
  NRes, NSelected: integer;
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  if Ed=nil then exit;

  List:= TStringList.Create;
  try
    List.Add(msgEndUnix);
    List.Add(msgEndWin);
    List.Add(msgEndMac);

    case Ed.Strings.Endings of
      TATLineEnds.Unix: NSelected:= 0;
      TATLineEnds.Windows: NSelected:= 1;
      TATLineEnds.Mac: NSelected:= 2;
      else NSelected:= 0;
    end;

    NRes:= DoDialogMenuList(msgStatusbarHintEnds, List, NSelected);
    if NRes<0 then exit;

    case NRes of
      0: Ed.DoCommand(cmd_LineEndUnix, TATCommandInvoke.AppPalette);
      1: Ed.DoCommand(cmd_LineEndWin, TATCommandInvoke.AppPalette);
      2: Ed.DoCommand(cmd_LineEndMac, TATCommandInvoke.AppPalette);
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmMain.DoDialogMenuEncodings;
var
  List: TStringList;
  NRes, NSelected, i: integer;
  bReloadFile: boolean;
  Ed: TATSynEdit;
  SEncName: string;
  DlgProps: TDlgMenuProps;
begin
  Ed:= CurrentEditor;
  if Ed=nil then exit;
  SEncName:= Ed.EncodingName;

  List:= TStringList.Create;
  try
    List.Clear;
    List.Add(msgEncReloadAs);
    List.Add(msgEncConvertTo);

    NRes:= DoDialogMenuList(msgStatusbarHintEnc, List, 0);
    if NRes<0 then exit;
    bReloadFile:= NRes=0;

    List.Clear;
    NSelected:= 0;
    for i:= Low(AppEncodings) to High(AppEncodings) do
    begin
      List.Add(AppEncodings[i].Name);
      if SameText(AppEncodings[i].Name, SEncName) then
        NSelected:= i;
    end;

    DlgProps:= Default(TDlgMenuProps);
    DlgProps.Caption:= msgStatusbarHintEnc;
    DlgProps.Collapse:= acsmRight;
    DlgProps.InitialIndex:= NSelected;
    DlgProps.ItemsText:= List.Text;

    NRes:= DoDialogMenuApi(DlgProps);
    if NRes<0 then exit;

    SetFrameEncoding(Ed, AppEncodings[NRes].Name, bReloadFile);
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmMain.SplitterOnPaintDummy(Sender: TObject);
begin
  //empty, to disable themed paint
end;


procedure TfmMain.GetEditorIndexes(Ed: TATSynEdit; out AGroupIndex, ATabIndex: Integer);
var
  Gr: TATGroups;
  Pages: TATPages;
  Frame: TEditorFrame;
  NLocalGroup: integer;
begin
  Frame:= TGroupsHelper.GetEditorFrame(Ed);
  if Assigned(Frame) and Assigned(Frame.Parent) then
    GetFrameLocation(Frame, Gr, Pages, NLocalGroup, AGroupIndex, ATabIndex)
  else
  begin
    AGroupIndex:= -1;
    ATabIndex:= -1;
  end;
end;

procedure TfmMain.DoHelpWiki;
begin
  OpenURL('https://wiki.freepascal.org/CudaText');
end;

procedure TfmMain.DoCodetree_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Frame: TEditorFrame;
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
      Frame.SetFocus;
    Key:= 0;
    exit
  end;

  if (Key=VK_RETURN) then
  begin
    (Sender as TTreeView).OnDblClick(Sender);
    Key:= 0;
    exit
  end;

  //handle Tab-key, because LCL by default can jump to bottom-panel form
  if (Key=VK_TAB) and (Shift=[]) then
  begin
    if CodeTreeFilterInput.CanFocus then
      CodeTreeFilterInput.SetFocus;
    Key:= 0;
    exit;
  end;
end;

procedure TfmMain.DoCodetree_OnContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  InitPopupTree;
  PopupTree.Popup;
  Handled:= true;
end;


procedure TfmMain.DoCodetree_GotoBlockForCurrentNode(AndSelect: boolean);
var
  Ed: TATSynEdit;
  Node: TTreeNode;
  P1, P2: TPoint;
begin
  Node:= CodeTree.Tree.Selected;
  if Node=nil then exit;

  DoCodetree_GetSyntaxRange(Node, P1, P2);
  if (P1.Y<0) or (P2.Y<0) then exit;

  if not AndSelect then
    P2:= Point(-1, -1);

  Ed:= CurrentEditor;
  Ed.DoGotoPos(P1, P2,
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    TATEditorActionIfFolded.Unfold
    );
end;

procedure TfmMain.DoCodetree_OnAdvDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  NColor: TColor;
  R: TRect;
  C: TCanvas;
begin
  DefaultDraw:= true;

  //feature for CSS lexer: convert Node.Text to TColor and render the colored rect
  if (AppCodetreeState.Lexer='CSS') and (Stage=cdPostPaint) then
  begin
    DefaultDraw:= false;
    NColor:= ConvertCssColorToTColor(Node.Text);
    if NColor<>clNone then
    begin
      R:= Node.DisplayRect(true);
      Inc(R.Top);
      Dec(R.Bottom);
      R.Right:= R.Left-2;
      R.Left:= R.Right-R.Height;

      C:= (Sender as TTreeView).Canvas;
      C.Pen.Color:= clBlack;
      C.Brush.Color:= NColor;
      C.Rectangle(R);
    end;
  end;
end;

procedure TfmMain.PopupBottomOnPopup(Sender: TObject);
var
  Popup: TPopupMenu;
  mi: TMenuItem;
  i: integer;
begin
  Popup:= Sender as TPopupMenu;
  for i:= 0 to Popup.Items.Count-1 do
  begin
    mi:= Popup.Items[i];
    case mi.Tag of
      100:
        mi.Caption:= ATEditorOptions.TextMenuitemCopy;
      101:
        mi.Caption:= ATEditorOptions.TextMenuitemSelectAll;
      102:
        mi.Caption:= msgConsoleClear;
      103:
        begin
          mi.Caption:= msgConsoleToggleWrap;
          mi.Checked:= (mi.Owner as TATSynEdit).OptWrapMode<>TATEditorWrapMode.ModeOff;
        end;
    end;
  end;
end;

procedure TfmMain.PopupToolbarCaseOnPopup(Sender: TObject);
begin
  if mnuToolbarCaseLow=nil then
  begin
    mnuToolbarCaseLow:= TMenuItem.Create(Self);
    mnuToolbarCaseLow.Tag:= cCommand_TextCaseLower;
    mnuToolbarCaseLow.OnClick:= @MenuitemClick_CommandFromTag;

    mnuToolbarCaseUp:= TMenuItem.Create(Self);
    mnuToolbarCaseUp.Tag:= cCommand_TextCaseUpper;
    mnuToolbarCaseUp.OnClick:= @MenuitemClick_CommandFromTag;

    mnuToolbarCaseTitle:= TMenuItem.Create(Self);
    mnuToolbarCaseTitle.Tag:= cCommand_TextCaseTitle;
    mnuToolbarCaseTitle.OnClick:= @MenuitemClick_CommandFromTag;

    mnuToolbarCaseInvert:= TMenuItem.Create(Self);
    mnuToolbarCaseInvert.Tag:= cCommand_TextCaseInvert;
    mnuToolbarCaseInvert.OnClick:= @MenuitemClick_CommandFromTag;

    mnuToolbarCaseSent:= TMenuItem.Create(Self);
    mnuToolbarCaseSent.Tag:= cCommand_TextCaseSentence;
    mnuToolbarCaseSent.OnClick:= @MenuitemClick_CommandFromTag;

    PopupToolbarCase.Items.Add(mnuToolbarCaseUp);
    PopupToolbarCase.Items.Add(mnuToolbarCaseLow);
    PopupToolbarCase.Items.Add(mnuToolbarCaseTitle);
    PopupToolbarCase.Items.Add(mnuToolbarCaseInvert);
    PopupToolbarCase.Items.Add(mnuToolbarCaseSent);
  end;

  mnuToolbarCaseLow.Caption:= msgTextCaseLower;
  mnuToolbarCaseUp.Caption:= msgTextCaseUpper;
  mnuToolbarCaseTitle.Caption:= msgTextCaseTitle;
  mnuToolbarCaseInvert.Caption:= msgTextCaseInvert;
  mnuToolbarCaseSent.Caption:= msgTextCaseSentence;
end;

procedure TfmMain.PopupToolbarCommentOnPopup(Sender: TObject);
begin
  if not AppPython.Inited then exit;

  if mnuToolbarCommentLineAdd=nil then
  begin
    mnuToolbarCommentLineAdd:= TMenuItem.Create(Self);
    mnuToolbarCommentLineAdd.Hint:= 'cuda_comments,cmt_add_line_body';
    mnuToolbarCommentLineAdd.OnClick:= @MenuitemClick_CommandFromHint;

    mnuToolbarCommentLineDel:= TMenuItem.Create(Self);
    mnuToolbarCommentLineDel.Hint:= 'cuda_comments,cmt_del_line';
    mnuToolbarCommentLineDel.OnClick:= @MenuitemClick_CommandFromHint;

    mnuToolbarCommentLineToggle:= TMenuItem.Create(Self);
    mnuToolbarCommentLineToggle.Hint:= 'cuda_comments,cmt_toggle_line_body';
    mnuToolbarCommentLineToggle.OnClick:= @MenuitemClick_CommandFromHint;

    mnuToolbarCommentStream:= TMenuItem.Create(Self);
    mnuToolbarCommentStream.Hint:= 'cuda_comments,cmt_toggle_stream';
    mnuToolbarCommentStream.OnClick:= @MenuitemClick_CommandFromHint;

    PopupToolbarComment.Items.Add(mnuToolbarCommentLineToggle);
    PopupToolbarComment.Items.Add(mnuToolbarCommentLineAdd);
    PopupToolbarComment.Items.Add(mnuToolbarCommentLineDel);
    PopupToolbarComment.Items.Add(mnuToolbarCommentStream);
  end;

  mnuToolbarCommentLineAdd.Caption:= msgCommentLineAdd;
  mnuToolbarCommentLineDel.Caption:= msgCommentLineDel;
  mnuToolbarCommentLineToggle.Caption:= msgCommentLineToggle;
  mnuToolbarCommentStream.Caption:= msgCommentStreamToggle;
end;

procedure TfmMain.EditorOutput_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Frame: TEditorFrame;
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
      Frame.SetFocus;
    Key:= 0;
    exit
  end;
end;

procedure TfmMain.MenuPicScaleClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F.FrameKind=TAppFrameKind.ImageViewer then
  begin
    F.PictureScale:= (Sender as TComponent).Tag;
  end;
end;


procedure TfmMain.DoHelpIssues;
begin
  OpenURL('https://github.com/Alexey-T/CudaText/issues');
end;


procedure TfmMain.mnuTabColorClick(Sender: TObject);
var
  F: TEditorFrame;
  NColor: TColor;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  NColor:= PyHelper_DialogColorPicker(F.TabColor);
  if NColor>=0 then
    F.TabColor:= NColor;
end;

procedure TfmMain.mnuTabPinnedClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;
  F.TabPinned:= not F.TabPinned;
end;


procedure TfmMain.mnuTabCopyDirClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  F.Editor.DoCommand(cmd_CopyFilenameDir, TATCommandInvoke.MenuContext);
end;

procedure TfmMain.mnuTabCopyFullPathClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  F.Editor.DoCommand(cmd_CopyFilenameFull, TATCommandInvoke.MenuContext);
end;

procedure TfmMain.mnuTabCopyNameClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  F.Editor.DoCommand(cmd_CopyFilenameName, TATCommandInvoke.MenuContext);
end;

procedure DoParseOutputLine(const AForm: TAppFormWithEditor;
  const AStr: string;
  out AFilename: string;
  out ALine, ACol: integer;
  out AMsg: string);
var
  Parts: TRegexParts;
begin
  AFilename:= AForm.DefFilename;
  ALine:= -1;
  ACol:= 0;
  AMsg:= '';

  if AForm.RegexStr='' then
    exit;

  if AFilename='' then
    if AForm.RegexIdName=0 then
    begin
      AMsg:= 'Log double-click: "filename" index in RegEx is not set';
      exit;
    end;

  if AForm.RegexIdLine=0 then
  begin
    AMsg:= 'Log double-click: "line" index in RegEx is not set';
    exit;
  end;

  if not SRegexFindParts(AForm.RegexStr, AStr, Parts) then
  begin
    AMsg:= 'Log double-click: cannot find parts by RegEx';
    exit;
  end;

  if AForm.RegexIdName>0 then
    AFilename:= Parts[AForm.RegexIdName].Str;

  if AForm.RegexIdLine>0 then
    ALine:= StrToIntDef(Parts[AForm.RegexIdLine].Str, -1);

  if AForm.RegexIdCol>0 then
    ACol:= StrToIntDef(Parts[AForm.RegexIdCol].Str, 0);

  if not AForm.ZeroBase then
  begin
    if ALine>0 then Dec(ALine);
    if ACol>0 then Dec(ACol);
  end;

  if AFilename='' then
    AMsg:= 'Log double-click: cannot find filename by RegEx'
  else
  if ALine<0 then
    AMsg:= 'Log double-click: cannot find line-index by RegEx';
end;

procedure TfmMain.EditorOutput_OnClickDbl(Sender: TObject; var AHandled: boolean);
var
  Form: TAppFormWithEditor;
  ResFilename: string;
  ResLine, ResCol: integer;
  Frame: TEditorFrame;
  CaretY: integer;
  bFound: boolean;
  SText, SMsg: string;
begin
  AHandled:= true; //avoid selection of word

  CloseFormAutoCompletion;

  Form:= FindBottomForm_ByEditor(Sender as TATSynEdit);
  if Form=nil then exit;

  CaretY:= Form.Ed.Carets[0].PosY;
  if not Form.Ed.Strings.IsIndexValid(CaretY) then exit;

  SText:= Form.Ed.Strings.Lines[CaretY];

  DoParseOutputLine(Form, SText, ResFilename, ResLine, ResCol, SMsg);
  if (ResFilename<>'') and (ResLine>=0) then
  begin
    MsgStatus(Format(msgStatusGotoFileLineCol, [ResFilename, ResLine+1, ResCol+1]));
    bFound:= false;

    //first check the active file-tab
    Frame:= CurrentFrame;
    if ExtractFileDir(ResFilename)='' then
      if SameFileName(ResFilename, ExtractFileName(Frame.FileName)) then
        bFound:= true;

    //next, find in all file-tabs
    if not bFound then
    begin
      Frame:= FindFrameOfFilename(ResFilename, true); //'true' is important here
      bFound:= Assigned(Frame);
    end;

    if bFound then
    begin
      Frame.SetFocus;
      Frame.Editor.DoGotoPos(
         Point(ResCol, ResLine),
         Point(-1, -1),
         UiOps.FindIndentHorz,
         UiOps.FindIndentVert,
         true{PlaceCaret},
         TATEditorActionIfFolded.Unfold{Unfold}
         );
      UpdateStatusbar;
    end
    else
      SMsg:= Format('Log double-click: no file "%s"', [ResFilename]);

    if SMsg<>'' then
      MsgStatus(SMsg);
  end
  else
  begin
    if SMsg='' then
      SMsg:= msgStatusClickingLogLine;
    if SMsg<>'' then
      MsgStatus(SMsg);
    DoPyEvent(nil, TAppPyEvent.OnOutputNav, [AppVariant(SText), AppVariant(0)]);
  end;
end;


procedure TfmMain.DoGotoDefinition(Ed: TATSynEdit);
begin
  if DoPyEvent(Ed, TAppPyEvent.OnGotoDef, []).Val <> TAppPyEventValue.True then
    MsgStatus(msgStatusNoGotoDefinitionPlugins);
end;

procedure TfmMain.DoShowFuncHint(Ed: TATSynEdit);
var
  S: string;
begin
  S:= DoPyEvent(Ed, TAppPyEvent.OnFuncHint, []).Str;
  S:= Trim(S);
  if S<>'' then
    DoTooltipShow(S, UiOps.AltTooltipTime, TAppTooltipPos.EditorCaret, true, -1, -1);
end;

procedure TfmMain.DoTooltipHide;
begin
  TimerTooltip.Enabled:= false;
  if Assigned(FFormTooltip) then
    FFormTooltip.Hide;
  FLastTooltipLine:= -1;
end;

procedure TfmMain.PopupTextPopup(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  UpdateMenuItemHotkey(mnuTextUndo, cCommand_Undo);
  UpdateMenuItemHotkey(mnuTextRedo, cCommand_Redo);
  UpdateMenuItemHotkey(mnuTextCut, cCommand_ClipboardCut);
  UpdateMenuItemHotkey(mnuTextCopy, cCommand_ClipboardCopy);
  UpdateMenuItemHotkey(mnuTextPaste, cCommand_ClipboardPaste);
  UpdateMenuItemHotkey(mnuTextDelete, cCommand_TextDeleteSelection);
  UpdateMenuItemHotkey(mnuTextSel, cCommand_SelectAll);
  UpdateMenuItemHotkey(mnuTextGotoDef, cmd_GotoDefinition);
  UpdateMenuItemHotkey(mnuTextOpenUrl, cmd_LinkAtCaret_Open);

  Ed:= CurrentEditor;

  if Assigned(mnuTextCut) then
    mnuTextCut.Enabled:= not Ed.ModeReadOnly;

  if Assigned(mnuTextPaste) then
    mnuTextPaste.Enabled:= not Ed.ModeReadOnly and Clipboard.HasFormat(CF_Text);

  if Assigned(mnuTextDelete) then
    mnuTextDelete.Enabled:= not Ed.ModeReadOnly and Ed.Carets.IsSelection;

  if Assigned(mnuTextUndo) then
    mnuTextUndo.Enabled:= not Ed.ModeReadOnly and (Ed.UndoCount>0);

  if Assigned(mnuTextRedo) then
    mnuTextRedo.Enabled:= not Ed.ModeReadOnly and (Ed.RedoCount>0);

  if Assigned(mnuTextOpenUrl) then
    mnuTextOpenUrl.Enabled:= EditorGetLinkAtCaret(Ed)<>'';
end;


procedure TfmMain.CharmapOnInsert(const AStr: string);
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  if Ed.Carets.Count=0 then exit;
  Ed.DoCommand(cCommand_TextInsert, TATCommandInvoke.AppCharMap, Utf8Decode(AStr));

  UpdateCurrentFrame(true);
  UpdateStatusbar;
end;


procedure TfmMain.DoDialogCharMap;
begin
  if fmCharmaps=nil then
  begin
    fmCharmaps:= TfmCharmaps.Create(Self);
    fmCharmaps.OnInsert:= @CharmapOnInsert;
    fmCharmaps.Localize;
  end;

  fmCharmaps.InitialStr:= Utf8Encode(Widestring(EditorGetCurrentChar(CurrentEditor)));
  fmCharmaps.Show;
end;

function TfmMain.DoPyEvent_ConsoleNav(const AText: string): boolean;
begin
  Result:= DoPyEvent(nil, TAppPyEvent.OnConsoleNav, [AppVariant(AText)]).Val <> TAppPyEventValue.False;
end;

procedure TfmMain.DoPyEvent_ConsoleComplete(Sender: TObject);
begin
  DoPyEvent(nil, TAppPyEvent.OnConsoleComplete, []);
end;

function TfmMain.DoPyEvent_Message(const AText: string): boolean;
begin
  Result:= DoPyEvent(nil, TAppPyEvent.OnMessage,
    [AppVariant(0), AppVariant(AText)]).Val <> TAppPyEventValue.False;
end;


procedure TfmMain.DoOnConsoleNumberChange(Sender: TObject);
begin
  UpdateSidebarButtonOverlay;
end;


function TfmMain.DoSplitter_StringToId(const AStr: string): integer;
begin
  Result:= -1;
  if AStr='L' then exit(SPLITTER_SIDE);
  if AStr='B' then exit(SPLITTER_BOTTOM);
  if AStr='G1' then exit(SPLITTER_G1);
  if AStr='G2' then exit(SPLITTER_G2);
  if AStr='G3' then exit(SPLITTER_G3);
end;

procedure TfmMain.DoSplitter_GetInfo(const Id: integer;
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

  case Id of
    SPLITTER_SIDE: GetSp(AppPanels[TAppPanelId.Side].Splitter);
    SPLITTER_BOTTOM: GetSp(AppPanels[TAppPanelId.Btm].Splitter);
    SPLITTER_G1: GetSp(Groups.Splitter1);
    SPLITTER_G2: GetSp(Groups.Splitter2);
    SPLITTER_G3: GetSp(Groups.Splitter3);
    SPLITTER_G4: GetSp(Groups.Splitter4);
    SPLITTER_G5: GetSp(Groups.Splitter5);
  end;
end;


procedure TfmMain.DoSplitter_SetInfo(const Id: integer; NPos: integer);
  //
  procedure SetSp(Sp: TSplitter);
  begin
    Sp.SetSplitterPosition(NPos);
    if Assigned(Sp.OnMoved) then
      Sp.OnMoved(Self);
  end;
  //
begin
  if NPos<0 then exit;
  case Id of
    SPLITTER_SIDE: SetSp(AppPanels[TAppPanelId.Side].Splitter);
    SPLITTER_BOTTOM: SetSp(AppPanels[TAppPanelId.Btm].Splitter);
    SPLITTER_G1: SetSp(Groups.Splitter1);
    SPLITTER_G2: SetSp(Groups.Splitter2);
    SPLITTER_G3: SetSp(Groups.Splitter3);
    SPLITTER_G4: SetSp(Groups.Splitter4);
    SPLITTER_G5: SetSp(Groups.Splitter5);
  end;
end;

procedure TfmMain.FrameLexerChange(Sender: TATSynEdit);
var
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  Keymap: TATKeymap;
  {$ifdef debug_on_lexer}
  SFileName: string;
  {$endif}
  SLexerName: string;
  //bDisFree: boolean;
begin
  Ed:= Sender;
  Frame:= TGroupsHelper.GetEditorFrame(Ed);
  if Frame=nil then exit;

  //bDisFree:= ShowDistractionFree;

  {$ifdef debug_on_lexer}
  SFileName:= Frame.GetFileName(Ed);
  {$endif}
  SLexerName:= Frame.LexerName[Ed];

  {$ifdef debug_on_lexer}
  MsgLogConsole('OnLexerChange: file "'+ExtractFileName(SFileName)+'" -> "'+SLexerName+'"');
  {$endif}

  //load lexer-specific config
  DoOps_LoadOptionsLexerSpecific(Frame, Ed);

  //API event on_lexer
  //better avoid it for empty editor
  if not AppSessionIsLoading then
    if (SLexerName<>'') or not EditorIsEmpty(Ed) then
    begin
      {$ifdef debug_on_lexer}
      MsgLogConsole('on_lexer: file "'+ExtractFileName(SFileName)+'" -> "'+SLexerName+'"');
      {$endif}

      DoPyEvent(Ed, TAppPyEvent.OnLexer, []);
    end;

  //apply lexer-specific keymap
  Keymap:= TKeymapHelper.GetForLexer(SLexerName);

  if Frame.EditorsLinked then
  begin
    Frame.Ed1.Keymap:= Keymap;
    Frame.Ed2.Keymap:= Keymap;
  end
  else
    Ed.Keymap:= Keymap;

  UpdateMenuPlugins_Shortcuts;

  //if bDisFree then
  //  SetShowDistractionFree_Forced;
end;


procedure TfmMain.DoToolbarClick(Sender: TObject);
var
  SData: string;
  NCmd: integer;
  Frame: TEditorFrame;
begin
  //str(int_command) or callback string
  SData:= (Sender as TATButton).DataString;
  NCmd:= StrToIntDef(SData, 0);

  if NCmd>0 then
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
    begin
      Frame.Editor.DoCommand(NCmd, TATCommandInvoke.AppToolbar);
      UpdateToolbarButtons(Frame);
    end;
  end
  else
  begin
    DoPyCallbackFromAPI(SData, [], []);
  end;

  UpdateCurrentFrame;
  UpdateStatusbar;
end;


function TfmMain.DoMenu_GetPyProps(mi: TMenuItem): PPyObject;
var
  NTag: PtrInt;
  NCommand: integer;
  SCommand, STagString: string;
begin
  NTag:= mi.Tag;
  if NTag>cAppMinMemoryAddress then
  begin
    NCommand:= TAppMenuProps(NTag).CommandCode;
    SCommand:= TAppMenuProps(NTag).CommandString;
    STagString:= TAppMenuProps(NTag).TagString;
  end
  else
  begin
    NCommand:= 0;
    SCommand:= '';
    STagString:= '';
  end;

  with AppPython.Engine do
  begin
    Result:= Py_BuildValue('{sLsssi}',
      'id',
      Int64(PtrInt(mi)),
      'cap',
      PChar(mi.Caption),
      'cmd',
      NCommand
      );

    if SCommand<>'' then
      PyDict_SetItemString(Result, 'hint', PyUnicodeFromString(SCommand));

    if mi.ShortCut<>0 then
      PyDict_SetItemString(Result, 'hotkey', PyUnicodeFromString(ShortCutToTextRaw(mi.ShortCut)));

    if STagString<>'' then
      PyDict_SetItemString(Result, 'tag', PyUnicodeFromString(STagString));

    if mi.Checked then
      PyDict_SetItemString(Result, 'checked', Py_True);

    if mi.RadioItem then
      PyDict_SetItemString(Result, 'radio', Py_True);

    if not mi.Enabled then
      PyDict_SetItemString(Result, 'en', Py_False);

    if not mi.Visible then
      PyDict_SetItemString(Result, 'vis', Py_False);
  end;
end;


function TfmMain.DoMenu_PyEnum(const AMenuId: string): PPyObject;
var
  mi: TMenuItem;
  NLen, i: integer;
begin
  //this updates PopupText items tags
  PopupText.OnPopup(nil);

  with AppPython.Engine do
  begin
    mi:= PyHelper_MenuItemFromId(AMenuId);
    if not Assigned(mi) then
      exit(ReturnNone);

    NLen:= mi.Count;
    Result:= PyList_New(NLen);
    if not Assigned(Result) then
      raise EPythonError.Create(msgPythonListError);

    for i:= 0 to NLen-1 do
      PyList_SetItem(Result, i,
        DoMenu_GetPyProps(mi.Items[i])
        );
  end;
end;


procedure TfmMain.DoMenuClear(const AMenuId: string);
  //
  procedure ClearMenuItem(mi: TMenuItem);
  var
    Obj: TObject;
    i: integer;
  begin
    for i:= mi.Count-1 downto 0 do
      ClearMenuItem(mi.Items[i]);
    if mi.Tag<>0 then
    begin
      //avoid crash in kvichans' plugin, detect not valid tag
      if (mi.Tag>0) and (mi.Tag<20000) then
      begin
        MsgLogConsole('ERROR: bad menu-item tag in menu_proc(..., MENU_CLEAR, ...): '+IntToStr(mi.Tag));
      end
      else
      begin
        Obj:= TObject(mi.Tag);
        Obj.Free;
      end;
      mi.Tag:= 0;
    end;
    mi.Clear;
  end;
  //
var
  mi: TMenuItem;
  i: integer;
begin
  mi:= PyHelper_MenuItemFromId(AMenuId);
  if Assigned(mi) then
  begin
    for i:= mi.Count-1 downto 0 do
      ClearMenuItem(mi.Items[i]);
    mi.Clear;

    case AMenuId of
      PyMenuId_Top:
        begin
          mnuFileOpenSub:= nil;
          mnuFileEnc:= nil;
          mnuPlugins:= nil;
          mnuOpPlugins:= nil;
          mnuLexers:= nil;
        end;
      PyMenuId_TopOptions:
        begin
        end;
      PyMenuId_TopFile:
        begin
          mnuFileOpenSub:= nil;
        end;
      PyMenuId_TopView:
        begin
          mnuLexers:= nil;
        end;
      PyMenuId_Text:
        begin
          mnuTextCopy:= nil;
          mnuTextCut:= nil;
          mnuTextDelete:= nil;
          mnuTextPaste:= nil;
          mnuTextUndo:= nil;
          mnuTextRedo:= nil;
          mnuTextSel:= nil;
          mnuTextGotoDef:= nil;
          mnuTextOpenUrl:= nil;
        end;
      PyMenuId_Tab:
        begin
          mnuTabMove1:= nil;
          mnuTabMove2:= nil;
          mnuTabMove3:= nil;
          mnuTabMove4:= nil;
          mnuTabMove5:= nil;
          mnuTabMove6:= nil;
          mnuTabMoveF1:= nil;
          mnuTabMoveF2:= nil;
          mnuTabMoveF3:= nil;
          mnuTabMoveNext:= nil;
          mnuTabMovePrev:= nil;
        end;
    end;
  end;
end;


function TfmMain.DoMenuAdd_Params(const AMenuId, AMenuCmd, AMenuCaption,
  AMenuHotkey, AMenuTagString: string; AIndex: integer): string;
var
  MenuProps: TAppMenuProps;
  mi, miMain: TMenuItem;
  Num: integer;
begin
  Result:= '';
  miMain:= PyHelper_MenuItemFromId(AMenuId);
  if Assigned(miMain) and (AMenuCaption<>'') then
  begin
    mi:= TMenuItem.Create(Self);
    mi.Caption:= AMenuCaption;
    MenuProps:= TAppMenuProps.Create(Self);
    MenuProps.TagString:= AMenuTagString;
    mi.Tag:= PtrInt(MenuProps);

    Num:= StrToIntDef(AMenuCmd, 0); //command code
    if Num>0 then
    begin
      UpdateMenuItemHotkey(mi, Num, false); //AllowSetShortcut=false to fix ConfigToolbar wrong shortcuts
      UpdateMenuItemAltObject(mi, Num);
    end
    else
    if AMenuCmd='_recents' then
    begin
      mnuFileOpenSub:= mi;
    end
    else
    if AMenuCmd='_plugins' then
    begin
      mnuPlugins:= mi;
      MenuProps.CommandString:= 'plugins';
      UpdateMenuPlugins;
    end
    else
    if AMenuCmd='_oplugins' then
    begin
      mnuOpPlugins:= mi;
      UpdateMenuPlugins;
    end
    else
    if AMenuCaption<>'-' then
    begin
      MenuProps.CommandCode:= -1;
      MenuProps.CommandString:= AMenuCmd;
      if (AMenuCmd<>'0') and (AMenuCmd<>'') then
        mi.OnClick:= @MenuMainClick;
    end;

    if AMenuHotkey<>'' then
      mi.ShortCut:= TextToShortCutRaw(AMenuHotkey);

    if AIndex>=0 then
      miMain.Insert(AIndex, mi)
    else
      miMain.Add(mi);

    if Assigned(mnuFileOpenSub) and Assigned(mnuFileOpenSub.Parent) then
      mnuFileOpenSub.Parent.OnClick:= @MenuRecentsPopup;

    Result:= IntToStr(PtrInt(mi));
  end;
end;

procedure TfmMain.DoMenu_Remove(const AMenuId: string);
var
  mi: TMenuItem;
begin
  mi:= PyHelper_MenuItemFromId(AMenuId);
  mi.Free;
end;

procedure TfmMain.DoFileNewMenu_ToolbarClick(Sender: TObject);
begin
  DoFileNewMenu(Sender, TATCommandInvoke.AppToolbar);
end;

procedure TfmMain.DoFileNewMenu(Sender: TObject; AInvoke: TATCommandInvoke);
begin
  if not AppPython.Inited then
  begin
    MsgBox(msgCommandNeedsPython, MB_OK or MB_ICONWARNING);
    exit;
  end;

  DoPyCommand('cuda_new_file', 'menu', [], AInvoke);
end;

procedure TfmMain.DoCommands_OnMsg(Sender: TObject; const ARes: string);
begin
  MsgStatus(ARes);
end;

procedure TfmMain.MenuTabsizeClick(Sender: TObject);
begin
  UpdateEditorTabsize((Sender as TComponent).Tag);
end;


procedure TfmMain.DoOnDeleteLexer(Sender: TObject; const ALexerName: string);
var
  F: TEditorFrame;
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    F.FixLexerIfDeleted(F.Ed1, ALexerName);
    if not F.EditorsLinked then
      F.FixLexerIfDeleted(F.Ed2, ALexerName);
  end;
end;


procedure TfmMain.SetShowFloatGroup1(AValue: boolean);
begin
  if GetShowFloatGroup1<>AValue then
  begin
    InitFloatGroups;
    FFormFloatGroups1.Visible:= AValue;
  end;
end;

procedure TfmMain.SetShowFloatGroup2(AValue: boolean);
begin
  if GetShowFloatGroup2<>AValue then
  begin
    InitFloatGroups;
    FFormFloatGroups2.Visible:= AValue;
  end;
end;

procedure TfmMain.SetShowFloatGroup3(AValue: boolean);
begin
  if GetShowFloatGroup3<>AValue then
  begin
    InitFloatGroups;
    FFormFloatGroups3.Visible:= AValue;
  end;
end;


procedure TfmMain.FormFloatGroups1_OnEmpty(Sender: TObject);
begin
  ShowFloatGroup1:= false;
end;

procedure TfmMain.FormFloatGroups2_OnEmpty(Sender: TObject);
begin
  ShowFloatGroup2:= false;
end;

procedure TfmMain.FormFloatGroups3_OnEmpty(Sender: TObject);
begin
  ShowFloatGroup3:= false;
end;

procedure TfmMain.FormFloatGroups1_OnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GroupsF1.MoveTabsFromGroupToAnother(
    GroupsF1.Pages1,
    Groups.Pages1
    );
end;

procedure TfmMain.FormFloatGroups2_OnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GroupsF2.MoveTabsFromGroupToAnother(
    GroupsF2.Pages1,
    Groups.Pages1
    );
end;

procedure TfmMain.FormFloatGroups3_OnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GroupsF3.MoveTabsFromGroupToAnother(
    GroupsF3.Pages1,
    Groups.Pages1
    );
end;

function TfmMain.GetFloatGroups: boolean;
begin
  Result:= Assigned(FFormFloatGroups1);
end;

function TfmMain.GetShowFloatGroup1: boolean;
begin
  Result:= Assigned(FFormFloatGroups1) and FFormFloatGroups1.Visible;
end;

function TfmMain.GetShowFloatGroup2: boolean;
begin
  Result:= Assigned(FFormFloatGroups2) and FFormFloatGroups2.Visible;
end;

function TfmMain.GetShowFloatGroup3: boolean;
begin
  Result:= Assigned(FFormFloatGroups3) and FFormFloatGroups3.Visible;
end;


procedure TfmMain.InitFloatGroup(var F: TForm; var G: TATGroups; AIndexOfGroup: integer;
  const ARect: TRect; AOnClose: TCloseEvent; AOnGroupEmpty: TNotifyEvent);
begin
  if not Assigned(F) then
  begin
    F:= TForm.CreateNew(Self);
    F.Hide;
    F.Position:= poDesigned;
    F.BoundsRect:= ARect;
    F.BorderIcons:= [biSystemMenu, biMaximize, biMinimize];
    F.OnClose:= AOnClose;
    F.OnActivate:= @FormActivate;
    F.OnKeyDown:= @FormFloatGroups_OnKeyDown;
    F.OnUTF8KeyPress:= @FormFloatGroups_OnUTF8KeyPress;
    F.Caption:= Format('[f%d]', [AIndexOfGroup]) + (' - ' + msgTitle);
    F.KeyPreview:= true;

    F.AllowDropFiles:= true;
    F.OnDropFiles:= @FormFloatGroups_OnDropFiles;
    F.ShowInTaskBar:= UiOps.FloatGroupsShowInTaskbar;

    G:= TATGroups.Create(Self);
    G.Pages1.EnabledEmpty:= true;
    G.IndexOfGroup:= AIndexOfGroup;
    G.Parent:= F;
    G.Align:= alClient;
    G.Mode:= gmOne;
    G.Images:= ImageListTabs;

    G.OnTabFocus:= @DoOnTabFocus;
    G.OnTabAdd:= @DoOnTabAdd;
    G.OnTabClose:= @DoOnTabClose;
    G.OnTabMove:= @DoOnTabMove;
    G.OnTabPopup:= @DoOnTabPopup;
    //G.OnTabOver:= @DoOnTabOver;
    G.OnTabGetTick:= @DoOnTabGetTick;
    G.OnTabDblClick:= @DoOnTabDblClick;
    G.OnEmpty:= AOnGroupEmpty;

    DoApplyThemeToGroups(G);
    DoApplyUiOpsToGroups(G);
  end;
end;

procedure TfmMain.InitFloatGroups;
begin
  InitFloatGroup(FFormFloatGroups1, GroupsF1, 1, FBoundsFloatGroups1,
    @FormFloatGroups1_OnClose,
    @FormFloatGroups1_OnEmpty);

  InitFloatGroup(FFormFloatGroups2, GroupsF2, 2, FBoundsFloatGroups2,
    @FormFloatGroups2_OnClose,
    @FormFloatGroups2_OnEmpty);

  InitFloatGroup(FFormFloatGroups3, GroupsF3, 3, FBoundsFloatGroups3,
    @FormFloatGroups3_OnClose,
    @FormFloatGroups3_OnEmpty);
end;

function TfmMain.DoOnTabGetTick(Sender: TObject; ATabObject: TObject): Int64;
begin
  Result:= TEditorFrame(ATabObject).Editor.ActivationTime;
end;

function TfmMain.IsWindowMaximizedOrFullscreen: boolean;
begin
  Result:= ShowFullscreen or (WindowState=wsMaximized);
end;

procedure TfmMain.MenuitemClick_CommandFromTag(Sender: TObject);
begin
  CurrentEditor.DoCommand((Sender as TComponent).Tag, TATCommandInvoke.AppToolbar);
end;

procedure TfmMain.MenuitemClick_CommandFromHint(Sender: TObject);
var
  Sep: TATStringSeparator;
  SModule, SProc: string;
begin
  Sep.Init((Sender as TMenuItem).Hint);
  Sep.GetItemStr(SModule);
  Sep.GetItemStr(SProc);
  DoPyCommand(SModule, SProc, [], TATCommandInvoke.AppToolbar);
end;


(*
  //
  function IsQuote(ch: char): boolean; inline;
  begin
    Result:= (ch='''') or (ch='"');
  end;
  //
  function IsDigit(ch: char): boolean; inline;
  begin
    if (ch='-') then exit(true);
    if (ch>='0') and (ch<='9') then exit(true);
    Result:= false;
  end;
  //
  function GetInt: integer;
  var
    N: integer;
  begin
    while (NPos<=Length(Str)) and not IsDigit(Str[NPos]) do Inc(NPos);
    N:= NPos;
    while (N<=Length(Str)) and IsDigit(Str[N]) do Inc(N);
    Result:= StrToIntDef(Copy(Str, NPos, N-NPos), -1);
    NPos:= N;
  end;
  //
  function GetStr: string;
  var
    N: integer;
    quote: char;
  begin
    //find first quote. then find the same quote, for string end.
    while (NPos<=Length(Str)) and not IsQuote(Str[NPos]) do Inc(NPos);
    quote:= Str[NPos];
    Inc(NPos);
    N:= NPos;
    while (N<=Length(Str)) and not ((Str[N]=quote) and (Str[N-1]<>'\')) do Inc(N);
    Result:= Copy(Str, NPos, N-NPos);
    Inc(N);
    NPos:= N;
  end;
*)

procedure TfmMain.DoCodetree_ApplyTreeHelperResults(Tree: TTreeView; Data: PPyObject);
var
  DataItem, DataPos, DataLevel, DataTitle, DataIcon: PPyObject;
  NCount, NX1, NY1, NX2, NY2, NLevel, NLevelPrev, NIcon: integer;
  STitle: string;
  Node, NodeParent: TTreeNode;
  Range: TATRangeInCodeTree;
  iItem, iLevel: integer;
begin
  Tree.BeginUpdate;
  try
    Tree.Items.Clear;

    Node:= nil;
    NodeParent:= nil;
    NLevelPrev:= 1;

    with AppPython.Engine do
    begin
      NCount:= PyList_Size(Data);
      if NCount<=0 then exit;

      for iItem:= 0 to NCount-1 do
      begin
        DataItem:= PyList_GetItem(Data, iItem);
        DataPos:= PyTuple_GetItem(DataItem, 0);
        DataLevel:= PyTuple_GetItem(DataItem, 1);
        DataTitle:= PyTuple_GetItem(DataItem, 2);
        DataIcon:= PyTuple_GetItem(DataItem, 3);

        NX1:= PyLong_AsLong(PyTuple_GetItem(DataPos, 0));
        NY1:= PyLong_AsLong(PyTuple_GetItem(DataPos, 1));
        NX2:= PyLong_AsLong(PyTuple_GetItem(DataPos, 2));
        NY2:= PyLong_AsLong(PyTuple_GetItem(DataPos, 3));
        NLevel:= PyLong_AsLong(DataLevel);
        STitle:= PyUnicodeAsUTF8String(DataTitle);
        NIcon:= PyLong_AsLong(DataIcon);

        if (Node=nil) or (NLevel<=1) then
          NodeParent:= nil
        else
        begin
          NodeParent:= Node;
          for iLevel:= NLevel to NLevelPrev do
            if Assigned(NodeParent) then
              NodeParent:= NodeParent.Parent;
        end;

        Range:= TATRangeInCodeTree.Create;
        Range.PosBegin:= Point(NX1, NY1);
        Range.PosEnd:= Point(NX2, NY2);

        Node:= Tree.Items.AddChildObject(NodeParent, STitle, Range);
        Node.ImageIndex:= NIcon;
        Node.SelectedIndex:= NIcon;

        NLevelPrev:= NLevel;
      end;
    end;
  finally
   Tree.EndUpdate;
  end;
end;

procedure EditorFold_SetTag(Ed: TATSynEdit; const ATag: Int64);
var
  i: integer;
begin
  for i:= 0 to Ed.Fold.Count-1 do
    Ed.Fold.ItemPtr(i)^.Tag:= ATag;
end;

function TfmMain.DoCodetree_ApplyTreeHelperInPascal(Ed, EdPair: TATSynEdit;
  ATree: TTreeView; const ALexer: string): boolean;
var
  Data: TATTreeHelperRecords;
  DataItem: PATTreeHelperRecord;
  NX1, NY1, NX2, NY2, NLevel, NLevelPrev, NIcon: integer;
  STitle: string;
  Node, NodeParent: TTreeNode;
  TreeSavedFold: TAppCodetreeSavedFold;
  Range: TATRangeInCodeTree;
  iItem, iLevel: integer;
const
  cTagOlder = -10;
begin
  Data:= TATTreeHelperRecords.Create;
  if Assigned(ATree) then
  begin
    TreeSavedFold:= Default(TAppCodetreeSavedFold);
    TreeSavedFold.Save(Ed, ATree);
    ATree.BeginUpdate;
    ATree.Items.Clear;
  end;

  try
    Node:= nil;
    NodeParent:= nil;
    NLevelPrev:= 1;

    Result:= TreeHelperInPascal(Ed, ALexer, Data);
    if Result and (Data.Count>0) then
    begin
      EditorFold_SetTag(Ed, cTagOlder);
      if Assigned(EdPair) then
        EditorFold_SetTag(EdPair, cTagOlder);

      for iItem:= 0 to Data.Count-1 do
      begin
        DataItem:= Data.ItemPtr[iItem];

        NX1:= DataItem^.X1;
        NY1:= DataItem^.Y1;
        NX2:= DataItem^.X2;
        NY2:= DataItem^.Y2;
        NLevel:= DataItem^.Level;
        STitle:= DataItem^.Title;
        NIcon:= DataItem^.Icon;

        if Assigned(ATree) then
        begin
          if (Node=nil) or (NLevel<=1) then
            NodeParent:= nil
          else
          begin
            NodeParent:= Node;
            for iLevel:= NLevel to NLevelPrev do
              if Assigned(NodeParent) then
                NodeParent:= NodeParent.Parent;
          end;

          Range:= TATRangeInCodeTree.Create;
          Range.PosBegin:= Point(NX1, NY1);
          Range.PosEnd:= Point(NX2, NY2);

          Node:= ATree.Items.AddChildObject(NodeParent, STitle, Range);
          Node.ImageIndex:= NIcon;
          Node.SelectedIndex:= NIcon;

          NLevelPrev:= NLevel;
        end;

        if NY2-NY1>=1 then
        begin
          //must mark fold ranges with tag=cTagPersistentFoldRange, so lexer adapter
          //won't clear ranges immediately on parsing start.
          //so user is able to do many editings and fold ranges are kept, until next tree-helper run.
          Ed.Fold.Merge(NX1+1, NY1, NX2+1, NY2, STitle, cTagPersistentFoldRange);
          if Assigned(EdPair) then
            EdPair.Fold.Merge(NX1+1, NY1, NX2+1, NY2, STitle, cTagPersistentFoldRange);
        end;
      end;

      Ed.Fold.DeleteAllByTag(cTagOlder);
      if Assigned(EdPair) then
        EdPair.Fold.DeleteAllByTag(cTagOlder);

      Ed.Fold.ClearLineIndexer(Ed.Strings.Count);
      Ed.Fold.UpdateLineIndexer;
      Ed.Update;

      if Assigned(EdPair) then
      begin
        EdPair.Fold.ClearLineIndexer(EdPair.Strings.Count);
        EdPair.Fold.UpdateLineIndexer;
        if EdPair.Visible then
          EdPair.Update;
      end;
    end;
  finally
    FreeAndNil(Data);
    if Assigned(ATree) then
    begin
      TreeSavedFold.Restore(Ed, ATree);
      ATree.EndUpdate;
    end;
  end;

  if Ed.FoldingAsStringTodo<>'' then
  begin
    Ed.FoldingAsString:= Ed.FoldingAsStringTodo;
    Ed.FoldingAsStringTodo:= '';
  end;
end;


procedure TfmMain.DoOnLexerParseProgress(Sender: TObject; AProgress: integer);
begin
  if Application.Terminated then exit;
  FLexerProgressIndex:= AProgress;
  TThread.Queue(nil, @DoOnLexerParseProgress_Sync);
end;

procedure TfmMain.DoOnLexerParseProgress_Sync();
begin
  if Application.Terminated then exit;
  if FLexerProgressIndex>=0 then
    UpdateLexerProgressbar(FLexerProgressIndex, true)
  else
    UpdateLexerProgressbar(0, true{false});
end;

function _FrameListCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  t1, t2: Int64;
begin
  t1:= TEditorFrame(List.Objects[Index1]).Editor.ActivationTime;
  t2:= TEditorFrame(List.Objects[Index2]).Editor.ActivationTime;
  if t1>t2 then
    Result:= -1
  else
  if t1<t2 then
    Result:= 1
  else
    Result:= 0;
end;

procedure TfmMain.DoDialogMenuTabSwitcher;
var
  Pages: TATPages;
  CurFrame, F: TEditorFrame;
  FrameList: TStringList;
  SGroup, SPrefix, SFilename: string;
  iGroup, iTab: integer;
begin
  CurFrame:= CurrentFrame;
  if CurFrame=nil then exit;

  FrameList:= TStringList.Create;
  try
    for iGroup:= 0 to cAppMaxGroup do
    begin
      Pages:= TGroupsHelper.GetPagesOfGroupIndex(iGroup);
      if Pages=nil then Continue;
      for iTab:= 0 to Pages.Tabs.TabCount-1 do
      begin
        F:= Pages.Tabs.GetTabData(iTab).TabObject as TEditorFrame;
        //if F=CurFrame then Continue; //allow current document in list too

        if iGroup>=6 then
          //floating groups
          SGroup:= 'f'+IntToStr(iGroup-6+1)
        else
          //normal groups
          SGroup:= IntToStr(iGroup+1);
        SPrefix:= Format('[%s-%d]  ', [SGroup, iTab+1]);

        if F.EditorsLinked and (F.FileName<>'') then
          SFilename:= ' ('+F.FileName+')'
        else
          SFilename:= '';

        FrameList.AddObject(SPrefix + F.TabCaption + SFilename, F);
      end;
    end;

    if FrameList.Count<=1 then exit;
    FrameList.CustomSort(@_FrameListCompare);

    iTab:= DoDialogMenuList(
      msgPanelTabs,
      FrameList,
      Min(1 {initially select 2nd item}, FrameList.Count-1),
      true
      );
    if iTab<0 then exit;

    F:= FrameList.Objects[iTab] as TEditorFrame;
    SetFrame(F);
    F.SetFocus;
  finally
    FreeAndNil(FrameList);
  end;
end;


procedure TfmMain.DoDialogLexerMenu;
var
  List: TStringList;
  NIndex, i: integer;
  Lexer: TecSyntAnalyzer;
  LexerLite: TATLiteLexer;
  Obj: TObject;
  Frame: TEditorFrame;
  SCaption: string;
  DlgProps: TDlgMenuProps;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  if Frame.FrameKind<>TAppFrameKind.Editor then exit;

  with TIniFile.Create(AppFile_Language) do
  try
    SCaption:= ReadString('m_o', 'l_', 'Lexers');
    SCaption:= StringReplace(SCaption, '&', '', [rfReplaceAll]);
  finally
    Free;
  end;

  List:= TStringList.Create;
  try
    for i:= 0 to AppManager.LexerCount-1 do
    begin
      Lexer:= AppManager.Lexers[i];
      if Lexer.Deleted then Continue;
      if Lexer.Internal then Continue;
      List.AddObject(Lexer.LexerName, Lexer);
    end;

    for i:= 0 to AppManagerLite.LexerCount-1 do
    begin
      LexerLite:= AppManagerLite.Lexers[i];
      List.AddObject(LexerLite.LexerName+msgLiteLexerSuffix, LexerLite);
    end;

    List.Sort;
    List.Insert(0, msgNoLexer);

    DlgProps:= Default(TDlgMenuProps);
    DlgProps.ItemsText:= List.Text;
    DlgProps.InitialIndex:= List.IndexOf(Frame.LexerName[Frame.Editor]);
    DlgProps.Caption:= SCaption;
    DlgProps.NoFuzzy:= not UiOps.ListboxFuzzySearch;

    NIndex:= DoDialogMenuApi(DlgProps);
    if NIndex<0 then exit;

    Obj:= List.Objects[NIndex];
    if Obj=nil then
      Frame.Lexer[Frame.Editor]:= nil
    else
    if Obj is TecSyntAnalyzer then
      Frame.Lexer[Frame.Editor]:= Obj as TecSyntAnalyzer
    else
    if Obj is TATLiteLexer then
      Frame.LexerLite[Frame.Editor]:= Obj as TATLiteLexer;
  finally
    FreeAndNil(List);
  end;

  UpdateStatusbar;
end;


procedure TfmMain.InitPaintTest;
begin
  if not Assigned(PaintTest) then
  begin
    PaintTest:= TPaintBox.Create(Self);
    PaintTest.Height:= 150;
    PaintTest.Align:= alTop;
    PaintTest.Parent:= PanelAll;
    PaintTest.Top:= ToolbarMain.Height;
  end;
end;

procedure TfmMain.InitSaveDlg;
begin
  if not Assigned(SaveDlg) then
  begin
    SaveDlg:= TSaveDialog.Create(Self);
    SaveDlg.Title:= msgDialogTitleSaveAs;
    SaveDlg.Options:= [ofOverwritePrompt,ofPathMustExist,ofEnableSizing,ofDontAddToRecent,ofViewDetail];
  end;
end;

procedure TfmMain.DoGetSaveDialog(var ASaveDlg: TSaveDialog);
begin
  InitSaveDlg;
  ASaveDlg:= SaveDlg;
end;

procedure TfmMain.InitImageListCodetree;
begin
  if not Assigned(ImageListTree) then
  begin
    ImageListTree:= TImageList.Create(Self);
    ImageListTree.AllocBy:= 10;
    CodeTree.Tree.Images:= ImageListTree;
    DoOps_LoadCodetreeIcons;
  end;
end;

procedure TfmMain.DoCodetree_PanelOnEnter(Sender: TObject);
begin
  CodeTree.SetFocus;
end;

procedure TfmMain.FormEnter(Sender: TObject);
var
  Frame: TEditorFrame;
begin
  Frame:= CurrentFrame;
  if Assigned(Frame) then
    Frame.SetFocus;
end;

function TfmMain.DoPyLexerDetection(const Filename: string; Lexers: TStringList): integer;
begin
  if not Assigned(AppLexersLastDetected) then
    AppLexersLastDetected:= TStringList.Create;
  AppLexersLastDetected.Assign(Lexers);
  Result:= 0;
end;

procedure TfmMain.InitConfirmPanel;
const
  //cW = 10; //in avg chars
  cH = 2.5; //in avg chars
begin
  if FCfmPanel=nil then
  begin
    FCfmPanel:= TPanel.Create(Self);
    FCfmPanel.Hide;
    FCfmPanel.BevelInner:= bvNone;
    FCfmPanel.BevelOuter:= bvNone;
    FCfmPanel.Caption:= '??';
    FCfmPanel.OnClick:= @ConfirmButtonOkClick;
    FCfmPanel.OnMouseLeave:= @ConfirmPanelMouseLeave;
  end;

  FCfmPanel.Color:= GetAppColor(TAppThemeColor.ButtonBgOver);
  FCfmPanel.Font.Name:= UiOps.VarFontName;
  FCfmPanel.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);
  FCfmPanel.Font.Color:= GetAppColor(TAppThemeColor.ButtonFont);

  //FCfmPanel.Width:= AppScaleFont(UiOps.VarFontSize)*cW;
  FCfmPanel.Height:= Trunc(ATEditorScaleFont(UiOps.VarFontSize)*cH);
end;

procedure TfmMain.ConfirmButtonOkClick(Sender: TObject);
begin
  FCfmPanel.Hide;
  EditorOpenLink(FCfmLink);
end;

procedure TfmMain.ConfirmPanelMouseLeave(Sender: TObject);
begin
  FCfmPanel.Hide;
end;

procedure TfmMain.FrameConfirmLink(Sender: TObject; const ALink: string);
var
  P: TPoint;
  CurForm: TCustomForm;
begin
  if not UiOps.ConfirmLinksClicks then
  begin
    EditorOpenLink(ALink);
    exit;
  end;

  FCfmLink:= ALink;
  InitConfirmPanel;

  CurForm:= GetParentForm(Sender as TControl);
  FCfmPanel.Hide;
  FCfmPanel.Parent:= CurForm;

  if EditorLinkIsEmail(ALink) then
    FCfmPanel.Caption:= '['+msgLinkOpenEmail+']'
  else
    FCfmPanel.Caption:= '['+msgLinkOpenSite+']';

  FCfmPanel.Width:= FCfmPanel.Canvas.TextWidth(FCfmPanel.Caption)+6;

  P:= Mouse.CursorPos;
  P:= CurForm.ScreenToClient(P);
  FCfmPanel.Left:= P.X - FCfmPanel.Width div 2;
  FCfmPanel.Top:= P.Y - FCfmPanel.Height div 2;
  FCfmPanel.Show;
end;

procedure TfmMain.DoOps_FindPythonLib(Sender: TObject);
const
  SFileMask = 'libpython3.*so*';
var
  L: TStringList;
  Searcher: TListFileSearcher;
  N: integer;
  SDir, S: string;
begin
  {$ifdef windows}
  exit;
  {$endif}

  ////with empty value of "pylib", disable "find python library" command
  //if UiOps.PyLibrary='' then exit;

  SDir:= cSystemLibDir;
  if not InputQuery(msgPythonFindCaption, msgPythonFindFromDir, SDir) then exit;

  L:= TStringList.Create;
  try
    Searcher:= TListFileSearcher.Create(L);
    try
      Searcher.FileAttribute:= faAnyFile and not faHidden{%H-};
      Searcher.FollowSymLink:= false;
      Searcher.OnDirectoryEnter:= @SearcherDirectoryEnter;
      Searcher.Search(SDir, SFileMask, true{SubDirs}, true{CaseSens});
      MsgStatus('');
    finally
      Searcher.Free;
    end;

    if L.Count=0 then
    begin
      MsgStatus(msgCannotFindPython);
      exit
    end;

    N:= DoDialogMenuList(msgPythonFindCaption, L, 0);
    if N<0 then exit;
    S:= L[N];

    DoOps_SaveOptionString('pylib'+cOptionSystemSuffix, S);
    MsgBox(msgSavedPythonLibOption, MB_OK+MB_ICONINFORMATION);
  finally
    FreeAndNil(L);
  end;
end;

procedure TfmMain.SearcherDirectoryEnter(FileIterator: TFileIterator);
const
  NDirCountToShow = 30;
  NCount: integer = 0;
begin
  Inc(NCount);
  if NCount mod NDirCountToShow = 0 then
  begin
    MsgStatus(msgSearchingInDir+' '+FileIterator.FileName);
    Application.ProcessMessages;
  end;
end;


procedure TfmMain.UpdateMenuTheming(AMenu: TPopupMenu);
begin
  {$ifdef windows}
  if UiOps.ThemedMainMenu then
    MenuStyler.ApplyToMenu(AMenu);
  {$endif}
end;

procedure TfmMain.UpdateMenuTheming_MainMenu;
begin
  {$ifdef windows}
  if UiOps.ThemedMainMenu then
    MenuStyler.ApplyToForm(Self);
  {$endif}
end;

procedure TfmMain.UpdateGroupsMode(AMode: TATGroupsMode);
begin
  if AMode=Groups.Mode then exit;

  //during group-mode change, we get redundant OnTabFocus call which
  //clears the tree; FDisableTreeClearing fixes it
  FDisableTreeClearing:= true;
  try
    Groups.Mode:= AMode;
    FNeedUpdateMenuChecks:= true;
  finally
    FDisableTreeClearing:= false;
  end;
end;

procedure TfmMain.UpdateGlobalProgressbar(AValue: integer; AVisible: boolean; AMaxValue: integer=100);
begin
  InitStatusProgress;
  StatusProgress.Visible:= AVisible;
  StatusProgress.MinValue:= 0;
  StatusProgress.MaxValue:= AMaxValue;
  StatusProgress.Progress:= AValue;
end;

procedure TfmMain.UpdateLexerProgressbar(AValue: integer; AVisible: boolean; AMaxValue: integer=100);
begin
  {
  LexerProgress.Visible:= AVisible;
  LexerProgress.MinValue:= 0;
  LexerProgress.MaxValue:= AMaxValue;
  LexerProgress.Progress:= AValue;
  }
end;

procedure TfmMain.FindDialogGetMainEditor(out AEditor: TATSynEdit);
begin
  AEditor:= CurrentEditor;
end;

procedure TfmMain.PyStatusbarPanelClick(Sender: TObject; const ATag: Int64);
var
  Bar: TATStatus;
  BarData: TATStatusData;
  NCell: integer;
begin
  if not (Sender is TATStatus) then exit;
  Bar:= Sender as TATStatus;

  NCell:= Bar.FindPanel(ATag);
  if NCell<0 then exit;

  BarData:= Bar.GetPanelData(NCell);
  if Assigned(BarData) and (BarData.Callback<>'') then
  begin
    DoPyCallbackFromAPI(BarData.Callback,
      [
      AppVariant(0), //id_dlg
      AppVariant(PtrInt(fmMain.Status)), //id_ctl
      AppVariant(ATag), //data
      AppVariant(0) //info
      ],
      [
      'id_dlg',
      'id_ctl',
      'data',
      'info'
      ]);
  end;
end;


function TfmMain.GetUntitledNumberedCaption: string;
const
  AppUntitledCount: integer = 0;
var
  Frame: TEditorFrame;
  NCount, NTabIndex: integer;
  S: string;
begin
  //reset the counter, when many tabs were opened, but were closed later
  if UiOps.TabsResetUntitledCounter then
  begin
    NCount:= FrameCount;
    if NCount=0 then
      AppUntitledCount:= 0
    else
    if NCount=1 then
    begin
      NTabIndex:= -1;
      Frame:= Frames[0];
      if Frame.FileName='' then
      begin
        S:= Frame.TabCaption;
        if SBeginsWith(S, msgUntitledEnglish) then
        begin
          Delete(S, 1, Length(msgUntitledEnglish));
          NTabIndex:= StrToIntDef(S, 0);
        end
        else
        if (UiOps.LangName<>'') and SBeginsWith(S, msgUntitledTab) then
        begin
          Delete(S, 1, Length(msgUntitledTab));
          NTabIndex:= StrToIntDef(S, 0);
        end;
      end;
      if NTabIndex>0 then
        AppUntitledCount:= NTabIndex
      else
        AppUntitledCount:= 0;
    end;
  end;

  Inc(AppUntitledCount);
  Result:= msgUntitledTab+IntToStr(AppUntitledCount);
end;

procedure TfmMain.PopupBottomClearClick(Sender: TObject);
var
  Ed: TATSynEdit;
  Form: TAppFormWithEditor;
begin
  Ed:= (Sender as TMenuItem).Owner as TATSynEdit;
  Form:= FindBottomForm_ByEditor(Ed);
  if Assigned(Form) then
  begin
    Form.Clear;
    UpdateSidebarButtonOverlay;
  end;
end;

procedure TfmMain.PopupBottomCopyClick(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  Ed:= (Sender as TMenuItem).Owner as TATSynEdit;
  Ed.DoCommand(cCommand_ClipboardCopy, TATCommandInvoke.MenuContext);
end;

procedure TfmMain.PopupBottomSelectAllClick(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  Ed:= (Sender as TMenuItem).Owner as TATSynEdit;
  Ed.DoCommand(cCommand_SelectAll, TATCommandInvoke.MenuContext);
end;

procedure TfmMain.PopupBottomWrapClick(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  Ed:= (Sender as TMenuItem).Owner as TATSynEdit;
  Ed.DoCommand(cCommand_ToggleWordWrap, TATCommandInvoke.MenuContext);
end;


procedure TfmMain.InitBottomEditor(var Form: TAppFormWithEditor);
begin
  Form:= TAppFormWithEditor.Create(Self);
  Form.ShowInTaskBar:= stNever;
  Form.BorderStyle:= bsNone;
  Form.IsDlgCounterIgnored:= true;

  Form.Ed:= TATSynEdit.Create(Form);
  Form.Ed.Name:= 'log';
  Form.Ed.Parent:= Form;
  Form.Ed.Align:= alClient;

  Form.Ed.OptRulerVisible:= false;
  Form.Ed.OptGutterVisible:= true;
  Form.Ed.Gutter[Form.Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible:= false;
  Form.Ed.Gutter[Form.Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible:= false;
  Form.Ed.Gutter[Form.Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagLineStates)].Visible:= false;
  Form.Ed.Gutter[Form.Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks)].Visible:= false;
  Form.Ed.Gutter[Form.Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagEmpty)].Size:= 3;
  Form.Ed.OptUnprintedVisible:= false;
  Form.Ed.OptShowMouseSelFrame:= false;
  Form.Ed.OptShowCurLine:= true;
  Form.Ed.OptCaretManyAllowed:= false;
  Form.Ed.OptMarginRight:= 2000;
  Form.Ed.ModeReadOnly:= true;

  InitPopupBottom(Form.Popup, Form.Ed);
  Form.Ed.PopupText:= Form.Popup;

  //support dlg_proc API, it needs PropsObject
  DoControl_InitPropsObject(Form.Ed, Form, 'editor');

  Form.Ed.OnClickDouble:= @EditorOutput_OnClickDbl;
  Form.Ed.OnKeyDown:= @EditorOutput_OnKeyDown;
end;


procedure TfmMain.TimerMouseStopTimer(Sender: TObject);
//call API event on_mouse_stop.
//we cannot reuse the TIdleTimer because it's not fired when Ctrl/Alt/Shift are holded.

  function _IsPointsDiffByDelta(const P1, P2: TPoint; Delta: integer): boolean;
  begin
    Result:=
      (Abs(P1.X-P2.X)>=Delta) or
      (Abs(P1.Y-P2.Y)>=Delta);
  end;

const
  cPixelDelta = 7;
var
  PntScreen, PntLocal: TPoint;
  Ed: TATSynEdit;
  iGroup: integer;
begin
  PntScreen:= Mouse.CursorPos;
  if _IsPointsDiffByDelta(PntScreen, FLastMousePos, cPixelDelta) then
  begin
    FLastMousePos:= PntScreen;
    for iGroup:= 0 to cAppMaxGroup do
    begin
      Ed:= TGroupsHelper.GetEditorActiveInGroup(iGroup);
      if Ed=nil then Continue; //not Break: support 3 floating grps
      if not Ed.Visible then Continue;
      PntLocal:= Ed.ScreenToClient(PntScreen);
      if PtInRect(Ed.ClientRect, PntLocal) then
      begin
        if not EditorIsEmpty(Ed) then
          DoPyEvent(Ed, TAppPyEvent.OnMouseStop,
            [AppVariant(PntLocal.X), AppVariant(PntLocal.Y)]);
        Break;
      end;
    end;
  end;
end;

procedure TfmMain.DoOnConsoleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Ctl: TWinControl;
  Frame: TEditorFrame;
begin
  //handle Tab-key because LCL by default can jump to side-panel form
  if (Key=VK_TAB) and (Shift=[]) then
  begin
    if fmConsole.EdInput.Focused then
      Ctl:= fmConsole.EdMemo
    else
      Ctl:= fmConsole.EdInput;
    if Ctl.Visible and Ctl.CanFocus then
      Ctl.SetFocus;
    Key:= 0;
    exit
  end;

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
      Frame.SetFocus;
    Key:= 0;
    exit
  end;

  inherited KeyDown(Key, Shift);
end;


procedure TfmMain.mnuEditClick(Sender: TObject);
var
  Ed: TATSynEdit;
  bSel: boolean;
begin
  Ed:= CurrentEditor;
  if Ed=nil then exit;

  bSel:= Ed.Carets.IsSelection;

  if Assigned(mnuEditUndo) then
    mnuEditUndo.Enabled:= not Ed.Strings.UndoEmpty;

  if Assigned(mnuEditRedo) then
    mnuEditRedo.Enabled:= not Ed.Strings.RedoEmpty;

  if Assigned(mnuEditPaste) then
  begin
    mnuEditPaste.Enabled:= Clipboard.HasFormat(CF_Text);
    if Assigned(mnuEditPasteIndent) then
      mnuEditPasteIndent.Enabled:= mnuEditPaste.Enabled;
    if Assigned(mnuEditPasteHist) then
      mnuEditPasteHist.Enabled:= Assigned(ATEditorClipboardRecents) and (ATEditorClipboardRecents.Count>0);
  end;

  if Assigned(mnuEditCopy) then
    mnuEditCopy.Enabled:= Ed.OptCopyLinesIfNoSel or bSel;

  if Assigned(mnuEditCut) then
    mnuEditCut.Enabled:= Ed.OptCutLinesIfNoSel or bSel;

  if Assigned(mnuEditCopyAppend) then
    mnuEditCopyAppend.Enabled:= bSel;
end;


procedure TfmMain.DoShowForVisibleFrames;
var
  Frame: TEditorFrame;
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
  begin
    Frame:= Frames[i];
    if Frame.Visible then
      Frame.DoShow;
  end;
end;

procedure TfmMain.DoDialogUnprinted(Ed: TATSynEdit);
var
  Form: TfmUnprinted;
  Symbol: TATEditorUnptintedEolSymbol;
begin
  Symbol:= ATEditorOptions.UnprintedEndSymbol;

  Form:= TfmUnprinted.Create(nil);
  try
    EditorApplyTheme(Form.EdPreview);

    Form.OnSaveOptionBool:= @DoOps_SaveOptionBool;
    Form.OnSaveOptionString:= @DoOps_SaveOptionString;

    Form.chkVisible.Checked:= Ed.OptUnprintedVisible;
    Form.chkShowWhitespace.Checked:= Ed.OptUnprintedSpaces;
    Form.chkOnlyTrail.Checked:= Ed.OptUnprintedSpacesTrailing;
    Form.chkOnlyLeadAndTrail.Checked:= Ed.OptUnprintedSpacesBothEnds;
    Form.chkOnlyInSel.Checked:= Ed.OptUnprintedSpacesOnlyInSelection;
    Form.chkAlsoInSel.Checked:= Ed.OptUnprintedSpacesAlsoInSelection;
    Form.chkForceShowTabs.Checked:= Ed.OptUnprintedForceTabs;
    Form.chkShowEndMarks.Checked:= Ed.OptUnprintedEnds;
    case Symbol of
      TATEditorUnptintedEolSymbol.Dot:
        Form.chkEndDot.Checked:= true;
      TATEditorUnptintedEolSymbol.ArrowDown:
        Form.chkEndArrow.Checked:= true;
      TATEditorUnptintedEolSymbol.Pilcrow:
        Form.chkEndPilcrow.Checked:= true;
    end;
    Form.chkEndDetails.Checked:= Ed.OptUnprintedEndsDetails;

    if Form.ShowModal=mrOk then
    begin
      Form.ApplyToEditor(Ed);
      UpdateToolbarButtons(CurrentFrame);
    end;
  finally
    FreeAndNil(Form);
  end;
end;

procedure TfmMain.InvalidateMouseoverDependantControls;
var
  i: integer;
begin
  for i:= 0 to Groups.PagesVisibleCount-1 do
    Groups.Pages[i].Tabs.Invalidate;

  Status.Invalidate;

  if ATFlatTheme.EnableColorBgOver then
  begin
    ToolbarSideTop.Invalidate;
    ToolbarSideMid.Invalidate;
    ToolbarSideLow.Invalidate;
  end;
end;

function TfmMain.CodeTreeFilter_OnFilterNode(ItemNode: TTreeNode; out Done: Boolean): Boolean;
var
  SNodeText, SFilter, SItem: string;
  Sep: TATStringSeparator;
begin
  SNodeText:= AnsiLowerCase(ItemNode.Text);

  SFilter:= AnsiLowerCase(CodeTreeFilter.Filter);
  SFilter:= Trim(StringReplace(SFilter, '  ', ' ', [rfReplaceAll]));

  Sep.Init(SFilter, ' ');
  while Sep.GetItemStr(SItem) and (SItem<>'') do
  begin
    if Pos(SItem, SNodeText)=0 then
    begin
      Done:= true;
      exit(false);
    end;
  end;
  Done:= true;
  Result:= true;
end;


procedure TfmMain.DoFocusUsualGroup(AIndex: integer);
begin
  case AIndex of
    0..High(TATGroupsNums):
      Groups.PagesSetIndex(AIndex);
    else
      exit;
  end;
end;

procedure TfmMain.DoFocusFloatingGroup(AIndex: integer);
var
  Form: TForm;
  Grp: TATGroups;
begin
  case AIndex of
    0:
      begin
        Form:= FFormFloatGroups1;
        Grp:= GroupsF1;
      end;
    1:
      begin
        Form:= FFormFloatGroups2;
        Grp:= GroupsF2;
      end;
    2:
      begin
        Form:= FFormFloatGroups3;
        Grp:= GroupsF3;
      end;
    else
      exit;
  end;

  if Form=nil then exit;
  if Grp=nil then exit;
  if Grp.GetTabTotalCount<=0 then exit;

  DoFormFocus(Form, true);
end;


procedure TfmMain.DoFocusNextGroup(ANext: boolean);
const
  cFloatingGroups = 3;
var
  FormsArray: array[0..Pred(cFloatingGroups)] of TForm;
  //
  function TryFocusLastFloating(AFromIndex: integer): boolean;
  var
    i: integer;
  begin
    Result:= false;
    for i:= AFromIndex downto Low(FormsArray) do
      if Assigned(FormsArray[i]) and FormsArray[i].Visible then
      begin
        DoFocusFloatingGroup(i);
        Result:= true;
        exit;
      end
  end;
  //
  function TryFocusFirstFloating(AFromIndex: integer): boolean;
  var
    i: integer;
  begin
    Result:= false;
    for i:= AFromIndex to High(FormsArray) do
      if Assigned(FormsArray[i]) and FormsArray[i].Visible then
      begin
        DoFocusFloatingGroup(i);
        Result:= true;
        exit;
      end
  end;
  //
var
  Frame: TEditorFrame;
  Grp: TATGroups;
  Pages: TATPages;
  NNewGroupIndex, NLocalGroupIndex, NGlobalGroupIndex, NTabIndex: integer;
  bWrapped: boolean;
begin
  FormsArray[0]:= FFormFloatGroups1;
  FormsArray[1]:= FFormFloatGroups2;
  FormsArray[2]:= FFormFloatGroups3;

  Frame:= CurrentFrame;
  if Frame=nil then exit;
  GetFrameLocation(Frame, Grp, Pages, NLocalGroupIndex, NGlobalGroupIndex, NTabIndex);

  case NGlobalGroupIndex of
    0..High(TATGroupsNums):
      begin
        //usual group is focused
        NNewGroupIndex:= Groups.PagesNextIndex(NLocalGroupIndex, ANext, false, bWrapped);
        if bWrapped then
        begin
          if ANext then
          begin
            if not TryFocusFirstFloating(0) then
              DoFocusUsualGroup(NNewGroupIndex);
          end
          else
          begin
            if not TryFocusLastFloating(cFloatingGroups-1) then
              DoFocusUsualGroup(NNewGroupIndex);
          end;
        end
        else
        begin
          if NNewGroupIndex>=0 then
            DoFocusUsualGroup(NNewGroupIndex);
        end;
      end;

    High(TATGroupsNums)+1..
    High(TATGroupsNums)+1+cFloatingGroups:
      begin
        //floating group is focused
        NLocalGroupIndex:= NGlobalGroupIndex-(High(TATGroupsNums)+1);
        if ANext then
        begin
          if not TryFocusFirstFloating(NLocalGroupIndex+1) then
            DoFocusUsualGroup(0);
        end
        else
        begin
          if not TryFocusLastFloating(NLocalGroupIndex-1) then
          begin
            NLocalGroupIndex:= Groups.PagesNextIndex(High(TATGroupsNums)+1, ANext, false, bWrapped);
            DoFocusUsualGroup(NLocalGroupIndex);
          end;
        end;
      end;
  end;
end;


//----------------------------
{$I formmain_loadsave.inc}
{$I formmain_updates_proc.inc}
{$I formmain_translation.inc}
{$I formmain_frame_proc.inc}
{$I formmain_tab_proc.inc}
{$I formmain_find.inc}
{$I formmain_cmd.inc}
{$I formmain_themes.inc}
{$I formmain_sidepanel.inc}
{$I formmain_bottompanel.inc}
{$I formmain_commandline.inc}

end.
