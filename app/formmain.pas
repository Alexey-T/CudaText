(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit FormMain;

{$mode objfpc}{$H+}

{$IFDEF DEBUG}
{$INLINE OFF}
{$ENDIF}
//{$define debug_on_lexer}

interface

uses
  {$ifdef windows}
  Windows,
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
  at__jsonconf,
  PythonEngine,
  {$ifdef unix}
  AppUniqueInstance,
  {$endif}
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
  ATSynEdit_CanvasProc,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Adapter_LiteLexer,
  ATSynEdit_CharSizer,
  ATSynEdit_Export_HTML,
  ATSynEdit_Edits,
  ATSynEdit_Form_Complete,
  ATSynEdit_Form_Complete_SynWrite,
  ATSynEdit_Form_Complete_CSS,
  ATSynEdit_Form_Complete_HTML,
  ATTabs,
  ATGroups,
  ATStatusBar,
  ATStrings,
  ATStringProc,
  ATGauge,
  ATBinHex,
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
  proc_windows_link,
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
  Math;

type
  { TAppNotifThread }

  TAppNotifThread = class(TThread)
  private
    CurFrame: TEditorFrame;
    procedure HandleOneFrame;
    procedure NotifyFrame1;
    procedure NotifyFrame2;
  protected
    procedure Execute; override;
  end;

  TAppStringArray = array of string;

var
  AppNotifThread: TAppNotifThread = nil;

{$ifdef unix}
var
  AppUniqInst: TUniqueInstance = nil;
{$endif}

type
  TATFindMarkingMode = (
    markingNone,
    markingSelections,
    markingMarkers,
    markingBookmarks
    );

  TATMenuItemsAlt = record
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

const
  cMenuTabsizeMin = 1;
  cMenuTabsizeMax = 10;

type
  { TfmMain }
  TfmMain = class(TForm)
    AppProps: TApplicationProperties;
    ButtonCancel: TATButton;
    mnuOpThemes: TMenuItem;
    mnuOpLangs: TMenuItem;
    mnuViewSidebar: TMenuItem;
    mnuGr6H: TMenuItem;
    mnuGr6V: TMenuItem;
    mnuViewFloatSide: TMenuItem;
    mnuViewFloatBottom: TMenuItem;
    mnuOpDefaultUser: TMenuItem;
    TimerStatusWork: TTimer;
    TimerAppIdle: TIdleTimer;
    ImageListTabs: TImageList;
    MenuItem5: TMenuItem;
    mnuSelExtWord: TMenuItem;
    mnuViewOnTop: TMenuItem;
    mnuOpPlugins: TMenuItem;
    mnuViewUnpriSpacesTail: TMenuItem;
    mnuViewMicromap: TMenuItem;
    mnuHelpCheckUpd: TMenuItem;
    StatusProgress: TATGauge;
    MenuItem4: TMenuItem;
    mnuViewDistFree: TMenuItem;
    SepV4: TMenuItem;
    mnuBmPlaceOnCarets: TMenuItem;
    mnuFileNewMenu: TMenuItem;
    mnuPluginsEmpty: TMenuItem;
    ImageListSide: TImageList;
    ImageListBm: TImageList;
    MainMenu: TMainMenu;
    SepOp2: TMenuItem;
    mnuBmDeleteLines: TMenuItem;
    mnuBmCopyLines: TMenuItem;
    mnuOpThemeSyntax: TMenuItem;
    mnuBmPlaceCarets: TMenuItem;
    PanelAll: TATPanelSimple;
    PanelMain: TATPanelSimple;
    PanelEditors: TATPanelSimple;
    PanelSide: TATPanelSimple;
    SepV3: TMenuItem;
    mnuLexers: TMenuItem;
    mnuHelpIssues: TMenuItem;
    mnuOpLexMap: TMenuItem;
    mnuTextOpenUrl: TMenuItem;
    mnuFontOutput: TMenuItem;
    mnuGr1p2H: TMenuItem;
    mnuEditSpToTab: TMenuItem;
    SepEd7: TMenuItem;
    mnuEditTabToSp: TMenuItem;
    mnuEditCharmap: TMenuItem;
    SepV2: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem14: TMenuItem;
    SepHelp1: TMenuItem;
    SepHelp2: TMenuItem;
    SepFile1: TMenuItem;
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
    mnuPlugins: TMenuItem;
    mnuViewSide: TMenuItem;
    mnuOpKeys: TMenuItem;
    mnuHelpWiki: TMenuItem;
    mnuOpThemeUi: TMenuItem;
    mnuEditTrimL: TMenuItem;
    mnuEditTrimR: TMenuItem;
    mnuEditTrim: TMenuItem;
    mnuFindPrev: TMenuItem;
    mnuOpLexLib: TMenuItem;
    mnuOpLexSub: TMenuItem;
    mnuOpLexProp: TMenuItem;
    mnuFileCloseDel: TMenuItem;
    mnuOpLexer: TMenuItem;
    mnuViewStatus: TMenuItem;
    mnuViewFullscr: TMenuItem;
    mnuFindWordNext: TMenuItem;
    mnuFindWordPrev: TMenuItem;
    SepSr2: TMenuItem;
    mnuHelpForum: TMenuItem;
    mnuViewToolbar: TMenuItem;
    mnuFontText: TMenuItem;
    mnuFontUi: TMenuItem;
    mnuFontSub: TMenuItem;
    mnuFileReopen: TMenuItem;
    mnuOpUser: TMenuItem;
    SepOp1: TMenuItem;
    mnuOp: TMenuItem;
    mnuOpDefault: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuBmSub: TMenuItem;
    mnuFindRepDialog: TMenuItem;
    mnuFindNext: TMenuItem;
    mnuFindDlg: TMenuItem;
    SepSr1: TMenuItem;
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
    mnuEditCopySub: TMenuItem;
    mnuGotoLine: TMenuItem;
    mnuSr: TMenuItem;
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
    mnuFileOpenDir: TMenuItem;
    mnuFileOpenSub: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuFileClose: TMenuItem;
    PopupText: TPopupMenu;
    PopupRecents: TPopupMenu;
    TimerStatusAlt: TTimer;
    TimerTreeFill: TTimer;
    TimerCmd: TTimer;
    TimerStatusClear: TTimer;
    ToolbarMain: TATFlatToolbar;
    ToolbarSideMid: TATFlatToolbar;
    ToolbarSideLow: TATFlatToolbar;
    ToolbarSideTop: TATFlatToolbar;
    procedure AppPropsActivate(Sender: TObject);
    procedure AppPropsEndSession(Sender: TObject);
    procedure AppPropsQueryEndSession(var Cancel: Boolean);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var ACanClose: boolean);
    procedure FormColorsApply(const AColors: TAppTheme);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FrameAddRecent(Sender: TObject);
    procedure FrameOnMsgStatus(Sender: TObject; const AStr: string);
    procedure FrameOnChangeCaretPos(Sender: TObject);
    procedure FrameOnInitAdapter(Sender: TObject);
    procedure FrameParseDone(Sender: TObject);
    procedure ListboxOutClick(Sender: TObject);
    procedure ListboxOutDrawItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure ListboxOutKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuTabColorClick(Sender: TObject);
    procedure mnuTabCopyDirClick(Sender: TObject);
    procedure mnuTabCopyFullPathClick(Sender: TObject);
    procedure mnuTabCopyNameClick(Sender: TObject);
    procedure mnuTabMoveF2Click(Sender: TObject);
    procedure mnuTabMoveF3Click(Sender: TObject);
    procedure DoHelpAbout;
    procedure DoHelpForum;
    procedure DoHelpWiki;
    procedure DoHelpIssues;
    procedure DoHelpHotkeys;

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
    procedure TimerStatusAltTimer(Sender: TObject);
    procedure TimerStatusWorkTimer(Sender: TObject);
    procedure TimerStatusClearTimer(Sender: TObject);
    procedure TimerTreeFillTimer(Sender: TObject);
    procedure UniqInstanceOtherInstance(Sender: TObject; ParamCount: Integer;
      const Parameters: array of String);
    {$ifdef windows}
    procedure SecondInstance(const Msg: TBytes);
    {$endif}
  private
    { private declarations }
    SaveDlg: TSaveDialog;
    ImageListTree: TImageList;
    PopupTab: TPopupMenu;
    PopupTree: TPopupMenu;
    PopupEnds: TPopupMenu;
    PopupEnc: TPopupMenu;
    PopupLex: TPopupMenu;
    PopupTabSize: TPopupMenu;
    PopupViewerMode: TPopupMenu;
    PopupPicScale: TPopupMenu;
    PopupListboxOutput: TPopupMenu;
    PopupListboxValidate: TPopupMenu;
    mnuTabCloseAllAll: TMenuItem;
    mnuTabCloseAllSame: TMenuItem;
    mnuTabCloseLeft: TMenuItem;
    mnuTabCloseOtherAll: TMenuItem;
    mnuTabCloseOtherSame: TMenuItem;
    mnuTabCloseRight: TMenuItem;
    mnuTabCloseSub: TMenuItem;
    mnuTabCloseThis: TMenuItem;
    mnuTabColor: TMenuItem;
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
    mnuEndsWin: TMenuItem;
    mnuEndsUnix: TMenuItem;
    mnuEndsMac: TMenuItem;
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
    mnuContextOutputCopy: TMenuItem;
    mnuContextOutputClear: TMenuItem;
    mnuContextValidateCopy: TMenuItem;
    mnuContextValidateClear: TMenuItem;
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
    FListTimers: TStringList;
    FConsoleMustShow: boolean;
    FSessionIsLoading: boolean;
    FColorDialog: TColorDialog;
    Status: TATStatus;
    StatusForm: TForm;
    StatusFormLabel: TLabel;
    Groups: TATGroups;
    GroupsCtx: TATGroups;
    GroupsF1: TATGroups;
    GroupsF2: TATGroups;
    GroupsF3: TATGroups;

    mnuApple: TMenuItem;
    mnuApple_About: TMenuItem;
    //mnuApple_Quit: TMenuItem;

    mnuViewWrap_Alt,
    mnuViewNums_Alt,
    mnuViewFold_Alt,
    mnuViewRuler_Alt,
    mnuViewMinimap_Alt,
    mnuViewMicromap_Alt,
    mnuViewSplitDo_Alt,
    mnuViewSplitHorz_Alt,
    mnuViewUnpriShow_Alt,
    mnuViewUnpriSpaces_Alt,
    mnuViewUnpriSpacesTail_Alt,
    mnuViewUnpriEnds_Alt,
    mnuViewUnpriEndsDet_Alt,
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
    mnuGr6_Alt: TATMenuItemsAlt;

    FFinder: TATEditorFinder;
    FFindStop: boolean;
    FFindConfirmAll: TModalResult;
    FFindMarkingMode: TATFindMarkingMode;
    FFindMarkingCaret1st: boolean;
    FShowFullScreen: boolean;
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
    FFileNamesDroppedInitially: array of string;
    FFileNameLogDebug: string;
    FFileNameLogConsole: string;
    FCodetreeBuffer: TTreeView;
    FCodetreeDblClicking: boolean;
    FCodetreeModifiedVersion: integer;
    FCodetreeNeedsSelJump: boolean;
    FCfmPanel: TPanel;
    FCfmLink: string;
    FMenuVisible: boolean;
    FNewClickedEditor: TATSynEdit;
    FPyComplete_Editor: TATSynEdit;
    FPyComplete_Text: string;
    FPyComplete_CharsLeft: integer;
    FPyComplete_CharsRight: integer;
    FPyComplete_CaretPos: TPoint;
    FLastDirOfOpenDlg: string;
    FLastLexerForPluginsMenu: string;
    FLastStatusbarMessage: string;
    FLastSelectedCommand: integer;
    FLastMousePos: TPoint;
    FLastMaximized: boolean;
    FLastMaximizedMonitor: integer;
    FLastFocusedFrame: TComponent;
    FInvalidateShortcuts: boolean;
    FInvalidateShortcutsForce: boolean;
    FLexerProgressIndex: integer;
    FOption_WindowPos: string;
    FOption_AllowSession: boolean;
    FOption_GroupMode: TATGroupsMode;
    FOption_GroupSizes: TATGroupsPoints;
    FOption_GroupPanelSize: TPoint;
    FOption_SidebarTab: string;
    FCmdlineFileCount: integer;

    procedure ConfirmButtonOkClick(Sender: TObject);
    procedure ConfirmPanelMouseLeave(Sender: TObject);
    procedure FrameConfirmLink(Sender: TObject; const ALink: string);
    procedure FormEnter(Sender: TObject);
    procedure GetParamsForUniqueInstance(out AParams: TAppStringArray);
    procedure PythonEngineAfterInit(Sender: TObject);
    procedure PythonIOSendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PythonModuleInitialization(Sender: TObject);
    procedure CodeTreeFilter_OnChange(Sender: TObject);
    procedure CodeTreeFilter_ResetOnClick(Sender: TObject);
    procedure CodeTreeFilter_OnCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    procedure DisablePluginMenuItems;
    procedure DoApplyNewdocLexer(F: TEditorFrame);
    procedure DoApplyCenteringOption;
    procedure DoApplyLexerStyleMaps(AndApplyTheme: boolean);
    procedure DoApplyTranslationToGroups(G: TATGroups);
    procedure DoClearSingleFirstTab;
    procedure DoCloseAllTabs;
    procedure DoDialogMenuThemes_ThemeSyntaxSelect(const AStr: string);
    procedure DoDialogMenuThemes_ThemeUiSelect(const AStr: string);
    procedure DoFileDialog_PrepareDir(Dlg: TFileDialog);
    procedure DoFileDialog_SaveDir(Dlg: TFileDialog);
    procedure DoCommandsMsgStatus(Sender: TObject; const ARes: string);
    procedure DoFindMarkingInit(AMode: TATFindMarkingMode);
    procedure DoFindOptions_OnChange(Sender: TObject);
    procedure DoFindOptions_ApplyDict(const AText: string);
    procedure DoFindOptions_ResetInSelection;
    function DoFindOptions_GetDict: PPyObject;
    procedure DoFolderOpen(const ADirName: string; ANewProject: boolean);
    procedure DoGetSaveDialog(var ASaveDlg: TSaveDialog);
    procedure DoGroupsChangeMode(Sender: TObject);
    procedure DoOnLexerParseProgress(Sender: TObject; AProgress: integer);
    //procedure DoOnLexerParseProgress(Sender: TObject; ALineIndex, ALineCount: integer);
    procedure DoOnLexerParseProgress_Sync();
    procedure DoOps_AddPluginMenuItem(const ACaption: string; ASubMenu: TMenuItem; ALangFile: TIniFile; ATag: integer);
    procedure DoOps_LexersDisableInFrames(ListNames: TStringList);
    procedure DoOps_LexersRestoreInFrames(ListNames: TStringList);
    procedure DoOps_LoadOptions_Editor(cfg: TJSONConfig; var Op: TEditorOps);
    procedure DoOps_LoadOptions_Global(cfg: TJSONConfig);
    procedure DoOps_LoadOptions_Ui(cfg: TJSONConfig);
    procedure DoOps_LoadOptions_UiAutoCompletion(cfg: TJSONConfig);
    procedure DoShowFirstStartInfo;
    procedure DoOps_OnCreate;
    function FindFrameOfFilename(const AName: string): TEditorFrame;
    procedure FixMainLayout;
    procedure FormFloatGroups1_OnEmpty(Sender: TObject);
    procedure FormFloatGroups2_OnEmpty(Sender: TObject);
    procedure FormFloatGroups3_OnEmpty(Sender: TObject);
    procedure FormFloatGroups_OnDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormFloatGroups1_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups2_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups3_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    function GetSessionFilename: string;
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
    procedure DoOnTabAdd(Sender: TObject);
    procedure DoOnTabClose(Sender: TObject; ATabIndex: Integer; var ACanClose, ACanContinue: boolean);
    procedure DoOnTabMove(Sender: TObject; NFrom, NTo: Integer);
    //procedure DoOnTabOver(Sender: TObject; ATabIndex: Integer);
    procedure DoOnTabPopup(Sender: TObject; APages: TATPages; ATabIndex: integer);
    function DoOnTabGetTick(Sender: TObject; ATabObject: TObject): Int64;
    procedure DoCodetree_PanelOnEnter(Sender: TObject);
    procedure DoCodetree_StopUpdate;
    procedure DoCodetree_OnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DoCodetree_GetSyntaxRange(ANode: TTreeNode; out APosBegin, APosEnd: TPoint);
    procedure DoCodetree_SetSyntaxRange(ANode: TTreeNode; const APosBegin, APosEnd: TPoint);
    procedure DoCodetree_OnDblClick(Sender: TObject);
    procedure DoCodetree_OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoCodetree_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoCodetree_GotoBlockForCurrentNode(AndSelect: boolean);
    procedure DoCodetree_ApplyTreeHelperResults(Data: PPyObject);
    procedure DoSidebar_OnCloseFloatForm(Sender: TObject; var CloseAction: TCloseAction);
    function DoSidebar_GetFormTitle(const ACaption: string): string;
    procedure DoSidebar_OnPythonCall(const ACallback: string);
    procedure DoSidebar_OnShowCodeTree(Sender: TObject);
    function DoSidebar_FilenameToImageIndex(ATabCaption, AFilename: string): integer;
    procedure DoSidebar_ListboxDrawItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure DoSidebar_MainMenuClick(Sender: TObject);
    procedure DoSidebar_FocusCodetreeFilter;
    procedure DoSidebar_FocusCodetree;
    procedure DoBottom_OnHide(Sender: TObject);
    procedure DoBottom_OnCloseFloatForm(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoBottom_FindClick(Sender: TObject);
    procedure DoAutoComplete(Ed: TATSynEdit);
    procedure DoPyCommand_Cudaxlib(Ed: TATSynEdit; const AMethod: string);
    procedure DoDialogCharMap;
    procedure DoFindActionFromString(const AStr: string);
    procedure DoGotoFromInput(const AInput: string);
    procedure DoGotoDefinition(Ed: TATSynEdit);
    procedure DoShowFuncHint(Ed: TATSynEdit);
    procedure DoHideFuncHint;
    procedure DoApplyGutterVisible(AValue: boolean);
    procedure DoApplyFrameOps(F: TEditorFrame; const Op: TEditorOps; AForceApply: boolean);
    procedure DoApplyFont_Text;
    procedure DoApplyFont_Ui;
    procedure DoApplyFont_UiStatusbar;
    procedure DoApplyFont_Output;
    procedure DoApplyAllOps;
    procedure DoApplyTheme;
    procedure DoApplyThemeToGroups(G: TATGroups);
    procedure DoClearRecentFileHistory;
    function DoOnMessage(const AText: string): boolean;
    function DoOnConsoleNav(const Str: string): boolean;
    procedure DoOnConsoleNumberChange(Sender: TObject);
    function DoOnMacro(Frame: TEditorFrame; const Str: string): boolean;
    function DoDialogConfigTheme(var AData: TAppTheme; AThemeUI: boolean): boolean;
    function DoDialogMenuApi(const AProps: TDlgMenuProps): integer;
    procedure DoDialogMenuTranslations;
    procedure DoDialogMenuThemes;
    procedure DoFileExportHtml(F: TEditorFrame);
    function DoFileInstallZip(const fn: string; out DirTarget: string; ASilent: boolean): boolean;
    procedure DoFileCloseAndDelete(Ed: TATSynEdit);
    procedure DoFileNew;
    procedure DoFileNewMenu(Sender: TObject);
    procedure DoFileNewFrom(const fn: string);
    procedure DoFileSave(Ed: TATSynEdit);
    procedure DoFileSaveAs(Ed: TATSynEdit);
    procedure DoFocusEditor(Ed: TATSynEdit);
    procedure DoSwitchTab(ANext: boolean);
    procedure DoSwitchTabSimply(ANext: boolean);
    procedure DoSwitchTabToRecent;
    procedure DoPyTimerTick(Sender: TObject);
    procedure DoPyRunLastPlugin;
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
    procedure InitAppleMenu;
    procedure InitImageListCodetree;
    procedure InitPaintTest;
    procedure InitPopupTree;
    procedure InitPopupPicScale;
    procedure InitPopupListboxOutput;
    procedure InitPopupListboxValidate;
    procedure InitPopupViewerMode;
    procedure InitPopupEnc;
    procedure InitPopupEnds;
    procedure InitPopupLex;
    procedure InitPopupTab;
    procedure InitPopupTabSize;
    procedure InitFloatGroup(var F: TForm; var G: TATGroups; ATag: integer;
      const ARect: TRect; AOnClose: TCloseEvent; AOnGroupEmpty: TNotifyEvent);
    procedure InitFloatGroups;
    procedure InitSaveDlg;
    procedure InitSidebar;
    procedure InitToolbar;
    function IsWindowMaximizedOrFullscreen: boolean;
    function IsAllowedToOpenFileNow: boolean;
    function IsThemeNameExist(const AName: string; AThemeUI: boolean): boolean;
    procedure ListboxOutContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ListboxValidateContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure PopupToolbarCaseOnPopup(Sender: TObject);
    procedure PopupToolbarCommentOnPopup(Sender: TObject);
    procedure LiteLexer_ApplyStyle(Sender: TObject; AStyleHash: integer; var APart: TATLinePart);
    function LiteLexer_GetStyleHash(Sender: TObject; const AStyleName: string): integer;
    procedure MenuRecentsClear(Sender: TObject);
    procedure MenuRecentsPopup(Sender: TObject);
    procedure MenuRecentItemClick(Sender: TObject);
    procedure MenuEncWithReloadClick(Sender: TObject);
    procedure MenuitemClick_CommandFromTag(Sender: TObject);
    procedure MenuitemClick_CommandFromHint(Sender: TObject);
    procedure MenuPicScaleClick(Sender: TObject);
    procedure MenuPluginClick(Sender: TObject);
    procedure MenuTabsizeClick(Sender: TObject);
    procedure MenuViewerModeClick(Sender: TObject);
    procedure MenuEncNoReloadClick(Sender: TObject);
    procedure MenuLexerClick(Sender: TObject);
    procedure MenuMainClick(Sender: TObject);
    procedure MsgLogDebug(const AText: string);
    procedure MsgLogToFilename(const AText, AFilename: string; AWithTime: boolean);
    function GetStatusbarPrefix(Frame: TEditorFrame): string;
    procedure MsgStatusFileOpened(const AFileName1, AFileName2: string);
    procedure PopupListboxOutputCopyClick(Sender: TObject);
    procedure PopupListboxOutputClearClick(Sender: TObject);
    procedure PopupListboxValidateClearClick(Sender: TObject);
    procedure PopupListboxValidateCopyClick(Sender: TObject);
    procedure SetShowFloatGroup1(AValue: boolean);
    procedure SetShowFloatGroup2(AValue: boolean);
    procedure SetShowFloatGroup3(AValue: boolean);
    procedure SetShowMenu(AValue: boolean);
    procedure SetShowOnTop(AValue: boolean);
    procedure SetShowSidebarOnRight(AValue: boolean);
    procedure SetSidebarPanel(const ACaption: string);
    procedure SetShowDistractionFree(AValue: boolean);
    procedure SetShowFullScreen(AValue: boolean);
    procedure SetFullScreen_Ex(AValue: boolean; AHideAll: boolean);
    procedure SetFullScreen_Universal(AValue: boolean);
    procedure SetFullScreen_Win32(AValue: boolean);
    procedure SetThemeSyntax(const AValue: string);
    procedure SetThemeUi(const AValue: string);
    function SFindOptionsToTextHint: string;
    procedure DoOps_ShowEventPlugins;
    procedure DoOps_ResetLexerSpecificOptions;
    procedure DoOps_LoadPluginFromInf(const fn_inf: string; IniPlugins: TMemIniFile);
    procedure DoOps_LoadSidebarIcons;
    procedure DoOps_LoadCodetreeIcons;
    procedure DoOps_LoadToolbarIcons;
    procedure DoOps_LoadLexerLib(AOnCreate: boolean);
    procedure DoOps_SaveHistory;
    procedure DoOps_SaveHistory_GroupView(cfg: TJsonConfig);
    procedure DoOps_SaveOptionBool(const APath: string; AValue: boolean);
    procedure DoOps_SaveThemes;
    procedure DoOps_LoadHistory;
    procedure DoOps_LoadHistory_GroupView(cfg: TJsonConfig);
    function DoOps_SaveSession(const AFileName: string): boolean;
    function DoOps_LoadSession(const AFileName: string; AllowShowPanels: boolean): boolean;
    procedure DoOps_LoadOptionsAndApplyAll;
    procedure DoOps_LoadOptionsLexerSpecific(F: TEditorFrame; Ed: TATSynEdit);
    procedure DoOps_OpenFile_LexerSpecific;
    procedure DoOps_LoadPlugins;
    procedure DoOps_DialogFont(var OpName: string; var OpSize: integer; const AConfigStrName, AConfigStrSize: string);
    procedure DoOps_DialogFont_Text;
    procedure DoOps_DialogFont_Ui;
    procedure DoOps_DialogFont_Output;
    procedure DoOps_FontSizeChange(AIncrement: integer);
    procedure DoOps_FontSizeReset;
    procedure DoOps_OpenFile_Default;
    procedure DoOps_OpenFile_User;
    procedure DoOps_OpenFile_DefaultAndUser;
    procedure DoOps_LoadOptions(const fn: string; var Op: TEditorOps;
      AllowUiOps: boolean=true; AllowGlobalOps: boolean=true);
    procedure DoOps_LoadOptionsFromString(const AString: string);
    procedure DoEditorsLock(ALock: boolean);
    procedure DoFindCurrentWordOrSel(Ed: TATSynEdit; ANext, AWordOrSel: boolean);
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
    procedure FrameOnCommand(Sender: TObject; ACommand: integer; const AText: string; var AHandled: boolean);
    function DoFileCloseAll(AWithCancel: boolean): boolean;
    procedure DoDialogFind(AReplaceMode: boolean);
    procedure DoDialogFind_Hide;
    procedure DoFindResult(ok: boolean);
    procedure DoFindResultSimple(ok: boolean);
    procedure DoFindFirst;
    procedure DoFindNext(ANext: boolean);
    procedure DoFindMarkAll(AMode: TATFindMarkingMode);
    procedure DoMoveTabToGroup(AGroupIndex: Integer; AFromCommandPalette: boolean=false);
    function DoFileOpen(AFileName, AFileName2: string; APages: TATPages=nil; const AOptions: string=''): TEditorFrame;
    procedure DoFileOpenDialog(AOptions: string= '');
    procedure DoFileOpenDialog_NoPlugins;
    function DoFileSaveAll: boolean;
    procedure DoFileReopen(Ed: TATSynEdit);
    procedure DoLoadCommandLineBaseOptions(out AWindowPos: string;
      out AAllowSession: boolean; out AFileFolderCount: integer);
    procedure DoLoadCommandParams(const AParams: array of string; AOpenOptions: string);
    procedure DoLoadCommandLine;
    //procedure DoToggleMenu;
    procedure DoToggleFloatSide;
    procedure DoToggleFloatBottom;
    procedure DoToggleOnTop;
    procedure DoToggleFullScreen;
    procedure DoToggleDistractionFree;
    procedure DoToggleSidePanel;
    procedure DoToggleBottomPanel;
    procedure DoToggleFindDialog;
    procedure DoToggleSidebar;
    procedure DoToggleToolbar;
    procedure DoToggleStatusbar;
    procedure DoToggleUiTabs;
    function FinderReplaceAll(Ed: TATSynEdit; AResetCaret: boolean): integer;
    procedure FinderShowReplaceReport(ACounter, ATime: integer);
    procedure FindDialogDone(Sender: TObject; Res: TAppFinderOperation);
    procedure FinderOnFound(Sender: TObject; APos1, APos2: TPoint);
    procedure FinderOnProgress(Sender: TObject; const ACurPos, AMaxPos: Int64; var AContinue: boolean);
    procedure FinderUpdateEditor(AUpdateText: boolean; AUpdateStatusbar: boolean=true);
    procedure FrameOnSaveFile(Sender: TObject);
    procedure GetEditorIndexes(Ed: TATSynEdit; out AGroupIndex, ATabIndex: Integer);
    function GetModifiedCount: integer;
    function GetShowSideBar: boolean;
    function GetShowStatus: boolean;
    function GetShowToolbar: boolean;
    function GetShowTabsMain: boolean;
    procedure InitFormFind;
    function IsFocusedBottom: boolean;
    function IsFocusedFind: boolean;
    procedure PyCompletionOnGetProp(Sender: TObject; out AText: string; out ACharsLeft, ACharsRight: integer);
    procedure PyCompletionOnResult(Sender: TObject; const ASnippetId: string; ASnippetIndex: integer);
    procedure DoPyCommand_ByPluginIndex(AIndex: integer);
    procedure SetFrameEncoding(Ed: TATSynEdit; const AEnc: string; AAlsoReloadFile: boolean);
    procedure SetFrameLexerByIndex(Ed: TATSynEdit; AIndex: integer);
    procedure SetShowStatus(AValue: boolean);
    procedure SetShowToolbar(AValue: boolean);
    procedure SetShowSideBar(AValue: boolean);
    procedure SetShowTabsMain(AValue: boolean);
    procedure SplitterOnPaint_Gr(Sender: TObject);
    procedure SplitterOnPaint_Main(Sender: TObject);
    procedure StopAllTimers;
    procedure UpdateMenuRecents(sub: TMenuItem);
    procedure UpdateSidebarButtonOverlay;
    procedure UpdateEditorTabsize(AValue: integer);
    procedure UpdateMenuItemAltObject(mi: TMenuItem; cmd: integer);
    procedure UpdateMenuItemChecked(mi: TMenuItem; saved: TATMenuItemsAlt; AValue: boolean);
    procedure UpdateMenuItemHint(mi: TMenuItem; const AHint: string);
    procedure UpdateMenuItemHotkey(mi: TMenuItem; cmd: integer);
    procedure UpdateMenuLexersTo(AMenu: TMenuItem);
    procedure UpdateMenuRecent(Ed: TATSynEdit);
    procedure UpdateMenuHotkeys;
    procedure UpdateMenuPlugins;
    procedure UpdateMenuPlugins_Shortcuts(AForceUpdate: boolean=false);
    procedure UpdateMenuPlugins_Shortcuts_Work(AForceUpdate: boolean);
    procedure UpdateMenuChecks;
    procedure UpdateMenuEnc(AMenu: TMenuItem);
    procedure DoApplyUiOps;
    procedure DoApplyUiOpsToGroups(G: TATGroups);
    procedure DoApplyInitialGroupSizes;
    procedure DoApplyInitialSidebarPanel;
    procedure DoApplyInitialWindowPos;
    procedure InitConfirmPanel;
    procedure InitPyEngine;
    procedure FrameOnChangeCaption(Sender: TObject);
    procedure FrameOnUpdateStatus(Sender: TObject);
    function CreateTab(APages: TATPages; const ACaption: string;
      AndActivate: boolean=true;
      AAllowNearCurrent: boolean=true): TATTabData;
    procedure FrameOnFocus(Sender: TObject);
    function GetFrame(AIndex: integer): TEditorFrame;
    procedure SetFrame(Frame: TEditorFrame);
    procedure UpdateFrameLineEnds(Frame: TEditorFrame; AValue: TATLineEnds);
    procedure MsgStatus(AText: string);
    procedure MsgStatusAlt(const AText: string; ASeconds: integer);
    procedure MsgStatusErrorInRegex;
    procedure UpdateStatusbarPanelsFromString(const AText: string);
    procedure UpdateStatusbarHints;
    procedure UpdateStatus_ForFrame(AStatus: TATStatus; F: TEditorFrame);
    procedure UpdateStatus_RealWork;
    procedure UpdateStatus_ToolButton(AToolbar: TATFlatToolbar; ACmd: integer; AChecked, AEnabled: boolean);
    procedure UpdateSideButtonFind;
    procedure UpdateTabCaptionsFromFolders;
    procedure UpdateTabsActiveColor(F: TEditorFrame);
    procedure UpdateTree(AFill: boolean; AConsiderTreeVisible: boolean=true; AForceUpdateAll: boolean=false);
    procedure UpdateTreeContents;
    procedure UpdateTreeSelection(Frame: TEditorFrame; Ed: TATSynEdit);
    procedure UpdateCaption;
    procedure UpdateEnabledAll(b: boolean);
    procedure InitFrameEvents(F: TEditorFrame);
    procedure UpdateInputForm(Form: TForm; AndHeight: boolean= true);
    procedure UpdateFrameEx(F: TEditorFrame; AUpdatedText: boolean);
    procedure UpdateCurrentFrame(AUpdatedText: boolean= false);
    procedure UpdateAppForSearch(AStart, AEdLock, AFindMode: boolean);
    procedure UpdateStatus;
    procedure InitStatusbarControls;
    procedure DoOnDeleteLexer(Sender: TObject; const ALexerName: string);
    procedure UpdateTreeFilter;
  public
    { public declarations }
    CodeTree: TAppTreeContainer;
    CodeTreeFilter: TTreeFilterEdit;
    CodeTreeFilterInput: TATComboEdit;
    CodeTreeFilterReset: TATButton;
    PanelCodeTreeAll: TATPanelSimple;
    PanelCodeTreeTop: TATPanelSimple;
    ListboxOut: TATListbox;
    ListboxVal: TATListbox;
    LexerProgress: TATGauge;
    LexersDetected: TStringList;
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
    property ShowDistractionFree: boolean read FShowFullScreen write SetShowDistractionFree;
    property ShowSideBar: boolean read GetShowSideBar write SetShowSideBar;
    property ShowSideBarOnRight: boolean read GetShowSidebarOnRight write SetShowSidebarOnRight;
    property ShowToolbar: boolean read GetShowToolbar write SetShowToolbar;
    property ShowStatus: boolean read GetShowStatus write SetShowStatus;
    property ShowTabsMain: boolean read GetShowTabsMain write SetShowTabsMain;
    property ThemeUi: string write SetThemeUi;
    property ThemeSyntax: string write SetThemeSyntax;
    function DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: TAppVariantArray): TAppPyEventResult;
    procedure DoPyCommand(const AModule, AMethod: string; const AParams: TAppVariantArray);
    function DoPyTreeHelper(Frame: TEditorFrame): boolean;
    function DoPyLexerDetection(const Filename: string; Lexers: TStringList): integer;
    procedure FinderOnGetToken(Sender: TObject; AX, AY: integer; out AKind: TATTokenKind);
    procedure FinderOnConfirmReplace(Sender: TObject; APos1, APos2: TPoint;
      AForMany: boolean; var AConfirm, AContinue: boolean; var AReplacement: UnicodeString);
    procedure FinderOnConfirmReplace_API(Sender: TObject; APos1, APos2: TPoint;
      AForMany: boolean; var AConfirm, AContinue: boolean; var AReplacement: UnicodeString);
  end;

var
  fmMain: TfmMain;

var
  NTickInitial: QWord = 0;

procedure DoOnLexerLoaded(Sender: TObject; ALexer: TecSyntAnalyzer);


implementation

uses
  Emmet,
  EmmetHelper;

{$R *.lfm}

var
  PythonEng: TPythonEngine = nil;
  PythonModule: TPythonModule = nil;
  PythonIO: TPythonInputOutput = nil;

const
  cThreadSleepTime = 50;
  cThreadSleepCount = 20;
  //SleepTime*SleepCount ~= 1 sec

const
  cAppSessionDefault = 'history session.json';

const
  StatusbarTag_Caret = 10;
  StatusbarTag_Enc = 11;
  StatusbarTag_LineEnds = 12;
  StatusbarTag_Lexer = 13;
  StatusbarTag_TabSize = 14;
  StatusbarTag_InsOvr = 15;
  StatusbarTag_SelMode = 16;
  StatusbarTag_WrapMode = 17;
  StatusbarTag_Msg = 20;

const
  BadPlugins: array[0..1] of string = (
    'cuda_tree',
    'cuda_brackets_hilite'
    );

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

  NColor:= GetAppColor(apclStatusFont);
  if NColor<>clNone then
    AppThemeStatusbar.ColorFont:= NColor;
end;

procedure InitUniqueInstanceObject;
begin
  {$ifdef unix}
  if Assigned(AppUniqInst) then exit;
  AppUniqInst:= TUniqueInstance.Create(nil);
  AppUniqInst.Identifier:= AppServerId;
  AppUniqInst.OnOtherInstance:= @fmMain.UniqInstanceOtherInstance;
  {$endif}
end;

function GetEditorFrame(Ed: TATSynEdit): TEditorFrame; inline;
begin
  if Assigned(Ed) and (Ed.Parent is TEditorFrame) then
    Result:= TEditorFrame(Ed.Parent)
  else
    Result:= nil;
end;

function GetEditorBrother(Ed: TATSynEdit): TATSynEdit;
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

function GetEditorFirstSecond(Ed: TATSynEdit; AFirst: boolean): TATSynEdit;
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


function GetPagesOfGroupIndex(AIndex: integer): TATPages;
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

function GetEditorActiveInGroup(AIndex: integer): TATSynEdit;
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


procedure AppCommandPut(Ed: TATSynEdit; ACommand: integer; AForceTimer: boolean);
var
  Frame: TEditorFrame;
  Item: TAppCommandDelayed;
  D: TATTabData;
  N: integer;
begin
  Item.Code:= ACommand;
  Item.EdAddress:= Ed;
  Item.EdIndex:= 0;
  Item.Tabs:= nil;
  Item.TabIndex:= -1;

  Frame:= GetEditorFrame(Ed);
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


type
  TAppCommandGetStatus = (acgNoCommands, acgBadCommand, acgOkCommand);

function AppCommandGet(out AEditor: TATSynEdit; out ACommand: integer): TAppCommandGetStatus;
var
  Item: TAppCommandDelayed;
  TabData: TATTabData;
  Frame: TEditorFrame;
  EdTemp: TATSynEdit;
begin
  AEditor:= nil;
  ACommand:= 0;
  if AppCommandsDelayed.IsEmpty() then
    exit(acgNoCommands);

  Result:= acgBadCommand;
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
  Result:= acgOkCommand;
end;

procedure Keymap_UpdateDynamicEx(AKeymap: TATKeymap; ACategory: TAppCommandCategory);
var
  MapItem: TATKeymapItem;
  CmdItem: TAppCommandInfo;
  Frame: TEditorFrame;
  An: TecSyntAnalyzer;
  KeysBackup: TAppHotkeyBackup;
  sl: TStringList;
  Cmd, i: integer;
  NPluginIndex: integer;
begin
  KeysBackup:= TAppHotkeyBackup.Create;

  for i:= AKeymap.Count-1 downto 0 do
  begin
    MapItem:= AKeymap[i];
    Cmd:= MapItem.Command;
    if Cmd<cmdFirstAppCommand then Break;
    if AppCommandCategory(Cmd)=ACategory then
    begin
      //backup hotkeys of plugins
      //this function must not loose any hotkeys!
      if ACategory in [categ_Plugin, categ_PluginSub] then
      begin
        NPluginIndex:= Cmd-cmdFirstPluginCommand;
        CmdItem:= TAppCommandInfo(AppCommandList[NPluginIndex]);
        KeysBackup.Add(MapItem, CmdItem.CommaStr);
      end;

      AKeymap.Delete(i);
    end;
  end;

  case ACategory of
    categ_Lexer:
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

    categ_Plugin:
      for i:= 0 to AppCommandList.Count-1 do
      begin
        CmdItem:= TAppCommandInfo(AppCommandList[i]);
        if CmdItem.ItemFromApi then Continue;
        if CmdItem.ItemModule='' then Break;
        if SEndsWith(CmdItem.ItemCaption, '-') then Continue;

        AKeymap.Add(
          cmdFirstPluginCommand+i,
          'plugin: '+AppNicePluginCaption(CmdItem.ItemCaption),
          [], []);

        KeysBackup.Get(AKeymap[AKeymap.Count-1], CmdItem.CommaStr);
      end;

    categ_PluginSub:
      for i:= 0 to AppCommandList.Count-1 do
      begin
        CmdItem:= TAppCommandInfo(AppCommandList[i]);
        if not CmdItem.ItemFromApi then Continue;
        if CmdItem.ItemModule='' then Break;
        if SEndsWith(CmdItem.ItemCaption, '-') then Continue;

        AKeymap.Add(
          cmdFirstPluginCommand+i,
          'plugin: '+AppNicePluginCaption(CmdItem.ItemCaption),
          [], []);

        KeysBackup.Get(AKeymap[AKeymap.Count-1], CmdItem.CommaStr);
      end;

    categ_OpenedFile:
      for i:= 0 to AppFrameList1.Count-1 do
      begin
        Frame:= TEditorFrame(AppFrameList1[i]);
        if Frame.FileName<>'' then
          AKeymap.Add(
            cmdFirstFileCommand+i,
            'opened file: '+FormatFilenameForMenu(Frame.FileName),
            [], []);
      end;

    categ_RecentFile:
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


procedure Keymap_UpdateDynamic(ACategory: TAppCommandCategory);
var
  Map: TATKeymap;
  i: integer;
begin
  Keymap_UpdateDynamicEx(AppKeymapMain, ACategory);

  for i:= 0 to AppKeymapLexers.Count-1 do
  begin
    Map:= TATKeymap(AppKeymapLexers.Objects[i]);
    Keymap_UpdateDynamicEx(Map, ACategory);
  end;
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

procedure TAppNotifThread.HandleOneFrame;
var
  NewProps: TAppFileProps;
begin
  AppGetFileProps(CurFrame.FileName, NewProps);
  if not CurFrame.FileProps.Inited then
  begin
    Move(NewProps, CurFrame.FileProps, SizeOf(NewProps));
  end
  else
  if NewProps<>CurFrame.FileProps then
  begin
    Move(NewProps, CurFrame.FileProps, SizeOf(NewProps));
    Synchronize(@NotifyFrame1);
  end;

  if not CurFrame.EditorsLinked then
    if CurFrame.FileName2<>'' then
    begin
      AppGetFileProps(CurFrame.FileName2, NewProps);
      if not CurFrame.FileProps2.Inited then
      begin
        Move(NewProps, CurFrame.FileProps2, SizeOf(NewProps));
      end
      else
      if NewProps<>CurFrame.FileProps2 then
      begin
        Move(NewProps, CurFrame.FileProps2, SizeOf(NewProps));
        Synchronize(@NotifyFrame2);
      end;
    end;
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
        if not CurFrame.IsText then Continue;
        HandleOneFrame;
      end;
    finally
      AppEventWatcher.SetEvent;
    end;
  until false;
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
  if not F.IsBinary then exit;
  F.Binary.Mode:= TATBinHexMode((Sender as TComponent).Tag);
  UpdateStatus;
end;

procedure TfmMain.InitPopupListboxOutput;
begin
  if PopupListboxOutput=nil then
  begin
    PopupListboxOutput:= TPopupMenu.Create(Self);

    mnuContextOutputClear:= TMenuItem.Create(Self);
    mnuContextOutputClear.OnClick:= @PopupListboxOutputClearClick;
    PopupListboxOutput.Items.Add(mnuContextOutputClear);

    mnuContextOutputCopy:= TMenuItem.Create(Self);
    mnuContextOutputCopy.OnClick:= @PopupListboxOutputCopyClick;
    PopupListboxOutput.Items.Add(mnuContextOutputCopy);
  end;
end;

procedure TfmMain.InitPopupListboxValidate;
begin
  if PopupListboxValidate=nil then
  begin
    PopupListboxValidate:= TPopupMenu.Create(Self);

    mnuContextValidateClear:= TMenuItem.Create(Self);
    mnuContextValidateClear.OnClick:= @PopupListboxValidateClearClick;
    PopupListboxValidate.Items.Add(mnuContextValidateClear);

    mnuContextValidateCopy:= TMenuItem.Create(Self);
    mnuContextValidateCopy.OnClick:= @PopupListboxValidateCopyClick;
    PopupListboxValidate.Items.Add(mnuContextValidateCopy);
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

procedure TfmMain.InitPopupEnc;
begin
  if PopupEnc=nil then
  begin
    PopupEnc:= TPopupMenu.Create(Self);
  end;
  UpdateMenuEnc(PopupEnc.Items);
end;

procedure TfmMain.InitPopupEnds;
begin
  if PopupEnds=nil then
  begin
    PopupEnds:= TPopupMenu.Create(Self);
    mnuEndsWin:= TMenuItem.Create(Self);
    mnuEndsUnix:= TMenuItem.Create(Self);
    mnuEndsMac:= TMenuItem.Create(Self);

    UpdateMenuItemHotkey(mnuEndsWin, cmd_LineEndWin);
    UpdateMenuItemHotkey(mnuEndsUnix, cmd_LineEndUnix);
    UpdateMenuItemHotkey(mnuEndsMac, cmd_LineEndMac);

    PopupEnds.Items.Add(mnuEndsWin);
    PopupEnds.Items.Add(mnuEndsUnix);
    PopupEnds.Items.Add(mnuEndsMac);
  end;

  mnuEndsWin.Caption:= msgEndWin;
  mnuEndsUnix.Caption:= msgEndUnix;
  mnuEndsMac.Caption:= msgEndMac;
end;

procedure TfmMain.InitPopupLex;
begin
  if PopupLex=nil then
  begin
    PopupLex:= TPopupMenu.Create(Self);
  end;
  UpdateMenuLexersTo(PopupLex.Items);
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
  Data: TATStatusData;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;

  Data:= Status.GetPanelData(AIndex);
  if Data=nil then exit;

  if Frame.IsPicture then
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

  if Frame.IsBinary then
  begin
    case Data.Tag of
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
    end;
    exit;
  end;

  case Data.Tag of
    StatusbarTag_Caret:
      begin
        DoDialogGoto;
      end;
    StatusbarTag_Enc:
      begin
        if not Frame.ReadOnly[Frame.Editor] then
        begin
          InitPopupEnc;
          PopupEnc.PopUp;
        end;
      end;
    StatusbarTag_LineEnds:
      begin
        if not Frame.ReadOnly[Frame.Editor] then
        begin
          InitPopupEnds;
          PopupEnds.PopUp;
        end;
      end;
    StatusbarTag_Lexer:
      begin
        InitPopupLex;
        PopupLex.PopUp;
      end;
    StatusbarTag_TabSize:
      begin
        InitPopupTabSize;
        PopupTabSize.Popup;
      end;
    StatusbarTag_SelMode:
      begin
        with Frame.Editor do
        begin
          OptMouseColumnSelectionWithoutKey:= not OptMouseColumnSelectionWithoutKey;
          UpdateStatus;
        end;
      end;
    StatusbarTag_WrapMode:
      begin
        //loop: no wrap - wrap at window - wrap at margin
        with Frame.Editor do
        begin
          if OptWrapMode=High(OptWrapMode) then
            OptWrapMode:= Low(OptWrapMode)
          else
            OptWrapMode:= Succ(OptWrapMode);
          UpdateStatus;
        end;
      end;
  end;
end;

procedure TfmMain.TimerAppIdleTimer(Sender: TObject);
var
  PntScreen, PntLocal: TPoint;
  Ed: TATSynEdit;
  S: UnicodeString;
  NCnt, i: integer;
  Params: TAppVariantArray;
begin
  //in Lazarus 2.1 trunk on Linux x64 gtk2/qt5, TimerAppIdle.Timer is called too early,
  //when Handle is not created
  if not HandleAllocated then
  begin
    //debug
    exit;
  end;

  //flush saved Python "print" results to console
  if not AppConsoleQueue.IsEmpty() then
  begin
    //avoid output of huge items count at once
    NCnt:= Min(AppConsoleQueue.Size, 300);
    for i:= 1 to NCnt do
    begin
      S:= AppConsoleQueue.Front();
      AppConsoleQueue.Pop();
      fmConsole.DoAddLine(S);
      if UiOps.LogConsole then
        MsgLogToFilename(S, FFileNameLogConsole, false);
    end;

    fmConsole.DoUpdateMemo;
  end;

  //call API event on_mouse_stop
  PntScreen:= Mouse.CursorPos;
  if PntScreen<>FLastMousePos then
  begin
    FLastMousePos:= PntScreen;
    for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
    begin
      Ed:= GetEditorActiveInGroup(i);
      if Ed=nil then Break;
      PntLocal:= Ed.ScreenToClient(PntScreen);
      if PtInRect(Ed.ClientRect, PntLocal) then
      begin
        SetLength(Params, 2);
        Params[0]:= AppVariant(PntLocal.X);
        Params[1]:= AppVariant(PntLocal.Y);
        DoPyEvent(Ed, cEventOnMouseStop, Params);
        Break;
      end;
    end;
  end;

  AppUpdateWatcherFrames;

  if FCodetreeNeedsSelJump then
  begin
    FCodetreeNeedsSelJump:= false;
    UpdateTree(false);
  end;

  if FInvalidateShortcuts then
  begin
    FInvalidateShortcuts:= false;
    UpdateMenuPlugins_Shortcuts_Work(FInvalidateShortcutsForce);
  end;
end;

procedure TfmMain.TimerStatusClearTimer(Sender: TObject);
begin
  MsgStatus('');
  TimerStatusClear.Enabled:= false;
end;

procedure TfmMain.TimerStatusAltTimer(Sender: TObject);
begin
  DoHideFuncHint;
end;

procedure TfmMain.TimerStatusWorkTimer(Sender: TObject);
begin
  TimerStatusWork.Enabled:= false;
  UpdateStatus_RealWork;
end;

procedure TfmMain.TimerTreeFillTimer(Sender: TObject);
begin
  TimerTreeFill.Enabled:= false;
  UpdateTree(true);
end;

procedure TfmMain.DoCodetree_OnDblClick(Sender: TObject);
var
  Ed: TATSynEdit;
  PntBegin, PntEnd: TPoint;
begin
  DoCodetree_GetSyntaxRange(CodeTree.Tree.Selected, PntBegin, PntEnd);

  Ed:= CurrentEditor;
  FCodetreeDblClicking:= true;
  Ed.DoGotoPos(
    PntBegin,
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );
  DoFocusEditor(Ed);
  FCodetreeDblClicking:= false;
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
  if not (DataObj is TATRangeInCodeTree) then exit;
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

function TfmMain.GetSessionFilename: string;
begin
  Result:= AppSessionName;
  if Result='' then
    Result:= cAppSessionDefault;
  if ExtractFileDir(Result)='' then
    Result:= AppDir_Settings+DirectorySeparator+Result;
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


procedure TfmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  OnEnter:= @FormEnter;

  (*
  mnuHelpCheckUpd.Enabled:=
    {$if defined(windows) or defined(linux) or defined(darwin)}
    true
    {$else}
    false
    {$endif};
    *)

  with AppPanels[cPaneSide] do
  begin
    PanelRoot:= Self.PanelMain;
    Toolbar:= ToolbarSideTop;
    DefaultPanel:= msgPanelTree_Init;
    OnCommand:= @DoSidebar_OnPythonCall;
    OnCloseFloatForm:= @DoSidebar_OnCloseFloatForm;
    OnGetTranslatedTitle:= @DoSidebar_GetFormTitle;
    Init(Self, alLeft);
    Splitter.OnPaint:= @SplitterOnPaint_Main;
  end;

  with AppPanels[cPaneOut] do
  begin
    PanelRoot:= Self.PanelAll;
    Toolbar:= ToolbarSideLow;
    OnHide:= @DoBottom_OnHide;
    OnCommand:= @DoSidebar_OnPythonCall;
    OnCloseFloatForm:= @DoBottom_OnCloseFloatForm;
    OnGetTranslatedTitle:= @DoSidebar_GetFormTitle;
    Init(Self, alBottom);
    Splitter.OnPaint:= @SplitterOnPaint_Main;
  end;

  LexerProgress:= TATGauge.Create(Self);
  LexerProgress.Parent:= Status;

  OnLexerParseProgress:= @DoOnLexerParseProgress;
  CustomDialog_DoPyCallback:= @DoPyCallbackFromAPI;
  FFileNameLogDebug:= AppDir_Settings+DirectorySeparator+'app.log';
  FFileNameLogConsole:= AppDir_Settings+DirectorySeparator+'console.log';

  DoMenuitemEllipsis(mnuOpThemeUi);
  DoMenuitemEllipsis(mnuOpThemeSyntax);
  DoMenuitemEllipsis(mnuOpKeys);
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
  AppPanels[cPaneSide].FormFloatBounds:= Rect(650, 50, 900, 700);
  AppPanels[cPaneOut].FormFloatBounds:= Rect(50, 480, 900, 700);
  FBoundsFloatGroups1:= Rect(300, 100, 800, 700);
  FBoundsFloatGroups2:= Rect(320, 120, 820, 720);
  FBoundsFloatGroups3:= Rect(340, 140, 840, 740);

  InitAppleMenu;
  InitToolbar;

  PanelCodeTreeAll:= TATPanelSimple.Create(Self);
  PanelCodeTreeAll.OnEnter:= @DoCodetree_PanelOnEnter;
  PanelCodeTreeAll.Focusable:= true;

  CodeTree:= TAppTreeContainer.Create(Self);
  CodeTree.Parent:= PanelCodeTreeAll;
  CodeTree.Align:= alClient;
  CodeTree.Themed:= true;
  CodeTree.Tree.OnDblClick:= @DoCodetree_OnDblClick;
  CodeTree.Tree.OnMouseMove:= @DoCodetree_OnMouseMove;
  CodeTree.Tree.OnKeyDown:= @DoCodetree_OnKeyDown;
  CodeTree.Tree.OnContextPopup:= @DoCodetree_OnContextPopup;

  PanelCodeTreeTop:= TATPanelSimple.Create(Self);
  PanelCodeTreeTop.Parent:= PanelCodeTreeAll;
  PanelCodeTreeTop.Align:= alTop;
  PanelCodeTreeTop.Height:= UiOps.InputHeight;

  CodeTreeFilter:= TTreeFilterEdit.Create(Self);
  CodeTreeFilter.Hide;

  CodeTreeFilterReset:= TATButton.Create(Self);
  CodeTreeFilterReset.Parent:= PanelCodeTreeTop;
  CodeTreeFilterReset.Align:= alRight;
  CodeTreeFilterReset.Width:= UiOps.ScrollbarWidth;
  CodeTreeFilterReset.Caption:= '';
  CodeTreeFilterReset.Arrow:= true;
  CodeTreeFilterReset.ArrowKind:= abakCross;
  CodeTreeFilterReset.Focusable:= false;
  CodeTreeFilterReset.ShowHint:= true;
  CodeTreeFilterReset.Hint:= msgTooltipClearFilter;
  CodeTreeFilterReset.OnClick:= @CodeTreeFilter_ResetOnClick;

  CodeTreeFilterInput:= TATComboEdit.Create(Self);
  CodeTreeFilterInput.Parent:= PanelCodeTreeTop;
  CodeTreeFilterInput.Align:= alClient;
  CodeTreeFilterInput.OnChange:= @CodeTreeFilter_OnChange;
  CodeTreeFilterInput.OnCommand:= @CodeTreeFilter_OnCommand;

  ListboxOut:= TATListbox.Create(Self);
  ListboxOut.VirtualMode:= false;
  ListboxOut.CanGetFocus:= true;
  ListboxOut.OwnerDrawn:= true;
  ListboxOut.ScrollStyleVert:= alssShow;
  ListboxOut.ScrollStyleHorz:= alssAuto;
  ListboxOut.OnDblClick:= @ListboxOutClick;
  ListboxOut.OnDrawItem:= @ListboxOutDrawItem;
  ListboxOut.OnKeyDown:= @ListboxOutKeyDown;
  ListboxOut.OnContextPopup:= @ListboxOutContextPopup;

  ListboxVal:= TATListbox.Create(Self);
  ListboxVal.VirtualMode:= false;
  ListboxVal.CanGetFocus:= true;
  ListboxVal.OwnerDrawn:= true;
  ListboxVal.ScrollStyleVert:= alssShow;
  ListboxVal.ScrollStyleHorz:= alssAuto;
  ListboxVal.OnDblClick:= @ListboxOutClick;
  ListboxVal.OnDrawItem:= @ListboxOutDrawItem;
  ListboxVal.OnKeyDown:= @ListboxOutKeyDown;
  ListboxVal.OnContextPopup:= @ListboxValidateContextPopup;

  fmConsole:= TfmConsole.Create(Self);
  fmConsole.OnConsoleNav:= @DoOnConsoleNav;
  fmConsole.OnNumberChange:= @DoOnConsoleNumberChange;

  InitSidebar; //after initing PanelCodeTreeAll, ListboxOut, ListboxVal, fmConsole

  AppBookmarkImagelist.AddImages(ImageListBm);
  for i:= 2 to 9 do
  begin
    AppBookmarkSetup[i].Color:= clDefault;
    AppBookmarkSetup[i].ImageIndex:= i-1;
  end;

  AppManager:= TecLexerList.Create(Self);
  AppManagerLite:= TATLiteLexers.Create(Self);
  AppManagerLite.OnGetStyleHash:= @LiteLexer_GetStyleHash;
  AppManagerLite.OnApplyStyle:= @LiteLexer_ApplyStyle;

  FMenuVisible:= true;
  AppSessionName:= '';
  FListTimers:= TStringList.Create;

  FillChar(AppPanelProp_Out, SizeOf(AppPanelProp_Out), 0);
  FillChar(AppPanelProp_Val, SizeOf(AppPanelProp_Val), 0);
  AppPanelProp_Out.Listbox:= ListboxOut;
  AppPanelProp_Val.Listbox:= ListboxVal;

  Status:= TATStatus.Create(Self);
  Status.Parent:= Self;
  Status.Align:= alBottom;
  Status.Top:= Height;
  Status.Padding:= 2;
  Status.ScaleFromFont:= true; //statusbar is autosized via its font size
  Status.OnPanelClick:= @StatusPanelClick;
  Status.ShowHint:= true;
  Status.Theme:= @AppThemeStatusbar;

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

  FFinder:= TATEditorFinder.Create;
  FFinder.OnConfirmReplace:= @FinderOnConfirmReplace;
  FFinder.OnProgress:= @FinderOnProgress;
  FFinder.OnFound:=@FinderOnFound;
  FFinder.OnGetToken:= @FinderOnGetToken;

  InitStatusbarControls;

  FFindStop:= false;
  FFindConfirmAll:= mrNone;

  Groups.Splitter1.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter2.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter3.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter4.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter5.OnPaint:= @SplitterOnPaint_Gr;

  FLastDirOfOpenDlg:= '';
  FLastLexerForPluginsMenu:= '-';

  UpdateMenuItemHint(mnuFile, 'top-file');
  UpdateMenuItemHint(mnuEdit, 'top-edit');
  UpdateMenuItemHint(mnuSel, 'top-sel');
  UpdateMenuItemHint(mnuSr, 'top-sr');
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

procedure TfmMain.DoOps_OnCreate;
begin
  //must load window position in OnCreate to fix flickering with maximized window, Win10
  DoLoadCommandLineBaseOptions(FOption_WindowPos, FOption_AllowSession, FCmdlineFileCount);
  DoOps_LoadOptions(AppFile_OptionsUser, EditorOps); //before LoadHistory
  DoOps_LoadLexerLib(true); //before LoadHistory
  DoFileOpen('', '', nil, '/nolexernewdoc'); //before LoadHistory

  DoOps_LoadToolbarIcons;
  DoOps_LoadSidebarIcons; //before LoadPlugins (for sidebar icons)

  InitPyEngine; //before LoadPlugins
  DoOps_LoadPlugins; //before LoadHistory (for on_open for restored session)

  DoOps_LoadHistory;
end;

procedure TfmMain.DoCloseAllTabs;
var
  Tabs: TATTabs;
  nGroup, nTab: integer;
begin
  for nGroup:= High(TATGroupsNums) to Low(TATGroupsNums) do
  begin
    Tabs:= Groups.Pages[nGroup].Tabs;
    for nTab:= Tabs.TabCount-1 downto 0 do
      Tabs.DeleteTab(nTab, true{AllowEvent}, false{AWithCancelBtn});
  end;
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  F: TEditorFrame;
  Params: TAppVariantArray;
  i: integer;
begin
  if Assigned(AppNotifThread) then
  begin
    AppNotifThread.Terminate;
    Sleep(cThreadSleepTime+10);
  end;

  //maybe no need too? done in DoCloseAllTabs
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    UpdateMenuRecent(F.Ed1);
    if not F.EditorsLinked then
      UpdateMenuRecent(F.Ed2);
  end;

  //after UpdateMenuRecent
  DoOps_SaveHistory;

  (*
  //no need, done via DoCloseAllTabs
  for i:= FrameCount-1 downto 0 do
  begin
    F:= Frames[i];
    //make sure adapters don't block closing
    F.Editor.AdapterForHilite:= nil;
    F.Editor2.AdapterForHilite:= nil;
    F.Adapter.Stop;
  end;
  *)

  DoCloseAllTabs;
  SetLength(Params, 0);
  DoPyEvent(nil, cEventOnExit, Params);
end;

procedure TfmMain.ButtonCancelClick(Sender: TObject);
begin
  FFindStop:= true;
end;

procedure TfmMain.FormActivate(Sender: TObject);
begin
  AppActiveForm:= Sender;
end;

procedure TfmMain.AppPropsActivate(Sender: TObject);
var
  F: TEditorFrame;
begin
  if EditorOps.OpShowCurLineOnlyFocused then
  begin
    F:= CurrentFrame;
    if F=nil then exit;
    F.Editor.Update;
  end;
end;

procedure TfmMain.AppPropsEndSession(Sender: TObject);
begin
  //
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
  Params: TAppVariantArray;
  i: integer;
begin
  //call on_close_pre for all tabs, it's needed to save all
  //tabs by AutoSave plugin
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    SetLength(Params, 0);
    DoPyEvent(F.Editor, cEventOnCloseBefore, Params);
  end;

  if GetModifiedCount>0 then
    ACanClose:= (
      UiOps.ReopenSession and
      UiOps.AutoSaveSession and
      UiOps.HistoryItems[ahhText]
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
  TimerStatusAlt.Enabled:= false;
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
      NCount:= ListboxVal.Items.Count;
      if NCount>0 then
        Btn.TextOverlay:= IntToStr(NCount)
      else
        Btn.TextOverlay:= '';
    end
    else
    if Btn.Caption=msgPanelOutput_Init then
    begin
      NCount:= ListboxOut.Items.Count;
      if NCount>0 then
        Btn.TextOverlay:= IntToStr(NCount)
      else
        Btn.TextOverlay:= '';
    end
    else
    if Btn.Caption=msgPanelConsole_Init then
    begin
      NCount:= fmConsole.ErrorCounter;
      if NCount>0 then
        Btn.TextOverlay:= IntToStr(NCount)
      else
        Btn.TextOverlay:= '';
    end;
  end;
end;

procedure TfmMain.FormColorsApply(const AColors: TAppTheme);
begin
  AppTheme:= AColors;
  DoClearLexersAskedList;
  DoApplyTheme;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to FListTimers.Count-1 do
    TTimer(FListTimers.Objects[i]).Enabled:= false;
  FreeAndNil(FListTimers);
end;

procedure TfmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  SName: string;
  Pages: TATPages;
  i: integer;
begin
  //support mac: it drops file too early
  //(dbl-click on file in Finder)
  if not FHandledOnShowPartly then
  begin
    SetLength(FFileNamesDroppedInitially, Length(FileNames));
    for i:= 0 to Length(FileNames)-1 do
      FFileNamesDroppedInitially[i]:= FileNames[i];
    exit;
  end;

  if not IsAllowedToOpenFileNow then exit;

  //MS WordPad, Notepad++ - they get focus on drag-drop from Explorer
  Application.BringToFront;

  //set group according to mouse cursor
  Pages:= nil;
  for i in [Low(TATGroupsNums)..High(TATGroupsNums)] do
    if fmMain.Groups.Pages[i].Visible then
      if PtInControl(fmMain.Groups.Pages[i], Mouse.CursorPos) then
      begin
        Pages:= fmMain.Groups.Pages[i];
        Break;
      end;

  for i:= 0 to Length(Filenames)-1 do
  begin
    SName:= FileNames[i];
    if DirectoryExistsUTF8(SName) then
      DoFolderOpen(SName, False)
    else
    if FileExistsUTF8(SName) then
      DoFileOpen(SName, '', Pages);
  end;
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
    if DirectoryExistsUTF8(SName) then
      DoFolderOpen(SName, False)
    else
    if FileExistsUTF8(SName) then
      DoFileOpen(SName, '', Gr.Pages[0]);
  end;
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  bEditorActive,
  bConsoleActive: boolean;
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    PyEscapeFlag:= true;
    if AppPython.IsRunning then
    begin
      Key:= 0;
      exit
    end;

    bEditorActive:=
      (ActiveControl is TATSynEdit) and
      (ActiveControl.Parent is TEditorFrame);
    bConsoleActive:=
      fmConsole.EdInput.Focused or
      fmConsole.EdMemo.Focused;

    DoHideFuncHint;

    if not bEditorActive then
    begin
      DoFocusEditor(CurrentEditor);

      if bConsoleActive then
        if UiOps.EscapeCloseConsole then
          AppPanels[cPaneOut].Visible:= false;
      Key:= 0;
    end
    else
    if UiOps.EscapeClose then
    begin
      Close;
      Key:= 0;
    end;

    exit
  end;
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  FixMainLayout;
end;

procedure TfmMain.FixMainLayout;
begin
  //issue #1814
  AppPanels[cPaneSide].UpdateSplitter;
  AppPanels[cPaneOut].UpdateSplitter;
end;

procedure TfmMain.FormShow(Sender: TObject);
  //
  procedure _Init_FixSplitters;
  {$ifdef darwin}
  var
    id: TAppPanelId;
  {$endif}
  begin
    {$ifdef darwin}
    // https://bugs.freepascal.org/view.php?id=35599
    for id:= Low(id) to High(id) do
      if id<>cPaneNone then
        with AppPanels[id] do
          Splitter.ResizeStyle:= rsUpdate;

    Groups.Splitter1.ResizeStyle:= rsUpdate;
    Groups.Splitter2.ResizeStyle:= rsUpdate;
    Groups.Splitter3.ResizeStyle:= rsUpdate;
    Groups.Splitter4.ResizeStyle:= rsUpdate;
    Groups.Splitter5.ResizeStyle:= rsUpdate;
    {$endif}
  end;
  //
  procedure _Init_WindowMaximized;
  begin
    if FLastMaximized then
    begin
      FLastMaximized:= false;
      if (FLastMaximizedMonitor>=0) and (FLastMaximizedMonitor<Screen.MonitorCount) then
        BoundsRect:= Screen.Monitors[FLastMaximizedMonitor].BoundsRect;
      WindowState:= wsMaximized;
    end;
  end;
  //
  procedure _Init_ApiOnStart;
  var
    Params: TAppVariantArray;
  begin
    SetLength(Params, 0);
    DoPyEvent(nil, cEventOnStart, Params);
  end;
  //
  procedure _Init_KeymapMain;
  begin
    //load keymap-main
    //after loading plugins (to apply plugins keys)
    Keymap_SetHotkey(AppKeymapMain, 'cuda_comments,cmt_toggle_line_body|Ctrl+/|', false);
    Keymap_LoadConfig(AppKeymapMain, AppFile_Hotkeys, false);
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
    if UiOps.ReopenSession and FOption_AllowSession then
      DoOps_LoadSession(GetSessionFilename, false);
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
  procedure _Init_FramesOnShow;
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
  //
  procedure _Init_ShowStartupTimes;
  var
    NTick: QWord;
  begin
    NTick:= GetTickCount64;
    MsgLogConsole(Format(
      'Startup: %dms, plugins: %s', [
      (NTick-NTickInitial) div 10 * 10,
      AppPython.GetTimingReport
      ]));
  end;
  //
begin
  _Init_FixSplitters;

  if FHandledOnShowPartly then exit;
  FormLock(Self);

  _Init_WindowMaximized;

  DoApplyInitialGroupSizes;
  DoApplyFont_Text;
  DoApplyFont_Ui;
  DoApplyFont_Output;

  if FConsoleMustShow then
    DoShowConsole(false);

  FHandledOnShowPartly:= true;

  _Init_ApiOnStart;
  _Init_KeymapMain;
  _Init_KeymapNoneForEmpty;
  _Init_StartupSession;

  //after on_start, ConfigToolbar is slow with visible toolbar
  DoApplyUiOps;
  DoApplyInitialSidebarPanel;

  UpdateMenuPlugins;

  //after loading keymap-main
  UpdateMenuPlugins_Shortcuts(true);
  UpdateMenuHotkeys;

  AppPanels[cPaneSide].UpdateButtons;
  AppPanels[cPaneOut].UpdateButtons;
  UpdateStatus;
  DoLoadCommandLine;
  DoApplyInitialWindowPos;

  if AppPanels[cPaneOut].Visible then
    if AppPanels[cPaneOut].LastActivePanel='' then
      DoShowConsole(false);

  //postpone parsing until frames are shown
  AllowFrameParsing:= true;
  _Init_FramesOnShow;

  FHandledUntilFirstFocus:= true;
  FormUnlock(Self);

  _Init_FrameFocus;
  _Init_ShowStartupTimes;

  AppPython.DisableTiming;
  DoShowFirstStartInfo;

  if UiOps.NotificationEnabled then
  begin
    AppNotifThread:= TAppNotifThread.Create(false);
    AppNotifThread.Priority:= tpLower;
  end;

  FHandledOnShowFully:= true;
end;

procedure TfmMain.DoShowFirstStartInfo;
var
  Frame: TEditorFrame;
begin
  if not FileExists(AppFile_History) then
  begin
    Frame:= Frames[0];
    if Frame.IsEmpty then
    begin
      Frame.TabCaption:= '(welcome)';
      Frame.Ed1.Strings.LoadFromString(msgFirstStartInfo);
    end;
  end;
end;

procedure TfmMain.FrameAddRecent(Sender: TObject);
begin
  UpdateMenuRecent(Sender as TATSynEdit);
end;

procedure TfmMain.FrameOnChangeCaretPos(Sender: TObject);
begin
  if FCodetreeDblClicking then exit;
  FCodetreeNeedsSelJump:= true;
end;

procedure TfmMain.FrameOnMsgStatus(Sender: TObject; const AStr: string);
begin
  MsgStatus(AStr);
end;

procedure TfmMain.MenuRecentsClear(Sender: TObject);
begin
  DoClearRecentFileHistory;
end;

procedure TfmMain.DoClearRecentFileHistory;
begin
  AppListRecents.Clear;
  UpdateMenuRecent(nil);
  //
  DeleteFileUTF8(AppFile_HistoryFiles);
end;

function TfmMain.DoFileInstallZip(const fn: string; out DirTarget: string;
  ASilent: boolean): boolean;
var
  msg, msg2: string;
  AddonType: TAppAddonType;
  bFileLexer: boolean;
  bNeedRestart: boolean;
  ListBackup: TStringlist;
begin
  bNeedRestart:= false;
  bFileLexer:= true;
  if bFileLexer then
  begin
    ListBackup:= TStringList.Create;
    DoOps_LexersDisableInFrames(ListBackup);
  end;

  DoInstallAddonFromZip(fn, AppDir_DataAutocomplete, msg, msg2,
    Result, AddonType, DirTarget, bNeedRestart, ASilent);

  if bFileLexer then
  begin
    DoOps_LexersRestoreInFrames(ListBackup);
    FreeAndNil(ListBackup);
  end;

  if Result then
  begin
    if AddonType in [cAddonTypeLexer, cAddonTypeLexerLite] then
    begin
      DoOps_LoadLexerLib(false);
    end;

    if AddonType=cAddonTypePlugin then
    begin
      DoOps_LoadPlugins;
      UpdateMenuPlugins;
      UpdateMenuPlugins_Shortcuts(true);
    end;

    if not ASilent then
      DoDialogAddonInstalledReport(msg, msg2, bNeedRestart);
  end;
end;


function TfmMain.GetModifiedCount: integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to FrameCount-1 do
    with Frames[i] do
      if Ed1.Modified or Ed2.Modified then
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
  res: TModalResult;
  i: integer;
begin
  Result:= false;
  Form:= TfmSaveTabs.Create(nil);
  try
    Form.List.Clear;
    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      if not (F.Ed1.Modified or F.Ed2.Modified) then Continue;
      SCaption:= F.TabCaption;
      if F.Filename<>'' then
        SCaption+= '  ('+ExtractFileDir(F.Filename)+')';
      Form.List.Items.AddObject(SCaption, F);
      Form.List.Checked[Form.List.Count-1]:= true;
    end;

    res:= Form.ShowModal;
    case res of
      mrClose:
        Result:= true;
      mrCancel:
        Result:= false;
      mrNoToAll:
        begin
          Result:= true; //like for mrClose
          UiOps.ReopenSession:= false; //dont save tabs to session
        end;
      mrOk:
        begin
          Result:= true;
          for i:= 0 to Form.List.Count-1 do
            if Form.List.Checked[i] then
            begin
              F:= Form.List.Items.Objects[i] as TEditorFrame;
              F.DoFileSave(false, true);
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
    UpdateStatus;
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
    UpdateStatus;
    UpdateCurrentFrame;
  end;
end;

procedure TfmMain.DoApplyLexerStyleMaps(AndApplyTheme: boolean);
var
  F: TEditorFrame;
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];

    if Assigned(F.Lexer[F.Ed1]) then
      F.Lexer[F.Ed1]:= F.Lexer[F.Ed1];

    if not F.EditorsLinked then
      if Assigned(F.Lexer[F.Ed2]) then
        F.Lexer[F.Ed2]:= F.Lexer[F.Ed2];

    if AndApplyTheme then
      F.ApplyTheme;
  end;
end;

procedure TfmMain.DoDialogLexerMap;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F=nil then exit;

  if DoDialogLexerStylesMap(F.Lexer[F.Editor]) then
    DoApplyLexerStyleMaps(false);
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
    Frame.LineEnds[Frame.Editor]:= AValue;
  UpdateStatus;
  MsgStatus(msgStatusEndsChanged);
end;


procedure TfmMain.DoApplyUiOpsToGroups(G: TATGroups);
begin
  G.SetTabFont(Self.Font);
  G.SetTabOption(tabOptionScalePercents, UiOps.Scale);
  G.SetTabOption(tabOptionAnimationEn, Ord(UiOps.TabAnimation));
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
  G.SetTabOptionString(tabOptionModifiedText, ''); //'*' is added in Frame
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

procedure TfmMain.GetParamsForUniqueInstance(out AParams: TAppStringArray);
var
  N, i: integer;
  S, WorkDir: string;
  bAddDir: boolean;
begin
  WorkDir:= GetCurrentDirUTF8;

  N:= ParamCount;
  SetLength(AParams, N);

  for i:= 1 to N do
  begin
    S:= ParamStrUTF8(i);

    bAddDir :=
      (S[1] <> '-') and
      (WorkDir <> '') and
      not IsOsFullPath(S);
    if bAddDir then
      S:= WorkDir+DirectorySeparator+S;

    AParams[i-1]:= S;
  end;
end;

procedure TfmMain.DoApplyUiOps;
var
  id: TAppPanelId;
  CmdParams: TAppStringArray;
  i: integer;
begin
  cAdapterIdleInterval:= UiOps.LexerDelayedParsingPause;
  cAdapterIdleTextSize:= UiOps.LexerDelayedParsingSize;
  CompletionOps.AppendOpeningBracket:= UiOps.AutocompleteAddOpeningBracket;
  CompletionOps.UpDownAtEdge:= TATCompletionUpDownAtEdge(UiOps.AutocompleteUpDownAtEdge);

  LexerProgress.Width:= AppScale(UiOps.ProgressbarHeightSmall);
  StatusProgress.Width:= AppScale(UiOps.ProgressbarWidth);
  ButtonCancel.Width:= AppScale(UiOps.ProgressbarWidth);

  AppScaleSplitter(AppPanels[cPaneSide].Splitter);
  AppScaleSplitter(AppPanels[cPaneOut].Splitter);
  AppScaleSplitter(Groups.Splitter1);
  AppScaleSplitter(Groups.Splitter2);
  AppScaleSplitter(Groups.Splitter3);
  AppScaleSplitter(Groups.Splitter4);
  AppScaleSplitter(Groups.Splitter5);

  //apply DoubleBuffered
  //no need for ToolbarMain and buttons
  for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
    Groups.Pages[i].Tabs.DoubleBuffered:= UiOps.DoubleBuffered;
  for i:= 0 to FrameCount-1 do
    with Frames[i] do
    begin
      Ed1.DoubleBuffered:= UiOps.DoubleBuffered;
      Ed2.DoubleBuffered:= UiOps.DoubleBuffered;
      Ed1.Font.Size:= EditorOps.OpFontSize;
      Ed2.Font.Size:= EditorOps.OpFontSize;
    end;
  Status.DoubleBuffered:= UiOps.DoubleBuffered;
  ButtonCancel.DoubleBuffered:= UiOps.DoubleBuffered;
  StatusProgress.DoubleBuffered:= UiOps.DoubleBuffered;
  LexerProgress.DoubleBuffered:= UiOps.DoubleBuffered;
  ListboxOut.DoubleBuffered:= UiOps.DoubleBuffered;
  ListboxVal.DoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmConsole) then
    fmConsole.IsDoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmFind) then
    fmFind.IsDoubleBuffered:= UiOps.DoubleBuffered;
  //end apply DoubleBuffered

  UpdateStatusbarPanelsFromString(UiOps.StatusPanels);
  UpdateStatusbarHints;

  TimerTreeFill.Interval:= UiOps.TreeTimeFill;
  CodeTree.Tree.ToolTips:= UiOps.TreeShowTooltips;
  CodeTree.Invalidate;

  CodeTreeFilterInput.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  CodeTreeFilterReset.Width:= AppScale(UiOps.ScrollbarWidth);

  EditorCaretPropsFromString(fmConsole.EdMemo.CaretPropsReadonly, EditorOps.OpCaretViewReadonly);
  fmConsole.EdMemo.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  fmConsole.EdInput.Height:= AppScale(UiOps.InputHeight);
  fmConsole.EdInput.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  fmConsole.MemoWordWrap:= UiOps.ConsoleWordWrap;

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
    if id<>cPaneNone then
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

  PanelCodeTreeTop.Height:= AppScale(UiOps.InputHeight);

  TimerStatusClear.Interval:= UiOps.StatusTime*1000;

  ATFlatTheme.FontName:= UiOps.VarFontName;
  ATFlatTheme.FontSize:= UiOps.VarFontSize;
  ATFlatTheme.ScalePercents:= UiOps.Scale;
  ATFlatTheme.ScaleFontPercents:= UiOps.ScaleFont;

  {$ifndef windows}
  ATFlatTheme.EnableColorBgOver:= false;
  {$endif}

  ATScrollbar.ATScrollbarTheme.InitialSize:= UiOps.ScrollbarWidth;
  ATScrollbar.ATScrollbarTheme.BorderSize:= UiOps.ScrollbarBorderSize;
  ATScrollbar.ATScrollbarTheme.ScalePercents:= UiOps.Scale;

  EditorScalePercents:= UiOps.Scale;
  EditorScaleFontPercents:= UiOps.ScaleFont;

  CompletionOps.FormSizeX:= AppScale(UiOps.ListboxCompleteSizeX);
  CompletionOps.FormSizeY:= AppScale(UiOps.ListboxCompleteSizeY);

  {$ifdef unix}
  if not AppAlwaysNewInstance and UiOps.OneInstance then
  begin
    InitUniqueInstanceObject;
    if not AppUniqInst.Enabled then
    begin
      AppUniqInst.Enabled:= true;
      GetParamsForUniqueInstance(CmdParams);
      AppUniqInst.Loaded(CmdParams);

      if AppUniqInst.PriorInstanceRunning then
        Application.Terminate;
        //note: app still works and will get DoFileOpen calls (e.g. on session opening)
        //so later need to check Application.Terminated
    end;
  end;
  {$endif}

  DoApplyTheme;
end;


procedure TfmMain.DoFolderOpen(const ADirName: string; ANewProject: boolean);
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 2);
  Params[0]:= AppVariant(ADirName);
  Params[1]:= AppVariant(ANewProject);

  DoPyCommand('cuda_project_man', 'open_dir', Params);
end;

procedure TfmMain.DoGroupsChangeMode(Sender: TObject);
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 1);
  Params[0]:= AppVariant(APPSTATE_GROUPS);

  DoPyEvent(nil, cEventOnState, Params);
  DoApplyCenteringOption;
end;

procedure TfmMain.DoApplyCenteringOption;
var
  F: TEditorFrame;
  NCentering, i: integer;
begin
  if EditorOps.OpCenteringWidth>0 then
  begin
    if Groups.Mode<>gmOne then
      NCentering:= 0
    else
      NCentering:= EditorOps.OpCenteringWidth;

    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      F.Ed1.OptTextCenteringCharWidth:= NCentering;
      F.Ed2.OptTextCenteringCharWidth:= NCentering;
      F.Ed1.Update;
      F.Ed2.Update;
    end;
  end;
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

function IsFilenameForLexerDetecter(const S: string): boolean;
begin
  Result:= LowerCase(ExtractFileExt(S))<>'.txt';
end;

function TfmMain.DoFileOpen(AFileName, AFileName2: string; APages: TATPages;
  const AOptions: string): TEditorFrame;
var
  D: TATTabData;
  F: TEditorFrame;
  bSilent, bPreviewTab, bEnableHistory,
  bEnableEventPre, bEnableEventOpened, bEnableEventOpenedNone,
  bAllowZip, bAllowPics, bAllowLexerDetect, bDetectedPics,
  bAndActivate, bAllowNear: boolean;
  OpenMode, NonTextMode: TAppOpenMode;
  CurGroups: TATGroups;
  Params: TAppVariantArray;
  //tick: QWord;
  //msg: string;
  i: integer;
begin
  Result:= nil;
  AppDir_LastInstalledAddon:= '';
  if Application.Terminated then exit;

  CurGroups:= CurrentGroups;

  bSilent:= Pos('/silent', AOptions)>0;
  bPreviewTab:= Pos('/preview', AOptions)>0;
  bEnableHistory:= Pos('/nohistory', AOptions)=0;
  bEnableEventPre:= Pos('/noevent', AOptions)=0;
  bEnableEventOpened:= Pos('/noopenedevent', AOptions)=0;
  bEnableEventOpenedNone:= Pos('/nononeevent', AOptions)=0;
  bAndActivate:= Pos('/passive', AOptions)=0;
  bAllowLexerDetect:= Pos('/nolexerdetect', AOptions)=0;
  bAllowNear:= Pos('/nonear', AOptions)=0;
  bAllowZip:= Pos('/nozip', AOptions)=0;
  bAllowPics:= Pos('/nopictures', AOptions)=0;

  if Pos('/view-text', AOptions)>0 then
    OpenMode:= cOpenModeViewText
  else
  if Pos('/view-binary', AOptions)>0 then
    OpenMode:= cOpenModeViewBinary
  else
  if Pos('/view-hex', AOptions)>0 then
    OpenMode:= cOpenModeViewHex
  else
  if Pos('/view-unicode', AOptions)>0 then
    OpenMode:= cOpenModeViewUnicode
  else
    OpenMode:= cOpenModeEditor;

  if Pos('/nontext-view-text', AOptions)>0 then
    NonTextMode:= cOpenModeViewText
  else
  if Pos('/nontext-view-binary', AOptions)>0 then
    NonTextMode:= cOpenModeViewBinary
  else
  if Pos('/nontext-view-hex', AOptions)>0 then
    NonTextMode:= cOpenModeViewHex
  else
  if Pos('/nontext-view-unicode', AOptions)>0 then
    NonTextMode:= cOpenModeViewUnicode
  else
  if Pos('/nontext-cancel', AOptions)>0 then
    NonTextMode:= cOpenModeNone
  else
    NonTextMode:= cOpenModeEditor;

  if APages=nil then
    APages:= CurGroups.PagesCurrent;

  if AFileName='' then
  begin
    D:= CreateTab(APages, '', bAndActivate, bAllowNear);
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
    AFileName:= ResolveWindowsLinkTarget(ExpandFileName(AFileName));
    if not FileExistsUTF8(AFileName) then
    begin
      MsgBox(msgCannotFindFile+#10+AFileName, MB_OK or MB_ICONERROR);
      Exit
    end;
  end;

  if AFileName2<>'' then
  begin
    AFileName2:= ResolveWindowsLinkTarget(ExpandFileName(AFileName2));
    if not FileExistsUTF8(AFileName2) then
    begin
      MsgBox(msgCannotFindFile+#10+AFileName2, MB_OK or MB_ICONERROR);
      Exit
    end;
  end;

  if OpenMode=cOpenModeEditor then
  begin
    //zip files
    if bAllowZip then
    if ExtractFileExt(AFileName)='.zip' then
    begin
      if DoFileInstallZip(AFileName, AppDir_LastInstalledAddon, bSilent) then
        Result:= CurrentFrame;
      exit
    end;

    //py event
    if bEnableEventPre then
    begin
      SetLength(Params, 1);
      Params[0]:= AppVariant(AFileName);
      if DoPyEvent(CurrentEditor, cEventOnOpenBefore, Params).Val = evrFalse then exit;
    end;

    bDetectedPics:= bAllowPics and IsFilenameListedInExtensionList(AFileName, UiOps.PictureTypes);

    //non-text option
    if not bDetectedPics then
    if UiOps.NonTextFiles<>1 then
      if not IsFileContentText(
               AFileName,
               UiOps.NonTextFilesBufferKb,
               GlobalDetectUf16BufferWords,
               false) then
      begin
        if NonTextMode=cOpenModeNone then
          Exit;
        if NonTextMode<>cOpenModeEditor then
          OpenMode:= NonTextMode
        else
        case UiOps.NonTextFiles of
          0:
            case DoDialogConfirmBinaryFile(AFileName, false) of
              ConfirmBinaryViewText: OpenMode:= cOpenModeViewText;
              ConfirmBinaryViewBinary: OpenMode:= cOpenModeViewBinary;
              ConfirmBinaryViewHex: OpenMode:= cOpenModeViewHex;
              ConfirmBinaryViewUnicode: OpenMode:= cOpenModeViewUnicode;
              ConfirmBinaryCancel: Exit;
            end;
          2:
            Exit;
          3:
            OpenMode:= cOpenModeViewBinary;
          4:
            OpenMode:= cOpenModeViewHex;
          else
            Exit;
        end;
      end;

    //too big size?
    if (OpenMode=cOpenModeEditor) and IsFileTooBigForOpening(AFileName) then
    begin
      case DoDialogConfirmBinaryFile(AFileName, true) of
        ConfirmBinaryViewText: OpenMode:= cOpenModeViewText;
        ConfirmBinaryViewBinary: OpenMode:= cOpenModeViewBinary;
        ConfirmBinaryViewHex: OpenMode:= cOpenModeViewHex;
        ConfirmBinaryViewUnicode: OpenMode:= cOpenModeViewUnicode;
        ConfirmBinaryCancel: Exit;
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
    UpdateStatus;
    UpdateTreeContents;
    Exit
  end;

  //preview-tab
  if bPreviewTab then
  begin
    APages:= Groups.Pages1; //open preview tab in 1st group
    for i:= 0 to APages.Tabs.TabCount-1 do
    begin
      D:= APages.Tabs.GetTabData(i);
      if D.TabSpecial then
      begin
        Result:= D.TabObject as TEditorFrame;
        SetFrame(Result);
        Break
      end;
    end;

    if Result=nil then
    begin
      if UiOps.TabsDisabled then
        D:= APages.Tabs.GetTabData(0)
      else
        D:= CreateTab(APages, 'pre', true, false);
      if not Assigned(D) then exit;
      UpdateTabPreviewStyle(D, true);
      Result:= D.TabObject as TEditorFrame;
    end;

    Result.Adapter[Result.Ed1].Stop;
    Result.Adapter[Result.Ed2].Stop;
    Result.DoFileOpen(AFileName, AFileName2, bEnableHistory, bAllowLexerDetect, true, OpenMode);
    MsgStatusFileOpened(AFileName, AFileName2);

    if bEnableEventOpened then
    begin
      SetLength(Params, 0);
      DoPyEvent(Result.Ed1, cEventOnOpen, Params);
    end;

    Result.SetFocus;
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
      F.DoFileOpen(AFileName, AFileName2, bEnableHistory, bAllowLexerDetect, true, OpenMode);
      Result:= F;
      //tick:= (GetTickCount64-tick) div 1000;

      UpdateStatus;
      //if tick>2 then
      //  msg:= msg+' ('+IntToStr(tick)+'s)';
      MsgStatusFileOpened(AFileName, AFileName2);

      if bEnableEventOpened then
      begin
        SetLength(Params, 0);
        DoPyEvent(F.Ed1, cEventOnOpen, Params);
      end;

      if IsFilenameForLexerDetecter(AFileName) then
        if F.IsText and (F.LexerName[F.Ed1]='') then
        begin
          if bEnableEventOpenedNone then
            DoPyEvent(F.Ed1, cEventOnOpenNone, Params);
          UpdateStatus;
        end;

      if AFileName2<>'' then
      begin
        if bEnableEventOpened then
          DoPyEvent(F.Ed2, cEventOnOpen, Params);
        UpdateStatus;
      end;

      Exit
    end;
  end;

  D:= CreateTab(APages, ExtractFileName(AFileName), bAndActivate, bAllowNear);
  if not Assigned(D) then
  begin
    D:= Groups.Pages1.Tabs.GetTabData(0);
    DoClearSingleFirstTab;
  end;
  F:= D.TabObject as TEditorFrame;

  //tick:= GetTickCount64;
  F.DoFileOpen(AFileName, AFileName2, bEnableHistory, bAllowLexerDetect, true, OpenMode);
  Result:= F;
  //tick:= (GetTickCount64-tick) div 1000;

  UpdateStatus;
  //if tick>2 then
  //  msg:= msg+' ('+IntToStr(tick)+'s)';
  MsgStatusFileOpened(AFileName, AFileName2);

  if bEnableEventOpened then
  begin
    SetLength(Params, 0);
    DoPyEvent(F.Ed1, cEventOnOpen, Params);
  end;

  if IsFilenameForLexerDetecter(AFileName) then
    if F.IsText and (F.LexerName[F.Ed1]='') then
      if bEnableEventOpenedNone then
        DoPyEvent(F.Ed1, cEventOnOpenNone, Params);

  if AFileName2<>'' then
    if bEnableEventOpened then
      DoPyEvent(F.Ed2, cEventOnOpen, Params);

  if Visible and Result.Visible and Result.Enabled then
    Result.SetFocus;
end;


procedure TfmMain.DoFileOpenDialog_NoPlugins;
begin
  DoFileOpenDialog('/noevent');
end;

procedure TfmMain.DoFileDialog_PrepareDir(Dlg: TFileDialog);
var
  fn: string;
begin
  //allow ProjectManager to set folder of Open/SaveAs dialog
  fn:= PyCurrentFolder;
  if fn<>'' then
    Dlg.InitialDir:= fn
  else
  begin
    fn:= CurrentFrame.FileName;
    if fn<>'' then
      Dlg.InitialDir:= ExtractFileDir(fn)
    else
    begin
      if UiOps.InitialDir<>'' then
        Dlg.InitialDir:= UiOps.InitialDir
      else
        Dlg.InitialDir:= FLastDirOfOpenDlg;
    end;
  end;
end;

procedure TfmMain.DoFileDialog_SaveDir(Dlg: TFileDialog);
begin
  FLastDirOfOpenDlg:= ExtractFileDir(Dlg.FileName);
end;


procedure TfmMain.DoFileOpenDialog(AOptions: string='');
const
  //passive option used only for many files
  SOptionPassive = '/passive /nonear';
  SOptionSilent = '/silent';
var
  dlg: TOpenDialog;
  NFileCount, NCountZip, i: integer;
  fn: string;
  bZip, bZipAllowed: boolean;
begin
  bZipAllowed:= Pos('/nozip', AOptions)=0;

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
      StatusProgress.Progress:= 0;
      StatusProgress.MaxValue:= NFileCount;
      StatusProgress.Show;

      for i:= 0 to NFileCount-1 do
      begin
        fn:= dlg.Files[i];
        if not FileExistsUTF8(fn) then Continue;
        bZip:= bZipAllowed and (ExtractFileExt(fn)='.zip');
        if bZip then
        begin
          Inc(NCountZip);
          StatusProgress.Progress:= i+1;
          Application.ProcessMessages;
        end;
        DoFileOpen(fn, '', nil, AOptions + SOptionPassive + IfThen(bZip, SOptionSilent));
      end;

      StatusProgress.Hide;
      if NCountZip>0 then
        MsgBox(
          Format(msgStatusAddonsInstalled, [NCountZip]),
          MB_OK or MB_ICONINFORMATION);
    end
    else
    begin
      if FileExistsUTF8(dlg.FileName) then
        DoFileOpen(dlg.FileName, '', nil, AOptions)
      else
      if MsgBox(
        Format(msgConfirmCreateNewFile, [dlg.FileName]),
        MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
      begin
        FCreateFile(dlg.FileName);
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
  MsgStatus(msgStatusHelpOnShowCommands);

  Keymap_UpdateDynamic(categ_Lexer);
  Keymap_UpdateDynamic(categ_OpenedFile);
  Keymap_UpdateDynamic(categ_RecentFile);

  FillChar(Props, SizeOf(Props), 0);
  Props.Caption:= '';
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
    Ed.DoCommand(NCmd);
    UpdateCurrentFrame;
  end;
end;


function TfmMain.DoDialogCommands_Py(var AProps: TDlgCommandsProps): string;
var
  F: TEditorFrame;
  NCmd, NIndex: integer;
begin
  Result:= '';

  F:= CurrentFrame;
  if F=nil then exit;

  AProps.LexerName:= F.LexerName[F.Editor];
  NCmd:= DoDialogCommands_Custom(F.Editor, AProps);
  if NCmd<=0 then exit;

  case AppCommandCategory(NCmd) of
    categ_Plugin:
      begin
        NIndex:= NCmd-cmdFirstPluginCommand;
        with TAppCommandInfo(AppCommandList[NIndex]) do
          if ItemProcParam<>'' then
            Result:= Format('p:module=%s;cmd=%s;info=%s;', [ItemModule, ItemProc, ItemProcParam])
          else
            Result:= Format('p:%s.%s', [ItemModule, ItemProc]);
      end;
    categ_Lexer:
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
    categ_OpenedFile:
      begin
        NIndex:= NCmd-cmdFirstFileCommand;
        if NIndex<AppFrameList1.Count then
          Result:= 'f:'+TEditorFrame(AppFrameList1[NIndex]).FileName
        else
          Result:= 'c:'+IntToStr(NCmd);
      end;
    categ_RecentFile:
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
    fmCommands.OnMsg:= @DoCommandsMsgStatus;
    fmCommands.CurrentLexerName:= AProps.LexerName;
    fmCommands.Keymap:= Ed.Keymap;
    fmCommands.ListCaption:= AProps.Caption;

    if AProps.ShowCentered then
      fmCommands.Position:= poScreenCenter;

    if AProps.W>0 then
      fmCommands.Width:= AProps.W;
    if AProps.H>0 then
      fmCommands.Height:= AProps.H;

    fmCommands.ShowModal;
    Result:= fmCommands.ResultCommand;
    bKeysChanged:= fmCommands.ResultHotkeysChanged;
  finally
    FreeAndNil(fmCommands);
  end;

  if bKeysChanged then
    UpdateMenuPlugins_Shortcuts(true);
end;


procedure TfmMain.DoDialogGoto;
var
  Params: TAppVariantArray;
  Str: string;
begin
  if not Assigned(fmGoto) then
    fmGoto:= TfmGoto.Create(Self);

  fmGoto.Localize;
  fmGoto.IsDoubleBuffered:= UiOps.DoubleBuffered;
  fmGoto.Width:= AppScale(UiOps.ListboxSizeX);
  UpdateInputForm(fmGoto, false);

  if fmGoto.ShowModal=mrOk then
  begin
    Str:= UTF8Encode(fmGoto.edInput.Text);

    SetLength(Params, 1);
    Params[0]:= AppVariant(Str);

    if DoPyEvent(CurrentEditor, cEventOnGotoEnter, Params).Val = evrFalse then exit;

    DoGotoFromInput(Str);
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

procedure TfmMain.DoGotoFromInput(const AInput: string);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
begin
  Frame:= CurrentFrame;
  Ed:= Frame.Editor;

    if Frame.IsBinary then
    begin
      if ViewerGotoFromString(Frame.Binary, AInput) then
        MsgStatus('')
      else
        MsgStatus(msgStatusBadLineNum);
    end
    else
    if Frame.IsText then
    begin
      if EditorGotoFromString(Ed, AInput) then
        MsgStatus('')
      else
        MsgStatus(msgStatusBadLineNum);
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
    Mark: TATBookmarkItem;
    NLine, i: integer;
  const
    cMaxLen = 150;
  begin
    Ed:= Frame.Editor;
    for i:= 0 to Ed.Strings.Bookmarks.Count-1 do
    begin
      Mark:= Ed.Strings.Bookmarks[i];
      if not Mark.Data.ShowInBookmarkList then Continue;

      NLine:= Mark.Data.LineNum;
      if not Ed.Strings.IsIndexValid(NLine) then Continue;

      SCaption:= Copy(Ed.Strings.Lines[NLine], 1, cMaxLen);
      SCaption:= StringReplace(SCaption, #9, '  ', [rfReplaceAll]);

      Prop:= TAppBookmarkProp.Create;
      Prop.Frame:= Frame;
      Prop.Ed:= Ed;
      Prop.LineIndex:= NLine;
      Prop.MenuCaption:=
        SCaption+
        #9+
        Frame.TabCaption+': '+
        NiceBookmarkKind(Mark.Data.Kind)+
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

  with TIniFile.Create(GetAppLangFilename) do
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
    Prop.Ed.DoGotoPos(
      Point(0, Prop.LineIndex),
      Point(-1, -1),
      UiOps.FindIndentHorz,
      UiOps.FindIndentVert,
      true,
      true
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

procedure TfmMain.PopupTabPopup(Sender: TObject);
var
  CurForm: TForm;
  NVis, NCur: Integer;
begin

  CurForm:= Screen.ActiveForm;
  GroupsCtx:= nil;
  NCur:= -1;

  if CurForm=Self then
  begin
    GroupsCtx:= Groups;
    NCur:= GroupsCtx.FindPages(GroupsCtx.PopupPages);
  end
  else
  if FloatGroups then
  begin
    if CurForm=FFormFloatGroups1 then
    begin
      GroupsCtx:= GroupsF1;
      NCur:= 6;
    end
    else
    if CurForm=FFormFloatGroups2 then
    begin
      GroupsCtx:= GroupsF2;
      NCur:= 7;
    end
    else
    if CurForm=FFormFloatGroups3 then
    begin
      GroupsCtx:= GroupsF3;
      NCur:= 8;
    end;
  end;

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

  {$ifdef windows}
  PathAppend:= false;
  dir:= ExtractFileDir(Application.ExeName)+DirectorySeparator;
  SetLength(Dirs, 2);
  Dirs[0]:= dir+ChangeFileExt(UiOps.PyLibrary, 'dlls');
  Dirs[1]:= dir+ChangeFileExt(UiOps.PyLibrary, '.zip');
  {$else}
  PathAppend:= true;
  SetLength(Dirs, 0);
  {$endif}

  //add to sys.path folders py/, py/sys/
  SetLength(Dirs, Length(Dirs)+2);
  Dirs[Length(Dirs)-2]:= AppDir_Py;
  Dirs[Length(Dirs)-1]:= AppDir_Py+DirectorySeparator+'sys';

  AppPython.SetPath(Dirs, PathAppend);
end;

procedure TfmMain.InitPyEngine;
begin
  {$ifdef windows}
  Windows.SetEnvironmentVariable('PYTHONIOENCODING', 'UTF-8');
  {$endif}

  PythonIO:= TPythonInputOutput.Create(Self);
  PythonIO.MaxLineLength:= 2000;
  PythonIO.OnSendUniData:= @PythonIOSendUniData;
  PythonIO.UnicodeIO:= True;
  PythonIO.RawOutput:= False;

  PythonEng:= TPythonEngine.Create(Self);
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

  PythonEng.UseLastKnownVersion:= False;
  PythonEng.DllPath:= ExtractFilePath(UiOps.PyLibrary);
  PythonEng.DllName:= ExtractFileName(UiOps.PyLibrary);
  PythonEng.LoadDll;

  if not AppPython.Inited then
  begin
    FConsoleMustShow:= true;
    MsgLogConsole(msgCannotInitPython1);
    MsgLogConsole(msgCannotInitPython2);
    DisablePluginMenuItems;
  end;
end;

procedure TfmMain.DisablePluginMenuItems;
  //
  procedure _Disable(mi: TMenuItem);
  begin
    if Assigned(mi) then
      mi.Enabled:= false;
  end;
  //
begin
  _Disable(mnuOpPlugins);
  _Disable(mnuPlugins);
end;

procedure TfmMain.MenuEncNoReloadClick(Sender: TObject);
begin
  SetFrameEncoding(CurrentEditor, (Sender as TMenuItem).Caption, false);
end;

procedure TfmMain.MenuEncWithReloadClick(Sender: TObject);
begin
  SetFrameEncoding(CurrentEditor, (Sender as TMenuItem).Caption, true);
end;


procedure TfmMain.SetFrameEncoding(Ed: TATSynEdit; const AEnc: string;
  AAlsoReloadFile: boolean);
var
  Frame: TEditorFrame;
begin
  Frame:= GetEditorFrame(Ed);
  if Frame=nil then exit;

  if SameText(Ed.EncodingName, AEnc) then exit;
  Ed.EncodingName:= AEnc;

  if AAlsoReloadFile then
  begin
    if Frame.GetFileName(Ed)<>'' then
      Frame.DoFileReload_DisableDetectEncoding(Ed)
    else
      MsgBox(msgCannotReloadUntitledTab, MB_OK or MB_ICONWARNING);
  end
  else
  begin
    //set modified to allow save
    Ed.Modified:= true;
  end;

  Ed.DoEventChange; //reanalyze all file
  UpdateFrameEx(Frame, false);
  UpdateStatus;
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
    UpdateTreeContents;

  UpdateFrameEx(F, false);
  UpdateStatus;
end;


procedure TfmMain.DoOps_LexersDisableInFrames(ListNames: TStringList);
var
  F: TEditorFrame;
  i: integer;
begin
  ListNames.Clear;
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    ListNames.Add(F.LexerName[F.Ed1]);

    F.Lexer[F.Ed1]:= nil;
    if not F.EditorsLinked then
      F.Lexer[F.Ed2]:= nil;

    //fix crash: lexer is active in passive tab, LoadLexerLib deletes all lexers, user switches tab
    F.LexerInitial[F.Ed1]:= nil;
    F.LexerInitial[F.Ed2]:= nil;
  end;
end;

procedure TfmMain.DoOps_LexersRestoreInFrames(ListNames: TStringList);
var
  Frame: TEditorFrame;
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
  begin
    Frame:= Frames[i];
    if i<ListNames.Count then
      Frame.LexerName[Frame.Ed1]:= ListNames[i];
  end;
end;


procedure TfmMain.DoOps_LoadLexerLib(AOnCreate: boolean);
var
  ListBackup: TStringlist;
begin
  if not AOnCreate then
    ListBackup:= TStringList.Create
  else
    ListBackup:= nil;

  try
    if Assigned(ListBackup) then
      DoOps_LexersDisableInFrames(ListBackup);

    //load lite lexers
    AppManagerLite.Clear;
    AppManagerLite.LoadFromDir(AppDir_LexersLite);
    if AppManagerLite.LexerCount=0 then
      MsgLogConsole(Format(msgCannotFindLexers, [AppDir_LexersLite]));

    //load EControl lexers
    AppManager.OnLexerLoaded:= @DoOnLexerLoaded;
    AppManager.InitLibrary(AppDir_Lexers);
    if AppManager.LexerCount=0 then
      MsgLogConsole(Format(msgCannotFindLexers, [AppDir_Lexers]));

    if Assigned(ListBackup) then
      DoOps_LexersRestoreInFrames(ListBackup);
  finally
    if Assigned(ListBackup) then
      FreeAndNil(ListBackup);
  end;
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
    sl.Free;
  end;
end;

function TfmMain.GetStatusbarPrefix(Frame: TEditorFrame): string;
begin
  Result:= '';
  if Frame=nil then exit;
  if Frame.IsText then
  begin
    if Frame.ReadOnly[Frame.Editor] then
      Result+= msgStatusReadonly+' ';
    if Frame.MacroRecord then
      Result+= msgStatusMacroRec+' ';
  end;
end;

procedure TfmMain.MsgStatus(AText: string);
begin
  SReplaceAll(AText, #10, ' ');
  SReplaceAll(AText, #13, ' ');

  if DoOnMessage(AText) then
  begin
    FLastStatusbarMessage:= AText;
    DoStatusbarTextByTag(Status, StatusbarTag_Msg, GetStatusbarPrefix(CurrentFrame)+AText);

    if AText='' then exit;
    TimerStatusClear.Enabled:= false;
    TimerStatusClear.Enabled:= true;
  end;
end;

procedure TfmMain.MsgStatusAlt(const AText: string; ASeconds: integer);
const
  cMaxSeconds = 30;
  cSpacing = 3;
var
  Ed: TATSynEdit;
  P: TPoint;
  WorkRect: TRect;
  NCellSize: integer;
begin
  WorkRect:= Screen.WorkAreaRect;
  if StatusForm=nil then
  begin
    StatusForm:= TForm.CreateNew(nil);
    StatusForm.BorderStyle:= bsNone;
    StatusForm.ShowInTaskBar:= stNever;
    StatusFormLabel:= TLabel.Create(StatusForm);
    StatusFormLabel.Parent:= StatusForm;
    StatusFormLabel.Align:= alClient;
    StatusFormLabel.AutoSize:= true;
    StatusFormLabel.Alignment:= taCenter;
    StatusFormLabel.BorderSpacing.Around:= cSpacing;
    StatusFormLabel.ParentFont:= true;
  end;

  StatusForm.FormStyle:= fsSystemStayOnTop;
  StatusForm.Color:= clInfoBk; //GetAppColor(apclListBg);
  StatusForm.Font.Name:= EditorOps.OpFontName;
  StatusForm.Font.Size:= AppScaleFont(EditorOps.OpFontSize);
  StatusForm.Font.Color:= clInfoText; //GetAppColor(apclListFont);

  if ASeconds<=0 then
  begin
    TimerStatusAlt.Enabled:= false;
    StatusForm.Hide;
    Exit
  end;

  if ASeconds>cMaxSeconds then
    ASeconds:= cMaxSeconds;

  StatusFormLabel.Caption:= AText;
  StatusForm.Width:= StatusFormLabel.Canvas.TextWidth(AText)+30;
  StatusForm.Height:= StatusFormLabel.Canvas.TextHeight('W') * (SFindCharCount(AText, #10)+1) + 2*cSpacing;

  case UiOps.AltTooltipPosition of
    0:
      begin
        P:= Self.ClientToScreen(Point(0, 0));
      end;
    1:
      begin
        P:= Status.ClientToScreen(Point(0, 0));
      end;
    2:
      begin
        Ed:= CurrentEditor;
        NCellSize:= Ed.TextCharSize.Y+2*cSpacing;
        P.X:= Ed.Carets[0].PosX;
        P.Y:= Ed.Carets[0].PosY;
        P:= Ed.CaretPosToClientPos(P);
        P:= Ed.ClientToScreen(P);
        Dec(P.Y, NCellSize);
        if P.Y<=WorkRect.Top then
          Inc(P.Y, 2*NCellSize);
      end;
  end;

  P.X:= Min(P.X, WorkRect.Right-StatusForm.Width);
  P.Y:= Min(P.Y, WorkRect.Bottom-StatusForm.Height);

  StatusForm.Left:= P.X;
  StatusForm.Top:= P.Y;

  StatusForm.Show;
  //get focus back from StatusForm
  LCLIntf.SetForegroundWindow(Self.Handle);

  TimerStatusAlt.Interval:= ASeconds*1000;
  TimerStatusAlt.Enabled:= false;
  TimerStatusAlt.Enabled:= true;
end;

procedure TfmMain.PopupListboxOutputCopyClick(Sender: TObject);
begin
  SClipboardCopy(ListboxOut.Items.Text);
end;

procedure TfmMain.PopupListboxOutputClearClick(Sender: TObject);
begin
  ListboxOut.Items.Clear;
  ListboxOut.ItemIndex:= -1;
  ListboxOut.ItemTop:= 0;
  ListboxOut.Invalidate;
  UpdateSidebarButtonOverlay;
end;

procedure TfmMain.PopupListboxValidateCopyClick(Sender: TObject);
begin
  SClipboardCopy(ListboxVal.Items.Text);
end;

procedure TfmMain.PopupListboxValidateClearClick(Sender: TObject);
begin
  ListboxVal.Items.Clear;
  ListboxVal.ItemIndex:= -1;
  ListboxVal.ItemTop:= 0;
  ListboxVal.Invalidate;
  UpdateSidebarButtonOverlay;
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
end;

function TfmMain.GetShowOnTop: boolean;
begin
  Result:= UiOps.ShowFormsOnTop;
end;

procedure TfmMain.SetShowOnTop(AValue: boolean);
begin
  UiOps.ShowFormsOnTop:= AValue;
  UpdateFormOnTop(Self);
  UpdateStatus;
end;

procedure TfmMain.SetSidebarPanel(const ACaption: string);
begin
  if (ACaption<>'-') and (ACaption<>'') then
    if AppPanels[cPaneSide].Visible then
      AppPanels[cPaneSide].UpdatePanels(ACaption, true, true);
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
  AppPanels[cPaneSide].Align:= cVal[AValue];
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

procedure TfmMain.DoFileReopen(Ed: TATSynEdit);
var
  F: TEditorFrame;
  fn: string;
  PrevRO: boolean;
  PrevLexer: string;
begin
  F:= GetEditorFrame(Ed);
  if F=nil then exit;

  fn:= F.GetFileName(Ed);
  if fn='' then exit;

  if not FileExistsUTF8(fn) then
  begin
    MsgStatus(msgCannotFindFile+' '+ExtractFileName(fn));
    exit;
  end;

  if Ed.Modified and UiOps.ReloadUnsavedConfirm then
    if MsgBox(
      Format(msgConfirmReopenModifiedTab, [fn]),
      MB_OKCANCEL or MB_ICONQUESTION
      ) <> ID_OK then exit;

  if Ed.IsReadOnlyChanged then
    PrevRO:= F.ReadOnly[Ed];
  PrevLexer:= F.LexerName[Ed];
  F.ReadOnly[Ed]:= false;
  F.DoFileReload(Ed);
  F.LexerName[Ed]:= PrevLexer;
  if Ed.IsReadOnlyChanged then
    F.ReadOnly[Ed]:= PrevRO;
  Ed.Modified:= false;

  UpdateStatus;
  MsgStatus(msgStatusReopened+' '+ExtractFileName(fn));
end;


function TfmMain.DoFileCloseAll(AWithCancel: boolean): boolean;
var
  Flags: integer;
  F: TEditorFrame;
  ListNoSave: TFPList;
  NCount, i: integer;
begin
  NCount:= FrameCount;
  if (NCount=1) and (Frames[0].IsEmpty) then
    exit(true);

  if AWithCancel then
    Flags:= MB_YESNOCANCEL or MB_ICONQUESTION
  else
    Flags:= MB_YESNO or MB_ICONQUESTION;

  ListNoSave:= TFPList.Create;
  try
    for i:= 0 to NCount-1 do
    begin
      F:= Frames[i];
      if F.Ed1.Modified or F.Ed2.Modified then
        case MsgBox(
               Format(msgConfirmSaveModifiedTab, [F.TabCaption]),
               Flags) of
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

  Result:= Groups.CloseTabs(tabCloseAll, false);
  if not Result then exit;
end;


procedure TfmMain.DoFileCloseAndDelete(Ed: TATSynEdit);
var
  Frame: TEditorFrame;
  fn: string;
begin
  Frame:= GetEditorFrame(Ed);
  if Frame=nil then exit;
  if not Frame.EditorsLinked then exit;

  fn:= Frame.GetFileName(Ed);
  if fn='' then exit;

  if MsgBox(
       msgConfirmCloseDelFile+#10+fn,
       MB_OKCANCEL or MB_ICONWARNING)=ID_OK then
    if Groups.CloseTabs(tabCloseCurrent, false) then
      DeleteFileUTF8(fn);
end;

procedure TfmMain.DoFileNew;
var
  Frame: TEditorFrame;
begin
  Frame:= DoFileOpen('', '');
  DoApplyNewdocLexer(Frame);
end;

procedure TfmMain.DoApplyNewdocLexer(F: TEditorFrame);
begin
  //call this for empty NewdocLexer too- to apply lexer-spec cfg for none-lexer
  if Assigned(F) then
    F.LexerName[F.Ed1]:= UiOps.NewdocLexer;
end;

procedure TfmMain.MenuRecentItemClick(Sender: TObject);
var
  fn: string;
  n: integer;
begin
  n:= (Sender as TComponent).Tag;
  fn:= SExpandHomeDirInFilename(AppListRecents[n]);
  if FileExistsUTF8(fn) then
    DoFileOpen(fn, '')
  else
  begin
    MsgBox(msgCannotFindFile+#10+fn, MB_OK or MB_ICONERROR);
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
  with AppPanels[cPaneSide] do
    Floating:= not Floating;
end;

procedure TfmMain.DoToggleFloatBottom;
begin
  with AppPanels[cPaneOut] do
    Floating:= not Floating;
end;

procedure TfmMain.DoToggleOnTop;
begin
  ShowOnTop:= not ShowOnTop;
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
  with AppPanels[cPaneSide] do
    Visible:= not Visible;
end;

procedure TfmMain.DoToggleBottomPanel;
begin
  with AppPanels[cPaneOut] do
    Visible:= not Visible;
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
      CurrentFrame.SetFocus;
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

procedure TfmMain.DoPyCommand_Cudaxlib(Ed: TATSynEdit; const AMethod: string);
var
  Params: TAppVariantArray;
begin
  Ed.Strings.BeginUndoGroup;
  try
    SetLength(Params, 0);
    DoPyCommand('cudax_lib', AMethod, Params);
  finally
    Ed.Strings.EndUndoGroup;
  end;
end;


procedure TfmMain.DoShowConsole(AndFocus: boolean);
begin
  AppPanels[cPaneOut].UpdatePanels(msgPanelConsole_Init, AndFocus, true);
end;

procedure TfmMain.DoShowOutput(AndFocus: boolean);
begin
  AppPanels[cPaneOut].UpdatePanels(msgPanelOutput_Init, AndFocus, true);
end;

procedure TfmMain.DoShowValidate(AndFocus: boolean);
begin
  AppPanels[cPaneOut].UpdatePanels(msgPanelValidate_Init, AndFocus, true);
end;

procedure TfmMain.SetShowFullScreen(AValue: boolean);
begin
  if FShowFullScreen=AValue then Exit;
  FShowFullScreen:= AValue;
  SetFullScreen_Ex(AValue, false);
end;

procedure TfmMain.SetShowDistractionFree(AValue: boolean);
begin
  if FShowFullScreen=AValue then Exit;
  FShowFullScreen:= AValue;
  SetFullScreen_Ex(AValue, true);
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
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  if AValue then
  begin
    FOrigShowToolbar:= ShowToolbar;
    FOrigShowStatusbar:= ShowStatus;
    FOrigShowBottom:= AppPanels[cPaneOut].Visible;
    FOrigShowSidePanel:= AppPanels[cPaneSide].Visible;
    FOrigShowSideBar:= ShowSideBar;
    FOrigShowTabs:= ShowTabsMain;

    if AHideAll then
    begin
      Ed.OptMinimapVisible:= false;
      Ed.OptMicromapVisible:= false;
      Ed.OptTextCenteringCharWidth:= EditorOps.OpCenteringForDistractionFree;
      Ed.Update;
    end;

    if AHideAll or (Pos('t', UiOps.FullScreen)>0) then ShowToolbar:= false;
    if AHideAll or (Pos('b', UiOps.FullScreen)>0) then AppPanels[cPaneOut].Visible:= false;
    if AHideAll or (Pos('i', UiOps.FullScreen)>0) then ShowStatus:= false;
    if AHideAll or (Pos('p', UiOps.FullScreen)>0) then AppPanels[cPaneSide].Visible:= false;
    if AHideAll or (Pos('a', UiOps.FullScreen)>0) then ShowSideBar:= false;
    if AHideAll or (Pos('u', UiOps.FullScreen)>0) then ShowTabsMain:= false;
    if AHideAll or (Pos('g', UiOps.FullScreen)>0) then DoApplyGutterVisible(false);
  end
  else
  begin
    ShowToolbar:= FOrigShowToolbar;
    ShowStatus:= FOrigShowStatusbar;
    AppPanels[cPaneOut].Visible:= FOrigShowBottom;
    AppPanels[cPaneSide].Visible:= FOrigShowSidePanel;
    ShowSideBar:= FOrigShowSideBar;
    ShowTabsMain:= FOrigShowTabs;
    Ed.OptMinimapVisible:= EditorOps.OpMinimapShow;
    Ed.OptMicromapVisible:= EditorOps.OpMicromapShow;
    Ed.OptTextCenteringCharWidth:= IfThen(Groups.Mode=gmOne, EditorOps.OpCenteringWidth, 0);
    DoApplyGutterVisible(EditorOps.OpGutterShow);
  end;

  {$ifdef windows}
  SetFullScreen_Win32(AValue);
  if not UiOps.ShowMenubar then
    ShowMenu:= false;
  {$else}
  SetFullScreen_Universal(AValue);
  {$endif}
end;

procedure TfmMain.SetFullScreen_Universal(AValue: boolean);
begin
  {$ifdef darwin}
  if AValue then
    BorderStyle:= bsNone
  else
    BorderStyle:= bsSizeable;
  {$endif}

  if AValue then
    ShowWindow(Handle, SW_SHOWFULLSCREEN)
  else
    ShowWindow(Handle, SW_SHOWNORMAL);
end;

procedure TfmMain.SetFullScreen_Win32(AValue: boolean);
begin
  if AValue then
  begin
    FOrigWndState:= WindowState;
    FOrigBounds:= BoundsRect;
    BorderStyle:= bsNone;
    BoundsRect:= Monitor.BoundsRect;
  end
  else
  begin
    WindowState:= FOrigWndState;
    BoundsRect:= FOrigBounds;
    BorderStyle:= bsSizeable;
    BoundsRect:= FOrigBounds; //again
  end;
end;

function TfmMain.GetShowTabsMain: boolean;
begin
  Result:= Groups.Pages1.Tabs.Visible;
end;

procedure TfmMain.SetShowTabsMain(AValue: boolean);
begin
  Groups.SetTabOption(tabOptionShowTabs, Ord(AValue));
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
  F:= DoFileOpen('', '');
  if F=nil then exit;
  F.Ed1.Strings.LoadFromFile(fn);
  F.DoLexerFromFilename(F.Ed1, fn);
  UpdateFrameEx(F, true);
  UpdateStatus;
end;

procedure TfmMain.DoFileSave(Ed: TATSynEdit);
var
  Frame: TEditorFrame;
  bSaveAs, bUntitled, bFileExists: boolean;
  SFilename: string;
begin
  Frame:= GetEditorFrame(Ed);
  if Frame=nil then exit;

  InitSaveDlg;
  DoFileDialog_PrepareDir(SaveDlg);

  bSaveAs:= false;
  SFilename:= Frame.GetFileName(Ed);
  bUntitled:= SFilename='';
  if bUntitled then
    bSaveAs:= true;
  bFileExists:= (SFilename<>'') and FileExistsUTF8(SFilename);

  //if file not exists, it's moved during Cud work, we must recreate it (like ST3)
  if Ed.Modified or bUntitled or not bFileExists then
  begin
    if Frame.DoFileSave_Ex(Ed, bSaveAs) then
      DoFileDialog_SaveDir(SaveDlg);
  end;
end;

procedure TfmMain.DoFileSaveAs(Ed: TATSynEdit);
var
  Frame: TEditorFrame;
begin
  Frame:= GetEditorFrame(Ed);
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
    if Frame.ActivationTime>Time then
    begin
      Time:= Frame.ActivationTime;
      NewFrame:= Frame;
    end;
  end;

  if Assigned(NewFrame) then
  begin
    SetFrame(NewFrame);
    NewFrame.SetFocus;
  end;
end;


function TfmMain.FindFrameOfFilename(const AName: string): TEditorFrame;
var
  F: TEditorFrame;
  i: integer;
begin
  Result:= nil;
  if AName='' then exit;
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    if SameFileName(AName, F.FileName) then
      exit(F);
    if not F.EditorsLinked then
      if SameFileName(AName, F.FileName2) then
        exit(F);
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
  if not FileExistsUTF8(fn) then
  begin
    FCreateFileJSON(fn);
    if not FileExistsUTF8(fn) then Exit;
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

  if not FileExistsUTF8(NameUser) then
  begin
    FCreateFileJSON(NameUser);
    if not FileExistsUTF8(NameUser) then exit;
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

  fn:= GetAppLexerSpecificConfig(CurLexer, false);
  fn_def:= GetAppLexerSpecificConfig(CurLexer, true);

  if not FileExistsUTF8(fn) then
  begin
    FCreateFileJSON(fn);
    if not FileExistsUTF8(fn) then exit;
  end;

  if FileExistsUTF8(fn_def) then
    DoFileOpen(fn_def, fn)
  else
    DoFileOpen(fn, '');
end;

procedure TfmMain.MenuMainClick(Sender: TObject);
var
  F: TEditorFrame;
  bFindFocused: boolean;
  NTag: PtrInt;
  NCommand: integer;
  SCallback: string;
  Params: TAppVariantArray;
begin
  NTag:= (Sender as TComponent).Tag;
  if NTag=0 then exit;

  NCommand:= TAppMenuProps(NTag).CommandCode;
  SCallback:= TAppMenuProps(NTag).CommandString;
  F:= CurrentFrame;

  //dont do editor commands here if ed not focused
  bFindFocused:= Assigned(fmFind) and
    (fmFind.edFind.Focused or fmFind.edRep.Focused);
  if bFindFocused then
    if (NCommand>0) and (NCommand<cmdFirstAppCommand) then exit;

  //-1 means run callback
  if NCommand=-1 then
  begin
    SetLength(Params, 0);
    if SCallback<>'' then
      DoPyCallbackFromAPI(SCallback, Params, []);
  end
  else
    F.Editor.DoCommand(NCommand);

  if (NCommand<>-1)
    and (NCommand<>cmd_FileClose)
    and (NCommand<>cmd_FileCloseAndDelete)
    and (NCommand<>cmd_FileCloseAll) then
    UpdateFrameEx(F, false);

  UpdateStatus;
end;

procedure TfmMain.SetFrameLexerByIndex(Ed: TATSynEdit; AIndex: integer);
var
  F: TEditorFrame;
  CountUsual, CountLite: integer;
begin
  F:= GetEditorFrame(Ed);
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
  UpdateStatus;
end;


procedure TfmMain.DoAutoComplete(Ed: TATSynEdit);
var
  Frame: TEditorFrame;
  LexName: string;
  IsCss, IsHtml, IsCaseSens: boolean;
  FileHtml, FileCss, FileAcp: string;
  Caret: TATCaretItem;
  Params: TAppVariantArray;
begin
  Frame:= GetEditorFrame(Ed);
  if Frame=nil then exit;

  CompletionOps.CommitChars:= UiOps.AutocompleteCommitChars; //before DoPyEvent
  CompletionOps.CloseChars:= UiOps.AutocompleteCloseChars; //before DoPyEvent

  SetLength(Params, 0);
  if DoPyEvent(Ed, cEventOnComplete, Params).Val = evrTrue then exit;

  //py event may handle auto-completion without lexer
  if Frame.Lexer[Ed]=nil then exit;

  Caret:= Ed.Carets[0];
  LexName:= Frame.LexerNameAtPos(Ed, Point(Caret.PosX, Caret.PosY));
  if LexName='' then exit;

  IsHtml:= UiOps.AutocompleteHtml and SRegexMatchesString(LexName, UiOps.AutocompleteHtml_Lexers, false);
  IsCss:= UiOps.AutocompleteCss and SRegexMatchesString(LexName, UiOps.AutocompleteCss_Lexers, false);
  IsCaseSens:= false; //cannot detect it yet
  FileCss:= AppDir_DataAutocompleteSpec+DirectorySeparator+'css_list.ini';
  FileHtml:= AppDir_DataAutocompleteSpec+DirectorySeparator+'html_list.ini';
  FileAcp:= GetAppLexerAcpFilename(LexName);

  //allow autocompletion with carets, only in HTML
  if Ed.Carets.Count>1 then
    if not IsHtml then
    begin
      MsgStatus(msgCannotAutocompleteMultiCarets);
      exit;
    end;
  MsgStatus(msgStatusTryingAutocomplete+' '+LexName);

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
    DoEditorCompletionAcp(Ed, FileAcp, IsCaseSens);
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
begin
  if CodeTree.Tree.SortType=stNone then
    CodeTree.Tree.SortType:= stText
  else
    CodeTree.Tree.SortType:= stNone;
end;

procedure TfmMain.mnuTreeUnfoldAllClick(Sender: TObject);
begin
  CodeTree.Tree.FullExpand;
end;

procedure TfmMain.DoFileExportHtml(F: TEditorFrame);
var
  Ed: TATSynEdit;
  Dlg: TSaveDialog;
  SFileName, STitle: string;
  NX, NY: integer;
begin
  Ed:= F.Editor;

  STitle:= ExtractFileName(F.GetFileName(Ed));
  if STitle='' then
    STitle:= 'untitled';

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
  NX:= 0;
  NY:= 0;
  if Ed.Carets.Count>0 then
    with Ed.Carets[0] do
    begin
      NX:= PosX;
      NY:= PosY;
    end;
  Ed.DoCaretSingle(-1, -1);
  Ed.DoEventCarets;
  Ed.Update;

  //Application.ProcessMessages; //crashes, dont do it

  DoEditorExportToHTML(Ed, SFileName, STitle,
    UiOps.ExportHtmlFontName,
    UiOps.ExportHtmlFontSize,
    UiOps.ExportHtmlNumbers,
    GetAppColor(apclExportHtmlBg),
    GetAppColor(apclExportHtmlNumbers)
    );

  //restore caret
  Ed.DoCaretSingle(NX, NY);
  Ed.DoEventCarets;
  Ed.Update;
  UpdateFrameEx(F, true);

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
  Params: TAppVariantArray;
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

    SetLength(Params, 1);
    Params[0]:= AppVariant(APPSTATE_LANG);
    DoPyEvent(nil, cEventOnState, Params);
  finally
    FreeAndNil(ListNames);
    FreeAndNil(ListFiles);
  end;
end;

procedure TfmMain.SplitterOnPaint_Gr(Sender: TObject);
begin
  //empty, to disable themed paint
end;

procedure TfmMain.SplitterOnPaint_Main(Sender: TObject);
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
  Frame:= GetEditorFrame(Ed);
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
  OpenURL('http://wiki.freepascal.org/CudaText');
end;

procedure TfmMain.DoCodetree_OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then
  begin
    CurrentFrame.SetFocus;
    Key:= 0;
    exit
  end;

  if (Key=VK_RETURN) then
  begin
    (Sender as TTreeView).OnDblClick(Sender);
    Key:= 0;
    exit
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
    true
    );
end;

procedure TfmMain.ListboxOutContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  InitPopupListboxOutput;
  mnuContextOutputCopy.Caption:= cStrMenuitemCopy;
  mnuContextOutputClear.Caption:= msgFileClearList;
  PopupListboxOutput.Popup;
  Handled:= true;
end;

procedure TfmMain.ListboxValidateContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  InitPopupListboxValidate;
  mnuContextValidateCopy.Caption:= cStrMenuitemCopy;
  mnuContextValidateClear.Caption:= msgFileClearList;
  PopupListboxValidate.Popup;
  Handled:= true;
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

procedure TfmMain.ListboxOutKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Prop: ^TAppPanelProps;
  List: TATListbox;
begin
  //Esc
  if (Key=VK_ESCAPE) then
  begin
    CurrentFrame.SetFocus;
    Key:= 0;
    exit
  end;

  List:= Sender as TATListbox;
  if Sender=ListboxOut then
    Prop:= @AppPanelProp_Out
  else
    Prop:= @AppPanelProp_Val;

  if not ((List.ItemIndex>=0) and
          (List.ItemIndex<Prop^.Listbox.Items.Count)) then exit;

  //Ctrl+C
  if (Key=Ord('C')) and (Shift=[ssCtrl]) then
  begin
    SClipboardCopy(Prop^.Listbox.Items.Text);
    Key:= 0;
    exit
  end;

  //Ctrl+D
  if (Key=Ord('D')) and (Shift=[ssCtrl]) then
  begin
    SClipboardCopy(Prop^.Listbox.Items[List.ItemIndex]);
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
      Prop^.Listbox.Items.Clear;

    if List.ItemCount=0 then
      List.ItemIndex:= -1
    else
    if List.ItemIndex>=List.ItemCount then
      List.ItemIndex:= List.ItemCount-1;

    List.Invalidate;
  end;
end;

procedure TfmMain.MenuPicScaleClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F.IsPicture then
  begin
    F.PictureScale:= (Sender as TComponent).Tag;
  end;
end;


procedure TfmMain.DoHelpIssues;
begin
  OpenURL('https://github.com/Alexey-T/CudaText/issues');
end;

procedure TfmMain.DoHelpHotkeys;
begin
  MsgBox(msgStatusHelpOnKeysConfig, MB_OK or MB_ICONINFORMATION);
end;


procedure TfmMain.mnuTabColorClick(Sender: TObject);
var
  F: TEditorFrame;
  NColor: TColor;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  NColor:= Py_DialogColorPicker(F.TabColor);
  if NColor<0 then exit;

  if NColor=clNone then
    F.TabColor:= clNone
  else
    F.TabColor:= NColor;
end;

procedure TfmMain.mnuTabCopyDirClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  F.Editor.DoCommand(cmd_CopyFilenameDir);
end;

procedure TfmMain.mnuTabCopyFullPathClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  F.Editor.DoCommand(cmd_CopyFilenameFull);
end;

procedure TfmMain.mnuTabCopyNameClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  F.Editor.DoCommand(cmd_CopyFilenameName);
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
    AFilename:= Parts[AProp.RegexIdName].Str;
  if AProp.RegexIdLine>0 then
    ALine:= StrToIntDef(Parts[AProp.RegexIdLine].Str, -1);
  if AProp.RegexIdCol>0 then
    ACol:= StrToIntDef(Parts[AProp.RegexIdCol].Str, 0);

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
  NIndex: integer;
  SText: string;
  Ed: TATSynEdit;
  ItemProp: TATListboxItemProp;
  Params: TAppVariantArray;
begin
  Ed:= CurrentEditor;

  if Sender=ListboxOut then
    Prop:= @AppPanelProp_Out
  else
    Prop:= @AppPanelProp_Val;

  NIndex:= Prop^.Listbox.ItemIndex;
  if NIndex<0 then exit;
  if NIndex>=Prop^.Listbox.Items.Count then exit;

  SText:= Prop^.Listbox.Items[NIndex];
  ItemProp:= TATListboxItemProp(Prop^.Listbox.Items.Objects[NIndex]);

  DoParseOutputLine(Prop^, SText, ResFilename, ResLine, ResCol);
  if (ResFilename<>'') and (ResLine>=0) then
  begin
    MsgStatus(Format(msgStatusGotoFileLineCol, [ResFilename, ResLine+1, ResCol+1]));
    if FileExists(ResFilename) then
    begin
      DoFileOpen(ResFilename, '');
      Ed.DoCaretSingle(ResCol, ResLine);
      Ed.DoGotoCaret(cEdgeTop);
      Ed.Update;
      UpdateStatus;
    end;
  end
  else
  begin
    MsgStatus(msgStatusClickingLogLine);
    SetLength(Params, 2);
    Params[0]:= AppVariant(SText);
    Params[1]:= AppVariant(ItemProp.Tag);
    DoPyEvent(nil, cEventOnOutputNav, Params);
  end;
end;


procedure TfmMain.ListboxOutDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
const
  cDx=4;
  cDy=1;
var
  Listbox: TATListbox;
  Prop: PAppPanelProps;
  ResFilename: string;
  ResLine, ResCol: integer;
begin
  Listbox:= Sender as TATListbox;
  Prop:= GetAppPanelProps_ByListbox(Listbox);
  if Prop=nil then exit;
  if AIndex<0 then exit;

  DoParseOutputLine(Prop^, Listbox.Items[AIndex], ResFilename, ResLine, ResCol);
  if (ResFilename<>'') and (ResLine>=0) then
  begin
    C.Font.Color:= GetAppColor(apclListFontHotkey);
    C.Brush.Color:= GetAppColor(apclListBg);
  end
  else
  begin
    C.Font.Color:= GetAppColor(apclListFont);
    C.Brush.Color:= GetAppColor(apclListBg);
  end;

  if AIndex=Listbox.ItemIndex then
  begin
    C.Font.Color:= GetAppColor(apclListSelFont);
    C.Brush.Color:= GetAppColor(apclListSelBg);
    C.FillRect(ARect);
  end;

  C.TextOut(
    ARect.Left+cDx-Listbox.ScrollHorz,
    ARect.Top+cDy,
    Listbox.Items[AIndex]
    );
end;


procedure TfmMain.DoGotoDefinition(Ed: TATSynEdit);
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 0);
  if DoPyEvent(Ed, cEventOnGotoDef, Params).Val <> evrTrue then
    MsgStatus(msgStatusNoGotoDefinitionPlugins);
end;

procedure TfmMain.DoShowFuncHint(Ed: TATSynEdit);
var
  Params: TAppVariantArray;
  S: string;
begin
  SetLength(Params, 0);
  S:= DoPyEvent(Ed, cEventOnFuncHint, Params).Str;
  if S='' then exit;
  MsgStatusAlt(S, UiOps.AltTooltipTime);
end;

procedure TfmMain.DoHideFuncHint;
begin
  TimerStatusAlt.Enabled:= false;
  if Assigned(StatusForm) then
    if StatusForm.Visible then
      StatusForm.Hide;
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
  UpdateMenuItemHotkey(mnuTextOpenUrl, cmd_LinkAtPopup_Open);

  Ed:= CurrentEditor;
  if assigned(mnuTextCut) then mnuTextCut.Enabled:= not Ed.ModeReadOnly;
  if assigned(mnuTextPaste) then mnuTextPaste.Enabled:= not Ed.ModeReadOnly and Clipboard.HasFormat(CF_Text);
  if assigned(mnuTextDelete) then mnuTextDelete.Enabled:= not Ed.ModeReadOnly and Ed.Carets.IsSelection;
  if assigned(mnuTextUndo) then mnuTextUndo.Enabled:= not Ed.ModeReadOnly and (Ed.UndoCount>0);
  if assigned(mnuTextRedo) then mnuTextRedo.Enabled:= not Ed.ModeReadOnly and (Ed.RedoCount>0);
  if assigned(mnuTextOpenUrl) then mnuTextOpenUrl.Enabled:= EditorGetLinkAtScreenCoord(Ed, PopupText.PopupPoint)<>'';
end;


procedure TfmMain.CharmapOnInsert(const AStr: string);
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  if Ed.Carets.Count=0 then exit;
  Ed.DoCommand(cCommand_TextInsert, Utf8Decode(AStr));

  UpdateCurrentFrame(true);
  UpdateStatus;
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

function TfmMain.DoOnConsoleNav(const Str: string): boolean;
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 1);
  Params[0]:= AppVariant(Str);

  Result:= DoPyEvent(nil, cEventOnConsoleNav, Params).Val <> evrFalse;
end;

function TfmMain.DoOnMessage(const AText: string): boolean;
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 2);
  Params[0]:= AppVariant(0); //reserved for future
  Params[1]:= AppVariant(AText);

  Result:= DoPyEvent(nil, cEventOnMessage, Params).Val <> evrFalse;
end;


procedure TfmMain.DoOnConsoleNumberChange(Sender: TObject);
begin
  UpdateSidebarButtonOverlay;
end;

function TfmMain.DoOnMacro(Frame: TEditorFrame; const Str: string): boolean;
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 1);
  Params[0]:= AppVariant(Str);

  Result:= DoPyEvent(Frame.Editor, cEventOnMacro, Params).Val <> evrFalse;
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
    SPLITTER_SIDE: GetSp(AppPanels[cPaneSide].Splitter);
    SPLITTER_BOTTOM: GetSp(AppPanels[cPaneOut].Splitter);
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
    SPLITTER_SIDE: SetSp(AppPanels[cPaneSide].Splitter);
    SPLITTER_BOTTOM: SetSp(AppPanels[cPaneOut].Splitter);
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
  Params: TAppVariantArray;
  Keymap: TATKeymap;
  {$ifdef debug_on_lexer}
  SFileName: string;
  {$endif}
  SLexerName: string;
begin
  Ed:= Sender;
  Frame:= GetEditorFrame(Ed);
  if Frame=nil then exit;

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
  if not FSessionIsLoading then
    if (SLexerName<>'') or not EditorIsEmpty(Ed) then
    begin
      {$ifdef debug_on_lexer}
      MsgLogConsole('on_lexer: file "'+ExtractFileName(SFileName)+'" -> "'+SLexerName+'"');
      {$endif}

      SetLength(Params, 0);
      DoPyEvent(Ed, cEventOnLexer, Params);
    end;

  //apply lexer-specific keymap
  Keymap:= Keymap_GetForLexer(SLexerName);

  if Frame.EditorsLinked then
  begin
    Frame.Ed1.Keymap:= Keymap;
    Frame.Ed2.Keymap:= Keymap;
  end
  else
    Ed.Keymap:= Keymap;

  UpdateMenuPlugins_Shortcuts;
end;


procedure TfmMain.DoToolbarClick(Sender: TObject);
var
  SData: string;
  NCmd: integer;
  Params: TAppVariantArray;
begin
  //str(int_command) or callback string
  SData:= (Sender as TATButton).DataString;
  NCmd:= StrToIntDef(SData, 0);

  if NCmd>0 then
    CurrentEditor.DoCommand(NCmd)
  else
  begin
    SetLength(Params, 0);
    DoPyCallbackFromAPI(SData, Params, []);
  end;

  UpdateCurrentFrame;
  UpdateStatus;
end;


function TfmMain.DoMenu_GetPyProps(mi: TMenuItem): PPyObject;
var
  NTag: PtrInt;
  NCommand: integer;
  SCommand, STagString: string;
  CmdObject: PPyObject;
begin
  NTag:= mi.Tag;
  if NTag<>0 then
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
    if NCommand>0 then
      CmdObject:= PyInt_FromLong(NCommand)
    else
      CmdObject:= PyString_FromString(PChar(SCommand));

    Result:= Py_BuildValue('{sLsssisssssssOsOsOsOsO}',
      'id',
      Int64(PtrInt(mi)),
      'cap',
      PChar(mi.Caption),
      'cmd',
      NCommand,
      'hint',
      PChar(SCommand),
      'hotkey',
      PChar(ShortCutToText(mi.ShortCut)),
      'tag',
      PChar(STagString),
      'command',
      CmdObject,
      'checked',
      PyBool_FromLong(Ord(mi.Checked)),
      'radio',
      PyBool_FromLong(Ord(mi.RadioItem)),
      'en',
      PyBool_FromLong(Ord(mi.Enabled)),
      'vis',
      PyBool_FromLong(Ord(mi.Visible))
      );
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
    mi:= Py_MenuItemFromId(AMenuId);
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
var
  mi: TMenuItem;
begin
  mi:= Py_MenuItemFromId(AMenuId);
  if Assigned(mi) then
  begin
    mi.Clear;
    if AMenuId=PyMenuId_Top then
    begin
      mnuFileOpenSub:= nil;
      mnuFileEnc:= nil;
      mnuPlugins:= nil;
      mnuOpPlugins:= nil;
      mnuLexers:= nil;
    end;
    if AMenuId=PyMenuId_TopOptions then
    begin
    end;
    if AMenuId=PyMenuId_TopFile then
    begin
      mnuFileOpenSub:= nil;
    end;
    if AMenuId=PyMenuId_TopView then
    begin
      mnuLexers:= nil;
    end;
    if AMenuId=PyMenuId_Text then
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
    if AMenuId=PyMenuId_Tab then
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


function TfmMain.DoMenuAdd_Params(const AMenuId, AMenuCmd, AMenuCaption,
  AMenuHotkey, AMenuTagString: string; AIndex: integer): string;
var
  mi, miMain: TMenuItem;
  Num: integer;
begin
  Result:= '';
  miMain:= Py_MenuItemFromId(AMenuId);
  if Assigned(miMain) and (AMenuCaption<>'') then
  begin
    mi:= TMenuItem.Create(Self);
    mi.Caption:= AMenuCaption;
    mi.Tag:= PtrInt(TAppMenuProps.Create);
    TAppMenuProps(mi.Tag).TagString:= AMenuTagString;

    Num:= StrToIntDef(AMenuCmd, 0); //command code
    if Num>0 then
    begin
      UpdateMenuItemHotkey(mi, Num);
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
      TAppMenuProps(mi.Tag).CommandString:= 'plugins';
      UpdateMenuPlugins;
    end
    else
    if AMenuCmd='_oplugins' then
    begin
      mnuOpPlugins:= mi;
      UpdateMenuPlugins;
    end
    else
    begin
      TAppMenuProps(mi.Tag).CommandCode:= -1;
      TAppMenuProps(mi.Tag).CommandString:= AMenuCmd;
      if (AMenuCmd<>'0') and (AMenuCmd<>'') then
        mi.OnClick:= @MenuMainClick;
    end;

    if AMenuHotkey<>'' then
      mi.ShortCut:= TextToShortCut(AMenuHotkey);

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
  mi:= Py_MenuItemFromId(AMenuId);
  mi.Free;
end;

procedure TfmMain.DoFileNewMenu(Sender: TObject);
var
  Params: TAppVariantArray;
begin
  if not AppPython.Inited then
  begin
    MsgStatus(msgCommandNeedsPython);
    exit;
  end;

  SetLength(Params, 0);
  DoPyCommand('cuda_new_file', 'menu', Params);
end;

procedure TfmMain.DoCommandsMsgStatus(Sender: TObject; const ARes: string);
begin
  MsgStatus(ARes);
end;

function TfmMain.LiteLexer_GetStyleHash(Sender: TObject; const AStyleName: string): integer;
var
  iStyle: TAppThemeStyleId;
begin
  Result:= -1;
  for iStyle:= Low(iStyle) to High(iStyle) do
  begin
    if AStyleName=AppTheme.Styles[iStyle].DisplayName then
      exit(Ord(iStyle));
  end;
end;

procedure TfmMain.LiteLexer_ApplyStyle(Sender: TObject; AStyleHash: integer;
  var APart: TATLinePart);
var
  st: TecSyntaxFormat;
begin
  if AStyleHash<0 then exit;
  st:= AppTheme.Styles[TAppThemeStyleId(AStyleHash)];
  ApplyPartStyleFromEcontrolStyle(APart, st);
end;

procedure TfmMain.MenuTabsizeClick(Sender: TObject);
begin
  UpdateEditorTabsize((Sender as TComponent).Tag);
end;

procedure TfmMain.MsgLogDebug(const AText: string);
begin
  if UiOps.LogDebug then
    MsgLogToFilename(AText, FFileNameLogDebug, true);
end;


procedure TfmMain.MsgLogToFilename(const AText, AFilename: string; AWithTime: boolean);
var
  f: TextFile;
  S: string;
begin
  AssignFile(f, AFileName);
  {$I-}
  Append(f);
  if IOResult<>0 then
    Rewrite(f);
  S:= AText;
  if AWithTime then
    S:= FormatDateTime('[MM.DD hh:nn] ', Now) + S;
  Writeln(f, S);
  CloseFile(f);
  {$I+}
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


procedure TfmMain.InitFloatGroup(var F: TForm; var G: TATGroups; ATag: integer;
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
    F.Caption:= Format('[f%d]', [ATag]) + ' - ' + msgTitle;

    F.AllowDropFiles:= true;
    F.OnDropFiles:= @FormFloatGroups_OnDropFiles;

    if UiOps.FloatGroupsInTaskbar then
      F.ShowInTaskBar:= stAlways
    else
      F.ShowInTaskBar:= stNever;

    G:= TATGroups.Create(Self);
    G.Pages1.EnabledEmpty:= true;
    G.Tag:= ATag;
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
  Result:= TEditorFrame(ATabObject).ActivationTime;
end;

function TfmMain.IsWindowMaximizedOrFullscreen: boolean;
begin
  Result:= ShowFullscreen or (WindowState=wsMaximized);
end;

procedure TfmMain.MenuitemClick_CommandFromTag(Sender: TObject);
begin
  CurrentEditor.DoCommand((Sender as TComponent).Tag);
end;

procedure TfmMain.MenuitemClick_CommandFromHint(Sender: TObject);
var
  Sep: TATStringSeparator;
  SModule, SProc: string;
  Params: TAppVariantArray;
begin
  Sep.Init((Sender as TMenuItem).Hint);
  Sep.GetItemStr(SModule);
  Sep.GetItemStr(SProc);
  SetLength(Params, 0);
  DoPyCommand(SModule, SProc, Params);
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

procedure TfmMain.DoCodetree_ApplyTreeHelperResults(Data: PPyObject);
var
  Tree: TTreeView;
  DataItem, DataPos, DataLevel, DataTitle, DataIcon: PPyObject;
  NCount, NX1, NY1, NX2, NY2, NLevel, NLevelPrev, NIcon: integer;
  STitle: string;
  Node, NodeParent: TTreeNode;
  Range: TATRangeInCodeTree;
  iItem, iLevel: integer;
begin
  Tree:= CodeTree.Tree;
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

        NX1:= PyInt_AsLong(PyTuple_GetItem(DataPos, 0));
        NY1:= PyInt_AsLong(PyTuple_GetItem(DataPos, 1));
        NX2:= PyInt_AsLong(PyTuple_GetItem(DataPos, 2));
        NY2:= PyInt_AsLong(PyTuple_GetItem(DataPos, 3));
        NLevel:= PyInt_AsLong(DataLevel);
        STitle:= PyString_AsAnsiString(DataTitle);
        NIcon:= PyInt_AsLong(DataIcon);

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
  begin
    LexerProgress.Progress:= FLexerProgressIndex;
    //if FLexerProgressIndex>0 then
    //  LexerProgress.Show;
  end
  else
    //LexerProgress.Hide;
    LexerProgress.Progress:= 0;
end;

function _FrameListCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  t1, t2: Int64;
begin
  t1:= TEditorFrame(List.Objects[Index1]).ActivationTime;
  t2:= TEditorFrame(List.Objects[Index2]).ActivationTime;
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
    for iGroup:= 0 to 8 do
    begin
      Pages:= GetPagesOfGroupIndex(iGroup);
      if Pages=nil then Continue;
      for iTab:= 0 to Pages.Tabs.TabCount-1 do
      begin
        F:= Pages.Tabs.GetTabData(iTab).TabObject as TEditorFrame;
        if F=CurFrame then Continue;

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

    if FrameList.Count=0 then exit;
    FrameList.CustomSort(@_FrameListCompare);

    iTab:= DoDialogMenuList(msgPanelTabs, FrameList, 0, true);
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

  with TIniFile.Create(GetAppLangFilename) do
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

    FillChar(DlgProps, SizeOf(DlgProps), 0);
    DlgProps.ItemsText:= List.Text;
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

  UpdateStatus;
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

procedure DoOnLexerLoaded(Sender: TObject; ALexer: TecSyntAnalyzer);
var
  fn_ops: string;
begin
  //load *.cuda-lexops
  fn_ops:= GetAppLexerOpsFilename(ALexer.LexerName);
  if FileExistsUTF8(fn_ops) then
    DoLoadLexerStylesFromFile_JsonLexerOps(ALexer, fn_ops, UiOps.LexerThemes);
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
  if not Assigned(LexersDetected) then
    LexersDetected:= TStringList.Create;
  LexersDetected.Assign(Lexers);
  Result:= 0;
end;

procedure TfmMain.InitConfirmPanel;
const
  cW = 10; //in avg chars
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

  FCfmPanel.Color:= GetAppColor(apclButtonBgOver);
  FCfmPanel.Font.Name:= UiOps.VarFontName;
  FCfmPanel.Font.Size:= AppScaleFont(UiOps.VarFontSize);
  FCfmPanel.Font.Color:= GetAppColor(apclButtonFont);

  //FCfmPanel.Width:= AppScaleFont(UiOps.VarFontSize)*cW;
  FCfmPanel.Height:= Trunc(AppScaleFont(UiOps.VarFontSize)*cH);
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

