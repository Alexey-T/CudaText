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
  LConvEncoding,
  TreeFilterEdit,
  gqueue,
  {$ifdef LCLGTK2}
  fix_gtk_clipboard,
  {$endif}
  fix_focus_window,
  at__jsonconf,
  PythonEngine,
  UniqueInstance,
  ec_LexerList,
  ec_SyntAnal,
  ec_SyntaxClient,
  ec_syntax_format,
  ec_rules,
  ATButtons,
  ATFlatToolbar,
  ATFlatThemes,
  ATListbox,
  ATScrollBar,
  ATPanelSimple,
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
  ATSynEdit_CanvasProc,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Adapter_LiteLexer,
  ATSynEdit_CharSizer,
  ATSynEdit_Export_HTML,
  ATSynEdit_Edits,
  ATTabs,
  ATGroups,
  ATStatusBar,
  ATStrings,
  ATStringProc,
  ATGauge,
  ATBinHex,
  atsynedit_form_complete,
  atsynedit_form_complete_synwrite,
  atsynedit_form_complete_css,
  atsynedit_form_complete_html,
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
  proc_customdialog_dummy,
  proc_scrollbars,
  proc_keymap_undolist,
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
  formlexerstylemap,
  formcolorsetup,
  formabout,
  formcharmaps,
  formkeyinput,
  form_addon_report,
  formconfirmbinary,
  math;

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

  TAppConsoleQueue = specialize TQueue<string>;

const
  cMenuTabsizeMin = 1;
  cMenuTabsizeMax = 10;

type
  { TfmMain }
  TfmMain = class(TForm)
    AppProps: TApplicationProperties;
    ButtonCancel: TATButton;
    mnuOpThemesUi: TMenuItem;
    mnuOpThemesSyntax: TMenuItem;
    mnuOpLangs: TMenuItem;
    mnuTabsizeSep: TMenuItem;
    mnuTabsizeConvTabs: TMenuItem;
    mnuTabsizeConvSpaces: TMenuItem;
    mnuViewSidebar: TMenuItem;
    mnuTabCopyName: TMenuItem;
    mnuTabCopyDir: TMenuItem;
    mnuTabCopyFullPath: TMenuItem;
    mnuTabCopySub: TMenuItem;
    mnuGr6H: TMenuItem;
    mnuGr6V: TMenuItem;
    mnuTabMoveF2: TMenuItem;
    mnuTabMoveF3: TMenuItem;
    mnuSepT2: TMenuItem;
    mnuViewFloatSide: TMenuItem;
    mnuViewFloatBottom: TMenuItem;
    mnuOpDefaultUser: TMenuItem;
    TimerStatusBusy: TTimer;
    TimerAppIdle: TIdleTimer;
    ImageListTabs: TImageList;
    ImageListToolbar: TImageList;
    MenuItem5: TMenuItem;
    mnuSelExtWord: TMenuItem;
    mnuViewOnTop: TMenuItem;
    mnuOpPlugins: TMenuItem;
    mnuTreeSep1: TMenuItem;
    mnuTreeSorted: TMenuItem;
    mnuViewUnpriSpacesTail: TMenuItem;
    mnuViewMicromap: TMenuItem;
    mnuHelpCheckUpd: TMenuItem;
    PopupPicScale: TPopupMenu;
    StatusProgress: TATGauge;
    LabelSideTitle: TLabel;
    MenuItem4: TMenuItem;
    mnuViewDistFree: TMenuItem;
    SepV4: TMenuItem;
    mnuBmPlaceOnCarets: TMenuItem;
    mnuFileNewMenu: TMenuItem;
    mnuPluginsEmpty: TMenuItem;
    ImageListSide: TImageList;
    FontDlg: TFontDialog;
    ImageListBm: TImageList;
    ImageListTree: TImageList;
    MainMenu: TMainMenu;
    SepOp2: TMenuItem;
    mnuBmDeleteLines: TMenuItem;
    mnuBmCopyLines: TMenuItem;
    mnuOpThemeSyntax: TMenuItem;
    mnuBmPlaceCarets: TMenuItem;
    PaintTest: TPaintBox;
    PanelAll: TATPanelSimple;
    PanelBottom: TATPanelSimple;
    PanelLeft: TATPanelSimple;
    PanelLeftTitle: TATPanelSimple;
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
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem14: TMenuItem;
    SepHelp1: TMenuItem;
    SepHelp2: TMenuItem;
    SepFile1: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
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
    mnuOpThemeUi: TMenuItem;
    mnuEditTrimL: TMenuItem;
    mnuEditTrimR: TMenuItem;
    mnuEditTrim: TMenuItem;
    mnuTabColor: TMenuItem;
    MenuItem29: TMenuItem;
    mnuTabsizeSpace: TMenuItem;
    mnuFind2Prev: TMenuItem;
    mnuTabSaveAs: TMenuItem;
    mnuTabSave: TMenuItem;
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
    mnuFind2Next: TMenuItem;
    MenuItem20: TMenuItem;
    mnuFind2WordNext: TMenuItem;
    mnuFind2WordPrev: TMenuItem;
    mnuHelpChangelog: TMenuItem;
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
    mnuTabMove1: TMenuItem;
    mnuTabMove2: TMenuItem;
    mnuTabMove3: TMenuItem;
    mnuTabMove4: TMenuItem;
    mnuTabMove5: TMenuItem;
    mnuTabMove6: TMenuItem;
    mnuTabMoveF1: TMenuItem;
    mnuSepT1: TMenuItem;
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
    mnuFileOpenDir: TMenuItem;
    mnuFileOpenSub: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuFileClose: TMenuItem;
    OpenDlg: TOpenDialog;
    PopupEnc: TPopupMenu;
    PopupEnds: TPopupMenu;
    PopupLex: TPopupMenu;
    PopupFind: TPopupMenu;
    PopupText: TPopupMenu;
    PopupTree: TPopupMenu;
    PopupTabSize: TPopupMenu;
    PopupRecents: TPopupMenu;
    PopupTab: TPopupMenu;
    PythonEngine: TPythonEngine;
    PythonIO: TPythonInputOutput;
    PythonMod: TPythonModule;
    SaveDlg: TSaveDialog;
    SplitterHorz: TSplitter;
    SplitterVert: TSplitter;
    TimerStatusAlt: TTimer;
    TimerTreeFill: TTimer;
    TimerCmd: TTimer;
    TimerStatus: TTimer;
    TimerEdCaret: TTimer;
    ToolbarMain: TATFlatToolbar;
    ToolbarSideMid: TATFlatToolbar;
    ToolbarSideLow: TATFlatToolbar;
    ToolbarSideTop: TATFlatToolbar;
    UniqInstance: TUniqueInstance;
    procedure AppPropsActivate(Sender: TObject);
    procedure AppPropsEndSession(Sender: TObject);
    procedure AppPropsQueryEndSession(var Cancel: Boolean);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var ACanClose: boolean);
    procedure FormColorsApply(const AColors: TAppTheme);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DoCodetree_StopUpdate;
    procedure FrameAddRecent(Sender: TObject; const AFileName: string);
    procedure FrameOnMsgStatus(Sender: TObject; const AStr: string);
    procedure FrameOnChangeCaretPos(Sender: TObject);
    procedure FrameOnInitAdapter(Sender: TObject);
    procedure FrameParseDone(Sender: TObject);
    procedure ListboxOutClick(Sender: TObject);
    procedure ListboxOutDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
    procedure ListboxOutKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuTabColorClick(Sender: TObject);
    procedure mnuTabCopyDirClick(Sender: TObject);
    procedure mnuTabCopyFullPathClick(Sender: TObject);
    procedure mnuTabCopyNameClick(Sender: TObject);
    procedure mnuTabMoveF2Click(Sender: TObject);
    procedure mnuTabMoveF3Click(Sender: TObject);
    procedure MenuRecentsClear(Sender: TObject);
    procedure mnuFind2NextClick(Sender: TObject);
    procedure mnuFind2PrevClick(Sender: TObject);
    procedure mnuFind2WordNextClick(Sender: TObject);
    procedure mnuFind2WordPrevClick(Sender: TObject);
    procedure DoHelpAbout;
    procedure DoHelpForum;
    procedure DoHelpChangelog;
    procedure DoHelpWiki;
    procedure DoHelpIssues;
    procedure DoHelpHotkeys;

    procedure MenuWindowClick(Sender: TObject);
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
    procedure PopupTabPopup(Sender: TObject);
    procedure PopupTabSizePopup(Sender: TObject);
    procedure PopupTextPopup(Sender: TObject);
    procedure PythonEngineAfterInit(Sender: TObject);
    procedure PythonIOSendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PythonModInitialization(Sender: TObject);
    procedure StatusPanelClick(Sender: TObject; AIndex: Integer);
    procedure TimerAppIdleTimer(Sender: TObject);
    procedure TimerCmdTimer(Sender: TObject);
    procedure TimerStatusAltTimer(Sender: TObject);
    procedure TimerStatusBusyTimer(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure TimerTreeFillTimer(Sender: TObject);
    procedure TimerEdCaretTimer(Sender: TObject);
    procedure UniqInstanceOtherInstance(Sender: TObject; ParamCount: Integer;
      Parameters: array of String);
    {$ifdef windows}
    procedure SecondInstance(const Msg: TBytes);
    {$endif}
  private
    { private declarations }
    mnuToolbarCaseLow: TMenuItem;
    mnuToolbarCaseUp: TMenuItem;
    mnuToolbarCaseTitle: TMenuItem;
    mnuToolbarCaseInvert: TMenuItem;
    mnuToolbarCaseSent: TMenuItem;
    mnuToolbarCommentLineAdd: TMenuItem;
    mnuToolbarCommentLineDel: TMenuItem;
    mnuToolbarCommentLineToggle: TMenuItem;
    mnuToolbarCommentStream: TMenuItem;
    mnuToolbarSortAsc: TMenuItem;
    mnuToolbarSortDesc: TMenuItem;
    mnuToolbarSortAscNocase: TMenuItem;
    mnuToolbarSortDescNocase: TMenuItem;
    mnuToolbarSortDialog: TMenuItem;
    mnuToolbarSortReverse: TMenuItem;
    mnuToolbarSortShuffle: TMenuItem;
    mnuToolbarSortRemoveDup: TMenuItem;
    mnuToolbarSortRemoveBlank: TMenuItem;
    mnuToolbarSortSep1: TMenuItem;
    mnuToolbarSortSep2: TMenuItem;
    PopupToolbarCase: TPopupMenu;
    PopupToolbarComment: TPopupMenu;
    PopupToolbarSort: TPopupMenu;
    FFormFloatSide: TForm;
    FFormFloatBottom: TForm;
    FFormFloatGroups1: TForm;
    FFormFloatGroups2: TForm;
    FFormFloatGroups3: TForm;
    FBoundsFloatSide: TRect;
    FBoundsFloatBottom: TRect;
    FBoundsFloatGroups1: TRect;
    FBoundsFloatGroups2: TRect;
    FBoundsFloatGroups3: TRect;
    FListRecents: TStringList;
    FListLangs: TStringList;
    FListTimers: TStringList;
    FConsoleQueue: TAppConsoleQueue;
    FKeymapUndoList: TATKeymapUndoList;
    FKeymapLastLexer: string;
    FConsoleMustShow: boolean;
    FThemeUi: string;
    FThemeSyntax: string;
    FSessionName: string;
    FColorDialog: TColorDialog;
    Status: TATStatus;
    StatusAlt: TATStatus;
    Groups: TATGroups;
    GroupsCtx: TATGroups;
    GroupsF1: TATGroups;
    GroupsF2: TATGroups;
    GroupsF3: TATGroups;

    mnuApple: TMenuItem;
    mnuApple_About: TMenuItem;
    mnuApple_CheckUpd: TMenuItem;
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
    FAllowLoadKeymap: boolean;
    FAllowOnFocus: boolean;
    FHandledOnShow: boolean;
    FFileNamesDroppedInitially: array of string;
    FFileNameLogDebug: string;
    FFileNameLogConsole: string;
    FCodetreeDblClicking: boolean;
    FCodetreeModifiedVersion: integer;
    FMenuCopy: TPopupMenu;
    FMenuItemTabSize: array[cMenuTabsizeMin..cMenuTabsizeMax] of TMenuItem;
    FMenuVisible: boolean;
    FPopupListboxOutput: TPopupMenu;
    FPopupListboxValidate: TPopupMenu;
    FNewClickedEditor: TATSynEdit;
    FPyComplete_Editor: TATSynEdit;
    FPyComplete_Text: string;
    FPyComplete_CharsLeft: integer;
    FPyComplete_CharsRight: integer;
    FPyComplete_CaretPos: TPoint;
    FLastDirOfOpenDlg: string;
    FLastLexerForPluginsMenu: string;
    FLastSidebarPanel: string;
    FLastBottomPanel: string;
    FLastStatusbarMessage: string;
    FLastSelectedCommand: integer;
    FLastMousePos: TPoint;
    FLexerProgressIndex: integer;
    FLexerProgressCount: integer;
    FOption_OpenReadOnly: boolean;
    FOption_OpenNewWindow: boolean;
    FOption_WindowPos: string;
    FOption_Encoding: string;
    FOption_FileOpenOptions: string;
    FOption_GroupMode: TATGroupsMode;
    FOption_GroupSizes: TATGroupsPoints;
    FOption_GroupPanelSize: TPoint;
    FOption_SidebarTab: string;

    procedure CodeTreeFilter_OnChange(Sender: TObject);
    procedure CodeTreeFilter_ResetOnClick(Sender: TObject);
    procedure CodeTreeFilter_OnCommand(Sender: TObject; ACmd: integer;
      const AText: string; var AHandled: boolean);
    procedure DisablePluginMenuItems;
    procedure DoApplyCenteringOption;
    procedure DoApplyLexerStyleMaps(AndApplyTheme: boolean);
    procedure DoApplyTranslationToGroups(G: TATGroups);
    procedure DoClearSingleFirstTab;
    procedure DoCloseAllTabs;
    procedure DoFileDialog_PrepareDir(Dlg: TFileDialog);
    procedure DoFileDialog_SaveDir(Dlg: TFileDialog);
    procedure DoCommandsMsgStatus(Sender: TObject; const ARes: string);
    procedure DoFindMarkingInit(AMode: TATFindMarkingMode);
    procedure DoFindOptions_OnChange(Sender: TObject);
    procedure DoFindOptions_ApplyDict(AText: string);
    procedure DoFindOptions_ResetInSelection;
    function DoFindOptions_GetDict: PPyObject;
    procedure DoFolderOpen(const ADirName: string; ANewProject: boolean);
    procedure DoGroupsChangeMode(Sender: TObject);
    procedure DoOnLexerParseProgress(Sender: TObject; AProgress: integer);
    //procedure DoOnLexerParseProgress(Sender: TObject; ALineIndex, ALineCount: integer);
    procedure DoOnLexerParseProgress_Sync();
    procedure DoOps_AddPluginMenuItem(ACaption: string; ASubMenu: TMenuItem; ATag: integer);
    procedure DoOps_LexersDisableInFrames(ListNames: TStringList);
    procedure DoOps_LexersRestoreInFrames(ListNames: TStringList);
    procedure DoOps_LoadOptions_Editor(c: TJSONConfig; var Op: TEditorOps);
    procedure DoOps_LoadOptions_Global(c: TJSONConfig);
    procedure DoOps_LoadOptions_Ui(c: TJSONConfig);
    procedure DoOps_LoadOptions_UiAutoCompletion(c: TJSONConfig);
    procedure DoOps_MultiInstaller;
    procedure DoOps_OnCreate;
    procedure DoShowBottomPanel(const ATabCaption: string; AndFocus: boolean);
    function DoSidebar_FilenameToImageIndex(ATabCaption, AFilename: string): integer;
    procedure DoSidebar_InitPanelForm(AItem: TAppSidePanel; const ACaption: string; AForm: TCustomForm;
      AParent: TWinControl);
    procedure DoSidebar_ListboxDrawItem(Sender: TObject; C: TCanvas;
      AIndex: integer; const ARect: TRect);
    procedure DoSidebar_MainMenuClick(Sender: TObject);
    function DoSidebar_TranslatedCaption(const ACaption: string): string;
    function FindFrameOfFilename(const AName: string): TEditorFrame;
    procedure FixMainLayout;
    procedure FormFloatBottomOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups1_OnEmpty(Sender: TObject);
    procedure FormFloatGroups2_OnEmpty(Sender: TObject);
    procedure FormFloatGroups3_OnEmpty(Sender: TObject);
    procedure FormFloatSideOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups1_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups2_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormFloatGroups3_OnClose(Sender: TObject; var CloseAction: TCloseAction);
    function GetSessionFilename: string;
    procedure CharmapOnInsert(const AStr: string);
    procedure DoLocalize;
    procedure DoLocalize_FormFind;
    procedure DoLocalize_FormGoto;
    function DoCheckFilenameOpened(const AName: string): boolean;
    procedure DoInvalidateEditors;
    function DoMenuAdd_Params(
      const AMenuId, AMenuCmd, AMenuCaption, AMenuHotkey, AMenuTagString: string;
      AIndex: integer): string;
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
    procedure DoCodetree_GetSyntaxRange(ANode: TTreeNode; out APosBegin, APosEnd: TPoint);
    procedure DoCodetree_SetSyntaxRange(ANode: TTreeNode; const APosBegin, APosEnd: TPoint);
    procedure DoCodetree_OnDblClick(Sender: TObject);
    procedure DoCodetree_OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoCodetree_OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoCodetree_GotoBlockForCurrentNode(AndSelect: boolean);
    procedure DoCodetree_ApplyTreeHelperResults(Data: PPyObject);
    procedure DoSidebar_OnTabClick(Sender: TObject);
    function DoSidebar_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
    function DoSidebar_AddTab(const ACaption: string;
      AImageIndex: integer; AHandle: PtrInt): boolean;
    function DoSidebar_AddTab_Empty(const ACaption: string;
      AImageIndex: integer; const AModule, AMethod: string): boolean;
    function DoSidebar_RemoveTab(const ACaption: string): boolean;
    function DoSidebar_CaptionToPanelsIndex(const ACaption: string): integer;
    function DoSidebar_CaptionToTabIndex(const Str: string): integer;
    function DoSidebar_CaptionToControlHandle(const ACaption: string): PtrInt;
    procedure DoSidebar_FocusCodetreeFilter;
    procedure DoSidebar_FocusCodetree;
    procedure DoBottom_OnTabClick(Sender: TObject);
    procedure DoBottom_AddonsClick(Sender: TObject);
    procedure DoBottom_FindClick(Sender: TObject);
    function DoBottom_CaptionToControlHandle(const ACaption: string): PtrInt;
    function DoBottom_AddTab(const ACaption: string;
      AImageIndex: integer; AHandle: PtrInt): boolean;
    function DoBottom_CaptionToPanelsIndex(const ACaption: string): integer;
    function DoBottom_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
    function DoBottom_CaptionToTabIndex(const ACaption: string): integer;
    function DoBottom_RemoveTab(const ACaption: string): boolean;
    procedure DoAutoComplete;
    procedure DoCudaLibAction(const AMethod: string);
    procedure DoDialogCharMap;
    procedure DoFindActionFromString(AStr: string);
    procedure DoFindOptionsFromString(const S: string);
    function DoFindOptionsToString: string;
    procedure DoGotoFromInput(const AInput: string);
    procedure DoGotoDefinition;
    procedure DoShowFuncHint;
    procedure DoApplyGutterVisible(AValue: boolean);
    procedure DoApplyFrameOps(F: TEditorFrame; const Op: TEditorOps; AForceApply: boolean);
    procedure DoApplyFont_Text;
    procedure DoApplyFont_Ui;
    procedure DoApplyFont_Output;
    procedure DoApplyAllOps;
    procedure DoApplyTheme;
    procedure DoApplyThemeToGroups(G: TATGroups);
    procedure DoClearRecentFileHistory;
    function DoOnConsoleNav(const Str: string): boolean;
    function DoOnMacro(const Str: string): boolean;
    function DoDialogConfigTheme(var AData: TAppTheme; AThemeUI: boolean): boolean;
    function DoDialogMenuApi(const AText, ACaption: string; AMultiline: boolean;
      AInitIndex: integer; ANoFuzzy, ANoFullFilter, AShowCentered: boolean): integer;
    procedure DoDialogMenuTranslations;
    procedure DoDialogMenuThemes(AThemeUI: boolean);
    procedure DoFileExportHtml;
    function DoFileInstallZip(const fn: string; out DirTarget: string; ASilent: boolean): boolean;
    procedure DoFileCloseAndDelete;
    procedure DoFileNew;
    procedure DoFileNewMenu(Sender: TObject);
    procedure DoFileNewFrom(const fn: string);
    procedure DoFileSave;
    procedure DoFileSaveAs;
    procedure DoFocusEditor;
    procedure DoSwitchTab(ANext: boolean);
    procedure DoSwitchTabSimply(ANext: boolean);
    procedure DoSwitchTabToRecent;
    procedure DoPyTimerTick(Sender: TObject);
    procedure DoPyRunLastPlugin;
    procedure DoPyResetPlugins;
    procedure DoPyRescanPlugins;
    procedure DoPyStringToEvents(const AEventStr: string;
      out AEvents: TAppPyEvents;
      out AEventsPrior: TAppPyEventsPrior;
      out AEventsLazy: TAppPyEventsLazy);
    procedure DoPyUpdateEvents(const AModuleName, AEventStr, ALexerStr, AKeyStr: string);
    function DoSplitter_StringToId(const AStr: string): integer;
    procedure DoSplitter_GetInfo(const Id: integer;
      out BoolVert, BoolVisible: boolean; out NPos, NTotal: integer);
    procedure DoSplitter_SetInfo(const Id: integer; NPos: integer);
    procedure DoToolbarClick(Sender: TObject);
    procedure FrameLexerChange(Sender: TObject);
    procedure FrameOnEditorClickEndSelect(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure FrameOnEditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    function GetFloatBottom: boolean;
    function GetFloatSide: boolean;
    function GetFloatGroups: boolean;
    function GetShowFloatGroup1: boolean;
    function GetShowFloatGroup2: boolean;
    function GetShowFloatGroup3: boolean;
    function GetShowOnTop: boolean;
    function GetShowSidebarOnRight: boolean;
    procedure InitAppleMenu;
    procedure InitFloatGroup(var F: TForm; var G: TATGroups; ATag: integer;
      const ARect: TRect; AOnClose: TCloseEvent; AOnGroupEmpty: TNotifyEvent);
    procedure InitFloatGroups;
    procedure InitSidebar;
    procedure InitToolbar;
    function IsWindowMaximizedOrFullscreen: boolean;
    function IsAllowedToOpenFileNow: boolean;
    function IsThemeNameExist(const AName: string; AThemeUI: boolean): boolean;
    procedure LiteLexer_ApplyStyle(Sender: TObject; AStyleHash: integer; var APart: TATLinePart);
    function LiteLexer_GetStyleHash(Sender: TObject; const AStyleName: string): integer;
    procedure MenuEncWithReloadClick(Sender: TObject);
    procedure MenuitemClick_CommandFromTag(Sender: TObject);
    procedure MenuitemClick_CommandFromHint(Sender: TObject);
    procedure MenuPicScaleClick(Sender: TObject);
    procedure MenuPluginClick(Sender: TObject);
    procedure MenuTabsizeClick(Sender: TObject);
    procedure MsgLogConsole(const AText: string);
    procedure MsgLogDebug(const AText: string);
    procedure MsgLogToFilename(const AText, AFilename: string; AWithTime: boolean);
    procedure MsgStatusAlt(const AText: string; ASeconds: integer);
    function GetStatusbarPrefix(Frame: TEditorFrame): string;
    procedure MsgStatusFileOpened(const AFileName1, AFileName2: string);
    procedure PopupListboxOutputCopyClick(Sender: TObject);
    procedure PopupListboxOutputClearClick(Sender: TObject);
    procedure PopupListboxValidateClearClick(Sender: TObject);
    procedure PopupListboxValidateCopyClick(Sender: TObject);
    procedure SetFloatBottom(AValue: boolean);
    procedure SetFloatSide(AValue: boolean);
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
    procedure DoOps_LoadPluginFromInf(const fn_inf: string);
    procedure DoOps_LoadSidebarIcons;
    procedure DoOps_LoadTreeIcons;
    procedure DoOps_LoadToolBarIcons;
    procedure DoOps_LoadCommandLineOptions;
    procedure DoOps_LoadLexerLib;
    procedure DoOps_SaveHistory;
    procedure DoOps_SaveHistory_GroupView(c: TJsonConfig);
    procedure DoOps_SaveOptionBool(const APath: string; AValue: boolean);
    procedure DoOps_LoadHistory;
    procedure DoOps_LoadHistory_GroupView(c: TJsonConfig);
    function DoOps_SaveSession(fn_session: string): boolean;
    function DoOps_LoadSession(fn_session: string): boolean;
    procedure DoOps_LoadOptionsAndApplyAll;
    procedure DoOps_LoadOptionsLexerSpecific(F: TEditorFrame);
    procedure DoOps_OpenFile_LexerSpecific;
    procedure DoOps_LoadPlugins;
    procedure DoOps_DialogFont(var OpName: string; var OpSize: integer;
      const AConfigStrName, AConfigStrSize: string);
    procedure DoOps_DialogFont_Text;
    procedure DoOps_DialogFont_Ui;
    procedure DoOps_DialogFont_Output;
    procedure DoOps_FontSizeChange(AIncrement: integer);
    procedure DoOps_OpenFile_Default;
    procedure DoOps_OpenFile_User;
    procedure DoOps_OpenFile_DefaultAndUser;
    procedure DoOps_LoadOptions(const fn: string; var Op: TEditorOps;
      AllowUiOps: boolean=true; AllowGlobalOps: boolean=true);
    procedure DoOps_LoadOptionsFromString(const AString: string);
    procedure DoOps_LoadKeymap;
    procedure DoOps_LoadKeymapFrom(const AFilenameKeymap: string; AUndoList: TATKeymapUndoList);
    procedure DoEditorsLock(ALock: boolean);
    procedure DoFindCurrentWordOrSel(ANext: boolean; AWordOrSel: boolean);
    procedure DoFind_ExpandSelectionToWord;
    procedure DoCopyFilenameDir;
    procedure DoCopyFilenameFull;
    procedure DoCopyFilenameName;
    procedure DoCopyLine;
    procedure DoDialogCommands;
    function DoDialogCommands_Custom(AShowUsual, AShowPlugins, AShowLexers,
      AAllowConfig, AShowCentered: boolean; ACaption: string;
  AFocusedCommand: integer): integer;
    function DoDialogCommands_Py(AShowUsual, AShowPlugins, AShowLexers,
      AAllowConfig, AShowCentered: boolean; ACaption: string): string;
    procedure DoDialogGoto;
    function DoDialogMenuList(const ACaption: string; AItems: TStringList; ACloseOnCtrlRelease: boolean=
      false): integer;
    procedure DoDialogMenuTabSwitcher;
    procedure DoDialogGotoBookmark;
    function DoDialogSaveTabs: boolean;
    procedure DoDialogLexerProp(an: TecSyntAnalyzer);
    procedure DoDialogLexerLib;
    procedure DoDialogLexerMap;
    procedure DoDialogTheme(AThemeUI: boolean);
    procedure DoShowConsole(AndFocus: boolean);
    procedure DoShowOutput(AndFocus: boolean);
    procedure DoShowValidate(AndFocus: boolean);
    procedure DoShowSidePanel(const ATabCaption: string; AndFocus: boolean);
    function FrameOfPopup: TEditorFrame;
    procedure FrameOnCommand(Sender: TObject; ACommand: integer; const AText: string;
      var AHandled: boolean);
    function DoFileCloseAll(AWithCancel: boolean): boolean;
    procedure DoDialogFind(AReplaceMode: boolean);
    procedure DoDialogFind_Hide;
    procedure DoFindResult(ok: boolean);
    procedure DoFindFirst;
    procedure DoFindNext(ANext: boolean);
    procedure DoFindMarkAll(AMode: TATFindMarkingMode);
    procedure DoMoveTabToGroup(AGroupIndex: Integer);
    function DoFileOpen(AFileName, AFileName2: string; APages: TATPages=nil; const AOptions: string=''): TEditorFrame;
    procedure DoFileOpenDialog(AOptions: string= '');
    procedure DoFileOpenDialog_NoPlugins;
    function DoFileSaveAll: boolean;
    procedure DoFileReopen;
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
    procedure FindDialogDone(Sender: TObject; const Res: string);
    procedure FinderOnGetToken(Sender: TObject; AX, AY: integer; out AKind: TATFinderTokenKind);
    procedure FinderOnFound(Sender: TObject; APos1, APos2: TPoint);
    procedure FinderOnBadRegex(Sender: TObject);
    procedure FinderOnConfirmReplace(Sender: TObject; APos1, APos2: TPoint;
      AForMany: boolean; var AConfirm, AContinue: boolean);
    procedure FinderOnProgress(Sender: TObject; const ACurPos, AMaxPos: Int64; var AContinue: boolean);
    procedure FinderUpdateEditor(AUpdateText: boolean);
    procedure FrameOnSaveFile(Sender: TObject);
    procedure GetEditorIndexes(Ed: TATSynEdit; out AGroupIndex, ATabIndex: Integer);
    function GetModifiedCount: integer;
    function GetShowSideBar: boolean;
    function GetShowSidePanel: boolean;
    function GetShowStatus: boolean;
    function GetShowToolbar: boolean;
    function GetShowBottom: boolean;
    function GetShowTabsMain: boolean;
    procedure InitFormFind;
    function IsFocusedBottom: boolean;
    function IsFocusedFind: boolean;
    procedure PyCompletionOnGetProp(Sender: TObject; out AText: string;
      out ACharsLeft, ACharsRight: integer);
    procedure PyCompletionOnResult(Sender: TObject;
      const ASnippetId: string; ASnippetIndex: integer);
    procedure DoPyCommand_ByPluginIndex(AIndex: integer);
    procedure SetFrameEncoding(Frame: TEditorFrame; const AEnc: string;
      AAlsoReloadFile: boolean);
    procedure SetLexerIndex(AIndex: integer);
    procedure SetShowStatus(AValue: boolean);
    procedure SetShowToolbar(AValue: boolean);
    procedure SetShowBottom(AValue: boolean);
    procedure SetShowSideBar(AValue: boolean);
    procedure SetShowSidePanel(AValue: boolean);
    procedure SetShowTabsMain(AValue: boolean);
    procedure SplitterOnPaint_Gr(Sender: TObject);
    procedure SplitterOnPaint_Main(Sender: TObject);
    procedure StopAllTimers;
    procedure UpdateSidebarButtonOverlay;
    procedure UpdateBottomPanels(const ACaption: string; AndFocus: boolean);
    procedure UpdateEditorTabsize(AValue: integer);
    procedure UpdateKeymapDynamicItems;
    procedure UpdateMenuItemAltObject(mi: TMenuItem; cmd: integer);
    procedure UpdateMenuItemChecked(mi: TMenuItem; saved: TATMenuItemsAlt; AValue: boolean);
    procedure UpdateMenuItemHint(mi: TMenuItem; const AHint: string);
    procedure UpdateMenuItemHotkey(mi: TMenuItem; cmd: integer);
    procedure UpdateMenuPicScale;
    procedure UpdateMenuTabsize;
    procedure UpdateMenuLexersTo(AMenu: TMenuItem);
    procedure UpdateMenuRecent(F: TEditorFrame; const AFileName: string);
    procedure UpdateMenuHotkeys;
    procedure UpdateMenuLexers;
    procedure UpdateMenuPlugins;
    procedure UpdateMenuPlugins_Shortcuts(AForceUpdate: boolean=false);
    procedure UpdateMenuChecks;
    procedure UpdateMenuEnc(AMenu: TMenuItem);
    procedure UpdateBottomLayout(ASetFloating: boolean);
    procedure DoApplyUiOps;
    procedure DoApplyUiOpsToGroups(G: TATGroups);
    procedure DoApplyInitialGroupSizes;
    procedure DoApplyInitialSidebarPanel;
    procedure DoApplyInitialWindowPos;
    procedure InitPyEngine;
    procedure FrameOnChangeCaption(Sender: TObject);
    procedure FrameOnUpdateStatus(Sender: TObject);
    function DoTabAdd(APages: TATPages; const ACaption: string;
      AndActivate: boolean=true;
      AAllowNearCurrent: boolean=true): TATTabData;
    procedure FrameOnFocus(Sender: TObject);
    function GetFrame(AIndex: integer): TEditorFrame;
    procedure MenuEncNoReloadClick(Sender: TObject);
    procedure MenuLexerClick(Sender: TObject);
    procedure MenuMainClick(Sender: TObject);
    procedure MenuRecentsClick(Sender: TObject);
    procedure SetFrame(Frame: TEditorFrame);
    procedure UpdateFrameLineEnds(Frame: TEditorFrame; AValue: TATLineEnds);
    procedure MsgStatus(AText: string);
    procedure UpdateSidebarButtons;
    procedure UpdateSidebarPanels(const ACaption: string; AndFocus: boolean);
    procedure UpdateStatusbarPanelsFromString(AStr: string);
    procedure UpdateStatusbarHints;
    procedure UpdateBottomButtons;
    procedure UpdateStatus_ForFrame(AStatus: TATStatus; F: TEditorFrame);
    procedure UpdateStatus_RealWork;
    procedure UpdateStatus_ToolButton(AToolbar: TATFlatToolbar; ACmd: integer;
      AChecked, AEnabled: boolean);
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
    PanelCodeTreeTop: TATPanelSimple;
    ListboxOut: TATListbox;
    ListboxVal: TATListbox;
    LexerProgress: TATGauge;
    function FrameCount: integer;
    property Frames[N: integer]: TEditorFrame read GetFrame;
    function CurrentGroups: TATGroups;
    function CurrentFrame: TEditorFrame;
    function CurrentEditor: TATSynEdit;
    property FloatSide: boolean read GetFloatSide write SetFloatSide;
    property FloatBottom: boolean read GetFloatBottom write SetFloatBottom;
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
    property ShowSidePanel: boolean read GetShowSidePanel write SetShowSidePanel;
    property ShowToolbar: boolean read GetShowToolbar write SetShowToolbar;
    property ShowStatus: boolean read GetShowStatus write SetShowStatus;
    property ShowBottom: boolean read GetShowBottom write SetShowBottom;
    property ShowTabsMain: boolean read GetShowTabsMain write SetShowTabsMain;
    property ThemeUi: string read FThemeUi write SetThemeUi;
    property ThemeSyntax: string read FThemeSyntax write SetThemeSyntax;
    property SidebarPanel: string read FLastSidebarPanel write SetSidebarPanel;
    function DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: array of string): string;
    procedure DoPyCommand(const AModule, AMethod: string; const AParams: array of string);
    function DoPyTreeHelper(Frame: TEditorFrame): boolean;
    //function DoPyCallbackFromAPI(const ACallback: string; const AParams: array of string): string;
  end;

var
  fmMain: TfmMain;

var
  NTickInitial: QWord = 0;


implementation

uses
  Emmet,
  EmmetHelper;

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

{$R *.lfm}

function GetEditorFrame(Ed: TATSynEdit): TEditorFrame;
begin
  if Ed.Parent is TEditorFrame then
    Result:= Ed.Parent as TEditorFrame
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


{ TfmMain }

{$I formmain_py_toolbars.inc}
{$I formmain_py_statusbars.inc}
{$I formmain_py_api.inc}
{$I formmain_py_helpers.inc}
{$I formmain_py_pluginwork.inc}


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
          PopupPicScale.Popup;
        end;
    end;
    exit;
  end;

  if Frame.IsBinary then
  begin
    case Data.Tag of
      StatusbarTag_Enc:
        with Mouse.CursorPos do
          Frame.Binary.TextEncodingsMenu(X, Y);
    end;
    exit;
  end;

  case Data.Tag of
    StatusbarTag_Enc:
      begin
        if not Frame.ReadOnly[Frame.Editor] then
          PopupEnc.PopUp;
      end;
    StatusbarTag_LineEnds:
      begin
        if not Frame.ReadOnly[Frame.Editor] then
          PopupEnds.PopUp;
      end;
    StatusbarTag_Lexer:
      begin
        PopupLex.PopUp;
      end;
    StatusbarTag_TabSize:
      begin
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
  S: string;
  NCnt, i: integer;
begin
  if not FConsoleQueue.IsEmpty() then
  begin
    //avoid output of huge items count at once
    NCnt:= 0;
    while not FConsoleQueue.IsEmpty() and (NCnt<500) do
    begin
      S:= FConsoleQueue.Front();
      FConsoleQueue.Pop();
      fmConsole.DoAddLine(S);
      if UiOps.LogConsole then
        MsgLogToFilename(S, FFileNameLogConsole, false);
      Inc(NCnt);
    end;

    fmConsole.DoUpdate;
  end;

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
        DoPyEvent(Ed, cEventOnMouseStop, [
          IntToStr(PntLocal.X),
          IntToStr(PntLocal.Y)
          ]);
        Break;
      end;
    end;
  end;
end;

procedure TfmMain.TimerStatusTimer(Sender: TObject);
begin
  MsgStatus('');
  TimerStatus.Enabled:= false;
end;

procedure TfmMain.TimerStatusAltTimer(Sender: TObject);
begin
  TimerStatusAlt.Enabled:= false;
  StatusAlt.Hide;
end;

procedure TfmMain.TimerStatusBusyTimer(Sender: TObject);
begin
  TimerStatusBusy.Enabled:= false;
  UpdateStatus_RealWork;
end;

procedure TfmMain.TimerTreeFillTimer(Sender: TObject);
begin
  TimerTreeFill.Enabled:= false;
  UpdateTree(true);
end;

procedure TfmMain.TimerEdCaretTimer(Sender: TObject);
begin
  TimerEdCaret.Enabled:= false;
  UpdateTree(false);
end;

procedure TfmMain.DoCodetree_OnDblClick(Sender: TObject);
var
  PntBegin, PntEnd: TPoint;
begin
  DoCodetree_GetSyntaxRange(CodeTree.Tree.Selected, PntBegin, PntEnd);

  FCodetreeDblClicking:= true;
  CurrentEditor.DoGotoPos(
    PntBegin,
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );
  DoFocusEditor;
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

procedure TfmMain.UniqInstanceOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
var
  SFilename: string;
  Frame: TEditorFrame;
  NLine, NColumn, i: integer;
begin
  if not IsAllowedToOpenFileNow then exit;

  for i:= 0 to ParamCount-1 do
  begin
    SFilename:= Parameters[i];
    SParseFilenameWithTwoNumbers(SFilename, NLine, NColumn);
    if DirectoryExistsUTF8(SFilename) then
    begin
      DoFolderOpen(SFilename, True);
    end
    else
    if FileExistsUTF8(SFilename) then
    begin
      Frame:= DoFileOpen(SFilename, '');
      if Assigned(Frame) and (NLine>0) then
        Frame.DoGotoPos(Frame.Editor, NColumn-1, NLine-1);
    end;
  end;

  if WindowState=wsMinimized then
  begin
    WindowState:= wsNormal;
    Application.ProcessMessages;
  end;

  {$ifdef windows}
  // Those two calls below conflicts with Windows SwitchToThisWindow API call
  // so they are left for other platforms
  {$else}
  Application.BringToFront;
  DoFocusWindow(Handle);
  {$ifend}
end;

{$ifdef windows}
procedure TfmMain.SecondInstance(const Msg: TBytes);
var
  SFilename: String;
  params: TStringList;
  Frame: TEditorFrame;
  NLine, NColumn, i: integer;
begin
  if not IsAllowedToOpenFileNow then Exit;

  params := TStringList.Create;
  try
    params.StrictDelimiter := True;
    params.Delimiter := '|';
    params.DelimitedText := UTF8Encode(TEncoding.UTF8.GetString(Msg));
    for i := 0 to params.Count - 1 do
    begin
      SFilename := params[i];
      if SFilename='' then Continue;
      SParseFilenameWithTwoNumbers(SFilename, NLine, NColumn);
      //if dir, open in ProjManager
      if DirectoryExistsUTF8(SFilename) then
      begin
        DoFolderOpen(SFilename, False);
      end
      else
      if FileExistsUTF8(SFilename) then
      begin
        Frame:= DoFileOpen(SFilename, '');
        if Assigned(Frame) and (NLine>0) then
          Frame.DoGotoPos(Frame.Ed1, NColumn-1, NLine-1);
      end;
    end;
  finally
    params.Free;
  end;

  if WindowState = wsMinimized then
  begin
    WindowState := wsNormal;
    Application.ProcessMessages;
  end;
end;
{$endif}

function TfmMain.GetSessionFilename: string;
begin
  Result:= FSessionName;
  if Result='' then
    Result:= 'history session.json';
  if ExtractFileDir(Result)='' then
    Result:= GetAppPath(cDirSettings)+DirectorySeparator+Result;
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

  mnuApple_CheckUpd:= TMenuItem.Create(Self);
  mnuApple_CheckUpd.Caption:= 'Check for updates';
  mnuApple.Add(mnuApple_CheckUpd);

  //"Check for Updates" sopported only on Windows
  mnuHelpCheckUpd.Visible:= false;
  mnuApple_CheckUpd.Enabled:= false;

  //macOS adds "Quit" item in Apple menu, "Exit" not needed
  mnuFileExit.Visible:= false;
end;


procedure TfmMain.FormCreate(Sender: TObject);
var
  Panel: TAppSidePanel;
  mi: TMenuItem;
  i: integer;
begin
  //"Check for Updates" sopported only on Windows
  {$ifndef windows}
  mnuHelpCheckUpd.Enabled:= false;
  {$endif}

  LexerProgress:= TATGauge.Create(Self);
  LexerProgress.Parent:= Status;

  OnLexerParseProgress:= @DoOnLexerParseProgress;
  CustomDialog_DoPyCallback:= @DoPyCallbackFromAPI;
  FFileNameLogDebug:= GetAppPath(cDirSettings)+DirectorySeparator+'app.log';
  FFileNameLogConsole:= GetAppPath(cDirSettings)+DirectorySeparator+'console.log';

  DoMenuitemEllipsis(mnuOpThemeUi);
  DoMenuitemEllipsis(mnuOpThemeSyntax);
  DoMenuitemEllipsis(mnuOpKeys);
  DoMenuitemEllipsis(mnuOpThemesUi);
  DoMenuitemEllipsis(mnuOpThemesSyntax);
  DoMenuitemEllipsis(mnuOpLangs);

  mnuToolbarCaseLow:= TMenuItem.Create(Self);
  mnuToolbarCaseLow.Caption:= mnuCaseLow.Caption;
  mnuToolbarCaseLow.Tag:= cCommand_TextCaseLower;
  mnuToolbarCaseLow.OnClick:= @MenuitemClick_CommandFromTag;
  mnuToolbarCaseUp:= TMenuItem.Create(Self);
  mnuToolbarCaseUp.Caption:= mnuCaseUp.Caption;
  mnuToolbarCaseUp.Tag:= cCommand_TextCaseUpper;
  mnuToolbarCaseUp.OnClick:= @MenuitemClick_CommandFromTag;
  mnuToolbarCaseTitle:= TMenuItem.Create(Self);
  mnuToolbarCaseTitle.Caption:= mnuCaseTitle.Caption;
  mnuToolbarCaseTitle.Tag:= cCommand_TextCaseTitle;
  mnuToolbarCaseTitle.OnClick:= @MenuitemClick_CommandFromTag;
  mnuToolbarCaseInvert:= TMenuItem.Create(Self);
  mnuToolbarCaseInvert.Caption:= mnuCaseInvert.Caption;
  mnuToolbarCaseInvert.Tag:= cCommand_TextCaseInvert;
  mnuToolbarCaseInvert.OnClick:= @MenuitemClick_CommandFromTag;
  mnuToolbarCaseSent:= TMenuItem.Create(Self);
  mnuToolbarCaseSent.Caption:= mnuCaseSent.Caption;
  mnuToolbarCaseSent.Tag:= cCommand_TextCaseSentence;
  mnuToolbarCaseSent.OnClick:= @MenuitemClick_CommandFromTag;

  PopupToolbarCase:= TPopupMenu.Create(Self);
  PopupToolbarCase.Items.Add(mnuToolbarCaseUp);
  PopupToolbarCase.Items.Add(mnuToolbarCaseLow);
  PopupToolbarCase.Items.Add(mnuToolbarCaseTitle);
  PopupToolbarCase.Items.Add(mnuToolbarCaseInvert);
  PopupToolbarCase.Items.Add(mnuToolbarCaseSent);

  mnuToolbarCommentLineToggle:= TMenuItem.Create(Self);
  mnuToolbarCommentLineToggle.Caption:= 'Line comment: toggle';
  mnuToolbarCommentLineToggle.Hint:= 'cuda_comments,cmt_toggle_line_body';
  mnuToolbarCommentLineToggle.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarCommentLineAdd:= TMenuItem.Create(Self);
  mnuToolbarCommentLineAdd.Caption:= 'Line comment: add';
  mnuToolbarCommentLineAdd.Hint:= 'cuda_comments,cmt_add_line_body';
  mnuToolbarCommentLineAdd.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarCommentLineDel:= TMenuItem.Create(Self);
  mnuToolbarCommentLineDel.Caption:= 'Line comment: remove';
  mnuToolbarCommentLineDel.Hint:= 'cuda_comments,cmt_del_line';
  mnuToolbarCommentLineDel.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarCommentStream:= TMenuItem.Create(Self);
  mnuToolbarCommentStream.Caption:= 'Stream comment: toggle';
  mnuToolbarCommentStream.Hint:= 'cuda_comments,cmt_toggle_stream';
  mnuToolbarCommentStream.OnClick:= @MenuitemClick_CommandFromHint;

  PopupToolbarComment:= TPopupMenu.Create(Self);
  PopupToolbarComment.Items.Add(mnuToolbarCommentLineToggle);
  PopupToolbarComment.Items.Add(mnuToolbarCommentLineAdd);
  PopupToolbarComment.Items.Add(mnuToolbarCommentLineDel);
  PopupToolbarComment.Items.Add(mnuToolbarCommentStream);

  mnuToolbarSortAsc:= TMenuItem.Create(Self);
  mnuToolbarSortAsc.Caption:= 'Sort ascending';
  mnuToolbarSortAsc.Hint:= 'cuda_sort,sort_asc';
  mnuToolbarSortAsc.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarSortDesc:= TMenuItem.Create(Self);
  mnuToolbarSortDesc.Caption:= 'Sort descending';
  mnuToolbarSortDesc.Hint:= 'cuda_sort,sort_desc';
  mnuToolbarSortDesc.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarSortAscNocase:= TMenuItem.Create(Self);
  mnuToolbarSortAscNocase.Caption:= 'Sort ascending, ignore case';
  mnuToolbarSortAscNocase.Hint:= 'cuda_sort,sort_asc_nocase';
  mnuToolbarSortAscNocase.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarSortDescNocase:= TMenuItem.Create(Self);
  mnuToolbarSortDescNocase.Caption:= 'Sort descending, ignore case';
  mnuToolbarSortDescNocase.Hint:= 'cuda_sort,sort_desc_nocase';
  mnuToolbarSortDescNocase.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarSortSep1:= TMenuItem.Create(Self);
  mnuToolbarSortSep1.Caption:= '-';
  mnuToolbarSortDialog:= TMenuItem.Create(Self);
  mnuToolbarSortDialog.Caption:= 'Sort dialog...';
  mnuToolbarSortDialog.Hint:= 'cuda_sort,sort_dlg';
  mnuToolbarSortDialog.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarSortSep2:= TMenuItem.Create(Self);
  mnuToolbarSortSep2.Caption:= '-';
  mnuToolbarSortReverse:= TMenuItem.Create(Self);
  mnuToolbarSortReverse.Caption:= 'Reverse lines';
  mnuToolbarSortReverse.Hint:= 'cuda_sort,reverse';
  mnuToolbarSortReverse.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarSortShuffle:= TMenuItem.Create(Self);
  mnuToolbarSortShuffle.Caption:= 'Shuffle lines';
  mnuToolbarSortShuffle.Hint:= 'cuda_sort,shuffle';
  mnuToolbarSortShuffle.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarSortRemoveDup:= TMenuItem.Create(Self);
  mnuToolbarSortRemoveDup.Caption:= 'Remove duplicate lines';
  mnuToolbarSortRemoveDup.Hint:= 'cuda_sort,del_dup';
  mnuToolbarSortRemoveDup.OnClick:= @MenuitemClick_CommandFromHint;
  mnuToolbarSortRemoveBlank:= TMenuItem.Create(Self);
  mnuToolbarSortRemoveBlank.Caption:= 'Remove blank lines';
  mnuToolbarSortRemoveBlank.Hint:= 'cuda_sort,del_blank';
  mnuToolbarSortRemoveBlank.OnClick:= @MenuitemClick_CommandFromHint;

  PopupToolbarSort:= TPopupMenu.Create(Self);
  PopupToolbarSort.Items.Add(mnuToolbarSortAsc);
  PopupToolbarSort.Items.Add(mnuToolbarSortDesc);
  PopupToolbarSort.Items.Add(mnuToolbarSortAscNocase);
  PopupToolbarSort.Items.Add(mnuToolbarSortDescNocase);
  PopupToolbarSort.Items.Add(mnuToolbarSortSep1);
  PopupToolbarSort.Items.Add(mnuToolbarSortDialog);
  PopupToolbarSort.Items.Add(mnuToolbarSortSep2);
  PopupToolbarSort.Items.Add(mnuToolbarSortReverse);
  PopupToolbarSort.Items.Add(mnuToolbarSortShuffle);
  PopupToolbarSort.Items.Add(mnuToolbarSortRemoveDup);
  PopupToolbarSort.Items.Add(mnuToolbarSortRemoveBlank);

  {$ifdef windows}
  if IsSetToOneInstance then
  begin
    FInstanceManage := TInstanceManage.Create(AppUniqueUID);
    FInstanceManage.Check;
    case FInstanceManage.Status of
      isFirst:
        begin
          FInstanceManage.SetFormHandleForActivate(Self.Handle);
          FInstanceManage.OnSecondInstanceStarted := @SecondInstance;
        end;
    end;
  end;
  {$endif}

  FBoundsFloatSide:= Rect(650, 50, 900, 700);
  FBoundsFloatBottom:= Rect(50, 480, 900, 700);
  FBoundsFloatGroups1:= Rect(300, 100, 800, 700);
  FBoundsFloatGroups2:= Rect(320, 120, 820, 720);
  FBoundsFloatGroups3:= Rect(340, 140, 840, 740);

  InitAppleMenu;
  InitToolbar;
  InitSidebar;

  CodeTree:= TAppTreeContainer.Create(Self);
  CodeTree.Parent:= PanelLeft;
  CodeTree.Align:= alClient;
  CodeTree.Tree.Images:= ImageListTree;
  CodeTree.Themed:= true;
  CodeTree.Tree.OnDblClick:= @DoCodetree_OnDblClick;
  CodeTree.Tree.OnMouseMove:= @DoCodetree_OnMouseMove;
  CodeTree.Tree.OnKeyDown:= @DoCodetree_OnKeyDown;
  CodeTree.Tree.PopupMenu:= PopupTree;

  PanelCodeTreeTop:= TATPanelSimple.Create(Self);
  PanelCodeTreeTop.Parent:= PanelLeft;
  PanelCodeTreeTop.Align:= alTop;
  PanelCodeTreeTop.Top:= PanelLeftTitle.Height; //fix pos relative to title
  PanelCodeTreeTop.Height:= UiOps.InputHeight;

  CodeTreeFilter:= TTreeFilterEdit.Create(Self);
  CodeTreeFilter.Hide;

  CodeTreeFilterReset:= TATButton.Create(Self);
  CodeTreeFilterReset.Parent:= PanelCodeTreeTop;
  CodeTreeFilterReset.Align:= alRight;
  CodeTreeFilterReset.Width:= UiOps.ScrollbarWidth;
  CodeTreeFilterReset.Caption:= msgButtonX;
  CodeTreeFilterReset.Focusable:= false;
  CodeTreeFilterReset.Flat:= true;
  CodeTreeFilterReset.ShowHint:= true;
  CodeTreeFilterReset.Hint:= msgTooltipClearFilter;
  CodeTreeFilterReset.OnClick:= @CodeTreeFilter_ResetOnClick;

  CodeTreeFilterInput:= TATComboEdit.Create(Self);
  CodeTreeFilterInput.Parent:= PanelCodeTreeTop;
  CodeTreeFilterInput.Align:= alClient;
  CodeTreeFilterInput.OnChange:= @CodeTreeFilter_OnChange;
  CodeTreeFilterInput.OnCommand:= @CodeTreeFilter_OnCommand;

  FPopupListboxOutput:= TPopupMenu.Create(Self);
  mi:= TMenuItem.Create(Self);
  mi.Caption:= msgFileClearList;
  mi.OnClick:= @PopupListboxOutputClearClick;
  FPopupListboxOutput.Items.Add(mi);
  mi:= TMenuItem.Create(Self);
  mi.Caption:= msgEditCopy;
  mi.OnClick:= @PopupListboxOutputCopyClick;
  FPopupListboxOutput.Items.Add(mi);

  FPopupListboxValidate:= TPopupMenu.Create(Self);
  mi:= TMenuItem.Create(Self);
  mi.Caption:= msgFileClearList;
  mi.OnClick:= @PopupListboxValidateClearClick;
  FPopupListboxValidate.Items.Add(mi);
  mi:= TMenuItem.Create(Self);
  mi.Caption:= msgEditCopy;
  mi.OnClick:= @PopupListboxValidateCopyClick;
  FPopupListboxValidate.Items.Add(mi);

  ListboxOut:= TATListbox.Create(Self);
  ListboxOut.VirtualMode:= false;
  ListboxOut.Parent:= PanelBottom;
  ListboxOut.Align:= alClient;
  ListboxOut.CanGetFocus:= true;
  ListboxOut.OwnerDrawn:= true;
  ListboxOut.PopupMenu:= FPopupListboxOutput;
  ListboxOut.OnDblClick:= @ListboxOutClick;
  ListboxOut.OnDrawItem:= @ListboxOutDrawItem;
  ListboxOut.OnKeyDown:= @ListboxOutKeyDown;

  ListboxVal:= TATListbox.Create(Self);
  ListboxVal.VirtualMode:= false;
  ListboxVal.Parent:= PanelBottom;
  ListboxVal.Align:= alClient;
  ListboxVal.CanGetFocus:= true;
  ListboxVal.OwnerDrawn:= true;
  ListboxVal.PopupMenu:= FPopupListboxValidate;
  ListboxVal.OnDblClick:= @ListboxOutClick;
  ListboxVal.OnDrawItem:= @ListboxOutDrawItem;
  ListboxVal.OnKeyDown:= @ListboxOutKeyDown;

  AppBookmarkImagelist.AddImages(ImageListBm);
  for i:= 2 to 9 do
  begin
    AppBookmarkSetup[i].Color:= clDefault;
    AppBookmarkSetup[i].ImageIndex:= i-1;
  end;

  PanelAll.Align:= alClient;
  PaintTest.Height:= 150;

  AppManager:= TecLexerList.Create(Self);
  AppManagerLite:= TATLiteLexers.Create(Self);
  AppManagerLite.OnGetStyleHash:= @LiteLexer_GetStyleHash;
  AppManagerLite.OnApplyStyle:= @LiteLexer_ApplyStyle;

  FMenuVisible:= true;
  FSessionName:= '';
  FListRecents:= TStringList.Create;
  FListLangs:= TStringList.Create;
  FListTimers:= TStringList.Create;
  FConsoleQueue:= TAppConsoleQueue.Create;

  FKeymapUndoList:= TATKeymapUndoList.Create;
  FKeymapLastLexer:= '??'; //not ''
  FAllowLoadKeymap:= false;
  FAllowOnFocus:= false;

  FillChar(AppPanelProp_Out, SizeOf(AppPanelProp_Out), 0);
  FillChar(AppPanelProp_Val, SizeOf(AppPanelProp_Val), 0);
  AppPanelProp_Out.Listbox:= ListboxOut;
  AppPanelProp_Val.Listbox:= ListboxVal;

  Status:= TATStatus.Create(Self);
  Status.Parent:= Self;
  Status.Align:= alBottom;
  Status.Top:= Height;
  Status.Height:= UiOps.StatusHeight;
  Status.HeightInitial:= Status.Height;
  Status.Padding:= 2;
  Status.OnPanelClick:= @StatusPanelClick;
  Status.ShowHint:= true;

  StatusAlt:= TATStatus.Create(Self);
  StatusAlt.Parent:= Self;
  StatusAlt.Align:= alBottom;
  StatusAlt.Height:= Status.Height;
  StatusAlt.HeightInitial:= Status.Height;
  StatusAlt.Padding:= 0;
  StatusAlt.AddPanel(-1, 5000, taLeftJustify, '?');
  StatusAlt.Hide;

  fmConsole:= TfmConsole.Create(Self);
  fmConsole.Parent:= PanelBottom;
  fmConsole.Align:= alClient;
  fmConsole.OnConsoleNav:= @DoOnConsoleNav;

  fmGoto:= TfmGoto.Create(Self);

  ListboxOut.Align:= alClient;
  ListboxVal.Align:= alClient;

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

  Panel:= TAppSidePanel.Create;
  Panel.ItemCaption:= msgPanelTree_Init;
  Panel.ItemControl:= CodeTree;
  AppSidePanels.Add(Panel);

  FFinder:= TATEditorFinder.Create;
  FFinder.OptRegex:= true;
  FFinder.OnConfirmReplace:= @FinderOnConfirmReplace;
  FFinder.OnProgress:= @FinderOnProgress;
  FFinder.OnBadRegex:= @FinderOnBadRegex;
  FFinder.OnFound:=@FinderOnFound;
  FFinder.OnGetToken:= @FinderOnGetToken;

  UpdateMenuEnc(PopupEnc.Items);
  UpdateMenuEnc(mnuFileEnc);
  InitStatusbarControls;

  FFindStop:= false;
  FFindConfirmAll:= mrNone;

  Groups.Splitter1.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter2.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter3.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter4.OnPaint:= @SplitterOnPaint_Gr;
  Groups.Splitter5.OnPaint:= @SplitterOnPaint_Gr;
  SplitterVert.OnPaint:= @SplitterOnPaint_Main;
  SplitterHorz.OnPaint:= @SplitterOnPaint_Main;

  FLastDirOfOpenDlg:= '';
  FLastLexerForPluginsMenu:= '-';
  FLastSidebarPanel:= '';
  FLastBottomPanel:= '';

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
  UpdateMenuItemHint(mnuFileEnc, '_enc');
  UpdateMenuItemHint(mnuFileEnds, '_ends');
  UpdateMenuItemHint(mnuLexers, '_lexers');
  UpdateMenuItemHint(mnuOpPlugins, '_oplugins');

  DoOps_OnCreate;

  //option is applied only once at app start
  if not UiOps.ShowMenubar then
    ShowMenu:= false;
end;

procedure TfmMain.DoOps_OnCreate;
begin
  //must load window position in OnCreate to fix flickering with maximized window, Win10
  DoOps_LoadCommandLineOptions;
  DoOps_LoadOptions(GetAppPath(cFileOptionsUser), EditorOps); //before LoadHistory
  DoOps_LoadLexerLib; //before LoadHistory
  DoFileOpen('', ''); //before LoadHistory

  DoOps_LoadSidebarIcons; //before LoadPlugins (for sidebar icons)
  DoOps_LoadTreeIcons;
  DoOps_LoadToolBarIcons;

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
  i: integer;
begin
  //maybe no need too? done in DoCloseAllTabs
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    UpdateMenuRecent(F, F.FileName);
    if not F.EditorsLinked then
      UpdateMenuRecent(F, F.GetFileName(F.Ed2));
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
  DoPyEvent(CurrentEditor, cEventOnExit, []);
end;

procedure TfmMain.ButtonCancelClick(Sender: TObject);
begin
  FFindStop:= true;
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
    DoPyEvent(F.Editor, cEventOnCloseBefore, []);
  end;

  if GetModifiedCount>0 then
    ACanClose:= (UiOps.ReopenSession and UiOps.AutoSaveSession) or DoDialogSaveTabs
  else
    ACanClose:= true;

  if ACanClose then
    StopAllTimers;
end;

procedure TfmMain.StopAllTimers;
begin
  TimerAppIdle.AutoEnabled:=false;
  TimerStatus.Enabled:= false;
  TimerStatusBusy.Enabled:= false;
  TimerStatusAlt.Enabled:= false;
  TimerTreeFill.Enabled:= false;
  TimerEdCaret.Enabled:= false;
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
  //seems gtk2 leaks memory for imagelists, tryin to fix it
  //ImageListBm.Clear;
  //ImageListSide.Clear;
  //ImageListTabs.Clear;
  //ImageListTree.Clear;
  //ImageListToolbar.Clear;

  {$ifdef windows}
  if Assigned(FInstanceManage) then
    FInstanceManage.Free;
  {$ifend}

  for i:= 0 to FListTimers.Count-1 do
    TTimer(FListTimers.Objects[i]).Enabled:= false;
  FreeAndNil(FListTimers);

  FreeAndNil(FListRecents);
  FreeAndNil(FListLangs);
  FreeAndNil(FConsoleQueue);
  FreeAndNil(FKeymapUndoList);
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
  if not FHandledOnShow then
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

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  bEditorActive,
  bConsoleActive: boolean;
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    PyEscapeFlag:= true;
    if PyCommandRunning then
    begin
      Key:= 0;
      exit
    end;

    bEditorActive:=
      (ActiveControl is TATSynEdit) and
      (ActiveControl.Parent is TEditorFrame);
    bConsoleActive:=
      fmConsole.ed.Focused or
      fmConsole.memo.Focused;

    if not bEditorActive then
    begin
      DoFocusEditor;

      if bConsoleActive then
        if UiOps.EscapeCloseConsole then
          ShowBottom:= false;
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
  if SplitterHorz.Visible and PanelBottom.Visible then
    SplitterHorz.Top:= PanelBottom.Top-8;

  if SplitterVert.Visible and PanelLeft.Visible then
    SplitterVert.Left:= PanelLeft.Width;
end;

procedure TfmMain.FormShow(Sender: TObject);
var
  NTickShowEnd,
  NTickPluginBegin, NTickPluginEnd: QWord;
  Frame: TEditorFrame;
  i: integer;
begin
  {$ifdef darwin}
  // https://bugs.freepascal.org/view.php?id=35599
  SplitterHorz.ResizeStyle:= rsUpdate;
  SplitterVert.ResizeStyle:= rsUpdate;
  Groups.Splitter1.ResizeStyle:= rsUpdate;
  Groups.Splitter2.ResizeStyle:= rsUpdate;
  Groups.Splitter3.ResizeStyle:= rsUpdate;
  Groups.Splitter4.ResizeStyle:= rsUpdate;
  Groups.Splitter5.ResizeStyle:= rsUpdate;
  {$endif}

  if FHandledOnShow then exit;
  DoControlLock(Self);

  DoApplyInitialGroupSizes;
  DoApplyFont_Text;
  DoApplyFont_Ui;
  DoApplyFont_Output;
  DoApplyUiOps;

  if UiOps.ReopenSession then
    DoOps_LoadSession(GetSessionFilename);

  FHandledOnShow:= true;

  FAllowLoadKeymap:= true;
  DoOps_LoadKeymap;

  NTickPluginBegin:= GetTickCount64;
  DoPyEvent(CurrentEditor, cEventOnStart, []);
  NTickPluginEnd:= GetTickCount64;

  DoApplyInitialSidebarPanel;

  UpdateMenuPlugins;
  UpdateMenuPlugins_Shortcuts(true);
  UpdateMenuHotkeys;
  UpdateMenuTabsize;
  UpdateMenuPicScale;

  UpdateSidebarButtons;
  UpdateBottomButtons;
  UpdateStatus;
  DoLoadCommandLine;
  DoApplyInitialWindowPos;

  //postpone parsing until frames are shown
  AllowFrameParsing:= true;
  for i:= 0 to FrameCount-1 do
  begin
    Frame:= Frames[i];
    if Frame.Visible then
      Frame.DoShow;
  end;

  FAllowOnFocus:= true;
  Frame:= CurrentFrame;
  if Assigned(Frame) then Frame.SetFocus;

  DoControlUnlock(Self);

  NTickShowEnd:= GetTickCount64;
  MsgLogConsole(Format(
    'Startup: total: %dms, including plugins: %dms', [
    (NTickShowEnd-NTickInitial) div 10 * 10,
    (NTickPluginEnd-NTickPluginBegin) div 10 * 10
    ]));

  MsgLogDebug('start');
  DoOps_MultiInstaller;
end;

procedure TfmMain.DoOps_MultiInstaller;
begin
  if not FileExistsUTF8(GetAppPath(cFileOptionsHistory)) then
    DoPyCommand('cuda_multi_installer', 'open_menu', []);
end;

procedure TfmMain.FrameAddRecent(Sender: TObject; const AFileName: string);
begin
  UpdateMenuRecent(Sender as TEditorFrame, AFileName);
end;

procedure TfmMain.FrameOnChangeCaretPos(Sender: TObject);
begin
  if FCodetreeDblClicking then exit;
  TimerEdCaret.Enabled:= false;
  TimerEdCaret.Enabled:= true;
end;

procedure TfmMain.FrameOnMsgStatus(Sender: TObject; const AStr: string);
begin
  MsgStatus(AStr);
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
  UpdateMenuRecent(nil, '');
  //
  DeleteFileUTF8(GetAppPath(cFileOptionsHistoryFiles));
end;

function TfmMain.DoFileInstallZip(const fn: string; out DirTarget: string;
  ASilent: boolean): boolean;
var
  msg, msg2: string;
  AddonType: TAppAddonType;
begin
  DoInstallAddonFromZip(fn, GetAppPath(cDirDataAutocomplete), msg, msg2,
    Result, AddonType, DirTarget, ASilent);

  if Result then
  begin
    if AddonType in [cAddonTypeLexer, cAddonTypeLexerLite] then
    begin
      DoOps_LoadLexerLib;
    end;

    if AddonType=cAddonTypePlugin then
    begin
      DoOps_LoadPlugins;
      DoOps_LoadKeymap;
      UpdateMenuPlugins;
      UpdateMenuPlugins_Shortcuts(true);
    end;

    if not ASilent then
      DoDialogAddonInstalledReport(msg, msg2);
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

function TfmMain.GetShowSidePanel: boolean;
begin
  if FloatSide then
    Result:= FFormFloatSide.Visible
  else
    Result:= PanelLeft.Visible;
end;

function TfmMain.GetShowBottom: boolean;
begin
  if FloatBottom then
    Result:= FFormFloatBottom.Visible
  else
    Result:= PanelBottom.Visible;
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
    DoLocalize_FormSaveTabs(Form);
    Form.List.Clear;
    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      if not (F.Ed1.Modified or F.Ed2.Modified) then Continue;
      SCaption:= F.TabCaption+IfThen(F.Filename<>'', '  ('+ExtractFileDir(F.Filename)+')');
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
    //DoLexerExportFromLibToFile(an);
    UpdateMenuLexers;
    UpdateStatus;
    UpdateCurrentFrame;
  end;
end;

procedure TfmMain.DoDialogLexerLib;
begin
  if DoShowDialogLexerLib(
    GetAppPath(cDirDataAutocomplete),
    EditorOps.OpFontName,
    EditorOps.OpFontSize,
    @DoOnDeleteLexer
    ) then
  begin
    UpdateMenuLexers;
    UpdateStatus;
    UpdateCurrentFrame;
  end;
end;

procedure TfmMain.DoApplyLexerStyleMaps(AndApplyTheme: boolean);
var
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
    with Frames[i] do
    begin
      Lexer[Ed1]:= Lexer[Ed1];
      if AndApplyTheme then
        ApplyTheme;
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

procedure TfmMain.DoCopyFilenameFull;
begin
  SClipboardCopy(CurrentFrame.FileName);
end;

procedure TfmMain.DoCopyFilenameDir;
begin
  SClipboardCopy(ExtractFileDir(CurrentFrame.FileName));
end;

procedure TfmMain.DoCopyFilenameName;
begin
  SClipboardCopy(ExtractFileName(CurrentFrame.FileName));
end;


procedure TfmMain.DoCopyLine;
var
  S: string;
  Ed: TATSynEdit;
  N: integer;
begin
  Ed:= CurrentEditor;
  N:= Ed.Carets[0].PosY;
  if Ed.Strings.IsIndexValid(N) then
  begin
    S:= Ed.Strings.LinesUTF8[N];
    SClipboardCopy(S);
  end;
end;

procedure TfmMain.DoHelpAbout;
var
  Form: TfmAbout;
begin
  Form:= TfmAbout.Create(Self);
  with Form do
  try
    DoLocalize_FormAbout(Form);
    labelVersion.Caption:= cAppExeVersion;
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
var
  fn: string;
begin
  fn:= GetAppPath(cDirReadme)+DirectorySeparator+'history.txt';
  if FileExistsUTF8(fn) then
    DoFileOpen(fn, '');
end;

procedure TfmMain.MenuWindowClick(Sender: TObject);
begin
  SetFrame(Frames[(Sender as TMenuItem).Tag]);
end;


procedure TfmMain.UpdateFrameLineEnds(Frame: TEditorFrame; AValue: TATLineEnds);
begin
  if Assigned(Frame) then
    Frame.LineEnds[Frame.Editor]:= AValue;
  UpdateStatus;
  MsgStatus(msgStatusEndsChanged);
end;

type
  TUniqInstanceHack = class(TUniqueInstance);


procedure TfmMain.DoApplyUiOpsToGroups(G: TATGroups);
begin
  G.SetTabFont(Self.Font);
  G.ScalePercents:= UiOps.Scale;
  G.SetTabOption(tabOptionAnimationEn, Ord(UiOps.TabAnimation));
  G.SetTabOption(tabOptionShowHint, 1);
  G.SetTabOption(tabOptionVarWidth, Ord(UiOps.TabVarWidth));
  G.SetTabOption(tabOptionMultiline, Ord(UiOps.TabMultiline));
  G.SetTabOption(tabOptionAngled, Ord(UiOps.TabAngled));
  G.SetTabOption(tabOptionSpaceInitial, IfThen(UiOps.TabAngled, 10, 4));
  G.SetTabOption(tabOptionSpaceBetweenTabs, IfThen(UiOps.TabAngled, 4, 0));
  G.SetTabOption(tabOptionShowFlat, Ord(UiOps.TabFlat));
  G.SetTabOption(tabOptionPosition, UiOps.TabPosition);
  G.SetTabOption(tabOptionShowXButtons, UiOps.TabShowX);
  G.SetTabOption(tabOptionShowPlus, Ord(UiOps.TabShowPlus));
  G.SetTabOption(tabOptionShowEntireColor, Ord(UiOps.TabColorFull));
  G.SetTabOption(tabOptionDoubleClickClose, Ord(UiOps.TabDblClickClose));
  G.SetTabOption(tabOptionWidthNormal, UiOps.TabWidth);
  G.SetTabOption(tabOptionWidthMin, UiOps.TabWidthMin);
  G.SetTabOption(tabOptionWidthMax, UiOps.TabWidthMax);
  G.SetTabOption(tabOptionHeight, UiOps.TabHeight+UiOps.TabSpacer);
  G.SetTabOption(tabOptionHeightInner, UiOps.TabHeightInner);
  G.SetTabOption(tabOptionSpacer, IfThen(UiOps.TabPosition=0, UiOps.TabSpacer));
  G.SetTabOption(tabOptionSpacer2, 1); //for multiline mode
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

procedure TfmMain.DoApplyUiOps;
var
  i: integer;
begin
  cAdapterIdleInterval:= UiOps.LexerDelayedParsingPause;
  cAdapterIdleTextSize:= UiOps.LexerDelayedParsingSize;
  CompletionOps.AppendOpeningBracket:= UiOps.AutocompleteAddOpeningBracket;
  CompletionOps.UpDownAtEdge:= TATCompletionUpDownAtEdge(UiOps.AutocompleteUpDownAtEdge);

  AppScaleToolbar(ToolbarMain);
  AppScaleToolbar(ToolbarSideTop);
  AppScaleToolbar(ToolbarSideLow);
  AppScaleToolbar(ToolbarSideMid);

  LexerProgress.Width:= AppScale(UiOps.ProgressbarWidth);
  LexerProgress.Height:= AppScale(UiOps.ProgressbarHeightSmall);
  StatusProgress.Width:= AppScale(UiOps.ProgressbarWidth);
  ButtonCancel.Width:= AppScale(UiOps.ProgressbarWidth);

  AppScaleSplitter(SplitterVert);
  AppScaleSplitter(SplitterHorz);
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
  StatusAlt.DoubleBuffered:= UiOps.DoubleBuffered;
  ButtonCancel.DoubleBuffered:= UiOps.DoubleBuffered;
  StatusProgress.DoubleBuffered:= UiOps.DoubleBuffered;
  LexerProgress.DoubleBuffered:= UiOps.DoubleBuffered;
  ListboxOut.DoubleBuffered:= UiOps.DoubleBuffered;
  ListboxVal.DoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmConsole) then
    fmConsole.IsDoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmFind) then
    fmFind.IsDoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmGoto) then
    fmGoto.IsDoubleBuffered:= UiOps.DoubleBuffered;
  //end apply DoubleBuffered

  UpdateBottomLayout(FloatBottom);
  UpdateStatusbarPanelsFromString(UiOps.StatusPanels);
  UpdateStatusbarHints;

  TimerTreeFill.Interval:= UiOps.TreeTimeFill;
  TimerEdCaret.Interval:= UiOps.TreeTimeCaret;
  CodeTree.Tree.ToolTips:= UiOps.TreeShowTooltips;
  CodeTree.Invalidate;

  CodeTreeFilterInput.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  CodeTreeFilterReset.Width:= AppScale(UiOps.ScrollbarWidth);

  EditorCaretPropsFromString(fmConsole.memo.CaretPropsReadonly, EditorOps.OpCaretViewReadonly);
  fmConsole.memo.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  fmConsole.ed.Height:= AppScale(UiOps.InputHeight);
  fmConsole.ed.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  fmConsole.Wordwrap:= UiOps.ConsoleWordWrap;
  fmConsole.ed.OptComboboxArrowSize:= UiOps.ScrollbarArrowSize;

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
  PanelLeftTitle.Height:= Groups.Pages1.Tabs.Height;

  if UiOps.TabPosition=1 then
    PanelLeftTitle.Align:= alBottom
  else
    PanelLeftTitle.Align:= alTop;

  case UiOps.TreeFilterLayout of
    0:
      PanelCodeTreeTop.Hide;
    1:
      begin
        PanelCodeTreeTop.Visible:= CodeTree.Visible;
        PanelCodeTreeTop.Align:= alTop;
        PanelCodeTreeTop.Top:= PanelLeftTitle.Height; //fix pos relative to title
      end;
    2:
      begin
        PanelCodeTreeTop.Visible:= CodeTree.Visible;
        PanelCodeTreeTop.Align:= alBottom;
      end;
  end;

  PanelCodeTreeTop.Height:= AppScale(UiOps.InputHeight);

  Status.HeightInitial:= UiOps.StatusHeight;
  StatusAlt.HeightInitial:= UiOps.StatusHeight;

  TimerStatus.Interval:= UiOps.StatusTime*1000;

  ATFlatTheme.FontName:= UiOps.VarFontName;
  ATFlatTheme.FontSize:= UiOps.VarFontSize;
  ATFlatTheme.ScalePercents:= UiOps.Scale;
  ATFlatTheme.ScaleFontPercents:= UiOps.ScaleFont;

  ATScrollbar.ATScrollbarTheme.InitialSize:= UiOps.ScrollbarWidth;
  ATScrollbar.ATScrollbarTheme.BorderSize:= UiOps.ScrollbarBorderSize;
  ATScrollbar.ATScrollbarTheme.ScalePercents:= UiOps.Scale;

  EditorScalePercents:= UiOps.Scale;
  EditorScaleFontPercents:= UiOps.ScaleFont;

  CompletionOps.FormSizeX:= AppScale(UiOps.ListboxCompleteSizeX);
  CompletionOps.FormSizeY:= AppScale(UiOps.ListboxCompleteSizeY);

  if UiOps.OneInstance and not FOption_OpenNewWindow then
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


procedure TfmMain.DoFolderOpen(const ADirName: string; ANewProject: boolean);
const
  cBool: array[boolean] of string = (cPyFalse, cPyTrue);
begin
  DoPyCommand('cuda_project_man', 'open_dir', [
    'r"'+ADirName+'"',
    cBool[ANewProject]
    ]);
end;

procedure TfmMain.DoGroupsChangeMode(Sender: TObject);
begin
  DoPyEvent(CurrentEditor, cEventOnState, [IntToStr(APPSTATE_GROUPS)]);
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
  if S='' then
  begin
    CodeTreeFilter.ForceFilter('');
    CodeTreeFilter.FilteredTreeview:= nil;
    exit;
  end;

  CodeTreeFilter.FilteredTreeview:= CodeTree.Tree;
  CodeTreeFilter.Text:= S;

  F:= CurrentFrame;
  if Assigned(F) then
    F.CodetreeFilter:= S;
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

function TfmMain.DoFileOpen(AFileName, AFileName2: string; APages: TATPages;
  const AOptions: string): TEditorFrame;
var
  D: TATTabData;
  F: TEditorFrame;
  bSilent, bPreviewTab, bEnableHistory, bEnableEvent,
  bAllowZip, bAllowPics, bDetectedPics,
  bAndActivate, bAllowNear: boolean;
  OpenMode, NonTextMode: TAppOpenMode;
  CurGroups: TATGroups;
  //tick: QWord;
  //msg: string;
  i: integer;
begin
  Result:= nil;
  AppFolderOfLastInstalledAddon:= '';
  if Application.Terminated then exit;

  CurGroups:= CurrentGroups;

  bSilent:= Pos('/silent', AOptions)>0;
  bPreviewTab:= Pos('/preview', AOptions)>0;
  bEnableHistory:= Pos('/nohistory', AOptions)=0;
  bEnableEvent:= Pos('/noevent', AOptions)=0;
  bAndActivate:= Pos('/passive', AOptions)=0;
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
    D:= DoTabAdd(APages, '', bAndActivate, bAllowNear);
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
    AFileName:= ExpandFileName(AFileName);
    if not FileExistsUTF8(AFileName) then
    begin
      MsgBox(msgCannotFindFile+#13+AFileName, MB_OK or MB_ICONERROR);
      Exit
    end;
  end;

  if AFileName2<>'' then
  begin
    AFileName2:= ExpandFileName(AFileName2);
    if not FileExistsUTF8(AFileName2) then
    begin
      MsgBox(msgCannotFindFile+#13+AFileName2, MB_OK or MB_ICONERROR);
      Exit
    end;
  end;

  if OpenMode=cOpenModeEditor then
  begin
    //zip files
    if bAllowZip then
    if ExtractFileExt(AFileName)='.zip' then
    begin
      if DoFileInstallZip(AFileName, AppFolderOfLastInstalledAddon, bSilent) then
        Result:= CurrentFrame;
      exit
    end;

    //py event
    if bEnableEvent then
      if DoPyEvent(CurrentEditor, cEventOnOpenBefore,
        [SStringToPythonString(AFileName)]) = cPyFalse then exit;

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
    F:= FindFrameOfFilename(AFileName2);
  if Assigned(F) then
  begin
    //don't work, if need to open 2 files
    if AFileName2<>'' then exit;

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
      D:= DoTabAdd(APages, 'pre', true, false);
      if not Assigned(D) then exit;
      D.TabSpecial:= true;
      D.TabFontStyle:= StringToFontStyles(UiOps.TabPreviewFontStyle);
      Result:= D.TabObject as TEditorFrame;
    end;

    Result.Adapter[Result.Ed1].Stop;
    Result.Adapter[Result.Ed2].Stop;
    Result.DoFileOpen(AFileName, AFileName2, bEnableHistory, true, OpenMode);
    MsgStatusFileOpened(AFileName, AFileName2);

    DoPyEvent(Result.Ed1, cEventOnOpen, []);
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
      F.DoFileOpen(AFileName, AFileName2, bEnableHistory, true, OpenMode);
      Result:= F;
      //tick:= (GetTickCount64-tick) div 1000;

      UpdateStatus;
      //if tick>2 then
      //  msg:= msg+' ('+IntToStr(tick)+'s)';
      MsgStatusFileOpened(AFileName, AFileName2);

      DoPyEvent(F.Ed1, cEventOnOpen, []);
      if F.IsText and (F.LexerName[F.Ed1]='') then
        DoPyEvent(F.Ed1, cEventOnOpenNone, []);
      if AFileName2<>'' then
        DoPyEvent(F.Ed2, cEventOnOpen, []);

      Exit
    end;
  end;

  D:= DoTabAdd(APages, ExtractFileName(AFileName), bAndActivate, bAllowNear);
  if not Assigned(D) then
  begin
    D:= Groups.Pages1.Tabs.GetTabData(0);
    DoClearSingleFirstTab;
  end;
  F:= D.TabObject as TEditorFrame;

  //tick:= GetTickCount64;
  F.DoFileOpen(AFileName, AFileName2, bEnableHistory, true, OpenMode);
  Result:= F;
  //tick:= (GetTickCount64-tick) div 1000;

  UpdateStatus;
  //if tick>2 then
  //  msg:= msg+' ('+IntToStr(tick)+'s)';
  MsgStatusFileOpened(AFileName, AFileName2);

  DoPyEvent(F.Ed1, cEventOnOpen, []);
  if F.IsText and (F.LexerName[F.Ed1]='') then
    DoPyEvent(F.Ed1, cEventOnOpenNone, []);
  if AFileName2<>'' then
    DoPyEvent(F.Ed2, cEventOnOpen, []);

  Result.SetFocus;
end;


procedure TfmMain.DoFileOpenDialog_NoPlugins;
begin
  DoFileOpenDialog('/noevent');
end;

procedure TfmMain.DoFileDialog_PrepareDir(Dlg: TFileDialog);
begin
  if CurrentFrame.FileName<>'' then
    Dlg.InitialDir:= ExtractFileDir(CurrentFrame.FileName)
  else
  begin
    if UiOps.InitialDir<>'' then
      Dlg.InitialDir:= UiOps.InitialDir
    else
      Dlg.InitialDir:= FLastDirOfOpenDlg;
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
  NCountZip, i: integer;
  fn: string;
  bZip, bZipAllowed: boolean;
begin
  bZipAllowed:= Pos('/nozip', AOptions)=0;
  with OpenDlg do
  begin
    FileName:= '';
    DoFileDialog_PrepareDir(OpenDlg);
    if not Execute then exit;
    DoFileDialog_SaveDir(OpenDlg);

    if Files.Count>1 then
    begin
      NCountZip:= 0;
      StatusProgress.Progress:= 0;
      StatusProgress.MaxValue:= Files.Count;
      StatusProgress.Show;

      for i:= 0 to Files.Count-1 do
      begin
        fn:= Files[i];
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
      if FileExistsUTF8(FileName) then
        DoFileOpen(FileName, '', nil, AOptions)
      else
      if MsgBox(
        Format(msgConfirmCreateNewFile, [FileName]),
        MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
      begin
        FCreateFile(FileName);
        DoFileOpen(FileName, '', nil, AOptions);
      end;
    end;
  end;
end;

procedure TfmMain.DoDialogCommands;
var
  Ed: TATSynEdit;
  NCmd: integer;
begin
  Ed:= CurrentEditor;
  MsgStatus(msgStatusHelpOnShowCommands);
  NCmd:= DoDialogCommands_Custom(true, true, true, true, false, '', FLastSelectedCommand);
  if NCmd>0 then
  begin
    FLastSelectedCommand:= NCmd;
    Ed.DoCommand(NCmd);
    UpdateCurrentFrame;
  end;
end;


function TfmMain.DoDialogCommands_Py(AShowUsual, AShowPlugins, AShowLexers,
  AAllowConfig, AShowCentered: boolean; ACaption: string): string;
var
  NCmd: integer;
begin
  Result:= '';
  NCmd:= DoDialogCommands_Custom(AShowUsual, AShowPlugins, AShowLexers, AAllowConfig, AShowCentered, ACaption, 0);
  if NCmd<=0 then exit;

  if (NCmd>=cmdFirstPluginCommand) and (NCmd<=cmdLastPluginCommand) then
  begin
    with TAppCommand(AppCommandList[NCmd-cmdFirstPluginCommand]) do
      if ItemProcParam<>'' then
        Result:= Format('p:module=%s;cmd=%s;info=%s;', [ItemModule, ItemProc, ItemProcParam])
      else
        Result:= Format('p:%s.%s', [ItemModule, ItemProc]);
  end
  else
  if (NCmd>=cmdFirstLexerCommand) and (NCmd<cmdFirstLexerCommand+AppManager.LexerCount) then
  begin
    Result:= 'l:'+AppManager.Lexers[NCmd-cmdFirstLexerCommand].LexerName
  end
  else
    Result:= 'c:'+IntToStr(NCmd);
end;


function TfmMain.DoDialogCommands_Custom(
  AShowUsual, AShowPlugins, AShowLexers, AAllowConfig, AShowCentered: boolean;
  ACaption: string; AFocusedCommand: integer): integer;
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  bKeysChanged: boolean;
begin
  Result:= 0;
  F:= CurrentFrame;
  if F=nil then exit;
  Ed:= F.Editor;

  fmCommands:= TfmCommands.Create(Self);
  try
    UpdateInputForm(fmCommands);
    fmCommands.OptShowUsual:= AShowUsual;
    fmCommands.OptShowPlugins:= AShowPlugins;
    fmCommands.OptShowLexers:= AShowLexers;
    fmCommands.OptAllowConfig:= AAllowConfig;
    fmCommands.OptFocusedCommand:= AFocusedCommand;
    fmCommands.OnMsg:= @DoCommandsMsgStatus;
    fmCommands.CurrentLexerName:= F.LexerName[Ed];
    fmCommands.Keymap:= Ed.Keymap;
    fmCommands.ListCaption:= ACaption;
    if AShowCentered then
      fmCommands.Position:= poScreenCenter;
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
  Str: string;
begin
  DoLocalize_FormGoto;
  fmGoto.Width:= AppScale(UiOps.ListboxSizeX);
  UpdateInputForm(fmGoto, false);

  if fmGoto.ShowModal=mrOk then
  begin
    Str:= UTF8Encode(fmGoto.edInput.Text);

    if DoPyEvent(CurrentEditor, cEventOnGotoEnter,
      [SStringToPythonString(Str)]) = cPyFalse then exit;

    DoGotoFromInput(Str);
  end;
end;

function TfmMain.DoDialogMenuList(const ACaption: string; AItems: TStringList;
  ACloseOnCtrlRelease: boolean=false): integer;
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
    Form.ShowModal;
    Result:= Form.ResultIndex;
  finally
    FreeAndNil(Form);
  end;
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

procedure TfmMain.DoDialogGotoBookmark;
const
  cMaxLen = 150;
var
  Ed: TATSynEdit;
  items: TStringList;
  bm: TATBookmarkItem;
  strInfo, strKind, strCaption: string;
  NLineMax, NLine, NKind, i: integer;
begin
  Ed:= CurrentEditor;
  NLineMax:= Ed.Strings.Count-1;
  items:= TStringList.Create;

  with TIniFile.Create(GetAppLangFilename) do
  try
    strCaption:= ReadString('m_sr', 'b_', 'Bookmarks');
    strCaption:= StringReplace(strCaption, '&', '', [rfReplaceAll]);
  finally
    Free;
  end;

  try
    for i:= 0 to ed.Strings.Bookmarks.Count-1 do
    begin
      bm:= ed.Strings.Bookmarks[i];
      if not bm.Data.ShowInBookmarkList then Continue;

      NLine:= bm.Data.LineNum;
      if not ed.Strings.IsIndexValid(NLine) then Continue;

      //paint prefix [N] for numbered bookmarks (kind=2..10)
      NKind:= bm.Data.Kind;
      if (NKind>=2) and (NKind<=10) then
        strKind:= '['+IntToStr(NKind-1)+'] '
      else
        strKind:= '';

      strInfo:= ed.Strings.LinesUTF8[NLine];
      strInfo:= Copy(strInfo, 1, cMaxLen) + #9 + strKind + IntToStr(NLine+1);
      items.AddObject(strInfo, TObject(PtrInt(NLine)));
    end;

    if items.Count=0 then
    begin
      MsgStatus(msgCannotFindBookmarks);
      Exit;
    end;

    NLine:= DoDialogMenuList(strCaption, items);
    if NLine<0 then
    begin
      MsgStatus(msgStatusCancelled);
      Exit
    end;

    NLine:= PtrInt(items.Objects[NLine]);
    if NLine>NLineMax then
      NLine:= NLineMax;

  finally
    FreeAndNil(items);
  end;

  Ed.DoGotoPos(
    Point(0, NLine),
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );
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


procedure TfmMain.SetShowBottom(AValue: boolean);
var
  bBottom: boolean;
  Frame: TEditorFrame;
begin
  if GetShowBottom<>AValue then
  begin
    bBottom:= IsFocusedBottom;

    PanelBottom.Visible:= AValue;
    if FloatBottom then
    begin
      FFormFloatBottom.Visible:= AValue;
    end
    else
    begin
      SplitterHorz.Visible:= AValue;
      SplitterHorz.Top:= PanelBottom.Top-8;
    end;

    if not AValue then
      if bBottom then
      begin
        Frame:= CurrentFrame;
        if Assigned(Frame) then
          Frame.SetFocus;
      end;
  end;

  UpdateBottomButtons;
  UpdateStatus;
end;

procedure TfmMain.SetShowSidePanel(AValue: boolean);
begin
  if GetShowSidePanel<>AValue then
  begin
    PanelLeft.Visible:= AValue;
    if FloatSide then
    begin
      FFormFloatSide.Visible:= AValue;
    end
    else
    begin
      SplitterVert.Visible:= AValue;
      SplitterVert.Left:= PanelLeft.Width;
    end;

    if AValue then
    begin
      if SidebarPanel='' then
        DoShowSidePanel(msgPanelTree_Init, false);
      UpdateTreeContents;
    end;
  end;
  UpdateSidebarButtons;
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
    NCur:= GroupsCtx.PagesIndexOf(GroupsCtx.PopupPages);
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

procedure TfmMain.PopupTabSizePopup(Sender: TObject);
var
  Ed: TATSynEdit;
  Msg: string;
  NTab, i: integer;
begin
  Ed:= CurrentEditor;
  if Ed.OptTabSpaces then
    Msg:= msgStatusbarTextSpaces
  else
    Msg:= msgStatusbarTextTab;
  NTab:= Ed.OptTabSize;

  for i:= cMenuTabsizeMin to cMenuTabsizeMax do
  begin
    FMenuItemTabSize[i].Caption:= Msg+': '+IntToStr(i);
    FMenuItemTabSize[i].Checked:= NTab=i;
  end;

  mnuTabsizeSpace.Checked:= Ed.OptTabSpaces;
end;

procedure TfmMain.PythonEngineAfterInit(Sender: TObject);
var
  Str: array of string;
  {$ifdef windows}
  dir: string;
  {$endif}
  PathAppend: boolean;
  InitList: TStringList;
  InitPy: string;
begin
  if not PythonOK then exit;

  PathAppend:= true;
  SetLength(Str, 0);

  {$ifdef windows}
  PathAppend:= false;
  dir:= ExtractFileDir(Application.ExeName)+DirectorySeparator;
  SetLength(Str, 2);
  Str[0]:= dir+'dlls';
  Str[1]:= dir+ChangeFileExt(UiOps.PyLibrary, '.zip');
  {$endif}

  //add to sys.path folders py/, py/sys/
  SetLength(Str, Length(Str)+2);
  Str[Length(Str)-2]:= GetAppPath(cDirPy);
  Str[Length(Str)-1]:= GetAppPath(cDirPy)+DirectorySeparator+'sys';

  Py_SetSysPath(Str, PathAppend);

  InitPy:= GetAppPath(cDirPy)+DirectorySeparator+'cudatext_init.py';
  if not FileExists(InitPy) then exit;

  InitList:= TStringList.Create;
  try
    InitList.LoadFromFile(InitPy);
    try
      GetPythonEngine.ExecStrings(InitList);
    except
    end;
  finally
    FreeAndNil(InitList);
  end;
end;

procedure TfmMain.InitPyEngine;
begin
  PythonEngine.DllPath:= ExtractFileDir(UiOps.PyLibrary);
  PythonEngine.DllName:= ExtractFileName(UiOps.PyLibrary);
  PythonEngine.LoadDll;

  if PythonOK then
    GetPythonEngine.ExecString('import sys')
  else
  begin
    FConsoleMustShow:= true;
    MsgLogConsole(msgCannotInitPython1);
    MsgLogConsole(msgCannotInitPython2);
    fmConsole.ShowError:= true;
    DisablePluginMenuItems;
  end;
end;

procedure TfmMain.DisablePluginMenuItems;
begin
  mnuPlugins.Enabled:= false;

  mnuToolbarCommentLineAdd.Enabled:= false;
  mnuToolbarCommentLineDel.Enabled:= false;
  mnuToolbarCommentLineToggle.Enabled:= false;
  mnuToolbarCommentStream.Enabled:= false;

  mnuToolbarSortAsc.Enabled:= false;
  mnuToolbarSortDesc.Enabled:= false;
  mnuToolbarSortAscNocase.Enabled:= false;
  mnuToolbarSortDescNocase.Enabled:= false;
  mnuToolbarSortDialog.Enabled:= false;
  mnuToolbarSortShuffle.Enabled:= false;
  mnuToolbarSortReverse.Enabled:= false;
  mnuToolbarSortRemoveBlank.Enabled:= false;
  mnuToolbarSortRemoveDup.Enabled:= false;
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
  if SameText(Frame.Editor.EncodingName, AEnc) then exit;
  Frame.Editor.EncodingName:= AEnc;

  if AAlsoReloadFile then
  begin
    if Frame.FileName<>'' then
      Frame.DoFileReload_DisableDetectEncoding
    else
      MsgBox(msgCannotReloadUntitledTab, MB_OK or MB_ICONWARNING);
  end
  else
  begin
    //set modified to allow save
    Frame.Editor.Modified:= true;
  end;

  Frame.Editor.DoEventChange; //reanalyze all file
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


procedure TfmMain.DoOps_LoadLexerLib;
var
  fn, fn_ops, DirLexers, LexName: string;
  ListFiles, ListBackup: TStringlist;
  an: TecSyntAnalyzer;
  ini: TIniFile;
  i, j: integer;
begin
  ListFiles:= TStringList.Create;
  ListBackup:= TStringList.Create;
  try
    DoOps_LexersDisableInFrames(ListBackup);

    AppManager.Clear;
    AppManagerLite.Clear;

    //load lite lexers
    DirLexers:= GetAppPath(cDirDataLexersLite);
    AppManagerLite.LoadFromDir(DirLexers);

    //load EControl lexers
    DirLexers:= GetAppPath(cDirDataLexers);
    FindAllFiles(ListFiles, DirLexers, '*.lcf', false);
    ListFiles.Sort;

    if ListFiles.Count=0 then
      MsgStatusAlt(msgCannotFindLexersAll, 3);

    for i:= 0 to ListFiles.Count-1 do
    begin
      an:= AppManager.AddLexer;
      //an.Name:= '_lx_'+LexerFilenameToComponentName(ListFiles[i]);
      an.LoadFromFile(ListFiles[i]);

      //load *.cuda-lexops
      fn_ops:= GetAppLexerOpsFilename(an.LexerName);
      if FileExistsUTF8(fn_ops) then
        DoLoadLexerStylesFromFile_JsonLexerOps(an, fn_ops, UiOps.LexerThemes);
    end;

    //correct sublexer links
    for i:= 0 to AppManager.LexerCount-1 do
    begin
      an:= AppManager.Lexers[i];
      fn:= GetAppLexerMapFilename(an.LexerName);
      if FileExists(fn) then
      begin
        ini:= TIniFile.Create(fn);
        try
          for j:= 0 to an.SubAnalyzers.Count-1 do
          begin
            LexName:= ini.ReadString('ref', IntToStr(j), '');
            if LexName<>'' then
              an.SubAnalyzers[j].SyntAnalyzer:= AppManager.FindLexerByName(LexName);
          end;
        finally
          FreeAndNil(ini);
        end;
      end;
    end;

    UpdateMenuLexers;

    DoOps_LexersRestoreInFrames(ListBackup);
  finally
    FreeAndNil(ListFiles);
    FreeAndNil(ListBackup);
  end;
end;


procedure TfmMain.UpdateMenuLexers;
begin
  UpdateKeymapDynamicItems;
  DoOps_LoadKeymap;
  UpdateMenuLexersTo(PopupLex.Items);
  UpdateMenuLexersTo(mnuLexers);
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
  FLastStatusbarMessage:= AText;

  DoStatusbarTextByTag(Status, StatusbarTag_Msg, GetStatusbarPrefix(CurrentFrame)+AText);

  if AText='' then exit;
  TimerStatus.Enabled:= false;
  TimerStatus.Enabled:= true;
end;

procedure TfmMain.MsgStatusAlt(const AText: string; ASeconds: integer);
const
  cMax=30;
begin
  if ASeconds<=0 then
  begin
    TimerStatusAlt.Enabled:= false;
    StatusAlt.Hide;
    Exit
  end;

  if ASeconds>cMax then
    ASeconds:= cMax;

  //StatusAlt.Parent:= Status; //place hint on statusbar
  //StatusAlt.Align:= alClient;
  StatusAlt.Top:= Status.Top-4;

  StatusAlt.Captions[0]:= AText;
  StatusAlt.Show;

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
    if ShowSidePanel then
      DoShowSidePanel(ACaption, true);
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
  PanelLeft.Align:= cVal[AValue];
  SplitterVert.Align:= cVal[AValue];

  if AValue then
    SplitterVert.Left:= PanelSide.Width
  else
    SplitterVert.Left:= ClientWidth-PanelSide.Width;
end;

procedure TfmMain.SetShowStatus(AValue: boolean);
begin
  Status.Visible:= AValue;
end;

procedure TfmMain.SetShowToolbar(AValue: boolean);
begin
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

procedure TfmMain.DoFileReopen;
var
  F: TEditorFrame;
  PrevRO: boolean;
  PrevLexer: string;
begin
  F:= CurrentFrame;
  if F.FileName='' then exit;

  if F.Ed1.Modified and UiOps.ReloadUnsavedConfirm then
    if MsgBox(
      Format(msgConfirmReopenModifiedTab, [F.FileName]),
      MB_OKCANCEL or MB_ICONQUESTION
      ) <> ID_OK then exit;

  PrevRO:= F.ReadOnly[F.Ed1];
  PrevLexer:= F.LexerName[F.Ed1];
  F.ReadOnly[F.Ed1]:= false;
  F.DoFileReload;
  F.LexerName[F.Ed1]:= PrevLexer;
  F.ReadOnly[F.Ed1]:= PrevRO;
  F.Ed1.Modified:= false;

  UpdateStatus;
  MsgStatus(msgStatusReopened+' '+ExtractFileName(F.Filename));
end;


function TfmMain.DoFileCloseAll(AWithCancel: boolean): boolean;
var
  Flags: integer;
  F: TEditorFrame;
  ListNoSave: TList;
  i: integer;
begin
  if AWithCancel then
    Flags:= MB_YESNOCANCEL or MB_ICONQUESTION
  else
    Flags:= MB_YESNO or MB_ICONQUESTION;

  ListNoSave:= TList.Create;
  try
    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      if F.Ed1.Modified then
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
    end;
  finally
    FreeAndNil(ListNoSave);
  end;

  Result:= Groups.CloseTabs(tabCloseAll, false);
  if not Result then exit;
end;


procedure TfmMain.DoFileCloseAndDelete;
var
  fn: string;
begin
  fn:= CurrentFrame.FileName;
  if fn='' then exit;

  if MsgBox(msgConfirmCloseDelFile+#13+fn, MB_OKCANCEL or MB_ICONWARNING)=id_ok then
    if Groups.CloseTabs(tabCloseCurrent, false) then
      DeleteFileUTF8(fn);
end;

procedure TfmMain.DoFileNew;
begin
  DoFileOpen('', '');
end;


procedure TfmMain.MenuRecentsClick(Sender: TObject);
var
  fn: string;
  n: integer;
begin
  n:= (Sender as TComponent).Tag;
  fn:= SExpandHomeDirInFilename(FListRecents[n]);
  if FileExistsUTF8(fn) then
    DoFileOpen(fn, '')
  else
  begin
    MsgBox(msgCannotFindFile+#13+fn, MB_OK or MB_ICONERROR);
    FListRecents.Delete(n);
    UpdateMenuRecent(nil, '');
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
  FloatSide:= not FloatSide;
end;

procedure TfmMain.DoToggleFloatBottom;
begin
  FloatBottom:= not FloatBottom;
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

procedure TfmMain.DoCudaLibAction(const AMethod: string);
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  Ed.Strings.BeginUndoGroup;
  try
    DoPyCommand('cudax_lib', AMethod, []);
  finally
    Ed.Strings.EndUndoGroup;
  end;
end;


procedure TfmMain.DoShowConsole(AndFocus: boolean);
begin
  DoShowBottomPanel(msgPanelConsole_Init, AndFocus);
end;

procedure TfmMain.DoShowOutput(AndFocus: boolean);
begin
  DoShowBottomPanel(msgPanelOutput_Init, AndFocus);
end;

procedure TfmMain.DoShowValidate(AndFocus: boolean);
begin
  DoShowBottomPanel(msgPanelValidate_Init, AndFocus);
end;

procedure TfmMain.DoShowSidePanel(const ATabCaption: string; AndFocus: boolean);
begin
  if ATabCaption='-' then
  begin
    ShowSidePanel:= false;
  end
  else
  begin
    ShowSidePanel:= true;
    if ATabCaption<>'' then
      DoSidebar_ActivateTab(ATabCaption, AndFocus);
  end;

  UpdateSidebarButtons;
end;


procedure TfmMain.DoShowBottomPanel(const ATabCaption: string; AndFocus: boolean);
begin
  if ATabCaption='-' then
  begin
    ShowBottom:= false;
  end
  else
  begin
    ShowBottom:= true;
    if ATabCaption<>'' then
      DoBottom_ActivateTab(ATabCaption, AndFocus);
  end;

  UpdateBottomButtons;
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
    FOrigShowBottom:= ShowBottom;
    FOrigShowSidePanel:= ShowSidePanel;
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
    if AHideAll or (Pos('b', UiOps.FullScreen)>0) then ShowBottom:= false;
    if AHideAll or (Pos('i', UiOps.FullScreen)>0) then ShowStatus:= false;
    if AHideAll or (Pos('p', UiOps.FullScreen)>0) then ShowSidePanel:= false;
    if AHideAll or (Pos('a', UiOps.FullScreen)>0) then ShowSideBar:= false;
    if AHideAll or (Pos('u', UiOps.FullScreen)>0) then ShowTabsMain:= false;
    if AHideAll or (Pos('g', UiOps.FullScreen)>0) then DoApplyGutterVisible(false);
  end
  else
  begin
    ShowToolbar:= FOrigShowToolbar;
    ShowStatus:= FOrigShowStatusbar;
    ShowBottom:= FOrigShowBottom;
    ShowSidePanel:= FOrigShowSidePanel;
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

procedure TfmMain.DoFileSave;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  DoFileDialog_PrepareDir(SaveDlg);
  if F.Editor.Modified or (F.GetFileName(F.Editor)='') then
    if F.DoFileSave(false, false) then
      DoFileDialog_SaveDir(SaveDlg);
end;

procedure TfmMain.DoFileSaveAs;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  DoFileDialog_PrepareDir(SaveDlg);
  if F.DoFileSave(true, false) then
    DoFileDialog_SaveDir(SaveDlg);
end;

procedure TfmMain.DoFocusEditor;
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
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
    if SameFileName(AName, F.GetFileName(F.Ed1)) or
      SameFileName(AName, F.GetFileName(F.Ed2)) then exit(F);
  end;
end;


function TfmMain.DoCheckFilenameOpened(const AName: string): boolean;
begin
  Result:= Assigned(FindFrameOfFilename(AName));
end;

procedure TfmMain.DoOps_OpenFile_Default;
var
  fn: string;
  F: TEditorFrame;
begin
  fn:= GetAppPath(cFileOptionsDefault);
  F:= DoFileOpen(fn, '');
  if Assigned(F) then
    F.ReadOnly[F.Ed1]:= true;
end;

procedure TfmMain.DoOps_OpenFile_User;
var
  fn: string;
begin
  fn:= GetAppPath(cFileOptionsUser);
  if not FileExistsUTF8(fn) then
  begin
    FCreateFile(fn, true);
    if not FileExistsUTF8(fn) then Exit;
  end;

  DoFileOpen(fn, '');
end;

procedure TfmMain.DoOps_OpenFile_DefaultAndUser;
var
  NameDef, NameUser: string;
  F: TEditorFrame;
begin
  NameDef:= GetAppPath(cFileOptionsDefault);
  NameUser:= GetAppPath(cFileOptionsUser);

  if not FileExistsUTF8(NameUser) then
  begin
    FCreateFile(NameUser, true);
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
    FCreateFile(fn, true);
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
    if SCallback<>'' then
      DoPyCallbackFromAPI(SCallback, []);
  end
  else
    F.Editor.DoCommand(NCommand);

  UpdateFrameEx(F, false);
  UpdateStatus;
end;

procedure TfmMain.SetLexerIndex(AIndex: integer);
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  CountUsual, CountLite: integer;
begin
  F:= CurrentFrame;
  Ed:= F.Editor;

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


procedure TfmMain.DoAutoComplete;
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  LexName: string;
  IsPascal, IsCss, IsHtml, IsCaseSens: boolean;
  FileHtml, FileCss, FileAcp: string;
  Caret: TATCaretItem;
begin
  F:= CurrentFrame;
  if F=nil then exit;
  Ed:= F.Editor;

  CompletionOps.CommitChars:= UiOps.AutocompleteCommitChars; //before DoPyEvent
  CompletionOps.CloseChars:= UiOps.AutocompleteCloseChars; //before DoPyEvent
  if DoPyEvent(Ed, cEventOnComplete, [])=cPyTrue then exit;

  if F.Lexer[Ed]=nil then exit;

  Caret:= Ed.Carets[0];
  LexName:= F.LexerNameAtPos(Ed, Point(Caret.PosX, Caret.PosY));
  if LexName='' then exit;

  IsPascal:= Pos('Pascal', LexName)>0;
  IsHtml:= UiOps.AutocompleteHtml and SRegexMatchesString(LexName, UiOps.AutocompleteHtml_Lexers, false);
  IsCss:= UiOps.AutocompleteCss and SRegexMatchesString(LexName, UiOps.AutocompleteCss_Lexers, false);
  IsCaseSens:= false; //cannot detect it yet
  FileCss:= GetAppPath(cDirDataAutocompleteSpec)+DirectorySeparator+'css_list.ini';
  FileHtml:= GetAppPath(cDirDataAutocompleteSpec)+DirectorySeparator+'html_list.ini';
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
    DoEditorCompletionAcp(Ed, FileAcp, IsCaseSens, IsPascal);
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

  mnuTreeSorted.Checked:= CodeTree.Tree.SortType<>stNone;
end;

procedure TfmMain.mnuTreeUnfoldAllClick(Sender: TObject);
begin
  CodeTree.Tree.FullExpand;
end;


procedure TfmMain.DoFileExportHtml;
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  Dlg: TSaveDialog;
  SFileName, STitle: string;
  NX, NY: integer;
begin
  F:= CurrentFrame;
  if F=nil then exit;
  Ed:= F.Editor;

  STitle:= ExtractFileName(F.GetFileName(Ed));
  if STitle='' then
    STitle:= 'untitled';

  Dlg:= TSaveDialog.Create(Self);
  try
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
  Application.ProcessMessages;

  DoEditorExportToHTML(Ed, SFileName, STitle,
    UiOps.ExportHtmlFontName,
    UiOps.ExportHtmlFontSize,
    UiOps.ExportHtmlNumbers,
    GetAppColor('ExportHtmlBg'),
    GetAppColor('ExportHtmlNumbers')
    );

  //restore caret
  Ed.DoCaretSingle(NX, NY);
  Ed.DoEventCarets;
  Ed.Update;
  UpdateFrameEx(F, true);

  if MsgBox(msgConfirmOpenCreatedDoc, MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
    OpenDocument(SFileName);
end;


function TfmMain.DoDialogMenuApi(const AText, ACaption: string;
  AMultiline: boolean; AInitIndex: integer; ANoFuzzy, ANoFullFilter,
  AShowCentered: boolean): integer;
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

    UpdateInputForm(Form);
    if AShowCentered then
      Form.Position:= poScreenCenter;

    Form.ListCaption:= ACaption;
    Form.Multiline:= AMultiline;
    Form.InitItemIndex:= AInitIndex;
    Form.DisableFuzzy:= ANoFuzzy;
    Form.DisableFullFilter:= ANoFullFilter;

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
  Items: TStringList;
  NResult, i: integer;
  S: string;
begin
  FFindFilesInDir(GetAppPath(cDirDataLangs), '*.ini', FListLangs);
  if FListLangs.Count=0 then exit;
  FListLangs.Sort;

  Items:= TStringList.Create;
  try
    Items.Add(cEnLang);
    for i:= 0 to FListLangs.Count-1 do
    begin
      S:= ExtractFileNameOnly(FListLangs[i]);
      if S='translation template' then Continue;
      Items.Add(S);
    end;

    NResult:= DoDialogMenuList(msgMenuTranslations, Items);
    if NResult<0 then exit;

    if Items[NResult]=cEnLang then
    begin
      AppLangName:= '';
      MsgBox('English translation will be applied after program restart', MB_OK or MB_ICONINFORMATION);
    end
    else
    begin
      AppLangName:= Items[NResult];
      DoLocalize;
    end;

    DoPyEvent(CurrentEditor, cEventOnState, [IntToStr(APPSTATE_LANG)]);
  finally
   FreeAndNil(Items);
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
  if Assigned(Frame) then
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
  NIndex, NTag: integer;
  SText: string;
  Ed: TATSynEdit;
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
  NTag:= PtrInt(Prop^.Listbox.Items.Objects[NIndex]);

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
    DoPyEvent(Ed, cEventOnOutputNav,
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
  if AIndex<0 then exit;

  DoParseOutputLine(Prop^, Prop^.Listbox.Items[AIndex], ResFilename, ResLine, ResCol);
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
    C.Font.Color:= GetAppColor('ListSelFont');
    C.Brush.Color:= GetAppColor('ListSelBg');
    C.FillRect(ARect);
  end;

  C.TextOut(ARect.Left+cDx, ARect.Top+cDy, Prop^.Listbox.Items[AIndex]);
end;


procedure TfmMain.DoGotoDefinition;
begin
  if DoPyEvent(CurrentEditor, cEventOnGotoDef, [])<>cPyTrue then
    MsgStatus(msgStatusNoGotoDefinitionPlugins);
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
    DoLocalize_FormCharmap(fmCharmaps);
  end;

  fmCharmaps.InitialStr:= Utf8Encode(Widestring(EditorGetCurrentChar(CurrentEditor)));
  fmCharmaps.Show;
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
    SPLITTER_SIDE: GetSp(SplitterVert);
    SPLITTER_BOTTOM: GetSp(SplitterHorz);
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
    SPLITTER_SIDE: SetSp(SplitterVert);
    SPLITTER_BOTTOM: SetSp(SplitterHorz);
    SPLITTER_G1: SetSp(Groups.Splitter1);
    SPLITTER_G2: SetSp(Groups.Splitter2);
    SPLITTER_G3: SetSp(Groups.Splitter3);
    SPLITTER_G4: SetSp(Groups.Splitter4);
    SPLITTER_G5: SetSp(Groups.Splitter5);
  end;
end;

procedure TfmMain.FrameLexerChange(Sender: TObject);
var
  Frame: TEditorFrame;
begin
  Frame:= (Sender as TComponent).Owner as TEditorFrame;

  DoOps_LoadOptionsLexerSpecific(Frame); //options override
  DoPyEvent(Frame.Editor, cEventOnLexer, []);
  DoOps_LoadKeymap; //keymap override

  UpdateMenuPlugins_Shortcuts;
end;


procedure TfmMain.DoToolbarClick(Sender: TObject);
var
  SData: string;
  NCmd: integer;
begin
  //str(int_command) or callback string
  SData:= (Sender as TATButton).DataString;
  NCmd:= StrToIntDef(SData, 0);

  if NCmd>0 then
    CurrentEditor.DoCommand(NCmd)
  else
    DoPyCallbackFromAPI(SData, []);

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

  with GetPythonEngine do
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

  with GetPythonEngine do
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
      UpdateMenuRecent(nil, '');
    end
    else
    if AMenuCmd='_plugins' then
    begin
      mnuPlugins:= mi;
      TAppMenuProps(mi.Tag).CommandString:= 'plugins';
      UpdateMenuPlugins;
    end
    else
    if AMenuCmd='_lexers' then
    begin
      mnuLexers:= mi;
      UpdateMenuLexers;
    end
    else
    if AMenuCmd='_oplugins' then
    begin
      mnuOpPlugins:= mi;
      UpdateMenuPlugins;
    end
    else
    if AMenuCmd='_enc' then
    begin
      mnuFileEnc:= mi;
      UpdateMenuEnc(mi);
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
begin
  DoPyCommand('cuda_new_file', 'menu', []);
end;

procedure TfmMain.DoCommandsMsgStatus(Sender: TObject; const ARes: string);
begin
  MsgStatus(ARes);
end;

function TfmMain.LiteLexer_GetStyleHash(Sender: TObject; const AStyleName: string): integer;
var
  st: TecSyntaxFormat;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to AppTheme.Styles.Count-1 do
  begin
    st:= TecSyntaxFormat(AppTheme.Styles[i]);
    if AStyleName=st.DisplayName then
      exit(i);
  end;
end;

procedure TfmMain.LiteLexer_ApplyStyle(Sender: TObject; AStyleHash: integer;
  var APart: TATLinePart);
var
  st: TecSyntaxFormat;
begin
  if AStyleHash<0 then exit;
  st:= TecSyntaxFormat(AppTheme.Styles[AStyleHash]);
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

procedure TfmMain.MsgLogConsole(const AText: string);
begin
  if UiOps.LogConsole then
    MsgLogToFilename(AText, FFileNameLogConsole, false);

  //if DoOnConsolePrint(AText) then //disabled on_console_print, later will delete it
    begin
      fmConsole.DoAddLine(AText);
      fmConsole.DoUpdate;;
    end;
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

    if F.LexerName[F.Ed1]=ALexerName then
      F.Lexer[F.Ed1]:= nil;

    if not F.EditorsLinked then
      if F.LexerName[F.Ed2]=ALexerName then
        F.Lexer[F.Ed2]:= nil;
  end;
end;


function TfmMain.GetFloatSide: boolean;
begin
  Result:= Assigned(FFormFloatSide) and
    (PanelLeft.Parent=FFormFloatSide);
end;

procedure TfmMain.SetFloatSide(AValue: boolean);
begin
  if GetFloatSide=AValue then exit;

  if not Assigned(FFormFloatSide) then
  begin
    FFormFloatSide:= TForm.CreateNew(Self);
    FFormFloatSide.Position:= poDesigned;
    FFormFloatSide.BoundsRect:= FBoundsFloatSide;
    FFormFloatSide.BorderIcons:= [biSystemMenu, biMaximize];
    FFormFloatSide.ShowInTaskBar:= stNever;
    FFormFloatSide.OnClose:= @FormFloatSideOnClose;
  end;

  PanelLeftTitle.Visible:= not AValue;
  FFormFloatSide.Visible:= AValue;
  FFormFloatSide.Caption:= FLastSidebarPanel + ' - ' + msgTitle;

  if AValue then
  begin
    PanelLeft.Parent:= FFormFloatSide;
    PanelLeft.Align:= alClient;
    PanelLeft.Show;
    SplitterVert.Hide;
  end
  else
  begin
    PanelLeft.Align:= alLeft;
    PanelLeft.Parent:= PanelMain;
    SplitterVert.Visible:= PanelLeft.Visible;
    SplitterVert.Left:= PanelLeft.Width;
  end
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


function TfmMain.GetFloatBottom: boolean;
begin
  Result:= Assigned(FFormFloatBottom) and
    (PanelBottom.Parent=FFormFloatBottom);
end;

procedure TfmMain.UpdateBottomLayout(ASetFloating: boolean);
begin
  if ASetFloating then
  begin
    PanelBottom.Parent:= FFormFloatBottom;
    PanelBottom.Align:= alClient;
    PanelBottom.Show;
    SplitterHorz.Hide;
  end
  else
  begin
    if UiOps.ConsoleCompact then
      PanelBottom.Parent:= PanelEditors
    else
      PanelBottom.Parent:= PanelAll;

    PanelBottom.Align:= alBottom;
    SplitterHorz.Parent:= PanelBottom.Parent;
    SplitterHorz.Visible:= PanelBottom.Visible;
    SplitterHorz.Top:= PanelBottom.Top-8;
  end;
end;

procedure TfmMain.SetFloatBottom(AValue: boolean);
begin
  if GetFloatBottom=AValue then exit;

  if not Assigned(FFormFloatBottom) then
  begin
    FFormFloatBottom:= TForm.CreateNew(Self);
    FFormFloatBottom.Position:= poDesigned;
    FFormFloatBottom.BoundsRect:= FBoundsFloatBottom;
    FFormFloatBottom.BorderIcons:= [biSystemMenu, biMaximize];
    FFormFloatBottom.ShowInTaskBar:= stNever;
    FFormFloatBottom.OnClose:= @FormFloatBottomOnClose;
  end;

  FFormFloatBottom.Visible:= AValue;
  FFormFloatBottom.Caption:= FLastBottomPanel + ' - ' + msgTitle;

  UpdateBottomLayout(AValue);
end;


procedure TfmMain.FormFloatBottomOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UpdateMenuItemChecked(mnuViewBottom, mnuViewBottom_Alt, false);
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

procedure TfmMain.FormFloatSideOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  UpdateMenuItemChecked(mnuViewSide, mnuViewSide_Alt, false);
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


procedure TfmMain.InitFloatGroup(var F: TForm; var G: TATGroups;
  ATag: integer;
  const ARect: TRect;
  AOnClose: TCloseEvent;
  AOnGroupEmpty: TNotifyEvent);
begin
  if not Assigned(F) then
  begin
    F:= TForm.CreateNew(Self);
    F.Hide;
    F.Position:= poDesigned;
    F.BoundsRect:= ARect;
    F.BorderIcons:= [biSystemMenu, biMaximize, biMinimize];
    F.OnClose:= AOnClose;
    F.Caption:= msgTitle + Format(' [f%d]', [ATag]);

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
  SCmd, SModule, SProc: string;
begin
  SCmd:= (Sender as TMenuItem).Hint;
  SModule:= SGetItem(SCmd);
  SProc:= SGetItem(SCmd);
  DoPyCommand(SModule, SProc, []);
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

    with GetPythonEngine do
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
    LexerProgress.Show;
  end
  else
    LexerProgress.Hide;
end;

(*
procedure TfmMain.DoOnLexerParseProgress(Sender: TObject; ALineIndex, ALineCount: integer);
begin
  if Application.Terminated then exit;
  FLexerProgressIndex:= ALineIndex;
  FLexerProgressCount:= ALineCount;
  TThread.Queue(nil, @DoOnLexerParseProgress_Sync);
end;

procedure TfmMain.DoOnLexerParseProgress_Sync();
begin
  if Application.Terminated then exit;
  LexerProgress.Progress:= FLexerProgressIndex*100 div FLexerProgressCount;
  LexerProgress.Show;
end;
*)

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

    iTab:= DoDialogMenuList(msgPanelTabs, FrameList, true);
    if iTab<0 then exit;

    F:= FrameList.Objects[iTab] as TEditorFrame;
    SetFrame(F);
    F.SetFocus;
  finally
    FreeAndNil(FrameList);
  end;
end;


procedure TfmMain.FinderOnGetToken(Sender: TObject;
  AX, AY: integer;
  out AKind: TATFinderTokenKind);
var
  Ed: TATSynEdit;
  Frame: TEditorFrame;
begin
  Ed:= Sender as TATSynEdit;
  Frame:= GetEditorFrame(Ed);
  if Assigned(Frame) then
    Frame.GetEditorToken(Ed, AX, AY, AKind)
  else
    AKind:= cTokenKindOther;
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

