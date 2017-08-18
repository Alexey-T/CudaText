(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus,
  Clipbrd, StrUtils, Variants, IniFiles,
  LclType, LclProc, LclIntf,
  LazFileUtils, LazUTF8, FileUtil,
  {$ifdef LCLGTK2}
  fix_gtk_clipboard,
  {$endif}
  fix_focus_window,
  jsonConf,
  PythonEngine,
  UniqueInstance,
  ecSyntAnal,
  ATButtons,
  ATButtonsToolbar,
  ATListbox,
  ATScrollbar,
  ATPanelSimple,
  ATSynEdit,
  ATSynEdit_ScrollBar,
  ATSynEdit_Keymap,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Commands,
  ATSynEdit_Finder,
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_Export_HTML,
  ATSynEdit_Ranges,
  ATSynEdit_Gaps,
  ATSynEdit_CanvasProc,
  ATSynEdit_Adapter_EControl,
  ATTabs,
  ATGroups,
  ATStatusBar,
  ATStrings,
  ATStringProc,
  ATGauge,
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
  formlexerstylesload,
  formlexerstylemap,
  formcolorsetup,
  formabout,
  formcharmaps,
  formkeyinput, form_addon_report,
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

type
  { TfmMain }
  TfmMain = class(TForm)
    AppProps: TApplicationProperties;
    ButtonCancel: TATButton;
    ImageListTabs: TImageList;
    ImageListToolbar: TImageList;
    mnuViewUnpriSpacesTail: TMenuItem;
    mnuViewMicromap: TMenuItem;
    mnuHelpCheckUpd: TMenuItem;
    StatusProgress: TGauge;
    LabelSideTitle: TLabel;
    MenuItem4: TMenuItem;
    mnuViewDistFree: TMenuItem;
    SepV4: TMenuItem;
    mnuBmPlaceOnCarets: TMenuItem;
    mnuFileNewMenu: TMenuItem;
    mnuPlugEmpty: TMenuItem;
    ImageListSide: TImageList;
    FontDlg: TFontDialog;
    ImageListBm: TImageList;
    ImageListTree: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mnuBmDeleteLines: TMenuItem;
    mnuBmCopyLines: TMenuItem;
    mnuOpThemeSyntax: TMenuItem;
    mnuThemesSyntax: TMenuItem;
    mnuBmPlaceCarets: TMenuItem;
    PaintTest: TPaintBox;
    PanelAll: TATPanelSimple;
    PanelBottom: TATPanelSimple;
    PanelLeft: TATPanelSimple;
    PanelLeftTitle: TATPanelSimple;
    PanelMain: TATPanelSimple;
    PanelSide: TATPanelSimple;
    SepV3: TMenuItem;
    mnuLexers: TMenuItem;
    mnuHelpIssues: TMenuItem;
    mnuOpLexMap: TMenuItem;
    mnuTst2: TMenuItem;
    mnuLang: TMenuItem;
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
    mnuThemesUI: TMenuItem;
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
    mnuOpThemeUi: TMenuItem;
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
    mnuFileCloseDel: TMenuItem;
    mnuOpLexer: TMenuItem;
    mnuOpMore: TMenuItem;
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
    TimerTreeFocus: TTimer;
    ToolbarMain: TATButtonsToolbar;
    ToolbarSideMid: TATButtonsToolbar;
    ToolbarSideLow: TATButtonsToolbar;
    ToolbarSideTop: TATButtonsToolbar;
    UniqInstance: TUniqueInstance;
    procedure AppPropsActivate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure DoOnTabOver(Sender: TObject; ATabIndex: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var ACanClose: boolean);
    procedure FormColorsApply(const AColors: TAppTheme);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FrameAddRecent(Sender: TObject);
    procedure FrameOnChangeCaretPos(Sender: TObject);
    procedure FrameParseBegin(Sender: TObject);
    procedure FrameParseDone(Sender: TObject);
    procedure ListboxOutClick(Sender: TObject);
    procedure ListboxOutDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
    procedure ListboxOutKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuThemesSyntaxClick(Sender: TObject);
    procedure mnuTabColorClick(Sender: TObject);
    procedure mnuTabsize1Click(Sender: TObject);
    procedure mnuTabsize2Click(Sender: TObject);
    procedure mnuTabsize4Click(Sender: TObject);
    procedure mnuTabsize8Click(Sender: TObject);
    procedure MenuRecentsClear(Sender: TObject);
    procedure mnuFind2NextClick(Sender: TObject);
    procedure mnuFind2PrevClick(Sender: TObject);
    procedure mnuFind2WordNextClick(Sender: TObject);
    procedure mnuFind2WordPrevClick(Sender: TObject);
    procedure DoHelpAbout;
    procedure DoHelpForum;
    procedure DoHelpChangelog;
    procedure DoHelpMouse;
    procedure DoHelpWiki;
    procedure DoHelpLexers;
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
    procedure PopupTabPopup(Sender: TObject);
    procedure PopupTextPopup(Sender: TObject);
    procedure PythonEngineAfterInit(Sender: TObject);
    procedure PythonIOSendData(Sender: TObject; const Data: AnsiString);
    procedure PythonIOSendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PythonModInitialization(Sender: TObject);
    procedure StatusPanelClick(Sender: TObject; AIndex: Integer);
    procedure TimerCmdTimer(Sender: TObject);
    procedure TimerStatusAltTimer(Sender: TObject);
    procedure TimerStatusTimer(Sender: TObject);
    procedure TimerTreeFillTimer(Sender: TObject);
    procedure TimerTreeFocusTimer(Sender: TObject);
    procedure UniqInstanceOtherInstance(Sender: TObject; ParamCount: Integer;
      Parameters: array of String);
  private
    { private declarations }
    FListRecents: TStringList;
    FListThemesUI: TStringList;
    FListThemesSyntax: TStringList;
    FListLangs: TStringList;
    FListTimers: TStringList;
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
    mnuViewSide_Alt,
    mnuViewBottom_Alt,
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
    FAllowEventOnOpenBefore: boolean;
    FAllowLoadKeymap: boolean;
    FHandledOnShow: boolean;
    FFileNamesDroppedInitially: array of string;
    FTreeClick: boolean;
    FNewClickedEditor: TATSynEdit;
    FPyComplete_Text: string;
    FPyComplete_CharsLeft: integer;
    FPyComplete_CharsRight: integer;
    FPyComplete_CaretPos: TPoint;
    FLastDirOfOpenDlg: string;
    FLastLexerForPluginsMenu: string;
    FLastSidebarPanel: string;
    FLastBottomPanel: string;
    FLastSelectedCommand: integer;
    FOption_OpenReadOnly: boolean;
    FOption_OpenNewWindow: boolean;
    FOption_WindowPos: string;
    FOption_Encoding: string;

    procedure DoCommandsMsgStatus(Sender: TObject; const ARes: string);
    procedure DoFindMarkingInit(AMode: TATFindMarkingMode);
    procedure DoFindOptions_OnChange(Sender: TObject);
    procedure DoFindOptions_ResetInSelection;
    procedure DoFindOptions_GetStrings(out AFind, AReplace: string);
    procedure DoShowBottomPanel(const ATabCaption: string);
    function DoSidebar_FilenameToImageIndex(ATabCaption, AFilename: string): integer;
    procedure DoSidebar_InitPanelForm(var AItem: TAppSidePanel;
      const ACaption: string; AForm: TCustomForm; AParent: TWinControl);
    procedure DoSidebar_ListboxDrawItem(Sender: TObject; C: TCanvas;
      AIndex: integer; const ARect: TRect);
    function DoSidebar_TranslatedCaption(const ACaption: string): string;
    //procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
    //  MaxWidth, MaxHeight: TConstraintSize);
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
    procedure DoMenuClear(const AMenuId: string);
    function DoMenuEnum_New(const AMenuId: string): PPyObject;
    procedure DoOnTabMove(Sender: TObject; NFrom, NTo: Integer);
    procedure DoPanel_TreeviewOnDblClick(Sender: TObject);
    procedure DoPanel_TreeviewOnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoPanel_TreeviewOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoSidebar_OnTabClick(Sender: TObject);
    function DoSidebar_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
    function DoSidebar_AddTab(const ACaption: string;
      AImageIndex: integer; AHandle: PtrInt): boolean;
    function DoSidebar_RemoveTab(const ACaption: string): boolean;
    function DoSidebar_CaptionToPanelsIndex(const Str: string): integer;
    function DoSidebar_CaptionToTabIndex(const Str: string): integer;
    function DoSidebar_CaptionToControlHandle(const ACaption: string): PtrInt;
    procedure DoBottom_OnTabClick(Sender: TObject);
    procedure DoBottom_AddonsClick(Sender: TObject);
    procedure DoBottom_FindClick(Sender: TObject);
    procedure DoBottom_GotoClick(Sender: TObject);
    function DoBottom_CaptionToControlHandle(const ACaption: string): PtrInt;
    function DoBottom_AddTab(const ACaption: string;
      AImageIndex: integer; AHandle: PtrInt): boolean;
    function DoBottom_CaptionToPanelsIndex(const Str: string): integer;
    function DoBottom_ActivateTab(const ACaption: string): boolean;
    function DoBottom_CaptionToTabIndex(const ACaption: string): integer;
    function DoBottom_RemoveTab(const ACaption: string): boolean;
    procedure DoAutoComplete;
    procedure DoCudaLibAction(const AMethod: string);
    procedure DoDialogCharMap;
    procedure DoFindActionFromString(AStr: string);
    procedure DoFindOptionsFromString(const S: string);
    function DoFindOptionsToString: string;
    procedure DoGotoDefinition;
    procedure DoShowFuncHint;
    procedure DoApplyGutterVisible(AValue: boolean);
    procedure DoApplyFrameOps(F: TEditorFrame; const Op: TEditorOps; AForceApply: boolean);
    procedure DoApplyFont_Text;
    procedure DoApplyFont_Ui;
    procedure DoApplyFont_Output;
    procedure DoApplyAllOps;
    procedure DoApplyTheme;
    procedure DoClearRecentFileHistory;
    function DoOnConsole(const Str: string): boolean;
    function DoOnConsoleNav(const Str: string): boolean;
    function DoOnMacro(const Str: string): boolean;
    function DoDialogConfigTheme(var AData: TAppTheme; AThemeUI: boolean): boolean;
    function DoDialogMenuApi(const AText, ACaption: string; AMultiline: boolean;
      AInitIndex: integer): integer;
    procedure DoFileExportHtml;
    procedure DoFileInstallZip(const fn: string; out DirTarget: string; ASilent: boolean);
    procedure DoFileCloseAndDelete;
    procedure DoFileNewMenu(Sender: TObject);
    procedure DoFileNewFrom(const fn: string);
    procedure DoFileSave;
    procedure DoFileSaveAs;
    procedure DoSwitchActiveTab(ANext: boolean);
    procedure DoPyTimerTick(Sender: TObject);
    procedure DoPyRunLastPlugin;
    procedure DoPyResetPlugins;
    procedure DoPyRescanPlugins;
    procedure DoPyStringToEvents(const AEventStr: string;
      out AEvents: TAppPyEvents;
      out AEventsPrior: TAppPyEventsPrior);
    procedure DoPyUpdateEvents(const AModuleName, AEventStr, ALexerStr, AKeyStr: string);
    function DoSplitter_StringToId(const AStr: string): integer;
    procedure DoSplitter_GetInfo(const Id: integer;
      out BoolVert, BoolVisible: boolean; out NPos, NTotal: integer);
    procedure DoSplitter_SetInfo(const Id: integer; NPos: integer);
    procedure DoToolbarClick(Sender: TObject);
    procedure FrameLexerChange(Sender: TObject);
    procedure FrameOnEditorClickEndSelect(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure FrameOnEditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure InitAppleMenu;
    procedure InitSidebar;
    procedure InitToolbar;
    function IsAllowedToOpenFileNow: boolean;
    function IsThemeNameExist(const AName: string; AThemeUI: boolean): boolean;
    procedure MenuEncWithReloadClick(Sender: TObject);
    procedure MenuLangClick(Sender: TObject);
    procedure MenuPluginClick(Sender: TObject);
    procedure MenuThemeDefaultUiClick(Sender: TObject);
    procedure MenuThemeDefaultSyntaxClick(Sender: TObject);
    procedure MenuThemesUiClick(Sender: TObject);
    procedure MsgStatusAlt(const AText: string; ASeconds: integer);
    procedure SetSidebarPanel(const ACaption: string);
    procedure SetShowDistractionFree(AValue: boolean);
    procedure SetShowFullScreen(AValue: boolean);
    procedure SetFullScreen_Ex(AValue: boolean; AHideAll: boolean);
    procedure SetFullScreen_Universal(AValue: boolean);
    procedure SetFullScreen_Win32(AValue: boolean);
    procedure SetThemeSyntax(const AValue: string);
    procedure SetThemeUi(const AValue: string);
    function SFindOptionsToTextHint: string;
    procedure StatusResize(Sender: TObject);
    procedure DoTreeGetSyntaxRange(ANode: TTreeNode; out P1, P2: TPoint);
    procedure DoOps_ShowEventPlugins;
    procedure DoOps_ResetLexerSpecificOptions;
    procedure DoOps_LoadPluginFromInf(const fn_inf: string);
    procedure DoOps_LoadSidebarIcons;
    procedure DoOps_LoadTreeIcons;
    procedure DoOps_LoadCommandLineOptions;
    procedure DoOps_LoadLexerLib;
    procedure DoOps_SaveHistory;
    procedure DoOps_SaveHistory_GroupView(c: TJsonConfig);
    procedure DoOps_LoadHistory;
    procedure DoOps_LoadHistory_GroupView(c: TJsonConfig);
    procedure DoOps_LoadHistory_AfterOnStart;
    function DoOps_SaveSession(fn_session: string): boolean;
    function DoOps_LoadSession(fn_session: string): boolean;
    procedure DoOps_LoadOptionsAndApplyAll;
    procedure DoOps_LoadOptionsLexerSpecific(F: TEditorFrame);
    procedure DoOps_OpenFile_FileTypes;
    procedure DoOps_OpenFile_LexerSpecific;
    procedure DoOps_LoadPlugins;
    procedure DoOps_DialogFont(var OpName: string; var OpSize: integer;
      const AConfigStrName, AConfigStrSize: string);
    procedure DoOps_DialogFont_Text;
    procedure DoOps_DialogFont_Ui;
    procedure DoOps_DialogFont_Output;
    procedure DoOps_OpenFile_Default;
    procedure DoOps_OpenFile_User;
    procedure DoOps_LoadOptions(const fn: string; var Op: TEditorOps);
    procedure DoOps_LoadKeymap;
    procedure DoOps_LoadKeymapFrom(const AFilenameKeymap: string; AUndoList: TATKeymapUndoList);
    procedure DoEditorsLock(ALock: boolean);
    procedure DoFindCurrentWordOrSel(ANext: boolean; AWordOrSel: boolean);
    procedure DoCopyFilenameDir;
    procedure DoCopyFilenameFull;
    procedure DoCopyFilenameName;
    procedure DoCopyLine;
    procedure DoDialogCommands;
    function DoDialogCommands_Custom(AShowUsual, AShowPlugins, AShowLexers,
      AAllowConfig: boolean; AFocusedCommand: integer): integer;
    function DoDialogCommands_Py(AShowUsual, AShowPlugins, AShowLexers, AAllowConfig: boolean): string;
    procedure DoDialogGoto;
    procedure DoDialogGoto_Hide;
    procedure DoDialogGotoBookmark;
    function DoDialogSaveTabs: boolean;
    procedure DoDialogLexerProp(an: TecSyntAnalyzer);
    procedure DoDialogLexerLib;
    procedure DoDialogLexerMap;
    procedure DoDialogRestoreLexerStyles;
    procedure DoDialogTheme(AThemeUI: boolean);
    procedure DoShowConsole(AFocusEdit: boolean);
    procedure DoShowOutput;
    procedure DoShowValidate;
    procedure DoShowSearchResults;
    procedure DoShowSidePanel(const ATabCaption: string; AndFocus: boolean);
    function FrameOfPopup: TEditorFrame;
    procedure FrameOnCommand(Sender: TObject; ACommand: integer; const AText: string;
      var AHandled: boolean);
    function DoFileCloseAll: boolean;
    procedure DoDialogFind(AReplaceMode: boolean);
    procedure DoDialogFind_Hide;
    procedure DoFindResult(ok: boolean);
    procedure DoFindFirst;
    procedure DoFindNext(ANext: boolean);
    procedure DoFindMarkAll(AMode: TATFindMarkingMode);
    procedure DoMoveTabTo(Num: Integer);
    procedure DoOnTabPopup(Sender: TObject);
    function DoFileOpen(AFilename: string; APages: TATPages=nil; const AArgs: string=''): TEditorFrame;
    procedure DoFileOpenDialog;
    procedure DoFileOpenDialog_NoPlugins;
    procedure DoFileSaveAll;
    procedure DoFileReopen;
    procedure DoLoadCommandLine;
    procedure DoToggleFullScreen;
    procedure DoToggleDistractionFree;
    procedure DoToggleSidePanel;
    procedure DoToggleBottomPanel;
    procedure DoToggleFindDialog;
    procedure DoToggleToolbar;
    procedure DoToggleStatusbar;
    procedure FindDialogDone(Sender: TObject; const Res: string);
    procedure FinderOnFound(Sender: TObject; APos1, APos2: TPoint);
    procedure FinderOnBadRegex(Sender: TObject);
    procedure FinderOnConfirmReplace(Sender: TObject; APos1, APos2: TPoint;
      AForMany: boolean; var AConfirm, AContinue: boolean);
    procedure FinderOnProgress(Sender: TObject; ACurPos, AMaxPos: integer; var AContinue: boolean);
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
    procedure GotoDialogDone(Sender: TObject; const Res: string);
    procedure InitFormFind;
    function IsFocusedBottom: boolean;
    function IsFocusedFind: boolean;
    function IsLexerMatches(const ANameList: string): boolean;
    procedure PyCompletionOnGetProp(Sender: TObject; out AText: string;
      out ACharsLeft, ACharsRight: integer);
    procedure PyCompletionOnResult(Sender: TObject;
      const ASnippetId: string; ASnippetIndex: integer);
    procedure DoPyCommand_ByPluginIndex(AIndex: integer);
    procedure SetFrameEncoding(Frame: TEditorFrame; const AEnc: string;
      AAlsoReloadFile: boolean);
    procedure SetLexerIndex(N: integer);
    procedure SetShowStatus(AValue: boolean);
    procedure SetShowToolbar(AValue: boolean);
    procedure SetShowBottom(AValue: boolean);
    procedure SetShowSideBar(AValue: boolean);
    procedure SetShowSidePanel(AValue: boolean);
    procedure SetShowTabsMain(AValue: boolean);
    procedure SplitterOnPaint_Gr(Sender: TObject);
    procedure SplitterOnPaint_Main(Sender: TObject);
    procedure UpdateBottomPanels(const ACaption: string);
    procedure UpdateEditorTabsize(N: integer);
    procedure UpdateKeymapDynamicItems;
    procedure UpdateMenuItemAltObject(mi: TMenuItem; cmd: integer);
    procedure UpdateMenuItemHint(mi: TMenuItem; const AHint: string);
    procedure UpdateMenuItemHotkey(mi: TMenuItem; cmd: integer);
    procedure UpdateMenuLangs(sub: TMenuItem);
    procedure UpdateMenuThemes(AThemeUI: boolean);
    procedure UpdateMenuLexersTo(AMenu: TMenuItem);
    procedure UpdateMenuRecent(F: TEditorFrame);
    procedure UpdateMenuHotkeys;
    procedure UpdateMenuLexers;
    procedure UpdateMenuPlugins;
    procedure UpdateMenuPlugins_Shortcuts(AForceUpdate: boolean=false);
    procedure UpdateMenuChecks;
    procedure UpdateMenuEnc(AMenu: TMenuItem);
    procedure DoApplyUiOps;
    procedure InitPyEngine;
    procedure FrameOnChangeCaption(Sender: TObject);
    procedure FrameOnUpdateStatus(Sender: TObject);
    function DoTabAdd(APages: TATPages; const ACaption: string): TATTabData;
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
    procedure SetLineEnds(Val: TATLineEnds);
    procedure MsgStatus(const AText: string);
    procedure UpdateSidebarButtons;
    procedure UpdateSidebarPanels(const ACaption: string; AndFocus: boolean);
    procedure UpdateStatusbarPanelAutosize;
    procedure UpdateStatusbarPanelsFromString(AStr: string);
    procedure UpdateBottomButtons;
    procedure UpdateStatus_ToolButton(AToolbar: TATButtonsToolbar; ACmd: integer;
      AChecked: boolean);
    procedure UpdateTabsActiveColor(F: TEditorFrame);
    procedure UpdateTree(AFill: boolean; AConsiderTreeVisible: boolean=true; AForceUpdateAll: boolean=false);
    procedure UpdateCaption;
    procedure UpdateEnabledAll(b: boolean);
    procedure InitFrameEvents(F: TEditorFrame);
    procedure UpdateInputForm(Form: TForm);
    procedure UpdateFrame(AUpdatedText: boolean= false);
    procedure UpdateAppForSearch(AStart, AEdLock, AFindMode: boolean);
    procedure UpdateStatus;
    procedure InitStatusButton;
  public
    { public declarations }
    Tree: TTreeViewMy;
    ListboxOut: TATListbox;
    ListboxVal: TATListbox;
    function FrameCount: integer;
    property Frames[N: integer]: TEditorFrame read GetFrame;
    function CurrentFrame: TEditorFrame;
    function CurrentEditor: TATSynEdit;
    function GetEditorFrame(Ed: TATSynEdit): TEditorFrame;
    function GetEditorBrother(Ed: TATSynEdit): TATSynEdit;
    property ShowFullscreen: boolean read FShowFullScreen write SetShowFullScreen;
    property ShowDistractionFree: boolean read FShowFullScreen write SetShowDistractionFree;
    property ShowSideBar: boolean read GetShowSideBar write SetShowSideBar;
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
    //function DoPyCallbackFromAPI(const ACallback: string; const AParams: array of string): string;
  end;

var
  fmMain: TfmMain;


implementation

{$R *.lfm}

{ TfmMain }

{$I formmain_py_toolbars.inc}
{$I formmain_py_api.inc}
{$I formmain_py_helpers.inc}
{$I formmain_py_pluginwork.inc}


procedure TfmMain.StatusPanelClick(Sender: TObject; AIndex: Integer);
begin
  if not CurrentFrame.IsText then exit;

  if AIndex=StatusbarIndex_Enc then
  begin
    if not CurrentFrame.ReadOnly then
      PopupEnc.PopUp;
  end
  else
  if AIndex=StatusbarIndex_LineEnds then
  begin
    if not CurrentFrame.ReadOnly then
      PopupEnds.PopUp;
  end
  else
  if AIndex=StatusbarIndex_Lexer then
  begin
    PopupLex.PopUp;
  end
  else
  if AIndex=StatusbarIndex_TabSize then
  begin
    PopupTabSize.Popup;
  end
  else
  if AIndex=StatusbarIndex_SelMode then
  begin
    with CurrentEditor do
    begin
      OptMouseColumnSelectionWithoutKey:= not OptMouseColumnSelectionWithoutKey;
      UpdateStatus;
    end;
  end
  else
  if AIndex=StatusbarIndex_WrapMode then
  begin
    //loop: no wrap - wrap at window - wrap at margin
    with CurrentEditor do
    begin
      if OptWrapMode=High(OptWrapMode) then
        OptWrapMode:= Low(OptWrapMode)
      else
        OptWrapMode:= Succ(OptWrapMode);
      UpdateStatus;
    end;
  end;
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

procedure TfmMain.DoPanel_TreeviewOnDblClick(Sender: TObject);
var
  R: TecTextRange;
  P: TPoint;
begin
  if Tree.Selected=nil then exit;
  if Tree.Selected.Data=nil then exit;
  R:= TecTextRange(Tree.Selected.Data);
  P:= CurrentFrame.Adapter.TreeGetPositionOfRange(R);

  FTreeClick:= true;
  CurrentEditor.DoGotoPos(
    P,
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );
  EditorFocus(CurrentEditor);
  FTreeClick:= false;
end;

procedure TfmMain.DoTreeGetSyntaxRange(ANode: TTreeNode; out P1, P2: TPoint);
var
  R: TecTextRange;
begin
  P1:= Point(-1, -1);
  P2:= Point(-1, -1);
  if ANode=nil then exit;
  if ANode.Data=nil then exit;
  R:= TecTextRange(ANode.Data);
  CurrentFrame.Adapter.TreeGetPositionOfRange(R, P1, P2);
end;

procedure TfmMain.DoPanel_TreeviewOnMouseMove(Sender: TObject; Shift: TShiftState; X,
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
    if FileExistsUTF8(SFilename) then
    begin
      Frame:= DoFileOpen(SFilename);
      if Assigned(Frame) and (NLine>0) then
        Frame.DoGotoPos(NColumn-1, NLine-1);
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

function TfmMain.GetSessionFilename: string;
begin
  if FSessionName<>'' then
  begin
    Result:= FSessionName;
    if ExtractFileDir(Result)='' then
      Result:= GetAppPath(cDirSettings)+DirectorySeparator+Result;
  end
  else
    Result:= '';
end;


procedure TfmMain.InitAppleMenu;
var
  cAppleString: string;
begin
  {$ifndef darwin}
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
  mnuHelpCheckUpd.Visible:= false;

  //macOS adds Quit item in apple menu
  mnuFileExit.Visible:= false;
end;


procedure TfmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  CustomDialog_DoPyCallback:= @DoPyCallbackFromAPI;

  {$ifdef windows}
  UiOps.ScreenScale:= MulDiv(100, Screen.PixelsPerInch, 96);
  
  //create SimpleIPC server to listen and send window handle
  //so it will switch to this instance
  if IsSetToOneInstance then
  begin
    OneWinInstanceRunning := TUniqueWinInstance.Create(Self);
    OneWinInstanceRunning.Id:='cudatext.2';
    OneWinInstanceRunning.TargetId:='cudatext.1';
    OneWinInstanceRunning.WindowHandle:=Self.Handle;
    OneWinInstanceRunning.StartListening;
  end;
  {$endif}
  //UiOps.ScreenScale:= 200; ////test
  UiOps_ScreenScale:= UiOps.ScreenScale;

  ToolbarMain.ScalePercents:= UiOps.ScreenScale;
  ToolbarSideTop.ScalePercents:= UiOps.ScreenScale;
  ToolbarSideLow.ScalePercents:= UiOps.ScreenScale;
  ToolbarSideMid.ScalePercents:= UiOps.ScreenScale;

  InitAppleMenu;
  InitToolbar;
  InitSidebar;

  Tree:= TTreeViewMy.Create(Self);
  Tree.Parent:= PanelLeft;
  Tree.Align:= alClient;
  Tree.Images:= ImageListTree;
  Tree.OnDblClick:= @DoPanel_TreeviewOnDblClick;
  Tree.OnMouseMove:= @DoPanel_TreeviewOnMouseMove;
  Tree.OnKeyDown:= @DoPanel_TreeviewOnKeyDown;
  Tree.PopupMenu:= PopupTree;

  ListboxOut:= TATListbox.Create(Self);
  ListboxOut.Parent:= PanelBottom;
  ListboxOut.Align:= alClient;
  ListboxOut.CanGetFocus:= true;
  ListboxOut.OwnerDrawn:= true;
  ListboxOut.OnDblClick:= @ListboxOutClick;
  ListboxOut.OnDrawItem:= @ListboxOutDrawItem;
  ListboxOut.OnKeyDown:= @ListboxOutKeyDown;

  ListboxVal:= TATListbox.Create(Self);
  ListboxVal.Parent:= PanelBottom;
  ListboxVal.Align:= alClient;
  ListboxVal.CanGetFocus:= true;
  ListboxVal.OwnerDrawn:= true;
  ListboxVal.OnDblClick:= @ListboxOutClick;
  ListboxVal.OnDrawItem:= @ListboxOutDrawItem;
  ListboxVal.OnKeyDown:= @ListboxOutKeyDown;

  AppBookmarkImagelist.AddImages(ImageListBm);
  for i:= 0 to 9 do
  begin
    AppBookmarkSetup[240+i].Color:= clMoneyGreen;
    AppBookmarkSetup[240+i].ImageIndex:= i;
  end;

  PanelAll.Align:= alClient;
  AppManager:= TecSyntaxManager.Create(Self);
  FSessionName:= 'history session.json';

  FListRecents:= TStringList.Create;
  FListThemesUI:= TStringList.Create;
  FListThemesSyntax:= TStringList.Create;
  FListLangs:= TStringList.Create;
  FListTimers:= TStringList.Create;
  FKeymapUndoList:= TATKeymapUndoList.Create;
  FKeymapLastLexer:= '??'; //not ''
  FAllowLoadKeymap:= false;
  FAllowEventOnOpenBefore:= true;

  FillChar(AppPanelProp_Out, SizeOf(AppPanelProp_Out), 0);
  FillChar(AppPanelProp_Val, SizeOf(AppPanelProp_Val), 0);
  AppPanelProp_Out.Listbox:= ListboxOut;
  AppPanelProp_Val.Listbox:= ListboxVal;

  Status:= TATStatus.Create(Self);
  Status.Parent:= Self;
  Status.ScalePercents:= UiOps.ScreenScale;
  Status.Align:= alBottom;
  Status.Top:= Height;
  Status.Height:= 23;
  Status.IndentLeft:= 2;
  Status.OnPanelClick:= @StatusPanelClick;
  Status.OnResize:= @StatusResize;

  Status.AddPanel(170, saMiddle, '?');
  Status.AddPanel(105, saMiddle, '?');
  Status.AddPanel(50, saMiddle, '?');
  Status.AddPanel(140, saMiddle, '?');
  Status.AddPanel(80, saMiddle, '?');
  Status.AddPanel(4000, saLeft, '');

  StatusAlt:= TATStatus.Create(Self);
  StatusAlt.Parent:= Self;
  StatusAlt.ScalePercents:= UiOps.ScreenScale;
  StatusAlt.Align:= alNone;
  StatusAlt.Height:= Status.Height;
  StatusAlt.IndentLeft:= 0;
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
  Groups.Images:= ImageListTabs;
  Groups.OnTabFocus:= @DoOnTabFocus;
  Groups.OnTabAdd:= @DoOnTabAdd;
  Groups.OnTabClose:= @DoOnTabClose;
  Groups.OnTabMove:= @DoOnTabMove;
  Groups.OnTabPopup:= @DoOnTabPopup;
  Groups.OnTabOver:= @DoOnTabOver;

  with AppSidePanels[0] do
  begin
    ItemCaption:= 'Tree';
    ItemControl:= Tree;
  end;

  FFinder:= TATEditorFinder.Create;
  FFinder.OptRegex:= true;
  FFinder.OnConfirmReplace:= @FinderOnConfirmReplace;
  FFinder.OnProgress:= @FinderOnProgress;
  FFinder.OnBadRegex:= @FinderOnBadRegex;
  FFinder.OnFound:=@FinderOnFound;

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
  UpdateMenuItemHint(mnuPlug, 'plugins');
  UpdateMenuItemHint(mnuFileOpenSub, '_recents');
  UpdateMenuItemHint(mnuFileEnc, '_enc');
  UpdateMenuItemHint(mnuFileEnds, '_ends');
  UpdateMenuItemHint(mnuLexers, '_lexers');
  UpdateMenuItemHint(mnuThemesUI, '_themes-ui');
  UpdateMenuItemHint(mnuThemesSyntax, '_themes-syntax');
  UpdateMenuItemHint(mnuLang, '_langs');
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

procedure TfmMain.AppPropsActivate(Sender: TObject);
begin
  if EditorOps.OpShowCurLineOnlyFocused then
    CurrentEditor.Update;
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var ACanClose: boolean);
begin
  if GetModifiedCount>0 then
    ACanClose:= DoDialogSaveTabs
  else
    ACanClose:= true;
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
  ImageListBm.Clear;
  ImageListSide.Clear;
  ImageListTabs.Clear;
  ImageListTree.Clear;
  ImageListToolbar.Clear;

  {$ifdef windows}
  FreeAndNil(OneWinInstanceRunning);
  {$ifend}

  for i:= 0 to FListTimers.Count-1 do
    TTimer(FListTimers.Objects[i]).Enabled:= false;
  FreeAndNil(FListTimers);

  FreeAndNil(FListRecents);
  FreeAndNil(FListThemesUI);
  FreeAndNil(FListThemesSyntax);
  FreeAndNil(FListLangs);
  FreeAndNil(FKeymapUndoList);
end;

procedure TfmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
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
    if FileExistsUTF8(FileNames[i]) and
      not DirectoryExistsUTF8(FileNames[i]) then
        DoFileOpen(FileNames[i], Pages);
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
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
        EditorFocus(CurrentEditor);
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

  DoOps_LoadCommandLineOptions;
  DoOps_LoadOptions(GetAppPath(cFileOptionsUser), EditorOps);
  DoApplyFont_Text;
  DoApplyFont_Ui;
  DoApplyFont_Output;
  DoApplyUiOps;

  InitPyEngine;
  DoOps_LoadSidebarIcons;
  DoOps_LoadTreeIcons;
  DoOps_LoadLexerLib;
  DoFileOpen('');
  FHandledOnShow:= true;

  DoOps_LoadPlugins;
  DoOps_LoadHistory;
  FAllowLoadKeymap:= true;
  DoOps_LoadKeymap;

  DoPyEvent(CurrentEditor, cEventOnFocus, []);
  DoPyEvent(CurrentEditor, cEventOnStart, []);
  DoOps_LoadHistory_AfterOnStart;

  UpdateMenuPlugins;
  UpdateMenuPlugins_Shortcuts(true);
  UpdateMenuThemes(true);
  UpdateMenuThemes(false);
  UpdateMenuLangs(mnuLang);
  UpdateMenuHotkeys;

  ActiveControl:= CurrentEditor;
  UpdateSidebarButtons;
  UpdateBottomButtons;
  UpdateStatus;
  DoLoadCommandLine;

  if FOption_WindowPos<>'' then
  begin
    Left:= StrToIntDef(SGetItem(FOption_WindowPos), Left);
    Top:= StrToIntDef(SGetItem(FOption_WindowPos), Top);
    Width:= StrToIntDef(SGetItem(FOption_WindowPos), Width);
    Height:= StrToIntDef(SGetItem(FOption_WindowPos), Height);
  end;
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
  DeleteFileUTF8(GetAppPath(cFileOptionsHistoryFiles));
end;

procedure TfmMain.DoFileInstallZip(const fn: string; out DirTarget: string;
  ASilent: boolean);
var
  msg, msg2: string;
  IsOk: boolean;
  AddonType: TAppAddonType;
begin
  DoInstallAddonFromZip(fn, AppManager, GetAppPath(cDirDataAutocomplete), msg, msg2,
    IsOk, AddonType, DirTarget, ASilent);

  if IsOk then
  begin
    if AddonType=cAddonTypeLexer then
    begin
      UpdateMenuLexers;
    end;

    if AddonType=cAddonTypeData then
    begin
      UpdateMenuLangs(mnuLang);
      UpdateMenuThemes(true);
      UpdateMenuThemes(false);
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

function TfmMain.GetShowSideBar: boolean;
begin
  Result:= PanelSide.Visible;
end;

function TfmMain.DoDialogSaveTabs: boolean;
var
  F: TEditorFrame;
  res: TModalResult;
  Form: TfmSaveTabs;
  i: integer;
  SCaption: string;
begin
  Result:= false;
  Form:= TfmSaveTabs.Create(nil);
  with Form do
  try
    DoLocalize_FormSaveTabs(Form);
    List.Clear;
    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      if not F.Modified then Continue;
      SCaption:= F.TabCaption+IfThen(F.Filename<>'', '  ('+ExtractFileDir(F.Filename)+')');
      List.Items.AddObject(SCaption, F);
      List.Checked[List.Count-1]:= true;
    end;

    res:= ShowModal;
    case res of
      mrClose:
        Result:= true;
      mrCancel:
        Result:= false;
      mrNoToAll:
        begin
          Result:= true; //like for mrClose
          UiOps.ShowLastFiles:= false; //dont save tabs to session
        end;
      mrOk:
        begin
          Result:= true;
          for i:= 0 to List.Count-1 do
            if List.Checked[i] then
            begin
              F:= List.Items.Objects[i] as TEditorFrame;
              F.DoFileSave(false);
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
    GetAppPath(cFileLexerStylesBackup)) then
  begin
    DoLexerExportFromLibToFile(an);
    UpdateMenuLexers;
    UpdateStatus;
    UpdateFrame;
  end;
end;

procedure TfmMain.DoDialogLexerLib;
begin
  if DoShowDialogLexerLib(
    GetAppPath(cDirDataAutocomplete),
    EditorOps.OpFontName,
    EditorOps.OpFontSize,
    GetAppPath(cFileLexerStylesBackup)) then
  begin
    UpdateMenuLexers;
    UpdateStatus;
    UpdateFrame;
  end;
end;

procedure TfmMain.DoDialogLexerMap;
var
  i: integer;
begin
  if DoDialogLexerStylesMap(CurrentFrame.Lexer) then
    for i:= 0 to FrameCount-1 do
      with Frames[i] do
        Lexer:= Lexer;
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
var
  Form: TfmAbout;
begin
  Form:= TfmAbout.Create(Self);
  with Form do
  try
    DoLocalize_FormAbout(Form);
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
  DoFileOpen(GetAppPath(cFileReadmeHistory));
end;

procedure TfmMain.DoHelpMouse;
begin
  DoFileOpen(GetAppPath(cFileReadmeHelpMouse));
end;

procedure TfmMain.MenuWindowClick(Sender: TObject);
begin
  SetFrame(Frames[(Sender as TMenuItem).Tag]);
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
  cAdapterIdleInterval:= UiOps.LexerDelayedParsingPause;
  cAdapterIdleTextSize:= UiOps.LexerDelayedParsingSize;

  //apply DoubleBuffered
  //no need for ToolbarMain and buttons
  for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
    Groups.Pages[i].Tabs.DoubleBuffered:= UiOps.DoubleBuffered;
  for i:= 0 to FrameCount-1 do
    with Frames[i] do
    begin
      Editor.DoubleBuffered:= UiOps.DoubleBuffered;
      Editor2.DoubleBuffered:= UiOps.DoubleBuffered;
    end;
  Status.DoubleBuffered:= UiOps.DoubleBuffered;
  StatusAlt.DoubleBuffered:= UiOps.DoubleBuffered;
  ButtonCancel.DoubleBuffered:= UiOps.DoubleBuffered;
  StatusProgress.DoubleBuffered:= UiOps.DoubleBuffered;
  ListboxOut.DoubleBuffered:= UiOps.DoubleBuffered;
  ListboxVal.DoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmConsole) then
    fmConsole.IsDoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmFind) then
    fmFind.IsDoubleBuffered:= UiOps.DoubleBuffered;
  if Assigned(fmGoto) then
    fmGoto.IsDoubleBuffered:= UiOps.DoubleBuffered;
  //end apply DoubleBuffered

  UpdateStatusbarPanelsFromString(UiOps.StatusPanels);

  TimerTreeFill.Interval:= UiOps.TreeTimeFill;
  TimerTreeFocus.Interval:= UiOps.TreeTimeFocus;

  fmConsole.memo.OptCaretShapeRO:= TATSynCaretShape(EditorOps.OpCaretShapeRO);
  fmConsole.memo.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  fmConsole.ed.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  fmConsole.Wordwrap:= UiOps.ConsoleWordWrap;

  Groups.ScalePercents:= UiOps.ScreenScale;
  Groups.SetTabOption(tabOptionBottomTabs, Ord(UiOps.TabBottom));
  Groups.SetTabOption(tabOptionShowXButtons, UiOps.TabShowX);
  Groups.SetTabOption(tabOptionShowPlus, Ord(UiOps.TabShowPlus));
  Groups.SetTabOption(tabOptionShowEntireColor, Ord(UiOps.TabColorFull));
  Groups.SetTabOption(tabOptionDoubleClickClose, Ord(UiOps.TabDblClickClose));
  Groups.SetTabOption(tabOptionAngle, UiOps.TabAngle);
  Groups.SetTabOption(tabOptionWidthMax, UiOps.TabWidth);
  Groups.SetTabOption(tabOptionHeight, UiOps.TabHeight);
  Groups.SetTabOption(tabOptionHeightInner, UiOps.TabHeightInner);
  Groups.SetTabOption(tabOptionIndentTop, IfThen(UiOps.TabBottom, 0, UiOps.TabIndentTop));
  Groups.SetTabOption(tabOptionIndentInit, UiOps.TabIndentInit);
  Groups.SetTabOption(tabOptionIndentColor, 4);
  Groups.SetTabOption(tabOptionWidecharModified, Ord('*'));
  Groups.SetTabOption(tabOptionShowNums, Ord(UiOps.TabNumbers));
  Groups.SetTabOption(tabOptionIndentXRight, 10);
  Groups.SetTabOption(tabOptionIndentXSize, 12);

  PanelSide.Visible:= UiOps.SidebarShow;
  PanelLeftTitle.Height:= Groups.Pages1.Tabs.Height;

  if UiOps.TabBottom then
    PanelLeftTitle.Align:= alBottom
  else
    PanelLeftTitle.Align:= alTop;

  Status.Height:= MulDiv(UiOps.StatusHeight, UiOps.ScreenScale, 100);
  TimerStatus.Interval:= UiOps.StatusTime*1000;

  ATButtonTheme.FontName:= UiOps.VarFontName;
  ATButtonTheme.FontSize:= UiOps.VarFontSize;

  CompletionOps.FormSizeX:= UiOps.ListboxCompleteSizeX;
  CompletionOps.FormSizeY:= UiOps.ListboxCompleteSizeY;

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


function TfmMain.DoFileOpen(AFilename: string; APages: TATPages;
  const AArgs: string): TEditorFrame;
var
  D: TATTabData;
  F: TEditorFrame;
  isOem, bSilent: boolean;
  tick: QWord;
  msg: string;
  i: integer;
begin
  Result:= nil;
  AppFolderOfLastInstalledAddon:= '';
  if Application.Terminated then exit;

  if APages=nil then
    APages:= Groups.PagesCurrent;

  if AFilename='' then
  begin
    D:= DoTabAdd(APages, GetUntitledCaption);
    Result:= D.TabObject as TEditorFrame;
    EditorFocus(Result.Editor);
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
    bSilent:= Pos('/silent', AArgs)>0;
    DoFileInstallZip(AFilename, AppFolderOfLastInstalledAddon, bSilent);
    exit
  end;

  //py event
  if FAllowEventOnOpenBefore then
    if DoPyEvent(CurrentEditor, cEventOnOpenBefore,
      [SStringToPythonString(AFilename)]) = cPyFalse then exit;

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

  //too big size?
  if FileSize(AFileName) div (1024*1024) >= UiOps.MaxFileSizeToOpen then
  begin
    MsgBox(
      msgCannotOpenTooBig+#10+
      AFileName+#10+
      '(option "ui_max_size_open")',
      MB_OK or MB_ICONWARNING);
    exit
  end;

  //is file already opened? activate frame
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    if SameFileName(F.FileName, AFilename) then
    begin
      SetFrame(F);
      Result:= F;
      EditorFocus(Result.Editor);
      UpdateStatus;
      UpdateTree(true);
      Exit
    end;
  end;

  //is current frame empty? use it
  if APages=Groups.PagesCurrent then
  begin
    F:= CurrentFrame;
    if Assigned(F) then
    if F.IsEmpty then
    begin
      tick:= GetTickCount64;
      F.DoFileOpen(AFilename, true, true);
      Result:= F;
      tick:= (GetTickCount64-tick) div 1000;

      UpdateStatus;
      msg:= msgStatusOpened+' '+ExtractFileName(AFilename);
      if tick>2 then
        msg:= msg+' ('+IntToStr(tick)+'s)';
      MsgStatus(msg);

      DoPyEvent(F.Editor, cEventOnOpen, []);
      Exit
    end;
  end;

  D:= DoTabAdd(APages, ExtractFileName(AFilename));
  F:= D.TabObject as TEditorFrame;

  tick:= GetTickCount64;
  F.DoFileOpen(AFilename, true, true);
  Result:= F;
  tick:= (GetTickCount64-tick) div 1000;

  UpdateStatus;
  msg:= msgStatusOpened+' '+ExtractFileName(AFilename);
  if tick>2 then
    msg:= msg+' ('+IntToStr(tick)+'s)';
  MsgStatus(msg);

  DoPyEvent(F.Editor, cEventOnOpen, []);
  EditorFocus(Result.Editor);
end;


procedure TfmMain.DoFileOpenDialog_NoPlugins;
begin
  FAllowEventOnOpenBefore:= false;
  try
    DoFileOpenDialog;
  finally
    FAllowEventOnOpenBefore:= true;
  end;
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
    begin
      if UiOps.InitialDir<>'' then
        InitialDir:= UiOps.InitialDir
      else
        InitialDir:= FLastDirOfOpenDlg;
    end;

    if not Execute then exit;
    FLastDirOfOpenDlg:= ExtractFileDir(FileName);

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
  NCmd: integer;
begin
  MsgStatus(msgStatusHelpOnShowCommands);
  NCmd:= DoDialogCommands_Custom(true, true, true, true, FLastSelectedCommand);
  if NCmd>0 then
  begin
    FLastSelectedCommand:= NCmd;
    CurrentEditor.DoCommand(NCmd);
    UpdateFrame;
  end;
end;


function TfmMain.DoDialogCommands_Py(AShowUsual, AShowPlugins, AShowLexers, AAllowConfig: boolean): string;
var
  NCmd: integer;
begin
  Result:= '';
  NCmd:= DoDialogCommands_Custom(AShowUsual, AShowPlugins, AShowLexers, AAllowConfig, 0);
  if NCmd<=0 then exit;

  if (NCmd>=cmdFirstPluginCommand) and (NCmd<=cmdLastPluginCommand) then
  begin
    with AppPluginsCommand[NCmd-cmdFirstPluginCommand] do
      if ItemProcParam<>'' then
        Result:= Format('p:module=%s;cmd=%s;info=%s;', [ItemModule, ItemProc, ItemProcParam])
      else
        Result:= Format('p:%s.%s', [ItemModule, ItemProc]);
  end
  else
  if (NCmd>=cmdFirstLexerCommand) and (NCmd<cmdFirstLexerCommand+AppManager.AnalyzerCount) then
  begin
    Result:= 'l:'+AppManager.Analyzers[NCmd-cmdFirstLexerCommand].LexerName
  end
  else
    Result:= 'c:'+IntToStr(NCmd);
end;


function TfmMain.DoDialogCommands_Custom(
  AShowUsual, AShowPlugins, AShowLexers, AAllowConfig: boolean;
  AFocusedCommand: integer): integer;
var
  bKeysChanged: boolean;
begin
  Result:= 0;
  fmCommands:= TfmCommands.Create(Self);
  try
    UpdateInputForm(fmCommands);
    fmCommands.OptShowUsual:= AShowUsual;
    fmCommands.OptShowPlugins:= AShowPlugins;
    fmCommands.OptShowLexers:= AShowLexers;
    fmCommands.OptAllowConfig:= AAllowConfig;
    fmCommands.OptFocusedCommand:= AFocusedCommand;
    fmCommands.OnMsg:= @DoCommandsMsgStatus;
    fmCommands.CurrentLexerName:= CurrentFrame.LexerName;
    fmCommands.Keymap:= CurrentEditor.Keymap;
    fmCommands.ShowModal;
    Result:= fmCommands.ResultCommand;
    bKeysChanged:= fmCommands.ResultHotkeysChanged;
  finally
    FreeAndNil(fmCommands);
  end;

  if bKeysChanged then
    UpdateMenuPlugins_Shortcuts(true);
end;


procedure TfmMain.DoDialogGoto_Hide;
begin
  if Assigned(fmGoto) and fmGoto.Visible then
  begin
    EditorFocus(CurrentEditor);
    fmGoto.Hide;
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
  DoLocalize_FormGoto;

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
    DoDialogGoto_Hide;
    UpdateStatus;
    Exit;
  end;

  if Res=cOpGotoLine then
  begin
    Num:= StrToIntDef(fmGoto.edInput.Text, 0)-1;
    if Num<0 then
    begin
      MsgStatus(msgStatusBadLineNum);
      Exit
    end;
    Num:= Min(Num, Ed.Strings.Count-1);

    fmGoto.Hide;
    MsgStatus(Format(msgStatusGotoLine, [Num+1]));

    Ed.DoGotoPos(
      Point(0, Num),
      Point(-1, -1),
      UiOps.FindIndentHorz,
      UiOps.FindIndentVert,
      true,
      true
      );
    Ed.Update;

    EditorFocus(Ed);
  end;
end;

procedure TfmMain.DoDialogGotoBookmark;
var
  Ed: TATSynEdit;
  Form: TfmGotoList;
  Num, NumMax: integer;
  items: TStringlist;
  str: atString;
  i: integer;
begin
  Ed:= CurrentEditor;
  NumMax:= Ed.Strings.Count-1;
  items:= TStringlist.Create;

  try
    for i:= 0 to ed.Strings.Count-1 do
      if ed.Strings.LinesBm[i]>0 then
      begin
        str:= cHintScrollPrefix + Inttostr(i+1) + ': ' + ed.Strings.Lines[i];
        items.AddObject(Utf8Encode(str), TObject(ptrint(i)));
      end;

    if items.Count=0 then
    begin
      MsgStatus(msgCannotFindBookmarks);
      Exit;
    end;

    Num:= -1;
    Form:= TfmGotoList.Create(Self);
    try
      UpdateInputForm(Form);
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
    begin MsgStatus(msgStatusCancelled); Exit end;
  if Num>NumMax then
    Num:= NumMax;

  Ed.DoGotoPos(
    Point(0, Num),
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );

  MsgStatus(Format(msgStatusGotoLine, [Num+1]));
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
begin
  if GetShowBottom<>AValue then
  begin
    bBottom:= IsFocusedBottom;

    PanelBottom.Visible:= AValue;
    SplitterHorz.Visible:= AValue;
    SplitterHorz.Top:= 0;

    if not AValue then
      if bBottom then
        EditorFocus(CurrentEditor);
  end;

  UpdateBottomButtons;
  UpdateStatus;
end;

procedure TfmMain.SetShowSidePanel(AValue: boolean);
begin
  if GetShowSidePanel<>AValue then
  begin
    PanelLeft.Visible:= AValue;
    SplitterVert.Visible:= AValue;
    SplitterVert.Left:= PanelLeft.Width;
    if AValue then
    begin
      if SidebarPanel='' then
        DoShowSidePanel('Tree', false);
      UpdateTree(true);
    end;
  end;
  UpdateSidebarButtons;
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
  Str: array of string;
  dir: string;
  PathAppend: boolean;
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

  SetLength(Str, Length(Str)+1);
  Str[Length(Str)-1]:= GetAppPath(cDirPy);

  Py_SetSysPath(Str, PathAppend);

  try
    GetPythonEngine.ExecString('_v=sys.version_info; print("Python %d.%d.%d" % (_v[0], _v[1], _v[2]) )');
    GetPythonEngine.ExecString('from cudatext import *');
  except
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
    fmConsole.DoLogConsoleLine(msgCannotInitPython1);
    fmConsole.DoLogConsoleLine(msgCannotInitPython2);
    //disable Plugins menu
    mnuPlug.Enabled:= false;
  end;
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
  if SameText(Frame.EncodingName, AEnc) then exit;
  Frame.EncodingName:= AEnc;

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

  UpdateFrame;
  UpdateStatus;
  MsgStatus(msgStatusEncChanged);
end;

procedure TfmMain.MenuLexClick(Sender: TObject);
var
  an: TecSyntAnalyzer;
begin
  an:= TecSyntAnalyzer((Sender as TComponent).Tag);
  CurrentFrame.Lexer:= an;
  UpdateFrame;
  UpdateStatus;
end;

procedure TfmMain.DoOps_LoadLexerLib;
var
  dir, fn, lexname: string;
  L: TStringlist;
  an: TecSyntAnalyzer;
  ini: TIniFile;
  i, j: integer;
begin
  AppManager.Clear;

  //load .lcf files to lib
  dir:= GetAppPath(cDirDataLexerlib);
  L:= TStringlist.Create;
  try
    FindAllFiles(L, dir, '*.lcf', false);
    L.Sort;

    if L.Count=0 then
    begin
      MsgStatusAlt('Cannot find lexer files: data/lexlib/*.lcf', 3);
      exit
    end;

    for i:= 0 to L.Count-1 do
    begin
      an:= AppManager.AddAnalyzer;
      an.LoadFromFile(L[i]);
    end;
  finally
    FreeAndNil(L);
  end;

  //correct sublexer links
  for i:= 0 to AppManager.AnalyzerCount-1 do
  begin
    an:= AppManager.Analyzers[i];
    fn:= GetAppLexerMapFilename(an.LexerName);
    if FileExists(fn) then
    begin
      ini:= TIniFile.Create(fn);
      try
        for j:= 0 to an.SubAnalyzers.Count-1 do
        begin
          lexname:= ini.ReadString('ref', IntToStr(j), '');
          if lexname<>'' then
            an.SubAnalyzers[j].SyntAnalyzer:= AppManager.FindAnalyzer(lexname);
        end;
      finally
        FreeAndNil(ini);
      end;
    end;
  end;

  UpdateMenuLexers;
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
  mi, mi0: TMenuItem;
  ch, ch0: char;
  i: integer;
begin
  if AMenu=nil then exit;
  AMenu.Clear;

  ch0:= '?';
  mi0:= nil;

  mi:= TMenuItem.create(self);
  mi.caption:= msgNoLexer;
  mi.OnClick:= @MenuLexClick;
  AMenu.Add(mi);

  sl:= tstringlist.create;
  try
    //make stringlist of all lexers
    for i:= 0 to AppManager.AnalyzerCount-1 do
    begin
      an:= AppManager.Analyzers[i];
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
        AMenu.Add(mi);
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
        AMenu.Add(mi0);
      end;

      mi:= TMenuItem.create(self);
      mi.caption:= sl[i];
      mi.tag:= ptrint(sl.Objects[i]);
      mi.OnClick:= @MenuLexClick;
      if assigned(mi0) then
        mi0.add(mi)
      else
        AMenu.Add(mi);
    end;
  finally
    sl.free;
  end;
end;

procedure TfmMain.MsgStatus(const AText: string);
var
  Frame: TEditorFrame;
  S: string;
begin
  Frame:= CurrentFrame;
  S:= AText;
  SReplaceAll(S, #10, ' ');
  SReplaceAll(S, #13, ' ');

  if Frame.IsText then
  begin
    if Frame.ReadOnly then
      S:= msgStatusReadonly+' '+S;
    if Frame.MacroRecord then
      S:= msgStatusMacroRec+' '+S;
  end;

  Status[StatusbarIndex_Msg]:= S;

  if S='' then exit;
  TimerStatus.Enabled:= false;
  TimerStatus.Enabled:= true;
end;

procedure TfmMain.MsgStatusAlt(const AText: string; ASeconds: integer);
var
  Ed: TATSynEdit;
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

  Ed:= CurrentEditor;
  StatusAlt.Parent:= Ed;
  StatusAlt.Align:= alBottom;

  StatusAlt[0]:= AText;
  StatusAlt.Show;

  TimerStatusAlt.Interval:= ASeconds*1000;
  TimerStatusAlt.Enabled:= false;
  TimerStatusAlt.Enabled:= true;
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
      F.DoFileSave(false);
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
  if F.Modified then
    if MsgBox(
      Format(msgConfirmReopenModifiedTab, [F.FileName]),
      MB_OKCANCEL or MB_ICONQUESTION)<>id_ok then exit;

  PrevRO:= F.ReadOnly;
  PrevLexer:= F.LexerName;
  F.ReadOnly:= false;
  F.DoFileReload;
  F.Lexer:= AppManager.FindAnalyzer(PrevLexer);
  F.ReadOnly:= PrevRO;

  UpdateStatus;
  MsgStatus(msgStatusReopened+' '+ExtractFileName(F.Filename));
end;

function TfmMain.DoFileCloseAll: boolean;
var
  i: integer;
begin
  Result:= Groups.CloseTabs(tabCloseAll, false);
  if not Result then exit;

  for i:= 0 to FrameCount-1 do
    if Frames[i].Modified then exit(false);
end;

procedure TfmMain.DoFileCloseAndDelete;
var
  fn: string;
begin
  if not CurrentFrame.IsText then exit;
  fn:= CurrentFrame.FileName;
  if fn='' then exit;

  if MsgBox(msgConfirmCloseDelFile+#13+fn, MB_OKCANCEL or MB_ICONWARNING)=id_ok then
    if Groups.CloseTabs(tabCloseCurrent, false) then
      DeleteFileUTF8(fn);
end;


procedure TfmMain.MenuRecentsClick(Sender: TObject);
var
  fn: string;
  n: integer;
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
      EditorFocus(CurrentEditor);
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
    DoPyCommand('cudax_lib', AMethod, []);
  finally
    Ed.Strings.EndUndoGroup;
  end;
end;


procedure TfmMain.DoShowConsole(AFocusEdit: boolean);
begin
  DoShowBottomPanel('Console');
  if AFocusEdit then
    fmConsole.ed.SetFocus;
end;

procedure TfmMain.DoShowOutput;
begin
  DoShowBottomPanel('Output');
end;

procedure TfmMain.DoShowValidate;
begin
  DoShowBottomPanel('Validate');
end;

procedure TfmMain.DoShowSearchResults;
begin
  //no need yet
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


procedure TfmMain.DoShowBottomPanel(const ATabCaption: string);
begin
  if ATabCaption='-' then
  begin
    ShowBottom:= false;
  end
  else
  begin
    ShowBottom:= true;
    if ATabCaption<>'' then
      DoBottom_ActivateTab(ATabCaption);
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
      Editor.OptGutterVisible:= AValue;
      Editor2.OptGutterVisible:= AValue;
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
    Ed.OptTextCenteringCharWidth:= EditorOps.OpCenteringWidth;
    DoApplyGutterVisible(EditorOps.OpGutterShow);
  end;

  {$ifdef windows}
  SetFullScreen_Win32(AValue);
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
  F:= DoFileOpen('');
  if F=nil then exit;
  F.Editor.Strings.LoadFromFile(fn);
  F.Lexer:= DoLexerFindByFilename(fn);
  UpdateFrame(true);
  UpdateStatus;
end;

procedure TfmMain.DoFileSave;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F.Modified or (F.FileName='') then
    F.DoFileSave(false);
end;

procedure TfmMain.DoFileSaveAs;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  F.DoFileSave(true);
end;

procedure TfmMain.DoSwitchActiveTab(ANext: boolean);
begin
  Groups.PagesCurrent.Tabs.SwitchTab(ANext);
end;

function TfmMain.DoCheckFilenameOpened(const AName: string): boolean;
var
  SName: string;
  i: integer;
begin
  Result:= false;
  if AName='' then exit;
  for i:= 0 to FrameCount-1 do
  begin
    SName:= Frames[i].FileName;
    if SameFileName(SName, AName) then exit(true);
  end;
end;

procedure TfmMain.DoOps_OpenFile_Default;
var
  fn: string;
begin
  fn:= GetAppPath(cFileOptionsDefault);
  DoFileOpen(fn);
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

  DoFileOpen(fn);
end;

procedure TfmMain.DoOps_OpenFile_FileTypes;
var
  fn: string;
begin
  fn:= GetAppPath(cFileOptionsFiletypes);
  if not FileExistsUTF8(fn) then
  begin
    FCreateFile(fn, true);
    if not FileExistsUTF8(fn) then Exit;
  end;

  DoFileOpen(fn);
end;

procedure TfmMain.DoOps_OpenFile_LexerSpecific;
var
  fn: string;
begin
  fn:= GetAppLexerSpecificConfig(CurrentFrame.LexerName);
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
  NTag: PtrInt;
  NCommand: integer;
  SCallback: string;
begin
  NTag:= (Sender as TComponent).Tag;
  if NTag=0 then exit;

  NCommand:= TAppMenuProps(NTag).CommandCode;
  SCallback:= TAppMenuProps(NTag).CommandString;

  //dont do editor commands here if ed not focused
  F:= CurrentFrame;
  EdFocus:=
    F.Editor.Focused or
    F.Editor2.Focused or
    fmConsole.ed.Focused or
    fmConsole.memo.Focused;
  if not EdFocus then
    if (NCommand>0) and (NCommand<cmdFirstAppCommand) then exit;

  //-1 means run callback
  if NCommand=-1 then
  begin
    if SCallback<>'' then
      DoPyCallbackFromAPI(SCallback, []);
  end
  else
    CurrentEditor.DoCommand(NCommand);

  UpdateFrame;
  UpdateStatus;
end;

procedure TfmMain.SetLexerIndex(N: integer);
begin
  if (N>=0) and (N<AppManager.AnalyzerCount) then
    CurrentFrame.Lexer:= AppManager.Analyzers[N]
  else
    CurrentFrame.Lexer:= nil;

  UpdateFrame;
  UpdateStatus;
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
  MsgStatus(msgStatusTryingAutocomplete+' '+LexName);
  if LexName='' then exit;

  //'php_'->'php'
  if LexName[Length(LexName)]='_' then
    Delete(LexName, Length(Lexname), 1);

  IsPascal:= Pos('Pascal', LexName)>0;
  IsHtml:= UiOps.AutocompleteHtml and (Pos('HTML', LexName)>0);
  IsCss:= UiOps.AutocompleteCss and (LexName='CSS');
  IsCaseSens:= false; //cannot detect it yet
  FileCss:= GetAppPath(cDirDataAutocompleteSpec)+DirectorySeparator+'css_list.ini';
  FileHtml:= GetAppPath(cDirDataAutocompleteSpec)+DirectorySeparator+'html_list.ini';
  FileAcp:= GetAppPath(cDirDataAutocomplete)+DirectorySeparator+LexName+'.acp';

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
  DoTreeviewFoldLevel(Tree, 2);
end;

procedure TfmMain.mnuTreeFold3Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(Tree, 3);
end;

procedure TfmMain.mnuTreeFold4Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(Tree, 4);
end;

procedure TfmMain.mnuTreeFold5Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(Tree, 5);
end;

procedure TfmMain.mnuTreeFold6Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(Tree, 6);
end;

procedure TfmMain.mnuTreeFold7Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(Tree, 7);
end;

procedure TfmMain.mnuTreeFold8Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(Tree, 8);
end;

procedure TfmMain.mnuTreeFold9Click(Sender: TObject);
begin
  DoTreeviewFoldLevel(Tree, 9);
end;

procedure TfmMain.mnuTreeFoldAllClick(Sender: TObject);
begin
  Tree.FullCollapse;
end;

procedure TfmMain.mnuTreeUnfoldAllClick(Sender: TObject);
begin
  Tree.FullExpand;
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

  CurrentFrame.Adapter.DynamicHiliteEnabled:= false; //turn off for html

  Ed:= CurrentEditor;
  Ed.DoCommand(cCommand_SelectNone);

  DoEditorExportToHTML(Ed, SaveDlg.FileName, STitle,
    UiOps.ExportHtmlFontName,
    UiOps.ExportHtmlFontSize,
    UiOps.ExportHtmlNumbers,
    GetAppColor('ExportHtmlBg'),
    GetAppColor('ExportHtmlNumbers')
    );

  CurrentFrame.Adapter.DynamicHiliteEnabled:= EditorOps.OpLexerDynamicHiliteEnabled; //turn back
  UpdateFrame(true);

  if MsgBox(msgConfirmOpenCreatedDoc, MB_OKCANCEL or MB_ICONQUESTION)=id_ok then
    OpenDocument(SaveDlg.FileName);
end;


function TfmMain.DoDialogMenuApi(const AText, ACaption: string;
  AMultiline: boolean;
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

    UpdateInputForm(Form);
    Form.ListCaption:= ACaption;
    Form.Multiline:= AMultiline;
    Form.InitItemIndex:= AInitIndex;
    Form.ShowModal;
    Result:= Form.ResultCode;
  finally
    Form.Free;
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

procedure TfmMain.DoPanel_TreeviewOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then
  begin
    EditorFocus(CurrentEditor);
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

procedure TfmMain.ListboxOutKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Prop: ^TAppPanelProps;
  List: TATListbox;
begin
  //Esc
  if (Key=VK_ESCAPE) then
  begin
    EditorFocus(CurrentEditor);
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
    Clipboard.AsText:= Prop^.Listbox.Items.Text;
    Key:= 0;
    exit
  end;

  //Ctrl+D
  if (Key=Ord('D')) and (Shift=[ssCtrl]) then
  begin
    Clipboard.AsText:= Prop^.Listbox.Items[List.ItemIndex];
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

    List.ItemCount:= Prop^.Listbox.Items.Count;
    if List.ItemCount=0 then
      List.ItemIndex:= -1
    else
    if List.ItemIndex>=List.ItemCount then
      List.ItemIndex:= List.ItemCount-1;

    List.Invalidate;
  end;
end;


procedure TfmMain.MenuLangClick(Sender: TObject);
var
  NTag: integer;
begin
  NTag:= (Sender as TComponent).Tag;
  if NTag>=0 then
  begin
    AppLangName:= ExtractFileNameOnly(FListLangs[NTag]);
    UpdateMenuLangs(mnuLang);
    DoLocalize;
  end
  else
  begin
    AppLangName:= '';
    MsgBox('Built-in translation will be used after app restart', mb_ok or MB_ICONINFORMATION);
  end;
end;


procedure TfmMain.DoHelpLexers;
begin
  DoFileOpen(GetAppPath(cFileReadmeHelpLexers));
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
  if NIndex>=Prop^.Listbox.Items.Count then exit;

  SText:= Prop^.Listbox.Items[NIndex];
  NTag:= PtrInt(Prop^.Listbox.Items.Objects[NIndex]);

  DoParseOutputLine(Prop^, SText, ResFilename, ResLine, ResCol);
  if (ResFilename<>'') and (ResLine>=0) then
  begin
    MsgStatus(Format(msgStatusGotoFileLineCol, [ResFilename, ResLine+1, ResCol+1]));
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
    MsgStatus(msgStatusClickingLogLine);
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


procedure TfmMain.DoDialogRestoreLexerStyles;
var
  Form: TfmLexerStylesRestore;
  An: TecSyntAnalyzer;
  i: integer;
begin
  Form:= TfmLexerStylesRestore.Create(nil);
  try
    DoLocalize_FormLexerRestoreStyles(Form);
    Form.StylesFilename:= GetAppPath(cFileLexerStylesBackup);

    if Form.ShowModal=mrOk then
    begin
      for i:= 0 to Form.List.Count-1 do
        if Form.List.Checked[i] then
        begin
          An:= AppManager.FindAnalyzer(Form.List.Items[i]);
          if Assigned(An) then
          begin
            DoLoadLexerStylesFromFile(An, Form.StylesFilename);
            DoLexerExportFromLibToFile(An);
          end
          else
            MsgBox(msgCannotFindLexerInLibrary+' '+Form.List.Items[i], MB_OK);
        end;

      UpdateFrame;
    end;
  finally
    FreeAndNil(Form);
  end;
end;


procedure TfmMain.CharmapOnInsert(const AStr: string);
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  if Ed.Carets.Count=0 then exit;
  Ed.DoCommand(cCommand_TextInsert, Utf8Decode(AStr));

  UpdateFrame(true);
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
  end;
end;

procedure TfmMain.FrameLexerChange(Sender: TObject);
begin
  DoOps_LoadOptionsLexerSpecific((Sender as TComponent).Owner as TEditorFrame); //options override
  DoPyEvent(CurrentEditor, cEventOnLexer, []);
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

  UpdateFrame;
  UpdateStatus;
end;


function TfmMain.DoMenuEnum_New(const AMenuId: string): PPyObject;
var
  mi: TMenuItem;
  NLen, i: integer;
  NTag: PtrInt;
  NCommand: integer;
  SCommand, STagString: string;
  CmdObject: PPyObject;
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
    begin
      NTag:= mi.Items[i].Tag;
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

      if NCommand>0 then
        CmdObject:= PyInt_FromLong(NCommand)
      else
        CmdObject:= PyString_FromString(PChar(SCommand));

      PyList_SetItem(Result, i,
        Py_BuildValue('{sLsssisssssssO}',
          'id',
          Int64(PtrInt(mi.Items[i])),
          'cap',
          PChar(mi.Items[i].Caption),
          'cmd',
          NCommand,
          'hint',
          PChar(SCommand),
          'hotkey',
          PChar(ShortCutToText(mi.Items[i].ShortCut)),
          'tag',
          PChar(STagString),
          'command',
          CmdObject
          ));
    end;
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
      mnuThemes:= nil;
      mnuThemesUI:= nil;
      mnuThemesSyntax:= nil;
      mnuLang:= nil;
      mnuPlug:= nil;
      mnuLexers:= nil;
    end;
    if AMenuId=PyMenuId_TopOptions then
    begin
      mnuThemes:= nil;
      mnuThemesUI:= nil;
      mnuThemesSyntax:= nil;
      mnuLang:= nil;
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
    if (AMenuCmd=PyMenuCmd_Recents) or (AMenuCmd='_'+PyMenuCmd_Recents) then
    begin
      mnuFileOpenSub:= mi;
      UpdateMenuRecent(nil);
    end
    else
    if (AMenuCmd=PyMenuCmd_ThemesUI) or (AMenuCmd='_'+PyMenuCmd_ThemesUI) then
    begin
      mnuThemesUI:= mi;
      UpdateMenuThemes(true);
    end
    else
    if (AMenuCmd=PyMenuCmd_ThemesSyntax) or (AMenuCmd='_'+PyMenuCmd_ThemesSyntax) then
    begin
      mnuThemesSyntax:= mi;
      UpdateMenuThemes(false);
    end
    else
    if (AMenuCmd=PyMenuCmd_Langs) or (AMenuCmd='_'+PyMenuCmd_Langs) then
    begin
      mnuLang:= mi;
      UpdateMenuLangs(mi);
    end
    else
    if (AMenuCmd=PyMenuCmd_Plugins) or (AMenuCmd='_'+PyMenuCmd_Plugins) then
    begin
      mnuPlug:= mi;
      TAppMenuProps(mi.Tag).CommandString:= 'plugins';
      UpdateMenuPlugins;
    end
    else
    if (AMenuCmd=PyMenuCmd_Lexers) or (AMenuCmd='_'+PyMenuCmd_Lexers) then
    begin
      mnuLexers:= mi;
      UpdateMenuLexers;
    end
    else
    if (AMenuCmd=PyMenuCmd_Enc) or (AMenuCmd='_'+PyMenuCmd_Enc) then
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

procedure TfmMain.DoFileNewMenu(Sender: TObject);
begin
  DoPyCommand('cuda_new_file', 'menu', []);
end;

procedure TfmMain.DoCommandsMsgStatus(Sender: TObject; const ARes: string);
begin
  MsgStatus(ARes);
end;

function TfmMain.IsLexerMatches(const ANameList: string): boolean;
begin
  Result:= IsLexerListed(CurrentFrame.LexerName, ANameList);
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

