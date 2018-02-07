program cudatext;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms, lazcontrols, uniqueinstance_package, FormMain, FormConsole, proc_str,
  proc_py, proc_py_const, proc_globdata, FormFrame, form_menu_commands,
  formgoto, proc_cmd, form_menu_list, formsavetabs, formconfirmrep,
  formlexerprop, formlexerlib, proc_msg, proc_install_zip, formcolorsetup,
  formabout, formkeys, formlexerstylesload, formcharmaps, proc_keysdialog,
  proc_customdialog, proc_miscutils, ATLinkLabel, formlexerstyle,
  formlexerstylemap, formkeyinput, proc_scrollbars, proc_keymap_undolist,
  proc_customdialog_dummy, form_addon_report, fix_focus_window,
  formconfirmbinary;

{$R *.res}

begin
  NTickInitial:= GetTickCount64;
  {$IFDEF WINDOWS}
  if IsAnotherInstanceRunning then Exit;
  {$IFEND}
  Application.Title:='CudaText';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

