program cudatext;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  proc_inittick,
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, lazcontrols,
  FormMain, FormConsole,
  form_menu_commands, formgoto, form_menu_list, formsavetabs,
  formconfirmrep, formlexerprop, formlexerlib,
  formcolorsetup, formabout, formkeys, formcharmaps,
  formlexerstyle, formlexerstylemap, formkeyinput,
  form_addon_report, formconfirmbinary, form_choose_theme,
  proc_globdata, fix_focus_window, proc_json_ex, form_unprinted;

{$R *.res}

begin
  if not AppAlwaysNewInstance then
    if IsSetToOneInstance then
      if IsAnotherInstanceRunning then exit; //func has different implementations for Win and Unix
  {$IFDEF WINDOWS}
  Application.{%H-}MainFormOnTaskBar:= True; //for issue #2864, do it for any MonitorCount
  {$IFEND}
  Application.Title:='CudaText';
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

