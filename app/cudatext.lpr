program cudatext;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  proc_inittick,
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, lazcontrols, FormMain, form_console, form_menu_commands,
  form_goto, form_menu_list, form_savetabs, form_confirm_rep, form_lexer_prop,
  form_lexer_lib, formcolorsetup, form_about, form_keys, form_charmaps,
  form_lexer_style, form_lexer_stylemap, form_keyinput, form_addon_report,
  form_confirm_binary, form_choose_theme, proc_globdata, fix_focus_window,
  proc_json_ex, form_unprinted, proc_editor_micromap;

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
