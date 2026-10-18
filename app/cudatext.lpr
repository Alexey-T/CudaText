program cudatext;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  proc_inittick,
  proc_heapguard, //pad-to-chunk MM wrapper (issue #6533): must init before Interfaces/LCL
  Interfaces, // this includes the LCL widgetset
  SysUtils, Forms, lazcontrols, FormMain, form_console, form_menu_commands,
  form_goto, form_menu_list, form_tabs_save, form_confirm_rep, form_lexer_prop,
  form_lexer_lib, form_color_setup, form_about, form_keys, form_char_map,
  form_lexer_style, form_lexer_stylemap, form_key_input, form_addon_report,
  form_confirm_binary, form_choose_theme, proc_globdata, fix_focus_window,
  form_unprinted, proc_editor_micromap, proc_unique_counter, proc_crash_backup,
  form_find, form_frame, form_menu_py, form_rename_file, proc_appvariant,
  proc_cmd, proc_colors, proc_cssprovider, proc_customdialog,
  proc_customdialog_dummy, proc_editor, proc_editor_saving, proc_files,
  proc_install_zip, proc_keysdialog, proc_lexer_styles, proc_miscutils,
  proc_msg, proc_ole_dragsource, proc_ole_droptarget, proc_panelhost, proc_py,
  proc_py_const, proc_scrollbars, proc_str;

{$R *.res}

begin
  if not AppAlwaysNewInstance then
    if IsSetToOneInstance then
      if IsAnotherInstanceRunning then exit; //func has different implementations for Win and Unix
  {$IFDEF WINDOWS}
  Application.{%H-}MainFormOnTaskBar:= True; //for issue #2864, do it for any MonitorCount
  {$ELSE}
  RegisterSignalHandler;
  {$IFEND}
  Application.Title:='CudaText';
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  InstallCrashBackup;
  Application.Run;
end.
