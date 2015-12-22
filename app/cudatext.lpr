program cudatext;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uniqueinstance_package, FormMain, FormConsole, proc_str, proc_py,
  proc_py_const, proc_globdata, FormFrame, form_menu_commands,
  formgoto, proc_cmd, form_menu_list, formsavetabs, formconfirmrep,
  formlexerprop, formlexerlib, proc_msg, lazcolorpalette, formpalette,
  proc_install_zip, formcolorsetup, atsynedit_form_complete_synwrite,
  ATSynEdit_Adapter_EControl, formabout, formkeys, formlexerstylesload, 
  formcharmaps, proc_keysdialog, proc_customdialog, proc_miscutils;

{$R *.res}

begin
  Application.Title:='CudaText';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

