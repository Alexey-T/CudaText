program cudatext;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormMain, FormConsole, proc_str, proc_py, proc_py_const, proc_globdata,
  FormFrame, formoutput, formcommands, formgoto, proc_cmd, formgotolist,
  formsavetabs, formconfirmrep, formlexerprop, formlexerlib, proc_msg,
  lazcolorpalette, formpalette, proc_lexer_install_zip, formcolorsetup,
  atsynedit_form_complete_synwrite, ATSynEdit_Adapter_EControl, formabout,
  formkeys, atstringproc_htmlcolor;

{$R *.res}

begin
  Application.Title:='CudaText';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

