unit form_addon_report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls, IniFiles,
  ATStringProc,
  proc_globdata,
  proc_msg;

type
  { TfmAddonReport }

  TfmAddonReport = class(TForm)
    ButtonPanel1: TButtonPanel;
    ListBox1: TListBox;
    PanelInfo: TPanel;
    procedure FormShow(Sender: TObject);
  private
  public
  end;

procedure DoDialogAddonInstalledReport(const SItems, SMsg: string);

implementation

{$R *.lfm}

procedure DoDialogAddonInstalledReport(const SItems, SMsg: string);
var
  F: TfmAddonReport;
  SAll, S: string;
begin
  F:= TfmAddonReport.Create(nil);
  try
    SAll:= SItems;
    repeat
      S:= SGetItem(SAll, #10);
      if S='' then Break;
      F.ListBox1.Items.Add(S);
    until false;

    F.PanelInfo.Caption:= SMsg;
    F.PanelInfo.Visible:= SMsg<>'';
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

procedure DoLocalize_FormAddonReport(F: TfmAddonReport);
begin
  with F do Caption:= msgStatusAddonInstalled;
  with F.ButtonPanel1.OKButton do Caption:= msgButtonOk;
end;

{ TfmAddonReport }

procedure TfmAddonReport.FormShow(Sender: TObject);
begin
  DoLocalize_FormAddonReport(Self);
  ButtonPanel1.OKButton.SetFocus;
end;

end.

