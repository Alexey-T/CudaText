unit form_addon_report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls, IniFiles,
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
  List: TStringList;
begin
  F:= TfmAddonReport.Create(nil);
  List:= TStringList.Create;
  try
    List.Text:= SItems;
    F.PanelInfo.Caption:= SMsg;
    F.PanelInfo.Visible:= SMsg<>'';
    F.ListBox1.Items.AddStrings(List);
    F.ShowModal;
  finally
    FreeAndNil(List);
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

