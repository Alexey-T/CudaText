unit form_addon_report;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls,
  ATStringProc_Separator,
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
    procedure Localize;
  public
  end;

procedure DoDialogAddonInstalledReport(const SItems, SMsg: string; AShowNeedRestart: boolean);

implementation

{$R *.lfm}

procedure DoDialogAddonInstalledReport(const SItems, SMsg: string; AShowNeedRestart: boolean);
var
  F: TfmAddonReport;
  Sep: TATStringSeparator;
  S: string;
begin
  F:= TfmAddonReport.Create(nil);
  try
    Sep.Init(SItems, #10);
    repeat
      if not Sep.GetItemStr(S) then Break;
      F.ListBox1.Items.Add(S);
    until false;

    F.PanelInfo.Caption:= SMsg;
    F.PanelInfo.Visible:= (SMsg<>'') and AShowNeedRestart;
    F.ShowModal;
  finally
    FreeAndNil(F);
  end;
end;

{ TfmAddonReport }

procedure TfmAddonReport.Localize;
begin
  Caption:= msgStatusAddonInstalled;
  ButtonPanel1.OKButton.Caption:= msgButtonOk;
end;

procedure TfmAddonReport.FormShow(Sender: TObject);
begin
  Localize;
  UpdateFormOnTop(Self);
  ButtonPanel1.OKButton.SetFocus;
end;

end.

