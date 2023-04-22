(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_unprinted;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ATSynEdit;

type
  TFormUnprintedSaveStringOption = procedure(const APath, AValue: string) of object;
  TFormUnprintedSaveBooleanOption = procedure(const APath: string; AValue: boolean) of object;

type
  { TfmUnprinted }

  TfmUnprinted = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    btnSaveConfig: TButton;
    chkVisible: TCheckBox;
    chkShowWhitespace: TCheckBox;
    chkOnlyInSel: TCheckBox;
    chkAlsoInSel: TCheckBox;
    chkOnlyLeadAndTrail: TCheckBox;
    chkOnlyTrail: TCheckBox;
    chkForceShowTabs: TCheckBox;
    chkShowEndMarks: TCheckBox;
    chkEndDetails: TCheckBox;
    PanelPreview: TPanel;
    chkEndDot: TRadioButton;
    chkEndArrow: TRadioButton;
    chkEndPilcrow: TRadioButton;
    procedure btnSaveConfigClick(Sender: TObject);
    procedure chkAlsoInSelChange(Sender: TObject);
    procedure chkEndArrowChange(Sender: TObject);
    procedure chkEndDetailsChange(Sender: TObject);
    procedure chkEndDotChange(Sender: TObject);
    procedure chkEndPilcrowChange(Sender: TObject);
    procedure chkForceShowTabsChange(Sender: TObject);
    procedure chkOnlyInSelChange(Sender: TObject);
    procedure chkOnlyLeadAndTrailChange(Sender: TObject);
    procedure chkOnlyTrailChange(Sender: TObject);
    procedure chkShowEndMarksChange(Sender: TObject);
    procedure chkShowWhitespaceChange(Sender: TObject);
    procedure chkVisibleChange(Sender: TObject);
    procedure comboEndMarksChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure Localize;
    function GetConfigValue: string;
  public
    EdPreview: TATSynEdit;
    OnSaveStringOption: TFormUnprintedSaveStringOption;
    OnSaveBooleanOption: TFormUnprintedSaveBooleanOption;
    procedure ApplyToEditor(Ed: TATSynEdit);
    procedure UpdateState;
  end;

var
  fmUnprinted: TfmUnprinted;

implementation

uses
  IniFiles,
  proc_globdata,
  proc_msg,
  proc_customdialog,
  ATSynEdit_Globals;

{$R *.lfm}

{ TfmUnprinted }

procedure TfmUnprinted.FormCreate(Sender: TObject);
begin
  DoForm_ScaleAuto(Self, false);

  EdPreview:= TATSynEdit.Create(Self);
  EdPreview.Align:= alClient;
  EdPreview.Parent:= PanelPreview;
  EdPreview.TabStop:= false;
  EdPreview.Font.Name:= EditorOps.OpFontName;
  EdPreview.Font.Size:= EditorOps.OpFontSize;
  EdPreview.OptTabSize:= 4;
  EdPreview.OptGutterVisible:= false;
  EdPreview.OptRulerVisible:= false;
  EdPreview.OptMinimapVisible:= false;
  EdPreview.OptMicromapVisible:= false;
  EdPreview.Show;

  EdPreview.Strings.LineAdd('    begin    end    ');
  EdPreview.Strings.LineAdd(#9#9'begin'#9#9'end'#9#9);
  EdPreview.UpdateWrapInfo(true, false);
end;

procedure TfmUnprinted.chkShowWhitespaceChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkVisibleChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.comboEndMarksChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkOnlyInSelChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkOnlyLeadAndTrailChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkOnlyTrailChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkShowEndMarksChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkAlsoInSelChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.btnSaveConfigClick(Sender: TObject);
begin
  if Assigned(OnSaveBooleanOption) then
    OnSaveBooleanOption('/unprinted_show', chkVisible.Checked);

  if Assigned(OnSaveStringOption) then
    OnSaveStringOption('/unprinted_content', GetConfigValue);
end;

procedure TfmUnprinted.chkEndArrowChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkEndDetailsChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkEndDotChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkEndPilcrowChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkForceShowTabsChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.FormShow(Sender: TObject);
begin
  Localize;
  UpdateState;
end;

procedure TfmUnprinted.ApplyToEditor(Ed: TATSynEdit);
begin
  Ed.OptUnprintedVisible:= chkVisible.Checked;
  Ed.OptUnprintedSpaces:= chkShowWhitespace.Checked;
  Ed.OptUnprintedEnds:= chkShowEndMarks.Checked;
  Ed.OptUnprintedEndsDetails:= chkEndDetails.Checked;
  Ed.OptUnprintedSpacesTrailing:= chkOnlyTrail.Checked;
  Ed.OptUnprintedSpacesBothEnds:= chkOnlyLeadAndTrail.Checked;
  Ed.OptUnprintedSpacesOnlyInSelection:= chkOnlyInSel.Checked;
  Ed.OptUnprintedSpacesAlsoInSelection:= chkAlsoInSel.Checked;
  Ed.OptUnprintedForceTabs:= chkForceShowTabs.Checked;

  if chkEndPilcrow.Checked then
    ATEditorOptions.UnprintedEndSymbol:= aeuePilcrow
  else
  if chkEndArrow.Checked then
    ATEditorOptions.UnprintedEndSymbol:= aeueArrowDown
  else
    ATEditorOptions.UnprintedEndSymbol:= aeueDot;

  Ed.Update;
end;

procedure TfmUnprinted.UpdateState;
begin
  ApplyToEditor(EdPreview);

  chkOnlyLeadAndTrail.Enabled:= not chkOnlyInSel.Checked;
  chkOnlyTrail.Enabled:= not chkOnlyInSel.Checked and not chkOnlyLeadAndTrail.Checked;
  chkAlsoInSel.Enabled:= not chkOnlyInSel.Checked;

  chkEndDot.Enabled:= not chkEndDetails.Checked;
  chkEndArrow.Enabled:= not chkEndDetails.Checked;
  chkEndPilcrow.Enabled:= not chkEndDetails.Checked;
end;

procedure TfmUnprinted.Localize;
const
  section = 'd_unpri';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= AppFile_Language;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= ini.ReadString(section, '_', Caption);
    with btnOk do Caption:= msgButtonOk;
    with btnCancel do Caption:= msgButtonCancel;
    with btnSaveConfig do Caption:= ini.ReadString(section, 'save', Caption);
    with chkVisible do Caption:= ini.ReadString(section, 'vis', Caption);
    with chkShowWhitespace do Caption:= ini.ReadString(section, 'sh_sp', Caption);
    with chkOnlyInSel do Caption:= ini.ReadString(section, 'only_sel', Caption);
    with chkAlsoInSel do Caption:= ini.ReadString(section, 'also_sel', Caption);
    with chkOnlyLeadAndTrail do Caption:= ini.ReadString(section, 'only_l_tr', Caption);
    with chkOnlyTrail do Caption:= ini.ReadString(section, 'only_tr', Caption);
    with chkForceShowTabs do Caption:= ini.ReadString(section, 'sh_tabs', Caption);
    with chkShowEndMarks do Caption:= ini.ReadString(section, 'sh_end', Caption);
    with chkEndDetails do Caption:= ini.ReadString(section, 'end_det', Caption);
    with chkEndDot do Caption:= ini.ReadString(section, 'end_dot', Caption);
    with chkEndArrow do Caption:= ini.ReadString(section, 'end_arr', Caption);
    with chkEndPilcrow do Caption:= ini.ReadString(section, 'end_pil', Caption);
  finally
    FreeAndNil(ini);
  end;
end;

function TfmUnprinted.GetConfigValue: string;
begin
  Result:= '';
  if chkShowWhitespace.Checked then Result+= 's';
  if chkShowEndMarks.Checked then Result+= 'e';
  if chkEndDetails.Checked then Result+= 'd';
  if chkOnlyTrail.Checked then Result+= 't';
  if chkOnlyLeadAndTrail.Checked then Result+= 'l';
  if chkOnlyInSel.Checked then Result+= 'x';
  if chkAlsoInSel.Checked then Result+= 'X';
  if chkForceShowTabs.Checked then Result+= 'T';

  if chkEndDot.Checked then Result+= '.';
  if chkEndPilcrow.Checked then Result+= 'p';
end;

end.

