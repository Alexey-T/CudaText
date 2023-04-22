unit form_unprinted;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ATSynEdit,
  proc_globdata,
  ATSynEdit_Globals;

type

  { TfmUnprinted }

  TfmUnprinted = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    chkVisible: TCheckBox;
    chkShowWhitespace: TCheckBox;
    chkOnlyInSel: TCheckBox;
    chkAlsoInSel: TCheckBox;
    chkOnlyLeadAndTrail: TCheckBox;
    chkOnlyTrail: TCheckBox;
    chkForceShowTabs: TCheckBox;
    chkShowEndMarks: TCheckBox;
    chkEndDetailed: TCheckBox;
    comboEndMarks: TComboBox;
    LabelEnds: TLabel;
    PanelPreview: TPanel;
    procedure chkAlsoInSelChange(Sender: TObject);
    procedure chkEndArrowsChange(Sender: TObject);
    procedure chkEndDetailedChange(Sender: TObject);
    procedure chkForceShowTabsChange(Sender: TObject);
    procedure chkOnlyInSelChange(Sender: TObject);
    procedure chkOnlyLeadAndTrailChange(Sender: TObject);
    procedure chkOnlyTrailChange(Sender: TObject);
    procedure chkShowEndMarksChange(Sender: TObject);
    procedure chkShowWhitespaceChange(Sender: TObject);
    procedure comboEndMarksChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    EdPreview: TATSynEdit;
    procedure ApplyToEditor(Ed: TATSynEdit);
    procedure UpdateState;
  end;

var
  fmUnprinted: TfmUnprinted;

implementation

{$R *.lfm}

{ TfmUnprinted }

procedure TfmUnprinted.FormCreate(Sender: TObject);
begin
  EdPreview:= TATSynEdit.Create(Self);
  EdPreview.Align:= alClient;
  EdPreview.Parent:= PanelPreview;
  EdPreview.TabStop:= false;
  EdPreview.Font.Name:= EditorOps.OpFontName;
  EdPreview.Font.Size:= EditorOps.OpFontSize;
  EdPreview.Strings.LineAdd('    begin    end    ');
  EdPreview.Strings.LineAdd(#9#9'begin'#9#9'end'#9#9);
  EdPreview.UpdateWrapInfo(true, false);
  EdPreview.OptRulerVisible:= false;
  EdPreview.OptMinimapVisible:= false;
  EdPreview.OptMicromapVisible:= false;
  EdPreview.Show;
end;

procedure TfmUnprinted.chkShowWhitespaceChange(Sender: TObject);
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

procedure TfmUnprinted.chkEndArrowsChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkEndDetailedChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.chkForceShowTabsChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.FormShow(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmUnprinted.ApplyToEditor(Ed: TATSynEdit);
begin
  Ed.OptUnprintedVisible:= chkVisible.Checked;
  Ed.OptUnprintedSpaces:= chkShowWhitespace.Checked;
  Ed.OptUnprintedEnds:= chkShowEndMarks.Checked;
  Ed.OptUnprintedEndsDetails:= chkEndDetailed.Checked;
  Ed.OptUnprintedSpacesTrailing:= chkOnlyTrail.Checked;
  Ed.OptUnprintedSpacesBothEnds:= chkOnlyLeadAndTrail.Checked;
  Ed.OptUnprintedSpacesOnlyInSelection:= chkOnlyInSel.Checked;
  Ed.OptUnprintedSpacesAlsoInSelection:= chkAlsoInSel.Checked;
  Ed.OptUnprintedForceTabs:= chkForceShowTabs.Checked;

  if comboEndMarks.ItemIndex>=0 then
    ATEditorOptions.UnprintedEndSymbol:= TATEditorUnptintedEolSymbol(comboEndMarks.ItemIndex);

  Ed.Update;
end;

procedure TfmUnprinted.UpdateState;
begin
  ApplyToEditor(EdPreview);
end;

end.

