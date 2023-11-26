unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls,
  ATSynEdit,
  ATSynEdit_Carets,
  TreeHelpers_Base,
  TreeHelpers_Proc;

type

  { TForm1 }

  TForm1 = class(TForm)
    Ed: TATSynEdit;
    btnFile: TButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    FTree: TTreeView;
    procedure btnFileClick(Sender: TObject);
    procedure EdChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FTreeDblClick(Sender: TObject);
  private
    FFileName: string;
    FData: TATTreeHelperRecords;
    FLexer: string;
    procedure UpdateTree;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
  TMyTreeInfo = class
    X1, Y1, X2, Y2: integer;
  end;

function ApplyTreeHelperInPascal(Ed: TATSynEdit;
  const ALexer: string; Tree: TTreeView; Data: TATTreeHelperRecords): boolean;
var
  DataItem: PATTreeHelperRecord;
  NX1, NY1, NX2, NY2: integer;
  NLevel, NLevelPrev, NIcon: integer;
  STitle: string;
  Node, NodeParent: TTreeNode;
  iItem, iLevel: integer;
  Info: TMyTreeInfo;
begin
  Tree.BeginUpdate;
  try
    Tree.Items.Clear;

    Node:= nil;
    NodeParent:= nil;
    NLevelPrev:= 1;

    Result:= TreeHelperInPascal(Ed, ALexer, Data);
    if Result and (Data.Count>0) then
    begin
      for iItem:= 0 to Data.Count-1 do
      begin
        DataItem:= Data.ItemPtr[iItem];

        NX1:= DataItem^.X1;
        NY1:= DataItem^.Y1;
        NX2:= DataItem^.X2;
        NY2:= DataItem^.Y2;
        NLevel:= DataItem^.Level;
        STitle:= DataItem^.Title;
        NIcon:= DataItem^.Icon;

        if (Node=nil) or (NLevel<=1) then
          NodeParent:= nil
        else
        begin
          NodeParent:= Node;
          for iLevel:= NLevel to NLevelPrev do
            if Assigned(NodeParent) then
              NodeParent:= NodeParent.Parent;
        end;

        Info:= TMyTreeInfo.Create;
        Info.X1:= NX1;
        Info.X2:= NX2;
        Info.Y1:= NY1;
        Info.Y2:= NY2;

        Node:= Tree.Items.AddChildObject(NodeParent, STitle, Info);
        Node.ImageIndex:= NIcon;
        Node.SelectedIndex:= NIcon;

        NLevelPrev:= NLevel;
      end;
    end;
  finally
    Tree.EndUpdate;
  end;
end;


{ TForm1 }

procedure TForm1.btnFileClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:= ExtractFilePath(Application.ExeName)+'tests';
  if OpenDialog1.Execute then
  begin
    FFileName:= OpenDialog1.FileName;
    Ed.LoadFromFile(FFileName, []);

    case ExtractFileExt(FFileName) of
      '.md':
        FLexer:= 'Markdown';
      '.wiki',
      '.mediawiki':
        FLexer:= 'MediaWiki';
      '.wikidpad':
        FLexer:= 'WikidPad';
      '.rst':
        FLexer:= 'reStructuredText';
      else
        FLexer:= 'none'
    end;

    Label1.Caption:= Format('File "%s", Lexer "%s"', [ExtractFileName(FFileName), FLexer]);

    UpdateTree;
  end;
end;

procedure TForm1.EdChange(Sender: TObject);
begin
  UpdateTree;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FData:= TATTreeHelperRecords.Create;
end;

procedure TForm1.FTreeDblClick(Sender: TObject);
var
  Node: TTreeNode;
  Info: TMyTreeInfo;
  P: TPoint;
begin
  P:= FTree.ScreenToClient(Mouse.CursorPos);
  Node:= FTree.GetNodeAt(P.X, P.Y);
  if Assigned(Node) then
  begin
    Info:= TMyTreeInfo(Node.Data);
    Ed.DoCaretSingle(Info.X1, Info.Y1, -1, -1);
    Ed.DoGotoCaret(cEdgeTop);
  end;
end;

procedure TForm1.UpdateTree;
begin
  FTree.Items.Clear;
  FData.Clear;
  TreeHelperInPascal(Ed, FLexer, FData);
  ApplyTreeHelperInPascal(Ed, FLexer, FTree, FData);
  FTree.FullExpand;
end;

end.

