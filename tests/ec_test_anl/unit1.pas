unit Unit1;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ecSyntAnal,
  ecMemoStrings;

type
  { TForm1 }
  TForm1 = class(TForm)
    bLoadSm: TButton;
    bLoadFl: TButton;
    bSave: TButton;
    bAn: TButton;
    Memo1: TMemo;
    procedure bAnClick(Sender: TObject);
    procedure bLoadSmClick(Sender: TObject);
    procedure bLoadFlClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Strings: TSyntMemoStrings;
    procedure TestRE;
    procedure TestLoad(const fn: string);
    function GetText: TSyntMemoStrings;
  public
    { public declarations }
  end;

type
  TSynStrArray = array[0..7] of Widestring;

var
  Form1: TForm1;
  Man: TSyntaxManager;
  //An: TSyntAnalyzer;
  //AnClient: TClientSyntAnalyzer;

implementation

uses ecZRegExpr;

{$R *.lfm}

procedure SParseRegexArray(const SStr, SRegex: Widestring;
  var Res: TSynStrArray);
var
  r: TecRegExpr;
  i, n: Integer;
begin
  for i:= Low(Res) to High(Res) do
    Res[i]:= '';
  R:= TecRegExpr.Create;
  try
    R.Expression:= SRegex;
    R.ModifierX:= false;
    n:= 1;
    if R.Match(SStr, n) then
    begin
      for i:= Low(Res) to High(Res) do
        Res[i]:= R.GetMatch(SStr, i+1);
    end;
  finally
    FreeAndNil(R);
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Man:= TSyntaxManager.Create(Self);
  Strings:= TSyntMemoStrings.Create;
end;

procedure TForm1.bAnClick(Sender: TObject);
var
  An: TSyntAnalyzer;
  AnClient: TClientSyntAnalyzer;
  i: Integer;
begin
  An:= Man.FindAnalyzer('C++');
  if An=nil then begin showmessage('Cannot find an'); exit end;

  AnClient:= TClientSyntAnalyzer.Create(An, GetText, nil);
  AnClient.Analyze();

  Memo1.Lines.Clear;
  for i:= 0 to AnClient.TagCount-1 do
    Memo1.Lines.Add(AnClient.TagStr[i]);
end;

procedure TForm1.bLoadSmClick(Sender: TObject);
begin
  TestLoad('OneOk.lxl');
end;

procedure TForm1.bLoadFlClick(Sender: TObject);
begin
  TestLoad('LexLib.lxl');
end;

procedure TForm1.bSaveClick(Sender: TObject);
const fn='LexLib.lxl';
begin
  Man.LoadFromFile(ExtractFilePath(Application.ExeName)+fn);
  Man.SaveToFile(ExtractFilePath(Application.ExeName)+fn+'.copy.lxl');
end;

procedure TForm1.TestRE;
var
  ar: TSynStrArray;
begin
  Fillchar(ar, sizeof(ar), 0);
  SParseRegexArray('nnnnnnn Some1234 nnnnnnnnnn', '(\w+) (\w+) (\w+)', ar);
  caption:= ar[1];
end;

procedure TForm1.TestLoad(const fn: string);
var
  i: Integer;
  s: string;
begin
  Man.LoadFromFile(ExtractFilePath(Application.ExeName)+fn);

  Memo1.Lines.Clear;
  for i:= 0 to Man.AnalyzerCount-1 do
  begin
    s:= '';
    Memo1.Lines.Add(Man.Analyzers[i].LexerName+s);
  end;
end;

function TForm1.GetText: TSyntMemoStrings;
begin
  Strings.Text:= '/*...*/void fname(int name) {/*..*/ "Test str";}//Left left';
  Result:= Strings;
end;


end.

