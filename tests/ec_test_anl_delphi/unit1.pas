unit Unit1;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ecSyntAnal, ecMemoStrings;

type
  { TForm1 }
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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

implementation

uses ecZRegExpr;

{$R *.dfm}

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

procedure TForm1.Button1Click(Sender: TObject);
begin
  TestLoad('OneBad.lxl');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TestLoad('OneOk.lxl');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TestLoad('LexLib.lxl');
  Man.SaveToFile('d:\de_Lexlib.lxl');
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
  Man.SaveToFile(ExtractFilePath(Application.ExeName)+fn+'.copy.lxl');

  Memo1.Lines.Clear;
  for i:= 0 to Man.AnalyzerCount-1 do
  begin
    s:= '';
    if Man.Analyzers[i].SubAnalyzers.Count>0 then
      if Man.Analyzers[i].SubAnalyzers.Items[0].SyntAnalyzer<>nil then
        s:= ' -- '+Man.Analyzers[i].SubAnalyzers.Items[0].SyntAnalyzer.LexerName;
    Memo1.Lines.Add(Man.Analyzers[i].LexerName+s);
  end;
end;


procedure TForm1.Button4Click(Sender: TObject);
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

function TForm1.GetText: TSyntMemoStrings;
begin
  Strings.Text:= '/*....*/ void FName(int name) { /*..*/ "Test";}';
  Result:= Strings;
end;

end.

