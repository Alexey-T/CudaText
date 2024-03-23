unit proc_str_c_syntax;

{$mode ObjFPC}{$H+}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils,
  atsynedit_regexpr;

type
  TEditorCSyntaxSymbol = (
    Unknown,
    OpenCurly,
    Semicolon,
    Comma
  );

const
  cEditorCSyntaxSymbol: array[TEditorCSyntaxSymbol] of string = (
    '??',
    '{',
    ';',
    ','
    );

function LineBeginsWithCBlockKeyword(const S: UnicodeString): boolean;
function LineEndsWithCSymbol(S: UnicodeString): TEditorCSyntaxSymbol;
procedure LineDeleteCStringsAndComments(var S: UnicodeString);


implementation

function LineBeginsWithCBlockKeyword(const S: UnicodeString): boolean;
var
  RE: TRegExpr;
begin
  RE:= TRegExpr.Create('^\s*(if|while|do|for|foreach|else|catch|switch|try)\b.+');
  try
    RE.ModifierI:= false;
    RE.Compile;
    Result:= RE.Exec(S);
  finally
    FreeAndNil(RE);
  end;
end;

procedure LineDeleteCStringsAndComments(var S: UnicodeString);
var
  RE: TRegExpr;
begin
  //delete string tokens
  RE:= TRegExpr.Create('"(\\.|.)*?"');
  try
    RE.Compile;
    S:= RE.Replace(S, '');
  finally
    FreeAndNil(RE);
  end;

  //delete block comments
  RE:= TRegExpr.Create('/\*.*?\*/');
  try
    RE.Compile;
    S:= RE.Replace(S, '');
  finally
    FreeAndNil(RE);
  end;

  //delete line comment
  RE:= TRegExpr.Create('//.*$');
  try
    RE.Compile;
    S:= RE.Replace(S, '');
  finally
    FreeAndNil(RE);
  end;
end;


function LineEndsWithCSymbol(S: UnicodeString): TEditorCSyntaxSymbol;
begin
  Result:= TEditorCSyntaxSymbol.Unknown;
  LineDeleteCStringsAndComments(S);
  S:= Trim(S);
  if S<>'' then
    case S[Length(S)] of
      '{': Result:= TEditorCSyntaxSymbol.OpenCurly;
      ';': Result:= TEditorCSyntaxSymbol.Semicolon;
      ',': Result:= TEditorCSyntaxSymbol.Comma;
    end;
end;

end.

