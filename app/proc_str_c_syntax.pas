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

function CSyntax_LineBeginsWithBlockKeyword(const S: UnicodeString): boolean;
function CSyntax_LineEndSymbol(S: UnicodeString): TEditorCSyntaxSymbol;
procedure CSyntax_DeleteStringsAndComments(var S: UnicodeString);


implementation

function CSyntax_LineBeginsWithBlockKeyword(const S: UnicodeString): boolean;
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


procedure CSyntax_DeleteStringsAndComments(var S: UnicodeString);
  //
  function _ReplaceByRegex(var S: UnicodeString; const SRepl: UnicodeString; RE: TRegExpr): boolean;
  begin
    Result:= RE.Exec(S);
    if Result then
    begin
      Delete(S, RE.MatchPos[0], RE.MatchLen[0]);
      if SRepl<>'' then
        Insert(SRepl, S, RE.MatchPos[0]);
    end;
  end;
  //
var
  RE_Str, RE_Cmt: TRegExpr;
  N_Str, N_Cmt: integer;
begin
  RE_Str:= TRegExpr.Create('"(\\.|.)*?"');
  RE_Cmt:= TRegExpr.Create('/\*.*?\*/');

  try
    RE_Str.Compile;
    RE_Cmt.Compile;

    repeat
      N_Str:= Pos('"', S);
      N_Cmt:= Pos('/*', S);
      if (N_Str>0) and ((N_Cmt=0) or (N_Str<N_Cmt)) then
      begin
        if not _ReplaceByRegex(S, '_', RE_Str) then Break;
      end
      else
      if (N_Cmt>0) and ((N_Str=0) or (N_Str>N_Cmt)) then
      begin
        if not _ReplaceByRegex(S, '', RE_Cmt) then Break;
      end
      else
        Break;
    until false;

  finally
    FreeAndNil(RE_Str);
    FreeAndNil(RE_Cmt);
  end;

  //delete line comment
  RE_Cmt:= TRegExpr.Create('//.*$');
  try
    RE_Cmt.Compile;
    S:= RE_Cmt.Replace(S, '');
  finally
    FreeAndNil(RE_Cmt);
  end;
end;


function CSyntax_LineEndSymbol(S: UnicodeString): TEditorCSyntaxSymbol;
begin
  Result:= TEditorCSyntaxSymbol.Unknown;
  CSyntax_DeleteStringsAndComments(S);
  S:= Trim(S);
  if S<>'' then
    case S[Length(S)] of
      '{': Result:= TEditorCSyntaxSymbol.OpenCurly;
      ';': Result:= TEditorCSyntaxSymbol.Semicolon;
      ',': Result:= TEditorCSyntaxSymbol.Comma;
    end;
end;

end.

