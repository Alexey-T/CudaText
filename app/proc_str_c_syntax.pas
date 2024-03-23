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

var
  RE_Str,
  RE_Cmt,
  RE_CmtLine: TRegExpr;

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
  function _ReplaceByRegex(ReplChar: WideChar; RE: TRegExpr): boolean;
  begin
    Result:= RE.Exec(S);
    if Result then
      FillWord(S[RE.MatchPos[0]], RE.MatchLen[0], Ord(ReplChar));
  end;
  //
var
  N_Str, N_Cmt: integer;
begin
  if RE_Str=nil then
    RE_Str:= TRegExpr.Create('"(\\.|.)*?"');
  if RE_Cmt=nil then
    RE_Cmt:= TRegExpr.Create('/\*.*?\*/');
  if RE_CmtLine=nil then
    RE_CmtLine:= TRegExpr.Create('//.*$');

  repeat
    N_Str:= Pos('"', S);
    N_Cmt:= Pos('/*', S);
    if (N_Str>0) and ((N_Cmt=0) or (N_Str<N_Cmt)) then
    begin
      if not _ReplaceByRegex('_', RE_Str) then Break;
    end
    else
    if (N_Cmt>0) and ((N_Str=0) or (N_Str>N_Cmt)) then
    begin
      if not _ReplaceByRegex(' ', RE_Cmt) then Break;
    end
    else
      Break;
  until false;

  _ReplaceByRegex(' ', RE_CmtLine);
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

finalization
  if Assigned(RE_Str) then
    FreeAndNil(RE_Str);
  if Assigned(RE_Cmt) then
    FreeAndNil(RE_Cmt);
  if Assigned(RE_CmtLine) then
    FreeAndNil(RE_CmtLine);

end.

