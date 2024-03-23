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
function CSyntax_LineEndSymbol(const S: UnicodeString): TEditorCSyntaxSymbol;
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
  function _FillByRegex(RE: TRegExpr; ReplChar: WideChar): boolean;
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
      if not _FillByRegex(RE_Str, '_') then
      begin
        //if regex cannot find ending, still fill
        FillWord(S[N_Str], Length(S)-N_Str+1, Ord('_'));
        Break;
      end;
    end
    else
    if (N_Cmt>0) and ((N_Str=0) or (N_Str>N_Cmt)) then
    begin
      if not _FillByRegex(RE_Cmt, ' ') then
      begin
        //if regex cannot find ending, still fill
        FillWord(S[N_Cmt], Length(S)-N_Cmt+1, Ord(' '));
        Break;
      end;
    end
    else
      Break;
  until false;

  _FillByRegex(RE_CmtLine, ' ');
end;


function CSyntax_LineEndSymbol(const S: UnicodeString): TEditorCSyntaxSymbol;
var
  T: UnicodeString;
  i: integer;
begin
  Result:= TEditorCSyntaxSymbol.Unknown;
  T:= S;
  UniqueString(T);
  CSyntax_DeleteStringsAndComments(T);
  i:= Length(T);
  while (i>0) and (T[i]=' ') do
    Dec(i);
  if i>0 then
    case T[i] of
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

