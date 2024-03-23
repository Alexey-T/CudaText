unit proc_str_c_syntax;

{$mode ObjFPC}{$H+}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils;

function CSyntax_LineBeginsWithBlockKeyword(const S: UnicodeString): boolean;
function CSyntax_LineEndSymbol(const S: UnicodeString): WideChar;
procedure CSyntax_DeleteStringsAndComments(var S: UnicodeString);


implementation

uses
  ATStringProc,
  atsynedit_regexpr;

var
  RE_Str: TRegExpr;

function CSyntax_LineBeginsWithBlockKeyword(const S: UnicodeString): boolean;
var
  Len, i1, i2: integer;
  Sub: UnicodeString;
begin
  Result:= false;

  Len:= Length(S);
  i1:= 1;
  while (i1<=Len) and (S[i1]=' ') do Inc(i1);
  if i1>Len then exit;

  i2:= i1;
  while (i2<=Len) and IsCharWordInIdentifier(s[i2]) do Inc(i2);
  Sub:= Copy(S, i1, i2-i1);

  case Sub of
    'if',
    'while',
    'do',
    'for',
    'else',
    'foreach':
      Result:= true
    else
      Result:= false;
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
  N_Str, N_Cmt, N_CmtEnd: integer;
begin
  if RE_Str=nil then
    RE_Str:= TRegExpr.Create('"(\\.|.)*?"');

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
      N_CmtEnd:= Pos('*/', S, N_Cmt+2);
      if N_CmtEnd>0 then
        FillWord(S[N_Cmt], N_CmtEnd-N_Cmt+2, Ord(' '))
      else
      begin
        FillWord(S[N_Cmt], Length(S)-N_Cmt+1, Ord(' '));
        Break;
      end;
    end
    else
      Break;
  until false;

  N_Cmt:= Pos('//', S);
  if N_Cmt>0 then
    FillWord(S[N_Cmt], Length(S)-N_Cmt+1, Ord(' '));
end;


function CSyntax_LineEndSymbol(const S: UnicodeString): WideChar;
var
  T: UnicodeString;
  i: integer;
begin
  Result:= '?';
  T:= S;
  UniqueString(T);
  CSyntax_DeleteStringsAndComments(T);
  i:= Length(T);
  while (i>0) and (T[i]=' ') do
    Dec(i);
  if i>0 then
    Result:= T[i];
end;

finalization
  if Assigned(RE_Str) then
    FreeAndNil(RE_Str);

end.

