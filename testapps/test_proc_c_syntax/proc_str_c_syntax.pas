unit proc_str_c_syntax;

{$mode ObjFPC}{$H+}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils;

function CSyntax_LineBeginsWithBlockKeyword(const S: UnicodeString): boolean;
function CSyntax_LineEndSymbol(const S: UnicodeString): WideChar;
function CSyntax_IsLineComment(const S: UnicodeString): boolean;
procedure CSyntax_DeleteStringsAndComments(var S: UnicodeString);


implementation

uses
  ATStringProc;


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
var
  N_Str, N_Cmt, N_End, Len: integer;
begin
  repeat
    N_Str:= Pos('"', S);
    N_Cmt:= Pos('/*', S);

    if (N_Str>0) and ((N_Cmt=0) or (N_Str<N_Cmt)) then
    begin
      Len:= Length(S);
      N_End:= N_Str;
      repeat
        Inc(N_End);
        if N_End>Len then
        begin
          FillWord(S[N_Str], Length(S)-N_Str+1, Ord('_'));
          Break;
        end;
        if S[N_End]='\' then //support escape char
        begin
          Inc(N_End);
          Continue;
        end;
        if S[N_End]='"' then
        begin
          FillWord(S[N_Str], N_End-N_Str+1, Ord('_'));
          Break;
        end;
      until false;
    end
    else
    if (N_Cmt>0) and ((N_Str=0) or (N_Str>N_Cmt)) then
    begin
      N_End:= Pos('*/', S, N_Cmt+2);
      if N_End>0 then
        FillWord(S[N_Cmt], N_End-N_Cmt+2, Ord(' '))
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

function CSyntax_IsLineComment(const S: UnicodeString): boolean;
var
  Len, i: integer;
begin
  Result:= false;
  Len:= Length(S);
  i:= 1;
  while (i<Len) and IsCharSpace(S[i]) do
    Inc(i);
  if i>Len then exit;
  if i+1>Len then exit;
  Result:= (S[i]='/') and (S[i+1]='/');
end;


end.

