{
    This file is part of the Free Component Library

    JSON source lexical scanner
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

unit jsonscanner;

interface

uses SysUtils, Classes;

resourcestring
  SErrInvalidCharacter = 'Invalid character at line %d, pos %d: ''%s''';
  SUnterminatedComment = 'Unterminated comment at line %d, pos %d: ''%s''';
  SErrOpenString = 'string exceeds end of line';

type

  TJSONToken = (
    tkEOF,
    tkWhitespace,
    tkString,
    tkNumber,
    tkTrue,
    tkFalse,
    tkNull,
    // Simple (one-character) tokens
    tkComma,                 // ','
    tkColon,                 // ':'
    tkCurlyBraceOpen,        // '{'
    tkCurlyBraceClose,       // '}'
    tkSquaredBraceOpen,       // '['
    tkSquaredBraceClose,      // ']'
    tkIdentifier,            // Any Javascript identifier
    tkComment,
    tkUnknown
    );

  EScannerError       = class(EParserError);

  TJSONOption = (joUTF8,joStrict,joComments,joIgnoreTrailingComma);
  TJSONOptions = set of TJSONOption;

Const
  DefaultOptions = [joUTF8];

Type

  { TJSONScanner }

  TJSONScanner = class
  private
    FSource : TStringList;
    FCurRow: Integer;
    FCurToken: TJSONToken;
    FCurTokenString: string;
    FCurLine: string;
    TokenStr: PChar;
    FOptions : TJSONOptions;
    function GetCurColumn: Integer; inline;
    function GetO(AIndex: TJSONOption): Boolean;
    procedure SetO(AIndex: TJSONOption; AValue: Boolean);
  protected
    procedure Error(const Msg: string);overload;
    procedure Error(const Msg: string; Const Args: array of Const);overload;
    function DoFetchToken: TJSONToken; inline;
  public
    constructor Create(Source : TStream; AUseUTF8 : Boolean = True); overload; deprecated 'use options form instead';
    constructor Create(const Source : String; AUseUTF8 : Boolean = True); overload; deprecated  'use options form instead';
    constructor Create(Source: TStream; AOptions: TJSONOptions); overload;
    constructor Create(const Source: String; AOptions: TJSONOptions); overload;
    destructor Destroy; override;
    function FetchToken: TJSONToken;


    property CurLine: string read FCurLine;
    property CurRow: Integer read FCurRow;
    property CurColumn: Integer read GetCurColumn;

    property CurToken: TJSONToken read FCurToken;
    property CurTokenString: string read FCurTokenString;
    // Use strict JSON: " for strings, object members are strings, not identifiers
    Property Strict : Boolean Index joStrict Read GetO Write SetO ; deprecated 'use options instead';
    // if set to TRUE, then strings will be converted to UTF8 ansistrings, not system codepage ansistrings.
    Property UseUTF8 : Boolean index joUTF8 Read GetO Write SetO; deprecated 'Use options instead';
    // Parsing options
    Property Options : TJSONOptions Read FOptions Write FOptions;
  end;

const
  TokenInfos: array[TJSONToken] of string = (
    'EOF',
    'Whitespace',
    'String',
    'Number',
    'True',
    'False',
    'Null',
    ',',
    ':',
    '{',
    '}',
    '[',
    ']',
    'identifier',
    'comment',
    ''
  );


implementation

constructor TJSONScanner.Create(Source : TStream; AUseUTF8 : Boolean = True);

Var
  O : TJSONOptions;

begin
  O:=DefaultOptions;
  if AUseUTF8 then
    Include(O,joUTF8)
  else
    Exclude(O,joUTF8);
  Create(Source,O);
end;

constructor TJSONScanner.Create(const Source : String; AUseUTF8 : Boolean = True);
Var
  O : TJSONOptions;

begin
  O:=DefaultOptions;
  if AUseUTF8 then
    Include(O,joUTF8)
  else
    Exclude(O,joUTF8);
  Create(Source,O);
end;

constructor TJSONScanner.Create(Source: TStream; AOptions: TJSONOptions);
begin
  FSource:=TStringList.Create;
  FSource.LoadFromStream(Source);
  FOptions:=AOptions;
end;

constructor TJSONScanner.Create(const Source: String; AOptions: TJSONOptions);
begin
  FSource:=TStringList.Create;
  FSource.Text:=Source;
  FOptions:=AOptions;
end;

destructor TJSONScanner.Destroy;
begin
  FreeAndNil(FSource);
  Inherited;
end;


function TJSONScanner.FetchToken: TJSONToken;
  
begin
  Result:=DoFetchToken;
end;

procedure TJSONScanner.Error(const Msg: string);
begin
  raise EScannerError.Create(Msg);
end;

procedure TJSONScanner.Error(const Msg: string; const Args: array of const);
begin
  raise EScannerError.CreateFmt(Msg, Args);
end;

function TJSONScanner.DoFetchToken: TJSONToken;

  function FetchLine: Boolean;
  begin
    Result:=FCurRow<FSource.Count;
    if Result then
      begin
      FCurLine:=FSource[FCurRow];
      TokenStr:=PChar(FCurLine);
      Inc(FCurRow);
      end
    else             
      begin
      FCurLine:='';
      TokenStr:=nil;
      end;
  end;

var
  TokenStart: PChar;
  it : TJSONToken;
  I : Integer;
  OldLength, SectionLength,  tstart,tcol: Integer;
  C : char;
  S : String;
  IsStar,EOC: Boolean;

begin
  if TokenStr = nil then
    if not FetchLine then
      begin
      Result := tkEOF;
      FCurToken := Result;
      exit;
      end;

  FCurTokenString := '';

  case TokenStr[0] of
    #0:         // Empty line
      begin
      FetchLine;
      Result := tkWhitespace;
      end;
    #9, ' ':
      begin
      Result := tkWhitespace;
      repeat
        Inc(TokenStr);
        if TokenStr[0] = #0 then
          if not FetchLine then
          begin
            FCurToken := Result;
            exit;
          end;
      until not (TokenStr[0] in [#9, ' ']);
      end;
    '"','''':
      begin
        C:=TokenStr[0];
        If (C='''') and (joStrict in Options) then
          Error(SErrInvalidCharacter, [CurRow,CurColumn,TokenStr[0]]);
        Inc(TokenStr);
        TokenStart := TokenStr;
        OldLength := 0;
        FCurTokenString := '';
        while not (TokenStr[0] in [#0,C]) do
          begin
          if (TokenStr[0]='\') then
            begin
            // Save length
            SectionLength := TokenStr - TokenStart;
            Inc(TokenStr);
            // Read escaped token
            Case TokenStr[0] of
              '"' : S:='"';
              '''' : S:='''';
              't' : S:=#9;
              'b' : S:=#8;
              'n' : S:=#10;
              'r' : S:=#13;
              'f' : S:=#12;
              '\' : S:='\';
              '/' : S:='/';
              'u' : begin
                    S:='0000';
                    For I:=1 to 4 do
                      begin
                      Inc(TokenStr);
                      Case TokenStr[0] of
                        '0'..'9','A'..'F','a'..'f' :
                          S[i]:=Upcase(TokenStr[0]);
                      else
                        Error(SErrInvalidCharacter, [CurRow,CurColumn,TokenStr[0]]);
                      end;
                      end;
                    // WideChar takes care of conversion...  
                    if (joUTF8 in Options) then
                      S:=Utf8Encode(WideString(WideChar(StrToInt('$'+S))))
                    else
                      S:=WideChar(StrToInt('$'+S));  
                    end;
              #0  : Error(SErrOpenString);
            else
              Error(SErrInvalidCharacter, [CurRow,CurColumn,TokenStr[0]]);
            end;
            SetLength(FCurTokenString, OldLength + SectionLength+1+Length(S));
            if SectionLength > 0 then
              Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
            Move(S[1],FCurTokenString[OldLength + SectionLength+1],Length(S));
            Inc(OldLength, SectionLength+Length(S));
            // Next char
            // Inc(TokenStr);
            TokenStart := TokenStr+1;
            end;
          if TokenStr[0] = #0 then
            Error(SErrOpenString);
          Inc(TokenStr);
          end;
        if TokenStr[0] = #0 then
          Error(SErrOpenString);
        SectionLength := TokenStr - TokenStart;
        SetLength(FCurTokenString, OldLength + SectionLength);
        if SectionLength > 0 then
          Move(TokenStart^, FCurTokenString[OldLength + 1], SectionLength);
        Inc(TokenStr);
        Result := tkString;
      end;
    ',':
      begin
        Inc(TokenStr);
        Result := tkComma;
      end;
    '0'..'9','.','-':
      begin
        TokenStart := TokenStr;
        while true do
        begin
          Inc(TokenStr);
          case TokenStr[0] of
            '.':
              begin
                if TokenStr[1] in ['0'..'9', 'e', 'E'] then
                begin
                  Inc(TokenStr);
                  repeat
                    Inc(TokenStr);
                  until not (TokenStr[0] in ['0'..'9', 'e', 'E','-','+']);
                end;
                break;
              end;
            '0'..'9': ;
            'e', 'E':
              begin
                Inc(TokenStr);
                if TokenStr[0] in ['-','+']  then
                  Inc(TokenStr);
                while TokenStr[0] in ['0'..'9'] do
                  Inc(TokenStr);
                break;
              end;
            else
              break;
          end;
        end;
        SectionLength := TokenStr - TokenStart;
        FCurTokenString:='';
        SetString(FCurTokenString, TokenStart, SectionLength);
        If (FCurTokenString[1]='.') then
          FCurTokenString:='0'+FCurTokenString;
        Result := tkNumber;
      end;
    ':':
      begin
        Inc(TokenStr);
        Result := tkColon;
      end;
    '{':
      begin
        Inc(TokenStr);
        Result := tkCurlyBraceOpen;
      end;
    '}':
      begin
        Inc(TokenStr);
        Result := tkCurlyBraceClose;
      end;  
    '[':
      begin
        Inc(TokenStr);
        Result := tkSquaredBraceOpen;
      end;
    ']':
      begin
        Inc(TokenStr);
        Result := tkSquaredBraceClose;
      end;
    '/' :
      begin
      if Not (joComments in Options) then
        Error(SErrInvalidCharacter, [CurRow,CurCOlumn,TokenStr[0]]);
      TokenStart:=TokenStr;
      Inc(TokenStr);
      Case Tokenstr[0] of
        '/' : begin
              SectionLength := Length(FCurLine)- (TokenStr - PChar(FCurLine));
              Inc(TokenStr);
              FCurTokenString:='';
              SetString(FCurTokenString, TokenStr, SectionLength);
              Fetchline;
              end;
        '*' :
          begin
          IsStar:=False;
          Inc(TokenStr);
          TokenStart:=TokenStr;
          Repeat
            if (TokenStr[0]=#0) then
              begin
              SectionLength := (TokenStr - TokenStart);
              S:='';
              SetString(S, TokenStart, SectionLength);
              FCurtokenString:=FCurtokenString+S;
              if not fetchLine then
                Error(SUnterminatedComment, [CurRow,CurCOlumn,TokenStr[0]]);
              TokenStart:=TokenStr;
              end;
            IsStar:=TokenStr[0]='*';
            Inc(TokenStr);
            EOC:=(isStar and (TokenStr[0]='/'));
          Until EOC;
          if EOC then
            begin
            SectionLength := (TokenStr - TokenStart-1);
            S:='';
            SetString(S, TokenStart, SectionLength);
            FCurtokenString:=FCurtokenString+S;
            Inc(TokenStr);
            end;
          end;
      else
        Error(SErrInvalidCharacter, [CurRow,CurCOlumn,TokenStr[0]]);
      end;
      Result:=tkComment;
      end;
    'a'..'z','A'..'Z','_':
      begin
        tstart:=CurRow;
        Tcol:=CurColumn;
        TokenStart := TokenStr;
        repeat
          Inc(TokenStr);
        until not (TokenStr[0] in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
        SectionLength := TokenStr - TokenStart;
        FCurTokenString:='';
        SetString(FCurTokenString, TokenStart, SectionLength);
        for it := tkTrue to tkNull do
          if CompareText(CurTokenString, TokenInfos[it]) = 0 then
            begin
            Result := it;
            FCurToken := Result;
            exit;
            end;
        if (joStrict in Options) then
          Error(SErrInvalidCharacter, [tStart,tcol,TokenStart[0]])
        else
          Result:=tkIdentifier;
      end;
  else
    Error(SErrInvalidCharacter, [CurRow,CurCOlumn,TokenStr[0]]);
  end;

  FCurToken := Result;
end;

function TJSONScanner.GetCurColumn: Integer;
begin
  Result := TokenStr - PChar(CurLine);
end;

function TJSONScanner.GetO(AIndex: TJSONOption): Boolean;
begin
  Result:=AIndex in FOptions;
end;

procedure TJSONScanner.SetO(AIndex: TJSONOption; AValue: Boolean);
begin
  If AValue then
    Include(Foptions,AIndex)
  else
    Exclude(Foptions,AIndex)
end;

end.
