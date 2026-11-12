{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown inline text handler.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Some ideas for the inline text parsing were gleaned from the parser at
  https://github.com/grahamegrieve/delphi-markdown
}

unit Markdown.InlineText;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs,
{$ELSE}
  Classes, SysUtils, Contnrs,
{$ENDIF}
  Markdown.Scanner,
  Markdown.Elements,
  Markdown.Utils;

Type
  TMarkdownDelimiterMode = (dmOpen,dmClose);
  TMarkdownDelimiterModes = Set of TMarkdownDelimiterMode;

  { TMarkdownDelimiter }

  TMarkdownDelimiter = class
  private
    FModes: TMarkdownDelimiterModes;
    FDelimiter: String;
    FActive: boolean;
    FNode: TMarkdownTextNode;
  public
    constructor Create(aNode: TMarkdownTextNode; const aDelimiter: String; aModes: TMarkdownDelimiterModes);
    procedure RemoveLast(aCount : integer);
    function CanClose(aDelim: TMarkdownDelimiter) : boolean;
    function Opens : Boolean;
    function Closes : Boolean;
    function OpensCloses : Boolean;
    property Node : TMarkdownTextNode read FNode;
    property Delimiter : String read FDelimiter write FDelimiter;
    property Active : Boolean read FActive write FActive;
    property Modes : TMarkdownDelimiterModes read FModes write FModes;
    function IsEmph : Boolean;
  end;
  TMarkdownDelimiterList = class (specialize TGFPObjectList<TMarkdownDelimiter>);

  { TInlineTextProcessor }

  TInlineTextProcessor = class
  Private
    FGFMExtensions : Boolean;
    FScanner : TMarkdownTextScanner;
    FNodes: TMarkdownTextNodeList;
    FWhiteSpaceMode: TWhitespaceMode;
    FEntities : TFPStringHashTable;
    FStack : TMarkdownDelimiterList;
  protected
    // Reading
    function ReadInlineBracketedLink(aBuilder: TStringBuilder): Boolean;
    function ReadInlineNormalLink(aBuilder: TStringBuilder): Boolean;
    function ReadLinkTitle(aBuilder: TStringBuilder): Boolean;
    function PeekEmailAddress(out len: integer): boolean; virtual;
    procedure AddTextTillNext(const aTerminal: String); virtual;
    // Handle various constructs
    procedure HandleEmphasis(aTerminator: TMarkdownDelimiter); virtual;
    function  HandleInlineLink(aDelim: TMarkdownDelimiter): boolean; virtual;
    procedure HandleBackTick; virtual;
    procedure HandleDelimiter(aAllowMulti: boolean); virtual;
    procedure HandleEntity; virtual;
    function  HandleEntityInner: String; virtual;
    procedure HandleTilde; virtual;
    procedure HandleTextEscape; virtual;
    procedure HandleTextCore; virtual;
    procedure HandleAutoLink; virtual;
    procedure HandleCloseDelimiter(); virtual;
    procedure HandleGFMExtensions; virtual;
    procedure HandleGFMLinkEmail(aLength: integer); virtual;
    procedure HandleGFMLinkURL(aStartChars : integer; const aProtocol : string = ''); virtual;
  public
    constructor Create(aLexer : TMarkdownTextScanner; aNodes: TMarkdownTextNodeList; aEntities : TFPStringHashTable; awsMode : TWhitespaceMode);
    destructor Destroy; override;
    function process(aCloseLast : boolean = false): TMarkdownTextNodeList;
    procedure DumpNodes;
    property Scanner : TMarkdownTextScanner read FScanner;
    property Nodes : TMarkdownTextNodeList Read FNodes;
    property WhitespaceMode : TWhitespaceMode read FWhiteSpaceMode;
    Property GFMExtensions : Boolean Read FGFMExtensions Write FGFMExtensions;
  end;
  TInlineTextProcessorClass = class of TInlineTextProcessor;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.StrUtils, System.Math;
{$ELSE}
uses StrUtils, Math;
{$ENDIF}

const
  dmOpenClose = [dmOpen,dmClose];

{ TMarkdownDelimiter }

constructor TMarkdownDelimiter.Create(aNode: TMarkdownTextNode; const aDelimiter : String; aModes: TMarkdownDelimiterModes);
begin
  inherited create;
  FNode:=aNode;
  FModes:=aModes;
  FDelimiter:=aDelimiter;
  FActive:=true;
end;

procedure TInlineTextProcessor.DumpNodes;

var
  I : Integer;

begin
  Writeln('Current Nodes (',FNodes.Count,'):');
  for I:=0 to FNodes.Count-1 do
    With FNodes[i] do
      Writeln('Node ',I,'[',Kind,']: ',NodeText);
  Writeln;
end;


procedure TMarkdownDelimiter.RemoveLast(aCount: integer);
begin
  SetLength(FDelimiter,Length(FDelimiter)-aCount);
end;

function TMarkdownDelimiter.CanClose(aDelim: TMarkdownDelimiter): boolean;
begin
  Result:=(Delimiter[1]=aDelim.Delimiter[1]) and aDelim.Opens;
end;

function TMarkdownDelimiter.Closes: Boolean;
begin
  Result:=dmClose in FModes;
end;

function TMarkdownDelimiter.OpensCloses: Boolean;
begin
  Result:=(FModes=dmOpenClose);
end;

function TMarkdownDelimiter.IsEmph: Boolean;

begin
  Result:=(Delimiter<>'[') and (Delimiter<>'![');
end;


function TMarkdownDelimiter.Opens: Boolean;

begin
  Result:=dmOpen in FModes;
end;

{ TInlineTextProcessor }

constructor TInlineTextProcessor.Create(aLexer: TMarkdownTextScanner; aNodes: TMarkdownTextNodeList; aEntities: TFPStringHashTable; awsMode: TWhitespaceMode);

begin
  FScanner:=aLexer;
  FNodes:=aNodes;
  FWhiteSpaceMode:=awsMode;
  FEntities:=aEntities;
  FStack:=TMarkdownDelimiterList.Create;
end;

destructor TInlineTextProcessor.Destroy;
begin
  FreeAndNil(FStack);
  inherited destroy;
end;

procedure TInlineTextProcessor.HandleTextEscape;

var
  C : Char;

begin
  Scanner.Bookmark;
  Scanner.NextChar();
  C:=Scanner.Peek;
  if MustEscape(C) then
    Nodes.addText(Scanner.location,htmlEscape(Scanner.NextChar()))
  else if C=#10 then
    Nodes.addTextNode(Scanner.location,nkLineBreak,'')
  else
    begin
    Scanner.GotoBookmark;
    Nodes.addText(Scanner.location,Scanner.NextChar());
    end;
end;


procedure TInlineTextProcessor.HandleAutoLink;

var
  lText : String;
  lNode : TMarkdownTextNode;
  lAdd : boolean;
  lNodeKind : TTextNodeKind;
  lURL : String;

begin
  Scanner.BookMark;
  Scanner.NextChar();
  lText:=Scanner.PeekUntil(['>']);
  lAdd:=false;
  if (WhiteSpaceMode<>wsLeave) then
    begin
    lAdd:=isAbsoluteURI(lText);
    if lAdd then
      begin
      lNodeKind:=nkURI;
      lURL:=urlEscape(lText);
      end
    else
      begin
      lAdd:=IsEmailLike(lText);
      if lAdd then
        begin
        lNodeKind:=nkEmail;
        lURL:='mailto:'+urlEscape(lText);
        end
      end;
    if lAdd then
      begin
      lNode:=Nodes.addTextNode(Scanner.location,lNodeKind,lText);
      lNode.attrs.Add('href',lURL);
      Scanner.NextChars(Length(lText));
      Scanner.NextChar();
      end;
    end;
  if lAdd then
    Exit;
  Scanner.GotoBookmark;
  Nodes.addText(Scanner.location,'&lt;');
  Scanner.NextChar();
end;

procedure TInlineTextProcessor.HandleEntity;
begin
  if WhiteSpaceMode<>wsLeave then
    Nodes.addText(Scanner.location,htmlEscape(HandleEntityInner()))
  else
    begin
    Nodes.addText(Scanner.location,'&amp;');
    Scanner.NextChar();
    end
end;

function TInlineTextProcessor.HandleEntityInner: String;

var
  lEntity : String;
  ch : UnicodeChar;
  I,lLen : integer;

begin
  Scanner.NextChar();
  lEntity:=Scanner.PeekUntil([';']);
  lLen:=Length(lEntity);
  Result:=FEntities.Items[lEntity];
  if Result<>'' then
    begin
    Scanner.NextChars(lLen+1);
    Exit;
    end;
  Result:='&';
  // If not a unicode char, just return the &
  if (lLen<0) or (lLen>9) or (lEntity[1]<>'#') then
    exit;
  // Unicode char.
  System.Delete(lEntity,1,1);
  if TryStrToInt(lEntity,I) then
    begin
    if (i=0) or (i>65535) then
      ch:=#$FFFD
    else
      ch:=UnicodeChar(I);
    Scanner.NextChars(lLen+1);
    Result:=Utf8Encode(ch);
    end;
end;

procedure TInlineTextProcessor.HandleBackTick;
var
  lBackTick : String;
  lLen : integer;

begin
  lBackTick:=Scanner.PeekRun(false);
  lLen:=length(lBackTick);
  if not Scanner.FindMatchingOccurrence(lBackTick) then
    begin
    Nodes.addText(Scanner.location,Scanner.NextChars(lLen));
    Exit;
    end;
  Nodes.addTextNode(Scanner.location,nkCode,'',false);
  AddTextTillNext(lBackTick);
end;

procedure TInlineTextProcessor.HandleTilde;
var
  lTilde : String;
  lLen : Integer;
  ldelete : boolean;
  lNode : TMarkdownTextNode;

begin
  lTilde:=Scanner.PeekRun(false);
  lLen:=Length(lTilde);
  lDelete:=FGFMExtensions and (lLen=2) and Scanner.FindMatchingOccurrence(lTilde,#10);
  if not lDelete then
    begin
    Nodes.addText(Scanner.location,Scanner.NextChars(lLen));
    Exit;
    end;
  lNode:=Nodes.addTextNode(Scanner.location,nkText,'',False);
  lNode.AddStyle(nsDelete);
  AddTextTillNext(lTilde);
end;

procedure TInlineTextProcessor.AddTextTillNext(const aTerminal : String);
var
  lWhitespace,lFirstWhitespace : boolean;
  C : char;
  lLen : integer;
begin
  lLen:=Length(aTerminal);
  Scanner.NextChars(lLen);
  lWhitespace:=false;
  lFirstWhitespace:=true;
  while Scanner.PeekRun(True)<>aTerminal do
    begin
    C:=Scanner.Peek;
    if isWhitespaceChar(C) then
      begin
      lWhitespace:=true;
      Scanner.NextChar;
      end
    else
      begin
      if lWhitespace and not lFirstWhitespace then
        Nodes.addText(Scanner.location,' ');
      lFirstWhitespace:=false;
      lWhitespace:=false;
      Nodes.addText(Scanner.location,htmlEscape(Scanner.NextChar));
      end;
    end;
  Scanner.NextChars(lLen);
  Nodes.lastNode.Active:=False;
end;

procedure TInlineTextProcessor.HandleDelimiter(aAllowMulti : boolean);

  function IsWhiteSpace(aChar : Char) : boolean;
  // Handle end of line in addition to normal whitespace
  const
    EOL = [#0,#10];
  begin
    Result:=CharInSet(aChar,EOL) or Markdown.Utils.IsWhiteSpace(aChar);
  end;

  function isPunctuation(aChar : Char) : boolean;
  // Handle special chars in addition to Unicode punctuation
  const
    SpecialPunctuation = ['!','"','#','$','%','&','''','(',')','*','+',',','-','.','/',':',';','<','=','>','?','@','[','\',']','^','_','`','{','|','}','~'];
  begin
    Result:=CharInSet(aChar,SpecialPunctuation) or isUnicodePunctuation(aChar);
  end;

  function isLeftFlanking(aPrevious,aNext : char) : boolean;

  begin
   // (1) not followed by Unicode whitespace.
    Result:=not isWhiteSpace(aNext);
    if not Result then
      Exit;
    // (2a) not followed by a punctuation character.
    // or
    // (2b) preceded by Unicode whitespace or a punctuation character.
    Result:=(not isPunctuation(aNext)) // 2a
             or isWhiteSpace(aPrevious)   // 2b
             or isPunctuation(aPrevious); // 2b
  end;

  function isRightFlanking(aPrevious,aNext : char) : boolean;

  begin
    // (1) not preceded by Unicode whitespace.
    Result:=Not isWhiteSpace(aPrevious);
    if not Result then
      exit;
    // (2a) not preceded by a Unicode punctuation character.
    // or
    // (2b) preceded by a Unicode punctuation character and followed by Unicode whitespace or a Unicode punctuation character.
    Result:=(not isPunctuation(aPrevious)) // 2a
            or isWhiteSpace(aNext) or isPunctuation(aNext); // 2b. No need to check unicode punctuation, it's true at this point
  end;

var
  cPrevious,cNext : Char;
  lDelim : String;
  lLeft,lRight : boolean;
  lPos : TPosition;
  lModes : TMarkdownDelimiterModes;
  lNode : TMarkdownTextNode;

begin
  if aAllowMulti then
    lDelim:=Scanner.PeekRun(false)
  else
    lDelim:=Scanner.PeekLen(1+Ord(Scanner.Peek='!'));
  cPrevious:=Scanner.PeekPrevious;
  lPos:=Scanner.location;
  // have pos, skip delimiter
  Scanner.NextChars(Length(lDelim));
  cNext:=Scanner.Peek;
  lLeft:=isLeftFlanking(cPrevious,cNext);
  lRight:=isRightFLanking(cPrevious,cNext);
  if lLeft then
    begin
    if lRight then
      lModes:=[dmOpen,dmClose]
    else
      lModes:=[dmOpen]
    end
  else if lRight then
    lModes:=[dmClose]
  else
    lModes:=[];
  lNode:=Nodes.addTextNode(lPos,nkText,'',False);
  FStack.Add(TMarkdownDelimiter.Create(lNode,lDelim,lModes));
end;

function TInlineTextProcessor.ReadInlineBracketedLink(aBuilder : TStringBuilder) : Boolean;

begin
  Result:=False;
  Scanner.NextChar;
  while not Scanner.EOF and (Scanner.Peek <> '>') do
    begin
    if (Scanner.Peek='\') and MustEscape(Scanner.PeekNext) then
      begin
      Scanner.NextChar;
      if Not Scanner.EOF then
        aBuilder.Append(Scanner.NextChar);
      end
    else if isWhitespaceChar(Scanner.Peek) then
      begin
      if FGFMExtensions and (Scanner.Peek=' ') then
        begin
        aBuilder.Append('%20');
        Scanner.NextChar;
        end
      else
        Exit;
      end
    else if Scanner.Peek='&' then
      aBuilder.Append(HandleEntityInner)
    else
      aBuilder.Append(Scanner.NextChar);
    end;
  Result:=Not Scanner.EOF;
end;

function TInlineTextProcessor.ReadInlineNormalLink(aBuilder : TStringBuilder) : Boolean;

var
  lLevel : Integer;
  C : Char;

  function EndOfLink : boolean;
  var
    lNext : Char;
  begin
    lNext:=Scanner.Peek;
    Result:=((lLevel=0) and (lNext=')')) or isWhitespace(lNext);
  end;

begin
  Result:=False;
  lLevel:=0;
  while not Scanner.EOF and not EndOfLink  do
    begin
    C:=Scanner.Peek;
    if (C='\') and MustEscape(Scanner.PeekNext) then
      begin
      Scanner.NextChar;
      if Not Scanner.EOF then
        aBuilder.Append(Scanner.NextChar);
      end
    else if isWhitespaceChar(C) then
      Exit
    else if (C='&') then
      aBuilder.Append(HandleEntityInner())
    else
      begin
      if C='(' then
        inc(lLevel)
      else if (C=')') then
        dec(lLevel);
      aBuilder.Append(Scanner.NextChar);
      end;
    end;
  Result:=Not Scanner.EOF;
end;

function TInlineTextProcessor.ReadLinkTitle(aBuilder : TStringBuilder) : Boolean;

Var
  C,EOT : char;

begin
  Result:=False;
  Scanner.skipWhitespace;
  EOT:=Scanner.NextChar;
  if not CharInSet(EOT,['"','''','(']) then
    Exit;
  if (EOT='(') then
    EOT:=')';
  while not Scanner.EOF and (Scanner.Peek<>EOT) do
    begin
    C:=Scanner.Peek;
    Case C of
    '\':
       begin
       Scanner.NextChar;
       if Scanner.EOF then
         Exit;
       aBuilder.Append(htmlEscape(Scanner.NextChar));
       end;
    '&':
       aBuilder.Append(htmlEscape(HandleEntityInner()));
    #10:
       aBuilder.Append(' ');
    '"':
      aBuilder.Append(htmlEscape(Scanner.NextChar));
    else
      aBuilder.Append(Scanner.NextChar);
    end;
    end;
  Result:=Not Scanner.EOF;
end;

function TInlineTextProcessor.HandleInlineLink(aDelim : TMarkdownDelimiter): boolean;
{
  There are 4 cases we want to handle:
  - inline link/image  [text](link title)
  - reference link/image [text][ref]
  - compact reference link/image [label][]
  - shortcut reference link/image [label]
}

var
  lURL : String;
  lTitle : String;
  lBuilder : TStringBuilder;
  lDelim : TMarkdownDelimiter;

begin
  Result:=false;
  lTitle:='';
  lBuilder:=TStringBuilder.Create;
  Scanner.BookMark;
  try
    Scanner.NextChar;
    if Scanner.Peek<>'(' then
      Exit;
    Scanner.NextChar; // (
    Scanner.skipWhitespace;
    if Scanner.Peek='<' then
      begin
      if not ReadInlineBracketedlink(lBuilder) then
        Exit;
      Scanner.NextChar;
      end
    else if not ReadInlineNormalLink(lBuilder) then
      Exit;
    lURL:=URLEscape(lBuilder.ToString);
    if Scanner.Peek <> ')' then
      begin
      lBuilder.clear;
      if not ReadLinkTitle(lBuilder) then
        Exit;
      Scanner.NextChar;
      lTitle:=lBuilder.toString;
      end;
    Scanner.skipWhiteSpace;
    if Scanner.Peek <> ')' then
      Exit;
    Scanner.NextChar;
    if aDelim.delimiter = '![' then
      begin
      aDelim.Node.Kind:=nkimg;
      aDelim.Node.active:=false;
      aDelim.Node.attrs.Add('src',URLEscape(lURL));
      aDelim.Node.attrs.Add('alt',aDelim.node.NodeText);
      end
    else
      begin
      aDelim.Node.Kind:=nkUri;
      aDelim.Node.attrs.Add('href',URLEscape(lURL));
      aDelim.Node.active:=false;
      for lDelim in FStack do
        if lDelim = aDelim then
          break
        else if lDelim.delimiter='[' then
          lDelim.active:=false;
      end;
    if lTitle<>'' then
      aDelim.node.attrs.Add('title',lTitle);
    HandleEmphasis(aDelim);
    FStack.remove(aDelim);
    Result:=true;
  finally
    if not Result then
      Scanner.GotoBookmark;
    lBuilder.free;
  end;
end;

procedure TInlineTextProcessor.HandleEmphasis(aTerminator : TMarkdownDelimiter);

const
  NodeStyles : Array[Boolean] of TNodeStyle = (nsEmph,nsStrong);

var
  lCount,lClose,lOpen,lBottom,lCurrBottom,lRemoveLen,lDelete :  integer;
  lStrong : boolean;
  lBottoms : Array[Boolean] of Integer; // False: strong or more, True: emph
  lOpens,lCloses : TMarkdownDelimiter;

  function IsMatch(aDelim1,aDelim2 : TMarkdownDelimiter) : boolean;

  begin
    Result:=aDelim2.CanClose(aDelim1);
    if Result and (aDelim1.OpensCloses or aDelim2.OpensCloses) then
      Result:=(Length(aDelim1.Delimiter)+Length(aDelim2.Delimiter)) mod 3 <> 0;
  end;

  Function FindClosingDelimiter(aStart : integer) : Integer;
  var
    lIdx : Integer;
  begin
    lIdx:=aStart;
    while (lIdx<lCount) and FStack[lIdx].isEmph and not FStack[lIdx].Closes do
      inc(lIdx);
    Result:=lIdx;
  end;

  Function FindOpeningDelimiter(aClose : integer) : Integer;
  var
    lIdx: integer;
  begin
    lIdx:=aClose-1;
    while (lIdx>=lCurrBottom) and not isMatch(FStack[lIdx],FStack[aClose]) do
      dec(lIdx);
    Result:=lIdx;
  end;

begin
  lBottom:=0;
  if Assigned(aTerminator) then
    lBottom:=FStack.IndexOf(aTerminator)+1;
  lBottoms[False]:=lBottom;
  lBottoms[True]:=lBottom;
  lCount:=FStack.count;
  lClose:=FindClosingDelimiter(lBottom);
  while lClose<lCount do
    begin
    lCloses:=FStack[lClose];
    // Determine bottom for this match
    lCurrBottom:=Max(lBottoms[lCloses.delimiter='*'],lBottom);
    lOpen:=FindOpeningDelimiter(lClose);
    if (lOpen>=0) and lCloses.CanClose(FStack[lOpen]) then
      begin
      // we have a match
      lOpens:=FStack[lOpen];
      lStrong:=(Length(lCloses.Delimiter)>1) and (Length(lOpens.Delimiter)>1);
      // apply the correct style
      Nodes.ApplyStyleBetween(lOpens.Node,lCloses.node,NodeStyles[lStrong]);
      // Remove the * or **
      lRemoveLen:=1+Ord(lStrong);
      lOpens.RemoveLast(lRemoveLen);
      lCloses.RemoveLast(lRemoveLen);
      // Remove delimiters in between opening and closing delimiter
      lDelete:=lClose;
      While lDelete>lOpen+1 do
        begin
        FStack.Delete(lDelete);
        Dec(lDelete);
        Dec(lClose);
        end;
      // if we have removed the last *, remove opening.
      if lOpens.Delimiter='' then
        begin
        FStack.Delete(lOpen);
        Dec(lClose);
        end;
      // if we have removed the last *, remove closing node.
      // If the attached node is empty, remove that too.
      if lCloses.Delimiter='' then
        begin
        if lCloses.node.isEmpty then
          Nodes.Remove(lCloses.node);
        FStack.Remove(lCloses);
        end;
      end
    else
      begin
      lBottoms[FStack[lClose].delimiter='*']:=lOpen+1;
      if FStack[lClose].Modes=dmOpenClose then
        inc(lClose)
      else
        FStack.Delete(lClose);
      end;
    lCount:=FStack.Count;
    lClose:=FindClosingDelimiter(lClose);
    end;
  for LOpen:=FStack.Count-1 downto lBottom do
    FStack.Delete(lOpen);
end;


procedure TInlineTextProcessor.HandleCloseDelimiter();

var
  I : integer;
  lDelim : TMarkdownDelimiter;

begin
  lDelim:=nil;
  I:=FStack.Count-1;
  while (I>=0) and (lDelim=Nil) do
    begin
    lDelim:=FStack[I];
    if IndexText(lDelim.delimiter,['[','!['])=-1 then
      lDelim:=nil;
    Dec(I);
    end;
  if Not Assigned(lDelim) then
    Nodes.addText(Scanner.location,Scanner.NextChar)
  else if Not (lDelim.active and HandleInlineLink(lDelim)) then
    begin
    FStack.Remove(lDelim);
    Nodes.addText(Scanner.location,Scanner.NextChar);
    end;
end;


procedure TInlineTextProcessor.HandleTextCore;

begin
  // code blocks leave whitespace intact
  if WhiteSpaceMode=wsLeave then
    begin
    Nodes.addText(Scanner.location,htmlEscape(Scanner.NextChar));
    Exit;
    end;
  case Scanner.Peek of
    '\' : HandleTextEscape();
    '<' : HandleAutoLink();
    '>','"' : Nodes.addText(Scanner.location,htmlEscape(Scanner.NextChar));
    '&' : HandleEntity;
    '`' : HandleBackTick;
    '~' : HandleTilde;
    '*' : HandleDelimiter(true);
    '_' : if isWhitespaceChar(Scanner.PeekPrevious)
             or MustEscape(Scanner.PeekPrevious)
             or (Scanner.PeekPrevious=#0)
             or isWhitespaceChar(Scanner.PeekEndRun)
             or MustEscape(Scanner.PeekEndRun)
             or (Scanner.PeekEndRun=#0) then
            HandleDelimiter(true)
          else
            Nodes.addText(Scanner.Location,htmlEscape(Scanner.NextEquals()));
    '[' : HandleDelimiter(false);
    '!' : if Scanner.PeekNext='[' then
            HandleDelimiter(false)
          else
            Nodes.addText(Scanner.location,Scanner.NextChar);
    ']' : HandleCloseDelimiter();
  else
    if GFMExtensions then
      HandleGFMExtensions
    else
      Nodes.addText(Scanner.location,Scanner.NextChar);
  end;
  //  DumpNodes;
end;


procedure TInlineTextProcessor.HandleGFMExtensions;

var
  lLen : integer;

begin
  if Scanner.has('www.') then
    HandleGFMLinkURL(4,'http://')
  else if Scanner.has('http://') then
    HandleGFMLinkURL(7)
  else if Scanner.has('https://') then
    HandleGFMLinkURL(8)
  else if Scanner.has('ftp://') then
    HandleGFMLinkURL(6)
  else if PeekEmailAddress(lLen) then
    HandleGFMLinkEmail(lLen)
  else
    Nodes.addText(Scanner.location,Scanner.NextChar);
end;


function TInlineTextProcessor.PeekEmailAddress(out len : integer) : boolean;

const
  cEMailChars = ['a'..'z','A'..'Z','0'..'9','+','-','_','@','.'];

var
  lLen : integer;
  cLast : Char;
  S : String;

begin
  Result:=False;
  S:=Scanner.PeekWhile(cEmailChars);
  lLen:=Length(S);
  if lLen=0 then exit;
  cLast:=s[lLen];
  // Strip off _ or -
  if CharInSet(cLast,['_','-']) then
    Exit;
  if cLast='.' then
    SetLength(S,lLen-1);
  Result:=IsEmailLike(S);
  if Result then
    Len:=Length(s);
end;


function TInlineTextProcessor.process(aCloseLast: boolean): TMarkdownTextNodeList;

var
  c : Char;
  lWhiteSpace : string;
  lHadNonWhitespace : boolean;

begin
  Result:=Nil;
  lWhiteSpace:='';
  lHadNonWhitespace:=false;
  while not Scanner.EOF do
    begin
    C:=Scanner.Peek;
    if (WhiteSpaceMode<>wsLeave) and isWhitespace(C) and ((C<>#10) or (WhiteSpaceMode=wsStrip)) then
      begin
      C:=Scanner.NextChar;
      if lHadNonWhitespace then
        lWhitespace:=lWhitespace+c;
      end
    else
      begin
      if (WhiteSpaceMode<>wsLeave) then
        begin
        lHadNonWhitespace:=C<>#10;
        if lHadNonWhitespace then
          begin
          if lWhiteSpace<>'' then
            begin
            if WhiteSpaceMode=wsStrip then
              Nodes.addText(Scanner.location,' ')
            else
              Nodes.addText(Scanner.location,lWhitespace);
            lWhiteSpace:='';
            end;
          end
        else
          begin
          if lWhitespace.EndsWith('  ') then
            Nodes.addText(Scanner.location,'<br />');
          lWhitespace:='';
          end;
        end;
      HandleTextCore();
      end;
    end;
  HandleEmphasis(nil);
  if aCloseLast and (Nodes.Count>0) then
    Nodes.lastNode.Active:=False;
end;


procedure TInlineTextProcessor.HandleGFMLinkEmail(aLength : integer);

var
  lEmail : String;
  lNode : TMarkdownTextNode;

begin
  lEmail:=Scanner.NextChars(aLength);
  lNode:=Nodes.addTextNode(Scanner.location,nkUri,htmlEscape(lEmail));
  lNode.attrs.Add('href','mailto:'+urlEscape(lEmail));
end;

procedure TInlineTextProcessor.HandleGFMLinkURL(aStartChars : integer; const aProtocol : String='');

Const
  URLChars = ['a'..'z','A'..'Z','0'..'9','_','-','.'];
  URLDelims = [' ',#10,'<'];

var
  lRewind : boolean;
  lRemove : Integer;
  lLink,lUrl,lText : string;
  lNode : TMarkdownTextNode;

begin
  Scanner.BookMark;
  lRewind:=True;
  try
    lLink:=Scanner.NextChars(aStartChars);
    while CharInSet(Scanner.Peek,UrlChars) do
      lLink:=lLink+Scanner.NextChar;
    if not lLink.Contains('.') then
      Exit;
    lRewind:=False;
    while not (Scanner.EOF or CharInSet(Scanner.Peek,URLDelims)) do
      lLink:=lLink+Scanner.NextChar;
    case lLink[lLink.Length] of
      '?','!','.',',',':','*','_','~':
        lRemove:=1;
      ')':
        lRemove:=Ord(lLink.CountChar('(')<lLink.CountChar(')'));
      ';':
        lRemove:=CheckForTrailingEntity(lLink);
    else
      lRemove:=0;
    end;
    if lRemove=0 then
      begin
      lNode:=Nodes.addTextNode(Scanner.location,nkEmail,htmlEscape(lLink));
      lNode.attrs.Add('href',aProtocol+urlEscape(lLink));
      end
    else
      begin
      lUrl:=Copy(lLink,1,Length(lLink)-lRemove);
      lNode:=Nodes.addTextNode(Scanner.location,nkURI,aProtocol+htmlEscape(lUrl));
      lNode.attrs.Add('href',aProtocol+urlEscape(lUrl));
      lText:=Copy(lLink,Length(lLink)-lRemove,lRemove);
      Nodes.addTextNode(Scanner.location,nkText,htmlEscape(lText));
      end;
  finally
    if lRewind then
      begin
      Scanner.GotoBookmark;
      Nodes.addText(Scanner.location,Scanner.NextChar);
      end;
  end;
end;

end.

