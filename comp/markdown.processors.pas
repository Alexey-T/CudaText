{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Basic Markdown block processors

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.Processors;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CodePages.unicodedata, System.Types,  System.SysUtils, 
  System.Classes, System.StrUtils, System.Contnrs,
{$ELSE}
  UnicodeData, Types,  SysUtils, Classes, StrUtils, Contnrs,
{$ENDIF}  
  Markdown.Elements,
  Markdown.Utils,
  Markdown.Line,
  Markdown.HtmlEntities,
  Markdown.Parser;

type
  { TMarkdownQuoteProcessor }

  TMarkdownQuoteProcessor = class (TMarkdownBlockProcessor)
  private
    FLevel : integer;
    function SkipQuotes(aLine: TMarkdownLine; aLevel: Integer): Integer;
  protected
    function IsQuotedLine(aLine: TMarkdownLine; SkipLevels: Boolean): boolean;
    function inListOrQuote : boolean; override;
    function IsRoot(aParent : TMarkdownContainerBlock) : Boolean;
  public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TMarkdownListProcessor }

  TMarkdownListProcessor = class (TMarkdownBlockProcessor)
  private
    FHasContent : boolean;
    FEmptyLine : integer;
    FLastList : TMarkdownListBlock;
    FLastItem : TMarkdownListItemBlock;
  protected
    function inListOrQuote : boolean; override;
    function Root : Boolean;
    function LastList : TMarkdownListBlock;
  public
    function prepareline(aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : boolean;
  end;

  { TUListProcessor }

  TUListProcessor = class (TMarkdownListProcessor)
  private
    function HasMarker(aLine: TMarkdownLine; out aIndent: Integer; out aMarker: String): boolean;
    function IsItemInList(aList: TMarkdownListBlock; aLine: TMarkdownLine): boolean;
  public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : boolean; override;
  end;

  { TOListProcessor }

  TOListProcessor = class (TMarkdownListProcessor)
  private
    FStart : Integer;
    function HasMarker(aLine: TMarkdownLine; out aIndent: integer; out aMarker: String; out aValue: Integer): boolean;
    function IsItemInList(aList: TMarkdownListBlock; aLine: TMarkdownLine): boolean;
  Public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : boolean; override;
  end;

  { TSeTextHeaderProcessor }

  TSeTextHeaderProcessor = class(TMarkdownBlockProcessor)
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : boolean; override;
  end;

  { TThematicBreakProcessor }

  TThematicBreakProcessor = class(TMarkdownBlockProcessor)
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;


  { TCodeBlockProcessor }

  TCodeBlockProcessor = class (TMarkdownBlockProcessor)
  private
    procedure ProcessCodeBlock(C: TMarkdownCodeBlock; aLine: TMarkdownLine);
  public
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TFencedCodeBlockProcessor }

  TFencedCodeBlockProcessor = class (TMarkdownBlockProcessor)
  Private
    FIndent : Integer;
    FLang : String;
    FTerminal: string;
  Public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TMarkdownHeadingProcessor }

  TMarkdownHeadingProcessor = class(TMarkdownBlockProcessor)
    FLen : Integer;
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TTableProcessor }

  TTableProcessor = class(TMarkdownBlockProcessor)
  private
    function CountCells(aLine: TMarkdownLine): Integer;
  protected
    procedure ParseTableLine(aTable: TMarkdownTableBlock; aLine: TMarkdownLine);
    function IsEndOfTable(aLine : TMarkdownLine) : boolean;
    // Check that number of cells in second line of table is the same as in first line
    function Strict : boolean; virtual;
  public
    class var
      DefaultStrict : Boolean;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TFrontmatterProcessor }

  TFrontmatterProcessor = class(TMarkdownBlockProcessor)
  private
    FFrontMatterType: TFrontMatterType;
    FTerminal: String;
  public
    class var lenient: Boolean;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TParagraphProcessor }

  TParagraphProcessor = class(TMarkdownBlockProcessor)
  public
    function processLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
  end;

implementation

{ ---------------------------------------------------------------------
  TMarkdownQuoteProcessor
  ---------------------------------------------------------------------}

function TMarkdownQuoteProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;

var
  len : integer;
  Inquote : Boolean;

begin
  Result:=(aLine=Nil);
  if Result then
    exit;
  Result:=(aLine.LineNo<>aBlock.line);
  if not Result then
    exit;
  // See if we have enough > markers
  Len:=SkipQuotes(aLine,FLevel);
  if Len=0 then
    Exit(False);
  inQuote:=(Len=FLevel);
  //  InQuote:=StartsWithWhiteSpace(aLine.Remainder,'>',len);
  // Not enough markers -> check continuation.
  // end of block if:
  // - empty line
  // - starts a new kind of block
  // - It is not plain text (laziness rule)
  if aLine.isWhitespace and not InQuote then // empty line
    Result:=True
  else if IsBlock(aBlock, aBlock.Blocks, aLine.Remainder) then // New kind of block
    Result:=True
  else if not TMarkdownParser.inPara(aBlock.blocks, false) then
    Result:=true
  else
    begin
    Result:=False;
    Parser.Lazy:=true;
    end;
end;

function TMarkdownQuoteProcessor.SkipQuotes(aLine : TMarkdownLine; aLevel : Integer) : integer;

var
  len,lLevel : integer;
begin
  // eat > markers up to current level
  aLine.Mark;
  lLevel:=aLevel;
  While (lLevel>0) and StartsWithWhitespace(aLine.Remainder, '>', len) do
    begin
    aLine.advance(len+1);
    if not aLine.isEmpty and isWhitespaceChar(aLine.Remainder[1]) then
      aLine.advance(1);
    Dec(lLevel);
    end;
  Result:=lLevel;
end;

function TMarkdownQuoteProcessor.IsQuotedLine(aLine: TMarkdownLine; SkipLevels : Boolean): boolean;

var
  len : integer;

begin
  if SkipLevels and (FLevel>0) then
    SkipQuotes(aLine,FLevel);
  Result:=StartsWithWhiteSpace(aLine.Remainder,'>',len);
  if not Result then
    exit;
  aLine.advance(len+1);
  if not aLine.isEmpty and isWhitespaceChar(aLine.Remainder[1]) then
    aLine.advance(1);
end;


function TMarkdownQuoteProcessor.EndsList: Boolean;
begin
  Result:=True;
end;

function TMarkdownQuoteProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

begin
  Result:=IsQuotedLine(aLine,False);
end;


function TMarkdownQuoteProcessor.inListOrQuote: boolean;
begin
  Result:=true;
end;

function TMarkdownQuoteProcessor.IsRoot(aParent : TMarkdownContainerBlock): Boolean;
var
  lParent : TMarkdownBlock;
begin
  Result:=aParent is TMarkdownDocument;
  if Result then
    exit;
  lParent:=aParent;
  While lParent<>Nil do
    begin
    Result:=Not (lParent is TMarkdownListBlock);
    if Result then
      Exit;
    lParent:=lParent.Parent;
    end;
end;

function TMarkdownQuoteProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  oldLevel : Integer;
  lBlock : TMarkdownQuoteBlock;

begin
  oldLevel:=Flevel;
  inc(FLevel);
  lBlock:=TMarkdownQuoteBlock.Create(aParent,aLine.LineNo);
  RedoLine(false);
  Parse(lBlock,Self);
  FLevel:=OldLevel;
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TMarkdownListProcessor
  ---------------------------------------------------------------------}

function TMarkdownListProcessor.inListOrQuote: boolean;
begin
  Result:=true;
end;

function TMarkdownListProcessor.Root: Boolean;
begin
  Result:=False;
end;

function TMarkdownListProcessor.LastList: TMarkdownListBlock;
begin
  Result:=FLastList;
end;


function TMarkdownListProcessor.prepareLine(aLine: TMarkdownLine; aContext : TMarkdownBlockProcessingContext): boolean;

var
  lWhiteSpace, lCurrLineNo,lLastIndent,len : integer;

begin
  Result:=False;
  lCurrLineNo:=aLine.LineNo;
  lLastIndent:=LastList.LastIndent;
  lWhiteSpace:=aLine.LeadingWhitespace;

  if lWhitespace >= lLastIndent then
    begin
    len:=lWhitespace;
    if (len>lLastIndent+1) then
      len:=LastList.baseIndent;
    aLine.advance(len);
    end
  else if (mdoGithubFlavoured in Parser.Options) and (lWhitespace>3) and (aLine.Remainder.Trim.StartsWith('-')) then
    Result:=False
  else if not aLine.isWhitespace and isBlock(LastList, FLastItem.blocks, aLine.Remainder, LastList.LastIndent+LastList.grace) then
    Result:=true;
  if not root then
    Exit;
  if not aLine.isWhiteSpace then
   begin
      FHasContent:=true;
      if root and not Result and LastList.HasSeenEmptyLine then
        LastList.loose:=true;
    end
    else if not Result  then
    begin
      Parser.Lazy:=true;
      if not FHasContent then
      begin
        if FEmptyLine = -1 then
          FEmptyLine:=lCurrLineNo
        else if FEmptyLine <> CurrentLine.LineNo then
          Result:=true;
      end;
      if (aContext = bpGeneral) and (lCurrLineNo<>FLastItem.Line) then
        LastList.HasSeenEmptyLine:=true;
    end;
end;


{ ---------------------------------------------------------------------
  TMarkdownHeadingProcessor
  ---------------------------------------------------------------------}

function TMarkdownHeadingProcessor.EndsList: Boolean;
begin
  Result:=True;
end;

function TMarkdownHeadingProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;
var
  ls : string;
  lLen,lCount : integer;
begin
  FLen:=0;
  if aLine.LeadingWhitespace >= 4 then
    Exit(false);
  ls:=Trim(aLine.Remainder);
  lLen:=Length(ls);
  lCount:=CountStartChars(ls,'#');
  Result:=(lCount>0) and ((lCount=lLen)) or ((lCount<lLen) and (ls[lCount+1] in [' ',#9]));
  if Result then
    FLen:=lCount;
end;


function TMarkdownHeadingProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownHeadingBlock;
  lLen : integer;
  s : String;

begin
  lBlock:=TMarkdownHeadingBlock.Create(aParent,aLine.LineNo,Flen);
  aLine.Advance(Flen);
  aLine.SkipWhiteSpace;
  s:=Trim(aLine.Remainder);
  if not isWhitespace(s) then
    begin
    lLen:=length(s);
    while (lLen>0) and (s[llen]='#') do
      Dec(lLen);
    if (lLen = 0) then
      s:=''
    else if (s[lLen]=' ') then
      s:=Copy(s,1,lLen-1);
    end;
  Parser.parseInline(lBlock,S);
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TUListProcessor
  ---------------------------------------------------------------------}

function TUListProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;
var
  lBlock : TMarkdownContainerBlock;
  lList : TMarkdownListBlock absolute lBlock;
  lProc : TMarkdownBlockProcessor;
begin
  lBlock:=aBlock;
  Result:=aBlock.line<>aLine.LineNo;
  if Not Result then
    Exit;
  if (lBlock is TMarkdownListItemBlock) and (lBlock.Parent is TMarkdownListBlock) then
     lBlock:=lBlock.Parent as TMarkdownListBlock;
  if (lBlock is TMarkdownListBlock) then
    if aLine.LeadingWhitespace>=lList.LastIndent then
      begin
//      aLine.Advance(lList.LastIndent);
      Result:=False;
      { A non-indented block that "interrupts" a list (CommonMark) ends it.
        Without this, e.g. a heading at column 0 following a top-level list
        would be parsed as a child of the last list item. }
      if aLine.LeadingWhitespace <= lList.BaseIndent then
        for lProc in Parser.Processors do
          if (lProc.ClassType <> ClassType)
             and lProc.EndsList
             and lProc.HandlesLine(aBlock, aLine) then
            begin
            Result:=True;
            Break;
            end;
      end;
end;

function TUListProcessor.HasMarker(aLine: TMarkdownLine; Out aIndent : Integer; out aMarker : String): boolean;
var
  lMarker : string;
  lIndent : Integer;

begin
  Result:=False;
  if aLine.isEmpty then
    Exit;
  lIndent:=aLine.LeadingWhitespace;
  lMarker:=CopySkipped(aLine.Remainder,[' ',#9]);
  if (lMarker='')  then
    Exit;
  if not CharInSet(lMarker[1],['+','-','*']) then
    Exit;
  if (lMarker[2]<>' ') then
    exit;
  aMarker:=lMarker[1];
  aIndent:=lIndent;
  Result:=true;
end;

function TUListProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lMarker : string;
  lIndent : integer;
begin
  Result:=HasMarker(aLine,lIndent,lMarker);
end;

function TUListProcessor.IsItemInList(aList : TMarkdownListBlock; aLine : TMarkdownLine) : boolean;

var
  len : integer;

begin
  Result:=Assigned(aLine);
  if Not Result then
    exit;
  Result:=StartsWithWhitespace(aLine.line,aList.marker[1],len,aList.baseIndent);
  if Result then
    Result := (len >= aList.baseIndent);
end;

function TUListProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): boolean;

var
  lOldLastList, lList : TMarkdownListBlock;
  lOldLastItem, lItem : TMarkdownListItemBlock;
  lRemain,lMarker : string;
  lIndent : Integer;
  lNewItem : Boolean;

begin
  Result:=False;
  lRemain:=aLine.Remainder;
  if not HasMarker(aLine,lIndent,lMarker) then exit;

  aLine.Advance(Pos(lMarker,aLine.Remainder)+1);
  lRemain:=aLine.Remainder;
  lOldLastList:=FLastList;
  lOldLastItem:=FLastItem;
  if InList(aParent,False,lMarker,lIndent,1,lList) then
    lList.Lastindent:=lIndent
  else
    begin
    lList:=TMarkdownListBlock.Create(aParent,aLine.LineNo);
    lList.Ordered:=false;
    lList.BaseIndent:=lIndent;
    lList.LastIndent:=lIndent;
    lList.Marker:=lMarker;
    FLastList:=lList;
    end;
  // While we have a list item part of this list block, add an item and parse it.
  repeat
    lItem:=TMarkdownListItemBlock.Create(lList,aLine.LineNo);
    FLastItem:=lItem;
    PrepareLine(aLine,bpGeneral);
    RedoLine(False);
    Parse(lItem,Self);
    lNewItem:=Not Done;
    if lNewItem then
      begin
      aLine:=PeekLine;
      // Check if next line is a list item at the SAME level
      lNewItem:=IsItemInList(lList,aLine);
      if lnewItem then
        aLine:=NextLine;
      end;
    lItem.Closed:=true;
  until not lNewItem;
  FLastItem:=lOldLastItem;
  FLastList:=lOldLastList;
  Result:=True;
end;


{ ---------------------------------------------------------------------
  TOListProcessor
  ---------------------------------------------------------------------}

function TOListProcessor.HasMarker(aLine: TMarkdownLine; out aIndent : integer; out aMarker : String; out aValue : Integer): boolean;
var
  lValueLen,lIndent : integer;
  lLine,lValue,lMarker : string;

begin
  Result:=False;
  lIndent:=aLine.LeadingWhitespace;
  lLine:=aLine.Remainder;
  lLine:=CopySkipped(lLine,[' ',#9]);
  lValue:=CopyMatching(lLine, ['0'..'9']);
  lValueLen:=length(lValue); // ending dot or )
  if (lValueLen=0) or (lValueLen>9) or (lValueLen>=Length(lLine)) then
    Exit;
  lMarker:=lLine[lValueLen+1];
  if not CharInSet(LMarker[1], ['.', ')']) then
    Exit;
  Delete(lLine,1,lValueLen+1);
  if isWhitespace(lLine) then
    Exit;
  aValue:=StrToIntDef(lValue,1);
  aIndent:=lIndent;
  aMarker:=lMarker;
  Result:=True;
end;

function TOListProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lIndent,lStart : integer;
  lMarker : String;
begin
  Result:=HasMarker(aLine,lIndent,lMarker,lStart);
end;

function TOListProcessor.IsItemInList(aList : TMarkdownListBlock; aLine : TMarkdownLine) : boolean;

var
  len : integer;
  lLine : string;

begin
  Result:=Assigned(aLine);
  if Not Result then
    exit;
  Result:=StartsWithWhitespace(aLine.Line,['0'..'9'],len,aList.baseIndent);
  if Not Result then
    exit;
  // Ensure indentation is exactly at the expected level for this list
  if len = aList.baseIndent then
    begin
    lLine:=aLine.Line;
    if Len>0 then
      Delete(lLine,1,Len);
    lLine:=CopySkipped(lLine,['0'..'9']);
    Result:=(lLine<>'') and (aList.marker=lLine[1]);
    if Result then
      Result:=(Length(lLine)>1) and (lLine[2]=' ');
    end
  else
    Result:=False;
end;

function TOListProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;
var
  lBlock : TMarkdownContainerBlock;
  lList : TMarkdownListBlock absolute lBlock;

begin
  lBlock:=aBlock;
  Result:=aBlock.line<>aLine.LineNo;
  if Not Result then
    Exit;
  if (lBlock is TMarkdownListItemBlock) and (lBlock.Parent is TMarkdownListBlock) then
     lBlock:=lBlock.Parent as TMarkdownListBlock;
  if not (lBlock is TMarkdownListBlock) then
    Exit;
  // Check if we're still in the parent list
  if aLine.LeadingWhitespace>=lList.baseIndent then
    begin
    aLine.Advance(lList.LastIndent);
    Result:=False
    end;
end;


function TOListProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): boolean;
var
  lOldLastList, lList : TMarkdownListBlock;
  lOldLastItem, lItem : TMarkdownListItemBlock;
  lNewItem : Boolean;
  lMarker : String;
  lStart,lIndent : Integer;
begin
  Result:=False;
  if not HasMarker(aLine,lIndent,lMarker,lStart) then
    exit;
  aLine.Advance(Pos(lMarker,aLine.Remainder)+1);
  lOldLastList:=FLastList;
  if inList(aParent, true, lMarker, lindent, 2, lList) then
    begin
    lList.lastIndent:=lIndent;
    FLastList:=lList;
    end
  else
    begin
    lList:=TMarkdownListBlock.Create(aParent,aLine.LineNo);
    lList.ordered:=true;
    lList.baseIndent:=lindent;
    lList.lastIndent:=lIndent;
    lList.marker:=lMarker;
    lList.Start:=FStart;
    FLastList:=lList;
    end;
  // While we have a line that part of this list block, add an item and parse it.
  repeat
    lItem:=TMarkdownListItemBlock.Create(lList,aLine.LineNo);
    lOldLastItem:=FLastItem;
    FLastItem:=lItem;
    PrepareLine(aLine,bpGeneral);
    RedoLine(False);
    Parse(lItem,Self);
    lNewItem:=Not Done;
    if lNewItem then
      begin
      aLine:=PeekLine;
      // Check if next line is a list item at the SAME level
      lNewItem:=IsItemInList(lList,aLine);
      if lNewItem then
        aLine:=NextLine;
      end;
  until not lNewItem;
  FLastItem:=lOldLastItem;
  FLastList:=lOldLastList;
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TCodeBlockProcessor
  ---------------------------------------------------------------------}

function TCodeBlockProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  Indent : integer;

begin
  Result:=False;
  Indent:=aLine.LeadingWhitespace;
  if Indent < 4 then
    Exit;
  if aLine.isWhitespace then
    Exit;
  if Parser.inPara(aParent.blocks,True) then
    Exit;
  Result:=true;
end;

procedure TCodeBlockProcessor.ProcessCodeBlock(C: TMarkdownCodeBlock; aLine: TMarkdownLine);

var
  S : String;
  lContinue : boolean;
  lIndent : Integer;
begin
  lIndent:=aLine.LeadingWhitespace;
  aLine.advance(lIndent);
  S:=aLine.Remainder;
  lContinue:=True;
  While lContinue do
    begin
    TMarkdownTextBlock.Create(C,aLine.LineNo,S);
    aLine:=peekLine;
    if (aLine = nil) then
      Exit;
    if not Handlesline(C,aLine) then
      Exit;
    S:=aLine.Remainder;
    if lengthWhitespaceCorrected(S)<=lIndent then
      lContinue:=isWhitespace(S)
    else
      lContinue:=LeadingWhitespace(S)>=lIndent;
    if lContinue then
      begin
      // Actually get the line
      aLine:=NextLine;
      aLine.advance(lIndent);
      S:=aLine.Remainder;
      end;
    end;
end;

function TCodeBlockProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownCodeBlock;

begin
  lBlock:=TMarkdownCodeBlock.Create(aParent,aLine.LineNo);
  ProcessCodeBlock(lBlock,aLine);
  // No whitespace at the end
  while (lBlock.ChildCount>0) and isWhitespace((lBlock.LastChild as TMarkdownTextBlock).text) do
    lBlock.DeleteChild(lBlock.ChildCount-1);
  Result:=true;
end;


function TFencedCodeBlockProcessor.EndsList: Boolean;
begin
  Result:=True;
end;

function TFencedCodeBlockProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  s, s1 : String;
  c : char;
  len, i, j : integer;
begin
  Result:=False;
  FLang:='';
  if aLine.LeadingWhitespace >= 4 then
    Exit;
  s:=aLine.Remainder.Trim;
  if (S='') then
    Exit;
  c:=S[1];
  if not (C in ['`','~']) then
    Exit;
  len:=CountStartChars(S,c);
  if (len<3) or (Pos(c,S,len+2)<>0) then
    Exit;
  FTerminal:=Copy(s,1,Len);
  Result:=true;
  // now, try to find anything off the end of the fence
  s:=aLine.Remainder;
  Findent:=1;
  while s[FIndent] = ' ' do
    inc(FIndent);
  if FIndent>1 then
    Delete(s,1,FIndent-1);
  S:=CopySkipped(S,[FTerminal[1]]).trim;
  i:=1;
  while (i<=Length(s)) do
    begin
    if S[i] = '\' then
      delete(s, i, 1) // and omit checking the next character
    else if s[i] = '`' then
      Exit(false)
    else if s[i] = '&' then
    begin
      j:=i+1;
      while (j <= Length(s)) and (s[j] <> ';') do
        inc(j);
      if j <= length(s) then
      begin
        s1:=parseEntityString(Parser.Entities,copy(s, i, j-i+1));
        delete(s, i, j-1);
        insert(s1, s, i);
        inc(i, Length(s1)-1);
      end;
    end;
    inc(i);
  end;

  if (s <> '') then
  begin
    Flang:=CopyUpTo(s, [' ']);
    if Flang.contains(FTerminal[1]) then
      Exit(false);
  end;
end;

function TFencedCodeBlockProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;

var
  s : String;
  lBlock: TMarkdownCodeBlock absolute aBlock;

begin
  Result:=(aLine=nil);
  if Result then
    Exit;
  // Ending may be preceded by 3 spaces

  Result:=aLine.LeadingWhitespace>=4+lBlock.Indent;
  if Result then
    Exit;
  S:=aLine.Remainder.Trim;
  Result:=IsStringOfChar(s) and s.StartsWith(FTerminal);
end;

function TFencedCodeBlockProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownCodeBlock;
  s : String;

begin
  lBlock:=TMarkdownCodeBlock.Create(aParent,aLine.LineNo);
  lBlock.fenced:=true;
  lBlock.lang:=Flang;
  lBlock.Indent:=aLine.CursorPos;
  while Not LineEndsBlock(lBlock,PeekLine) do
    begin
    aLine:=NextLine;
    if aLine.LeadingWhitespace>=lBlock.Indent then
      aLine.Advance(lBlock.Indent)
    else
      aLine.Advance(aLine.LeadingWhitespace);
    s:=aLine.Remainder;
    TMarkdownTextBlock.Create(lBlock,aLine.LineNo,S);
    end;
  NextLine;
  Result:=true;
end;

function TTableProcessor.CountCells(aLine: TMarkdownLine): Integer;

var
  c : char;
  lEscaped: boolean;
  S : String;

begin
  Result:=0;
  lEscaped:=false;
  for c in aLine.Line do
    begin
    if not lEscaped and (c = '|') then
      inc(Result)
    else if lEscaped then
      lEscaped:=false
    else if c = '\' then
      lEscaped:=true;
    end;
  if Result=0 then
    exit;
  S:=Trim(aLine.Line);
  if (S<>'') then
    begin
    if not StartsStr('|',S) then
      inc(Result);
    if not EndsStr('|',S) or EndsStr('\|',S) then
      inc(Result);
    end;
end;

function TTableProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lCount,lCountNext  : integer;
  lNextLine : TMarkdownLine;
  lLine : string;
  c : char;

begin
  Result:=False;
  if aLine.isEmpty then
    exit;
  lCount:=CountCells(aLine);
  if lCount=0 then
    Exit;
  lNextLine:=peekLine;
  if (lNextLine=nil) then
    Exit;
  lLine:=lNextLine.Line;
  for c in lLine do
    if not CharInSet(c,[' ','|','-',':']) then
      Exit(false);
  Result:=not Strict;
  lCountNext:=CountCells(lNextLine);
  // We have a table if the number of cells is equal
  Result:=(lCount=lCountNext)
end;

function TTableProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  i : integer;
  lLine,lCell : String;
  lTable : TMarkdownTableBlock;
  lCells : TStringDynArray;
  lColumns : TCellAlignArray;

begin
  LColumns:=[];
  lTable:=TMarkdownTableBlock.Create(aParent,aLine.LineNo);
  parseTableLine(lTable, aLine);
  lLine:=Trim(NextLine.Line);
  if StartsStr('|',lLine) then
    lLine:=Copy(lLine,2,Length(lLine)-1);
  if EndsStr('|',lLine) then
    SetLength(lLine,Length(lLine)-1);
  lCells:=lLine.Split(['|']);
  SetLength(LColumns, length(lCells));
  for i:=0 to length(lCells) - 1 do
    begin
    lColumns[i]:=caLeft;
    lCell:=Trim(lCells[i]);
    if StartsStr(':',lCell) and EndsStr(':',lCell) then
      lColumns[i]:=caCenter
    else if EndsStr(':',lCell) then
      lColumns[i]:=caRight
    end;
  lTable.Columns:=lColumns;
  while not isEndOfTable(peekLine) do
    ParseTableLine(lTable,NextLine);
  Result:=True;
end;

procedure TTableProcessor.ParseTableLine(aTable: TMarkdownTableBlock; aLine: TMarkdownLine);

var
  lRow : TMarkdownTableRowBlock;
  lLen,lStart, i : integer;
  lMaxCells, lCellCount : integer;
  lEscaped : boolean;
  lLine : string;
  lChar : char;

  procedure AddCell(aStart,aEnd : integer);

  var
    lCell : String;

  begin
    if (lMaxCells>0) and (lCellCount>=lMaxCells) then
      exit;
    lCell:=copy(aLine.Line, aStart, aEnd-aStart);
    lCell:=StringReplace(Trim(lCell),'\|', '|',[rfReplaceAll]);
    TMarkdownTextBlock.Create(lRow,aLine.LineNo,lCell);
    Inc(lCellCount);
  end;

begin
  lRow:=TMarkdownTableRowBlock.Create(aTable,aLine.LineNo);
  lMaxCells:=Length(aTable.Columns);
  lCellCount:=0;
  i:=1;
  lStart:=1;
  lEscaped:=false;
  lLine:=aLine.Line;
  lLen:=Length(lLine);
  while i<=lLen do
    begin
    lChar:=lLine[i];
    if (i=1) and (lChar='|') then
      lStart:=2
    else if lEscaped then
      lEscaped:=False
    else if lChar = '\' then
      lEscaped:=True
    else if lChar = '|' then
      begin
      AddCell(lStart,i);
      lStart:=i+1;
      end;
    Inc(i);
    end;
  if (lStart<i) then
    addCell(lStart,i);
end;

function TTableProcessor.IsEndOfTable(aLine: TMarkdownLine): boolean;

var
  S : String;
  lLen : integer;

begin
  if aLine=nil then
    Exit(true);
  S:=aLine.Line;
  lLen:=Length(S);
  Result:=(Trim(S) = '')
          or ((lLen>0) and (S[1] in ['#','~']))
          or StartsStr('   ',S);
end;

function TTableProcessor.Strict: boolean;
begin
  Result:=DefaultStrict;
end;

{ ---------------------------------------------------------------------
  TParagraphProcessor
  ---------------------------------------------------------------------}

function TParagraphProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;
begin
  Result:=True;
end;

function TParagraphProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): boolean;

var
  lPar : TMarkdownParagraphBlock;

begin
  Result:=true;
  if Parser.inPara(aParent.blocks, true) then
    lPar:=aParent.Blocks.Last as TMarkdownParagraphBlock
  else
    lPar:=nil;
  if aLine.isEmpty or aLine.isWhitespace then
    begin
    if Assigned(lPar) then
      lPar.closed:=true;
    end
  else
    begin
    if Not Assigned(lPar) then
      lPar:=TMarkdownParagraphBlock.Create(aParent,aLine.LineNo);
    Parser.parseInLine(lPar,aLine.Remainder);
    end;
end;

{ ---------------------------------------------------------------------
  TSeTextHeaderProcessor
  ---------------------------------------------------------------------}

function TSeTextHeaderProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  p : TMarkdownParagraphBlock;
  s : String;

begin
  Result:=False;
  if aParent.blocks.Count=0 then
    Exit;
  if aLine.LeadingWhitespace>=4 then
    Exit;
  if not TMarkdownParser.inPara(aParent.blocks,False) then
    Exit;
  p:=aParent.blocks.Last as TMarkdownParagraphBlock;
  if p.closed then
    Exit;
  if p.header<>0 then
    Exit;
  if Parser.Lazy and inListOrQuote then
    Exit;
  s:=aLine.Remainder.Trim;
  if (s='') then
    Exit;
  if not IsStringOfChar(s) then
    Exit;
  Result:=True;
end;


function TSeTextHeaderProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): boolean;

var
  S : String;
  h : integer;
  P : TMarkdownParagraphBlock;

begin
  s:=aLine.Remainder.Trim;
  h:=Pos(S[1],'=-');
  Result:=h>0;
  if Result then
    begin
    p:=aParent.blocks.Last as TMarkdownParagraphBlock;
    p.header:=h;
    end;
end;

{ ---------------------------------------------------------------------
  TThematicBreakProcessor
  ---------------------------------------------------------------------}

function TThematicBreakProcessor.EndsList: Boolean;
begin
  Result:=True;
end;

function TThematicBreakProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  S : String;
  C : char;

begin
  Result:=False;
  if aLine.LeadingWhitespace >= 4 then
    Exit;
  S:=StripWhitespace(aLine.Remainder);
  if (S='') then
    Exit;
  C:=S[1];
  Result:=(C in ['*','-','_']) and (CountStartChars(S,C)>2) and IsStringOfChar(S);
end;


function TThematicBreakProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

begin
  TMarkdownThematicBreakBlock.Create(aParent,aLine.LineNo);
  Result:=true;
end;

{ ---------------------------------------------------------------------
  TFrontmatterProcessor
  ---------------------------------------------------------------------}

function TFrontmatterProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  S : String;

begin
  Result := False;
  if aLine.LineNo <> 1 then
    Exit;
  S := aLine.Remainder;
  if Lenient then
    S:=TrimRight(S);
  if length(s)<>3 then
    exit;
  Result:=True;
  case S of
  '---':
    FFrontMatterType := fmtYAML;
  '+++':
    FFrontMatterType := fmtTOML;
  ';;;':
    FFrontMatterType := fmtJSON;
  else
    Result:=False;
  end;
  if Result then
    FTerminal:=S;
end;

function TFrontmatterProcessor.processLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownFrontmatterBlock;
  lLine : TMarkdownLine;

begin
  lBlock := TMarkdownFrontmatterBlock.Create(aParent, aLine.LineNo);
  lBlock.FrontMatterType := FFrontMatterType;
  while True do
    begin
    lLine := PeekLine;
    if lLine = nil then
      Break;
    if lLine.Remainder.Trim = FTerminal then
      begin
      NextLine;
      Break;
      end;
    lLine := NextLine;
    lBlock.Content.Add(lLine.Remainder);
    end;
  if aParent is TMarkdownDocument then
    TMarkdownDocument(aParent).Frontmatter := lBlock;
  Result := True;
end;

Procedure RegisterDefaultProcessors;

begin
  TFrontmatterProcessor.Register('frontmatter');
  TMarkdownQuoteProcessor.Register('quote');
  // Must be registered before thematic break
  TSeTextHeaderProcessor.register('setextheader');
  TThematicBreakProcessor.Register('break');
  TCodeBlockProcessor.register('code');
  TFencedCodeBlockProcessor.register('fencedcode');
  TMarkdownHeadingProcessor.register('heading');
  TUListProcessor.Register('unorderedlist');
  TOListProcessor.Register('orderedlist');
  TTableProcessor.Register('table');
  TParagraphProcessor.Register('paragraph');
end;

initialization
  TFrontmatterProcessor.Lenient:=False;
  TTableProcessor.DefaultStrict:=True;
  RegisterDefaultProcessors;
end.

