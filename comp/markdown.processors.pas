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
    function IsQuotedLine(aLine: TMarkdownLine; SkipLevels: Boolean; aWhiteSpaceLen : Integer = 3): boolean;
    function StripLinePrefix(aLine : TMarkdownLine) : Boolean; override;
    function InListOrQuote : boolean; override;
    function IsRoot(aParent : TMarkdownContainerBlock) : Boolean;
  public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TMarkdownListProcessor }

  TMarkdownListProcessor = class (TMarkdownBlockProcessor)
  private
    FHasContent : boolean;
    FEmptyLine : integer;
    FLastList : TMarkdownListBlock;
    FLastItem : TMarkdownListItemBlock;
    FOuter : TMarkdownBlockProcessor;
  protected
    function InListOrQuote : boolean; override;
    function Root : Boolean;
    function LastList : TMarkdownListBlock;
    // Set the processor enclosing the outermost list, returns the previous one.
    function EnterList : TMarkdownBlockProcessor;
    // Strip the prefix of the processor enclosing the outermost list from aLine.
    function StripOuter(aLine : TMarkdownLine) : Boolean;
    // Length of the prefix of the processor enclosing the outermost list in aLine.
    function OuterPrefixLength(aLine : TMarkdownLine) : Integer;
    // Does a line that is not inside the enclosing block end aBlock ?
    function OutsideLineEndsBlock(aBlock : TMarkdownContainerBlock; aLine : TMarkdownLine) : Boolean;
    // Is the next line an item of aList ? If so, it becomes the current line.
    function NextItemLine(aList : TMarkdownListBlock; out aLine : TMarkdownLine) : Boolean;
    function IsItemInList(aList : TMarkdownListBlock; aLine : TMarkdownLine) : Boolean; virtual; abstract;
    // Column at which the content of an item starts, for a line positioned just after the marker.
    class function ContentIndent(aLine : TMarkdownLine) : Integer;
  public
    function PrepareLine(aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : boolean;
  end;

  { TUListProcessor }

  TUListProcessor = class (TMarkdownListProcessor)
  private
    function HasMarker(aLine: TMarkdownLine; out aIndent: Integer; out aMarker: TMarkdownString): boolean;
  protected
    function IsItemInList(aList: TMarkdownListBlock; aLine: TMarkdownLine): boolean; override;
  public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : boolean; override;
  end;

  { TOListProcessor }

  TOListProcessor = class (TMarkdownListProcessor)
  private
    FStart : Integer;
    function HasMarker(aLine: TMarkdownLine; out aIndent: integer; out aMarker: TMarkdownString; out aValue: Integer): boolean;
  protected
    function IsItemInList(aList: TMarkdownListBlock; aLine: TMarkdownLine): boolean; override;
  Public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : boolean; override;
  end;

  { TSeTextHeaderProcessor }

  TSeTextHeaderProcessor = class(TMarkdownBlockProcessor)
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : boolean; override;
  end;

  { TThematicBreakProcessor }

  TThematicBreakProcessor = class(TMarkdownBlockProcessor)
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;


  { TCodeBlockProcessor }

  TCodeBlockProcessor = class (TMarkdownBlockProcessor)
  private
    procedure ProcessCodeBlock(C: TMarkdownCodeBlock; aLine: TMarkdownLine);
  public
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TFencedCodeBlockProcessor }

  TFencedCodeBlockProcessor = class (TMarkdownBlockProcessor)
  Private
    FIndent : Integer;
    FLang : TMarkdownString;
    FTerminal: TMarkdownString;
    FAttributeSpec : TMarkdownString;
  Public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TMarkdownHeadingProcessor }

  TMarkdownHeadingProcessor = class(TMarkdownBlockProcessor)
    FLen : Integer;
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
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
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TFrontmatterProcessor }

  TFrontmatterProcessor = class(TMarkdownBlockProcessor)
  private
    FFrontMatterType: TFrontMatterType;
    FTerminal: TMarkdownString;
  public
    class var lenient: Boolean;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TParagraphProcessor }

  TParagraphProcessor = class(TMarkdownBlockProcessor)
  public
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
  end;

  { TCommentBlockProcessor }

  TCommentBlockProcessor = class(TMarkdownBlockProcessor)
  public
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TIndentedContainerProcessor }

  // Base for blocks whose continuation lines are indented to a content column, as in list items.
  TIndentedContainerProcessor = class(TMarkdownBlockProcessor)
  private
    type
      TContainerContext = record
        Block : TMarkdownContainerBlock;
        Outer : TMarkdownBlockProcessor;
      end;
    var
      FContexts : array of TContainerContext;
      FContextCount : Integer;
      FEndedAfterBlank : Boolean;
    function ContextIndex(aBlock : TMarkdownBlock) : Integer;
    function StripOuter(aIndex : Integer; aLine : TMarkdownLine) : Boolean;
    function StripContext(aIndex : Integer; aLine : TMarkdownLine) : Boolean;
  protected
    // Did the last container end at a line that follows a blank line ?
    property EndedAfterBlank : Boolean read FEndedAfterBlank;
    // Parse the lines of aBlock, starting with the current line.
    procedure ParseContainer(aBlock : TMarkdownContainerBlock);
    function StripLinePrefix(aLine : TMarkdownLine) : Boolean; override;
    // Does aLine start a new block of this kind ? Such a line is never a lazy continuation.
    function IsContainerStart(aLine : TMarkdownLine) : Boolean; virtual; abstract;
  public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
  end;

  { TDefinitionListProcessor }

  TDefinitionListProcessor = class(TIndentedContainerProcessor)
  private
    function GetTermParagraph(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine) : TMarkdownParagraphBlock;
  protected
    // Is aLine a definition line ? aIndent receives the content column.
    class function IsDefinitionLine(aLine : TMarkdownLine; aMaxIndent : Integer; out aIndent : Integer) : Boolean;
    function IsContainerStart(aLine : TMarkdownLine) : Boolean; override;
  public
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TFootnoteDefinitionProcessor }

  TFootnoteDefinitionProcessor = class(TIndentedContainerProcessor)
  protected
    // Is aLine a footnote definition line ? aLabel receives the label, aLength the length of the [^label]: prefix.
    class function IsFootnoteLine(aLine : TMarkdownLine; aMaxIndent : Integer; out aLabel : TMarkdownString; out aLength : Integer) : Boolean;
    function IsContainerStart(aLine : TMarkdownLine) : Boolean; override;
  public
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TAdmonitionProcessor }

  // !!! type "title" admonitions, parsed as alert blocks whose content is indented 4 columns.
  TAdmonitionProcessor = class(TIndentedContainerProcessor)
  protected
    // Is aLine an admonition marker line ? aType receives the alert type, aTitle the title text.
    class function IsAdmonitionLine(aLine : TMarkdownLine; aMaxIndent : Integer; out aType : TAlertType; out aTitle : TMarkdownString) : Boolean;
    function IsContainerStart(aLine : TMarkdownLine) : Boolean; override;
  public
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TDirectiveProcessor }

  // Container directives :::name[label]{attributes} ... ::: and leaf directives ::name[label]{attributes}.
  TDirectiveProcessor = class(TMarkdownBlockProcessor)
  private
    type
      TDirectiveContext = record
        Block : TMarkdownContainerDirectiveBlock;
        Outer : TMarkdownBlockProcessor;
      end;
    var
      FContexts : array of TDirectiveContext;
      FContextCount : Integer;
      FColons : Integer;
      FName : TMarkdownString;
      FLabel : TMarkdownString;
      FAttributes : TMarkdownString;
      FHasLabel : Boolean;
    function ContextIndex(aBlock : TMarkdownBlock) : Integer;
    function StripOuter(aIndex : Integer; aLine : TMarkdownLine) : Boolean;
    function StripContext(aIndex : Integer; aLine : TMarkdownLine) : Boolean;
    procedure ParseContainer(aBlock : TMarkdownContainerDirectiveBlock);
  protected
    // Is aLine a fence of colons that closes aBlock ? Does not move the cursor.
    function IsClosingFence(aBlock : TMarkdownContainerDirectiveBlock; aLine : TMarkdownLine) : Boolean;
    function StripLinePrefix(aLine : TMarkdownLine) : Boolean; override;
  public
    function LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; override;
    function EndsList: Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

  { TLinkReferenceProcessor }

  TLinkReferenceProcessor = class(TMarkdownBlockProcessor)
  protected
    // Parse a link reference definition [label]: destination "title".
    class function ParseDefinition(const aText : TMarkdownString; out aLabel, aURL, aTitle : TMarkdownString) : Boolean;
  public
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
  end;

implementation

uses
  Markdown.Transforms;

// Indentation at which a line is indented code instead of the start of a new block.
// Inside an indented container this is 4 columns past the indentation of its content.
function MaxBlockIndent(aParent : TMarkdownBlock) : Integer;

var
  lBlock : TMarkdownBlock;
  lIndent : Integer;

begin
  Result:=4;
  lBlock:=aParent;
  While Assigned(lBlock) do
    begin
    lIndent:=lBlock.ContentIndentation;
    if lIndent>0 then
      Exit(Result+lIndent);
    lBlock:=lBlock.Parent;
    end;
end;


{ ---------------------------------------------------------------------
  TMarkdownQuoteProcessor
  ---------------------------------------------------------------------}

function TMarkdownQuoteProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;

var
  len : integer;

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
  // Not enough markers -> check continuation.
  // end of block if:
  // - empty line
  // - starts a new kind of block
  // - It is not plain text (laziness rule)
  if aLine.isWhitespace then // empty line
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
  if Result then
    aLine.Rewind;
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

function TMarkdownQuoteProcessor.IsQuotedLine(aLine: TMarkdownLine; SkipLevels : Boolean; aWhiteSpaceLen : Integer = 3): boolean;

var
  len : integer;

begin
  if SkipLevels and (FLevel>0) then
    SkipQuotes(aLine,FLevel);
  Result:=StartsWithWhiteSpace(aLine.Remainder,'>',len,aWhiteSpaceLen);
  if not Result then
    exit;
  aLine.advance(len+1);
  if not aLine.isEmpty and isWhitespaceChar(aLine.Remainder[1]) then
    aLine.advance(1);
end;


function TMarkdownQuoteProcessor.StripLinePrefix(aLine: TMarkdownLine): Boolean;

begin
  Result:=SkipQuotes(aLine,FLevel)=0;
end;


function TMarkdownQuoteProcessor.EndsList: Boolean;
begin
  Result:=True;
end;

function TMarkdownQuoteProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lPos : Integer;

begin
  lPos:=aLine.CursorPos;
  // An enclosing block may indent the marker past the three columns allowed at top level.
  Result:=IsQuotedLine(aLine,False,MaxBlockIndent(aParent)-1);
  aLine.CursorPos:=lPos;
end;


function TMarkdownQuoteProcessor.InListOrQuote: boolean;
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

function TMarkdownQuoteProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  oldLevel : Integer;
  lBlock : TMarkdownQuoteBlock;

begin
  IsQuotedLine(aLine,False,MaxBlockIndent(aParent)-1);
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

function TMarkdownListProcessor.InListOrQuote: boolean;
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


function TMarkdownListProcessor.EnterList: TMarkdownBlockProcessor;

begin
  Result:=FOuter;
  if ParentProcessor is TMarkdownListProcessor then
    FOuter:=TMarkdownListProcessor(ParentProcessor).FOuter
  else
    FOuter:=ParentProcessor;
end;


function TMarkdownListProcessor.StripOuter(aLine: TMarkdownLine): Boolean;

begin
  Result:=True;
  if Assigned(FOuter) then
    Result:=FOuter.StripLinePrefix(aLine);
end;


function TMarkdownListProcessor.OuterPrefixLength(aLine: TMarkdownLine): Integer;

var
  lPos : Integer;

begin
  lPos:=aLine.CursorPos;
  aLine.Reset;
  StripOuter(aLine);
  Result:=aLine.CursorPos-1;
  aLine.CursorPos:=lPos;
end;


function TMarkdownListProcessor.OutsideLineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;

begin
  Result:=aLine.isWhitespace
          or (Assigned(CurrentLine) and CurrentLine.isWhitespace)
          or not TMarkdownParser.InPara(aBlock.Blocks,False)
          or IsBlock(aBlock,aBlock.Blocks,aLine.Remainder);
  if not Result then
    Parser.Lazy:=True;
end;


function TMarkdownListProcessor.NextItemLine(aList: TMarkdownListBlock; out aLine: TMarkdownLine): Boolean;

var
  lPos : Integer;

begin
  aLine:=PeekLine;
  Result:=Assigned(aLine);
  if not Result then
    Exit;
  lPos:=aLine.CursorPos;
  Result:=StripOuter(aLine) and IsItemInList(aList,aLine);
  if Result then
    aLine:=NextLine
  else
    aLine.CursorPos:=lPos;
end;


class function TMarkdownListProcessor.ContentIndent(aLine: TMarkdownLine): Integer;

var
  lWhiteSpace : Integer;

begin
  // The cursor is 1 based and sits just after the marker and one space.
  Result:=aLine.CursorPos-1;
  if aLine.isWhitespace then
    Exit;
  lWhiteSpace:=aLine.LeadingWhitespace;
  // Five or more spaces after the marker start an indented code block in the item.
  if lWhiteSpace<4 then
    Inc(Result,lWhiteSpace);
end;


function TMarkdownListProcessor.PrepareLine(aLine: TMarkdownLine; aContext : TMarkdownBlockProcessingContext): boolean;

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
  ls : TMarkdownString;
  lLen,lCount : integer;
begin
  FLen:=0;
  if aLine.LeadingWhitespace >= MaxBlockIndent(aParent) then
    Exit(false);
  ls:=Trim(aLine.Remainder);
  lLen:=Length(ls);
  lCount:=CountStartChars(ls,'#');
  Result:=(lCount>0) and ((lCount=lLen)) or ((lCount<lLen) and (ls[lCount+1] in [' ',#9]));
  if Result then
    FLen:=lCount;
end;


function TMarkdownHeadingProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownHeadingBlock;
  lLen : integer;
  s : TMarkdownString;

begin
  lBlock:=TMarkdownHeadingBlock.Create(aParent,aLine.LineNo,Flen);
  // The markers start after the indentation, which may come from an enclosing block.
  aLine.Advance(aLine.LeadingWhitespace+Flen);
  aLine.SkipWhiteSpace;
  s:=Trim(aLine.Remainder);
  if mdoAttributes in Parser.Options then
    begin
    lBlock.ApplyAttributes(ExtractTrailingAttributeSpec(s));
    if lBlock.ID<>'' then
      Parser.RegisterAnchor(lBlock.ID,lBlock,aLine.LineNo);
    end;
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
  lStart : Integer;
begin
  lBlock:=aBlock;
  Result:=aBlock.line<>aLine.LineNo;
  if Not Result then
    Exit;
  lStart:=aLine.CursorPos;
  if not StripOuter(aLine) then
    begin
    Result:=OutsideLineEndsBlock(aBlock,aLine);
    if Result then
      aLine.CursorPos:=lStart;
    Exit;
    end;
  if (lBlock is TMarkdownListItemBlock) and (lBlock.Parent is TMarkdownListBlock) then
     lBlock:=lBlock.Parent as TMarkdownListBlock;
  if (lBlock is TMarkdownListBlock) then
    if aLine.LeadingWhitespace>=lList.LastIndent then
      begin
//      aLine.Advance(lList.LastIndent);
      Result:=False;
      if aLine.LeadingWhitespace <= lList.BaseIndent then
        begin
        { A non-indented block that "interrupts" a list (CommonMark) ends it.
          Without this, e.g. a heading at column 0 following a top-level list
          would be parsed as a child of the last list item. }
        for lProc in Parser.Processors do
          if (lProc.ClassType <> ClassType)
             and lProc.EndsList
             and lProc.HandlesLine(aBlock, aLine) then
            begin
            Result:=True;
            Break;
            end;
        { A non-indented, non-blank line after a blank line that is not a marker
          of this list is a new top-level block, not a lazy continuation. }
        if (not Result) and (not aLine.isWhitespace)
           and Assigned(CurrentLine) and CurrentLine.isWhitespace
           and (not IsItemInList(lList,aLine)) then
          Result:=True;
        end;
      end;
  if Result then
    aLine.CursorPos:=lStart;
end;

function TUListProcessor.HasMarker(aLine: TMarkdownLine; Out aIndent : Integer; out aMarker : TMarkdownString): boolean;
var
  lMarker : TMarkdownString;
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
  if (Length(lMarker)<2) or not CharInSet(lMarker[2],[' ',#9]) then
    exit;
  aMarker:=lMarker[1];
  aIndent:=lIndent;
  Result:=true;
end;

function TUListProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lMarker : TMarkdownString;
  lIndent : integer;
begin
  Result:=HasMarker(aLine,lIndent,lMarker);
end;

function TUListProcessor.IsItemInList(aList : TMarkdownListBlock; aLine : TMarkdownLine) : boolean;

var
  len : integer;
  lLine : TMarkdownString;

begin
  Result:=Assigned(aLine);
  if Not Result then
    exit;
  lLine:=aLine.Remainder;
  Result:=StartsWithWhitespace(lLine,aList.marker[1],len,aList.baseIndent);
  if Result then
    Result := (len >= aList.baseIndent);
  if not Result then
    exit;
  // The marker is followed by whitespace, and the line is not a thematic break
  Result:=(Length(lLine)=len+1) or (lLine[len+2] in [' ',#9]);
  if Result then
    begin
    lLine:=StripWhitespace(lLine);
    Result:=not ((CountStartChars(lLine,lLine[1])>2) and IsStringOfChar(lLine));
    end;
end;

function TUListProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): boolean;

var
  lOldLastList, lList : TMarkdownListBlock;
  lOldLastItem, lItem : TMarkdownListItemBlock;
  lOldOuter : TMarkdownBlockProcessor;
  lRemain,lMarker : TMarkdownString;
  lIndent,lContentIndent : Integer;
  lNewItem : Boolean;

begin
  Result:=False;
  lRemain:=aLine.Remainder;
  if not HasMarker(aLine,lIndent,lMarker) then exit;
  lOldOuter:=EnterList;
  aLine.Advance(Pos(lMarker,aLine.Remainder)+1);
  lContentIndent:=ContentIndent(aLine)-OuterPrefixLength(aLine);
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
  lList.ContentIndent:=lContentIndent;
  // While we have a list item part of this list block, add an item and parse it.
  repeat
    lItem:=TMarkdownListItemBlock.Create(lList,aLine.LineNo);
    FLastItem:=lItem;
    PrepareLine(aLine,bpGeneral);
    RedoLine(False);
    Parse(lItem,Self);
    // Check if next line is a list item at the SAME level
    lNewItem:=(Not Done) and NextItemLine(lList,aLine);
    lItem.Closed:=true;
  until not lNewItem;
  FLastItem:=lOldLastItem;
  FLastList:=lOldLastList;
  FOuter:=lOldOuter;
  Result:=True;
end;


{ ---------------------------------------------------------------------
  TOListProcessor
  ---------------------------------------------------------------------}

function TOListProcessor.HasMarker(aLine: TMarkdownLine; out aIndent : integer; out aMarker : TMarkdownString; out aValue : Integer): boolean;
var
  lValueLen,lIndent : integer;
  lLine,lValue,lMarker : TMarkdownString;

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
  if (lLine='') or not CharInSet(lLine[1],[' ',#9]) then
    Exit;
  if isWhitespace(lLine) then
    Exit;
  aValue:=MarkdownToIntDef(lValue,1);
  aIndent:=lIndent;
  aMarker:=lMarker;
  Result:=True;
end;

function TOListProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lIndent,lStart : integer;
  lMarker : TMarkdownString;
begin
  Result:=HasMarker(aLine,lIndent,lMarker,lStart);
end;

function TOListProcessor.IsItemInList(aList : TMarkdownListBlock; aLine : TMarkdownLine) : boolean;

var
  len : integer;
  lLine : TMarkdownString;

begin
  Result:=Assigned(aLine);
  if Not Result then
    exit;
  Result:=StartsWithWhitespace(aLine.Remainder,['0'..'9'],len,aList.baseIndent);
  if Not Result then
    exit;
  // Ensure indentation is exactly at the expected level for this list
  if len = aList.baseIndent then
    begin
    lLine:=aLine.Remainder;
    if Len>0 then
      Delete(lLine,1,Len);
    lLine:=CopySkipped(lLine,['0'..'9']);
    Result:=(lLine<>'') and (aList.marker=lLine[1]);
    if Result then
      Result:=(Length(lLine)>1) and CharInSet(lLine[2],[' ',#9]);
    end
  else
    Result:=False;
end;

function TOListProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;
var
  lBlock : TMarkdownContainerBlock;
  lList : TMarkdownListBlock absolute lBlock;
  lProc : TMarkdownBlockProcessor;
  lStart : Integer;

begin
  lBlock:=aBlock;
  Result:=aBlock.line<>aLine.LineNo;
  if Not Result then
    Exit;
  lStart:=aLine.CursorPos;
  if not StripOuter(aLine) then
    begin
    Result:=OutsideLineEndsBlock(aBlock,aLine);
    if Result then
      aLine.CursorPos:=lStart;
    Exit;
    end;
  if (lBlock is TMarkdownListItemBlock) and (lBlock.Parent is TMarkdownListBlock) then
     lBlock:=lBlock.Parent as TMarkdownListBlock;
  if not (lBlock is TMarkdownListBlock) then
    begin
    aLine.CursorPos:=lStart;
    Exit;
    end;
  // Check if we're still in the parent list
  if aLine.LeadingWhitespace>=lList.baseIndent then
    begin
    Result:=False;
    if aLine.LeadingWhitespace <= lList.BaseIndent then
      begin
      { A non-indented block that "interrupts" a list (CommonMark) ends it.
        Without this, e.g. a heading at column 0 following a top-level ordered
        list would be parsed as a child of the last list item (the same fix as
        in TUListProcessor.LineEndsBlock). }
      for lProc in Parser.Processors do
        if (lProc.ClassType <> ClassType)
           and lProc.EndsList
           and lProc.HandlesLine(aBlock, aLine) then
          begin
          Result:=True;
          Break;
          end;
      { A non-indented, non-blank line after a blank line that is not a marker
        of this list is a new top-level block, not a lazy continuation. }
      if (not Result) and (not aLine.isWhitespace)
         and Assigned(CurrentLine) and CurrentLine.isWhitespace
         and (not IsItemInList(lList,aLine)) then
        Result:=True;
      end;
    if not Result then
      aLine.Advance(lList.LastIndent);
    end;
  if Result then
    aLine.CursorPos:=lStart;
end;


function TOListProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): boolean;
var
  lOldLastList, lList : TMarkdownListBlock;
  lOldLastItem, lItem : TMarkdownListItemBlock;
  lOldOuter : TMarkdownBlockProcessor;
  lNewItem : Boolean;
  lMarker : TMarkdownString;
  lStart,lIndent,lContentIndent : Integer;
begin
  Result:=False;
  if not HasMarker(aLine,lIndent,lMarker,lStart) then
    exit;
  lOldOuter:=EnterList;
  aLine.Advance(Pos(lMarker,aLine.Remainder)+1);
  lContentIndent:=ContentIndent(aLine)-OuterPrefixLength(aLine);
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
    lList.Start:=lStart;
    FLastList:=lList;
    end;
  // Continuation lines of an ordered item arrive with LastIndent already removed.
  lList.ContentIndent:=lContentIndent-lIndent;
  // While we have a line that part of this list block, add an item and parse it.
  repeat
    lItem:=TMarkdownListItemBlock.Create(lList,aLine.LineNo);
    lOldLastItem:=FLastItem;
    FLastItem:=lItem;
    PrepareLine(aLine,bpGeneral);
    RedoLine(False);
    Parse(lItem,Self);
    // Check if next line is a list item at the SAME level
    lNewItem:=(Not Done) and NextItemLine(lList,aLine);
  until not lNewItem;
  FLastItem:=lOldLastItem;
  FLastList:=lOldLastList;
  FOuter:=lOldOuter;
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
  if Indent < MaxBlockIndent(aParent) then
    Exit;
  if aLine.isWhitespace then
    Exit;
  if Parser.inPara(aParent.blocks,True) then
    Exit;
  Result:=true;
end;

procedure TCodeBlockProcessor.ProcessCodeBlock(C: TMarkdownCodeBlock; aLine: TMarkdownLine);

var
  S : TMarkdownString;
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
    // The enclosing block takes its prefix off before the line is examined.
    if ParentEndsLine(aLine) then
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

function TCodeBlockProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

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
  s, s1 : TMarkdownString;
  c : TMarkdownChar;
  len, i, j : integer;
begin
  Result:=False;
  FLang:='';
  FAttributeSpec:='';
  if aLine.LeadingWhitespace >= MaxBlockIndent(aParent) then
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
  if mdoAttributes in Parser.Options then
    FAttributeSpec:=ExtractTrailingAttributeSpec(S);
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
  s : TMarkdownString;
  lBlock: TMarkdownCodeBlock absolute aBlock;

begin
  Result:=(aLine=nil);
  if Result then
    Exit;
  // Ending may be preceded by 3 spaces

  if aLine.LeadingWhitespace>=4+lBlock.Indent then
    Exit(False);
  S:=aLine.Remainder.Trim;
  Result:=IsStringOfChar(s) and s.StartsWith(FTerminal);
end;

function TFencedCodeBlockProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownCodeBlock;
  lLine : TMarkdownLine;
  lClosed : Boolean;
  s : TMarkdownString;

begin
  lBlock:=TMarkdownCodeBlock.Create(aParent,aLine.LineNo);
  lBlock.fenced:=true;
  lBlock.lang:=Flang;
  if lBlock.ApplyAttributes(FAttributeSpec) and (lBlock.ID<>'') then
    Parser.RegisterAnchor(lBlock.ID,lBlock,aLine.LineNo);
  // Prefixes already consumed, plus the indentation an enclosing block gives the fence.
  lBlock.Indent:=aLine.CursorPos-1+aLine.LeadingWhitespace;
  lClosed:=False;
  Repeat
    lLine:=PeekLine;
    if lLine=Nil then
      Break;
    // The enclosing block takes its prefix off before the line is examined.
    if ParentEndsLine(lLine) then
      Break;
    lClosed:=LineEndsBlock(lBlock,lLine);
    if lClosed then
      Break;
    aLine:=NextLine;
    if aLine.LeadingWhitespace>=lBlock.Indent then
      aLine.Advance(lBlock.Indent)
    else
      aLine.Advance(aLine.LeadingWhitespace);
    s:=aLine.Remainder;
    TMarkdownTextBlock.Create(lBlock,aLine.LineNo,S);
  Until False;
  // Consume the closing fence, but leave a line belonging to the enclosing block.
  if lClosed then
    NextLine;
  Result:=true;
end;

function TTableProcessor.CountCells(aLine: TMarkdownLine): Integer;

var
  c : TMarkdownChar;
  lEscaped: boolean;
  S : TMarkdownString;

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
    if not S.StartsWith('|') then
      inc(Result);
    if not S.EndsWith('|') or S.EndsWith('\|') then
      inc(Result);
    end;
end;

function TTableProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lCount,lCountNext  : integer;
  lNextLine : TMarkdownLine;
  lLine : TMarkdownString;
  c : TMarkdownChar;

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

function TTableProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

var
  i : integer;
  lLine,lCell : TMarkdownString;
  lTable : TMarkdownTableBlock;
  lCells : TMarkdownStringArray;
  lColumns : TCellAlignArray;

begin
  LColumns:=[];
  lTable:=TMarkdownTableBlock.Create(aParent,aLine.LineNo);
  parseTableLine(lTable, aLine);
  lLine:=Trim(NextLine.Line);
  if lLine.StartsWith('|') then
    lLine:=Copy(lLine,2,Length(lLine)-1);
  if lLine.EndsWith('|') then
    SetLength(lLine,Length(lLine)-1);
  lCells:=lLine.Split(['|']);
  SetLength(LColumns, length(lCells));
  for i:=0 to length(lCells) - 1 do
    begin
    lColumns[i]:=caLeft;
    lCell:=Trim(lCells[i]);
    if lCell.StartsWith(':') and lCell.EndsWith(':') then
      lColumns[i]:=caCenter
    else if lCell.EndsWith(':') then
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
  lLine : TMarkdownString;
  lChar : TMarkdownChar;

  procedure AddCell(aStart,aEnd : integer);

  var
    lCell : TMarkdownString;

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
  S : TMarkdownString;
  lLen : integer;

begin
  if aLine=nil then
    Exit(true);
  S:=aLine.Line;
  lLen:=Length(S);
  Result:=(Trim(S) = '')
          or ((lLen>0) and (S[1] in ['#','~']))
          or S.StartsWith('   ')
          or ((mdoDirectives in Parser.Options) and (CountStartChars(TrimLeft(S),':')>=3));
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

function TParagraphProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): boolean;

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
      begin
      // A second paragraph in an item follows a blank line, which makes the list loose.
      if (aParent is TMarkdownListItemBlock) and (aParent.Blocks.Count>0)
         and (aParent.Parent is TMarkdownListBlock) then
        TMarkdownListBlock(aParent.Parent).Loose:=True;
      lPar:=TMarkdownParagraphBlock.Create(aParent,aLine.LineNo);
      end;
    Parser.parseInLine(lPar,aLine.Remainder);
    end;
end;

{ ---------------------------------------------------------------------
  TSeTextHeaderProcessor
  ---------------------------------------------------------------------}

function TSeTextHeaderProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  p : TMarkdownParagraphBlock;
  s : TMarkdownString;

begin
  Result:=False;
  if aParent.blocks.Count=0 then
    Exit;
  if aLine.LeadingWhitespace>=MaxBlockIndent(aParent) then
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


function TSeTextHeaderProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): boolean;

var
  S,lText : TMarkdownString;
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
    if (mdoAttributes in Parser.Options) and (p.LastChild is TMarkdownTextBlock) then
      begin
      lText:=TMarkdownTextBlock(p.LastChild).Text;
      if p.ApplyAttributes(ExtractTrailingAttributeSpec(lText)) then
        begin
        TMarkdownTextBlock(p.LastChild).Text:=lText;
        if p.ID<>'' then
          Parser.RegisterAnchor(p.ID,p,p.Line);
        end;
      end;
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
  S : TMarkdownString;
  C : TMarkdownChar;

begin
  Result:=False;
  if aLine.LeadingWhitespace >= MaxBlockIndent(aParent) then
    Exit;
  S:=StripWhitespace(aLine.Remainder);
  if (S='') then
    Exit;
  C:=S[1];
  Result:=(C in ['*','-','_']) and (CountStartChars(S,C)>2) and IsStringOfChar(S);
end;


function TThematicBreakProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

begin
  TMarkdownThematicBreakBlock.Create(aParent,aLine.LineNo);
  Result:=true;
end;

{ ---------------------------------------------------------------------
  TFrontmatterProcessor
  ---------------------------------------------------------------------}

function TFrontmatterProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  S : TMarkdownString;

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

function TFrontmatterProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine; aContext: TMarkdownBlockProcessingContext): Boolean;

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
    lBlock.Content.Add(MarkdownToString(lLine.Remainder));
    end;
  if aParent is TMarkdownDocument then
    TMarkdownDocument(aParent).Frontmatter := lBlock;
  Result := True;
end;

{ ---------------------------------------------------------------------
  TCommentBlockProcessor
  ---------------------------------------------------------------------}

function TCommentBlockProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

begin
  Result:=False;
  if not (mdoComments in Parser.Options) then
    Exit;
  if aLine.LeadingWhitespace>=MaxBlockIndent(aParent) then
    Exit;
  if TMarkdownParser.InPara(aParent.Blocks,False) then
    Exit;
  Result:=TrimLeft(aLine.Remainder).StartsWith('<!--');
end;


function TCommentBlockProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine;
  aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownCommentBlock;
  lLine : TMarkdownLine;
  S,lText : TMarkdownString;
  P : Integer;

begin
  lBlock:=TMarkdownCommentBlock.Create(aParent,aLine.LineNo);
  S:=TrimLeft(aLine.Remainder);
  // <!--> and <!---> are complete comments
  P:=Pos('-->',S,3);
  if P>0 then
    lText:=Copy(S,5,P-5)
  else
    begin
    lText:=Copy(S,5,Length(S)-4);
    repeat
      lLine:=PeekLine;
      if (lLine=Nil) or ParentEndsLine(lLine) then
        Break;
      NextLine;
      S:=lLine.Remainder;
      P:=Pos('-->',S);
      if P>0 then
        lText:=lText+#10+Copy(S,1,P-1)
      else
        lText:=lText+#10+S;
    until P>0;
    end;
  lBlock.Text:=lText;
  lBlock.Closed:=True;
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TIndentedContainerProcessor
  ---------------------------------------------------------------------}

function TIndentedContainerProcessor.ContextIndex(aBlock: TMarkdownBlock): Integer;

begin
  Result:=FContextCount-1;
  while (Result>=0) and (FContexts[Result].Block<>aBlock) do
    Dec(Result);
end;


function TIndentedContainerProcessor.StripOuter(aIndex: Integer; aLine: TMarkdownLine): Boolean;

var
  lOuter : TMarkdownBlockProcessor;

begin
  lOuter:=FContexts[aIndex].Outer;
  if lOuter=Self then
    Result:=(aIndex>0) and StripContext(aIndex-1,aLine)
  else if Assigned(lOuter) then
    Result:=lOuter.StripLinePrefix(aLine)
  else
    Result:=True;
end;


function TIndentedContainerProcessor.StripContext(aIndex: Integer; aLine: TMarkdownLine): Boolean;

begin
  Result:=StripOuter(aIndex,aLine);
  if Result and not aLine.isWhitespace then
    Result:=aLine.LeadingWhitespace>=FContexts[aIndex].Block.ContentIndentation;
end;


procedure TIndentedContainerProcessor.ParseContainer(aBlock: TMarkdownContainerBlock);

begin
  if FContextCount=Length(FContexts) then
    SetLength(FContexts,FContextCount+4);
  FContexts[FContextCount].Block:=aBlock;
  FContexts[FContextCount].Outer:=ParentProcessor;
  Inc(FContextCount);
  try
    RedoLine(False);
    Parse(aBlock,Self);
  finally
    Dec(FContextCount);
  end;
end;


function TIndentedContainerProcessor.StripLinePrefix(aLine: TMarkdownLine): Boolean;

begin
  Result:=(FContextCount>0) and StripContext(FContextCount-1,aLine);
end;


function TIndentedContainerProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;

var
  lIdx,lStart : Integer;
  lPrevious : TMarkdownLine;

begin
  Result:=(aLine=Nil);
  if Result or (aLine.LineNo=aBlock.Line) then
    Exit;
  lIdx:=ContextIndex(aBlock);
  if lIdx<0 then
    Exit(True);
  lStart:=aLine.CursorPos;
  Result:=not StripOuter(lIdx,aLine);
  if not Result and not aLine.isWhitespace and (aLine.LeadingWhitespace<aBlock.ContentIndentation) then
    begin
    // A lazy continuation line of a paragraph, unless a blank line came before or a new block starts
    lPrevious:=CurrentLine;
    Result:=(Assigned(lPrevious) and lPrevious.isWhitespace)
            or IsContainerStart(aLine)
            or not TMarkdownParser.InPara(aBlock.Blocks,False)
            or IsBlock(aBlock,aBlock.Blocks,aLine.Remainder);
    end;
  if Result then
    begin
    lPrevious:=CurrentLine;
    FEndedAfterBlank:=Assigned(lPrevious) and lPrevious.isWhitespace;
    aLine.CursorPos:=lStart;
    end;
end;

{ ---------------------------------------------------------------------
  TDefinitionListProcessor
  ---------------------------------------------------------------------}

class function TDefinitionListProcessor.IsDefinitionLine(aLine: TMarkdownLine; aMaxIndent: Integer; out aIndent: Integer): Boolean;

var
  S : TMarkdownString;
  lWhite,lSpaces : Integer;

begin
  Result:=False;
  aIndent:=0;
  lWhite:=aLine.LeadingWhitespace;
  if lWhite>=aMaxIndent then
    Exit;
  S:=aLine.Remainder;
  Delete(S,1,lWhite);
  if (Length(S)<3) or (S[1]<>':') or (S[2]<>' ') then
    Exit;
  Delete(S,1,1);
  lSpaces:=LeadingWhitespace(S);
  if (lSpaces>4) or IsWhitespace(S) then
    Exit;
  aIndent:=lWhite+1+lSpaces;
  Result:=True;
end;


function TDefinitionListProcessor.IsContainerStart(aLine: TMarkdownLine): Boolean;

var
  lIndent : Integer;

begin
  Result:=IsDefinitionLine(aLine,4,lIndent);
end;


function TDefinitionListProcessor.GetTermParagraph(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): TMarkdownParagraphBlock;

var
  lLast : TMarkdownBlock;

begin
  Result:=Nil;
  lLast:=aParent.LastChild;
  if not (Assigned(lLast) and (lLast.ClassType=TMarkdownParagraphBlock)) then
    Exit;
  if TMarkdownParagraphBlock(lLast).Header<>0 then
    Exit;
  if not (lLast.LastChild is TMarkdownTextBlock) then
    Exit;
  if not lLast.Closed then
    Result:=TMarkdownParagraphBlock(lLast)
  else if (lLast.Line=aLine.LineNo-2) and (Pos(#10,TMarkdownTextBlock(lLast.LastChild).Text)=0) then
    // A one-line paragraph separated by one blank line
    Result:=TMarkdownParagraphBlock(lLast);
end;


function TDefinitionListProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lIndent : Integer;

begin
  Result:=False;
  if not (mdoDefinitionLists in Parser.Options) then
    Exit;
  if not IsDefinitionLine(aLine,MaxBlockIndent(aParent),lIndent) then
    Exit;
  Result:=Assigned(GetTermParagraph(aParent,aLine)) or (aParent.LastChild is TMarkdownDefinitionListBlock);
end;


function TDefinitionListProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine;
  aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lPar : TMarkdownParagraphBlock;
  lList : TMarkdownDefinitionListBlock;
  lTerm : TMarkdownDefinitionTermBlock;
  lDef : TMarkdownDefinitionBlock;
  lText : TMarkdownTextBlock;
  lTermText : TMarkdownString;
  lIndent,lTermLine,P,lIdx : Integer;
  lPrevious : TMarkdownBlock;
  lLoose : Boolean;

begin
  lList:=Nil;
  lPar:=GetTermParagraph(aParent,aLine);
  if Assigned(lPar) then
    begin
    lText:=TMarkdownTextBlock(lPar.LastChild);
    lTermText:=lText.Text;
    P:=Length(lTermText);
    while (P>0) and (lTermText[P]<>#10) do
      Dec(P);
    lTermLine:=lPar.Line+lTermText.CountChar(#10);
    lLoose:=lPar.Closed;
    lIdx:=aParent.IndexOfChild(lPar);
    if P=0 then
      begin
      if lIdx>0 then
        lPrevious:=aParent.Children[lIdx-1]
      else
        lPrevious:=Nil;
      if lPrevious is TMarkdownDefinitionListBlock then
        lList:=TMarkdownDefinitionListBlock(lPrevious);
      aParent.DeleteChild(lIdx);
      end
    else
      begin
      lText.Text:=Copy(lTermText,1,P-1);
      Delete(lTermText,1,P);
      lPar.Closed:=True;
      end;
    if lList=Nil then
      lList:=TMarkdownDefinitionListBlock.Create(aParent,lTermLine);
    lList.Loose:=lList.Loose or lLoose;
    lTerm:=TMarkdownDefinitionTermBlock.Create(lList,lTermLine);
    TMarkdownTextBlock.Create(lTerm,lTermLine,Trim(lTermText));
    lTerm.Closed:=True;
    end
  else
    begin
    lList:=aParent.LastChild as TMarkdownDefinitionListBlock;
    lList.Loose:=lList.Loose or EndedAfterBlank;
    end;
  IsDefinitionLine(aLine,MaxBlockIndent(aParent),lIndent);
  lDef:=TMarkdownDefinitionBlock.Create(lList,aLine.LineNo);
  lDef.ContentIndent:=lIndent;
  aLine.Advance(lIndent);
  ParseContainer(lDef);
  lDef.Closed:=True;
  if lDef.ChildCount>1 then
    lList.Loose:=True;
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TFootnoteDefinitionProcessor
  ---------------------------------------------------------------------}

class function TFootnoteDefinitionProcessor.IsFootnoteLine(aLine: TMarkdownLine; aMaxIndent: Integer; out aLabel: TMarkdownString;
  out aLength: Integer): Boolean;

var
  S : TMarkdownString;
  lWhite,P,I : Integer;

begin
  Result:=False;
  aLabel:='';
  aLength:=0;
  lWhite:=aLine.LeadingWhitespace;
  if lWhite>=aMaxIndent then
    Exit;
  S:=aLine.Remainder;
  Delete(S,1,lWhite);
  if not S.StartsWith('[^') then
    Exit;
  P:=Pos(']:',S);
  if P<4 then
    Exit;
  aLabel:=Copy(S,3,P-3);
  for I:=1 to Length(aLabel) do
    if IsWhitespaceChar(aLabel[I]) or (aLabel[I] in ['[',']','^']) then
      Exit;
  aLength:=lWhite+P+1;
  Result:=True;
end;


function TFootnoteDefinitionProcessor.IsContainerStart(aLine: TMarkdownLine): Boolean;

var
  lLabel : TMarkdownString;
  lLength : Integer;

begin
  Result:=IsFootnoteLine(aLine,4,lLabel,lLength);
end;


function TFootnoteDefinitionProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lLabel : TMarkdownString;
  lLength : Integer;

begin
  Result:=(mdoFootnotes in Parser.Options) and IsFootnoteLine(aLine,MaxBlockIndent(aParent),lLabel,lLength);
end;


function TFootnoteDefinitionProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine;
  aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownFootnoteBlock;
  lLabel : TMarkdownString;
  lLength : Integer;

begin
  IsFootnoteLine(aLine,MaxBlockIndent(aParent),lLabel,lLength);
  lBlock:=TMarkdownFootnoteBlock.Create(aParent,aLine.LineNo);
  lBlock.FootnoteLabel:=lLabel;
  lBlock.ContentIndent:=aLine.LeadingWhitespace+4;
  if Assigned(Parser.Document) and not Parser.Document.AddFootnoteDef(lBlock) then
    Parser.DoMessageFmt(mlError,aLine.LineNo,SErrDuplicateFootnote,[MarkdownToString(lLabel)]);
  aLine.Advance(lLength);
  ParseContainer(lBlock);
  lBlock.Closed:=True;
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TAdmonitionProcessor
  ---------------------------------------------------------------------}

// Alert type for an admonition keyword; unknown keywords are notes.
function AdmonitionAlertType(const aKeyword : TMarkdownString) : TAlertType;

begin
  case LowerCase(aKeyword) of
    'tip','hint','example' :
      Result:=atTip;
    'success','check','done' :
      Result:=atSuccess;
    'important','question','help','faq' :
      Result:=atImportant;
    'warning','attention' :
      Result:=atWarning;
    'caution','danger','error','bug','failure','fail','missing' :
      Result:=atCaution;
  else
    Result:=atNote;
  end;
end;


class function TAdmonitionProcessor.IsAdmonitionLine(aLine: TMarkdownLine; aMaxIndent: Integer; out aType: TAlertType;
  out aTitle: TMarkdownString): Boolean;

var
  S : TMarkdownString;
  lWhite,I,lLen : Integer;

begin
  Result:=False;
  aType:=atNote;
  aTitle:='';
  lWhite:=aLine.LeadingWhitespace;
  if lWhite>=aMaxIndent then
    Exit;
  S:=aLine.Remainder;
  Delete(S,1,lWhite);
  if not S.StartsWith('!!!') then
    Exit;
  Delete(S,1,3);
  lLen:=Length(S);
  I:=1;
  while (I<=lLen) and IsWhitespaceChar(S[I]) do
    Inc(I);
  if I=1 then
    Exit;
  Delete(S,1,I-1);
  lLen:=Length(S);
  I:=1;
  while (I<=lLen) and (S[I] in ['A'..'Z','a'..'z','0'..'9','-','_']) do
    Inc(I);
  if (I=1) or ((I<=lLen) and not IsWhitespaceChar(S[I])) then
    Exit;
  aType:=AdmonitionAlertType(Copy(S,1,I-1));
  aTitle:=Trim(Copy(S,I,lLen-I+1));
  if (Length(aTitle)>=2) and (aTitle[1]='"') and (aTitle[Length(aTitle)]='"') then
    aTitle:=Copy(aTitle,2,Length(aTitle)-2);
  Result:=True;
end;


function TAdmonitionProcessor.IsContainerStart(aLine: TMarkdownLine): Boolean;

var
  lType : TAlertType;
  lTitle : TMarkdownString;

begin
  Result:=IsAdmonitionLine(aLine,4,lType,lTitle);
end;


function TAdmonitionProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lType : TAlertType;
  lTitle : TMarkdownString;

begin
  Result:=(mdoAdmonitions in Parser.Options) and IsAdmonitionLine(aLine,MaxBlockIndent(aParent),lType,lTitle);
end;


function TAdmonitionProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine;
  aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownAlertBlock;
  lType : TAlertType;
  lTitle : TMarkdownString;

begin
  IsAdmonitionLine(aLine,MaxBlockIndent(aParent),lType,lTitle);
  lBlock:=TMarkdownAlertBlock.Create(aParent,aLine.LineNo);
  lBlock.AlertType:=lType;
  lBlock.TitleText:=lTitle;
  lBlock.ContentIndent:=aLine.LeadingWhitespace+4;
  aLine.Advance(Length(aLine.Remainder));
  ParseContainer(lBlock);
  lBlock.Closed:=True;
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TLinkReferenceProcessor
  ---------------------------------------------------------------------}

class function TLinkReferenceProcessor.ParseDefinition(const aText: TMarkdownString; out aLabel, aURL, aTitle: TMarkdownString): Boolean;

var
  S : TMarkdownString;
  P,lLen : Integer;
  lClose : TMarkdownChar;

  function Unescape(const aValue : TMarkdownString) : TMarkdownString;
  var
    I : Integer;
  begin
    Result:='';
    I:=1;
    while I<=Length(aValue) do
      begin
      if (aValue[I]='\') and (I<Length(aValue)) and MustEscape(aValue[I+1]) then
        Inc(I);
      Result:=Result+aValue[I];
      Inc(I);
      end;
  end;

  procedure SkipSpaces;
  begin
    while (P<=lLen) and IsWhitespaceChar(S[P]) do
      Inc(P);
  end;

begin
  Result:=False;
  aLabel:='';
  aURL:='';
  aTitle:='';
  S:=Trim(aText);
  lLen:=Length(S);
  if (lLen<4) or (S[1]<>'[') then
    Exit;
  P:=2;
  while (P<=lLen) and (S[P]<>']') do
    begin
    if S[P]='[' then
      Exit;
    if (S[P]='\') and (P<lLen) then
      Inc(P);
    Inc(P);
    end;
  if (P>=lLen) or (S[P+1]<>':') then
    Exit;
  aLabel:=Copy(S,2,P-2);
  if Trim(aLabel)='' then
    Exit;
  P:=P+2;
  SkipSpaces;
  if P>lLen then
    Exit;
  if S[P]='<' then
    begin
    Inc(P);
    while (P<=lLen) and (S[P]<>'>') do
      begin
      aURL:=aURL+S[P];
      Inc(P);
      end;
    if P>lLen then
      Exit;
    Inc(P);
    end
  else
    while (P<=lLen) and not IsWhitespaceChar(S[P]) do
      begin
      aURL:=aURL+S[P];
      Inc(P);
      end;
  aURL:=URLEscape(Unescape(aURL));
  if (P<=lLen) and not IsWhitespaceChar(S[P]) then
    Exit;
  SkipSpaces;
  if P<=lLen then
    begin
    case S[P] of
      '"' : lClose:='"';
      '''' : lClose:='''';
      '(' : lClose:=')';
    else
      Exit;
    end;
    if S[lLen]<>lClose then
      Exit;
    aTitle:=Unescape(Copy(S,P+1,lLen-P-1));
    end;
  Result:=True;
end;


function TLinkReferenceProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

var
  lLabel,lURL,lTitle : TMarkdownString;

begin
  Result:=False;
  if not (mdoLinkReferences in Parser.Options) then
    Exit;
  if aLine.LeadingWhitespace>=MaxBlockIndent(aParent) then
    Exit;
  if TMarkdownParser.InPara(aParent.Blocks,False) then
    Exit;
  Result:=ParseDefinition(aLine.Remainder,lLabel,lURL,lTitle);
end;


function TLinkReferenceProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine;
  aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lLabel,lURL,lTitle : TMarkdownString;

begin
  ParseDefinition(aLine.Remainder,lLabel,lURL,lTitle);
  if Assigned(Parser.Document) and not Parser.Document.AddLinkRef(lLabel,lURL,lTitle) then
    Parser.DoMessageFmt(mlWarning,aLine.LineNo,SWarnDuplicateLinkRef,[MarkdownToString(lLabel)]);
  Result:=True;
end;


{ ---------------------------------------------------------------------
  TDirectiveProcessor
  ---------------------------------------------------------------------}

function TDirectiveProcessor.ContextIndex(aBlock: TMarkdownBlock): Integer;

begin
  Result:=FContextCount-1;
  while (Result>=0) and (FContexts[Result].Block<>aBlock) do
    Dec(Result);
end;


function TDirectiveProcessor.StripOuter(aIndex: Integer; aLine: TMarkdownLine): Boolean;

var
  lOuter : TMarkdownBlockProcessor;

begin
  lOuter:=FContexts[aIndex].Outer;
  if lOuter=Self then
    Result:=(aIndex>0) and StripContext(aIndex-1,aLine)
  else if Assigned(lOuter) then
    Result:=lOuter.StripLinePrefix(aLine)
  else
    Result:=True;
end;


function TDirectiveProcessor.StripContext(aIndex: Integer; aLine: TMarkdownLine): Boolean;

begin
  Result:=StripOuter(aIndex,aLine) and not IsClosingFence(FContexts[aIndex].Block,aLine);
end;


function TDirectiveProcessor.IsClosingFence(aBlock: TMarkdownContainerDirectiveBlock; aLine: TMarkdownLine): Boolean;

var
  S : TMarkdownString;
  lColons : Integer;

begin
  Result:=aLine.LeadingWhitespace<MaxBlockIndent(aBlock.Parent);
  if not Result then
    Exit;
  S:=Trim(aLine.Remainder);
  lColons:=CountStartChars(S,':');
  Result:=(lColons>=aBlock.FenceLength) and (lColons=Length(S));
end;


procedure TDirectiveProcessor.ParseContainer(aBlock: TMarkdownContainerDirectiveBlock);

var
  lLine : TMarkdownLine;
  lStart : Integer;

begin
  if FContextCount=Length(FContexts) then
    SetLength(FContexts,FContextCount+4);
  FContexts[FContextCount].Block:=aBlock;
  FContexts[FContextCount].Outer:=ParentProcessor;
  Inc(FContextCount);
  try
    Parse(aBlock,Self);
    lLine:=PeekLine;
    if Assigned(lLine) then
      begin
      lStart:=lLine.CursorPos;
      if StripOuter(FContextCount-1,lLine) and IsClosingFence(aBlock,lLine) then
        NextLine
      else
        lLine.CursorPos:=lStart;
      end;
  finally
    Dec(FContextCount);
  end;
end;


function TDirectiveProcessor.StripLinePrefix(aLine: TMarkdownLine): Boolean;

begin
  Result:=(FContextCount>0) and StripContext(FContextCount-1,aLine);
end;


function TDirectiveProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;

var
  lIdx,lStart : Integer;

begin
  Result:=(aLine=Nil);
  if Result then
    Exit;
  lIdx:=ContextIndex(aBlock);
  if lIdx<0 then
    Exit(True);
  lStart:=aLine.CursorPos;
  Result:=not StripContext(lIdx,aLine);
  if Result then
    aLine.CursorPos:=lStart;
end;


function TDirectiveProcessor.EndsList: Boolean;

begin
  Result:=True;
end;


function TDirectiveProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

begin
  Result:=False;
  if not (mdoDirectives in Parser.Options) then
    Exit;
  if aLine.LeadingWhitespace>=MaxBlockIndent(aParent) then
    Exit;
  Result:=ParseDirectiveFence(TrimLeft(aLine.Remainder),FColons,FName,FLabel,FAttributes,FHasLabel) and (FName<>'');
end;


function TDirectiveProcessor.ProcessLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine;
  aContext: TMarkdownBlockProcessingContext): Boolean;

var
  lBlock : TMarkdownDirectiveBlock;

begin
  if FColons=2 then
    lBlock:=TMarkdownLeafDirectiveBlock.Create(aParent,aLine.LineNo)
  else
    lBlock:=TMarkdownContainerDirectiveBlock.Create(aParent,aLine.LineNo);
  lBlock.DirectiveName:=FName;
  lBlock.LabelText:=FLabel;
  lBlock.HasLabel:=FHasLabel;
  lBlock.ContentIndent:=aLine.LeadingWhitespace;
  if lBlock.ApplyAttributes(FAttributes,True) and (lBlock.ID<>'') then
    Parser.RegisterAnchor(lBlock.ID,lBlock,aLine.LineNo);
  if lBlock is TMarkdownContainerDirectiveBlock then
    begin
    TMarkdownContainerDirectiveBlock(lBlock).FenceLength:=FColons;
    ParseContainer(TMarkdownContainerDirectiveBlock(lBlock));
    end;
  lBlock.Closed:=True;
  Result:=True;
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
  TCommentBlockProcessor.Register('comment');
  TFootnoteDefinitionProcessor.Register('footnote');
  TLinkReferenceProcessor.Register('linkreference');
  TDefinitionListProcessor.Register('definitionlist');
  TAdmonitionProcessor.Register('admonition');
  TDirectiveProcessor.Register('directive');
  TParagraphProcessor.Register('paragraph');
end;

initialization
  TFrontmatterProcessor.Lenient:=False;
  TTableProcessor.DefaultStrict:=True;
  RegisterDefaultProcessors;
end.

