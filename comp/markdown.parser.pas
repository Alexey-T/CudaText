{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown block structure parser.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit Markdown.Parser;

{$mode objfpc}
{$h+}
interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CodePages.unicodedata, System.SysUtils, System.Classes, System.Contnrs,
{$ELSE}
  UnicodeData, SysUtils, Classes, Contnrs,
{$ENDIF}
  Markdown.Elements,
  Markdown.Utils,
  Markdown.Scanner,
  Markdown.Line,
  Markdown.InlineText,
  Markdown.HtmlEntities;

resourcestring
  SErrDuplicateID = 'Duplicate id "%s"';
  SErrDuplicateFootnote = 'Duplicate footnote label "%s"';
  SErrUndefinedFootnote = 'Footnote "%s" is not defined';
  SWarnUnusedFootnote = 'Footnote "%s" is not referenced';
  SWarnAutoIDClash = 'Automatic id "%s" is also an explicit id, using "%s"';
  SWarnDuplicateLinkRef = 'Duplicate link reference definition "%s"';

type
  EMarkdown = class(Exception);
  // Forward definition
  TMarkdownParser = class;

  // Options
  TMarkdownOption = (
    mdoGithubFlavoured,  // GitHub flavoured markdown: strikethrough, autolinks
    mdoComments,         // HTML comment blocks, inline comments and markers
    mdoAttributes,       // {#id .class key=value} attribute blocks, explicit heading ids
    mdoHeadingIds,       // automatic heading ids
    mdoDefinitionLists,  // definition lists
    mdoAlerts,           // > [!NOTE] alerts
    mdoFootnotes,        // footnote references and definitions
    mdoCaptions,         // Table: captions and implicit figures
    mdoLinkReferences    // link reference definitions and reference links
  );
  TMarkdownOptions = set of TMarkdownOption;

  TMarkdownMessageLevel = (mlInfo, mlWarning, mlError);
  TMarkdownMessageEvent = procedure(Sender : TObject; aLevel : TMarkdownMessageLevel; aLine : Integer; const aMessage : String) of object;

const
  // All extensions used for documentation
  MarkdownDocExtensions = [mdoGithubFlavoured, mdoComments, mdoAttributes, mdoHeadingIds, mdoDefinitionLists,
                           mdoAlerts, mdoFootnotes, mdoCaptions, mdoLinkReferences];

type

  // Parent block context
  TMarkdownBlockProcessingContext = (bpGeneral, bpCodeBlock, bpFencedCodeBlock);

  { TMarkdownBlockProcessor }
  TMarkdownBlockProcessor = class abstract (TObject)
  private
    FParser : TMarkdownParser;
    function GetParentProcessor: TMarkdownBlockProcessor;
  protected
    function InListOrQuote : boolean; virtual;
    // Access to parser methods
    function PeekLine : TMarkdownLine;
    function NextLine : TMarkdownLine;
    function Done : Boolean;
    procedure RedoLine(aResetLine: Boolean);
    // Same, for the enclosing block, when a line is consumed outside the parse loop.
    // Returns True when the line no longer belongs to that block.
    function ParentEndsLine(aLine : TMarkdownLine) : Boolean;
    function InList(aBlock : TMarkdownBlock; ordered : boolean; const marker : TMarkdownString; indent : integer; grace : integer; out list : TMarkdownListBlock) : boolean;
    function IsBlock(aBlock : TMarkdownBlock; blocks : TMarkdownBlockList; const aLine : TMarkdownString; wsLen : integer = 3) : boolean;
    function CurrentLine : TMarkdownLine;
    procedure Parse(aParent: TMarkdownContainerBlock; aPArentProcessor: TMarkdownBlockProcessor); overload;
    // Our parser
    Property parser : TMarkdownParser read FParser;
    // Parent processor
    property ParentProcessor : TMarkdownBlockProcessor Read GetParentProcessor;

  public
    // One instance is created for each block type.
    constructor Create(aParser : TMarkdownParser); virtual;
    // Register for given block type
    class procedure Register(const aBlockType : String);
    // Take off the prefix that this processor's block puts in front of its content lines.
    // Returns False when the line does not jave it, and so leaves the block.
    function StripLinePrefix(aLine : TMarkdownLine) : Boolean; virtual;
    // This is called to see whether aLine closes the current block.
    function LineEndsBlock(aBlock : TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean; virtual;
    // Return true if a line this processor would handle should terminate an
    // enclosing list (i.e. this block type "interrupts" a list per CommonMark).
    // Override to True in heading, thematic-break, fenced-code, quote, etc.
    // HandlesLine is consulted right after and must be side-effect-free on
    // processors that override this to True.
    function EndsList: Boolean; virtual;
    // Return true if this processor handles the current line. Needs to handle state.
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; virtual; abstract;
    // When HandlesLine returned true, ProcessLine is called.
    // If ProcessLine returned true, the next line is started.
    // If it is false, the line is given to another processor.
    // processline is where a new block is created and attached to the parent block.
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; context : TMarkdownBlockProcessingContext) : boolean; virtual; abstract;
  end;
  TMarkdownBlockProcessorArray = array of TMarkdownBlockProcessor;
  TMarkdownBlockProcessorClass = class of TMarkdownBlockProcessor;
  TMarkdownBlockProcessorClassArray = array of TMarkdownBlockProcessorClass;

  { TMarkdownDocumentProcessor }

  // We always need this one. It is not registered, but created hardcoded
  TMarkdownDocumentProcessor = class (TMarkdownBlockProcessor)
  public
    function ProcessLine(aParent : TMarkdownContainerBlock; aLine : TMarkdownLine; aContext : TMarkdownBlockProcessingContext) : Boolean; override;
    function HandlesLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine): boolean; override;
  end;


  { TMarkdownParser }

  TMarkdownParser = class(TComponent)
  private
    FDocument: TMarkdownDocument;
    FFileName: String;
    FOnMessage: TMarkdownMessageEvent;
    FLazy: Boolean;
    FLines : TMarkdownLineList;
    FCurrentLine : integer;
    FBuilder : TMarkdownStringBuilder;
    FEntities : TFPStringHashTable;
    FOptions : TMarkdownOptions;
    FProcessors : TMarkdownBlockProcessorArray;
    FProcessorStack : TStack;
    function CreateScanner(const aText: TMarkdownString; aStartLine: Integer): TMarkdownTextScanner;
  Protected
    // Collect all entitues
    procedure CollectEntities(aList: TFPStringHashTable);
    // aLine operations
    // Convert strings in lines to TMarkdownLine instances
    procedure ConvertLines(aLines : TStrings);
    // Get the current line (can be nil);
    function CurrentLine : TMarkdownLine;
    // Get the next line (can be nil), but do not move the current line pointer
    function PeekLine : TMarkdownLine;
    // Get the next line (can be nil) and move the current line pointer
    function NextLine : TMarkdownLine;
    // Should we re-process the current line ? Moves line pointer one back, optionally resets the line.
    procedure RedoLine(aResetLine : Boolean = True);
    // is there a line available for redo ?
    function CanRedo : boolean;
    // Have we reached the last line ?
    function Done : boolean;
    // status
    // Is the last block a list with the given properties ?
    // if yes, return the list
    function InList(aBlock: TMarkdownBlock; aOrdered: boolean; const aMarker: TMarkdownString; aIndent: integer; aGrace: integer; out
      aList: TMarkdownListBlock): boolean;
    // Does aLine start a new block (true) or can it be a continuation (false) ?
    function IsBlock(aParent: TMarkdownBlock; aBlocks: TMarkdownBlockList; const aLine: TMarkdownString; aWhiteSpaceLen: integer): boolean;
    // block parsing loop
    procedure Parse(aParent: TMarkdownContainerBlock; aPArentProcessor: TMarkdownBlockProcessor); overload;
    // Parent processor of current processor
    function ParentProcessor : TMarkdownBlockProcessor;
    // process a text line
    function ProcessText(const aText: TMarkdownString; wsMode: TWhitespaceMode; aStartLine: integer): TMarkdownTextNodeList;
    // Recursively process inline text
    procedure ProcessInlines(aBlock : TMarkdownBlock; wsMode : TWhitespaceMode);
    // Initialize processors
    Procedure InitProcessors;
    // Done with processors
    Procedure DoneProcessors;
    // To customize the Inline Text processor class, override this.
    function GetInlineTextProcessorClass : TInlineTextProcessorClass; virtual;
    // To customize top-level document class, override this.
    function CreateDocument(aLine: integer): TMarkdownDocument; virtual;
    // To customize top-level document bloc k parser, override this.
    function CreateDocumentProcessor: TMarkdownDocumentProcessor; virtual;
    // Run all registered transforms on aDocument
    procedure ApplyTransforms(aDocument : TMarkdownDocument); virtual;
  public
    class var
      // Inline text processor to use. Assign to install an add-on such as emoji shortcodes.
      DefaultInlineTextProcessorClass : TInlineTextProcessorClass;
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Utility function to handle parsing of inline text.
    procedure ParseInline(aParent : TMarkdownContainerBlock; const aLine : TMarkdownString);
    // Parse the markDown in strings
    function Parse(aSource: TStrings): TMarkdownDocument; overload;
    // Utility function: Parse the markDown in file aFileName.
    function ParseFile(const aFilename : String): TMarkdownDocument;
    // Helper : is the last block a plain paragraph ?
    class function InPara(blocks : TMarkdownBlockList; canBeQuote : boolean) : boolean;
    // Helper to quickly parse a stringlist into a markdown document
    class function FastParse(aSource: TStrings; aOptions: TMarkdownOptions): TMarkdownDocument;
    // Helper to quickly parse a stringlist into a markdown document
    class function FastParseFile(const aFileName : String; aOptions: TMarkdownOptions = []): TMarkdownDocument;
    // Report a problem through OnMessage
    procedure DoMessage(aLevel : TMarkdownMessageLevel; aLine : Integer; const aMessage : String);
    // Report a problem through OnMessage, formatting aFmt with aArgs
    procedure DoMessageFmt(aLevel : TMarkdownMessageLevel; aLine : Integer; const aFmt : String; const aArgs : Array of const);
    // Register aTarget under aID in the document being parsed. A duplicate id is reported and returns False.
    function RegisterAnchor(const aID : TMarkdownString; aTarget : TMarkdownElement; aLine : Integer) : Boolean;
    // Inline processing of aText, which starts on line aLine. The caller owns the result.
    function ParseInlineText(const aText : TMarkdownString; aLine : Integer) : TMarkdownTextNodeList;
    // State control in lazy continuation .
    property Lazy : Boolean Read FLazy Write FLazy;
    // The document being parsed
    property Document : TMarkdownDocument Read FDocument;
    // HTML entities to convert
    Property Entities : TFPStringHashTable read FEntities;
    // Registered block processors (read-only, parser-owned).
    Property Processors : TMarkdownBlockProcessorArray Read FProcessors;
  published
    // Options
    Property Options : TMarkdownOptions Read FOptions Write FOptions;
    // Source file name, copied to the document. Set by ParseFile.
    Property FileName : String Read FFileName Write FFileName;
    // Called for problems that do not stop the parse, such as duplicate ids.
    Property OnMessage : TMarkdownMessageEvent Read FOnMessage Write FOnMessage;
  end;

  { TMarkdownTransform }

  TMarkdownTransform = class abstract (TObject)
  private
    FParser : TMarkdownParser;
  public
    constructor Create(aParser : TMarkdownParser); virtual;
    // Register as transform aName. Transforms run in registration order.
    class procedure Register(const aName : String);
    // Transform the parsed document
    procedure Apply(aDocument : TMarkdownDocument); virtual; abstract;
    // The parser that produced the document
    property Parser : TMarkdownParser read FParser;
  end;
  TMarkdownTransformClass = class of TMarkdownTransform;
  TMarkdownTransformClassArray = array of TMarkdownTransformClass;

  { TMarkdownTransformFactory }

  TMarkdownTransformFactory = class(TObject)
  private
    class var _instance : TMarkdownTransformFactory;
  private
    FNames : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    class constructor Init;
    class destructor Done;
    // All registered transforms, in registration order
    function All : TMarkdownTransformClassArray;
    // Register aTransform as aName. An existing registration with that name is replaced in place.
    procedure RegisterTransform(const aName : String; aTransform : TMarkdownTransformClass);
    // Find the transform registered as aName
    function FindTransform(const aName : String) : TMarkdownTransformClass;
    // Singleton instance
    class property Instance : TMarkdownTransformFactory Read _instance;
  end;

  { TMarkdownProcessorFactory }

  TMarkdownProcessorFactory = class(TObject)
  Private
    class var _instance : TMarkdownProcessorFactory;
  private
    Type
      TRegisteredProcessor = class(TObject)
        Name : String;
        Processor : TMarkdownBlockProcessorClass;
        constructor Create(const aName : String; aProcessor : TMarkdownBlockProcessorClass);
      end;
      TRegisteredProcessorList = class(Specialize TGFPObjectList<TRegisteredProcessor>);
  Private
    FList : TRegisteredProcessorList;
    function Find(const aBlockType : String; aAllowCreate : Boolean) : TRegisteredProcessor;
  public
    constructor Create;
    destructor Destroy; override;
    class constructor Init;
    class destructor Done;
    // Return an array with all known processors.
    function All : TMarkdownBlockProcessorClassArray;
    // Find a processor for a block of type aBlockType
    function FindProcessor(const aBlockType : String) : TMarkdownBlockProcessorClass;
    // Register a processor for block type aBlockType. Existing processor will be overwritten
    Procedure RegisterProcessor(const aBlockType : String; aProcessor : TMarkdownBlockProcessorClass);
    // Singleton instance
    class property Instance : TMarkdownProcessorFactory Read _instance;
  end;


implementation

{ TMarkdownBlockProcessor }

constructor TMarkdownBlockProcessor.Create(aParser: TMarkdownParser);

begin
  inherited Create;
  FParser:=aParser;
end;


class procedure TMarkdownBlockProcessor.Register(const aBlockType: String);

begin
  TMarkdownProcessorFactory.Instance.RegisterProcessor(aBlockType,Self);
end;


function TMarkdownBlockProcessor.LineEndsBlock(aBlock: TMarkdownContainerBlock; aLine: TMarkdownLine): Boolean;

begin
  Result:=(aLine=Nil) or (aBlock=Nil);
end;


function TMarkdownBlockProcessor.EndsList: Boolean;

begin
  Result:=False;
end;


function TMarkdownBlockProcessor.InListOrQuote: boolean;

begin
  // Todo
  Result:=false;
end;


function TMarkdownBlockProcessor.PeekLine: TMarkdownLine;

begin
  Result:=FParser.PeekLine;
end;


function TMarkdownBlockProcessor.NextLine: TMarkdownLine;

begin
  Result:=FParser.NextLine;
end;


function TMarkdownBlockProcessor.Done: Boolean;

begin
  Result:=FParser.Done;
end;


procedure TMarkdownBlockProcessor.RedoLine(aResetLine: Boolean);

begin
  FParser.RedoLine(aResetLine);
end;


function TMarkdownBlockProcessor.InList(aBlock: TMarkdownBlock; ordered: boolean; const marker: TMarkdownString; indent: integer;
  grace: integer; out list: TMarkdownListBlock): boolean;

begin
  Result:=FParser.InList(aBlock,ordered,marker,indent,grace,list);
end;


function TMarkdownBlockProcessor.IsBlock(aBlock: TMarkdownBlock; blocks: TMarkdownBlockList; const aLine: TMarkdownString; wsLen: integer
  ): boolean;

begin
  Result:=FParser.IsBlock(aBlock,Blocks,aLine,wsLen);
end;


function TMarkdownBlockProcessor.CurrentLine: TMarkdownLine;

begin
  Result:=FParser.CurrentLine;
end;


procedure TMarkdownBlockProcessor.Parse(aParent: TMarkdownContainerBlock; aPArentProcessor: TMarkdownBlockProcessor);

begin
  FParser.Parse(aParent,aParentProcessor);
end;


function TMarkdownBlockProcessor.GetParentProcessor: TMarkdownBlockProcessor;

begin
  Result:=FParser.ParentProcessor;
end;


function TMarkdownBlockProcessor.StripLinePrefix(aLine: TMarkdownLine): Boolean;

begin
  if aLine=Nil then ; // Silence warning
  Result:=True;
end;


function TMarkdownBlockProcessor.ParentEndsLine(aLine: TMarkdownLine): Boolean;

var
  lProcessor : TMarkdownBlockProcessor;

begin
  Result:=False;
  lProcessor:=ParentProcessor;
  if Assigned(lProcessor) and (lProcessor<>Self) then
    Result:=Not lProcessor.StripLinePrefix(aLine);
end;

{ TMarkdownDocumentProcessor }

function TMarkdownDocumentProcessor.ProcessLine(aParent : TMarkdownContainerBlock; aLine: TMarkdownLine; aContext : TMarkdownBlockProcessingContext): Boolean;

begin
  Result:=False;
end;


function TMarkdownDocumentProcessor.HandlesLine(aParent: TMarkdownContainerBlock; aLine: TMarkdownLine): boolean;

begin
  Result:=False;
end;


{ TMarkdownParser }

class function TMarkdownParser.FastParse(aSource: TStrings; aOptions: TMarkdownOptions): TMarkdownDocument;

var
  lParser : TMarkdownParser;
  lDone : Boolean;

begin
  Result:=Nil;
  lDone:=false;
  lParser:=TMarkdownParser.Create(Nil);
  try
    lParser.Options:=aOptions;
    Result:=LParser.Parse(aSource);
    lDone:=true;
  finally
    if not lDone then
      Result.Free;
    lParser.free;
  end;
end;

class function TMarkdownParser.FastParseFile(const aFileName: String; aOptions: TMarkdownOptions): TMarkdownDocument;
var
  lFile : TStrings;
begin
  lFile:=TStringList.Create;
  try
    lFile.LoadFromFile(aFileName,TEncoding.UTF8);
    Result:=FastParse(lFile,aOptions);
  finally
    lFile.Free;
  end;
end;


procedure TMarkdownParser.CollectEntities(aList :TFPStringHashTable);

var
  Ent : THTMLEntityDef;

begin
  for ent in EntityDefList do
    aList.Add(ent.e,UnicodeToMarkdown(ent.u));
end;


constructor TMarkdownParser.Create(aOwner : TComponent);

begin
  inherited ;
  FOptions:=[];
  FBuilder:=TMarkdownStringBuilder.Create;
  FProcessorStack:=TStack.Create;
  FLines:=TMarkdownLineList.create(true);
  FEntities:=TFPStringHashTable.create;
  CollectEntities(FEntities);
end;


destructor TMarkdownParser.Destroy;

begin
  FreeAndNil(FEntities);
  FreeAndNil(FProcessorStack);
  FreeAndNil(FBuilder);
  FreeAndNil(FLines);
  inherited;
end;


function TMarkdownParser.CreateDocument(aLine : integer) : TMarkdownDocument;

begin
  Result:=TMarkdownDocument.Create(Nil,aLine);
end;


function TMarkdownParser.CreateDocumentProcessor : TMarkdownDocumentProcessor;

begin
  Result:=TMarkdownDocumentProcessor.Create(Self);
end;

function TMarkdownParser.Parse(aSource: TStrings): TMarkdownDocument;

var
  lProc : TMarkdownDocumentProcessor;
  lDone : Boolean;

begin
  Result:=Nil;
  lProc:=Nil;
  lDone:=false;
  try
    InitProcessors;
    ConvertLines(aSource);
    lProc:=CreateDocumentProcessor;
    Result:=CreateDocument(1);
    Result.FileName:=FFileName;
    FDocument:=Result;
    parse(Result,lProc);
    processInlines(Result, wsTrim);
    ApplyTransforms(Result);
    lDone:=True;
    Result.closed:=True;
  finally
    FDocument:=Nil;
    DoneProcessors;
    if not lDone then
      Result.Free;
    lProc.Free;
  end;
end;

function TMarkdownParser.ParseFile(const aFilename: String): TMarkdownDocument;
var
  lFile : TStrings;
begin
  lFile:=TStringList.Create;
  try
    lFile.LoadFromFile(aFileName,TEncoding.UTF8);
    FFileName:=aFileName;
    Result:=Parse(lFile);
  finally
    lFile.Free;
  end;
end;


procedure TMarkdownParser.DoMessage(aLevel: TMarkdownMessageLevel; aLine: Integer; const aMessage: String);

begin
  if Assigned(FOnMessage) then
    FOnMessage(Self,aLevel,aLine,aMessage);
end;


procedure TMarkdownParser.DoMessageFmt(aLevel: TMarkdownMessageLevel; aLine: Integer; const aFmt: String; const aArgs: array of const);

begin
  DoMessage(aLevel,aLine,Format(aFmt,aArgs));
end;


function TMarkdownParser.RegisterAnchor(const aID: TMarkdownString; aTarget: TMarkdownElement; aLine: Integer): Boolean;

begin
  Result:=Assigned(FDocument) and FDocument.AddAnchor(aID,aTarget);
  if not Result and Assigned(FDocument) then
    DoMessageFmt(mlError,aLine,SErrDuplicateID,[MarkdownToString(aID)]);
end;


function TMarkdownParser.ParseInlineText(const aText: TMarkdownString; aLine: Integer): TMarkdownTextNodeList;

begin
  Result:=ProcessText(aText,wsTrim,aLine);
end;


procedure TMarkdownParser.ApplyTransforms(aDocument: TMarkdownDocument);

var
  lClass : TMarkdownTransformClass;
  lTransform : TMarkdownTransform;

begin
  for lClass in TMarkdownTransformFactory.Instance.All do
    begin
    lTransform:=lClass.Create(Self);
    try
      lTransform.Apply(aDocument);
    finally
      lTransform.Free;
    end;
    end;
end;


function TMarkdownParser.NextLine: TMarkdownLine;

begin
  Result:=Nil;
  inc(FCurrentLine);
  if FCurrentLine<FLines.Count then
    Result:=FLines[FCurrentLine];
end;


function TMarkdownParser.PeekLine: TMarkdownLine;

begin
  Result:=nil;
  if (FCurrentLine<FLines.Count-1) then
    Result:=FLines[FCurrentLine+1];
end;


procedure TMarkdownParser.RedoLine(aResetLine: Boolean);

begin
  if aResetLine then
    FLines[FCurrentLine].reset;
  dec(FCurrentLine);
end;


function TMarkdownParser.CanRedo: boolean;

begin
  Result:=(FCurrentLine<FLines.Count);
end;


function TMarkdownParser.Done: boolean;
begin
  Result:=(FCurrentLine>=FLines.Count-1)
end;


function TMarkdownParser.InList(aBlock: TMarkdownBlock; aOrdered: boolean; const aMarker: TMarkdownString; aIndent: integer;
  aGrace: integer; out aList: TMarkdownListBlock): boolean;
var
  lBlock: TMarkdownBlock;
  lList : TMarkdownListBlock absolute lBlock;
begin
  Result:=False;
  lBlock:=aBlock;
  While (Not Result) and Assigned(lBlock) do
    begin
    Result:=lBlock is TMarkdownListBlock;
    // Check for exact match: same type, marker, and base indentation level
    if Result then
      Result:=(lList.ordered=aOrdered)
              and (lList.Marker=aMarker)
              and (aIndent-aGrace <= lList.BaseIndent)
              and not lList.closed;
    if Result then
      aList:=lList;
    // A list does not continue outside the container holding it
    if not ((lBlock is TMarkdownListBlock) or (lBlock is TMarkdownListItemBlock)) then
      Break;
    lBlock:=lBlock.Parent;
    end;
end;


class function TMarkdownParser.InPara(blocks: TMarkdownBlockList; canBeQuote: boolean): boolean;

begin
  Result:=(blocks.Count > 0)
            and (blocks.Last is TMarkdownParagraphBlock)
            and not (blocks.Last as TMarkdownParagraphBlock).closed
            and ((blocks.Last as TMarkdownParagraphBlock).header = 0);
  if Result and not canBeQuote and not (blocks.Last as TMarkdownParagraphBlock).isPlainPara then
    Result:=false;
end;


function TMarkdownParser.IsBlock(aParent: TMarkdownBlock; aBlocks: TMarkdownBlockList; const aLine: TMarkdownString; aWhiteSpaceLen: integer): boolean;

  function inOrderedList : boolean;
  begin
    Result:=(aParent is TMarkdownListBlock) and (aParent as TMarkdownListBlock).Ordered;
  end;

  function IsOpenPara : boolean;
  begin
    Result:=(aBlocks.count > 0) and (aBlocks.lastblock is TMarkdownParagraphBlock) and not (aBlocks.LastBlock.Closed)
  end;

var
  lSkip : integer;
  lLine,lMarker : TMarkdownString;

begin
  // Known blocks
  Result:=True;
  if StartsWithWhitespace(aLine, ['*','-','+','#','`','~','>'],lSkip,aWhiteSpaceLen) then
    Exit;
  // Thematic break
  if StartsWithWhitespace(aLine, '___',lSkip) then
    Exit;
  // Code block
  if (LeadingWhitespace(aLine) >= 4) and not InPara(aBlocks,False) then
    Exit;
  // open para ?
  Result:=Not IsOpenPara;
  if Result then
    exit;
  // Remove whitespace
  lSkip:=LeadingWhitespace(aLine);
  lLine:=RemoveLeadingWhiteSpace(aLine,lSkip);
  // Check ordered List item.
  lMarker:=CopyMatching(lLine, ['0'..'9']);
  if (lMarker='') then
    Exit;
  // 1. is always a new block. In an ordered list we must check.
  if (lMarker='1') or inOrderedList then
    begin
    Delete(lLine,1,Length(lMarker));
    if (lLine<>'') then
      begin
      Result:=(lLine[1] in [')','.']);
      if Result then
        Result:=(length(lLine)>1) and (lLine[2]=' ');
      end;
    end;
end;


procedure TMarkdownParser.ParseInline(aParent: TMarkdownContainerBlock; const aLine: TMarkdownString);

var
  lBlock : TMarkdownTextBlock;

begin
  if (aParent.blocks.Count > 0) and (aParent.blocks.Last is TMarkdownTextBlock) then
    lBlock:=aParent.blocks.Last as TMarkdownTextBlock
  else
    lBlock:=TMarkdownTextBlock.create(aParent,FCurrentLine,'');
  if lBlock.Text<>'' then
    lBlock.Text:=lBlock.Text+#10;
  lBlock.Text:=lBlock.Text+aLine;
end;


procedure TMarkdownParser.Parse(aParent: TMarkdownContainerBlock; aPArentProcessor: TMarkdownBlockProcessor);

var
  lLine : TMarkdownLine;
  lprocessor : TMarkdownBlockProcessor;
  i,lProcCount : Integer;
  lProcessed : boolean;

begin
  FProcessorStack.Push(aParentProcessor);
  try
    Lazy:=False;
    while not done do
      begin
      if aParentProcessor.LineEndsBlock(aParent,PeekLine) then
        exit;
      lLine:=NextLine;
      lProcessed:=False;
      I:=0;
      lProcCount:=Length(FProcessors);
      While (not lProcessed) and (I<lProcCount) do
        begin
        lprocessor:=FProcessors[i];
        lProcessed:=lprocessor.HandlesLine(aParent,lLine);
        if lProcessed then
           lProcessed:=lprocessor.processLine(aParent,lLine,bpGeneral);
        // The last processor is normally the paragraph block...
        inc(I);
        end;
      if not lProcessed then
        Raise EMarkdown.CreateFmt('Line %s not processed',[lLine.LineNo]);
      end;
    if aParent.ChildCount>0 then
      aParent[aParent.ChildCount-1].closed:=True;
  finally
    FProcessorStack.Pop;
  end;
end;


procedure TMarkdownParser.ConvertLines(aLines: TStrings);

var
  i : integer;

begin
  FLines.Clear;
  For I:=0 to aLines.Count-1 do
    FLines.Add(TMarkdownLine.Create(StringToMarkdown(aLines[i]),I+1));
  FCurrentLine:=-1;
end;

function TMarkdownParser.CurrentLine: TMarkdownLine;

begin
  Result:=Nil;
  if (FCurrentLine>=0) and (FCurrentLine<FLines.Count) then
    Result:=FLines[FCurrentLine];
end;


function TMarkdownParser.ParentProcessor: TMarkdownBlockProcessor;

begin
  Result:=TMarkdownBlockProcessor(FProcessorStack.Peek);
end;

function TMarkdownParser.CreateScanner(const aText : TMarkdownString; aStartLine : Integer) : TMarkdownTextScanner;
begin
  Result:=TMarkdownTextScanner.Create(aText,aStartLine);
end;

function TMarkdownParser.ProcessText(const aText: TMarkdownString; wsMode: TWhitespaceMode; aStartLine: integer): TMarkdownTextNodeList;

var
  Scanner : TMarkdownTextScanner;
  Processor : TInlineTextProcessor;
  lClass : TInlineTextProcessorClass;
begin
  Result:=TMarkdownTextNodeList.Create;
  Scanner:=CreateScanner(aText,aStartLine);
  try
    lClass:=GetInlineTextProcessorClass;
    Processor:= LClass.Create(Scanner,Result,FEntities,wsMode);
    Processor.GFMExtensions:=mdoGithubFlavoured in Options;
    Processor.Comments:=mdoComments in Options;
    Processor.Footnotes:=mdoFootnotes in Options;
    if mdoLinkReferences in Options then
      Processor.LinkReferences:=FDocument;
    Processor.process(true);
  finally
    Scanner.Free;
    Processor.Free;
  end;
end;


procedure TMarkdownParser.ProcessInlines(aBlock: TMarkdownBlock; wsMode: TWhitespaceMode);

var
  I : Integer;
  lTextBlock : TMarkdownTextBlock absolute aBlock;

begin
  if aBlock is TMarkdownTextBlock then
    lTextBlock.Nodes:=processText(lTextBlock.Text,wsMode,aBlock.Line);
  for I:=0 to aBlock.ChildCount-1 do
    processInlines(aBlock.Children[i],aBlock.WhiteSpaceMode);
end;


procedure TMarkdownParser.InitProcessors;

var
  lClasses : TMarkdownBlockProcessorClassArray;
  lPar : TMarkdownBlockProcessorClass;
  lCount,I : Integer;

begin
  DoneProcessors;
  lClasses:=TMarkdownProcessorFactory.Instance.All;
  if Length(lClasses)=0 then
    Raise EMarkdown.Create('No markdown processors registered');
  lPar:=TMarkdownProcessorFactory.Instance.findprocessor('paragraph');
  SetLength(FProcessors,Length(lClasses));
  lCount:=0;
  For I:=0 to Length(lClasses)-1 do
    begin
    if lClasses[i]<>lPar then
      begin
      FProcessors[lCount]:=lClasses[i].Create(Self);
      inc(lCount);
      end;
    end;
  FProcessors[lCount]:=LPar.Create(Self);
end;


procedure TMarkdownParser.DoneProcessors;

var
  I : Integer;

begin
  For I:=0 to Length(FProcessors)-1 do
    FreeAndNil(FProcessors[i]);
end;


function TMarkdownParser.GetInlineTextProcessorClass: TInlineTextProcessorClass;

begin
  Result:=DefaultInlineTextProcessorClass;
  if Result=Nil then
    Result:=TInlineTextProcessor;
end;


{ TMarkdownProcessorFactory }

function TMarkdownProcessorFactory.Find(const aBlockType: String; aAllowCreate: Boolean): TRegisteredProcessor;

var
  Idx : integer;

begin
  Result:=Nil;
  Idx:=FList.Count-1;
  While (Result=Nil) and (Idx>=0) do
    begin
    Result:=FList[Idx];
    if not SameText(Result.Name,aBlockType) then
      Result:=Nil;
    Dec(Idx);
    end;
  if (Result=Nil) and aAllowCreate then
    begin
    Result:=TRegisteredProcessor.Create(aBlockType,Nil);
    FList.Add(Result);
    end;
end;


constructor TMarkdownProcessorFactory.Create;

begin
  FList:=TRegisteredProcessorList.Create(True);
end;


destructor TMarkdownProcessorFactory.Destroy;

begin
  FreeAndNil(FList);
  inherited destroy;
end;


class constructor TMarkdownProcessorFactory.Init;

begin
  _instance:=TMarkdownProcessorFactory.Create;
end;


class destructor TMarkdownProcessorFactory.Done;

begin
  FreeAndNil(_instance);
end;


function TMarkdownProcessorFactory.All: TMarkdownBlockProcessorClassArray;

var
  i : integer;

begin
  Result:=[];
  SetLength(Result,FList.Count);
  For I:=0 to FList.Count-1 do
    Result[I]:=FList[i].Processor;
end;


function TMarkdownProcessorFactory.FindProcessor(const aBlockType: String): TMarkdownBlockProcessorClass;

var
  lReg : TRegisteredProcessor;

begin
  Result:=Nil;
  LReg:=Find(aBlockType,False);
  if Assigned(lReg) then
    Result:=lReg.Processor;
end;


procedure TMarkdownProcessorFactory.RegisterProcessor(const aBlockType: String; aProcessor: TMarkdownBlockProcessorClass);

var
  lReg : TRegisteredProcessor;

begin
  LReg:=Find(LowerCase(aBlockType),True);
  lReg.Processor:=aProcessor;
end;


{ TMarkdownProcessorFactory.TRegisteredProcessor }

constructor TMarkdownProcessorFactory.TRegisteredProcessor.Create(const aName: String; aProcessor: TMarkdownBlockProcessorClass);

begin
  Name:=aName;
  Processor:=aProcessor;
end;

{ TMarkdownTransform }

constructor TMarkdownTransform.Create(aParser: TMarkdownParser);

begin
  inherited Create;
  FParser:=aParser;
end;


class procedure TMarkdownTransform.Register(const aName: String);

begin
  TMarkdownTransformFactory.Instance.RegisterTransform(aName,Self);
end;

{ TMarkdownTransformFactory }

constructor TMarkdownTransformFactory.Create;

begin
  FNames:=TStringList.Create;
end;


destructor TMarkdownTransformFactory.Destroy;

begin
  FreeAndNil(FNames);
  inherited Destroy;
end;


class constructor TMarkdownTransformFactory.Init;

begin
  _instance:=TMarkdownTransformFactory.Create;
end;


class destructor TMarkdownTransformFactory.Done;

begin
  FreeAndNil(_instance);
end;


function TMarkdownTransformFactory.All: TMarkdownTransformClassArray;

var
  I : Integer;

begin
  Result:=[];
  SetLength(Result,FNames.Count);
  for I:=0 to FNames.Count-1 do
    Result[I]:=TMarkdownTransformClass(FNames.Objects[I]);
end;


procedure TMarkdownTransformFactory.RegisterTransform(const aName: String; aTransform: TMarkdownTransformClass);

var
  lIdx : Integer;

begin
  lIdx:=FNames.IndexOf(LowerCase(aName));
  if lIdx<0 then
    FNames.AddObject(LowerCase(aName),TObject(aTransform))
  else
    FNames.Objects[lIdx]:=TObject(aTransform);
end;


function TMarkdownTransformFactory.FindTransform(const aName: String): TMarkdownTransformClass;

var
  lIdx : Integer;

begin
  Result:=Nil;
  lIdx:=FNames.IndexOf(LowerCase(aName));
  if lIdx>=0 then
    Result:=TMarkdownTransformClass(FNames.Objects[lIdx]);
end;


end.
