{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown basic block definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Markdown.Elements;

{$mode ObjFPC}
{$h+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, System.Regexpr, System.CodePages.unicodedata,
{$ELSE}
  Classes, SysUtils, Contnrs, RegExpr, UnicodeData,
{$ENDIF}
  Markdown.Utils,
  Markdown.HTMLEntities;

Type
  EMarkdown = class(Exception);

  TWhitespaceMode = (wsLeave, wsTrim, wsStrip);

  TPosition = record
    line : integer;
    col : integer;
  end;

  TMarkdownElement = class (TObject);
  TMarkdownElementClass = class of TMarkdownElement;

  TTextNodeKind = (nkNamed,nkLineBreak,nkText,nkCode,nkURI,nkEmail,nkImg);
  TTextNodeKinds = set of TTextNodeKind;

  TNodeStyle = (nsStrong,nsEmph,nsDelete);
  TNodeStyles = Set of TNodeStyle;

  { TMarkdownTextNode }

  TMarkdownTextNode = class(TMarkdownElement)
  private
    FKind: TTextNodeKind;
    FName : Ansistring;
    FAttrs: THashTable;
    FContent : AnsiString;
    FBuild : RawByteString;
    FLength : integer;
    FPos : TPosition;
    FActive : Boolean;
    FStyles: TNodeStyles;
    function GetAttrs: THashTable;
    function GetHasAttrs: Boolean;
    function getText : AnsiString;
    function GetNodetext : ansistring;
    procedure SetName(const Value: AnsiString);
    procedure SetActive(const aValue: boolean);
  public
    constructor Create(aPos : TPosition; aKind : TTextNodeKind);
    destructor Destroy; override;
    procedure AddStyle(aStyle : TNodeStyle);
    procedure IncCol(aCount : integer);
    property Kind : TTextNodeKind Read FKind Write FKind;
    property Name : AnsiString read FName write SetName;
    property Attrs : THashTable read GetAttrs;
    property HasAttrs : Boolean Read GetHasAttrs;
    property NodeText : ansistring read GetNodetext;
    procedure AddText(ch : char); overload;
    procedure AddText(const s : AnsiString); overload;
    procedure RemoveChars(count : integer);
    function IsEmpty : boolean;
    property Pos : TPosition Read FPos;
    property Active : Boolean Read FActive Write SetActive;
    property Styles : TNodeStyles Read FStyles Write FStyles;
  end;

  { TMarkdownTextNodeList }

  TMarkdownTextNodeList = class (specialize TGFPObjectList<TMarkdownTextNode>)
  private
    procedure ClearActive; inline;
  public
    // When the last block is active, add to that block. Otherwise, create a new text node
    function AddText(aPos: TPosition; const aContent: AnsiString): TMarkdownTextNode;
    function AddTextNode(aPos: TPosition; aKind: TTextNodeKind; const cnt: AnsiString; aDoClose: Boolean=True): TMarkdownTextNode; // always make a new node, and make it inactive
    procedure RemoveAfter(node : TMarkdownTextNode);
    function LastNode : TMarkdownTextNode;
    procedure ApplyStyleBetween(aStart,aStop : TMarkdownTextNode; aStyle : TNodeStyle);
  end;

  { TMarkdownBlock }

  TMarkdownBlock = class abstract (TMarkdownElement)
  private
    FClosed: boolean;
    FLine : integer;
    FParent : TMarkdownBlock;
  protected
    procedure SetClosed(const aValue: boolean); virtual;
    function GetChild(aIndex : Integer): TMarkdownBlock; virtual;
    function GetChildCount: Integer; virtual;
    procedure AddChild(aChild : TMarkdownBlock); virtual;
    function GetLastChild: TMarkdownBlock; virtual;
  public
    constructor Create(aParent : TMarkdownBlock; aLine : Integer);  virtual; reintroduce;
    procedure Dump(const aIndent : string = '');
    Function GetFirstText : String;
    function WhitespaceMode : TWhitespaceMode; virtual;
    property Closed : boolean read FClosed write SetClosed;
    property Line : Integer read FLine;
    property Parent : TMarkdownBlock read FParent;
    property LastChild : TMarkdownBlock Read GetLastChild;
    property ChildCount : Integer read GetChildCount;
    property Children[aIndex : Integer] : TMarkdownBlock read GetChild; default;
  end;
  TMarkdownBlockClass = class of TMarkdownBlock;

  { TMarkdownBlockList }

  TMarkdownBlockList = class(specialize TGFPObjectList<TMarkdownBlock>)
    function lastblock : TMarkdownBlock;
  end;

  { TMarkdownContainerBlock }

  TMarkdownContainerBlock = class (TMarkdownBlock)
  private
    FBlocks: TMarkdownBlockList;
  protected
    procedure AddChild(aChild : TMarkdownBlock); override;
    function GetChild(aIndex : Integer): TMarkdownBlock; override;
    function GetChildCount: Integer; override;
    function GetLastChild: TMarkdownBlock; override;
  public
    constructor Create(aParent : TMarkdownBlock; aLine : Integer); override;
    destructor Destroy; override;
    procedure DeleteChild(aIndex : Integer);
    property Blocks : TMarkdownBlockList read FBlocks;
  end;

  TFrontMatterType = (fmtYAML, fmtTOML, fmtJSON);

  { TMarkdownFrontmatterBlock }

  TMarkdownFrontmatterBlock = class (TMarkdownContainerBlock)
  private
    FFrontMatterType: TFrontMatterType;
    FContent: TStringList;
  public
    constructor Create(aParent : TMarkdownBlock; aLine : Integer); override;
    destructor Destroy; override;
    function WhiteSpaceMode : TWhitespaceMode; override;
    property FrontMatterType : TFrontMatterType read FFrontMatterType write FFrontMatterType;
    property Content : TStringList read FContent;
  end;

  { TMarkdownDocument }

  TMarkdownDocument = class (TMarkdownContainerBlock)
  private
    FFrontmatter: TMarkdownFrontmatterBlock;
  public
    property Frontmatter : TMarkdownFrontmatterBlock read FFrontmatter write FFrontmatter;
  end;

  { TMarkdownParagraphBlock }

  TMarkdownParagraphBlock = class (TMarkdownContainerBlock)
  private
    FHeader: integer;
  public
    function IsPlainPara : boolean; virtual;
    property Header : integer read FHeader write FHeader;
  end;

  TMarkdownQuoteBlock = class (TMarkdownParagraphBlock)
  public
    function IsPlainPara : boolean; override;
  end;

  TMarkdownListBlock = class (TMarkdownContainerBlock)
  private
    FOrdered: boolean;
    FStart: integer;
    FMarker: AnsiString;
    FLoose: boolean;
    FLastIndent: integer;
    FBaseIndent: integer;
    FHasSeenEmptyLine : boolean; // parser state
  public
    function Grace : integer;
    property Ordered : boolean read FOrdered write FOrdered;
    property BaseIndent : integer read FBaseIndent write FBaseIndent;
    property LastIndent : integer read FLastIndent write FLastIndent;
    property Start : integer read FStart write FStart;
    property Marker : AnsiString read FMarker write FMarker;
    property Loose : boolean read FLoose write FLoose;
    property HasSeenEmptyLine : boolean read FHasSeenEmptyLine Write FHasSeenEmptyLine; // parser state
  end;

  TMarkdownListItemBlock = class (TMarkdownParagraphBlock)
  public
    function isPlainPara : boolean; override;
  end;

  TMarkdownHeadingBlock = class (TMarkdownContainerBlock)
  private
    FLevel: integer;
  public
    constructor Create(aParent : TMarkdownBlock; aLine, alevel : Integer); reintroduce;
    property Level : integer read FLevel write FLevel;
  end;

  TMarkdownCodeBlock = class (TMarkdownContainerBlock)
  private
    FFenced: boolean;
    FLang: AnsiString;
    FIndent : Integer;
  public
    function WhiteSpaceMode : TWhitespaceMode; override;
    property Fenced : boolean read FFenced write FFenced;
    property Lang : AnsiString read FLang write FLang;
    Property Indent : Integer Read FIndent Write FIndent;
  end;


  TMarkdownTableRowBlock = class (TMarkdownContainerBlock);

  TCellAlign = (caLeft, caCenter, caRight);
  TCellAlignArray = array of TCellAlign;

  { TMarkdownTableBlock }

  TMarkdownTableBlock = class (TMarkdownContainerBlock)
  private
    FColumns: TCellAlignArray;
  public
    property Columns : TCellAlignArray read FColumns Write FColumns;
  end;

  TMarkdownLeafBlock = class abstract (TMarkdownBlock);

  TMarkdownThematicBreakBlock = class (TMarkdownLeafBlock)
  end;

  { TMarkdownTextBlock }

  TMarkdownTextBlock = class (TMarkdownLeafBlock)
  private
    FText: AnsiString;
    FNodes : TMarkdownTextNodeList;
  protected
    procedure SetClosed(const aValue: boolean);override;
  public
    constructor Create(aParent : TMarkdownBlock; aLine : integer; const aText : AnsiString); reintroduce;
    destructor Destroy; override;
    property Text : AnsiString read FText write FText;
    property Nodes : TMarkdownTextNodeList Read FNodes Write FNodes;
  end;

implementation

const
  GrowSize = 10;


{ TMarkdownTextNode }

procedure TMarkdownTextNode.addText(const s: AnsiString);

var
  len,AddLen,NewLen : Integer;
begin
  AddLen:=Length(S);
  if AddLen=0 then exit;
  Len:=Length(FBuild);
  NewLen:=FLength+AddLen;
  if NewLen>=Len then
    setLength(FBuild, NewLen+GrowSize);
  Move(S[1],FBuild[FLength+1],AddLen);
  inc(FLength,AddLen);
end;

constructor TMarkdownTextNode.Create(aPos : TPosition; aKind : TTextNodeKind);
begin
  inherited Create;
  FActive:=True;
  FPos:=aPos;
  FKind:=aKind;
end;

procedure TMarkdownTextNode.addText(ch: char);
var
  len : Integer;
begin
  Len:=Length(FBuild);
  if FLength>=Len then
    setLength(FBuild, Len+GrowSize);
  inc(FLength);
  FBuild[FLength]:=ch;
end;

destructor TMarkdownTextNode.Destroy;
begin
  FreeAndNil(FAttrs);
  inherited;
end;

procedure TMarkdownTextNode.AddStyle(aStyle: TNodeStyle);
begin
  include(FStyles,aStyle);
end;

procedure TMarkdownTextNode.IncCol(aCount: integer);
begin
  Inc(FPos.Col,aCount);
end;

function TMarkdownTextNode.GetAttrs: THashTable;
begin
  if FAttrs = nil then
    FAttrs:=THashTable.create;
  Result:=FAttrs;
end;

function TMarkdownTextNode.GetHasAttrs: Boolean;
begin
  Result:=Assigned(FAttrs);
end;

function TMarkdownTextNode.getText: AnsiString;
begin
  Active:=False;
  Result:=FContent;
end;

function TMarkdownTextNode.GetNodetext: ansistring;
begin
  Result:=Copy(FContent,1,Length(FContent));
end;

function TMarkdownTextNode.isEmpty: boolean;
begin
  if Active then
    Result:=FBuild = ''
  else
    Result:=FContent = '';
end;

procedure TMarkdownTextNode.removeChars(count : integer);
begin
  Active:=False;
  delete(FContent, 1, count);
end;


procedure TMarkdownTextNode.SetActive(const aValue: boolean);
begin
  if FActive and not aValue then
    FContent:=Copy(FBuild,1,FLength);
  FActive:=aValue;
end;

procedure TMarkdownTextNode.SetName(const Value: AnsiString);
begin
  FName:=Value;
  Active:=False;
end;

{ TMarkdownTextNodeList }

procedure TMarkdownTextNodeList.ClearActive;
begin
  if count > 0 then
    Self[Count-1].Active:=False;
end;

procedure TMarkdownTextNodeList.ApplyStyleBetween(aStart,aStop : TMarkdownTextNode; aStyle : TNodeStyle);

var
  Idx,lStart,lStop : Integer;

begin
  lStart:=IndexOf(aStart);
  lStop:=IndexOf(aStop)-1;
  For Idx:=lStart to lStop do
    begin
    Elements[Idx].AddStyle(aStyle)
    end;
end;

function TMarkdownTextNodeList.AddText(aPos: TPosition; const aContent: AnsiString): TMarkdownTextNode;
var
  lNode : TMarkdownTextNode;
begin
  lNode:=Nil;
  if (Count>0) then
    lNode:=Elements[Count-1];
  if (lNode=Nil) or not lNode.Active then
    begin
    Result:=TMarkdownTextNode.Create(aPos,nkText);
    add(Result);
    end
  else
    Result:=lNode;
  Result.addText(aContent);
end;

function TMarkdownTextNodeList.AddTextNode(aPos: TPosition; aKind: TTextNodeKind; const cnt: AnsiString; aDoClose: Boolean
  ): TMarkdownTextNode;
begin
  ClearActive;
  Result:=TMarkdownTextNode.Create(aPos,aKind);
  add(Result);
  Result.addText(cnt);
  if aDoClose then
    Result.Active:=False;
end;

procedure TMarkdownTextNodeList.removeAfter(node: TMarkdownTextNode);
var
  i, idx : integer;

begin
  idx:=indexOf(node);
  for i:=count-1 downto idx+1 do
    Delete(i);
end;

function TMarkdownTextNodeList.lastNode: TMarkdownTextNode;
begin
  Result:=Nil;
  if Count>0 then
    Result:=Elements[Count-1];
end;

{ TMarkdownBlock }

function TMarkdownBlock.GetLastChild: TMarkdownBlock;
begin
  Result:=Nil;
end;

procedure TMarkdownBlock.SetClosed(const aValue: boolean);
begin
  if FClosed=aValue then Exit;
  FClosed:=aValue;
  if aValue and (ChildCount>0) then
    Children[ChildCount-1].Closed:=True;
end;

function TMarkdownBlock.GetChild(aIndex : Integer): TMarkdownBlock;
begin
  if aIndex<0 then ; // Silence compiler warning
  Result:=Nil;
end;

function TMarkdownBlock.GetChildCount: Integer;
begin
  Result:=0;
end;

procedure TMarkdownBlock.AddChild(aChild: TMarkdownBlock);
begin
  if (aChild<>Nil) then
  Raise Exception.Create('Cannot add child to simple block');
end;

constructor TMarkdownBlock.Create(aParent: TMarkdownBlock; aLine: Integer);
begin
  Inherited Create;
  FParent:=aParent;
  if assigned(aParent) then
    aParent.AddChild(Self);
  FLine:=aLine;
end;

procedure TMarkdownBlock.dump(const aIndent: string = '');
var
  I : Integer;
begin
  Write(aIndent);
  if not Closed then
    Write('! ')
  else
    Write('  ');
  Writeln(ClassName);
  For I:=0 to ChildCount-1 do
    Children[i].Dump(aIndent+'  ');
end;

function TMarkdownBlock.WhiteSpaceMode: TWhitespaceMode;
begin
  Result:=wsTrim;
end;

function TMarkdownBlock.GetFirstText: String;
var
  lText : TMarkdownTextBlock;
begin
  Result:='';
  if ChildCount=0 then
    exit;
  if not (Children[0] is TMarkdownTextBlock) then
    exit;
  lText:=TMarkdownTextBlock(Children[0]);
  if lText.Nodes.Count=0 then
    exit;
  Result:=lText.Nodes[0].NodeText;
end;

{ TMarkdownBlockList }

function TMarkdownBlockList.lastblock: TMarkdownBlock;
begin
  Result:=nil;
  if Count>0 then
    Result:=Self[Count-1];
end;

{ TMarkdownContainerBlock }

procedure TMarkdownContainerBlock.AddChild(aChild: TMarkdownBlock);
begin
  if aChild=Nil then
    Raise EMarkdown.CreateFmt('Cannot add nil child to block "%s"',[ClassName]);
  FBlocks.Add(aChild);
end;

function TMarkdownContainerBlock.GetChild(aIndex: Integer): TMarkdownBlock;
begin
  Result:=FBlocks[aIndex];
end;

function TMarkdownContainerBlock.GetChildCount: Integer;
begin
  Result:=FBlocks.Count;
end;

function TMarkdownContainerBlock.GetLastChild: TMarkdownBlock;
begin
  if FBlocks.Count>0 then
    Result:=FBlocks[FBlocks.Count-1]
  else
    Result:=Nil;
end;

constructor TMarkdownContainerBlock.Create(aParent : TMarkdownBlock; aLine : Integer);
begin
  inherited create(aParent,aLine);
  FBlocks:=TMarkdownBlockList.Create(true);
end;

destructor TMarkdownContainerBlock.Destroy;
begin
  FreeAndNil(FBlocks);
  inherited Destroy;
end;

procedure TMarkdownContainerBlock.DeleteChild(aIndex: Integer);
begin
  FBlocks.Delete(aIndex);
end;

{ TMarkdownFrontmatterBlock }

constructor TMarkdownFrontmatterBlock.Create(aParent: TMarkdownBlock; aLine: Integer);
begin
  inherited Create(aParent, aLine);
  FContent := TStringList.Create;
end;

destructor TMarkdownFrontmatterBlock.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

function TMarkdownFrontmatterBlock.WhiteSpaceMode: TWhitespaceMode;
begin
  Result := wsLeave;
end;

{ TMarkdownParagraphBlock }

function TMarkdownParagraphBlock.IsPlainPara: boolean;
begin
  Result:=FHeader = 0;
end;


{ TMarkdownQuoteBlock }

function TMarkdownQuoteBlock.isPlainPara: boolean;
begin
  Result:=false;
end;

{ TMarkdownListBlock }

function TMarkdownListBlock.grace: integer;
begin
  if ordered then
    Result:=2
  else
    Result:=1;
end;

{ TMarkdownListItemBlock }

function TMarkdownListItemBlock.isPlainPara: boolean;
begin
  Result:=false;
end;

{ TMarkdownHeadingBlock }

constructor TMarkdownHeadingBlock.Create(aParent : TMarkdownBlock;aLine, aLevel: Integer);
begin
  Inherited Create(aParent,aLine);
  FLevel:=aLevel;
end;

{ TMarkdownCodeBlock }


function TMarkdownCodeBlock.WhiteSpaceMode: TWhitespaceMode;
begin
  Result:=wsLeave;
end;

{ TMarkdownTableBlock }

{ TMarkdownTextBlock }

procedure TMarkdownTextBlock.SetClosed(const aValue: boolean);
begin
  inherited SetClosed(aValue);
  if assigned(FNodes) then
    FNodes.ClearActive;
end;

constructor TMarkdownTextBlock.Create(aParent: TMarkdownBlock; aLine: integer; const aText: AnsiString);
begin
  inherited Create(aParent,aLine);
  FText:=aText;
end;

destructor TMarkdownTextBlock.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

end.
