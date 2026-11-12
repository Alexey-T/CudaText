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

  // nkRaw holds content already in the target format, emitted without escaping.
  // nkComment holds the text of an HTML comment, with marker, argument and value in Attrs for a marker.
  // nkFootnoteRef is a footnote reference, with label and number in Attrs.
  TTextNodeKind = (nkNamed,nkLineBreak,nkText,nkCode,nkURI,nkEmail,nkImg,nkRaw,nkComment,nkFootnoteRef);
  TTextNodeKinds = set of TTextNodeKind;

  TNodeStyle = (nsStrong,nsEmph,nsDelete);
  TNodeStyles = Set of TNodeStyle;
  TNodeStyleArray = Array of TNodeStyle;

  TMarkdownTextNodeList = class;

  { TMarkdownTextNode }

  TMarkdownTextNode = class(TMarkdownElement)
  private
    FKind: TTextNodeKind;
    FName : TMarkdownString;
    FAttrs: THashTable;
    FChildren: TMarkdownTextNodeList;
    FContent : TMarkdownString;
    FBuild : TMarkdownString;
    FLength : integer;
    FPos : TPosition;
    FActive : Boolean;
    FStyles: TNodeStyles;
    FStyleList : TNodeStyleArray;
    function GetAttrs: THashTable;
    function GetChildren: TMarkdownTextNodeList;
    function GetHasAttrs: Boolean;
    function GetHasChildren: Boolean;
    function GetText : TMarkdownString;
    function GetNodeText : TMarkdownString;
    procedure SetName(const Value: TMarkdownString);
    procedure SetActive(const aValue: boolean);
    procedure SetStyles(const aValue: TNodeStyles);
  public
    constructor Create(aPos : TPosition; aKind : TTextNodeKind);
    destructor Destroy; override;
    procedure AddStyle(aStyle : TNodeStyle);
    procedure IncCol(aCount : integer);
    property Kind : TTextNodeKind Read FKind Write FKind;
    property Name : TMarkdownString read FName write SetName;
    property Attrs : THashTable read GetAttrs;
    property HasAttrs : Boolean Read GetHasAttrs;
    property NodeText : TMarkdownString read GetNodetext;
    procedure AddText(ch : TMarkdownChar); overload;
    procedure AddText(const s : TMarkdownString); overload;
    procedure RemoveChars(count : integer);
    // Insert aText before the current content of the node
    procedure PrependText(const aText : TMarkdownString);
    // Replace the content of the node with aText
    procedure SetNodeText(const aText : TMarkdownString);
    function IsEmpty : boolean;
    property Pos : TPosition Read FPos;
    property Active : Boolean Read FActive Write SetActive;
    property Styles : TNodeStyles Read FStyles Write SetStyles;
    // The styles of the node in nesting order, outermost first
    property StyleList : TNodeStyleArray Read FStyleList;
    // The nodes rendered inside this node
    property Children : TMarkdownTextNodeList Read GetChildren;
    // Does the node have nodes rendered inside it ?
    property HasChildren : Boolean Read GetHasChildren;
  end;

  { TMarkdownTextNodeList }

  TMarkdownTextNodeList = class (specialize TGFPObjectList<TMarkdownTextNode>)
  private
    procedure ClearActive; inline;
  public
    // When the last block is active, add to that block. Otherwise, create a new text node
    function AddText(aPos: TPosition; const aContent: TMarkdownString): TMarkdownTextNode;
    function AddTextNode(aPos: TPosition; aKind: TTextNodeKind; const cnt: TMarkdownString; aDoClose: Boolean=True): TMarkdownTextNode; // always make a new node, and make it inactive
    procedure RemoveAfter(node : TMarkdownTextNode);
    function LastNode : TMarkdownTextNode;
    procedure ApplyStyleBetween(aStart,aStop : TMarkdownTextNode; aStyle : TNodeStyle);
    // The concatenated text of aNode and all nodes following it
    function TextFrom(aNode : TMarkdownTextNode) : TMarkdownString;
    // Move all nodes following aNode into the child list of aNode
    procedure MoveToChildren(aNode : TMarkdownTextNode);
    // The text of all nodes and their children, without comments and footnote references. Images give their alt text.
    function PlainText : TMarkdownString;
  end;

  { TMarkdownBlock }

  TMarkdownBlock = class abstract (TMarkdownElement)
  private
    FClosed: boolean;
    FLine : integer;
    FParent : TMarkdownBlock;
    FID : TMarkdownString;
    FClasses : TMarkdownStringArray;
    FAttrs : TStrings;
    function GetAttrs: TStrings;
    function GetHasAttrs: Boolean;
  protected
    procedure SetClosed(const aValue: boolean); virtual;
    function GetChild(aIndex : Integer): TMarkdownBlock; virtual;
    function GetChildCount: Integer; virtual;
    procedure AddChild(aChild : TMarkdownBlock); virtual;
    function GetLastChild: TMarkdownBlock; virtual;
  public
    constructor Create(aParent : TMarkdownBlock; aLine : Integer);  virtual; reintroduce;
    destructor Destroy; override;
    procedure Dump(const aIndent : TMarkdownString = '');
    Function GetFirstText : TMarkdownString;
    // The plain text of all text blocks inside this block, lines separated by a space.
    function PlainText : TMarkdownString;
    // Apply an attribute specification {#id .class key=value}. Returns False and changes nothing when aSpec is invalid.
    function ApplyAttributes(const aSpec : TMarkdownString) : Boolean;
    // Explicit or automatic id of the block.
    property ID : TMarkdownString read FID write FID;
    // Classes given in an attribute specification.
    property Classes : TMarkdownStringArray read FClasses write FClasses;
    // key=value attributes given in an attribute specification, created on demand.
    property Attrs : TStrings read GetAttrs;
    // Does the block have an id, classes or key=value attributes ?
    property HasAttrs : Boolean read GetHasAttrs;
    function WhitespaceMode : TWhitespaceMode; virtual;
    property Closed : boolean read FClosed write SetClosed;
    property Line : Integer read FLine;
    property Parent : TMarkdownBlock read FParent;
    // Columns of indentation used by the content of this block. Zero when it has none.
    function ContentIndentation : Integer; virtual;
    property LastChild : TMarkdownBlock Read GetLastChild;
    property ChildCount : Integer read GetChildCount;
    property Children[aIndex : Integer] : TMarkdownBlock read GetChild; default;
  end;
  TMarkdownBlockClass = class of TMarkdownBlock;

  { TMarkdownBlockList }

  TMarkdownBlockList = class(specialize TGFPObjectList<TMarkdownBlock>)
    function LastBlock : TMarkdownBlock;
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
    // Insert aChild at position aIndex and make this block its parent.
    procedure InsertChild(aIndex : Integer; aChild : TMarkdownBlock);
    // Remove the child at aIndex without freeing it. The child has no parent afterwards.
    function ExtractChild(aIndex : Integer) : TMarkdownBlock;
    // Put aNew in the place of aOld and free aOld.
    procedure ReplaceChild(aOld, aNew : TMarkdownBlock);
    // Index of aChild in Blocks, -1 if it is not a child.
    function IndexOfChild(aChild : TMarkdownBlock) : Integer;
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

  TMarkdownFootnoteBlock = class;

  { TMarkdownLinkReference }

  TMarkdownLinkReference = class(TObject)
  private
    FTitle: TMarkdownString;
    FURL: TMarkdownString;
  public
    constructor Create(const aURL, aTitle : TMarkdownString);
    // Link destination
    property URL : TMarkdownString read FURL;
    // Link title, empty when the definition has none
    property Title : TMarkdownString read FTitle;
  end;

  { TMarkdownMarker }

  TMarkdownMarker = class(TObject)
  private
    FArgument: TMarkdownString;
    FBlock: TMarkdownBlock;
    FName: TMarkdownString;
    FNode: TMarkdownTextNode;
    FNodeIndex: Integer;
    FValue: TMarkdownString;
  public
    constructor Create(const aName, aArgument, aValue : TMarkdownString; aBlock : TMarkdownBlock; aNode : TMarkdownTextNode; aNodeIndex : Integer);
    // Marker name: index in <!-- index: x -->
    property Name : TMarkdownString read FName;
    // Marker argument: msgnr in <!-- index[msgnr]: x -->
    property Argument : TMarkdownString read FArgument;
    // Marker value: x in <!-- index: x -->
    property Value : TMarkdownString read FValue;
    // The comment block, or the text block that contains Node.
    property Block : TMarkdownBlock read FBlock;
    // The comment node for an inline marker, Nil for a comment block.
    property Node : TMarkdownTextNode read FNode;
    // Index in the nodes of Block of the node that is or contains Node, -1 for a comment block.
    property NodeIndex : Integer read FNodeIndex;
  end;
  TMarkdownMarkerList = class(specialize TGFPObjectList<TMarkdownMarker>);

  { TMarkdownDocument }

  TMarkdownDocument = class (TMarkdownContainerBlock)
  private
    FFrontmatter: TMarkdownFrontmatterBlock;
    FAnchors : TFPObjectHashTable;
    FFootnoteDefs : TFPObjectHashTable;
    FLinkRefs : TFPObjectHashTable;
    FMarkers : TMarkdownMarkerList;
    FFootnotes : TMarkdownBlockList;
    FFileName : String;
  public
    constructor Create(aParent : TMarkdownBlock; aLine : Integer); override;
    destructor Destroy; override;
    // Register aTarget (a block or a text node) under aID. Returns False when aID is already in use.
    function AddAnchor(const aID : TMarkdownString; aTarget : TMarkdownElement) : Boolean;
    // The block or text node with id aID, Nil if there is none.
    function FindAnchor(const aID : TMarkdownString) : TMarkdownElement;
    // Register a footnote definition. Returns False when its label is already in use.
    function AddFootnoteDef(aBlock : TMarkdownFootnoteBlock) : Boolean;
    // The footnote definition with label aLabel, Nil if there is none.
    function FindFootnoteDef(const aLabel : TMarkdownString) : TMarkdownFootnoteBlock;
    // Register a link reference definition. Returns False when its label is already in use.
    function AddLinkRef(const aLabel, aURL, aTitle : TMarkdownString) : Boolean;
    // The link reference definition with label aLabel, Nil if there is none.
    function FindLinkRef(const aLabel : TMarkdownString) : TMarkdownLinkReference;
    // The front matter block, if the document has one.
    property Frontmatter : TMarkdownFrontmatterBlock read FFrontmatter write FFrontmatter;
    // Id to block or text node.
    property Anchors : TFPObjectHashTable read FAnchors;
    // Lowercase footnote label to TMarkdownFootnoteBlock.
    property FootnoteDefs : TFPObjectHashTable read FFootnoteDefs;
    // Normalized link label to TMarkdownLinkReference.
    property LinkRefs : TFPObjectHashTable read FLinkRefs;
    // All markers in document order.
    property Markers : TMarkdownMarkerList read FMarkers;
    // The referenced footnote blocks, in the order of their numbers.
    property Footnotes : TMarkdownBlockList read FFootnotes;
    // The source file, used in messages.
    property FileName : String read FFileName write FFileName;
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
    FMarker: TMarkdownString;
    FLoose: boolean;
    FLastIndent: integer;
    FBaseIndent: integer;
    FContentIndent: integer;
    FHasSeenEmptyLine : boolean; // parser state
  public
    function Grace : integer;
    property Ordered : boolean read FOrdered write FOrdered;
    property BaseIndent : integer read FBaseIndent write FBaseIndent;
    property LastIndent : integer read FLastIndent write FLastIndent;
    // Column at which the content of the last item starts: marker indent, marker and the spaces after it.
    property ContentIndent : integer read FContentIndent write FContentIndent;
    property Start : integer read FStart write FStart;
    property Marker : TMarkdownString read FMarker write FMarker;
    property Loose : boolean read FLoose write FLoose;
    property HasSeenEmptyLine : boolean read FHasSeenEmptyLine Write FHasSeenEmptyLine; // parser state
  end;

  TMarkdownListItemBlock = class (TMarkdownParagraphBlock)
  public
    function IsPlainPara : boolean; override;
    function ContentIndentation : Integer; override;
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
    FLang: TMarkdownString;
    FIndent : Integer;
  public
    function WhiteSpaceMode : TWhitespaceMode; override;
    property Fenced : boolean read FFenced write FFenced;
    property Lang : TMarkdownString read FLang write FLang;
    Property Indent : Integer Read FIndent Write FIndent;
  end;


  TMarkdownTableRowBlock = class (TMarkdownContainerBlock);

  TCellAlign = (caLeft, caCenter, caRight);
  TCellAlignArray = array of TCellAlign;

  { TMarkdownTableBlock }

  TMarkdownTableBlock = class (TMarkdownContainerBlock)
  private
    FColumns: TCellAlignArray;
    FCaption: TMarkdownTextNodeList;
    procedure SetCaption(const aValue: TMarkdownTextNodeList);
  public
    destructor Destroy; override;
    property Columns : TCellAlignArray read FColumns Write FColumns;
    // Inline content of the caption, Nil when the table has no caption. The table owns the list.
    property Caption : TMarkdownTextNodeList read FCaption Write SetCaption;
  end;

  TMarkdownLeafBlock = class abstract (TMarkdownBlock);

  TMarkdownThematicBreakBlock = class (TMarkdownLeafBlock)
  end;

  { TMarkdownTextBlock }

  TMarkdownTextBlock = class (TMarkdownLeafBlock)
  private
    FText: TMarkdownString;
    FNodes : TMarkdownTextNodeList;
  protected
    procedure SetClosed(const aValue: boolean);override;
  public
    constructor Create(aParent : TMarkdownBlock; aLine : integer; const aText : TMarkdownString); reintroduce;
    destructor Destroy; override;
    property Text : TMarkdownString read FText write FText;
    property Nodes : TMarkdownTextNodeList Read FNodes Write FNodes;
  end;

  { TMarkdownCommentBlock }

  TMarkdownCommentBlock = class (TMarkdownLeafBlock)
  private
    FMarkerArgument: TMarkdownString;
    FMarkerName: TMarkdownString;
    FMarkerValue: TMarkdownString;
    FText: TMarkdownString;
    procedure SetText(const aValue: TMarkdownString);
  public
    // Is the comment a name: value marker ?
    function IsMarker : Boolean;
    // Text between <!-- and -->. Setting it determines the marker properties.
    property Text : TMarkdownString read FText write SetText;
    // Marker name: index in <!-- index: x -->, empty when the comment is no marker.
    property MarkerName : TMarkdownString read FMarkerName;
    // Marker argument: msgnr in <!-- index[msgnr]: x -->
    property MarkerArgument : TMarkdownString read FMarkerArgument;
    // Marker value: x in <!-- index: x -->
    property MarkerValue : TMarkdownString read FMarkerValue;
  end;

  { TMarkdownDefinitionListBlock }

  TMarkdownDefinitionListBlock = class (TMarkdownContainerBlock)
  private
    FLoose: Boolean;
  public
    // A loose list has blank lines between its parts.
    property Loose : Boolean read FLoose write FLoose;
  end;

  { TMarkdownDefinitionTermBlock }

  TMarkdownDefinitionTermBlock = class (TMarkdownParagraphBlock)
  public
    function IsPlainPara : boolean; override;
  end;

  { TMarkdownDefinitionBlock }

  TMarkdownDefinitionBlock = class (TMarkdownParagraphBlock)
  private
    FContentIndent: Integer;
  public
    function IsPlainPara : boolean; override;
    function ContentIndentation : Integer; override;
    // Column at which the content of the definition starts.
    property ContentIndent : Integer read FContentIndent write FContentIndent;
  end;

  TAlertType = (atNote, atTip, atImportant, atWarning, atCaution);

  { TMarkdownAlertBlock }

  TMarkdownAlertBlock = class (TMarkdownQuoteBlock)
  private
    FAlertType: TAlertType;
  public
    // Kind of alert, from the [!TYPE] marker
    property AlertType : TAlertType read FAlertType write FAlertType;
  end;

  { TMarkdownFootnoteBlock }

  TMarkdownFootnoteBlock = class (TMarkdownContainerBlock)
  private
    FContentIndent: Integer;
    FFootnoteLabel: TMarkdownString;
    FNumber: Integer;
  public
    function ContentIndentation : Integer; override;
    // Label as written in the definition.
    property FootnoteLabel : TMarkdownString read FFootnoteLabel write FFootnoteLabel;
    // Number in order of first reference, 0 when the footnote is not referenced.
    property Number : Integer read FNumber write FNumber;
    // Column at which continuation lines start.
    property ContentIndent : Integer read FContentIndent write FContentIndent;
  end;

  { TMarkdownFigureBlock }

  TMarkdownFigureBlock = class (TMarkdownParagraphBlock)
  private
    FCaption: TMarkdownTextNodeList;
    FImage: TMarkdownTextNode;
    procedure SetCaption(const aValue: TMarkdownTextNodeList);
  public
    destructor Destroy; override;
    function IsPlainPara : boolean; override;
    // The image node, owned by the text block child of the figure.
    property Image : TMarkdownTextNode read FImage write FImage;
    // Inline content of the caption, owned by the figure.
    property Caption : TMarkdownTextNodeList read FCaption write SetCaption;
  end;

const
  AlertTypeNames : Array[TAlertType] of TMarkdownString = ('note','tip','important','warning','caution');

implementation

const
  GrowSize = 10;


{ TMarkdownTextNode }

procedure TMarkdownTextNode.AddText(const s: TMarkdownString);

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

procedure TMarkdownTextNode.AddText(ch: TMarkdownChar);
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
  FreeAndNil(FChildren);
  inherited;
end;

procedure TMarkdownTextNode.AddStyle(aStyle: TNodeStyle);

var
  lChild : TMarkdownTextNode;

begin
  include(FStyles,aStyle);
  Insert(aStyle,FStyleList,0);
  if Assigned(FChildren) then
    for lChild in FChildren do
      lChild.AddStyle(aStyle);
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

function TMarkdownTextNode.GetChildren: TMarkdownTextNodeList;
begin
  if FChildren = nil then
    FChildren:=TMarkdownTextNodeList.Create(True);
  Result:=FChildren;
end;

function TMarkdownTextNode.GetHasAttrs: Boolean;
begin
  Result:=Assigned(FAttrs);
end;

function TMarkdownTextNode.GetHasChildren: Boolean;
begin
  Result:=Assigned(FChildren) and (FChildren.Count>0);
end;

function TMarkdownTextNode.GetText: TMarkdownString;
begin
  Active:=False;
  Result:=FContent;
end;

function TMarkdownTextNode.GetNodeText: TMarkdownString;
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


procedure TMarkdownTextNode.PrependText(const aText : TMarkdownString);

begin
  if aText='' then
    Exit;
  Active:=False;
  Insert(aText,FContent,1);
end;


procedure TMarkdownTextNode.SetNodeText(const aText : TMarkdownString);

begin
  Active:=False;
  FContent:=aText;
end;


procedure TMarkdownTextNode.SetActive(const aValue: boolean);
begin
  if FActive and not aValue then
    FContent:=Copy(FBuild,1,FLength);
  FActive:=aValue;
end;

procedure TMarkdownTextNode.SetStyles(const aValue: TNodeStyles);

var
  S : TNodeStyle;

begin
  FStyles:=aValue;
  SetLength(FStyleList,0);
  for S in TNodeStyle do
    if S in aValue then
      begin
      SetLength(FStyleList,Length(FStyleList)+1);
      FStyleList[High(FStyleList)]:=S;
      end;
end;


procedure TMarkdownTextNode.SetName(const Value: TMarkdownString);
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

function TMarkdownTextNodeList.AddText(aPos: TPosition; const aContent: TMarkdownString): TMarkdownTextNode;
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

function TMarkdownTextNodeList.AddTextNode(aPos: TPosition; aKind: TTextNodeKind; const cnt: TMarkdownString; aDoClose: Boolean
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


function TMarkdownTextNodeList.TextFrom(aNode: TMarkdownTextNode): TMarkdownString;

var
  I : Integer;

begin
  Result:='';
  for I:=IndexOf(aNode) to Count-1 do
    begin
    Elements[I].Active:=False;
    Result:=Result+Elements[I].NodeText;
    end;
end;


procedure TMarkdownTextNodeList.MoveToChildren(aNode: TMarkdownTextNode);

var
  I,lIdx : Integer;

begin
  lIdx:=IndexOf(aNode);
  for I:=lIdx+1 to Count-1 do
    begin
    Elements[I].Active:=False;
    aNode.Children.Add(Elements[I]);
    end;
  for I:=Count-1 downto lIdx+1 do
    Extract(Elements[I]);
end;

function TMarkdownTextNodeList.lastNode: TMarkdownTextNode;
begin
  Result:=Nil;
  if Count>0 then
    Result:=Elements[Count-1];
end;


function TMarkdownTextNodeList.PlainText: TMarkdownString;

var
  lNode : TMarkdownTextNode;
  lAlt : TMarkdownString;

begin
  Result:='';
  for lNode in Self do
    case lNode.Kind of
      nkComment,nkFootnoteRef,nkRaw : ;
      nkLineBreak : Result:=Result+' ';
      nkImg :
        if lNode.HasAttrs and lNode.Attrs.TryGet('alt',lAlt) then
          Result:=Result+lAlt;
    else
      Result:=Result+lNode.NodeText;
      if lNode.HasChildren then
        Result:=Result+lNode.Children.PlainText;
    end;
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


destructor TMarkdownBlock.Destroy;

begin
  FreeAndNil(FAttrs);
  inherited Destroy;
end;


function TMarkdownBlock.GetAttrs: TStrings;

begin
  if FAttrs=Nil then
    FAttrs:=TStringList.Create;
  Result:=FAttrs;
end;


function TMarkdownBlock.GetHasAttrs: Boolean;

begin
  Result:=(FID<>'') or (Length(FClasses)>0) or (Assigned(FAttrs) and (FAttrs.Count>0));
end;


function TMarkdownBlock.PlainText: TMarkdownString;

var
  I : Integer;
  S : TMarkdownString;

begin
  if Self is TMarkdownTextBlock then
    begin
    if Assigned(TMarkdownTextBlock(Self).Nodes) then
      Result:=TMarkdownTextBlock(Self).Nodes.PlainText
    else
      Result:=TMarkdownTextBlock(Self).Text;
    Exit;
    end;
  Result:='';
  for I:=0 to ChildCount-1 do
    begin
    S:=Children[I].PlainText;
    if (Result<>'') and (S<>'') then
      Result:=Result+' ';
    Result:=Result+S;
    end;
end;


function TMarkdownBlock.ApplyAttributes(const aSpec: TMarkdownString): Boolean;

var
  lID : TMarkdownString;
  lClasses : TMarkdownStringArray;
  lAttrs : TStringList;

begin
  lAttrs:=TStringList.Create;
  try
    Result:=ParseAttributeSpec(aSpec,lID,lClasses,lAttrs);
    if not Result then
      Exit;
    if lID<>'' then
      FID:=lID;
    FClasses:=Concat(FClasses,lClasses);
    if lAttrs.Count>0 then
      Attrs.AddStrings(lAttrs);
  finally
    lAttrs.Free;
  end;
end;


function TMarkdownBlock.ContentIndentation: Integer;
begin
  Result:=0;
end;


procedure TMarkdownBlock.dump(const aIndent: TMarkdownString = '');
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

function TMarkdownBlock.GetFirstText: TMarkdownString;
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

function TMarkdownBlockList.LastBlock: TMarkdownBlock;
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


procedure TMarkdownContainerBlock.InsertChild(aIndex: Integer; aChild: TMarkdownBlock);

begin
  if aChild=Nil then
    Raise EMarkdown.CreateFmt('Cannot add nil child to block "%s"',[ClassName]);
  FBlocks.Insert(aIndex,aChild);
  aChild.FParent:=Self;
end;


function TMarkdownContainerBlock.ExtractChild(aIndex: Integer): TMarkdownBlock;

begin
  Result:=FBlocks[aIndex];
  FBlocks.OwnsObjects:=False;
  try
    FBlocks.Delete(aIndex);
  finally
    FBlocks.OwnsObjects:=True;
  end;
  Result.FParent:=Nil;
end;


procedure TMarkdownContainerBlock.ReplaceChild(aOld, aNew: TMarkdownBlock);

var
  lIdx : Integer;

begin
  lIdx:=IndexOfChild(aOld);
  if lIdx<0 then
    Raise EMarkdown.CreateFmt('Block "%s" is not a child of block "%s"',[aOld.ClassName,ClassName]);
  ExtractChild(lIdx).Free;
  InsertChild(lIdx,aNew);
end;


function TMarkdownContainerBlock.IndexOfChild(aChild: TMarkdownBlock): Integer;

begin
  Result:=FBlocks.IndexOf(aChild);
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

function TMarkdownQuoteBlock.IsPlainPara: boolean;
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

function TMarkdownListItemBlock.IsPlainPara: boolean;
begin
  Result:=false;
end;


function TMarkdownListItemBlock.ContentIndentation: Integer;
begin
  if Parent is TMarkdownListBlock then
    Result:=TMarkdownListBlock(Parent).ContentIndent
  else
    Result:=0;
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

{ TMarkdownTextBlock }

procedure TMarkdownTextBlock.SetClosed(const aValue: boolean);
begin
  inherited SetClosed(aValue);
  if assigned(FNodes) then
    FNodes.ClearActive;
end;

constructor TMarkdownTextBlock.Create(aParent: TMarkdownBlock; aLine: integer; const aText: TMarkdownString);
begin
  inherited Create(aParent,aLine);
  FText:=aText;
end;

destructor TMarkdownTextBlock.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

{ TMarkdownTableBlock }

destructor TMarkdownTableBlock.Destroy;

begin
  FreeAndNil(FCaption);
  inherited Destroy;
end;


procedure TMarkdownTableBlock.SetCaption(const aValue: TMarkdownTextNodeList);

begin
  if FCaption=aValue then
    Exit;
  FreeAndNil(FCaption);
  FCaption:=aValue;
end;

{ TMarkdownLinkReference }

constructor TMarkdownLinkReference.Create(const aURL, aTitle: TMarkdownString);

begin
  FURL:=aURL;
  FTitle:=aTitle;
end;

{ TMarkdownMarker }

constructor TMarkdownMarker.Create(const aName, aArgument, aValue: TMarkdownString; aBlock: TMarkdownBlock; aNode: TMarkdownTextNode;
  aNodeIndex: Integer);

begin
  FName:=aName;
  FArgument:=aArgument;
  FValue:=aValue;
  FBlock:=aBlock;
  FNode:=aNode;
  FNodeIndex:=aNodeIndex;
end;

{ TMarkdownDocument }

constructor TMarkdownDocument.Create(aParent: TMarkdownBlock; aLine: Integer);

begin
  inherited Create(aParent, aLine);
  FAnchors:=TFPObjectHashTable.Create(False);
  FFootnoteDefs:=TFPObjectHashTable.Create(False);
  FLinkRefs:=TFPObjectHashTable.Create(True);
  FMarkers:=TMarkdownMarkerList.Create(True);
  FFootnotes:=TMarkdownBlockList.Create(False);
end;


destructor TMarkdownDocument.Destroy;

begin
  FreeAndNil(FFootnotes);
  FreeAndNil(FMarkers);
  FreeAndNil(FLinkRefs);
  FreeAndNil(FFootnoteDefs);
  FreeAndNil(FAnchors);
  inherited Destroy;
end;


function TMarkdownDocument.AddAnchor(const aID: TMarkdownString; aTarget: TMarkdownElement): Boolean;

begin
  Result:=(aID<>'') and (FAnchors.Items[aID]=Nil);
  if Result then
    FAnchors.Add(aID,aTarget);
end;


function TMarkdownDocument.FindAnchor(const aID: TMarkdownString): TMarkdownElement;

begin
  Result:=TMarkdownElement(FAnchors.Items[aID]);
end;


function TMarkdownDocument.AddFootnoteDef(aBlock: TMarkdownFootnoteBlock): Boolean;

var
  lKey : TMarkdownString;

begin
  lKey:=NormalizeLinkLabel(aBlock.FootnoteLabel);
  Result:=(lKey<>'') and (FFootnoteDefs.Items[lKey]=Nil);
  if Result then
    FFootnoteDefs.Add(lKey,aBlock);
end;


function TMarkdownDocument.FindFootnoteDef(const aLabel: TMarkdownString): TMarkdownFootnoteBlock;

begin
  Result:=TMarkdownFootnoteBlock(FFootnoteDefs.Items[NormalizeLinkLabel(aLabel)]);
end;


function TMarkdownDocument.AddLinkRef(const aLabel, aURL, aTitle: TMarkdownString): Boolean;

var
  lKey : TMarkdownString;

begin
  lKey:=NormalizeLinkLabel(aLabel);
  Result:=(lKey<>'') and (FLinkRefs.Items[lKey]=Nil);
  if Result then
    FLinkRefs.Add(lKey,TMarkdownLinkReference.Create(aURL,aTitle));
end;


function TMarkdownDocument.FindLinkRef(const aLabel: TMarkdownString): TMarkdownLinkReference;

begin
  Result:=TMarkdownLinkReference(FLinkRefs.Items[NormalizeLinkLabel(aLabel)]);
end;

{ TMarkdownCommentBlock }

procedure TMarkdownCommentBlock.SetText(const aValue: TMarkdownString);

begin
  FText:=aValue;
  if not ParseMarker(FText,FMarkerName,FMarkerArgument,FMarkerValue) then
    begin
    FMarkerName:='';
    FMarkerArgument:='';
    FMarkerValue:='';
    end;
end;


function TMarkdownCommentBlock.IsMarker: Boolean;

begin
  Result:=FMarkerName<>'';
end;

{ TMarkdownDefinitionTermBlock }

function TMarkdownDefinitionTermBlock.IsPlainPara: boolean;

begin
  Result:=False;
end;

{ TMarkdownDefinitionBlock }

function TMarkdownDefinitionBlock.IsPlainPara: boolean;

begin
  Result:=False;
end;


function TMarkdownDefinitionBlock.ContentIndentation: Integer;

begin
  Result:=FContentIndent;
end;

{ TMarkdownFootnoteBlock }

function TMarkdownFootnoteBlock.ContentIndentation: Integer;

begin
  Result:=FContentIndent;
end;

{ TMarkdownFigureBlock }

destructor TMarkdownFigureBlock.Destroy;

begin
  FreeAndNil(FCaption);
  inherited Destroy;
end;


function TMarkdownFigureBlock.IsPlainPara: boolean;

begin
  Result:=False;
end;


procedure TMarkdownFigureBlock.SetCaption(const aValue: TMarkdownTextNodeList);

begin
  if FCaption=aValue then
    Exit;
  FreeAndNil(FCaption);
  FCaption:=aValue;
end;

end.
