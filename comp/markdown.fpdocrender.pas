{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown FPDoc input file renderer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.FPDocRender;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.StrUtils, System.Contnrs, Xml.Dom, Xml.Writer,
{$ELSE}
  Classes, SysUtils, strutils, contnrs, dom, XMLWrite,
{$ENDIF}
  Markdown.Elements, Markdown.Render, Markdown.Utils;

type
  TElementType = (etPackage, etModule, etTopic, etElement);
  TSectionType = (stShort,stDescription,stErrors,stExamples,stSeeAlso);

const
  SectionNodeNames : Array[TSectionType] of string = ('short','descr','errors','examples','seealso');
  ElementNodeNames : Array[TElementType] of string = ('package','module','topic','element');

type
  EFPDocRender = Class(EMarkdown);
  { TMarkdownFPDocRenderer }

  TMarkdownFPDocRenderer = class(TMarkdownRenderer)
  private
    FDoc : TXMLDocument;
    FFPDoc: String;
    FPackageName: String;
    FStack: Array[0..100] of TDomElement;
    FStackCount : Integer;
    FSkipParagraph : Boolean;
    function GetParent: TDomElement;
    procedure PushElement(aElement : TDomElement);
    function PopElement : TDomElement;
  Protected
    Procedure AppendText(const aContent : String);
    function Push(const aElementName : String; const aName : string = '') : TDOMElement;
    function PushSection(aSection : TSectionType) : TDomElement;
    function Pop : TDomElement;
    function PopTill(const aElementName : string) : TDomElement;
    function PopTill(const aElementNames : array of string) : TDomElement;
    Property Doc : TXMLDocument Read FDoc;
    Property Parent : TDomElement Read GetParent;
    Property SkipParagraph : Boolean Read FSkipParagraph;
  public
    procedure RenderToXML(aDocument : TMarkdownDocument; aXML : TXMLDocument);
    procedure RenderToStream(aDocument : TMarkdownDocument; aStream : TStream);
    Procedure RenderDocument(aDocument : TMarkdownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings); overload;
    function RenderFPDoc(aDocument : TMarkdownDocument) : string;
    Property PackageName : String read FPackageName Write FPackageName;
    Property FPDoc : String Read FFPDoc;
  end;

  { TFPDocMarkdownBlockRenderer }

  TFPDocMarkdownBlockRenderer = Class (TMarkdownBlockRenderer)
  Private
    function GetFPDocRenderer: TMarkdownFPDocRenderer;
    function GetParent: TDomElement;
  protected
    procedure CheckParent(const aParent,aChild : String);
    function CheckIsValidName(const aText : string) : boolean;
    function GetSectionType(const aText: string): TSectionType;
    function GetElementType(var aText: string): TElementType;
    procedure Append(const S : String); inline;
    procedure AppendNl(const S : String = ''); inline;
  public
    property FPDoc : TMarkdownFPDocRenderer Read GetFPDocRenderer;
    Property Parent : TDomElement Read GetParent;
  end;
  TFPDocMarkdownBlockRendererClass = class of TFPDocMarkdownBlockRenderer;
  { TFPDocMarkdownTextRenderer }

  TFPDocMarkdownTextRenderer = class(TMarkdownTextRenderer)
  Private
    FStyleStack: Array of TNodeStyle;
    FStyleStackLen : Integer;
    FLastStyles : TNodeStyles;
    FKeys : Array of String;
    FKeyCount : integer;
    FText : String;
    procedure DoKey(aItem: AnsiString; const aKey: AnsiString; var aContinue: Boolean);
    procedure EmitStyleDiff(aStyles: TNodeStyles);
    function GetFPDocRenderer: TMarkdownFPDocRenderer;
    function GetNodeTag(aElement: TMarkdownTextNode): string;
    function MustCloseNode(aElement: TMarkdownTextNode): boolean;
  protected
    procedure StartText;
    procedure EndText;
    procedure PushStyle(aStyle : TNodeStyle);
    function PopStyles(aStyle: TNodeStyles): TNodeStyle;
    procedure PopStyle(aStyle : TNodeStyle);
    procedure Append(const S : String); inline;
    procedure DoRender(aElement: TMarkdownTextNode); override;
  Public
    procedure BeginBlock; override;
    procedure EndBlock; override;
    property FPDoc : TMarkdownFPDocRenderer Read GetFPDocRenderer;
    function renderAttrs(aElement: TMarkdownTextNode): AnsiString;
  end;
  TFPDocMarkdownTextRendererClass = class of TFPDocMarkdownTextRenderer;

  { TFPDocParagraphBlockRenderer }

  TFPDocParagraphBlockRenderer = class (TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownQuoteBlockRenderer }

  TFPDocMarkdownQuoteBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure dorender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownTextBlockRenderer }

  TFPDocMarkdownTextBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownListBlockRenderer }

  TFPDocMarkdownListBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownListItemBlockRenderer }

  TFPDocMarkdownListItemBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownCodeBlockRenderer }

  TFPDocMarkdownCodeBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownHeadingBlockRenderer }

  TFPDocMarkdownHeadingBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownThematicBreakBlockRenderer }

  TFPDocMarkdownThematicBreakBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownTableBlockRenderer }

  TFPDocMarkdownTableBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TMarkdownTableRowBlockRenderer }

  TFPDocMarkdownTableRowBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownFrontmatterBlockRenderer }

  TFPDocMarkdownFrontmatterBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownDocumentRenderer }

  TFPDocMarkdownDocumentRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;


implementation

type

  { TStringBuilderHelper }

  TStringBuilderHelper = class helper for TAnsiStringBuilder
    function Append(const aAnsiString : Ansistring) : TAnsiStringBuilder;
  end;


function TStringBuilderHelper.Append(const aAnsiString: Ansistring): TAnsiStringBuilder;
begin
  Result:=Inherited Append(aAnsiString,0,System.Length(aAnsistring))
end;

{ TMarkdownBlockRenderer }

function TFPDocMarkdownBlockRenderer.GetFPDocRenderer: TMarkdownFPDocRenderer;
begin
  if Renderer is TMarkdownFPDocRenderer then
    Result:=TMarkdownFPDocRenderer(Renderer)
  else
    Result:=Nil;
end;

function TFPDocMarkdownBlockRenderer.GetParent: TDomElement;
begin
  Result:=FPDoc.Parent;
end;

procedure TFPDocMarkdownBlockRenderer.CheckParent(const aParent, aChild: String);
begin
  if (UTF8Encode(Parent.NodeName)<>aParent) then
    Raise EFPDocRender.CreateFmt('Cannot have %s below %s',[aChild,aParent]);
end;

function TFPDocMarkdownBlockRenderer.CheckIsValidName(const aText: string): boolean;
const
  StartIdentChars = ['a'..'z','A'..'Z','_'];
  AllIdentChars = StartIdentChars+['.','_','0'..'9'];
var
  I,Len : integer;
begin
  len:=Length(aText);
  Result:=(Len>0) and (aText[1] in StartIdentChars);
  I:=2;
  While Result and (I<=Len) do
    begin
    Result:=aText[I] in AllIdentChars;
    inc(i);
    end;
end;

function TFPDocMarkdownBlockRenderer.GetSectionType(const aText: string): TSectionType;
var
  lText : string;
begin
  lText:=LowerCase(Trim(aText));
  case lText of
    'short' : result:=stShort;
    'descr',
    'description' : result:=stDescription;
    'errors' : Result:=stErrors;
    'example',
    'examples' : Result:=stExamples;
    'seealso': Result:=stSeeAlso;
  else
    result:=stDescription;
  end;
end;

function TFPDocMarkdownBlockRenderer.GetElementType(var aText: string): TElementType;
var
  p : integer;
begin
  Result:=etElement;
  aText:=Trim(aText);
  p:=Pos(':',aText);
  if p=0 then
    exit;
  if SameText(Copy(aText,1,P-1),'topic') then
    begin
    Result:=etTopic;
    Delete(aText,1,P);
    aText:=Trim(aText);
    end;
end;

procedure TFPDocMarkdownBlockRenderer.Append(const S: String);
begin
  FPDoc.AppendText(S);
end;

procedure TFPDocMarkdownBlockRenderer.AppendNl(const S: String);
begin
  FPDoc.AppendText(S);
end;



{ TMarkdownFPDocRenderer }

function TMarkdownFPDocRenderer.GetParent: TDomElement;
begin
  if FStackCount>0 then
    Result:=FStack[FStackCount-1]
  else
    Result:=Nil;
end;

function TMarkdownFPDocRenderer.Push(const aElementName: String; const aName: string): TDOMElement;
begin
  Result:=FDoc.CreateElement(UTF8Decode(aElementName));
  PushElement(Result);
  if aName<>'' then
    Result['name']:=UTF8Decode(aName);
end;

procedure TMarkdownFPDocRenderer.PushElement(aElement: TDomElement);
begin
  if FStackCount=Length(FStack) then
    Raise EFPDocRender.Create('Max stack size reached');
  if FStackCount=0 then
    FDoc.AppendChild(aElement)
  else
    Parent.AppendChild(aElement);
  FStack[FStackCount]:=aElement;
  Inc(FStackCount);
end;

function TMarkdownFPDocRenderer.PopElement: TDomElement;
begin
  if FStackCount>0 then
    begin
    Result:=FStack[FStackCount-1];
    Dec(FStackCount);
    end
  else
    Result:=Nil;
end;

procedure TMarkdownFPDocRenderer.AppendText(const aContent: String);
begin
  Parent.AppendChild(FDoc.CreateTextNode(UTF8Decode(aContent)));
end;

function TMarkdownFPDocRenderer.PushSection(aSection: TSectionType): TDomElement;
begin
  Result:=Push(SectionNodeNames[aSection]);
  FSkipParagraph:=aSection in [stShort,stSeeAlso];
end;

function TMarkdownFPDocRenderer.Pop: TDomElement;
begin
  Result:=PopElement;
end;

function TMarkdownFPDocRenderer.PopTill(const aElementName: string): TDomElement;
begin
  Result:=PopTill([aElementName]);
end;


function TMarkdownFPDocRenderer.PopTill(const aElementNames: array of string): TDomElement;
begin
  FSkipParagraph:=False;
  While IndexStr(UTF8Encode(Parent.NodeName),aElementNames)=-1 do
    begin
    Pop;
    if Parent=Nil then
      Raise EFPDocRender.CreateFmt('Could not pop to %s',[aElementNames[0]]);
    end;
  Result:=Parent;
end;

procedure TMarkdownFPDocRenderer.RenderToXML(aDocument: TMarkdownDocument; aXML: TXMLDocument);
begin
  FDoc:=aXML;
  try
    Push('fpdoc-descriptions');
    Push('package',FPackageName);
    RenderBlock(aDocument);
    Pop;
    Pop;
  finally
    FDoc:=Nil;
  end;
end;


procedure TMarkdownFPDocRenderer.RenderToStream(aDocument: TMarkdownDocument; aStream: TStream);
var
  lDoc : TXMLDocument;
begin
  LDoc:=TXMLDocument.Create;
  try
    RenderToXML(aDocument,LDoc);
    WriteXML(LDoc,aStream);
  finally
    FreeAndNil(LDoc);
  end;
end;

procedure TMarkdownFPDocRenderer.RenderDocument(aDocument: TMarkdownDocument);
var
  S : TStringStream;
begin
  S:=TStringStream.Create('');
  try
    RenderToStream(aDocument,S);
    FFPDoc:=S.DataString;
  finally
    S.Free;
  end;
end;

procedure TMarkdownFPDocRenderer.RenderDocument(aDocument: TMarkdownDocument; aDest: TStrings);
begin
  aDest.Text:=RenderFPDoc(aDocument);
end;

function TMarkdownFPDocRenderer.RenderFPDoc(aDocument: TMarkdownDocument): string;
begin
  RenderDocument(aDocument);
  Result:=FFPDoc;
  FFPDoc:='';
end;


procedure TFPDocMarkdownTextRenderer.Append(const S: String);
begin
  FText:=FText+S;
end;

function TFPDocMarkdownTextRenderer.MustCloseNode(aElement: TMarkdownTextNode) : boolean;

begin
  Result:=aElement.kind<>nkImg;
end;

procedure TFPDocMarkdownTextRenderer.StartText;
begin
  FText:='';
end;

procedure TFPDocMarkdownTextRenderer.EndText;
begin
  FPDoc.AppendText(FText);
  FText:='';
end;

const
  StyleNames : Array[TNodeStyle] of string = ('b','i','u');

procedure TFPDocMarkdownTextRenderer.PushStyle(aStyle: TNodeStyle);

begin
  FPDoc.Push(styleNames[aStyle]);
  if FStyleStackLen=Length(FStyleStack) then
    SetLength(FStyleStack,FStyleStackLen+3);
  FStyleStack[FStyleStackLen]:=aStyle;
  Inc(FStyleStackLen);
end;

function TFPDocMarkdownTextRenderer.PopStyles(aStyle: TNodeStyles): TNodeStyle;

begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1] in aStyle) then
    begin
    Result:=FStyleStack[FStyleStackLen-1];
    FPDoc.Pop;
    Dec(FStyleStackLen);
    end;
end;

procedure TFPDocMarkdownTextRenderer.PopStyle(aStyle: TNodeStyle);
begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1]=aStyle) then
    begin
    FPDoc.Pop;
    Dec(FStyleStackLen);
    end;
end;

function TFPDocMarkdownTextRenderer.GetNodeTag(aElement: TMarkdownTextNode) : string;
begin
  case aElement.Kind of
    nkCode: Result:='code';
    nkImg : Result:='img';
    nkURI,nkEmail : Result:='link'
  end;
end;

function TFPDocMarkdownTextRenderer.GetFPDocRenderer: TMarkdownFPDocRenderer;
begin
  if Renderer is TMarkdownFPDocRenderer then
    Result:=TMarkdownFPDocRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure TFPDocMarkdownTextRenderer.DoKey(aItem: AnsiString; const aKey: Ansistring; var aContinue: Boolean);
begin
  aContinue:=True;
  FKeys[FKeyCount]:=aKey;
  inc(FKeyCount);
end;

procedure TFPDocMarkdownTextRenderer.EmitStyleDiff(aStyles : TNodeStyles);

var
  lRemove : TNodeStyles;
  lAdd : TNodeStyles;
  S : TNodeStyle;

begin
  lRemove:=[];
  lAdd:=[];
  For S in TNodeStyle do
    begin
    if (S in FLastStyles) and Not (S in aStyles) then
      Include(lRemove,S);
    if (S in aStyles) and Not (S in FLastStyles) then
      Include(lAdd,S);
    end;
  While lRemove<>[] do
    begin
    S:=PopStyles(lRemove);
    Exclude(lRemove,S);
    end;
  For S in TNodeStyle do
    if S in lAdd then
      PushStyle(S);
  FLastStyles:=aStyles;
end;

procedure TFPDocMarkdownTextRenderer.DoRender(aElement: TMarkdownTextNode);

begin
  EmitStyleDiff(aElement.Styles);
  if aElement.Kind<>nkText then
    begin
    FPDoc.Push(GetNodeTag(aElement));
    RenderAttrs(aElement);
    end;
  StartText;
  if aElement.NodeText<>'' then
    Append(aElement.NodeText);
  EndText;
  if aElement.Kind<>nkText then
    FPDoc.Pop;
  aElement.Active:=False;
end;

procedure TFPDocMarkdownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  FStyleStackLen:=0;
  FLastStyles:=[];
end;

procedure TFPDocMarkdownTextRenderer.EndBlock;
begin
  While (FStyleStackLen>0) do
    Popstyle(FStyleStack[FStyleStackLen-1]);
  FLastStyles:=[];
  inherited EndBlock;
end;

function TFPDocMarkdownTextRenderer.renderAttrs(aElement: TMarkdownTextNode): AnsiString;

  function KeyAlias(const aKey : string): string;

  begin
    case aKey of
      'src' : Result:='file';
      'href' : Result:='id';
      'alt' : Result:='title';
    else
      Result:='';
    end
  end;

  procedure addKey(const aKey,aValue : String);
  var
    lKey : String;
  begin
    lKey:=KeyAlias(aKey);
    if lKey<>'' then
      FPDoc.Parent[UTF8Decode(lKey)]:=UTF8Decode(aValue);
  end;

var
  lKey,lAttr : String;
  lAttrs : THashTable;
  lKeys : Array of string;
begin
  result := '';
  if not Assigned(aElement.Attrs) then
    exit;
  lAttrs:=aElement.Attrs;
  // First the known keys
  lKeys:=['src','alt','href','title'];
  for lKey in lKeys do
    if lAttrs.TryGet(lKey,lAttr) then
      AddKey(lKey,lAttr);
  // Then the other keys
  SetLength(FKeys,lAttrs.Count);
  FKeyCount:=0;
  lAttrs.Iterate(@DoKey);
  for lKey in FKeys do
    if IndexStr(lKey,['src','alt','href','title'])=-1 then
      AddKey(lKey,lAttrs[lKey]);
end;

procedure TFPDocParagraphBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownParagraphBlock absolute aElement;
  et : TElementType;
  st : TSectionType;
  lText : string;
begin
  if lNode.header=0 then
    begin
    if not FPDoc.SkipParagraph then
      FPDoc.Push('p');
    end
  else
    begin
    lText:=Trim(lNode.GetFirstText);
    Case lNode.header of
    1:
      begin
      fpDoc.PopTill('package');
      CheckIsValidName(lText);
      FPDoc.Push(ElementNodeNames[etModule],lText);
      end;
    2:
      begin
      et:=GetElementType(lText);
      CheckIsValidName(lText);
      fpDoc.PopTill('module');
      FPDoc.Push(ElementNodeNames[et]);
      end;
    3:
      begin
      st:=GetSectionType(lText);
      FPDoc.PushSection(st);
      end;
    end;
    end;
  Renderer.RenderChildren(lNode);
end;

class function TFPDocParagraphBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownParagraphBlock;
end;

class function TFPDocMarkdownTextBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTextBlock;
end;

procedure TFPDocMarkdownTextBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownTextBlock absolute aElement;
begin
  if assigned(lNode) and assigned(lNode.Nodes) then
    Renderer.RenderTextNodes(lNode.Nodes);
end;

procedure TFPDocMarkdownQuoteBlockRenderer.dorender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownQuoteBlock absolute aElement;

begin
  CheckParent('descr','remark');
  fpDoc.Push('remark');
  Renderer.RenderChildren(lNode);
  fpDoc.Pop;
end;

class function TFPDocMarkdownQuoteBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownQuoteBlock;
end;

procedure TFPDocMarkdownListBlockRenderer.DoRender(aElement : TMarkdownBlock);

var
  lNode : TMarkdownListBlock absolute aElement;
  lNodeKind : String;
begin
  if not lNode.Ordered then
    lNodeKind:='ul'
  else
    lNodeKind:='ol';
  FPDoc.Push(lNodeKind);
  Renderer.RenderChildren(lNode);
  FPDoc.Pop;
end;

class function TFPDocMarkdownListBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownListBlock;
end;


procedure TFPDocMarkdownListItemBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lItemBlock : TMarkdownListItemBlock absolute aElement;
  lBlock : TMarkdownBlock;
  lPar : TMarkdownParagraphBlock absolute lBlock;

  function IsPlainBlock(aBlock : TMarkdownBlock) : boolean;
  begin
    Result:=(aBlock is TMarkdownParagraphBlock)
             and (aBlock as TMarkdownParagraphBlock).isPlainPara
             and not (lItemblock.parent as TMarkdownListBlock).loose
  end;


begin
  fpDoc.Push('li');
  For lBlock in lItemBlock.Blocks do
    if IsPlainBlock(lBlock) then
      FPDoc.RenderChildren(lPar)
    else
      Renderer.RenderBlock(lBlock);
  fpDoc.Pop;
end;

class function TFPDocMarkdownListItemBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownListItemBlock;
end;

procedure TFPDocMarkdownCodeBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lNode : TMarkdownCodeBlock absolute aElement;
  lBlock : TMarkdownBlock;

begin
  FPDoc.Push('code');
  for lBlock in LNode.Blocks do
    begin
    Renderer.RenderBlock(LBlock);
    AppendNl;
    end;
  FPDoc.Pop;
end;

class function TFPDocMarkdownCodeBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownCodeBlock;
end;

procedure TFPDocMarkdownThematicBreakBlockRenderer.DoRender(aElement : TMarkdownBlock);

begin
  if Not Assigned(aElement) then;
end;

class function TFPDocMarkdownThematicBreakBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownThematicBreakBlock;
end;

{ TMarkdownTableBlock }

procedure TFPDocMarkdownTableBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownTableBlock absolute aElement;
  i : integer;
begin
  fpdoc.Push('table');
  Renderer.RenderBlock(lNode.blocks[0]);
  if lNode.blocks.Count > 1 then
  begin
    for i := 1 to lNode.blocks.Count -1  do
      Renderer.RenderBlock(lnode.blocks[i]);
  end;
  fpDoc.Pop;
end;

class function TFPDocMarkdownTableBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableBlock;
end;

{ TFPDocMarkdownFrontmatterBlockRenderer }

procedure TFPDocMarkdownFrontmatterBlockRenderer.DoRender(aElement: TMarkdownBlock);
begin
  // Frontmatter produces no visible output
end;

class function TFPDocMarkdownFrontmatterBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result := TMarkdownFrontmatterBlock;
end;

{ TFPDocMarkdownDocumentRenderer }

procedure TFPDocMarkdownDocumentRenderer.DoRender(aElement: TMarkdownBlock);

begin
  Renderer.RenderChildren(aElement as TMarkdownDocument);
end;

class function TFPDocMarkdownDocumentRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownDocument
end;

{ TMarkdownTableRowBlock }

procedure TFPDocMarkdownTableRowBlockRenderer.DoRender(aElement : TMarkdownBlock);
const
  CellTypes : Array[Boolean] of string = ('td','th'); //
var
  lNode : TMarkdownTableRowBlock absolute aElement;
  first : boolean;
  i : integer;
  cType : String;
begin
  first:=(lNode.parent as TMarkdownContainerBlock).blocks.First = self;
  cType:=Celltypes[First];
  fpDoc.Push('tr');
  for i := 0 to length((lNode.parent as TMarkdownTableBlock).Columns) - 1 do
    begin
    fpDoc.Push(cType);
    if i < lNode.blocks.Count then
      Renderer.RenderBlock(lNode.blocks[i]);
    fpDoc.Pop;
    end;
  fpDoc.Pop;
end;

class function TFPDocMarkdownTableRowBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableRowBlock;
end;

procedure TFPDocMarkdownHeadingBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lNode : TMarkdownHeadingBlock absolute aElement;
  lText : String;
  et : TElementType;
  st : TSectionType;
begin
  lText:=Trim(lNode.GetFirstText);
  Case lNode.Level of
  1:
    begin
    fpDoc.PopTill('package');
    CheckIsValidName(lText);
    FPDoc.Push(ElementNodeNames[etModule],lText);
    end;
  2:
    begin
    et:=GetElementType(lText);
    CheckIsValidName(lText);
    fpDoc.PopTill('module');
    FPDoc.Push(ElementNodeNames[et],lText);
    end;
  3:
    begin
    fpDoc.PopTill('element');
    st:=GetSectionType(lText);
    FPDoc.PushSection(st);
    end;
  end;
  // Renderer.RenderChildren(lNode);
end;

class function TFPDocMarkdownHeadingBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownHeadingBlock;
end;

initialization
  TFPDocMarkdownHeadingBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocParagraphBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownQuoteBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownTextBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownListBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownListItemBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownCodeBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownHeadingBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownThematicBreakBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownTableBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownTableRowBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownFrontmatterBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownDocumentRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownTextRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
end.

