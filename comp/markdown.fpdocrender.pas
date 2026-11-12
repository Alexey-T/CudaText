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
  SectionNodeNames : Array[TSectionType] of TMarkdownString = ('short','descr','errors','examples','seealso');
  ElementNodeNames : Array[TElementType] of TMarkdownString = ('package','module','topic','element');

type
  EFPDocRender = Class(EMarkdown);
  { TMarkdownFPDocRenderer }

  TMarkdownFPDocRenderer = class(TMarkdownRenderer)
  private
    FDocument : TMarkdownDocument;
    FDoc : TXMLDocument;
    FFPDoc: TMarkdownString;
    FPackageName: TMarkdownString;
    FStack: Array[0..100] of TDomElement;
    FStackCount : Integer;
    FSkipParagraph : Boolean;
    function GetParent: TDomElement;
    procedure PushElement(aElement : TDomElement);
    function PopElement : TDomElement;
  Protected
    Procedure AppendText(const aContent : TMarkdownString);
    function Push(const aElementName : TMarkdownString; const aName : TMarkdownString = '') : TDOMElement;
    function PushSection(aSection : TSectionType) : TDomElement;
    function Pop : TDomElement;
    function PopTill(const aElementName : TMarkdownString) : TDomElement;
    function PopTill(const aElementNames : array of TMarkdownString) : TDomElement;
    Property Doc : TXMLDocument Read FDoc;
    Property Parent : TDomElement Read GetParent;
    Property SkipParagraph : Boolean Read FSkipParagraph;
  public
    procedure RenderToXML(aDocument : TMarkdownDocument; aXML : TXMLDocument);
    procedure RenderToStream(aDocument : TMarkdownDocument; aStream : TStream);
    Procedure RenderDocument(aDocument : TMarkdownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings); overload;
    function RenderFPDoc(aDocument : TMarkdownDocument) : TMarkdownString;
    // The document being rendered
    Property Document : TMarkdownDocument read FDocument;
    Property PackageName : TMarkdownString read FPackageName Write FPackageName;
    Property FPDoc : TMarkdownString Read FFPDoc;
  end;

  { TFPDocMarkdownBlockRenderer }

  TFPDocMarkdownBlockRenderer = Class (TMarkdownBlockRenderer)
  Private
    function GetFPDocRenderer: TMarkdownFPDocRenderer;
    function GetParent: TDomElement;
  protected
    procedure CheckParent(const aParent,aChild : TMarkdownString);
    function CheckIsValidName(const aText : TMarkdownString) : boolean;
    function GetSectionType(const aText: TMarkdownString): TSectionType;
    function GetElementType(var aText: TMarkdownString): TElementType;
    procedure Append(const S : TMarkdownString); inline;
    procedure AppendNl(const S : TMarkdownString = ''); inline;
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
    FKeys : Array of TMarkdownString;
    FKeyCount : integer;
    FText : TMarkdownString;
    procedure DoKey(aItem: TMarkdownString; const aKey: TMarkdownString; var aContinue: Boolean);
    procedure EmitStyleDiff(const aStyles: TNodeStyleArray);
    function GetFPDocRenderer: TMarkdownFPDocRenderer;
    function GetNodeTag(aElement: TMarkdownTextNode): TMarkdownString;
    function MustCloseNode(aElement: TMarkdownTextNode): boolean;
  protected
    procedure StartText;
    procedure EndText;
    procedure PushStyle(aStyle : TNodeStyle);
    procedure PopStyle;
    procedure Append(const S : TMarkdownString); inline;
    procedure DoRender(aElement: TMarkdownTextNode); override;
    // Render a link node, after OnResolveLink
    procedure RenderLink(aElement: TMarkdownTextNode); virtual;
    // Render a footnote reference as the note text in parentheses
    procedure RenderFootnoteRef(aElement: TMarkdownTextNode); virtual;
  Public
    procedure BeginBlock; override;
    procedure EndBlock; override;
    property FPDoc : TMarkdownFPDocRenderer Read GetFPDocRenderer;
    function RenderAttrs(aElement: TMarkdownTextNode): TMarkdownString;
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
    procedure DoRender(aElement : TMarkdownBlock); override;
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

  { TFPDocMarkdownCommentBlockRenderer }

  TFPDocMarkdownCommentBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownDefinitionListBlockRenderer }

  // Writes dt and dd alternately: consecutive definitions of one term share a dd.
  TFPDocMarkdownDefinitionListBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure RenderDefinition(aDef : TMarkdownDefinitionBlock; aTight : Boolean);
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownAlertBlockRenderer }

  TFPDocMarkdownAlertBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownFootnoteBlockRenderer }

  TFPDocMarkdownFootnoteBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TFPDocMarkdownFigureBlockRenderer }

  TFPDocMarkdownFigureBlockRenderer = class(TFPDocMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;


implementation

type

  { TStringBuilderHelper }

  TStringBuilderHelper = class helper for TAnsiStringBuilder
    function Append(const aAnsiString : TMarkdownString) : TAnsiStringBuilder;
  end;


function TStringBuilderHelper.Append(const aAnsiString: TMarkdownString): TAnsiStringBuilder;
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

procedure TFPDocMarkdownBlockRenderer.CheckParent(const aParent, aChild: TMarkdownString);
begin
  if (UnicodeToMarkdown(Parent.NodeName)<>aParent) then
    Raise EFPDocRender.CreateFmt('Cannot have %s below %s',[aChild,aParent]);
end;

function TFPDocMarkdownBlockRenderer.CheckIsValidName(const aText: TMarkdownString): boolean;
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

function TFPDocMarkdownBlockRenderer.GetSectionType(const aText: TMarkdownString): TSectionType;
var
  lText : TMarkdownString;
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

function TFPDocMarkdownBlockRenderer.GetElementType(var aText: TMarkdownString): TElementType;
var
  p : integer;
begin
  Result:=etElement;
  aText:=Trim(aText);
  p:=Pos(':',aText);
  if p=0 then
    exit;
  if SameText(MarkdownToString(Copy(aText,1,P-1)),'topic') then
    begin
    Result:=etTopic;
    Delete(aText,1,P);
    aText:=Trim(aText);
    end;
end;

procedure TFPDocMarkdownBlockRenderer.Append(const S: TMarkdownString);
begin
  FPDoc.AppendText(S);
end;

procedure TFPDocMarkdownBlockRenderer.AppendNl(const S: TMarkdownString);
begin
  FPDoc.AppendText(S+#10);
end;



{ TMarkdownFPDocRenderer }

function TMarkdownFPDocRenderer.GetParent: TDomElement;
begin
  if FStackCount>0 then
    Result:=FStack[FStackCount-1]
  else
    Result:=Nil;
end;

function TMarkdownFPDocRenderer.Push(const aElementName: TMarkdownString; const aName: TMarkdownString): TDOMElement;
begin
  Result:=FDoc.CreateElement(MarkdownToUnicode(aElementName));
  PushElement(Result);
  if aName<>'' then
    Result['name']:=MarkdownToUnicode(aName);
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

procedure TMarkdownFPDocRenderer.AppendText(const aContent: TMarkdownString);
begin
  Parent.AppendChild(FDoc.CreateTextNode(MarkdownToUnicode(aContent)));
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

function TMarkdownFPDocRenderer.PopTill(const aElementName: TMarkdownString): TDomElement;
begin
  Result:=PopTill([aElementName]);
end;


function TMarkdownFPDocRenderer.PopTill(const aElementNames: array of TMarkdownString): TDomElement;
begin
  FSkipParagraph:=False;
  While IndexStr(UnicodeToMarkdown(Parent.NodeName),aElementNames)=-1 do
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
  FDocument:=aDocument;
  try
    Push('fpdoc-descriptions');
    Push('package',FPackageName);
    RenderBlock(aDocument);
    Pop;
    Pop;
  finally
    FDocument:=Nil;
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
  S : TBytesStream;
begin
  S:=TBytesStream.Create;
  try
    RenderToStream(aDocument,S);
    FFPDoc:='';
    SetLength(FFPDoc,S.Size);
    if S.Size>0 then
      Move(S.Bytes[0],FFPDoc[1],S.Size);
  finally
    S.Free;
  end;
end;

procedure TMarkdownFPDocRenderer.RenderDocument(aDocument: TMarkdownDocument; aDest: TStrings);
begin
  aDest.Text:=MarkdownToString(RenderFPDoc(aDocument));
end;

function TMarkdownFPDocRenderer.RenderFPDoc(aDocument: TMarkdownDocument): TMarkdownString;
begin
  RenderDocument(aDocument);
  Result:=FFPDoc;
  FFPDoc:='';
end;


procedure TFPDocMarkdownTextRenderer.Append(const S: TMarkdownString);
begin
  FText:=FText+S;
end;

function TFPDocMarkdownTextRenderer.MustCloseNode(aElement: TMarkdownTextNode) : boolean;

begin
  Result:=not (aElement.kind in [nkImg,nkLineBreak,nkRaw]);
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
  StyleNames : Array[TNodeStyle] of TMarkdownString = ('b','i','u');

procedure TFPDocMarkdownTextRenderer.PushStyle(aStyle: TNodeStyle);

begin
  FPDoc.Push(styleNames[aStyle]);
  if FStyleStackLen=Length(FStyleStack) then
    SetLength(FStyleStack,FStyleStackLen+3);
  FStyleStack[FStyleStackLen]:=aStyle;
  Inc(FStyleStackLen);
end;

procedure TFPDocMarkdownTextRenderer.PopStyle;
begin
  if FStyleStackLen=0 then
    Exit;
  Dec(FStyleStackLen);
  FPDoc.Pop;
end;

function TFPDocMarkdownTextRenderer.GetNodeTag(aElement: TMarkdownTextNode) : TMarkdownString;
begin
  case aElement.Kind of
    nkCode: Result:='code';
    nkImg : Result:='img';
    nkLineBreak : Result:='br';
    nkURI,nkEmail : Result:='link'
  else
    Result:='';
  end;
end;

function TFPDocMarkdownTextRenderer.GetFPDocRenderer: TMarkdownFPDocRenderer;
begin
  if Renderer is TMarkdownFPDocRenderer then
    Result:=TMarkdownFPDocRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure TFPDocMarkdownTextRenderer.DoKey(aItem: TMarkdownString; const aKey: TMarkdownString; var aContinue: Boolean);
begin
  aContinue:=True;
  FKeys[FKeyCount]:=aKey;
  inc(FKeyCount);
end;

procedure TFPDocMarkdownTextRenderer.EmitStyleDiff(const aStyles : TNodeStyleArray);

var
  lKeep,I : Integer;

begin
  lKeep:=0;
  While (lKeep<FStyleStackLen) and (lKeep<Length(aStyles)) and (FStyleStack[lKeep]=aStyles[lKeep]) do
    Inc(lKeep);
  While FStyleStackLen>lKeep do
    PopStyle;
  For I:=lKeep to Length(aStyles)-1 do
    PushStyle(aStyles[I]);
end;

procedure TFPDocMarkdownTextRenderer.DoRender(aElement: TMarkdownTextNode);

var
  lChild : TMarkdownTextNode;
  lName : TMarkdownString;

begin
  if aElement.Kind=nkComment then
    begin
    Renderer.DoMarkerNode(aElement);
    Exit;
    end;
  EmitStyleDiff(aElement.StyleList);
  if aElement.Kind=nkFootnoteRef then
    begin
    RenderFootnoteRef(aElement);
    Exit;
    end;
  if aElement.Kind in [nkURI,nkEmail] then
    begin
    RenderLink(aElement);
    Exit;
    end;
  lName:='';
  if aElement.Kind<>nkText then
    begin
    lName:=GetNodeTag(aElement);
    if lName<>'' then
      begin
      FPDoc.Push(lName);
      RenderAttrs(aElement);
      end;
    end;
  if MustCloseNode(aElement) then
    begin
    StartText;
    if aElement.NodeText<>'' then
      Append(aElement.NodeText);
    EndText;
    if aElement.HasChildren then
      begin
      for lChild in aElement.Children do
        DoRender(lChild);
      EmitStyleDiff(aElement.StyleList);
      end;
    end;
  if lName<>'' then
    FPDoc.Pop;
  aElement.Active:=False;
end;

procedure TFPDocMarkdownTextRenderer.RenderLink(aElement: TMarkdownTextNode);

var
  lHref,lText : TMarkdownString;
  lChild : TMarkdownTextNode;

begin
  aElement.Active:=False;
  lHref:=aElement.Attrs['href'];
  lText:=aElement.NodeText;
  if aElement.HasChildren then
    lText:=lText+aElement.Children.PlainText;
  if Renderer.DoResolveLink(lHref,lText) then
    Exit;
  FPDoc.Push('link');
  FPDoc.Parent['id']:=MarkdownToUnicode(lHref);
  if (aElement.NodeText='') and not aElement.HasChildren then
    begin
    if lText='' then
      lText:=lHref;
    FPDoc.AppendText(lText);
    end
  else
    begin
    if aElement.NodeText<>'' then
      FPDoc.AppendText(aElement.NodeText);
    if aElement.HasChildren then
      begin
      for lChild in aElement.Children do
        DoRender(lChild);
      EmitStyleDiff(aElement.StyleList);
      end;
    end;
  FPDoc.Pop;
end;


procedure TFPDocMarkdownTextRenderer.RenderFootnoteRef(aElement: TMarkdownTextNode);

var
  lDef : TMarkdownFootnoteBlock;

begin
  lDef:=Nil;
  if Assigned(FPDoc.Document) then
    lDef:=FPDoc.Document.FindFootnoteDef(aElement.Attrs['label']);
  if Assigned(lDef) then
    FPDoc.AppendText(' ('+lDef.PlainText+')')
  else
    FPDoc.AppendText('[^'+aElement.Attrs['label']+']');
end;


procedure TFPDocMarkdownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  FStyleStackLen:=0;
end;

procedure TFPDocMarkdownTextRenderer.EndBlock;
begin
  While (FStyleStackLen>0) do
    Popstyle;
  inherited EndBlock;
end;

function TFPDocMarkdownTextRenderer.RenderAttrs(aElement: TMarkdownTextNode): TMarkdownString;

  function KeyAlias(const aKey : TMarkdownString): TMarkdownString;

  begin
    case aKey of
      'src' : Result:='file';
      'href' : Result:='id';
      'alt' : Result:='title';
    else
      Result:='';
    end
  end;

  procedure addKey(const aKey,aValue : TMarkdownString);
  var
    lKey : TMarkdownString;
  begin
    lKey:=KeyAlias(aKey);
    if lKey<>'' then
      FPDoc.Parent[MarkdownToUnicode(lKey)]:=MarkdownToUnicode(aValue);
  end;

var
  lKey,lAttr : TMarkdownString;
  lAttrs : THashTable;
  lKeys : Array of TMarkdownString;
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
  lText : TMarkdownString;
  lPushed : Boolean;
begin
  lPushed:=False;
  if lNode.header=0 then
    begin
    lPushed:=not FPDoc.SkipParagraph;
    if lPushed then
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
  if lPushed then
    FPDoc.Pop;
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

procedure TFPDocMarkdownQuoteBlockRenderer.DoRender(aElement: TMarkdownBlock);
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
  lNodeKind : TMarkdownString;
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
    if lBlock<>LNode.Blocks.First then
      AppendNl;
    Renderer.RenderBlock(LBlock);
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
  if Assigned(lNode.Caption) then
    begin
    fpdoc.Push('caption');
    Renderer.RenderTextNodes(lNode.Caption);
    fpdoc.Pop;
    end;
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
  RowTypes : Array[Boolean] of TMarkdownString = ('tr','th');
var
  lNode : TMarkdownTableRowBlock absolute aElement;
  first : boolean;
  i : integer;
begin
  first:=(lNode.parent as TMarkdownContainerBlock).blocks.First = lNode;
  fpDoc.Push(RowTypes[First]);
  for i := 0 to length((lNode.parent as TMarkdownTableBlock).Columns) - 1 do
    begin
    fpDoc.Push('td');
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
  lText : TMarkdownString;
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

{ TFPDocMarkdownCommentBlockRenderer }

procedure TFPDocMarkdownCommentBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownCommentBlock absolute aElement;

begin
  if lNode.IsMarker then
    Renderer.DoMarker(lNode.MarkerName,lNode.MarkerArgument,lNode.MarkerValue);
end;


class function TFPDocMarkdownCommentBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownCommentBlock;
end;

{ TFPDocMarkdownDefinitionListBlockRenderer }

procedure TFPDocMarkdownDefinitionListBlockRenderer.RenderDefinition(aDef: TMarkdownDefinitionBlock; aTight: Boolean);

var
  lBlock : TMarkdownBlock;

begin
  for lBlock in aDef.Blocks do
    if aTight and (lBlock.ClassType=TMarkdownParagraphBlock) and TMarkdownParagraphBlock(lBlock).IsPlainPara then
      FPDoc.RenderChildren(TMarkdownParagraphBlock(lBlock))
    else
      Renderer.RenderBlock(lBlock);
end;


procedure TFPDocMarkdownDefinitionListBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lList : TMarkdownDefinitionListBlock absolute aElement;
  lBlock : TMarkdownBlock;
  lInDD : Boolean;

begin
  FPDoc.Push('dl');
  lInDD:=False;
  for lBlock in lList.Blocks do
    if lBlock is TMarkdownDefinitionTermBlock then
      begin
      if lInDD then
        FPDoc.Pop;
      lInDD:=False;
      FPDoc.Push('dt');
      FPDoc.RenderChildren(TMarkdownDefinitionTermBlock(lBlock));
      FPDoc.Pop;
      end
    else if lBlock is TMarkdownDefinitionBlock then
      begin
      if not lInDD then
        FPDoc.Push('dd');
      lInDD:=True;
      RenderDefinition(TMarkdownDefinitionBlock(lBlock),not lList.Loose);
      end;
  if lInDD then
    FPDoc.Pop;
  FPDoc.Pop;
end;


class function TFPDocMarkdownDefinitionListBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionListBlock;
end;

{ TFPDocMarkdownAlertBlockRenderer }

procedure TFPDocMarkdownAlertBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownAlertBlock absolute aElement;

begin
  if (lNode.AlertType in [atNote,atTip,atImportant]) and (UnicodeToMarkdown(Parent.NodeName)='descr') then
    begin
    FPDoc.Push('remark');
    Renderer.RenderChildren(lNode);
    FPDoc.Pop;
    end
  else
    begin
    FPDoc.Push('p');
    FPDoc.Push('b');
    FPDoc.AppendText(Renderer.AlertTitles[lNode.AlertType]+':');
    FPDoc.Pop;
    FPDoc.Pop;
    Renderer.RenderChildren(lNode);
    end;
end;


class function TFPDocMarkdownAlertBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownAlertBlock;
end;

{ TFPDocMarkdownFootnoteBlockRenderer }

procedure TFPDocMarkdownFootnoteBlockRenderer.DoRender(aElement: TMarkdownBlock);

begin
  if aElement=Nil then ; // Silence warning
end;


class function TFPDocMarkdownFootnoteBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownFootnoteBlock;
end;

{ TFPDocMarkdownFigureBlockRenderer }

procedure TFPDocMarkdownFigureBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownFigureBlock absolute aElement;
  lImg : TDOMElement;

begin
  if not Assigned(lNode.Image) then
    Exit;
  lImg:=FPDoc.Push('img');
  lImg['file']:=MarkdownToUnicode(lNode.Image.Attrs['src']);
  if Assigned(lNode.Caption) and (lNode.Caption.Count>0) then
    lImg['caption']:=MarkdownToUnicode(lNode.Caption.PlainText);
  if lNode.ID<>'' then
    lImg['name']:=MarkdownToUnicode(lNode.ID);
  FPDoc.Pop;
end;


class function TFPDocMarkdownFigureBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownFigureBlock;
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
  TFPDocMarkdownCommentBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownDefinitionListBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownAlertBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownFootnoteBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownFigureBlockRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
  TFPDocMarkdownTextRenderer.RegisterRenderer(TMarkdownFPDocRenderer);
end.

