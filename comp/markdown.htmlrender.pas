{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown HTML renderer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.HtmlRender;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.StrUtils, System.Contnrs,
{$ELSE}
  Classes, SysUtils, strutils, contnrs,
{$ENDIF}
  Markdown.Elements,
  Markdown.Render,
  Markdown.Utils;

type
  { TMarkdownHTMLRenderer }
  THTMLOption = (hoEnvelope,hoHead);
  THTMLOptions = set of THTMLOption;

  TMarkdownHTMLRenderer = class;
  THTMLMarkdownTextRenderer = class;

  { THTMLDirectiveRenderer }

  // Writes directives. The default writes a div or span with a data-directive attribute.
  THTMLDirectiveRenderer = class(TObject)
  private
    FRenderer: TMarkdownHTMLRenderer;
    FKeys: TStringList;
    procedure CollectKey(aItem: TMarkdownString; const aKey: TMarkdownString; var aContinue: Boolean);
  protected
    procedure Append(const aContent : TMarkdownString);
    procedure AppendNL(const aContent : TMarkdownString = '');
    // data-directive, id and class attributes, and key=value attributes as data-key, each preceded by a space.
    function DirectiveAttributes(const aName, aID, aClasses : TMarkdownString; aAttrs : TStrings) : TMarkdownString;
    // DirectiveAttributes for a container or leaf directive.
    function BlockAttributes(aBlock : TMarkdownDirectiveBlock) : TMarkdownString;
    // DirectiveAttributes for an inline directive.
    function InlineAttributes(aNode : TMarkdownTextNode) : TMarkdownString;
  public
    constructor Create(aRenderer : TMarkdownHTMLRenderer); virtual;
    // Write a container or leaf directive.
    procedure RenderBlock(aBlock : TMarkdownDirectiveBlock); virtual;
    // Write an inline directive. aTextRenderer writes the label nodes. A directive without label and attributes is written as text.
    procedure RenderInline(aTextRenderer : THTMLMarkdownTextRenderer; aNode : TMarkdownTextNode); virtual;
    // The renderer that writes the document.
    property Renderer : TMarkdownHTMLRenderer read FRenderer;
  end;
  THTMLDirectiveRendererClass = class of THTMLDirectiveRenderer;

  TMarkdownHTMLRenderer = class(TMarkdownRenderer)
  private
    FBuilder: TMarkdownStringBuilder;
    FDirectiveRenderers: TStringList;
    FHead: TStrings;
    FHTML: TMarkdownString;
    FOptions: THTMLOptions;
    FTitle: TMarkdownString;
    procedure SetHead(const aValue: TStrings);
  Protected
    Procedure Append(const aContent : TMarkdownString);
    Procedure AppendNL(const aContent : TMarkdownString = '');
    Property Builder : TMarkdownStringBuilder Read FBuilder;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    Procedure RenderDocument(aDocument : TMarkdownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings); overload;
    procedure RenderChildren(aBlock : TMarkdownContainerBlock; aAppendNewLine : Boolean); overload;
    function RenderHTML(aDocument : TMarkdownDocument) : TMarkdownString;
    procedure RenderHTMLToFile(aDocument : TMarkdownDocument; const aFileName : String);
    class function FastRender(aDocument : TMarkdownDocument; aOptions : THTMLOptions; const aTitle : TMarkdownString = ''; aHead : TStrings = Nil) : TMarkdownString;
    class procedure FastRenderToFile(aDocument : TMarkdownDocument; const aFileName : String; aOptions : THTMLOptions; const aTitle : TMarkdownString = ''; aHead : TStrings = Nil);
    // Write aText to the output as-is.
    procedure WriteRaw(const aText : TMarkdownString); override;
    // HTML attributes for the id, classes and key=value attributes of aBlock, each preceded by a space.
    function BlockAttributes(aBlock : TMarkdownBlock; aKeyValues : Boolean = True) : TMarkdownString;
    // Render the footnotes referenced in aDocument as a section at the end.
    procedure RenderFootnotes(aDocument : TMarkdownDocument); virtual;
    // Write directives named aName with aClass. Nil restores the default rendering.
    procedure RegisterDirectiveRenderer(const aName : TMarkdownString; aClass : THTMLDirectiveRendererClass);
    // The class that writes directives named aName: the registered one, or THTMLDirectiveRenderer.
    function FindDirectiveRenderer(const aName : TMarkdownString) : THTMLDirectiveRendererClass;
    Property HTML : TMarkdownString Read FHTML;
  published
    Property Options : THTMLOptions Read FOptions Write FOptions;
    property Title : TMarkdownString Read FTitle Write FTitle;
    property Head : TStrings Read FHead Write SetHead;
  end;

  { THTMLMarkdownBlockRenderer }

  THTMLMarkdownBlockRenderer = Class (TMarkdownBlockRenderer)
  Private
    function GetHTMLRenderer: TMarkdownHTMLRenderer;
  protected
    procedure Append(const S : TMarkdownString); inline;
    procedure AppendNl(const S : TMarkdownString = ''); inline;
    function HasOption(aOption : THTMLOption) : Boolean;
  public
    property HTMLRenderer : TMarkdownHTMLRenderer Read GetHTMLRenderer;
  end;
  THTMLMarkdownBlockRendererClass = class of THTMLMarkdownBlockRenderer;
  { THTMLMarkdownTextRenderer }

  THTMLMarkdownTextRenderer = class(TMarkdownTextRenderer)
  Private
    FStyleStack: Array of TNodeStyle;
    FStyleStackLen : Integer;
    FKeys : Array of TMarkdownString;
    FKeyCount : integer;
    procedure DoKey(aItem: TMarkdownString; const aKey: TMarkdownString; var aContinue: Boolean);
    procedure EmitStyleDiff(const aStyles: TNodeStyleArray);
    function GetHTMLRenderer: TMarkdownHTMLRenderer;
    function GetNodeTag(aElement: TMarkdownTextNode): TMarkdownString;
    function MustCloseNode(aElement: TMarkdownTextNode): boolean;
  protected
    procedure PushStyle(aStyle : TNodeStyle);
    procedure PopStyle;
    procedure Append(const S : TMarkdownString); inline;
    procedure DoRender(aElement: TMarkdownTextNode); override;
    // Render a link node, after OnResolveLink
    procedure RenderLink(aElement: TMarkdownTextNode); virtual;
    // Render a footnote reference as a superscript link
    procedure RenderFootnoteRef(aElement: TMarkdownTextNode); virtual;
    // Render a wiki link, after OnResolveWikiLink. A link to a page that does not exist gets class missing by default.
    procedure RenderWikiLink(aElement: TMarkdownTextNode); virtual;
    // Render an inline directive with the directive renderer registered for its name
    procedure RenderDirective(aElement: TMarkdownTextNode); virtual;
  Public
    procedure BeginBlock; override;
    procedure EndBlock; override;
    // Render the children of aElement
    procedure RenderChildren(aElement: TMarkdownTextNode);
    property HTMLRenderer : TMarkdownHTMLRenderer Read GetHTMLRenderer;
    function RenderAttrs(aElement: TMarkdownTextNode): TMarkdownString;
  end;
  THTMLMarkdownTextRendererClass = class of THTMLMarkdownTextRenderer;

  { THTMLParagraphBlockRenderer }

  THTMLParagraphBlockRenderer = class (THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownQuoteBlockRenderer }

  THTMLMarkdownQuoteBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownTextBlockRenderer }

  THTMLMarkdownTextBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownListBlockRenderer }

  THTMLMarkdownListBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownListItemBlockRenderer }

  THTMLMarkdownListItemBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownCodeBlockRenderer }

  THTMLMarkdownCodeBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownHeadingBlockRenderer }

  THTMLMarkdownHeadingBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownThematicBreakBlockRenderer }

  THTMLMarkdownThematicBreakBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownTableBlockRenderer }

  THTMLMarkdownTableBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TMarkdownTableRowBlockRenderer }

  THTMLMarkdownTableRowBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownFrontmatterBlockRenderer }

  THTMLMarkdownFrontmatterBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownDocumentRenderer }

  THTMLMarkdownDocumentRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownCommentBlockRenderer }

  THTMLMarkdownCommentBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownDefinitionListBlockRenderer }

  THTMLMarkdownDefinitionListBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownDefinitionTermBlockRenderer }

  THTMLMarkdownDefinitionTermBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownDefinitionBlockRenderer }

  THTMLMarkdownDefinitionBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownAlertBlockRenderer }

  THTMLMarkdownAlertBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownFootnoteBlockRenderer }

  THTMLMarkdownFootnoteBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownFigureBlockRenderer }

  THTMLMarkdownFigureBlockRenderer = class(THTMLMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { THTMLMarkdownDirectiveBlockRenderer }

  // Writes container and leaf directives with the directive renderer registered for their name.
  THTMLMarkdownDirectiveBlockRenderer = class(THTMLMarkdownBlockRenderer)
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

function THTMLMarkdownBlockRenderer.GetHTMLRenderer: TMarkdownHTMLRenderer;
begin
  if Renderer is TMarkdownHTMLRenderer then
    Result:=TMarkdownHTMLRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure THTMLMarkdownBlockRenderer.Append(const S: TMarkdownString);
begin
  HTMLRenderer.Append(S);
end;

procedure THTMLMarkdownBlockRenderer.AppendNl(const S: TMarkdownString);
begin
  HTMLRenderer.AppendNL(S);
end;

function THTMLMarkdownBlockRenderer.HasOption(aOption: THTMLOption): Boolean;
begin
  Result:=(Self.Renderer is TMarkdownHTMLRenderer);
  if Result then
    Result:=aOption in TMarkdownHTMLRenderer(Renderer).Options;
end;


{ TMarkdownHTMLRenderer }

procedure TMarkdownHTMLRenderer.SetHead(const aValue: TStrings);
begin
  if FHead=aValue then Exit;
  FHead.Assign(aValue);
end;

procedure TMarkdownHTMLRenderer.Append(const aContent: TMarkdownString);
begin
  FBuilder.Append(aContent);
end;

procedure TMarkdownHTMLRenderer.AppendNL(const aContent: TMarkdownString);
begin
  if aContent<>'' then
    FBuilder.Append(aContent);
  FBuilder.Append(sLineBreak);
end;

constructor TMarkdownHTMLRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHead:=TStringList.Create;
  FDirectiveRenderers:=TStringList.Create;
  FDirectiveRenderers.CaseSensitive:=True;
end;

destructor TMarkdownHTMLRenderer.Destroy;
begin
  FreeAndNil(FDirectiveRenderers);
  FreeAndNil(FHead);
  inherited destroy;
end;

procedure TMarkdownHTMLRenderer.RenderDocument(aDocument: TMarkdownDocument);
begin
  FBuilder:=TMarkdownStringBuilder.Create;
  try
    RenderBlock(aDocument);
    FHTML:=BuilderToMarkdown(FBuilder);
  finally
    FreeAndNil(FBuilder);
  end;
end;

procedure TMarkdownHTMLRenderer.RenderDocument(aDocument: TMarkdownDocument; aDest: TStrings);
begin
  aDest.Text:=MarkdownToString(RenderHTML(aDocument));
end;

procedure TMarkdownHTMLRenderer.RenderChildren(aBlock: TMarkdownContainerBlock; aAppendNewLine: Boolean);
var
  i,iMax : integer;
begin
  iMax:=aBlock.Blocks.Count-1;
  for I:=0 to iMax do
    begin
    if aAppendNewLine and (I>0) then
      AppendNl();
    RenderBlock(aBlock.Blocks[I]);
    end;
end;

function TMarkdownHTMLRenderer.RenderHTML(aDocument: TMarkdownDocument): TMarkdownString;
begin
  RenderDocument(aDocument);
  Result:=FHTML;
  FHTML:='';
end;

procedure TMarkdownHTMLRenderer.RenderHTMLToFile(aDocument: TMarkdownDocument; const aFileName: String);
var
  lHTML : TMarkdownString;
  lFile : THandle;
begin
  lHTML:=RenderHTML(aDocument);
  lFile:=FileCreate(aFileName);
  try
    if lHTML<>'' then
      FileWrite(lFile,lHTML[1],Length(lHTML));
  finally
    FileClose(lFile);
  end;
end;

class function TMarkdownHTMLRenderer.FastRender(aDocument: TMarkdownDocument; aOptions: THTMLOptions; const aTitle: TMarkdownString;
  aHead: TStrings): TMarkdownString;
var
  lRender : TMarkdownHTMLRenderer;
begin
  lRender:=TMarkdownHTMLRenderer.Create(Nil);
  try
    lRender.Options:=aOptions;
    lRender.Title:=aTitle;
    if assigned(aHead) then
      lRender.Head.Assign(aHead);
    Result:=lRender.RenderHTML(aDocument);
  finally
    lRender.Free;
  end;
end;

class procedure TMarkdownHTMLRenderer.FastRenderToFile(aDocument: TMarkdownDocument; const aFileName: String;
  aOptions: THTMLOptions; const aTitle: TMarkdownString; aHead: TStrings);
var
  lRender : TMarkdownHTMLRenderer;
begin
  lRender:=TMarkdownHTMLRenderer.Create(Nil);
  try
    lRender.Options:=aOptions;
    lRender.Title:=aTitle;
    if assigned(aHead) then
      lRender.Head.Assign(aHead);
    lRender.RenderHTMLToFile(aDocument,aFileName);
  finally
    lRender.Free;
  end;
end;


procedure TMarkdownHTMLRenderer.WriteRaw(const aText: TMarkdownString);

begin
  if Assigned(FBuilder) then
    Append(aText);
end;


function TMarkdownHTMLRenderer.BlockAttributes(aBlock: TMarkdownBlock; aKeyValues: Boolean): TMarkdownString;

var
  I : Integer;

begin
  Result:='';
  if not aBlock.HasAttrs then
    Exit;
  if aBlock.ID<>'' then
    Result:=' id="'+HtmlEscape(aBlock.ID)+'"';
  if Length(aBlock.Classes)>0 then
    Result:=Result+' class="'+HtmlEscape(TMarkdownString.Join(' ',aBlock.Classes))+'"';
  if aKeyValues then
    for I:=0 to aBlock.Attrs.Count-1 do
      Result:=Result+' '+StringToMarkdown(aBlock.Attrs.Names[I])+'="'+HtmlEscape(StringToMarkdown(aBlock.Attrs.ValueFromIndex[I]))+'"';
end;


procedure TMarkdownHTMLRenderer.RenderFootnotes(aDocument: TMarkdownDocument);

var
  lBlock : TMarkdownBlock;
  lLabel : TMarkdownString;

begin
  if aDocument.Footnotes.Count=0 then
    Exit;
  AppendNL('<section class="footnotes">');
  AppendNL('<ol>');
  for lBlock in aDocument.Footnotes do
    begin
    lLabel:=HtmlEscape(TMarkdownFootnoteBlock(lBlock).FootnoteLabel);
    AppendNL('<li id="fn-'+lLabel+'">');
    RenderChildren(TMarkdownFootnoteBlock(lBlock));
    AppendNL('<a href="#fnref-'+lLabel+'" class="footnote-backref">&#8617;</a>');
    AppendNL('</li>');
    end;
  AppendNL('</ol>');
  AppendNL('</section>');
end;


procedure TMarkdownHTMLRenderer.RegisterDirectiveRenderer(const aName: TMarkdownString; aClass: THTMLDirectiveRendererClass);

var
  lIdx : Integer;

begin
  lIdx:=FDirectiveRenderers.IndexOf(MarkdownToString(aName));
  if aClass=Nil then
    begin
    if lIdx>=0 then
      FDirectiveRenderers.Delete(lIdx);
    end
  else if lIdx>=0 then
    FDirectiveRenderers.Objects[lIdx]:=TObject(aClass)
  else
    FDirectiveRenderers.AddObject(MarkdownToString(aName),TObject(aClass));
end;


function TMarkdownHTMLRenderer.FindDirectiveRenderer(const aName: TMarkdownString): THTMLDirectiveRendererClass;

var
  lIdx : Integer;

begin
  lIdx:=FDirectiveRenderers.IndexOf(MarkdownToString(aName));
  if lIdx>=0 then
    Result:=THTMLDirectiveRendererClass(FDirectiveRenderers.Objects[lIdx])
  else
    Result:=THTMLDirectiveRenderer;
end;

{ THTMLDirectiveRenderer }

constructor THTMLDirectiveRenderer.Create(aRenderer: TMarkdownHTMLRenderer);

begin
  inherited Create;
  FRenderer:=aRenderer;
end;


procedure THTMLDirectiveRenderer.CollectKey(aItem: TMarkdownString; const aKey: TMarkdownString; var aContinue: Boolean);

begin
  if aItem='' then ; // Silence warning
  FKeys.Add(MarkdownToString(aKey));
  aContinue:=True;
end;


procedure THTMLDirectiveRenderer.Append(const aContent: TMarkdownString);

begin
  FRenderer.Append(aContent);
end;


procedure THTMLDirectiveRenderer.AppendNL(const aContent: TMarkdownString);

begin
  FRenderer.AppendNL(aContent);
end;


function THTMLDirectiveRenderer.DirectiveAttributes(const aName, aID, aClasses: TMarkdownString; aAttrs: TStrings): TMarkdownString;

var
  I,J : Integer;
  lKey : TMarkdownString;

begin
  Result:=' data-directive="'+HtmlEscape(aName)+'"';
  if aID<>'' then
    Result:=Result+' id="'+HtmlEscape(aID)+'"';
  if aClasses<>'' then
    Result:=Result+' class="'+HtmlEscape(aClasses)+'"';
  if Assigned(aAttrs) then
    for I:=0 to aAttrs.Count-1 do
      begin
      lKey:=LowerCase(StringToMarkdown(aAttrs.Names[I]));
      for J:=1 to Length(lKey) do
        if not (lKey[J] in ['a'..'z','0'..'9','-','_','.']) then
          lKey[J]:='-';
      Result:=Result+' data-'+lKey+'="'+HtmlEscape(StringToMarkdown(aAttrs.ValueFromIndex[I]))+'"';
      end;
end;


function THTMLDirectiveRenderer.BlockAttributes(aBlock: TMarkdownDirectiveBlock): TMarkdownString;

var
  lAttrs : TStrings;

begin
  lAttrs:=Nil;
  if aBlock.HasAttrs then
    lAttrs:=aBlock.Attrs;
  Result:=DirectiveAttributes(aBlock.DirectiveName,aBlock.ID,TMarkdownString.Join(' ',aBlock.Classes),lAttrs);
end;


function THTMLDirectiveRenderer.InlineAttributes(aNode: TMarkdownTextNode): TMarkdownString;

var
  lAttrs : TStringList;
  lKeys : TStringList;
  lKey,lID,lClasses : TMarkdownString;
  I : Integer;

begin
  lID:='';
  lClasses:='';
  lAttrs:=TStringList.Create;
  lKeys:=TStringList.Create;
  try
    if aNode.HasAttrs then
      begin
      aNode.Attrs.TryGet('id',lID);
      aNode.Attrs.TryGet('class',lClasses);
      FKeys:=lKeys;
      aNode.Attrs.Iterate(@CollectKey);
      end;
    lKeys.Sorted:=True;
    for I:=0 to lKeys.Count-1 do
      begin
      lKey:=StringToMarkdown(lKeys[I]);
      if (lKey<>'id') and (lKey<>'class') then
        lAttrs.Add(MarkdownToString(lKey+'='+aNode.Attrs[lKey]));
      end;
    Result:=DirectiveAttributes(aNode.Name,lID,lClasses,lAttrs);
  finally
    lKeys.Free;
    lAttrs.Free;
  end;
end;


procedure THTMLDirectiveRenderer.RenderBlock(aBlock: TMarkdownDirectiveBlock);

begin
  if aBlock is TMarkdownLeafDirectiveBlock then
    begin
    Append('<div'+BlockAttributes(aBlock)+'>');
    if Assigned(aBlock.LabelNodes) then
      Renderer.RenderTextNodes(aBlock.LabelNodes);
    AppendNL('</div>');
    end
  else
    begin
    AppendNL('<div'+BlockAttributes(aBlock)+'>');
    if Assigned(aBlock.LabelNodes) then
      begin
      Append('<p data-directive-label="">');
      Renderer.RenderTextNodes(aBlock.LabelNodes);
      AppendNL('</p>');
      end;
    Renderer.RenderChildren(aBlock);
    AppendNL('</div>');
    end;
end;


procedure THTMLDirectiveRenderer.RenderInline(aTextRenderer: THTMLMarkdownTextRenderer; aNode: TMarkdownTextNode);

begin
  if aNode.NodeText<>'' then
    begin
    Append(HtmlEscape(aNode.NodeText));
    Exit;
    end;
  Append('<span'+InlineAttributes(aNode)+'>');
  aTextRenderer.RenderChildren(aNode);
  Append('</span>');
end;


procedure THTMLMarkdownTextRenderer.Append(const S: TMarkdownString);
begin
  HTMLRenderer.Append(S);
end;

function THTMLMarkdownTextRenderer.MustCloseNode(aElement: TMarkdownTextNode) : boolean;

begin
  Result:=aElement.kind<>nkImg;
end;

const
  StyleNames : Array[TNodeStyle] of TMarkdownString = ('b','i','del');

procedure THTMLMarkdownTextRenderer.PushStyle(aStyle: TNodeStyle);

begin
  HTMLRenderer.Append('<'+styleNames[aStyle]+'>');
  if FStyleStackLen=Length(FStyleStack) then
    SetLength(FStyleStack,FStyleStackLen+3);
  FStyleStack[FStyleStackLen]:=aStyle;
  Inc(FStyleStackLen);
end;

procedure THTMLMarkdownTextRenderer.PopStyle;
begin
  if FStyleStackLen=0 then
    Exit;
  Dec(FStyleStackLen);
  HTMLRenderer.Append('</'+styleNames[FStyleStack[FStyleStackLen]]+'>');
end;

function THTMLMarkdownTextRenderer.GetNodeTag(aElement: TMarkdownTextNode) : TMarkdownString;
begin
  case aElement.Kind of
    nkCode: Result:='code';
    nkImg : Result:='img';
    nkURI,nkEmail : Result:='a'
  else
    Result:='';
  end;
end;

function THTMLMarkdownTextRenderer.GetHTMLRenderer: TMarkdownHTMLRenderer;
begin
  if Renderer is TMarkdownHTMLRenderer then
    Result:=TMarkdownHTMLRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure THTMLMarkdownTextRenderer.DoKey(aItem: TMarkdownString; const aKey: TMarkdownString; var aContinue: Boolean);
begin
  aContinue:=True;
  FKeys[FKeyCount]:=aKey;
  inc(FKeyCount);
end;

procedure THTMLMarkdownTextRenderer.EmitStyleDiff(const aStyles : TNodeStyleArray);

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

procedure THTMLMarkdownTextRenderer.DoRender(aElement: TMarkdownTextNode);
var
  lName : TMarkdownString;
  lChild : TMarkdownTextNode;
begin
  lName:='';
  if aElement.Kind=nkComment then
    begin
    Renderer.DoMarkerNode(aElement);
    aElement.Active:=False;
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
  if aElement.Kind=nkWikiLink then
    begin
    RenderWikiLink(aElement);
    Exit;
    end;
  if aElement.Kind=nkDirective then
    begin
    RenderDirective(aElement);
    Exit;
    end;
  if aElement.Kind=nkLineBreak then
    begin
    Append('<br />');
    aElement.Active:=False;
    Exit;
    end;
  if aElement.Kind=nkRaw then
    begin
    Append(aElement.NodeText);
    aElement.Active:=False;
    Exit;
    end;
  if aElement.Kind<>nkText then
    begin
    lName:=GetNodeTag(aElement);
    if lName<>'' then
      begin
      Append('<');
      Append(lName);
      Append(renderAttrs(aElement));
      Append('>');
      end;
    end;
  if MustCloseNode(aElement) then
    begin
    if aElement.NodeText<>'' then
      Append(HtmlEscape(aElement.NodeText));
    if aElement.HasChildren then
      begin
      for lChild in aElement.Children do
        DoRender(lChild);
      EmitStyleDiff(aElement.StyleList);
      end;
    end;
  if (lName<>'') and MustCloseNode(aElement) then
    begin
    Append('</');
    Append(lName);
    Append('>');
    end;
  aElement.Active:=False;
end;

procedure THTMLMarkdownTextRenderer.RenderLink(aElement: TMarkdownTextNode);

var
  lHref,lText,lTitle : TMarkdownString;
  lChild : TMarkdownTextNode;

begin
  aElement.Active:=False;
  lHref:=aElement.Attrs['href'];
  lText:=aElement.NodeText;
  if aElement.HasChildren then
    lText:=lText+aElement.Children.PlainText;
  if Renderer.DoResolveLink(lHref,lText) then
    Exit;
  Append('<a href="'+HtmlEscape(lHref)+'"');
  if aElement.Attrs.TryGet('title',lTitle) then
    Append(' title="'+HtmlEscape(lTitle)+'"');
  Append('>');
  if (aElement.NodeText='') and not aElement.HasChildren then
    begin
    if lText='' then
      lText:=lHref;
    Append(HtmlEscape(lText));
    end
  else
    begin
    if aElement.NodeText<>'' then
      Append(HtmlEscape(aElement.NodeText));
    if aElement.HasChildren then
      begin
      for lChild in aElement.Children do
        DoRender(lChild);
      EmitStyleDiff(aElement.StyleList);
      end;
    end;
  Append('</a>');
end;


procedure THTMLMarkdownTextRenderer.RenderFootnoteRef(aElement: TMarkdownTextNode);

var
  lLabel,lNumber,lIndex,lID : TMarkdownString;

begin
  aElement.Active:=False;
  lLabel:=aElement.Attrs['label'];
  lNumber:=aElement.Attrs['number'];
  lIndex:=aElement.Attrs['refindex'];
  if lNumber='' then
    lNumber:=lLabel;
  lID:='fnref-'+lLabel;
  if (lIndex<>'') and (lIndex<>'1') then
    lID:=lID+'-'+lIndex;
  Append('<sup><a href="#fn-'+HtmlEscape(lLabel)+'" id="'+HtmlEscape(lID)+'">'+HtmlEscape(lNumber)+'</a></sup>');
end;


procedure THTMLMarkdownTextRenderer.RenderWikiLink(aElement: TMarkdownTextNode);

var
  lHref,lClass,lTag : TMarkdownString;
  lExists : Boolean;

begin
  aElement.Active:=False;
  Renderer.DoResolveWikiLink(aElement.Attrs['target'],aElement.Attrs['fragment'],lHref,lClass,lExists);
  if not lExists and (lClass='') then
    lClass:='missing';
  if lHref<>'' then
    begin
    lTag:='a';
    Append('<a href="'+HtmlEscape(lHref)+'"');
    end
  else
    begin
    lTag:='span';
    Append('<span');
    end;
  if lClass<>'' then
    Append(' class="'+HtmlEscape(lClass)+'"');
  Append('>');
  if aElement.NodeText<>'' then
    Append(HtmlEscape(aElement.NodeText));
  RenderChildren(aElement);
  Append('</'+lTag+'>');
end;


procedure THTMLMarkdownTextRenderer.RenderDirective(aElement: TMarkdownTextNode);

var
  lRenderer : THTMLDirectiveRenderer;

begin
  aElement.Active:=False;
  lRenderer:=HTMLRenderer.FindDirectiveRenderer(aElement.Name).Create(HTMLRenderer);
  try
    lRenderer.RenderInline(Self,aElement);
  finally
    lRenderer.Free;
  end;
end;


procedure THTMLMarkdownTextRenderer.RenderChildren(aElement: TMarkdownTextNode);

var
  lChild : TMarkdownTextNode;

begin
  if not aElement.HasChildren then
    Exit;
  for lChild in aElement.Children do
    DoRender(lChild);
  EmitStyleDiff(aElement.StyleList);
end;


procedure THTMLMarkdownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  FStyleStackLen:=0;
end;

procedure THTMLMarkdownTextRenderer.EndBlock;
begin
  While (FStyleStackLen>0) do
    Popstyle;
  inherited EndBlock;
end;

function THTMLMarkdownTextRenderer.RenderAttrs(aElement: TMarkdownTextNode): TMarkdownString;

  procedure addKey(aKey,aValue : TMarkdownString);
  begin
    Result:=Result+' '+aKey+'="'+HtmlEscape(aValue)+'"';
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

procedure THTMLParagraphBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownParagraphBlock absolute aElement;
  c : TMarkdownBlock;
  first : boolean;
  lNumber : TMarkdownString;
begin
  if lNode.header=0 then
    Append('<p>')
  else
    begin
    Append('<h'+IntToMarkdown(lNode.Header)+HTMLRenderer.BlockAttributes(lNode)+'>');
    lNumber:=Renderer.GetHeadingNumber(lNode);
    if lNumber<>'' then
      Append(HtmlEscape(lNumber)+' ');
    end;
  first := true;
  for c in lNode.Blocks do
    begin
    if first then
      first := false
    else
      AppendNl;
   Renderer.RenderChildren(lNode);
    end;
  if lNode.header=0 then
    Append('</p>')
  else
    Append('</h'+IntToMarkdown(lNode.Header)+'>');
  AppendNl;
end;

class function THTMLParagraphBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownParagraphBlock;
end;

class function THTMLMarkdownTextBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTextBlock;
end;

procedure THTMLMarkdownTextBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownTextBlock absolute aElement;
begin
  if assigned(lNode) and assigned(lNode.Nodes) then
    Renderer.RenderTextNodes(lNode.Nodes);
end;

procedure THTMLMarkdownQuoteBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownQuoteBlock absolute aElement;

begin
  AppendNl('<blockquote>');
  Renderer.RenderChildren(lNode);
  AppendNl('</blockquote>');
end;

class function THTMLMarkdownQuoteBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownQuoteBlock;
end;

procedure THTMLMarkdownListBlockRenderer.DoRender(aElement : TMarkdownBlock);

var
  lNode : TMarkdownListBlock absolute aElement;
  lBlock : TMarkdownBlock;
  lClass : TMarkdownString;

begin
  lClass:='';
  for lBlock in lNode.Blocks do
    if (lBlock is TMarkdownListItemBlock) and (TMarkdownListItemBlock(lBlock).TaskState<>tisNone) then
      lClass:=' class="contains-task-list"';
  if not lNode.Ordered then
    AppendNl('<ul'+lClass+'>')
  else if lNode.Start=1 then
    AppendNL('<ol'+lClass+'>')
  else
    AppendNl('<ol start="'+IntToMarkdown(lNode.Start)+'"'+lClass+'>');
  Renderer.RenderChildren(lNode);
  if lNode.Ordered then
    AppendNl('</ol>')
  else
    AppendNl('</ul>');
end;

class function THTMLMarkdownListBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownListBlock;
end;


procedure THTMLMarkdownListItemBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lItemBlock : TMarkdownListItemBlock absolute aElement;
  lBlock : TMarkdownBlock;
  lPar : TMarkdownParagraphBlock absolute lBlock;
  lCount : Integer;

  function IsPlainBlock(aBlock : TMarkdownBlock) : boolean;
  begin
    Result:=(aBlock is TMarkdownParagraphBlock)
             and (aBlock as TMarkdownParagraphBlock).isPlainPara
             and not (lItemblock.parent as TMarkdownListBlock).loose
  end;


const
  Checked : Array[Boolean] of TMarkdownString = ('',' checked=""');

var
  lCheckbox : TMarkdownString;

begin
  lCheckbox:='';
  if lItemBlock.TaskState=tisNone then
    Append('<li>')
  else
    begin
    Append('<li class="task-list-item">');
    lCheckbox:='<input type="checkbox" class="task-list-item-checkbox" disabled=""'+Checked[lItemBlock.TaskState=tisChecked]+' /> ';
    end;
  lCount:=0;
  For lBlock in lItemBlock.Blocks do
    if IsPlainBlock(lBlock) then
      begin
      Append(lCheckbox);
      lCheckbox:='';
      HTMLRenderer.RenderChildren(lPar,True);
      end
    else
      begin
      if lCount=0 then
        AppendNl;
      Inc(lCount);
      if lCheckbox<>'' then
        begin
        // The checkbox of a loose task item goes inside its first paragraph.
        Append('<p>'+lCheckbox);
        lCheckbox:='';
        HTMLRenderer.RenderChildren(lPar,True);
        AppendNl('</p>');
        end
      else
        Renderer.RenderBlock(lBlock);
      end;
  AppendNl('</li>');
end;

class function THTMLMarkdownListItemBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownListItemBlock;
end;

procedure THTMLMarkdownCodeBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lNode : TMarkdownCodeBlock absolute aElement;
  lBlock : TMarkdownBlock;
  lLang : TMarkdownString;
begin
  lLang:=lNode.Lang;
  AppendNL('');
  Append('<pre'+HTMLRenderer.BlockAttributes(lNode)+'>');
  if lLang<> '' then
    Append('<code class="language-'+lLang+'">')
  else
    Append('<code>');
  for lBlock in LNode.Blocks do
    begin
    Renderer.RenderCodeBlock(LBlock,lLang);
    AppendNl;
    end;
  Append('</code></pre>');
  AppendNl;
end;

class function THTMLMarkdownCodeBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownCodeBlock;
end;

procedure THTMLMarkdownThematicBreakBlockRenderer.DoRender(aElement : TMarkdownBlock);

begin
  if Not Assigned(aElement) then
    exit;
  Append('<hr />');
  AppendNl;
end;

class function THTMLMarkdownThematicBreakBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownThematicBreakBlock;
end;

{ TMarkdownTableBlock }

procedure THTMLMarkdownTableBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownTableBlock absolute aElement;
  i : integer;
  lLabel : TMarkdownString;
begin
  AppendNl('<table'+HTMLRenderer.BlockAttributes(lNode)+'>');
  lLabel:=Renderer.GetCaptionNumber(lNode);
  if Assigned(lNode.Caption) or (lLabel<>'') then
    begin
    Append('<caption>');
    if lLabel<>'' then
      Append(HtmlEscape(lLabel)+': ');
    if Assigned(lNode.Caption) then
      Renderer.RenderTextNodes(lNode.Caption);
    AppendNl('</caption>');
    end;
  AppendNl('<thead>');
  Renderer.RenderBlock(lNode.blocks[0]);
  AppendNl('</thead>');
  if lNode.blocks.Count > 1 then
  begin
    AppendNl('<tbody>');
    for i := 1 to lNode.blocks.Count -1  do
      Renderer.RenderBlock(lnode.blocks[i]);
    AppendNl('</tbody>');
  end;
  AppendNl('</table>');
end;

class function THTMLMarkdownTableBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableBlock;
end;

{ THTMLMarkdownFrontmatterBlockRenderer }

procedure THTMLMarkdownFrontmatterBlockRenderer.DoRender(aElement: TMarkdownBlock);
begin
  // Frontmatter produces no visible output
end;

class function THTMLMarkdownFrontmatterBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result := TMarkdownFrontmatterBlock;
end;

{ THTMLMarkdownDocumentRenderer }

procedure THTMLMarkdownDocumentRenderer.DoRender(aElement: TMarkdownBlock);
var
  H : String;
begin
  if HasOption(hoEnvelope) then
    begin
    AppendNL('<!DOCTYPE html>');
    AppendNL('<html>');
    if HasOption(hoHead) then
      begin
      AppendNL('<head>');
      if HTMLRenderer.Title<>'' then
        begin
        Append('<title>');
        Append(HTMLRenderer.Title);
        AppendNL('</title>');
        end;
      for H in HTMLRenderer.Head do
        AppendNL(StringToMarkdown(H));
      AppendNL('</head>');
      end;
    AppendNL('<body>');
    end;
  Renderer.RenderChildren(aElement as TMarkdownDocument);
  HTMLRenderer.RenderFootnotes(aElement as TMarkdownDocument);
  if HasOption(hoEnvelope) then
    begin
    AppendNL('</body>');
    AppendNL('</html>');
    end;
end;

class function THTMLMarkdownDocumentRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownDocument
end;

{ TMarkdownTableRowBlock }

procedure THTMLMarkdownTableRowBlockRenderer.DoRender(aElement : TMarkdownBlock);
const
  CellTypes : Array[Boolean] of TMarkdownString = ('td','th'); //
var
  lNode : TMarkdownTableRowBlock absolute aElement;
  lFirst : boolean;
  i,lCount : integer;
  lType,lAttr : TMarkdownString;
  lAlign: TCellAlign;
begin
  lFirst:=(lNode.parent as TMarkdownContainerBlock).blocks.First = lNode;
  lCount:=length((lNode.parent as TMarkdownTableBlock).Columns);
  lType:=CellTypes[lFirst];
  AppendNl('<tr>');
  for i:=0 to lCount-1 do
    begin
    lAlign:=(lNode.parent as TMarkdownTableBlock).Columns[i];
    case lAlign of
      caLeft   : lAttr:='';
      caCenter : lAttr:=' align="center"';
      caRight  : lAttr:=' align="right"';
    end;
    Append('<'+lType+lAttr+'>');
    if i<lNode.blocks.Count then
      Renderer.RenderBlock(lNode.blocks[i]);
    AppendNl('</'+lType+'>');
    end;
  AppendNl('</tr>');
end;

class function THTMLMarkdownTableRowBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableRowBlock;
end;

procedure THTMLMarkdownHeadingBlockRenderer.DoRender(aElement : TMarkdownBlock);

var
  lNode : TMarkdownHeadingBlock absolute aElement;
  lNumber : TMarkdownString;
begin
  Append('<h'+IntToMarkdown(Lnode.Level)+HTMLRenderer.BlockAttributes(lNode)+'>');
  lNumber:=Renderer.GetHeadingNumber(lNode);
  if lNumber<>'' then
    Append(HtmlEscape(lNumber)+' ');
  Renderer.RenderChildren(lNode);
  Append('</h'+IntToMarkdown(lNode.Level)+'>');
  AppendNl;
end;

class function THTMLMarkdownHeadingBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownHeadingBlock;
end;

{ THTMLMarkdownCommentBlockRenderer }

procedure THTMLMarkdownCommentBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownCommentBlock absolute aElement;

begin
  if lNode.IsMarker then
    Renderer.DoMarker(lNode.MarkerName,lNode.MarkerArgument,lNode.MarkerValue);
end;


class function THTMLMarkdownCommentBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownCommentBlock;
end;

{ THTMLMarkdownDefinitionListBlockRenderer }

procedure THTMLMarkdownDefinitionListBlockRenderer.DoRender(aElement: TMarkdownBlock);

begin
  AppendNl('<dl'+HTMLRenderer.BlockAttributes(aElement)+'>');
  Renderer.RenderChildren(aElement as TMarkdownContainerBlock);
  AppendNl('</dl>');
end;


class function THTMLMarkdownDefinitionListBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionListBlock;
end;

{ THTMLMarkdownDefinitionTermBlockRenderer }

procedure THTMLMarkdownDefinitionTermBlockRenderer.DoRender(aElement: TMarkdownBlock);

begin
  Append('<dt>');
  Renderer.RenderChildren(aElement as TMarkdownContainerBlock);
  AppendNl('</dt>');
end;


class function THTMLMarkdownDefinitionTermBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionTermBlock;
end;

{ THTMLMarkdownDefinitionBlockRenderer }

procedure THTMLMarkdownDefinitionBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lDef : TMarkdownDefinitionBlock absolute aElement;
  lBlock : TMarkdownBlock;
  lTight : Boolean;
  lCount : Integer;

begin
  lTight:=(lDef.Parent is TMarkdownDefinitionListBlock) and not TMarkdownDefinitionListBlock(lDef.Parent).Loose;
  Append('<dd>');
  lCount:=0;
  for lBlock in lDef.Blocks do
    if lTight and (lBlock.ClassType=TMarkdownParagraphBlock) and TMarkdownParagraphBlock(lBlock).IsPlainPara then
      HTMLRenderer.RenderChildren(TMarkdownParagraphBlock(lBlock),True)
    else
      begin
      if lCount=0 then
        AppendNl;
      Inc(lCount);
      Renderer.RenderBlock(lBlock);
      end;
  AppendNl('</dd>');
end;


class function THTMLMarkdownDefinitionBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionBlock;
end;

{ THTMLMarkdownAlertBlockRenderer }

procedure THTMLMarkdownAlertBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownAlertBlock absolute aElement;

begin
  AppendNl('<div class="alert alert-'+AlertTypeNames[lNode.AlertType]+'">');
  Append('<p class="alert-title">');
  if Assigned(lNode.Title) then
    Renderer.RenderTextNodes(lNode.Title)
  else
    Append(HtmlEscape(Renderer.AlertTitles[lNode.AlertType]));
  AppendNl('</p>');
  Renderer.RenderChildren(lNode);
  AppendNl('</div>');
end;


class function THTMLMarkdownAlertBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownAlertBlock;
end;

{ THTMLMarkdownFootnoteBlockRenderer }

procedure THTMLMarkdownFootnoteBlockRenderer.DoRender(aElement: TMarkdownBlock);

begin
  if aElement=Nil then ; // Silence warning
end;


class function THTMLMarkdownFootnoteBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownFootnoteBlock;
end;

{ THTMLMarkdownFigureBlockRenderer }

procedure THTMLMarkdownFigureBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownFigureBlock absolute aElement;
  lLabel,lValue : TMarkdownString;
  I : Integer;

begin
  AppendNl('<figure'+HTMLRenderer.BlockAttributes(lNode,False)+'>');
  if Assigned(lNode.Image) then
    begin
    Append('<img src="'+HtmlEscape(lNode.Image.Attrs['src'])+'" alt="'+HtmlEscape(lNode.Image.Attrs['alt'])+'"');
    if lNode.Image.Attrs.TryGet('title',lValue) then
      Append(' title="'+HtmlEscape(lValue)+'"');
    if lNode.HasAttrs then
      for I:=0 to lNode.Attrs.Count-1 do
        Append(' '+StringToMarkdown(lNode.Attrs.Names[I])+'="'+HtmlEscape(StringToMarkdown(lNode.Attrs.ValueFromIndex[I]))+'"');
    AppendNl('>');
    end;
  lLabel:=Renderer.GetCaptionNumber(lNode);
  if (Assigned(lNode.Caption) and (lNode.Caption.Count>0)) or (lLabel<>'') then
    begin
    Append('<figcaption>');
    if lLabel<>'' then
      Append(HtmlEscape(lLabel)+': ');
    if Assigned(lNode.Caption) then
      Renderer.RenderTextNodes(lNode.Caption);
    AppendNl('</figcaption>');
    end;
  AppendNl('</figure>');
end;


class function THTMLMarkdownFigureBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownFigureBlock;
end;

{ THTMLMarkdownDirectiveBlockRenderer }

procedure THTMLMarkdownDirectiveBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownDirectiveBlock absolute aElement;
  lRenderer : THTMLDirectiveRenderer;

begin
  lRenderer:=HTMLRenderer.FindDirectiveRenderer(lNode.DirectiveName).Create(HTMLRenderer);
  try
    lRenderer.RenderBlock(lNode);
  finally
    lRenderer.Free;
  end;
end;


class function THTMLMarkdownDirectiveBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDirectiveBlock;
end;

initialization
  THTMLMarkdownHeadingBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLParagraphBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownQuoteBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownTextBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownListBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownListItemBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownCodeBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownHeadingBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownThematicBreakBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownTableBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownTableRowBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownFrontmatterBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownDocumentRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownCommentBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownDefinitionListBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownDefinitionTermBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownDefinitionBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownAlertBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownFootnoteBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownFigureBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownDirectiveBlockRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
  THTMLMarkdownTextRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
end.

