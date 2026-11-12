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

  TMarkdownHTMLRenderer = class(TMarkdownRenderer)
  private
    FBuilder: TStringBuilder;
    FHead: TStrings;
    FHTML: String;
    FOptions: THTMLOptions;
    FTitle: String;
    procedure SetHead(const aValue: TStrings);
  Protected
    Procedure Append(const aContent : String);
    Procedure AppendNL(const aContent : String = '');
    Property Builder : TStringBuilder Read FBuilder;
  public
    constructor Create(aOwner : TComponent); override;
    destructor destroy; override;
    Procedure RenderDocument(aDocument : TMarkdownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings); overload;
    procedure RenderChildren(aBlock : TMarkdownContainerBlock; aAppendNewLine : Boolean); overload;
    function RenderHTML(aDocument : TMarkdownDocument) : string;
    procedure RenderHTMLToFile(aDocument : TMarkdownDocument; const aFileName : string);
    class function FastRender(aDocument : TMarkdownDocument; aOptions : THTMLOptions; const aTitle : String = ''; aHead : TStrings = Nil) : String;
    class procedure FastRenderToFile(aDocument : TMarkdownDocument; const aFileName : string; aOptions : THTMLOptions; const aTitle : String = ''; aHead : TStrings = Nil);
    Property HTML : String Read FHTML;
  published
    Property Options : THTMLOptions Read FOptions Write FOptions;
    property Title : String Read FTitle Write FTitle;
    property Head : TStrings Read FHead Write SetHead;
  end;

  { THTMLMarkdownBlockRenderer }

  THTMLMarkdownBlockRenderer = Class (TMarkdownBlockRenderer)
  Private
    function GetHTMLRenderer: TMarkdownHTMLRenderer;
  protected
    procedure Append(const S : String); inline;
    procedure AppendNl(const S : String = ''); inline;
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
    FLastStyles : TNodeStyles;
    FKeys : Array of String;
    FKeyCount : integer;
    procedure DoKey(aItem: AnsiString; const aKey: AnsiString; var aContinue: Boolean);
    procedure EmitStyleDiff(aStyles: TNodeStyles);
    function GetHTMLRenderer: TMarkdownHTMLRenderer;
    function GetNodeTag(aElement: TMarkdownTextNode): string;
    function MustCloseNode(aElement: TMarkdownTextNode): boolean;
  protected
    procedure PushStyle(aStyle : TNodeStyle);
    function PopStyles(aStyle: TNodeStyles): TNodeStyle;
    procedure PopStyle(aStyle : TNodeStyle);
    procedure Append(const S : String); inline;
    procedure DoRender(aElement: TMarkdownTextNode); override;
  Public
    procedure BeginBlock; override;
    procedure EndBlock; override;
    property HTMLRenderer : TMarkdownHTMLRenderer Read GetHTMLRenderer;
    function renderAttrs(aElement: TMarkdownTextNode): AnsiString;
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
    procedure dorender(aElement : TMarkdownBlock); override;
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

function THTMLMarkdownBlockRenderer.GetHTMLRenderer: TMarkdownHTMLRenderer;
begin
  if Renderer is TMarkdownHTMLRenderer then
    Result:=TMarkdownHTMLRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure THTMLMarkdownBlockRenderer.Append(const S: String);
begin
  HTMLRenderer.Append(S);
end;

procedure THTMLMarkdownBlockRenderer.AppendNl(const S: String);
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
  FHead:=aValue;
end;

procedure TMarkdownHTMLRenderer.Append(const aContent: String);
begin
  FBuilder.Append(aContent);
end;

procedure TMarkdownHTMLRenderer.AppendNL(const aContent: String);
begin
  if aContent<>'' then
    FBuilder.Append(aContent);
  FBuilder.Append(sLineBreak);
end;

constructor TMarkdownHTMLRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHead:=TStringList.Create;
end;

destructor TMarkdownHTMLRenderer.destroy;
begin
  FreeAndNil(FHead);
  inherited destroy;
end;

procedure TMarkdownHTMLRenderer.RenderDocument(aDocument: TMarkdownDocument);
begin
  FBuilder:=TStringBuilder.Create;
  try
    RenderBlock(aDocument);
    FHTML:=FBuilder.ToString;
  finally
    FreeAndNil(FBuilder);
  end;
end;

procedure TMarkdownHTMLRenderer.RenderDocument(aDocument: TMarkdownDocument; aDest: TStrings);
begin
  aDest.Text:=RenderHTML(aDocument);
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

function TMarkdownHTMLRenderer.RenderHTML(aDocument: TMarkdownDocument): string;
begin
  RenderDocument(aDocument);
  Result:=FHTML;
  FHTML:='';
end;

procedure TMarkdownHTMLRenderer.RenderHTMLToFile(aDocument: TMarkdownDocument; const aFileName: string);
var
  lHTML : String;
  lFile : THandle;
begin
  lHTML:=RenderHTML(aDocument);
  lFile:=FileCreate(aFileName);
  try
    if lHTML<>'' then
      FileWrite(lFile,lHTML[1],Length(lHTML)*SizeOf(Char));
  finally
    FileClose(lFile);
  end;
end;

class function TMarkdownHTMLRenderer.FastRender(aDocument: TMarkdownDocument; aOptions: THTMLOptions; const aTitle: String;
  aHead: TStrings): String;
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

class procedure TMarkdownHTMLRenderer.FastRenderToFile(aDocument: TMarkdownDocument; const aFileName: string;
  aOptions: THTMLOptions; const aTitle: String; aHead: TStrings);
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


procedure THTMLMarkdownTextRenderer.Append(const S: String);
begin
  HTMLRenderer.Append(S);
end;

function THTMLMarkdownTextRenderer.MustCloseNode(aElement: TMarkdownTextNode) : boolean;

begin
  Result:=aElement.kind<>nkImg;
end;

const
  StyleNames : Array[TNodeStyle] of string = ('b','i','del');

procedure THTMLMarkdownTextRenderer.PushStyle(aStyle: TNodeStyle);

begin
  HTMLRenderer.Append('<'+styleNames[aStyle]+'>');
  if FStyleStackLen=Length(FStyleStack) then
    SetLength(FStyleStack,FStyleStackLen+3);
  FStyleStack[FStyleStackLen]:=aStyle;
  Inc(FStyleStackLen);
end;

function THTMLMarkdownTextRenderer.Popstyles(aStyle: TNodeStyles) : TNodeStyle;

begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1] in aStyle) then
    begin
    Result:=FStyleStack[FStyleStackLen-1];
    HTMLRenderer.Append('</'+StyleNames[Result]+'>');
    Dec(FStyleStackLen);
    end;
end;

procedure THTMLMarkdownTextRenderer.PopStyle(aStyle: TNodeStyle);
begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1]=aStyle) then
    begin
    HTMLRenderer.Append('</'+styleNames[aStyle]+'>');
    Dec(FStyleStackLen);
    end;
end;

function THTMLMarkdownTextRenderer.GetNodeTag(aElement: TMarkdownTextNode) : string;
begin
  case aElement.Kind of
    nkCode: Result:='code';
    nkImg : Result:='img';
    nkURI,nkEmail : Result:='a'
  end;
end;

function THTMLMarkdownTextRenderer.GetHTMLRenderer: TMarkdownHTMLRenderer;
begin
  if Renderer is TMarkdownHTMLRenderer then
    Result:=TMarkdownHTMLRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure THTMLMarkdownTextRenderer.DoKey(aItem: AnsiString; const aKey: Ansistring; var aContinue: Boolean);
begin
  aContinue:=True;
  FKeys[FKeyCount]:=aKey;
  inc(FKeyCount);
end;

procedure THTMLMarkdownTextRenderer.EmitStyleDiff(aStyles : TNodeStyles);

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

procedure THTMLMarkdownTextRenderer.DoRender(aElement: TMarkdownTextNode);
var
  lName : string;
begin
  EmitStyleDiff(aElement.Styles);
  if aElement.Kind<>nkText then
    begin
    lName:=GetNodeTag(aElement);
    Append('<');
    Append(lName);
    Append(renderAttrs(aElement));
    Append('>');
    end;
  if aElement.NodeText<>'' then
    Append(aElement.NodeText);
  if (lName<>'') and MustCloseNode(aElement) then
    begin
    Append('</');
    Append(lName);
    Append('>');
    end;
  aElement.Active:=False;
end;

procedure THTMLMarkdownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  FStyleStackLen:=0;
  FLastStyles:=[];
end;

procedure THTMLMarkdownTextRenderer.EndBlock;
begin
  While (FStyleStackLen>0) do
    Popstyle(FStyleStack[FStyleStackLen-1]);
  FLastStyles:=[];
  inherited EndBlock;
end;

function THTMLMarkdownTextRenderer.renderAttrs(aElement: TMarkdownTextNode): AnsiString;

  procedure addKey(aKey,aValue : String);
  begin
    Result:=Result+' '+aKey+'="'+aValue+'"';
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

procedure THTMLParagraphBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownParagraphBlock absolute aElement;
  c : TMarkdownBlock;
  first : boolean;
begin
  if lNode.header=0 then
    Append('<p>')
  else
    Append('<h'+IntToStr(lNode.Header)+'>');
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
    Append('</h'+IntToStr(lNode.Header)+'>');
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

procedure THTMLMarkdownQuoteBlockRenderer.dorender(aElement: TMarkdownBlock);
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

begin
  if not lNode.Ordered then
    AppendNl('<ul>')
  else if lNode.Start=1 then
    AppendNL('<ol>')
  else
    AppendNl('<ol start="'+IntToStr(lNode.Start)+'">');
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


begin
  Append('<li>');
  lCount:=0;
  For lBlock in lItemBlock.Blocks do
    if IsPlainBlock(lBlock) then
      HTMLRenderer.RenderChildren(lPar,True)
    else
      begin
      if lCount=0 then
        AppendNl;
      Inc(lCount);
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
  lLang : string;
begin
  lLang:=lNode.Lang;
  AppendNL('');
  if lLang<> '' then
    AppendNl('<pre><code class="language-'+lLang+'">')
  else
    AppendNl('<pre><code>');
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
begin
  AppendNl('<table>');
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
        AppendNL(H);
      AppendNL('</head>');
      end;
    AppendNL('<body>');
    end;
  Renderer.RenderChildren(aElement as TMarkdownDocument);
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
  CellTypes : Array[Boolean] of string = ('td','th'); //
var
  lNode : TMarkdownTableRowBlock absolute aElement;
  lFirst : boolean;
  i,lCount : integer;
  lType,lAttr : String;
  lAlign: TCellAlign;
begin
  lFirst:=(lNode.parent as TMarkdownContainerBlock).blocks.First = self;
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
begin
  Append('<h'+inttostr(Lnode.Level)+'>');
  Renderer.RenderChildren(lNode);
  Append('</h'+inttostr(lNode.Level)+'>');
  AppendNl;
end;

class function THTMLMarkdownHeadingBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownHeadingBlock;
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
  THTMLMarkdownTextRenderer.RegisterRenderer(TMarkdownHTMLRenderer);
end.

