{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown LaTeX renderer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 *********************************************************************}

unit Markdown.LatexRender;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.StrUtils, System.Contnrs,
{$ELSE}
  Classes, SysUtils,  contnrs,
{$ENDIF}
  Markdown.Elements,
  Markdown.Render,
  Markdown.Utils;

type
  { TMarkdownLaTeXRenderer }
  TLaTeXOption = (loEnvelope, loNumberedSections);
  TLaTeXOptions = set of TLaTeXOption;

  TMarkdownLaTeXRenderer = class(TMarkdownRenderer)
  private
    FBuilder: TMarkdownStringBuilder;
    FDocument: TMarkdownDocument;
    FHead: TStrings;
    FLaTeX: TMarkdownString;
    FOptions: TLaTeXOptions;
    FTitle: TMarkdownString;
    FAuthor: TMarkdownString;
    procedure SetHead(const aValue: TStrings);
  Protected
    Procedure Append(const aContent : TMarkdownString);
    Procedure AppendNL(const aContent : TMarkdownString = '');
    Property Builder : TMarkdownStringBuilder Read FBuilder;
    function EscapeLaTeX(const S: TMarkdownString): TMarkdownString;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    Procedure RenderDocument(aDocument : TMarkdownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings); overload;
    procedure RenderChildren(aBlock : TMarkdownContainerBlock; aAppendNewLine : Boolean); overload;
    function RenderLaTeX(aDocument : TMarkdownDocument) : TMarkdownString;
    Procedure RenderToFile(aDocument : TMarkdownDocument; const aFileName : String);
    class procedure FastRenderToFile(aDocument : TMarkdownDocument; const aFileName : String; aOptions : TLaTeXOptions = []; const aTitle : TMarkdownString = ''; const aAuthor : TMarkdownString = '');
    class function FastRender(aDocument : TMarkdownDocument; aOptions : TLaTeXOptions = []; const aTitle : TMarkdownString = ''; const aAuthor : TMarkdownString = '') : TMarkdownString;
    // Write aText to the output as-is.
    procedure WriteRaw(const aText : TMarkdownString); override;
    // Remove line breaks at the end of the output.
    procedure TrimTrailingNewLines;
    // \label{id} for a block with an id, empty otherwise.
    function LabelFor(aBlock : TMarkdownBlock) : TMarkdownString;
    // The document being rendered
    property Document : TMarkdownDocument read FDocument;
  published
    Property Options : TLaTeXOptions Read FOptions Write FOptions;
    property Title : TMarkdownString Read FTitle Write FTitle;
    property Author : TMarkdownString Read FAuthor Write FAuthor;
    property Head : TStrings Read FHead Write SetHead;
  end;

  { TLaTeXMarkdownBlockRenderer }

  TLaTeXMarkdownBlockRenderer = Class (TMarkdownBlockRenderer)
  Private
    function GetLaTeXRenderer: TMarkdownLaTeXRenderer;
  protected
    procedure Append(const S : TMarkdownString); inline;
    procedure AppendNl(const S : TMarkdownString = ''); inline;
    function HasOption(aOption : TLaTeXOption) : Boolean;
    function Escape(const S: TMarkdownString): TMarkdownString;
    // Write aBlock as a sectioning command of level aLevel
    procedure RenderHeading(aBlock : TMarkdownContainerBlock; aLevel : Integer);
  public
    property LaTeXRenderer : TMarkdownLaTeXRenderer Read GetLaTeXRenderer;
  end;
  TLaTeXMarkdownBlockRendererClass = class of TLaTeXMarkdownBlockRenderer;

  { TLaTeXMarkdownTextRenderer }

  TLaTeXMarkdownTextRenderer = class(TMarkdownTextRenderer)
  Private
    FStyleStack: Array of TNodeStyle;
    FStyleStackLen : Integer;
    function GetLaTeXRenderer: TMarkdownLaTeXRenderer;
    function GetNodeTag(aElement: TMarkdownTextNode; Closing: Boolean): TMarkdownString;
  protected
    procedure PushStyle(aStyle : TNodeStyle);
    procedure PopStyle;
    procedure Append(const S : TMarkdownString); inline;
    procedure DoRender(aElement: TMarkdownTextNode); override;
    function Escape(const S: TMarkdownString): TMarkdownString;
    procedure EmitStyleDiff(const aStyles : TNodeStyleArray);
    // Render a link node, after OnResolveLink
    procedure RenderLink(aElement: TMarkdownTextNode); virtual;
    // Render a footnote reference as \footnote with the definition inside
    procedure RenderFootnoteRef(aElement: TMarkdownTextNode); virtual;
  Public
    procedure BeginBlock; override;
    procedure EndBlock; override;
    property LaTeXRenderer : TMarkdownLaTeXRenderer Read GetLaTeXRenderer;
  end;
  TLaTeXMarkdownTextRendererClass = class of TLaTeXMarkdownTextRenderer;

  { TLaTeXParagraphBlockRenderer }

  TLaTeXParagraphBlockRenderer = class (TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownQuoteBlockRenderer }

  TLaTeXMarkdownQuoteBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownTextBlockRenderer }

  TLaTeXMarkdownTextBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownListBlockRenderer }

  TLaTeXMarkdownListBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownListItemBlockRenderer }

  TLaTeXMarkdownListItemBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownCodeBlockRenderer }

  TLaTeXMarkdownCodeBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownHeadingBlockRenderer }

  TLaTeXMarkdownHeadingBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownThematicBreakBlockRenderer }

  TLaTeXMarkdownThematicBreakBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownTableBlockRenderer }

  TLaTeXMarkdownTableBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownTableRowBlockRenderer }

  TLaTeXMarkdownTableRowBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownFrontmatterBlockRenderer }

  TLaTeXMarkdownFrontmatterBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownDocumentRenderer }

  TLaTeXMarkdownDocumentRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownCommentBlockRenderer }

  TLaTeXMarkdownCommentBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownDefinitionListBlockRenderer }

  TLaTeXMarkdownDefinitionListBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownDefinitionTermBlockRenderer }

  TLaTeXMarkdownDefinitionTermBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownDefinitionBlockRenderer }

  TLaTeXMarkdownDefinitionBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownAlertBlockRenderer }

  TLaTeXMarkdownAlertBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownFootnoteBlockRenderer }

  TLaTeXMarkdownFootnoteBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
  protected
    procedure DoRender(aElement : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  { TLaTeXMarkdownFigureBlockRenderer }

  TLaTeXMarkdownFigureBlockRenderer = class(TLaTeXMarkdownBlockRenderer)
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

{ TLaTeXMarkdownBlockRenderer }

function TLaTeXMarkdownBlockRenderer.GetLaTeXRenderer: TMarkdownLaTeXRenderer;
begin
  if Renderer is TMarkdownLaTeXRenderer then
    Result:=TMarkdownLaTeXRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure TLaTeXMarkdownBlockRenderer.Append(const S: TMarkdownString);
begin
  LaTeXRenderer.Append(S);
end;

procedure TLaTeXMarkdownBlockRenderer.AppendNl(const S: TMarkdownString);
begin
  LaTeXRenderer.AppendNL(S);
end;

function TLaTeXMarkdownBlockRenderer.HasOption(aOption: TLaTeXOption): Boolean;
begin
  Result:=(Self.Renderer is TMarkdownLaTeXRenderer);
  if Result then
    Result:=aOption in TMarkdownLaTeXRenderer(Renderer).Options;
end;

function TLaTeXMarkdownBlockRenderer.Escape(const S: TMarkdownString): TMarkdownString;
begin
  Result:=LaTeXRenderer.EscapeLaTeX(S);
end;


procedure TLaTeXMarkdownBlockRenderer.RenderHeading(aBlock: TMarkdownContainerBlock; aLevel: Integer);

var
  lSection,lNumber: TMarkdownString;
  lNumbered: Boolean;
begin
  lNumbered:=HasOption(loNumberedSections);
  case aLevel of
    1: lSection:='section';
    2: lSection:='subsection';
    3: lSection:='subsubsection';
    4: lSection:='paragraph';
    5: lSection:='subparagraph';
  else
    lSection:='';
  end;
  if lSection='' then
    lSection:='textbf'
  else if not lNumbered then
    lSection:=lSection+'*';
  Append('\'+lSection+'{');
  if not lNumbered then
    begin
    lNumber:=Renderer.GetHeadingNumber(aBlock);
    if lNumber<>'' then
      Append(Escape(lNumber)+' ');
    end;
  Renderer.RenderChildren(aBlock);
  Append('}');
  Append(LaTeXRenderer.LabelFor(aBlock));
  AppendNl;
end;


{ TMarkdownLaTeXRenderer }

procedure TMarkdownLaTeXRenderer.SetHead(const aValue: TStrings);
begin
  if FHead=aValue then Exit;
  FHead:=aValue;
end;

procedure TMarkdownLaTeXRenderer.Append(const aContent: TMarkdownString);
begin
  FBuilder.Append(aContent);
end;

procedure TMarkdownLaTeXRenderer.AppendNL(const aContent: TMarkdownString);
begin
  if aContent<>'' then
    FBuilder.Append(aContent);
  FBuilder.Append(sLineBreak);
end;

constructor TMarkdownLaTeXRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHead:=TStringList.Create;
end;

destructor TMarkdownLaTeXRenderer.Destroy;
begin
  FreeAndNil(FHead);
  inherited destroy;
end;

procedure TMarkdownLaTeXRenderer.RenderDocument(aDocument: TMarkdownDocument);
begin
  FBuilder:=TMarkdownStringBuilder.Create;
  FDocument:=aDocument;
  try
    RenderBlock(aDocument);
    FLaTeX:=BuilderToMarkdown(FBuilder);
  finally
    FDocument:=Nil;
    FreeAndNil(FBuilder);
  end;
end;


procedure TMarkdownLaTeXRenderer.WriteRaw(const aText: TMarkdownString);

begin
  if Assigned(FBuilder) then
    Append(aText);
end;


procedure TMarkdownLaTeXRenderer.TrimTrailingNewLines;

begin
  while (FBuilder.Length>0) and (FBuilder.Chars[FBuilder.Length-1] in [#10,#13]) do
    FBuilder.Length:=FBuilder.Length-1;
end;


function TMarkdownLaTeXRenderer.LabelFor(aBlock: TMarkdownBlock): TMarkdownString;

begin
  if aBlock.ID<>'' then
    Result:='\label{'+aBlock.ID+'}'
  else
    Result:='';
end;


procedure TMarkdownLaTeXRenderer.RenderDocument(aDocument: TMarkdownDocument; aDest: TStrings);
begin
  aDest.Text:=MarkdownToString(RenderLaTeX(aDocument));
end;

procedure TMarkdownLaTeXRenderer.RenderChildren(aBlock: TMarkdownContainerBlock; aAppendNewLine: Boolean);
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

function TMarkdownLaTeXRenderer.RenderLaTeX(aDocument: TMarkdownDocument): TMarkdownString;
begin
  RenderDocument(aDocument);
  Result:=FLaTeX;
  FLaTeX:='';
end;

procedure TMarkdownLaTeXRenderer.RenderToFile(aDocument: TMarkdownDocument; const aFileName: String);
var
  lTeX : TMarkdownString;
  lFile : THandle;
begin
  lTeX:=RenderLaTex(aDocument);
  lFile:=FileCreate(aFileName);
  try
    if lTex<>'' then
      FileWrite(lFile,lTex[1],Length(lTex));
  finally
    FileClose(lFile);
  end;
end;

class procedure TMarkdownLaTeXRenderer.FastRenderToFile(aDocument: TMarkdownDocument; const aFileName: String; aOptions: TLaTeXOptions;
  const aTitle: TMarkdownString; const aAuthor: TMarkdownString);
var
  lRender : TMarkdownLaTexRenderer;
begin
  lRender:=TMarkdownLaTexRenderer.Create(Nil);
  try
    lRender.Options:=aOptions;
    lRender.Title:=aTitle;
    lRender.Author:=aAuthor;
    lRender.RenderToFile(aDocument,aFileName);
  finally
    lRender.Free;
  end;
end;

class function TMarkdownLaTeXRenderer.FastRender(aDocument: TMarkdownDocument; aOptions: TLaTeXOptions; const aTitle: TMarkdownString;
  const aAuthor: TMarkdownString): TMarkdownString;
var
  lRender : TMarkdownLaTexRenderer;
begin
  lRender:=TMarkdownLaTexRenderer.Create(Nil);
  try
    lRender.Options:=aOptions;
    lRender.Title:=aTitle;
    lRender.Author:=aAuthor;
    Result:=lRender.RenderLatex(aDocument);
  finally
    lRender.Free;
  end;
end;

function TMarkdownLaTeXRenderer.EscapeLaTeX(const S: TMarkdownString): TMarkdownString;
var
  i: Integer;
  c: TMarkdownChar;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    case c of
      '\': Result := Result + '\textbackslash{}';
      '{': Result := Result + '\{';
      '}': Result := Result + '\}';
      '$': Result := Result + '\$';
      '&': Result := Result + '\&';
      '#': Result := Result + '\#';
      '^': Result := Result + '\textasciicircum{}';
      '_': Result := Result + '\_';
      '%': Result := Result + '\%';
      '~': Result := Result + '\textasciitilde{}';
      '<': Result := Result + '\textless{}';
      '>': Result := Result + '\textgreater{}';
    else
      Result := Result + c;
    end;
  end;
end;

{ TLaTeXMarkdownTextRenderer }

procedure TLaTeXMarkdownTextRenderer.Append(const S: TMarkdownString);
begin
  LaTeXRenderer.Append(S);
end;

function TLaTeXMarkdownTextRenderer.Escape(const S: TMarkdownString): TMarkdownString;
begin
  Result:=LaTeXRenderer.EscapeLaTeX(S);
end;

function TLaTeXMarkdownTextRenderer.GetLaTeXRenderer: TMarkdownLaTeXRenderer;
begin
  if Renderer is TMarkdownLaTeXRenderer then
    Result:=TMarkdownLaTeXRenderer(Renderer)
  else
    Result:=Nil;
end;

function TLaTeXMarkdownTextRenderer.GetNodeTag(aElement: TMarkdownTextNode; Closing: Boolean): TMarkdownString;
var
  lUrl: TMarkdownString;
begin
  Result := '';
  case aElement.Kind of
    nkCode:
      if Closing then Result := '}' else Result := '\texttt{';
    nkURI, nkEmail:
      begin
        lUrl := '';
        if aElement.HasAttrs then
          aElement.Attrs.TryGet('href', lUrl);

        if Closing then
          Result := '}'
        else
          Result := '\href{' + lUrl + '}{';
      end;
    nkImg:
      begin
        lUrl := '';
        if aElement.HasAttrs then
          aElement.Attrs.TryGet('src', lUrl);

        if Closing then
          Result := ''
        else
          Result := '\includegraphics{' + lUrl + '}';
      end;
    nkLineBreak:
      if Closing then
        Result := ''
      else
        Result := '\\';
  else
    Result:='';
  end;
end;

procedure TLaTeXMarkdownTextRenderer.PushStyle(aStyle: TNodeStyle);
begin
  case aStyle of
    nsStrong: Append('\textbf{');
    nsEmph: Append('\textit{');
    nsDelete: Append('\sout{'); // Requires ulem package
  end;
  if FStyleStackLen=Length(FStyleStack) then
    SetLength(FStyleStack,FStyleStackLen+3);
  FStyleStack[FStyleStackLen]:=aStyle;
  Inc(FStyleStackLen);
end;

procedure TLaTeXMarkdownTextRenderer.PopStyle;
begin
  if FStyleStackLen=0 then
    Exit;
  Dec(FStyleStackLen);
  Append('}');
end;

procedure TLaTeXMarkdownTextRenderer.EmitStyleDiff(const aStyles : TNodeStyleArray);
var
  lKeep,I : Integer;
begin
  lKeep:=0;
  While (lKeep<FStyleStackLen) and (lKeep<Length(aStyles)) and (FStyleStack[lKeep]=aStyles[lKeep]) do
    Inc(lKeep);
  While FStyleStackLen>lKeep do
    Self.PopStyle;
  For I:=lKeep to Length(aStyles)-1 do
    Self.PushStyle(aStyles[I]);
end;

procedure TLaTeXMarkdownTextRenderer.RenderLink(aElement: TMarkdownTextNode);

var
  lHref,lText : TMarkdownString;
  lChild : TMarkdownTextNode;

begin
  lHref:=aElement.Attrs['href'];
  lText:=aElement.NodeText;
  if aElement.HasChildren then
    lText:=lText+aElement.Children.PlainText;
  if Renderer.DoResolveLink(lHref,lText) then
    Exit;
  Append('\href{'+lHref+'}{');
  if (aElement.NodeText='') and not aElement.HasChildren then
    begin
    if lText='' then
      lText:=lHref;
    Append(Escape(lText));
    end
  else
    begin
    if aElement.NodeText<>'' then
      Append(Escape(aElement.NodeText));
    if aElement.HasChildren then
      begin
      for lChild in aElement.Children do
        DoRender(lChild);
      EmitStyleDiff(aElement.StyleList);
      end;
    end;
  Append('}');
end;


procedure TLaTeXMarkdownTextRenderer.RenderFootnoteRef(aElement: TMarkdownTextNode);

var
  lDef : TMarkdownFootnoteBlock;
  lStack : Array of TNodeStyle;
  lStackLen : Integer;

begin
  lDef:=Nil;
  if Assigned(LaTeXRenderer.Document) then
    lDef:=LaTeXRenderer.Document.FindFootnoteDef(aElement.Attrs['label']);
  if lDef=Nil then
    begin
    Append(Escape('[^'+aElement.Attrs['label']+']'));
    Exit;
    end;
  lStack:=Copy(FStyleStack);
  lStackLen:=FStyleStackLen;
  Append('\footnote{');
  Renderer.RenderChildren(lDef);
  LaTeXRenderer.TrimTrailingNewLines;
  Append('}');
  FStyleStack:=lStack;
  FStyleStackLen:=lStackLen;
end;


procedure TLaTeXMarkdownTextRenderer.DoRender(aElement: TMarkdownTextNode);
var
  lChild : TMarkdownTextNode;
begin
  if aElement.Kind=nkComment then
    begin
    Renderer.DoMarkerNode(aElement);
    Exit;
    end;
  Self.EmitStyleDiff(aElement.StyleList);
  if aElement.Kind=nkFootnoteRef then
    begin
    RenderFootnoteRef(aElement);
    Exit;
    end;
  if aElement.Kind in [nkURI,nkEmail] then
    begin
    RenderLink(aElement);
    aElement.Active:=False;
    Exit;
    end;
  if aElement.Kind <> nkText then
    Append(Self.GetNodeTag(aElement, False));

  if not (aElement.Kind in [nkImg,nkLineBreak,nkRaw]) then
    begin
    if aElement.NodeText<>'' then
      Append(Self.Escape(aElement.NodeText));
    if aElement.HasChildren then
      begin
      for lChild in aElement.Children do
        DoRender(lChild);
      Self.EmitStyleDiff(aElement.StyleList);
      end;
    end;

  if aElement.Kind <> nkText then
    Append(Self.GetNodeTag(aElement, True));

  aElement.Active:=False;
end;

procedure TLaTeXMarkdownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  Self.FStyleStackLen:=0;
end;

procedure TLaTeXMarkdownTextRenderer.EndBlock;
begin
  While (Self.FStyleStackLen>0) do
    Self.Popstyle;
  inherited EndBlock;
end;

{ TLaTeXParagraphBlockRenderer }

procedure TLaTeXParagraphBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownParagraphBlock absolute aElement;
begin
  if lNode.Header>0 then
    begin
    RenderHeading(lNode,lNode.Header);
    exit;
    end;
  // LaTeX paragraphs are separated by blank lines.
  // No special environment needed usually, unless we want to enforce spacing.
  Renderer.RenderChildren(lNode);
  AppendNl; // Blank line after paragraph
  AppendNl;
end;

class function TLaTeXParagraphBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownParagraphBlock;
end;

{ TLaTeXMarkdownTextBlockRenderer }

class function TLaTeXMarkdownTextBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTextBlock;
end;

procedure TLaTeXMarkdownTextBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownTextBlock absolute aElement;
begin
  if assigned(lNode) and assigned(lNode.Nodes) then
    Renderer.RenderTextNodes(lNode.Nodes);
end;

{ TLaTeXMarkdownQuoteBlockRenderer }

procedure TLaTeXMarkdownQuoteBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownQuoteBlock absolute aElement;
begin
  AppendNl('\begin{quote}');
  Renderer.RenderChildren(lNode);
  AppendNl('\end{quote}');
end;

class function TLaTeXMarkdownQuoteBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownQuoteBlock;
end;

{ TLaTeXMarkdownListBlockRenderer }

procedure TLaTeXMarkdownListBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lNode : TMarkdownListBlock absolute aElement;
begin
  if not lNode.Ordered then
    AppendNl('\begin{itemize}')
  else
    AppendNl('\begin{enumerate}');

  Renderer.RenderChildren(lNode);

  if lNode.Ordered then
    AppendNl('\end{enumerate}')
  else
    AppendNl('\end{itemize}');
end;

class function TLaTeXMarkdownListBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownListBlock;
end;


{ TLaTeXMarkdownListItemBlockRenderer }

procedure TLaTeXMarkdownListItemBlockRenderer.DoRender(aElement : TMarkdownBlock);
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
  Append('\item ');
  For lBlock in lItemBlock.Blocks do
    if IsPlainBlock(lBlock) then
      LaTeXRenderer.RenderChildren(lPar,True)
    else
      Renderer.RenderBlock(lBlock);
  AppendNl;
end;

class function TLaTeXMarkdownListItemBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownListItemBlock;
end;

{ TLaTeXMarkdownCodeBlockRenderer }

procedure TLaTeXMarkdownCodeBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lNode : TMarkdownCodeBlock absolute aElement;
  lBlock : TMarkdownBlock;
begin
  AppendNl('\begin{verbatim}');
  for lBlock in LNode.Blocks do
    begin
    Renderer.RenderCodeBlock(LBlock,lNode.Lang);
    AppendNl;
    end;
  AppendNl('\end{verbatim}');
end;

class function TLaTeXMarkdownCodeBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownCodeBlock;
end;

{ TLaTeXMarkdownThematicBreakBlockRenderer }

procedure TLaTeXMarkdownThematicBreakBlockRenderer.DoRender(aElement : TMarkdownBlock);
begin
  if Not Assigned(aElement) then
    exit;
  AppendNl('\hrule');
end;

class function TLaTeXMarkdownThematicBreakBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownThematicBreakBlock;
end;

{ TLaTeXMarkdownTableBlockRenderer }

procedure TLaTeXMarkdownTableBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownTableBlock absolute aElement;
  i : integer;
  lCols: TMarkdownString;
  c: TCellAlign;
  lFloat : Boolean;
begin
  // Construct column definition
  lCols := '';
  for c in lNode.Columns do
  begin
    case c of
      caLeft: lCols := lCols + 'l|';
      caRight: lCols := lCols + 'r|';
      caCenter: lCols := lCols + 'c|';
    end;
  end;
  if Length(lCols) > 0 then
    lCols := '|' + lCols;

  lFloat:=Assigned(lNode.Caption) or (lNode.ID<>'');
  if lFloat then
    begin
    AppendNl('\begin{table}[htbp]');
    AppendNl('\centering');
    if Assigned(lNode.Caption) then
      begin
      Append('\caption{');
      Renderer.RenderTextNodes(lNode.Caption);
      AppendNl('}');
      end;
    if lNode.ID<>'' then
      AppendNl(LaTeXRenderer.LabelFor(lNode));
    end;
  AppendNl('\begin{tabular}{' + lCols + '}');
  AppendNl('\hline');

  // Header
  Renderer.RenderBlock(lNode.blocks[0]);
  AppendNl('\hline');

  if lNode.blocks.Count > 1 then
  begin
    for i := 1 to lNode.blocks.Count -1  do
      Renderer.RenderBlock(lnode.blocks[i]);
    AppendNl('\hline');
  end;
  AppendNl('\end{tabular}');
  if lFloat then
    AppendNl('\end{table}');
end;

class function TLaTeXMarkdownTableBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableBlock;
end;

{ TLaTeXMarkdownTableRowBlockRenderer }

procedure TLaTeXMarkdownTableRowBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lNode : TMarkdownTableRowBlock absolute aElement;
  i, lCount : integer;
begin
  lCount:=lNode.blocks.Count;
  for i:=0 to lCount-1 do
    begin
    if i > 0 then Append(' & ');
    Renderer.RenderBlock(lNode.blocks[i]);
    end;
  AppendNl(' \\');
end;

class function TLaTeXMarkdownTableRowBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableRowBlock;
end;

{ TLaTeXMarkdownHeadingBlockRenderer }

procedure TLaTeXMarkdownHeadingBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lNode : TMarkdownHeadingBlock absolute aElement;
begin
  RenderHeading(lNode,lNode.Level);
end;

class function TLaTeXMarkdownHeadingBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownHeadingBlock;
end;


{ TLaTeXMarkdownFrontmatterBlockRenderer }

procedure TLaTeXMarkdownFrontmatterBlockRenderer.DoRender(aElement: TMarkdownBlock);
begin
  // Frontmatter produces no visible output
end;

class function TLaTeXMarkdownFrontmatterBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result := TMarkdownFrontmatterBlock;
end;

{ TLaTeXMarkdownDocumentRenderer }

// Does aBlock contain an alert block ?
function ContainsAlert(aBlock : TMarkdownBlock) : Boolean;

var
  I : Integer;

begin
  Result:=aBlock is TMarkdownAlertBlock;
  I:=0;
  while not Result and (I<aBlock.ChildCount) do
    begin
    Result:=ContainsAlert(aBlock.Children[I]);
    Inc(I);
    end;
end;


procedure TLaTeXMarkdownDocumentRenderer.DoRender(aElement: TMarkdownBlock);
var
  H : String;
begin
  if HasOption(loEnvelope) then
    begin
    AppendNL('\documentclass{article}');
    AppendNL('\usepackage[utf8]{inputenc}');
    AppendNL('\usepackage{graphicx}');
    AppendNL('\usepackage{hyperref}');
    AppendNL('\usepackage{ulem}'); // For strikethrough
    if ContainsAlert(aElement) then
      AppendNL('\newenvironment{mdalert}[2]{\par\noindent\textbf{#2}\par\begin{quote}}{\end{quote}}');

    if LaTeXRenderer.Title<>'' then
      AppendNL('\title{' + LaTeXRenderer.EscapeLaTeX(LaTeXRenderer.Title) + '}');
    if LaTeXRenderer.Author<>'' then
      AppendNL('\author{' + LaTeXRenderer.EscapeLaTeX(LaTeXRenderer.Author) + '}');

    for H in LaTeXRenderer.Head do
      AppendNL(StringToMarkdown(H));

    AppendNL('\begin{document}');

    if LaTeXRenderer.Title<>'' then
      AppendNL('\maketitle');
    end;

  Renderer.RenderChildren(aElement as TMarkdownDocument);

  if HasOption(loEnvelope) then
    begin
    AppendNL('\end{document}');
    end;
end;

class function TLaTeXMarkdownDocumentRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownDocument
end;

{ TLaTeXMarkdownCommentBlockRenderer }

procedure TLaTeXMarkdownCommentBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownCommentBlock absolute aElement;

begin
  if lNode.IsMarker then
    Renderer.DoMarker(lNode.MarkerName,lNode.MarkerArgument,lNode.MarkerValue);
end;


class function TLaTeXMarkdownCommentBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownCommentBlock;
end;

{ TLaTeXMarkdownDefinitionListBlockRenderer }

procedure TLaTeXMarkdownDefinitionListBlockRenderer.DoRender(aElement: TMarkdownBlock);

begin
  AppendNl('\begin{description}');
  Renderer.RenderChildren(aElement as TMarkdownContainerBlock);
  AppendNl('\end{description}');
end;


class function TLaTeXMarkdownDefinitionListBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionListBlock;
end;

{ TLaTeXMarkdownDefinitionTermBlockRenderer }

procedure TLaTeXMarkdownDefinitionTermBlockRenderer.DoRender(aElement: TMarkdownBlock);

begin
  Append('\item[{');
  Renderer.RenderChildren(aElement as TMarkdownContainerBlock);
  Append('}] ');
end;


class function TLaTeXMarkdownDefinitionTermBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionTermBlock;
end;

{ TLaTeXMarkdownDefinitionBlockRenderer }

procedure TLaTeXMarkdownDefinitionBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lDef : TMarkdownDefinitionBlock absolute aElement;
  lList : TMarkdownContainerBlock;
  lBlock : TMarkdownBlock;
  lIdx : Integer;
  lTight : Boolean;

begin
  lList:=lDef.Parent as TMarkdownContainerBlock;
  lIdx:=lList.IndexOfChild(lDef);
  if (lIdx>0) and (lList.Children[lIdx-1] is TMarkdownDefinitionBlock) then
    Append('\item[] ');
  lTight:=(lList is TMarkdownDefinitionListBlock) and not TMarkdownDefinitionListBlock(lList).Loose;
  for lBlock in lDef.Blocks do
    if lTight and (lBlock.ClassType=TMarkdownParagraphBlock) and TMarkdownParagraphBlock(lBlock).IsPlainPara then
      LaTeXRenderer.RenderChildren(TMarkdownParagraphBlock(lBlock),True)
    else
      Renderer.RenderBlock(lBlock);
  AppendNl;
end;


class function TLaTeXMarkdownDefinitionBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionBlock;
end;

{ TLaTeXMarkdownAlertBlockRenderer }

procedure TLaTeXMarkdownAlertBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownAlertBlock absolute aElement;

begin
  Append('\begin{mdalert}{'+AlertTypeNames[lNode.AlertType]+'}{');
  if Assigned(lNode.Title) then
    Renderer.RenderTextNodes(lNode.Title)
  else
    Append(Escape(Renderer.AlertTitles[lNode.AlertType]));
  AppendNl('}');
  Renderer.RenderChildren(lNode);
  AppendNl('\end{mdalert}');
end;


class function TLaTeXMarkdownAlertBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownAlertBlock;
end;

{ TLaTeXMarkdownFootnoteBlockRenderer }

procedure TLaTeXMarkdownFootnoteBlockRenderer.DoRender(aElement: TMarkdownBlock);

begin
  if aElement=Nil then ; // Silence warning
end;


class function TLaTeXMarkdownFootnoteBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownFootnoteBlock;
end;

{ TLaTeXMarkdownFigureBlockRenderer }

procedure TLaTeXMarkdownFigureBlockRenderer.DoRender(aElement: TMarkdownBlock);

var
  lNode : TMarkdownFigureBlock absolute aElement;
  lWidth,lOptions : TMarkdownString;
  lPercent : Double;

begin
  lOptions:='';
  if lNode.HasAttrs then
    begin
    lWidth:=StringToMarkdown(lNode.Attrs.Values['width']);
    if lWidth.EndsWith('%') and TryStrToFloat(MarkdownToString(Copy(lWidth,1,Length(lWidth)-1)),lPercent,DefaultFormatSettings) then
      lOptions:='[width='+StringToMarkdown(FloatToStr(lPercent/100,DefaultFormatSettings))+'\textwidth]'
    else if lWidth<>'' then
      lOptions:='[width='+lWidth+']';
    end;
  AppendNl('\begin{figure}[htbp]');
  AppendNl('\centering');
  if Assigned(lNode.Image) then
    AppendNl('\includegraphics'+lOptions+'{'+lNode.Image.Attrs['src']+'}');
  if Assigned(lNode.Caption) and (lNode.Caption.Count>0) then
    begin
    Append('\caption{');
    Renderer.RenderTextNodes(lNode.Caption);
    AppendNl('}');
    end;
  if lNode.ID<>'' then
    AppendNl(LaTeXRenderer.LabelFor(lNode));
  AppendNl('\end{figure}');
end;


class function TLaTeXMarkdownFigureBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownFigureBlock;
end;


initialization
  TLaTeXMarkdownHeadingBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXParagraphBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownQuoteBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownTextBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownListBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownListItemBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownCodeBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownThematicBreakBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownTableBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownTableRowBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownFrontmatterBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownDocumentRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownCommentBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownDefinitionListBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownDefinitionTermBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownDefinitionBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownAlertBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownFootnoteBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownFigureBlockRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
  TLaTeXMarkdownTextRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
end.
