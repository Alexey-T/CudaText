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
    FBuilder: TStringBuilder;
    FHead: TStrings;
    FLaTeX: String;
    FOptions: TLaTeXOptions;
    FTitle: String;
    FAuthor: String;
    procedure SetHead(const aValue: TStrings);
  Protected
    Procedure Append(const aContent : String);
    Procedure AppendNL(const aContent : String = '');
    Property Builder : TStringBuilder Read FBuilder;
    function EscapeLaTeX(const S: String): String;
  public
    constructor Create(aOwner : TComponent); override;
    destructor destroy; override;
    Procedure RenderDocument(aDocument : TMarkdownDocument); override;overload;
    Procedure RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings); overload;
    procedure RenderChildren(aBlock : TMarkdownContainerBlock; aAppendNewLine : Boolean); overload;
    function RenderLaTeX(aDocument : TMarkdownDocument) : string;
    Procedure RenderToFile(aDocument : TMarkdownDocument; const aFileName : string);
    class procedure FastRenderToFile(aDocument : TMarkdownDocument; const aFileName : string; aOptions : TLaTeXOptions = []; const aTitle : String = ''; const aAuthor : string = '');
    class function FastRender(aDocument : TMarkdownDocument; aOptions : TLaTeXOptions = []; const aTitle : String = ''; const aAuthor : string = '') : string;
  published
    Property Options : TLaTeXOptions Read FOptions Write FOptions;
    property Title : String Read FTitle Write FTitle;
    property Author : String Read FAuthor Write FAuthor;
    property Head : TStrings Read FHead Write SetHead;
  end;

  { TLaTeXMarkdownBlockRenderer }

  TLaTeXMarkdownBlockRenderer = Class (TMarkdownBlockRenderer)
  Private
    function GetLaTeXRenderer: TMarkdownLaTeXRenderer;
  protected
    procedure Append(const S : String); inline;
    procedure AppendNl(const S : String = ''); inline;
    function HasOption(aOption : TLaTeXOption) : Boolean;
    function Escape(const S: String): String;
  public
    property LaTeXRenderer : TMarkdownLaTeXRenderer Read GetLaTeXRenderer;
  end;
  TLaTeXMarkdownBlockRendererClass = class of TLaTeXMarkdownBlockRenderer;

  { TLaTeXMarkdownTextRenderer }

  TLaTeXMarkdownTextRenderer = class(TMarkdownTextRenderer)
  Private
    FStyleStack: Array of TNodeStyle;
    FStyleStackLen : Integer;
    FLastStyles : TNodeStyles;
    function GetLaTeXRenderer: TMarkdownLaTeXRenderer;
    function GetNodeTag(aElement: TMarkdownTextNode; Closing: Boolean): string;
  protected
    procedure PushStyle(aStyle : TNodeStyle);
    function PopStyles(aStyle: TNodeStyles): TNodeStyle;
    procedure PopStyle(aStyle : TNodeStyle);
    procedure Append(const S : String); inline;
    procedure DoRender(aElement: TMarkdownTextNode); override;
    function Escape(const S: String): String;
    procedure EmitStyleDiff(aStyles : TNodeStyles);
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
    procedure dorender(aElement : TMarkdownBlock); override;
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

{ TLaTeXMarkdownBlockRenderer }

function TLaTeXMarkdownBlockRenderer.GetLaTeXRenderer: TMarkdownLaTeXRenderer;
begin
  if Renderer is TMarkdownLaTeXRenderer then
    Result:=TMarkdownLaTeXRenderer(Renderer)
  else
    Result:=Nil;
end;

procedure TLaTeXMarkdownBlockRenderer.Append(const S: String);
begin
  LaTeXRenderer.Append(S);
end;

procedure TLaTeXMarkdownBlockRenderer.AppendNl(const S: String);
begin
  LaTeXRenderer.AppendNL(S);
end;

function TLaTeXMarkdownBlockRenderer.HasOption(aOption: TLaTeXOption): Boolean;
begin
  Result:=(Self.Renderer is TMarkdownLaTeXRenderer);
  if Result then
    Result:=aOption in TMarkdownLaTeXRenderer(Renderer).Options;
end;

function TLaTeXMarkdownBlockRenderer.Escape(const S: String): String;
begin
  Result:=LaTeXRenderer.EscapeLaTeX(S);
end;

{ TMarkdownLaTeXRenderer }

procedure TMarkdownLaTeXRenderer.SetHead(const aValue: TStrings);
begin
  if FHead=aValue then Exit;
  FHead:=aValue;
end;

procedure TMarkdownLaTeXRenderer.Append(const aContent: String);
begin
  FBuilder.Append(aContent);
end;

procedure TMarkdownLaTeXRenderer.AppendNL(const aContent: String);
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

destructor TMarkdownLaTeXRenderer.destroy;
begin
  FreeAndNil(FHead);
  inherited destroy;
end;

procedure TMarkdownLaTeXRenderer.RenderDocument(aDocument: TMarkdownDocument);
begin
  FBuilder:=TStringBuilder.Create;
  try
    RenderBlock(aDocument);
    FLaTeX:=FBuilder.ToString;
  finally
    FreeAndNil(FBuilder);
  end;
end;

procedure TMarkdownLaTeXRenderer.RenderDocument(aDocument: TMarkdownDocument; aDest: TStrings);
begin
  aDest.Text:=RenderLaTeX(aDocument);
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

function TMarkdownLaTeXRenderer.RenderLaTeX(aDocument: TMarkdownDocument): string;
begin
  RenderDocument(aDocument);
  Result:=FLaTeX;
  FLaTeX:='';
end;

procedure TMarkdownLaTeXRenderer.RenderToFile(aDocument: TMarkdownDocument; const aFileName: string);
var
  lTeX : String;
  lFile : THandle;
begin
  lTeX:=RenderLaTex(aDocument);
  lFile:=FileCreate(aFileName);
  try
    if lTex<>'' then
      FileWrite(lFile,lTex[1],Length(lTex)*SizeOf(Char));
  finally
    FileClose(lFile);
  end;
end;

class procedure TMarkdownLaTeXRenderer.FastRenderToFile(aDocument: TMarkdownDocument; const aFileName: string; aOptions: TLaTeXOptions;
  const aTitle: String; const aAuthor: string);
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

class function TMarkdownLaTeXRenderer.FastRender(aDocument: TMarkdownDocument; aOptions: TLaTeXOptions; const aTitle: String;
  const aAuthor: string): string;
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

function TMarkdownLaTeXRenderer.EscapeLaTeX(const S: String): String;
var
  i: Integer;
  c: Char;
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
    else
      Result := Result + c;
    end;
  end;
end;

{ TLaTeXMarkdownTextRenderer }

procedure TLaTeXMarkdownTextRenderer.Append(const S: String);
begin
  LaTeXRenderer.Append(S);
end;

function TLaTeXMarkdownTextRenderer.Escape(const S: String): String;
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

function TLaTeXMarkdownTextRenderer.GetNodeTag(aElement: TMarkdownTextNode; Closing: Boolean): string;
var
  lUrl: String;
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
          Result := '}'
        else
          Result := '\includegraphics{' + lUrl + '}{';
      end;
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

function TLaTeXMarkdownTextRenderer.Popstyles(aStyle: TNodeStyles) : TNodeStyle;
begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1] in aStyle) then
    begin
    Result:=FStyleStack[FStyleStackLen-1];
    Append('}');
    Dec(FStyleStackLen);
    end;
end;

procedure TLaTeXMarkdownTextRenderer.PopStyle(aStyle: TNodeStyle);
begin
  if (FStyleStackLen>0) and (FStyleStack[FStyleStackLen-1]=aStyle) then
    begin
    Append('}');
    Dec(FStyleStackLen);
    end;
end;

procedure TLaTeXMarkdownTextRenderer.EmitStyleDiff(aStyles : TNodeStyles);
var
  lRemove : TNodeStyles;
  lAdd : TNodeStyles;
  S : TNodeStyle;
begin
  lRemove:=[];
  lAdd:=[];
  For S in TNodeStyle do
    begin
    if (S in Self.FLastStyles) and Not (S in aStyles) then
      Include(lRemove,S);
    if (S in aStyles) and Not (S in Self.FLastStyles) then
      Include(lAdd,S);
    end;
  While lRemove<>[] do
    begin
    S:=Self.PopStyles(lRemove);
    Exclude(lRemove,S);
    end;
  For S in TNodeStyle do
    if S in lAdd then
      Self.PushStyle(S);
  Self.FLastStyles:=aStyles;
end;

procedure TLaTeXMarkdownTextRenderer.DoRender(aElement: TMarkdownTextNode);
var
  lTag : string;
begin
  Self.EmitStyleDiff(aElement.Styles);
  if aElement.Kind <> nkText then
  begin
    lTag := Self.GetNodeTag(aElement, False);
    Append(lTag);
  end;

  if aElement.Kind = nkImg then
  begin
     // Img handling logic here...
  end;

  if aElement.NodeText<>'' then
    Append(Self.Escape(aElement.NodeText));

  if aElement.Kind <> nkText then
  begin
    lTag := Self.GetNodeTag(aElement, True);
    Append(lTag);
  end;

  aElement.Active:=False;
end;

procedure TLaTeXMarkdownTextRenderer.BeginBlock;
begin
  inherited BeginBlock;
  Self.FStyleStackLen:=0;
  Self.FLastStyles:=[];
end;

procedure TLaTeXMarkdownTextRenderer.EndBlock;
begin
  While (Self.FStyleStackLen>0) do
    Self.Popstyle(Self.FStyleStack[Self.FStyleStackLen-1]);
  Self.FLastStyles:=[];
  inherited EndBlock;
end;

{ TLaTeXParagraphBlockRenderer }

procedure TLaTeXParagraphBlockRenderer.DoRender(aElement: TMarkdownBlock);
var
  lNode : TMarkdownParagraphBlock absolute aElement;
begin
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

procedure TLaTeXMarkdownQuoteBlockRenderer.dorender(aElement: TMarkdownBlock);
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
  lCols: String;
  c: TCellAlign;
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
  AppendNl(' \');
end;

class function TLaTeXMarkdownTableRowBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableRowBlock;
end;

{ TLaTeXMarkdownHeadingBlockRenderer }

procedure TLaTeXMarkdownHeadingBlockRenderer.DoRender(aElement : TMarkdownBlock);
var
  lNode : TMarkdownHeadingBlock absolute aElement;
  lSection: String;
  lNumbered: Boolean;
begin
  lNumbered := HasOption(loNumberedSections);
  case lNode.Level of
    1: lSection := 'section';
    2: lSection := 'subsection';
    3: lSection := 'subsubsection';
    4: lSection := 'paragraph';
    5: lSection := 'subparagraph';
    else lSection := 'textbf'; // Fallback
  end;

  if not lNumbered then
    lSection := lSection + '*';

  Append('\' + lSection + '{');
  Renderer.RenderChildren(lNode);
  Append('}');
  AppendNl;
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

    if LaTeXRenderer.Title<>'' then
      AppendNL('\title{' + LaTeXRenderer.EscapeLaTeX(LaTeXRenderer.Title) + '}');
    if LaTeXRenderer.Author<>'' then
      AppendNL('\author{' + LaTeXRenderer.EscapeLaTeX(LaTeXRenderer.Author) + '}');

    for H in LaTeXRenderer.Head do
      AppendNL(H);

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
  TLaTeXMarkdownTextRenderer.RegisterRenderer(TMarkdownLaTeXRenderer);
end.
