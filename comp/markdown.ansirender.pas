{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown ANSI terminal renderer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.ANSIRender;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, System.Console.Ansi,
{$ELSE}
  Classes, SysUtils, Contnrs, FPAnsi,
{$ENDIF}
  Markdown.Elements,
  Markdown.Render,
  Markdown.Utils;

type
  // Style flags collected for a text run.
  TRunStyle = record
    Bold, Italic, Underline, Strike, Inverse, Faint : Boolean;
    Fg : Integer;     // 256-color code, or -1 for the terminal default
    Href : TMarkdownString;    // when set (and Hyperlinks is on) emitted as an OSC 8 link
  end;

  TLinePrefix = record
    Vis : Integer;    // visible width of the prefix
    Styled : TMarkdownString;  // styled text emitted at the start of every line
  end;

  { TMarkDownANSIRenderer }

  TMarkDownANSIRenderer = class(TMarkdownRenderer)
  private
    FOutput : TStrings;
    FResult : TStringList; 
    FWidth : Integer;
    FUseColor : Boolean;
    FHyperlinks : Boolean;
    // line layout state
    FLine : TMarkdownString;
    FCol : Integer;
    FLineStartCol : Integer; 
    FStarted : Boolean;   
    FPendingSpace : Boolean;
    // line-start prefixes (indents, quote bars, ...)
    FPrefix : array of TLinePrefix;
    FMarker : TMarkdownString;
    FMarkerVis : Integer;
    // inherited inline style (set by heading/quote block renderers)
    FBaseStyle : TRunStyle;
    FInListItem : Boolean; 
    procedure StartLine;
    procedure PlaceStyled(const aVisible, aStyled : TMarkdownString);
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    // Rendering
    procedure RenderDocument(aDocument : TMarkdownDocument); override; overload;
    procedure RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings); overload;
    function RenderToString(aDocument : TMarkdownDocument) : TMarkdownString;
    // Helpers used by the block/text renderers
    function VisibleWidth(const aText : TMarkdownString) : Integer;
    function StyleText(const aText : TMarkdownString; const aStyle : TRunStyle) : TMarkdownString;
    function DefaultStyle : TRunStyle;
    // Writer primitives
    procedure NewLine;
    procedure FlushLine;
    procedure BlankLine;
    procedure AddRun(const aText : TMarkdownString; const aStyle : TRunStyle; aWordWrap : Boolean);
    procedure AddRaw(const aVisible, aStyled : TMarkdownString);
    procedure PushPrefix(aVis : Integer; const aStyled : TMarkdownString);
    procedure PopPrefix;
    procedure SetMarker(const aStyled : TMarkdownString; aVis : Integer);
    function PrefixWidth : Integer;
    procedure RenderInline(aNode : TMarkdownTextNode);
    // Write aText as-is; it counts as visible text.
    procedure WriteRaw(const aText : TMarkdownString); override;
    // Render the referenced footnotes of aDocument as a numbered list.
    procedure RenderFootnotes(aDocument : TMarkdownDocument); virtual;
    // Theme/state accessed by block renderers
    property BaseStyle : TRunStyle read FBaseStyle write FBaseStyle;
    property InListItem : Boolean read FInListItem write FInListItem;
    property Output : TStrings read FOutput;
    // Result of the parameterless RenderDocument
    property Lines : TStringList read FResult;
  published
    property Width : Integer read FWidth write FWidth;
    property UseColor : Boolean read FUseColor write FUseColor;
    property Hyperlinks : Boolean read FHyperlinks write FHyperlinks;
  end;

  { TANSIBlockRenderer }

  TANSIBlockRenderer = class(TMarkdownBlockRenderer)
  protected
    function ANSI : TMarkDownANSIRenderer; inline;
    // Write aBlock as a heading
    procedure RenderHeading(aBlock : TMarkdownContainerBlock);
  end;

implementation

const
  // 256-color palette codes (0..15 = the terminal's themed 16 colors)
  clHeading = 12; // bright blue
  clLink    = 14; // bright cyan
  clCodeInl = 11; // bright yellow
  clCodeBlk = 10; // bright green
  clBullet  = 4;  // blue
  clQuote   = 2;  // green
  clRule    = 8;  // bright black / grey

type
  { Block renderers }

  TANSIParagraphRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSITextBlockRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIHeadingRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIQuoteRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIListRenderer = class(TANSIBlockRenderer)
  private
    FItemNumber : Integer;
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    property ItemNumber : Integer read FItemNumber;
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIListItemRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSICodeRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIThematicBreakRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSITableRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIFrontmatterRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSICommentRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIDefinitionListRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIDefinitionTermRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIDefinitionRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIAlertRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIFootnoteRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIFigureRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

function Spaces(aCount : Integer) : TMarkdownString;
begin
  if aCount<=0 then
    Result:=''
  else
    Result:=StringOfChar(' ',aCount);
end;

{ TMarkDownANSIRenderer }

constructor TMarkDownANSIRenderer.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FWidth:=80;
  FUseColor:=True;
  FHyperlinks:=True;
  FResult:=TStringList.Create;
  FBaseStyle:=DefaultStyle;
end;


destructor TMarkDownANSIRenderer.Destroy;
begin
  FreeAndNil(FResult);
  inherited Destroy;
end;


function TMarkDownANSIRenderer.DefaultStyle : TRunStyle;
begin
  Result:=Default(TRunStyle);
  Result.Fg:=-1;
end;


function TMarkDownANSIRenderer.VisibleWidth(const aText : TMarkdownString) : Integer;
var
  i : Integer;
begin
  // Count UTF-8 codepoints (ignore continuation bytes). ANSI escapes are never
  // passed here; callers always measure the plain visible text.
  Result:=0;
  for i:=1 to Length(aText) do
    if (Ord(aText[i]) and $C0)<>$80 then
      Inc(Result);
end;


function TMarkDownANSIRenderer.StyleText(const aText : TMarkdownString; const aStyle : TRunStyle) : TMarkdownString;
const
  // Styled in place of the text, which is then put in its position.
  cPlaceholder = #1;
var
  a : TAnsi;
  lVis : TMarkdownString;
  lPos : Integer;
begin
  lVis:=aText;
  if (aStyle.Href<>'') and FHyperlinks then
    lVis:=StringToMarkdown(TAnsi.Hyperlink(MarkdownToString(aStyle.Href),MarkdownToString(aText)));
  if not FUseColor then
    Exit(lVis);
  if not (aStyle.Bold or aStyle.Italic or aStyle.Underline or aStyle.Strike
          or aStyle.Inverse or aStyle.Faint) and (aStyle.Fg<0) then
    Exit(lVis);
  a:=cPlaceholder;
  if aStyle.Bold then a:=a.Bold;
  if aStyle.Faint then a:=a.Faint;
  if aStyle.Italic then a:=a.Italic;
  if aStyle.Underline then a:=a.Underline;
  if aStyle.Inverse then a:=a.Inverse;
  if aStyle.Strike then a:=a.Strikethrough;
  if aStyle.Fg>=0 then a:=a.Fg(aStyle.Fg);
  Result:=StringToMarkdown(a.ToString);
  lPos:=Pos(cPlaceholder,Result);
  Result:=Copy(Result,1,lPos-1)+lVis+Copy(Result,lPos+1,Length(Result)-lPos);
end;


function TMarkDownANSIRenderer.PrefixWidth : Integer;
var
  i : Integer;
begin
  Result:=0;
  for i:=0 to High(FPrefix) do
    Inc(Result,FPrefix[i].Vis);
end;


procedure TMarkDownANSIRenderer.StartLine;
var
  i : Integer;
begin
  FLine:='';
  FCol:=0;
  for i:=0 to High(FPrefix) do
    if (i=High(FPrefix)) and (FMarker<>'') then
      begin
      FLine:=FLine+FMarker;
      Inc(FCol,FMarkerVis);
      end
    else
      begin
      FLine:=FLine+FPrefix[i].Styled;
      Inc(FCol,FPrefix[i].Vis);
      end;
  FMarker:='';
  FLineStartCol:=FCol;
  FStarted:=True;
end;


procedure TMarkDownANSIRenderer.NewLine;
begin
  if not FStarted then
    StartLine;
  FOutput.Add(MarkdownToString(FLine));
  FLine:='';
  FCol:=0;
  FStarted:=False;
  FPendingSpace:=False;
end;


procedure TMarkDownANSIRenderer.FlushLine;
begin
  if FStarted then
    NewLine;
end;


procedure TMarkDownANSIRenderer.BlankLine;
begin
  FlushLine;
  if (FOutput.Count>0) and (FOutput[FOutput.Count-1]<>'') then
    FOutput.Add('');
end;


procedure TMarkDownANSIRenderer.PlaceStyled(const aVisible, aStyled : TMarkdownString);
var
  w : Integer;
begin
  w:=VisibleWidth(aVisible);
  if not FStarted then
    StartLine;
  if FCol>FLineStartCol then
    begin
    if FPendingSpace then
      begin
      if FCol+1+w>FWidth then
        begin NewLine; StartLine; end
      else
        begin FLine:=FLine+' '; Inc(FCol); end;
      end
    else if FCol+w>FWidth then
      begin NewLine; StartLine; end;
    end;
  FLine:=FLine+aStyled;
  Inc(FCol,w);
  FPendingSpace:=False;
end;


procedure TMarkDownANSIRenderer.AddRun(const aText : TMarkdownString; const aStyle : TRunStyle; aWordWrap : Boolean);
var
  i : Integer;
  c : TMarkdownChar;
  lWord : TMarkdownString;
begin
  if aText='' then
    exit;
  if not aWordWrap then
    begin
    PlaceStyled(aText,StyleText(aText,aStyle));
    exit;
    end;
  lWord:='';
  for i:=1 to Length(aText) do
    begin
    c:=aText[i];
    if (c=' ') or (c=#9) or (c=#10) or (c=#13) then
      begin
      if lWord<>'' then
        begin
        PlaceStyled(lWord,StyleText(lWord,aStyle));
        lWord:='';
        end;
      FPendingSpace:=True;
      end
    else
      lWord:=lWord+c;
    end;
  if lWord<>'' then
    PlaceStyled(lWord,StyleText(lWord,aStyle));
end;


procedure TMarkDownANSIRenderer.AddRaw(const aVisible, aStyled : TMarkdownString);
begin
  if not FStarted then
    StartLine;
  FLine:=FLine+aStyled;
  Inc(FCol,VisibleWidth(aVisible));
end;


procedure TMarkDownANSIRenderer.PushPrefix(aVis : Integer; const aStyled : TMarkdownString);
begin
  SetLength(FPrefix,Length(FPrefix)+1);
  FPrefix[High(FPrefix)].Vis:=aVis;
  FPrefix[High(FPrefix)].Styled:=aStyled;
end;


procedure TMarkDownANSIRenderer.PopPrefix;
begin
  if Length(FPrefix)>0 then
    SetLength(FPrefix,Length(FPrefix)-1);
end;


procedure TMarkDownANSIRenderer.SetMarker(const aStyled : TMarkdownString; aVis : Integer);
begin
  FMarker:=aStyled;
  FMarkerVis:=aVis;
end;


procedure TMarkDownANSIRenderer.RenderInline(aNode : TMarkdownTextNode);
var
  lStyle,lSaveBase : TRunStyle;
  lText,lHref : TMarkdownString;
  lChild : TMarkdownTextNode;
begin
  if not assigned(aNode) then
    exit;
  lStyle:=FBaseStyle;
  if nsStrong in aNode.Styles then lStyle.Bold:=True;
  if nsEmph in aNode.Styles then lStyle.Italic:=True;
  if nsDelete in aNode.Styles then lStyle.Strike:=True;
  case aNode.Kind of
    nkText:
      AddRun(aNode.NodeText,lStyle,True);
    nkCode:
      begin
      lStyle.Fg:=clCodeInl;
      AddRun(aNode.NodeText,lStyle,False);
      end;
    nkURI,nkEmail:
      begin
      lHref:=aNode.Attrs['href'];
      lText:=aNode.NodeText;
      if aNode.HasChildren then
        lText:=lText+aNode.Children.PlainText;
      if DoResolveLink(lHref,lText) then
        Exit;
      lStyle.Underline:=True;
      lStyle.Fg:=clLink;
      lStyle.Href:=lHref;
      if (aNode.NodeText='') and not aNode.HasChildren then
        begin
        if lText='' then
          lText:=lHref;
        AddRun(lText,lStyle,False);
        end
      else
        AddRun(aNode.NodeText,lStyle,False);
      if aNode.HasChildren then
        begin
        lSaveBase:=FBaseStyle;
        FBaseStyle:=lStyle;
        for lChild in aNode.Children do
          RenderInline(lChild);
        FBaseStyle:=lSaveBase;
        end;
      end;
    nkComment:
      DoMarkerNode(aNode);
    nkFootnoteRef:
      begin
      lText:=aNode.Attrs['number'];
      if lText='' then
        lText:=aNode.Attrs['label'];
      lStyle.Fg:=clLink;
      AddRun('['+lText+']',lStyle,False);
      end;
    nkImg:
      begin
      lText:=aNode.Attrs['alt'];
      if lText='' then
        lText:=aNode.Attrs['src'];
      lStyle.Faint:=True;
      AddRun('[image: '+lText+']',lStyle,True);
      end;
    nkLineBreak:
      NewLine;
    nkDirective,nkWikiLink:
      begin
      if aNode.Kind=nkWikiLink then
        lStyle.Fg:=clLink;
      if aNode.NodeText<>'' then
        AddRun(aNode.NodeText,lStyle,True);
      if aNode.HasChildren then
        begin
        lSaveBase:=FBaseStyle;
        FBaseStyle:=lStyle;
        for lChild in aNode.Children do
          RenderInline(lChild);
        FBaseStyle:=lSaveBase;
        end;
      end;
  end;
end;


procedure TMarkDownANSIRenderer.RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings);
begin
  FOutput:=aDest;
  SetLength(FPrefix,0);
  FMarker:='';
  FLine:='';
  FCol:=0;
  FStarted:=False;
  FPendingSpace:=False;
  FBaseStyle:=DefaultStyle;
  RenderChildren(aDocument);
  FlushLine;
  RenderFootnotes(aDocument);
  // drop a trailing blank line if present
  while (FOutput.Count>0) and (FOutput[FOutput.Count-1]='') do
    FOutput.Delete(FOutput.Count-1);
end;


procedure TMarkDownANSIRenderer.WriteRaw(const aText : TMarkdownString);

begin
  if Assigned(FOutput) then
    AddRaw(aText,aText);
end;


procedure TMarkDownANSIRenderer.RenderFootnotes(aDocument : TMarkdownDocument);

var
  lBlock : TMarkdownBlock;
  lMarker : TMarkdownString;
  lWasInItem : Boolean;
begin
  if aDocument.Footnotes.Count=0 then
    exit;
  BlankLine;
  lWasInItem:=FInListItem;
  FInListItem:=True;
  for lBlock in aDocument.Footnotes do
    begin
    lMarker:='['+IntToMarkdown(TMarkdownFootnoteBlock(lBlock).Number)+'] ';
    PushPrefix(VisibleWidth(lMarker),Spaces(VisibleWidth(lMarker)));
    SetMarker(lMarker,VisibleWidth(lMarker));
    RenderChildren(TMarkdownFootnoteBlock(lBlock));
    FlushLine;
    PopPrefix;
    end;
  FInListItem:=lWasInItem;
end;


procedure TMarkDownANSIRenderer.RenderDocument(aDocument : TMarkdownDocument);
begin
  FResult.Clear;
  RenderDocument(aDocument,FResult);
end;


function TMarkDownANSIRenderer.RenderToString(aDocument : TMarkdownDocument) : TMarkdownString;
var
  L : TStringList;
begin
  L:=TStringList.Create;
  try
    RenderDocument(aDocument,L);
    Result:=StringToMarkdown(L.Text);
  finally
    L.Free;
  end;
end;


{ TANSIBlockRenderer }

function TANSIBlockRenderer.ANSI : TMarkDownANSIRenderer;
begin
  Result:=TMarkDownANSIRenderer(Renderer);
end;


procedure TANSIBlockRenderer.RenderHeading(aBlock : TMarkdownContainerBlock);

var
  lSaved, lStyle : TRunStyle;
  lNumber : TMarkdownString;
begin
  ANSI.BlankLine;
  lSaved:=ANSI.BaseStyle;
  lStyle:=lSaved;
  lStyle.Bold:=True;
  lStyle.Fg:=clHeading;
  ANSI.BaseStyle:=lStyle;
  lNumber:=Renderer.GetHeadingNumber(aBlock);
  if lNumber<>'' then
    ANSI.AddRun(lNumber+' ',lStyle,True);
  Renderer.RenderChildren(aBlock);
  ANSI.BaseStyle:=lSaved;
  ANSI.FlushLine;
  ANSI.BlankLine;
end;


{ TANSIParagraphRenderer }

procedure TANSIParagraphRenderer.DoRender(aBlock : TMarkdownBlock);
begin
  if TMarkdownParagraphBlock(aBlock).Header>0 then
    begin
    RenderHeading(TMarkdownParagraphBlock(aBlock));
    exit;
    end;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.FlushLine;
  if not ANSI.InListItem then
    ANSI.BlankLine;
end;

class function TANSIParagraphRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownParagraphBlock;
end;


{ TANSITextBlockRenderer }

procedure TANSITextBlockRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lTextBlock : TMarkdownTextBlock;
  i : Integer;
begin
  if not (aBlock is TMarkdownTextBlock) then
    exit;
  lTextBlock:=TMarkdownTextBlock(aBlock);
  if not assigned(lTextBlock.Nodes) then
    exit;
  for i:=0 to lTextBlock.Nodes.Count-1 do
    ANSI.RenderInline(lTextBlock.Nodes[i]);
end;

class function TANSITextBlockRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownTextBlock;
end;


{ TANSIHeadingRenderer }

procedure TANSIHeadingRenderer.DoRender(aBlock : TMarkdownBlock);
begin
  RenderHeading(aBlock as TMarkdownContainerBlock);
end;

class function TANSIHeadingRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownHeadingBlock;
end;


{ TANSIQuoteRenderer }

procedure TANSIQuoteRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lSaved : TRunStyle;
  lBarStyle : TRunStyle;
begin
  ANSI.BlankLine;
  lBarStyle:=ANSI.DefaultStyle;
  lBarStyle.Fg:=clQuote;
  ANSI.PushPrefix(2,ANSI.StyleText('| ',lBarStyle));
  lSaved:=ANSI.BaseStyle;
  lBarStyle:=lSaved;
  lBarStyle.Faint:=True;
  lBarStyle.Fg:=clQuote;
  ANSI.BaseStyle:=lBarStyle;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.BaseStyle:=lSaved;
  ANSI.PopPrefix;
  ANSI.BlankLine;
end;

class function TANSIQuoteRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownQuoteBlock;
end;


{ TANSIListRenderer }

procedure TANSIListRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lList : TMarkdownListBlock;
  i : Integer;
begin
  lList:=aBlock as TMarkdownListBlock;
  ANSI.BlankLine;
  FItemNumber:=0;
  for i:=0 to aBlock.ChildCount-1 do
    begin
    if lList.Ordered then
      FItemNumber:=lList.Start+i;
    Renderer.RenderBlock(aBlock.Children[i]);
    end;
  ANSI.BlankLine;
end;

class function TANSIListRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownListBlock;
end;


{ TANSIListItemRenderer }

procedure TANSIListItemRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lList : TANSIListRenderer;
  lMarker : TMarkdownString;
  lMarkerVis : Integer;
  lStyle : TRunStyle;
  lWasInItem : Boolean;
begin
  lList:=TANSIListRenderer(GetFirstParentWithClass(TANSIListRenderer));
  if Assigned(lList) and (lList.ItemNumber>0) then
    lMarker:=IntToMarkdown(lList.ItemNumber)+'. '
  else
    lMarker:='* ';
  lMarkerVis:=ANSI.VisibleWidth(lMarker);
  lStyle:=ANSI.DefaultStyle;
  lStyle.Fg:=clBullet;
  lStyle.Bold:=True;
  ANSI.PushPrefix(lMarkerVis,Spaces(lMarkerVis));
  ANSI.SetMarker(ANSI.StyleText(lMarker,lStyle),lMarkerVis);
  lWasInItem:=ANSI.InListItem;
  ANSI.InListItem:=True;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.InListItem:=lWasInItem;
  ANSI.FlushLine;
  ANSI.PopPrefix;
end;

class function TANSIListItemRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownListItemBlock;
end;


{ TANSICodeRenderer }

procedure TANSICodeRenderer.DoRender(aBlock : TMarkdownBlock);
var
  i,j : Integer;
  lChild : TMarkdownBlock;
  lLine : TMarkdownString;
  lStyle : TRunStyle;
begin
  ANSI.BlankLine;
  ANSI.PushPrefix(4,Spaces(4));
  lStyle:=ANSI.DefaultStyle;
  lStyle.Fg:=clCodeBlk;
  for i:=0 to aBlock.ChildCount-1 do
    begin
    lChild:=aBlock.Children[i];
    lLine:='';
    if lChild is TMarkdownTextBlock then
      for j:=0 to TMarkdownTextBlock(lChild).Nodes.Count-1 do
        lLine:=lLine+TMarkdownTextBlock(lChild).Nodes[j].NodeText;
    // verbatim: no wrapping, no entity resolution, exact whitespace
    ANSI.AddRun(lLine,lStyle,False);
    ANSI.NewLine;
    end;
  ANSI.PopPrefix;
  ANSI.BlankLine;
end;

class function TANSICodeRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownCodeBlock;
end;


{ TANSIThematicBreakRenderer }

procedure TANSIThematicBreakRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lStyle : TRunStyle;
  lWidth : Integer;
  lRule : TMarkdownString;
begin
  if aBlock=nil then ;
  ANSI.BlankLine;
  lStyle:=ANSI.DefaultStyle;
  lStyle.Faint:=True;
  lStyle.Fg:=clRule;
  lWidth:=ANSI.Width-ANSI.PrefixWidth;
  if lWidth<1 then
    lWidth:=1;
  lRule:=StringOfChar('-',lWidth);
  ANSI.AddRaw(lRule,ANSI.StyleText(lRule,lStyle));
  ANSI.NewLine;
  ANSI.BlankLine;
end;

class function TANSIThematicBreakRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownThematicBreakBlock;
end;


{ TANSITableRenderer }

procedure TANSITableRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lTable : TMarkdownTableBlock;
  lCols, lRows, r, c : Integer;
  lColW : array of Integer;
  lCellText : array of array of TMarkdownString;
  lRow, lCell : TMarkdownBlock;
  lText, lLine, lSep, lLabel : TMarkdownString;
  lStyle : TRunStyle;

  function CellText(aCell : TMarkdownBlock) : TMarkdownString;
  begin
    Result:='';
    if (aCell is TMarkdownTextBlock) and Assigned(TMarkdownTextBlock(aCell).Nodes) then
      Result:=TMarkdownTextBlock(aCell).Nodes.PlainText;
  end;

begin
  lTable:=aBlock as TMarkdownTableBlock;
  lRows:=lTable.ChildCount;
  if lRows=0 then
    exit;
  lCols:=Length(lTable.Columns);
  if lCols=0 then
    exit;
  SetLength(lColW,lCols);
  SetLength(lCellText,lRows,lCols);
  for r:=0 to lRows-1 do
    begin
    lRow:=lTable.Children[r];
    for c:=0 to lCols-1 do
      begin
      if c<lRow.ChildCount then
        lCell:=lRow.Children[c]
      else
        lCell:=nil;
      if lCell<>nil then
        lText:=CellText(lCell)
      else
        lText:='';
      lCellText[r][c]:=lText;
      if ANSI.VisibleWidth(lText)>lColW[c] then
        lColW[c]:=ANSI.VisibleWidth(lText);
      end;
    end;
  ANSI.BlankLine;
  lLabel:=Renderer.GetCaptionNumber(lTable);
  if Assigned(lTable.Caption) or (lLabel<>'') then
    begin
    lStyle:=ANSI.DefaultStyle;
    lStyle.Bold:=True;
    if lLabel<>'' then
      ANSI.AddRun(lLabel+':',lStyle,True);
    if Assigned(lTable.Caption) then
      for c:=0 to lTable.Caption.Count-1 do
        ANSI.RenderInline(lTable.Caption[c]);
    ANSI.FlushLine;
    end;
  // separator line "+----+----+"
  lSep:='+';
  for c:=0 to lCols-1 do
    lSep:=lSep+StringOfChar('-',lColW[c]+2)+'+';
  ANSI.AddRaw(lSep,lSep);
  ANSI.NewLine;
  for r:=0 to lRows-1 do
    begin
    lStyle:=ANSI.DefaultStyle;
    if r=0 then
      lStyle.Bold:=True; // header row
    lLine:='|';
    for c:=0 to lCols-1 do
      begin
      lText:=lCellText[r][c];
      lText:=lText+Spaces(lColW[c]-ANSI.VisibleWidth(lText));
      lLine:=lLine+' '+ANSI.StyleText(lText,lStyle)+' |';
      end;
    // styled line: pass empty visible (column position is irrelevant before NewLine)
    ANSI.AddRaw('',lLine);
    ANSI.NewLine;
    if r=0 then
      begin
      ANSI.AddRaw(lSep,lSep);
      ANSI.NewLine;
      end;
    end;
  ANSI.AddRaw(lSep,lSep);
  ANSI.NewLine;
  ANSI.BlankLine;
end;

class function TANSITableRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownTableBlock;
end;


{ TANSIFrontmatterRenderer }

procedure TANSIFrontmatterRenderer.DoRender(aBlock : TMarkdownBlock);
begin
  // Frontmatter produces no visible output
end;


class function TANSIFrontmatterRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownFrontmatterBlock;
end;


{ TANSICommentRenderer }

procedure TANSICommentRenderer.DoRender(aBlock : TMarkdownBlock);

var
  lNode : TMarkdownCommentBlock absolute aBlock;
begin
  if lNode.IsMarker then
    Renderer.DoMarker(lNode.MarkerName,lNode.MarkerArgument,lNode.MarkerValue);
end;


class function TANSICommentRenderer.BlockClass : TMarkdownBlockClass;

begin
  Result:=TMarkdownCommentBlock;
end;


{ TANSIDefinitionListRenderer }

procedure TANSIDefinitionListRenderer.DoRender(aBlock : TMarkdownBlock);

begin
  ANSI.BlankLine;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.BlankLine;
end;


class function TANSIDefinitionListRenderer.BlockClass : TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionListBlock;
end;


{ TANSIDefinitionTermRenderer }

procedure TANSIDefinitionTermRenderer.DoRender(aBlock : TMarkdownBlock);

var
  lSaved, lStyle : TRunStyle;
begin
  lSaved:=ANSI.BaseStyle;
  lStyle:=lSaved;
  lStyle.Bold:=True;
  ANSI.BaseStyle:=lStyle;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.BaseStyle:=lSaved;
  ANSI.FlushLine;
end;


class function TANSIDefinitionTermRenderer.BlockClass : TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionTermBlock;
end;


{ TANSIDefinitionRenderer }

procedure TANSIDefinitionRenderer.DoRender(aBlock : TMarkdownBlock);

var
  lWasInItem : Boolean;
begin
  ANSI.PushPrefix(4,Spaces(4));
  lWasInItem:=ANSI.InListItem;
  ANSI.InListItem:=True;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.InListItem:=lWasInItem;
  ANSI.FlushLine;
  ANSI.PopPrefix;
end;


class function TANSIDefinitionRenderer.BlockClass : TMarkdownBlockClass;

begin
  Result:=TMarkdownDefinitionBlock;
end;


{ TANSIAlertRenderer }

procedure TANSIAlertRenderer.DoRender(aBlock : TMarkdownBlock);

var
  lNode : TMarkdownAlertBlock absolute aBlock;
  lStyle : TRunStyle;
begin
  ANSI.BlankLine;
  lStyle:=ANSI.DefaultStyle;
  lStyle.Bold:=True;
  lStyle.Fg:=clQuote;
  if Assigned(lNode.Title) then
    ANSI.AddRun(lNode.Title.PlainText,lStyle,True)
  else
    ANSI.AddRun(Renderer.AlertTitles[lNode.AlertType],lStyle,True);
  ANSI.FlushLine;
  ANSI.PushPrefix(2,Spaces(2));
  Renderer.RenderChildren(lNode);
  ANSI.FlushLine;
  ANSI.PopPrefix;
  ANSI.BlankLine;
end;


class function TANSIAlertRenderer.BlockClass : TMarkdownBlockClass;

begin
  Result:=TMarkdownAlertBlock;
end;


{ TANSIFootnoteRenderer }

procedure TANSIFootnoteRenderer.DoRender(aBlock : TMarkdownBlock);

begin
  if aBlock=nil then ; // Silence warning
end;


class function TANSIFootnoteRenderer.BlockClass : TMarkdownBlockClass;

begin
  Result:=TMarkdownFootnoteBlock;
end;


{ TANSIFigureRenderer }

procedure TANSIFigureRenderer.DoRender(aBlock : TMarkdownBlock);

var
  lNode : TMarkdownFigureBlock absolute aBlock;
  lText, lLabel : TMarkdownString;
  lStyle : TRunStyle;
begin
  lText:='';
  if Assigned(lNode.Caption) then
    lText:=lNode.Caption.PlainText;
  lLabel:=Renderer.GetCaptionNumber(lNode);
  if lLabel='' then
    lLabel:='Figure';
  lStyle:=ANSI.DefaultStyle;
  lStyle.Faint:=True;
  ANSI.AddRun('['+lLabel+': '+lText+']',lStyle,True);
  ANSI.FlushLine;
  ANSI.BlankLine;
end;


class function TANSIFigureRenderer.BlockClass : TMarkdownBlockClass;

begin
  Result:=TMarkdownFigureBlock;
end;


initialization
  TANSIParagraphRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSITextBlockRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIHeadingRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIQuoteRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIListRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIListItemRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSICodeRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIThematicBreakRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSITableRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIFrontmatterRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSICommentRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIDefinitionListRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIDefinitionTermRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIDefinitionRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIAlertRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIFootnoteRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIFigureRenderer.RegisterRenderer(TMarkDownANSIRenderer);
end.
