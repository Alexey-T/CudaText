{
 /***************************************************************************
                                markdown.control
                                ----------------
                             Markdown renderer to render on canvas

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
{
  Text Layout routine was inspired on the algorithm found in https://github.com/BeRo1985/pashtmldown
}
unit MarkDown.CanvasRender;
{$mode objfpc}
{$h+}
{$codepage UTF8}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, Contnrs, Graphics, Types, Math, MarkDown.Elements, MarkDown.Parser, MarkDown.Render, Markdown.HTMLEntities, LazUTF8;

type
  TLayoutItemKind = (
    likText,
    likHR,
    likRect,
    likLine,
    likImage,
    likBackground
  );
  TFontContextItem = (fcMono,fcCode,fcFencedCode,fcQuote,fcHyperLink);
  TFontContext = set of TFontContextItem;

  { Selection support types }
  TSelectionPoint = record
    LayoutItemIndex: Integer;
    CharOffset: Integer;
  end;

  { TLayoutItem }
  TColorKind = (ckForeground,ckBackground);
  TLayoutItem = class
  private
    FBGColor: TColor;
    FColor: TColor;
    FContext: TFontContext;
    FDeltaX: LongInt;
    FDeltaY: LongInt;
    FKind: TLayoutItemKind;
    FX: LongInt;
    FY: LongInt;
    FWidth: LongInt;
    FHeight: LongInt;
    FText: utf8string;
    FFontSize: LongInt;
    FFontStyle: TFontStyles;
    FURL: utf8string;
    FIsSelected: Boolean;
    FSelectionStart: LongInt;
    FSelectionEnd: LongInt;
    FSelectionStartChar: Integer;
    FSelectionEndChar: Integer;
  protected
    function GetColor(aKind : TColorKind; aDefault : TColor) : TColor;
  public
    constructor Create(aKind : TLayoutItemKind; aX,aY : Longint);
    property Kind: TLayoutItemKind read FKind;
    property X: LongInt read FX write FX;
    property Y: LongInt read FY write FY;
    property BGColor : TColor Read FBGColor Write FBGColor;
    property Color : TColor Read FColor Write FColor;
    property DeltaX: LongInt read FDeltaX write FDeltaX;
    property DeltaY: LongInt read FDeltaY write FDeltaY;
    property Width: LongInt read FWidth write FWidth;
    property Height: LongInt read FHeight write FHeight;
    property Text: utF8string read FText write FText;
    property FontSize: LongInt read FFontSize write FFontSize;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property URL : utF8string read FURL write FURL;
    property Context: TFontContext Read FContext Write FContext;
    property IsSelected: Boolean read FIsSelected write FIsSelected;
    property SelectionStart: LongInt read FSelectionStart write FSelectionStart;
    property SelectionEnd: LongInt read FSelectionEnd write FSelectionEnd;
    property SelectionStartChar: Integer read FSelectionStartChar write FSelectionStartChar;
    property SelectionEndChar: Integer read FSelectionEndChar write FSelectionEndChar;
  end;

  { TLayoutItemList }

  TLayoutItemList = class(TFPObjectList)
  private
    function GetItem(const aIndex: LongInt): TLayoutItem;
  public
    function NewItem: TLayoutItem;
    function NewItem(aKind: TLayoutItemKind; aX, aY: Integer): TLayoutItem;
    property Items[const Index: LongInt]: TLayoutItem read GetItem; default;
  end;

  { TLinkHitRect }

  TLinkHitRect = class
  private
    FX: LongInt;
    FY: LongInt;
    FWidth: LongInt;
    FHeight: LongInt;
    FHREF: utf8string;
  public
    constructor create (aX,aY,aWidth,aHeight : Longint; aHREF : UTF8String);
    function HasPoint(aX,aY : Longint) : Boolean;
    property X: LongInt read FX;
    property Y: LongInt read FY;
    property Width: LongInt read FWidth;
    property Height: LongInt read FHeight;
    property HREF: utf8string read FHREF;
  end;

  TLinkHitRectList = class (TFPObjectList)
  private
    function GetItem(const aIndex: LongInt): TLinkHitRect;
  public
    function Add(const aX, aY, aWidth, aHeight: LongInt; const aHref: utf8string): TLinkHitRect; reintroduce;
    property Items[const Index: LongInt]: TLinkHitRect read GetItem; default;
  end;

  TLayoutData = record
    MaxWidth : LongInt;
    CurrentIndent : LongInt;
    LineX : LongInt;
    LineY : LongInt;
    LineHeight : LongInt;
    LineAboveExtra : LongInt;
    LineBelowExtra : LongInt;
    BaselineShiftCurrent : LongInt;
    BaselineFromIndex : Longint;
    NeedSpaceBeforeNextText : Boolean;
  end;

  TLayoutLists = record
    Items: TLayoutItemList;
    LinkRects: TLinkHitRectList;
    class function Create : TLayoutLists; Static;
    procedure Clear;
    procedure Free;
  end;

  TMarkdownImageEvent = procedure(Sender : TObject; const aURL : string; var aImage : TPicture) of object;

  { TMarkDownCanvasRenderer }

  TMarkDownCanvasRenderer = class(TMarkDownRenderer)
  private
    FCalculatedBlockQuoteIndent: Integer;
    const BulletCount = 3;
  private
    FBlockQuoteIndent: Integer;
    FBulletLevel: Integer;
    FBullets: Array[1..BulletCount] of string;
    FCanvas: TCanvas;
    FDocument: TMarkDownDocument;
    FHyperLinkColor: TColor;
    FImageMargin: integer;
    FOnGetImage: TMarkdownImageEvent;
    FParser: TMarkDownParser;
    FLists : TLayoutLists;
    FCalculatedWidth: LongInt;
    FCalculatedHeight: LongInt;

    FLineWidth: LongInt;
    FLayout : TLayoutData;
    FParagraphSpacing: LongInt;
    FExtraIndent: LongInt;
    FFontName: utf8string;
    FMonoFontName: utf8string;
    FBaseFontSize: LongInt;
    FTargetDPI: LongInt;

    // Colors
    FBGCodeColor: TColor;
    FFontCodeColor: TColor;
    FBGMarkColor: TColor;
    FFontMarkColor: TColor;
    FBGColor: TColor;
    FFontColor: TColor;
    FFontQuoteColor: TColor;
    FNextLineIndent : Integer;
    FImageCache : TFPObjectHashTable;

    // Selection support
    FSelectionColor: TColor;
    FHTMLEntities : TFPStringHashTable;
  Protected
    // Selection methods
    procedure ClearAllSelections;
    procedure ApplySelection(const aStartPoint, aEndPoint: TSelectionPoint);
    function CalculateCharXPosition(aItem: TLayoutItem; aCharIndex: Integer): LongInt;

    // Layout management
    procedure Clear;
    procedure BeginLayout;
    procedure EndLayout;
    function GetBulletChar(AIndex: Integer): string;
    procedure NewLine(const aCanvas: TCanvas);
    procedure ParagraphBreak(const aCanvas: TCanvas);
    procedure MaybeStartParagraph;
    function SwapLayout(const aData : TLayoutData) : TLayoutData;
    function SwapLayoutLists(const aData : TLayoutLists) : TLayoutLists;
    procedure SetCurrentY(aValue: Integer);

    // Font and measurement
    function DIP(const aValue: LongInt): LongInt;
    procedure ApplyFont(const aCanvas: TCanvas; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext : TFontContext);
    procedure MeasureText(const aCanvas: TCanvas; const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aContext : TFontContext; out aWidth, aHeight: LongInt);
    function MeasureTextWidth(const aCanvas: TCanvas; const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aContext : TFontContext): LongInt;
    function MeasureTextHeight(const aCanvas: TCanvas; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext : TFontContext): LongInt;
    function GetTextColor(aContext: TFontContext): TColor; virtual;
    function GetTextBGColor(aContext: TFontContext): TColor; virtual;

    // Text layout
    procedure CollectHTMLEntities;
    function ResolveEntities(const aText: String): String; virtual;
    procedure FlushTextRun(const aCanvas: TCanvas; const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext : TFontContext;
      const aDryRun: boolean); virtual;
    procedure LayoutTextWrapped(const aCanvas: TCanvas; const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext : TFontContext;
      const aPreserveWhitespace, aDryRun: boolean); virtual;
    function LayoutImage(aImageURL: UTF8String): Boolean; virtual;
    procedure RenderTextNode(aTextNode: TMarkDownTextNode; aFontSize: LongInt; aFontStyle: TFontStyles; const aContext : TFontContext);

    // Actual drawing
    procedure DrawLayoutItem(aCanvas: TCanvas; aItem: TLayoutItem; const aLeftPosition, aTopPosition: LongInt);
    procedure DrawItemSelection(aCanvas: TCanvas; aItem: TLayoutItem; const aLeftPosition, aTopPosition: LongInt);

    // Utility
    function GetNodeFontStyle(aTextNode: TMarkDownTextNode): TFontStyles;
    // indent
    Procedure Indent(aSize : integer);
    procedure SetBulletChar(AIndex: Integer; AValue: string);
    Procedure Undent(aSize : integer);
    procedure IncBulletLevel;
    Procedure DecBulletLevel;
    function GetBullet : String;
    function GetBulletAt(aLevel : integer) : string;
    function CreateLayoutItem(aKind : TLayoutItemKind; aX,aY : Longint) : TLayoutItem; inline;
    Property BulletLevel : Integer Read FBulletLevel;
    Property CalculatedBlockQuoteIndent : Integer Read FCalculatedBlockQuoteIndent;
    Property Layout : TLayoutData read FLayout;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Markdown parsing
    procedure ParseMarkdown(const aMarkDown: utf8string); overload;
    procedure ParseMarkdown(const aMarkDown: TStrings); overload;

    // Rendering interface
    procedure RenderDocument(aDocument: TMarkDownDocument); override;
    procedure CalculateLayout(const aCanvas: TCanvas; const aMaxWidth: LongInt; out aContentWidth: LongInt;
      out aContentHeight: LongInt);
    procedure DrawLayout(const aCanvas: TCanvas; const aLeftPosition, aTopPosition: LongInt);
    function HitTestLink(const aX, aY: LongInt; out aHref: utf8string): boolean;

    // Selection methods
    procedure SetSelection(const aStart, aEnd: TSelectionPoint);
    procedure ClearSelection;
    function GetSelectedText: String;
    function HitTestText(const aX, aY: LongInt): TSelectionPoint;

    // Properties
    property CalculatedWidth: LongInt read FCalculatedWidth;
    property CalculatedHeight: LongInt read FCalculatedHeight;
    Property Document : TMarkdownDocument Read FDocument;
    property TargetDPI: LongInt read FTargetDPI write FTargetDPI;
    property FontName: utf8string read FFontName write FFontName;
    property MonoFontName: utf8string read FMonoFontName write FMonoFontName;

    property FontHyperLinkColor: TColor read FHyperLinkColor write FHyperLinkColor;
    property BGCodeColor: TColor read FBGCodeColor write FBGCodeColor;
    property FontCodeColor: TColor read FFontCodeColor write FFontCodeColor;
    property BGMarkColor: TColor read FBGMarkColor write FBGMarkColor;
    property FontMarkColor: TColor read FFontMarkColor write FFontMarkColor;

    property BGColor: TColor read FBGColor write FBGColor;
    property FontColor: TColor read FFontColor write FFontColor;
    property FontQuoteColor: TColor read FFontQuoteColor write FFontQuoteColor;
    property BaseFontSize: LongInt read FBaseFontSize write FBaseFontSize;
    property BulletChar1 : string Index 1 Read GetBulletChar Write SetBulletChar;
    property BulletChar2 : string Index 2 read GetBulletChar Write SetBulletChar;
    property BulletChar3 : string Index 3 read GetBulletChar Write SetBulletChar;
    Property BlockQuoteIndent : Integer Read FBlockQuoteIndent Write FBlockQuoteIndent;
    Property ParagraphSpacing : Integer Read FParagraphSpacing Write FParagraphSpacing;
    Property ExtraIndent : Integer Read FExtraIndent Write FExtraIndent;
    Property ImageMargin : integer read FImageMargin Write FImageMargin;
    Property OnGetImage : TMarkdownImageEvent Read FOnGetImage Write FOnGetImage;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor;
  end;

  { TCanvasMarkDownBlockRenderer }

  TCanvasMarkDownBlockRenderer = class(TMarkDownBlockRenderer)
  private
    function GetCanvas: TCanvas;
    function GetCanvasRenderer: TMarkDownCanvasRenderer;
  protected
    // Helper methods for canvas rendering
    function DIP(aSize : integer) : Integer;
    procedure Indent(aSize : Integer);
    procedure Undent(aSize : Integer);
    function LayoutData : TLayoutData;
    function CreateLayoutItem(aKind : TLayoutItemKind; aX,aY : Longint) : TLayoutItem; inline;
    function MeasureTextHeight(const aFontSize: LongInt; const aFontStyle: TFontStyles; const aContext : TFontContext): LongInt;
    function MeasureTextWidth(const aText: utf8string; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      aFontContext: TFontContext): LongInt;
    procedure LayoutText(const aText: utf8string; const aFontSize: LongInt; const aFontStyle: TFontStyles; const aLinkHref: utf8string;
      const aContext: TFontContext); inline;
    procedure SetCurrentY(aValue: Integer);
    function GetCurrentY : Integer;
    procedure NewLine; inline;
    procedure ParagraphBreak; inline;
    procedure MaybeStartParagraph; inline;
    property Canvas : TCanvas read GetCanvas;
  public
    property CanvasRenderer: TMarkDownCanvasRenderer read GetCanvasRenderer;
  end;

  TCanvasMarkDownBlockRendererClass = class of TCanvasMarkDownBlockRenderer;

  { TCanvasMarkDownTextRenderer }

  TCanvasMarkDownTextRenderer = class(TMarkDownTextRenderer)
  private
    function GetCanvasRenderer: TMarkDownCanvasRenderer;
  protected
    procedure DoRender(aElement: TMarkDownTextNode); override;
  public
    property CanvasRenderer: TMarkDownCanvasRenderer read GetCanvasRenderer;
  end;

  { Individual Canvas Block Renderers }

  { TCanvasMarkDownParagraphBlockRenderer }
  TCanvasMarkDownParagraphBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownHeadingBlockRenderer }
  TCanvasMarkDownHeadingBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    fontSize: LongInt;
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownQuoteBlockRenderer }
  TCanvasMarkDownQuoteBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownListBlockRenderer }
  TCanvasMarkDownListBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  Private
    FItemNumber : Integer;
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
    property ItemNumber : Integer Read FItemNumber;
  public
    procedure reset; override;
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownListItemBlockRenderer }
  TCanvasMarkDownListItemBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownCodeBlockRenderer }
  TCanvasMarkDownCodeBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownTableBlockRenderer }
  TCanvasMarkDownTableBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  Protected
    Type
      TColData = Record
        Width, Min, Pref : Longint;
      end;
      TColDataArray = array of TColData;
      TLongintArray = array of LongInt;
  private
    FColLayout : TColDataArray;
    FAvailableWidth : Integer;
    FGridSize : Integer;
    FCellPadding : Integer;
    FStartX,FStartY : Integer;
    FRowHeights: TLongintArray;
    FCurrentRow : integer;
    FFontStyles : TFontStyles;
    procedure CalcRowHeights(aTable: TMarkDownTableBlock; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext: TFontContext);
    procedure DistributeColumns(const aAvailable: LongInt);
    procedure DrawBorders;
    procedure GetCellTexts(aCell: TMarkDownBlock; aTexts: TStrings);
    function GetTotalColumns: Integer;
    function GetTotalHeight: Integer;
    procedure MeasureTableColumns(const aTableModel: TMarkDownTableBlock);
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
    // Column layout, calculated at start of DoRender
    property ColLayout : TColDataArray Read FColLayout;
    // Number of columns
    property TotalColumns : Integer Read GetTotalColumns;
    // Row heights, calculated at start of DoRender
    property RowHeights : TLongintArray Read FRowHeights;
    // Current row while rendering rows
    property CurrentRow : Integer Read FCurrentRow;
    // Grid size
    Property GridSize : Integer Read FGridSize;
    // Cell padding
    Property CellPadding : Integer Read FCellPadding;
    // Available width, calculated in DoRender
    Property AvailableWidth : Integer Read FAvailableWidth;
    // Font styles
    Property FontStyles : TFontStyles Read FFontStyles;
    // Total height as calculated
    Property TotalHeight : Integer Read GetTotalHeight;
  public
    Procedure Reset; override;
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownTableRowBlockRenderer }
  TCanvasMarkDownTableRowBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  private
    FTableRenderer : TCanvasMarkDownTableBlockRenderer;
    procedure GetTableRenderer;
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
    Property TableRenderer : TCanvasMarkDownTableBlockRenderer read FTableRenderer;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownThematicBreakBlockRenderer }
  TCanvasMarkDownThematicBreakBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TCanvasMarkDownTextBlockRenderer }
  TCanvasMarkDownTextBlockRenderer = class(TCanvasMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

implementation

const
  ExtendHeight = UTF8String('Áy');

{ TLayoutItem }

function TLayoutItem.GetColor(aKind: TColorKind; aDefault: TColor): TColor;
begin
  case aKind of
    ckBackground : Result:=FBGColor;
    ckForeground : Result:=FColor;
  else
    Result:=clNone;
  end;
  if Result=clNone then
    Result:=aDefault;
end;

constructor TLayoutItem.Create(aKind: TLayoutItemKind; aX, aY: Longint);
begin
  FKind:=aKind;
  FColor:=clNone;
  FBGColor:=clNone;

  FX:=aX;
  FY:=aY;
  FIsSelected:=False;
  FSelectionStart:=0;
  FSelectionEnd:=0;
  FSelectionStartChar:=0;
  FSelectionEndChar:=0;
end;

function TLayoutItemList.NewItem: TLayoutItem;
begin
  Result:=NewItem(likText,0,0)
end;

function TLayoutItemList.NewItem(aKind: TLayoutItemKind; aX, aY: Integer): TLayoutItem;
begin
  Result:=TLayoutItem.Create(aKind,aX,aY);
  Add(Result);
end;

function TLayoutItemList.GetItem(const aIndex: LongInt): TLayoutItem;
begin
  Result:=TLayoutItem(Inherited GetItem(aIndex));
end;

{ TLinkHitRect }

constructor TLinkHitRect.create(aX, aY, aWidth, aHeight: Longint; aHREF: UTF8String);
begin
  FX:=aX;
  FY:=aY;
  FWidth:=aWidth;
  FHeight:=aHeight;
  FHREF:=aHREF;
end;

function TLinkHitRect.HasPoint(aX, aY: Longint): Boolean;
begin
  Result:= ((aX>=X) and (aX<(X+Width))) and
           ((aY>=Y) and (aY<(Y+Height)));
end;

{=== TLinkHitRectList ======================================================}


function TLinkHitRectList.Add(const aX, aY, aWidth, aHeight: LongInt; const aHref: utf8string) : TLinkHitRect;

begin
  Result:=TLinkHitRect.Create(ax,ay,aWidth,aHeight,aHREF);
  Inherited add(Result);
end;

function TLinkHitRectList.GetItem(const aIndex: LongInt): TLinkHitRect;
begin
  Result:=TLinkHitRect(Inherited GetItem(aIndex));
end;


{ TMarkDownCanvasRenderer }

constructor TMarkDownCanvasRenderer.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FHTMLEntities:=TFPStringHashTable.Create;
  FImageCache:=TFPObjectHashTable.Create(True);
  FBullets[1]:='•';
  FBullets[2]:='◦';
  FBullets[3]:='▪';
  FBulletLevel:=0;
  FImageMargin:=2;
  FLists.Items:=TLayoutItemList.Create;
  FLists.LinkRects:=TLinkHitRectList.Create;

  fParser:=TMarkDownParser.Create(Self);

  // Initialize default values
  fParagraphSpacing:=10;
  fExtraIndent:=0;
  fTargetDPI:=96;
  fFontName:='Sans Serif';
  fMonoFontName:='Monospace';

  BGCodeColor:=RGBToColor(253,245,227);
  fFontCodeColor:=clInfoText;
  fBGMarkColor:=clYellow;
  fFontMarkColor:=clBlack;
  fHyperLinkColor:=RGBToColor(17,85,204);
  fBGColor:=clWindow;
  fFontColor:=clWindowText;
  fFontQuoteColor:=clWindowFrame;

  fBaseFontSize:=10;
  FLayout.BaselineShiftCurrent:=0;

  // Initialize selection
  FSelectionColor:=clHighlight;
end;

destructor TMarkDownCanvasRenderer.Destroy;
begin
  FreeAndNil(FHTMLEntities);
  FreeAndNil(FImageCache);
  FreeAndNil(FLists.Items);
  FreeAndNil(FLists.LinkRects);
  FreeAndNil(fDocument);
  FreeAndNil(fParser);
  inherited Destroy;
end;


procedure TMarkDownCanvasRenderer.ClearAllSelections;
var
  I: Integer;
begin
  for I:=0 to FLists.Items.Count - 1 do
  With FLists.Items[I] do
    begin
    IsSelected:=False;
    SelectionStart:=0;
    SelectionEnd:=0;
    SelectionStartChar:=0;
    SelectionEndChar:=0;
    end;
end;

function TMarkDownCanvasRenderer.CalculateCharXPosition(aItem: TLayoutItem; aCharIndex: Integer): LongInt;
var
  SubText: UTF8String;
begin
  Result:=0;
  if (aItem.Kind<>likText) or (aCharIndex <= 0) or (Length(aItem.Text) = 0) then
    Exit;

  if aCharIndex >= UTF8Length(aItem.Text) then
  begin
    Result:=aItem.Width;
    Exit;
  end;

  // Apply the same font that was used during layout
  ApplyFont(fCanvas, aItem.FontSize, aItem.FontStyle, aItem.Context);
  SubText:=UTF8Copy(aItem.Text, 1, aCharIndex);
  Result:=fCanvas.TextWidth(SubText);
end;

procedure TMarkDownCanvasRenderer.ApplySelection(const aStartPoint, aEndPoint: TSelectionPoint);
var
  I: Integer;
  Item: TLayoutItem;
  StartIdx, EndIdx: Integer;
begin
  ClearAllSelections;
  StartIdx:=aStartPoint.LayoutItemIndex;
  EndIdx:=aEndPoint.LayoutItemIndex;
  if (StartIdx = -1) or (EndIdx = -1) then
    Exit;

  // Ensure proper order
  if StartIdx>EndIdx then
    begin
    I:=StartIdx;
    StartIdx:=EndIdx;
    EndIdx:=I;
    end;

  for I:=StartIdx to EndIdx do
    begin
    if I >= FLists.Items.Count then
      Break;
    Item:=FLists.Items[I];
    if Item.Kind<>likText then
      Continue;
    Item.IsSelected:=True;
    if (I = StartIdx) and (I = EndIdx) then
      begin
      // Single item selection
      Item.SelectionStartChar:=aStartPoint.CharOffset;
      Item.SelectionEndChar:=aEndPoint.CharOffset;
      Item.SelectionStart:=CalculateCharXPosition(Item, aStartPoint.CharOffset);
      Item.SelectionEnd:=CalculateCharXPosition(Item, aEndPoint.CharOffset);
      end
    else if I = StartIdx then
      begin
      // Start item
      Item.SelectionStartChar:=aStartPoint.CharOffset;
      Item.SelectionEndChar:=UTF8Length(Item.Text);
      Item.SelectionStart:=CalculateCharXPosition(Item, aStartPoint.CharOffset);
      Item.SelectionEnd:=Item.Width;
      end
    else if I = EndIdx then
      begin
      // End item
      Item.SelectionStartChar:=0;
      Item.SelectionEndChar:=aEndPoint.CharOffset;
      Item.SelectionStart:=0;
      Item.SelectionEnd:=CalculateCharXPosition(Item, aEndPoint.CharOffset);
      end
    else
      begin
      // Completely selected item (optimization)
      Item.SelectionStartChar:=0;
      Item.SelectionEndChar:=UTF8Length(Item.Text);
      Item.SelectionStart:=0;
      Item.SelectionEnd:=Item.Width;
      end;
    end;
end;

procedure TMarkDownCanvasRenderer.DrawItemSelection(aCanvas: TCanvas; aItem: TLayoutItem; const aLeftPosition, aTopPosition: LongInt);
var
  SelectionRect: TRect;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenStyle: TPenStyle;
begin
  if not aItem.IsSelected then
    Exit;

  OldBrushColor:=aCanvas.Brush.Color;
  OldBrushStyle:=aCanvas.Brush.Style;
  OldPenStyle:=aCanvas.Pen.Style;

  aCanvas.Brush.Color:=FSelectionColor;
  aCanvas.Brush.Style:=bsSolid;
  aCanvas.Pen.Style:=psClear;

  SelectionRect.Left:=aItem.X + aLeftPosition + aItem.SelectionStart;
  SelectionRect.Top:=aItem.Y + aTopPosition;
  SelectionRect.Right:=aItem.X + aLeftPosition + aItem.SelectionEnd;
  SelectionRect.Bottom:=aItem.Y + aTopPosition + aItem.Height;

  aCanvas.FillRect(SelectionRect);

  aCanvas.Brush.Color:=OldBrushColor;
  aCanvas.Brush.Style:=OldBrushStyle;
  aCanvas.Pen.Style:=OldPenStyle;
end;

function TMarkDownCanvasRenderer.HitTestText(const aX, aY: LongInt): TSelectionPoint;
var
  I: Integer;
  Item: TLayoutItem;
  RelativeX: LongInt;
  CharIndex: Integer;
  CharWidth: LongInt;
  CurrentX: LongInt;
  TestChar: UTF8String;
begin
  Result.LayoutItemIndex:=-1;
  Result.CharOffset:=0;

  for I:=0 to FLists.Items.Count - 1 do
    begin
    Item:=FLists.Items[I];
    if (Item.Kind=likText) and
       (aX>=Item.X) and (aX<=Item.X+Item.Width) and
       (aY>=Item.Y) and (aY<=Item.Y+Item.Height) then
      begin
      Result.LayoutItemIndex:=I;
      RelativeX:=aX - Item.X;

      // Find character position
      ApplyFont(fCanvas, Item.FontSize, Item.FontStyle, Item.Context);
      CurrentX:=0;
      CharIndex:=0;

      while CharIndex<UTF8Length(Item.Text) do
        begin
        TestChar:=UTF8Copy(Item.Text, CharIndex + 1, 1);
        CharWidth:=fCanvas.TextWidth(TestChar);
        if RelativeX <= CurrentX + (CharWidth div 2) then
          Break;
        CurrentX:=CurrentX + CharWidth;
        Inc(CharIndex);
        end;

      Result.CharOffset:=CharIndex;
      Exit;
      end;
    end;
end;

procedure TMarkDownCanvasRenderer.SetSelection(const aStart, aEnd: TSelectionPoint);
begin
  ApplySelection(aStart, aEnd);
end;

procedure TMarkDownCanvasRenderer.ClearSelection;
begin
  ClearAllSelections;
end;

function TMarkDownCanvasRenderer.GetSelectedText: String;
var
  I: Integer;
  Item,PrevItem: TLayoutItem;
  ItemText: String;
begin
  Result:='';
  PrevItem:=Nil;
  for I:=0 to FLists.Items.Count - 1 do
    begin
    Item:=FLists.Items[I];
    if Item.IsSelected and (Item.Kind=likText) then
      begin
      ItemText:=UTF8Copy(Item.Text, Item.SelectionStartChar + 1,
                          Item.SelectionEndChar - Item.SelectionStartChar);
      if Result<>'' then
        begin
        // Add spacing between items
        if (I>0) and (PrevItem.Y<>Item.Y) then
          Result:=Result + LineEnding
        else
          Result:=Result + ' ';
        end;
      Result:=Result + ItemText;
      end;
    PrevItem:=Item;
    end;
end;

procedure TMarkDownCanvasRenderer.ParseMarkdown(const aMarkDown: utf8string);
var
  S: TStrings;
begin
  FreeAndNil(fDocument);
  S:=TStringList.Create;
  try
    S.Text:=aMarkDown;
  finally
    S.Free;
  end;
end;

procedure TMarkDownCanvasRenderer.ParseMarkdown(const aMarkDown: TStrings);
begin
  fDocument:=fParser.Parse(aMarkDown);
end;

procedure TMarkDownCanvasRenderer.RenderDocument(aDocument: TMarkDownDocument);
begin
  if not assigned(aDocument) then exit;
  fDocument:=aDocument;
  // Render all child blocks
  RenderChildren(aDocument);
end;

procedure TMarkDownCanvasRenderer.Clear;
begin
  FLists.Items.Clear;
  FLists.LinkRects.Clear;
  fCalculatedWidth:=0;
  fCalculatedHeight:=0;
  FLayout:=Default(TLayoutData);
  fLineWidth:=0;
  FLayout.NeedSpaceBeforeNextText:=False;
end;

procedure TMarkDownCanvasRenderer.BeginLayout;
begin
  Clear;
end;

procedure TMarkDownCanvasRenderer.EndLayout;
begin
  // Reserved for future batching hooks
end;

function TMarkDownCanvasRenderer.GetBulletChar(AIndex: Integer): string;
begin
  result:=FBullets[aIndex];
end;

function TMarkDownCanvasRenderer.DIP(const aValue: LongInt): LongInt;
begin
  Result:=(aValue * fTargetDPI + 48) div 96;
end;

procedure TMarkDownCanvasRenderer.NewLine(const aCanvas: TCanvas);
begin
  if (aCanvas=Nil) then; // Silence compiler warning
  FLayout.LineX:=0;
  if FNextLineIndent<>0 then
    begin
    FLayout.CurrentIndent:=FLayout.CurrentIndent+FNextLineIndent;
    FNextLineIndent:=0;
    end;
  FLayout.LineY:=FLayout.LineY + (FLayout.LineAboveExtra + FLayout.LineHeight + FLayout.LineBelowExtra);
  FLayout.LineHeight:=0;
  FLayout.LineAboveExtra:=0;
  FLayout.LineBelowExtra:=0;
  FLayout.NeedSpaceBeforeNextText:=False;
  FLayout.BaselineFromIndex:=FLists.Items.Count-1;
  FLayout.BaselineShiftCurrent:=0;
end;

procedure TMarkDownCanvasRenderer.ParagraphBreak(const aCanvas: TCanvas);
begin
  if FLayout.LineX<>0 then
    NewLine(aCanvas);
  FLayout.LineY:=FLayout.LineY + DIP(fParagraphSpacing);
  FLayout.LineAboveExtra:=0;
  FLayout.LineBelowExtra:=0;
end;

procedure TMarkDownCanvasRenderer.MaybeStartParagraph;
begin
  if FLayout.LineX<>0 then
   ParagraphBreak(FCanvas);
end;

function TMarkDownCanvasRenderer.SwapLayout(const aData: TLayoutData): TLayoutData;
begin
  Result:=FLayout;
  FLayout:=aData;
end;

function TMarkDownCanvasRenderer.SwapLayoutLists(const aData: TLayoutLists): TLayoutLists;
begin
  Result:=FLists;
  FLists:=aData;
end;

procedure TMarkDownCanvasRenderer.SetCurrentY(aValue: Integer);
begin
  FLayout.LineY:=aValue;
end;

procedure TMarkDownCanvasRenderer.ApplyFont(const aCanvas: TCanvas; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext : TFontContext);

  Procedure SetFontName(aName : string);
  begin
    if aName<>'' then
     aCanvas.Font.Name:=aName;
  end;

begin
  // We do not apply color, it has no influence on measurement.
  // Color is set explicitly when drawing.
  if fcMono in aContext then
    SetFontName(fMonoFontName)
  else
    SetFontName(fFontName);
  if aCanvas.Font.Size<>aFontSize then
    aCanvas.Font.Size:=aFontSize;
  if aCanvas.Font.Style<>aFontStyle then
    aCanvas.Font.Style:=aFontStyle;
end;


procedure TMarkDownCanvasRenderer.MeasureText(const aCanvas: TCanvas; const aText: utf8string;
  const aFontSize: LongInt; const aFontStyle: TFontStyles; const aContext : TFontContext;
  out aWidth, aHeight: LongInt);
begin
  if length(aText)>0 then
  begin
    ApplyFont(aCanvas, aFontSize, aFontStyle, aContext);
    aWidth:=aCanvas.TextWidth(aText);
    aHeight:=aCanvas.TextHeight(aText + ExtendHeight);
  end
  else
  begin
    aWidth:=0;
    aHeight:=0;
  end;
end;

function TMarkDownCanvasRenderer.MeasureTextWidth(const aCanvas: TCanvas; const aText: utf8string;
  const aFontSize: LongInt; const aFontStyle: TFontStyles; const aContext : TFontContext): LongInt;
var
  Width, Height: LongInt;
begin
  MeasureText(aCanvas, aText, aFontSize, aFontStyle, aContext, Width, Height);
  Result:=Width;
end;

function TMarkDownCanvasRenderer.MeasureTextHeight(const aCanvas: TCanvas; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext: TFontContext): LongInt;
var
  Width, Height: LongInt;
begin
  MeasureText(aCanvas, ExtendHeight, aFontSize, aFontStyle, aContext, Width, Height);
  Result:=Height;
end;

function TMarkDownCanvasRenderer.GetTextColor(aContext : TFontContext) : TColor;

begin
  if fcHyperLink in aContext then
    result:=FontHyperLinkColor
  else if fcQuote in aContext then
    Result:=fFontQuoteColor
  else if ([fcCode,fcFencedCode]*aContext)<>[] then
    Result:=FontCodeColor
  else
    Result:=FontColor;
end;

function TMarkDownCanvasRenderer.GetTextBGColor(aContext : TFontContext) : TColor;

begin
  if fcFencedCode in aContext then
    Result:=BGCodeColor
  else
    Result:=BGColor;
end;

procedure TMarkDownCanvasRenderer.FlushTextRun(const aCanvas: TCanvas; const aText: utf8string;
  const aFontSize: LongInt; const aFontStyle: TFontStyles; const aLinkHref: utf8string;
  const aContext : TFontContext; const aDryRun: boolean);
var
  Item: TLayoutItem;
  lX,lY : Longint;
  lNewWidth, lHeight, lWidth, BaselineShift: LongInt;
begin
  if length(aText)=0 then
    exit;

  lWidth:=MeasureTextWidth(aCanvas, aText, aFontSize, aFontStyle, aContext);
  lHeight:=MeasureTextHeight(aCanvas, aFontSize, aFontStyle, aContext);
  BaselineShift:=FLayout.BaselineShiftCurrent;
  if FLayout.LineHeight<(lHeight) then
    FLayout.LineHeight:=lHeight;

  if not aDryRun then
    begin
    lX:=FLayout.CurrentIndent + FLayout.LineX;
    lY:=FLayout.LineY + FLayout.LineAboveExtra + BaselineShift;
    Item:=FLists.Items.NewItem(TLayoutItemKind.likText,lX,lY);

    item.BGColor:=GetTextBGColor(aContext);
    item.color:=GetTextColor(aContext);
    Item.Width:=lWidth;
    Item.Height:=lHeight;
    Item.Text:=aText;
    Item.FontSize:=aFontSize;
    Item.FontStyle:=aFontStyle;
    Item.URL:=aLinkHref;
    Item.Context:=aContext;
    if length(aLinkHref)<>0 then
      FLists.LinkRects.Add(lX, lY, lWidth, lHeight, aLinkHref);
    end;

  FLayout.LineX:=FLayout.LineX + lWidth;
  lNewWidth:=FLayout.CurrentIndent + FLayout.LineX;
  if fLineWidth<lNewWidth then
    fLineWidth:=lNewWidth;
  if fCalculatedWidth<lNewWidth then
    fCalculatedWidth:=lNewWidth;
end;

procedure TMarkDownCanvasRenderer.LayoutTextWrapped(const aCanvas: TCanvas; const aText: utf8string;
  const aFontSize: LongInt; const aFontStyle: TFontStyles; const aLinkHref: utf8string;
  const aContext : TFontContext; const aPreserveWhitespace, aDryRun: boolean);
Const
  WhiteSpace = [#32, #9, #13, #10];

var
  LineBuffer: AnsiString;
  NeedSpace: boolean;

  procedure FlushCurrentLine;
  begin
    if length(LineBuffer)>0 then
      begin
      FlushTextRun(aCanvas, TrimRight(LineBuffer), aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
      LineBuffer:='';
      end;
  end;

  function NextUTF8CharLenAt(const aString: utf8string; const aPosition: integer): integer;
  var
    ByteValue: byte;
  begin
    if (aPosition<1) or (aPosition>length(aString)) then
      begin
      Result:=0;
      exit;
      end;
    ByteValue:=byte(aString[aPosition]);
    if ByteValue<$80 then
      Result:=1
    else if (ByteValue and $e0) = $c0 then
      Result:=2
    else if (ByteValue and $f0) = $e0 then
      Result:=3
    else
      Result:=4;
  end;

  procedure BreakLongWord(const aWord: utf8string);
  var
    CharPos, WordLen, CharLen: LongInt;
    CharBuffer, CurrentChar: utf8string;
    CharWidth: LongInt;
  begin
    WordLen:=length(aWord);
    CharBuffer:='';
    CharPos:=1;

    while CharPos <= WordLen do
      begin
      CharLen:=NextUTF8CharLenAt(aWord, CharPos);
      if CharLen <= 0 then
        CharLen:=1;

      CurrentChar:=copy(aWord, CharPos, CharLen);
      CharWidth:=MeasureTextWidth(aCanvas, CharBuffer + CurrentChar, aFontSize, aFontStyle, aContext);

      if (FLayout.CurrentIndent + FLayout.LineX + CharWidth)<=FLayout.MaxWidth then
        CharBuffer:=CharBuffer + CurrentChar
      else
        begin
        if length(CharBuffer)>0 then
          begin
          FlushTextRun(aCanvas, CharBuffer, aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
          CharBuffer:='';
          end;
        NewLine(aCanvas);
        CharBuffer:=CurrentChar;
        end;
      Inc(CharPos, CharLen);
      end;
    if length(CharBuffer)>0 then
      FlushTextRun(aCanvas, CharBuffer, aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
    NeedSpace:=True;
  end;

  procedure AddWordToLine(const aWord: utf8string);
  var
    lLine: utf8string;
    lWidth: LongInt;
  begin
    if length(aWord)=0 then
      exit;
    lLine:=LineBuffer;
    if NeedSpace and (length(LineBuffer)>0) then
      lLine:=lLine + ' ';
    lLine:=lLine + aWord;
    lWidth:=MeasureTextWidth(aCanvas, lLine, aFontSize, aFontStyle, aContext);

    if (FLayout.CurrentIndent+FLayout.LineX+lWidth)<=FLayout.MaxWidth then
      begin
      // aWord fits on current line
      if NeedSpace and (length(LineBuffer)>0) then
        LineBuffer:=LineBuffer + ' '
      else if NeedSpace and (length(LineBuffer) = 0) and (FLayout.LineX>0) then
        // Add space at start of line buffer if we're continuing text on the same line
        LineBuffer:=' ';
      LineBuffer:=LineBuffer + aWord;
      NeedSpace:=True;
      end
    else
      begin
      // aWord doesn't fit, flush current line and try on new line
      FlushCurrentLine;
      NewLine(aCanvas);
      NeedSpace:=False;
      // Check if aWord fits on new line
      lWidth:=MeasureTextWidth(aCanvas, aWord, aFontSize, aFontStyle, aContext);
      if (FLayout.CurrentIndent + FLayout.LineX + lWidth) <= FLayout.MaxWidth then
        begin
        LineBuffer:=aWord;
        NeedSpace:=True;
        end
      else
        // aWord is too long, break it character by character
        BreakLongWord(aWord);
      end;
  end;

var
  Index, lLength: LongInt;
  lChar: ansichar;
  WordBuffer, lText: utf8string;
  TextHeight: LongInt;
  FirstIndex, LastIndex: LongInt;

begin
  if aText='' then
    exit;
  lText:=aText;
  TextHeight:=MeasureTextHeight(aCanvas,aFontSize, aFontStyle, aContext);
  if TextHeight>FLayout.LineHeight then
    FLayout.LineHeight:=TextHeight;
  lLength:=length(lText);
  Index:=1;
  WordBuffer:='';
  LineBuffer:='';
  NeedSpace:=FLayout.NeedSpaceBeforeNextText;

  FirstIndex:=-1;
  LastIndex:=-1;

  while Index <= lLength do
    begin
    lChar:=lText[Index];
    if Not (lChar in WhiteSpace) then
      begin
      // Regular character
      if FirstIndex<0 then
        FirstIndex:=Index;
      LastIndex:=Index;
      end
    else
      begin
      // Whitespace - end of word, process accumulated word
      if FirstIndex >= 0 then
        begin
        // We were accumulating a word, extract it
        WordBuffer:=copy(lText, FirstIndex, (LastIndex - FirstIndex) + 1);
        FirstIndex:=-1;
        LastIndex:=-1;
        end;
      if length(WordBuffer)>0 then
        begin
        AddWordToLine(WordBuffer);
        WordBuffer:='';
        end;

      NeedSpace:=(lChar in [#13, #10]) or aPreserveWhitespace;
      if Not NeedSpace then
        begin
        AddWordToLine(lChar);
        NeedSpace:=False;
        end;
      // Treat #13#10 as #10
      if (lChar = #13) and (Index<lLength) and (lText[Index+1]=#10) then
        Inc(Index);
      end;
    Inc(Index);
    end;

  // Process final word if any
  if FirstIndex >= 0 then
    begin
    // We were accumulating a word, extract it
    WordBuffer:=copy(lText, FirstIndex, (LastIndex - FirstIndex)+1);
    FirstIndex:=-1;
    LastIndex:=-1;
    end;
  if length(WordBuffer)>0 then
    AddWordToLine(WordBuffer);
  // Flush final line
  FlushCurrentLine;
  FLayout.NeedSpaceBeforeNextText:=(lLength>0) and (lText[lLength] in WhiteSpace);
end;

procedure TMarkDownCanvasRenderer.CalculateLayout(const aCanvas: TCanvas; const aMaxWidth: LongInt;
  out aContentWidth: LongInt; out aContentHeight: LongInt);
begin
  if not assigned(aCanvas) then
    begin
    aContentWidth:=0;
    aContentHeight:=0;
    exit;
    end;
  fCanvas:=aCanvas;
  if FBlockQuoteIndent<>0 then
    FCalculatedBlockQuoteIndent:=FBlockQuoteIndent
  else
    FCalculatedBlockQuoteIndent:=MeasureTextWidth(FCanvas,'WWWW',fBaseFontSize,[],[]);
  BeginLayout;

  FLayout.MaxWidth:=aMaxWidth;
  if FLayout.MaxWidth <= 0 then
    FLayout.MaxWidth:=1;

  if assigned(fDocument) then
    begin
    RenderDocument(fDocument);
    if FLayout.LineX<>0 then
      NewLine(fCanvas);
    fCalculatedHeight:=FLayout.LineY;
    end
  else
    begin
    // No document parsed - set minimal dimensions
    fCalculatedHeight:=0;
    fCalculatedWidth:=aMaxWidth;
    end;

  aContentWidth:=fCalculatedWidth;
  aContentHeight:=fCalculatedHeight;

  EndLayout;
end;

procedure TMarkDownCanvasRenderer.DrawLayoutItem(aCanvas : TCanvas; aItem : TLayoutItem;const aLeftPosition, aTopPosition: LongInt);
var
  lImage : TPicture;
  lX,lY : LongInt;
begin
  // Draw selection background first if item is selected
  DrawItemSelection(aCanvas, aItem, aLeftPosition, aTopPosition);

  lX:=aItem.X+aLeftPosition;
  lY:=aItem.Y+aTopPosition;
  case aItem.Kind of
   TLayoutItemKind.likText:
     begin
     if aItem.IsSelected then
       aCanvas.Brush.Style:=bsClear
     else if ([fcCode,fcFencedCode] * aItem.Context)<>[] then
       aCanvas.Brush.Color:=aItem.GetColor(ckBackground,BGCodeColor)
     else
       aCanvas.Brush.Color:=aItem.GetColor(ckBackground,BGColor);
     with aItem do
       begin
       ApplyFont(aCanvas, FontSize, FontStyle, Context);
       aCanvas.Font.Color:=aItem.GetColor(ckForeground,GetTextColor(aItem.Context));
       aCanvas.TextOut(lX,lY, Text);
       end;
     end;
   TLayoutItemKind.likRect,
   TLayoutItemKind.likBackground:
     begin
     aCanvas.Pen.Style:=psSolid;
     if aItem.Kind=TLayoutItemKind.likRect then
       aCanvas.Pen.Color:=clBlack
     else
       aCanvas.Pen.Color:=aItem.GetColor(ckBackground,BGColor);
     aCanvas.Pen.Width:=1;
     aCanvas.Brush.Color:=aItem.GetColor(ckBackGround,BGColor);
     aCanvas.Rectangle(lX,LY,lX+aItem.Width,lY+aItem.Height);
     end;
   TLayoutItemKind.likLine:
     begin
     aCanvas.Pen.Color:=clBlack;
     aCanvas.Pen.Width:=1;
     With aItem do
       aCanvas.Line(lX,lY,lX+DeltaX,lY+DeltaY);
     end;
   TLayoutItemKind.likImage:
     begin
     lImage:=TPicture(FImageCache[aItem.URL]);
     if assigned(lImage) then
       With aItem do
         aCanvas.Draw(lX,lY,lImage.Graphic);
     end;
 end;
end;

procedure TMarkDownCanvasRenderer.DrawLayout(const aCanvas: TCanvas; const aLeftPosition, aTopPosition: LongInt);
var
  Index: LongInt;
  Item: TLayoutItem;
  OldStyle: TFontStyles;
  OldSize: LongInt;
  OldName: utf8string;
  OldBrushStyle: TBrushStyle;
  OldPenStyle: TPenStyle;
  OldPenColor: TColor;
  OldBrushColor: TColor;

begin
  if not assigned(aCanvas) then
    exit;
  Index:=0;
  while Index<FLists.Items.Count do
    begin
    OldStyle:=aCanvas.Font.Style;
    OldSize:=aCanvas.Font.Size;
    OldName:=aCanvas.Font.Name;
    OldBrushStyle:=aCanvas.Brush.Style;
    OldPenStyle:=aCanvas.Pen.Style;
    OldPenColor:=aCanvas.Pen.Color;
    OldBrushColor:=aCanvas.Brush.Color;
    aCanvas.Font.Style:=[];
    Item:=FLists.Items[Index];
    DrawLayoutItem(aCanvas,Item,aLeftPosition,aTopPosition);
    aCanvas.Font.Style:=OldStyle;
    aCanvas.Font.Size:=OldSize;
    aCanvas.Font.Name:=OldName;
    aCanvas.Brush.Style:=OldBrushStyle;
    aCanvas.Pen.Style:=OldPenStyle;
    aCanvas.Brush.Color:=OldBrushColor;
    aCanvas.Pen.Color:=OldPenColor;

    Inc(Index);
    end;
end;

function TMarkDownCanvasRenderer.HitTestLink(const aX, aY: LongInt; out aHref: utf8string): boolean;
var
  I: LongInt;
  Itm: TLinkHitRect;
begin
  aHref:='';
  Result:=False;
  I:=0;
  While Not Result and (I<FLists.LinkRects.Count) do
    begin
    Itm:=FLists.LinkRects[I];
    Result:=Itm.HasPoint(aX,aY);
    if Result then
      begin
      aHref:=Itm.Href;
      Exit;
      end;
    Inc(I);
    end;
end;

function TMarkDownCanvasRenderer.CreateLayoutItem(aKind: TLayoutItemKind; aX, aY: Longint): TLayoutItem;
begin
  Result:=FLists.Items.NewItem(aKind,aX,aY);
end;

function TMarkDownCanvasRenderer.LayoutImage(aImageURL : UTF8String) : Boolean;
var
  lImage : TPicture;
  lItem : TLayoutItem;
  I,LineDelta,BaseLineShiftDelta : Integer;
  TM: TLCLTextMetric;

begin
  lImage:=TPicture(FImageCache.Items[aImageURL]);
  if not assigned(lImage) then
    begin
    if assigned(FOnGetImage) then
      begin
      FOnGetImage(Self,aImageURL,lImage);
      if assigned(lImage) then
        FImageCache.Add(aImageURL,lImage);
      end;
    end;
  Result:=assigned(lImage);
  if not Result then exit;

  // Shift all texts before the image if needed.
  // To make the image aligned with the baseline, we need the descender font metric
  fCanvas.GetTextMetrics(TM);
  BaselineShiftDelta:=lImage.Height-FLayout.LineHeight-FLayout.BaselineShiftCurrent-TM.Descender;
  // If we need to shift, shift.
  if BaselineShiftDelta>0 then
    For I:=FLayout.BaselineFromIndex+1 to FLists.Items.Count-1 do
      With FLists.Items[i] do
        Y:=Y+BaselineShiftDelta;
  LineDelta:=FLayout.LineHeight-lImage.Height;
  if LineDelta<0 then
    LineDelta:=0;
  lItem:=CreateLayoutItem(likImage,FLayout.LineX+ImageMargin+FLayout.CurrentIndent,FLayout.LineY+lineDelta);
  lItem.Width:=lImage.Width;
  lItem.Height:=lImage.Height;
  lItem.URL:=aImageURL;
  FLayout.LineX:=FLayout.LineX + lImage.Width+(ImageMargin*2);
  // adapt baseshift so it is taken into account for the rest of the line.
  if BaselineShiftDelta>0 then
    FLayout.BaselineShiftCurrent:=FLayout.BaselineShiftCurrent+BaseLineShiftDelta;
  // Adapt line height
  if FLayout.LineHeight<lImage.Height then
    FLayout.LineHeight:=lImage.Height;
end;

procedure TMarkDownCanvasRenderer.CollectHTMLEntities;
var
  Ent : THTMLEntityDef;
begin
  for ent in EntityDefList do
    FHTMLEntities.Add(ent.e,Utf8Encode(ent.u));
end;

function TMarkDownCanvasRenderer.ResolveEntities(const aText: String): String;


  procedure copytoresult(aEnd,aStart : Integer);
  begin
    Result:=Result+Copy(aText,aStart,aEnd-aStart+1);
  end;

var
  lend,lPrev,lNext,lUnicode : Integer;
  lUChar : UnicodeChar;
  lEnt,lUTF8 : String;

begin
  if FHTMLEntities.Count=0 then
    CollectHTMLEntities;
  lPrev:=1;
  lNext:=Pos('&',aText,1);
  if lNext=0 then
    Exit(aText);
  Result:='';
  While lNext>0 do
    begin
    lUTF8:='';
    CopyToResult(lNext-1,lPrev);
    lEnd:=Pos(';',aText,lNext+1);
    lEnt:=Copy(aText,lNext+1,lEnd-lNext-1);
    if (lEnt<>'') then
      begin
      if lEnt[1]<>'#' then
        lUTF8:=FHTMLEntities.Items[LowerCase(lEnt)]
      else if TryStrToInt(Copy(lEnt,2,Length(lEnt)-1),lUnicode) then
        begin
        if (lUnicode=0) or (lUnicode>65535) then
          lUChar:=#$FFFD
        else
          lUChar:=UnicodeChar(lUnicode);
        lUTF8:=Utf8Encode(lUChar);
        end;
      lNext:=lEnd;
      end;
    if lUTF8='' then
      Result:=Result+lEnt+';'
    else
      Result:=Result+lUTF8;
    lPrev:=lNext+1;
    lNext:=Pos('&',aText,lPrev);
    end;
  CopyToResult(Length(aText),lPrev+1);
end;

procedure TMarkDownCanvasRenderer.RenderTextNode(aTextNode: TMarkDownTextNode; aFontSize: LongInt;
  aFontStyle: TFontStyles; const aContext : TFontContext);
var
  lItem : TLayoutItem;
  fontStyle: TFontStyles;
  lText,linkHref: utf8string;
  lContext : TFontContext;
  Alt : string;
begin
  if not assigned(aTextNode) then exit;
  lContext:=aContext;
  fontStyle:=aFontStyle + GetNodeFontStyle(aTextNode);
  lText:=ResolveEntities(aTextNode.NodeText);
  linkHref:='';
  if aTextNode.Kind = nkCode then
    lContext:=lContext+[fcMono,fcCode];
  case aTextNode.Kind of
    nkCode,
    nkText:
      begin
      if fcFencedCode in lContext then
        begin
        lItem:=FLists.Items.NewItem(likBackground,FLayout.LineX,FLayout.LineY);
        lItem.BGColor:=GetTextBGColor([fcFencedCode]);
        lItem.Width:=FLayout.MaxWidth-LItem.X;
        FlushTextRun(fCanvas, lText, aFontSize, fontStyle, '', lContext, False);
        NewLine(fCanvas);
        lItem.Height:=FLayout.LineY-lItem.Y;
        end
      else
        LayoutTextWrapped(fCanvas, lText, aFontSize, fontStyle, linkHref, lContext, false, False);
      end;
    nkLineBreak:
      NewLine(fCanvas);
    nkURI, nkEmail:
      begin
      linkHref:=aTextNode.Attrs['href'];
      LayoutTextWrapped(fCanvas, lText, aFontSize, fontStyle + [fsUnderline], linkHref,
        lContext+[fcHyperLink], False, False);
      end;
    nkImg:
      if Not LayoutImage(aTextNode.Attrs['src']) then
        begin
        Alt:=ResolveEntities(aTextNode.Attrs['alt']);
        if Alt='' then
          Alt:='img';
        LayoutTextWrapped(fCanvas, '['+Alt+']', aFontSize, fontStyle, '', lContext+[fcQuote], False, False);
        end;
  end;
end;

function TMarkDownCanvasRenderer.GetNodeFontStyle(aTextNode: TMarkDownTextNode): TFontStyles;
begin
  Result:=[];
  if nsStrong in aTextNode.Styles then
    Include(Result, fsBold);
  if nsEmph in aTextNode.Styles then
    Include(Result, fsItalic);
  if nsDelete in aTextNode.Styles then
    Include(Result, fsStrikeOut);
end;

procedure TMarkDownCanvasRenderer.Indent(aSize: integer);
begin
  inc(aSize,fExtraIndent);
  if FLayout.LineX>0 then
    FNextLineIndent:=FNextLineIndent+aSize
  else
    FLayout.CurrentIndent:=FLayout.CurrentIndent+aSize;
end;

procedure TMarkDownCanvasRenderer.SetBulletChar(AIndex: Integer; AValue: string);
begin
  FBullets[aIndex]:=aValue;
end;

procedure TMarkDownCanvasRenderer.Undent(aSize: integer);
begin
  inc(aSize,fExtraIndent);
  { Unwind any deferred indent first, so a matched Indent/Undent pair on a
    single line leaves no phantom indent behind for the next line. }
  if FNextLineIndent >= aSize then
    begin
    Dec(FNextLineIndent, aSize);
    Exit;
    end;
  Dec(aSize, FNextLineIndent);
  FNextLineIndent := 0;
  FLayout.CurrentIndent:=FLayout.CurrentIndent-aSize;
  if FLayout.CurrentIndent<0 then
    FLayout.CurrentIndent:=0;
end;

procedure TMarkDownCanvasRenderer.IncBulletLevel;
begin
  Inc(FBulletLevel);
end;

procedure TMarkDownCanvasRenderer.DecBulletLevel;
begin
  Dec(FBulletLevel);
  if FBulletLevel<0 then
    FBulletLevel:=0;
end;

function TMarkDownCanvasRenderer.GetBullet: String;
begin
  Result:=GetBulletAt(BulletLevel)
end;

function TMarkDownCanvasRenderer.GetBulletAt(aLevel: integer): string;
begin
  While aLevel>BulletCount do
    aLevel:=aLevel-BulletCount;
  Result:=FBullets[aLevel];
end;


{ TLayoutLists }

procedure TLayoutLists.Clear;
begin
  Items.Clear;
  LinkRects.Clear;
end;

class function TLayoutLists.Create: TLayoutLists;
begin
  Result.Items:=TLayoutItemList.Create;
  Result.LinkRects:=TLinkHitRectList.Create;
end;

procedure TLayoutLists.Free;
begin
  FreeAndNil(Items);
  FreeAndNil(LinkRects);
end;


{ TCanvasMarkDownBlockRenderer }

function TCanvasMarkDownBlockRenderer.GetCanvas: TCanvas;
begin
  Result:=CanvasRenderer.FCanvas;
end;

function TCanvasMarkDownBlockRenderer.GetCanvasRenderer: TMarkDownCanvasRenderer;
begin
  if Renderer is TMarkDownCanvasRenderer then
    Result:=TMarkDownCanvasRenderer(Renderer)
  else
    Result:=nil;
end;

function TCanvasMarkDownBlockRenderer.DIP(aSize: integer): Integer;
begin
  Result:=CanvasRenderer.DIP(aSize);
end;

procedure TCanvasMarkDownBlockRenderer.Indent(aSize: Integer);
begin
  CanvasRenderer.Indent(aSize);
end;

procedure TCanvasMarkDownBlockRenderer.Undent(aSize: Integer);
begin
 CanvasRenderer.Undent(aSize);
end;

function TCanvasMarkDownBlockRenderer.LayoutData: TLayoutData;
begin
  Result:=CanvasRenderer.Layout;
end;

function TCanvasMarkDownBlockRenderer.CreateLayoutItem(aKind: TLayoutItemKind; aX, aY: Longint): TLayoutItem;
begin
  Result:=CanvasRenderer.CreateLayoutItem(aKind, aX, aY);
end;

function TCanvasMarkDownBlockRenderer.MeasureTextHeight(const aFontSize: LongInt; const aFontStyle: TFontStyles;
  const aContext: TFontContext): LongInt;
begin
  Result:=CanvasRenderer.MeasureTextHeight(CanvasRenderer.FCanvas,aFontSize,aFontStyle,aContext);
end;

function TCanvasMarkDownBlockRenderer.MeasureTextWidth(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; aFontContext : TFontContext): LongInt;
begin
  Result:=CanvasRenderer.MeasureTextWidth(CanvasRenderer.FCanvas,aText,aFontSize,aFontStyle,aFontContext);
end;

procedure TCanvasMarkDownBlockRenderer.LayoutText(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext: TFontContext);
begin
  CanvasRenderer.LayoutTextWrapped(CanvasRenderer.fCanvas, aText, aFontSize, aFontStyle, aLinkHref,aContext, False, False);
end;

procedure TCanvasMarkDownBlockRenderer.SetCurrentY(aValue: Integer);
begin
  CanvasRenderer.SetCurrentY(aValue);
end;

function TCanvasMarkDownBlockRenderer.GetCurrentY: Integer;
begin
  Result:=CanvasRenderer.FLayout.Liney;
end;

procedure TCanvasMarkDownBlockRenderer.NewLine;
begin
  CanvasRenderer.NewLine(CanvasRenderer.fCanvas);
end;

procedure TCanvasMarkDownBlockRenderer.ParagraphBreak;
begin
  CanvasRenderer.ParagraphBreak(CanvasRenderer.fCanvas);
end;

procedure TCanvasMarkDownBlockRenderer.MaybeStartParagraph;
begin
  CanvasRenderer.MaybeStartParagraph;
end;

{ TCanvasMarkDownTextRenderer }

function TCanvasMarkDownTextRenderer.GetCanvasRenderer: TMarkDownCanvasRenderer;
begin
  if Renderer is TMarkDownCanvasRenderer then
    Result:=TMarkDownCanvasRenderer(Renderer)
  else
    Result:=nil;
end;

procedure TCanvasMarkDownTextRenderer.DoRender(aElement: TMarkDownTextNode);
begin
  if not assigned(aElement) or not assigned(CanvasRenderer) then exit;
  CanvasRenderer.RenderTextNode(aElement, CanvasRenderer.fBaseFontSize, [], []);
end;

{ TCanvasMarkDownParagraphBlockRenderer }

procedure TCanvasMarkDownParagraphBlockRenderer.DoRender(aBlock: TMarkDownBlock);
begin
  if not assigned(aBlock) then exit;

  // Render paragraph children and add paragraph break
  Renderer.RenderChildren(aBlock as TMarkDownContainerBlock);
  ParagraphBreak;
end;

class function TCanvasMarkDownParagraphBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownParagraphBlock;
end;

{ TCanvasMarkDownHeadingBlockRenderer }

procedure TCanvasMarkDownHeadingBlockRenderer.DoRender(aBlock: TMarkDownBlock);
const
  FontSizeCount = 5;
  FontSizes : Array[1..FontSizeCount] of Integer = (10,8,6,4,2);
var
  lHeading: TMarkDownHeadingBlock;

begin
  if not assigned(aBlock) then exit;
  lHeading:=aBlock as TMarkDownHeadingBlock;
  FontSize:=CanvasRenderer.fBaseFontSize;
  if (lHeading.Level in [1..FontSizeCount]) then
    Inc(FontSize,FontSizes[lHeading.Level]);
  Renderer.RenderChildren(lHeading);
  ParagraphBreak;
end;

class function TCanvasMarkDownHeadingBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownHeadingBlock;
end;

{ TCanvasMarkDownQuoteBlockRenderer }

procedure TCanvasMarkDownQuoteBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lIndent : Integer;
begin
  if not assigned(aBlock) then exit;
  lIndent:=CanvasRenderer.CalculatedBlockQuoteIndent;
  CanvasRenderer.Indent(lIndent);
  Renderer.RenderChildren(aBlock as TMarkDownContainerBlock);
  CanvasRenderer.Undent(lindent);
  ParagraphBreak;
end;

class function TCanvasMarkDownQuoteBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownQuoteBlock;
end;

{ TCanvasMarkDownListBlockRenderer }

procedure TCanvasMarkDownListBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lBlock : TMarkDownListBlock absolute aBlock;
  i : Integer;
begin
  if not assigned(aBlock) then exit;
  if not lBlock.Ordered then
    CanvasRenderer.IncBulletLevel;
  // Render all list items
  FItemNumber:=0;
  For I:=0 to aBlock.ChildCount-1 do
    begin
    if lBlock.Ordered then
      FItemNumber:=lBlock.Start+i;
    Renderer.RenderBlock(aBlock.Children[i]);
    end;
  if not lBlock.Ordered then
    CanvasRenderer.DecBulletLevel;
  ParagraphBreak;
end;

procedure TCanvasMarkDownListBlockRenderer.reset;
begin
  inherited reset;
  FItemNumber:=0;
end;

class function TCanvasMarkDownListBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownListBlock;
end;

{ TCanvasMarkDownListItemBlockRenderer }

procedure TCanvasMarkDownListItemBlockRenderer.DoRender(aBlock: TMarkDownBlock);

var
  lBlock : TMarkDownListItemBlock absolute aBlock;
  lListRenderer : TCanvasMarkDownListBlockRenderer;
  lPrefix : string;
  lIndent : Integer;

begin
  if not assigned(aBlock) then exit;
  // Add bullet point and render children
  lListRenderer:=TCanvasMarkDownListBlockRenderer(GetFirstParentWithClass(TCanvasMarkDownListBlockRenderer));
  if Assigned(lListRenderer) and (lListRenderer.ItemNumber>0) then
    lPrefix:=IntToStr(lListRenderer.ItemNumber)+'.'
  else
    lPrefix:=CanvasRenderer.GetBullet;
  LayoutText(lPrefix+' ', CanvasRenderer.fBaseFontSize, [], '', []);
  lIndent:=MeasureTextWidth(lPrefix+'_',CanvasRenderer.BaseFontSize,[],[]);
  CanvasRenderer.Indent(lIndent);
  Renderer.RenderChildren(lBlock);
  CanvasRenderer.Undent(lIndent);
  NewLine;
end;

class function TCanvasMarkDownListItemBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownListItemBlock;
end;

{ TCanvasMarkDownCodeBlockRenderer }

procedure TCanvasMarkDownCodeBlockRenderer.DoRender(aBlock: TMarkDownBlock);
begin
  if not assigned(aBlock) then exit;
  ParagraphBreak;
  // Render code with monospace font
  Renderer.RenderChildren(aBlock as TMarkDownContainerBlock);
  ParagraphBreak;
end;

class function TCanvasMarkDownCodeBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownCodeBlock;
end;

{ TCanvasMarkDownTableBlockRenderer }
procedure TCanvasMarkDownTableBlockRenderer.GetCellTexts(aCell: TMarkDownBlock; aTexts : TStrings);

var
  lCont : TMarkDownContainerBlock absolute aCell;
  lText : TMarkDownTextBlock absolute aCell;
  i : integer;

begin
  if aCell is TMarkDownTextBlock then
    begin
    for I:=0 to lText.Nodes.Count-1 do
      aTexts.Add(lText.Nodes[i].NodeText)
    end
  else
    For I:=0 to lCont.ChildCount do
      GetCellTexts(lCont[i],aTexts);
end;

function TCanvasMarkDownTableBlockRenderer.GetTotalColumns: Integer;
begin
  Result:=Length(FColLayout);
end;

function TCanvasMarkDownTableBlockRenderer.GetTotalHeight: Integer;
var
  I : Integer;
begin
  Result:=0;
  For I:=0 to Length(FRowHeights)-1 do
    Result:=Result+FRowHeights[i];
end;

procedure TCanvasMarkDownTableBlockRenderer.CalcRowHeights(aTable: TMarkDownTableBlock; const aFontSize: LongInt; const aFontStyle: TFontStyles; const aContext : TFontContext);

var
  lRow,lColumn:LongInt;
  CellWidth,TextWidth:LongInt;
  Saved, Temp: TLayoutData;
  SavedItems, TempItems : TLayoutLists;
  Cell:TMarkDownBlock;
  CellFontStyle:TFontStyles;

begin
 SetLength(FRowHeights,aTable.ChildCount);
 For lRow:=0 to aTable.ChildCount-1 do
   begin
   FRowHeights[lRow]:=0;
   For lColumn:=0 to Min(aTable.Children[lRow].ChildCount-1, TotalColumns-1) do
     begin
     Cell:=aTable.Children[lRow].Children[lColumn];

     CellWidth:=0;
     CellWidth:=CellWidth+FColLayout[lColumn].Width;
     TextWidth:=CellWidth-(FCellPadding*2);
     if TextWidth<1 then
      TextWidth:=1;

     Temp:=Default(TLayoutData);
     Temp.MaxWidth:=TextWidth;
     Saved:=CanvasRenderer.SwapLayout(Temp);
     CellFontStyle:=aFontStyle;
     if lRow=0 then
       Include(CellFontStyle,fsBold);
      // dry-run traversal inside a temporary items/link list to measure height only
      TempItems:=TLayoutLists.Create;
      SavedItems:=CanvasRenderer.SwapLayoutLists(TempItems);
      try
         CanvasRenderer.RenderBlock(Cell);
      finally
        CanvasRenderer.SwapLayoutLists(SavedItems);
        TempItems.Free;
      end;
      Temp:=CanvasRenderer.SwapLayout(Saved);
      if (Temp.LineY+Temp.LineHeight)>FRowHeights[lRow] then
       FRowHeights[lRow]:=Temp.LineY+Temp.LineHeight;
      end;
    if FRowHeights[lRow]<MeasureTextHeight(aFontSize,aFontStyle,aContext) then
      FRowHeights[lRow]:=MeasureTextHeight(aFontSize,aFontStyle,aContext);
    FRowHeights[lRow]:=FRowHeights[lRow]+(FCellPadding*2);
    end;
end;

procedure TCanvasMarkDownTableBlockRenderer.MeasureTableColumns(const aTableModel:TMarkDownTableBlock);


  function MaxWordWidth(aTexts : TStrings; aFontSize : integer):LongInt;

  var ScanIndex:LongInt;
      WordBuffer:UTF8String;
      BestWidth,Current:LongInt;
      text : string;

      procedure Updatebest;
      begin
        if WordBuffer='' then
          exit;
        Current:=MeasureTextWidth(WordBuffer,aFontSize,[],[fcQuote]);
        if Current>BestWidth then
          BestWidth:=Current;
        WordBuffer:='';
      end;

  begin
    BestWidth:=0;
    WordBuffer:='';
    for Text in aTexts do
      begin
      ScanIndex:=1;
      while ScanIndex<=length(Text) do
        begin
        if (Text[ScanIndex]=' ') or (Text[ScanIndex]=#9) or (Text[ScanIndex]=#10) or (Text[ScanIndex]=#13) then
          UpdateBest
        else
          WordBuffer:=WordBuffer+Text[ScanIndex];
        inc(ScanIndex);
        end;
        UpdateBest;
      end;
    result:=BestWidth;
  end;

  function NoWrapWidth(Texts : TStrings; aFontSize : Integer):LongInt;
  begin
    Result:=MeasureTextWidth(Texts.DelimitedText,aFontSize,[],[fcQuote]);
  end;

var
  lColumn,lRow :LongInt;
  MinimumWord,PreferredNoWrap:LongInt;
  Cell:TMarkDownBlock;
  lRowBlock : TMarkDownTableRowBlock;
  lFontSize : Integer;
  Texts : TStrings;

begin
  lFontSize:=CanvasRenderer.BaseFontSize;
  For lColumn:=0 to TotalColumns-1 do
    begin
    FColLayout[lColumn].Min:=DIP(8);
    FColLayout[lColumn].Pref:=DIP(16);
    end;
 Texts:=TstringList.Create;
 try
   Texts.Delimiter:=' ';
   For lRow:=0 to aTableModel.ChildCount-1 do
     begin
     lRowBlock:=aTableModel.Children[lRow] as TMarkDownTableRowBlock;
     For lColumn:=0 to Min(lRowBlock.ChildCount-1, TotalColumns-1) do
       begin
       Cell:=lRowBlock.Children[lColumn];
       Texts.Clear;
       GetCellTexts(Cell,Texts);
       MinimumWord:=MaxWordWidth(texts,lFontSize)+DIP(8);
       PreferredNoWrap:=NoWrapWidth(texts,lFontSize)+DIP(8);
       With FColLayout[lColumn] do
         begin
         if MinimumWord>Min then
           Min:=MinimumWord;
         if PreferredNoWrap>Pref then
           Pref:=PreferredNoWrap;
         end;
       end;
     end;
  finally
    Texts.Free;
  end;
end;

procedure TCanvasMarkDownTableBlockRenderer.DistributeColumns(const aAvailable:LongInt);

var I : LongInt;
    TotalMin,TotalPref,Extra,Remain,Room,Denom,SpanVal:LongInt;

begin
  TotalMin:=0;
  TotalPref:=0;
  For I:=0 to TotalColumns-1 do
    With FColLayout[i] do
      begin
      TotalMin:=TotalMin+Min;
      TotalPref:=TotalPref+Pref;
     end;
  if aAvailable<=TotalMin then
    begin
    // Less room than minimum - we allocate the minimum.
    For I:=0 to TotalColumns-1 do
      With FColLayout[i] do
        Width:=Min;
    end
  else if aAvailable>=TotalPref then
    begin
    // More room than total needed
    // First distribute evenly
    Extra:=aAvailable-TotalPref;
    For I:=0 to TotalColumns-1 do
      With FColLayout[i] do
        Width:=Pref+(Extra div TotalColumns);
    // Then distribute remainder over first cols
    Remain:=Extra mod TotalColumns;
    For I:=0 to Remain-1 do
      inc(FColLayout[i].Width);
    end
  else
    begin
    // More than minimum, less than preferred.
    // Give space evenly
    Room:=aAvailable-TotalMin;
    Denom:=TotalPref-TotalMin;
    for I:=0 to TotalColumns-1 do
      with FColLayout[i] do
        begin
        SpanVal:=Pref-Min;
        if Denom>0 then
          Width:=Min+((SpanVal*Room) div Denom)
        else
          Width:=Min;
        end;
    end;
end;

procedure TCanvasMarkDownTableBlockRenderer.DrawBorders;

var
  I, ColX : Integer;
  Itm : TLayoutItem;
begin
  Itm:=CreateLayoutItem(likRect,FStartX,FStartY);
  Itm.Width:=AvailableWidth;
  Itm.Height:=TotalHeight;
  ColX:=FStartX;
  For I:=0 to Length(FColLayout)-2 do
    begin
    ColX:=ColX+FColLayout[i].Width+FGridSize;
    Itm:=CreateLayoutItem(likLine,ColX,FStartY);
    Itm.DeltaX:=0;
    Itm.DeltaY:=TotalHeight;
    end;
end;

procedure TCanvasMarkDownTableBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lTable : TMarkDownTableBlock absolute aBlock;
  I : Integer;

begin
  if not assigned(aBlock) then exit;
  CanvasRenderer.MaybeStartParagraph;
  SetLength(FColLayout,Length(lTable.Columns));
  FGridSize:=DIP(1);
  FCellPadding:=DIP(4);
  FAvailableWidth:=CanvasRenderer.FLayout.MaxWidth-CanvasRenderer.FLayout.CurrentIndent-(TotalColumns-1)*FGridSize;
  if FAvailableWidth<1 then
    FAvailableWidth:=1;
  MeasureTableColumns(lTable);
  DistributeColumns(AvailableWidth);
  FStartX:=CanvasRenderer.FLayout.CurrentIndent;
  FStartY:=CanvasRenderer.FLayout.LineY;
  CalcRowHeights(lTable,CanvasRenderer.BaseFontSize,[],[]);
  DrawBorders;
  // Render table rows
  For I:=0 to aBlock.ChildCount-1 do
    begin
    FCurrentRow:=I;
    Renderer.RenderBlock(aBlock.Children[i]);
    FStartY:=FStartY+RowHeights[i]+GridSize;
    CanvasRenderer.SetCurrentY(FStartY);
    end;
  FCurrentRow:=-1;
  ParagraphBreak;
end;

procedure TCanvasMarkDownTableBlockRenderer.Reset;
begin
  inherited Reset;
  FCurrentRow:=-1;
  SetLength(FColLayout,0);
end;


class function TCanvasMarkDownTableBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableBlock;
end;

{ TCanvasMarkDownTableRowBlockRenderer }

procedure TCanvasMarkDownTableRowBlockRenderer.GetTableRenderer;

var
  A: TMarkDownElementRendererArray;
  I : integer;

begin
  A:=GetParentRenderers;
  FTableRenderer:=nil;
  I:=Length(A)-1;
  While (I>=0) and Not Assigned(FTableRenderer) do
    begin
    if A[I] is TCanvasMarkDownTableBlockRenderer then
      FTableRenderer:=TCanvasMarkDownTableBlockRenderer(A[i]);
    Dec(i);
    end;
  if FTableRenderer=nil then
    Raise EMarkDown.Create('Could not find markdown table renderer');
end;

procedure TCanvasMarkDownTableRowBlockRenderer.DoRender(aBlock: TMarkDownBlock);

var
  lColumn:LongInt;
  CellWidth,CurrentY,CurrentX:LongInt;
  Saved, Temp: TLayoutData;
  Cell:TMarkDownBlock;
  Itm:TLayoutItem;

begin
  if not assigned(aBlock) then
    exit;
  GetTableRenderer;
  CurrentY:=TableRenderer.FStartY;
  CurrentX:=TableRenderer.FStartX;
  if FTableRenderer.CurrentRow>0 then
    begin
    Itm:=CreateLayoutItem(likLine,CurrentX,CurrentY);
    Itm.DeltaX:=FTableRenderer.AvailableWidth;
    end;
  For lColumn:=0 to Min(aBlock.ChildCount-1, Length(FTableRenderer.ColLayout)-1) do
    begin
    Cell:=aBlock.Children[lColumn];
    CellWidth:=FTableRenderer.ColLayout[lColumn].Width;
    Temp:=Default(TLayoutData);
    Temp.MaxWidth:=CurrentX+CellWidth-FTableRenderer.CellPadding;
    Temp.CurrentIndent:=CurrentX+FTableRenderer.FCellPadding;
    Temp.LineY:=CurrentY+FTableRenderer.FCellPadding;
    Saved:=CanvasRenderer.SwapLayout(Temp);
    CanvasRenderer.RenderBlock(Cell);
    CanvasRenderer.SwapLayout(Saved);
    CurrentX:=CurrentX+FTableRenderer.FColLayout[lColumn].Width;
    CurrentX:=CurrentX+FTableRenderer.FGridSize;
    end;
end;

class function TCanvasMarkDownTableRowBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableRowBlock;
end;

{ TCanvasMarkDownThematicBreakBlockRenderer }

procedure TCanvasMarkDownThematicBreakBlockRenderer.DoRender(aBlock: TMarkDownBlock);
begin
  if not assigned(aBlock) then exit;

  // Render horizontal line
  LayoutText('────────────────────────────────────────', CanvasRenderer.fBaseFontSize, [], '', []);
  ParagraphBreak;
end;

class function TCanvasMarkDownThematicBreakBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownThematicBreakBlock;
end;

{ TCanvasMarkDownTextBlockRenderer }

procedure TCanvasMarkDownTextBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lTextBlock: TMarkDownTextBlock absolute aBlock;
  Parents : TMarkDownElementRendererArray;
  Parent : TMarkDownElementRenderer;
  i: integer;
  FontStyles : TFontStyles;
  FontSize : integer;
  FontContext : TFontContext;
begin
  if not assigned(aBlock) then exit;
  if not assigned(lTextBlock.Nodes) then exit;
  FontStyles:=[];
  FontContext:=[];
  FontSize:=CanvasRenderer.BaseFontSize;
  Parents:=GetParentRenderers;
  For I:=Length(Parents)-1 downto 0 do
    begin
    Parent:=Parents[i];
    if Parent is TCanvasMarkDownTableRowBlockRenderer then
      if TCanvasMarkDownTableRowBlockRenderer(Parent).FTableRenderer.CurrentRow=0 then
         Include(FontStyles,fsBold);
    if Parent is TCanvasMarkDownHeadingBlockRenderer then
      Fontsize:=TCanvasMarkDownHeadingBlockRenderer(Parent).fontSize;
    if (Parent is TCanvasMarkDownQuoteBlockRenderer) then
      Include(FontContext,fcQuote);
    if (Parent is TCanvasMarkdownCodeBlockRenderer) then
      FontContext:=FontContext+[fcMono,fcFencedCode];
    end;
  for i:=0 to lTextBlock.Nodes.Count - 1 do
  begin
    CanvasRenderer.RenderTextNode(lTextBlock.Nodes[i],FontSize, FontStyles, FontContext);
  end;
end;

class function TCanvasMarkDownTextBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTextBlock;
end;

initialization
  // Register all canvas block renderers
  TCanvasMarkDownParagraphBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownHeadingBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownQuoteBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownListBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownListItemBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownCodeBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownTableBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownTableRowBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownThematicBreakBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
  TCanvasMarkDownTextBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);

  // Register text renderer
  TCanvasMarkDownTextRenderer.RegisterRenderer(TMarkDownCanvasRenderer);
end.
