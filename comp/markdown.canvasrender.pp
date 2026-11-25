{
 /***************************************************************************
                                markdown.canvasrender
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
unit Markdown.CanvasRender;
{$mode objfpc}
{$h+}
{$codepage UTF8}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, Classes, Contnrs, Graphics, Types, Math, Markdown.Elements, Markdown.Parser, Markdown.Render, Markdown.HTMLEntities, LazUTF8, LazFileUtils, Character;

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
    FLineIndex: Integer;
    FSoftBreak: Boolean;
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
    // Index of the layout line the item belongs to
    property LineIndex: Integer read FLineIndex write FLineIndex;
    // True if the item continues the text of the previous line after a wrap inside a word or code line
    property SoftBreak: Boolean read FSoftBreak write FSoftBreak;
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
    LinkFromIndex : Longint;
    LineAscent : LongInt;
    LineDescent : LongInt;
    NeedSpaceBeforeNextText : Boolean;
  end;

  TAscentCacheEntry = record
    Mono : Boolean;
    Size : LongInt;
    Style : TFontStyles;
    Ascent : LongInt;
  end;

  TLayoutLists = record
    Items: TLayoutItemList;
    LinkRects: TLinkHitRectList;
    class function Create : TLayoutLists; Static;
    procedure Clear;
    procedure Free;
  end;

  TMarkdownImageEvent = procedure(Sender : TObject; const aURL : string; var aImage : TPicture) of object;

  { TMarkdownCanvasRenderer }

  TMarkdownCanvasRenderer = class(TMarkdownRenderer)
  private
    FCalculatedBlockQuoteIndent: Integer;
    const BulletCount = 3;
  private
    FBlockQuoteIndent: Integer;
    FBulletLevel: Integer;
    FBullets: Array[1..BulletCount] of string;
    FCanvas: TCanvas;
    FDocument: TMarkdownDocument;
    FHyperLinkColor: TColor;
    FImageMargin: integer;
    FOnGetImage: TMarkdownImageEvent;
    FParser: TMarkdownParser;
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
    FTableLinesColor: TColor;
    FHTMLEntities : TFPStringHashTable;
    FBaseDir: string;
    FLineCount: Integer;
    FWrapCodeBlocks: Boolean;
    FHeadingSpacing: Integer;
    FListItemSpacing: Integer;
    FCodeBlockPadding: Integer;
    FTableCellPaddingX: Integer;
    FTableCellPaddingY: Integer;
    FBaseLineHeight: LongInt;
    FAscentCache: array of TAscentCacheEntry;
    procedure SetBaseDir(const aValue: string);
  Protected
    // Selection methods
    procedure ClearAllSelections;
    procedure ApplySelection(const aStartPoint, aEndPoint: TSelectionPoint);
    function CalculateCharXPosition(aItem: TLayoutItem; aCharIndex: Integer): LongInt;
    function AdjacentTextItem(aIndex, aDirection: Integer): Integer;

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
    // Pixels for aPercent percent of the base font line height.
    function EmPercent(const aPercent: Integer): LongInt;
    procedure ApplyFont(const aCanvas: TCanvas; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext : TFontContext);
    procedure MeasureText(const aCanvas: TCanvas; const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aContext : TFontContext; out aWidth, aHeight: LongInt);
    function MeasureTextWidth(const aCanvas: TCanvas; const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aContext : TFontContext): LongInt;
    function MeasureTextHeight(const aCanvas: TCanvas; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext : TFontContext): LongInt;
    function MeasureTextAscent(const aCanvas: TCanvas; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext : TFontContext): LongInt;
    procedure ShiftLineItems(aDelta: LongInt);
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
    procedure LayoutCharWrapped(const aCanvas: TCanvas; const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext : TFontContext;
      const aDryRun: boolean);
    function LayoutImage(aImageURL: UTF8String): Boolean; virtual;
    function LoadLocalImage(const aURL: string): TPicture; virtual;
    procedure RenderTextNode(aTextNode: TMarkdownTextNode; aFontSize: LongInt; aFontStyle: TFontStyles; const aContext : TFontContext;
      const aLinkHref: utf8string = '');

    // Actual drawing
    procedure DrawLayoutItem(aCanvas: TCanvas; aItem: TLayoutItem; const aLeftPosition, aTopPosition: LongInt);
    procedure DrawItemSelection(aCanvas: TCanvas; aItem: TLayoutItem; const aLeftPosition, aTopPosition: LongInt);

    // Utility
    function GetNodeFontStyle(aTextNode: TMarkdownTextNode): TFontStyles;
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
    // Items of the current layout
    Property LayoutItems : TLayoutItemList read FLists.Items;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Markdown parsing
    procedure ParseMarkdown(const aMarkdown: utf8string); overload;
    procedure ParseMarkdown(const aMarkdown: TStrings); overload;

    // Rendering interface
    procedure RenderDocument(aDocument: TMarkdownDocument); override;
    procedure CalculateLayout(const aCanvas: TCanvas; const aMaxWidth: LongInt; out aContentWidth: LongInt;
      out aContentHeight: LongInt);
    procedure DrawLayout(const aCanvas: TCanvas; const aLeftPosition, aTopPosition: LongInt);
    function HitTestLink(const aX, aY: LongInt; out aHref: utf8string): boolean;

    // Selection methods
    procedure SetSelection(const aStart, aEnd: TSelectionPoint);
    procedure ClearSelection;
    function GetSelectedText: String;
    function HitTestText(const aX, aY: LongInt): TSelectionPoint;
    // Bounds of the word around aPoint, which may span several runs of one line.
    // Returns False if aPoint is not next to a letter, digit or underscore.
    function GetWordBounds(const aPoint: TSelectionPoint; out aStart, aEnd: TSelectionPoint): Boolean;
    // Bounds of the layout line containing aPoint.
    function GetLineBounds(const aPoint: TSelectionPoint; out aStart, aEnd: TSelectionPoint): Boolean;
    // Height of a line of text in the base font.
    function MeasureBaseLineHeight(const aCanvas: TCanvas): LongInt;
    // Free all cached images.
    procedure ClearImageCache;

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
    // Color of the table borders and cell separators.
    property TableLinesColor: TColor read FTableLinesColor write FTableLinesColor;
    // Directory used to resolve relative image file names.
    property BaseDir: string read FBaseDir write SetBaseDir;
    // Wrap long lines of fenced code blocks at the available width.
    property WrapCodeBlocks: Boolean read FWrapCodeBlocks write FWrapCodeBlocks;
    // Extra space above a heading, in percent of the base line height.
    property HeadingSpacing: Integer read FHeadingSpacing write FHeadingSpacing;
    // Extra space between list items, in percent of the base line height.
    property ListItemSpacing: Integer read FListItemSpacing write FListItemSpacing;
    // Padding around the contents of a code block, in percent of the base line height.
    property CodeBlockPadding: Integer read FCodeBlockPadding write FCodeBlockPadding;
    // Horizontal padding of table cells, in percent of the base line height.
    property TableCellPaddingX: Integer read FTableCellPaddingX write FTableCellPaddingX;
    // Vertical padding of table cells, in percent of the base line height.
    property TableCellPaddingY: Integer read FTableCellPaddingY write FTableCellPaddingY;
  end;

  { TCanvasMarkdownBlockRenderer }

  TCanvasMarkdownBlockRenderer = class(TMarkdownBlockRenderer)
  private
    function GetCanvas: TCanvas;
    function GetCanvasRenderer: TMarkdownCanvasRenderer;
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
    // Ends aBlock: a line break inside an item of a tight list, a paragraph break elsewhere.
    procedure EndBlock(aBlock : TMarkdownBlock);
    procedure MaybeStartParagraph; inline;
    property Canvas : TCanvas read GetCanvas;
  public
    property CanvasRenderer: TMarkdownCanvasRenderer read GetCanvasRenderer;
  end;

  TCanvasMarkdownBlockRendererClass = class of TCanvasMarkdownBlockRenderer;

  { TCanvasMarkdownTextRenderer }

  TCanvasMarkdownTextRenderer = class(TMarkdownTextRenderer)
  private
    function GetCanvasRenderer: TMarkdownCanvasRenderer;
  protected
    procedure DoRender(aElement: TMarkdownTextNode); override;
  public
    property CanvasRenderer: TMarkdownCanvasRenderer read GetCanvasRenderer;
  end;

  { Individual Canvas Block Renderers }

  { TCanvasMarkdownParagraphBlockRenderer }
  TCanvasMarkdownParagraphBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownHeadingBlockRenderer }
  TCanvasMarkdownHeadingBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    fontSize: LongInt;
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownQuoteBlockRenderer }
  TCanvasMarkdownQuoteBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownListBlockRenderer }
  TCanvasMarkdownListBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  Private
    FItemNumber : Integer;
    FOrdered : Boolean;
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
    property ItemNumber : Integer Read FItemNumber;
    // True while rendering an ordered list.
    property Ordered : Boolean Read FOrdered;
  public
    procedure reset; override;
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownListItemBlockRenderer }
  TCanvasMarkdownListItemBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownCodeBlockRenderer }
  TCanvasMarkdownCodeBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownTableBlockRenderer }
  TCanvasMarkdownTableBlockRenderer = class(TCanvasMarkdownBlockRenderer)
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
    FCellPaddingX : Integer;
    FCellPaddingY : Integer;
    FStartX,FStartY : Integer;
    FRowHeights: TLongintArray;
    FCurrentRow : integer;
    FFontStyles : TFontStyles;
    procedure CalcRowHeights(aTable: TMarkdownTableBlock; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext: TFontContext);
    procedure DistributeColumns(const aAvailable: LongInt);
    procedure DrawBorders;
    procedure MeasureCell(aCell: TMarkdownBlock; aHeader: Boolean; var aMin, aPref: LongInt);
    function GetTotalColumns: Integer;
    function GetTotalHeight: Integer;
    function GetTotalWidth: Integer;
    procedure MeasureTableColumns(const aTableModel: TMarkdownTableBlock);
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
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
    // Horizontal cell padding
    Property CellPaddingX : Integer Read FCellPaddingX;
    // Vertical cell padding
    Property CellPaddingY : Integer Read FCellPaddingY;
    // Available width, calculated in DoRender
    Property AvailableWidth : Integer Read FAvailableWidth;
    // Font styles
    Property FontStyles : TFontStyles Read FFontStyles;
    // Total height as calculated
    Property TotalHeight : Integer Read GetTotalHeight;
    // Total width of the columns and grid lines
    Property TotalWidth : Integer Read GetTotalWidth;
  public
    Procedure Reset; override;
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownTableRowBlockRenderer }
  TCanvasMarkdownTableRowBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  private
    FTableRenderer : TCanvasMarkdownTableBlockRenderer;
    procedure GetTableRenderer;
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
    Property TableRenderer : TCanvasMarkdownTableBlockRenderer read FTableRenderer;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownThematicBreakBlockRenderer }
  TCanvasMarkdownThematicBreakBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownTextBlockRenderer }
  TCanvasMarkdownTextBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownFrontmatterBlockRenderer }
  // Front matter is not displayed.
  TCanvasMarkdownFrontmatterBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownContainerBlockRenderer }
  // Container blocks without a renderer of their own: only their children are displayed.
  TCanvasMarkdownContainerBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
  end;

  { TCanvasMarkdownLeafBlockRenderer }
  // Leaf blocks without a renderer of their own are not displayed.
  TCanvasMarkdownLeafBlockRenderer = class(TCanvasMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  public
    class function BlockClass: TMarkdownBlockClass; override;
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


{ TMarkdownCanvasRenderer }

constructor TMarkdownCanvasRenderer.Create(aOwner: TComponent);

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

  fParser:=TMarkdownParser.Create(Self);

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
  FTableLinesColor:=clBlack;

  fBaseFontSize:=10;
  FTableCellPaddingX:=25;
  FTableCellPaddingY:=25;
  FLayout.BaselineShiftCurrent:=0;

  // Initialize selection
  FSelectionColor:=clHighlight;
end;

destructor TMarkdownCanvasRenderer.Destroy;
begin
  FreeAndNil(FHTMLEntities);
  FreeAndNil(FImageCache);
  FreeAndNil(FLists.Items);
  FreeAndNil(FLists.LinkRects);
  FreeAndNil(fDocument);
  FreeAndNil(fParser);
  inherited Destroy;
end;


procedure TMarkdownCanvasRenderer.ClearAllSelections;
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

function TMarkdownCanvasRenderer.CalculateCharXPosition(aItem: TLayoutItem; aCharIndex: Integer): LongInt;
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

procedure TMarkdownCanvasRenderer.ApplySelection(const aStartPoint, aEndPoint: TSelectionPoint);
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

procedure TMarkdownCanvasRenderer.DrawItemSelection(aCanvas: TCanvas; aItem: TLayoutItem; const aLeftPosition, aTopPosition: LongInt);
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

function TMarkdownCanvasRenderer.HitTestText(const aX, aY: LongInt): TSelectionPoint;
var
  I, lBest, lBestDist, lDist, lLine, lCharIndex, lLen: Integer;
  lItem: TLayoutItem;
  lRelX, lCurX, lCharW: LongInt;
begin
  Result.LayoutItemIndex:=-1;
  Result.CharOffset:=0;
  // Text item vertically closest to aY: its line is the target line
  lBest:=-1;
  lBestDist:=MaxInt;
  for I:=0 to FLists.Items.Count-1 do
    begin
    lItem:=FLists.Items[I];
    if lItem.Kind<>likText then
      Continue;
    if aY<lItem.Y then
      lDist:=lItem.Y-aY
    else if aY>=lItem.Y+lItem.Height then
      lDist:=aY-(lItem.Y+lItem.Height)+1
    else
      lDist:=0;
    if lDist<lBestDist then
      begin
      lBest:=I;
      lBestDist:=lDist;
      end;
    end;
  if lBest<0 then
    Exit;
  // Last item of that line starting at or before aX, else the first item of the line
  lLine:=FLists.Items[lBest].LineIndex;
  lBest:=-1;
  for I:=0 to FLists.Items.Count-1 do
    begin
    lItem:=FLists.Items[I];
    if (lItem.Kind<>likText) or (lItem.LineIndex<>lLine) then
      Continue;
    if (lBest<0) or (lItem.X<=aX) then
      lBest:=I;
    end;
  Result.LayoutItemIndex:=lBest;
  lItem:=FLists.Items[lBest];
  lRelX:=aX-lItem.X;
  if lRelX<=0 then
    Exit;
  lLen:=UTF8Length(lItem.Text);
  if lRelX>=lItem.Width then
    begin
    Result.CharOffset:=lLen;
    Exit;
    end;
  ApplyFont(fCanvas, lItem.FontSize, lItem.FontStyle, lItem.Context);
  lCurX:=0;
  lCharIndex:=0;
  while lCharIndex<lLen do
    begin
    lCharW:=fCanvas.TextWidth(UTF8Copy(lItem.Text, lCharIndex+1, 1));
    if lRelX<=lCurX+(lCharW div 2) then
      Break;
    Inc(lCurX, lCharW);
    Inc(lCharIndex);
    end;
  Result.CharOffset:=lCharIndex;
end;


// Check whether a single UTF-8 encoded character is part of a word.
function IsWordChar(const aChar: utf8string): Boolean;

var
  lChar: UnicodeString;

begin
  if aChar='_' then
    Exit(True);
  lChar:=UTF8ToUTF16(aChar);
  Result:=(lChar<>'') and (IsLetterOrDigit(lChar,1)
          or (GetUnicodeCategory(lChar,1) in [TUnicodeCategory.ucNonSpacingMark,TUnicodeCategory.ucCombiningMark]));
end;


// Index of the next text item of the same line in aDirection (1 or -1), or -1.
function TMarkdownCanvasRenderer.AdjacentTextItem(aIndex, aDirection: Integer): Integer;

var
  lLine: Integer;

begin
  Result:=-1;
  lLine:=FLists.Items[aIndex].LineIndex;
  Inc(aIndex,aDirection);
  while (aIndex>=0) and (aIndex<FLists.Items.Count) do
    case FLists.Items[aIndex].Kind of
      likText:
        begin
        if FLists.Items[aIndex].LineIndex=lLine then
          Result:=aIndex;
        Exit;
        end;
      likBackground:
        Inc(aIndex,aDirection);
    else
      Exit;
    end;
end;


function TMarkdownCanvasRenderer.GetWordBounds(const aPoint: TSelectionPoint; out aStart, aEnd: TSelectionPoint): Boolean;

var
  lIndex: Integer;
  lText: utf8string;

begin
  aStart:=aPoint;
  aEnd:=aPoint;
  if (aPoint.LayoutItemIndex<0) or (aPoint.LayoutItemIndex>=FLists.Items.Count) then
    Exit(False);
  repeat
    lText:=FLists.Items[aStart.LayoutItemIndex].Text;
    if aStart.CharOffset>0 then
      begin
      if not IsWordChar(UTF8Copy(lText,aStart.CharOffset,1)) then
        Break;
      Dec(aStart.CharOffset);
      end
    else
      begin
      lIndex:=AdjacentTextItem(aStart.LayoutItemIndex,-1);
      if lIndex<0 then
        Break;
      lText:=FLists.Items[lIndex].Text;
      if not IsWordChar(UTF8Copy(lText,UTF8Length(lText),1)) then
        Break;
      aStart.LayoutItemIndex:=lIndex;
      aStart.CharOffset:=UTF8Length(lText);
      end;
  until False;
  repeat
    lText:=FLists.Items[aEnd.LayoutItemIndex].Text;
    if aEnd.CharOffset<UTF8Length(lText) then
      begin
      if not IsWordChar(UTF8Copy(lText,aEnd.CharOffset+1,1)) then
        Break;
      Inc(aEnd.CharOffset);
      end
    else
      begin
      lIndex:=AdjacentTextItem(aEnd.LayoutItemIndex,1);
      if (lIndex<0) or not IsWordChar(UTF8Copy(FLists.Items[lIndex].Text,1,1)) then
        Break;
      aEnd.LayoutItemIndex:=lIndex;
      aEnd.CharOffset:=0;
      end;
  until False;
  Result:=(aStart.LayoutItemIndex<>aEnd.LayoutItemIndex) or (aStart.CharOffset<>aEnd.CharOffset);
end;


function TMarkdownCanvasRenderer.GetLineBounds(const aPoint: TSelectionPoint; out aStart, aEnd: TSelectionPoint): Boolean;

var
  I, lLine: Integer;

begin
  aStart:=aPoint;
  aEnd:=aPoint;
  if (aPoint.LayoutItemIndex<0) or (aPoint.LayoutItemIndex>=FLists.Items.Count) then
    Exit(False);
  lLine:=FLists.Items[aPoint.LayoutItemIndex].LineIndex;
  aStart.LayoutItemIndex:=-1;
  for I:=0 to FLists.Items.Count-1 do
    with FLists.Items[I] do
      if (Kind=likText) and (LineIndex=lLine) then
        begin
        if aStart.LayoutItemIndex<0 then
          aStart.LayoutItemIndex:=I;
        aEnd.LayoutItemIndex:=I;
        end;
  aStart.CharOffset:=0;
  aEnd.CharOffset:=UTF8Length(FLists.Items[aEnd.LayoutItemIndex].Text);
  Result:=True;
end;



function TMarkdownCanvasRenderer.MeasureBaseLineHeight(const aCanvas: TCanvas): LongInt;
begin
  Result:=MeasureTextHeight(aCanvas, fBaseFontSize, [], []);
end;


procedure TMarkdownCanvasRenderer.ClearImageCache;
begin
  FImageCache.Clear;
end;


procedure TMarkdownCanvasRenderer.SetBaseDir(const aValue: string);
begin
  if FBaseDir=aValue then
    Exit;
  FBaseDir:=aValue;
  ClearImageCache;
end;

procedure TMarkdownCanvasRenderer.SetSelection(const aStart, aEnd: TSelectionPoint);
begin
  ApplySelection(aStart, aEnd);
end;

procedure TMarkdownCanvasRenderer.ClearSelection;
begin
  ClearAllSelections;
end;

function TMarkdownCanvasRenderer.GetSelectedText: String;
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
      // Runs of one line already contain their separating spaces
      if (Result<>'') and (PrevItem<>nil) and (PrevItem.LineIndex<>Item.LineIndex)
         and not Item.SoftBreak then
        Result:=Result + LineEnding;
      Result:=Result + ItemText;
      PrevItem:=Item;
      end;
    end;
end;

procedure TMarkdownCanvasRenderer.ParseMarkdown(const aMarkdown: utf8string);
var
  S: TStrings;
begin
  S:=TStringList.Create;
  try
    S.Text:=aMarkdown;
    ParseMarkdown(S);
  finally
    S.Free;
  end;
end;

procedure TMarkdownCanvasRenderer.ParseMarkdown(const aMarkdown: TStrings);
var
  lDocument: TMarkdownDocument;
begin
  lDocument:=fParser.Parse(aMarkdown);
  FreeAndNil(fDocument);
  fDocument:=lDocument;
end;

procedure TMarkdownCanvasRenderer.RenderDocument(aDocument: TMarkdownDocument);
begin
  if not assigned(aDocument) then exit;
  fDocument:=aDocument;
  // Render all child blocks
  RenderChildren(aDocument);
end;

procedure TMarkdownCanvasRenderer.Clear;
begin
  FLists.Items.Clear;
  FLists.LinkRects.Clear;
  fCalculatedWidth:=0;
  fCalculatedHeight:=0;
  FLayout:=Default(TLayoutData);
  FLayout.BaselineFromIndex:=-1;
  fLineWidth:=0;
  FLineCount:=0;
  FLayout.NeedSpaceBeforeNextText:=False;
end;

procedure TMarkdownCanvasRenderer.BeginLayout;
begin
  Clear;
end;

procedure TMarkdownCanvasRenderer.EndLayout;
begin
  // Reserved for future batching hooks
end;

function TMarkdownCanvasRenderer.GetBulletChar(AIndex: Integer): string;
begin
  result:=FBullets[aIndex];
end;

function TMarkdownCanvasRenderer.DIP(const aValue: LongInt): LongInt;
begin
  Result:=(aValue * fTargetDPI + 48) div 96;
end;

function TMarkdownCanvasRenderer.EmPercent(const aPercent: Integer): LongInt;

begin
  Result:=(aPercent*FBaseLineHeight+50) div 100;
end;



procedure TMarkdownCanvasRenderer.NewLine(const aCanvas: TCanvas);
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
  FLayout.LinkFromIndex:=FLists.LinkRects.Count;
  FLayout.BaselineShiftCurrent:=0;
  FLayout.LineAscent:=0;
  FLayout.LineDescent:=0;
  Inc(FLineCount);
end;

procedure TMarkdownCanvasRenderer.ParagraphBreak(const aCanvas: TCanvas);
begin
  if FLayout.LineX<>0 then
    NewLine(aCanvas);
  FLayout.LineY:=FLayout.LineY + DIP(fParagraphSpacing);
  FLayout.LineAboveExtra:=0;
  FLayout.LineBelowExtra:=0;
end;

procedure TMarkdownCanvasRenderer.MaybeStartParagraph;
begin
  if FLayout.LineX<>0 then
   ParagraphBreak(FCanvas);
end;

function TMarkdownCanvasRenderer.SwapLayout(const aData: TLayoutData): TLayoutData;
begin
  Result:=FLayout;
  FLayout:=aData;
end;

function TMarkdownCanvasRenderer.SwapLayoutLists(const aData: TLayoutLists): TLayoutLists;
begin
  Result:=FLists;
  FLists:=aData;
end;

procedure TMarkdownCanvasRenderer.SetCurrentY(aValue: Integer);
begin
  FLayout.LineY:=aValue;
end;

procedure TMarkdownCanvasRenderer.ApplyFont(const aCanvas: TCanvas; const aFontSize: LongInt;
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


procedure TMarkdownCanvasRenderer.MeasureText(const aCanvas: TCanvas; const aText: utf8string;
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

function TMarkdownCanvasRenderer.MeasureTextWidth(const aCanvas: TCanvas; const aText: utf8string;
  const aFontSize: LongInt; const aFontStyle: TFontStyles; const aContext : TFontContext): LongInt;
var
  Width, Height: LongInt;
begin
  MeasureText(aCanvas, aText, aFontSize, aFontStyle, aContext, Width, Height);
  Result:=Width;
end;

function TMarkdownCanvasRenderer.MeasureTextHeight(const aCanvas: TCanvas; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext: TFontContext): LongInt;
var
  Width, Height: LongInt;
begin
  MeasureText(aCanvas, ExtendHeight, aFontSize, aFontStyle, aContext, Width, Height);
  Result:=Height;
end;


function TMarkdownCanvasRenderer.MeasureTextAscent(const aCanvas: TCanvas; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext: TFontContext): LongInt;

var
  I: Integer;
  lMono: Boolean;
  lTM: TLCLTextMetric;

begin
  lMono:=fcMono in aContext;
  for I:=0 to Length(FAscentCache)-1 do
    with FAscentCache[I] do
      if (Mono=lMono) and (Size=aFontSize) and (Style=aFontStyle) then
        Exit(Ascent);
  ApplyFont(aCanvas, aFontSize, aFontStyle, aContext);
  if aCanvas.GetTextMetrics(lTM) then
    Result:=lTM.Ascender
  else
    Result:=aCanvas.TextHeight(ExtendHeight);
  I:=Length(FAscentCache);
  SetLength(FAscentCache, I+1);
  FAscentCache[I].Mono:=lMono;
  FAscentCache[I].Size:=aFontSize;
  FAscentCache[I].Style:=aFontStyle;
  FAscentCache[I].Ascent:=Result;
end;


procedure TMarkdownCanvasRenderer.ShiftLineItems(aDelta: LongInt);

var
  I: Integer;

begin
  for I:=FLayout.BaselineFromIndex+1 to FLists.Items.Count-1 do
    with FLists.Items[I] do
      if Kind in [likText,likImage] then
        Y:=Y+aDelta;
  for I:=FLayout.LinkFromIndex to FLists.LinkRects.Count-1 do
    with FLists.LinkRects[I] do
      FY:=FY+aDelta;
end;

function TMarkdownCanvasRenderer.GetTextColor(aContext : TFontContext) : TColor;

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

function TMarkdownCanvasRenderer.GetTextBGColor(aContext : TFontContext) : TColor;

begin
  if fcFencedCode in aContext then
    Result:=BGCodeColor
  else
    Result:=BGColor;
end;

procedure TMarkdownCanvasRenderer.FlushTextRun(const aCanvas: TCanvas; const aText: utf8string;
  const aFontSize: LongInt; const aFontStyle: TFontStyles; const aLinkHref: utf8string;
  const aContext : TFontContext; const aDryRun: boolean);
var
  Item: TLayoutItem;
  lX,lY : Longint;
  lNewWidth, lHeight, lWidth, lAscent, lDescent: LongInt;
begin
  if length(aText)=0 then
    exit;

  lWidth:=MeasureTextWidth(aCanvas, aText, aFontSize, aFontStyle, aContext);
  lHeight:=MeasureTextHeight(aCanvas, aFontSize, aFontStyle, aContext);
  lAscent:=MeasureTextAscent(aCanvas, aFontSize, aFontStyle, aContext);
  lDescent:=Max(0, lHeight-lAscent);
  // All runs of a line share the baseline at LineAscent below the line top
  if lAscent>FLayout.LineAscent then
    begin
    if FLayout.LineAscent>0 then
      ShiftLineItems(lAscent-FLayout.LineAscent);
    FLayout.LineAscent:=lAscent;
    end;
  if lDescent>FLayout.LineDescent then
    FLayout.LineDescent:=lDescent;
  if FLayout.LineHeight<FLayout.LineAscent+FLayout.LineDescent then
    FLayout.LineHeight:=FLayout.LineAscent+FLayout.LineDescent;

  if not aDryRun then
    begin
    lX:=FLayout.CurrentIndent + FLayout.LineX;
    lY:=FLayout.LineY + FLayout.LineAboveExtra + FLayout.BaselineShiftCurrent + (FLayout.LineAscent-lAscent);
    Item:=FLists.Items.NewItem(TLayoutItemKind.likText,lX,lY);
    Item.LineIndex:=FLineCount;
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

procedure TMarkdownCanvasRenderer.LayoutTextWrapped(const aCanvas: TCanvas; const aText: utf8string;
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

  procedure BreakLongWord(const aWord: utf8string);
  begin
    LayoutCharWrapped(aCanvas, aWord, aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
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
      if FLayout.LineX>0 then
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

// Lay out aText, breaking it between characters where it exceeds the available width.
procedure TMarkdownCanvasRenderer.LayoutCharWrapped(const aCanvas: TCanvas; const aText: utf8string;
  const aFontSize: LongInt; const aFontStyle: TFontStyles; const aLinkHref: utf8string;
  const aContext: TFontContext; const aDryRun: boolean);

var
  lPos, lLen, lStart, lCharLen: Integer;
  lRunWidth, lCharWidth: LongInt;
  lAfterWrap: Boolean;

  procedure FlushRun;
  begin
    if lPos>lStart then
      begin
      FlushTextRun(aCanvas, Copy(aText,lStart,lPos-lStart), aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
      if lAfterWrap and not aDryRun then
        FLists.Items[FLists.Items.Count-1].SoftBreak:=True;
      end;
    lAfterWrap:=False;
  end;

begin
  lLen:=Length(aText);
  lPos:=1;
  lStart:=1;
  lRunWidth:=0;
  lAfterWrap:=False;
  while lPos<=lLen do
    begin
    lCharLen:=UTF8CodepointSize(@aText[lPos]);
    if (lCharLen<1) or (lPos+lCharLen>lLen+1) then
      lCharLen:=1;
    lCharWidth:=MeasureTextWidth(aCanvas, Copy(aText,lPos,lCharLen), aFontSize, aFontStyle, aContext);
    if (FLayout.CurrentIndent+FLayout.LineX+lRunWidth+lCharWidth>FLayout.MaxWidth)
       and ((lPos>lStart) or (FLayout.LineX>0)) then
      begin
      FlushRun;
      NewLine(aCanvas);
      lAfterWrap:=lPos>1;
      lStart:=lPos;
      lRunWidth:=0;
      end;
    Inc(lRunWidth,lCharWidth);
    Inc(lPos,lCharLen);
    end;
  FlushRun;
end;


procedure TMarkdownCanvasRenderer.CalculateLayout(const aCanvas: TCanvas; const aMaxWidth: LongInt;
  out aContentWidth: LongInt; out aContentHeight: LongInt);

var
  I: Integer;

begin
  if not assigned(aCanvas) then
    begin
    aContentWidth:=0;
    aContentHeight:=0;
    exit;
    end;
  fCanvas:=aCanvas;
  SetLength(FAscentCache,0);
  if FBlockQuoteIndent<>0 then
    FCalculatedBlockQuoteIndent:=FBlockQuoteIndent
  else
    FCalculatedBlockQuoteIndent:=MeasureTextWidth(FCanvas,'WWWW',fBaseFontSize,[],[]);
  FBaseLineHeight:=MeasureBaseLineHeight(FCanvas);
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
    // Code backgrounds cover lines that are wider than the available width
    for I:=0 to FLists.Items.Count-1 do
      with FLists.Items[I] do
        if (Kind=likBackground) and (X+Width<fCalculatedWidth) then
          Width:=fCalculatedWidth-X;
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

procedure TMarkdownCanvasRenderer.DrawLayoutItem(aCanvas : TCanvas; aItem : TLayoutItem;const aLeftPosition, aTopPosition: LongInt);
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
     if aItem.IsSelected or (aItem.BGColor=clNone) then
       aCanvas.Brush.Style:=bsClear
     else
       begin
       aCanvas.Brush.Style:=bsSolid;
       aCanvas.Brush.Color:=aItem.BGColor;
       end;
     with aItem do
       begin
       ApplyFont(aCanvas, FontSize, FontStyle, Context);
       aCanvas.Font.Color:=aItem.GetColor(ckForeground,GetTextColor(aItem.Context));
       aCanvas.TextOut(lX,lY, Text);
       end;
     end;
   TLayoutItemKind.likRect:
     begin
     aCanvas.Pen.Style:=psSolid;
     aCanvas.Pen.Color:=aItem.GetColor(ckForeground,TableLinesColor);
     aCanvas.Pen.Width:=1;
     aCanvas.Brush.Style:=bsSolid;
     aCanvas.Brush.Color:=aItem.GetColor(ckBackGround,BGColor);
     aCanvas.Rectangle(lX,LY,lX+aItem.Width,lY+aItem.Height);
     end;
   TLayoutItemKind.likBackground:
     begin
     aCanvas.Pen.Style:=psSolid;
     aCanvas.Pen.Color:=aItem.GetColor(ckBackground,BGCodeColor);
     aCanvas.Pen.Width:=1;
     aCanvas.Brush.Style:=bsSolid;
     aCanvas.Brush.Color:=aItem.GetColor(ckBackGround,BGCodeColor);
     aCanvas.Rectangle(lX,LY,lX+aItem.Width,lY+aItem.Height);
     end;
   TLayoutItemKind.likLine:
     begin
     aCanvas.Pen.Color:=aItem.GetColor(ckForeground,TableLinesColor);
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
   TLayoutItemKind.likHR: ;
 end;
end;

procedure TMarkdownCanvasRenderer.DrawLayout(const aCanvas: TCanvas; const aLeftPosition, aTopPosition: LongInt);
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

function TMarkdownCanvasRenderer.HitTestLink(const aX, aY: LongInt; out aHref: utf8string): boolean;
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

function TMarkdownCanvasRenderer.CreateLayoutItem(aKind: TLayoutItemKind; aX, aY: Longint): TLayoutItem;
begin
  Result:=FLists.Items.NewItem(aKind,aX,aY);
  Result.LineIndex:=FLineCount;
end;

function TMarkdownCanvasRenderer.LayoutImage(aImageURL : UTF8String) : Boolean;
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
      FOnGetImage(Self,aImageURL,lImage);
    if not assigned(lImage) then
      lImage:=LoadLocalImage(aImageURL);
    if assigned(lImage) then
      FImageCache.Add(aImageURL,lImage);
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

function TMarkdownCanvasRenderer.LoadLocalImage(const aURL: string): TPicture;

  function PercentDecode(const aValue: string): string;
  var
    I, lLen, lCode: Integer;
  begin
    Result:='';
    I:=1;
    lLen:=Length(aValue);
    while I<=lLen do
      begin
      if (aValue[I]='%') and (I+2<=lLen) and TryStrToInt('$'+Copy(aValue,I+1,2),lCode) then
        begin
        Result:=Result+Chr(lCode);
        Inc(I,3);
        end
      else
        begin
        Result:=Result+aValue[I];
        Inc(I);
        end;
      end;
  end;

var
  lName: string;

begin
  Result:=nil;
  lName:=aURL;
  if SameText(Copy(lName,1,7),'file://') then
    begin
    Delete(lName,1,7);
    // file:///C:/dir/name
    if (Length(lName)>=3) and (lName[1]='/') and (lName[3]=':') then
      Delete(lName,1,1);
    end
  else if Pos('://',lName)>0 then
    Exit;
  lName:=SetDirSeparators(PercentDecode(lName));
  if lName='' then
    Exit;
  if not FilenameIsAbsolute(lName) and (FBaseDir<>'') then
    lName:=CreateAbsolutePath(lName,FBaseDir);
  if not FileExists(lName) then
    Exit;
  Result:=TPicture.Create;
  try
    Result.LoadFromFile(lName);
  except
    FreeAndNil(Result);
  end;
end;


procedure TMarkdownCanvasRenderer.CollectHTMLEntities;
var
  Ent : THTMLEntityDef;
begin
  for ent in EntityDefList do
    FHTMLEntities.Add(ent.e,Utf8Encode(ent.u));
end;

function TMarkdownCanvasRenderer.ResolveEntities(const aText: String): String;


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

procedure TMarkdownCanvasRenderer.RenderTextNode(aTextNode: TMarkdownTextNode; aFontSize: LongInt;
  aFontStyle: TFontStyles; const aContext : TFontContext; const aLinkHref: utf8string);

var
  lItem : TLayoutItem;
  fontStyle: TFontStyles;
  lText,linkHref: utf8string;
  lHref,lClass: string;
  lContext : TFontContext;
  Alt : string;
  lExists : Boolean;
  lChild : TMarkdownTextNode;

  procedure LayoutLink(const aHref, aDefaultText: utf8string);
  begin
    linkHref:=aHref;
    fontStyle:=fontStyle+[fsUnderline];
    lContext:=lContext+[fcHyperLink];
    if (lText='') and not aTextNode.HasChildren then
      lText:=aDefaultText;
    LayoutTextWrapped(fCanvas, lText, aFontSize, fontStyle, linkHref, lContext, False, False);
  end;

begin
  if not assigned(aTextNode) then exit;
  lContext:=aContext;
  fontStyle:=aFontStyle + GetNodeFontStyle(aTextNode);
  lText:=ResolveEntities(aTextNode.NodeText);
  linkHref:=aLinkHref;
  if aTextNode.Kind = nkCode then
    lContext:=lContext+[fcMono,fcCode];
  case aTextNode.Kind of
    nkCode,
    nkText,
    nkNamed:
      begin
      if fcFencedCode in lContext then
        begin
        lItem:=FLists.Items.NewItem(likBackground,FLayout.LineX,FLayout.LineY);
        lItem.Width:=FLayout.MaxWidth-LItem.X;
        if FWrapCodeBlocks then
          LayoutCharWrapped(fCanvas, lText, aFontSize, fontStyle, '', lContext, False)
        else
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
      LayoutLink(aTextNode.Attrs['href'], aTextNode.Attrs['href']);
    nkWikiLink:
      begin
      DoResolveWikiLink(aTextNode.Attrs['target'], aTextNode.Attrs['fragment'], lHref, lClass, lExists);
      LayoutLink(lHref, aTextNode.Attrs['target']);
      end;
    nkFootnoteRef:
      begin
      lText:=aTextNode.Attrs['number'];
      if lText='' then
        lText:=aTextNode.Attrs['label'];
      LayoutTextWrapped(fCanvas, '['+lText+']', aFontSize, fontStyle, linkHref, lContext, False, False);
      end;
    nkDirective:
      LayoutTextWrapped(fCanvas, lText, aFontSize, fontStyle, linkHref, lContext, False, False);
    nkRaw:
      if SameText(Copy(lText,1,3),'<br') then
        NewLine(fCanvas);
    nkComment: ;
    nkImg:
      if Not LayoutImage(aTextNode.Attrs['src']) then
        begin
        Alt:=ResolveEntities(aTextNode.Attrs['alt']);
        if Alt='' then
          Alt:='img';
        LayoutTextWrapped(fCanvas, '['+Alt+']', aFontSize, fontStyle, '', lContext+[fcQuote], False, False);
        end;
  end;
  if aTextNode.HasChildren and not (aTextNode.Kind in [nkImg,nkComment,nkRaw]) then
    for lChild in aTextNode.Children do
      RenderTextNode(lChild, aFontSize, fontStyle, lContext, linkHref);
end;

function TMarkdownCanvasRenderer.GetNodeFontStyle(aTextNode: TMarkdownTextNode): TFontStyles;
begin
  Result:=[];
  if nsStrong in aTextNode.Styles then
    Include(Result, fsBold);
  if nsEmph in aTextNode.Styles then
    Include(Result, fsItalic);
  if nsDelete in aTextNode.Styles then
    Include(Result, fsStrikeOut);
end;

procedure TMarkdownCanvasRenderer.Indent(aSize: integer);
begin
  inc(aSize,fExtraIndent);
  if FLayout.LineX>0 then
    FNextLineIndent:=FNextLineIndent+aSize
  else
    FLayout.CurrentIndent:=FLayout.CurrentIndent+aSize;
end;

procedure TMarkdownCanvasRenderer.SetBulletChar(AIndex: Integer; AValue: string);
begin
  FBullets[aIndex]:=aValue;
end;

procedure TMarkdownCanvasRenderer.Undent(aSize: integer);
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

procedure TMarkdownCanvasRenderer.IncBulletLevel;
begin
  Inc(FBulletLevel);
end;

procedure TMarkdownCanvasRenderer.DecBulletLevel;
begin
  Dec(FBulletLevel);
  if FBulletLevel<0 then
    FBulletLevel:=0;
end;

function TMarkdownCanvasRenderer.GetBullet: String;
begin
  Result:=GetBulletAt(BulletLevel)
end;

function TMarkdownCanvasRenderer.GetBulletAt(aLevel: integer): string;
begin
  if aLevel<1 then
    aLevel:=1;
  Result:=FBullets[((aLevel-1) mod BulletCount)+1];
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


{ TCanvasMarkdownBlockRenderer }

function TCanvasMarkdownBlockRenderer.GetCanvas: TCanvas;
begin
  Result:=CanvasRenderer.FCanvas;
end;

function TCanvasMarkdownBlockRenderer.GetCanvasRenderer: TMarkdownCanvasRenderer;
begin
  if Renderer is TMarkdownCanvasRenderer then
    Result:=TMarkdownCanvasRenderer(Renderer)
  else
    Result:=nil;
end;

function TCanvasMarkdownBlockRenderer.DIP(aSize: integer): Integer;
begin
  Result:=CanvasRenderer.DIP(aSize);
end;

procedure TCanvasMarkdownBlockRenderer.Indent(aSize: Integer);
begin
  CanvasRenderer.Indent(aSize);
end;

procedure TCanvasMarkdownBlockRenderer.Undent(aSize: Integer);
begin
 CanvasRenderer.Undent(aSize);
end;

function TCanvasMarkdownBlockRenderer.LayoutData: TLayoutData;
begin
  Result:=CanvasRenderer.Layout;
end;

function TCanvasMarkdownBlockRenderer.CreateLayoutItem(aKind: TLayoutItemKind; aX, aY: Longint): TLayoutItem;
begin
  Result:=CanvasRenderer.CreateLayoutItem(aKind, aX, aY);
end;

function TCanvasMarkdownBlockRenderer.MeasureTextHeight(const aFontSize: LongInt; const aFontStyle: TFontStyles;
  const aContext: TFontContext): LongInt;
begin
  Result:=CanvasRenderer.MeasureTextHeight(CanvasRenderer.FCanvas,aFontSize,aFontStyle,aContext);
end;

function TCanvasMarkdownBlockRenderer.MeasureTextWidth(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; aFontContext : TFontContext): LongInt;
begin
  Result:=CanvasRenderer.MeasureTextWidth(CanvasRenderer.FCanvas,aText,aFontSize,aFontStyle,aFontContext);
end;

procedure TCanvasMarkdownBlockRenderer.LayoutText(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext: TFontContext);
begin
  CanvasRenderer.LayoutTextWrapped(CanvasRenderer.fCanvas, aText, aFontSize, aFontStyle, aLinkHref,aContext, False, False);
end;

procedure TCanvasMarkdownBlockRenderer.SetCurrentY(aValue: Integer);
begin
  CanvasRenderer.SetCurrentY(aValue);
end;

function TCanvasMarkdownBlockRenderer.GetCurrentY: Integer;
begin
  Result:=CanvasRenderer.FLayout.Liney;
end;

procedure TCanvasMarkdownBlockRenderer.NewLine;
begin
  CanvasRenderer.NewLine(CanvasRenderer.fCanvas);
end;

procedure TCanvasMarkdownBlockRenderer.ParagraphBreak;
begin
  CanvasRenderer.ParagraphBreak(CanvasRenderer.fCanvas);
end;

// True when aBlock is a direct child of an item of a tight list.
function InTightListItem(aBlock : TMarkdownBlock) : Boolean;

var
  lItem : TMarkdownBlock;

begin
  Result:=False;
  lItem:=aBlock.Parent;
  if not (lItem is TMarkdownListItemBlock) then
    exit;
  Result:=(lItem.Parent is TMarkdownListBlock) and not TMarkdownListBlock(lItem.Parent).Loose;
end;


procedure TCanvasMarkdownBlockRenderer.EndBlock(aBlock : TMarkdownBlock);

begin
  if not InTightListItem(aBlock) then
    ParagraphBreak
  else if LayoutData.LineX<>0 then
    NewLine;
end;


procedure TCanvasMarkdownBlockRenderer.MaybeStartParagraph;
begin
  CanvasRenderer.MaybeStartParagraph;
end;

{ TCanvasMarkdownTextRenderer }

function TCanvasMarkdownTextRenderer.GetCanvasRenderer: TMarkdownCanvasRenderer;
begin
  if Renderer is TMarkdownCanvasRenderer then
    Result:=TMarkdownCanvasRenderer(Renderer)
  else
    Result:=nil;
end;

procedure TCanvasMarkdownTextRenderer.DoRender(aElement: TMarkdownTextNode);
begin
  if not assigned(aElement) or not assigned(CanvasRenderer) then exit;
  CanvasRenderer.RenderTextNode(aElement, CanvasRenderer.fBaseFontSize, [], []);
end;

{ TCanvasMarkdownParagraphBlockRenderer }

procedure TCanvasMarkdownParagraphBlockRenderer.DoRender(aBlock: TMarkdownBlock);
begin
  if not assigned(aBlock) then exit;

  // Render paragraph children and add paragraph break
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  EndBlock(aBlock);
end;

class function TCanvasMarkdownParagraphBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownParagraphBlock;
end;

{ TCanvasMarkdownHeadingBlockRenderer }

procedure TCanvasMarkdownHeadingBlockRenderer.DoRender(aBlock: TMarkdownBlock);
const
  FontSizeCount = 5;
  FontSizes : Array[1..FontSizeCount] of Integer = (10,8,6,4,2);
var
  lHeading: TMarkdownHeadingBlock;

begin
  if not assigned(aBlock) then exit;
  lHeading:=aBlock as TMarkdownHeadingBlock;
  FontSize:=CanvasRenderer.fBaseFontSize;
  if (lHeading.Level in [1..FontSizeCount]) then
    Inc(FontSize,FontSizes[lHeading.Level]);
  if GetCurrentY>0 then
    SetCurrentY(GetCurrentY+CanvasRenderer.EmPercent(CanvasRenderer.HeadingSpacing));
  Renderer.RenderChildren(lHeading);
  ParagraphBreak;
end;

class function TCanvasMarkdownHeadingBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownHeadingBlock;
end;

{ TCanvasMarkdownQuoteBlockRenderer }

procedure TCanvasMarkdownQuoteBlockRenderer.DoRender(aBlock: TMarkdownBlock);
var
  lIndent : Integer;
begin
  if not assigned(aBlock) then exit;
  lIndent:=CanvasRenderer.CalculatedBlockQuoteIndent;
  CanvasRenderer.Indent(lIndent);
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  CanvasRenderer.Undent(lindent);
  ParagraphBreak;
end;

class function TCanvasMarkdownQuoteBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownQuoteBlock;
end;

{ TCanvasMarkdownListBlockRenderer }

procedure TCanvasMarkdownListBlockRenderer.DoRender(aBlock: TMarkdownBlock);
var
  lBlock : TMarkdownListBlock absolute aBlock;
  i : Integer;
begin
  if not assigned(aBlock) then exit;
  if not lBlock.Ordered then
    CanvasRenderer.IncBulletLevel;
  // Render all list items
  FItemNumber:=0;
  FOrdered:=lBlock.Ordered;
  For I:=0 to aBlock.ChildCount-1 do
    begin
    if I>0 then
      SetCurrentY(GetCurrentY+CanvasRenderer.EmPercent(CanvasRenderer.ListItemSpacing));
    if lBlock.Ordered then
      FItemNumber:=lBlock.Start+i;
    Renderer.RenderBlock(aBlock.Children[i]);
    end;
  if not lBlock.Ordered then
    CanvasRenderer.DecBulletLevel;
  EndBlock(aBlock);
end;

procedure TCanvasMarkdownListBlockRenderer.reset;
begin
  inherited reset;
  FItemNumber:=0;
  FOrdered:=False;
end;

class function TCanvasMarkdownListBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownListBlock;
end;

{ TCanvasMarkdownListItemBlockRenderer }

procedure TCanvasMarkdownListItemBlockRenderer.DoRender(aBlock: TMarkdownBlock);

var
  lBlock : TMarkdownListItemBlock absolute aBlock;
  lListRenderer : TCanvasMarkdownListBlockRenderer;
  lPrefix : string;
  lIndent : Integer;

begin
  if not assigned(aBlock) then exit;
  // Add bullet point and render children
  lListRenderer:=TCanvasMarkdownListBlockRenderer(GetFirstParentWithClass(TCanvasMarkdownListBlockRenderer));
  if Assigned(lListRenderer) and lListRenderer.Ordered then
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

class function TCanvasMarkdownListItemBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownListItemBlock;
end;

{ TCanvasMarkdownCodeBlockRenderer }

procedure TCanvasMarkdownCodeBlockRenderer.DoRender(aBlock: TMarkdownBlock);

var
  lPad : Integer;
  lBack : TLayoutItem;

begin
  if not assigned(aBlock) then exit;
  ParagraphBreak;
  lPad:=CanvasRenderer.EmPercent(CanvasRenderer.CodeBlockPadding);
  lBack:=Nil;
  if lPad>0 then
    begin
    // Background of the whole block, padding included
    lBack:=CreateLayoutItem(likBackground,0,GetCurrentY);
    lBack.Width:=LayoutData.MaxWidth;
    SetCurrentY(GetCurrentY+lPad);
    Indent(lPad);
    Dec(CanvasRenderer.FLayout.MaxWidth,lPad);
    end;
  // Render code with monospace font
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  if Assigned(lBack) then
    begin
    Inc(CanvasRenderer.FLayout.MaxWidth,lPad);
    Undent(lPad);
    SetCurrentY(GetCurrentY+lPad);
    lBack.Height:=GetCurrentY-lBack.Y;
    end;
  ParagraphBreak;
end;

class function TCanvasMarkdownCodeBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownCodeBlock;
end;

{ TCanvasMarkdownTableBlockRenderer }
procedure TCanvasMarkdownTableBlockRenderer.MeasureCell(aCell: TMarkdownBlock; aHeader: Boolean;
  var aMin, aPref: LongInt);

var
  lCont : TMarkdownContainerBlock absolute aCell;
  lText : TMarkdownTextBlock absolute aCell;
  I : Integer;
  lWord, lLine : LongInt;

  procedure EndWord;
  begin
    if lWord>aMin then
      aMin:=lWord;
    lWord:=0;
  end;

  procedure EndLine;
  begin
    EndWord;
    if lLine>aPref then
      aPref:=lLine;
    lLine:=0;
  end;

  procedure MeasureNode(aNode: TMarkdownTextNode; aStyle: TFontStyles);
  var
    lPos, lStart : Integer;
    lStyle : TFontStyles;
    lContext : TFontContext;
    lStr : string;
    lChild : TMarkdownTextNode;
  begin
    if aNode.Kind=nkLineBreak then
      begin
      EndLine;
      Exit;
      end;
    if aNode.Kind in [nkImg,nkComment,nkRaw] then
      Exit;
    lStyle:=aStyle+CanvasRenderer.GetNodeFontStyle(aNode);
    if aNode.Kind=nkCode then
      lContext:=[fcMono,fcCode]
    else
      lContext:=[];
    lStr:=CanvasRenderer.ResolveEntities(aNode.NodeText);
    Inc(lLine,MeasureTextWidth(lStr,CanvasRenderer.BaseFontSize,lStyle,lContext));
    // A word may continue in the next node
    lStart:=1;
    for lPos:=1 to Length(lStr)+1 do
      if (lPos>Length(lStr)) or (lStr[lPos] in [' ',#9,#10,#13]) then
        begin
        if lPos>lStart then
          Inc(lWord,MeasureTextWidth(Copy(lStr,lStart,lPos-lStart),CanvasRenderer.BaseFontSize,lStyle,lContext));
        if lPos<=Length(lStr) then
          EndWord;
        lStart:=lPos+1;
        end;
    if aNode.HasChildren then
      for lChild in aNode.Children do
        MeasureNode(lChild,lStyle);
  end;

begin
  if aCell is TMarkdownTextBlock then
    begin
    lWord:=0;
    lLine:=0;
    for I:=0 to lText.Nodes.Count-1 do
      if aHeader then
        MeasureNode(lText.Nodes[I],[fsBold])
      else
        MeasureNode(lText.Nodes[I],[]);
    EndLine;
    end
  else if aCell is TMarkdownContainerBlock then
    for I:=0 to lCont.ChildCount-1 do
      MeasureCell(lCont[I],aHeader,aMin,aPref);
end;


function TCanvasMarkdownTableBlockRenderer.GetTotalColumns: Integer;
begin
  Result:=Length(FColLayout);
end;

function TCanvasMarkdownTableBlockRenderer.GetTotalHeight: Integer;
var
  I : Integer;
begin
  Result:=0;
  For I:=0 to Length(FRowHeights)-1 do
    Result:=Result+FRowHeights[i];
  if Length(FRowHeights)>1 then
    Result:=Result+(Length(FRowHeights)-1)*FGridSize;
end;

// Width of the columns and the grid lines between them.
function TCanvasMarkdownTableBlockRenderer.GetTotalWidth: Integer;

var
  I : Integer;

begin
  Result:=0;
  For I:=0 to Length(FColLayout)-1 do
    Result:=Result+FColLayout[I].Width;
  if Length(FColLayout)>1 then
    Result:=Result+(Length(FColLayout)-1)*FGridSize;
end;

procedure TCanvasMarkdownTableBlockRenderer.CalcRowHeights(aTable: TMarkdownTableBlock; const aFontSize: LongInt; const aFontStyle: TFontStyles; const aContext : TFontContext);

var
  lRow,lColumn:LongInt;
  CellWidth,TextWidth:LongInt;
  Saved, Temp: TLayoutData;
  SavedItems, TempItems : TLayoutLists;
  Cell:TMarkdownBlock;

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
     TextWidth:=CellWidth-(FCellPaddingX*2);
     if TextWidth<1 then
      TextWidth:=1;

     Temp:=Default(TLayoutData);
     Temp.MaxWidth:=TextWidth;
     Temp.BaselineFromIndex:=-1;
     Saved:=CanvasRenderer.SwapLayout(Temp);
      // dry-run traversal inside a temporary items/link list to measure height only
      TempItems:=TLayoutLists.Create;
      SavedItems:=CanvasRenderer.SwapLayoutLists(TempItems);
      FCurrentRow:=lRow;
      try
         CanvasRenderer.RenderBlock(Cell);
      finally
        FCurrentRow:=-1;
        CanvasRenderer.SwapLayoutLists(SavedItems);
        TempItems.Free;
      end;
      Temp:=CanvasRenderer.SwapLayout(Saved);
      if (Temp.LineY+Temp.LineHeight)>FRowHeights[lRow] then
       FRowHeights[lRow]:=Temp.LineY+Temp.LineHeight;
      end;
    if FRowHeights[lRow]<MeasureTextHeight(aFontSize,aFontStyle,aContext) then
      FRowHeights[lRow]:=MeasureTextHeight(aFontSize,aFontStyle,aContext);
    FRowHeights[lRow]:=FRowHeights[lRow]+(FCellPaddingY*2);
    end;
end;

procedure TCanvasMarkdownTableBlockRenderer.MeasureTableColumns(const aTableModel:TMarkdownTableBlock);

var
  lColumn,lRow :LongInt;
  lMin,lPref:LongInt;
  lRowBlock : TMarkdownTableRowBlock;

begin
  For lColumn:=0 to TotalColumns-1 do
    begin
    FColLayout[lColumn].Min:=DIP(8);
    FColLayout[lColumn].Pref:=DIP(16);
    end;
  For lRow:=0 to aTableModel.ChildCount-1 do
    begin
    lRowBlock:=aTableModel.Children[lRow] as TMarkdownTableRowBlock;
    For lColumn:=0 to Min(lRowBlock.ChildCount-1, TotalColumns-1) do
      begin
      lMin:=0;
      lPref:=0;
      MeasureCell(lRowBlock.Children[lColumn],lRow=0,lMin,lPref);
      With FColLayout[lColumn] do
        begin
        if lMin+2*FCellPaddingX>Min then
          Min:=lMin+2*FCellPaddingX;
        if lPref+2*FCellPaddingX>Pref then
          Pref:=lPref+2*FCellPaddingX;
        end;
      end;
    end;
end;

procedure TCanvasMarkdownTableBlockRenderer.DistributeColumns(const aAvailable:LongInt);

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
    // Less room than minimum: shrink proportionally, long words get broken.
    For I:=0 to TotalColumns-1 do
      With FColLayout[i] do
        if TotalMin>0 then
          Width:=Math.Max(DIP(8),(Min*aAvailable) div TotalMin)
        else
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

procedure TCanvasMarkdownTableBlockRenderer.DrawBorders;

var
  I, ColX : Integer;
  Itm : TLayoutItem;
begin
  Itm:=CreateLayoutItem(likRect,FStartX,FStartY);
  Itm.Width:=TotalWidth;
  Itm.Height:=TotalHeight;
  ColX:=FStartX;
  For I:=0 to Length(FColLayout)-2 do
    begin
    ColX:=ColX+FColLayout[i].Width;
    Itm:=CreateLayoutItem(likLine,ColX,FStartY);
    Itm.DeltaX:=0;
    Itm.DeltaY:=TotalHeight;
    ColX:=ColX+FGridSize;
    end;
end;

procedure TCanvasMarkdownTableBlockRenderer.DoRender(aBlock: TMarkdownBlock);
var
  lTable : TMarkdownTableBlock absolute aBlock;
  I : Integer;

begin
  if not assigned(aBlock) then exit;
  CanvasRenderer.MaybeStartParagraph;
  SetLength(FColLayout,Length(lTable.Columns));
  FGridSize:=DIP(1);
  FCellPaddingX:=CanvasRenderer.EmPercent(CanvasRenderer.TableCellPaddingX);
  FCellPaddingY:=CanvasRenderer.EmPercent(CanvasRenderer.TableCellPaddingY);
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

procedure TCanvasMarkdownTableBlockRenderer.Reset;
begin
  inherited Reset;
  FCurrentRow:=-1;
  SetLength(FColLayout,0);
end;


class function TCanvasMarkdownTableBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableBlock;
end;

{ TCanvasMarkdownTableRowBlockRenderer }

procedure TCanvasMarkdownTableRowBlockRenderer.GetTableRenderer;

var
  A: TMarkdownElementRendererArray;
  I : integer;

begin
  A:=GetParentRenderers;
  FTableRenderer:=nil;
  I:=Length(A)-1;
  While (I>=0) and Not Assigned(FTableRenderer) do
    begin
    if A[I] is TCanvasMarkdownTableBlockRenderer then
      FTableRenderer:=TCanvasMarkdownTableBlockRenderer(A[i]);
    Dec(i);
    end;
  if FTableRenderer=nil then
    Raise EMarkdown.Create('Could not find markdown table renderer');
end;

procedure TCanvasMarkdownTableRowBlockRenderer.DoRender(aBlock: TMarkdownBlock);

var
  lColumn:LongInt;
  CellWidth,CurrentY,CurrentX:LongInt;
  Saved, Temp: TLayoutData;
  Cell:TMarkdownBlock;
  Itm:TLayoutItem;

begin
  if not assigned(aBlock) then
    exit;
  GetTableRenderer;
  CurrentY:=TableRenderer.FStartY;
  CurrentX:=TableRenderer.FStartX;
  if FTableRenderer.CurrentRow>0 then
    begin
    Itm:=CreateLayoutItem(likLine,CurrentX,CurrentY-FTableRenderer.GridSize);
    Itm.DeltaX:=FTableRenderer.TotalWidth;
    end;
  For lColumn:=0 to Min(aBlock.ChildCount-1, Length(FTableRenderer.ColLayout)-1) do
    begin
    Cell:=aBlock.Children[lColumn];
    CellWidth:=FTableRenderer.ColLayout[lColumn].Width;
    Temp:=Default(TLayoutData);
    Temp.MaxWidth:=CurrentX+CellWidth-FTableRenderer.CellPaddingX;
    Temp.CurrentIndent:=CurrentX+FTableRenderer.CellPaddingX;
    Temp.LineY:=CurrentY+FTableRenderer.CellPaddingY;
    Temp.BaselineFromIndex:=CanvasRenderer.FLists.Items.Count-1;
    Temp.LinkFromIndex:=CanvasRenderer.FLists.LinkRects.Count;
    Inc(CanvasRenderer.FLineCount);
    Saved:=CanvasRenderer.SwapLayout(Temp);
    CanvasRenderer.RenderBlock(Cell);
    CanvasRenderer.SwapLayout(Saved);
    CurrentX:=CurrentX+FTableRenderer.FColLayout[lColumn].Width;
    CurrentX:=CurrentX+FTableRenderer.FGridSize;
    end;
end;

class function TCanvasMarkdownTableRowBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTableRowBlock;
end;

{ TCanvasMarkdownThematicBreakBlockRenderer }

procedure TCanvasMarkdownThematicBreakBlockRenderer.DoRender(aBlock: TMarkdownBlock);
var
  lItem: TLayoutItem;
  lData: TLayoutData;
  lHeight: Integer;
begin
  if not assigned(aBlock) then exit;
  MaybeStartParagraph;
  lData:=LayoutData;
  lHeight:=MeasureTextHeight(CanvasRenderer.fBaseFontSize, [], []);
  // Line across the available width, vertically centered in one text line
  lItem:=CreateLayoutItem(likLine, lData.CurrentIndent, lData.LineY+(lHeight div 2));
  lItem.DeltaX:=Max(1, lData.MaxWidth-lData.CurrentIndent);
  lItem.DeltaY:=0;
  lItem.Width:=lItem.DeltaX;
  SetCurrentY(lData.LineY+lHeight);
  ParagraphBreak;
end;

class function TCanvasMarkdownThematicBreakBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownThematicBreakBlock;
end;

{ TCanvasMarkdownTextBlockRenderer }

procedure TCanvasMarkdownTextBlockRenderer.DoRender(aBlock: TMarkdownBlock);
var
  lTextBlock: TMarkdownTextBlock absolute aBlock;
  Parents : TMarkdownElementRendererArray;
  Parent : TMarkdownElementRenderer;
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
    if (Parent is TCanvasMarkdownTableBlockRenderer)
       and (TCanvasMarkdownTableBlockRenderer(Parent).CurrentRow=0) then
      Include(FontStyles,fsBold);
    if Parent is TCanvasMarkdownTableRowBlockRenderer then
      if TCanvasMarkdownTableRowBlockRenderer(Parent).FTableRenderer.CurrentRow=0 then
         Include(FontStyles,fsBold);
    if Parent is TCanvasMarkdownHeadingBlockRenderer then
      Fontsize:=TCanvasMarkdownHeadingBlockRenderer(Parent).fontSize;
    if (Parent is TCanvasMarkdownQuoteBlockRenderer) then
      Include(FontContext,fcQuote);
    if (Parent is TCanvasMarkdownCodeBlockRenderer) then
      FontContext:=FontContext+[fcMono,fcFencedCode];
    end;
  for i:=0 to lTextBlock.Nodes.Count - 1 do
  begin
    CanvasRenderer.RenderTextNode(lTextBlock.Nodes[i],FontSize, FontStyles, FontContext);
  end;
end;

class function TCanvasMarkdownTextBlockRenderer.BlockClass: TMarkdownBlockClass;
begin
  Result:=TMarkdownTextBlock;
end;

{ TCanvasMarkdownFrontmatterBlockRenderer }

procedure TCanvasMarkdownFrontmatterBlockRenderer.DoRender(aBlock: TMarkdownBlock);

begin
end;


class function TCanvasMarkdownFrontmatterBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownFrontmatterBlock;
end;


{ TCanvasMarkdownContainerBlockRenderer }

procedure TCanvasMarkdownContainerBlockRenderer.DoRender(aBlock: TMarkdownBlock);

begin
  if not assigned(aBlock) then exit;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ParagraphBreak;
end;


class function TCanvasMarkdownContainerBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownContainerBlock;
end;


{ TCanvasMarkdownLeafBlockRenderer }

procedure TCanvasMarkdownLeafBlockRenderer.DoRender(aBlock: TMarkdownBlock);

begin
end;


class function TCanvasMarkdownLeafBlockRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownLeafBlock;
end;

initialization
  // Register all canvas block renderers
  TCanvasMarkdownParagraphBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownHeadingBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownQuoteBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownListBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownListItemBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownCodeBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownTableBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownTableRowBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownThematicBreakBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownTextBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownFrontmatterBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownContainerBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
  TCanvasMarkdownLeafBlockRenderer.RegisterRenderer(TMarkdownCanvasRenderer);

  // Register text renderer
  TCanvasMarkdownTextRenderer.RegisterRenderer(TMarkdownCanvasRenderer);
end.
