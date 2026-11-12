{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown renderer class & render factory.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.Render;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs,
{$ELSE}
  Classes, SysUtils, Contnrs,
{$ENDIF}
  Markdown.Elements,
  Markdown.Utils;

Type
  TMarkdownElementRenderer = Class;
  TMarkdownElementRendererClass = class of TMarkdownElementRenderer;

  TMarkdownBlockRenderer = class;
  TMarkdownBlockRendererClass = class of TMarkdownBlockRenderer;
  TMarkdownElementRendererArray = array of TMarkdownElementRenderer;

  TMarkdownTextRenderer = class;
  TMarkdownTextRendererClass = class of TMarkdownTextRenderer;

  TMarkdownRenderer = class;

  // Called where a marker comment occurs.
  TMarkdownMarkerEvent = procedure(aRenderer : TMarkdownRenderer; const aName, aArgument, aValue : TMarkdownString) of object;
  // Called for every link before it is written. Set aHandled when the handler wrote the link itself.
  TMarkdownResolveLinkEvent = procedure(aRenderer : TMarkdownRenderer; var aHref, aText : TMarkdownString; out aHandled : Boolean) of object;
  // Returns the number to write before a heading.
  TMarkdownHeadingNumberEvent = procedure(aRenderer : TMarkdownRenderer; aBlock : TMarkdownBlock; out aNumber : TMarkdownString) of object;
  // Returns the label to write before a table or figure caption.
  TMarkdownCaptionNumberEvent = procedure(aRenderer : TMarkdownRenderer; aBlock : TMarkdownBlock; out aLabel : TMarkdownString) of object;

resourcestring
  SAlertNote = 'Note';
  SAlertTip = 'Tip';
  SAlertImportant = 'Important';
  SAlertWarning = 'Warning';
  SAlertCaution = 'Caution';

type
  { TMarkdownRenderer }

  TMarkdownRenderer = class(TComponent)
  private
    FSkipUnknownElements: Boolean;
    FTextRenderer : TMarkdownTextRenderer;
    FRenderStack : TFPList;
    FAlertTitles : Array[TAlertType] of TMarkdownString;
    FOnCaptionNumber: TMarkdownCaptionNumberEvent;
    FOnHeadingNumber: TMarkdownHeadingNumberEvent;
    FOnMarker: TMarkdownMarkerEvent;
    FOnResolveLink: TMarkdownResolveLinkEvent;
    function GetAlertTitle(aType : TAlertType): TMarkdownString;
    function GetParentElementRenderer: TMarkdownElementRenderer;
    procedure SetAlertTitle(aType : TAlertType; const aValue: TMarkdownString);
  protected
    function CreateRendererInstance(aClass : TMarkdownBlockRendererClass) : TMarkdownBlockRenderer; virtual;
    function CreateRendererForBlock(aBlock : TMarkdownBlock) : TMarkdownBlockRenderer; virtual;
    function CreateTextRendererInstance(aClass : TMarkdownTextRendererClass): TMarkdownTextRenderer; virtual;
    function GetTextRenderer : TMarkdownTextRenderer;
    Property ParentElementRenderer: TMarkdownElementRenderer Read GetParentElementRenderer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure RenderText(aText : TMarkdownTextNode); virtual;
    Procedure RenderTextNodes(aTextNodes : TMarkdownTextNodeList);
    Procedure RenderBlock(aBlock : TMarkdownBlock); virtual;
    procedure RenderCodeBlock(aBlock: TMarkdownBlock; const aLang: TMarkdownString); virtual;
    procedure RenderChildren(aBlock : TMarkdownContainerBlock); virtual;
    Procedure RenderDocument(aDocument : TMarkdownDocument); virtual; abstract;
    // Write text that is already in the target format. The default does nothing.
    procedure WriteRaw(const aText : TMarkdownString); virtual;
    // Call OnMarker for a marker.
    procedure DoMarker(const aName, aArgument, aValue : TMarkdownString); virtual;
    // Call OnMarker when aNode is a marker comment node.
    procedure DoMarkerNode(aNode : TMarkdownTextNode);
    // Call OnResolveLink. Returns True when the handler wrote the link.
    function DoResolveLink(var aHref, aText : TMarkdownString) : Boolean; virtual;
    // The number to write before heading aBlock, from OnHeadingNumber.
    function GetHeadingNumber(aBlock : TMarkdownBlock) : TMarkdownString; virtual;
    // The label to write before the caption of table or figure aBlock, from OnCaptionNumber.
    function GetCaptionNumber(aBlock : TMarkdownBlock) : TMarkdownString; virtual;
    // Title written above an alert of the given type.
    property AlertTitles[aType : TAlertType] : TMarkdownString read GetAlertTitle write SetAlertTitle;
  published
    Property SkipUnknownElements : Boolean read FSkipUnknownElements Write FSkipUnknownElements;
    // Called where a marker comment occurs.
    Property OnMarker : TMarkdownMarkerEvent read FOnMarker write FOnMarker;
    // Called for every link before it is written.
    Property OnResolveLink : TMarkdownResolveLinkEvent read FOnResolveLink write FOnResolveLink;
    // Called for the number to write before a heading.
    Property OnHeadingNumber : TMarkdownHeadingNumberEvent read FOnHeadingNumber write FOnHeadingNumber;
    // Called for the label to write before a table or figure caption.
    Property OnCaptionNumber : TMarkdownCaptionNumberEvent read FOnCaptionNumber write FOnCaptionNumber;
  end;
  TMarkdownRendererClass = class of TMarkdownRenderer;

  { TMarkdownElementRenderer }

  TMarkdownElementRenderer = Class (TObject)
  private
    FRenderer: TMarkdownRenderer;
    function GetParentElementRenderer: TMarkdownElementRenderer;
  protected
    function GetParentRenderers : TMarkdownElementRendererArray;
    function GetFirstParentWithClass(aClass : TMarkdownElementRendererClass) : TMarkdownElementRenderer;
    Property Renderer : TMarkdownRenderer read FRenderer;
    Property ParentElementRenderer : TMarkdownElementRenderer read GetParentElementRenderer;
  public
    constructor Create(aRenderer : TMarkdownRenderer);
    procedure Reset; virtual;
  end;


  { TMarkdownBlockRenderer }

  TMarkdownBlockRenderer = Class (TMarkdownElementRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); virtual; abstract;
  Public
    class function BlockClass : TMarkdownBlockClass; virtual; abstract;
    class procedure RegisterRenderer(aRendererClass : TMarkdownRendererClass);
    procedure Render(aBlock : TMarkdownBlock); inline;
  end;

  { TMarkdownTextRenderer }

  TMarkdownTextRenderer = class(TMarkdownElementRenderer)
  protected
    procedure DoRender(aElement: TMarkdownTextNode); virtual; abstract;
  public
    class procedure RegisterRenderer(aRendererClass : TMarkdownRendererClass);
    procedure Render(aElement : TMarkdownTextNode); inline;
    // Block state management.
    procedure BeginBlock; virtual;
    procedure EndBlock; virtual;
  end;

  { TNullRenderer }

  TNullRenderer = class(TMarkdownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkdownBlock); override;
  end;

  { TDocumentRenderer }

  TDocumentRenderer = class(TMarkdownBlockRenderer)
    class function BlockClass : TMarkdownBlockClass; override;
  end;


  { TMarkdownRendererFactory }

  TMarkdownRendererFactory = class(TObject)
  private
    class var _Instance : TMarkdownRendererFactory;
  type
    { TBlockRenderRegistration }

    TBlockRenderRegistration = class
      BlockClass : TMarkdownBlockClass;
      RendererClass : TMarkdownBlockRendererClass;
      constructor Create(aBlockClass : TMarkdownBlockClass; aRendererClass : TMarkdownBlockRendererClass);
    end;
    TBlockRenderRegistrationList = Specialize TGFPObjectList<TBlockRenderRegistration>;

    { TRenderBlockRenderers }

    TRenderBlockRenderers = class(TObject)
      Renderer : TMarkdownRendererClass;
      BlockRenderers : TBlockRenderRegistrationList;
      Textrenderer : TMarkdownTextRendererClass;
      Constructor Create(aRenderer : TMarkdownRendererClass);
      destructor Destroy; override;
      function FindBlock (aClass : TMarkdownBlockClass; aAllowCreate : Boolean) : TBlockRenderRegistration;
     end;
     TRenderBlockRenderersList = specialize TGFPObjectList<TRenderBlockRenderers>;

  private
    FRegistry : TRenderBlockRenderersList;
  protected
    function FindRenderer(aClass : TMarkdownRendererClass; allowCreate : Boolean) : TRenderBlockRenderers;
  public
    class constructor Init;
    class destructor Done;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterBlockRenderer(aRendererClass : TMarkdownRendererClass; aBlockClass : TMarkdownBlockClass; aBlockRendererClass : TMarkdownBlockRendererClass);
    function FindBlockRendererClass(aRendererClass : TMarkdownRendererClass; aBlockClass : TMarkdownBlockClass) : TMarkdownBlockRendererClass;
    procedure RegisterTextRenderer(aRendererClass : TMarkdownRendererClass; aTextRendererClass : TMarkdownTextRendererClass);
    function FindTextRendererClass(aRendererClass : TMarkdownRendererClass) : TMarkdownTextRendererClass;
    class property Instance : TMarkdownRendererFactory read _Instance;
  end;

implementation

class procedure TMarkdownBlockRenderer.RegisterRenderer(aRendererClass: TMarkdownRendererClass);

begin
  TMarkdownRendererFactory.Instance.RegisterBlockRenderer(aRendererClass, BlockClass, Self);
end;


procedure TMarkdownBlockRenderer.Render(aBlock: TMarkdownBlock);

begin
  DoRender(aBlock);
end;


{ TMarkdownTextRenderer }

class procedure TMarkdownTextRenderer.RegisterRenderer(aRendererClass: TMarkdownRendererClass);

begin
  TMarkdownRendererFactory.Instance.RegisterTextRenderer(aRendererClass,Self);
end;


procedure TMarkdownTextRenderer.Render(aElement: TMarkdownTextNode);

begin
  DoRender(aElement);
end;


procedure TMarkdownTextRenderer.BeginBlock;

begin
  // Do nothing
end;

procedure TMarkdownTextRenderer.EndBlock;

begin
  //
end;

{ TNullRenderer }

procedure TNullRenderer.DoRender(aBlock: TMarkdownBlock);

begin
  if aBlock is TMarkdownContainerBlock then
    Renderer.RenderChildren(TMarkdownContainerBlock(aBlock));
end;


{ TDocumentRenderer }

class function TDocumentRenderer.BlockClass: TMarkdownBlockClass;

begin
  Result:=TMarkdownDocument;
end;


function TMarkdownRenderer.CreateRendererInstance(aClass: TMarkdownBlockRendererClass): TMarkdownBlockRenderer;

begin
  Result:=aClass.Create(Self);
end;


function TMarkdownRenderer.CreateRendererForBlock(aBlock: TMarkdownBlock): TMarkdownBlockRenderer;

var
  lRenderClass : TMarkdownRendererClass;
  lBlockClass : TMarkdownBlockClass;
  LBlockRendererClass : TMarkdownBlockRendererClass;

begin
  Result:=Nil;
  lRenderClass:=TMarkdownRendererClass(Self.ClassType);
  lBlockClass:=TMarkdownBlockClass(aBlock.ClassType);
  LBlockRendererClass:=TMarkdownRendererFactory.Instance.FindBlockRendererClass(lRenderClass,lBlockClass);
  if assigned(LBlockRendererClass) then
    Result:=CreateRendererInstance(LBlockRendererClass);
  if assigned(Result) then
    Result.Reset;
end;


function TMarkdownRenderer.CreateTextRendererInstance(aClass : TMarkdownTextRendererClass): TMarkdownTextRenderer;

begin
  Result:=aClass.Create(Self);
end;

function TMarkdownRenderer.GetTextRenderer: TMarkdownTextRenderer;

var
  lClass : TMarkdownTextRendererClass;
  lRenderClass : TMarkdownRendererClass;

begin
  Result:=nil;
  if FTextRenderer=Nil then
    begin
    lRenderClass:=TMarkdownRendererClass(Self.ClassType);
    lClass:=TMarkdownRendererFactory.Instance.FindTextRendererClass(lRenderClass);
    if assigned(lClass) then
      FTextRenderer:=CreateTextRendererInstance(lClass);
    end;
  Result:=FTextRenderer;
end;

function TMarkdownRenderer.GetParentElementRenderer: TMarkdownElementRenderer;
begin
  if FRenderStack.Count>1 then
    Result:=TMarkdownElementRenderer(FRenderStack[FRenderStack.Count-2])
  else
    Result:=Nil;
end;

constructor TMarkdownRenderer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderStack:=TFPList.Create;
  FAlertTitles[atNote]:=StringToMarkdown(SAlertNote);
  FAlertTitles[atTip]:=StringToMarkdown(SAlertTip);
  FAlertTitles[atImportant]:=StringToMarkdown(SAlertImportant);
  FAlertTitles[atWarning]:=StringToMarkdown(SAlertWarning);
  FAlertTitles[atCaution]:=StringToMarkdown(SAlertCaution);
end;


function TMarkdownRenderer.GetAlertTitle(aType : TAlertType): TMarkdownString;

begin
  Result:=FAlertTitles[aType];
end;


procedure TMarkdownRenderer.SetAlertTitle(aType : TAlertType; const aValue: TMarkdownString);

begin
  FAlertTitles[aType]:=aValue;
end;


procedure TMarkdownRenderer.WriteRaw(const aText: TMarkdownString);

begin
  if aText='' then ; // Silence warning
end;


procedure TMarkdownRenderer.DoMarker(const aName, aArgument, aValue: TMarkdownString);

begin
  if Assigned(FOnMarker) then
    FOnMarker(Self,aName,aArgument,aValue);
end;


procedure TMarkdownRenderer.DoMarkerNode(aNode: TMarkdownTextNode);

var
  lName,lArgument,lValue : TMarkdownString;

begin
  if (aNode.Kind<>nkComment) or not aNode.HasAttrs then
    Exit;
  if not aNode.Attrs.TryGet('marker',lName) then
    Exit;
  aNode.Attrs.TryGet('argument',lArgument);
  aNode.Attrs.TryGet('value',lValue);
  DoMarker(lName,lArgument,lValue);
end;


function TMarkdownRenderer.DoResolveLink(var aHref, aText: TMarkdownString): Boolean;

begin
  Result:=False;
  if Assigned(FOnResolveLink) then
    FOnResolveLink(Self,aHref,aText,Result);
end;


function TMarkdownRenderer.GetHeadingNumber(aBlock: TMarkdownBlock): TMarkdownString;

begin
  Result:='';
  if Assigned(FOnHeadingNumber) then
    FOnHeadingNumber(Self,aBlock,Result);
end;


function TMarkdownRenderer.GetCaptionNumber(aBlock: TMarkdownBlock): TMarkdownString;

begin
  Result:='';
  if Assigned(FOnCaptionNumber) then
    FOnCaptionNumber(Self,aBlock,Result);
end;

destructor TMarkdownRenderer.Destroy;
begin
  FreeAndNil(FRenderStack);
  FreeAndNil(FTextRenderer);
  inherited destroy;
end;


procedure TMarkdownRenderer.RenderText(aText: TMarkdownTextNode);

var
  lRender : TMarkdownTextRenderer;

begin
  lRender:=GetTextRenderer;
  lRender.BeginBlock;
  lRender.render(aText);
  lRender.EndBlock;
end;

procedure TMarkdownRenderer.RenderTextNodes(aTextNodes: TMarkdownTextNodeList);

var
  lRender : TMarkdownTextRenderer;
  lNode : TMarkdownTextNode;

begin
  lRender:=GetTextRenderer;
  lRender.BeginBlock;
  For lNode in aTextNodes do
    lRender.render(lNode);
  lRender.EndBlock;
end;

procedure TMarkdownRenderer.RenderBlock(aBlock: TMarkdownBlock);

var
  lRender : TMarkdownBlockRenderer;

begin
  if aBlock=Nil then
    Raise EMarkdown.Create('Cannot render nil block');
  lRender:=CreateRendererForBlock(aBlock);
  try
    if Assigned(lRender) then
      begin
      FRenderStack.Add(lRender);
      lRender.render(aBlock);
      end
    else
      Raise EMarkdown.CreateFmt('No renderer for block class: %s',[aBlock.ClassName]);
  finally
    if assigned(lRender) then
      FRenderStack.Delete(FRenderStack.Count-1);
    lRender.Free;
  end;
end;

procedure TMarkdownRenderer.RenderCodeBlock(aBlock: TMarkdownBlock; const aLang: TMarkdownString);
begin
  if (aLang='') then ; // Silence warning
  RenderBlock(aBlock);
end;

procedure TMarkdownRenderer.RenderChildren(aBlock: TMarkdownContainerBlock);

var
  I : integer;

begin
  for I:=0 to aBlock.Blocks.Count-1 do
    RenderBlock(aBlock.Blocks[I]);
end;

{ TMarkdownElementRenderer }

function TMarkdownElementRenderer.GetParentElementRenderer: TMarkdownElementRenderer;
begin
  Result:=Renderer.ParentElementRenderer;
end;

function TMarkdownElementRenderer.GetParentRenderers: TMarkdownElementRendererArray;
var
  i : integer;
begin
  Result:=[];
  SetLength(Result,Renderer.FRenderStack.Count);
  For I:=0 to Renderer.FRenderStack.Count-1 do
    Result[i]:=TMarkdownElementRenderer(Renderer.FRenderStack.items[i]);
end;

function TMarkdownElementRenderer.GetFirstParentWithClass(aClass: TMarkdownElementRendererClass): TMarkdownElementRenderer;
var
  I : integer;
begin
  Result:=Nil;
  I:=Renderer.FRenderStack.Count-1;
  While (Result=Nil) and (I>=0) do
    begin
    Result:=TMarkdownElementRenderer(Renderer.FRenderStack.items[i]);
    if Not Result.InheritsFrom(aClass) then
      Result:=Nil;
    Dec(i);
    end;
end;

constructor TMarkdownElementRenderer.Create(aRenderer: TMarkdownRenderer);

begin
  FRenderer:=aRenderer;
end;

procedure TMarkdownElementRenderer.Reset;

begin
  // Do nothing
end;

{ TMarkdownRendererFactory }

constructor TMarkdownRendererFactory.Create;

begin
  FRegistry:=TRenderBlockRenderersList.Create(True);
end;

destructor TMarkdownRendererFactory.Destroy;

begin
  FreeAndNil(FRegistry);
  inherited destroy;
end;


procedure TMarkdownRendererFactory.RegisterBlockRenderer(aRendererClass: TMarkdownRendererClass; aBlockClass: TMarkdownBlockClass;
  aBlockRendererClass: TMarkdownBlockRendererClass);

var
  lList : TRenderBlockRenderers;
  lReg : TBlockRenderRegistration;

begin
  lList:=FindRenderer(aRendererClass,True);
  lReg:=lList.FindBlock(aBlockClass,True);
  lReg.RendererClass:=aBlockRendererClass;
end;


function TMarkdownRendererFactory.FindBlockRendererClass(aRendererClass: TMarkdownRendererClass; aBlockClass: TMarkdownBlockClass
  ): TMarkdownBlockRendererClass;

var
  lList : TRenderBlockRenderers;
  lReg : TBlockRenderRegistration;
  lClass,lRenderClass : TClass;

begin
  Result:=Nil;
  lClass:=aBlockClass;
  while (Result=Nil) and Assigned(lClass) and (lClass<>TMarkdownBlock) do
    begin
    lRenderClass:=aRendererClass;
    while (Result=Nil) and Assigned(lRenderClass) and (lRenderClass<>TMarkdownRenderer) do
      begin
      lList:=FindRenderer(TMarkdownRendererClass(lRenderClass),False);
      if Assigned(lList) then
        begin
        lReg:=lList.FindBlock(TMarkdownBlockClass(lClass),False);
        if assigned(lReg) then
          Result:=lReg.RendererClass;
        end;
      lRenderClass:=lRenderClass.ClassParent;
      end;
    lClass:=lClass.ClassParent;
    end;
end;


procedure TMarkdownRendererFactory.RegisterTextRenderer(aRendererClass: TMarkdownRendererClass;
  aTextRendererClass: TMarkdownTextRendererClass);

var
  lList : TRenderBlockRenderers;

begin
  lList:=FindRenderer(aRendererClass,True);
  lList.Textrenderer:=aTextRendererClass;
end;


function TMarkdownRendererFactory.FindTextRendererClass(aRendererClass: TMarkdownRendererClass): TMarkdownTextRendererClass;

var
  lList : TRenderBlockRenderers;
  lRenderClass : TClass;

begin
  Result:=Nil;
  lRenderClass:=aRendererClass;
  while (Result=Nil) and Assigned(lRenderClass) and (lRenderClass<>TMarkdownRenderer) do
    begin
    lList:=FindRenderer(TMarkdownRendererClass(lRenderClass),False);
    if assigned(lList) then
      Result:=lList.Textrenderer;
    lRenderClass:=lRenderClass.ClassParent;
    end;
end;


function TMarkdownRendererFactory.FindRenderer(aClass: TMarkdownRendererClass; allowCreate: Boolean): TRenderBlockRenderers;

var
  I : Integer;

begin
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<FRegistry.Count) do
    begin
    Result:=FRegistry[I];
    if Result.Renderer<>aClass then
      Result:=Nil;
    Inc(I);
    end;
  if (Result=nil) and AllowCreate then
    begin
    Result:=TRenderBlockRenderers.Create(aClass);
    FRegistry.Add(Result);
    end;
end;


class constructor TMarkdownRendererFactory.Init;

begin
  _Instance:=TMarkdownRendererFactory.Create;
end;


class destructor TMarkdownRendererFactory.Done;

begin
  FreeAndNil(_Instance);
end;


{ TMarkdownRendererFactory.TBlockRenderRegistration }

constructor TMarkdownRendererFactory.TBlockRenderRegistration.Create(aBlockClass: TMarkdownBlockClass;
  aRendererClass: TMarkdownBlockRendererClass);

begin
  BlockClass:=aBlockClass;
  RendererClass:=aRendererClass;
end;


{ TMarkdownRendererFactory.TRenderBlockRenderers }

constructor TMarkdownRendererFactory.TRenderBlockRenderers.Create(aRenderer: TMarkdownRendererClass);

begin
  Renderer:=aRenderer;
  BlockRenderers:=TBlockRenderRegistrationList.Create(True);
end;


destructor TMarkdownRendererFactory.TRenderBlockRenderers.Destroy;

begin
  FreeAndNil(BlockRenderers);
  inherited destroy;
end;


function TMarkdownRendererFactory.TRenderBlockRenderers.FindBlock(aClass: TMarkdownBlockClass; aAllowCreate: Boolean
  ): TBlockRenderRegistration;

var
  I : Integer;

begin
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<BlockRenderers.Count) do
    begin
    Result:=BlockRenderers[I];
    if Result.BlockClass<>aClass then
      Result:=Nil;
    Inc(I);
    end;
  if (Result=nil) and aAllowCreate then
    begin
    Result:=TBlockRenderRegistration.Create(aClass,Nil);
    BlockRenderers.Add(Result);
    end;
end;


end.

