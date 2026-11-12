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


  { TMarkdownRenderer }

  TMarkdownRenderer = class(TComponent)
  private
    FSkipUnknownElements: Boolean;
    FTextRenderer : TMarkdownTextRenderer;
    FRenderStack : TFPList;
    function GetParentElementRenderer: TMarkdownElementRenderer;
  protected
    function CreateRendererInstance(aClass : TMarkdownBlockRendererClass) : TMarkdownBlockRenderer; virtual;
    function CreateRendererForBlock(aBlock : TMarkdownBlock) : TMarkdownBlockRenderer; virtual;
    function CreateTextRendererInstance(aClass : TMarkdownTextRendererClass): TMarkdownTextRenderer; virtual;
    function GetTextRenderer : TMarkdownTextRenderer;
    Property ParentElementRenderer: TMarkdownElementRenderer Read GetParentElementRenderer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    Procedure RenderText(aText : TMarkdownTextNode); virtual;
    Procedure RenderTextNodes(aTextNodes : TMarkdownTextNodeList);
    Procedure RenderBlock(aBlock : TMarkdownBlock); virtual;
    procedure RenderCodeBlock(aBlock: TMarkdownBlock; const aLang: string); virtual;
    procedure RenderChildren(aBlock : TMarkdownContainerBlock); virtual;
    Procedure RenderDocument(aDocument : TMarkdownDocument); virtual; abstract;
  published
    Property SkipUnknownElements : Boolean read FSkipUnknownElements Write FSkipUnknownElements;
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
    constructor create(aRenderer : TMarkdownRenderer);
    procedure reset; virtual;
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
    procedure render(aElement : TMarkdownTextNode); inline;
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
      constructor create(aBlockClass : TMarkdownBlockClass; aRendererClass : TMarkdownBlockRendererClass);
    end;
    TBlockRenderRegistrationList = Specialize TGFPObjectList<TBlockRenderRegistration>;

    { TRenderBlockRenderers }

    TRenderBlockRenderers = class(TObject)
      Renderer : TMarkdownRendererClass;
      BlockRenderers : TBlockRenderRegistrationList;
      Textrenderer : TMarkdownTextRendererClass;
      Constructor create(aRenderer : TMarkdownRendererClass);
      destructor destroy; override;
      function FindBlock (aClass : TMarkdownBlockClass; aAllowCreate : Boolean) : TBlockRenderRegistration;
     end;
     TRenderBlockRenderersList = specialize TGFPObjectList<TRenderBlockRenderers>;

  private
    FRegistry : TRenderBlockRenderersList;
  protected
    function FindRenderer(aClass : TMarkdownRendererClass; allowCreate : Boolean) : TRenderBlockRenderers;
  public
    class constructor init;
    class destructor done;
    constructor create;
    destructor destroy; override;
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


procedure TMarkdownBlockRenderer.render(aBlock: TMarkdownBlock);

begin
  DoRender(aBlock);
end;


{ TMarkdownTextRenderer }

class procedure TMarkdownTextRenderer.RegisterRenderer(aRendererClass: TMarkdownRendererClass);

begin
  TMarkdownRendererFactory.Instance.RegisterTextRenderer(aRendererClass,Self);
end;


procedure TMarkdownTextRenderer.render(aElement: TMarkdownTextNode);

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
  if FTextRenderer=Nil then;
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
end;

destructor TMarkdownRenderer.destroy;
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

procedure TMarkdownRenderer.RenderCodeBlock(aBlock: TMarkdownBlock; const aLang: string);
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

constructor TMarkdownElementRenderer.create(aRenderer: TMarkdownRenderer);

begin
  FRenderer:=aRenderer;
end;

procedure TMarkdownElementRenderer.reset;

begin
  // Do nothing
end;

{ TMarkdownRendererFactory }

constructor TMarkdownRendererFactory.create;

begin
  FRegistry:=TRenderBlockRenderersList.Create(True);
end;

destructor TMarkdownRendererFactory.destroy;

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

begin
  Result:=Nil;
  lList:=FindRenderer(aRendererClass,False);
  if Assigned(lList) then
    begin
    lReg:=lList.FindBlock(aBlockClass,False);
    if assigned(lReg) then
      Result:=lReg.RendererClass;
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

begin
  lList:=FindRenderer(aRendererClass,True);
  if assigned(lList) then
    Result:=lList.Textrenderer;
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


class constructor TMarkdownRendererFactory.init;

begin
  _Instance:=TMarkdownRendererFactory.Create;
end;


class destructor TMarkdownRendererFactory.done;

begin
  FreeAndNil(_Instance);
end;


{ TMarkdownRendererFactory.TBlockRenderRegistration }

constructor TMarkdownRendererFactory.TBlockRenderRegistration.create(aBlockClass: TMarkdownBlockClass;
  aRendererClass: TMarkdownBlockRendererClass);

begin
  BlockClass:=aBlockClass;
  RendererClass:=aRendererClass;
end;


{ TMarkdownRendererFactory.TRenderBlockRenderers }

constructor TMarkdownRendererFactory.TRenderBlockRenderers.create(aRenderer: TMarkdownRendererClass);

begin
  Renderer:=aRenderer;
  BlockRenderers:=TBlockRenderRegistrationList.Create(True);
end;


destructor TMarkdownRendererFactory.TRenderBlockRenderers.destroy;

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

