{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown transforms applied to the parsed document.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.Transforms;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs,
{$ELSE}
  Classes, SysUtils, Contnrs,
{$ENDIF}
  Markdown.Elements,
  Markdown.Utils,
  Markdown.Parser;

type
  { TAlertTransform }

  // Turns a quote that starts with [!NOTE], [!TIP], [!IMPORTANT], [!WARNING] or [!CAUTION] into an alert.
  TAlertTransform = class(TMarkdownTransform)
  private
    function GetAlertType(aQuote : TMarkdownBlock; out aType : TAlertType) : Boolean;
    function ConvertQuote(aParent : TMarkdownContainerBlock; aQuote : TMarkdownQuoteBlock; aType : TAlertType) : TMarkdownAlertBlock;
    procedure Walk(aBlock : TMarkdownContainerBlock);
  public
    procedure Apply(aDocument : TMarkdownDocument); override;
  end;

  { TTaskListTransform }

  // Turns list items whose text starts with [ ], [x] or [X] into task list items.
  TTaskListTransform = class(TMarkdownTransform)
  private
    procedure CheckItem(aItem : TMarkdownListItemBlock);
    procedure Walk(aBlock : TMarkdownBlock);
  public
    procedure Apply(aDocument : TMarkdownDocument); override;
  end;

  { TCaptionTransform }

  // Merges Table: paragraphs into the adjacent table and turns single-image paragraphs into figures.
  TCaptionTransform = class(TMarkdownTransform)
  private
    function IsCaptionParagraph(aBlock : TMarkdownBlock) : Boolean;
    procedure CaptionTable(aParent : TMarkdownContainerBlock; aTable : TMarkdownTableBlock);
    function MakeFigure(aParent : TMarkdownContainerBlock; aPar : TMarkdownParagraphBlock) : Boolean;
    procedure Walk(aBlock : TMarkdownContainerBlock);
  public
    procedure Apply(aDocument : TMarkdownDocument); override;
  end;

  { TFootnoteTransform }

  // Numbers footnotes in order of first reference and links references to their definitions.
  TFootnoteTransform = class(TMarkdownTransform)
  private
    FDocument : TMarkdownDocument;
    FCount : Integer;
    FRefCounts : TStringList;
    procedure ResolveNodes(aNodes : TMarkdownTextNodeList);
    procedure Walk(aBlock : TMarkdownBlock);
    procedure CheckUnused(aBlock : TMarkdownBlock);
  public
    procedure Apply(aDocument : TMarkdownDocument); override;
  end;

  { THeadingIDTransform }

  // Gives every heading without an explicit id an automatic one.
  THeadingIDTransform = class(TMarkdownTransform)
  private
    FDocument : TMarkdownDocument;
    FAutoIDs : TStringList;
    procedure AssignID(aBlock : TMarkdownBlock);
    procedure Walk(aBlock : TMarkdownBlock);
  public
    procedure Apply(aDocument : TMarkdownDocument); override;
  end;

  { TMarkerTransform }

  // Collects all markers in document order in TMarkdownDocument.Markers.
  TMarkerTransform = class(TMarkdownTransform)
  private
    FDocument : TMarkdownDocument;
    procedure AddNodes(aBlock : TMarkdownBlock; aNodes : TMarkdownTextNodeList);
    procedure Walk(aBlock : TMarkdownBlock);
  public
    procedure Apply(aDocument : TMarkdownDocument); override;
  end;

// Is aBlock a heading: a heading block, or a setext heading paragraph ?
function IsHeadingBlock(aBlock : TMarkdownBlock) : Boolean;

implementation

function IsHeadingBlock(aBlock: TMarkdownBlock): Boolean;

begin
  Result:=(aBlock is TMarkdownHeadingBlock)
          or ((aBlock.ClassType=TMarkdownParagraphBlock) and (TMarkdownParagraphBlock(aBlock).Header>0));
end;


// Is aBlock a paragraph, and not a block derived from a paragraph ?
function IsPlainParagraph(aBlock : TMarkdownBlock) : Boolean;

begin
  Result:=(aBlock.ClassType=TMarkdownParagraphBlock) and (TMarkdownParagraphBlock(aBlock).Header=0)
          and (aBlock.ChildCount=1) and (aBlock.Children[0] is TMarkdownTextBlock);
end;

{ TAlertTransform }

function TAlertTransform.GetAlertType(aQuote: TMarkdownBlock; out aType: TAlertType): Boolean;

var
  lText : TMarkdownString;
  P : Integer;
  lType : TAlertType;

begin
  Result:=False;
  aType:=atNote;
  if (aQuote.ChildCount=0) or not IsPlainParagraph(aQuote.Children[0]) then
    Exit;
  lText:=TMarkdownTextBlock(aQuote.Children[0].Children[0]).Text;
  P:=Pos(#10,lText);
  if P>0 then
    SetLength(lText,P-1);
  lText:=LowerCase(Trim(lText));
  for lType in TAlertType do
    if lText='[!'+AlertTypeNames[lType]+']' then
      begin
      aType:=lType;
      Exit(True);
      end;
end;


function TAlertTransform.ConvertQuote(aParent: TMarkdownContainerBlock; aQuote: TMarkdownQuoteBlock; aType: TAlertType): TMarkdownAlertBlock;

var
  lPar : TMarkdownBlock;
  lText : TMarkdownTextBlock;
  lRest : TMarkdownString;
  P : Integer;

begin
  Result:=TMarkdownAlertBlock.Create(Nil,aQuote.Line);
  Result.AlertType:=aType;
  while aQuote.ChildCount>0 do
    Result.InsertChild(Result.ChildCount,aQuote.ExtractChild(0));
  aParent.ReplaceChild(aQuote,Result);
  Result.Closed:=True;
  lPar:=Result.Children[0];
  lText:=TMarkdownTextBlock(lPar.Children[0]);
  P:=Pos(#10,lText.Text);
  if P=0 then
    Result.DeleteChild(0)
  else
    begin
    lRest:=Copy(lText.Text,P+1,Length(lText.Text)-P);
    lText.Text:=lRest;
    lText.Nodes.Free;
    lText.Nodes:=Parser.ParseInlineText(lRest,lPar.Line+1);
    end;
end;


procedure TAlertTransform.Walk(aBlock: TMarkdownContainerBlock);

var
  I : Integer;
  lChild : TMarkdownBlock;
  lType : TAlertType;

begin
  for I:=0 to aBlock.ChildCount-1 do
    begin
    lChild:=aBlock.Children[I];
    if (lChild.ClassType=TMarkdownQuoteBlock) and GetAlertType(lChild,lType) then
      lChild:=ConvertQuote(aBlock,TMarkdownQuoteBlock(lChild),lType);
    if lChild is TMarkdownContainerBlock then
      Walk(TMarkdownContainerBlock(lChild));
    end;
end;


procedure TAlertTransform.Apply(aDocument: TMarkdownDocument);

begin
  if mdoAlerts in Parser.Options then
    Walk(aDocument);
end;

{ TTaskListTransform }

procedure TTaskListTransform.CheckItem(aItem: TMarkdownListItemBlock);

var
  lText : TMarkdownTextBlock;
  S : TMarkdownString;

begin
  if (aItem.ChildCount=0) or not IsPlainParagraph(aItem.Children[0]) then
    Exit;
  lText:=TMarkdownTextBlock(aItem.Children[0].Children[0]);
  S:=TrimLeft(lText.Text);
  if (Length(S)<5) or (S[1]<>'[') or not (S[2] in [' ','x','X']) or (S[3]<>']') or not (S[4] in [' ',#9,#10]) then
    Exit;
  if S[2]=' ' then
    aItem.TaskState:=tisUnchecked
  else
    aItem.TaskState:=tisChecked;
  S:=TrimLeft(Copy(S,5,Length(S)-4));
  lText.Text:=S;
  lText.Nodes.Free;
  lText.Nodes:=Parser.ParseInlineText(S,lText.Line);
end;


procedure TTaskListTransform.Walk(aBlock: TMarkdownBlock);

var
  I : Integer;

begin
  if aBlock is TMarkdownListItemBlock then
    CheckItem(TMarkdownListItemBlock(aBlock));
  for I:=0 to aBlock.ChildCount-1 do
    Walk(aBlock.Children[I]);
end;


procedure TTaskListTransform.Apply(aDocument: TMarkdownDocument);

begin
  if mdoTaskLists in Parser.Options then
    Walk(aDocument);
end;

{ TCaptionTransform }

function TCaptionTransform.IsCaptionParagraph(aBlock: TMarkdownBlock): Boolean;

begin
  Result:=IsPlainParagraph(aBlock)
          and TrimLeft(TMarkdownTextBlock(aBlock.Children[0]).Text).StartsWith('Table:');
end;


procedure TCaptionTransform.CaptionTable(aParent: TMarkdownContainerBlock; aTable: TMarkdownTableBlock);

var
  lIdx,lCapIdx,lTableEnd : Integer;
  lPar : TMarkdownBlock;
  lText,lSpec : TMarkdownString;

  function LastLine(aBlock : TMarkdownBlock) : Integer;
  begin
    if aBlock is TMarkdownTableBlock then
      Result:=aBlock.LastChild.Line
    else
      Result:=aBlock.Line+TMarkdownTextBlock(aBlock.Children[0]).Text.CountChar(#10);
  end;

begin
  lIdx:=aParent.IndexOfChild(aTable);
  lCapIdx:=-1;
  if (lIdx>0) and IsCaptionParagraph(aParent.Children[lIdx-1])
     and (aTable.Line-LastLine(aParent.Children[lIdx-1])<=2) then
    lCapIdx:=lIdx-1
  else if (lIdx<aParent.ChildCount-1) and IsCaptionParagraph(aParent.Children[lIdx+1]) then
    begin
    lTableEnd:=aTable.Line;
    if aTable.ChildCount>0 then
      lTableEnd:=LastLine(aTable);
    if aParent.Children[lIdx+1].Line-lTableEnd<=2 then
      lCapIdx:=lIdx+1;
    end;
  if lCapIdx<0 then
    Exit;
  lPar:=aParent.Children[lCapIdx];
  lText:=Trim(TMarkdownTextBlock(lPar.Children[0]).Text);
  Delete(lText,1,Length('Table:'));
  if mdoAttributes in Parser.Options then
    begin
    lSpec:=ExtractTrailingAttributeSpec(lText);
    if aTable.ApplyAttributes(lSpec) and (aTable.ID<>'') then
      Parser.RegisterAnchor(aTable.ID,aTable,lPar.Line);
    end;
  aTable.Caption:=Parser.ParseInlineText(Trim(lText),lPar.Line);
  aParent.DeleteChild(lCapIdx);
end;


function TCaptionTransform.MakeFigure(aParent: TMarkdownContainerBlock; aPar: TMarkdownParagraphBlock): Boolean;

var
  lTextBlock : TMarkdownTextBlock;
  lImage,lNode : TMarkdownTextNode;
  lTrail,lSpec,lAlt,lRaw : TMarkdownString;
  I,lImgIdx,lDepth : Integer;
  lFigure : TMarkdownFigureBlock;

begin
  Result:=False;
  lTextBlock:=TMarkdownTextBlock(aPar.Children[0]);
  if not Assigned(lTextBlock.Nodes) then
    Exit;
  lImage:=Nil;
  lImgIdx:=-1;
  lTrail:='';
  for I:=0 to lTextBlock.Nodes.Count-1 do
    begin
    lNode:=lTextBlock.Nodes[I];
    if lImage=Nil then
      begin
      if lNode.Kind=nkImg then
        begin
        lImage:=lNode;
        lImgIdx:=I;
        end
      else if (lNode.Kind<>nkText) or not IsWhitespace(lNode.NodeText) then
        Exit;
      end
    else if (lNode.Kind=nkText) and (lNode.Styles=[]) then
      lTrail:=lTrail+lNode.NodeText
    else
      Exit;
    end;
  if lImage=Nil then
    Exit;
  lTrail:=Trim(lTrail);
  lSpec:='';
  if lTrail<>'' then
    begin
    if not (mdoAttributes in Parser.Options) then
      Exit;
    lSpec:=lTrail;
    if ExtractTrailingAttributeSpec(lTrail)='' then
      Exit;
    if lTrail<>'' then
      Exit;
    end;
  // Raw alt text: from ![ up to the matching ]
  lRaw:=TrimLeft(lTextBlock.Text);
  lAlt:='';
  lDepth:=0;
  I:=3;
  while I<=Length(lRaw) do
    begin
    if (lRaw[I]='\') and (I<Length(lRaw)) then
      begin
      lAlt:=lAlt+lRaw[I]+lRaw[I+1];
      Inc(I,2);
      Continue;
      end;
    if lRaw[I]='[' then
      Inc(lDepth)
    else if lRaw[I]=']' then
      begin
      if lDepth=0 then
        Break;
      Dec(lDepth);
      end;
    lAlt:=lAlt+lRaw[I];
    Inc(I);
    end;
  while lTextBlock.Nodes.Count>lImgIdx+1 do
    lTextBlock.Nodes.Delete(lTextBlock.Nodes.Count-1);
  for I:=lImgIdx-1 downto 0 do
    lTextBlock.Nodes.Delete(I);
  lFigure:=TMarkdownFigureBlock.Create(Nil,aPar.Line);
  lFigure.InsertChild(0,aPar.ExtractChild(0));
  lFigure.Image:=lImage;
  lFigure.Caption:=Parser.ParseInlineText(lAlt,aPar.Line);
  lFigure.Closed:=True;
  if lFigure.ApplyAttributes(lSpec) and (lFigure.ID<>'') then
    Parser.RegisterAnchor(lFigure.ID,lFigure,aPar.Line);
  aParent.ReplaceChild(aPar,lFigure);
  Result:=True;
end;


procedure TCaptionTransform.Walk(aBlock: TMarkdownContainerBlock);

var
  I : Integer;
  lChild : TMarkdownBlock;

begin
  I:=0;
  while I<aBlock.ChildCount do
    begin
    lChild:=aBlock.Children[I];
    if lChild is TMarkdownTableBlock then
      begin
      CaptionTable(aBlock,TMarkdownTableBlock(lChild));
      I:=aBlock.IndexOfChild(lChild);
      end
    else if IsPlainParagraph(lChild) then
      MakeFigure(aBlock,TMarkdownParagraphBlock(lChild))
    else if lChild is TMarkdownContainerBlock then
      Walk(TMarkdownContainerBlock(lChild));
    Inc(I);
    end;
end;


procedure TCaptionTransform.Apply(aDocument: TMarkdownDocument);

begin
  if mdoCaptions in Parser.Options then
    Walk(aDocument);
end;

{ TFootnoteTransform }

procedure TFootnoteTransform.ResolveNodes(aNodes: TMarkdownTextNodeList);

var
  lNode : TMarkdownTextNode;
  lLabel : TMarkdownString;
  lDef : TMarkdownFootnoteBlock;
  lIdx : Integer;

begin
  if aNodes=Nil then
    Exit;
  for lNode in aNodes do
    begin
    if lNode.Kind=nkFootnoteRef then
      begin
      lLabel:=lNode.Attrs['label'];
      lDef:=FDocument.FindFootnoteDef(lLabel);
      if lDef=Nil then
        begin
        Parser.DoMessageFmt(mlError,lNode.Pos.line,SErrUndefinedFootnote,[MarkdownToString(lLabel)]);
        lNode.Kind:=nkText;
        lNode.SetNodeText('[^'+lLabel+']');
        lNode.Attrs.Clear;
        end
      else
        begin
        if lDef.Number=0 then
          begin
          Inc(FCount);
          lDef.Number:=FCount;
          FDocument.Footnotes.Add(lDef);
          end;
        lIdx:=FRefCounts.IndexOf(MarkdownToString(lDef.FootnoteLabel));
        if lIdx<0 then
          lIdx:=FRefCounts.AddObject(MarkdownToString(lDef.FootnoteLabel),TObject(PtrInt(0)));
        FRefCounts.Objects[lIdx]:=TObject(PtrInt(FRefCounts.Objects[lIdx])+1);
        lNode.Attrs.Add('number',IntToMarkdown(lDef.Number));
        lNode.Attrs.Add('refindex',IntToMarkdown(PtrInt(FRefCounts.Objects[lIdx])));
        end;
      end;
    if lNode.HasChildren then
      ResolveNodes(lNode.Children);
    end;
end;


procedure TFootnoteTransform.Walk(aBlock: TMarkdownBlock);

var
  I : Integer;

begin
  if aBlock is TMarkdownTextBlock then
    ResolveNodes(TMarkdownTextBlock(aBlock).Nodes)
  else if aBlock is TMarkdownTableBlock then
    ResolveNodes(TMarkdownTableBlock(aBlock).Caption)
  else if aBlock is TMarkdownFigureBlock then
    ResolveNodes(TMarkdownFigureBlock(aBlock).Caption)
  else if aBlock is TMarkdownDirectiveBlock then
    ResolveNodes(TMarkdownDirectiveBlock(aBlock).LabelNodes);
  for I:=0 to aBlock.ChildCount-1 do
    Walk(aBlock.Children[I]);
end;


procedure TFootnoteTransform.CheckUnused(aBlock: TMarkdownBlock);

var
  I : Integer;

begin
  if (aBlock is TMarkdownFootnoteBlock) and (TMarkdownFootnoteBlock(aBlock).Number=0) then
    Parser.DoMessageFmt(mlWarning,aBlock.Line,SWarnUnusedFootnote,[MarkdownToString(TMarkdownFootnoteBlock(aBlock).FootnoteLabel)]);
  for I:=0 to aBlock.ChildCount-1 do
    CheckUnused(aBlock.Children[I]);
end;


procedure TFootnoteTransform.Apply(aDocument: TMarkdownDocument);

begin
  if not (mdoFootnotes in Parser.Options) then
    Exit;
  FDocument:=aDocument;
  FCount:=0;
  FRefCounts:=TStringList.Create;
  try
    FRefCounts.CaseSensitive:=False;
    Walk(aDocument);
    CheckUnused(aDocument);
  finally
    FreeAndNil(FRefCounts);
  end;
end;

{ THeadingIDTransform }

procedure THeadingIDTransform.AssignID(aBlock: TMarkdownBlock);

var
  lBase,lID : TMarkdownString;
  lCount : Integer;

begin
  lBase:=HeadingTextToID(aBlock.PlainText);
  if lBase='' then
    lBase:='section';
  lID:=lBase;
  lCount:=0;
  while FDocument.FindAnchor(lID)<>Nil do
    begin
    Inc(lCount);
    lID:=lBase+'-'+IntToMarkdown(lCount);
    end;
  if (lCount>0) and (FAutoIDs.IndexOf(MarkdownToString(lBase))<0) then
    Parser.DoMessageFmt(mlWarning,aBlock.Line,SWarnAutoIDClash,[MarkdownToString(lBase),MarkdownToString(lID)]);
  aBlock.ID:=lID;
  FDocument.AddAnchor(lID,aBlock);
  FAutoIDs.Add(MarkdownToString(lID));
end;


procedure THeadingIDTransform.Walk(aBlock: TMarkdownBlock);

var
  I : Integer;

begin
  if IsHeadingBlock(aBlock) and (aBlock.ID='') then
    AssignID(aBlock);
  for I:=0 to aBlock.ChildCount-1 do
    Walk(aBlock.Children[I]);
end;


procedure THeadingIDTransform.Apply(aDocument: TMarkdownDocument);

begin
  if not (mdoHeadingIds in Parser.Options) then
    Exit;
  FDocument:=aDocument;
  FAutoIDs:=TStringList.Create;
  try
    FAutoIDs.Sorted:=True;
    Walk(aDocument);
  finally
    FreeAndNil(FAutoIDs);
  end;
end;

{ TMarkerTransform }

procedure TMarkerTransform.AddNodes(aBlock: TMarkdownBlock; aNodes: TMarkdownTextNodeList);

  procedure AddNode(aNode : TMarkdownTextNode; aIndex : Integer);
  var
    lChild : TMarkdownTextNode;
  begin
    if (aNode.Kind=nkComment) and aNode.HasAttrs and aNode.Attrs.Contains('marker') then
      FDocument.Markers.Add(TMarkdownMarker.Create(aNode.Attrs['marker'],aNode.Attrs['argument'],aNode.Attrs['value'],
                                                   aBlock,aNode,aIndex));
    if aNode.HasChildren then
      for lChild in aNode.Children do
        AddNode(lChild,aIndex);
  end;

var
  I : Integer;

begin
  if aNodes=Nil then
    Exit;
  for I:=0 to aNodes.Count-1 do
    AddNode(aNodes[I],I);
end;


procedure TMarkerTransform.Walk(aBlock: TMarkdownBlock);

var
  I : Integer;
  lComment : TMarkdownCommentBlock absolute aBlock;

begin
  if aBlock is TMarkdownCommentBlock then
    begin
    if lComment.IsMarker then
      FDocument.Markers.Add(TMarkdownMarker.Create(lComment.MarkerName,lComment.MarkerArgument,lComment.MarkerValue,aBlock,Nil,-1));
    end
  else if aBlock is TMarkdownTextBlock then
    AddNodes(aBlock,TMarkdownTextBlock(aBlock).Nodes)
  else if aBlock is TMarkdownTableBlock then
    AddNodes(aBlock,TMarkdownTableBlock(aBlock).Caption)
  else if aBlock is TMarkdownFigureBlock then
    AddNodes(aBlock,TMarkdownFigureBlock(aBlock).Caption)
  else if aBlock is TMarkdownDirectiveBlock then
    AddNodes(aBlock,TMarkdownDirectiveBlock(aBlock).LabelNodes);
  for I:=0 to aBlock.ChildCount-1 do
    Walk(aBlock.Children[I]);
end;


procedure TMarkerTransform.Apply(aDocument: TMarkdownDocument);

begin
  if not (mdoComments in Parser.Options) then
    Exit;
  FDocument:=aDocument;
  Walk(aDocument);
end;


initialization
  TTaskListTransform.Register('tasklists');
  TAlertTransform.Register('alerts');
  TCaptionTransform.Register('captions');
  TFootnoteTransform.Register('footnotes');
  THeadingIDTransform.Register('headingids');
  TMarkerTransform.Register('markers');
end.
