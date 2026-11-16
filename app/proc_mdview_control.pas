{
    CudaText addon 'md_view' control: syntax highlighting of fenced code blocks.

    Registered for the base TMarkDownCanvasRenderer class, so every
    TMarkDownControl instance created by dialog API uses it.
    When enabled, fenced code blocks are tokenized with the app's EControl
    lexers and painted with per-token colors, like in the editor.

    App-side only: the vendored markdown component files in comp/ stay
    unmodified. Design follows badrelmers/CudaText branch 'laz_markd'
    (app/proc_markdown_hilite.pas), adapted to work WITHOUT the component's
    OnHiliteCode hook:
      - block renderer subclass registered via RegisterRenderer()
      - per-run colors by switching CanvasRenderer.FontCodeColor between runs
      - tokenization cached per (info, code) pair: layout is recalculated on
        every resize/repaint, we don't re-run the lexer each time
      - analyzer parsing via ParseInThread(), freed on a background thread:
        its destructor joins the parser thread (polls termination every
        ~100 msec) - too slow for a paint cycle on the UI thread
}
unit proc_mdview_control;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math,
  // markdown component (vendored, read-only)
  Markdown.Control,
  Markdown.Elements,
  Markdown.Render,
  Markdown.CanvasRender,
  // EControl lexer engine of CudaText
  ec_SyntAnal,
  ec_LexerList,
  ec_Syntax_Format,
  ec_gramma,
  ec_lists,
  ATStringProc_TextBuffer;

type
  { TCudaMarkdownCodeBlockRenderer }

  { grants access to protected FlushTextRun / GetTextBGColor /
  CreateLayoutItem of the vendored renderer, to paint fenced code
  byte-for-byte like upstream's own code branch }
  TMarkDownCanvasRendererAccess = class(TMarkDownCanvasRenderer);

  TCudaMarkdownCodeBlockRenderer = class(TCanvasMarkDownCodeBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

// Enable/disable syntax highlighting of fenced code blocks for all
// 'md_view' controls. Applied on the next text/layout change.
var
  MdViewCodeSyntaxEnabled: boolean = true;

type
  TCudaMdSegment = record
    Text: utf8string;
    Color: TColor;
    Styles: TFontStyles;
    // byte range of the token in the source code (1-based, 0 = gap segment
    // filled by the caller with the default code color)
    Pos, Len: integer;
  end;
  TCudaMdSegmentList = array of TCudaMdSegment;
  TCudaMdLineSegments = array of TCudaMdSegmentList;

// Tokenize code (info = lexer name/alias), with caching:
// returns per-line colored segments; empty result = no highlighting.
function MdView_HighlightCodeBlock(const aInfo: utf8string;
  const aCode: utf8string): TCudaMdLineSegments;

// Debug capture of runs painted by the highlighted code renderer,
// for verification tests: what was actually sent to the canvas engine.
type
  TCudaMdPaintedRun = record
    Line: integer;
    Text: utf8string;
    X: integer;      // layout LineX before the run was painted
    Y: integer;      // layout LineY of the run (row top)
    Color: TColor;
  end;

function MdView_DebugPaintedRunCount: integer;
function MdView_DebugPaintedRun(AIndex: integer): TCudaMdPaintedRun;

type
  TCudaMdChunk = record
    Text: utf8string;
    Color: TColor;
    Styles: TFontStyles;
    WrapAfter: boolean; // paint this chunk, then advance to a new row
  end;
  TCudaMdChunkArray = array of TCudaMdChunk;

function MdView_ChunkSegments(const aSegs: TCudaMdSegmentList;
  aRenderer: TMarkDownCanvasRenderer; aCanvas: TCanvas): TCudaMdChunkArray;

implementation

uses
  syncobjs,
  StrUtils,
  proc_globdata,
  form_lexer_stylemap;

var
  GSection: TCriticalSection = nil;

  // cache of the last tokenized code block
  GCachedInfo: utf8string = '';
  GCachedCode: utf8string = '';
  GCachedOk: boolean = false;
  GCachedSegments: TCudaMdLineSegments;
  GLastPaintedRuns: array of TCudaMdPaintedRun;

procedure MdView_DebugCaptureRun(ALine: integer; const AText: utf8string;
  ARenderer: TMarkDownCanvasRenderer);
var
  N: integer;
begin
  N:= Length(GLastPaintedRuns);
  SetLength(GLastPaintedRuns, N+1);
  GLastPaintedRuns[N].Line:= ALine;
  GLastPaintedRuns[N].X:= TMarkDownCanvasRendererAccess(ARenderer).Layout.LineX;
  GLastPaintedRuns[N].Y:= TMarkDownCanvasRendererAccess(ARenderer).Layout.LineY;
  GLastPaintedRuns[N].Color:= ARenderer.FontCodeColor;
  GLastPaintedRuns[N].Text:= AText;
end;

function MdView_DebugPaintedRunCount: integer;
begin
  Result:= Length(GLastPaintedRuns);
end;

function MdView_DebugPaintedRun(AIndex: integer): TCudaMdPaintedRun;
begin
  if (AIndex>=0) and (AIndex<Length(GLastPaintedRuns)) then
    Result:= GLastPaintedRuns[AIndex]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

{ Helpers: entity-resolved text of a code line, line counting/splitting }

function ResolveEntitiesText(aBlock: TMarkdownTextBlock): utf8string;
var
  j: integer;
begin
  // plain text of the line, like the component's own code path uses;
  // Nodes can be unassigned (e.g. text parsed without inline processing):
  // fall back to the raw Text then - for code lines they are the same
  Result:= '';
  if not assigned(aBlock.Nodes) then
    exit(aBlock.Text);
  for j:= 0 to aBlock.Nodes.Count-1 do
    Result:= Result + aBlock.Nodes[j].NodeText;
end;

function CountLines(const S: utf8string): integer;
var
  i: integer;
begin
  Result:= 1;
  for i:= 1 to Length(S) do
    if S[i]=#10 then
      Inc(Result);
end;

function LineN(const S: utf8string; N: integer): utf8string;
var
  Start, i, Cnt: integer;
begin
  Start:= 1;
  Cnt:= 0;
  for i:= 1 to Length(S)+1 do
    if (i>Length(S)) or (S[i]=#10) then
    begin
      if Cnt=N then
        exit(Copy(S, Start, i-Start));
      Inc(Cnt);
      Start:= i+1;
    end;
  Result:= '';
end;

{ Width-aware chunking of code lines: the highlighted renderer must wrap
  a source line that is wider than the layout width (the "fancy code
  blocks do not wrap" bug): FlushTextRun alone lays the whole run out and
  never checks MaxWidth, so a long line became one item wider than the
  window - its tail was clipped/invisible. This mirrors the wrapping the
  upstream renderer now does for fenced code (LayoutFencedCodeLine), but
  per SEGMENT, keeping each token its lexer color across the wrap.
  The returned chunk list is ONE ROW's worth of pieces: when a chunk
  would not fit the remaining row width, the current row is closed and
  the caller must NewLine before painting the rest (chunks are returned
  in order; each chunk fits the FULL row width, UTF-8 sequences are
  never split, color/style stay attached to their characters). }

function MdView_NextUTF8LenAt(const S: utf8string; P: integer): integer;
begin
  if P>Length(S) then
    exit(0);
  if Byte(S[P])<$80 then
    Result:= 1
  else if (Byte(S[P]) and $e0)=$c0 then
    Result:= 2
  else if (Byte(S[P]) and $f0)=$e0 then
    Result:= 3
  else
    Result:= 4;
  if P+Result>Length(S)+1 then
    Result:= 1; // truncated sequence: take it byte-wise
end;

function MdView_ChunkSegments(const aSegs: TCudaMdSegmentList;
  aRenderer: TMarkDownCanvasRenderer; aCanvas: TCanvas): TCudaMdChunkArray;
var
  Vis: TMarkDownCanvasRendererAccess absolute aRenderer;
  Chunk: utf8string;
  ChunkColor: TColor;
  ChunkStyles: TFontStyles;
  ChunkW, W, Room: integer;
  i, P, L, N: integer;
begin
  Result:= nil;
  Chunk:= '';
  ChunkColor:= clNone;
  ChunkStyles:= [];
  ChunkW:= 0;
  Room:= Vis.Layout.MaxWidth - (Vis.Layout.CurrentIndent + Vis.Layout.LineX);
  for i:= 0 to Length(aSegs)-1 do
  begin
    P:= 1;
    while P<=Length(aSegs[i].Text) do
    begin
      L:= MdView_NextUTF8LenAt(aSegs[i].Text, P);
      if L<=0 then
        L:= 1;
      W:= Vis.MeasureTextWidth(aCanvas, Copy(aSegs[i].Text, P, L),
        aRenderer.BaseFontSize, aSegs[i].Styles, [fcMono, fcFencedCode]);
      if (W>Room) and (Chunk<>'') then
      begin
        // this character does not fit the row anymore: close the row.
        // The caller paints the chunk, calls NewLine, then continues
        // with this character on the next row.
        N:= Length(Result);
        SetLength(Result, N+1);
        Result[N].Text:= Chunk;
        Result[N].Color:= ChunkColor;
        Result[N].Styles:= ChunkStyles;
        Result[N].WrapAfter:= True;
        Chunk:= '';
        ChunkW:= 0;
        // a fresh row starts at LineX=0 after the caller's NewLine
        Room:= Vis.Layout.MaxWidth - Vis.Layout.CurrentIndent;
      end
      else if (W>Room) and (Chunk='') then
      begin
        // degenerate: one character wider than a whole empty row (a
        // pathological font, never in practice); paint it anyway
        Room:= W;
      end;
      if (Chunk<>'') and ((ChunkColor<>aSegs[i].Color) or (ChunkStyles<>aSegs[i].Styles)) then
      begin
        // style change: close the chunk (same row, no wrap)
        N:= Length(Result);
        SetLength(Result, N+1);
        Result[N].Text:= Chunk;
        Result[N].Color:= ChunkColor;
        Result[N].Styles:= ChunkStyles;
        Result[N].WrapAfter:= False;
        Chunk:= '';
        ChunkW:= 0;
      end;
      ChunkColor:= aSegs[i].Color;
      ChunkStyles:= aSegs[i].Styles;
      Chunk:= Chunk + Copy(aSegs[i].Text, P, L);
      ChunkW:= ChunkW + W;
      Dec(Room, W);
      Inc(P, L);
    end;
  end;
  if Chunk<>'' then
  begin
    N:= Length(Result);
    SetLength(Result, N+1);
    Result[N].Text:= Chunk;
    Result[N].Color:= ChunkColor;
    Result[N].Styles:= ChunkStyles;
    Result[N].WrapAfter:= False;
  end;
end;

type
  { TMDClientFreer }

  TMDClientFreer = class(TThread)
  //Frees a retired TecClientSyntAnalyzer (and its text buffer) off the UI
  //thread: the analyzer's destructor joins its parser thread, which polls
  //the termination flag every ~100 msec - too slow to run in a paint cycle
  public
    Client: TecClientSyntAnalyzer;
    Buf: TATStringBuffer;
    procedure Execute; override;
  end;

procedure TMDClientFreer.Execute;
var
  C: TecClientSyntAnalyzer;
  B: TATStringBuffer;
begin
  C:= Client;
  B:= Buf;
  Client:= nil;
  Buf:= nil;
  GSection.Enter;
  try
    C.Free;
    B.Free;
  finally
    GSection.Leave;
  end;
end;

{ Find lexer: alias first, then name, then extension;
  applies the syntax theme like editor tabs do. }

function MdView_FindLexer(const aInfo: utf8string): TecSyntAnalyzer;
{
var
  anNotCorrect: TecSyntAnalyzer;
}
begin
  Result:= nil;
  if aInfo='' then exit;
  if AppManager=nil then exit;
  if AppManager.LexerCount=0 then exit;

  // 1) aliases from data/lexlib/aliases.ini, e.g. 'py' -> Python
  Result:= AppManager.FindLexerByAlias(LowerCase(aInfo));
  if Result<>nil then exit;

  {
  // 2) lexer name, e.g. 'Python', 'C++'
  Result:= AppManager.FindLexerByName(aInfo);
  if Result<>nil then exit;

  // 3) extension, e.g. 'py' as in 'file.py'
  if Pos('/', aInfo)=0 then
    Result:= AppManager.FindLexerByFilename('x.'+aInfo, nil);

  if Result<>nil then
    if Assigned(EControlOptions.OnLexerApplyTheme) then
      EControlOptions.OnLexerApplyTheme(Result, anNotCorrect);
  }
end;

function MdView_Utf8CharLen(B: byte): integer; inline;
begin
  if B<$80 then
    Result:= 1
  else if (B and $E0)=$C0 then
    Result:= 2
  else if (B and $F0)=$E0 then
    Result:= 3
  else
    Result:= 4;
end;

procedure MdView_FreeClientAsync(Client: TecClientSyntAnalyzer; Buf: TATStringBuffer);
var
  Freer: TMDClientFreer;
begin
  if (Client=nil) and (Buf=nil) then exit;
  try
    //create suspended: Execute must not read the fields before they are set
    Freer:= TMDClientFreer.Create(true);
    Freer.FreeOnTerminate:= true;
    Freer.Client:= Client;
    Freer.Buf:= Buf;
    Freer.Start;
  except
    //could not spawn the freer thread: free here, blocking
    FreeAndNil(Freer);
    Client.Free;
    Buf.Free;
  end;
end;

{ Split aCode (UTF-8, #10-separated) into per-line colored segments.
  Called with GSection already entered. }

procedure MdView_FillGaps(const aCode: utf8string;
  const aLineStarts: array of integer; var aSegments: TCudaMdLineSegments);
// Sort each line's segments by Pos and insert gap segments for the bytes
// not covered by any token (leading indentation, whitespace between
// tokens). Gap segments carry clNone color: the renderer paints those with
// the default code color. After this, concat(segments of a line) = the
// line text byte-for-byte, so leading spaces survive the highlighted render.
var
  i, j, k, N, GapStart, GapLen, LineEnd: integer;
  Seg, GapSeg: TCudaMdSegment;
  Segs, Res: TCudaMdSegmentList;
begin
  for i:= 0 to High(aSegments) do
  begin
    Segs:= aSegments[i];

    if Length(Segs)=0 then
    begin
      // line without any styled token: whole line is a gap, if non-empty
      if i<High(aLineStarts) then
        LineEnd:= aLineStarts[i+1]-1 // offset of the line's #10
      else
        LineEnd:= Length(aCode);
      if LineEnd>aLineStarts[i] then
      begin
        GapSeg.Text:= Copy(aCode, aLineStarts[i]+1, LineEnd-aLineStarts[i]);
        GapSeg.Color:= clNone;
        GapSeg.Styles:= [];
        GapSeg.Pos:= aLineStarts[i]+1;
        GapSeg.Len:= LineEnd-aLineStarts[i];
        SetLength(Segs, 1);
        Segs[0]:= GapSeg;
      end;
      aSegments[i]:= Segs;
      continue;
    end;

    // insertion sort by Pos (a line has few tokens)
    for j:= 1 to High(Segs) do
    begin
      Seg:= Segs[j];
      k:= j-1;
      while (k>=0) and (Segs[k].Pos>Seg.Pos) do
      begin
        Segs[k+1]:= Segs[k];
        Dec(k);
      end;
      Segs[k+1]:= Seg;
    end;

    // build the gap-filled result for this line; GapStart is the 0-based
    // offset of the first byte not yet covered
    Res:= nil;
    GapStart:= aLineStarts[i];
    for j:= 0 to High(Segs) do
    begin
      if Segs[j].Pos-1>GapStart then
      begin
        GapLen:= (Segs[j].Pos-1)-GapStart;
        GapSeg.Text:= Copy(aCode, GapStart+1, GapLen);
        GapSeg.Color:= clNone;
        GapSeg.Styles:= [];
        GapSeg.Pos:= GapStart+1;
        GapSeg.Len:= GapLen;
        N:= Length(Res);
        SetLength(Res, N+1);
        Res[N]:= GapSeg;
      end;
      N:= Length(Res);
      SetLength(Res, N+1);
      Res[N]:= Segs[j];
      GapStart:= (Segs[j].Pos-1)+Segs[j].Len;
    end;
    // trailing gap up to the line end (the #10 itself is not painted)
    if i<High(aLineStarts) then
      LineEnd:= aLineStarts[i+1]-1
    else
      LineEnd:= Length(aCode);
    if LineEnd>GapStart then
    begin
      GapSeg.Text:= Copy(aCode, GapStart+1, LineEnd-GapStart);
      GapSeg.Color:= clNone;
      GapSeg.Styles:= [];
      GapSeg.Pos:= GapStart+1;
      GapSeg.Len:= LineEnd-GapStart;
      N:= Length(Res);
      SetLength(Res, N+1);
      Res[N]:= GapSeg;
    end;
    aSegments[i]:= Res;
  end;
end;

procedure MdView_TokenizeToSegments(An: TecSyntAnalyzer; const aCode: utf8string;
  var aSegments: TCudaMdLineSegments);
var
  Buf: TATStringBuffer;
  Client: TecClientSyntAnalyzer;
  Tokens: TecTokenList;
  Token: PecSyntToken;
  St: TecSyntaxFormat;
  StrU16: UnicodeString;
  MapU16ToU8: array of integer;
  LineStarts: array of integer;
  i, N, i8, i16, CharLen, ByteLen: integer;
  NPosU8, NLenU8, NLine, NLineCount: integer;
  Seg: TCudaMdSegment;
begin
  SetLength(aSegments, 0);

  StrU16:= UnicodeString(aCode);
  if StrU16='' then exit;
  ByteLen:= length(aCode);

  // map UTF-16 unit index (0-based) -> UTF-8 byte offset (0-based)
  SetLength(MapU16ToU8, length(StrU16)+1);
  for i:= 0 to High(MapU16ToU8) do
    MapU16ToU8[i]:= ByteLen;
  MapU16ToU8[0]:= 0;

  i8:= 1;
  i16:= 0;
  while i8<=ByteLen do
  begin
    CharLen:= MdView_Utf8CharLen(byte(aCode[i8]));
    Inc(i8, CharLen);
    if CharLen=4 then
      Inc(i16, 2)
    else
      Inc(i16, 1);
    if i16<=High(MapU16ToU8) then
      MapU16ToU8[i16]:= i8-1;
    if (CharLen=4) and (i16-1>0) and (i16-1<=High(MapU16ToU8)) then
      // middle of a surrogate pair: map to the end of the char
      MapU16ToU8[i16-1]:= i8-1;
  end;

  // line starts (UTF-8 byte offsets, 0-based) for bucketing tokens per line
  SetLength(LineStarts, 1);
  LineStarts[0]:= 0;
  NLineCount:= 1;
  for i8:= 1 to ByteLen do
    if aCode[i8]=#10 then
    begin
      SetLength(LineStarts, NLineCount+1);
      LineStarts[NLineCount]:= i8; // offset of char AFTER the #10
      Inc(NLineCount);
    end;

  Buf:= TATStringBuffer.Create;
  Buf.SetupSlow(StrU16);
  Client:= nil;

  try
    try
      Client:= TecClientSyntAnalyzer.Create(An, Buf);
    except
      Client:= nil;
    end;

    if Client<>nil then
    try
      // synchronous full parse, fills PublicData.Tokens
      Client.ParseInThread;

      Tokens:= Client.PublicData.Tokens;
      if Tokens<>nil then
        for i:= 0 to Tokens.Count-1 do
        begin
          if not Tokens.IsIndexValid(i) then Continue;
          Token:= Tokens._GetItemPtr(i);
          if Token=nil then Continue;

          St:= Token^.Style;
          if St=nil then Continue;
          if not (St.FormatType in [ftCustomFont, ftFontAttr, ftColor]) then Continue;
          if St.Font=nil then Continue;
          if St.Font.Color=clNone then Continue;

          if (Token^.Range.StartPos<0) or (Token^.Range.StartPos>High(MapU16ToU8)) then Continue;
          if (Token^.Range.EndPos<0) or (Token^.Range.EndPos>High(MapU16ToU8)) then Continue;
          if Token^.Range.EndPos<=Token^.Range.StartPos then Continue;

          NPosU8:= MapU16ToU8[Token^.Range.StartPos]+1; //1-based
          NLenU8:= MapU16ToU8[Token^.Range.EndPos]-(NPosU8-1);
          if NLenU8<=0 then Continue;
          if NPosU8+NLenU8-1>ByteLen then Continue;
          // a token must stay inside one source line: a token spanning
          // a #10 would paint a newline inside one text run
          if PosEx(#10, aCode, NPosU8)>0 then
            if NPosU8+NLenU8-1>=PosEx(#10, aCode, NPosU8) then Continue;

          // find the line this token starts on
          NLine:= 0;
          while (NLine<NLineCount-1) and (LineStarts[NLine+1]<=NPosU8-1) do
            Inc(NLine);

          if Length(aSegments)<NLine+1 then
            SetLength(aSegments, NLine+1);

          // clip the token to its line end (defensive: even single-line
          // tokens are cut at the #10 so no run can ever carry it)
          if NLine<NLineCount-1 then
            if NPosU8+NLenU8-1>LineStarts[NLine+1] then
              NLenU8:= LineStarts[NLine+1]-(NPosU8-1)-1;
          if NLenU8<=0 then Continue;

          Seg.Text:= Copy(aCode, NPosU8, NLenU8);
          Seg.Color:= St.Font.Color;
          Seg.Styles:= [];
          if St.FormatType in [ftCustomFont, ftFontAttr] then
            Seg.Styles:= St.Font.Style;
          Seg.Pos:= NPosU8;
          Seg.Len:= NLenU8;

          N:= Length(aSegments[NLine]);
          SetLength(aSegments[NLine], N+1);
          aSegments[NLine][N]:= Seg;
        end;

      // lexers do not emit styled tokens for unstyled runs (leading
      // indentation, gaps between tokens): fill those gaps so the
      // segments cover each line byte-for-byte, painted with the
      // default code color by the renderer
      MdView_FillGaps(aCode, LineStarts, aSegments);
    finally
      //free the analyzer in background: its destructor joins the parser
      //thread (polls every ~100 msec), too slow for the UI thread;
      //the buffer goes with it, the client's destructor marks it invalid
      MdView_FreeClientAsync(Client, Buf);
    end;
  except
    //any failure in lexing must not break rendering: plain code lines remain
    SetLength(aSegments, 0);
  end;
end;

function MdView_HighlightCodeBlock(const aInfo: utf8string;
  const aCode: utf8string): TCudaMdLineSegments;
var
  An: TecSyntAnalyzer;
begin
  if GSection=nil then
    GSection:= TCriticalSection.Create;

  GSection.Enter;
  try
    if GCachedOk and (GCachedInfo=aInfo) and (GCachedCode=aCode) then
      exit(GCachedSegments); // hits the copy-out in the caller

    SetLength(GCachedSegments, 0);
    GCachedOk:= false;
    GCachedInfo:= aInfo;
    GCachedCode:= aCode;

    An:= MdView_FindLexer(aInfo);
    if An<>nil then
      MdView_TokenizeToSegments(An, aCode, GCachedSegments);
    //cache also the negative result, to not search for the lexer again
    GCachedOk:= true;
    Result:= GCachedSegments;
  finally
    GSection.Leave;
  end;
end;

{ TCudaMarkdownCodeBlockRenderer }

procedure TCudaMarkdownCodeBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lCode: TMarkdownCodeBlock absolute aBlock;
  lCodeU8: utf8string;
  lSegments: TCudaMdLineSegments;
  lChunks: array of TCudaMdChunk;
  lSavedFontColor: TColor;
  lLine: utf8string;
  lItem: TLayoutItem;
  i, j: integer;
begin
  if not assigned(aBlock) then exit;

  // plain rendering when disabled, not fenced, or no language tag
  if (not MdViewCodeSyntaxEnabled) or (lCode.Lang='') then
  begin
    inherited DoRender(aBlock);
    exit;
  end;

  // gather the code text, like TMarkDownCanvasRenderer.RenderTextNode
  // would: children are TMarkdownTextBlock, text resolved the same way
  lCodeU8:= '';
  for i:= 0 to lCode.ChildCount-1 do
    if lCode.Children[i] is TMarkdownTextBlock then
      lCodeU8:= lCodeU8 + ResolveEntitiesText(TMarkdownTextBlock(lCode.Children[i])) + #10;
  // last line is empty after the trailing #10: drop it
  if (lCodeU8<>'') and (lCodeU8[Length(lCodeU8)]=#10) then
    SetLength(lCodeU8, Length(lCodeU8)-1);

  lSegments:= MdView_HighlightCodeBlock(lCode.Lang, lCodeU8);

  // paint lines like upstream's fenced-code branch (RenderTextNode ->
  // FlushTextRun) BUT width-aware: a source line wider than the layout
  // is WRAPPED at character boundaries (bug "fancy code blocks do not
  // wrap: line bigger than the window, tail invisible"), keeping each
  // lexer token its color across the wrap. Gap segments (clNone) keep
  // the default code color. Per ROW also the background rect is
  // created, like upstream does, so the code block keeps its shaded
  // background and rows stay visually separated. Segments never
  // contain #10: a token must not paint a newline inside one
  // FlushTextRun run.
  lSavedFontColor:= CanvasRenderer.FontCodeColor;
  SetLength(GLastPaintedRuns, 0);

  ParagraphBreak;
  for i:= 0 to CountLines(lCodeU8)-1 do
  begin
    lLine:= LineN(lCodeU8, i);
    if (Length(lSegments)<=i) or (Length(lSegments[i])=0) then
    begin
      // line beyond the tokenized range, or line without tokens: default
      // code color, still byte-faithful, WRAPPED like upstream's fenced
      // path (LayoutFencedCodeLine) so long lines stay visible
      lItem:= TMarkDownCanvasRendererAccess(CanvasRenderer).CreateLayoutItem(
        likBackground,
        TMarkDownCanvasRendererAccess(CanvasRenderer).Layout.LineX,
        TMarkDownCanvasRendererAccess(CanvasRenderer).Layout.LineY);
      lItem.BGColor:= TMarkDownCanvasRendererAccess(CanvasRenderer).GetTextBGColor([fcFencedCode]);
      lItem.Width:= TMarkDownCanvasRendererAccess(CanvasRenderer).Layout.MaxWidth - lItem.X;
      if lLine<>'' then
      begin
        MdView_DebugCaptureRun(i, lLine, CanvasRenderer);
        TMarkDownCanvasRendererAccess(CanvasRenderer).LayoutFencedCodeLine(Canvas,
          lLine, CanvasRenderer.BaseFontSize, [], [fcMono, fcFencedCode]);
      end;
      NewLine;
      lItem.Height:= TMarkDownCanvasRendererAccess(CanvasRenderer).Layout.LineY - lItem.Y;
      continue;
    end;

    // background rect for the FIRST row of this line: covers every
    // wrapped row, because Height is finalized after all rows below
    lItem:= TMarkDownCanvasRendererAccess(CanvasRenderer).CreateLayoutItem(
      likBackground,
      TMarkDownCanvasRendererAccess(CanvasRenderer).Layout.LineX,
      TMarkDownCanvasRendererAccess(CanvasRenderer).Layout.LineY);
    lItem.BGColor:= TMarkDownCanvasRendererAccess(CanvasRenderer).GetTextBGColor([fcFencedCode]);
    lItem.Width:= TMarkDownCanvasRendererAccess(CanvasRenderer).Layout.MaxWidth - lItem.X;

    // paint the line's segments as width-aware chunks: a WrapAfter
    // chunk closes the current row; the rest continues on the next row
    lChunks:= MdView_ChunkSegments(lSegments[i], CanvasRenderer, Canvas);
    for j:= 0 to Length(lChunks)-1 do
    begin
      if lChunks[j].Color=clNone then
        CanvasRenderer.FontCodeColor:= lSavedFontColor
      else
        CanvasRenderer.FontCodeColor:= lChunks[j].Color;
      MdView_DebugCaptureRun(i, lChunks[j].Text, CanvasRenderer);
      TMarkDownCanvasRendererAccess(CanvasRenderer).FlushTextRun(Canvas,
        lChunks[j].Text,
        CanvasRenderer.BaseFontSize, lChunks[j].Styles, '',
        [fcMono, fcFencedCode], False);
      if lChunks[j].WrapAfter then
        NewLine;
    end;

    NewLine;
    lItem.Height:= TMarkDownCanvasRendererAccess(CanvasRenderer).Layout.LineY - lItem.Y;
  end;

  CanvasRenderer.FontCodeColor:= lSavedFontColor;
  ParagraphBreak;
end;

class function TCudaMarkdownCodeBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:= TMarkdownCodeBlock;
end;


initialization
  // register for the canvas renderer class used by TMarkDownControl:
  // it resolves block renderers through TMarkDownCanvasRenderer
  TCudaMarkdownCodeBlockRenderer.RegisterRenderer(TMarkDownCanvasRenderer);

end.
