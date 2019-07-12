unit Image32_Draw;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.03                                                            *
* Date      :  13 July 2019                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Polygon renderer for TImage32                                   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Windows, Math, Image32, Image32_Vector;

type
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);

  TGradientFillStyle = (gfsClamp, gfsMirror, gfsRepeat);
  TTileFillStyle     = (tfsRepeat, tfsMirrorHorz, tfsMirrorVert, tfsRotate180);

  //TBoundsProc: Function template for TCustomRenderer.
  TBoundsProc = function(X, startX, endX: integer): integer;

  TCustomRenderer = class {$IFDEF ABSTRACT_CLASSES} abstract {$ENDIF}
  private
    fImage: TImage32;
    fLastX: integer;
    fLastY: integer;
    fDst: PColor32;
  protected
    function Initialize(targetImage: TImage32): Boolean; virtual;
    function GetDstPixel(x,y: integer): PColor32;

    property Target: TImage32 read fImage;
  public
    //RenderProc: x & y refer to pixel coords in the destination image and
    //where x1 is the start (and left) and x2 is the end of the render
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); virtual; abstract;
  end;

  TColorRenderer = class(TCustomRenderer)
  private
    fAlphaTbl: PByteArray;
    fColor: TColor32;
  public
    constructor Create(color: TColor32 = clNone32);
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetColor(value: TColor32);
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  end;

  TImageRenderer = class(TCustomRenderer)
  private
    fImage        : TImage32;
    fOffset       : TPoint;
    fBrushPixel   :  PARGB;
    fLastXX       : integer;
    fLastYY       : integer;
    fMirrorY      : Boolean;
    fBoundsProc   : TBoundsProc;
    function GetFirstBrushPixel(x, y: integer): PARGB;
    function GetNextBrushPixel: PARGB;
  public
    constructor Create(tileFillStyle: TTileFillStyle = tfsRepeat;
      brushImage: TImage32 = nil);
    destructor Destroy; override;
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetTileFillStyle(value: TTileFillStyle);
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
    property Image: TImage32 read fImage;
    property Offset: TPoint read fOffset write fOffset;
  end;

  TLinearGradientRenderer = class(TCustomRenderer)
  private
    fStartPt    : TPointD;
    fEndPt      : TPointD;
    fStartColor : TColor32;
    fEndColor   : TColor32;
    fColors     : TArrayOfColor32;
    fPerpOffsets: TArrayOfInteger;
    fBoundsProc : TBoundsProc;
    fEndDist    : integer;
    fIsVert     : Boolean;
  public
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetParameters(const startPt, endPt: TPointD;
      startColor, endColor: TColor32;
      gradFillStyle: TGradientFillStyle = gfsClamp);
    procedure SetGradientFillStyle(value: TGradientFillStyle);
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  end;

  TRadialGradientRenderer = class(TCustomRenderer)
  private
    fFocalPt   : TPointD;
    fScaleX     : double;
    fScaleY     : double;
    fMaxColors : integer;
    fInnerColor: TColor32;
    fOuterColor: TColor32;
    fColors    : TArrayOfColor32;
    fBoundsProc: TBoundsProc;
  public
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetParameters(const focalRect: TRect;
      innerColor, outerColor: TColor32;
      gradientFillStyle: TGradientFillStyle = gfsClamp);
    procedure SetGradientFillStyle(value: TGradientFillStyle);
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  end;

  ///////////////////////////////////////////////////////////////////////////
  // DRAWING FUNCTIONS
  ///////////////////////////////////////////////////////////////////////////

  procedure DrawPoint(img: TImage32; const pt: TPointD;
    width: double; color: TColor32); overload;
  procedure DrawPoint(img: TImage32; const pt: TPointD;
    width: double; renderer: TCustomRenderer); overload;

  procedure DrawLine(img: TImage32;
    const line: TArrayOfPointD; lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawLine(img: TImage32;
    const line: TArrayOfPointD; lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawLine(img: TImage32; const lines: TArrayOfArrayOfPointD;
    lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawLine(img: TImage32; const lines: TArrayOfArrayOfPointD;
    lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;

  function DrawDashedLine(img: TImage32; const line: TArrayOfPointD;
    dashPattern: TArrayOfInteger; patternOffset: double;
    lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto): double; overload;
  function DrawDashedLine(img: TImage32; const lines: TArrayOfArrayOfPointD;
    dashPattern: TArrayOfInteger; patternOffset: double;
    lineWidth: double; color: TColor32; endStyle: TEndStyle;
    joinStyle: TJoinStyle = jsAuto): double; overload;
  function DrawDashedLine(img: TImage32; const line: TArrayOfPointD;
    dashPattern: TArrayOfInteger; patternOffset: double;
    lineWidth: double; renderer: TCustomRenderer; endStyle: TEndStyle;
    joinStyle: TJoinStyle = jsAuto): double; overload;
  function DrawDashedLine(img: TImage32; const lines: TArrayOfArrayOfPointD;
    dashPattern: TArrayOfInteger; patternOffset: double;
    lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto): double; overload;

  procedure DrawPolygon(img: TImage32; const polygon: TArrayOfPointD;
    fillRule: TFillRule; color: TColor32); overload;
  procedure DrawPolygon(img: TImage32; const polygon: TArrayOfPointD;
    fillRule: TFillRule; renderer: TCustomRenderer); overload;
  procedure DrawPolygon(img: TImage32; const polygons: TArrayOfArrayOfPointD;
    fillRule: TFillRule; color: TColor32); overload;
  procedure DrawPolygon(img: TImage32; const polygons: TArrayOfArrayOfPointD;
    fillRule: TFillRule; renderer: TCustomRenderer); overload;

  ///////////////////////////////////////////////////////////////////////////
  // MISCELLANEOUS FUNCTIONS
  ///////////////////////////////////////////////////////////////////////////

  procedure SetGamma(gamma: double);

  procedure Rasterize(const paths: TArrayOfArrayOfPointD;
    const clipRec: TRect; fillRule: TFillRule; renderer: TCustomRenderer);

implementation

type
  TArray256Bytes = array[0..255] of byte;

  //A horizontal scanline contains any number of line fragments. A fragment
  //can be a number of pixels wide but it can't be more than one pixel high.

  TFragment = record
    botX, topX, dy, dydx: double;
  end;
  TFragmentArray = array[0 .. (Maxint div SizeOf(TFragment)) -1] of TFragment;
  PFragments = ^TFragmentArray;
  PFragment = ^TFragment;

  TScanLine = record
    Y: integer;
    minX, maxX: integer;
    count: integer;
    fragments: PFragments;
  end;
  PScanline = ^TScanline;
  TArrayOfScanline = array of TScanline;

  TDoubletArray = array[0 .. (Maxint div SizeOf(Double)) -1] of Double;
  PDoubleArray = ^TDoubletArray;

var
  gammaTable: TArray256Bytes;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetPixel(current: PARGB; delta: integer): PARGB;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := current;
  inc(Result, delta);
end;
//------------------------------------------------------------------------------

procedure SwapColors(var color1, color2: TColor32);
var
  c: TColor32;
begin
  c := color1;
  color1 := color2;
  color2 := c;
end;
//------------------------------------------------------------------------------

procedure SwapPoints(var point1, point2: TPoint); overload;
var
  pt: TPoint;
begin
  pt := point1;
  point1 := point2;
  point2 := pt;
end;
//------------------------------------------------------------------------------

procedure SwapPoints(var point1, point2: TPointD); overload;
var
  pt: TPointD;
begin
  pt := point1;
  point1 := point2;
  point2 := pt;
end;
//------------------------------------------------------------------------------

function ClampQ(q, startQ, endQ: integer): integer;
begin
  if q < startQ then result := 0
  else if q > endQ then result := endQ - startQ
  else result := q - startQ;
end;
//------------------------------------------------------------------------------

function MirrorQ(q, startQ, endQ: integer): integer;
var
  dx: integer;
begin
  q := q - startQ;
  dx := endQ - StartQ +1;
  result := q mod dx;
  if (result < 0) then inc(result, dx);
  if Odd(Floor(q / dx)) then
    result := dx - result -1;
end;
//------------------------------------------------------------------------------

function RepeatQ(q, startQ, endQ: integer): integer;
var
  dx: integer;
begin
  if (q < startQ) or (q > endQ) then
  begin
    dx := Abs(endQ - startQ) +1;
    result := (q - startQ) mod dx;
    if result < 0 then inc(result, dx);
  end
  else result := q - startQ;
end;
//------------------------------------------------------------------------------

function SoftRepeat(q, startQ, endQ: integer): integer;
var
  dx: integer;
begin
  if (q < startQ) or (q > endQ) then
  begin
    dx := Abs(endQ - startQ) +1;
    result := (q - startQ) mod dx;
    if result < 0 then inc(result, dx);
    //exactly where the colors repeat, average the two colors
    if result = 0 then result := dx div 2;
  end else
  begin
    result := q - startQ;
    if result <> 0 then Exit;
    //soft blend at startQ too
    dx := Abs(endQ - startQ) +1;
    result := dx div 2;
  end;
end;
//------------------------------------------------------------------------------

function SoftRepeatExStart(q, startQ, endQ: integer): integer;
var
  dx: integer;
begin
  if (q < startQ) or (q > endQ) then
  begin
    dx := Abs(endQ - startQ) +1;
    result := (q - startQ) mod dx;
    if result < 0 then inc(result, dx);
    if result = 0 then result := dx div 2;
  end else
    result := q - startQ;
end;
//------------------------------------------------------------------------------

function BlendColorUsingMask(bgColor, fgColor: TColor32; mask: Byte): TColor32;
var
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
  res: TARGB absolute Result;
  R, invR: PByteArray;
begin
  if (mask = 0) then
    Result := bgColor
  else if (mask = 255) then
    Result := fgColor
  else
  begin
    R    := @MulTable[mask];
    InvR := @MulTable[not mask];
    res.A := R[fg.A] + InvR[bg.A];
    res.R := R[fg.R] + InvR[bg.R];
    res.G := R[fg.G] + InvR[bg.G];
    res.B := R[fg.B] + InvR[bg.B];
  end;
end;
//------------------------------------------------------------------------------

function GetLinearColorGradient(color1, color2: TColor32;
  length: integer): TArrayOfColor32;
var
  i, highI: integer;
begin
  SetLength(result, length);
  if length = 0 then Exit;
  highI := length - 1;
  result[0] := color1;
  result[highI] := color2;
  for i := 1 to highI -1 do
    result[i] := BlendColorUsingMask(color1, color2, (i * 255) div highI);
end;
//------------------------------------------------------------------------------

procedure SetGamma(gamma: double);
var
  i: integer;
const
  inv255: double = 1/255;
begin
  if gamma < 0.1 then gamma := 0.1
  else if gamma > 3.0 then gamma := 3;
  if gamma = 1 then
    for i := 0 to 255 do gammaTable[i] := i
  else
    for i := 0 to 255 do
      gammaTable[i] := Min(255, Round(255 * power(i* inv255, gamma)));
end;
//------------------------------------------------------------------------------

function ApplyGamma(color: TColor32): TColor32;
var
  res: TARGB absolute Result;
begin
  result := color;
  res.R := gammaTable[res.R];
  res.G := gammaTable[res.G];
  res.B := gammaTable[res.B];
end;

//------------------------------------------------------------------------------
// Rasterize() support functions
//------------------------------------------------------------------------------

procedure AllocateScanlines(const polygons: TArrayOfArrayOfPointD;
  var scanlines: TArrayOfScanline; clipBottom, clipRight: integer);
var
  i,j, highI, highJ: integer;
  y1, y2: integer;
  psl: PScanline;
begin
  //first count how often each edge intersects with each horizontal scanline
  for i := 0 to high(polygons) do
  begin
    highJ := high(polygons[i]);
    if highJ < 2 then continue;
    y1 := Round(polygons[i][highJ].Y); //nb: Round is much faster than Floor
    for j := 0 to highJ do             //    and a little faster than Trunc
    begin
      y2 := Round(polygons[i][j].Y);
      if y1 < y2 then
      begin
        //descending
        if (y2 >= 0) and (y1 <= clipBottom) then
        begin
          if (y1 > 0) and (y1 <= clipBottom)  then
            dec(scanlines[y1 -1].count);
          if y2 >= clipBottom then
            inc(scanlines[clipBottom].count) else
            inc(scanlines[y2].count);
        end;
      end else
      begin
        //ascending
        if (y1 >= 0) and (y2 <= clipBottom) then
        begin
          if (y2 > 0) then
            dec(scanlines[y2 -1].count);
          if y1 >= clipBottom then
            inc(scanlines[clipBottom].count) else
            inc(scanlines[y1].count);
        end;
      end;
      y1 := y2;
    end;
  end;

  //convert 'count' accumulators into real counts and allocate storage
  j := 0;
  highI := high(scanlines);
  psl := @scanlines[highI];
  for i := highI downto 0 do
  begin
    inc(j, psl.count);
    //nb: 1. GetMem is faster than a dynamic array as it's not initialized
    //    2. psl.fragments is zero initialized by SetLength()
    if j > 0 then
      GetMem(psl.fragments, j * SizeOf(TFragment));
    psl.count := 0;
    psl.minX := clipRight;
    psl.maxX := 0;
    psl.Y := i;
    dec(psl);
  end;
end;
//------------------------------------------------------------------------------

procedure SplitEdgeIntoFragments(const pt1, pt2: TPointD;
  const scanlines: TArrayOfScanline; const clipRec: TRect);
var
  x,y, dx,dy, absDx, dydx, dxdy: double;
  i, scanlineY, maxY, maxX: integer;
  psl: PScanLine;
  pFrag: PFragment;
  bot, top: TPointD;
begin
  dy := pt1.Y - pt2.Y;
  dx := pt2.X - pt1.X;
  maxY := RectHeight(clipRec);
  maxX := RectWidth(clipRec);
  absDx := abs(dx);

  if dy > 0 then
  begin
    //ASCENDING EDGE (+VE WINDING DIR)
    if dy < 0.0001 then Exit;            //ignore near horizontals
    bot := pt1; top := pt2;
    //exclude edges that are completely outside the top or bottom clip region
    if (top.Y >= maxY) or (bot.Y <= 0) then Exit;
  end else
  begin
    //DESCENDING EDGE (-VE WINDING DIR)
    if dy > -0.0001 then Exit;           //ignore near horizontals
    bot := pt2; top := pt1;
    //exclude edges that are completely outside the top or bottom clip region
    if (top.Y >= maxY) or (bot.Y <= 0) then Exit;
  end;

  if absDx < 0.000001 then
  begin
    //VERTICAL EDGE
    top.X := bot.X; //this circumvents v. rare rounding issues.

    //exclude vertical edges that are outside the right clip region
    //but still update maxX for each scanline the edge passes
    if bot.X > maxX then
    begin
      for i := Min(maxY, Round(bot.Y)) downto Max(0, Round(top.Y)) do
        scanlines[i].maxX := maxX;
      Exit;
    end;

    dxdy := 0;
    if dy > 0 then dydx := 1 else dydx := -1;
  end else
  begin
    dxdy := dx/dy;
    dydx := dy/absDx;
  end;

  //TRIM EDGES THAT CROSS CLIPPING BOUNDARIES (EXCEPT THE LEFT BOUNDARY)
  if bot.X > maxX then
  begin
    if top.X > maxX then
    begin
      for i := Min(maxY, Round(bot.Y)) downto Max(0, Round(top.Y)) do
        scanlines[i].maxX := maxX;
      Exit;
    end;
    //here the edge must be oriented bottom-right to top-left
    y := bot.Y - (bot.X - maxX) * Abs(dydx);
    for i := Min(maxY, Round(bot.Y)) downto Max(0, Round(y)) do
      scanlines[i].maxX := maxX;
    bot.Y := y;
    if bot.Y <= 0 then Exit;
    bot.X := maxX;
  end
  else if top.X > maxX then
  begin
    //here the edge must be oriented bottom-left to top-right
    y := top.Y + (top.X - maxX) * Abs(dydx);
    for i := Min(maxY, Round(y)) downto Max(0, Round(top.Y)) do
      scanlines[i].maxX := maxX;
    top.Y := y;
    if top.Y >= maxY then Exit;
    top.X := maxX;
  end;
  if bot.Y > maxY then
  begin
    bot.X := bot.X + dxdy * (bot.Y - maxY);
    if (bot.X > maxX) then Exit;        //nb: no clipping on the left
    bot.Y := maxY;
  end;
  if top.Y < 0 then
  begin
    top.X := top.X + (dxdy * top.Y);
    if (top.X > maxX) then Exit;        //nb: no clipping on the left
    top.Y := 0;
  end;

  //SPLIT THE EDGE INTO MULTIPLE SCANLINE FRAGMENTS
  scanlineY := Round(bot.Y);
  if bot.Y = scanlineY then dec(scanlineY);
  //at the lower-most extent of the edge 'split' the first fragment
  psl := @scanlines[scanlineY];
  pFrag := @psl.fragments[psl.count];
  inc(psl.count);
  pFrag.botX := bot.X;
  if scanlineY <= top.Y then
  begin
    //the whole edge is within 1 scanline
    pFrag.topX := top.X;
    pFrag.dy := bot.Y - top.Y;
    pFrag.dydx := dydx;
    Exit;
  end;
  x := bot.X + (bot.Y - scanlineY) * dxdy;
  pFrag.topX := x;
  pFrag.dy := bot.Y - scanlineY;
  pFrag.dydx := dydx;
  //'split' subsequent fragments until the top fragment
  dec(psl);
  while psl.Y > top.Y do
  begin
    pFrag := @psl.fragments[psl.count];
    inc(psl.count);
    pFrag.botX := x;
    x := x + dxdy;
    pFrag.topX := x;
    pFrag.dy := 1;
    pFrag.dydx := dydx;
    dec(psl);
  end;
  //and finally the top fragment
  pFrag := @psl.fragments[psl.count];
  inc(psl.count);
  pFrag.botX := x;
  pFrag.topX := top.X;
  pFrag.dy := psl.Y + 1 - top.Y;
  pFrag.dydx := dydx;
end;
//------------------------------------------------------------------------------

procedure InitializeScanlines(var polygons: TArrayOfArrayOfPointD;
  const scanlines: TArrayOfScanline; const clipRec: TRect);
var
  i,j, highJ: integer;
  pt, pt2: PPointD;
begin
 for i := 0 to high(polygons) do
  begin
    highJ := high(polygons[i]);
    if highJ < 2 then continue;
    pt := @polygons[i][highJ];
    pt2 := @polygons[i][0];
    for j := 0 to highJ do
    begin
      SplitEdgeIntoFragments(pt^, pt2^, scanlines, clipRec);
      pt := pt2;
      inc(pt2);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure ProcessScanlineFragments(var scanline: TScanLine;
  var buffer: TArrayOfDouble);
var
  i,j, leftXi,rightXi, windDir: integer;
  fracX, yy, q: double;
  pd: PDouble;
  frag: PFragment;
begin
  frag := @scanline.fragments[0];
  for i := 1 to scanline.count do
  begin

    //if frag.botX > frag.topX then swap. (Simplifies code)
    if frag.botX > frag.topX then
    begin
      q := frag.botX;
      frag.botX := frag.topX;
      frag.topX  := q;
    end;

    leftXi := Max(0, Round(frag.botX));
    rightXi := Max(0, Round(frag.topX));

    //winding direction is stored as the sign of dydx.
    if frag.dydx < 0 then windDir := -1 else windDir := 1;

    if (leftXi = rightXi) then
    begin
      //the fragment is only one pixel wide
      if leftXi < scanline.minX then
        scanline.minX := leftXi;
      if rightXi > scanline.maxX then
        scanline.maxX := rightXi;
      pd := @buffer[leftXi];
      if (frag.botX <= 0) then
      begin
        pd^ := pd^ + frag.dy * windDir;
      end else
      begin
        q := (frag.botX + frag.topX) * 0.5 - leftXi;
        pd^ := pd^ + (1-q) * frag.dy * windDir;
        inc(pd);
        pd^ := pd^ + q * frag.dy * windDir;
      end;
    end else
    begin
      if leftXi < scanline.minX then
        scanline.minX := leftXi;
      if rightXi > scanline.maxX then
        scanline.maxX := rightXi;
      pd := @buffer[leftXi];
      //left pixel
      fracX := leftXi + 1 - frag.botX;
      yy := frag.dydx * fracX;
      q := fracX * yy * 0.5;
      pd^ := pd^ + q;
      q :=  yy - q;
      inc(pd);
      //middle pixels
      for j := leftXi +1 to rightXi -1 do
      begin
        pd^ := pd^ + q + frag.dydx * 0.5;
        q := frag.dydx * 0.5;
        inc(pd);
      end;
      //right pixel
      fracX := frag.topX - rightXi;
      yy :=  fracX * frag.dydx;
      pd^ := pd^ + q + (1 - fracX * 0.5) * yy;
      inc(pd);
      //overflow
      pd^ := pd^ + fracX * 0.5 * yy;
    end;
    inc(frag);
  end;
end;
//------------------------------------------------------------------------------

{$IF COMPILERVERSION < 23} //TRoundingMode - added in Delphi XE2
type
  TRoundingMode = Math.TFPURoundingMode;
{$IFEND}

procedure Rasterize(const paths: TArrayOfArrayOfPointD;
  const clipRec: TRect; fillRule: TFillRule; renderer: TCustomRenderer);
var
  i,j, xli, xri, maxW, maxH, aa: integer;
  clipRec2: TRect;
  paths2: TArrayOfArrayOfPointD;
  accum: double;
  windingAccum: TArrayOfDouble;
  byteBuffer: TArrayOfByte;
  scanlines: TArrayOfScanline;
  scanline: PScanline;
  rm: TRoundingMode;
begin
  //See also https://nothings.org/gamedev/rasterize/

  if not assigned(renderer) then Exit;
  rm := SetRoundMode(rmDown); //because this is little faster than Trunc.
  Windows.IntersectRect(clipRec2, clipRec, GetBounds(paths));
  paths2 := OffsetPath(paths, -clipRec2.Left, -clipRec2.Top);
  maxW := RectWidth(clipRec2);
  maxH := RectHeight(clipRec2);
  SetLength(scanlines, maxH +1);
  SetLength(windingAccum, maxW +2);
  AllocateScanlines(paths2, scanlines, maxH, maxW-1);
  InitializeScanlines(paths2, scanlines, clipRec2);
  SetLength(byteBuffer, RectWidth(clipRec2));

  scanline := @scanlines[0];
  for i := 0 to high(scanlines) do
  begin
    if scanline.count = 0 then
    begin
      FreeMem(scanline.fragments);
      inc(scanline);
      Continue;
    end;

    //process each scanline to fill the winding count accumulation buffer
    ProcessScanlineFragments(scanline^, windingAccum);
    //it's faster to process only the modified sub-array of windingAccum
    xli := scanline.minX;
    xri := Min(maxW -1, scanline.maxX +1);
    FillChar(byteBuffer[xli], xri - xli +1, 0);

    accum := 0; //winding count accumulator
    for j := xli to xri do
    begin
      accum := accum + windingAccum[j];
      case fillRule of
        frEvenOdd:
          begin
            aa := Round(Abs(accum) * 255) and $1FF;
            if aa >= $100 then aa := aa xor $1ff;
            byteBuffer[j] := aa;
          end;
        frNonZero:
          begin
            byteBuffer[j] := Min($FF, Round(Abs(accum) * 255));
          end;
        frPositive:
          begin
            if accum > 0.002 then
              byteBuffer[j] := Min($FF, Round(accum * 255));
          end;
        frNegative:
          begin
            if accum < -0.002 then
              byteBuffer[j] := Min($FF, Round(-accum * 255));
          end;
      end;
    end;
    renderer.RenderProc(clipRec2.Left + xli, clipRec2.Left + xri,
      clipRec2.Top + i, @byteBuffer[xli]);

    //cleanup and deallocate memory
    FillChar(windingAccum[xli], (xri - xli +1) * sizeOf(Double), 0);
    FreeMem(scanline.fragments);
    inc(scanline);
  end;
  SetRoundMode(rm);
end;

//------------------------------------------------------------------------------
// TAbstractRenderer
//------------------------------------------------------------------------------

function TCustomRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  result := true;
  fImage := targetImage;
  fDst := targetImage.PixelBase;
  fLastY := 0; fLastX := 0;
end;
//------------------------------------------------------------------------------

function TCustomRenderer.GetDstPixel(x, y: integer): PColor32;
begin
  if (y <> fLastY) then
  begin
    fDst := @fImage.Pixels[y * fImage.Width + x];
    fLastY := y;
  end else
    inc(fDst, x - fLastX);
  fLastX := x;
  Result := fDst;
end;

//------------------------------------------------------------------------------
// TBrushColorRenderer
//------------------------------------------------------------------------------

constructor TColorRenderer.Create(color: TColor32 = clNone32);
begin
  if color <> clNone32 then SetColor(color);
end;
//------------------------------------------------------------------------------

function TColorRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  //there's no point rendering if the color is fully transparent
  result := inherited Initialize(targetImage) and assigned(fAlphaTbl);
end;
//------------------------------------------------------------------------------

procedure TColorRenderer.SetColor(value: TColor32);
begin
  fColor := value and $FFFFFF;
  fAlphaTbl := @MulTable[value shr 24];
end;
//------------------------------------------------------------------------------

procedure TColorRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dst: PColor32;
begin
  dst := GetDstPixel(x1,y);
  for i := x1 to x2 do
  begin
    //Ord() used here since old compilers require PByte to be defined as PChar
    dst^ := BlendToAlpha(dst^, fAlphaTbl[Ord(alpha^)] shl 24 or fColor);
    inc(dst); inc(alpha);
  end;
end;

//------------------------------------------------------------------------------
// TBrushImageRenderer
//------------------------------------------------------------------------------

constructor TImageRenderer.Create(tileFillStyle: TTileFillStyle;
  brushImage: TImage32);
begin
  fImage := TImage32.Create(brushImage);
  SetTileFillStyle(tileFillStyle);
end;
//------------------------------------------------------------------------------

destructor TImageRenderer.Destroy;
begin
  fImage.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TImageRenderer.SetTileFillStyle(value: TTileFillStyle);
begin
  case value of
    tfsRepeat: fBoundsProc := RepeatQ;
    tfsMirrorHorz: fBoundsProc := MirrorQ;
    tfsMirrorVert: fBoundsProc := RepeatQ;
    tfsRotate180 : fBoundsProc := MirrorQ;
  end;
  fMirrorY := value in [tfsMirrorVert, tfsRotate180];
end;
//------------------------------------------------------------------------------

function TImageRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  result := inherited and (not fImage.IsEmpty);
  if not result then Exit;
  fLastXX := 0; fLastYY := 0;
  fBrushPixel := PARGB(fImage.PixelBase);
end;
//------------------------------------------------------------------------------

procedure TImageRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  pDst: PColor32;
  pBrush: PARGB;
begin
  pDst := GetDstPixel(x1,y);
  pBrush := GetFirstBrushPixel(x1, y);
  for i := x1 to x2 do
  begin
    pDst^ := BlendToAlpha(pDst^,
      MulTable[pBrush.A, Ord(alpha^)] shl 24 or (pBrush.Color and $FFFFFF));
    inc(pDst); inc(alpha); pBrush := GetNextBrushPixel;
  end;
end;
//------------------------------------------------------------------------------

function TImageRenderer.GetFirstBrushPixel(x, y: integer): PARGB;
var
  w: integer;
begin
  w := fImage.Width;
  dec(x, fOffset.X);
  x := fBoundsProc(x, 0, w -1);
  dec(y, fOffset.Y);
  if fMirrorY then
    y := MirrorQ(y, 0, fImage.Height -1) else
    y := RepeatQ(y, 0, fImage.Height -1);
  if y <> fLastYY then
  begin
    fBrushPixel := PARGB(@fImage.Pixels[y * w]);
    fLastYY := y;
  end;
  fLastXX := x;
  result := GetPixel(fBrushPixel, x);
end;
//------------------------------------------------------------------------------

function TImageRenderer.GetNextBrushPixel: PARGB;
begin
  inc(fLastXX);
  result := GetPixel(fBrushPixel,
    fBoundsProc(fLastXX, 0, fImage.Width -1));
end;

//------------------------------------------------------------------------------
// TLinearGradientRenderer
//------------------------------------------------------------------------------

procedure TLinearGradientRenderer.SetParameters(const startPt, endPt: TPointD;
  startColor, endColor: TColor32; gradFillStyle: TGradientFillStyle);
begin
  SetGradientFillStyle(gradFillStyle);
  fStartPt := startPt;
  fStartColor := startColor;
  fEndPt := endPt;
  fEndColor := endColor;
end;
//------------------------------------------------------------------------------

procedure TLinearGradientRenderer.SetGradientFillStyle(value: TGradientFillStyle);
begin
  case value of
    gfsClamp: fBoundsProc := ClampQ;
    gfsMirror: fBoundsProc := MirrorQ;
    else fBoundsProc := SoftRepeat;
  end;
end;
//------------------------------------------------------------------------------

function TLinearGradientRenderer.Initialize(targetImage: TImage32): Boolean;
var
  i: integer;
  dx,dy, dxdy,dydx: double;
begin
  result := inherited;
  if not result then Exit;

  if abs(fEndPt.Y - fStartPt.Y) > abs(fEndPt.X - fStartPt.X) then
  begin
    //gradient ==> vertical
    if (fEndPt.Y < fStartPt.Y) then
    begin
      SwapColors(fStartColor, fEndColor);
      SwapPoints(fStartPt, fEndPt);
    end;
    fIsVert := true;
    dx := (fEndPt.X - fStartPt.X);
    dy := (fEndPt.Y - fStartPt.Y);
    dxdy := dx/dy;

    fEndDist := Ceil(dy + dxdy * (fEndPt.X - fStartPt.X));
    fColors := GetLinearColorGradient(fStartColor, fEndColor, fEndDist +1);
    //get a list of perpendicular offsets for each
    SetLength(fPerpOffsets, Target.Width);
    //from an imaginary line that's through fStartPt and perpendicular to
    //the gradient line, get a list of Y offsets for each X in image width
    for i := 0 to Target.Width -1 do
      fPerpOffsets[i] := Round(dxdy * (fStartPt.X - i));
  end
  else //gradient ==> horizontal
  begin
    if (fEndPt.X = fStartPt.X) then
    begin
      Result := false;
      Exit;
    end;
    if (fEndPt.X < fStartPt.X) then
    begin
      SwapColors(fStartColor, fEndColor);
      SwapPoints(fStartPt, fEndPt);
    end;
    fIsVert := false;
    dx := (fEndPt.X - fStartPt.X);
    dy := (fEndPt.Y - fStartPt.Y);
    dydx := dy/dx; //perpendicular slope

    fEndDist := Ceil(dx + dydx * (fEndPt.Y - fStartPt.Y));
    fColors := GetLinearColorGradient(fStartColor, fEndColor, fEndDist +1);
    SetLength(fPerpOffsets, Target.Height);
    //from an imaginary line that's through fStartPt and perpendicular to
    //the gradient line, get a list of X offsets for each Y in image height
    for i := 0 to Target.Height -1 do
      fPerpOffsets[i] := Round(dydx * (fStartPt.Y - i));
  end;
end;
//------------------------------------------------------------------------------

procedure TLinearGradientRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i, off: integer;
  pDst: PColor32;
  color: TARGB;
begin
  pDst := GetDstPixel(x1,y);
  for i := x1 to x2 do
  begin
    if fIsVert then
    begin
      off := fPerpOffsets[i] + Round(fStartPt.Y);
      color.Color := fColors[fBoundsProc(y, off, off + fEndDist -1)];
    end else
    begin
      off := fPerpOffsets[y] + Round(fStartPt.X);
      color.Color := fColors[fBoundsProc(i, off, off + fEndDist -1)];
    end;
    pDst^ := BlendToAlpha(pDst^,
      MulTable[color.A, Ord(alpha^)] shl 24 or (color.Color and $FFFFFF));
    inc(pDst); inc(alpha);
  end;
end;

//------------------------------------------------------------------------------
// TRadialGradientRenderer
//------------------------------------------------------------------------------

function TRadialGradientRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  result := inherited Initialize(targetImage) and (fMaxColors > 1);
  if not result then Exit;
  fColors := GetLinearColorGradient(fInnerColor, fOuterColor, fMaxColors+1);
end;
//------------------------------------------------------------------------------

procedure TRadialGradientRenderer.SetParameters(const focalRect: TRect;
  innerColor, outerColor: TColor32;
  gradientFillStyle: TGradientFillStyle);
var
  radX,radY: double;
begin
  SetGradientFillStyle(gradientFillStyle);
  fInnerColor := innerColor;
  fOuterColor := outerColor;

  fFocalPt.X  := (focalRect.Left + focalRect.Right) * 0.5;
  fFocalPt.Y  := (focalRect.Top + focalRect.Bottom) * 0.5;
  radX    :=  RectWidth(focalRect) * 0.5;
  radY    :=  RectHeight(focalRect) * 0.5;
  if radX >= radY then
  begin
    fScaleX     := 1;
    fScaleY     := radX/radY;
    fMaxColors := Ceil(radX);
  end else
  begin
    fScaleX     := radY/radX;
    fScaleY     := 1;
    fMaxColors := Ceil(radY);
  end;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientRenderer.SetGradientFillStyle(value: TGradientFillStyle);
begin
  case value of
    gfsClamp: fBoundsProc := ClampQ;
    gfsMirror: fBoundsProc := MirrorQ;
    else fBoundsProc := SoftRepeatExStart;
  end;
end;
//------------------------------------------------------------------------------

procedure TRadialGradientRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dist: double;
  color: TARGB;
  pDst: PColor32;
begin
  pDst := GetDstPixel(x1,y);
  for i := x1 to x2 do
  begin
    dist := Sqrt(Sqr((y - fFocalPt.Y)*fScaleY) + Sqr((i - fFocalPt.X)*fScaleX));
    color.Color := fColors[fBoundsProc(Round(dist), 0, fMaxColors)];
    pDst^ := BlendToAlpha(pDst^,
      MulTable[color.A, Ord(alpha^)] shl 24 or (color.Color and $FFFFFF));
    inc(pDst); inc(alpha);
  end;
end;

//------------------------------------------------------------------------------
// Draw functions
//------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32;
  const pt: TPointD; width: double; color: TColor32);
var
  path: TArrayOfPointD;
begin
  path := Ellipse(RectD(pt.X -width, pt.Y -width, pt.X +width, pt.Y +width));
  DrawPolygon(img, path, frEvenOdd, color);
end;
//------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32; const pt: TPointD;
  width: double; renderer: TCustomRenderer);
var
  path: TArrayOfPointD;
begin
  path := Ellipse(RectD(pt.X -width, pt.Y -width, pt.X +width, pt.Y +width));
  DrawPolygon(img, path, frEvenOdd, renderer);
end;
//------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const line: TArrayOfPointD; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  lines: TArrayOfArrayOfPointD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawLine(img, lines, lineWidth, color, endStyle, joinStyle);
end;
//------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const line: TArrayOfPointD; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  lines: TArrayOfArrayOfPointD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawLine(img, lines, lineWidth, renderer, endStyle, joinStyle);
end;
//------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const lines: TArrayOfArrayOfPointD;
  lineWidth: double; color: TColor32;
  endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  lines2: TArrayOfArrayOfPointD;
  cr: TColorRenderer;
begin
  if not assigned(lines) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines2 := Outline(lines, lineWidth, joinStyle, endStyle, 2);
  cr := TColorRenderer.Create(color);
  try
    if cr.Initialize(img) then
      Rasterize(lines2, img.bounds, frNonZero, cr);
  finally
    cr.free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const lines: TArrayOfArrayOfPointD;
  lineWidth: double; renderer: TCustomRenderer;
  endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  lines2: TArrayOfArrayOfPointD;
begin
  if (not assigned(lines)) or (not assigned(renderer)) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines2 := Outline(lines, lineWidth, joinStyle, endStyle, 2);
  if renderer.Initialize(img) then
    Rasterize(lines2, img.bounds, frNonZero, renderer);
end;
//------------------------------------------------------------------------------

function DrawDashedLine(img: TImage32; const line: TArrayOfPointD;
  dashPattern: TArrayOfInteger; patternOffset: double; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle): double;
var
  lines: TArrayOfArrayOfPointD;
  cr: TColorRenderer;
begin
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  result := patternOffset;
  if not assigned(line) then exit;
  lines := BuildDashPath(line, endStyle = esClosed, dashPattern, Result);
  if Length(lines) = 0 then Exit;
  lines := Outline(lines, lineWidth, joinStyle, endStyle);
  cr := TColorRenderer.Create(color);
  try
    if cr.Initialize(img) then
      Rasterize(lines, img.bounds, frNonZero, cr);
  finally
    cr.free;
  end;
end;
//------------------------------------------------------------------------------

function DrawDashedLine(img: TImage32; const lines: TArrayOfArrayOfPointD;
  dashPattern: TArrayOfInteger; patternOffset: double; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle): double;
var
  i: integer;
begin
  result := patternOffset;
  if not assigned(lines) then exit;
  for i := 0 to high(lines) do
    result := DrawDashedLine(img, lines[i],
      dashPattern, result, lineWidth, color, endStyle, joinStyle);
end;
//------------------------------------------------------------------------------

function DrawDashedLine(img: TImage32; const line: TArrayOfPointD;
  dashPattern: TArrayOfInteger; patternOffset: double; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle): double;
var
  lines: TArrayOfArrayOfPointD;
begin
  result := patternOffset;
  if (not assigned(line)) or (not assigned(renderer)) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines := BuildDashPath(line, endStyle = esClosed, dashPattern, result);
  if Length(lines) = 0 then Exit;
  lines := Outline(lines, lineWidth, joinStyle, endStyle);
  if renderer.Initialize(img) then
    Rasterize(lines, img.bounds, frNonZero, renderer);
end;
//------------------------------------------------------------------------------

function DrawDashedLine(img: TImage32; const lines: TArrayOfArrayOfPointD;
  dashPattern: TArrayOfInteger; patternOffset: double; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle): double;
var
  i: integer;
begin
  result := patternOffset;
  if not assigned(lines) then exit;
  for i := 0 to high(lines) do
    DrawDashedLine(img, lines[i],
      dashPattern, result, lineWidth, renderer, endStyle, joinStyle);
end;
//------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; color: TColor32);
var
  polygons: TArrayOfArrayOfPointD;
begin
  if not assigned(polygon) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  DrawPolygon(img, polygons, fillRule, color);
end;
//------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; renderer: TCustomRenderer);
var
  polygons: TArrayOfArrayOfPointD;
begin
  if (not assigned(polygon)) or (not assigned(renderer)) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  if renderer.Initialize(img) then
    Rasterize(polygons, img.Bounds, fillRule, renderer);
end;
//------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; color: TColor32);
var
  cr: TColorRenderer;
begin
  if not assigned(polygons) then exit;
  cr := TColorRenderer.Create(color);
  try
    if cr.Initialize(img) then
      Rasterize(polygons, img.bounds, fillRule, cr);
  finally
    cr.free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; renderer: TCustomRenderer);
begin
  if (not assigned(polygons)) or (not assigned(renderer)) then exit;
  if renderer.Initialize(img) then
    Rasterize(polygons, img.bounds, fillRule, renderer);
end;
//------------------------------------------------------------------------------

initialization
  SetGamma(1.0);

end.
