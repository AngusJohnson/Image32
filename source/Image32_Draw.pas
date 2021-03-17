unit Image32_Draw;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.16                                                            *
* Date      :  18 March 2021                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  Polygon renderer for TImage32                                   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

{.$DEFINE MemCheck} //for debugging only (adds a minimal cost to performance)

uses
  SysUtils, Classes, Types, Math, Image32, Image32_Vector;

type
  TFillRule = Image32_Vector.TFillRule;

  //TGradientColor: used internally by both
  //TLinearGradientRenderer and TRadialGradientRenderer
  TGradientColor = record
    offset: double;
    color: TColor32;
  end;
  TArrayOfGradientColor = array of TGradientColor;

  TGradientFillStyle = (gfsClamp, gfsMirror, gfsRepeat);

  //TBoundsProc: Function template for TCustomRenderer.
  TBoundsProc = function(X, maxX: integer): integer;

  TImage32ChangeProc = procedure of object;

  //TCustomRenderer: can accommodate pixels of any size
  TCustomRenderer = class {$IFDEF ABSTRACT_CLASSES} abstract {$ENDIF}
  private
    fImgWidth    : integer;
    fImgHeight   : integer;
    fImgBase     : Pointer;
    fCurrY       : integer;
    fCurrLinePtr : Pointer;
    fPixelSize   : integer;
    fChangeProc  : TImage32ChangeProc;
  protected
    function Initialize(imgBase: Pointer;
      imgWidth, imgHeight, pixelSize: integer): Boolean; overload; virtual;
    function Initialize(targetImage: TImage32): Boolean; overload; virtual;
    function GetDstPixel(x,y: integer): Pointer;
    //RenderProc: x & y refer to pixel coords in the destination image and
    //where x1 is the start (and left) and x2 is the end of the render
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); virtual; abstract;
    property ImgWidth: integer read fImgWidth;
    property ImgHeight: integer read fImgHeight;
    property ImgBase: Pointer read fImgBase;
    property PixelSize: integer read fPixelSize;
  public
    destructor Destroy; override;
  end;

  TColorRenderer = class(TCustomRenderer)
  private
    fAlpha: Byte;
    fColor: TColor32;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    constructor Create(color: TColor32 = clNone32);
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetColor(value: TColor32);
  end;

  TEraseRenderer = class(TCustomRenderer)
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  end;

  TInverseRenderer = class(TCustomRenderer)
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  end;

  TImageRenderer = class(TCustomRenderer)
  private
    fImage        : TImage32;
    fOffset       : TPoint;
    fBrushPixel   :  PARGB;
    fLastYY       : integer;
    fMirrorY      : Boolean;
    fBoundsProc   : TBoundsProc;
    function GetFirstBrushPixel(x, y: integer): PARGB;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    constructor Create(tileFillStyle: TTileFillStyle = tfsRepeat;
      brushImage: TImage32 = nil);
    destructor Destroy; override;
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetTileFillStyle(value: TTileFillStyle);
    property Image: TImage32 read fImage;
    property Offset: TPoint read fOffset write fOffset;
  end;

  //TCustomGradientRenderer is also an abstract class
  TCustomGradientRenderer = class(TCustomRenderer)
  private
    fStartPt         : TPointD;
    fEndPt           : TPointD;
    fColors          : TArrayOfColor32;
    fBoundsProc      : TBoundsProc;
    fGradientColors  : TArrayOfGradientColor;
  protected
    procedure SetGradientFillStyle(value: TGradientFillStyle);
  public
    constructor Create;
    procedure InsertColorStop(offsetFrac: double; color: TColor32);
  end;

  TLinearGradientRenderer = class(TCustomGradientRenderer)
  private
    fPerpendicOffsets: TArrayOfInteger;
    fEndDist         : integer;
    fIsVert          : Boolean;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetParameters(const startPt, endPt: TPointD;
      startColor, endColor: TColor32;
      gradFillStyle: TGradientFillStyle = gfsClamp);
  end;

  TRadialGradientRenderer = class(TCustomGradientRenderer)
  private
    fCenterPt       : TPointD;
    fScaleX         : double;
    fScaleY         : double;
    fMaxColors      : integer;
    fColors         : TArrayOfColor32;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetParameters(const focalRect: TRect;
      innerColor, outerColor: TColor32;
      gradientFillStyle: TGradientFillStyle = gfsClamp);
  end;

  TSvgRadialGradientRenderer = class(TCustomGradientRenderer)
  private
    aa,bb           : double;
    dx,dy           : double;
    rec             : TRectD;
    fFocusPt        : TPointD;
    fMaxColors      : integer;
    fColors         : TArrayOfColor32;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    function Initialize(targetImage: TImage32): Boolean; override;
    procedure SetParameters(const ellipseRect: TRect;
      const focus: TPoint; innerColor, outerColor: TColor32);
  end;

  //Barycentric rendering colorizes inside triangles
  TBarycentricRenderer = class(TCustomRenderer)
  private
    a: TPointD;
    c1, c2, c3: TARGB;
    v0, v1: TPointD;
    d00, d01, d11, invDenom: double;
    function GetColor(const pt: TPointD): TColor32;
  protected
    procedure RenderProc(x1, x2, y: integer; alpha: PByte); override;
  public
    procedure SetParameters(const a, b, c: TPointD; c1, c2, c3: TColor32);
  end;

  ///////////////////////////////////////////////////////////////////////////
  // DRAWING FUNCTIONS
  ///////////////////////////////////////////////////////////////////////////

  procedure DrawPoint(img: TImage32; const pt: TPointD;
    radius: double; color: TColor32); overload;
  procedure DrawPoint(img: TImage32; const pt: TPointD;
    radius: double; renderer: TCustomRenderer); overload;
  procedure DrawPoint(img: TImage32; const points: TPathD;
    radius: double; color: TColor32); overload;
  procedure DrawPoint(img: TImage32; const paths: TPathsD;
    radius: double; color: TColor32); overload;

  procedure DrawLine(img: TImage32;
    const line: TPathD; lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;
  procedure DrawLine(img: TImage32;
    const line: TPathD; lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;
  procedure DrawLine(img: TImage32; const lines: TPathsD;
    lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;
  procedure DrawLine(img: TImage32; const lines: TPathsD;
    lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto;
    miterLimit: double = 2); overload;

   procedure DrawInvertedLine(img: TImage32;
     const line: TPathD; lineWidth: double;
     endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;
   procedure DrawInvertedLine(img: TImage32;
     const lines: TPathsD; lineWidth: double;
     endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;

  procedure DrawDashedLine(img: TImage32; const line: TPathD;
    dashPattern: TArrayOfInteger; patternOffset: PDouble;
    lineWidth: double; color: TColor32;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawDashedLine(img: TImage32; const lines: TPathsD;
    dashPattern: TArrayOfInteger; patternOffset: PDouble;
    lineWidth: double; color: TColor32; endStyle: TEndStyle;
    joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawDashedLine(img: TImage32; const line: TPathD;
    dashPattern: TArrayOfInteger; patternOffset: PDouble;
    lineWidth: double; renderer: TCustomRenderer; endStyle: TEndStyle;
    joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawDashedLine(img: TImage32; const lines: TPathsD;
    dashPattern: TArrayOfInteger; patternOffset: PDouble;
    lineWidth: double; renderer: TCustomRenderer;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;

  procedure DrawInvertedDashedLine(img: TImage32;
    const line: TPathD; dashPattern: TArrayOfInteger;
    patternOffset: PDouble; lineWidth: double; endStyle: TEndStyle;
    joinStyle: TJoinStyle = jsAuto); overload;
  procedure DrawInvertedDashedLine(img: TImage32;
    const lines: TPathsD; dashPattern: TArrayOfInteger;
    patternOffset: PDouble; lineWidth: double;
    endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto); overload;

  procedure DrawPolygon(img: TImage32; const polygon: TPathD;
    fillRule: TFillRule; color: TColor32); overload;
  procedure DrawPolygon(img: TImage32; const polygon: TPathD;
    fillRule: TFillRule; renderer: TCustomRenderer); overload;
  procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule; color: TColor32); overload;
  procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule; renderer: TCustomRenderer); overload;

  // 'Clear Type' text rendering is quite useful for low resolution
  // displays (96 ppi). However it's of little to no benefit on higher
  // resolution displays and becomes unnecessary overhead. See also:
  // https://en.wikipedia.org/wiki/Subpixel_rendering
  // https://www.grc.com/ctwhat.htm
  // https://www.grc.com/cttech.htm
  procedure DrawPolygon_ClearType(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule; color: TColor32; backColor: TColor32 = clWhite32);

  ///////////////////////////////////////////////////////////////////////////
  // MISCELLANEOUS FUNCTIONS
  ///////////////////////////////////////////////////////////////////////////

  procedure ErasePolygon(img: TImage32; const polygon: TPathD;
    fillRule: TFillRule); overload;
  procedure ErasePolygon(img: TImage32; const polygons: TPathsD;
    fillRule: TFillRule); overload;

  //Both DrawBoolMask and DrawAlphaMask require
  //'mask' length to equal 'img' width * height
  procedure DrawBoolMask(img: TIMage32;
    const mask: TArrayOfByte; color: TColor32 = clBlack32);
  procedure DrawAlphaMask(img: TIMage32;
    const mask: TArrayOfByte; color: TColor32 = clBlack32);

  procedure Rasterize(const paths: TPathsD;
    const clipRec: TRect; fillRule: TFillRule; renderer: TCustomRenderer);

implementation

{$IFDEF MemCheck}
resourcestring
  sMemCheckError = 'Image32_Draw: Memory allocation error';
{$ENDIF}

type

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
    fragCnt: integer;
    {$IFDEF MemCheck} total: integer; {$ENDIF}
    fragments: PFragments;
  end;
  PScanline = ^TScanline;
  TArrayOfScanline = array of TScanline;

//------------------------------------------------------------------------------
// ApplyClearType (see DrawPolygon_ClearType below)
//------------------------------------------------------------------------------

type
  PArgbs = ^TArgbs;
  TArgbs = array [0.. (Maxint div SizeOf(TARGB)) -1] of TARGB;

procedure ApplyClearType(img: TImage32;
  textColor: TColor32 = clBlack32; bkColor: TColor32 = clWhite32);
const
  centerWeighting = 5; //0 <= centerWeighting <= 25
var
  h, w: integer;
  src, dst: PARGB;
  srcArr: PArgbs;
  fgColor: TARGB absolute textColor;
  bgColor: TARGB absolute bkColor;
  diff_R, diff_G, diff_B: integer;
  bg8_R, bg8_G, bg8_B: integer;
  rowBuffer: TArrayOfARGB;
  primeTbl, nearTbl, FarTbl: PByteArray;
begin
  // Precondition: the background to text drawn onto 'img' must be transparent

  // multiplication tables (see Image32.pas)
  // 85 + (2 * 57) + (2 * 28) == 255
  primeTbl := PByteArray(@MulTable[85 + centerWeighting *2]);
  nearTbl  := PByteArray(@MulTable[57]);
  farTbl   := PByteArray(@MulTable[28 - centerWeighting]);

  SetLength(rowBuffer, img.Width+4);
  FillChar(rowBuffer[0], Length(rowBuffer) *  SizeOf(TColor32), 0);

  for h := 0 to img.Height -1 do
  begin
    dst := PARGB(@img.Pixels[h * img.Width]);
    //each row of the image is copied into a temporary buffer ...
    src := PARGB(@rowBuffer[2]);
    Move(dst^, src^, img.Width * SizeOf(TColor32));
    srcArr := PArgbs(rowBuffer);

    //using this buffer update the image ...
    w := 2;
    while w < img.Width do
    begin
      dst.R := primeTbl[srcArr[w].A] +
        nearTbl[srcArr[w-1].A] + farTbl[srcArr[w-2].A] +
        nearTbl[srcArr[w+1].A] + farTbl[srcArr[w+2].A];
      inc(w);
      dst.G := primeTbl[srcArr[w].A] +
        nearTbl[srcArr[w-1].A] + farTbl[srcArr[w-2].A] +
        nearTbl[srcArr[w+1].A] + farTbl[srcArr[w+2].A];
      inc(w);
      dst.B := primeTbl[srcArr[w].A] +
        nearTbl[srcArr[w-1].A] + farTbl[srcArr[w-2].A] +
        nearTbl[srcArr[w+1].A] + farTbl[srcArr[w+2].A];
      inc(w);
      dst.A := 255;
      inc(dst);
    end;
  end;

  //The right 2/3 of the image can now be removed ...
   img.Crop(Types.Rect(0,0, img.Width div 3, img.Height));

  //currently text is white and the background is black
  //so blend in the text and background colors ...
  diff_R := fgColor.R - bgColor.R;
  diff_G := fgColor.G - bgColor.G;
  diff_B := fgColor.B - bgColor.B;
  bg8_R := bgColor.R shl 8;
  bg8_G := bgColor.G shl 8;
  bg8_B := bgColor.B shl 8;
  dst := PARGB(img.PixelBase);
  for h := 0 to img.Width * img.Height -1 do
  begin
    if dst.R > 0 then
    begin
      //blend font and background colors ...
      dst.R := (bg8_R + diff_R * dst.R) shr 8;
      dst.G := (bg8_G + diff_G * dst.G) shr 8;
      dst.B := (bg8_B + diff_B * dst.B) shr 8;
    end
    else dst.Color := 0;
    inc(dst);
  end;
end;

//------------------------------------------------------------------------------
// Other miscellaneous functions
//------------------------------------------------------------------------------

////__Trunc: A very efficient Trunc() algorithm (ie rounds toward zero)
//function __Trunc(val: double): integer; {$IFDEF INLINE} inline; {$ENDIF}
//var
//  exp: integer;
//  i64: UInt64 absolute val;
//begin
//  //https://en.wikipedia.org/wiki/Double-precision_floating-point_format
//  Result := 0;
//  if i64 = 0 then Exit;
//  exp := Integer(Cardinal(i64 shr 52) and $7FF) - 1023;
//  //nb: when exp == 1024 then val == INF or NAN
//  if exp < 0 then Exit;
//  Result := ((i64 and $1FFFFFFFFFFFFF) shr (52-exp)) or (1 shl exp);
//  if val < 0 then Result := -Result;
//end;
//------------------------------------------------------------------------------

function ClampByte(val: double): byte; {$IFDEF INLINE} inline; {$ENDIF}
begin
  if val < 0 then result := 0
  else if val > 255 then result := 255
  else result := Round(val);
end;
//------------------------------------------------------------------------------

function GetPixel(current: PARGB; delta: integer): PARGB;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := current;
  inc(Result, delta);
end;
//------------------------------------------------------------------------------

function ReverseColors(const colors: TArrayOfGradientColor): TArrayOfGradientColor;
var
  i, highI: integer;
begin
  highI := High(colors);
  SetLength(result, highI +1);
  for i := 0 to highI do
  begin
    result[i].color := colors[highI -i].color;
    result[i].offset := 1 - colors[highI -i].offset;
  end;
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

function ClampQ(q, endQ: integer): integer;
begin
  if q < 0 then result := 0
  else if q >= endQ then result := endQ -1
  else result := q;
end;
//------------------------------------------------------------------------------

function MirrorQ(q, endQ: integer): integer;
begin
  result := q mod endQ;
  if (result < 0) then result := -result;
  if Odd(q div endQ) then
    result := (endQ -1) - result;
end;
//------------------------------------------------------------------------------

function RepeatQ(q, endQ: integer): integer;
begin
  if (q < 0) or (q >= endQ) then
  begin
    endQ := Abs(endQ);
    result := q mod endQ;
    if result < 0 then inc(result, endQ);
  end
  else result := q;
end;
//------------------------------------------------------------------------------

function SoftRepeat(q, endQ: integer): integer;
var
  dx: integer;
begin
  if (q < 0) or (q >= endQ) then
  begin
    dx := Abs(endQ);
    result := q mod dx;
    if result < 0 then inc(result, dx);
    //exactly where the colors repeat, average the two colors
    if result = 0 then result := dx div 2;
  end else
  begin
    result := q;
    if result <> 0 then Exit;
    //soft blend at startQ too
    dx := Abs(endQ);
    result := dx div 2;
  end;
end;
//------------------------------------------------------------------------------

function SoftRepeatExStart(q, endQ: integer): integer;
var
  dx: integer;
begin
  if (q < 0) or (q >= endQ) then
  begin
    dx := Abs(endQ);
    result := q mod dx;
    if result < 0 then inc(result, dx);
    if result = 0 then result := dx div 2;
  end else
    result := q;
end;
//------------------------------------------------------------------------------

function BlendColorUsingMask(bgColor, fgColor: TColor32; mask: Byte): TColor32;
var
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
  res: TARGB absolute Result;
  R, invR: PByteArray;
begin
  if fg.A = 0 then
  begin
    Result := bgColor;
    res.A := MulBytes(res.A, not mask);
  end
  else if bg.A = 0 then
  begin
    Result := fgColor;
    res.A := MulBytes(res.A, mask);
  end
  else if (mask = 0) then
    Result := bgColor
  else if (mask = 255) then
    Result := fgColor
  else
  begin
    R    := PByteArray(@MulTable[mask]);
    InvR := PByteArray(@MulTable[not mask]);
    res.A := R[fg.A] + InvR[bg.A];
    res.R := R[fg.R] + InvR[bg.R];
    res.G := R[fg.G] + InvR[bg.G];
    res.B := R[fg.B] + InvR[bg.B];
  end;
end;
//------------------------------------------------------------------------------

//GetColorGradient: using the supplied array of TGradientColor,
//create an array of TColor32 of the specified length
function GetColorGradient(const gradColors: TArrayOfGradientColor;
  len: integer): TArrayOfColor32;
var
  i,j, lenC: integer;
  dist, offset1, offset2, step, pos: double;
  color1, color2: TColor32;
begin
  lenC := length(gradColors);
  if (len = 0) or (lenC < 2) then Exit;
  SetLength(result, len);
  step := 1/len;

  color2 := gradColors[0].color;
  result[0] := color2;
  pos := step;
  offset2 := 0;
  i := 1; j := 1;
  repeat
    offset1 := offset2;
    offset2 := gradColors[i].offset;
    dist := offset2 - offset1;
    color1 := color2;
    color2 := gradColors[i].color;
    while (pos <= dist) and (j < len) do
    begin
      result[j] := BlendColorUsingMask(color1, color2, Round(pos/dist * 255));
      inc(j);
      pos := pos + step;
    end;
    pos := pos - dist;
    inc(i);
  until i = lenC;
end;

//------------------------------------------------------------------------------
// Rasterize() support functions
//------------------------------------------------------------------------------

procedure AllocateScanlines(const polygons: TPathsD;
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
    y1 := Round(polygons[i][highJ].Y);
    for j := 0 to highJ do
    begin
      y2 := Round(polygons[i][j].Y);
      if y1 < y2 then
      begin
        //descending (ignoring edges outside the clipping range)
        if (y2 >= 0) and (y1 <= clipBottom) then
        begin
          if (y1 > 0) and (y1 <= clipBottom)  then
            dec(scanlines[y1 -1].fragCnt);
          if y2 >= clipBottom then
            inc(scanlines[clipBottom].fragCnt) else
            inc(scanlines[y2].fragCnt);
        end;
      end else
      begin
        //ascending (ignoring edges outside the clipping range)
        if (y1 >= 0) and (y2 <= clipBottom) then
        begin
          if (y2 > 0) then
            dec(scanlines[y2 -1].fragCnt);
          if y1 >= clipBottom then
            inc(scanlines[clipBottom].fragCnt) else
            inc(scanlines[y1].fragCnt);
        end;
      end;
      y1 := y2;
    end;
  end;

  //convert 'count' accumulators into real counts and allocate storage
  j := 0;
  highI := high(scanlines);
  psl := @scanlines[highI];

  //'fragments' is a pointer and not a dynamic array because
  //dynamic arrays are zero initialized (hence slower than GetMem).
  for i := highI downto 0 do
  begin
    inc(j, psl.fragCnt); //nb: psl.fragCnt may be < 0 here!
    if j > 0 then
      GetMem(psl.fragments, j * SizeOf(TFragment));
    {$IFDEF MemCheck} psl.total := j; {$ENDIF}
    psl.fragCnt := 0; //reset for later
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
  if bot.X >= maxX then
  begin
    if top.X >= maxX then
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
  if scanlineY < 0 then Exit;

  psl := @scanlines[scanlineY];
  if not assigned(psl.fragments) then Exit; //a very rare event
  {$IFDEF MemCheck}
  if psl.fragCnt = psl.total then raise Exception.Create(sMemCheckError);
  {$ENDIF}

  pFrag := @psl.fragments[psl.fragCnt];
  inc(psl.fragCnt);

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
    {$IFDEF MemCheck}
    if psl.fragCnt = psl.total then raise Exception.Create(sMemCheckError);
    {$ENDIF}
    pFrag := @psl.fragments[psl.fragCnt];
    inc(psl.fragCnt);
    pFrag.botX := x;
    x := x + dxdy;
    pFrag.topX := x;
    pFrag.dy := 1;
    pFrag.dydx := dydx;
    dec(psl);
  end;
  //and finally the top fragment
  {$IFDEF MemCheck}
  if psl.fragCnt = psl.total then raise Exception.Create(sMemCheckError);
  {$ENDIF}
  pFrag := @psl.fragments[psl.fragCnt];
  inc(psl.fragCnt);
  pFrag.botX := x;
  pFrag.topX := top.X;
  pFrag.dy := psl.Y + 1 - top.Y;
  pFrag.dydx := dydx;
end;
//------------------------------------------------------------------------------

procedure InitializeScanlines(var polygons: TPathsD;
  const scanlines: TArrayOfScanline; const clipRec: TRect);
var
  i,j, highJ: integer;
  pt1, pt2: PPointD;
begin
 for i := 0 to high(polygons) do
  begin
    highJ := high(polygons[i]);
    if highJ < 2 then continue;
    pt1 := @polygons[i][highJ];
    pt2 := @polygons[i][0];
    for j := 0 to highJ do
    begin
      SplitEdgeIntoFragments(pt1^, pt2^, scanlines, clipRec);
      pt1 := pt2;
      inc(pt2);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure ProcessScanlineFragments(var scanline: TScanLine;
  var buffer: TArrayOfDouble);
var
  i,j, leftXi,rightXi: integer;
  fracX, yy, q, windDir: double;
  pd: PDouble;
  frag: PFragment;
begin
  frag := @scanline.fragments[0];
  for i := 1 to scanline.fragCnt do
  begin
    if frag.botX > frag.topX then
    begin
      //just swapping botX and topX simplifies code
      q := frag.botX;
      frag.botX := frag.topX;
      frag.topX  := q;
    end;

    leftXi := Max(0, Round(frag.botX));
    rightXi := Max(0, Round(frag.topX));

    if (leftXi = rightXi) then
    begin
      if frag.dydx < 0 then windDir := -1.0 else windDir := 1.0;
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

{$IFNDEF TROUNDINGMODE}
type
  TRoundingMode = {$IFNDEF FPC}Math.{$ENDIF}TFPURoundingMode;
{$ENDIF}

procedure Rasterize(const paths: TPathsD; const clipRec: TRect;
  fillRule: TFillRule; renderer: TCustomRenderer);
var
  i,j, xli, xri, maxW, maxH, aa: integer;
  clipRec2: TRect;
  paths2: TPathsD;
  accum: double;
  windingAccum: TArrayOfDouble;
  byteBuffer: TArrayOfByte;
  scanlines: TArrayOfScanline;
  scanline: PScanline;
  savedRoundMode: TRoundingMode;
begin
  //See also https://nothings.org/gamedev/rasterize/
  if not assigned(renderer) then Exit;
  clipRec2 := Image32_Vector.IntersectRect(clipRec, GetBounds(paths));
  paths2 := OffsetPath(paths, -clipRec2.Left, -clipRec2.Top);

  //Delphi's Round() function is *much* faster than its Trunc function, and
  //it's even a little faster than the __Trunc function above (except when
  //the FastMM4 memory manager is enabled.)
  savedRoundMode := SetRoundMode(rmDown);

  maxW := RectWidth(clipRec2);
  maxH := RectHeight(clipRec2);
  SetLength(scanlines, maxH +1);
  SetLength(windingAccum, maxW +2);
  AllocateScanlines(paths2, scanlines, maxH, maxW-1);
  InitializeScanlines(paths2, scanlines, clipRec2);
  SetLength(byteBuffer, RectWidth(clipRec2));
  if byteBuffer = nil then Exit;

  scanline := @scanlines[0];
  for i := 0 to high(scanlines) do
  begin
    if scanline.fragCnt = 0 then
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

    //a 25% weighting has been added to the alpha channel to minimize any
    //background bleed-through where polygons join with a common edge.

    accum := 0; //winding count accumulator
    for j := xli to xri do
    begin
      accum := accum + windingAccum[j];
      case fillRule of
        frEvenOdd:
          begin
            aa := Round(Abs(accum) * 1275) mod 2550; //(255 * 5) shr 2.
            if aa > 1275 then
              byteBuffer[j] := Min(255, (2550 - aa) shr 2) else
              byteBuffer[j] := Min(255, aa shr 2);
          end;
        frNonZero:
          begin
            byteBuffer[j] := Min(255, Round(Abs(accum) * 318)); //318=255*1.25
          end;
        frPositive:
          begin
            if accum > 0.002 then
              byteBuffer[j] := Min(255, Round(accum * 318));
          end;
        frNegative:
          begin
            if accum < -0.002 then
              byteBuffer[j] := Min(255, Round(-accum * 318));
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
  SetRoundMode(savedRoundMode);
end;

//------------------------------------------------------------------------------
// TAbstractRenderer
//------------------------------------------------------------------------------

destructor TCustomRenderer.Destroy;
begin
  if assigned(fChangeProc) then fChangeProc;
  inherited;
end;
//------------------------------------------------------------------------------

function TCustomRenderer.Initialize(imgBase: Pointer;
  imgWidth, imgHeight, pixelSize: integer): Boolean;
begin
  fImgBase := imgBase;
  fImgWidth := ImgWidth;
  fImgHeight := ImgHeight;
  fPixelSize := pixelSize;

  fCurrLinePtr := fImgBase;
  fCurrY       := 0;
  result       := true;
end;
//------------------------------------------------------------------------------

type THackedImage32 = class(TImage32); //to expose protected 'Changed' method.

function TCustomRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  fChangeProc := THackedImage32(targetImage).Changed;
  with targetImage do
    result := Initialize(PixelBase, Width, Height, 4);
end;
//------------------------------------------------------------------------------

function TCustomRenderer.GetDstPixel(x, y: integer): Pointer;
begin
  if (y <> fCurrY) then
  begin
    fCurrY := y;
    fCurrLinePtr := fImgBase;
    inc(PByte(fCurrLinePtr), fCurrY * fImgWidth * fPixelSize);
  end;
  Result := fCurrLinePtr;
  inc(PByte(Result), x * fPixelSize);
end;

//------------------------------------------------------------------------------
// TColorRenderer
//------------------------------------------------------------------------------

constructor TColorRenderer.Create(color: TColor32 = clNone32);
begin
  if color <> clNone32 then SetColor(color);
end;
//------------------------------------------------------------------------------

function TColorRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  //there's no point rendering if the color is fully transparent
  result := inherited Initialize(targetImage) and (fAlpha > 0);
end;
//------------------------------------------------------------------------------

procedure TColorRenderer.SetColor(value: TColor32);
begin
  fColor := value and $FFFFFF;
  fAlpha := value shr 24;
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
    //BlendToAlpha is marginally slower than BlendToOpaque but it's used
    //here because it's universally applicable.
    //Ord() is used here because very old compilers define PByte as a PChar

    dst^ := BlendToAlpha(dst^, ((Ord(alpha^) * fAlpha) shr 8) shl 24 or fColor);
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
  result := inherited Initialize(targetImage) and (not fImage.IsEmpty);
  if not result then Exit;
  fLastYY := 0;
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
  dec(x1, fOffset.X);
  dec(x2, fOffset.X);
  dec(y, fOffset.Y);
  pBrush := GetFirstBrushPixel(x1, y);
  for i := x1 to x2 do
  begin
    pDst^ := BlendToAlpha(pDst^,
      MulBytes(pBrush.A, Ord(alpha^)) shl 24 or (pBrush.Color and $FFFFFF));
    inc(pDst); inc(alpha);
    pBrush := GetPixel(fBrushPixel, fBoundsProc(i, fImage.Width));
  end;
end;
//------------------------------------------------------------------------------

function TImageRenderer.GetFirstBrushPixel(x, y: integer): PARGB;
begin
  if fMirrorY then
    y := MirrorQ(y, fImage.Height) else
    y := RepeatQ(y, fImage.Height);
  if y <> fLastYY then
  begin
    fBrushPixel := PARGB(fImage.PixelRow[y]);
    fLastYY := y;
  end;
  x := fBoundsProc(x, fImage.Width);
  result := GetPixel(fBrushPixel, x);
end;

//------------------------------------------------------------------------------
// TGradientRenderer
//------------------------------------------------------------------------------

constructor TCustomGradientRenderer.Create;
begin
  fBoundsProc := ClampQ; //default proc
end;
//------------------------------------------------------------------------------

procedure TCustomGradientRenderer.SetGradientFillStyle(value: TGradientFillStyle);
begin
  case value of
    gfsClamp: fBoundsProc := ClampQ;
    gfsMirror: fBoundsProc := MirrorQ;
    else fBoundsProc := SoftRepeat;
  end;
end;
//------------------------------------------------------------------------------

procedure TCustomGradientRenderer.InsertColorStop(offsetFrac: double; color: TColor32);
var
  i, len: integer;
  gradColor: TGradientColor;
begin
  if offsetFrac < 0 then offsetFrac := 0
  else if offsetFrac > 1 then offsetFrac := 1;
  gradColor.offset := offsetFrac;
  gradColor.color := color;
  len := Length(fGradientColors);
  if len = 0 then
  begin
    if (offsetFrac = 0) or (offsetFrac = 1) then
      SetLength(fGradientColors, 2) else
      SetLength(fGradientColors, 3);
    for i := 0 to high(fGradientColors) do
      fGradientColors[i] := gradColor;
  end else
  begin
    for i := 0 to len -1 do
      if offsetFrac <= fGradientColors[i].offset then
      begin
        if offsetFrac < fGradientColors[i].offset then
        begin
          SetLength(fGradientColors, len +1);
          if i < len then Move(fGradientColors[i],
            fGradientColors[i+1], (len -i) * SizeOf(TGradientColor));
          fGradientColors[i] := gradColor;
        end else
          fGradientColors[i] := gradColor;
        Exit;
      end;
    fGradientColors[len-1] := gradColor;
  end;
end;

//------------------------------------------------------------------------------
// TLinearGradientRenderer
//------------------------------------------------------------------------------

procedure TLinearGradientRenderer.SetParameters(const startPt, endPt: TPointD;
  startColor, endColor: TColor32; gradFillStyle: TGradientFillStyle);
begin
  SetGradientFillStyle(gradFillStyle);
  fStartPt := startPt;
  fEndPt := endPt;

  //reset gradient colors if perviously set
  SetLength(fGradientColors, 2);
  fGradientColors[0].offset := 0;
  fGradientColors[0].color := startColor;
  fGradientColors[1].offset := 1;
  fGradientColors[1].color := endColor;
end;
//------------------------------------------------------------------------------

function TLinearGradientRenderer.Initialize(targetImage: TImage32): Boolean;
var
  i: integer;
  dx,dy, dxdy,dydx: double;
begin
  result := inherited Initialize(targetImage) and assigned(fGradientColors);
  if not result then Exit;

  if abs(fEndPt.Y - fStartPt.Y) > abs(fEndPt.X - fStartPt.X) then
  begin
    //gradient > 45 degrees
    if (fEndPt.Y < fStartPt.Y) then
    begin
      fGradientColors := ReverseColors(fGradientColors);
      SwapPoints(fStartPt, fEndPt);
    end;
    fIsVert := true;
    dx := (fEndPt.X - fStartPt.X);
    dy := (fEndPt.Y - fStartPt.Y);
    dxdy := dx/dy;

    fEndDist := Ceil(dy + dxdy * (fEndPt.X - fStartPt.X));
    fColors := GetColorGradient(fGradientColors, fEndDist +1);
    //get a list of perpendicular offsets for each
    SetLength(fPerpendicOffsets, ImgWidth);
    //from an imaginary line that's through fStartPt and perpendicular to
    //the gradient line, get a list of Y offsets for each X in image width
    for i := 0 to ImgWidth -1 do
      fPerpendicOffsets[i] := Round(dxdy * (fStartPt.X - i));
  end
  else //gradient <= 45 degrees
  begin
    if (fEndPt.X = fStartPt.X) then
    begin
      Result := false;
      Exit;
    end;
    if (fEndPt.X < fStartPt.X) then
    begin
      fGradientColors := ReverseColors(fGradientColors);
      SwapPoints(fStartPt, fEndPt);
    end;
    fIsVert := false;
    dx := (fEndPt.X - fStartPt.X);
    dy := (fEndPt.Y - fStartPt.Y);
    dydx := dy/dx; //perpendicular slope

    fEndDist := Ceil(dx + dydx * (fEndPt.Y - fStartPt.Y));
    fColors := GetColorGradient(fGradientColors, fEndDist +1);
    SetLength(fPerpendicOffsets, ImgHeight);
    //from an imaginary line that's through fStartPt and perpendicular to
    //the gradient line, get a list of X offsets for each Y in image height
    for i := 0 to ImgHeight -1 do
      fPerpendicOffsets[i] := Round(dydx * (fStartPt.Y - i));
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
      //when fIsVert = true, fPerpendicOffsets is an array of Y for each X
      off := fPerpendicOffsets[i] + Round(fStartPt.Y);
      color.Color := fColors[fBoundsProc(y - off, fEndDist)];
    end else
    begin
      //when fIsVert = false, fPerpendicOffsets is an array of X for each Y
      off := fPerpendicOffsets[y] + Round(fStartPt.X);
      color.Color := fColors[fBoundsProc(i - off, fEndDist)];
    end;
    pDst^ := BlendToAlpha(pDst^,
      MulBytes(color.A, Ord(alpha^)) shl 24 or (color.Color and $FFFFFF));
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
  fColors := GetColorGradient(fGradientColors, fMaxColors);
end;
//------------------------------------------------------------------------------

procedure TRadialGradientRenderer.SetParameters(const focalRect: TRect;
  innerColor, outerColor: TColor32;
  gradientFillStyle: TGradientFillStyle);
var
  radX,radY: double;
begin
  fMaxColors := 0;
  if IsEmptyRect(focalRect) then Exit;

  SetGradientFillStyle(gradientFillStyle);

  //reset gradient colors if perviously set
  SetLength(fGradientColors, 2);
  fGradientColors[0].offset := 0;
  fGradientColors[0].color := innerColor;
  fGradientColors[1].offset := 1;
  fGradientColors[1].color := outerColor;

  fCenterPt.X  := (focalRect.Left + focalRect.Right) * 0.5;
  fCenterPt.Y  := (focalRect.Top + focalRect.Bottom) * 0.5;
  radX    :=  RectWidth(focalRect) * 0.5;
  radY    :=  RectHeight(focalRect) * 0.5;
  if radX >= radY then
  begin
    fScaleX     := 1;
    fScaleY     := radX/radY;
    fMaxColors := Ceil(radX) +1;
  end else
  begin
    fScaleX     := radY/radX;
    fScaleY     := 1;
    fMaxColors := Ceil(radY) +1;
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
    dist := Hypot((y - fCenterPt.Y) *fScaleY, (i - fCenterPt.X) *fScaleX);
    color.Color := fColors[fBoundsProc(Round(dist), fMaxColors)];
    pDst^ := BlendToAlpha(pDst^,
      MulBytes(color.A, Ord(alpha^)) shl 24 or (color.Color and $FFFFFF));
    inc(pDst); inc(alpha);
  end;
end;

//------------------------------------------------------------------------------
// TSvgRadialGradientRenderer
//------------------------------------------------------------------------------

function TSvgRadialGradientRenderer.Initialize(targetImage: TImage32): Boolean;
begin
  result := inherited Initialize(targetImage) and (fMaxColors > 1);
  if not result then Exit;
  fColors := GetColorGradient(fGradientColors, fMaxColors);
end;
//------------------------------------------------------------------------------

procedure TSvgRadialGradientRenderer.SetParameters(const ellipseRect: TRect;
  const focus: TPoint; innerColor, outerColor: TColor32);
begin
  fMaxColors := 0;
  if IsEmptyRect(ellipseRect) then Exit;

  //reset gradient colors if perviously set
  SetLength(fGradientColors, 2);
  fGradientColors[0].offset := 0;
  fGradientColors[0].color := innerColor;
  fGradientColors[1].offset := 1;
  fGradientColors[1].color := outerColor;

  aa    :=  RectWidth(ellipseRect) * 0.5;
  bb    :=  RectHeight(ellipseRect) * 0.5;
  dx    :=  ellipseRect.Left + aa;
  dy    :=  ellipseRect.Top  + bb;
  rec := RectD(ellipseRect);
  OffsetRect(rec, -dx, -dy);
  fFocusPt.X := focus.X - dx;
  fFocusPt.Y := focus.Y - dy;
  fMaxColors := Ceil(Hypot(aa *2, bb *2)) +1;
  aa := aa * aa;
  bb := bb * bb;
end;
//------------------------------------------------------------------------------

procedure TSvgRadialGradientRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  q,m,c, qa,qb,qc,qs: double;
  distMax, dist: double;
  color: TARGB;
  pDst: PColor32;
  pt, ellipsePt: TPointD;
begin
  //get left-most pixel to render
  pDst := GetDstPixel(x1,y);
  pt.X := x1 - dx; pt.Y := y - dy;
  for i := x1 to x2 do
  begin
    if (pt.X <= rec.Left) or (pt.X >= rec.Right) or
      (pt.Y <= rec.Top) or (pt.Y >= rec.Bottom) then q := 1

    //equation of ellipse = (x*x)/aa + (y*y)/bb = 1
    //equation of line = y = mx + c;
    else if (pt.X = fFocusPt.X) then //vertical line
    begin
      //let x = pt.X, then y*y = b*b(1 - Sqr(pt.X)/aa)
      q := Sqrt(bb*(1 - Sqr(pt.X)/aa));
      ellipsePt.X := pt.X;
      if pt.Y >= fFocusPt.Y then
        ellipsePt.Y := q else
        ellipsePt.Y := -q;
      dist := abs(pt.Y - fFocusPt.Y);
      distMax := abs(ellipsePt.Y - fFocusPt.Y);
      if (dist >= distMax) then
        q := 1 else
        q := dist/ distMax;
    end else
    begin
      //using simultaneous equations and substitution
      //given y = mx + c
      m := (pt.Y - fFocusPt.Y)/(pt.X - fFocusPt.X);
      c := pt.Y - m * pt.X;
      //given (x*x)/aa + (y*y)/bb = 1
      //(x*x)/aa*bb + (y*y) = bb
      //bb/aa *(x*x) + Sqr(m*x +c) = bb
      //bb/aa *(x*x) + (m*m)*(x*x) + 2*m*x*c +c*c = b*b
      //(bb/aa +(m*m)) *(x*x) + 2*m*c*(x) + (c*c) - bb = 0
      //solving quadratic equation
      qa := (bb/aa +(m*m));
      qb := 2*m*c;
      qc := (c*c) - bb;
      qs := (qb*qb) - 4*qa*qc;
      if qs >= 0 then
      begin
        qs := Sqrt(qs);
        if pt.X <= fFocusPt.X then
          ellipsePt.X := (-qb -qs)/(2 * qa) else
          ellipsePt.X := (-qb +qs)/(2 * qa);
        ellipsePt.Y := m * ellipsePt.X + c;
        dist := Hypot(pt.X - fFocusPt.X, pt.Y - fFocusPt.Y);
        distMax := Hypot(ellipsePt.X - fFocusPt.X, ellipsePt.Y - fFocusPt.Y);
        if dist > distMax then q := 1
        else q := dist/ distMax;
      end else
        q := 1;
    end;
    color.Color := fColors[fBoundsProc(Round(q * fMaxColors), fMaxColors)];
    pDst^ := BlendToAlpha(pDst^,
      MulBytes(color.A, Ord(alpha^)) shl 24 or (color.Color and $FFFFFF));
    inc(pDst); pt.X := pt.X + 1; inc(alpha);
  end;
end;

//------------------------------------------------------------------------------
// TEraseRenderer
//------------------------------------------------------------------------------

procedure TEraseRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dst: PARGB;
begin
  dst := PARGB(GetDstPixel(x1,y));
  for i := x1 to x2 do
  begin
    {$IFDEF PBYTE}
    dst.A := MulBytes(dst.A, not alpha^);
    {$ELSE}
    dst.A := MulBytes(dst.A, not Ord(alpha^));
    {$ENDIF}
    inc(dst); inc(alpha);
  end;
end;

//------------------------------------------------------------------------------
// TInverseRenderer
//------------------------------------------------------------------------------

procedure TInverseRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  i: integer;
  dst: PARGB;
  c: TARGB;
begin
  dst := PARGB(GetDstPixel(x1,y));
  for i := x1 to x2 do
  begin
    c.Color := not dst.Color;
    c.A := MulBytes(dst.A, Ord(alpha^));
    dst.Color := BlendToAlpha(dst.Color, c.Color);
    inc(dst); inc(alpha);
  end;
end;

//------------------------------------------------------------------------------

procedure TBarycentricRenderer.SetParameters(const a, b, c: TPointD;
  c1, c2, c3: TColor32);
begin

  self.a := a;
  self.c1.Color := c1;
  self.c2.Color := c2;
  self.c3.Color := c3;

  v0.X := b.X - a.X;
  v0.Y := b.Y - a.Y;
  v1.X := c.X - a.X;
  v1.Y := c.Y - a.Y;
  d00 := (v0.X * v0.X + v0.Y * v0.Y);
  d01 := (v0.X * v1.X + v0.Y * v1.Y);
  d11 := (v1.X * v1.X + v1.Y * v1.Y);
  invDenom := 1/(d00 * d11 - d01 * d01);
end;
//------------------------------------------------------------------------------

function TBarycentricRenderer.GetColor(const pt: TPointD): TColor32;
var
  v2: TPointD;
  d20, d21, v, w, u: Double;
  res: TARGB absolute Result;
begin
  Result := 0;
  v2.X := pt.X - a.X;
  v2.Y := pt.Y - a.Y;
  d20 := (v2.X * v0.X + v2.Y * v0.Y);
  d21 := (v2.X * v1.X + v2.Y * v1.Y);

  v := (d11 * d20 - d01 * d21) * invDenom;
  w := (d00 * d21 - d01 * d20) * invDenom;
  u := 1.0 - v - w;

  Res.A := ClampByte(c1.A * u + c2.A * v + c3.A * w);
  Res.R := ClampByte(c1.R * u + c2.R * v + c3.R * w);
  Res.G := ClampByte(c1.G * u + c2.G * v + c3.G * w);
  Res.B := ClampByte(c1.B * u + c2.B * v + c3.B * w);
end;
//------------------------------------------------------------------------------

procedure TBarycentricRenderer.RenderProc(x1, x2, y: integer; alpha: PByte);
var
  x: integer;
  p: PARGB;
  c: TARGB;
begin
  p := PARGB(fImgBase);
  inc(p, y * ImgWidth + x1);
  for x := x1 to x2 do
  begin
    c.Color := GetColor(PointD(x, y));
    c.A := c.A * Ord(alpha^) shr 8;
    p.Color := BlendToAlpha(p.Color, c.Color);
    inc(p); inc(alpha);
  end;
end;

//------------------------------------------------------------------------------
// Draw functions
//------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32;
  const pt: TPointD; radius: double; color: TColor32);
var
  path: TPathD;
begin
  if radius <= 1 then
    path := Rectangle(pt.X-radius, pt.Y-radius, pt.X+radius, pt.Y+radius) else
    path := Ellipse(RectD(pt.X-radius, pt.Y-radius, pt.X+radius, pt.Y+radius));
  DrawPolygon(img, path, frEvenOdd, color);
end;
//------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32; const pt: TPointD;
  radius: double; renderer: TCustomRenderer);
var
  path: TPathD;
begin
  path := Ellipse(RectD(pt.X -radius, pt.Y -radius, pt.X +radius, pt.Y +radius));
  DrawPolygon(img, path, frEvenOdd, renderer);
end;
//------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32; const points: TPathD;
  radius: double; color: TColor32);
var
  i: integer;
begin
  for i := 0 to high(points) do
    DrawPoint(img, points[i], radius, color);
end;
//------------------------------------------------------------------------------

procedure DrawPoint(img: TImage32; const paths: TPathsD;
  radius: double; color: TColor32);
var
  i: integer;
begin
  for i := 0 to high(paths) do
    DrawPoint(img, paths[i], radius, color);
end;
//------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const line: TPathD; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle;
  miterLimit: double);
var
  lines: TPathsD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawLine(img, lines, lineWidth, color, endStyle, joinStyle, miterLimit);
end;
//------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const line: TPathD; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle;
  miterLimit: double);
var
  lines: TPathsD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawLine(img, lines, lineWidth, renderer, endStyle, joinStyle, miterLimit);
end;
//------------------------------------------------------------------------------

procedure DrawInvertedLine(img: TImage32; const line: TPathD;
lineWidth: double; endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto);
var
  lines: TPathsD;
begin
  setLength(lines, 1);
  lines[0] := line;
  DrawInvertedLine(img, lines, lineWidth, endStyle, joinStyle);
end;
//------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const lines: TPathsD;
  lineWidth: double; color: TColor32;
  endStyle: TEndStyle; joinStyle: TJoinStyle;
  miterLimit: double);
var
  lines2: TPathsD;
  cr: TColorRenderer;
begin
  if not assigned(lines) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines2 := Outline(lines, lineWidth, joinStyle, endStyle, miterLimit);
  cr := TColorRenderer.Create(color);
  try
    if cr.Initialize(img) then
      Rasterize(lines2, img.bounds, frNonZero, cr);
  finally
    cr.free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawLine(img: TImage32; const lines: TPathsD;
  lineWidth: double; renderer: TCustomRenderer;
  endStyle: TEndStyle; joinStyle: TJoinStyle;
  miterLimit: double);
var
  lines2: TPathsD;
begin
  if (not assigned(lines)) or (not assigned(renderer)) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines2 := Outline(lines, lineWidth, joinStyle, endStyle, miterLimit);
  if renderer.Initialize(img) then
    Rasterize(lines2, img.bounds, frNonZero, renderer);
end;
//------------------------------------------------------------------------------

procedure DrawInvertedLine(img: TImage32;
  const lines: TPathsD; lineWidth: double;
  endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto);
var
  lines2: TPathsD;
  ir: TInverseRenderer;
begin
  if not assigned(lines) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines2 := Outline(lines, lineWidth, joinStyle, endStyle, 2);
  ir := TInverseRenderer.Create;
  try
    if ir.Initialize(img) then
      Rasterize(lines2, img.bounds, frNonZero, ir);
  finally
    ir.free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawDashedLine(img: TImage32; const line: TPathD;
  dashPattern: TArrayOfInteger; patternOffset: PDouble; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  lines: TPathsD;
  cr: TColorRenderer;
begin
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  if not assigned(line) then exit;
  lines := GetDashedPath(line, endStyle = esPolygon, dashPattern, patternOffset);
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

procedure DrawDashedLine(img: TImage32; const lines: TPathsD;
  dashPattern: TArrayOfInteger; patternOffset: PDouble; lineWidth: double;
  color: TColor32; endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  i: integer;
begin
  if not assigned(lines) then exit;
  for i := 0 to high(lines) do
    DrawDashedLine(img, lines[i],
      dashPattern, patternOffset, lineWidth, color, endStyle, joinStyle);
end;
//------------------------------------------------------------------------------

procedure DrawDashedLine(img: TImage32; const line: TPathD;
  dashPattern: TArrayOfInteger; patternOffset: PDouble; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  lines: TPathsD;
begin
  if (not assigned(line)) or (not assigned(renderer)) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines := GetDashedPath(line, endStyle = esPolygon, dashPattern, patternOffset);
  if Length(lines) = 0 then Exit;
  lines := Outline(lines, lineWidth, joinStyle, endStyle);
  if renderer.Initialize(img) then
    Rasterize(lines, img.bounds, frNonZero, renderer);
end;
//------------------------------------------------------------------------------

procedure DrawDashedLine(img: TImage32; const lines: TPathsD;
  dashPattern: TArrayOfInteger; patternOffset: PDouble; lineWidth: double;
  renderer: TCustomRenderer; endStyle: TEndStyle; joinStyle: TJoinStyle);
var
  i: integer;
begin
  if not assigned(lines) then exit;
  for i := 0 to high(lines) do
    DrawDashedLine(img, lines[i],
      dashPattern, patternOffset, lineWidth, renderer, endStyle, joinStyle);
end;
//------------------------------------------------------------------------------

procedure DrawInvertedDashedLine(img: TImage32;
  const line: TPathD; dashPattern: TArrayOfInteger;
  patternOffset: PDouble; lineWidth: double; endStyle: TEndStyle;
  joinStyle: TJoinStyle = jsAuto);
var
  lines: TPathsD;
  renderer: TInverseRenderer;
begin
  if not assigned(line) then exit;
  if (lineWidth < MinStrokeWidth) then lineWidth := MinStrokeWidth;
  lines := GetDashedPath(line, endStyle = esPolygon, dashPattern, patternOffset);
  if Length(lines) = 0 then Exit;
  lines := Outline(lines, lineWidth, joinStyle, endStyle);
  renderer := TInverseRenderer.Create;
  try
    if renderer.Initialize(img) then
      Rasterize(lines, img.bounds, frNonZero, renderer);
  finally
    renderer.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawInvertedDashedLine(img: TImage32;
  const lines: TPathsD; dashPattern: TArrayOfInteger;
  patternOffset: PDouble; lineWidth: double;
  endStyle: TEndStyle; joinStyle: TJoinStyle = jsAuto);
var
  i: integer;
begin
  if not assigned(lines) then exit;
  for i := 0 to high(lines) do
    DrawInvertedDashedLine(img, lines[i],
      dashPattern, patternOffset, lineWidth, endStyle, joinStyle);
end;
//------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; color: TColor32);
var
  polygons: TPathsD;
begin
  if not assigned(polygon) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  DrawPolygon(img, polygons, fillRule, color);
end;
//------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule; renderer: TCustomRenderer);
var
  polygons: TPathsD;
begin
  if (not assigned(polygon)) or (not assigned(renderer)) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  if renderer.Initialize(img) then
    Rasterize(polygons, img.Bounds, fillRule, renderer);
end;
//------------------------------------------------------------------------------

procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
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

procedure DrawPolygon(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; renderer: TCustomRenderer);
begin
  if (not assigned(polygons)) or (not assigned(renderer)) then exit;
  if renderer.Initialize(img) then
    Rasterize(polygons, img.bounds, fillRule, renderer);
end;
//------------------------------------------------------------------------------

procedure DrawPolygon_ClearType(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule; color: TColor32; backColor: TColor32);
var
  tmpImg: TImage32;
  rec: TRect;
  tmpPolygons: TPathsD;
  cr: TColorRenderer;
begin
  if not assigned(polygons) then exit;

  rec := GetBounds(polygons);
  tmpImg := TImage32.Create(RectWidth(rec) *3, RectHeight(rec));
  try
    tmpPolygons := OffsetPath(polygons, -rec.Left, -rec.Top);
    tmpPolygons := ScalePath(tmpPolygons, 3, 1);
    cr := TColorRenderer.Create(clBlack32);
    try
      if cr.Initialize(tmpImg) then
        Rasterize(tmpPolygons, tmpImg.bounds, fillRule, cr);
    finally
      cr.Free;
    end;
    ApplyClearType(tmpImg, color, backColor);
    img.CopyBlend(tmpImg, tmpImg.Bounds, rec, BlendToAlpha);
  finally
    tmpImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure ErasePolygon(img: TImage32; const polygon: TPathD;
  fillRule: TFillRule);
var
  polygons: TPathsD;
begin
  if not assigned(polygon) then exit;
  setLength(polygons, 1);
  polygons[0] := polygon;
  ErasePolygon(img, polygons, fillRule);
end;
//------------------------------------------------------------------------------

procedure ErasePolygon(img: TImage32; const polygons: TPathsD;
  fillRule: TFillRule);
var
  er: TEraseRenderer;
begin
  er := TEraseRenderer.Create;
  try
    if er.Initialize(img) then
      Rasterize(polygons, img.bounds, fillRule, er);
  finally
    er.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawBoolMask(img: TIMage32; const mask: TArrayOfByte; color: TColor32);
var
  i, len: integer;
  pc: PColor32;
  pb: PByte;
begin
  len := Length(mask);
  if (len = 0) or (len <> img.Width * img.Height) then Exit;
  pc := img.PixelBase;
  pb := @mask[0];
  for i := 0 to len -1 do
  begin
    {$IFDEF PBYTE}
    if pb^ > 0 then
    {$ELSE}
    if pb^ > #0 then
    {$ENDIF}
      pc^ := color else
      pc^ := clNone32;
    inc(pc); inc(pb);
  end;
end;
//------------------------------------------------------------------------------

procedure DrawAlphaMask(img: TIMage32; const mask: TArrayOfByte; color: TColor32);
var
  i, len: integer;
  pc: PColor32;
  pb: PByte;
begin
  len := Length(mask);
  if (len = 0) or (len <> img.Width * img.Height) then Exit;
  color := color and $FFFFFF; //strip alpha value
  pc := img.PixelBase;
  pb := @mask[0];
  for i := 0 to len -1 do
  begin
    {$IFDEF PBYTE}
    if pb^ > 0 then
      pc^ := color or pb^ shl 24 else
      pc^ := clNone32;
    {$ELSE}
    if pb^ > #0 then
      pc^ := color or Ord(pb^) shl 24 else
      pc^ := clNone32;
    {$ENDIF}
    inc(pc); inc(pb);
  end;
end;
//------------------------------------------------------------------------------

end.
