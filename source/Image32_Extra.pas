unit Image32_Extra;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.10                                                            *
* Date      :  23 July 2019                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Miscellaneous routines for TImage32 that don't obviously        *
*           :  belong in other modules.                                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Windows, Math, Image32, Image32_Draw;

type
  //TCompareFunction: Function template for FloodFill procedure
  TCompareFunction = function(current, compare: TColor32; data: integer): Boolean;
  TButtonOption = (boSquare, boPressed, boDropShadow);
  TButtonOptions = set of TButtonOption;

procedure DrawShadow(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; depth: double; angleRads: double = angle225;
  color: TColor32 = $80000000; cutoutInsideShadow: Boolean = false); overload;
procedure DrawShadow(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; depth: double; angleRads: double = angle225;
  color: TColor32 = $80000000; cutoutInsideShadow: Boolean = false); overload;

procedure DrawGlow(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer); overload;
procedure DrawGlow(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer); overload;

//FloodFill: If no CompareFunc is provided, FloodFill will fill whereever
//adjoining pixels exactly match the starting pixel - Point(x,y).
procedure FloodFill(img: TImage32; x, y: Integer; newColor: TColor32;
  compareFunc: TCompareFunction = nil; tolerance: Integer = 0);

//RedEyeRemove: Removes 'red eye' from flash photo images.
procedure RedEyeRemove(img: TImage32; const rect: TRect);

procedure Erase(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; inverted: Boolean = false); overload;
procedure Erase(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; inverted: Boolean = false); overload;

procedure Draw3D(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32 = $DDFFFFFF; colorDk: TColor32 = $80000000;
  angleRads: double = angle45); overload;
procedure Draw3D(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32 = $DDFFFFFF; colorDk: TColor32 = $80000000;
  angleRads: double = angle45); overload;

procedure DrawButton(img: TImage32; const pt: TPointD;
  size: double; color: TColor32 = clNone32;
  buttonOptions: TButtonOptions = []);

//FLOODFILL COMPARE COLOR FUNCTIONS ( see FloodFill )

function FloodFillRGB(initial, current: TColor32; tolerance: Integer): Boolean;
function FloodFillHue(initial, current: TColor32; tolerance: Integer): Boolean;

implementation

uses
  Image32_Vector, Image32_Clipper;

resourcestring
  rsDraw3DNeedsNonZeroFill =
    'Draw3D Error: Procedure requires non-zero filling rule.';

const
  FloodFillDefaultRGBTolerance: byte = 20;
  FloodFillDefaultHueTolerance: byte = 1;

type
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0.. maxint div SizeOf(TColor32) -1] of TColor32;

  PFloodFillRec = ^TFloodFillRec;
  TFloodFillRec = record
    xLeft     : Integer;
    xRight    : Integer;
    y         : Integer;
    direction : Integer;
    next      : PFloodFillRec;
  end;

  TFloodFillStack = class
    first     : PFloodFillRec;
    maxY      : Integer;
    constructor Create(maxY: Integer);
    destructor Destroy; override;
    procedure Push(xLeft, xRight,y, direction: Integer);
    procedure Pop(out xLeft, xRight,y, direction: Integer);
    function IsEmpty: Boolean;
  end;

  TFloodFillMask = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    mask         : TArrayOfByte;
    width        : Integer;
    height       : Integer;
    tolerance    : Integer;
    initialColor : TColor32;
    colorsBase   : PColor32Array;
    colorsRow    : PColor32Array;
    maskRow      : PByteArray;
    compareFunc  : TCompareFunction;
    procedure Reset(w, h, x, y: Integer; pixelBase: PColor32;
      compFunc: TCompareFunction; aTolerance: Integer = 0);
    procedure SetCurrentY(y: Integer);
    function IsMatch(x: Integer): Boolean;
  end;

//------------------------------------------------------------------------------
// FloodFill compare functions (examples) ...
//------------------------------------------------------------------------------

function FloodFillRGB(initial, current: TColor32; tolerance: Integer): Boolean;
var
  curr: TARGB absolute current;
  comp: TARGB absolute initial;
begin
  result := (Abs(curr.R - comp.R) + Abs(curr.G - comp.G) +
    Abs(curr.B - comp.B)) div 3 <= tolerance;
end;
//------------------------------------------------------------------------------

function FloodFillHue(initial, current: TColor32; tolerance: Integer): Boolean;
var
  curr, comp: THsl;
  val: Integer;
begin
  curr := RGBtoHsl(current);
  comp := RGBtoHsl(initial);
  if curr.hue > comp.hue then
  begin
    val := curr.hue - comp.hue;
    if val > 127 then val := comp.hue - curr.hue + 255;
  end else
  begin
    val := comp.hue - curr.hue;
    if val > 127 then val := curr.hue - comp.hue + 255;
  end;
  result := val <= tolerance;
end;

//------------------------------------------------------------------------------
// TFloodFillStack methods
//------------------------------------------------------------------------------

constructor TFloodFillStack.Create(maxY: Integer);
begin
  self.maxY := maxY;
end;
//------------------------------------------------------------------------------

destructor TFloodFillStack.Destroy;
var
  ffr: PFloodFillRec;
begin
  while assigned(first) do
  begin
    ffr := first;
    first := first.next;
    dispose(ffr);
  end;
end;
//------------------------------------------------------------------------------

procedure TFloodFillStack.Push(xLeft, xRight, y, direction: Integer);
var
  ffr: PFloodFillRec;
begin
  if ((y = 0) and (direction = -1)) or
    ((y = maxY) and (direction = 1)) then Exit;
  new(ffr);
  ffr.xLeft     := xLeft;
  ffr.xRight    := xRight;
  ffr.y         := y;
  ffr.direction := direction;
  ffr.next := first;
  first := ffr;
end;
//------------------------------------------------------------------------------

procedure TFloodFillStack.Pop(out xLeft, xRight, y, direction: Integer);
var
  ffr: PFloodFillRec;
begin
  xLeft     := first.xLeft;
  xRight    := first.xRight;
  direction := first.direction;
  y         := first.y + direction;
  ffr := first;
  first := first.next;
  dispose(ffr);
end;
//------------------------------------------------------------------------------

function TFloodFillStack.IsEmpty: Boolean;
begin
  result := not assigned(first);
end;

//------------------------------------------------------------------------------
// TFloodFillMask methods
//------------------------------------------------------------------------------

procedure TFloodFillMask.Reset(w, h, x, y: Integer;
  pixelBase: PColor32; compFunc: TCompareFunction; aTolerance: Integer);
begin
   mask := nil; //clear a existing mask

   //create a mask the size of the image
   setLength(mask, w * h);
   Self.width := w;
   Self.height := h;
   colorsBase := PColor32Array(pixelBase);
   Self.initialColor := colorsBase[x + y * w];
   Self.compareFunc := compFunc;
   Self.tolerance := aTolerance;
   //Self.colorsRow and Self.maskRow are left undefined here
end;
//------------------------------------------------------------------------------

procedure TFloodFillMask.SetCurrentY(y: Integer);
begin
  colorsRow := @colorsBase[y * width];
  maskRow := @mask[y * width];
end;
//------------------------------------------------------------------------------

function TFloodFillMask.IsMatch(x: Integer): Boolean;
begin
  result := (maskRow[x] = 0) and
    compareFunc(initialColor, colorsRow[x], tolerance);
  if result then maskRow[x] := 1;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure DrawShadow(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; depth: double; angleRads: double;
  color: TColor32; cutoutInsideShadow: Boolean);
var
  polygons: TArrayOfArrayOfPointD;
begin
  setlength(polygons, 1);
  polygons[0] := polygon;
  DrawShadow(img, polygons, fillRule, depth,
    angleRads, color, cutoutInsideShadow);
end;
//------------------------------------------------------------------------------

procedure DrawShadow(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; depth: double; angleRads: double;
  color: TColor32; cutoutInsideShadow: Boolean);
var
  x,y: extended; //D7 compatible
  blurSize, rpt: integer;
  rec: TRect;
  polys, shadowPolys: TArrayOfArrayOfPointD;
  shadowImg: TImage32;
begin
  rec := GetBounds(polygons);
  if IsEmptyRect(rec) or (depth < 1) then Exit;
  Math.SinCos(-angleRads, y, x);
  x := depth * x;
  y := depth * y;
  blurSize := Max(1,Round(depth / 4));
  if depth <= 2 then rpt :=1 else rpt := 2;
  Windows.InflateRect(rec, Ceil(depth*2), Ceil(depth*2));
  polys := OffsetPath(polygons, -rec.Left, -rec.Top);
  shadowPolys := OffsetPath(polys, x, y);
  shadowImg := TImage32.Create(RectWidth(rec), RectHeight(rec));
  try
    DrawPolygon(shadowImg, shadowPolys, fillRule, color);
    shadowImg.BoxBlur(shadowImg.Bounds, blurSize, rpt);
    if cutoutInsideShadow then
      Erase(shadowImg, polys, fillRule);
    img.CopyFrom(shadowImg, shadowImg.Bounds, rec, BlendToAlpha);
  finally
    shadowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawGlow(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer);
var
  polygons: TArrayOfArrayOfPointD;
begin
  setlength(polygons, 1);
  polygons[0] := polygon;
  DrawGlow(img, polygons, fillRule, color, blurRadius);
end;
//------------------------------------------------------------------------------

procedure DrawGlow(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; color: TColor32; blurRadius: integer);
var
  rec: TRect;
  glowPolys: TArrayOfArrayOfPointD;
  glowImg: TImage32;
begin
  rec := GetBounds(polygons);
  glowPolys := OffsetPath(polygons,
    blurRadius -rec.Left +1, blurRadius -rec.Top +1);
  Windows.InflateRect(rec, blurRadius +1, blurRadius +1);
  glowImg := TImage32.Create(RectWidth(rec), RectHeight(rec));
  try
    DrawPolygon(glowImg, glowPolys, fillRule, color);
    glowImg.boxBlur(glowImg.Bounds, Ceil(blurRadius/3), 2);
    glowImg.ScaleAlpha(4);
    img.CopyFrom(glowImg, glowImg.Bounds, rec, BlendToAlpha);
  finally
    glowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

function GetFloodFillMask(img: TImage32; x, y: Integer;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfByte;
var
  xl, xr, xr2, direction: Integer;
  maxX, maxY: Integer;
  ffs: TFloodFillStack;
  ffm: TFloodFillMask;
begin
  result := nil;
  if (x < 0) or (x >= img.Width) or (y < 0) or (y >= img.Height) then
    Exit;
  maxX := img.Width -1;
  maxY := img.Height -1;

  ffs := TFloodFillStack.create(maxY);
  try
    xl := x; xr := x;
    ffm.Reset(img.Width, img.Height, x, y,
      img.PixelBase, compareFunc, tolerance);
    ffm.SetCurrentY(y);
    ffm.IsMatch(x);

    while (xl > 0) and ffm.IsMatch(xl -1) do dec(xl);
    while (xr < maxX) and ffm.IsMatch(xr +1) do inc(xr);
    ffs.Push(xl, xr, y, -1); //down
    ffs.Push(xl, xr, y, 1);  //up
    while not ffs.IsEmpty do
    begin
      ffs.Pop(xl, xr, y, direction);
      ffm.SetCurrentY(y);
      xr2 := xl;
      //check left ...
      if ffm.IsMatch(xl) then
      begin
        while (xl > 0) and ffm.IsMatch(xl-1) do dec(xl);
        if xl <= xr2 -2 then
          ffs.Push(xl, xr2-2, y, -direction);
        while (xr2 < maxX) and ffm.IsMatch(xr2+1) do inc(xr2);
        ffs.Push(xl,xr2, y, direction);
        if xr2 >= xr +2 then
          ffs.Push(xr+2, xr2, y, -direction);
        xl := xr2 +2;
      end;
      //check right ...
      while (xl <= xr) and not ffm.IsMatch(xl) do inc(xl);
      while (xl <= xr) do
      begin
        xr2 := xl;
        while (xr2 < maxX) and ffm.IsMatch(xr2+1) do inc(xr2);
        ffs.Push(xl, xr2, y, direction);
        if xr2 >= xr +2 then
        begin
          ffs.Push(xr+2,xr2,y, -direction);
          break;
        end;
        inc(xl, 2);
        while (xl <= xr) and not ffm.IsMatch(xl) do inc(xl);
      end;
    end;
    result := ffm.mask;
  finally
    ffs.Free;
  end;
end;
//------------------------------------------------------------------------------

function GetFloodFillBounds(img: TImage32; x,y: Integer;
  const byteArray: TArrayOfByte): TRect;

  function RowHasFill(i: Integer): Boolean;
  var
    pb, pEnd: PByte;
  begin
    result := true;
    pb := @byteArray[i * img.Width];
    pEnd := pb + img.Width;
    while (pb < pEnd) do
      if Ord(pb^) = 1 then Exit
      else inc(pb);
    result := false;
  end;

  function ColHasFill(i: Integer): Boolean;
  var
    pb, pEnd: PByte;
  begin
    result := true;
    pb := @byteArray[i];
    pEnd := @byteArray[length(byteArray)-1];
    while (pb < pEnd) do
      if Ord(pb^) > 0 then Exit
      else inc(pb, img.Width);
    result := false;
  end;

begin
  Result := Rect(x,y, x,y);
  while (Result.Top > 0) and RowHasFill(Result.Top -1) do
    dec(Result.Top);
  while (Result.Bottom < img.Height -1) and RowHasFill(Result.Bottom) do
    inc(Result.Bottom);
  while (Result.Left > 0) and ColHasFill(Result.Left -1) do
    dec(Result.Left);
  while (Result.Right < img.Width -1) and ColHasFill(Result.Right) do
    inc(Result.Right);
end;
//------------------------------------------------------------------------------

procedure FloodFill(img: TImage32; x, y: Integer; newColor: TColor32;
  compareFunc: TCompareFunction; tolerance: Integer);
var
  i: Integer;
  ba: TArrayOfByte;
  pb: PByte;
  pc: PColor32;
begin
  if not assigned(compareFunc) then
  begin
    compareFunc := FloodFillRGB;
    tolerance := FloodFillDefaultRGBTolerance;
  end;

  if (tolerance < 0) then
  begin
    if Addr(compareFunc) = Addr(FloodFillRGB) then
      tolerance := FloodFillDefaultRGBTolerance
    else if Addr(compareFunc) = Addr(FloodFillHue) then
      tolerance := FloodFillDefaultHueTolerance;
  end;

  ba := GetFloodFillMask(img, x, y, compareFunc, tolerance);
  if ba = nil then Exit;
  pb := @ba[0];
  pc := img.PixelBase;
  for i := 0 to High(ba) do
  begin
    if Ord(pb^) > 0 then pc^ := newColor;
    inc(pb); inc(pc);
  end;
end;
//------------------------------------------------------------------------------

procedure RedEyeRemove(img: TImage32; const rect: TRect);
var
  k: integer;
  cutout, mask: TImage32;
  path: TArrayOfPointD;
  cutoutRec, rect3: TRect;
  radGrad: TRadialGradientRenderer;
begin
  k := RectWidth(rect) * RectHeight(rect);
  if k < 120 then k := 2
  else if k < 230 then k := 3
  else k := 4;
  cutoutRec := rect;
  Windows.InflateRect(cutoutRec, k, k);

  cutout  := TImage32.Create(img, cutoutRec);
  mask    := TImage32.Create(cutout.Width, cutout.Height);
  radGrad := TRadialGradientRenderer.Create;
  try
    //fill behind the cutout with black also
    //blurring the fill to soften its edges
    rect3 := cutout.Bounds;
    Windows.InflateRect(rect3, -k, -k);
    path := Ellipse(rect3);
    DrawPolygon(mask, path, frNonZero, clBlack32);
    //given the very small area and small radius of the blur, the
    //speed improvement of BoxBlur over GaussianBlur is inconsequential.
    mask.GaussianBlur(mask.Bounds, k);
    img.CopyFrom(mask, mask.Bounds, cutoutRec, BlendToOpaque);

    //gradient fill to clNone32 a mask to soften cutout's edges
    path := Ellipse(cutoutRec);
    radGrad.SetParameters(rect3, clBlack32, clNone32);
    DrawPolygon(mask, path, frNonZero, radGrad);
    cutout.CopyFrom(mask, mask.Bounds, cutout.Bounds, BlendMask);
    //now remove red from the cutout
    cutout.EraseColor(clRed32);
    //finally replace the cutout ...
    img.CopyFrom(cutout, cutout.Bounds, cutoutRec, BlendToOpaque);
  finally
    mask.Free;
    cutout.Free;
    radGrad.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure Erase(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; inverted: Boolean);
var
  mask: TImage32;
begin
  if not assigned(polygon) then Exit;
  mask := TImage32.Create(img.Width, img.Height);
  try
    DrawPolygon(mask, polygon, fillRule, clBlack32);
    if inverted then
      img.CopyFrom(mask, mask.Bounds, img.Bounds, BlendMask) else
      img.CopyFrom(mask, mask.Bounds, img.Bounds, BlendInvertedMask);
  finally
    mask.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure Erase(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; inverted: Boolean);
var
  mask: TImage32;
begin
  if not assigned(polygons) then Exit;
  mask := TImage32.Create(img.Width, img.Height);
  try
    DrawPolygon(mask, polygons, fillRule, clBlack32);
    if inverted then
      img.CopyFrom(mask, mask.Bounds, img.Bounds, BlendMask) else
      img.CopyFrom(mask, mask.Bounds, img.Bounds, BlendInvertedMask);
  finally
    mask.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure Draw3D(img: TImage32; const polygon: TArrayOfPointD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32; colorDk: TColor32; angleRads: double);
var
  polygons: TArrayOfArrayOfPointD;
begin
  setLength(polygons, 1);
  polygons[0] := polygon;
  Draw3D(img, polygons, fillRule, height, blurRadius, colorLt, colorDk, angleRads);
end;
//------------------------------------------------------------------------------

procedure Draw3D(img: TImage32; const polygons: TArrayOfArrayOfPointD;
  fillRule: TFillRule; height, blurRadius: double;
  colorLt: TColor32; colorDk: TColor32; angleRads: double);
var
  tmp: TImage32;
  recI: TRect;
  recD: TRectD;
  paths, paths2: TArrayOfArrayOfPointD;
  x,y: extended;
begin
  Math.SinCos(angleRads, y, x);
  recD := GetBoundsD(polygons);
  recI := Rect(recD);
  paths := OffsetPath(polygons, -recI.Left, -recI.Top);
  tmp := TImage32.Create(rectWidth(recI), rectHeight(recI));
  try
    if colorLt shr 24 > 0 then
    begin
      tmp.Clear(colorLt);
      paths2 := OffsetPath(paths, -height*x, height*y);
      Erase(tmp, paths2, fillRule);
      tmp.BoxBlur(tmp.Bounds, Round(blurRadius), 2);
      Erase(tmp, paths, fillRule, true);
      img.CopyFrom(tmp, tmp.Bounds, recI, BlendToAlpha);
    end;

    if colorDk shr 24 > 0 then
    begin
      tmp.Clear(colorDk);
      paths2 := OffsetPath(paths, height*x, -height*y);
      Erase(tmp, paths2, fillRule);
      tmp.BoxBlur(tmp.Bounds, Round(blurRadius), 2);
      Erase(tmp, paths, fillRule, true);
      img.CopyFrom(tmp, tmp.Bounds, recI, BlendToAlpha);
    end;
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawButton(img: TImage32; const pt: TPointD;
  size: double; color: TColor32; buttonOptions: TButtonOptions);
var
  path: TArrayOfPointD;
  rec: TRectD;
  shadowSize, shadowAngle: double;
begin
  if (size < 5) then Exit;
  size := size /2;
  shadowSize := size / 4;
  rec := RectD(pt.X -size, pt.Y -size, pt.X +size, pt.Y +size);
  if boSquare in buttonOptions then
    path := Rectangle(rec) else
    path := Ellipse(rec);
  if boPressed in buttonOptions then
    shadowAngle := angle45 else
    shadowAngle := angle225;
  //nb: only need to cutout the inside shadow if
  //the pending color fill is semi-transparent
  if (boDropShadow in buttonOptions) then
    DrawShadow(img, path, frNonZero, shadowSize,
      255, $80000000, color shr 24 < 254);
  if color shr 24 > 2 then
    DrawPolygon(img, path, frNonZero, color);

  DrawLine(img, path, 1, clBlack32, esClosed);
  Draw3D(img, path, frNonZero, shadowSize*2,
    Round(shadowSize), $80000000, $DDFFFFFF, shadowAngle);
end;
//------------------------------------------------------------------------------

end.
