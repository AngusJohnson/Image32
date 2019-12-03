unit Image32_Extra;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.32                                                            *
* Date      :  4 December 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Miscellaneous routines for TImage32 that don't obviously        *
*           :  belong in other modules.                                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Windows, Math, Types,
  Image32, Image32_Draw, Image32_Vector;

type
  //TCompareFunction: Function template for FloodFill procedure
  TCompareFunction = function(master, current: TColor32; data: integer): Boolean;
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

//BoxBlur: With several repetitions and a smaller radius, BoxBlur can
//achieve a close approximation of a GaussianBlur, and it's faster.
procedure BoxBlur(img: TImage32; rect: TRect; radius, repeats: Integer);
procedure GaussianBlur(img: TImage32; rec: TRect; radius: Integer);

//Emboss: A smaller radius is sharper. Increasing depth increases contrast.
//Luminance changes grayscale balance (unless preserveColor = true)
procedure Emboss(img: TImage32; radius: Integer = 1; depth: Integer = 10;
  luminance: Integer = 75; preserveColor: Boolean = false);

//Sharpen: Radius range is 1 - 10; amount range is 1 - 100.<br>
//see https://en.wikipedia.org/wiki/Unsharp_masking
procedure Sharpen(img: TImage32; radius: Integer = 2; amount: Integer = 10);

//HatchBackground: Assumes the current image is semi-transparent.
procedure HatchBackground(img: TImage32; color1: TColor32 = clWhite32;
  color2: TColor32= $FFE8E8E8; hatchSize: Integer = 10);

procedure ReplaceColor(img: TImage32; oldColor, newColor: TColor32);

//EraseColor: Removes the specified color from the image, even from
//pixels that are a blend of colors including the specified color.<br>
//see https://stackoverflow.com/questions/9280902/
procedure EraseColor(img: TImage32; color: TColor32);

//RedEyeRemove: Removes 'red eye' from flash photo images.
procedure RedEyeRemove(img: TImage32; const rect: TRect);

procedure PencilEffect(img: TImage32; intensity: integer = 0);
procedure TraceContours(img: TImage32; intensity: integer);

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

//Vectorize: convert an image into polygon vectors
function Vectorize(img: TImage32; compareColor: TColor32;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfArrayOfPointD;

//COMPARE COLOR FUNCTIONS ( see FloodFill )

function CompareRGB(master, current: TColor32; tolerance: Integer): Boolean;
function CompareHue(master, current: TColor32; tolerance: Integer): Boolean;
function CompareAlpha(master, current: TColor32; tolerance: Integer): Boolean;

function GetMask(img: TImage32; compareColor: TColor32;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfByte;

function GetFloodFillMask(img: TImage32; x, y: Integer;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfByte;

implementation

resourcestring
  rsDraw3DNeedsNonZeroFill =
    'Draw3D Error: Procedure requires non-zero filling rule.';

const
  FloodFillDefaultRGBTolerance: byte = 20;
  FloodFillDefaultHueTolerance: byte = 1;
  MaxBlur = 100;

type
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0.. maxint div SizeOf(TColor32) -1] of TColor32;

  PWeightedColorArray = ^TWeightedColorArray;
  TWeightedColorArray = array [0.. $FFFFFF] of TWeightedColor;

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

function CompareRGB(master, current: TColor32; tolerance: Integer): Boolean;
var
  mast: TARGB absolute master;
  curr: TARGB absolute current;
begin
  if curr.A = 0 then
    Result := false
  else if master and $FFFFFF = current and $FFFFFF then
    Result := true
  else if tolerance = 0 then
    Result := false
  else result :=
    (Abs(curr.R - mast.R) <= tolerance) and
    (Abs(curr.G - mast.G) <= tolerance) and
    (Abs(curr.B - mast.B) <= tolerance);
end;
//------------------------------------------------------------------------------

function CompareAlpha(master, current: TColor32; tolerance: Integer): Boolean;
var
  mast: TARGB absolute master;
  curr: TARGB absolute current;
begin
  if mast.A = curr.A then Result := true
  else if tolerance = 0 then Result := false
  else result := Abs(curr.A - mast.A) <= tolerance;
end;
//------------------------------------------------------------------------------

function CompareHue(master, current: TColor32; tolerance: Integer): Boolean;
var
  curr, mast: THsl;
  val: Integer;
begin
  if TARGB(current).A = 0 then
  begin
    Result := false;
    Exit;
  end;
  curr := RGBtoHsl(current);
  mast := RGBtoHsl(master);
  if curr.hue > mast.hue then
  begin
    val := curr.hue - mast.hue;
    if val > 127 then val := mast.hue - curr.hue + 255;
  end else
  begin
    val := mast.hue - curr.hue;
    if val > 127 then val := curr.hue - mast.hue + 255;
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
    BoxBlur(shadowImg, shadowImg.Bounds, blurSize, rpt);
    if cutoutInsideShadow then
      Erase(shadowImg, polys, fillRule);
    img.CopyBlend(shadowImg, shadowImg.Bounds, rec, BlendToAlpha);
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
    BoxBlur(glowImg, glowImg.Bounds, Ceil(blurRadius/3), 2);
    glowImg.ScaleAlpha(4);
    img.CopyBlend(glowImg, glowImg.Bounds, rec, BlendToAlpha);
  finally
    glowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

function GetMask(img: TImage32; compareColor: TColor32;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfByte;
var
  i: integer;
  pa: PByte;
  pc: PColor32;
begin
  result := nil;
  if not assigned(img) or img.IsEmpty then Exit;
  if not Assigned(compareFunc) then compareFunc := CompareRGB;
  SetLength(Result, img.Width * img.Height);
  pa := @Result[0];
  pc := img.PixelBase;
  for i := 0 to img.Width * img.Height -1 do
  begin
    if compareFunc(compareColor, pc^, tolerance) then
      pa^ := 1 else pa^ := 0;
    inc(pc); inc(pa);
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

  if not Assigned(compareFunc) then compareFunc := CompareRGB;

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
  Result := Types.Rect(x,y, x,y);
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
// Classes used by MaskToPolygons function (vectorization)
//------------------------------------------------------------------------------

type
  TPntsContainer = class;
  TPt = class
    pnt        : TPoint;
    owner      : TPntsContainer;
    isStart    : Boolean; //always ascending
    nextInPath : TPt;
    prevInPath : TPt;
    nextInRow  : TPt;
    prevInRow  : TPt;
    destructor Destroy; override;
    procedure Update(x, y: integer);
    function GetCount: integer;
    procedure Clean1(var count: integer);
    procedure Clean2(var count: integer);
    procedure Clean3(var count: integer);
    function GetPoints: TArrayOfPointD;
  end;

  TPntsContainer = class
    lastRight: integer;
    leftMostPt, rightMost: TPt;
    resultPaths: TArrayOfArrayOfPointD;
    procedure AddToResult(const path: TArrayOfPointD);
    function StartNewPath(insertBefore: TPt;
      xLeft, xRight, y: integer; isHole: Boolean): TPt;
    procedure AddRange(var current: TPt; xLeft, xRight, y: integer);
    procedure JoinAscDesc(path1, path2: TPt);
    procedure JoinDescAsc(path1, path2: TPt);
    procedure CheckRowEnds(leftPt, rightPt: TPt);
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

destructor TPt.Destroy;
var
  startPt, endPt, pt: TPt;
begin
  if not isStart then Exit;
  startPt := self;
  endPt := startPt.prevInPath;

  //remove 'endPt' from double linked list
  if endPt = owner.rightMost then
    owner.rightMost := endPt.prevInRow
  else if assigned(endPt.nextInRow) then
    endPt.nextInRow.prevInRow := endPt.prevInRow;
  if endPt = owner.leftMostPt then
    owner.leftMostPt := endPt.nextInRow
  else if assigned(endPt.prevInRow) then
    endPt.prevInRow.nextInRow := endPt.nextInRow;

  //remove 'startPt' from double linked list
  if startPt = owner.leftMostPt then
    owner.leftMostPt := startPt.nextInRow
  else if assigned(startPt.prevInRow) then
    startPt.prevInRow.nextInRow := startPt.nextInRow;
  if assigned(startPt.nextInRow) then
    startPt.nextInRow.prevInRow := startPt.prevInRow;

  owner.AddToResult(GetPoints);

  //now Free the entire path (except self)
  pt := startPt.nextInPath;
  while pt <> startPt do
  begin
    endPt := pt;
    pt := pt.nextInPath;
    endPt.Free;
  end;
end;
//------------------------------------------------------------------------------

function IsColinear(const pt1, pt2, pt3: TPoint): Boolean; overload;
begin
  //cross product = 0
  result := (pt1.X - pt2.X)*(pt2.Y - pt3.Y) = (pt2.X - pt3.X)*(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function IsColinear(const pt1, pt2, pt3, pt4: TPoint): Boolean; overload;
begin
  result := (pt1.X - pt2.X)*(pt3.Y - pt4.Y) = (pt3.X - pt4.X)*(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

procedure TPt.Update(x, y: integer);
var
  newPt, newPt2: TPt;
begin
  if isStart then
  begin
    if IsColinear(Types.Point(x, y), pnt, nextInPath.pnt) then
    begin
      pnt := Types.Point(x,y);
      Exit;
    end;

    newPt := TPt.Create;
    newPt.pnt := pnt;
    newPt2 := TPt.Create;
    newPt2.pnt := Types.Point(pnt.X, pnt.Y +1);
    pnt := Types.Point(x,y);

    //self -> 2 -> 1 -> nip
    nextInPath.prevInPath := newPt;
    newPt.nextInPath := nextInPath;
    newPt2.nextInPath := newPt;
    newPt.prevInPath := newPt2;
    newPt2.prevInPath := self;
    nextInPath := newPt2;
  end else
  begin
    if IsColinear(Types.Point(x, y), pnt, prevInPath.pnt) then
    begin
      pnt := Types.Point(x,y);
      Exit;
    end;

    newPt := TPt.Create;
    newPt.pnt := pnt;
    newPt2 := TPt.Create;
    newPt2.pnt := Types.Point(pnt.X, pnt.Y +1);
    pnt := Types.Point(x,y);

    //self <- 2 <- 1 <- pip
    newPt.prevInPath := prevInPath;
    prevInPath.nextInPath := newPt;
    newPt2.prevInPath := newPt;
    newPt.nextInPath := newPt2;
    newPt2.nextInPath := self;
    prevInPath := newPt2;
  end;
end;
//------------------------------------------------------------------------------

function TPt.GetCount: integer;
var
  pt: TPt;
begin
  result := 1;
  pt := nextInPath;
  while pt <> self do
  begin
    inc(Result);
    pt := pt.nextInPath;
  end;
end;
//------------------------------------------------------------------------------

procedure TPt.Clean1(var count: integer);

  procedure DeletePt(pt: TPt);
  begin
    pt.prevInPath.nextInPath := pt.nextInPath;
    pt.nextInPath.prevInPath := pt.prevInPath;
    pt.Free;
  end;

var
  p1,p2,p3: TPt;
begin
  //strip micro steps
  p1 := self; p2 := nextInPath; p3 := p2.nextInPath;
  while true do
  begin
    if (abs(p1.pnt.X - p3.pnt.X) = 1) or
      (abs(p1.pnt.Y - p3.pnt.Y) = 1) then
    begin
      if (p2 = self) then Break;
      DeletePt(p2);
      Dec(count);
    end
    else if (abs(p1.pnt.X - p2.pnt.X) <= 1) and
      (abs(p1.pnt.Y - p2.pnt.Y) <= 1) then
    begin
      if (p2 = self) then Break;
      DeletePt(p2);
      Dec(count);
    end;
    p1 := p1.nextInPath; p2 := p1.nextInPath; p3 := p2.nextInPath;
    if p1 = self then break; //circuit completed
  end;
end;
//------------------------------------------------------------------------------

procedure TPt.Clean2(var count: integer);

  procedure DeletePt(pt: TPt);
  begin
    pt.prevInPath.nextInPath := pt.nextInPath;
    pt.nextInPath.prevInPath := pt.prevInPath;
    pt.Free;
  end;

var
  p1,p2,p3: TPt;
begin
  //merge colinear edges
  p1 := self; p2 := nextInPath; p3 := p2.nextInPath;
  while true do
  begin
    if IsColinear(p1.pnt, p2.pnt, p3.pnt) then
    begin
      if (p2 = self) then Break;
      DeletePt(p2);
      Dec(count);
    end
    else
      p1 := p1.nextInPath;
    p2 := p1.nextInPath; p3 := p2.nextInPath;
    if p1 = self then break; //circuit completed
  end;
end;
//------------------------------------------------------------------------------

procedure TPt.Clean3(var count: integer);

  procedure DeletePt(pt: TPt);
  begin
    pt.prevInPath.nextInPath := pt.nextInPath;
    pt.nextInPath.prevInPath := pt.prevInPath;
    pt.Free;
  end;

var
  p1,p2,p3,p4: TPt;
begin
  //strip micro step between colinear steps
  p1 := self; p2 := nextInPath; p3 := p2.nextInPath; p4 := p3.nextInPath;
  while true do
  begin
    if (abs(p2.pnt.X - p3.pnt.X) <= 1) and
      (abs(p2.pnt.Y - p3.pnt.Y) <= 1) and
        IsColinear(p1.pnt, p2.pnt, p3.pnt, p4.pnt) then
    begin
      if (p2 = self) or (p3 = self) then Break;
      DeletePt(p2);
      DeletePt(p3);
      Dec(count, 2);
    end else
      p1 := p1.nextInPath;
    p2 := p1.nextInPath; p3 := p2.nextInPath; p4 := p3.nextInPath;
    if p1 = self then break; //circuit completed
  end;
end;
//------------------------------------------------------------------------------

function TPt.GetPoints: TArrayOfPointD;
var
  i, count: integer;
  pt: TPt;
begin
  Update(pnt.X, pnt.Y+1);
  with prevInPath do Update(pnt.X, pnt.Y+1); //path end
  count := GetCount;
  Clean1(count);
  Clean2(count);
  Clean3(count);
  SetLength(Result, count);

  pt := self;
  for i := 0 to count -1 do
  begin
    Result[i] := PointD(pt.pnt);
    pt := pt.nextInPath;
  end;
end;
//------------------------------------------------------------------------------

procedure TPntsContainer.AddToResult(const path: TArrayOfPointD);
var
  len: integer;
begin
  len := Length(resultPaths);
  SetLength(resultPaths, len + 1);
  resultPaths[len] := path;
end;
//------------------------------------------------------------------------------

function TPntsContainer.StartNewPath(insertBefore: TPt;
  xLeft, xRight, y: integer; isHole: Boolean): TPt;
var
  ptL, ptR: TPt;
begin
  ptL := TPt.Create;
  ptL.owner := self;
  ptL.isStart := not isHole;
  ptL.pnt := Types.Point(xLeft, y);

  ptR := TPt.Create;
  ptR.owner := self;
  ptR.isStart := isHole;
  ptR.pnt := Types.Point(xRight, y);

  ptL.nextInPath := ptR;
  ptL.prevInPath := ptR;
  ptR.nextInPath := ptL;
  ptR.prevInPath := ptL;

  ptL.nextInRow := ptR;
  ptR.prevInRow := ptL;
  if not Assigned(insertBefore) then
  begin
    //must be a new rightMost path
    ptL.prevInRow := rightMost;
    if Assigned(rightMost) then rightMost.nextInRow := ptL;
    ptR.nextInRow := nil;
    rightMost := ptR;
    if not Assigned(leftMostPt) then leftMostPt := ptL;
  end else
  begin
    ptR.nextInRow := insertBefore;
    if leftMostPt = insertBefore then
    begin
      //must be a new leftMostPt path
      leftMostPt := ptL;
      ptL.prevInRow := nil;
    end else
    begin
      ptL.prevInRow := insertBefore.prevInRow;
      insertBefore.prevInRow.nextInRow := ptL;
    end;
    insertBefore.prevInRow := ptR;
  end;
  result := ptR.nextInRow;
end;
//------------------------------------------------------------------------------

procedure TPntsContainer.CheckRowEnds(leftPt, rightPt: TPt);
begin
  if leftPt = leftMostPt then leftMostPt := rightPt.nextInRow;
  if rightPt = rightMost then rightMost := leftPt.prevInRow;
end;
//------------------------------------------------------------------------------

procedure TPntsContainer.JoinAscDesc(path1, path2: TPt);
begin
  CheckRowEnds(path1, path2);
  if path2 = path1.prevInPath then
  begin
    path1.Free;
    Exit;
  end;

  inc(path1.pnt.Y);
  inc(path2.pnt.Y);
  path1.isStart := false;
  //remove path1 from double linked list
  if assigned(path1.nextInRow) then
    path1.nextInRow.prevInRow := path1.prevInRow;
  if assigned(path1.prevInRow) then
    path1.prevInRow.nextInRow := path1.nextInRow;
  //remove path2 from double linked list
  if assigned(path2.nextInRow) then
    path2.nextInRow.prevInRow := path2.prevInRow;
  if assigned(path2.prevInRow) then
    path2.prevInRow.nextInRow := path2.nextInRow;

  path1.prevInPath.nextInPath := path2.nextInPath; //1e.n := 2s
  path2.nextInPath.prevInPath := path1.prevInPath; //2s.p := 1e
  path2.nextInPath := path1;
  path1.prevInPath := path2;
end;
//------------------------------------------------------------------------------

procedure TPntsContainer.JoinDescAsc(path1, path2: TPt);
begin
  CheckRowEnds(path1, path2);
  if path1 = path2.prevInPath then
  begin
    path2.Free;
    Exit;
  end;

  inc(path1.pnt.Y);
  inc(path2.pnt.Y);
  path2.isStart := false;
  //remove path1 'end' from double linked list
  if assigned(path1.nextInRow) then
    path1.nextInRow.prevInRow := path1.prevInRow;
  if assigned(path1.prevInRow) then
    path1.prevInRow.nextInRow := path1.nextInRow;
  //remove path2 'start' from double linked list
  if assigned(path2.nextInRow) then
    path2.nextInRow.prevInRow := path2.prevInRow;
  if assigned(path2.prevInRow) then
    path2.prevInRow.nextInRow := path2.nextInRow;

  path1.nextInPath.prevInPath := path2.prevInPath; //1s.p = 2e
  path2.prevInPath.nextInPath := path1.nextInPath; //2e.n = 1s
  path1.nextInPath := path2;
  path2.prevInPath := path1;
end;
//------------------------------------------------------------------------------

procedure TPntsContainer.AddRange(var current: TPt;
  xLeft, xRight, y: integer);
var
  tmp: TPt;
begin
  if (lastRight > 0) then
  begin
    if xLeft < current.pnt.X then
    begin
      //'current' must be descending and hence <lastRight-xLeft> a hole
      current := StartNewPath(current, lastRight, xLeft, y, true);
      lastRight := xRight;
      Exit;
    end;

    while assigned(current) and assigned(current.nextInRow) and
      (lastRight > current.nextInRow.pnt.X) do
    begin
      tmp := current.nextInRow.nextInRow;
      if current.isStart then
        JoinAscDesc(current, current.nextInRow) else
        JoinDescAsc(current, current.nextInRow);
      current := tmp;
    end;

    if xLeft < current.pnt.X then
    begin
      StartNewPath(current, lastRight, xLeft, y, true);
      current.Update(xRight, y);
      current := current.nextInRow;
      lastRight := 0;
      exit;
    end;

    current.Update(lastRight, y);
    current := current.nextInRow;
    lastRight := 0;
  end;

  while assigned(current) and assigned(current.nextInRow) and
    (xLeft > current.nextInRow.pnt.X) do
  begin
    tmp := current.nextInRow.nextInRow;
    if current.isStart then
      JoinAscDesc(current, current.nextInRow) else
      JoinDescAsc(current, current.nextInRow);
    current := tmp;
  end;

  if not assigned(current) or (xRight < current.pnt.X) then
  begin
    StartNewPath(current, xLeft, xRight, y, false);
  end else
  begin
    //'range' somewhat overlaps one or more paths above
    current.Update(xLeft, y);
    current := current.nextInRow;
    lastRight := xRight;
  end
end;
//------------------------------------------------------------------------------

function VectorizeMask(const mask: TArrayOfByte;
  width: integer): TArrayOfArrayOfPointD;
var
  i,j, len, height, blockStart: integer;
  current, tmp: TPt;
  ba: PByteArray;
  pntsContainer: TPntsContainer;
begin
  Result := nil;
  len := Length(mask);
  if (len = 0) or (width = 0) or (len mod width <> 0) then Exit;
  height := len div width;

  pntsContainer := TPntsContainer.Create;
  try
    for i := 0 to height -1 do
    begin
      ba := @mask[width * i];
      blockStart := -2;
      current := pntsContainer.leftMostPt;
      for j := 0 to width -1 do
      begin
        if (ba[j] > 0) = (blockStart >= 0) then Continue;
        if blockStart >= 0 then
        begin
          pntsContainer.AddRange(current, blockStart, j, i);
          blockStart := -1;
        end else
          blockStart := j;
      end;

      if blockStart >= 0 then
        pntsContainer.AddRange(current, blockStart, width, i);

      if (pntsContainer.lastRight > 0) then
      begin
        while Assigned(current.nextInRow) and
          (pntsContainer.lastRight >= current.nextInRow.pnt.X) do
        begin
          tmp := current.nextInRow.nextInRow;
          if current.isStart then
            pntsContainer.JoinAscDesc(current, current.nextInRow) else
            pntsContainer.JoinDescAsc(current, current.nextInRow);
          current := tmp;
        end;
        current.Update(pntsContainer.lastRight, i);
        current := current.nextInRow;
        pntsContainer.lastRight := 0;
      end;

      while assigned(current) do
      begin
        tmp := current.nextInRow.nextInRow;
        if current.isStart then
          pntsContainer.JoinAscDesc(current, current.nextInRow) else
          pntsContainer.JoinDescAsc(current, current.nextInRow);
        current := tmp;
      end
    end;

    with pntsContainer do
      while Assigned(leftMostPt) do
        if leftMostPt.isStart then
          JoinAscDesc(leftMostPt, leftMostPt.nextInRow) else
          JoinDescAsc(leftMostPt, leftMostPt.nextInRow);

    //Result := resultPaths;
    Result := RamerDouglasPeucker(pntsContainer.resultPaths, 2);
  finally
    pntsContainer.Free;
  end;
end;
//------------------------------------------------------------------------------

function Vectorize(img: TImage32; compareColor: TColor32;
  compareFunc: TCompareFunction; tolerance: Integer): TArrayOfArrayOfPointD;
var
  mask: TArrayOfByte;
begin
  mask := GetMask(img, compareColor, compareFunc, tolerance);
  Result := VectorizeMask(mask, img.Width);
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
    compareFunc := CompareRGB;
    tolerance := FloodFillDefaultRGBTolerance;
  end;

  if (tolerance < 0) then
  begin
    if Addr(compareFunc) = Addr(CompareRGB) then
      tolerance := FloodFillDefaultRGBTolerance
    else if Addr(compareFunc) = Addr(CompareHue) then
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

procedure Sharpen(img: TImage32; radius: Integer; amount: Integer);
var
  i: Integer;
  amt: double;
  weightAmount: array [-255 .. 255] of Integer;
  bmpBlur: TImage32;
  pColor, pBlur: PARGB;
begin
  amt := ClampRange(amount/20, 0.05, 5);
  radius := ClampRange(radius, 1, 10);
  for i := -255 to 255 do
    weightAmount[i] := Round(amt * i);

  bmpBlur := TImage32.Create(img); //clone self
  try
    pColor := PARGB(img.pixelBase);
    //bmpBlur.GaussianBlur(Bounds, radius);
    BoxBlur(bmpBlur, bmpBlur.Bounds, Ceil(radius/4), 3);
    pBlur := PARGB(bmpBlur.pixelBase);
    for i := 1 to img.Width * img.Height do
    begin
      if pColor.A > 0 then
      begin
        pColor.R := ClampByte(pColor.R  + weightAmount[pColor.R - pBlur.R]);
        pColor.G := ClampByte(pColor.G  + weightAmount[pColor.G - pBlur.G]);
        pColor.B := ClampByte(pColor.B  + weightAmount[pColor.B - pBlur.B]);
      end;
      Inc(pColor); Inc(pBlur);
    end;
  finally
    bmpBlur.Free;
  end;
end;
//------------------------------------------------------------------------------

//HatchBackground: Assumes the current image is semi-transparent.
procedure HatchBackground(img: TImage32;
  color1: TColor32; color2: TColor32; hatchSize: Integer);
var
  i,j: Integer;
  pc: PColor32;
  colors: array[boolean] of TColor32;
  hatch: Boolean;
begin
  colors[false] := color1;
  colors[true] := color2;
  pc := img.Pixelbase;
  for i := 0 to img.Height -1 do
  begin
    hatch := Odd(i div hatchSize);
    for j := 0 to img.Width -1 do
    begin
      if (j + 1) mod hatchSize = 0 then hatch := not hatch;
      pc^ := BlendToOpaque(colors[hatch], pc^);
     inc(pc);
    end;
  end;
end;
//------------------------------------------------------------------------------

function ColorDifference(color1, color2: TColor32): cardinal;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
begin
  result := Abs(c1.R - c2.R) + Abs(c1.G - c2.G) + Abs(c1.B - c2.B);
end;
//------------------------------------------------------------------------------

procedure ReplaceColor(img: TImage32; oldColor, newColor: TColor32);
var
  color: PColor32;
  i: Integer;
begin
  color := img.PixelBase;
  for i := 0 to img.Width * img.Height -1 do
  begin
    if color^ = oldColor then color^ := newColor;
    inc(color);
  end;
end;
//------------------------------------------------------------------------------

procedure EraseColor(img: TImage32; color: TColor32);
var
  fg: TARGB absolute color;
  bg: PARGB;
  i: Integer;
  Q: byte;
begin
  if fg.A = 0 then Exit;
  bg := PARGB(img.PixelBase);

  for i := 0 to img.Width * img.Height -1 do
  begin
    if bg.A > 0 then
    begin
      if (bg.R > fg.R) then Q := DivTable[bg.R - fg.R, not fg.R]
      else if (bg.R < fg.R) then Q := DivTable[fg.R - bg.R, fg.R]
      else Q := 0;
      if (bg.G > fg.G) then Q := Max(Q, DivTable[bg.G - fg.G, not fg.G])
      else if (bg.G < fg.G) then Q := Max(Q, DivTable[fg.G - bg.G, fg.G]);
      if (bg.B > fg.B) then Q := Max(Q, DivTable[bg.B - fg.B, not fg.B])
      else if (bg.B < fg.B) then Q := Max(Q, DivTable[fg.B - bg.B, fg.B]);
      if (Q > 0) then
      begin
        bg.A := MulTable[bg.A, Q];
        bg.R := DivTable[bg.R - MulTable[fg.R, not Q], Q];
        bg.G := DivTable[bg.G - MulTable[fg.G, not Q], Q];
        bg.B := DivTable[bg.B - MulTable[fg.B, not Q], Q];
      end else
        bg.Color := clNone32;
    end;
    inc(bg);
  end;
end;
//------------------------------------------------------------------------------

procedure BlurHorizontal(img: TImage32; rect: TRect; radius: Integer);
var
  i, x,y, widthLess1: Integer;
  pc0, pcB, pcF: PColor32;
  wc: TWeightedColor;
  buffer: TArrayOfColor32;
begin
  Windows.IntersectRect(rect, img.Bounds, rect);
  if IsEmptyRect(rect) or (radius < 1) then Exit;
  widthLess1 := RectWidth(rect) -1;
  radius := ClampRange(radius, 1, Min(widthLess1, MaxBlur));
  setLength(buffer, widthLess1 +1);

  for y := 0 to RectHeight(rect) -1 do
  begin
    pc0 := @img.Pixels[(rect.Top + y) * img.Width + rect.Left];
    //copy the row's pixels into a buffer because blurring spoils the color
    //of pixels being removed from the kernel (especially with larger radii).
    Move(pc0^, buffer[0], RectWidth(rect) * SizeOf(TColor32));

    wc.Reset;
    //build the horizontal kernel (wc) using the first pixel in each row ...
    wc.Add(pc0^, 1);
    pcB := @buffer[0]; pcF := pcB;
    for i := 1 to radius do
    begin
      inc(pcF);
      wc.Add(pcF^, 1);
    end;
    pc0^ := wc.Color; //updates the first pixel in the row

    inc(pcF);
    //pcB & pcF now both point to the color buffer, representing the
    //left-most and right-most kernel pixels respectively

    //process the rest of the row, updating the kernel each time - removing
    //the old left-most pixel in the kernel and adding the new right-most one.
    for x := 1 to widthLess1 do
    begin
      if x > radius then
      begin
        wc.Subtract(pcB^, 1);
        inc(pcB);
      end;
      if x < (widthLess1 - radius) then
      begin
        wc.add(pcF^, 1);
        inc(pcF)
      end;
      inc(pc0);
      pc0^ := wc.Color;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure BlurVertical(img: TImage32; rect: TRect; radius: Integer);
var
  i, x,y, heightLess1: Integer;
  pc0, pcB, pcF: PColor32;
  wc: TWeightedColor;
  buffer: TArrayOfColor32;
begin
  heightLess1 := RectHeight(rect) -1;
  radius := ClampRange(radius, 1, Min(heightLess1, MaxBlur));
  setLength(buffer, heightLess1 +1);

  for x := 0 to RectWidth(rect) -1 do
  begin
    pc0 := @img.Pixels[(rect.Top * img.Width) + rect.Left + x];
    //build the vertical pixel buffer ...
    pcF := pc0;
    for i := 0 to heightLess1 do
    begin
      buffer[i] := pcF^;
      inc(pcF, img.Width);
    end;

    wc.Reset;
    wc.Add(pc0^, 1);
    pcB := @buffer[0]; pcF := pcB;
    for i := 1 to radius do
    begin
      inc(pcF);
      wc.Add(pcF^, 1);
    end;
    pc0^ := wc.Color;
    inc(pcF);
    for y := 1 to heightLess1 do
    begin
      if y > radius then
      begin
        wc.Subtract(pcB^, 1);
        inc(pcB);
      end;
      if y < (heightLess1 - radius) then
      begin
        wc.add(pcF^, 1);
        inc(pcF);
      end;
      inc(pc0, img.Width);
      pc0^ := wc.Color;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure BoxBlur(img: TImage32; rect: TRect; radius, repeats: Integer);
begin
  if radius < 1 then Exit;
  for repeats := 0 to repeats do
  begin
    BlurHorizontal(img, rect, radius);
    BlurVertical(img, rect, radius);
  end;
end;
//------------------------------------------------------------------------------

procedure GaussianBlur(img: TImage32; rec: TRect; radius: Integer);
var
  i, x,y,yy,z: Integer;
  gaussTable: array [-MaxBlur .. MaxBlur] of Cardinal;
  wc: TWeightedColor;
  wca: TArrayOfWeightedColor;
  row: PColor32Array;
  wcRow: PWeightedColorArray;
begin
  Windows.IntersectRect(rec, img.Bounds, rec);
  if IsEmptyRect(rec) or (radius < 1) then Exit
  else if radius > MaxBlur then radius := MaxBlur;

  for i := 0 to radius do
  begin
    gaussTable[i] := Sqr(Radius - i +1);
    gaussTable[-i] := gaussTable[i];
  end;

  setLength(wca, RectWidth(rec) * RectHeight(rec));

  for y := 0 to RectHeight(rec) -1 do
  begin
    row := PColor32Array(@img.Pixels[(y + rec.Top) * img.Width + rec.Left]);
    wcRow := PWeightedColorArray(@wca[y * RectWidth(rec)]);
    for x := 0 to RectWidth(rec) -1 do
      for z := max(0, x - radius) to min(img.Width -1, x + radius) do
        wcRow[x].Add(row[z], gaussTable[x-z]);
  end;

  for x := 0 to RectWidth(rec) -1 do
  begin
    for y := 0 to RectHeight(rec) -1 do
    begin
      wc.Reset;
      yy := max(0, y - radius) * RectWidth(rec);
      for z := max(0, y - radius) to min(RectHeight(rec) -1, y + radius) do
      begin
        wc.Add(wca[x + yy].Color, gaussTable[y-z]);
        inc(yy, RectWidth(rec));
      end;
      img.Pixels[x + rec.Left + (y + rec.Top) * img.Width] := wc.Color;
    end;
  end;
end;
//------------------------------------------------------------------------------

function IncPWeightColor(pwc: PWeightedColor; cnt: Integer): PWeightedColor;
begin
  result := PWeightedColor(PByte(pwc) + cnt * SizeOf(TWeightedColor));
end;
//------------------------------------------------------------------------------

function Intensity(color: TColor32): byte;
var
  c: TARGB absolute color;
begin
  Result := (c.R * 61 + c.G * 174 + c.B * 21) shr 8;
end;
//------------------------------------------------------------------------------

function Gray(color: TColor32): TColor32;
var
  c: TARGB absolute color;
  res: TARGB absolute Result;
begin
  res.A := c.A;
  res.R := Intensity(color);
  res.G := res.R;
  res.B := res.R;
end;
//------------------------------------------------------------------------------

procedure Emboss(img: TImage32; radius: Integer;
  depth: Integer; luminance: Integer; preserveColor: Boolean);
var
  yy,xx, x,y, w,h: Integer;
  b: byte;
  kernel: array [0 .. MaxBlur, 0 .. MaxBlur] of Integer;
  wca: TArrayOfWeightedColor;
  pc0, pcf, pcb: PColor32; //pointers to pixels (forward & backward in kernel)
  pw0, pw: PWeightedColor; //pointers to weight
  customGray: TColor32;
  pc: PColor32;
const
  maxDepth = 50;
begin
  //grayscale luminance as percent where 0% is black and 100% is white
  //(luminance is ignored when preserveColor = true)
  luminance := ClampRange(luminance, 0, 100);
  b := luminance *255 div 100;
  customGray := $FF000000 + b shl 16 + b shl 8 + b;

  ClampRange(radius, 1, 5);
  inc(depth);
  ClampRange(depth, 2, maxDepth);

  kernel[0][0] := 1;
  for y := 1 to radius do
    for x := 1 to radius do
      kernel[y][x] := depth;

  w := img.Width; h := img.Height;
  //nb: dynamic arrays are zero-initialized (unless they're a function result)
  SetLength(wca, w * h);

  pc0 := IncPColor32(img.PixelBase, radius * w);
  pw0 := @wca[radius * w];
  for y := radius to h -1 - radius do
  begin
    for x := radius to w -1 - radius do
    begin
      pw := IncPWeightColor(pw0, x);
      pcb := IncPColor32(pc0, x - 1);
      if preserveColor then
      begin
        pcf := IncPColor32(pc0, x);
        pw^.Add(pcf^, kernel[0,0]);
        inc(pcf);
      end else
      begin
        pw^.Add(customGray, kernel[0,0]);
        pcf := IncPColor32(pc0, x + 1);
      end;

      //parse the kernel ...
      for yy := 1 to radius do
      begin
        for xx := 1 to radius do
        begin
          pw^.Subtract(Gray(pcf^), kernel[yy,xx]);
          pw^.Add(Gray(pcb^), kernel[yy,xx]);
          dec(pcb); inc(pcf);
        end;
        dec(pcb, img.Width - radius);
        inc(pcf, img.Width - radius);
      end;
    end;
    inc(pc0, img.Width);
    inc(pw0, img.Width);
  end;

  pc := @img.Pixels[0]; pw := @wca[0];
  for x := 0 to img.width * img.Height - 1 do
  begin
    pc^ := pw.Color or $FF000000;
    inc(pc); inc(pw);
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
    GaussianBlur(mask, mask.Bounds, k);
    img.CopyBlend(mask, mask.Bounds, cutoutRec, BlendToOpaque);

    //gradient fill to clNone32 a mask to soften cutout's edges
    path := Ellipse(cutoutRec);
    radGrad.SetParameters(rect3, clBlack32, clNone32);
    DrawPolygon(mask, path, frNonZero, radGrad);
    cutout.CopyBlend(mask, mask.Bounds, cutout.Bounds, BlendMask);
    //now remove red from the cutout
    EraseColor(cutout, clRed32);
    //finally replace the cutout ...
    img.CopyBlend(cutout, cutout.Bounds, cutoutRec, BlendToOpaque);
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
      img.CopyBlend(mask, mask.Bounds, img.Bounds, BlendMask) else
      img.CopyBlend(mask, mask.Bounds, img.Bounds, BlendInvertedMask);
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
      img.CopyBlend(mask, mask.Bounds, img.Bounds, BlendMask) else
      img.CopyBlend(mask, mask.Bounds, img.Bounds, BlendInvertedMask);
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
      BoxBlur(tmp, tmp.Bounds, Round(blurRadius), 2);
      Erase(tmp, paths, fillRule, true);
      img.CopyBlend(tmp, tmp.Bounds, recI, BlendToAlpha);
    end;

    if colorDk shr 24 > 0 then
    begin
      tmp.Clear(colorDk);
      paths2 := OffsetPath(paths, height*x, -height*y);
      Erase(tmp, paths2, fillRule);
      BoxBlur(tmp, tmp.Bounds, Round(blurRadius), 2);
      Erase(tmp, paths, fillRule, true);
      img.CopyBlend(tmp, tmp.Bounds, recI, BlendToAlpha);
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
  size := size * 0.5;
  shadowSize := size * 0.25;
  rec := RectD(pt.X -size, pt.Y -size, pt.X +size, pt.Y +size);
  if boSquare in buttonOptions then
  begin
    path := Rectangle(rec);
    rec := InflateRect(rec, -1,-1);
  end else
    path := Ellipse(rec);
  if boPressed in buttonOptions then
    shadowAngle := angle45 else
    shadowAngle := angle225;
  //nb: only need to cutout the inside shadow if
  //the pending color fill is semi-transparent
  if (boDropShadow in buttonOptions) then
    DrawShadow(img, path, frNonZero, shadowSize,
      255, $AA000000, color shr 24 < 254);
  if color shr 24 > 2 then
    DrawPolygon(img, path, frNonZero, color);

  Draw3D(img, path, frNonZero, shadowSize*2,
    Ceil(shadowSize), $AA000000, $CCFFFFFF, shadowAngle);
  DrawLine(img, path, 1, clBlack32, esClosed);

  if not (boSquare in buttonOptions) then Exit;
  path := Rectangle(rec);
  setLength(path, 3);
  DrawLine(img, path, 0.75, clWhite32, esSquare);
end;
//------------------------------------------------------------------------------

function AlphaAverage(color1, color2: TColor32): cardinal;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
begin
  result := c1.A + c2.A shr 1;
end;
//------------------------------------------------------------------------------

function BlendLinearBurn(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  res.A := 255;
  res.R := Max(0, bg.R + fg.R - 255);
  res.G := Max(0, bg.G + fg.G - 255);
  res.B := Max(0, bg.B + fg.B - 255);
end;
//------------------------------------------------------------------------------

function BlendColorDodge(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
begin
  res.A := 255;
  res.R := DivTable[bg.R, not fg.R];
  res.G := DivTable[bg.G, not fg.G];
  res.B := DivTable[bg.B, not fg.B];
end;
//------------------------------------------------------------------------------

procedure PencilEffect(img: TImage32; intensity: integer);
var
  w,h, rpt: integer;
  img2: TImage32;
begin
  w := img.Width; h := img.Height;
  if w * h = 0 then Exit;

  rpt := max(0, min(3, intensity));
  intensity := max(1, min(10, intensity));
  img.Grayscale;
  img2 := TImage32.Create(img);
  try
    img2.InvertColors;
    BoxBlur(img2, img2.Bounds, intensity * 3, rpt);
    img.CopyBlend(img2, img2.Bounds, img.Bounds, BlendColorDodge);
  finally
    img2.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TraceContours(img: TImage32; intensity: integer);
var
  i,j, w,h: integer;
  tmp, tmp2: TArrayOfColor32;
  s: PColor32;
  d: PARGB;
begin
  w := img.Width; h := img.Height;
  if w * h = 0 then Exit;
  SetLength(tmp, w * h);
  SetLength(tmp2, w * h);
  s := img.PixelRow[0]; d := @tmp[0];
  for j := 0 to h-1 do
  begin
    for i := 0 to w-2 do
    begin
      d.A := Min($FF, ColorDifference(s^, IncPColor32(s, 1)^));
      inc(s); inc(d);
    end;
    inc(s); inc(d);
  end;

  for j := 0 to w-1 do
  begin
    s := @tmp[j]; d := @tmp2[j];
    for i := 0 to h-2 do
    begin
      d.A := Min($FF, AlphaAverage(s^, IncPColor32(s, w)^));
      inc(s, w); inc(d, w);
    end;
  end;
  Move(tmp2[0], img.PixelBase^, w * h * sizeOf(TColor32));

  if intensity < 1 then Exit;
  if intensity > 10 then
    intensity := 10; //range = 1-10
  img.ScaleAlpha(intensity);
end;
//------------------------------------------------------------------------------

end.
