unit Image32_Transform;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.19                                                            *
* Date      :  21 March 2021                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  Affine and projective transformation routines for TImage32      *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Math, Types, Image32, Image32_Vector;

type
  TMatrixD = array [0..2, 0..2] of double;

  //Matrix functions
  function IsIdentityMatrix(const matrix: TMatrixD): Boolean;
  function Matrix(const m00, m01, m02, m10, m11, m12, m20, m21, m22: double): TMatrixD;
  function MatrixDeterminant(const matrix: TMatrixD): double;
  function MatrixAdjugate(const matrix: TMatrixD): TMatrixD;
  function MatrixMultiply(const modifier, matrix: TMatrixD): TMatrixD;

  procedure MatrixApply(const matrix: TMatrixD;
    var x, y: double); overload; {$IFDEF INLINE} inline; {$ENDIF}
  procedure MatrixApply(const matrix: TMatrixD;
    var pt: TPointD); overload; {$IFDEF INLINE} inline; {$ENDIF}
  procedure MatrixApply(const matrix: TMatrixD;
    var path: TPathD); overload;
  procedure MatrixApply(const matrix: TMatrixD;
    var paths: TPathsD); overload;
  procedure MatrixInvert(var matrix: TMatrixD);

  //MatrixSkew: dx represents the delta offset of an X coordinate as a
  //fraction of its Y coordinate, and likewise for dy. For example, if dx = 0.1
  //and dy = 0, and the matrix is applied to the coordinate [20,15], then the
  //transformed coordinate will become [20 + (15 * 0.1),10], ie [21.5,10].
  procedure MatrixSkew(var matrix: TMatrixD; dx, dy: double);
  procedure MatrixScale(var matrix: TMatrixD; scale: double); overload;
  procedure MatrixScale(var matrix: TMatrixD; scaleX, scaleY: double); overload;
  procedure MatrixRotate(var matrix: TMatrixD;
    const center: TPointD; angRad: double);
  procedure MatrixTranslate(var matrix: TMatrixD; dx, dy: double);

  procedure AffineTransformImage(img: TImage32; matrix: TMatrixD); overload;
  procedure AffineTransformImage(img: TImage32;
    matrix: TMatrixD; out offset: TPoint); overload;

  //ProjectiveTransform:
  //  srcPts, dstPts => each path must contain 4 points
  //  margins => the margins around dstPts (in the dest. projective).
  //  Margins are only meaningful when srcPts are inside the image.
  function ProjectiveTransform(img: TImage32;
    const srcPts, dstPts: TPathD; const margins: TRect): Boolean;

  function SplineVertTransform(img: TImage32; const topSpline: TPathD;
    splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
    out offset: TPoint): Boolean;
  function SplineHorzTransform(img: TImage32; const leftSpline: TPathD;
    splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
    out offset: TPoint): Boolean;

const
  IdentityMatrix: TMatrixD = ((1, 0, 0),(0, 1, 0),(0, 0, 1));

implementation

resourcestring
  rsInvalidScale   = 'Invalid matrix scaling factor (0)';

//------------------------------------------------------------------------------
// Matrix functions
//------------------------------------------------------------------------------

function IsIdentityMatrix(const matrix: TMatrixD): Boolean;
var
  i,j: integer;
const
  matVal: array [boolean] of double = (0.0, 1.0);
begin
  result := false;
  for i := 0 to 2 do
    for j := 0 to 2 do
      if matrix[i][j] <> matVal[j=i] then Exit;
  Result := true;
end;
//------------------------------------------------------------------------------

function Matrix(const m00, m01, m02, m10, m11, m12, m20, m21, m22: double): TMatrixD;
begin
  Result[0,0] := m00; Result[0,1] := m01; Result[0,2] := m02;
  Result[1,0] := m10; Result[1,1] := m11; Result[1,2] := m12;
  Result[2,0] := m20; Result[2,1] := m21; Result[2,2] := m22;
end;
//------------------------------------------------------------------------------

function Det4(a1, a2, b1, b2: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := a1 * b2 - a2 * b1;
end;
//------------------------------------------------------------------------------

function Det9(a1, a2, a3, b1, b2, b3, c1, c2, c3: double): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := a1 * Det4(b2, b3, c2, c3) -
            b1 * Det4(a2, a3, c2, c3) +
            c1 * Det4(a2, a3, b2, b3);
end;
//------------------------------------------------------------------------------

function MatrixDeterminant(const matrix: TMatrixD): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Det9(matrix[0,0], matrix[1,0], matrix[2,0],
                 matrix[0,1], matrix[1,1], matrix[2,1],
                 matrix[0,2], matrix[1,2], matrix[2,2]);
end;
//------------------------------------------------------------------------------

function MatrixAdjugate(const matrix: TMatrixD): TMatrixD;
begin
  //https://en.wikipedia.org/wiki/Adjugate_matrix
  Result[0,0] :=  Det4(matrix[1,1], matrix[1,2], matrix[2,1], matrix[2,2]);
  Result[0,1] := -Det4(matrix[0,1], matrix[0,2], matrix[2,1], matrix[2,2]);
  Result[0,2] :=  Det4(matrix[0,1], matrix[0,2], matrix[1,1], matrix[1,2]);

  Result[1,0] := -Det4(matrix[1,0], matrix[1,2], matrix[2,0], matrix[2,2]);
  Result[1,1] :=  Det4(matrix[0,0], matrix[0,2], matrix[2,0], matrix[2,2]);
  Result[1,2] := -Det4(matrix[0,0], matrix[0,2], matrix[1,0], matrix[1,2]);

  Result[2,0] :=  Det4(matrix[1,0], matrix[1,1], matrix[2,0], matrix[2,1]);
  Result[2,1] := -Det4(matrix[0,0], matrix[0,1], matrix[2,0], matrix[2,1]);
  Result[2,2] :=  Det4(matrix[0,0], matrix[0,1], matrix[1,0], matrix[1,1]);
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var x, y: double);
var
  tmpX: double;
begin
  tmpX := x;
  x := tmpX * matrix[0, 0] + y * matrix[1, 0] + matrix[2, 0];
  y := tmpX * matrix[0, 1] + y * matrix[1, 1] + matrix[2, 1];
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var pt: TPointD);
var
  tmpX: double;
begin
  tmpX := pt.x;
  pt.X := tmpX * matrix[0, 0] + pt.Y * matrix[1, 0] + matrix[2, 0];
  pt.Y := tmpX * matrix[0, 1] + pt.Y * matrix[1, 1] + matrix[2, 1];
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var path: TPathD);
var
  i, len: integer;
  tmpX: double;
  pp: PPointD;
begin
  len := Length(path);
  if (len = 0) or IsIdentityMatrix(matrix) then Exit;
  pp := @path[0];
  for i := 0 to len -1 do
  begin
    tmpX := pp.X;
    pp.X := tmpX * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
    pp.Y := tmpX * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
    inc(pp);
  end;
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var paths: TPathsD);
var
  i,j,len: integer;
  tmpX: double;
  pp: PPointD;
begin
  if IsIdentityMatrix(matrix) then Exit;
  for i := 0 to High(paths) do
  begin
    len := Length(paths[i]);
    if len = 0 then Continue;
    pp := @paths[i][0];
    for j := 0 to High(paths[i]) do
    begin
      tmpX := pp.X;
      pp.X := tmpX * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
      pp.Y := tmpX * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
      inc(pp);
    end;
  end;
end;
//------------------------------------------------------------------------------

function MatrixMultiply(const modifier, matrix: TMatrixD): TMatrixD;
var
  i, j: Integer;
begin
//  if (modifier[2][2] <> 1) or (matrix[2][2] <> 1) then
//    raise Exception.Create(rsInvalidMatrix);
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] :=
        (modifier[0, j] * matrix[i, 0]) +
        (modifier[1, j] * matrix[i, 1]) +
        (modifier[2, j] * matrix[i, 2]);
end;
//------------------------------------------------------------------------------

procedure MatrixScale(var matrix: TMatrixD; scaleX, scaleY: double);
var
  m: TMatrixD;
begin
  m := IdentityMatrix;
  if (scaleX = 0) or (scaleY = 0) then
    raise Exception(rsInvalidScale);

  if ValueAlmostOne(scaleX) and ValueAlmostOne(scaleY) then Exit;
  m[0, 0] := scaleX;
  m[1, 1] := scaleY;
  matrix := MatrixMultiply(m, matrix);
end;
//------------------------------------------------------------------------------

procedure MatrixScale(var matrix: TMatrixD; scale: double);
begin
  MatrixScale(matrix, scale, scale);
end;
//------------------------------------------------------------------------------

procedure MatrixRotate(var matrix: TMatrixD;
  const center: TPointD; angRad: double);
var
  m: TMatrixD;
  sinA, cosA: double;
  origOffset: Boolean;
begin
  NormalizeAngle(angRad);
  if angRad = 0 then Exit;
  if not ClockwiseRotationIsAnglePositive then angRad := -angRad;
  m := IdentityMatrix;
  origOffset := (center.X <> 0) or (center.Y <> 0);
  if origOffset then MatrixTranslate(matrix, -center.X, -center.Y);
  GetSinCos(-angRad, sinA, cosA); //negated angle because of inverted Y-axis.
  m := IdentityMatrix;
  m[0, 0] := cosA;   m[1, 0] := sinA;
  m[0, 1] := -sinA;  m[1, 1] := cosA;
  matrix := MatrixMultiply(m, matrix);
  if origOffset then MatrixTranslate(matrix, center.X, center.Y);
end;
//------------------------------------------------------------------------------

procedure MatrixTranslate(var matrix: TMatrixD; dx, dy: double);
var
  m: TMatrixD;
begin
  if ValueAlmostZero(dx) and ValueAlmostZero(dy) then Exit;
  m := IdentityMatrix;
  m[2, 0] := dx;
  m[2, 1] := dy;
  matrix := MatrixMultiply(m, matrix);
end;
//------------------------------------------------------------------------------

procedure ScaleInternal(var matrix: TMatrixD; s: double);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      matrix[i,j] := matrix[i,j] * s;
end;
//------------------------------------------------------------------------------

procedure MatrixInvert(var matrix: TMatrixD);
var
  d: double;
const
  tolerance = 1.0E-5;
begin
  d := MatrixDeterminant(matrix);
  if abs(d) > tolerance then
  begin
    matrix := MatrixAdjugate(matrix);
    ScaleInternal(matrix, 1/d);
  end
  else matrix := IdentityMatrix;
end;
//------------------------------------------------------------------------------

procedure MatrixSkew(var matrix: TMatrixD; dx, dy: double);
var
  m: TMatrixD;
begin
  if ValueAlmostZero(dx) and ValueAlmostZero(dy) then Exit;
  m := IdentityMatrix;
  m[1, 0] := dx;
  m[0, 1] := dy;
  matrix := MatrixMultiply(m, matrix);
end;

//------------------------------------------------------------------------------
// Affine Transformation
//------------------------------------------------------------------------------

function GetTransformBounds(img: TImage32; const matrix: TMatrixD): TRect;
var
  pts: TPathD;
begin
  pts := Rectangle(img.Bounds);
  MatrixApply(matrix, pts);
  Result := GetBounds(pts);
end;
//------------------------------------------------------------------------------

procedure AffineTransformImage(img: TImage32; matrix: TMatrixD);
var
  dummy: TPoint;
begin
  AffineTransformImage(img, matrix, dummy);
end;
//------------------------------------------------------------------------------

procedure AffineTransformImage(img: TImage32;
  matrix: TMatrixD; out offset: TPoint); overload;
var
  i,j, dstWidth,dstHeight, dstOffsetX,dstOffsetY, srcWidth, srcHeight, xi,yi: integer;
  x, y: double;
  pc: PColor32;
  tmp: TArrayOfColor32;
  rec: TRect;
begin
  offset := NullPoint;
  srcWidth := img.Width;
  srcHeight := img.Height;
  if (srcWidth * srcHeight = 0) or IsIdentityMatrix(matrix) then Exit;
  rec := GetTransformBounds(img, matrix);
  offset := rec.TopLeft; //out parameter
  dstWidth := RectWidth(rec);
  dstHeight := RectHeight(rec);

  //starting with the result pixel coords, reverse find
  //the fractional coordinates in the current image
  MatrixInvert(matrix);
  SetLength(tmp, dstWidth * dstHeight);
  pc := @tmp[0];
  dstOffsetX := rec.Left; dstOffsetY := rec.Top;
  if img.AntiAliased then
  begin
    for i := dstOffsetY to + dstOffsetY + dstHeight -1 do
      for j := dstOffsetX to dstOffsetX + dstWidth -1 do
      begin
        //convert dest X,Y to src X,Y ...
        x := j; y := i;
        MatrixApply(matrix, x, y);
        //get weighted pixel (slow)
        pc^ := GetWeightedPixel(img, Round(x * 256), Round(y * 256));
        inc(pc);
      end;
  end else
  begin
    for i := dstOffsetY to dstOffsetY + dstHeight -1 do
      for j := dstOffsetX to dstOffsetX + dstWidth -1 do
      begin
        //convert dest X,Y to src X,Y ...
        x := j; y := i;
        MatrixApply(matrix, x, y);
        //get nearest pixel (fast)
        xi := Round(x); yi := Round(y);
        if (xi < 0) or (xi >= srcWidth) or (yi < 0) or (yi >= srcHeight) then
          pc^ := clNone32 else
          pc^ := img.Pixel[xi, yi];
        inc(pc);
      end;
  end;
  img.BeginUpdate;
  try
    img.SetSize(dstWidth, dstHeight);
    Move(tmp[0], img.Pixels[0], dstWidth * dstHeight * sizeOf(TColor32));
  finally
    img.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
// Projective Transformation
//------------------------------------------------------------------------------

procedure MatrixMulCoord(const matrix: TMatrixD; var x,y,z: double);
{$IFDEF INLINE} inline; {$ENDIF}
var
  xx, yy: double;
begin
  xx := x; yy := y;
  x := matrix[0,0] *xx + matrix[0,1] *yy + matrix[0,2] *z;
  y := matrix[1,0] *xx + matrix[1,1] *yy + matrix[1,2] *z;
  z := matrix[2,0] *xx + matrix[2,1] *yy + matrix[2,2] *z;
end;
//------------------------------------------------------------------------------

function BasisToPoints(x1, y1, x2, y2, x3, y3, x4, y4: double): TMatrixD;
var
  m, m2: TMatrixD;
  z4: double;
begin
  m := Matrix(x1, x2, x3, y1, y2, y3, 1,  1,  1);
  m2 := MatrixAdjugate(m);
  z4 := 1;
  MatrixMulCoord(m2, x4, y4, z4);
  m2 := Matrix(x4, 0, 0, 0, y4, 0, 0, 0, z4);
  Result := MatrixMultiply(m2, m);
end;
//------------------------------------------------------------------------------

procedure GetSrcCoords256(const matrix: TMatrixD; var x, y: integer);
{$IFDEF INLINE} inline; {$ENDIF}
var
  xx,yy,zz: double;
const
  Q: integer = MaxInt div 256;
begin
  //returns coords multiplied by 256 in anticipation of the following
  //GetWeightedPixel function call which in turn expects the lower 8bits
  //of the integer coord value to represent a fraction.
  xx := x; yy := y; zz := 1;
  MatrixMulCoord(matrix, xx, yy, zz);

  if zz = 0 then
  begin
    if xx >= 0 then x := Q else x := -MaxInt;
    if yy >= 0 then y := Q else y := -MaxInt;
  end else
  begin
    xx := xx/zz;
    if xx > Q then x := MaxInt
    else if xx < -Q then x := -MaxInt
    else x := Round(xx *256);

    yy := yy/zz;
    if yy > Q then y := MaxInt
    else if yy < -Q then y := -MaxInt
    else y := Round(yy *256);
  end;
end;
//------------------------------------------------------------------------------

function GetProjectionMatrix(const srcPts, dstPts: TPathD): TMatrixD;
var
  srcMat, dstMat: TMatrixD;
begin
  if (length(srcPts) <> 4) or (length(dstPts) <> 4) then
  begin
    Result := IdentityMatrix;
    Exit;
  end;
  srcMat := BasisToPoints(srcPts[0].X, srcPts[0].Y,
    srcPts[1].X, srcPts[1].Y, srcPts[2].X, srcPts[2].Y, srcPts[3].X, srcPts[3].Y);
  dstMat := BasisToPoints(dstPts[0].X, dstPts[0].Y,
    dstPts[1].X, dstPts[1].Y, dstPts[2].X, dstPts[2].Y, dstPts[3].X, dstPts[3].Y);
  Result := MatrixMultiply(MatrixAdjugate(dstMat), srcMat);
end;
//------------------------------------------------------------------------------

function ProjectiveTransform(img: TImage32;
  const srcPts, dstPts: TPathD; const margins: TRect): Boolean;
var
  w,h,i,j: integer;
  x,y: integer;
  rec: TRect;
  dstPts2: TPathD;
  mat: TMatrixD;
  tmp: TArrayOfColor32;
  pc: PColor32;
begin
  //https://math.stackexchange.com/a/339033/384709

  Result := not img.IsEmpty and
    (Length(dstPts) = 4) and IsPathConvex(dstPts);
  if not Result then Exit;

  rec := GetBounds(dstPts);
  dec(rec.Left, margins.Left);
  dec(rec.Top, margins.Top);
  inc(rec.Right, margins.Right);
  inc(rec.Bottom, margins.Bottom);
  dstPts2 := OffsetPath(dstPts, -rec.Left, -rec.Top);

  mat := GetProjectionMatrix(srcPts, dstPts2);
  w := RectWidth(rec);
  h := RectHeight(rec);
  SetLength(tmp, w * h);
  pc := @tmp[0];
  for i :=  0 to h -1 do
    for j := 0 to w -1 do
    begin
      x := j; y := i;
      GetSrcCoords256(mat, x, y);
      pc^ := GetWeightedPixel(img, x, y);
      inc(pc);
    end;
  img.SetSize(w, h);
  Move(tmp[0], img.PixelBase^, w * h * sizeOf(TColor32));
end;

//------------------------------------------------------------------------------
// Spline transformations
//------------------------------------------------------------------------------

function ReColor(color, newColor: TColor32): TColor32;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Alpha(color) or NoAlpha(newColor);
end;
//------------------------------------------------------------------------------

function Interpolate(const pt1, pt2: TPointD; frac: double): TPointD;
begin
  if frac <= 0 then Result := pt1
  else if frac >= 1 then Result := pt2
  else
  begin
    result.X := pt1.X + frac * (pt2.X - pt1.X);
    result.Y := pt1.Y + frac * (pt2.Y - pt1.Y);
  end;
end;
//------------------------------------------------------------------------------

function InterpolateSegment(const pt1, pt2: TPointD): TPathD;
var
  i, len: integer;
  x,y,dx,dy: double;
begin
  len := Ceil(Distance(pt1, pt2));
  SetLength(Result, len);
  dy := (pt2.Y - pt1.Y)/ len;
  dx := (pt2.X - pt1.X)/ len;
  x := pt1.X; y := pt1.Y;
  for i := 0 to len -1 do
  begin
    x := x + dx; y := y + dy;
    Result[i] := PointD(x, y);
  end;
end;
//------------------------------------------------------------------------------

function InterpolatePath(const path: TPathD): TPathD;
var
  i,len,len2: integer;
  tmp: TPathD;
begin
  //returns a coordinate array for every value of X and y along the path based
  //on 2D distance. (This is a sadly only a poor approximation to perspective
  //distance - eg with tight bezier curves).
  len := length(path);
  setLength(result, 0);
  for i := 1 to len -1 do
  begin
    tmp := InterpolateSegment(path[i-1], path[i]);
    len := Length(Result);
    len2 := Length(tmp);
    SetLength(Result, len + len2);
    Move(tmp[0], Result[len], len2 * SizeOf(TPointD));
  end;
end;
//------------------------------------------------------------------------------

function SplineVertTransform(img: TImage32; const topSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
  out offset: TPoint): Boolean;
var
  t,u,v, i,j, x,len, w,h: integer;
  prevX: integer;
  dx, dy, y, sy: double;
  topPath, botPath: TPathD;
  rec: TRect;
  scaleY: TArrayOfDouble;
  pc: PColor32;
  tmp: TArrayOfColor32;
  backColoring, allowBackColoring: Boolean;
begin
  offset := NullPoint;
  //convert the top spline control points into a flattened path
  if splineType = stQuadratic then
    topPath := FlattenQSpline(topSpline) else
    topPath := FlattenCSpline(topSpline);

  rec := GetBounds(topPath);
  //return false if the spline is invalid or there's no vertical transformation
  Result := not IsEmptyRect(rec);
  if not Result then Exit;

  offset := rec.TopLeft;
  topPath := OffsetPath(topPath, -rec.Left, -rec.Top);
  //'Interpolate' the path so that there's a coordinate for every rounded
  //X and Y between the start and end of the flattened spline path. This
  //is to give a very rough approximatation of distance, so that the image
  //is roughly proportionally spaced even when the spline causes overlap.
  topPath := InterpolatePath(topPath);
  botPath := OffsetPath(topPath, 0, img.Height);
  Image32_Vector.OffsetRect(rec, -rec.Left, -rec.Top);
  rec := Rect(UnionRect(RectD(rec), GetBoundsD(botPath)));
  w := RectWidth(rec); h := RectHeight(rec);
  len  := Length(topPath);

  setLength(scaleY, len);
  for i := 0 to len -1 do
    if botPath[i].Y <= topPath[i].Y then
      scaleY[i] := 0 else
      scaleY[i] := img.Height/ (botPath[i].Y - topPath[i].Y);

  dx := (img.Width / len) * 256;
  SetLength(tmp, w * h);

  if reverseFill then
  begin
    //ie fill from right-to-left or bottom-to-top
    t := -1; u := len -1; v := -1;
  end else
  begin
    t := 1; u := 0; v := len;
  end;
  prevX := u - t;

  backColoring := false;
  allowBackColoring := (backColor shr 24) > 2;
  while u <> v do
  begin
    //dst x:
    x := Round(topPath[u].X);
    if x >= w then begin inc(u, t); Continue; end;

    //check if reversing fill direction - ie folding overlap
    if allowBackColoring then
    begin
      if (x <> prevX) then
      begin
        if reverseFill then
          backColoring := prevX < x else
          backColoring := prevX > x;
      end
      else if (Abs(u -v) > 1) then //ie it's safe to look ahead
      begin
        if reverseFill then
          backColoring := prevX < topPath[u+t].X else
          backColoring := prevX > topPath[u+t].X;
      end;
    end;
    prevX := x;
    pc := @tmp[x];

    //src x:
    x := Round(u * dx);

    dy := topPath[u].Y;
    sy :=  scaleY[u];
    for j := 0 to h -1 do
    begin
      y := (j - dy) * sy;

      if backColoring then
        pc^ := BlendToAlpha(pc^,
          ReColor(GetWeightedPixel(img, x, Round(y * 256)), backColor))
      else
        //blend in case spline causes folding overlap
        pc^ := BlendToAlpha(pc^,
          GetWeightedPixel(img, x, Round(y * 256)));
      inc(pc, w);
    end;
    inc(u, t);
  end;
  img.SetSize(w, h);
  Move(tmp[0], img.PixelBase^, w * h * SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

function SplineHorzTransform(img: TImage32; const leftSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
  out offset: TPoint): Boolean;
var
  t,u,v, i,j, y,prevY, len, w,h: integer;
  x, dx,dy,sx: double;
  leftPath, rightPath: TPathD;
  rec: TRect;
  scaleX: TArrayOfDouble;
  pc: PColor32;
  tmp: TArrayOfColor32;
  backColoring, allowBackColoring: Boolean;
begin
  offset := NullPoint;

  //convert the left spline control points into a flattened path
  if splineType = stQuadratic then
    leftPath := FlattenQSpline(leftSpline) else
    leftPath := FlattenCSpline(leftSpline);
  rec := GetBounds(leftPath);
  //return false if the spline is invalid or there's no horizontal transformation
  Result := not IsEmptyRect(rec);
  if not Result then Exit;

  offset := rec.TopLeft;
  leftPath := OffsetPath(leftPath, -rec.Left, -rec.Top);
  //'Interpolate' the path so that there's a coordinate for every rounded
  //X and Y between the start and end of the flattened spline path. This
  //is to give a very rough approximatation of distance, so that the image
  //is roughly proportionally spaced even when the spline causes overlap.
  leftPath := InterpolatePath(leftPath);
  rightPath := OffsetPath(leftPath, img.Width, 0);
  Image32_Vector.OffsetRect(rec, -rec.Left, -rec.Top);
  rec := Rect(UnionRect(RectD(rec), GetBoundsD(rightPath)));
  w := RectWidth(rec); h := RectHeight(rec);
  len  := Length(leftPath);

  setLength(scaleX, len);
  for i := 0 to len -1 do
    if rightPath[i].X <= leftPath[i].X then
      scaleX[i] := 0 else
      scaleX[i] := img.Width / (rightPath[i].X - leftPath[i].X);

  dy := (img.Height / len) * 256;
  SetLength(tmp, w * h);

  if reverseFill then
  begin
    t := -1; u := len -1; v := -1;
  end else
  begin
    t := 1; u := 0; v := len;
  end;
  prevY := u - t;

  backColoring := false;
  allowBackColoring := (backColor shr 24) > 2;
  while u <> v do
  begin
    //dst y:
    y := Round(leftPath[u].Y);
    if y >= h then begin inc(u, t); Continue; end;

    //check if reversing fill direction - ie folding overlap
    if allowBackColoring then
    begin
      if (y <> prevY) then
      begin
        if reverseFill then
          backColoring := prevY < y else
          backColoring := prevY > y;
      end
      else if (Abs(u -v) > 1) then //ie it's safe to look ahead
      begin
        if reverseFill then
          backColoring := prevY < leftPath[u+t].Y else
          backColoring := prevY > leftPath[u+t].Y;
      end;
    end;

    prevY := y;
    pc := @tmp[y  * w];
    //src y:
    y := Round(u * dy);

    dx := leftPath[u].X;
    sx :=  scaleX[u];
    for j := 0 to w -1 do
    begin
      x := (j - dx) * sx;
      if backColoring then
        pc^ := BlendToAlpha(pc^,
          ReColor(GetWeightedPixel(img, Round(x * 256), y), backColor))
      else
        //blend in case spline causes folding overlap
        pc^ := BlendToAlpha(pc^, GetWeightedPixel(img, Round(x * 256), y));
      inc(pc);
    end;
    inc(u, t);
  end;
  img.SetSize(w, h);
  Move(tmp[0], img.PixelBase^, w * h * SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

end.
