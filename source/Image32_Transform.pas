unit Image32_Transform;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.37                                                            *
* Date      :  15 January 2020                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2020                                         *
* Purpose   :  Affine and projective transformation routines for TImage32      *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Windows, Math, Types,
  Image32, Image32_Draw, Image32_Vector;

type
  TMatrixD = array [0..2, 0..2] of double;

procedure AffineTransform(img: TImage32;
  matrix: TMatrixD); overload;
procedure AffineTransform(img: TImage32;
  matrix: TMatrixD; out offset: TPoint); overload;

function ProjectiveTransform(img: TImage32;
  const dstPts: TArrayOfPointD): Boolean; overload;
function ProjectiveTransform(img: TImage32;
  const dstPts: TArrayOfPointD; out offset: TPoint): Boolean; overload;

function SplineVertTransform(img: TImage32; const topSpline: TArrayOfPointD;
  splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
  out offset: TPoint): Boolean;
function SplineHorzTransform(img: TImage32; const leftSpline: TArrayOfPointD;
  splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
  out offset: TPoint): Boolean;

//Note: matrix multiplication is not symmetric
//function MultiplyMatrices(const current, new: TMatrixD): TMatrixD;

const
  IdentityMatrix: TMatrixD = ((1, 0, 0),(0, 1, 0),(0, 0, 1));

implementation

resourcestring
  rsInvalidMatrix = 'Invalid matrix.'; //nb: always start with IdentityMatrix

//------------------------------------------------------------------------------
// Affine Transformation
//------------------------------------------------------------------------------

function MultiplyMatrices(const current, new: TMatrixD): TMatrixD;
var
  i, j: Integer;
begin
  if (current[2][2] <> 1) or (new[2][2] <> 1) then
    Raise Exception.Create(rsInvalidMatrix);
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := (new[0, j] * current[i, 0]) +
        (new[1, j] * current[i, 1]) + (new[2, j] * current[i, 2]);
end;
//------------------------------------------------------------------------------

function Det4(a1, a2, b1, b2: double): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := a1 * b2 - a2 * b1;
end;
//------------------------------------------------------------------------------

function Det9(a1, a2, a3, b1, b2, b3, c1, c2, c3: double): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := a1 * (b2 * c3 - b3 * c2) -
            b1 * (a2 * c3 - a3 * c2) +
            c1 * (a2 * b3 - a3 * b2);
end;
//------------------------------------------------------------------------------

function Determinant(const matrix: TMatrixD): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Det9(matrix[0,0], matrix[1,0], matrix[2,0],
                 matrix[0,1], matrix[1,1], matrix[2,1],
                 matrix[0,2], matrix[1,2], matrix[2,2]);
end;
//------------------------------------------------------------------------------

procedure Adjoint(var matrix: TMatrixD);
var
  tmp: TMatrixD;
begin
  tmp := matrix;
  matrix[0,0] :=  Det4(tmp[1,1], tmp[1,2], tmp[2,1], tmp[2,2]);
  matrix[0,1] := -Det4(tmp[0,1], tmp[0,2], tmp[2,1], tmp[2,2]);
  matrix[0,2] :=  Det4(tmp[0,1], tmp[0,2], tmp[1,1], tmp[1,2]);

  matrix[1,0] := -Det4(tmp[1,0], tmp[1,2], tmp[2,0], tmp[2,2]);
  matrix[1,1] :=  Det4(tmp[0,0], tmp[0,2], tmp[2,0], tmp[2,2]);
  matrix[1,2] := -Det4(tmp[0,0], tmp[0,2], tmp[1,0], tmp[1,2]);

  matrix[2,0] :=  Det4(tmp[1,0], tmp[1,1], tmp[2,0], tmp[2,1]);
  matrix[2,1] := -Det4(tmp[0,0], tmp[0,1], tmp[2,0], tmp[2,1]);
  matrix[2,2] :=  Det4(tmp[0,0], tmp[0,1], tmp[1,0], tmp[1,1]);
end;
//------------------------------------------------------------------------------

procedure ScaleMatrix(var matrix: TMatrixD; val: double);
var
  i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      matrix[i, j] := matrix[i, j] * val;
end;
//------------------------------------------------------------------------------

procedure InvertMatrix(var matrix: TMatrixD);
var
  d: double;
const
  tolerance = 1.0E-5;
begin
  d := Determinant(matrix);
  if abs(d) > tolerance then
  begin
    Adjoint(matrix);
    ScaleMatrix(matrix, 1/d);
  end
  else matrix := IdentityMatrix;
end;
//------------------------------------------------------------------------------

procedure AffineTransformPt(var x, y: double; const matrix: TMatrixD);
var
  tmpX: double;
begin
  tmpX := x;
  x := tmpX * matrix[0, 0] + y * matrix[1, 0] + matrix[2, 0];
  y := tmpX * matrix[0, 1] + y * matrix[1, 1] + matrix[2, 1];
end;
//------------------------------------------------------------------------------

function GetTransformBounds(img: TImage32; const matrix: TMatrixD): TRect;
var
  i: integer;
  pts: array[0..3] of TPointD;
  rec: TRectD;
begin
  pts[0] := PointD(0,0);
  pts[1] := PointD(img.Width,0);
  pts[2] := PointD(img.Width,img.Height);
  pts[3] := PointD(0,img.Height);
  for i := 0 to 3 do
    AffineTransformPt(pts[i].X, pts[i].Y, matrix);
  rec.Left := pts[0].X; rec.Left := rec.Left;
  rec.Top := pts[0].Y; rec.Bottom := rec.Top;
  for i := 0 to 3 do
  begin
    if pts[i].X < rec.Left then rec.Left := pts[i].X
    else if pts[i].X > rec.Right then rec.Right := pts[i].X;
    if pts[i].Y < rec.Top then rec.Top := pts[i].Y
    else if pts[i].Y > rec.Bottom then rec.Bottom := pts[i].Y;
  end;
  Result := Rect(rec);
end;
//------------------------------------------------------------------------------

procedure AffineTransform(img: TImage32; matrix: TMatrixD);
var
  dummy: TPoint;
begin
  AffineTransform(img, matrix, dummy);
end;
//------------------------------------------------------------------------------

procedure AffineTransform(img: TImage32;
  matrix: TMatrixD; out offset: TPoint); overload;
var
  i,j, w,h, dx,dy: integer;
  x, y: double;
  pc: PColor32;
  tmp: TArrayOfColor32;
  rec: TRect;
begin
  if img.Width * img.Height = 0 then Exit;
  rec := GetTransformBounds(img, matrix);
  offset := rec.TopLeft;

  dx := rec.Left; dy := rec.Top;
  w := RectWidth(rec); h := RectHeight(rec);

  //starting with the result pixel coords, reverse find
  //the fractional coordinates in the current image
  InvertMatrix(matrix);

  SetLength(tmp, w * h);
  pc := @tmp[0];
  for i := 0 to h -1 do
    for j := 0 to w -1 do
    begin
      x := j + dx; y := i + dy;
      AffineTransformPt(x, y, matrix);
      pc^ := GetWeightedPixel(img, Round(x * 256), Round(y * 256));
      inc(pc);
    end;
  img.SetSize(w, h);
  Move(tmp[0], img.Pixels[0], w * h * sizeOf(TColor32));
end;

//------------------------------------------------------------------------------
// Projective Transformation
//------------------------------------------------------------------------------

// The code below is based on code from GR32_Transforms.pas in
// Graphics32 - https://sourceforge.net/projects/graphics32/
// Portions created by the initial developer, Alex A. Denisov, are protected
// by copyright (C) 2000-2009 under Mozilla Public License.
// MPL 1.1 or LGPL 2.1 with linking exception (http://www.mozilla.org/MPL/ )

procedure Scale(var M: TMatrixD; Factor: double);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      M[i,j] := M[i,j] * Factor;
end;
//------------------------------------------------------------------------------

procedure Invert(var M: TMatrixD);
var
  Det: double;
begin
  Det := Determinant(M);
  if Abs(Det) < 1E-5 then M := IdentityMatrix
  else
  begin
    Adjoint(M);
    Scale(M, 1 / Det);
  end;
end;
//------------------------------------------------------------------------------

function InvTransformPt(var x, y: double; invMatrix: TMatrixD): Boolean;
var
  xx, z: double;
begin
  z := invMatrix[0, 2] * x + invMatrix[1, 2] * y + invMatrix[2, 2];
  Result := z <> 0;
  if not Result then Exit;

  xx := x;
  x := invMatrix[0, 0] *xx + invMatrix[1, 0] *y + invMatrix[2, 0];
  y := invMatrix[0, 1] *xx + invMatrix[1, 1] *y + invMatrix[2, 1];

  if z = 1 then Exit;
  z := 1 / z; x := x * z; y := y * z;
  Result := (abs(x) < $7FFFFF) and (abs(y) < $7FFFFF); //avoids range error
end;
//------------------------------------------------------------------------------

function GetProjectiveTransformInvMatrix(const srcRect: TRect;
  dst: TArrayOfPointD): TMatrixD;
var
  dx1, dx2, px, dy1, dy2, py: double;
  g, h, k: double;
  R: TMatrixD;
begin
  px := dst[0].X - dst[1].X + dst[2].X - dst[3].X;
  py := dst[0].Y - dst[1].Y + dst[2].Y - dst[3].Y;

  //if (top.dx = bottom.dx) and (top.dy = bottom.dy) then affine transform
  if (px = 0) and (py = 0) then
  begin
    Result[0, 0] := dst[1].X - dst[0].X;
    Result[1, 0] := dst[2].X - dst[1].X;
    Result[2, 0] := dst[0].X;

    Result[0, 1] := dst[1].Y - dst[0].Y;
    Result[1, 1] := dst[2].Y - dst[1].Y;
    Result[2, 1] := dst[0].Y;

    Result[0, 2] := 0;
    Result[1, 2] := 0;
    Result[2, 2] := 1;
  end else
  begin
    dx1 := dst[1].X - dst[2].X;
    dx2 := dst[3].X - dst[2].X;
    dy1 := dst[1].Y - dst[2].Y;
    dy2 := dst[3].Y - dst[2].Y;
    k := dx1 * dy2 - dx2 * dy1;
    if k <> 0 then
    begin
      k := 1 / k;
      g := (px * dy2 - py * dx2) * k;
      h := (dx1 * py - dy1 * px) * k;

      Result[0, 0] := dst[1].X - dst[0].X + g * dst[1].X;
      Result[1, 0] := dst[3].X - dst[0].X + h * dst[3].X;
      Result[2, 0] := dst[0].X;

      Result[0, 1] := dst[1].Y - dst[0].Y + g * dst[1].Y;
      Result[1, 1] := dst[3].Y - dst[0].Y + h * dst[3].Y;
      Result[2, 1] := dst[0].Y;

      Result[0, 2] := g;
      Result[1, 2] := h;
      Result[2, 2] := 1;
    end else
    begin
      FillChar(Result, SizeOf(Result), 0);
    end;
  end;

  // denormalize texture space (u, v)
  R := IdentityMatrix;
  R[0, 0] := 1 / (SrcRect.Right - SrcRect.Left);
  R[1, 1] := 1 / (SrcRect.Bottom - SrcRect.Top);
  Result := MultiplyMatrices(R, Result);

  R := IdentityMatrix;
  R[2, 0] := -SrcRect.Left;
  R[2, 1] := -SrcRect.Top;
  Result := MultiplyMatrices(R, Result);

  Invert(Result);
end;
//------------------------------------------------------------------------------

function ProjectiveTransform(img: TImage32; const dstPts: TArrayOfPointD): Boolean;
var
  dummy: TPoint;
begin
  Result := ProjectiveTransform(img, dstPts, dummy);
end;
//------------------------------------------------------------------------------

function ProjectiveTransform(img: TImage32;
  const dstPts: TArrayOfPointD; out offset: TPoint): Boolean;
var
  w,h,i,j, dx,dy: integer;
  x,y: double;
  rec: TRect;
  invMatrix: TMatrixD;
  tmp: TArrayOfColor32;
  pc: PColor32;
begin
  result := false;
  if img.IsEmpty or (Length(dstPts) <> 4) or
    not IsPathConvex(dstPts) then Exit;

  invMatrix := GetProjectiveTransformInvMatrix(img.Bounds, dstPts);
  rec := GetBounds(dstPts);
  offset := rec.TopLeft;

  w := RectWidth(rec); h := RectHeight(rec);
  dx := rec.Left;
  dy := rec.Top;
  SetLength(tmp, w * h);
  pc := @tmp[0];
  for i := 0 to h -1 do
    for j := 0 to w -1 do
    begin
      x := j + dx; y := i + dy;
      if not InvTransformPt(x, y, invMatrix) then Exit;
      pc^ := GetWeightedPixel(img, Round(x * 256), Round(y * 256));
      inc(pc);
    end;
  Result := true;
  img.SetSize(w, h);
  Move(tmp[0], img.Pixels[0], w * h * sizeOf(TColor32));
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

function InterpolateSegment(const pt1, pt2: TPointD): TArrayOfPointD;
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

function InterpolatePath(const path: TArrayOfPointD): TArrayOfPointD;
var
  i,len,len2: integer;
  tmp: TArrayOfPointD;
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

function SplineVertTransform(img: TImage32; const topSpline: TArrayOfPointD;
  splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
  out offset: TPoint): Boolean;
var
  t,u,v, i,j, x,len, w,h: integer;
  prevX: integer;
  dx, dy, y, sy: double;
  topPath, botPath: TArrayOfPointD;
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
  Windows.OffsetRect(rec, -rec.Left, -rec.Top);
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

function SplineHorzTransform(img: TImage32; const leftSpline: TArrayOfPointD;
  splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
  out offset: TPoint): Boolean;
var
  t,u,v, i,j, y,prevY, len, w,h: integer;
  x, dx,dy,sx: double;
  leftPath, rightPath: TArrayOfPointD;
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
  Windows.OffsetRect(rec, -rec.Left, -rec.Top);
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
