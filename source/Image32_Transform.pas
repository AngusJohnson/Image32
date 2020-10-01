unit Image32_Transform;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.52                                                            *
* Date      :  1 October 2020                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  Affine and projective transformation routines for TImage32      *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Math, Types, Image32, Image32_Draw, Image32_Vector;

procedure AffineTransformImage(img: TImage32; matrix: TMatrixD); overload;
procedure AffineTransformImage(img: TImage32;
  matrix: TMatrixD; out offset: TPoint); overload;

function ProjectiveTransform(img: TImage32;
  const dstPts: TPathD): Boolean; overload;
function ProjectiveTransform(img: TImage32;
  const dstPts: TPathD; out offset: TPoint): Boolean; overload;

function SplineVertTransform(img: TImage32; const topSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
  out offset: TPoint): Boolean;
function SplineHorzTransform(img: TImage32; const leftSpline: TPathD;
  splineType: TSplineType; backColor: TColor32; reverseFill: Boolean;
  out offset: TPoint): Boolean;

implementation

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
  i,j, w,h, dx,dy: integer;
  pt: TPointD;
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
  MatrixInvert(matrix);

  SetLength(tmp, w * h);
  pc := @tmp[0];
  for i := 0 to h -1 do
    for j := 0 to w -1 do
    begin
      pt.X := j + dx; pt.Y := i + dy;
      MatrixApply(matrix, pt);
      pc^ := GetWeightedPixel(img, Round(pt.X * 256), Round(pt.Y * 256));
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

function InvTransform(var x, y: double; invMatrix: TMatrixD): Boolean;
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
  dst: TPathD): TMatrixD;
var
  dx1, dx2, px, dy1, dy2, py: double;
  g, h, k: double;
  m: TMatrixD;
begin
  px := dst[0].X - dst[1].X + dst[2].X - dst[3].X;
  py := dst[0].Y - dst[1].Y + dst[2].Y - dst[3].Y;

  if (px = 0) and (py = 0) then
  begin
    //almost certainly rectangular but in theory could be rotated & skewed too
    Result[0, 0] := (dst[1].X - dst[0].X)/(SrcRect.Right - SrcRect.Left);
    Result[0, 1] := dst[1].Y - dst[0].Y;
    Result[0, 2] := 0;
    Result[1, 0] := dst[2].X - dst[1].X;
    Result[1, 1] := (dst[2].Y - dst[1].Y)/(SrcRect.Bottom - SrcRect.Top);
    Result[1, 2] := 0;
    Result[2, 0] := dst[0].X - SrcRect.Left;
    Result[2, 1] := dst[0].Y - SrcRect.Top;
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

      m[0, 0] := dst[1].X - dst[0].X + g * dst[1].X;
      m[1, 0] := dst[3].X - dst[0].X + h * dst[3].X;
      m[2, 0] := dst[0].X;

      m[0, 1] := dst[1].Y - dst[0].Y + g * dst[1].Y;
      m[1, 1] := dst[3].Y - dst[0].Y + h * dst[3].Y;
      m[2, 1] := dst[0].Y;

      m[0, 2] := g;
      m[1, 2] := h;
      m[2, 2] := 1;

      Result := IdentityMatrix;
      Result[0, 0] := 1 / (SrcRect.Right - SrcRect.Left);
      Result[1, 1] := 1 / (SrcRect.Bottom - SrcRect.Top);
      Result[0, 2] := -SrcRect.Left;
      Result[1, 2] := -SrcRect.Top;

      //de-scale and translate before the projective transform!?
      Result := MatrixMultiply(m, Result);

    end else
      Result := IdentityMatrix;
  end;
  MatrixInvert(Result);
end;
//------------------------------------------------------------------------------

function ProjectiveTransform(img: TImage32; const dstPts: TPathD): Boolean;
var
  dummy: TPoint;
begin
  Result := ProjectiveTransform(img, dstPts, dummy);
end;
//------------------------------------------------------------------------------

function ProjectiveTransform(img: TImage32;
  const dstPts: TPathD; out offset: TPoint): Boolean;
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
      if not InvTransform(x, y, invMatrix) then Exit;
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
