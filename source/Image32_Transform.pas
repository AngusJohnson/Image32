unit Image32_Transform;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.10                                                            *
* Date      :  23 July 2019                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Affine and projective transformation routines for TImage32      *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Windows, Math, Image32, Image32_Draw;

type
  TMatrixD = array [0..2, 0..2] of double;

procedure AffineTransform(img: TImage32; matrix: TMatrixD);

//Note: matrix multiplication is not symmetric
function MultiplyMatrices(const current, new: TMatrixD): TMatrixD;

function ProjectiveTransform(img: TImage32;
  const dstPts: TArrayOfPointD): Boolean;

const
  IdentityMatrix: TMatrixD = ((1, 0, 0),(0, 1, 0),(0, 0, 1));

implementation

uses
  Image32_Vector;

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
  i,j, w,h, dx,dy: integer;
  x, y: double;
  pc: PColor32;
  tmp: TArrayOfColor32;
  rec: TRect;
begin
  if img.Width * img.Height = 0 then Exit;
  rec := GetTransformBounds(img, matrix);
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

function ProjectiveTransform(img: TImage32;
  const dstPts: TArrayOfPointD): Boolean;
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

end.
