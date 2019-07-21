unit Image32_Transform;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.06                                                            *
* Date      :  17 July 2019                                                    *
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

procedure ProjectiveTransform(img: TImage32;
  const srcCorners, dstCorners: TArrayOfPointD);


const
  IdentityMatrix: TMatrixD = ((1, 0, 0),(0, 1, 0),(0, 0, 1));

implementation

uses
  Image32_Vector;

resourcestring
  rsMatrixNotInverible = 'InvertMatrix Error: Matrix not invertible';
  rsInvalidMatrix = 'Invalid matrix. Tip - start with IdentityMatrix';

//------------------------------------------------------------------------------
// Affine Transformations
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
  tolerance = 1.0E-10;
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
// Projective Transformations
//------------------------------------------------------------------------------

type
  TMatRow8 = array of double;
  TMatrix8 = array of TMatRow8;

//------------------------------------------------------------------------------

function DotMMsmall(const x, y: TMatrix8): TMatrix8;
var
  i,j,k, p,q,r: integer;
  bar: TMatRow8;
  woo: double;
begin
  p := High(x); q := High(y);
  r := High(y[0]);
  SetLength(Result, p +1);
  for i := 0 to p do SetLength(Result[i], r +1);

  for i := p downto 0 do
  begin
    bar := x[i];
    k := r;
    while k >= 0 do
    begin
      woo := bar[q] * y[q][k];
      j := q -1;
      while j >= 1 do
      begin
        woo := woo + bar[j] * y[j][k] + bar[j-1] * y[j-1][k];
        dec(j, 2);
      end;
      if (j = 0) then woo := woo + bar[0] * y[0][k];
      Result[i][k] := woo;
      dec(k);
    end;
  end;
end;
//------------------------------------------------------------------------------

function DotVV(const x, y: TMatRow8): double;
{$IFDEF INLINE} inline; {$ENDIF}
var
  i, n: integer;
begin
  n := High(x);
  Result := x[n] * y[n];
  i := n -1;
  while i >= 1 do
  begin
     Result := Result + x[i] * y[i] + x[i-1] * y[i-1];
     dec(i,2);
  end;
  if (i = 0) then
    Result := Result + x[0] * y[0];
end;
//------------------------------------------------------------------------------

function DotMV(const matrix: TMatrix8; const vec: TMatRow8): TMatRow8;
{$IFDEF INLINE} inline; {$ENDIF}
var
  i: integer;
  row: TMatRow8;
begin
  SetLength(Result, Length(matrix));
  for i := 0 to high(matrix) do
  begin
    row := matrix[i];
    Result[i] := dotVV(row, vec);
  end;
end;
//------------------------------------------------------------------------------

function MakeIdentity(size: integer): TMatrix8;
var
  i: integer;
begin
  setLength(Result, size);
  for i := 0 to size -1 do
  begin
    SetLength(Result[i], size);
    Result[i][i] := 1;
  end;
end;
//------------------------------------------------------------------------------

function InvMatrix(const mat: TMatrix8): TMatrix8;
var
  i, j, k, m,n, I0: integer;
  A: TMatrix8;
  Ai, Aj, Ii, Ij: TMatRow8;
  kx, x, v0: double;
begin
  m := High(mat);
  n := High(mat[0]);
  Result := MakeIdentity(m+1);
  SetLength(A, m +1);
  for i := 0 to m do
  begin
    SetLength(A[i], n+1);
    Move(mat[i][0], A[i][0], (n+1) * SizeOf(double));
  end;

  for j := 0 to n do
  begin
    i0 := -1; v0 := -1;
    for i := j to m do
    begin
      kx := abs(A[i][j]);
      if kx > v0 then
      begin
        i0 := i;
        v0 := kx;
      end;
    end;
    if (i0 = -1) then
      Raise Exception.Create(rsMatrixNotInverible);

    Aj := A[i0];
    A[i0] := A[j];
    A[j] := Aj;
    Ij := Result[i0];
    Result[i0] := Result[j];
    Result[j] := Ij;
    x := Aj[j];

    for k := j to n do Aj[k] := Aj[k] / x;
    for k := n downto 0 do Ij[k] := Ij[k] / x;

    for i := m downto 0 do
    begin
      if (i = j) then Continue;
      Ai := A[i];
      Ii := Result[i];
      x := Ai[j];
      for k := j+1 to n do Ai[k] := Ai[k] - Aj[k] * x;
      k := n;
      while k > 0 do
      begin
        Ii[k] := Ii[k] - Ij[k] * x;
        dec(k);
        Ii[k] := Ii[k] - Ij[k] * x;
        dec(k);
      end;
      if (k = 0) then Ii[0] := Ii[0] - Ij[0] * x;
    end;
  end;
end;
//------------------------------------------------------------------------------

function Round10DP(num: double): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  result := (num * 10000000000)/10000000000;
end;
//------------------------------------------------------------------------------

function TransposeMatrix(const matrix: TMatrix8): TMatrix8;
var
  i,j, highRow, highCol: integer;
  A0, A1, Bj: TMatRow8;
begin
  highRow := High(matrix[0]);
  highCol := High(matrix);
  SetLength(Result, highCol +1);
  for i := 0 to highCol do
    SetLength(Result[i], highRow +1);

  i := highCol;
  while i >= 1 do
  begin
    A1 := matrix[i];
    A0 := matrix[i - 1];
    for j := highRow downto 0 do
    begin
      Bj := Result[j];
      Bj[i] := A1[j];
      Bj[i-1] := A0[j];
    end;
    dec(i, 2);
  end;
  if i = 0 then
  begin
    A0 := matrix[0];
    for J := highRow downto 0 do
      Result[j][0] := A0[j];
  end;
end;
//------------------------------------------------------------------------------

function GetProjectiveMatrix(const srcCorners,
  dstCorners: TArrayOfPointD): TMatrixD;
var
  i,j: integer;
  matA, matC, matD: TMatrix8;
  matB, matX: TMatRow8;
  srcPts, dstPts: array[0..7] of Double;
  r1,r2,r3,r4,r5,r6,r7,r8: TMatRow8;
  ptr: PDouble;
begin
  Result := IdentityMatrix;
  if (length(srcCorners) <> 4) or (length(dstCorners) <> 4) then Exit;

  //Original code from https://github.com/jlouthan/perspective-transform
  //Copyright (c) 2015 Jenny Louthan (MIT License)

  srcPts[0] := srcCorners[0].X;
  srcPts[1] := srcCorners[0].Y;
  srcPts[2] := srcCorners[1].X;
  srcPts[3] := srcCorners[1].Y;
  srcPts[4] := srcCorners[2].X;
  srcPts[5] := srcCorners[2].Y;
  srcPts[6] := srcCorners[3].X;
  srcPts[7] := srcCorners[3].Y;

  dstPts[0] := dstCorners[0].X;
  dstPts[1] := dstCorners[0].Y;
  dstPts[2] := dstCorners[1].X;
  dstPts[3] := dstCorners[1].Y;
  dstPts[4] := dstCorners[2].X;
  dstPts[5] := dstCorners[2].Y;
  dstPts[6] := dstCorners[3].X;
  dstPts[7] := dstCorners[3].Y;

  r1 := [srcPts[0], srcPts[1], 1, 0, 0, 0,
    -1*dstPts[0]*srcPts[0], -1*dstPts[0]*srcPts[1]];
	r2 := [0, 0, 0, srcPts[0], srcPts[1], 1,
    -1*dstPts[1]*srcPts[0], -1*dstPts[1]*srcPts[1]];
	r3 := [srcPts[2], srcPts[3], 1, 0, 0, 0,
    -1*dstPts[2]*srcPts[2], -1*dstPts[2]*srcPts[3]];
	r4 := [0, 0, 0, srcPts[2], srcPts[3], 1,
    -1*dstPts[3]*srcPts[2], -1*dstPts[3]*srcPts[3]];
	r5 := [srcPts[4], srcPts[5], 1, 0, 0, 0,
    -1*dstPts[4]*srcPts[4], -1*dstPts[4]*srcPts[5]];
	r6 := [0, 0, 0, srcPts[4], srcPts[5], 1,
    -1*dstPts[5]*srcPts[4], -1*dstPts[5]*srcPts[5]];
	r7 := [srcPts[6], srcPts[7], 1, 0, 0, 0,
    -1*dstPts[6]*srcPts[6], -1*dstPts[6]*srcPts[7]];
	r8 := [0, 0, 0, srcPts[6], srcPts[7], 1,
    -1*dstPts[7]*srcPts[6], -1*dstPts[7]*srcPts[7]];

  SetLength(matB, 8);
  for i := 0 to 7 do matB[i] := dstPts[i];

  SetLength(matA, 8);
  SetLength(matC, 8);
  matA := [r1, r2, r3, r4, r5, r6, r7, r8];
  matC := TransposeMatrix(matA);
  matC := dotMMsmall(matC, matA);
  try
    matC := InvMatrix(matC);
  except
    //error!!!
  end;
  matD := DotMMsmall(matC, TransposeMatrix(matA));
  matX := DotMV(matD, matB);

  ptr := @matX[0];
  for i := 0 to 2 do
    for j := 0 to 2 do
    begin
      Result[i][j] := Round10DP(ptr^);
      inc(ptr);
    end;
  Result[2][2] := 1;
end;
//------------------------------------------------------------------------------

function ProjectiveTransformPt(x,y: double; const mat: TMatrixD): TPointD;
begin
  Result.X := (mat[0][0]*x + mat[0][1]*y + mat[0][2]) /
    (mat[2][0]*x + mat[2][1]*y + 1);
  Result.Y := (mat[1][0]*x + mat[1][1]*y + mat[1][2]) /
    (mat[2][0]*x + mat[2][1]*y + 1);
end;
//------------------------------------------------------------------------------

procedure ProjectiveTransform(img: TImage32;
  const srcCorners, dstCorners: TArrayOfPointD);
var
  i,j, w,h, dx, dy: integer;
  pt: TPointD;
  matrix: TMatrixD;
  rec: TRect;
  tmp: TArrayOfColor32;
  pc: PColor32;
begin
  if img.Width * img.Height = 0 then Exit;
  rec := GetBounds(dstCorners);
  matrix := GetProjectiveMatrix(dstCorners, srcCorners);
  w := RectWidth(rec); h := RectHeight(rec);
  dx := rec.Left;
  dy := rec.Top;
  SetLength(tmp, w * h);
  pc := @tmp[0];
  for i := 0 to h -1 do
    for j := 0 to w -1 do
    begin
      pt := ProjectiveTransformPt(j + dx, i + dy, matrix);
      pc^ := GetWeightedPixel(img, Round(pt.X * 256), Round(pt.Y * 256));
      inc(pc);
    end;
  img.SetSize(w, h);
  Move(tmp[0], img.Pixels[0], w * h * sizeOf(TColor32));
end;
//------------------------------------------------------------------------------

end.
