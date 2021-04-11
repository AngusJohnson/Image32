unit Image32_Resamplers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.22                                                            *
* Date      :  2 April 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  For image transformations (scaling, rotating etc.)              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types;

implementation

uses
  Image32, Image32_Vector, Image32_Transform;

//------------------------------------------------------------------------------
// NearestNeighbor resampler
//------------------------------------------------------------------------------

function NearestResampler(img: TImage32; x256, y256: Integer): TColor32;
begin
  if (x256 < -$7f) then
  begin
    Result := clNone32;
    Exit;
  end;

  if (y256 < -$7f) then
  begin
    Result := clNone32;
    Exit;
  end;

  if (x256 and $FF > $7F) then inc(x256, $100);
  x256 := x256 shr 8;
  if y256 and $FF > $7F then inc(y256, $100);
  y256 := y256 shr 8;

  if (x256 < 0) or (x256 >= img.Width) or
    (y256 < 0) or (y256 >= img.Height) then
      Result := clNone32 else
      Result := img.Pixels[y256 * img.Width + x256];
end;

//------------------------------------------------------------------------------
// BiLinear resampler
//------------------------------------------------------------------------------

function BilinearResample(img: TImage32; x256, y256: Integer): TColor32;
var
  xi,yi, weight: Integer;
  iw, ih: integer;
  pixels: TArrayOfColor32;
  color: TWeightedColor;
  xf, yf: cardinal;
begin
  iw := img.Width;
  ih := img.Height;
  pixels := img.Pixels;

  if (x256 <= -$100) or (x256 >= iw *$100) or
     (y256 <= -$100) or (y256 >= ih *$100) then
  begin
    result := clNone32;
    Exit;
  end;

  if x256 < 0 then xi := -1
  else xi := x256 shr 8;

  if y256 < 0 then yi := -1
  else yi := y256 shr 8;

  xf := x256 and $FF;
  yf := y256 and $FF;

  color.Reset;

  weight := (($100 - xf) * ($100 - yf)) shr 8;        //top-left
  if (xi < 0) or (yi < 0) then
    color.AddWeight(weight) else
    color.Add(pixels[xi + yi * iw], weight);

  weight := (xf * ($100 - yf)) shr 8;                 //top-right
  if ((xi+1) >= iw) or (yi < 0) then
    color.AddWeight(weight) else
    color.Add(pixels[(xi+1) + yi * iw], weight);

  weight := (($100 - xf) * yf) shr 8;                 //bottom-left
  if (xi < 0) or ((yi+1) >= ih) then
    color.AddWeight(weight) else
    color.Add(pixels[(xi) + (yi+1) * iw], weight);

  weight := (xf * yf) shr 8;                          //bottom-right
  if (xi + 1 >= iw) or (yi + 1 >= ih) then
    color.AddWeight(weight) else
    color.Add(pixels[(xi+1) + (yi+1) * iw], weight);

  Result := color.Color;
end;

//------------------------------------------------------------------------------
// BiCubic resampler
//------------------------------------------------------------------------------

type
  TBiCubicEdgeAdjust = (eaNone, eaOne, eaTwo, eaThree, eaFour);

var
  byteFrac: array [0..255] of double;
  byteFracSq: array [0..255] of double;
  byteFracCubed: array [0..255] of double;

//------------------------------------------------------------------------------

function ClampByte(val: double): byte;
begin
  if val <= 0 then result := 0
  else if val >= 255 then result := 255
  else result := Round(val);
end;
//------------------------------------------------------------------------------

function CubicHermite(aclr: PColor32; t: Byte; bce: TBiCubicEdgeAdjust): TColor32;
var
  a,b,c,d: PARGB;
  q: TARGB;
	aa, bb, cc: integer;
  t1, t2, t3: double;
  res: TARGB absolute Result;
const
  clTrans: TColor32 = clNone32;
begin
  case bce of
    eaOne:
      begin
        a := @clTrans;
        b := @clTrans;
        c := PARGB(aclr);
        Inc(aclr);
        d := PARGB(aclr);
      end;
    eaTwo:
      begin
        a := PARGB(aclr);
        b := a;
        Inc(aclr);
        c := PARGB(aclr);
        Inc(aclr);
        d := PARGB(aclr);
      end;
    eaThree:
      begin
        a := PARGB(aclr);
        Inc(aclr);
        b := PARGB(aclr);
        Inc(aclr);
        c := PARGB(aclr);
        d := c;
      end;
    eaFour:
      begin
        a := PARGB(aclr);
        Inc(aclr);
        b := PARGB(aclr);
        c := @clTrans;
        d := @clTrans;
      end;
    else
      begin
        a := PARGB(aclr);
        Inc(aclr);
        b := PARGB(aclr);
        Inc(aclr);
        c := PARGB(aclr);
        Inc(aclr);
        d := PARGB(aclr);
      end;
  end;

  if (b.A = 0) and (c.A = 0) then
  begin
    result := clNone32;
    Exit;
  end
  else if b.A = 0 then
  begin
    q := c^;
    q.A := 0;
    b := @q;
  end;
  if c.A = 0 then
  begin
    q := b^;
    q.A := 0;
    c := @q;
  end;

  t1 := byteFrac[t];
  t2 := byteFracSq[t];
  t3 := byteFracCubed[t];

	aa := (-a.A + 3*b.A - 3*c.A + d.A) div 2;
	bb := (2*a.A - 5*b.A + 4*c.A - d.A) div 2;
	cc := (-a.A + c.A) div 2;
  Res.A := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.A);

	aa := (-a.R + 3*b.R - 3*c.R + d.R) div 2;
	bb := (2*a.R - 5*b.R + 4*c.R - d.R) div 2;
	cc := (-a.R + c.R) div 2;
  Res.R := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.R);

	aa := (-a.G + 3*b.G - 3*c.G + d.G) div 2;
	bb := (2*a.G - 5*b.G + 4*c.G - d.G) div 2;
	cc := (-a.G + c.G) div 2;
  Res.G := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.G);

	aa := (-a.B + 3*b.B - 3*c.B + d.B) div 2;
	bb := (2*a.B - 5*b.B + 4*c.B - d.B) div 2;
	cc := (-a.B + c.B) div 2;
  Res.B := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.B);
end;
//------------------------------------------------------------------------------

function BicubicResample(img: TImage32; x256, y256: Integer): TColor32;
var
  i, dx,dy, pi, iw, w,h: Integer;
  c: array[0..3] of TColor32;
  x, y: Integer;
  bceX, bceY: TBiCubicEdgeAdjust;
begin
  Result := clNone32;

  iw := img.Width;
  w := iw -1;
  h := img.Height -1;

  x := Abs(x256) shr 8;
  y := Abs(y256) shr 8;

  if (x256 < -$FF) or (x > w) or  (y256 < -$FF) or (y > h) then Exit;

  if (x256 < 0) then bceX := eaOne
  else if (x = 0) then bceX := eaTwo
  else if (x256 > w shl 8) then bceX := eaFour
  else if (x256 > (w -1) shl 8) then bceX := eaThree
  else bceX := eaNone;

  if (bceX = eaOne) or (bceX = eaTwo) then dx := 1
  else dx := 0;

  if (y256 < 0) then bceY := eaOne
  else if y = 0 then bceY := eaTwo
  else if y = h -1 then bceY := eaThree
  else if y = h then bceY := eaFour
  else bceY := eaNone;

  if (bceY = eaOne) or (bceY = eaTwo) then dy := 1
  else dy := 0;

  pi := (y -1 +dy) * iw + (x -1 + dx);

  if bceY = eaFour then dx := 2
  else if bceY = eaThree then dx := 1
  else dx := 0;

  for i := dy to 3 -dx do
  begin
    c[i] := CubicHermite(@img.Pixels[pi], x256 and $FF, bceX);
    inc(pi, iw);
  end;
  Result := CubicHermite(@c[dy], y256 and $FF, bceY);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure InitByteExponents;
var
  i: integer;
const
  inv255     : double = 1/255;
  inv255sqrd : double = 1/(255*255);
  inv255cubed: double = 1/(255*255*255);
begin
  for i := 0 to 255 do
  begin
    byteFrac[i]  := i     *inv255;
    byteFracSq[i]  := i*i   *inv255sqrd;
    byteFracCubed[i] := i*i*i *inv255cubed;
  end;
end;
//------------------------------------------------------------------------------

initialization
  InitByteExponents;

  rNearestResampler  := RegisterResampler(NearestResampler, 'NearestNeighbor');
  rBilinearResampler := RegisterResampler(BilinearResample, 'Bilinear');
  rBicubicResampler  := RegisterResampler(BicubicResample, 'HermiteBicubic');
  DefaultResampler   := rBilinearResampler;

end.

