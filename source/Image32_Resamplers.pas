unit Image32_Resamplers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.21                                                            *
* Date      :  29 March 2021                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  For image transformations (scaling, rotating etc.)              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Math, Types, Image32, Image32_Vector;

implementation


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
  xi, yi, weight: Integer;
  width, height: integer;
  pixels: TArrayOfColor32;
  color: TWeightedColor;
  xf, yf: cardinal;
begin
  width := img.Width; height := img.Height;
  pixels := img.Pixels;
  //coordinate integers (can be negative) -> x256 div 256 & y256 div 256.
  //coordinate fractions ->  (x256 and $FF) / 256 & (y256 and $FF) / 256
  if (x256 < -$FF) or (y256 < -$FF) or
    (x256 >= width * $100) or (y256 >= height * $100) then
  begin
    result := clNone32;
    Exit;
  end;
  xi := abs(x256) shr 8;
  xf := x256 and $FF;
  yi := abs(y256) shr 8;
  yf := y256 and $FF;

  color.Reset;
  weight := (($100 - xf) * ($100 - yf)) shr 8;         //top-left
  if (x256 < 0) or (y256 < 0) then
    color.AddWeight(weight) else
    color.Add(pixels[xi + yi * width], weight);

  weight := (xf * ($100 - yf)) shr 8;                  //top-right
  if (xi + 1 >= width) or (y256 < 0) then
    color.AddWeight(weight) else
    color.Add(pixels[xi + 1 + yi * width], weight);

  weight := (($100 - xf) * yf) shr 8;                  //bottom-left
  if (x256 < 0) or (yi + 1 = height) then
    color.AddWeight(weight) else
    color.Add(pixels[xi + (yi +1) * width], weight);

  weight := (xf * yf) shr 8;                           //bottom-right
  if (xi + 1 >= width) or (yi + 1 = height) then
    color.AddWeight(weight) else
    color.Add(pixels[(xi + 1)  + (yi + 1) * width], weight);
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
	aa, bb, cc: double;
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

	aa := -a.A*0.5 + 1.5*b.A - 1.5*c.A + d.A*0.5;
	bb := a.A - 2.5*b.A + 2*c.A - d.A*0.5;
	cc := -a.A*0.5 + c.A*0.5;
  Res.A := ClampByte(aa*byteFracCubed[t] + bb*byteFracSq[t] + cc*byteFrac[t] + b.A);

	aa := -a.R*0.5 + 1.5*b.R - 1.5*c.R + d.R*0.5;
	bb := a.R - 2.5*b.R + 2*c.R - d.R*0.5;
	cc := -a.R*0.5 + c.R*0.5;
  Res.R := ClampByte(aa*byteFracCubed[t] + bb*byteFracSq[t] + cc*byteFrac[t] + b.R);

	aa := -a.G*0.5 + 1.5*b.G - 1.5*c.G + d.G*0.5;
	bb := a.G - 2.5*b.G + 2*c.G - d.G*0.5;
	cc := -a.G*0.5 + c.G*0.5;
  Res.G := ClampByte(aa*byteFracCubed[t] + bb*byteFracSq[t] + cc*byteFrac[t] + b.G);

	aa := -a.B*0.5 + 1.5*b.B - 1.5*c.B + d.B*0.5;
	bb := a.B - 2.5*b.B + 2*c.B - d.B*0.5;
	cc := -a.B*0.5 + c.B*0.5;
  Res.B := ClampByte(aa*byteFracCubed[t] + bb*byteFracSq[t] + cc*byteFrac[t] + b.B);
end;
//------------------------------------------------------------------------------

function BicubicResample(img: TImage32; x256, y256: Integer): TColor32;
var
  i, dx,dy, pi, iw, w,h: Integer;
  c: array[0..3] of TColor32;
  x: Integer;
  y: Integer;
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

  rNearestResampler  := RegisterResampler(NearestResampler);
  rBilinearResampler := RegisterResampler(BilinearResample);
  rBicubicResampler  := RegisterResampler(BicubicResample);
  DefaultResampler   := rBilinearResampler;

end.
