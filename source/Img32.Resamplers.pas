unit Img32.Resamplers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.4                                                             *
* Date      :  24 April 2024                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2024                                         *
* Purpose   :  For image transformations (scaling, rotating etc.)              *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Img32;

// BoxDownSampling: As the name implies, is only intended for image
// down-sampling (ie shrinking images) where it performs a little better
// than other resamplers which tend toward pixelation. Nevertheless, this
// routine is inferior to other resamplers when performing other
// types of transformations (ie when enlarging, rotating, and skewing images),
// so BoxDownSampling should not be used as a general purpose resampler.
procedure BoxDownSampling(Image: TImage32; scale: double); overload;
procedure BoxDownSampling(Image: TImage32; scaleX, scaleY: double); overload;
procedure BoxDownSampling(Image: TImage32; newWidth, newHeight: Integer); overload;

// The following general purpose resamplers are registered below:
// function NearestResampler(img: TImage32; x, y: double): TColor32;
// function BilinearResample(img: TImage32; x, y: double): TColor32;
// function BicubicResample (img: TImage32; x, y: double): TColor32;
// function WeightedBilinear(img: TImage32; x, y: double): TColor32;

implementation

uses
  Img32.Transform;

var
  sinWeighted: array [0..255] of Cardinal;

//------------------------------------------------------------------------------
// NearestNeighbor resampler
//------------------------------------------------------------------------------

function NearestResampler(img: TImage32; x, y: double): TColor32;
var
  xi, yi: integer;
begin
  xi := Round(x); yi := Round(y);
  if (xi < 0) or (yi < 0) or (xi >= img.Width) or (yi >= img.Height) then
    Result := clNone32 else
    Result := img.Pixels[xi + yi * img.Width];
end;

//------------------------------------------------------------------------------
// BiLinear resampler
//------------------------------------------------------------------------------

function BilinearResample(img: TImage32; x, y: double): TColor32;
var
  iw, ih: integer;
  xx, yy, xR, yB: integer;
  weight: Cardinal;
  pixels: TArrayOfColor32;
  weightedColor: TWeightedColor;
  xf, yf: double;
begin
  iw := img.Width;
  ih := img.Height;
  pixels := img.Pixels;

  // avoid antialiasing when x or y is very close to zero
  if (x < 0) and (x >= -0.5) then x := 0;
  if (y < 0) and (y >= -0.5) then y := 0;

  if x < 0 then
    xf := -frac(x) else
    xf := 1-frac(x);
  if y < 0 then
    yf := -frac(y) else
    yf := 1-frac(y);

  xx := Floor(x);
  yy := Floor(y);
  xR := xx +1;
  yB := yy +1;

  if xx >= iw -1 then
  begin
    xx := iw -1;
    xR := xx;
  end;
  if yy >= ih -1 then
  begin
    yy := ih -1;
    yB := yy;
  end;

  weightedColor.Reset;

  weight := Round(xf * yf * 255);      //top-left
  if weight > 0 then
  begin
    if (x < 0) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yy * iw], weight);
  end;

  weight := Round((1-xf) * yf * 255);         //top-right
  if weight > 0 then
  begin
    if (x > iw - 0.5) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yy * iw], weight);
  end;

  weight := Round(xf * (1-yf) * 255);          //bottom-left
  if weight > 0 then
  begin
    if (x < 0) or (y > ih - 0.5) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yB * iw], weight);
  end;

  weight := Round((1-xf) * (1-yf) * 255);              //bottom-right
  if weight > 0 then
  begin
    if (x > iw - 0.5) or (y > ih - 0.5) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yB * iw], weight);
  end;
  Result := weightedColor.Color;
end;
//------------------------------------------------------------------------------

// WeightedBilinearResample: A modified bilinear resampler that's
// less blurry but also a little more pixelated.
function WeightedBilinearResample(img: TImage32; x, y: double): TColor32;
var
  iw, ih: integer;
  xx, yy, xR, yB: integer;
  weight: Cardinal;
  pixels: TArrayOfColor32;
  weightedColor: TWeightedColor;
  xf, yf: double;
begin
  iw := img.Width;
  ih := img.Height;
  pixels := img.Pixels;

  // avoid antialiasing when x or y is very close to zero
  if (x < 0) and (x >= -0.5) then x := 0;
  if (y < 0) and (y >= -0.5) then y := 0;

  if x < 0 then
    xf := -frac(x) else
    xf := 1-frac(x);
  if y < 0 then
    yf := -frac(y) else
    yf := 1-frac(y);

  xx := Floor(x);
  yy := Floor(y);
  xR := xx +1;
  yB := yy +1;

  if xx >= iw -1 then
  begin
    xx := iw -1;
    xR := xx;
  end;
  if yy >= ih -1 then
  begin
    yy := ih -1;
    yB := yy;
  end;

  weightedColor.Reset;

  weight := sinWeighted[Round(xf * yf * 255)];      //top-left
  if weight > 0 then
  begin
    if (x < 0) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yy * iw], weight);
  end;

  weight := sinWeighted[Round((1-xf) * yf * 255)];        //top-right
  if weight > 0 then
  begin
    if (x > iw - 0.5) or (y < 0) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yy * iw], weight);
  end;

  weight := sinWeighted[Round(xf * (1-yf) * 255)];          //bottom-left
  if weight > 0 then
  begin
    if (x < 0) or (y > ih - 0.5) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xx + yB * iw], weight);
  end;

  weight := sinWeighted[Round((1-xf) * (1-yf) * 255)];              //bottom-right
  if weight > 0 then
  begin
    if (x > iw - 0.5) or (y > ih - 0.5) then
      weightedColor.AddWeight(weight) else
      weightedColor.Add(pixels[xR + yB * iw], weight);
  end;
  Result := weightedColor.Color;
end;

//------------------------------------------------------------------------------
// BiCubic resampler
//------------------------------------------------------------------------------

type
  TBiCubicEdgeAdjust = (eaNone, eaPreStart, eaStart, eaStartPlus, eaEnd, eaPostEnd);

var
  byteFrac: array [0..255] of double;
  byteFracSq: array [0..255] of double;
  byteFracCubed: array [0..255] of double;

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
    eaPreStart:
      begin
        a := @clTrans;
        b := @clTrans;
        c := PARGB(aclr);
        d := c;
      end;
    eaStart:
      begin
        Result := aclr^;
        Exit;
      end;
    eaStartPlus:
      begin
        a := PARGB(aclr);
        b := a;
        Inc(aclr);
        c := PARGB(aclr);
        d := c;
      end;
    eaEnd:
      begin
        Inc(aclr);
        Result := aclr^;
        Exit;
      end;
    eaPostEnd:
      begin
        Inc(aclr);
        a := PARGB(aclr);
        b := a;
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
  else if (b = c) then
  begin
    result := b.Color;
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

	aa := Integer(-a.A + 3*b.A - 3*c.A + d.A) div 2;
	bb := Integer(2*a.A - 5*b.A + 4*c.A - d.A) div 2;
	cc := Integer(-a.A + c.A) div 2;
  Res.A := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.A);

	aa := Integer(-a.R + 3*b.R - 3*c.R + d.R) div 2;
	bb := Integer(2*a.R - 5*b.R + 4*c.R - d.R) div 2;
	cc := Integer(-a.R + c.R) div 2;
  Res.R := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.R);

	aa := Integer(-a.G + 3*b.G - 3*c.G + d.G) div 2;
	bb := Integer(2*a.G - 5*b.G + 4*c.G - d.G) div 2;
	cc := Integer(-a.G + c.G) div 2;
  Res.G := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.G);

	aa := Integer(-a.B + 3*b.B - 3*c.B + d.B) div 2;
	bb := Integer(2*a.B - 5*b.B + 4*c.B - d.B) div 2;
	cc := Integer(-a.B + c.B) div 2;
  Res.B := ClampByte(aa*t3 + bb*t2 + cc*t1 + b.B);
end;
//------------------------------------------------------------------------------

function BicubicResample(img: TImage32; x, y: double): TColor32;
var
  i, pi, iw, ih, last: Integer;
  c: array[0..3] of TColor32;
  xFrac, yFrac: byte;
  bceX, bceY: TBiCubicEdgeAdjust;
begin
  iw := img.Width;
  ih := img.Height;
  last := iw * ih -1;

  if x < 0 then
    xFrac := Round(frac(1+x) *255) else
    xFrac := Round(frac(x) *255);
  if y < 0 then
    yFrac := Round(frac(1+y) *255) else
    yFrac := Round(frac(y) *255);

  if x < -0.5 then bceX := eaPreStart
  else if (x < 1) then
  begin
    if (x < 0.5) and ((iw >= 6) or (x < 0)) then bceX := eaStart
    else bceX := eaStartPlus;
  end
  else if x > iw - 0.5 then bceX := eaPostEnd
  else if x > iw - 1 then bceX := eaEnd
  else bceX := eaNone;

  if y < -0.5 then bceY := eaPreStart
  else if (y < 1) then
  begin
    if (y < 0.5) and ((ih >= 6) or (y < 0)) then bceY := eaStart
    else bceY := eaStartPlus;
  end
  else if y > ih - 0.5 then bceY := eaPostEnd
  else if y > ih - 1 then bceY := eaEnd
  else bceY := eaNone;

  x := Max(0, Min(iw -1, x -1));
  y := Max(0, Min(ih -1, y -1));
  pi := Trunc(y) * iw + Trunc(x);

  for i := 0 to 3 do
  begin
    c[i] := CubicHermite(@img.Pixels[pi], xFrac, bceX);
    inc(pi, iw);
    if pi > last then break;
  end;
  Result := CubicHermite(@c[0], yFrac, bceY);
end;

//------------------------------------------------------------------------------
// BoxDownSampling and related functions
//------------------------------------------------------------------------------

function GetWeightedColor(const srcBits: TArrayOfColor32;
  x256, y256, xx256, yy256, maxX: Integer): TColor32;
var
  i, j, xi, yi, xxi, yyi, weight: Integer;
  xf, yf, xxf, yyf: cardinal;
  color: TWeightedColor;
begin
  //This function performs 'box sampling' and differs from GetWeightedPixel
  //(bilinear resampling) in one important aspect - it accommodates weighting
  //any number of pixels (rather than just adjacent pixels) and this produces
  //better image quality when significantly downsizing.

  //Note: there's no range checking here, so the precondition is that the
  //supplied boundary values are within the bounds of the srcBits array.

  color.Reset;

  xi := x256 shr 8; xf := x256 and $FF;
  yi := y256 shr 8; yf := y256 and $FF;
  xxi := xx256 shr 8; xxf := xx256 and $FF;
  yyi := yy256 shr 8; yyf := yy256 and $FF;

  //1. average the corners ...
  weight := (($100 - xf) * ($100 - yf)) shr 8;
  color.Add(srcBits[xi + yi * maxX], weight);
  weight := (xxf * ($100 - yf)) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yi * maxX], weight);
  weight := (($100 - xf) * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xi + yyi * maxX], weight);
  weight := (xxf * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yyi * maxX], weight);

  //2. average the edges
  if (yi +1 < yyi) then
  begin
    xf := $100 - xf;
    for i := yi + 1 to yyi - 1 do
      color.Add(srcBits[xi + i * maxX], xf);
    if (xxf <> 0) then
      for i := yi + 1 to yyi - 1 do
        color.Add(srcBits[xxi + i * maxX], xxf);
  end;
  if (xi + 1 < xxi) then
  begin
    yf := $100 - yf;
    for i := xi + 1 to xxi - 1 do
      color.Add(srcBits[i + yi * maxX], yf);
    if (yyf <> 0) then
      for i := xi + 1 to xxi - 1 do
        color.Add(srcBits[i + yyi * maxX], yyf);
  end;

  //3. average the non-fractional pixel 'internals' ...
  for i := xi + 1 to xxi - 1 do
    for j := yi + 1 to yyi - 1 do
      color.Add(srcBits[i + j * maxX], $100);

  //4. finally get the weighted color ...
  if color.AddCount = 0 then
    Result := srcBits[xi + yi * maxX] else
    Result := color.Color;
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image: TImage32; scaleX, scaleY: double);
begin
  BoxDownSampling(Image,
    Max(1, Integer(Round(Image.Width * scaleX))),
    Max(1, Integer(Round(Image.Height * scaleY))));
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image: TImage32; scale: double);
begin
  BoxDownSampling(Image,
    Max(1, Integer(Round(Image.Width * scale))),
    Max(1, Integer(Round(Image.Height * scale))));
end;
//------------------------------------------------------------------------------

procedure BoxDownSampling(Image: TImage32; newWidth, newHeight: Integer);
var
  x,y, x256,y256,xx256,yy256: Integer;
  sx,sy: double;
  tmp: TArrayOfColor32;
  pc: PColor32;
  scaledX: array of Integer;
begin
  sx := Image.Width/newWidth * 256;
  sy := Image.Height/newHeight * 256;
  SetLength(tmp, newWidth * newHeight);

  SetLength(scaledX, newWidth +1); //+1 for fractional overrun
  for x := 0 to newWidth -1 do
    scaledX[x] := Round((x+1) * sx);

  y256 := 0;
  pc := @tmp[0];
  for y := 0 to newHeight - 1 do
  begin
    x256 := 0;
    yy256 := Round((y+1) * sy);
    for x := 0 to newWidth - 1 do
    begin
      xx256 := scaledX[x];
      pc^ := GetWeightedColor(Image.Pixels,
        x256, y256, xx256, yy256, Image.Width);
      x256 := xx256;
      inc(pc);
    end;
    y256 := yy256;
  end;

  Image.BeginUpdate;
  Image.SetSize(newWidth, newHeight);
  Move(tmp[0], Image.Pixels[0], newWidth * newHeight * SizeOf(TColor32));
  Image.EndUpdate;
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
  piDiv256   : double = Pi / 256;
begin
  for i := 0 to 255 do
  begin
    byteFrac[i]  := i     *inv255;
    byteFracSq[i]  := i*i   *inv255sqrd;
    byteFracCubed[i] := i*i*i *inv255cubed;

    sinWeighted[i] := Round((Sin(i * piDiv256 - Pi/2) +1) /2 * 255);
  end;
end;
//------------------------------------------------------------------------------

initialization
  InitByteExponents;

  rNearestResampler  := RegisterResampler(NearestResampler, 'NearestNeighbor');
  rBilinearResampler := RegisterResampler(BilinearResample, 'Bilinear');
  rBicubicResampler  := RegisterResampler(BicubicResample, 'HermiteBicubic');
  rWeightedBilinear  := RegisterResampler(WeightedBilinearResample, 'WeightedBilinear');
  DefaultResampler   := rBilinearResampler;

end.
