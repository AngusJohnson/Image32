unit Image32_CQ;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.17                                                            *
* Date      :  11 August 2019                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Color reduction for TImage32                                    *
*           :  Uses Octree Color Quantization & Floyd / Steinberg Dithering    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses Windows, SysUtils, Classes, Math, Image32;

procedure ReduceColors(image: TImage32;
  MaxColors: integer; UseDithering: Boolean = true);
function CreatePalette(image: TImage32; MaxColors: integer): TArrayOfColor32;
//nb: dithering is recommended for pretty much all palette reduction
procedure ApplyPalette(image: TImage32; const palette: array of TColor32;
  UseDithering: Boolean = true);

function CreateLogPalette(const palColors: TArrayOfColor32): TMaxLogPalette;

//DrawPalette: Useful for debugging
//procedure DrawPalette(image: TImage32; const palette: array of TColor32);

const

  MonoPal2: array [0..1] of TColor32 = (
    $FF000000, $FFFFFFFF);

  macPal16: array [0 .. 15] of TColor32 = (
    $FFFFFFFF, $FF05F3FC, $FF0264FF, $FF0608DD,
    $FF8408F2, $FFA50046, $FFD40000, $FFEAAB02,
    $FF14B71F, $FF126400, $FF052C56, $FF3A7190,
    $FFC0C0C0, $FF808080, $FF404040, $FF000000);

  winPal16: array [0..15] of TColor32 = (
    $FF000000, $FF800000, $FF008000, $FF000080,
    $FF808000, $FF008080, $FF800080, $FF808080,
    $FFC0C0C0, $FFFF0000, $FF00FF00, $FF0000FF,
    $FFFFFF00, $FFFF00FF, $FF00FFFF, $FFFFFFFF);

implementation

type

  //Octree Color Quantization:
  //https://web.archive.org/web/20140605161956/ -->
  // <-- http://www.microsoft.com/msj/archive/S3F1.aspx

  TOctNode = class;
  TOctNodes8 = array[0 .. 7] of TOctNode;

  TOctNode = class
    protected
      IsLeaf     : Boolean;
      Level      : integer;
      Count      : integer;
      Next       : TOctNode;
      Childs     : TOctNodes8;
      TotalR     : integer;
      TotalG     : integer;
      TotalB     : integer;
      procedure  Add(color: TColor32);
      procedure  Get(var color: TColor32);
      procedure  GetNearest(var color: TColor32);
    public
      constructor Create(aLevel: byte);
      destructor Destroy; override;
  end;

  TOctree = class
    protected
      Leaves     : integer;
      MaxColors  : integer;
      Top        : TOctNode;
      Reducible8 : TOctNodes8;
      procedure  Reduce;
      procedure  Delete(var node: TOctNode);
    public
      constructor Create(aMaxColors: integer);
      destructor  Destroy; override;
      procedure  Add(color: TColor32);
      procedure  GetNearest(var color: TColor32);
      function   GetPalette: TArrayOfColor32;
  end;

  PARGBArray = ^TARGBArray;
  TARGBArray = array [0 .. $FFFFFF -1] of TARGB;

const
  NullOctNodes8 : TOctNodes8 = (nil, nil, nil, nil, nil, nil, nil, nil);

//------------------------------------------------------------------------------
// Miscellaneous Octree functions
//------------------------------------------------------------------------------

function GetIndex(color: TColor32; level: byte): byte;
{$IFDEF INLINE} inline; {$ENDIF}
const
  mask: array[0..7] of Byte= ($80, $40, $20, $10, $8, $4, $2, $1);
var
  argb: TARGB absolute color;
  shift: integer;
begin
  shift := 7 - level;
  Result:=
    ((argb.R   and mask[level]) shr (shift - 2)) or
    ((argb.G and mask[level]) shr (shift - 1)) or
    ((argb.B  and mask[level]) shr shift);
end;
//------------------------------------------------------------------------------

function GetDistance(color1,color2: TColor32): integer;
{$IFDEF INLINE} inline; {$ENDIF}
var
  argb1: TARGB absolute color1;
  argb2: TARGB absolute color2;
begin
  result := abs(argb2.R - argb1.R) +
    abs(argb2.G - argb1.G) + abs(argb2.B - argb1.B);
end;

//------------------------------------------------------------------------------
// TOctNode methods
//------------------------------------------------------------------------------

constructor TOctNode.Create(aLevel: byte);
begin
  Level   := aLevel;
  IsLeaf  := Level = 8;
  Next    := nil;
  Childs  := NullOctNodes8;
  TotalR  := 0;
  TotalG  := 0;
  TotalB  := 0;
  Count   := 0;
end;
//------------------------------------------------------------------------------

destructor TOctNode.Destroy;
var
  i: integer;
begin
  for i:= 0 to 7 do Childs[i].Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TOctNode.Add(color: TColor32);
var
  argb: TARGB absolute color;
begin
  Inc (TotalR, argb.R);
  Inc (TotalG, argb.G);
  Inc (TotalB, argb.B);
  Inc (Count);
end;
//------------------------------------------------------------------------------

procedure TOctNode.GetNearest(var color: TColor32);
var
  i,j,k,distJ,distK: integer;
  colorJ, colorK: TColor32;
begin
  if IsLeaf then
  begin
    Get(color);
    Exit;
  end;
  i := GetIndex(color, level);
  if assigned(Childs[i]) then
  begin
    Childs[i].GetNearest(color);
    Exit;
  end;
  //when 'color' isn't found, pick the child that's closest
  j := 0;
  while not assigned(Childs[j]) do inc(j);
  colorJ := color;
  Childs[j].GetNearest(colorJ);
  distJ := GetDistance(color, colorJ);
  for k := j +1 to 7 do
    if assigned(Childs[k]) then
    begin
      colorK := color;
      Childs[k].GetNearest(colorK);
      distK := GetDistance(color, colorK);
      if distK >= distJ then continue;
      distJ := distK;
      colorJ := colorK;
    end;
  color := colorJ;
end;
//------------------------------------------------------------------------------

procedure TOctNode.Get(var color: TColor32);
var
  argb: TARGB absolute color;
begin
  if Count > 0 then
  begin
    argb.R := TotalR div Count;
    argb.G := TotalG div Count;
    argb.B := TotalB div Count;
  end;
end;

//------------------------------------------------------------------------------
// TOctree methods
//------------------------------------------------------------------------------

constructor TOctree.Create(aMaxColors: integer);
begin
  MaxColors := Max(8, aMaxColors); //nb: fails when maxColors < 8
  Leaves := 0;
  Top := TOctNode.Create(0);
  Reducible8 := NullOctNodes8;
end;
//------------------------------------------------------------------------------

destructor TOctree.Destroy;
begin
  Delete(Top);
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TOctree.Reduce;
var
  i, childCnt: integer;
  node: TOctNode;
begin
  //find the lowest level with a reducible node ...
  i := 7;
  while (i > 0) and not Assigned(Reducible8[i]) do Dec(i);

  //reduce the most recently added node at level 'i' ...
  node          := Reducible8[i];
  Reducible8[i] := node.Next;
  node.IsLeaf   := True;
  node.TotalR   := 0; node.TotalG := 0; node.TotalB := 0;
  node.Count    := 0;
  childCnt      := 0;

  //now merge the leaves into the parent node ...
  for i:= 0 to 7 do
    if Assigned (node.Childs[i]) then
    begin
      Inc (node.TotalR, node.Childs[i].TotalR);
      Inc (node.TotalG, node.Childs[i].TotalG);
      Inc (node.TotalB, node.Childs[i].TotalB);
      Inc (node.Count, node.Childs[i].Count);
      node.Childs[i].Free;
      node.Childs[i]:= nil;
      inc(childCnt);
    end;
  Dec(Leaves, childCnt -1);
end;
//------------------------------------------------------------------------------

procedure TOctree.Add(color: TColor32);
var
  argb: TARGB absolute color;

 procedure AddColor(var node: TOctNode; level: byte);
 begin
   if not Assigned(node) then
   begin
     node:= TOctNode.Create(level +1);
     if node.IsLeaf then
     begin
       Inc(Leaves);
     end else
     begin
       node.Next  := Reducible8[node.level];
       Reducible8[node.level] := node;
     end;
   end;

   if node.IsLeaf then
     node.Add(color) else
     AddColor(node.Childs[GetIndex(color, node.level)], node.level);
 end;

begin
  color := color and $FFFFFF;
  AddColor(Top, 0);
  while (Leaves > MaxColors) do
    Reduce;
end;
//------------------------------------------------------------------------------

procedure TOctree.GetNearest(var color: TColor32);
begin
  Top.GetNearest(color);
end;
//------------------------------------------------------------------------------

procedure TOctree.Delete(var node: TOctNode);
var
  i: integer;
begin
  for i := Low (node.Childs) to High (node.Childs) do
    if Assigned(node.Childs[i]) then
      Delete(node.Childs[i]);
  FreeAndNil(node);
end;
//------------------------------------------------------------------------------

function TOctree.GetPalette: TArrayOfColor32;
var
  count: integer;

  procedure FillPalette(Node: TOctNode);
  var
    i: integer;
  begin
    if Node.IsLeaf then
    begin
      Node.Get(Result[Count]);
      Inc(Count);
    end else
    begin
      for i := 0 to 7 do
        if assigned(Node.Childs[i]) then
          FillPalette(Node.Childs[i]);
    end;
  end;

begin
  SetLength(result, Leaves);
  count := 0;
  FillPalette(Top);
end;

//------------------------------------------------------------------------------
// Floyd / Steinberg Dithering -
// see https://en.wikipedia.org/wiki/Floyd%E2%80%93Steinberg_dithering
//------------------------------------------------------------------------------

type
  TMulDibTable = array [-255 .. 255] of integer;

var
  Mul1Div16Table: TMulDibTable;
  Mul3Div16Table: TMulDibTable;
  Mul5Div16Table: TMulDibTable;
  Mul7Div16Table: TMulDibTable;

procedure Monochrome(var color: TColor32);
var
  c: TARGB absolute color;
begin
  c.R := (c.R * 61 + c.G * 174 + c.B * 21) shr 8;
  if c.R > 127 then c.R := 255 else c.R := 0;
  c.G := c.R; c.B := c.R; c.A := 255;
end;
//------------------------------------------------------------------------------

function ClampByte(val: integer): Byte;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  if val < 0 then Result:= 0
  else if val >= 255 then Result:= 255
  else Result:= val;
end;
//------------------------------------------------------------------------------

function GetPColor32(pc: PARGB; offset: integer): PARGB;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  result := PARGB(PByte(pc) + (offset * SizeOf(TColor32)));
end;
//------------------------------------------------------------------------------

procedure Dither(image: TImage32; octree: TOctree);
var
  X, Y, W         : Integer;
  qeR,qeG, qeB    : integer;
  qeRq,qeGq, qeBq : integer;
  oldC            : TARGB;
  newC            : PARGB;
  tmp             : TImage32;

  procedure AdjustPixel(pc: PARGB; r, g, b: integer);
  begin
    pc.R := ClampByte(pc.R + r);
    pc.G := ClampByte(pc.G + g);
    pc.B := ClampByte(pc.B + b);
    dec(qeRq, r); dec(qeGq, g); dec(qeBq, b);
  end;

begin
  tmp := TImage32.Create(image);
  try
    W := image.Width;
    newC := PARGB(image.PixelBase);
    for Y := 0 to image.Height-1 do
      for X := 0 to W -1 do
      begin
        if newC.A > 0 then
        begin
          oldC := newC^;
          //get the reduced color
          if assigned(octree) then
            octree.GetNearest(newC.Color) else
            Monochrome(newC.Color);

          qeR := oldC.R - newC.R;
          qeG := oldC.G - newC.G;
          qeB := oldC.B - newC.B;
          qeRq := qeR; qeGq := qeG; qeBq := qeB;

          if X < image.Width-1 then
            AdjustPixel(GetPColor32(newC, +1),
              Mul7Div16Table[qeR], Mul7Div16Table[qeG], Mul7Div16Table[qeB]);

          if Y < image.Height -1 then
          begin
            if X > 0 then
              AdjustPixel(GetPColor32(newC, W-1),
                Mul3Div16Table[qeR], Mul3Div16Table[qeG], Mul3Div16Table[qeB]);

            AdjustPixel(GetPColor32(newC, W),
              Mul5Div16Table[qeR], Mul5Div16Table[qeG], Mul5Div16Table[qeB]);

            if X < W -1 then
              AdjustPixel(GetPColor32(newC, W +1),
                Mul1Div16Table[qeRq], Mul1Div16Table[qeGq], Mul1Div16Table[qeBq]);
          end;
        end;
        inc(newC);
      end;
  finally
    tmp.Free;
  end;
end;

//------------------------------------------------------------------------------
// CreatePalette...
//------------------------------------------------------------------------------

function CreatePalette(image: TImage32; MaxColors: integer): TArrayOfColor32;
var
  i: integer;
  pc: PARGB;
  octree: TOctree;
begin

  if MaxColors < 8 then
  begin
    setLength(Result, 2);
    result[0] := clWhite32;
    result[1] := clBlack32;
    Exit;
  end;

  MaxColors := Min(256, MaxColors);
  result := nil;
  octree := TOctree.Create(MaxColors);
  try
    pc := PARGB(image.PixelBase);
    for i := 0 to image.Width * image.Height - 1 do
    begin
      octree.Add(pc.Color);
      inc(pc);
    end;
    result :=  octree.GetPalette;
  finally
    octree.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure ApplyPalette(image: TImage32; const palette: array of TColor32;
  UseDithering: Boolean = true);
var
  i, len: integer;
  pc: PARGB;
  octree: TOctree;
begin
  len := Length(palette);

  if len < 4 then
  begin
    Dither(image, nil);
    Exit;
  end;

  octree := TOctree.Create(len);
  try
    for i := 0 to len - 1 do
      octree.Add(palette[i]);

    if UseDithering then
    begin
      Dither(image, octree);
    end else
    begin
      pc := PARGB(image.PixelBase);
      for i := 0 to image.Width * image.Height - 1 do
      begin
        octree.GetNearest(pc.Color);
        inc(pc);
      end;
    end;

  finally
    octree.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure ReduceColors(image: TImage32;
  MaxColors: integer; UseDithering: Boolean = true);
var
  pal: TArrayOfColor32;
begin
  pal := CreatePalette(image, MaxColors);
  ApplyPalette(image, pal, UseDithering);
end;
//------------------------------------------------------------------------------

function CreateLogPalette(const palColors: TArrayOfColor32): TMaxLogPalette;
var
  i, len: integer;
begin
  len := Length(palColors);
  Result.palVersion := $300;
  Result.palNumEntries := len;
  for i := 0 to len -1 do
  begin
    Result.palPalEntry[0].peRed := TARGB(palColors[i]).R;
    Result.palPalEntry[0].peGreen := TARGB(palColors[i]).G;
    Result.palPalEntry[0].peBlue := TARGB(palColors[i]).B;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawPalette(image: TImage32; const palette: array of TColor32);
var
  i, w, h, len: integer;
  rec: TRect;
begin
  len := length(palette);
  if len < 16 then w := len else w := 16;
  h := (len +15) div 16;
  image.SetSize(w * 16, h * 16);
  rec := Rect(0,0,16,16);
  for i := 0 to len -1 do
  begin
    image.FillRect(rec, palette[i] or $FF000000);
    if (i + 1) mod w = 0 then
      OffsetRect(rec, -15 * w, 16) else
      OffsetRect(rec, 16, 0);
  end;
end;

//------------------------------------------------------------------------------
// Initialization functions
//------------------------------------------------------------------------------

procedure MakeDitherTables;
const
  OneDiv16   = 0.0625;
  ThreeDiv16 = 0.1875;
  FiveDiv16  = 0.3125;
  SevenDiv16 = 0.4375;
var
  i: Integer;
begin
  for i := -255 to 255 do
  begin
    Mul1Div16Table[i] := Round(i * OneDiv16);
    Mul3Div16Table[i] := Round(i * ThreeDiv16);
    Mul5Div16Table[i] := Round(i * FiveDiv16);
    Mul7Div16Table[i] := Round(i * SevenDiv16);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeDitherTables;

end.

