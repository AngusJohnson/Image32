unit Img32.CQ;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.4                                                             *
* Date      :  20 April 2023                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  Color reduction for TImage32                                    *
*           :  Uses Octree Color Quantization & Floyd / Steinberg Dithering    *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, Types, Math, Img32, Img32.Vector;

type

  TArrayOfArrayOfColor32  = array of TArrayOfColor32;
  TArrayOfArrayOfInteger  = array of TArrayOfInteger;
  TArrayOfPointer         = array of Pointer;

  // TPointerSortFunc: p2 > p1 for an ascending sort
  TPointerSortFunc        = function (p1, p2: Pointer): Boolean;

  //Octree Color Quantization:
  //https://web.archive.org/web/20140605161956/ -->
  // <-- http://www.microsoft.com/msj/archive/S3F1.aspx

  TOctree = class;
  TOctNode = class;
  TOctNodes8 = array[0 .. 7] of TOctNode;

  TOctNode = class
    protected
      Level      : integer;
      Count      : integer;
      Next       : TOctNode;
      Childs     : TOctNodes8;
      TotalR     : Int64;
      TotalG     : Int64;
      TotalB     : Int64;
      procedure  AddOne(color: TColor32);
      procedure  AddCount(color: TColor32; count: integer);
      function   Find(color: TColor32): TOctNode;
      function   FindFirstLeaf: TOctNode;
      function   FindLastLeaf: TOctNode;
      function   FindLeafNearestToIndex(index: integer): TOctNode;
      function   DeleteColor(color: TColor32; tree: TOctree): integer;
      procedure  Get(out color: TColor32; out freq: integer);
      function   GetNearest(var color: TColor32): Boolean;
      //procedure  GetAny(var color: TColor32);
      function GetIsLeaf: Boolean;
      property IsLeaf : Boolean read GetIsLeaf;
    public
      constructor Create(aLevel: byte);
      destructor Destroy; override;
  end;

  TOctree = class
    protected
      Leaves     : integer;
      Top        : TOctNode;
      TotalCount : integer;
      Reducible8 : TOctNodes8;
      procedure   Reset;
      procedure   BuildTree(image: TImage32; maxColors: integer = 256);
      procedure   Delete(var node: TOctNode);
      function    Reduce: Boolean;
      procedure   GetNearest(var color: TColor32);
      procedure   GetTreePalette(out colors: TArrayOfColor32;
        out freq: TArrayOfInteger);
    public
      constructor Create;
      destructor  Destroy; override;
      procedure   Add(color: TColor32);
      function    DeleteColor(color: TColor32): integer;
      procedure   GetPalette(palSize: integer;
        out colors: TArrayOfColor32; out freq: TArrayOfInteger);
      procedure   ApplyPalette(image: TImage32);
      property ColorCount: integer read Leaves;
      // PixelCount: note semi-transparent pixels are ignored
      property PixelCount: integer read TotalCount;
  end;

function MakePalette(image: TImage32;
  MaxColors: integer): TArrayOfColor32; overload;
function MakePalette(image: TImage32; MaxColors: integer;
  out frequencies: TArrayOfInteger): TArrayOfColor32; overload;

procedure ApplyPalette(image: TImage32;
  const palette: TArrayOfColor32; UseDithering: Boolean = true);

//MakeAndApplyPalette: This is *much* faster than calling MakePalette and
//ApplyPalette separately as it uses the internally constructed Octree
//structure to apply palette colors to the image.
function MakeAndApplyPalette(image: TImage32;
  MaxColors: integer; UseDithering: Boolean): TArrayOfColor32; overload;
function MakeAndApplyPalette(image: TImage32;
  MaxColors: integer; UseDithering: Boolean;
  out frequencies: TArrayOfInteger): TArrayOfColor32; overload;

function CreatePaletteOctree(image: TImage32): TOctree;

//GetPalettes: gets (very efficiently) multiple palettes for an image
//and returns the total number of opaque pixels
function GetPalettes(image: TImage32;
  const sizesNeeded: array of integer;
  out palettes: TArrayOfArrayOfColor32;
  out frequencies: TArrayOfArrayOfInteger): integer;

{$IFDEF MSWINDOWS}
function CreateLogPalette(const palColors: TArrayOfColor32): TMaxLogPalette;
{$ENDIF}

//GetNearestPaletteColor: This function is relatively slow so be
//careful how you use it :).
function GetNearestPaletteColor(color: TColor32;
  const palette: TArrayOfColor32): TColor32;

//GetColorDistance: returns Euclidean distance squared
function GetColorDistance(color1, color2: TColor32): integer;
{$IFDEF INLINE} inline; {$ENDIF}

//DrawPalette: Useful for debugging
procedure DrawPalette(image: TImage32; const palette: TArrayOfColor32);
//SavePalette: Also useful for debugging
procedure SavePalette(const filename: string; const palette: TArrayOfColor32);

//https://en.wikipedia.org/wiki/List_of_software_palettes
function BlackWhitePal: TArrayOfColor32;
function DefaultMacPal16: TArrayOfColor32;
function DefaultWinPal16: TArrayOfColor32;

var
  OpacityThreshold: byte = $80;

implementation

resourcestring
  rsTrimPalette   = 'TrimPalette: Invalid frequency length';
  rsTrimPalette2  = 'TrimPalette: Invalid value for ''newSize''.';
  rsGetPalette    = 'TOctree.GetPalette error';

type
  PARGBArray = ^TARGBArray;
  TARGBArray = array [0 .. $FFFFFF -1] of TARGB;

  //redefine TByteArray (see also TByteArray in System.SysUtils)
  TByteArray = array[0..MaxInt -1] of Byte;
  PByteArray = ^TByteArray;

type
  PColFreqRec = ^TColFreqRec;
  TColFreqRec = record
    color   : TColor32;
    freq    : integer;
  end;
  TArrayOfColFreq = array of PColFreqRec;

const
  //LeafLevel: with a generally maximum 256 color palette,
  //it's very inefficient to use a LeafLevel > 4.
  LeafLevel = 4;

  NullOctNodes8 : TOctNodes8 =
    (nil, nil, nil, nil, nil, nil, nil, nil);

  MonoPal2: array [0..1] of TColor32 = (
    $FF000000, $FFFFFFFF);

  macPal16: array [0 .. 15] of TColor32 = (
    $FF000000, $FF404040, $FF808080, $FFC0C0C0,
    $FF3A7190, $FF052C56, $FF126400, $FF14B71F,
    $FFEAAB02, $FFD40000, $FFA50046, $FF8408F2,
    $FF0608DD, $FF0264FF, $FF05F3FC, $FFFFFFFF);

  winPal16_int: array [0..15] of TColor32 = (
    $FF000000, $FF800000, $FF008000, $FF000080,
    $FF808000, $FF008080, $FF800080, $FF808080,
    $FFC0C0C0, $FFFF0000, $FF00FF00, $FF0000FF,
    $FFFFFF00, $FFFF00FF, $FF00FFFF, $FFFFFFFF);

//------------------------------------------------------------------------------
// Miscellaneous Octree functions
//------------------------------------------------------------------------------

//GetIndex: gets the nearest color index for a given level in OctTree
function GetIndex(color: TColor32; level: byte): byte;
{$IFDEF INLINE} inline; {$ENDIF}
var
  argb: TARGB absolute color;
  mask: Byte;
begin
  mask := $80 shr level;
  result := 0;
  if (argb.R and mask) <> 0 then result := 4;
  if (argb.G and mask) <> 0 then result := result or 2;
  if (argb.B and mask) <> 0 then result := result or 1;
end;
//------------------------------------------------------------------------------

function GetColorDistance(color1, color2: TColor32): integer;
var
  c1: TARGB absolute color1;
  c2: TARGB absolute color2;
  //avgR: double;
begin
  // see https://en.wikipedia.org/wiki/Color_difference
  // Option 1:
  //result := Sqr(c2.R - c1.R) + Sqr(c2.G - c1.G) + Sqr(c2.B - c1.B);
  // Option 2: (a little better than Option 1)
  if c2.R + c1.R < 256 then
    result := 2*Sqr(c2.R - c1.R) + 4*Sqr(c2.G - c1.G) + 3*Sqr(c2.B - c1.B) else
    result := 3*Sqr(c2.R - c1.R) + 4*Sqr(c2.G - c1.G) + 2*Sqr(c2.B - c1.B);
end;
//------------------------------------------------------------------------------

function GetNearestPaletteColorIndex(color: TColor32;
  const palette: TArrayOfColor32): integer;
var
  i, highI, distI, distJ: integer;
begin
  highI := High(palette);
  if (highI < 0) or (TARGB(color).A < OpacityThreshold) then
  begin
    Result := -1;
    Exit;
  end;
  Result := 0;
  distJ := GetColorDistance(color, palette[0]);
  if distJ = 0 then Exit;
  for i := 1 to highI do
  begin
    distI := GetColorDistance(color, palette[i]);
    if distI >= distJ then Continue;
    Result := i;
    if distI = 0 then Exit;
    distJ := distI;
  end;
end;
//------------------------------------------------------------------------------

function GetNearestPaletteColor(color: TColor32;
  const palette: TArrayOfColor32): TColor32;
var
  i: integer;
begin
  i := GetNearestPaletteColorIndex(color, palette);
  if i < 0 then Result := clNone32 else
  Result := palette[i];
end;
//------------------------------------------------------------------------------

procedure QuickSortIntArray(var intArray: array of Integer; l, r: Integer);
var
  i,j, P, T: integer;
begin
  repeat
    i := l;
    j := r;
    P := intArray[(l + r) shr 1];
    repeat
      while intArray[i] < P do Inc(i);
      while intArray[j] > P do Dec(j);
      if i <= j then
      begin
        T := intArray[i];
        intArray[i] := intArray[j];
        intArray[j] := T;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if l < j then QuickSortIntArray(intArray, l, j);
    l := i;
  until i >= r;
end;
//------------------------------------------------------------------------------

function FindLeafNode(color: TColor32; node: TOctNode): TOctNode;
var
  i: integer;
  c: TColor32;
begin
  if node.IsLeaf then
  begin
    node.Get(c, i);
    if i = 0 then Result := nil
    else Result := node;
  end else
  begin
    i := GetIndex(color, node.level);
    if Assigned(node.Childs[i]) then
      Result := FindLeafNode(color, node.Childs[i]) else
      Result := nil;
  end;
end;

//------------------------------------------------------------------------------
// TOctNode methods
//------------------------------------------------------------------------------

constructor TOctNode.Create(aLevel: byte);
begin
  Level   := aLevel;
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

function TOctNode.GetIsLeaf: Boolean;
begin
  result := (Count > 0) or (Level = LeafLevel);
end;
//------------------------------------------------------------------------------

procedure TOctNode.AddOne(color: TColor32);
var
  argb: TARGB absolute color;
begin
  Inc (TotalR, argb.R);
  Inc (TotalG, argb.G);
  Inc (TotalB, argb.B);
  Inc (Self.Count);
end;
//------------------------------------------------------------------------------

procedure TOctNode.AddCount(color: TColor32; count: integer);
var
  argb: TARGB absolute color;
begin
  Inc (TotalR, argb.R * count);
  Inc (TotalG, argb.G * count);
  Inc (TotalB, argb.B * count);
  Inc (Self.Count, count);
end;
//------------------------------------------------------------------------------

function TOctNode.DeleteColor(color: TColor32; tree: TOctree): integer;
var
  i,lvl: Integer;
  node: TOctNode;
begin
  i := GetIndex(color, level);
  if not Assigned(Childs[i]) then
    Result := 0
  else if Childs[i].Count > 0 then
  begin
    Result := Childs[i].Count;
    FreeAndNil(Childs[i]);
    Dec(tree.Leaves);
    node := FindLeafNearestToIndex(i);
    if Assigned(node) then
    begin
      // node.Add(color) would sligthly change node's color
      // so to avoid that we could get node's color first
      //node.Get(color, i);
      node.AddCount(color, Result);
      Result := 0;
    end;
  end else
  begin
    Result := Childs[i].DeleteColor(color, tree);
    if Result > 0 then // Childs[i] must be empty
    begin
      // 1. Remove Childs[i] from tree.Reducible8 linked list
      lvl := Childs[i].Level;
      node := tree.Reducible8[lvl];
      if node = Childs[i] then
        tree.Reducible8[lvl] := Childs[i].Next
      else
      begin
        while node.Next <> Childs[i] do
          node := node.Next;
        node.Next := Childs[i].Next;
      end;
      FreeAndNil(Childs[i]);

      // 2. If possible, reassign deleted color to its nearest neighbor.
      // Also, finding a sibling indicates that the node isn't now empty.
      node := FindLeafNearestToIndex(i);
      if Assigned(node) then
      begin
        //node.Add(color, Result);
        Dec(tree.TotalCount, Result);
        Result := 0; // flag not to discard
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TOctNode.Find(color: TColor32): TOctNode;
var
  i: Integer;
begin
  i := GetIndex(color, level);
  if not Assigned(Childs[i]) then
    Result := nil
  else if Childs[i].Count > 0 then
    Result := Childs[i]
  else
    Result := Childs[i].Find(color);
end;
//------------------------------------------------------------------------------

function TOctNode.FindFirstLeaf: TOctNode;
var
  i: integer;
begin
  for i := 0 to 7 do
    if Assigned(Childs[i]) then
    begin
      if Childs[i].IsLeaf then
        Result := Childs[i] else
        Result := Childs[i].FindFirstLeaf;
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TOctNode.FindLastLeaf: TOctNode;
var
  i: integer;
begin
  for i := 7 downto 0 do
    if Assigned(Childs[i]) then
    begin
      if Childs[i].IsLeaf then
        Result := Childs[i] else
        Result := Childs[i].FindLastLeaf;
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TOctNode.FindLeafNearestToIndex(index: integer): TOctNode;
var
  i,j,k : integer;
begin
  for i := 1 to 7 do
  begin
    j := index + i;
    k := index - i;
    if (j < 8) and Assigned(Childs[j]) then
    begin
      if Childs[j].IsLeaf then
        Result := Childs[j] else
        Result := Childs[j].FindFirstLeaf;
      Exit;
    end;
    if (k >= 0) and Assigned(Childs[k]) then
    begin
      if Childs[k].IsLeaf then
        Result := Childs[k] else
        Result := Childs[k].FindLastLeaf;
      Exit;
    end;
  end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TOctNode.GetNearest(var color: TColor32): Boolean;
var
  i,j,k: integer;
  node: TOctNode;
begin
  if IsLeaf then
  begin
    Get(color, j);
    Result := true;
  end else
  begin
    i := GetIndex(color, level);
    if Assigned(Childs[i]) and
      Childs[i].GetNearest(color) then
    begin
      Result := true;
      Exit;
    end;

    // we should only get here when this color wasn't in the
    // the image used to construct the Octree, or if the color
    // has subsequently been deleted from the tree (DeleteColor).

    node := FindLeafNearestToIndex(i);
    if Assigned(node) then
    begin
      node.Get(color, k);
      Result := true;
    end else
      Result := false;
  end;
end;
//------------------------------------------------------------------------------

procedure TOctNode.Get(out color: TColor32; out freq: integer);
var
  argb: TARGB absolute color;
begin
  freq := Count;
  if Count > 0 then
  begin
    argb.R := TotalR div Count;
    argb.G := TotalG div Count;
    argb.B := TotalB div Count;
    argb.A := 255;
  end;
end;

//------------------------------------------------------------------------------
// TOctree methods
//------------------------------------------------------------------------------

constructor TOctree.Create;
begin
  Leaves := 0;
  Top := TOctNode.Create(0);
  Reducible8 := NullOctNodes8;
end;
//------------------------------------------------------------------------------

destructor TOctree.Destroy;
begin
  if Assigned(Top) then Delete(Top);
  inherited Destroy;
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

function TOctree.DeleteColor(color: TColor32): integer;
begin
  Result := top.DeleteColor(color, self);
end;
//------------------------------------------------------------------------------

procedure TOctree.Reset;
begin
  if Assigned(Top) then Delete(Top);
  Leaves := 0;
  Top := TOctNode.Create(0);
  Reducible8 := NullOctNodes8;
end;
//------------------------------------------------------------------------------

function PalSorter(p1, p2: Pointer): Boolean;
var
  cf1: PColFreqRec absolute p1;
  cf2: PColFreqRec absolute p2;
begin
 result := cf1.freq < cf2.freq;
end;
//------------------------------------------------------------------------------

procedure QuickSort(var ptrArray: TArrayOfPointer;
  l, r: Integer; sortFunc: TPointerSortFunc);
var
  i,j: integer;
  P, T: PColFreqRec;
begin
  repeat
    i := l;
    j := r;
    P := ptrArray[(l + r) shr 1];
    repeat
      while sortFunc(P, ptrArray[i]) do Inc(i);
      while sortFunc(ptrArray[j], P) do Dec(j);
      if i <= j then
      begin
        T := ptrArray[i];
        ptrArray[i] := ptrArray[j];
        ptrArray[j] := T;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if l < j then QuickSort(ptrArray, l, j, sortFunc);
    l := i;
  until i >= r;
end;
//------------------------------------------------------------------------------

procedure TOctree.BuildTree(image: TImage32; maxColors: integer = 256);
var
  i   : integer;
  pc  : PARGB;
begin
  Reset;
  pc := PARGB(image.PixelBase);
  for i := 0 to image.Width * image.Height - 1 do
  begin
    //ignore transparent and semi-transparent colors
    if pc.A >= OpacityThreshold then Add(pc.Color);
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

function TOctree.Reduce: Boolean;
var
  lvl, i,j,k,n, childCnt: integer;
  c: TColor32;
  node: TOctNode;
begin
  //find the lowest level with a reducible node ...
  lvl := LeafLevel -1;
  while (lvl > 0) and not Assigned(Reducible8[lvl]) do Dec(lvl);

  //reduce the most recently added node at level 'i' ...
  node := Reducible8[lvl];

  if not assigned(node) then
  begin
    // top level (should very rarely get here, so this is crude)
    for i := 0 to 7 do
    begin
      if not Assigned(top.Childs[i]) then Continue;
      n := top.Childs[i].Count;
      FreeAndNil(top.Childs[i]);
      for j := i +1 to 7 do
      begin
        if not Assigned(top.Childs[j]) then Continue;
        top.Childs[j].Get(c, k);
        top.Childs[j].AddCount(c, n);
        break;
      end;
      Dec(Leaves, 1);
      Result := true;
      Exit;
    end;
    Result := false;
  end else
  begin
    node := Reducible8[lvl];
    Reducible8[lvl] := node.Next;
    node.TotalR   := 0; node.TotalG := 0; node.TotalB := 0;
    node.Count    := 0; childCnt    := 0;
    //now merge the leaves into the parent node ...
    for i:= 0 to 7 do
      if Assigned (node.Childs[i]) then
      begin
        Inc (node.TotalR, node.Childs[i].TotalR);
        Inc (node.TotalG, node.Childs[i].TotalG);
        Inc (node.TotalB, node.Childs[i].TotalB);
        Inc (node.Count, node.Childs[i].Count);
        FreeAndNil(node.Childs[i]);
        inc(childCnt);
      end;
    Dec(Leaves, childCnt -1);
    Result := true;
  end;
end;
//------------------------------------------------------------------------------

procedure AddColor(octree: TOctree; color: TColor32;
  var node: TOctNode; level: integer);
begin
  if not Assigned(node) then
  begin
    node:= TOctNode.Create(level +1);
    if node.IsLeaf then
    begin
      Inc(octree.Leaves);
    end else
    begin
      node.Next  := octree.Reducible8[node.level];
      octree.Reducible8[node.level] := node;
    end;
  end;

  if node.IsLeaf then node.AddOne(color)
  else AddColor(octree, color, node.Childs[GetIndex(color, node.level)], node.level);
end;
//------------------------------------------------------------------------------

procedure TOctree.Add(color: TColor32);
begin
  inc(TotalCount);
  AddColor(self, color, Top, 0);
end;
//------------------------------------------------------------------------------

procedure TOctree.GetNearest(var color: TColor32);
var
  a: TColor32;
begin
  a := color and $FF000000;
  Top.GetNearest(color);
  color := (color and $FFFFFF) or a;
end;
//------------------------------------------------------------------------------

procedure TOctree.GetTreePalette(out colors: TArrayOfColor32;
  out freq: TArrayOfInteger);
var
  count: integer;

  procedure FillPalette(Node: TOctNode);
  var
    i: integer;
  begin
    if (Node.IsLeaf) then
    begin
      Node.Get(colors[Count], freq[Count]);
      Inc(Count);
    end else
    begin
      for i := 0 to 7 do
        if assigned(Node.Childs[i]) then
          FillPalette(Node.Childs[i]);
    end;
  end;

begin
  SetLength(colors, Leaves);
  SetLength(freq, Leaves +1);
  count := 0;
  FillPalette(Top);
  freq[count] := TotalCount;
end;
//------------------------------------------------------------------------------

procedure TOctree.GetPalette(palSize: integer;
  out colors: TArrayOfColor32; out freq: TArrayOfInteger);
var
  i, len  : integer;
  cf      : PColFreqRec;
  cfArr   : TArrayOfColFreq;
begin
  // Reducing a palette by just calling 'Reduce' will return inferior
  // results once palette sizes are sufficiently small (eg < 64 colors).
  // Better results can be achieved by deleting, in order, colors with the
  // lowest frequency.

  i := Max(64, palSize);
  while Leaves > i do Reduce;
  GetTreePalette(colors, freq);
  if (Leaves <= palSize) then Exit;

  len := Length(colors);
  SetLength(cfArr, len);
  for i := 0 to len -1 do
    new(cfArr[i]);

  try
    // fill and sort the color-frequency array
    // with the least frequently used colors listed last
    for i := 0 to len -1 do
    begin
      cf := cfArr[i];
      cf.color := colors[i];
      cf.freq := freq[i];
    end;
    QuickSort(TArrayOfPointer(cfArr), 0, len -1, PalSorter);

    // while the palette's too large,
    // remove the least frequently used color
    dec(len);
    while Leaves > palSize do
    begin
      cf := cfArr[len];
      cf.freq := DeleteColor(cf.color);
      dec(len);
    end;
  finally
    // clean up memory allocation
    for i := 0 to High(cfArr) do Dispose(cfArr[i]);
  end;

  // finally repeat GetTreePalette
  GetTreePalette(colors, freq);
end;
//------------------------------------------------------------------------------

procedure TOctree.ApplyPalette(image: TImage32);
var
  i: integer;
  pc: PARGB;
begin
  pc := PARGB(image.PixelBase);
  for i := 0 to image.Width * image.Height -1 do
  begin
    if pc.A < OpacityThreshold then
      pc.Color := clNone32 else
      top.GetNearest(pc.Color);
    inc(pc);
  end;
end;

//------------------------------------------------------------------------------
// Floyd-Steinberg Dithering -
// see https://en.wikipedia.org/wiki/Floyd-Steinberg_dithering
//------------------------------------------------------------------------------

type
  TMulDibTable = array [-255 .. 255] of integer;

var
  Mul1Div16Table: TMulDibTable;
  Mul3Div16Table: TMulDibTable;
  Mul5Div16Table: TMulDibTable;
  Mul7Div16Table: TMulDibTable;

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

procedure Dither(image: TImage32; octree: TOctree); overload;
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
          octree.GetNearest(newC.Color);

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

procedure Dither(image: TImage32; const palette: TArrayOfColor32); overload;
var
  qeRq,qeGq, qeBq : integer;

  procedure AdjustPixel(pc: PARGB; r, g, b: integer);
  begin
    pc.R := ClampByte(pc.R + r);
    pc.G := ClampByte(pc.G + g);
    pc.B := ClampByte(pc.B + b);
    dec(qeRq, r); dec(qeGq, g); dec(qeBq, b);
  end;

var
  j            : Cardinal;
  X, Y, W      : Integer;
  qeR,qeG, qeB : integer;
  oldC         : TARGB;
  newC         : PARGB;
  tmp          : TImage32;
  allColors    : PByteArray;
const
  cube256 = 256 * 256 * 256;
begin
  allColors := AllocMem(cube256);
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

          j := allColors[newC.Color and $FFFFFF];
          if j = 0 then //not found
          begin
            j := GetNearestPaletteColorIndex(newC.Color, palette);
            allColors[newC.Color and $FFFFFF] := j +1;
            newC.Color := palette[j];
          end else
            newC.Color := palette[j -1];

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
    FreeMem(allColors);
  end;
end;

//------------------------------------------------------------------------------
// CreatePalette...
//------------------------------------------------------------------------------

function CreatePaletteOctree(image: TImage32): TOctree;
var
  i: integer;
  pc: PARGB;
begin
  Result := TOctree.Create;
  pc := PARGB(image.PixelBase);
  for i := 0 to image.Width * image.Height - 1 do
  begin
    //ignore transparent and semi-transparent colors
    if pc.A >= OpacityThreshold then
      Result.Add(pc.Color);
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

function MakeAndApplyPalette(image: TImage32;
  MaxColors: integer; UseDithering: Boolean;
  out frequencies: TArrayOfInteger): TArrayOfColor32;
var
  octree: TOctree;
begin
  result := nil;
  octree := CreatePaletteOctree(image);
  try
    if UseDithering then
    begin
      octree.GetPalette(MaxColors, result, frequencies);
      Dither(image, octree);
      Exit;
    end;
    if MaxColors > 256 then MaxColors := 256;
    octree.GetPalette(MaxColors, result, frequencies);
    octree.ApplyPalette(image);
  finally
    octree.Free;
  end;
end;
//------------------------------------------------------------------------------

function MakeAndApplyPalette(image: TImage32;
  MaxColors: integer; UseDithering: Boolean): TArrayOfColor32;
var
  dummy: TArrayOfInteger;
begin
  Result := MakeAndApplyPalette(image, MaxColors, UseDithering, dummy);
end;
//------------------------------------------------------------------------------

function MakePalette(image: TImage32; MaxColors: integer): TArrayOfColor32;
var
  dummy: TArrayOfInteger;
begin
  result := MakePalette(image, MaxColors, dummy);
end;
//------------------------------------------------------------------------------

function MakePalette(image: TImage32; MaxColors: integer;
  out frequencies: TArrayOfInteger): TArrayOfColor32;
var
  octree: TOctree;
begin
  MaxColors := Max(2, Min(256, MaxColors));
  result := nil;
  octree := CreatePaletteOctree(image);
  try
    while octree.Leaves > 64 do octree.Reduce;
    if octree.Leaves > MaxColors then
      octree.GetPalette(MaxColors, result, frequencies) else
      octree.GetTreePalette(result, frequencies);
  finally
    octree.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure ApplyPalette(image: TImage32;
  const palette: TArrayOfColor32; UseDithering: Boolean = true);
var
  i, len: integer;
  j: cardinal;
  pc: PARGB;
  allColors: PByteArray;
const
  cube256 = 256 * 256 * 256;
begin
  len := Length(palette);
  if len = 0 then Exit;
  if UseDithering then
  begin
    Dither(image, palette);
    Exit;
  end;

  pc := PARGB(image.PixelBase);
  allColors := AllocMem(cube256);
  try
    for i := 0 to image.Width * image.Height - 1 do
    begin
      if pc.A >= OpacityThreshold then
      begin
        j := allColors[pc.Color and $FFFFFF];
        if j = 0 then //not found
        begin
          j := GetNearestPaletteColorIndex(pc.Color, palette);
          allColors[pc.Color and $FFFFFF] := j +1;
          pc.Color := palette[j];
        end else
        begin
          pc.Color := palette[j -1];
        end;
      end
      else
        pc.Color := clNone32;
      inc(pc);
    end;
  finally
    FreeMem(allColors);
  end;
end;
//------------------------------------------------------------------------------

function GetPalettes(image: TImage32;
  const sizesNeeded: array of integer;
  out palettes: TArrayOfArrayOfColor32;
  out frequencies: TArrayOfArrayOfInteger): integer;
var
  i, len: integer;
  octree: TOctree;
  sizes: TArrayOfInteger;
begin
  len := Length(sizesNeeded);
  SetLength(sizes, len);
  SetLength(palettes, len);
  SetLength(frequencies, len);
  if len = 0 then
  begin
    Result := 0;
    Exit;
  end;

  //first make sure sizes are in ascending order
  for i := 0 to len -1 do
    sizes[i] := sizesNeeded[i];
  QuickSortIntArray(sizes, 0, len -1);

  octree := CreatePaletteOctree(image);
  try
    Result := octree.PixelCount;
    i := len -1;
    while (i > 0) and
      ((sizes[i] > 256) or (octree.ColorCount <= sizes[i-1])) do dec(i);
    for i := i downto 0 do
      octree.GetPalette(sizes[i], palettes[i], frequencies[i]);
  finally
    octree.Free;
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
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
{$ENDIF}

procedure DrawPalette(image: TImage32; const palette: TArrayOfColor32);
var
  i, w, h, len: integer;
  rec: TRect;
begin
  len := length(palette);
  if len < 16 then w := len else w := 16;
  h := (len +15) div 16;
  image.SetSize(w * 16, h * 16);
  rec := Img32.Vector.Rect(0,0,16,16);
  for i := 0 to len -1 do
  begin
    image.FillRect(rec, palette[i] or $FF000000);
    if (i + 1) mod w = 0 then
      Types.OffsetRect(rec, -15 * w, 16) else
      Types.OffsetRect(rec, 16, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure SavePalette(const filename: string; const palette: TArrayOfColor32);
var
  tmpImg: TImage32;
begin
  tmpImg := TImage32.Create;
  try
    DrawPalette(tmpImg, palette);
    tmpImg.SaveToFile(filename);
  finally
    tmpImg.Free;
  end;
end;
//------------------------------------------------------------------------------

function BlackWhitePal: TArrayOfColor32;
var
  i, len: integer;
begin
  len := Length(MonoPal2);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := MonoPal2[i];
end;
//------------------------------------------------------------------------------

function DefaultMacPal16: TArrayOfColor32;
var
  i, len: integer;
begin
  len := Length(macPal16);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := macPal16[i];
end;
//------------------------------------------------------------------------------

function DefaultWinPal16: TArrayOfColor32;
var
  i, len: integer;
begin
  len := Length(winPal16_int);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := winPal16_int[i];
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

