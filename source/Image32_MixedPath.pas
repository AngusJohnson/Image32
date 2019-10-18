unit Image32_MixedPath;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.26                                                            *
* Date      :  14 October 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Module that supports mixed-type (curved & straight) paths       *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Math, Image32, Image32_Layers;

{$I Image32.inc}

type
  TMixedType = (mtStraight, mtCBezier, mtQBezier, mtCSpline, mtQSpline);

  TMixedPoint = record
    PointType: TMixedType;
    case boolean of
      false: (Point    : TPointD);
      true: (X: double; Y: double);
  end;

  //TMixedPath: structure for paths containing both curved and straight edges
  TMixedPath = class
  private
    fCapacity       : integer;
    fTotalCount     : integer;      //total control points (ie <= capacity)
    fCurrentType    : TMixedType;
    fCurrentCount   : integer;      //count of current PointType
    fCtrlPoints     : array of TMixedPoint;
    fFlattened      : TArrayOfPointD;
    function GetCtrlPoints: TArrayOfPointD;
    function GetPoint(index: integer): TPointD;
    procedure SetPoint(index: integer; const ctrlPt: TPointD);
    function GetPointType(index: integer): TMixedType;
    procedure SetPointType(index: integer; const mixedType: TMixedType);
    function GetFlattenedPath: TArrayOfPointD;
    procedure AddInternal(const pt: TPointD);
  public
    function Add(const pt: TPointD; pointType: TMixedType): Boolean; overload;
    function Add(const pt: TPointD): Boolean; overload;
    procedure Assign(mixedPath: TMixedPath);
    procedure AssignTo(mixedPath: TMixedPath);
    //CurrentTypeIsComplete: indicates if the current control points
    //constitute a valid path or whether more control points are required.
    function CurrentTypeIsComplete: Boolean;
    procedure Clear;
    //DeleteLast: deletes the last control point
    procedure DeleteLast;
    //DeleteLastType: starting at the end and heading toward the beginning,
    //deletes contiguous control points with the same TMixedType
    procedure DeleteLastType;
    procedure Offset(dx, dy: integer);
    procedure Rotate(const focalPoint: TPointD; angleRads: double);
    procedure Scale(sx, sy: double);

    property Points[index: integer]: TPointD
      read GetPoint write SetPoint; Default;
    property PointTypes[index: integer]: TMixedType
      read GetPointType write SetPointType;
    property CtrlPoints: TArrayOfPointD read GetCtrlPoints;
    property CurrentType: TMixedType read fCurrentType;
    property Count: integer read fTotalCount;
    property FlattenedPath: TArrayOfPointD read GetFlattenedPath;
  end;

  TSmoothType = (stSmoothSym, stSmoothAsym, stSharpWithHdls, stSharpNoHdls);

  TSmoothPoint = record
    PointType: TSmoothType;
    case boolean of
      false: (Point    : TPointD);
      true: (X: double; Y: double);
  end;

  TSmoothPath = class
  private
    fCapacity       : integer;
    fCount          : integer;
    fCtrlPoints     : array of TSmoothPoint;
    fFlattened      : TArrayOfPointD;
    fAutoAdjust     : Boolean;
    function GetCtrlPoints: TArrayOfPointD;
    function GetPoint(index: integer): TPointD;
    procedure SetPoint(index: integer; const pt: TPointD);
    function GetPointType(index: integer): TSmoothType;
    procedure SetPointType(index: integer; const newType: TSmoothType);
    function GetLastType: TSmoothType;

    function GetFlattenedPath: TArrayOfPointD;
    procedure AddInternal(const pt: TPointD; pointType: TSmoothType);
    procedure MovePoint(index: integer; const newPt: TPointD);
  public
    constructor Create; virtual;
    procedure Add(const pt: TPointD; pointType: TSmoothType); overload;
    procedure Add(const pt: TPointD); overload;
    procedure Assign(mixedPath: TSmoothPath);
    procedure AssignTo(mixedPath: TSmoothPath);
    function CurrentCurveIsComplete: Boolean;
    procedure Clear;
    //DeleteLast: deletes the last control point
    procedure DeleteLast;
    procedure Offset(dx, dy: integer);
    procedure Rotate(const focalPoint: TPointD; angleRads: double);
    procedure Scale(sx, sy: double);

    //AutoAdjust: Where moving one point might also move others
    property AutoAdjust: Boolean read fAutoAdjust write fAutoAdjust;
    property LastType: TSmoothType read GetLastType;
    property Points[index: integer]: TPointD
      read GetPoint write SetPoint; Default;
    property PointTypes[index: integer]: TSmoothType
      read GetPointType write SetPointType;
    property CtrlPoints: TArrayOfPointD read GetCtrlPoints;
    property Count: integer read fCount;
    property FlattenedPath: TArrayOfPointD read GetFlattenedPath;
  end;

  //Anticipating that both TMixedPath and TSmoothPath will
  //mostly be used inside a TLayeredImage32 ...

  TMixedPathLayer32 = class(TLayer32)
  private
    fMixedPath: TMixedPath;
  public
    constructor Create(owner: TLayeredImage32); override;
    destructor Destroy; override;
    procedure Offset(dx, dy: integer); override;
    property MixedPath: TMixedPath read fMixedPath;
  end;

  TSmoothPathLayer32 = class(TLayer32)
  private
    //saved copy of smoothPath in case layer needs re-editing
    fSmoothPath: TSmoothPath;
  public
    constructor Create(owner: TLayeredImage32); override;
    destructor Destroy; override;
    procedure Offset(dx, dy: integer); override;
    property SmoothPath: TSmoothPath read fSmoothPath;
  end;

  procedure DrawMixedPathOnDesigner(const mixedPath: TMixedPath;
    designerLayer: TDesignerLayer32);
  procedure DrawSmoothPathOnDesigner(const smoothPath: TSmoothPath;
    designerLayer: TDesignerLayer32);

implementation

uses
  Image32_Draw, Image32_Vector, Image32_Extra;

resourcestring
  rsMixedPathRangeError = 'TMixedPath: index is out of range.';
  rsSmoothPathRangeError = 'TSmoothPath: index is out of range.';

const
  capacityIncrement = 16;

//------------------------------------------------------------------------------
// TMixedPath
//------------------------------------------------------------------------------

procedure TMixedPath.Clear;
begin
  fCtrlPoints := nil;
  fFlattened := nil;
  fCapacity := 0;
  fTotalCount := 0;
  fCurrentCount := 0;
end;
//------------------------------------------------------------------------------

procedure TMixedPath.Assign(mixedPath: TMixedPath);
begin
  mixedPath.AssignTo(self);
end;
//------------------------------------------------------------------------------

procedure TMixedPath.AssignTo(mixedPath: TMixedPath);
begin
  if not assigned(mixedPath) then Exit;
  mixedPath.fCapacity := self.fCapacity;
  mixedPath.fTotalCount := self.fTotalCount;
  mixedPath.fCurrentType := self.fCurrentType;
  mixedPath.fCurrentCount := self.fCurrentCount;
  mixedPath.fCtrlPoints := Copy(self.fCtrlPoints, 0, self.fCapacity);
  mixedPath.fFlattened := nil;
end;
//------------------------------------------------------------------------------

function TMixedPath.GetCtrlPoints: TArrayOfPointD;
var
  i, cnt: integer;
begin
  cnt := Count;
  setLength(Result, cnt);
  for i := 0 to cnt - 1 do
    Result[i] := fCtrlPoints[i].Point;
end;
//------------------------------------------------------------------------------

function TMixedPath.GetPoint(index: integer): TPointD;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsMixedPathRangeError);
  Result := fCtrlPoints[index].Point;
end;
//------------------------------------------------------------------------------

procedure TMixedPath.SetPoint(index: integer; const ctrlPt: TPointD);
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsMixedPathRangeError);
  fCtrlPoints[index].Point := ctrlPt;
end;
//------------------------------------------------------------------------------

function TMixedPath.GetPointType(index: integer): TMixedType;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsMixedPathRangeError);
  Result := fCtrlPoints[index].PointType;
end;
//------------------------------------------------------------------------------

procedure TMixedPath.SetPointType(index: integer; const mixedType: TMixedType);
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsMixedPathRangeError);
  fCtrlPoints[index].PointType := mixedType;
end;
//------------------------------------------------------------------------------

function TMixedPath.GetFlattenedPath: TArrayOfPointD;
var
  i,j,k, highI: integer;
  tmp: TArrayOfPointD;
  pointType: TMixedType;
  virtualPt: TPointD;
begin
  if not assigned(fCtrlPoints) then
  begin
    Result := nil;
    Exit;
  end
  else if assigned(fFlattened) then
  begin
    Result := fFlattened;
    Exit;
  end;

  virtualPt := NullPointD;
  i := 0;
  highI := fTotalCount -1;
  while i < highI do
  begin
    j := i +1;
    pointType := fCtrlPoints[j].PointType;
    while (j < highI) and
      (fCtrlPoints[j+1].PointType = pointType) do inc(j);
    case pointType of
      mtStraight:
        begin
          SetLength(tmp, j - i +1);
          for k := i to j do
            tmp[k-i] := fCtrlPoints[k].Point;
          Result := JoinPaths(Result, tmp);
        end;
      mtCBezier:
        begin
          if j < i + 3 then break;
          SetLength(tmp, 4);
          while i < j - 2 do
          begin
            for k := i to i +3 do
              tmp[k-i] := fCtrlPoints[k].Point;
            Result := JoinPaths(Result, CBezier(tmp));
            inc(i, 3);
          end;
        end;
      mtQBezier:
        begin
          if j < i + 2 then break;
          SetLength(tmp, 3);
          while i < j - 1 do
          begin
            for k := i to i +2 do
              tmp[k-i] := fCtrlPoints[k].Point;
            Result := JoinPaths(Result, QBezier(tmp));
            inc(i, 2);
          end;
        end;
      mtCSpline:
        begin
          SetLength(tmp, 4);
          tmp[0] := fCtrlPoints[i].Point;
          if i = 0 then
          begin
            if j < 3 then break;
            tmp[1] := fCtrlPoints[1].Point;
            inc(i);
          end
          else if fCtrlPoints[i].PointType = mtQSpline then
          begin
            if j < i +2 then break;
            tmp[1] := ReflectPoint(virtualPt, tmp[0]);
          end else
          begin
            if j < i +2 then break;
            tmp[1] := ReflectPoint(fCtrlPoints[i-1].Point, tmp[0]);
          end;

          tmp[2] := fCtrlPoints[i+1].Point;
          tmp[3] := fCtrlPoints[i+2].Point;
          Result := JoinPaths(Result, CSpline(tmp));
          inc(i,2);
          while i < j - 1 do
          begin
            tmp[0] := fCtrlPoints[i].Point;
            tmp[1] := ReflectPoint(fCtrlPoints[i-1].Point, tmp[0]);
            tmp[2] := fCtrlPoints[i+1].Point;
            tmp[3] := fCtrlPoints[i+2].Point;
            Result := JoinPaths(Result, CSpline(tmp));
            inc(i, 2);
          end;
        end;
      mtQSpline:
        begin
          SetLength(tmp, 3);
          tmp[0] := fCtrlPoints[i].Point;
          if i = 0 then
          begin
            if j < 2 then break;
            tmp[1] := fCtrlPoints[1].Point;
            inc(i);
          end else
            tmp[1] := ReflectPoint(fCtrlPoints[i-1].Point, tmp[0]);

          tmp[2] := fCtrlPoints[i+1].Point;
          Result := JoinPaths(Result, QSpline(tmp));
          inc(i);
          while i < j do
          begin
            tmp[0] := fCtrlPoints[i].Point;
            tmp[1] := ReflectPoint(tmp[1], tmp[0]);
            tmp[2] := fCtrlPoints[i+1].Point;
            Result := JoinPaths(Result, QSpline(tmp));
            inc(i);
          end;
          virtualPt := tmp[1];
        end;
    end;
    i := j;
  end;
  fFlattened := Result;
end;
//------------------------------------------------------------------------------

function TMixedPath.CurrentTypeIsComplete: Boolean;
var
  isFirst: Boolean;
begin
  result := true;
  if fTotalCount > 0 then
  begin
    isFirst := fTotalCount = fCurrentCount;
    case fCurrentType of
      mtStraight:
        if isFirst then Result := fCurrentCount > 1
        else Result := fCurrentCount > 0;
      mtCBezier:
        if isFirst then Result := (fCurrentCount -1) mod 3 = 0
        else Result := fCurrentCount mod 3 = 0;
      mtQBezier:
        if isFirst then Result := (fCurrentCount -1) mod 2 = 0
        else Result := fCurrentCount mod 2 = 0;
      mtCSpline:
        if isFirst then Result := fCurrentCount > 3
        else Result := fCurrentCount mod 2 = 0;
      mtQSpline:
        if isFirst then Result := fCurrentCount > 2
        else Result := fCurrentCount > 0;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TMixedPath.AddInternal(const pt: TPointD);
begin
  if fTotalCount = fCapacity then
  begin
    inc(fCapacity, capacityIncrement);
    SetLength(fCtrlPoints, fCapacity);
  end;
  fCtrlPoints[fTotalCount].PointType := fCurrentType;
  fCtrlPoints[fTotalCount].Point := pt;
  inc(fTotalCount);
  inc(fCurrentCount);
  fFlattened := nil;
end;
//------------------------------------------------------------------------------

function TMixedPath.Add(const pt: TPointD): Boolean;
begin
  //nb: the first Add() must specify a TMixedType
  Result := assigned(fCtrlPoints);
  if not Result then Exit;
  AddInternal(pt);
end;
//------------------------------------------------------------------------------

function TMixedPath.Add(const pt: TPointD; pointType: TMixedType): Boolean;
begin
  if not assigned(fCtrlPoints) then
  begin
    fCurrentType := pointType;
    Result := true;
  end
  else if (pointType <> fCurrentType) then
  begin
    Result := CurrentTypeIsComplete;
    if not Result then Exit;
    fCurrentType := pointType;
    fCurrentCount := 0;
  end else
    Result := true;
  AddInternal(pt);
end;
//------------------------------------------------------------------------------

procedure TMixedPath.DeleteLast;
begin
  if fTotalCount = 0 then
    Exit
  else if fCurrentCount > 1 then
  begin
    dec(fTotalCount, fCurrentCount);
    dec(fCurrentCount);
  end else
    DeleteLastType;
  fFlattened := nil;
end;
//------------------------------------------------------------------------------

procedure TMixedPath.DeleteLastType;
var
  i: integer;
  pt: TMixedType;
begin
  if fTotalCount = 0 then Exit;

  dec(fTotalCount, fCurrentCount);
  if fTotalCount > 0 then
  begin
    pt := fCtrlPoints[fTotalCount].PointType;
    i := fTotalCount -1;
    while (i >= 0) and (fCtrlPoints[i].PointType = pt) do Dec(i);
    fCurrentCount := fTotalCount - i;
  end
  else
    fCurrentCount := 0;
  fFlattened := nil;
end;
//------------------------------------------------------------------------------

procedure TMixedPath.Offset(dx, dy: integer);
var
  i: integer;
begin
  for i := 0 to Count -1 do
  begin
    fCtrlPoints[i].X := fCtrlPoints[i].X + dx;
    fCtrlPoints[i].Y := fCtrlPoints[i].Y + dy;
  end;
end;
//------------------------------------------------------------------------------

procedure TMixedPath.Rotate(const focalPoint: TPointD; angleRads: double);
var
  i: integer;
  sinA, cosA: extended;
begin
  Math.SinCos(angleRads, sinA, cosA);
  for i := 0 to Count -1 do
    RotatePoint(fCtrlPoints[i].Point, focalPoint, sinA, cosA);
end;
//------------------------------------------------------------------------------

procedure TMixedPath.Scale(sx, sy: double);
var
  i: integer;
begin
  for i := 0 to Count -1 do
  begin
    fCtrlPoints[i].X := fCtrlPoints[i].X * sx;
    fCtrlPoints[i].Y := fCtrlPoints[i].Y * sy;
  end;
end;

//------------------------------------------------------------------------------
// TSmoothPath
//------------------------------------------------------------------------------

constructor TSmoothPath.Create;
begin
  fAutoAdjust := true;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Clear;
begin
  fCtrlPoints := nil;
  fFlattened := nil;
  fCapacity := 0;
  fCount := 0;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Assign(mixedPath: TSmoothPath);
begin
  mixedPath.AssignTo(self);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.AssignTo(mixedPath: TSmoothPath);
begin
  if not assigned(mixedPath) then Exit;
  mixedPath.fCapacity := self.fCapacity;
  mixedPath.fCount := self.fCount;
  mixedPath.fCtrlPoints := Copy(self.fCtrlPoints, 0, self.fCapacity);
  mixedPath.fFlattened := nil;
end;
//------------------------------------------------------------------------------

function TSmoothPath.GetCtrlPoints: TArrayOfPointD;
var
  i, cnt: integer;
begin
  cnt := Count;
  setLength(Result, cnt);
  for i := 0 to cnt - 1 do
    Result[i] := fCtrlPoints[i].Point;
end;
//------------------------------------------------------------------------------

function TSmoothPath.GetPoint(index: integer): TPointD;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSmoothPathRangeError);
  Result := fCtrlPoints[index].Point;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.SetPoint(index: integer; const pt: TPointD);
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSmoothPathRangeError);
  if fAutoAdjust then
    case index mod 3 of
      0: MovePoint(index, pt);
      1: if (index > 1) and (fCtrlPoints[index -1].PointType in
        [stSmoothSym, stSmoothAsym]) then
          MovePoint(index, pt) else
          fCtrlPoints[index].Point := pt;
      2: if (index < Count - 2) and (fCtrlPoints[index+1].PointType in
        [stSmoothSym, stSmoothAsym]) then
          MovePoint(index, pt) else
          fCtrlPoints[index].Point := pt;
    end
  else
    fCtrlPoints[index].Point := pt;
end;
//------------------------------------------------------------------------------

function TSmoothPath.GetPointType(index: integer): TSmoothType;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSmoothPathRangeError);
  dec(index, index mod 3);
  Result := fCtrlPoints[index].PointType;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.SetPointType(index: integer; const newType: TSmoothType);
var
  idx1, idx2: integer;
  vec: TPointD;
  dist: double;
  oldType: TSmoothType;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSmoothPathRangeError);
  //only assign TSmoothType to 'node' controls, not 'handles'
  if index mod 3 > 0 then Exit;

  oldType := fCtrlPoints[index].PointType;
  if (newType = oldType) then Exit;
  fCtrlPoints[index].PointType := newType;
  if (Count = 1) or (index mod 3 > 0) then Exit;

  if (oldType = stSharpNoHdls) then
  begin
    if (index > 0) then
      fCtrlPoints[index -1].Point :=
        MidPoint(fCtrlPoints[index -2].Point, fCtrlPoints[index].Point);
    if (index < Count - 3) then
      fCtrlPoints[index +1].Point :=
        MidPoint(fCtrlPoints[index +2].Point, fCtrlPoints[index].Point);
  end;

  if index = 0 then idx1 := 0
  else idx1 := index -1;
  if index > Count -2 then idx2 := index
  else idx2 := index +1;

  case newType of
    stSmoothSym:
      if (idx2 > index) then
      begin
        vec := GetUnitVector(fCtrlPoints[idx1].Point, fCtrlPoints[idx2].Point);
        dist := (Distance(fCtrlPoints[index].Point, fCtrlPoints[idx1].Point) +
          Distance(fCtrlPoints[index].Point, fCtrlPoints[idx2].Point)) * 0.5;
        if (idx1 < index) then
          fCtrlPoints[index-1].Point :=
            OffsetPoint(fCtrlPoints[index].Point, -vec.X *dist, -vec.Y *dist);
        fCtrlPoints[index+1].Point :=
          OffsetPoint(fCtrlPoints[index].Point, vec.X *dist, vec.Y *dist);
      end;
    stSmoothAsym:
      if (idx2 > index) then
      begin
        vec := GetUnitVector(fCtrlPoints[idx1].Point, fCtrlPoints[idx2].Point);
        if (idx1 < index) then
        begin
          dist := Distance(fCtrlPoints[idx1].Point, fCtrlPoints[index].Point);
          fCtrlPoints[idx1].Point :=
            OffsetPoint(fCtrlPoints[index].Point, -vec.X *dist, -vec.Y *dist);
        end;
        dist := Distance(fCtrlPoints[idx2].Point, fCtrlPoints[index].Point);
        fCtrlPoints[idx2].Point :=
          OffsetPoint(fCtrlPoints[index].Point, vec.X *dist, vec.Y *dist);
      end;
    stSharpWithHdls:
      if (idx2 > index) then
      begin
        if (idx1 < index) then
        begin
          vec := GetUnitVector(fCtrlPoints[index].Point, fCtrlPoints[idx1].Point);
          dist := Distance(fCtrlPoints[idx1].Point, fCtrlPoints[index].Point);
          fCtrlPoints[idx1].Point :=
            OffsetPoint(fCtrlPoints[index].Point, vec.X *dist, vec.Y *dist);
        end;
        vec := GetUnitVector(fCtrlPoints[index].Point, fCtrlPoints[idx2].Point);
        dist := Distance(fCtrlPoints[idx2].Point, fCtrlPoints[index].Point);
        fCtrlPoints[idx2].Point :=
          OffsetPoint(fCtrlPoints[index].Point, vec.X *dist, vec.Y *dist);
      end;
    stSharpNoHdls:
      begin
        fCtrlPoints[idx1].Point := fCtrlPoints[index].Point;
        fCtrlPoints[idx2].Point := fCtrlPoints[index].Point;
      end;
  end;
  if (index = Count -3) then DeleteLast
  else if (newType = stSharpNoHdls) and (index = Count - 2) then DeleteLast;
end;
//------------------------------------------------------------------------------

function TSmoothPath.GetLastType: TSmoothType;
begin
  if (fCount = 0) then
    Result := stSmoothSym else
    result := PointTypes[fCount -1];
end;
//------------------------------------------------------------------------------

function TSmoothPath.GetFlattenedPath: TArrayOfPointD;
var
  i,j,k, cnt: integer;
  tmp: TArrayOfPointD;
begin
  Result := nil;
  if not assigned(fCtrlPoints) or (Count < 4) then Exit;
  SetLength(tmp, 4);
  j := 0;
  cnt := (fCount -1) div 3;
  for i := 1 to cnt do
  begin
    for k := j to j + 3 do
        tmp[k - j] := fCtrlPoints[k].Point;
    Result := JoinPaths(Result, CBezier(tmp));
    inc(j, 3);
  end;
  fFlattened := Result;
end;
//------------------------------------------------------------------------------

function TSmoothPath.CurrentCurveIsComplete: Boolean;
begin
  result := (fCount = 0) or ((fCount > 3) and (fCount mod 3 = 1));
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.AddInternal(const pt: TPointD; pointType: TSmoothType);

  procedure CheckCapacity(neededCapacity: integer);
  begin
    if neededCapacity <= fCapacity then Exit;
    inc(fCapacity, capacityIncrement);
    SetLength(fCtrlPoints, fCapacity);
  end;

  function GetPointAtFracDist(dist: Double): TPointD;
  var
    prevPt: TPointD;
  begin
    prevPt := fCtrlPoints[fCount - 1].Point;
    Result.X := prevPt.X + (pt.X - prevPt.X) * dist;
    Result.Y := prevPt.Y + (pt.Y - prevPt.Y) * dist;
  end;

  function ReflectedPoint: TPointD;
  var
    prevPt, prevPrevPt: TPointD;
  begin
    prevPt := fCtrlPoints[fCount - 1].Point;
    prevPrevPt := fCtrlPoints[fCount - 2].Point;
    Result := ReflectPoint(prevPrevPt, prevPt);
  end;

var
  i: integer;
  oldType: TSmoothType;
const
  OneThird  = 0.3333;
  OneHalf   = 0.5;
begin
  oldType := GetLastType;
  if (fCount = 0) or (fAutoAdjust = false) then
  begin
    CheckCapacity(fCount +1);
    fCtrlPoints[fCount].Point := pt;
    if fCount mod 3 = 0 then
      fCtrlPoints[fCount].PointType := pointType;
    inc(fCount);
  end else
  begin
    //auto-add 2 handles before adding the end control
    CheckCapacity(fCount + 3);
    i := fCount mod 3;
    if i = 1 then
    begin
      case oldType of
        stSmoothSym, stSmoothAsym:
          if fCount = 1 then
            fCtrlPoints[fCount].Point := GetPointAtFracDist(OneThird) else
            fCtrlPoints[fCount].Point :=  ReflectedPoint;
         stSharpWithHdls:
           fCtrlPoints[fCount].Point := GetPointAtFracDist(OneThird);
         stSharpNoHdls:
           fCtrlPoints[fCount].Point := fCtrlPoints[fCount - 1].Point;
      end;
      inc(fCount); i := 2;
    end;

    if i = 2 then
    begin
      case pointType of
        stSmoothSym, stSmoothAsym, stSharpWithHdls:
          fCtrlPoints[fCount].Point := GetPointAtFracDist(OneHalf);
        stSharpNoHdls: fCtrlPoints[fCount].Point := pt;
      end;
      inc(fCount);
    end;

    fCtrlPoints[fCount].Point := pt;
    fCtrlPoints[fCount].PointType := pointType;
    inc(fCount);
  end;
  fFlattened := nil;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Add(const pt: TPointD);
begin
  AddInternal(pt, GetLastType);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Add(const pt: TPointD; pointType: TSmoothType);
begin
  AddInternal(pt, pointType);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.DeleteLast;
begin
  if fCount = 0  then Exit;
  dec(fCount);
  if fCount < 2 then Exit
  else if fCount < 4 then fCount := 1
  else dec(fCount, (fCount -1) mod 3);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.MovePoint(index: integer; const newPt: TPointD);
var
  oldPt, pt, vec: TPointD;
  dx,dy, dist: double;
  pointType: TSmoothType;
  i: integer;

  procedure Offset(idx: integer; dx, dy: double);
  var
    ctrlPt: TSmoothPoint;
  begin
    ctrlPt := fCtrlPoints[idx];
    ctrlPt.X := ctrlPt.X + dx;
    ctrlPt.Y := ctrlPt.Y + dy;
    fCtrlPoints[idx] := ctrlPt;
  end;

  procedure MoveTo(idx: integer; const pt: TPointD);
  var
    ctrlPt: TSmoothPoint;
  begin
    ctrlPt := fCtrlPoints[idx];
    ctrlPt.Point := pt;
    fCtrlPoints[idx] := ctrlPt;
  end;

begin
  oldPt := fCtrlPoints[index].Point;
  MoveTo(index, newPt);
  i := index mod 3;
  case i of
    0:
      begin
        //move adjacent handles too
        dx := newPt.X - oldPt.X;
        dy := newPt.Y - oldPt.Y;
        if index > 0 then Offset(index -1, dx, dy);
        if index < Count -1 then Offset(index +1, dx, dy);
      end;
    1:
      begin
        if index = 1 then Exit;
        pointType := fCtrlPoints[index -1].PointType;
        case PointType of
          stSmoothSym:
            begin
              pt := fCtrlPoints[index-1].Point;
              dist := Distance(pt, fCtrlPoints[index].Point);
              vec := GetUnitVector(pt, fCtrlPoints[index].Point);
              pt := OffsetPoint(pt, -vec.X * dist, -vec.Y * dist);
              MoveTo(index -2, pt);
            end;
          stSmoothAsym:
            begin
              pt := fCtrlPoints[index-1].Point;
              dist := Distance(pt, fCtrlPoints[index -2].Point);
              vec := GetUnitVector(pt, fCtrlPoints[index].Point);
              pt := OffsetPoint(pt, -vec.X * dist, -vec.Y * dist);
              MoveTo(index -2, pt);
            end;
        end;
      end;
    2:
      begin
        if index >= Count -2 then Exit;
        pointType := fCtrlPoints[index +1].PointType;
        case PointType of
          stSmoothSym:
            begin
              pt := fCtrlPoints[index+1].Point;
              dist := Distance(pt, fCtrlPoints[index].Point);
              vec := GetUnitVector(pt, fCtrlPoints[index].Point);
              pt := OffsetPoint(pt, -vec.X * dist, -vec.Y * dist);
              MoveTo(index +2, pt);
            end;
          stSmoothAsym:
            begin
              pt := fCtrlPoints[index+1].Point;
              dist := Distance(pt, fCtrlPoints[index +2].Point);
              vec := GetUnitVector(pt, fCtrlPoints[index].Point);
              pt := OffsetPoint(pt, -vec.X * dist, -vec.Y * dist);
              MoveTo(index +2, pt);
            end;
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Offset(dx, dy: integer);
var
  i: integer;
begin
  for i := 0 to Count -1 do
  begin
    fCtrlPoints[i].X := fCtrlPoints[i].X + dx;
    fCtrlPoints[i].Y := fCtrlPoints[i].Y + dy;
  end;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Rotate(const focalPoint: TPointD; angleRads: double);
var
  i: integer;
  sinA, cosA: extended;
begin
  Math.SinCos(angleRads, sinA, cosA);
  for i := 0 to Count -1 do
    RotatePoint(fCtrlPoints[i].Point, focalPoint, sinA, cosA);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Scale(sx, sy: double);
var
  i: integer;
begin
  for i := 0 to Count -1 do
  begin
    fCtrlPoints[i].X := fCtrlPoints[i].X * sx;
    fCtrlPoints[i].Y := fCtrlPoints[i].Y * sy;
  end;
end;

//------------------------------------------------------------------------------
// TMixedPathLayer32
//------------------------------------------------------------------------------

constructor TMixedPathLayer32.Create(owner: TLayeredImage32);
begin
  inherited;
  fMixedPath := TMixedPath.Create;
end;
//------------------------------------------------------------------------------

destructor TMixedPathLayer32.Destroy;
begin
 fMixedPath.Free;
 inherited;
end;
//------------------------------------------------------------------------------

procedure TMixedPathLayer32.Offset(dx, dy: integer);
var
  i: integer;
  mpPt: TPointD;
begin
  inherited Offset(dx, dy);
  //when the layer is offset, offset MixedPath too
  for i := 0 to fMixedPath.Count -1 do
  begin
    mpPt := fMixedPath.Points[i];
    mpPt.X := mpPt.X + dx;
    mpPt.Y := mpPt.Y + dy;
    fMixedPath.Points[i] := mpPt;
  end;
end;

//------------------------------------------------------------------------------
// TSmoothPathLayer32
//------------------------------------------------------------------------------

constructor TSmoothPathLayer32.Create(owner: TLayeredImage32);
begin
  inherited;
  fSmoothPath := TSmoothPath.Create;
end;
//------------------------------------------------------------------------------

destructor TSmoothPathLayer32.Destroy;
begin
 fSmoothPath.Free;
 inherited;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathLayer32.Offset(dx, dy: integer);
begin
  inherited Offset(dx, dy);
  fSmoothPath.Offset(dx, dy);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure DrawMixedPathOnDesigner(const mixedPath: TMixedPath;
  designerLayer: TDesignerLayer32);
var
  i,j,highI: integer;
  btnSize: double;
  pointType: TMixedType;
  pts: TArrayOfPointD;
  virtualPt: TPointD;
begin
  setLength(pts, 2);
  highI := mixedPath.Count -1;
  btnSize := designerLayer.ButtonSize;
  if btnSize < 2 then btnSize := DefaultButtonSize;
  virtualPt := NullPointD;

  i := 0;
  while i < highI do
  begin
    j := i + 1;
    pointType := mixedPath.PointTypes[j];
    while (j < highI) and (mixedPath.PointTypes[j+1] = pointType) do inc(j);
    case pointType of
      mtCBezier:
        while i < j -2 do
        begin
          pts[0] := mixedPath.Points[i];
          pts[1] := mixedPath.Points[i+1];
          designerLayer.DrawLine(pts);
          pts[0] := mixedPath.Points[i+2];
          pts[1] := mixedPath.Points[i+3];
          designerLayer.DrawLine(pts);
          inc(i, 3);
        end;
      mtQBezier:
        while i < j -1 do
        begin
          pts[0] := mixedPath.Points[i];
          pts[1] := mixedPath.Points[i+1];
          designerLayer.DrawLine(pts);
          pts[0] := mixedPath.Points[i+1];
          pts[1] := mixedPath.Points[i+2];
          designerLayer.DrawLine(pts);
          inc(i, 2);
        end;
      mtCSpline:
        begin
          pts[0] := mixedPath.Points[i];
          if i = 0 then
          begin
            if i > j - 3 then break;
            pts[1] := mixedPath.Points[1];
            designerLayer.DrawLine(pts);
            inc(i);
          end
          else if mixedPath.PointTypes[i] = mtQSpline then
          begin
            if i > j - 2 then break;
            pts[1] := ReflectPoint(virtualPt, pts[0]);
            designerLayer.DrawLine(pts);
            DrawButton(designerLayer.Image, pts[1], btnSize);
          end else
          begin
            if i > j - 2 then break;
            pts[1] := ReflectPoint(mixedPath.Points[i-1], pts[0]);
            designerLayer.DrawLine(pts);
            DrawButton(designerLayer.Image, pts[1], btnSize);
          end;
          pts[0] := mixedPath.Points[i+1];
          pts[1] := mixedPath.Points[i+2];
          designerLayer.DrawLine(pts);
          inc(i, 2);
          while i < j - 1 do
          begin
            pts[0] := mixedPath.Points[i];
            pts[1] := ReflectPoint(mixedPath.Points[i-1], pts[0]);
            designerLayer.DrawLine(pts);
            DrawButton(designerLayer.Image, pts[1], btnSize);
            pts[0] := mixedPath.Points[i+1];
            pts[1] := mixedPath.Points[i+2];
            designerLayer.DrawLine(pts);
            inc(i, 2);
          end;
        end;
      mtQSpline:
        begin
          pts[0] := mixedPath.Points[i];
          if i = 0 then
          begin
            pts[1] := mixedPath.Points[1];
            inc(i);
          end
          else
            pts[1] := ReflectPoint(mixedPath.Points[i-1], pts[0]);

          designerLayer.DrawLine(pts);
          DrawButton(designerLayer.Image, pts[1], btnSize);
          while i < j do
          begin
            pts[0] := mixedPath.Points[i+1];
            inc(i);
            designerLayer.DrawLine(pts);
            if i = j then break;
            pts[1] := ReflectPoint(pts[1], pts[0]);
            designerLayer.DrawLine(pts);
            DrawButton(designerLayer.Image, pts[1], btnSize);
          end;
          virtualPt := pts[1];
        end;
    end;
    i := j;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawSmoothPathOnDesigner(const smoothPath: TSmoothPath;
  designerLayer: TDesignerLayer32);
var
  i, highI: integer;
  pts: TArrayOfPointD;
begin
  SetLength(pts, 2);
  highI := smoothPath.Count -1;
  i := 0;
  while i < highI do
  begin
    pts[0] := smoothPath.Points[i];
    pts[1] := smoothPath.Points[i+1];
    designerLayer.DrawLine(pts);
    if i >= highI -2 then break;
    pts[0] := smoothPath.Points[i+2];
    pts[1] := smoothPath.Points[i+3];
    designerLayer.DrawLine(pts);
    inc(i, 3);
  end;
end;
//------------------------------------------------------------------------------

end.
