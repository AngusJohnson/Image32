unit Image32_SmoothPath;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.32                                                            *
* Date      :  4 December 2019                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Supports paths with multiple sub-curves                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Math, Image32, Image32_Layers;

{$I Image32.inc}

type
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

  //Anticipating that TSmoothPath will mostly be used inside a TLayeredImage32
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

  procedure DrawSmoothPathOnDesigner(const smoothPath: TSmoothPath;
    designerLayer: TDesignerLayer32);

implementation

uses
  Image32_Draw, Image32_Vector, Image32_Extra;

resourcestring
  rsSmoothPathRangeError = 'TSmoothPath: index is out of range.';

const
  capacityIncrement = 16;

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
    Result := JoinPaths(Result, FlattenCBezier(tmp));
    inc(j, 3);
  end;
  fFlattened := Result;
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
