unit Image32_MixedPath;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.24                                                            *
* Date      :  18 September 2019                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Module that supports mixed-type (curved & straight) paths       *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Math, Image32, Image32_Draw, Image32_Vector;

{$I Image32.inc}

type
  TPointType = (ptStraight, ptCBezier, ptQBezier, ptCSpline, ptQSpline);

  TCtrlPoint = record
    PointType: TPointType;
    case boolean of
      false: (Point    : TPointD);
      true: (X: double; Y: double);
  end;

  //TMixedPath: structure for paths containing both curved and straight edges
  TMixedPath = class
  private
    fCapacity       : integer;
    fTotalCount     : integer;      //total control points (ie <= capacity)
    fCurrentType    : TPointType;
    fCurrentCount   : integer;      //count of current PointType
    fCtrlPoints     : array of TCtrlPoint;
    fFlattened      : TArrayOfPointD;
    function GetCtrlPoints: TArrayOfPointD;
    function GetPoint(index: integer): TCtrlPoint;
    procedure SetPoint(index: integer; const ctrlPt: TCtrlPoint);
    function GetFlattenedPath: TArrayOfPointD;
    procedure AddInternal(const pt: TPointD);
  public
    function Add(const pt: TPointD; pointType: TPointType): Boolean; overload;
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
    //deletes contiguous control points with the same TPointType
    procedure DeleteLastType;

    property Points[index: integer]: TCtrlPoint
      read GetPoint write SetPoint; default;
    property CtrlPoints: TArrayOfPointD read GetCtrlPoints;
    property CurrentType: TPointType read fCurrentType;
    property Count: integer read fTotalCount;
    property FlattenedPath: TArrayOfPointD read GetFlattenedPath;
  end;

implementation

resourcestring
  rsMixedPathRangeError = 'TMixedPath: index is out of range.';

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

function TMixedPath.GetPoint(index: integer): TCtrlPoint;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsMixedPathRangeError);
  Result := fCtrlPoints[index];
end;
//------------------------------------------------------------------------------

procedure TMixedPath.SetPoint(index: integer; const ctrlPt: TCtrlPoint);
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsMixedPathRangeError);
  fCtrlPoints[index] := ctrlPt;
end;
//------------------------------------------------------------------------------

function TMixedPath.GetFlattenedPath: TArrayOfPointD;
var
  i,j,k, len, len2, rLen: integer;
  tmp: TArrayOfPointD;
  pointType: TPointType;
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

  len := fTotalCount;
  i := 1;
  while i < len do
  begin
    rLen := Length(Result);
    pointType := fCtrlPoints[i].PointType;
    j := i + 1;
    while (j < len) and
      (fCtrlPoints[j].PointType = pointType) do inc(j);
    case pointType of
      ptStraight:
        begin
          len2 := j - i +1;
          SetLength(Result, rLen + len2);
          for k := 0 to len2 - 1 do
              Result[rLen + k] := fCtrlPoints[i -1 + k].Point;
        end;
      ptCBezier:
        begin
          if i + 3 > len then break;
          SetLength(tmp, 4);
          while i < j - 2 do
          begin
            for k := 0 to 3 do
              tmp[k] := fCtrlPoints[i -1 + k].Point;
            AddToPath(Result, CBezier(tmp));
            inc(i, 3);
          end;
        end;
      ptQBezier:
        begin
          if i + 2 > len then break;
          SetLength(tmp, 3);
          while i < j -1 do
          begin
            for k := 0 to 2 do
              tmp[k] := fCtrlPoints[i -1 + k].Point;
            AddToPath(Result, QBezier(tmp));
            inc(i, 2);
          end;
        end;
      ptCSpline:
        begin
          if i = 1 then
          begin
            len2 := j;
            if len2 < 4 then break;
            SetLength(tmp, len2);
            for k := 0 to len2 - 1 do
              tmp[k] := fCtrlPoints[k].Point;
          end else
          begin
            if j < i + 2 then break;
            len2 := j - i  + 2;
            SetLength(tmp, len2);
            tmp[0] := fCtrlPoints[i-1].Point;
            tmp[1] := ReflectPoint(fCtrlPoints[i-2].Point, tmp[0]);
            for k := 0 to len2 - 3 do
              tmp[k +2] := fCtrlPoints[i + k].Point;
          end;
          AddToPath(Result, CSpline(tmp));
        end;
      ptQSpline:
        begin
          if i = 1 then
          begin
            len2 := j;
            if len2 < 3 then break;
            SetLength(tmp, len2);
            for k := 0 to len2 - 1 do
              tmp[k] := fCtrlPoints[k].Point;
          end else
          begin
            len2 := j - i  + 2;
            SetLength(tmp, len2);
            tmp[0] := fCtrlPoints[i-1].Point;
            tmp[1] := ReflectPoint(fCtrlPoints[i-2].Point, tmp[0]);
            for k := 0 to len2 - 3 do
              tmp[k +2] := fCtrlPoints[i + k].Point;
          end;
          AddToPath(Result, QSpline(tmp));
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
      ptStraight:
        if isFirst then Result := fCurrentCount > 1
        else Result := fCurrentCount > 0;
      ptCBezier:
        if isFirst then Result := (fCurrentCount -1) mod 3 = 0
        else Result := fCurrentCount mod 3 = 0;
      ptQBezier:
        if isFirst then Result := (fCurrentCount -1) mod 2 = 0
        else Result := fCurrentCount mod 2 = 0;
      ptCSpline:
        if isFirst then Result := fCurrentCount > 3
        else Result := fCurrentCount mod 2 = 0;
      ptQSpline:
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
  //nb: the first Add() must specify a TPointType
  Result := assigned(fCtrlPoints);
  if not Result then Exit;
  AddInternal(pt);
end;
//------------------------------------------------------------------------------

function TMixedPath.Add(const pt: TPointD; pointType: TPointType): Boolean;
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
  pt: TPointType;
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

end.
