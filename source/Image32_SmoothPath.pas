unit Image32_SmoothPath;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.0                                                             *
* Date      :  6 March 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  Supports paths with multiple sub-curves                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Math,
  Image32, Image32_Vector, Image32_Layers, Image32_Extra;

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
    fFlattened      : TPathD;
    fAutoAdjust     : Boolean;
    fOnChange       : TNotifyEvent;
    function GetPoint(index: integer): TPointD;
    procedure SetPoint(index: integer; const pt: TPointD);
    function GetPointType(index: integer): TSmoothType;
    procedure SetPointType(index: integer; const newType: TSmoothType);
    function GetLastType: TSmoothType;

    function GetFlattenedPath: TPathD;
    procedure AddInternal(const pt: TPointD; pointType: TSmoothType);
  protected
    procedure MovePoint(index: integer; const newPt: TPointD);
    procedure Changed; virtual;
  public
    constructor Create; virtual;
    procedure Add(const pt: TPointD; pointType: TSmoothType); overload;
    procedure Add(const pt: TPointD); overload;
    procedure Assign(mixedPath: TSmoothPath);
    procedure AssignTo(mixedPath: TSmoothPath);
    procedure Clear;

    function  GetPoints: TPathD;
    function  GetBounds: TRect;
    procedure DeleteLast; //DeleteLast: deletes the last curve
    procedure Offset(dx, dy: integer);
    procedure Rotate(const focalPoint: TPointD; angleRads: double);
    procedure Scale(sx, sy: double);

    //property AutoAdjust: Boolean read fAutoAdjust write fAutoAdjust;
    property LastType: TSmoothType read GetLastType;
    property Points[index: integer]: TPointD
      read GetPoint write SetPoint; Default;
    property PointTypes[index: integer]: TSmoothType
      read GetPointType write SetPointType;
    property Count: integer read fCount;
    property FlattenedPath: TPathD read GetFlattenedPath;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TSmoothButtonLayer32 = class(TButtonDesignerLayer32)
  private
    fPathIdx : integer;
  public
    destructor Destroy; override;
    property PathIdx: integer read fPathIdx write fPathIdx;
  end;

  //Anticipating that TSmoothPath will mostly be used inside a TLayeredImage32
  TSmoothPathGroupLayer32 = class(TGroupLayer32)
  private
    fSmoothPath: TSmoothPath;
    fVectorLayer32: TVectorLayer32;
    fDesignLayer32: TDesignerLayer32;

    fPenColor       : TColor32;
    fPenWidth       : double;
    fBrushColor     : TColor32;
    fFillRule       : TFillRule;
    fIsClosePath    : Boolean;

    fButtonSize1    : integer;
    fButtonSize2    : integer;
    fColorFirstBtn  : TColor32;
    fColorLastBtn   : TColor32;
    fColorMiddleBtn : TColor32;
    fColorCtrlBtn   : TColor32;

    fDesignMargin   : integer;
    fActiveButton   : TSmoothButtonLayer32;
    procedure SetActiveButton(activeButton: TSmoothButtonLayer32);
    function UpdateButtonsAndCalcBounds: TRect;
  protected
    procedure PaintSmoothPathLayer(layer: TVectorLayer32); virtual;
    procedure PaintDesignerLayer(layer: TDesignerLayer32); virtual;
    procedure DoOnMerge; override;
  public
    constructor Create(groupOwner: TGroupLayer32;
      const name: string = ''); override;
    destructor Destroy; override;
    procedure Offset(dx, dy: integer); override;
    property SmoothPath: TSmoothPath read fSmoothPath;

    property ButtonSize1: integer read fButtonSize1 write fButtonSize1;
    property ButtonSize2: integer read fButtonSize2 write fButtonSize2;
    property ColorFirstBtn: TColor32 read fColorFirstBtn write fColorFirstBtn;
    property ColorLastBtn: TColor32 read fColorLastBtn write fColorLastBtn;
    property ColorMiddleBtn: TColor32 read fColorMiddleBtn write fColorMiddleBtn;
    property ColorCtrlBtn : TColor32 read fColorCtrlBtn write fColorCtrlBtn;

    property PenColor: TColor32 read fPenColor write fPenColor;
    property PenWidth: double read fPenWidth write fPenWidth;
    property BrushColor: TColor32 read fBrushColor write fBrushColor;
    property FillRule: TFillRule read fFillRule write fFillRule;
    property IsClosePath: Boolean read fIsClosePath write fIsClosePath;

    property DesignLayer: TDesignerLayer32 read fDesignLayer32;
    property VectorLayer: TVectorLayer32 read fVectorLayer32;
    property ActiveButtonLayer: TSmoothButtonLayer32
      read fActiveButton write SetActiveButton;
    property DesignMargin: integer read fDesignMargin write fDesignMargin;
  end;

var
  defaultSmoothBtnColor1: TColor32 = $FF0088FF;
  defaultSmoothBtnColor2: TColor32 = clRed32;

implementation

uses
  Image32_Draw;

resourcestring
  rsSmoothPath = 'SmoothPath';
  rsSmoothPathRangeError =
    'TSmoothPath: index is out of range.';
  rsSmoothPathGroupLayerError =
    'TSmoothPathGroupLayer32: invalid button layer type';

const
  capacityIncrement = 16;

//------------------------------------------------------------------------------
// TNotifySmoothPath
//------------------------------------------------------------------------------

type

  TNotifySmoothPath = class(TSmoothPath)
  private
    fGroupLayerOwner: TSmoothPathGroupLayer32;
  protected
    procedure Changed; override;
  end;

procedure TNotifySmoothPath.Changed;
begin
  fGroupLayerOwner.ForceRefresh;
  inherited;
end;

//------------------------------------------------------------------------------
// TSmoothButtonLayer32
//------------------------------------------------------------------------------

destructor TSmoothButtonLayer32.Destroy;
var
  i: integer;
begin
  if fPathIdx >= 0 then
  begin
    //this button is being deleted directly (not via SmoothPath)
    i := PathIdx;
    with TSmoothPathGroupLayer32(GroupOwner) do
      while SmoothPath.Count > i do SmoothPath.DeleteLast;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
// TSmoothPath
//------------------------------------------------------------------------------

constructor TSmoothPath.Create;
begin
  fAutoAdjust := true;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Changed;
begin
  if Assigned(fOnChange) then fOnChange(Self);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Clear;
begin
  if fCount = 0 then Exit;
  fCtrlPoints := nil;
  fFlattened := nil;
  fCapacity := 0;
  fCount := 0;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Assign(mixedPath: TSmoothPath);
begin
  mixedPath.AssignTo(self);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.AssignTo(mixedPath: TSmoothPath);
begin
  if not assigned(mixedPath) or (mixedPath = Self) then Exit;
  mixedPath.fCapacity := self.fCapacity;
  mixedPath.fCount := self.fCount;
  mixedPath.fCtrlPoints := Copy(self.fCtrlPoints, 0, self.fCapacity);
  mixedPath.fFlattened := nil;
  Changed;
end;
//------------------------------------------------------------------------------

function TSmoothPath.GetPoints: TPathD;
var
  i, cnt: integer;
begin
  cnt := Count;
  setLength(Result, cnt);
  for i := 0 to cnt - 1 do
    Result[i] := fCtrlPoints[i].Point;
end;
//------------------------------------------------------------------------------

function  TSmoothPath.GetBounds: TRect;
begin
  Result := Image32_Vector.GetBounds(GetPoints);
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
    raise Exception.Create(rsSmoothPathRangeError)
  else if PointsEqual(fCtrlPoints[index].Point, pt) then Exit;

  if fAutoAdjust then
  begin
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
    end;
  end
  else
    fCtrlPoints[index].Point := pt;

  Changed;
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
  //only assign TSmoothType to on-path 'nodes', not off-path 'handles'
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
  else if (newType = stSharpNoHdls) and (index = Count - 2) then DeleteLast
  else Changed;
end;
//------------------------------------------------------------------------------

function TSmoothPath.GetLastType: TSmoothType;
begin
  if (fCount = 0) then
    Result := stSmoothSym else
    result := PointTypes[fCount -1];
end;
//------------------------------------------------------------------------------

function TSmoothPath.GetFlattenedPath: TPathD;
var
  i,j,k, cnt: integer;
  tmp: TPathD;
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
  if (fCount = 0) or not fAutoAdjust then
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
  Changed;
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
  if fCount > 1 then
  begin
    if fCount < 4 then fCount := 1
    else dec(fCount, (fCount -1) mod 3);
  end;
  Changed;
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
  if (dx = 0) and (dy = 0) then Exit;
  for i := 0 to Count -1 do
  begin
    fCtrlPoints[i].X := fCtrlPoints[i].X + dx;
    fCtrlPoints[i].Y := fCtrlPoints[i].Y + dy;
  end;
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Rotate(const focalPoint: TPointD; angleRads: double);
var
  i: integer;
  sinA, cosA: extended;
begin
  if angleRads = 0.0 then Exit;

  Math.SinCos(angleRads, sinA, cosA);
  for i := 0 to Count -1 do
    RotatePoint(fCtrlPoints[i].Point, focalPoint, sinA, cosA);
  Changed;
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Scale(sx, sy: double);
var
  i: integer;
begin
  if (sx = 1.0) and (sy = 1.0) then Exit;
  for i := 0 to Count -1 do
  begin
    fCtrlPoints[i].X := fCtrlPoints[i].X * sx;
    fCtrlPoints[i].Y := fCtrlPoints[i].Y * sy;
  end;
  Changed;
end;

//------------------------------------------------------------------------------
// TSmoothPathGroupLayer32
//------------------------------------------------------------------------------

constructor TSmoothPathGroupLayer32.Create(groupOwner: TGroupLayer32; const name: string);
begin
  inherited;
  if Self.Name = '' then
    Self.Name := rsSmoothPath;

  fSmoothPath     := TNotifySmoothPath.Create;
  TNotifySmoothPath(fSmoothPath).fGroupLayerOwner := Self;

  //initially this group consists of 2 layers ...
  //a TVectorLayer32 where the smoothpath is drawn
  //and a TDesignerLayer32 where designer lines etc are drawn.
  fVectorLayer32  := TVectorLayer32(AddChild(TVectorLayer32));
  fDesignLayer32  := TDesignerLayer32(AddChild(TDesignerLayer32));

  //button layers will be added when points are added to SmoothPath.

  fColorFirstBtn  := defaultSmoothBtnColor1;
  fColorLastBtn   := defaultSmoothBtnColor1;
  fColorMiddleBtn := defaultSmoothBtnColor1;
  fColorCtrlBtn   := defaultSmoothBtnColor2;

  fButtonSize1     := DefaultButtonSize + DpiAware(1);
  fButtonSize2     := DefaultButtonSize;
  fPenColor       := clBlack32;
  fPenWidth       := DPIAware(3);
  fBrushColor     := clWhite32;
  fIsClosePath    := false;
end;
//------------------------------------------------------------------------------

destructor TSmoothPathGroupLayer32.Destroy;
var
  i: integer;
begin
  for i := 2 to ChildCount -1 do
    TSmoothButtonLayer32(Child[i]).fPathIdx := -1; //flag for safe disposal.
 fSmoothPath.Free;
 inherited;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.Offset(dx, dy: integer);
begin
  fSmoothPath.Offset(dx, dy);
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.SetActiveButton(activeButton: TSmoothButtonLayer32);
begin
  if (fActiveButton = activeButton) or
    (Assigned(activeButton) and (activeButton.GroupOwner <> self)) then Exit;
  fActiveButton := activeButton;
  if Assigned(activeButton) then Invalidate(activeButton.Bounds);
end;
//------------------------------------------------------------------------------

function TSmoothPathGroupLayer32.UpdateButtonsAndCalcBounds: TRect;

  procedure UpdateAttribs(btnLayer: TSmoothButtonLayer32);
  var
    i: integer;
  begin
    with btnLayer do
    begin
      i := fPathIdx mod 3;
      case i of
        0: Visible := true;
        1: Visible := SmoothPath.PointTypes[fPathIdx -1] <> stSharpNoHdls;
        2: Visible := (fPathIdx = SmoothPath.Count -1) or
          (SmoothPath.PointTypes[fPathIdx +1] <> stSharpNoHdls);
      end;
      PositionCenteredAt(self.SmoothPath[fPathIdx]);
    end;
  end;

  procedure SetAttribs(btnLayer: TSmoothButtonLayer32);
  var
    i: integer;
  begin
    with btnLayer do
    begin
      CursorId := crSizeAll;
      if PathIdx mod 3 = 0 then
      begin
        Shape := bsDiamond;
        Size := fButtonSize1;
        if PathIdx = 0 then
          Color := fColorFirstBtn
        else if PathIdx = SmoothPath.Count -1 then
          Color := fColorLastBtn
        else
          Color := fColorMiddleBtn;
      end else
      begin
        Shape := bsRound;
        Size := fButtonSize2;
        Color := fColorCtrlBtn;
      end;
      i := Round(Size * 2.25);
      Image.SetSize(i, i);
      Draw;
    end;
    UpdateAttribs(btnLayer);
  end;

var
  i,j, pathCnt, btnCnt: integer;
  btnLayer: TSmoothButtonLayer32;
begin
  pathCnt := SmoothPath.Count;
  btnCnt := ChildCount -2;

  //?? delete active
  if Assigned(fActiveButton) and (fActiveButton.fPathIdx >= pathCnt) then
    fActiveButton := nil;

  //update color of last button
  if (btnCnt > 1) and ((btnCnt -1) mod 3 = 0) and (pathCnt > btnCnt) then
    with TSmoothButtonLayer32(Child[btnCnt]) do
    begin
      Color := fColorMiddleBtn;
      Draw;
    end;

  //remove obsolete buttons
  while btnCnt > pathCnt do
  begin
    dec(btnCnt);
    TSmoothButtonLayer32(Child[btnCnt +2]).PathIdx := -1;
    DeleteChild(btnCnt +2);
  end;

  //precondition: there will always be at least one button


  for i := 0 to pathCnt -1 do
  begin
    //control buttons should always be on top of path buttons
    case i mod 3 of
      0: if (i = 0) then j := 0 else j := i -1;
      2: if (i = pathCnt -1) then j := i else j := i+1;
      else j := i;
    end;

    if j >= btnCnt then       //either add new buttons
    begin
      btnLayer := TSmoothButtonLayer32(AddChild(TSmoothButtonLayer32));
      btnLayer.fPathIdx := j;
      SetAttribs(btnLayer);
    end else
    begin                     //or update old ones
      btnLayer := TSmoothButtonLayer32(Child[i +2]);
      UpdateAttribs(btnLayer);
    end;

    if i = 0 then
      Result := btnLayer.Bounds else
      Result := Image32_Vector.UnionRect(Result, btnLayer.Bounds);
  end;

end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.PaintSmoothPathLayer(layer: TVectorLayer32);
var
  flattenedPath: TPathD;
begin
  layer.Image.Clear;
  flattenedPath := fSmoothPath.GetFlattenedPath;
  flattenedPath := OffsetPath(flattenedPath, -layer.Left, -layer.Top);
  if IsClosePath then
  begin
    DrawPolygon(layer.Image, flattenedPath, fFillRule, fBrushColor);
    DrawLine(layer.Image, flattenedPath, fPenWidth, fPenColor, esPolygon);
  end else
    DrawLine(layer.Image, flattenedPath, fPenWidth, fPenColor, esRound);
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.PaintDesignerLayer(layer: TDesignerLayer32);
var
  i, dashLen, pathLen, dx,dy: integer;
  rec: TRect;
  tmpPath, ctrlLine: TPathD;
begin
  dashLen := DPIAware(2);

  layer.Image.Clear;
  if Assigned(fActiveButton) then
  begin
    rec := fActiveButton.Bounds;
    OffsetRect(rec, -layer.Left, -layer.Top);
    tmpPath := Ellipse(rec);
    DrawDashedLine(layer.Image, tmpPath,
      [dashLen,dashLen*2], nil, DPIAware(1), clRed32, esPolygon);
  end;

  SetLength(ctrlLine, 2);
  i := 0;
  dx := layer.Left;
  dy := layer.Top;

  pathLen := SmoothPath.Count -1;
  while i < pathLen -1 do
  begin
    if smoothPath.PointTypes[i] <> stSharpNoHdls then
    begin
      ctrlLine[0] := smoothPath[i];
      ctrlLine[1] := smoothPath[i+1];
      ctrlLine := OffsetPath(ctrlLine, -dx, -dy);
      DrawDashedLine(layer.Image, ctrlLine,
        [dashLen,dashLen*2], nil, DPIAware(1), clRed32, esSquare);
    end;

    if i >= pathLen -1 then break;
    if smoothPath.PointTypes[i+3] <> stSharpNoHdls then
    begin
      ctrlLine[0] := smoothPath[i+2];
      ctrlLine[1] := smoothPath[i+3];
      ctrlLine := OffsetPath(ctrlLine, -dx, -dy);
      DrawDashedLine(layer.Image, ctrlLine,
        [dashLen,dashLen*2], nil, DPIAware(1), clRed32, esSquare);
    end;
    inc(i, 3);
  end;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.DoOnMerge;
var
  idx, margin, pathLen: integer;
  rec: TRect;
begin
  if (ChildCount < 2) or
    not (Child[0] is TVectorLayer32) or
    not (Child[1] is TDesignerLayer32) then
      raise Exception.Create(rsSmoothPathGroupLayerError);

  pathLen := fSmoothPath.Count;
  idx := ChildCount -1;
  while idx >= pathLen + 2 do
  begin
    // flag safe button removal
    TSmoothButtonLayer32(Child[idx]).fPathIdx := -1;
    DeleteChild(idx);
    Dec(idx);
  end;

  if pathLen = 0 then
  begin
    Child[0].SetSize(0,0);
    Child[1].SetSize(0,0);
    inherited;
    Exit;
  end;

  if assigned(fActiveButton) and
    (fActiveButton.Index >= ChildCount) then
      fActiveButton := nil;

  rec := UpdateButtonsAndCalcBounds;

  margin := Max(fButtonSize1, fButtonSize2) + Round(PenWidth);
  rec := InflateRect(rec, margin, margin);

  Child[0].SetBounds(rec);

  if fDesignMargin > 0 then
    rec := Image32_Vector.InflateRect(rec, fDesignMargin, fDesignMargin);
  Child[1].SetBounds(rec);

  PaintSmoothPathLayer(TVectorLayer32(Child[0]));
  PaintDesignerLayer(TDesignerLayer32(Child[1]));

  inherited;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
