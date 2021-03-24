unit Image32_SmoothPath;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.19                                                            *
* Date      :  21 March 2021                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  Supports paths with multiple sub-curves                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Math, Image32, Image32_Vector, Image32_Layers;

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
    fUpdateCount    : integer;
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
    function CanUpdate: Boolean;
  public
    constructor Create; virtual;
    procedure Add(const pt: TPointD; pointType: TSmoothType); overload;
    procedure Add(const pt: TPointD); overload;
    procedure Assign(mixedPath: TSmoothPath);
    procedure AssignTo(mixedPath: TSmoothPath);
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;

    function  GetPoints: TPathD;
    function  GetBounds: TRect;
    procedure DeleteLast; //DeleteLast: deletes the last curve
    procedure Offset(dx, dy: double);
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
    fIsClosedPath   : Boolean;

    fButtonSize1    : integer;
    fButtonSize2    : integer;
    fColorFirstBtn  : TColor32;
    fColorLastBtn   : TColor32;
    fColorMiddleBtn : TColor32;
    fColorCtrlBtn   : TColor32;
    fColorActiveBtn : TColor32;

    fDesignMargin   : integer;
    fActiveButton   : TSmoothButtonLayer32;
    procedure SetActiveButton(activeButton: TSmoothButtonLayer32);
    function UpdateButtonsAndCalcBounds: TRect;
    function GetCursorId: integer;
    procedure SetCursorId(newCursorId: integer);
    procedure SetPenWidth(val: double);
    procedure VectorLayerDraw(Sender: TObject);
  protected
    procedure SmoothPathUpdated; virtual;
    procedure PaintVectorLayer; virtual;
    procedure PaintDesignerLayer; virtual;
  public
    constructor Create(grpOwner: TGroupLayer32;
      const aName: string = ''); override;
    destructor Destroy; override;
    procedure Offset(dx, dy: double); override;
    function GroupIdxToPathIdx(groupIdx: integer): integer;

    property SmoothPath: TSmoothPath read fSmoothPath;

    property ButtonSize1: integer read fButtonSize1 write fButtonSize1;
    property ButtonSize2: integer read fButtonSize2 write fButtonSize2;
    property ColorFirstBtn: TColor32 read fColorFirstBtn write fColorFirstBtn;
    property ColorLastBtn: TColor32 read fColorLastBtn write fColorLastBtn;
    property ColorMiddleBtns: TColor32 read fColorMiddleBtn write fColorMiddleBtn;
    property ColorCtrlBtns : TColor32 read fColorCtrlBtn write fColorCtrlBtn;
    property ColorActiveBtn : TColor32 read fColorActiveBtn write fColorActiveBtn;

    property PenColor: TColor32 read fPenColor write fPenColor;
    property PenWidth: double read fPenWidth write SetPenWidth;
    property BrushColor: TColor32 read fBrushColor write fBrushColor;
    property FillRule: TFillRule read fFillRule write fFillRule;
    property IsClosedPath: Boolean read fIsClosedPath write fIsClosedPath;
    property CursorId : integer read GetCursorId write SetCursorId;

    property DesignLayer: TDesignerLayer32 read fDesignLayer32;
    property VectorLayer: TVectorLayer32 read fVectorLayer32;
    property ActiveButtonLayer: TSmoothButtonLayer32
      read fActiveButton write SetActiveButton;
    property DesignMargin: integer read fDesignMargin write fDesignMargin;
  end;

  //SmoothToBezier: this function is based on -
  //"An Algorithm for Automatically Fitting Digitized Curves"
  //by Philip J. Schneider in "Graphics Gems", Academic Press, 1990
  function SmoothToBezier(const path: TPathD; closed: Boolean;
    tolerance: double; minSegLength: double = 2): TPathD; overload;
  function SmoothToBezier(const paths: TPathsD; closed: Boolean;
    tolerance: double; minSegLength: double = 2): TPathsD; overload;

var
  defaultSmoothBtnColor1: TColor32 = $FF0088FF;
  defaultSmoothBtnColor2: TColor32 = clRed32;

implementation

uses
  Image32_Extra, Image32_Draw;

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
  if not CanUpdate then Exit;
  fGroupLayerOwner.SmoothPathUpdated;
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

function TSmoothPath.CanUpdate: Boolean;
begin
  Result := (fUpdateCount = 0);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.Changed;
begin
  if CanUpdate and Assigned(fOnChange) then
    fOnChange(Self);
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

procedure TSmoothPath.BeginUpdate;
begin
  inc(fUpdateCount);
end;
//------------------------------------------------------------------------------

procedure TSmoothPath.EndUpdate;
begin
  dec(fUpdateCount);
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
  mixedPath.Changed;
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

  if PointsEqual(pt, fCtrlPoints[index].Point) then Exit;

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

  procedure OffsetPt(idx: integer; dx, dy: double);
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
        if index > 0 then OffsetPt(index -1, dx, dy);
        if index < Count -1 then OffsetPt(index +1, dx, dy);
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

procedure TSmoothPath.Offset(dx, dy: double);
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
  sinA, cosA: double;
begin
  NormalizeAngle(angleRads);
  if angleRads = 0.0 then Exit;
  if not ClockwiseRotationIsAnglePositive then angleRads := -angleRads;
  GetSinCos(angleRads, sinA, cosA);
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

constructor TSmoothPathGroupLayer32.Create(grpOwner: TGroupLayer32; const aName: string);
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
  fVectorLayer32.OnDraw := Self.VectorLayerDraw;


  fDesignLayer32  := TDesignerLayer32(AddChild(TDesignerLayer32));

  //button layers will be added when points are added to SmoothPath.

  fColorFirstBtn  := defaultSmoothBtnColor1;
  fColorLastBtn   := defaultSmoothBtnColor1;
  fColorMiddleBtn := defaultSmoothBtnColor1;
  fColorCtrlBtn   := defaultSmoothBtnColor2;
  fColorActiveBtn := clLime32;

  fButtonSize1    := DefaultButtonSize + DpiAware(1);
  fButtonSize2    := DefaultButtonSize;
  fPenColor       := clBlack32;
  fPenWidth       := DPIAware(3);
  fBrushColor     := clWhite32;
  fIsClosedPath   := false;
end;
//------------------------------------------------------------------------------

destructor TSmoothPathGroupLayer32.Destroy;
var
  i: integer;
begin
  //set flag for safe disposal ...
  for i := 2 to ChildCount -1 do
    TSmoothButtonLayer32(Child[i]).fPathIdx := -1;

 fSmoothPath.Free;
 inherited;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.Offset(dx, dy: double);
begin
  fSmoothPath.Offset(dx, dy);
end;
//------------------------------------------------------------------------------

function TSmoothPathGroupLayer32.GroupIdxToPathIdx(groupIdx: integer): integer;
begin
  if not (Child[groupIdx] is TSmoothButtonLayer32) then Result := -1
  else Result := TSmoothButtonLayer32(Child[groupIdx]).fPathIdx;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.SetActiveButton(activeButton: TSmoothButtonLayer32);
begin
  if (fActiveButton = activeButton) or
    (Assigned(activeButton) and (activeButton.GroupOwner <> self)) then Exit;
  fActiveButton := activeButton;
  if Assigned(activeButton) then Invalidate(activeButton.Bounds);
  PaintDesignerLayer;
end;
//------------------------------------------------------------------------------

function TSmoothPathGroupLayer32.GetCursorId: integer;
begin
  Result := fVectorLayer32.CursorId;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.SetCursorId(newCursorId: integer);
begin
  fVectorLayer32.CursorId := newCursorId;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.SetPenWidth(val: double);
begin
  val := Max(0.5, val);
  if (fPenWidth = val)  then Exit;
  fPenWidth := val;
  fVectorLayer32.Margin := ceil(val/2);
end;
//------------------------------------------------------------------------------

function TSmoothPathGroupLayer32.UpdateButtonsAndCalcBounds: TRect;

  procedure SetBtnVisibilityAndPosition(btnLayer: TSmoothButtonLayer32);
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

  procedure SetBtnColor(btnLayer: TSmoothButtonLayer32);
  begin
    with btnLayer do
    begin
      if btnLayer = fActiveButton then Color := fColorActiveBtn
      else if PathIdx mod 3 > 0 then Color := fColorCtrlBtn
      else if PathIdx = 0  then Color := fColorFirstBtn
      else if PathIdx = SmoothPath.Count -1 then Color := fColorLastBtn
      else Color := fColorMiddleBtn;

      if Visible then btnLayer.Draw;
    end;
  end;

  procedure SetAttributes(btnLayer: TSmoothButtonLayer32);
  begin
    with btnLayer do
    begin
      CursorId := crSizeAll;
      if PathIdx mod 3 = 0 then
      begin
        Shape := bsDiamond;
        Size := fButtonSize1;
      end else
      begin
        Shape := bsRound;
        Size := fButtonSize2;
      end;
      SetButtonAttributes(Shape, Size, Color);
    end;
  end;

var
  i,j, pathCnt, btnCnt: integer;
  btnLayer: TSmoothButtonLayer32;
begin
  //precondition: there will always be at least one button

  pathCnt := SmoothPath.Count;
  btnCnt := ChildCount -2;

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

  for i := 0 to pathCnt -1 do
  begin
    //control buttons should always be on top of path buttons
    case i mod 3 of
      0: if (i = 0) then j := 0 else j := i -1;
      2: if (i = pathCnt -1) then j := i else j := i+1;
      else j := i;
    end;

    if j >= btnCnt then       //adding a new button
    begin
      btnLayer := TSmoothButtonLayer32(AddChild(TSmoothButtonLayer32));
      btnLayer.fPathIdx := j;
      SetAttributes(btnLayer);
    end else
      btnLayer := TSmoothButtonLayer32(Child[i +2]);

    SetBtnVisibilityAndPosition(btnLayer);
    SetBtnColor(btnLayer);

    //update group bounds
    if i = 0 then
      Result := btnLayer.Bounds else
      Result := Image32_Vector.UnionRect(Result, btnLayer.Bounds);
  end;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.VectorLayerDraw(Sender: TObject);
begin
  PaintVectorLayer;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.PaintVectorLayer;
var
  pp: TPathsD;
begin
  with fVectorLayer32 do
  begin
    Image.Clear;

    pp := OffsetPath(Paths, -Left, -Top);
    if fIsClosedPath then
    begin
      DrawPolygon(Image, pp, fFillRule, fBrushColor);
      DrawLine(Image, pp, fPenWidth, fPenColor, esPolygon);
      UpdateHitTestMask(pp, frEvenOdd);
    end else
      DrawLine(Image, pp, fPenWidth, fPenColor, esRound);
  end;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.PaintDesignerLayer;
var
  i, pathLen, dx,dy: integer;
  rec: TRectD;
  tmpPath, ctrlLine: TPathD;
begin
  with DesignLayer do
  begin
    Image.Clear;
    dx := Left;
    dy := Top;

    //if ActiveButton is assigned, then give it a dashed outline
    if Assigned(Self.ActiveButtonLayer) then
    begin
      with ActiveButtonLayer do
      begin
        rec := RectD(Bounds);
        rec := InflateRect(rec, Size/2, Size/2);
      end;
      OffsetRect(rec, -dx, -dy);
      tmpPath := Ellipse(rec);
      DrawDashedLine(Image, tmpPath,
        dashes, nil, DPIAware(1), clRed32, esPolygon);
    end;

    SetLength(ctrlLine, 2);
    i := 0;

    //draw dashed lines between on-path 'nodes' and off-path 'controls'
    pathLen := SmoothPath.Count -1;
    while i < pathLen -1 do
    begin
      if smoothPath.PointTypes[i] <> stSharpNoHdls then
      begin
        ctrlLine[0] := smoothPath[i];
        ctrlLine[1] := smoothPath[i+1];
        ctrlLine := OffsetPath(ctrlLine, -dx, -dy);
        DrawDashedLine(Image, ctrlLine,
          dashes, nil, DPIAware(1), clRed32, esSquare);
      end;

      if i >= pathLen -1 then break;
      if smoothPath.PointTypes[i+3] <> stSharpNoHdls then
      begin
        ctrlLine[0] := smoothPath[i+2];
        ctrlLine[1] := smoothPath[i+3];
        ctrlLine := OffsetPath(ctrlLine, -dx, -dy);
        DrawDashedLine(Image, ctrlLine,
          dashes, nil, DPIAware(1), clRed32, esSquare);
      end;
      inc(i, 3);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TSmoothPathGroupLayer32.SmoothPathUpdated;
var
  idx, margin, pathLen: integer;
  rec: TRect;
begin
  if (ChildCount < 2) or
    not (Child[0] is TVectorLayer32) or
    not (Child[1] is TDesignerLayer32) then
      raise Exception.Create(rsSmoothPathGroupLayerError);

  pathLen := fSmoothPath.Count;

  if assigned(fActiveButton) and
    (fActiveButton.PathIdx >= pathLen) then
      fActiveButton := nil;

  idx := ChildCount -1;
  while idx >= pathLen + 2 do
  begin
    // set a flag for safe button removal (ie avoids
    // trying to delete a non-existant point in SmoothPath)
    TSmoothButtonLayer32(Child[idx]).fPathIdx := -1;
    DeleteChild(idx);
    Dec(idx);
  end;

  if pathLen = 0 then
  begin
    VectorLayer.SetSize(0,0);
    DesignLayer.SetSize(0,0);
    Exit;
  end;

  margin := Max(fButtonSize1, fButtonSize2) * 2; // *2 for active btn outline
  margin := Max(Max(Margin, Ceil(PenWidth/2)), DesignMargin);

  BeginUpdate;
  try
    rec := UpdateButtonsAndCalcBounds;
    rec := InflateRect(rec, margin, margin);

    VectorLayer.Margin := margin;
    //nb: assigning VectorLayer.Paths will adjust Bounds automatically
    VectorLayer.Paths := Image32_Vector.Paths(SmoothPath.FlattenedPath);
    DesignLayer.SetBounds(rec);
    PaintDesignerLayer;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
// SmoothToBezier() support structures and functions
//------------------------------------------------------------------------------

type
  PPt = ^TPt;
  TPt = record
    pt   : TPointD;
    vec  : TPointD;
    len  : double;
    next : PPt;
    prev : PPt;
  end;

  TFitCurveContainer = class
  private
    ppts      : PPt;
    solution  : TPathD;
    tolSqrd   : double;
    function Count(first, last: PPt): integer;
    function AddPt(const pt: TPointD): PPt;
    procedure Clear;
    function ComputeLeftTangent(p: PPt): TPointD;
    function ComputeRightTangent(p: PPt): TPointD;
    function ComputeCenterTangent(p: PPt): TPointD;
    function ChordLengthParameterize(
      first: PPt; cnt: integer): TArrayOfDouble;
    function GenerateBezier(first, last: PPt; cnt: integer;
      const u: TArrayOfDouble; const firstTan, lastTan: TPointD): TPathD;
    function Reparameterize(first: PPt; cnt: integer;
      const u: TArrayOfDouble; const bezier: TPathD): TArrayOfDouble;
    function NewtonRaphsonRootFind(const q: TPathD;
      const pt: TPointD; u: double): double;
    function ComputeMaxErrorSqrd(first, last: PPt;
      const bezier: TPathD; const u: TArrayOfDouble;
      out SplitPoint: PPt): double;
    function FitCubic(first, last: PPt;
      firstTan, lastTan: TPointD): Boolean;
    procedure AppendSolution(const bezier: TPathD);
  public
    function FitCurve(const path: TPathD; closed: Boolean;
      tolerance: double; minSegLength: double): TPathD;
  end;

//------------------------------------------------------------------------------

function Scale(const vec: TPointD; newLen: double): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec.X * newLen;
  Result.Y := vec.Y * newLen;
end;
//------------------------------------------------------------------------------

function Mul(const vec: TPointD; val: double): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec.X * val;
  Result.Y := vec.Y * val;
end;
//------------------------------------------------------------------------------

function AddVecs(const vec1, vec2: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec1.X + vec2.X;
  Result.Y := vec1.Y + vec2.Y;
end;
//------------------------------------------------------------------------------

function SubVecs(const vec1, vec2: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec1.X - vec2.X;
  Result.Y := vec1.Y - vec2.Y;
end;
//------------------------------------------------------------------------------

function DotProdVecs(const vec1, vec2: TPointD): double;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := (vec1.X * vec2.X + vec1.Y * vec2.Y);
end;
//---------------------------------------------------------------------------

function NormalizeVec(const vec: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  len: double;
begin
  len := Sqrt(vec.X * vec.X + vec.Y * vec.Y);
  if len <> 0 then
  begin
    Result.X := vec.X / len;
    Result.Y := vec.Y / len;
  end else
    result := vec;
end;
//------------------------------------------------------------------------------

function NormalizeTPt(const pt: PPt): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  with pt^ do
    if len <> 0 then
    begin
      Result.X := vec.X / len;
      Result.Y := vec.Y / len;
    end else
      result := vec;
end;
//------------------------------------------------------------------------------

function NegateVec(vec: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := -vec.X;
  Result.Y := -vec.Y;
end;
//------------------------------------------------------------------------------

function B0(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
var
  tmp: double;
begin
  tmp := 1.0 - u;
  result := tmp * tmp * tmp;
end;
//------------------------------------------------------------------------------

function B1(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
var
  tmp: double;
begin
  tmp := 1.0 - u;
  result := 3 * u * tmp * tmp;
end;
//------------------------------------------------------------------------------

function B2(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := 3 * u * u * (1.0 - u);
end;
//------------------------------------------------------------------------------

function B3(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := u * u * u;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.AddPt(const pt: TPointD): PPt;
begin
  new(Result);
  Result.pt := pt;
  if not assigned(ppts) then
  begin
    Result.prev := Result;
    Result.next := Result;
    ppts := Result;
  end else
  begin
    Result.prev := ppts.prev;
    ppts.prev.next := Result;
    ppts.prev := Result;
    Result.next := ppts;
  end;
end;
//------------------------------------------------------------------------------

procedure TFitCurveContainer.Clear;
var
  p: PPt;
begin
  solution := nil;
  ppts.prev.next := nil; //break loop
  while assigned(ppts) do
  begin
    p := ppts;
    ppts := ppts.next;
    Dispose(p);
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.Count(first, last: PPt): integer;
begin
  if first = last then
    result := 0 else
    result := 1;
  repeat
    inc(Result);
    first := first.next;
  until (first = last);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeLeftTangent(p: PPt): TPointD;
begin
  Result := NormalizeTPt(p);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeRightTangent(p: PPt): TPointD;
begin
  Result := NegateVec(NormalizeTPt(p.prev));
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeCenterTangent(p: PPt): TPointD;
var
  v1, v2: TPointD;
begin
  v1 := SubVecs(p.pt, p.prev.pt);
  v2 := SubVecs(p.next.pt, p.pt);
  Result := AddVecs(v1, v2);
  Result := NormalizeVec(Result);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ChordLengthParameterize(
  first: PPt; cnt: integer): TArrayOfDouble;
var
  d: double;
  i: integer;
begin
  SetLength(Result, cnt);
  Result[0] := 0;
  d := 0;
  for i := 1 to cnt -1 do
  begin
    d := d + first.len;
    Result[i] := d;
    first := first.next;
  end;
  for i := 1 to cnt -1 do
    Result[i] := Result[i] / d;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.GenerateBezier(first, last: PPt; cnt: integer;
  const u: TArrayOfDouble; const firstTan, lastTan: TPointD): TPathD;
var
  i: integer;
  p: PPt;
  dist, epsilon: double;
  v1,v2, tmp: TPointD;
  a0, a1: TPathD;
  c: array [0..1, 0..1] of double;
  x: array [0..1] of double;
  det_c0_c1, det_c0_x, det_x_c1, alphaL, alphaR: double;
begin
  SetLength(a0, cnt);
  SetLength(a1, cnt);
  dist := Distance(first.pt, last.pt);

  for i := 0 to cnt -1 do
  begin
		v1 := Scale(firstTan, B1(u[i]));
		v2 := Scale(lastTan, B2(u[i]));
		a0[i] := v1;
		a1[i] := v2;
  end;

  FillChar(c[0][0], 4 * SizeOf(double), 0);
  FillChar(x[0], 2 * SizeOf(double), 0);

  p := first;
  for i := 0 to cnt -1 do
  begin
		c[0][0] := c[0][0] + DotProdVecs(a0[i], (a0[i]));
		c[0][1] := c[0][1] + DotProdVecs(a0[i], (a1[i]));
		c[1][0] := c[0][1];
		c[1][1] := c[1][1] + DotProdVecs(a1[i], (a1[i]));

    tmp := SubVecs(p.pt,
      AddVecs(Mul(first.pt, B0(u[i])),
      AddVecs(Mul(first.pt, B1(u[i])),
      AddVecs(Mul(last.pt, B2(u[i])),
      Mul(last.pt, B3(u[i]))))));

    x[0] := x[0] + DotProdVecs(a0[i], tmp);
    x[1] := x[1] + DotProdVecs(a1[i], tmp);
    p := p.next;
  end;

  det_c0_c1 := c[0][0] * c[1][1] - c[1][0] * c[0][1];
	det_c0_x := c[0][0] * x[1] - c[1][0] * x[0];
	det_x_c1 := x[0] * c[1][1] - x[1] * c[0][1];

  if det_c0_c1 = 0 then
    alphaL := 0 else
    alphaL := det_x_c1 / det_c0_c1;

  if det_c0_c1 = 0 then
    alphaR := 0 else
    alphaR := det_c0_x / det_c0_c1;

  //check for unlikely fit
  if (alphaL > dist * 2) then alphaL := 0
  else if (alphaR > dist * 2) then alphaR := 0;
  epsilon := 1.0e-6 * dist;

  SetLength(Result, 4);
  Result[0] := first.pt;
  Result[3] := last.pt;
  if (alphaL < epsilon) or (alphaR < epsilon) then
  begin
    dist := dist / 3;
    Result[1] := AddVecs(Result[0], Scale(firstTan, dist));
    Result[2] := AddVecs(Result[3], Scale(lastTan, dist));
  end else
  begin
    Result[1] := AddVecs(Result[0], Scale(firstTan, alphaL));
    Result[2] := AddVecs(Result[3], Scale(lastTan, alphaR));
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.Reparameterize(first: PPt; cnt: integer;
  const u: TArrayOfDouble; const bezier: TPathD): TArrayOfDouble;
var
  i: integer;
begin
  SetLength(Result, cnt);
  for i := 0 to cnt -1 do
  begin
    Result[i] := NewtonRaphsonRootFind(bezier, first.pt, u[i]);
    first := first.next;
  end;
end;
//------------------------------------------------------------------------------

function BezierII(degree: integer; const v: array of TPointD; t: double): TPointD;
var
  i,j: integer;
  tmp: array[0..3] of TPointD;
begin
  Move(v[0], tmp[0], (degree +1) * sizeOf(TPointD));
  for i := 1 to degree do
    for j := 0 to degree - i do
    begin
      tmp[j].x := (1.0 - t) * tmp[j].x + t * tmp[j+1].x;
      tmp[j].y := (1.0 - t) * tmp[j].y + t * tmp[j+1].y;
    end;
  Result := tmp[0];
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeMaxErrorSqrd(first, last: PPt;
	const bezier: TPathD; const u: TArrayOfDouble;
  out SplitPoint: PPt): double;
var
  i: integer;
  distSqrd: double;
  pt: TPointD;
  p: PPt;
begin
	Result := 0;
  i := 1;
  SplitPoint := first.next;
  p := first.next;
	while p <> last do
	begin
		pt := BezierII(3, bezier, u[i]);
		distSqrd := DistanceSqrd(pt, p.pt);
		if (distSqrd >= Result) then
    begin
      Result := distSqrd;
      SplitPoint := p;
    end;
    inc(i);
    p := p.next;
	end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.NewtonRaphsonRootFind(const q: TPathD;
  const pt: TPointD; u: double): double;
var
  numerator, denominator: double;
  qu, q1u, q2u: TPointD;
  q1: array[0..2] of TPointD;
  q2: array[0..1] of TPointD;
begin

  q1[0].x := (q[1].x - q[0].x) * 3.0;
  q1[0].y := (q[1].y - q[0].y) * 3.0;
  q1[1].x := (q[2].x - q[1].x) * 3.0;
  q1[1].y := (q[2].y - q[1].y) * 3.0;
  q1[2].x := (q[3].x - q[2].x) * 3.0;
  q1[2].y := (q[3].y - q[2].y) * 3.0;

  q2[0].x := (q1[1].x - q1[0].x) * 2.0;
  q2[0].y := (q1[1].y - q1[0].y) * 2.0;
  q2[1].x := (q1[2].x - q1[1].x) * 2.0;
  q2[1].y := (q1[2].y - q1[1].y) * 2.0;

  qu  := BezierII(3, q, u);
  q1u := BezierII(2, q1, u);
  q2u := BezierII(1, q2, u);

  numerator := (qu.x - pt.x) * (q1u.x) + (qu.y - pt.y) * (q1u.y);
  denominator := (q1u.x) * (q1u.x) + (q1u.y) * (q1u.y) +
    (qu.x - pt.x) * (q2u.x) + (qu.y - pt.y) * (q2u.y);

  if (denominator = 0) then
    Result := u else
    Result := u - (numerator / denominator);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.FitCubic(first, last: PPt;
  firstTan, lastTan: TPointD): Boolean;
var
  i, cnt: integer;
  splitPoint: PPt;
  centerTan: TPointD;
  bezier: TPathD;
  clps, uPrime: TArrayOfDouble;
  maxErrorSqrd: double;
const
  maxRetries = 4;
begin
  Result := true;
  cnt := Count(first, last);
  if cnt = 2 then
  begin
    SetLength(bezier, 4);
    bezier[0] := first.pt;
    bezier[3] := last.pt;
    bezier[1] := bezier[0];
    bezier[2] := bezier[3];
    AppendSolution(bezier);
    Exit;
  end
  else if cnt = 3 then
  begin
    if TurnsLeft(first.prev.pt, first.pt, first.next.pt) =
      TurnsLeft(first.pt, first.next.pt, last.pt) then
        firstTan := ComputeCenterTangent(first);
    if TurnsLeft(last.prev.pt, last.pt, last.next.pt) =
      TurnsLeft(first.pt, first.next.pt, last.pt) then
        lastTan := NegateVec(ComputeCenterTangent(last));
  end;


  clps := ChordLengthParameterize(first, cnt);
  bezier := GenerateBezier(first, last, cnt, clps, firstTan, lastTan);
  maxErrorSqrd := ComputeMaxErrorSqrd(first, last, bezier, clps, splitPoint);
  if (maxErrorSqrd < tolSqrd) then
  begin
    AppendSolution(bezier);
    Exit;
  end;

  if (maxErrorSqrd < tolSqrd * 4) then //close enough to try again
  begin
		for i := 1 to maxRetries do
    begin
      uPrime := Reparameterize(first, cnt, clps, bezier);
      bezier := GenerateBezier(first, last, cnt, uPrime, firstTan, lastTan);
      maxErrorSqrd :=
        ComputeMaxErrorSqrd(first, last, bezier, uPrime, splitPoint);
			if (maxErrorSqrd < tolSqrd) then
      begin
        AppendSolution(bezier);
        Exit;
			end;
			clps := uPrime;
		end;
	end;

  //We need to break the curve because it's too complex for a single Bezier.
  //If we're changing direction then make this a 'hard' break (see below).
  if TurnsLeft(splitPoint.prev.prev.pt, splitPoint.prev.pt, splitPoint.pt) <>
    TurnsLeft(splitPoint.prev.pt, splitPoint.pt, splitPoint.next.pt) then
  begin
    centerTan := ComputeRightTangent(splitPoint);
    FitCubic(first, splitPoint, firstTan, centerTan);
    centerTan := ComputeLeftTangent(splitPoint);
    FitCubic(splitPoint, last, centerTan, lastTan);
  end else
  begin
    centerTan := ComputeCenterTangent(splitPoint);
    FitCubic(first, splitPoint, firstTan, NegateVec(centerTan));
    FitCubic(splitPoint, last, centerTan, lastTan);
  end;
end;
//------------------------------------------------------------------------------

function HardBreakCheck(ppt: PPt; compareLen: double): Boolean;
var
  q: double;
const
  longLen = 15;
begin
  //A 'break' means starting a new Bezier. A 'hard' break avoids smoothing
  //whereas a 'soft' break will still be smoothed. There is as much art as
  //science in determining where to smooth and where not to. For example,
  //long edges should generally remain straight but how long does an edge
  //have to be to be considered a 'long' edge?

  if (ppt.prev.len * 4 < ppt.len) or (ppt.len * 4 < ppt.prev.len) then
  begin
    //We'll hard break whenever there's significant asymmetry between
    //segment lengths because GenerateBezier() will perform poorly.
    result := true;
  end
  else if ((ppt.prev.len > longLen) and (ppt.len > longLen)) then
  begin
    //hard break long segments only when turning by more than ~45 degrees
    q := (Sqr(ppt.prev.len) + Sqr(ppt.len) - DistanceSqrd(ppt.prev.pt, ppt.next.pt)) /
      (2 * ppt.prev.len * ppt.len); //Cosine Rule.
    result := (1 - abs(q)) > 0.3;
  end
  else if ((TurnsLeft(ppt.prev.prev.pt, ppt.prev.pt, ppt.pt) =
    TurnsRight(ppt.prev.pt, ppt.pt, ppt.next.pt)) and
    (ppt.prev.len > compareLen) and (ppt.len > compareLen)) then
  begin
    //we'll also hard break whenever there's a significant inflection point
    result := true;
  end else
  begin
    //Finally, we'll also force a 'hard' break when there's a significant bend.
    //Again uses the Cosine Rule.
    q :=(Sqr(ppt.prev.len) + Sqr(ppt.len) -
      DistanceSqrd(ppt.prev.pt, ppt.next.pt)) / (2 * ppt.prev.len * ppt.len);
    Result := (q > -0.2); //ie more than 90%
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.FitCurve(const path: TPathD;
  closed: Boolean; tolerance: double; minSegLength: double): TPathD;
var
  i, highI: integer;
  d: double;
  p, p2, pEnd: PPt;
begin
  //tolerance: specifies the maximum allowed variance between the existing
  //vertices and the new Bezier curves. More tolerance will produce
  //fewer Beziers and simpler paths, but at the cost of less precison.
  tolSqrd := Sqr(Max(1, Min(10, tolerance))); //range 1..10

  //minSegLength: Typically when vectorizing raster images, the produced
  //vector paths will have many series of axis aligned segments that trace
  //pixel boundaries. These paths will also contain many 1 unit segments at
  //right angles to adjacent segments. Importantly, these very short segments
  //will cause artifacts in the solution unless they are trimmed.
  highI     := High(path);
  if closed then
    while (highI > 0) and (Distance(path[highI], path[0]) < minSegLength) do
      dec(highI);

  p := AddPt(path[0]);
  for i := 1 to highI do
  begin
    d := Distance(p.pt, path[i]);
    //skip line segments with lengths less than 'minSegLength'
    if d < minSegLength then Continue;
    p := AddPt(path[i]);
    p.prev.len := d;
    p.prev.vec := SubVecs(p.pt, p.prev.pt);
  end;
  p.len := Distance(ppts.pt, p.pt);
  p.vec := SubVecs(p.next.pt, p.pt);
  p := ppts;

  if (p.next = p) or (closed and (p.next = p.prev)) then
  begin
    Clear;
    result := nil;
    Exit;
  end;

  //for closed paths, find a good starting point
  if closed then
  begin
    repeat
      if HardBreakCheck(p, tolerance) then break;
      p := p.next;
    until p = ppts;
    pEnd := p;
  end else
    pEnd := ppts.prev;

  p2 := p.next;
  repeat
    if HardBreakCheck(p2, tolerance) then
    begin
      FitCubic(p, p2, ComputeLeftTangent(p), ComputeRightTangent(p2));
      p := p2;
    end;
    p2 := p2.next;
  until (p2 = pEnd);
  FitCubic(p, p2, ComputeLeftTangent(p), ComputeRightTangent(p2));

  Result := solution;
  Clear;
end;
//------------------------------------------------------------------------------

procedure TFitCurveContainer.AppendSolution(const bezier: TPathD);
var
  i, len: integer;
begin
  len := Length(solution);
  if len > 0 then
  begin
    SetLength(solution, len + 3);
    for i := 0 to 2 do
      solution[len +i] := bezier[i +1];
  end else
    solution := bezier;
end;
//------------------------------------------------------------------------------

function SmoothToBezier(const path: TPathD; closed: Boolean;
  tolerance: double; minSegLength: double): TPathD;
var
  paths, solution: TPathsD;
begin
  SetLength(paths, 1);
  paths[0] := path;
  solution := SmoothToBezier(paths, closed, tolerance, minSegLength);
  if solution <> nil then
    Result := solution[0];
end;
//------------------------------------------------------------------------------

function SmoothToBezier(const paths: TPathsD; closed: Boolean;
  tolerance: double; minSegLength: double): TPathsD;
var
  i,j, len: integer;
begin
  j := 0;
  len := Length(paths);
  SetLength(Result, len);
  with TFitCurveContainer.Create do
  try
    for i := 0 to len -1 do
      if (paths[i] <> nil) and (Abs(Area(paths[i])) > Sqr(tolerance)) then
      begin
        Result[j] := FitCurve(paths[i], closed, tolerance, minSegLength);
        inc(j);
      end;
  finally
    Free;
  end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
