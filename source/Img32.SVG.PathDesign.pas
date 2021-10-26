unit Img32.SVG.PathDesign;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.4                                                             *
* Date      :  25 October 2021                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Supports designing SVG paths                                    *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Types,
  Math, Img32, Img32.Svg.Core, Img32.Vector, Img32.Layers;

type
  TSegPos = (spFirst, spMiddle, spLast);
  TSvgSubPathLayer    = class;
  TSvgPathLayer       = class;

  TSegBaseLayer = class(TVectorLayer32)
  private
    fOwner        : TSvgPathLayer;
    fFocused      : Boolean;
    fRotAngle     : double;
    fSeg          : TSvgBaseSeg;
  protected
    function  GetPrevSegLayer: TSegBaseLayer;
    function  GetNextSegLayer: TSegBaseLayer;
    function  GetDesignerLayer(btnLayer: TLayer32): TDesignerLayer32;
    procedure DrawDesigner(designer: TDesignerLayer32); virtual;
    procedure SetFocus(value: Boolean);
    procedure SetBtnCtrlPts(const pts: TPathD); virtual;
    procedure BtnMoveCheck(const pos: TPointD); virtual;
    property  Owner: TSvgPathLayer read fOwner;
  public
    constructor Create(parent: TLayer32;  const name: string = ''); override;
    procedure Init(seg: TSvgBaseSeg); virtual;
    function  CreateBtnGroup: TButtonGroupLayer32; virtual;
    procedure UpdateBtnGroup(movedBtn: TLayer32); virtual;
    function  CreateRotateBtnGroup(target: TSegBaseLayer;
      startAngle: double = 0; angleOffset: double = 0): TRotatingGroupLayer32; virtual;
    procedure UpdateRotateBtnGroup(movedBtn: TLayer32); virtual;
    function  TestUpdateBtnGroup(movedBtn: TLayer32;
      var dx, dy: integer): Boolean; virtual;
    procedure DrawPath;
    property  Focused: Boolean read fFocused write SetFocus;
    property  Seg: TSvgBaseSeg read fSeg;
  end;

  TSvgASegLayer = class(TSegBaseLayer)
  private
    fRectMargin     : integer;
    fRotateDistance : double;
    procedure OffsetHorzBtn(btn: TLayer32);
    procedure OffsetVertBtn(btn: TLayer32);
    function GetPointFromHorzBtn(btn: TLayer32): TPointD;
    function GetPointFromVertBtn(btn: TLayer32): TPointD;
  protected
    procedure DrawDesigner(designer: TDesignerLayer32); override;
  public
    constructor Create(parent: TLayer32;  const name: string = ''); override;
    procedure Init(seg: TSvgBaseSeg); override;
    function CreateBtnGroup: TButtonGroupLayer32; override;
    function TestUpdateBtnGroup(movedBtn: TLayer32;
      var dx, dy: integer): Boolean; override; //todo - redo this with absolute points
    procedure UpdateBtnGroup(movedBtn: TLayer32); override;
    function  CreateRotateBtnGroup(target: TSegBaseLayer;
      startAngle: double = 0; angleOffset: double = 0): TRotatingGroupLayer32; override;
    procedure UpdateRotateBtnGroup(movedBtn: TLayer32); override;
    procedure ReverseArcDirection;
    //RectMargin: offset for rec horz and vert ctrl buttons
    //so they don't get in the way of the arc button
    property RectMargin: integer read fRectMargin write fRectMargin;
  end;

  TSvgCSegLayer = class(TSegBaseLayer)
  protected
    procedure DrawDesigner(designer: TDesignerLayer32); override;
  public
    function CreateBtnGroup: TButtonGroupLayer32; override;
  end;

  TSvgHSegLayer = class(TSegBaseLayer)
  public
    function TestUpdateBtnGroup(movedBtn: TLayer32;
      var dx, dy: integer): Boolean; override;
  end;

  TSvgLSegLayer = class(TSegBaseLayer)
  end;

  TSvgQSegLayer = class(TSegBaseLayer)
  public
    function CreateBtnGroup: TButtonGroupLayer32; override;
    procedure DrawDesigner(designer: TDesignerLayer32); override;
  end;

  TSvgSSegLayer = class(TSegBaseLayer)
  protected
    procedure BtnMoveCheck(const pos: TPointD); override;
  public
    function CreateBtnGroup: TButtonGroupLayer32; override;
    procedure DrawDesigner(designer: TDesignerLayer32); override;
  end;

  TSvgTSegLayer = class(TSegBaseLayer)
  protected
    procedure BtnMoveCheck(const pos: TPointD); override;
  end;

  TSvgVSegLayer = class(TSegBaseLayer)
  public
    function TestUpdateBtnGroup(movedBtn: TLayer32;
      var dx, dy: integer): Boolean; override; //todo - redo this with absolute points
  end;

  TSvgZSegLayer = class(TSegBaseLayer)
  protected
    procedure BtnMoveCheck(const pos: TPointD); override;
  public
    destructor Destroy; override;
    procedure Init(seg: TSvgBaseSeg); override;
    function CreateBtnGroup: TButtonGroupLayer32; override;
    function TestUpdateBtnGroup(movedBtn: TLayer32;
      var moveDx, moveDy: integer): Boolean; override;
    procedure UpdateBtnGroup(movedBtn: TLayer32); override;
    function  CreateRotateBtnGroup(target: TSegBaseLayer;
      startAngle: double = 0; angleOffset: double = 0): TRotatingGroupLayer32; override;
    procedure UpdateRotateBtnGroup(movedBtn: TLayer32); override;
  end;

  TSvgSubPathLayer = class(TGroupLayer32)
  private
    fOwner        : TSvgPathLayer;
    fSubPath      : TSvgSubPath;
    function GetSegLayer(index: integer): TSegBaseLayer;
  protected
    property Owner : TSvgPathLayer read fOwner;
  public
    procedure Init(subPath: TSvgSubPath); virtual;
    procedure Offset(dx, dy: integer); override;
    function GetLastSegLayer: TSegBaseLayer;
    function GetStringDef(decimalPrec: integer): string;
    property SubPath: TSvgSubPath read fSubPath;
    property SegLayer[index: integer]: TSegBaseLayer read GetSegLayer;
  end;

  TSvgPathLayer = class(TGroupLayer32)
  private
    fStrokeColor  : TColor32;
    fStrokeColor2 : TColor32;
    fstrokeWidth  : double;
    fRelativeStr  : Boolean;
    fSvgPath      : TSvgPath;
    function GetMargin: integer;
    procedure SetStrokeWidth(width: double);
    function GetSubPathLayer(index: integer): TSvgSubPathLayer;
  protected
    property StrokeColor  : TColor32 read fStrokeColor;
    property StrokeColor2 : TColor32 read fStrokeColor2;
  public
    constructor Create(parent: TLayer32;  const name: string = ''); override;
    destructor Destroy; override;
    procedure LoadPath(const dpath: string; const destRect: TRect; scale: double = 0);
    property StrokeWidth  : double read fstrokeWidth write SetStrokeWidth;
    property SvgPath: TSvgPath read fSvgPath;
    property SvgSubPath[index: integer]: TSvgSubPathLayer read GetSubPathLayer;
    property Margin  : integer read GetMargin;
  end;

implementation

uses
  Img32.Extra, Img32.Draw;

resourcestring
  rsErrorCreatingButtonGroup =
    'Error: only the last segment in an SVG path can be edited.';
  rsErrorArc = 'Error: Invalid Arc segment.';

const
  controlBtnColor   : TColor32 = $FFFFBB00;
  arcRectBtnColor   : TColor32 = clGreen32;
  rotatePivotColor  : TColor32 = clLime32;
  rotateBtnColor    : TColor32 = clGreen32;

//------------------------------------------------------------------------------
// TSegBaseLayer
//------------------------------------------------------------------------------

constructor TSegBaseLayer.Create(parent: TLayer32;  const name: string = '');
begin
  inherited;
  fOwner := Parent.Parent as TSvgPathLayer;
  Margin := Max(fOwner.Margin, Ceil(DefaultButtonSize /2));
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.Init(seg: TSvgBaseSeg);
begin
  fSeg := seg;
  Paths := Img32.Vector.Paths(fSeg.FlatPath);
  DrawPath;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.DrawPath;
var
  c: TColor32;
  p: TPathD;
begin
  if not Assigned(Paths) or not Assigned(Paths[0]) then Exit;
  //Image.Clear; //Image.Clear($10FF0000);
  if Focused then c := Owner.fStrokeColor2 else c := Owner.fStrokeColor;
  p := OffsetPath(Paths[0], -Left, -Top);
  DrawLine(Image, p, Owner.StrokeWidth, c, esRound);

  //UpdateHitTestMaskFromImage;
  //draw a wider stroke to define the HT region
  HitTestRec.htImage.SetSize(Image.Width, Image.Height);
  DrawLine(HitTestRec.htImage, p, Owner.StrokeWidth*2, clBlack32, esRound);
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.SetBtnCtrlPts(const pts: TPathD);
begin
  fSeg.ctrlPts := pts;
  Paths := Img32.Vector.Paths(fSeg.FlatPath);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.GetPrevSegLayer: TSegBaseLayer;
var
  layer: TLayer32;
begin
  layer := PrevLayerInGroup;
  if Assigned(layer) and (layer is TSegBaseLayer) then
    Result := TSegBaseLayer(layer) else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.GetNextSegLayer: TSegBaseLayer;
var
  layer: TLayer32;
begin
  layer := NextLayerInGroup;
  if Assigned(layer) and (layer is TSegBaseLayer) then
    Result := TSegBaseLayer(layer) else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.GetDesignerLayer(btnLayer: TLayer32): TDesignerLayer32;
begin
  if Assigned(btnLayer) and
    (btnLayer.Parent is TButtonGroupLayer32) and
    (btnLayer.Parent[0] is TDesignerLayer32) then
      Result := TDesignerLayer32(btnLayer.Parent[0])
  else
    Result := nil;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.DrawDesigner(designer: TDesignerLayer32);
begin
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.SetFocus(value: Boolean);
begin
  if fFocused = value then Exit;
  fFocused := value;
  DrawPath;
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  designer: TDesignerLayer32;
begin
  Result := CreateButtonGroup(Root,
    fSeg.ctrlPts, bsRound, DefaultButtonSize, clRed32);

  //insert the designer layer **below** the button layers
  designer := Result.InsertChild(TDesignerLayer32, 0) as TDesignerLayer32;
  DrawDesigner(designer);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.TestUpdateBtnGroup(movedBtn: TLayer32;
  var dx, dy: integer): Boolean;
begin
  Result := (movedBtn is TButtonDesignerLayer32) or
    not TButtonDesignerLayer32(movedBtn).Enabled;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.BtnMoveCheck(const pos: TPointD);
var
  segLayer  : TSegBaseLayer;
  dx, dy    : integer;
begin
  if PointsEqual(pos, fSeg.FirstPt) then Exit;
  dx := Round(pos.X - fSeg.FirstPt.X);
  dy := Round(pos.Y - fSeg.FirstPt.Y);
  fSeg.Offset(dx, dy);
  Offset(dx, dy);
  segLayer := GetNextSegLayer;
  if Assigned(segLayer) then
    segLayer.BtnMoveCheck(fSeg.CtrlPts[High(fSeg.CtrlPts)]);
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.UpdateBtnGroup(movedBtn: TLayer32);
var
  i         : integer;
  pts       : TPathD;
  designer  : TDesignerLayer32;
  segLayer  : TSegBaseLayer;
begin
  designer := GetDesignerLayer(movedBtn);
  if not assigned(designer)  then Exit;
  i := TButtonDesignerLayer32(movedBtn).BtnIdx;
  pts := fSeg.ctrlPts;
  pts[i] := movedBtn.MidPoint;
  SetBtnCtrlPts(pts);
  DrawPath;
  DrawDesigner(designer);

  //move the following segments
  segLayer := GetNextSegLayer;
  if Assigned(segLayer) then
    segLayer.BtnMoveCheck(pts[High(pts)]);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.CreateRotateBtnGroup(target: TSegBaseLayer;
  startAngle: double; angleOffset: double): TRotatingGroupLayer32;
begin
//  if (GetNextSegLayer <> nil) then
//    Raise Exception.Create(rsErrorCreatingButtonGroup);
  //get control points stripped of local coord reference

  //fRotAngle := 0;
  Result := CreateRotatingButtonGroup(target, fSeg.FirstPt, DefaultButtonSize,
   clWhite32, rotateBtnColor, startAngle, angleOffset);
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.UpdateRotateBtnGroup(movedBtn: TLayer32);
var
  a: double;
  segLayer  : TSegBaseLayer;
begin
  if not (movedBtn.Parent is TRotatingGroupLayer32) then Exit;
  a := UpdateRotatingButtonGroup(movedBtn);
  with fSeg do
   ctrlPts := RotatePath(ctrlPts, FirstPt, a - fRotAngle);
  fRotAngle := a;
  Paths := Img32.Vector.Paths(fSeg.FlatPath);
  DrawPath;

  //move the following segments
  segLayer := GetNextSegLayer;
  if Assigned(segLayer) then
    with Self.fSeg do
      segLayer.BtnMoveCheck(ctrlPts[High(ctrlPts)]);
end;

//------------------------------------------------------------------------------
// TSvgASegLayer
//------------------------------------------------------------------------------

constructor TSvgASegLayer.Create(parent: TLayer32;  const name: string = '');
begin
  inherited;
  fRectMargin := 20;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.Init(seg: TSvgBaseSeg);
begin
  fSeg    := seg;
  Paths := Img32.Vector.Paths(fSeg.FlatPath);
  DrawPath;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.ReverseArcDirection;
begin
  with TSvgASegment(fSeg).arcInfo do
    sweepClockW := not sweepClockW;
  Paths := Img32.Vector.Paths(fSeg.FlatPath);
  DrawPath;
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.OffsetHorzBtn(btn: TLayer32);
var
  pt: TPointD;
begin
  pt := btn.MidPoint;
  with TSvgASegment(fSeg), arcInfo do
  begin
    if rectAngle <> 0 then RotatePoint(pt, rec.MidPoint, -rectAngle);
    if arcRectLeft then
      pt.X := pt.X - fRectMargin else
      pt.X := pt.X + fRectMargin;
    if rectAngle <> 0 then RotatePoint(pt, rec.MidPoint, rectAngle);
  end;
  btn.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.OffsetVertBtn(btn: TLayer32);
var
  pt: TPointD;
begin
  pt := btn.MidPoint;
  with TSvgASegment(fSeg), arcInfo do
  begin
    if rectAngle <> 0 then RotatePoint(pt, rec.MidPoint, -rectAngle);
    if arcRectTop then
      pt.Y := pt.Y - fRectMargin else
      pt.Y := pt.Y + fRectMargin;
    if rectAngle <> 0 then RotatePoint(pt, rec.MidPoint, rectAngle);
  end;
  btn.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetPointFromHorzBtn(btn: TLayer32): TPointD;
begin
  Result := btn.MidPoint;
  with TSvgASegment(fSeg), arcInfo do
  begin
    if rectAngle <> 0 then RotatePoint(Result, rec.MidPoint, -rectAngle);
    if arcRectLeft then
      Result.X := Result.X + fRectMargin else
      Result.X := Result.X - fRectMargin;
    if rectAngle <> 0 then RotatePoint(Result, rec.MidPoint, rectAngle);
  end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetPointFromVertBtn(btn: TLayer32): TPointD;
begin
  Result := btn.MidPoint;
  with TSvgASegment(fSeg), arcInfo do
  begin
    if rectAngle <> 0 then RotatePoint(Result, rec.MidPoint, -rectAngle);
    if arcRectTop then
      Result.Y := Result.Y + fRectMargin else
      Result.Y := Result.Y - fRectMargin;
    if rectAngle <> 0 then RotatePoint(Result, rec.MidPoint, rectAngle);
  end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  i: integer;
  designer: TDesignerLayer32;
begin
  Result := CreateButtonGroup(Root, fSeg.ctrlPts,
    bsRound, DefaultButtonSize, clRed32);

  TButtonDesignerLayer32(Result[0]).Visible := false;
  OffsetHorzBtn(Result[1]);
  OffsetVertBtn(Result[3]);

  //insert the designer layer below the button layers (ie level 0)
  designer := Result.InsertChild(TDesignerLayer32, 0) as TDesignerLayer32;
  DrawDesigner(designer);

  for i := 2 to 4 do
    with TButtonDesignerLayer32(Result[i]) do   //rect horz
    begin
      Color := arcRectBtnColor;
      Enabled := true;
      Draw;
    end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.TestUpdateBtnGroup(movedBtn: TLayer32;
  var dx, dy: integer): Boolean;
var
  a: double;
  r: TRectD;
  pt, mp: TPointD;
begin
  Result := inherited TestUpdateBtnGroup(movedBtn, dx, dy);
  if not Result then Exit;

  with TSvgASegment(fSeg), arcInfo do
  begin
    r := rec;
    mp := r.MidPoint;
    pt := PointD(movedBtn.MidPoint.X + dx, movedBtn.MidPoint.Y + dy);


    if movedBtn.Parent is TRotatingGroupLayer32 then
    begin
      a := GetAngle(startPos, pt);
      pt := GetPointAtAngleAndDist(startPos, a, fRotateDistance);
      dx := Round(pt.X - movedBtn.MidPoint.X);
      dy := Round(pt.Y - movedBtn.MidPoint.Y);
      Exit;
    end;

    img32.Vector.RotatePoint(pt, mp, -rectAngle);

    case TButtonDesignerLayer32(movedBtn).BtnIdx of
      1: pt.Y := mp.Y;                  //rect horz
      2:                                //rect midpoint
        begin
          Result := true;
          Exit;
        end;
      3: pt.X := mp.X;                  //rect top
      4:                                //end point
        begin
          a := GetEllipticalAngleFromPoint(r, pt);
          pt := GetPtOnEllipseFromAngle(r, a);
        end;
      else
      begin
        Result := false;
        Exit;
      end;
    end;
    img32.Vector.RotatePoint(pt, mp, rectAngle);
  end;
  dx := Round(pt.X - movedBtn.MidPoint.X);
  dy := Round(pt.Y - movedBtn.MidPoint.Y);
end;
//------------------------------------------------------------------------------

function FlipAngleHorizontally(angle: double): double;
begin
  Result := angle180 - angle;
  NormalizeAngle(Result);
end;
//------------------------------------------------------------------------------

function FlipAngleVertically(angle: double): double;
begin
  NormalizeAngle(angle);
  Result := -angle;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.UpdateBtnGroup(movedBtn: TLayer32);
var
  sa,ea, a  : double;
  dx,dy     : double;
  mp, sp    : TPointD;
  designer  : TDesignerLayer32;
  segLayer  : TSegBaseLayer;
begin
  designer := GetDesignerLayer(movedBtn);
  if not Assigned(designer) then Exit;

  with TSvgASegment(fSeg), arcInfo do
  begin
    case TButtonDesignerLayer32(movedBtn).BtnIdx of
      1:                                //rect horz
        begin
          //save the start point before resizing rect
          //then after resizing, realign the rect so
          //the start point doesn't change
          sa := GetStartAngle;
          ea := GetEndAngle;
          //a: button distance adjustment
          //ie how much more the rec edge should move to keep it
          //aligned with the move button (avoids overshoot & undershoot)
          a := 1-Sqrt(Abs(cos(sa)));

          mp := GetPointFromHorzBtn(movedBtn);
          img32.Vector.RotatePoint(mp, rec.MidPoint, - rectAngle);
          if arcRectLeft then
          begin
            dx := mp.X - rec.Left;
            rec.Left := rec.Left + dx + a*dx;
          end else
          begin
            dx := mp.X - rec.Right;
            rec.Right := rec.Right + dx + a*dx;
          end;

          if rec.Normalize then
          begin
            arcRectLeft := not arcRectLeft;
            sa := FlipAngleHorizontally(sa);
            ea := FlipAngleHorizontally(ea);
            sweepClockW := not sweepClockW;
          end;

          sp := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, sa);
          endPos := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, ea);
          dx := startPos.X - sp.X; dy := startPos.Y - sp.Y;
          OffsetRect(rec, dx, dy);
          endPos := OffsetPoint(endPos, dx, dy);

          UpdateCtrlPts;
          Paths := Img32.Vector.Paths(fSeg.FlatPath);
          DrawPath;
          DrawDesigner(designer);

          movedBtn.PositionCenteredAt(ctrlPts[1]);
          OffsetHorzBtn(movedBtn);
          movedBtn.Parent[3].PositionCenteredAt(ctrlPts[2]);
          movedBtn.Parent[4].PositionCenteredAt(ctrlPts[3]);
          OffsetVertBtn(movedBtn.Parent[4]);
          movedBtn.Parent[5].PositionCenteredAt(endPos);
        end;
      2:                                //rect midpoint
        begin
          mp := movedBtn.MidPoint;
          dx := mp.X - rec.MidPoint.X;
          dy := mp.Y - rec.MidPoint.Y;
          OffsetRect(rec, dx, dy);
          endPos := OffsetPoint(endPos, dx, dy);
          sa := GetStartAngle;
          sp := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, sa);
          dx := (startpos.X - sp.X); dy:= (startpos.Y - sp.Y);
          OffsetRect(rec, dx, dy);
          endPos := OffsetPoint(endPos, dx, dy);
          UpdateCtrlPts;
          Paths := Img32.Vector.Paths(fSeg.FlatPath);
          DrawPath;
          DrawDesigner(designer);

          movedBtn.Parent[2].PositionCenteredAt(ctrlPts[1]);
          OffsetHorzBtn(movedBtn.Parent[2]);
          movedBtn.Parent[3].PositionCenteredAt(ctrlPts[2]);
          movedBtn.Parent[4].PositionCenteredAt(ctrlPts[3]);
          OffsetVertBtn(movedBtn.Parent[4]);
          movedBtn.Parent[5].PositionCenteredAt(endPos);
        end;
      3:                                //rect vert
        begin
          //save the start point before resizing rect
          //then after resizing, realign the rect so
          //the start point doesn't change
          sa := GetStartAngle;
          ea := GetEndAngle;
          a := 1-Sqrt(Abs(sin(sa))); //button distance adjustment
          mp := movedBtn.MidPoint;
          mp := GetPointFromVertBtn(movedBtn);
          img32.Vector.RotatePoint(mp, rec.MidPoint, - rectAngle);
          if arcRectTop then
          begin
            dy := mp.Y - rec.Top;
            rec.Top := rec.Top + dy + a*dy;
          end else
          begin
            dy := mp.Y - rec.Bottom;
            rec.Bottom := rec.Bottom + dy + a*dy;
          end;
          if rec.Normalize then
          begin
            arcRectTop := not arcRectTop;
            sa := FlipAngleVertically(sa);
            ea := FlipAngleVertically(ea);
            sweepClockW := not sweepClockW;
          end;
          sp := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, sa);
          dx := (startpos.X - sp.X); dy:= (startpos.Y - sp.Y);
          OffsetRect(rec, dx, dy);
          endPos := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, ea);
          UpdateCtrlPts;

          Paths := Img32.Vector.Paths(fSeg.FlatPath);
          DrawPath;
          DrawDesigner(designer);

          movedBtn.Parent[2].PositionCenteredAt(ctrlPts[1]);
          OffsetHorzBtn(movedBtn.Parent[2]);
          movedBtn.Parent[3].PositionCenteredAt(ctrlPts[2]);
          movedBtn.PositionCenteredAt(ctrlPts[3]);
          OffsetVertBtn(movedBtn);
          movedBtn.Parent[5].PositionCenteredAt(endPos);
        end;
      4:
        begin
          mp := movedBtn.MidPoint;
          endPos := GetClosestPtOnRotatedEllipse(rec, rectAngle, mp);
          UpdateCtrlPts;
          Paths := Img32.Vector.Paths(fSeg.FlatPath);
          DrawPath;
        end;
    end; //end case
  end; //end with

  //move the following segments
  segLayer := GetNextSegLayer;
  if Assigned(segLayer) then
    with TSvgASegment(fSeg).arcInfo do
      segLayer.BtnMoveCheck(endPos);
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.CreateRotateBtnGroup(target: TSegBaseLayer;
  startAngle: double; angleOffset: double): TRotatingGroupLayer32;
var
  a: double;
begin
  with TSvgASegment(fSeg).arcInfo do
  begin
    a := GetAngle(startPos, rec.MidPoint) - rectAngle;
    Result := inherited CreateRotateBtnGroup(target, rectAngle, a);
    Result.AngleButton.PositionCenteredAt(rec.MidPoint);
  end;
  UpdateRotateBtnGroup(Result.AngleButton);
  fRotateDistance := Result.DistBetweenButtons;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.UpdateRotateBtnGroup(movedBtn: TLayer32);
var
  a, sa, ea, dx, dy: double;
  sp: TPointD;
  segLayer  : TSegBaseLayer;
begin
  if not (movedBtn.Parent is TRotatingGroupLayer32) then Exit;
  a := UpdateRotatingButtonGroup(movedBtn);
  if a = 0 then Exit;

  with TSvgASegment(fSeg), arcInfo do
  begin
    //save the start angle before rotating rect
    //then after rotating, realign the rect so
    //the start point doesn't change
    sa := GetStartAngle;
    ea := GetEndAngle;

    rectAngle := a;
    sp := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, sa);
    dx := sp.X - startPos.X; dy := sp.Y - startPos.Y;
    OffsetRect(rec, -dx, -dy);
    endPos := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, ea);

    UpdateCtrlPts;
    Paths := Img32.Vector.Paths(fSeg.FlatPath);
    DrawPath;
  end;
  Invalidate(Bounds);

  //move the following segments
  segLayer := GetNextSegLayer;
  if Assigned(segLayer) then
    with Self.fSeg do
      segLayer.BtnMoveCheck(ctrlPts[High(ctrlPts)]);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.DrawDesigner(designer: TDesignerLayer32);
var
  j: integer;
  r,r2: TRectD;
  p, p2: TPathD;
begin
  j := Ceil(Owner.StrokeWidth);
  with TSvgASegment(fSeg).arcInfo do
  begin
    r := rec;
    p := Ellipse(r);
    if rectAngle <> 0 then
      p := RotatePath(p, r.MidPoint, rectAngle);

    r := Img32.Vector.GetBoundsD(p);
    p2 := OffsetPath(p, startPos.X - r.MidPoint.X, startPos.Y - r.MidPoint.Y);
    r2 := Img32.Vector.GetBoundsD(p2);
    r := UnionRect(r, r2);
    InflateRect(r, dpiAware1, dpiAware1); //ie pen width
    designer.SetBounds(Rect(r));

    p := OffsetPath(p, -designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, p, [j,j],
      nil, dpiAware1, clMaroon32, esPolygon);

    p2 := OffsetPath(p2, -designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, p2, [j,j],
      nil, dpiAware1, clSilver32, esPolygon);
  end;
end;

//------------------------------------------------------------------------------
// TSvgCSegLayer
//------------------------------------------------------------------------------

function TSvgCSegLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  i: integer;
begin
  Result := inherited CreateBtnGroup;
  //NB: THE FIRST LAYER IN THIS GROUP IS THE DESIGNER LAYER
  for i := 1 to Result.ChildCount div 3 do
  begin
    with TButtonDesignerLayer32(Result[i*3-2]) do
    begin
      Color := controlBtnColor;
      Draw;
    end;
    with TButtonDesignerLayer32(Result[i*3-1]) do
    begin
      Color := controlBtnColor;
      Draw;
      //make sure 'control' buttons are on top of adjacent 'end' buttons
      //move every second ctrl button on top of the following button
      Move(Result, i*3);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgCSegLayer.DrawDesigner(designer: TDesignerLayer32);
var
  i,j: integer;
  pt: TPointD;
  p: TPathD;
begin
  j := Ceil(Owner.StrokeWidth);
  SetLength(p, 2);
  with fSeg do
  begin
    designer.SetBounds(Rect(GetCtrlBounds));
    pt := FirstPt;
    for i := 0 to Length(ctrlPts) div 3 -1 do
    begin
      p[0] := pt;
      p[1] := ctrlPts[i*3];
      p := OffsetPath(p, -designer.Left, -designer.Top);
      DrawDashedLine(designer.Image, p, [j,j],
        nil, Self.Owner.StrokeWidth/2, clRed32, esRound);
      pt := ctrlPts[i*3+2];
      p[0] := ctrlPts[i*3+1];
      p[1] := pt;
      p := OffsetPath(p, -designer.Left, -designer.Top);
      DrawDashedLine(designer.Image, p, [j,j],
        nil, Self.Owner.StrokeWidth/2, clRed32, esRound);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TSvgHSegLayer
//------------------------------------------------------------------------------

function TSvgHSegLayer.TestUpdateBtnGroup(movedBtn: TLayer32;
  var dx, dy: integer): Boolean;
begin
  Result := inherited TestUpdateBtnGroup(movedBtn, dx, dy) and
    not (movedBtn.Parent is TRotatingGroupLayer32);
  if not Result then Exit;
  dy := 0;
end;
//------------------------------------------------------------------------------

function TSvgQSegLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  i: integer;
begin
  Result := inherited CreateBtnGroup;
  //NB: THE FIRST LAYER IN THIS GROUP IS THE DESIGNER LAYER
  //and control buttons need to be on top of adjacent 'end' buttons
  for i := 1 to Result.ChildCount div 2 do
    with TButtonDesignerLayer32(Result[i*2-1]) do
    begin
      Color := controlBtnColor;
      Draw;
      Move(Result, i*2);
    end;
end;
//------------------------------------------------------------------------------

procedure TSvgQSegLayer.DrawDesigner(designer: TDesignerLayer32);
var
  i,j: integer;
  p: TPathD;
  pt: TPointD;
begin
  j := Ceil(Owner.StrokeWidth);
  SetLength(p, 2);
  with fSeg do
  begin
    designer.SetBounds(Rect(GetCtrlBounds));
    pt := FirstPt;
    for i := 0 to High(ctrlPts) div 2 do
    begin
      p[0] := pt;
      p[1] := ctrlPts[i*2];
      p := OffsetPath(p, -designer.Left, -designer.Top);
      DrawDashedLine(designer.Image, p, [j,j],
        nil, Self.Owner.StrokeWidth/2, clRed32, esRound);
      pt := ctrlPts[i*2+1];
      p[0] := ctrlPts[i*2];
      p[1] := pt;
      p := OffsetPath(p, -designer.Left, -designer.Top);
      DrawDashedLine(designer.Image, p, [j,j],
        nil, Self.Owner.StrokeWidth/2, clRed32, esRound);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TSvgSSegLayer
//------------------------------------------------------------------------------

function TSvgSSegLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  i: integer;
begin
  Result := inherited CreateBtnGroup;
  //NB: THE FIRST LAYER IN THIS GROUP IS THE DESIGNER LAYER
  for i := 1 to Result.ChildCount div 2 do
    with TButtonDesignerLayer32(Result[i*2-1]) do
    begin
      Color := controlBtnColor;
      Draw;
      Move(Result, i*2);
    end;
end;
//------------------------------------------------------------------------------

procedure TSvgSSegLayer.DrawDesigner(designer: TDesignerLayer32);
var
  i,j: integer;
  p: TPathD;
begin
  j := Ceil(Owner.StrokeWidth);
  SetLength(p, 2);
  with fSeg do
  begin
    designer.SetBounds(Rect(GetCtrlBounds));
    for i := 0 to High(ctrlPts) div 2 do
    begin
      p[0] := ctrlPts[i*2];
      p[1] := ctrlPts[i*2+1];
      p := OffsetPath(p, -designer.Left, -designer.Top);
      DrawDashedLine(designer.Image, p, [j,j],
        nil, Self.Owner.StrokeWidth/2, clRed32, esRound);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgSSegLayer.BtnMoveCheck(const pos: TPointD);
var
  dx,dy : integer;
  layer : TSegBaseLayer;
begin
  layer := GetPrevSegLayer;
  if layer is TSvgCSegLayer then
  begin
    dx := Round(pos.X - fSeg.FirstPt.X);
    dy := Round(pos.Y - fSeg.FirstPt.Y);
    if (dx <> 0) or (dy <> 0) then
    begin
      fSeg.Offset(dx, dy);
      Offset(dx, dy);
    end;
    //reset Path to redraw and reposition the layer
    SetBtnCtrlPts(fSeg.CtrlPts);
    DrawPath;
    layer := GetNextSegLayer;
    if Assigned(layer) then
      layer.BtnMoveCheck(fSeg.CtrlPts[High(fSeg.CtrlPts)]);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TSvgTSegLayer
//------------------------------------------------------------------------------

procedure TSvgTSegLayer.BtnMoveCheck(const pos: TPointD);
var
  dx,dy : integer;
  layer : TSegBaseLayer;
begin
  layer := GetPrevSegLayer;
  if layer is TSvgQSegLayer then
  begin
    dx := Round(pos.X - fSeg.FirstPt.X);
    dy := Round(pos.Y - fSeg.FirstPt.Y);
    if (dx <> 0) or (dy <> 0) then
    begin
      fSeg.Offset(dx, dy);
      Offset(dx, dy);
    end;
    //reset Path to redraw and reposition the layer
    SetBtnCtrlPts(fSeg.CtrlPts);
    DrawPath;
    layer := GetNextSegLayer;
    if Assigned(layer) then
      layer.BtnMoveCheck(fSeg.CtrlPts[High(fSeg.CtrlPts)]);
  end else
    inherited;
end;

//------------------------------------------------------------------------------
// TSvgVSegLayer
//------------------------------------------------------------------------------

function TSvgVSegLayer.TestUpdateBtnGroup(movedBtn: TLayer32;
  var dx, dy: integer): Boolean;
begin
  Result := inherited TestUpdateBtnGroup(movedBtn, dx, dy) and
    not (movedBtn.Parent is TRotatingGroupLayer32);
  if not Result then Exit;
  dx := 0;
end;

//------------------------------------------------------------------------------
// TSvgZSegLayer
//------------------------------------------------------------------------------

destructor TSvgZSegLayer.Destroy;
begin
  TSvgSubPathLayer(Parent).fSubPath.isClosed := false;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgZSegLayer.Init(seg: TSvgBaseSeg);
begin
  with TSvgSubPathLayer(Parent).fSubPath do
    Paths := Img32.Vector.Paths([GetFirstPt, GetLastPt]);
  DrawPath;
end;
//------------------------------------------------------------------------------

function TSvgZSegLayer.CreateBtnGroup: TButtonGroupLayer32;
begin
  Result := CreateButtonGroup(Root,
    Paths[0], bsRound, DefaultButtonSize, clRed32);
  TButtonDesignerLayer32(Result[0]).Visible := false;
  TButtonDesignerLayer32(Result[1]).Visible := false;
  Result.InsertChild(TDesignerLayer32, 0);
end;
//------------------------------------------------------------------------------

function TSvgZSegLayer.TestUpdateBtnGroup(movedBtn: TLayer32;
  var moveDx, moveDy: integer): Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

procedure TSvgZSegLayer.UpdateBtnGroup(movedBtn: TLayer32);
begin
end;
//------------------------------------------------------------------------------

function  TSvgZSegLayer.CreateRotateBtnGroup(target: TSegBaseLayer;
  startAngle: double; angleOffset: double): TRotatingGroupLayer32;
begin
  Result := nil;
end;
//------------------------------------------------------------------------------

procedure TSvgZSegLayer.UpdateRotateBtnGroup(movedBtn: TLayer32);
begin
end;
//------------------------------------------------------------------------------

procedure TSvgZSegLayer.BtnMoveCheck(const pos: TPointD);
begin
  with TSvgSubPathLayer(Parent).fSubPath do
    Paths := Img32.Vector.Paths([GetFirstPt, GetLastPt]);
  DrawPath;
end;

//------------------------------------------------------------------------------
// TSvgSubPathLayer
//------------------------------------------------------------------------------

procedure TSvgSubPathLayer.Init(subPath: TSvgSubPath);
var
  i: integer;
  seg: TSegBaseLayer;
begin
  fOwner    := Parent as TSvgPathLayer;
  fSubPath  := subPath;
  for i := 0 to subPath.Count -1 do
  begin
    case subPath[i].segType of
      dsLine    : seg := AddChild(TSvgLSegLayer,'L') as TSegBaseLayer;
      dsHorz    : seg := AddChild(TSvgHSegLayer,'H') as TSegBaseLayer;
      dsVert    : seg := AddChild(TSvgVSegLayer,'V') as TSegBaseLayer;
      dsArc     : seg := AddChild(TSvgASegLayer,'A') as TSegBaseLayer;
      dsQBez    : seg := AddChild(TSvgQSegLayer,'Q') as TSegBaseLayer;
      dsCBez    : seg := AddChild(TSvgCSegLayer,'C') as TSegBaseLayer;
      dsQSpline : seg := AddChild(TSvgTSegLayer,'T') as TSegBaseLayer;
      dsCSpline : seg := AddChild(TSvgSSegLayer,'S') as TSegBaseLayer;
      else Continue;
    end;
    seg.Init(subPath[i]);
  end;
  if fSubPath.isClosed then
  begin
    seg := AddChild(TSvgZSegLayer,'Z') as TSegBaseLayer;
    seg.Init(nil);
  end;
end;
//------------------------------------------------------------------------------

procedure  TSvgSubPathLayer.Offset(dx, dy: integer);
begin
  inherited;
  //moving a segment must also involve the whole path
  fSubPath.Offset(dx, dy);
end;
//------------------------------------------------------------------------------

function TSvgSubPathLayer.GetStringDef(decimalPrec: integer): string;
begin
  Result := fSubPath.GetStringDef(true, decimalPrec);
end;
//------------------------------------------------------------------------------

function TSvgSubPathLayer.GetSegLayer(index: integer): TSegBaseLayer;
begin
  Result := Child[index] as TSegBaseLayer;
end;
//------------------------------------------------------------------------------

function TSvgSubPathLayer.GetLastSegLayer: TSegBaseLayer;
var
  i: integer;
begin
  i := ChildCount -1;
  while (i >= 0) and not
    (Child[i] is TSegBaseLayer) do dec(i);
  if i < 0 then Result := nil
  else Result := TSegBaseLayer(Child[i]);
end;

//------------------------------------------------------------------------------
// TSvgPathLayer
//------------------------------------------------------------------------------

constructor TSvgPathLayer.Create(parent: TLayer32;  const name: string = '');
begin
  inherited;
  fSvgPath := TSvgPath.Create;
  fStrokeWidth  := DPIAware(5);
  fStrokeColor  := clBlack32;
  fStrokeColor2 := $FF990000;
  fRelativeStr  := true;
end;
//------------------------------------------------------------------------------

destructor TSvgPathLayer.Destroy;
begin
  fSvgPath.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgPathLayer.LoadPath(const dpath: string;
  const destRect: TRect; scale: double);
var
  i, w,h  : integer;
  subPath : TSvgSubPathLayer;
  srcRect : TRect;
begin
  ClearChildren;
  if IsEmptyRect(destRect) then Exit;

  svgPath.Parse(UTF8String(dpath));
  srcRect := Rect(Img32.Vector.UnionRect(svgPath.Bounds, svgPath.CtrlBounds));

  if scale > 0 then
    //do nothing
  else if not srcRect.IsEmpty then
  begin
    w := RectWidth(destRect);
    h := RectHeight(destRect);
    scale := Min(w/RectWidth(srcRect), h/RectHeight(srcRect));
  end else
    scale := 1;

  svgPath.ScaleAndOffset(scale,
    destRect.Left + Round(-srcRect.Left *scale),
    destRect.Top + Round(-srcRect.top*scale));

  with svgPath do
    for i := 0 to Count -1 do
    begin
      subPath := AddChild(TSvgSubPathLayer) as TSvgSubPathLayer;
      subPath.Init(svgPath[i]);
    end;
end;
//------------------------------------------------------------------------------

function TSvgPathLayer.GetMargin: integer;
begin
  Result := Ceil(fstrokeWidth / 2);
end;
//------------------------------------------------------------------------------

procedure TSvgPathLayer.SetStrokeWidth(width: double);
begin
  fstrokeWidth := Max(1.0, width);
end;
//------------------------------------------------------------------------------

function TSvgPathLayer.GetSubPathLayer(index: integer): TSvgSubPathLayer;
begin
  Result := Child[index] as TSvgSubPathLayer;
end;
//------------------------------------------------------------------------------

end.
