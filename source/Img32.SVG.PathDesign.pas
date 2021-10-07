unit Img32.SVG.PathDesign;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.4                                                             *
* Date      :  5 October 2021                                                  *
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
    fOwner        : TSvgPathLayer;
    fFocused      : Boolean;
    fCtrlPts      : TPathD;
    fRotAngle     : double;
    fHighlightF   : Boolean;
    fHighlightL   : Boolean;
  protected
    function  GetPrevSegLayer: TSegBaseLayer;
    function  GetNextSegLayer: TSegBaseLayer;
    function  GetDesignerLayer(btnLayer: TLayer32): TDesignerLayer32;
    procedure DrawDesigner(designer: TDesignerLayer32); virtual;
    procedure SetFocus(value: Boolean);
    function  GetBtnCtrlPts: TPathD; virtual;
    procedure SetBtnCtrlPts(const pts: TPathD); virtual;
    procedure FlattenPath; virtual;
    function  AsIntStr(val: double): string;
    function  AsFloatStr(val: double;
      precision: integer; useComma: Boolean): string;
    function  AsPointStr(pt: TPointD;
      const relPt: TPointD; precision: integer): string;
    property  Owner  : TSvgPathLayer read fOwner;
  public
    constructor Create(parent: TLayer32;  const name: string = ''); override;
    destructor  Destroy; override;
    procedure Init(seg: TSvgBaseSeg); virtual;
    procedure ScaleAndOffset(scale: double; dx,dy: integer); virtual;
    procedure Offset(dx,dy: integer); override;
    function  CreateBtnGroup: TButtonGroupLayer32; virtual;
    procedure UpdateBtnGroup(movedBtn: TLayer32); virtual;
    function  CreateRotateBtnGroup(target: TSegBaseLayer): TRotatingGroupLayer32; virtual;
    procedure UpdateRotateBtnGroup(movedBtn: TLayer32); virtual;
    function  TestUpdateBtnGroup(movedBtn: TLayer32; var moveDx, moveDy: integer): Boolean; virtual;
    procedure DrawFirstHighLight; virtual;
    procedure DrawPath;
    procedure DrawLastHighlight; virtual;
    function  Detach: Boolean; virtual;
    function  GetStringDef: string; virtual;

    property  Focused: Boolean read fFocused write SetFocus;
    property  HighlightFirst: Boolean read fHighlightF write fHighlightF;
    property  HighlightLast: Boolean read fHighlightL write fHighlightL;
  end;

  TSvgASegLayer = class(TSegBaseLayer)
  private
    //note: While SVG 'A' paths can define multiple arcs, these are
    //      separated into consecutive TSvgASegLayer objects
    fArcInfo  : TArcInfo;
    fHorzLeft : Boolean;
    fVertTop  : Boolean;
    procedure SetArcInfo(const arcInfo: TArcInfo);
    procedure GetRectSizePositions(out pt1, pt2: TPointD);
  protected
    procedure SetBtnCtrlPts(const pts: TPathD); override;
    function GetBtnCtrlPts: TPathD; override;
    function GetHorzSizeBtnPos: TPointD;
    function GetVertSizeBtnPos: TPointD;
    function GetStartAngleBtnPos: TPointD;
    function GetEndAngleBtnPos: TPointD;
    procedure DrawDesigner(designer: TDesignerLayer32); override;
    procedure FlattenPath; override;
  public
    procedure Init(seg: TSvgBaseSeg; arcIdx: integer); reintroduce;
    procedure ScaleAndOffset(scale: double; dx,dy: integer); override;
    function CreateBtnGroup: TButtonGroupLayer32; override;
    function TestUpdateBtnGroup(movedBtn: TLayer32;
      var moveDx, moveDy: integer): Boolean; override; //todo - redo this with absolute points
    procedure UpdateBtnGroup(movedBtn: TLayer32); override;
    function  CreateRotateBtnGroup(target: TSegBaseLayer):
      TRotatingGroupLayer32; override;
    procedure UpdateRotateBtnGroup(movedBtn: TLayer32); override;
    procedure ReverseArcDirection;
    procedure Offset(dx,dy: integer); override;
    function  GetStringDef: string; override;
    property ArcInfo: TArcInfo read fArcInfo write SetArcInfo;
  end;

  TSvgCSegLayer = class(TSegBaseLayer)
  protected
    procedure DrawDesigner(designer: TDesignerLayer32); override;
    procedure FlattenPath; override;
  public
    function  GetStringDef: string; override;
    function CreateBtnGroup: TButtonGroupLayer32; override;
  end;

  TSvgHSegLayer = class(TSegBaseLayer)
  public
    function GetStringDef: string; override;
  end;

  TSvgLSegLayer = class(TSegBaseLayer)
  public
    function  GetStringDef: string; override;
  end;

  TSvgQSegLayer = class(TSegBaseLayer)
  protected
    procedure FlattenPath; override;
  public
    function GetStringDef: string; override;
    function CreateBtnGroup: TButtonGroupLayer32; override;
    procedure DrawDesigner(designer: TDesignerLayer32); override;
  end;

  TSvgSSegLayer = class(TSegBaseLayer)
  protected
    procedure FlattenPath; override;
  public
    procedure Init(seg: TSvgBaseSeg); override;
    function GetStringDef: string; override;
    function CreateBtnGroup: TButtonGroupLayer32; override;
    procedure UpdateBtnGroup(movedBtn: TLayer32); override;
    procedure DrawDesigner(designer: TDesignerLayer32); override;
  end;

  TSvgTSegLayer = class(TSegBaseLayer)
  protected
    procedure FlattenPath; override;
  public
    function GetStringDef: string; override;
    procedure DrawDesigner(designer: TDesignerLayer32); override;
  end;

  TSvgVSegLayer = class(TSegBaseLayer)
  public
    function GetStringDef: string; override;
  end;

  TSvgSubPathLayer = class(TGroupLayer32)
  private
    fOwner        : TSvgPathLayer;
    fSubPath      : TSvgSubPath;
    fStartPos     : TPointD;
    function GetSegLayer(index: integer): TSegBaseLayer;
  protected
    property Owner : TSvgPathLayer read fOwner;
  public
    procedure Init(subPath: TSvgSubPath); virtual;
    property SubPath: TSvgSubPath read fSubPath;
    property SegLayer[index: integer]: TSegBaseLayer read GetSegLayer;
  end;

  TSvgPathLayer = class(TGroupLayer32)
  private
    fScale        : double;
    fStrokeColor  : TColor32;
    fStrokeColor2 : TColor32;
    fstrokeWidth  : double;
    fOffset       : TPoint;
    fRelativeStr  : Boolean;
    fSvgPath      : TSvgPath;
    function GetMargin: integer;
    procedure SetStrokeWidth(width: double);
    function GetSubPathLayer(index: integer): TSvgSubPathLayer;
  protected
    property Scale        : double read fScale;
    property PathOffset   : TPoint read fOffset;
    property StrokeColor  : TColor32 read fStrokeColor;
    property StrokeColor2 : TColor32 read fStrokeColor2;
  public
    procedure Init(svgPath: TSvgPath; scale: double; const offset: TPoint); virtual;
    procedure ScaleAndOffset(scale: double; dx, dy: integer);

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
  ArcRectMargin = 20;
  controlBtnColor   : TColor32 = $FFFFBB00;
  arcRectBtnColor   : TColor32 = clGreen32;
  rotatePivotColor  : TColor32 = clLime32;
  rotateBtnColor    : TColor32 = clGreen32;


//------------------------------------------------------------------------------

function ScaleAndOffsetPath(const p: TPathD;
  scale: double; dx,dy: integer): TPathD;
begin
  result := ScalePath(p, scale);
  result := OffsetPath(result, dx, dy);
end;
//------------------------------------------------------------------------------

function ScaleAndOffsetRect(const rec: TRectD;
  scale: double; dx,dy: integer): TRectD;
begin
  result := ScaleRect(rec, scale);
  OffsetRect(result, dx, dy);
end;

//------------------------------------------------------------------------------
// TSegBaseLayer
//------------------------------------------------------------------------------

constructor TSegBaseLayer.Create(parent: TLayer32;  const name: string = '');
begin
  inherited;
  fOwner := Parent.Parent as TSvgPathLayer;
  Margin := Max(fOwner.Margin, Ceil(DefaultButtonSize /2));
  fHighlightF := false;
  fHighlightL := true;
end;
//------------------------------------------------------------------------------

destructor TSegBaseLayer.Destroy;
var
  prevLayer: TLayer32;
begin
  prevLayer := PrevLayerInGroup;
  if Assigned(prevLayer) then
  begin
    if fHighlightF then
      TSegBaseLayer(prevLayer).DrawFirstHighLight;
    if fHighlightL then
      TSegBaseLayer(prevLayer).DrawLastHighlight;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.Init(seg: TSvgBaseSeg);
var
  p: TPathD;
begin
  p := ScaleAndOffsetPath(seg.ctrlPts,
    Owner.Scale, Owner.fOffset.X, Owner.fOffset.Y);
  SetBtnCtrlPts(p);
  DrawPath;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.ScaleAndOffset(scale: double; dx,dy: integer);
begin
  if scale = 0 then scale := 1;
  fCtrlPts := ScaleAndOffsetPath(fCtrlPts, scale, dx,dy);
  FlattenPath;
  DrawPath;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.Offset(dx,dy: integer);
begin
  inherited;
  fCtrlPts := OffsetPath(fCtrlPts, dx, dy);
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.DrawPath;
var
  c: TColor32;
  p: TPathD;
begin
  if Length(Paths[0]) = 0 then Exit;
  Image.Clear; //Image.Clear($10FF0000);
  if Focused then c := Owner.fStrokeColor2 else c := Owner.fStrokeColor;
  p := OffsetPath(Paths[0], -Left, -Top);
  DrawLine(Image, p, Owner.StrokeWidth, c, esRound);

  UpdateHitTestMaskFromImage;
  //widen the HT region
  DrawLine(HitTestRec.htImage, p, Owner.StrokeWidth*2, clBlack32, esRound);
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.DrawFirstHighLight;
var
  pt: TPointD;
  p: TPathD;
begin
  if not Assigned(fCtrlPts) then Exit;
  pt := fCtrlPts[0];
  pt := OffsetPoint(pt, fOwner.fOffset.X -Left, fOwner.fOffset.Y -Top);
  Img32.Extra.DrawButton(Image, pt,
    DefaultButtonSize, $FFFF9999, bsRound, [ba3D, baShadow]);

  UpdateHitTestMaskFromImage;
  //widen the HT region
  p := OffsetPath(Paths[0], -Left, -Top);
  DrawLine(HitTestRec.htImage, p, Owner.StrokeWidth*2, clBlack32, esRound);
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.DrawLastHighlight;
var
  pt: TPointD;
  p: TPathD;
begin
  if not Assigned(fCtrlPts) then Exit;
  pt := fCtrlPts[High(fCtrlPts)];
  pt := OffsetPoint(pt, -Left, -Top);
  Img32.Extra.DrawButton(Image,
    pt, DefaultButtonSize, $FF9999FF, bsRound, [ba3D, baShadow, baEraseBeneath]);

  UpdateHitTestMaskFromImage;
  //widen the HT region
  p := OffsetPath(Paths[0], -Left, -Top);
  DrawLine(HitTestRec.htImage, p, Owner.StrokeWidth*2, clBlack32, esRound);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.GetBtnCtrlPts: TPathD;
begin
  Result := fCtrlPts;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.SetBtnCtrlPts(const pts: TPathD);
begin
  fCtrlPts := pts;
  FlattenPath;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.FlattenPath;
begin
  Paths := Img32.Vector.Paths(fCtrlPts);
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
  i: integer;
  designer: TDesignerLayer32;
begin
  Result := CreateButtonGroup(Root,
    GetBtnCtrlPts, bsRound, DefaultButtonSize, clRed32);
  if (GetPrevSegLayer <> nil) then
    TButtonDesignerLayer32(Result[0]).Enabled := false;

  if (GetNextSegLayer <> nil) then
    for i := 0 to Result.ChildCount -1 do
      TButtonDesignerLayer32(Result[i]).Enabled := false;

  //insert the designer layer **below** the button layers
  designer := Result.InsertChild(TDesignerLayer32, 0) as TDesignerLayer32;
  DrawDesigner(designer);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.TestUpdateBtnGroup(movedBtn: TLayer32;
  var moveDx, moveDy: integer): Boolean;
begin
  Result := (movedBtn is TButtonDesignerLayer32) or
    not TButtonDesignerLayer32(movedBtn).Enabled;
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.UpdateBtnGroup(movedBtn: TLayer32);
var
  i         : integer;
  pts       : TPathD;
  designer  : TDesignerLayer32;
begin
  designer := GetDesignerLayer(movedBtn);
  if not assigned(designer)  then Exit;
  i := TButtonDesignerLayer32(movedBtn).BtnIdx;
  pts := GetBtnCtrlPts;
  pts[i] := movedBtn.MidPoint;
  SetBtnCtrlPts(pts);
  DrawPath;
  DrawDesigner(designer);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.CreateRotateBtnGroup(target: TSegBaseLayer): TRotatingGroupLayer32;
begin
  if (GetNextSegLayer <> nil) then
    Raise Exception.Create(rsErrorCreatingButtonGroup);
  //get control points stripped of local coord reference

  fRotAngle := 0;
  Result := CreateRotatingButtonGroup(target,
    target.fCtrlPts[0], DefaultButtonSize, rotatePivotColor, rotateBtnColor);
end;
//------------------------------------------------------------------------------

procedure TSegBaseLayer.UpdateRotateBtnGroup(movedBtn: TLayer32);
var
  a: double;
begin
  if not (movedBtn.Parent is TRotatingGroupLayer32) then Exit;
  a := UpdateRotatingButtonGroup(movedBtn);
  fCtrlPts := RotatePath(fCtrlPts, fCtrlPts[0], a - fRotAngle);
  fRotAngle := a;
  FlattenPath;
  DrawPath;
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.Detach: Boolean;
var
  i: integer;
  oldSubPath, newSubPath: TSvgSubPathLayer;
begin
  Result := Index > 0;
  if not Result then Exit;
  oldSubPath := Parent as TSvgSubPathLayer;
  newSubPath := Owner.AddChild(TSvgSubPathLayer) as TSvgSubPathLayer;
  for i := Index to oldSubPath.ChildCount -1 do
    oldSubPath[i].Move(newSubPath, MaxInt);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.GetStringDef: string;
begin
  Result := name + ' ';
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.AsIntStr(val: double): string;
begin
  Result := Format('%1.0n ', [val]);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.AsFloatStr(val: double;
  precision: integer; useComma: Boolean): string;
var
  ext: string;
begin
  if useComma then ext := ',' else ext := '';
  Result := Format('%1.*f%s ', [precision, val, ext]);
end;
//------------------------------------------------------------------------------

function TSegBaseLayer.AsPointStr(pt: TPointD;
  const relPt: TPointD; precision: integer): string;
var
  delta: TPointD;
begin
  with fOwner do
  begin
    pt.x := (pt.x - fOffset.X) /Scale;
    pt.y := (pt.y - fOffset.Y) /Scale;
    if fRelativeStr then
    begin
      delta.X := (relPt.X - fOffset.X) /Scale;
      delta.Y := (relPt.Y - fOffset.Y) /Scale;
      pt := OffsetPoint(pt, -delta.X, -delta.Y);
    end;
  end;
  Result := Format('%1.*f %1.*f, ', [precision, pt.x, precision, pt.y]);
end;

//------------------------------------------------------------------------------
// TSvgASegLayer
//------------------------------------------------------------------------------

procedure TSvgASegLayer.Init(seg: TSvgBaseSeg; arcIdx: integer);
begin
  with TSvgASegment(seg) do
    fArcInfo := arcInfos[arcIdx];
  fArcInfo.rec := ScaleAndOffsetRect(fArcInfo.rec,
    Owner.Scale, Owner.fOffset.X, Owner.fOffset.Y);

  SetBtnCtrlPts(nil);
  DrawPath;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.ScaleAndOffset(scale: double; dx,dy: integer);
begin
  if scale = 0 then scale := 1;
  fCtrlPts := ScaleAndOffsetPath(fCtrlPts, scale, dx,dy);
  fArcInfo.rec := ScaleAndOffsetRect(fArcInfo.rec, scale, dx,dy);
  FlattenPath;
  DrawPath;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.SetArcInfo(const arcInfo: TArcInfo);
begin
  fArcInfo := arcInfo;
  SetBtnCtrlPts(nil);
  DrawPath;
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.GetRectSizePositions(out pt1, pt2: TPointD);
var
  r: TRectD;
  startPt, pt: TPointD;
begin
  r := fArcInfo.rec;
  startPt := GetPtOnEllipseFromAngle(r, fArcInfo.startAngle);
  pt := PointD(r.Left, r.MidPoint.Y);
  pt2 := PointD(r.Right, r.MidPoint.Y);
  fHorzLeft := DistanceSqrd(pt, startPt) > DistanceSqrd(pt2, startPt);
  if fHorzLeft then
    pt1 := PointD(r.Left - ArcRectMargin, r.MidPoint.Y) else
    pt1 := PointD(r.Right + ArcRectMargin, r.MidPoint.Y);

  pt := PointD(r.MidPoint.X, r.Top);
  pt2 := PointD(r.MidPoint.X, r.Bottom);
  fVertTop := DistanceSqrd(pt, startPt) > DistanceSqrd(pt2, startPt);
  if fVertTop then
    pt2 := PointD(r.MidPoint.X, r.Top - ArcRectMargin) else
    pt2 := PointD(r.MidPoint.X, r.Bottom + ArcRectMargin);
  RotatePoint(pt1, r.MidPoint, fArcInfo.angle);
  RotatePoint(pt2, r.MidPoint, fArcInfo.angle);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.ReverseArcDirection;
begin
  fArcInfo.sweepFlag := not fArcInfo.sweepFlag;
  SetBtnCtrlPts(nil);
  DrawPath;
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.Offset(dx,dy: integer);
begin
  inherited;
  OffsetRect(fArcInfo.rec, dx, dy);
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetStringDef: string;
var
  a: double;
  pt: TPointD;
begin
  with fArcInfo do
  begin
    //descaled radii
    if Owner.fRelativeStr then Result := 'a ' else Result := 'A ';

    Result := Result + AsFloatStr(rec.Width/(Owner.Scale *2), 2, true);
    Result := Result + AsFloatStr(rec.Height/(Owner.Scale *2), 2, false);
    //angle as degrees
    Result := Result + AsIntStr(RadToDeg(angle));

    //large arce and direction flags
    if fArcInfo.endAngle < fArcInfo.startAngle then
      a := endAngle + angle360 - startAngle else
      a := endAngle - startAngle;
    if sweepFlag then
    begin
      if a  > angle180 then
        Result := Result + '1 1 ' else
        Result := Result + '0 1 ';
    end else
    begin
      if a > angle180 then
        Result := Result + '0 0 ' else
        Result := Result + '1 0 ';
    end;
    //descaled and de-offset end position
    pt := GetPtOnEllipseFromAngle(rec, endAngle);

    if Owner.fRelativeStr then
      Result := Result + AsPointStr(pt, fCtrlPts[0], 2) else
      Result := Result + AsPointStr(pt, NullPointD, 2);

  end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetStartAngleBtnPos: TPointD;
begin
  with fArcInfo do
    Result := GetPtOnRotatedEllipseFromAngle(rec, angle, startAngle);
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetEndAngleBtnPos: TPointD;
begin
  with fArcInfo do
    Result := GetPtOnRotatedEllipseFromAngle(rec, angle, endAngle);
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetHorzSizeBtnPos: TPointD;
begin
  with fArcInfo do
  begin
    if fHorzLeft then
      Result := PointD(rec.Left - ArcRectMargin, rec.MidPoint.Y) else
      Result := PointD(rec.Right + ArcRectMargin, rec.MidPoint.Y);
    if angle <> 0 then RotatePoint(Result, rec.MidPoint, angle);
  end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetVertSizeBtnPos: TPointD;
begin
  with fArcInfo do
  begin
    if fVertTop then
      Result := PointD(rec.MidPoint.X, rec.Top - ArcRectMargin) else
      Result := PointD(rec.MidPoint.X, rec.Bottom + ArcRectMargin);
    if angle <> 0 then RotatePoint(Result, rec.MidPoint, angle);
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.SetBtnCtrlPts(const pts: TPathD);
begin
  fCtrlPts := GetBtnCtrlPts;
  FlattenPath;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetBtnCtrlPts: TPathD;
begin
  SetLength(Result, 4);
  with fArcInfo do
  begin
    Result[0] := GetStartAngleBtnPos;
    GetRectSizePositions(Result[1], Result[2]);
    Result[3] := GetEndAngleBtnPos;
  end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  designer: TDesignerLayer32;
begin
  Result := CreateButtonGroup(Root, GetBtnCtrlPts,
    bsRound, DefaultButtonSize, clRed32);

  if (GetNextSegLayer <> nil) then
  begin
    TButtonDesignerLayer32(Result[0]).Enabled := false;
    TButtonDesignerLayer32(Result[1]).Visible := false;
    TButtonDesignerLayer32(Result[2]).Visible := false;
    TButtonDesignerLayer32(Result[3]).Enabled := false;
  end
  else if (GetPrevSegLayer <> nil) then
    TButtonDesignerLayer32(Result[0]).Enabled := false;

  //insert the designer layer below the button layers (ie level 0)
  designer := Result.InsertChild(TDesignerLayer32, 0) as TDesignerLayer32;

  if (GetNextSegLayer <> nil) then Exit;

  with TButtonDesignerLayer32(Result[2]) do   //rect horz
  begin
    Color := arcRectBtnColor;
    Enabled := true;
    Draw;
  end;
  with TButtonDesignerLayer32(Result[3]) do   //rect vert
  begin
    Color := arcRectBtnColor;
    Draw;
  end;
  DrawDesigner(designer);
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.TestUpdateBtnGroup(movedBtn: TLayer32;
  var moveDx, moveDy: integer): Boolean;
var
  a: double;
  r: TRectD;
  pt, mp: TPointD;
begin
  Result := inherited TestUpdateBtnGroup(movedBtn, moveDx, moveDy);
  if not Result then Exit;

  r := ArcInfo.rec;
  mp := r.MidPoint;
  pt := PointD(movedBtn.MidPoint.X + moveDx, movedBtn.MidPoint.Y + moveDy);
  img32.Vector.RotatePoint(pt, mp, -fArcInfo.angle);
  //see GetBtnCtrlPts() for button order
  case TButtonDesignerLayer32(movedBtn).BtnIdx of
    1:                                //rect horz
      begin
        pt.Y := mp.Y;
        if fHorzLeft then
          pt.X := min(pt.X, mp.X -2 - ArcRectMargin) else
          pt.X := max(pt.X, mp.X +2 + ArcRectMargin);
      end;
    2:                                //rect top
      begin
        pt.X := mp.X;
        if fVertTop then
          pt.Y := min(pt.Y, mp.Y -2 - ArcRectMargin) else
          pt.Y := max(pt.Y, mp.Y +2 + ArcRectMargin);
      end;
    3:                                //end point
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
  img32.Vector.RotatePoint(pt, mp, fArcInfo.angle);
  moveDx := Round(pt.X - movedBtn.MidPoint.X);
  moveDy := Round(pt.Y - movedBtn.MidPoint.Y);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.UpdateBtnGroup(movedBtn: TLayer32);
var
  d,dx,dy: double;
  r: TRectD;
  mp, mp2, sp, sp2: TPointD;
  designer  : TDesignerLayer32;
begin
  designer := GetDesignerLayer(movedBtn);
  if not Assigned(designer) then Exit;

  r := fArcInfo.rec;
  mp := r.MidPoint;
  mp2 := movedBtn.MidPoint;
  img32.Vector.RotatePoint(mp2, mp, - fArcInfo.angle);

  case TButtonDesignerLayer32(movedBtn).BtnIdx of
    1:                                //rect horz
      begin
        if fHorzLeft then
          d := (mp2.X + ArcRectMargin - r.Left) else
          d := (r.Right + ArcRectMargin - mp2.X);
        if d = 0 then Exit;

        //save the start point before resizing rect
        //then after resizing, realign the rect so
        //the start point doesn't change
        sp  := Paths[0][0];
        fArcInfo.rec.Left := fArcInfo.rec.Left + d;
        fArcInfo.rec.Right := fArcInfo.rec.Right - d;
        sp2 := GetStartAngleBtnPos;
        dx := sp.X - sp2.X; dy := sp.Y - sp2.Y;
        OffsetRect(fArcInfo.rec, dx, dy);
        movedBtn.Offset(Round(dx), Round(dy));

        FlattenPath;
        DrawPath;
        DrawDesigner(designer);
        movedBtn.Parent[3].PositionCenteredAt(GetVertSizeBtnPos);
        movedBtn.Parent[4].PositionCenteredAt(GetEndAngleBtnPos);
      end;
    2:                                //rect vert
      begin
        if fVertTop then
          d := (mp2.Y + ArcRectMargin - r.Top) else
          d := (r.Bottom + ArcRectMargin - mp2.Y);
        if d = 0 then Exit;

        sp  := Paths[0][0];
        fArcInfo.rec.Top := fArcInfo.rec.Top + d;
        fArcInfo.rec.Bottom := fArcInfo.rec.Bottom - d;
        sp2 := GetStartAngleBtnPos;
        dx := sp.X - sp2.X; dy := sp.Y - sp2.Y;
        OffsetRect(fArcInfo.rec, dx, dy);
        movedBtn.Offset(Round(dx),Round(dy));

        FlattenPath;
        DrawPath;
        DrawDesigner(designer);
        movedBtn.Parent[2].PositionCenteredAt(GetHorzSizeBtnPos);
        movedBtn.Parent[4].PositionCenteredAt(GetEndAngleBtnPos);
      end;
    3:
      begin
        fArcInfo.endAngle := GetEllipticalAngleFromPoint(r, mp2);
        FlattenPath;
        DrawPath;
      end;
  end;
  fCtrlPts[High(fCtrlPts)] := Paths[0][High(Paths[0])];
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.CreateRotateBtnGroup(target: TSegBaseLayer): TRotatingGroupLayer32;
begin
  Result := inherited CreateRotateBtnGroup(target);
  fRotAngle := fArcInfo.angle;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.UpdateRotateBtnGroup(movedBtn: TLayer32);
var
  a, dx, dy: double;
  sp,sp2: TPointD;
begin
  if not (movedBtn.Parent is TRotatingGroupLayer32) then Exit;
  a := UpdateRotatingButtonGroup(movedBtn);
  if a = 0 then Exit;

  //save the start point before rotating rect
  //then after rotating, realign the rect so
  //the start point doesn't change
  sp  := Paths[0][0];
  fArcInfo.angle := fRotAngle + a;
  sp2 := GetStartAngleBtnPos;
  dx := sp.X - sp2.X; dy := sp.Y - sp2.Y;
  OffsetRect(fArcInfo.rec, dx, dy);

  FlattenPath;
  DrawPath;
  Invalidate(Bounds);
  fCtrlPts[High(fCtrlPts)] := Paths[0][High(Paths[0])];
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.FlattenPath;
var
  p: TPathD;
begin
  with fArcInfo do
  begin
    if not sweepFlag then
      p := Arc(rec, endAngle, startAngle) else
      p := Arc(rec, startAngle, endAngle);
    if angle <> 0 then
      p := RotatePath(p, rec.MidPoint, angle);
  end;
  Paths := Img32.Vector.Paths(p);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.DrawDesigner(designer: TDesignerLayer32);
var
  j: integer;
  r,r2: TRectD;
  p: TPathD;
begin
  if not Assigned(designer) then Exit;
  j := Ceil(Owner.StrokeWidth);
  r := fArcInfo.rec;

  r2 := r;
  InflateRect(r2, dpiAware1, dpiAware1);
  if fArcInfo.angle = 0 then
  begin
    designer.SetBounds(Rect(r2));
    OffsetRect(r, - designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, Rectangle(r), [j,j],
      nil, dpiAware1, clMaroon32, esPolygon);
  end else
  begin
    p := Rectangle(r2);
    p := RotatePath(p, r2.MidPoint, fArcInfo.angle);
    designer.SetBounds(Rect(Img32.Vector.GetBoundsD(p)));
    p := Rectangle(r);
    p := RotatePath(p, r2.MidPoint, fArcInfo.angle);
    p := OffsetPath(p, - designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, p, [j,j],
      nil, dpiAware1, clMaroon32, esPolygon);
  end;
end;

//------------------------------------------------------------------------------
// TSvgCSegLayer
//------------------------------------------------------------------------------

function TSvgCSegLayer.GetStringDef: string;
var
  i: integer;
  relPt: TPointD;
begin
  if Owner.fRelativeStr then
  begin
    Result := 'c ';
    relPt := fCtrlPts[0];
    for i := 1 to High(fCtrlPts) do
    begin
      Result := Result + AsPointStr(fCtrlPts[i], relPt, 2);
      if (i mod 3) = 0 then relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'C ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + AsPointStr(fCtrlPts[i], NullPointD, 2);
  end;
end;
//------------------------------------------------------------------------------

function TSvgCSegLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  i: integer;
begin
  Result := inherited CreateBtnGroup;
  //make sure 'control' buttons are on top of adjacent 'end' buttons
  //move every second ctrl button on top of the following button
  //nb: the first layer in this group isn't a button
  for i := 1 to Result.ChildCount div 3 do
  begin
    with TButtonDesignerLayer32(Result[i*3-1]) do
    begin
      Color := controlBtnColor;
      Draw;
    end;
    with TButtonDesignerLayer32(Result[i*3]) do
    begin
      Color := controlBtnColor;
      Draw;
      Move(Result, i*3+1);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgCSegLayer.FlattenPath;
begin
  Paths := Img32.Vector.Paths(FlattenCBezier(fCtrlPts));
end;
//------------------------------------------------------------------------------

procedure TSvgCSegLayer.DrawDesigner(designer: TDesignerLayer32);
var
  i,j: integer;
  p: TPathD;
begin
  if not Assigned(designer) then Exit;
  designer.SetBounds(Img32.Vector.GetBounds(fCtrlPts));
  j := Ceil(Owner.StrokeWidth);
  SetLength(p, 2);
  for i := 0 to Length(fCtrlPts) div 3 -1 do
  begin
    p[0] := fCtrlPts[i*3];
    p[1] := fCtrlPts[i*3+1];
    p := OffsetPath(p, -designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, p, [j,j], nil, Owner.StrokeWidth/2, clRed32, esRound);
    p[0] := fCtrlPts[i*3+2];
    p[1] := fCtrlPts[i*3+3];
    p := OffsetPath(p, -designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, p, [j,j], nil, Owner.StrokeWidth/2, clRed32, esRound);
  end;
end;

//------------------------------------------------------------------------------
// TSvgHSegLayer
//------------------------------------------------------------------------------

function  TSvgHSegLayer.GetStringDef: string;
var
  i: integer;
  dx: double;
begin
  if Owner.fRelativeStr then
  begin
    Result := 'h ';
    for i := 1 to High(fCtrlPts) do
    begin
      dx := (fCtrlPts[i].X - fCtrlPts[i-1].X)/fOwner.Scale;
      Result := Result + AsFloatStr(dx, 2, false);
    end;
  end else
  begin
    Result := 'H ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + AsFloatStr(fCtrlPts[i].X, 2, false);
  end;
end;

//------------------------------------------------------------------------------
// TSvgLSegLayer
//------------------------------------------------------------------------------

function  TSvgLSegLayer.GetStringDef: string;
var
  i: integer;
  relPt: TPointD;
begin
  if Owner.fRelativeStr then
  begin
    relPt := fCtrlPts[0];
    Result := 'l ';
    for i := 1 to High(fCtrlPts) do
    begin
      Result := Result + AsPointStr(fCtrlPts[i], relPt, 2);
      relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'L ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + AsPointStr(fCtrlPts[i], NullPointD, 2);
  end;
end;

//------------------------------------------------------------------------------
// TSvgQSegLayer
//------------------------------------------------------------------------------

function TSvgQSegLayer.GetStringDef: string;
var
  i: integer;
  relPt: TPointD;
begin
  if Owner.fRelativeStr then
  begin
    relPt := fCtrlPts[0];
    Result := 'q ';
    for i := 1 to High(fCtrlPts) do
    begin
      Result := Result + AsPointStr(fCtrlPts[i], relPt, 2);
      if (i mod 2) = 0 then relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'Q ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + AsPointStr(fCtrlPts[i], NullPointD, 2);
  end;
end;
//------------------------------------------------------------------------------

function TSvgQSegLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  i: integer;
begin
  Result := inherited CreateBtnGroup;
  //make sure 'control' buttons are on top of adjacent 'end' buttons
  //move every ctrl button on top of the following button
  //nb: the first layer in this group isn't a button
  for i := 1 to Result.ChildCount div 2 -1 do
    with TButtonDesignerLayer32(Result[i*2]) do
    begin
      Color := controlBtnColor;
      Draw;
      Move(Result, i*2+1);
    end;
end;
//------------------------------------------------------------------------------

procedure TSvgQSegLayer.FlattenPath;
begin
  Paths := Img32.Vector.Paths(FlattenQBezier(fCtrlPts));
end;
//------------------------------------------------------------------------------

procedure TSvgQSegLayer.DrawDesigner(designer: TDesignerLayer32);
var
  i,j: integer;
  p: TPathD;
begin
  if not Assigned(designer) then Exit;
  designer.SetBounds(Img32.Vector.GetBounds(fCtrlPts));
  j := Ceil(Owner.StrokeWidth);
  SetLength(p, 2);
  for i := 0 to Length(fCtrlPts) div 2 -1 do
  begin
    p[0] := fCtrlPts[i*2];
    p[1] := fCtrlPts[i*2+1];
    p := OffsetPath(p, -designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, p, [j,j], nil, Owner.StrokeWidth/2, clRed32, esRound);
    p[0] := fCtrlPts[i*2+1];
    p[1] := fCtrlPts[i*2+2];
    p := OffsetPath(p, -designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, p, [j,j], nil, Owner.StrokeWidth/2, clRed32, esRound);
  end;
end;

//------------------------------------------------------------------------------
// TSvgSSegLayer
//------------------------------------------------------------------------------

procedure TSvgSSegLayer.Init(seg: TSvgBaseSeg);
begin
  inherited;
  if Index = 0 then
  begin
    fCtrlPts[1] := fCtrlPts[0];
    FlattenPath;
    DrawPath;
  end;
end;
//------------------------------------------------------------------------------

function TSvgSSegLayer.GetStringDef: string;
var
  i: integer;
  relPt: TPointD;
begin
  if Owner.fRelativeStr then
  begin
    relPt := fCtrlPts[0];
    Result := 's ';
    for i := 2 to High(fCtrlPts) do
    begin
      Result := Result + AsPointStr(fCtrlPts[i], relPt, 2);
      if (i mod 2 = 1) then relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'S ';
    for i := 2 to High(fCtrlPts) do
      Result := Result + AsPointStr(fCtrlPts[i], NullPointD, 2);
  end;
end;
//------------------------------------------------------------------------------

function TSvgSSegLayer.CreateBtnGroup: TButtonGroupLayer32;
var
  i: integer;
begin
  Result := inherited CreateBtnGroup;
  //nb: don't forget there's a designer layer inside this TButtonGroupLayer32
  with TButtonDesignerLayer32(Result[2]) do
  begin
    Visible := False;
    Move(Result, Index -1);
  end;

  for i := 2 to Result.ChildCount div 2 do
    with TButtonDesignerLayer32(Result[i*2-1]) do
    begin
      Color := controlBtnColor;
      Draw;
      Move(Result, i*2);
    end;
end;
//------------------------------------------------------------------------------

procedure TSvgSSegLayer.UpdateBtnGroup(movedBtn: TLayer32);
begin
  inherited;
  if not (movedBtn is TButtonDesignerLayer32) then Exit;
  if TButtonDesignerLayer32(movedBtn).BtnIdx = 0 then
    movedBtn.Parent[2].PositionAt(movedBtn.Location);
end;
//------------------------------------------------------------------------------

procedure TSvgSSegLayer.FlattenPath;
begin
  Paths := Img32.Vector.Paths(FlattenCSpline(fCtrlPts));
end;
//------------------------------------------------------------------------------

procedure TSvgSSegLayer.DrawDesigner(designer: TDesignerLayer32);
var
  i,j: integer;
  p: TPathD;
begin
  if not Assigned(designer) then Exit;
  designer.SetBounds(Img32.Vector.GetBounds(fCtrlPts));
  j := Ceil(Owner.StrokeWidth);

  SetLength(p, 2);
  for i := 1 to Length(fCtrlPts) div 2 -1 do
  begin
    p[0] := fCtrlPts[i*2];
    p[1] := fCtrlPts[i*2+1];
    p := OffsetPath(p, -designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, p, [j,j], nil, Owner.StrokeWidth/2, clRed32, esRound);
  end;
end;

//------------------------------------------------------------------------------
// TSvgTSegLayer
//------------------------------------------------------------------------------

function  TSvgTSegLayer.GetStringDef: string;
var
  i: integer;
  relPt: TPointD;
begin
  if Owner.fRelativeStr then
  begin
    relPt := fCtrlPts[0];
    Result := 't ';
    for i := 2 to High(fCtrlPts) do
    begin
      Result := Result + AsPointStr(fCtrlPts[i], relPt, 2);
      relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'T ';
    for i := 2 to High(fCtrlPts) do
      Result := Result + AsPointStr(fCtrlPts[i], NullPointD, 2);
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgTSegLayer.FlattenPath;
begin
  Paths := Img32.Vector.Paths(FlattenQSpline(fCtrlPts));
end;
//------------------------------------------------------------------------------

procedure TSvgTSegLayer.DrawDesigner(designer: TDesignerLayer32);
begin
  if not Assigned(designer) then Exit;
  designer.SetBounds(Img32.Vector.GetBounds(fCtrlPts));
end;

//------------------------------------------------------------------------------
// TSvgVSegLayer
//------------------------------------------------------------------------------

function  TSvgVSegLayer.GetStringDef: string;
var
  i: integer;
  dy: double;
begin
  if Owner.fRelativeStr then
  begin
    Result := 'v ';
    for i := 1 to High(fCtrlPts) do
    begin
      dy := (fCtrlPts[i].Y - fCtrlPts[i-1].Y)/fOwner.Scale;
      Result := Result + AsFloatStr(dy, 2, false);
    end;
  end else
  begin
    Result := 'V ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + AsFloatStr(fCtrlPts[i].Y, 2, false);
  end;
end;

//------------------------------------------------------------------------------
// TSvgSubPathLayer
//------------------------------------------------------------------------------

procedure TSvgSubPathLayer.Init(subPath: TSvgSubPath);
var
  i,j: integer;
  seg: TSegBaseLayer;
begin
  fOwner    := Parent as TSvgPathLayer;
  fSubPath  := subPath;
  fStartPos := subPath.firstPt;
  for i := 0 to subPath.Count -1 do
  begin
    case subPath[i].segType of
      dsLine    : seg := AddChild(TSvgLSegLayer,'L') as TSegBaseLayer;
      dsHorz    : seg := AddChild(TSvgHSegLayer,'H') as TSegBaseLayer;
      dsVert    : seg := AddChild(TSvgVSegLayer,'V') as TSegBaseLayer;
      dsArc     :
        begin
          with TSvgASegment(subPath[i]) do
            for j := 0 to High(arcInfos) do
            begin
              seg := AddChild(TSvgASegLayer,'A') as TSegBaseLayer;
              TSvgASegLayer(seg).Init(subPath[i], j);
            end;
          Continue;
        end;
      dsQBez    : seg := AddChild(TSvgQSegLayer,'Q') as TSegBaseLayer;
      dsCBez    : seg := AddChild(TSvgCSegLayer,'C') as TSegBaseLayer;
      dsQSpline : seg := AddChild(TSvgTSegLayer,'T') as TSegBaseLayer;
      dsCSpline : seg := AddChild(TSvgSSegLayer,'S') as TSegBaseLayer;
      else Continue;
    end;
    seg.Init(subPath[i]);
  end;
end;
//------------------------------------------------------------------------------

function TSvgSubPathLayer.GetSegLayer(index: integer): TSegBaseLayer;
begin
  Result := Child[index] as TSegBaseLayer;
end;

//------------------------------------------------------------------------------
// TSvgPathLayer
//------------------------------------------------------------------------------

procedure TSvgPathLayer.Init(svgPath: TSvgPath;
  scale: double; const offset: TPoint);
var
  i       : integer;
  subPath : TSvgSubPathLayer;
begin
  if not Assigned(svgPath) then Exit;

  fSvgPath      := svgPath;
  fScale        := scale;
  fOffset       := offset;
  fStrokeWidth  := DPIAware(5);
  fStrokeColor  := clBlack32;
  fStrokeColor2 := $FFCC0000;
  fRelativeStr  := true;
  subPath       := nil;
  with svgPath do
    for i := 0 to Count -1 do
    begin
      subPath := AddChild(TSvgSubPathLayer) as TSvgSubPathLayer;
      subPath.Init(svgPath[i]);
    end;
  if Assigned(subPath) and (subPath.ChildCount > 0)  then
  begin
    with TSegBaseLayer(subPath[subPath.ChildCount -1]) do
      if fHighlightF then DrawFirstHighLight;
    with TSegBaseLayer(subPath[subPath.ChildCount -1]) do
      if fHighlightL then DrawLastHighlight;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgPathLayer.ScaleAndOffset(scale: double; dx, dy: integer);
var
  i,j: integer;
begin
  for i := 0 to ChildCount -1 do
    if Child[i] is TSvgSubPathLayer then
        for j := 0 to Child[i].ChildCount -1 do
          if Child[i][j] is TSegBaseLayer then
            TSegBaseLayer(Child[i][j]).ScaleAndOffset(scale, dx, dy);
  fScale := fScale * scale;
  fOffset.Offset(dx, dy);
  Invalidate(Image.Bounds);
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
