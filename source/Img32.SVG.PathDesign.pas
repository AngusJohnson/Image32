unit Img32.SVG.PathDesign;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.4                                                             *
* Date      :  12 October 2021                                                 *
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
  windows, //debugging

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
    procedure GetRectBtnPoints(out pt1, pt2: TPointD);
  protected
    procedure SetBtnCtrlPts(const pts: TPathD); override;
    function GetBtnCtrlPts: TPathD; override;
    function GetStartAngle: double;
    function GetEndAngle: double;
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
    function CreateBtnGroup: TButtonGroupLayer32; override;
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
    function GetLastSegLayer: TSegBaseLayer;
    function GetStringDef: string;
    property SubPath: TSvgSubPath read fSubPath;
    property SegLayer[index: integer]: TSegBaseLayer read GetSegLayer;
  end;

  TSvgPathLayer = class(TGroupLayer32)
  private
    fScale        : double;
    fStrokeColor  : TColor32;
    fStrokeColor2 : TColor32;
    fstrokeWidth  : double;
    fRelativeStr  : Boolean;
    fSvgPath      : TSvgPath;
    function GetMargin: integer;
    procedure SetStrokeWidth(width: double);
    function GetSubPathLayer(index: integer): TSvgSubPathLayer;
  protected
    function AsPointStr(pt: TPointD;
      const relPt: TPointD; precision: integer): string;
    property StrokeColor  : TColor32 read fStrokeColor;
    property StrokeColor2 : TColor32 read fStrokeColor2;
  public
    procedure Init(svgPath: TSvgPath; scale: double;
      offsetX, offsetY: integer); virtual;
    procedure ScaleAndOffset(scale: double; dx, dy: integer);

    property StrokeWidth  : double read fstrokeWidth write SetStrokeWidth;
    property SvgPath: TSvgPath read fSvgPath;
    property SvgSubPath[index: integer]: TSvgSubPathLayer read GetSubPathLayer;
    property Margin  : integer read GetMargin;
    property Scale   : double read fScale;
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

function TrimTrailingZeros(const floatValStr: string): string;
var
  i: integer;
begin
  Result := floatValStr;
  i := Length(Result);
  while Result[i] = '0' do dec(i);
  if Result[i] = '.' then dec(i);
  SetLength(Result, i);
end;
//------------------------------------------------------------------------------

function AsIntStr(val: double): string;
begin
  Result := Format('%1.0n ', [val]);
end;
//------------------------------------------------------------------------------

function AsFloatStr(val: double; precision: integer): string;
begin
  Result := TrimTrailingZeros(Format('%1.*f', [precision, val]));
end;
//------------------------------------------------------------------------------

function ScaleAndOffsetPath(const p: TPathD;
  scale: double; dx,dy: integer): TPathD;
begin
  result := ScalePath(p, scale);
  result := OffsetPath(result, dx, dy);
end;
//------------------------------------------------------------------------------

function ScaleAndOffsetPoint(const pt: TPointD;
  scale: double; dx,dy: integer): TPointD;
begin
  result := ScalePoint(pt, scale);
  result := OffsetPoint(result, dx, dy);
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
    Owner.Scale, Owner.Location.X, Owner.Location.Y);
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
  pt := OffsetPoint(pt, fOwner.Location.X -Left, fOwner.Location.Y -Top);
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
// TSvgASegLayer
//------------------------------------------------------------------------------

procedure TSvgASegLayer.Init(seg: TSvgBaseSeg; arcIdx: integer);
var
  offset: TPoint;
  s: double;
begin
  with TSvgASegment(seg) do
    fArcInfo := arcInfos[arcIdx];

  offset := Owner.Location;
  s := Owner.Scale;
  with fArcInfo do
  begin
    rec := ScaleAndOffsetRect(rec, s, offset.X, offset.Y);
    startPos := ScaleAndOffsetPoint(startPos, s, offset.X, offset.Y);
    endPos := ScaleAndOffsetPoint(endPos,s, offset.X, offset.Y);
  end;
  SetBtnCtrlPts(nil);
  DrawPath;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.ScaleAndOffset(scale: double; dx,dy: integer);
begin
  if scale = 0 then scale := 1;
  if Assigned(fCtrlPts) then
    fCtrlPts := ScaleAndOffsetPath(fCtrlPts, scale, dx,dy);
  with fArcInfo do
  begin
    rec := ScaleAndOffsetRect(rec, scale, dx,dy);
    startPos := ScaleAndOffsetPoint(startPos, scale, dx,dy);
    endPos := ScaleAndOffsetPoint(endPos, scale, dx,dy);
  end;
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

procedure TSvgASegLayer.GetRectBtnPoints(out pt1, pt2: TPointD);
var
  pt: TPointD;
begin
  with fArcInfo do
  begin
    pt := PointD(rec.Left, rec.MidPoint.Y);
    pt2 := PointD(rec.Right, rec.MidPoint.Y);
    fHorzLeft := DistanceSqrd(pt, startPos) > DistanceSqrd(pt2, startPos);
    if fHorzLeft then
      pt1 := PointD(rec.Left - ArcRectMargin, rec.MidPoint.Y) else
      pt1 := PointD(rec.Right + ArcRectMargin, rec.MidPoint.Y);

    pt := PointD(rec.MidPoint.X, rec.Top);
    pt2 := PointD(rec.MidPoint.X, rec.Bottom);
    fVertTop := DistanceSqrd(pt, startPos) > DistanceSqrd(pt2, startPos);
    if fVertTop then
      pt2 := PointD(rec.MidPoint.X, rec.Top - ArcRectMargin) else
      pt2 := PointD(rec.MidPoint.X, rec.Bottom + ArcRectMargin);
    RotatePoint(pt1, rec.MidPoint, fArcInfo.rectAngle);
    RotatePoint(pt2, rec.MidPoint, fArcInfo.rectAngle);
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.ReverseArcDirection;
begin
  fArcInfo.sweepClockW := not fArcInfo.sweepClockW;
  FlattenPath;
  DrawPath;
  Invalidate(Bounds);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.Offset(dx,dy: integer);
begin
  inherited;
  with fArcInfo do
  begin
    startPos := OffsetPoint(startPos, dx, dy);
    endPos := OffsetPoint(endPos, dx, dy);
    OffsetRect(rec, dx, dy);
  end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetStringDef: string;
var
  a, a1,a2: double;
begin
  with fArcInfo do
  begin
    //descaled radii
    if Owner.fRelativeStr then Result := 'a ' else Result := 'A ';

    Result := Result + AsFloatStr(rec.Width/(Owner.Scale *2), 2) + ',';
    Result := Result + AsFloatStr(rec.Height/(Owner.Scale *2), 2) + ' ';
    //angle as degrees
    Result := Result + AsIntStr(RadToDeg(rectAngle));

    a1 := GetStartAngle;
    a2 := GetEndAngle;

    //large arce and direction flags
    a := a2 - a1;
    if a < 0 then a := a + angle360;

    if sweepClockW then
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

    if Owner.fRelativeStr then
      Result := Result + Owner.AsPointStr(endPos, startPos, 2) else
      Result := Result + Owner.AsPointStr(endPos, NullPointD, 2);

  end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetStartAngle: double;
var
  pt: TPointD;
begin
  with fArcInfo do
  begin
    pt := startPos;
    Img32.Vector.RotatePoint(pt, rec.MidPoint, -rectAngle);
    Result := GetEllipticalAngleFromPoint(rec, pt);
  end;
end;
//------------------------------------------------------------------------------

function TSvgASegLayer.GetEndAngle: double;
var
  pt: TPointD;
begin
  with fArcInfo do
  begin
    pt := endPos;
    Img32.Vector.RotatePoint(pt, rec.MidPoint, -rectAngle);
    Result := GetEllipticalAngleFromPoint(rec, pt);
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
    Result[0] := startPos;
    GetRectBtnPoints(Result[1], Result[2]);
    Result[3] := endPos;
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
  img32.Vector.RotatePoint(pt, mp, -fArcInfo.rectAngle);

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
  img32.Vector.RotatePoint(pt, mp, fArcInfo.rectAngle);
  moveDx := Round(pt.X - movedBtn.MidPoint.X);
  moveDy := Round(pt.Y - movedBtn.MidPoint.Y);
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.UpdateBtnGroup(movedBtn: TLayer32);
var
  sa,ea, d,dx,dy: double;
  mp, sp: TPointD;
  designer  : TDesignerLayer32;
begin
  designer := GetDesignerLayer(movedBtn);
  if not Assigned(designer) then Exit;
  mp := movedBtn.MidPoint;

  case TButtonDesignerLayer32(movedBtn).BtnIdx of
    1:                                //rect horz
      begin
        with fArcInfo do
        begin
          img32.Vector.RotatePoint(mp, rec.MidPoint, - rectAngle);
          if fHorzLeft then
            d := (mp.X + ArcRectMargin - rec.Left) else
            d := (rec.Right + ArcRectMargin - mp.X);
          d := d/2;
          if d = 0 then Exit;
          //save the start point before resizing rect
          //then after resizing, realign the rect so
          //the start point doesn't change
          sa := GetStartAngle;
          ea := GetEndAngle;
          rec.Left := rec.Left + d;
          rec.Right := rec.Right - d;
          sp := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, sa);
          endPos := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, ea);
          dx := startPos.X - sp.X; dy := startPos.Y - sp.Y;
          OffsetRect(fArcInfo.rec, dx, dy);
          endPos := OffsetPoint(endPos, dx, dy);
        end;
        fCtrlPts := GetBtnCtrlPts;  //todo check if needed
        FlattenPath;
        DrawPath;
        DrawDesigner(designer);
        movedBtn.PositionCenteredAt(fCtrlPts[1]);
        movedBtn.Parent[3].PositionCenteredAt(fCtrlPts[2]);
        movedBtn.Parent[4].PositionCenteredAt(fArcInfo.endPos);
      end;
    2:                                //rect vert
      begin
        with fArcInfo do
        begin
          img32.Vector.RotatePoint(mp, rec.MidPoint, - rectAngle);
          if fVertTop then
            d := (mp.Y + ArcRectMargin - rec.Top) else
            d := (rec.Bottom + ArcRectMargin - mp.Y);
          if d = 0 then Exit;

          //save the start point before resizing rect
          //then after resizing, realign the rect so
          //the start point doesn't change
          sa := GetStartAngle;
          ea := GetEndAngle;
          rec.Top := rec.Top + d;
          rec.Bottom := rec.Bottom - d;
          sp := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, sa);
          dx := (startpos.X - sp.X); dy:= (startpos.Y - sp.Y);
          OffsetRect(rec, dx, dy);
          endPos := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, ea);
        end;
        fCtrlPts := GetBtnCtrlPts;  //todo check if needed
        FlattenPath;
        DrawPath;
        DrawDesigner(designer);
        movedBtn.PositionCenteredAt(fCtrlPts[2]);
        movedBtn.Parent[2].PositionCenteredAt(fCtrlPts[1]);
        movedBtn.Parent[4].PositionCenteredAt(fArcInfo.endPos);
      end;
    3:
      begin
        with fArcInfo do
        begin
          endPos := GetClosestPtOnRotatedEllipse(rec, rectAngle, mp);
        end;
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
  fRotAngle := fArcInfo.rectAngle;
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.UpdateRotateBtnGroup(movedBtn: TLayer32);
var
  a, sa, ea, dx, dy: double;
  sp: TPointD;
begin
  if not (movedBtn.Parent is TRotatingGroupLayer32) then Exit;
  a := UpdateRotatingButtonGroup(movedBtn);
  if a = 0 then Exit;

  //save the start angle before rotating rect
  //then after rotating, realign the rect so
  //the start point doesn't change
  sa := GetStartAngle;
  ea := GetEndAngle;
  with fArcInfo do
  begin
    rectAngle := fRotAngle + a;
    sp := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, sa);
    dx := sp.X - startPos.X; dy := sp.Y - startPos.Y;
    OffsetRect(rec, -dx, -dy);
    endPos := GetPtOnRotatedEllipseFromAngle(rec, rectAngle, ea);
  end;
  FlattenPath;
  DrawPath;
  Invalidate(Bounds);
  fCtrlPts[High(fCtrlPts)] := Paths[0][High(Paths[0])];
end;
//------------------------------------------------------------------------------

procedure TSvgASegLayer.FlattenPath;
var
  p: TPathD;
  a1,a2: double;
begin
  with fArcInfo do
  begin
    a1 := GetStartAngle;
    a2 := GetEndAngle;
    if not sweepClockW then
      p := Arc(rec, a2, a1) else
      p := Arc(rec, a1, a2);
    if rectAngle <> 0 then
      p := RotatePath(p, rec.MidPoint, rectAngle);
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
  if fArcInfo.rectAngle = 0 then
  begin
    designer.SetBounds(Rect(r2));
    OffsetRect(r, - designer.Left, -designer.Top);
    DrawDashedLine(designer.Image, Ellipse(r), [j,j],
      nil, dpiAware1, clMaroon32, esPolygon);
  end else
  begin
    p := Rectangle(r2);
    p := RotatePath(p, r2.MidPoint, fArcInfo.rectAngle);
    designer.SetBounds(Rect(Img32.Vector.GetBoundsD(p)));

    p := Ellipse(r);
    p := RotatePath(p, r2.MidPoint, fArcInfo.rectAngle);
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
      Result := Result + Owner.AsPointStr(fCtrlPts[i], relPt, 2);
      if (i mod 3) = 0 then relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'C ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + Owner.AsPointStr(fCtrlPts[i], NullPointD, 2);
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
      Result := Result + AsFloatStr(dx, 2) + ' ';
    end;
  end else
  begin
    Result := 'H ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + AsFloatStr(fCtrlPts[i].X, 2) + ' ';
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
      Result := Result + Owner.AsPointStr(fCtrlPts[i], relPt, 2);
      relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'L ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + Owner.AsPointStr(fCtrlPts[i], NullPointD, 2);
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
      Result := Result + Owner.AsPointStr(fCtrlPts[i], relPt, 2);
      if (i mod 2) = 0 then relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'Q ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + Owner.AsPointStr(fCtrlPts[i], NullPointD, 2);
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
      Result := Result + Owner.AsPointStr(fCtrlPts[i], relPt, 2);
      if (i mod 2 = 1) then relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'S ';
    for i := 2 to High(fCtrlPts) do
      Result := Result + Owner.AsPointStr(fCtrlPts[i], NullPointD, 2);
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

function TSvgTSegLayer.CreateBtnGroup: TButtonGroupLayer32;
begin
  Result := inherited CreateBtnGroup;
  //nb: don't forget there's a designer layer inside this TButtonGroupLayer32
  with TButtonDesignerLayer32(Result[2]) do
  begin
    Visible := False;
    Move(Result, Index -1);
  end;
end;
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
      Result := Result + Owner.AsPointStr(fCtrlPts[i], relPt, 2);
      relPt := fCtrlPts[i];
    end;
  end else
  begin
    Result := 'T ';
    for i := 2 to High(fCtrlPts) do
      Result := Result + Owner.AsPointStr(fCtrlPts[i], NullPointD, 2);
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
      Result := Result + AsFloatStr(dy, 2) + ' ';
    end;
  end else
  begin
    Result := 'V ';
    for i := 1 to High(fCtrlPts) do
      Result := Result + AsFloatStr(fCtrlPts[i].Y, 2) + ' ';
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

function TSvgSubPathLayer.GetStringDef: string;
var
  i: integer;
begin
  Result := 'M ' +
    AsFloatStr(fStartPos.X, 2) + ',' +
    AsFloatStr(fStartPos.Y, 2) + ' ';

  for i := 0 to ChildCount -1 do
    if Child[i] is TSegBaseLayer then
      Result := Result + TSegBaseLayer(Child[i]).GetStringDef;
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

procedure TSvgPathLayer.Init(svgPath: TSvgPath;
  scale: double; offsetX, offsetY: integer);
var
  i       : integer;
  subPath : TSvgSubPathLayer;
begin
  if not Assigned(svgPath) then Exit;

  ClearChildren; //in case of reusing

  PositionAt(offsetX, offsetY);
  fSvgPath      := svgPath;
  fScale        := scale;
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

function TSvgPathLayer.AsPointStr(pt: TPointD;
  const relPt: TPointD; precision: integer): string;
var
  delta: TPointD;
  s1, s2: string;
begin
  pt.x := (pt.x - Location.X) /Scale;
  pt.y := (pt.y - Location.Y) /Scale;
  if fRelativeStr then
  begin
    delta.X := (relPt.X - Location.X) /Scale;
    delta.Y := (relPt.Y - Location.Y) /Scale;
    pt := OffsetPoint(pt, -delta.X, -delta.Y);
  end;
  s1 := TrimTrailingZeros(Format('%1.*f', [precision, pt.x]));
  s2 := TrimTrailingZeros(Format('%1.*f', [precision, pt.y]));
  Result := s1 + ',' + s2 + ' ';
end;
//------------------------------------------------------------------------------

end.
