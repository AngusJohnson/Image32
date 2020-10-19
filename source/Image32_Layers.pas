unit Image32_Layers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.52                                                            *
* Date      :  1 October 2020                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  Layer support for the Image32 library                           *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Math, Types, Image32, Image32_Extra;

type
  TSizingStyle = (ssCorners, ssEdges, ssEdgesAndCorners, ssCustom);
  TMergeImageState = (misUnknown, misMergeAll, misHideDesigners);

  TLayer32 = class;
  TLayer32Class = class of TLayer32;
  TLayeredImage32 = class;

  TLayerHitTestEvent =
    function (layer: TLayer32; const pt: TPoint): Boolean of Object;

  TLayer32 = class
  private
    fOwner       : TLayeredImage32;
    fIndex       : integer;
    fImage       : TImage32;
    fPosition    : TPoint;
    fVisible     : Boolean;
    fOpacity     : Byte;
    fName        : string;
    fGroupId     : integer;
    fCursorId    : integer;
    fHitTestRegions: TPathsD;
    function GetBounds: TRect;
    function GetMidPoint: TPointD;
    function GetClientMidPoint: TPointD;
    procedure SetVisible(value: Boolean);
    procedure ImageChanged(Sender: TObject);
    function GetHeight: integer;
    function GetWidth: integer;
    function GetIsInGroup: Boolean;
    function GetGroupIndex: integer;
    procedure SetOpacity(value: Byte);
  protected
    function HitTest(const pt: TPoint): Boolean; virtual;
    property IsInGroup: Boolean read GetIsInGroup;
  public
    constructor Create(owner: TLayeredImage32); virtual;
    destructor Destroy; override;
    procedure SetSize(width, height: integer);
    function BringForward(newLevel: integer): Boolean;
    function SendBack(newLevel: integer): Boolean;
    procedure Offset(dx, dy: integer); virtual;
    procedure PositionAt(const pt: TPoint); overload;
    procedure PositionAt(x, y: integer); overload;
    procedure PositionCenteredAt(X, Y: integer); overload;
    procedure PositionCenteredAt(const pt: TPoint); overload;
    procedure PositionCenteredAt(const pt: TPointD); overload;
    procedure SetBounds(const bounds: TRect);

    property Bounds: TRect read GetBounds;
    property ClientMidPoint: TPointD read GetClientMidPoint;
    property CursorId: integer read fCursorId write fCursorId;
    property GroupId: integer read fGroupId;
    property Height: integer read GetHeight;
    property HitTestRegions: TPathsD
      read fHitTestRegions write fHitTestRegions;
    property Image: TImage32 read fImage;
    property Index: integer read fIndex;
    property IndexInGroup: integer read GetGroupIndex;
    property Left: integer read fPosition.X;
    property MidPoint: TPointD read GetMidPoint;
    property Name: string read fName write fName;
    property Opacity: Byte read fOpacity write SetOpacity;
    property Owner: TLayeredImage32 read fOwner;
    property Top: integer read fPosition.Y;
    property Visible: Boolean read fVisible write SetVisible;
    property Width: integer read GetWidth;
  end;

  //TCustomDesignerLayer32 objects can be filtered
  //(ie excluded) from TLayeredImage32.GetMergedImage
  TCustomDesignerLayer32 = class(TLayer32);

  TButtonDesignerLayer32 = class(TCustomDesignerLayer32);
  TButtonDesignerLayer32Class = class of TButtonDesignerLayer32;

  //TDesignerLayer32 objects are 'non-click' in that they won't be
  //returned by calls to TLayeredImage32.GetLayerAt(). This class also
  //contains a number of special designer methods.
  TDesignerLayer32 = class(TCustomDesignerLayer32)
  private
    fPenWidth: double;
    fPenColor: TColor32;
    fButtonSize: double;
  protected
    procedure DrawDashedLine(const ctrlPts: TPathD; closed: Boolean);
    procedure DrawGridLine(const pt1, pt2: TPointD;
      width: double; color: TColor32); virtual;
    function HitTest(const pt: TPoint): Boolean; override;
  public
    constructor Create(owner: TLayeredImage32); override;
    procedure DrawGrid(majorInterval, minorInterval: integer);
    procedure DrawLine(const path: TPathD);
    procedure DrawRectangle(const rec: TRect);
    procedure DrawEllipse(const rec: TRect);
    procedure DrawCSplineDesign(
      const ctrlPts: TPathD; isStartOfPath: Boolean = true);
    procedure DrawQSplineDesign(
      const ctrlPts: TPathD; isStartOfPath: Boolean = true);
    property ButtonSize: double read fButtonSize write fButtonSize;
    property PenColor: TColor32 read fPenColor write fPenColor;
    property PenWidth: double read fPenWidth write fPenWidth;
  end;

  TLayeredImage32 = class
  private
    fMergedImage: TImage32;
    fList: TList;
    //fGroupList: Lists the layer index of the bottom of each group.
    //Negative offsets indicate the group no longer exists.
    //Given that group indexes start at 1 and fGroupList is zero based, then
    //fGroupList[0] refers to group 1 and fGroupList[1] refers to group 2 etc.
    //eg fGroupList[1] := 10 --> group 2 starts at Layer[10].
    fGroupList: TList;
    fBackColor: TColor32;
    fUpdatePending: Boolean;
    fMergeImageState: TMergeImageState;
    function GetCount: integer;
    function GetHeight: integer;
    procedure SetHeight(value: integer);
    function GetWidth: integer;
    procedure SetWidth(value: integer);
    function GetBounds: TRect;
    function GetMidPoint: TPointD;
    function GetLayer(index: integer): TLayer32;
    function GetTopLayer: TLayer32;
    procedure ReIndexFrom(start, stop: integer);
  public
    constructor Create(Width: integer = 0; Height: integer =0); virtual;
    destructor Destroy; override;
    procedure SetSize(width, height: integer);
    function AddLayer(layerClass: TLayer32Class): TLayer32; overload;
    function AddLayer: TLayer32; overload;
    function InsertLayer(layerClass: TLayer32Class;
      index: integer): TLayer32; overload;
    function InsertLayer(index: integer): TLayer32; overload;
    procedure DeleteLayer(index: integer);
    procedure Clear;
    procedure Invalidate;
    function FindLayerNamed(const name: string): TLayer32;
    function GetLayerAt(const pt: TPoint): TLayer32;
    function GetMergedImage(hideDesigners: Boolean = false): TImage32;

    function Group(startIdx, endIdx: integer): integer;
    procedure UnGroup(GroupId: integer);
    function GroupCount(groupId: integer): integer;
    function GetFirstInGroupIdx(GroupId: integer): integer;
    function GetLastInGroupIdx(GroupId: integer): integer;
    function BringGroupForward(GroupId, newLevel: integer): Boolean;
    function SendGroupBack(GroupId, newLevel: integer): Boolean;
    procedure OffsetGroup(GroupId, dx, dy: integer);
    function DeleteGroup(GroupId: integer): Boolean;
    procedure HideGroup(GroupId: integer);
    procedure ShowGroup(GroupId: integer);

    property BackgroundColor: TColor32 read fBackColor write fBackColor;
    property Bounds: TRect read GetBounds;
    property Count: integer read GetCount;
    property Height: integer read GetHeight write SetHeight;
    property Layer[index: integer]: TLayer32 read GetLayer; default;
    property MidPoint: TPointD read GetMidPoint;
    property TopLayer: TLayer32 read GetTopLayer;
    property Width: integer read GetWidth write SetWidth;
  end;

  function CreateSizingBtnsGroup(targetLayer: TLayer32;
    style: TSizingStyle; buttonColor: TColor32;
    buttonSize: integer; buttonOptions: TButtonOptions;
    buttonLayerClass: TButtonDesignerLayer32Class = nil): integer;
  function UpdateButtonSizingGroup(movedBtnLayer: TButtonDesignerLayer32): TRect;
  function CreateButtonGroup(layeredImage32: TLayeredImage32;
    const buttonPts: TPathD; buttonColor: TColor32;
    buttonSize: integer; buttonOptions: TButtonOptions;
    buttonLayerClass: TButtonDesignerLayer32Class = nil): integer;
  function StartButtonGroup(layeredImage32: TLayeredImage32;
    const buttonPt: TPoint; buttonColor: TColor32;
    buttonSize: integer; buttonOptions: TButtonOptions;
    buttonLayerClass: TButtonDesignerLayer32Class = nil): TButtonDesignerLayer32;
  function AddToButtonGroup(layeredImage32: TLayeredImage32;
    GroupId: integer; const pt: TPoint; buttonColor: TColor32; buttonSize: integer;
    buttonOptions: TButtonOptions): TButtonDesignerLayer32; overload;
  function AddToButtonGroup(layeredImage32: TLayeredImage32;
    GroupId: integer; const pt: TPoint): TButtonDesignerLayer32; overload;

var
  DefaultButtonSize: integer;
  ButtonLayerName: string = 'button';

const
  crDefault   =   0;
  crArrow     =  -2;
  crSizeNESW  =  -6;
  crSizeNS    =  -7;
  crSizeNWSE  =  -8;
  crSizeWE    =  -9;
  crHandPoint = -21;
  crSizeAll   = -22;

implementation

uses
  Image32_Draw, Image32_Vector;

resourcestring
  rsLayer32DeleteError =
    'TLayer32 delete error: Individual layers must be ungrouped before they '+
    'can be deleted. Otherwise, use TLayeredImage32.DeleteGroup.';
  rsImageLayerRangeError = 'TLayeredImage32 error: index out of range.';

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetRectCorners(const rec: TRect): TPathD;
begin
  Result := Rectangle(rec);
end;
//------------------------------------------------------------------------------

function GetRectEdgeMidPoints(const rec: TRect): TPathD;
var
  mp: TPointD;
begin
  mp := MidPoint(RectD(rec));
  SetLength(result, 4);
  Result[0] := PointD(mp.X, rec.Top);
  Result[1] := PointD(rec.Right, mp.Y);
  Result[2] := PointD(mp.X, rec.Bottom);
  Result[3] := PointD(rec.Left, mp.Y);
end;

//------------------------------------------------------------------------------
// TLayer32 class
//------------------------------------------------------------------------------

constructor TLayer32.Create(owner: TLayeredImage32);
begin
  fImage   := TImage32.Create;
  fOwner   := owner;
  fIndex   := 0;
  fVisible := true;
  fOpacity := 255;
  fGroupId := 0;
  CursorId := crSizeAll;
  fImage.OnChange := ImageChanged;
end;
//------------------------------------------------------------------------------

destructor TLayer32.Destroy;
begin
 fImage.Free;
 inherited;
end;
//------------------------------------------------------------------------------

function TLayer32.HitTest(const pt: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, pt);
  if Result and assigned(fHitTestRegions) then
    Result := PointInPolygons(
      OffsetPoint(PointD(pt),-left,-top), fHitTestRegions, frEvenOdd)
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetSize(width, height: integer);
begin
  if (width <> fImage.Width) or (height <> fImage.Height) then
    fImage.SetSize(width, height) else
    fImage.Clear;
end;
//------------------------------------------------------------------------------

function TLayer32.GetIsInGroup: Boolean;
begin
  Result := fGroupId > 0;
end;
//------------------------------------------------------------------------------

function TLayer32.GetGroupIndex: integer;
begin
  if IsInGroup then
    Result := Index - Owner.GetFirstInGroupIdx(GroupId) else
    result := -1;
end;
//------------------------------------------------------------------------------

function TLayer32.GetHeight: integer;
begin
  result := fImage.Height;
end;
//------------------------------------------------------------------------------

function TLayer32.GetWidth: integer;
begin
  result := fImage.Width;
end;
//------------------------------------------------------------------------------

procedure TLayer32.ImageChanged(Sender: TObject);
begin
  fOwner.Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.GetBounds: TRect;
begin
  Result.TopLeft := fPosition;
  Result.Right := Result.Left + fImage.Width;
  Result.Bottom := Result.Top + fImage.Height;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetBounds(const bounds: TRect);
begin
  SetSize(RectWidth(bounds), RectHeight(bounds));
  PositionAt(bounds.TopLeft);
end;
//------------------------------------------------------------------------------

function TLayer32.GetClientMidPoint: TPointD;
begin
  if Image.IsEmpty then
    Result := NullPointD else
    Result := PointD(Image.Width, Image.Height);
end;
//------------------------------------------------------------------------------

function TLayer32.GetMidPoint: TPointD;
begin
  Result := Image32_Vector.MidPoint(RectD(GetBounds));
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionAt(const pt: TPoint);
begin
  if (pt.X = fPosition.X) and (pt.Y = fPosition.Y) then Exit;
  fPosition := pt;
  fOwner.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionAt(x, y: integer);
begin
  PositionAt(Types.Point(x, y));
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionCenteredAt(X, Y: integer);
var
  pt2: TPoint;
begin
  pt2.X := X - fImage.Width div 2;
  pt2.Y := Y - fImage.Height div 2;
  PositionAt(pt2);
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionCenteredAt(const pt: TPoint);
var
  pt2: TPoint;
begin
  pt2.X := pt.X - fImage.Width div 2;
  pt2.Y := pt.Y - fImage.Height div 2;
  PositionAt(pt2);
end;
//------------------------------------------------------------------------------

procedure TLayer32.PositionCenteredAt(const pt: TPointD);
var
  pt2: TPoint;
begin
  pt2.X := Round(pt.X - fImage.Width * 0.5);
  pt2.Y := Round(pt.Y - fImage.Height  * 0.5);
  PositionAt(pt2);
end;
//------------------------------------------------------------------------------

procedure TLayer32.Offset(dx, dy: integer);
begin
  PositionAt(Types.Point(Left + dx, Top + dy));
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetVisible(value: Boolean);
begin
  if (value = fVisible) then Exit;
  fVisible := value;
  ImageChanged(self);
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetOpacity(value: Byte);
begin
  if value = fOpacity then Exit;
  fOpacity := value;
  ImageChanged(self);
end;
//------------------------------------------------------------------------------

function TLayer32.BringForward(newLevel: integer): Boolean;
var
  topIdx: integer;
begin
  topIdx := fOwner.Count -1;
  if newLevel > topIdx then newLevel := topIdx;
  Result := (newLevel > fIndex) and not IsInGroup;
  if not Result then Exit;

  if fOwner[newLevel].IsInGroup then
    while (newLevel < topIdx) and
      (fOwner[newLevel].fGroupId = fOwner[newLevel+1].fGroupId) do
        inc(newLevel);

  fOwner.fList.Move(fIndex, newLevel);
  fOwner.ReIndexFrom(fIndex, newLevel);
  fOwner.Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.SendBack(newLevel: integer): Boolean;
begin
  if newLevel < 0 then newLevel := 0;
  Result := (newLevel < fIndex) and not IsInGroup;
  if not Result then Exit;

  while fOwner[newLevel].IsInGroup and (newLevel > 0) and
    (fOwner[newLevel].fGroupId = fOwner[newLevel-1].fGroupId) do
      dec(newLevel);
  fOwner.fList.Move(fIndex, newLevel);
  fOwner.ReIndexFrom(newLevel,fIndex);
  fOwner.Invalidate;
end;

//------------------------------------------------------------------------------
// TDesignerLayer32 class
//------------------------------------------------------------------------------

constructor TDesignerLayer32.Create(owner: TLayeredImage32);
begin
  inherited Create(owner);
  fPenWidth := 1;
  fPenColor := clRed32;
  CursorId := 0;
end;
//------------------------------------------------------------------------------

function TDesignerLayer32.HitTest(const pt: TPoint): Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawDashedLine(const ctrlPts: TPathD;
  closed: Boolean);
const
  endstyle: array[Boolean] of TEndStyle = (esSquare, esClosed);
var
  step: integer;
begin
  step := Round(fPenWidth * 1.5) + 3;
  Image32_Draw.DrawDashedLine(Image, ctrlPts,
    MakeArrayOfInteger([step,step]), nil, fPenWidth, fPenColor, endstyle[closed]);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawGridLine(const pt1, pt2: TPointD;
  width: double; color: TColor32);
var
  path: TPathD;
begin
  SetLength(path, 2);
  path[0] := pt1; path[1] := pt2;
  Image32_Draw.DrawLine(Image, path, width, color, esSquare);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawGrid(majorInterval, minorInterval: integer);
var
  i, x,y, w,h: integer;
begin
  w := image.Width; h := image.Height;
  if minorInterval <> 0 then
  begin
    x := minorInterval; y := minorInterval;
    for i := 1 to (w div minorInterval) -1 do
    begin
      DrawGridLine(PointD(x, 0), PointD(x, h), 1, $20000000);
      inc(x, minorInterval);
    end;
    for i := 1 to (h div minorInterval) -1 do
    begin
      DrawGridLine(PointD(0, y), PointD(w, y), 1, $20000000);
      inc(y, minorInterval);
    end;
  end;
  if majorInterval <> 0 then
  begin
    x := majorInterval; y := majorInterval;
    for i := 1 to (w div majorInterval) -1 do
    begin
      DrawGridLine(PointD(x, 0), PointD(x, h), 1, $30000000);
      inc(x, majorInterval);
    end;
    for i := 1 to (h div majorInterval) -1 do
    begin
      DrawGridLine(PointD(0, y), PointD(w, y), 1, $30000000);
      inc(y, majorInterval);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawQSplineDesign(
  const ctrlPts: TPathD; isStartOfPath: Boolean);
var
  i,j, len: integer;
  pt, pt2: TPointD;
  path: TPathD;
begin
  len := length(ctrlPts);
  if len < 3 then Exit;
  if fButtonSize < 2 then fButtonSize := DefaultButtonSize;
  SetLength(path, 2);
  if isStartOfPath then
  begin
    path[0] := ctrlPts[0];
    path[1] := ctrlPts[1];
    DrawDashedLine(path, false);
    j := 2;
  end else
    j := 1;
  pt := ctrlPts[j -1];
  for i := j to len -2 do
  begin
    pt2 := ReflectPoint(pt, ctrlPts[i]);
    path[0] := pt;
    path[1] := pt2;
    DrawDashedLine(path, false);
    DrawButton(Image, pt2, fButtonSize);
    pt := pt2;
  end;
  path[0] := pt; path[1] := ctrlPts[len-1];
  DrawDashedLine(path, false);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawCSplineDesign(
  const ctrlPts: TPathD; isStartOfPath: Boolean = true);
var
  i,len: integer;
  pt: TPointD;
  path: TPathD;
begin
  len := length(ctrlPts);
  if Odd(len) then dec(len);
  if len < 4 then Exit;
  if fButtonSize < 2 then fButtonSize := DefaultButtonSize;
  SetLength(path, 2);
  if isStartOfPath then
  begin
    path[0] := ctrlPts[0];
    path[1] := ctrlPts[1];
    DrawDashedLine(path, false);
    i := 2;
  end else
    i := 0;
  while i < len -2 do
  begin
    pt := ReflectPoint(ctrlPts[i], ctrlPts[i+1]);
    path[0] := ctrlPts[i];
    path[1] := pt;
    DrawDashedLine(path, false);
    DrawButton(Image, pt, fButtonSize);
    inc(i, 2);
  end;
  path[0] := ctrlPts[len-2];
  path[1] := ctrlPts[len-1];
  DrawDashedLine(path, false);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawLine(const path: TPathD);
begin
  DrawDashedLine(path, false);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawRectangle(const rec: TRect);
var
  path: TPathD;
begin
  path := Rectangle(rec);
  DrawDashedLine(path, true);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawEllipse(const rec: TRect);
var
  path: TPathD;
begin
  path := Ellipse(rec);
  DrawDashedLine(path, true);
end;

//------------------------------------------------------------------------------
// TLayeredImage32 class
//------------------------------------------------------------------------------

constructor TLayeredImage32.Create(Width: integer; Height: integer);
begin
  fList := TList.Create;
  fGroupList := TList.Create;
  fMergedImage := TImage32.Create(Width, Height);
  fMergeImageState := misUnknown;
end;
//------------------------------------------------------------------------------

destructor TLayeredImage32.Destroy;
begin
  Clear;
  fGroupList.Free;
  fList.Free;
  fMergedImage.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetSize(width, height: integer);
begin
  fMergedImage.SetSize(width, height);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayer(index: integer): TLayer32;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsImageLayerRangeError);
  Result := TLayer32(fList[index]);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetTopLayer: TLayer32;
begin
  if Count = 0 then Result := nil else
  result := Layer[Count -1];
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.ReIndexFrom(start, stop: integer);
var
  i, lastGroupId: integer;
begin
  stop := Min(stop, Count -1);
  lastGroupId := 0;
  for i := start to stop do
    with Layer[i] do
    begin
      fIndex := i;
      if (fGroupId > 0) and (lastGroupId <> fGroupId) then
      begin
        fGroupList[fGroupId -1] := Pointer(i); //ie first in group
        lastGroupId := fGroupId;
      end;
    end;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.Invalidate;
begin
  fUpdatePending := true;
end;
//------------------------------------------------------------------------------

procedure AdjustOpacity(image: TImage32; opacity: byte);
var
  i: Integer;
  pb: PARGB;
  alphaTbl: PByteArray;
begin
  pb := PARGB(image.PixelBase);
  alphaTbl := PByteArray(@MulTable[opacity]);
  for i := 0 to image.Width * image.Height - 1 do
  begin
    pb.A := alphaTbl[pb.A];
    inc(pb);
  end;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMergedImage(hideDesigners: Boolean): TImage32;
var
  i: integer;
  blendFunc: TBlendFunction;
  tmp: TImage32;
begin
  if fUpdatePending or
    (hideDesigners and (fMergeImageState <> misHideDesigners)) or
    (not hideDesigners and (fMergeImageState <> misMergeAll)) then
  begin
    fMergedImage.Clear(fBackColor);

    if fBackColor shr 24 < 254 then //ie semi-transparent
      blendFunc  := BlendToAlpha else
      blendFunc := BlendToOpaque;

    for i := 0 to Count -1 do
    begin
      with Layer[i] do
      begin
        if not Visible or (fOpacity < 2) or Image.IsEmpty or
          (hideDesigners and (layer[i] is TCustomDesignerLayer32)) then
            Continue;

        if fOpacity < 254 then
        begin
          //reduce layer opacity
          tmp := TImage32.Create(Image);
          try
            AdjustOpacity(tmp, fOpacity);
            fMergedImage.CopyBlend(tmp, tmp.Bounds, Bounds, blendFunc);
          finally
            tmp.Free;
          end;
        end else
          fMergedImage.CopyBlend(Image, Image.Bounds, Bounds, blendFunc);
      end;
    end;

    fUpdatePending := false;
    if hideDesigners then fMergeImageState := misHideDesigners
    else fMergeImageState := misMergeAll;
  end;
  Result := fMergedImage;
 end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.Clear;
var
  i: integer;
begin
  fMergedImage.Clear;
  for i := 0 to Count -1 do
    TLayer32(fList[i]).free;
  fList.Clear;
  fGroupList.Clear;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetCount: integer;
begin
  Result := fList.Count;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetHeight: integer;
begin
  Result := fMergedImage.Height;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetHeight(value: integer);
begin
  if fMergedImage.Height = value then Exit;
  fMergedImage.SetSize(Width, value);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetWidth: integer;
begin
  Result := fMergedImage.Width;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetWidth(value: integer);
begin
  if fMergedImage.Width = value then Exit;
  fMergedImage.SetSize(value, Height);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetBounds: TRect;
begin
  Result := Types.Rect(0, 0, Width, Height);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMidPoint: TPointD;
begin
  Result := PointD(fMergedImage.Width * 0.5, fMergedImage.Height * 0.5);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.AddLayer(layerClass: TLayer32Class): TLayer32;
begin
  Result := InsertLayer(layerClass, Count);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.AddLayer: TLayer32;
begin
  Result := AddLayer(TLayer32);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertLayer(index: integer): TLayer32;
begin
  Result := InsertLayer(TLayer32, index);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertLayer(layerClass: TLayer32Class;
  index: integer): TLayer32;
begin
  if (index < 0) or (index > Count) then
    raise Exception.Create(rsImageLayerRangeError);

  //make sure a new layer isn't inserted into the middle of a group
  if (index < Count) and Layer[index].IsInGroup then
    while (index > 0) and
      (Layer[index].fGroupId = Layer[index-1].fGroupId) do
        dec(index);

  //insert new layer into fList and update indexes
  Result := layerClass.Create(Self);
  fList.Insert(index, Result);
  ReIndexFrom(index, Count -1);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteLayer(index: integer);
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsImageLayerRangeError);
  if Layer[index].IsInGroup then
    raise Exception.Create(rsLayer32DeleteError);
  Layer[index].Free;
  fList.Delete(index);
  ReIndexFrom(index, Count -1);
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.FindLayerNamed(const name: string): TLayer32;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    if SameText(Layer[i].Name, name) then
    begin
      Result := Layer[i];
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayerAt(const pt: TPoint): TLayer32;
var
  i: integer;
begin
  Result := nil;
  for i := Count -1 downto 0 do
    with Layer[i] do
      if Visible and not Image.IsEmpty and HitTest(pt) then
      begin
        Result := Layer[i];
        Break;
      end;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.Group(startIdx, endIdx: integer): integer;
var
  i: integer;
begin
  Result := 0;
  startIdx := Max(0, startIdx);
  endIdx := Min(Count -1, endIdx);
  for i := startIdx to endIdx do
    if Layer[i].IsInGroup then Exit;

  //first look for an expired group (negative values => expired group)
  Result := fGroupList.Count +1;
  for i := 0 to fGroupList.Count -1 do
    if integer(fGroupList[i]) < 0 then
    begin
      Result := i +1;
      fGroupList[i] := Pointer(startIdx);
      Break;
    end;
  //if no expired groups then add the new group index
  if Result > fGroupList.Count then
    fGroupList.Add(pointer(startIdx));

  for i := startIdx to endIdx do
    Layer[i].fGroupId := Result;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.UnGroup(GroupId: integer);
var
  i, fig, lig: integer;
begin
  fig := GetFirstInGroupIdx(GroupId);
  if fig < 0 then Exit;
  lig := GetLastInGroupIdx(GroupId);
  for i := fig to lig do
    Layer[i].fGroupId := 0;
  fGroupList[GroupId -1] := pointer(-1);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetFirstInGroupIdx(GroupId: integer): integer;
begin
  if (GroupId < 1) or (GroupId > fGroupList.Count) then
    Result := -1 else
    Result := integer(fGroupList[GroupId -1]);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLastInGroupIdx(GroupId: integer): integer;
var
  topLevel: integer;
begin
  Result := GetFirstInGroupIdx(GroupId);
  if Result < 0 then Exit;
  topLevel := Count -1;
  while (Result < topLevel) and
    (Layer[Result +1].fGroupId = GroupId) do inc(Result);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GroupCount(groupId: integer): integer;
var
  idx, idx2, cnt: integer;
begin
  Result := 0;
  cnt := Count;
  idx := GetFirstInGroupIdx(groupId);
  if idx < 0 then Exit;
  idx2 := idx +1;
  while (idx2  < cnt) and (Layer[idx2].fGroupId = groupId) do inc(idx2);
  Result := idx2 - idx;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.OffsetGroup(GroupId, dx, dy: integer);
var
  i, fig, lig: integer;
begin
  fig := GetFirstInGroupIdx(GroupId);
  if fig < 0 then Exit;
  lig := GetLastInGroupIdx(GroupId);
  //nb: it almost never makes sense offsetting design layers
  for i := fig to lig do
    if not (Layer[i] is TDesignerLayer32) then
      Layer[i].Offset(dx, dy);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.BringGroupForward(GroupId, newLevel: integer): Boolean;
var
  i, fig, lig, cnt, topIdx: integer;
begin
  topIdx := Count -1;
  fig := GetFirstInGroupIdx(GroupId);
  if fig < 0 then begin Result := false; Exit; end;
  lig := GetLastInGroupIdx(GroupId);
  cnt := lig - fig +1;
  result := (newLevel > lig) and (newLevel <= topIdx);
  if not result then Exit;
  //if newLevel is inside another group, adjust newLevel to above that group
  if Layer[newLevel].IsInGroup then
    while (newLevel < topIdx) and
      (Layer[newLevel].fGroupId = Layer[newLevel+1].fGroupId) do
        inc(newLevel);
  for i := 1 to cnt do
    fList.Move(fig, newLevel);
  fGroupList[GroupId -1] := pointer(newLevel);
  ReIndexFrom(fig, newLevel + cnt);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.SendGroupBack(GroupId, newLevel: integer): Boolean;
var
  i, fig, lig: integer;
begin
  fig := GetFirstInGroupIdx(GroupId);
  if fig < 0 then begin Result := false; Exit; end;
  lig := GetLastInGroupIdx(GroupId);
  Result := (newLevel < fig) and (newLevel >= 0);
  if not Result then Exit;
  if Layer[newLevel].IsInGroup then
    while (newLevel > 0) and
      (Layer[newLevel].fGroupId = Layer[newLevel-1].fGroupId) do
        dec(newLevel);
  for i := fig to lig do
    fList.Move(lig, newLevel);
  fGroupList[GroupId -1] := pointer(newLevel);
  ReIndexFrom(newLevel, lig);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.DeleteGroup(GroupId: integer): Boolean;
var
  i, fig, lig: integer;
begin
  fig := GetFirstInGroupIdx(GroupId);
  result := fig >= 0;
  if not Result then Exit;
  lig := GetLastInGroupIdx(GroupId);
  for i := lig downto fig do
  begin
    Layer[i].Free;
    fList.Delete(i);
  end;
  ReIndexFrom(fig, Count -1);
  //remove group from fGroupList
  fGroupList[GroupId -1] := pointer(-1);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.HideGroup(GroupId: integer);
var
  i, fig, lig: integer;
begin
  fig := GetFirstInGroupIdx(GroupId);
  if fig < 0 then Exit;
  lig := GetLastInGroupIdx(GroupId);
  for i := fig to lig do
    Layer[i].Visible := false;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.ShowGroup(GroupId: integer);
var
  i, fig, lig: integer;
begin
  fig := GetFirstInGroupIdx(GroupId);
  if fig < 0 then Exit;
  lig := GetLastInGroupIdx(GroupId);
  for i := fig to lig do
    Layer[i].Visible := true;
end;

//------------------------------------------------------------------------------
// Interface functions
//------------------------------------------------------------------------------

function GetRectFromGroupButtons(layeredImage32: TLayeredImage32;
  GroupId: integer): TRect;
var
  fig, lig: integer;
  pt: TPoint;
begin
  Result := NullRect;
  fig := layeredImage32.GetFirstInGroupIdx(GroupId);
  if fig < 0 then Exit;
  lig := layeredImage32.GetLastInGroupIdx(GroupId);
  result.TopLeft := Point(layeredImage32[fig].MidPoint);
  result.BottomRight := result.TopLeft;
  inc(fig);
  while fig <= lig do
  begin
    pt := Point(layeredImage32[fig].MidPoint);
    if pt.X < result.Left then result.Left := pt.X
    else if pt.X > result.Right then result.Right := pt.X;
    if pt.Y < result.Top then result.Top := pt.Y
    else if pt.Y > result.Bottom then result.Bottom := pt.Y;
    inc(fig);
  end;
end;
//------------------------------------------------------------------------------

procedure SizeAndDrawButtonLayer(layer: TLayer32; buttonSize: double;
  buttonColor: TColor32; buttonOptions: TButtonOptions);
var
  layerSize: integer;
begin
  if buttonSize < 2 then buttonSize := DefaultButtonSize;
  //allow room for a button drop shadow
  layerSize := Ceil(DefaultButtonSize * 1.25);
  //button layers must have even numbered width and height so the layer's
  //(rounded) Midpoint will be accurate (see PositionCenteredAt)
  //which is important when moving
  if Odd(layerSize) then inc(layerSize);
  layer.SetSize(layerSize, layerSize);
  Image32_Extra.DrawButton(layer.Image,
    layer.MidPoint, buttonSize, buttonColor, buttonOptions);
end;
//------------------------------------------------------------------------------

function CreateSizingBtnsGroup(targetLayer: TLayer32;
  style: TSizingStyle; buttonColor: TColor32;
  buttonSize: integer; buttonOptions: TButtonOptions;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): integer;
var
  i, idxFirstBtn: integer;
  rec: TRect;
  corners, edges: TPathD;
  layer: TLayer32;
  lim: TLayeredImage32;
const
  cnrCursorIds: array [0..3] of integer =
    (crSizeNWSE, crSizeNESW, crSizeNWSE, crSizeNESW);
  edgeCursorIds: array [0..3] of integer =
    (crSizeNS, crSizeWE, crSizeNS, crSizeWE);
begin
  result := 0;
  if targetLayer is TDesignerLayer32 or
    targetLayer.IsInGroup or targetLayer.Image.IsEmpty then Exit;

  lim := targetLayer.fOwner;
  idxFirstBtn := lim.Count;
  if style = ssCustom then style := ssEdgesAndCorners;
  if not assigned(buttonLayerClass) then
    buttonLayerClass := TButtonDesignerLayer32;

  rec := targetLayer.Bounds;
  corners := Rectangle(rec);
  edges := GetRectEdgeMidPoints(rec);

  case style of
    ssCorners:
      for i := 0 to 3 do
      begin
        layer := lim.AddLayer(buttonLayerClass);
        layer.Name := ButtonLayerName;
        SizeAndDrawButtonLayer(layer, buttonSize, buttonColor, buttonOptions);
        layer.PositionCenteredAt(Point(corners[i]));
        layer.CursorId := cnrCursorIds[i];
      end;
    ssEdges:
      for i := 0 to 3 do
      begin
        layer := lim.AddLayer(buttonLayerClass);
        layer.Name := ButtonLayerName;
        SizeAndDrawButtonLayer(layer, buttonSize, buttonColor, buttonOptions);
        layer.PositionCenteredAt(Point(edges[i]));
        layer.CursorId := edgeCursorIds[i];
      end;
    else
      for i := 0 to 3 do
      begin
        layer := lim.AddLayer(buttonLayerClass);
        layer.Name := ButtonLayerName;
        SizeAndDrawButtonLayer(layer, buttonSize, buttonColor, buttonOptions);
        layer.PositionCenteredAt(Point(corners[i]));
        layer.CursorId := cnrCursorIds[i];

        layer := lim.AddLayer(buttonLayerClass);
        layer.Name := ButtonLayerName;
        SizeAndDrawButtonLayer(layer, buttonSize, buttonColor, buttonOptions);
        layer.PositionCenteredAt(Point(edges[i]));
        layer.CursorId := edgeCursorIds[i];
      end;
  end;
  Result := lim.Group(idxFirstBtn, layer.fIndex);
end;
//------------------------------------------------------------------------------

function UpdateButtonSizingGroup(movedBtnLayer: TButtonDesignerLayer32): TRect;
var
  i, btnIdx, cnt, fig: integer;
  lim: TLayeredImage32;
  btnMP: TPoint;
  corners, edgeMps: TPathD;
  style: TSizingStyle;
begin
  lim := movedBtnLayer.Owner;
  fig := lim.GetFirstInGroupIdx(movedBtnLayer.GroupId);
  cnt := lim.GroupCount(movedBtnLayer.GroupId);
  if (cnt <> 4) and (cnt = 8) then Exit; //not a recognised button group

  Result := GetRectFromGroupButtons(lim, movedBtnLayer.GroupId);

  //ADJUST REC ACCORDING TO THE NEW POSITION OF MOVEDBTNLAYER

  btnMP := Point(movedBtnLayer.MidPoint);

  //it's simpler for the library user (and probably safer too)
  //if we determine sizing style here
  style := ssCorners;
  if (cnt = 4) and
    ((Round(lim[fig].MidPoint.X) = btnMP.X) and
      (Round(lim[fig+2].MidPoint.X) = btnMP.X)) or
    ((Round(lim[fig+1].MidPoint.Y) = btnMP.Y) and
      (Round(lim[fig+3].MidPoint.Y) = btnMP.Y)) then style := ssEdges;

  //btnIdx: moved button index relative to base button index
  btnIdx := movedBtnLayer.Index - fig;
  if cnt = 8 then
  begin
    case btnIdx of
      0: begin Result.Left := btnMP.X; Result.Top := btnMP.Y; end;
      1: Result.Top := btnMP.Y;
      2: begin Result.Right := btnMP.X; Result.Top := btnMP.Y; end;
      3: Result.Right := btnMP.X;
      4: begin Result.Right := btnMP.X; Result.Bottom := btnMP.Y; end;
      5: Result.Bottom := btnMP.Y;
      6: begin Result.Left := btnMP.X; Result.Bottom := btnMP.Y; end;
      7: Result.Left := btnMP.X;
    end;
  end
  else if style = ssCorners then
  begin
    case btnIdx of
      0: begin Result.Left := btnMP.X; Result.Top := btnMP.Y; end;
      1: begin Result.Right := btnMP.X; Result.Top := btnMP.Y; end;
      2: begin Result.Right := btnMP.X; Result.Bottom := btnMP.Y; end;
      3: begin Result.Left := btnMP.X; Result.Bottom := btnMP.Y; end;
    end;
  end else
  begin
    case btnIdx of
      0: Result.Top := btnMP.Y;
      1: Result.Right := btnMP.X;
      2: Result.Bottom := btnMP.Y;
      3: Result.Left := btnMP.X;
    end;
  end;

  //REPOSITION ALL BUTTONS SO THAT THEY ALIGN WITH REC
  //AND EDGE BUTTONS ARE RE-CENTERED TOO

  if cnt = 8 then
  begin
    corners := GetRectCorners(Result);
    edgeMps :=  GetRectEdgeMidPoints(Result);
    for i := 0 to 3 do
    begin
      lim[fig + i*2].PositionCenteredAt(Point(corners[i]));
      lim[fig + i*2 +1].PositionCenteredAt(Point(edgeMps[i]));
    end;
  end
  else if style = ssCorners then
  begin
    corners := GetRectCorners(Result);
    for i := 0 to 3 do
      lim[fig + i].PositionCenteredAt(Point(corners[i]));
  end else
  begin
    edgeMps :=  GetRectEdgeMidPoints(Result);
    for i := 0 to 3 do
      lim[fig + i].PositionCenteredAt(Point(edgeMps[i]));
  end;
end;
//------------------------------------------------------------------------------

function CreateButtonGroup(layeredImage32: TLayeredImage32;
  const buttonPts: TPathD; buttonColor: TColor32;
  buttonSize: integer; buttonOptions: TButtonOptions;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): integer;
var
  i: integer;
  layer: TButtonDesignerLayer32;
begin
  layer := StartButtonGroup(layeredImage32, Point(buttonPts[0]), buttonColor,
    buttonSize, buttonOptions, buttonLayerClass);
  Result := layer.GroupId;
  for i := 1 to High(buttonPts) do
    AddToButtonGroup(layeredImage32, layer.GroupId, Point(buttonPts[i]),
      buttonColor, buttonSize, buttonOptions);
end;
//------------------------------------------------------------------------------

function StartButtonGroup(layeredImage32: TLayeredImage32;
  const buttonPt: TPoint; buttonColor: TColor32;
  buttonSize: integer; buttonOptions: TButtonOptions;
  buttonLayerClass: TButtonDesignerLayer32Class = nil): TButtonDesignerLayer32;
begin
  if not assigned(buttonLayerClass) then
    buttonLayerClass := TButtonDesignerLayer32;

  Result := TButtonDesignerLayer32(layeredImage32.AddLayer(buttonLayerClass));
  Result.Name := ButtonLayerName;
  SizeAndDrawButtonLayer(Result, buttonSize, buttonColor, buttonOptions);
  Result.PositionCenteredAt(buttonPt);
  Result.CursorId := crHandPoint;
  layeredImage32.Group(Result.fIndex, Result.fIndex);
end;
//------------------------------------------------------------------------------

function AddToButtonGroup(layeredImage32: TLayeredImage32;
  GroupId: integer; const pt: TPoint; buttonColor: TColor32;
  buttonSize: integer; buttonOptions: TButtonOptions): TButtonDesignerLayer32;
var
  fig, lig: integer;
begin
  Result := nil;
  fig := layeredImage32.GetFirstInGroupIdx(GroupId);
  if fig < 0 then Exit;
  lig := layeredImage32.GetLastInGroupIdx(GroupId);
  //nb: the only way to add to a group is to temporarily ungroup
  layeredImage32.UnGroup(GroupId);
  //create a new layer of the same class type as the first button layer
  Result := TButtonDesignerLayer32(layeredImage32.InsertLayer(
    TButtonDesignerLayer32Class(layeredImage32[fig].ClassType), lig+1));
  Result.Name := ButtonLayerName;
  SizeAndDrawButtonLayer(Result, buttonSize, buttonColor, buttonOptions);
  Result.PositionCenteredAt(pt);
  Result.CursorId := crHandPoint;
  layeredImage32.Group(fig, Result.Index);
end;
//------------------------------------------------------------------------------

function AddToButtonGroup(layeredImage32: TLayeredImage32;
  GroupId: integer; const pt: TPoint): TButtonDesignerLayer32;
var
  fig, lig: integer;
  layer: TLayer32;
begin
  Result := nil;
  fig := layeredImage32.GetFirstInGroupIdx(GroupId);
  if fig < 0 then Exit;
  lig := layeredImage32.GetLastInGroupIdx(GroupId);
  layer := layeredImage32[lig];
  Result := AddToButtonGroup(layeredImage32, GroupId, pt, clNone32, 0, []);
  Result.Image.Assign(layer.Image);
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

initialization
  DefaultButtonSize := DPIAware(10);

end.
