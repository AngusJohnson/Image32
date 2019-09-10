unit Image32_Layers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.22                                                            *
* Date      :  10 September 2019                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Layer support for the Image32 library                           *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  Windows, SysUtils, Classes, Math, Types, Image32, Image32_Extra;

type
  TSizingStyle = (ssCorners, ssEdges, ssEdgesAndCorners, ssCustom);
  TMergeImageState = (misUnknown, misMergeAll, misHideDesigners);

  TLayer32Class = class of TLayer32;
  TLayeredImage32 = class;

  TLayer32 = class
  private
    fOwner       : TLayeredImage32;
    fIndex       : integer;
    fImage       : TImage32;
    fPosition    : TPoint;
    fVisible     : Boolean;
    fOpacity     : Byte;
    fName        : string;
    fGroupIndex  : integer;
    fCursorId    : integer;
    function GetBounds: TRect;
    function GetMidPoint: TPointD;
    function GetClientMidPoint: TPointD;
    procedure SetVisible(value: Boolean);
    procedure ImageChanged(Sender: TObject);
    function GetHeight: integer;
    function GetWidth: integer;
    function GetIsInGroup: Boolean;
    procedure SetOpacity(value: Byte);
  protected
    property Owner: TLayeredImage32 read fOwner;
    property IsInGroup: Boolean read GetIsInGroup;
  public
    constructor Create(owner: TLayeredImage32); virtual;
    destructor Destroy; override;
    procedure SetSize(width, height: integer);
    function BringForward(newLevel: integer): Boolean;
    function SendBack(newLevel: integer): Boolean;
    procedure Offset(dx, dy: integer);
    procedure PositionAt(const pt: TPoint);
    procedure PositionCenteredAt(const pt: TPoint); overload;
    procedure PositionCenteredAt(const pt: TPointD); overload;

    property Bounds: TRect read GetBounds;
    property ClientMidPoint: TPointD read GetClientMidPoint;
    //CursorId: see VCL.Controls.TCursor
    property CursorId: integer read fCursorId write fCursorId;
    property GroupIndex: integer read fGroupIndex;
    property Image: TImage32 read fImage;
    property Index: integer read fIndex;
    property Height: integer read GetHeight;
    property Left: integer read fPosition.X;
    property MidPoint: TPointD read GetMidPoint;
    property Name: string read fName write fName;
    property Opacity: Byte read fOpacity write SetOpacity;
    property Top: integer read fPosition.Y;
    property Visible: Boolean read fVisible write SetVisible;
    property Width: integer read GetWidth;
  end;

  //TCustomDesignerLayer32 behaves exactly like TLayer32 except this class and
  //its descendants can be excluded from TLayeredImage32.GetMergedImage (when
  //the HideDesigners parameter is true). However, unlike its descendant class
  //TDesignerLayer32, it will still be visible to TLayeredImage32.GetLayerAt
  //which makes it the ideal class for designer button control layers.
  TCustomDesignerLayer32 = class(TLayer32);

  TDesignerLayer32 = class(TCustomDesignerLayer32)
  private
    fPenWidth: double;
    fPenColor: TColor32;
    fButtonSize: Double;
  protected
    procedure DrawDashedLine(const ctrlPts: TArrayOfPointD; closed: Boolean); virtual;
    procedure DrawGridLine(const pt1, pt2: TPointD;
      width: double; color: TColor32); virtual;
    procedure DrawButton(const pt: TPointD; color: TColor32); virtual;
  public
    constructor Create(owner: TLayeredImage32); override;
    procedure DrawGrid(majorInterval, minorInterval: integer);
    procedure DrawQSplineDesign(const ctrlPts: TArrayOfPointD);
    procedure DrawCSplineDesign(const ctrlPts: TArrayOfPointD);
    procedure DrawRectangle(const rec: TRect);
    procedure DrawEllipse(const rec: TRect);
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
    function AddNewLayer(layerClass: TLayer32Class;
      const layerName: string = ''): TLayer32; overload;
    function AddNewLayer(const layerName: string = ''): TLayer32; overload;
    function InsertNewLayer(layerClass: TLayer32Class;
      index: integer; const layerName: string = ''): TLayer32; overload;
    function InsertNewLayer(index: integer;
      const layerName: string = ''): TLayer32; overload;
    procedure DeleteLayer(index: integer);
    procedure Clear;
    procedure Invalidate;
    function GetLayerNamed(const name: string): TLayer32;
    function GetLayerAt(const pt: TPoint): TLayer32;
    function GetMergedImage(hideDesigners: Boolean): TImage32;

    function Group(startIdx, endIdx: integer): Boolean;
    procedure UnGroup(groupIdx: integer);
    function CountLayersInGroup(groupIdx: integer): integer;
    function GetIdxFirstLayerInGroup(groupIdx: integer): integer;
    function GetIdxLastLayerInGroup(groupIdx: integer): integer;
    function BringGroupForward(groupIdx, newLevel: integer): Boolean;
    function SendGroupBack(groupIdx, newLevel: integer): Boolean;
    procedure OffsetGroup(groupIdx, dx, dy: integer);
    procedure DeleteGroup(groupIdx: integer);
    procedure HideGroup(groupIdx: integer);
    procedure ShowGroup(groupIdx: integer);

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
    buttonSize: integer; buttonOptions: TButtonOptions): Boolean;
  function UpdateSizingGroup(targetLayer, movedBtnLayer: TLayer32;
    masterImg: TImage32): Boolean;

  procedure CreateButtonGroup(layeredImage32: TLayeredImage32;
    const buttonPts: TArrayOfPointD; buttonColor: TColor32;
    buttonSize: integer; buttonOptions: TButtonOptions);
  procedure AddToButtonGroup(layeredImage32: TLayeredImage32;
    groupIdx: integer; const pt: TPoint);

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
    'TLayer32 delete error: layers must be ungrouped before they can be '+
    'deleted separately. Otherwise, use TLayeredImage32.DeleteGroup.';
  rsImageLayerRangeError =
    'TLayeredImage32 error: index out of range.';
  rsImageLayerAddInsertDesignerError =
    'TDesignerLayer32 error: This layer must be added/inserted immediately' +
    'above a ''normal'' layer.';
  rsImageLayerAddButtonsError =
    'TDesignerLayer32 error: There''s a problem matching the designer layer'+
    'with its target layer while adding buttons.';

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetRectCorners(const rec: TRect): TArrayOfPointD;
begin
  Result := Rectangle(rec);
end;
//------------------------------------------------------------------------------

function GetRectEdgeMidPoints(const rec: TRect): TArrayOfPointD;
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
  fGroupIndex := 0;
  fCursorId := 0;
  fImage.OnChange := ImageChanged;
end;
//------------------------------------------------------------------------------

destructor TLayer32.Destroy;
begin
 fImage.Free;
 inherited;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetSize(width, height: integer);
begin
  fImage.SetSize(width, height);
end;
//------------------------------------------------------------------------------

function TLayer32.GetIsInGroup: Boolean;
begin
  Result := fGroupIndex > 0;
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
  PositionAt(Point(Left + dx, Top + dy));
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetVisible(value: Boolean);
begin
  if (value = fVisible) or (value and fImage.IsEmpty) then Exit;
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
  Result := (newLevel > fIndex) and (fIndex < topIdx) and not IsInGroup;
  if not Result then Exit;

  if fOwner[newLevel].IsInGroup then
    while (newLevel < topIdx) and
      (fOwner[newLevel].fGroupIndex = fOwner[newLevel+1].fGroupIndex) do
        inc(newLevel);

  fOwner.fList.Move(fIndex, newLevel);
  fOwner.ReIndexFrom(fIndex, newLevel);
  fOwner.Invalidate;
end;
//------------------------------------------------------------------------------

function TLayer32.SendBack(newLevel: integer): Boolean;
begin
  Result := (newLevel < fIndex) and (fIndex > 0) and not IsInGroup;
  if not Result then Exit;

  while fOwner[newLevel].IsInGroup and (newLevel > 0) and
    (fOwner[newLevel].fGroupIndex = fOwner[newLevel-1].fGroupIndex) do
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
  fButtonSize := DefaultButtonSize;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawDashedLine(const ctrlPts: TArrayOfPointD;
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
  path: TArrayOfPointD;
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

procedure TDesignerLayer32.DrawButton(const pt: TPointD; color: TColor32);
begin
  Image32_Extra.DrawButton(image, pt, fButtonSize, color, []);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawQSplineDesign(const ctrlPts: TArrayOfPointD);
var
  i, len: integer;
  pt, pt2: TPointD;
  path: TArrayOfPointD;
begin
  len := length(ctrlPts);
  if len < 3 then Exit;
  if fButtonSize < 2 then fButtonSize := fButtonSize;
  SetLength(path, 2);
  path[0] := ctrlPts[0];
  path[1] := ctrlPts[1];
  DrawDashedLine(path, false);
  pt := ctrlPts[1];
  for i := 2 to len -2 do
  begin
    pt2 := ReflectPoint(pt, ctrlPts[i]);
    path[0] := pt;
    path[1] := pt2;
    DrawDashedLine(path, false);
    DrawButton(pt2, clNone32);
    pt := pt2;
  end;
  path[0] := pt; path[1] := ctrlPts[len-1];
  DrawDashedLine(path, false);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawCSplineDesign(const ctrlPts: TArrayOfPointD);
var
  i, len: integer;
  pt: TPointD;
  path: TArrayOfPointD;
begin
  len := length(ctrlPts);
  if Odd(len) then dec(len);
  if len < 4 then Exit;
  if fButtonSize < 2 then fButtonSize := DefaultButtonSize;
  SetLength(path, 2);
  path[0] := ctrlPts[0];
  path[1] := ctrlPts[1];
  DrawDashedLine(path, false);
  i := 2;
  while i < len -2 do
  begin
    pt := ReflectPoint(ctrlPts[i], ctrlPts[i+1]);
    path[0] := ctrlPts[i];
    path[1] := pt;
    DrawDashedLine(path, false);
    DrawButton(pt, clNone32);
    inc(i, 2);
  end;
  path[0] := ctrlPts[len-2];
  path[1] := ctrlPts[len-1];
  DrawDashedLine(path, false);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawRectangle(const rec: TRect);
var
  path: TArrayOfPointD;
begin
  path := Rectangle(rec);
  DrawDashedLine(path, true);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawEllipse(const rec: TRect);
var
  path: TArrayOfPointD;
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
  i: integer;
begin
  if stop < 0 then stop := Count -1
  else stop := Min(stop, Count -1);
  for i := start to stop do
    TLayer32(fList[i]).fIndex := i;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.Invalidate;
begin
  fUpdatePending := true;
end;
//------------------------------------------------------------------------------

procedure FracAlpha(image: TImage32; opacity: byte);
var
  i: Integer;
  pb: PARGB;
  alphaTbl: PByteArray;
begin
  pb := PARGB(image.PixelBase);
  alphaTbl := @MulTable[opacity];
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
        if not Visible or (fOpacity < 2) or
          (hideDesigners and (layer[i] is TCustomDesignerLayer32)) then
            Continue;

        if fOpacity < 254 then
        begin
          //reduce layer opacity
          tmp := TImage32.Create(Image);
          try
            FracAlpha(tmp, fOpacity);
            fMergedImage.CopyFrom(tmp, tmp.Bounds, Bounds, blendFunc);
          finally
            tmp.Free;
          end;
        end else
          fMergedImage.CopyFrom(Image, Image.Bounds, Bounds, blendFunc);
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

function TLayeredImage32.AddNewLayer(layerClass: TLayer32Class;
  const layerName: string = ''): TLayer32;
begin
  Result := InsertNewLayer(layerClass, Count, layerName);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.AddNewLayer(const layerName: string = ''): TLayer32;
begin
  Result := AddNewLayer(TLayer32, layerName);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertNewLayer(index: integer;
  const layerName: string = ''): TLayer32;
begin
  Result := InsertNewLayer(TLayer32, index, layerName);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertNewLayer(layerClass: TLayer32Class;
  index: integer; const layerName: string = ''): TLayer32;
var
  i: integer;
begin
  if (index < 0) or (index > Count) then
    raise Exception.Create(rsImageLayerRangeError);

  //make sure a new layer isn't inserted into the middle of a group
  if (index < Count) and Layer[index].IsInGroup then
    while (index > 0) and
      (Layer[index].fGroupIndex = Layer[index-1].fGroupIndex) do
        dec(index);

  //insert new layer into fList and update indexes
  Result := layerClass.Create(Self);
  Result.Name := layerName;
  fList.Insert(index, Result);
  for i := index to Count -1 do Layer[i].fIndex := i;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteLayer(index: integer);
var
  i: integer;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsImageLayerRangeError);
  if Layer[index].IsInGroup then
    raise Exception.Create(rsLayer32DeleteError);
  Layer[index].Free;
  fList.Delete(index);
  //update indexes
  for i := index to Count -1 do
    Layer[i].fIndex := i;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayerNamed(const name: string): TLayer32;
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
  //GetLayerAt ignores TDesignerLayer32 layers but not
  //TCustomDesignerLayer32 layers (eg buttons)
  for i := Count -1 downto 0 do
    if (Layer[i] is TDesignerLayer32) or not Layer[i].Visible then continue
    else with Layer[i] do
      if PtInRect(Bounds, pt) then
      begin
        Result := Layer[i];
        Exit;
      end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.Group(startIdx, endIdx: integer): Boolean;
var
  i, groupIdx: integer;
begin
  Result := false;
  startIdx := Max(0, startIdx);
  endIdx := Min(Count -1, endIdx);
  for i := startIdx to endIdx do
    if Layer[i].IsInGroup then Exit;

  //first look for an expired group (negative values => expired group)
  groupIdx := fGroupList.Count +1;
  for i := 0 to fGroupList.Count -1 do
    if integer(fGroupList[i]) < 0 then
    begin
      groupIdx := i +1;
      fGroupList[i] := Pointer(startIdx);
    end;
  //if no expired groups then add the new group index
  if groupIdx > fGroupList.Count then
    fGroupList.Add(pointer(startIdx));

  for i := startIdx to endIdx do
    Layer[i].fGroupIndex := groupIdx;
  Result := true;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.UnGroup(groupIdx: integer);
var
  i, fig, lig: integer;
begin
  fig := GetIdxFirstLayerInGroup(groupIdx);
  lig := GetIdxLastLayerInGroup(groupIdx);
  for i := fig to lig do
    Layer[i].fGroupIndex := 0;
  fGroupList[groupIdx -1] := pointer(-1);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetIdxFirstLayerInGroup(groupIdx: integer): integer;
begin
  Result := integer(fGroupList[groupIdx -1]);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetIdxLastLayerInGroup(groupIdx: integer): integer;
var
  topLevel: integer;
begin
  Result := GetIdxFirstLayerInGroup(groupIdx);
  topLevel := Count -1;
  while (Result < topLevel) and (Layer[Result +1].fGroupIndex = groupIdx) do
    inc(Result);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.CountLayersInGroup(groupIdx: integer): integer;
begin
  Result := GetIdxLastLayerInGroup(groupIdx) - GetIdxFirstLayerInGroup(groupIdx) +1;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.OffsetGroup(groupIdx, dx, dy: integer);
var
  i, fig, lig: integer;
begin
  fig := GetIdxFirstLayerInGroup(groupIdx);
  lig := GetIdxLastLayerInGroup(groupIdx);
  //nb: it almost never makes sense offseting design layers
  for i := fig to lig do
    if not (Layer[i] is TDesignerLayer32) then
      Layer[i].Offset(dx, dy);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.BringGroupForward(groupIdx, newLevel: integer): Boolean;
var
  i, fig, lig, cnt, topIdx: integer;
begin
  topIdx := Count -1;
  fig := GetIdxFirstLayerInGroup(groupIdx);
  lig := GetIdxLastLayerInGroup(groupIdx);
  cnt := lig - fig +1;
  result := (newLevel > lig) and (newLevel <= topIdx);
  if not result then Exit;
  //if newLevel is inside another group, adjust newLevel to above that group
  if Layer[newLevel].IsInGroup then
    while (newLevel < topIdx) and
      (Layer[newLevel].fGroupIndex = Layer[newLevel+1].fGroupIndex) do
        inc(newLevel);
  for i := 1 to cnt do
    fList.Move(fig, newLevel);
  fGroupList[groupIdx -1] := pointer(newLevel);
  ReIndexFrom(fig, newLevel + cnt);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.SendGroupBack(groupIdx, newLevel: integer): Boolean;
var
  i, fig, lig: integer;
begin
  fig := GetIdxFirstLayerInGroup(groupIdx);
  lig := GetIdxLastLayerInGroup(groupIdx);
  Result := (newLevel < fig) and (newLevel >= 0);
  if not Result then Exit;
  if Layer[newLevel].IsInGroup then
    while (newLevel > 0) and
      (Layer[newLevel].fGroupIndex = Layer[newLevel-1].fGroupIndex) do
        dec(newLevel);
  for i := fig to lig do
    fList.Move(lig, newLevel);
  fGroupList[groupIdx -1] := pointer(newLevel);
  ReIndexFrom(newLevel, lig);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteGroup(groupIdx: integer);
var
  i, fig, lig: integer;
begin
  fig := GetIdxFirstLayerInGroup(groupIdx);
  lig := GetIdxLastLayerInGroup(groupIdx);
  for i := lig downto fig do
  begin
    Layer[i].Free;
    fList.Delete(i);
  end;
  //update indexes
  for i := fig to Count -1 do
    Layer[i].fIndex := i;
  //remove group from fGroupList
  fGroupList[groupIdx -1] := pointer(-1);

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.HideGroup(groupIdx: integer);
var
  i, fig, lig: integer;
begin
  fig := GetIdxFirstLayerInGroup(groupIdx);
  lig := GetIdxLastLayerInGroup(groupIdx);
  for i := fig to lig do
    Layer[i].Visible := false;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.ShowGroup(groupIdx: integer);
var
  i, fig, lig: integer;
begin
  fig := GetIdxFirstLayerInGroup(groupIdx);
  lig := GetIdxLastLayerInGroup(groupIdx);
  for i := fig to lig do
    Layer[i].Visible := true;
end;

//------------------------------------------------------------------------------
// Interface functions
//------------------------------------------------------------------------------

function GetRectFromGroupButtons(layeredImage32: TLayeredImage32;
  groupIdx: integer): TRect;
var
  fig, lig: integer;
  pt: TPoint;
begin
  Result := NullRect;
  fig := layeredImage32.GetIdxFirstLayerInGroup(groupIdx);
  lig := layeredImage32.GetIdxLastLayerInGroup(groupIdx);
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

function CreateSizingBtnsGroup(targetLayer: TLayer32;
  style: TSizingStyle; buttonColor: TColor32;
  buttonSize: integer; buttonOptions: TButtonOptions): Boolean;
var
  i, btnSizeEx, startGroupIdx: integer;
  rec: TRect;
  corners, edges: TArrayOfPointD;
  mp: TPointD;
  layer: TLayer32;
  lim: TLayeredImage32;
const
  cnrCursorIds: array [0..3] of integer =
    (crSizeNWSE, crSizeNESW, crSizeNWSE, crSizeNESW);
  edgeCursorIds: array [0..3] of integer =
    (crSizeNS, crSizeWE, crSizeNS, crSizeWE);
begin
  result := not targetLayer.IsInGroup and not targetLayer.Image.IsEmpty;
  if not result then Exit;

  lim := targetLayer.fOwner;
  startGroupIdx := lim.Count;
  if style = ssCustom then style := ssEdgesAndCorners;

  rec := targetLayer.Bounds;
  corners := Rectangle(rec);
  edges := GetRectEdgeMidPoints(rec);

  if buttonSize < 2 then buttonSize := DefaultButtonSize;
  //allow room for a button drop shadow
  btnSizeEx := Ceil(DefaultButtonSize * 1.25);
  //buttons must have even numbered width and height so
  //its rounded Midpoint will be accurate (see PositionCenteredAt)
  //which is important when moving them (and adjusting image dimensions)
  if Odd(btnSizeEx) then inc(btnSizeEx);
  mp := PointD(btnSizeEx/2, btnSizeEx/2);
  case style of
    ssCorners:
      for i := 0 to 3 do
      begin
        layer := lim.AddNewLayer(TDesignerLayer32, ButtonLayerName);
        layer.SetSize(btnSizeEx, btnSizeEx);
        layer.PositionCenteredAt(Point(corners[i]));
        layer.CursorId := cnrCursorIds[i];
        Image32_Extra.DrawButton(layer.Image,
          mp, buttonSize, buttonColor, buttonOptions);
      end;
    ssEdges:
      for i := 0 to 3 do
      begin
        layer := lim.AddNewLayer(TCustomDesignerLayer32, ButtonLayerName);
        layer.SetSize(btnSizeEx, btnSizeEx);
        layer.PositionCenteredAt(Point(edges[i]));
        layer.CursorId := edgeCursorIds[i];
        Image32_Extra.DrawButton(layer.Image,
          mp, buttonSize, buttonColor, buttonOptions);
      end;
    else
      for i := 0 to 3 do
      begin
        layer := lim.AddNewLayer(TCustomDesignerLayer32, ButtonLayerName);
        layer.SetSize(btnSizeEx, btnSizeEx);
        layer.PositionCenteredAt(Point(corners[i]));
        layer.CursorId := cnrCursorIds[i];
        Image32_Extra.DrawButton(layer.Image,
          mp, buttonSize, buttonColor, buttonOptions);

        layer := lim.AddNewLayer(TCustomDesignerLayer32, ButtonLayerName);
        layer.SetSize(btnSizeEx, btnSizeEx);
        layer.PositionCenteredAt(Point(edges[i]));
        layer.CursorId := edgeCursorIds[i];
        Image32_Extra.DrawButton(layer.Image,
          mp, buttonSize, buttonColor, buttonOptions);
      end;
  end;
  lim.Group(startGroupIdx, layer.fIndex);
end;
//------------------------------------------------------------------------------

function UpdateSizingGroup(targetLayer, movedBtnLayer: TLayer32;
    masterImg: TImage32): Boolean;
var
  i, btnIdx, cnt, fig: integer;
  lim: TLayeredImage32;
  rec: TRect;
  btnMP, recMP: TPoint;
  corners, edgeMps: TArrayOfPointD;
  style: TSizingStyle;
begin
  result := movedBtnLayer.IsInGroup;
  if not result then Exit;
  lim := movedBtnLayer.Owner;
  fig := lim.GetIdxFirstLayerInGroup(movedBtnLayer.GroupIndex);
  cnt := lim.CountLayersInGroup(movedBtnLayer.GroupIndex);

  result := (cnt = 4) or (cnt = 8);
  if not result then Exit;
  rec := GetRectFromGroupButtons(lim, movedBtnLayer.GroupIndex);

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
      0: begin rec.Left := btnMP.X; rec.Top := btnMP.Y; end;
      1: rec.Top := btnMP.Y;
      2: begin rec.Right := btnMP.X; rec.Top := btnMP.Y; end;
      3: rec.Right := btnMP.X;
      4: begin rec.Right := btnMP.X; rec.Bottom := btnMP.Y; end;
      5: rec.Bottom := btnMP.Y;
      6: begin rec.Left := btnMP.X; rec.Bottom := btnMP.Y; end;
      7: rec.Left := btnMP.X;
    end;
  end
  else if style = ssCorners then
  begin
    case btnIdx of
      0: begin rec.Left := btnMP.X; rec.Top := btnMP.Y; end;
      1: begin rec.Right := btnMP.X; rec.Top := btnMP.Y; end;
      2: begin rec.Right := btnMP.X; rec.Bottom := btnMP.Y; end;
      3: begin rec.Left := btnMP.X; rec.Bottom := btnMP.Y; end;
    end;
  end else
  begin
    case btnIdx of
      0: rec.Top := btnMP.Y;
      1: rec.Right := btnMP.X;
      2: rec.Bottom := btnMP.Y;
      3: rec.Left := btnMP.X;
    end;
  end;
  recMP := MidPoint(rec);

  //REPOSITION ALL BUTTONS SO THAT THEY ALIGN WITH REC
  //AND EDGE BUTTONS ARE RE-CENTERED TOO

  if cnt = 8 then
  begin
    corners := GetRectCorners(rec);
    edgeMps :=  GetRectEdgeMidPoints(rec);
    for i := 0 to 3 do
    begin
      lim[fig + i*2].PositionCenteredAt(Point(corners[i]));
      lim[fig + i*2 +1].PositionCenteredAt(Point(edgeMps[i]));
    end;
  end
  else if style = ssCorners then
  begin
    corners := GetRectCorners(rec);
    for i := 0 to 3 do
      lim[fig + i].PositionCenteredAt(Point(corners[i]));
  end else
  begin
    edgeMps :=  GetRectEdgeMidPoints(rec);
    for i := 0 to 3 do
      lim[fig + i].PositionCenteredAt(Point(edgeMps[i]));
  end;

  //copy the master image to the target image, then resize and reposition
  targetLayer.Image.Assign(masterImg);
  targetLayer.Image.Resize(RectWidth(rec), RectHeight(rec));
  targetLayer.PositionAt(rec.TopLeft);
end;
//------------------------------------------------------------------------------

procedure CreateButtonGroup(layeredImage32: TLayeredImage32;
  const buttonPts: TArrayOfPointD; buttonColor: TColor32;
  buttonSize: integer; buttonOptions: TButtonOptions);
var
  i, btnSizeEx, startGroupIdx: integer;
  mp: TPointD;
  layer: TLayer32;
begin
  if length(buttonPts) = 0 then Exit;
  if buttonSize < 2 then buttonSize := DefaultButtonSize;
  //allow room for a button drop shadow
  btnSizeEx := Ceil(DefaultButtonSize * 1.25);
  //buttons must have even numbered width and height so
  //its rounded Midpoint will be accurate (see PositionCenteredAt)
  //which is important when moving them (and adjusting image dimensions)
  if Odd(btnSizeEx) then inc(btnSizeEx);
  mp := PointD(btnSizeEx/2, btnSizeEx/2);

  layer := nil; //avoids a warning
  startGroupIdx := layeredImage32.Count;
  for i := 0 to high(buttonPts) do
  begin
    layer := layeredImage32.AddNewLayer(TCustomDesignerLayer32, ButtonLayerName);
    layer.SetSize(btnSizeEx, btnSizeEx);
    layer.PositionCenteredAt(Point(buttonPts[i]));
    layer.CursorId := crHandPoint;
    Image32_Extra.DrawButton(layer.Image,
      mp, buttonSize, buttonColor, buttonOptions);
  end;
  layeredImage32.Group(startGroupIdx, layer.fIndex);
end;
//------------------------------------------------------------------------------

procedure AddToButtonGroup(layeredImage32: TLayeredImage32;
  groupIdx: integer; const pt: TPoint);
var
  newLayer: TLayer32;
  fig, lig: integer;
begin
  fig := layeredImage32.GetIdxFirstLayerInGroup(groupIdx);
  lig := layeredImage32.GetIdxLastLayerInGroup(groupIdx);
  //nb: the only way to add to a group is to temporarily ungroup
  layeredImage32.UnGroup(groupIdx);
  newLayer :=
    layeredImage32.InsertNewLayer(TCustomDesignerLayer32, lig+1, ButtonLayerName);
  newLayer.Image.Assign(layeredImage32[lig].Image);
  newLayer.PositionCenteredAt(pt);
  newLayer.CursorId := crHandPoint;
  layeredImage32.Group(fig, newLayer.Index);
end;
//------------------------------------------------------------------------------

initialization
  DefaultButtonSize := DPI(10);

end.
