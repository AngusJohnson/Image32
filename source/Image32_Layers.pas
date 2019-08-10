unit Image32_Layers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.17                                                            *
* Date      :  11 August 2019                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Layer support for the Image32 library                           *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  Windows, SysUtils, Classes, Math, Types, Image32;

type
  TLayer32Class = class of TLayer32;
  TLayeredImage32 = class;

  TLayer32 = class
    fOwner       : TLayeredImage32;
    fIndex       : integer;
    fImage       : TImage32;
    fPosition    : TPoint;
    fVisible     : Boolean;
    fOpacity     : Byte;
    fName        : string;
    function GetBounds: TRect;
    function GetMidPoint: TPointD;
    function GetClientMidPoint: TPointD;
    procedure SetVisible(value: Boolean);
    procedure ImageChanged(Sender: TObject);
    function GetHeight: integer;
    function GetWidth: integer;
    procedure SetOpacity(value: Byte);
  protected
    property Owner: TLayeredImage32 read fOwner;
  public
    constructor Create(owner: TLayeredImage32); virtual;
    destructor Destroy; override;
    procedure SetSize(width, height: integer);
    function RaiseUpOne: Boolean;
    function RaiseToTop: Boolean;
    function LowerDownOne: Boolean;
    function LowerToBottom: Boolean;
    procedure Move(dx, dy: integer);
    procedure PositionAt(const pt: TPoint);
    procedure PositionCenteredAt(const pt: TPoint); overload;
    procedure PositionCenteredAt(const pt: TPointD); overload;
    property Bounds: TRect read GetBounds;
    property ClientMidPoint: TPointD read GetClientMidPoint;
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

  TDesignerLayer32 = class(TLayer32)
  private
    fButtonSize: integer;
  protected
    procedure DrawDashedLine(const ctrlPts: TArrayOfPointD); virtual;
    procedure DrawGridLine(const pt1, pt2: TPointD;
      width: double; color: TColor32); virtual;
    procedure DrawButton(const pt: TPointD; color: TColor32); virtual;
  public
    constructor Create(owner: TLayeredImage32); override;
    procedure DrawGrid(majorInterval, minorInterval: integer);
    procedure DrawQSplineDesign(const ctrlPts: TArrayOfPointD);
    procedure DrawCSplineDesign(const ctrlPts: TArrayOfPointD);
    procedure DrawRectangle(const rec: TRect);
    property ButtonSize: integer read fButtonSize write fButtonSize;
  end;

  TLayeredImage32 = class
  private
    fBackground: TImage32;
    fList: TList;
    fBackColor: TColor32;
    fUpdatePending: Boolean;
    function GetCount: integer;
    function GetHeight: integer;
    procedure SetHeight(value: integer);
    function GetWidth: integer;
    procedure SetWidth(value: integer);
    function GetBounds: TRect;
    function GetMidPoint: TPointD;
    function GetMergedImage: TImage32;
    function GetLayer(index: integer): TLayer32;
  protected
    procedure UpdatePending;
  public
    procedure Clear;
    constructor Create(Width: integer = 0; Height: integer =0); virtual;
    destructor Destroy; override;
    function AddNewLayer(layerClass: TLayer32Class;
      const layerName: string = ''): TLayer32; overload;
    function AddNewLayer(const layerName: string = ''): TLayer32; overload;
    function InsertNewLayer(layerClass: TLayer32Class;
      index: integer; const layerName: string = ''): TLayer32; overload;
    function InsertNewLayer(index: integer;
      const layerName: string = ''): TLayer32; overload;
    procedure DeleteLayer(index: integer);
    function GetLayerAt(const pt: TPoint): TLayer32;
    procedure SetSize(width, height: integer);
    property BackgroundColor: TColor32 read fBackColor write fBackColor;
    property Bounds: TRect read GetBounds;
    property Count: integer read GetCount;
    property Height: integer read GetHeight write SetHeight;
    property Layer[index: integer]: TLayer32 read GetLayer; default;
    property MidPoint: TPointD read GetMidPoint;
    property MergedImage: TImage32 read GetMergedImage;
    property Width: integer read GetWidth write SetWidth;
  end;

var
  DefaultButtonSize: integer;

implementation

uses
  Image32_Draw, Image32_Vector, Image32_Extra;

resourcestring
  rsImageLayerRangeError = 'TLayeredImage32 error: index out of range.';

//------------------------------------------------------------------------------
// TLayer32 class
//------------------------------------------------------------------------------

constructor TLayer32.Create(owner: TLayeredImage32);
begin
  fImage   := TImage32.Create;
  fOwner   := owner;
  fIndex   := 0;
  fVisible := true;
  fOpacity   := 255;
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
  if Visible then
    fOwner.UpdatePending;
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
  fOwner.UpdatePending;
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

procedure TLayer32.Move(dx, dy: integer);
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

function TLayer32.RaiseUpOne: Boolean;
var
  i: integer;
  layer2: TLayer32;
begin
  result := (fIndex < fOwner.Count -1);
  if not result then Exit;
  i := fIndex;
  layer2 := fOwner[i +1];

  fOwner.fList[i] := layer2;
  layer2.fIndex := i;
  fOwner.fList[i+1] := self;
  self.fIndex := i+1;
  fOwner.UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayer32.RaiseToTop: Boolean;
var
  i, highI: integer;
  layer2: TLayer32;
begin
  highI := fOwner.Count -1;
  result := (fIndex < highI);
  if not result then Exit;
  for i := fIndex +1 to highI do
  begin
    layer2 := fOwner.Layer[i];
    layer2.fIndex := i -1;
    fOwner.fList[i-1] := layer2;
  end;
  fOwner.fList[highI] := self;
  self.fIndex := highI;
  fOwner.UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayer32.LowerDownOne: Boolean;
var
  i: integer;
  layer2: TLayer32;
begin
  result := (fIndex > 0);
  if not result then Exit;
  i := fIndex;
  layer2 := fOwner[i -1];

  fOwner.fList[i] := layer2;
  layer2.fIndex := i;
  fOwner.fList[i-1] := self;
  self.fIndex := i-1;
  fOwner.UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayer32.LowerToBottom: Boolean;
var
  i: integer;
  layer2: TLayer32;
begin
  result := (fIndex > 0);
  if not result then Exit;
  for i := fIndex -1 downto 0 do
  begin
    layer2 := fOwner.Layer[i];
    layer2.fIndex := i +1;
    fOwner.fList[i+1] := layer2;
  end;
  fOwner.fList[0] := self;
  self.fIndex := 0;
  fOwner.UpdatePending;
end;

//------------------------------------------------------------------------------
// TDesignerLayer32 class
//------------------------------------------------------------------------------

constructor TDesignerLayer32.Create(owner: TLayeredImage32);
begin
  inherited;
  fButtonSize := DefaultButtonSize;
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawDashedLine(const ctrlPts: TArrayOfPointD);
begin
  Image32_Draw.DrawDashedLine(Image, ctrlPts,
    MakeArrayOfInteger([4,4]), nil, 1, clRed32, esSquare);
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
  SetLength(path, 2);
  path[0] := ctrlPts[0];
  path[1] := ctrlPts[1];
  DrawDashedLine(path);
  pt := ctrlPts[1];
  for i := 2 to len -2 do
  begin
    pt2 := ReflectPoint(pt, ctrlPts[i]);
    path[0] := pt;
    path[1] := pt2;
    DrawDashedLine(path);
    DrawButton(pt2, clNone32);
    pt := pt2;
  end;
  path[0] := pt; path[1] := ctrlPts[len-1];
  DrawDashedLine(path);
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
  SetLength(path, 2);
  path[0] := ctrlPts[0];
  path[1] := ctrlPts[1];
  DrawDashedLine(path);
  i := 2;
  while i < len -2 do
  begin
    pt := ReflectPoint(ctrlPts[i], ctrlPts[i+1]);
    path[0] := ctrlPts[i];
    path[1] := pt;
    DrawDashedLine(path);
    DrawButton(pt, clNone32);
    inc(i, 2);
  end;
  path[0] := ctrlPts[len-2];
  path[1] := ctrlPts[len-1];
  DrawDashedLine(path);
end;
//------------------------------------------------------------------------------

procedure TDesignerLayer32.DrawRectangle(const rec: TRect);
var
  path: TArrayOfPointD;
begin
  path := Rectangle(rec);
  DrawDashedLine(path);
end;

//------------------------------------------------------------------------------
// TLayeredImage32 class
//------------------------------------------------------------------------------

constructor TLayeredImage32.Create(Width: integer; Height: integer);
begin
  fList := TList.Create;
  fBackground := TImage32.Create(Width, Height);
end;
//------------------------------------------------------------------------------

destructor TLayeredImage32.Destroy;
begin
  Clear;
  fList.Free;
  fBackground.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetSize(width, height: integer);
begin
  fBackground.SetSize(width, height);
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayer(index: integer): TLayer32;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsImageLayerRangeError);
  Result := TLayer32(fList[index]);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.UpdatePending;
begin
  fUpdatePending := true;
end;
//------------------------------------------------------------------------------

procedure FracAlpha(image: TImage32; opacity: byte);
var
  i: Integer;
  pb: PARGB;
  alphaTbl: TArrayOfByte;
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

function TLayeredImage32.GetMergedImage: TImage32;
var
  i: integer;
  dstRec: TRect;
  blendFunc: TBlendFunction;
  tmp: TImage32;
begin
  if fUpdatePending then
  begin
    fBackground.FillRect(fBackground.Bounds, fBackColor);

    if fBackColor shr 24 < 254 then //ie semi-transparent
      blendFunc  := BlendToAlpha else
      blendFunc := BlendToOpaque;

    tmp := TImage32.Create;
    try
      for i := 0 to Count -1 do
      begin
        with Layer[i] do
        begin
          if not Visible or (fOpacity < 2) then Continue;
          dstRec := Types.Rect(fPosition.X, fPosition.Y,
            fPosition.X + Image.Width, fPosition.Y + Image.Height);
          if fOpacity < 254 then
          begin
            //reduce layer opacity
            tmp.Assign(Image);
            FracAlpha(tmp, fOpacity);
            fBackground.CopyFrom(tmp, tmp.Bounds, dstRec, blendFunc);
          end else
            fBackground.CopyFrom(Image, Image.Bounds, dstRec, blendFunc);
        end;
      end;
    finally
      tmp.Free;
    end;
    fUpdatePending := false;
  end;
  Result := fBackground;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.Clear;
var
  i: integer;
begin
  fBackground.Clear;
  for i := 0 to Count -1 do
    TLayer32(fList[i]).free;
  fList.Clear;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetCount: integer;
begin
  Result := fList.Count;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetHeight: integer;
begin
  Result := fBackground.Height;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetHeight(value: integer);
begin
  if fBackground.Height = value then Exit;
  fBackground.SetSize(Width, value);
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetWidth: integer;
begin
  Result := fBackground.Width;
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.SetWidth(value: integer);
begin
  if fBackground.Width = value then Exit;
  fBackground.SetSize(value, Height);
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetBounds: TRect;
begin
  Result := Types.Rect(0, 0, Width, Height);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetMidPoint: TPointD;
begin
  Result := PointD(fBackground.Width * 0.5, fBackground.Height * 0.5);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.AddNewLayer(layerClass: TLayer32Class;
  const layerName: string = ''): TLayer32;
var
  i: integer;
begin
  i := Count;
  result := layerClass.Create(self);
  fList.Add(Result);
  Result.fName := layerName;
  Result.fIndex := i;
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.AddNewLayer(const layerName: string = ''): TLayer32;
begin
  Result := AddNewLayer(TLayer32, layerName);
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertNewLayer(layerClass: TLayer32Class;
  index: integer; const layerName: string = ''): TLayer32;
var
  i: integer;
begin
  if (index < 0) or (index > Count) then
    raise Exception.Create(rsImageLayerRangeError);
  Result := layerClass.Create(Self);
  Result.Name := layerName;
  fList.Insert(index, Result);
  for i := index to Count -1 do Layer[i].fIndex := i;
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.InsertNewLayer(index: integer;
  const layerName: string = ''): TLayer32;
begin
  Result := InsertNewLayer(TLayer32, index, layerName);
end;
//------------------------------------------------------------------------------

procedure TLayeredImage32.DeleteLayer(index: integer);
var
  i: integer;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsImageLayerRangeError);
  TLayer32(fList[index]).Free;
  fList.Delete(index);
  for i := index to Count -1 do
    Layer[i].fIndex := i;
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TLayeredImage32.GetLayerAt(const pt: TPoint): TLayer32;
var
  i: integer;
begin
  for i := Count -1 downto 0 do
    with Layer[i] do
      if PtInRect(Bounds, pt) then
      begin
        Result := Layer[i];
        Exit;
      end;
  Result := nil;
end;
//------------------------------------------------------------------------------

initialization
  DefaultButtonSize := DPI(11);

end.
