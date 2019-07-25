unit Image32_Layers;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.10                                                            *
* Date      :  23 July 2019                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Layer support for the Image32 library                           *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  Windows, SysUtils, Classes, Math, Image32;

type
  TImageLayers32 = class;

  TLayer32 = class
    fOwner: TImageLayers32;
    fIndex: integer;
    fImage: TImage32;
    fPosition: TPoint;
    fVisible: Boolean;
    procedure SetPosition(const pt: TPoint);
    procedure SetVisible(value: Boolean);
    procedure ImageChanged(Sender: TObject);
  public
    constructor Create(owner: TImageLayers32; index: integer); virtual;
    destructor Destroy; override;
    function RaiseOne: Boolean;
    function RaiseTop: Boolean;
    function LowerOne: Boolean;
    function LowerBottom: Boolean;
    property Image: TImage32 read fImage;
    property Index: integer read fIndex;
    //Position: Layers will be clipped to the bounds of TImageLayers32.
    property Position: TPoint read fPosition write SetPosition;
    //Visible: when false the TImageLayers32 owner will still measure this layer
    property Visible: Boolean read fVisible write SetVisible;
  end;

  TImageLayers32 = class
  private
    fImageBase: TImage32;
    fList: TList;
    fBackColor: TColor32;
    fUpdatePending: Boolean;
    function GetCount: integer;
    function GetHeight: integer;
    procedure SetHeight(value: integer);
    function GetWidth: integer;
    procedure SetWidth(value: integer);
    procedure Update;
    function GetMergedLayers: TImage32;
    function GetLayer(index: integer): TLayer32;
  protected
    procedure UpdatePending;
  public
    //Clear: deletes all layers
    procedure Clear;
    constructor Create; virtual;
    destructor Destroy; override;
    function AddNewLayer: TLayer32;
    function InsertNewLayer(index: integer): TLayer32;
    procedure DeleteLayer(index: integer);
    procedure SetSize(width, height: integer);
    //BackgroundColor: default = clNone32.
    property BackgroundColor: TColor32 read fBackColor write fBackColor;
    property Count: integer read GetCount;
    //Height: Setting Height will be ignored when AutoSize = true
    property Height: integer read GetHeight write SetHeight;
    property Layer[index: integer]: TLayer32 read GetLayer; default;
    property MergedLayers: TImage32 read GetMergedLayers;
    //Width: Setting Width will be ignored when AutoSize = true
    property Width: integer read GetWidth write SetWidth;
  end;

implementation

resourcestring
  rsImageLayerRangeError = 'TImageLayer32 error: Invalid layer index.';
//------------------------------------------------------------------------------
// TLayer32 class
//------------------------------------------------------------------------------

constructor TLayer32.Create(owner: TImageLayers32; index: integer);
begin
  fImage := TImage32.Create;
  fOwner := owner;
  fIndex := index;
  fImage.OnChange := ImageChanged;
end;
//------------------------------------------------------------------------------

destructor TLayer32.Destroy;
begin
 fImage.Free;
 inherited;
end;
//------------------------------------------------------------------------------

procedure TLayer32.ImageChanged(Sender: TObject);
begin
  if Visible then
    fOwner.UpdatePending;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetPosition(const pt: TPoint);
begin
  if (pt.X = fPosition.X) and (pt.Y = fPosition.Y) then Exit;
  fPosition := pt;
  fOwner.UpdatePending;
end;
//------------------------------------------------------------------------------

procedure TLayer32.SetVisible(value: Boolean);
begin
  if (value = fVisible) or (value and fImage.IsEmpty) then Exit;
  fVisible := value;
  ImageChanged(self);
end;
//------------------------------------------------------------------------------

function TLayer32.RaiseOne: Boolean;
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

function TLayer32.RaiseTop: Boolean;
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

function TLayer32.LowerOne: Boolean;
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

function TLayer32.LowerBottom: Boolean;
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
// TImageLayers32 class
//------------------------------------------------------------------------------

constructor TImageLayers32.Create;
begin
  fList := TList.Create;
  fImageBase := TImage32.Create;
end;
//------------------------------------------------------------------------------

destructor TImageLayers32.Destroy;
begin
  Clear;
  fList.Free;
  fImageBase.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TImageLayers32.SetSize(width, height: integer);
begin
  fImageBase.SetSize(width, height);
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TImageLayers32.GetLayer(index: integer): TLayer32;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsImageLayerRangeError);
  Result := TLayer32(fList[index]);
end;
//------------------------------------------------------------------------------

procedure TImageLayers32.UpdatePending;
begin
  fUpdatePending := true;
end;
//------------------------------------------------------------------------------

procedure TImageLayers32.Update;
var
  i: integer;
  dstRec: TRect;
  blendFunc: TBlendFunction;
begin
  fImageBase.FillRect(fImageBase.Bounds, fBackColor);

  if fBackColor shr 24 < 254 then //ie semi-transparent
    blendFunc := BlendToAlpha else
    blendFunc := BlendToOpaque;

  for i := 0 to Count -1 do
  begin
    with Layer[i] do
    begin
      if not Visible then Continue;
      dstRec := Rect(fPosition.X, fPosition.Y,
        fPosition.X + Width, fPosition.Y + Height);
      fImageBase.CopyFrom(Image, Image.Bounds, dstRec, blendFunc);
    end;
  end;
  fUpdatePending := false;
end;
//------------------------------------------------------------------------------

function TImageLayers32.GetMergedLayers: TImage32;
begin
  if fUpdatePending then Update;
  Result := fImageBase;
end;
//------------------------------------------------------------------------------

procedure TImageLayers32.Clear;
var
  i: integer;
begin
  SetSize(0, 0);
  for i := 0 to Count -1 do
    TLayer32(fList[i]).free;
  fList.Clear;
end;
//------------------------------------------------------------------------------

function TImageLayers32.GetCount: integer;
begin
  Result := fList.Count;
end;
//------------------------------------------------------------------------------

function TImageLayers32.GetHeight: integer;
begin
  Result := fImageBase.Height;
end;
//------------------------------------------------------------------------------

procedure TImageLayers32.SetHeight(value: integer);
begin
  if fImageBase.Height = value then Exit;
  fImageBase.SetSize(Width, value);
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TImageLayers32.GetWidth: integer;
begin
  Result := fImageBase.Width;
end;
//------------------------------------------------------------------------------

procedure TImageLayers32.SetWidth(value: integer);
begin
  if fImageBase.Width = value then Exit;
  fImageBase.SetSize(value, Height);
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TImageLayers32.AddNewLayer: TLayer32;
var
  i: integer;
begin
  i := Count;
  Result := TLayer32.Create(Self, i);
  fList.Add(Result);
  UpdatePending;
end;
//------------------------------------------------------------------------------

function TImageLayers32.InsertNewLayer(index: integer): TLayer32;
var
  i: integer;
begin
  if (index < 0) or (index > Count) then
    raise Exception.Create(rsImageLayerRangeError);
  Result := TLayer32.Create(Self, index);
  fList.Insert(index, Result);
  for i := index + 1 to Count -1 do
    Layer[i].fIndex := i;
  UpdatePending;
end;
//------------------------------------------------------------------------------

procedure TImageLayers32.DeleteLayer(index: integer);
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

end.
