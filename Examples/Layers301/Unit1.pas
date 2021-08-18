unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, Dialogs, ShellApi,
  Img32, Img32.Layers, Img32.Draw;

type

  //----------------------------------------------------------------------
  // A couple of custom layer classes ...
  //----------------------------------------------------------------------

  TMyVectorLayer32 = class(TVectorLayer32) //for vector drawn layers
  private
    BrushColor : TColor32;
    PenColor   : TColor32;
    PenWidth   : double;
    procedure InitRandomColors;
    procedure UpdateGroupClipPath;
  protected
    procedure Draw; override;
  public
    procedure SetBounds(const newBounds: TRect); override;
    procedure Offset(dx,dy: integer); override;
  end;

  //----------------------------------------------------------------------
  //----------------------------------------------------------------------

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    PopupMenu1: TPopupMenu;
    mnuDeleteLayer: TMenuItem;
    AddEllipse1: TMenuItem;
    AddRectangle1: TMenuItem;
    Action1: TMenuItem;
    mnuAddRectangle: TMenuItem;
    mnuAddEllipse: TMenuItem;
    N5: TMenuItem;
    mnuDeleteLayer2: TMenuItem;
    N1: TMenuItem;
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuBringToFrontClick(Sender: TObject);
    procedure mnuSendToBackClick(Sender: TObject);
    procedure mnuDeleteLayerClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuAddEllipseClick(Sender: TObject);
    procedure mnuAddRectangleClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    layeredImg32: TLayeredImage32;
    clickedLayer: TLayer32;
    targetLayer: TMyVectorLayer32;
    buttonGroup: TGroupLayer32;
    popupPoint: TPoint;
    clickPoint: TPoint;
    procedure SetTargetLayer(layer: TMyVectorLayer32);
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Img32.Fmt.BMP, Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Fmt.SVG,
  Img32.Vector, Img32.Extra, Img32.Clipper;

const
  margin = 100;


//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function MakeDarker(color: TColor32; percent: integer): TColor32;
var
  pcFrac: double;
  r: TARGB absolute Result;
begin
  percent := Max(0, Min(100, percent));
  pcFrac := percent/100;
  Result := color;
  r.R := r.R - Round(r.R * pcFrac);
  r.G := r.G - Round(r.G * pcFrac);
  r.B := r.B - Round(r.B * pcFrac);
end;

//------------------------------------------------------------------------------
// TMyVectorLayer32
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.InitRandomColors;
var
  hsl: THsl;
begin
  hsl.hue := Random(256);
  hsl.sat := 240;
  hsl.lum := 200;
  hsl.Alpha := 128;
  BrushColor := HslToRgb(hsl);
  PenColor := MakeDarker(BrushColor, 60) or $FF000000;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.UpdateGroupClipPath;
var
  pp: TPathsD;
begin
  pp := InflatePath(Paths[0], PenWidth/2);
  GroupOwner.ClipPath := pp[0];
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.SetBounds(const newBounds: TRect);
begin
  inherited SetBounds(newBounds);
  UpdateGroupClipPath;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.Offset(dx,dy: integer);
begin
  inherited Offset(dx,dy);
  UpdateGroupClipPath;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.Draw;
var
  p: TPathsD;
begin
  if PenWidth = 0 then
  begin
    InitRandomColors;
    PenWidth := DpiAware(1.5);
  end;

  p := OffsetPath(Paths, -Left, -Top);
  DrawPolygon(Image, p, frEvenOdd, BrushColor);
  DrawLine(Image, p, PenWidth, PenColor, esPolygon);
  UpdateHitTestMask(p, frEvenOdd);
end;

//------------------------------------------------------------------------------
// TForm1 methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;

  layeredImg32 := TLayeredImage32.Create; //sized in FormResize below.

  //add a hatched background design layer (see FormResize below).
  layeredImg32.AddLayer(TDesignerLayer32);

  popupPoint := Point(layeredImg32.MidPoint);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(layeredImg32); //see FormResize
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  if not Assigned(layeredImg32) then Exit;

  layeredImg32.SetSize(ClientWidth, ClientHeight);

  //and resize and repaint the hatched design background layer
  with TDesignerLayer32(layeredImg32[0]) do
  begin
    //nb: use SetSize not Resize which would waste
    //CPU cycles stretching any previous hatching
    SetSize(layeredImg32.Width, layeredImg32.Height);
    HatchBackground(Image);
  end;

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddEllipseClick(Sender: TObject);
var
  targetGroup, newGroupLayer: TGroupLayer32;
  newLayer: TMyVectorLayer32;
  x,y: integer;
  rec: TRect;
begin
  //create a semi-random sized object
  x := DPIAware(25 + Random(100));
  y := DPIAware(25 + Random(100));
  rec := Rect(popupPoint.X -x,popupPoint.Y -y,
    popupPoint.X +x,popupPoint.Y +y);

  if Assigned(targetLayer) then
    targetGroup := targetLayer.GroupOwner else
    targetGroup := nil;
  //create the new layer and its owner grouplayer
  newGroupLayer := layeredImg32.AddLayer(TGroupLayer32, targetGroup) as TGroupLayer32;
  newLayer := layeredImg32.AddLayer(TMyVectorLayer32, newGroupLayer) as TMyVectorLayer32;
  newLayer.Paths := Img32.Vector.Paths(Ellipse(rec));
  newLayer.UpdateGroupClipPath;
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddRectangleClick(Sender: TObject);
var
  targetGroup, newGroupLayer: TGroupLayer32;
  newLayer: TMyVectorLayer32;
  x,y: integer;
  rec: TRect;
begin
  //create a semi-random sized object
  x := DPIAware(25 + Random(100));
  y := DPIAware(25 + Random(100));
  rec := Rect(popupPoint.X -x,popupPoint.Y -y,
    popupPoint.X +x,popupPoint.Y +y);

  if Assigned(targetLayer) then
    targetGroup := targetLayer.GroupOwner else
    targetGroup := nil;
  //create the new layer and its owner grouplayer
  newGroupLayer := layeredImg32.AddLayer(TGroupLayer32, targetGroup) as TGroupLayer32;
  newLayer := layeredImg32.AddLayer(TMyVectorLayer32, newGroupLayer) as TMyVectorLayer32;
  newLayer.Paths := Img32.Vector.Paths(Rectangle(rec));
  newLayer.UpdateGroupClipPath;
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.WMERASEBKGND(var message: TMessage);
begin
  //don't erase because we're only doing partial paints (see FormPaint below)
  message.Result := 1;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRect: TRect;
begin
  //layeredImg32.GetMergedImage optionally returns the portion of
  //the image that's changed since the previous GetMergedImage call.
  //Painting only this changed region significantly speeds up drawing.
  with layeredImg32.GetMergedImage(false, updateRect) do
  begin
    CopyToDc(updateRect, self.Canvas.Handle,
      updateRect.Left, updateRect.Top, false);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and Assigned(targetLayer) then
  begin
    SetTargetLayer(nil); //deselect the active control
    Key := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.SetTargetLayer(layer: TMyVectorLayer32);
begin
  if targetLayer = layer then Exit;
  FreeAndNil(buttonGroup);
  targetLayer := layer;
  Invalidate;
  if not assigned(targetLayer) then Exit;
  //add sizing buttons around the target layer
  buttonGroup := CreateSizingButtonGroup(layer,
    ssCorners, bsRound, DPIAware(10), clGreen32);
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickPoint := Types.Point(X,Y);
  clickedLayer := layeredImg32.GetLayerAt(clickPoint);

  if not Assigned(clickedLayer) then
  begin
    FreeAndNil(buttonGroup);
    targetLayer := nil;
  end
  else if Assigned(clickedLayer) and
    (clickedLayer <> targetLayer) and
    (clickedLayer is TMyVectorLayer32) then
      SetTargetLayer(clickedLayer as TMyVectorLayer32);

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy: integer;
  pt: TPoint;
  layer: TLayer32;
  rec: TRect;
begin
  pt := Types.Point(X,Y);

  if not (ssLeft in Shift) then
  begin
    //not moving anything so just update the cursor
    layer := layeredImg32.GetLayerAt(pt);
    if Assigned(layer) then
      Cursor := layer.CursorId else
      Cursor := crDefault;
    Exit;
  end;

  if not Assigned(clickedLayer) then Exit; //we're not moving anything :)

  dx := pt.X - clickPoint.X;
  dy := pt.Y - clickPoint.Y;
  clickPoint := pt;

  if clickedLayer is TButtonDesignerLayer32 then
  begin
    //OK, we're moving a sizing button

    clickedLayer.Offset(dx, dy);

    //now call UpdateSizingButtonGroup to reposition the other buttons
    //in the sizing group and get the bounds rect for the target layer
    rec := UpdateSizingButtonGroup(clickedLayer);
    targetLayer.SetBounds(rec);

  end
  else if Assigned(targetLayer) then
  begin
    targetLayer.Offset(dx, dy);
    if assigned(buttonGroup) then buttonGroup.Offset(dx, dy);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickedLayer := nil;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuBringToFrontClick(Sender: TObject);
begin
  if not assigned(targetLayer) then Exit;

  //don't send above the (top-most) sizing button group
  if targetLayer.Index = targetLayer.GroupOwner.ChildCount -2 then Exit;

  if targetLayer.BringForwardOne then
    Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuSendToBackClick(Sender: TObject);
begin
  //don't send below the (bottom-most) hatched background.
  if targetLayer.Index = 1 then Exit;

  if targetLayer.SendBackOne then
    Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuDeleteLayerClick(Sender: TObject);
begin
  if not assigned(targetLayer) then Exit;
  FreeAndNil(targetLayer.GroupOwner);
  FreeAndNil(buttonGroup);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  mnuDeleteLayer.Enabled := assigned(targetLayer);
  mnuDeleteLayer2.Enabled := mnuDeleteLayer.Enabled;

  if Sender = PopupMenu1 then
  begin
    GetCursorPos(popupPoint);
    popupPoint := ScreenToClient(popupPoint);
  end else
    popupPoint := Point(layeredImg32.MidPoint);
end;
//------------------------------------------------------------------------------

end.
