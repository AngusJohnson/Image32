unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, Dialogs, ShellApi,
  Img32, Img32.Layers, Img32.Draw, Img32.Panels;

type

  //----------------------------------------------------------------------
  // A couple of custom layer classes ...
  //----------------------------------------------------------------------

  TMyVectorLayer32 = class(TVectorLayer32) //for vector drawn layers
  private
    BrushColor : TColor32;
    PenColor   : TColor32;
    PenWidth   : double;
    IsRectangle: Boolean;
    procedure InitLayer;
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    pnlMain: TImage32Panel;
    layeredImg32: TLayeredImage32;
    clickedLayer: TLayer32;
    targetLayer: TMyVectorLayer32;
    buttonGroup   : TGroupLayer32;
    clickPoint    : TPoint;
    function GetRandomRect: TRect;
    procedure SetTargetLayer(layer: TMyVectorLayer32);
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

procedure TMyVectorLayer32.InitLayer;
var
  hsl: THsl;
begin
  hsl.hue := Random(256);
  hsl.sat := 240;
  hsl.lum := 200;
  hsl.Alpha := 192;
  BrushColor := HslToRgb(hsl);
  PenColor := MakeDarker(BrushColor, 60) or $FF000000;
  PenWidth := DpiAware(2);
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.UpdateGroupClipPath;
begin
  if IsRectangle then Exit;
  //ClipPath must be relative to the Image bounds
  ClipPath := OffsetPath(Paths, -Left, -Top);
  UpdateHitTestMask(ClipPath, frEvenOdd);
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.SetBounds(const newBounds: TRect);
begin
  inherited;
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
  i: integer;
  p: TPathD;
  rec: TRect;
begin
  i := Ceil(PenWidth /2);
  rec := Image.Bounds;
  types.InflateRect(rec, -i,-i);
  if IsRectangle then
    p := Rectangle(rec) else
    p := Ellipse(rec);
  DrawPolygon(Image, p, frEvenOdd, BrushColor);
  DrawLine(Image, p, PenWidth, PenColor, esPolygon);
end;

//------------------------------------------------------------------------------
// TForm1 methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;

  //the following can be bypassed by installing TImage32Panel in the IDE
  pnlMain := TImage32Panel.Create(self);
  pnlMain.parent := Self;
  pnlMain.Align := alClient;
  pnlMain.OnMouseDown := pnlMainMouseDown;
  pnlMain.OnMouseMove := pnlMainMouseMove;
  pnlMain.OnMouseUp := pnlMainMouseUp;
  pnlMain.PopupMenu := PopupMenu1;
  pnlMain.OnKeyDown := FormKeyDown;

  layeredImg32 := TLayeredImage32.Create; //sized in FormResize below.
  layeredImg32.DefaultBorderMargin := DPIAware(2);
  //prepare for a hatched background design layer (see FormResize below).
  layeredImg32.AddLayer(TDesignerLayer32);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(layeredImg32); //see also FormResize
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  rec: TRect;
  w,h: integer;
begin
  if not Assigned(layeredImg32) then Exit; //ie when destroying

  rec := pnlMain.InnerClientRect;
  w := RectWidth(rec);
  h := RectHeight(rec);
  //resize pnlMain's Image
  pnlMain.Image.SetSize(w,h);
  //resize layeredImg32 to pnlMain
  layeredImg32.SetSize(w, h);
  //resize and repaint the hatched design background layer
  with TDesignerLayer32(layeredImg32[0]) do
  begin
    //nb: use SetSize not Resize which would waste
    //CPU cycles stretching any previous hatching
    SetSize(w,h);
    HatchBackground(Image);
  end;
  invalidate; //force repaint (ie even when resizing smaller)
end;
//------------------------------------------------------------------------------

function TMainForm.GetRandomRect: TRect;
begin
  if Assigned(targetLayer) then
  begin
    Result.Left := Random(targetLayer.Width);
    Result.Top := Random(targetLayer.Height);
    Result.Right := Random(targetLayer.Width);
    Result.Bottom := Random(targetLayer.Height);
  end else
  begin
    Result.Left := Random(layeredImg32.Width);
    Result.Top := Random(layeredImg32.Height);
    Result.Right := Random(layeredImg32.Width);
    Result.Bottom := Random(layeredImg32.Height);
  end;
  Img32.Vector.NormalizeRect(Result);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddEllipseClick(Sender: TObject);
var
  newLayer: TMyVectorLayer32;
  rec: TRect;
begin
  rec := GetRandomRect;
  newLayer := layeredImg32.AddLayer(TMyVectorLayer32, targetLayer) as TMyVectorLayer32;
  newLayer.IsRectangle := false;
  newLayer.InitLayer;
  newLayer.Paths := Img32.Vector.Paths(Ellipse(rec));
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddRectangleClick(Sender: TObject);
var
  newLayer: TMyVectorLayer32;
  rec: TRect;
begin
  rec := GetRandomRect;
  newLayer := layeredImg32.AddLayer(TMyVectorLayer32, targetLayer) as TMyVectorLayer32;
  newLayer.IsRectangle := true;
  newLayer.InitLayer;
  newLayer.Paths := Img32.Vector.Paths(Rectangle(rec));
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRect: TRect;
  img: TImage32;
begin
  img := layeredImg32.GetMergedImage(false, updateRect);
  //only update updateRect otherwise repainting can be quite slow
  if not IsEmptyRect(updateRect) then
    pnlMain.Image.Copy(img, updateRect, updateRect);
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
  //add sizing buttons around the target layer
  if assigned(targetLayer) then
    buttonGroup := CreateSizingButtonGroup(layer,
      ssCorners, bsRound, DPIAware(10), clGreen32);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickPoint := Types.Point(X,Y);
  clickPoint := pnlMain.ClientToImage(clickPoint);
  clickedLayer := layeredImg32.GetLayerAt(clickPoint);

  if Assigned(clickedLayer) then
  begin
    if (clickedLayer = targetLayer) or not
      (clickedLayer is TMyVectorLayer32) then Exit;
    SetTargetLayer(clickedLayer as TMyVectorLayer32);
    Invalidate;
  end else if assigned(targetLayer) then
  begin
    FreeAndNil(buttonGroup);
    targetLayer := nil;
    Invalidate;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  dx, dy: integer;
  pt: TPoint;
  layer: TLayer32;
  rec: TRect;
begin
  pt := Types.Point(X,Y);
  pt := pnlMain.ClientToImage(pt);

  if not (ssLeft in Shift) then
  begin
    //not moving anything so just update the cursor
    layer := layeredImg32.GetLayerAt(pt);
    if Assigned(layer) then
      pnlMain.Cursor := layer.CursorId else
      pnlMain.Cursor := crDefault;
    Exit;
  end;

  if not Assigned(clickedLayer) then Exit; //we're not moving anything :)

  dx := pt.X - clickPoint.X;
  dy := pt.Y - clickPoint.Y;
  clickPoint := pt;

  if clickedLayer is TButtonDesignerLayer32 then
  begin
    //we're moving a sizing button of the target
    clickedLayer.Offset(dx, dy);
    //call UpdateSizingButtonGroup to reposition the other buttons
    //in the sizing group and get the bounds rect for the target layer
    rec := UpdateSizingButtonGroup(clickedLayer);
    //convert rec from layeredImg32 coordinates to targetLayer coords
    pt := targetLayer.MergedImagePtToLayerPt(NullPoint);
    OffsetRect(rec, pt.X, pt.Y);

    targetLayer.SetBounds(rec);
    Invalidate;
  end
  else if Assigned(targetLayer) then
  begin
    //we're moving the target itself
    targetLayer.Offset(dx, dy);
    if assigned(buttonGroup) then buttonGroup.Offset(dx, dy);
    Invalidate;
  end;
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
  if targetLayer.Index = targetLayer.Parent.ChildCount -2 then Exit;

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
  FreeAndNil(targetLayer);
  FreeAndNil(buttonGroup);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  mnuDeleteLayer.Enabled := assigned(targetLayer);
  mnuDeleteLayer2.Enabled := mnuDeleteLayer.Enabled;
end;
//------------------------------------------------------------------------------

end.
