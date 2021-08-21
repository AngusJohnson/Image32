unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, Dialogs, ShellApi,
  Img32, Img32.Layers, Img32.Text, Img32.Draw;

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
  protected
    procedure Draw; override;
  end;

  TMyRasterLayer32 = class(TRasterLayer32) //for raster image layers
  public
    procedure Init(const pt: TPoint);
  end;

  //----------------------------------------------------------------------
  //----------------------------------------------------------------------

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    SaveDialog1: TSaveDialog;
    N2: TMenuItem;
    CopytoClipboard1: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuBringToFront: TMenuItem;
    mnuSendToBack: TMenuItem;
    N3: TMenuItem;
    mnuDeleteLayer: TMenuItem;
    AddEllipse1: TMenuItem;
    N4: TMenuItem;
    AddRectangle1: TMenuItem;
    AddText1: TMenuItem;
    OpenDialog1: TOpenDialog;
    PastefromClipboard1: TMenuItem;
    Action1: TMenuItem;
    mnuAddImage2: TMenuItem;
    mnuAddRectangle: TMenuItem;
    mnuAddEllipse: TMenuItem;
    mnuAddText: TMenuItem;
    N5: TMenuItem;
    mnuDeleteLayer2: TMenuItem;
    N6: TMenuItem;
    mnuBringtoFront2: TMenuItem;
    mnuSendtoBack2: TMenuItem;
    mnuAddImage: TMenuItem;
    mnuCutToClipboard: TMenuItem;
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuCopytoClipboardClick(Sender: TObject);
    procedure mnuBringToFrontClick(Sender: TObject);
    procedure mnuSendToBackClick(Sender: TObject);
    procedure mnuDeleteLayerClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuAddEllipseClick(Sender: TObject);
    procedure mnuAddRectangleClick(Sender: TObject);
    procedure mnuAddTextClick(Sender: TObject);
    procedure mnuPastefromClipboardClick(Sender: TObject);
    procedure mnuAddImageClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure mnuCutToClipboardClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    layeredImg32: TLayeredImage32;
    fontReader: TFontReader;
    fontCache: TGlyphCache;
    wordStrings: TStringList;
    clickedLayer: TLayer32;
    targetLayer: TLayer32;
    buttonGroup: TGroupLayer32;
    popupPoint: TPoint;
    clickPoint: TPoint;
    procedure SetTargetLayer(layer: TLayer32);
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{$R WORDS.RES}
{$R FONT.RES}

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
// TMyRasterLayer32 methods
//------------------------------------------------------------------------------

procedure TMyRasterLayer32.Init(const pt: TPoint);
begin
  MasterImage.CropTransparentPixels;
  UpdateHitTestMaskTransparent;
  PositionCenteredAt(pt);
end;

//------------------------------------------------------------------------------
// TForm1 methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  resStream: TResourceStream;
begin
  Randomize;

  layeredImg32 := TLayeredImage32.Create; //sized in FormResize below.

  //add a hatched background design layer (see FormResize below).
  layeredImg32.AddLayer(TDesignerLayer32);

  //create text rendering objects
  fontReader := FontManager.LoadFromResource('FONT_NSB', RT_RCDATA);
  fontCache := TGlyphCache.Create(fontReader, DPIAware(48));

  //load a word list (for random words)
  wordStrings := TStringList.Create;
  resStream := TResourceStream.Create(hInstance, 'WORDS', RT_RCDATA);
  try
    wordStrings.LoadFromStream(resStream);
  finally
    resStream.Free;
  end;

  popupPoint := Point(layeredImg32.MidPoint);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  wordStrings.Free;
  fontCache.Free;
  //fontReader.Free; //this will be done by FontManager
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
  newLayer: TLayer32;
  x,y: integer;
  rec: TRect;
begin
  //create a semi-random sized object
  x := DPIAware(25 + Random(100));
  y := DPIAware(25 + Random(100));
  rec := Rect(popupPoint.X -x,popupPoint.Y -y,
    popupPoint.X +x,popupPoint.Y +y);

  //create the new layer
  newLayer := TMyVectorLayer32(layeredImg32.AddLayer(TMyVectorLayer32));
  with newLayer as TMyVectorLayer32 do
    Paths := Img32.Vector.Paths(Ellipse(rec));
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddRectangleClick(Sender: TObject);
var
  newLayer: TLayer32;
  x,y: integer;
  rec: TRect;
begin
  //create a semi-random sized object
  x := DPIAware(25 + Random(100));
  y := DPIAware(25 + Random(100));
  rec := Rect(popupPoint.X -x,popupPoint.Y -y,
    popupPoint.X +x,popupPoint.Y +y);

  newLayer := layeredImg32.AddLayer(TMyVectorLayer32);
  with newLayer as TMyVectorLayer32 do
    Paths := Img32.Vector.Paths(Rectangle(rec));
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddTextClick(Sender: TObject);
var
  newLayer: TLayer32;
  randomWord: string;
  tmp: TPathsD;
  rec: TRectD;
begin
  randomWord := wordStrings[Random(wordStrings.Count)];
  tmp := fontCache.GetTextGlyphs(0, 0, randomWord);
  tmp := ScalePath(tmp, 1, 2.0);
  rec := Img32.Vector.GetBoundsD(tmp);
  with popupPoint do
    tmp := OffsetPath(tmp,
      X - rec.Left - rec.Width/2,
      Y -rec.Top - rec.Height/2);

  newLayer := layeredImg32.AddLayer(TMyVectorLayer32);
  with newLayer as TMyVectorLayer32 do Paths := tmp;
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddImageClick(Sender: TObject);
var
  rasterLayer: TMyRasterLayer32;
begin
  if not OpenDialog1.Execute then Exit;

  rasterLayer := layeredImg32.AddLayer(TMyRasterLayer32) as TMyRasterLayer32;
  with rasterLayer do
  begin
    MasterImage.LoadFromFile(OpenDialog1.FileName);
    if MasterImage.IsEmpty then
    begin
      Free;
      Exit;
    end;
    Init(Point(layeredImg32.MidPoint));
  end;
  SetTargetLayer(rasterLayer);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuPastefromClipboardClick(Sender: TObject);
var
  rasterLayer: TMyRasterLayer32;
begin
  if not TImage32.CanPasteFromClipBoard then Exit;
  FreeAndNil(buttonGroup);
  rasterLayer := layeredImg32.AddLayer(TMyRasterLayer32) as TMyRasterLayer32;
  with rasterLayer do
  begin
    MasterImage.PasteFromClipBoard;
    if MasterImage.IsEmpty then
    begin
      Free;
      Exit;
    end;
    Init(Point(layeredImg32.MidPoint));
  end;
  SetTargetLayer(rasterLayer);
  Invalidate;
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

procedure TMainForm.SetTargetLayer(layer: TLayer32);
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
     not (clickedLayer is TButtonDesignerLayer32) and
    (clickedLayer <> targetLayer) then
      SetTargetLayer(clickedLayer);

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

procedure TMainForm.mnuCutToClipboardClick(Sender: TObject);
begin
  if Assigned(targetLayer) then
  begin
    targetLayer.Image.CopyToClipBoard;
    FreeAndNil(targetLayer);
    FreeAndNil(buttonGroup);
    Invalidate;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuCopytoClipboardClick(Sender: TObject);
begin
  if Assigned(targetLayer) then
    targetLayer.Image.CopyToClipBoard;
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
  FreeAndNil(targetLayer);
  FreeAndNil(buttonGroup);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  mnuBringToFront.Enabled := assigned(targetLayer) and
    (targetLayer.Index < layeredImg32.Root.ChildCount -2);
  mnuBringtoFront2.Enabled := mnuBringtoFront.Enabled;
  mnuSendToBack.Enabled := assigned(targetLayer) and (targetLayer.Index > 1);
  mnuSendtoBack2.Enabled := mnuSendtoBack.Enabled;
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
