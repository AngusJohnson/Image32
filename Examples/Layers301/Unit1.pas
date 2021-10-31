unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, Dialogs, ShellApi,
  Img32, Img32.Layers, Img32.Draw, Img32.Text;

type

  //----------------------------------------------------------------------
  // Customise a TVectorLayer32 ...
  //----------------------------------------------------------------------

  TMyVectorLayer32 = class(TVectorLayer32) //for vector drawn layers
  private
    useRandomColors: Boolean;
    BrushColor : TColor32;
    FrameWidth : integer;
    textPath   : TPathsD;
    textRect   : TRect;
  protected
    //override TVectorLayer32's empty Draw method
    procedure Draw; override;
  public
    constructor Create(parent: TLayer32;  const name: string = ''); override;
    procedure SetBounds(const newBounds: TRect); override;
    procedure UpdateHitTestAndClipPath;
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
    N2: TMenuItem;
    mnuSendBack: TMenuItem;
    mnuBringForward: TMenuItem;
    AddStar1: TMenuItem;
    AddStar2: TMenuItem;
    AddImage1: TMenuItem;
    OpenDialog1: TOpenDialog;
    AddImage2: TMenuItem;
    N1: TMenuItem;
    SendBack1: TMenuItem;
    BringForward1: TMenuItem;
    N3: TMenuItem;
    ColorDialog1: TColorDialog;
    ChangeColor1: TMenuItem;
    mnuHideHatching: TMenuItem;
    N4: TMenuItem;
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuDeleteLayerClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuAddEllipseClick(Sender: TObject);
    procedure mnuAddRectangleClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure mnuSendBackClick(Sender: TObject);
    procedure mnuBringForwardClick(Sender: TObject);
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
    procedure AddStar1Click(Sender: TObject);
    procedure AddImage1Click(Sender: TObject);
    procedure ChangeColor1Click(Sender: TObject);
    procedure mnuHideHatchingClick(Sender: TObject);
  private
    layeredImg32  : TLayeredImage32;
    clickedLayer  : TLayer32;
    targetLayer   : TRotateLayer32;
    buttonGroup   : TGroupLayer32;
    clickPoint    : TPoint;
    //this just speeds up clip resizing
    delayedMovePending : Boolean;
    delayedShift       : TShiftState;
    delayedPos         : TPoint;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    procedure DelayedMouseMove(Sender: TObject;
      Shift: TShiftState; X,Y: Integer);

    function MakeRandomRect(const mp:TPointD): TRect;
    function MakeRandomSquare(const mp:TPointD): TRect;
    procedure SetTargetLayer(layer: TRotateLayer32);
  public
  end;

var
  MainForm    : TMainForm;
  glyphs      : TGlyphCache;
  hsl         : THsl;

implementation

{$R *.dfm}

uses
  Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Fmt.SVG, Img32.Vector, Img32.Extra;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function MakeStar(const rec: TRect; IndentFrac: double = 0.4): TPathsD;
begin
  Result := Img32.Vector.Paths(Star(RectD(rec), 7, IndentFrac));
end;
//------------------------------------------------------------------------------

function MakeEllipse(const rec: TRect): TPathsD;
begin
  Result := Img32.Vector.Paths(Ellipse(RectD(rec)));
end;

//------------------------------------------------------------------------------
// TMyVectorLayer32
//------------------------------------------------------------------------------

constructor TMyVectorLayer32.Create(parent: TLayer32; const name: string = '');
begin
  inherited;

  //setup object brush color
  useRandomColors := true;//false;//
  if useRandomColors then
  begin
    hsl.hue := (hsl.hue + 23) mod 256;
    BrushColor := HslToRgb(hsl);
  end else
    BrushColor := clBtnFace32;

  Margin := 0;
  FrameWidth := DpiAware(10);
  if name = 'star' then
    FrameWidth := Round(FrameWidth * 1.5);

  //get 'textPath' ready to draw centered text
  textPath := glyphs.GetTextGlyphs(0, 0, name);
  textRect := Img32.Vector.GetBounds(textPath);
  textPath := OffsetPath(textPath, -textRect.Left, -textRect.Top);
  types.OffsetRect(textRect,-textRect.Left, -textRect.Top);
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.SetBounds(const newBounds: TRect);
begin
  inherited;
  UpdateHitTestAndClipPath;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.UpdateHitTestAndClipPath;
var
  rec: TRect;
begin
  rec := Image.Bounds;
  if Name = 'rectangle' then
  begin
    //don't worry about hittesting for rectangles because the
    //default hittest region is the layer's rectangular bounds :)

    //but ClipPath still needs setting
    Img32.Vector.InflateRect(rec, -FrameWidth, -FrameWidth);
    ClipPath := Img32.Vector.Paths(Rectangle(rec))
  end else if Name = 'ellipse' then
  begin
    //make anywhere within pp responsive to clicking
    UpdateHitTestMaskFromImage;
    //update ClipPath
    Img32.Vector.InflateRect(rec, -FrameWidth, -FrameWidth);
    ClipPath := MakeEllipse(rec)
  end
  else if Name = 'star' then
  begin
    //make anywhere within pp responsive to clicking
    UpdateHitTestMaskFromImage;
    //update ClipPath
    Img32.Vector.InflateRect(rec, -FrameWidth, -FrameWidth);
    ClipPath := MakeStar(rec);
  end;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.Draw;
var
  i, fw: integer;
  pp: TPathsD;
  rec: TRect;
  delta: TPointD;
begin
  rec := Image.Bounds;
  //preparing to center text
  delta.X := (RectWidth(rec) - RectWidth(textRect)) div 2;
  delta.Y := (RectHeight(rec) - RectHeight(textRect)) div 2;

  if name = 'rectangle' then
  begin
    //fill the rectangular layer's background
    Image.Clear(rec, clBtnFace32);
    //and draw a pale gray frame that's FrameWidth wide
    fw := FrameWidth - dpiAware1*2;
    i := Ceil(DpiAwareOne/2);
    img32.Vector.InflateRect(rec, -i, -i);
    //draw the frame's outer edge
    DrawEdge(Image, rec, clWhite32, $FFCCCCCC, dpiAwareOne*2);
    img32.Vector.InflateRect(rec, -fw, -fw);
    Image.Clear(rec, BrushColor);
    //draw the frame's inner edge
    DrawEdge(Image, rec, $FFCCCCCC, clWhite32, dpiAwareOne*2);
  end
  else if name = 'ellipse' then
  begin
    //clear background
    Image.Clear;
    fw := FrameWidth - dpiAware1*2;
    //fill ellipse border background (with clBtnFace32 color)
    DrawPolygon(Image, MakeEllipse(rec), frNonZero, clBtnFace32);
    //draw outer 3D border
    img32.Vector.InflateRect(rec, -dpiAware1,-dpiAware1);
    DrawEdge(Image, Ellipse(rec), clWhite32, $FFCCCCCC, dpiAwareOne*2);
    img32.Vector.InflateRect(rec, -fw, -fw);
    //fill ellipse background (with random) color
    DrawPolygon(Image, MakeEllipse(rec), frNonZero, BrushColor);
    //draw inner 3D border
    DrawEdge(Image, Ellipse(rec), $FFCCCCCC, clWhite32, dpiAwareOne*2);
  end
  else //name = 'star'
  begin
    Image.Clear;
    fw := FrameWidth - dpiAware1*2;
    DrawPolygon(Image, MakeStar(rec), frNonZero, clBtnFace32);
    img32.Vector.InflateRect(rec, -dpiAware1,-dpiAware1);
    DrawEdge(Image, MakeStar(rec)[0], clWhite32, $FFCCCCCC, dpiAwareOne*2);
    img32.Vector.InflateRect(rec, -fw, -fw);
    DrawPolygon(Image, MakeStar(rec), frNonZero, BrushColor);
    DrawEdge(Image, MakeStar(rec)[0], $FFCCCCCC, clWhite32, dpiAwareOne*2);
  end;
  //draw the text
  pp := OffsetPath(textPath, delta.X, delta.Y);
  DrawPolygon(Image, pp, frNonZero, clBlack32);
end;

//------------------------------------------------------------------------------
// TMainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  fr: TFontReader;
begin
  Randomize;

  hsl.hue := Random(256);
  hsl.sat := 240;
  hsl.lum := 180;
  hsl.Alpha := 255;

  Application.OnIdle := AppOnIdle;

  self.OnMouseDown := pnlMainMouseDown;
  self.OnMouseMove := pnlMainMouseMove;
  self.OnMouseUp := pnlMainMouseUp;
  self.PopupMenu := PopupMenu1;
  self.OnKeyDown := FormKeyDown;

  layeredImg32 := TLayeredImage32.Create; //sized in FormResize below.
  layeredImg32.Resampler := rBilinearResampler;

  //prepare for a hatched background design layer (see FormResize below).
  layeredImg32.AddLayer(TDesignerLayer32);

  fr := FontManager.Load('Arial');
  glyphs := TGlyphCache.Create(fr, DPIAware(12));
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(layeredImg32); //see also FormResize
  glyphs.free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.AppOnIdle(Sender: TObject; var Done: Boolean);
begin
  Done := true;
  if not delayedMovePending then Exit;
  delayedMovePending := false;
  DelayedMouseMove(Sender, delayedShift, delayedPos.X, delayedPos.Y);
end;
//------------------------------------------------------------------------------

procedure TMainForm.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 0; //speed up drawing
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  rec: TRect;
  w,h: integer;
begin
  if not Assigned(layeredImg32) then Exit; //ie when destroying

  rec := ClientRect;
  w := RectWidth(rec);
  h := RectHeight(rec);
  //resize layeredImg32 to pnlMain
  layeredImg32.SetSize(w, h);
  //resize and repaint the hatched design background layer
  with TDesignerLayer32(layeredImg32[0]) do
  begin
    //nb: use SetSize not Resize which would waste
    //CPU cycles stretching any previous hatching
    SetSize(w,h);
    if visible then
      HatchBackground(Image);
  end;
  invalidate; //force repaint (ie even when resizing smaller)
end;
//------------------------------------------------------------------------------

function TMainForm.MakeRandomRect(const mp:TPointD): TRect;
var
  x2,y2: integer;
begin
  with Point(mp) do
  begin
    x2 := X div 2 + Random(X div 2);
    y2 := Y div 2 + Random(Y div 2);
    Result.Left := X - x2;
    Result.Right := X + x2;
    Result.Top := Y - y2;
    Result.Bottom := Y + y2;
  end;
end;
//------------------------------------------------------------------------------

function TMainForm.MakeRandomSquare(const mp:TPointD): TRect;
var
  x2,y2: integer;
begin
  with Point(mp) do
  begin
    x2 := X div 2 + Random(X div 2);
    y2 := Y div 2 + Random(Y div 2);
    x2 := Min(x2,y2);
    Result.Left := X - x2;
    Result.Right := X + x2;
    Result.Top := Y - x2;
    Result.Bottom := Y + x2;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.AddStar1Click(Sender: TObject);
var
  newLayer: TMyVectorLayer32;
  rec: TRect;
begin
  if Assigned(targetLayer) then
    rec := MakeRandomSquare(targetLayer.Image.MidPoint) else
    rec := MakeRandomSquare(layeredImg32.MidPoint);
  newLayer := layeredImg32.AddLayer(
    TMyVectorLayer32, targetLayer, 'star') as TMyVectorLayer32;
  //setting a path will automatically define the layer's bounds
  newLayer.Paths := MakeStar(rec);
  newLayer.UpdateHitTestAndClipPath;
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddEllipseClick(Sender: TObject);
var
  newLayer: TMyVectorLayer32;
  rec: TRect;
begin
  if Assigned(targetLayer) then
    rec := MakeRandomRect(targetLayer.Image.MidPoint) else
    rec := MakeRandomRect(layeredImg32.MidPoint);
  newLayer := layeredImg32.AddLayer(
    TMyVectorLayer32, targetLayer, 'ellipse') as TMyVectorLayer32;
  //setting a path will automatically define the layer's bounds
  newLayer.Paths := MakeEllipse(rec);
  newLayer.UpdateHitTestAndClipPath;
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddRectangleClick(Sender: TObject);
var
  newLayer: TMyVectorLayer32;
  rec: TRect;
begin
  if Assigned(targetLayer) then
    rec := MakeRandomRect(targetLayer.Image.MidPoint) else
    rec := MakeRandomRect(layeredImg32.MidPoint);
  newLayer := layeredImg32.AddLayer(
    TMyVectorLayer32, targetLayer, 'rectangle') as TMyVectorLayer32;
  newLayer.SetBounds(rec);
  newLayer.UpdateHitTestAndClipPath;
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.AddImage1Click(Sender: TObject);
var
  newLayer: TRasterLayer32;
begin
  if not OpenDialog1.Execute then Exit;
  newLayer := layeredImg32.AddLayer(
    TRasterLayer32, targetLayer, 'image') as TRasterLayer32;
  //in case it's an SVG image since they're best given a default size
  newLayer.Image.SetSize(640,480);
  newLayer.Image.LoadFromFile(OpenDialog1.FileName);
  //setting a path will automatically define the layer's bounds
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRect: TRect;
  img: TImage32;
begin
  img := layeredImg32.GetMergedImage(false, updateRect);
  //only update 'updateRect' otherwise repainting can be quite slow
  if not IsEmptyRect(updateRect) then
    img.CopyToDc(updateRect, updateRect, canvas.Handle);
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

procedure TMainForm.SetTargetLayer(layer: TRotateLayer32);
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
  clickedLayer := layeredImg32.GetLayerAt(clickPoint);

  if Assigned(clickedLayer) then
  begin
    if (clickedLayer = targetLayer) or not
      (clickedLayer is TRotateLayer32) then Exit;
    SetTargetLayer(clickedLayer as TRotateLayer32);
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
begin
  delayedShift := Shift;
  delayedPos := Types.Point(X,Y);
  delayedMovePending := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DelayedMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
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
      self.Cursor := layer.CursorId else
      self.Cursor := crDefault;
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
    pt := targetLayer.GetAbsoluteOrigin;
    types.OffsetRect(rec, -pt.X, -pt.Y);

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
  if Assigned(targetLayer) then
  begin
    if targetLayer.Parent is TRotateLayer32 then
      SetTargetLayer(TRotateLayer32(targetLayer.Parent)) else
      SetTargetLayer(nil);
  end else
    Close;
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

procedure TMainForm.mnuSendBackClick(Sender: TObject);
begin
  if not Assigned(targetLayer) or
   ((targetLayer.Parent = targetLayer.Root) and
    (targetLayer.Index < 2)) then Exit;
  targetLayer.SendBackOne;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuBringForwardClick(Sender: TObject);
begin
  if Assigned(targetLayer) then
    targetLayer.BringForwardOne;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ChangeColor1Click(Sender: TObject);
begin
  if not Assigned(targetLayer) or not (targetLayer is TMyVectorLayer32) then
    Exit;
  ColorDialog1.Color := RGBColor(TMyVectorLayer32(targetLayer).BrushColor);
  if not Assigned(targetLayer) or not ColorDialog1.Execute then Exit;
  with TMyVectorLayer32(targetLayer) do
  begin
    BrushColor := Color32(ColorDialog1.Color);
    //this is an easy way to force a repaint and redo hit-testing too.
    SetBounds(Bounds);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuHideHatchingClick(Sender: TObject);
begin
  with layeredImg32[0] do
  begin
    visible := not mnuHideHatching.Checked;
    if visible then
      HatchBackground(Image);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

end.
