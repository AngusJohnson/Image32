unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,   Img32.Storage,
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
    textRect   : TRectD;
  protected
    procedure SetName(const aName: string); override;
    procedure Draw; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure UpdateHitTestAndClipPath;
  end;

  TMyStarLayer32 = class(TMyVectorLayer32)
  public
    procedure SetInnerBounds(const newBounds: TRectD); override;
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
    ShadowDepth1: TMenuItem;
    MnuShadow0: TMenuItem;
    MnuShadow5: TMenuItem;
    MnuShadow10: TMenuItem;
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
    targetLayer   : TRotLayer32;
    buttonGroup   : TGroupLayer32;
    clickPoint    : TPoint;
    //this just speeds up clip resizing
    delayedMovePending : Boolean;
    delayedShift       : TShiftState;
    delayedPos         : TPoint;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    procedure DelayedMouseMove(Sender: TObject;
      Shift: TShiftState; X,Y: Integer);

    function MakeRandomRect(const mp:TPointD): TRectD;
    function MakeRandomSquare(const mp:TPointD): TRectD;
    procedure SetTargetLayer(layer: TRotLayer32);
  public
  end;

var
  MainForm    : TMainForm;
  arial12     : TFontCache;
  arial14     : TFontCache;
  hsl         : THsl;

implementation

{$R *.dfm}

uses
  Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Fmt.SVG, Img32.Vector, Img32.Extra;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function MakeStar(const rec: TRectD; IndentFrac: double = 0.4): TPathD;
begin
  Result := Star(rec, 7, IndentFrac);
end;
//------------------------------------------------------------------------------

function MakeEllipse(const rec: TRect): TPathD;
begin
  Result := Ellipse(RectD(rec));
end;

//------------------------------------------------------------------------------
// TMyVectorLayer32
//------------------------------------------------------------------------------

constructor TMyVectorLayer32.Create(parent: TLayer32; const name: string);
begin
  inherited;

  //setup object brush color
  useRandomColors := true;//false;//
  if useRandomColors then
  begin
    hsl.hue := (hsl.hue + 17) mod 256;
    BrushColor := HslToRgb(hsl);
  end else
    BrushColor := clBtnFace32;

  if MainForm.MnuShadow0.Checked then
    OuterMargin := dpiAware(0)
  else if MainForm.MnuShadow5.Checked then
    OuterMargin := dpiAware(5)
  else
    OuterMargin := dpiAware(10);

  FrameWidth := DpiAware(10);
  if self is TMyStarLayer32 then
    FrameWidth := Round(FrameWidth * 1.72);
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.SetName(const aName: string);
begin
  inherited;
  //get 'textPath' ready to draw centered text
  textPath := arial12.GetTextOutline(0, 0, name);
  textRect := Img32.Vector.GetBoundsD(textPath);
  textPath := TranslatePath(textPath, -textRect.Left, -textRect.Top);
  TranslateRect(textRect, -textRect.Left, -textRect.Top);
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.SetInnerBounds(const newBounds: TRectD);
begin
  inherited;
  UpdateHitTestAndClipPath;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.UpdateHitTestAndClipPath;
var
  rec: TRectD;
begin
  rec := RectD(0,0,Width,Height);
  if Copy(Name,1,9) = 'rectangle' then
  begin
    //don't worry about hittesting for rectangles because the
    //default hittest region is the layer's rectangular bounds :)

    //but ClipPath still needs setting
    Img32.Vector.InflateRect(rec,
      -FrameWidth, -FrameWidth);
    ClipPath := Img32.Vector.Paths(Rectangle(rec))
  end else if Name = 'ellipse' then
  begin
    //make anywhere within pp responsive to clicking
    UpdateHitTestMaskFromImage;
    //update ClipPath
    Img32.Vector.InflateRect(rec, -FrameWidth, -FrameWidth);
    ClipPath := Img32.Vector.Paths(MakeEllipse(Rect(rec)))
  end
  else if Name = 'star' then
  begin
    //make anywhere within pp responsive to clicking
    UpdateHitTestMaskFromImage;
    //update ClipPath
    Img32.Vector.InflateRect(rec, -FrameWidth, -FrameWidth);
    ClipPath := Img32.Vector.Paths(MakeStar(rec));
  end;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.Draw;
var
  p: TPathD;
  pp: TPathsD;
  rec: TRectD;
  delta: TPointD;
begin
  rec := RectD(0,0, Width, Height);
  TranslateRect(rec, OuterMargin, OuterMargin);
  //preparing to center text
  delta.X := (rec.Width - textRect.Width) / 2;
  delta.Y := (rec.Height - textRect.Height) / 2;
  Image.Clear;

  if copy(name, 1, 9) = 'rectangle' then
  begin
    //draw a drop shadow and then fill the layer's background
    DrawShadowRect(Image, Rect(rec), OuterMargin, angle45, $33000000);
    Image.FillRect(Rect(rec), clBtnFace32);
    //and draw a pale 3D gray frame
    DrawEdge(Image, rec, clWhite32, $FFCCCCCC, dpiAware(2));
    img32.Vector.InflateRect(rec, -FrameWidth, -FrameWidth);
    Image.FillRect(Rect(rec), BrushColor);
    DrawEdge(Image, rec, $FFCCCCCC, clWhite32, dpiAware(2));
  end
  else if name = 'ellipse' then
  begin
    //draw drop shadow and fill the ellipse layer's background
    p := MakeEllipse(Rect(rec));
    DrawShadow(Image, p, frNonZero, OuterMargin, angle45, $33000000);
    DrawPolygon(Image, p, frNonZero, clBtnFace32);
    //and draw a pale 3D gray frame
    p := MakeEllipse(Rect(rec));
    DrawEdge(Image, p, clWhite32, $FFCCCCCC, dpiAwareOne*2);
    img32.Vector.InflateRect(rec, -FrameWidth, -FrameWidth);
    p := MakeEllipse(Rect(rec));
    DrawPolygon(Image, p, frNonZero, BrushColor);
    DrawEdge(Image, Ellipse(rec), $FFCCCCCC, clWhite32, dpiAwareOne*2);
  end
  else //name = 'star'
  begin
    p := MakeStar(rec);
    DrawShadow(Image, p, frNonZero, OuterMargin, angle45, $33000000);
    DrawPolygon(Image, p, frNonZero, clBtnFace32);
    DrawEdge(Image, MakeStar(rec), clWhite32, $FFCCCCCC, dpiAwareOne*2);
    img32.Vector.InflateRect(rec, -FrameWidth, -FrameWidth);
    DrawPolygon(Image, MakeStar(rec), frNonZero, BrushColor);
    DrawEdge(Image, MakeStar(rec), $FFCCCCCC, clWhite32, dpiAwareOne*2);
  end;
  //draw the text
  pp := TranslatePath(textPath,
    delta.X + OuterMargin, delta.Y + OuterMargin);
  DrawPolygon(Image, pp, frNonZero, clBlack32);
end;

//------------------------------------------------------------------------------
// TMyStarLayer32 methods
//------------------------------------------------------------------------------

procedure TMyStarLayer32.SetInnerBounds(const newBounds: TRectD);
var
  i: double;
  r: TRectD;
begin
  r := newBounds;
  i := Average(r.Right - r.Left, r.Bottom - r.Top);
  r.Right := r.Left + i;
  r.Bottom := r.Top + i;
  inherited SetInnerBounds(r);
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
  hsl.lum := 160;
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
  layeredImg32.AddLayer(TLayer32);

  fr := FontManager.LoadFontReader('Arial');
  arial12 := TFontCache.Create(fr, DPIAware(12));
  arial14 := TFontCache.Create(fr, DPIAware(14));

  ClientWidth := DpiAware(800);
  ClientHeight := DpiAware(600) + StatusBar1.Height;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(layeredImg32); //see also FormResize
  arial12.free;
  arial14.free;
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
  w,h: integer;
begin
  if not Assigned(layeredImg32) then Exit; //ie when destroying

  RectWidthHeight(ClientRect, w, h);
  dec(h, StatusBar1.Height);
  //resize layeredImg32 to pnlMain
  layeredImg32.SetSize(w, h);
  //resize and repaint the hatched design background layer
  with layeredImg32[0] do
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

function TMainForm.MakeRandomRect(const mp:TPointD): TRectD;
var
  x2,y2: double;
begin
  with Point(mp) do
  begin
    x2 := X / 2 + Random(Round(X / 2));
    y2 := Y / 2 + Random(Round(Y / 2));
    Result.Left := X - x2;
    Result.Right := X + x2;
    Result.Top := Y - y2;
    Result.Bottom := Y + y2;
  end;
end;
//------------------------------------------------------------------------------

function TMainForm.MakeRandomSquare(const mp:TPointD): TRectD;
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
  rec: TRectD;
begin
  if Assigned(targetLayer) then
    rec := MakeRandomSquare(targetLayer.Image.MidPoint) else
    rec := MakeRandomSquare(layeredImg32.MidPoint);
  newLayer := layeredImg32.AddLayer(
    TMyStarLayer32, targetLayer, 'star') as TMyVectorLayer32;
  //setting a path will automatically define the layer's bounds
  newLayer.Paths := Paths(MakeStar(rec));
  newLayer.UpdateHitTestAndClipPath;
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddEllipseClick(Sender: TObject);
var
  newLayer: TMyVectorLayer32;
  rec: TRectD;
begin
  if Assigned(targetLayer) then
    rec := MakeRandomRect(targetLayer.Image.MidPoint) else
    rec := MakeRandomRect(layeredImg32.MidPoint);
  newLayer := layeredImg32.AddLayer(
    TMyVectorLayer32, targetLayer, 'ellipse') as TMyVectorLayer32;
  //setting a path will automatically define the layer's bounds
  newLayer.Paths := Paths(MakeEllipse(Rect(rec)));
  newLayer.UpdateHitTestAndClipPath;
  SetTargetLayer(newLayer);
end;
//------------------------------------------------------------------------------

var recCnt: integer = 1;

procedure TMainForm.mnuAddRectangleClick(Sender: TObject);
var
  newLayer: TMyVectorLayer32;
  rec: TRectD;
begin
  if Assigned(targetLayer) then
    rec := MakeRandomRect(targetLayer.Image.MidPoint) else
    rec := MakeRandomRect(layeredImg32.MidPoint);

  newLayer := layeredImg32.AddLayer(
    TMyVectorLayer32, targetLayer, 'rectangle ' + inttostr(recCnt)) as TMyVectorLayer32;
  inc(recCnt);
  newLayer.SetInnerBounds(rec);
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
  newLayer.MasterImage.SetSize(DPIAware(320),DPIAware(240));
  newLayer.MasterImage.LoadFromFile(OpenDialog1.FileName);
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

procedure TMainForm.SetTargetLayer(layer: TRotLayer32);
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
  clickedLayer := layeredImg32.GetLayerAt(PointD(clickPoint));

  if Assigned(clickedLayer) then
  begin
    if (clickedLayer = targetLayer) or not
      (clickedLayer is TRotLayer32) then Exit;
    SetTargetLayer(clickedLayer as TRotLayer32);
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
  rec: TRectD;
begin
  pt := Types.Point(X,Y);

  if not (ssLeft in Shift) then
  begin
    //not moving anything so just update the cursor
    layer := layeredImg32.GetLayerAt(PointD(pt));
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
    rec := RectD(UpdateSizingButtonGroup(clickedLayer));
    //convert rec from layeredImg32 coordinates to targetLayer coords
    //nb: bounds must be set relative to the layer's parent
    rec := TLayer32(targetLayer.Parent).MakeRelative(rec);
    targetLayer.SetInnerBounds(rec);
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
    if targetLayer.Parent is TRotLayer32 then
      SetTargetLayer(TRotLayer32(targetLayer.Parent)) else
      SetTargetLayer(nil);
  end else
    Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuDeleteLayerClick(Sender: TObject);
var
  layer: TLayer32;
begin
  if not assigned(targetLayer) then Exit;
  layer := TLayer32(targetLayer.Parent);
  FreeAndNil(targetLayer);
  FreeAndNil(buttonGroup);
  if Assigned(layer) and (layer is TRotLayer32) then
    SetTargetLayer(TRotLayer32(layer));
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
    SetInnerBounds(InnerBounds);
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
