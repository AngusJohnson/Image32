unit main;

interface

uses
  Windows, Messages, SysUtils, Types, Classes, Graphics,
  Controls, Forms,  Dialogs, Math, ComCtrls, Menus,
  Image32, Image32_Layers, Image32_Ttf;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    File1: TMenuItem;
    mnuExit: TMenuItem;
    Action1: TMenuItem;
    AddImage1: TMenuItem;
    AddText1: TMenuItem;
    N1: TMenuItem;
    Rotate1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    SendBackOne1: TMenuItem;
    BringForwardOne1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure AddText1Click(Sender: TObject);
    procedure AddImage1Click(Sender: TObject);
    procedure Rotate1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure SendBackOne1Click(Sender: TObject);
    procedure BringForwardOne1Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
    layeredImage: TLayeredImage32;
    words: TStringList;

    clickedLayer: TLayer32;
    targetLayer: TLayer32;
    sizingButtonGroup: TSizingGroupLayer32;
    rotatingButtonGroup: TRotatingGroupLayer32;
    clickPoint: TPoint;
  end;

var
  MainForm: TMainForm;

  fontReader: TFontReader; //TTF font reader
  fontCache: TGlyphCache;
  
const
  crRotate = 1;
  crMove   = 2;

implementation

{$R *.dfm}
{$R font.res}
{$R words.res}
{$R Cursors.res}

uses
  Image32_Draw, Image32_Extra, Image32_Vector,
  Image32_BMP, Image32_PNG, Image32_JPG, Image32_Transform;


type

  TMyRasterLayer32 = class(TRasterLayer32)
  public
    CurrentAngle: double;
    ScaledX: double;
    ScaledY: double;
  public
    procedure Init(const filename: string; const centerPt: TPointD);
    procedure Rotate(newAngle: double);
    procedure Stretch(const maxBounds: TRect);
  end;

  TMyTextLayer32 = class(TVectorLayer32)
  private
    drawPaths: TPathsD;
    procedure Draw(const mp: TPointD);
  public
    CurrentAngle: double;
    Margin: integer;
    BaseWidth: double;
    BaseHeight: double;
    ScaledX: double;
    ScaledY: double;
    PenColor  : TColor32;
    BrushColor: TColor32;
    procedure Init(const word: string; const centerPt: TPointD);
    procedure Rotate(newAngle: double);
    procedure Stretch(const maxBounds: TRect);
  end;

//------------------------------------------------------------------------------
// TMyRasterLayer32
//------------------------------------------------------------------------------

procedure TMyRasterLayer32.Init(const filename: string; const centerPt: TPointD);
begin
  CursorId := crSizeAll;
  if not MasterImage.LoadFromFile(filename) then
    MasterImage.SetSize(100,100, clBlack32);
  MasterImage.CropTransparentPixels;
  Image.Assign(MasterImage);
  PositionCenteredAt(centerPt);
  UpdateHitTestMaskTransparent;
  ScaledX := 1;
  ScaledY := 1;
end;
//------------------------------------------------------------------------------

procedure TMyRasterLayer32.Rotate(newAngle: double);
var
  mat: TMatrixD;
  mp: TPointD;
begin
  mp := MidPoint;
  NormalizeAngle(newAngle);
  CurrentAngle := newAngle;
  BeginUpdate;
  try
    Image.Assign(MasterImage);
    //when performing more than one affine transformation
    //(ie scale and rotate here), it's quicker to use a matrix transform
    mat := IdentityMatrix;
    MatrixScale(mat, ScaledX, ScaledY);
    MatrixRotate(mat, NullPointD, CurrentAngle);
    AffineTransformImage(Image, mat);
    Image.CropTransparentPixels;
    PositionCenteredAt(mp);
    UpdateHitTestMaskTransparent;
  finally
    EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TMyRasterLayer32.Stretch(const maxBounds: TRect);
var
  w,h: integer;
  mat: TMatrixD;
  rec: TRect;
  scale: double;
begin
  w := RectWidth(maxBounds);
  h := RectHeight(maxBounds);
  if (w = 0) or (h = 0) then Exit;
  BeginUpdate;
  try
    Image.Assign(MasterImage);
    if CurrentAngle = 0 then
    begin
      ScaledX := w / MasterImage.Width;
      ScaledY := h / MasterImage.Height;
      Image.Resize(w,h);
    end else
    begin
      //we'll only allow proportional scaling here
      //to avoid the added complications of skewing.
      mat := IdentityMatrix;
      MatrixScale(mat, ScaledX, ScaledY);
      MatrixRotate(mat, NullPointD, CurrentAngle);
      AffineTransformImage(Image, mat);
      Image.CropTransparentPixels;
      rec := image.Bounds;
      scale := Min(w / RectWidth(rec), h / RectHeight(rec));
      ScaledX := ScaledX * scale;
      ScaledY := ScaledY * scale;
    end;

    PositionCenteredAt(Image32_Vector.MidPoint(maxBounds));
    UpdateHitTestMaskTransparent;
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
// TMyTextLayer
//------------------------------------------------------------------------------

procedure TMyTextLayer32.Init(const word: string; const centerPt: TPointD);
var
  rec: TRectD;
  hsl: THsl;
begin
  Name := word;
  CursorId := crSizeAll;
  Margin := DPIAware(2); //allow a little extra space for the pen's width

  //set a random color
  with hsl do
  begin
    hue := Random(256); sat := 240; lum := 200; Alpha := 128;
  end;
  BrushColor := HslToRgb(hsl);
  PenColor := MakeDarker(BrushColor, 60) or $FF000000;

  //get the 'word' outline (paths)
  fontCache.GetTextGlyphs(0, 0, word, drawPaths);
  drawPaths := ScalePath(drawPaths, 1, 2);
  rec := Image32_Vector.GetBoundsD(drawPaths);
  drawPaths := OffsetPath(drawPaths, -rec.Left, -rec.Top);

  BaseWidth := rec.Width;
  BaseHeight := rec.Height;
  ScaledX := 1.0; ScaledY := 1.0;

  OffsetRect(rec,
    centerPt.X -rec.Left - BaseWidth/2,
    centerPt.Y -rec.Top - BaseHeight/2);
  rec := InflateRect(rec, Margin, Margin);
  SetBounds(Rect(rec));

  Paths := CopyPaths(drawPaths); //Paths = base paths
  Draw(centerPt);    //ie avoid repeating setting the bounds
end;
//------------------------------------------------------------------------------

procedure TMyTextLayer32.Rotate(newAngle: double);
var
  mat: TMatrixD;
begin
  NormalizeAngle(newAngle);
  CurrentAngle := newAngle;
  drawPaths := CopyPaths(paths);
  //when performing more than one affine transformation
  //(ie scale and rotate here), it's quicker to use a matrix transform
  mat := IdentityMatrix;
  MatrixScale(mat, ScaledX, ScaledY);
  MatrixRotate(mat, NullPointD, CurrentAngle);
  MatrixApply(mat, drawPaths);
  Draw(MidPoint);
end;
//------------------------------------------------------------------------------

procedure TMyTextLayer32.Stretch(const maxBounds: TRect);
var
  w,h: integer;
  mat: TMatrixD;
  rec: TRectD;
  scale: double;
begin
  w := RectWidth(maxBounds) -Margin*2;
  h := RectHeight(maxBounds) -Margin*2;
  if (w <= 0) or (h <= 0) then Exit;
  if CurrentAngle = 0 then
  begin
    ScaledX := w/BaseWidth;
    ScaledY := h/BaseHeight;
    drawPaths := ScalePath(paths, ScaledX, ScaledY);
  end else
  begin
    //we'll only allow proportional scaling here
    //to avoid the added complications of skewing.
    drawPaths := CopyPaths(paths);
    mat := IdentityMatrix;
    MatrixScale(mat, ScaledX, ScaledY);
    MatrixRotate(mat, NullPointD, CurrentAngle);
    MatrixApply(mat, drawPaths);
    rec := Image32_Vector.GetBoundsD(drawPaths);

    scale := Min(w/rec.Width, h/rec.Height);
    ScaledX := ScaledX * scale;
    ScaledY := ScaledY * scale;
  end;
  Draw(PointD(Image32_Vector.MidPoint(maxBounds)));
end;
//------------------------------------------------------------------------------

procedure TMyTextLayer32.Draw(const mp: TPointD);
var
  rec: TRectD;
begin
  rec := Image32_Vector.GetBoundsD(drawPaths);
  drawPaths := OffsetPath(drawPaths, Margin -rec.Left, Margin -rec.Top);

  BeginUpdate;
  try
    rec := InflateRect(rec, Margin *2, Margin*2);
    OffsetRect(rec,
      mp.X -rec.Left -rec.Width/2,
      mp.Y -rec.Top -rec.Height/2);
    SetBounds(Rect(rec));
    DrawPolygon(Image, drawPaths, frEvenOdd, BrushColor);
    DrawLine(Image, drawPaths, DPIAware(2), PenColor, esPolygon);
    UpdateHitTestMask(drawPaths, frEvenOdd);
  finally
    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
// Miscellaneous
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

function GetBoundsOfAngledRect(const rec: TRectD; angle: double): TRectD;
var
  mp: TPointD;
  path: TPathD;
begin
  mp := Image32_Vector.MidPoint(rec);
  path := Rectangle(rec);
  path := RotatePath(path, mp, angle);
  Result := GetBoundsD(path);
end;

//------------------------------------------------------------------------------
// TMainForm
//------------------------------------------------------------------------------

procedure TMainForm.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 0; //speeds up drawing
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  resStream: TResourceStream;
begin
  layeredImage := TLayeredImage32.Create();
  layeredImage.AddLayer(TDesignerLayer32);

  fontReader := TFontReader.Create;
  fontReader.LoadFromResource('FONT_NSB', RT_RCDATA);
  fontCache := TGlyphCache.Create(fontReader, DPIAware(48));

  words := TStringList.Create;
  words.Capacity := $FFFF;
  resStream := TResourceStream.Create(HInstance, 'WORDS', RT_RCDATA);
  try
    words.LoadFromStream(resStream);
  finally
    resStream.Free;
  end;

  Screen.Cursors[crRotate] :=
    LoadImage(hInstance, 'ROTATE', IMAGE_CURSOR, 32,32, LR_DEFAULTSIZE);
  Screen.Cursors[crMove] :=
    LoadImage(hInstance, 'MOVE', IMAGE_CURSOR, 32,32, LR_DEFAULTSIZE);

  Randomize;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  words.Free;
  fontReader.Free;
  fontCache.Free;
  layeredImage.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickPoint := Point(X,Y);
  clickedLayer := layeredImage.GetLayerAt(clickPoint);

  if not assigned(clickedLayer) then
  begin
    FreeAndNil(sizingButtonGroup);
    FreeAndNil(rotatingButtonGroup);
    targetLayer := nil;
  end

  else if (clickedLayer is TMyRasterLayer32) or
    (clickedLayer is TMyTextLayer32) then
  begin
    FreeAndNil(rotatingButtonGroup);

    //assign targetLayer
    if targetLayer <> clickedLayer then
    begin
      FreeAndNil(sizingButtonGroup);
      FreeAndNil(rotatingButtonGroup);
      targetLayer := clickedLayer;
    end;
    if not Assigned(sizingButtonGroup) then
      sizingButtonGroup := CreateSizingButtonGroup(targetLayer,
        ssCorners, bsRound, DefaultButtonSize, clRed32);
  end;

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  layer: TLayer32;
  rec: TRect;
  dx, dy: integer;
  newAngle: double;
begin
  dx := X - clickPoint.X;
  dy := Y - clickPoint.Y;
  clickPoint := Point(X, Y);

  if not (ssLeft in Shift) then
  begin
    //just moving the unclicked mouse
    //so update the cursor and exit
    layer := layeredImage.GetLayerAt(clickPoint);
    if assigned(layer) then
      Cursor := layer.CursorId else
      Cursor := crDefault;
    Exit;
  end
  //however if nothing was clicked then exit
  else if not Assigned(clickedLayer) then Exit;

  //if click moving a sizing button
  if (clickedLayer is TButtonDesignerLayer32) and
    (clickedLayer.GroupOwner is TSizingGroupLayer32) then
  begin
    clickedLayer.PositionCenteredAt(clickPoint);
    rec := Rect(UpdateSizingButtonGroup(TButtonDesignerLayer32(clickedLayer)));
    if targetLayer is TMyRasterLayer32 then
      with TMyRasterLayer32(targetLayer) do
        Stretch(rec)
    else if targetLayer is TMyTextLayer32 then
      with TMyTextLayer32(targetLayer) do
      begin
        Stretch(rec);
      end;
  end

  //if click moving the rotate button layer
  else if (clickedLayer.GroupOwner = rotatingButtonGroup) then
  begin
    clickedLayer.PositionCenteredAt(clickPoint);
    //Update rotatingButtonGroup and get the new angle
    newAngle := UpdateRotatingButtonGroup(clickedLayer);

    if targetLayer is TMyTextLayer32 then
      with TMyTextLayer32(targetLayer) do
        Rotate(newAngle)
    else if targetLayer is TMyRasterLayer32 then
      with TMyRasterLayer32(targetLayer) do
        Rotate(newAngle);

    if newAngle > PI then newAngle := newAngle - Pi*2;
    StatusBar1.SimpleText := format('angle: %1.0n deg.',[newAngle * 180/pi]);
  end

  //if click moving a non-button layer
  else if (clickedLayer is TMyRasterLayer32) or
    (clickedLayer is TMyTextLayer32) then
  begin
    FreeAndNil(rotatingButtonGroup);
    clickedLayer.Offset(dx, dy);
    if Assigned(sizingButtonGroup) then
      sizingButtonGroup.Offset(dx, dy);
    StatusBar1.SimpleText := '';
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(rotatingButtonGroup) then StatusBar1.SimpleText := '';
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRec: TRect;
begin
  //draw layeredImage onto the form's canvas. However, to optimize performance,
  //only draw what has changed since the last draw (ie in updateRec).
  with layeredImage.GetMergedImage(false, updateRec) do
  begin
    CopyToDc(updateRec, self.Canvas.Handle,
      updateRec.Left, updateRec.Top, false);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  rec: TRect;
begin
  if not visible then Exit; //ie when closing

  // resize layeredImage
  rec := ClientRect;
  Dec(rec.Bottom, StatusBar1.Height);
  layeredImage.SetSize(RectWidth(rec), RectHeight(rec));
  with layeredImage[0] do
  begin
    SetSize(layeredImage.Width, layeredImage.Height);
    HatchBackground(Image);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.AddText1Click(Sender: TObject);
begin
  //just in case the sizing or rotating buttons are visible
  FreeAndNil(sizingButtonGroup);
  FreeAndNil(rotatingButtonGroup);

  //create a text layer
  targetLayer  := layeredImage.AddLayer(TMyTextLayer32);
  with TMyTextLayer32(targetLayer) do
    Init(Words[Random(Words.Count)], layeredImage.MidPoint);

  sizingButtonGroup := CreateSizingButtonGroup(targetLayer, ssCorners,
    bsRound, DefaultButtonSize, clRed32);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.AddImage1Click(Sender: TObject);
var
  pt: TPointD;
begin
  if not OpenDialog1.Execute then Exit;

  //just in case the sizing or rotating buttons are visible
  FreeAndNil(sizingButtonGroup);
  FreeAndNil(rotatingButtonGroup);

  pt := layeredImage.MidPoint;
  targetLayer := TMyRasterLayer32(layeredImage.AddLayer(TMyRasterLayer32));
  with TMyRasterLayer32(targetLayer) do
    Init(OpenDialog1.FileName, layeredImage.MidPoint);

  sizingButtonGroup := CreateSizingButtonGroup(targetLayer, ssCorners,
    bsRound, DefaultButtonSize, clRed32);
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Rotate1Click(Sender: TObject);
var
  angle: double;
begin
  if not assigned(targetLayer) then Exit;

  FreeAndNil(sizingButtonGroup);
  clickedLayer := nil;

  //toggle rotating buttons on and off
  if assigned(rotatingButtonGroup) then
  begin
    FreeAndNil(rotatingButtonGroup);
  end else
  begin
    //restart rotating at the previous rotation angle
    if targetLayer is TMyRasterLayer32 then
      angle := TMyRasterLayer32(targetLayer).CurrentAngle
    else if targetLayer is TMyTextLayer32 then
      angle := TMyTextLayer32(targetLayer).CurrentAngle
    else
      angle := 0;

    rotatingButtonGroup := CreateRotatingButtonGroup(
      targetLayer, DPIAware(10), clWhite32, clLime32, angle);
    rotatingButtonGroup.RotateCursorId := crRotate;

    if angle > PI then angle := angle - Pi*2;
    StatusBar1.SimpleText := format('angle: %1.0n deg.',[angle * 180/pi]);
  end;

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Delete1Click(Sender: TObject);
begin
  if not assigned(targetLayer) then Exit;
  FreeAndNil(targetLayer);
  FreeAndNil(sizingButtonGroup);
  clickedLayer := nil;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.SendBackOne1Click(Sender: TObject);
begin
  if assigned(targetLayer) and
    (targetLayer.Index > 1) then //ie don't send behind the hatched layer :).
      targetLayer.SendBackOne;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.BringForwardOne1Click(Sender: TObject);
begin
  if assigned(targetLayer) then targetLayer.BringForwardOne;
  Invalidate;
end;
//------------------------------------------------------------------------------

end.
