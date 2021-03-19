unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Rtti, Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls, FMX.Platform, FMX.Surfaces,
  FMX.StdCtrls, FMX.Controls.Presentation, Image32, Image32_FMX, Image32_Layers;


type
  TMainForm = class(TForm)
    Timer1: TTimer;
    btnRefresh: TButton;
    btnClose: TButton;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure Layout1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure FormResize(Sender: TObject);
  private
    zoomIdx: integer;
    bigTextGlyphs, copyrightGlyphs: TPathsD;
    bookImg: TImage32;
    layeredImage32: TLayeredImage32;
    procedure ReloadImage;
  public
  end;

var
  MainForm: TMainForm;
  bkColor, penColor, txtColor: TColor32;
  margin: integer;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}

{$R font.res}
{$R image.res}

uses
  Image32_Vector, Image32_Transform, Image32_Draw, Image32_Extra,
    Image32_Ttf, Image32_Clipper;

ResourceString
  rsBigText = 'Image32 Rocks!';


//------------------------------------------------------------------------------
// TMainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  matrix: TMatrixD;
  fontCache: TGlyphCache;
  fontReader : TFontReader;
begin
  margin := Round(screenScale * 14);
  Layout1.Scale.Point := PointF(1/ScreenScale,1/ScreenScale);

  bkColor  := $FFF8F8BB; //yellow
  penColor := clMaroon32;
  txtColor := $FF00BB00; //green
{$IFNDEF MSWINDOWS}
  bkColor  := SwapRedBlue(bkColor);
  penColor := SwapRedBlue(penColor);
  txtColor := SwapRedBlue(txtColor);
{$ENDIF}

  //GET SOME TEXT OUTLINES READY TO DRAW LATER ...

  //create a fontReader to access truetype font files (*.ttf) that
  //have been stored as font resources and create a glyph cache too
  fontReader := TFontReader.Create;
  fontCache := TGlyphCache.Create(fontReader, screenScale *13);
  try
    //connect fontReader to a simple ttf font resource
    //and get 'copyright' glyph outline ...
    fontReader.LoadFromResource('FONT_1', RT_RCDATA);
    if fontReader.IsValidFontFormat then
      copyrightGlyphs :=
        fontCache.GetTextGlyphs(0,0,#$00A9' 2020 Angus Johnson');

    //connect fontReader to a decorative ttf font resource
    //and get 'bigText' glyph outlines ...
    fontReader.LoadFromResource('FONT_2', RT_RCDATA);
    fontCache.FontHeight := screenScale * 25;
    if fontReader.IsValidFontFormat then
      bigTextGlyphs := fontCache.GetTextGlyphs(0, 0, rsBigText);

    bigTextGlyphs := InflatePaths(bigTextGlyphs,1.5,jsAuto,esPolygon); //bolder
    matrix := IdentityMatrix;
    MatrixScale(matrix, 1, 1.75); //stretched vertically
    MatrixSkew(matrix, -0.25, 0); //and italicized too.
    MatrixApply(matrix, bigTextGlyphs);

  finally
    fontCache.Free;
    fontReader.free;
  end;

  layeredImage32 := TLayeredImage32.Create;
  layeredImage32.BackgroundColor := bkColor;

  bookImg := TImage32.Create;
  bookImg.LoadFromResource('BOOKS', RT_RCDATA);

  btnRefresh.Enabled := false;
  Timer1.Enabled := false;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  bookImg.Free;
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ReloadImage;
var
  i: integer;
  mainGlyphs: TPathsD;
  tmpPath: TPathD;
  rec, textRec, tmpRec: TRect;
  scale: double;
begin
  //clear (all layers in) layeredImage32
  layeredImage32.Clear;

  with Layout1 do
    rec := Image32_Vector.Rect(0, 0, Trunc(Width), Trunc(Height));
  layeredImage32.SetSize(rec.Width, rec.Height);
  rec := Image32_Vector.InflateRect(rec, -margin, -margin);

  textRec := Image32_Vector.GetBounds(bigTextGlyphs);
  //scale and position bigTextGlyphs so it fits
  //comfortably into the ImageViewer
  scale := (rec.Width - margin*2) / textRec.Width;
  mainGlyphs := Image32_Vector.ScalePath(bigTextGlyphs, scale);
  textRec := Image32_Vector.GetBounds(mainGlyphs);
  mainGlyphs := Image32_Vector.OffsetPath(mainGlyphs, -textRec.left, -textRec.top);

  //scale and position copyright text
  tmpRec := Image32_Vector.GetBounds(copyrightGlyphs);
  copyrightGlyphs := Image32_Vector.OffsetPath(copyrightGlyphs,
    rec.Right -tmpRec.Right -margin,
    rec.Bottom -tmpRec.Top -margin *2);

  //add the bottom-most layer
  with layeredImage32.AddLayer(TVectorLayer32) as TVectorLayer32 do
  begin
    SetBounds(layeredImage32.Root.Bounds);

    //draw a rectangular frame
    tmpPath := Rectangle(rec);
    Image.FillRect(rec, clWhite32);
    DrawLine(Image, tmpPath, DPIAwareFMX(1), penColor, esPolygon);

    //draw copyright
    DrawPolygon(Image, copyrightGlyphs, frNonZero, clBlack32);

    //get destination rect for books image ...
    if rec.Width < rec.Height then
      i := rec.Width div 2 - margin else
      i := rec.Height div 2 - margin;
    with Image32_Vector.MidPoint(rec) do
      tmpRec := Image32_Vector.Rect(X,Y,X,Y);
    tmpRec := Image32_Vector.InflateRect(tmpRec, i, i);
    Image.CopyBlend(bookImg, bookImg.Bounds, tmpRec, BlendToOpaque);
  end;

  //now add lots of 'zoom' layers
  for i := 0 to 19 do
    with layeredImage32.AddLayer(TVectorLayer32) as TVectorLayer32 do
    begin
      Visible := false;
      SetBounds(textRec);
      PositionCenteredAt(layeredImage32.MidPoint);
      DrawPolygon(Image, mainGlyphs, frNonZero, txtColor);
      if i = 0 then
      begin
        DrawLine(Image, mainGlyphs, 1.5, clBlack32, esClosed);
        Draw3D(Image, mainGlyphs, frNonZero, Round(textRec.Height/15), 3);
      end;
      mainGlyphs := ScalePath(mainGlyphs, 0.9, 0.9);
      textRec := Image32_Vector.GetBounds(mainGlyphs);
    end;

  btnRefresh.Enabled := false;
  zoomIdx := 0;
  Timer1.Enabled := true;
  Layout1.Repaint;
end;
//------------------------------------------------------------------------------

procedure CopyImage32ToFmxBitmap(img: TImage32; bmp: TBitmap);
var
  src, dst: TBitmapData;
begin
  src := TBitMapData.Create(img.Width, img.Height, TPixelFormat.BGRA);
  src.Data := img.PixelBase;
  src.Pitch := img.Width * 4;
  if not Assigned(img) or not Assigned(bmp) then Exit;
  bmp.SetSize(img.Width, img.Height);
  if bmp.Map(TMapAccess.Write, dst) then
  try
    dst.Copy(src);
  finally
    bmp.Unmap(dst);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Layout1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  bmp: TBitmap;
  img: TImage32;
  rec: TRectF;
begin
  img := layeredImage32.GetMergedImage(false);

  //unfortunately we need an intermediate TBitmap object
  //to draw raster images onto an FMX canvas.
  bmp := TBitmap.Create;
  try
    CopyImage32ToFmxBitmap(img, bmp);
    rec := RectF(0,0,bmp.Width,bmp.Height);
    Canvas.Lock;
    Canvas.DrawBitmap(bmp, rec, rec, 1.0);
    Canvas.Unlock;
  finally
    bmp.Free;
  end;

end;
//------------------------------------------------------------------------------

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if (zoomIdx < 1) or (zoomIdx >= layeredImage32.Root.ChildCount) then
  begin
    zoomIdx := layeredImage32.Root.ChildCount -1;
    Timer1.Interval := 25;
  end
  else if zoomIdx = 1 then
  begin
    Timer1.Enabled := false;
    btnRefresh.Enabled := true;
    Exit;
  end else
  begin
    layeredImage32.Root[zoomIdx].Visible := false;
    Dec(zoomIdx);
  end;
  layeredImage32.Root[zoomIdx].Visible := true;
  Layout1.Repaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  btnRefresh.Enabled := false;
  zoomIdx := layeredImage32.Root.ChildCount;
  layeredImage32.Root[1].Visible := false;
  Layout1.Repaint;
  Timer1.Interval := 1000;
  Timer1.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  layeredImage32.SetSize(ClientWidth, ClientHeight);

  btnClose.Position.X := ClientWidth - btnClose.Width - margin;
  btnClose.Position.Y := ClientHeight - btnClose.Height - margin * 1.75;

  btnRefresh.Position.X := btnClose.Position.X;
  btnRefresh.Position.Y := btnClose.Position.Y - btnRefresh.Height - margin/2;

  Timer1.Enabled := false;
  ReloadImage;
end;
//------------------------------------------------------------------------------


end.
