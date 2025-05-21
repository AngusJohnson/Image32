unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Rtti, Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls, FMX.Platform, FMX.Surfaces,
  FMX.StdCtrls, FMX.Controls.Presentation, Img32, Img32.FMX, Img32.Layers,
  FMX.ListBox, System.ImageList, FMX.ImgList, Img32.Fmt.SVG;


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
    layeredImg32: TLayeredImage32;
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
  Img32.Vector, Img32.Transform, Img32.Draw, Img32.Extra,
    Img32.Text, Img32.Clipper2;

ResourceString
  rsBigText = 'Image32 Rocks!';


//------------------------------------------------------------------------------
// TMainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  matrix: TMatrixD;
  fontCache: TFontCache;
  fontReader : TFontReader;
begin
  margin := DPIAware(14);
  Layout1.Scale.Point := PointF(1 / DpiAwareOne, 1 / DpiAwareOne);

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
  fontCache := TFontCache.Create(fontReader, DPIAware(11));
  try
    //connect fontReader to a simple ttf font resource
    //and get 'copyright' glyph outline ...
    fontReader.LoadFromResource('FONT_1', RT_RCDATA);
    if fontReader.IsValidFontFormat then
      copyrightGlyphs :=
        fontCache.GetTextOutline(0, 0, #$00A9' 2021 Angus Johnson');

    //connect fontReader to a decorative ttf font resource
    //and get 'bigText' glyph outlines ...
    fontReader.LoadFromResource('FONT_2', RT_RCDATA);
    //nb: fontCache automatically updates when fontReader changes
    fontCache.FontHeight := DPIAware(50);
    if fontReader.IsValidFontFormat then
      bigTextGlyphs := fontCache.GetTextOutline(0, 0, rsBigText);

    bigTextGlyphs := InflatePaths(bigTextGlyphs, 1.5, jsAuto);
    matrix := IdentityMatrix;
    MatrixScale(matrix, 1, 1.75); //stretched vertically
    MatrixSkew(matrix, -0.25, 0); //and italicized too.
    MatrixApply(matrix, bigTextGlyphs);

  finally
    fontCache.Free;
    fontReader.free;
  end;

  layeredImg32 := TLayeredImage32.Create;
  layeredImg32.BackgroundColor := bkColor;

  bookImg := TImage32.Create;
  bookImg.LoadFromResource('BOOKS', RT_RCDATA);

  btnRefresh.Enabled := false;
  Timer1.Enabled := false;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  bookImg.Free;
  layeredImg32.Free;
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
  layeredImg32.Clear;

  with Layout1 do
    rec := Img32.Vector.Rect(0, 0, Trunc(Width), Trunc(Height));
  layeredImg32.SetSize(rec.Width, rec.Height);
  Img32.Vector.InflateRect(rec, -margin, -margin);

  textRec := Img32.Vector.GetBounds(bigTextGlyphs);
  //scale and position bigTextGlyphs so it fits
  //comfortably into the ImageViewer
  scale := (rec.Width - margin * 2) / textRec.Width;
  mainGlyphs := Img32.Vector.ScalePath(bigTextGlyphs, scale);
  textRec := Img32.Vector.GetBounds(mainGlyphs);
  mainGlyphs := TranslatePath(mainGlyphs, -textRec.left, -textRec.top);

  //reposition copyright text
  tmpRec := Img32.Vector.GetBounds(copyrightGlyphs);
  copyrightGlyphs := TranslatePath(copyrightGlyphs,
    rec.Right - tmpRec.Right - margin,
    rec.Bottom - tmpRec.Top - margin * 2);

  //add the bottom-most layer
  with layeredImg32.AddLayer(TVectorLayer32) as TVectorLayer32 do
  begin
    SetInnerBounds(layeredImg32.Root.InnerBounds);

    //draw a rectangular frame
    tmpPath := Rectangle(rec);
    Image.FillRect(rec, clWhite32);
    DrawLine(Image, tmpPath, DPIAware(1), penColor, esPolygon);

    //draw copyright
    DrawPolygon(Image, copyrightGlyphs, frNonZero, clBlack32);

    //get destination rect for books image ...
    if rec.Width < rec.Height then
      i := rec.Width div 2 - margin else
      i := rec.Height div 2 - margin;
    with Img32.Vector.MidPoint(rec) do
      tmpRec := Img32.Vector.Rect(X, Y, X, Y);
    Img32.Vector.InflateRect(tmpRec, i, i);
    Image.CopyBlend(bookImg, bookImg.Bounds, tmpRec, BlendToOpaque);
  end;

  //now add lots of 'zoom' layers
  for i := 0 to 19 do
    with layeredImg32.AddLayer(TVectorLayer32) as TVectorLayer32 do
    begin
      Visible := false;
      SetInnerBounds(RectD(textRec));
      PositionCenteredAt(layeredImg32.MidPoint);
      DrawPolygon(Image, mainGlyphs, frNonZero, txtColor);
      DrawLine(Image, mainGlyphs, 1.5, clBlack32, esPolygon);
      Draw3D(Image, mainGlyphs, frNonZero, Round(textRec.Height / 30), 3);
      mainGlyphs := ScalePath(mainGlyphs, 0.9, 0.9);
      textRec := Img32.Vector.GetBounds(mainGlyphs);
    end;
  btnRefresh.Enabled := false;
  zoomIdx := 0;
  Timer1.Enabled := true;
  Layout1.Repaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Layout1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  bmp: TBitmap;
  img: TImage32;
  rec: TRectF;
begin
  img := layeredImg32.GetMergedImage(false);
  //unfortunately we need an intermediate TBitmap object
  //to draw raster images onto an FMX canvas.
  bmp := TBitmap.Create;
  try
    AssignImage32ToFmxBitmap(img, bmp);
    rec := RectF(0, 0, bmp.Width, bmp.Height);
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
  if (zoomIdx < 1) or (zoomIdx >= layeredImg32.Root.ChildCount) then
  begin
    zoomIdx := layeredImg32.Root.ChildCount -1;
    Timer1.Interval := 25;
  end
  else if zoomIdx = 1 then
  begin
    Timer1.Enabled := false;
    btnRefresh.Enabled := true;
    Exit;
  end else
  begin
    layeredImg32.Root[zoomIdx].Visible := false;
    Dec(zoomIdx);
  end;
  layeredImg32.Root[zoomIdx].Visible := true;
  Layout1.Repaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  btnRefresh.Enabled := false;
  zoomIdx := layeredImg32.Root.ChildCount;
  layeredImg32.Root[1].Visible := false;
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
  layeredImg32.SetSize(ClientWidth, ClientHeight);

  btnClose.Position.X := ClientWidth - btnClose.Width - margin;
  btnClose.Position.Y := ClientHeight - btnClose.Height - margin * 1.75;

  btnRefresh.Position.X := btnClose.Position.X;
  btnRefresh.Position.Y := btnClose.Position.Y - btnRefresh.Height - margin / 2;

  Timer1.Enabled := false;
  ReloadImage;
end;
//------------------------------------------------------------------------------

end.
