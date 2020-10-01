unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.TabControl,
  Image32, Image32_Vector, Image32_Draw, Image32_Extra, Image32_FMX,
  Image32_Ttf, Image32_Clipper;

type
  TMainForm = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Timer1: TTimer;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure Layout1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure TabControl1Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    margin: integer;
    fontHeight: double;
    clockRadius: double;
    clBackground, clDarkRed: TColor32;
    clDarkMaroon, clNearWhite, clDarkBlue: TColor32;
    clLightGray, clMidGray, clDarkGray: TColor32;

    imgMain, imgClockface: TImage32;
    essay: string;
    procedure UpdateImage1;
    procedure UpdateImage2;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{$R font.res}
{$R image.res}

{$ZEROBASEDSTRINGS OFF}

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function OffsetRectF(const rec: TRectF; x, y: single): TRectF;
begin
  result.Left := rec.Left + x;
  result.Right := rec.Right + x;
  result.Top := rec.Top + y;
  result.Bottom := rec.Bottom + y;
end;
//------------------------------------------------------------------------------

procedure CopyImage32ToBitmap(img: TImage32; bmp: TBitmap);
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    img.SaveToStream(stream, 'PNG');
    bmp.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

//------------------------------------------------------------------------------
// MainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  rs: TResourceStream;
begin

  clDarkRed     := clMaroon32;
  clDarkMaroon  := $FF400000;
  clDarkBlue    := clNavy32;
  clBackground  := $FFF8F8F8;
  clNearWhite   := $FFF0F0F8;
  clLightGray   := $FFC0C0C8;
  clMidGray     := $FFAAAAAA;
  clDarkGray    := $FF999999;
{$IFDEF ANDROID}
  clDarkRed := SwapRedBlue(clDarkRed);
  clNearWhite := SwapRedBlue(clNearWhite);
  clLightGray := SwapRedBlue(clLightGray);
  clDarkBlue := SwapRedBlue(clDarkBlue);
  clDarkMaroon := SwapRedBlue(clDarkMaroon);
{$ENDIF}

  imgMain      := TImage32.Create();
  imgClockface := TImage32.Create();

  ScreenScale := 2;
  margin := DPIAware(20);

  rs := TResourceStream.Create(hInstance, 'ESSAY', RT_RCDATA);
  with TStringStream.Create('', TEncoding.Unicode) do
  try
    LoadFromStream(rs);
    essay := ReadString(Size);
  finally
    free;
    rs.Free;
  end;
  essay := StringReplace(essay, '\n', #10, [rfReplaceAll]);

  fontHeight := DPIAware( PointHeightToPixelHeight(12) );
  UpdateImage1;

{$IFNDEF ANDROID}
  //center on desktop (Form.Position seems borked in FMX!?)
  with Screen.DesktopRect do
  begin
    self.Left := (Width - DPIAware(self.Width)) div 2;
    self.Top := (Height - DPIAware(self.Height)) div 2;
  end;
{$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  imgMain.Free;
  imgClockface.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateImage1;
var
  nextCharIdx, innerMargin: integer;
  rectangle: TPathD;
  txtPaths: TPathsD;
  outerRec, innerRec, tmpRec, imageRec: TRect;
  nextCharPt: TPointD;
  tmpStr: string;

  imgBooks: TImage32;
  glyphCache: TGlyphCache;
  fontReader : TTtfFontReader;
begin
  Timer1.Enabled := false;
  //IMPORTANT: we need to undo any screen scaling
  //as it causes unacceptible blurring (especially of text) ...
  Layout1.Scale.Point := PointF(1/ScreenScale,1/ScreenScale);

  innermargin := margin div 2;
  outerRec := Image32_Vector.Rect(0, 0,
    Round(DPIAware(ClientWidth) - margin * 2),
    Round(DPIAware(ClientHeight - TabControl1.TabHeight) - margin * 2));

  imgMain.SetSize(outerRec.Width, outerRec.Height);
  //color fill the base image and give it a border ...
  imgMain.Clear(clWhite32);

  //create a fontReader to access truetype font files (*.ttf) that
  //have been stored as font resources and create a glyph cache too
  fontReader := TTtfFontReader.Create;
  glyphCache := TGlyphCache.Create(fontReader, fontHeight);
  try
    fontReader.LoadFromResource('FONT_1', RT_RCDATA); // see font.res
    if not fontReader.IsValidFontFormat then Exit;

    //load an image and scale it to a useful size ...
    imgBooks := TImage32.Create();
    try
      imgBooks.LoadFromResource('PNGIMAGE', RT_RCDATA);
      imgBooks.Scale((outerRec.Width - innermargin*2)/(imgBooks.Width *2));

      innerRec := Image32_Vector.InflateRect(outerRec, -2, -2);
      rectangle := Image32_Vector.Rectangle(innerRec);
      DrawLine(imgMain, rectangle, 2, clDarkRed, esClosed);

      innerRec := Image32_Vector.InflateRect(outerRec,
        -innermargin,-innermargin);

      //draw an image at the top right
      imageRec := imgBooks.Bounds;
      Image32_Vector.OffsetRect(imageRec,
        outerRec.Width div 2, innermargin);
      imgMain.CopyBlend(imgBooks, imgBooks.Bounds, imageRec, BlendToOpaque);
    finally
      imgBooks.Free;
    end;

    //draw text starting at the top left
    //while avoiding the image on the right
    tmpRec := innerRec;
    tmpRec.Width := tmpRec.Width div 2 - innermargin;
    tmpRec.Bottom := innermargin div 2 +
      Min(imageRec.Bottom + Round(glyphCache.Ascent), innerRec.Bottom);
    glyphCache.GetTextGlyphs(tmpRec, essay,
      taJustify, tvaTop, txtPaths, nextCharIdx, nextCharPt);
    DrawPolygon(imgMain, txtPaths, frNonZero, clBlack32);

    //now draw text that didn't fit on the left of the image
    if (nextCharIdx <> 0) then
    begin
      tmpStr := Copy(essay, nextCharIdx, Length(essay));
      if (tmpStr[1] <= #32) then Delete(tmpStr,1,1);
      innerRec.Top := Round(nextCharPt.Y);
      glyphCache.GetTextGlyphs(innerRec, tmpStr,
        taJustify, tvaTop, txtPaths, nextCharIdx, nextCharPt);
      DrawPolygon(imgMain, txtPaths, frNonZero, clBlack32);
    end;

  finally
    glyphCache.Free;
    fontReader.free;
  end;
  Layout1.Repaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateImage2;
var
  i, nextCharIdx: integer;
  x,y: double;
  blockWidth, dashLen, frameWidth, lineWidth6: double;
  path, path2, path3: TPathD;
  paths: TPathsD;
  rec: TRect;
  recD, recD2: TRectD;
  nextCharPt: TPointD;
  ir: TImageRenderer;
  lgr: TLinearGradientRenderer;
  fontReader: TTtfFontReader;
  glyphCache: TGlyphCache;
begin
  imgClockface.SetSize(
    DPIAware(ClientWidth) - margin * 2,
    Round(DPIAware(ClientHeight - TabControl1.TabHeight)) - margin * 2);
  imgClockface.Clear(clBackground);

  //DRAW WATCH BAND
  //use an image renderer to tile fill the watch band (polygon)
  ir := TImageRenderer.Create;
  try
    ir.SetTileFillStyle(tfsRotate180); //or tfsMirrorHorz or tfsRepeat
    ir.Image.LoadFromResource('KOOKABURRA' , RT_RCDATA); // see image.res
    ir.Image.Resize(DPIAware(16), DPIAware(16));
    lineWidth6 := ScreenScale * 6;
    if imgClockface.Width > imgClockface.Height then // landscape
    begin
      recD := RectD(0, imgClockface.Height /4,
        imgClockface.Width, imgClockface.Height * 0.75);
      path := Rectangle(recD);
      DrawPolygon(imgClockface, path, frEvenOdd, ir);
      path2 := Copy(path, 0,2);
      path3 := Copy(path, 2,2);
      DrawLine(imgClockface, path2, lineWidth6, clDarkMaroon, esClosed);
      DrawLine(imgClockface, path3, lineWidth6, clDarkMaroon, esClosed);
    end else
    begin                                            // portrait
      recD := RectD(imgClockface.Width /4, 0,
        imgClockface.Width * 0.75, imgClockface.Height);
      path := Rectangle(recD);
      DrawPolygon(imgClockface, path, frEvenOdd, ir);
      path2 := Copy(path, 1,2);
      SetLength(path3, 2); path3[0] := path[3]; path3[1] := path[0];
      DrawLine(imgClockface, path2, lineWidth6, clDarkMaroon, esClosed);
      DrawLine(imgClockface, path3, lineWidth6, clDarkMaroon, esClosed);
    end;
  finally
    ir.Free;
  end;

  //DRAW THE WATCH FRAME

  clockRadius := min(imgClockface.Width, imgClockface.Height) *2 div 5;
  x := imgClockface.Width / 2; y := imgClockface.Height / 2;
  recD := RectD(x-clockRadius, y-clockRadius, x+clockRadius, y+clockRadius);
  frameWidth := clockRadius /6;

  //outer ring:
  //use linear gradient fills to give the impression of 3D light and shadow

  path := Image32_Vector.Ellipse(recD);
  recD2 := InflateRect(recD, frameWidth/2, frameWidth/2);
  path2 := Image32_Vector.Ellipse(recD2);
  DrawShadow(imgClockface, path2, frEvenOdd, frameWidth/4, angle315);

  lgr := TLinearGradientRenderer.Create;
  try
    with recD do
      lgr.SetParameters(
        PointD(Right,Top), PointD(Left,Bottom),
        clNearWhite, clLightGray, gfsClamp);
    DrawLine(imgClockface, path, frameWidth, lgr, esClosed);
    recD := inflateRect(recD, frameWidth/2, frameWidth/2);
    path2 := Ellipse(recD);
    DrawLine(imgClockface, path2, 1.0, clGray32, esClosed);
    //inner ring
    recD := inflateRect(recD, -frameWidth, -frameWidth);
    path2 := Ellipse(recD);
    with recD do
      lgr.SetParameters(
        PointD(Left, Bottom), PointD(Right, Top),
        clNearWhite, clDarkGray, gfsClamp);
    DrawLine(imgClockface, path2, frameWidth/3, lgr, esClosed);
  finally
    lgr.Free;
  end;

  //DRAW THE WATCH FACE

  recD := inflateRect(recD, -frameWidth/8, -frameWidth/8);
  path2 := Image32_Vector.Ellipse(recD);
  DrawLine(imgClockface, path2, 1.0, clSilver32, esClosed);
  DrawPolygon(imgClockface, path2, frEvenOdd, clWhite32);

  //draw the "maker's mark"
  rec := Image32_Vector.Rect(recD);
  Image32_Vector.OffsetRect(rec, 0, Round(-clockRadius *2/5));
  fontReader := TTtfFontReader.Create;
  glyphCache := TGlyphCache.Create(fontReader, clockRadius / 12);
  try
    fontReader.LoadFromResource('FONT_1', RT_RCDATA);
    glyphCache.GetTextGlyphs(rec, 'angusj',
      taCenter, tvaMiddle, paths, nextCharIdx, nextCharPt);
    DrawPolygon(imgClockface, paths, frNonZero, clBlack32);
  finally
    fontReader.Free;
    glyphCache.Free;
  end;

  //draw minute marks ...
  blockWidth  := clockRadius /30;
  dashLen     := clockRadius /14;
  recD := inflateRect(recD, -dashLen, -dashLen);
  path  := Ellipse(recD, 60);
  recD := inflateRect(recD, -dashLen, -dashLen);
  path2  := Ellipse(RecD, 60);
  SetLength(path3, 2);
  for i := 0 to high(path) do
  begin
    path3[0] := path[i]; path3[1] := path2[i];
    if (i) mod 5 = 0 then
      DrawLine(imgClockface, path3, blockWidth, clGray32, esSquare) else
      DrawLine(imgClockface, path3, ScreenScale, clMidGray, esButt);
  end;
  imgMain.Assign(imgClockface);
  Timer1Timer(nil);

  Timer1.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  path, pathShad: TPathD;
  mp, shadOffset: TPointD;
  angle: double;
  sinA, cosA: Extended;
  anow: TDateTime;
  secHandLen, minHandLen, hrHandLen: double;
  handWidth: double;
const
  secsPerDay = 24*60*60;
begin
  //round to nearest second (avoids second hand being stuck between seconds)
  anow := Round(Frac(Now) * secsPerDay)/secsPerDay;

  //UPDATE CLOCK HANDS

  secHandLen   := clockRadius * 0.75;
  minHandLen   := clockRadius * 0.65;
  hrHandLen    := clockRadius * 0.46;
  handWidth    := clockRadius /15;
  shadOffset.X := handWidth/4;
  shadOffset.Y :=  handWidth/3;

  imgMain.Assign(imgClockface);

  //draw hour hand
  angle := Frac(anow *2); //fraction of 12hrs elapsed
  angle := angle90 + angle*(pi*2);
  SinCos(angle, sinA, cosA);
  setLength(path, 2);
  mp := PointD(imgMain.Width/2, imgMain.Height/2);
  path[0] := PointD(mp.X + cosA * hrHandLen/5, mp.Y + sinA * hrHandLen/5);
  path[1] := PointD(mp.X - cosA * hrHandLen, mp.Y - sinA * hrHandLen);
  pathShad := OffsetPath(path, shadOffset.X, shadOffset.Y);
  DrawLine(imgMain, pathShad, handWidth, $30000000, esSquare);
  DrawLine(imgMain, path, handWidth, clDarkBlue, esSquare);

  //draw minute hand
  angle := Frac(anow * 24); //fraction of 1hr elapsed
  angle := pi/2 + angle*(pi*2);
  SinCos(angle, sinA, cosA);
  path[0] := PointD(mp.X + cosA * minHandLen/5, mp.Y + sinA * minHandLen/5);
  path[1] := PointD(mp.X - cosA * minHandLen, mp.Y - sinA * minHandLen);
  pathShad := OffsetPath(path, shadOffset.X, shadOffset.Y);
  DrawLine(imgMain, pathShad, handWidth, $30000000, esSquare);
  DrawLine(imgMain, path, handWidth, clDarkBlue, esSquare);

  //draw second hand
  angle := Frac(anow * 24 *60); //fraction of 1min  elapsed
  angle := pi/2 + angle*(pi*2);
  SinCos(angle, sinA, cosA);
  path[0] := PointD(mp.X + cosA * secHandLen/5, mp.Y + sinA * secHandLen/5);
  path[1] := PointD(mp.X - cosA * secHandLen, mp.Y - sinA * secHandLen);
  pathShad := OffsetPath(path, shadOffset.X, shadOffset.Y);
  DrawLine(imgMain, pathShad, handWidth/3, $30000000, esSquare);
  DrawLine(imgMain, path, handWidth/3, clDarkRed, esSquare);
  DrawPoint(imgMain, mp, handWidth, clDarkRed);

  Layout1.Repaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Layout1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  tmpBitmap: TBitmap;
  srcRec, dstRec: TRectF;
begin
  srcRec := RectF(0,0,imgMain.Width,imgMain.Height);
  dstRec := OffsetRectF(srcRec,
    (Layout1.Width - imgMain.Width)/2, margin);

  tmpBitmap := TBitmap.Create;
  try
    CopyImage32ToBitmap(imgMain,tmpBitmap);
    Canvas.Lock;
    Canvas.DrawBitmap(tmpBitmap, srcRec, dstRec, 1);
    Canvas.UnLock;
  finally
    tmpBitmap.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  TabControl1Change(nil);
end;
//------------------------------------------------------------------------------

procedure TMainForm.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab = TabItem1 then  UpdateImage1
  else if TabControl1.ActiveTab = TabItem2 then UpdateImage2
  else Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if key = 27 then Close;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


end.
