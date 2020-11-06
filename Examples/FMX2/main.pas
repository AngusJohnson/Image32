unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.TabControl,
  Image32;

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
    procedure FormActivate(Sender: TObject);
  private
    margin: integer;
    fontHeight: double;
    clockRadius: integer;
    clBackground, clDarkRed: TColor32;
    clDarkMaroon, clNearWhite, clDarkGreen: TColor32;
    clLightGray, clMidGray, clDarkGray: TColor32;

    imgMain, imgClockface: TImage32;
    essay: string;
    procedure DrawTextTab;
    procedure DrawClockTab;
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

uses
  Image32_Vector, Image32_Draw, Image32_Extra, Image32_FMX,
  Image32_Ttf, Image32_Clipper;

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

  clDarkRed     := $FFCC0000;
  clDarkMaroon  := $FF400000;
  clDarkGreen   := $FF00AA00;
  clBackground  := $FFF8F8F8;
  clNearWhite   := $FFF0F0F8;
  clLightGray   := $FFC0C0C8;
  clMidGray     := $FFAAAAAA;
  clDarkGray    := $FF999999;
{$IFDEF ANDROID}
  clDarkRed := SwapRedBlue(clDarkRed);
  clNearWhite := SwapRedBlue(clNearWhite);
  clLightGray := SwapRedBlue(clLightGray);
  clDarkGreen := SwapRedBlue(clDarkGreen);
  clDarkMaroon := SwapRedBlue(clDarkMaroon);
{$ENDIF}

  imgMain      := TImage32.Create();
  imgClockface := TImage32.Create();
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

  Layout1.Scale.Point := PointF(1/ScreenScale,1/ScreenScale);
  fontHeight := DPIAware( PointHeightToPixelHeight(9));
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  imgMain.Free;
  imgClockface.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DrawTextTab;
var
  nextCharIdx, innerMargin, w,h: integer;
  rectangle: TPathD;
  txtPaths: TPathsD;
  outerRec, innerRec, tmpRec, imageRec: TRect;
  nextCharPt: TPointD;
  tmpStr: string;

  imgBooks: TImage32;
  noto14Cache: TGlyphCache;
  notoSansReg : TFontReader;
  useClearType: Boolean;
begin
  Timer1.Enabled := false;
  //IMPORTANT: we need to avoid any screen scaling
  //as it causes unacceptible blurring (especially of text) ...
  outerRec := Image32_Vector.Rect(0, 0,
    Round(DPIAware(ClientWidth) - margin * 2),
    Round(DPIAware(ClientHeight - TabControl1.TabHeight) - margin * 2));

  imgMain.SetSize(outerRec.Width, outerRec.Height);
  //color fill the base image and give it a border ...
  imgMain.Clear(clWhite32);

  useClearType := fontHeight < 14;
  innermargin := margin div 2;

  notoSansReg := TFontReader.Create;
  noto14Cache := TGlyphCache.Create(notoSansReg, fontHeight);
  try
    //load truetype font file stored as a resource (see font.res)
    notoSansReg.LoadFromResource('FONT_1', RT_RCDATA);
    if not notoSansReg.IsValidFontFormat then Exit;

    tmpRec := Image32_Vector.InflateRect(outerRec, -1, -1);
    rectangle := Image32_Vector.Rectangle(tmpRec);
    DrawLine(imgMain, rectangle, 2, clDarkRed, esPolygon);

    innerRec := Image32_Vector.InflateRect(outerRec,
      -innermargin, -innermargin);
    w := RectWidth(innerRec) div 2; //half innerRec.width

    //load an image and scale it to a useful size ...
    imgBooks := TImage32.Create();
    try
      imgBooks.LoadFromResource('PNGIMAGE', RT_RCDATA);
      //draw an image at the top right
      imgBooks.Scale(w / imgBooks.Width);
      imageRec := imgBooks.Bounds;
      Image32_Vector.OffsetRect(imageRec,
        imgBooks.Width + innermargin, innermargin);
      imgMain.CopyBlend(imgBooks,
        imgBooks.Bounds, imageRec, BlendToOpaque);
      h := imgBooks.Height;
    finally
      imgBooks.Free;
    end;

    //draw text starting at the top left corner while avoiding the image ...
    tmpRec := Image32_Vector.Rect(innerMargin, innerMargin, w,
      h + innerMargin + Round(noto14Cache.LineHeight));

    noto14Cache.GetTextGlyphs(tmpRec, essay,
      taJustify, tvaTop, txtPaths, nextCharIdx, nextCharPt);
    if useClearType then
      DrawPolygon_ClearType(imgMain, txtPaths, frNonZero, clBlack32) else
      DrawPolygon(imgMain, txtPaths, frNonZero, clBlack32);

    //now draw the remaining text under the image
    if (nextCharIdx <> 0) then
    begin
      tmpStr := Copy(essay, nextCharIdx, Length(essay));
      if (tmpStr[1] <= #32) then Delete(tmpStr,1,1); //ie skip a CR or a space

      innerRec.Top := Round(nextCharPt.Y);
      noto14Cache.GetTextGlyphs(innerRec, tmpStr,
        taJustify, tvaTop, txtPaths, nextCharIdx, nextCharPt);

      if useClearType then
        DrawPolygon_ClearType(imgMain, txtPaths, frNonZero, clBlack32) else
        DrawPolygon(imgMain, txtPaths, frNonZero, clBlack32);
    end;

  finally
    noto14Cache.Free;
    notoSansReg.free;
  end;
  Layout1.Repaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DrawClockTab;
var
  i,j, nextCharIdx: integer;
  x,y: double;
  frameWidth, lineWidth6: double;
  path, path2, path3: TPathD;
  paths: TPathsD;
  recI: TRect;
  recD, rec2, rec3: TRectD;
  nextCharPt, mp, mp3: TPointD;
  ir: TImageRenderer;
  lgr: TLinearGradientRenderer;
  fontReader: TFontReader;
  glyphCache: TGlyphCache;

  numGlyphs: array[1..12] of TPathsD;
  tmpImg: TImage32;
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
    ir.Image.LoadFromResource('RED_DOT' , RT_RCDATA); // see image.res
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
      DrawLine(imgClockface, path2, lineWidth6, clDarkMaroon, esPolygon);
      DrawLine(imgClockface, path3, lineWidth6, clDarkMaroon, esPolygon);
    end else
    begin                                            // portrait
      recD := RectD(imgClockface.Width /4, 0,
        imgClockface.Width * 0.75, imgClockface.Height);
      path := Rectangle(recD);
      DrawPolygon(imgClockface, path, frEvenOdd, ir);
      path2 := Copy(path, 1,2);
      SetLength(path3, 2); path3[0] := path[3]; path3[1] := path[0];
      DrawLine(imgClockface, path2, lineWidth6, clDarkMaroon, esPolygon);
      DrawLine(imgClockface, path3, lineWidth6, clDarkMaroon, esPolygon);
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
  rec2 := InflateRect(recD, frameWidth/2, frameWidth/2);
  path2 := Image32_Vector.Ellipse(rec2);
  DrawShadow(imgClockface, path2, frEvenOdd, frameWidth/4, angle315);

  lgr := TLinearGradientRenderer.Create;
  try
    with recD do
      lgr.SetParameters(
        PointD(Right,Top), PointD(Left,Bottom),
        clNearWhite, clLightGray, gfsClamp);
    DrawLine(imgClockface, path, frameWidth, lgr, esPolygon);
    recD := inflateRect(recD, frameWidth/2, frameWidth/2);
    path2 := Ellipse(recD);
    DrawLine(imgClockface, path2, 1.0, clGray32, esPolygon);
    //inner ring
    recD := inflateRect(recD, -frameWidth, -frameWidth);
    path2 := Ellipse(recD);
    with recD do
      lgr.SetParameters(
        PointD(Left, Bottom), PointD(Right, Top),
        clNearWhite, clDarkGray, gfsClamp);
    DrawLine(imgClockface, path2, frameWidth/3, lgr, esPolygon);
  finally
    lgr.Free;
  end;

  //DRAW THE WATCH FACE

  recD := inflateRect(recD, -frameWidth/8, -frameWidth/8);
  path2 := Image32_Vector.Ellipse(recD);
  DrawLine(imgClockface, path2, 1.0, clSilver32, esPolygon);
  DrawPolygon(imgClockface, path2, frEvenOdd, clWhite32);


  tmpImg := TImage32.Create;
  tmpImg.LoadFromResource('THOMAS', RT_RCDATA);
  tmpImg.ScaleAlpha(0.75);
  tmpImg.Scale(recD.Width *0.5 / tmpImg.Width);
  x := recD.Left + (recD.Width - tmpImg.Width) / 2;
  y := recD.Top + (recD.Height - tmpImg.Height) / 2;
  rec2 := RectD(x, y, x+tmpImg.Width, y+tmpImg.Height);
  imgClockface.CopyBlend(tmpImg, tmpImg.Bounds, Rect(rec2), BlendToOpaque);
  tmpImg.Free;

  //draw the "maker's mark"
  recI := Image32_Vector.Rect(recD);
  recI.Bottom := recI.Top + clockRadius;
  fontReader := TFontReader.Create;
  glyphCache := TGlyphCache.Create(fontReader, clockRadius / 12);
  try
    fontReader.LoadFromResource('FONT_1', RT_RCDATA);
    glyphCache.GetTextGlyphs(recI, 'angusj',
      taCenter, tvaMiddle, paths, nextCharIdx, nextCharPt);
    //DrawPolygon(imgClockface, paths, frNonZero, clBlack32);

    glyphCache.FontHeight := glyphCache.FontHeight * 1.5;
    for i := 1 to 12 do
      glyphCache.GetTextGlyphs(0,0,inttostr(i), numGlyphs[i]);

  finally
    fontReader.Free;
    glyphCache.Free;
  end;

  //draw minute marks ...
  mp := MidPoint(recD);
  SetLength(path, 2);

  for i := 1 to 60 do
  begin
    if (i) mod 5 = 0 then
    begin
      j := i div 5;
      rec3 := GetBoundsD(numGlyphs[j]);
      mp3 := MidPoint(rec3);
      numGlyphs[j] := OffsetPath(numGlyphs[j],
        mp.X - mp3.X, mp.Y - mp3.y  - clockRadius * 0.78);
      numGlyphs[j] := RotatePath(numGlyphs[j], mp, j *PI/6);
      DrawPolygon(imgClockface, numGlyphs[j], frEvenOdd, clBlack32);
    end else if ((i-1) mod 5 <> 0) and ((i+1) mod 5 <> 0) then
    begin
      path[0] := PointD(mp.X, mp.Y - clockRadius * 0.80);
      path[1] := PointD(mp.X, mp.Y - clockRadius * 0.75);
      path := RotatePath(path, mp, i *PI/30);
      DrawLine(imgClockface, path, ScreenScale, clMidGray, esButt);
    end;
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
  tmpColor: TColor32;
const
  secsPerDay = 24*60*60;
begin
  //round to nearest second (avoids second hand being stuck between seconds)
  anow := Round(Frac(Now) * secsPerDay)/secsPerDay;

  //UPDATE CLOCK HANDS

  secHandLen   := clockRadius * 0.75;
  minHandLen   := clockRadius * 0.69;
  hrHandLen    := clockRadius * 0.49;
  handWidth    := clockRadius /15;
  shadOffset.X := handWidth/4;
  shadOffset.Y :=  handWidth/3;

  imgMain.Assign(imgClockface);
  tmpColor := MakeLighter(clDarkGreen, 20);

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
  DrawLine(imgMain, path, handWidth, clDarkGreen, esSquare);
  DrawLine(imgMain, path, handWidth/2, tmpColor, esSquare);

  //draw minute hand
  angle := Frac(anow * 24); //fraction of 1hr elapsed
  angle := pi/2 + angle*(pi*2);
  SinCos(angle, sinA, cosA);
  path[0] := PointD(mp.X + cosA * minHandLen/5, mp.Y + sinA * minHandLen/5);
  path[1] := PointD(mp.X - cosA * minHandLen, mp.Y - sinA * minHandLen);
  pathShad := OffsetPath(path, shadOffset.X, shadOffset.Y);
  DrawLine(imgMain, pathShad, handWidth, $30000000, esSquare);
  DrawLine(imgMain, path, handWidth, clDarkGreen, esSquare);
  DrawLine(imgMain, path, handWidth/2, tmpColor, esSquare);

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
  if Visible then
    TabControl1Change(nil);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormActivate(Sender: TObject);
begin
  TabControl1Change(nil);
end;
//------------------------------------------------------------------------------

procedure TMainForm.TabControl1Change(Sender: TObject);
begin
  if TabControl1.ActiveTab = TabItem1 then  DrawTextTab
  else if TabControl1.ActiveTab = TabItem2 then DrawClockTab
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
