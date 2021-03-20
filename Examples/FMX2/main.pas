unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Platform, FMX.Surfaces, FMX.Controls.Presentation,
  FMX.Layouts, FMX.TabControl, Image32;

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
    handWidth: double;
    secHandLen, minHandLen, hrHandLen: double;

    clBackground32, clDarkRed32: TColor32;
    clDarkMaroon32, clNearWhite32, clMidGreen32: TColor32;
    clLightGray32, clDarkGray32: TColor32;

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
// MainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  rs: TResourceStream;
begin
  margin := DPIAwareFMX(12);

  clDarkRed32     := $FFCC0000;
  clDarkMaroon32  := $FF400000;
  clMidGreen32    := $FF00A000;
  clBackground32  := $FFF8F8F8;
  clNearWhite32   := $FFF4F4F4;
  clLightGray32   := $FFB0B0B0;
  clDarkGray32    := $FF999999;
{$IFDEF ANDROID}
  clDarkRed32 := SwapRedBlue(clDarkRed32);
  clDarkMaroon32 := SwapRedBlue(clDarkMaroon32);
{$ENDIF}

  imgMain      := TImage32.Create;
  imgClockface := TImage32.Create;
  margin := DPIAwareFMX(20);

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
  fontHeight := DPIAwareFMX(13);
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
  nextCharIdx, innerMargin, w, h: integer;
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
    DPIAwareFMX(ClientWidth) - margin*2,
    Round(DPIAwareFMX(ClientHeight - TabControl1.TabHeight)) -margin*2);

  imgMain.SetSize(outerRec.Width, outerRec.Height);
  //color fill the base image and give it a border ...
  imgMain.Clear(clWhite32);

  useClearType := fontHeight < 14;
  innermargin := margin div 2;

  notoSansReg := TFontReader.CreateFromResource('FONT_NSR', RT_RCDATA);
  noto14Cache := TGlyphCache.Create(notoSansReg, fontHeight);
  try
    if not notoSansReg.IsValidFontFormat then Exit;
    //noto14Cache.Underlined := true;

    tmpRec := Image32_Vector.InflateRect(outerRec, -1, -1);
    rectangle := Image32_Vector.Rectangle(tmpRec);
    DrawLine(imgMain, rectangle, 2, clDarkRed32, esPolygon);

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

    txtPaths := noto14Cache.GetTextGlyphs(tmpRec, essay,
      taJustify, tvaTop, nextCharIdx, nextCharPt);
    if useClearType then
      DrawPolygon_ClearType(imgMain, txtPaths, frNonZero, clBlack32) else
      DrawPolygon(imgMain, txtPaths, frNonZero, clBlack32);

    //now draw the remaining text under the image
    if (nextCharIdx <> 0) then
    begin
      tmpStr := Copy(essay, nextCharIdx, Length(essay));
      if (tmpStr[1] <= #32) then Delete(tmpStr,1,1); //ie skip a CR or a space

      innerRec.Top := Round(nextCharPt.Y);
      txtPaths := noto14Cache.GetTextGlyphs(innerRec, tmpStr,
        taJustify, tvaTop, nextCharIdx, nextCharPt);

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
  i,j: integer;
  x,y: double;
  frameWidth, lineWidth6: double;
  path, path2, path3: TPathD;
  recD, rec2, rec3: TRectD;
  pt, mp, mp3: TPointD;

  recI       : TRect;
  ir         : TImageRenderer;
  lgr        : TLinearGradientRenderer;
  fontReader : TFontReader;
  glyphCache : TGlyphCache;
  numGlyphs  : array[1..12] of TPathsD;
begin
  imgClockface.SetSize(
    DPIAwareFMX(ClientWidth) - margin * 2,
    Round(DPIAwareFMX(ClientHeight - TabControl1.TabHeight)) - margin * 2);
  imgClockface.Clear(clBackground32);

  //DRAW WATCH BAND
  //use an image renderer to tile fill the watch band (polygon)
  ir := TImageRenderer.Create;
  try
    ir.SetTileFillStyle(tfsRotate180); //or tfsMirrorHorz or tfsRepeat
    ir.Image.LoadFromResource('KOOKABURRA' , RT_RCDATA); // see image.res
    ir.Image.Resize(DPIAwareFMX(16), DPIAwareFMX(16));

    ir.Image.AdjustHue(10);
    ir.Image.AdjustSaturation(20);


    lineWidth6 := DPIAwareFMX(6);
    if imgClockface.Width > imgClockface.Height then // landscape
    begin
      recD := RectD(0, imgClockface.Height /4,
        imgClockface.Width, imgClockface.Height * 0.75);
      path := Rectangle(recD);
      DrawPolygon(imgClockface, path, frEvenOdd, ir);
      path2 := Copy(path, 0,2);
      path3 := Copy(path, 2,2);
      DrawLine(imgClockface, path2, lineWidth6, clDarkMaroon32, esPolygon);
      DrawLine(imgClockface, path3, lineWidth6, clDarkMaroon32, esPolygon);
    end else
    begin                                            // portrait
      recD := RectD(imgClockface.Width /4, 0,
        imgClockface.Width * 0.75, imgClockface.Height);
      path := Rectangle(recD);
      DrawPolygon(imgClockface, path, frEvenOdd, ir);
      path2 := Copy(path, 1,2);
      SetLength(path3, 2); path3[0] := path[3]; path3[1] := path[0];
      DrawLine(imgClockface, path2, lineWidth6, clDarkMaroon32, esPolygon);
      DrawLine(imgClockface, path3, lineWidth6, clDarkMaroon32, esPolygon);
    end;
  finally
    ir.Free;
  end;

  /////////////////////////////////////////////////////////////
  //DRAW THE WATCH FRAME
  /////////////////////////////////////////////////////////////

  clockRadius := min(imgClockface.Width, imgClockface.Height) *2 div 5;
  x := imgClockface.Width / 2; y := imgClockface.Height / 2;
  recD := RectD(x-clockRadius, y-clockRadius, x+clockRadius, y+clockRadius);
  frameWidth := clockRadius /6;

  handWidth := clockRadius /14;
  secHandLen   := clockRadius * 0.70;
  minHandLen   := clockRadius * 0.69;
  hrHandLen    := clockRadius * 0.52;

  //outer ring (with linear gradients to give the impression of 3D light)

  path := Image32_Vector.Ellipse(recD);
  rec2 := InflateRect(recD, frameWidth/2, frameWidth/2);
  path2 := Image32_Vector.Ellipse(rec2);
  DrawShadow(imgClockface, path2, frEvenOdd, frameWidth/4, angle315);

  lgr := TLinearGradientRenderer.Create;
  try
    with recD do
      lgr.SetParameters(
        PointD(Right,Top), PointD(Left,Bottom),
        clNearWhite32, clLightGray32, gfsClamp);
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
        clNearWhite32, clDarkGray32, gfsClamp);
    DrawLine(imgClockface, path2, frameWidth/3, lgr, esPolygon);
  finally
    lgr.Free;
  end;

  /////////////////////////////////////////////////////////////
  //DRAW THE WATCH FACE
  /////////////////////////////////////////////////////////////

  recD := inflateRect(recD, -frameWidth/8, -frameWidth/8);
  path2 := Image32_Vector.Ellipse(recD);
  DrawLine(imgClockface, path2, 1.0, clSilver32, esPolygon);
  DrawPolygon(imgClockface, path2, frEvenOdd, clWhite32);
  mp := MidPoint(recD);

  //get the Noto Sans Regular font from the registry
  fontReader := TFontReader.CreateFromResource('FONT_NSR', RT_RCDATA);
  //create a glyph cache for the font with font height = clockRadius / 13
  glyphCache := TGlyphCache.Create(fontReader, clockRadius / 13);
  try
    //DRAW THE "MAKER'S MARK"
    recI := Image32_Vector.Rect(recD);
    recI.Bottom := recI.Top + clockRadius;
    DrawText(imgClockface, recI, 'angusj', taCenter, tvaMiddle, glyphCache);

    fontReader.LoadFromResource('FONT_NSB', RT_RCDATA); //noto sans bold
    glyphCache.FontHeight := glyphCache.FontHeight * 1.5;

    //GET CLOCK NUMBERS (AGAIN MAKING THE FONT LARGER)
    glyphCache.FontHeight := glyphCache.FontHeight * 1.4;
    for i := 1 to 12 do
      numGlyphs[i] := glyphCache.GetTextGlyphs(0,0,inttostr(i));

  finally
    fontReader.Free;
    glyphCache.Free;
  end;

  pt.X := mp.X;
  pt.Y := mp.Y - clockRadius * 0.78;
  SetLength(path, 2);
  for i := 1 to 60 do
  begin
    if (i) mod 5 = 0 then
    begin
      //DRAW NUMBERS 1 .. 12 ...
      j := i div 5;
      rec3 := GetBoundsD(numGlyphs[j]);
      mp3 := MidPoint(rec3);

      RotatePoint(pt, mp, PI/6);
      numGlyphs[j] := OffsetPath(numGlyphs[j], pt.X -mp3.X, pt.Y -mp3.y);
      DrawPolygon(imgClockface, numGlyphs[j], frEvenOdd, clBlack32);
    end else if ((i-1) mod 5 <> 0) and ((i+1) mod 5 <> 0) then
    begin
      //DRAW MINUTE MARKS ...
      path[0] := PointD(mp.X, mp.Y - clockRadius * 0.80);
      path[1] := PointD(mp.X, mp.Y - clockRadius * 0.75);
      path := RotatePath(path, mp, i *PI/30);
      DrawLine(imgClockface, path, clockRadius/150, clDarkGray32, esButt);
    end;
  end;
  imgMain.Assign(imgClockface);
  Timer1Timer(nil);

  Timer1.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Timer1Timer(Sender: TObject);

  function MakeHandPath(const origin: TPointD;
    handLength, handWidth, arrowOffset: double): TPathD;
  const
    widthDelta = 1.5; //to enlarge the tip of eacb hand a little
  begin
    handWidth := handWidth /2;
    setLength(Result, 5);
    //base
    Result[0].X := origin.X - handWidth;
    Result[0].Y := origin.Y;
    Result[1].X := origin.X + handWidth;
    Result[1].Y := origin.Y;
    //tip
    handWidth := handWidth *widthDelta;
    Result[2].X := origin.X  + handWidth;
    Result[2].Y := origin.Y - handLength *arrowOffset;
    Result[3].X := origin.X;
    Result[3].Y := origin.Y - handLength;
    Result[4].X := origin.X  - handWidth;
    Result[4].Y := origin.Y - handLength *arrowOffset;
  end;

  procedure DrawShadow(path: TPathD; handwidth: double);
  begin
    path := OffsetPath(path, handWidth/4, handWidth/3);
    DrawPolygon(imgMain, path, frNonZero, $30000000);
  end;

  procedure DrawHourMinHand(const origin: TPointD;
    handLength, handWidth, angle: double);
  var
    path1: TPathD;
  begin
    path1 := MakeHandPath(origin, handLength, handWidth, 0.95);
    path1 := RotatePath(path1, origin, angle);
    DrawShadow(path1, handWidth);
    DrawPolygon(imgMain, path1, frNonZero, clMidGreen32);
    DrawLine(imgMain, path1, handWidth/8, clBlack32, esClosed);
  end;

  procedure DrawSecHand(const origin: TPointD;
    handLength, handWidth, angle: double);
  var
    path1, path2: TPathD;
    rec: TRectD;
    pt: TPointD;
  begin
    pt.X := origin.X;
    pt.Y := origin.Y + handLength/5;
    path1 := MakeHandPath(pt, handLength*1.2, handWidth, 0.97);
    path1 := RotatePath(path1, origin, angle);
    handWidth := handWidth *3;
    rec := RectD(origin.X - handWidth, origin.Y - handWidth,
      origin.X + handWidth, origin.Y + handWidth);
    path2 := Ellipse(rec);
    DrawShadow(path1, handWidth);
    DrawLine(imgMain, path2, handWidth/5, clBlack32, esClosed);
    DrawPolygon(imgMain, path1, frNonZero, clDarkRed32);
    DrawPolygon(imgMain, path2, frNonZero, clDarkRed32);
  end;

var
  t: double;
  mp: TPointD;
  angle, tnow: double;
const
  secsPerDay = 24*60*60;
begin
  t := time * secsPerDay;
  //round 't' so that 'tnow' isn't caught between secs.
  tnow := Round(t)/secsPerDay;
  Timer1.Interval := Round(1000*(1 - abs(t - Round(t))));

  //UPDATE CLOCK HANDS

  imgMain.Assign(imgClockface);
  mp := PointD(imgMain.Width/2, imgMain.Height/2);

  //draw hour hand
  angle := Frac(tnow *2) *PI*2;//fraction of 12hrs elapsed
  DrawHourMinHand(mp, hrHandLen, handWidth, angle);

  //draw minute hand
  angle := Frac(tnow *24) *PI*2; //fraction of 1hr elapsed
  DrawHourMinHand(mp, minHandLen, handWidth, angle);

  //draw second hand
  angle := Frac(tnow * 24 *60) *PI*2;
  DrawSecHand(mp, secHandLen, handWidth/3, angle);

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
    CopyImage32ToFmxBitmap(imgMain,tmpBitmap);
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
