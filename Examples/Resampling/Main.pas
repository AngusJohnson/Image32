unit Main;

interface

uses
  Windows, Types, SysUtils, Classes, Controls, Forms,
  Math, ComCtrls, ShellApi,
  Img32, Img32.Panels, Img32.Resamplers, Img32.Vector, Img32.Extra,
  Img32.Fmt.BMP, Img32.Fmt.PNG, Img32.Draw, Img32.Text, Img32.Transform;

type
  TMainForm = class(TForm)
    TabControl1: TTabControl;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TabControl1Change(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    boxDownsamplingLinkRect: TRect;
    fontReader: TFontReader;
    fontCache12: TFontCache;
    fontCache16: TFontCache;
    fontCache125: TFontCache;
    ImagePanel: TImage32Panel;
    procedure ImagePanelMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImagePanelMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowGeneralResamplers1;
    procedure ShowGeneralResamplers2;
    procedure ShowGeneralResamplers3;
    procedure ShowGeneralResamplers4;
    procedure ShowDownSampling;
  public
  end;

var
  MainForm: TMainForm;

implementation

//------------------------------------------------------------------------------
// Timer/StopWatch
//------------------------------------------------------------------------------

type
  TTimeRec = record
    started         : Boolean;
    freq            : TLargeInteger;
    startTime       : TLargeInteger;
    cumulativeTime  : double;
    endTime         : TLargeInteger;
  end;

procedure ResetTimer(out timeRec: TTimeRec;
  startNow: Boolean = true);
begin
  with timeRec do
  begin
    QueryPerformanceFrequency(freq);
    QueryPerformanceCounter(startTime);
    started := startNow;
    cumulativeTime := 0;
    if started then
      QueryPerformanceCounter(startTime);
  end;
end;

procedure PauseTimer(out timeRec: TTimeRec);
begin
  with timeRec do
  begin
    if not started then Exit;
    QueryPerformanceCounter(endTime);
    cumulativeTime := cumulativeTime + (endTime - startTime)/freq;
    started := false;
  end;
end;

procedure ResumeTimer(out timeRec: TTimeRec);
begin
  with timeRec do
  begin
    if started then Exit;
    started := true;
    QueryPerformanceCounter(startTime);
  end;
end;

function StopTimer(timeRec: TTimeRec): double;
begin
  PauseTimer(timeRec);
  with timeRec do
  begin
    QueryPerformanceCounter(endTime);
    Result := cumulativeTime;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$R *.dfm}
{$R images.res}

const
  boxDownSamplingUrl = 'https://angusj.com/image32/Docs/Units/'+
    'Img32.Resamplers/Routines/BoxDownSampling.htm';
  boxDownSamplingText = 'See - BoxDownSampling';

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fontReader := FontManager.LoadFontReader('Arial');
  fontCache12 := TFontCache.Create(fontReader, DPIAware(12));
  fontCache16 := TFontCache.Create(fontReader, DPIAware(16));
  fontCache125 := TFontCache.Create(fontReader, DPIAware(125));

  ImagePanel := TImage32Panel.Create(self);
  ImagePanel.Parent := TabControl1;
  ImagePanel.Align := alClient;
  ImagePanel.OnMouseDown := ImagePanelMouseDown;
  ImagePanel.OnMouseMove := ImagePanelMouseMove;
  ActiveControl := ImagePanel;

  with ImagePanel.InnerClientRect do
    ImagePanel.Image.SetSize(Width, Height);
  TabControl1Change(nil);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fontCache12.Free;
  fontCache16.Free;
  fontCache125.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.TabControl1Change(Sender: TObject);
begin
  ImagePanel.Scale := 1.0;
  case TabControl1.TabIndex of
    0: ShowGeneralResamplers1;
    1: ShowGeneralResamplers2;
    2: ShowGeneralResamplers3;
    3: ShowGeneralResamplers4;
    4: ShowDownSampling;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F5) and (TabControl1.TabIndex = 1) then // F5 => refresh
    ShowGeneralResamplers2;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowGeneralResamplers1;
var
  displaySize     : integer;
  preRotatedSize  : integer;
  margin, dpi8    : integer;
  topOffset       : integer;
  angle           : double;
  img             : TImage32;
  rec, displayRect: TRect;
begin
  dpi8 := DPIAware(8);

  margin := DPIAware(10);
  displaySize := DPIAware(110);
  topOffset := DPIAware(60);
  angle := DegToRad(-15);
  displayRect := Types.Rect(margin, margin + topOffset,
    margin + displaySize, margin + displaySize + topOffset);

  rec := GetRotatedRectBounds(displayRect, angle);
  preRotatedSize := Round(displaySize * displaySize/rec.Width);

  ImagePanel.Image.Clear;

  DrawText(ImagePanel.Image, margin, margin + DpiAware(13),
    'Scale 3x3 image then scale and rotate 3x3 image', fontCache16);

  DrawText(ImagePanel.Image, margin,
    topOffset - dpi8, 'rNearestResampler', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + displaySize*2,
    topOffset - dpi8, 'rBilinearResampler', fontCache12);
  DrawText(ImagePanel.Image, margin,
    topOffset - dpi8 + displaySize + margin*6,
    'rWeightedBilinear', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + displaySize*2,
    topOffset - dpi8 + displaySize + margin*6,
    'rBicubicResampler', fontCache12);

  img := TImage32.Create;
  try

    // rNearestResampler

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rNearestResampler;
    img.Resize(displaySize,displaySize);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin*3, 0);

    // rBilinearResampler

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rBilinearResampler;
    img.Resize(displaySize,displaySize);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect,
      -displayRect.Left + margin, displaySize + margin*6);

    // rWeightedBilinear

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rWeightedBilinear;
    img.Resize(displaySize,displaySize);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin*3, 0);

    // rBicubicResampler

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rBicubicResampler;
    img.Resize(displaySize,displaySize);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    //img.SaveToFile('c:\temp\test.png');
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);

  finally
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowGeneralResamplers2;
var
  imgDisplaySize    : integer;
  dpi5, dpi8, dpi18 : integer;
  scale             : double;
  i, margin         : integer;
  topOffset         : integer;
  angle             : double;
  img               : TImage32;
  rec, displayRect  : TRect;
  tr                : TTimeRec;
  mat               : TMatrixD;
  times             : array[0..3] of double;
const
  loopCnt     = 20;
  // when performing multiple transformations then using a matrix
  // that combines these into a single transformation will be faster
  useMatrix    = false;//true;//
begin
  dpi5 := DPIAware(5);
  dpi8 := DPIAware(8);
  dpi18 := DPIAware(18);
  margin := DPIAware(10);
  imgDisplaySize := DPIAware(180);
  topOffset := DPIAware(60);
  displayRect := Types.Rect(margin, topOffset -margin,
    margin + imgDisplaySize, topOffset -margin + imgDisplaySize);
  angle := DegToRad(60);

  ImagePanel.Image.Clear;

  DrawText(ImagePanel.Image, margin, margin + DpiAware(13),
    'Scale and rotate a small bitmap image', fontCache16);

  Screen.Cursor := crHourGlass;
  img := TImage32.Create;
  try
    img.LoadFromResource('BEETLE', 'PNG');
    rec := GetRotatedRectBounds(img.Bounds, angle);
    scale := (imgDisplaySize/rec.Width);
    mat := IdentityMatrix;
    MatrixScale(mat, scale);
    MatrixRotate(mat, NullPointD, angle);

    img.Resampler := rNearestResampler;
    ResetTimer(tr, false);
    for i := 1 to loopCnt do
    begin
      img.LoadFromResource('BEETLE', 'PNG');
      ResumeTimer(tr);
      if useMatrix then
        AffineTransformImage(img, mat)
      else begin
        img.Scale(scale);
        img.Rotate(angle);
      end;
      PauseTimer(tr);
    end;
    times[0] := StopTimer(tr) / loopCnt;

    displayRect.Right := displayRect.Left + img.Width;
    displayRect.Bottom := displayRect.Top + img.Height;
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, imgDisplaySize + margin*3, 0);

    img.Resampler := rBilinearResampler;
    ResetTimer(tr, false);
    for i := 1 to loopCnt do
    begin
      img.LoadFromResource('BEETLE', 'PNG');
      ResumeTimer(tr);
      if useMatrix then
        AffineTransformImage(img, mat)
      else begin
        img.Scale(scale);
        img.Rotate(angle);
      end;
      PauseTimer(tr);
    end;
    times[1] := StopTimer(tr) / loopCnt;
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect,
      -displayRect.Left + margin, imgDisplaySize);

    img.Resampler := rWeightedBilinear;
    ResetTimer(tr, false);
    for i := 1 to loopCnt do
    begin
      img.LoadFromResource('BEETLE', 'PNG');
      ResumeTimer(tr);
      if useMatrix then
        AffineTransformImage(img, mat)
      else begin
        img.Scale(scale);
        img.Rotate(angle);
      end;
      PauseTimer(tr);
    end;
    times[2] := StopTimer(tr) / loopCnt;
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, imgDisplaySize + margin*5, 0);

    img.Resampler := rBicubicResampler;
    ResetTimer(tr, false);
    for i := 1 to loopCnt do
    begin
      img.LoadFromResource('BEETLE', 'PNG');
      ResumeTimer(tr);
      if useMatrix then
        AffineTransformImage(img, mat)
      else begin
        img.Scale(scale);
        img.Rotate(angle);
      end;
      PauseTimer(tr);
    end;
    times[3] := StopTimer(tr) / loopCnt;
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, imgDisplaySize + margin*5, 0);
    //img.SaveToFile('c:\temp\resampling_bc.png');

  finally
    img.Free;
    Screen.Cursor := crDefault;
  end;

  DrawText(ImagePanel.Image, margin,
    topOffset - dpi8, 'rNearestResampler', fontCache12);
  DrawText(ImagePanel.Image, margin,
    topOffset + dpi5, 'Fast, but also pixelated', fontCache12);
  DrawText(ImagePanel.Image, margin,
    topOffset + dpi18,
    Format('%1.2n msec',[times[0]*1e3]), fontCache12);

  DrawText(ImagePanel.Image, margin*5 + imgDisplaySize,
    topOffset - dpi8, 'rBilinearResampler', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + imgDisplaySize,
    topOffset + dpi5, 'Note blurring', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + imgDisplaySize,
    topOffset + dpi18,
    Format('%1.2n msec',[times[1]*1e3]), fontCache12);

  DrawText(ImagePanel.Image, margin,
    topOffset - dpi8 + imgDisplaySize,
    'rWeightedBilinear', fontCache12);
  DrawText(ImagePanel.Image, margin,
    topOffset + dpi5 + imgDisplaySize,
    'Very mildly pixelated and blurred', fontCache12);
  DrawText(ImagePanel.Image, margin,
    topOffset + dpi18 + imgDisplaySize,
    Format('%1.2n msec',[times[2]*1e3]), fontCache12);

  DrawText(ImagePanel.Image, margin*5 + imgDisplaySize,
    topOffset - dpi8 + imgDisplaySize,
    'rBicubicResampler', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + imgDisplaySize,
    topOffset + dpi5 + imgDisplaySize,
    'Slow, but no pixelation or blurring', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + imgDisplaySize,
    topOffset + dpi18 + imgDisplaySize,
    Format('%1.2n msec',[times[3]*1e3]), fontCache12);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowGeneralResamplers3;
var
  i, displaySize  : integer;
  preRotatedSize  : integer;
  margin, dpi8    : integer;
  topOffset       : integer;
  angle           : double;
  img             : TImage32;
  rec, displayRect: TRect;
  mat             : TMatrixD;
const
  useMatrix    = false;//true;//
begin
  dpi8 := DPIAware(8);

  margin := DPIAware(10);
  displaySize := DPIAware(110);
  topOffset := DPIAware(60);
  angle := DegToRad(-15);
  displayRect := Types.Rect(margin, margin + topOffset,
    margin + displaySize, margin + topOffset + displaySize);

  rec := GetRotatedRectBounds(displayRect, angle);
  preRotatedSize := Round(displaySize * displaySize/rec.Width);
  mat := IdentityMatrix;
  // initial image will be 100 wide and 1 high
  MatrixScale(mat, preRotatedSize/100, preRotatedSize);
  MatrixRotate(mat, NullPointD, angle);

  ImagePanel.Image.Clear;

  DrawText(ImagePanel.Image, margin, margin + DpiAware(13),
    'Scale 100x1 image, then scale and rotate the same image',
    fontCache16);

  DrawText(ImagePanel.Image, margin,
    topOffset - dpi8, 'rNearestResampler', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + displaySize*2,
    topOffset - dpi8, 'rBilinearResampler', fontCache12);
  DrawText(ImagePanel.Image, margin,
    topOffset - dpi8 + displaySize + margin*6,
    'rWeightedBilinear', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + displaySize*2,
    topOffset - dpi8 + displaySize + margin*6,
    'rBicubicResampler', fontCache12);

  img := TImage32.Create;
  try

    // rNearestResampler

    img.SetSize(100,1);
    for i := 0 to 99 do
      img.Pixels[i] := RainbowColor(i/100);
    img.Resampler := rNearestResampler;
    img.Resize(displaySize,displaySize);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(100,1);
    for i := 0 to 99 do
      img.Pixels[i] := RainbowColor(i/100);
    if useMatrix then
      AffineTransformImage(img, mat, true)
    else
    begin
      img.Resize(preRotatedSize, preRotatedSize);
      img.Rotate(angle);
    end;
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin*3, 0);

    // rBilinearResampler

    img.SetSize(100,1);
    for i := 0 to 99 do
      img.Pixels[i] := RainbowColor(i/100);
    img.Resampler := rBilinearResampler;
    img.Resize(displaySize,displaySize);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(100,1);
    for i := 0 to 99 do
      img.Pixels[i] := RainbowColor(i/100);
    if useMatrix then
      AffineTransformImage(img, mat, true)
    else
    begin
      img.Resize(preRotatedSize, preRotatedSize);
      img.Rotate(angle);
    end;
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect,
      -displayRect.Left + margin, displaySize + margin*6);

    // rWeightedBilinear

    img.SetSize(100,1);
    for i := 0 to 99 do
      img.Pixels[i] := RainbowColor(i/100);
    img.Resampler := rWeightedBilinear;
    img.Resize(displaySize,displaySize);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(100,1);
    for i := 0 to 99 do
      img.Pixels[i] := RainbowColor(i/100);
    if useMatrix then
      AffineTransformImage(img, mat, true)
    else
    begin
      img.Resize(preRotatedSize, preRotatedSize);
      img.Rotate(angle);
    end;
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin*3, 0);

    // rBicubicResampler

    img.SetSize(100,1);
    for i := 0 to 99 do
      img.Pixels[i] := RainbowColor(i/100);
    img.Resampler := rBicubicResampler;
    img.Resize(displaySize,displaySize);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(100,1);
    for i := 0 to 99 do
      img.Pixels[i] := RainbowColor(i/100);
    if useMatrix then
      AffineTransformImage(img, mat, true)
    else
    begin
      img.Resize(preRotatedSize, preRotatedSize);
      img.Rotate(angle);
    end;
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);

  finally
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowGeneralResamplers4;
var
  displaySize     : integer;
  preRotatedSize  : integer;
  margin, dpi8    : integer;
  topOffset       : integer;
  angle           : double;
  img             : TImage32;
  rec, displayRect: TRect;
begin
  dpi8 := DPIAware(8);

  margin := DPIAware(10);
  displaySize := DPIAware(110);
  topOffset := DPIAware(60);
  angle := DegToRad(-15);
  displayRect := Types.Rect(margin, margin + topOffset,
    margin + displaySize, margin + displaySize + topOffset);

  rec := GetRotatedRectBounds(displayRect, angle);
  preRotatedSize := Round(displaySize * displaySize/rec.Width);

  ImagePanel.Image.Clear;

  DrawText(ImagePanel.Image, margin, margin + DpiAware(13),
    'Scale and rotate 1x1 image then scale and rotate 2x2 image', fontCache16);

  DrawText(ImagePanel.Image, margin,
    topOffset - dpi8, 'rNearestResampler', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + displaySize*2,
    topOffset - dpi8, 'rBilinearResampler', fontCache12);
  DrawText(ImagePanel.Image, margin,
    topOffset - dpi8 + displaySize + margin*6,
    'rWeightedBilinear', fontCache12);
  DrawText(ImagePanel.Image, margin*5 + displaySize*2,
    topOffset - dpi8 + displaySize + margin*6,
    'rBicubicResampler', fontCache12);

  img := TImage32.Create;
  try

    // rNearestResampler

    img.SetSize(1,1);
    img.Pixels[0] := clBlue32;
    img.Resampler := rNearestResampler;
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(2,2);
    img.Pixels[0] := clLime32;
    img.Pixels[1] := clAqua32;
    img.Pixels[2] := clYellow32;
    img.Pixels[3] := clFuchsia32;
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin*3, 0);

    // rBilinearResampler

    img.SetSize(1,1);
    img.Pixels[0] := clBlue32;
    img.Resampler := rBilinearResampler;
    img.Resize(displaySize,displaySize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(2,2);
    img.Pixels[0] := clLime32;
    img.Pixels[1] := clAqua32;
    img.Pixels[2] := clYellow32;
    img.Pixels[3] := clFuchsia32;
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect,
      -displayRect.Left + margin, displaySize + margin*6);

    // rWeightedBilinear

    img.SetSize(1,1);
    img.Pixels[0] := clBlue32;
    img.Resampler := rWeightedBilinear;
    img.Resize(displaySize,displaySize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(2,2);
    img.Pixels[0] := clLime32;
    img.Pixels[1] := clAqua32;
    img.Pixels[2] := clYellow32;
    img.Pixels[3] := clFuchsia32;
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin*3, 0);

    // rBicubicResampler

    img.SetSize(1,1);
    img.Pixels[0] := clBlue32;
    img.Resampler := rBicubicResampler;
    img.Resize(displaySize,displaySize);
    img.Rotate(angle);
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);
    TranslateRect(displayRect, displaySize + margin, 0);

    img.SetSize(2,2);
    img.Pixels[0] := clLime32;
    img.Pixels[1] := clAqua32;
    img.Pixels[2] := clYellow32;
    img.Pixels[3] := clFuchsia32;
    img.Resize(preRotatedSize, preRotatedSize);
    img.Rotate(angle);
    //img.SaveToFile('c:\temp\test.png');
    ImagePanel.Image.Copy(img, img.Bounds, displayRect);

  finally
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowDownSampling;
var
  margin    : integer;
  lineHt    : integer;
  img       : TImage32;
  dstRect   : TRect;
  pt        : TPoint;
begin
  ImagePanel.Image.Clear;

  margin := DPIAware(15);
  lineHt := DPIAware(16);
  img := TImage32.Create;
  try
    img.LoadFromResource('TEXTONPATH', 'PNG');
    img.CropTransparentPixels;
    with ImagePanel.InnerClientRect do
      ImagePanel.Image.SetSize(Width, Height);
    dstRect := ImagePanel.Image.Bounds;
    Types.InflateRect(dstRect, -margin, -margin);
    img.ScaleToFit(dstRect.Width * 3 div 4, dstRect.Height * 3 div 4);
    dstRect.Right := dstRect.Left + img.Width;
    dstRect.Bottom := dstRect.Top + img.Height;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);

    img.LoadFromResource('TEXTONPATH', 'PNG');
    // the image's specified resampler is usually ignored when downsampling
    // UNLESS the compiler conditional USE_DOWNSAMPLER_AUTOMATICALLY has
    // been disabled. (See BoxDownsampling for more info.)
    img.Resampler := rBicubicResampler;
    img.Scale(0.2);
    dstRect.Top := dstRect.Bottom - img.Height;
    dstRect.Left := img.Width + margin;
    dstRect.Right := dstRect.Left + img.Width;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);


    pt.X := dstRect.Right + margin;
    pt.Y := dstRect.Bottom - lineHt *2;

    boxDownsamplingLinkRect.Left := pt.X;
    boxDownsamplingLinkRect.Bottom := pt.Y;
    boxDownsamplingLinkRect.Top := pt.Y - lineHt;
    boxDownsamplingLinkRect.Right := boxDownsamplingLinkRect.Left +
      Ceil(fontCache12.GetTextWidth(boxDownSamplingText));
    DrawText(ImagePanel.Image,
      pt.X, pt.Y, boxDownSamplingText, fontCache12, clBlue32);
  finally
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ImagePanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if (TabControl1.TabIndex <> 4) then Exit;
  pt := ImagePanel.ClientToImage(Point(X,Y));
  if PtInRect(boxDownsamplingLinkRect, pt) then
    ShellExecute(0, 'open', PChar(boxDownSamplingUrl), nil, nil, SW_SHOWNORMAL);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ImagePanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  pt: TPoint;
begin
  if (TabControl1.TabIndex <> 4) then Exit;
  pt := ImagePanel.ClientToImage(Point(X,Y));
  if PtInRect(boxDownsamplingLinkRect, pt) then
    ImagePanel.Cursor := crHandPoint else
    ImagePanel.Cursor := crDefault;
end;
//------------------------------------------------------------------------------

end.
