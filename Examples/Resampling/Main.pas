unit Main;

interface

uses
  Windows, Types, SysUtils, Classes, Controls, Forms, Math, ComCtrls,
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
  private
    fontReader: TFontReader;
    fontCacheSmall: TFontCache;
    fontCacheBig: TFontCache;
    ImagePanel: TImage32Panel;
    procedure ShowGeneralResamplers;
    procedure ShowDownSampling;

  public
    times: array[0..3] of double;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
// Timer/StopWatch
//------------------------------------------------------------------------------

type
  TTimeRec = record
    freq      : TLargeInteger;
    startTime : TLargeInteger;
    endTime   : TLargeInteger;
  end;

procedure StartTimer(out timeRec: TTimeRec);
begin
  QueryPerformanceFrequency(timeRec.freq);
  QueryPerformanceCounter(timeRec.startTime);
  timeRec.endTime := timeRec.startTime;
end;

function EndTimer(timeRec: TTimeRec): double;
begin
  QueryPerformanceCounter(timeRec.endTime);
  with timeRec do
    Result := (endTime - startTime)/freq;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FontManager.Load('Arial', 800);
  fontReader := FontManager.GetFont('Arial');
  fontCacheSmall := TFontCache.Create(fontReader, 20);
  fontCacheBig := TFontCache.Create(fontReader, 250);

  ImagePanel := TImage32Panel.Create(self);
  ImagePanel.Parent := TabControl1;
  ImagePanel.Align := alClient;
  ActiveControl := ImagePanel;

  with ImagePanel.InnerClientRect do
    ImagePanel.Image.SetSize(Width, Height);
  TabControl1Change(nil);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fontCacheSmall.Free;
  fontCacheBig.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.TabControl1Change(Sender: TObject);
begin
  if TabControl1.TabIndex = 0 then
    ShowGeneralResamplers else
    ShowDownSampling;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowGeneralResamplers;
var
  d: integer;
  img: TImage32;
  rec, dstRect: TRect;
  tr: TTimeRec;
  mat: TMatrixD;
const
  len = 220;
  useMatix = false;//true;//
begin
  ImagePanel.Image.Clear;
  img := TImage32.Create;
  try
    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rNearestResampler;
    img.Resize(len,len);
    dstRect := Types.Rect(10,10, len + 10, len + 10);
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len + 10);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rBilinearResampler;
    img.Resize(len,len);
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len + 10);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rWeightedBilinear;
    img.Resize(len,len);
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len + 10);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rBicubicResampler;
    img.Resize(len,len);
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, len + 10, -dstRect.Top + 10);
    //-----------------

    rec := Types.Rect(10, 10, len, len);
    rec := GetRotatedRectBounds(rec, angle15);
    d := Round(len * len/rec.Width);
    mat := IdentityMatrix;
    MatrixScale(mat, d);
    MatrixRotate(mat, NullPointD, DegToRad(15));

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rNearestResampler; /////
    if useMatix then
      AffineTransformImage(img, mat, true)
    else
    begin
      img.Resize(d, d);
      img.Rotate(angle15);
    end;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len + 10);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rBilinearResampler; /////
    if useMatix then
      AffineTransformImage(img, mat, true)
    else
    begin
      img.Resize(d, d);
      img.Rotate(angle15);
    end;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len + 10);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rWeightedBilinear; /////
    if useMatix then
      AffineTransformImage(img, mat, true)
    else
    begin
      img.Resize(d, d);
      img.Rotate(angle15);
    end;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len + 10);

    img.SetSize(3,3);
    img.Clear(clBlue32);
    img.FillRect(Types.Rect(1,1,2,2), clRed32);
    img.Resampler := rBicubicResampler; /////
    if useMatix then
      AffineTransformImage(img, mat, true)
    else
    begin
      img.Resize(d, d);
      img.Rotate(angle15);
    end;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, len + 10, -dstRect.Top + 10);
    //-----------------

    img.LoadFromFile('beetle.png');
    img.Resampler := rNearestResampler;
    StartTimer(tr);
    img.Rotate(angle60);
    times[0] := EndTimer(tr);
    img.CropTransparentPixels;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len + 10);

    img.LoadFromFile('beetle.png');
    img.Resampler := rBilinearResampler;
    StartTimer(tr);
    img.Rotate(angle60);
    times[1] := EndTimer(tr);
    img.CropTransparentPixels;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len +10);

    img.LoadFromFile('beetle.png');
    img.Resampler := rWeightedBilinear;
    StartTimer(tr);
    img.Rotate(angle60);
    times[2] := EndTimer(tr);
    img.CropTransparentPixels;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, 0, len +10);

    img.LoadFromFile('beetle.png');
    img.Resampler := rBicubicResampler;
    StartTimer(tr);
    img.Rotate(angle60);
    times[3] := EndTimer(tr);
    img.CropTransparentPixels;
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);
    TranslateRect(dstRect, len + 10, -dstRect.Top + 10);

  finally
    img.Free;
  end;

  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 -10,
    'Resampler: rNearestResampler', fontCacheSmall);
  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 +20,
    format('Resampling time = %1.2n msecs', [times[0]*1e3]), fontCacheSmall);
  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 +50,
    'Note pixelation', fontCacheSmall);
  TranslateRect(dstRect, 0, len +10);

  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 -10,
    'Resampler: rBilinearResampler', fontCacheSmall);
  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 +20,
    format('Resampling time = %1.2n msecs', [times[1]*1e3]), fontCacheSmall);
  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 +50,
    'Note mild blurring', fontCacheSmall);
  TranslateRect(dstRect, 0, len +10);

  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 -10,
    'Resampler: rWeightedBilinear', fontCacheSmall);
  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 +20,
    format('Resampling time = %1.2n msecs', [times[2]*1e3]), fontCacheSmall);
  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 +50,
    'Very little blurring but slight pixelation', fontCacheSmall);
  TranslateRect(dstRect, 0, len +10);

  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 -10,
    'Resampler: rBiCubicResampler', fontCacheSmall);
  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 +20,
    format('Resampling time = %1.2n msecs', [times[3]*1e3]), fontCacheSmall);
  DrawText(ImagePanel.Image, dstRect.Left, dstRect.Top + len div 2 +50,
    'No blurring or pixelation but slower', fontCacheSmall);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowDownSampling;
var
  x,y: integer;
  img: TImage32;
  dstRect: TRect;
begin
  ImagePanel.Image.Clear;

  img := TImage32.Create;
  try
    img.LoadFromFile('TextOnPath.png');
    // display 2/3 sized image so it fits
    dstRect := Types.Rect(0,0,img.Width * 2 div 3,img.Height * 2 div 3);
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);

    img.Resampler := rBicubicResampler;
    img.Scale(0.1);
    dstRect := Types.Rect(820, 300, 820 + img.Width, 300 + img.Height);
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);

    img.LoadFromFile('TextOnPath.png');
    BoxDownSampling(img, 0.1, 0.1);
    TranslateRect(dstRect, 0, img.Height + 60);
    ImagePanel.Image.Copy(img, img.Bounds, dstRect);

    x := 1000;
    y := 300 + img.Height div 2;
    DrawText(ImagePanel.Image,
      x, y, 'rBicubicResampler', fontCacheSmall);
    DrawText(ImagePanel.Image,
      x, y + 30, 'Note pixelation', fontCacheSmall);
    DrawText(ImagePanel.Image,
      x, y + img.Height + 60, 'BoxDownSampling', fontCacheSmall);
    DrawText(ImagePanel.Image,
      x, y + img.Height + 90, 'Note mild blurring', fontCacheSmall);
  finally
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then Close;
end;

end.
