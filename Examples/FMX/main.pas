unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Rtti, Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls, FMX.Platform, FMX.Surfaces,
  FMX.StdCtrls, FMX.Controls.Presentation,
  Image32;


type
  TMainForm = class(TForm)
    ImageViewer1: TImageViewer;
    Timer1: TTimer;
    btnRefresh: TButton;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure ImageViewer1Resized(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
  private
    zoomIdx: cardinal;
    zoomImages: array[0..20] of TImage32;
    outlineText: TPathsD;
    procedure UpdateImageViewer;
  public
  end;

var
  MainForm: TMainForm;
  bkColor, penColor, txtColor: TColor32;

const
  margin: integer = 20;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}

{$R font.res}

uses
  Image32_Vector, Image32_Draw, Image32_Extra, Image32_FMX,
    Image32_Ttf, Image32_Clipper;

//------------------------------------------------------------------------------
// TMainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  i, zoomCnt: integer;
  baseImg, imgBooks: TImage32;
  bigTextGlyphs, copyrightGlyphs: TPathsD;
  tmpPath: TPathD;
  textRec, textRec2, imgRec, tmpRec: TRect;
  matrix: TMatrixD;
  delta: TPoint;
  screenScale, fontHeight, nextX: double;

  fontCache: TGlyphCache;
  fontReader : TFontReader;
  ScreenService: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService (
    IFMXScreenService, IInterface(ScreenService)) then
      screenScale := ScreenService.GetScreenScale else
      screenScale := 1.0;
  fontHeight := 35 * screenScale;

  bkColor  := $FFF8F8BB; //yellow
  penColor := clMaroon32;
  txtColor := $FF00BB00; //green
{$IFNDEF MSWINDOWS}
  bkColor  := SwapRedBlue(bkColor);
  penColor := SwapRedBlue(penColor);
  txtColor := SwapRedBlue(txtColor);
{$ENDIF}

  baseImg := TImage32.Create;

  //create a fontReader to access truetype font files (*.ttf) that
  //have been stored as font resources and create a glyph cache too
  fontReader := TFontReader.Create;
  fontCache := TGlyphCache.Create(fontReader, fontHeight);
  try
    //connect fontReader to a fancy ttf font stored as a resource
    fontReader.LoadFromResource('FONT_2', RT_RCDATA);
    if not fontReader.IsValidFontFormat then Exit;
    //and get the outline for some text ...
    fontCache.GetTextGlyphs(0,0,'Image32  rocks!', bigTextGlyphs, nextX);

    //Now that we've gotten 'bigTextGlyphs' we need
    //a different font for the copyright text
    fontReader.LoadFromResource('FONT_1', RT_RCDATA);
    if not fontReader.IsValidFontFormat then Exit;
    fontCache.FontHeight := fontHeight / 4;

    fontCache.GetTextGlyphs(0,0,#$00A9' 2020 Angus Johnson',
      copyrightGlyphs, nextX);
  finally
    fontCache.Free;
    fontReader.free;
  end;

  //also load a colourful background image (some books) ...
  imgBooks := TImage32.Create;
  try
    imgBooks.LoadFromResource('PNGIMAGE_1', RT_RCDATA);

    //size the base image
    baseImg.SetSize(
      Image32_Vector.GetBounds(bigTextGlyphs).Width + margin *2,
      imgBooks.Height);

    //offset 'imgBooks' so it'll be centered in the base image
    imgRec := imgBooks.Bounds;
    delta.X := (baseImg.Width - imgBooks.Width)   div 2;
    delta.Y := (baseImg.Height - imgBooks.Height) div 2;
    Image32_Vector.OffsetRect(imgRec, delta.X, delta.Y);

    //draw the background color and imgBooks onto baseImg
    baseImg.Clear(bkColor);
    baseImg.CopyBlend(imgBooks, imgBooks.Bounds, imgRec, BlendToOpaque);

  finally
    imgBooks.Free;
  end;

  //now give baseImg a border
  imgRec := baseImg.Bounds;
  imgRec := Image32_Vector.InflateRect(imgRec, -1, -1);
  tmpPath := Rectangle(imgRec);
  DrawLine(baseImg, tmpPath, 2, penColor, esPolygon);

  //draw the copyright text in the bottom right corner
  tmpRec := Image32_Vector.GetBounds(copyrightGlyphs);
  copyrightGlyphs :=
    Image32_Vector.OffsetPath(copyrightGlyphs,
    imgRec.Right - margin - tmpRec.Width,
    imgRec.Bottom - margin);
  DrawPolygon(baseImg, copyrightGlyphs, frNonZero, clBlack32);

  //get ready for some simple text animation
  zoomCnt := Length(zoomImages);
  zoomIdx := zoomCnt -1;
  zoomImages[zoomIdx] := baseImg;

  //make the 'bigTextGlyphs' text a little bolder ...
  bigTextGlyphs := InflatePaths(bigTextGlyphs, 1.5, jsAuto, esPolygon);

  //and perform an affine transformation to
  //stretch the text glyphs vertically and italicize too
  matrix := IdentityMatrix;
  MatrixScale(matrix, 1, 1.75);
  MatrixSkew(matrix, -0.25, 0);
  MatrixApply(matrix, bigTextGlyphs);

  //center mainTxtPaths inside displayImg ...
  textRec := Image32_Vector.GetBounds(bigTextGlyphs);
  delta.X := (baseImg.Width - textRec.Width) div 2 - textRec.Left;
  delta.Y := (baseImg.Height - textRec.Height) div 2 - textRec.Top;
  outlineText := OffsetPath(bigTextGlyphs, delta.X, delta.Y);

  //fill an array of images copying the base image and overlaying
  //some scaled text in preparation for some text 'zoom' animation
  zoomImages[0] := TImage32.Create(baseImg);

  DrawPolygon(zoomImages[0], outlineText, frNonZero, clBlack32);
  //outlineText := InflatePaths(outlineText, -1.0, jsAuto, esPolygon);
  DrawPolygon(zoomImages[0], outlineText, frNonZero, txtColor);
  Draw3D(zoomImages[0], outlineText, frNonZero, 6, 3);

  for i := 1 to zoomCnt -2 do
  begin
    textRec := Image32_Vector.GetBounds(outlineText);
    outlineText := ScalePath(outlineText, 0.9, 0.9);
    textRec2 := Image32_Vector.GetBounds(outlineText);
    outlineText := OffsetPath(outlineText,
      (textRec.Left-textRec2.Left) + (textRec.Width-textRec2.Width)/2,
      (textRec.Top-textRec2.Top) + (textRec.Height-textRec2.Height)/2);
    zoomImages[i] := TImage32.Create(baseImg);
    DrawPolygon(zoomImages[i], outlineText, frNonZero, clGray32);
  end;

  UpdateImageViewer;
  ImageViewer1Resized(nil);
  btnRefresh.Enabled := false;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := High(zoomImages) downto 0 do
    zoomImages[i].Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateImageViewer;
var
  surf: TBitmapSurface;
  img: TImage32;
begin
  img := zoomImages[zoomIdx];
  surf := TBitmapSurface.Create;
  try
    surf.SetSize(img.Width, img.Height, TPixelFormat.BGRA);
    Move(img.PixelBase^, surf.Scanline[0]^, img.Width * img.Height *4);
    ImageViewer1.Bitmap.Assign(surf);
  finally
    surf.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ImageViewer1Resized(Sender: TObject);
begin
  if ImageViewer1.Bitmap.Width > 0 then
    ImageViewer1.BitmapScale :=
      Min((ImageViewer1.ClientWidth - margin * 2) /
      ImageViewer1.Bitmap.Width ,
      (ImageViewer1.ClientHeight - margin * 2) /
      ImageViewer1.Bitmap.Height);

  btnClose.Position.X := ClientWidth - btnClose.Width - btnClose.Height / 2;
  btnClose.Position.Y := ClientHeight - btnClose.Height * 3 / 2;

  btnRefresh.Position.X := btnClose.Position.X;
  btnRefresh.Position.Y := btnClose.Position.Y - btnRefresh.Height * 4/3;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormShow(Sender: TObject);
begin
  if zoomImages[0] = nil then Exit; //something went wrong while loading
  Timer1.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Interval := 25;
  dec(zoomIdx);
  UpdateImageViewer;
  if zoomIdx = 0 then
  begin
    Timer1.Enabled := false;
    btnRefresh.Enabled := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  btnRefresh.Enabled := false;
  zoomIdx := High(zoomImages);
  UpdateImageViewer;
  Timer1.Interval := 1000;
  Timer1.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
