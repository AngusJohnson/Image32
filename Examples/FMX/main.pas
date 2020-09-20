unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Rtti, Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  Image32, Image32_Vector, Image32_Draw, Image32_FMX,
  Image32_Ttf, Image32_Clipper,
  FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls, FMX.Platform, FMX.Surfaces,
  FMX.StdCtrls, FMX.Controls.Presentation;

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
    zoomCount: cardinal;
    displayImg: TImage32;
    zoomText: array[0..19] of TPathsD;
    outlineText: TPathsD;
    procedure UpdateImageViewer(img: TImage32);
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

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  resourceImg: TImage32;
  tmp: TPathD;
  paths: TPathsD;
  textRec, imageRec: TRect;
  rec1, rec2: TRect;

  delta: TPoint;
  screenScale, fontHeight, nextX: double;
  glyphManager: TGlyphManager;
  fontReader : TTtfFontReader;

  ScreenService: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService (
    IFMXScreenService, IInterface(ScreenService)) then
      screenScale := ScreenService.GetScreenScale else
      screenScale := 1.0;

  displayImg := TImage32.Create();

  bkColor  := $FFF8F8BB; //yellow
  penColor := clMaroon32;
  txtColor := $FF00BB00; //green
{$IFNDEF MSWINDOWS}
  bkColor  := SwapRedBlue(bkColor);
  penColor := SwapRedBlue(penColor);
  txtColor := SwapRedBlue(txtColor);
{$ENDIF}

  //get outline for some text from the font resource
  fontHeight := 35 * screenScale;
  glyphManager := TGlyphManager.Create(fontHeight);
  fontReader := TTtfFontReader.Create;
  try
    //fontReader.LoadFromFile('c:\windows\fonts\arial.ttf');
    fontReader.LoadFromResource('FONT', RT_RCDATA);
    if not fontReader.IsValidFontFormat then Exit;

    glyphManager.GetString('Image32  rocks!', fontReader, paths, nextX);
  finally
    fontReader.free;
    glyphManager.Free;
  end;

  resourceImg := TImage32.Create;
  try
    //load the background image ...
    resourceImg.LoadFromResource('PNGIMAGE_1', RT_RCDATA);

    //set the size of the display image so that it fits
    //both the resource image and the text ...
    textRec := Image32_Vector.GetBounds(paths);
    displayImg.SetSize(
      max(resourceImg.Width, textRec.Width) + margin *4,
      max(resourceImg.Height, textRec.Height) + margin *4);

    //fill the display image and give it a border too
    displayImg.FillRect(displayImg.Bounds, bkColor);
    tmp := Image32_Vector.Rectangle(displayImg.Bounds);
    DrawLine(displayImg, tmp, 2, penColor, esClosed);

    //offset the resource image so it's centered in the display image
    //and copy the resource image onto the display image ...
    delta := Point((displayImg.Width - resourceImg.Width) div 2,
      (displayImg.Height - resourceImg.Height) div 2);
    imageRec := resourceImg.Bounds;
    OffsetRect(imageRec, delta.X, delta.Y);
    displayImg.CopyBlend(resourceImg, resourceImg.Bounds, imageRec, BlendToOpaque);

    //center the text too ...
    delta := Point((displayImg.Width - textRec.Width) div 2 - textRec.Left,
      (displayImg.Height - textRec.Height) div 2 - textRec.Top);
    zoomText[0] := OffsetPath(paths, delta.X, delta.Y);
    outlineText := InflatePolygons(zoomText[0], 1.5);

    //get an array of zoomed test paths
    zoomCount := Length(zoomText);
    for i := 1 to zoomCount -1 do
    begin
      rec1 := Image32_Vector.GetBounds(zoomText[i-1]);
      zoomText[i] := OffsetPath(zoomText[i-1], -rec1.Left, -rec1.Top);
      zoomText[i] := ScalePath(zoomText[i], 0.9, 0.9);
      rec2 := Image32_Vector.GetBounds(zoomText[i]);
      zoomText[i] := OffsetPath(zoomText[i],
        rec1.Left + (rec1.Width - rec2.Width) div 2,
        rec1.Top + (rec1.Height - rec2.Height) div 2);
    end;

    UpdateImageViewer(displayImg);
    ImageViewer1Resized(nil);
    btnRefresh.Enabled := false;

  finally
    resourceImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  displayImg.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateImageViewer(img: TImage32);
var
  surf: TBitmapSurface;
begin
  surf := TBitmapSurface.Create;
  try
    surf.SetSize(img.Width, img.Height, TPixelFormat.BGRA);
    Move(img.PixelBase^, surf.Scanline[0]^, img.Width * img.Height *4);
    ImageViewer1.Bitmap.Assign(surf);

    ImageViewer1.RealignContent;
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

  btnRefresh.Position.X :=
    btnClose.Position.X - btnRefresh.Width - btnClose.Height /2;
  btnRefresh.Position.Y := btnClose.Position.Y;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;
//------------------------------------------------------------------------------


procedure TMainForm.Timer1Timer(Sender: TObject);
var
  img: TImage32;
  zoomTotal, alpha: cardinal;
begin
  Timer1.Interval := 25;
  dec(zoomCount);

  img := TImage32.Create(displayImg);
  try
    if zoomCount = 0 then
    begin
      Timer1.Enabled := false;
      btnRefresh.Enabled := true;
      DrawPolygon(img, outlineText, frNonZero, clBlack32);
      DrawPolygon(img, zoomText[zoomCount], frNonZero, txtColor);
    end else
    begin
      zoomTotal := High(zoomText);
      alpha :=  ((zoomTotal - zoomCount) *255 div zoomTotal) shl 24;
      DrawPolygon(img, zoomText[zoomCount], frNonZero, alpha);
    end;

    UpdateImageViewer(img);
  finally
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  btnRefresh.Enabled := false;
  UpdateImageViewer(displayImg);
  zoomCount := Length(zoomText);
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
