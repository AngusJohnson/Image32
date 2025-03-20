unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Rtti, Math, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls, FMX.Platform, FMX.Surfaces,
  FMX.StdCtrls, FMX.Controls.Presentation, Img32, Img32.FMX, Img32.Layers,
  System.ImageList, Img32.Fmt.SVG;


type
  TMainForm = class(TForm)
    btnClose: TButton;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Layout1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure FormResize(Sender: TObject);
  private
    img: TImage32;
    procedure Display;
  end;

var
  MainForm: TMainForm;
  margin: integer;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}
{$R *.LgXhdpiTb.fmx ANDROID}

{$R font.res}
{$R image.res}

uses
  Img32.Vector, Img32.Transform, Img32.Draw, Img32.Extra,
  Img32.Text, Img32.Clipper2;


//------------------------------------------------------------------------------
// TMainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  margin := DPIAware(14);
  Layout1.Scale.Point := PointF(1 / DpiAwareOne, 1 / DpiAwareOne);

  // Image32's SVG reader uses FontManager and, to display any
  // SVG text, it will require at least one font to be loaded
  FontManager.LoadFromResource('FONT_1', RT_RCDATA);
  img := TImage32.Create;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  img.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  w,h: integer;
begin
  w := ClientWidth;
  h := ClientHeight;
  btnClose.Position.X := w - btnClose.Width - margin;
  btnClose.Position.Y := h - btnClose.Height - margin;
  // To avoid loading SVG images using their native sizes and later scaling the
  // generated (raster) images to their display sizes, specify image sizes
  // BEFORE loading. (This isn't important here because the size differences
  // between the native SVG and display image sizes will be small, but this
  // 'pre-sizing' will be important whenever there will be significant scaling.)
  // Of course 'pre-sizing' only works with SVG images because image resampling
  // can't be bypassed when scaling raster images (PNG, BMP formats etc.)
  img.SetSize(w, h);
  img.LoadFromResource('SVG_TEXT', RT_RCDATA);
  Display;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Display;
var
  rec: TRect;
begin
  with Layout1 do
    rec := Img32.Vector.Rect(0, 0, Trunc(Width), Trunc(Height));
  Img32.Vector.InflateRect(rec, -margin, -margin);
  img.ScaleToFit(rec.Width, rec.Height);
end;
//------------------------------------------------------------------------------

procedure TMainForm.Layout1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  bmp: TBitmap;
  rec: TRectF;
begin
  //unfortunately we need an intermediate TBitmap object
  //to draw raster images onto an FMX canvas.
  bmp := TBitmap.Create;
  try
    AssignImage32ToFmxBitmap(img, bmp);
    rec := RectF(0,0,bmp.Width,bmp.Height);
    Canvas.Lock;
    Canvas.DrawBitmap(bmp, rec, rec, 1.0);
    Canvas.Unlock;
  finally
    bmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
