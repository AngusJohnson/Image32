unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  IntfGraphics, ComCtrls, GraphType,
  Img32, Img32.Text, Img32.Fmt.BMP, Img32.Fmt.SVG, Img32.Vector, Img32.Draw;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnClose: TButton;
    Image1: TImage;
    StatusBar1: TStatusBar;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    img: TImage32;
    copyTxtPaths: TPathsD;
    penColor: TColor32;
    procedure DrawImage;
  end;

var
  MainForm: TMainForm;
const
  margin = 20;

{$IFDEF FPC}
  RT_RCDATA = PChar(10);
{$ENDIF}

implementation

{$R *.lfm}
{$R font.res}

//------------------------------------------------------------------------------
// TMainForm
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
   copyright  : UnicodeString;
   fontReader2: TFontReader;
   fontCache  : TFontCache;
   nextX      : double;
begin
  penColor := clMaroon32;
{$IFNDEF MSWINDOWS}
  penColor := SwapRedBlue(penColor);
{$ENDIF}
  img := TImage32.create;
  Image1.Picture.Bitmap.PixelFormat := pf32bit;

  copyright := UnicodeString('© 2023 Angus Johnson');
  // Create a TFontReader object to access a truetype font stored as font
  // resource. This can be destroyed here, otherwise the FontManager
  // will automatically destroy it on close.
  fontReader2 := FontManager.LoadFromResource('FONT_2', RT_RCDATA);
  //fontReader2 := FontManager.Load('Arial'); //only Windows
  if not Assigned(fontReader2) or not fontReader2.IsValidFontFormat then Exit;
  //get the 'copyright' text glyphs outline
  fontCache := TFontCache.Create(fontReader2, DPIAware(15));
  try
    copyTxtPaths := fontCache.GetTextOutline(0, 0, copyright, nextX);
  finally
    fontCache.Free;
    fontReader2.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  img.Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DrawImage;
var
  w, h, dx, dy: integer;
  tmpImg: TImage32;
  tmpPath: TPathD;
  textRec, dstRec: TRect;
begin
  w := Image1.Picture.Bitmap.Width;
  h := btnClose.Top - DpiAware(10);
  img.SetSize(w, h);
  if img.IsEmpty then Exit;
  img.LoadFromResource('IMAGE32', RT_RCDATA);

  // this just re-centers the imported SVG image ...
  dx := (w - img.Width) div 2;
  dy := (h - img.Height) div 2;
  tmpImg := TImage32.Create(img);
  try
    img.SetSize(w, h);
    img.Copy(tmpImg, tmpImg.Bounds,
      Rect(dx, dy, dx + tmpImg.Width, dy + tmpImg.Height));
  finally
    tmpImg.free;
  end;

  // now box outline the whole image and add a copyright notice :)
  tmpPath := Img32.Vector.Rectangle(img.Bounds);
  DrawLine(img, tmpPath, 10, penColor, esPolygon);
  textRec := GetBounds(copyTxtPaths);
  copyTxtPaths := OffsetPath(copyTxtPaths, 10- textRec.Left, 10 - textRec.Top);
  DrawPolygon(img, copyTxtPaths, frNonZero, clBlack32);

  // finally draw 'img' onto the Image1 component ...
  Image1.Picture.Bitmap.PixelFormat := pf32bit;
  Image1.Picture.Bitmap.SetSize(img.Width, img.Height);
  img.CopyToDc(Image1.Picture.Bitmap.Canvas.Handle);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  Image1.Picture.Bitmap.SetSize(ClientWidth, ClientHeight);
  DrawImage;
end;
//------------------------------------------------------------------------------

end.

