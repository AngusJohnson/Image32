unit Image32_JPG;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.17                                                            *
* Date      :  11 August 2019                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  JPG/JPEG file format extension for TImage32                     *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Windows, Math, Image32, Graphics, JPEG;

type

  TImage32Fmt_JPG = class(TImageFormat)
  public
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; override;
    procedure SaveToStream(stream: TStream; img32: TImage32); override;
    function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    function PasteFromClipboard(img32: TImage32): Boolean; override;
  end;

var
  CF_JPG: Cardinal = 0;     //Windows Clipboard
  CF_IMAGEJPG: Cardinal = 0;

implementation

//------------------------------------------------------------------------------
// Loading (reading) Jpeg images from file ...
//------------------------------------------------------------------------------

type
  TJpegImageHack = class(TJpegImage);

function TImage32Fmt_JPG.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  jpeg: TJpegImage;
begin
  result := false;
  jpeg := TJpegImage.Create;
  try
    jpeg.LoadFromStream(stream);
    if jpeg.Empty then Exit;
    with TJpegImageHack(jpeg).Bitmap do
      img32.CopyFromDC(Canvas.Handle, Width, Height);
    result := true;
  finally
    jpeg.Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) Jpeg images to file ...
//------------------------------------------------------------------------------

procedure TImage32Fmt_JPG.SaveToStream(stream: TStream; img32: TImage32);
var
  Jpeg: TJpegImage;
begin
  Jpeg := TJpegImage.Create;
  try
    TJpegImageHack(jpeg).NewImage;
    TJpegImageHack(jpeg).NewBitmap;
    TJpegImageHack(jpeg).Bitmap.Width := img32.Width;
    TJpegImageHack(jpeg).Bitmap.Height := img32.Height;
    img32.CopyToDc(TJpegImageHack(jpeg).Bitmap.Canvas.Handle,0,0, false);
    Jpeg.SaveToStream(stream);
  finally
    Jpeg.Free;
  end;
end;
//------------------------------------------------------------------------------

function TImage32Fmt_JPG.CopyToClipboard(img32: TImage32): Boolean;
begin
  result := false; //not implemented
end;
//------------------------------------------------------------------------------

class function TImage32Fmt_JPG.CanPasteFromClipboard: Boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------

function TImage32Fmt_JPG.PasteFromClipboard(img32: TImage32): Boolean;
begin
  result := false; //not implemented
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('JPG', TImage32Fmt_JPG, cpLow);
  TImage32.RegisterImageFormatClass('JPEG', TImage32Fmt_JPG, cpLow);
  //don't bother with clipboard formats as PNG and BMP formats are preferred

end.
