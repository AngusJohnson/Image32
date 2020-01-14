unit Image32_GIF;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.17                                                            *
* Date      :  11 August 2019                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  GIF file format extension for TImage32                          *
* Dependency:  Requires GifImage.pas from http://www.tolderlund.eu/delphi/     *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}
uses
  SysUtils, Classes, Windows, Math, Image32, Graphics, GifImage;

type

  TImageFormat_GIF = class(TImageFormat)
  public
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; override;
    procedure SaveToStream(stream: TStream; img32: TImage32); override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
  end;

implementation

//------------------------------------------------------------------------------
// Loading (reading) GIF images from file ...
//------------------------------------------------------------------------------

function TImageFormat_GIF.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  gif: TGIFImage;
begin
  result := false;
  gif := TGIFImage.Create;
  try
    gif.LoadFromStream(stream);
    if gif.Empty then Exit;
    with gif.Bitmap do
      img32.CopyFromDC(Canvas.Handle, Rect(0,0, Width, Height));
    result := true;
  finally
    gif.Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) gif images to file ...
//------------------------------------------------------------------------------

procedure TImageFormat_GIF.SaveToStream(stream: TStream; img32: TImage32);
var
  gif: TGIFImage;
begin
  gif := TGIFImage.Create;
  try
    gif.Bitmap.Width := img32.Width;
    gif.Bitmap.Height := img32.Height;
    img32.CopyToDc(gif.Bitmap.Canvas.Handle,0,0, false);
    gif.SaveToStream(stream);
  finally
    gif.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_GIF.CopyToClipboard(img32: TImage32): Boolean;
begin
  result := false; //not implemented
end;
//------------------------------------------------------------------------------

class function TImageFormat_GIF.CanPasteFromClipboard: Boolean;
begin
  result := false;
end;
//------------------------------------------------------------------------------

class function TImageFormat_GIF.PasteFromClipboard(img32: TImage32): Boolean;
begin
  result := false; //not implemented
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('GIF', TImageFormat_GIF, cpLow);

end.

