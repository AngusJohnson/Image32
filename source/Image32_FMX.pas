unit Image32_FMX;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.48                                                            *
* Date      :  29 August 2020                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  Image file format support for TImage32 and FMX                  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}
uses
  SysUtils, Classes, Math, Image32, System.Rtti,
  FMX.Platform, FMX.Types, FMX.Surfaces, FMX.Graphics;

type

  TImageFormat_FMX = class(TImageFormat)
  private
    fExt: string;
  public
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; override;
    function SaveToFile(const filename: string; img32: TImage32): Boolean; override;
    procedure SaveToStream(stream: TStream; img32: TImage32); override;
    class function CopyToClipboard(img32: TImage32): Boolean; override;
    class function CanPasteFromClipboard: Boolean; override;
    class function PasteFromClipboard(img32: TImage32): Boolean; override;
    property Ext: string read fExt write fExt;
  end;

implementation

//------------------------------------------------------------------------------
// Loading (reading) images from file ...
//------------------------------------------------------------------------------

function TImageFormat_FMX.LoadFromStream(stream: TStream; img32: TImage32): Boolean;
var
  cm: TBitmapCodecManager;
  surf: TBitmapSurface;
begin
  result := false;
  surf := TBitmapSurface.Create;
  cm := TBitmapCodecManager.Create;
  try
    cm.LoadFromStream(stream, surf);
    if (surf.Width = 0) or (surf.Height = 0) then Exit;
    if (surf.PixelFormat <> TPixelFormat.BGRA) then Exit;

    img32.SetSize(surf.Width, surf.Height);
    Move(surf.Scanline[0]^, img32.PixelBase^, surf.Width * surf.Height * 4);
    result := true;
  finally
    cm.Free;
    surf.Free;
  end;
end;

//------------------------------------------------------------------------------
// Saving (writing) Jpeg images to file ...
//------------------------------------------------------------------------------

function TImageFormat_FMX.SaveToFile(const filename: string; img32: TImage32): Boolean;
begin
  Ext := ExtractFileExt(filename);
  result := inherited;
end;
//------------------------------------------------------------------------------

procedure TImageFormat_FMX.SaveToStream(stream: TStream; img32: TImage32);
var
  cm: TBitmapCodecManager;
  surf: TBitmapSurface;
begin
  if img32.IsEmpty then Exit;
  surf := TBitmapSurface.Create;
  cm := TBitmapCodecManager.Create;
  try
    surf.SetSize(img32.Width, img32.Height, TPixelFormat.BGRA);
    Move(img32.PixelBase^, surf.Scanline[0]^, img32.Width * img32.Height * 4);
    if Ext = '' then
      cm.SaveToStream(stream, surf, 'PNG') else
      cm.SaveToStream(stream, surf, Ext);
  finally
    cm.Free;
    surf.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_FMX.CopyToClipboard(img32: TImage32): Boolean;
var
  surf: TBitmapSurface;
  svc: IFMXClipboardService;
begin
  Result := assigned(img32) and not img32.IsEmpty and
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, svc);
  if not Result then Exit;

  surf := TBitmapSurface.Create;
  try
    surf.SetSize(img32.Width, img32.Height, TPixelFormat.BGRA);
    Move(img32.PixelBase^, surf.Scanline[0]^, img32.Width * img32.Height * 4);
    svc.SetClipboard(surf);
  finally
    surf.Free;
  end;
end;
//------------------------------------------------------------------------------

class function TImageFormat_FMX.CanPasteFromClipboard: Boolean;
var
  svc: IFMXClipboardService;
  value: TValue;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, svc) then
  begin
    value := svc.GetClipboard;
    Result := Value.IsType<TBitmapSurface>;
    if value.IsObject then value.AsObject.Free;
  end else
    Result := false;
end;

//------------------------------------------------------------------------------

class function TImageFormat_FMX.PasteFromClipboard(img32: TImage32): Boolean;
var
  svc: IFMXClipboardService;
  value: TValue;
  surf: TBitmapSurface;
begin
  Result := false;
  if not assigned(img32) or
    not TPlatformServices.Current.SupportsPlatformService(
    IFMXClipboardService, svc) then Exit;

  value := svc.GetClipboard;
  if not Value.IsObject then Exit;

  try
    if Value.IsType<TBitmapSurface> and
      (Value.AsType<TBitmapSurface>.PixelFormat = TPixelFormat.BGRA) then
    begin
      surf := Value.AsType<TBitmapSurface>;
      img32.SetSize(surf.Width, surf.Height);
      Move(surf.Scanline[0]^, img32.PixelBase^, surf.Width * surf.Height * 4);
      Result := true;
    end;
  finally
    value.AsObject.Free;
  end;

end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  TImage32.RegisterImageFormatClass('BMP', TImageFormat_FMX, cpLow);
  TImage32.RegisterImageFormatClass('PNG', TImageFormat_FMX, cpHigh);
  TImage32.RegisterImageFormatClass('JPG', TImageFormat_FMX, cpLow);
  TImage32.RegisterImageFormatClass('GIF', TImageFormat_FMX, cpLow);

end.

