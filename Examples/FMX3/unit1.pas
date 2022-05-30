unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Img32, Img32.FMX, Img32.Fmt.SVG, FMX.Layouts;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    procedure Layout1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    { Private declarations }
  public
    img: TImage32;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses Img32.Draw, Img32.Vector;

procedure TForm1.Layout1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  rec, rec2: TRectF;
  size: integer;
  bmp: TBitmap;
begin
  //it's important to presize 'img' here so the
  //SVG loader and renderer knows the required
  //precision when rendering curved paths.
  size := Round(Layout1.Width *0.5);
  rec := RectF(0, 0, size, size);
  img := TImage32.Create(size, size);

  //unfortunately we also need a TBitmap object
  //to draw raster images onto FMX canvases
  bmp := TBitmap.Create;
  Canvas.Lock;
  try
    Canvas.Clear(clBlack32);

    img.LoadFromResource('ALARM_CLOCK', '10');
    AssignImage32ToFmxBitmap(img, bmp);
    Canvas.DrawBitmap(bmp, rec, rec, 1.0);
    Canvas.Flush;

    rec2 := rec;
    OffsetRect(rec2, bmp.Width, 0);
    img.LoadFromResource('AMAZON', '10');
    AssignImage32ToFmxBitmap(img, bmp);
    Canvas.DrawBitmap(bmp, rec, rec2, 1.0);
    Canvas.Flush;

    rec2 := rec;
    OffsetRect(rec2, 0, bmp.Height);
    img.LoadFromResource('AVITO', '10');
    AssignImage32ToFmxBitmap(img, bmp);
    Canvas.DrawBitmap(bmp, rec, rec2, 1.0);
    Canvas.Flush;

    rec2 := rec;
    OffsetRect(rec2, bmp.Width, BMP.Height);
    img.LoadFromResource('MUSIC_PLAYER', '10');
    AssignImage32ToFmxBitmap(img, bmp);
    Canvas.DrawBitmap(bmp, rec, rec2, 1.0);
    Canvas.Flush;

    rec2 := rec;
    OffsetRect(rec2, 0, BMP.Height*2);
    img.LoadFromResource('DUCK', '10');
    AssignImage32ToFmxBitmap(img, bmp);
    Canvas.DrawBitmap(bmp, rec, rec2, 1.0);
    Canvas.Flush;

    rec2 := rec;
    OffsetRect(rec2, bmp.Width, BMP.Height*2);
    img.LoadFromResource('PREVIEW', '10');
    AssignImage32ToFmxBitmap(img, bmp);
    Canvas.DrawBitmap(bmp, rec, rec2, 1.0);

  finally
    Canvas.Unlock;
    img.Free;
    bmp.Free;
  end;
end;

end.
