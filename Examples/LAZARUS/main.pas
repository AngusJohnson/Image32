unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Math,
  Image32, Image32_PNG, Image32_Ttf, Image32_Vector, Image32_Extra, Image32_Draw;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnRefresh: TButton;
    btnClose: TButton;
    Image1: TImage;
    Timer1: TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    zoomIdx: integer;
    zoomImages: array[0..16] of TImage32;
    procedure UpdateImageViewer;
  public

  end;

var
  MainForm: TMainForm;
  bkColor, penColor, txtColor: TColor32;
const
  margin = 20;

{$IFDEF FPC}
  RT_RCDATA = PChar(10);
{$ENDIF}

implementation

{$R *.lfm}
{$R font.res}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i,zoomCnt: integer;
  baseImg, imgBooks: TImage32;
  tmp: TPathD;
  outlineText, mainTxtPaths, copyTxtPaths: TPathsD;
  textRec, textRec2, imageRec: TRect;
  matrix: TMatrixD;
  delta: TPoint;
  screenScale, fontHeight, nextX: double;

  glyphCache: TGlyphCache;
  fontReader : TTtfFontReader;
begin
  screenScale := 2.0;

  fontHeight := 35 * screenScale;

  bkColor  := $FFF8F8BB; //yellow
  penColor := clMaroon32;
  txtColor := $FF00BB00; //green
{$IFNDEF MSWINDOWS}
  bkColor  := SwapRedBlue(bkColor);
  penColor := SwapRedBlue(penColor);
  txtColor := SwapRedBlue(txtColor);
{$ENDIF}


  baseImg := TImage32.Create();
  //get ready for some simple text animation
  zoomCnt := Length(zoomImages);
  zoomIdx := zoomCnt -1;
  zoomImages[zoomIdx] := baseImg;

  //create a fontReader to access truetype font files (*.ttf) that
  //have been stored as font resources and create a glyph cache too
  fontReader := TTtfFontReader.Create;
  glyphCache := TGlyphCache.Create(fontReader, fontHeight);
  try
    //connect fontReader to a specific font
    fontReader.LoadFromResource('FONT_2', RT_RCDATA);
    if not fontReader.IsValidFontFormat then Exit;

    //and get the outline for some text ...
    glyphCache.GetString('Image32  Rocks!', mainTxtPaths, nextX);

    //Normally we'd create different fontReaders for different fonts and
    //also use different glyphManagers for different font heights.
    //But here, I'm showing it's possible to reuse these objects ...

    fontReader.LoadFromResource('FONT_1', RT_RCDATA);
    if not fontReader.IsValidFontFormat then Exit;
    //nb: fontReader changing fonts will automatically clear glyphCache
    //cache, though changing glyphCache fontHeight will also do the same...
    glyphCache.FontHeight := fontHeight / 4;

    //and now get the copyright text outline
    glyphCache.GetString('© 2020 Angus Johnson', copyTxtPaths, nextX);
  finally
    glyphCache.Free;
    fontReader.free;
  end;

  //and some affine transformations of mainTxtPaths, just for fun :)
  matrix := IdentityMatrix;
  //stretch it vertically ...
  MatrixScale(matrix, 1, 1.75);
  //and skew (yes, we could've used an italicized font instead) ...
  MatrixSkew(matrix, -0.25, 0);
  MatrixApply(matrix, mainTxtPaths);

  TImage32.RegisterImageFormatClass('PNG', TImageFormat_PNG, cpHigh);

  imgBooks := TImage32.Create;
  try
    //load a colourful background image (some books) ...
    imgBooks.LoadFromResource('PNGIMAGE_1', RT_RCDATA);

    //set the size of the base image so that it contains
    //both 'imgBooks' and the text with a decent margin ...
    textRec := Image32_Vector.GetBounds(mainTxtPaths);
    baseImg.SetSize(
      max(imgBooks.Width, textRec.Width) + (margin * 4),
      max(imgBooks.Height, textRec.Height) + (margin * 4));

    //color fill the base image and give it a border ...
    baseImg.FillRect(baseImg.Bounds, bkColor);
    tmp := Image32_Vector.Rectangle(baseImg.Bounds);
    DrawLine(baseImg, tmp, 2, penColor, esClosed);

    //offset 'imgBooks' so it's centered in the base image
    //and copy it onto the base image ...
    imageRec := imgBooks.Bounds;
    delta := Point((baseImg.Width - imgBooks.Width) div 2,
      (baseImg.Height - imgBooks.Height) div 2);
    OffsetRect(imageRec, delta.X, delta.Y);
    baseImg.CopyBlend(imgBooks, imgBooks.Bounds, imageRec, @BlendToOpaque);

  finally
    imgBooks.Free;
  end;

  //draw a simple copyright notice using a normal font (font_1)
  //and locate it in the bottom right corner of the display image
  with GetBoundsD(copyTxtPaths) do
    copyTxtPaths := OffsetPath(copyTxtPaths,
      baseImg.Width - Width -10, baseImg.Height - 10);
  DrawPolygon(baseImg, copyTxtPaths, frNonZero, clBlack32);

  //center mainTxtPaths inside displayImg ...
  delta.X := (baseImg.Width - textRec.Width) div 2 - textRec.Left;
  delta.Y := (baseImg.Height - textRec.Height) div 2 - textRec.Top;
  outlineText := OffsetPath(mainTxtPaths, delta.X, delta.Y);

  //fill an array of images copying the base image and overlaying
  //some scaled text in preparation for some text 'zoom' animation
  zoomImages[0] := TImage32.Create(baseImg);


  //a lot of fiddling to embelish final text appearance:

  //use zoomImages[1] as a temporary drawing surface
  zoomImages[1] := TImage32.Create(baseImg.Width, baseImg.Height);
  //to aid contrasting the text from the background image, create a
  //white text background image and blur it for a faint white halo
  DrawPolygon(zoomImages[1], outlineText, frNonZero, clWhite32);
  BoxBlur(zoomImages[1] , zoomImages[1].Bounds, 2, 1);
  //and make the halo standout a little more
  zoomImages[1].ScaleAlpha(1.5);
  //draw a drop shadow
  DrawShadow(zoomImages[1], outlineText, frNonZero, 4, -angle45);
  //draw text background (green)
  DrawPolygon(zoomImages[1], outlineText, frNonZero, txtColor);
  //draw the text edges (black) and add a 3D effect
  DrawLine(zoomImages[1], outlineText, 1.5, clBlack32, esClosed);
  Draw3D(zoomImages[1], outlineText, frNonZero, 6, 3);
  //copy merge the text onto imgBase.
  zoomImages[0].CopyBlend(zoomImages[1],
    zoomImages[1].Bounds,zoomImages[0].Bounds, @BlendToAlpha);
  zoomImages[1].Free;

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
  btnRefresh.Enabled := false;
  UpdateImageViewer;
end;

procedure TMainForm.UpdateImageViewer;
begin
  with zoomImages[zoomIdx] do
    Image1.Picture.Bitmap.SetSize(Width, Height);
  zoomImages[zoomIdx].CopyToDc(Image1.Picture.Bitmap.Canvas.Handle);
  Image1.Repaint;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if zoomImages[0] = nil then Exit; //something went wrong while loading
  Timer1.Enabled := true;
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  btnRefresh.Enabled := false;
  zoomIdx := High(zoomImages);
  UpdateImageViewer;
  Timer1.Interval := 1000;
  Timer1.Enabled := true;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

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

end.

