unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, Math,
  Types, Img32, Img32.Fmt.PNG, Img32.Text,
  Img32.Vector, Img32.Transform, Img32.Extra, Img32.Draw;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnRefresh: TButton;
    btnClose: TButton;
    Timer1: TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    bkColor, penColor, txtColor: TColor32;
    zoomIdx: integer;
    zoomImages: array[0..16] of TImage32;
    procedure PrepareBaseImage;
    procedure PrepareZoomImages(var zoomGlyphs: TPathsD; const offset: TPoint);
  public

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
//------------------------------------------------------------------------------

function ToUnicode(const s: string): UnicodeString;
begin
  Result := UnicodeString(s);
end;

//------------------------------------------------------------------------------
// TMainForm
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  bkColor  := $FFF8F8E0; //pale yellow
  penColor := clMaroon32;
  txtColor := $FF00BB00; //green
{$IFNDEF MSWINDOWS}
  bkColor  := SwapRedBlue(bkColor);
  penColor := SwapRedBlue(penColor);
  txtColor := SwapRedBlue(txtColor);
{$ENDIF}
  PrepareBaseImage;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to High(zoomImages) do
    zoomImages[i].Free;
end;

//------------------------------------------------------------------------------

procedure TMainForm.PrepareBaseImage;
var
  imgBooks: TImage32;
  tmpPath: TPathD;
  mainTxtPaths, copyTxtPaths: TPathsD;
  textRec, imageRec: TRect;
  matrix: TMatrixD;
  delta: TPoint;
  nextX: double;
  copyright, sillyText: UnicodeString;

  glyphCache: TGlyphCache;
  fontReader : TFontReader;
begin
  //workaround a Lazarus quirk ...
  //it doesn't seem to dynamically convert static text ...
  copyright := ToUnicode('© 2020 Angus Johnson');
  sillyText := ToUnicode('Image32  Rocks!');

  //get ready for some simple text animation
  zoomIdx := 0;
  zoomImages[0] := TImage32.Create();

  //create a TFontReader to access a couple of truetype font files (*.ttf)
  //that have been stored as font resources and create a TGlyphCache too
  fontReader := TFontReader.Create;
  glyphCache := TGlyphCache.Create(fontReader, DpiAware(36));
  try
    //connect fontReader to a specific font
    fontReader.LoadFromResource('FONT_2', RT_RCDATA);
    if not fontReader.IsValidFontFormat then Exit;

    //and get the outline for some text ...
    mainTxtPaths := glyphCache.GetTextGlyphs(0,0, sillyText, nextX);

    //Normally we'd create different fontReaders for different fonts and
    //also use different glyphManagers for different font heights,
    //but it's also possible to reuse these objects ...

    fontReader.LoadFromResource('FONT_3', RT_RCDATA);
    if not fontReader.IsValidFontFormat then Exit;
    //nb: fontReader changing fonts will automatically clear glyphCache
    //cache, though changing glyphCache fontHeight will also do the same...
    glyphCache.FontHeight := DPIAware(10);

    //and now get the copyright text outline
    copyTxtPaths := glyphCache.GetTextGlyphs(0,0, copyright, nextX);
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

  //this seems necessary because Lazarus appears to bypass the initialization
  //section in Img32.PNG unless some code from that unit is used explicitly.
  TImage32.RegisterImageFormatClass('PNG', TImageFormat_PNG, cpHigh);

  imgBooks := TImage32.Create;
  try
    //load a colourful background image (some books) ...
    imgBooks.LoadFromResource('PNGIMAGE_1', RT_RCDATA);

    //set the size of the base image so that it contains
    //both 'imgBooks' and the text with a decent margin ...
    textRec := Img32.Vector.GetBounds(mainTxtPaths);
    zoomImages[0].SetSize(
      max(imgBooks.Width, textRec.Width) + (margin * 4),
      max(imgBooks.Height, textRec.Height) + (margin * 4));

    //color fill the base image and give it a border ...
    with zoomImages[0] do
    begin
       FillRect(Bounds, bkColor);
       tmpPath := Img32.Vector.Rectangle(Bounds);
    end;
    DrawLine(zoomImages[0], tmpPath, 2, penColor, esPolygon);

    //offset 'imgBooks' so it's centered in the base image
    //and copy it onto the base image ...
    imageRec := imgBooks.Bounds;
    delta := Point((zoomImages[0].Width - imgBooks.Width) div 2,
      (zoomImages[0].Height - imgBooks.Height) div 2);
    Types.OffsetRect(imageRec, delta.X, delta.Y);
    zoomImages[0].CopyBlend(imgBooks, imgBooks.Bounds,
      imageRec, @BlendToOpaque);

  finally
    imgBooks.Free;
  end;

  //draw a simple copyright notice using a normal font (font_1)
  //and locate it in the bottom right corner of the display image
  with GetBoundsD(copyTxtPaths) do
    copyTxtPaths := OffsetPath(copyTxtPaths,
      zoomImages[0].Width - Width -10, zoomImages[0].Height - 10);
  DrawPolygon(zoomImages[0], copyTxtPaths, frNonZero, clBlack32);

  //center mainTxtPaths inside displayImg ...
  delta.X := (zoomImages[0].Width - textRec.Width) div 2 - textRec.Left;
  delta.Y := (zoomImages[0].Height - textRec.Height) div 2 - textRec.Top;

  PrepareZoomImages(mainTxtPaths, delta);

  btnRefresh.Enabled := false;

  Invalidate;
end;
//------------------------------------------------------------------------------


procedure TMainForm.PrepareZoomImages(var zoomGlyphs: TPathsD;
  const offset: TPoint);
var
  i,highI: integer;
  textRec, textRec2: TRect;
begin

  zoomGlyphs := OffsetPath(zoomGlyphs, offset.X, offset.Y);

  //fill an array of images copying the base image and overlaying
  //some scaled text in preparation for some text 'zoom' animation
  highI := High(zoomImages);
  for i := 1 to highI do
    zoomImages[i] := TImage32.Create(zoomImages[0]);

  //draw a drop shadow
  DrawShadow(zoomImages[highI], zoomGlyphs, frNonZero, 4, -angle45);
  //draw text background (green)
  DrawPolygon(zoomImages[highI], zoomGlyphs, frNonZero, txtColor);
  //draw the text edges (black) and add a 3D effect
  DrawLine(zoomImages[highI], zoomGlyphs, 1.5, clBlack32, esPolygon);
  Draw3D(zoomImages[highI], zoomGlyphs, frNonZero, 6, 3);

  for i := highI-1 downto 1 do
  begin
    textRec := Img32.Vector.GetBounds(zoomGlyphs);
    zoomGlyphs := ScalePath(zoomGlyphs, 0.9, 0.9);
    textRec2 := Img32.Vector.GetBounds(zoomGlyphs);
    zoomGlyphs := OffsetPath(zoomGlyphs,
      (textRec.Left-textRec2.Left) + (textRec.Width-textRec2.Width)/2,
      (textRec.Top-textRec2.Top) + (textRec.Height-textRec2.Height)/2);
    zoomImages[i] := TImage32.Create(zoomImages[zoomIdx]);
    DrawPolygon(zoomImages[i], zoomGlyphs, frNonZero, clGray32);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
  btnRefresh.Enabled := false;
  zoomIdx := 0;
  Timer1.Interval := 1000;
  Timer1.Enabled := true;
  Invalidate;;
end;
//------------------------------------------------------------------------------

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Interval := 25;
  inc(zoomIdx);
  Invalidate;
  if zoomIdx = High(zoomImages) then
  begin
    Timer1.Enabled := false;
    btnRefresh.Enabled := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  dstRec: TRect;
begin
  dstRec := zoomImages[0].Bounds;
  OffsetRect(dstRec,
    (ClientWidth-RectWidth(dstRec)) div 2 - dstRec.Left,
    (ClientHeight- btnClose.Height -RectHeight(dstRec)) div 2 - dstRec.Top);
  zoomImages[zoomIdx].CopyToDc(Canvas.Handle, dstRec.Left, dstRec.Top);
end;
//------------------------------------------------------------------------------

end.

