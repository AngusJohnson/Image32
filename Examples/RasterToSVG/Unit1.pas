unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, StdCtrls, ExtCtrls, ComCtrls, ShellApi, IniFiles, Dialogs,
  Image32, BitmapPanels, CustomDialogs;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    N1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    pnlDisplayParent: TPanel;
    pnlTop: TPanel;
    SaveDialog1: TSaveDialog;
    mnuSaveAs: TMenuItem;
    lblRaster: TLabel;
    lblSVG: TLabel;
    Help1: TMenuItem;
    About1: TMenuItem;
    Options1: TMenuItem;
    mnuEraseBackground: TMenuItem;
    N2: TMenuItem;
    Refresh1: TMenuItem;
    N3: TMenuItem;
    mnuTips: TMenuItem;
    pnlOptions: TPanel;
    pnlRaster: TPanel;
    pnlSVG: TPanel;
    lblOptions: TLabel;
    ProgressBar: TProgressBar;
    mnuManualPaletteSizes: TMenuItem;
    mnuChangeSmoothness: TMenuItem;
    N5: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure mnuTipsClick(Sender: TObject);
    procedure mnuOptionsClick(Sender: TObject);
    procedure mnuIniOptionsClick(Sender: TObject);
    procedure mnuChangeSmoothnessClick(Sender: TObject);
  private
    rasterScale        : integer;
    cancelOp           : boolean;
    rasterImg          : TImage32;
    svgImg             : TImage32;
    isDrawing          : Boolean;
    maxColors          : integer;
    maximumColors      : integer;
    smoothness         : integer;
    ImageMargin        : integer;
    hideJPGMessage     : Boolean;
    svgStringStream    : TStringStream;
    palVectorsList: TList;
    procedure LoadIniSettings;
    procedure WMDropFiles(var msg: TWMDropFiles); message WM_DROPFILES;
    procedure DoOpenFile(const filename: string; forceColorCheck: Boolean);
    function CheckJpg(const filename: string): Boolean;
    procedure BuildVectorListFromMonochromeImage(tmpMasterImg: TImage32; scale: double);
    procedure BuildVectorListFromColorDrawing(tmpMasterImg: TImage32; scale: double);
    procedure BuildSvgFromVectorList(
      newWidth, newHeight: integer; scaling: double);
    function GetMaxColors(forceCheck: Boolean): Boolean;
    procedure DisplayImages;
    procedure ClearPalVectors;
    procedure CheckboxOnJPGMessageDialog(Sender: TObject;
      isChecked: Boolean);
    procedure UpdateOptions(isDrawing: Boolean);
    function GetTransformOptions(isDrawing: Boolean): string;
    procedure SvgPnlKeydown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}
{$R Image.res}

uses
  Image32_Draw, Image32_Vector, Image32_Extra, Image32_BMP, Image32_JPG,
  Image32_PNG, Image32_Clipper, Image32_CQ, Clipper, ClipperCore;

resourcestring
  menuTips =
    #10+
    'Options Menu:'#10+
    '=========='#10#10+
    'Erase Background:'#10+
    'An image will be assumed to have a background color if '+
    'there''s a common color in at least 3 of its 4 corners. '+
    'Note that the background color will be removed from both outer and '+
    'inner regions. This will be problematic if this color has also been '+
    'used in non-background regions.'#10#10+
    'Manually assign color palette size:'#10+
    'Normally this utility creates palettes just large enough to ensure '+
    'excellent color reproduction. However since this can lead to '+
    'overly complex images, and larger that acceptable files, the user '+
    'may wish to retain full control of palette sizes. (Note that even when '+
    'this option is unchecked, ''Refreshing'' (Crtl+R) will still '+
    'require manually setting the palette size.)';
  jpgWarning =
    'Warning: JPG images rarely convert well into SVG format. Not only do '+
    'they normally contain photos that are unsuited to SVGs, '+
    'but they also contain ''lossy'' compression  artifacts that '+
    'tend to spoil conversions.';
  colorCountMessage =
    'The raster image uses %1.0n different colors.'#10#10+
    'Select the number of colors you want to use in the new SVG image. '+
    'More colors may give better reproduction, but the file size '+
    'will also be larger.   Range: 4 .. 256';

//------------------------------------------------------------------------------
// SVG file storage
//------------------------------------------------------------------------------

const
  FillRule: array[TFillRule] of string = ('evenodd', 'nonzero', '', '');
  SvgHeader: string = //expects with and height %d replacement
     '<?xml version="1.0" standalone="no"?>'#10+
     '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"'#10+
     '  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">'#10+
     '<svg width="%dpx" height="%dpx" viewBox="0 0 %0:d %d" '+
     'version="1.1" xmlns="http://www.w3.org/2000/svg">'#10;

  SvgDefsBegin: string =
    '<style type="text/css"><![CDATA['#10;
  SvgDefsEnd: string = ']]></style>'#10+#10;

  SvgFooter: string = '</svg>'#10;

//------------------------------------------------------------------------------

function FormatPoint(const pt: Image32.TPointD): string;
begin
  result := format('%1.1f,%1.1f ', [pt.X, pt.Y]);
end;
//------------------------------------------------------------------------------

function Color32Hex6(clr: Cardinal): string;
begin
  result := format('%.6x', [clr and $FFFFFF]);
end;
//------------------------------------------------------------------------------

procedure WriteCBezierPathToSvgStringStream(ss: TStringStream;
  const path: TArrayOfArrayOfPointD; index: integer);
var
  i,j,k, highI,highJ: integer;
  prevOp: Char;
begin
  highI := High(path);
  if (highI < 0) then Exit;
  ss.WriteString(Format('  <path class="style_%3.3d"  d=" ',[index]));
  for i := 0 to highI do
  begin
    highJ := High(path[i]);
    if (highJ < 1) or (highJ mod 3 > 0) then Continue;
    ss.WriteString('M ' + formatPoint(path[i][0]));
    prevOp := 'M';
    for j := 0 to (highJ div 3) -1 do
    begin
      k := j*3;
      if PointsEqual(path[i][k], path[i][k+1]) and
        PointsEqual(path[i][k+2], path[i][k+3]) then
      begin
        if prevOp <> 'L' then ss.WriteString('L ');
        ss.WriteString(formatPoint(path[i][k+3]));
        prevOp := 'L';
      end else
      begin
        if prevOp <> 'C' then ss.WriteString('C ');
        ss.WriteString(formatPoint(path[i][k+1]) +
          formatPoint(path[i][k+2]) + formatPoint(path[i][k+3]));
        prevOp := 'C';
      end;
    end;
  end;
  ss.WriteString('Z" />'#10);
end;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetFileSize(const Filename: string): Int64;
var
  sr: TSearchRec;
begin
  Result := 0;
  if findfirst(Filename, faAnyFile, sr) = 0 then
    result := sr.Size;
  findClose(sr);
end;
//------------------------------------------------------------------------------

procedure MakePartialFullyTransparent(img: TImage32);
var
  i: integer;
  pc: PARGB;
begin
  pc := PARGB(img.PixelBase);
  for i := 0 to img.Width * img.Height -1 do
  begin
    if pc.A <= $D8 then
      pc.A := $0;
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

function GetBackgroundColor(img: TImage32): TColor32;
var
  cs: array[0..3] of integer; //corners
begin
  //assumes one of the corner colors is the background color when at least
  //3 out of the 4 corners have this color or very nearly thid color
  cs[0] := 0;
  cs[1] := img.Width -1;
  cs[2] := img.Width * img.Height -1;
  cs[3] := img.Width * (img.Height -1);
  Result := clNone32;
  if GetColorDistance(img.Pixels[cs[0]], img.Pixels[cs[1]]) < $20 then
  begin
    if (GetColorDistance(img.Pixels[cs[0]], img.Pixels[cs[2]]) < $20) or
      (GetColorDistance(img.Pixels[cs[0]], img.Pixels[cs[2]]) < $20) then
        Result := img.Pixels[0]
    else if (GetColorDistance(img.Pixels[cs[1]], img.Pixels[cs[2]]) < $20) or
      (GetColorDistance(img.Pixels[cs[1]],img.Pixels[cs[3]]) < $20) then
      Result := img.Pixels[1];
  end
  else if GetColorDistance(img.Pixels[cs[2]],img.Pixels[cs[3]]) < $20 then
  begin
    if (GetColorDistance(img.Pixels[cs[2]],img.Pixels[cs[0]]) < $20) or
      (GetColorDistance(img.Pixels[cs[2]],img.Pixels[cs[1]]) < $20) then
        Result := img.Pixels[2]
    else if (GetColorDistance(img.Pixels[cs[3]],img.Pixels[cs[0]]) < $20) or
      (GetColorDistance(img.Pixels[cs[3]],img.Pixels[cs[1]]) < $20) then
        Result := img.Pixels[3];
  end;
end;
//------------------------------------------------------------------------------

procedure EraseBackgroundIncludeHolesInside(img: TImage32; tolerance: integer);
var
  mask: TArrayOfByte;
  pp: TArrayOfArrayOfPointD;
  bkgndClr: TColor32;
  i: integer;
  p: PColor32;
begin
  bkgndClr := GetBackgroundColor(img);
  if bkgndClr = clNone32 then Exit;
  p := img.PixelBase;
  for i := 0 to img.Width * img.Height -2 do
  begin
    if (p^ shr 24 = $FF) and (GetColorDistance(p^, bkgndClr) < tolerance) then
    begin
      mask := GetFloodFillMask(img,
        i mod img.Width , i div img.Width, CompareRGB, tolerance);
      pp := VectorizeMask(mask, img.Width);
      Erase(img, pp, Image32_Vector.frEvenOdd);
    end;
    inc(p);
  end;
end;
//------------------------------------------------------------------------------

procedure EraseBackgroundFromEdgesOnly(img: TImage32; tolerance: integer);
var
  bkgndClr: TColor32;
  i: integer;
  p: PColor32;

  procedure EraseBackGndInternal(x,y: integer);
  var
    mask: TArrayOfByte;
    pp: TArrayOfArrayOfPointD;
  begin
    mask := GetFloodFillMask(img, x , y, CompareRGB, tolerance);
    pp := VectorizeMask(mask, img.Width);
    pp := InflatePolygons(pp, 0.5);
    Erase(img, pp, Image32_Vector.frEvenOdd);
  end;

begin
  bkgndClr := GetBackgroundColor(img);
  if bkgndClr = clNone32 then Exit;

  p := img.PixelBase; //left edge
  for i := 0 to img.Height -1 do
  begin
    if (p^ shr 24 = $FF) and (GetColorDistance(p^, bkgndClr) < tolerance) then
      EraseBackGndInternal(0, i);
    inc(p, img.width);
  end;

  p := img.PixelBase; inc(p, img.width -1); //right edge
  for i := 0 to img.Height -1 do
  begin
    if (p^ shr 24 = $FF) and (GetColorDistance(p^, bkgndClr) < tolerance) then
      EraseBackGndInternal(img.width -1, i);
    inc(p, img.width);
  end;

  p := img.PixelBase; //top edge
  for i := 0 to img.Width -1 do
  begin
    if (p^ shr 24 = $FF) and (GetColorDistance(p^, bkgndClr) < tolerance) then
      EraseBackGndInternal(i, 0);
    inc(p);
  end;

  p := img.PixelBase; inc(p, img.width * (img.Height -1));  //bottom edge
  for i := 0 to img.Width -1 do
  begin
    if (p^ shr 24 = $FF) and (GetColorDistance(p^, bkgndClr) < tolerance) then
      EraseBackGndInternal(i, img.Height -1);
    inc(p);
  end;
end;

//------------------------------------------------------------------------------
// TMainForm methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  smoothness := 2;
  LoadIniSettings;
  DragAcceptFiles(Handle, True);
  lblRaster.Left := DPI(16);
  UpdateOptions(true);

  //SETUP THE 2 DISPLAY PANELS
  pnlRaster.BorderWidth := DPI(16);
  pnlSVG.BorderWidth := DPI(16);
  //BevelInner := bvLowered;     //set in IDE
  //TabStop := true;             //set in IDE (for keyboard controls)
  pnlRaster.FocusedColor := clGradientInactiveCaption;
  pnlSVG.FocusedColor := clGradientInactiveCaption;
  pnlRaster.BitmapProperties.Scale := 1;
  pnlSVG.BitmapProperties.Scale := 1;
  //enable image transparency - at least, as far as the panel background ;)
  pnlRaster.Bitmap.PixelFormat := pf32bit;
  pnlSVG.Bitmap.PixelFormat := pf32bit;
  //and set initial image scale
  pnlRaster.BitmapProperties.ScaleType := stFit;
  pnlSVG.BitmapProperties.ScaleType := stFit;
  pnlSVG.BitmapProperties.OnKeyDown := SvgPnlKeydown;

  palVectorsList  := TList.Create;
  svgStringStream := TStringStream.Create;
  rasterImg       := TImage32.Create;
  svgImg         := TImage32.Create;

  DisplayImages;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
  ClearPalVectors;
  svgStringStream.Free;
  palVectorsList.Free;
  rasterImg.Free;
  svgImg.Free;
end;
//------------------------------------------------------------------------------

function StringToBoolean(const s: string): Boolean;
begin
  Result := (length(s) > 0) and CharInSet(s[1], ['1','y','Y','t','T']);
end;
//------------------------------------------------------------------------------

procedure TMainForm.LoadIniSettings;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    mnuManualPaletteSizes.Checked :=
      not StringToBoolean(ini.ReadString('Setup', 'AutoAssignPaletteSize', 'Y'));
    maximumColors :=
      Max(8,(Min(512,ini.ReadInteger('Setup', 'MaximumColors', 256))));
    ImageMargin := Max(0,(Min(200,ini.ReadInteger('Setup', 'ImageMargin', 0))));
  finally
    ini.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.WMDropFiles(var msg: TWMDropFiles);
var
  dropHdl: HDROP;
  len: integer;
  filename: string;
begin
  dropHdl := msg.Drop;
  len := DragQueryFile(dropHdl, 0, nil, 0);
  SetLength(filename, len);
  DragQueryFile(dropHdl, 0, PChar(FileName), len + 1);
  OpenDialog1.InitialDir := ExtractFilePath(filename);
  OpenDialog1.FileName := filename;
  if TImage32.IsRegisteredFormat(ExtractFileExt(filename)) then
  begin
    DoOpenFile(filename, false);
  end;
  DragFinish(dropHdl);
  msg.Result := 0;     //ie message has been handled
end;
//------------------------------------------------------------------------------

type
  PPathsNColor = ^TPathsNColor;
  TPathsNColor = record
    paths: TArrayOfArrayOfPointD;
    color: TColor32;
  end;

//------------------------------------------------------------------------------

procedure TMainForm.ClearPalVectors;
var
  i: integer;
begin
  for i := 0 to palVectorsList.Count -1 do
    Dispose(PPathsNColor(palVectorsList[i]));
  palVectorsList.Clear;
end;
//------------------------------------------------------------------------------

procedure TMainForm.BuildVectorListFromMonochromeImage(tmpMasterImg: TImage32; scale: double);
var
  pp: TArrayOfArrayOfPointD;
  pnc: PPathsNColor;
begin
  svgStringStream.Clear;
  ClearPalVectors;

  //get the raw vectors (either using transparency or filtering on black)
  if tmpMasterImg.HasTransparency then
    pp := Vectorize(tmpMasterImg, $FF000000, CompareAlpha, $80) else
    pp := Vectorize(tmpMasterImg, $FF000000, CompareRGB, $80);

  pp := RamerDouglasPeucker(pp, smoothness * 1.5 * scale);
  pp := SmoothLine(pp, true, smoothness * 1.5 * scale, scale);
  new(pnc);
  pnc.paths := pp;
  pnc.color := clBlack32;
  palVectorsList.Add(pnc);
end;
//------------------------------------------------------------------------------

procedure TMainForm.BuildVectorListFromColorDrawing(tmpMasterImg: TImage32; scale: double);
var
  i,j, shadowCnt: integer;
  pathsAndColor: PPathsNColor;
  pal: TArrayOfColor32;
  palFrequencies: TArrayOfInteger;
  pp, shadow, black, opaque: TArrayOfArrayOfPointD;
  shadowImg: TImage32;
const
  nearBlackTolerance = $60;
begin
  //convert palette entres into vectors and store them in palVectorsList.
  //Note that lower palette entries are darker than higher ones and so
  //darker polygons will be drawn before lighter ones, unless order is
  //rearranged (as is the case for black)

  if cancelOp then Exit;
  svgStringStream.Clear;
  ClearPalVectors;
  shadow := nil; opaque := nil; shadowCnt := 0;
  maxColors := Min(maxColors, tmpMasterImg.ColorCount);

  if mnuEraseBackground.Checked then
  begin
    StatusBar1.SimpleText := ' Erasing Background: ' +
      ExtractFilename(OpenDialog1.FileName);
    Application.ProcessMessages;
    EraseBackgroundIncludeHolesInside(tmpMasterImg, $20);
    //alternatively ... EraseBackgroundFromEdges(tmpMasterImg, $20);
  end;

  //VECTORIZE A SEMI-TRANSPARENT 'SHADOW'

  if tmpMasterImg.HasTransparency then
  begin
    StatusBar1.SimpleText := ' Creating Shadow layer: ' +
      ExtractFilename(OpenDialog1.FileName);
    Application.ProcessMessages;

    //vectorize 'shadow' from all pixels with at least 20% opacity
    shadowImg := TImage32.Create(tmpMasterImg);
    try
      shadowImg.ConvertToBoolMask($FF000000, $CC, CompareAlpha, clNone32, clBlack32);
      shadow := Vectorize(shadowImg, clBlack32, CompareAlpha, 0);

      shadowImg.Assign(tmpMasterImg);
      shadowImg.ConvertToBoolMask($FF000000, $0, CompareAlpha, clNone32, clBlack32);
      opaque := Vectorize(shadowImg, clNone32, CompareAlpha, 0);
    finally
      shadowImg.Free;
    end;

    if not Assigned(shadow) then Exit; //ie nothing reasonably opaque
    shadow := RamerDouglasPeucker(shadow, 1.3* scale);
    shadow := SmoothLine(shadow, true, smoothness *2, 1);
    new(pathsAndColor);
    pathsAndColor.paths := shadow;
    pathsAndColor.color := $FFC0C0C0;; //light gray
    palVectorsList.Add(pathsAndColor);
    shadowCnt := 1;
  end;

  //PALETTE

  StatusBar1.SimpleText := ' Creating Palette: ' +
    ExtractFilename(OpenDialog1.FileName);
  Application.ProcessMessages;

  //get ready to separate the image into palette entry colors.
  //assigning extra colors before trimming the least used colors
  Dec(maxColors, shadowCnt);
  pal := MakeAndApplyPalette(tmpMasterImg, maxColors, false, palFrequencies);

  maxColors := length(pal) + shadowCnt;
  pal := TrimPaletteByFraction(pal, palFrequencies, 0.1);
  //ApplyPalette(tmpMasterImg, pal, false);
  //don't bother re-applying the palette, we'll just ignore the deleted colors

  maxColors := length(pal) + shadowCnt;

  if maxColors = 1 then
  begin
    if Assigned(shadow) then
      PPathsNColor(palVectorsList[0]).color := pal[0];
    Exit;
  end;

  //BLACK (WITH GRAYSCALE PALETTE)

  svgImg.Assign(tmpMasterImg);
  svgImg.Grayscale;
  Sharpen(svgImg, 5, 10);
  svgImg.ConvertToBoolMask(clBlack32, $20, CompareRGB, clNone32, clBlack32);
  black := Vectorize(svgImg, clBlack32, CompareAlpha, $0);

  //TIDY UP OPAQUE/TRANSPARENT BOUNDARIES

  MakePartialFullyTransparent(tmpMasterImg);

  if assigned(opaque) then
  begin
    opaque := Image32_Clipper.InflatePolygons(opaque, 2 * scale);
    ErasePolygon(tmpMasterImg, opaque, Image32_Vector.frEvenOdd);
  end;

//  pp := InflatePolygons(black, 0.25 * scale);
//  DrawPolygon(tmpMasterImg, pp, Image32_Vector.frEvenOdd, clBlack32);

  StatusBar1.SimpleText := ' Starting color layers: ' +
    ExtractFilename(OpenDialog1.FileName);
  Application.ProcessMessages;
  if cancelOp then Exit;

  //CREATE AND STORE COLORS (WITH BLACK LAST)

  i := 0;
  while MaxRgbDifference(clBlack32, pal[i+1]) < $20 do inc(i);

  for i := i to high(pal) do
  begin
    ProgressBar.Position := Round( (i * 100) / maxColors);
    application.processmessages;
    if cancelOp then Exit;

    //create a mask for each color in the palatte and vectorize it
    svgImg.Assign(tmpMasterImg);
    svgImg.ConvertToBoolMask(pal[i], 0, CompareRGB, clNone32, clBlack32);
    pp := Vectorize(svgImg, clBlack32, CompareAlpha, $0);

    for j := high(pp) downto 0 do
      if Abs(Image32_Vector.Area(pp[j])) < 2 then
        Delete(pp, j, 1);
    if pp = nil then Continue;

    pp := RamerDouglasPeucker(pp, 1.35 * scale);
    pp := Image32_Clipper.InflatePolygons(pp, 2.8 * scale);
    pp := SmoothLine(pp, true, 2 * scale, 1);

    new(pathsAndColor);
    pathsAndColor.paths := pp;
    pathsAndColor.color := pal[i];
    palVectorsList.Add(pathsAndColor);
  end;

  black := Image32_Clipper.InflatePolygons(black, 0.95 * scale);
  black := RamerDouglasPeucker(black, (1 + smoothness * 0.1) * scale);
  black := Image32_Clipper.InflatePolygons(black, -0.75 * scale);
  black := SmoothLine(black, true, (smoothness +0.75) * scale, 1);

  new(pathsAndColor);
  pathsAndColor.paths := black;
  pathsAndColor.color := pal[0];
  palVectorsList.Add(pathsAndColor);
end;
//------------------------------------------------------------------------------

procedure TMainForm.BuildSvgFromVectorList(
  newWidth, newHeight: integer; scaling: double);
var
  i: integer;
  pp: TArrayOfArrayOfPointD;
  svgSize: TSize;
begin
  svgStringStream.Clear;
  newWidth := Round(newWidth * scaling);
  newHeight := Round(newHeight * scaling);
  svgSize.cx := Round(ImageMargin*2 + newWidth);
  svgSize.cy := Round(ImageMargin*2 + newHeight);
  //scale up the raster representation of the SVG image (ie imgSvg)
  //otherwise it'll still appear pixelated when zooming in (though
  //of course the SVG file itself will be free of any pixelation).
  rasterScale := Max(1, 3200 div Max(svgSize.cx, svgSize.cy));

  svgImg.SetSize(svgSize.cx * rasterScale, svgSize.cy * rasterScale);

  svgStringStream.WriteString(format(SvgHeader,
    [svgSize.cx, svgSize.cy]));
  svgStringStream.WriteString(SvgDefsBegin);
  for i := 0 to palVectorsList.Count -1 do
  begin
    svgStringStream.WriteString(Format(
      '  .style_%3.3d { fill:#%s; fill-opacity:1.0; '+
      'fill-rule:evenodd; stroke-width:0.0; }'#10,
        [i +1, Color32Hex6(PPathsNColor(palVectorsList[i]).color)]));
  end;
  svgStringStream.WriteString(SvgDefsEnd);

  maxColors := palVectorsList.Count;
  for i := 0 to maxColors -1 do
  begin
    with PPathsNColor(palVectorsList[i])^ do
    begin
      if ImageMargin <> 0 then
        paths := OffsetPath(paths, ImageMargin, ImageMargin);
      if scaling <> 1 then
        paths := Image32_Vector.ScalePath(paths, scaling, scaling);
      WriteCBezierPathToSvgStringStream(svgStringStream, paths, i+1);
      pp := FlattenCBezier(paths);
      pp := ScalePath(pp, rasterScale, rasterScale);
      DrawPolygon(svgImg, pp, Image32_Vector.frEvenOdd, color);
    end;
  end;
  svgStringStream.WriteString(SvgFooter);
end;
//------------------------------------------------------------------------------

procedure TMainForm.DisplayImages;
begin
  pnlRaster.Bitmap.Width := rasterImg.Width;
  pnlRaster.Bitmap.Height := rasterImg.Height;
  pnlRaster.ClearBitmap; //otherwise images will be blended
  rasterImg.CopyToDc(pnlRaster.Bitmap.Canvas.Handle);

  pnlSVG.Bitmap.Width := svgImg.Width;
  pnlSVG.Bitmap.Height := svgImg.Height;
  pnlSVG.ClearBitmap; //otherwise images will be blended
  svgImg.CopyToDc(pnlSVG.Bitmap.Canvas.Handle);
end;

//------------------------------------------------------------------------------
// 2 callback functions for Custom Dialogs - which is currently undocumented :(
//------------------------------------------------------------------------------

procedure TMainForm.CheckboxOnJPGMessageDialog(Sender: TObject; isChecked: Boolean);
begin
  hideJPGMessage := isChecked;
end;
//------------------------------------------------------------------------------

function TMainForm.CheckJpg(const filename: string): Boolean;
var
  messageBoxOpts: TMessageBoxOptions;
begin
  FillChar(messageBoxOpts, sizeof(messageBoxOpts), 0);
  messageBoxOpts.buttonCaptions := ['&Continue', '&Cancel'];
  messageBoxOpts.checkBoxCallBk := CheckboxOnJPGMessageDialog;

  Result := CustomDialogs.MessageBox(self, 'JPG Images',
    jpgWarning, 'Image32 - JPG files', MB_YESNO or MB_ICONWARNING,
    messageBoxOpts) = mrYes;
end;
//------------------------------------------------------------------------------

function TMainForm.GetMaxColors(forceCheck: Boolean): Boolean;
var
  newMaxColors: integer;
  numBoxOpts: TNumBoxOptions;
begin
  newMaxColors := Max(4, Min(maximumColors, maxColors));
  if not forceCheck and not mnuManualPaletteSizes.Checked then
  begin
    maxColors := newMaxColors;
    Result := true;
  end else
  begin
    FillChar(numBoxOpts, sizeof(numBoxOpts), 0);
    numBoxOpts.minVal := 4;
    numBoxOpts.maxVal := 256;

    result := CustomDialogs.NumInputBox(self, 'Maximum Color Count',
      format(colorCountMessage,[maxColors/1.0]),
      'Image32 - Image Color Count', newMaxColors, numBoxOpts);
    if Result then maxColors := newMaxColors;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DoOpenFile(const filename: string; forceColorCheck: Boolean);
var
  oldSvgImg, newRasterImg, newGoldRasterImg: TImage32;
  oldCaption1, oldCaption2: string;
  origMaxColors: integer;
  s: string;
  scaling: double;
begin
  //before proceeding, save copies of masterImg and workImg
  //in case processing of the new image is cancelled.

  screen.Cursor := crHourGlass;
  oldCaption1 := lblRaster.Caption;
  oldCaption2 := lblSVG.Caption;
  oldSvgImg := TImage32.Create(svgImg);
  newRasterImg := TImage32.Create;
  newGoldRasterImg := TImage32.Create;
  try
    lblOptions.Caption := '';
    StatusBar1.SimpleText := ' Opening: ' + ExtractFilename(filename);
    pnlRaster.Bitmap.SetSize(0,0);
    pnlSVG.Bitmap.SetSize(0,0);
    lblRaster.Caption := 'Raster image:';
    lblSVG.Caption := 'SVG image:';
    Application.ProcessMessages;

    isDrawing := true;
    if newRasterImg.LoadFromFile(fileName) and
      not newRasterImg.IsEmpty then
    begin
      //display the just loaded 'tmpImg' in pnlRaster
      pnlRaster.Bitmap.Width := newRasterImg.Width;
      pnlRaster.Bitmap.Height := newRasterImg.Height;
      pnlRaster.ClearBitmap;
      newRasterImg.CopyToDc(pnlRaster.Bitmap.Canvas.Handle);

      pnlRaster.BitmapProperties.ScaleType := stFit;

      origMaxColors := newRasterImg.ColorCount;
      maxColors := origMaxColors;
      if newRasterImg.HasTransparency then inc(maxColors);
      cancelOp := cancelOp or
        ((maxColors > 2) and not GetMaxColors(forceColorCheck)) or
        ((lowercase(ExtractFileExt(filename)) = '.jpg') and
        not hideJPGMessage and not CheckJpg(filename));
    end else
    begin
      cancelOp := true;
      Exit;
    end;

    s := GetTransformOptions(isDrawing);
      if s = '' then
        lblOptions.Caption := 'Building Image:' else
        lblOptions.Caption := 'Building with ' +s +': ';

    ProgressBar.Left := lblOptions.Left + lblOptions.Width  + DPI(16);
    ProgressBar.Width := pnlOptions.ClientWidth - DPI(16) - ProgressBar.Left;
    ProgressBar.Visible := true;
    ProgressBar.Position := 0;

    //large images will be too slow to process sensibly
    if (newRasterImg.Width > 1200) or (newRasterImg.Height > 1200) then
      scaling := Min(1200 / newRasterImg.Width,
        1200 / newRasterImg.Height) else
      scaling := 1;

    lblRaster.Caption :=
      format('Raster: %d x %d; file size: %1.0n; colors: %d.',
      [newRasterImg.Width, newRasterImg.Height,
      GetFileSize(fileName)/1.0, origMaxColors]);

    StatusBar1.SimpleText := ' Transforming: ' + ExtractFilename(filename);
    Application.ProcessMessages;

    if cancelOp then Exit;
    //keep a clean (gold) backup of newRasterImg
    newGoldRasterImg.Assign(newRasterImg);
    newRasterImg.Scale(scaling);

    if (maxColors <= 2) then //or IsGrayscale(newRasterImg.Pixels, $20) then
      BuildVectorListFromMonochromeImage(newRasterImg, scaling) else
      BuildVectorListFromColorDrawing(newRasterImg, scaling);

    Application.ProcessMessages;
    if cancelOp then Exit;

    scaling := 1/scaling;
    BuildSvgFromVectorList(newRasterImg.Width,
      newRasterImg.Height, scaling);

    //all done !!

    pnlSVG.BitmapProperties.ScaleType := stFit;
    //pnlSVG.BitmapProperties.Scale := 1/rasterScale;

    lblSVG.Caption :=
      format('SVG: %1.0n x %1.0n; file size: %1.0n; colors: %d.',
      [newRasterImg.Width * scaling, newRasterImg.Height * scaling,
      svgStringStream.Size/1.0, maxColors]);

    rasterImg.Assign(newGoldRasterImg);
    mnuSaveAs.Enabled := true;

  finally
    UpdateOptions(isDrawing);

    ProgressBar.Visible := false;
    ProgressBar.Position := 0;
    screen.Cursor := crDefault;
    if cancelOp then
    begin
      lblRaster.Caption := oldCaption1;
      lblSVG.Caption := oldCaption2;
      svgImg.Assign(oldSvgImg);
    end;
    cancelOp := false;
    newRasterImg.Free;
    newGoldRasterImg.Free;
    oldSvgImg.Free;
    DisplayImages;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    DoOpenFile(OpenDialog1.FileName, false);
end;
//------------------------------------------------------------------------------

procedure TMainForm.Refresh1Click(Sender: TObject);
begin
  if OpenDialog1.FileName = '' then
    Open1Click(nil) else
    DoOpenFile(OpenDialog1.FileName, true);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuSaveAsClick(Sender: TObject);
begin
  if svgStringStream.Size = 0 then Exit;
  SaveDialog1.InitialDir := OpenDialog1.InitialDir;
  SaveDialog1.FileName :=
    ChangeFileExt(ExtractFilename(OpenDialog1.FileName), '.svg');
  if SaveDialog1.Execute then
  begin
    svgStringStream.SaveToFile(SaveDialog1.FileName);
    StatusBar1.SimpleText :=
      format(' %s saved.',[ExtractFilename(SaveDialog1.FileName)]);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  pnlSVG.Width := ClientWidth div 2;
  lblSVG.Left := pnlSVG.Left + DPI(16);
  lblRaster.Left := DPI(16);
  lblOptions.Left := lblRaster.Left;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuTipsClick(Sender: TObject);
var
  messageBoxOpts: TMessageBoxOptions;
begin
  FillChar(messageBoxOpts, sizeof(messageBoxOpts), 0);
  messageBoxOpts.customIcon := Application.Icon;

  CustomDialogs.MessageBox(self, 'RasterToSVG Tips', menuTips,
    'RasterToSVG', MB_OK, messageBoxOpts);
end;
//------------------------------------------------------------------------------

function TMainForm.GetTransformOptions(isDrawing: Boolean): string;
const
  smooth: array [0..3] of string =
    ('Minimal', 'Normal', 'High', 'Very High');
begin
  if isDrawing then
  begin
    Result := Format(' %s smoothing', [smooth[smoothness -1]]);
    if mnuEraseBackground.Checked then
      Result := Result + ' + Erase Background';
  end
  else
    Result := '';
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateOptions(isDrawing: Boolean);
begin
  lblOptions.Caption := 'Transform Options: ' + GetTransformOptions(isDrawing);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuIniOptionsClick(Sender: TObject);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Ini.WriteBool('Setup', 'AutoAssignPaletteSize',
      not mnuManualPaletteSizes.Checked);
  finally
    ini.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuChangeSmoothnessClick(Sender: TObject);
var
  comboboxOpts: TComboboxOptions;
  num: integer;
begin
  FillChar(comboboxOpts, sizeof(comboboxOpts), 0);
  SetLength(comboboxOpts.comboItems, 4);
  comboboxOpts.comboItems[0] := 'Very high';
  comboboxOpts.comboItems[1] := 'High';
  comboboxOpts.comboItems[2] := 'Normal';
  comboboxOpts.comboItems[3] := 'Minimal';
  num := 4 -smoothness;

  if not CustomDialogs.ComboInputBox(Self, 'Drawing Smoothness', '',
    'RasterToSVG', num, comboboxOpts) or (4 - num = smoothness) then Exit;

  smoothness := 4 - num;
  if OpenDialog1.FileName <> '' then
    DoOpenFile(OpenDialog1.FileName, false) else
    UpdateOptions(true);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuOptionsClick(Sender: TObject);
begin
 UpdateOptions(true);
end;
//------------------------------------------------------------------------------

procedure TMainForm.SvgPnlKeydown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [Ord('1')..Ord('9')] then
  begin
    if Shift = [ssShift] then
      pnlSVG.BitmapProperties.Scale := (Key - Ord('0')) * 0.1/rasterScale else
      pnlSVG.BitmapProperties.Scale := (Key - Ord('0')) * 1/rasterScale;
    pnlSVG.Invalidate;
    Key := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  if ProgressBar.Visible then
    cancelOp := true else
    Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.About1Click(Sender: TObject);
var
  messageBoxOpts: TMessageBoxOptions;
begin
  FillChar(messageBoxOpts, sizeof(messageBoxOpts), 0);
  messageBoxOpts.customIcon := Application.Icon;

  CustomDialogs.MessageBox(self, 'RasterToSVG',
    'Version 1.6'#10+
    'Created by Angus Johnson'#10#10+
    'Open source freeware.'#10+
    'Copyright © 2019 - 2020'#10+
    'Boost Software License 1.0',
    'RasterToSVG', MB_OK, messageBoxOpts);
end;
//------------------------------------------------------------------------------

end.
