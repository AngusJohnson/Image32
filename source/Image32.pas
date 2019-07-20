unit Image32;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.07                                                            *
* Date      :  20 July 2019                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  The core module of the Image32 library                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  Windows, SysUtils, Classes, {$IFDEF UITYPES} UITypes, {$ENDIF} Math;

type
  TColor32 = type Cardinal;

const
  clNone32     = TColor32($00000000);
  clAqua32     = TColor32($FF00FFFF);
  clBlack32    = TColor32($FF000000);
  clBlue32     = TColor32($FF0000FF);
  clFuchsia32  = TColor32($FFFF00FF);
  clGray32     = TColor32($FF7F7F7F);
  clGreen32    = TColor32($FF008000);
  clLime32     = TColor32($FF00FF00);
  clMaroon32   = TColor32($FF800000);
  clNavy32     = TColor32($FF000080);
  clOlive32    = TColor32($FF7F7F00);
  clOrange32   = TColor32($FFFF7F00);
  clPurple32   = TColor32($FF7F00FF);
  clRed32      = TColor32($FFFF0000);
  clSilver32   = TColor32($FFC0C0C0);
  clTeal32     = TColor32($FF007F7F);
  clWhite32    = TColor32($FFFFFFFF);
  clYellow32   = TColor32($FFFFFF00);

type
  TClipboardPriority = (cpLow, cpMedium, cpHigh);

  PColor32 = ^TColor32;
  TArrayOfColor32 = array of TColor32;
  TArrayOfInteger = array of Integer;
  TArrayOfByte = array of Byte;

  TImage32 = class;
  TImageFormatClass = class of TImageFormat;

  //TImageFormat: Abstract base class for loading and saving images in TImage32.<br>
  //This class is overridden to provide support for separate
  //file storage formats (eg BMP, PNG & JPG).<br>
  //Derived classes register with TImage32 using TImage32.RegisterImageFormatClass.
  TImageFormat = class
    procedure SaveToStream(stream: TStream; img32: TImage32); virtual; abstract;
    function SaveToFile(const filename: string; img32: TImage32): Boolean; virtual;
    function LoadFromStream(stream: TStream; img32: TImage32): Boolean; virtual; abstract;
    function LoadFromFile(const filename: string; img32: TImage32): Boolean; virtual;
    function CopyToClipboard(img32: TImage32): Boolean; virtual; abstract;
    function PasteFromClipboard(img32: TImage32): Boolean; virtual; abstract;
  end;

  //TBlendFunction: Function template for TImage32.CopyFrom.
  TBlendFunction = function(bgColor, fgColor: TColor32): TColor32;

  TImage32 = class
  private
    fWidth: integer;
    fHeight: Integer;
    fAntiAliase: Boolean;
    fIsPremultiplied: Boolean;
    fPixels: TArrayOfColor32;
    function GetPixel(x,y: Integer): TColor32;
    procedure SetPixel(x,y: Integer; color: TColor32);
    function GetWeightedPixel(x256, y256: Integer): TColor32;
    function GetIsEmpty: Boolean;
    function GetPixelBase: PColor32;
    function GetPixelRow(row: Integer): PColor32;
    procedure DoResizeAA(newWidth, newHeight: Integer);
    procedure DoResize(newWidth, newHeight: Integer);
    procedure RotateLeft90;
    procedure RotateRight90;
    procedure Rotate180;
    function GetColorCount: Integer;
    function GetHasTransparency: Boolean;
    function GetBounds: TRect;
  protected
    function CopyPixels(rec: TRect): TArrayOfColor32;
    //CopyInternal: Internal routine (has no bounds checking)
    procedure CopyInternal(src: TImage32;
      const srcRec, dstRec: TRect; blendFunc: TBlendFunction);
    procedure BlurHorizontal(rect: TRect; radius: Integer);
    procedure BlurVertical(rect: TRect; radius: Integer);
  public
    constructor Create(width: Integer = 0; height: Integer = 0); overload;
    constructor Create(src: TImage32); overload;
    constructor Create(src: TImage32; const srcRec: TRect); overload;
    destructor Destroy; override;

    procedure Assign(src: TImage32);
    procedure AssignTo(dst: TImage32);
    //SetSize: Erases any current image, and fills with the specified color.
    procedure SetSize(newWidth, newHeight: Integer; color: TColor32 = 0);
    //Resize: Unlike SetSize, resize will not eraze any existing image. The
    //existing image will either be stretched or cropped depending on the
    //stretchImage parameter.
    procedure Resize(newWidth, newHeight: Integer; stretchImage: Boolean = true);
    procedure Scale(sx, sy: single);
    //CopyFrom: Copies part or all of an image from another TImage32 object
    //(src). If no blend function is provided, then the pixels in src's srcRec
    //simply replace whatever is in 'dstRec'. If a blend function is provided,
    //then that functions determines how the image from src will be blended
    //with the current image. Also the image from src's srcRec will be stretch
    //resized if necessary to fit into dstRec.
    function CopyFrom(src: TImage32; srcRec, dstRec: TRect;
      blendFunc: TBlendFunction = nil): Boolean;
    //CopyFromDC: Copies an image from a Window's device context, erasing
    //any current image in TImage32. (eg copying from TBitmap.canvas.handle)
    procedure CopyFromDC(srcDc: HDC; width, height: Integer);
    //CopyToDc: Copies the image into a Window's device context
    procedure CopyToDc(dstDc: HDC; x: Integer = 0; y: Integer = 0; 
      transparent: Boolean = true; bkColor: TColor32 = 0);
    function CopyToClipBoard: Boolean;
    function PasteFromClipBoard: Boolean;
    procedure Crop(const rec: TRect);
    //SetBackgroundColor: Assumes the current image is semi-transparent.
    procedure SetBackgroundColor(bgColor: TColor32);
    //HatchBackground: Assumes the current image is semi-transparent.
    procedure HatchBackground(color1: TColor32 = clWhite32;
      color2: TColor32= clSilver32; hatchSize: Integer = 10);
    //Clear: Fills the entire image with the specified color
    procedure Clear(color: TColor32 = 0);
    procedure FillRect(rec: TRect; color: TColor32);
    procedure FlipVertical;
    procedure FlipHorizontal;
    procedure PreMultiply;
    //SetAlpha: Sets 'alpha' to the alpha byte of every pixel in the image
    procedure SetAlpha(alpha: Byte);
    //Grayscale: Only changes color channels. The alpha channel is untouched.
    procedure Grayscale;
    procedure InvertColors;
    procedure InvertAlphas;
    //ColorToAlpha: Removes the specified color from the image, even from
    //pixels that are a blend of colors including the specified color.<br>
    //see https://stackoverflow.com/questions/9280902/
    procedure EraseColor(color: TColor32; ExactMatchOnly: Boolean = false);

    procedure AdjustHue(percent: Integer);         //ie +/- 100%
    procedure AdjustLuminance(percent: Integer);   //ie +/- 100%
    procedure AdjustSaturation(percent: Integer);  //ie +/- 100%

    //CropTransparentPixels: Trim transparent edges until each edge contains
    //at least one opaque or semi-opaque pixel.
    procedure CropTransparentPixels;
    procedure Rotate(angleRads: single);
    //RotateRect: Rotates part of an image, but also clips those parts of the
    //rotated image that fall outside rec. The eraseColor parameter indicates
    //the color to fill those uncovered pixels in rec following rotation.
    procedure RotateRect(const rec: TRect;
      angleRads: single; eraseColor: TColor32 = 0);
    procedure Skew(dx,dy: double);

    //BoxBlur: With several repetitions and a smaller radius, BoxBlur can
    //achieve a close approximation of a GaussianBlur, and it's faster.
    procedure BoxBlur(rect: TRect; radius, repeats: Integer);
    procedure GaussianBlur(rec: TRect; radius: Integer);
    //Emboss: A smaller radius is sharper. Increasing depth increases contrast.
    //Luminance changes grayscale balance (unless preserveColor = true)
    procedure Emboss(radius: Integer = 1; depth: Integer = 10;
      luminance: Integer = 75; preserveColor: Boolean = false);
    //ScaleAlpha: Scales the alpha byte of every pixel by the specified amount.
    procedure ScaleAlpha(scale: double);
    //Sharpen: Radius range is 1 - 10; amount range is 1 - 100.<br>
    //see https://en.wikipedia.org/wiki/Unsharp_masking
    procedure Sharpen(radius: Integer = 2; amount: Integer = 10);
    //RegisterImageFormatClass: Registers a TImageFormatClass with TImage32 and
    //associates it with a specific 3 character file extension (eg BMP).
    class procedure RegisterImageFormatClass(ext: string;
      bm32ExClass: TImageFormatClass; clipPriority: TClipboardPriority);
    class function GetImageFormatClass(const ext: string): TImageFormatClass;
    //GetExtFromImageStream: Returns the stream's format as a three character
    //string (eg BMP, PNG, JPG).
    class function GetExtFromImageStream(stream: TStream): string;
    //IsRegisteredFormat: Returns true if 'ext' (a three character string) is
    //registered with TImage32 as a storage format.<br> See also
    //TImage32.RegisterImageFormatClass.
    class function IsRegisteredFormat(const ext: string): Boolean;
    //SaveToFile: Will fail if filename's extension hasn't been registered
    //as a supported storage format.<br>See TImage32.RegisterImageFormatClass.
    function SaveToFile(const filename: string): Boolean;
    function SaveToStream(stream: TStream; const FmtExt: string): Boolean;
    //LoadFromFile: Requires a TImageFormat registered with TImage32 that's
    //associated with filename's extension.<br>
    //See TImage32.RegisterImageFormatClass.
    function LoadFromFile(const filename: string): Boolean;
    function LoadFromStream(stream: TStream; const FmtExt: string = ''): Boolean;
    function LoadFromResource(const resName, resType: string): Boolean;

    //properties ...

    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    //Bounds: Result := Rect(0, 0, Width, Height);
    property Bounds: TRect read GetBounds;
    property IsEmpty: Boolean read GetIsEmpty;
    property Pixel[x,y: Integer]: TColor32 read GetPixel write SetPixel;
    property Pixels: TArrayOfColor32 read fPixels;
    property PixelBase: PColor32 read GetPixelBase;
    property PixelRow[row: Integer]: PColor32 read GetPixelRow;
    property ColorCount: Integer read GetColorCount;
    //HasTransparency: Returns true if any pixel's alpha byte < 255.
    property HasTransparency: Boolean read GetHasTransparency;
    //AntiAliased: Antialiasing is used in scaling and rotation transforms
    property AntiAliased: Boolean read fAntiAliase write fAntiAliase;
  end;

  PARGB = ^TARGB;
  TARGB = packed record
    case boolean of
      false: (B: Byte; G: Byte; R: Byte; A: Byte);
      true : (Color: TColor32);
  end;
  TArrayOfARGB = array of TARGB;
  PArgbArray = ^TArrayOfARGB;

  THsl = packed record
    hue  : byte;
    sat  : byte;
    lum  : byte;
    alpha: byte;
  end;

  PPointD = ^TPointD;
  TPointD = record X, Y: double; end;
  TArrayOfPointD = array of TPointD;
  TArrayOfArrayOfPointD = array of TArrayOfPointD;

  //TPath = TArrayOfPointD;          //may cause ambiguity if
  //TPaths = TArrayOfArrayOfPointD;  //also using Clipper.pas

  TArrayOfDouble = array of double;

  TPathD = array of TPointD;

  TRectD = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    Left, Top, Right, Bottom: double;
    function IsEmpty: Boolean;
    function Width: double;
    function Height: double;
    //Normalize: Returns True if either top & bottom or left & right swap
    function Normalize: Boolean;
  end;

  {$IFNDEF PBYTE}
  PByte = type PChar;
  {$ENDIF}

  PWeightedColor = ^TWeightedColor;
  TWeightedColor = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
  private
    fAddCount : Integer;
    fAlphaTot : Int64;
    fColorTotR: Int64;
    fColorTotG: Int64;
    fColorTotB: Int64;
    function GetColor: TColor32;
  public
    procedure Reset;
    procedure Add(c: TColor32; weight: Integer);
    procedure Subtract(c: TColor32; weight: Integer);
    procedure AddWeight(weight: Integer);
    property AddCount: Integer read fAddCount;
    property Color: TColor32 read GetColor;
  end;

  //BLEND FUNCTIONS ( see TImage32.CopyFrom() )

  //BlendToOpaque: Blends a semi-transparent image onto an opaque background
  function BlendToOpaque(bgColor, fgColor: TColor32): TColor32;
  //BlendToAlpha: Blends two semi-transparent images (slower than BlendToOpaque)
  function BlendToAlpha(bgColor, fgColor: TColor32): TColor32;
  //BlendMask: Whereever the mask is, preserves the background
  function BlendMask(bgColor, mask: TColor32): TColor32;
  {$IFDEF INLINE} inline; {$ENDIF}
  //BlendMaskInverted: Whereever the mask is, removes the background
  function BlendMaskInverted(bgColor, mask: TColor32): TColor32;
  {$IFDEF INLINE} inline; {$ENDIF}

  //MISCELLANEOUS FUNCTIONS ...

  //Color32: Converts Graphics.TColor values into TColor32 values.
  function Color32(rgbColor: Integer): TColor32;
  //RgbtoHsl: See https://en.wikipedia.org/wiki/HSL_and_HSV
  function RgbtoHsl(color: TColor32): THsl;
  //HslToRgb: See https://en.wikipedia.org/wiki/HSL_and_HSV
  function HslToRgb(hslColor: THsl): TColor32;
  function AdjustHue(color: TColor32; percent: Integer): TColor32;

  function PointD(const X, Y: Double): TPointD; overload;
  function PointD(const pt: TPoint): TPointD; overload;

  function RectD(left, top, right, bottom: double): TRectD; overload;
  function RectD(const rec: TRect): TRectD; overload;

  //Rad: Converts degrees to radians.
  function Rad(angleDegrees: double): double;
  {$IFDEF INLINE} inline; {$ENDIF}

  //DPI: Useful for DPIAware sizing of images and their container controls.<br>
  //Scales values relative to the display's resolution (PixelsPerInch).<br>
  //See https://docs.microsoft.com/en-us/windows/desktop/hidpi/high-dpi-desktop-application-development-on-windows
  function DPI(val: Integer): Integer; overload;
  {$IFDEF INLINE} inline; {$ENDIF}
  function DPI(val: double): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}

const
  angle180 = Pi;
  angle360 = Pi *2;
  angle15  = Pi /12;
  angle30  = angle15 *2;
  angle45  = angle15 *3;
  angle60  = angle15 *4;
  angle75  = angle15 *5;
  angle90  = Pi /2;
  angle105 = angle180 - angle75;
  angle120 = angle180 - angle60;
  angle135 = angle180 - angle45;
  angle150 = angle180 - angle30;
  angle165 = angle180 - angle15;
  angle195 = angle180 + angle15;
  angle210 = angle180 + angle30;
  angle225 = angle180 + angle45;
  angle240 = angle180 + angle60;
  angle255 = angle180 + angle75;
  angle270 = angle360 - angle90;
  angle285 = angle360 - angle75;
  angle300 = angle360 - angle60;
  angle315 = angle360 - angle45;
  angle330 = angle360 - angle30;
  angle345 = angle360 - angle15;

var
  //Both MulTable and DivTable are used in blend functions<br>
  //MulTable[a,b] = a * b / 255
  MulTable: array [Byte,Byte] of Byte; 
  //DivTable[a,b] = a * 255/b (for a &lt;= b)
  DivTable: array [Byte,Byte] of Byte; 

  ScreenPixelsY: Integer;
  //DPI: Useful for DPIAware sizing of images and their container controls.<br>
  //Scales values relative to the display's resolution (PixelsPerInch).
  DpiScale: double;

  //AND BECAUSE OLDER DELPHI COMPILERS (OLDER THAN D2006)
  //DON'T SUPPORT RECORD METHODS

  function RectWidth(const rec: TRect): Integer;
  {$IFDEF INLINE} inline; {$ENDIF}
  function RectHeight(const rec: TRect): Integer;
  {$IFDEF INLINE} inline; {$ENDIF}

  function IsEmptyRect(const rec: TRect): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}

implementation

var
  ImageFormatClassList: TStringList; //list of supported file extensions

const
  div255 : Double = 1 / 255;
  div6   : Double = 1 / 6;
  MaxBlur = 20;
type
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0.. maxint div SizeOf(TColor32) -1] of TColor32;

  TArrayofHSL = array of THsl;

  TArrayOfWeightedColor = array of TWeightedColor;
  PWeightedColorArray = ^TWeightedColorArray;
  TWeightedColorArray = array [0.. $FFFFFF] of TWeightedColor;

  TByteArray = array[0..MaxInt -1] of Byte;
  PByteArray = ^TByteArray;

//------------------------------------------------------------------------------
// Blend functions - used by TImage32.CopyFrom()
//------------------------------------------------------------------------------

function BlendToOpaque(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
  fw,bw: PByteArray;
begin
  if fg.A = 0 then Result := bgColor
  else if fg.A = 255 then Result := fgColor
  else
  begin
    //assuming bg.A = 255, use just fg.A for color weighting
    res.A := 255;
    fw := @MulTable[fg.A];         //ie weight of foreground
    bw := @MulTable[not fg.A];     //ie weight of foreground
    res.R := fw[fg.R] + bw[bg.R];
    res.G := fw[fg.G] + bw[bg.G];
    res.B := fw[fg.B] + bw[bg.B];
  end;
end;
//------------------------------------------------------------------------------

function BlendToAlpha(bgColor, fgColor: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute fgColor;
  fgWeight: byte;
  R, InvR: PByteArray;
begin
  //(see https://en.wikipedia.org/wiki/Alpha_compositing)
  if fg.A = 0 then Result := bgColor
  else if fg.A = 255 then Result := fgColor
  else
  begin
    //combine alphas ...
    res.A := not MulTable[not fg.A, not bg.A];
    fgWeight := DivTable[fg.A, res.A]; //fgWeight = amount foreground color
                                       //contibutes to total (result) color

    R     := @MulTable[fgWeight];      //ie weight of foreground
    InvR  := @MulTable[not fgWeight];  //ie weight of foreground
    res.R := R[fg.R] + InvR[bg.R];
    res.G := R[fg.G] + InvR[bg.G];
    res.B := R[fg.B] + InvR[bg.B];
  end;
end;
//------------------------------------------------------------------------------

function BlendMask(bgColor, mask: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute mask;
begin
  Result := bgColor;
  res.A := MulTable[bg.A, fg.A];
  if res.A = 0 then Result := 0;
end;
//------------------------------------------------------------------------------

function BlendMaskInverted(bgColor, mask: TColor32): TColor32;
var
  res: TARGB absolute Result;
  bg: TARGB absolute bgColor;
  fg: TARGB absolute mask;
begin
  Result := bgColor;
  res.A := MulTable[bg.A, 255 - fg.A];
  if res.A < 2 then Result := 0;
end;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

{$IFNDEF UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

function RectWidth(const rec: TRect): Integer;
begin
  Result := rec.Right - rec.Left;
end;
//------------------------------------------------------------------------------

function RectHeight(const rec: TRect): Integer;
begin
  Result := rec.Bottom - rec.Top;
end;
//------------------------------------------------------------------------------

function IsEmptyRect(const rec: TRect): Boolean;
begin
  Result := (rec.Right <= rec.Left) or (rec.Bottom <= rec.Top);
end;
//------------------------------------------------------------------------------

function Color32(rgbColor: Integer): TColor32;
var
  res: TARGB absolute Result;
begin
  if rgbColor < 0 then
    result := GetSysColor(-rgbColor) else
    result := rgbColor;
  res.A := res.B; res.B := res.R; res.R := res.A; //byte swap
  res.A := 255;
end;
//------------------------------------------------------------------------------

function AdjustHue(color: TColor32; percent: Integer): TColor32;
var
  hsl: THsl;
begin
  percent := percent mod 100;
  if percent < 0 then inc(percent, 100);
  hsl := RgbtoHsl(color);
  hsl.hue := (hsl.hue + Round(percent*255/100)) mod 256;
  result := HslToRgb(hsl);
end;
//------------------------------------------------------------------------------

function GetCompatibleMemDc(wnd: HWnd = 0): HDC;
var
  dc: HDC;
begin
  dc := Windows.GetDC(wnd);
  try
    Result := CreateCompatibleDC(dc);
  finally
    Windows.ReleaseDC(wnd, dc);
  end;
end;
//------------------------------------------------------------------------------

function GetBitmapInfoHeader(width, height: Integer): TBitmapInfoHeader;
begin
  FillChar(Result, sizeof(Result), #0);
  Result.biSize := sizeof(TBitmapInfoHeader);
  Result.biWidth := width;
  Result.biHeight := height;
  Result.biPlanes := 1;
  Result.biBitCount := 32;
  Result.biSizeImage := width * height * SizeOf(TColor32);
  Result.biCompression := BI_RGB;
end;
//------------------------------------------------------------------------------

function RectsEqual(const rec1, rec2: TRect): Boolean;
begin
  result := (rec1.Left = rec2.Left) and (rec1.Top = rec2.Top) and
    (rec1.Right = rec2.Right) and (rec1.Bottom = rec2.Bottom);
end;
//------------------------------------------------------------------------------

function Rad(angleDegrees: double): double;
begin
  Result := angleDegrees * Pi/180;
end;
//------------------------------------------------------------------------------

function DPI(val: Integer): Integer;
begin
  result := Round( val * DpiScale);
end;
//------------------------------------------------------------------------------

function DPI(val: double): double;
begin
  result := val * DpiScale;
end;
//------------------------------------------------------------------------------

function ClampByte(val: Integer): byte;
begin
  if val < 0 then result := 0
  else if val > 255 then result := 255
  else result := val;
end;
//------------------------------------------------------------------------------

function ClampRange(val, min, max: Integer): Integer; overload;

begin
  if val < min then result := min
  else if val > max then result := max
  else result := val;
end;
//------------------------------------------------------------------------------

function ClampRange(val, min, max: single): single; overload;

begin
  if val < min then result := min
  else if val > max then result := max
  else result := val;
end;
//------------------------------------------------------------------------------

procedure ScaleRect(var rec: TRect; scale: TPointD);
begin
  rec.Right := rec.Left + Round((rec.Right - rec.Left) * scale.X);
  rec.Bottom := rec.Top + Round((rec.Bottom - rec.Top) * scale.Y);
end;
//------------------------------------------------------------------------------

function IncPColor32(pc: PColor32; cnt: Integer): PColor32;

begin
  result := PColor32(PByte(pc) + cnt * SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

function IncPWeightColor(pwc: PWeightedColor; cnt: Integer): PWeightedColor;

begin
  result := PWeightedColor(PByte(pwc) + cnt * SizeOf(TWeightedColor));
end;
//------------------------------------------------------------------------------

function DivRound(num, denom: Integer): Byte;

begin
  result := ClampByte((num  + (denom div 2)) div denom);
end;
//------------------------------------------------------------------------------

function PointD(const X, Y: Double): TPointD;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function PointD(const pt: TPoint): TPointD;
begin
  Result.X := pt.X;
  Result.Y := pt.Y;
end;
//------------------------------------------------------------------------------

procedure RotatePt(var pt: TPointD; const origin: TPointD; sinA, cosA: double);
var
  tmpX, tmpY: double;
begin
  tmpX := pt.X-origin.X;
  tmpY := pt.Y-origin.Y;
  pt.X := tmpX * cosA - tmpY * sinA + origin.X;
  pt.Y := tmpX * sinA + tmpY * cosA + origin.Y;
end;
//------------------------------------------------------------------------------

function GetRotatedRectD(const rec: TRectD; angleRad: double): TRectD;
var
  i: Integer;
  sinA, cosA: extended;
  cp: TPointD;
  pts: TPathD;
begin
  setLength(pts, 4);
  Math.SinCos(-angleRad, sinA, cosA);
  cp.X := (rec.Right + rec.Left) / 2;
  cp.Y := (rec.Bottom + rec.Top) / 2;
  pts[0] := PointD(rec.Left, rec.Top);
  pts[1] := PointD(rec.Right, rec.Top);
  pts[2] := PointD(rec.Left, rec.Bottom);
  pts[3] := PointD(rec.Right, rec.Bottom);
  for i := 0 to 3 do RotatePt(pts[i], cp, sinA, cosA);
  result.Left := pts[0].X;
  result.Right := result.Left;
  result.Top := pts[0].Y;
  result.Bottom := result.Top;
  for i := 1 to 3 do
  begin
    if pts[i].X < result.Left then result.Left := pts[i].X;
    if pts[i].Y < result.Top then result.Top := pts[i].Y;
    if pts[i].X > result.Right then result.Right := pts[i].X;
    if pts[i].Y > result.Bottom then result.Bottom := pts[i].Y;
  end;
end;
//------------------------------------------------------------------------------

function GetWeightedColor(const srcBits: TArrayOfColor32;
  x256, y256, xx256, yy256, maxX: Integer): TColor32;
var
  i, j, xi, yi, xxi, yyi, weight: Integer;
  xf, yf, xxf, yyf: cardinal;
  color: TWeightedColor;
begin
  color.Reset;

  xi := x256 shr 8; xf := x256 and $FF;
  yi := y256 shr 8; yf := y256 and $FF;
  xxi := xx256 shr 8; xxf := xx256 and $FF;
  yyi := yy256 shr 8; yyf := yy256 and $FF;

  //1. average the corners ...
  weight := (($100 - xf) * ($100 - yf)) shr 8;
  color.Add(srcBits[xi + yi * maxX], weight);
  weight := (xxf * ($100 - yf)) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yi * maxX], weight);
  weight := (($100 - xf) * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xi + yyi * maxX], weight);
  weight := (xxf * yyf) shr 8;
  if (weight <> 0) then color.Add(srcBits[xxi + yyi * maxX], weight);

  //2. average the edges
  if (yi +1 < yyi) then
  begin
    xf := $100 - xf;
    for i := yi + 1 to yyi - 1 do
      color.Add(srcBits[xi + i * maxX], xf);
    if (xxf <> 0) then
      for i := yi + 1 to yyi - 1 do
        color.Add(srcBits[xxi + i * maxX], xxf);
  end;
  if (xi + 1 < xxi) then
  begin
    yf := $100 - yf;
    for i := xi + 1 to xxi - 1 do
      color.Add(srcBits[i + yi * maxX], yf);
    if (yyf <> 0) then
      for i := xi + 1 to xxi - 1 do
        color.Add(srcBits[i + yyi * maxX], yyf);
  end;

  //3. average the non-fractional pixel 'internals' ...
  for i := xi + 1 to xxi - 1 do
    for j := yi + 1 to yyi - 1 do
      color.Add(srcBits[i + j * maxX], $100);

  //4. finally get the weighted color ...
  if color.AddCount = 0 then
    Result := srcBits[xi + yi * maxX] else
    Result := color.Color;
end;
//------------------------------------------------------------------------------

function RgbtoHsl(color: TColor32): THsl;
var
  rgba: TARGB absolute color;
  hsl: THsl absolute result;
  r,g,b: byte;
  maxRGB, minRGB, mAdd, mSub: Integer;
begin
  //https://en.wikipedia.org/wiki/HSL_and_HSV and
  //http://en.wikipedia.org/wiki/HSL_color_space
  r := rgba.R; g := rgba.G; b := rgba.B;
  maxRGB := Max(r, Max(g, b));
  minRGB := Min(r, Min(g, b));
  mAdd := maxRGB + minRGB;
  hsl.lum := mAdd shr 1;
  hsl.alpha := rgba.A;

  if maxRGB = minRGB then
  begin
    hsl.hue := 0; //hsl.hue is undefined when gray
    hsl.sat := 0;
    Exit;
  end;

  mSub := maxRGB - minRGB;
  if mAdd <= 255 then
    hsl.sat := DivTable[mSub, mAdd] else
    hsl.sat := DivTable[mSub, 511 - mAdd];

  mSub := mSub * 6;
  if r = maxRGB then
  begin
    if g >= b then
      hsl.hue := (g - b) * 255 div mSub else
      hsl.hue := 255 - ((b - g) * 255 div mSub);
  end
  else if G = maxRGB then
  begin
    if b > r then
      hsl.hue := 85 + (b - r) * 255 div mSub else
      hsl.hue := 85 - (r - b)  * 255 div mSub;
  end else
  begin
    if r > g then
      hsl.hue := 170 + (r - g)  * 255 div mSub else
      hsl.hue := 170 - (g - r)  * 255 div mSub;
  end;
end;
//------------------------------------------------------------------------------

function HslToRgb(hslColor: THsl): TColor32;
var
  rgba: TARGB absolute result;
  hsl: THsl absolute hslColor;
  c, x, m: Integer;
begin
  //formula from https://www.rapidtables.com/convert/color/hsl-to-rgb.html
  c := (255 - abs(2 * hsl.lum - 255)) * hsl.sat div 255;
  x := c * (255 - abs((hsl.hue mod 85) * 6 - 255)) div 255;
  m := hsl.lum - c div 2;
  rgba.A := hsl.alpha;
  case (hsl.hue * 6) shr 8 of
    0: begin rgba.R := c + m; rgba.G := x + m; rgba.B := 0 + m; end;
    1: begin rgba.R := x + m; rgba.G := c + m; rgba.B := 0 + m; end;
    2: begin rgba.R := 0 + m; rgba.G := c + m; rgba.B := x + m; end;
    3: begin rgba.R := 0 + m; rgba.G := x + m; rgba.B := c + m; end;
    4: begin rgba.R := x + m; rgba.G := 0 + m; rgba.B := c + m; end;
    5: begin rgba.R := c + m; rgba.G := 0 + m; rgba.B := x + m; end;
  end;
end;
//------------------------------------------------------------------------------

function ArrayOfColor32ToArrayHSL(const clr32Arr: TArrayOfColor32): TArrayofHSL;
var
  i, len: Integer;
begin
  len := length(clr32Arr);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := RgbtoHsl(clr32Arr[i]);
end;
//------------------------------------------------------------------------------

function ArrayOfHSLToArrayColor32(const hslArr: TArrayofHSL): TArrayOfColor32;
var
  i, len: Integer;
begin
  len := length(hslArr);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := HslToRgb(hslArr[i]);
end;
//------------------------------------------------------------------------------

function Intensity(color: TColor32): byte;
var
  c: TARGB absolute color;
begin
  Result := (c.R * 61 + c.G * 174 + c.B * 21) shr 8;
end;
//------------------------------------------------------------------------------

function Gray(color: TColor32): TColor32;
var
  c: TARGB absolute color;
  res: TARGB absolute Result;
begin
  res.A := c.A;
  res.R := Intensity(color);
  res.G := res.R;
  res.B := res.R;
end;

//------------------------------------------------------------------------------
// TRectD methods
//------------------------------------------------------------------------------

function TRectD.IsEmpty: Boolean;
begin
  result := (right <= left) or (bottom <= top);
end;
//------------------------------------------------------------------------------

function TRectD.Width: double;
begin
  result := Max(0, right - left);
end;
//------------------------------------------------------------------------------

function TRectD.Height: double;
begin
  result := Max(0, bottom - top);
end;
//------------------------------------------------------------------------------

function TRectD.Normalize: Boolean;
var
  d: double;
begin
  Result := false;
  if Left > Right then
  begin
    d := Left;
    Left := Right;
    Right := d;
    Result := True;
  end;
  if Top > Bottom then
  begin
    d := Top;
    Top := Bottom;
    Bottom := d;
    Result := True;
  end;
end;
//------------------------------------------------------------------------------

function RectD(left, top, right, bottom: double): TRectD;
begin
  result.Left := left;
  result.Top := top;
  result.Right := right;
  result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function RectD(const rec: TRect): TRectD;
begin
  with rec do
  begin
    result.Left := left;
    result.Top := top;
    result.Right := right;
    result.Bottom := bottom;
  end;
end;

//------------------------------------------------------------------------------
// TImage32 methods
//------------------------------------------------------------------------------

constructor TImage32.Create(width: Integer; height: Integer);
begin
  fAntiAliase := true;
  SetSize(width, height, clNone32);
end;
//------------------------------------------------------------------------------

constructor TImage32.Create(src: TImage32);
begin
  Assign(src);
end;
//------------------------------------------------------------------------------

constructor TImage32.Create(src: TImage32; const srcRec: TRect);
begin
  fAntiAliase := src.fAntiAliase;
  SetSize(RectWidth(srcRec), RectHeight(srcRec));
  if not IsEmptyRect(srcRec) then
    fPixels := src.CopyPixels(srcRec);
end;
//------------------------------------------------------------------------------

destructor TImage32.Destroy;
begin
  fPixels := nil;
  inherited;
end;
//------------------------------------------------------------------------------

class function TImage32.IsRegisteredFormat(const ext: string): Boolean;
begin
  result := Assigned(TImage32.GetImageFormatClass(ext));
end;
//------------------------------------------------------------------------------

class procedure TImage32.RegisterImageFormatClass(ext: string;
  bm32ExClass: TImageFormatClass; clipPriority: TClipboardPriority);
var
  cpChar: Char;
begin
  if (ext = '') or (ext = '.') then Exit
  else if (ext[1] = '.') then Delete(ext, 1,1);
  if not CharInSet(ext[1], ['A'..'Z','a'..'z']) then Exit;

  cpChar := Char(clipPriority);
  ImageFormatClassList.AddObject(cpChar + ext, Pointer(bm32ExClass));
end;
//------------------------------------------------------------------------------

class function TImage32.GetImageFormatClass(const ext: string): TImageFormatClass;
var
  i: Integer;
  pattern, matchStr: string;
begin
  Result := nil;
  pattern := ext;
  if (pattern = '')  or (pattern = '.') then Exit;
  if pattern[1] = '.' then Delete(pattern, 1,1);
  if CharInSet(pattern[1],
    [ Chr( Ord(Low(TClipboardPriority)) ) ..
    Chr( Ord(High(TClipboardPriority))) ]) then
      Delete(pattern, 1,1); //ie sort priority

  for i := 0 to imageFormatClassList.count -1 do
  begin
    matchStr := Copy(imageFormatClassList[i], 2, 80);
    if not SameText(matchStr, pattern) then Continue;
    Result := TImageFormatClass(ImageFormatClassList.objects[i]);
    break;
  end;
end;
//------------------------------------------------------------------------------

class function TImage32.GetExtFromImageStream(stream: TStream): string;
var
  i, pos, len: Integer;
  flag: Cardinal;
  buffer: array [0..79] of Byte;
begin
  result := '';
  pos := stream.position;
  len := stream.size - pos;
  if len <= 4 then Exit;
  stream.read(flag, SizeOf(flag));
  if (flag and $FFFF) = $4D42 then Result := 'BMP'
  else if flag = $38464947 then result := 'GIF'
  else if flag and $FFFF = $D8FF then result := 'JPG'
  else if flag = $474E5089 then result := 'PNG'
  else if (flag = $002A4949) or (flag = $2A004949) then result := 'TIF'
  else
  begin
    len := min(len, 80);
    stream.read(buffer[0], len);
    for i := 0 to len -1 do
      if (buffer[i] < 9) then Exit;
    Result := 'TXT'; //eg MIME encoded
  end;
  stream.position := pos;
end;
//------------------------------------------------------------------------------

procedure TImage32.Assign(src: TImage32);
begin
  if assigned(src) then
    src.AssignTo(self);
end;
//------------------------------------------------------------------------------

procedure TImage32.AssignTo(dst: TImage32);
begin
  dst.fAntiAliase := fAntiAliase;
  dst.fIsPremultiplied := fIsPremultiplied;
  dst.SetSize(Width, Height);
  if (Width > 0) and (Height > 0) then
    move(fPixels[0], dst.fPixels[0], Width * Height * SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

procedure TImage32.SetBackgroundColor(bgColor: TColor32);
var
  i: Integer;
  pc: PColor32;
begin
  pc := Pixelbase;
  for i := 0 to high(fPixels) do
  begin
    pc^ := BlendToOpaque(bgColor, pc^);
     inc(pc);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.HatchBackground(color1: TColor32;
  color2: TColor32; hatchSize: Integer);
var
  i,j: Integer;
  pc: PColor32;
  colors: array[boolean] of TColor32;
  hatch: Boolean;
begin
  colors[false] := color1;
  colors[true] := color2;
  pc := Pixelbase;
  for i := 0 to Height -1 do
  begin
    hatch := Odd(i div hatchSize);
    for j := 0 to Width -1 do
    begin
      if (j + 1) mod hatchSize = 0 then hatch := not hatch;
      pc^ := BlendToOpaque(colors[hatch], pc^);
     inc(pc);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Clear(color: TColor32);
var
  i: Integer;
  pc: PColor32;
begin
  fIsPremultiplied := false;
  if IsEmpty then Exit;
  if color = clNone32 then
    FillChar(fPixels[0], Width * Height * SizeOf(TColor32), 0)
  else
  begin
    pc := PixelBase;
    for i := 0 to Width * Height -1 do
    begin
      pc^ := color; inc(pc);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.FillRect(rec: TRect; color: TColor32);
var
  i,j, rw: Integer;
  c: PColor32;
begin
  IntersectRect(rec, rec, bounds);
  if IsEmptyRect(rec) then Exit;
  rw := RectWidth(rec);
  c := @Pixels[rec.Top * Width + rec.Left];
  for i := rec.Top to rec.Bottom -1 do
  begin
    for j := 1 to rw do
    begin
      c^ := color;
      inc(c);
    end;
    inc(c, Width - rw);
  end;
end;
//------------------------------------------------------------------------------

procedure CheckBlendFill(pc: PColor32; color: TColor32);
{$IFDEF INLINE} inline; {$ENDIF}
begin
  if not assigned(pc) then Exit;
  pc^ := BlendToAlpha(pc^, color);
end;
//------------------------------------------------------------------------------

function TImage32.CopyPixels(rec: TRect): TArrayOfColor32;
var
  i, clipW, w,h: Integer;
  pSrc, pDst, pDst2: PColor32;
  recClipped: TRect;
begin
  w := RectWidth(rec);
  h := RectHeight(rec);
  setLength(result, w * h);

  if w * h = 0 then Exit;
  IntersectRect(recClipped, rec, Bounds);
  //if recClipped is wholely outside the bounds of the image ...
  if IsEmptyRect(recClipped) then
  begin
    //rec is considered valid even when completely outside the image bounds,
    //and so when that happens we simply return a fully transparent image ...
    FillChar(Result[0], w * h * SizeOf(TColor32), 0);
    Exit;
  end;

  //if recClipped is wholely within the bounds of the image ...
  if RectsEqual(recClipped, rec) then
  begin
    pDst := @Result[0];
    pSrc := @fPixels[recClipped.Top * Width + rec.Left];
    for i := recClipped.Top to recClipped.Bottom -1 do
    begin
      Move(pSrc^, pDst^, w * SizeOf(TColor32));
      inc(pSrc, Width); inc(pDst, w);
    end;
    Exit;
  end;

  //a part of 'rec' must be outside the bounds of the image ...

  pDst := @Result[0];
  for i := rec.Top to -1 do
  begin
    FillChar(pDst^, w * SizeOf(TColor32), 0);
    inc(pDst, w);
  end;
  pSrc := @fPixels[recClipped.Top * Width + Max(0,rec.Left)];
  if (rec.Left < 0) or (rec.Right > Width) then
  begin
    clipW := RectWidth(recClipped);
    pDst2 := IncPColor32(pDst, -Min(0, rec.Left));
    for i := recClipped.Top to recClipped.Bottom -1 do
    begin
      //when rec.left < 0 or rec.right > width it's simplest to
      //start with a prefilled row of transparent pixels
      FillChar(pDst^, w * SizeOf(TColor32), 0);
      Move(pSrc^, pDst2^, clipW * SizeOf(TColor32));
      inc(pDst, w); inc(pDst2, w); inc(pSrc, Width);
    end;
  end else
  begin
    //things are simpler when there's no part of 'rec' is
    //outside the image, at least not on the left or right sides ...
    for i := recClipped.Top to recClipped.Bottom -1 do
    begin
      Move(pSrc^, pDst^, w * SizeOf(TColor32));
      inc(pSrc, Width); inc(pDst, w);
    end;
  end;
  for i := Height to rec.Bottom -1 do
  begin
    FillChar(pDst^, w * SizeOf(TColor32), 0);
    inc(pDst, w);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Crop(const rec: TRect);
var
  newPixels: TArrayOfColor32;
begin
  newPixels := CopyPixels(rec);
  SetSize(RectWidth(rec), RectHeight(rec));
  if not IsEmptyRect(rec) then fPixels := newPixels;
end;
//------------------------------------------------------------------------------

function TImage32.GetBounds: TRect;
begin
  result := Rect(0, 0, Width, Height);
end;
//------------------------------------------------------------------------------

procedure TImage32.SetSize(newWidth, newHeight: Integer; color: TColor32);
begin
  fwidth := Max(0, newWidth);
  fheight := Max(0, newHeight);
  fPixels := nil;
  setLength(fPixels, fwidth * fheight);
  fIsPremultiplied := false;
  //nb: dynamic arrays are zero-initialized with SetLength() unless the array
  //is returned as a function result. And so SetSize() zero-initializes pixels.
  if color > 0 then Clear(color);
end;
//------------------------------------------------------------------------------

procedure TImage32.Resize(newWidth, newHeight: Integer; stretchImage: Boolean);
var
  tmp: TImage32;
  rec: TRect;
begin
  if (newWidth <= 0) or (newHeight <= 0) then
  begin
    fwidth := 0;
    fheight := 0;
    fPixels := nil;
    fIsPremultiplied := false;
    Exit;
  end;
  if (newWidth = fwidth) and (newHeight = fheight) then Exit;
  if IsEmpty then
  begin
    SetSize(newWidth, newHeight);
    Exit;
  end;

  if stretchImage then
  begin
    if fAntiAliase then
      DoResizeAA(newWidth, newHeight) else
      DoResize(newWidth, newHeight);
  end else
  begin
    tmp := TImage32.create(self);
    try
      rec := Bounds;
      SetSize(newWidth, newHeight, clNone32);
      CopyFrom(tmp, rec, rec);
    finally
      tmp.Free;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.DoResize(newWidth, newHeight: Integer);
var
  x, y, srcY: Integer;
  scaledXi, scaledYi: TArrayOfInteger;
  tmp: TArrayOfColor32;
  pc: PColor32;
begin
  SetLength(tmp, newWidth * newHeight * SizeOf(TColor32));

  //get scaled X & Y values once only (storing them in lookup arrays) ...
  SetLength(scaledXi, newWidth);
  for x := 0 to newWidth -1 do
    scaledXi[x] := Floor(x * fWidth / newWidth);
  SetLength(scaledYi, newHeight);
  for y := 0 to newHeight -1 do
    scaledYi[y] := Floor(y * fHeight / newHeight);

  pc := @tmp[0];
  for y := 0 to newHeight - 1 do
  begin
    srcY := scaledYi[y];
    if (srcY < 0) or (srcY >= fHeight) then Continue;
    for x := 0 to newWidth - 1 do
    begin
      pc^ := fPixels[scaledXi[x] + srcY * fWidth];
      inc(pc);
    end;
  end;

  fPixels := tmp;
  fwidth := newWidth;
  fheight := newHeight;
end;
//------------------------------------------------------------------------------

procedure TImage32.DoResizeAA(newWidth, newHeight: Integer);
var
  x,y, x256,y256,xx256,yy256: Integer;
  sx,sy: double;
  tmp: TArrayOfColor32;
  pc: PColor32;
  scaledX: array of Integer;
begin
  sx := fWidth/newWidth * 256;
  sy := fHeight/newHeight * 256;
  SetLength(tmp, newWidth * newHeight);

  SetLength(scaledX, newWidth +1); //+1 for fractional overrun
  for x := 0 to newWidth -1 do
    scaledX[x] := Round((x+1) * sx);

  y256 := 0;
  pc := @tmp[0];
  for y := 0 to newHeight - 1 do
  begin
    x256 := 0;
    yy256 := Round((y+1) * sy);
    for x := 0 to newWidth - 1 do
    begin
      xx256 := scaledX[x];
      pc^ := GetWeightedColor(fPixels, x256, y256, xx256, yy256, fWidth);
      x256 := xx256;
      inc(pc);
    end;
    y256 := yy256;
  end;

  fPixels := tmp;
  fwidth := newWidth;
  fheight := newHeight;
end;
//------------------------------------------------------------------------------

procedure TImage32.Scale(sx, sy: single);
begin
  sx := Min(sx, 100); sy := Min(sy, 100);
  if (sx > 0) and (sy > 0) then
    ReSize(Round(width * sx), Round(height * sy));
end;
//------------------------------------------------------------------------------

procedure TImage32.RotateLeft90;
var
  x,y, xx: Integer;
  src, dst: PColor32;
  tmp: TImage32;
begin
  if IsEmpty then Exit;
  tmp := TImage32.create(Self);
  try
    SetSize(Height, Width);
    xx := (width - 1) * Height;
    dst := PixelBase;
    for y := 0 to Height -1 do
    begin
      src := @tmp.Pixels[xx + y];
      for x := 0 to Width -1 do
      begin
        dst^ := src^;
        inc(dst); dec(src, Height);
      end;
    end;
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.RotateRight90;
var
  x,y: Integer;
  src, dst: PColor32;
  tmp: TImage32;
begin
  if IsEmpty then Exit;
  tmp := TImage32.create(Self);
  try
    SetSize(Height, Width);
    dst := PixelBase;
    for y := 0 to Height -1 do
    begin
      src := @tmp.Pixels[Height -1 - y];
      for x := 0 to Width -1 do
      begin
        dst^ := src^;
        inc(dst); inc(src, Height);
      end;
    end;
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Rotate180;
var
  x,y: Integer;
  src, dst: PColor32;
  tmp: TImage32;
begin
  if IsEmpty then Exit;
  tmp := TImage32.create(Self);
  try
    dst := PixelBase;
    src := @tmp.Pixels[Width * Height -1];
    for y := 0 to Height -1 do
    begin
      for x := 0 to Width -1 do
      begin
        dst^ := src^;
        inc(dst); dec(src);
      end;
    end;
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

function TImage32.GetWeightedPixel(x256, y256: Integer): TColor32;
var
  xi, yi, weight: Integer;
  color: TWeightedColor;
  xf, yf: cardinal;
begin
  //coordinate integers (can be negative) -> x256 div 256 & y256 div 256.
  //coordinate fractions ->  (x256 and $FF) / 256 & (y256 and $FF) / 256
  if (x256 < -$FF) or (y256 < -$FF) or
    (x256 >= fWidth * $100) or (y256 >= fHeight * $100) then
  begin
    result := clNone32;
    Exit;
  end;
  xi := abs(x256) shr 8;
  xf := x256 and $FF;
  yi := abs(y256) shr 8;
  yf := y256 and $FF;

  color.Reset;
  weight := (($100 - xf) * ($100 - yf)) shr 8;         //top-left
  if (x256 < 0) or (y256 < 0) then
    color.AddWeight(weight) else
    color.Add(fPixels[xi + yi * fWidth], weight);

  weight := (xf * ($100 - yf)) shr 8;                  //top-right
  if (xi + 1 >= fWidth) or (y256 < 0) then
    color.AddWeight(weight) else
    color.Add(fPixels[xi + 1 + yi * fWidth], weight);

  weight := (($100 - xf) * yf) shr 8;                  //bottom-left
  if (x256 < 0) or (yi + 1 = fHeight) then
    color.AddWeight(weight) else
    color.Add(fPixels[xi + (yi +1) * fWidth], weight);

  weight := (xf * yf) shr 8;                           //bottom-right
  if (xi + 1 >= fWidth) or (yi + 1 = fHeight) then
    color.AddWeight(weight) else
    color.Add(fPixels[(xi + 1)  + (yi + 1) * fWidth], weight);
  Result := color.Color;
end;
//------------------------------------------------------------------------------

function TImage32.GetColorCount: Integer;
var
  allColors: PByteArray;
  i: Integer;
  c: PColor32;
const
  cube256 = 256 * 256 * 256;
begin
  result := 0;
  if IsEmpty then Exit;
  //this is fast but, becuse it uses a chunk of memory, it's probably
  //safer to allocate allColors on the heap rather than in the stack ...
  allColors := AllocMem(cube256); //nb: zero initialized
  try
    c := PixelBase;
    for i := 0 to Width * Height -1 do
    begin
      allColors[c^ and $FFFFFF] := 1;
      inc(c);
    end;
    for i := 0 to cube256 -1 do
      if allColors[i] = 1 then inc(Result);
  finally
    FreeMem(allColors);
  end;
end;
//------------------------------------------------------------------------------

function TImage32.GetHasTransparency: Boolean;
var
  i: Integer;
  pc: PARGB;
begin
  result := true;
  If IsEmpty then Exit;
  pc := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    if pc.A < 255 then Exit;
    inc(pc);
  end;
  result := false;
end;
//------------------------------------------------------------------------------

function TImage32.SaveToFile(const filename: string): Boolean;
var
  fileFormatClass: TImageFormatClass;
  folder: string;
begin
  result := false;
  folder := ExtractFilePath(filename);
  if IsEmpty or not DirectoryExists(folder) then Exit;
  fileFormatClass := GetImageFormatClass(ExtractFileExt(filename));
  if assigned(fileFormatClass) then
    with fileFormatClass.Create do
    try
      result := SaveToFile(filename, self);
    finally
      free;
    end;
end;
//------------------------------------------------------------------------------

function TImage32.SaveToStream(stream: TStream; const FmtExt: string): Boolean;
var
  fileFormatClass: TImageFormatClass;
begin
  result := false;
  fileFormatClass := GetImageFormatClass(FmtExt);
  if assigned(fileFormatClass) then
    with fileFormatClass.Create do
    try
      SaveToStream(stream, self);
      result := true;
    finally
      free;
    end;
end;
//------------------------------------------------------------------------------

function TImage32.LoadFromFile(const filename: string): Boolean;
var
  image32FileFmtClass: TImageFormatClass;
begin
  image32FileFmtClass := GetImageFormatClass(ExtractFileExt(filename));
  result := assigned(image32FileFmtClass);
  if result then
    with image32FileFmtClass.Create do
    try
      result := LoadFromFile(filename, self);
    finally
      free;
    end;
end;
//------------------------------------------------------------------------------

function TImage32.LoadFromStream(stream: TStream; const FmtExt: string): Boolean;
var
  image32FileFmtClass: TImageFormatClass;
  fmt: string;
begin
  if FmtExt = '' then
    fmt := TImage32.GetExtFromImageStream(stream) else
    fmt := FmtExt;
  image32FileFmtClass := GetImageFormatClass(fmt);
  result := assigned(image32FileFmtClass);
  if result then
    with image32FileFmtClass.Create do
    try
      result := LoadFromStream(stream, self);
    finally
      free;
    end;
end;
//------------------------------------------------------------------------------

function TImage32.LoadFromResource(const resName, resType: string): Boolean;
var
  resStream: TResourceStream;
begin
  if Uppercase(resType) = 'BMP' then
  begin
    result := FindResource(hInstance, PChar(resName), RT_BITMAP) <> 0;
    if not result then Exit;
    resStream := TResourceStream.Create(hInstance, resName, RT_BITMAP);
  end else
  begin
    result := FindResource(hInstance, PChar(resName), PChar(resType)) <> 0;
    if not result then Exit;
    resStream := TResourceStream.Create(hInstance, resName, PChar(resType));
  end;
  try
    LoadFromStream(resStream, resType);
  finally
    resStream.Free;
  end;
end;
//------------------------------------------------------------------------------


function TImage32.GetPixel(x, y: Integer): TColor32;
begin
  if (x < 0) or (x >= Width) or (y < 0) or (y >= Height) then
    result := clNone32 else
    result := fPixels[y * width + x];
end;
//------------------------------------------------------------------------------

procedure TImage32.SetPixel(x,y: Integer; color: TColor32);
begin
  if (x < 0) or (x >= Width) or (y < 0) or (y >= Height) then Exit;
  fPixels[y * width + x] := color;
end;
//------------------------------------------------------------------------------

function TImage32.GetIsEmpty: Boolean;
begin
  result := fPixels = nil;
end;
//------------------------------------------------------------------------------

function TImage32.GetPixelBase: PColor32;
begin
  if IsEmpty then result := nil
  else result := @fPixels[0];
end;
//------------------------------------------------------------------------------

function TImage32.GetPixelRow(row: Integer): PColor32;
begin
  if IsEmpty then result := nil
  else result := @fPixels[row * Width];
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyInternal(src: TImage32;
  const srcRec, dstRec: TRect; blendFunc: TBlendFunction);
var
  i, j, srcRecWidth: Integer;
  s, d: PColor32;
begin
  srcRecWidth := srcRec.Right - srcRec.Left;
  s := @src.Pixels[srcRec.Top * src.Width + srcRec.Left];
  d := @Pixels[dstRec.top * Width + dstRec.Left];

  if assigned(blendFunc) then
    for i := srcRec.Top to srcRec.Bottom -1 do
    begin
      for j := 1 to srcRecWidth do
      begin
        d^ := blendFunc(d^, s^);
        inc(s); inc(d);
      end;
      inc(s, src.Width - srcRecWidth);
      inc(d, Width - srcRecWidth);
    end
  else
    //simply overwrite src with dst (ie without blending)
    for i := srcRec.Top to srcRec.Bottom -1 do
    begin
      move(s^, d^, srcRecWidth * SizeOf(TColor32));
      inc(s, src.Width);
      inc(d, Width);
    end;
end;
//------------------------------------------------------------------------------

function TImage32.CopyFrom(src: TImage32; srcRec, dstRec: TRect;
  blendFunc: TBlendFunction): Boolean;
var
  tmp: TImage32;
  srcRecClipped, dstRecClipped, dummy: TRect;
  scale, scaleSrc, scaleDst: TPointD;
begin
  result := false;
  if IsEmptyRect(srcRec) or IsEmptyRect(dstRec) then Exit;
  IntersectRect(srcRecClipped, srcRec, src.Bounds);

  //get the scaling amount (if any) before
  //dstRec might be adjusted due to clipping ...
  scale.X := RectWidth(dstRec)/RectWidth(srcRec);
  scale.Y := RectHeight(dstRec)/RectHeight(srcRec);

  //check if the source rec has been clipped ...
  if not RectsEqual(srcRecClipped, srcRec) then
  begin
    if IsEmptyRect(srcRecClipped) then Exit;
    //the source has been clipped so clip the destination too ...
    scaleSrc.X := RectWidth(srcRecClipped)/RectWidth(srcRec);
    scaleSrc.Y := RectHeight(srcRecClipped)/RectHeight(srcRec);
    ScaleRect(dstRec, scaleSrc);
    OffsetRect(dstRec,
      srcRecClipped.Left - srcRec.Left,
      srcRecClipped.Top - srcRec.Top);
  end;

  if (scale.X <> 1.0) or (scale.Y <> 1.0) then
  begin
    //scale source (tmp) to the destination then call CopyFrom() again ...
    tmp := TImage32.Create(src, srcRecClipped);
    try
      tmp.Scale(scale.X, scale.Y);
      result := CopyFrom(tmp, tmp.Bounds, dstRec, blendFunc);
    finally
      tmp.Free;
    end;
    Exit;
  end;

  IntersectRect(dstRecClipped, dstRec, Bounds);
  if IsEmptyRect(dstRecClipped) then Exit;

  //there's no scaling if we get here, but further clipping may be needed if
  //the destination rec is partially outside the destination image's bounds

  if not RectsEqual(dstRecClipped, dstRec) then
  begin
    //the destination rec has been clipped so clip the source too ...
    scaleDst.X := RectWidth(dstRecClipped)/RectWidth(dstRec);
    scaleDst.Y := RectHeight(dstRecClipped)/RectHeight(dstRec);
    ScaleRect(srcRecClipped, scaleDst);
    OffsetRect(srcRecClipped,
      dstRecClipped.Left - dstRec.Left,
      dstRecClipped.Top - dstRec.Top);
  end;

  //when copying to self and srcRec & dstRec overlap then
  //copy srcRec to a temporary image and use it as the source ...
  if (src = self) and
    IntersectRect(dummy, srcRecClipped, dstRecClipped) then
  begin
    tmp := TImage32.Create(self, srcRecClipped);
    try
      result := src.CopyFrom(tmp, tmp.Bounds, dstRecClipped, blendFunc);
    finally
      tmp.Free;
    end;
    Exit;
  end;
  CopyInternal(src, srcRecClipped, dstRecClipped, blendFunc);
  result := true;
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyFromDC(srcDc: HDC; width, height: Integer);
var
  bi: TBitmapInfoHeader;
  bm, oldBm: HBitmap;
  memDc: HDC;
  pixels: Pointer;
begin
  SetSize(width, height, 0);
  bi := GetBitmapInfoHeader(width, height);
  memDc := GetCompatibleMemDc;
  try
    bm := CreateDIBSection(memDc,
      PBITMAPINFO(@bi)^, DIB_RGB_COLORS, pixels, 0, 0);
    if bm = 0 then Exit;
    try
      oldBm := SelectObject(memDc, bm);
      BitBlt(memDc, 0,0, width, height, srcDc, 0,0, SRCCOPY);
      Move(pixels^, fPixels[0], width * height * sizeOf(TColor32));
      SelectObject(memDc, oldBm);
    finally
      DeleteObject(bm);
    end;
  finally
    DeleteDc(memDc);
  end;
  SetAlpha(255);
  FlipVertical;
end;
//------------------------------------------------------------------------------

procedure TImage32.CopyToDc(dstDc: HDC;
  x,y: Integer; transparent: Boolean; bkColor: TColor32);
var
  tmp: TImage32;
  bi: TBitmapInfoHeader;
  bm, oldBm: HBitmap;
  dibBits: Pointer;
  memDc: HDC;
  hasTransparency: Boolean;
  bf: BLENDFUNCTION;
begin
  if IsEmpty then Exit;
  bi := GetBitmapInfoHeader(Width, Height);
  tmp := TImage32.create(self);
  try
    tmp.FlipVertical; //DIB sections store pixels Y-inverted
    hasTransparency := transparent and (bkColor < 255) and Self.HasTransparency;
    if HasTransparency then
      tmp.Premultiply;  //this is required for DIB sections
    if bkColor <> 0 then
      tmp.SetBackgroundColor(bkColor);
    memDc := GetCompatibleMemDc;
    try
      bm := CreateDIBSection(memDc,
        PBITMAPINFO(@bi)^, DIB_RGB_COLORS, dibBits,0,0);
      if bm = 0 then Exit;
      try
        Move(tmp.PixelBase^, dibBits^, Width * Height * SizeOf(TColor32));
        oldBm := SelectObject(memDC, bm);
        if HasTransparency then
        begin
          bf.BlendOp := AC_SRC_OVER;
          bf.BlendFlags := 0;
          bf.SourceConstantAlpha := 255;
          bf.AlphaFormat := AC_SRC_ALPHA;
          AlphaBlend(dstDc, x,y, Width, Height, memDC, 0, 0, Width, Height, bf);
        end
        else
          BitBlt(dstDc, x,y, Width, Height, memDc, 0,0, SRCCOPY);
        SelectObject(memDC, oldBm);
      finally
        DeleteObject(bm);
      end;
    finally
      DeleteDc(memDc);
    end;
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

function TImage32.CopyToClipBoard: Boolean;
var
  ext: string;
  i: Integer;
  fileFormatClass: TImageFormatClass;
begin
  //Sadly with CF_DIB (and even CF_DIBV5) clipboard formats, transparency is
  //usually lost during copy pasting, so we use the CF_PNG format instead, as
  //this format is widely supported by other software.
  result := not IsEmpty;
  if not result then Exit;
  result := false;
  OpenClipboard(0);
  try
    EmptyClipboard;
    //copy each registered format to clipboard
    for i := ImageFormatClassList.Count -1 downto 0 do
    begin
      ext := ImageFormatClassList[i];
      fileFormatClass := GetImageFormatClass(ext);
      if assigned(fileFormatClass) then
        with fileFormatClass.Create do
        try
          if CopyToClipboard(self) then result := true;
        finally
          free;
        end;
    end;
  finally
    CloseClipboard;
  end;
end;
//------------------------------------------------------------------------------

function TImage32.PasteFromClipBoard: Boolean;
var
  ext: string;
  i: Integer;
  fileFormatClass: TImageFormatClass;
begin
  result := not IsEmpty;
  if not result then Exit;
  result := true;
  OpenClipboard(0);
  try
    for i := ImageFormatClassList.Count -1 downto 0 do
    begin
      ext := ImageFormatClassList[i];
      fileFormatClass := GetImageFormatClass(ext);
      if assigned(fileFormatClass) then
        with fileFormatClass.Create do
        try
          if PasteFromClipboard(self) then Exit;
        finally
          free;
        end;
    end;
    result := false;
  finally
    CloseClipboard;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.FlipVertical;
var
  i: Integer;
  a: TArrayOfColor32;
  row: PColor32;
begin
  if IsEmpty then Exit;
  SetLength(a, fWidth * fHeight);
  row := @fPixels[(height-1) * width];
  for i := 0 to fHeight -1 do
  begin
    move(row^, a[i * fWidth], fWidth * SizeOf(TColor32));
    dec(row, fWidth);
  end;
  fPixels := a;
end;
//------------------------------------------------------------------------------

procedure TImage32.FlipHorizontal;
var
  i,j, widthLess1: Integer;
  a: TArrayOfColor32;
  row: PColor32;
begin
  if IsEmpty then Exit;
  SetLength(a, fWidth);
  widthLess1 := fWidth -1;
  row := @fPixels[(height-1) * width]; //top row
  for i := 0 to fHeight -1 do
  begin
    move(row^, a[0], fWidth * SizeOf(TColor32));
    for j := 0 to widthLess1 do
    begin
      row^ := a[widthLess1 - j];
      inc(row);
    end;
    dec(row, fWidth *2);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.PreMultiply;
var
  i: Integer;
  c: PARGB;
begin
  if IsEmpty or fIsPremultiplied then Exit;
  fIsPremultiplied := true;
  c := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    if c.A > 0 then
    begin
      c.R  := MulTable[c.R, c.A];
      c.G  := MulTable[c.G, c.A];
      c.B  := MulTable[c.B, c.A];
    end
    else
      c.Color := 0;
    inc(c);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.SetAlpha(alpha: Byte);
var
  i: Integer;
  c: PARGB;
begin
  if IsEmpty then Exit;
  c := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    c.A := alpha;
    inc(c);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Grayscale;
begin
  AdjustSaturation(-100);
end;
//------------------------------------------------------------------------------

procedure TImage32.InvertColors;
var
  pc: PARGB;
  i: Integer;
begin
  pc := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    pc.R := 255 - pc.R;
    pc.G := 255 - pc.G;
    pc.B := 255 - pc.B;
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.InvertAlphas;
var
  pc: PARGB;
  i: Integer;
begin
  pc := PARGB(PixelBase);
  for i := 0 to Width * Height -1 do
  begin
    pc.A := 255 - pc.A;
    inc(pc);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.EraseColor(color: TColor32; ExactMatchOnly: Boolean);
var
  fg: TARGB absolute color;
  bg: PARGB;
  i: Integer;
  Q: byte;
begin
  if fg.A = 0 then Exit;
  bg := PARGB(PixelBase);

  if ExactMatchOnly then
  begin
    for i := 0 to Width * Height -1 do
    begin
      if bg.Color = color then bg.Color := clNone32;
      inc(bg);
    end;
    Exit;
  end;

  for i := 0 to Width * Height -1 do
  begin
    if bg.A > 0 then
    begin
      Q := 0;
      if (bg.R > fg.R) then Q := Max(Q, DivTable[bg.R - fg.R, fg.R xor 255])
      else if (bg.R < fg.R) then Q := Max(Q, DivTable[fg.R - bg.R, fg.R]);
      if (bg.G > fg.G) then Q := Max(Q, DivTable[bg.G - fg.G, fg.G xor 255])
      else if (bg.G < fg.G) then Q := Max(Q, DivTable[fg.G - bg.G, fg.G]);
      if (bg.B > fg.B) then Q := Max(Q, DivTable[bg.B - fg.B, fg.B xor 255])
      else if (bg.B < fg.B) then Q := Max(Q, DivTable[fg.B - bg.B, fg.B]);
      if Q > 0 then
      begin
        bg.A := MulTable[bg.A, Q];
        bg.R := DivTable[bg.R - MulTable[fg.R, 255 - Q], Q];
        bg.G := DivTable[bg.G - MulTable[fg.G, 255 - Q], Q];
        bg.B := DivTable[bg.B - MulTable[fg.B, 255 - Q], Q];
      end else
        bg.Color := clNone32;
    end;
    inc(bg);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustHue(percent: Integer);
var
  i: Integer;
  tmpImage: TArrayofHSL;
  lut: array [byte] of byte;
begin
  percent := percent mod 100;
  if percent < 0 then inc(percent, 100);
  percent := Round(percent * 255 / 100);
  if (percent = 0) or IsEmpty then Exit;
  for i := 0 to 255 do lut[i] := (i + percent) mod 255;
  tmpImage := ArrayOfColor32ToArrayHSL(fPixels);
  for i := 0 to high(tmpImage) do
    tmpImage[i].hue := lut[ tmpImage[i].hue ];
  fPixels := ArrayOfHSLToArrayColor32(tmpImage);
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustLuminance(percent: Integer);
var
  i: Integer;
  tmpImage: TArrayofHSL;
  pc: double;
  lut: array [byte] of byte;
begin
  if (percent = 0) or IsEmpty then Exit;
  percent := percent mod 101;
  pc := percent / 100;
  if pc > 0 then
    for i := 0 to 255 do lut[i] := Round(i + (255 - i) * pc)
  else
    for i := 0 to 255 do lut[i] := Round(i + (i * pc));

  tmpImage := ArrayOfColor32ToArrayHSL(fPixels);
  for i := 0 to high(tmpImage) do
    tmpImage[i].lum := lut[ tmpImage[i].lum ];
  fPixels := ArrayOfHSLToArrayColor32(tmpImage);
end;
//------------------------------------------------------------------------------

procedure TImage32.AdjustSaturation(percent: Integer);
var
  i: Integer;
  tmpImage: TArrayofHSL;
  lut: array [byte] of byte;
  pc: double;
begin
  if (percent = 0) or IsEmpty then Exit;
  percent := percent mod 101;
  pc := percent / 100;
  if pc > 0 then
    for i := 0 to 255 do lut[i] := Round(i + (255 - i) * pc)
  else
    for i := 0 to 255 do lut[i] := Round(i + (i * pc));

  tmpImage := ArrayOfColor32ToArrayHSL(fPixels);
  for i := 0 to high(tmpImage) do
    tmpImage[i].sat := lut[ tmpImage[i].sat ];
  fPixels := ArrayOfHSLToArrayColor32(tmpImage);
end;
//------------------------------------------------------------------------------

procedure TImage32.GaussianBlur(rec: TRect; radius: Integer);
var
  i, x,y,yy,z: Integer;
  gaussTable: array [-MaxBlur .. MaxBlur] of Cardinal;
  wc: TWeightedColor;
  wca: TArrayOfWeightedColor;
  row: PColor32Array;
  wcRow: PWeightedColorArray;
begin
  IntersectRect(rec, Bounds, rec);
  if IsEmptyRect(rec) or (radius < 1) then Exit
  else if radius > MaxBlur then radius := MaxBlur;

  for i := 0 to radius do
  begin
    gaussTable[i] := Sqr(Radius - i +1);
    gaussTable[-i] := gaussTable[i];
  end;

  setLength(wca, RectWidth(rec) * RectHeight(rec));

  for y := 0 to RectHeight(rec) -1 do
  begin
    row := PColor32Array(@fPixels[(y + rec.Top) * Width + rec.Left]);
    wcRow := PWeightedColorArray(@wca[y * RectWidth(rec)]);
    for x := 0 to RectWidth(rec) -1 do
      for z := max(0, x - radius) to min(Width -1, x + radius) do
        wcRow[x].Add(row[z], gaussTable[x-z]);
  end;

  for x := 0 to RectWidth(rec) -1 do
  begin
    for y := 0 to RectHeight(rec) -1 do
    begin
      wc.Reset;
      yy := max(0, y - radius) * RectWidth(rec);
      for z := max(0, y - radius) to min(RectHeight(rec) -1, y + radius) do
      begin
        wc.Add(wca[x + yy].Color, gaussTable[y-z]);
        inc(yy, RectWidth(rec));
      end;
      fPixels[x + rec.Left + (y + rec.Top) * Width] := wc.Color;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.CropTransparentPixels;
var
  x,y, x1,x2,y1,y2: Integer;
  rec: TRect;
  found: Boolean;
begin
  y1 := 0; y2 := 0;
  found := false;
  for y := 0 to Height -1 do
  begin
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        y1 := y;
        found := true;
        break;
      end;
    if found then break;
  end;

  if not found then
  begin
    SetSize(0, 0);
    Exit;
  end;

  found := false;
  for y := Height -1 downto 0 do
  begin
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        y2 := y;
        found := true;
        break;
      end;
    if found then break;
  end;

  x1 := Width; x2 := 0;
  for y := y1 to y2 do
    for x := 0 to Width -1 do
      if TARGB(fPixels[y * Width + x]).A > 0 then
      begin
        if x < x1 then x1 := x;
        if x > x2 then x2 := x;
      end;

  rec := Rect(x1, height-y2-1, x2+1, height-y1);
  Crop(rec);
end;
//------------------------------------------------------------------------------

procedure TImage32.Rotate(angleRads: single);
var
  tmp: TArrayOfColor32;
  x, y, xi, yi, newWidth, newHeight: Integer;
  sinA, cosA: extended;
  dx, dy: double;
  pt, cp, cp2: TPointD;
  rec: TRectD;
  dstColor: PColor32;
const
  TwoPi = 2 * pi;
begin
  while angleRads >= TwoPi do angleRads := angleRads - TwoPi;
  while angleRads < 0 do angleRads := angleRads + TwoPi;
  if IsEmpty or (angleRads < 0.001) or
    (angleRads > TwoPi - 0.001) then Exit; //ignores rotations < 1/10 degree

  Math.SinCos(angleRads, sinA, cosA);
  if abs(sinA) < 0.000001 then
  begin
    Rotate180; //because we've excluded 0 & 360 deg angles
    Exit;
  end
  else if sinA > 0.999999 then
  begin
    RotateRight90;
    Exit;
  end
  else if sinA < -0.999999 then
  begin
    RotateLeft90;
    Exit;
  end;

  cp := PointD(width / 2, height / 2);
  rec.Left := 0; rec.Top := 0;
  rec.Right := Width; rec.Bottom := Height;
  rec := GetRotatedRectD(rec, angleRads);
  newWidth := Ceil(rec.Right - rec.Left);
  newHeight := Ceil(rec.Bottom - rec.Top);
  cp2 := PointD(newWidth / 2, newHeight / 2);
  SetLength(tmp, newWidth * newHeight);
  dstColor := @tmp[0];
  dx := (newWidth - fWidth) / 2;
  dy := (newHeight - fHeight) / 2;
  if fAntiAliase then
  begin
    for y := 0 to newHeight -1 do
      for x := 0 to newWidth -1 do
      begin
        pt := PointD(x, y);
        RotatePt(pt, cp2, sinA, cosA);
        xi := Round((pt.X - dx) *256);
        yi := Round((pt.Y - dy) *256);
        dstColor^ := GetWeightedPixel(xi, yi);
        inc(dstColor);
      end;
  end else
  begin
    for y := 0 to newHeight -1 do
      for x := 0 to newWidth -1 do
      begin
        pt := PointD(x, y);
        RotatePt(pt, cp2, sinA, cosA);
        xi := Round(pt.X - dx);
        yi := Round(pt.Y - dy);
        if (xi < 0) or (xi >= Width) or (yi < 0) or (yi >= Height) then
          dstColor^ := clNone32
        else
          dstColor^ := fPixels[xi + yi * Width];
        inc(dstColor);
      end;
  end;
  SetSize(newWidth, newHeight);
  fPixels := tmp;
end;
//------------------------------------------------------------------------------

procedure TImage32.RotateRect(const rec: TRect;
  angleRads: single; eraseColor: TColor32 = 0);
var
  tmp: TImage32;
  rec2: TRect;
  midPoint: TPointD;
begin
  midPoint.X := (rec.Left + rec.Right) /2;
  midPoint.Y := (rec.Top + rec.Bottom) /2;
  //create a tmp image with a copy of the pixels inside rec ...
  tmp := TImage32.Create(self, rec);
  try
    tmp.Rotate(angleRads);
    //since rotating also resizes, get a centered
    //(clipped) rect of the rotated pixels ...
    rec2.Left := (tmp.Width - RectWidth(rec)) div 2;
    rec2.Top := (tmp.Height - RectHeight(rec)) div 2;
    rec2.Right := rec2.Left + RectWidth(rec);
    rec2.Bottom := rec2.Top + RectHeight(rec);
    //finally move the rotated rec back to the image ...
    if eraseColor <> clNone32 then
      FillRect(rec, eraseColor);
    CopyFrom(tmp, rec2, rec);
  finally
    tmp.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Skew(dx,dy: double);
var
  i,j, newWidth, newHeight: integer;
  x,y: double;
  tmpPxls: TArrayOfColor32;
  pcDst: PColor32;
begin
  if IsEmpty or ((dx = 0) and (dy = 0)) then Exit;
  newWidth := Width + Ceil(Abs(dx));
  newHeight := Height + Ceil(Abs(dy));
  SetLength(tmpPxls, newWidth * newHeight);
  pcDst := @tmpPxls[0];
  for i := 0 to newHeight - 1 do
    for j := 0 to newWidth - 1 do
    begin
      x := Min(0,  dx) + j - i * dx/newHeight;
      y := Min(0, -dy) + j * dy/newWidth + i;
      pcDst^ := GetWeightedPixel(Round(x*$100), Round(y*$100));
      inc(pcDst);
    end;
  SetSize(newWidth, newHeight);
  fPixels := tmpPxls;
end;
//------------------------------------------------------------------------------

procedure TImage32.BoxBlur(rect: TRect; radius, repeats: Integer);
begin
  //nb: BoxBlur can achieve a very good approximation of a GaussianBlur in a
  //shorter time by using a smaller radius and repeating several (2-3) times.
  if radius < 1 then Exit;
  for repeats := 0 to repeats do
  begin
    BlurHorizontal(rect, radius);
    BlurVertical(rect, radius);
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.BlurHorizontal(rect: TRect; radius: Integer);
var
  i, x,y, widthLess1: Integer;
  pc0, pcB, pcF: PColor32;
  wc: TWeightedColor;
  buffer: TArrayOfColor32;
begin
  IntersectRect(rect, Bounds, rect);
  if IsEmptyRect(rect) or (radius < 1) then Exit;
  widthLess1 := RectWidth(rect) -1;
  radius := ClampRange(radius, 1, Min(widthLess1, MaxBlur));
  setLength(buffer, widthLess1 +1);

  for y := 0 to RectHeight(rect) -1 do
  begin
    pc0 := @fPixels[(rect.Top + y) * width + rect.Left];
    //copy the row's pixels into a buffer because blurring spoils the color
    //of pixels being removed from the kernel (especially with larger radii).
    Move(pc0^, buffer[0], RectWidth(rect) * SizeOf(TColor32));

    wc.Reset;
    //build the horizontal kernel (wc) using the first pixel in each row ...
    wc.Add(pc0^, 1);
    pcB := @buffer[0]; pcF := pcB;
    for i := 1 to radius do
    begin
      inc(pcF);
      wc.Add(pcF^, 1);
    end;
    pc0^ := wc.Color; //updates the first pixel in the row

    inc(pcF);
    //pcB & pcF now both point to the color buffer, representing the
    //left-most and right-most kernel pixels respectively

    //process the rest of the row, updating the kernel each time - removing
    //the old left-most pixel in the kernel and adding the new right-most one.
    for x := 1 to widthLess1 do
    begin
      if x > radius then
      begin
        wc.Subtract(pcB^, 1);
        inc(pcB);
      end;
      if x < (widthLess1 - radius) then
      begin
        wc.add(pcF^, 1);
        inc(pcF)
      end;
      inc(pc0);
      pc0^ := wc.Color;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.BlurVertical(rect: TRect; radius: Integer);
var
  i, x,y, heightLess1: Integer;
  pc0, pcB, pcF: PColor32;
  wc: TWeightedColor;
  buffer: TArrayOfColor32;
begin
  heightLess1 := RectHeight(rect) -1;
  radius := ClampRange(radius, 1, Min(heightLess1, MaxBlur));
  setLength(buffer, heightLess1 +1);

  for x := 0 to RectWidth(rect) -1 do
  begin
    pc0 := @fPixels[(rect.Top * Width) + rect.Left + x];
    //build the vertical pixel buffer ...
    pcF := pc0;
    for i := 0 to heightLess1 do
    begin
      buffer[i] := pcF^;
      inc(pcF, Width);
    end;

    wc.Reset;
    wc.Add(pc0^, 1);
    pcB := @buffer[0]; pcF := pcB;
    for i := 1 to radius do
    begin
      inc(pcF);
      wc.Add(pcF^, 1);
    end;
    pc0^ := wc.Color;
    inc(pcF);
    for y := 1 to heightLess1 do
    begin
      if y > radius then
      begin
        wc.Subtract(pcB^, 1);
        inc(pcB);
      end;
      if y < (heightLess1 - radius) then
      begin
        wc.add(pcF^, 1);
        inc(pcF);
      end;
      inc(pc0, Width);
      pc0^ := wc.Color;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Sharpen(radius: Integer; amount: Integer);
var
  i: Integer;
  amt: double;
  weightAmount: array [-255 .. 255] of Integer;
  bmpBlur: TImage32;
  pColor, pBlur: PARGB;
begin
  amt := ClampRange(amount/20, 0.05, 5);
  radius := ClampRange(radius, 1, 10);
  for i := -255 to 255 do
    weightAmount[i] := Round(amt * i);

  bmpBlur := TImage32.Create(self); //clone self
  try
    pColor := PARGB(pixelBase);
    bmpBlur.GaussianBlur(Bounds, radius);
    pBlur := PARGB(bmpBlur.pixelBase);
    for i := 1 to Width * Height do
    begin
      if pColor.A > 0 then
      begin
        pColor.R := ClampByte(pColor.R  + weightAmount[pColor.R - pBlur.R]);
        pColor.G := ClampByte(pColor.G  + weightAmount[pColor.G - pBlur.G]);
        pColor.B := ClampByte(pColor.B  + weightAmount[pColor.B - pBlur.B]);
      end;
      Inc(pColor); Inc(pBlur);
    end;
  finally
    bmpBlur.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TImage32.Emboss(radius: Integer;
  depth: Integer; luminance: Integer; preserveColor: Boolean);
var
  yy,xx, x,y: Integer;
  b: byte;
  kernel: array [0 .. MaxBlur, 0 .. MaxBlur] of Integer;
  wca: TArrayOfWeightedColor;
  pc0, pcf, pcb: PColor32; //pointers to pixels (forward & backward in kernel)
  pw0, pw: PWeightedColor; //pointers to weight
  customGray: TColor32;
const
  maxDepth = 50;
begin
  //grayscale luminance as percent where 0% is black and 100% is white
  //(luminance is ignored when preserveColor = true)
  luminance := ClampRange(luminance, 0, 100);
  b := luminance *255 div 100;
  customGray := $FF000000 + b shl 16 + b shl 8 + b;

  ClampRange(radius, 1, 5);
  inc(depth);
  ClampRange(depth, 2, maxDepth);

  kernel[0][0] := 1;
  for y := 1 to radius do
    for x := 1 to radius do
      kernel[y][x] := depth;

  //nb: dynamic arrays are zero-initialized (unless they're a function result)
  SetLength(wca, Width * Height);

  pc0 := IncPColor32(PixelBase, radius * width);
  pw0 := @wca[radius * width];
  for y := radius to height -1 - radius do
  begin
    for x := radius to width -1 - radius do
    begin
      pw := IncPWeightColor(pw0, x);
      pcb := IncPColor32(pc0, x - 1);
      if preserveColor then
      begin
        pcf := IncPColor32(pc0, x);
        pw^.Add(pcf^, kernel[0,0]);
        inc(pcf);
      end else
      begin
        pw^.Add(customGray, kernel[0,0]);
        pcf := IncPColor32(pc0, x + 1);
      end;

      //parse the kernel ...
      for yy := 1 to radius do
      begin
        for xx := 1 to radius do
        begin
          pw^.Subtract(Gray(pcf^), kernel[yy,xx]);
          pw^.Add(Gray(pcb^), kernel[yy,xx]);
          dec(pcb); inc(pcf);
        end;
        dec(pcb, Width - radius);
        inc(pcf, Width - radius);
      end;
    end;
    inc(pc0, Width);
    inc(pw0, Width);
  end;

  for x := 0 to width * height - 1 do
    fPixels[x] := wca[x].Color or $FF000000;
end;
//------------------------------------------------------------------------------

procedure TImage32.ScaleAlpha(scale: double);
var
  i: Integer;
  pb: PARGB;
begin
  pb := PARGB(PixelBase);
  for i := 0 to Width * Height - 1 do
  begin
    pb.A := ClampByte(Round(pb.A * scale));
    inc(pb);
  end;
end;

//------------------------------------------------------------------------------
// TWeightedColor record
//------------------------------------------------------------------------------

procedure TWeightedColor.Reset;
begin
  fAddCount := 0;
  fAlphaTot := 0;
  fColorTotR := 0;
  fColorTotG := 0;
  fColorTotB := 0;
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.AddWeight(weight: Integer);
begin
  inc(fAddCount, weight);
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Add(c: TColor32; weight: Integer);
var
  a: Integer;
  argb: TARGB absolute c;
begin
  inc(fAddCount, weight);
  a := weight * argb.A;
  if a = 0 then Exit;
  inc(fAlphaTot, a);
  inc(fColorTotB, (a * argb.B));
  inc(fColorTotG, (a * argb.G));
  inc(fColorTotR, (a * argb.R));
end;
//------------------------------------------------------------------------------

procedure TWeightedColor.Subtract(c: TColor32; weight: Integer);
var
  a: Integer;
  argb: TARGB absolute c;
begin
  dec(fAddCount, weight);
  a := weight * argb.A;
  if a = 0 then Exit;
  dec(fAlphaTot, a);
  dec(fColorTotB, (a * argb.B));
  dec(fColorTotG, (a * argb.G));
  dec(fColorTotR, (a * argb.R));
end;
//------------------------------------------------------------------------------

function TWeightedColor.GetColor: TColor32;
var
  a: byte;
  halfAlphaTot: Integer;
  argb: TARGB absolute result;
begin
  result := clNone32;
  if (fAlphaTot <= 0) or (fAddCount <= 0) then Exit;
  a := DivRound(fAlphaTot, fAddCount);
  if (a = 0) then Exit;
  argb.A := a;
  halfAlphaTot := fAlphaTot div 2;
  //nb: alpha weighting is applied to colors when added
  //so we now need to div by fAlphaTot (with rounding) here ...
  argb.R := ClampByte((fColorTotR + halfAlphaTot) div fAlphaTot);
  argb.G := ClampByte((fColorTotG + halfAlphaTot) div fAlphaTot);
  argb.B := ClampByte((fColorTotB + halfAlphaTot) div fAlphaTot);
end;

//------------------------------------------------------------------------------
// TImageFormat methods
//------------------------------------------------------------------------------

function TImageFormat.LoadFromFile(const filename: string;
  img32: TImage32): Boolean;
var
  fs: TFileStream;
begin
  result := FileExists(filename);
  if not result then Exit;
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(fs, img32);
  finally
    fs.Free;
  end;
end;
//------------------------------------------------------------------------------

function TImageFormat.SaveToFile(const filename: string;
  img32: TImage32): Boolean;
var
  fs: TFileStream;
begin
  result := DirectoryExists(ExtractFilePath(filename));
  if not result then Exit;
  fs := TFileStream.Create(filename, fmCreate);
  try
    SaveToStream(fs, img32);
  finally
    fs.Free;
  end;
end;

//------------------------------------------------------------------------------
// Initialization functions
//------------------------------------------------------------------------------

procedure SetupImageFormatClassList;
begin
  ImageFormatClassList := TStringList.Create;
  ImageFormatClassList.Duplicates := dupIgnore;
end;
//------------------------------------------------------------------------------

procedure MakeBlendTables;
var
  i,j: Integer;
begin
  for j := 0 to 255 do MulTable[0, j] := 0;
  for i := 0 to 255 do MulTable[i, 0] := 0;
  for j := 0 to 255 do DivTable[0, j] := 0;
  for i := 0 to 255 do DivTable[i, 0] := 0;

  for i := 1 to 255 do
    for j := 1 to 255 do
    begin
      MulTable[i, j] := Round(i * j * div255);
      if i >= j then
        DivTable[i, j] := 255 else
        DivTable[i, j] := Round(i * $FF / j);
    end;
end;
//------------------------------------------------------------------------------

procedure GetDPI;
var
  dc: HDC;
begin
  dc := GetDC(0);
  ScreenPixelsY := GetDeviceCaps(dc, LOGPIXELSY);
  DpiScale := ScreenPixelsY / 96;
  ReleaseDC(0, dc);
end;

//------------------------------------------------------------------------------

initialization
  SetupImageFormatClassList;
  MakeBlendTables;
  GetDPI;

finalization
  ImageFormatClassList.Free;

end.
