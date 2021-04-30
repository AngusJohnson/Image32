unit Image32_SVG_Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.24                                                            *
* Date      :  30 April 2021                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2021                                         *
*                                                                              *
* Purpose   :  Read SVG files                                                  *
*              To propery read SVG files and support all the features of the   *
*              vers. 2 specification is a huge task. So while this unit does   *
*              fairly decent job of reading most SVG files, it remains mostly  *
*              a proof of concept with numerous deficiencies including:        *
*              consistent inheritance of default values; proper handling of    *
*              measurement units (cms, ems etc.); managing different line join *
*              and line end styles; slow rendering of blurs etc. etc.          *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*******************************************************************************)

interface

{$I Image32.inc}

//https://www.google.com/get/noto/
{$R Image32_SVG_Reader_Noto-Fonts.res}
{$R Image32_SVG_Reader_Noto-Sans-Extra.res}
{$R Image32_SVG_Reader_Noto-Serif-Extra.res}

uses
  SysUtils, Classes, Types, Math,
  Image32, Image32_Vector, Image32_Draw, Image32_Transform, Image32_Ttf;

type
  TGradientElement = class;
  TMarkerElement   = class;
  TFilterElement   = class;
  TClipPathElement = class;

  TFontFamily = (ffSansSerif, ffSerif, ffMonospace);
  TFontSyle = (fsBold, fsItalic);
  TFontSyles = set of TFontSyle;
  TTextAlign = (taLeft, taMiddle, taRight);

  TFontInfo = record
    family: TFontFamily;
    size: double;
    styles: TFontSyles;
    align: TTextAlign;
  end;

  TDrawInfo = record
  public
    fillColor     : TColor32;
    fillRule      : TFillRule;
    fillGradEl    : TGradientElement;
    strokeColor   : TColor32;
    strokeWidth   : double;
    strokeGradEl  : TGradientElement;
    dashArray     : TArrayOfDouble;
    dashOffset    : double;
    markerStart   : TMarkerElement;
    markerMiddle  : TMarkerElement;
    markerEnd     : TMarkerElement;
    filterEl      : TFilterElement;
    clipPathEl    : TClipPathElement;
    opacity       : Byte;
    //lineCap       : TEndStyle;     //stroke-linecap
    //lineJoin      : TJoinStyle;    //stroke-linejoin
    //miterLim      : double;        //stroke-miterlimit
    matrix        : TMatrixD;
    hidden        : Boolean;
  end;

  TAnsiRec = record
    ansi: AnsiString;
  end;
  PAnsiRec = ^TAnsiRec;

  TStrAnsiList = class(TStringList)
  public
    function Add(const str: string; const ansi: AnsiString): integer; reintroduce;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    destructor Destroy; override;
  end;

  TElement = class;

  PAttrib = ^TAttrib;
  TAttrib = record
    aOwnerEl  : TElement;
    aName     : PAnsiChar;
    aNameLen  : integer;
    aValue    : PAnsiChar;
    aValueLen : integer;
  end;
  TArrayOfAttrib = array of TAttrib;

  TSvgReader = class;

  TElementClass = class of TElement;

  TElement = class
  private
    fParent         : TElement;
    fReader         : TSvgReader;
    fChilds         : TList;
    fIsValid        : Boolean;
    fHasContent     : Boolean;
    fName           : PAnsiChar;
    fNameLen        : integer;
    fNameHash       : Cardinal;
    fAttribs        : TArrayOfAttrib;
    //nb: non-shape elements may still contain
    //drawing info (transforms etc)
    fDrawInfo       : TDrawInfo;
    fCurrent        : PAnsiChar;
    fCurrentEnd     : PAnsiChar;
    function LoadChild: TElement;
    function HashCurrentWord(out hash: Cardinal): Boolean;
    function GetName(out name: PAnsiChar;
      out nameLen: integer): Boolean;
    function GetAttribValue(out value: PAnsiChar; out valueLen: integer): Boolean;
    function FindRefElement(refname: PAnsiChar; refNameLen: integer): TElement;
    procedure ProcessAttrib(const attrib: PAttrib);
    function PeekNextChar: AnsiChar;
    procedure ParseClassAttrib(classAttrib: PAttrib);
    procedure ParseStyle(classStyle: PAnsiChar; len: integer);
    procedure ParseTransform(transform: PAnsiChar; len: integer);
    function AddAttribute: PAttrib;
  protected
    function LoadAttributes: Boolean; virtual;
    procedure LoadContent; virtual;
    procedure AssignTo(var other: TElement); virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); virtual;
    destructor  Destroy; override;
  end;

  TRootElement = class(TElement)
  protected
    viewbox: TRectD;
    width: double;
    height: double;
  public
    constructor Create(reader: TSvgReader; hashName: Cardinal); reintroduce;
  end;

  TDefsElement = class(TElement)
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TStyleElement = class(TElement)
  protected
    procedure LoadContent; override;
  end;

  TUseElement = class(TElement)
  protected
    pt: TPointD;
    refEl: TElement;
    surrogate: TElement;
    function LoadAttributes: Boolean; override;
  end;

  TShapeElement = class(TElement)
  protected
    drawPathsF: TPathsD;
    drawPathsO: TPathsD;
    drawPathsC: TPathsD;
    function IsFilled: Boolean;
    function IsStroked: Boolean;
    function GetVal(out val: double): Boolean;

    procedure GetDrawPaths(ArcScale: double); virtual; abstract;
    procedure DrawFilled(img: TImage32; matrix: TMatrixD); virtual;
    procedure DrawStroke(img: TImage32; matrix: TMatrixD; isClosed: Boolean); virtual;
    procedure Draw(img, tmpImg: TImage32; RootMatrix: TMatrixD); virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TDsegType = (dsMove, dsLine, dsHorz, dsVert, dsArc,
    dsQBez, dsCBez, dsQSpline, dsCSpline, dsClose);

  PDpathSeg = ^TDpathSeg;
  TDpathSeg = record
    segType : TDsegType;
    vals    : TArrayOfDouble;
  end;

  PDpath = ^TDpath;
  TDpath = record
    firstPt   : TPointD;
    isClosed  : Boolean;
    segs      : array of TDpathSeg;
  end;
  TDpaths = array of TDpath;

  TPathElement = class(TShapeElement)
  private
    currSeg     : PDpathSeg;
    currSegCap  : integer;
    currSegCnt  : integer;
    currSegType : TDsegType;
    currDpath   : PDpath;
    dpaths      : TDpaths;
    lastPt      : TPointD;
    function GetSegType(out isRelative: Boolean): Boolean;
    function GetSingleDigit(out digit: integer): Boolean;
    procedure StartNewDpath;
    procedure StartNewSeg(segType: TDsegType);
    procedure AddSegValue(val: double);
    procedure AddSegPoint(const pt: TPointD);
    function Get2Num(var pt: TPointD; isRelative: Boolean): Boolean;
    procedure Flatten(index: integer;
      out path: TPathD; out isClosed: Boolean);
  protected
    procedure ParseD(attrib: PAttrib);
    procedure GetDrawPaths(ArcScale: double); override;
  end;

  TPolyElement = class(TShapeElement) //polyline or polygon
  protected
    path: TPathD;
    procedure ParsePoints(attrib: PAttrib);
    procedure GetDrawPaths(ArcScale: double); override;
  end;

  TLineElement = class(TShapeElement)
  protected
    path: TPathD;
    procedure GetDrawPaths(ArcScale: double); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TCircleElement = class(TShapeElement)
  protected
    centerPt  : TPointD;
    radius    : double;
    procedure GetDrawPaths(ArcScale: double); override;
  end;

  TEllipseElement = class(TShapeElement)
  protected
    centerPt  : TPointD;
    radius    : TPointD;
    procedure GetDrawPaths(ArcScale: double); override;
  end;

  TRectElement = class(TShapeElement)
  protected
    recWH     : TRectWH;
    radius    : TPointD;
    procedure GetDrawPaths(ArcScale: double); override;
  end;

  //TTextElement: although this is a TShapeElement descendant,
  //it's really only a container for 'subtext' and 'tspan' elements
  TTextElement = class(TShapeElement)
  protected
    pt        : TPointD;
    delta     : TPointD;
    tmpX      : double;
    fontInfo  : TFontInfo;
    procedure GetDrawPaths(ArcScale: double); override;
    procedure AddSubtext(text: PAnsiChar; len: integer);
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TSubtextElement   = class(TShapeElement)
  protected
    text: PAnsiChar;
    textLen: integer;
    procedure GetDrawPaths(ArcScale: double); override;
  end;

  TTSpanElement     = class(TSubtextElement)
  protected
    pt        : TPointD;
    delta     : TPointD;
    fontInfo  : TFontInfo;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TMarkerElement = class(TShapeElement)
  private
    points: TPathD;
  protected
    ref: TPointD;
    width: double;
    height: double;
    viewbox: TRectD;
    angle: double;
    callerEl: TShapeElement;
    procedure SetSoloPoint(const pt: TPointD; angle: double);
    procedure SetPoints(const points: TPathD);
    procedure Draw(img, tmpImg: TImage32; RootMatrix: TMatrixD); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TSvgColorStop = record
    offset: double;
    color: TColor32;
  end;
  TSvgColorStops = array of TSvgColorStop;

  TGradientElement = class(TElement)
  protected
    stops: TSvgColorStops;
    gradientUnits: Cardinal;
    procedure LoadContent; override;
    procedure AddStop(color: TColor32; offset: double);
    procedure AssignTo(var other: TElement); override;
    function PrepareGradientRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRect): Boolean; virtual;
  end;

  TRadGradElement = class(TGradientElement)
  protected
    F, C: TPointD;
    function PrepareGradientRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRect): Boolean; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TLinGradElement = class(TGradientElement)
  protected
    startPt, endPt: TPointD;
    procedure AssignTo(var other: TElement); override;
    function PrepareGradientRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRect): Boolean; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TGradStopElement = class(TElement)
  protected
    offset: double;
    color: TColor32;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TFilterElement = class(TElement)
  protected
    recWH: TRectWH;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TGaussianElement  = class(TElement)
  protected
    stdDev: double;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TClipPathElement = class(TShapeElement)
  protected
    procedure GetDrawPaths(ArcScale: double); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TAttribFunc = procedure (attrib: PAttrib);

  TSvgReader = class
  private
    fMemStream        : TMemoryStream;
    fEndStream        : PAnsiChar;
    fIdList           : TStringList;
    fShapesList       : TList;
    fClassStylesList  : TStrAnsiList;

    fLinGradRenderer  : TLinearGradientRenderer;
    fRadGradRenderer  : TSvgRadialGradientRenderer;

    //multiple font readers
    fSansFontReader       : TFontReader;
    fSerifFontReader      : TFontReader;
    fMonoFontReader       : TFontReader;

    fSansBoldReader       : TFontReader;
    fSansItalicReader     : TFontReader;
    fSansBoldItalReader   : TFontReader;

    fSerifBoldReader      : TFontReader;
    fSerifItalicReader    : TFontReader;
    fSerifBoldItalReader  : TFontReader;

    //and font caches
    fSansFontCache        : TGlyphCache;
    fSerifFontCache       : TGlyphCache;
    fMonoFontCache        : TGlyphCache;

    fRootElement      : TRootElement;
  protected
    function GetSvgStart(out svgStart: PAnsiChar): Boolean;
    property RadGradRenderer: TSvgRadialGradientRenderer read fRadGradRenderer;
    property LinGradRenderer: TLinearGradientRenderer read fLinGradRenderer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DrawImage(img: TImage32; scaleToImage: Boolean);
    function LoadFromStream(stream: TStream): Boolean;
    function LoadFromFile(const filename: string): Boolean;
  end;

var
  defaultFontHeight: double;

implementation

uses
  Image32_Extra;

type
  TColorConst = record
    ColorName : string;
    ColorValue: Cardinal;
  end;

const
  buffSize = 32;

  defaultDrawInfo: TDrawInfo =
    (fillColor: clBlack32; fillRule: frNonZero; fillGradEl: nil;
    strokeColor: clNone32; strokeWidth: 1.0;
    dashArray: nil; dashOffset: 0;
    markerStart: nil; markerMiddle: nil; markerEnd: nil;
    filterEl: nil;  clipPathEl: nil; opacity: 255;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); hidden: false);

  defaultFontInfo: TFontInfo =
    (family: ffSansSerif; size: 10; styles: []; align: taLeft);

  //include lots of color constants
  {$I Image32_SVG_ColorConsts.inc}

  //include many string hash constants
  {$I Image32_SVG_HashConsts.inc}

var
  ColorConstList : TStringList;
  AttribFuncList : TStringList;
  LowerCaseTable : array[#0..#255] of AnsiChar;

resourcestring
  rsListBoundsError     = 'List index out of bounds (%d)';

//------------------------------------------------------------------------------
// TStrStrList
//------------------------------------------------------------------------------

function TStrAnsiList.Add(const str: string; const ansi: AnsiString): integer;
var
  i: integer;
  sr: PAnsiRec;
begin
  Result := IndexOf(str);
  if (Result >= 0) then
  begin
    sr := PAnsiRec(Objects[Result]);
    i := Length(sr.ansi);
    if sr.ansi[i] <> ';' then
      sr.ansi := sr.ansi + ';' + ansi else
      sr.ansi := sr.ansi + ansi;
  end else
  begin
    new(sr);
    sr.ansi := ansi;
    Result := inherited AddObject(str, Pointer(sr));
  end;
end;
//------------------------------------------------------------------------------

procedure TStrAnsiList.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    Dispose(PAnsiRec(Objects[i]));
  inherited Clear;
end;
//------------------------------------------------------------------------------

procedure TStrAnsiList.Delete(Index: Integer);
begin
  if (index < 0) or (index >= Count) then
    Error(@rsListBoundsError, index);
  Dispose(PAnsiRec(Objects[index]));
  inherited Delete(Index);
end;
//------------------------------------------------------------------------------

destructor TStrAnsiList.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function SkipBlanks(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
begin
  while (current < currentEnd) and (current^ <= #32) do inc(current);
  Result := (current < currentEnd);
end;
//------------------------------------------------------------------------------

function TrimBlanks(var current: PAnsiChar; var length: integer): Boolean;
var
  endC: PAnsiChar;
begin
  while (length > 0) and (current^ <= #32) do
  begin
    inc(current); dec(length);
  end;
  Result := length > 0;
  if not Result then Exit;
  endC := current + length -1;
  while endC^ <= #32 do
  begin
    dec(endC); dec(length);
  end;
end;
//------------------------------------------------------------------------------

function SkipBlanksIgnoreComma(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
begin
  Result := SkipBlanks(current, currentEnd);
  if not Result or (current^ <> ',') then Exit;
  inc(current);
  Result := SkipBlanks(current, currentEnd);
end;
//------------------------------------------------------------------------------

function SkipStyleBlanks(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
var
  inComment: Boolean;
begin
  //nb: style content may include /* ... */ comment blocks
  inComment := false;
  while (current < currentEnd) do
  begin
    if inComment then
    begin
      if (current^ = '*') and ((current +1)^ = '/')  then
      begin
        inComment := false;
        inc(current);
      end;
    end
    else if (current^ > #32) then
    begin
      inComment := (current^ = '/') and ((current +1)^ = '*');
      if not inComment then break;
    end;
    inc(current);
  end;
  Result := (current < currentEnd);
end;
//------------------------------------------------------------------------------

function GetNextChar(var current: PAnsiChar; currentEnd: PAnsiChar): AnsiChar;
begin
  if SkipBlanks(current, currentEnd) then
  begin
    Result := current^;
    inc(current);
  end
  else Result := #0;
end;
//------------------------------------------------------------------------------

function Utf8String_(pName: PAnsiChar; len: integer): AnsiString;
begin
  SetLength(Result, len);
  Move(pName^, Result[1], len);
end;
//--------------------------------------------------------------------------

function LowercaseAnsi(pName: PAnsiChar; len: integer): AnsiString;
var
  i: integer;
begin
  SetLength(Result, len);
  for i := 1 to len do
  begin
    Result[i] := LowerCaseTable[pName^];
    inc(pName);
  end;
end;
//--------------------------------------------------------------------------

function IsAlpha(c: AnsiChar): Boolean;
begin
  if c >= 'a' then dec(c, $20);
  Result := (c >= 'A') and (c <= 'Z');
end;
//------------------------------------------------------------------------------

function GetNameLength(var c: PAnsiChar; endC: PAnsiChar): integer;
var
  startC: PAnsiChar;
const
  validNonFirstChars =  ['0'..'9','A'..'Z','a'..'z','_',':','-'];
begin
  Result := 0;
  if not IsAlpha(c^) then Exit;
  startC := c; inc(c);
  while (c < endC) and CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - startC;
end;
//------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}
function GetHashedName(c: PAnsiChar; length: integer): cardinal; overload;
var
  i: integer;
begin
  //Bob Jenkins' "One at A Time" hash
  //https://en.wikipedia.org/wiki/Jenkins_hash_function
  Result := 0;
  for i := 1 to length do
  begin
    Result := (Result + Ord(LowerCaseTable[c^]));
    Result := Result + (Result shl 10);
    Result := Result xor (Result shr 6);
    inc(c);
  end;
  Result := Result + (Result shl 3);
  Result := Result xor (Result shr 11);
  Result := Result + (Result shl 15);
end;
//------------------------------------------------------------------------------
{$OVERFLOWCHECKS ON}

function GetHashedName(var c: PAnsiChar; endC: PAnsiChar): cardinal; overload;
var
  len: integer;
  startC: PAnsiChar;
begin
  startC := c;
  len := GetNameLength(c, endC);
  if len = 0 then Result := 0
  else Result := GetHashedName(startC, len);
end;
//------------------------------------------------------------------------------

function HashToElementClass(hash: Cardinal): TElementClass;
begin
  case hash of
    hClippath       : Result := TClipPathElement;
    hCircle         : Result := TCircleElement;
    hDefs           : Result := TDefsElement;
    hEllipse        : Result := TEllipseElement;
    hFilter         : Result := TFilterElement;
    hFeGaussianBlur : Result := TGaussianElement;
    hG              : Result := TElement;
    hLine           : Result := TLineElement;
    hLineargradient : Result := TLinGradElement;
    hMarker         : Result := TMarkerElement;
    hPath           : Result := TPathElement;
    hPolyline       : Result := TPolyElement;
    hPolygon        : Result := TPolyElement;
    hRadialgradient : Result := TRadGradElement;
    hRect           : Result := TRectElement;
    hStop           : Result := TGradStopElement;
    hStyle          : Result := TStyleElement;
    hText           : Result := TTextElement;
    hTSpan          : Result := TTSpanElement;
    hUse            : Result := TUseElement;
    else              Result := TElement;
  end;
end;
//------------------------------------------------------------------------------

function GetStyleNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
var
  startC: PAnsiChar;
const
  validNonFirstChars =  ['0'..'9','A'..'Z','a'..'z','-'];
begin
  Result := 0;
  //nb: style names may start with a hyphen
  if (c^ = '-') then
  begin
    if not IsAlpha((c+1)^) then Exit;
  end
  else if not IsAlpha(c^) then Exit;

  startC := c; inc(c);
  while (c < endC) and CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - startC;
end;
//------------------------------------------------------------------------------

function TrigClampVal(val: double): double;
begin
  //force : -1 <= val <= 1
  if val < -1 then Result := -1
  else if val > 1 then Result := 1
  else Result := val;
end;
//------------------------------------------------------------------------------

function  Radian2(vx, vy: double): double;
begin
  Result := ArcCos( TrigClampVal(vx / Sqrt( vx * vx + vy * vy)) );
  if( vy < 0.0 ) then Result := -Result;
end;
//------------------------------------------------------------------------------

function  Radian4(ux, uy, vx, vy: double): double;
var
  dp, md: double;
begin
  dp := ux * vx + uy * vy;
  md := Sqrt( ( ux * ux + uy * uy ) * ( vx * vx + vy * vy ) );
    Result := ArcCos( TrigClampVal(dp / md) );
    if( ux * vy - uy * vx < 0.0 ) then Result := -Result;
end;
//------------------------------------------------------------------------------

//https://stackoverflow.com/a/12329083
function SvgArc(const p1, p2: TPointD; radii: TPointD;
  phi_rads: double; fA, fS: boolean;
  out startAngle, endAngle: double; out rec: TRectD): Boolean;
var
  x1_, y1_, rxry, rxy1_, ryx1_, s_phi, c_phi: double;
  hd_x, hd_y, hs_x, hs_y, sum_of_sq, lambda, coe: double;
  cx, cy, cx_, cy_, xcr1, xcr2, ycr1, ycr2, deltaAngle: double;
const
  twoPi: double = PI *2;
begin
    Result := false;
    if (radii.X < 0) then radii.X := -radii.X;
    if (radii.Y < 0) then radii.Y := -radii.Y;
    if (radii.X = 0) or (radii.Y = 0) then Exit;

    SinCos(phi_rads, s_phi, c_phi);;
    hd_x := (p1.X - p2.X) / 2.0; // half diff of x
    hd_y := (p1.Y - p2.Y) / 2.0; // half diff of y
    hs_x := (p1.X + p2.X) / 2.0; // half sum of x
    hs_y := (p1.Y + p2.Y) / 2.0; // half sum of y

    // F6.5.1
    x1_ := c_phi * hd_x + s_phi * hd_y;
    y1_ := c_phi * hd_y - s_phi * hd_x;

    // F.6.6 Correction of out-of-range radii
    // Step 3: Ensure radii are large enough
    lambda := (x1_ * x1_) / (radii.X * radii.X) +
      (y1_ * y1_) / (radii.Y * radii.Y);
    if (lambda > 1) then
    begin
      radii.X := radii.X * Sqrt(lambda);
      radii.Y := radii.Y * Sqrt(lambda);
    end;

    rxry := radii.X * radii.Y;
    rxy1_ := radii.X * y1_;
    ryx1_ := radii.Y * x1_;
    sum_of_sq := rxy1_ * rxy1_ + ryx1_ * ryx1_; // sum of square
    if (sum_of_sq = 0) then Exit;

    coe := Sqrt(Abs((rxry * rxry - sum_of_sq) / sum_of_sq));
    if (fA = fS) then coe := -coe;

    // F6.5.2
    cx_ := coe * rxy1_ / radii.Y;
    cy_ := -coe * ryx1_ / radii.X;

    // F6.5.3
    cx := c_phi * cx_ - s_phi * cy_ + hs_x;
    cy := s_phi * cx_ + c_phi * cy_ + hs_y;

    xcr1 := (x1_ - cx_) / radii.X;
    xcr2 := (x1_ + cx_) / radii.X;
    ycr1 := (y1_ - cy_) / radii.Y;
    ycr2 := (y1_ + cy_) / radii.Y;

    // F6.5.5
    startAngle := Radian2(xcr1, ycr1);
    NormalizeAngle(startAngle);

    // F6.5.6
    deltaAngle := Radian4(xcr1, ycr1, -xcr2, -ycr2);
    while (deltaAngle > twoPi) do deltaAngle := deltaAngle - twoPi;
    while (deltaAngle < 0.0) do deltaAngle := deltaAngle + twoPi;
    if not fS then deltaAngle := deltaAngle - twoPi;
    endAngle := startAngle + deltaAngle;
    NormalizeAngle(endAngle);

    rec.Left := cx - radii.X;
    rec.Right := cx + radii.X;
    rec.Top := cy - radii.Y;
    rec.Bottom := cy + radii.Y;

    Result := true;
end;
//------------------------------------------------------------------------------

function HexByteToInt(h: AnsiChar): Cardinal;
begin
  case h of
    '0'..'9': Result := Ord(h) - Ord('0');
    'A'..'F': Result := 10 + Ord(h) - Ord('A');
    'a'..'f': Result := 10 + Ord(h) - Ord('a');
    else Result := 0;
  end;
end;
//------------------------------------------------------------------------------

function AttribToFillRule(const attrib: TAttrib): TFillRule;
begin
  Result := frNonZero; //default
  if (attrib.aValueLen = 0) then Exit;
  if LowerCaseTable[attrib.aValue[0]] = 'e' then
    Result := frEvenOdd;
end;
//------------------------------------------------------------------------------

function GetNextAlphaWord(var current: PAnsiChar; currentEnd: PAnsiChar;
  out word: AnsiString): Boolean;
var
  i: integer;
  c: PAnsiChar;
begin
  Result := SkipBlanks(current, currentEnd);
  if not Result then Exit;

  i := 0; c := current;
  while (current < currentEnd) and
    (LowerCaseTable[current^] >= 'a') and
    (LowerCaseTable[current^] <= 'z') do
  begin
    inc(i);
    inc(current);
  end;
  Result := i > 0;
  if not Result then Exit;
  SetLength(word, i);
  Move(c^, word[1], i);
end;
//------------------------------------------------------------------------------

function GetAttribUrl(attrib: PAttrib): TElement;
var
  current, currentEnd, startC: PAnsiChar;
begin
  Result := nil;
  current := attrib.aValue;
  currentEnd := current + attrib.aValueLen;
  if (GetHashedName(current, currentEnd) <> hUrl) or
    (current^ <> '(') then Exit;
  inc(current);
  SkipBlanks(current, currentEnd);
  startC := current;
  while (current < currentEnd) and (current^ <> ')') do inc(current);
  if (current = currentEnd) then Exit;
  while (current-1)^ <= #32 do dec(current);
  Result := attrib.aOwnerEl.FindRefElement(startC, current - startC);
end;
//------------------------------------------------------------------------------

function NumPending(current, currentEnd: PAnsiChar; ignoreComma: Boolean): Boolean;
begin
  Result := false;

  //skip white space +/- single comma
  if ignoreComma then
  begin
    while (current < currentEnd) and (current^ <= #32) do inc(current);
    if (current^ = ',') then inc(current);
  end;
  while (current < currentEnd) and (current^ <= ' ') do inc(current);
  if (current = currentEnd) then Exit;

  if (current^ = '-') then inc(current);
  if (current^ = '.') then inc(current);
  Result := (current < currentEnd) and (current^ >= '0') and (current^ <= '9');
end;
//------------------------------------------------------------------------------

function GetNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean;
var
  decPos,exp: integer;
  isNeg, expIsNeg: Boolean;
  start: PAnsiChar;
begin
  Result := false;

  //skip white space +/- single comma
  if skipComma then
  begin
    while (current < currentEnd) and (current^ <= #32) do inc(current);
    if (current^ = ',') then inc(current);
  end;
  while (current < currentEnd) and (current^ <= #32) do inc(current);
  if (current = currentEnd) then Exit;

  decPos := -1; exp := Invalid; expIsNeg := false;
  isNeg := current^ = '-';
  if isNeg then inc(current);

  val := 0;
  start := current;
  while current < currentEnd do
  begin
    if Ord(current^) = Ord(FormatSettings.DecimalSeparator) then
    begin
      if decPos >= 0 then break;
      decPos := 0;
    end
    else if (LowerCaseTable[current^] = 'e') then
    begin
      if (current +1)^ = '-' then expIsNeg := true;
      inc(current);
      exp := 0;
    end
    else if (current^ < '0') or (current^ > '9') then
      break
    else if IsValid(exp) then
    begin
      exp := exp * 10 + (Ord(current^) - Ord('0'))
    end else
    begin
      val := val *10 + Ord(current^) - Ord('0');
      if decPos >= 0 then inc(decPos);
    end;
    inc(current);
  end;
  Result := current > start;
  if not Result then Exit;

  if decPos > 0 then val := Power10(val, -decPos);
  if isNeg then val := -val;
  if IsValid(exp) then
  begin
    if expIsNeg then
      val := Power10(val, -exp) else
      val := Power10(val, exp);
  end;
  //convert percentages to fractions
  if (current^ = '%') then
  begin
    val := val *0.00999; //fractionally less than 1.0
    inc(current);
  end;
  //ignore 'px'
  if (current^ = 'p') and  ((current+1)^ = 'x') then
    inc(current, 2);
end;
//------------------------------------------------------------------------------

function IsFraction(val: double): Boolean;
begin
  Result := (val <> 0) and (val > -1) and (val < 1);
end;
//------------------------------------------------------------------------------

function AttribToColor32(attrib: PAttrib; var color: TColor32): Boolean;
var
  i: integer;
  j: Cardinal;
  clr: TColor32;
  alpha: Byte;
  vals: array[0..3] of double;
  p, pEnd: PAnsiChar;
  refElement: TElement;
begin
  Result := false;
  if (attrib.aValueLen < 3) then Exit;
  alpha := color shr 24;

  if (attrib.aValue[0] = 'r') and                 //RGBA()
     (attrib.aValue[1] = 'g') and
     (attrib.aValue[2] = 'b') then
  begin
    pEnd := attrib.aValue + attrib.aValueLen;
    p := attrib.aValue + 3;
    if (p^ = 'a') then inc(p);
    if (GetNextChar(p, pEnd) <> '(') or
      not GetNum(p, pEnd, false, vals[0]) or
      not GetNum(p, pEnd, true, vals[1]) or
      not GetNum(p, pEnd, true, vals[2]) then Exit;
    if GetNum(p, pEnd, false, vals[3]) then
      alpha := 0 else //stops further alpha adjustment
      vals[3] := 255;
    if GetNextChar(p, pEnd) <> ')' then Exit;
    for i := 0 to 3 do if IsFraction(vals[i]) then
      vals[i] := vals[i] * 255;
    color := ClampByte(Round(vals[3])) shl 24 +
      ClampByte(Round(vals[0])) shl 16 +
      ClampByte(Round(vals[1])) shl 8 +
      ClampByte(Round(vals[2]));
  end
  else if (attrib.aValue[0] = 'u') and            //URL()
          (attrib.aValue[1] = 'r') and
          (attrib.aValue[2] = 'l') then
  begin
    refElement := GetAttribUrl(attrib);
    if not assigned(refElement) then Exit;
    with attrib.aOwnerEl do
      if refElement is TGradientElement then
      begin
        if GetHashedName(attrib.aName, attrib.aNameLen) = hFill then
          fDrawInfo.fillGradEl := TGradientElement(refElement) else
          fDrawInfo.strokeGradEl := TGradientElement(refElement);
      end;
  end
  else if (attrib.aValue[0] = '#') then           //#RRGGBB or #RGB
  begin
    if (attrib.aValueLen = 7) then
    begin
      clr := $0;
      for i := 1 to 6 do
        clr := clr shl 4 + HexByteToInt(attrib.aValue[i]);
      clr := clr or $FF000000;
    end
    else if (attrib.aValueLen = 4) then
    begin
      clr := $0;
      for i := 1 to 3 do
      begin
        j := HexByteToInt(attrib.aValue[i]);
        clr := clr shl 4 + j;
        clr := clr shl 4 + j;
      end;
      clr := clr or $FF000000;
    end
    else
      Exit;
    color :=  clr;
  end else                                        //color name lookup
  begin
    with attrib^ do
      i := ColorConstList.IndexOf(string(LowercaseAnsi(aValue, aValueLen)));
    if i < 0 then Exit;
    color := Cardinal(ColorConstList.Objects[i]);
  end;

  //and just in case the opacity has been set before the color
  if (alpha > 0) and (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
  Result := true;
end;
//------------------------------------------------------------------------------

function AttribToFloat(attrib: PAttrib; var value: double): Boolean;
var
  c: PAnsiChar;
begin
  c := attrib.aValue;
  Result := GetNum(c, attrib.aValue + attrib.aValueLen, false, value);
end;
//------------------------------------------------------------------------------

procedure AttribToOpacity(attrib: PAttrib; var color: TColor32);
var
  opacity: double;
begin
  if color = clNone32 then Exit;
  if not AttribToFloat(attrib, opacity) then Exit;
  if (opacity < 0) or (opacity > 1) then Exit;
  color := (color and $FFFFFF) or (Round(255 * opacity) shl 24);
end;
//------------------------------------------------------------------------------

procedure AttribToFontInfo(attrib: PAttrib; var fontInfo: TFontInfo);
var
  c, endC: PAnsiChar;
  hash: Cardinal;
begin
  c := attrib.aValue;
  endC := c + attrib.aValueLen;
  while (c < endC) and SkipBlanks(c, endC) do
  begin
    if c = ';' then
      break
    else if NumPending(c, endC, true) then
      GetNum(c, endC, true, fontInfo.size)
    else
    begin
      hash := GetHashedName(c, endC);
      case hash of
        hSans_045_Serif : fontInfo.family := ffSansSerif;
        hSerif          : fontInfo.family := ffSerif;
        hMonospace      : fontInfo.family := ffMonospace;
        hBold           : Include(fontInfo.styles, fsBold);
        hItalic         : Include(fontInfo.styles, fsItalic);
        hStart          : fontInfo.align := taLeft;
        hMiddle         : fontInfo.align := taMiddle;
        hEnd            : fontInfo.align := taRight;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function MakeDashArray(const da: TArrayOfDouble; scale: double): TArrayOfInteger;
var
  i, len: integer;
begin
  len := Length(da);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Ceil(da[i] * scale);
end;
//------------------------------------------------------------------------------

function ExtractScaleFromMatrix(mat: TMatrixD): double;
begin
  Result := Sqrt(mat[0][1]*mat[0][1] + mat[1][1]*mat[1][1]);
end;

//------------------------------------------------------------------------------
// TDefsElement
//------------------------------------------------------------------------------

constructor TDefsElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.hidden := true;
end;

//------------------------------------------------------------------------------
// TUseElement
//------------------------------------------------------------------------------

function TUseElement.LoadAttributes: Boolean;
var
  attrib: PAttrib;
  h: Cardinal;
  i: integer;
begin
  Result := inherited;
  if not Result then Exit;

  //very occasionally, elements are referenced before they are declared :(
  if not Assigned(refEl) then Exit;

  surrogate := TElementClass(refEl.ClassType).Create(fParent, refEl.fNameHash);
  surrogate.fName := refEl.fName;
  surrogate.fDrawInfo := refEl.fParent.fDrawInfo;
  surrogate.fDrawInfo.matrix := IdentityMatrix;
  surrogate.fDrawInfo.hidden := fDrawInfo.hidden;

  for i := 0 to High(refEl.fAttribs) do
    with refEl.fAttribs[i] do
    begin
      h := GetHashedName(aName, aNameLen);
      case h of
        hId, hHref, hXlink_058_Href: continue;
      else
        begin
          attrib := surrogate.AddAttribute;
          attrib.aName := aName;
          attrib.aNameLen := aNameLen;
          attrib.aValue := aValue;
          attrib.aValueLen := aValueLen;
          ProcessAttrib(attrib);
        end;
      end;
    end;

  MatrixTranslate(surrogate.fDrawInfo.matrix, pt.X, pt.Y);
  surrogate.fDrawInfo.matrix :=
    MatrixMultiply(fDrawInfo.matrix, surrogate.fDrawInfo.matrix);

  for i := 0 to High(fAttribs) do
    with fAttribs[i] do
    begin
      h := GetHashedName(aName, aNameLen);
      case h of
        hHref, hX, hY, hXlink_058_Href, hTransform: continue;
      else
        begin
          attrib := surrogate.AddAttribute;
          attrib.aName := aName;
          attrib.aNameLen := aNameLen;
          attrib.aValue := aValue;
          attrib.aValueLen := aValueLen;
          ProcessAttrib(attrib);
        end;
      end;
    end;
end;

//------------------------------------------------------------------------------
// TGradElement
//------------------------------------------------------------------------------

procedure TGradientElement.LoadContent;
var
  i: integer;
begin
  inherited;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TGradStopElement then
      with TGradStopElement(fChilds[i]) do
        AddStop(color, offset);
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AddStop(color: TColor32; offset: double);
var
  len: integer;
begin
  len := Length(stops);
  setLength(stops, len+1);
  stops[len].offset := Min(1,Max(0, offset));
  stops[len].color := color;
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AssignTo(var other: TElement);
var
  i, len: integer;
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;

  with TGradientElement(other) do
  begin
    if gradientUnits = 0 then
      gradientUnits := Self.gradientUnits;

    if Length(stops) = 0 then
    begin
      len := Length(self.stops);
      SetLength(stops, len);
      for i := 0 to len -1 do
        stops[i] := Self.stops[i];
    end;

    if IsIdentityMatrix(fDrawInfo.matrix) then
      fDrawInfo.matrix := self.fDrawInfo.matrix;
  end;
end;
//------------------------------------------------------------------------------

function TGradientElement.PrepareGradientRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRect): Boolean;
var
  i, hiStops: integer;
begin
  hiStops := High(stops);
  Result := hiStops > 0;
  if not Result then Exit;
  //todo - make sure the stops are sorted!
  for i := 0 to hiStops do
    with stops[i] do renderer.InsertColorStop(offset, color);
end;

//------------------------------------------------------------------------------
// TRadGradElement
//------------------------------------------------------------------------------

constructor TRadGradElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  F := InvalidPointD;
  C := InvalidPointD;
end;
//------------------------------------------------------------------------------

function TRadGradElement.PrepareGradientRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRect): Boolean;
var
  hiStops: integer;
  cp, fp: TPoint;
  pt: TPointD;
begin
  hiStops := High(stops);
  Result := hiStops > 0;
  if not Result then Exit;

  //nb: the 'r' attribute isn't currently supported but
  //it can be simulated by judicious resizing of 'rec'.

  //gradientUnits="userSpaceOnUse"
  //gradientUnits="objectBoundingBox"

  MatrixApply(fDrawInfo.matrix, rec);

  if not IsValid(C) then
    cp := MidPoint(rec)
  else if (C.X <=1) and (C.Y <=1) then
  begin
    cp.X := rec.Left + Round(RectWidth(rec) * C.X);
    cp.Y := rec.Top + Round(RectHeight(rec) * C.Y);
  end else
  begin
    pt := C;
    MatrixApply(matrix, pt);
    cp := Point(pt);
  end;

  with MidPoint(rec) do
    Image32_Vector.OffsetRect(rec, cp.X -X, cp.Y -y);

  if not IsValid(F) then
    fp := MidPoint(rec)
  else if (F.X <=1) and (F.Y <=1) then
  begin
    fp.X := rec.Left + Round(RectWidth(rec) * F.X);
    fp.Y := rec.Top + Round(RectHeight(rec) * F.Y);
  end else
  begin
    pt := F;
    MatrixApply(matrix, pt);
    fp := Point(pt);
  end;

  with renderer as TSvgRadialGradientRenderer do
    SetParameters(rec, fp, stops[0].color, stops[High(stops)].color);
  inherited; //nb: must do this after SetParameters
end;

//------------------------------------------------------------------------------
// TLinGradElement
//------------------------------------------------------------------------------

constructor TLinGradElement.Create(parent: TElement; hashName: Cardinal);
begin
  startPt := InvalidPointD;
  endPt := InvalidPointD;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLinGradElement.AssignTo(var other: TElement);
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;
  if other is TLinGradElement then
    with TLinGradElement(other) do
    begin
      if not IsValid(startPt) then startPt := self.startPt;
      if not IsValid(endPt) then endPt := self.endPt;
    end;
end;
//------------------------------------------------------------------------------

function TLinGradElement.PrepareGradientRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRect): Boolean;
var
  pt1, pt2: TPointD;
  hiStops: integer;
begin
  hiStops := High(stops);
  Result := (hiStops > 0);
  if not Result then Exit;

  with renderer as TLinearGradientRenderer do
  begin
    if IsValid(startPt.X) then
      pt1.X := startPt.X else
      pt1.X := 0;
    if IsValid(startPt.Y) then
      pt1.Y := startPt.Y else
      pt1.Y := 0;
    if IsValid(endPt.X) then
      pt2.X := endPt.X else
      pt2.X := 1;
    if IsValid(endPt.Y) then
      pt2.Y := endPt.Y else
      pt2.Y := 0;
    if gradientUnits <> hUserSpaceOnUse then
    begin
      matrix := fDrawInfo.matrix;
      MatrixScale(matrix, Min(RectWidth(rec), RectHeight(rec)));
      MatrixTranslate(matrix, rec.Left, rec.Top);
    end else
      matrix := MatrixMultiply(matrix, fDrawInfo.matrix);

    MatrixApply(matrix, pt1);
    MatrixApply(matrix, pt2);

    SetParameters(pt1, pt2, stops[0].color, stops[High(stops)].color);
  end;
  inherited; //nb: must do this after SetParameters
end;

//------------------------------------------------------------------------------
// TGradStopElement
//------------------------------------------------------------------------------

constructor TGradStopElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  color := clBlack32;
end;

//------------------------------------------------------------------------------
// TFilterElement
//------------------------------------------------------------------------------

constructor TFilterElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  recWH.Left := InvalidD;
  recWH.Top := InvalidD;
  recWH.Width := InvalidD;
  recWH.Height := InvalidD;
  fDrawInfo.hidden := true;
end;

//------------------------------------------------------------------------------
// TGaussianElement
//------------------------------------------------------------------------------

constructor TGaussianElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  stdDev := InvalidD;
end;

//------------------------------------------------------------------------------
// TClipPathElement
//------------------------------------------------------------------------------

constructor TClipPathElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.hidden := true;
end;
//------------------------------------------------------------------------------

procedure TClipPathElement.GetDrawPaths(ArcScale: double);
var
  i: integer;
begin
  inherited;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        GetDrawPaths(ArcScale);
        AppendPath(self.drawPathsF, drawPathsF);
        AppendPath(self.drawPathsO, drawPathsO);
        AppendPath(self.drawPathsC, drawPathsC);
      end;
end;

//------------------------------------------------------------------------------
// TShapeElement
//------------------------------------------------------------------------------

constructor TShapeElement.Create(parent: TElement; hashName: Cardinal);
var
  i: integer;
  className: AnsiString;
  classRec: PAnsiRec;
begin
  inherited;
  //load any class styles
  className := LowercaseAnsi(fName, fNameLen);
  i := fReader.fClassStylesList.IndexOf(string(classname));
  if i < 0 then Exit;
  classRec := PAnsiRec(fReader.fClassStylesList.objects[i]);
  with classRec^ do
    ParseStyle(PAnsiChar(ansi), Length(ansi));
end;
//------------------------------------------------------------------------------

function TShapeElement.GetVal(out val: double): Boolean;
begin
  Result := GetNum(fCurrent, fCurrentEnd, true, val);
end;
//------------------------------------------------------------------------------

function TShapeElement.IsFilled: Boolean;
begin
  Result := fDrawInfo.hidden or //can't tell yet so default to true
    Assigned(fDrawInfo.fillGradEl) or (TARGB(fDrawInfo.fillColor).A > 0);
end;
//------------------------------------------------------------------------------

function TShapeElement.IsStroked: Boolean;
begin
  Result := //fDrawInfo.hidden or //can't tell yet so default to true
    ((fDrawInfo.strokeWidth > 0) and
    (Assigned(fDrawInfo.strokeGradEl) or (TARGB(fDrawInfo.strokeColor).A > 0)));
end;
//------------------------------------------------------------------------------

procedure TShapeElement.Draw(img, tmpImg: TImage32; RootMatrix: TMatrixD);
var
  i: integer;
  stroked, filled: Boolean;
  ArcScale: double;
  mat: TMatrixD;
  clipRec: TRect;
  usingSpecialEffects: Boolean;
const
  blurQual = 0; //0 .. 2; 0=OK (faster), 1=very good, 2=high qual. (slower)
begin
  filled := IsFilled; stroked := IsStroked;
  if not (filled or stroked) then Exit;

  drawPathsF := nil; drawPathsO := nil; drawPathsC := nil;
  ArcScale := ExtractScaleFromMatrix(RootMatrix);
  GetDrawPaths(ArcScale);

  mat := MatrixMultiply(RootMatrix, fDrawInfo.matrix);
  MatrixApply(mat, drawPathsF);
  MatrixApply(mat, drawPathsO);
  MatrixApply(mat, drawPathsC);

  usingSpecialEffects := Assigned(fDrawInfo.clipPathEl) or
    (fDrawInfo.opacity < 255) or Assigned(fDrawInfo.filterEl);

  if usingSpecialEffects then
  begin
    clipRec := Image32_Vector.UnionRect(GetBounds(drawPathsF),
      Image32_Vector.UnionRect(GetBounds(drawPathsC), GetBounds(drawPathsO)));

    if Assigned(fDrawInfo.filterEl) then
      with fDrawInfo.filterEl do
      begin
        if IsValid(recWH.Left) and IsValid(recWH.Top) and
          IsValid(recWH.Width) and IsValid(recWH.Height) then
        begin
          Image32_Vector.OffsetRect(clipRec,
            Round(clipRec.Width * recWH.Left), Round(clipRec.Height * recWH.Top));
          clipRec.Right := clipRec.Left + Round(RectWidth(clipRec) * recWH.Width);
          clipRec.Bottom := clipRec.Top + Round(RectHeight(clipRec) * recWH.Height);
        end
        else clipRec := Image32_Vector.InflateRect(clipRec,
          Round(RectWidth(clipRec) * 0.2), Round(RectHeight(clipRec) * 0.2));
      end;

    tmpImg.FillRect(clipRec, clNone32);

    if Assigned(fDrawInfo.clipPathEl) then
      with fDrawInfo.clipPathEl do
      begin
        GetDrawPaths(ArcScale);
        MatrixApply(mat, drawPathsF);
      end;
    ArcScale := ExtractScaleFromMatrix(mat);

    DrawFilled(tmpImg, mat);
    DrawStroke(tmpImg, mat, true);
    DrawStroke(tmpImg, mat, false);

    if Assigned(fDrawInfo.filterEl) then
      with fDrawInfo.filterEl do
        for i := 0 to fChilds.Count -1 do
          if (TElement(fChilds[i]) is TGaussianElement) then
            with TGaussianElement(fChilds[i]) do
              if IsValid(stdDev) then
                FastGaussianBlur(tmpImg,
                  clipRec, Ceil(stdDev  * ArcScale), blurQual);

    if fDrawInfo.opacity < 255 then
      tmpImg.ReduceOpacity(fDrawInfo.opacity, clipRec);


    if Assigned(fDrawInfo.clipPathEl) then
      with fDrawInfo.clipPathEl do
        EraseInverted(tmpImg, drawPathsF, fDrawInfo.fillRule, clipRec);

    img.CopyBlend(tmpImg, clipRec, clipRec, BlendToAlpha);
  end else
  begin
    if filled then DrawFilled(img, mat);
    if not stroked then Exit;
    DrawStroke(img, mat, true);
    DrawStroke(img, mat, false);
  end;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawFilled(img: TImage32; matrix: TMatrixD);
var
  rec: TRect;
begin
  if not Assigned(drawPathsF) then Exit;

  if Assigned(fDrawInfo.fillGradEl) then
  begin
    rec := GetBounds(drawPathsF);
    if fDrawInfo.fillGradEl is TRadGradElement then
    begin
      with fDrawInfo.fillGradEl do
        PrepareGradientRenderer(fReader.RadGradRenderer, matrix, rec);
      DrawPolygon(img, drawPathsF, fDrawInfo.fillRule, fReader.RadGradRenderer);
    end
    else if fDrawInfo.fillGradEl is TLinGradElement then
    begin
      with fDrawInfo.fillGradEl do
        PrepareGradientRenderer(fReader.LinGradRenderer, matrix, rec);
      DrawPolygon(img, drawPathsF, fDrawInfo.fillRule, fReader.LinGradRenderer);
    end;
  end else
    DrawPolygon(img, drawPathsF, fDrawInfo.fillRule, fDrawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawStroke(img: TImage32;
  matrix: TMatrixD; isClosed: Boolean);
var
  i,k, dashOffset: integer;
  dashArray: TArrayOfInteger;
  scale: Double;
  rec: TRect;
  strokePaths: TPathsD;
const
  endStyle: array [Boolean] of TEndStyle = (esRound, esPolygon);
begin
  if isClosed then
    strokePaths := drawPathsC else
    strokePaths := drawPathsO;
  if not Assigned(strokePaths) then Exit;

  scale := ExtractScaleFromMatrix(matrix);

  if Length(fDrawInfo.dashArray) > 1 then
  begin
    dashOffset := Round(fDrawInfo.dashOffset * scale);
    dashArray := MakeDashArray(fDrawInfo.dashArray, scale);
    DrawDashedLine(img, strokePaths, dashArray, @dashOffset,
      fDrawInfo.strokeWidth * scale, fDrawInfo.strokeColor, esButt)
  end
  else if Assigned(fDrawInfo.strokeGradEl) then
  begin
    rec := GetBounds(strokePaths);
    if fDrawInfo.strokeGradEl is TRadGradElement then
    begin
      with fDrawInfo.strokeGradEl do
        PrepareGradientRenderer(fReader.RadGradRenderer, matrix, rec);
      DrawLine(img, strokePaths, fDrawInfo.strokeWidth * scale,
        fReader.RadGradRenderer, endStyle[isClosed]);
    end else
    begin
      with fDrawInfo.strokeGradEl do
        PrepareGradientRenderer(fReader.LinGradRenderer, matrix, rec);
      DrawLine(img, strokePaths, fDrawInfo.strokeWidth * scale,
        fReader.LinGradRenderer, endStyle[isClosed]);
    end;
  end else
  begin
    DrawLine(img, strokePaths, fDrawInfo.strokeWidth * scale,
      fDrawInfo.strokeColor, endStyle[isClosed]);

    if Assigned(fDrawInfo.markerStart) then
    begin
      fDrawInfo.markerStart.callerEl := Self;
      for i := 0 to High(strokePaths) do
      begin
        if Length(strokePaths[i]) < 2 then Continue;
        fDrawInfo.markerStart.SetSoloPoint(
          strokePaths[i][0],
          GetAngle(strokePaths[i][1], strokePaths[i][0]));
        fDrawInfo.markerStart.Draw(img, nil, matrix);
      end;
    end;

    if Assigned(fDrawInfo.markerMiddle) then
    begin
      fDrawInfo.markerMiddle.callerEl := Self;
      for i := 0 to High(strokePaths) do
      begin
        k := High(strokePaths[i]);
        if k < 1 then Continue;

        fDrawInfo.markerMiddle.SetPoints(
          Copy(strokePaths[i], 1, k -1));
        fDrawInfo.markerMiddle.Draw(img, nil, matrix);
      end;
    end;

    if Assigned(fDrawInfo.markerEnd) then
    begin
      fDrawInfo.markerEnd.callerEl := Self;
      for i := 0 to High(strokePaths) do
      begin
        k := High(strokePaths[i]);
        if k < 1 then Continue;

        fDrawInfo.markerEnd.SetSoloPoint(
          strokePaths[i][k],
          GetAngle(strokePaths[i][k-1], strokePaths[i][k]));
        fDrawInfo.markerEnd.Draw(img, nil, matrix);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TPathElement
//------------------------------------------------------------------------------

function TPathElement.GetSegType(out isRelative: Boolean): Boolean;
var
  c: AnsiChar;
begin
  Result := false;
  if not SkipBlanks(fCurrent, fCurrentEnd) then Exit;
  c := upcase(fCurrent^);
  result := CharInSet(c, ['A','C','H','M','L','Q','S','T','V','Z']);
  if not result then Exit;
  case c of
    'M': currSegType := dsMove;
    'L': currSegType := dsLine;
    'H': currSegType := dsHorz;
    'V': currSegType := dsVert;
    'A': currSegType := dsArc;
    'Q': currSegType := dsQBez;
    'C': currSegType := dsCBez;
    'T': currSegType := dsQSpline;
    'S': currSegType := dsCSpline;
    'Z': currSegType := dsClose;
  end;
  isRelative := fCurrent^ >= 'a';
  inc(fCurrent);
end;
//------------------------------------------------------------------------------

function TPathElement.GetSingleDigit(out digit: integer): Boolean;
begin
  Result := SkipBlanksIgnoreComma(fCurrent, fCurrentEnd) and
    (fCurrent^ >= '0') and (fCurrent^ <= '9');
  if not Result then Exit;
  digit := Ord(fCurrent^) - Ord('0');
  inc(fCurrent);
end;
//------------------------------------------------------------------------------

procedure TPathElement.StartNewDpath;
var
  cnt: integer;
begin
  if Assigned(currDpath) then
  begin
    if not Assigned(currDpath.segs) then Exit;
    SetLength(CurrSeg.vals, currSegCnt); //trim the last segment;
  end;

  cnt := Length(dpaths);
  SetLength(dpaths, cnt +1);

  currDpath := @dpaths[cnt];
  currDpath.firstPt := lastPt;
  currDpath.isClosed := False;
  currDpath.segs := nil;
  currSeg := nil;
end;
//------------------------------------------------------------------------------

procedure TPathElement.StartNewSeg(segType: TDsegType);
var
  cnt: integer;
begin
  if Assigned(currSeg) then 
    SetLength(CurrSeg.vals, currSegCnt)     
  else if not Assigned(currDpath) then
    StartNewDpath;

  currSegType := segType;
  cnt := Length(currDpath.segs);
  SetLength(currDpath.segs, cnt +1);
  currSeg := @currDpath.segs[cnt];
  currSeg.segType := segType;

  currSegCap := buffSize;
  SetLength(currSeg.vals, currSegCap);
  currSegCnt := 0;
end;
//------------------------------------------------------------------------------

procedure TPathElement.AddSegValue(val: double);
begin
  if not Assigned(currSeg) then 
    StartNewSeg(currSegType);
  
  if currSegCnt = currSegCap then
  begin
    inc(currSegCap, buffSize);
    SetLength(currSeg.vals, currSegCap);
  end;
  currSeg.vals[currSegCnt] := val;
  inc(currSegCnt);
end;
//------------------------------------------------------------------------------

procedure TPathElement.AddSegPoint(const pt: TPointD);
begin
  AddSegValue(pt.X); AddSegValue(pt.Y);
end;
//------------------------------------------------------------------------------

function TPathElement.Get2Num(var pt: TPointD; isRelative: Boolean): Boolean;
begin
  Result := GetVal(pt.X) and GetVal(pt.Y);
  if not Result or not isRelative then Exit;
  pt.X := pt.X + lastPt.X;
  pt.Y := pt.Y + lastPt.Y;
end;
//------------------------------------------------------------------------------

procedure TPathElement.ParseD(attrib: PAttrib);
var
  i: integer;
  d: double;
  currPt: TPointD;
  isRelative: Boolean;
begin
  fCurrent := attrib.aValue;
  fCurrentEnd := fCurrent + attrib.aValueLen;

  isRelative := false;
  currPt := NullPointD;

  while GetSegType(isRelative) do
  begin
    lastPt := currPt;

    if (currSegType = dsMove) then
    begin
      if Assigned(currSeg) then
      begin
        SetLength(currSeg.vals, currSegCnt); //trim buffer
        currDpath.isClosed := false;
      end;
      currDpath := nil;
      currSeg := nil;

      if not Get2Num(currPt, isRelative) then break;
      self.lastPt :=  currPt;

      //values immediately following a Move are implicitly Line statements
      if NumPending(fCurrent, fCurrentEnd, true) then
        currSegType := dsLine else
        Continue;
    end
    else if (currSegType = dsClose) then
    begin
      if not Assigned(currSeg) then break;
      SetLength(currSeg.vals, currSegCnt); //trim buffer
      currDpath.isClosed := true;
      currDpath := nil;
      currSeg := nil;
      Continue;
    end;

    if Assigned(currSeg) then
      SetLength(currSeg.vals, currSegCnt); //trim buffer
    currSeg := nil;

    case currSegType of
      dsHorz:
        while NumPending(fCurrent, fCurrentEnd, true) do
        begin
          GetVal(currPt.X);
          if isRelative then
            currPt.X := currPt.X + lastPt.X;
          AddSegValue(currPt.X);
        end;

      dsVert:
        while NumPending(fCurrent, fCurrentEnd, true) do
        begin
          GetVal(currPt.Y);
          if isRelative then
            currPt.Y := currPt.Y + lastPt.Y;
          AddSegValue(currPt.Y);
        end;

      dsLine:
        while true do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
          SkipBlanks(fCurrent, fCurrentEnd);
          if NumPending(fCurrent, fCurrentEnd, true) then Continue;
          if LowerCaseTable[fCurrent^] = 'l' then GetSegType(isRelative)
          else break;
        end;

      dsQSpline:
        while NumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCSpline:
        while NumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsQBez:
        while NumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCBez:
        while NumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsArc:
        while NumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not GetVal(currPt.X) or
            not GetVal(currPt.Y) then break;
          AddSegPoint(currPt);                              //radii
          if GetVal(d) then AddSegValue(d);                 //angle
          if not GetSingleDigit(i) then break;              //arc-flag
          AddSegValue(i);
          if not GetSingleDigit(i) then break;              //sweep-flag
          AddSegValue(i);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;
    end;
  end;
  if Assigned(currSeg) then
    SetLength(currSeg.vals, currSegCnt); //trim buffer
  fCurrent    := fCurrentEnd +1;
  fCurrentEnd := fReader.fEndStream;
end;
//------------------------------------------------------------------------------

procedure TPathElement.Flatten(index: integer;
  out path: TPathD; out isClosed: Boolean);
var
  i,j,k, pathLen, pathCap: integer;
  currPt, radii, pt1, pt2, pt3, pt4: TPointD;
  lastQCtrlPt, lastCCtrlPt: TPointD;
  arcFlag, sweepFlag: integer;
  angle, arc1, arc2: double;
  rec: TRectD;
  path2: TPathD;

  procedure AddPoint(const pt: TPointD);
  begin
    if pathLen = pathCap then
    begin
      pathCap := pathCap + buffSize;
      SetLength(path, pathCap);
    end;
    path[pathLen] := pt;
    currPt := pt;
    inc(pathLen);
  end;

  procedure AddPath(const p: TPathD);
  var
    i, pLen: integer;
  begin
    pLen := Length(p);
    if pLen = 0 then Exit;
    currPt := p[pLen -1];
    if pathLen + pLen >= pathCap then
    begin
      pathCap := pathLen + pLen + buffSize;
      SetLength(path, pathCap);
    end;
    for i := 0 to pLen -1 do
    begin
      path[pathLen] := p[i];
      inc(pathLen);
    end;
  end;

begin
  pathLen := 0; pathCap := 0;
  lastQCtrlPt := InvalidPointD;
  lastCCtrlPt := InvalidPointD;
  isClosed := dpaths[index].isClosed;
  with dpaths[index] do
  begin
    AddPoint(firstPt);
    for i := 0 to High(segs) do
      with segs[i] do
      begin
        case segType of
          dsLine:
            if High(vals) > 0 then
              for j := 0 to High(vals) div 2 do
                AddPoint(PointD(vals[j*2], vals[j*2 +1]));
          dsHorz:
            for j := 0 to High(vals) do
              AddPoint(PointD(vals[j], currPt.Y));
          dsVert:
            for j := 0 to High(vals) do
              AddPoint(PointD(currPt.X, vals[j]));
          dsArc:
            if High(vals) > 5 then
              for j := 0 to High(vals) div 7 do
              begin
                radii.X   := vals[j*7];
                radii.Y   := vals[j*7 +1];
                angle     := DegToRad(vals[j*7 +2]);
                arcFlag   := Round(vals[j*7 +3]);
                sweepFlag := Round(vals[j*7 +4]);
                pt2.X := vals[j*7 +5];
                pt2.Y := vals[j*7 +6];

                SvgArc(currPt, pt2, radii, angle,
                  arcFlag <> 0, sweepFlag <> 0, arc1, arc2, rec);
                if (sweepFlag = 0)  then
                begin
                  path2 := Arc(rec, arc2, arc1);
                  path2 := ReversePath(path2);
                end else
                  path2 := Arc(rec, arc1, arc2);
                path2 := RotatePath(path2, rec.MidPoint, angle);
                AddPath(path2);
              end;
          dsQBez:
            if High(vals) > 2 then
              for j := 0 to High(vals) div 4 do
              begin
                pt2.X := vals[j*4];
                pt2.Y := vals[j*4 +1];
                pt3.X := vals[j*4 +2];
                pt3.Y := vals[j*4 +3];
                lastQCtrlPt := pt2;
                path2 := FlattenQBezier(currPt, pt2, pt3);
                AddPath(path2);
              end;
          dsQSpline:
            if High(vals) > 0 then
              for j := 0 to High(vals) div 2 do
              begin
                if IsValid(lastQCtrlPt) then
                  pt2 := ReflectPoint(lastQCtrlPt, currPt) else
                  pt2 := currPt;
                pt3.X := vals[j*2];
                pt3.Y := vals[j*2 +1];
                lastQCtrlPt := pt2;
                path2 := FlattenQBezier(currPt, pt2, pt3);
                AddPath(path2);
              end;
          dsCBez:
            if High(vals) > 4 then
              for j := 0 to High(vals) div 6 do
              begin
                pt2.X := vals[j*6];
                pt2.Y := vals[j*6 +1];
                pt3.X := vals[j*6 +2];
                pt3.Y := vals[j*6 +3];
                pt4.X := vals[j*6 +4];
                pt4.Y := vals[j*6 +5];
                lastCCtrlPt := pt3;
                path2 := FlattenCBezier(currPt, pt2, pt3, pt4);
                AddPath(path2);
              end;
          dsCSpline:
            if High(vals) > 2 then
              for j := 0 to High(vals) div 4 do
              begin
                if IsValid(lastCCtrlPt) then
                  pt2 := ReflectPoint(lastCCtrlPt, currPt) else
                  pt2 := currPt;
                pt3.X := vals[j*4];
                pt3.Y := vals[j*4 +1];
                pt4.X := vals[j*4 +2];
                pt4.Y := vals[j*4 +3];
                lastCCtrlPt := pt3;
                path2 := FlattenCBezier(currPt, pt2, pt3, pt4);
                AddPath(path2);
              end;
        end;
      end;
  end;
  SetLength(path, pathLen);
end;
//------------------------------------------------------------------------------

procedure TPathElement.GetDrawPaths(ArcScale: double);
var
  i: integer;
  filled, stroked, isClosed: Boolean;
  path: TPathD;
begin
  filled := isFilled;
  stroked := isStroked;

  for i := 0 to High(dpaths) do
  begin
    Flatten(i, path, isClosed);
    if not Assigned(path) then Continue;
    if filled then AppendPath(drawPathsF, path);
    if not stroked then Continue;
    if isClosed then AppendPath(drawPathsC, path)
    else AppendPath(drawPathsO, path);
  end;
end;

//------------------------------------------------------------------------------
// TPolyElement
//------------------------------------------------------------------------------

procedure TPolyElement.GetDrawPaths(ArcScale: double);
begin
  if not Assigned(path) then Exit;
  if IsFilled then AppendPath(drawPathsF, path);
  if not IsStroked then Exit;
  if (fNameHash = hPolygon) then
    AppendPath(drawPathsC, path) else
    AppendPath(drawPathsO, path);
end;
//------------------------------------------------------------------------------

procedure TPolyElement.ParsePoints(attrib: PAttrib);
var
  currCnt, currCap: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if currCnt = currCap then
    begin
      currCap := currCap + buffSize;
      SetLength(path, currCap);
    end;
    path[currCnt] := pt;
    inc(currCnt);
  end;

var
  pt: TPointD;
begin
  currCnt     := 0;
  currCap     := buffSize;
  fCurrent := attrib.aValue;
  SetLength(path, currCap);
  while NumPending(fCurrent, fCurrentEnd, true) and
    GetVal(pt.X) and GetVal(pt.Y) do
      AddPoint(pt);
  SetLength(path, currCnt);
end;

//------------------------------------------------------------------------------
// TLineElement
//------------------------------------------------------------------------------

constructor TLineElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  SetLength(path, 2);
  path[0] := NullPointD; path[1] := NullPointD;
end;
//------------------------------------------------------------------------------

procedure TLineElement.GetDrawPaths(ArcScale: double);
begin
  if IsFilled then AppendPath(drawPathsF, path);
  if IsStroked then AppendPath(drawPathsO, path);
end;

//------------------------------------------------------------------------------
// TCircleElement
//------------------------------------------------------------------------------

procedure TCircleElement.GetDrawPaths(ArcScale: double);
var
  rec: TRectD;
  path: TPathD;
begin
  if (radius <= 0) then Exit;
  with CenterPt do
    rec := RectD(X -radius, Y -radius, X +radius, Y +radius);
  path := Ellipse(rec, ArcScale);

  if IsFilled then AppendPath(drawPathsF, path);
  if IsStroked then AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TEllipseElement
//------------------------------------------------------------------------------

procedure TEllipseElement.GetDrawPaths(ArcScale: double);
var
  rec: TRectD;
  path: TPathD;
begin
  if (radius.X <= 0) or (radius.Y <= 0) then Exit;
  with centerPt do
    rec := RectD(X -radius.X, Y -radius.Y, X +radius.X, Y +radius.Y);
  path := Ellipse(rec, ArcScale);

  if IsFilled then AppendPath(drawPathsF, path);
  if IsStroked then AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TRectElement
//------------------------------------------------------------------------------

procedure TRectElement.GetDrawPaths(ArcScale: double);
var
  rec2: TRectD;
  path: TPathD;
begin
  if (recWH.width <= 0) or (recWH.height <= 0) then Exit;
  rec2 := recWH.RectD;
  if (radius.X > 0) or (radius.Y > 0) then
  begin
    if (radius.X <= 0) then radius.X := radius.Y
    else if (radius.Y <= 0) then radius.Y := radius.X;
    //nb: elliptical rect rounding is not supported.
    path := RoundRect(rec2, Average(radius.X, radius.Y))
  end else
    path := Rectangle(rec2);

  if IsFilled then AppendPath(drawPathsF, path);
  if IsStroked then AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TTextElement
//------------------------------------------------------------------------------

constructor TTextElement.Create(parent: TElement; hashName: Cardinal);
begin
  fontInfo := defaultFontInfo;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TTextElement.GetDrawPaths(ArcScale: double);
begin
end;
//------------------------------------------------------------------------------

procedure TTextElement.AddSubtext(text: PAnsiChar; len: integer);
var
  subtextEl: TSubtextElement;
begin
  subtextEl := TSubtextElement.Create(self, 0);
  subtextEl.fName := text;
  subtextEl.fNameLen := 0;
  subtextEl.text := text;
  subtextEl.textLen := len;
  fChilds.add(subtextEl);
  if not subtextEl.fDrawInfo.hidden then
    fReader.fShapesList.Add(subtextEl);
end;

//------------------------------------------------------------------------------
// TSubtextElement
//------------------------------------------------------------------------------

procedure TSubtextElement.GetDrawPaths(ArcScale: double);
var
  outPaths: ^TPathsD;
  pt: TPointD;
  parentTextEl: TTextElement;
  s: string;
  i: integer;
  fontInfo: TFontInfo;
  fontReader: TFontReader;
  fontCache: TGlyphCache;
  width: double;
begin
  if not (fParent is TTextElement) then Exit;
  parentTextEl := TTextElement(fParent);

  if IsFilled then
    outPaths := @drawPathsF else
    outPaths := @drawPathsC;

  //if first subtext then reset X offset
  if (self = parentTextEl.fChilds[0]) then parentTextEl.tmpX := InvalidD;

  //get the text offset
  if (self is TTSpanElement) and IsValid(TTSpanElement(self).pt) then
  begin
      pt := TTSpanElement(self).pt;
  end else
  begin
    pt := TTextElement(fParent).pt;
    if IsValid(parentTextEl.tmpX) then pt.X := parentTextEl.tmpX;
  end;

  //trim CRLFs and multiple spaces
  s := UTF8ToUnicodeString(Utf8String_(text, textLen));
  for i := 1 to Length(s) do
    if s[i] < #32 then s[i] := #32;
  i := Pos(#32#32, s);
  while i > 0 do
  begin
    Delete(s, i, 1);
    i := Pos(#32#32, s, i);
  end;

  if Self is TTSpanElement then
    fontInfo := TTSpanElement(self).fontInfo else
    fontInfo := parentTextEl.fontInfo;

  case fontInfo.family of
    ffSansSerif :
      begin
        case Byte(fontInfo.styles) of
          0: fontReader := fReader.fSansFontReader;
          1: fontReader := fReader.fSansBoldReader;
          2: fontReader := fReader.fSansItalicReader;
          else fontReader := fReader.fSansBoldItalReader;
        end;
        fontCache := fReader.fSansFontCache;
      end;
    ffSerif :
      begin
        case Byte(fontInfo.styles) of
          0: fontReader := fReader.fSerifFontReader;
          1: fontReader := fReader.fSerifBoldReader;
          2: fontReader := fReader.fSerifItalicReader;
          else fontReader := fReader.fSerifBoldItalReader;
        end;
        fontCache := fReader.fSerifFontCache;
      end
    else
    begin
      fontReader := fReader.fMonoFontReader;
      fontCache := fReader.fMonoFontCache;
    end;
  end;

  fontCache.FontReader := fontReader;

  if fontInfo.size > 2 then
    fontCache.FontHeight := fontInfo.size;
  width := fontCache.GetTextWidth(s);
  case fontInfo.align of
    taLeft:
      outPaths^ := fontCache.GetTextGlyphs(pt.X, pt.Y, s, parentTextEl.tmpX);
    taRight:
      begin
        pt.X := pt.X - width;
        outPaths^ := fontCache.GetTextGlyphs(pt.X, pt.Y, s, parentTextEl.tmpX);
        parentTextEl.tmpX := pt.X;
      end;
    taMiddle:
      begin
        pt.X := pt.X - width/2;
        outPaths^ := fontCache.GetTextGlyphs(pt.X, pt.Y, s, parentTextEl.tmpX);
      end;
  end;

  if IsFilled and IsStroked then
    drawPathsC := CopyPaths(drawPathsF);
end;

//------------------------------------------------------------------------------
// TTSpanElement
//------------------------------------------------------------------------------

constructor TTSpanElement.Create(parent: TElement; hashName: Cardinal);
begin
  if parent is TTextElement then
    fontInfo := TTextElement(parent).fontInfo else
    fontInfo := defaultFontInfo; //should never get here
  pt := InvalidPointD;
  delta := InvalidPointD;
  inherited;
end;

//------------------------------------------------------------------------------
// TMarkerElement
//------------------------------------------------------------------------------

constructor TMarkerElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  ref := InvalidPointD;
  width := InvalidD;
  height := InvalidD;
  viewbox := NullRectD;
  fDrawInfo.hidden := true;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.Draw(img, tmpImg: TImage32; RootMatrix: TMatrixD);
var
  i,j: integer;
  scale, sw: double;
  mat, mat2: TMatrixD;
  pt: TPointD;
begin
  if not Assigned(callerEl) then Exit;
  scale := ExtractScaleFromMatrix(RootMatrix);
  mat := fDrawInfo.matrix;
  sw := callerEl.fDrawInfo.strokeWidth;
  MatrixScale(mat, sw * scale);

  //for each 'point' draw the marker
  for i := 0 to High(points) do
  begin
    pt := points[i];

    //for each marker shape (though very rarely more than one)
    for j := 0 to fChilds.Count -1 do
      if TElement(fChilds[j]) is TShapeElement then
        with TShapeElement(fChilds[j]) do
        begin
          GetDrawPaths(scale);
          mat2 := IdentityMatrix;
          if IsValid(ref) then
            MatrixTranslate(mat2, -ref.X, -ref.Y);
          if not IsEmptyRect(viewbox) and
            IsValid(width) and IsValid(height) then
              MatrixScale(mat2, width/viewbox.Width, height/viewbox.Height);
          MatrixScale(mat2, sw * scale);
          MatrixRotate(mat2, NullPointD, angle);
          MatrixTranslate(mat2, pt.X, pt.Y);
          MatrixApply(mat2, drawPathsF);
          with fDrawInfo do DrawPolygon(img, drawPathsF, fillRule, fillColor);
        end;
  end;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.SetSoloPoint(const pt: TPointD; angle: double);
begin
  SetLength(points, 1);
  points[0] := pt;
  self.angle := angle;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.SetPoints(const points: TPathD);
begin
  angle := 0;
  Self.points := CopyPath(points);
end;

//------------------------------------------------------------------------------
// TStyleElement
//------------------------------------------------------------------------------

procedure TStyleElement.LoadContent;
var
  i, aNameLen, len, cap, styleLen: integer;
  hash: Cardinal;
  dotted: Boolean;
  pClassName, pstyle: PAnsiChar;
  names: array of string;

  procedure AddName(const name: string);
  begin
    if len = cap then
    begin
      cap := cap + buffSize;
      SetLength(names, cap);
    end;
    names[len] := name;
    inc(len);
  end;

begin
  len := 0; cap := 0;
  if (PeekNextChar = '<') and ((fCurrent+1)^ = '!') and
    ((fCurrent+2)^ = '[') and
    (GetHashedName(fCurrent + 3, 5) = hCDATA) then
  begin
    inc(fCurrent, 8);
    if fCurrent^ <> '[' then Exit;
    inc(fCurrent);
  end;

  while SkipStyleBlanks(fCurrent,fCurrentEnd) and
    CharInSet(LowerCaseTable[PeekNextChar], ['.', 'a'..'z']) do
  begin
    dotted := fCurrent^ = '.';
    //get one or more class names for each pending style
    if dotted then inc(fCurrent);
    GetName(pClassName, aNameLen);
    if dotted then
      AddName(String(LowercaseAnsi((pClassName -1), aNameLen +1))) else
      AddName(String(LowercaseAnsi(pClassName, aNameLen)));
    if PeekNextChar = ',' then
    begin
      inc(fCurrent);
      Continue;
    end;
    if len = 0 then break;
    SetLength(names, len); //ie no more comma separated names

    //now get the style
    if PeekNextChar <> '{' then Break;
    inc(fCurrent);
    pstyle := fCurrent;
    while (fCurrent < fCurrentEnd) and (fCurrent^ <> '}') do
      inc(fCurrent);
    if (fCurrent = fCurrentEnd) then break;
    styleLen := fCurrent - pstyle;

    //finally, for each class name add this style
    //todo - check for existing classes and append styles
    for i := 0 to High(names) do
      fReader.fClassStylesList.Add(names[i], LowercaseAnsi(pstyle, styleLen));
    names := nil;
    len := 0; cap := 0;
    inc(fCurrent);
  end;

  //skip trailing unknown content
  while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
  //we should now be at </STYLE>
  if (PeekNextChar <> '<') then Exit;
  if ((fCurrent +1)^ <> '/') then Exit;
  inc(fCurrent, 2);
  fIsValid := HashCurrentWord(hash) and (fNameHash = hash);
  if fIsValid then inc(fCurrent); //trailing '>'
end;

//------------------------------------------------------------------------------
// TRootElement
//------------------------------------------------------------------------------

constructor TRootElement.Create(reader: TSvgReader; hashName: Cardinal);
var
  svgStart: PAnsiChar;
begin
  fChilds    := TList.Create;
  fNameHash  := hashName;
  fReader    := reader;

  fCurrentEnd := fReader.fEndStream;
  fDrawInfo :=  defaultDrawInfo;

  if not fReader.GetSvgStart(svgStart) then Exit;
  fName := svgStart;
  inc(svgStart, 3);
  fCurrent := svgStart;
  fIsValid := LoadAttributes;
end;

//------------------------------------------------------------------------------
// TElement
//------------------------------------------------------------------------------

constructor TElement.Create(parent: TElement; hashName: Cardinal);
begin
  fChilds    := TList.Create;
  fNameHash  := hashName;
  fIsValid   := false;

  self.fParent := parent;
  fReader      := parent.fReader;
  fCurrentEnd  := fReader.fEndStream;
  fDrawInfo    := parent.fDrawInfo;
  fCurrent     := parent.fCurrent;
  fIsValid := SkipBlanks(fCurrent, fCurrentEnd) and
    GetName(fName, fNameLen);
end;
//------------------------------------------------------------------------------

destructor  TElement.Destroy;
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    TElement(fChilds[i]).Free;
  fChilds.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TElement.GetName(out name: PAnsiChar; out nameLen: integer): Boolean;
begin
  Result := IsAlpha(fCurrent^);
  if not Result then Exit;
  name := fCurrent;
  nameLen := GetNameLength(fCurrent, fCurrentEnd);
end;
//------------------------------------------------------------------------------

function TElement.HashCurrentWord(out hash: Cardinal): Boolean;
begin
  Result := IsAlpha(fCurrent^);
  if not Result then Exit;
  hash := GetHashedName(fCurrent, fCurrentEnd);
end;
//------------------------------------------------------------------------------

function TElement.GetAttribValue(out value: PAnsiChar;
  out valueLen: integer): Boolean;
begin
  Result := fCurrent^ = '"';
  if not Result then Exit;
  inc(fCurrent);
  value := fCurrent;
  while (fCurrent < fCurrentEnd) and (fCurrent^ <> '"')  do inc(fCurrent);
  valueLen := fCurrent - value;
end;
//------------------------------------------------------------------------------

function TElement.AddAttribute: PAttrib;
var
  len: integer;
begin
  len := Length(fAttribs);
  setLength(fAttribs, len +1);
  Result := @fAttribs[len];
  Result.aOwnerEl := self;
end;
//------------------------------------------------------------------------------

function TElement.LoadAttributes: Boolean;
var
  attrib: PAttrib;
  nextCurrent: PAnsiChar;
begin
  Result := false;
  fHasContent := true;
  while SkipBlanks(fCurrent, fCurrentEnd) do
  begin
    if (fCurrent^ = '/') or (fCurrent^ = '>') then
    begin
      fHasContent := (fCurrent^ <> '/');
      if fHasContent then
        inc(fCurrent) else
        inc(fCurrent, 2);
      Result := true;
      fIsValid := true;
      break;
    end;
    attrib := AddAttribute;
    GetName(attrib.aName, attrib.aNameLen);
    if GetNextChar(fCurrent, fCurrentEnd) <> '=' then Exit;
    SkipBlanks(fCurrent, fCurrentEnd);
    GetAttribValue(attrib.aValue, attrib.aValueLen);
    nextCurrent := attrib.aValue + attrib.aValueLen +1;
    TrimBlanks(attrib.aValue, attrib.aValueLen);
    //nb: ProcessAttrib() below sometimes adds additional attributes
    //which may cause the 'fAttribs' array to reallocate memory
    //(ie when its capacity is reached). Consequently, the 'attrib'
    //pointer isn't safe to use after calling ProcessAttrib().
    ProcessAttrib(attrib);
    //nextCurrent := attrib.aValue + attrib.aValueLen +1; //Don't do this!
    fCurrent := nextCurrent; //this is definitely safe
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.ProcessAttrib(const attrib: PAttrib);
var
  i: integer;
  hash: Cardinal;
begin
  hash := GetHashedName(attrib.aName, attrib.aNameLen);
  i := AttribFuncList.IndexOf(inttohex(hash, 8));
  if i >= 0 then
    TAttribFunc(AttribFuncList.Objects[i])(attrib);
end;
//------------------------------------------------------------------------------

function TElement.FindRefElement(refname: PAnsiChar; refNameLen: integer): TElement;
var
  i: integer;
begin
  if (refname^ = 'u') and
    ((refname +1)^ = 'r') and
    ((refname +2)^ = 'l') and
    ((refname +3)^ = '(') then
  begin
    inc(refname, 4);
    dec(refNameLen, 5); //includes trailing ')'
  end;

  if refname^ = '#' then
  begin
    inc(refname);
    dec(refNameLen);
  end;

  i := fReader.fIdList.IndexOf(string(LowercaseAnsi(refname, refNameLen)));
  if i >= 0 then
    Result := TElement(fReader.fIdList.Objects[i]) else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TElement.LoadChild: TElement;
var
  hashedName: Cardinal;
  name, savedCurrent: PAnsiChar;
  nameLen: integer;
  elClass: TElementClass;
  tmpEl: TElement;
begin
  Result := nil;
  savedCurrent := fCurrent;
  if not GetName(name, nameLen) then Exit;
  fCurrent := savedCurrent;
  hashedName := GetHashedName(name, nameLen);

  elClass := HashToElementClass(hashedName);
  Result := elClass.Create(self, hashedName);

  if not Result.fIsValid or not Result.LoadAttributes then
  begin
    FreeAndNil(Result);
    fIsValid := false;
    Exit;
  end;

  if (Result is TUseElement) then
    with TUseElement(Result) do
      if Assigned(surrogate) then
      begin
        tmpEl := surrogate;
        tmpEl.fHasContent := fHasContent;
        tmpEl.fCurrent := fCurrent;
        FreeAndNil(Result);
        Result := tmpEl;
      end;

  fChilds.Add(Result);
  if (Result is TShapeElement) and not Result.fDrawInfo.hidden then
    fReader.fShapesList.Add(Result);

  fCurrent := Result.fCurrent;
end;
//------------------------------------------------------------------------------

function TElement.PeekNextChar: AnsiChar;
begin
  if not SkipBlanks(fCurrent, fCurrentEnd) then
    Result := #0 else
    Result := fCurrent^;
end;
//------------------------------------------------------------------------------

procedure TElement.ParseClassAttrib(classAttrib: PAttrib);
var
  i: integer;
  aClassName: AnsiString;
  classRec: PAnsiRec;
begin

  with classAttrib^ do
      aClassName := '.' + LowercaseAnsi(aValue, aValueLen);

  with classAttrib.aOwnerEl.fReader do
  begin
    i := fClassStylesList.IndexOf(string(aClassName));
    if i < 0 then Exit;
    classRec := PAnsiRec(fClassStylesList.objects[i]);
  end;

  ParseStyle(PAnsiChar(classRec.ansi), Length(classRec.ansi));
end;
//------------------------------------------------------------------------------

procedure TElement.ParseStyle(classStyle: PAnsiChar; len: integer);
var
  aNameLen: integer;
  newAt: PAttrib;
  current, endCurrent, styleName: PAnsiChar;
begin
  current := classStyle;
  endCurrent := current + len;

  while SkipBlanks(current, endCurrent) do
  begin
    styleName := current;
    aNameLen := GetStyleNameLen(current, endCurrent);
    if aNameLen = 0 then Break;

    newAt := AddAttribute;
    newAt.aName := styleName;
    newAt.aNameLen := aNameLen;

    if (GetNextChar(current, endCurrent) <> ':') or  //syntax check
      not SkipBlanks(current,endCurrent) then Break;
    newAt.aValue := current;
    inc(current);
    while (current < endCurrent) and (current^ <> ';') do inc(current);
    newAt.aValueLen := current - newAt.aValue;
    inc(current);
    ProcessAttrib(newAt);
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.ParseTransform(transform: PAnsiChar; len: integer);
var
  i: integer;
  current, endC: PAnsiChar;
  word: AnsiString;
  values: array[0..5] of double;
  mat: TMatrixD;
  pMat: PMatrixD;
  mats: array of TMatrixD;

  function NewMatrix: PMatrixD;
  var
    len: integer;
  begin
    len := length(mats);
    setLength(mats, len +1);
    mats[len] := IdentityMatrix;
    Result := @mats[len];
  end;

begin
  current := transform;
  endC := current + len;

  //surprisingly, and I think this is a bug in SVG not a feature ...
  //transform operations must be performed in reverse order
  while GetNextAlphaWord(current, endC, word) do
  begin
    if (Length(word) < 5) then Exit;
    word[5] := LowerCaseTable[word[5]];
    if GetNextChar(current, endC) <> '(' then Exit; //syntax check
    //reset values variables
    for i := 0 to High(values) do values[i] := InvalidD;
    //and since every transform function requires at least one value
    if not GetNum(current, endC, false, values[0]) then Exit;
    //now get additional variables
    i := 1;
    while (i < 6) and NumPending(current, endC, true) do
    begin
      GetNum(current, endC, true, values[i]);
      inc(i);
    end;
    if GetNextChar(current, endC) <> ')' then Exit; //syntax check

    pMat := NewMatrix;
    case word[5] of
      'e' : //scalE
        if not IsValid(values[1]) then
          MatrixScale(pMat^, values[0]) else
            MatrixScale(pMat^, values[0], values[1]);
      'i' : //matrIx
        if IsValid(values[5]) then
        begin
          pMat[0,0] :=  values[0];
          pMat[0,1] :=  values[1];
          pMat[1,0] :=  values[2];
          pMat[1,1] :=  values[3];
          pMat[2,0] :=  values[4];
          pMat[2,1] :=  values[5];
        end;
      's' : //tranSlateX, tranSlateY & tranSlate
        if Length(word) =10  then
        begin
          if LowerCaseTable[word[10]] = 'x' then
            MatrixTranslate(pMat^, values[0], 0)
          else if LowerCaseTable[word[10]] = 'y' then
            MatrixTranslate(pMat^, 0, values[0]);
        end
        else if IsValid(values[1]) then
          MatrixTranslate(pMat^, values[0], values[1])
        else
          MatrixTranslate(pMat^, values[0], 0);
      't' : //rotaTe
        if IsValid(values[2]) then
          MatrixRotate(pMat^, PointD(values[1],values[2]), DegToRad(values[0]))
        else
          MatrixRotate(pMat^, NullPointD, DegToRad(values[0]));
       'x' : //skewX
         begin
            MatrixSkew(pMat^, DegToRad(values[0]), 0);
         end;
       'y' : //skewY
         begin
            MatrixSkew(pMat^, 0, DegToRad(values[0]));
         end;
    end;
  end;

  if not assigned(mats) then Exit;
  mat := mats[0];
  for i := 1 to High(mats) do
    mat := MatrixMultiply(mat, mats[i]);
  fDrawInfo.matrix := MatrixMultiply(fDrawInfo.matrix, mat);
end;
//------------------------------------------------------------------------------

procedure TElement.AssignTo(var other: TElement);
begin
end;
//------------------------------------------------------------------------------

procedure TElement.LoadContent;
var
  c: PAnsiChar;
  el: TElement;
  hash: Cardinal;
begin
  while SkipBlanks(fCurrent, fCurrentEnd) do
  begin
    if (fCurrent^ = '<') then
    begin
      inc(fCurrent);
      if fCurrent^ = '!' then
      begin
        //a comment section
        while (fCurrent < fCurrentEnd) do
        begin
          if (fCurrent^ = '>') and
            ((fCurrent -1)^ = '-') and  ((fCurrent -2)^ = '-') then
          begin
            inc(fCurrent);
            break; //inner comment loop :)
          end;
          inc(fCurrent);
        end;
      end
      else if fCurrent^ = '/' then
      begin
        //this should be the element close
        inc(fCurrent);
        fIsValid := HashCurrentWord(hash) and (fNameHash = hash);
        if fIsValid then inc(fCurrent);
        Exit; //if fIsValid then all good :)
      end else
      begin
        //load a child element
        el := LoadChild;
        if not Assigned(el) then Break;
        if el.fHasContent then el.LoadContent;
        fCurrent := el.fCurrent;
      end;
    end
    else if fCurrent^ = '>' then
    begin
      //oops! something's wrong
      inc(fCurrent);
      fIsValid := false;
      break;
    end
    else if (self is TTextElement) then
    begin
      while (fCurrent -1)^ = #32 do dec(fCurrent);
      c := fCurrent;
      while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
      TTextElement(self).AddSubtext(c, fCurrent - c);
    end
    else if (self is TTSpanElement) then
    begin
      while (fCurrent -1)^ = #32 do dec(fCurrent);
      c := fCurrent;
      while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
      TTSpanElement(self).text := c;
      TTSpanElement(self).textLen := fCurrent - c;
    end else
    begin
      //skip unknown content
      while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TSvgReader
//------------------------------------------------------------------------------

constructor TSvgReader.Create;
begin
  fShapesList     := TList.Create;
  fMemStream      := TMemoryStream.Create;

  fIdList         := TStringList.Create;
  //fIdList.Duplicates := dupError; // I'm unsure how best to manage this, but
  fIdList.Duplicates := dupIgnore;  // pro tem, duplicates will be ignored.
  fIdList.Sorted  := True;

  fClassStylesList    := TStrAnsiList.Create;
  fClassStylesList.Duplicates := dupIgnore;
  fClassStylesList.Sorted := True;

  fLinGradRenderer := TLinearGradientRenderer.Create;
  fRadGradRenderer := TSvgRadialGradientRenderer.Create;

  fSansFontReader   := TFontReader.CreateFromResource('NOTO-SANS', RT_RCDATA);
  fSerifFontReader  := TFontReader.CreateFromResource('NOTO-SERIF', RT_RCDATA);
  fMonoFontReader   := TFontReader.CreateFromResource('NOTO-MONO', RT_RCDATA);

  fSansBoldReader     := TFontReader.CreateFromResource('NOTO-SANS-BOLD', RT_RCDATA);
  fSansItalicReader   := TFontReader.CreateFromResource('NOTO-SANS-ITALIC', RT_RCDATA);
  fSansBoldItalReader := TFontReader.CreateFromResource('NOTO-SANS-BOLDITALIC', RT_RCDATA);

  fSerifBoldReader      := TFontReader.CreateFromResource('NOTO-SERIF-BOLD', RT_RCDATA);
  fSerifItalicReader    := TFontReader.CreateFromResource('NOTO-SERIF-ITALIC', RT_RCDATA);
  fSerifBoldItalReader  := TFontReader.CreateFromResource('NOTO-SERIF-BOLDITALIC', RT_RCDATA);

  fSansFontCache    := TGlyphCache.Create(fSansFontReader, defaultFontHeight);
  fSerifFontCache   := TGlyphCache.Create(fSerifFontReader, defaultFontHeight);
  fMonoFontCache    := TGlyphCache.Create(fMonoFontReader, defaultFontHeight);

end;
//------------------------------------------------------------------------------

destructor TSvgReader.Destroy;
begin
  Clear;
  fMemStream.Free;
  fShapesList.Free;
  fIdList.Free;
  fClassStylesList.Free;
  fLinGradRenderer.Free;
  fRadGradRenderer.Free;

  fSansFontReader.Free;
  fSerifFontReader.Free;
  fMonoFontReader.Free;

  fSansBoldReader.Free;
  fSansItalicReader.Free;
  fSansBoldItalReader.Free;

  fSerifBoldReader.Free;
  fSerifItalicReader.Free;
  fSerifBoldItalReader.Free;

  fSansFontCache.Free;
  fSerifFontCache.Free;
  fMonoFontCache.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.Clear;
begin
  FreeAndNil(fRootElement);
  fMemStream.clear;
  fShapesList.Clear;
  fIdList.Clear;
  fClassStylesList.Clear;
  fEndStream := nil;
  fSansFontCache.FontHeight := defaultFontHeight;
  fSerifFontCache.FontHeight := defaultFontHeight;
  fMonoFontCache.FontHeight := defaultFontHeight;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.DrawImage(img: TImage32; scaleToImage: Boolean);
var
  i: integer;
  rec: TRect;
  sx, sy: double;
  rootMat: TMatrixD;
  clipImg: TImage32;
const
  endStyle: array[boolean] of TEndStyle = (esPolygon, esRound);
begin
  if not Assigned(fRootElement) or
    fRootElement.viewbox.IsEmpty then Exit;

  rec := Rect(fRootElement.viewbox);

  if scaleToImage and not img.IsEmpty then
  begin
    sx := img.width / fRootElement.viewbox.Width;
    sy := img.height / fRootElement.viewbox.Height;
    if sy < sx then sx := sy;
  end else
  begin
    img.SetSize(RectWidth(rec), RectHeight(rec));
    sx := 1;
  end;

  rootMat := IdentityMatrix;
  with fRootElement.viewbox do
    MatrixTranslate(rootMat, -Left, -Top);
  MatrixScale(rootMat, sx, sx);

  clipImg := TImage32.Create(img.Width, img.Height);
  try
    for i := 0 to fShapesList.Count -1 do
      with TShapeElement(fShapesList[i]) do
        Draw(img, clipImg, rootMat);
  finally
    clipImg.Free;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromFile(const filename: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromStream(stream: TStream): Boolean;
begin
  Result := false;
  Clear;
  if not Assigned(stream) then Exit;

  fMemStream.LoadFromStream(stream);
  fEndStream := PAnsiChar(fMemStream.Memory) + fMemStream.Size;

  fRootElement := TRootElement.Create(self, hSvg);
  with fRootElement do
  begin
    if viewbox.IsEmpty then
    begin
      if (width <= 0) or (height <= 0) then Exit;
      viewbox := RectD(0, 0, width, height);
//    end
//    else if (width > 0) and (height > 0) then
//    begin
//      MatrixScale(fDrawInfo.matrix,
//        width / viewbox.Width,
//        height / viewbox.Height);
    end;
    if fHasContent then LoadContent;
    Result := fIsValid;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.GetSvgStart(out svgStart: PAnsiChar): Boolean;
var
  svgEnd: PAnsiChar;
begin
  Result := false;
  svgStart := fMemStream.Memory;
  svgEnd := svgStart + fMemStream.Size -3;
  while (svgStart < svgEnd) do
  begin
    if (svgStart^ = '<') and
      (LowerCaseTable[(svgStart+1)^] = 's') and
      (LowerCaseTable[(svgStart+2)^] = 'v') and
      (LowerCaseTable[(svgStart+3)^] = 'g') then
    begin
      Result := true;
      inc(svgStart);
      Exit;
    end;
    inc(svgStart);
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure MakeLowerCaseTable;
var
  i: AnsiChar;
begin
  for i:= #0 to #$40 do LowerCaseTable[i]:= i;
  for i:= #$41 to #$5A do LowerCaseTable[i]:= AnsiChar(Ord(i) + $20);
  for i:= #$5B to #$FF do LowerCaseTable[i]:= i;
end;
//------------------------------------------------------------------------------

procedure RegisterAttribute(hashedName: Cardinal; func: TAttribFunc); overload;
begin
  AttribFuncList.AddObject(InttoHex(hashedName, 8), @func) ;
end;
//------------------------------------------------------------------------------

procedure RegisterAttribute(const attribName: AnsiString; func: TAttribFunc); overload;
begin
  RegisterAttribute(
    GetHashedName(PAnsiChar(attribName), Length(attribName)), func);
end;
//------------------------------------------------------------------------------

procedure GradientTransform_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradientElement then
    with TGradientElement(attrib.aOwnerEl) do
      ParseTransform(attrib.aValue, attrib.aValueLen);
end;
//------------------------------------------------------------------------------

procedure Gradientunits_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradientElement then
    with TGradientElement(attrib.aOwnerEl) do
      gradientUnits := GetHashedName(attrib.aValue, attrib.aValueLen);
end;
//------------------------------------------------------------------------------

procedure Id_Attrib(attrib: PAttrib);
var
  id: AnsiString;
begin
  with attrib^ do
  begin
    id := LowercaseAnsi(aValue, aValueLen);
    aOwnerEl.fReader.fIdList.AddObject(string(id), aOwnerEl);
  end;
end;
//------------------------------------------------------------------------------

procedure Href_Attrib(attrib: PAttrib);
var
  el, refEl: TElement;
begin
  el := attrib.aOwnerEl;
  if el is TGradientElement then
  begin
    refEl := el.FindRefElement(attrib.aValue, attrib.aValueLen);
    if assigned(refEl) and (refEl is TGradientElement) then
      TGradientElement(refEl).AssignTo(el);
  end
  else if el is TUseElement then
  begin
    refEl := el.FindRefElement(attrib.aValue, attrib.aValueLen);
    if assigned(refEl) and (el is TUseElement) then
      TUseElement(el).refEl := refEl;
  end;
end;
//------------------------------------------------------------------------------

procedure Class_Attrib(attrib: PAttrib);
begin
  TElement(attrib.aOwnerEl).ParseClassAttrib(attrib);
end;
//------------------------------------------------------------------------------

procedure ClipPath_Attrib(attrib: PAttrib);
var
  refEl: TElement;
begin
  refEl := attrib.aOwnerEl.FindRefElement(attrib.aValue, attrib.aValueLen);
  if assigned(refEl) and (refEl is TClipPathElement) then
    attrib.aOwnerEl.fDrawInfo.clipPathEl := TClipPathElement(refEl);
end;
//------------------------------------------------------------------------------

procedure D_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TPathElement then
    TPathElement(attrib.aOwnerEl).ParseD(attrib);
end;
//------------------------------------------------------------------------------

procedure Fill_Attrib(attrib: PAttrib);
begin
  AttribToColor32(attrib, attrib.aOwnerEl.fDrawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure FillOpacity_Attrib(attrib: PAttrib);
begin
  AttribToOpacity(attrib, attrib.aOwnerEl.fDrawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure DashArray_Attrib(attrib: PAttrib);
var
  current, currentEnd: PAnsiChar;
  val: double;
  len: integer;
begin
  current := attrib.aValue;
  currentEnd := current + attrib.aValueLen;
  with attrib.aOwnerEl.fDrawInfo do
  begin
    len := Length(dashArray);
    while GetNum(current, currentEnd, true, val) do
    begin
      SetLength(dashArray, len +1);
      dashArray[len] := val;
      inc(len);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure DashOffset_Attrib(attrib: PAttrib);
var
  current, currentEnd: PAnsiChar;
begin
  current := attrib.aValue;
  currentEnd := current + attrib.aValueLen;
  with attrib.aOwnerEl.fDrawInfo do
    GetNum(current, currentEnd, true, dashOffset);
end;
//------------------------------------------------------------------------------

procedure Display_Attrib(attrib: PAttrib);
begin
  if GetHashedName(attrib.aValue, attrib.aValueLen) = hNone then
    attrib.aOwnerEl.fDrawInfo.hidden := true;
end;
//------------------------------------------------------------------------------

procedure Font_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TTextElement then
    with TTextElement(attrib.aOwnerEl) do
      AttribToFontInfo(attrib, fontInfo)
  else if attrib.aOwnerEl is TTSpanElement then
    with TTSpanElement(attrib.aOwnerEl) do
      AttribToFontInfo(attrib, fontInfo);
end;
//------------------------------------------------------------------------------

procedure FontFamily_Attrib(attrib: PAttrib);

  procedure GetFamily(var fontInfo: TFontInfo);
  var
    word: AnsiString;
  begin
    if GetNextAlphaWord(attrib.aValue,
      attrib.aValue + attrib.aValueLen, word) then
    begin
      case GetHashedName(PAnsiChar(word), Length(word)) of
        hSerif      : fontInfo.family := ffSerif;
        hMonospace  : fontInfo.family := ffMonospace;
        else          fontInfo.family := ffSansSerif;
      end;
    end;
  end;

begin
  if attrib.aOwnerEl is TTextElement then
    GetFamily(TTextElement(attrib.aOwnerEl).fontInfo)
  else if attrib.aOwnerEl is TTSpanElement then
    GetFamily(TTSpanElement(attrib.aOwnerEl).fontInfo);
end;
//------------------------------------------------------------------------------

procedure FontSize_Attrib(attrib: PAttrib);
var
  num: double;
begin
  if attrib.aOwnerEl is TTextElement then
  begin
    if GetNum(attrib.aValue, attrib.aValue + attrib.aValueLen,
      false, num) then TTextElement(attrib.aOwnerEl).fontInfo.size := num;
  end
  else if (attrib.aOwnerEl is TTSpanElement) and
    GetNum(attrib.aValue, attrib.aValue + attrib.aValueLen, false, num) then
      TTSpanElement(attrib.aOwnerEl).fontInfo.size := num
end;
//------------------------------------------------------------------------------

procedure FontStyle_Attrib(attrib: PAttrib);

  procedure GetStyle(var fontInfo: TFontInfo);
  var
    word: AnsiString;
  begin
    if GetNextAlphaWord(attrib.aValue,
      attrib.aValue + attrib.aValueLen, word) then
    begin
      if GetHashedName(PAnsiChar(word), Length(word)) = hItalic then
        Include(fontInfo.styles, fsItalic) else
        Exclude(fontInfo.styles, fsItalic);
    end;
  end;
begin
  if attrib.aOwnerEl is TTextElement then
    GetStyle(TTextElement(attrib.aOwnerEl).fontInfo)
  else if attrib.aOwnerEl is TTSpanElement then
    GetStyle(TTSpanElement(attrib.aOwnerEl).fontInfo);
end;
//------------------------------------------------------------------------------

procedure FontWeight_Attrib(attrib: PAttrib);

  procedure GetWeight(var fontInfo: TFontInfo);
  var
    num: double;
    word: AnsiString;
  begin
    if NumPending(attrib.aValue, attrib.aValue + attrib.aValueLen, false) and
      GetNum(attrib.aValue, attrib.aValue + attrib.aValueLen, false, num) then
    begin
      if num >= 600 then
        Include(fontInfo.styles, fsBold) else
        Exclude(fontInfo.styles, fsBold);
    end
    else if GetNextAlphaWord(attrib.aValue,
      attrib.aValue + attrib.aValueLen, word) then
    begin
      if GetHashedName(PAnsiChar(word), Length(word)) = hBold then
        Include(fontInfo.styles, fsBold) else
        Exclude(fontInfo.styles, fsBold);
    end;
  end;

begin
  if attrib.aOwnerEl is TTextElement then
    GetWeight(TTextElement(attrib.aOwnerEl).fontInfo)
  else if attrib.aOwnerEl is TTSpanElement then
    GetWeight(TTSpanElement(attrib.aOwnerEl).fontInfo);
end;
//------------------------------------------------------------------------------

procedure Fx_Attrib(attrib: PAttrib);
var
  num: double;
begin
  if (attrib.aOwnerEl is TRadGradElement) and
    AttribToFloat(attrib, num) then
      TRadGradElement(attrib.aOwnerEl).F.X := num;
end;
//------------------------------------------------------------------------------

procedure Fy_Attrib(attrib: PAttrib);
var
  num: double;
begin
  if (attrib.aOwnerEl is TRadGradElement) and
    AttribToFloat(attrib, num) then
      TRadGradElement(attrib.aOwnerEl).F.Y := num;
end;
//------------------------------------------------------------------------------

procedure TextAlign_Attrib(attrib: PAttrib);

  procedure GetAlign(var fontInfo: TFontInfo);
  var
    word: AnsiString;
  begin
    if GetNextAlphaWord(attrib.aValue,
      attrib.aValue + attrib.aValueLen, word) then
    begin
      case GetHashedName(PAnsiChar(word), Length(word)) of
        hStart  : fontInfo.align := taLeft;
        hMiddle : fontInfo.align := taMiddle;
        hEnd    : fontInfo.align := taRight;
      end;
    end;
  end;

begin
  if attrib.aOwnerEl is TTextElement then
    GetAlign(TTextElement(attrib.aOwnerEl).fontInfo)
  else if attrib.aOwnerEl is TTSpanElement then
    GetAlign(TTSpanElement(attrib.aOwnerEl).fontInfo);
end;
//------------------------------------------------------------------------------

procedure MarkerStart_Attrib(attrib: PAttrib);
var
  el: TElement;
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  el := GetAttribUrl(attrib);
  if assigned(el) and (el is TMarkerElement) then
    attrib.aOwnerEl.fDrawInfo.markerStart := TMarkerElement(el);
end;
//------------------------------------------------------------------------------

procedure MarkerMiddle_Attrib(attrib: PAttrib);
var
  el: TElement;
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  el := GetAttribUrl(attrib);
  if assigned(el) and (el is TMarkerElement) then
    attrib.aOwnerEl.fDrawInfo.markerMiddle := TMarkerElement(el);
end;
//------------------------------------------------------------------------------

procedure MarkerEnd_Attrib(attrib: PAttrib);
var
  el: TElement;
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  el := GetAttribUrl(attrib);
  if assigned(el) and (el is TMarkerElement) then
    attrib.aOwnerEl.fDrawInfo.markerEnd := TMarkerElement(el);
end;
//------------------------------------------------------------------------------

procedure Filter_Attrib(attrib: PAttrib);
var
  el: TElement;
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  el := GetAttribUrl(attrib);
  if assigned(el) and (el is TFilterElement) then
    attrib.aOwnerEl.fDrawInfo.filterEl := TFilterElement(el);
end;
//------------------------------------------------------------------------------


procedure Offset_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradStopElement then
    AttribToFloat(attrib, TGradStopElement(attrib.aOwnerEl).offset);
end;
//------------------------------------------------------------------------------

procedure Opacity_Attrib(attrib: PAttrib);
var
  opacity: double;
begin
  if not AttribToFloat(attrib, opacity) then Exit;
  if opacity < 0 then opacity := 0
  else if opacity > 1 then opacity := 1;
  attrib.aOwnerEl.fDrawInfo.opacity := Round(opacity * 255);
end;
//------------------------------------------------------------------------------

procedure StopColor_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradStopElement then
    AttribToColor32(attrib, TGradStopElement(attrib.aOwnerEl).color);
end;
//------------------------------------------------------------------------------

procedure StopOpacity_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradStopElement then
  AttribToOpacity(attrib, TGradStopElement(attrib.aOwnerEl).color);
end;
//------------------------------------------------------------------------------

procedure Points_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TPolyElement then
    TPolyElement(attrib.aOwnerEl).ParsePoints(attrib);
end;
//------------------------------------------------------------------------------

procedure Stroke_Attrib(attrib: PAttrib);
begin
  AttribToColor32(attrib, attrib.aOwnerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeOpacity_Attrib(attrib: PAttrib);
begin
  AttribToOpacity(attrib, attrib.aOwnerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeWidth_Attrib(attrib: PAttrib);
begin
    AttribToFloat(attrib, attrib.aOwnerEl.fDrawInfo.strokeWidth);
end;
//------------------------------------------------------------------------------

procedure FillRule_Attrib(attrib: PAttrib);
begin
  if LowerCaseTable[attrib.aValue[0]] = 'e' then
    attrib.aOwnerEl.fDrawInfo.fillRule := frEvenOdd else
    attrib.aOwnerEl.fDrawInfo.fillRule := frNonZero;
end;
//------------------------------------------------------------------------------

procedure Style_Attrib(attrib: PAttrib);
begin
  attrib.aOwnerEl.ParseStyle(attrib.aValue, attrib.aValueLen);
end;
//------------------------------------------------------------------------------

procedure Transform_Attrib(attrib: PAttrib);
begin
  attrib.aOwnerEl.ParseTransform(attrib.aValue, attrib.aValueLen);
end;
//------------------------------------------------------------------------------

procedure Viewbox_Attrib(attrib: PAttrib);

  function LoadViewbox: TRectD;
  var
    current, currentEnd: PAnsiChar;
  begin
    current := attrib.aValue;
    currentEnd := current + attrib.aValueLen;
    with Result do
    if GetNum(current, currentEnd, false, Left) and
      GetNum(current, currentEnd, true, Top) and
      GetNum(current, currentEnd, true, Right) and
      GetNum(current, currentEnd, true, Bottom) then
    begin
      Right := Right + Left;
      Bottom := Bottom + Top;
    end else
      Result := NullRectD;
  end;

begin
  if attrib.aOwnerEl is TRootElement then
    TRootElement(attrib.aOwnerEl).viewbox := LoadViewbox
  else if attrib.aOwnerEl is TMarkerElement then
    TMarkerElement(attrib.aOwnerEl).viewbox := LoadViewbox;
end;
//------------------------------------------------------------------------------

procedure Height_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, recWH.Height);
    hSvg:
      with TRootElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, height);
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, height);
    hFilter:
      with TFilterElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, recWH.Height);
  end;
end;
//------------------------------------------------------------------------------

procedure Width_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, recWH.Width);
    hSvg:
      with TRootElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, width);
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, width);
    hFilter:
      with TFilterElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, recWH.Width);
  end;
end;
//------------------------------------------------------------------------------

procedure Cx_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hCircle:
      with TCircleElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, centerPt.X);
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, centerPt.X);
    hRadialGradient:
      with TRadGradElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, C.X);
  end;
end;
//------------------------------------------------------------------------------

procedure Cy_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hCircle:
      with TCircleElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, centerPt.Y);
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, centerPt.Y);
    hRadialGradient:
      with TRadGradElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, C.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure Rx_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, radius.X);
    hCircle:
      with TCircleElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, radius);
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, radius.X);
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, ref.X);
  end;
end;
//------------------------------------------------------------------------------

procedure Ry_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, radius.Y);
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, radius.Y);
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, ref.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure StdDev_Attrib(attrib: PAttrib);
var
  sd: double;
begin
  if (attrib.aOwnerEl is TGaussianElement) then
    with TGaussianElement(attrib.aOwnerEl) do
    begin
      AttribToFloat(attrib, sd);
      if (sd > 0) and (sd < 100) then stdDev := sd;
    end;
end;
//------------------------------------------------------------------------------

procedure X1_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, recWH.left);
    hLine:
      with TLineElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, path[0].X);
    hLinearGradient:
      with TLinGradElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, startPt.X);
    hText:
      with TTextElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, pt.X);
    hTSpan:
      with TTSpanElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, pt.X);
    hUse:
      with TUseElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, pt.X);
    hFilter:
      with TFilterElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, recWH.Left);
  end;
end;
//------------------------------------------------------------------------------

procedure X2_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hLine:
      with TLineElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, path[1].X);
    hLinearGradient:
      with TLinGradElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, endPt.X);
  end;
end;
//------------------------------------------------------------------------------

procedure Y1_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, recWH.top);
    hLine:
      with TLineElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, path[0].Y);
    hLinearGradient:
      with TLinGradElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, startPt.Y);
    hText:
      with TTextElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, pt.Y);
    hTSpan:
      with TTSpanElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, pt.Y);
    hUse:
      with TUseElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, pt.Y);
    hFilter:
      with TFilterElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, recWH.Top);
  end;
end;
//------------------------------------------------------------------------------

procedure Y2_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hLine:
      with TLineElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, path[1].Y);
    hLinearGradient:
      with TLinGradElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, endPt.Y);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure MakeColorConstList;
var
  i: integer;
begin
  ColorConstList := TStringList.Create;
  ColorConstList.Capacity := Length(ColorConsts);
  for i := 0 to High(ColorConsts) do
    with ColorConsts[i] do
      ColorConstList.AddObject(ColorName, Pointer(ColorValue));
  ColorConstList.Sorted := true;
end;
//------------------------------------------------------------------------------

procedure MakeAttribFuncList;
begin
  AttribFuncList := TStringList.Create;
  AttribFuncList.Duplicates := dupError;

  RegisterAttribute(hClass,                 Class_Attrib);
  RegisterAttribute(hClip_045_path,         ClipPath_Attrib);
  RegisterAttribute(hCx,                    Cx_Attrib);
  RegisterAttribute(hCy,                    Cy_Attrib);
  RegisterAttribute(hD,                     D_Attrib);
  RegisterAttribute(hDisplay,               Display_Attrib);
  RegisterAttribute(hStroke_045_DashArray,  DashArray_Attrib);
  RegisterAttribute(hStroke_045_DashOffset, DashOffset_Attrib);
  RegisterAttribute(hFill,                  Fill_Attrib);
  RegisterAttribute(hFill_045_Opacity,      FillOpacity_Attrib);
  RegisterAttribute(hFill_045_Rule,         FillRule_Attrib);
  RegisterAttribute(hFilter,                Filter_Attrib);
  RegisterAttribute(hFont,                  Font_Attrib);
  RegisterAttribute(hFont_045_Family,       FontFamily_Attrib);
  RegisterAttribute(hFont_045_Size,         FontSize_Attrib);
  RegisterAttribute(hFont_045_Style,        FontStyle_Attrib);
  RegisterAttribute(hFont_045_Weight,       FontWeight_Attrib);
  RegisterAttribute(hFx,                    Fx_Attrib);
  RegisterAttribute(hFy,                    Fy_Attrib);
  RegisterAttribute(hGradientTransform,     GradientTransform_Attrib);
  RegisterAttribute(hGradientUnits,         Gradientunits_Attrib);
  RegisterAttribute(hHeight,                Height_Attrib);
  RegisterAttribute(hHref,                  Href_Attrib);
  RegisterAttribute(hId,                    Id_Attrib);
  RegisterAttribute(hMarker_045_End,        MarkerEnd_Attrib);
  RegisterAttribute(hMarkerHeight,          Height_Attrib);
  RegisterAttribute(hMarker_045_Mid,        MarkerMiddle_Attrib);
  RegisterAttribute(hMarker_045_Start,      MarkerStart_Attrib);
  RegisterAttribute(hMarkerWidth,           Width_Attrib);
  RegisterAttribute(hOffset,                Offset_Attrib);
  RegisterAttribute(hOpacity,               Opacity_Attrib);
  RegisterAttribute(hPoints,                Points_Attrib);
  RegisterAttribute(hR,                     Rx_Attrib);
  RegisterAttribute(hRefX,                  Rx_Attrib);
  RegisterAttribute(hRefY,                  Ry_Attrib);
  RegisterAttribute(hRx,                    Rx_Attrib);
  RegisterAttribute(hRy,                    Ry_Attrib);
  RegisterAttribute(hstdDeviation,          StdDev_Attrib);
  RegisterAttribute(hStop_045_Color,        StopColor_Attrib);
  RegisterAttribute(hStop_045_Opacity,      StopOpacity_Attrib);
  RegisterAttribute(hStroke,                Stroke_Attrib);
  RegisterAttribute(hStroke_045_Opacity,    StrokeOpacity_Attrib);
  RegisterAttribute(hStroke_045_Width,      StrokeWidth_Attrib);
  RegisterAttribute(hStyle,                 Style_Attrib);
  RegisterAttribute(hText_045_Anchor,       TextAlign_Attrib);
  RegisterAttribute(hTransform,             Transform_Attrib);
  RegisterAttribute(hViewbox,               Viewbox_Attrib);
  RegisterAttribute(hWidth,                 Width_Attrib);
  RegisterAttribute(hX,                     X1_Attrib);
  RegisterAttribute(hX1,                    X1_Attrib);
  RegisterAttribute(hX2,                    X2_Attrib);
  RegisterAttribute(hXlink_058_Href,        Href_Attrib);
  RegisterAttribute(hY,                     Y1_Attrib);
  RegisterAttribute(hY1,                    Y1_Attrib);
  RegisterAttribute(hY2,                    Y2_Attrib);

  AttribFuncList.Sorted := true;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  defaultFontHeight := DPIAware(9);
  MakeLowerCaseTable;
  MakeAttribFuncList;
  MakeColorConstList;

finalization
  AttribFuncList.Free;
  ColorConstList.Free;
end.
