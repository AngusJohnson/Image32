unit Image32_SVG_Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.24                                                            *
* Date      :  6 May 2021                                                      *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2021                                         *
*                                                                              *
* Purpose   :  Read SVG files                                                  *
*                                                                              *
*              To propery read SVG files and support all the features of the   *
*              vers. 2 specification is a huge task. So while this unit does a *
*              pretty decent job of reading most SVG files, it remains mostly  *
*              a proof of concept.                                             *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types, Math,
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Image32, Image32_Vector, Image32_Draw, Image32_Transform, Image32_Ttf;

type
  TFillElement      = class;
  TMarkerElement    = class;
  TFilterElement    = class;
  TClipPathElement  = class;
  TSvgElement       = class;

  TFontSyle = (fsBold, fsItalic);
  TFontSyles = set of TFontSyle;
  TTextAlign = (taLeft, taMiddle, taRight);

  TFontInfo = record
    family: TTtfFontFamily;
    size: double;
    styles: TFontSyles;
    align: TTextAlign;
  end;

  TMeasureUnit = (muNone, muPercent, muDegree, muRadian,
    muInch, muCm, muMm, muEm, muEn);

  TDrawInfo = record
    fillColor     : TColor32;
    fillRule      : TFillRule;
    fillEl        : PAnsiChar;
    strokeColor   : TColor32;
    strokeWidth   : double;
    strokeEl      : PAnsiChar;
    dashArray     : TArrayOfDouble;
    dashOffset    : double;
    markerStart   : PAnsiChar;
    markerMiddle  : PAnsiChar;
    markerEnd     : PAnsiChar;
    filterEl      : PAnsiChar;
    clipPathEl    : PAnsiChar;
    opacity       : Byte;
    //lineCap       : TEndStyle;     //stroke-linecap
    //lineJoin      : TJoinStyle;    //stroke-linejoin
    //miterLim      : double;        //stroke-miterlimit
    matrix        : TMatrixD;
    visible       : Boolean;
    using         : Boolean;
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
{$IFDEF XPLAT_GENERICS}
    fChilds         : TList<TElement>;
{$ELSE}
    fChilds         : TList;
{$ENDIF}
    fHasContent     : Boolean;
    fStyleAttribIdx : integer;
    fName           : PAnsiChar;
    fNameLen        : integer;
    fNameHash       : Cardinal;
    fAttribs        : TArrayOfAttrib;
    fDrawInfo       : TDrawInfo;
    fCurrent        : PAnsiChar;
    fCurrentEnd     : PAnsiChar;
    function LoadChild: TElement;
    function HashCurrentWord(out hash: Cardinal): Boolean;
    function GetName(out name: PAnsiChar;
      out nameLen: integer): Boolean;
    function SetAttribValue(attrib: PAttrib): Boolean;
    function FindRefElement(refname: PAnsiChar): TElement;
    procedure ProcessAttrib(const attrib: PAttrib);
    function PeekNextChar: AnsiChar;
    procedure ParseClassAttrib(classAttrib: PAttrib);
    procedure ParseStyle(classStyle: PAnsiChar; len: integer);
    procedure ParseTransform(transform: PAnsiChar; len: integer);
    function AddAttribute: PAttrib;
    function GetSvgElement: TSvgElement;
  protected
    function LoadAttributes: Boolean; virtual;
    function LoadContent: Boolean; virtual;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); virtual;
    destructor  Destroy; override;
  end;

  TSvgElement = class(TElement)
  protected
    viewbox: TRectWH;
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
    function LoadContent: Boolean; override;
  end;

  TShapeElement = class(TElement)
  protected
    drawPathsO  : TPathsD;
    drawPathsC  : TPathsD;
    function HasMarkers: Boolean;
    function GetVal(out val: double): Boolean;

    procedure GetDrawPaths(arcScale: double); virtual;
    procedure DrawFilled(img: TImage32; const drawInfo: TDrawInfo);
    procedure DrawStroke(img: TImage32; const drawInfo: TDrawInfo; isClosed: Boolean);
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
    procedure DrawMarkers(img: TImage32; drawInfo: TDrawInfo);
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TUseElement = class(TShapeElement)
  protected
    recWH: TRectWH;
    refEl: PAnsiChar;
    procedure GetDrawPaths(arcScale: double); override;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
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
    procedure Flatten(index: integer; arcScale: double;
      out path: TPathD; out isClosed: Boolean);
  protected
    procedure ParseD(attrib: PAttrib);
    procedure GetDrawPaths(arcScale: double); override;
  end;

  TPolyElement = class(TShapeElement) //polyline or polygon
  protected
    path: TPathD;
    procedure ParsePoints(attrib: PAttrib);
    procedure GetDrawPaths(arcScale: double); override;
  end;

  TLineElement = class(TShapeElement)
  protected
    path: TPathD;
    procedure GetDrawPaths(arcScale: double); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TCircleElement = class(TShapeElement)
  protected
    centerPt  : TPointD;
    radius    : double;
    procedure GetDrawPaths(arcScale: double); override;
  end;

  TEllipseElement = class(TShapeElement)
  protected
    centerPt  : TPointD;
    radius    : TPointD;
    procedure GetDrawPaths(arcScale: double); override;
  end;

  TRectElement = class(TShapeElement)
  protected
    recWH     : TRectWH;
    radius    : TPointD;
    procedure GetDrawPaths(arcScale: double); override;
  end;

  //TTextElement: although this is a TShapeElement descendant, it's really
  //only a container for 'subtext' and 'tspan' elements. (See Draw method.)
  TTextElement = class(TShapeElement)
  protected
    pt        : TPointD;
    delta     : TPointD;
    tmpPt     : TPointD;
    fontInfo  : TFontInfo;
    procedure ResetTmpPt;
    procedure GetDrawPaths(arcScale: double); override;
    function AddSubtext(atext: PAnsiChar; len: integer): TElement;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TSubtextElement   = class(TTextElement)
  protected
    text: PAnsiChar;
    textLen: integer;
    function GetTextElement: TTextElement;
    procedure GetDrawPaths(arcScale: double); override;
  end;

  TTSpanElement = class(TSubtextElement)
  protected
    delta     : TPointD;
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
    viewbox: TRectWH;
    angle: double;
    autoStartReverse: Boolean;
    callerEl: TShapeElement;
    procedure SetSoloPoint(const pt: TPointD; angle: double);
    procedure SetPoints(const points: TPathD);
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TSvgColorStop = record
    offset: double;
    color: TColor32;
  end;
  TSvgColorStops = array of TSvgColorStop;

  TFillElement = class(TElement) //either a gradient or pattern element
  protected
    refEl: PAnsiChar;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRect): Boolean; virtual;
  end;

  TGradientElement = class(TFillElement)
  protected
    stops: TSvgColorStops;
    gradientUnits: Cardinal;
    function LoadContent: Boolean; override;
    procedure AddStop(color: TColor32; offset: double);
    procedure AssignTo(other: TElement);  virtual;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRect): Boolean; override;
  end;

  TRadGradElement = class(TGradientElement)
  protected
    F, C: TPointD;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRect): Boolean; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TLinGradElement = class(TGradientElement)
  protected
    startPt, endPt: TPointD;
    procedure AssignTo(other: TElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
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
    filterRecWH: TRectWH;
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
    procedure GetDrawPaths(arcScale: double); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TAttribFunc = procedure (attrib: PAttrib);

  TSvgReader = class
  private
    fMemStream        : TMemoryStream;
    fEndStream        : PAnsiChar;
    fBackgroundColor  : TColor32;
    fTempImage        : TImage32;
    fIdList           : TStringList;
    fClassStylesList  : TStrAnsiList;

    fLinGradRenderer  : TLinearGradientRenderer;
    fRadGradRenderer  : TSvgRadialGradientRenderer;

{$IFDEF XPLAT_GENERICS}
    fFontList         : TList<TFontReader>;
{$ELSE}
    fFontList         : TList;
{$ENDIF}
    fFontCache        : TGlyphCache;
    fViewMatrix           : TMatrixD;
    fRootElement          : TSvgElement;
  protected
    function GetSvgStart(out svgStart: PAnsiChar): Boolean;
    procedure GetBestFont(const svgFontInfo: TFontInfo);
    property RadGradRenderer: TSvgRadialGradientRenderer read fRadGradRenderer;
    property LinGradRenderer: TLinearGradientRenderer read fLinGradRenderer;
    property TempImage      : TImage32 read fTempImage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
{$IFDEF MSWINDOWS}
    function AddFont(const fontName: string): Boolean;
{$ENDIF}
    function AddFontFromResource(const resName: string; resType: PChar): Boolean;
    procedure DrawImage(img: TImage32; scaleToImage: Boolean);
    function LoadFromStream(stream: TStream): Boolean;
    function LoadFromFile(const filename: string): Boolean;
    property BackgroundColor: TColor32
      read fBackgroundColor write fBackgroundColor;
  end;

implementation

uses
  Image32_Extra, StrUtils;

type
  TColorConst = record
    ColorName : string;
    ColorValue: Cardinal;
  end;

const
  buffSize = 32;
  clInvalid = $00010001;

  defaultDrawInfo: TDrawInfo =
    (fillColor: clBlack32; fillRule: frNonZero; fillEl: nil;
    strokeColor: clInvalid; strokeWidth: 1.0;
    dashArray: nil; dashOffset: 0;
    markerStart: nil; markerMiddle: nil; markerEnd: nil;
    filterEl: nil;  clipPathEl: nil; opacity: 255;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); visible: true; using: false);

  emptyDrawInfo: TDrawInfo =
    (fillColor: clInvalid; fillRule: frNonZero; fillEl: nil;
    strokeColor: clInvalid; strokeWidth: -Infinity;
    dashArray: nil; dashOffset: 0;
    markerStart: nil; markerMiddle: nil; markerEnd: nil;
    filterEl: nil;  clipPathEl: nil; opacity: 255;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); visible: true; using: false);

  defaultFontInfo: TFontInfo =
    (family: ttfSansSerif; size: 10; styles: []; align: taLeft);

  //include lots of color constants
  {$I html_color_consts.inc}

  //include many string hash constants
  {$I Image32_SVG_HashConsts.inc}

var
  ColorConstList : TStringList;
  AttribFuncList : TStringList;

  LowerCaseTable : array[#0..#255] of AnsiChar;
  defaultFontHeight: double;

resourcestring
  rsListBoundsError     = 'List index out of bounds (%d)';

{$IF COMPILERVERSION < 17}
type
  TSetOfChar = set of Char;

function CharInSet(chr: Char; chrs: TSetOfChar): Boolean;
begin
  Result := chr in chrs;
end;
{$IFEND}

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

procedure UpdateDrawInfo(var drawInfo: TDrawInfo; const parentDI: TDrawInfo);
begin
  with parentDI do
  begin
    if fillColor <> clInvalid then
      drawInfo.fillColor := fillColor;
    if Assigned(fillEl) then
      drawInfo.fillEl := fillEl;
    if strokeColor <> clInvalid then
      drawInfo.strokeColor := strokeColor;
    if strokeWidth >= 0 then
      drawInfo.strokeWidth := strokeWidth;
    if Assigned(dashArray) then
      drawInfo.dashArray := Copy(dashArray, 0, Length(dashArray));
    if Assigned(strokeEl) then
      drawInfo.strokeEl := strokeEl;
    if opacity < 255 then
      drawInfo.opacity := opacity;
    if Assigned(filterEl) then
      drawInfo.filterEl := filterEl;
    if not IsIdentityMatrix(matrix) then
      drawInfo.matrix := MatrixMultiply(drawInfo.matrix, matrix);
  end;
end;
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

function SkipBlanksAndComma(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
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
  //nb: style content may include multi-line comment blocks
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

function GetUtf8String(pName: PAnsiChar; len: integer): AnsiString;
begin
  SetLength(Result, len);
  if len = 0 then Exit;
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

function IsAlpha(c: AnsiChar): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := CharInSet(c, ['A'..'Z','a'..'z']);
end;
//------------------------------------------------------------------------------

function GetNameLength(var c: PAnsiChar): integer; overload;
var
  startC: PAnsiChar;
const
  validNonFirstChars =  ['0'..'9','A'..'Z','a'..'z','_',':','-'];
begin
  Result := 0;
  if not IsAlpha(c^) then Exit;
  startC := c; inc(c);
  while CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - startC;
end;
//------------------------------------------------------------------------------

function GetNameLength(var c: PAnsiChar; endC: PAnsiChar): integer; overload;
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

function GetHashedNameCaseSens(c: PAnsiChar; length: integer): cardinal;
var
  i: integer;
begin
  //https://en.wikipedia.org/wiki/Jenkins_hash_function
  Result := 0;
  for i := 1 to length do
  begin
    Result := (Result + Ord(c^));
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

function GetRefName(href: PAnsiChar): PAnsiChar; {$IFDEF INLINE} inline; {$ENDIF}
begin
  if (href^ = 'u') and
    ((href +1)^ = 'r') and
    ((href +2)^ = 'l') and
    ((href +3)^ = '(') then
      inc(href, 4);
  if href^ = '#' then inc(href);
  Result := href;
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

function HtmlDecode(const s: ansiString): ansistring;
var
  val, len: integer;
  c,ce,endC: PAnsiChar;
begin
  len := Length(s);
  SetLength(Result, len*3);
  c := PAnsiChar(s);
  endC := c + len;
  ce := c;
  len := 1;
  while (ce < endC) and (ce^ <> '&') do
    inc(ce);

  while (ce < endC) do
  begin
    if ce > c then
    begin
      Move(c^, Result[len], ce - c);
      inc(len, ce - c);
    end;
    c := ce; inc(ce);
    while (ce < endC) and (ce^ <> ';') do inc(ce);
    if ce = endC then break;

    val := -1; //assume error
    if (c +1)^ = '#' then
    begin
      val := 0;
      //decode unicode value
      if (c +2)^ = 'x' then
      begin
        inc(c, 3);
        while c < ce do
        begin
          if (c^ >= 'a') and (c^ <= 'f') then
            val := val * 16 + Ord(c^) - 87
          else if (c^ >= 'A') and (c^ <= 'F') then
            val := val * 16 + Ord(c^) - 55
          else if (c^ >= '0') and (c^ <= '9') then
            val := val * 16 + Ord(c^) - 48
          else
          begin
            val := -1;
            break;
          end;
          inc(c);
        end;
      end else
      begin
        inc(c, 2);
        while c < ce do
        begin
          val := val * 10 + Ord(c^) - 48;
          inc(c);
        end;
      end;
    end else
    begin
      //decode html entity ...
      case GetHashedNameCaseSens(c, ce - c) of
        {$I html_entities.inc}
      end;
    end;

    //convert unicode value to utf8 chars
    //this saves the overhead of multiple ansistring<-->string conversions.
    case val of
      0 .. $7F:
        result[len] := AnsiChar(val);
      $80 .. $7FF:
        begin
          Result[len] := AnsiChar($C0 or (val shr 6));
          Result[len+1] := AnsiChar($80 or (val and $3f));
          inc(len, 2);
        end;
      $800 .. $7FFF:
        begin
          Result[len] := AnsiChar($E0 or (val shr 12));
          Result[len+1] := AnsiChar($80 or ((val shr 6) and $3f));
          Result[len+2] := AnsiChar($80 or (val and $3f));
          inc(len, 3);
        end;
      $10000 .. $10FFFF:
        begin
          Result[len] := AnsiChar($F0 or (val shr 18));
          Result[len+1] := AnsiChar($80 or ((val shr 12) and $3f));
          Result[len+2] := AnsiChar($80 or ((val shr 6) and $3f));
          Result[len+3] := AnsiChar($80 or (val and $3f));
          inc(len, 4);
        end;
      else
      begin
        //ie: error
        Move(c^, Result[len], ce- c +1);
        inc(len, ce - c +1);
      end;
    end;
    inc(ce);
    c := ce;
    while (ce < endC) and (ce^ <> '&') do inc(ce);
  end;
  if (c < endC) and (ce > c) then
  begin
    Move(c^, Result[len], (ce - c));
    inc(len, ce - c);
  end;
  setLength(Result, len -1);
end;
//------------------------------------------------------------------------------

function TrigClampVal(val: double): double; {$IFDEF INLINE} inline; {$ENDIF}
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

    Image32_Vector.GetSinCos(phi_rads, s_phi, c_phi);;
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

function IsFilled(const drawInfo: TDrawInfo): Boolean;
begin
  Result := not drawInfo.visible or //can't tell yet so default to true
    (drawInfo.fillColor = clInvalid) or
    Assigned(drawInfo.fillEl) or (TARGB(drawInfo.fillColor).A > 0);
end;
//------------------------------------------------------------------------------

function IsStroked(const drawInfo: TDrawInfo): Boolean;
begin
  Result := not drawInfo.visible or //can't tell yet so default to true
    ((drawInfo.strokeWidth > 0) and
    (Assigned(drawInfo.strokeEl) or (TARGB(drawInfo.strokeColor).A > 0)));
end;
//------------------------------------------------------------------------------

function HexByteToInt(h: AnsiChar): Cardinal; {$IFDEF INLINE} inline; {$ENDIF}
begin
  case h of
    '0'..'9': Result := Ord(h) - Ord('0');
    'A'..'F': Result := 10 + Ord(h) - Ord('A');
    'a'..'f': Result := 10 + Ord(h) - Ord('a');
    else Result := 0;
  end;
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
  skipComma: Boolean; out val: double; out measureUnit: TMeasureUnit): Boolean; overload;
var
  decPos,exp: integer;
  isNeg, expIsNeg: Boolean;
  start: PAnsiChar;
begin
  Result := false;
  measureUnit := muNone;

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
{$IF COMPILERVERSION >= 17}
    if Ord(current^) = Ord(FormatSettings.DecimalSeparator) then
{$ELSE}
    if Ord(current^) = Ord(DecimalSeparator) then
{$IFEND}
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

  if decPos > 0 then val := val * Power(10, -decPos);
  if isNeg then val := -val;
  if IsValid(exp) then
  begin
    if expIsNeg then
      val := val * Power(10, -exp) else
      val := val * Power(10, exp);
  end;

  //https://oreillymedia.github.io/Using_SVG/guide/units.html
  case current^ of
    '%':
      begin
        val := val *0.00999; //fractionally less than 1.0
        inc(current);
        measureUnit := muPercent;
      end;
    'c': //convert cm to pixels
      if ((current+1)^ = 'm') then
      begin
        val := val * 96 / 2.54;
        inc(current, 2);
        measureUnit := muCm;
      end;
    'd': //ignore deg
      if ((current+1)^ = 'e') and ((current+2)^ = 'g') then
      begin
        inc(current, 3);
        measureUnit := muDegree;
      end;
    'i': //convert inchs to pixels
      if ((current+1)^ = 'n') then
      begin
        val := val * 96;
        inc(current, 2);
        measureUnit := muInch;
      end;
    'm': //convert mm to pixels
      if ((current+1)^ = 'm') then
      begin
        val := val * 96 / 25.4;
        inc(current, 2);
        measureUnit := muMm;
      end;
    'p': //ignore px
      if (current+1)^ = 'x' then inc(current, 2);
    'r': //convert radian angles to degrees
      if ((current+1)^ = 'a') and ((current+2)^ = 'd') then
      begin
        val := val * 180/PI;
        inc(current, 3);
        measureUnit := muRadian;
      end;
  end;
end;
//------------------------------------------------------------------------------

function GetNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean; overload;
var
  dummy: TMeasureUnit;
begin
  Result := GetNum(current, currentEnd, skipComma, val, dummy);
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
begin
  Result := false;
  if (attrib.aValueLen < 3) then Exit;
  alpha := color shr 24;

  if (attrib.aValue[0] = 'r') and                 //RGB / RGBA
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
    if GetHashedName(attrib.aName, attrib.aNameLen) = hFill then
      attrib.aOwnerEl.fDrawInfo.fillEl := GetRefName(attrib.aValue) else
      attrib.aOwnerEl.fDrawInfo.strokeEl := GetRefName(attrib.aValue);
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

  //and in case the opacity has been set before the color
  if (alpha > 0) and (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
  Result := true;
end;
//------------------------------------------------------------------------------

function AttribToFloat(attrib: PAttrib; var value: double): Boolean; overload;
var
  c: PAnsiChar;
begin
  c := attrib.aValue;
  Result := GetNum(c, attrib.aValue + attrib.aValueLen, false, value);
end;
//------------------------------------------------------------------------------

function AttribToFloat(attrib: PAttrib;
  var value: double; out measureUnit: TMeasureUnit): Boolean; overload;
var
  c: PAnsiChar;
begin
  c := attrib.aValue;
  Result := GetNum(c, attrib.aValue + attrib.aValueLen,
    false, value, measureUnit);
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
        hSans_045_Serif : fontInfo.family := ttfSansSerif;
        hSerif          : fontInfo.family := ttfSerif;
        hMonospace      : fontInfo.family := ttfMonospace;
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
// TDefsElement
//------------------------------------------------------------------------------

constructor TDefsElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.visible := false;
end;

//------------------------------------------------------------------------------
// TUseElement
//------------------------------------------------------------------------------

constructor TUseElement.Create(parent: TElement; hashName: Cardinal);
begin
  recWH.Left    := InvalidD;
  recWH.Top     := InvalidD;
  recWH.Width   := InvalidD;
  recWH.Height  := InvalidD;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TUseElement.GetDrawPaths(arcScale: double);
var
  el: TElement;
begin
  inherited;
  if Assigned(drawPathsC) or assigned(drawPathsO) then Exit;

  if not Assigned(refEl) then Exit;
  el := FindRefElement(refEl);
  if Assigned(el) and (el is TShapeElement) then
    with TShapeElement(el) do
    begin
      GetDrawPaths(arcScale);
      self.drawPathsC := CopyPaths(drawPathsC);
      self.drawPathsO := CopyPaths(drawPathsO);
    end;
end;
//------------------------------------------------------------------------------

procedure TUseElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  el: TElement;
  scale, dx, dy: double;
begin
  if not Assigned(self.refEl) then Exit;
  el := FindRefElement(refEl);
  if not Assigned(el) then Exit;
  if el is TShapeElement then
  begin
    UpdateDrawInfo(drawInfo, el.fDrawInfo);
    UpdateDrawInfo(drawInfo, fDrawInfo);
    drawInfo.using := true;
    if IsValid(recWH.Left) then dx := recWH.Left else dx := 0;
    if IsValid(recWH.Top) then dy := recWH.Top else dy := 0;
    scale := ExtractAvgScaleFromMatrix(drawInfo.matrix);
    MatrixTranslate(drawInfo.matrix, dx * scale, dy * scale);
    TShapeElement(el).Draw(img, drawInfo);
  end;
end;

//------------------------------------------------------------------------------
// TFillElement (gradient or filter fill)
//------------------------------------------------------------------------------

function TFillElement.PrepareRenderer(renderer: TCustomGradientRenderer;
  matrix: TMatrixD; rec: TRect): Boolean;
var
  el: TElement;
begin
  if Assigned(refEl) and (self is TGradientElement) then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TGradientElement) then
      TGradientElement(el).AssignTo(self);
  end;
  Result := true;
end;

//------------------------------------------------------------------------------
// TGradElement
//------------------------------------------------------------------------------

function TGradientElement.LoadContent: Boolean;
var
  i: integer;
begin
  Result := inherited LoadContent;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TGradStopElement then
      with TGradStopElement(fChilds[i]) do
        AddStop(color, offset);
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AddStop(color: TColor32; offset: double);
var
  i, len: integer;
begin
  len := Length(stops);
  setLength(stops, len+1);

  //make sure stops are in ascending offset order
  for i := 0 to len -1 do
    if offset < stops[i].offset then
    begin
      Move(stops[i], stops[i+1], (len -i) * SizeOf(TSvgColorStop));
      stops[i].offset := Min(1,Max(0, offset));
      stops[i].color := color;
      Exit;
    end;

  stops[len].offset := Min(1,Max(0, offset));
  stops[len].color := color;
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AssignTo(other: TElement);
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

function TGradientElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRect): Boolean;
var
  i, hiStops: integer;
begin
  inherited;
  hiStops := High(stops);
  Result := hiStops > 0;
  if not Result then Exit;
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

function TRadGradElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRect): Boolean;
var
  hiStops: integer;
  cp, fp: TPoint;
  pt: TPointD;
begin
  inherited;
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
  //nb: must call inherited after SetParameters
  Result := inherited PrepareRenderer(renderer, matrix, rec);
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

procedure TLinGradElement.AssignTo(other: TElement);
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

function TLinGradElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRect): Boolean;
var
  pt1, pt2: TPointD;
  hiStops: integer;
begin
  inherited;
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
  //nb: must call inherited after SetParameters
  Result := inherited PrepareRenderer(renderer, matrix, rec);
end;

//------------------------------------------------------------------------------
// TGradStopElement
//------------------------------------------------------------------------------

constructor TGradStopElement.Create(parent: TElement; hashName: Cardinal);
begin
  color := clBlack32;
  inherited;
end;

//------------------------------------------------------------------------------
// TFilterElement
//------------------------------------------------------------------------------

constructor TFilterElement.Create(parent: TElement; hashName: Cardinal);
begin
  filterRecWH.Left := InvalidD;
  filterRecWH.Top := InvalidD;
  filterRecWH.Width := InvalidD;
  filterRecWH.Height := InvalidD;
  inherited;
  fDrawInfo.visible := false;
end;

//------------------------------------------------------------------------------
// TGaussianElement
//------------------------------------------------------------------------------

constructor TGaussianElement.Create(parent: TElement; hashName: Cardinal);
begin
  stdDev := InvalidD;
  inherited;
end;

//------------------------------------------------------------------------------
// TClipPathElement
//------------------------------------------------------------------------------

constructor TClipPathElement.Create(parent: TElement; hashName: Cardinal);
begin
  fDrawInfo.visible := false;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipPathElement.GetDrawPaths(arcScale: double);
var
  i: integer;
begin
  inherited;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        GetDrawPaths(arcScale);
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

function TShapeElement.HasMarkers: Boolean;
begin
  Result := IsStroked(fDrawInfo) and (Assigned(fDrawInfo.markerStart) or
    Assigned(fDrawInfo.markerMiddle) or Assigned(fDrawInfo.markerEnd));
end;
//------------------------------------------------------------------------------

procedure TShapeElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
  q: double;
  stroked, filled: Boolean;
  arcScale: double;
  clipRec: TRectD;
  clipRec2: TRect;
  clipEl, filterEl: TElement;
  usingSpecialEffects: Boolean;
  tmpImg: TImage32;
const
  blurQual = 0; //0=Quite OK (faster); 1=very good; 2=best qualty (slow)
begin
  arcScale := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  if not drawInfo.using then
    UpdateDrawInfo(drawInfo, fDrawInfo);
  filled := IsFilled(drawInfo);
  stroked := IsStroked(drawInfo);

  GetDrawPaths(arcScale);

  if not (filled or stroked) or
    (not Assigned(drawPathsO) and
    not Assigned(drawPathsC)) then
  begin
    inherited; //probably a subtext pseudo-element
    Exit;
  end;

  MatrixApply(drawInfo.matrix, drawPathsO);
  MatrixApply(drawInfo.matrix, drawPathsC);

  usingSpecialEffects :=
    Assigned(fDrawInfo.clipPathEl) or   //clippath or
    (DrawInfo.opacity < 255) or         //reduced opacity or
    Assigned(fDrawInfo.filterEl);       //filter effect

  if usingSpecialEffects then
  begin
    clipRec := NullRectD;
    if Assigned(fDrawInfo.clipPathEl) then
      clipEl := FindRefElement(fDrawInfo.clipPathEl) else
      clipEl := nil;
    if Assigned(fDrawInfo.filterEl) then
      filterEl := FindRefElement(fDrawInfo.filterEl) else
      filterEl := nil;

    if Assigned(clipEl) then
    begin
      with TClipPathElement(clipEl) do
      begin
        GetDrawPaths(arcScale);
        //cliprect paths may be open or closed paths
        MatrixApply(drawInfo.matrix, drawPathsC);
        MatrixApply(drawInfo.matrix, drawPathsO);
        clipRec := UnionRect(GetBoundsD(drawPathsC), GetBoundsD(drawPathsO));
      end;
    end
    else if Assigned(filterEl) then
    begin
      clipRec := UnionRect(GetBoundsD(drawPathsC), GetBoundsD(drawPathsO));
      with TFilterElement(filterEl) do
      begin
        if IsValid(filterRecWH.Left) and IsValid(filterRecWH.Top) and
          IsValid(filterRecWH.Width) and IsValid(filterRecWH.Height) then
        begin
          //assume % of path bounds
          Image32_Vector.OffsetRect(clipRec,
            Round(clipRec.Width * filterRecWH.Left),
            Round(clipRec.Height * filterRecWH.Top));
          clipRec.Right := clipRec.Left + clipRec.Width * filterRecWH.Width;
          clipRec.Bottom := clipRec.Top + clipRec.Height * filterRecWH.Height;
        end
        else clipRec := InflateRect(clipRec,
          clipRec.Width * 0.2, clipRec.Height * 0.2);
      end;
    end else
    begin
      clipRec := UnionRect(GetBoundsD(drawPathsC), GetBoundsD(drawPathsO));
      q := drawInfo.strokeWidth / 2;
      clipRec := InflateRect(clipRec, q, q);
    end;

    if clipRec.IsEmpty then
    begin
      inherited; //something is broken
      Exit;
    end;

    clipRec2 := Rect(clipRec);
    tmpImg := fReader.TempImage;
    tmpImg.Clear(clipRec2);

    if filled then DrawFilled(tmpImg, drawInfo);
    if stroked then
    begin
      DrawStroke(tmpImg, drawInfo, true);
      DrawStroke(tmpImg, drawInfo, false);
    end;

    if Assigned(filterEl) then
      with TFilterElement(filterEl) do
        for i := 0 to fChilds.Count -1 do
          if (TElement(fChilds[i]) is TGaussianElement) then
            with TGaussianElement(fChilds[i]) do
              if IsValid(stdDev) then
                FastGaussianBlur(tmpImg, clipRec2,
                  Ceil(stdDev * arcScale) , blurQual);

    if DrawInfo.opacity < 255 then
      tmpImg.ReduceOpacity(DrawInfo.opacity, clipRec2);

    if Assigned(clipEl) then
      with TClipPathElement(clipEl) do
      begin
        EraseOutsidePaths(tmpImg, drawPathsC, fDrawInfo.fillRule, clipRec2);
        EraseOutsidePaths(tmpImg, drawPathsO, fDrawInfo.fillRule, clipRec2);
      end;

    img.CopyBlend(tmpImg, clipRec2, clipRec2, BlendToAlpha);
  end else
  begin
    if filled then
      DrawFilled(img, drawInfo);
    if stroked then
    begin
      DrawStroke(img, drawInfo, true);
      DrawStroke(img, drawInfo, false);
    end;
  end;

  //todo: enable "paint-order" to change filled/stroked/marker paint order
  if HasMarkers then DrawMarkers(img, drawInfo);

  inherited;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawMarkers(img: TImage32; drawInfo: TDrawInfo);
var
  i,j: integer;
  markerEl: TElement;
  strokePaths: TPathsD;
  pt1, pt2: TPointD;
begin
  drawInfo.matrix := fReader.fViewMatrix;

  //currently we'll assume markers are scaled according to strokewidth
  MatrixScale(drawInfo.matrix, fDrawInfo.strokeWidth);

  if Assigned(fDrawInfo.markerStart) and Assigned(drawPathsO) then
  begin
    markerEl := FindRefElement(fDrawInfo.markerStart);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        callerEl := Self;
        for i := 0 to High(callerEl.drawPathsO) do
        begin
          if Length(callerEl.drawPathsO[i]) < 2 then Continue;
          pt1 := callerEl.drawPathsO[i][0];
          pt2 := callerEl.drawPathsO[i][1];
          if autoStartReverse then
            SetSoloPoint(pt1, GetAngle(pt2, pt1)) else
            SetSoloPoint(pt1, GetAngle(pt1, pt2));
          Draw(img, drawInfo);
        end;
      end;
  end;

  if Assigned(fDrawInfo.markerMiddle) then
  begin
    markerEl := FindRefElement(fDrawInfo.markerMiddle);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        callerEl := Self;
        if Assigned(callerEl.drawPathsO) then
          strokePaths := callerEl.drawPathsO else
          strokePaths := callerEl.drawPathsC;
        for i := 0 to High(strokePaths) do
        begin
          j := High(strokePaths[i]);
          if j < 1 then Continue;
          SetPoints(Copy(strokePaths[i], 1, j -1));
          Draw(img, drawInfo);
        end;
      end;
  end;

  if Assigned(fDrawInfo.markerEnd) and Assigned(drawPathsO) then
  begin
    markerEl := FindRefElement(fDrawInfo.markerEnd);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        callerEl := Self;
        for i := 0 to High(callerEl.drawPathsO) do
        begin
          j := High(callerEl.drawPathsO[i]);
          if j < 1 then Continue;

          pt1 := callerEl.drawPathsO[i][j];
          pt2 := callerEl.drawPathsO[i][j-1];

          SetSoloPoint(pt1, GetAngle(pt2, pt1));
          Draw(img, drawInfo);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.GetDrawPaths(arcScale: double);
begin
  drawPathsO := nil; drawPathsC := nil;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawFilled(img: TImage32; const drawInfo: TDrawInfo);
var
  rec: TRect;
  refEl: TElement;
  paths: TPathsD;
begin
  if Assigned(drawPathsO) then
  begin
    paths := CopyPaths(drawPathsC);
    AppendPath(paths, drawPathsO);
  end else
  begin
    paths := drawPathsC;
    if not Assigned(paths) then Exit;
  end;

  if Assigned(drawInfo.fillEl) then
  begin
    refEl := FindRefElement(drawInfo.fillEl);
    if Assigned(refEl) and (refEl is TFillElement) then
    begin
      rec := GetBounds(paths);
      if refEl is TRadGradElement then
      begin
        with TRadGradElement(refEl) do
          PrepareRenderer(fReader.RadGradRenderer, drawInfo.matrix, rec);
        DrawPolygon(img, paths, drawInfo.fillRule, fReader.RadGradRenderer);
      end
      else if refEl is TLinGradElement then
      begin
        with TLinGradElement(refEl) do
          PrepareRenderer(fReader.LinGradRenderer, drawInfo.matrix, rec);
        DrawPolygon(img, paths, drawInfo.fillRule, fReader.LinGradRenderer);
      end;
    end;
  end else
  begin
    if drawInfo.fillColor = clInvalid then
      DrawPolygon(img, paths, drawInfo.fillRule, clBlack32) else
      DrawPolygon(img, paths, drawInfo.fillRule, drawInfo.fillColor);
  end;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawStroke(img: TImage32;
  const drawInfo: TDrawInfo; isClosed: Boolean);
var
  dashOffset: integer;
  dashArray: TArrayOfInteger;
  scale: Double;
  rec: TRect;
  strokePaths: TPathsD;
  refEl: TElement;
const
  endStyle: array [Boolean] of TEndStyle = (esRound, esPolygon);
  endStyle2: array [Boolean] of TEndStyle = (esButt, esPolygon);
begin
  if isClosed then
    strokePaths := drawPathsC else
    strokePaths := drawPathsO;
  if not Assigned(strokePaths) then Exit;

  scale := ExtractAvgScaleFromMatrix(drawInfo.matrix);

  if Length(drawInfo.dashArray) > 1 then
  begin
    dashOffset := Round(drawInfo.dashOffset * scale);
    dashArray := MakeDashArray(drawInfo.dashArray, scale);
    DrawDashedLine(img, strokePaths, dashArray, @dashOffset,
      drawInfo.strokeWidth * scale, drawInfo.strokeColor, endStyle2[isClosed])
  end
  else if Assigned(drawInfo.strokeEl) then
  begin
    refEl := FindRefElement(drawInfo.strokeEl);
    if not Assigned(refEl) then Exit;

    rec := GetBounds(strokePaths);
    if refEl is TRadGradElement then
    begin
      with TRadGradElement(refEl) do
        PrepareRenderer(fReader.RadGradRenderer, drawInfo.matrix, rec);
      DrawLine(img, strokePaths, drawInfo.strokeWidth * scale,
        fReader.RadGradRenderer, endStyle[isClosed], jsAuto, scale);
    end
    else if refEl is TLinGradElement then
    begin
      with TLinGradElement(refEl) do
        PrepareRenderer(fReader.LinGradRenderer, drawInfo.matrix, rec);
      DrawLine(img, strokePaths, drawInfo.strokeWidth * scale,
        fReader.LinGradRenderer, endStyle[isClosed], jsAuto, scale);
    end;
  end else
  begin
    DrawLine(img, strokePaths, drawInfo.strokeWidth * scale,
      drawInfo.strokeColor, endStyle[isClosed], jsAuto, scale);
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
  Result := SkipBlanksAndComma(fCurrent, fCurrentEnd) and
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

procedure TPathElement.Flatten(index: integer; arcScale: double;
  out path: TPathD; out isClosed: Boolean);
var
  i,j, pathLen, pathCap: integer;
  currPt, radii, pt2, pt3, pt4: TPointD;
  lastQCtrlPt, lastCCtrlPt: TPointD;
  arcFlag, sweepFlag: integer;
  angle, arc1, arc2, bezTolerance: double;
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
  bezTolerance := BezierTolerance/arcScale;
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
                  path2 := Arc(rec, arc2, arc1, arcScale);
                  path2 := ReversePath(path2);
                end else
                  path2 := Arc(rec, arc1, arc2, arcScale);
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
                path2 := FlattenQBezier(currPt, pt2, pt3, bezTolerance);
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
                path2 := FlattenQBezier(currPt, pt2, pt3, bezTolerance);
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
                path2 := FlattenCBezier(currPt, pt2, pt3, pt4, bezTolerance);
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
                path2 := FlattenCBezier(currPt, pt2, pt3, pt4, bezTolerance);
                AddPath(path2);
              end;
        end;
      end;
  end;
  SetLength(path, pathLen);
end;
//------------------------------------------------------------------------------

procedure TPathElement.GetDrawPaths(arcScale: double);
var
  i: integer;
  isClosed: Boolean;
  path: TPathD;
begin
  inherited;
  if arcScale <= 0 then arcScale := 1;

  for i := 0 to High(dpaths) do
  begin
    Flatten(i, arcScale, path, isClosed);
    if not Assigned(path) then Continue;
    if isClosed then
      AppendPath(drawPathsC, path) else
      AppendPath(drawPathsO, path);
  end;
end;

//------------------------------------------------------------------------------
// TPolyElement
//------------------------------------------------------------------------------

procedure TPolyElement.GetDrawPaths(arcScale: double);
begin
  inherited;
  if not Assigned(path) then Exit;
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

procedure TLineElement.GetDrawPaths(arcScale: double);
begin
  inherited;
  AppendPath(drawPathsO, path);
end;

//------------------------------------------------------------------------------
// TCircleElement
//------------------------------------------------------------------------------

procedure TCircleElement.GetDrawPaths(arcScale: double);
var
  rec: TRectD;
  path: TPathD;
begin
  inherited;
  if (radius <= 0) then Exit;
  with CenterPt do
    rec := RectD(X -radius, Y -radius, X +radius, Y +radius);
  path := Ellipse(rec, arcScale);
  AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TEllipseElement
//------------------------------------------------------------------------------

procedure TEllipseElement.GetDrawPaths(arcScale: double);
var
  rec: TRectD;
  path: TPathD;
begin
  inherited;
  if (radius.X <= 0) or (radius.Y <= 0) then Exit;
  with centerPt do
    rec := RectD(X -radius.X, Y -radius.Y, X +radius.X, Y +radius.Y);
  path := Ellipse(rec, arcScale);
  AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TRectElement
//------------------------------------------------------------------------------

procedure TRectElement.GetDrawPaths(arcScale: double);
var
  rec2: TRectD;
  path: TPathD;
begin
  inherited;
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
  AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TTextElement
//------------------------------------------------------------------------------

constructor TTextElement.Create(parent: TElement; hashName: Cardinal);
begin
  if parent is TTSpanElement then
    fontInfo := TTSpanElement(parent).fontInfo
  else if parent is TTextElement then
    fontInfo := TTextElement(parent).fontInfo
  else
    fontInfo := defaultFontInfo;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TTextElement.GetDrawPaths(arcScale: double);
begin
  //deferred to TSubtextElement.GetDrawPaths
end;
//------------------------------------------------------------------------------

procedure TTextElement.ResetTmpPt;
begin
  tmpPt.Y := pt.Y;
  tmpPt.X := InvalidD;
end;
//------------------------------------------------------------------------------

function TTextElement.AddSubtext(atext: PAnsiChar; len: integer): TElement;
begin
  Result := TSubtextElement.Create(self, 0);
  with TSubtextElement(Result) do
  begin
    fName := text;
    fNameLen := 0;
    text := atext;
    textLen := len;
    pt := InvalidPointD;
    fontInfo := Self.fontInfo;
  end;
  fChilds.add(Result);
end;
//------------------------------------------------------------------------------

procedure TTextElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
begin
  if (self is TSubtextElement) then
  begin
    inherited Draw(img, drawInfo)
  end else
  begin
    if not IsIdentityMatrix(fDrawInfo.matrix) then
      drawInfo.matrix := MatrixMultiply(drawInfo.matrix, fDrawInfo.matrix);
    for i := 0 to fChilds.Count -1 do
      with TElement(fChilds[i]) do
        if fDrawInfo.visible then Draw(img, drawInfo);
  end;
end;

//------------------------------------------------------------------------------
// TSubtextElement
//------------------------------------------------------------------------------

function TSubtextElement.GetTextElement: TTextElement;
var
  el: TElement;
begin
  el := fParent;
  while el is TSubtextElement do el := el.fParent;
  Result := TTextElement(el);
end;
//------------------------------------------------------------------------------

procedure TSubtextElement.GetDrawPaths(arcScale: double);
var
  tmpPt: TPointD;
  parentTextEl: TTextElement;
  s: string;
  i: integer;
  tmpX, offsetX, scale: double;
  mat: TMatrixD;
begin
  inherited;
  fReader.GetBestFont(fontInfo);
  if (fontInfo.size < 2) or not Assigned(fReader.fFontCache) then Exit;

  parentTextEl := GetTextElement;
  //if first subtext then reset X offset
  if (parentTextEl = fParent) and (self = fParent.fChilds[0]) then
    parentTextEl.ResetTmpPt;

  //get the text offset
  if IsValid(self.pt) then
    tmpPt := self.pt
  else if IsValid(parentTextEl.tmpPt.X) then
    tmpPt := parentTextEl.tmpPt
  else
    tmpPt := parentTextEl.Pt;
  parentTextEl.tmpPt := tmpPt;

  if textLen = 0 then
    Exit; //nb: only exit AFTER setting parentTextEl.tmpPt.

  //trim CRLFs and multiple spaces
  {$IFDEF UNICODE}
  s := UTF8ToUnicodeString(HtmlDecode(GetUtf8String(text, textLen)));
  {$ELSE}
  s := HtmlDecode(GetUtf8String(text, textLen));
  {$ENDIF}
  for i := 1 to Length(s) do
    if s[i] < #32 then s[i] := #32;
  i := Pos(#32#32, s);
  while i > 0 do
  begin
    Delete(s, i, 1);
    i := PosEx(#32#32, s, i);
  end;

  drawPathsC := fReader.fFontCache.GetTextGlyphs(0, 0, s, tmpX);
  //by not changing the fontCache.FontHeight, the quality of
  //small font render improves very significantly (though of course
  //this requires additional glyph scaling and offsetting).
  scale := fontInfo.size/fReader.fFontCache.FontHeight;
  mat := IdentityMatrix;
  MatrixScale(mat, scale);

  with parentTextEl.tmpPt do
    case fontInfo.align of
      taLeft:
        begin
          offsetX := X;
          X := X + tmpX * scale;
        end;
      taRight:
        begin
          offsetX := X - tmpX * scale;
          X := offsetX;
        end;
      else //taMiddle:
        begin
          offsetX := X - tmpX * scale /2;
          X := X + tmpX * scale/2;
        end;
    end;

  MatrixTranslate(mat, offsetX, tmpPt.Y);
  MatrixApply(mat, drawPathsC);
end;

//------------------------------------------------------------------------------
// TTSpanElement
//------------------------------------------------------------------------------

constructor TTSpanElement.Create(parent: TElement; hashName: Cardinal);
begin
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
  fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i,j: integer;
  di: TDrawInfo;
  scale: double;
  mat: TMatrixD;
begin
  if not Assigned(callerEl) then Exit;

  mat := drawInfo.matrix;
  if not viewbox.IsEmpty then
    MatrixScale(mat, width/viewbox.Width, height/viewbox.Height);

  if IsValid(ref) then
  begin
    scale := ExtractAvgScaleFromMatrix(mat);
    MatrixTranslate(mat, -ref.X * scale, -ref.Y * scale);
  end;

  MatrixRotate(mat, NullPointD, angle);

  //for each 'point' draw the marker
  for i := 0 to High(points) do
  begin
    di := defaultDrawInfo;
    di.matrix := mat;
    MatrixTranslate(di.matrix, points[i].X, points[i].Y);

    //for each marker shape (though there's very rarely more than one)
    for j := 0 to fChilds.Count -1 do
      if TElement(fChilds[j]) is TShapeElement then
        with TShapeElement(fChilds[j]) do
          Draw(img, di);
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

function TStyleElement.LoadContent: Boolean;
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
  Result := false;
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
  Result := HashCurrentWord(hash) and (fNameHash = hash);
  if Result then inc(fCurrent); //trailing '>'
end;

//------------------------------------------------------------------------------
// TSvgElement
//------------------------------------------------------------------------------

constructor TSvgElement.Create(reader: TSvgReader; hashName: Cardinal);
var
  svgStart: PAnsiChar;
begin
  inherited Create(nil, hashName);
  fReader    := reader;
  width       := InvalidD;
  height      := InvalidD;

  fCurrentEnd := fReader.fEndStream;
  fDrawInfo :=  defaultDrawInfo;

  if not fReader.GetSvgStart(svgStart) then Exit;
  fName := svgStart;
  inc(svgStart, 3);
  fCurrent := svgStart;
end;

//------------------------------------------------------------------------------
// TElement
//------------------------------------------------------------------------------

constructor TElement.Create(parent: TElement; hashName: Cardinal);
begin
{$IFDEF XPLAT_GENERICS}
  fChilds    := TList<TElement>.create;
{$ELSE}
  fChilds    := TList.Create;
{$ENDIF}
  fNameHash  := hashName;
  fStyleAttribIdx := -1;
  if not Assigned(parent) then Exit;

  self.fParent := parent;
  fReader      := parent.fReader;
  fCurrentEnd  := fReader.fEndStream;
  fDrawInfo    := parent.fDrawInfo;
  fDrawInfo.matrix := IdentityMatrix;
  fDrawInfo.clipPathEl := nil;
  fCurrent     := parent.fCurrent;
  if not SkipBlanks(fCurrent, fCurrentEnd) or
    not GetName(fName, fNameLen) then fName := nil;
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
  nameLen := 0;
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

function TElement.SetAttribValue(attrib: PAttrib): Boolean;
begin
  Result := fCurrent^ = '"';
  if not Result then Exit;
  inc(fCurrent);
  attrib.aValue := fCurrent;
  while (fCurrent < fCurrentEnd) and (fCurrent^ <> '"')  do inc(fCurrent);
  attrib.avalueLen := fCurrent - attrib.aValue;
  TrimBlanks(attrib.aValue, attrib.aValueLen);
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
      break;
    end;
    attrib := AddAttribute;
    GetName(attrib.aName, attrib.aNameLen);
    if GetNextChar(fCurrent, fCurrentEnd) <> '=' then Exit;
    SkipBlanks(fCurrent, fCurrentEnd);
    SetAttribValue(attrib);
    nextCurrent := fCurrent +1;
    //nb: ProcessAttrib() below sometimes adds additional attributes
    //which may cause the 'fAttribs' array to reallocate memory.
    //So the 'attrib' pointer isn't safe to use after calling ProcessAttrib().
    ProcessAttrib(attrib);
    fCurrent := nextCurrent;
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

function TElement.GetSvgElement: TSvgElement;
var
  el: TElement;
begin
  el := fParent;
  while Assigned(el) and not (el is TSvgElement) do el := el.fParent;
  if Assigned(el) then
    Result := TSvgElement(el) else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TElement.FindRefElement(refname: PAnsiChar): TElement;
var
  i, refNameLen: integer;
  c: PAnsiChar;
begin
  c := refname;
  refNameLen := GetNameLength(c);

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
begin
  Result := nil;
  savedCurrent := fCurrent;
  if not GetName(name, nameLen) then Exit;
  fCurrent := savedCurrent;
  hashedName := GetHashedName(name, nameLen);

  elClass := HashToElementClass(hashedName);
  Result := elClass.Create(self, hashedName);

  if not Assigned(Result.fName) or
    not Result.LoadAttributes then
  begin
    FreeAndNil(Result);
    Exit;
  end;

  //parse 'style' AFTER other attributes have been set
  //since style values have precedence.
  with Result do
    if fStyleAttribIdx >= 0 then
      with fAttribs[fStyleAttribIdx] do
        ParseStyle(aValue, aValueLen);

  fChilds.Add(Result);
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

function TElement.LoadContent: Boolean;
var
  c: PAnsiChar;
  el: TElement;
  hash: Cardinal;
begin
  Result := false;
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
            ((fCurrent -1)^ = '-') and
            ((fCurrent -2)^ = '-') then
          begin
            inc(fCurrent);
            break; //break out of inner comment loop
          end;
          inc(fCurrent);
        end;
      end
      else if fCurrent^ = '/' then
      begin
        //this should be the element close
        inc(fCurrent);
        Result := HashCurrentWord(hash) and (fNameHash = hash);
        if Result then inc(fCurrent);
        Exit;
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
      break;
    end
    else if (self is TTextElement) then
    begin
      while (fCurrent -1)^ = #32 do dec(fCurrent);
      c := fCurrent;
      while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
      TTextElement(self).AddSubtext(c, fCurrent - c);
    end else
    begin
      //skip unknown content
      while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
  arcScale: double;
  clipEl: TElement;
  tmpImg: TImage32;
  clipRec: TRect;
begin
  if fChilds.Count = 0 then Exit;

  if not IsIdentityMatrix(fDrawInfo.matrix) then
    drawInfo.matrix := MatrixMultiply(drawInfo.matrix, fDrawInfo.matrix);

  if Assigned(fDrawInfo.clipPathEl) then
    clipEl := FindRefElement(fDrawInfo.clipPathEl) else
    clipEl := nil;

  if Assigned(clipEl) then
  begin
    with TClipPathElement(clipEl) do
    begin
      arcScale := ExtractAvgScaleFromMatrix(drawInfo.matrix);
      GetDrawPaths(arcScale);
      MatrixApply(drawInfo.matrix, drawPathsC);
      MatrixApply(drawInfo.matrix, drawPathsO);
      clipRec := Image32_Vector.UnionRect(
        GetBounds(drawPathsC), GetBounds(drawPathsO));
    end;
    if IsEmptyRect(clipRec) then Exit;

    tmpImg := TImage32.Create(img.Width, img.Height);
    try
      for i := 0 to fChilds.Count -1 do
        with TElement(fChilds[i]) do
          if fDrawInfo.visible then Draw(tmpImg, drawInfo);


      with TClipPathElement(clipEl) do
      begin
        EraseOutsidePaths(tmpImg, drawPathsC, fDrawInfo.fillRule, clipRec);
        EraseOutsidePaths(tmpImg, drawPathsO, fDrawInfo.fillRule, clipRec);
      end;
      img.CopyBlend(tmpImg, clipRec, clipRec, BlendToAlpha);

    finally
      tmpImg.Free;
    end;

  end else
  begin
    for i := 0 to fChilds.Count -1 do
      with TElement(fChilds[i]) do
        if fDrawInfo.visible then Draw(img, drawInfo);
  end;
end;

//------------------------------------------------------------------------------
// TSvgReader
//------------------------------------------------------------------------------

constructor TSvgReader.Create;
begin
  fMemStream        := TMemoryStream.Create;
  fIdList           := TStringList.Create;
  //fIdList.Duplicates := dupError; // I'm unsure how best to manage this, but
  fIdList.Duplicates := dupIgnore;  // pro tem, duplicates will be ignored.
  fIdList.Sorted    := True;

  fClassStylesList  := TStrAnsiList.Create;
  fClassStylesList.Duplicates := dupIgnore;
  fClassStylesList.Sorted := True;

  fLinGradRenderer  := TLinearGradientRenderer.Create;
  fRadGradRenderer  := TSvgRadialGradientRenderer.Create;

{$IFDEF XPLAT_GENERICS}
    fFontList       := TList<TFontReader>.create;
{$ELSE}
    fFontList       := TList.create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TSvgReader.Destroy;
var
  i: integer;
begin
  Clear;
  fMemStream.Free;
  fIdList.Free;
  fClassStylesList.Free;
  for i := 0 to fFontList.Count -1 do
    TFontReader(fFontList[i]).Free;
  fFontList.Free;

  fLinGradRenderer.Free;
  fRadGradRenderer.Free;
  FreeAndNil(fFontCache);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.Clear;
begin
  FreeAndNil(fRootElement);
  fMemStream.clear;
  fIdList.Clear;
  fClassStylesList.Clear;
  fEndStream := nil;
  fLinGradRenderer.Clear;
  fRadGradRenderer.Clear;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function TSvgReader.AddFont(const fontName: string): Boolean;
var
  fr: TFontReader;
begin
  try
    fr := TFontReader.Create(fontName);
  except;
    Result := false;
    Exit;
  end;
  Result := true;
  fFontList.Add(fr);
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TSvgReader.AddFontFromResource(const resName: string;
  resType: PChar): Boolean;
var
  fr: TFontReader;
begin
  try
    fr := TFontReader.CreateFromResource(resName, resType);
  except;
    Result := false;
    Exit;
  end;
  Result := true;
  fFontList.Add(fr);
end;
//------------------------------------------------------------------------------

procedure TSvgReader.DrawImage(img: TImage32; scaleToImage: Boolean);
var
  sx, sy: double;
  di: TDrawInfo;
const
  endStyle: array[boolean] of TEndStyle = (esPolygon, esRound);
begin
  if not Assigned(fRootElement) then Exit;

    with fRootElement do
    begin
      di := emptyDrawInfo;
      MatrixTranslate(di.matrix, -viewbox.Left, -viewbox.Top);

      if viewbox.IsEmpty then
      begin
        if not IsValid(width) or not IsValid(height) then Exit;
        fRootElement.viewbox := RectWH(0, 0, width, height);
      end
      else if IsValid(width) or IsValid(height) then
      begin
        if IsValid(width) then
          sx := width/viewbox.Width else
          sx := height/viewbox.Height;
        if IsValid(height) then
          sy := height/viewbox.Height else
          sy := sx;
        //assume default preserveAspectRatio - ie xMidYMid.
        sx := (sx + sy) * 0.5; sy := sx;
        MatrixScale(di.matrix, sx, sy);
        viewbox.Width := viewbox.Width * sx;
        viewbox.Height := viewbox.Height * sy;
      end;

      //the rootElement's matrix may have been modified by
      //the svg element's height/width and viewbox settings
      if scaleToImage and not img.IsEmpty then
      begin
        sx := img.width / (viewbox.Width);
        sy := img.height / (viewbox.Height);
        if sy < sx then sx := sy;
        MatrixScale(di.matrix, sx);

        img.SetSize(Round(viewbox.Width * sx), Round(viewbox.Height * sx));
      end else
        img.SetSize(Round(viewbox.Width), Round(viewbox.Height));
    end;

  if fBackgroundColor <> clNone32 then
    img.Clear(fBackgroundColor);

  fViewMatrix := di.matrix;
  fTempImage := TImage32.Create(img.Width, img.Height);
  try
    fRootElement.Draw(img, di);
  finally
    fTempImage.Free;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromFile(const filename: string): Boolean;
var
  Stream: TStream;
begin
  Result := FileExists(filename);
  if not Result then Exit;

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

  fRootElement := TSvgElement.Create(self, hSvg);
  with fRootElement do
  begin
    result := fRootElement.LoadAttributes;
    if not Result then Exit;

    if viewbox.IsEmpty then
    begin
      if not IsValid(width) or not IsValid(height) then Exit;
      viewbox := RectWH(0, 0, width, height);
    end;
    Result := not fHasContent or LoadContent;
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

function MacStylesToFontStyles(macStyles: TMacStyles): TFontSyles;
begin
  Result := [];
  if msBold in macStyles then Include(Result, fsBold);
    if msItalic in macStyles then Include(Result, fsItalic);
end;
//------------------------------------------------------------------------------

procedure TSvgReader.GetBestFont(const svgFontInfo: TFontInfo);
var
  i, bestIdx, currMask, bestMask: integer;
  fontStyles: TFontSyles;

  function GetMask(fr: TFontReader): integer;
  begin
    fontStyles := MacStylesToFontStyles(fr.FontInfo.macStyles);
    Result := Abs(Integer(Byte(fontStyles)) -
      Integer(Byte(svgFontInfo.styles))) shl 8;
    Result := Result or Abs(Ord(svgFontInfo.family) - Ord(fr.FontFamily));
  end;

begin
  if fFontList.Count = 0 then Exit;
  bestIdx := 0;
  bestMask := GetMask(TFontReader(fFontList[0]));

  for i := 1 to fFontList.Count -1 do
  begin
    currMask := GetMask(TFontReader(fFontList[i]));
    if currMask >= bestMask then Continue;
    bestMask := currMask;
    bestIdx := i;
  end;

  if not Assigned(fFontCache) then
    fFontCache :=
      TGlyphCache.Create(TFontReader(fFontList[bestIdx]), defaultFontHeight)
  else
    fFontCache.FontReader := TFontReader(fFontList[bestIdx]);
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

procedure RegisterAttribute(hashedName: Cardinal; func: TAttribFunc);
begin
  AttribFuncList.AddObject(InttoHex(hashedName, 8), @func) ;
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
  el: TElement;
begin
  el := attrib.aOwnerEl;
  if el is TFillElement then
    TFillElement(el).refEl := GetRefName(attrib.aValue)
  else if el is TUseElement then
    TUseElement(el).refEl := GetRefName(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure Class_Attrib(attrib: PAttrib);
begin
  TElement(attrib.aOwnerEl).ParseClassAttrib(attrib);
end;
//------------------------------------------------------------------------------

procedure ClipPath_Attrib(attrib: PAttrib);
begin
  attrib.aOwnerEl.fDrawInfo.clipPathEl := GetRefName(attrib.aValue);
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
    attrib.aOwnerEl.fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

procedure Font_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TTextElement then
    with TTextElement(attrib.aOwnerEl) do
      AttribToFontInfo(attrib, fontInfo)
  else if attrib.aOwnerEl is TSubtextElement then
    with TSubtextElement(attrib.aOwnerEl) do
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
        hSerif      : fontInfo.family := ttfSerif;
        hMonospace  : fontInfo.family := ttfMonospace;
        else          fontInfo.family := ttfSansSerif;
      end;
    end;
  end;

begin
  if attrib.aOwnerEl is TTextElement then
    GetFamily(TTextElement(attrib.aOwnerEl).fontInfo)
  else if attrib.aOwnerEl is TSubtextElement then
    GetFamily(TSubtextElement(attrib.aOwnerEl).fontInfo);
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
  else if (attrib.aOwnerEl is TSubtextElement) and
    GetNum(attrib.aValue, attrib.aValue + attrib.aValueLen, false, num) then
      TSubtextElement(attrib.aOwnerEl).fontInfo.size := num
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
  else if attrib.aOwnerEl is TSubtextElement then
    GetStyle(TSubtextElement(attrib.aOwnerEl).fontInfo);
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
  else if attrib.aOwnerEl is TSubtextElement then
    GetWeight(TSubtextElement(attrib.aOwnerEl).fontInfo);
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
  else if attrib.aOwnerEl is TSubtextElement then
    GetAlign(TSubtextElement(attrib.aOwnerEl).fontInfo);
end;
//------------------------------------------------------------------------------

procedure MarkerStart_Attrib(attrib: PAttrib);
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  attrib.aOwnerEl.fDrawInfo.markerStart := GetRefName(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure MarkerMiddle_Attrib(attrib: PAttrib);
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  attrib.aOwnerEl.fDrawInfo.markerMiddle := GetRefName(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure MarkerEnd_Attrib(attrib: PAttrib);
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  attrib.aOwnerEl.fDrawInfo.markerEnd := GetRefName(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure Filter_Attrib(attrib: PAttrib);
begin
  if (attrib.aOwnerEl is TShapeElement) then
    attrib.aOwnerEl.fDrawInfo.filterEl := GetRefName(attrib.aValue);
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

procedure Orient_Attrib(attrib: PAttrib);
begin
  if (attrib.aOwnerEl is TMarkerElement) and
    (GetHashedName(attrib.aValue, attrib.aValueLen) =
      hauto_045_start_045_reverse) then
        TMarkerElement(attrib.aOwnerEl).autoStartReverse := true;
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
  with attrib.aOwnerEl do
    case fNameHash of
      hFlowRegion, hFlowRoot: fDrawInfo.fillColor := clNone32;
      else fStyleAttribIdx := High(fAttribs);
    end;
end;
//------------------------------------------------------------------------------

procedure Transform_Attrib(attrib: PAttrib);
begin
  attrib.aOwnerEl.ParseTransform(attrib.aValue, attrib.aValueLen);
end;
//------------------------------------------------------------------------------

procedure Viewbox_Attrib(attrib: PAttrib);

  function LoadViewbox: TRectWH;
  var
    current, currentEnd: PAnsiChar;
  begin
    current := attrib.aValue;
    currentEnd := current + attrib.aValueLen;
    with Result do
    if not GetNum(current, currentEnd, false, Left) or
      not GetNum(current, currentEnd, true, Top) or
      not GetNum(current, currentEnd, true, Width) or
      not GetNum(current, currentEnd, true, Height) then
        Result := RectWH(0,0,0,0);
  end;

begin
  if attrib.aOwnerEl is TSvgElement then
    TSvgElement(attrib.aOwnerEl).viewbox := LoadViewbox
  else if attrib.aOwnerEl is TMarkerElement then
    TMarkerElement(attrib.aOwnerEl).viewbox := LoadViewbox;
end;
//------------------------------------------------------------------------------

procedure Height_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  svgElement: TSvgElement;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  if mu = muPercent then
  begin
    svgElement := attrib.aOwnerEl.GetSvgElement;
    if not Assigned(svgElement) then Exit;
    val := val * svgElement.viewbox.Height;
  end;

  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do recWH.Height := val;
    hSvg:
      with TSvgElement(attrib.aOwnerEl) do height := val;
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do height := val;
    hFilter:
      with TFilterElement(attrib.aOwnerEl) do filterRecWH.Height := val;
    hUse:
      with TUseElement(attrib.aOwnerEl) do recWH.Height := val;
  end;
end;
//------------------------------------------------------------------------------

procedure Width_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  svgElement: TSvgElement;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  if mu = muPercent then
  begin
    svgElement := attrib.aOwnerEl.GetSvgElement;
    if not Assigned(svgElement) then Exit;
    val := val * svgElement.viewbox.Width;
  end;

  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do recWH.Width := val;
    hSvg:
      with TSvgElement(attrib.aOwnerEl)  do width := val;
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do width := val;
    hFilter:
      with TFilterElement(attrib.aOwnerEl) do filterRecWH.Width := val;
    hUse:
      with TUseElement(attrib.aOwnerEl) do recWH.Width := val;
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
        AttribToFloat(attrib, recWH.Left);
    hFilter:
      with TFilterElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, filterRecWH.Left);
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
        AttribToFloat(attrib, recWH.Top);
    hFilter:
      with TFilterElement(attrib.aOwnerEl) do
        AttribToFloat(attrib, filterRecWH.Top);
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
  RegisterAttribute(hOrient,                Orient_Attrib);
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
  defaultFontHeight := DPIAware(12);
  MakeLowerCaseTable;
  MakeAttribFuncList;
  MakeColorConstList;

finalization
  AttribFuncList.Free;
  ColorConstList.Free;
end.
