unit Image32_SVG_Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.23                                                            *
* Date      :  18 April 2021                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2021                                         *
*                                                                              *
* Purpose   :  Read simple SVG files                                           *
*                                                                              *
* Note: this unit is just an early beta release, there's still much to do.     *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*******************************************************************************)

//immediate todos:
//clipPath, lineCap, lineJoin

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types, Math,
  Image32, Image32_Vector, Image32_Transform, Image32_Draw;

type
  TGradientElement = class;

  PDrawInfo = ^TDrawInfo;
  TDrawInfo = record
  public
    fillColor   : TColor32;
    fillRule    : TFillRule;
    gradElement : TGradientElement;
    strokeColor : TColor32;
    strokeWidth : double;
    //lineCap  : TEndStyle;     //stroke-linecap
    //lineJoin : TJoinStyle;    //stroke-linejoin
    //miterLim : double;        //stroke-miterlimit
    matrix      : TMatrixD;
    inDefs      : Boolean;
  end;

  TAnsiStringRec = record
    str: AnsiString;
    obj: TObject;
  end;
  PAnsiStringRec = ^TAnsiStringRec;

  TStrStrList = class(TStringList)
  public
    function Add(const str1: string; const str2: AnsiString): integer; reintroduce;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    destructor Destroy; override;
  end;

  TElement = class;

  PAttrib = ^TAttrib;
  TAttrib = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    ownerEl  : TElement;
    aname     : PAnsiChar;
    nameLen  : integer;
    value    : PAnsiChar;
    valueLen : integer;
    function GetName: AnsiString;
    function GetValue: AnsiString;
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
    fNameHash       : Cardinal;
    fAttribs        : TArrayOfAttrib;
    //nb: non-shape elements may still contain
    //drawing info (transforms etc)
    fDrawInfo       : TDrawInfo;
    fCurrent        : PAnsiChar;
    fCurrentEnd     : PAnsiChar;
    function AddAttribute: PAttrib;
    function LoadChild: TElement;
    function GetHashedCurrentWord(out hash: Cardinal): Boolean;
    function GetElemOrAttribName(out name: PAnsiChar;
      out nameLen: integer): Boolean;
    function GetAttribValue(out value: PAnsiChar; out valueLen: integer): Boolean;
    function FindRefElement(refname: PAnsiChar; refNameLen: integer): TElement;
    function FindAttribute(const attribName: AnsiString): PAttrib; overload;
    function FindAttribute(hash: Cardinal): PAttrib; overload;
    procedure ProcessAttrib(const attrib: TAttrib);
    function PeekChar: AnsiChar;
    procedure ParseClassAttrib(const classAttrib: TAttrib);
    procedure ParseStyle(style: AnsiString);
    procedure ParseTransform(transform: AnsiString);
  protected
    surrogate: TElement;
    function LoadAttributes: Boolean; virtual;
    procedure LoadContent; virtual;
    procedure AssignTo(var other: TElement); virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); virtual;
    destructor  Destroy; override;
  end;

  TUseElement = class(TElement)
  protected
    procedure DoHrefClone(refEl: TElement);
  end;

  TShapeElement = class(TElement)
  protected
    isClosed    : Boolean;
    paths       : TPathsD;
    function GetVal(out val: double): Boolean;
    function GetDrawInfo: TDrawInfo;
    function GetPathCount: integer; virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
    function GetPath(index: integer; out path: TPathD;
      out isClosed: Boolean): Boolean;  virtual;
    property DrawInfo: TDrawInfo read GetDrawInfo;
    property PathCount: integer read GetPathCount;
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
    procedure Flatten(index: integer;  out path: TPathD; out isClosed: Boolean);
  protected
    procedure ParseD(const attrib: TAttrib);
    function GetPathCount: integer; override;
  public
    function GetPath(index: integer; out path: TPathD;
      out isClosed: Boolean): Boolean;  override;
  end;

  TPolyElement = class(TShapeElement) //polyline or polygon
  protected
    procedure ParsePoints(const attrib: TAttrib);
  public
    constructor Create(parent: TElement; hashName: Cardinal; isClosed: Boolean); reintroduce;
  end;

  TLineElement = class(TShapeElement)
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TCircleElement = class(TShapeElement)
  protected
    centerPt  : TPointD;
    radius    : double;
    function LoadAttributes: Boolean; override;
end;

  TEllipseElement = class(TShapeElement)
  protected
    centerPt  : TPointD;
    radius    : TPointD;
    function LoadAttributes: Boolean; override;
  end;

  TRectElement = class(TShapeElement)
  protected
    rec     : TRectWH;
    radius  : TPointD;
    function LoadAttributes: Boolean; override;
  end;

  TStyleElement = class(TElement)
  protected
    procedure LoadContent; override;
  end;

  TSvgColorStop = record
    offset: double;
    color: TColor32;
  end;
  TSvgColorStops = array of TSvgColorStop;

  TGradientElement = class(TElement)
  private
    fStops: TSvgColorStops;
    fGradientUnits: Cardinal;
  protected
    procedure LoadContent; override;
    procedure AddStop(color: TColor32; offset: double);
    procedure AssignTo(var other: TElement); override;
  end;

  TRadGradElement = class(TGradientElement)
  end;

  TLinGradElement = class(TGradientElement)
  protected
    startPt, endPt: TPointD;
    procedure AssignTo(var other: TElement); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TGradStopElement = class(TElement)
  protected
    offset: double;
    color: TColor32;
  end;

  TClipPathElement = class(TElement)
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TAttribFunc = procedure (const attrib: TAttrib);

  TSvgReader = class
  private
    fMemStream        : TMemoryStream;
    fEndStream        : PAnsiChar;
    fIdList           : TStringList;
    fShapesList       : TList;
    fClassesList      : TStrStrList;
    fLinGradRenderer  : TLinearGradientRenderer;
    fRadGradRenderer  : TSvgRadialGradientRenderer;
    fViewbox          : TRectD;
    fRootElement      : TElement;
    function GetSvgStart(out svgStart: PAnsiChar): Boolean;
    procedure GetViewBox;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DrawImage(img: TImage32; scaleToImage: Boolean);
    function LoadFromStream(stream: TStream): Boolean;
    function LoadFromFile(const filename: string): Boolean;
  end;

implementation

type
  TColorConst = record
    ColorName : string; ColorValue: Cardinal;
  end;

const
  buffSize = 32;

  defaultDrawInfo: TDrawInfo =
    (fillColor: clBlack32; fillRule: frNonZero; gradElement: nil;
    strokeColor: clNone32; strokeWidth: 1.0;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); inDefs: false);

  {$I Image32_SVG_ColorConsts.inc}
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

function TStrStrList.Add(const str1: string; const str2: AnsiString): integer;
var
  i,j: integer;
  sr: PAnsiStringRec;
  ps: PAnsiString;
begin
  new(sr);
  sr.str := str2;
  sr.obj := nil;
  i := IndexOf(str1);
  if (i >= 0) then
  begin
    ps := @PAnsiStringRec(Objects[i]).str;
    j := Length(ps^);
    if ps^[j] <> ';' then
      ps^ := ps^ + ';' + sr.str else
      ps^ := ps^ + sr.str;
    Dispose(sr);
    Result := i;
  end
  else
    Result := inherited AddObject(str1, TObject(sr));
end;
//------------------------------------------------------------------------------

procedure TStrStrList.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    Dispose(PAnsiStringRec(Objects[i]));
  inherited Clear;
end;
//------------------------------------------------------------------------------

procedure TStrStrList.Delete(Index: Integer);
begin
  if (index < 0) or (index >= Count) then
    Error(@rsListBoundsError, index);
  Dispose(PAnsiStringRec(Objects[index]));
  inherited Delete(Index);
end;
//------------------------------------------------------------------------------

destructor TStrStrList.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function SkipBlanks(var current: PAnsiChar; stop: PAnsiChar): Boolean;
begin
  while (current < stop) and (current^ <= #32) do inc(current);
  Result := (current < stop);
end;
//------------------------------------------------------------------------------

function SkipBlanksIgnoreComma(var current: PAnsiChar; stop: PAnsiChar): Boolean;
begin
  Result := SkipBlanks(current, stop);
  if Result and (current^ = ',') then
  begin
    inc(current);
    Result := SkipBlanks(current, stop);
  end;
end;
//------------------------------------------------------------------------------

function GetChar(var current: PAnsiChar; stop: PAnsiChar; out c: AnsiChar): Boolean;
begin
  Result := SkipBlanks(current, stop);
  if not Result then Exit;
  c := current^;
  inc(current);
end;
//------------------------------------------------------------------------------

function AnsiName(pName: PAnsiChar; len: integer): AnsiString;
begin
  SetLength(Result, len);
  Move(pName^, Result[1], len);
end;
//--------------------------------------------------------------------------

function AnsiLowercase(pName: PAnsiChar; len: integer): AnsiString;
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

function GetHashedName(c: PAnsiChar; length: integer): cardinal;
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

function IsAlpha(c: AnsiChar): Boolean;
begin
  if c >= 'a' then dec(c, $20);
  Result := (c >= 'A') and (c <= 'Z');
end;
//------------------------------------------------------------------------------

function GetElemOrAttribNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
var
  startC: PAnsiChar;
const
  validChars =  ['0'..'9','A'..'Z','a'..'z','_',':','-'];
begin
  Result := 0;
  if not IsAlpha(c^) then Exit;
  startC := c; inc(c); 
  while (c < endC) and CharInSet(LowerCaseTable[c^], validChars) do inc(c);
  Result := c - startC;
end;
//------------------------------------------------------------------------------

function GetStyleNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
var
  startC: PAnsiChar;
const
  validChars =  ['0'..'9','A'..'Z','a'..'z','-'];
begin
  Result := 0;
  if not IsAlpha(c^) then Exit;
  startC := c; inc(c); 
  while (c < endC) and CharInSet(LowerCaseTable[c^], validChars) do inc(c);
  Result := c - startC;
end;
//------------------------------------------------------------------------------

function CompareNames(p: PAnsiChar; pLen: integer;
  p2: PAnsiChar; p2Len: integer): Boolean; overload;
var
  i: integer;
begin
  result := (p2Len = pLen);
  if not result then Exit;
  for i := 0 to pLen -1 do
    if LowerCaseTable[p^] = LowerCaseTable[p2^] then
    begin
      inc(p); inc(p2);
    end else
    begin
      Result := false;
      Exit;
    end;
end;
//------------------------------------------------------------------------------

//avoid AnsiString as much as possible as it really slow things down
function CompareNames(const text: AnsiString;
  p: PAnsiChar; pLen: integer): Boolean; overload;
begin
  result := CompareNames(PAnsiChar(text), Length(text), p, pLen);
end;
//------------------------------------------------------------------------------

function Color32ToHtml(clr: Cardinal): string;
begin
  result := format('#%.6x', [clr and $FFFFFF]);
end;
//------------------------------------------------------------------------------

function TextToHtml(const s: string): string;
var
  i, len: integer;
begin
  result := s;
  len := length(s);
  for i := len downto 1 do
  begin
    case result[i] of
      '&':
        begin
          len := len + 4;
          setlength(result, len);
          move(result[i+1], result[i+5], (len - i - 4) * sizeof(Char));
          move('amp;', result[i+1], 4 * sizeof(Char));
        end;
      '<':
        begin
          len := len + 3;
          setlength(result, len);
          move(result[i+1], result[i+4], (len - i - 3) * sizeof(Char));
          move('&lt;', result[i], 4 * sizeof(Char));
        end;
      '>':
        begin
          len := len + 3;
          setlength(result, len);
          move(result[i+1], result[i+4], (len - i - 3) * sizeof(Char));
          move('&lt;', result[i], 4 * sizeof(Char));
        end;
      '''':
        begin
          len := len + 5;
          setlength(result, len);
          move(result[i+1], result[i+6], (len - i - 5) * sizeof(Char));
          move('&apos;', result[i], 6 * sizeof(Char));
        end;
      '"':
        begin
          len := len + 5;
          setlength(result, len);
          move(result[i+1], result[i+6], (len - i - 5) * sizeof(Char));
          move('&quot;', result[i], 6 * sizeof(Char));
        end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TrigClamp(val: double): double;
begin
  if val < -1 then Result := -1
  else if val > 1 then Result := 1
  else Result := val;
end;
//------------------------------------------------------------------------------

function  Radian2(vx, vy: double): double;
begin
  Result := ArcCos( TrigClamp(vx / Sqrt( vx * vx + vy * vy)) );
  if( vy < 0.0 ) then Result := -Result;
end;
//------------------------------------------------------------------------------

function  Radian4(ux, uy, vx, vy: double): double;
var
  dp, md: double;
begin
  dp := ux * vx + uy * vy;
  md := Sqrt( ( ux * ux + uy * uy ) * ( vx * vx + vy * vy ) );
    Result := ArcCos( TrigClamp(dp / md) );
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

function AttribToColor32(const attrib: TAttrib; var color: TColor32): Boolean;
var
  i: integer;
  j: Cardinal;
  c: TColor32;
  alpha: Byte;
  p1, p2, p3: PAnsiChar;
  refElement: TElement;
begin
  Result := false;
  if (attrib.valueLen < 3) then Exit;
  alpha := color shr 24;

  if (attrib.value[0] = 'u') then
  begin
    //todo - currently assumes only fills have links
    if (attrib.valueLen < 6) or
    (LowerCaseTable[attrib.value[1]] <> 'r') or
    (LowerCaseTable[attrib.value[2]] <> 'l') or
    (attrib.value[3] <> '(') then Exit;
    i := 4;
    while (attrib.value[i] <= #32) do inc(i);
    if (attrib.value[i] <> '#') then Exit;

    p3 := attrib.value + attrib.valueLen;
    p1 := attrib.value + i +1;
    p2 := p1;
    while (p2 < p3) and (p2^ <> ')') do inc(p2);

    refElement := attrib.ownerEl.FindRefElement(p1, p2 - p1);
    if not assigned(refElement) then Exit;

    with attrib.ownerEl do
      if refElement is TGradientElement then
        fDrawInfo.gradElement := TGradientElement(refElement);
  end
  else if (attrib.value[0] = '#') then
  begin
    if (attrib.valueLen = 7) then
    begin
      c := $0;
      for i := 1 to 6 do
        c := c shl 4 + HexByteToInt(attrib.value[i]);
      c := c or $FF000000;
    end
    else if (attrib.valueLen = 4) then
    begin
      c := $0;
      for i := 1 to 3 do
      begin
        j := HexByteToInt(attrib.value[i]);
        c := c shl 4 + j;
        c := c shl 4 + j;
      end;
      c := c or $FF000000;
    end
    else
      Exit;
    color :=  c;
  end else
  begin
    i := ColorConstList.IndexOf(string(attrib.GetValue));
    if i < 0 then Exit;
    color := Cardinal(ColorConstList.Objects[i]);
  end;

  //and just in case the opacity has been set before the color
  if (alpha > 0) and (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
  Result := true;
end;
//------------------------------------------------------------------------------

function AttribToFillRule(const attrib: TAttrib): TFillRule;
begin
  Result := frNonZero; //default
  if (attrib.valueLen = 0) then Exit;
  if LowerCaseTable[attrib.value[0]] = 'e' then
    Result := frEvenOdd;
end;
//------------------------------------------------------------------------------

function GetWord(var current: PAnsiChar; stop: PAnsiChar; out word: AnsiString): Boolean;
var
  i: integer;
  c: PAnsiChar;
begin
  Result := SkipBlanks(current, stop);
  if not Result then Exit;

  i := 0; c := current;
  while (current < stop) and
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

function GetValueLen(var current: PAnsiChar; stop: PAnsiChar): integer;
var
  c: PAnsiChar;
begin
  Result := 0;
  if not SkipBlanks(current, stop) then Exit;
  c := current;
  while (current < stop) and
    CharInSet(current^, ['.','-','#','0'..'9', 'A'..'Z', 'a'..'z']) do 
      inc(current);
  Result := current - c;
end;
//------------------------------------------------------------------------------

function NumPending(current, stop: PAnsiChar; ignoreComma: Boolean): Boolean;
begin
  Result := false;

  //skip white space +/- single comma
  if ignoreComma then
  begin
    while (current < stop) and (current^ <= #32) do inc(current);
    if (current^ = ',') then inc(current);
  end;
  while (current < stop) and (current^ <= ' ') do inc(current);
  if (current = stop) then Exit;

  if (current^ = '-') then inc(current);
  if (current^ = '.') then inc(current);
  Result := (current < stop) and (current^ >= '0') and (current^ <= '9');
end;
//------------------------------------------------------------------------------

function GetNum(var current: PAnsiChar; stop: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean;
var
  decPos,exp: integer;
  isNeg: Boolean;
begin
  Result := false;

  //skip white space +/- single comma
  if skipComma then
  begin
    while (current < stop) and (current^ <= #32) do inc(current);
    if (current^ = ',') then inc(current);
  end;
  while (current < stop) and (current^ <= #32) do inc(current);
  if (current = stop) then Exit;

  decPos := -1; exp := 0;
  isNeg := current^ = '-';
  if isNeg then inc(current);

  val := 0;
  while current < stop do
  begin
    if Ord(current^) = Ord(FormatSettings.DecimalSeparator) then
    begin
      if decPos >= 0 then break;
      decPos := 0;
    end
    else if (LowerCaseTable[current^] = 'e') then
    begin
      if (current +1)^ = '-' then
      begin
        exp := -1;
        inc(current);
      end else
        exp := 1;
    end
    else if (current^ < '0') or (current^ > '9') then
      break
    else if (exp <> 0) then
      exp := exp * 10 + (Ord(current^) - Ord('0'))
    else
    begin
      val := val *10 + Ord(current^) - Ord('0');
      if decPos >= 0 then inc(decPos);
    end;
    inc(current);
  end;
  if decPos > 0 then val := Power10(val, -decPos);
  if isNeg then val := -val;
  if exp <> 0 then val := Power10(val, exp);
  //convert percentages to fractions
  if (current^ = '%') then
  begin
    val := val *0.01;
    inc(current);
  end;
  //ignore 'px'
  if (current^ = 'p') and  ((current+1)^ = 'x') then
    inc(current, 2);

  Result := true;
end;
//------------------------------------------------------------------------------

function AttribToFloat(const attrib: TAttrib; var value: double): Boolean;
var
  c: PAnsiChar;
begin
  c := attrib.value;
  Result := GetNum(c, attrib.value + attrib.valueLen, false, value);
end;
//------------------------------------------------------------------------------

procedure AttribToOpacity(const attrib: TAttrib; var color: TColor32);
var
  opacity: double;
begin
  if color = clNone32 then Exit;
  if not AttribToFloat(attrib, opacity) then Exit;
  if (opacity < 0) or (opacity > 1) then Exit;
  color := (color and $FFFFFF) or (Round(255 * opacity) shl 24);
end;
//------------------------------------------------------------------------------

function FloatToAnsi(val: double): AnsiString;
var
  i, len: integer;
  s: string;
begin
  s := FloatToStr(val);
  len := Length(s);
  i := 2;
  while (i < len) and (s[i] <> FormatSettings.DecimalSeparator) do inc(i);
  if i < len then
  begin
    inc(i);
    while (i <= len) and (s[i] <> '0') do inc(i);
    len := i -1;
  end;
  if s[len] = FormatSettings.DecimalSeparator then dec(len);
  SetLength(Result, len +1);
  for i := 1 to len do
    Result[i] := AnsiChar(s[i]);
  Result[len+1] := #32;
end;

//------------------------------------------------------------------------------
// TUseElement
//------------------------------------------------------------------------------

procedure TUseElement.DoHrefClone(refEl: TElement);
var
  attrib: PAttrib;
  h: Cardinal;
  i: integer;
begin

  if refEl.fNameHash = hPolyline then
    surrogate := TPolyElement.Create(fParent, refEl.fNameHash, false) else
    surrogate := TElementClass(refEl.ClassType).Create(fParent, refEl.fNameHash);

//  tmpAttribs := fAttribs;
//  fAttribs := nil;

  for i := 0 to High(refEl.fAttribs) do
    with refEl.fAttribs[i] do
    begin
      h := GetHashedName(aname, nameLen);
      case h of
        hId, hHref, hXlink_058_Href: continue;
      else
        begin
          attrib := surrogate.AddAttribute;
          attrib.aname := aname;
          attrib.nameLen := nameLen;
          attrib.value := value;
          attrib.valueLen := valueLen;
          ProcessAttrib(attrib^);
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
  len := Length(fStops);
  setLength(fStops, len+1);
  fStops[len].offset := Min(1,Max(0, offset));
  fStops[len].color := color;
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
    if fGradientUnits = 0 then
      fGradientUnits := Self.fGradientUnits;

    if Length(fStops) = 0 then
    begin
      len := Length(self.fStops);
      SetLength(fStops, len);
      for i := 0 to len -1 do
        fStops[i] := Self.fStops[i];
    end;

    if IsIdentityMatrix(fDrawInfo.matrix) then
      fDrawInfo.matrix := self.fDrawInfo.matrix;
  end;
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
  if not Assigned(other) or not (other is TLinGradElement) then Exit;
  inherited;
  with TLinGradElement(other) do
  begin
    if not IsValid(self.startPt) then startPt := self.startPt;
    if not IsValid(self.endPt) then endPt := self.endPt;
  end;
end;

//------------------------------------------------------------------------------
// TClipPathElement
//------------------------------------------------------------------------------

constructor TClipPathElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.inDefs := true;
end;

//------------------------------------------------------------------------------
// TShapeElement
//------------------------------------------------------------------------------

constructor TShapeElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fReader.fShapesList.Add(Self);
  SetLength(paths, 1);
  isClosed := true;
end;
//------------------------------------------------------------------------------

function TShapeElement.GetVal(out val: double): Boolean;
begin
  Result := GetNum(fCurrent, fCurrentEnd, true, val);
end;
//------------------------------------------------------------------------------

function TShapeElement.GetDrawInfo: TDrawInfo;
begin
    Result := fDrawInfo;
end;
//------------------------------------------------------------------------------

function TShapeElement.GetPathCount: integer;
begin
  Result := Length(paths);
end;
//------------------------------------------------------------------------------

function TShapeElement.GetPath(index: integer; out path: TPathD;
  out isClosed: Boolean): Boolean;
begin
  Result := Length(paths) > 0;
  if not Result then Exit;
  path := CopyPath(self.paths[0]);
  isClosed := self.isClosed;
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

procedure TPathElement.ParseD(const attrib: TAttrib);
var
  i: integer;
  d: double;
  currPt: TPointD;
  isRelative: Boolean;
begin
  fCurrent := attrib.value;
  fCurrentEnd := fCurrent + attrib.valueLen;

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

function TPathElement.GetPathCount: integer;
begin
  Result := Length(dpaths);
end;
//------------------------------------------------------------------------------


//function TPathElement.Save: AnsiString;
//
//  function SegTypeToChar(st: TDsegType): AnsiChar;
//  begin
//    case st of
//      dsMove:     Result := 'M';
//      dsLine:     Result := 'L';
//      dsHorz:     Result := 'H';
//      dsVert:     Result := 'V';
//      dsArc:      Result := 'A';
//      dsQBez:     Result := 'Q';
//      dsCBez:     Result := 'C';
//      dsQSpline:  Result := 'T';
//      dsCSpline:  Result := 'S';
//      dsClose:    Result := 'Z';
//      else Result := '?';
//    end;
//  end;
//
//begin
//  //todo :)
//end;
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
    currPt := path[pathLen -1];
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
                pt3.X := vals[j*2];
                pt3.Y := vals[j*2 +1];
                pt4.X := vals[j*2 +2];
                pt4.Y := vals[j*2 +3];
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

function TPathElement.GetPath(index: integer; out path: TPathD;
  out isClosed: Boolean): Boolean;
begin
  Result := (index >= 0) and (index < Length(dpaths));
  if not Result then Exit;
  Flatten(index, path, isClosed);
end;

//------------------------------------------------------------------------------
// TPolyElement
//------------------------------------------------------------------------------

constructor TPolyElement.Create(parent: TElement;
  hashName: Cardinal; isClosed: Boolean);
begin
  inherited Create(parent, hashName);
  Self.isClosed := isClosed;
end;
//------------------------------------------------------------------------------

procedure TPolyElement.ParsePoints(const attrib: TAttrib);
var
  currCnt, currCap: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if currCnt = currCap then
    begin
      currCap := currCap + buffSize;
      SetLength(paths[0], currCap);
    end;
    paths[0][currCnt] := pt;
    inc(currCnt);
  end;

var
  pt: TPointD;
begin
  currCnt     := 0;
  currCap     := buffSize;
  fCurrent := attrib.value;
  SetLength(paths[0], currCap);
  while NumPending(fCurrent, fCurrentEnd, true) and
    GetVal(pt.X) and GetVal(pt.Y) do
      AddPoint(pt);
  SetLength(paths[0], currCnt);
end;

//------------------------------------------------------------------------------
// TLineElement
//------------------------------------------------------------------------------

constructor TLineElement.Create(parent: TElement; hashName: Cardinal);
begin
  SetLength(paths[0], 2);
  paths[0][0] := NullPointD; paths[0][1] := NullPointD;
  inherited;
end;

//------------------------------------------------------------------------------
// TCircleElement
//------------------------------------------------------------------------------

function TCircleElement.LoadAttributes: Boolean;
begin
  Result := inherited;
  if radius > 0 then
    with centerPt do
      paths[0] := Ellipse(RectD(X -radius, Y -radius, X +radius, Y +radius));
end;

//------------------------------------------------------------------------------
// TEllipseElement
//------------------------------------------------------------------------------

function TEllipseElement.LoadAttributes: Boolean;
begin
  Result := inherited;
  if (radius.X > 0) and (radius.Y > 0) then
    with centerPt do
      paths[0] := Ellipse(RectD(X-radius.X, Y-radius.Y, X+radius.X, Y+radius.Y));
end;

//------------------------------------------------------------------------------
// TRectElement
//------------------------------------------------------------------------------

function TRectElement.LoadAttributes: Boolean;
begin
  Result := inherited;

  if not Result or (rec.width <= 0) or (rec.height <= 0) then Exit;
  if (radius.X > 0) or (radius.Y > 0) then
  begin
    if (radius.X <= 0) then radius.X := radius.Y
    else if (radius.Y <= 0) then radius.Y := radius.X;
    //elliptical rect rounding not currently supported, so ...
    paths[0] := RoundRect(rec.RectD, Average(radius.X, radius.Y))
  end else
    paths[0] := Rectangle(rec.RectD);
end;

//------------------------------------------------------------------------------
// TStyleElement
//------------------------------------------------------------------------------

procedure TStyleElement.LoadContent;
var
  i, nameLen, len, cap: integer;
  hash: Cardinal;
  pname, pstyle: PAnsiChar;
  names: array of string;
  style: AnsiString;

  procedure AddName(const name: AnsiString);
  begin
    if len = cap then
    begin
      cap := cap + buffSize;
      SetLength(names, cap);
    end;
    names[len] := string(name);
    inc(len);
  end;

begin
  len := 0; cap := 0;
  if (PeekChar = '<') and ((fCurrent+1)^ = '!') and
    ((fCurrent+2)^ = '[') and
    (GetHashedName(fCurrent + 3, 5) = hCDATA) then
  begin
    inc(fCurrent, 8);
    if fCurrent^ <> '[' then Exit;
    inc(fCurrent);
  end;

  while PeekChar = '.' do
  begin
    //get one or more class names for each pending style
    inc(fCurrent);
    GetElemOrAttribName(pname, nameLen);
    AddName(AnsiLowercase(pname, nameLen));
    if PeekChar = ',' then
    begin
      inc(fCurrent);
      Continue;
    end;
    if len = 0 then break;
    SetLength(names, len);

    //now get the style
    if PeekChar <> '{' then Break;
    inc(fCurrent);
    pstyle := fCurrent;
    while (fCurrent < fCurrentEnd) and (fCurrent^ <> '}') do
      inc(fCurrent);
    if (fCurrent = fCurrentEnd) then break;
    style := AnsiLowerCase(pstyle, fCurrent - pstyle);

    //finally, for each class name add this style
    //todo - check for existing classes and append styles
    for i := 0 to High(names) do
      fReader.fClassesList.Add(string(names[i]), style);
    names := nil;
    len := 0; cap := 0;
    inc(fCurrent);
  end;

  //skip trailing unknown content
  while (fCurrent < fCurrentEnd) and (fCurrent^ <> '>') do inc(fCurrent);
  inc(fCurrent);
  //we should now be at </STYLE>
  if (PeekChar <> '<') then Exit;
  if  ((fCurrent +1)^ <> '/') then Exit;
  inc(fCurrent, 2);
  fIsValid := GetHashedCurrentWord(hash) and (fNameHash = hash);
  if fIsValid then inc(fCurrent); //trailing '>'
end;

//------------------------------------------------------------------------------
// TAttrib
//------------------------------------------------------------------------------

function TAttrib.GetName: AnsiString;
var
  i: integer;
  c: PAnsiChar;
begin
  SetLength(Result, nameLen);
  c := aname;
  for i := 1 to nameLen do
  begin
    Result[i] := LowerCaseTable[c^]; inc(c);
  end;
end;
//------------------------------------------------------------------------------

function TAttrib.GetValue: AnsiString;
var
  i: integer;
  c: PAnsiChar;
begin
  SetLength(Result, valueLen);
  c := value;
  for i := 1 to valueLen do
  begin
    Result[i] := LowerCaseTable[c^]; inc(c);
  end;
end;

//------------------------------------------------------------------------------
// TElement
//------------------------------------------------------------------------------

constructor TElement.Create(parent: TElement; hashName: Cardinal);
var
  nameLen: integer;
begin
  fChilds    := TList.Create;
  fNameHash  := hashName;
  fIsValid   := false;

  if not Assigned(parent) then Exit; //svg root element

  self.fParent      := parent;
  fReader      := parent.fReader;
  fCurrentEnd  := fReader.fEndStream;
  fDrawInfo    := parent.fDrawInfo;
  if (fNameHash = hDefs) then fDrawInfo.inDefs := fDrawInfo.inDefs;
  fCurrent     := parent.fCurrent;
  fIsValid := SkipBlanks(fCurrent, fCurrentEnd) and
    GetElemOrAttribName(fName, nameLen);
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

function TElement.GetElemOrAttribName(
  out name: PAnsiChar; out nameLen: integer): Boolean;
begin
  Result := IsAlpha(fCurrent^);
  if not Result then Exit;
  name := fCurrent; nameLen := GetElemOrAttribNameLen(fCurrent, fCurrentEnd);
end;
//------------------------------------------------------------------------------

function TElement.GetHashedCurrentWord(out hash: Cardinal): Boolean;
var
  name: PAnsiChar;
  nameLen: integer;
begin
  Result := IsAlpha(fCurrent^);
  if not Result then Exit;
  name := fCurrent;
  nameLen := GetElemOrAttribNameLen(fCurrent, fCurrentEnd);
  hash := GetHashedName(name, nameLen);
end;
//------------------------------------------------------------------------------

function TElement.GetAttribValue(out value: PAnsiChar;
  out valueLen: integer): Boolean;
begin
  IsAlpha(fCurrent^);
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
  if Assigned(surrogate) then
    Result.ownerEl := surrogate else
    Result.ownerEl := self;
end;
//------------------------------------------------------------------------------

function TElement.LoadAttributes: Boolean;
var
  attrib: PAttrib;
  savedCurrent: PAnsiChar;
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
    GetElemOrAttribName(attrib.aname, attrib.nameLen);
    if fCurrent^ <> '=' then Exit; inc(fCurrent);
    GetAttribValue(attrib.value, attrib.valueLen);
    if fCurrent^ <> '"' then Exit; //end of attrib. value
    inc(fCurrent);
    savedCurrent := fCurrent;
    ProcessAttrib(attrib^);
    fCurrent := savedCurrent;
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.ProcessAttrib(const attrib: TAttrib);
var
  i: integer;
  hash: Cardinal;
begin
  hash := GetHashedName(attrib.aname, attrib.nameLen);
  i := AttribFuncList.IndexOf(inttohex(hash, 8));
  if i >= 0 then
    TAttribFunc(AttribFuncList.Objects[i])(attrib);
end;
//------------------------------------------------------------------------------

function TElement.FindRefElement(refname: PAnsiChar; refNameLen: integer): TElement;
var
  i: integer;
begin
  if refname^ = '#' then
  begin
    inc(refname);
    dec(refNameLen);
  end;

  i := fReader.fIdList.IndexOf(string(AnsiLowercase(refname, refNameLen)));
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
begin
  Result := nil;
  savedCurrent := fCurrent;
  if not GetElemOrAttribName(name, nameLen) then Exit;
  fCurrent := savedCurrent;
  hashedName := GetHashedName(name, nameLen);
  case hashedName of
    hCircle         : Result := TCircleElement.Create(self, hashedName);
    hEllipse        : Result := TEllipseElement.Create(self, hashedName);
    hLineargradient : Result := TLinGradElement.Create(self, hashedName);
    hPath           : Result := TPathElement.Create(self, hashedName);
    hPolyline       : Result := TPolyElement.Create(self, hashedName, false);
    hPolygon        : Result := TPolyElement.Create(self, hashedName, true);
    hRadialgradient : Result := TRadGradElement.Create(self, hashedName);
    hRect           : Result := TRectElement.Create(self, hashedName);
    hStop           : Result := TGradStopElement.Create(self, hashedName);
    hStyle          : Result := TStyleElement.Create(self, hashedName);
    hUse            : Result := TUseElement.Create(self, hashedName);
    else              Result := TElement.Create(self, hashedName);
  end;

  if not Result.fIsValid then
  begin
    FreeAndNil(Result);
    Exit;
  end;
  fCurrent := Result.fCurrent;
end;
//------------------------------------------------------------------------------

function TElement.PeekChar: AnsiChar;
begin
  if not SkipBlanks(fCurrent, fCurrentEnd) then
    Result := #0 else
    Result := fCurrent^;
end;
//------------------------------------------------------------------------------

procedure TElement.ParseClassAttrib(const classAttrib: TAttrib);
var
  i: integer;
  className: AnsiString;
  style: AnsiString;
begin
  className := AnsiName(classAttrib.value, classAttrib.valueLen);
  i := classAttrib.ownerEl.fReader.fClassesList.IndexOf(string(classname));
  if i < 0 then Exit;
  style := PAnsiStringRec(classAttrib.ownerEl.fReader.fClassesList.objects[i]).str;
  ParseStyle(style);
end;
//------------------------------------------------------------------------------

procedure TElement.ParseStyle(style: AnsiString);
var
  attribCnt, nameLen: integer;
  newAt: PAttrib;
  current, endC, name: PAnsiChar;
  c: AnsiChar;
begin
  attribCnt := Length(fAttribs);
  current := @style[1];
  endC := current + Length(style);

  while SkipBlanks(current, endC) do
  begin
    name := current;
    nameLen := GetStyleNameLen(current, endC);
    if nameLen = 0 then Break;

    SetLength(fAttribs, attribCnt +1);
    newAt := @fAttribs[attribCnt];
    inc(attribCnt);

    newAt.ownerEl := self;
    newAt.aname := name;
    newAt.nameLen := nameLen;

    if not GetChar(current, endC, c) or (c <> ':') or  //syntax check
      not SkipBlanks(current,endC) then Break;
    newAt.value := current;
    inc(current);
    while (current < endC) and (current^ <> ';') do inc(current);
    newAt.valueLen := current - newAt.value;
    inc(current);
    ProcessAttrib(newAt^);
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.ParseTransform(transform: AnsiString);
var
  i: integer;
  current, endC: PAnsiChar;
  c: AnsiChar;

  word: AnsiString;
  values: array[0..5] of double;
  mat: PMatrixD;
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
  current := @transform[1];
  endC := current + Length(transform);

  //surprisingly, and I think this is a bug in SVG not a feature ...
  //transform operations must be performed in reverse order
  while GetWord(current, endC, word) do
  begin
    if Length(word) < 5 then Exit;
    word[5] := LowerCaseTable[word[5]];
    if not GetChar(current, endC, c) or (c <> '(') then Exit; //syntax check
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
    if not GetChar(current, endC, c) or (c <> ')') then Exit; //syntax check

    mat := NewMatrix;
    case word[5] of
      'e' : //scalE
        if not IsValid(values[1]) then
          MatrixScale(mat^, values[0]) else
            MatrixScale(mat^, values[0], values[1]);
      'i' : //matrIx
        if IsValid(values[5]) then
        begin
          mat[0,0] :=  values[0];
          mat[0,1] :=  values[1];
          mat[1,0] :=  values[2];
          mat[1,1] :=  values[3];
          mat[2,0] :=  values[4];
          mat[2,1] :=  values[5];
        end;
      's' : //tranSlateX, tranSlateY & tranSlate
        if Length(word) =10  then
        begin
          if LowerCaseTable[word[10]] = 'x' then
            MatrixTranslate(mat^, values[0], 0)
          else if LowerCaseTable[word[10]] = 'y' then
            MatrixTranslate(mat^, 0, values[0]);
        end
        else if IsValid(values[1]) then
          MatrixTranslate(mat^, values[0], values[1])
        else
          MatrixTranslate(mat^, values[0], 0);
      't' : //rotaTe
        if IsValid(values[2]) then
          MatrixRotate(mat^, PointD(values[1],values[2]), DegToRad(values[0]))
        else
          MatrixRotate(mat^, NullPointD, DegToRad(values[0]));
       'x' : //skewX
         begin
            MatrixSkew(mat^, DegToRad(values[0]), 0);
         end;
       'y' : //skewY
         begin
            MatrixSkew(mat^, 0, DegToRad(values[0]));
         end;
    end;
  end;
  for i := High(mats) downto 0 do
    fDrawInfo.matrix := MatrixMultiply(mats[i], fDrawInfo.matrix);
end;
//------------------------------------------------------------------------------

procedure TElement.AssignTo(var other: TElement);
begin
end;
//------------------------------------------------------------------------------

procedure TElement.LoadContent;
var
  el, el2: TElement;
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
            break;
          end;
          inc(fCurrent);
        end;
      end
      else if fCurrent^ = '/' then
      begin
        //this should be the element close
        inc(fCurrent);
        fIsValid := GetHashedCurrentWord(hash) and (fNameHash = hash);
        if fIsValid then inc(fCurrent);
        Exit;
      end else
      begin
        //load a child element
        el := LoadChild;
        if not Assigned(el) then
          Break
        else if not el.LoadAttributes then
        begin
          if Assigned(el.surrogate) then el.surrogate.Free;
          el.Free;
          break;
        end
        else if Assigned(el.surrogate) then
        begin
          el2 := el.surrogate;
          el2.fHasContent := el.fHasContent;
          el2.fCurrent := el.fCurrent;
          el.Free;
          el := el2;
        end;
        fChilds.Add(el);
        if el.fHasContent then
          el.LoadContent;
        fCurrent := el.fCurrent;
      end;
    end
    else if fCurrent^ = '>' then
    begin
      //oops! something's wrong
      inc(fCurrent);
      fIsValid := false;
      break;
    end else
    begin
      //skip unknown (probably text) content
      while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
    end;
  end;
end;
//------------------------------------------------------------------------------

function TElement.FindAttribute(const attribName: AnsiString): PAttrib;
var
  i, nLen: integer;
begin
  Result := nil;
  nLen := Length(attribName);
  for i := 0 to High(fAttribs) do
    with fAttribs[i] do
      if (nLen = nameLen) and CompareNames(attribName, aname, nLen) then
    begin
      Result := @fAttribs[i];
      Break;
    end;
end;
//------------------------------------------------------------------------------

function TElement.FindAttribute(hash: Cardinal): PAttrib;
var
  i: integer;
begin
  result := nil;
  for i := 0 to High(fAttribs) do
    with fAttribs[i] do
      if GetHashedName(aname, nameLen) = hash then
      begin
        Result := @fAttribs[i];
        Break;
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
  fIdList.Duplicates := dupError;
  fIdList.Sorted  := True;

  fClassesList    := TStrStrList.Create;
  fClassesList.Duplicates := dupIgnore;
  fClassesList.Sorted := True;

  fLinGradRenderer := TLinearGradientRenderer.Create;
  fRadGradRenderer := TSvgRadialGradientRenderer.Create;
end;
//------------------------------------------------------------------------------

destructor TSvgReader.Destroy;
begin
  Clear;
  fMemStream.Free;
  fShapesList.Free;
  fIdList.Free;
  fClassesList.Free;
  fLinGradRenderer.Free;
  fRadGradRenderer.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.Clear;
begin
  FreeAndNil(fRootElement);
  fMemStream.clear;
  fShapesList.Clear;
  fIdList.Clear;
  fClassesList.Clear;
  fViewbox := NullRectD;
  fEndStream := nil;
end;
//------------------------------------------------------------------------------

function DeScaleMatrix(mat: TMatrixD): double;
begin
    Result := Sqrt(mat[0][1]*mat[0][1] + mat[1][1]*mat[1][1]);
end;
//------------------------------------------------------------------------------

procedure TSvgReader.DrawImage(img: TImage32; scaleToImage: Boolean);
var
  i,j, hiStops: integer;
  rec: TRect;
  sx, sy: double;
  p: TPathD;
  closedPP, openPP: TPathsD;
  pt1, pt2: TPointD;
  gradEl: TGradientElement;
  offsetAndScaleMat, mat: TMatrixD;
const
  endStyle: array[boolean] of TEndStyle = (esPolygon, esRound);
begin
  if not Assigned(fRootElement) or fViewbox.IsEmpty then Exit;

  rec := Rect(fViewbox);

  if scaleToImage and not img.IsEmpty then
  begin
    sx := img.width / fViewbox.Width;
    sy := img.height / fViewbox.Height;
    if sy < sx then sx := sy;
  end else
  begin
    img.SetSize(RectWidth(rec), RectHeight(rec));
    sx := 1;
  end;

  for i := 0 to fShapesList.Count -1 do
    with TShapeElement(fShapesList[i]) do
    begin
      closedPP := nil;
      openPP := nil;
      gradEl := DrawInfo.gradElement;

      for j := 0 to PathCount -1 do
      begin
        GetPath(j, p, isClosed);
        if isClosed or (DrawInfo.fillColor shr 24 > 0) then
          AppendPath(closedPP, p) else
          AppendPath(openPP, p);
      end;

      mat := DrawInfo.matrix;
      MatrixTranslate(mat, -fViewbox.Left, -fViewbox.Top);
      MatrixScale(mat, sx, sx);

      MatrixApply(mat, openPP);
      MatrixApply(mat, closedPP);

      if assigned(gradEl) then
      begin
        rec := GetBounds(closedPP);

        //GradientUnits: default = objectBoundingBox
        //if (gradEl.fGradientUnits <> hUserSpaceOnUse) then

        if (gradEl is TRadGradElement) then
        begin
          with TRadGradElement(gradEl) do
            if (Length(fStops) > 1) then
            begin
              hiStops := High(fStops);
              if not IsIdentityMatrix(fDrawInfo.matrix) then
              begin
                mat := fDrawInfo.matrix;
                pt1 := PointD(rec.TopLeft);
                pt2 := PointD(rec.BottomRight);
                MatrixApply(mat, pt1);
                MatrixApply(mat, pt2);
                rec.TopLeft := Point(pt1);
                rec.BottomRight := Point(pt2);
              end;
              fRadGradRenderer.SetParameters(rec, MidPoint(rec),
                fStops[0].color, fStops[hiStops].color);
              for j := 1 to hiStops -1 do
                 fRadGradRenderer.InsertColorStop(fStops[j].offset,
                   fStops[j].color);
              //nb: radial gradient transforms aren't currently supported.
            end
        end
        else if (gradEl is TLinGradElement) then
          with TLinGradElement(gradEl) do
            if (Length(fStops) > 1) and
              IsValid(startPt) and IsValid(endPt) then
            begin
              hiStops := High(fStops);
              pt1 := startPt; pt2 := endPt;
              mat := MatrixMultiply(mat, fDrawInfo.matrix);
              MatrixApply(mat, pt1);
              MatrixApply(mat, pt2);
              fLinGradRenderer.SetParameters(
                pt1, pt2, fStops[0].color, fStops[hiStops].color);
              for j := 1 to hiStops -1 do
                 fLinGradRenderer.InsertColorStop(fStops[j].offset,
                   fStops[j].color);
            end;
      end;

      sy := DeScaleMatrix(mat);

      if Assigned(closedPP) then
      begin
        if Assigned(gradEl) then
        begin
          if gradEl is TRadGradElement then
            DrawPolygon(img, closedPP, DrawInfo.fillRule, fRadGradRenderer)
          else
            DrawPolygon(img, closedPP, DrawInfo.fillRule, fLinGradRenderer);
        end
        else if (DrawInfo.fillColor <> clNone32) then
          DrawPolygon(img, closedPP, DrawInfo.fillRule,  DrawInfo.fillColor);

        if (DrawInfo.strokeColor <> clNone32) and
          (DrawInfo.strokeWidth <> 0) then
            DrawLine(img, closedPP, DrawInfo.strokeWidth *sy,
              DrawInfo.strokeColor, esPolygon);
      end;

      if Assigned(openPP) and
        (DrawInfo.strokeColor <> clNone32) and
        (DrawInfo.strokeWidth <> 0.0) then
      begin
        DrawLine(img, openPP, DrawInfo.strokeWidth * sy,
          DrawInfo.strokeColor, esRound);
      end;
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
var
  svgStart: PAnsiChar;
begin
  Result := false;
  Clear;
  if not Assigned(stream) then Exit;

  fMemStream.LoadFromStream(stream);
  fEndStream := PAnsiChar(fMemStream.Memory) + fMemStream.Size;

  fRootElement := TElement.Create(nil, hSvg);
  fRootElement.fReader := self;
  fRootElement.fCurrentEnd := self.fEndStream;
  fRootElement.fDrawInfo :=  defaultDrawInfo;

  if not GetSvgStart(svgStart) then Exit;
  fRootElement.fName := svgStart;
  inc(svgStart, 3);
  fRootElement.fCurrent := svgStart;
  if not fRootElement.LoadAttributes or
    not fRootElement.fHasContent then Exit;
  GetViewBox;
  fRootElement.fCurrent := svgStart;
  fRootElement.LoadContent;
  Result := fRootElement.fIsValid;
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

procedure TSvgReader.GetViewBox;
var
  attrib: PAttrib;
  c, stop: PAnsiChar;
begin
  if not Assigned(fRootElement) then Exit;
  fViewbox := NullRectD;
  attrib := fRootElement.FindAttribute('viewbox');
  if Assigned(attrib) then
  begin
    c := PAnsiChar(attrib.value);
    if GetNum(c, fEndStream, false, fViewbox.Left) and
      GetNum(c, fEndStream, true, fViewbox.Top) and
      GetNum(c, fEndStream, true, fViewbox.Right) and
      GetNum(c, fEndStream, true, fViewbox.Bottom) then
    begin
      fViewbox.Right := fViewbox.Right + fViewbox.Left;
      fViewbox.Bottom := fViewbox.Bottom + fViewbox.Top;
    end;
  end;
  if not fViewbox.IsEmpty then Exit;
  attrib := fRootElement.FindAttribute(hWidth);
  if not Assigned(attrib) then Exit;
  c := attrib.value;
  stop := c + attrib.valueLen;
  if not GetNum(c, stop, false, fViewbox.Right) then Exit;
  attrib := fRootElement.FindAttribute(hHeight);
  if not Assigned(attrib) then Exit;
  c := attrib.value;
  stop := c + attrib.valueLen;
  GetNum(c, stop, false, fViewbox.Bottom);
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

procedure GradientTransform(const attrib: TAttrib);
begin
  if attrib.ownerEl is TGradientElement then
    with TGradientElement(attrib.ownerEl) do
      ParseTransform(AnsiName(attrib.value, attrib.valueLen));
end;
//------------------------------------------------------------------------------

procedure Gradientunits(const attrib: TAttrib);
begin
  if attrib.ownerEl is TGradientElement then
    with TGradientElement(attrib.ownerEl) do
      fGradientUnits := GetHashedName(attrib.value, attrib.valueLen);
end;
//------------------------------------------------------------------------------

procedure Id(const attrib: TAttrib);
var
  id: AnsiString;
begin
  with attrib do
  begin
    id := AnsiLowercase(value, valueLen);
    ownerEl.fReader.fIdList.AddObject(string(id), ownerEl);
  end;
end;
//------------------------------------------------------------------------------

procedure Href(const attrib: TAttrib);
var
  el, refEl: TElement;
begin
  with attrib do
  begin
    el := attrib.ownerEl;
    case el.fNameHash of
      hLinearGradient, hRadialGradient:
        begin
          refEl := el.FindRefElement(value, valueLen);
          if assigned(refEl) and (refEl is TGradientElement) then
            TGradientElement(refEl).AssignTo(el);
        end;
      hUse:
        begin
          refEl := el.FindRefElement(value, valueLen);
          if assigned(refEl) and (el is TUseElement) then
            TUseElement(el).DoHrefClone(refEl);
        end;
    end;

  end;
end;
//------------------------------------------------------------------------------

procedure Class_(const attrib: TAttrib);
begin
  if attrib.ownerEl is TShapeElement then
    TShapeElement(attrib.ownerEl).ParseClassAttrib(attrib);
end;
//------------------------------------------------------------------------------

procedure D_(const attrib: TAttrib);
begin
  if attrib.ownerEl is TPathElement then
    TPathElement(attrib.ownerEl).ParseD(attrib);
end;
//------------------------------------------------------------------------------

procedure Fill(const attrib: TAttrib);
begin
  AttribToColor32(attrib, attrib.ownerEl.fDrawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure FillOpacity(const attrib: TAttrib);
begin
  AttribToOpacity(attrib, attrib.ownerEl.fDrawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure Offset_(const attrib: TAttrib);
begin
  if attrib.ownerEl is TGradStopElement then
    AttribToFloat(attrib, TGradStopElement(attrib.ownerEl).offset);
end;
//------------------------------------------------------------------------------

procedure StopColor(const attrib: TAttrib);
begin
  if attrib.ownerEl is TGradStopElement then
    AttribToColor32(attrib, TGradStopElement(attrib.ownerEl).color);
end;
//------------------------------------------------------------------------------

procedure StopOpacity(const attrib: TAttrib);
begin
  if attrib.ownerEl is TGradStopElement then
  AttribToOpacity(attrib, TGradStopElement(attrib.ownerEl).color);
end;
//------------------------------------------------------------------------------

procedure Points(const attrib: TAttrib);
begin
  if attrib.ownerEl is TPolyElement then
    TPolyElement(attrib.ownerEl).ParsePoints(attrib);
end;
//------------------------------------------------------------------------------

procedure Stroke(const attrib: TAttrib);
begin
  AttribToColor32(attrib, attrib.ownerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeOpacity(const attrib: TAttrib);
begin
  AttribToOpacity(attrib, attrib.ownerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeWidth(const attrib: TAttrib);
begin
    AttribToFloat(attrib, attrib.ownerEl.fDrawInfo.strokeWidth);
end;
//------------------------------------------------------------------------------

procedure FillRule(const attrib: TAttrib);
begin
  if LowerCaseTable[attrib.value[0]] = 'e' then
    attrib.ownerEl.fDrawInfo.fillRule := frEvenOdd else
    attrib.ownerEl.fDrawInfo.fillRule := frNonZero;
end;
//------------------------------------------------------------------------------

procedure Style(const attrib: TAttrib);
var
  style: AnsiString;
begin
  style := AnsiName(attrib.value, attrib.valueLen);
  attrib.ownerEl.ParseStyle(style);
end;
//------------------------------------------------------------------------------

procedure Transform(const attrib: TAttrib);
var
  transform: AnsiString;
begin
  transform := AnsiName(attrib.value, attrib.valueLen);
  attrib.ownerEl.ParseTransform(transform);
end;
//------------------------------------------------------------------------------

procedure Height_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hRect:
      with TRectElement(attrib.ownerEl) do
        AttribToFloat(attrib, rec.Height);
  end;
end;
//------------------------------------------------------------------------------

procedure Width_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hRect:
      with TRectElement(attrib.ownerEl) do
        AttribToFloat(attrib, rec.Width);
  end;
end;
//------------------------------------------------------------------------------

procedure Cx_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hCircle:
      with TCircleElement(attrib.ownerEl) do
        AttribToFloat(attrib, centerPt.X);
    hEllipse:
      with TEllipseElement(attrib.ownerEl) do
        AttribToFloat(attrib, centerPt.X);
  end;
end;
//------------------------------------------------------------------------------

procedure Cy_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hCircle:
      with TCircleElement(attrib.ownerEl) do
        AttribToFloat(attrib, centerPt.Y);
    hEllipse:
      with TEllipseElement(attrib.ownerEl) do
        AttribToFloat(attrib, centerPt.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure Rx_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hRect:
      with TRectElement(attrib.ownerEl) do
        AttribToFloat(attrib, radius.X);
    hCircle:
      with TCircleElement(attrib.ownerEl) do
        AttribToFloat(attrib, radius);
    hEllipse:
      with TEllipseElement(attrib.ownerEl) do
        AttribToFloat(attrib, radius.X);
  end;
end;
//------------------------------------------------------------------------------

procedure Ry_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hRect:
      with TRectElement(attrib.ownerEl) do
        AttribToFloat(attrib, radius.Y);
    hEllipse:
      with TEllipseElement(attrib.ownerEl) do
        AttribToFloat(attrib, radius.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure X1_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hRect:
      with TRectElement(attrib.ownerEl) do
        AttribToFloat(attrib, rec.left);
    hLine:
      with TLineElement(attrib.ownerEl) do
        AttribToFloat(attrib, paths[0][0].X);
    hLinearGradient:
      with TLinGradElement(attrib.ownerEl) do
        AttribToFloat(attrib, startPt.X);
  end;
end;
//------------------------------------------------------------------------------

procedure X2_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hLine:
      with TLineElement(attrib.ownerEl) do
        AttribToFloat(attrib, paths[0][1].X);
    hLinearGradient:
      with TLinGradElement(attrib.ownerEl) do
        AttribToFloat(attrib, endPt.X);
  end;
end;
//------------------------------------------------------------------------------

procedure Y1_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hRect:
      with TRectElement(attrib.ownerEl) do
        AttribToFloat(attrib, rec.top);
    hLine:
      with TLineElement(attrib.ownerEl) do
        AttribToFloat(attrib, paths[0][0].Y);
    hLinearGradient:
      with TLinGradElement(attrib.ownerEl) do
        AttribToFloat(attrib, startPt.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure Y2_(const attrib: TAttrib);
begin
  case attrib.ownerEl.fNameHash of
    hLine:
      with TLineElement(attrib.ownerEl) do
        AttribToFloat(attrib, paths[0][1].Y);
    hLinearGradient:
      with TLinGradElement(attrib.ownerEl) do
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

  RegisterAttribute('class',              Class_);
  RegisterAttribute('cx',                 Cx_);
  RegisterAttribute('cy',                 Cy_);
  RegisterAttribute('d',                  D_);
  RegisterAttribute('fill',               Fill);
  RegisterAttribute('fill-opacity',       FillOpacity);
  RegisterAttribute('fill-fule',          FillRule);
  RegisterAttribute(hGradientTransform,   GradientTransform);
  RegisterAttribute(hGradientUnits,       GradientUnits);
  RegisterAttribute(hHeight,              Height_);
  RegisterAttribute(hHref,                Href);
  RegisterAttribute(hId,                  Id);
  RegisterAttribute('offset',             Offset_);
  RegisterAttribute('points',             Points);
  RegisterAttribute('r',                  Rx_);
  RegisterAttribute('rx',                 Rx_);
  RegisterAttribute('ry',                 Ry_);
  RegisterAttribute(hStop_045_Color,      StopColor);
  RegisterAttribute(hStop_045_Opacity,    StopOpacity);
  RegisterAttribute('stroke',             Stroke);
  RegisterAttribute('stroke-opacity',     StrokeOpacity);
  RegisterAttribute('stroke-width',       StrokeWidth);
  RegisterAttribute('style',              Style);
  RegisterAttribute('transform',          Transform);
  RegisterAttribute(hWidth,               Width_);
  RegisterAttribute(hX,                   X1_);
  RegisterAttribute(hX1,                  X1_);
  RegisterAttribute(hX2,                  X2_);
  RegisterAttribute(hXlink_058_Href,      Href);
  RegisterAttribute(hY,                   Y1_);
  RegisterAttribute(hY1,                  Y1_);
  RegisterAttribute(hY2,                  Y2_);

  AttribFuncList.Sorted := true;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeLowerCaseTable;
  MakeAttribFuncList;
  MakeColorConstList;

finalization
  AttribFuncList.Free;
  ColorConstList.Free;
end.
