unit Image32_SVG_Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.23                                                            *
* Date      :  14 April 2021                                                   *
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
//clipPath, url, gradients, class (for styles), style CDATA, lineCap, lineJoin

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types, Math,
  Image32, Image32_Vector, Image32_Transform;

type
  PDrawInfo = ^TDrawInfo;
  TDrawInfo = record
  public
    fillColor   : TColor32;
    fillRule    : TFillRule;
    strokeColor : TColor32;
    strokeWidth : double;
    //lineCap  : TEndStyle;     //stroke-linecap
    //lineJoin : TJoinStyle;    //stroke-linejoin
    //miterLim : double;        //stroke-miterlimit
    matrix      : TMatrixD;
    inDefs      : Boolean;
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
    function LoadChildAndAttribs: TElement;
    function LoadAttributes: Boolean;
    procedure LoadContent;
    function GetHashedCurrentWord(out hash: Cardinal): Boolean;
    function GetElemOrAttribName(out name: PAnsiChar;
      out nameLen: integer): Boolean;
    function GetAttribValue(out value: PAnsiChar; out valueLen: integer): Boolean;
    function FindReferencedElement(const ref: AnsiString): TElement;
    function FindAttribute(const attribName: AnsiString): PAttrib; overload;
    function FindAttribute(hash: Cardinal): PAttrib; overload;
    procedure ProcessAttrib(const attrib: TAttrib);
  public
    constructor Create(parent: TElement; hashName: Cardinal); virtual;
    destructor  Destroy; override;
  end;

  TUseElement = class(TElement)
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TShapeElement = class(TElement)
  protected
    isClosed    : Boolean;
    path        : TPathD;
    function GetVal(out val: double): Boolean;
    function GetDrawInfo: TDrawInfo;
    function GetPathCount: integer; virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
    procedure ParseAttrib(const attrib: TAttrib); virtual;
    procedure ParseAttributes; virtual;
    function GetFlattenedPath(index: integer;
      out path: TPathD; out isClosed: Boolean): Boolean;  virtual;
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
    function GetPathCount: integer; override;
  public
    procedure ParseAttrib(const attrib: TAttrib);  override;
    function GetFlattenedPath(index: integer;
      out path: TPathD; out isClosed: Boolean): Boolean;  override;
  end;

  TPolyElement = class(TShapeElement) //polyline or polygon
  private
    isClosed    : Boolean;
  public
    constructor Create(parent: TElement; hashName: Cardinal; isClosed: Boolean); reintroduce;
    procedure ParseAttrib(const attrib: TAttrib); override;
  end;

  TLineElement = class(TShapeElement)
  public
    procedure ParseAttributes; override;
  end;

  TCircleElement = class(TShapeElement)
  public
    procedure ParseAttributes; override;
  end;

  TEllipseElement = class(TShapeElement)
  public
    procedure ParseAttributes; override;
  end;

  TRectElement = class(TShapeElement)
  public
    procedure ParseAttributes; override;
  end;

  TAttribFunc = procedure (const attrib: TAttrib);

  TSvgReader = class
  private
    fMemStream     : TMemoryStream;
    fEndStream     : PAnsiChar;
    fIdList        : TStringList;
    fGradientsList : TStringList;
    fShapesList    : TList;
    fViewbox       : TRectD;
    fRootElement   : TElement;
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

uses
  Image32_Draw;

type
  TColorConst = record
    ColorName : string; ColorValue: Cardinal;
  end;

const
  ColorConsts: array[0..144] of TColorConst = (
    (ColorName: 'aliceblue'; ColorValue: $FFF0F8FF),
    (ColorName: 'antiquewhite'; ColorValue: $FFFAEBD7),
    (ColorName: 'aqua'; ColorValue: $FF00FFFF),
    (ColorName: 'aquamarine'; ColorValue: $FF7FFFD4),
    (ColorName: 'azure'; ColorValue: $FFF0FFFF),
    (ColorName: 'beige'; ColorValue: $FFF5F5DC),
    (ColorName: 'bisque'; ColorValue: $FFFFE4C4),
    (ColorName: 'black'; ColorValue: $FF000000),
    (ColorName: 'blanchedalmond'; ColorValue: $FFFFEBCD),
    (ColorName: 'blue'; ColorValue: $FF0000FF),
    (ColorName: 'blueviolet'; ColorValue: $FF8A2BE2),
    (ColorName: 'brown'; ColorValue: $FFA52A2A),
    (ColorName: 'burlywood'; ColorValue: $FFDEB887),
    (ColorName: 'cadetblue'; ColorValue: $FF5F9EA0),
    (ColorName: 'chartreuse'; ColorValue: $FF7FFF00),
    (ColorName: 'chocolate'; ColorValue: $FFD2691E),
    (ColorName: 'coral'; ColorValue: $FFFF7F50),
    (ColorName: 'cornflowerblue'; ColorValue: $FF6495ED),
    (ColorName: 'cornsilk'; ColorValue: $FFFFF8DC),
    (ColorName: 'crimson'; ColorValue: $FFDC143C),
    (ColorName: 'cyan'; ColorValue: $FF00FFFF),
    (ColorName: 'darkblue'; ColorValue: $FF00008B),
    (ColorName: 'darkcyan'; ColorValue: $FF008B8B),
    (ColorName: 'darkgoldenrod'; ColorValue: $FFB8860B),
    (ColorName: 'darkgray'; ColorValue: $FFA9A9A9),
    (ColorName: 'darkgreen'; ColorValue: $FF006400),
    (ColorName: 'darkgrey'; ColorValue: $FFA9A9A9),
    (ColorName: 'darkkhaki'; ColorValue: $FFBDB76B),
    (ColorName: 'darkmagenta'; ColorValue: $FF8B008B),
    (ColorName: 'darkolivegreen'; ColorValue: $FF556B2F),
    (ColorName: 'darkorange'; ColorValue: $FFFF8C00),
    (ColorName: 'darkorchid'; ColorValue: $FF9932CC),
    (ColorName: 'darkred'; ColorValue: $FF8B0000),
    (ColorName: 'darksalmon'; ColorValue: $FFE9967A),
    (ColorName: 'darkseagreen'; ColorValue: $FF8FBC8F),
    (ColorName: 'darkslateblue'; ColorValue: $FF483D8B),
    (ColorName: 'darkslategray'; ColorValue: $FF2F4F4F),
    (ColorName: 'darkslategrey'; ColorValue: $FF2F4F4F),
    (ColorName: 'darkturquoise'; ColorValue: $FF00CED1),
    (ColorName: 'darkviolet'; ColorValue: $FF9400D3),
    (ColorName: 'deeppink'; ColorValue: $FFFF1493),
    (ColorName: 'deepskyblue'; ColorValue: $FF00BFFF),
    (ColorName: 'dimgray'; ColorValue: $FF696969),
    (ColorName: 'dimgrey'; ColorValue: $FF696969),
    (ColorName: 'dodgerblue'; ColorValue: $FF1E90FF),
    (ColorName: 'firebrick'; ColorValue: $FFB22222),
    (ColorName: 'floralwhite'; ColorValue: $FFFFFAF0),
    (ColorName: 'forestgreen'; ColorValue: $FF228B22),
    (ColorName: 'fuchsia'; ColorValue: $FFFF00FF),
    (ColorName: 'gainsboro'; ColorValue: $FFDCDCDC),
    (ColorName: 'ghostwhite'; ColorValue: $FFF8F8FF),
    (ColorName: 'gold'; ColorValue: $FFFFD700),
    (ColorName: 'goldenrod'; ColorValue: $FFDAA520),
    (ColorName: 'gray'; ColorValue: $FF808080),
    (ColorName: 'green'; ColorValue: $FF008000),
    (ColorName: 'greenyellow'; ColorValue: $FFADFF2F),
    (ColorName: 'grey'; ColorValue: $FF808080),
    (ColorName: 'honeydew'; ColorValue: $FFF0FFF0),
    (ColorName: 'hotpink'; ColorValue: $FFFF69B4),
    (ColorName: 'indianred'; ColorValue: $FFCD5C5C),
    (ColorName: 'indigo'; ColorValue: $FF4B0082),
    (ColorName: 'ivory'; ColorValue: $FFFFFFF0),
    (ColorName: 'khaki'; ColorValue: $FFF0E68C),
    (ColorName: 'lavender'; ColorValue: $FFE6E6FA),
    (ColorName: 'lavenderblush'; ColorValue: $FFFFF0F5),
    (ColorName: 'lawngreen'; ColorValue: $FF7CFC00),
    (ColorName: 'lemonchiffon'; ColorValue: $FFFFFACD),
    (ColorName: 'lightblue'; ColorValue: $FFADD8E6),
    (ColorName: 'lightcoral'; ColorValue: $FFF08080),
    (ColorName: 'lightcyan'; ColorValue: $FFE0FFFF),
    (ColorName: 'lightgoldenrodyellow'; ColorValue: $FFFAFAD2),
    (ColorName: 'lightgray'; ColorValue: $FFD3D3D3),
    (ColorName: 'lightgreen'; ColorValue: $FF90EE90),
    (ColorName: 'lightpink'; ColorValue: $FFFFB6C1),
    (ColorName: 'lightsalmon'; ColorValue: $FFFFA07A),
    (ColorName: 'lightseagreen'; ColorValue: $FF20B2AA),
    (ColorName: 'lightskyblue'; ColorValue: $FF87CEFA),
    (ColorName: 'lightslategray'; ColorValue: $FF778899),
    (ColorName: 'lightslategrey'; ColorValue: $FF778899),
    (ColorName: 'lightsteelblue'; ColorValue: $FFB0C4DE),
    (ColorName: 'lightyellow'; ColorValue: $FFFFFFE0),
    (ColorName: 'lime'; ColorValue: $FF00FF00),
    (ColorName: 'limegreen'; ColorValue: $FF32CD32),
    (ColorName: 'linen'; ColorValue: $FFFAF0E6),
    (ColorName: 'magenta'; ColorValue: $FFFF00FF),
    (ColorName: 'maroon'; ColorValue: $FF800000),
    (ColorName: 'mediumaquamarine'; ColorValue: $FF66CDAA),
    (ColorName: 'mediumblue'; ColorValue: $FF0000CD),
    (ColorName: 'mediumorchid'; ColorValue: $FFBA55D3),
    (ColorName: 'mediumpurple'; ColorValue: $FF9370DB),
    (ColorName: 'mediumseagreen'; ColorValue: $FF3CB371),
    (ColorName: 'mediumslateblue'; ColorValue: $FF7B68EE),
    (ColorName: 'mediumspringgreen'; ColorValue: $FF00FA9A),
    (ColorName: 'mediumturquoise'; ColorValue: $FF48D1CC),
    (ColorName: 'mediumvioletred'; ColorValue: $FFC71585),
    (ColorName: 'midnightblue'; ColorValue: $FF191970),
    (ColorName: 'mintcream'; ColorValue: $FFF5FFFA),
    (ColorName: 'mistyrose'; ColorValue: $FFFFE4E1),
    (ColorName: 'moccasin'; ColorValue: $FFFFE4B5),
    (ColorName: 'navajowhite'; ColorValue: $FFFFDEAD),
    (ColorName: 'navy'; ColorValue: $FF000080),
    (ColorName: 'none'; ColorValue: $0),
    (ColorName: 'oldlace'; ColorValue: $FFFDF5E6),
    (ColorName: 'olive'; ColorValue: $FF808000),
    (ColorName: 'olivedrab'; ColorValue: $FF6B8E23),
    (ColorName: 'orange'; ColorValue: $FFFFA500),
    (ColorName: 'orangered'; ColorValue: $FFFF4500),
    (ColorName: 'orchid'; ColorValue: $FFDA70D6),
    (ColorName: 'palegoldenrod'; ColorValue: $FFEEE8AA),
    (ColorName: 'palegreen'; ColorValue: $FF98FB98),
    (ColorName: 'paleturquoise'; ColorValue: $FFAFEEEE),
    (ColorName: 'palevioletred'; ColorValue: $FFDB7093),
    (ColorName: 'papayawhip'; ColorValue: $FFFFEFD5),
    (ColorName: 'peachpuff'; ColorValue: $FFFFDAB9),
    (ColorName: 'peru'; ColorValue: $FFCD853F),
    (ColorName: 'pink'; ColorValue: $FFFFC0CB),
    (ColorName: 'plum'; ColorValue: $FFDDA0DD),
    (ColorName: 'powderblue'; ColorValue: $FFB0E0E6),
    (ColorName: 'purple'; ColorValue: $FF800080),
    (ColorName: 'red'; ColorValue: $FFFF0000),
    (ColorName: 'rosybrown'; ColorValue: $FFBC8F8F),
    (ColorName: 'royalblue'; ColorValue: $FF4169E1),
    (ColorName: 'saddlebrown'; ColorValue: $FF8B4513),
    (ColorName: 'salmon'; ColorValue: $FFFA8072),
    (ColorName: 'sandybrown'; ColorValue: $FFF4A460),
    (ColorName: 'seagreen'; ColorValue: $FF2E8B57),
    (ColorName: 'seashell'; ColorValue: $FFFFF5EE),
    (ColorName: 'sienna'; ColorValue: $FFA0522D),
    (ColorName: 'silver'; ColorValue: $FFC0C0C0),
    (ColorName: 'skyblue'; ColorValue: $FF87CEEB),
    (ColorName: 'slateblue'; ColorValue: $FF6A5ACD),
    (ColorName: 'slategray'; ColorValue: $FF708090),
    (ColorName: 'springgreen'; ColorValue: $FF00FF7F),
    (ColorName: 'steelblue'; ColorValue: $FF4682B4),
    (ColorName: 'tan'; ColorValue: $FFD2B48C),
    (ColorName: 'teal'; ColorValue: $FF008080),
    (ColorName: 'thistle'; ColorValue: $FFD8BFD8),
    (ColorName: 'tomato'; ColorValue: $FFFF6347),
    (ColorName: 'turquoise'; ColorValue: $FF40E0D0),
    (ColorName: 'violet'; ColorValue: $FFEE82EE),
    (ColorName: 'wheat'; ColorValue: $FFF5DEB3),
    (ColorName: 'white'; ColorValue: $FFFFFFFF),
    (ColorName: 'whitesmoke'; ColorValue: $FFF5F5F5),
    (ColorName: 'yellow'; ColorValue: $FFFFFF00),
    (ColorName: 'yellowgreen'; ColorValue: $FF9ACD32));

  buffSize = 32;

  defaultDrawInfo: TDrawInfo =
    (fillColor: clBlack32; fillRule: frNonZero;
    strokeColor: clNone32; strokeWidth: 1.0;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); inDefs: false);

  //hashed string constants
  hCircle                 = $C6AE4953;    // circle
  hDefs                   = $A819FE18;    // defs
  hEllipse                = $30C7994A;    // ellipse
  hGradienttransform      = $5B6A9CF7;    // gradienttransform
  hHeight                 = $52FDF336;    // height
  hHref                   = $8E926F4B;    // href
  hId                     = $1B60404D;    // id
  hLine                   = $7A6D718F;    // line
  hLineargradient         = $C16E9BCD;    // lineargradient
  hMatrix                 = $31E3AD46;    // matrix
  hOffset                 = $A13C79EF;    // offset
  hPath                   = $AD8A7AB5;    // path
  hPolygon                = $509667FD;    // polygon
  hPolyline               = $58B3D280;    // polyline
  hRect                   = $FA3BCBEA;    // rect
  hStop                   = $930CA7F2;    // stop
  hStopColor              = $9FD6E0BE;    // stop-color
  hStopOpacity            = $FC7751F2;    // stop-opacity
  hSvg                    = $ED67C37E;    // svg
  hUrl                    = $BC113F8C;    // url
  hUse                    = $65736EEA;    // use
  hWidth                  = $96BEECA0;    // width
  hX                      = $9303A5E5;    // x
  hX1                     = $74A7DE27;    // x1
  hX2                     = $168E21F1;    // x2
  hXlinkHref              = $2BDA798B;    // xlink:href
  hY                      = $80950108;    // y
  hY1                     = $7F247680;    // y1
  hY2                     = $6DE3D3FF;    // y2

var
  ColorConstList : TStringList;
  AttribFuncList : TStringList;
  LowerCaseTable : array[#0..#255] of AnsiChar;

//------------------------------------------------------------------------------
//Miscellaneous functions ...
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

function AnsiLowercase(pName: PAnsiChar; len: integer): AnsiString;
var
  i: integer;
begin
  SetLength(Result, len);
  Move(pName^, Result[1], len);
  for i := 1 to len do
    Result[i] := LowerCaseTable[Result[i]];
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

function AttribToColor32(const attrib: TAttrib; var color: TColor32): Boolean;
var
  i: integer;
  c: TColor32;
  alpha: Byte;
begin
  Result := false;
  if (attrib.valueLen < 3) then Exit;
  alpha := color shr 24;

  if (attrib.value[0] = '#') then
  begin
    if (attrib.valueLen <> 7) then Exit;
    c := $0;
    for i := 1 to 6 do
      if (attrib.value[i] >= 'a') and (attrib.value[i] <= 'f') then
        c := c *16 + 10 + Ord(attrib.value[i]) - Ord('a')
      else if (attrib.value[i] >= 'A') and (attrib.value[i] <= 'F') then
        c := c *16 + 10 + Ord(attrib.value[i]) - Ord('A')
      else if (attrib.value[i] >= '0') and (attrib.value[i] <= '9') then
        c := c *16 + Ord(attrib.value[i]) - Ord('0')
      else Exit;
    c := c or $FF000000;
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

function AttribToFloat(const attrib: TAttrib; var value: double): Boolean;
var
  i, decPos: integer;
  isNeg, inExponent: Boolean;
  val: double;
begin
  Result := False;
  if (attrib.valueLen = 0) then Exit;

  decPos := -1;
  inExponent := false;
  isNeg := Ord(attrib.value[0]) = Ord('-');
  if isNeg then i := 1 else i := 0;

  val := 0;
  for i := i to attrib.valueLen -1 do
  begin
    if attrib.value[i] = '.' then
    begin
      if decPos >= 0 then Exit; //error
      decPos := 0;
    end
    else if (LowerCaseTable[attrib.value[i]] = 'e') then
    begin
      if inExponent then Break;
      break;// todo - inExponent := true;
    end
    else if (attrib.value[i] < '0') or (attrib.value[i] > '9') then
      break
    else
    begin
      val := val *10 + Ord(attrib.value[i]) - Ord('0');
      if decPos >= 0 then inc(decPos);
    end;
  end;
  if decPos > 0 then val := Power10(val, -decPos);
  if isNeg then val := -val;

  Result := true;
  value := val;
end;
//------------------------------------------------------------------------------

procedure AttribToOpacity(const attrib: TAttrib; var color: TColor32);
var
  opacity: double;
begin
  if not AttribToFloat(attrib, opacity) then Exit;
  if (opacity < 0) or (opacity > 1) then Exit;
  color := (color and $FFFFFF) or (Round(255 * opacity) shl 24);
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

function PeekNum(current, stop: PAnsiChar; ignoreComma: Boolean): Boolean;
begin
  Result := false;

  //skip white space +/- single comma
  if ignoreComma then
    while (current < stop) and ((current^ <= ' ') or (current^ = ',')) do
      inc(current);
  while (current < stop) and (current^ <= ' ') do
    inc(current);
  if (current = stop) then Exit;

  if (current^ = '-') then inc(current);
  if (current^ = '.') then inc(current);
  Result := (current < stop) and (current^ >= '0') and (current^ <= '9');
end;
//------------------------------------------------------------------------------

function GetNum(var current: PAnsiChar; stop: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean;
var
  loopCnt, decPos: integer;
  isNeg: Boolean;
begin
  Result := false;

  //skip white space +/- single comma
  if skipComma then while (current < stop) and
    ((current^ <= #32) or (current^ = ',')) do inc(current);

  while (current < stop) and (current^ <= ' ') do inc(current);
  if (current = stop) then Exit;

  isNeg := (current^ = '-');
  if isNeg then inc(current);
  if (current^ = '.') then
  begin
    decpos  := 0;
    loopCnt := 2;
    inc(current);
  end else
  begin
    decpos  := -1;
    loopCnt := 0;
  end;

  Result := (current < stop) and (current^ >= '0') and (current^ <= '9');
  if not Result then Exit;

  val := Ord(current^) - Ord('0');
  inc(current);
  while (current < stop) do
  begin
    if (current^ = '.') then
    begin
        if (decpos >= 0) then break;
        decpos := loopCnt;
    end
    else if (val = 0) and (decpos < 0) then break
    else if (current^ < '0') or (current^ > '9') then Break
    else val := val * 10 + Ord(current^) - Ord('0');
    inc(current); inc(loopCnt);
  end;
  if (decpos >= 0) then
  begin
      decpos := loopCnt - decpos -1;
      val := val * power(10, -decpos);
  end;
  if (isNeg) then val := -val;
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

constructor TUseElement.Create(parent: TElement; hashName: Cardinal);
var
  attrib: PAttrib;
  //savedCurrent: PAnsiChar;
  ref: AnsiString;
  refEl, clonedRef: TElement;
  i, attribCnt: integer;
  nameHash: Cardinal;
  hashList: array of Cardinal;

   function HashFoundInList(hash: Cardinal; listLength: integer): Boolean;
   var
     i: integer;
   begin
     Result := true;
     for i := 0 to listLength -1 do
       if hashList[i] = hash then Exit;
     Result := false;
   end;

begin
  inherited;

  attrib := FindAttribute(hHref);
  if not assigned(attrib) then
    attrib := FindAttribute(hXlinkHref);
  if not assigned(attrib) or (attrib.value[0] <> '#') then Exit;
  setLength(ref, attrib.valueLen);
  ref := AnsiLowercase(attrib.value, attrib.valueLen);
  refEl := FindReferencedElement(ref);
  if not Assigned(refEl) then Exit;

  fParent.fCurrent := refEl.fName; //an element start with its name
  clonedRef := fParent.LoadChildAndAttribs;

  SetLength(hashList, Length(clonedRef.fAttribs) + Length(fAttribs));

  attribCnt := 0;
  for i := High(clonedRef.fAttribs) downto 0 do
    with clonedRef.fAttribs[i] do
    begin
      nameHash := GetHashedName(aname, nameLen);
      if (nameHash = hId) or (nameHash = hHref) or
        (nameHash = hXlinkHref) then Delete(clonedRef.fAttribs, i, 1)
      else
      begin
        hashList[attribCnt] := nameHash;
        inc(attribCnt);
      end;
    end;

  //and now append valid 'use' attributes
  SetLength(clonedRef.fAttribs, attribCnt + Length(fAttribs));
  for i := 0 to High(fAttribs) do
  begin
    with fAttribs[i] do
      nameHash := GetHashedName(aname, nameLen);
      if HashFoundInList(nameHash, attribCnt) then
        case nameHash of
          //only x, y, width & height are allowed to *replace* attributes
          hX, hY, hWidth, hHeight:; //OK :)
          else Continue;
        end;

    clonedRef.fAttribs[attribCnt].aname := fAttribs[i].aname;
    clonedRef.fAttribs[attribCnt].nameLen := fAttribs[i].nameLen;
    clonedRef.fAttribs[attribCnt].value := fAttribs[i].value;
    clonedRef.fAttribs[attribCnt].valueLen := fAttribs[i].valueLen;
    clonedRef.fAttribs[attribCnt].ownerEl := clonedRef;

    ProcessAttrib(clonedRef.fAttribs[attribCnt]);
    inc(attribCnt);
  end;
  SetLength(clonedRef.fAttribs, attribCnt);

  //it's now safe to unload <USE>, assuming it has no content
  Self.fIsValid := fHasContent;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TShapeElement
//------------------------------------------------------------------------------

constructor TShapeElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fReader.fShapesList.Add(Self);
  ParseAttributes;
  isClosed := true;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.ParseAttributes;
begin
  //do in descendant classes
end;
//------------------------------------------------------------------------------

procedure TShapeElement.ParseAttrib(const attrib: TAttrib);
begin
  //do in descendant classes
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
  Result := 1;
end;
//------------------------------------------------------------------------------

function TShapeElement.GetFlattenedPath(index: integer;
  out path: TPathD; out isClosed: Boolean): Boolean;
begin
  Result := index = 0;
  if not Result then Exit;
  path := CopyPath(self.path);
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

procedure TPathElement.ParseAttrib(const attrib: TAttrib);
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
        SetLength(currSeg.vals, currSegCnt); //trim buffer
      currSeg := nil;
      if not Get2Num(currPt, isRelative) then break;
      self.lastPt :=  currPt;

      //values immediately following a Move are implicitly Line statements
      if PeekNum(fCurrent, fCurrentEnd, true) then
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
        while PeekNum(fCurrent, fCurrentEnd, true) do
        begin
          GetVal(currPt.X);
          if isRelative then
            currPt.X := currPt.X + lastPt.X;
          AddSegValue(currPt.X);
        end;

      dsVert:
        while PeekNum(fCurrent, fCurrentEnd, true) do
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
          if PeekNum(fCurrent, fCurrentEnd, true) then Continue;
          if LowerCaseTable[fCurrent^] = 'l' then GetSegType(isRelative)
          else break;
        end;

      dsQSpline:
        while PeekNum(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCSpline:
        while PeekNum(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsQBez:
        while PeekNum(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCBez:
        while PeekNum(fCurrent, fCurrentEnd, true) do
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
        while PeekNum(fCurrent, fCurrentEnd, true) do
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

function TPathElement.GetFlattenedPath(index: integer;
  out path: TPathD; out isClosed: Boolean): Boolean;
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

procedure TPolyElement.ParseAttrib(const attrib: TAttrib);
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
  SetLength(path, currCap);
  fCurrent    := attrib.value;
  fCurrentEnd := fCurrent + attrib.valueLen;
  while GetVal(pt.X) and GetVal(pt.Y) do AddPoint(pt);
  SetLength(path, currCnt);
end;

//------------------------------------------------------------------------------
// TLineElement
//------------------------------------------------------------------------------

procedure TLineElement.ParseAttributes;
var
  attrib: PAttrib;
begin
  SetLength(path, 2);
  path[0] := NullPointD; path[1] := NullPointD;

  attrib := FindAttribute(hX1);
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, path[0].X);
  attrib := FindAttribute(hY1);
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, path[0].Y);
  attrib := FindAttribute(hX2);
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, path[1].X);
  attrib := FindAttribute(hY2);
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, path[1].Y);
end;

//------------------------------------------------------------------------------
// TCircleElement
//------------------------------------------------------------------------------

procedure TCircleElement.ParseAttributes;
var
  centerPt: TPointD;
  radius: double;
  attrib: PAttrib;
begin
  centerPt := NullPointD;

  attrib := FindAttribute('cx');
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, centerPt.X);
  attrib := FindAttribute('cy');
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, centerPt.Y);
  attrib := FindAttribute('r');
  if not Assigned(attrib) then Exit;
  with attrib^ do
    GetNum(value, value + valueLen, false, radius);

  with centerPt do
    path := Ellipse(RectD(X -radius, Y -radius, X +radius, Y +radius));

end;

//------------------------------------------------------------------------------
// TEllipseElement
//------------------------------------------------------------------------------

procedure TEllipseElement.ParseAttributes;
var
  centerPt, radii: TPointD;
  attrib: PAttrib;
begin
  centerPt := NullPointD;

  attrib := FindAttribute('cx');
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, centerPt.X);
  attrib := FindAttribute('cy');
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, centerPt.Y);

  attrib := FindAttribute('rx');
  if not Assigned(attrib) then Exit;
  with attrib^ do
    GetNum(value, value + valueLen, false, radii.X);

  attrib := FindAttribute('ry');
  if not Assigned(attrib) then Exit;
  with attrib^ do
    GetNum(value, value + valueLen, false, radii.Y);

  with centerPt do
    path := Ellipse(RectD(X -radii.X, Y -radii.Y, X +radii.X, Y +radii.Y));
end;

//------------------------------------------------------------------------------
// TRectElement
//------------------------------------------------------------------------------

procedure TRectElement.ParseAttributes;
var
  rec: TRectD;
  radii: TPointD;
  attrib: PAttrib;
begin
  rec := NullRectD;
  radii := NullPointD;

  attrib := FindAttribute(hX);
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, rec.Left);
  attrib := FindAttribute(hY);
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, rec.Top);

  attrib := FindAttribute(hWidth);
  if not Assigned(attrib) then Exit;
  with attrib^ do
    GetNum(value, value + valueLen, false, rec.Right);

  attrib := FindAttribute(hHeight);
  if not Assigned(attrib) then Exit;
  with attrib^ do
    GetNum(value, value + valueLen, false, rec.Bottom);

  rec.Right := rec.Right + rec.Left;
  rec.Bottom := rec.Bottom + rec.Top;

  attrib := FindAttribute('rx');
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, radii.X);
  attrib := FindAttribute('ry');
  if Assigned(attrib) then
    with attrib^ do
      GetNum(value, value + valueLen, false, radii.Y)
  else
    radii.Y := radii.X;

  if (radii.X <> 0) or (radii.Y <> 0) then
    path := RoundRect(rec, Average(radii.X, radii.Y)) else
    path := Rectangle(rec);
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

constructor TElement.Create(parent: TElement;
  hashName: Cardinal);
var
  i, nameLen: integer;
begin
  fChilds    := TList.Create;
  fNameHash  := hashName;
  fIsValid   := false;

  if not Assigned(parent) then Exit; //svg root element

  self.fParent      := parent;
  self.fReader      := parent.fReader;
  self.fCurrentEnd  := fReader.fEndStream;
  self.fDrawInfo    := parent.fDrawInfo;
  self.fCurrent     := parent.fCurrent;
  if fCurrent^ = '>' then inc(fCurrent);

  if not SkipBlanks(fCurrent, fCurrentEnd) or
    not GetElemOrAttribName(fName, nameLen) then Exit;

  fDrawInfo.inDefs := fDrawInfo.inDefs or (fNameHash = hDefs);

  if not LoadAttributes then Exit;
  for i := 0 to High(fAttribs) do
    ProcessAttrib(fAttribs[i]);
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

function TElement.LoadAttributes: Boolean;
var
  attribCnt: integer;
  attrib: PAttrib;
begin
  Result := false;
  fHasContent := true;
  attribCnt := 0;
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
      Exit;
    end;

    inc(attribCnt);
    setLength(fAttribs, attribCnt);
    attrib := @fAttribs[attribCnt-1];
    attrib.ownerEl := self;

    GetElemOrAttribName(attrib.aname, attrib.nameLen);
    if fCurrent^ <> '=' then Exit; inc(fCurrent);
    GetAttribValue(attrib.value, attrib.valueLen);
    if fCurrent^ <> '"' then Exit; inc(fCurrent);
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

function TElement.FindReferencedElement(const ref: AnsiString): TElement;
var
  i: integer;
begin
  i := fReader.fIdList.IndexOf(string(ref));
  if i >= 0 then
    Result := TElement(fReader.fIdList.Objects[i]) else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TElement.LoadChildAndAttribs: TElement;
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
    hCircle   : Result := TCircleElement.Create(self, hashedName);
    hEllipse  : Result := TEllipseElement.Create(self, hashedName);
    hPath     : Result := TPathElement.Create(self, hashedName);
    hPolyline : Result := TPolyElement.Create(self, hashedName, false);
    hPolygon  : Result := TPolyElement.Create(self, hashedName, true);
    hRect     : Result := TRectElement.Create(self, hashedName);
    hUse      : Result := TUseElement.Create(self, hashedName);
    else        Result := TElement.Create(self, hashedName);
  end;
  fCurrent := Result.fCurrent;
  if Result.fIsValid then
    fChilds.Add(Result) else
    FreeAndNil(Result);
end;
//------------------------------------------------------------------------------

procedure TElement.LoadContent;
var
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
          if (fCurrent^ = '>') and ((fCurrent -1)^ = '-') and  ((fCurrent -2)^ = '-') then
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
        el := LoadChildAndAttribs;
        if Assigned(el) and el.fHasContent then
        begin
          el.LoadContent;
          fCurrent := el.fCurrent;
        end;
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
  fIdList.Sorted  := True;
  fGradientsList  := TStringList.Create;
  fGradientsList.Sorted  := True;
end;
//------------------------------------------------------------------------------

destructor TSvgReader.Destroy;
begin
  Clear;
  fMemStream.Free;
  fShapesList.Free;
  fIdList.Free;
  fGradientsList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.Clear;
begin
  FreeAndNil(fRootElement);
  fShapesList.Clear;
  fGradientsList.Clear;
  fIdList.Clear;
  fViewbox := NullRectD;
  fEndStream := nil;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.DrawImage(img: TImage32; scaleToImage: Boolean);
var
  i,j: integer;
  rec: TRect;
  sx, sy: double;
  p: TPathD;
  closedPP, openPP: TPathsD;
const
  endStyle: array[boolean] of TEndStyle = (esPolygon, esRound);
begin
  if not Assigned(fRootElement) or fViewbox.IsEmpty then Exit;

  rec := Rect(fViewbox);

  if scaleToImage and not img.IsEmpty then
  begin
    sx := img.width / fViewbox.Width;
    sy := img.height / fViewbox.Height;
    if sy <= sx then sx := sy;
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
      for j := 0 to PathCount -1 do
      begin
        GetFlattenedPath(j, p, isClosed);
        if isClosed then
          AppendPath(closedPP, p) else
          AppendPath(openPP, p);
      end;

      //nb: apply the matrix before offsetting
      if not IsIdentityMatrix(DrawInfo.matrix) then
      begin
        MatrixApply(DrawInfo.matrix, openPP);
        MatrixApply(DrawInfo.matrix, closedPP);
      end;

      if Assigned(closedPP) then
      begin
        closedPP := OffsetPath(closedPP, -fViewbox.Left, -fViewbox.Top);
        if sx <> 1.0 then
          closedPP := ScalePath(closedPP, sx);

        if (DrawInfo.fillColor <> clNone32) then
          DrawPolygon(img, closedPP, DrawInfo.fillRule,  DrawInfo.fillColor);

        if (DrawInfo.strokeColor <> clNone32) and
          (DrawInfo.strokeWidth <> 0) then
            DrawLine(img, closedPP, DrawInfo.strokeWidth * sx,
              DrawInfo.strokeColor, esPolygon);
      end;

      if Assigned(openPP) and
        (DrawInfo.strokeColor <> clNone32) and
        (DrawInfo.strokeWidth <> 0.0) then
      begin
        openPP := OffsetPath(openPP, -fViewbox.Left, -fViewbox.Top);
        if sx <> 1.0 then
          openPP := ScalePath(openPP, sx);
        DrawLine(img, openPP, DrawInfo.strokeWidth * sx,
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

procedure Id(const attrib: TAttrib);
var
  id: AnsiString;
begin
  with attrib do
  begin
    id := AnsiLowercase(value, valueLen);
    ownerEl.fReader.fIdList.AddObject('#' + string(id), ownerEl);
  end;
end;
//------------------------------------------------------------------------------

procedure Href(const attrib: TAttrib);
begin
end;
//------------------------------------------------------------------------------

procedure D(const attrib: TAttrib);
begin
  if attrib.ownerEl is TPathElement then
    TPathElement(attrib.ownerEl).ParseAttrib(attrib);
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

procedure Points(const attrib: TAttrib);
begin
  if attrib.ownerEl is TPolyElement then
    TPolyElement(attrib.ownerEl).ParseAttrib(attrib);
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
  attribCnt, nameLen: integer;
  newAt: PAttrib;
  element: TElement;
  current, endC, name: PAnsiChar;
  c: AnsiChar;
begin
  element := attrib.ownerEl;
  attribCnt := Length(element.fAttribs);
  current := attrib.value;
  endC := current + attrib.valueLen;

  while SkipBlanks(current, endC) do
  begin
    name := current;
    nameLen := GetStyleNameLen(current, endC);
    if nameLen = 0 then Break;

    inc(attribCnt);
    SetLength(element.fAttribs, attribCnt);
    newAt := @element.fAttribs[attribCnt-1];
    newAt.ownerEl := element;

    newAt.aname := name;
    newAt.nameLen := nameLen;

    if not GetChar(current, endC, c) or (c <> ':') or  //syntax check
      not SkipBlanks(current,endC) then Break;
    newAt.value := current;
    inc(current);
    while (current < endC) and (current^ <> ';') do inc(current);
    newAt.valueLen := current - newAt.value;
    inc(current);
    element.ProcessAttrib(newAt^);
  end;
end;
//------------------------------------------------------------------------------

procedure Transform(const attrib: TAttrib);
var
  i: integer;
  word: AnsiString;
  values: array[0..5] of double;
  current, endC: PAnsiChar;
  c: AnsiChar;
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
  //surprisingly, and I think this is a bug in SVG not a feature ...
  //transform operations must be performed in reverse order
  current := attrib.value;
  endC := current + attrib.valueLen;

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
    while (i < 6) and PeekNum(current, endC, true) do
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
  with attrib.ownerEl do
    for i := High(mats) downto 0 do
      fDrawInfo.matrix := MatrixMultiply(mats[i], fDrawInfo.matrix);
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

  //the attributes here don't need to know much about their parent element
  RegisterAttribute('d',                  D);
  RegisterAttribute('fill',               Fill);
  RegisterAttribute('fill-opacity',       FillOpacity);
  RegisterAttribute('fill-fule',          FillRule);
  RegisterAttribute(hId,                  Id);
  //RegisterAttribute(hHref,                Href);
  RegisterAttribute('points',             Points);
  RegisterAttribute('stroke',             Stroke);
  RegisterAttribute('stroke-opacity',     StrokeOpacity);
  RegisterAttribute('stroke-width',       StrokeWidth);
  RegisterAttribute('style',              Style);
  RegisterAttribute('transform',          Transform);
  //RegisterAttribute(hXlinkHref,           Href);

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
