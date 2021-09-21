unit Img32.SVG.Core;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.3                                                             *
* Date      :  21 September 2021                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Essential structures and functions to read SVG files            *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Types, Math,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.Vector, Img32.Text, Img32.Transform;

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

type
  TTriState = (tsUnknown, tsYes, tsNo);
  TSvgEncoding = (eUnknown, eUtf8, eUnicodeLE, eUnicodeBE);

  TUnitType = (utUnknown, utNumber, utPercent, utEm, utEx, utPixel,
    utCm, utMm, utInch, utPt, utPica, utDegree, utRadian);

  //////////////////////////////////////////////////////////////////////
  // TValue - Structure to store numerics with measurement units.
  // See https://www.w3.org/TR/SVG/types.html#InterfaceSVGLength
  // and https://www.w3.org/TR/SVG/types.html#InterfaceSVGAngle
  //////////////////////////////////////////////////////////////////////

  //Unfortunately unit-less values can exhibit ambiguity, especially when their
  //values are small (eg < 1.0). These values can be either absolute values or
  //relative values (ie relative to the supplied dimension size).
  //The 'assumeRelValBelow' parameter (see below) attempts to address this
  //ambiguity, such that unit-less values will be assumed to be 'relative' when
  //'rawVal' is less than the supplied 'assumeRelValBelow' value.

  TValue = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    rawVal    : double;
    unitType  : TUnitType;
    procedure Init;
    procedure SetValue(val: double; unitTyp: TUnitType = utNumber);
    function  GetValue(relSize: double; assumeRelValBelow: Double): double;
    function  GetValueXY(const relSize: TRectD; assumeRelValBelow: Double): double;
    function  IsValid: Boolean;
    function  IsRelativeValue(assumeRelValBelow: double): Boolean;
      {$IFDEF INLINE} inline; {$ENDIF}
    function  HasFontUnits: Boolean;
    function  HasAngleUnits: Boolean;
  end;

  TValuePt = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    X, Y    : TValue;
    procedure Init;
    function  GetPoint(const relSize: double; assumeRelValBelow: Double): TPointD; overload;
    function  GetPoint(const relSize: TRectD; assumeRelValBelow: Double): TPointD; overload;
    function  IsValid: Boolean;
  end;

  TValueRecWH = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    left    : TValue;
    top     : TValue;
    width   : TValue;
    height  : TValue;
    procedure Init;
    function  GetRectD(const relSize: TRectD; assumeRelValBelow: Double): TRectD; overload;
    function  GetRectD(relSize: double; assumeRelValBelow: Double): TRectD; overload;
    function  GetRectWH(const relSize: TRectD; assumeRelValBelow: Double): TRectWH;
    function  IsValid: Boolean;
    function  IsEmpty: Boolean;
  end;

  {$IFNDEF UNICODE}
  UTF8Char  = Char;
  PUTF8Char = PChar;
  {$ELSE}
  {$IF COMPILERVERSION < 31}
  UTF8Char = AnsiChar;
  PUTF8Char = PAnsiChar;
  {$IFEND}
  {$ENDIF}

  TSvgItalicSyle  = (sfsUndefined, sfsNone, sfsItalic);
  TFontDecoration = (fdUndefined, fdNone, fdUnderline, fdStrikeThrough);
  TSvgTextAlign = (staUndefined, staLeft, staCenter, staRight);

  TSVGFontInfo = record
    family      : TTtfFontFamily;
    size        : double;
    spacing     : double;
    textLength  : double;
    italic      : TSvgItalicSyle;
    weight      : Integer;
    align       : TSvgTextAlign;
    decoration  : TFontDecoration;
    baseShift   : TValue;
  end;

  //////////////////////////////////////////////////////////////////////
  // TClassStylesList: custom TStringList that stores ansistring objects
  //////////////////////////////////////////////////////////////////////

  PAnsStringiRec = ^TAnsiStringRec;   //used internally by TClassStylesList
  TAnsiStringRec = record
    ansi  : UTF8String;
  end;

  TClassStylesList = class
  private
    fList : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function  AddAppendStyle(const classname: string; const ansi: UTF8String): integer;
    function  GetStyle(const classname: UTF8String): UTF8String;
    procedure Clear;
  end;

  //////////////////////////////////////////////////////////////////////
  // TSvgParser and associated classes - a simple parser for SVG xml
  //////////////////////////////////////////////////////////////////////

  PSvgAttrib = ^TSvgAttrib;   //element attribute
  TSvgAttrib = record
    hash      : Cardinal;     //hashed name
    name      : UTF8String;
    value     : UTF8String;
  end;

  TSvgParser = class;

  TXmlEl = class              //base element class
  public
    name        : UTF8String;
    {$IFDEF XPLAT_GENERICS}
    attribs     : TList <PSvgAttrib>;
    {$ELSE}
    attribs     : TList;
    {$ENDIF}
    owner       : TSvgParser;
    selfClosed  : Boolean;
    constructor Create(owner: TSvgParser); virtual;
    destructor  Destroy; override;
    procedure   Clear; virtual;
    function    ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean; virtual;
    function    ParseAttribName(var c: PUTF8Char; endC: PUTF8Char; attrib: PSvgAttrib): Boolean;
    function    ParseAttribValue(var c: PUTF8Char; endC: PUTF8Char; attrib: PSvgAttrib): Boolean;
    function    ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean; virtual;
    procedure   ParseStyleAttribute(const style: UTF8String);
  end;

  TDocTypeEl = class(TXmlEl)
  private
    procedure   SkipWord(var c, endC: PUTF8Char);
    function    ParseEntities(var c, endC: PUTF8Char): Boolean;
  public
    function    ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean; override;
  end;

  TSvgTreeEl = class(TXmlEl)
  public
    hash        : Cardinal;
    text        : UTF8String;
    {$IFDEF XPLAT_GENERICS}
    childs      : TList<TSvgTreeEl>;
    {$ELSE}
    childs      : TList;
    {$ENDIF}
    constructor Create(owner: TSvgParser); override;
    destructor  Destroy; override;
    procedure   Clear; override;
    function    ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean; override;
    function    ParseContent(var c: PUTF8Char; endC: PUTF8Char): Boolean; virtual;
  end;

  TSvgParser = class
  private
    svgStream : TMemoryStream;
    procedure ParseStream;
  public
    classStyles :TClassStylesList;
    xmlHeader   : TXmlEl;
    docType     : TDocTypeEl;
    svgTree     : TSvgTreeEl;
    {$IFDEF XPLAT_GENERICS}
    entities    : TList<TSvgTreeEl>;
    {$ELSE}
    entities    : TList;
    {$ENDIF}
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    function  FindEntity(hash: Cardinal): PSvgAttrib;
    function  LoadFromFile(const filename: string): Boolean;
    function  LoadFromStream(stream: TStream): Boolean;
    function  LoadFromString(const str: string): Boolean;
  end;

  //////////////////////////////////////////////////////////////////////
  //TSvgSubPath structures
  //////////////////////////////////////////////////////////////////////

  TSvgPathSegType = (dsUnknown, dsMove, dsLine, dsHorz, dsVert, dsArc,
    dsQBez, dsCBez, dsQSpline, dsCSpline, dsClose);

  TSvgSubPathSeg = class
  private
    fCnt    : integer;
    fVals   : TArrayOfDouble;
    function GetVal(index: integer): double;
  public
    segType : TSvgPathSegType;
    procedure AddVal(val: double);
    property Count: integer read fCnt;
    property Val[index: integer]: double read GetVal;
  end;

  TSvgSubPath = class
  private
    segs      : array of TSvgSubPathSeg;
    function GetCount: integer;
    function GetSeg(index: integer): TSvgSubPathSeg;
  public
    firstPt   : TPointD;
    destructor Destroy; override;
    procedure Clear;
    function GetBounds: TRectD;
    function AddSeg: TSvgSubPathSeg;
    function DeleteLastSeg: Boolean;
    //scalePending: if an SVG will be scaled later, then this parameter
    //allows curve 'flattening' to occur with a corresponding precision
    function GetFlattenedPath(scalePending: double = 1.0): TPathD;
    //GetSimplePath - ignores curves and is only used with markers
    function GetSimplePath: TPathD;
    function IsClosed: Boolean;
    function Clone: TSvgSubPath;
    property Count: integer read GetCount;
    property seg[index: integer]: TSvgSubPathSeg read GetSeg; default;
  end;

  TSvgPath = class
  private
    fSubPaths: array of TSvgSubPath;
    function GetPath(index: integer): TSvgSubPath;
    function GetCount: integer;
  public
    destructor Destroy; override;
    procedure Clear;
    function AddPath: TSvgSubPath;
    function Clone: TSvgPath;
    property Path[index: integer]: TSvgSubPath read GetPath; default;
    property Count: integer read GetCount;
  end;

  //////////////////////////////////////////////////////////////////////
  // Miscellaneous SVG functions
  //////////////////////////////////////////////////////////////////////

  //general parsing functions //////////////////////////////////////////
  function ParseNextWord(var c: PUTF8Char; endC: PUTF8Char;
    out word: UTF8String): Boolean;
  function ParseNextWordEx(var c: PUTF8Char; endC: PUTF8Char;
    out word: UTF8String): Boolean;
  function ParseNextNum(var c: PUTF8Char; endC: PUTF8Char;
    skipComma: Boolean; out val: double): Boolean;
  function ParseNextNumEx(var c: PUTF8Char; endC: PUTF8Char; skipComma: Boolean;
    out val: double; out unitType: TUnitType): Boolean;
  function GetHash(const name: UTF8String): cardinal;
  function GetHashCaseSensitive(name: PUTF8Char; nameLen: integer): cardinal;
  function ExtractRef(const href: UTF8String): UTF8String;
  function IsNumPending(var c: PUTF8Char;
    endC: PUTF8Char; ignoreComma: Boolean): Boolean;
  function UTF8StringToColor32(const value: UTF8String; var color: TColor32): Boolean;
  function MakeDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfInteger;
  function Match(c: PUTF8Char; const compare: UTF8String): Boolean; overload;
  function Match(const compare1, compare2: UTF8String): Boolean; overload;
  function ToUTF8String(var c: PUTF8Char; endC: PUTF8Char): UTF8String;
  function Base64Decode(const str: UTF8String): UTF8String;

  //special parsing functions //////////////////////////////////////////
  procedure ParseSvgPath(const value: UTF8String; svgPaths: TSvgPath);
  procedure ParseStyleElementContent(const value: UTF8String; stylesList: TClassStylesList);
  function ParseTransform(const transform: UTF8String): TMatrixD;

  procedure GetSvgFontInfo(const value: UTF8String; var fontInfo: TSVGFontInfo);
  function GetSvgArcInfo(const p1, p2: TPointD; radii: TPointD; phi_rads: double;
    fA, fS: boolean; out startAngle, endAngle: double; out rec: TRectD): Boolean;
  function HtmlDecode(const html: UTF8String): UTF8String;

  function GetXmlEncoding(memory: Pointer; len: integer): TSvgEncoding;
  function ClampRange(val, min, max: double): double;

type
  TSetOfUTF8Char = set of UTF8Char;
  UTF8Strings = array of UTF8String;

function CharInSet(chr: UTF8Char; chrs: TSetOfUTF8Char): Boolean;

const
  clInvalid   = $00010001;
  clCurrent   = $00010002;
  sqrt2       = 1.4142135623731;
  quote       = '''';
  dquote      = '"';
  space       = #32;
  SvgDecimalSeparator = '.'; //do not localize

  {$I Img32.SVG.HashConsts.inc}

var
  LowerCaseTable : array[#0..#255] of UTF8Char;
  ColorConstList : TStringList;

implementation

resourcestring
  rsSvgPathRangeError = 'TSvgPath.GetPath range error';
  rsSvgSubPathRangeError = 'TSvgSubPath.GetSeg range error';
  rsSvgSubPathSegRangeError = 'TSvgSubPathSeg.GetVal range error';

type
  TColorConst = record
    ColorName : string;
    ColorValue: Cardinal;
  end;

  TColorObj = class
    cc: TColorConst;
  end;

const
  buffSize    = 32;

  //include hashed html entity constants
  {$I Img32.SVG.HtmlHashConsts.inc}

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function ClampRange(val, min, max: double): double;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  if val <= min then Result := min
  else if val >= max then Result := max
  else Result := val;
end;
//------------------------------------------------------------------------------

function CharInSet(chr: UTF8Char; chrs: TSetOfUTF8Char): Boolean;
begin
  Result := chr in chrs;
end;
//------------------------------------------------------------------------------

function Match(c: PUTF8Char; const compare: UTF8String): Boolean;
var
  i: integer;
begin
  Result := false;
  for i := 1 to Length(compare) do
  begin
    if LowerCaseTable[c^] <> compare[i] then Exit;
    inc(c);
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function Match(const compare1, compare2: UTF8String): Boolean;
var
  i, len: integer;
  c1, c2: PUTF8Char;
begin
  Result := false;
  len := Length(compare1);
  if len <> Length(compare2) then Exit;
  c1 := @compare1[1]; c2 := @compare2[1];
  for i := 1 to len do
  begin
    if LowerCaseTable[c1^] <> LowerCaseTable[c2^] then Exit;
    inc(c1); inc(c2);
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

function Split(const str: UTF8String): UTF8Strings;
var
  i,j,k, spcCnt, len: integer;
begin
  spcCnt := 0;
  i := 1;
  len := Length(str);
  while (len > 0) and (str[len] <= #32) do dec(len);
  while (i <= len) and (str[i] <= #32) do inc(i);
  for j := i + 1 to len do
    if (str[j] <= #32) and (str[j -1] > #32) then inc(spcCnt);
  SetLength(Result, spcCnt +1);
  for k := 0 to spcCnt do
  begin
    j := i;
    while (j <= len) and (str[j] > #32) do inc(j);
    SetLength(Result[k], j -i);
    Move(str[i], Result[k][1], j -i);
    while (j <= len) and (str[j] <= #32) do inc(j);
    i := j;
  end;
end;
//------------------------------------------------------------------------------

function GetXmlEncoding(memory: Pointer; len: integer): TSvgEncoding;
var
  p: PUTF8Char;
begin
  Result := eUnknown;
  if (len < 4) or not Assigned(memory) then Exit;
  p := PUTF8Char(memory);
  case p^ of
    #$EF: if ((p +1)^ = #$BB) and ((p +2)^ = #$BF) then Result := eUtf8;
    #$FF: if ((p +1)^ = #$FE) then Result := eUnicodeLE;
    #$FE: if ((p +1)^ = #$FF) then Result := eUnicodeBE;
  end;
end;
//------------------------------------------------------------------------------

function SkipBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function SkipBlanksAndComma(var current: PUTF8Char; currentEnd: PUTF8Char): Boolean;
begin
  Result := SkipBlanks(current, currentEnd);
  if not Result or (current^ <> ',') then Exit;
  inc(current);
  Result := SkipBlanks(current, currentEnd);
end;
//------------------------------------------------------------------------------

function SkipStyleBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  inComment: Boolean;
begin
  //style content may include multi-line comment blocks
  inComment := false;
  while (c < endC) do
  begin
    if inComment then
    begin
      if (c^ = '*') and ((c +1)^ = '/')  then
      begin
        inComment := false;
        inc(c);
      end;
    end
    else if (c^ > space) then
    begin
      inComment := (c^ = '/') and ((c +1)^ = '*');
      if not inComment then break;
    end;
    inc(c);
  end;
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function IsAlpha(c: UTF8Char): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := CharInSet(c, ['A'..'Z','a'..'z']);
end;
//------------------------------------------------------------------------------

function GetSingleDigit(var c, endC: PUTF8Char;
  out digit: integer): Boolean;
begin
  Result := SkipBlanksAndComma(c, endC) and (c^ >= '0') and (c^ <= '9');
  if not Result then Exit;
  digit := Ord(c^) - Ord('0');
  inc(c);
end;
//------------------------------------------------------------------------------

function ParseStyleNameLen(var c: PUTF8Char; endC: PUTF8Char): integer;
var
  c2: PUTF8Char;
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

  c2 := c; inc(c);
  while (c < endC) and CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - c2;
end;
//------------------------------------------------------------------------------

function ParseNextWord(var c: PUTF8Char; endC: PUTF8Char; out word: UTF8String): Boolean;
var
  c2: PUTF8Char;
begin
  Result := SkipBlanksAndComma(c, endC);
  if not Result then Exit;
  c2 := c;
  while (c < endC) and
    (LowerCaseTable[c^] >= 'a') and (LowerCaseTable[c^] <= 'z') do
      inc(c);
  word := ToUTF8String(c2, c);
end;
//------------------------------------------------------------------------------

function ParseNextWordEx(var c: PUTF8Char; endC: PUTF8Char;
  out word: UTF8String): Boolean;
var
  isQuoted: Boolean;
  c2: PUTF8Char;
begin
  Result := SkipBlanksAndComma(c, endC);
  if not Result then Exit;
  isQuoted := (c^) = quote;
  if isQuoted then
  begin
    inc(c);
    c2 := c;
    while (c < endC) and (c^ <> quote) do inc(c);
    word := ToUTF8String(c2, c);
    inc(c);
  end else
  begin
    Result := CharInSet(LowerCaseTable[c^], ['A'..'Z', 'a'..'z']);
    if not Result then Exit;
    c2 := c;
    inc(c);
    while (c < endC) and
      CharInSet(LowerCaseTable[c^], ['A'..'Z', 'a'..'z', '-', '_']) do inc(c);
    word := ToUTF8String(c2, c);
  end;
end;
//------------------------------------------------------------------------------

function ParseNameLength(var c: PUTF8Char; endC: PUTF8Char): integer; overload;
var
  c2: PUTF8Char;
const
  validNonFirstChars =  ['0'..'9','A'..'Z','a'..'z','_',':','-'];
begin
  c2 := c;
  inc(c);
  while (c < endC) and CharInSet(c^, validNonFirstChars) do inc(c);
  Result := c - c2;
end;
//------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}
function GetHash(const name: UTF8String): cardinal;
var
  i: integer;
  c: PUTF8Char;
begin
  //https://en.wikipedia.org/wiki/Jenkins_hash_function
  c := PUTF8Char(name);
  Result := 0;
  if c = nil then Exit;
  for i := 1 to Length(name) do
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

function GetHashCaseSensitive(name: PUTF8Char; nameLen: integer): cardinal;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to nameLen do
  begin
    Result := (Result + Ord(name^));
    Result := Result + (Result shl 10);
    Result := Result xor (Result shr 6);
    inc(name);
  end;
  Result := Result + (Result shl 3);
  Result := Result xor (Result shr 11);
  Result := Result + (Result shl 15);
end;
{$OVERFLOWCHECKS ON}
//------------------------------------------------------------------------------

function ParseNextWordHashed(var c: PUTF8Char; endC: PUTF8Char): cardinal;
var
  c2: PUTF8Char;
  name: UTF8String;
begin
  c2 := c;
  ParseNameLength(c, endC);
  name := ToUTF8String(c2, c);
  if name = '' then Result := 0
  else Result := GetHash(name);
end;
//------------------------------------------------------------------------------

function ParseNextNumEx(var c: PUTF8Char; endC: PUTF8Char; skipComma: Boolean;
  out val: double; out unitType: TUnitType): Boolean;
var
  decPos,exp: integer;
  isNeg, expIsNeg: Boolean;
  start: PUTF8Char;
begin
  Result := false;
  unitType := utNumber;

  //skip white space +/- single comma
  if skipComma then
  begin
    while (c < endC) and (c^ <= space) do inc(c);
    if (c^ = ',') then inc(c);
  end;
  while (c < endC) and (c^ <= space) do inc(c);
  if (c = endC) then Exit;

  decPos := -1; exp := Invalid; expIsNeg := false;
  isNeg := c^ = '-';
  if isNeg then inc(c);

  val := 0;
  start := c;
  while c < endC do
  begin
    if Ord(c^) = Ord(SvgDecimalSeparator) then
    begin
      if decPos >= 0 then break;
      decPos := 0;
    end
    else if (LowerCaseTable[c^] = 'e') and
      (CharInSet((c+1)^, ['-','0'..'9'])) then
    begin
      if (c +1)^ = '-' then expIsNeg := true;
      inc(c);
      exp := 0;
    end
    else if (c^ < '0') or (c^ > '9') then
      break
    else if IsValid(exp) then
    begin
      exp := exp * 10 + (Ord(c^) - Ord('0'))
    end else
    begin
      val := val *10 + Ord(c^) - Ord('0');
      if decPos >= 0 then inc(decPos);
    end;
    inc(c);
  end;
  Result := c > start;
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
  case c^ of
    '%':
      begin
        inc(c);
        unitType := utPercent;
      end;
    'c': //convert cm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utCm;
      end;
    'd': //ignore deg
      if ((c+1)^ = 'e') and ((c+2)^ = 'g') then
      begin
        inc(c, 3);
        unitType := utDegree;
      end;
    'e': //convert cm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utEm;
      end
      else if ((c+1)^ = 'x') then
      begin
        inc(c, 2);
        unitType := utEx;
      end;
    'i': //convert inchs to pixels
      if ((c+1)^ = 'n') then
      begin
        inc(c, 2);
        unitType := utInch;
      end;
    'm': //convert mm to pixels
      if ((c+1)^ = 'm') then
      begin
        inc(c, 2);
        unitType := utMm;
      end;
    'p':
      case (c+1)^ of
        'c':
          begin
            inc(c, 2);
            unitType := utPica;
          end;
        't':
          begin
            inc(c, 2);
            unitType := utPt;
          end;
        'x':
          begin
            inc(c, 2);
            unitType := utPixel;
          end;
      end;
    'r': //convert radian angles to degrees
      if Match(c, 'rad') then
      begin
        inc(c, 3);
        unitType := utRadian;
      end;
  end;
end;
//------------------------------------------------------------------------------

function ParseNextNum(var c: PUTF8Char; endC: PUTF8Char;
  skipComma: Boolean; out val: double): Boolean;
var
  tmp: TValue;
begin
  tmp.Init;
  Result := ParseNextNumEx(c, endC, skipComma, tmp.rawVal, tmp.unitType);
  val := tmp.GetValue(1, 1);
end;
//------------------------------------------------------------------------------

function ExtractRef(const href: UTF8String): UTF8String; {$IFDEF INLINE} inline; {$ENDIF}
var
  c, c2, endC: PUTF8Char;
begin
  c := PUTF8Char(href);
  endC := c + Length(href);
  if Match(c, 'url(') then
  begin
    inc(c, 4);
    dec(endC); // avoid trailing ')'
  end;
  if c^ = '#' then inc(c);
  c2 := c;
  while (c < endC) and (c^ <> ')') do inc(c);
  Result := ToUTF8String(c2, c);
end;
//------------------------------------------------------------------------------

function ParseNextChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  Result := #0;
  if not SkipBlanks(c, endC) then Exit;
  Result := c^;
  inc(c);
end;
//------------------------------------------------------------------------------

function ParseQuoteChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  if SkipBlanks(c, endC) and (c^ in [quote, dquote]) then
  begin
    Result := c^;
    inc(c);
  end else
    Result := #0;
end;
//------------------------------------------------------------------------------

function AllTrim(var name: UTF8String): Boolean;
var
  i, len: integer;
begin
  len := Length(name);
  i := 0;
  while (len > 0) and (name[1] <= space) do
  begin
    inc(i); dec(len);
  end;
  if i > 0 then Delete(name, 1, i);
  Result := len > 0;
  if not Result then Exit;
  while name[len] <= space do dec(len);
  SetLength(name, len);
end;
//------------------------------------------------------------------------------

function ToUTF8String(var c: PUTF8Char; endC: PUTF8Char): UTF8String;
var
  len: integer;
begin
  len := endC - c;
  SetLength(Result, len);
  if len = 0 then Exit;
  Move(c^, Result[1], len * SizeOf(UTF8Char));
  c := endC;
end;
//------------------------------------------------------------------------------

function IsKnownEntity(owner: TSvgParser;
  var c: PUTF8Char; endC: PUTF8Char; out entity: PSvgAttrib): boolean;
var
  c2, c3: PUTF8Char;
  entityName: UTF8String;
begin
  inc(c); //skip ampersand.
  c2 := c; c3 := c;
  ParseNameLength(c3, endC);
  entityName := ToUTF8String(c2, c3);
  entity := owner.FindEntity(GetHash(entityName));
  Result := (c3^ = ';') and Assigned(entity);
  //nb: increments 'c' only if the entity is found.
  if Result then c := c3 +1 else dec(c);
end;
//------------------------------------------------------------------------------

function ParseQuotedString(var c: PUTF8Char; endC: PUTF8Char;
  out quotStr: UTF8String): Boolean;
var
  quote: UTF8Char;
  c2: PUTF8Char;
begin
  quote := c^;
  inc(c);
  c2 := c;
  while (c < endC) and (c^ <> quote) do inc(c);
  Result := (c < endC);
  if not Result then Exit;
  quotStr := ToUTF8String(c2, c);
  inc(c);
end;
//------------------------------------------------------------------------------

function IsNumPending(var c: PUTF8Char;
  endC: PUTF8Char; ignoreComma: Boolean): Boolean;
var
  c2: PUTF8Char;
begin
  Result := false;

  //skip white space +/- single comma
  if ignoreComma then
  begin
    while (c < endC) and (c^ <= space) do inc(c);
    if (c^ = ',') then inc(c);
  end;
  while (c < endC) and (c^ <= ' ') do inc(c);
  if (c = endC) then Exit;

  c2 := c;
  if (c2^ = '-') then inc(c2);
  if (c2^ = SvgDecimalSeparator) then inc(c2);
  Result := (c2 < endC) and (c2^ >= '0') and (c2^ <= '9');
end;
//------------------------------------------------------------------------------

function ParseTransform(const transform: UTF8String): TMatrixD;
var
  i: integer;
  c, endC: PUTF8Char;
  c2: UTF8Char;
  word: UTF8String;
  values: array[0..5] of double;
  mat: TMatrixD;
begin
  c := PUTF8Char(transform);
  endC := c + Length(transform);
  Result := IdentityMatrix; //in case of invalid or referenced value

  while ParseNextWord(c, endC, word) do
  begin
    if Length(word) < 5 then Exit;
    if ParseNextChar(c, endC) <> '(' then Exit; //syntax check
    //reset values variables
    for i := 0 to High(values) do values[i] := InvalidD;
    //and since every transform function requires at least one value
    if not ParseNextNum(c, endC, false, values[0]) then Break;
    //now get additional variables
    i := 1;
    while (i < 6) and IsNumPending(c, endC, true) and
      ParseNextNum(c, endC, true, values[i]) do inc(i);
    if ParseNextChar(c, endC) <> ')' then Exit; //syntax check

    mat := IdentityMatrix;
    //scal(e), matr(i)x, tran(s)late, rota(t)e, skew(X), skew(Y)
    case LowerCaseTable[word[5]] of
      'e' : //scalE
        if not IsValid(values[1]) then
          MatrixScale(mat, values[0]) else
            MatrixScale(mat, values[0], values[1]);
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
          c2 := LowerCaseTable[word[10]];
          if c2 = 'x' then
            MatrixTranslate(mat, values[0], 0)
          else if c2 = 'y' then
            MatrixTranslate(mat, 0, values[0]);
        end
        else if IsValid(values[1]) then
          MatrixTranslate(mat, values[0], values[1])
        else
          MatrixTranslate(mat, values[0], 0);
      't' : //rotaTe
        if IsValid(values[2]) then
          MatrixRotate(mat, PointD(values[1],values[2]), DegToRad(values[0]))
        else
          MatrixRotate(mat, NullPointD, DegToRad(values[0]));
       'x' : //skewX
         begin
            MatrixSkew(mat, DegToRad(values[0]), 0);
         end;
       'y' : //skewY
         begin
            MatrixSkew(mat, 0, DegToRad(values[0]));
         end;
    end;
    Result := MatrixMultiply(Result, mat);
  end;
end;
//------------------------------------------------------------------------------

procedure GetSvgFontInfo(const value: UTF8String; var fontInfo: TSVGFontInfo);
var
  c, endC: PUTF8Char;
  hash: Cardinal;
begin
  c := PUTF8Char(value);
  endC := c + Length(value);
  while (c < endC) and SkipBlanks(c, endC) do
  begin
    if c = ';' then
      break
    else if IsNumPending(c, endC, true) then
      ParseNextNum(c, endC, true, fontInfo.size)
    else
    begin
      hash := ParseNextWordHashed(c, endC);
      case hash of
        hSans_045_Serif   : fontInfo.family := ttfSansSerif;
        hSerif            : fontInfo.family := ttfSerif;
        hMonospace        : fontInfo.family := ttfMonospace;
        hBold             : fontInfo.weight := 600;
        hItalic           : fontInfo.italic := sfsItalic;
        hNormal           : 
          begin
            fontInfo.weight := 400;
            fontInfo.italic := sfsNone;
          end;
        hStart            : fontInfo.align := staLeft;
        hMiddle           : fontInfo.align := staCenter;
        hEnd              : fontInfo.align := staRight;
        hline_045_through : fontInfo.decoration := fdStrikeThrough;
        hUnderline        : fontInfo.decoration := fdUnderline;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function HtmlDecode(const html: UTF8String): UTF8String;
var
  val, len: integer;
  c,ce,endC: PUTF8Char;
begin
  len := Length(html);
  SetLength(Result, len*3);
  c := PUTF8Char(html);
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
      case GetHashCaseSensitive(c, ce - c) of
        {$I Img32.SVG.HtmlValues.inc}
      end;
    end;

    //convert unicode value to utf8 chars
    //this saves the overhead of multiple UTF8String<-->string conversions.
    case val of
      0 .. $7F:
        begin
          result[len] := UTF8Char(val);
          inc(len);
        end;
      $80 .. $7FF:
        begin
          Result[len] := UTF8Char($C0 or (val shr 6));
          Result[len+1] := UTF8Char($80 or (val and $3f));
          inc(len, 2);
        end;
      $800 .. $7FFF:
        begin
          Result[len] := UTF8Char($E0 or (val shr 12));
          Result[len+1] := UTF8Char($80 or ((val shr 6) and $3f));
          Result[len+2] := UTF8Char($80 or (val and $3f));
          inc(len, 3);
        end;
      $10000 .. $10FFFF:
        begin
          Result[len] := UTF8Char($F0 or (val shr 18));
          Result[len+1] := UTF8Char($80 or ((val shr 12) and $3f));
          Result[len+2] := UTF8Char($80 or ((val shr 6) and $3f));
          Result[len+3] := UTF8Char($80 or (val and $3f));
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

function HexByteToInt(h: UTF8Char): Cardinal; {$IFDEF INLINE} inline; {$ENDIF}
begin
  case h of
    '0'..'9': Result := Ord(h) - Ord('0');
    'A'..'F': Result := 10 + Ord(h) - Ord('A');
    'a'..'f': Result := 10 + Ord(h) - Ord('a');
    else Result := 0;
  end;
end;
//------------------------------------------------------------------------------

function IsFraction(val: double): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (val <> 0) and (Abs(val) < 1);
end;
//------------------------------------------------------------------------------

function UTF8StringToColor32(const value: UTF8String; var color: TColor32): Boolean;
var
  i, len  : integer;
  j       : Cardinal;
  clr     : TColor32;
  alpha   : Byte;
  vals    : array[0..3] of double;
  mus     :  array[0..3] of TUnitType;
  c, endC : PUTF8Char;
begin
  Result := false;
  len := Length(value);
  if len < 3 then Exit;
  c := PUTF8Char(value);

  if (color = clInvalid) or (color = clCurrent) or (color = clNone32) then
    alpha := 255 else
    alpha := color shr 24;

  if Match(c, 'rgb') then
  begin
    endC := c + len;
    inc(c, 3);
    if (c^ = 'a') then inc(c);
    if (ParseNextChar(c, endC) <> '(') or
      not ParseNextNumEx(c, endC, false, vals[0], mus[0]) or
      not ParseNextNumEx(c, endC, true, vals[1], mus[1]) or
      not ParseNextNumEx(c, endC, true, vals[2], mus[2]) then Exit;
    for i := 0 to 2 do
      if mus[i] = utPercent then
        vals[i] := vals[i] * 255 / 100;

    if ParseNextNumEx(c, endC, true, vals[3], mus[3]) then
      alpha := 255 else //stops further alpha adjustment
      vals[3] := 255;
    if ParseNextChar(c, endC) <> ')' then Exit;
    for i := 0 to 3 do if IsFraction(vals[i]) then
      vals[i] := vals[i] * 255;
    color := ClampByte(Round(vals[3])) shl 24 +
      ClampByte(Round(vals[0])) shl 16 +
      ClampByte(Round(vals[1])) shl 8 +
      ClampByte(Round(vals[2]));
  end
  else if (c^ = '#') then           //#RRGGBB or #RGB
  begin
    if (len = 9) then
    begin
      clr := $0;
      alpha := $0;
      for i := 1 to 6 do
      begin
        inc(c);
        clr := clr shl 4 + HexByteToInt(c^);
      end;
      for i := 1 to 2 do
      begin
        inc(c);
        alpha := alpha shl 4 + HexByteToInt(c^);
      end;
      clr := clr or alpha shl 24;
    end
    else if (len = 7) then
    begin
      clr := $0;
      for i := 1 to 6 do
      begin
        inc(c);
        clr := clr shl 4 + HexByteToInt(c^);
      end;
      clr := clr or $FF000000;
    end
    else if (len = 5) then
    begin
      clr := $0;
      for i := 1 to 3 do
      begin
        inc(c);
        j := HexByteToInt(c^);
        clr := clr shl 4 + j;
        clr := clr shl 4 + j;
      end;
      inc(c);
      alpha := HexByteToInt(c^);
      alpha := alpha + alpha shl 4;
      clr := clr or alpha shl 24;
    end
    else if (len = 4) then
    begin
      clr := $0;
      for i := 1 to 3 do
      begin
        inc(c);
        j := HexByteToInt(c^);
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
    i := ColorConstList.IndexOf(string(value));
    if i < 0 then Exit;
    color := TColorObj(ColorConstList.Objects[i]).cc.ColorValue;
  end;

  //and in case the opacity has been set before the color
  if (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
{$IFDEF ANDROID}
  color := SwapRedBlue(color);
{$ENDIF}
  Result := true;
end;
//------------------------------------------------------------------------------

function MakeDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfInteger;
var
  i, len: integer;
  dist: double;
begin
  dist := 0;
  len := Length(dblArray);
  SetLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i] := Ceil(dblArray[i] * scale);
    dist := Result[i] + dist;
  end;

  if dist = 0 then
  begin
    Result := nil;
  end
  else if Odd(len) then
  begin
    SetLength(Result, len *2);
    Move(Result[0], Result[len], len * SizeOf(integer));
  end;
end;
//------------------------------------------------------------------------------

function PeekNextChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  if not SkipBlanks(c, endC) then
    Result := #0 else
    Result := c^;
end;
//------------------------------------------------------------------------------

procedure ParseStyleElementContent(const value: UTF8String;
  stylesList: TClassStylesList);
var
  len, cap: integer;
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

var
  i: integer;
  aclassName: UTF8String;
  aStyle: UTF8String;
  c, c2, endC: PUTF8Char;
begin
  //https://oreillymedia.github.io/Using_SVG/guide/style.html

  stylesList.Clear;
  if value = '' then Exit;

  len := 0; cap := 0;
  c := @value[1];
  endC := c + Length(value);

  SkipBlanks(c, endC);
  if Match(c, '<![cdata[') then inc(c, 9);

  while SkipStyleBlanks(c, endC) and
    CharInSet(LowerCaseTable[PeekNextChar(c, endC)],
      [SvgDecimalSeparator, '#', 'a'..'z']) do
  begin
    //get one or more class names for each pending style
    c2 := c;
    ParseNameLength(c, endC);
    aclassName := ToUTF8String(c2, c);

    AddName(Lowercase(String(aclassName)));
    if PeekNextChar(c, endC) = ',' then
    begin
      inc(c);
      Continue;
    end;
    if len = 0 then break;
    SetLength(names, len); //ie no more comma separated names

    //now get the style
    if PeekNextChar(c, endC) <> '{' then Break;
    inc(c);
    c2 := c;
    while (c < endC) and (c^ <> '}') do inc(c);
    if (c = endC) then break;
    aStyle := ToUTF8String(c2, c);

    //finally, for each class name add (or append) this style
    for i := 0 to High(names) do
      stylesList.AddAppendStyle(names[i], aStyle);
    names := nil;
    len := 0; cap := 0;
    inc(c);
  end;
end;

//------------------------------------------------------------------------------
// TXmlEl classes
//------------------------------------------------------------------------------

constructor TXmlEl.Create(owner: TSvgParser);
begin
{$IFDEF XPLAT_GENERICS}
  attribs := TList<PSvgAttrib>.Create;
{$ELSE}
  attribs := TList.Create;
{$ENDIF}
  selfClosed := true;
  Self.owner := owner;
end;
//------------------------------------------------------------------------------

destructor TXmlEl.Destroy;
begin
  Clear;
  attribs.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TXmlEl.Clear;
var
  i: integer;
begin
  for i := 0 to attribs.Count -1 do
    Dispose(PSvgAttrib(attribs[i]));
  attribs.Clear;
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  style: UTF8String;
  c2: PUTF8Char;
begin
  SkipBlanks(c, endC);
  c2 := c;;
  ParseNameLength(c, endC);
  name := ToUTF8String(c2, c);

  //load the class's style (ie undotted style) if found.
  style := owner.classStyles.GetStyle(name);
  if style <> '' then ParseStyleAttribute(style);

  Result := ParseAttributes(c, endC);
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttribName(var c: PUTF8Char;
  endC: PUTF8Char; attrib: PSvgAttrib): Boolean;
var
  c2: PUTF8Char;
  //attribName: UTF8String;
begin
  Result := SkipBlanks(c, endC);
  if not Result then Exit;
  c2 := c;
  ParseNameLength(c, endC);
  attrib.Name := ToUTF8String(c2, c);
  attrib.hash := GetHash(attrib.Name);
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttribValue(var c: PUTF8Char;
  endC: PUTF8Char; attrib: PSvgAttrib): Boolean;
var
  quoteChar : UTF8Char;
  c2, c3: PUTF8Char;
begin
  Result := ParseNextChar(c, endC) = '=';
  if not Result then Exit;
  quoteChar := ParseQuoteChar(c, endC);
  if quoteChar = #0 then Exit;
  //trim leading and trailing spaces
  while (c < endC) and (c^ <= space) do inc(c);
  c2 := c;
  while (c < endC) and (c^ <> quoteChar) do inc(c);
  c3 := c;
  while (c3 > c2) and ((c3 -1)^ <= space) do 
    dec(c3);
  attrib.value := ToUTF8String(c2, c3);
  inc(c); //skip end quote
end;
//------------------------------------------------------------------------------

function TXmlEl.ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  i: integer;
  attrib, styleAttrib, classAttrib, idAttrib: PSvgAttrib;
  classes: UTF8Strings;
  ansi: UTF8String;
begin
  Result := false;
  styleAttrib := nil;  classAttrib := nil;  idAttrib := nil;

  while SkipBlanks(c, endC) do
  begin
    if CharInSet(c^, ['/', '?', '>']) then
    begin
      if (c^ <> '>') then
      begin
        inc(c);
        if (c^ <> '>') then Exit; //error
        selfClosed := true;
      end;
      inc(c);
      Result := true;
      break;
    end
    else if (c^ = 'x') and Match(c, 'xml:') then
    begin
      inc(c, 4); //ignore xml: prefixes
    end;

    New(attrib);
    if not ParseAttribName(c, endC, attrib) or
      not ParseAttribValue(c, endC, attrib) then
    begin
      Dispose(attrib);
      Exit;
    end;

    attribs.Add(attrib);    
    case attrib.hash of
      hId     : idAttrib := attrib;
      hClass  : classAttrib := attrib;
      hStyle  : styleAttrib := attrib;
    end;    
  end;

  if assigned(classAttrib) then
    with classAttrib^ do
    begin
      //get the 'dotted' classname(s)
      classes := Split(value);
      for i := 0 to High(classes) do
      begin
        ansi := SvgDecimalSeparator + classes[i];
        //get the style definition
        ansi := owner.classStyles.GetStyle(ansi);
        if ansi <> '' then ParseStyleAttribute(ansi);
      end;
    end;

  if assigned(styleAttrib) then
    ParseStyleAttribute(styleAttrib.value);

  if assigned(idAttrib) then
  begin
    //get the 'hashed' classname
    ansi := '#' + idAttrib.value;
    //get the style definition
    ansi := owner.classStyles.GetStyle(ansi);
    if ansi <> '' then ParseStyleAttribute(ansi);
  end;
  
end;
//------------------------------------------------------------------------------

procedure TXmlEl.ParseStyleAttribute(const style: UTF8String);
var
  styleName, styleVal: UTF8String;
  c, c2, endC: PUTF8Char;
  attrib: PSvgAttrib;
begin
  //there are 4 ways to load styles (in ascending precedence) -
  //1. a class element style (called during element contruction)
  //2. a non-element class style (called via a class attribute)
  //3. an inline style (called via a style attribute)
  //4. an id specific class style

  c := PUTF8Char(style);
  endC := c + Length(style);
  while SkipStyleBlanks(c, endC) do
  begin
    c2 := c;
    ParseStyleNameLen(c, endC);
    styleName := ToUTF8String(c2, c);
    if styleName = '' then Break;

    if (ParseNextChar(c, endC) <> ':') or  //syntax check
      not SkipBlanks(c,endC) then Break;

    c2 := c;
    inc(c);
    while (c < endC) and (c^ <> ';') do inc(c);
    styleVal := ToUTF8String(c2, c);
    AllTrim(styleVal);
    inc(c);

    new(attrib);
    attrib.name := styleName;
    attrib.value := styleVal;
    attrib.hash := GetHash(attrib.name);
    attribs.Add(attrib);
  end;
end;

//------------------------------------------------------------------------------
// TDocTypeEl
//------------------------------------------------------------------------------

procedure TDocTypeEl.SkipWord(var c, endC: PUTF8Char);
begin
  while (c < endC) and (c^ > space) do inc(c);
  inc(c);
end;
//------------------------------------------------------------------------------

function TDocTypeEl.ParseEntities(var c, endC: PUTF8Char): Boolean;
var
  attrib: PSvgAttrib;
begin
  attrib := nil;
  inc(c); //skip opening '['
  while (c < endC) and SkipBlanks(c, endC) do
  begin
    if (c^ = ']') then break
    else if not Match(c, '<!entity') then
    begin
      while c^ > space do inc(c); //skip word.
      Continue;
    end;
    inc(c, 8);
    new(attrib);
    if not ParseAttribName(c, endC, attrib) then break;
    SkipBlanks(c, endC);
    if not (c^ in [quote, dquote]) then break;
    if not ParseQuotedString(c, endC, attrib.value) then break;
    attribs.Add(attrib);
    attrib := nil;
    SkipBlanks(c, endC);
    if c^ <> '>' then break;
    inc(c); //skip entity's trailing '>'
  end;
  if Assigned(attrib) then Dispose(attrib);
  Result := (c < endC) and (c^ = ']');
  inc(c);
end;
//------------------------------------------------------------------------------

function TDocTypeEl.ParseAttributes(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  dummy : UTF8String;
begin
  while SkipBlanks(c, endC) do
  begin
    //we're currently only interested in ENTITY declarations
    case c^ of
      '[': ParseEntities(c, endC);
      '"', '''': ParseQuotedString(c, endC, dummy);
      '>': break;
      else SkipWord(c, endC);
    end;
  end;
  Result := (c < endC) and (c^ = '>');
  inc(c);
end;

//------------------------------------------------------------------------------
// TSvgTreeEl
//------------------------------------------------------------------------------

constructor TSvgTreeEl.Create(owner: TSvgParser);
begin
  inherited Create(owner);
{$IFDEF XPLAT_GENERICS}
  childs := TList<TSvgTreeEl>.Create;
{$ELSE}
  childs := TList.Create;
{$ENDIF}
  selfClosed := false;
end;
//------------------------------------------------------------------------------

destructor TSvgTreeEl.Destroy;
begin
  inherited;
  childs.Free;
end;
//------------------------------------------------------------------------------

procedure TSvgTreeEl.Clear;
var
  i: integer;
begin
  for i := 0 to childs.Count -1 do
    TSvgTreeEl(childs[i]).free;
  childs.Clear;
  inherited;
end;
//------------------------------------------------------------------------------

function TSvgTreeEl.ParseHeader(var c: PUTF8Char; endC: PUTF8Char): Boolean;
begin
  Result := inherited ParseHeader(c, endC);
  if Result then hash := GetHash(name);
end;
//------------------------------------------------------------------------------

function TSvgTreeEl.ParseContent(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  child: TSvgTreeEl;
  entity: PSvgAttrib;
  c2, tmpC, tmpEndC: PUTF8Char;
begin
  Result := false;
  while SkipBlanks(c, endC) do
  begin
    if (c^ = '<') then
    begin
      inc(c);
      case c^ of
        '!':
          begin
            if Match(c, '!--') then             //start comment
            begin
              inc(c, 3);
              while (c < endC) and ((c^ <> '-') or
                not Match(c, '-->')) do inc(c); //end comment
              inc(c, 3);
            end else
            begin
              //it's very likely <![CDATA[
              c2 := c - 1;
              if Match(c, '![cdata[') then
              begin
                while (c < endC) and ((c^ <> ']') or not Match(c, ']]>')) do
                  inc(c);
                text := ToUTF8String(c2, c);
                inc(c, 3);
                if (hash = hStyle) then
                  ParseStyleElementContent(text, owner.classStyles);
              end else
              begin
                while (c < endC) and (c^ <> '<') do inc(c);
                text := ToUTF8String(c2, c);
              end;
            end;
          end;
        '/', '?':
          begin
            //element closing tag
            inc(c);
            if Match(c, name) then
            begin
              inc(c, Length(name));
              //very rarely there's a space before '>'
              SkipBlanks(c, endC);
              Result := c^ = '>';
              inc(c);
            end;
            Exit;
          end;
        else
        begin
          //starting a new element
          child := TSvgTreeEl.Create(owner);
          childs.Add(child);
          if not child.ParseHeader(c, endC) then break;
          if not child.selfClosed then
              child.ParseContent(c, endC);
        end;
      end;
    end
    else if c^ = '>' then
    begin
      break; //oops! something's wrong
    end
    else if (c^ = '&') and IsKnownEntity(owner, c, endC, entity) then
    begin
      tmpC := PUTF8Char(entity.value);
      tmpEndC := tmpC + Length(entity.value);
      ParseContent(tmpC, tmpEndC);
    end
    else if (hash = hTSpan) or (hash = hText) or (hash = hTextPath) then
    begin
      //text content: and because text can be mixed with one or more
      //<tspan> elements we need to create sub-elements for each text block.
      //And <tspan> elements can even have <tspan> sub-elements.
      tmpC := c;
      //preserve a leading space
      if (tmpC -1)^ = space then dec(tmpC);
      while (c < endC) and (c^ <> '<') do inc(c);
      if (hash = hTextPath) then
      begin
        text := ToUTF8String(tmpC, c);
      end else
      begin
        child := TSvgTreeEl.Create(owner);
        childs.Add(child);
        child.text := ToUTF8String(tmpC, c);
      end;
    end else
    begin
      tmpC := c;
      while (c < endC) and (c^ <> '<') do inc(c);
      text := ToUTF8String(tmpC, c);

      //if <style> element then load styles into owner.classStyles
      if (hash = hStyle) then
        ParseStyleElementContent(text, owner.classStyles);
    end;
  end;
end;
//------------------------------------------------------------------------------

constructor TSvgParser.Create;
begin
  classStyles := TClassStylesList.Create;
  svgStream   := TMemoryStream.Create;
  xmlHeader   := TXmlEl.Create(Self);
  docType     := TDocTypeEl.Create(Self);
{$IFDEF XPLAT_GENERICS}
  entities    := TList<TSvgTreeEl>.Create;
{$ELSE}
  entities    := TList.Create;
{$ENDIF}
  svgTree     := nil;
end;
//------------------------------------------------------------------------------

destructor TSvgParser.Destroy;
begin
  Clear;
  svgStream.Free;
  xmlHeader.Free;
  docType.Free;
  entities.Free;
  classStyles.Free;
end;
//------------------------------------------------------------------------------

procedure TSvgParser.Clear;
begin
  classStyles.Clear;
  svgStream.Clear;
  xmlHeader.Clear;
  docType.Clear;
  entities.Clear;
  FreeAndNil(svgTree);
end;
//------------------------------------------------------------------------------

function TSvgParser.FindEntity(hash: Cardinal): PSvgAttrib;
var
  i: integer;
begin
  //there are usually so few, that there seems little point sorting etc.
  for i := 0 to docType.attribs.Count -1 do
    if PSvgAttrib(docType.attribs[i]).hash = hash then
    begin
      Result := PSvgAttrib(docType.attribs[i]);
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromFile(const filename: string): Boolean;
var
  fs: TFileStream;
begin
  Result := false;
  if not FileExists(filename) then Exit;

  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure FlipEndian(var ch: WideChar); {$IFDEF INLINE} inline; {$ENDIF}
begin
  Word(ch) := Swap(Word(ch));
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromStream(stream: TStream): Boolean;
var
  i, len: LongInt;
  encoding: TSvgEncoding;
  s: UnicodeString;
  wc: PWord;
  utf8: UTF8String;
begin
  Clear;
  Result := true;
  try
    svgStream.LoadFromStream(stream);

    //check encoding and set to UTF-8 if necessary
    encoding := GetXmlEncoding(svgStream.Memory, svgStream.Size);
    case encoding of
      eUnicodeLE, eUnicodeBE:
        begin
          SetLength(s, svgStream.Size div 2);
          Move(svgStream.Memory^, s[1], svgStream.Size);
          if encoding = eUnicodeBE then
          begin
            wc := @s[1];
            for i := 1 to Length(s) do
            begin
              wc^ := Swap(wc^);
              inc(wc);
            end;
          end;
          utf8 := UTF8Encode(s);
          len := Length(utf8);
          svgStream.SetSize(len);
          Move(utf8[1], svgStream.Memory^, len);
        end;
    end;
    ParseStream;
  except
    Result := false;
  end;
end;
//------------------------------------------------------------------------------

function TSvgParser.LoadFromString(const str: string): Boolean;
var
  ss: TStringStream;
begin
{$IFDEF UNICODE}
  ss := TStringStream.Create(str, TEncoding.UTF8);
{$ELSE}
  ss := TStringStream.Create(UTF8Encode(str));
{$ENDIF}
  try
    Result := LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgParser.ParseStream;
var
  c, endC: PUTF8Char;
begin
  c := svgStream.Memory;
  endC := c + svgStream.Size;
  SkipBlanks(c, endC);
  if Match(c, '<?xml') then
  begin
    inc(c, 2); //todo: accommodate space after '<' eg using sMatchEl function
    if not xmlHeader.ParseHeader(c, endC) then Exit;
    SkipBlanks(c, endC);
  end;
  if Match(c, '<!doctype') then
  begin
    inc(c, 2);
    if not docType.ParseHeader(c, endC) then Exit;
  end;
  while SkipBlanks(c, endC) do
  begin
    if (c^ = '<') and Match(c, '<svg') then
    begin
      inc(c);
      svgTree := TSvgTreeEl.Create(self);
      if svgTree.ParseHeader(c, endC) and
        not svgTree.selfClosed then
          svgTree.ParseContent(c, endC);
      break;
    end;
    inc(c);
  end;
end;

//------------------------------------------------------------------------------
// TDpath
//------------------------------------------------------------------------------

function TSvgSubPath.GetFlattenedPath(scalePending: double): TPathD;
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
      SetLength(Result, pathCap);
    end;
    Result[pathLen] := pt;
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
      SetLength(Result, pathCap);
    end;
    for i := 0 to pLen -1 do
    begin
      Result[pathLen] := p[i];
      inc(pathLen);
    end;
  end;

  function LastSegWasCubic(segIdx: integer): Boolean;
  begin
    Result := (segIdx > 0) and
      (segs[segIdx -1].segType in [dsCBez, dsCSpline]);
  end;

  function LastSegWasQuad(segIdx: integer): Boolean;
  begin
    Result := (segIdx > 0) and
      (segs[segIdx -1].segType in [dsQBez, dsQSpline]);
  end;

begin
  if scalePending <= 0 then scalePending := 1.0;

  bezTolerance := BezierTolerance / scalePending;
  pathLen := 0; pathCap := 0;
  lastQCtrlPt := InvalidPointD;
  lastCCtrlPt := InvalidPointD;
  AddPoint(firstPt);
  for i := 0 to High(segs) do
    with segs[i] do
    begin
      case segType of
        dsLine:
          if Count > 1 then
            for j := 0 to (Count -1) div 2 do
              AddPoint(PointD(fVals[j*2], fVals[j*2 +1]));
        dsHorz:
          for j := 0 to (Count -1) do
            AddPoint(PointD(fVals[j], currPt.Y));
        dsVert:
          for j := 0 to (Count -1) do
            AddPoint(PointD(currPt.X, fVals[j]));
        dsArc:
          if (Count -1) > 5 then
            for j := 0 to (Count -1) div 7 do
            begin
              radii.X   := fVals[j*7];
              radii.Y   := fVals[j*7 +1];
              angle     := DegToRad(fVals[j*7 +2]);
              arcFlag   := Round(fVals[j*7 +3]);
              sweepFlag := Round(fVals[j*7 +4]);
              pt2.X := fVals[j*7 +5];
              pt2.Y := fVals[j*7 +6];

              GetSvgArcInfo(currPt, pt2, radii, angle,
                arcFlag <> 0, sweepFlag <> 0, arc1, arc2, rec);
              if (sweepFlag = 0)  then
              begin
                path2 := Arc(rec, arc2, arc1, scalePending);
                path2 := ReversePath(path2);
              end else
              begin
                path2 := Arc(rec, arc1, arc2, scalePending);
              end;
              path2 := RotatePath(path2, rec.MidPoint, angle);
              AddPath(path2);
            end;
        dsQBez:
          if (Count) > 3 then
            for j := 0 to (Count -1) div 4 do
            begin
              pt2.X := fVals[j*4];
              pt2.Y := fVals[j*4 +1];
              pt3.X := fVals[j*4 +2];
              pt3.Y := fVals[j*4 +3];
              lastQCtrlPt := pt2;
              path2 := FlattenQBezier(currPt, pt2, pt3, bezTolerance);
              AddPath(path2);
            end;
        dsQSpline:
          if Count > 1 then
            for j := 0 to (Count -1) div 2 do
            begin
              if LastSegWasQuad(i) then
                pt2 := ReflectPoint(lastQCtrlPt, currPt) else
                pt2 := currPt;
              pt3.X := fVals[j*2];
              pt3.Y := fVals[j*2 +1];
              lastQCtrlPt := pt2;
              path2 := FlattenQBezier(currPt, pt2, pt3, bezTolerance);
              AddPath(path2);
            end;
        dsCBez:
          if Count > 5 then
            for j := 0 to (Count -1) div 6 do
            begin
              pt2.X := fVals[j*6];
              pt2.Y := fVals[j*6 +1];
              pt3.X := fVals[j*6 +2];
              pt3.Y := fVals[j*6 +3];
              pt4.X := fVals[j*6 +4];
              pt4.Y := fVals[j*6 +5];
              lastCCtrlPt := pt3;
              path2 := FlattenCBezier(currPt, pt2, pt3, pt4, bezTolerance);
              AddPath(path2);
            end;
        dsCSpline:
          if Count > 3 then
            for j := 0 to (Count -1) div 4 do
            begin
              if LastSegWasCubic(i) then
                pt2 := ReflectPoint(lastCCtrlPt, currPt) else
                pt2 := currPt;
              pt3.X := fVals[j*4];
              pt3.Y := fVals[j*4 +1];
              pt4.X := fVals[j*4 +2];
              pt4.Y := fVals[j*4 +3];
              lastCCtrlPt := pt3;
              path2 := FlattenCBezier(currPt, pt2, pt3, pt4, bezTolerance);
              AddPath(path2);
            end;
      end;
    end;
  SetLength(Result, pathLen);
end;

//------------------------------------------------------------------------------
// TSvgSubPathSeg
//------------------------------------------------------------------------------

procedure TSvgSubPathSeg.AddVal(val: double);
begin
  if Length(fVals) = fCnt then
    SetLength(fVals, fCnt + buffSize);
  fVals[fCnt] := val;
  inc(fCnt);
end;
//------------------------------------------------------------------------------

function TSvgSubPathSeg.GetVal(index: integer): double;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSvgSubPathSegRangeError);
  Result := fVals[index];
end;

//------------------------------------------------------------------------------
// TSvgSubPath
//------------------------------------------------------------------------------

function TSvgSubPath.Clone: TSvgSubPath;
var
  i, segLen: integer;
begin
  Result := TSvgSubPath.Create;
  Result.firstPt := firstPt;
  segLen := Length(segs);
  SetLength(Result.segs, segLen);
  for i := 0 to segLen -1 do
    with segs[i] do
    begin
      Result.segs[i].segType := segType;
      Result.segs[i].fVals := Copy(fVals, 0, Length(fVals));
    end;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.IsClosed: Boolean;
var
  len: integer;
begin
  len := Length(segs);
  Result := (len > 0) and (segs[len-1].segType = dsClose);
end;
//------------------------------------------------------------------------------

function TSvgSubPath.AddSeg: TSvgSubPathSeg;
var
  i: integer;
begin
  i := Length(segs);
  SetLength(segs, i+1);
  Result := TSvgSubPathSeg.Create;
  segs[i] := Result;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.DeleteLastSeg: Boolean;
var
  cnt: integer;
begin
  cnt := Count;
  Result := cnt > 0;
  if not Result then Exit;
  seg[cnt -1].Free;
  SetLength(segs, cnt -1);
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetSimplePath: TPathD;
var
  i,j, pathLen, pathCap: integer;
  currPt, pt2: TPointD;

  procedure AddPoint(const pt: TPointD);
  begin
    if pathLen = pathCap then
    begin
      pathCap := pathCap + buffSize;
      SetLength(Result, pathCap);
    end;
    Result[pathLen] := pt;
    currPt := pt;
    inc(pathLen);
  end;

begin
  pathLen := 0; pathCap := 0;
  AddPoint(firstPt);
  for i := 0 to High(segs) do
    with segs[i] do
    begin
      case segType of
        dsLine:
          if Count > 1 then
            for j := 0 to (Count -1) div 2 do
              AddPoint(PointD(fVals[j*2], fVals[j*2 +1]));
        dsHorz:
          for j := 0 to (Count -1) do
            AddPoint(PointD(fVals[j], currPt.Y));
        dsVert:
          for j := 0 to (Count -1) do
            AddPoint(PointD(currPt.X, fVals[j]));
        dsArc:
          if Count > 6 then
            for j := 0 to (Count -1) div 7 do
              AddPoint(PointD(fVals[j*7 +5], fVals[j*7 +6]));
        dsQBez:
          if Count > 3 then
            for j := 0 to (Count -1) div 4 do
            begin
              pt2.X := fVals[j*4];
              pt2.Y := fVals[j*4 +1];
              AddPoint(PointD(fVals[j*4 +2], fVals[j*4 +3]));
            end;
        dsQSpline:
          if Count > 1 then
            for j := 0 to (Count -1) div 2 do
              AddPoint(PointD(fVals[j*2 +1], fVals[j*2 +1]));
        dsCBez:
          if Count > 5 then
            for j := 0 to (Count -1) div 6 do
              AddPoint(PointD(fVals[j*6 +4], fVals[j*6 +5]));
        dsCSpline:
          if Count > 3 then
            for j := 0 to (Count -1) div 4 do
              AddPoint(PointD(fVals[j*4 +2], fVals[j*4 +3]));
      end;
    end;
  SetLength(Result, pathLen);
end;
//------------------------------------------------------------------------------

destructor TSvgSubPath.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgSubPath.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    segs[i].Free;
  segs := nil;
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetCount: integer;
begin
  Result := Length(segs);
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetSeg(index: integer): TSvgSubPathSeg;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSvgSubPathRangeError);
  Result := segs[index];
end;
//------------------------------------------------------------------------------

function TSvgSubPath.GetBounds: TRectD;
var
  i,j, pathLen, pathCap: integer;
  currPt, radii, pt2, pt3, pt4: TPointD;
  lastQCtrlPt, lastCCtrlPt: TPointD;
  arcFlag, sweepFlag: integer;
  angle, arc1, arc2: double;
  rec: TRectD;
  path2, path3: TPathD;

  procedure AddPoint(const pt: TPointD);
  begin
    if pathLen = pathCap then
    begin
      pathCap := pathCap + buffSize;
      SetLength(path2, pathCap);
    end;
    path2[pathLen] := pt;
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
      SetLength(path2, pathCap);
    end;
    for i := 0 to pLen -1 do
    begin
      path2[pathLen] := p[i];
      inc(pathLen);
    end;
  end;

  function LastSegWasCubic(segIdx: integer): Boolean;
  begin
    Result := (segIdx > 0) and
      (segs[segIdx -1].segType in [dsCBez, dsCSpline]);
  end;

  function LastSegWasQuad(segIdx: integer): Boolean;
  begin
    Result := (segIdx > 0) and
      (segs[segIdx -1].segType in [dsQBez, dsQSpline]);
  end;

begin
  path2 := nil;
  pathLen := 0; pathCap := 0;
  lastQCtrlPt := InvalidPointD;
  lastCCtrlPt := InvalidPointD;
  AddPoint(firstPt);
  for i := 0 to High(segs) do
    with segs[i] do
    begin
      case segType of
        dsLine:
          if Count > 1 then
            for j := 0 to (Count -1) div 2 do
              AddPoint(PointD(fVals[j*2], fVals[j*2 +1]));
        dsHorz:
          for j := 0 to (Count -1) do
            AddPoint(PointD(fVals[j], currPt.Y));
        dsVert:
          for j := 0 to (Count -1) do
            AddPoint(PointD(currPt.X, fVals[j]));
        dsArc:
          if Count > 6 then
            for j := 0 to (Count -1) div 7 do
            begin
              radii.X   := fVals[j*7];
              radii.Y   := fVals[j*7 +1];
              angle     := DegToRad(fVals[j*7 +2]);
              arcFlag   := Round(fVals[j*7 +3]);
              sweepFlag := Round(fVals[j*7 +4]);
              pt2.X := fVals[j*7 +5];
              pt2.Y := fVals[j*7 +6];

              GetSvgArcInfo(currPt, pt2, radii, angle,
                arcFlag <> 0, sweepFlag <> 0, arc1, arc2, rec);
              if (sweepFlag = 0)  then
              begin
                path3 := Arc(rec, arc2, arc1, 1);
                path3 := ReversePath(path3);
              end else
                path3 := Arc(rec, arc1, arc2, 1);
              path3 := RotatePath(path3, rec.MidPoint, angle);
              AddPath(path3);
            end;
        dsQBez:
          if Count > 3 then
            for j := 0 to (Count -1) div 4 do
            begin
              pt2.X := fVals[j*4];
              pt2.Y := fVals[j*4 +1];
              pt3.X := fVals[j*4 +2];
              pt3.Y := fVals[j*4 +3];
              lastQCtrlPt := pt2;
              path3 := FlattenQBezier(currPt, pt2, pt3, 1);
              AddPath(path3);
            end;
        dsQSpline:
          if Count > 1 then
            for j := 0 to (Count -1) div 2 do
            begin
              if LastSegWasQuad(i) then
                pt2 := ReflectPoint(lastQCtrlPt, currPt) else
                pt2 := currPt;
              pt3.X := fVals[j*2];
              pt3.Y := fVals[j*2 +1];
              lastQCtrlPt := pt2;
              path3 := FlattenQBezier(currPt, pt2, pt3, 1);
              AddPath(path3);
            end;
        dsCBez:
          if Count > 5 then
            for j := 0 to (Count -1) div 6 do
            begin
              pt2.X := fVals[j*6];
              pt2.Y := fVals[j*6 +1];
              pt3.X := fVals[j*6 +2];
              pt3.Y := fVals[j*6 +3];
              pt4.X := fVals[j*6 +4];
              pt4.Y := fVals[j*6 +5];
              lastCCtrlPt := pt3;
              path3 := FlattenCBezier(currPt, pt2, pt3, pt4, 1);
              AddPath(path3);
            end;
        dsCSpline:
          if Count > 3 then
            for j := 0 to (Count -1) div 4 do
            begin
              if LastSegWasCubic(i) then
                pt2 := ReflectPoint(lastCCtrlPt, currPt) else
                pt2 := currPt;
              pt3.X := fVals[j*4];
              pt3.Y := fVals[j*4 +1];
              pt4.X := fVals[j*4 +2];
              pt4.Y := fVals[j*4 +3];
              lastCCtrlPt := pt3;
              path3 := FlattenCBezier(currPt, pt2, pt3, pt4, 1);
              AddPath(path3);
            end;
      end;
    end;
  SetLength(path2, pathLen);
  Result := GetBoundsD(path2);
end;

//------------------------------------------------------------------------------
// TSvgPath
//------------------------------------------------------------------------------

destructor TSvgPath.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

function TSvgPath.GetCount: integer;
begin
  Result := Length(fSubPaths);
end;
//------------------------------------------------------------------------------

function TSvgPath.GetPath(index: integer): TSvgSubPath;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create(rsSvgPathRangeError);
  Result := fSubPaths[index];
end;
//------------------------------------------------------------------------------

procedure TSvgPath.Clear;
var
  i: integer;
begin
  for i := 0 to Count -1 do
    fSubPaths[i].Free;
  fSubPaths := nil;
end;
//------------------------------------------------------------------------------

function TSvgPath.Clone: TSvgPath;
var
  i: integer;
begin
  Result := TSvgPath.Create;
  SetLength(Result.fSubPaths, Count);
  for i := 0 to Count -1 do
    Result.fSubPaths[i] := fSubPaths[i].Clone;
end;
//------------------------------------------------------------------------------

function TSvgPath.AddPath: TSvgSubPath;
var
  i: integer;
begin
  i := Count;
  Result := TSvgSubPath.Create;
  SetLength(fSubPaths, i + 1);
  fSubPaths[i] := Result;
end;

//------------------------------------------------------------------------------
// TValue
//------------------------------------------------------------------------------

function ConvertValue(const value: TValue; scale: double): double;
const
  mm  = 96 / 25.4;
  cm  = 96 / 2.54;
  rad = 180 / PI;
  pt  = 4 / 3;
begin
  //https://oreillymedia.github.io/Using_SVG/guide/units.html
  //todo: still lots of units to support (eg times for animation)
  with value do
    if not IsValid or (rawVal = 0) then
      Result := 0
    else
      case value.unitType of
        utNumber:
          Result := rawVal;
        utPercent:
          Result := rawVal * 0.01 * scale;
        utRadian:
          Result := rawVal * rad;
        utInch:
          Result := rawVal * 96;
        utCm:
          Result := rawVal * cm;
        utMm:
          Result := rawVal * mm;
        utEm:
          if scale <= 0 then
            Result := rawVal * 16 else
            Result := rawVal * scale;
        utEx:
          if scale <= 0 then
            Result := rawVal * 8 else
            Result := rawVal * scale * 0.5;
        utPica:
          Result := rawVal * 16;
        utPt:
          Result := rawVal * pt;
        else
          Result := rawVal;
      end;
end;
//------------------------------------------------------------------------------

procedure TValue.Init;
begin
  rawVal      := InvalidD;
  unitType          := utNumber;
end;
//------------------------------------------------------------------------------

procedure TValue.SetValue(val: double; unitTyp: TUnitType);
begin
  rawVal  := val;
  unitType      := unitTyp;
end;
//------------------------------------------------------------------------------

function TValue.GetValue(relSize: double; assumeRelValBelow: Double): double;
begin
  if not IsValid or (rawVal = 0) then
    Result := 0
  else if IsRelativeValue(assumeRelValBelow) then
    Result := rawVal * relSize
  else
    Result := ConvertValue(self, relSize);
end;
//------------------------------------------------------------------------------

function TValue.GetValueXY(const relSize: TRectD; assumeRelValBelow: Double): double;
begin
  //https://www.w3.org/TR/SVG11/coords.html#Units
  Result := GetValue(Hypot(relSize.Width, relSize.Height)/sqrt2, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function  TValue.IsRelativeValue(assumeRelValBelow: double): Boolean;
begin
  Result := (unitType = utNumber) and (Abs(rawVal) <= assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValue.IsValid: Boolean;
begin
  Result := (unitType <> utUnknown) and Img32.Vector.IsValid(rawVal);
end;
//------------------------------------------------------------------------------

function TValue.HasFontUnits: Boolean;
begin
  case unitType of
    utEm, utEx: Result := true;
    else Result := False;
  end;
end;

//------------------------------------------------------------------------------

function TValue.HasAngleUnits: Boolean;
begin
  case unitType of
    utDegree, utRadian: Result := true;
    else Result := False;
  end;
end;

//------------------------------------------------------------------------------
// TValuePt
//------------------------------------------------------------------------------

procedure TValuePt.Init;
begin
  X.Init;
  Y.Init;
end;
//------------------------------------------------------------------------------

function TValuePt.GetPoint(const relSize: double; assumeRelValBelow: Double): TPointD;
begin
  Result.X := X.GetValue(relSize, assumeRelValBelow);
  Result.Y := Y.GetValue(relSize, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValuePt.GetPoint(const relSize: TRectD; assumeRelValBelow: Double): TPointD;
begin
  Result.X := X.GetValue(relSize.Width, assumeRelValBelow);
  Result.Y := Y.GetValue(relSize.Height, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValuePt.IsValid: Boolean;
begin
  Result := X.IsValid and Y.IsValid;
end;

//------------------------------------------------------------------------------
// TValueRec
//------------------------------------------------------------------------------

procedure TValueRecWH.Init;
begin
  left.Init;
  top.Init;
  width.Init;
  height.Init;
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectD(const relSize: TRectD; assumeRelValBelow: Double): TRectD;
begin
  with GetRectWH(relSize, assumeRelValBelow) do
  begin
    Result.Left :=Left;
    Result.Top := Top;
    Result.Right := Left + Width;
    Result.Bottom := Top + Height;
  end;
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectD(relSize: double; assumeRelValBelow: Double): TRectD;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValue(relSize, assumeRelValBelow);

  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValue(relSize, assumeRelValBelow);

  Result.Right := Result.Left + width.GetValue(relSize, assumeRelValBelow);
  Result.Bottom := Result.Top + height.GetValue(relSize, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectWH(const relSize: TRectD; assumeRelValBelow: Double): TRectWH;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValue(relSize.Width, assumeRelValBelow);

  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValue(relSize.Height, assumeRelValBelow);

  Result.Width := width.GetValue(relSize.Width, assumeRelValBelow);
  Result.Height := height.GetValue(relSize.Height, assumeRelValBelow);
end;
//------------------------------------------------------------------------------

function TValueRecWH.IsValid: Boolean;
begin
  Result := width.IsValid and height.IsValid;
end;
//------------------------------------------------------------------------------

function TValueRecWH.IsEmpty: Boolean;
begin
  Result := (width.rawVal <= 0) or (height.rawVal <= 0);
end;

//------------------------------------------------------------------------------
// TClassStylesList
//------------------------------------------------------------------------------

constructor TClassStylesList.Create;
begin
  fList := TStringList.Create;
  fList.Duplicates := dupIgnore;
  fList.CaseSensitive := false;
  fList.Sorted := True;
end;
//------------------------------------------------------------------------------

destructor TClassStylesList.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

function TClassStylesList.AddAppendStyle(const classname: string; const ansi: UTF8String): integer;
var
  i: integer;
  sr: PAnsStringiRec;
begin
  Result := fList.IndexOf(classname);
  if (Result >= 0) then
  begin
    sr := PAnsStringiRec(fList.Objects[Result]);
    i := Length(sr.ansi);
    if sr.ansi[i] <> ';' then
      sr.ansi := sr.ansi + ';' + ansi else
      sr.ansi := sr.ansi + ansi;
  end else
  begin
    new(sr);
    sr.ansi := ansi;
    Result := fList.AddObject(classname, Pointer(sr));
  end;
end;
//------------------------------------------------------------------------------

function TClassStylesList.GetStyle(const classname: UTF8String): UTF8String;
var
  i: integer;
begin
  SetLength(Result, 0);
  i := fList.IndexOf(string(className));
  if i >= 0 then
    Result := PAnsStringiRec(fList.objects[i]).ansi;
end;
//------------------------------------------------------------------------------

procedure TClassStylesList.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
    Dispose(PAnsStringiRec(fList.Objects[i]));
  fList.Clear;
end;

//------------------------------------------------------------------------------
// GetSvgArcInfo - and support functions
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
function GetSvgArcInfo(const p1, p2: TPointD; radii: TPointD;
  phi_rads: double; fA, fS: boolean;
  out startAngle, endAngle: double; out rec: TRectD): Boolean;
var
  x1_, y1_, rxry, rxy1_, ryx1_, s_phi, c_phi: double;
  hd_x, hd_y, hs_x, hs_y, sum_of_sq, lambda, coe: double;
  cx, cy, cx_, cy_, xcr1, xcr2, ycr1, ycr2, deltaAngle: double;
begin
    Result := false;
    if (radii.X < 0) then radii.X := -radii.X;
    if (radii.Y < 0) then radii.Y := -radii.Y;
    if (radii.X = 0) or (radii.Y = 0) then Exit;

    GetSinCos(phi_rads, s_phi, c_phi);;
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

    // F6.5.6
    deltaAngle := Radian4(xcr1, ycr1, -xcr2, -ycr2);

    rec.Left := cx - radii.X;
    rec.Right := cx + radii.X;
    rec.Top := cy - radii.Y;
    rec.Bottom := cy + radii.Y;

    NormalizeAngle(startAngle);
    endAngle := startAngle + deltaAngle;
    NormalizeAngle(endAngle);

    if not ClockwiseRotationIsAnglePositive then
    begin
      startAngle := -startAngle;
      endAngle := -endAngle;
    end;

    Result := true;
end;

//------------------------------------------------------------------------------
// DParse and support functions
//------------------------------------------------------------------------------

function GetSegType(var c, endC: PUTF8Char;
  out isRelative: Boolean): TSvgPathSegType;
var
  ch: UTF8Char;
begin
  Result := dsUnknown;
  if not SkipBlanks(c, endC) then Exit;
  ch := upcase(c^);
  if not CharInSet(ch,
    ['A','C','H','M','L','Q','S','T','V','Z']) then Exit;
  case ch of
    'M': Result := dsMove;
    'L': Result := dsLine;
    'H': Result := dsHorz;
    'V': Result := dsVert;
    'A': Result := dsArc;
    'Q': Result := dsQBez;
    'C': Result := dsCBez;
    'T': Result := dsQSpline;
    'S': Result := dsCSpline;
    'Z': Result := dsClose;
  end;
  isRelative := c^ >= 'a';
  inc(c);
end;
//------------------------------------------------------------------------------

procedure ParseSvgPath(const value: UTF8String; svgPaths: TSvgPath);
var
  currSeg     : TSvgSubPathSeg;
  currDpath   : TSvgSubPath;
  currSegType : TSvgPathSegType;
  lastPt      : TPointD;

  procedure StartNewDpath;
  begin
    if Assigned(currDpath) and
      not Assigned(currDpath.segs) then Exit;
    currDpath := svgPaths.AddPath;
    currDpath.firstPt := lastPt;
    currDpath.segs := nil;
    currSeg := nil;
  end;

  procedure StartNewSeg;
  begin
    if not Assigned(currDpath) then
      StartNewDpath;
    currSeg := currDpath.AddSeg;
    currSeg.segType := currSegType;
  end;

  procedure AddSegValue(val: double);
  begin
    if not Assigned(currSeg) then StartNewSeg;
    currSeg.AddVal(val);
  end;

  procedure AddSegPoint(const pt: TPointD);
  begin
    AddSegValue(pt.X); AddSegValue(pt.Y);
  end;

  function Parse2Num(var c, endC: PUTF8Char;
    var pt: TPointD; isRelative: Boolean): Boolean;
  begin
    Result := ParseNextNum(c, endC, true, pt.X) and
      ParseNextNum(c, endC, true, pt.Y);
    if not Result or not isRelative then Exit;
    pt.X := pt.X + lastPt.X;
    pt.Y := pt.Y + lastPt.Y;
  end;

var
  i: integer;
  d: double;
  c, endC: PUTF8Char;
  currPt: TPointD;
  isRelative: Boolean;
begin
  if not Assigned(svgPaths) then Exit;
  svgPaths.Clear;

  currSeg     := nil;
  currDpath   := nil;
  currSegType := dsMove;

  c := PUTF8Char(value);
  endC := c + Length(value);
  isRelative := false;
  currPt := NullPointD;

  while true do
  begin
    currSegType := GetSegType(c, endC, isRelative);
    if currSegType = dsUnknown then break;

    lastPt := currPt;
    if (currSegType = dsMove) then
    begin
      currDpath := nil;
      currSeg := nil;

      if not Parse2Num(c, endC, currPt, isRelative) then break;
      lastPt :=  currPt;

      //values immediately following a Move are implicitly Line statements
      if IsNumPending(c, endC, true) then
        currSegType := dsLine else
        Continue;
    end
    else if (currSegType = dsClose) then
    begin
      if Assigned(currDpath) then
        currPt := currDpath.firstPt;
      StartNewSeg;
      Continue;
    end;
    currSeg := nil;

    case currSegType of
      dsHorz:
        while IsNumPending(c, endC, true) and
          ParseNextNum(c, endC, true, currPt.X) do
        begin
          if isRelative then
            currPt.X := currPt.X + lastPt.X;
          AddSegValue(currPt.X);
          lastPt := currPt;
        end;

      dsVert:
        while IsNumPending(c, endC, true) and
          ParseNextNum(c, endC, true, currPt.Y) do
        begin
          if isRelative then
            currPt.Y := currPt.Y + lastPt.Y;
          AddSegValue(currPt.Y);
          lastPt := currPt;
        end;

      dsLine:
        while Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          lastPt := currPt;
          SkipBlanks(c, endC);
          if IsNumPending(c, endC, true) then Continue;
          if LowerCaseTable[c^] = 'l' then GetSegType(c, endC, isRelative)
          else break;
        end;

      dsQSpline:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCSpline:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsQBez:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCBez:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, isRelative) do
        begin
          AddSegPoint(currPt);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsArc:
        while IsNumPending(c, endC, true) and
          Parse2Num(c, endC, currPt, false) do
        begin
          AddSegPoint(currPt);                              //radii
          if ParseNextNum(c, endC, true, d) then
            AddSegValue(d);                                 //angle
          if not GetSingleDigit(c, endC, i) then break;     //arc-flag
          AddSegValue(i);
          if not GetSingleDigit(c, endC, i) then break;     //sweep-flag
          AddSegValue(i);
          if not Parse2Num(c, endC, currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// Base64 (MIME) Encode & Decode
//------------------------------------------------------------------------------

type
  PFourChars = ^TFourChars;
  TFourChars = record
    c1: UTF8Char;
    c2: UTF8Char;
    c3: UTF8Char;
    c4: UTF8Char;
  end;

//------------------------------------------------------------------------------

function Chr64ToVal(c: UTF8Char): integer;
begin
  case c of
    '+': result := 62;
    '/': result := 63;
    '0'..'9': result := ord(c) + 4;
    'A'..'Z': result := ord(c) -65;
    'a'..'z': result := ord(c) -71;
    else Raise Exception.Create('Corrupted MIME encoded text');
  end;
end;
//------------------------------------------------------------------------------

function Frst6Bits(val: PCardinal): byte;
begin
  result := (val^ shr 2) and $3F;
end;
//------------------------------------------------------------------------------

function FrstChr(c: PFourChars): UTF8Char;
begin
  result := ansichar(Chr64ToVal(c.c1) shl 2 or Chr64ToVal(c.c2) shr 4);
end;
//------------------------------------------------------------------------------

function Scnd6Bits(val: PCardinal): byte;
begin
  result := (val^ shr 12) and $F or ((val^ and $3) shl 4);
end;
//------------------------------------------------------------------------------

function ScndChr(c: PFourChars): UTF8Char;
begin
  result := ansichar(Chr64ToVal(c.c2) shl 4 or Chr64ToVal(c.c3) shr 2);
end;
//------------------------------------------------------------------------------

function Thrd6Bits(val: PCardinal): byte;
begin
  result := (val^ shr 22) and $3 or ((val^ shr 6) and $3C);
end;
//------------------------------------------------------------------------------

function ThrdChr(c: PFourChars): UTF8Char;
begin
  result := ansichar( Chr64ToVal(c.c3) shl 6 or Chr64ToVal(c.c4) );
end;
//------------------------------------------------------------------------------

function Frth6Bits(val: PCardinal): byte;
begin
  result := (val^ shr 16) and $3F;
end;
//------------------------------------------------------------------------------

function Base64Decode(const str: UTF8String): UTF8String;
var
  i, j, len, extra: integer;
  Chars4: PFourChars;
begin
  result := '';
  len := length(str);
  if (len = 0) then exit
  else if (len mod 4 > 0) then //oops!!!
    Raise Exception.Create('Corrupted MIME encoded text');
  if str[len-1] = '=' then extra := 2
  else if str[len] = '=' then extra := 1
  else extra := 0;
  setlength(result, (len div 4 * 3) - extra);
  Chars4 := @str[1];
  i := 1;
  for j := 1 to (len div 4) -1 do
  begin
    result[i] := FrstChr(Chars4);
    result[i+1] := ScndChr(Chars4);
    result[i+2] := ThrdChr(Chars4);
    inc(pbyte(Chars4),4);
    inc(i,3);
  end;
  result[i] := FrstChr(Chars4);
  if extra < 2  then result[i+1] := ScndChr(Chars4);
  if extra < 1 then result[i+2] := ThrdChr(Chars4);
end;

//------------------------------------------------------------------------------
// initialization procedures
//------------------------------------------------------------------------------

procedure MakeLowerCaseTable;
var
  i: UTF8Char;
begin
  for i:= #0 to #$40 do LowerCaseTable[i]:= i;
  for i:= #$41 to #$5A do LowerCaseTable[i]:= UTF8Char(Ord(i) + $20);
  for i:= #$5B to #$FF do LowerCaseTable[i]:= i;
end;
//------------------------------------------------------------------------------

procedure MakeColorConstList;
var
  i   : integer;
  co  : TColorObj;
  {$I Img32.SVG.HtmlColorConsts.inc}
begin
  ColorConstList := TStringList.Create;
  ColorConstList.CaseSensitive := false;
  //ColorConstList.OwnsObjects := true; //not all versions of Delphi
  ColorConstList.Capacity := Length(ColorConsts);
  for i := 0 to High(ColorConsts) do
  begin
    co := TColorObj.Create;
    co.cc := ColorConsts[i];
    ColorConstList.AddObject(co.cc.ColorName, co);
  end;
  ColorConstList.Sorted := true;
end;
//------------------------------------------------------------------------------

procedure CleanupColorConstList;
var
  i   : integer;
begin
  for i := 0 to ColorConstList.Count -1 do
    TColorObj(ColorConstList.Objects[i]).Free;
  ColorConstList.Free;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeLowerCaseTable;
  MakeColorConstList;

finalization
  CleanupColorConstList;
end.
