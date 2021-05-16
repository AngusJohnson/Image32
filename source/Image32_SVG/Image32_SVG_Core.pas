unit Image32_SVG_Core;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.24                                                            *
* Date      :  12 May 2021                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2021                                         *
*                                                                              *
* Purpose   :  Essential structures and functions to read SVG files            *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types, Math, Image32, Image32_Vector, Image32_Ttf;

type
  TFontSyle = (fsBold, fsItalic);
  TFontSyles = set of TFontSyle;

  TSVGFontInfo = record
    family: TTtfFontFamily;
    size: double;
    styles: TFontSyles;
    align: TTextAlign;
  end;

  TElementMeasureUnit = (emuBoundingBox, emuUserSpace);

  TMeasureUnit = (muPixel, muPercent, muDegree, muRadian,
    muInch, muCm, muMm, muEm, muEn);

  TSizeType = (stAverage, stWidth, stHeight);

  TSizeD = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    sx  : double;
    sy  : double;
    function average: double;
  end;

  TValue = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    rawVal  : double;
    mu      : TMeasureUnit;
    procedure Init;
    procedure SetValue(val: double; measureUnit: TMeasureUnit = muPixel);
    function GetValue: double;
    function GetValueX(const scaleRect: TRectD): double;
    function GetValueY(const scaleRect: TRectD): double;
    function IsValid: Boolean;
  end;

  TValuePt = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    X       : TValue;
    Y       : TValue;
    procedure Init;
    function GetPoint(const scaleRect: TRectD): TPointD;
    function IsValid: Boolean;
  end;

  TValueRecWH = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    left    : TValue;
    top     : TValue;
    width   : TValue;
    height  : TValue;
    procedure Init;
    function GetRect(const scaleRect: TRectD): TRectD;
    function GetRectWH(const scaleRect: TRectD): TRectWH;
    function GetRectWHForcePercent(const scaleRect: TRectD): TRectWH;
    function IsValid: Boolean;
    function IsEmpty: Boolean;
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

  TDsegType = (dsMove, dsLine, dsHorz, dsVert, dsArc,
    dsQBez, dsCBez, dsQSpline, dsCSpline, dsClose);

  PDpathSeg = ^TDpathSeg;
  TDpathSeg = record
    segType : TDsegType;
    vals    : TArrayOfDouble;
  end;

  PDpath = ^TDpath;
  TDpath = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    firstPt   : TPointD;
    isClosed  : Boolean;
    segs      : array of TDpathSeg;
    //scalePending parameter: the SVG will be scaled later by this amount,
    //so make sure that curves are flattened to an appropriate precision
    function GetFlattened(scalePending: double): TPathD;
  end;
  TDpaths = array of TDpath;


function HtmlDecode(const s: ansiString): ansistring;
function SkipBlanks(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
function TrimBlanks(var current: PAnsiChar; var length: integer): Boolean;
function SkipBlanksAndComma(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
function SkipStyleBlanks(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
function ParseNextChar(var current: PAnsiChar; currentEnd: PAnsiChar): AnsiChar;
function GetUtf8String(pName: PAnsiChar; len: integer): AnsiString;
function LowercaseAnsi(pName: PAnsiChar; len: integer): AnsiString;
function IsAlpha(c: AnsiChar): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
function ParseNameLength(var c: PAnsiChar): integer; overload;
function ParseNameLength(var c: PAnsiChar; endC: PAnsiChar): integer; overload;
function GetHashedName(c: PAnsiChar; length: integer): cardinal;
function GetHashedNameCaseSens(c: PAnsiChar; length: integer): cardinal;
function ParseHashedName(var c: PAnsiChar; endC: PAnsiChar): cardinal;
function GetRefName(href: PAnsiChar): PAnsiChar; {$IFDEF INLINE} inline; {$ENDIF}
function ParseStyleNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
function SvgArc(const p1, p2: TPointD; radii: TPointD; phi_rads: double;
  fA, fS: boolean; out startAngle, endAngle: double; out rec: TRectD): Boolean;
function HexByteToInt(h: AnsiChar): Cardinal; {$IFDEF INLINE} inline; {$ENDIF}
function ParseNextAlphaWord(var current: PAnsiChar; currentEnd: PAnsiChar;
  out word: AnsiString): Boolean;
function NumPending(current, currentEnd: PAnsiChar; ignoreComma: Boolean): Boolean;
function ParseNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double; out measureUnit: TMeasureUnit): Boolean; overload;
function ParseNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean; overload;
function IsFraction(val: double): Boolean;
function ColorIsURL(p: PAnsiChar): Boolean;
function PCharToColor32(p: PAnsiChar; pLen: integer; var color: TColor32): Boolean;
function MakeDashArray(const da: TArrayOfDouble; scale: double): TArrayOfInteger;

{$IF COMPILERVERSION < 17}
type
  TSetOfChar = set of Char;
function CharInSet(chr: Char; chrs: TSetOfChar): Boolean;
{$IFEND}

const
  clInvalid = $00010001;
var
  LowerCaseTable : array[#0..#255] of AnsiChar;

implementation

type
  TColorConst = record
    ColorName : string;
    ColorValue: Cardinal;
  end;

//include hashed html entity constants
{$I html_entity_hash_consts.inc}

//include lots of color constants
{$I html_color_consts.inc}


var
  ColorConstList : TStringList;

resourcestring
  rsListBoundsError     = 'List index out of bounds (%d)';

{$IF COMPILERVERSION < 17}
function CharInSet(chr: Char; chrs: TSetOfChar): Boolean;
begin
  Result := chr in chrs;
end;
{$IFEND}

//------------------------------------------------------------------------------
// TDpath
//------------------------------------------------------------------------------

function TDpath.GetFlattened(scalePending: double): TPathD;
const
  buffSize = 32;
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

begin
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
                path2 := Arc(rec, arc2, arc1, scalePending);
                path2 := ReversePath(path2);
              end else
                path2 := Arc(rec, arc1, arc2, scalePending);
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
  SetLength(Result, pathLen);
end;

//------------------------------------------------------------------------------
// TSizeD
//------------------------------------------------------------------------------

function TSizeD.average: double;
begin
  Result := (sx + sy) * 0.5;
end;

//------------------------------------------------------------------------------
// TValue
//------------------------------------------------------------------------------

procedure TValue.Init;
begin
  rawVal  := InvalidD;
  mu      := muPixel;
end;
//------------------------------------------------------------------------------

procedure TValue.SetValue(val: double; measureUnit: TMeasureUnit);
begin
  rawVal  := val;
  mu      := measureUnit;
end;
//------------------------------------------------------------------------------

function TValue.GetValue: double;
begin
  case mu of
    muPercent : Result := rawVal * 0.01;
    muDegree  : Result := rawVal;
    muRadian  : Result := rawVal * 180/PI; //convert to degrees pro. tem.
    muInch    : Result := rawVal * 96;
    muCm      : Result := rawVal * 96 / 2.54;
    muMm      : Result := rawVal * 96 / 25.4;
    muEm      : Result := rawVal * 16;
    muEn      : Result := rawVal * 8;
    else Result := rawVal;
  end;
end;
//------------------------------------------------------------------------------

function TValue.GetValueX(const scaleRect: TRectD): double;
begin
  case mu of
    muPercent : Result := (scaleRect.Right - scaleRect.Left) * rawVal * 0.01;
    muDegree  : Result := rawVal;
    muRadian  : Result := rawVal * 180/PI; //convert to degrees pro. tem.
    muInch    : Result := rawVal * 96;
    muCm      : Result := rawVal * 96 / 2.54;
    muMm      : Result := rawVal * 96 / 25.4;
    muEm      : Result := rawVal * 16;
    muEn      : Result := rawVal * 8;
    else Result := rawVal;
  end;
end;
//------------------------------------------------------------------------------

function TValue.GetValueY(const scaleRect: TRectD): double;
begin
  case mu of
    muPercent : Result := (scaleRect.Bottom - scaleRect.Top) * rawVal * 0.01;
    muDegree  : Result := rawVal;
    muRadian  : Result := rawVal * 180/PI; //convert to degrees pro. tem.
    muInch    : Result := rawVal * 96;
    muCm      : Result := rawVal * 96 / 2.54;
    muMm      : Result := rawVal * 96 / 25.4;
    muEm      : Result := rawVal * 16;
    muEn      : Result := rawVal * 8;
    else Result := rawVal;
  end;
end;
//------------------------------------------------------------------------------

function TValue.IsValid: Boolean;
begin
  Result := Image32_Vector.IsValid(rawVal);
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

function TValuePt.GetPoint(const scaleRect: TRectD): TPointD;
begin
  Result.X := X.GetValueX(scaleRect);
  Result.Y := Y.GetValueY(scaleRect);
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

function TValueRecWH.GetRect(const scaleRect: TRectD): TRectD;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValueX(scaleRect);
  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValueY(scaleRect);
  Result.Right := Result.Left + width.GetValueX(scaleRect);
  Result.Bottom := Result.Top + height.GetValueY(scaleRect);
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectWH(const scaleRect: TRectD): TRectWH;
begin
  Result.Left := left.GetValueX(scaleRect);
  Result.Top := top.GetValueY(scaleRect);
  Result.Width := width.GetValueX(scaleRect);
  Result.Height := height.GetValueY(scaleRect);
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectWHForcePercent(const scaleRect: TRectD): TRectWH;
begin
  if left.mu = muPercent then
    Result.Left := left.GetValueX(scaleRect) else
    Result.Left := left.GetValueX(scaleRect) * scaleRect.Width;
  if top.mu = muPercent then
    Result.Top := top.GetValueY(scaleRect) else
    Result.Top := top.GetValueY(scaleRect) * scaleRect.Height;
  if width.mu = muPercent then
    Result.Width := width.GetValueX(scaleRect) else
    Result.Width := width.GetValueX(scaleRect) * scaleRect.Width;
  if height.mu = muPercent then
    Result.Height := height.GetValueY(scaleRect) else
    Result.Height := height.GetValueY(scaleRect) * scaleRect.Height;
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

function ParseNextChar(var current: PAnsiChar; currentEnd: PAnsiChar): AnsiChar;
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

function ParseNameLength(var c: PAnsiChar): integer; overload;
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

function ParseNameLength(var c: PAnsiChar; endC: PAnsiChar): integer; overload;
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
function GetHashedName(c: PAnsiChar; length: integer): cardinal;
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

function ParseHashedName(var c: PAnsiChar; endC: PAnsiChar): cardinal;
var
  len: integer;
  startC: PAnsiChar;
begin
  startC := c;
  len := ParseNameLength(c, endC);
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

function ParseStyleNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
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
        {$I html_entity_hash_values.inc}
      end;
    end;

    //convert unicode value to utf8 chars
    //this saves the overhead of multiple ansistring<-->string conversions.
    case val of
      0 .. $7F:
        begin
          result[len] := AnsiChar(val);
          inc(len);
        end;
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

function ParseNextAlphaWord(var current: PAnsiChar; currentEnd: PAnsiChar;
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

function ParseNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double; out measureUnit: TMeasureUnit): Boolean; overload;
var
  decPos,exp: integer;
  isNeg, expIsNeg: Boolean;
  start: PAnsiChar;
begin
  Result := false;
  measureUnit := muPixel;

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
        inc(current);
        measureUnit := muPercent;
      end;
    'c': //convert cm to pixels
      if ((current+1)^ = 'm') then
      begin
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
        inc(current, 2);
        measureUnit := muInch;
      end;
    'm': //convert mm to pixels
      if ((current+1)^ = 'm') then
      begin
        inc(current, 2);
        measureUnit := muMm;
      end;
    'p': //ignore px
      if (current+1)^ = 'x' then inc(current, 2);
    'r': //convert radian angles to degrees
      if ((current+1)^ = 'a') and ((current+2)^ = 'd') then
      begin
        inc(current, 3);
        measureUnit := muRadian;
      end;
  end;
end;
//------------------------------------------------------------------------------

function ParseNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean; overload;
var
  mu: TMeasureUnit;
begin
  Result := ParseNum(current, currentEnd, skipComma, val, mu);
  case mu of
    muPercent: val := val/100;
    muRadian: val := val * 180/PI;
    muInch    : val := val * 96;
    muCm      : val := val * 96 / 2.54;
    muMm      : val := val * 96 / 25.4;
    muEm      : val := val * 16;
    muEn      : val := val * 8;
  end;
end;
//------------------------------------------------------------------------------

function IsFraction(val: double): Boolean;
begin
  Result := (val <> 0) and (val > -1) and (val < 1);
end;
//------------------------------------------------------------------------------

function ColorIsURL(p: PAnsiChar): Boolean;
begin
  Result := (p^ = 'u') and ((p+1)^ = 'r') and ((p+2)^ = 'l');
end;
//------------------------------------------------------------------------------

function PCharToColor32(p: PAnsiChar; pLen: integer;
  var color: TColor32): Boolean;
var
  i: integer;
  j: Cardinal;
  clr: TColor32;
  alpha: Byte;
  vals: array[0..3] of double;
  pEnd: PAnsiChar;
begin
  Result := false;
  if (pLen < 3) then Exit;
  alpha := color shr 24;

  if (p^    = 'r') and                 //RGB / RGBA
    ((p+1)^ = 'g') and
    ((p+2)^ = 'b') then
  begin
    pEnd := p + pLen;
    inc(p, 3);
    if (p^ = 'a') then inc(p);
    if (ParseNextChar(p, pEnd) <> '(') or
      not ParseNum(p, pEnd, false, vals[0]) or
      not ParseNum(p, pEnd, true, vals[1]) or
      not ParseNum(p, pEnd, true, vals[2]) then Exit;
    if ParseNum(p, pEnd, false, vals[3]) then
      alpha := 0 else //stops further alpha adjustment
      vals[3] := 255;
    if ParseNextChar(p, pEnd) <> ')' then Exit;
    for i := 0 to 3 do if IsFraction(vals[i]) then
      vals[i] := vals[i] * 255;
    color := ClampByte(Round(vals[3])) shl 24 +
      ClampByte(Round(vals[0])) shl 16 +
      ClampByte(Round(vals[1])) shl 8 +
      ClampByte(Round(vals[2]));
  end
  else if (p^ = '#') then           //#RRGGBB or #RGB
  begin
    if (pLen = 7) then
    begin
      clr := $0;
      for i := 1 to 6 do
      begin
        inc(p);
        clr := clr shl 4 + HexByteToInt(p^);
      end;
      clr := clr or $FF000000;
    end
    else if (pLen = 4) then
    begin
      clr := $0;
      for i := 1 to 3 do
      begin
        inc(p);
        j := HexByteToInt(p^);
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
    i := ColorConstList.IndexOf(string(LowercaseAnsi(p, pLen)));
    if i < 0 then Exit;
    color := Cardinal(ColorConstList.Objects[i]);
  end;

  //and in case the opacity has been set before the color
  if (alpha > 0) and (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
  Result := true;
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

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeLowerCaseTable;
  MakeColorConstList;

finalization
  ColorConstList.Free;
end.
