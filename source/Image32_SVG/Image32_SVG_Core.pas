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
  TElementMeasureUnit = (emuBoundingBox, emuUserSpace);
  TMeasureUnit = (muUndefined, muPixel, muPercent,
    muDegree, muRadian, muInch, muCm, muMm, muEm, muEx, muPt, muPica);

  TValue = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    rawVal  : double;
    mu      : TMeasureUnit;
    pcBelow : double; //manages % vs frac. ambiguity with untyped values
    procedure Init(asPercentBelow: double);
    procedure SetValue(val: double; measureUnit: TMeasureUnit = muUndefined);
    function  GetValue(scale: double; fontSize: double = 16.0): double;
    function  GetValueX(const scale: double; fontSize: double): double;
    function  GetValueY(const scale: double; fontSize: double): double;
    function  GetValueXY(const scaleRec: TRectD; fontSize: double): double;
    function  IsValid: Boolean;
    function  IsPercent: Boolean;
  end;

  TValuePt = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    X       : TValue;
    Y       : TValue;
    procedure Init(asPercentBelow: double);
    function  GetPoint(const scaleRec: TRectD; fontSize: double): TPointD;
    function  IsValid: Boolean;
  end;

  TValueRecWH = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    left    : TValue;
    top     : TValue;
    width   : TValue;
    height  : TValue;
    procedure Init(asPercentBelow: double);
    function  GetRectD(const scaleRec: TRectD; fontSize: double): TRectD;
    function  GetRectWH(const scaleRec: TRectD; fontSize: double): TRectWH;
    function  IsValid: Boolean;
    function  IsEmpty: Boolean;
  end;

  TAnsiRec = record
    ansi  : AnsiString;
  end;
  PAnsiRec = ^TAnsiRec;

  TClassStylesList = class
  private
    fList : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function  AddAppendStyle(const classname: string; const ansi: AnsiString): integer;
    function  GetStyle(const classname: AnsiString): AnsiString;
    procedure Clear;
  end;

  TSvgFontSyles = (sfsUndefined, sfsNone, sfsBold, sfsItalic, sfsBoldItalic);
  TFontDecoration = (fdUndefined, fdNone, fdUnderline, fdStrikeThrough);
  TSvgTextAlign = (staUndefined, staLeft, staCenter, staRight);

  TSVGFontInfo = record
    family      : TTtfFontFamily;
    size        : double;
    spacing     : double;
    textLength  : double;
    styles      : TSvgFontSyles;
    align       : TSvgTextAlign;
    decoration  : TFontDecoration;
    baseShift   : TValue;
  end;

  TSizeType = (stAverage, stWidth, stHeight);

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
    function GetBounds: TRectD;
    //scalePending parameter: if the SVG will be scaled later by this amount,
    //then make sure that curves are flattened to an appropriate precision
    function GetFlattenedPath(scalePending: double = 1.0): TPathD;
    function GetUncurvedPath: TPathD; //only needed for markers
  end;
  TDpaths = array of TDpath;

  TAnsiName = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    name: PAnsiChar;
    len : integer;
    function AsAnsiString: AnsiString;
    function Equals(const othername: TAnsiName): Boolean;
  end;
  TArrayOfAnsiName = array of TAnsiName;

function SkipBlanks(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
function SkipBlanksAndComma(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
function SkipStyleBlanks(var current: PAnsiChar; currentEnd: PAnsiChar): Boolean;
function ParseNameLength(var c: PAnsiChar; endC: PAnsiChar): integer;
function ParseStyleNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
function ParseNextChar(var current: PAnsiChar; currentEnd: PAnsiChar): AnsiChar;
function ParseNextWord(var current: PAnsiChar; currentEnd: PAnsiChar;
  out word: TAnsiName): Boolean;
function ParseNextWordEx(var current: PAnsiChar; currentEnd: PAnsiChar;
  out word: TAnsiName): Boolean;
function ParseNextWordHashed(var c: PAnsiChar; endC: PAnsiChar): cardinal;
function ParseNextNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double; out measureUnit: TMeasureUnit): Boolean; overload;
function ParseNextNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean; overload;
function TrimBlanks(var name: TAnsiName): Boolean;

function GetHashedName(const name: TAnsiName): cardinal;
function GetHashedNameCaseSens(name: PAnsiChar; nameLen: integer): cardinal;
function ExtractRefFromValue(const href: TAnsiName): TAnsiName;
function ExtractWordFromValue(value: TAnsiName; out word: TAnsiName): Boolean;
function IsNumPending(current, currentEnd: PAnsiChar; ignoreComma: Boolean): Boolean;

function GetUtf8String(pName: PAnsiChar; len: integer): AnsiString;
function LowercaseAnsi(const name: TAnsiName): AnsiString;
function IsAlpha(c: AnsiChar): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
function SvgArc(const p1, p2: TPointD; radii: TPointD; phi_rads: double;
  fA, fS: boolean; out startAngle, endAngle: double; out rec: TRectD): Boolean;
function HtmlDecode(const html: ansiString): ansistring;
function ColorIsURL(pColor: PAnsiChar): Boolean;
function ValueToColor32(const value: TAnsiName; var color: TColor32): Boolean;
function MakeDashArray(const dblArray: TArrayOfDouble; scale: double): TArrayOfInteger;

{$IF COMPILERVERSION < 17}
type
  TSetOfChar = set of Char;
function CharInSet(chr: Char; chrs: TSetOfChar): Boolean;
{$IFEND}

const
  clInvalid   = $00010001;
  clCurrent   = $00010002;
  sqrt2       = 1.4142135623731;
  quote       = '''';
  dquote      = '"';
  space       = #32;

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

function TDpath.GetFlattenedPath(scalePending: double): TPathD;
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

function TDpath.GetUncurvedPath: TPathD;
const
  buffSize = 32;
var
  i,j, pathLen, pathCap: integer;
  currPt, radii, pt2, pt3, pt4: TPointD;
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

begin
  pathLen := 0; pathCap := 0;
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
              AddPoint(PointD(vals[j*7 +5], vals[j*7 +6]));
        dsQBez:
          if High(vals) > 2 then
            for j := 0 to High(vals) div 4 do
            begin
              pt2.X := vals[j*4];
              pt2.Y := vals[j*4 +1];
              AddPoint(PointD(vals[j*4 +2], vals[j*4 +3]));
            end;
        dsQSpline:
          if High(vals) > 0 then
            for j := 0 to High(vals) div 2 do
              AddPoint(PointD(vals[j*2 +1], vals[j*2 +1]));
        dsCBez:
          if High(vals) > 4 then
            for j := 0 to High(vals) div 6 do
              AddPoint(PointD(vals[j*6 +4], vals[j*6 +5]));
        dsCSpline:
          if High(vals) > 2 then
            for j := 0 to High(vals) div 4 do
              AddPoint(PointD(vals[j*4 +2], vals[j*4 +3]));
      end;
    end;
  SetLength(Result, pathLen);
end;
//------------------------------------------------------------------------------

function TDpath.GetBounds: TRectD;
const
  buffSize = 32;
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
                path3 := Arc(rec, arc2, arc1, 1);
                path3 := ReversePath(path3);
              end else
                path3 := Arc(rec, arc1, arc2, 1);
              path3 := RotatePath(path3, rec.MidPoint, angle);
              AddPath(path3);
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
              path3 := FlattenQBezier(currPt, pt2, pt3, 1);
              AddPath(path3);
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
              path3 := FlattenQBezier(currPt, pt2, pt3, 1);
              AddPath(path3);
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
              path3 := FlattenCBezier(currPt, pt2, pt3, pt4, 1);
              AddPath(path3);
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
              path3 := FlattenCBezier(currPt, pt2, pt3, pt4, 1);
              AddPath(path3);
            end;
      end;
    end;
  SetLength(path2, pathLen);
  Result := GetBoundsD(path2);
end;
//------------------------------------------------------------------------------

function ConvertValue(const value: TValue;
  scale: double; fontSize: double): double;
const
  mm  = 96 / 25.4;
  cm  = 96 / 2.54;
  rad = 180 / PI;
  pt  = 4 / 3;
begin
  if fontSize = 0 then fontSize := 96;

  //https://oreillymedia.github.io/Using_SVG/guide/units.html
  //todo: still lots of units to support (eg times for animation)
  with value do
    if not IsValid or (rawVal = 0) then
      Result := 0
    else
      case value.mu of
        muUndefined:
          if (Abs(rawVal) < pcBelow) then
            Result := rawVal * scale else
            Result := rawVal;
        muPercent:
          Result := rawVal * 0.01 * scale;
        muRadian:
          Result := rawVal * rad;
        muInch:
          Result := rawVal * 96;
        muCm:
          Result := rawVal * cm;
        muMm:
          Result := rawVal * mm;
        muEm:
          Result := rawVal * fontSize;
        muEx:
          Result := rawVal * fontSize * 0.5;
        muPica:
          Result := rawVal * 16;
        muPt:
          Result := rawVal * pt;
        else
          Result := rawVal;
      end;
end;

//------------------------------------------------------------------------------
// TValue
//------------------------------------------------------------------------------

procedure TValue.Init(asPercentBelow: double);
begin
  rawVal  := InvalidD;
  mu      := muUndefined;
  pcBelow := asPercentBelow;
end;
//------------------------------------------------------------------------------

procedure TValue.SetValue(val: double; measureUnit: TMeasureUnit);
begin
  rawVal  := val;
  mu      := measureUnit;
end;
//------------------------------------------------------------------------------

function TValue.GetValue(scale: double; fontSize: double): double;
begin
  Result := ConvertValue(self, scale, fontSize);
end;
//------------------------------------------------------------------------------

function TValue.GetValueX(const scale: double; fontSize: double): double;
begin
  Result := ConvertValue(self, scale, fontSize);
end;
//------------------------------------------------------------------------------

function TValue.GetValueY(const scale: double; fontSize: double): double;
begin
  Result := ConvertValue(self, scale, fontSize);
end;
//------------------------------------------------------------------------------

function TValue.GetValueXY(const scaleRec: TRectD; fontSize: double): double;
begin
  //https://www.w3.org/TR/SVG11/coords.html#Units
  Result := ConvertValue(self,
    Hypot(scaleRec.Width, scaleRec.Height)/sqrt2, fontSize);
end;
//------------------------------------------------------------------------------

function TValue.IsValid: Boolean;
begin
  Result := Image32_Vector.IsValid(rawVal);
end;
//------------------------------------------------------------------------------

function TValue.IsPercent: Boolean;
begin
  case mu of
    muUndefined: Result := Abs(rawVal) < pcBelow;
    muPercent: Result := True;
    else Result := False;
  end;
end;

//------------------------------------------------------------------------------
// TValuePt
//------------------------------------------------------------------------------

procedure TValuePt.Init(asPercentBelow: double);
begin
  X.Init(asPercentBelow);
  Y.Init(asPercentBelow);
end;
//------------------------------------------------------------------------------

function TValuePt.GetPoint(const scaleRec: TRectD; fontSize: double): TPointD;
begin
  Result.X := X.GetValueX(scaleRec.Width, fontSize);
  Result.Y := Y.GetValueY(scaleRec.Height, fontSize);
end;
//------------------------------------------------------------------------------

function TValuePt.IsValid: Boolean;
begin
  Result := X.IsValid and Y.IsValid;
end;

//------------------------------------------------------------------------------
// TValueRec
//------------------------------------------------------------------------------

procedure TValueRecWH.Init(asPercentBelow: double);
begin
  left.Init(asPercentBelow);
  top.Init(asPercentBelow);
  width.Init(asPercentBelow);
  height.Init(asPercentBelow);
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectD(const scaleRec: TRectD; fontSize: double): TRectD;
begin
  with GetRectWH(scaleRec, fontSize) do
  begin
    Result.Left :=Left;
    Result.Top := Top;
    Result.Right := Left + Width;
    Result.Bottom := Top + Height;
  end;
end;
//------------------------------------------------------------------------------

function TValueRecWH.GetRectWH(const scaleRec: TRectD; fontSize: double): TRectWH;
begin
  if not left.IsValid then
    Result.Left := 0 else
    Result.Left := left.GetValueX(scaleRec.Width, fontSize);

  if not top.IsValid then
    Result.Top := 0 else
    Result.Top := top.GetValueY(scaleRec.Height, fontSize);

  Result.Width := width.GetValueX(scaleRec.Width, fontSize);
  Result.Height := height.GetValueY(scaleRec.Height, fontSize);
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
// TAnsiClassList
//------------------------------------------------------------------------------

constructor TClassStylesList.Create;
begin
  fList := TStringList.Create;
  fList.Duplicates := dupIgnore;
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

function TClassStylesList.AddAppendStyle(const classname: string; const ansi: AnsiString): integer;
var
  i: integer;
  sr: PAnsiRec;
begin
  Result := fList.IndexOf(classname);
  if (Result >= 0) then
  begin
    sr := PAnsiRec(fList.Objects[Result]);
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

function TClassStylesList.GetStyle(const classname: AnsiString): AnsiString;
var
  i: integer;
begin
  SetLength(Result, 0);
  i := fList.IndexOf(string(className));
  if i >= 0 then
    Result := PAnsiRec(fList.objects[i]).ansi;
end;
//------------------------------------------------------------------------------

procedure TClassStylesList.Clear;
var
  i: integer;
begin
  for i := 0 to fList.Count -1 do
    Dispose(PAnsiRec(fList.Objects[i]));
  fList.Clear;
end;

//------------------------------------------------------------------------------
// TAnsiName
//------------------------------------------------------------------------------

function TAnsiName.AsAnsiString: AnsiString;
begin
  SetLength(Result, len);
  Move(name^, Result[1], len);
end;
//------------------------------------------------------------------------------

function TAnsiName.Equals(const othername: TAnsiName): Boolean;
var
  i: integer;
  p1, p2: PAnsiChar;
begin
  Result := len = othername.len;
  if not Result or (len = 0) then Exit;
  p1 := name; p2 := othername.name;
  for i := 1 to len do
  begin
    if LowerCaseTable[p1^] <> LowerCaseTable[p2^] then Exit(false);
    inc(p1); inc(p2);
  end;
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

function ParseNextWord(var current: PAnsiChar;
  currentEnd: PAnsiChar; out word: TAnsiName): Boolean;
begin
  Result := SkipBlanksAndComma(current, currentEnd);
  if not Result then Exit;

  word.len := 0; word.name := current;
  while (current < currentEnd) and
    (LowerCaseTable[current^] >= 'a') and
    (LowerCaseTable[current^] <= 'z') do
  begin
    inc(word.len);
    inc(current);
  end;
  Result := word.len > 0;
end;
//------------------------------------------------------------------------------

function ParseNextWordEx(var current: PAnsiChar;
  currentEnd: PAnsiChar; out word: TAnsiName): Boolean;
var
  isQuoted: Boolean;
begin
  Result := SkipBlanksAndComma(current, currentEnd);
  if not Result then Exit;
  isQuoted := (current^) = quote;
  if isQuoted then
  begin
    inc(current);
    word.name := current;
    while (current < currentEnd) and (current^ <> quote) do inc(current);
    Result := current < currentEnd;
    if not Result then Exit;
    word.len := current - word.name;
    inc(current);
  end else
  begin

    Result := CharInSet(LowerCaseTable[current^], ['A'..'Z', 'a'..'z']);
    if not Result then Exit;
    word.len := 1; word.name := current;
    inc(current);
    while (current < currentEnd) and
      CharInSet(LowerCaseTable[current^], ['A'..'Z', 'a'..'z', '-', '_']) do
    begin
      inc(word.len);
      inc(current);
    end;
  end;
end;
//------------------------------------------------------------------------------

function ParseNextWordHashed(var c: PAnsiChar; endC: PAnsiChar): cardinal;
var
  name: TAnsiName;
begin
  name.name := c;
  name.len := ParseNameLength(c, endC);
  if name.len = 0 then Result := 0
  else Result := GetHashedName(name);
end;
//------------------------------------------------------------------------------

function ParseNextNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double; out measureUnit: TMeasureUnit): Boolean; overload;
var
  decPos,exp: integer;
  isNeg, expIsNeg: Boolean;
  start: PAnsiChar;
begin
  Result := false;
  measureUnit := muUndefined;

  //skip white space +/- single comma
  if skipComma then
  begin
    while (current < currentEnd) and (current^ <= #32) do inc(current);
    if (current^ = ',') then inc(current);
  end;
  while (current < currentEnd) and (current^ <= #32) do inc(current);
  if (current = currentEnd) then Exit;

//  if (current^ = 'n') and
//    (ParseNextWordHashed(current, current +4) = hNone) then
//  begin
//    inc(current, 4);
//    Result := true;
//    val := 0;
//    measureUnit := muUndefined;
//    Exit;
//  end;

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
    else if (LowerCaseTable[current^] = 'e') and
      (CharInSet((current+1)^, ['-','0'..'9'])) then
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
    'e': //convert cm to pixels
      if ((current+1)^ = 'm') then
      begin
        inc(current, 2);
        measureUnit := muEm;
      end
      else if ((current+1)^ = 'x') then
      begin
        inc(current, 2);
        measureUnit := muEx;
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
    'p':
      case (current+1)^ of
        'c':
          begin
            inc(current, 2);
            measureUnit := muPica;
          end;
        't':
          begin
            inc(current, 2);
            measureUnit := muPt;
          end;
        'x':
          begin
            inc(current, 2);
            measureUnit := muPixel;
          end;
      end;
    'r': //convert radian angles to degrees
      if ((current+1)^ = 'a') and ((current+2)^ = 'd') then
      begin
        inc(current, 3);
        measureUnit := muRadian;
      end;
  end;
end;
//------------------------------------------------------------------------------

function ParseNextNum(var current: PAnsiChar; currentEnd: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean; overload;
var
  tmp: TValue;
begin
  Result := ParseNextNum(current, currentEnd, skipComma, tmp.rawVal, tmp.mu);
  val := tmp.GetValue(1);
end;
//------------------------------------------------------------------------------

function TrimBlanks(var name: TAnsiName): Boolean;
var
  endC: PAnsiChar;
begin
  while (name.len > 0) and (name.name^ <= #32) do
  begin
    inc(name.name); dec(name.len);
  end;
  Result := name.len > 0;
  if not Result then Exit;
  endC := name.name + name.len -1;
  while endC^ <= #32 do
  begin
    dec(endC); dec(name.len);
  end;
end;
//------------------------------------------------------------------------------

{$OVERFLOWCHECKS OFF}
function GetHashedName(const name: TAnsiName): cardinal;
var
  i: integer;
  c: PAnsiChar;
begin
  //https://en.wikipedia.org/wiki/Jenkins_hash_function
  c := name.name;
  Result := 0;
  if c = nil then Exit;
  for i := 1 to name.len do
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

function GetHashedNameCaseSens(name: PAnsiChar; nameLen: integer): cardinal;
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
//------------------------------------------------------------------------------
{$OVERFLOWCHECKS ON}

function ExtractRefFromValue(const href: TAnsiName): TAnsiName; {$IFDEF INLINE} inline; {$ENDIF}
var
  curr, endR: PAnsiChar;
begin
  Result := href;
  endR := Result.name + href.len;
  if (Result.name^ = 'u') and
    ((Result.name +1)^ = 'r') and
    ((Result.name +2)^ = 'l') and
    ((Result.name +3)^ = '(') then
  begin
    inc(Result.name, 4);
    dec(endR); // avoid trailing ')'
  end;
  if Result.name^ = '#' then inc(Result.name);
  Result.len := 0;
  curr := Result.name;
  while (curr < endR) and (curr^ <> ')') do
  begin
    inc(Result.len);
    inc(curr);
  end;
end;
//------------------------------------------------------------------------------

function ExtractWordFromValue(value: TAnsiName; out word: TAnsiName): Boolean;
begin
  Result := ParseNextWord(value.name, value.name + value.len, word);
end;
//------------------------------------------------------------------------------

function IsNumPending(current, currentEnd: PAnsiChar; ignoreComma: Boolean): Boolean;
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

function GetUtf8String(pName: PAnsiChar; len: integer): AnsiString;
begin
  SetLength(Result, len);
  if len = 0 then Exit;
  Move(pName^, Result[1], len);
end;
//--------------------------------------------------------------------------

function LowercaseAnsi(const name: TAnsiName): AnsiString;
var
  i: integer;
  p: PAnsiChar;
begin
  SetLength(Result, name.len);
  p := name.name;
  for i := 1 to name.len do
  begin
    Result[i] := LowerCaseTable[p^];
    inc(p);
  end;
end;
//--------------------------------------------------------------------------

function IsAlpha(c: AnsiChar): Boolean; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := CharInSet(c, ['A'..'Z','a'..'z']);
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

function HtmlDecode(const html: ansiString): ansistring;
var
  val, len: integer;
  c,ce,endC: PAnsiChar;
begin
  len := Length(html);
  SetLength(Result, len*3);
  c := PAnsiChar(html);
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
        {$I html_entity_values.inc}
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

function ColorIsURL(pColor: PAnsiChar): Boolean;
begin
  Result := (pColor^ = 'u') and ((pColor+1)^ = 'r') and ((pColor+2)^ = 'l');
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

function IsFraction(val: double): Boolean;
begin
  Result := (val <> 0) and (val > -1) and (val < 1);
end;
//------------------------------------------------------------------------------

function ValueToColor32(const value: TAnsiName; var color: TColor32): Boolean;
var
  i     : integer;
  j     : Cardinal;
  clr   : TColor32;
  alpha : Byte;
  vals  : array[0..3] of double;
  mus   :  array[0..3] of TMeasureUnit;
  p     : PAnsiChar;
  pEnd  : PAnsiChar;
begin
  Result := false;
  p := value.name;
  if (value.len < 3) then Exit;

  if (color = clInvalid) or (color = clCurrent) or (color = clNone32) then
    alpha := 255 else
    alpha := color shr 24;

  if (p^    = 'r') and                 //RGB / RGBA
    ((p+1)^ = 'g') and
    ((p+2)^ = 'b') then
  begin
    pEnd := p + value.len;
    inc(p, 3);
    if (p^ = 'a') then inc(p);
    if (ParseNextChar(p, pEnd) <> '(') or
      not ParseNextNum(p, pEnd, false, vals[0], mus[0]) or
      not ParseNextNum(p, pEnd, true, vals[1], mus[1]) or
      not ParseNextNum(p, pEnd, true, vals[2], mus[2]) then Exit;
    for i := 0 to 2 do
      if mus[i] = muPercent then
        vals[i] := vals[i] * 255 / 100;

    if ParseNextNum(p, pEnd, true, vals[3], mus[3]) then
      alpha := 255 else //stops further alpha adjustment
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
    if (value.len = 7) then
    begin
      clr := $0;
      for i := 1 to 6 do
      begin
        inc(p);
        clr := clr shl 4 + HexByteToInt(p^);
      end;
      clr := clr or $FF000000;
    end
    else if (value.len = 4) then
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
    i := ColorConstList.IndexOf(string(LowercaseAnsi(value)));
    if i < 0 then Exit;
    color := Cardinal(ColorConstList.Objects[i]);
  end;

  //and in case the opacity has been set before the color
  if (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
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
    Result := nil
  else if len = 1 then
  begin
    SetLength(Result, 2);
    Result[1] := Result[0];
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
