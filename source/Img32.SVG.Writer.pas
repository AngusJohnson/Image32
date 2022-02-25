unit Img32.SVG.Writer;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.3                                                             *
* Date      :  21 September 2021                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Write SVG ver 2 files                                           *
*                                                                              *
*              This is just the very beginning, and very likely                *
*              sometime later it'll be merged with the SVG reader unit.        *
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
  Img32, Img32.SVG.Core, Img32.SVG.Path, Img32.Vector, Img32.Draw,
  Img32.Transform, Img32.Text;
{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

type

  TSvgElWriterClass = class of TBaseElWriter;

  TBaseElWriter = class
  private
    {$IFDEF XPLAT_GENERICS}
    fChilds   : TList<TBaseElWriter>;
    {$ELSE}
    fChilds   : TList;
    {$ENDIF}
    fParent   : TBaseElWriter;
    fIndent   : string;
    fElStr    : string;
  protected
    Id      : string;
    function  Write: string; virtual;
    function  WriteHeader: string; virtual;
    function  WriteContent: string; virtual;
    property Indent: string read fIndent;
    property Parent: TBaseElWriter read fParent;
  public
    constructor Create(parent: TBaseElWriter); virtual;
    destructor  Destroy; override;
    function  AddChild(childClass: TSvgElWriterClass): TBaseElWriter;
    procedure DeleteChild(index: integer);
    procedure Clear; virtual;
  end;

  TSvgElWriter = class(TBaseElWriter)
  private
//    fwidth    : integer;
//    fheight   : integer;
    fViewbox  : TRect;
  protected
    function WriteHeader: string; override;
  public
    constructor Create(parent: TBaseElWriter); override;
//    property width: integer read fwidth write fwidth;
//    property height: integer read fheight write fheight;
    property Viewbox: TRect read fViewbox write fViewbox;
  end;

  TExBaseElWriter = class(TBaseElWriter)
  protected
    fFillClr     : TColor32;
    fFillRule : TFillRule;
    fStrokeClr   : TColor32;
    fStrokeWidth : double;
    fDashes     : TArrayOfDouble;
    function WriteHeader: string; override;
  public
    Matrix      : TMatrixD;
    constructor Create(parent: TBaseElWriter); override;
    procedure Rotate(const pivotPt: TPointD; angleRad: double);
    procedure Translate(dx, dy: double);
    procedure Skew(dx, dy: double);
    property FillColor    : TColor32 read fFillClr write fFillClr;
    property StrokeColor  : TColor32 read fStrokeClr write fStrokeClr;
    property StrokeWidth  : double read fStrokeWidth write fStrokeWidth;
    property Dashes       : TArrayOfDouble read fDashes write fDashes;
    property FillRule     : TFillRule read fFillRule write fFillRule;
  end;

  TSvgGroupWriter = class(TExBaseElWriter)
  public
    constructor Create(parent: TBaseElWriter); override;
  end;

  TSvgPathWriter = class(TExBaseElWriter)
  private
    fLastPt   : TPointD;
    fSvgPaths : TSvgPath;
    function GetPathCount: integer;
    function GetCurrentPath: TSvgSubPath;
    function GetNewPath: TSvgSubPath;
  protected
    function  WriteHeader: string; override;
  public
    constructor Create(parent: TBaseElWriter); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure DeleteLastSegment(subPath: TSvgSubPath);
    procedure MoveTo(X,Y: double);
    procedure LineHTo(X: double);
    procedure LineVTo(Y: double);
    procedure LineTo(X,Y: double);
    procedure ArcTo(const radii: TPointD; angle: double;
      arcFlag, sweepFlag: Boolean; const endPt: TPointD); overload;
    procedure ArcTo(const endPt: TPointD; const rec: TRectD;
      angle: double; sweepFlag: Boolean); overload;
    procedure CubicBezierTo(const ctrl1, ctrl2, endPt: TPointD);
    procedure CubicSplineTo(const ctrl2, endPt: TPointD);
    procedure QuadBezierTo(const ctrl, endPt: TPointD);
    procedure QuadSplineTo(const endPt: TPointD);
    procedure ClosePath;
    property PathCount: integer read GetPathCount;
  end;

  TSvgCircleWriter = class(TExBaseElWriter)
  public
    Origin  : TPointD;
    Radius  : double;
    constructor Create(parent: TBaseElWriter); override;
    function  WriteHeader: string; override;
  end;

  TSvgEllipseWriter = class(TExBaseElWriter)
  public
    Origin  : TPointD;
    Radii   : TSizeD;
    constructor Create(parent: TBaseElWriter); override;
    function  WriteHeader: string; override;
  end;

  TSvgRectWriter = class(TExBaseElWriter)
  public
    RecWH   : TRectWH;
    Radii   : TSizeD;
    constructor Create(parent: TBaseElWriter); override;
    function  WriteHeader: string; override;
  end;

  TSvgPolygonWriter = class(TExBaseElWriter)
  public
    path  : TPathD;
    constructor Create(parent: TBaseElWriter); override;
    procedure Clear; override;
    function  WriteHeader: string; override;
  end;

  TSvgPolylineWriter = class(TSvgPolygonWriter)
  public
    constructor Create(parent: TBaseElWriter); override;
  end;

  TSVGFontInfo = record
    family      : TTtfFontFamily;
    size        : double;
    spacing     : double;
    textLength  : double;
    italic      : boolean;
    weight      : integer;
    align       : TSvgTextAlign;
    decoration  : TFontDecoration;
    baseShift   : TValue;
  end;

  TSvgTextWriter = class(TExBaseElWriter)
  protected
    fPosition: TPointD;
    fOffset: TSizeD;
    fFontInfo: TSVGFontInfo;
    function Write: string; override;
    function WriteHeader: string; override;
  public
    constructor Create(parent: TBaseElWriter); override;
    procedure AddText(const aText: string; X,Y: double; font: TFontCache);
  end;

  TSvgTSpanWriter = class(TSvgTextWriter)
  public
    constructor Create(parent: TBaseElWriter); override;
  end;

  TSvgSubTextWriter = class(TBaseElWriter)
  protected
    text: string;
  end;

  TSvgWriter = class
  private
    fSvgElememt : TSvgElWriter;
    function WriteHeader: string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure SaveToFile(const filename: string);
    procedure SaveToStream(stream: TStream);
    procedure Clear;
    property Svg: TSvgElWriter read fSvgElememt;
  end;

function GetFontInfo(font: TFontCache): TSVGFontInfo;

implementation

const
  indentSize = 2;

//------------------------------------------------------------------------------
// Miscellaneous routines
//------------------------------------------------------------------------------

function GetFontInfo(font: TFontCache): TSVGFontInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.family := font.FontReader.FontFamily;
  Result.size := font.FontHeight;
  Result.italic :=
    msItalic in font.FontReader.FontInfo.macStyles;
  Result.weight := font.FontReader.Weight;
end;
//------------------------------------------------------------------------------

procedure AppendStr(var s: string; const s2: string; omitSpace: Boolean = false);
begin
  if omitSpace then
    s := s + s2 else
    s := Format('%s%s ',[s, s2]);
end;
//------------------------------------------------------------------------------

procedure AppendStrAttrib(var s: string;
  const attribName, val: string);
begin
  s := Format('%s%s="%s" ', [s, attribName, val]);
end;
//------------------------------------------------------------------------------

procedure AppendInt(var s: string; val: double);
begin
  s := Format('%s%1.0f ',[s, val]);
end;
//------------------------------------------------------------------------------

function ValueToStr(val: double): string;
var
  absVal: double;
begin
  absVal := Abs(val);
  if Frac(absVal) < 0.01 then
    Result := Format('%1.0f', [val])
  else if Frac(absVal*10) < 0.01 then
    Result := Format('%1.1f', [val])
  else
    Result := Format('%1.2f', [val]);
end;
//------------------------------------------------------------------------------

procedure AppendFloat(var s: string; val: double);
begin
  s := Format('%s%s ', [s, ValueToStr(val)]);
end;
//------------------------------------------------------------------------------

procedure AppendFloatAttrib(var s: string;
  const attribName: string; val: double);
begin
  s := Format('%s%s="%s" ', [s, attribName, ValueToStr(val)]);
end;
//------------------------------------------------------------------------------

procedure AppendPoint(var s: string; X, Y: double); overload;
begin
  s := Format('%s%s,%s ',[s, ValueToStr(X), ValueToStr(Y)]);
end;
//------------------------------------------------------------------------------

procedure AppendPoint(var s: string; const pt: TPointD); overload;
begin
  s := Format('%s%s,%s ',[s, ValueToStr(pt.X), ValueToStr(pt.Y)]);
end;
//------------------------------------------------------------------------------

procedure AppendPathSegType(var s: string; segType: TSvgPathSegType);
var
  ch: UTF8Char;
begin
  case segType of
    stMove    : ch := 'M';
    stLine    : ch := 'L';
    stHorz    : ch := 'H';
    stVert    : ch := 'V';
    stArc     : ch := 'A';
    stQBezier : ch := 'Q';
    stCBezier : ch := 'C';
    stQSpline : ch := 'T';
    stCSpline : ch := 'S';
    else        ch := 'Z';
  end;
  s := Format('%s%s ',[s, ch]);
end;
//------------------------------------------------------------------------------

function ColorToRGBA(color: TColor32): string;
begin
  with TARGB(color) do
    case A of
      0:
        Result := 'none';
      255:
        begin
          case Color of
            clAqua32    : Result := 'aqua';
            clBlack32   : Result := 'black';
            clBlue32    : Result := 'blue';
            clFuchsia32 : Result := 'fuchsia';
            clGray32    : Result := 'gray';
            clGreen32   : Result := 'green';
            clLime32    : Result := 'lime';
            clMaroon32  : Result := 'maroon';
            clNavy32    : Result := 'navy';
            clOlive32   : Result := 'olive';
            clOrange32  : Result := 'orange';
            clPurple32  : Result := 'purple';
            clRed32     : Result := 'red';
            clSilver32  : Result := 'silver';
            clTeal32    : Result := 'teal';
            clWhite32   : Result := 'white';
            clYellow32  : Result := 'yellow';
            else Result := Format('rgb(%d, %d, %d)', [R, G, B]);
          end;
        end;
      else
        Result := Format('rgba(%d, %d, %d, %1.2n)', [R, G, B, A/255]);
    end;
end;
//------------------------------------------------------------------------------

procedure AppendColorAttrib(var s: string;
  const attribName: string; color: TColor32);
begin
  s := format('%s%s="%s" ', [s, attribName, ColorToRGBA(color)]);
end;

//------------------------------------------------------------------------------
// TSvgElementWriter
//------------------------------------------------------------------------------

constructor TBaseElWriter.Create(parent: TBaseElWriter);
begin
  fParent := parent;
  if Assigned(parent) and (indentSize > 0) then
    fIndent := parent.Indent + StringOfChar(#32, indentSize);
  {$IFDEF XPLAT_GENERICS}
  fChilds := TList<TBaseElWriter>.Create;
  {$ELSE}
  fChilds := TList.Create;
  {$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TBaseElWriter.Destroy;
begin
  Clear;
  fChilds.Free;
end;
//------------------------------------------------------------------------------

function TBaseElWriter.AddChild(childClass: TSvgElWriterClass): TBaseElWriter;
begin
  Result := childClass.Create(self);
  fChilds.Add(Result);
end;
//------------------------------------------------------------------------------

procedure TBaseElWriter.DeleteChild(index: integer);
begin
  if (index < 0) or (index >= fChilds.Count) then
    raise Exception.Create('TBaseElWriter.DeleteChild range error.');
  TBaseElWriter(fChilds[index]).Free;
  fChilds.Delete(index);
end;
//------------------------------------------------------------------------------

procedure TBaseElWriter.Clear;
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    TBaseElWriter(fChilds[i]).Free;
  fChilds.Clear;
end;
//------------------------------------------------------------------------------

function TBaseElWriter.Write: string;
begin
  Result := Format(#10'%s<%s ',[indent, fElStr]);
  AppendStr(Result, WriteHeader, true);
  if fChilds.Count > 0 then
  begin
    AppendStr(Result, '> ', true);
    AppendStr(Result, WriteContent, true);
    AppendStr(Result, Format(#10'%s</%s>',[indent, fElStr]), true);
  end else
    AppendStr(Result, '/>', true);
end;
//------------------------------------------------------------------------------

function  TBaseElWriter.WriteHeader: string;
begin
  Result := '';
end;
//------------------------------------------------------------------------------

function  TBaseElWriter.WriteContent: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to fChilds.Count -1 do
    AppendStr(Result, TBaseElWriter(fChilds[i]).Write, true);
end;

//------------------------------------------------------------------------------
// TExBaseElWriter
//------------------------------------------------------------------------------

constructor TExBaseElWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  Matrix        := IdentityMatrix;
  fFillClr      := clInvalid;
  fStrokeClr    := clInvalid;
  fStrokeWidth  := 1.0;
end;
//------------------------------------------------------------------------------

function TExBaseElWriter.WriteHeader: string;
var
  i,j: integer;
begin
  Result := inherited WriteHeader;
  if fFillClr <> clInvalid then
  begin
    AppendColorAttrib(Result, 'fill', fFillClr);
    if fFillRule = frEvenOdd then
      AppendStr(Result, 'fill-rule="evenodd"', false) else
      AppendStr(Result, 'fill-rule="nonzero"', false);
  end;
  if fStrokeClr <> clInvalid then
  begin
    AppendColorAttrib(Result, 'stroke', fStrokeClr);
    AppendFloatAttrib(Result, 'stroke-width', fStrokeWidth);
  end;
  if Assigned(fDashes) then
  begin
    AppendStr(Result, 'stroke-dasharray="', true);
    for i := 0 to High(fDashes) do
      AppendFloat(Result, fDashes[i]);
    AppendStr(Result, '"');
  end;
  if not IsIdentityMatrix(Matrix) then
  begin
    AppendStr(Result, 'transform="matrix(');
    for i := 0 to 1 do for j := 0 to 1 do
      AppendFloat(Result, Matrix[i][j]);
    AppendFloat(Result, Matrix[2][0]);
    AppendFloat(Result, Matrix[2][1]);
    AppendStr(Result, ')"');
  end;
end;
//------------------------------------------------------------------------------

procedure TExBaseElWriter.Rotate(const pivotPt: TPointD; angleRad: double);
begin
  MatrixRotate(Matrix, pivotPt, angleRad);
end;
//------------------------------------------------------------------------------

procedure TExBaseElWriter.Translate(dx, dy: double);
begin
  MatrixTranslate(Matrix, dx, dy);
end;
//------------------------------------------------------------------------------

procedure TExBaseElWriter.Skew(dx, dy: double);
begin
  MatrixSkew(Matrix, dx, dy);
end;

//------------------------------------------------------------------------------
// TSvgSvgWriter
//------------------------------------------------------------------------------

constructor TSvgElWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'svg';
end;
//------------------------------------------------------------------------------


function TSvgElWriter.WriteHeader: string;
const
  svgHeader  = 'width="%2:dpx" height="%3:dpx" viewBox="%0:d %1:d %2:d %3:d"';
  svgHeader2 = 'version="1.1" xmlns="http://www.w3.org/2000/svg"';
begin
  Result := '';
  with fViewbox do
    AppendStr(Result, Format(svgHeader, [left, top, right-left, bottom -top]));
  AppendStr(Result, svgHeader2);
end;

//------------------------------------------------------------------------------
// TSvgGroupWriter
//------------------------------------------------------------------------------

constructor TSvgGroupWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'g';
end;

//------------------------------------------------------------------------------
// TSvgPathWriter
//------------------------------------------------------------------------------

constructor TSvgPathWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fSvgPaths := TSvgPath.Create;
  fElStr := 'path';
  fFillClr := clBlack32;
end;
//------------------------------------------------------------------------------

destructor TSvgPathWriter.Destroy;
begin
  fSvgPaths.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TSvgPathWriter.GetPathCount: integer;
begin
  Result := fSvgPaths.Count;
end;
//------------------------------------------------------------------------------

function TSvgPathWriter.GetNewPath: TSvgSubPath;
begin
  //don't get a new path if the old current path is still empty
  Result := GetCurrentPath;
  if (Result.Count > 0) then
    Result := fSvgPaths.AddPath;
end;
//------------------------------------------------------------------------------

function TSvgPathWriter.GetCurrentPath: TSvgSubPath;
var
  len: integer;
begin
  len := fSvgPaths.Count;
  if len = 0 then
    Result := fSvgPaths.AddPath else
    Result := fSvgPaths[len -1];
end;
//------------------------------------------------------------------------------

function TSvgPathWriter.WriteHeader: string;
begin
  Result := inherited WriteHeader;
  Result := Result +
    Format('d="%s"', [fSvgPaths.GetStringDef(true, 2)]);
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.Clear;
begin
  inherited;
  fSvgPaths := nil;
  fLastPt := NullPointD;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.MoveTo(X,Y: double);
begin
  fLastPt := PointD(X,Y);
  GetNewPath;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.LineHTo(X: double);
var
  currPath  : TSvgSubPath;
  lastSeg   : TSvgPathSeg;
  path      : TPathD;
begin
  currPath  := GetCurrentPath;
  lastSeg   := currPath.GetLastSeg;
  path      := MakePath([X, fLastPt.Y]);
  if Assigned(lastSeg) and (lastSeg is TSvgHSegment) then
    lastSeg.ExtendSeg(path) else
    currPath.AddHSeg(fLastPt, path);
  fLastPt.X := X;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.LineVTo(Y: double);
var
  currPath  : TSvgSubPath;
  lastSeg   : TSvgPathSeg;
  path      : TPathD;
begin
  currPath  := GetCurrentPath;
  lastSeg   := currPath.GetLastSeg;
  path      := MakePath([fLastPt.X, Y]);
  if Assigned(lastSeg) and (lastSeg is TSvgVSegment) then
    lastSeg.ExtendSeg(path) else
    currPath.AddVSeg(fLastPt, path);
  fLastPt.Y := Y;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.LineTo(X,Y: double);
var
  currPath  : TSvgSubPath;
  lastSeg   : TSvgPathSeg;
  path      : TPathD;
begin
  currPath  := GetCurrentPath;
  lastSeg   := currPath.GetLastSeg;
  path      := MakePath([X, Y]);
  if Assigned(lastSeg) and (lastSeg is TSvgLSegment) then
    lastSeg.ExtendSeg(path) else
    currPath.AddLSeg(fLastPt, path);
  fLastPt := path[0];
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.ArcTo(const radii: TPointD; angle: double;
  arcFlag, sweepFlag: Boolean; const endPt: TPointD);
var
  currPath  : TSvgSubPath;
  rec       : TRectD;
begin
  rec := GetSvgArcInfoRect(fLastPt, endPt, radii, angle, arcFlag, sweepFlag);
  if rec.IsEmpty then Exit;
  currPath := GetCurrentPath;
  currPath.AddASeg(fLastPt, endPt, rec, angle, sweepFlag);
  fLastPt := endPt;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.ArcTo(const endPt: TPointD; const rec: TRectD;
  angle: double; sweepFlag: Boolean);
var
  currPath  : TSvgSubPath;
begin
  if rec.IsEmpty then Exit;
  currPath := GetCurrentPath;
  currPath.AddASeg(fLastPt, endPt, rec, angle, sweepFlag);
  fLastPt := endPt;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.CubicBezierTo(const ctrl1, ctrl2, endPt: TPointD);
var
  currPath  : TSvgSubPath;
  lastSeg   : TSvgPathSeg;
  path      : TPathD;
begin
  currPath  := GetCurrentPath;
  lastSeg   := currPath.GetLastSeg;
  SetLength(path, 3);
  path[0] := ctrl1; path[1] := ctrl2; path[2] := endPt;
  if Assigned(lastSeg) and (lastSeg is TSvgCSegment) then
    lastSeg.ExtendSeg(path) else
    currPath.AddCSeg(fLastPt, path);
  fLastPt := endPt;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.CubicSplineTo(const ctrl2, endPt: TPointD);
var
  currPath  : TSvgSubPath;
  lastSeg   : TSvgPathSeg;
  path      : TPathD;
begin
  currPath  := GetCurrentPath;
  lastSeg   := currPath.GetLastSeg;
  SetLength(path, 2);
  path[0] := ctrl2; path[1] := endPt;
  if Assigned(lastSeg) and (lastSeg is TSvgSSegment) then
    lastSeg.ExtendSeg(path) else
    currPath.AddSSeg(fLastPt, path);
  fLastPt := endPt;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.QuadBezierTo(const ctrl, endPt: TPointD);
var
  currPath  : TSvgSubPath;
  lastSeg   : TSvgPathSeg;
  path      : TPathD;
begin
  currPath  := GetCurrentPath;
  lastSeg   := currPath.GetLastSeg;
  SetLength(path, 2);
  path[0] := ctrl; path[1] := endPt;
  if Assigned(lastSeg) and (lastSeg is TSvgQSegment) then
    lastSeg.ExtendSeg(path) else
    currPath.AddQSeg(fLastPt, path);
  fLastPt := endPt;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.QuadSplineTo(const endPt: TPointD);
var
  currPath  : TSvgSubPath;
  lastSeg   : TSvgPathSeg;
  path      : TPathD;
begin
  currPath  := GetCurrentPath;
  lastSeg   := currPath.GetLastSeg;
  SetLength(path, 1);
  path[0] := endPt;
  if Assigned(lastSeg) and (lastSeg is TSvgTSegment) then
    lastSeg.ExtendSeg(path) else
    currPath.AddTSeg(fLastPt, path);
  fLastPt := endPt;
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.ClosePath;
var
  currPath  : TSvgSubPath;
begin
  currPath := GetCurrentPath;
  if (currPath.Count > 0) and not currPath.isClosed then
    currPath.AddZSeg(fLastPt, currPath.GetFirstPt);
end;
//------------------------------------------------------------------------------

procedure TSvgPathWriter.DeleteLastSegment(subPath: TSvgSubPath);
begin
  subPath.DeleteLastSeg;
end;

//------------------------------------------------------------------------------
// TSvgCircleWriter
//------------------------------------------------------------------------------

constructor TSvgCircleWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'circle';
end;
//------------------------------------------------------------------------------

function  TSvgCircleWriter.WriteHeader: string;
begin
  Result := inherited WriteHeader;
  AppendFloatAttrib(Result, 'cx', Origin.X);
  AppendFloatAttrib(Result, 'cy', Origin.Y);
  AppendFloatAttrib(Result, 'r', radius);
end;

//------------------------------------------------------------------------------
// TSvgEllipseWriter
//------------------------------------------------------------------------------

constructor TSvgEllipseWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'ellipse';
end;
//------------------------------------------------------------------------------

function  TSvgEllipseWriter.WriteHeader: string;
begin
  Result := inherited WriteHeader;
  AppendFloatAttrib(Result, 'cx', Origin.X);
  AppendFloatAttrib(Result, 'cy', Origin.Y);
  AppendFloatAttrib(Result, 'rx', radii.cx);
  AppendFloatAttrib(Result, 'ry', radii.cy);
end;

//------------------------------------------------------------------------------
// TSvgRectWriter
//------------------------------------------------------------------------------

constructor TSvgRectWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'rect';
end;
//------------------------------------------------------------------------------

function  TSvgRectWriter.WriteHeader: string;
begin
  Result := inherited WriteHeader;
  AppendFloatAttrib(Result, 'x', RecWH.Left);
  AppendFloatAttrib(Result, 'y', RecWH.Top);
  AppendFloatAttrib(Result, 'width', RecWH.Width);
  AppendFloatAttrib(Result, 'height', RecWH.Height);
  if radii.cx > 0 then
    AppendFloatAttrib(Result, 'rx', radii.cx);
  if radii.cy > 0 then
    AppendFloatAttrib(Result, 'ry', radii.cy);
end;

//------------------------------------------------------------------------------
// TSvgPolygonWriter
//------------------------------------------------------------------------------

constructor TSvgPolygonWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'polygon';
end;
//------------------------------------------------------------------------------

procedure TSvgPolygonWriter.Clear;
begin
  inherited;
  path := nil;
end;
//------------------------------------------------------------------------------

function  TSvgPolygonWriter.WriteHeader: string;
var
  i, len: integer;
  s: string;
begin
  Result := inherited WriteHeader;
  len := Length(path);
  if len = 0 then Exit;
  for i := 0 to len -1 do
    AppendPoint(s, path[i]);
  AppendStrAttrib(Result, 'points', s);
end;

//------------------------------------------------------------------------------
// TSvgPolylineWriter
//------------------------------------------------------------------------------

constructor TSvgPolylineWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'polyline';
end;

//------------------------------------------------------------------------------
// TSvgTextWriter
//------------------------------------------------------------------------------

constructor TSvgTextWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'text';
  fOffset.cx := InvalidD;
  fOffset.cy := InvalidD;
end;
//------------------------------------------------------------------------------

procedure TSvgTextWriter.AddText(const aText: string; X,Y: double; font: TFontCache);
begin
  with AddChild(TSvgSubTextWriter) as TSvgSubTextWriter do
  begin
    text := atext;
    fFontInfo := GetFontInfo(font);
    fPosition := PointD(X,Y);
  end;
end;
//------------------------------------------------------------------------------

function TSvgTextWriter.WriteHeader: string;
begin
  Result := inherited WriteHeader;
  with fFontInfo do
  begin
    case family of
      ttfUnknown:;
      ttfSerif      : AppendStrAttrib(Result, 'font-family', 'serif');
      ttfSansSerif  : AppendStrAttrib(Result, 'font-family', 'sans-serif');
      ttfMonospace  : AppendStrAttrib(Result, 'font-family', 'monospace');
    end;
    if size > 2 then
      AppendFloatAttrib(Result, 'font-size', size);
    if spacing <> 0 then
      AppendFloatAttrib(Result, 'font-spacing', spacing);
    if italic then
      AppendStrAttrib(Result, 'font-style', 'italic') else
      AppendStrAttrib(Result, 'font-style', 'normal');
    if (weight >= 100) and (weight <= 900) then
      AppendFloatAttrib(Result, 'font-weight', weight);
    case decoration of
      fdNone:          AppendStrAttrib(Result, 'text-decoration', 'none');
      fdUnderline:     AppendStrAttrib(Result, 'text-decoration', 'underline');
      fdStrikeThrough: AppendStrAttrib(Result, 'text-decoration', 'line-through');
    end;
  end;
  if fPosition.X <> InvalidD then
    AppendFloatAttrib(Result, 'x', fPosition.X);
  if fPosition.Y <> InvalidD then
    AppendFloatAttrib(Result, 'y', fPosition.Y);
  if fOffset.cx <> InvalidD then
    AppendFloatAttrib(Result, 'dx', fOffset.cx);
  if fOffset.cy <> InvalidD then
    AppendFloatAttrib(Result, 'dy', fOffset.cy);
end;
//------------------------------------------------------------------------------

function TSvgTextWriter.Write: string;
var
  i: integer;
begin
  if (Self is TSvgTSpanWriter) then
    Result := Format('<%s ',[fElStr]) else
    Result := Format(#10'%s<%s ',[indent, fElStr]);
  AppendStr(Result, WriteHeader, true);
  if fChilds.Count > 0 then
  begin
    AppendStr(Result, '>', true);
    for i := 0 to fChilds.Count -1 do
      if TBaseElWriter(fChilds[i]) is TSvgTSpanWriter then
        AppendStr(Result, TBaseElWriter(fChilds[i]).Write, true)
      else if TBaseElWriter(fChilds[i]) is TSvgSubTextWriter then
        AppendStr(Result, TSvgSubTextWriter(fChilds[i]).text, true);
    AppendStr(Result, Format('</%s>',[fElStr]), true);
  end else
    AppendStr(Result, '/>', true);
end;

//------------------------------------------------------------------------------
// TSvgTSpanWriter
//------------------------------------------------------------------------------

constructor TSvgTSpanWriter.Create(parent: TBaseElWriter);
begin
  inherited;
  fElStr := 'tspan';
  fPosition := InvalidPointD;
end;

//------------------------------------------------------------------------------
// TSvgWriter
//------------------------------------------------------------------------------

constructor TSvgWriter.Create;
begin
  inherited;
  fSvgElememt := TSvgElWriter.Create(nil);
end;
//------------------------------------------------------------------------------

destructor TSvgWriter.Destroy;
begin
  Clear;
  fSvgElememt.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.Clear;
begin
  fSvgElememt.Clear;
end;
//------------------------------------------------------------------------------

function TSvgWriter.WriteHeader: string;
const
  xmlHeader   = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>';
begin
  Result := xmlHeader;
  AppendStr(Result, fSvgElememt.Write, true);
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.SaveToFile(const filename: string);
var
  str: string;
begin
  str := WriteHeader;
  with TStringList.Create do
  try
  {$IFDEF UNICODE}
    text := str;
    SaveToFile(filename, TEncoding.UTF8);
  {$ELSE}
    text := UTF8Encode(str);
    SaveToFile(filename);
  {$ENDIF}
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgWriter.SaveToStream(stream: TStream);
var
  str: string;
begin
  str := WriteHeader;
  with TStringList.Create do
  try
  {$IFDEF UNICODE}
    text := str;
    SaveToStream (stream, TEncoding.UTF8);
  {$ELSE}
    text := UTF8Encode(str);
    SaveToStream (stream);
  {$ENDIF}
  finally
    free;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
