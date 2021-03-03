unit Image32_Vector;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.0                                                             *
* Date      :  20 February 2021                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
* Purpose   :  Vector drawing for TImage32                                     *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Math, Types, Image32;

type
  TArrowStyle = (asNone, asSimple, asFancy, asDiamond, asCircle, asTail);
  TJoinStyle  = (jsAuto, jsSquare, jsMiter, jsRound);
  TEndStyle   = (esPolygon, esButt, esSquare, esRound, esClosed);
  TPathEnd    = (peStart, peEnd, peBothEnds);
  TSplineType = (stQuadratic, stCubic);
  TFillRule = (frEvenOdd, frNonZero, frPositive, frNegative);

  TMatrixD = array [0..2, 0..2] of double;

  function InflateRect(const rec: TRect; dx, dy: integer): TRect; overload;
  function InflateRect(const rec: TRectD; dx, dy: double): TRectD; overload;

  function Rectangle(const rec: TRect): TPathD; overload;
  function Rectangle(const rec: TRectD): TPathD; overload;
  function Rectangle(l, t, r, b: double): TPathD; overload;

  function RoundRect(const rec: TRect; radius: integer): TPathD; overload;
  function RoundRect(const rec: TRectD; radius: double): TPathD; overload;

  function Ellipse(const rec: TRect; steps: integer = 0): TPathD; overload;
  function Ellipse(const rec: TRectD; steps: integer = 0): TPathD; overload;

  function Circle(const pt: TPoint; radius: double; steps: integer = 0): TPathD; overload;
  function Circle(const pt: TPointD; radius: double; steps: integer = 0): TPathD; overload;

  function Star(const focalPt: TPointD;
    innerRadius, outerRadius: double; points: integer): TPathD;

  function Arc(const rec: TRect; startAngle, endAngle: double): TPathD;
  function Pie(const rec: TRect; StartAngle, EndAngle: double): TPathD;

  function FlattenQBezier(const pt1, pt2, pt3: TPointD): TPathD; overload;
  function FlattenQBezier(const pts: TPathD): TPathD; overload;
  function FlattenQBezier(const pts: TPathsD): TPathsD; overload;

  function FlattenCBezier(const pt1, pt2, pt3, pt4: TPointD): TPathD; overload;
  function FlattenCBezier(const pts: TPathD): TPathD; overload;
  function FlattenCBezier(const pts: TPathsD): TPathsD; overload;

  //FlattenCSpline: Approximates the 'S' command inside the 'd' property of an
  //SVG path. (See https://www.w3.org/TR/SVG/paths.html#DProperty)
  function FlattenCSpline(const pts: TPathD): TPathD;

  //FlattenQSpline: Approximates the 'T' command inside the 'd' property of an
  //SVG path. (See https://www.w3.org/TR/SVG/paths.html#DProperty)
  function FlattenQSpline(const pts: TPathD): TPathD;

  //ArrowHead: The ctrlPt's only function is to control the angle of the arrow.
  function ArrowHead(const arrowTip, ctrlPt: TPointD; size: double;
    arrowStyle: TArrowStyle): TPathD;

  function GetDefaultArrowHeadSize(lineWidth: double): double;

  function ShortenPath(const path: TPathD;
    pathEnd: TPathEnd; amount: double): TPathD;

  //RamerDouglasPeucker: simplifies paths, recursively removing vertices where
  //they deviate no more than 'epsilon' from their adjacent vertices.
  function RamerDouglasPeucker(const path: TPathD;
    epsilon: double): TPathD; overload;
  function RamerDouglasPeucker(const paths: TPathsD;
    epsilon: double): TPathsD; overload;

  //GetDashPath: Returns a polyline (not polygons)
  function GetDashedPath(const path: TPathD;
    closed: Boolean; const pattern: TArrayOfInteger;
    patternOffset: PDouble): TPathsD;

  function GetDashedOutLine(const path: TPathD;
    closed: Boolean; const pattern: TArrayOfInteger;
    patternOffset: PDouble; lineWidth: double;
    joinStyle: TJoinStyle; endStyle: TEndStyle): TPathsD;

  //CopyPaths: Because only dynamic string arrays are copy-on-write
  //function CopyPaths(const paths: TPathsD): TPathsD;

  function OffsetPoint(const pt: TPoint; dx, dy: integer): TPoint; overload;
  function OffsetPoint(const pt: TPointD; dx, dy: double): TPointD; overload;

  function OffsetPath(const path: TPathD;
    dx, dy: double): TPathD; overload;
  function OffsetPath(const paths: TPathsD;
    dx, dy: double): TPathsD; overload;
  function OffsetPath(const ppp: TArrayOfPathsD;
    dx, dy: double): TArrayOfPathsD; overload;

  function Paths(const path: TPathD): TPathsD;
  function CopyPaths(const paths: TPathsD): TPathsD;

  function ScalePath(const path: TPathD;
    sx, sy: double): TPathD; overload;
  function ScalePath(const path: TPathD;
    scale: double): TPathD; overload;
  function ScalePath(const paths: TPathsD;
    sx, sy: double): TPathsD; overload;
  function ScalePath(const paths: TPathsD;
    scale: double): TPathsD; overload;

  function ScaleRect(const rec: TRect; scale: double): TRect; overload;
  function ScaleRect(const rec: TRectD; scale: double): TRectD; overload;

  function ReversePath(const path: TPathD): TPathD; overload;
  function ReversePath(const paths: TPathsD): TPathsD; overload;

  function OpenPathToFlatPolygon(const path: TPathD): TPathD;
  function JoinPaths(const path1, path2: TPathD):TPathD;
  procedure AppendPoint(var path: TPathD; const extra: TPointD);
  procedure AppendPath(var paths: TPathsD;
    const extra: TPathD);
    {$IFDEF INLINE} inline; {$ENDIF} overload;
  procedure AppendPath(var paths: TPathsD;
    const extra: TPathsD);
    {$IFDEF INLINE} inline; {$ENDIF} overload;
  procedure AppendPath(var ppp: TArrayOfPathsD;
    const extra: TPathsD); overload;

  function GetAngle(const origin, pt: TPoint): double; overload;
  function GetAngle(const origin, pt: TPointD): double; overload;
  function GetAngle(const a, b, c: TPoint): double; overload;
  function GetAngle(const a, b, c: TPointD): double; overload;

  function GetPointAtAngleAndDist(const origin: TPointD;
    angle, distance: double): TPointD;

  function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD;
    out ip: TPointD): Boolean;

  procedure RotatePoint(var pt: TPointD;
    const focalPoint: TPointD; angleRad: double); overload;
  procedure RotatePoint(var pt: TPointD;
    const focalPoint: TPointD; sinA, cosA: double); overload;
  function RotatePath(const path: TPathD;
    const focalPoint: TPointD; angleRads: double): TPathD; overload;
  function RotatePath(const paths: TPathsD;
    const focalPoint: TPointD; angleRads: double): TPathsD; overload;

  function MakePathI(const pts: array of integer): TPathD; overload;
  function MakePathD(const pts: array of double): TPathD; overload;

  function GetBounds(const path: TPathD): TRect; overload;
  function GetBounds(const paths: TPathsD): TRect; overload;
  function GetBoundsD(const path: TPathD): TRectD; overload;
  function GetBoundsD(const paths: TPathsD): TRectD; overload;

  function GetRotatedRectBounds(const rec: TRect; angle: double): TRect; overload;
  function GetRotatedRectBounds(const rec: TRectD; angle: double): TRectD; overload;

  function Rect(const recD: TRectD): TRect; overload;
  function Rect(const left,top,right,bottom: integer): TRect; overload;

  function RectWH(left, top, width, height: integer): TRect;

  function Area(const path: TPathD): Double;
  function RectsEqual(const rec1, rec2: TRect): Boolean;
  procedure OffsetRect(var rec: TRect; dx, dy: integer); overload;
  procedure OffsetRect(var rec: TRectD; dx, dy: double); overload;

  function Point(X,Y: Integer): TPoint; overload;
  function Point(const pt: TPointD): TPoint; overload;

  function PointsEqual(const pt1, pt2: TPointD): Boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
  function PointsNearEqual(const pt1, pt2: TPointD; distSqrd: double): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
  function StripNearDuplicates(const path: TPathD;
    minLength: double; isClosedPath: Boolean): TPathD; overload;
  function StripNearDuplicates(const paths: TPathsD;
    minLength: double; isClosedPath: Boolean): TPathsD; overload;

  function MidPoint(const rec: TRect): TPoint; overload;
  function MidPoint(const rec: TRectD): TPointD; overload;

  function MidPoint(const pt1, pt2: TPoint): TPoint; overload;
  function MidPoint(const pt1, pt2: TPointD): TPointD; overload;

  function ReflectPoint(const pt, pivot: TPointD): TPointD;

  function IntersectRect(const rec1, rec2: TRect): TRect; overload;
  function IntersectRect(const rec1, rec2: TRectD): TRectD; overload;
  function UnionRect(const rec1, rec2: TRect): TRect; overload;
  function UnionRect(const rec1, rec2: TRectD): TRectD; overload;

  //these 2 functions are only needed to support older versions of Delphi
  function MakeArrayOfInteger(const ints: array of integer): TArrayOfInteger;
  function MakeArrayOfDouble(const doubles: array of double): TArrayOfDouble;

  function CrossProduct(const pt1, pt2, pt3: TPointD): double;
  function DotProduct(const pt1, pt2, pt3: TPointD): double;
  function TurnsLeft(const pt1, pt2, pt3: TPointD): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
  function TurnsRight(const pt1, pt2, pt3: TPointD): boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
  function IsPathConvex(const path: TPathD): Boolean;

  //GetUnitVector: Used internally
  function GetUnitVector(const pt1, pt2: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
  //GetUnitNormal: Used internally
  function GetUnitNormal(const pt1, pt2: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
  //GetVectors: Used internally
  function GetVectors(const path: TPathD): TPathD;
  //GetNormals: Used internally
  function GetNormals(const path: TPathD): TPathD;
  //DistanceSqrd: Used internally
  function DistanceSqrd(const pt1, pt2: TPoint): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}
  //DistanceSqrd: Used internally
  function DistanceSqrd(const pt1, pt2: TPointD): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}
  function Distance(const pt1, pt2: TPoint): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}
  function Distance(const pt1, pt2: TPointD): double; overload;
  {$IFDEF INLINE} inline; {$ENDIF}

  function PointInPolygon(const pt: TPointD;
    const polygon: TPathD; fillRule: TFillRule): Boolean;
  function PointInPolygons(const pt: TPointD;
    const polygons: TPathsD; fillRule: TFillRule): Boolean;

  function PerpendicularDist(const pt, line1, line2: TPointD): double;
  function ClosestPointOnLine(const pt, linePt1, linePt2: TPointD): TPointD;
  function ClosestPointOnSegment(const pt, segPt1, segPt2: TPointD): TPointD;

  function IsPointInEllipse(const ellipseRec: TRect; const pt: TPoint): Boolean;

  //GetIntersectsEllipseAndLine: Gets the intersection of an ellipse and
  //a line. The function result = true when the line either touches
  //tangentially or passes through the ellipse. If the line touches
  //tangentially, the coordintates returned in pt1 and pt2 will match.
  function GetLineEllipseIntersects(const ellipseRect: TRect;
    var linePt1, linePt2: TPointD): Boolean;

  function Outline(const line: TPathD; lineWidth: double;
    joinStyle: TJoinStyle; endStyle: TEndStyle;
    miterLimit: double = 0): TPathsD; overload;
  function Outline(const lines: TPathsD; lineWidth: double;
    joinStyle: TJoinStyle; endStyle: TEndStyle;
    miterLimit: double = 0): TPathsD; overload;

   //Grow: Offsets a closed path by 'delta' amount toward the left of the path.
   //Hence clockwise paths expand and counter-clockwise paths contract.<br>
   //No consideration is given to overlapping joins as these only very rarely
   //cause artifacts when paths are rendered.
  function Grow(const path, normals: TPathD; delta: double;
    joinStyle: TJoinStyle; miterLimit: double): TPathD;

  //SmoothToBezier: this function is based on -
  //"An Algorithm for Automatically Fitting Digitized Curves"
  //by Philip J. Schneider in "Graphics Gems", Academic Press, 1990
  function SmoothToBezier(const path: TPathD; closed: Boolean;
    tolerance: double; minSegLength: double = 2): TPathD; overload;
  function SmoothToBezier(const paths: TPathsD; closed: Boolean;
    tolerance: double; minSegLength: double = 2): TPathsD; overload;

  //Matrix functions
  function Matrix(const m00, m01, m02, m10, m11, m12, m20, m21, m22: double): TMatrixD;
  function MatrixDeterminant(const matrix: TMatrixD): double;
  function MatrixAdjugate(const matrix: TMatrixD): TMatrixD;
  function MatrixMultiply(const modifier, matrix: TMatrixD): TMatrixD;

  procedure MatrixApply(const matrix: TMatrixD;
    var pt: TPointD); overload;
  procedure MatrixApply(const matrix: TMatrixD;
    var path: TPathD); overload;
  procedure MatrixApply(const matrix: TMatrixD;
    var paths: TPathsD); overload;
  procedure MatrixInvert(var matrix: TMatrixD);

  //MatrixSkew: dx represents the delta offset of an X coordinate as a
  //fraction of its Y coordinate, and likewise for dy. For example, if dx = 0.1
  //and dy = 0, and the matrix is applied to the coordinate [20,15], then the
  //transformed coordinate will become [20 + (15 * 0.1),10], ie [21.5,10].
  procedure MatrixSkew(var matrix: TMatrixD; dx, dy: double);
  procedure MatrixScale(var matrix: TMatrixD; scale: double); overload;
  procedure MatrixScale(var matrix: TMatrixD; scaleX, scaleY: double); overload;
  procedure MatrixRotate(var matrix: TMatrixD;
    const center: TPointD; angRad: double);
  procedure MatrixTranslate(var matrix: TMatrixD; dx, dy: double);

const
  IdentityMatrix: TMatrixD = ((1, 0, 0),(0, 1, 0),(0, 0, 1));

  NullPoint: TPoint = (X: 0; Y: 0);
  NullPointD: TPointD = (X: 0; Y: 0);
  NullRect: TRect = (left: 0; top: 0; right: 0; Bottom: 0);
  NullRectD: TRectD = (left: 0; top: 0; right: 0; Bottom: 0);

var
  //AutoWidthThreshold: When JoinStyle = jsAuto, this is the threshold at
  //which line joins will be rounded instead of squared. With wider strokes,
  //rounded joins generally look better, but as rounding is more complex it
  //also requries more processing and hence is slower to execute.
  AutoWidthThreshold: double = 5.0;

  //Empirically, line widths of less than about 1.3 will produce noticeable
  //Moire Effect when the lines are close to horizontal or close to vertical.
  MinStrokeWidth: double = 0.5;

  //DefaultMiterLimit: Avoids excessive spikes when line offsetting
  DefaultMiterLimit: double = 2.0;

resourcestring
  rsInvalidMatrix = 'Invalid matrix.'; //nb: always start with IdentityMatrix

implementation

resourcestring
  rsInvalidQBezier = 'Invalid number of control points for a QBezier';
  rsInvalidCBezier = 'Invalid number of control points for a CBezier';
  rsInvalidScale   = 'Invalid matrix scaling factor (0)';

const
  CBezierTolerance  = 0.5;
  QBezierTolerance  = 0.5;
  BuffSize          = 64;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function RectsEqual(const rec1, rec2: TRect): Boolean;
begin
  result := (rec1.Left = rec2.Left) and (rec1.Top = rec2.Top) and
    (rec1.Right = rec2.Right) and (rec1.Bottom = rec2.Bottom);
end;
//------------------------------------------------------------------------------

function Point(X,Y: Integer): TPoint;
begin
  result.X := X;
  result.Y := Y;
end;
//------------------------------------------------------------------------------

function Point(const pt: TPointD): TPoint;
begin
  result.X := Round(pt.x);
  result.Y := Round(pt.y);
end;
//------------------------------------------------------------------------------

function PointsEqual(const pt1, pt2: TPointD): Boolean;
begin
  result := (pt1.X = pt2.X) and (pt1.Y = pt2.Y);
end;
//------------------------------------------------------------------------------

function PointsNearEqual(const pt1, pt2: TPointD; distSqrd: double): Boolean;
begin
  Result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y) < distSqrd;
end;
//------------------------------------------------------------------------------

function StripNearDuplicates(const path: TPathD;
  minLength: double; isClosedPath: Boolean): TPathD;
var
  i,j, len: integer;
begin
  len := length(path);
  SetLength(Result, len);
  if len = 0 then Exit;
  Result[0] := path[0];
  j := 0;
  minLength := minLength * minLength;
  for i := 1 to len -1 do
    if not PointsNearEqual(Result[j], path[i], minLength) then
    begin
      inc(j);
      Result[j] := path[i];
    end;
  if isClosedPath and
    PointsNearEqual(Result[j], Result[0], minLength) then dec(j);
  SetLength(Result, j +1);
end;
//------------------------------------------------------------------------------

function StripNearDuplicates(const paths: TPathsD;
  minLength: double; isClosedPath: Boolean): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := StripNearDuplicates(paths[i], minLength, isClosedPath);
end;
//------------------------------------------------------------------------------

function Rect(const left, top, right, bottom: integer): TRect;
begin
  Result.Left := left;
  Result.Top := top;
  Result.Right := right;
  Result.Bottom := bottom;
end;
//------------------------------------------------------------------------------

function RectWH(left, top, width, height: integer): TRect;
begin
  Result := Rect(left, top, left+width, top+height);
end;
//------------------------------------------------------------------------------

function GetRotatedRectBounds(const rec: TRect; angle: double): TRect;
begin
  Result := Rect(GetRotatedRectBounds(RectD(rec), angle));
end;
//------------------------------------------------------------------------------

function GetRotatedRectBounds(const rec: TRectD; angle: double): TRectD;
var
  s,c: extended;
  w,h: double;
  mp: TPointD;
begin
  NormalizeAngle(angle);
  if angle <> 0 then
  begin
    SinCos(angle, s, c);
    s := Abs(s); c := Abs(c);
    w := (rec.Width *c + rec.Height *s) /2;
    h := (rec.Width *s + rec.Height *c) /2;
    mp := MidPoint(rec);
    Result := RectD(mp.X - w, mp.Y - h, mp.X + w, mp.Y +h);
  end
  else
    Result := rec;
end;
//------------------------------------------------------------------------------

function Rect(const recD: TRectD): TRect;
begin
  Result.Left := Floor(recD.Left);
  Result.Top := Floor(recD.Top);
  Result.Right := Ceil(recD.Right);
  Result.Bottom := Ceil(recD.Bottom);
end;
//------------------------------------------------------------------------------

function Area(const path: TPathD): Double;
var
  i, j, highI: Integer;
  d: Double;
begin
  Result := 0.0;
  highI := High(path);
  if (highI < 2) then Exit;
  j := highI;
  for i := 0 to highI do
  begin
    d := (path[j].X + path[i].X);
    Result := Result + d * (path[j].Y - path[i].Y);
    j := i;
  end;
  Result := -Result * 0.5;
end;
//------------------------------------------------------------------------------

procedure OffsetRect(var rec: TRect; dx, dy: integer);
begin
  rec.Left := rec.Left + dx;
  rec.Top := rec.Top + dy;
  rec.Right := rec.Right + dx;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

procedure OffsetRect(var rec: TRectD; dx, dy: double);
begin
  rec.Left := rec.Left + dx;
  rec.Top := rec.Top + dy;
  rec.Right := rec.Right + dx;
  rec.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

function MidPoint(const rec: TRect): TPoint;
begin
  Result.X := (rec.Left + rec.Right) div 2;
  Result.Y := (rec.Top + rec.Bottom) div 2;
end;
//------------------------------------------------------------------------------

function MidPoint(const rec: TRectD): TPointD;
begin
  Result.X := (rec.Left + rec.Right) * 0.5;
  Result.Y := (rec.Top + rec.Bottom) * 0.5;
end;
//------------------------------------------------------------------------------

function MidPoint(const pt1, pt2: TPoint): TPoint;
begin
  Result.X := (pt1.X + pt2.X) div 2;
  Result.Y := (pt1.Y + pt2.Y) div 2;
end;
//------------------------------------------------------------------------------

function MidPoint(const pt1, pt2: TPointD): TPointD;
begin
  Result.X := (pt1.X + pt2.X) * 0.5;
  Result.Y := (pt1.Y + pt2.Y) * 0.5;
end;
//------------------------------------------------------------------------------

function IntersectRect(const rec1, rec2: TRect): TRect;
begin
  result.Left := Max(rec1.Left, rec2.Left);
  result.Top := Max(rec1.Top, rec2.Top);
  result.Right := Min(rec1.Right, rec2.Right);
  result.Bottom := Min(rec1.Bottom, rec2.Bottom);
  if (Result.Left > Result.Right) or
    (Result.Top > Result.Bottom) then Result := NullRect;
end;
//------------------------------------------------------------------------------

function IntersectRect(const rec1, rec2: TRectD): TRectD;
begin
  result.Left := Max(rec1.Left, rec2.Left);
  result.Top := Max(rec1.Top, rec2.Top);
  result.Right := Min(rec1.Right, rec2.Right);
  result.Bottom := Min(rec1.Bottom, rec2.Bottom);
end;
//------------------------------------------------------------------------------

function UnionRect(const rec1, rec2: TRect): TRect;
begin
  if IsEmptyRect(rec1) then
    Result := rec2
  else if IsEmptyRect(rec2) then
    Result := rec1
  else
  begin
    result.Left := Min(rec1.Left, rec2.Left);
    result.Top := Min(rec1.Top, rec2.Top);
    result.Right := Max(rec1.Right, rec2.Right);
    result.Bottom := Max(rec1.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function UnionRect(const rec1, rec2: TRectD): TRectD;
begin
  if IsEmptyRect(rec1) then
    Result := rec2
  else if IsEmptyRect(rec2) then
    Result := rec1
  else
  begin
    result.Left := Min(rec1.Left, rec2.Left);
    result.Top := Min(rec1.Top, rec2.Top);
    result.Right := Max(rec1.Right, rec2.Right);
    result.Bottom := Max(rec1.Bottom, rec2.Bottom);
  end;
end;
//------------------------------------------------------------------------------

function MakeArrayOfInteger(const ints: array of integer): TArrayOfInteger;
var
  i, len: integer;
begin
  len := Length(ints);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := ints[i];
end;
//------------------------------------------------------------------------------

function MakeArrayOfDouble(const doubles: array of double): TArrayOfDouble;
var
  i, len: integer;
begin
  len := Length(doubles);
  SetLength(Result, len);
  for i := 0 to len -1 do Result[i] := doubles[i];
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPointD): double;
var
  x1,x2,y1,y2: double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt3.X - pt2.X;
  y2 := pt3.Y - pt2.Y;
  result := (x1 * y2 - y1 * x2);
end;
//---------------------------------------------------------------------------

function DotProduct(const pt1, pt2, pt3: TPointD): double;
var
  x1,x2,y1,y2: double;
begin
  x1 := pt2.X - pt1.X;
  y1 := pt2.Y - pt1.Y;
  x2 := pt2.X - pt3.X;
  y2 := pt2.Y - pt3.Y;
  result := (x1 * x2 + y1 * y2);
end;
//---------------------------------------------------------------------------

function TurnsLeft(const pt1, pt2, pt3: TPointD): boolean;
begin
  result := CrossProduct(pt1, pt2, pt3) < 0;
end;
//---------------------------------------------------------------------------

function TurnsRight(const pt1, pt2, pt3: TPointD): boolean;
begin
  result := CrossProduct(pt1, pt2, pt3) > 0;
end;
//---------------------------------------------------------------------------

function IsPathConvex(const path: TPathD): Boolean;
var
  i, pathLen: integer;
  dir: boolean;
begin
  result := false;
  pathLen := length(path);
  if pathLen < 3 then Exit;
  //get the winding direction of the first angle
  dir := TurnsRight(path[0], path[1], path[2]);
  //check that each other angle has the same winding direction
  for i := 1 to pathLen -1 do
    if TurnsRight(path[i], path[(i+1) mod pathLen],
      path[(i+2) mod pathLen]) <> dir then Exit;
  result := true;
end;
//------------------------------------------------------------------------------

function GetUnitVector(const pt1, pt2: TPointD): TPointD;
{$IFDEF INLINE} inline; {$ENDIF}
var
  dx, dy, inverseHypot: Double;
begin
  if (pt1.x = pt2.x) and (pt1.y = pt2.y) then
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  inverseHypot := 1 / Hypot(dx, dy);
  dx := dx * inverseHypot;
  dy := dy * inverseHypot;
  Result.X := dx;
  Result.Y := dy;
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TPointD): TPointD;
{$IFDEF INLINE} inline; {$ENDIF}
var
  dx, dy, inverseHypot: Double;
begin
  if (pt1.x = pt2.x) and (pt1.y = pt2.y) then
  begin
    Result.X := 0;
    Result.Y := 0;
    Exit;
  end;
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);
  inverseHypot := 1 / Hypot(dx, dy);
  dx := dx * inverseHypot;
  dy := dy * inverseHypot;
  Result.X := dy;
  Result.Y := -dx
end;
//------------------------------------------------------------------------------

function Paths(const path: TPathD): TPathsD;
begin
  SetLength(Result, 1);
  result[0] := Copy(path, 0, length(path));
end;
//------------------------------------------------------------------------------

function CopyPaths(const paths: TPathsD): TPathsD;
var
  i, len1: integer;
begin
  len1 := length(paths);
  setLength(result, len1);
  for i := 0 to len1 -1 do
    result[i] := Copy(paths[i], 0, length(paths[i]));
end;
//------------------------------------------------------------------------------

function OffsetPoint(const pt: TPoint; dx, dy: integer): TPoint;
begin
  result.x := pt.x + dx;
  result.y := pt.y + dy;
end;
//------------------------------------------------------------------------------

function OffsetPoint(const pt: TPointD; dx, dy: double): TPointD;
begin
  result.x := pt.x + dx;
  result.y := pt.y + dy;
end;
//------------------------------------------------------------------------------

function OffsetPath(const path: TPathD; dx, dy: double): TPathD;
var
  i, len: integer;
begin
  len := length(path);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x + dx;
    result[i].y := path[i].y + dy;
  end;
end;
//------------------------------------------------------------------------------

function OffsetPath(const paths: TPathsD;
  dx, dy: double): TPathsD;
var
  i,len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := OffsetPath(paths[i], dx, dy);
end;
//------------------------------------------------------------------------------

function OffsetPath(const ppp: TArrayOfPathsD; dx, dy: double): TArrayOfPathsD;
var
  i,len: integer;
begin
  len := length(ppp);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := OffsetPath(ppp[i], dx, dy);
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPathD; sx, sy: double): TPathD;
var
  i, len: integer;
begin
  if sx = 0 then sx := 1;
  if sy = 0 then sy := 1;
  len := length(path);
  setLength(result, len);
  for i := 0 to len -1 do
  begin
    result[i].x := path[i].x * sx;
    result[i].y := path[i].y * sy;
  end;
end;
//------------------------------------------------------------------------------

function ScalePath(const path: TPathD;
  scale: double): TPathD;
begin
  result := ScalePath(path, scale, scale);
end;
//------------------------------------------------------------------------------

function ScalePath(const paths: TPathsD;
  sx, sy: double): TPathsD;
var
  i,len: integer;
begin
  len := length(paths);
  setLength(result, len);
  for i := 0 to len -1 do
    result[i] := ScalePath(paths[i], sx, sy);
end;
//------------------------------------------------------------------------------

function ScalePath(const paths: TPathsD;
  scale: double): TPathsD;
begin
  result := ScalePath(paths, scale, scale);
end;
//------------------------------------------------------------------------------

function ScaleRect(const rec: TRect; scale: double): TRect;
begin
  result := rec;
  Result.Left := Round(Result.Left * scale);
  Result.Top := Round(Result.Top * scale);
  Result.Right := Round(Result.Right * scale);
  Result.Bottom := Round(Result.Bottom * scale);
end;
//------------------------------------------------------------------------------

function ScaleRect(const rec: TRectD; scale: double): TRectD;
begin
  result := rec;
  Result.Left := Result.Left * scale;
  Result.Top := Result.Top * scale;
  Result.Right := Result.Right * scale;
  Result.Bottom := Result.Bottom * scale;
end;
//------------------------------------------------------------------------------

function ReversePath(const path: TPathD): TPathD;
var
  i, highI: integer;
begin
  highI := High(path);
  SetLength(result, highI +1);
  for i := 0 to highI do
    result[i] := path[highI -i];
end;
//------------------------------------------------------------------------------

function ReversePath(const paths: TPathsD): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(result, len);
  for i := 0 to len -1 do
    result[i] := ReversePath(paths[i]);
end;
//------------------------------------------------------------------------------

function OpenPathToFlatPolygon(const path: TPathD): TPathD;
var
  i, len, len2: integer;
begin
  len := Length(path);
  len2 := Max(0, len - 2);
  setLength(Result, len + len2);
  if len = 0 then Exit;
  Move(path[0], Result[0], len * SizeOf(TPointD));
  if len2 = 0 then Exit;
  for i := 0 to len - 3 do
    result[len + i] := path[len - 2 -i];
end;
//------------------------------------------------------------------------------

function GetVectors(const path: TPathD): TPathD;
var
  i,j, len: cardinal;
  pt: TPointD;
begin
  len := length(path);
  setLength(result, len);
  if len = 0 then Exit;
  pt := path[0];
  //skip duplicates
  i := len -1;
  while (i > 0) and
    (path[i].X = pt.X) and (path[i].Y = pt.Y) do dec(i);
  if (i = 0) then
  begin
    //all points are equal!
    for i := 0 to len -1 do result[i] := PointD(0,0);
    Exit;
  end;
  result[i] := GetUnitVector(path[i], pt);
  //fix up any duplicates at the end of the path
  for j := i +1 to len -1 do
    result[j] := result[j-1];
  //with at least one valid vector, we can now
  //safely get the remaining vectors
  pt := path[i];
  for i := i -1 downto 0 do
  begin
    if (path[i].X <> pt.X) or (path[i].Y <> pt.Y) then
    begin
      result[i] := GetUnitVector(path[i], pt);
      pt := path[i];
    end else
      result[i] := result[i+1]
  end;
end;
//------------------------------------------------------------------------------

function GetNormals(const path: TPathD): TPathD;
var
  i,j, len: cardinal;
  pt: TPointD;
begin
  len := length(path);
  setLength(result, len);
  if len = 0 then Exit;
  pt := path[0];
  //skip duplicates
  i := len -1;
  while (i > 0) and
    (path[i].X = pt.X) and (path[i].Y = pt.Y) do dec(i);
  if (i = 0) then
  begin
    //all points are equal!
    for i := 0 to len -1 do result[i] := PointD(0,0);
    Exit;
  end;
  result[i] := GetUnitNormal(path[i], pt);
  //fix up any duplicates at the end of the path
  for j := i +1 to len -1 do
    result[j] := result[j-1];
  //with at least one valid vector, we can now
  //safely get the remaining vectors
  pt := path[i];
  for i := i -1 downto 0 do
  begin
    if (path[i].X <> pt.X) or (path[i].Y <> pt.Y) then
    begin
      result[i] := GetUnitNormal(path[i], pt);
      pt := path[i];
    end else
      result[i] := result[i+1]
  end;
end;
//------------------------------------------------------------------------------

function DistanceSqrd(const pt1, pt2: TPoint): double;
begin
  result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function DistanceSqrd(const pt1, pt2: TPointD): double;
begin
  result := Sqr(pt1.X - pt2.X) + Sqr(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function Distance(const pt1, pt2: TPoint): double;
begin
  Result := Sqrt(DistanceSqrd(pt1, pt2));
end;
//------------------------------------------------------------------------------

function Distance(const pt1, pt2: TPointD): double;
begin
  Result := Sqrt(DistanceSqrd(pt1, pt2));
end;
//------------------------------------------------------------------------------

function PointInPolyWindingCount(const pt: TPointD;
  const path: TPathD; out PointOnEdgeDir: integer): integer;
var
  i, len: integer;
  prevPt: TPointD;
  isAbove: Boolean;
  crossProd: double;
begin
  //nb: PointOnEdgeDir == 0 unless 'pt' is on 'path'
  Result := 0;
  PointOnEdgeDir := 0;
  i := 0;
  len := Length(path);
  if len = 0 then Exit;
  prevPt := path[len-1];
  while (i < len) and (path[i].Y = prevPt.Y) do inc(i);
  if i = len then Exit;
  isAbove := (prevPt.Y < pt.Y);
  while (i < len) do
  begin
    if isAbove then
    begin
      while (i < len) and (path[i].Y < pt.Y) do inc(i);
      if i = len then break
      else if i > 0 then prevPt := path[i -1];
      crossProd := CrossProduct(prevPt, path[i], pt);
      if crossProd = 0 then
      begin
        PointOnEdgeDir := -1;
        //nb: could safely exit here with frNonZero or frEvenOdd fill rules
      end
      else if crossProd < 0 then dec(Result);
    end else
    begin
      while (i < len) and (path[i].Y > pt.Y) do inc(i);
      if i = len then break
      else if i > 0 then prevPt := path[i -1];
      crossProd := CrossProduct(prevPt, path[i], pt);
      if crossProd = 0 then
      begin
        PointOnEdgeDir := 1;
        //nb: could safely exit here with frNonZero or frEvenOdd fill rules
      end
      else if crossProd > 0 then inc(Result);
    end;
    inc(i);
    isAbove := not isAbove;
  end;
end;
//------------------------------------------------------------------------------

function PointInPolygon(const pt: TPointD;
  const polygon: TPathD; fillRule: TFillRule): Boolean;
var
  wc: integer;
  PointOnEdgeDir: integer;
begin
  wc := PointInPolyWindingCount(pt, polygon, PointOnEdgeDir);
  case fillRule of
    frEvenOdd: result := (PointOnEdgeDir <> 0)  or Odd(wc);
    frNonZero: result := (PointOnEdgeDir <> 0)  or (wc <> 0);
    frPositive: result := (PointOnEdgeDir + wc > 0);
    else {frNegative} result := (PointOnEdgeDir + wc < 0);
  end;
end;
//------------------------------------------------------------------------------

function PointInPolysWindingCount(const pt: TPointD;
  const paths: TPathsD; out PointOnEdgeDir: integer): integer;
var
  i,j, len: integer;
  p: TPathD;
  prevPt: TPointD;
  isAbove: Boolean;
  crossProd: double;
begin
  //nb: PointOnEdgeDir == 0 unless 'pt' is on 'path'
  Result := 0;
  PointOnEdgeDir := 0;
  for i := 0 to High(paths) do
  begin
    j := 0;
    p := paths[i];
    len := Length(p);
    if len < 3 then Continue;
    prevPt := p[len-1];
    while (j < len) and (p[j].Y = prevPt.Y) do inc(j);
    if j = len then continue;
    isAbove := (prevPt.Y < pt.Y);
    while (j < len) do
    begin
      if isAbove then
      begin
        while (j < len) and (p[j].Y < pt.Y) do inc(j);
        if j = len then break
        else if j > 0 then prevPt := p[j -1];
        crossProd := CrossProduct(prevPt, p[j], pt);
        if crossProd = 0 then PointOnEdgeDir := -1
        else if crossProd < 0 then dec(Result);
      end else
      begin
        while (j < len) and (p[j].Y > pt.Y) do inc(j);
        if j = len then break
        else if j > 0 then prevPt := p[j -1];
        crossProd := CrossProduct(prevPt, p[j], pt);
        if crossProd = 0 then PointOnEdgeDir := 1
        else if crossProd > 0 then inc(Result);
      end;
      inc(j);
      isAbove := not isAbove;
    end;
  end;
end;
//------------------------------------------------------------------------------

function PointInPolygons(const pt: TPointD;
  const polygons: TPathsD; fillRule: TFillRule): Boolean;
var
  wc: integer;
  PointOnEdgeDir: integer;
begin
  wc := PointInPolysWindingCount(pt, polygons, PointOnEdgeDir);
  case fillRule of
    frEvenOdd: result := (PointOnEdgeDir <> 0) or Odd(wc);
    frNonZero: result := (PointOnEdgeDir <> 0) or (wc <> 0);
    frPositive: result := (PointOnEdgeDir + wc > 0);
    else {frNegative} result := (PointOnEdgeDir + wc < 0);
  end;
end;
//------------------------------------------------------------------------------

function PerpendicularDist(const pt, line1, line2: TPointD): double;
var
  a,b,c,d: double;
begin
  //given: cross product of 2 vectors = area of parallelogram
  //and given: area of parallelogram = length base * height
  //height (ie perpendic. dist.) = cross product of 2 vectors / length base
  a := pt.X - line1.X;
  b := pt.Y - line1.Y;
  c := line2.X - line1.X;
  d := line2.Y - line1.Y;
  result := abs(a * d - c * b) / Sqrt(c * c + d * d);
end;
//------------------------------------------------------------------------------

function ClosestPoint(const pt, linePt1, linePt2: TPointD;
  constrainToSegment: Boolean): TPointD;
var
  q: double;
begin
  if (linePt1.X = linePt2.X) and (linePt1.Y = linePt2.Y) then
  begin
    Result := linePt1;
  end else
  begin
    q := ((pt.X-linePt1.X)*(linePt2.X-linePt1.X) +
      (pt.Y-linePt1.Y)*(linePt2.Y-linePt1.Y)) /
      (sqr(linePt2.X-linePt1.X) + sqr(linePt2.Y-linePt1.Y));
    if constrainToSegment then
    begin
      if q < 0 then q := 0 else if q > 1 then q := 1;
    end;
    Result.X := round((1-q)*linePt1.X + q*linePt2.X);
    Result.Y := round((1-q)*linePt1.Y + q*linePt2.Y);
  end;
end;
//------------------------------------------------------------------------------

function ClosestPointOnLine(const pt, linePt1, linePt2: TPointD): TPointD;
begin
  result := ClosestPoint(pt, linePt1, linePt2, false);
end;
//------------------------------------------------------------------------------

function ClosestPointOnSegment(const pt, segPt1, segPt2: TPointD): TPointD;
begin
  result := ClosestPoint(pt, segPt1, segPt2, true);
end;
//------------------------------------------------------------------------------

function IsPointInEllipse(const ellipseRec: TRect; const pt: TPoint): Boolean;
var
  rec: TRectD;
  x,y, y2, a,b, dx,dy: double;
begin
  a := RectWidth(ellipseRec) *0.5;
  b := RectHeight(ellipseRec) *0.5;
  dx := ellipseRec.Left + a;
  dy := ellipseRec.Top + b;
  rec := RectD(ellipseRec);
  OffsetRect(rec, -dx, -dy);
  x := pt.X -dx; y := pt.Y -dy;
  //first make sure pt is inside rect
  Result := (abs(x) <= a) and (abs(y) <= b);
  if not result then Exit;
  //given (x*x)/(a*a) + (y*y)/(b*b) = 1
  //then y*y = b*b(1 - (x*x)/(a*a))
  //nb: contents of Sqrt below will always be positive
  //since the substituted x must be within ellipseRec bounds
  y2 := Sqrt((b*b*(1 - (x*x)/(a*a))));
  Result := (y >= -y2) and (y <= y2);
end;
//------------------------------------------------------------------------------

function GetLineEllipseIntersects(const ellipseRect: TRect;
  var linePt1, linePt2: TPointD): Boolean;
var
  dx, dy, m,a,b,c,q: double;
  qa,qb,qc,qs: double;
  rec: TRectD;
  pt1, pt2: TPointD;
begin
  a := RectWidth(ellipseRect) *0.5;
  b := RectHeight(ellipseRect) *0.5;
  //offset ellipseRect so it's centered over the coordinate origin
  dx := ellipseRect.Left + a; dy := ellipseRect.Top + b;
  rec := RectD(ellipseRect);
  offsetRect(rec, -dx, -dy);
  pt1 := OffsetPoint(linePt1, -dx, -dy);
  pt2 := OffsetPoint(linePt2, -dx, -dy);
  //equation of ellipse = (x*x)/(a*a) + (y*y)/(b*b) = 1
  //equation of line = y = mx + c;
  if (pt1.X = pt2.X) then //vertical line (ie infinite slope)
  begin
    //given x = K, then y*y = b*b(1 - (x*x)/(a*a))
    q := (b*b)*(1 - Sqr(pt1.X)/(a*a));
    result := q >= 0;
    if not result then Exit;
    q := Sqrt(q);
    pt1.Y := q;
    pt2.Y := -q;
  end else
  begin
    //using simultaneous equations and substitution
    //given y = mx + c
    m := (pt1.Y - pt2.Y)/(pt1.X - pt2.X);
    c := pt1.Y - m * pt1.X;
    //given (x*x)/(a*a) + (y*y)/(b*b) = 1
    //(x*x)/(a*a)*(b*b) + (y*y) = (b*b)
    //(b*b)/(a*a) *(x*x) + Sqr(m*x +c) = (b*b)
    //(b*b)/(a*a) *(x*x) + (m*m)*(x*x) + 2*m*x*c +c*c = b*b
    //((b*b)/(a*a) +(m*m)) *(x*x) + 2*m*c*(x) + (c*c) - (b*b) = 0
    //solving quadratic equation
    qa := ((b*b)/(a*a) +(m*m));
    qb := 2*m*c;
    qc := (c*c) - (b*b);
    qs := (qb*qb) - 4*qa*qc;
    Result := qs >= 0;
    if not result then Exit;
    qs := Sqrt(qs);
    pt1.X := (-qb +qs)/(2 * qa);
    pt1.Y := m * pt1.X + c;
    pt2.X := (-qb -qs)/(2 * qa);
    pt2.Y := m * pt2.X + c;
  end;
  //finally reverse initial offset
  linePt1 := OffsetPoint(pt1, dx, dy);
  linePt2 := OffsetPoint(pt2, dx, dy);
end;
//------------------------------------------------------------------------------

function Grow(const path, normals: TPathD; delta: double;
  joinStyle: TJoinStyle; miterLimit: double): TPathD;
var
  i,prevI, highI: cardinal;
  buffLen, resultLen: cardinal;
  steps360, stepsPerRad: double;
  stepSin, stepCos, cosA: extended;
  P: TPointD;
  norms: TPathD;
  normalA, normalB: TPointD;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultLen >= buffLen then
    begin
      inc(buffLen, 128);
      setLength(result, buffLen);
    end;
    result[resultLen] := pt;
    inc(resultLen);
  end;

  function GetNormal(const pt: TPointD; const norm: TPointD): TPointD;
  begin
    result := PointD(pt.x + norm.x * delta, pt.y + norm.y * delta);
  end;

  procedure AddMiter(const N1, N2: TPointD; cosAplus1: double);
  var
    q: double;
  begin
    q := delta / cosAplus1;
    AddPoint(PointD(P.X + (N2.X + N1.X) * q,
      P.Y + (N1.Y + N2.Y) * q));
  end;

  procedure AddBevel(const N1, N2: TPointD);
  begin
    AddPoint(GetNormal(P, N1));
    AddPoint(GetNormal(P, N2));
  end;

  procedure AddRound(const N1, N2: TPointD; cosA: double);
  var
    i, steps: Integer;
    a, sinA: Double;
    P2: TPointD;
  begin
    sinA := normalA.x * normalB.y - normalA.y * normalB.x;
    if (delta * sinA < 0) then //concave
      AddBevel(N1, N2)
    else
    begin
      a := ArcTan2(sinA, cosA);
      steps := Round(stepsPerRad * Abs(a));
      P2 := PointD(N1.x * delta, N1.y * delta);
      AddPoint(PointD(P.x + P2.x, P.y + P2.y));
      for i := 1 to steps do
      begin
        P2 := PointD(P2.x * stepCos - stepSin * P2.y,
          P2.x * stepSin + P2.y * stepCos);
        AddPoint(PointD(P.x + P2.x, P.y + P2.y));
      end;
    end;
  end;

begin
  highI := high(path);
  if (highI < 1) then exit;
  if delta < MinStrokeWidth/2 then delta := MinStrokeWidth/2;

  if delta < 1 then        //ie linewidth < 2
    joinStyle := jsSquare
  else if joinStyle = jsAuto then
  begin
    if delta < AutoWidthThreshold / 2 then
      joinStyle := jsSquare else
      joinStyle := jsRound;
  end;

  if joinStyle = jsRound then
  begin
    steps360 := PI / ArcCos(1 - 0.2 / abs(delta));
    stepsPerRad := steps360 / (Pi *2);
    SinCos(Pi*2/steps360, stepSin, stepCos);
  end;

  result := nil; resultLen := 0; buffLen := 0;
  if miterLimit < 1 then miterLimit := DefaultMiterLimit
  else miterLimit := 2/(sqr(miterLimit));

  norms := normals;
  if not assigned(norms) then
    norms := GetNormals(path);

  PrevI := HighI;
  for i := 0 to HighI do
  begin
    P := path[i];
    normalA := norms[PrevI];
    normalB := norms[i];
    cosA := normalB.x * normalA.x + normalB.y * normalA.y;

    case
      joinStyle of
        jsRound: AddRound(normalA, normalB, cosA);
      jsSquare:
        if cosA > 0 then AddMiter(normalA, normalB, cosA +1)
        else AddBevel(normalA, normalB);
      jsMiter:
        if (1 + cosA < miterLimit) then AddBevel(normalA, normalB)
        else AddMiter(normalA, normalB, cosA +1);
    end;
    PrevI := i;
  end;
  setLength(result, resultLen);
end;
//------------------------------------------------------------------------------

function JoinPaths(const path1, path2: TPathD):TPathD;
var
  len1, len2: integer;
begin
  len1 := length(path1);
  len2 := length(path2);
  if len1 > 0 then
    Result := Copy(path1, 0, len1);
  if len2 = 0 then Exit;
  if (len1 > 0) and PointsEqual(path2[0], path1[len1 -1]) then
  begin
    if len2 = 1 then Exit;
    dec(len2);
    setLength(Result, len1 + len2);
    Move(path2[1], Result[len1], len2 * SizeOf(TPointD));
  end else
  begin
    setLength(Result, len1 + len2);
    Move(path2[0], Result[len1], len2 * SizeOf(TPointD));
  end;
end;
//------------------------------------------------------------------------------

procedure AppendPoint(var path: TPathD; const extra: TPointD);
var
  len: integer;
begin
  len := length(path);
  SetLength(path, len +1);
  path[len] := extra;
end;
//------------------------------------------------------------------------------

procedure AppendPath(var paths: TPathsD;
  const extra: TPathD);
var
  len1, len2: integer;
begin
  len2 := length(extra);
  if len2 = 0 then Exit;
  len1 := length(paths);
  setLength(paths, len1 + 1);
  paths[len1] := Copy(extra, 0, len2);
end;
//------------------------------------------------------------------------------

procedure AppendPath(var paths: TPathsD;
  const extra: TPathsD);
var
  i, len1, len2: integer;
begin
  len2 := length(extra);
  if len2 = 0 then Exit;
  len1 := length(paths);
  setLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1+i] := Copy(extra[i], 0, length(extra[i]));
end;
//------------------------------------------------------------------------------

procedure AppendPath(var ppp: TArrayOfPathsD;
  const extra: TPathsD);
var
  len: integer;
begin
  len := length(ppp);
  setLength(ppp, len + 1);
  if Assigned(extra) then
    AppendPath(ppp[len], extra) else
    ppp[len] := nil;
end;
//------------------------------------------------------------------------------

procedure RotatePoint(var pt: TPointD;
  const focalPoint: TPointD; angleRad: double);
var
  {$IFDEF FPC}
  sinA, cosA: double;
  {$ELSE}
  sinA, cosA: {$IF COMPILERVERSION = 15} extended; {$ELSE} double; {$IFEND}
  {$ENDIF}
begin
  SinCos(angleRad, sinA, cosA);
  RotatePoint(pt, focalPoint, sinA, cosA);
end;
//------------------------------------------------------------------------------

procedure RotatePoint(var pt: TPointD;
  const focalPoint: TPointD; sinA, cosA: double);
var
  tmpX, tmpY: double;
begin
  tmpX := pt.X-focalPoint.X;
  tmpY := pt.Y-focalPoint.Y;
  pt.X := tmpX * cosA - tmpY * sinA + focalPoint.X;
  pt.Y := tmpX * sinA + tmpY * cosA + focalPoint.Y;
end;
//------------------------------------------------------------------------------

function RotatePathInternal(const path: TPathD;
  const focalPoint: TPointD; sinA, cosA: extended): TPathD;
var
  i: integer;
  x,y: double;
begin
  SetLength(Result, length(path));
  for i := 0 to high(path) do
  begin
    x := path[i].X - focalPoint.X;
    y := path[i].Y - focalPoint.Y;
    Result[i].X := x * cosA - y * sinA + focalPoint.X;
    Result[i].Y := x * sinA + y * cosA + focalPoint.Y;
  end;
end;
//------------------------------------------------------------------------------

function RotatePath(const path: TPathD;
  const focalPoint: TPointD; angleRads: double): TPathD;
var
  sinA, cosA: extended;
begin
  Math.SinCos(angleRads, sinA, cosA);
  Result := RotatePathInternal(path, focalPoint, sinA, cosA);
end;
//------------------------------------------------------------------------------

function RotatePath(const paths: TPathsD;
  const focalPoint: TPointD; angleRads: double): TPathsD;
var
  i: integer;
  sinA, cosA: extended;
begin
  Math.SinCos(angleRads, sinA, cosA);
  SetLength(Result, length(paths));
  for i := 0 to high(paths) do
    Result[i] := RotatePathInternal(paths[i], focalPoint, sinA, cosA);
end;
//------------------------------------------------------------------------------

function GetAngle(const origin, pt: TPoint): double;
var
  x,y: double;
begin
  x := pt.X - origin.X;
  y := pt.Y - origin.Y;
  if x = 0 then
  begin
    if y > 0 then result := angle270
    else result := angle90;
  end else
  begin
    result := arctan2(-y, x);
    if result < 0 then result := result + angle360;
  end;
end;
//------------------------------------------------------------------------------

function GetAngle(const origin, pt: TPointD): double;
var
  x,y: double;
begin
  x := pt.X - origin.X;
  y := pt.Y - origin.Y;
  if x = 0 then
  begin
    if y > 0 then result := angle270
    else result := angle90;
  end else
  begin
    result := arctan2(-y, x);
    if result < 0 then result := result + angle360;
  end;
end;
//------------------------------------------------------------------------------

function GetAngle(const a, b, c: TPoint): double;
var
  ab,bc: TPointD;
  dotProd, abSqr, bcSqr, cosSqr, cos2, alpha2: single;
begin
  //http://stackoverflow.com/questions/3486172/angle-between-3-points/3487062
  ab.X := b.X - a.X;
  ab.Y := b.Y - a.Y;
  bc.X := b.X - c.X;
  bc.Y := b.Y - c.Y;
  dotProd := ab.X * bc.X + ab.Y * bc.Y;
  abSqr := ab.x * ab.x + ab.y * ab.y;
  bcSqr := bc.x * bc.x + bc.y * bc.y;
  cosSqr := dotProd * dotProd / abSqr / bcSqr;
  cos2 := 2 * cosSqr - 1;
  if (cos2 <= -1) then alpha2 := pi
  else if (cos2 >= 1) then alpha2 := 0
  else alpha2 := arccos(cos2);
  result := alpha2 / 2;
  if (dotProd < 0) then result := pi - result;
  if (ab.x * bc.y - ab.y * bc.x) < 0 then result := -result;
end;
//---------------------------------------------------------------------------

function GetAngle(const a, b, c: TPointD): double;
var
  ab,bc: TPointD;
  dotProd, abSqr, bcSqr, cosSqr, cos2, alpha2: single;
begin
  ab.X := b.X - a.X;
  ab.Y := b.Y - a.Y;
  bc.X := b.X - c.X;
  bc.Y := b.Y - c.Y;
  dotProd := ab.X * bc.X + ab.Y * bc.Y;
  abSqr := ab.x * ab.x + ab.y * ab.y;
  bcSqr := bc.x * bc.x + bc.y * bc.y;
  if (abSqr = 0) or (bcSqr = 0) then
  begin
    Result := 0;
    Exit;
  end;
  cosSqr := dotProd * dotProd / abSqr / bcSqr;
  cos2 := 2 * cosSqr - 1;
  if (cos2 <= -1) then alpha2 := pi
  else if (cos2 >= 1) then alpha2 := 0
  else alpha2 := arccos(cos2);
  result := alpha2 / 2;
  if (dotProd < 0) then result := pi - result;
  if (ab.x * bc.y - ab.y * bc.x) < 0 then result := -result;
end;
//------------------------------------------------------------------------------

function GetPointAtAngleAndDist(const origin: TPointD;
  angle, distance: double): TPointD;
begin
  Result := origin;
  Result.X := Result.X + distance;
  RotatePoint(Result, origin, angle);
end;
//------------------------------------------------------------------------------


function IntersectPoint(const ln1a, ln1b, ln2a, ln2b: TPointD; out ip: TPointD): Boolean;
var
  m1,b1,m2,b2: double;
begin
  result := false;
  //see http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
  if (ln1B.X = ln1A.X) then
  begin
    if (ln2B.X = ln2A.X) then exit; //parallel lines
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    ip.X := ln1A.X;
    ip.Y := m2*ln1A.X + b2;
  end
  else if (ln2B.X = ln2A.X) then
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    ip.X := ln2A.X;
    ip.Y := m1*ln2A.X + b1;
  end else
  begin
    m1 := (ln1B.Y - ln1A.Y)/(ln1B.X - ln1A.X);
    b1 := ln1A.Y - m1 * ln1A.X;
    m2 := (ln2B.Y - ln2A.Y)/(ln2B.X - ln2A.X);
    b2 := ln2A.Y - m2 * ln2A.X;
    if m1 = m2 then exit; //parallel lines
    ip.X := (b2 - b1)/(m1 - m2);
    ip.Y := m1 * ip.X + b1;
  end;
  result := true;
end;

//------------------------------------------------------------------------------
// Matrix functions
//------------------------------------------------------------------------------

function Matrix(const m00, m01, m02, m10, m11, m12, m20, m21, m22: double): TMatrixD;
begin
  Result[0,0] := m00; Result[0,1] := m01; Result[0,2] := m02;
  Result[1,0] := m10; Result[1,1] := m11; Result[1,2] := m12;
  Result[2,0] := m20; Result[2,1] := m21; Result[2,2] := m22;
end;
//------------------------------------------------------------------------------

function Det4(a1, a2, b1, b2: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := a1 * b2 - a2 * b1;
end;
//------------------------------------------------------------------------------

function Det9(a1, a2, a3, b1, b2, b3, c1, c2, c3: double): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := a1 * Det4(b2, b3, c2, c3) -
            b1 * Det4(a2, a3, c2, c3) +
            c1 * Det4(a2, a3, b2, b3);
end;
//------------------------------------------------------------------------------

function MatrixDeterminant(const matrix: TMatrixD): double;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := Det9(matrix[0,0], matrix[1,0], matrix[2,0],
                 matrix[0,1], matrix[1,1], matrix[2,1],
                 matrix[0,2], matrix[1,2], matrix[2,2]);
end;
//------------------------------------------------------------------------------

function MatrixAdjugate(const matrix: TMatrixD): TMatrixD;
begin
  //https://en.wikipedia.org/wiki/Adjugate_matrix
  Result[0,0] :=  Det4(matrix[1,1], matrix[1,2], matrix[2,1], matrix[2,2]);
  Result[0,1] := -Det4(matrix[0,1], matrix[0,2], matrix[2,1], matrix[2,2]);
  Result[0,2] :=  Det4(matrix[0,1], matrix[0,2], matrix[1,1], matrix[1,2]);

  Result[1,0] := -Det4(matrix[1,0], matrix[1,2], matrix[2,0], matrix[2,2]);
  Result[1,1] :=  Det4(matrix[0,0], matrix[0,2], matrix[2,0], matrix[2,2]);
  Result[1,2] := -Det4(matrix[0,0], matrix[0,2], matrix[1,0], matrix[1,2]);

  Result[2,0] :=  Det4(matrix[1,0], matrix[1,1], matrix[2,0], matrix[2,1]);
  Result[2,1] := -Det4(matrix[0,0], matrix[0,1], matrix[2,0], matrix[2,1]);
  Result[2,2] :=  Det4(matrix[0,0], matrix[0,1], matrix[1,0], matrix[1,1]);
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var pt: TPointD);
var
  tmpX: double;
begin
  tmpX := pt.x;
  pt.X := tmpX * matrix[0, 0] + pt.Y * matrix[1, 0] + matrix[2, 0];
  pt.Y := tmpX * matrix[0, 1] + pt.Y * matrix[1, 1] + matrix[2, 1];
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var path: TPathD);
var
  i, len: integer;
  tmpX: double;
  pp: PPointD;
begin
  len := Length(path);
  if len = 0 then Exit;
  pp := @path[0];
  for i := 0 to len -1 do
  begin
    tmpX := pp.X;
    pp.X := tmpX * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
    pp.Y := tmpX * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
    inc(pp);
  end;
end;
//------------------------------------------------------------------------------

procedure MatrixApply(const matrix: TMatrixD; var paths: TPathsD);
var
  i,j,len: integer;
  tmpX: double;
  pp: PPointD;
begin
  for i := 0 to High(paths) do
  begin
    len := Length(paths[i]);
    if len = 0 then Continue;
    pp := @paths[i][0];
    for j := 0 to High(paths[i]) do
    begin
      tmpX := pp.X;
      pp.X := tmpX * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
      pp.Y := tmpX * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
      inc(pp);
    end;
  end;
end;
//------------------------------------------------------------------------------

function MatrixMultiply(const modifier, matrix: TMatrixD): TMatrixD;
var
  i, j: Integer;
begin
//  if (modifier[2][2] <> 1) or (matrix[2][2] <> 1) then
//    raise Exception.Create(rsInvalidMatrix);
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] :=
        (modifier[0, j] * matrix[i, 0]) +
        (modifier[1, j] * matrix[i, 1]) +
        (modifier[2, j] * matrix[i, 2]);
end;
//------------------------------------------------------------------------------

procedure MatrixScale(var matrix: TMatrixD; scaleX, scaleY: double);
var
  m: TMatrixD;
begin
  m := IdentityMatrix;
  if (scaleX = 0) or (scaleY = 0) then
    raise Exception(rsInvalidScale);
  m[0, 0] := scaleX;
  m[1, 1] := scaleY;
  matrix := MatrixMultiply(m, matrix);
end;
//------------------------------------------------------------------------------

procedure MatrixScale(var matrix: TMatrixD; scale: double);
begin
  MatrixScale(matrix, scale, scale);
end;
//------------------------------------------------------------------------------

procedure MatrixRotate(var matrix: TMatrixD;
  const center: TPointD; angRad: double);
var
  m: TMatrixD;
  sn, cs: extended; //D7 compatible
  origOffset: Boolean;
begin
  NormalizeAngle(angRad);
  if angRad < 0.001 then Exit;

  m := IdentityMatrix;
  origOffset := (center.X <> 0) or (center.Y <> 0);
  if origOffset then MatrixTranslate(matrix, -center.X, -center.Y);
  SinCos(angRad, sn, cs);
  m := IdentityMatrix;
  m[0, 0] := cs;   m[1, 0] := sn;
  m[0, 1] := -sn;  m[1, 1] := cs;
  matrix := MatrixMultiply(m, matrix);
  if origOffset then MatrixTranslate(matrix, center.X, center.Y);
end;
//------------------------------------------------------------------------------

procedure MatrixTranslate(var matrix: TMatrixD; dx, dy: double);
var
  m: TMatrixD;
begin
  m := IdentityMatrix;
  m[2, 0] := dx;
  m[2, 1] := dy;
  matrix := MatrixMultiply(m, matrix);
end;
//------------------------------------------------------------------------------

procedure ScaleInternal(var matrix: TMatrixD; s: double);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      matrix[i,j] := matrix[i,j] * s;
end;
//------------------------------------------------------------------------------

procedure MatrixInvert(var matrix: TMatrixD);
var
  d: double;
const
  tolerance = 1.0E-5;
begin
  d := MatrixDeterminant(matrix);
  if abs(d) > tolerance then
  begin
    matrix := MatrixAdjugate(matrix);
    ScaleInternal(matrix, 1/d);
  end
  else matrix := IdentityMatrix;
end;
//------------------------------------------------------------------------------

procedure MatrixSkew(var matrix: TMatrixD; dx,dy: double);
var
  m: TMatrixD;
begin
  m := IdentityMatrix;
  m[1, 0] := dx;
  m[0, 1] := dy;
  matrix := MatrixMultiply(m, matrix);
end;
//------------------------------------------------------------------------------

function GrowOpenLine(const line: TPathD; width: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimit: double): TPathsD;
var
  len, x,y, wDiv2: integer;
  wd2: double;
  line1, line2: TPathD;
  vec1, vec2: TPointD;
begin
  Result := nil;
  len := length(line);
  if len = 0 then Exit;

  if width < MinStrokeWidth then
    width := MinStrokeWidth;

  if len = 1 then
  begin
    wDiv2 := Round(width/2);
    x := Round(line[0].X);
    y := Round(line[0].Y);
    SetLength(result, 1);
    result[0] := Ellipse(RectD(x -wDiv2, y -wDiv2, x +wDiv2, y +wDiv2));
    Exit;
  end;

  if joinStyle = jsAuto then
  begin
    if (endStyle = esRound) and (width >= AutoWidthThreshold) then
      joinStyle := jsRound else
      joinStyle := jsSquare;
  end;

  if joinStyle = jsRound then
  begin
    line2 := OpenPathToFlatPolygon(line);
  end else
  begin
    line1 := Copy(line, 0, len);
    if endStyle = esSquare then
    begin
      //extend both ends of the line by 1/2 lineWidth
      wd2 := width/2;
      vec1 := GetUnitVector(line1[1], line1[0]);
      vec2 := GetUnitVector(line1[len-2], line1[len-1]);
      with line1[0] do
        line1[0] := PointD(X + vec1.X * wd2, Y + vec1.y * wd2);
      with line1[len-1] do
        line1[len-1] := PointD(X + vec2.X * wd2, Y + vec2.y * wd2);
    end;
    line2 := OpenPathToFlatPolygon(line1);
  end;
  SetLength(result, 1);
  Result[0] := Grow(line2, nil, width/2, joinStyle, miterLimit);
end;
//------------------------------------------------------------------------------

function ReverseNormals(const norms: TPathD): TPathD;
var
  i, highI: integer;
begin
  highI := high(norms);
  setLength(result, highI +1);
  for i := 1 to highI  do
  begin
    result[i -1].X := -norms[highI -i].X;
    result[i -1].Y := -norms[highI -i].Y;
  end;
  result[highI].X := -norms[highI].X;
  result[highI].Y := -norms[highI].Y;
end;
//------------------------------------------------------------------------------

function GrowClosedLine(const line: TPathD; width: double;
  joinStyle: TJoinStyle; miterLimit: double): TPathsD;
var
  len: integer;
  line2, norms: TPathD;
begin
  len := length(line);
  if len < 3 then
  begin
    result := GrowOpenLine(line, width, joinStyle, esPolygon, miterLimit);
    Exit;
  end;

  if width < MinStrokeWidth then width := MinStrokeWidth;
  if joinStyle = jsAuto then
  begin
    if width < AutoWidthThreshold then
      joinStyle := jsSquare else
      joinStyle := jsRound;
  end;
  SetLength(Result, 2);
  norms := GetNormals(line);
  Result[0] := Grow(line, norms, width/2, joinStyle, miterLimit);
  line2 := ReversePath(line);
  norms := ReverseNormals(norms);
  Result[1] := Grow(line2, norms, width/2, joinStyle, miterLimit);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function Outline(const line: TPathD; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimit: double): TPathsD;
begin
  if not assigned(line) then
    Result := nil
  else if endStyle = esPolygon then
    result := GrowClosedLine(line, lineWidth, joinStyle, miterLimit)
  else
    result := GrowOpenLine(line, lineWidth, joinStyle, endStyle, miterLimit);
end;
//------------------------------------------------------------------------------

function Outline(const lines: TPathsD; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimit: double): TPathsD;
var
  i: integer;

  procedure AddPaths(const paths: TPathsD);
  var
    i, len, len2: integer;
  begin
    len := length(result);
    len2 := length(paths);
    setLength(result, len + len2);
    for i := 0 to len2 -1 do
      result[len+i] := paths[i];
  end;

begin
  result := nil;
  if not assigned(lines) then exit;
  if joinStyle = jsAuto then
  begin
    if endStyle in [esPolygon, esRound] then
      joinStyle := jsRound else
      joinStyle := jsSquare;
  end;
  if endStyle in [esPolygon, esClosed] then
    for i := 0 to high(lines) do
      AddPaths(GrowClosedLine(lines[i],
        lineWidth, joinStyle, miterLimit))
  else
    for i := 0 to high(lines) do
      AddPaths(GrowOpenLine(lines[i],
        lineWidth, joinStyle, endStyle, miterLimit));
end;
//------------------------------------------------------------------------------

function Rectangle(const rec: TRect): TPathD;
begin
  setLength(Result, 4);
  with rec do
  begin
    result[0] := PointD(left, top);
    result[1] := PointD(right, top);
    result[2] := PointD(right, bottom);
    result[3] := PointD(left, bottom);
  end;
end;
//------------------------------------------------------------------------------

function Rectangle(const rec: TRectD): TPathD;
begin
  setLength(Result, 4);
  with rec do
  begin
    result[0] := PointD(left, top);
    result[1] := PointD(right, top);
    result[2] := PointD(right, bottom);
    result[3] := PointD(left, bottom);
  end;
end;
//------------------------------------------------------------------------------

function Rectangle(l, t, r, b: double): TPathD;
begin
  setLength(Result, 4);
  result[0] := PointD(l, t);
  result[1] := PointD(r, t);
  result[2] := PointD(r, b);
  result[3] := PointD(l, b);
end;
//------------------------------------------------------------------------------

function InflateRect(const rec: TRect; dx, dy: integer): TRect;
begin
  result.Left := rec.Left - dx;
  result.Top := rec.Top - dy;
  result.Right := rec.Right + dx;
  result.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

function InflateRect(const rec: TRectD; dx, dy: double): TRectD;
begin
  result.Left := rec.Left - dx;
  result.Top := rec.Top - dy;
  result.Right := rec.Right + dx;
  result.Bottom := rec.Bottom + dy;
end;
//------------------------------------------------------------------------------

function RoundRect(const rec: TRect; radius: integer): TPathD;
var
  rec2: TRectD;
begin
  rec2 := RectD(rec);
  InflateRect(rec2, -radius, -radius);
  if IsEmptyRect(rec2) then
    result := nil else
    result := Grow(Rectangle(rec2), nil, radius, jsRound, 2);
end;
//------------------------------------------------------------------------------

function RoundRect(const rec: TRectD; radius: double): TPathD;
var
  rec2: TRectD;
begin
  rec2 := InflateRect(rec, -radius, -radius);
  if rec2.IsEmpty then
    result := Rectangle(rec) else
    result := Grow(Rectangle(rec2), nil, radius, jsRound, 2);
end;
//------------------------------------------------------------------------------

function Circle(const pt: TPoint; radius: double; steps: integer = 0): TPathD;
var
  rec: TRectD;
begin
  rec.Left := pt.X - radius;
  rec.Right := pt.X + radius;
  rec.Top := pt.Y - radius;
  rec.Bottom := pt.Y + radius;
  Result := Ellipse(rec);
end;
//------------------------------------------------------------------------------

function Circle(const pt: TPointD; radius: double; steps: integer = 0): TPathD;
var
  rec: TRectD;
begin
  rec.Left := pt.X - radius;
  rec.Right := pt.X + radius;
  rec.Top := pt.Y - radius;
  rec.Bottom := pt.Y + radius;
  Result := Ellipse(rec);
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRect; steps: integer): TPathD;
begin
  Result := Ellipse(RectD(rec), steps);
end;
//------------------------------------------------------------------------------

function Ellipse(const rec: TRectD; steps: integer): TPathD;
var
  i: Integer;
  f: double;
  sinA, cosA: extended;
  centre, radius, delta: TPointD;
begin
  result := nil;
  if rec.IsEmpty then Exit;
  with rec do
  begin
    centre := PointD((left+right)/2, (top+bottom)/2);
    radius := PointD(width/2, Height/2);
  end;
  f := (radius.x + radius.y)/2;
  if f < 0.5 then Exit;
  
  if steps < 3 then
    steps := Max(3, Trunc(Pi / (ArcCos(1 - 0.2 /f ))));
  SinCos(2 * Pi / Steps, sinA, cosA);
  delta.x := cosA; delta.y := sinA;

  SetLength(Result, Steps);
  Result[0] := PointD(centre.X + radius.X, centre.Y);
  for i := 1 to steps -1 do
  begin
    Result[i] := PointD(centre.X + radius.X * delta.x,
      centre.Y + radius.y * delta.y);
    delta :=  PointD(delta.X * cosA - delta.Y * sinA,
      delta.Y * cosA + delta.X * sinA);
  end;
end;
//------------------------------------------------------------------------------

function Star(const focalPt: TPointD;
  innerRadius, outerRadius: double; points: integer): TPathD;
var
  i: Integer;
  sinA, cosA: extended;
  delta: TPointD;
begin
  result := nil;
  if (innerRadius <= 0) or (outerRadius <= 0) then Exit;
  if points <= 5 then points := 10
  else points := points * 2;
  SinCos(2 * Pi / points, sinA, cosA);
  delta.x := cosA; delta.y := sinA;
  SetLength(Result, points);
  Result[0] := PointD(focalPt.X + innerRadius, focalPt.Y);
  for i := 1 to points -1 do
  begin
    if Odd(i) then
      Result[i] := PointD(focalPt.X + outerRadius * delta.x,
        focalPt.Y + outerRadius * delta.y)
    else
      Result[i] := PointD(focalPt.X + innerRadius * delta.x,
        focalPt.Y + innerRadius * delta.y);
    delta :=  PointD(delta.X * cosA - delta.Y * sinA,
      delta.Y * cosA + delta.X * sinA);
  end;
end;
//------------------------------------------------------------------------------

function Arc(const rec: TRect; startAngle, endAngle: double): TPathD;
var
  i, steps: Integer;
  angle, radiusAvg: double;
  sinA, cosA: extended;
  centre, radius: TPointD;
  deltaX, deltaX2, deltaY: extended;
begin
  Result := nil;
  if (endAngle = startAngle) or IsEmptyRect(rec) then Exit;
  centre := PointD((rec.left+rec.right)/2, (rec.top+rec.bottom)/2);
  radius := PointD(RectWidth(rec)/2, RectHeight(rec)/2);
  radiusAvg := (radius.X + radius.Y)/2;
  angle := endAngle - startAngle;
  if angle < 0 then angle := Pi * 2 + angle;
  //steps = (No. steps for a whole ellipse) * angle/(2*Pi)
  steps := Max(2, Trunc(Pi/ArcCos(1 - 0.2/radiusAvg) * angle /(2*Pi) ));

  SetLength(Result, Steps +1);
  //angle of the first step ...
  SinCos(-startAngle, deltaY, deltaX);
  Result[0].X := centre.X + radius.X * deltaX;
  Result[0].Y := centre.Y + radius.y * deltaY;
  //angle of each subsequent step ...
  SinCos(-angle / Steps, sinA, cosA);
  for i := 1 to steps do
  begin
    deltaX2 := deltaX * cosA - deltaY * sinA;
    deltaY := deltaY * cosA + deltaX * sinA;
    deltaX := deltaX2;
    Result[i] := PointD(centre.X + radius.X * deltaX,
      centre.Y + radius.y * deltaY);
  end;
end;
//------------------------------------------------------------------------------

function Pie(const rec: TRect; StartAngle, EndAngle: double): TPathD;
var
  len: integer;
begin
  result := Arc(rec, StartAngle, EndAngle);
  len := length(result);
  setLength(result, len +1);
  result[len] := PointD((rec.Left + rec.Right)/2, (rec.Top + rec.Bottom)/2);
end;
//------------------------------------------------------------------------------

function ArrowHead(const arrowTip, ctrlPt: TPointD; size: double;
  arrowStyle: TArrowStyle): TPathD;
var
  unitVec, basePt: TPointD;
  sDiv40, sDiv50, sDiv60, sDiv120: double;
begin
  result := nil;
  sDiv40 := size * 0.40;
  sDiv50 := size * 0.50;
  sDiv60 := size * 0.60;
  sDiv120 := sDiv60 * 2;
  unitVec := GetUnitVector(ctrlPt, arrowTip);

  case arrowStyle of
    asNone:
      Exit;
    asSimple:
      begin
        setLength(result, 3);
        basePt := OffsetPoint(arrowTip, -unitVec.X * size, -unitVec.Y * size);
        result[0] := arrowTip;
        result[1] := OffsetPoint(basePt, -unitVec.Y * sDiv50, unitVec.X * sDiv50);
        result[2] := OffsetPoint(basePt, unitVec.Y * sDiv50, -unitVec.X * sDiv50);
      end;
    asFancy:
      begin
        setLength(result, 4);
        basePt := OffsetPoint(arrowTip,
          -unitVec.X * sDiv120, -unitVec.Y * sDiv120);
        result[0] := OffsetPoint(basePt, -unitVec.Y *sDiv50, unitVec.X *sDiv50);
        result[1] := OffsetPoint(arrowTip, -unitVec.X *size, -unitVec.Y *size);
        result[2] := OffsetPoint(basePt, unitVec.Y *sDiv50, -unitVec.X *sDiv50);
        result[3] := arrowTip;
      end;
    asDiamond:
      begin
        setLength(result, 4);
        basePt := OffsetPoint(arrowTip, -unitVec.X * sDiv60, -unitVec.Y * sDiv60);
        result[0] := arrowTip;
        result[1] := OffsetPoint(basePt, -unitVec.Y * sDiv50, unitVec.X * sDiv50);
        result[2] := OffsetPoint(arrowTip, -unitVec.X * sDiv120, -unitVec.Y * sDiv120);
        result[3] := OffsetPoint(basePt, unitVec.Y * sDiv50, -unitVec.X * sDiv50);
      end;
    asCircle:
      begin
        basePt := OffsetPoint(arrowTip, -unitVec.X * sDiv50, -unitVec.Y * sDiv50);
        with Point(basePt) do
          result := Ellipse(RectD(x - sDiv50, y - sDiv50, x + sDiv50, y + sDiv50));
      end;
    asTail:
      begin
        setLength(result, 6);
        basePt := OffsetPoint(arrowTip, -unitVec.X * sDiv60, -unitVec.Y * sDiv60);
        result[0] := OffsetPoint(arrowTip, -unitVec.X * sDiv50, -unitVec.Y * sDiv50);
        result[1] := OffsetPoint(arrowTip, -unitVec.Y * sDiv40, unitVec.X * sDiv40);
        result[2] := OffsetPoint(basePt, -unitVec.Y * sDiv40, unitVec.X * sDiv40);
        result[3] := OffsetPoint(arrowTip, -unitVec.X * sDiv120, -unitVec.Y * sDiv120);
        result[4] := OffsetPoint(basePt, unitVec.Y * sDiv40, -unitVec.X * sDiv40);
        result[5] := OffsetPoint(arrowTip, unitVec.Y * sDiv40, -unitVec.X * sDiv40);
      end;

  end;
end;
//------------------------------------------------------------------------------

function GetDefaultArrowHeadSize(lineWidth: double): double;
begin
  Result := lineWidth *3 + 7;
end;
//------------------------------------------------------------------------------

function ShortenPath(const path: TPathD;
  pathEnd: TPathEnd; amount: double): TPathD;
var
  len, amount2: double;
  vec: TPointD;
  i, highPath: integer;
begin
  result := path;
  highPath := high(path);
  if highPath < 1 then Exit;
  amount2 := amount;

  if pathEnd <> peEnd then
  begin
    //shorten start
    i := 0;
    while (i < highPath) do
    begin
      len := Distance(result[i], result[i+1]);
      if (len >= amount) then Break;
      amount := amount - len;
      inc(i);
    end;
    if i > 0 then
    begin
      Move(path[i], Result[0], (highPath - i +1) * SizeOf(TPointD));
      dec(highPath, i);
      SetLength(Result, highPath +1);
    end;
    if amount > 0 then
    begin
      vec := GetUnitVector(result[0], result[1]);
      result[0].X := result[0].X + vec.X * amount;
      result[0].Y := result[0].Y + vec.Y * amount;
    end;
  end;

  if pathEnd <> peStart then
  begin
    //shorten end
    while (highPath > 1) do
    begin
      len := Distance(result[highPath], result[highPath -1]);
      if (len >= amount2) then Break;
      amount2 := amount2 - len;
      dec(highPath);
    end;
    SetLength(Result, highPath +1);
    if amount2 > 0 then
    begin
      vec := GetUnitVector(result[highPath], result[highPath -1]);
      result[highPath].X := result[highPath].X + vec.X * amount2;
      result[highPath].Y := result[highPath].Y + vec.Y * amount2;
    end;
  end;
end;
//------------------------------------------------------------------------------

function PerpendicularDistSqrd(const pt, line1, line2: TPointD): double;
var
  a,b,c,d: double;
begin
  a := pt.X - line1.X;
  b := pt.Y - line1.Y;
  c := line2.X - line1.X;
  d := line2.Y - line1.Y;
  if (c = 0) and (d = 0) then
    result := 0 else
    result := Sqr(a * d - c * b) / (c * c + d * d);
end;
//------------------------------------------------------------------------------

procedure RDP(const path: TPathD; startIdx, endIdx: integer;
  epsilonSqrd: double; const flags: TArrayOfInteger);
var
  i, idx: integer;
  d, maxD: double;
begin
  idx := 0;
  maxD := 0;
  for i := startIdx +1 to endIdx -1 do
  begin
    //PerpendicularDistSqrd - avoids expensive Sqrt()
    d := PerpendicularDistSqrd(path[i], path[startIdx], path[endIdx]);
    if d <= maxD then Continue;
    maxD := d;
    idx := i;
  end;
  if maxD < epsilonSqrd then Exit;
  flags[idx] := 1;
  if idx > startIdx + 1 then RDP(path, startIdx, idx, epsilonSqrd, flags);
  if endIdx > idx + 1 then RDP(path, idx, endIdx, epsilonSqrd, flags);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const path: TPathD;
  epsilon: double): TPathD;
var
  i,j, len: integer;
  buffer: TArrayOfInteger;
begin
  len := length(path);
  if len < 5 then
  begin
    result := Copy(path, 0, len);
    Exit;
  end;
  SetLength(buffer, len); //buffer is zero initialized

  buffer[0] := 1;
  buffer[len -1] := 1;
  RDP(path, 0, len -1, Sqr(epsilon), buffer);
  j := 0;
  SetLength(Result, len);
  for i := 0 to len -1 do
    if buffer[i] = 1 then
    begin
      Result[j] := path[i];
      inc(j);
    end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function RamerDouglasPeucker(const paths: TPathsD;
  epsilon: double): TPathsD;
var
  i,j, len: integer;
begin
  j := 0;
  len := length(paths);
  setLength(Result, len);
  for i := 0 to len -1 do
  begin
    Result[i] := RamerDouglasPeucker(paths[i], epsilon);
    if Result[i] <> nil then inc(j);
  end;
  setLength(Result, j);
end;
//------------------------------------------------------------------------------

function GetDashedPath(const path: TPathD;
  closed: Boolean; const pattern: TArrayOfInteger;
  patternOffset: PDouble): TPathsD;
var
  i, highI, paIdx: integer;
  vecs, path2, dash: TPathD;
  patCnt, patLen: integer;
  dashCapacity, dashCnt, ptsCapacity, ptsCnt: integer;
  segLen, residualPat, patOff: double;
  filling: Boolean;
  pt, pt2: TPointD;

  procedure NewDash;
  begin
    if ptsCnt = 1 then ptsCnt := 0;
    if ptsCnt = 0 then Exit;
    if dashCnt = dashCapacity then
    begin
      inc(dashCapacity, BuffSize);
      setLength(result, dashCapacity);
    end;
    result[dashCnt] := Copy(dash, 0, ptsCnt);
    inc(dashCnt);
    ptsCapacity := BuffSize;
    setLength(dash, ptsCapacity);
    ptsCnt := 0;
  end;

  procedure ExtendDash(const pt: TPointD);
  begin
    if ptsCnt = ptsCapacity then
    begin
      inc(ptsCapacity, BuffSize);
      setLength(dash, ptsCapacity);
    end;
    dash[ptsCnt] := pt;
    inc(ptsCnt);
  end;

begin
  Result := nil;
  paIdx := 0;
  patCnt := length(pattern);

  path2 := path;
  highI := high(path2);
  if (highI < 1) or (patCnt = 0) then Exit;

  if closed and
    ((path2[highI].X <> path2[0].X) or (path2[highI].Y <> path2[0].Y)) then
  begin
    inc(highI);
    setLength(path2, highI +2);
    path2[highI] := path2[0];
  end;

  vecs := GetVectors(path2);
  if (vecs[0].X = 0) and (vecs[0].Y = 0) then Exit; //not a line

  if not assigned(patternOffset) then
    patOff := 0 else
    patOff := patternOffset^;

  if patOff < 0 then
  begin
    patLen := 0;
    for i := 0 to patCnt -1 do inc(patLen, pattern[i]);
    patOff := patLen - patOff;
  end;
  paIdx := paIdx mod patCnt;

  //nb: each dash is made up of 2 or more pts
  dashCnt := 0;
  dashCapacity := 0;
  ptsCnt := 0;
  ptsCapacity := 0;

  filling := true;
  while patOff >= pattern[paIdx] do
  begin
    filling := not filling;
    patOff := patOff - pattern[paIdx];
    paIdx := (paIdx + 1) mod patCnt;
  end;
  residualPat := pattern[paIdx] - patOff;

  pt := path2[0];
  ExtendDash(pt);
  i := 0;
  while (i < highI) do
  begin
    segLen := Distance(pt, path2[i+1]);
    if residualPat > segLen then
    begin
      if filling then ExtendDash(path2[i+1]);
      residualPat := residualPat - segLen;
      pt := path2[i+1];
      inc(i);
    end else
    begin
      pt2.X := pt.X + vecs[i].X * residualPat;
      pt2.Y := pt.Y + vecs[i].Y * residualPat;
      if filling then ExtendDash(pt2);
      filling := not filling;
      NewDash;
      paIdx := (paIdx + 1) mod patCnt;
      residualPat := pattern[paIdx];
      pt := pt2;
      ExtendDash(pt);
    end;
  end;
  NewDash;
  SetLength(Result, dashCnt);

  if not assigned(patternOffset) then Exit;
  patOff := 0;
  for i := 0 to paIdx -1 do
    patOff := patOff + pattern[i];
  patternOffset^ := patOff + (pattern[paIdx] - residualPat);
end;
//------------------------------------------------------------------------------

function GetDashedOutLine(const path: TPathD;
  closed: Boolean; const pattern: TArrayOfInteger;
  patternOffset: PDouble; lineWidth: double;
  joinStyle: TJoinStyle; endStyle: TEndStyle): TPathsD;
var
  i: integer;
  tmp: TPathsD;
begin
  Result := nil;
  tmp := GetDashedPath(path, closed, pattern, patternOffset);
  for i := 0 to high(tmp) do
    AppendPath(Result, GrowOpenLine(tmp[i],
      lineWidth, joinStyle, endStyle, 2));
end;
//------------------------------------------------------------------------------

function GetBoundsD(const paths: TPathsD): TRectD;
var
  i,j: integer;
  l,t,r,b: double;
  p: PPointD;
begin
  l := MaxInt; t := MaxInt;
  r := -MaxInt; b := -MaxInt;
  for i := 0 to high(paths) do
  begin
    p := PPointD(paths[i]);
    if not assigned(p) then Continue;
    for j := 0 to high(paths[i]) do
    begin
      if p.x < l then l := p.x;
      if p.x > r then r := p.x;
      if p.y < t then t := p.y;
      if p.y > b then b := p.y;
      inc(p);
    end;
  end;
  if r < l then
    result := NullRectD else
    result := RectD(l, t, r, b);
end;
//------------------------------------------------------------------------------

function GetBoundsD(const path: TPathD): TRectD;
var
  i,highI: integer;
  l,t,r,b: double;
  p: PPointD;
begin
  highI := High(path);
  if highI < 0 then
  begin
    Result := NullRectD;
    Exit;
  end;
  l := path[0].X; r := l;
  t := path[0].Y; b := t;
  p := PPointD(path);
  for i := 1 to highI do
  begin
    inc(p);
    if p.x < l then l := p.x;
    if p.x > r then r := p.x;
    if p.y < t then t := p.y;
    if p.y > b then b := p.y;
  end;
  result := RectD(l, t, r, b);
end;
//------------------------------------------------------------------------------

function GetBounds(const path: TPathD): TRect;
var
  recD: TRectD;
begin
  recD := GetBoundsD(path);
  Result := Rect(recD);
end;
//------------------------------------------------------------------------------

function GetBounds(const paths: TPathsD): TRect;
var
  recD: TRectD;
begin
  recD := GetBoundsD(paths);
  Result := Rect(recD);
end;
//------------------------------------------------------------------------------

function FlattenQBezier(const pts: TPathsD): TPathsD;
var
  i, len: integer;
begin
  len := Length(pts);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FlattenQBezier(pts[i]);
end;
//------------------------------------------------------------------------------

function FlattenQBezier(const pts: TPathD): TPathD;
var
  i, highI: integer;
  p: TPathD;
begin
  Result := nil;
  highI := high(pts);
  if highI < 0 then Exit;
  if (highI < 2) or Odd(highI) then
    raise Exception.Create(rsInvalidQBezier);
  setLength(Result, 1);
  Result[0] := pts[0];
  for i := 0 to (highI div 2) -1 do
  begin
    if PointsEqual(pts[i*2], pts[i*2+1]) and
      PointsEqual(pts[i*2+1], pts[i*2+2]) then
    begin
      AppendPoint(Result, pts[i*2]);
      AppendPoint(Result, pts[i*2 +2]);
    end else
    begin
      p := FlattenQBezier(pts[i*2], pts[i*2+1], pts[i*2+2]);
      Result := JoinPaths(Result, Copy(p, 1, Length(p) -1));
    end;
  end;
end;
//------------------------------------------------------------------------------

function FlattenQBezier(const pt1, pt2, pt3: TPointD): TPathD;
var
  resultCnt, resultLen: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultCnt = resultLen then
    begin
      inc(resultLen, BuffSize);
      setLength(result, resultLen);
    end;
    result[resultCnt] := pt;
    inc(resultCnt);
  end;

  procedure DoCurve(const p1, p2, p3: TPointD);
  var
    p12, p23, p123: TPointD;
  begin
    if (abs(p1.x + p3.x - 2 * p2.x) +
      abs(p1.y + p3.y - 2 * p2.y) <
      QBezierTolerance) then //assessing curve 'flatness'
    begin
      AddPoint(p3);
    end else
    begin
      P12.X := (P1.X + P2.X) * 0.5;
      P12.Y := (P1.Y + P2.Y) * 0.5;
      P23.X := (P2.X + P3.X) * 0.5;
      P23.Y := (P2.Y + P3.Y) * 0.5;
      P123.X := (P12.X + P23.X) * 0.5;
      P123.Y := (P12.Y + P23.Y) * 0.5;
      DoCurve(p1, p12, p123);
      DoCurve(p123, p23, p3);
    end;
  end;

begin
  resultLen := 0; resultCnt := 0;
  AddPoint(pt1);
  if ((pt1.X = pt2.X) and (pt1.Y = pt2.Y)) or
    ((pt2.X = pt3.X) and (pt2.Y = pt3.Y)) then
  begin
    AddPoint(pt3)
  end else
    DoCurve(pt1, pt2, pt3);
  SetLength(result, resultCnt);
end;
//------------------------------------------------------------------------------

function FlattenCBezier(const pts: TPathsD): TPathsD;
var
  i, len: integer;
begin
  len := Length(pts);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := FlattenCBezier(pts[i]);
end;
//------------------------------------------------------------------------------

function FlattenCBezier(const pts: TPathD): TPathD;
var
  i, len: integer;
  p: TPathD;
begin
  Result := nil;
  len := Length(pts) -1;
  if len < 0 then Exit;
  if (len < 3) or (len mod 3 <> 0) then
    raise Exception.Create(rsInvalidCBezier);
  setLength(Result, 1);
  Result[0] := pts[0];
  for i := 0 to (len div 3) -1 do
  begin
    if PointsEqual(pts[i*3], pts[i*3+1]) and
      PointsEqual(pts[i*3+2], pts[i*3+3]) then
    begin
      AppendPoint(Result, pts[i*3]);
      AppendPoint(Result, pts[i*3 +3]);
    end else
    begin
      p := FlattenCBezier(pts[i*3], pts[i*3+1], pts[i*3+2], pts[i*3+3]);
      Result := JoinPaths(Result, Copy(p, 1, Length(p) -1));
    end;
  end;
end;
//------------------------------------------------------------------------------

function FlattenCBezier(const pt1, pt2, pt3, pt4: TPointD): TPathD;
var
  resultCnt, resultLen: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultCnt = resultLen then
    begin
      inc(resultLen, BuffSize);
      setLength(result, resultLen);
    end;
    result[resultCnt] := pt;
    inc(resultCnt);
  end;

  procedure DoCurve(const p1, p2, p3, p4: TPointD);
  var
    p12, p23, p34, p123, p234, p1234: TPointD;
  begin
    if (abs(p1.x + p3.x - 2*p2.x) + abs(p2.x + p4.x - 2*p3.x) +
      abs(p1.y + p3.y - 2*p2.y) + abs(p2.y + p4.y - 2*p3.y)) <
        CBezierTolerance then
    begin
      if resultCnt = length(result) then
        setLength(result, length(result) +BuffSize);
      result[resultCnt] := p4;
      inc(resultCnt);
    end else
    begin
      p12.X := (p1.X + p2.X) / 2;
      p12.Y := (p1.Y + p2.Y) / 2;
      p23.X := (p2.X + p3.X) / 2;
      p23.Y := (p2.Y + p3.Y) / 2;
      p34.X := (p3.X + p4.X) / 2;
      p34.Y := (p3.Y + p4.Y) / 2;
      p123.X := (p12.X + p23.X) / 2;
      p123.Y := (p12.Y + p23.Y) / 2;
      p234.X := (p23.X + p34.X) / 2;
      p234.Y := (p23.Y + p34.Y) / 2;
      p1234.X := (p123.X + p234.X) / 2;
      p1234.Y := (p123.Y + p234.Y) / 2;
      DoCurve(p1, p12, p123, p1234);
      DoCurve(p1234, p234, p34, p4);
    end;
  end;

begin
  result := nil;
  resultLen := 0; resultCnt := 0;
  AddPoint(pt1);
  if (pt1.X = pt2.X) and (pt1.Y = pt2.Y) and
    (pt3.X = pt4.X) and (pt3.Y = pt4.Y) then
  begin
    AddPoint(pt4)
  end else
    DoCurve(pt1, pt2, pt3, pt4);
  SetLength(result,resultCnt);
end;
//------------------------------------------------------------------------------

function ReflectPoint(const pt, pivot: TPointD): TPointD;
begin
  Result.X := pivot.X + (pivot.X - pt.X);
  Result.Y := pivot.Y + (pivot.Y - pt.Y);
end;
//------------------------------------------------------------------------------

function FlattenCSpline(const pts: TPathD): TPathD;
var
  resultCnt, resultLen: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultCnt = resultLen then
    begin
      inc(resultLen, BuffSize);
      setLength(result, resultLen);
    end;
    result[resultCnt] := pt;
    inc(resultCnt);
  end;

  procedure DoCurve(const p1, p2, p3, p4: TPointD);
  var
    p12, p23, p34, p123, p234, p1234: TPointD;
  begin
    if (abs(p1.x + p3.x - 2*p2.x) + abs(p2.x + p4.x - 2*p3.x) +
      abs(p1.y + p3.y - 2*p2.y) + abs(p2.y + p4.y - 2*p3.y)) <
        CBezierTolerance then
    begin
      if resultCnt = length(result) then
        setLength(result, length(result) +BuffSize);
      result[resultCnt] := p4;
      inc(resultCnt);
    end else
    begin
      p12.X := (p1.X + p2.X) / 2;
      p12.Y := (p1.Y + p2.Y) / 2;
      p23.X := (p2.X + p3.X) / 2;
      p23.Y := (p2.Y + p3.Y) / 2;
      p34.X := (p3.X + p4.X) / 2;
      p34.Y := (p3.Y + p4.Y) / 2;
      p123.X := (p12.X + p23.X) / 2;
      p123.Y := (p12.Y + p23.Y) / 2;
      p234.X := (p23.X + p34.X) / 2;
      p234.Y := (p23.Y + p34.Y) / 2;
      p1234.X := (p123.X + p234.X) / 2;
      p1234.Y := (p123.Y + p234.Y) / 2;
      DoCurve(p1, p12, p123, p1234);
      DoCurve(p1234, p234, p34, p4);
    end;
  end;

var
  i, len: integer;
  p: PPointD;
  pt1,pt2,pt3,pt4: TPointD;
begin
  result := nil;
  len := Length(pts); resultLen := 0; resultCnt := 0;
  if (len < 4) then Exit;
  //ignore incomplete trailing control points
  if Odd(len) then dec(len);
  p := @pts[0];
  AddPoint(p^);
  pt1 := p^; inc(p);
  pt2 := p^; inc(p);
  for i := 0 to (len shr 1) - 2 do
  begin
    pt3 := p^; inc(p);
    pt4 := p^; inc(p);
    DoCurve(pt1, pt2, pt3, pt4);
    pt1 := pt4;
    pt2 := ReflectPoint(pt3, pt1);
  end;
  SetLength(result,resultCnt);
end;
//------------------------------------------------------------------------------

function FlattenQSpline(const pts: TPathD): TPathD;
var
  resultCnt, resultLen: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if resultCnt = resultLen then
    begin
      inc(resultLen, BuffSize);
      setLength(result, resultLen);
    end;
    result[resultCnt] := pt;
    inc(resultCnt);
  end;

  procedure DoCurve(const p1, p2, p3: TPointD);
  var
    p12, p23, p123: TPointD;
  begin
    if (abs(p1.x + p3.x - 2 * p2.x) +
      abs(p1.y + p3.y - 2 * p2.y) <
      QBezierTolerance) then //assessing curve 'flatness'
    begin
      AddPoint(p3);
    end else
    begin
      P12.X := (P1.X + P2.X) * 0.5;
      P12.Y := (P1.Y + P2.Y) * 0.5;
      P23.X := (P2.X + P3.X) * 0.5;
      P23.Y := (P2.Y + P3.Y) * 0.5;
      P123.X := (P12.X + P23.X) * 0.5;
      P123.Y := (P12.Y + P23.Y) * 0.5;
      DoCurve(p1, p12, p123);
      DoCurve(p123, p23, p3);
    end;
  end;

var
  i, len: integer;
  p: PPointD;
  pt1, pt2, pt3: TPointD;
begin
  result := nil;
  len := Length(pts); resultLen := 0; resultCnt := 0;
  if (len < 3) then Exit;
  p := @pts[0];
  AddPoint(p^);
  pt1 := p^; inc(p);
  pt2 := p^; inc(p);
  for i := 0 to len - 3 do
  begin
    pt3 := p^; inc(p);
    DoCurve(pt1, pt2, pt3);
    pt1 := pt3;
    pt2 := ReflectPoint(pt2, pt1);
  end;
  SetLength(result,resultCnt);
end;
//------------------------------------------------------------------------------

function MakePathI(const pts: array of integer): TPathD; overload;
var
  i,j, x,y, len: Integer;
begin
  Result := nil;
  len := length(pts) div 2;
  if len < 1 then Exit;

  setlength(Result, len);
  Result[0].X := pts[0];
  Result[0].Y := pts[1];
  j := 0;
  for i := 1 to len -1 do
  begin
    x := pts[i*2];
    y := pts[i*2 +1];
    inc(j);
    Result[j].X := x;
    Result[j].Y := y;
  end;
  setlength(Result, j+1);
end;
//------------------------------------------------------------------------------

function MakePathD(const pts: array of double): TPathD;
var
  i, j, len: Integer;
  x,y: double;
begin
  Result := nil;
  len := length(pts) div 2;
  if len < 1 then Exit;

  setlength(Result, len);
  Result[0].X := pts[0];
  Result[0].Y := pts[1];
  j := 0;
  for i := 1 to len -1 do
  begin
    x := pts[i*2];
    y := pts[i*2 +1];
    inc(j);
    Result[j].X := x;
    Result[j].Y := y;
  end;
  setlength(Result, j+1);
end;

//------------------------------------------------------------------------------
// SmoothToBezier() support structures and functions
//------------------------------------------------------------------------------

type
  PPt = ^TPt;
  TPt = record
    pt   : TPointD;
    vec  : TPointD;
    len  : double;
    next : PPt;
    prev : PPt;
  end;

  TFitCurveContainer = class
  private
    ppts      : PPt;
    solution  : TPathD;
    tolSqrd   : double;
    function Count(first, last: PPt): integer;
    function AddPt(const pt: TPointD): PPt;
    procedure Clear;
    function ComputeLeftTangent(p: PPt): TPointD;
    function ComputeRightTangent(p: PPt): TPointD;
    function ComputeCenterTangent(p: PPt): TPointD;
    function ChordLengthParameterize(
      first, last: PPt; count: integer): TArrayOfDouble;
    function GenerateBezier(first, last: PPt; count: integer;
      const u: TArrayOfDouble; const firstTan, lastTan: TPointD): TPathD;
    function Reparameterize(first, last: PPt; count: integer;
      const u: TArrayOfDouble; const bezier: TPathD): TArrayOfDouble;
    function NewtonRaphsonRootFind(const q: TPathD;
      const pt: TPointD; u: double): double;
    function ComputeMaxErrorSqrd(first, last: PPt;
      const bezier: TPathD; const u: TArrayOfDouble;
      out SplitPoint: PPt): double;
    function FitCubic(first, last: PPt;
      firstTan, lastTan: TPointD): Boolean;
    procedure AppendSolution(const bezier: TPathD);
  public
    function FitCurve(const path: TPathD; closed: Boolean;
      tolerance: double; minSegLength: double): TPathD;
  end;

//------------------------------------------------------------------------------

function Scale(const vec: TPointD; newLen: double): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec.X * newLen;
  Result.Y := vec.Y * newLen;
end;
//------------------------------------------------------------------------------

function Mul(const vec: TPointD; val: double): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec.X * val;
  Result.Y := vec.Y * val;
end;
//------------------------------------------------------------------------------

function AddVecs(const vec1, vec2: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec1.X + vec2.X;
  Result.Y := vec1.Y + vec2.Y;
end;
//------------------------------------------------------------------------------

function SubVecs(const vec1, vec2: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := vec1.X - vec2.X;
  Result.Y := vec1.Y - vec2.Y;
end;
//------------------------------------------------------------------------------

function DotProdVecs(const vec1, vec2: TPointD): double;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := (vec1.X * vec2.X + vec1.Y * vec2.Y);
end;
//---------------------------------------------------------------------------

function NormalizeVec(const vec: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  len: double;
begin
  len := Sqrt(vec.X * vec.X + vec.Y * vec.Y);
  if len <> 0 then
  begin
    Result.X := vec.X / len;
    Result.Y := vec.Y / len;
  end else
    result := vec;
end;
//------------------------------------------------------------------------------

function NormalizePt2(const pt2: PPt): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  with pt2^ do
    if len <> 0 then
    begin
      Result.X := vec.X / len;
      Result.Y := vec.Y / len;
    end else
      result := vec;
end;
//------------------------------------------------------------------------------

function NegateVec(vec: TPointD): TPointD;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result.X := -vec.X;
  Result.Y := -vec.Y;
end;
//------------------------------------------------------------------------------

function B0(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
var
  tmp: double;
begin
  tmp := 1.0 - u;
  result := tmp * tmp * tmp;
end;
//------------------------------------------------------------------------------

function B1(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
var
  tmp: double;
begin
  tmp := 1.0 - u;
  result := 3 * u * tmp * tmp;
end;
//------------------------------------------------------------------------------

function B2(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := 3 * u * u * (1.0 - u);
end;
//------------------------------------------------------------------------------

function B3(u: double): double; {$IFDEF INLINE} inline; {$ENDIF}
begin
  result := u * u * u;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.AddPt(const pt: TPointD): PPt;
begin
  new(Result);
  Result.pt := pt;
  if not assigned(ppts) then
  begin
    Result.prev := Result;
    Result.next := Result;
    ppts := Result;
  end else
  begin
    Result.prev := ppts.prev;
    ppts.prev.next := Result;
    ppts.prev := Result;
    Result.next := ppts;
  end;
end;
//------------------------------------------------------------------------------

procedure TFitCurveContainer.Clear;
var
  p: PPt;
begin
  solution := nil;
  ppts.prev.next := nil; //break loop
  while assigned(ppts) do
  begin
    p := ppts;
    ppts := ppts.next;
    Dispose(p);
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.Count(first, last: PPt): integer;
begin
  if first = last then
    result := 0 else
    result := 1;
  repeat
    inc(Result);
    first := first.next;
  until (first = last);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeLeftTangent(p: PPt): TPointD;
begin
  Result := NormalizePt2(p);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeRightTangent(p: PPt): TPointD;
begin
  Result := NegateVec(NormalizePt2(p.prev));
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeCenterTangent(p: PPt): TPointD;
var
  v1, v2: TPointD;
begin
  v1 := SubVecs(p.pt, p.prev.pt);
  v2 := SubVecs(p.next.pt, p.pt);
  Result := AddVecs(v1, v2);
  Result := NormalizeVec(Result);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ChordLengthParameterize(
  first, last: PPt; count: integer): TArrayOfDouble;
var
  d: double;
  i: integer;
begin
  SetLength(Result, count);
  Result[0] := 0;
  d := 0;
  for i := 1 to count -1 do
  begin
    d := d + first.len;
    Result[i] := d;
    first := first.next;
  end;
  for i := 1 to count -1 do
    Result[i] := Result[i] / d;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.GenerateBezier(first, last: PPt; count: integer;
  const u: TArrayOfDouble; const firstTan, lastTan: TPointD): TPathD;
var
  i: integer;
  p: PPt;
  dist, epsilon: double;
  v1,v2, tmp: TPointD;
  a0, a1: TPathD;
  c: array [0..1, 0..1] of double;
  x: array [0..1] of double;
  det_c0_c1, det_c0_x, det_x_c1, alphaL, alphaR: double;
begin
  SetLength(a0, count);
  SetLength(a1, count);
  dist := Distance(first.pt, last.pt);

  for i := 0 to count -1 do
  begin
		v1 := Scale(firstTan, B1(u[i]));
		v2 := Scale(lastTan, B2(u[i]));
		a0[i] := v1;
		a1[i] := v2;
  end;

  FillChar(c[0][0], 4 * SizeOf(double), 0);
  FillChar(x[0], 2 * SizeOf(double), 0);

  p := first;
  for i := 0 to count -1 do
  begin
		c[0][0] := c[0][0] + DotProdVecs(a0[i], (a0[i]));
		c[0][1] := c[0][1] + DotProdVecs(a0[i], (a1[i]));
		c[1][0] := c[0][1];
		c[1][1] := c[1][1] + DotProdVecs(a1[i], (a1[i]));

    tmp := SubVecs(p.pt,
      AddVecs(Mul(first.pt, B0(u[i])),
      AddVecs(Mul(first.pt, B1(u[i])),
      AddVecs(Mul(last.pt, B2(u[i])),
      Mul(last.pt, B3(u[i]))))));

    x[0] := x[0] + DotProdVecs(a0[i], tmp);
    x[1] := x[1] + DotProdVecs(a1[i], tmp);
    p := p.next;
  end;

  det_c0_c1 := c[0][0] * c[1][1] - c[1][0] * c[0][1];
	det_c0_x := c[0][0] * x[1] - c[1][0] * x[0];
	det_x_c1 := x[0] * c[1][1] - x[1] * c[0][1];

  if det_c0_c1 = 0 then
    alphaL := 0 else
    alphaL := det_x_c1 / det_c0_c1;

  if det_c0_c1 = 0 then
    alphaR := 0 else
    alphaR := det_c0_x / det_c0_c1;

  //check for unlikely fit
  if (alphaL > dist * 2) then alphaL := 0
  else if (alphaR > dist * 2) then alphaR := 0;
  epsilon := 1.0e-6 * dist;

  SetLength(Result, 4);
  Result[0] := first.pt;
  Result[3] := last.pt;
  if (alphaL < epsilon) or (alphaR < epsilon) then
  begin
    dist := dist / 3;
    Result[1] := AddVecs(Result[0], Scale(firstTan, dist));
    Result[2] := AddVecs(Result[3], Scale(lastTan, dist));
  end else
  begin
    Result[1] := AddVecs(Result[0], Scale(firstTan, alphaL));
    Result[2] := AddVecs(Result[3], Scale(lastTan, alphaR));
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.Reparameterize(first, last: PPt; count: integer;
  const u: TArrayOfDouble; const bezier: TPathD): TArrayOfDouble;
var
  i: integer;
begin
  SetLength(Result, count);
  for i := 0 to count -1 do
  begin
		Result[i] := NewtonRaphsonRootFind(bezier, first.pt, u[i]);
    first := first.next;
  end;
end;
//------------------------------------------------------------------------------

function BezierII(degree: integer; const v: array of TPointD; t: double): TPointD;
var
  i,j: integer;
  tmp: array[0..3] of TPointD;
begin
  Move(v[0], tmp[0], (degree +1) * sizeOf(TPointD));
  for i := 1 to degree do
    for j := 0 to degree - i do
    begin
      tmp[j].x := (1.0 - t) * tmp[j].x + t * tmp[j+1].x;
      tmp[j].y := (1.0 - t) * tmp[j].y + t * tmp[j+1].y;
    end;
  Result := tmp[0];
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.ComputeMaxErrorSqrd(first, last: PPt;
	const bezier: TPathD; const u: TArrayOfDouble;
  out SplitPoint: PPt): double;
var
  i: integer;
  distSqrd: double;
  pt: TPointD;
  p: PPt;
begin
	Result := 0;
  i := 1;
  SplitPoint := first.next;
  p := first.next;
	while p <> last do
	begin
		pt := BezierII(3, bezier, u[i]);
		distSqrd := DistanceSqrd(pt, p.pt);
		if (distSqrd >= Result) then
    begin
      Result := distSqrd;
      SplitPoint := p;
    end;
    inc(i);
    p := p.next;
	end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.NewtonRaphsonRootFind(const q: TPathD;
  const pt: TPointD; u: double): double;
var
  numerator, denominator: double;
  qu, q1u, q2u: TPointD;
  q1: array[0..2] of TPointD;
  q2: array[0..1] of TPointD;
begin

  q1[0].x := (q[1].x - q[0].x) * 3.0;
  q1[0].y := (q[1].y - q[0].y) * 3.0;
  q1[1].x := (q[2].x - q[1].x) * 3.0;
  q1[1].y := (q[2].y - q[1].y) * 3.0;
  q1[2].x := (q[3].x - q[2].x) * 3.0;
  q1[2].y := (q[3].y - q[2].y) * 3.0;

  q2[0].x := (q1[1].x - q1[0].x) * 2.0;
  q2[0].y := (q1[1].y - q1[0].y) * 2.0;
  q2[1].x := (q1[2].x - q1[1].x) * 2.0;
  q2[1].y := (q1[2].y - q1[1].y) * 2.0;

  qu  := BezierII(3, q, u);
  q1u := BezierII(2, q1, u);
  q2u := BezierII(1, q2, u);

  numerator := (qu.x - pt.x) * (q1u.x) + (qu.y - pt.y) * (q1u.y);
  denominator := (q1u.x) * (q1u.x) + (q1u.y) * (q1u.y) +
    (qu.x - pt.x) * (q2u.x) + (qu.y - pt.y) * (q2u.y);

  if (denominator = 0) then
    Result := u else
    Result := u - (numerator / denominator);
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.FitCubic(first, last: PPt;
  firstTan, lastTan: TPointD): Boolean;
var
  i, cnt: integer;
  splitPoint: PPt;
  centerTan: TPointD;
  bezier: TPathD;
  clps, uPrime: TArrayOfDouble;
  maxErrorSqrd: double;
const
  maxRetries = 4;
begin
  Result := true;
  cnt := Count(first, last);
  if cnt = 2 then
  begin
    SetLength(bezier, 4);
    bezier[0] := first.pt;
    bezier[3] := last.pt;
    bezier[1] := bezier[0];
    bezier[2] := bezier[3];
    AppendSolution(bezier);
    Exit;
  end
  else if cnt = 3 then
  begin
    if TurnsLeft(first.prev.pt, first.pt, first.next.pt) =
      TurnsLeft(first.pt, first.next.pt, last.pt) then
        firstTan := ComputeCenterTangent(first);
    if TurnsLeft(last.prev.pt, last.pt, last.next.pt) =
      TurnsLeft(first.pt, first.next.pt, last.pt) then
        lastTan := NegateVec(ComputeCenterTangent(last));
  end;


  clps := ChordLengthParameterize(first, last, cnt);
  bezier := GenerateBezier(first, last, cnt, clps, firstTan, lastTan);
  maxErrorSqrd := ComputeMaxErrorSqrd(first, last, bezier, clps, splitPoint);
  if (maxErrorSqrd < tolSqrd) then
  begin
    AppendSolution(bezier);
    Exit;
  end;

  if (maxErrorSqrd < tolSqrd * 4) then //close enough to try again
  begin
		for i := 1 to maxRetries do
    begin
			uPrime := Reparameterize(first, last, cnt, clps, bezier);
      bezier := GenerateBezier(first, last, cnt, uPrime, firstTan, lastTan);
      maxErrorSqrd :=
        ComputeMaxErrorSqrd(first, last, bezier, uPrime, splitPoint);
			if (maxErrorSqrd < tolSqrd) then
      begin
        AppendSolution(bezier);
        Exit;
			end;
			clps := uPrime;
		end;
	end;

  //We need to break the curve because it's too complex for a single Bezier.
  //If we're changing direction then make this a 'hard' break (see below).
  if TurnsLeft(splitPoint.prev.prev.pt, splitPoint.prev.pt, splitPoint.pt) <>
    TurnsLeft(splitPoint.prev.pt, splitPoint.pt, splitPoint.next.pt) then
  begin
    centerTan := ComputeRightTangent(splitPoint);
    FitCubic(first, splitPoint, firstTan, centerTan);
    centerTan := ComputeLeftTangent(splitPoint);
    FitCubic(splitPoint, last, centerTan, lastTan);
  end else
  begin
    centerTan := ComputeCenterTangent(splitPoint);
    FitCubic(first, splitPoint, firstTan, NegateVec(centerTan));
    FitCubic(splitPoint, last, centerTan, lastTan);
  end;
end;
//------------------------------------------------------------------------------

function HardBreakCheck(ppt: PPt; compareLen: double): Boolean;
var
  q: double;
const
  longLen = 15;
begin
  //A 'break' means starting a new Bezier. A 'hard' break avoids smoothing
  //whereas a 'soft' break will still be smoothed. There is as much art as
  //science in determining where to smooth and where not to. For example,
  //long edges should generally remain straight but how long does an edge
  //have to be to be considered a 'long' edge?

  if (ppt.prev.len * 4 < ppt.len) or (ppt.len * 4 < ppt.prev.len) then
  begin
    //We'll hard break whenever there's significant asymmetry between
    //segment lengths because GenerateBezier() will perform poorly.
    result := true;
  end
  else if ((ppt.prev.len > longLen) and (ppt.len > longLen)) then
  begin
    //hard break long segments only when turning by more than ~45 degrees
    q := (Sqr(ppt.prev.len) + Sqr(ppt.len) - DistanceSqrd(ppt.prev.pt, ppt.next.pt)) /
      (2 * ppt.prev.len * ppt.len); //Cosine Rule.
    result := (1 - abs(q)) > 0.3;
  end
  else if ((TurnsLeft(ppt.prev.prev.pt, ppt.prev.pt, ppt.pt) =
    TurnsRight(ppt.prev.pt, ppt.pt, ppt.next.pt)) and
    (ppt.prev.len > compareLen) and (ppt.len > compareLen)) then
  begin
    //we'll also hard break whenever there's a significant inflection point
    result := true;
  end else
  begin
    //Finally, we'll also force a 'hard' break when there's a significant bend.
    //Again uses the Cosine Rule.
    q :=(Sqr(ppt.prev.len) + Sqr(ppt.len) -
      DistanceSqrd(ppt.prev.pt, ppt.next.pt)) / (2 * ppt.prev.len * ppt.len);
    Result := (q > -0.2); //ie more than 90%
  end;
end;
//------------------------------------------------------------------------------

function TFitCurveContainer.FitCurve(const path: TPathD;
  closed: Boolean; tolerance: double; minSegLength: double): TPathD;
var
  i, highI: integer;
  d: double;
  p, p2, pEnd: PPt;
begin
  //tolerance: specifies the maximum allowed variance between the existing
  //vertices and the new Bezier curves. More tolerance will produce
  //fewer Beziers and simpler paths, but at the cost of less precison.
  tolSqrd := Sqr(Max(1, Min(10, tolerance))); //range 1..10

  //minSegLength: Typically when vectorizing raster images, the produced
  //vector paths will have many series of axis aligned segments that trace
  //pixel boundaries. These paths will also contain many 1 unit segments at
  //right angles to adjacent segments. Importantly, these very short segments
  //will cause artifacts in the solution unless they are trimmed.
  highI     := High(path);
  if closed then
    while (highI > 0) and (Distance(path[highI], path[0]) < minSegLength) do
      dec(highI);

  p := AddPt(path[0]);
  for i := 1 to highI do
  begin
    d := Distance(p.pt, path[i]);
    //skip line segments with lengths less than 'minSegLength'
    if d < minSegLength then Continue;
    p := AddPt(path[i]);
    p.prev.len := d;
    p.prev.vec := SubVecs(p.pt, p.prev.pt);
  end;
  p.len := Distance(ppts.pt, p.pt);
  p.vec := SubVecs(p.next.pt, p.pt);
  p := ppts;

  if (p.next = p) or (closed and (p.next = p.prev)) then
  begin
    Clear;
    result := nil;
    Exit;
  end;

  //for closed paths, find a good starting point
  if closed then
  begin
    repeat
      if HardBreakCheck(p, tolerance) then break;
      p := p.next;
    until p = ppts;
    pEnd := p;
  end else
    pEnd := ppts.prev;

  p2 := p.next;
  repeat
    if HardBreakCheck(p2, tolerance) then
    begin
      FitCubic(p, p2, ComputeLeftTangent(p), ComputeRightTangent(p2));
      p := p2;
    end;
    p2 := p2.next;
  until (p2 = pEnd);
  FitCubic(p, p2, ComputeLeftTangent(p), ComputeRightTangent(p2));

  Result := solution;
  Clear;
end;
//------------------------------------------------------------------------------

procedure TFitCurveContainer.AppendSolution(const bezier: TPathD);
var
  i, len: integer;
begin
  len := Length(solution);
  if len > 0 then
  begin
    SetLength(solution, len + 3);
    for i := 0 to 2 do
      solution[len +i] := bezier[i +1];
  end else
    solution := bezier;
end;
//------------------------------------------------------------------------------

function SmoothToBezier(const path: TPathD; closed: Boolean;
  tolerance: double; minSegLength: double): TPathD;
var
  paths, solution: TPathsD;
begin
  SetLength(paths, 1);
  paths[0] := path;
  solution := SmoothToBezier(paths, closed, tolerance, minSegLength);
  if solution <> nil then
    Result := solution[0];
end;
//------------------------------------------------------------------------------

function SmoothToBezier(const paths: TPathsD; closed: Boolean;
  tolerance: double; minSegLength: double): TPathsD;
var
  i,j, len: integer;
begin
  j := 0;
  len := Length(paths);
  SetLength(Result, len);
  with TFitCurveContainer.Create do
  try
    for i := 0 to len -1 do
      if (paths[i] <> nil) and (Abs(Area(paths[i])) > Sqr(tolerance)) then
      begin
        Result[j] := FitCurve(paths[i], closed, tolerance, minSegLength);
        inc(j);
      end;
  finally
    Free;
  end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

end.
