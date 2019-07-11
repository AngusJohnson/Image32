unit ClipperOffset;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  9 March 2019                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2017                                         *
* Purpose   :  Offset paths and clipping solutions                             *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$IFDEF FPC}
  {$DEFINE INLINING}
{$ELSE}
  {$IF CompilerVersion < 14}
    Requires Delphi version 6 or above.
  {$IFEND}
  {$IF CompilerVersion >= 18}         //Delphi 2007
    //While Inlining has been supported since D2005, both D2005 and D2006
    //have an Inline codegen bug (QC41166) so ignore Inline until D2007.
    {$DEFINE INLINING}
    {$IF CompilerVersion >= 25.0}     //Delphi XE4+
      {$LEGACYIFEND ON}
    {$IFEND}
  {$IFEND}
{$ENDIF}

interface

uses
  SysUtils, Classes, Math, ClipperCore, Clipper;

type

  TJoinType = (jtSquare, jtRound, jtMiter);
  TEndType = (etPolygon, etOpenJoined, etOpenButt, etOpenSquare, etOpenRound);

  TClipperOffset = class
  private
    FDelta: Double;
    FStepSizeSin, FStepSizeCos: Extended;
    FMiterLim, FMiterLimit: Double;
    FStepsPerRad: Double;

    FNorms: TPathD;
    FPathsIn: TPaths;
    FPathIn: TPath;
    FPathOut: TPath;
    FPathOutLen: Integer;
    FSolution: TPaths;
    FSolutionLen: Integer;

    FJoinType: TJoinType;
    FEndType: TEndType;
    FArcTolerance: Double;

    procedure AddPoint(const pt: TPoint64);
    procedure DoSquare(j, k: Integer);
    procedure DoMiter(j, k: Integer; cosAplus1: Double);
    procedure DoRound(j, k: Integer);
    procedure OffsetPoint(j,k: Integer);

    procedure CheckPaths;
    function GetLowestPolygonIdx: integer;
    procedure BuildNormals;
    procedure ReverseNormals;
    procedure OffsetPaths;
    procedure OffsetPolygon;
    procedure OffsetOpenJoined;
    procedure OffsetOpenPath;
  public
    constructor Create(MiterLimit: double = 2.0; ArcTolerance: double = 0.0);
    destructor Destroy; override;
    procedure AddPath(const path: TPath);
    procedure AddPaths(const paths: TPaths);
    procedure Clear;
    procedure Execute(delta: Double;
      jt: TJoinType; et: TEndType; out solution: TPaths);
    property MiterLimit: Double read FMiterLimit write FMiterLimit;
    property ArcTolerance: Double read FArcTolerance write FArcTolerance;
  end;

  function ClipperOffsetPaths(const paths: TPaths;
    delta: Double; jt: TJoinType; et: TEndType): TPaths; overload;

  function ClipperOffsetPaths(const paths: TPathsD;
    delta: Double; jt: TJoinType; et: TEndType): TPathsD; overload;

implementation

//OVERFLOWCHECKS is OFF as a necessary workaround for a Delphi compiler bug that
//very occasionally reports overflow errors while still returning correct values
//eg var A, B: Int64; begin A := -$13456780; B := -$73456789; A := A * B; end;
//see https://forums.embarcadero.com/message.jspa?messageID=871444
//nb: this issue has now been resolved in Delphi 10.2
{$OVERFLOWCHECKS OFF}

resourcestring
  rsClipperOffset = 'ClipperOffset error';

const
  Tolerance           : Double = 1.0E-15;
  DefaultArcFrac      : Double = 0.02;
  Two_Pi              : Double = 2 * PI;
  LowestIp            : TPoint64 = (X: High(Int64); Y: High(Int64));

//------------------------------------------------------------------------------
//  Miscellaneous offset support functions
//------------------------------------------------------------------------------

function PointD(const X, Y: Double): TPointD; overload;
begin
  Result.X := X;
  Result.Y := Y;
end;
//------------------------------------------------------------------------------

function PointD(const pt: TPoint64): TPointD; overload;
begin
  Result.X := pt.X;
  Result.Y := pt.Y;
end;
//------------------------------------------------------------------------------

function DistanceSqr(const pt1, pt2: TPoint64): Int64;
begin
  Result := (pt1.X - pt2.X)*(pt1.X - pt2.X) + (pt1.Y - pt2.Y)*(pt1.Y - pt2.Y);
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TPoint64): TPointD;
var
  dx, dy, inverseHypot: Double;
begin
  if (pt2.X = pt1.X) and (pt2.Y = pt1.Y) then
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
// TClipperOffset methods
//------------------------------------------------------------------------------

constructor TClipperOffset.Create(MiterLimit: double; ArcTolerance: double);
begin
  inherited Create;
  FMiterLimit := MiterLimit;
  FArcTolerance := ArcTolerance;
end;
//------------------------------------------------------------------------------

destructor TClipperOffset.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Clear;
begin
  FPathsIn := nil;
  FNorms := nil;
  FSolution := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPath(const path: TPath);
begin
  if assigned(path) then
    AppendPath(FPathsIn, path);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPaths(const paths: TPaths);
var
  i: Integer;
begin
  for i := 0 to High(paths) do AddPath(paths[i]);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.CheckPaths;
var
  i,len, minLen: Integer;
  openPaths: Boolean;
begin
  openPaths := not (FEndType in [etPolygon, etOpenJoined]);
  if openPaths then minLen := 1 else minLen := 3;
  for i := 0 to high(FPathsIn) do
  begin
    StripDuplicates(FPathsIn[i]);
    len := length(FPathsIn[i]);
    if not openPaths and (len > 1) and
      PointsEqual(FPathsIn[i][0], FPathsIn[i][len-1]) then
    begin
      setlength(FPathsIn[i], len -1);
      dec(len);
    end;
    if len < minLen then FPathsIn[i] := nil;
  end;
end;
//------------------------------------------------------------------------------

function TClipperOffset.GetLowestPolygonIdx: integer;
var
  i,j, len: Integer;
  pt: TPoint64;
  p: TPath;
begin
  result := -1;
  pt := Point64(MaxInt64, MinInt64);
  for i := 0 to high(FPathsIn) do
  begin
    if FPathsIn[i] = nil then
      Continue;
    p := FPathsIn[i];
    len := length(p);
    for j := 0 to len -1 do
    begin
      if (p[j].Y < pt.Y) then
        continue;
      if (p[j].Y > pt.Y) or (p[j].X < pt.X) then
      begin
        pt := p[j];
        result := i;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPaths;
var
  i, len: Integer;
  arcTol, absDelta, steps: Double;
  tmpEndType: TEndType;
begin
  absDelta := Abs(FDelta);
  len := length(FPathsIn);

  //if a Zero offset, then simply copy paths to FSolution and return ...
  if absDelta < Tolerance then
  begin
    FSolutionLen := 0;
    SetLength(FSolution, len);
    for i := 0 to high(FPathsIn) do
      if assigned(FPathsIn[i]) then
      begin
        FSolution[FSolutionLen] := FPathsIn[i];
        inc(FSolutionLen);
      end;
    SetLength(FSolution, FSolutionLen);
    Exit;
  end;

  //FMiterLimit: see offset_triginometry3.svg
  if FMiterLimit > 1 then FMiterLim := 2/(sqr(FMiterLimit))
  else FMiterLim := 2;

  if (FArcTolerance <= DefaultArcFrac) then
    arcTol := DefaultArcFrac else
    arcTol := FArcTolerance;

  if (FJoinType = jtRound) or (FEndType = etOpenRound) then
  begin
    //get steps per 360 degrees (see offset_triginometry2.svg)
    steps := PI / ArcCos(1 - arcTol / absDelta);
    steps := Min(steps, absDelta * Pi); //avoids excessive precision
    FStepsPerRad := steps / Two_Pi;

    Math.SinCos(Two_Pi / steps, FStepSizeSin, FStepSizeCos);
    if FDelta < 0 then FStepSizeSin := -FStepSizeSin;
  end;

  if (FEndType = etOpenJoined) then
    SetLength(FSolution, len *2) else
    SetLength(FSolution, len);

  FSolutionLen := 0;
  for i := 0 to len -1 do
  begin
    FPathIn := FPathsIn[i];
    if FPathIn = nil then Continue;

    FPathOutLen := 0;
    FPathOut := nil;

    if Length(FPathIn) = 1 then
    begin
      //a simple workaround using OffsetOpenPath to construct
      //either a circle or a square point offset ...
      tmpEndType := FEndType;
      if FEndType = etOpenButt then FEndType := etOpenSquare;
      SetLength(FPathIn, 2);
      FPathIn[1] := FPathIn[0];
      SetLength(FNorms, 2);
      FNorms[0] := PointD(1,0);
      OffsetOpenPath;
      FEndType := tmpEndType;
    end else
    begin
      BuildNormals;
      if FEndType = etPolygon then
        OffsetPolygon
      else if FEndType = etOpenJoined then
        OffsetOpenJoined
      else
        OffsetOpenPath;
    end;

    if FPathOutLen = 0 then Continue;

    SetLength(FPathOut, FPathOutLen);
    FSolution[FSolutionLen] := FPathOut;
    Inc(FSolutionLen);
  end;
  SetLength(FSolution, FSolutionLen);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.BuildNormals;
var
  i, len: integer;
begin
  len := Length(FPathIn);
  SetLength(FNorms, len);
  for i := 0 to len-2 do
    FNorms[i] := GetUnitNormal(FPathIn[i], FPathIn[i+1]);
  FNorms[len -1] := GetUnitNormal(FPathIn[len -1], FPathIn[0]);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.ReverseNormals;
var
  i, highI: integer;
  tmp: TPointD;
begin
  FNorms := ReversePath(FNorms);
  highI := high(FNorms);
  tmp := FNorms[0];
  for i := 1 to highI  do
  begin
    FNorms[i-1].X := -FNorms[i].X;
    FNorms[i-1].Y := -FNorms[i].Y;
  end;
  FNorms[highI].X := -tmp.X;
  FNorms[highI].Y := -tmp.Y;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPolygon;
var
  i,j: integer;
begin
  j := high(FPathIn);
  for i := 0 to high(FPathIn) do
  begin
    OffsetPoint(i, j);
    j := i;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenJoined;
begin
  OffsetPolygon;
  FPathIn := ReversePath(FPathIn);

  SetLength(FPathOut, FPathOutLen);
  FSolution[FSolutionLen] := FPathOut;
  Inc(FSolutionLen);
  FPathOutLen := 0;
  FPathOut := nil;

  ReverseNormals;
  OffsetPolygon;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenPath;

  procedure DoButtCap(highI: integer);
  begin
    AddPoint(Point64(round(FPathIn[highI].X + FNorms[highI-1].X *FDelta),
      round(FPathIn[highI].Y + FNorms[highI-1].Y * FDelta)));
    AddPoint(Point64(round(FPathIn[highI].X - FNorms[highI-1].X *FDelta),
      round(FPathIn[highI].Y - FNorms[highI-1].Y * FDelta)));
  end;

  procedure DoSquareCap(highI: integer; toStart: Boolean);
  var
    pt: TPoint64;
  const
    sc: array[boolean] of integer = (1, -1);
  begin
    pt := Point64(round(FPathIn[highI].X + FNorms[highI-1].X *FDelta),
      round(FPathIn[highI].Y + FNorms[highI-1].Y * FDelta));
    AddPoint(pt);
    AddPoint(Point64(round(pt.X - FNorms[highI-1].Y *FDelta),
      round(pt.Y - FNorms[highI-1].X * FDelta * sc[true])));
    pt := Point64(round(FPathIn[highI].X - FNorms[highI-1].X *FDelta),
      round(FPathIn[highI].Y - FNorms[highI-1].Y * FDelta));
    AddPoint(Point64(round(pt.X - FNorms[highI-1].Y *FDelta),
      round(pt.Y - FNorms[highI-1].X * FDelta * sc[true])));
    AddPoint(pt);
  end;

  procedure DoRoundCap(highI: integer); //180 degrees
  var
    i: integer;
    steps: Integer;
    ptD: TPointD;
  begin
    steps := Round(FStepsPerRad * PI);
    ptD.X := FNorms[highI-1].X * FDelta;
    ptD.Y := FNorms[highI-1].Y * FDelta;
    for i := 1 to steps do
    begin
      AddPoint(Point64(round(
        FPathIn[highI].X + ptD.X),
        round(FPathIn[highI].Y + ptD.Y)));
      ptD := PointD(ptD.X * FStepSizeCos - FStepSizeSin * ptD.Y,
        ptD.X * FStepSizeSin + ptD.Y * FStepSizeCos);
    end;
  end;

var
  i,j, highI: integer;
begin
  highI := high(FPathIn);
  j := 0;
  for i := 1 to highI -1 do
  begin
    OffsetPoint(i, j);
    j := i;
  end;

  //cap the end first ...
  case FEndType of
    etOpenButt: DoButtCap(highI);
    etOpenRound: DoRoundCap(highI);
    else DoSquareCap(highI, false);
  end;

  FPathIn := ReversePath(FPathIn);
  ReverseNormals;
  j := 0;
  for i := 0 to highI -1 do
  begin
    OffsetPoint(i, j);
    j := i;
  end;

  //now cap the start ...
  case FEndType of
    etOpenButt: DoButtCap(highI);
    etOpenRound: DoRoundCap(highI);
    else DoSquareCap(highI, true);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Execute(delta: Double;
  jt: TJoinType; et: TEndType; out solution: TPaths);
var
  negate: Boolean;
  lowestIdx: integer;
begin
  solution := nil;
  if length(FPathsIn) = 0 then Exit;
  FJoinType := jt;
  FEndType := et;

  CheckPaths;
  negate := false;
  if (et = etPolygon) then
  begin
    //the lowermost polygon must be an outer polygon. So we can use that as the
    //designated orientation for outer polygons (needed for tidy-up clipping)
    lowestIdx := GetLowestPolygonIdx;
    negate := (Area(FPathsIn[lowestIdx]) < 0);
    //if polygon orientations are reversed, then 'negate' ...
    if negate then FDelta := FDelta;
  end;

  if FEndType <> etPolygon then
    FDelta := Abs(delta) /2 else
    FDelta := delta;
  OffsetPaths;

  //clean up self-intersections ...
  with TClipper.Create do
  try
    AddPaths(FSolution, ptSubject);
    if negate then
      Execute(ctUnion, frNegative, solution) else
      Execute(ctUnion, frPositive, solution);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPoint(const pt: TPoint64);
const
  BuffLength = 32;
begin
  if FPathOutLen = length(FPathOut) then
    SetLength(FPathOut, FPathOutLen + BuffLength);
  if (FPathOutLen > 0) and PointsEqual(FPathOut[FPathOutLen-1], pt) then Exit;
  FPathOut[FPathOutLen] := pt;
  Inc(FPathOutLen);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoSquare(j, k: Integer);
begin
  //Two vertices, one using the prior offset's (k) normal one the current (j).
  //Do a 'normal' offset (by delta) and then another by 'de-normaling' the
  //normal hence parallel to the direction of the respective edges.
  if FDelta > 0 then
  begin
    AddPoint(Point64(
      round(FPathIn[j].X + FDelta * (FNorms[k].X - FNorms[k].Y)),
      round(FPathIn[j].Y + FDelta * (FNorms[k].Y + FNorms[k].X))));
    AddPoint(Point64(
      round(FPathIn[j].X + FDelta * (FNorms[j].X + FNorms[j].Y)),
      round(FPathIn[j].Y + FDelta * (FNorms[j].Y - FNorms[j].X))));
  end else
  begin
    AddPoint(Point64(
      round(FPathIn[j].X + FDelta * (FNorms[k].X + FNorms[k].Y)),
      round(FPathIn[j].Y + FDelta * (FNorms[k].Y - FNorms[k].X))));
    AddPoint(Point64(
      round(FPathIn[j].X + FDelta * (FNorms[j].X - FNorms[j].Y)),
      round(FPathIn[j].Y + FDelta * (FNorms[j].Y + FNorms[j].X))));
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoMiter(j, k: Integer; cosAplus1: Double);
var
  q: Double;
begin
  //see offset_triginometry4.svg
  q := FDelta / cosAplus1;
  AddPoint(Point64(round(FPathIn[j].X + (FNorms[k].X + FNorms[j].X)*q),
    round(FPathIn[j].Y + (FNorms[k].Y + FNorms[j].Y)*q)));
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoRound(j, k: Integer);
var
  i, m,n, steps: Integer;
  a, delta, sinA, cosA: Double;
  pt, pt2, pt3: TPoint64;
  ptD: TPointD;
begin
  sinA := FNorms[k].X * FNorms[j].Y - FNorms[k].Y * FNorms[j].X;
  cosA := FNorms[j].X * FNorms[k].X + FNorms[j].Y * FNorms[k].Y;
  a := ArcTan2(sinA, cosA);
  steps := Round(FStepsPerRad * Abs(a));

  if (FDelta * sinA < 0) then //ie concave
  begin
    a := FDelta / (cosA +1);
    if (j = 0) then m := high(FPathIn) else m := j -1;
    if j = high(FPathIn) then n := 0 else n := j +1;

    //offset pt of concave vertex ...
    pt.X := round(FPathIn[j].X + (FNorms[k].X + FNorms[j].X)*a);
    pt.Y := round(FPathIn[j].Y + (FNorms[k].Y + FNorms[j].Y)*a);

    a := Min(DistanceSqr(FPathIn[m], FPathIn[j]),
      DistanceSqr(FPathIn[n], FPathIn[j]));

    //there's no space to draw anything ...
    if DistanceSqr(pt, FPathIn[j]) > a then
    begin
      //get the perpendicular offsets from pt2 ...
      //this creates a self-intersection that'll be clipped later
      pt2.X := round(FPathIn[j].X + FNorms[k].X * FDelta);
      pt2.Y := round(FPathIn[j].Y + FNorms[k].Y * FDelta);
      pt3.X := round(FPathIn[j].X + FNorms[j].X * FDelta);
      pt3.Y := round(FPathIn[j].Y + FNorms[j].Y * FDelta);
      AddPoint(pt2);
      AddPoint(pt3);
      Exit;
    end;

    a := Sqrt(a);
    //get the point on each edge being the distance of the shortest edge
    //from the concave vertex. (nb: unit normals to unit vectors here)
    pt2.X := round(FPathIn[j].X + FNorms[k].Y * a);
    pt2.Y := round(FPathIn[j].Y - FNorms[k].X * a);
    pt3.X := round(FPathIn[j].X - FNorms[j].Y * a);
    pt3.Y := round(FPathIn[j].Y + FNorms[j].X * a);

    //now FDelta offset these points ...
    pt2.X := round(pt2.X + FNorms[k].X * FDelta);
    pt2.Y := round(pt2.Y + FNorms[k].Y * FDelta);
    pt3.X := round(pt3.X + FNorms[j].X * FDelta);
    pt3.Y := round(pt3.Y + FNorms[j].Y * FDelta);

    if DistanceSqr(pt2, pt3) < Sqr(FDelta *2/MiterLimit) then
      delta := Sqrt(DistanceSqr(pt2, pt3))/2 else
      delta := FDelta/MiterLimit;

    a := (delta + FDelta) / (cosA +1);
    pt.X := round(FPathIn[j].X + (FNorms[k].X + FNorms[j].X)*a);
    pt.Y := round(FPathIn[j].Y + (FNorms[k].Y + FNorms[j].Y)*a);

    ptD.X := -FNorms[k].X * delta;
    ptD.Y := -FNorms[k].Y * delta;
    AddPoint(Point64(round(pt.X + ptD.X), round(pt.Y + ptD.Y)));
    for i := 1 to steps -1 do
    begin
      ptD := PointD(ptD.X * FStepSizeCos + FStepSizeSin * ptD.Y,
        -ptD.X * FStepSizeSin + ptD.Y * FStepSizeCos);
      AddPoint(Point64(round(pt.X + ptD.X), round(pt.Y + ptD.Y)));
    end;
  end else
  begin
    //a convex vertex ...
    pt := FPathIn[j];
    ptD.X := FNorms[k].X * FDelta;
    ptD.Y := FNorms[k].Y * FDelta;
    AddPoint(Point64(round(pt.X + ptD.X), round(pt.Y + ptD.Y)));
    for i := 1 to steps -1 do
    begin
      ptD := PointD(ptD.X * FStepSizeCos - FStepSizeSin * ptD.Y,
        ptD.X * FStepSizeSin + ptD.Y * FStepSizeCos);
      AddPoint(Point64(round(pt.X + ptD.X), round(pt.Y + ptD.Y)));
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPoint(j,k: Integer);
var
  sinA, cosA: Double;
begin
  //A: angle between adjoining edges (on left side WRT winding direction).
  //A == 0 deg (or A == 360 deg): collinear edges heading in same direction
  //A == 180 deg: collinear edges heading in opposite directions (ie a 'spike')
  //sin(A) < 0: convex on left.
  //cos(A) > 0: angles on both left and right sides > 90 degrees
  sinA := (FNorms[k].X * FNorms[j].Y - FNorms[j].X * FNorms[k].Y);
  cosA := (FNorms[j].X * FNorms[k].X + FNorms[j].Y * FNorms[k].Y);

  if (Abs(sinA * FDelta) < 1.0) then //angle is close to 0 or 180 deg.
  begin
    if (cosA > 0) then //given condition above the angle is approaching 0 deg.
    begin
      //with angles approaching 0 deg collinear (whether concave or convex),
      //offsetting with two or more vertices (that would be so close together)
      //occasionally causes tiny self-intersections due to rounding.
      //So we offset with just a single vertex here ...
      AddPoint(Point64(round(FPathIn[j].X + FNorms[k].X * FDelta),
        round(FPathIn[j].Y + FNorms[k].Y * FDelta)));
      Exit;
    end;
    //else angle must be approaching 180 deg.
  end
  else if (sinA > 1.0) then sinA := 1.0
  else if (sinA < -1.0) then sinA := -1.0;

  if sinA * FDelta < 0 then //ie a concave offset
  begin
    AddPoint(Point64(round(FPathIn[j].X + FNorms[k].X * FDelta),
      round(FPathIn[j].Y + FNorms[k].Y * FDelta)));
    AddPoint(FPathIn[j]); //this improves clipping removal later
    AddPoint(Point64(round(FPathIn[j].X + FNorms[j].X * FDelta),
      round(FPathIn[j].Y + FNorms[j].Y * FDelta)));
  end
  else
  begin
    //convex offsets here ...
    case FJoinType of
      jtMiter:
        //see offset_triginometry3.svg
        if (1 + cosA < FMiterLim) then DoSquare(j, k)
        else DoMiter(j, k, 1 + cosA);
      jtSquare:
        //angles >= 90 deg. don't need squaring
        if cosA >= 0 then
          DoMiter(j, k, 1 + cosA) else
          DoSquare(j, k);

      else DoRound(j, k);
    end;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function ClipperOffsetPaths(const paths: TPaths;
  delta: Double; jt: TJoinType; et: TEndType): TPaths;
begin
  with TClipperOffset.Create(1) do
  try
    AddPaths(paths);
    Execute(delta, jt, et, Result);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function ClipperOffsetPaths(const paths: TPathsD;
  delta: Double; jt: TJoinType; et: TEndType): TPathsD;
var
  paths64, sol64: TPaths;
begin
  paths64 := ScalePaths(paths, 1000, 1000);
  with TClipperOffset.Create do
  try
    AddPaths(paths64);
    Execute(delta*1000, jt, et, sol64);
  finally
    free;
  end;
  Result := ScalePathsD(sol64, 0.001, 0.001);
end;
//------------------------------------------------------------------------------

end.
