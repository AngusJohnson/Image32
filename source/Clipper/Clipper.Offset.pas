unit Clipper.Offset;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta) - aka Clipper2                                      *
* Date      :  11 April 2022                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2022                                         *
* Purpose   :  Offset paths and clipping solutions                             *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$I Clipper.inc}

interface

uses
  SysUtils, Classes, Clipper.Core;

type

  TJoinType = (jtSquare, jtRound, jtMiter);
  TEndType = (etPolygon, etJoined, etButt, etSquare, etRound);
  //etButt   : offsets both sides of a path, with square blunt ends
  //etSquare : offsets both sides of a path, with square extended ends
  //etRound  : offsets both sides of a path, with round extended ends
  //etJoined : offsets both sides of a path, with joined ends
  //etPolygon: offsets only one side of a closed path

  TPathGroup = class
	  paths     : TPathsD;
    reversed  : Boolean;
	  joinType  : TJoinType;
	  endType   : TEndType;
    constructor Create(jt: TJoinType; et: TEndType);
  end;

  TClipperOffset = class
  private
    fDelta       : Double;
    fPrecision   : integer;
    fMinLenSqrd  : double;
    fJoinType    : TJoinType;
    fTmpLimit    : Double;
    fMiterLimit  : Double;
    fArcTolerance: Double;
    fStepsPerRad : Double;
    fNorms       : TPathD;
    fInGroups    : TList;
    fMergeGroups : Boolean;
    fInPath      : TPathD;
    fOutPath     : TPathD;
    fOutPaths    : TPathsD;
    fOutPathLen  : Integer;
    fSolution    : TPathsD;

    procedure AddPoint(x,y: double); overload;
    procedure AddPoint(const pt: TPointD); overload;
    procedure DoSquare(j, k: Integer);
    procedure DoMiter(j, k: Integer; cosAplus1: Double);
    procedure DoRound(j, k: integer; angle: double);
    procedure OffsetPoint(j: Integer; var k: integer);

    procedure BuildNormals;
    procedure DoGroupOffset(pathGroup: TPathGroup; delta: double);
    procedure OffsetPolygon;
    procedure OffsetOpenJoined;
    procedure OffsetOpenPath(endType: TEndType);
  public
    constructor Create(miterLimit: double = 2.0;
      arcTolerance: double = 0.0; roundingDecimalPrecision: integer = 2);
    destructor Destroy; override;
    procedure AddPath(const path: TPathD;
      joinType: TJoinType; endType: TEndType);
    procedure AddPaths(const paths: TPathsD;
      joinType: TJoinType; endType: TEndType);
    procedure Clear;
    function Execute(delta: Double): TPathsD;

    //MiterLimit: needed for mitered offsets (see offset_triginometry3.svg)
    property MiterLimit: Double read fMiterLimit write fMiterLimit;
    //ArcTolerance: needed for rounded offsets (See offset_triginometry2.svg)
    property ArcTolerance: Double read fArcTolerance write fArcTolerance;
    //MergeGroups: A path group is one or more paths added via the AddPath or
    //AddPaths methods. By default these path groups will be offset
    //independently of other groups and this may cause overlaps (intersections).
    //However, when MergeGroups is enabled, any overlapping offsets will be
    //merged (via a clipping union operation) to remove overlaps.
    property MergeGroups: Boolean read fMergeGroups write fMergeGroups;
    property RoundingDecimalPrecision: integer read fPrecision write fPrecision;
  end;

implementation

uses
  Math, Clipper.Engine;

const
  Tolerance           : Double = 1.0E-15;
  Two_Pi              : Double = 2 * PI;

//------------------------------------------------------------------------------
//  Miscellaneous offset support functions
//------------------------------------------------------------------------------

function DotProduct(const vec1, vec2: TPointD): double;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := (vec1.X * vec2.X + vec1.Y * vec2.Y);
end;
//------------------------------------------------------------------------------

function GetUnitNormal(const pt1, pt2: TPointD): TPointD;
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

function GetLowestPolygonIdx(const paths: TPathsD): integer;
var
  i,j: integer;
  lp: TPointD;
  p: TPathD;
begin
	Result := -1;
  lp := PointD(0, -MaxDouble);
	for i := 0 to High(paths) do
	begin
		p := paths[i];
		for j := 0 to High(p) do
			if (p[j].Y < lp.Y) then continue
      else if ((p[j].Y > lp.Y) or (p[j].X < lp.X)) then
      begin
				Result := i;
				lp := p[j];
			end;
  end;
end;
//------------------------------------------------------------------------------

function CopyPaths(const paths: TPathsD): TPathsD;
var
  i, len: integer;
begin
  len := Length(paths);
  SetLength(Result, len);
  for i := 0 to len -1 do
    Result[i] := Copy(paths[i], 0, Length(paths[i]));
end;

//------------------------------------------------------------------------------
// TPathGroup methods
//------------------------------------------------------------------------------

constructor TPathGroup.Create(jt: TJoinType; et: TEndType);
begin
  Self.joinType := jt;
  Self.endType := et;
end;

//------------------------------------------------------------------------------
// TClipperOffset methods
//------------------------------------------------------------------------------

constructor TClipperOffset.Create(miterLimit: double;
  arcTolerance: double; roundingDecimalPrecision: integer);
begin
  fMiterLimit   := MiterLimit;
  fArcTolerance := ArcTolerance;
  fInGroups     := TList.Create;
  fPrecision    := roundingDecimalPrecision;
end;
//------------------------------------------------------------------------------

destructor TClipperOffset.Destroy;
begin
  Clear;
  fInGroups.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.Clear;
var
  i: integer;
begin
  for i := 0 to fInGroups.Count -1 do
    TPathGroup(fInGroups[i]).Free;
  fInGroups.Clear;
  fSolution := nil;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPath(const path: TPathD;
  joinType: TJoinType; endType: TEndType);
var
  paths: TPathsD;
begin
  if not assigned(path) then Exit;
  SetLength(paths, 1);
  paths[0] := path;
  AddPaths(Paths, joinType, endType);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPaths(const paths: TPathsD;
  joinType: TJoinType; endType: TEndType);
var
  group: TPathGroup;
begin
  if Length(paths) = 0 then Exit;
  group := TPathGroup.Create(joinType, endType);
  AppendPaths(group.paths, paths);
  fInGroups.Add(group);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoGroupOffset(pathGroup: TPathGroup; delta: double);
var
  i, len, lowestIdx: Integer;
  absDelta, arcTol, steps: Double;
  IsClosedPaths: Boolean;
begin
  if pathgroup.endType <> etPolygon then delta := Abs(delta) / 2;

  IsClosedPaths := (pathgroup.endType in [etPolygon, etJoined]);
  if IsClosedPaths then
  begin
    //the lowermost polygon must be an outer polygon. So we can use that as the
    //designated orientation for outer polygons (needed for tidy-up clipping)
    lowestIdx := GetLowestPolygonIdx(pathgroup.paths);
    if lowestIdx < 0 then Exit;
    if Area(pathgroup.paths[lowestIdx]) < 0 then
    begin
      //more efficient than literally reversing paths
      pathgroup.reversed := true;
      delta := -delta;
    end;
  end;

{$IFDEF REVERSE_ORIENTATION}
  fDelta := delta;
{$ELSE}
  fDelta := -delta;
{$ENDIF}

  absDelta := Abs(fDelta);
  fJoinType := pathGroup.joinType;

  if fArcTolerance > 0 then
    arcTol := fArcTolerance else
    arcTol := Log10(2 + absDelta) * 0.25; //empirically derived

  //calculate a sensible number of steps (for 360 deg for the given offset
  if (pathgroup.joinType = jtRound) or (pathgroup.endType = etRound) then
  begin
    //get steps per 180 degrees (see offset_triginometry2.svg)
    steps := PI / ArcCos(1 - arcTol / absDelta);
    fStepsPerRad := steps / Two_Pi;
  end;

  fOutPaths := nil;
  for i := 0 to High(pathgroup.paths) do
  begin
    fInPath := StripNearDuplicates(pathgroup.paths[i], fMinLenSqrd, IsClosedPaths);
    len := Length(fInPath);
    if (fInPath = nil) or
      ((pathGroup.endType in [etPolygon, etJoined]) and (len < 3)) then Continue;

    fNorms := nil;
    fOutPath := nil;
    fOutPathLen := 0;

		//if a single vertex then build a circle or a square ...
    if len = 1 then
    begin
      if (pathgroup.endType = etRound) then
      begin
        SetLength(fNorms, 2);
        fNorms[0] := PointD(1,0);
        fNorms[1] := PointD(-1,0);
        DoRound(0, 1, Two_Pi);
        dec(fOutPathLen);
        SetLength(fOutPath, fOutPathLen);
      end else
      begin
        fOutPathLen := 4;
        SetLength(fOutPath, fOutPathLen);
        with fInPath[0] do
        begin
          fOutPath[0] := PointD(X-fDelta,Y-fDelta);
          fOutPath[1] := PointD(X+fDelta,Y-fDelta);
          fOutPath[2] := PointD(X+fDelta,Y+fDelta);
          fOutPath[3] := PointD(X-fDelta,Y+fDelta);
        end;
      end;
    end else
    begin
      BuildNormals;
      if pathgroup.endType = etPolygon then
      begin
        OffsetPolygon;
      end
      else if pathgroup.endType = etJoined then
      begin
        OffsetOpenJoined;
      end else
        OffsetOpenPath(pathgroup.endType);
    end;

    if fOutPathLen = 0 then Continue;
    SetLength(fOutPath, fOutPathLen);
    AppendPath(fOutPaths, fOutPath);
  end;

  if not fMergeGroups then
  begin
    //clean up self-intersections ...
    with TClipperD.Create(fPrecision) do
    try
      AddSubject(fOutPaths);
      if pathgroup.reversed then
        Execute(ctUnion, frNegative, fOutPaths) else
        Execute(ctUnion, frPositive, fOutPaths);
    finally
      free;
    end;
  end;

  //finally copy the working 'outPaths' to the solution
  AppendPaths(fSolution, fOutPaths);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.BuildNormals;
var
  i, len: integer;
begin
  len := Length(fInPath);
  SetLength(fNorms, len);
  for i := 0 to len-2 do
    fNorms[i] := GetUnitNormal(fInPath[i], fInPath[i+1]);
  fNorms[len -1] := GetUnitNormal(fInPath[len -1], fInPath[0]);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPolygon;
var
  i,j: integer;
begin
  j := high(fInPath);
  for i := 0 to high(fInPath) do
    OffsetPoint(i, j);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenJoined;
begin
  OffsetPolygon;
  SetLength(fOutPath, fOutPathLen);
  AppendPath(fOutPaths, fOutPath);
  fOutPath := nil;
  fOutPathLen := 0;
  fInPath := ReversePath(fInPath);
  BuildNormals;
  OffsetPolygon;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetOpenPath(endType: TEndType);

  procedure DoButtEnd(highI: integer);
  begin
    AddPoint(fInPath[highI].X + fNorms[highI-1].X *fDelta,
      fInPath[highI].Y + fNorms[highI-1].Y * fDelta);
    AddPoint(fInPath[highI].X - fNorms[highI-1].X *fDelta,
      fInPath[highI].Y - fNorms[highI-1].Y * fDelta);
  end;

  procedure DoButtStart;
  begin
    AddPoint(fInPath[0].X + fNorms[1].X *fDelta,
      fInPath[0].Y + fNorms[1].Y * fDelta);
    AddPoint(fInPath[0].X - fNorms[1].X *fDelta,
      fInPath[0].Y - fNorms[1].Y * fDelta);
  end;

var
  i, k, highI: integer;
begin
  highI := high(fInPath);
  k := 0;
  for i := 1 to highI -1 do
    OffsetPoint(i, k);

  k := highI -1;
  fNorms[highI].X := -fNorms[k].X;
  fNorms[highI].Y := -fNorms[k].Y;

 //cap the end first ...
  case endType of
    etButt: DoButtEnd(highI);
{$IFDEF REVERSE_ORIENTATION}
    etRound: DoRound(highI, k, PI);
{$ELSE}
    etRound: DoRound(highI, k, -PI);
{$ENDIF}
    else DoSquare(highI, k);
  end;

  //reverse normals ...
  for i := highI -1 downto 1 do
  begin
    fNorms[i].X := -fNorms[i-1].X;
    fNorms[i].Y := -fNorms[i-1].Y;
  end;
  fNorms[0].X := -fNorms[1].X;
  fNorms[0].Y := -fNorms[1].Y;
  k := highI;
  for i := highI -1 downto 1 do
    OffsetPoint(i, k);

  //now cap the start ...
  case endType of
    etButt: DoButtStart;
{$IFDEF REVERSE_ORIENTATION}
    etRound: DoRound(0, 1, PI);
{$ELSE}
    etRound: DoRound(0, 1, -PI);
{$ENDIF}
    else doSquare(0, 1);
  end;
end;
//------------------------------------------------------------------------------

function TClipperOffset.Execute(delta: Double): TPathsD;
var
  i: integer;
  scale: double;
  group: TPathGroup;
begin
  fSolution := nil;
  Result := nil;
  if fInGroups.Count = 0 then Exit;

  scale := Math.Power(10, fPrecision);
  fMinLenSqrd := 1/Sqr(scale);

  if abs(delta) < Tolerance then
  begin
    //if delta ~= 0, just copy paths to Result
    for i := 0 to fInGroups.Count -1 do
      with TPathGroup(fInGroups[i]) do
          AppendPaths(fSolution, paths);
    Result := fSolution;
    Exit;
  end;

  //Miter Limit: see offset_triginometry3.svg
  if fMiterLimit > 1 then
    fTmpLimit := 2 / Sqr(fMiterLimit) else
    fTmpLimit := 2.0;

  //nb: delta will depend on whether paths are polygons or open
  for i := 0 to fInGroups.Count -1 do
  begin
    group := TPathGroup(fInGroups[i]);
    DoGroupOffset(group, delta);
  end;

  if fMergeGroups and (fInGroups.Count > 0) then
  begin
    //clean up self-intersections ...
    with TClipperD.Create(fPrecision) do
    try
      AddSubject(fSolution);
      if TPathGroup(fInGroups[0]).reversed then
        Execute(ctUnion, frNegative, fSolution) else
        Execute(ctUnion, frPositive, fSolution);
    finally
      free;
    end;
  end;
  Result := fSolution;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPoint(x,y: double);
const
  BuffLength = 32;
var
  pt: TPointD;
begin
  pt := PointD(x,y);
  if fOutPathLen = length(fOutPath) then
    SetLength(fOutPath, fOutPathLen + BuffLength);
  if (fOutPathLen > 0) and
    PointsNearEqual(fOutPath[fOutPathLen-1], pt, fMinLenSqrd) then Exit;
  fOutPath[fOutPathLen] := pt;
  Inc(fOutPathLen);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.AddPoint(const pt: TPointD);
const
  BuffLength = 32;
begin
  if fOutPathLen = length(fOutPath) then
    SetLength(fOutPath, fOutPathLen + BuffLength);
  if (fOutPathLen > 0) and
    PointsNearEqual(fOutPath[fOutPathLen-1], pt, fMinLenSqrd) then Exit;
  fOutPath[fOutPathLen] := pt;
  Inc(fOutPathLen);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoSquare(j, k: Integer);
begin
  //Two vertices, one using the prior offset's (k) normal one the current (j).
  //Do a 'normal' offset (by delta) and then another by 'de-normaling' the
  //normal hence parallel to the direction of the respective edges.
  if (fDelta > 0) then
  begin
    AddPoint(
      fInPath[j].X + fDelta * (fNorms[k].X - fNorms[k].Y),
      fInPath[j].Y + fDelta * (fNorms[k].Y + fNorms[k].X));

    AddPoint(
      fInPath[j].X + fDelta * (fNorms[j].X + fNorms[j].Y),
      fInPath[j].Y + fDelta * (fNorms[j].Y - fNorms[j].X));
  end else
  begin
    AddPoint(
      fInPath[j].X + fDelta * (fNorms[k].X + fNorms[k].Y),
      fInPath[j].Y + fDelta * (fNorms[k].Y - fNorms[k].X));
    AddPoint(
      fInPath[j].X + fDelta * (fNorms[j].X - fNorms[j].Y),
      fInPath[j].Y + fDelta * (fNorms[j].Y + fNorms[j].X));
  end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoMiter(j, k: Integer; cosAplus1: Double);
var
  q: Double;
begin
  //see offset_triginometry4.svg
  q := fDelta / cosAplus1;
  AddPoint(fInPath[j].X + (fNorms[k].X + fNorms[j].X)*q,
    fInPath[j].Y + (fNorms[k].Y + fNorms[j].Y)*q);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoRound(j, k: Integer; angle: double);
var
  i, steps: Integer;
  stepSin, stepCos: Extended;
  pt, pt2: TPointD;
begin
	//even though angle may be negative this is a convex join
  pt := fInPath[j];
  pt2.X := fNorms[k].X * fDelta;
  pt2.Y := fNorms[k].Y * fDelta;
  AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);

  steps := Ceil(fStepsPerRad * abs(angle));
  if steps > 0 then
  begin
    Math.SinCos(angle / steps, stepSin, stepCos);
    for i := 1 to steps -1 do
    begin
      pt2 := PointD(pt2.X * stepCos - stepSin * pt2.Y,
        pt2.X * stepSin + pt2.Y * stepCos);
      AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);
    end;
  end;

  pt2.X := fNorms[j].X * fDelta;
  pt2.Y := fNorms[j].Y * fDelta;
  AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPoint(j: Integer; var k: integer);
var
  sinA, cosA: Double;
  p1, p2: TPointD;
begin
  //A: angle between adjoining edges (on left side WRT winding direction).
  //A == 0 deg (or A == 360 deg): collinear edges heading in same direction
  //A == 180 deg: collinear edges heading in opposite directions (ie a 'spike')
  //sin(A) < 0: convex on left.
  //cos(A) > 0: angles on both left and right sides > 90 degrees
  sinA := (fNorms[k].X * fNorms[j].Y - fNorms[j].X * fNorms[k].Y);

  if (sinA > 1.0) then sinA := 1.0
  else if (sinA < -1.0) then sinA := -1.0;

  if sinA * fDelta < 0 then //ie a concave offset
  begin
    p1.X := fInPath[j].X + fNorms[k].X * fDelta;
    p1.Y := fInPath[j].Y + fNorms[k].Y * fDelta;
    p2.X := fInPath[j].X + fNorms[j].X * fDelta;
    p2.Y := fInPath[j].Y + fNorms[j].Y * fDelta;
    AddPoint(p1);
    if not PointsNearEqual(p1, p2, FMinLenSqrd) then
    begin
      AddPoint(fInPath[j]); //this aids with clipping removal later
      AddPoint(p2);
    end;
  end else
  begin
    cosA := DotProduct(fNorms[j], fNorms[k]);
    //convex offsets here ...
    case fJoinType of
      jtMiter:
        //see offset_triginometry3.svg
        if (1 + cosA < fTmpLimit) then
          DoSquare(j, k) else
          DoMiter(j, k, 1 + cosA);
      jtSquare:
        begin
          //angles >= 90 deg. don't need squaring
          if cosA >= 0 then
            DoMiter(j, k, 1 + cosA) else
            DoSquare(j, k);
        end
      else
        DoRound(j, k, ArcTan2(sinA, cosA));
    end;
  end;
  k := j;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
