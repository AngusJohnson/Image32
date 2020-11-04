unit ClipperOffset;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0 (beta)                                                     *
* Date      :  24 October 2020                                                 *
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

  TPathGroup = class
	  paths     : TPathsD;
	  joinType  : TJoinType;
	  endType   : TEndType;
    constructor Create(const paths: TPathsD; jt: TJoinType; et: TEndType);
  end;

  TClipperOffset = class
  private
    fDelta       : Double;
    fJoinType    : TJoinType;
    fTmpLimit    : Double;
    fMiterLimit  : Double;
    fArcTolerance: Double;
    fStepsPerRad : Double;
    fNorms       : TPathD;
    fInGroups    : TList;
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

    procedure CheckPaths(et: TEndType);
    procedure BuildNormals;
    procedure DoOffset(const paths: TPathsD;
      delta: double; joinType: TJoinType; endType: TEndType);
    procedure OffsetPolygon;
    procedure OffsetOpenJoined;
    procedure OffsetOpenPath(endType: TEndType);
  public
    constructor Create(MiterLimit: double = 2.0; ArcTolerance: double = 0.0);
    destructor Destroy; override;
    procedure AddPath(const path: TPathD;
      joinType: TJoinType; endType: TEndType);
    procedure AddPaths(const paths: TPathsD;
      joinType: TJoinType; endType: TEndType);
    procedure Clear;
    function Execute(delta: Double): TPathsD;

    property MiterLimit: Double read fMiterLimit write fMiterLimit;
    property ArcTolerance: Double read fArcTolerance write fArcTolerance;
  end;

  function ClipperOffsetPaths(const paths: TPaths;
    delta: Double; jt: TJoinType; et: TEndType;
    miterLimit: double = 2.0;
    arcTolerance: double = 0.0): TPaths; overload;

  function ClipperOffsetPaths(const paths: TPathsD;
    delta: Double; jt: TJoinType; et: TEndType;
    miterLimit: double = 2.0;
    arcTolerance: double = 0.0): TPathsD; overload;

implementation

const
  Tolerance           : Double = 1.0E-15;
  Two_Pi              : Double = 2 * PI;
  Half_Pi             : Double = 0.5 * PI;
  Quarter_Pi          : Double = 0.25 * PI;

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
	for i := 0 to High(paths) do
		if Length(paths[i]) > 0 then
    begin
			Result := i;
			lp := paths[i][0];
			break;
		end;
	if (Result < 0) then Exit;

	for i := Result to High(paths) do
	begin
		p := paths[i];
		for j := 0 to High(p) do
			if (p[j].Y > lp.Y) or ((p[j].Y = lp.Y) and (p[j].X < lp.X)) then
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

constructor TPathGroup.Create(const paths: TPathsD; jt: TJoinType; et: TEndType);
begin
  Self.paths := CopyPaths(paths);
  Self.joinType := jt;
  Self.endType := et;
end;

//------------------------------------------------------------------------------
// TClipperOffset methods
//------------------------------------------------------------------------------

constructor TClipperOffset.Create(MiterLimit: double; ArcTolerance: double);
begin
  inherited Create;
  fMiterLimit := MiterLimit;
  fArcTolerance := ArcTolerance;
  fInGroups     := TList.Create;
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
  node: TPathGroup;
begin
  if Length(paths) = 0 then Exit;
  node := TPathGroup.Create(paths, joinType, endType);
  fInGroups.Add(node);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.CheckPaths(et: TEndType);
var
  i,j, len, minLen: Integer;
  openPaths: Boolean;
begin
  for i := 0 to fInGroups.Count -1 do
    with TPathGroup(fInGroups[i]) do
    begin
      openPaths := not (et in [etPolygon, etOpenJoined]);
      if openPaths then minLen := 1 else minLen := 3;
      for j := 0 to High(paths) do
      begin
        StripDuplicates(paths[j]);
        len := length(paths[j]);
        if (len > 1) and not openPaths and
          PointsEqual(paths[j][0], paths[j][len-1]) then
        begin
          setlength(paths[j], len -1);
          dec(len);
        end;
        if len < minLen then paths[j] := nil;
      end;
    end;
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.DoOffset(const paths: TPathsD;
  delta: double; joinType: TJoinType; endType: TEndType);
var
  i, lowestIdx: Integer;
  arcTol, steps: Double;
  outPathsI: TPaths;
  openPaths, isClockwise: Boolean;
const
  minVert: array [boolean] of integer = (1, 3);
begin
  fJoinType := joinType;
  CheckPaths(endType);

  if endType <> etPolygon then delta := Abs(delta) / 2;
  openPaths := not (endType in [etPolygon, etOpenJoined]);
  if not openPaths then
  begin
    //th  e lowermost polygon must be an outer polygon. So we can use that as the
    //designated orientation for outer polygons (needed for tidy-up clipping)
    lowestIdx := GetLowestPolygonIdx(paths);
    if lowestIdx < 0 then Exit;
    isClockwise := Area(paths[lowestIdx]) > 0;
    if not isClockwise then delta := -delta;
  end else
    isClockwise := true;
  fDelta := delta;

  //fMiterLimit: see offset_triginometry3.svg
  if fMiterLimit > 1 then
    fTmpLimit := 2/(sqr(fMiterLimit)) else
    fTmpLimit := 2;

  //default elliptical precision to 1/10 radius
  if fArcTolerance > 0 then
    arcTol := fArcTolerance else
    arcTol := 0.1 * Abs(fDelta);

  //calculate a sensible number of steps (for 360 deg for the given offset
  if (joinType = jtRound) or (endType = etOpenRound) then
  begin
    //get steps per 180 degrees (see offset_triginometry2.svg)
    steps := PI / ArcCos(1 - arcTol / Abs(fDelta));
    //steps := Min(steps, fDelta * Pi); //avoid excessive precision
    fStepsPerRad := steps / Two_Pi;
  end;

  fOutPaths := nil;
  for i := 0 to High(paths) do
  begin
    fInPath := paths[i];
    //open paths may have a single vertex, but polygons must have 3.
    if Length(fInPath) < minVert[endType = etPolygon] then Exit;

    fNorms := nil;
    fOutPath := nil;
    fOutPathLen := 0;

		//if a single vertex then build a circle or a square ...
    if Length(fInPath) = 1 then
    begin
      if (endType = etOpenRound) then
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
      if endType = etPolygon then
      begin
        OffsetPolygon;
      end
      else if endType = etOpenJoined then
      begin
        OffsetOpenJoined;
      end else
        OffsetOpenPath(endType);
    end;

    if fOutPathLen = 0 then Continue;
    SetLength(fOutPath, fOutPathLen);
    AppendPath(fOutPaths, fOutPath);
  end;

  //clean up self-intersections ...
  outPathsI := ScalePaths(fOutPaths, 100, 100);
  with TClipper.Create do
  try
    AddPaths(outPathsI, ptSubject);
    if not isClockwise then
      Execute(ctUnion, frNegative, outPathsI) else
      Execute(ctUnion, frPositive, outPathsI);
  finally
    free;
  end;
  fOutPaths := ScalePathsD(outPathsI, 0.01, 0.01);

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
var
  i,j: integer;
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
    etOpenButt: DoButtEnd(highI);
    etOpenRound: DoRound(highI, k, PI);
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
    etOpenButt: DoButtStart;
    etOpenRound: DoRound(0, 1, PI);
    else doSquare(0, 1);
  end;
end;
//------------------------------------------------------------------------------

function TClipperOffset.Execute(delta: Double): TPathsD;
var
  i: integer;
begin
  fSolution := nil;
  Result := nil;
  if fInGroups.Count = 0 then Exit;

  if abs(delta) < Tolerance then
  begin
    //if delta == 0, just copy paths to Result
    for i := 0 to fInGroups.Count -1 do
      with TPathGroup(fInGroups[i]) do
          AppendPaths(Result, paths);
  end else
  begin
	  //nb: delta will depend on whether paths are polygons or open
    for i := 0 to fInGroups.Count -1 do
      with TPathGroup(fInGroups[i]) do
        DoOffset(paths, delta, jointype, endtype);
    Result := fSolution;
  end;
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
    PointsEqual(fOutPath[fOutPathLen-1], pt) then Exit;
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
  if (fOutPathLen > 0) and PointsEqual(fOutPath[fOutPathLen-1], pt) then Exit;
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

  steps := Round(fStepsPerRad * abs(angle));
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

  pt := fInPath[j];
  pt2.X := fNorms[j].X * fDelta;
  pt2.Y := fNorms[j].Y * fDelta;
  AddPoint(pt.X + pt2.X, pt.Y + pt2.Y);
end;
//------------------------------------------------------------------------------

procedure TClipperOffset.OffsetPoint(j: Integer; var k: integer);
var
  sinA, cosA: Double;
begin
  //A: angle between adjoining edges (on left side WRT winding direction).
  //A == 0 deg (or A == 360 deg): collinear edges heading in same direction
  //A == 180 deg: collinear edges heading in opposite directions (ie a 'spike')
  //sin(A) < 0: convex on left.
  //cos(A) > 0: angles on both left and right sides > 90 degrees
  sinA := (fNorms[k].X * fNorms[j].Y - fNorms[j].X * fNorms[k].Y);

  if (sinA < 0.01) and (sinA > -0.01) then
  begin
    k := j;
    Exit;
  end;

  if (sinA > 1.0) then sinA := 1.0
  else if (sinA < -1.0) then sinA := -1.0;

  if sinA * fDelta < 0 then //ie a concave offset
  begin
    AddPoint(fInPath[j].X + fNorms[k].X * fDelta,
      fInPath[j].Y + fNorms[k].Y * fDelta);
    AddPoint(fInPath[j]); //this aids with clipping removal later
    AddPoint(fInPath[j].X + fNorms[j].X * fDelta,
      fInPath[j].Y + fNorms[j].Y * fDelta);
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

function ClipperOffsetPaths(const paths: TPaths;
  delta: Double; jt: TJoinType; et: TEndType;
  miterLimit: double; arcTolerance: double): TPaths;
var
  pp: TPathsD;
const
  scale = 100; invScale = 1/scale;
begin
  pp := ScalePathsD(paths, scale, scale);
  with TClipperOffset.Create(miterLimit, arcTolerance) do
  try
    AddPaths(pp, jt, et);
    pp := Execute(delta);
  finally
    free;
  end;
  Result := ScalePaths(pp, invScale, invScale);
end;
//------------------------------------------------------------------------------

function ClipperOffsetPaths(const paths: TPathsD;
  delta: Double; jt: TJoinType; et: TEndType;
  miterLimit: double; arcTolerance: double): TPathsD;
begin
  with TClipperOffset.Create(miterLimit, arcTolerance) do
  try
    AddPaths(paths, jt, et);
    Result := Execute(delta);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

end.
