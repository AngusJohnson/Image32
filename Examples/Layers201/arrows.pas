unit arrows;

interface

uses
  Windows, Messages, SysUtils, Types, Classes, Math,
  Img32, Img32.Layers, Main, Img32.Draw, Img32.Extra,
  Img32.Vector, Img32.Transform;

type

  TMyArrowLayer32 = class(TMyVectorLayer32) //TMyVectorLayer32 - see main.pas
  public
    procedure Init(const centerPt: TPointD);
    procedure UpdateArrow(btnGroup: TGroupLayer32; btnIdx: integer);
  end;

var
  defaultArrowBtns: TPathsD;

implementation


//------------------------------------------------------------------------------
// TMyArrowLayer32 - demonstrates using custom designer buttons
//------------------------------------------------------------------------------

procedure TMyArrowLayer32.Init(const centerPt: TPointD);
var
  rec: TRectD;
begin
  rec := Img32.Vector.GetBoundsD(defaultArrowBtns);
  Self.Paths := OffsetPath(defaultArrowBtns,
    centerPt.X - rec.Left - rec.Width/2,
    centerPt.Y -rec.Top - rec.Height/2);
end;
//------------------------------------------------------------------------------

function TurnsLeft2(const pt1, pt2, pt3: TPointD): boolean;
begin
  result := CrossProduct(pt1, pt2, pt3) <= 0;
end;
//---------------------------------------------------------------------------

function TurnsRight2(const pt1, pt2, pt3: TPointD): boolean;
begin
  result := CrossProduct(pt1, pt2, pt3) >= 0;
end;
//---------------------------------------------------------------------------

function GrtrEqu90(const pt1, pt2, pt3: TPointD): boolean;
begin
  result := DotProduct(pt1, pt2, pt3) <= 0;
end;
//---------------------------------------------------------------------------

function AlmostParallel(const pt1, pt2, pt3: TPointD): boolean;
var
  a: double;
const
  angle5 = angle1 *5;
  angle175 = angle180 - angle5;
begin
  //precondition: angle <= 180 degrees
  a := Abs(GetAngle(pt1, pt2, pt3));
  Result := (a < angle5) or (a > angle175);
end;
//---------------------------------------------------------------------------

function AlmostPerpendicular(const pt1, pt2, pt3: TPointD): boolean;
var
  a: double;
const
  angle5 = angle1 *5;
  angle85 = angle90 - angle5;
begin
  //precondition: angle <= 90 degrees
  a := GetAngle(pt1, pt2, pt3);
  Result := a > angle85;
end;
//---------------------------------------------------------------------------

procedure TMyArrowLayer32.UpdateArrow(btnGroup: TGroupLayer32;
  btnIdx: integer);
var
  i: integer;
  center, newPt, pt2, vec: TPointD;
  dist: Double;
  p: TPathD;
label
  bottom; //goto label
begin
  //preserve arrow symmetry and avoids 'broken' non-arrow polygons
  p := Copy(Paths[0], 0, 7);
  center := Img32.Vector.MidPoint(p[3], p[4]);
  newPt := btnGroup[btnIdx].MidPoint;
  case btnIdx of
    0:
      begin
        newPt := ClosestPointOnLine(newPt, p[0], center);
        if TurnsRight(newPt, p[1], p[6]) and
          TurnsRight(newPt, p[1], p[2]) then
          p[0] := newPt;
      end;
    1:
      begin
        dist := Distance(p[2], p[5]) * 0.5;
        pt2 := ClosestPointOnLine(newPt, p[0], center);
        if GrtrEqu90(newPt, center, p[0]) or
          GrtrEqu90(newPt, p[0], center) or
          (Distance(newPt, pt2) <= dist) or
          AlmostPerpendicular(center, p[0], newPt) then Goto bottom;
        p[1] := newPt;
        p[6] := ReflectPoint(newPt, pt2);
        if TurnsLeft2(newPt, p[2], p[0]) or
          TurnsRight(newPt, p[2], p[5]) or
          TurnsRight2(newPt, p[2], p[3]) then
        begin
          vec := GetUnitVector(pt2, p[1]);
          p[2] := PointD(pt2.X + vec.X * dist, pt2.Y + vec.Y * dist);
          p[5] := PointD(pt2.X + vec.X * -dist, pt2.Y + vec.Y * -dist);
        end;
      end;
    2:
      begin
        if TurnsRight2(newPt, p[1], p[0]) or
          TurnsRight2(p[1], newPt, p[3]) or
          TurnsLeft2(newPt, p[3], p[4]) or
          TurnsLeft2(p[0], newPt, center) or
          AlmostParallel(p[0], newPt, center) then Goto bottom;
        p[2] := newPt;
        pt2 := ClosestPointOnLine(newPt, p[0], center);
        p[5] := ReflectPoint(newPt, pt2);
        if TurnsRight2(p[1], newPt, p[6]) then
        begin
          dist := Distance(p[1], p[6]) * 0.5;
          vec := GetUnitNormal(p[0], center); //perpendicular to center line
          p[1] := PointD(pt2.X + vec.X * dist, pt2.Y + vec.Y * dist);
          p[6] := PointD(pt2.X + vec.X * -dist, pt2.Y + vec.Y * -dist);
        end;
      end;
    3:
      begin
        if TurnsRight(newPt, p[0], center) or
          TurnsRight2(p[1], p[2], newPt) then Goto bottom;
        p[3] := newPt;
        center := ClosestPointOnLine(newPt, p[0], center);
        p[4] := ReflectPoint(newPt, center);
      end;
    4:
      begin
        if TurnsLeft(newPt, p[0], center) or
          TurnsLeft2(p[6], p[5], newPt) then Goto bottom;
        p[4] := newPt;
        center := ClosestPointOnLine(newPt, p[0], center);
        p[3] := ReflectPoint(newPt, center);
      end;
    5:
      begin
        if TurnsLeft2(newPt, p[6], p[0]) or
          TurnsLeft2(p[6], newPt, p[4]) or
          TurnsRight2(newPt, p[4], p[3]) or
          TurnsRight2(p[0], newPt, center) or
          AlmostParallel(p[0], newPt, center) then Goto bottom;
        p[5] := newPt;
        pt2 := ClosestPointOnLine(newPt, p[0], center);
        p[2] := ReflectPoint(newPt, pt2);
        if TurnsLeft2(p[6], newPt, p[1]) then
        begin
          dist := Distance(p[6], p[1]) * 0.5;
          vec := GetUnitNormal(center, p[0]); //perpendicular to center line
          p[6] := PointD(pt2.X + vec.X * dist, pt2.Y + vec.Y * dist);
          p[1] := PointD(pt2.X + vec.X * -dist, pt2.Y + vec.Y * -dist);
        end;
      end;
    6:
      begin
        dist := Distance(p[5], p[2]) * 0.5;
        pt2 := ClosestPointOnLine(newPt, p[0], center);
        if GrtrEqu90(newPt, center, p[0]) or
          GrtrEqu90(newPt, p[0], center) or
          (Distance(newPt, pt2) <= dist) or
          AlmostPerpendicular(center, p[0], newPt) then Goto bottom;
        p[6] := newPt;
        p[1] := ReflectPoint(newPt, pt2);
        if TurnsRight2(newPt, p[5], p[0]) or
          TurnsLeft(newPt, p[5], p[2]) or
          TurnsLeft2(newPt, p[5], p[4]) then
        begin
          vec := GetUnitVector(pt2, p[6]);
          p[5] := PointD(pt2.X + vec.X * dist, pt2.Y + vec.Y * dist);
          p[2] := PointD(pt2.X + vec.X * -dist, pt2.Y + vec.Y * -dist);
        end;
      end;
  end;

bottom: //label
  for i := 0 to btnGroup.ChildCount -1 do
    btnGroup[i].PositionCenteredAt(p[i]);
  Paths := Img32.Vector.Paths(p);
end;
//---------------------------------------------------------------------------

initialization
  SetLength(defaultArrowBtns, 1);
  defaultArrowBtns[0] :=
    MakePathI([0,100, 100,0, 100,50, 200,50, 200,150, 100,150, 100,200]);

end.
