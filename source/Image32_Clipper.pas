unit Image32_Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.52                                                            *
* Date      :  1 October 2020                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  Wrapper module for the Clipper library                          *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  ClipperCore, Clipper, ClipperOffset,
  Image32, Image32_Draw, Image32_Vector;

//nb: InflatePolygons assumes that there's consistent winding where
//outer paths wind in one direction and inner paths in the other
function InflatePolygon(const polygon: TPathD;
  delta: Double; joinStyle: TJoinStyle = jsAuto;
  miterLimit: double = 2.0): TPathsD;
function InflatePolygons(const polygons: TPathsD;
  delta: Double; joinStyle: TJoinStyle = jsAuto;
  miterLimit: double = 2.0): TPathsD;

function InflateOpenPath(const path: TPathD;
  delta: Double; joinStyle: TJoinStyle = jsAuto; endStyle: TEndStyle = esSquare;
  miterLimit: double = 2.0): TPathsD;
function InflateOpenPaths(const paths: TPathsD;
  delta: Double; joinStyle: TJoinStyle = jsAuto; endStyle: TEndStyle = esSquare;
  miterLimit: double = 2.0): TPathsD;

//UnionPolygon: removes self-intersections
function UnionPolygon(const polygon: TPathD;
  fillRule: TFillRule): TPathsD;

function UnionPolygons(const polygons: TPathsD;
  fillRule: TFillRule): TPathsD; overload;
function UnionPolygons(const polygon1, polygon2: TPathD;
  fillRule: TFillRule): TPathsD; overload;
function UnionPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD; overload;

function IntersectPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;

function DifferencePolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;

implementation

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function InflatePolygon(const polygon: TPathD;
  delta: Double; joinStyle: TJoinStyle;
  miterLimit: double): TPathsD;
var
  polygons: TPathsD;
begin
  setLength(polygons, 1);
  polygons[0] := polygon;
  Result := InflatePolygons(polygons, delta, joinStyle, miterLimit);
end;
//------------------------------------------------------------------------------

function InflatePolygons(const polygons: TPathsD;
  delta: Double; joinStyle: TJoinStyle;
  miterLimit: double): TPathsD;
var
  jt: ClipperOffset.TJoinType;
begin
  case joinStyle of
    jsSquare: jt := jtSquare;
    jsMiter: jt :=  jtMiter;
    jsRound: jt := jtRound;

    else if abs(delta) <= 2 then
      jt := jtSquare else
      jt := jtRound;
  end;
  Result := TPathsD(ClipperOffsetPaths(
    ClipperCore.TPathsD(polygons), delta, jt, etPolygon, miterLimit));
end;
//------------------------------------------------------------------------------

function InflateOpenPath(const path: TPathD;
  delta: Double; joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimit: double): TPathsD;
var
  paths: TPathsD;
begin
  setLength(paths, 1);
  paths[0] := path;
  Result := InflateOpenPaths(paths, delta, joinStyle, endStyle, miterLimit);
end;
//------------------------------------------------------------------------------

function InflateOpenPaths(const paths: TPathsD;
  delta: Double; joinStyle: TJoinStyle; endStyle: TEndStyle;
  miterLimit: double): TPathsD;
var
  jt: ClipperOffset.TJoinType;
  et: TEndType;
begin
  case joinStyle of
    jsSquare: jt := jtSquare;
    jsMiter:  jt :=  jtMiter;
    jsRound:  jt := jtRound;
    else if endStyle = esRound then jt := jtRound
    else jt := jtSquare;
  end;
  case endStyle of
    esButt: et := etOpenButt;
    esSquare: et := etOpenSquare;
    else et := etOpenRound;
  end;
  Result := TPathsD(ClipperOffsetPaths(
    ClipperCore.TPathsD(paths), delta, jt, et, miterLimit));
end;
//------------------------------------------------------------------------------

function UnionPolygon(const polygon: TPathD; fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddPath(ClipperCore.TPathD(polygon));
    Execute(ctUnion,
      ClipperCore.TFillRule(fillRule), ClipperCore.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygons: TPathsD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddPaths(ClipperCore.TPathsD(polygons));
    Execute(ctUnion,
      ClipperCore.TFillRule(fillRule), ClipperCore.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygon1, polygon2: TPathD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddPath(ClipperCore.TPathD(polygon1), ptSubject);
    AddPath(ClipperCore.TPathD(polygon2), ptClip);
    Execute(ctUnion,
      ClipperCore.TFillRule(fillRule), ClipperCore.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function UnionPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddPaths(ClipperCore.TPathsD(polygons1), ptSubject);
    AddPaths(ClipperCore.TPathsD(polygons2), ptClip);
    Execute(ctUnion,
      ClipperCore.TFillRule(fillRule), ClipperCore.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function IntersectPolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddPaths(ClipperCore.TPathsD(polygons1), ptSubject);
    AddPaths(ClipperCore.TPathsD(polygons2), ptClip);
    Execute(ctIntersection,
      ClipperCore.TFillRule(fillRule), ClipperCore.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function DifferencePolygons(const polygons1, polygons2: TPathsD;
  fillRule: TFillRule): TPathsD;
begin
  with TClipperD.Create do
  try
    AddPaths(ClipperCore.TPathsD(polygons1), ptSubject);
    AddPaths(ClipperCore.TPathsD(polygons2), ptClip);
    Execute(ctDifference,
      ClipperCore.TFillRule(fillRule), ClipperCore.TPathsD(result));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

end.
