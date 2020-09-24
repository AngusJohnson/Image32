unit Clipper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  10.0                                                            *
* Date      :  18 September 2020                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  This is the main polygon clipping module                        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

{$IFDEF FPC}
  {$DEFINE INLINING}
  {$MODE DELPHI}
{$ELSE}
  {$IF CompilerVersion >= 18}         //Delphi 2007
    //While Inlining has been supported since D2005, both D2005 and D2006
    //have an Inline codegen bug (QC41166) so ignore Inline until D2007.
    {$DEFINE INLINING}
    {$IF COMPILERVERSION >= 23}         //Delphi XE2+
      {$DEFINE XPLAT_GENERICS}
      {$IF CompilerVersion >= 25.0}     //Delphi XE4+
        {$LEGACYIFEND ON}
      {$IFEND}
    {$IFEND}
  {$IFEND}
  {$IF CompilerVersion < 14}
    Requires Delphi version 6 or above.
  {$IFEND}
{$ENDIF}

{$IFDEF DEBUG}
  {$UNDEF INLINING}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math,
{$IFDEF XPLAT_GENERICS}
  Generics.Collections, Generics.Defaults,
{$ENDIF}
  ClipperCore;

type
  TVertexFlag = (vfOpenStart, vfOpenEnd, vfLocMax, vfLocMin);
  TVertexFlags = set of TVertexFlag;

  PVertex = ^TVertex;
  TVertex = record
    Pt    : TPoint64;
    next  : PVertex;
    prev  : PVertex;
    flags : TVertexFlags;
  end;

  PVertexArray = ^TVertexArray;
  TVertexArray = array[0..MaxInt div sizeof(TVertex) -1] of TVertex;

  //Every closed path (or polygon) is made up of a series of vertices forming
  //edges that alternate between going up (relative to the Y-axis) and going
  //down. Edges consecutively going up or consecutively going down are called
  //'bounds' (or sides if they're simple polygons). 'Local Minima' refer to
  //vertices where descending bounds become ascending ones.

  PLocalMinima = ^TLocalMinima;
  TLocalMinima = record
    vertex    : PVertex;
    PolyType  : TPathType;
    IsOpen    : Boolean;
  end;

  TOutRec = class;

  TOutPt = class
    Pt       : TPoint64;
    Next     : TOutPt;
    Prev     : TOutPt;
    OutRec   : TOutRec;       //used in descendant classes
  end;

  PActive = ^TActive;
  TActive = record
    op       : TOutPt;        //used in descendant classes
    Bot      : TPoint64;
    Top      : TPoint64;
    CurrX    : Int64;
    Dx       : Double;        //inverse of edge slope (zero = vertical)
    WindDx   : Integer;       //wind direction (ascending: +1; descending: -1)
    WindCnt  : Integer;       //current wind count
    WindCnt2 : Integer;       //current wind count of the opposite TPolyType
    OutRec   : TOutRec;
    //AEL: 'active edge list' (Vatti's AET - active edge table)
    //     a linked list of all edges (from left to right) that are present
    //     (or 'active') within the current scanbeam (a horizontal 'beam' that
    //     sweeps from bottom to top over the paths in the clipping operation).
    PrevInAEL: PActive;
    NextInAEL: PActive;
    //SEL: 'sorted edge list' (Vatti's ST - sorted table)
    //     linked list used when sorting edges into their new positions at the
    //     top of scanbeams, but also (re)used to process horizontals.
    PrevInSEL: PActive;
    NextInSEL: PActive;
    Jump     : PActive;       //for merge sorting (see BuildIntersectList())
    VertTop  : PVertex;
    LocMin   : PLocalMinima;  //the bottom of an edge 'bound' (also Vatti)
  end;

  PIntersectNode = ^TIntersectNode;
  TIntersectNode = record
    Edge1  : PActive;
    Edge2  : PActive;
    Pt     : TPoint64;
  end;

  PScanLine = ^TScanLine;
  TScanLine = record
    Y        : Int64;
    Next     : PScanLine;
  end;

  TOutRecState = (osUndefined, osOpen, osOuter,
    osOuterCheck, osInner, osInnerCheck);

  TPolyTree  = class;
  TPolyPath  = class;
  TPolyTreeD = class;
  TPolyPathD = class;

  //OutRec: contains a path in the clipping solution. Edges in the AEL will
  //have OutRec pointers assigned when they form part of the clipping solution.
  TOutRec = class
    Idx      : Integer;
    Owner    : TOutRec;
    frontE   : PActive;
    backE    : PActive;
    Pts      : TOutPt;
    PolyPath : TPolyPath;
    State    : TOutRecState;
  end;

  TClipper = class
  private
    FBotY               : Int64;
    FScanLine           : PScanLine;
    FLocMinListSorted   : Boolean;
    FHasOpenPaths       : Boolean;
    FCurrentLocMinIdx   : Integer;
    FClipType           : TClipType;
    FFillRule           : TFillRule;
{$IFDEF XPLAT_GENERICS}
    FIntersectList      : TList<PIntersectNode>;
    FOutRecList         : TList<TOutRec>;
    FLocMinList         : TList<PLocalMinima>;
    FVertexList         : TList<PVertex>;
{$ELSE}
    FIntersectList      : TList;
    FOutRecList         : TList;
    FLocMinList         : TList;
    FVertexList         : TList;
{$ENDIF}
    FActives            : PActive; //see AEL above
    FSel                : PActive; //see SEL above
    procedure Reset;
    procedure InsertScanLine(const Y: Int64);
    function PopScanLine(out Y: Int64): Boolean;
    function PopLocalMinima(Y: Int64;
      out localMinima: PLocalMinima): Boolean;
    procedure DisposeScanLineList;
    procedure DisposeOutRec(index: Integer);
    procedure DisposeAllOutRecs;
    procedure DisposeVerticesAndLocalMinima;
    procedure AddPathToVertexList(const p: TPath;
      polyType: TPathType; isOpen: Boolean);
    function IsContributingClosed(e: PActive): Boolean;
    function IsContributingOpen(e: PActive): Boolean;
    procedure SetWindCountForClosedPathEdge(e: PActive);
    procedure SetWindCountForOpenPathEdge(e: PActive);
    procedure InsertLocalMinimaIntoAEL(const botY: Int64);
    procedure InsertLeftEdge(e: PActive);
    procedure PushHorz(e: PActive); {$IFDEF INLINING} inline; {$ENDIF}
    function PopHorz(out e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
    procedure StartOpenPath(e: PActive; const pt: TPoint64);
    procedure UpdateEdgeIntoAEL(var e: PActive);
    procedure IntersectEdges(e1, e2: PActive;
      const pt: TPoint64; orientationCheckRequired: Boolean = false);
    procedure DeleteFromAEL(e: PActive);
    procedure AdjustCurrXAndCopyToSEL(topY: Int64);
    procedure DoIntersections(const topY: Int64);
    procedure DisposeIntersectNodes;
    procedure AddNewIntersectNode(e1, e2: PActive; topY: Int64);
    function BuildIntersectList(const topY: Int64): Boolean;
    procedure ProcessIntersectList;
    procedure SwapPositionsInAEL(e1, e2: PActive);
    procedure DoHorizontal(horzEdge: PActive);
    procedure DoTopOfScanbeam(Y: Int64);
    function DoMaxima(e: PActive): PActive;
    function AddOutPt(e: PActive; const pt: TPoint64): TOutPt;
    procedure AddLocalMinPoly(e1, e2: PActive; const pt: TPoint64;
      IsNew: Boolean = false; orientationCheckRequired: Boolean = false);
    procedure AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64);
    procedure JoinOutrecPaths(e1, e2: PActive);
    function GetIntersectNode(index: integer): PIntersectNode;
      {$IFDEF INLINING} inline; {$ENDIF}
  protected
    procedure CleanUp; //unlike Clear, CleanUp preserves added paths
    procedure ExecuteInternal(clipType: TClipType;
      fillRule: TFillRule); virtual;
    function BuildResult(out closedPaths, openPaths: TPaths): Boolean;
    function BuildResultTree(polyTree: TPolyTree; out openPaths: TPaths): Boolean;
{$IFDEF XPLAT_GENERICS}
    property OutRecList: TList<TOutRec> read FOutRecList;
{$ELSE}
    property OutRecList: TList read FOutRecList;
{$ENDIF}
    property IntersectNode[index: integer]: PIntersectNode
      read GetIntersectNode;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function GetBounds: TRect64;

    //ADDPATH & ADDPATHS METHODS ...
    //Integer paths (TPath) ...
    procedure AddPath(const path64: TPath; polyType: TPathType = ptSubject;
      isOpen: Boolean = false);
    procedure AddPaths(const paths64: TPaths; polyType: TPathType = ptSubject;
      isOpen: Boolean = false);
    //EXECUTE METHODS ...
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths: TPaths): Boolean; overload; virtual;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths, openPaths: TPaths): Boolean; overload; virtual;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      var polytree: TPolyTree; out openPaths: TPaths): Boolean; overload; virtual;
  end;

  TPolyPathClass = class of TPolyPath;

  TPolyPath = class
  private
    FParent      : TPolyPath;
    FClass       : TPolyPathClass;
    FPath        : TPath;
{$IFDEF XPLAT_GENERICS}
    FChildList   : TList<TPolyPath>;
{$ELSE}
    FChildList   : TList;
{$ENDIF}
    function     GetChildCnt: Integer;
    function     GetChild(index: Integer): TPolyPath;
    function     GetIsHole: Boolean;
  protected
    function     AddChild(const path: TPath): TPolyPath;
  public
    constructor  Create;  virtual;
    destructor   Destroy; override;
    procedure    Clear; virtual;
    property     Parent: TPolyPath read FParent;
    property     IsHole: Boolean read GetIsHole;
    property     ChildCount: Integer read GetChildCnt;
    property     Child[index: Integer]: TPolyPath read GetChild;
    property     Path: TPath read FPath;
  end;

  //TPolyTree: is intended as a READ-ONLY data structure for closed paths
  //returned by a clipping operation. While this structure is more complex than
  //the alternative TPaths structure, it does preserve path ownership (ie those
  //paths that contain (own) other paths) which may be useful to some users.
  TPolyTree = class(TPolyPath)
  public
    constructor  Create; override;
  end;

  //FLOATING POINT POLYGON COORDINATES (D suffix to indicate double precision)

  //TClipperD: is a TClipper wrapper to manage floating point conversions.
  //Since all clipping is done using integer polygon coordinates to maintain
  //robustness, floating point coords must be converted to and from integer
  //values. Also, scaling is often required to achieve a desired precision.

  TClipperD = class(TClipper)
  private
    FScale: double;
  public
    procedure AddPath(const pathD: TPathD; polyType: TPathType = ptSubject;
      isOpen: Boolean = false); overload;
    procedure AddPaths(const pathsD: TPathsD; polyType: TPathType = ptSubject;
      isOpen: Boolean = false); overload;

    constructor Create(scale: double); reintroduce; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths: TPathsD): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      out closedPaths, openPaths: TPathsD): Boolean; overload;
    function Execute(clipType: TClipType; fillRule: TFillRule;
      var polytree: TPolyTreeD; out openPaths: TPathsD): Boolean; overload;
  end;

  //TPolyPathD and TPolyTreeD are wrappers for TPolyPath and TPolyTree ...
  TPolyPathD = class(TPolyPath)
  private
    FScale : double;
    function  GetPathD: TPathD;
    function  GetChild(index: Integer): TPolyPathD;
  protected
    procedure SetScale(scale : double);
  public
    property  PathD: TPathD read GetPathD;
    property  Child[index: Integer]: TPolyPathD read GetChild;
  end;

  TPolyTreeD = class(TPolyPathD)
  public
    constructor Create; override;
  end;


  function PolyTreeToPaths(PolyTree: TPolyTree): TPaths; overload;
  function PolyTreeToPaths(PolyTree: TPolyTreeD): TPathsD; overload;

implementation

const
  DefaultScale = 100;


//OVERFLOWCHECKS OFF is a necessary workaround for a compiler bug that very
//occasionally reports incorrect overflow errors in Delphi versions before 10.2.
//see https://forums.embarcadero.com/message.jspa?messageID=871444
{$OVERFLOWCHECKS OFF}

resourcestring
  rsClipper_OpenPathErr = 'Only subject paths can be open.';
  rsClipper_PolyTreeErr = 'The TPolyTree parameter must be assigned.';
  rsClipper_ClippingErr = 'Undefined clipping error';
  rsClipper_ScalingErr  =
    'The scaling fraction must be assigned before any paths are added.';
  rsClipper_ScalingErr2 =
    'The scaling fraction must be greater than zero.';

//------------------------------------------------------------------------------
// Miscellaneous Functions ...
//------------------------------------------------------------------------------

procedure RaiseError(const msg: string); {$IFDEF INLINING} inline; {$ENDIF}
begin
  raise EClipperLibException.Create(msg);
end;
//------------------------------------------------------------------------------

function IsOpen(e: PActive): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.LocMin.IsOpen;
end;
//------------------------------------------------------------------------------

function IsOpen(outrec: TOutRec): Boolean; overload; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State = osOpen;
end;
//------------------------------------------------------------------------------

function IsOuter(outrec: TOutRec): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State in [osOuter, osOuterCheck];
end;
//------------------------------------------------------------------------------

procedure SetAsOuter(outrec: TOutRec); {$IFDEF INLINING} inline; {$ENDIF}
begin
  outrec.State := osOuter;
end;
//------------------------------------------------------------------------------

function IsInner(outrec: TOutRec): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := outrec.State in [osInner, osInnerCheck];
end;
//------------------------------------------------------------------------------

procedure SetAsInner(outrec: TOutRec); {$IFDEF INLINING} inline; {$ENDIF}
begin
  outrec.State := osInner;
end;
//------------------------------------------------------------------------------

procedure SetCheckFlag(outrec: TOutRec); {$IFDEF INLINING} inline; {$ENDIF}
begin
  if outrec.State = osInner then
    outrec.State := osInnerCheck
  else if outrec.State = osOuter then
    outrec.State := osOuterCheck;
end;
//------------------------------------------------------------------------------

procedure UnsetCheckFlag(outrec: TOutRec); {$IFDEF INLINING} inline; {$ENDIF}
begin
  if outrec.State = osInnerCheck then outrec.State := osInner
  else if outrec.State = osOuterCheck then outrec.State := osOuter;
end;
//------------------------------------------------------------------------------

function IsHotEdge(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := assigned(e.OutRec);
end;
//------------------------------------------------------------------------------

function GetPrevHotEdge(e: PActive): PActive; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.PrevInAEL;
  while assigned(Result) and (IsOpen(Result) or not IsHotEdge(Result)) do
    Result := Result.PrevInAEL;
end;
//------------------------------------------------------------------------------

function IsFront(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  //the front edge will be the LEFT edge when it's an OUTER polygon
  //so that outer polygons will be orientated clockwise
  Result := (e = e.OutRec.frontE);
end;
//------------------------------------------------------------------------------

function LastOp(e: PActive): TOutPt;
begin
  result := e.OutRec.Pts;
  if not IsFront(e) then
    result := result.Next;
end;
//------------------------------------------------------------------------------

function IsInvalidPath(op: TOutPt): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  result := not assigned(op) or (op.Next = op);
end;
//------------------------------------------------------------------------------

(*******************************************************************************
*  Dx:                             0(90deg)                                    *
*                                  |                                           *
*               +inf (180deg) <--- o ---> -inf (0deg)                          *
*******************************************************************************)

function GetDx(const pt1, pt2: TPoint64): double;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dy: Int64;
begin
  dy := (pt2.Y - pt1.Y);
  if dy <> 0 then result := (pt2.X - pt1.X) / dy
  else if (pt2.X > pt1.X) then result := NegInfinity
  else result := Infinity;
end;
//------------------------------------------------------------------------------

function TopX(e: PActive; const currentY: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if (currentY = e.Top.Y) or (e.Top.X = e.Bot.X) then Result := e.Top.X
  else Result := e.Bot.X + Round(e.Dx*(currentY - e.Bot.Y));
end;
//------------------------------------------------------------------------------

function TopX(const pt1, pt2: TPoint64; const Y: Int64): Int64; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
var
  dx: double;
begin
  if (Y = pt1.Y) then Result := pt1.X
  else if (Y = pt2.Y) then Result := pt2.X
  else if (pt1.Y = pt2.Y) or (pt1.X = pt2.X) then Result := pt2.X
  else
  begin
    dx := GetDx(pt1, pt2);
    Result := pt1.X + Round(dx * (Y - pt1.Y));
  end;
end;
//------------------------------------------------------------------------------

function IsHorizontal(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Top.Y = e.Bot.Y);
end;
//------------------------------------------------------------------------------

function IsHeadingRightHorz(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Dx = NegInfinity);
end;
//------------------------------------------------------------------------------

function IsHeadingLeftHorz(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := (e.Dx = Infinity);
end;
//------------------------------------------------------------------------------

procedure SwapActives(var e1, e2: PActive); {$IFDEF INLINING} inline; {$ENDIF}
var
  e: PActive;
begin
  e := e1; e1 := e2; e2 := e;
end;
//------------------------------------------------------------------------------

function GetPolyType(const e: PActive): TPathType;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.LocMin.PolyType;
end;
//------------------------------------------------------------------------------

function IsSamePolyType(const e1, e2: PActive): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e1.LocMin.PolyType = e2.LocMin.PolyType;
end;
//------------------------------------------------------------------------------

function GetIntersectPoint(e1, e2: PActive): TPoint64;
var
  b1, b2, m: Double;
begin
  if (e1.Dx = e2.Dx) then
  begin
    Result := e1.Top;
    Exit;
  end
  else if e1.Dx = 0 then
  begin
    Result.X := e1.Bot.X;
    if IsHorizontal(e2) then
      Result.Y := e2.Bot.Y
    else
    begin
      with e2^ do b2 := Bot.Y - (Bot.X/Dx);
      Result.Y := round(Result.X/e2.Dx + b2);
    end;
  end
  else if e2.Dx = 0 then
  begin
    Result.X := e2.Bot.X;
    if IsHorizontal(e1) then
      Result.Y := e1.Bot.Y
    else
    begin
      with e1^ do b1 := Bot.Y - (Bot.X/Dx);
      Result.Y := round(Result.X/e1.Dx + b1);
    end;
  end else
  begin
    with e1^ do b1 := Bot.X - Bot.Y * Dx;
    with e2^ do b2 := Bot.X - Bot.Y * Dx;
    m := (b2-b1)/(e1.Dx - e2.Dx);
    Result.Y := round(m);
    if Abs(e1.Dx) < Abs(e2.Dx) then
      Result.X := round(e1.Dx * m + b1) else
      Result.X := round(e2.Dx * m + b2);
  end;
end;
//------------------------------------------------------------------------------

procedure SetDx(e: PActive);  {$IFDEF INLINING} inline; {$ENDIF}
begin
  e.Dx := GetDx(e.Bot, e.Top);
end;
//------------------------------------------------------------------------------

function IsLeftBound(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := e.WindDx > 0;
end;
//------------------------------------------------------------------------------

function NextVertex(e: PActive): PVertex; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if IsLeftBound(e) then
    Result := e.vertTop.next else
    Result := e.vertTop.prev;
end;
//------------------------------------------------------------------------------

function NextVertex(op: PVertex; goingFwd: Boolean): PVertex; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if goingFwd then Result := op.next
  else Result := op.prev;
end;
//------------------------------------------------------------------------------

function PrevVertex(op: PVertex; goingFwd: Boolean): PVertex;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  if goingFwd then Result := op.prev
  else Result := op.next;
end;
//------------------------------------------------------------------------------

function CrossProduct(const pt1, pt2, pt3: TPoint64): double;
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

function IsClockwise(vertex: PVertex): boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := CrossProduct(vertex.prev.Pt, vertex.Pt, vertex.next.Pt) >= 0;
end;
//----------------------------------------------------------------------

function IsClockwise(op: TOutPt): boolean; overload;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := CrossProduct(op.prev.Pt, op.Pt, op.next.Pt) >= 0;
end;
//----------------------------------------------------------------------

function IsMaxima(e: PActive): Boolean; {$IFDEF INLINING} inline; {$ENDIF}
begin
  Result := vfLocMax in e.vertTop.flags;
end;
//------------------------------------------------------------------------------

procedure TerminateHotOpen(e: PActive);
begin
  if e.OutRec.frontE = e then
    e.OutRec.frontE := nil else
    e.OutRec.backE := nil;
  e.OutRec := nil;
end;
//------------------------------------------------------------------------------

function GetMaximaPair(e: PActive): PActive;
begin
  if IsHorizontal(e) then
  begin
    //we can't be sure whether the MaximaPair is on the left or right, so ...
    Result := e.PrevInAEL;
    while assigned(Result) and (Result.CurrX >= e.Top.X) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.PrevInAEL;
    end;
    Result := e.NextInAEL;
    while assigned(Result) and (TopX(Result, e.Top.Y) <= e.Top.X) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.NextInAEL;
    end;
  end else
  begin
    Result := e.NextInAEL;
    while assigned(Result) do
    begin
      if Result.vertTop = e.vertTop then Exit;  //Found!
      Result := Result.NextInAEL;
    end;
  end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function PointCount(pts: TOutPt): Integer; {$IFDEF INLINING} inline; {$ENDIF}
var
  p: TOutPt;
begin
  Result := 0;
  if not Assigned(pts) then Exit;
  p := pts;
  repeat
    Inc(Result);
    p := p.Next;
  until p = pts;
end;
//------------------------------------------------------------------------------

function BuildPath(op: TOutPt): TPath;
var
  i,j, opCnt: integer;
begin
  result := nil;
  opCnt := PointCount(op);
  if (opCnt < 2) then Exit;
  setLength(result, opCnt);
  result[0] := op.Pt;
  op := op.Next;
  j := 0;
  for i := 0 to opCnt -2 do
  begin
    if not PointsEqual(result[j], op.Pt) then
    begin
      inc(j);
      result[j] := op.Pt;
    end;
    op := op.Next;
  end;
  setLength(result, j +1);
end;
//------------------------------------------------------------------------------

procedure DisposeOutPt(pp: TOutPt); {$IFDEF INLINING} inline; {$ENDIF}
begin
  pp.Prev.Next := pp.Next;
  pp.Next.Prev := pp.Prev;
  pp.Free;
end;
//------------------------------------------------------------------------------

procedure DisposePolyPts(pp: TOutPt);  {$IFDEF INLINING} inline; {$ENDIF}
var
  tmpPp: TOutPt;
begin
  pp.Prev.Next := nil;
  while Assigned(pp) do
  begin
    tmpPp := pp;
    pp := pp.Next;
    tmpPp.Free;
  end;
end;
//------------------------------------------------------------------------------

function LocMinListSort(item1, item2: Pointer): Integer;
var
  dy: Int64;
begin
  dy := PLocalMinima(item2).vertex.Pt.Y - PLocalMinima(item1).vertex.Pt.Y;
  if dy < 0 then Result := -1
  else if dy > 0 then Result := 1
  else Result := 0;
end;
//------------------------------------------------------------------------------

procedure SetSides(outRec: TOutRec; startEdge, endEdge: PActive);
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  outRec.frontE := startEdge;
  outRec.backE := endEdge;
end;
//------------------------------------------------------------------------------

procedure SwapOutRecs(e1, e2: PActive);
var
  or1, or2: TOutRec;
  e: PActive;
begin
  or1 := e1.OutRec;
  or2 := e2.OutRec;
  if (or1 = or2) then
  begin
    e := or1.frontE;
    or1.frontE := or1.backE;
    or1.backE := e;
    Exit;
  end;
  if assigned(or1) then
  begin
    if e1 = or1.frontE then
      or1.frontE := e2 else
      or1.backE := e2;
  end;
  if assigned(or2) then
  begin
    if e2 = or2.frontE then
      or2.frontE := e1 else
      or2.backE := e1;
  end;
  e1.OutRec := or2;
  e2.OutRec := or1;
end;
//------------------------------------------------------------------------------

function Area(op: TOutPt): Double;
var
  op2: TOutPt;
  d: Double;
begin
  //positive results are clockwise
  Result := 0;
  op2 := op;
  if Assigned(op2) then
  repeat
    d := op2.Prev.Pt.X + op2.Pt.X;
    Result := Result + d * (op2.Prev.Pt.Y - op2.Pt.Y);
    op2 := op2.Next;
  until op2 = op;
  Result := Result * -0.5;
end;
//------------------------------------------------------------------------------

procedure ReverseOutPts(op: TOutPt);
var
  op1, op2: TOutPt;
begin
  if not Assigned(op) then Exit;
  op1 := op;
  repeat
    op2:= op1.Next;
    op1.Next := op1.Prev;
    op1.Prev := op2;
    op1 := op2;
  until op1 = op;
end;
//------------------------------------------------------------------------------

function RecheckInnerOuter(e: PActive): Boolean;
var
  wasOuter, isOuter: Boolean;
  e2: PActive;
  area: double;
begin
  area := Clipper.Area(e.outrec.Pts);
  result := area <> 0;
  if not result then Exit; //returns false when area == 0

  wasOuter := Clipper.IsOuter(e.OutRec);
  isOuter := true;
  e2 := e.PrevInAEL;
  while assigned(e2) do
  begin
   if IsHotEdge(e2) and not IsOpen(e2) then isOuter := not isOuter;
   e2 := e2.PrevInAEL;
  end;

  if isOuter <> wasOuter then
  begin
    if isOuter then SetAsOuter(e.outrec)
    else SetAsInner(e.outrec);
  end;

  e2 := GetPrevHotEdge(e);
  if isOuter then
  begin
    if assigned(e2) and IsInner(e2.OutRec) then e.OutRec.Owner := e2.OutRec
    else e.OutRec.Owner := nil;
  end else
  begin
    if not assigned(e2) then SetAsOuter(e.OutRec)
    else if IsInner(e2.OutRec) then e.OutRec.Owner := e2.OutRec.Owner
    else e.OutRec.Owner := e2.OutRec;
  end;

  if (area > 0) <> isOuter then ReverseOutPts(e.outrec.Pts);
  UnsetCheckFlag(e.OutRec);
end;
//------------------------------------------------------------------------------

procedure SwapSides(outRec: TOutRec); {$IFDEF INLINING} inline; {$ENDIF}
var
  e2: PActive;
begin
  e2 := outRec.frontE;
  outRec.frontE := outRec.backE;
  outRec.backE := e2;
  outRec.Pts := outRec.Pts.Next;
end;
//------------------------------------------------------------------------------

function FixSides(e: PActive): Boolean;
begin
  Result := not RecheckInnerOuter(e) or (IsOuter(e.OutRec) <> IsFront(e));
  if Result then SwapSides(e.OutRec);
end;
//------------------------------------------------------------------------------

procedure SetOwnerAndInnerOuterState(e: PActive);
var
  e2: PActive;
  outRec: TOutRec;
begin
  outRec := e.OutRec;
  if IsOpen(e) then
  begin
    outRec.Owner := nil;
    outRec.State := osOpen;
    Exit;
  end;
  //set owner ...
  if IsHeadingLeftHorz(e) then
  begin
    e2 := e.NextInAEL; //ie assess state from opposite direction
    while assigned(e2) and (not IsHotEdge(e2) or IsOpen(e2)) do
      e2 := e2.NextInAEL;
    if not assigned(e2) then outRec.Owner := nil
    else if IsOuter(e2.OutRec) = (e2.OutRec.frontE = e2) then
      outRec.Owner := e2.OutRec.Owner
    else
      outRec.Owner := e2.OutRec;
  end else
  begin
    e2 := GetPrevHotEdge(e);
    if not assigned(e2) then
      outRec.Owner := nil
    else if IsOuter(e2.OutRec) = (e2.OutRec.backE = e2) then
      outRec.Owner := e2.OutRec.Owner
    else
      outRec.Owner := e2.OutRec;
  end;

  //set inner/outer ...
  if not assigned(outRec.Owner) or IsInner(outRec.Owner) then
    outRec.State := osOuter else
    outRec.State := osInner;

end;
//------------------------------------------------------------------------------

function EdgesAdjacentInAEL(node: PIntersectNode): Boolean;
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  with node^ do
    Result := (Edge1.NextInAEL = Edge2) or (Edge1.PrevInAEL = Edge2);
end;
//------------------------------------------------------------------------------

function IntersectListSort(node1, node2: Pointer): Integer;
begin
  result := PIntersectNode(node2).Pt.Y - PIntersectNode(node1).Pt.Y;
  if (result = 0) and (node1 <> node2) then
    result := PIntersectNode(node1).Pt.X - PIntersectNode(node2).Pt.X;
end;

//------------------------------------------------------------------------------
// TClipper methods ...
//------------------------------------------------------------------------------

constructor TClipper.Create;
begin
{$IFDEF XPLAT_GENERICS}
  FLocMinList := TList<PLocalMinima>.Create;
  FOutRecList := TList<TOutRec>.Create;
  FIntersectList := TList<PIntersectNode>.Create;
  FVertexList := TList<PVertex>.Create;
{$ELSE}
  FLocMinList := TList.Create;
  FOutRecList := TList.Create;
  FIntersectList := TList.Create;
  FVertexList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TClipper.Destroy;
begin
  Clear;
  FLocMinList.Free;
  FOutRecList.Free;
  FIntersectList.Free;
  FVertexList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipper.CleanUp;
var
  dummy: Int64;
begin
  try
    //in case of exceptions ...
    while assigned(FActives) do DeleteFromAEL(FActives);
    while assigned(FScanLine) do PopScanLine(dummy);
    DisposeIntersectNodes;

    DisposeScanLineList;
    DisposeAllOutRecs;
  except
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.Clear;
begin
  CleanUp;
  DisposeVerticesAndLocalMinima;
  FCurrentLocMinIdx := 0;
  FLocMinListSorted := false;
  FHasOpenPaths := False;
end;
//------------------------------------------------------------------------------

procedure TClipper.Reset;
var
  i: Integer;
begin
  if not FLocMinListSorted then
  begin

{$IFDEF XPLAT_GENERICS}
    FLocMinList.Sort(TComparer<PLocalMinima>.Construct(
    function (const item1, item2: PLocalMinima): integer
    var
      dy: Int64;
    begin
      dy := item2.vertex.Pt.Y - item1.vertex.Pt.Y;
      if dy < 0 then Result := -1
      else if dy > 0 then Result := 1
      else Result := 0;
    end));
{$ELSE}
    FLocMinList.Sort(LocMinListSort);
{$ENDIF}
    FLocMinListSorted := true;
  end;

  for i := FLocMinList.Count -1 downto 0 do
    InsertScanLine(PLocalMinima(FLocMinList[i]).vertex.Pt.Y);
  FCurrentLocMinIdx := 0;
  FActives := nil;
  FSel := nil;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertScanLine(const Y: Int64);
var
  newSl, sl: PScanLine;
begin
  //The scanline list is a single-linked list of all the Y coordinates of
  //subject and clip vertices in the clipping operation (sorted descending).
  //However, only scanline Y's at Local Minima are inserted before clipping
  //starts. While scanlines are removed sequentially during the sweep operation,
  //new scanlines are only inserted whenever edge bounds are updated. This keeps
  //the scanline list relatively short, optimising performance.
  if not Assigned(FScanLine) then
  begin
    new(newSl);
    newSl.Y := Y;
    FScanLine := newSl;
    newSl.Next := nil;
  end else if Y > FScanLine.Y then
  begin
    new(newSl);
    newSl.Y := Y;
    newSl.Next := FScanLine;
    FScanLine := newSl;
  end else
  begin
    sl := FScanLine;
    while Assigned(sl.Next) and (Y <= sl.Next.Y) do
      sl := sl.Next;
    if Y = sl.Y then Exit; //skip duplicates
    new(newSl);
    newSl.Y := Y;
    newSl.Next := sl.Next;
    sl.Next := newSl;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.PopScanLine(out Y: Int64): Boolean;
var
  sl: PScanLine;
begin
  Result := assigned(FScanLine);
  if not Result then Exit;
  Y := FScanLine.Y;
  sl := FScanLine;
  FScanLine := FScanLine.Next;
  dispose(sl);
end;
//------------------------------------------------------------------------------

function TClipper.PopLocalMinima(Y: Int64;
  out localMinima: PLocalMinima): Boolean;
begin
  Result := false;
  if FCurrentLocMinIdx = FLocMinList.Count then Exit;
  localMinima := PLocalMinima(FLocMinList[FCurrentLocMinIdx]);
  if (localMinima.vertex.Pt.Y = Y) then
  begin
    inc(FCurrentLocMinIdx);
    Result := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeScanLineList;
var
  sl: PScanLine;
begin
  while Assigned(FScanLine) do
  begin
    sl := FScanLine.Next;
    Dispose(FScanLine);
    FScanLine := sl;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeOutRec(index: Integer);
var
  outRec: TOutRec;
begin
  outRec := FOutRecList[index];
  if Assigned(outRec.Pts) then DisposePolyPts(outRec.Pts);
  outRec.Free;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeAllOutRecs;
var
  i: Integer;
begin
  for i := 0 to FOutRecList.Count -1 do DisposeOutRec(i);
  FOutRecList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeVerticesAndLocalMinima;
var
  i: Integer;
begin
  for i := 0 to FLocMinList.Count -1 do
    Dispose(PLocalMinima(FLocMinList[i]));
  FLocMinList.Clear;
  for i := 0 to FVertexList.Count -1 do
    FreeMem(FVertexList[i]);
  FVertexList.Clear;
end;
//------------------------------------------------------------------------------

procedure SetVertex(v, prevV: PVertex; const point: TPoint64);
  {$IFDEF INLINING} inline; {$ENDIF}
begin
  v.pt := point;
  if assigned(prevV) then
    prevV.next := v;
  v.next := nil;
  v.prev := prevV;
  v.flags := [];
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPathToVertexList(const p: TPath;
  polyType: TPathType; isOpen: Boolean);
var
  i,j, highI: integer;
  va: PVertexArray;
  ascending, ascending0: Boolean;

  procedure AddLocMin(vert: PVertex);
  var
    lm: PLocalMinima;
  begin
    Include(vert.flags, vfLocMin);
    new(lm);
    lm.vertex := vert;
    lm.PolyType := polyType;
    lm.IsOpen := isOpen;
    FLocMinList.Add(lm);              //nb: sorted in Reset()
  end;
  //---------------------------------------------------------

begin
  highI := high(p);
  while (highI > 0) and PointsEqual(p[highI], p[0]) do dec(highI);

  GetMem(va, sizeof(TVertex) * (highI +1));
  fVertexList.Add(@va[0]);

  SetVertex(@va[0], nil, p[0]);
  if p[0].Y = p[highI].Y then
  begin
    //since path[0] and path[highI] are horizontal
    //find the first prior non-horizontal pt
    i := (highI -1);
    while (i > 0) and (p[i].Y = p[highI].Y) do dec(i);
    if (i = 0) and not isOpen then
      Exit; //path is entirely horizontal
    //get the initial winding direction
    ascending := p[0].Y < p[i].Y;
  end else
    ascending := p[0].Y < p[highI].Y;
  ascending0 := ascending; //save the initial winding direction

  if isOpen then
  begin
    if ascending then
    begin
      va[0].flags := [vfOpenStart];
      AddLocMin(@va[0]);
    end else
      va[0].flags := [vfOpenStart, vfLocMax];
  end
  else if highI < 2 then
    Exit;

  j := 0;
  for i := 1 to highI do
  begin
    if PointsEqual(p[i], va[j].pt) then Continue; //skip duplicate
    inc(j);
    SetVertex(@va[j], @va[j-1], p[i]);

    if ascending and (va[j].pt.Y > va[j-1].pt.Y) then
    begin
      Include(va[j-1].flags, vfLocMax);
      ascending := false;
    end
    else if not ascending and (va[j].pt.Y < va[j-1].pt.Y) then
    begin
      AddLocMin(@va[j-1]);
      ascending := true;
    end;
  end;

  if ascending0 <> ascending then
  begin
    if ascending0 then
    begin
      AddLocMin(@va[j]);
    end else
      Include(va[j].flags, vfLocMax);
  end;

  va[j].next := @va[0];
  va[0].prev := @va[j];

  if isOpen then
    Include(va[j].flags, vfOpenEnd);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPath(const path64: TPath; PolyType: TPathType;
  isOpen: Boolean);
begin
  if Length(path64) < 2 then Exit;
  if isOpen then
  begin
    if (PolyType = ptClip) then RaiseError(rsClipper_OpenPathErr);
    FHasOpenPaths := true;
  end;
  FLocMinListSorted := false;
  AddPathToVertexList(path64, polyType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipper.AddPaths(const paths64: TPaths; polyType: TPathType;
  isOpen: Boolean);
var
  i: Integer;
begin
  for i := 0 to high(paths64) do AddPath(paths64[i], polyType, isOpen);
end;
//------------------------------------------------------------------------------

function TClipper.IsContributingClosed(e: PActive): Boolean;
begin
  Result := false;
  case FFillRule of
    frNonZero: if abs(e.WindCnt) <> 1 then Exit;
    frPositive: if (e.WindCnt <> 1) then Exit;
    frNegative: if (e.WindCnt <> -1) then Exit;
  end;

  case FClipType of
    ctIntersection:
      case FFillRule of
        frEvenOdd, frNonZero: Result := (e.WindCnt2 <> 0);
        frPositive: Result := (e.WindCnt2 > 0);
        frNegative: Result := (e.WindCnt2 < 0);
      end;
    ctUnion:
      case FFillRule of
        frEvenOdd, frNonZero: Result := (e.WindCnt2 = 0);
        frPositive: Result := (e.WindCnt2 <= 0);
        frNegative: Result := (e.WindCnt2 >= 0);
      end;
    ctDifference:
      if GetPolyType(e) = ptSubject then
        case FFillRule of
          frEvenOdd, frNonZero: Result := (e.WindCnt2 = 0);
          frPositive: Result := (e.WindCnt2 <= 0);
          frNegative: Result := (e.WindCnt2 >= 0);
        end
      else
        case FFillRule of
          frEvenOdd, frNonZero: Result := (e.WindCnt2 <> 0);
          frPositive: Result := (e.WindCnt2 > 0);
          frNegative: Result := (e.WindCnt2 < 0);
        end;
    ctXor:
        Result := true;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.IsContributingOpen(e: PActive): Boolean;
begin
    case FClipType of
      ctIntersection:
        Result := (e.WindCnt2 <> 0);
      ctXor:
        Result := (e.WindCnt <> 0) <> (e.WindCnt2 <> 0);
      ctDifference:
        Result := (e.WindCnt2 = 0);
      else //ctUnion:
        Result := (e.WindCnt = 0) and (e.WindCnt2 = 0);
    end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindCountForClosedPathEdge(e: PActive);
var
  e2: PActive;
begin
  //Wind counts refer to polygon regions not edges, so here an edge's WindCnt
  //indicates the higher of the wind counts for the two regions touching the
  //edge. (nb: Adjacent regions can only ever have their wind counts differ by
  //one. Also, open paths have no meaningful wind directions or counts.)

  e2 := e.PrevInAEL;
  //find the nearest closed path edge of the same PolyType in AEL (heading left)
  while Assigned(e2) and (not IsSamePolyType(e2, e) or IsOpen(e2)) do
    e2 := e2.PrevInAEL;

  if not Assigned(e2) then
  begin
    e.WindCnt := e.WindDx;
    e2 := FActives;
  end
  else if (FFillRule = frEvenOdd) then
  begin
    e.WindCnt := e.WindDx;
    e.WindCnt2 := e2.WindCnt2;
    e2 := e2.NextInAEL;
  end else
  begin
    //NonZero, positive, or negative filling here ...
    //if e's WindCnt is in the SAME direction as its WindDx, then polygon
    //filling will be on the right of 'e'.
    //nb: neither e2.WindCnt nor e2.WindDx should ever be 0.
    if (e2.WindCnt * e2.WindDx < 0) then
    begin
      //opposite directions so 'e' is outside 'e2' ...
      if (Abs(e2.WindCnt) > 1) then
      begin
        //outside prev poly but still inside another.
        if (e2.WindDx * e.WindDx < 0) then
          //reversing direction so use the same WC
          e.WindCnt := e2.WindCnt else
          //otherwise keep 'reducing' the WC by 1 (ie towards 0) ...
          e.WindCnt := e2.WindCnt + e.WindDx;
      end
      //now outside all polys of same polytype so set own WC ...
      else e.WindCnt := e.WindDx;
    end else
    begin
      //'e' must be inside 'e2'
      if (e2.WindDx * e.WindDx < 0) then
        //reversing direction so use the same WC
        e.WindCnt := e2.WindCnt
      else
        //otherwise keep 'increasing' the WC by 1 (ie away from 0) ...
        e.WindCnt := e2.WindCnt + e.WindDx;
    end;
    e.WindCnt2 := e2.WindCnt2;
    e2 := e2.NextInAEL;
  end;

  //update WindCnt2 ...
  if FFillRule = frEvenOdd then
    while (e2 <> e) do
    begin
      if IsSamePolyType(e2, e) or IsOpen(e2) then //do nothing
      else if e.WindCnt2 = 0 then e.WindCnt2 := 1
      else e.WindCnt2 := 0;
      e2 := e2.NextInAEL;
    end
  else
    while (e2 <> e) do
    begin
      if not IsSamePolyType(e2, e) and not IsOpen(e2) then
        Inc(e.WindCnt2, e2.WindDx);
      e2 := e2.NextInAEL;
    end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SetWindCountForOpenPathEdge(e: PActive);
var
  e2: PActive;
  cnt1, cnt2: Integer;
begin
  e2 := FActives;
  if FFillRule = frEvenOdd then
  begin
    cnt1 := 0;
    cnt2 := 0;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(cnt2)
      else if not IsOpen(e2) then inc(cnt1);
      e2 := e2.NextInAEL;
    end;
    if Odd(cnt1) then e.WindCnt := 1 else e.WindCnt := 0;
    if Odd(cnt2) then e.WindCnt2 := 1 else e.WindCnt2 := 0;
  end else
  begin
    //if FClipType in [ctUnion, ctDifference] then e.WindCnt := e.WindDx;
    while (e2 <> e) do
    begin
      if (GetPolyType(e2) = ptClip) then inc(e.WindCnt2, e2.WindDx)
      else if not IsOpen(e2) then inc(e.WindCnt, e2.WindDx);
      e2 := e2.NextInAEL;
    end;
  end;
end;
//------------------------------------------------------------------------------

function IsValidAelOrder(a1, a2: PActive): Boolean;
var
  pt1, pt2: TPoint64;
  op1, op2: PVertex;
  X: Int64;
begin
  if a2.CurrX <> a1.CurrX then
  begin
    Result := a2.CurrX > a1.CurrX;
    Exit;
  end;

  pt1 := a1.Bot; pt2 := a2.Bot;
  op1 := a1.VertTop; op2 := a2.VertTop;
  while true do
  begin
    if op1.Pt.Y >= op2.Pt.Y then
    begin
      X := TopX(pt2, op2.Pt, op1.Pt.Y) - op1.Pt.X;
      result := X > 0;
      if X <> 0 then Exit;
      if op2.Pt.Y = op1.Pt.Y then
      begin
        pt2 := op2.Pt;
        op2 := NextVertex(op2, IsLeftBound(a2));
      end;
      pt1 := op1.Pt;
      op1 := NextVertex(op1, IsLeftBound(a1));
    end else
    begin
      X := op2.Pt.X - TopX(pt1, op1.Pt, op2.Pt.Y);
      result := X > 0;
      if X <> 0 then Exit;
      pt2 := op2.Pt;
      op2 := NextVertex(op2, IsLeftBound(a2));
    end;

    if (op1.Pt.Y > pt1.Y) then
    begin
      result := (a1.WindDx > 0) <> IsClockwise(PrevVertex(op1, a1.WindDx > 0));
      Exit;
    end else if (op2.Pt.Y > pt2.Y) then
    begin
      result := (a2.WindDx > 0) = IsClockwise(PrevVertex(op2, a2.WindDx > 0));
      Exit;
    end;
  end;
  result := true;
end;
//------------------------------------------------------------------------------

procedure TClipper.InsertLeftEdge(e: PActive);
var
  e2: PActive;
begin
  if not Assigned(FActives) then
  begin
    e.PrevInAEL := nil;
    e.NextInAEL := nil;
    FActives := e;
  end
  else if IsValidAelOrder(e, FActives) then
  begin
    e.PrevInAEL := nil;
    e.NextInAEL := FActives;
    FActives.PrevInAEL := e;
    FActives := e;
  end else
  begin
    e2 := FActives;
    while Assigned(e2.NextInAEL) and IsValidAelOrder(e2.NextInAEL, e) do
      e2 := e2.NextInAEL;
    e.NextInAEL := e2.NextInAEL;
    if Assigned(e2.NextInAEL) then e2.NextInAEL.PrevInAEL := e;
    e.PrevInAEL := e2;
    e2.NextInAEL := e;
  end;
end;
//----------------------------------------------------------------------

procedure InsertRightEdge(e, e2: PActive);
begin
  e2.NextInAEL := e.NextInAEL;
  if Assigned(e.NextInAEL) then e.NextInAEL.PrevInAEL := e2;
  e2.PrevInAEL := e;
  e.NextInAEL := e2;
end;
//----------------------------------------------------------------------

procedure TClipper.InsertLocalMinimaIntoAEL(const botY: Int64);
var
  leftB, rightB: PActive;
  locMin: PLocalMinima;
  contributing: Boolean;
begin
  //Add local minima (if any) at BotY ...
  //nb: horizontal local minima edges should contain locMin.vertex.prev

  while PopLocalMinima(botY, locMin) do
  begin
    if (vfOpenStart in locMin.vertex.flags) then
    begin
      leftB := nil;
    end else
    begin
      new(leftB);
      FillChar(leftB^, sizeof(TActive), 0);
      leftB.LocMin := locMin;
      leftB.OutRec := nil;
      leftB.Bot := locMin.vertex.Pt;
      leftB.vertTop := locMin.vertex.prev; //ie descending
      leftB.Top := leftB.vertTop.Pt;
      leftB.CurrX := leftB.Bot.X;
      leftB.WindDx := -1;
      SetDx(leftB);
    end;

    if (vfOpenEnd in locMin.vertex.flags) then
    begin
      rightB := nil;
    end else
    begin
      new(rightB);
      FillChar(rightB^, sizeof(TActive), 0);
      rightB.LocMin := locMin;
      rightB.OutRec := nil;
      rightB.Bot := locMin.vertex.Pt;
      rightB.vertTop := locMin.vertex.next; //ie ascending
      rightB.Top := rightB.vertTop.Pt;
      rightB.CurrX := rightB.Bot.X;
      rightB.WindDx := 1;
      SetDx(rightB);
    end;
    //Currently LeftB is just the descending bound and RightB is the ascending.
    //Now if the LeftB isn't on the left of RightB then we need swap them.
    if assigned(leftB) and assigned(rightB) then
    begin
      if IsHorizontal(leftB) then
      begin
        if IsHeadingRightHorz(leftB) then SwapActives(leftB, rightB);
      end
      else if IsHorizontal(rightB) then
      begin
        if IsHeadingLeftHorz(rightB) then SwapActives(leftB, rightB);
      end
      else if (leftB.Dx < rightB.Dx) then SwapActives(leftB, rightB);
    end
    else if not assigned(leftB) then
    begin
      leftB := rightB;
      rightB := nil;
    end;

    InsertLeftEdge(leftB);                   ///////
    //todo: further validation of position in AEL ???

    if IsOpen(leftB) then
    begin
      SetWindCountForOpenPathEdge(leftB);
      contributing := IsContributingOpen(leftB);
    end else
    begin
      SetWindCountForClosedPathEdge(leftB);
      contributing := IsContributingClosed(leftB);
    end;

    if assigned(rightB) then
    begin
      rightB.WindCnt := leftB.WindCnt;
      rightB.WindCnt2 := leftB.WindCnt2;
      InsertRightEdge(leftB, rightB);        ///////
      if contributing then
        AddLocalMinPoly(leftB, rightB, leftB.Bot, true);

      if IsHorizontal(rightB) then
        PushHorz(rightB) else
        InsertScanLine(rightB.Top.Y);
    end
    else if contributing then
      StartOpenPath(leftB, leftB.Bot);

    if IsHorizontal(leftB) then
      PushHorz(leftB) else
      InsertScanLine(leftB.Top.Y);
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.PushHorz(e: PActive);
begin
  if assigned(FSel) then
    e.NextInSEL := FSel else
    e.NextInSEL := nil;
  FSel := e;
end;
//------------------------------------------------------------------------------

function TClipper.PopHorz(out e: PActive): Boolean;
begin
  Result := assigned(FSel);
  if not Result then Exit;
  e := FSel;
  FSel := FSel.NextInSEL;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMinPoly(e1, e2: PActive; const pt: TPoint64;
  IsNew: Boolean = false; orientationCheckRequired: Boolean = false);
var
  outRec: TOutRec;
  op: TOutPt;
begin
  outRec := TOutRec.Create;
  outRec.Idx := FOutRecList.Add(outRec);
  outRec.Pts := nil;
  outRec.PolyPath := nil;

  e1.OutRec := outRec;
  SetOwnerAndInnerOuterState(e1);
  //flag when orientatation needs to be rechecked later ...
  if orientationCheckRequired then SetCheckFlag(outRec);

  e2.OutRec := outRec;
  if not IsOpen(e1) then
  begin
    //Setting the owner and inner/outer states (above) is an essential
    //precursor to setting edge 'sides' (ie left and right sides of output
    //polygons) and hence the orientation of output paths ...
    if IsOuter(outRec) = IsNew then
      SetSides(outRec, e1, e2) else
      SetSides(outRec, e2, e1);
  end;

  op := TOutPt.Create;
  outRec.Pts := op;
  op.Pt := pt;
  op.Prev := op;
  op.Next := op;

  //nb: currently e1.NextInAEL == e2 but this could change immediately on return
end;
//------------------------------------------------------------------------------

procedure TClipper.AddLocalMaxPoly(e1, e2: PActive; const pt: TPoint64);
var
  op: TOutPt;
begin
  if not IsOpen(e1) and (IsFront(e1) = IsFront(e2)) then
    if not FixSides(e1) then FixSides(e2);

  op := AddOutPt(e1, pt);
  //AddOutPt(e2, pt); //this may no longer be necessary

  if  (e1.OutRec = e2.OutRec) then
  begin
    if e1.OutRec.State in [osOuterCheck, osInnerCheck] then
      RecheckInnerOuter(e1);

    //assert(IsFront(e1) <> IsOuter(e1.OutRec), 'oops!');

    //nb: IsClockwise() is generally faster than Area() but will occasionally
    //give false positives when there are tiny self-intersections at the top...
    if IsOuter(e1.OutRec) then
    begin
      if not IsClockwise(op) and (Area(op) < 0) then
        ReverseOutPts(e1.OutRec.Pts);
    end else
    begin
      if IsClockwise(op) and (Area(op) > 0) then
        ReverseOutPts(e1.OutRec.Pts);
    end;

    e1.outRec.frontE := nil;
    e1.outRec.backE := nil;
    e1.OutRec := nil;
    e2.OutRec := nil;
  end
  //and to preserve the winding orientation of Outrec ...
  else if e1.OutRec.Idx < e2.OutRec.Idx then
    JoinOutrecPaths(e1, e2) else
    JoinOutrecPaths(e2, e1);

end;
//------------------------------------------------------------------------------

procedure TClipper.JoinOutrecPaths(e1, e2: PActive);
var
  p1_start, p1_end, p2_start, p2_end: TOutPt;
begin
  if (IsFront(e1) = IsFront(e2)) then
  begin
    //one or other 'side' must be wrong ...
    if IsOpen(e1) then SwapSides(e2.OutRec)
    else if not FixSides(e1) and not FixSides(e2) then
      RaiseError(rsClipper_ClippingErr);
    if e1.OutRec.Owner = e2.OutRec then
      e1.OutRec.Owner := e2.OutRec.Owner;
  end;

  //join e2 outrec path onto e1 outrec path and then delete e2 outrec path
  //pointers. (see joining_outpt.svg)
  p1_start :=  e1.OutRec.Pts;
  p2_start :=  e2.OutRec.Pts;
  p1_end := p1_start.Next;
  p2_end := p2_start.Next;

  if IsFront(e1) then
  begin
    p2_end.Prev := p1_start;
    p1_start.Next := p2_end;
    p2_start.Next := p1_end;
    p1_end.Prev := p2_start;
    e1.OutRec.Pts := p2_start;
    e1.OutRec.frontE := e2.OutRec.frontE;
    if not IsOpen(e1) then e1.OutRec.frontE.OutRec := e1.OutRec;
    //strip duplicates ...
    if (p2_end <> p2_start) and PointsEqual(p2_end.Pt, p2_end.Prev.Pt) then
      DisposeOutPt(p2_end);
  end else
  begin
    p1_end.Prev := p2_start;
    p2_start.Next := p1_end;
    p1_start.Next := p2_end;
    p2_end.Prev := p1_start;
    e1.OutRec.backE := e2.OutRec.backE;
    if not IsOpen(e1) then e1.OutRec.backE.OutRec := e1.OutRec;
    //strip duplicates ...
    if (p1_end <> p1_start) and PointsEqual(p1_end.Pt, p1_end.Prev.Pt) then
      DisposeOutPt(p1_end);
  end;

  if PointsEqual(e1.OutRec.Pts.Pt, e1.OutRec.Pts.Prev.Pt) and
    not IsInvalidPath(e1.OutRec.Pts) then
      DisposeOutPt(e1.OutRec.Pts.Prev);

  //after joining, the e2.OutRec must contains no vertices ...
  e2.OutRec.frontE := nil;
  e2.OutRec.backE := nil;
  e2.OutRec.Pts := nil;
  e2.OutRec.Owner := e1.OutRec; //this may be redundant

  //and e1 and e2 are maxima and are about to be dropped from the Actives list.
  e1.OutRec := nil;
  e2.OutRec := nil;
end;
//------------------------------------------------------------------------------

function TClipper.AddOutPt(e: PActive; const pt: TPoint64): TOutPt;
var
  opFront, opBack: TOutPt;
  toFront: Boolean;
  outrec: TOutRec;
begin
  //Outrec.OutPts: a circular doubly-linked-list of POutPt where ...
  //opFront[.Prev]* ~~~> opBack & opBack == opFront.Next
  outrec := e.OutRec;
  toFront := IsFront(e);
  opFront := outrec.Pts;
  opBack := opFront.Next;
  if toFront and PointsEqual(pt, opFront.Pt) then
    result := opFront
  else if not toFront and PointsEqual(pt, opBack.Pt) then
    result := opBack
  else
  begin
    Result := TOutPt.Create;
    Result.Pt := pt;
    opBack.Prev := Result;
    Result.Prev := opFront;
    Result.Next := opBack;
    opFront.Next := Result;
    if toFront then outrec.Pts := Result;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.StartOpenPath(e: PActive; const pt: TPoint64);
var
  outRec: TOutRec;
  op: TOutPt;
begin
  outRec := TOutRec.Create;
  outRec.Idx := FOutRecList.Add(outRec);
  outRec.Owner := nil;
  outRec.State := osOpen;
  outRec.Pts := nil;
  outRec.PolyPath := nil;
  outRec.frontE := nil;
  outRec.backE := nil;
  e.OutRec := outRec;

  op := TOutPt.Create;
  outRec.Pts := op;
  op.Pt := pt;
  op.Prev := op;
  op.Next := op;
end;
//------------------------------------------------------------------------------

procedure TClipper.UpdateEdgeIntoAEL(var e: PActive);
begin
  e.Bot := e.Top;
  e.vertTop := NextVertex(e);
  e.Top := e.vertTop.Pt;
  e.CurrX := e.Bot.X;
  SetDx(e);
  if not IsHorizontal(e) then InsertScanLine(e.Top.Y);
end;
//------------------------------------------------------------------------------

procedure TClipper.IntersectEdges(e1, e2: PActive;
  const pt: TPoint64; orientationCheckRequired: Boolean = false);
var
  e1WindCnt, e2WindCnt, e1WindCnt2, e2WindCnt2: Integer;
begin

  //MANAGE OPEN PATH INTERSECTIONS SEPARATELY ...
  if FHasOpenPaths and (IsOpen(e1) or IsOpen(e2)) then
  begin
    if (IsOpen(e1) and IsOpen(e2) ) then Exit;
    //the following line avoids duplicating a whole lot of code ...
    if IsOpen(e2) then SwapActives(e1, e2);
    case FClipType of
      ctIntersection, ctDifference:
        if IsSamePolyType(e1, e2) or (abs(e2.WindCnt) <> 1) then Exit;
      ctUnion:
        if IsHotEdge(e1) <> ((abs(e2.WindCnt) <> 1) or
          (IsHotEdge(e1) <> (e2.WindCnt2 <> 0))) then Exit; //just works!
      ctXor:
        if (abs(e2.WindCnt) <> 1) then Exit;
    end;
    //toggle contribution ...
    if IsHotEdge(e1) then
    begin
      AddOutPt(e1, pt);
      TerminateHotOpen(e1);
    end
    else StartOpenPath(e1, pt);
    Exit;
  end;

  //UPDATE WINDING COUNTS...

  if IsSamePolyType(e1, e2) then
  begin
    if FFillRule = frEvenOdd then
    begin
      e1WindCnt := e1.WindCnt;
      e1.WindCnt := e2.WindCnt;
      e2.WindCnt := e1WindCnt;
    end else
    begin
      if e1.WindCnt + e2.WindDx = 0 then
        e1.WindCnt := -e1.WindCnt else
        Inc(e1.WindCnt, e2.WindDx);
      if e2.WindCnt - e1.WindDx = 0 then
        e2.WindCnt := -e2.WindCnt else
        Dec(e2.WindCnt, e1.WindDx);
    end;
  end else
  begin
    if FFillRule <> frEvenOdd then Inc(e1.WindCnt2, e2.WindDx)
    else if e1.WindCnt2 = 0 then e1.WindCnt2 := 1
    else e1.WindCnt2 := 0;

    if FFillRule <> frEvenOdd then Dec(e2.WindCnt2, e1.WindDx)
    else if e2.WindCnt2 = 0 then e2.WindCnt2 := 1
    else e2.WindCnt2 := 0;
  end;

  case FFillRule of
    frPositive:
      begin
        e1WindCnt := e1.WindCnt;
        e2WindCnt := e2.WindCnt;
      end;
    frNegative:
      begin
        e1WindCnt := -e1.WindCnt;
        e2WindCnt := -e2.WindCnt;
      end;
    else
      begin
        e1WindCnt := abs(e1.WindCnt);
        e2WindCnt := abs(e2.WindCnt);
      end;
  end;

  if (not IsHotEdge(e1) and not (e1WindCnt in [0,1])) or
    (not IsHotEdge(e2) and not (e2WindCnt in [0,1])) then Exit;

  //NOW PROCESS THE INTERSECTION ...

  //if both edges are 'hot' ...
  if IsHotEdge(e1) and IsHotEdge(e2) then
  begin
    if not (e1WindCnt in [0,1]) or not (e2WindCnt in [0,1]) or
      (not IsSamePolyType(e1, e2) and (fClipType <> ctXor)) then
    begin
      AddLocalMaxPoly(e1, e2, pt);
    end else if IsFront(e1) or (e1.OutRec = e2.OutRec) then
    begin
      AddLocalMaxPoly(e1, e2, pt);
      AddLocalMinPoly(e1, e2, pt);
    end else
    begin
      //right & left bounds touching, NOT maxima & minima ...
      AddOutPt(e1, pt);
      AddOutPt(e2, pt);
      SwapOutRecs(e1, e2);
    end;
  end

  //if one or other edge is 'hot' ...
  else if IsHotEdge(e1) then
  begin
    AddOutPt(e1, pt);
    SwapOutRecs(e1, e2);
  end
  else if IsHotEdge(e2) then
  begin
    AddOutPt(e2, pt);
    SwapOutRecs(e1, e2);
  end

  else //neither edge is 'hot'
  begin
    case FFillRule of
      frPositive:
      begin
        e1WindCnt2 := e1.WindCnt2;
        e2WindCnt2 := e2.WindCnt2;
      end;
      frNegative:
      begin
        e1WindCnt2 := -e1.WindCnt2;
        e2WindCnt2 := -e2.WindCnt2;
      end
      else
      begin
        e1WindCnt2 := abs(e1.WindCnt2);
        e2WindCnt2 := abs(e2.WindCnt2);
      end;
    end;

    if not IsSamePolyType(e1, e2) then
    begin
      AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
    end
    else if (e1WindCnt = 1) and (e2WindCnt = 1) then
      case FClipType of
        ctIntersection:
          if (e1WindCnt2 > 0) and (e2WindCnt2 > 0) then
            AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
        ctUnion:
          if (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0) then
            AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
        ctDifference:
          if ((GetPolyType(e1) = ptClip) and
                (e1WindCnt2 > 0) and (e2WindCnt2 > 0)) or
              ((GetPolyType(e1) = ptSubject) and
                (e1WindCnt2 <= 0) and (e2WindCnt2 <= 0)) then
                  AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
        ctXor:
          AddLocalMinPoly(e1, e2, pt, false, orientationCheckRequired);
      end
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DeleteFromAEL(e: PActive);
var
  aelPrev, aelNext: PActive;
begin
  aelPrev := e.PrevInAEL;
  aelNext := e.NextInAEL;
  if not Assigned(aelPrev) and not Assigned(aelNext) and
    (e <> FActives) then Exit; //already deleted
  if Assigned(aelPrev) then aelPrev.NextInAEL := aelNext
  else FActives := aelNext;
  if Assigned(aelNext) then aelNext.PrevInAEL := aelPrev;
  Dispose(e);
end;
//------------------------------------------------------------------------------

procedure TClipper.AdjustCurrXAndCopyToSEL(topY: Int64);
var
  e: PActive;
begin
  FSel := FActives;
  e := FActives;
  while Assigned(e) do
  begin
    e.PrevInSEL := e.PrevInAEL;
    e.NextInSEL := e.NextInAEL;
    e.CurrX := TopX(e, topY);
    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.ExecuteInternal(clipType: TClipType;
  fillRule: TFillRule);
var
  Y: Int64;
  e: PActive;
begin
  if clipType = ctNone then Exit;
  FFillRule := fillRule;
  FClipType := clipType;
  Reset;
  if not PopScanLine(Y) then Exit;
  while true do
  begin
    InsertLocalMinimaIntoAEL(Y);
    while PopHorz(e) do DoHorizontal(e);
    FBotY := Y;                       //FBotY == bottom of scanbeam
    if not PopScanLine(Y) then Break; //Y new top of scanbeam
    DoIntersections(Y);
    DoTopOfScanbeam(Y);
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType;
  fillRule: TFillRule; out closedPaths: TPaths): Boolean;
var
  dummy: TPaths;
begin
  Result := true;
  closedPaths := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildResult(closedPaths, dummy);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedPaths, openPaths: TPaths): Boolean;
begin
  Result := true;
  closedPaths := nil;
  openPaths := nil;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildResult(closedPaths, openPaths);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.Execute(clipType: TClipType; fillRule: TFillRule;
  var polytree: TPolyTree; out openPaths: TPaths): Boolean;
begin
  if not assigned(polytree) then RaiseError(rsClipper_PolyTreeErr);
  polytree.Clear;
  openPaths := nil;
  Result := true;
  try try
    ExecuteInternal(clipType, fillRule);
    BuildResultTree(polytree, openPaths);
  except
    Result := false;
  end;
  finally
    CleanUp;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DoIntersections(const topY: Int64);
begin
  if BuildIntersectList(topY) then
  try
    ProcessIntersectList;
  finally
    DisposeIntersectNodes;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.DisposeIntersectNodes;
var
  i: Integer;
begin
  for i := 0 to FIntersectList.Count - 1 do
    Dispose(IntersectNode[i]);
  FIntersectList.Clear;
end;
//------------------------------------------------------------------------------

procedure TClipper.AddNewIntersectNode(e1, e2: PActive; topY: Int64);
var
  pt: TPoint64;
  node: PIntersectNode;
begin
  pt := GetIntersectPoint(e1, e2);
  //Rounding errors can occasionally place the calculated intersection
  //point either below or above the scanbeam, so check and correct ...
  if (pt.Y > FBotY) then
  begin
    //E.Curr.Y is still at the bottom of scanbeam here
    pt.Y := FBotY;
    //use the more vertical of the 2 edges to derive pt.X ...
    if (abs(e1.Dx) < abs(e2.Dx)) then
      pt.X := TopX(e1, FBotY) else
      pt.X := TopX(e2, FBotY);
  end
  else if pt.Y < topY then
  begin
    //TopY = top of scanbeam
    pt.Y := topY;
    if e1.Top.Y = topY then
      pt.X := e1.Top.X
    else if e2.Top.Y = topY then
      pt.X := e2.Top.X
    else if (abs(e1.Dx) < abs(e2.Dx)) then
      pt.X := e1.CurrX
    else
      pt.X := e2.CurrX;
  end;

  new(node);
  node.Edge1 := e1;
  node.Edge2 := e2;
  node.Pt := pt;
  FIntersectList.Add(node);
end;
//------------------------------------------------------------------------------

function TClipper.BuildIntersectList(const topY: Int64): Boolean;
var
  i, lCnt, rCnt, jumpSize: integer;
  first, second, base, prevBase, p, n, tmp: PActive;
begin
  result := false;
  if not Assigned(FActives) or not Assigned(FActives.NextInAEL) then Exit;

  //Calculate edge positions at the top of the current scanbeam, and from this
  //we will determine the intersections required to reach these new positions.
  AdjustCurrXAndCopyToSEL(topY);
  //Track every edge intersection between the bottom and top of each scanbeam,
  //using a stable merge sort to ensure edges are adjacent when intersecting.
  //Re merge sorts see https://stackoverflow.com/a/46319131/359538
	jumpSize := 1;
	while (true) do
  begin
    first := FSel;
    prevBase := nil;
		//sort successive larger jump counts of nodes ...
		while assigned(first) do
    begin
			if (jumpSize = 1) then
      begin
        second := first.NextInSEL;
        if not assigned(second) then
        begin
          first.Jump := nil;
          break;
        end;
        first.Jump := second.NextInSEL;
      end else
      begin
			  second := first.Jump;
        if not assigned(second) then
        begin
          first.Jump := nil;
          break;
        end;
        first.Jump := second.Jump;
      end;

      //now sort first and second groups ...
      base := first;
			lCnt := jumpSize; rCnt := jumpSize;
			while (lCnt > 0) and (rCnt > 0) do
			begin
				if (first.CurrX > second.CurrX) then
        begin
          tmp := second.PrevInSEL;

          //create intersect 'node' events for each time 'second' needs to
          //move left, ie intersecting with its prior edge ...
          for i := 1 to lCnt do
          begin
            AddNewIntersectNode(tmp, second, topY);
            tmp := tmp.PrevInSEL;
          end;

          //now move the out of place 'second' to it's new position in SEL ...
          if (first = base) then
          begin
            if assigned(prevBase) then prevBase.Jump := second;
            base := second;
            base.Jump := first.Jump;
            if (first.PrevInSEL = nil) then FSel := second;
          end;
          tmp := second.NextInSEL;

          //first remove 'second' from list ...
          p := second.PrevInSEL;
          n := second.NextInSEL;
          p.NextInSEL := n;
          if Assigned(n) then n.PrevInSEL := p;
          //and then reinsert 'second' into list just before 'first' ...
          p := first.PrevInSEL;
          if assigned(p) then p.NextInSEL := second;
          first.PrevInSEL := second;
          second.PrevInSEL := p;
          second.NextInSEL := first;

          second := tmp;
          if not assigned(second) then break;
          dec(rCnt);
        end else
        begin
          first := first.NextInSEL;
          dec(lCnt);
        end;
      end;
      first := base.Jump;
      prevBase := base;
    end;
    if FSel.Jump = nil then Break
    else jumpSize := jumpSize shl 1;
  end;
  result := FIntersectList.Count > 0;
end;
//------------------------------------------------------------------------------

function TClipper.GetIntersectNode(index: integer): PIntersectNode;
begin
  result := PIntersectNode(FIntersectList[index]);
end;
//------------------------------------------------------------------------------

procedure TClipper.ProcessIntersectList;
var
  i, j, highI: Integer;
  node: PIntersectNode;
begin
  //We now have a list of intersections required so that edges will be
  //correctly positioned at the top of the scanbeam. However, it's important
  //that edge intersections are processed from the bottom up, but it's also
  //crucial that intersections only occur between adjacent edges.

  //First we do a quicksort so intersections proceed in a bottom up order ...
{$IFDEF XPLAT_GENERICS}
  FIntersectList.Sort(TComparer<PIntersectNode>.Construct(
    function (const node1, node2: PIntersectNode): integer
    begin
      result := node2.Pt.Y - node1.Pt.Y;
      if (result = 0) and (node1 <> node2) then
        result := node1.Pt.X - node2.Pt.X;
    end));
{$ELSE}
  FIntersectList.Sort(IntersectListSort);
{$ENDIF}

  //Now as we process these intersections, we must sometimes adjust the order
  //to ensure that intersecting edges are always adjacent ...
  highI := FIntersectList.Count - 1;
  for i := 0 to highI do
  begin
    if not EdgesAdjacentInAEL(FIntersectList[i]) then
    begin
      j := i + 1;
      while not EdgesAdjacentInAEL(FIntersectList[j]) do inc(j);
      //Swap IntersectNodes ...
      node := FIntersectList[i];
      FIntersectList[i] := FIntersectList[j];
      FIntersectList[j] := node;
    end;

    with IntersectNode[i]^ do
    begin
      //Occasionally a non-minima intersection is processed before its own
      //minima. This causes problems with orientation so we need to flag it ...
      if (i < highI) and (IntersectNode[i+1].Pt.Y > Pt.Y) then
          IntersectEdges(Edge1, Edge2, Pt, true) else
          IntersectEdges(Edge1, Edge2, Pt);
      SwapPositionsInAEL(Edge1, Edge2);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TClipper.SwapPositionsInAEL(e1, e2: PActive);
var
  prev, next: PActive;
begin
  //preconditon: e1 must be immediately to the left of e2
  next := e2.NextInAEL;
  if Assigned(next) then next.PrevInAEL := e1;
  prev := e1.PrevInAEL;
  if Assigned(prev) then prev.NextInAEL := e2;
  e2.PrevInAEL := prev;
  e2.NextInAEL := e1;
  e1.PrevInAEL := e2;
  e1.NextInAEL := next;
  if not Assigned(e2.PrevInAEL) then FActives := e2;
end;
//------------------------------------------------------------------------------

procedure TClipper.DoHorizontal(horzEdge: PActive);
var
  e, maxPair: PActive;
  horzLeft, horzRight: Int64;
  isLeftToRight: Boolean;
  pt: TPoint64;
  isMax: Boolean;

  procedure ResetHorzDirection;
  var
    e: PActive;
  begin
    if (horzEdge.Bot.X = horzEdge.Top.X) then
    begin
      //the horizontal edge is going nowhere ...
      horzLeft := horzEdge.CurrX;
      horzRight := horzEdge.CurrX;
      e := horzEdge.NextInAEL;
      while assigned(e) and (e <> maxPair) do
        e := e.NextInAEL;
      isLeftToRight := assigned(e);
    end
    else if horzEdge.CurrX < horzEdge.Top.X then
    begin
      horzLeft := horzEdge.CurrX;
      horzRight := horzEdge.Top.X;
      isLeftToRight := true;
    end else
    begin
      horzLeft := horzEdge.Top.X;
      horzRight := horzEdge.CurrX;
      isLeftToRight := false;
    end;
  end;
  //------------------------------------------------------------------------

begin
(*******************************************************************************
* Notes: Horizontal edges (HEs) at scanline intersections (ie at the top or    *
* bottom of a scanbeam) are processed as if layered. The order in which HEs    *
* are processed doesn't matter. HEs intersect with the bottom vertices of      *
* other HEs [#] and with non-horizontal edges [*]. Once these intersections    *
* are completed, intermediate HEs are 'promoted' to the next edge in their     *
* bounds, and they in turn may be intersected [%] by other HEs.                *
*                                                                              *
* eg: 3 horizontals at a scanline:  /   |                     /          /     *
*              |                   /    |    (HE3) o=========%==========o      *
*              o=======o (HE2)    /     |         /         /                  *
*         o============#=========*======*========#=========o (HE1)             *
*        /             |        /       |       /                              *
*******************************************************************************)

  //with closed paths, simplify consecutive horizontals into a 'single' edge ...
  if not IsOpen(horzEdge) then
  begin
    pt := horzEdge.Bot;
    while not IsMaxima(horzEdge) and
      (NextVertex(horzEdge).Pt.Y = pt.Y) do
        UpdateEdgeIntoAEL(horzEdge);
    horzEdge.Bot := pt;
    horzEdge.CurrX := pt.X;
    //update Dx in case of direction change ...
    if horzEdge.Bot.X < horzEdge.Top.X then
      horzEdge.Dx := NegInfinity else
      horzEdge.Dx := Infinity;
  end;

  maxPair := nil;
  if IsMaxima(horzEdge) and (not IsOpen(horzEdge) or
      ([vfOpenStart, vfOpenEnd] * horzEdge.vertTop.flags = [])) then
        maxPair := GetMaximaPair(horzEdge);

  ResetHorzDirection;
  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, Point64(horzEdge.CurrX, horzEdge.Bot.Y));

  while true do //loops through consec. horizontal edges (if open)
  begin
    isMax := IsMaxima(horzEdge);
    if isLeftToRight  then
      e := horzEdge.NextInAEL else
      e := horzEdge.PrevInAEL;

    while assigned(e) do
    begin
      //Break if we've gone past the end of the horizontal ...
      if (isLeftToRight and (e.CurrX > horzRight)) or
        (not isLeftToRight and (e.CurrX < horzLeft)) then Break;
      //or if we've got to the end of an intermediate horizontal edge ...
      if (E.CurrX = horzEdge.Top.X) and not isMax and not IsHorizontal(e) then
      begin
        pt := NextVertex(horzEdge).Pt;
        if(isLeftToRight and (TopX(E, pt.Y) >= pt.X)) or
          (not isLeftToRight and (TopX(E, pt.Y) <= pt.X)) then Break;
      end;

      if (e = maxPair) then
      begin
        if IsHotEdge(horzEdge)  then
        begin
          if isLeftToRight then
            AddLocalMaxPoly(horzEdge, e, horzEdge.Top) else
            AddLocalMaxPoly(e, horzEdge, horzEdge.Top);
        end;
        DeleteFromAEL(e);
        DeleteFromAEL(horzEdge);
        Exit;
      end;

      pt := Point64(e.CurrX, horzEdge.Bot.Y);
      if (isLeftToRight) then
      begin
        IntersectEdges(horzEdge, e, pt);
        SwapPositionsInAEL(horzEdge, e);
        e := horzEdge.NextInAEL;
      end else
      begin
        IntersectEdges(e, horzEdge, pt);
        SwapPositionsInAEL(e, horzEdge);
        e := horzEdge.PrevInAEL;
      end;
    end;

    //check if we've finished with (consecutive) horizontals ...
    if isMax or (NextVertex(horzEdge).Pt.Y <> horzEdge.Top.Y) then Break;

    //still more horizontals in bound to process ...
    UpdateEdgeIntoAEL(horzEdge);
    ResetHorzDirection;

    if IsOpen(horzEdge) then
    begin
      if IsMaxima(horzEdge) then maxPair := GetMaximaPair(horzEdge);
      if IsHotEdge(horzEdge) then AddOutPt(horzEdge, horzEdge.Bot);
    end;
  end;

  if IsHotEdge(horzEdge) then
    AddOutPt(horzEdge, horzEdge.Top);

  if not IsOpen(horzEdge) then
    UpdateEdgeIntoAEL(horzEdge) //this is the end of an intermediate horiz.
  else if not IsMaxima(horzEdge) then
    UpdateEdgeIntoAEL(horzEdge)
  else if not assigned(maxPair) then //ie open at top
    DeleteFromAEL(horzEdge)
  else if IsHotEdge(horzEdge) then
      AddLocalMaxPoly(horzEdge, maxPair, horzEdge.Top)
  else
  begin
    DeleteFromAEL(maxPair); DeleteFromAEL(horzEdge);
  end;

end;
//------------------------------------------------------------------------------

procedure TClipper.DoTopOfScanbeam(Y: Int64);
var
  e: PActive;
begin
  FSel := nil; //FSel is reused to flag horizontals (see PushHorz below)
  e := FActives;
  while Assigned(e) do
  begin
    //nb: 'e' will never be horizontal here
    if (e.Top.Y = Y) then
    begin
      e.CurrX := e.Top.X;

      if IsHotEdge(e) then
      begin
        if assigned(e.PrevInAEL) and (e.PrevInAEL.CurrX = e.CurrX) and
          IsHotEdge(e.PrevInAEL) then
        begin
          //remove 'minima spikes' ...
          if LastOp(e.PrevInAEL) = LastOp(e) then
            LastOp(e).Pt := e.Top
          //help prevent micro self-intersections ...
          else if (e.PrevInAEL.Bot.Y <> Y) then  AddOutPt(e.PrevInAEL, e.Top);
        end;
        if assigned(e.NextInAEL) and (e.NextInAEL.CurrX = e.CurrX) and
          IsHotEdge(e.NextInAEL) then
        begin
          //remove 'minima spikes' ...
          if LastOp(e.NextInAEL) = LastOp(e) then
            LastOp(e).Pt := e.Top
          //help prevent micro self-intersections ...
          else if (e.NextInAEL.Top.Y <> Y) then AddOutPt(e.NextInAEL, e.Top);
        end;
      end;

      if IsMaxima(e) then
      begin
        e := DoMaxima(e);  //TOP OF BOUND (MAXIMA)
        Continue;
      end else
      begin
        //INTERMEDIATE VERTEX ...
        UpdateEdgeIntoAEL(e);
        if IsHotEdge(e) then AddOutPt(e, e.Bot);
        if IsHorizontal(e) then
          PushHorz(e);     //horizontals are processed later
      end;
    end;
    e := e.NextInAEL;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.DoMaxima(e: PActive): PActive;
var
  eNext, ePrev, eMaxPair: PActive;
begin
  ePrev := e.PrevInAEL;
  eNext := e.NextInAEL;
  Result := eNext;

  if IsOpen(e) and ([vfOpenStart, vfOpenEnd] * e.vertTop.flags <> []) then
  begin
    if IsHotEdge(e) then AddOutPt(e, e.Top);
    if not IsHorizontal(e) then
    begin
      if IsHotEdge(e) then TerminateHotOpen(e);
      DeleteFromAEL(e);
    end;
    Exit;
  end else
  begin
    eMaxPair := GetMaximaPair(e);
    if not assigned(eMaxPair) then Exit; //EMaxPair is a horizontal ...
  end;

  //only non-horizontal maxima here.
  //process any edges between maxima pair ...
  while (eNext <> eMaxPair) do
  begin
    IntersectEdges(e, eNext, e.Top);
    SwapPositionsInAEL(e, eNext);
    eNext := e.NextInAEL;
  end;

  if IsOpen(e) then
  begin
    if IsHotEdge(e) then
    begin
      if assigned(eMaxPair) then
        AddLocalMaxPoly(e, eMaxPair, e.Top) else
        AddOutPt(e, e.Top);
    end;
    if assigned(eMaxPair) then
      DeleteFromAEL(eMaxPair);
    DeleteFromAEL(e);

    if assigned(ePrev) then
      Result := ePrev.NextInAEL else
      Result := FActives;
    Exit;
  end;

  //here E.NextInAEL == ENext == EMaxPair ...
  if IsHotEdge(e) then
    AddLocalMaxPoly(e, eMaxPair, e.Top);

  DeleteFromAEL(e);
  DeleteFromAEL(eMaxPair);
  if assigned(ePrev) then
    Result := ePrev.NextInAEL else
    Result := FActives;
end;
//------------------------------------------------------------------------------

function TClipper.BuildResult(out closedPaths, openPaths: TPaths): Boolean;
var
  i, j, cntClosed, cntOpen: Integer;
  outRec: TOutRec;
begin
  try
    cntClosed := 0; cntOpen := 0;
    SetLength(closedPaths, FOutRecList.Count);
    SetLength(openPaths, FOutRecList.Count);
    for i := 0 to FOutRecList.Count -1 do
    begin
      outRec := FOutRecList[i];
      if not assigned(outRec.Pts) then Continue;

      if IsOpen(outRec) then
      begin
        openPaths[cntOpen] := BuildPath(outRec.Pts);
        if length(openPaths[cntOpen]) > 1 then inc(cntOpen);
      end else
      begin
        closedPaths[cntClosed] := BuildPath(outRec.Pts);
        j := length(closedPaths[cntClosed]);
        if (j > 2) and
          PointsEqual(closedPaths[cntClosed][0],
            closedPaths[cntClosed][j -1]) then
              setlength(closedPaths[cntClosed], j - 1);
        if j > 2 then inc(cntClosed);
      end;
    end;
    SetLength(closedPaths, cntClosed);
    SetLength(openPaths, cntOpen);
    result := true;
  except
    result := false;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.BuildResultTree(polyTree: TPolyTree;
  out openPaths: TPaths): Boolean;
var
  i, j, cntOpen: Integer;
  outRec: TOutRec;
  path: TPath;
begin
  try
    polyTree.Clear;
    setLength(openPaths, FOutRecList.Count);
    cntOpen := 0;
    for i := 0 to FOutRecList.Count -1 do
      if Assigned(FOutRecList[i]) then
      begin
        outRec := FOutRecList[i];
        //make sure outer/owner paths preceed their inner paths ...
        if assigned(outRec.Owner) and (outRec.Owner.Idx > outRec.Idx) then
        begin
          j := outRec.Owner.Idx;
          outRec.idx := j;
          FOutRecList[i] := FOutRecList[j];
          FOutRecList[j] := outRec;
          outRec := FOutRecList[i];
          outRec.Idx := i;
        end;

        if not assigned(outRec.Pts) then Continue;

        if IsOpen(outRec) then
        begin
          openPaths[cntOpen] := BuildPath(outRec.Pts);
          if length(openPaths[cntOpen]) > 1 then inc(cntOpen);
        end else
        begin
          path := BuildPath(outRec.Pts);
          j := length(path);
          if (j > 2) and PointsEqual(path[0], path[j -1]) then
            setlength(path, j - 1);
          if j < 3 then continue;

          //update ownership ...
          while assigned(outRec.Owner) and not assigned(outRec.Owner.Pts) do
            outRec.Owner := outRec.Owner.Owner;
          if assigned(outRec.Owner) and (outRec.Owner.State = outRec.State) then
          begin
            if IsOuter(outRec) then outRec.Owner := nil
            else outRec.Owner := outRec.Owner.Owner;
          end;

          if assigned(outRec.Owner) and assigned(outRec.Owner.PolyPath) then
            outRec.PolyPath := outRec.Owner.PolyPath.AddChild(path)
          else
            outRec.PolyPath := polyTree.AddChild(path);
        end;
      end;
    setLength(openPaths, cntOpen);
    result := true;
  except
    result := false;
  end;
end;
//------------------------------------------------------------------------------

function TClipper.GetBounds: TRect64;
var
  i: Integer;
  v, vStart: PVertex;
begin
  if FVertexList.Count = 0 then
    Result := Rect64(0, 0, 0, 0)
  else
    with PVertex(FVertexList[0]).Pt do
      Result := Rect64(X, Y, X, Y);
  for i := 0 to FVertexList.Count -1 do
  begin
    vStart := FVertexList[i];
    v := vStart;
    repeat
      if v.Pt.X < Result.Left then Result.Left := v.Pt.X
      else if v.Pt.X > Result.Right then Result.Right := v.Pt.X;
      if v.Pt.Y < Result.Top then Result.Top := v.Pt.Y
      else if v.Pt.Y > Result.Bottom then Result.Bottom := v.Pt.Y;
      v := v.next;
    until v = vStart;
  end;
end;

//------------------------------------------------------------------------------
//  TPolyPath methods
//------------------------------------------------------------------------------

constructor TPolyPath.Create;
begin
{$IFDEF XPLAT_GENERICS}
  FChildList := TList<TPolyPath>.Create;
{$ELSE}
  FChildList := TList.Create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TPolyPath.Destroy;
begin
  Clear;
  FChildList.Free;
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TPolyPath.Clear;
var
  i: integer;
begin
  for i := 0 to FChildList.Count -1 do
    TPolyPath(FChildList[i]).Free;
  FChildList.Clear;
end;
//------------------------------------------------------------------------------

function  TPolyPath.GetChild(index: Integer): TPolyPath;
begin
  if (index < 0) or (index >= FChildList.Count) then
    Result := nil else
    Result := FChildList[index];
end;
//------------------------------------------------------------------------------

function  TPolyPath.GetIsHole: Boolean;
var
  pp: TPolyPath;
begin
  result := true;
  pp := FParent;
  while assigned(pp) do
  begin
    result := not result;
    pp := pp.FParent;
  end;
//  Result := not assigned(FParent) or not FParent.GetIsHole;
end;
//------------------------------------------------------------------------------

function  TPolyPath.GetChildCnt: Integer;
begin
  Result := FChildList.Count;
end;
//------------------------------------------------------------------------------

function TPolyPath.AddChild(const path: TPath): TPolyPath;
begin
  Result := fClass.Create;
  Result.FParent := self;
  Result.FClass := FClass;
  FChildList.Add(Result);
  Result.FPath := path;
end;

//------------------------------------------------------------------------------
// TPolyTree methods
//------------------------------------------------------------------------------

constructor TPolyTree.Create;
begin
  inherited;
  FClass := TPolyPath;
end;

//------------------------------------------------------------------------------
//  TClipperD methods
//------------------------------------------------------------------------------

constructor TClipperD.Create(scale: double);
begin
  inherited Create;
  FScale := scale;
end;
//------------------------------------------------------------------------------

procedure TClipperD.AddPath(const pathD: TPathD; polyType: TPathType = ptSubject;
  isOpen: Boolean = false);
var
  p: TPath;
begin
  if FScale = 0 then FScale := DefaultScale;
  p := ScalePath(pathD, FScale, FScale);
  Inherited AddPath(p, polyType, isOpen);
end;
//------------------------------------------------------------------------------

procedure TClipperD.AddPaths(const pathsD: TPathsD; polyType: TPathType = ptSubject;
  isOpen: Boolean = false);
var
  pp: TPaths;
begin
  if FScale = 0 then FScale := DefaultScale;
  pp := ScalePaths(pathsD, FScale, FScale);
  Inherited AddPaths(pp, polyType, isOpen);
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedPaths: TPathsD): Boolean;
var
  closedP: TPaths;
begin
  Result := (FScale > 0) and
    inherited Execute(clipType, fillRule, closedP);
  if not Result then Exit;
  closedPaths := ScalePathsD(closedP, 1/FScale, 1/FScale);
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  out closedPaths, openPaths: TPathsD): Boolean;
var
  closedP, openP: TPaths;
begin
  Result := (FScale > 0) and
    inherited Execute(clipType, fillRule, closedP, openP);
  if not Result then Exit;
  closedPaths := ScalePathsD(closedP, 1/FScale, 1/FScale);
  openPaths := ScalePathsD(openP, 1/FScale, 1/FScale);
end;
//------------------------------------------------------------------------------

function TClipperD.Execute(clipType: TClipType; fillRule: TFillRule;
  var polytree: TPolyTreeD; out openPaths: TPathsD): Boolean;
var
  openP: TPaths;
begin
  Result := (FScale > 0) and
    inherited Execute(clipType, fillRule, TPolyTree(polytree), openP);
  if not Result then Exit;
  polytree.SetScale(1/FScale); //recursive
  openPaths := ScalePathsD(openP, 1/FScale, 1/FScale);
end;

//------------------------------------------------------------------------------
// TPolyPathD methods
//------------------------------------------------------------------------------

procedure TPolyPathD.SetScale(scale : double);
var
  i: integer;
begin
  FScale := scale;
  for i := 0 to FChildList.Count -1 do
    TPolyPathD(FChildList[i]).SetScale(scale);
end;
//------------------------------------------------------------------------------

function TPolyPathD.GetChild(index: Integer): TPolyPathD;
begin
  if (index < 0) or (index >= FChildList.Count) then
    Result := nil else
    Result := TPolyPathD(FChildList[index]);
end;
//------------------------------------------------------------------------------

function TPolyPathD.GetPathD: TPathD;
var
  i, len: integer;
begin
  len := length(FPath);
  setLength(Result, len);
  if (FScale = 0) or (FScale = 1.0) then
  begin
    for i := 0 to len -1 do
    begin
      Result[i].X := FPath[i].X;
      Result[i].Y := FPath[i].Y;
    end;
  end else
  begin
    for i := 0 to len -1 do
    begin
      Result[i].X := FPath[i].X / FScale;
      Result[i].Y := FPath[i].Y / FScale;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TPolyTreeD methods
//------------------------------------------------------------------------------

constructor TPolyTreeD.Create;
begin
  inherited;
  FClass := TPolyPathD;
end;

//------------------------------------------------------------------------------
// Additional functions
//------------------------------------------------------------------------------

procedure AddPolyNodeToPaths(Poly: TPolyPath; var Paths: TPaths);
var
  i: Integer;
begin
  if (Length(Poly.Path) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.Path;
  end;

  for i := 0 to Poly.ChildCount - 1 do
    AddPolyNodeToPaths(Poly.Child[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPaths(PolyTree: TPolyTree): TPaths;
begin
  Result := nil;
  AddPolyNodeToPaths(PolyTree, Result);
end;
//------------------------------------------------------------------------------

procedure AddPolyNodeToPathsD(Poly: TPolyPathD; var Paths: TPathsD);
var
  i: Integer;
begin
  if (Length(Poly.Path) > 0) then
  begin
    i := Length(Paths);
    SetLength(Paths, i +1);
    Paths[i] := Poly.GetPathD;
  end;

  for i := 0 to Poly.ChildCount - 1 do
    AddPolyNodeToPathsD(Poly.Child[i], Paths);
end;
//------------------------------------------------------------------------------

function PolyTreeToPaths(PolyTree: TPolyTreeD): TPathsD;
begin
  Result := nil;
  AddPolyNodeToPathsD(PolyTree, Result);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.

