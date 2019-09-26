unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, Image32, Image32_Layers, BitmapPanels,
  Vcl.Dialogs, Image32_MixedPath, Vcl.StdCtrls;

const
  UM_REPAINT = WM_USER +1;
type
  TMoveType = (mtNone, mtAllButtons, mtOneButton, mtRotateButton, mtLayer);

  TFrmMain = class(TForm)
    pnlMain: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    mnuHideControls: TMenuItem;
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    View1: TMenuItem;
    N2: TMenuItem;
    CopytoClipboard1: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuDeleteLayer: TMenuItem;
    N4: TMenuItem;
    mnuDeleteButton: TMenuItem;
    mnuMakePolyline: TMenuItem;
    mnuDeleteAllButtonControls: TMenuItem;
    mnuEditLayer: TMenuItem;
    pnlTop: TPanel;
    Label1: TLabel;
    cbTypes: TComboBox;
    mnuMakePolygon: TMenuItem;
    Label2: TLabel;
    edWidth: TEdit;
    UpDown1: TUpDown;
    N3: TMenuItem;
    mnuRotateButtons: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure pnlMainDblClick(Sender: TObject);
    procedure mnuMakePolylineClick(Sender: TObject);
    procedure mnuDeleteAllButtonControlsClick(Sender: TObject);
    procedure mnuHideControlsClick(Sender: TObject);
    procedure mnuEditLayerClick(Sender: TObject);
    procedure mnuDeleteLayerClick(Sender: TObject);
    procedure cbTypesChange(Sender: TObject);
    procedure mnuDeleteButtonClick(Sender: TObject);
    procedure edWidthChange(Sender: TObject);
    procedure mnuRotateButtonsClick(Sender: TObject);
  private
    layeredImage32: TLayeredImage32;
    clickPoint    : TPoint;
    rotatePoint   : TPoint;
    moveType      : TMoveType;
    clickedLayer  : TLayer32;
    designLayer   : TLayer32;
    currPointType : TPointType;
    buttonPath    : TMixedPath;
    btnPathRegion : TArrayOfArrayOfPointD;
    procedure UpdateButtonPath;
    function PointOnPath(const pt: TPoint): Boolean;
    procedure RepaintPanel;
    procedure UMREPAINT(var message: TMessage); message UM_REPAINT;
  public
    { Public declarations }
  end;

  //TMixedPathLayer32: Base class for TLineLayer32 and TPolygonLayer32
  TMixedPathLayer32 = class(THitTestLayer32)
  protected
    fMixedPath: TMixedPath;
  public
    constructor Create(owner: TLayeredImage32); override;
    destructor Destroy; override;
    procedure Offset(dx, dy: integer); override;
    property MixedPath: TMixedPath read fMixedPath;
  end;

  TLineLayer32 = class(TMixedPathLayer32)
  public
    procedure SetMixedPath(const mixedPath: TMixedPath);
  end;

  TPolygonLayer32 = class(TMixedPathLayer32)
  public
    procedure SetMixedPath(const mixedPath: TMixedPath);
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}
{$R RotateCursor.res}

uses
  Image32_BMP, Image32_PNG, Image32_JPG, Image32_Draw, Image32_Vector,
  Image32_Extra, Image32_Text, Image32_Clipper;

const
  crRotate = 1;
  margin = 100;
  hitTestWidth = 5;
var
  lineWidth: integer = 5;

type
  TExButtonDesignerLayer32 = class(TButtonDesignerLayer32)
  public
    PointType: TPointType;
  end;

//------------------------------------------------------------------------------
// TMixedPathLayer32
//------------------------------------------------------------------------------

constructor TMixedPathLayer32.Create(owner: TLayeredImage32);
begin
  inherited;
  fMixedPath := TMixedPath.Create;
end;
//------------------------------------------------------------------------------

destructor TMixedPathLayer32.Destroy;
begin
 fMixedPath.Free;
 inherited;
end;
//------------------------------------------------------------------------------

procedure TMixedPathLayer32.Offset(dx, dy: integer);
var
  i: integer;
  cp: TCtrlPoint;
begin
  inherited Offset(dx, dy);
  //when the layer is offset, offset fMixedPath control points too
  for i := 0 to fMixedPath.Count -1 do
  begin
    cp := fMixedPath[i];
    cp.X := cp.X + dx;
    cp.Y := cp.Y + dy;
    fMixedPath[i] := cp;
  end;
end;

//------------------------------------------------------------------------------
// TLineLayer32
//------------------------------------------------------------------------------

procedure TLineLayer32.SetMixedPath(const mixedPath: TMixedPath);
var
  rec: TRect;
  flattened: TArrayOfPointD;
begin
  //calculate and assign hit test regions
  flattened := mixedPath.FlattenedPath;
  self.HitTestRegions := InflateOpenPath(OpenPathToFlatPolygon(flattened),
    lineWidth + hitTestWidth);

  fMixedPath.Assign(mixedPath);

  //draw flattened line
  rec := GetBounds(HitTestRegions);
  self.SetBounds(rec);
  Image.Clear;
  flattened := OffsetPath(flattened, -Left, -Top);
  DrawLine(Image, flattened, lineWidth, clNavy32, esRound);
  CursorId := crSizeAll;
end;

//------------------------------------------------------------------------------
// TPolygonLayer32
//------------------------------------------------------------------------------

procedure TPolygonLayer32.SetMixedPath(const mixedPath: TMixedPath);
var
  rec: TRect;
  flattened: TArrayOfPointD;
begin
  //calculate and assign hit test regions
  flattened := mixedPath.FlattenedPath;
  //nb: UnionPolygon fixes up any self-intersections
  self.HitTestRegions := UnionPolygon(flattened, frEvenOdd);
  HitTestRegions := InflatePolygons(HitTestRegions, lineWidth + hitTestWidth);

  fMixedPath.Assign(mixedPath);

  //draw polygon
  rec := GetBounds(HitTestRegions);
  self.SetBounds(rec);
  flattened := OffsetPath(flattened, -Left, -Top);
  Image.Clear;
  DrawPolygon(Image, flattened, frNonZero, $88FFFF66);
  DrawLine(Image, flattened, lineWidth, clMaroon32, esClosed);
  CursorId := crSizeAll;
end;

//------------------------------------------------------------------------------
// TFrmMain
//------------------------------------------------------------------------------

procedure TFrmMain.FormCreate(Sender: TObject);
var
  layer: TLayer32;
  rec: TRect;
begin
  buttonPath := TMixedPath.Create;

  //SETUP THE DISPLAY PANEL
  pnlMain.BorderWidth := DPI(16);
  pnlMain.BevelInner := bvLowered;
  //set pnlMain.Tabstop -> true to enable keyboard controls
  pnlMain.TabStop := true;
  pnlMain.FocusedColor := clGradientInactiveCaption;
  pnlMain.BitmapProperties.Scale := 1;

  //SETUP LAYEREDIMAGE32
  rec := pnlMain.InnerClientRect;
  layeredImage32 := TLayeredImage32.Create(RectWidth(rec), RectHeight(rec));
  layeredImage32.BackgroundColor := clWhite32;

  //hatched image layer is TDesignerLayer32 so it's non-clickable
  layer := layeredImage32.AddNewLayer(TDesignerLayer32, 'hatched background');
  layer.SetSize(layeredImage32.Width, layeredImage32.Height);
  HatchBackground(layer.Image, $FFF0F0F0, clWhite32);

  //designer layer to highlight layer selection, virtual spine controls etc.
  designLayer := layeredImage32.AddNewLayer(TDesignerLayer32, 'design layer');
  designLayer.SetSize(layeredImage32.Width, layeredImage32.Height);

  Screen.Cursors[crRotate] :=
    LoadImage(hInstance, 'ROTATE', IMAGE_CURSOR, 32,32,0);

  cbTypesChange(nil);
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  buttonPath.Free;
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.UpdateButtonPath;
var
  i, fig,lig: integer;
begin
  buttonPath.Clear;
  //create an array of points using the midpoint of each button layer
  if layeredImage32.GroupCount(1) > 0 then
  begin
    fig := layeredImage32.GetIdxFirstLayerInGroup(1);
    lig := layeredImage32.GetIdxLastLayerInGroup(1);
    for i := fig to lig do
      with TExButtonDesignerLayer32(layeredImage32[i]) do
        buttonPath.Add(MidPoint, PointType);
  end;
  if buttonPath.Count > 1 then
    btnPathRegion := InflateOpenPath(OpenPathToFlatPolygon(
      buttonPath.FlattenedPath), hitTestWidth, jsRound) else
    btnPathRegion := nil;
end;
//------------------------------------------------------------------------------

function TFrmMain.PointOnPath(const pt: TPoint): Boolean;
begin
  result := PointInPolgons(PointD(pt), btnPathRegion, frEvenOdd);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeleteButtonClick(Sender: TObject);
var
  fig, lig: integer;
begin
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then Exit;

  //layers must be ungrouped before individual layers can be deleted
  fig := layeredImage32.GetIdxFirstLayerInGroup(1);
  lig := layeredImage32.GetIdxLastLayerInGroup(1);
  layeredImage32.UnGroup(1);
  layeredImage32.DeleteLayer(lig);
  if lig > fig then
  begin
    layeredImage32.Group(fig, lig -1); //re-group
    cbTypes.ItemIndex :=
      Integer(TExButtonDesignerLayer32(layeredImage32.TopLayer).PointType);
  end;

  UpdateButtonPath;
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeleteAllButtonControlsClick(Sender: TObject);
begin
  if layeredImage32.TopLayer.GroupIndex <> 1 then Exit;
  layeredImage32.DeleteGroup(1);
  UpdateButtonPath;
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.cbTypesChange(Sender: TObject);
begin
  currPointType := TPointType(cbTypes.ItemIndex);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.RepaintPanel;
var
  dc: HDC;
  i,j,k, d, len, cnt: integer;
  rec: TRect;
  pointType: TPointType;
  pts, pts2: TArrayOfPointD;
  dl: TDesignerLayer32;
  isStartOfPath: Boolean;
begin
  dl := TDesignerLayer32(designLayer);
  dl.Image.Clear;
  cbTypes.Enabled := (buttonPath.Count = 0) or
    buttonPath.CurrentTypeIsComplete;

  if (layeredImage32.TopLayer is TButtonDesignerLayer32) then
  begin
    if layeredImage32.TopLayer.GroupIndex = 2 then //ie is rotating
    begin
      DrawButton(dl.Image, PointD(rotatePoint), DefaultButtonSize, clMaroon32);
      d := Round(Distance(Point(layeredImage32.TopLayer.MidPoint), rotatePoint));
      with rotatePoint do rec := Rect(X - d, Y - d, X + d, Y + d);
      dl.DrawEllipse(rec);
    end;

    //flattened buttons path
    if layeredImage32.GroupCount(1) > 1 then
      DrawLine(dl.Image, buttonPath.FlattenedPath, lineWidth, clBlack32, esRound);

    //display control lines and virtual buttons for spline segments
    cnt := buttonPath.Count;
    i := 1;
    while i < cnt do
    begin
      j := i + 1;
      isStartOfPath := i = 1;
      pointType := buttonPath[i].PointType;
      while (j < cnt) and (buttonPath[j].PointType = pointType) do inc(j);
      if not isStartOfPath and (pointType in [ptCSpline,ptQSpline]) then dec(i);
      len := j-i +1;
      setLength(pts, len);
      for k := 0 to len -1 do pts[k] := buttonPath[i - 1 + k].Point;

      case pointType of
        ptCBezier:
          while Length(pts) >= 4 do
          begin
            pts2 := Copy(pts, 0, 4);
            dl.DrawCSplineDesign(pts2);
            Delete(pts, 0, 3);
          end;
        ptQBezier:
          while Length(pts) >= 3 do
          begin
            pts2 := Copy(pts, 0, 3);
            dl.DrawQSplineDesign(pts2);
            Delete(pts, 0, 2);
          end;
        ptCSpline:
          begin
            if len < 4 then break;
            dl.DrawCSplineDesign(pts, isStartOfPath);
          end;
        ptQSpline:
          begin
            if len < 3 then break;
            dl.DrawQSplineDesign(pts, isStartOfPath);
          end;
      end;
      i := j;
    end;
  end;

  //display dashed hit-test outline for selected layer
  if Assigned(clickedLayer) and (moveType = mtLayer) then
  begin
    if (clickedLayer is TLineLayer32) then
      DrawDashedLine(dl.Image, TLineLayer32(clickedLayer).HitTestRegions,
        [5,5], nil, 1, clRed32, esClosed)
    else if (clickedLayer is TPolygonLayer32) then
      DrawDashedLine(dl.Image, TPolygonLayer32(clickedLayer).HitTestRegions,
        [5,5], nil, 1, clRed32, esClosed);
  end;

  //merge layeredImage32 onto pnlMain
  pnlMain.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  dc := pnlMain.Bitmap.Canvas.Handle;
  layeredImage32.GetMergedImage(mnuHideControls.Checked).CopyToDc(dc,0,0,false);
  pnlMain.Refresh;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.UMREPAINT(var message: TMessage);
begin
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuMakePolylineClick(Sender: TObject);
var
  polygonLayer: TPolygonLayer32;
  lineLayer: TLineLayer32;
begin
  if not cbTypes.Enabled or
    not (layeredImage32.TopLayer is TButtonDesignerLayer32) then Exit;

  //create a 'completed' path (polyline or polygon) from the buttons
  if Sender = mnuMakePolygon then
  begin
    if buttonPath.Count < 3 then Exit;
    layeredImage32.DeleteGroup(1);
    polygonLayer :=
      TPolygonLayer32(layeredImage32.AddNewLayer(TPolygonLayer32, ''));
    polygonLayer.SetMixedPath(buttonPath);
    designLayer.BringForward(polygonLayer.Index);
  end else
  begin
    if buttonPath.Count < 2 then Exit;
    layeredImage32.DeleteGroup(1);
    lineLayer :=
      TLineLayer32(layeredImage32.AddNewLayer(TLineLayer32, ''));
    lineLayer.SetMixedPath(buttonPath);
    designLayer.BringForward(lineLayer.Index);
  end;
  buttonPath.Clear;
  btnPathRegion := nil;
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuEditLayerClick(Sender: TObject);
var
  fig, lig, i: integer;
begin
  if not assigned(clickedLayer) or
    not (clickedLayer is TMixedPathLayer32) then Exit;

  //When 'editing' a layer (line or polygon), delete any buttons first
  if layeredImage32.GroupCount(1) > 0 then
    layeredImage32.DeleteGroup(1);

  //now create buttons from the selected layer
  CreateButtonGroup(layeredImage32,
    TMixedPathLayer32(clickedLayer).MixedPath.CtrlPoints,
    clGreen32, DPI(9), [], TExButtonDesignerLayer32);

  //update PointType for each button layer
  fig := layeredImage32.GetIdxFirstLayerInGroup(1);
  lig := layeredImage32.GetIdxLastLayerInGroup(1);
  for i := fig to lig do
    TExButtonDesignerLayer32(layeredImage32[i]).PointType :=
      TMixedPathLayer32(clickedLayer).MixedPath[i - fig].PointType;

  //delete the selected layer now we have the edit buttons
  layeredImage32.DeleteLayer(clickedLayer.Index);
  clickedLayer := nil;

  UpdateButtonPath;
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuRotateButtonsClick(Sender: TObject);
var
  rec: TRect;
  pt: TPointD;
  i: integer;
begin
  if buttonPath.Count < 2 then Exit;
  rec := GetBounds(buttonPath.CtrlPoints);
  rotatePoint := MidPoint(rec);
  i := (RectWidth(rec) + RectHeight(rec)) div 4;
  pt := PointD(rotatePoint.X,rotatePoint.Y - i);
  CreateButtonGroup(layeredImage32, [pt], clRed32,
    DPI(9), [], TButtonDesignerLayer32);
  layeredImage32.TopLayer.CursorId := crRotate;
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainDblClick(Sender: TObject);
begin
  //adding a button - either starting a button group or adding to it
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then
    CreateButtonGroup(layeredImage32, [PointD(clickPoint)],
      clGreen32, DPI(9), [], TExButtonDesignerLayer32)
  else
    AddToButtonGroup(layeredImage32, 1, clickPoint);
  TExButtonDesignerLayer32(layeredImage32.TopLayer).PointType := currPointType;
  UpdateButtonPath;
  PostMessage(handle, UM_REPAINT, 0,0);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  clickedLayer := nil;
  moveType := mtNone;
  //convert pnlMain client coordinates to bitmap coordinates
  pt := Types.Point(X,Y);
  if not pnlMain.ClientToBitmap(pt) then Exit;
  clickPoint := pt;

  //get the layer that was clicked (if any)
  clickedLayer := layeredImage32.GetLayerAt(pt);
  //disable pnlMain scrolling if we're about to move a layer
  pnlMain.BitmapProperties.ZoomAndScrollEnabled := not Assigned(clickedLayer);

  with layeredImage32 do
    if (TopLayer.GroupIndex = 2) and (TopLayer <> clickedLayer) then
      DeleteGroup(2);

  if Assigned(clickedLayer) and (clickedLayer is TButtonDesignerLayer32) then
  begin
    if clickedLayer.GroupIndex = 2 then
      moveType := mtRotateButton else
      moveType := mtOneButton
  end
  else if PointOnPath(pt) then
    moveType := mtAllButtons
  else if Assigned(clickedLayer) then
    moveType := mtLayer; //ie a completed layer (line or polygon)
  PostMessage(handle, UM_REPAINT, 0,0);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  i, dx,dy, fig, lig: integer;
  pt: TPoint;
  path: TArrayOfPointD;
  layer: TLayer32;
  angle: double;
begin
  pt := Types.Point(X,Y);
  if not pnlMain.ClientToBitmap(pt) then Exit;

  if moveType = mtNone then
  begin
    //just update the cursor
    layer := layeredImage32.GetLayerAt(pt);
    if Assigned(layer) and (layer is TButtonDesignerLayer32) then
      pnlMain.Cursor := layer.CursorId
    else if PointOnPath(pt) then
      pnlMain.Cursor := crSizeAll
    else if Assigned(layer) then
      pnlMain.Cursor := layer.CursorId
    else
      pnlMain.Cursor := crDefault;
    Exit;
  end;

  //moving either one or all buttons, or a completed layer
  dx := pt.X - clickPoint.X;
  dy := pt.Y - clickPoint.Y;
  if moveType = mtAllButtons then
    layeredImage32.OffsetGroup(1, dx, dy) else
    clickedLayer.Offset(dx, dy);

  if moveType = mtRotateButton then
  begin
    angle := GetAngle(clickPoint, rotatePoint, pt);
    path := RotatePath(buttonPath.CtrlPoints, PointD(rotatePoint), angle);
    fig := layeredImage32.GetIdxFirstLayerInGroup(1);
    lig := layeredImage32.GetIdxLastLayerInGroup(1);
    for i := fig to lig do
      layeredImage32[i].PositionCenteredAt(path[i-fig]);
  end;

  clickPoint := pt;
  if moveType in [mtAllButtons, mtOneButton, mtRotateButton] then
    UpdateButtonPath;
  PostMessage(handle, UM_REPAINT, 0,0);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  moveType := mtNone;
  //re-enable pnlMain zoom and scroll
  pnlMain.BitmapProperties.ZoomAndScrollEnabled := true;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeleteLayerClick(Sender: TObject);
begin
  if Assigned(clickedLayer) and (clickedLayer is TMixedPathLayer32) then
  begin
    layeredImage32.DeleteLayer(clickedLayer.Index);
    clickedLayer := nil;
  RepaintPanel;
  end;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.CopytoClipboard1Click(Sender: TObject);
begin
  layeredImage32.GetMergedImage(mnuHideControls.Checked).CopyToClipBoard;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuHideControlsClick(Sender: TObject);
begin
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.edWidthChange(Sender: TObject);
begin
  lineWidth := Max(1, Min(25, strtoint(edWidth.text)));
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
