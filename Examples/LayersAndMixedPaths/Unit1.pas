unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, StdCtrls, ComCtrls,
  BitmapPanels, Image32, Image32_Layers, Image32_MixedPath;

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
    currPointType : TMixedType;
    //buttonPath: represents the button path that's being constructed
    buttonPath    : TMixedPath;
    //btnPathRegion: hit-test region for (unfinished) button path
    btnPathRegion : TArrayOfArrayOfPointD;
    procedure UpdateButtonPathFromButtons;
    function PointOnButtonPath(const pt: TPoint): Boolean;
    procedure RepaintPanel;
    procedure UMREPAINT(var message: TMessage); message UM_REPAINT;
  public
    { Public declarations }
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
    PointType: TMixedType;
  end;

//------------------------------------------------------------------------------
// TLineLayer32
//------------------------------------------------------------------------------

procedure TLineLayer32.SetMixedPath(const mixedPath: TMixedPath);
var
  rec: TRect;
  flattened: TArrayOfPointD;
begin
  flattened := mixedPath.FlattenedPath;
  //calculate and assign hit test regions
  HitTestRegions := InflateOpenPath(OpenPathToFlatPolygon(flattened),
    lineWidth + hitTestWidth);
  rec := GetBounds(HitTestRegions);
  self.SetBounds(rec);

  self.MixedPath.Assign(mixedPath);

  //draw flattened open path
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
  flattened := mixedPath.FlattenedPath;

  //calculate and assign hit test regions
  //nb: UnionPolygon here fixes up any potential self-intersections
  HitTestRegions := UnionPolygon(flattened, frEvenOdd);
  HitTestRegions := InflatePolygons(HitTestRegions, lineWidth + hitTestWidth);
  rec := GetBounds(HitTestRegions);
  self.SetBounds(rec);

  self.MixedPath.Assign(mixedPath);

  //draw polygon
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

  //hatched image layer is TDesignerLayer32 so it's 'non-clickable'
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

procedure TFrmMain.UpdateButtonPathFromButtons;
var
  i, fig,lig: integer;
begin
  buttonPath.Clear;
  //create an array of points using the midpoint of each button layer
  if layeredImage32.GroupCount(1) > 0 then
  begin
    fig := layeredImage32.GetFirstInGroupIdx(1);
    lig := layeredImage32.GetLastInGroupIdx(1);
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

function TFrmMain.PointOnButtonPath(const pt: TPoint): Boolean;
begin
  result := PointInPolygons(PointD(pt), btnPathRegion, frEvenOdd);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeleteButtonClick(Sender: TObject);
var
  fig, lig: integer;
begin
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then Exit;

  //layers must be ungrouped before individual layers can be deleted
  fig := layeredImage32.GetFirstInGroupIdx(1);
  lig := layeredImage32.GetLastInGroupIdx(1);
  layeredImage32.UnGroup(1);
  layeredImage32.DeleteLayer(lig);
  if lig > fig then
  begin
    layeredImage32.Group(fig, lig -1); //re-group
    cbTypes.ItemIndex :=
      Integer(TExButtonDesignerLayer32(layeredImage32.TopLayer).PointType);
  end;

  UpdateButtonPathFromButtons;
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeleteAllButtonControlsClick(Sender: TObject);
begin
  if layeredImage32.TopLayer.GroupId <> 1 then Exit;
  layeredImage32.DeleteGroup(1);
  UpdateButtonPathFromButtons;
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.cbTypesChange(Sender: TObject);
begin
  currPointType := TMixedType(cbTypes.ItemIndex);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.RepaintPanel;
var
  dc: HDC;
  d: integer;
  rec: TRect;
  dl: TDesignerLayer32;
  dashes: TArrayOfInteger;
begin
  dl := TDesignerLayer32(designLayer);
  dl.Image.Clear;
  cbTypes.Enabled := (buttonPath.Count = 0) or
    buttonPath.CurrentTypeIsComplete;

  if (layeredImage32.TopLayer is TButtonDesignerLayer32) then
  begin
    if layeredImage32.TopLayer.GroupId = 2 then //ie is rotating
    begin
      DrawButton(dl.Image, PointD(rotatePoint), DefaultButtonSize, clMaroon32);
      d := Round(Distance(Point(layeredImage32.TopLayer.MidPoint), rotatePoint));
      with rotatePoint do rec := Rect(X - d, Y - d, X + d, Y + d);
      dl.DrawEllipse(rec);
    end;

    //flattened buttons path
    if layeredImage32.GroupCount(1) > 1 then
      DrawLine(dl.Image, buttonPath.FlattenedPath, lineWidth, clBlack32, esRound);
    DrawMixedPathDesigner(buttonPath, dl);
  end;

  setLength(dashes, 2);
  dashes[0] := 5; dashes[1] := 5;
  //display dashed hit-test outline for selected layer
  if Assigned(clickedLayer) and (moveType = mtLayer) then
  begin
    if (clickedLayer is TLineLayer32) then
      DrawDashedLine(dl.Image, TLineLayer32(clickedLayer).HitTestRegions,
        dashes, nil, 1, clRed32, esClosed)
    else if (clickedLayer is TPolygonLayer32) then
      DrawDashedLine(dl.Image, TPolygonLayer32(clickedLayer).HitTestRegions,
        dashes, nil, 1, clRed32, esClosed);
  end;

  //merge layeredImage32 onto pnlMain
  //pnlMain.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  pnlMain.Bitmap.Width := layeredImage32.Width;
  pnlMain.Bitmap.Height := layeredImage32.Height;
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
  //just in case rotating ...
  if layeredImage32.GroupCount(2) > 0 then layeredImage32.DeleteGroup(2);

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
  fig := layeredImage32.GetFirstInGroupIdx(1);
  lig := layeredImage32.GetLastInGroupIdx(1);
  for i := fig to lig do
    TExButtonDesignerLayer32(layeredImage32[i]).PointType :=
      TMixedPathLayer32(clickedLayer).MixedPath.PointTypes[i - fig];

  //delete the selected layer now we have the edit buttons
  layeredImage32.DeleteLayer(clickedLayer.Index);
  clickedLayer := nil;

  UpdateButtonPathFromButtons;
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
  StartButtonGroup(layeredImage32, Point(pt), clRed32,
    DPI(9), [], TButtonDesignerLayer32);
  layeredImage32.TopLayer.CursorId := crRotate;
  RepaintPanel;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainDblClick(Sender: TObject);
begin
  //adding a button - either starting a button group or adding to it
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then
    StartButtonGroup(layeredImage32, clickPoint,
      clGreen32, DPI(9), [], TExButtonDesignerLayer32)
  else
    AddToButtonGroup(layeredImage32, 1, clickPoint);
  TExButtonDesignerLayer32(layeredImage32.TopLayer).PointType := currPointType;
  UpdateButtonPathFromButtons;
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
    if (TopLayer.GroupId = 2) and (TopLayer <> clickedLayer) then
      DeleteGroup(2);

  if Assigned(clickedLayer) and (clickedLayer is TButtonDesignerLayer32) then
  begin
    if clickedLayer.GroupId = 2 then
      moveType := mtRotateButton else
      moveType := mtOneButton
  end
  else if PointOnButtonPath(pt) then
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
    else if PointOnButtonPath(pt) then
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
    buttonPath.Rotate(PointD(rotatePoint), angle);
    //path := RotatePath(buttonPath.CtrlPoints, PointD(rotatePoint), angle);
    fig := layeredImage32.GetFirstInGroupIdx(1);
    lig := layeredImage32.GetLastInGroupIdx(1);
    for i := fig to lig do
      layeredImage32[i].PositionCenteredAt(buttonPath[i-fig]);
  end;

  clickPoint := pt;
  if moveType in [mtAllButtons, mtOneButton, mtRotateButton] then
    UpdateButtonPathFromButtons;
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
