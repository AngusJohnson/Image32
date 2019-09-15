unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, Image32, Image32_Layers, BitmapPanels,
  Vcl.Dialogs;

type
  TMoveType = (mtNone, mtAllButtons, mtOneButton, mtLayer);

  TForm1 = class(TForm)
    Panel1: TPanel;
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
    mnuAddControlPoint: TMenuItem;
    mnuClosePath: TMenuItem;
    mnuDeleteButton: TMenuItem;
    mnuEndPath: TMenuItem;
    mnuDeleteAllButtonControls: TMenuItem;
    mnuEditLayer: TMenuItem;
    N3: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuAddControlPointClick(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
    procedure mnuClosePathClick(Sender: TObject);
    procedure mnuEndPathClick(Sender: TObject);
    procedure mnuDeleteAllButtonControlsClick(Sender: TObject);
    procedure mnuHideControlsClick(Sender: TObject);
    procedure mnuEditLayerClick(Sender: TObject);
    procedure mnuDeleteLayerClick(Sender: TObject);
  private
    layeredImage32: TLayeredImage32;
    clickPoint    : TPoint;
    moveType      : TMoveType;
    clickedLayer  : TLayer32;
    designLayer   : TLayer32;
    popupPoint    : TPoint;
    function ButtonsToPath: TArrayOfPointD;
    function PointOnPath(const pt: TPoint;
      const path: TArrayOfPointD): Boolean;
    procedure PaintDesignLayer;
    procedure PaintLayeredImage;
  public
    { Public declarations }
  end;

  //TExHitTestLayer32: Base class for TLineLayer32 and TPolygonLayer32
  TExHitTestLayer32 = class(THitTestLayer32)
  protected
    fHitTestPaths: TArrayOfArrayOfPointD;
    function HitTest(const pt: TPoint): Boolean; override;
  public
    procedure Offset(dx, dy: integer); override;
  end;

  TLineLayer32 = class(TExHitTestLayer32)
  public
    procedure SetPath(const path: TArrayOfPointD); override;
  end;

  TPolygonLayer32 = class(TExHitTestLayer32)
  public
    procedure SetPath(const path: TArrayOfPointD); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Image32_BMP, Image32_PNG, Image32_JPG, Image32_Draw, Image32_Vector,
  Image32_Extra, Image32_Text, Image32_Clipper;

const
  margin = 100;
  hitTestWidth = 5;


//------------------------------------------------------------------------------
// TExHitTestLayer32
//------------------------------------------------------------------------------

function TExHitTestLayer32.HitTest(const pt: TPoint): Boolean;
begin
  Result := PointInPaths(PointD(pt), fHitTestPaths, frEvenOdd);
end;
//------------------------------------------------------------------------------

procedure TExHitTestLayer32.Offset(dx, dy: integer);
begin
  inherited Offset(dx, dy);
  fHitTestPaths := OffsetPath(fHitTestPaths, dx, dy);
end;

//------------------------------------------------------------------------------
// TLineLayer32
//------------------------------------------------------------------------------

procedure TLineLayer32.SetPath(const path: TArrayOfPointD);
var
  rec: TRect;
  path2: TArrayOfPointD;
begin
  inherited;
  path2 := QSpline(path); //flattened
  fHitTestPaths := InflatePolygon(OpenPathToFlatPolygon(path2), hitTestWidth);
  rec := GetBounds(path2);
  InflateRect(rec, 10, 10); //allow room for hittest and line drawing
  self.SetBounds(rec);
  Image.Clear;
  path2 := OffsetPath(path2, -Left, -Top);
  DrawLine(Image, path2, 5, clNavy32, esRound);
  CursorId := crSizeAll;
end;

//------------------------------------------------------------------------------
// TPolygonLayer32
//------------------------------------------------------------------------------

procedure TPolygonLayer32.SetPath(const path: TArrayOfPointD);
var
  rec: TRect;
  path2: TArrayOfPointD;
  paths: TArrayOfArrayOfPointD;
begin
  inherited;
  path2 := QSpline(path); //flattened
  //before calling InflatePolygons, fix up any self-intersections
  paths := UnionPolygon(path2, frEvenOdd);
  fHitTestPaths := InflatePolygons(paths, hitTestWidth);
  rec := GetBounds(path2);
  InflateRect(rec, 10, 10); //allows room for hittest and drawing line width
  self.SetBounds(rec);
  path2 := OffsetPath(path2, -Left, -Top);
  Image.Clear;
  DrawPolygon(Image, path2, frNonZero, $88FFFF66);
  DrawLine(Image, path2, 5, clMaroon32, esClosed);
  CursorId := crSizeAll;
end;

//------------------------------------------------------------------------------
// TForm1
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  layer: TLayer32;
  rec: TRect;
begin
  //SETUP THE DISPLAY PANEL
  Panel1.BorderWidth := DPI(16);
  Panel1.BevelInner := bvLowered;
  //set Panel1.Tabstop -> true to enable keyboard controls
  Panel1.TabStop := true;
  Panel1.FocusedColor := clGradientInactiveCaption;
  Panel1.BitmapProperties.Scale := 1;

  //SETUP LAYEREDIMAGE32
  rec := Panel1.InnerClientRect;
  layeredImage32 := TLayeredImage32.Create(RectWidth(rec), RectHeight(rec));
  //note: we don't need to worry about BackgroundColor as we're
  //going to add a hatched image for the background instead.

  //hatched image layer is TDesignerLayer32 so it's non-clickable
  layer := layeredImage32.AddNewLayer(TDesignerLayer32, 'hatched background');
  layer.SetSize(layeredImage32.Width, layeredImage32.Height);
  HatchBackground(layer.Image, $FFF0F0F0, clWhite32);

  //designer layer to highlight layer selection, virtual spine controls etc.
  designLayer := layeredImage32.AddNewLayer(TDesignerLayer32, 'design layer');
  designLayer.SetSize(layeredImage32.Width, layeredImage32.Height);

  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

function TForm1.ButtonsToPath: TArrayOfPointD;
var
  i,j, cnt: integer;
begin
  //create an array of points using the midpoint of each button layer
  if layeredImage32.TopLayer is TButtonDesignerLayer32 then
  begin
    cnt := layeredImage32.CountLayersInGroup(1);
    setLength(Result, cnt);
    i := layeredImage32.GetIdxFirstLayerInGroup(1);
    for j := 0 to cnt -1 do
      Result[j] := layeredImage32[i +j].MidPoint;
  end else
    result := nil;
end;
//------------------------------------------------------------------------------

function TForm1.PointOnPath(const pt: TPoint;
  const path: TArrayOfPointD): Boolean;
var
  outlinePaths: TArrayOfArrayOfPointD;
begin
  //widen a virtual path by hitTestWidth
  if not assigned(path) then begin Result := False; Exit; end;
  outlinePaths := InflatePolygon(Image32_Vector.OpenPathToFlatPolygon(path),
    hitTestWidth, jsRound);
  result := PointInPaths(PointD(pt), outlinePaths, frEvenOdd);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuDeleteAllButtonControlsClick(Sender: TObject);
begin
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then Exit;
  layeredImage32.DeleteGroup(1);
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAddControlPointClick(Sender: TObject);
begin
  //either start a button group or add to it
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then
    CreateButtonGroup(layeredImage32, [PointD(clickPoint)], clGreen32, DPI(9), [])
  else
    AddToButtonGroup(layeredImage32, 1, clickPoint);

  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.PaintDesignLayer;
var
  path: TArrayOfPointD;
begin
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then
  begin
    //no designer buttons so just clear the design layer
    layeredImage32.TopLayer.Image.Clear;
  end
  else if layeredImage32.CountLayersInGroup(1) > 2 then
  begin
    path := ButtonsToPath;
    designLayer.Image.Clear;
    DrawLine(designLayer.Image, QSpline(path), 5, clBlack32, esRound);
    TDesignerLayer32(designLayer).DrawQSplineDesign(path);
  end;

  if Assigned(clickedLayer) and (moveType = mtLayer) then
  begin
    if (clickedLayer is TLineLayer32) then
      DrawDashedLine(TDesignerLayer32(designLayer).Image,
        TLineLayer32(clickedLayer).fHitTestPaths,
        [5,5], nil, 1, clRed32, esRound)
    else if (clickedLayer is TPolygonLayer32) then
      DrawDashedLine(TDesignerLayer32(designLayer).Image,
        TPolygonLayer32(clickedLayer).fHitTestPaths,
        [5,5], nil, 1, clRed32, esClosed);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.PaintLayeredImage;
var
  dc: HDC;
begin
  PaintDesignLayer;
  Panel1.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  dc := Panel1.Bitmap.Canvas.Handle;
  layeredImage32.GetMergedImage(mnuHideControls.Checked).CopyToDc(dc,0,0,false);
  Panel1.Refresh;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuClosePathClick(Sender: TObject);
var
  polygonLayer: TPolygonLayer32;
  path: TArrayOfPointD;
begin
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) or
    (layeredImage32.CountLayersInGroup(1) < 3) then Exit;
  //Close Path creates a polygon, End Path finishes an open path (line)
  path := ButtonsToPath;
  layeredImage32.DeleteGroup(1);
  polygonLayer :=
    TPolygonLayer32(layeredImage32.AddNewLayer(TPolygonLayer32, 'poly'));
  polygonLayer.SetPath(path);

  //move designLayer back to the top
  designLayer.BringForward(polygonLayer.Index);
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuEndPathClick(Sender: TObject);
var
  lineLayer: TLineLayer32;
  path: TArrayOfPointD;
begin
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) or
    (layeredImage32.CountLayersInGroup(1) < 3) then Exit;

  //Close Path creates a polygon, End Path finishes an open path (line)
  path := ButtonsToPath;
  layeredImage32.DeleteGroup(1);
  lineLayer :=
    TLineLayer32(layeredImage32.AddNewLayer(TLineLayer32, 'line'));
  lineLayer.SetPath(path);

  designLayer.BringForward(lineLayer.Index);
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuEditLayerClick(Sender: TObject);
begin
  if not assigned(clickedLayer) or not (clickedLayer is THitTestLayer32) then
    Exit;
  //nb: only button layers are grouped and there should only ever be one group
  //of buttons and they should always be on top. Therefore, when editing a
  //previously finished layer (line of polygon), delete any buttons first.
  if (layeredImage32.TopLayer is TButtonDesignerLayer32) then
    layeredImage32.DeleteGroup(1);
  CreateButtonGroup(layeredImage32, THitTestLayer32(clickedLayer).Path,
    clGreen32, DPI(9), []);
  layeredImage32.DeleteLayer(clickedLayer.Index);
  clickedLayer := nil;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1DblClick(Sender: TObject);
begin
  //add a new button
  GetCursorPos(popupPoint);
  popupPoint := Panel1.ScreenToClient(popupPoint);
  mnuAddControlPointClick(nil);
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  clickedLayer := nil;
  moveType := mtNone;
  //convert Panel1 client coordinates to bitmap coordinates
  pt := Types.Point(X,Y);
  if not Panel1.ClientToBitmap(pt) then Exit;
  clickPoint := pt;

  //get the layer that was clicked (if any)
  clickedLayer := layeredImage32.GetLayerAt(pt);
  //disable panel1 scrolling if we're about to move a layer
  panel1.BitmapProperties.ZoomAndScrollEnabled := not Assigned(clickedLayer);

  if Assigned(clickedLayer) and (clickedLayer is TButtonDesignerLayer32) then
    moveType := mtOneButton
  else if PointOnPath(pt, QSpline(ButtonsToPath)) then
    moveType := mtAllButtons
  else if Assigned(clickedLayer) then
    moveType := mtLayer;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy: integer;
  pt: TPoint;
  layer: TLayer32;
begin
  pt := Types.Point(X,Y);
  if not Panel1.ClientToBitmap(pt) then Exit;
  dx := pt.X - clickPoint.X;
  dy := pt.Y - clickPoint.Y;
  clickPoint := pt;

  if moveType = mtNone then
  begin
    //just update the cursor
    layer := layeredImage32.GetLayerAt(pt);
    if Assigned(layer) and (layer is TButtonDesignerLayer32) then
      Panel1.Cursor := layer.CursorId
    else if PointOnPath(pt, QSpline(ButtonsToPath)) then
      Panel1.Cursor := crSizeAll
    else if Assigned(layer) then
      Panel1.Cursor := layer.CursorId
    else
      Panel1.Cursor := crDefault;
    Exit;
  end;

  if moveType = mtLayer then TExHitTestLayer32(clickedLayer).Offset(dx, dy)
  else if moveType = mtOneButton then clickedLayer.Offset(dx, dy)
  else layeredImage32.OffsetGroup(1, dx, dy); //clickType = ctAllButtons

  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  moveType := mtNone;
  //re-enable Panel1 zoom and scroll
  Panel1.BitmapProperties.ZoomAndScrollEnabled := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuDeleteLayerClick(Sender: TObject);
begin
  if Assigned(clickedLayer) and (clickedLayer is TExHitTestLayer32) then
  begin
    layeredImage32.DeleteLayer(clickedLayer.Index);
    clickedLayer := nil;
    PaintLayeredImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.CopytoClipboard1Click(Sender: TObject);
begin
  layeredImage32.GetMergedImage(true).CopyToClipBoard;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuHideControlsClick(Sender: TObject);
begin
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  GetCursorPos(popupPoint);
  popupPoint := Panel1.ScreenToClient(popupPoint);
  Panel1.ClientToBitmap(popupPoint);
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
