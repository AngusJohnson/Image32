unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, StdCtrls,
  BitmapPanels, Image32, Image32_Layers, Image32_SmoothPath;

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
    mnuMakePolygon: TMenuItem;
    Label2: TLabel;
    edWidth: TEdit;
    UpDown1: TUpDown;
    N3: TMenuItem;
    mnuRotateButtons: TMenuItem;
    N5: TMenuItem;
    mnuSmoothSym: TMenuItem;
    mnuSmoothAsym: TMenuItem;
    mnuSharpWithHdls: TMenuItem;
    mnuSharpNoHdls: TMenuItem;
    lblPenColor: TLabel;
    lblFillColor: TLabel;
    edPenColor: TEdit;
    edFillColor: TEdit;
    Shape1: TShape;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure pnlMainDblClick(Sender: TObject);
    procedure mnuMakePolylineClick(Sender: TObject);
    procedure mnuDeleteAllButtonControlsClick(Sender: TObject);
    procedure mnuHideControlsClick(Sender: TObject);
    procedure mnuEditLayerClick(Sender: TObject);
    procedure mnuDeleteLayerClick(Sender: TObject);
    procedure mnuDeleteButtonClick(Sender: TObject);
    procedure edWidthChange(Sender: TObject);
    procedure mnuRotateButtonsClick(Sender: TObject);
    procedure mnuSharpNoHdlsClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure edPenColorChange(Sender: TObject);
  private
    layeredImage32: TLayeredImage32;
    clickPoint    : TPoint;
    rotatePoint   : TPoint;
    moveType      : TMoveType;
    clickedLayer  : TLayer32;
    designLayer   : TDesignerLayer32;
    buttonPath    : TSmoothPath;
    btnPathRegion : TArrayOfArrayOfPointD;
    dashes        : TArrayOfInteger;
    disableTypeChange: Boolean;
    procedure RefreshButtonGroupFromPath;
    procedure UpdateButtonGroupFromPath;
    function HitTestButtonPath(const pt: TPoint): Boolean;
    function ClearRotateButton: Boolean;
    procedure UpdatePanelBitmap;
    procedure BeginPanelPaint(Sender: TObject);
    procedure PanelKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    { Public declarations }
  end;


  TLineLayer32 = class(TSmoothPathLayer32)
  public
    procedure SetSmoothPath(const smoothPath: TSmoothPath);
  end;

  TPolygonLayer32 = class(TSmoothPathLayer32)
  public
    procedure SetSmoothPath(const smoothPath: TSmoothPath);
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
var
  buttonSizes: array [boolean] of integer;
  hitTestWidth: integer;
  lineWidth: integer;
  fillColor, penColor: TColor32;

type

  TExButtonDesignerLayer32 = class(TButtonDesignerLayer32)
  public
    PointType: TSmoothType; //point type for curve end controls
  end;

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetFillColor: TColor32;
begin
  result := StrToInt64Def(FrmMain.edFillColor.Text, clNone32);
end;
//------------------------------------------------------------------------------

function GetPenColor: TColor32;
begin
  result := StrToInt64Def(FrmMain.edPenColor.Text, clBlack32);
end;

//------------------------------------------------------------------------------
// TLineLayer32
//------------------------------------------------------------------------------

procedure TLineLayer32.SetSmoothPath(const smoothPath: TSmoothPath);
var
  rec: TRect;
  flatPath: TArrayOfPointD;
begin
  flatPath := smoothPath.FlattenedPath;
  //calculate and assign hit test regions
  self.HitTestRegions := InflateOpenPath(OpenPathToFlatPolygon(flatPath),
    lineWidth + hitTestWidth, jsAuto, esRound);

  self.SmoothPath.Assign(smoothPath);

  //draw flattened line
  rec := GetBounds(HitTestRegions);
  self.SetBounds(rec);
  Image.Clear;
  flatPath := OffsetPath(flatPath, -Left, -Top);
  DrawLine(Image, flatPath, lineWidth, GetPenColor, esRound);
  CursorId := crSizeAll;
end;

//------------------------------------------------------------------------------
// TPolygonLayer32
//------------------------------------------------------------------------------

procedure TPolygonLayer32.SetSmoothPath(const smoothPath: TSmoothPath);
var
  rec: TRect;
  flattened: TArrayOfPointD;
begin
  //calculate and assign hit test regions
  flattened := smoothPath.FlattenedPath;
  //nb: UnionPolygon fixes up any self-intersections
  self.HitTestRegions := UnionPolygon(flattened, frEvenOdd);
  HitTestRegions :=
    InflatePolygons(HitTestRegions, (lineWidth + hitTestWidth) *0.5);

  self.SmoothPath.Assign(smoothPath);

  //draw polygon
  rec := GetBounds(HitTestRegions);
  self.SetBounds(rec);
  flattened := OffsetPath(flattened, -Left, -Top);
  Image.Clear;
  DrawPolygon(Image, flattened, frNonZero, GetFillColor);
  DrawLine(Image, flattened, lineWidth, GetPenColor, esClosed);
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
  lineWidth          := strtoint(edWidth.text);
  hitTestWidth       := DPI(5);
  buttonSizes[false] := DPI(9);
  buttonSizes[true]  := DPI(8);
  fillColor := $88FFFF66;
  penColor := clNavy32;
  edFillColor.Text  := '$' + inttohex(fillColor, 8);
  edPenColor.Text   := '$' + inttohex(penColor, 8);

  with pnlTop do
  begin
    Bitmap.Width := RectWidth(InnerClientRect);
    Bitmap.Height := RectHeight(InnerClientRect);
    BitmapProperties.AutoCenter := false;
  end;
  edPenColorChange(nil);

  setLength(dashes, 2);
  dashes[0] := 5; dashes[1] := 5;
  buttonPath := TSmoothPath.Create;

  //SETUP THE DISPLAY PANEL
  pnlMain.BorderWidth := DPI(16);
  pnlMain.BevelInner := bvLowered;
  //set pnlMain.Tabstop -> true to enable keyboard controls
  pnlMain.TabStop := true;
  pnlMain.FocusedColor := clGradientInactiveCaption;
  pnlMain.BitmapProperties.Scale := 1;
  pnlMain.BitmapProperties.OnBeginPaint := BeginPanelPaint;
  pnlMain.BitmapProperties.OnKeyDown := PanelKeyDown;

  //SETUP LAYEREDIMAGE32
  rec := pnlMain.InnerClientRect;
  layeredImage32 := TLayeredImage32.Create(RectWidth(rec), RectHeight(rec));
  //layeredImage32.BackgroundColor := clWhite32;

  //hatched image layer is TDesignerLayer32 so it's 'non-clickable'
  layer := layeredImage32.AddNewLayer(TDesignerLayer32, 'hatched background');
  layer.SetSize(layeredImage32.Width, layeredImage32.Height);
  HatchBackground(layer.Image, $FFF0F0F0, clWhite32);

  //and another designer layer
  designLayer := TDesignerLayer32(layeredImage32.AddNewLayer(
    TDesignerLayer32, 'design layer'));
  designLayer.SetSize(layeredImage32.Width, layeredImage32.Height);

  Screen.Cursors[crRotate] :=
    LoadImage(hInstance, 'ROTATE', IMAGE_CURSOR, 32,32,0);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  buttonPath.Free;
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.RefreshButtonGroupFromPath;
var
  i, fig,lig: integer;
begin
  //Group '1' is always the button designer group
  fig := layeredImage32.GetFirstInGroupIdx(1);
  if fig < 0 then Exit;
  lig := layeredImage32.GetLastInGroupIdx(1);

  for i := fig to lig do
    layeredImage32[i].PositionCenteredAt(buttonPath[i-fig]);

  //update hittest region too
  if buttonPath.Count > 1 then
    btnPathRegion := InflateOpenPath(OpenPathToFlatPolygon(
      buttonPath.FlattenedPath), lineWidth + hitTestWidth, jsRound)
  else
    btnPathRegion := nil;

  UpdatePanelBitmap;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.UpdateButtonGroupFromPath;
var
  i: integer;
  hideNextLayer, IsCurveEnd: Boolean;
  layer: TLayer32;
  btnColor: TColor32;
const
  //There are 2 types of CBezier buttons controls:
  //1. curve ends and 2. curve shape controls (aka handles)
  btnColors: array [boolean] of TColor32 = (clBlue32, clGreen32);
  buttonOpts: array [boolean] of TButtonOptions = ([], [boSquare]);
begin
  clickedLayer := nil;
  layeredImage32.DeleteGroup(1);

  if buttonPath.Count = 0  then
  begin
    UpdatePanelBitmap;
    Exit;
  end;

  if buttonPath.Count = 1 then
    btnColor := clLime32 else
    btnColor := btnColors[true];
  layer := StartButtonGroup(layeredImage32, Point(buttonPath[0]),
    btnColor, buttonSizes[true], buttonOpts[true], TExButtonDesignerLayer32);
  with TExButtonDesignerLayer32(layer) do
  begin
    PointType := buttonPath.PointTypes[0];
    hideNextLayer := (PointType = stSharpNoHdls);
  end;

  for i := 1 to buttonPath.Count -1 do
  begin
      IsCurveEnd := i mod 3 = 0;
      if i = buttonPath.Count -1 then
    btnColor := clLime32 else
    btnColor := btnColors[IsCurveEnd];
      layer := AddToButtonGroup(layeredImage32, 1, Point(buttonPath[i]),
        btnColor, buttonSizes[IsCurveEnd], buttonOpts[IsCurveEnd]);
      with TExButtonDesignerLayer32(layer) do
      begin
        PointType := buttonPath.PointTypes[i];
        Visible := not hideNextLayer;
        if IsCurveEnd and (PointType = stSharpNoHdls) then
        begin
          layeredImage32[layer.Index -1].Visible := false;
          hideNextLayer := True;
        end
        else hideNextLayer := false;
      end;
  end;

  //update hittest region too
  if buttonPath.Count > 1 then
    btnPathRegion := InflateOpenPath(OpenPathToFlatPolygon(
      buttonPath.FlattenedPath), Max(lineWidth, hitTestWidth), jsRound)
  else
    btnPathRegion := nil;

  UpdatePanelBitmap;
end;
//------------------------------------------------------------------------------

function TFrmMain.HitTestButtonPath(const pt: TPoint): Boolean;
begin
  //hittest for button path
  result := PointInPolygons(PointD(pt), btnPathRegion, frEvenOdd);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeleteButtonClick(Sender: TObject);
begin
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then Exit;
  buttonPath.DeleteLast;
  UpdateButtonGroupFromPath;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeleteAllButtonControlsClick(Sender: TObject);
begin
  if layeredImage32.TopLayer.GroupId <> 1 then Exit;
  layeredImage32.DeleteGroup(1);

  buttonPath.Clear;
  clickedLayer := nil;
  btnPathRegion := nil;
  UpdatePanelBitmap;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.UpdatePanelBitmap;
var
  i, d: integer;
  rec: TRect;
  path: TArrayOfPointD;
begin
  if not Active then Exit; //just avoids several calls in FormCreate

  //Repaints the designer layer
  designLayer.Image.Clear;

  //designer (non-rotation) buttons
  //though most of this is just arrow head stuff
  path := buttonPath.FlattenedPath;
  if Assigned(path) then
  begin
    //draw plain old flattened button path
    DrawLine(designLayer.Image, path, lineWidth, GetPenColor, esRound);
    //display control lines
    DrawSmoothPathOnDesigner(buttonPath, designLayer);
  end;

  //outline selected button
  if Assigned(clickedLayer) and (clickedLayer.GroupId = 1) then
  begin
    rec.TopLeft := Point(clickedLayer.MidPoint);
    rec.BottomRight := rec.TopLeft;
    i := Max(DefaultButtonSize, linewidth) div 2 + hitTestWidth;
    Windows.InflateRect(rec, i, i);
    DrawPolygon(designLayer.Image, Ellipse(rec), frEvenOdd, $40AAAAAA);
    DrawLine(designLayer.Image, Ellipse(rec), 1, $AAFF0000, esClosed);
  end;

  //rotation (always group 2)
  i := layeredImage32.GetFirstInGroupIdx(2);
  if i > 0 then
  begin
    DrawButton(designLayer.Image,
      PointD(rotatePoint), DefaultButtonSize, clSilver32);
    d := Round(Distance(Point(layeredImage32.TopLayer.MidPoint), rotatePoint));
    with rotatePoint do rec := Rect(X - d, Y - d, X + d, Y + d);
    designLayer.DrawEllipse(rec);
  end;

  //display dashed hit-test outline for selected layer
  if Assigned(clickedLayer) and (clickedLayer is TSmoothPathLayer32) then
    DrawDashedLine(designLayer.Image,
      TSmoothPathLayer32(clickedLayer).HitTestRegions,
      dashes, nil, 1, clRed32, esClosed);

  //and update Statusbar too
  if assigned(clickedLayer) and (clickedLayer.GroupId = 1) then
  begin
    with clickedLayer.MidPoint do
    begin
      if (clickedLayer.IndexInGroup mod 3 = 0) then
      begin
        case TExButtonDesignerLayer32(clickedLayer).PointType of
          stSmoothSym: StatusBar1.SimpleText :=
              format(' Smooth - symmetric (%1.0n,%1.0n)',[X,Y]);
          stSmoothAsym: StatusBar1.SimpleText :=
              format(' Smooth - asymmetric (%1.0n,%1.0n)',[X,Y]);
          stSharpWithHdls: StatusBar1.SimpleText :=
              format(' Sharp - with handles (%1.0n,%1.0n)',[X,Y]);
          else
            StatusBar1.SimpleText :=
              format(' Sharp - without handles (%1.0n,%1.0n)',[X,Y]);
        end;
      end else
      StatusBar1.SimpleText := format(' (%1.0n,%1.0n)',[X,Y]);
    end;
  end else
    StatusBar1.SimpleText := '';

  pnlMain.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.BeginPanelPaint(Sender: TObject);
var
  dc: HDC;
begin
  //update pnlMain.Bitmap with layeredImage32's merged image
  {$IFDEF SETSIZE} 
  pnlMain.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  {$ELSE}
  pnlMain.Bitmap.Width := layeredImage32.Width;
  pnlMain.Bitmap.Height := layeredImage32.Height;
  {$ENDIF}
  dc := pnlMain.Bitmap.Canvas.Handle;
  layeredImage32.GetMergedImage(mnuHideControls.Checked).CopyToDc(dc,0,0,false);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuMakePolylineClick(Sender: TObject);
var
  polygonLayer: TPolygonLayer32;
  lineLayer: TLineLayer32;
begin
  if not (layeredImage32.TopLayer is TButtonDesignerLayer32) then Exit;

  //create a 'completed' path (either polyline or polygon) from the buttons
  if Sender = mnuMakePolygon then
  begin
    if buttonPath.Count < 3 then Exit;
    layeredImage32.DeleteGroup(1);
    polygonLayer :=
      TPolygonLayer32(layeredImage32.AddNewLayer(TPolygonLayer32, ''));
    polygonLayer.SetSmoothPath(buttonPath);
    designLayer.BringForward(polygonLayer.Index);
  end else
  begin
    if buttonPath.Count < 2 then Exit;
    layeredImage32.DeleteGroup(1);
    lineLayer :=
      TLineLayer32(layeredImage32.AddNewLayer(TLineLayer32, ''));
    lineLayer.SetSmoothPath(buttonPath);
    designLayer.BringForward(lineLayer.Index);
  end;
  buttonPath.Clear;
  clickedLayer := nil;
  btnPathRegion := nil;
  UpdatePanelBitmap;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuEditLayerClick(Sender: TObject);
begin
  if not assigned(clickedLayer) or
    not (clickedLayer is TSmoothPathLayer32) then Exit;

  //When 'editing' a layer (line or polygon), delete any buttons first
  if layeredImage32.GroupCount(1) > 0 then
    layeredImage32.DeleteGroup(1);

  buttonPath.Assign(TSmoothPathLayer32(clickedLayer).SmoothPath);
  layeredImage32.DeleteLayer(clickedLayer.Index);
  UpdateButtonGroupFromPath;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuRotateButtonsClick(Sender: TObject);
var
  rec: TRect;
  pt: TPointD;
  i: integer;
  layer: TLayer32;
begin
  if buttonPath.Count < 2 then Exit;
  rec := GetBounds(buttonPath.CtrlPoints);
  rotatePoint := MidPoint(rec);
  i := (RectWidth(rec) + RectHeight(rec)) div 4;
  pt := PointD(rotatePoint.X,rotatePoint.Y - i);
  layer := StartButtonGroup(layeredImage32, Point(pt), clRed32,
    DPI(9), [], TButtonDesignerLayer32);
  layer.CursorId := crRotate;
  UpdatePanelBitmap;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainDblClick(Sender: TObject);
begin
  //double click adds a button control layer
  buttonPath.Add(PointD(clickPoint), buttonPath.LastType);
  UpdateButtonGroupFromPath;
  clickedLayer := layeredImage32.TopLayer;
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

  //unless clicking the rotation button, delete a rotation button
  with layeredImage32 do
    if (TopLayer.GroupId = 2) and (TopLayer <> clickedLayer) then
      DeleteGroup(2);

  if not Assigned(clickedLayer) then
  begin
    if HitTestButtonPath(pt) then moveType := mtAllButtons;
  end
  else if (clickedLayer is TButtonDesignerLayer32) then
  begin
    if clickedLayer.GroupId = 1 then
      moveType := mtOneButton else
      moveType := mtRotateButton;
  end
  else
    moveType := mtLayer; //ie a completed layer (line or polygon)

  //disable pnlMain scrolling if we're about to move a layer
  pnlMain.BitmapProperties.ZoomAndScrollEnabled := moveType = mtNone;

  UpdatePanelBitmap;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx,dy: integer;
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
    else if HitTestButtonPath(pt) then
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
  case moveType of
    mtAllButtons:
      begin
        buttonPath.Offset(dx, dy);
        RefreshButtonGroupFromPath;
      end;
    mtOneButton:
      begin
        buttonPath[clickedLayer.IndexInGroup] := PointD(pt);
        RefreshButtonGroupFromPath;
      end;
    mtRotateButton:
      begin
        clickedLayer.Offset(dx, dy);
        angle := GetAngle(clickPoint, rotatePoint, pt);
        buttonPath.Rotate(PointD(rotatePoint), angle);
        RefreshButtonGroupFromPath;
      end;
    else
    begin
      clickedLayer.Offset(dx, dy);
      UpdatePanelBitmap;
    end;
  end;
  clickPoint := pt;
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
  if Assigned(clickedLayer) and (clickedLayer is TSmoothPathLayer32) then
  begin
    layeredImage32.DeleteLayer(clickedLayer.Index);
    clickedLayer := nil;
    UpdatePanelBitmap;
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
  pnlMain.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.edWidthChange(Sender: TObject);
begin
  lineWidth := Max(1, Min(25, strtoint(edWidth.text)));
  if Assigned(clickedLayer) and (clickedLayer is TLineLayer32) then
    with TLineLayer32(clickedLayer) do SetSmoothPath(SmoothPath);
  UpdatePanelBitmap;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.edPenColorChange(Sender: TObject);
begin
  if Length(FrmMain.edPenColor.Text) = 9 then
    penColor := TColor32(StrToInt64Def(FrmMain.edPenColor.Text, penColor));
  if Length(FrmMain.edFillColor.Text) = 9 then
    fillColor := TColor32(StrToInt64Def(FrmMain.edFillColor.Text, fillColor));
  UpdatePanelBitmap;

  Shape1.Brush.Color := RGBColor(BlendToOpaque(Color32(clBtnFace), fillColor));
  Shape1.Pen.Width := 3;
  Shape1.Pen.Color := RGBColor(BlendToOpaque(Color32(clBtnFace), penColor));
end;
//------------------------------------------------------------------------------

procedure TFrmMain.PopupMenu1Popup(Sender: TObject);
var
  nodeTypeEnabled: Boolean;
begin
  mnuMakePolyline.Enabled := (buttonPath.Count > 0);
  mnuMakePolygon.Enabled := mnuMakePolyline.Enabled;
  mnuEditLayer.Enabled :=
    Assigned(clickedLayer) and (clickedLayer is TSmoothPathLayer32);
  mnuDeleteLayer.Enabled := mnuEditLayer.Enabled;
  mnuRotateButtons.Enabled := buttonPath.Count > 1;
  mnuDeleteButton.Enabled := buttonPath.Count > 0;
  mnuDeleteAllButtonControls.Enabled := mnuDeleteButton.Enabled;

  nodeTypeEnabled := Assigned(clickedLayer) and
    (clickedLayer.GroupId = 1) and (clickedLayer.IndexInGroup mod 3 = 0);
  mnuSmoothSym.Enabled := nodeTypeEnabled;
  mnuSmoothAsym.Enabled := nodeTypeEnabled;
  mnuSharpWithHdls.Enabled := nodeTypeEnabled;
  mnuSharpNoHdls.Enabled := nodeTypeEnabled;
  if nodeTypeEnabled then
  begin
    disableTypeChange := true;
    try
      if Assigned(clickedLayer) then
        case TExButtonDesignerLayer32(clickedLayer).PointType of
          stSmoothSym: mnuSmoothSym.Checked := true;
          stSmoothAsym: mnuSmoothAsym.Checked := true;
          stSharpWithHdls: mnuSharpWithHdls.Checked := true;
          stSharpNoHdls: mnuSharpNoHdls.Checked := true;
        end
      else mnuSmoothSym.Checked := true;
    finally
      disableTypeChange := false;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuSharpNoHdlsClick(Sender: TObject);
var
  i, clicklayerIdxInGroup: integer;
  newType, oldType: TSmoothType;
begin
   //changing vertex point type
  if disableTypeChange then Exit;
  if not assigned(clickedLayer) or (clickedLayer.GroupId <> 1) then Exit;

  oldType := TExButtonDesignerLayer32(clickedLayer).PointType;
  if Sender = mnuSmoothSym then newType := stSmoothSym
  else if Sender = mnuSmoothAsym then newType := stSmoothAsym
  else if Sender = mnuSharpWithHdls then newType := stSharpWithHdls
  else newType := stSharpNoHdls;
  if newType = oldType then Exit;

  clicklayerIdxInGroup := clickedLayer.IndexInGroup;
  buttonPath.PointTypes[clicklayerIdxInGroup] := newType;
  UpdateButtonGroupFromPath;
  i := layeredImage32.GetFirstInGroupIdx(1) + clicklayerIdxInGroup;
  clickedLayer := layeredImage32[i];
  UpdatePanelBitmap;
end;
//------------------------------------------------------------------------------

function TFrmMain.ClearRotateButton: Boolean;
begin
  Result := layeredImage32.GroupCount(2) > 0;
  if Result then layeredImage32.DeleteGroup(2);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.PanelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i, dist, dx, dy: integer;
  angle: double;
  pt: TPointD;
begin
  //keyboard control of buttons
  case Key of
    VK_ESCAPE:
      begin
        clickedLayer := nil;
        ClearRotateButton;
      end;
    VK_TAB: // Ctrl+Tab => changes button selection
      begin
        if not (ssCtrl in Shift) then Exit;
        ClearRotateButton;
        if (ssShift in Shift) then
        begin
          if Assigned(clickedLayer) then i := clickedLayer.Index -1
          else i := layeredImage32.Count -1;
          while (i >= 0) and (not layeredImage32[i].Visible or
            (layeredImage32[i].GroupId <> 1)) do dec(i);
          if (i < 0) then
          begin
            if not Assigned(clickedLayer) then Exit;
            i := layeredImage32.Count -1;
            while (i > 0) and (not layeredImage32[i].Visible or
              (layeredImage32[i].GroupId <> 1)) do dec(i);
          end;
        end else
        begin
          if Assigned(clickedLayer) then i := clickedLayer.Index +1
          else i := 0;
          while (i < layeredImage32.Count) and
            (not layeredImage32[i].Visible or
            (layeredImage32[i].GroupId <> 1)) do inc(i);
          if (i = layeredImage32.Count) then
          begin
            if not Assigned(clickedLayer) then Exit;
            i := 0;
            while (i < clickedLayer.Index) and
              (not layeredImage32[i].Visible or
              (layeredImage32[i].GroupId <> 1)) do inc(i);
          end;
        end;
        clickedLayer := layeredImage32[i];
      end;
    VK_LEFT .. VK_DOWN:
      begin
        if (clickedLayer = nil) then Exit;
        ClearRotateButton;
        if ssCtrl in Shift then dist := 10 else dist := 1;
        if clickedLayer is TButtonDesignerLayer32 then
        begin
          pt := clickedLayer.MidPoint;
          clickPoint := Point(pt);
          case Key of
            VK_LEFT: pt.X := pt.X - dist;
            VK_RIGHT: pt.X := pt.X + dist;
            VK_UP: pt.Y := pt.Y - dist;
            VK_DOWN: pt.Y := pt.Y + dist;
          end;
          if clickedLayer.GroupId = 2 then
          begin
            dx := 0; dy := 0;
            case Key of
              VK_LEFT: dx := -dist;
              VK_RIGHT: dx := dist;
              VK_UP: dy := -dist;
              VK_DOWN: dy := dist;
            end;
            if clickedLayer.Name = 'Rotate' then
            begin
              clickedLayer.Offset(dx, dy);
              angle := GetAngle(clickPoint, rotatePoint, Point(pt));
              buttonPath.Rotate(PointD(rotatePoint), angle);
              RefreshButtonGroupFromPath;
            end
            else Exit;
          end else
          begin
            buttonPath[clickedLayer.IndexInGroup] := pt;
          end;
          RefreshButtonGroupFromPath;
        end else
        begin
           case Key of
            VK_LEFT: clickedLayer.Offset(-dist, 0);
            VK_RIGHT: clickedLayer.Offset(dist, 0);
            VK_UP: clickedLayer.Offset(0, -dist);
            VK_DOWN: clickedLayer.Offset(0, dist);
          end;
        end;
      end;
    else
      Exit;
  end;
  UpdatePanelBitmap;
  Key := 0;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
