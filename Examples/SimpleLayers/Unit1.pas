unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, Image32, Image32_Layers, BitmapPanels,
  Vcl.Dialogs;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    mnuHideControls: TMenuItem;
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    SaveDialog1: TSaveDialog;
    View1: TMenuItem;
    mnuSave: TMenuItem;
    N2: TMenuItem;
    CopytoClipboard1: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuBringToFront: TMenuItem;
    mnuSendToBack: TMenuItem;
    N3: TMenuItem;
    mnuDeleteLayer: TMenuItem;
    AddEllipse1: TMenuItem;
    N4: TMenuItem;
    AddRectangle1: TMenuItem;
    AddText1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuHideControlsClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure mnuBringToFrontClick(Sender: TObject);
    procedure mnuSendToBackClick(Sender: TObject);
    procedure mnuDeleteLayerClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure AddEllipse1Click(Sender: TObject);
    procedure AddRectangle1Click(Sender: TObject);
    procedure AddText1Click(Sender: TObject);
  private
    masterImageList: TImageList32;
    layeredImage32: TLayeredImage32;
    activeLayerClicked: Boolean;
    activeLayer: TLayer32;
    buttonMovingLayer: TLayer32;
    popupPoint: TPoint;
    clickPoint: TPoint;
    function AddRectangle(const pt: TPoint): TLayer32;
    function AddEllipse(const pt: TPoint): TLayer32;
    function AddText(const pt: TPoint): TLayer32;
    procedure DeleteButtons;
    procedure AddButtons(layer: TLayer32);
    procedure PaintLayeredImage;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R image.res}

uses
  Image32_BMP, Image32_PNG, Image32_JPG, Image32_Draw, Image32_Vector,
  Image32_Extra, Image32_Text;

const
  margin = 100;

//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  layer: TLayer32;
  rec: TRect;
  image: TImage32;
begin
  //SETUP THE DISPLAY PANEL
  Panel1.BorderWidth := DPI(16);
  Panel1.BevelInner := bvLowered;
  //set Panel1.Tabstop := true to enable keyboard controls
  Panel1.TabStop := true;
  Panel1.FocusedColor := clGradientInactiveCaption;
  Panel1.BitmapProperties.Scale := 1;

  //SETUP THE MASTERIMAGE LIST (containing a list of off-screen images)
  masterImageList := TImageList32.Create;
  //and add a dummy image to masterImageList so masterImageList's images
  //directly correlate with layeredImage32's display images (given that
  //layeredImage32's first image will simply be a background image)
  masterImageList.Add(TImage32.Create);

  //SETUP LAYEREDIMAGE32

  rec := Panel1.InnerClientRect;
  layeredImage32 := TLayeredImage32.Create(RectWidth(rec), RectHeight(rec));
  //nb: we don't need to worry about BackgroundColor as we're
  //going to add a hatched image for the background instead.

  layer := layeredImage32.AddNewLayer(TNonClickableDesignLayer32, 'hatched background');
  layer.SetSize(layeredImage32.Width, layeredImage32.Height);
  HatchBackground(layer.Image, $FFF0F0F0, clWhite32);

  //LOAD A SINGLE IMAGE (OTHERS CAN BE ADDED LATER WITH A RIGHT-CLICK).
  image := TImage32.Create;
  image.LoadFromResource('UNION_JACK', 'BMP');
  masterImageList.Add(image);

  layer := layeredImage32.AddNewLayer('');
  layer.image.Assign(image);
  layer.PositionCenteredAt(Point(layeredImage32.MidPoint));

  //DISPLAY WHAT'S ON SHOW
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

function TForm1.AddRectangle(const pt: TPoint): TLayer32;
var
  image: TImage32;
  rec: TRect;
  path: TArrayOfPointD;
begin
  image := TImage32.Create(192,192);
  rec := image.Bounds;
  InflateRect(rec, -10, -10);
  path := Rectangle(rec);
  DrawPolygon(image, path, frNonZero, $AA00FFFF);
  DrawLine(image, path, 5, clNavy32, esClosed);
  masterImageList.Add(image);
  Result := layeredImage32.AddNewLayer('rectangle');
  Result.image.Assign(image);
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

function TForm1.AddEllipse(const pt: TPoint): TLayer32;
var
  image: TImage32;
  rec: TRect;
  path: TArrayOfPointD;
begin
  image := TImage32.Create(256,192);
  rec := image.Bounds;
  InflateRect(rec, -10, -10);
  path := Ellipse(rec);
  DrawPolygon(image, path, frNonZero, $AAFFFF00);
  DrawLine(image, path, 5, clMaroon32, esClosed);
  masterImageList.Add(image);
  Result := layeredImage32.AddNewLayer('ellipse');
  Result.image.Assign(image);
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

function TForm1.AddText(const pt: TPoint): TLayer32;
var
  image: TImage32;
  lf: TLogfont;
begin
  image := TImage32.Create(312,128);
  lf := DefaultLogfont;
  lf.lfHeight := -96;
  lf.lfFaceName := 'Impact';
  DrawText(image, 10, 100, 'Testing', GetFontInfo(lf));
  masterImageList.Add(image);
  Result := layeredImage32.AddNewLayer('text');
  Result.image.Assign(image);
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

procedure TForm1.PaintLayeredImage;
var
  dc: HDC;
begin
  //copy the merged layeredImage32 to Panel1 (+/- designer layers)
  Panel1.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  //Panel1.ClearBitmap;
  dc := Panel1.Bitmap.Canvas.Handle;
  layeredImage32.GetMergedImage(mnuHideControls.Checked).CopyToDc(dc,0,0,false);
  Panel1.Refresh;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  masterImageList.Free;
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.DeleteButtons;
begin
  if not assigned(activeLayer) then Exit;
  with layeredImage32 do
    DeleteGroup(TopLayer.GroupIndex);
  activeLayer.CursorId := crDefault;
  activeLayer := nil;
end;
//------------------------------------------------------------------------------

procedure TForm1.AddButtons(layer: TLayer32);
begin
  activeLayer := layer;
  CreateSizingBtnsGroup(activeLayer, ssEdgesAndCorners, clBlue32, DPI(8), []);
  activeLayer.CursorId := crSizeAll;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  clickedLayer: TLayer32;
begin
  buttonMovingLayer := nil;
  //convert Panel1 client coordinates to bitmap coordinates
  pt := Types.Point(X,Y);
  if not Panel1.ClientToBitmap(pt) then Exit;

  //find out which layer was clicked (if any)
  clickPoint := pt;
  clickedLayer := layeredImage32.GetLayerAt(pt);
  //disable panel1 scrolling if we're about to move an image or a button
  panel1.BitmapProperties.ZoomAndScrollEnabled := not Assigned(clickedLayer);

  //if a control button was clicked then get ready to move it
  if Assigned(clickedLayer) and (clickedLayer.Name = 'button') then
  begin
    buttonMovingLayer := clickedLayer;
    Exit;
  end;

  //if anything but the activelayer was clicked then delete buttons
  if assigned(activeLayer) and  (activeLayer <> clickedLayer) then
    DeleteButtons;

  if Assigned(clickedLayer) then
  begin
    //assign a new activeLayer
    if not Assigned(activeLayer) then AddButtons(clickedLayer);
    activeLayerClicked := true;
  end;

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

  if activeLayerClicked then
  begin
    //move the image and its buttons too
    activeLayer.Offset(dx, dy);
    with layeredImage32 do OffsetGroup(1, dx, dy);
  end
  else if assigned(buttonMovingLayer) then
  begin
    //moving a 'resize' button
    if buttonMovingLayer.CursorId = crSizeNS then
      buttonMovingLayer.Offset(0, dy)
    else if buttonMovingLayer.CursorId = crSizeWE then
      buttonMovingLayer.Offset(dx, 0)
    else
      buttonMovingLayer.Offset(dx, dy);
    UpdateSizingGroup(activeLayer, buttonMovingLayer,
      masterImageList[activeLayer.Index]);
  end else
  begin
    //neither image nor button clicked so just update the cursor
    layer := layeredImage32.GetLayerAt(pt);
    if Assigned(layer) then
      Panel1.Cursor := layer.CursorId else
      Panel1.Cursor := crDefault;
    Exit;
  end;
  clickPoint := pt;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  activeLayerClicked := false;
  buttonMovingLayer := nil;
  //re-enable Panel1 zoom and scroll
  Panel1.BitmapProperties.ZoomAndScrollEnabled := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuHideControlsClick(Sender: TObject);
begin
  layeredImage32.Invalidate;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute(handle) then
    layeredImage32.GetMergedImage(true).SaveToFile(SaveDialog1.FileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.CopytoClipboard1Click(Sender: TObject);
begin
  layeredImage32.GetMergedImage(true).CopyToClipBoard;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuBringToFrontClick(Sender: TObject);
var
  idx: integer;
begin
  idx := activeLayer.Index;
  if activeLayer.BringForward(idx +1) then
  begin
    masterImageList.Move(idx, idx +1);
    PaintLayeredImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuSendToBackClick(Sender: TObject);
var
  idx: integer;
begin
  idx := activeLayer.Index;
  if idx = 1 then Exit; //don't send an image behind the hatched background
  if activeLayer.SendBack(idx -1) then
  begin
    masterImageList.Move(idx, idx-1);
    PaintLayeredImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuDeleteLayerClick(Sender: TObject);
begin
  if not assigned(activeLayer) then Exit;
  masterImageList.Delete(activeLayer.Index);
  with layeredImage32 do DeleteGroup(TopLayer.GroupIndex);
  layeredImage32.DeleteLayer(activeLayer.Index);
  activeLayer := nil;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  mnuBringToFront.Enabled := assigned(activeLayer) and
    (activeLayer.Index < masterImageList.Count -1);
  mnuSendToBack.Enabled := assigned(activeLayer) and (activeLayer.Index > 1);
  mnuDeleteLayer.Enabled := assigned(activeLayer);

  GetCursorPos(popupPoint);
  popupPoint := Panel1.ScreenToClient(popupPoint);
  Panel1.ClientToBitmap(popupPoint);
end;
//------------------------------------------------------------------------------

procedure TForm1.AddEllipse1Click(Sender: TObject);
begin
  DeleteButtons;
  AddButtons(AddEllipse(popupPoint));
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.AddRectangle1Click(Sender: TObject);
begin
  DeleteButtons;
  AddButtons(AddRectangle(popupPoint));
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.AddText1Click(Sender: TObject);
begin
  DeleteButtons;
  AddButtons(AddText(popupPoint));
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

end.
