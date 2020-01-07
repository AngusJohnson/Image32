unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, Image32, Image32_Layers, BitmapPanels,
  Dialogs;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    mnuShowControls: TMenuItem;
    mnuShowAll: TMenuItem;
    mnuShowImage: TMenuItem;
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    View1: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    N2: TMenuItem;
    CopytoClipboard1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuShowControlsClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure CopytoClipboard1Click(Sender: TObject);
  private
    layeredImage32: TLayeredImage32;
    buttonMovingLayer, hatchedLayer: TLayer32;
    masterLayer, transformLayer: TLayer32;
    designLayer: TDesignerLayer32;
    fClickPoint: TPoint;
    fImageIsClicked: Boolean;
    fSplinePts: TArrayOfPointD;
    fMargin: integer;
    fButtonGroupId: integer;
    procedure ResetSplinePts;
    procedure UpdateLayeredImage(transform: Boolean);
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
  Image32_Extra, Image32_Transform;

//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  //SETUP THE DISPLAY PANEL

  Panel1.BorderWidth := DPI(16);
  Panel1.BevelInner := bvLowered;
  Panel1.TabStop := true; //enables keyboard control
  Panel1.FocusedColor := clGradientInactiveCaption;
  Panel1.BitmapProperties.Scale := 1;

  //SETUP THE LAYERED IMAGE
  DefaultButtonSize := DPI(10);
  fMargin := DPI(50);

  layeredImage32 := TLayeredImage32.Create;
  //width & height will be set in UpdateLayeredImage method below

  //Layer 0: 'hatched' layer to highlight transparency
  hatchedLayer :=
    layeredImage32.AddNewLayer(TDesignerLayer32, 'background - hatched');

  //Layer 1: hidden layer contains the starting image
  masterLayer := layeredImage32.AddNewLayer(TDesignerLayer32, 'master - hidden');
  masterLayer.image.LoadFromResource('UNION_JACK', 'BMP');
  masterLayer.Visible := false;

  //Layer 2: for the transformed image
  transformLayer := layeredImage32.AddNewLayer('transformed');

  //Layer 3: a design layer for design lines etc
  designLayer := TDesignerLayer32(
    layeredImage32.AddNewLayer(TDesignerLayer32, 'design'));

  //Layers 4+: button layers
  ResetSplinePts;

  //finally update and display the layered image
  UpdateLayeredImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSplinePts;
begin
  if fButtonGroupId > 0 then
  begin
    layeredImage32.DeleteGroup(fButtonGroupId);
    fButtonGroupId := 0;
  end;

  with masterLayer.Image do     //with the master image
    fSplinePts := MakePathI([0,0, Width div 2,fMargin, Width,0]);
  fSplinePts := OffsetPath(fSplinePts, fMargin, fMargin);

  fButtonGroupId := CreateButtonGroup(layeredImage32, fSplinePts,
    clGreen32, DefaultButtonSize, [boDropShadow]);
end;
//------------------------------------------------------------------------------

procedure TForm1.UpdateLayeredImage(transform: Boolean);
var
  i, w,h, firstButtonIdx: integer;
  pt: TPoint;
  path: TArrayOfPointD;
begin
  if transform then
  begin
    //using splinePts, update the 'transformed' layer
    transformLayer.image.Assign(masterLayer.Image); //copy the master image
    if not SplineTransformVert(transformLayer.image, fSplinePts,
      stQuadratic, clRed32, false, pt) then Exit;
    transformLayer.PositionAt(pt); //nb: 'pt' returned by SplineTransformVert

    //resize layeredImage32 so there's room for further transforms
    w := transformLayer.image.Width + fMargin  + pt.X;
    h := transformLayer.image.Height + fMargin + pt.Y;
    layeredImage32.SetSize(w, h);

    hatchedLayer.SetSize(w, h);
    HatchBackground(hatchedLayer.Image, clWhite32, $FFE0E0E0);
  end else
  begin
    w := layeredImage32.Width;
    h := layeredImage32.Height;
  end;

  //draw spline's design lines and implicit buttons on the design layer
  if not mnuShowImage.Checked then
  begin
    designLayer.SetSize(w, h); //this also clears the layer
    if mnuShowControls.Checked then
    begin
      path := FlattenQSpline(fSplinePts);
      Image32_Draw.DrawLine(designLayer.image, path, 1.5, clBlack32, esSquare);
    end;
    designLayer.DrawQSplineDesign(fSplinePts); //draws virtual buttons etc.
  end;

  //move the 'button' layers to the splinePts coordinates (index 4+)
  firstButtonIdx := layeredImage32.GetFirstInGroupIdx(fButtonGroupId);
  for i := 0 to high(fSplinePts) do
    with layeredImage32[firstButtonIdx +i] do
      PositionCenteredAt(Point(fSplinePts[i]));

  //since transformLayer isn't a design layer,
  //we need to manually change its visibility.
  transformLayer.Visible := not mnuShowControls.Checked;

  //copy the merged layeredImage32 to Panel1
  {$IFDEF SETSIZE}
  Panel1.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  {$ELSE}
  Panel1.Bitmap.Width := layeredImage32.Width;
  Panel1.Bitmap.Height := layeredImage32.Height;
  {$ENDIF}
  //when copying semi-transparent images, the panel's bitmap needs clearing
  Panel1.ClearBitmap;
  layeredImage32.GetMergedImage(mnuShowImage.Checked).CopyToDc(
    Panel1.Bitmap.Canvas.Handle, 0, 0, true);
  Panel1.Refresh;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  len: integer;
  pt: TPoint;
begin
  pt := Types.Point(X,Y);
  if not Panel1.ClientToBitmap(pt) then Exit;

  if ssRight in Shift then
  begin
    //add an extra spline control point on right click

    len := Length(fSplinePts);
    SetLength(fSplinePts, len +1);
    fSplinePts[len] := PointD(pt);

    AddToButtonGroup(layeredImage32, 1, pt);
    UpdateLayeredImage(true);
    Exit;
  end;

  buttonMovingLayer := layeredImage32.GetLayerAt(pt);
  if Assigned(buttonMovingLayer) and
    (buttonMovingLayer.Name <> 'button') then
      buttonMovingLayer := nil;

  if Assigned(buttonMovingLayer) then
  begin
    //while moving buttons temporarily disable panel zoom and scroll
    panel1.BitmapProperties.ZoomAndScrollEnabled := false
  end
  else if PtInRect(transformLayer.Bounds, pt) then
  begin
    fClickPoint := pt;
    fImageIsClicked := true;
    //while moving the image temporarily disable panel zoom and scroll
    panel1.BitmapProperties.ZoomAndScrollEnabled := false;
  end;
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

  if fImageIsClicked then
  begin
    dx := pt.X - fClickPoint.X;
    dy := pt.Y - fClickPoint.Y;
    fClickPoint := pt;
    fSplinePts := OffsetPath(fSplinePts, dx, dy);
    //move (offset) the buttons and the image too
    layeredImage32.OffsetGroup(1, dx, dy);
    transformLayer.Offset(dx, dy);
    UpdateLayeredImage(false);
  end
  else if not Assigned(buttonMovingLayer) then
  begin
    layer := layeredImage32.GetLayerAt(pt);
    if Assigned(layer) and (layer.Name = 'button') then
      Panel1.Cursor := crHandPoint
    else if PtInRect(transformLayer.Bounds, pt) then
      //nb: the 'designer' layer covers the 'transformed' layer
      //so if layer isn't a 'button' it'll be 'designer'
      Panel1.Cursor := crSizeAll
    else
      Panel1.Cursor := crDefault;
  end else
  begin
    buttonMovingLayer.PositionCenteredAt(pt);
    //nb: first 4 layers are non-button layers
    fSplinePts[buttonMovingLayer.Index -4] := PointD(pt);
    UpdateLayeredImage(true);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fImageIsClicked := false;
  buttonMovingLayer := nil;
  //re-enable Panel1 zoom and scroll
  Panel1.BitmapProperties.ZoomAndScrollEnabled := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuShowControlsClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := true;
  UpdateLayeredImage(false);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    masterLayer.Image.LoadFromFile(OpenDialog1.FileName);
    ResetSplinePts;
    UpdateLayeredImage(true);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    transformLayer.Image.SaveToFile(SaveDialog1.FileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.CopytoClipboard1Click(Sender: TObject);
begin
  transformLayer.Image.CopyToClipBoard;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------


end.
