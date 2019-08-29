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
    fButtonClickedLayer: TLayer32;
    fClickPoint: TPoint;
    fImageIsClicked: Boolean;
    fSplinePts: TArrayOfPointD;
    fMargin: integer;
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
var
  layer: TLayer32;
begin
  //SETUP THE DISPLAY PANEL

  Panel1.BorderWidth := DPI(16);
  Panel1.BevelInner := bvLowered;
  Panel1.TabStop := true; //enables keyboard control
  Panel1.FocusedColor := clGradientInactiveCaption;
  Panel1.BitmapProperties.Scale := 1;

  //SETUP THE LAYERED IMAGE
  DefaultButtonSize := DPI(8);
  fMargin := DPI(50);

  layeredImage32 := TLayeredImage32.Create;

  //Layer 0: 'hatched' layer to highlight transparency
  layeredImage32.AddNewLayer('background - hatched');

  //Layer 1: hidden layer contains the starting image
  layer := layeredImage32.AddNewLayer('master - hidden');
  layer.image.LoadFromResource('UNION_JACK', 'BMP');
  layer.Visible := false;

  //Layer 2: for the transformed image
  layeredImage32.AddNewLayer('transformed');

  //Layer 3: a design layer for design lines etc
  layeredImage32.AddNewLayer(TDesignerLayer32, 'design');

  //Layers 4+: button layers
  ResetSplinePts;

  //finally update and display the layered image
  UpdateLayeredImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSplinePts;
begin
  with layeredImage32 do
    if Count > 4 then DeleteGroup(1); //button are the only group

  with layeredImage32[1].Image do     //with the master image
    fSplinePts := MakePathI([0,0, Width div 2,fMargin, Width,0]);
  fSplinePts := OffsetPath(fSplinePts, fMargin, fMargin);

  CreateButtonGroup(layeredImage32, fSplinePts, clGreen32, DefaultButtonSize, []);
end;
//------------------------------------------------------------------------------

procedure TForm1.UpdateLayeredImage(transform: Boolean);
var
  i,w,h: integer;
  pt: TPoint;
  path: TArrayOfPointD;
begin
  if transform then
  begin
    with layeredImage32[2] do
    begin
      //using splinePts, update the 'transformed' layer
      image.Assign(layeredImage32[1].Image); //copy the master image
      if not SplineTransformVert(image, fSplinePts,
        stQuadratic, clRed32, false, pt) then Exit;
      PositionAt(pt); //nb: 'pt' returned by SplineTransformVert

      //resize layeredImage32 so there's room for further transforms
      w := image.Width + fMargin  + pt.X;
      h := image.Height + fMargin + pt.Y;
      layeredImage32.SetSize(w, h);
      layeredImage32[0].SetSize(w, h);
      HatchBackground(layeredImage32[0].Image, clWhite32, $FFE0E0E0);
    end;
  end else
  begin
    w := layeredImage32.Width;
    h := layeredImage32.Height;
  end;

  //draw spline's design lines and implicit buttons on the design layer
  if not mnuShowImage.Checked then
  begin
    with TDesignerLayer32(layeredImage32[3]) do
    begin
      SetSize(w, h); //also clears layer
      if mnuShowControls.Checked then
      begin
        path := QSpline(fSplinePts);
        DrawLine(image, path, 1.5, clBlack32, esSquare);
      end;
      DrawQSplineDesign(fSplinePts);
    end;
  end;

  //move the 'button' layers to the splinePts coordinates (index 4+)
  for i := 0 to high(fSplinePts) do
    with layeredImage32[4 +i] do
      PositionCenteredAt(Point(fSplinePts[i]));

  layeredImage32[2].Visible := not mnuShowControls.Checked;

  //copy the merged layeredImage32 to Panel1
  Panel1.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  layeredImage32.GetMergedImage(mnuShowImage.Checked).CopyToDc(Panel1.Bitmap.Canvas.Handle);
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

  fButtonClickedLayer := layeredImage32.GetLayerAt(pt);
  if Assigned(fButtonClickedLayer) and
    (fButtonClickedLayer.Name <> 'button') then
      fButtonClickedLayer := nil;

  if Assigned(fButtonClickedLayer) then
  begin
    //while moving buttons temporarily disable panel zoom and scroll
    panel1.BitmapProperties.ZoomAndScrollEnabled := false
  end
  else if PtInRect(layeredImage32[2].Bounds, pt) then
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
    layeredImage32[2].Offset(dx, dy);
    UpdateLayeredImage(false);
  end
  else if not Assigned(fButtonClickedLayer) then
  begin
    layer := layeredImage32.GetLayerAt(pt);
    if Assigned(layer) and (layer.Name = 'button') then
      Panel1.Cursor := crHandPoint
    else if PtInRect(layeredImage32[2].Bounds, pt) then
      //nb: the 'designer' layer covers the 'transformed' layer
      //so if layer isn't a 'button' it'll be 'designer'
      Panel1.Cursor := crSizeAll
    else
      Panel1.Cursor := crDefault;
  end else
  begin
    fButtonClickedLayer.PositionCenteredAt(pt);
    //nb: first 4 layers are non-button layers
    fSplinePts[fButtonClickedLayer.Index -4] := PointD(pt);
    UpdateLayeredImage(true);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fImageIsClicked := false;
  fButtonClickedLayer := nil;
  //re-enable Panel1 zoom and scroll
  Panel1.BitmapProperties.ZoomAndScrollEnabled := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuShowControlsClick(Sender: TObject);
begin
  UpdateLayeredImage(false);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute(handle) then
  begin
    layeredImage32[1].Image.LoadFromFile(OpenDialog1.FileName);
    ResetSplinePts;
    UpdateLayeredImage(true);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute(handle) then
    layeredImage32[2].Image.SaveToFile(SaveDialog1.FileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.CopytoClipboard1Click(Sender: TObject);
begin
  layeredImage32[2].Image.CopyToClipBoard;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------


end.
