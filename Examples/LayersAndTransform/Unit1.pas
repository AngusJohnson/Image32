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
    mnuShowImage: TMenuItem;
    N1: TMenuItem;
    mnuShowAll: TMenuItem;
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
    buttonMovingLayer: TLayer32;
    btnLayerSize: integer;
    clickPoint: TPoint;
    ImageIsMoving: Boolean;
    splinePts: TArrayOfPointD;
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

const
  margin = 100;

//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  //SETUP THE DISPLAY PANEL

  Panel1.BorderWidth := DPI(16);
  Panel1.BevelInner := bvLowered;
  //set Panel1.Tabstop := true to enable keyboard controls
  Panel1.TabStop := true;
  Panel1.FocusedColor := clGradientInactiveCaption;
  Panel1.BitmapProperties.Scale := 1;

  //SETUP THE LAYERED IMAGE
  DefaultButtonSize := DPI(8);

  layeredImage32 := TLayeredImage32.Create(100, 100);
  //layeredImage32.BackgroundColor := clWhite32;

  //add a 'hatched' layer (0) to highlight transparency
  layeredImage32.AddNewLayer('background - hatched');               //0

  //add a hidden master layer (1) containing the starting image
  with layeredImage32.AddNewLayer('master - hidden') do             //1
  begin
    image.LoadFromResource('UNION_JACK', 'BMP');
    Visible := false;
  end;

  //add another layer (2) for the transformed image
  layeredImage32.AddNewLayer('transformed');                        //2

  //add a design layer for design lines etc
  layeredImage32.AddNewLayer(TDesignerLayer32, 'design');           //3

  //and reset for a very basic starting transformation
  //adding layers for each tangible spline control button
  btnLayerSize := Ceil(DefaultButtonSize * 1.25);
  if Odd(btnLayerSize) then inc(btnLayerSize);
  ResetSplinePts;                                                   //4+

  //finally update and display the layered image
  UpdateLayeredImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSplinePts;
begin
  //remove the group of buttons
  //and the only group (1) will be the buttons
  with layeredImage32 do
    if Count > 4 then DeleteGroup(1);

  with layeredImage32[1].Image do //with the master image
    splinePts := MakePathI([0,margin, Width div 2,0, Width,margin]);
  splinePts := OffsetPath(splinePts, margin, margin);

  CreateButtonGroup(layeredImage32,
    splinePts, clGreen32, DefaultButtonSize, []);
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
      if not SplineTransformVert(image, splinePts,
        stQuadratic, clRed32, false, pt) then Exit;
      PositionAt(pt); //nb: 'pt' returned by SplineTransformVert above

      //resize layeredImage32 so there's room for further transforms
      w := image.Width + margin  + pt.X;
      h := image.Height + margin + pt.Y;
      layeredImage32.SetSize(w, h);
      layeredImage32[0].SetSize(w, h);
      layeredImage32[0].Image.HatchBackground(clWhite32, $FFE0E0E0);
    end;
  end else
  begin
    w := layeredImage32.Width;
    h := layeredImage32.Height;
  end;

  //draw spline's design lines and implicit buttons on the design layer
  if not mnuShowImage.Checked then
    with TDesignerLayer32(layeredImage32[3]) do
    begin
      SetSize(w, h); //also clears layer
      if mnuShowControls.Checked then
      begin
        path := QSpline(splinePts);
        DrawLine(image, path, 1.5, clBlack32, esSquare);
      end;
      DrawQSplineDesign(splinePts);
    end;

  //move the 'button' layers to the splinePts coordinates (index 4+)
  for i := 0 to high(splinePts) do
    with layeredImage32[4 +i] do
      PositionCenteredAt(Point(splinePts[i]));

  layeredImage32[2].Visible := not mnuShowControls.Checked;
  for i := 3 to layeredImage32.Count -1 do
    layeredImage32[i].Visible := not mnuShowImage.Checked;

  //copy the merged layeredImage32 to Panel1
  Panel1.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  layeredImage32.GetMergedImage(false).CopyToDc(Panel1.Bitmap.Canvas.Handle);
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

    len := Length(splinePts);
    SetLength(splinePts, len +1);
    splinePts[len] := PointD(pt);

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
  else if PtInRect(layeredImage32[2].Bounds, pt) then
  begin
    clickPoint := pt;
    ImageIsMoving := true;
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

  if ImageIsMoving then
  begin
    dx := pt.X - clickPoint.X;
    dy := pt.Y - clickPoint.Y;
    clickPoint := pt;
    splinePts := OffsetPath(splinePts, dx, dy);
    //move (offset) the buttons and the image too
    layeredImage32.OffsetGroup(1, dx, dy);
    layeredImage32[2].Offset(dx, dy);
    UpdateLayeredImage(false);
  end
  else if not Assigned(buttonMovingLayer) then
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
    buttonMovingLayer.PositionCenteredAt(pt);
    //nb: first 4 layers are non-button layers
    splinePts[buttonMovingLayer.Index -4] := PointD(pt);
    UpdateLayeredImage(true);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ImageIsMoving := false;
  buttonMovingLayer := nil;
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
