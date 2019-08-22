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
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    N2: TMenuItem;
    CopytoClipboard1: TMenuItem;
    Options1: TMenuItem;
    mnuVerticalEdges: TMenuItem;
    mnuHorizontalEdges: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure mnuHorizontalEdgesClick(Sender: TObject);
  private
    layeredImage32: TLayeredImage32;
    buttonMovingLayer: TLayer32;
    btnLayerSize: integer;
    clickPoint: TPoint;
    ImageIsMoving: Boolean;
    buttonPts: TArrayOfPointD;
    margin: integer;
    procedure UpdateButtonLayers;
    procedure UpdateLayeredImage(transform: Boolean);
    procedure ButtonPointsToStatusbar;
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
  margin := DPI(50);

  layeredImage32 := TLayeredImage32.Create(100, 100);
  //layeredImage32.BackgroundColor := clWhite32;

  //add a background layer to show hatched transparency
  //and make it a TDesignerLayer32 so it isn't 'clickable'
  layeredImage32.AddNewLayer(TDesignerLayer32, 'background - hatched');  //0


  //add a hidden master layer containing the starting image
  with layeredImage32.AddNewLayer('master - hidden') do                  //1
  begin
    image.LoadFromResource('BEETLE', 'BMP');
    Visible := false;
  end;

  //add a third layer for the transformed image
  layeredImage32.AddNewLayer('transformed');                             //2
  layeredImage32[2].Image.Assign(layeredImage32[1].Image);
  layeredImage32[2].PositionAt(Point(margin, margin));

  //add 4 more layers for control buttons                                //3-7

  btnLayerSize := DefaultButtonSize +1; //+1 = room for pen outline
  //and to avoid rounding issues when positioning layers using its midpoint
  if Odd(btnLayerSize) then inc(btnLayerSize);
  buttonPts := Rectangle(layeredImage32[2].Bounds);
  CreateButtonGroup(layeredImage32, buttonPts, clBlue32, DefaultButtonSize, []);

  //TRANSFORM AND DISPLAY
  UpdateLayeredImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.UpdateLayeredImage(transform: Boolean);
var
  i,w,h: integer;
  path: TArrayOfPointD;
  rec: TRect;
begin
  if transform then
    with layeredImage32[2] do
    begin
      //copy the master image the the 'transform' layer (2) and transform
      image.Assign(layeredImage32[1].Image);
      if not ProjectiveTransform(layeredImage32[2].Image, buttonPts) then
        Exit;

      rec := GetBounds(buttonPts);
      layeredImage32[2].PositionAt(rec.TopLeft);
      //resize layeredImage32 so there's room for further transforms
      w := image.Width + margin  + rec.Left;
      h := image.Height + margin + rec.Top;
      layeredImage32.SetSize(w, h);
      //redo hatched bakground image
      layeredImage32[0].SetSize(w, h);
      layeredImage32[0].Image.HatchBackground(clWhite32, $FFE0E0E0);
    end;

  //copy the merged layeredImage32 to Panel1
  Panel1.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  layeredImage32.GetMergedImage(false).CopyToDc(Panel1.Bitmap.Canvas.Handle);
  Panel1.Refresh;
  ButtonPointsToStatusbar;
end;
//------------------------------------------------------------------------------

procedure TForm1.ButtonPointsToStatusbar;
var
  i: integer;
  s: string;
begin
  for i := 0 to 3 do
    s := format('%s  (%1.0n,%1.0n)', [s, buttonPts[i].X, buttonPts[i].Y]);
  StatusBar1.SimpleText := s;
end;
//------------------------------------------------------------------------------

procedure TForm1.UpdateButtonLayers;
var
  i: integer;
begin
  //nb: first 3 layers are non-button layers
  for i := 0 to 3 do
    layeredImage32[3+i].PositionCenteredAt(buttonPts[i]);
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
  btnIdx, dx,dy: integer;
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
    buttonPts := OffsetPath(buttonPts, dx, dy);
    //offset (move) both buttons and image
    layeredImage32[2].Offset(dx, dy);
    layeredImage32.OffsetGroup(1, dx, dy);
    UpdateLayeredImage(false);
  end
  else if Assigned(buttonMovingLayer) then
  begin
    //nb: first 3 layers are non-button layers
    btnIdx := buttonMovingLayer.Index -3;
    buttonPts[btnIdx] := PointD(pt);

    if mnuVerticalEdges.Checked then
    begin
      case btnIdx of
        0: buttonPts[3].X := buttonPts[0].X;
        1: buttonPts[2].X := buttonPts[1].X;
        2: buttonPts[1].X := buttonPts[2].X;
        3: buttonPts[0].X := buttonPts[3].X;
      end;
    end else if mnuHorizontalEdges.Checked then
    begin
      case btnIdx of
        0: buttonPts[1].Y := buttonPts[0].Y;
        1: buttonPts[0].Y := buttonPts[1].Y;
        2: buttonPts[3].Y := buttonPts[2].Y;
        3: buttonPts[2].Y := buttonPts[3].Y;
      end;
    end;
    UpdateButtonLayers;
    UpdateLayeredImage(true);
  end else
  begin
    layer := layeredImage32.GetLayerAt(pt);
    if Assigned(layer) and (layer.Name = 'button') then
      Panel1.Cursor := crHandPoint
    else if PtInRect(layeredImage32[2].Bounds, pt) then
      Panel1.Cursor := crSizeAll
    else
      Panel1.Cursor := crDefault;
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

procedure TForm1.mnuOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute(handle) then
  begin
    //reload the master image
    layeredImage32[1].Image.LoadFromFile(OpenDialog1.FileName);
    //copy the master image to the 'transform' layer and position it
    layeredImage32[2].Image.Assign(layeredImage32[1].Image);
    layeredImage32[2].PositionAt(Point(margin, margin));
    buttonPts := Rectangle(layeredImage32[2].Image.Bounds);
    UpdateButtonLayers;
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

procedure TForm1.mnuHorizontalEdgesClick(Sender: TObject);
begin
  if mnuVerticalEdges.Checked then
  begin
    buttonPts[3].X := buttonPts[0].X;
    buttonPts[2].X := buttonPts[1].X;
  end else if mnuHorizontalEdges.Checked then
  begin
    buttonPts[1].Y := buttonPts[0].Y;
    buttonPts[2].Y := buttonPts[3].Y;
  end
  else Exit;
  UpdateButtonLayers;
  UpdateLayeredImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------


end.
