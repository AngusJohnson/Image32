unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, Image32, Image32_Layers, ImagePanels,
  Dialogs, ClipBrd;

type
  TTransformType = (ttAfine, ttProjective, ttSpline);

  TForm1 = class(TForm)
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
    mnuPastefromClipboard: TMenuItem;
    ransformType1: TMenuItem;
    mnuVertSkew: TMenuItem;
    mnuHorizontalSkew: TMenuItem;
    mnuVertProjective: TMenuItem;
    mnuVerticalSpline: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuAddNewCtrlPoint: TMenuItem;
    N3: TMenuItem;
    mnuHideControls: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure mnuPastefromClipboardClick(Sender: TObject);
    procedure mnuVerticalSplineClick(Sender: TObject);
    procedure mnuAddNewCtrlPointClick(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure mnuHideControlsClick(Sender: TObject);
  private
    pnlMain: TBitmapPanel;
    layeredImage32: TLayeredImage32;
    buttonMovingLayer, hatchedLayer: TLayer32;
    masterLayer, transformLayer: TLayer32;
    designLayer: TDesignerLayer32;
    fClickPoint: TPoint;
    fImageIsClicked: Boolean;
    fPts: TArrayOfPointD;
    fMargin: integer;
    fButtonGroupId: integer;
    fTransformType: TTransformType;
    procedure ResetSpline;
    procedure ResetVertProjective;
    procedure ResetSkew;
    procedure UpdateImage(transform: Boolean);
    procedure RedrawPanel;
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

  pnlMain := TBitmapPanel.Create(self);
  pnlMain.Parent := self;
  pnlMain.Align := alClient;
  pnlMain.OnMouseDown := pnlMainMouseDown;
  pnlMain.OnMouseUp := pnlMainMouseUp;
  pnlMain.OnMouseMove := pnlMainMouseMove;
  pnlMain.PopupMenu := PopupMenu1;

  //SETUP THE LAYERED IMAGE
  DefaultButtonSize := DPIAware(10);
  fMargin := DPIAware(50);

  layeredImage32 := TLayeredImage32.Create;
  //width & height will be set in UpdateLayeredImage method below

  //Layer 0: 'hatched' layer to highlight transparency
  hatchedLayer :=
    layeredImage32.AddLayer(TDesignerLayer32);

  //Layer 1: hidden layer contains the master image
  masterLayer := layeredImage32.AddLayer(TDesignerLayer32);
  masterLayer.image.LoadFromResource('UNION_JACK', 'BMP');
  masterLayer.Visible := false;

  //Layer 2: for the transformed image
  transformLayer := layeredImage32.AddLayer;

  //Layer 3: a design layer for design lines etc
  designLayer := TDesignerLayer32(
    layeredImage32.AddLayer(TDesignerLayer32));

  fTransformType := ttAfine;
  ResetSkew;
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSkew;
begin
  if fButtonGroupId > 0 then
  begin
    layeredImage32.DeleteGroup(fButtonGroupId);
    fButtonGroupId := 0;
  end;

  fTransformType := ttAfine;
  SetLength(fPts, 2);
  fPts[0] := PointD(fMargin, fMargin);
  with masterLayer.Image do
    fPts[1] := PointD(width + fMargin, height + fMargin);
  fButtonGroupId := CreateButtonGroup(layeredImage32,
    fPts, clGreen32, DefaultButtonSize, [boDropShadow]);
  designLayer.Visible := false;
  mnuAddNewCtrlPoint.Enabled := false;
  if mnuVertSkew.Checked then
    StatusBar1.SimpleText := ' VERTICAL SKEW' else
    StatusBar1.SimpleText := ' HORIZONTAL SKEW';
  UpdateImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetVertProjective;
begin
  if fButtonGroupId > 0 then
  begin
    layeredImage32.DeleteGroup(fButtonGroupId);
    fButtonGroupId := 0;
  end;

  fTransformType := ttProjective;
  with masterLayer.Image do     //with the master image
    fPts := Rectangle(Bounds);
  fPts := OffsetPath(fPts, fMargin, fMargin);

  fButtonGroupId := CreateButtonGroup(layeredImage32, fPts,
    clGreen32, DefaultButtonSize, [boDropShadow]);
  designLayer.Visible := false;
  mnuAddNewCtrlPoint.Enabled := false;
  StatusBar1.SimpleText := ' PROJECTIVE TRANSFORM';
  UpdateImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSpline;
begin
  if fButtonGroupId > 0 then
  begin
    layeredImage32.DeleteGroup(fButtonGroupId);
    fButtonGroupId := 0;
  end;

  fTransformType := ttSpline;
  with masterLayer.Image do
    fPts := MakePathI([0,0, Width div 2,0, Width,0]);
  fPts := OffsetPath(fPts, fMargin, fMargin);

  fButtonGroupId := CreateButtonGroup(layeredImage32, fPts,
    clGreen32, DefaultButtonSize, [boDropShadow]);
  designLayer.Visible := true;
  mnuAddNewCtrlPoint.Enabled := true;
  StatusBar1.SimpleText := ' VERT SPLINE TRANSFORM: Right click to add control points';
  UpdateImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.UpdateImage(transform: Boolean);
var
  w,h: integer;
  pt: TPoint;
  matrix: TMatrixD;
begin
  if transform then
  begin
    //using fPts, update the 'transformed' layer
    transformLayer.image.Assign(masterLayer.Image); //copy the master image

    case fTransformType of
      ttAfine:
        begin
          //AffineTransform ...
          matrix := IdentityMatrix;
          with masterLayer.Image do
          begin
            if mnuVertSkew.Checked then
              matrix[0][1] := (fPts[1].Y -fPts[0].Y -Height) / Width else
              matrix[1][0] := (fPts[1].X - fPts[0].X -Width)/ Height;
            //otherwise for unrestricted skews do the code below instead
            //(and make changes in Panel1MouseMove too) ...
            //matrix[0][1] := (fPts[1].Y -fPts[0].Y -Height) / Width;
            //matrix[1][0] := (fPts[1].X - fPts[0].X -Width)/ Height;
          end;
          AffineTransformImage(transformLayer.Image, matrix, pt);
          pt := OffsetPoint(pt, Round(fPts[0].X), Round(fPts[0].Y));
        end;
      ttProjective:
        //ProjectiveTransform ...
        if not ProjectiveTransform(transformLayer.image, fPts, pt) then Exit;
      ttSpline:
        //SplineTransformVert ...
        if not SplineVertTransform(transformLayer.image,
          fPts, stQuadratic, clRed32, false, pt) then pt := Point(fPts[0]);
    end;

    transformLayer.PositionAt(pt); //nb: 'pt' returned by SplineTransformVert

    //resize layeredImage32 so there's room for further transforms
    w := transformLayer.image.Width + fMargin * 2;
    h := transformLayer.image.Height + fMargin * 2;
    layeredImage32.SetSize(w, h);

    hatchedLayer.SetSize(w, h);
    HatchBackground(hatchedLayer.Image, clWhite32, $FFE0E0E0);
  end else
  begin
    w := layeredImage32.Width;
    h := layeredImage32. Height;
  end;

  if not mnuHideControls.Checked and designLayer.Visible then
  begin
    //draw spline's design lines and virtual buttons on the design layer
    designLayer.SetSize(w, h); //also clears the layer
    //draw virtual buttons etc.
    designLayer.DrawQSplineDesign(fPts);
  end;
  RedrawPanel;
end;
//------------------------------------------------------------------------------

procedure TForm1.RedrawPanel;
begin
  //copy the merged layeredImage32 to Panel1
  {$IFDEF SETSIZE}
  pnlMain.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  {$ELSE}
  pnlMain.Bitmap.Width := layeredImage32.Width;
  pnlMain.Bitmap.Height := layeredImage32.Height;
  {$ENDIF}
  layeredImage32.GetMergedImage(mnuHideControls.Checked).CopyToDc(
    pnlMain.Bitmap.Canvas.Handle);
  pnlMain.Refresh;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuVerticalSplineClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := true;
  masterLayer.Image.Assign(transformLayer.Image);
  if (Sender = mnuVertSkew) or (Sender = mnuHorizontalSkew) then
    ResetSkew
  else if Sender = mnuVertProjective then
    ResetVertProjective
  else
    ResetSpline;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAddNewCtrlPointClick(Sender: TObject);
var
  len: integer;
begin
  //add an extra spline control point
  len := Length(fPts);
  SetLength(fPts, len +1);
  fPts[len] := PointD(fClickPoint);
  AddToButtonGroup(layeredImage32, fButtonGroupId, fClickPoint);
  UpdateImage(true);
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  pt := Types.Point(X,Y);
  pt := pnlMain.ClientToImage(pt);
  fClickPoint := pt;

  if (fTransformType = ttSpline) and (ssRight in Shift) then
    Exit; //popup menu

  buttonMovingLayer := layeredImage32.GetLayerAt(pt);
  if Assigned(buttonMovingLayer) and
    not (buttonMovingLayer is TButtonDesignerLayer32) then
      buttonMovingLayer := nil;

  if Assigned(buttonMovingLayer) then
  begin
    //while moving buttons temporarily disable panel zoom and scroll
    pnlMain.AllowZoom := false;
    pnlMain.AllowScroll := false;
  end
  else if PtInRect(transformLayer.Bounds, pt) then
  begin
    fImageIsClicked := true;
    //while moving the image temporarily disable panel zoom and scroll
    pnlMain.AllowZoom := false;
    pnlMain.AllowScroll := false;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy, btnBaseIndex, altGroupIdx: integer;
  pt: TPoint;
  layer: TLayer32;
begin
  pt := Types.Point(X,Y);
  pt := pnlMain.ClientToImage(pt);

  if fImageIsClicked then
  begin
    pnlMain.Cursor := crSizeNESW;
    dx := pt.X - fClickPoint.X;
    dy := pt.Y - fClickPoint.Y;
    fClickPoint := pt;
    fPts := OffsetPath(fPts, dx, dy);
    //move (offset) the buttons and the image too
    layeredImage32.OffsetGroup(1, dx, dy);
    transformLayer.Offset(dx, dy);
    UpdateImage(false);
  end
  else if not Assigned(buttonMovingLayer) then
  begin
    layer := layeredImage32.GetLayerAt(pt);
    if Assigned(layer) and (layer is TButtonDesignerLayer32) then
      pnlMain.Cursor := crHandPoint
    else if PtInRect(transformLayer.Bounds, pt) then
      //nb: the 'designer' layer covers the 'transformed' layer
      //so if layer isn't a 'button' it'll be 'designer'
      pnlMain.Cursor := crSizeAll
    else
      pnlMain.Cursor := crDefault;
  end else
  begin
    pnlMain.Cursor := crHandPoint;

    //keep button controls axis aligned for appropriate transforms, otherwise
    //comment out below for an unrestricted skews (and change UpdateImage too)
    if mnuVertSkew.Checked then
      pt.X := Round(buttonMovingLayer.MidPoint.X);
    if mnuHorizontalSkew.Checked then
      pt.Y := Round(buttonMovingLayer.MidPoint.Y);

    //for unrestricted projective transforms, comment out the code block below
    if mnuVertProjective.Checked then
    begin
      btnBaseIndex := layeredImage32.GetFirstInGroupIdx(fButtonGroupId);
      //get the index of the moving button's vertical partner
      altGroupIdx := 3 - buttonMovingLayer.IndexInGroup;
      fPts[altGroupIdx].X := pt.X;
      with layeredImage32[btnBaseIndex + altGroupIdx] do
        PositionCenteredAt(fPts[altGroupIdx]);
    end;

    buttonMovingLayer.PositionCenteredAt(pt);
    fPts[buttonMovingLayer.IndexInGroup] := PointD(pt);
    UpdateImage(true);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fImageIsClicked := false;
  buttonMovingLayer := nil;
  //re-enable Panel1 zoom and scroll
  pnlMain.AllowZoom := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuHideControlsClick(Sender: TObject);
begin
  if mnuHideControls.Checked then
    layeredImage32.HideGroup(fButtonGroupId) else
    layeredImage32.ShowGroup(fButtonGroupId);
  UpdateImage(false);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuOpenClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  masterLayer.Image.LoadFromFile(OpenDialog1.FileName);
  case fTransformType of
    ttAfine: ResetSkew;
    ttProjective: ResetVertProjective;
    ttSpline: ResetSpline;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuPastefromClipboardClick(Sender: TObject);
begin
  if (TImageFormat_PNG.CanPasteFromClipboard and
    TImageFormat_PNG.PasteFromClipboard(masterLayer.Image)) or
    (TImageFormat_BMP.CanPasteFromClipboard and
    TImageFormat_BMP.PasteFromClipboard(masterLayer.Image)) then
  begin
    case fTransformType of
      ttAfine: ResetSkew;
      ttProjective: ResetVertProjective;
      ttSpline: ResetSpline;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.File1Click(Sender: TObject);
begin
  mnuPastefromClipboard.Enabled := TImage32.CanPasteFromClipboard;
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
