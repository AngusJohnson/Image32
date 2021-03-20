unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, Image32, Image32_Layers, ImagePanels,
  Dialogs, ClipBrd;

type
  TTransformType = (ttAffineSkew, ttProjective, ttSpline, ttAffineRotate);

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
    Rotate1: TMenuItem;
    N4: TMenuItem;
    mnuHideDesigners: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure mnuPastefromClipboardClick(Sender: TObject);
    procedure mnuVerticalSplineClick(Sender: TObject);
    procedure mnuAddNewCtrlPointClick(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure mnuHideDesignersClick(Sender: TObject);
  private
    layeredImage: TLayeredImage32;
    buttonGroup: TButtonGroupLayer32;
    buttonRotateGroup: TRotatingGroupLayer32;

    clickedLayer: TLayer32;
    transformLayer: TRasterLayer32;

    fPopupPoint: TPoint;
    fClickPoint: TPoint;
    fCtrlPoints: TPathD;
    fTransformType: TTransformType;
    procedure ResetSpline;
    procedure ResetVertProjective;
    procedure ResetSkew(isVerticalSkew: Boolean);
    procedure ResetRotate;
    procedure DoTransform;
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
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

  //SETUP THE LAYERED IMAGE
  DefaultButtonSize := DPIAware(10);

  layeredImage := TLayeredImage32.Create;
  layeredImage.BackgroundColor := Color32(clBtnFace);

  //width & height will be set in UpdateLayeredImage method below

  //Layer 0: bottom 'hatched' design layer
  layeredImage.AddLayer(TDesignerLayer32, nil, 'hatched');

  //Layer 1: for the transformed image
  transformLayer := TRasterLayer32(layeredImage.AddLayer(TRasterLayer32));
  transformLayer.MasterImage.LoadFromResource('UNION_JACK', 'BMP');
  transformLayer.CursorId := crSizeAll;
  transformLayer.PositionAt(100,100);

  ResetSkew(mnuVertSkew.Checked);
end;
//------------------------------------------------------------------------------

procedure TForm1.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 1;
  //this stops windows unhelpfully erasing the form's canvas.
  //We want full control of painting (see FormPaint below).
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSkew(isVerticalSkew: Boolean);
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(buttonRotateGroup);

  fTransformType := ttAffineSkew;

  SetLength(fCtrlPoints, 2);
  with transformLayer.MasterImage.Bounds do
  begin
    fCtrlPoints[0] := PointD(TopLeft);
    fCtrlPoints[1] := PointD(BottomRight);
  end;
  //now make fPts relative to the canvas surface
  with transformLayer do
    fCtrlPoints := OffsetPath(fCtrlPoints, Left, Top);

  buttonGroup := CreateButtonGroup(layeredImage.Root,
    fCtrlPoints, bsRound, DefaultButtonSize, clGreen32);

  if isVerticalSkew then StatusBar1.SimpleText := ' VERTICAL SKEW'
  else StatusBar1.SimpleText := ' HORIZONTAL SKEW';

  DoTransform;
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetVertProjective;
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(buttonRotateGroup);

  fTransformType := ttProjective;
  with transformLayer.MasterImage do     //with the master image
    fCtrlPoints := Rectangle(Bounds);
  //now make fPts relative to the canvas surface
  with transformLayer do
    fCtrlPoints := OffsetPath(fCtrlPoints, Left, Top);

  buttonGroup := CreateButtonGroup(layeredImage.Root, fCtrlPoints,
    bsRound, DefaultButtonSize, clGreen32);
  StatusBar1.SimpleText := ' PROJECTIVE TRANSFORM';
  DoTransform;
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSpline;
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(buttonRotateGroup);

  fTransformType := ttSpline;
  with transformLayer.MasterImage do
    fCtrlPoints := MakePathI([0,0, Width div 2,0, Width,0]);

  //now make fPts relative to the canvas surface
  with transformLayer do
    fCtrlPoints := OffsetPath(fCtrlPoints, Left, Top);
  buttonGroup := CreateButtonGroup(layeredImage.Root, fCtrlPoints,
    bsRound, DefaultButtonSize, clGreen32);
  StatusBar1.SimpleText := ' VERT SPLINE TRANSFORM: Right click to add control points';
  DoTransform;
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetRotate;
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(buttonRotateGroup);

  fTransformType := ttAffineRotate;

  transformLayer.Image.CropTransparentPixels;
  transformLayer.UpdateHitTestMaskOpaque;

  //nb: fCtrlPoints are ignored with rotation

  buttonRotateGroup := CreateRotatingButtonGroup(transformLayer,
    DefaultButtonSize, clWhite32, clAqua32, 0, PI/2);

  StatusBar1.SimpleText := ' ROTATE TRANSFORM';
  DoTransform;
end;
//------------------------------------------------------------------------------

procedure TForm1.DoTransform;
var
  pt: TPoint;
  mat: TMatrixD;
begin
  //using fPts, update the 'transformed' layer
  with transformLayer do
  begin
    Image.Assign(masterImage);

    case fTransformType of
      ttAffineSkew:
        begin
          mat := IdentityMatrix;
          with Image do
          begin
            if mnuVertSkew.Checked then
              mat[0][1] := (fCtrlPoints[1].Y -fCtrlPoints[0].Y -Height) / Width
            else
              mat[1][0] := (fCtrlPoints[1].X - fCtrlPoints[0].X -Width)/ Height;
            //for unrestricted skews, use the following commented code instead.
            //(and make changes in Panel1MouseMove too) ...
            //mat[0][1] := (fCtrlPoints[1].Y -fCtrlPoints[0].Y -Height) / Width;
            //mat[1][0] := (fCtrlPoints[1].X - fCtrlPoints[0].X -Width)/ Height;
          end;
          AffineTransformImage(Image, mat, pt);
          pt := OffsetPoint(pt, Round(fCtrlPoints[0].X), Round(fCtrlPoints[0].Y));
        end;
      ttAffineRotate:
        begin
          pt := Point(buttonRotateGroup.Pivot);
          Image.Rotate(buttonRotateGroup.Angle);
          SymmetricCropTransparent(Image);
          with Point(Image.MidPoint) do
            pt := OffsetPoint(pt, -X, -Y);
          StatusBar1.SimpleText := Format(' ROTATE TRANSFORM - angle:%1.0n',
            [buttonRotateGroup.Angle *180/PI]);

        end;
      ttProjective:
        begin
          if not ProjectiveTransform(image,
            Rectangle(image.Bounds), fCtrlPoints, NullRect) then Exit;
        pt := GetBounds(fCtrlPoints).TopLeft;
        end;
      ttSpline:
        begin
          SplineVertTransform(Image, fCtrlPoints,
            stQuadratic, clRed32, false, pt);
          pt := GetBounds(fCtrlPoints).TopLeft;
        end;
    end;
    PositionAt(pt);
    UpdateHitTestMaskTransparent;
  end;

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormResize(Sender: TObject);
var
  w,h: integer;
begin
  if csDestroying in ComponentState then Exit;

  w := ClientWidth; h := ClientHeight;
  layeredImage.SetSize(w, h);
  //and update hatched layer too
  with layeredImage[0] do
  begin
    SetSize(w, h);
    HatchBackground(Image, clWhite32, $FFE0E0E0);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormPaint(Sender: TObject);
var
  updateRect: TRect;
begin
  //nb: layeredImage32.GetMergedImage returns the rectangular region of the
  //image that has changed since the last GetMergedImage call.
  //This accommodates updating just this changed region (which is generally
  //much faster than updating the whole merged image).
  with layeredImage.GetMergedImage(mnuHideControls.Checked, updateRect) do
  begin
    CopyToDc(updateRect, self.Canvas.Handle,
      updateRect.Left, updateRect.Top, false);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuVerticalSplineClick(Sender: TObject);
var
  rec: TRect;
begin
  TMenuItem(Sender).Checked := true;

  //rather than started each transform afresh,
  //let's make them additive ...
  with transformLayer do
  begin
    MasterImage.Assign(Image);
    rec := MasterImage.CropTransparentPixels;
    //and adjust for the cropped offset
    Offset(rec.Left, rec.Top);
  end;

  if (Sender = mnuVertSkew) then            ResetSkew(true)
  else if (Sender = mnuHorizontalSkew) then ResetSkew(false)
  else if Sender = mnuVertProjective then   ResetVertProjective
  else if Sender = mnuVerticalSpline then   ResetSpline
  else                                      ResetRotate;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  layeredImage.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  mnuAddNewCtrlPoint.Visible := fTransformType = ttSpline;

  GetCursorPos(fPopupPoint);
  fPopupPoint := ScreenToClient(fPopupPoint);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAddNewCtrlPointClick(Sender: TObject);
var
  len: integer;
begin
  //add an extra spline control point
  if not Assigned(buttonGroup) then Exit;

  len := Length(fCtrlPoints);
  SetLength(fCtrlPoints, len +1);
  fCtrlPoints[len] := PointD(fPopupPoint);
  buttonGroup.AddButton(PointD(fPopupPoint));
  DoTransform;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDblClick(Sender: TObject);
begin
  if fTransformType <> ttSpline then Exit;
  GetCursorPos(fPopupPoint);
  fPopupPoint := ScreenToClient(fPopupPoint);
  mnuAddNewCtrlPointClick(nil);
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) then Exit; //popup menu

  fClickPoint := Types.Point(X,Y);
  clickedLayer := layeredImage.GetLayerAt(fClickPoint);
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy, idx, altIdx: integer;
  pt: TPoint;
  layer: TLayer32;
begin
  pt := Types.Point(X,Y);

  if not (ssLeft in Shift) then
  begin
    layer := layeredImage.GetLayerAt(pt);
    if Assigned(layer) then
      Cursor := layer.CursorId else
      Cursor := crDefault;
    Exit;
  end;

  if not Assigned(clickedLayer) then Exit;

  if clickedLayer = transformLayer then
  begin
    dx := pt.X - fClickPoint.X;
    dy := pt.Y - fClickPoint.Y;
    fClickPoint := pt;
    fCtrlPoints := OffsetPath(fCtrlPoints, dx, dy);
    clickedLayer.Offset(dx, dy);
    if Assigned(buttonGroup) then buttonGroup.Offset(dx, dy);
    if Assigned(buttonRotateGroup) then buttonRotateGroup.Offset(dx, dy);
    Invalidate;
  end else if clickedLayer.GroupOwner = buttonRotateGroup then
  begin
    clickedLayer.PositionCenteredAt(pt);
    UpdateRotatingButtonGroup(clickedLayer); //redraws dotted circle
    fCtrlPoints[1] := PointD(pt);
    DoTransform;
  end else if clickedLayer.GroupOwner = buttonGroup then
  begin
    //keep button controls axis aligned for appropriate transforms, otherwise
    //comment out below for an unrestricted skews (and change DoTransform too)
    if mnuVertSkew.Checked then
      pt.X := Round(clickedLayer.MidPoint.X);
    if mnuHorizontalSkew.Checked then
      pt.Y := Round(clickedLayer.MidPoint.Y);

    idx := clickedLayer.Index;
    //for unrestricted projective transforms, comment out the code block below
    if mnuVertProjective.Checked then
    begin
      //get the index of the moving button's vertical partner
      altIdx := 3 - idx;
      fCtrlPoints[altIdx].X := pt.X;
      buttonGroup[altIdx].PositionCenteredAt(fCtrlPoints[altIdx]);
    end;

    clickedLayer.PositionCenteredAt(pt);
    fCtrlPoints[idx] := PointD(pt);
    DoTransform;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuHideDesignersClick(Sender: TObject);
begin
  mnuHideDesigners.Checked := not mnuHideDesigners.Checked;
  mnuHideControls.Checked := mnuHideDesigners.Checked;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuOpenClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  transformLayer.MasterImage.LoadFromFile(OpenDialog1.FileName);
  transformLayer.MasterImage.CropTransparentPixels;
  case fTransformType of
    ttAffineSkew:   ResetSkew(mnuVertSkew.Checked);
    ttProjective:   ResetVertProjective;
    ttSpline:       ResetSpline;
    ttAffineRotate: ResetRotate;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuPastefromClipboardClick(Sender: TObject);
begin
  if TImage32.CanPasteFromClipboard and
    transformLayer.MasterImage.PasteFromClipboard then
  begin
    transformLayer.MasterImage.CropTransparentPixels;
    case fTransformType of
      ttAffineSkew  : ResetSkew(mnuVertSkew.Checked);
      ttProjective  : ResetVertProjective;
      ttSpline      : ResetSpline;
      ttAffineRotate: ResetRotate;
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
