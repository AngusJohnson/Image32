unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, Img32, Img32.Layers,
  Dialogs, ClipBrd, StdCtrls;

type
  TTransformType = (ttAffineSkew, ttProjective,
    ttSplineV, ttSplineH, ttAffineRotate);

  TTransformLayer32 = class(TRotLayer32)
  private
    fImgCopy: TImage32;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor  Destroy; override;
    function Rotate(angleDelta: double): Boolean; override;
    property ImageBak: TImage32 read fImgCopy;
  end;

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
    mnuHorizontalSpline: TMenuItem;
    Reset1: TMenuItem;
    N5: TMenuItem;
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
    procedure Reset1Click(Sender: TObject);
  private
    layeredImage: TLayeredImage32;
    buttonGroup: TButtonGroupLayer32;
    rotateGroup: TRotatingGroupLayer32;
    transformLayer: TTransformLayer32;
    clickedLayer: TLayer32;

    popupPoint: TPoint;
    clickPoint: TPoint;
    ctrlPoints: TPathD;
    transformType: TTransformType;
    doTransformOnIdle: Boolean;
    allowRotatePivotMove: Boolean;
    procedure ResetSpline(vert: Boolean);
    procedure ResetVertProjective;
    procedure ResetSkew(isVerticalSkew: Boolean);
    procedure ResetRotate;
    procedure DoTransform;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  a,b: double;

implementation

{$R *.dfm}
{$R image.res}

uses
  Img32.Fmt.BMP, Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Draw, Img32.Vector,
  Img32.Extra, Img32.Resamplers, Img32.Transform;

//------------------------------------------------------------------------------
// TTransformLayer32
//------------------------------------------------------------------------------

constructor TTransformLayer32.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited;
  fImgCopy := TImage32.create;
end;
//------------------------------------------------------------------------------

destructor TTransformLayer32.Destroy;
begin
  fImgCopy.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TTransformLayer32.Rotate(angleDelta: double): Boolean;
var
  mp: TPointD;
begin
  Result := inherited Rotate(angleDelta);
  if not Result then Exit;
  mp := MidPoint;
  Image.Rotate(angleDelta);
  SymmetricCropTransparent(Image);
  PositionCenteredAt(mp);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  //DefaultResampler := rNearestResampler;
  //DefaultResampler := rBilinearResampler;
  DefaultResampler := rBicubicResampler;

  //SETUP THE LAYERED IMAGE
  DefaultButtonSize := DPIAware(10);
  allowRotatePivotMove := true;//false;//

  Application.OnIdle := AppIdle;

  layeredImage := TLayeredImage32.Create;
  layeredImage.BackgroundColor := Color32(clBtnFace);

  //Layer 0: bottom 'hatched' design layer
  layeredImage.AddLayer(TLayer32, nil, 'hatched');

  //Layer 1: for the transformed image
  transformLayer := TTransformLayer32(layeredImage.AddLayer(TTransformLayer32));
  //transformLayer.MasterImage.LoadFromResource('GRADIENT', 'PNG');

  transformLayer.Image.LoadFromResource('UNION_JACK', 'BMP');
  transformLayer.ImageBak.Assign(transformLayer.Image);

  transformLayer.CursorId := crHandPoint;
  transformLayer.AutoPivot := true;
  transformLayer.UpdateHitTestMask;

  ResetSkew(mnuVertSkew.Checked);
end;
//------------------------------------------------------------------------------

procedure TForm1.FormResize(Sender: TObject);
var
  w,h: integer;
  mp: TPointD;
  dx, dy: double;
begin
  if csDestroying in ComponentState then Exit;

  w := ClientWidth; h := ClientHeight;

  //resize layeredImage and the background hatch layer
  layeredImage.SetSize(w, h);
  with layeredImage[0] do
  begin
    SetSize(w, h);
    HatchBackground(Image, clWhite32, $FFE0E0E0);
  end;
  //and center transformlayer
  mp := transformLayer.MidPoint;
  transformlayer.PositionCenteredAt(PointD(w/2,h/2));

  //and offset everything else
  with transformLayer.MidPoint do
  begin
    dx := X - mp.X;
    dy := Y - mp.Y;
  end;
  if Assigned(buttonGroup) then
    buttonGroup.Offset(Round(dx), Round(dy))
  else if Assigned(rotateGroup) then
    rotateGroup.Offset(Round(dx), Round(dy));
  ctrlPoints := OffsetPath(ctrlPoints, dx, dy);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.AppIdle(Sender: TObject; var Done: Boolean);
begin
  if doTransformOnIdle then
  begin
    doTransformOnIdle := false;
    DoTransform;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.WMERASEBKGND(var message: TMessage);
begin
  //Since we want full control of painting (see FormPaint below),
  //we'll stops Windows unhelpfully erasing the form's canvas.
  message.Result := 1;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormPaint(Sender: TObject);
var
  updateRect: TRect;
begin
  //nb: layeredImage32.GetMergedImage returns the rectangular region of the
  //image that has changed since the last GetMergedImage call.
  //This accommodates updating just the region that's changed. This is
  //generally a lot faster than updating the whole merged image).
  with layeredImage.GetMergedImage(mnuHideControls.Checked, updateRect) do
  begin
    //now we just refresh the 'updateRect' region
    CopyToDc(updateRect, updateRect, self.Canvas.Handle, false);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSkew(isVerticalSkew: Boolean);
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(rotateGroup);

  transformType := ttAffineSkew;
  SetLength(ctrlPoints, 2);
  with transformLayer.InnerRect do
  begin
    ctrlPoints[0] := TopLeft;
    ctrlPoints[1] := BottomRight;
  end;
  //now make fPts relative to the canvas surface
  with transformLayer do
    ctrlPoints := OffsetPath(ctrlPoints, Left, Top);

  buttonGroup := CreateButtonGroup(layeredImage.Root,
    ctrlPoints, bsRound, DefaultButtonSize, clGreen32);

  Invalidate;
  if isVerticalSkew then StatusBar1.SimpleText := ' VERTICAL SKEW'
  else StatusBar1.SimpleText := ' HORIZONTAL SKEW';
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetVertProjective;
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(rotateGroup);

  transformType := ttProjective;
  ctrlPoints := Rectangle(transformLayer.InnerRect);
  //now make fPts relative to the canvas surface
  with transformLayer do
    ctrlPoints := OffsetPath(ctrlPoints, Left, Top);

  buttonGroup := CreateButtonGroup(layeredImage.Root, ctrlPoints,
    bsRound, DefaultButtonSize, clGreen32);

  Invalidate;
  StatusBar1.SimpleText := ' PROJECTIVE TRANSFORM';
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetSpline(vert: Boolean);
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(rotateGroup);

  if vert then
  begin
    transformType := ttSplineV;
    with transformLayer.Image do
      ctrlPoints := MakePathI([0, 0, Width div 2, 0, Width, 0]);
    StatusBar1.SimpleText := ' VERT SPLINE TRANSFORM: Right click to add control points';
  end else
  begin
    transformType := ttSplineH;
    with transformLayer.Image do
      ctrlPoints := MakePathI([0,0, 0,Height div 2, 0, Height]);
    StatusBar1.SimpleText := ' HORZ SPLINE TRANSFORM: Right click to add control points';
  end;

  //now make fPts relative to the canvas surface
  with transformLayer do
    ctrlPoints := OffsetPath(ctrlPoints, Left, Top);
  buttonGroup := CreateButtonGroup(layeredImage.Root, ctrlPoints,
    bsRound, DefaultButtonSize, clGreen32);

  Invalidate;

end;
//------------------------------------------------------------------------------

procedure TForm1.ResetRotate;
begin
  FreeAndNil(buttonGroup);
  FreeAndNil(rotateGroup);

  transformType := ttAffineRotate;
  transformLayer.UpdateHitTestMask;
  transformLayer.ResetAngle;

  transformLayer.AutoPivot := not allowRotatePivotMove;
  if allowRotatePivotMove then
    transformLayer.PivotPt := transformLayer.MidPoint;

  //create rotate button group while also disabling pivot button moves
  rotateGroup := CreateRotatingButtonGroup(transformLayer,
    DefaultButtonSize, clWhite32, clAqua32, 0, -Angle90);
  rotateGroup.AngleButton.CursorId := crSizeWE;

  Invalidate;
  StatusBar1.SimpleText := ' ROTATE TRANSFORM';
end;
//------------------------------------------------------------------------------

procedure TForm1.DoTransform;
var
  pt: TPoint;
  mat: TMatrixD;
  delta: double;
begin
  //except for rotation, use ctrlPoints to update the 'transformed' layer
  with transformLayer do
  begin
    if (Image.Width = 0) or (Image.Height = 0) then Exit;
    case transformType of
      ttAffineSkew:
        begin
          Image.Assign(ImageBak);
          mat := IdentityMatrix;
          if mnuVertSkew.Checked then
          begin
            delta := (ctrlPoints[1].Y-Image.Height) - ctrlPoints[0].Y;
            mat[0][1] := delta / Image.Width;
          end else
          begin
            delta := (ctrlPoints[1].X-Image.Width) - ctrlPoints[0].X;
            mat[1][0] := delta / Image.Height;
          end;
          //the returned pt states the offset of the new (transformed) image
          pt := AffineTransformImage(Image, mat);
          with Point(ctrlPoints[0]) do
            PositionAt(X +pt.X, Y +pt.Y);
        end;
      ttAffineRotate:
        begin
          transformLayer.Angle := UpdateRotatingButtonGroup(rotateGroup);
          StatusBar1.SimpleText := Format(' ROTATE TRANSFORM - angle:%1.0n',
            [transformLayer.Angle *180/PI]);
        end;
      ttProjective:
        begin
          Image.Assign(ImageBak);
          if not ProjectiveTransform(image,
            Rectangle(image.Bounds), ctrlPoints, NullRect) then Exit;
          pt := GetBounds(ctrlPoints).TopLeft;
          PositionAt(pt.X, pt.Y);
        end;
      ttSplineV:
        begin
          Image.Assign(ImageBak);
          if not SplineVertTransform(Image, ctrlPoints,
            stQuadratic, clRed32, false, pt) then Exit;
          PositionAt(pt.X, pt.Y);
        end;
      ttSplineH:
        begin
          Image.Assign(ImageBak);
          if not SplineHorzTransform(Image, ctrlPoints,
            stQuadratic, clRed32, false, pt) then Exit;
          PositionAt(pt.X, pt.Y);
        end;
    end;
    UpdateHitTestMask;
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuVerticalSplineClick(Sender: TObject);
var
  i: integer;
  pt: TPointD;
begin
  with TMenuItem(Sender).Parent do
  for i := 0 to Count -1 do
    Items[i].Checked := false;

  TMenuItem(Sender).Checked := true;

  //make each transform additive
  with transformLayer do
  begin
    pt := MidPoint;
    Image.CropTransparentPixels;
    PositionCenteredAt(pt);
    ImageBak.Assign(Image);
  end;

  if (Sender = mnuVertSkew) then            ResetSkew(true)
  else if (Sender = mnuHorizontalSkew) then ResetSkew(false)
  else if Sender = mnuVertProjective then   ResetVertProjective
  else if Sender = mnuVerticalSpline then   ResetSpline(true)
  else if Sender = mnuHorizontalSpline then ResetSpline(false)
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
  mnuAddNewCtrlPoint.Visible :=
    (transformType = ttSplineV) or (transformType = ttSplineH);

  GetCursorPos(popupPoint);
  popupPoint := ScreenToClient(popupPoint);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAddNewCtrlPointClick(Sender: TObject);
var
  len: integer;
begin
  //add an extra spline control point
  if not Assigned(buttonGroup) then Exit;

  len := Length(ctrlPoints);
  SetLength(ctrlPoints, len +1);
  ctrlPoints[len] := PointD(popupPoint);
  with buttonGroup.AddButton(PointD(popupPoint)) do
    CursorId := crSizeAll;
  DoTransform;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDblClick(Sender: TObject);
begin
  if (transformType <> ttSplineV) and (transformType <> ttSplineH) then Exit;
  GetCursorPos(popupPoint);
  popupPoint := ScreenToClient(popupPoint);
  mnuAddNewCtrlPointClick(nil);
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) then Exit; //popup menu

  clickPoint := Types.Point(X,Y);
  clickedLayer := layeredImage.GetLayerAt(clickPoint);
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

  //if not clicked-moving a layer, then update the cursor and exit.
  if not (ssLeft in Shift) then
  begin
    //get the top-most 'clickable' layer under the mouse cursor
    layer := layeredImage.GetLayerAt(pt);
    if Assigned(layer) then
      Cursor := layer.CursorId else
      Cursor := crDefault;
    Exit;
  end;
  if not Assigned(clickedLayer) then Exit;

  if clickedLayer = transformLayer then
  begin
    dx := pt.X - clickPoint.X;
    dy := pt.Y - clickPoint.Y;
    clickPoint := pt;
    ctrlPoints := OffsetPath(ctrlPoints, dx, dy);
    clickedLayer.Offset(dx, dy);
    if Assigned(buttonGroup) then buttonGroup.Offset(dx, dy);
    if Assigned(rotateGroup) and not allowRotatePivotMove then
      rotateGroup.Offset(dx, dy);
    Invalidate;
  end else if clickedLayer.Parent = rotateGroup then
  begin
    if clickedLayer = rotateGroup.PivotButton then
    begin
      //moving the pivot button in the rotation group
      dx := pt.X - clickPoint.X;
      dy := pt.Y - clickPoint.Y;
      clickPoint := pt;
      rotateGroup.Offset(dx, dy);
      transformLayer.PivotPt := rotateGroup.PivotButton.MidPoint;
    end else
    begin
      //moving the angle button in the rotation group
      clickedLayer.PositionCenteredAt(pt.X, pt.Y);
      //we could do the rotation here, but it's
      //much smoother when done via the AppIdle event.
      doTransformOnIdle := True;
    end;
    Invalidate;
  end
  else if clickedLayer.Parent = buttonGroup then
  begin
    //clicking a general purpose button (layer)

    //if skewing, keep the buttons axis aligned
    if mnuVertSkew.Checked then
      pt.X := Round(clickedLayer.MidPoint.X);
    if mnuHorizontalSkew.Checked then
      pt.Y := Round(clickedLayer.MidPoint.Y);

    idx := clickedLayer.Index;
    if mnuVertProjective.Checked then
    begin
      //get the index of the moving button's vertical partner
      //noting that there are 4 buttons in the group ...
      altIdx := 3 - idx;
      ctrlPoints[altIdx].X := pt.X;
      buttonGroup[altIdx].PositionCenteredAt(ctrlPoints[altIdx]);
    end;
    clickedLayer.PositionCenteredAt(pt.X, pt.Y);
    ctrlPoints[idx] := PointD(pt);
    doTransformOnIdle := true;
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
  transformLayer.Image.LoadFromFile(OpenDialog1.FileName);
  transformLayer.Image.CropTransparentPixels;
  case transformType of
    ttAffineSkew:   ResetSkew(mnuVertSkew.Checked);
    ttProjective:   ResetVertProjective;
    ttSplineV:       ResetSpline(true);
    ttSplineH:       ResetSpline(false);
    ttAffineRotate: ResetRotate;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuPastefromClipboardClick(Sender: TObject);
begin
  if TImage32.CanPasteFromClipboard and
    transformLayer.Image.PasteFromClipboard then
  begin
    transformLayer.Image.CropTransparentPixels;
    case transformType of
      ttAffineSkew  : ResetSkew(mnuVertSkew.Checked);
      ttProjective  : ResetVertProjective;
      ttSplineV      : ResetSpline(True);
      ttSplineH      : ResetSpline(False);
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

procedure TForm1.Reset1Click(Sender: TObject);
begin
  transformLayer.Image.LoadFromResource('UNION_JACK', 'BMP');
  transformLayer.ImageBak.Assign(transformLayer.Image);
  transformLayer.UpdateHitTestMask;
  transformLayer.AutoPivot := true;
  transformlayer.PositionCenteredAt(PointD(ClientWidth/2, ClientHeight/2));
  mnuVerticalSplineClick(mnuVertSkew);
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
