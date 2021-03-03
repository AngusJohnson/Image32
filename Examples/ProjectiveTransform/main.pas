unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, JPEG,
  Controls, Forms, Dialogs, ExtCtrls, Menus,
  Image32, Image32_Layers, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Samples.Spin;

type

  //TPanel subclassed to allow painting directly onto its canvas
  TPanel = class(ExtCtrls.TPanel)
  private
    fOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
  end;

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    Panel1: TPanel;
    btnTransform: TButton;
    N1: TMenuItem;
    ransform1: TMenuItem;
    mnuClearTransform: TMenuItem;
    mnuStartTransform: TMenuItem;
    gbMargins: TGroupBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    Help1: TMenuItem;
    mnuHowTo: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlLeftResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlLeftMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlLeftMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlLeftMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnTransformClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure mnuClearTransformClick(Sender: TObject);
    procedure mnuHowToClick(Sender: TObject);
  private
    srcPts, dstPts, dstMargins: TPathD;
    srcBtnGroup: TGroupLayer32;
    dstBtnGroup: TGroupLayer32;
    updateNeeded: Boolean;
    oldImageWidth: integer;
    transformedImage: TImage32;
    layeredImage: TLayeredImage32;
    rasterLayer: TRasterLayer32;
    designLayer: TDesignerLayer32;
    clickedLayer: TLayer32;
    clickedPoint: TPoint;
    procedure ResetImage;
    procedure PanelPaint(Sender: TObject);
    procedure PaintDesignerLayer;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


uses
  Image32_BMP, Image32_PNG, Image32_JPG,
  Image32_Vector, Image32_Draw, Image32_Transform;


//------------------------------------------------------------------------------
// (subclassed) TPanel
//------------------------------------------------------------------------------

procedure TPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if Assigned(fOnPaint) then //speeds things up and avoids flicker :)
    message.Result := 1 else
    inherited;
end;
//------------------------------------------------------------------------------

procedure TPanel.Paint;
begin
  if Assigned(fOnPaint) then
    fOnPaint(Self) else
    inherited Paint;
end;
//------------------------------------------------------------------------------

procedure TPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if Assigned(fOnPaint) then
  begin
    //paint the full background when resizing
    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(ClientRect);
  end;
end;

//------------------------------------------------------------------------------
// TForm1 methods
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  img: TImage32;
begin
  Application.OnIdle := OnIdle;

  pnlLeft.OnPaint := PanelPaint;
  pnlRight.OnPaint := PanelPaint;

  //layered image (on left side of form)
  layeredImage := TLayeredImage32.Create;
  layeredImage.BackgroundColor := Color32(pnlLeft.Color);

  //raster image layer (will contain the image to be transformed).
  rasterLayer := layeredImage.AddLayer(TRasterLayer32) as TRasterLayer32;

  //designer layer to display design quadrilaterals
  designLayer := layeredImage.AddLayer(TDesignerLayer32) as TDesignerLayer32;

  //non-layered image (on right side of form)
  transformedImage := TImage32.Create;

end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  transformedImage.Free;
  FreeAndNil(layeredImage);
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlLeftResize(Sender: TObject);
var
  i, w,h: integer;
  rec: TRect;
  scale: double;
begin
  if not Assigned(layeredImage) then
    Exit; //ie when form destroying

  //ON THE LEFT SIDE ...

  pnlLeft.Width := ClientWidth div 2;

  with rasterLayer do
    if not MasterImage.IsEmpty then
    begin
      Image.Assign(MasterImage);
      Image.ScaleToFit(pnlLeft.ClientWidth, pnlLeft.ClientHeight);

      scale := Image.Width/oldImageWidth;
      oldImageWidth := Image.Width;

      //rescale control points
      FreeAndNil(srcBtnGroup);
      FreeAndNil(dstBtnGroup);

      srcPts := ScalePath(srcPts, scale);
      srcBtnGroup := CreateButtonGroup(layeredImage.Root, srcPts,
        TButtonShape.bsRound, DefaultButtonSize, clRed32);

      if assigned(dstPts) then
      begin
        dstPts := ScalePath(dstPts, scale);
        dstBtnGroup := CreateButtonGroup(layeredImage.Root, dstPts,
          TButtonShape.bsRound, DefaultButtonSize, clBlue32);
      end;

      //resize layeredImage to the size of the scaled image
      layeredImage.SetSize(Image.Width, Image.Height);
    end;

  //resize designLayer too
  with designLayer do
    SetSize(layeredImage.Width, layeredImage.Height);
  PaintDesignerLayer;

  //ON THE RIGHT SIDE ...

  pnlRight.Left := pnlLeft.Left + pnlLeft.Width;
  pnlRight.Width := ClientWidth div 2;

  gbMargins.Left := pnlRight.Left;

  if not transformedImage.IsEmpty then
    transformedImage.ScaleToFit(pnlRight.ClientWidth, pnlRight.ClientHeight);

end;
//------------------------------------------------------------------------------

procedure TForm1.PanelPaint(Sender: TObject);
var
  updateRec: TRect;
begin
  with TPanel(Sender) do
  begin
    if Sender = pnlLeft then
    begin
      //partial repainting (faster for button moves)
      with layeredImage.GetMergedImage(false, updateRec) do
        CopyToDc(updateRec, Canvas.Handle,
          updateRec.Left, updateRec.Top, false);
    end
    else if Sender = pnlRight then
    begin
      //repaint the whole image
      with transformedImage do
        CopyToDc(Bounds, Bounds, {Sender's}Canvas.Handle, false);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlLeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickedPoint := Point(X,Y);
  clickedLayer := layeredImage.GetLayerAt(clickedPoint);
end;
//------------------------------------------------------------------------------

function SafePoint(const pt: TPoint; const bounds: TRect): TPoint;
begin
  //restrict 'pt' to the 'bounds'
  Result := pt;
  if Result.X < bounds.Left then Result.X := bounds.Left;
  if Result.X > bounds.Right then Result.X := bounds.Right;
  if Result.Y < bounds.Top then Result.Y := bounds.Top;
  if Result.Y > bounds.Bottom then Result.Y := bounds.Bottom;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlLeftMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  layer: TLayer32;
  i, dx,dy: integer;
  rec: TRect;
  pt: TPoint;
begin

  if not (ssLeft in Shift) then
  begin
    //update the cursor
    layer := layeredImage.GetLayerAt(Point(X,Y));
    if Assigned(layer) then
      pnlLeft.Cursor := layer.CursorId else
      pnlLeft.Cursor := crDefault;
    Exit;
  end;

  if not assigned(clickedLayer) then
    Exit; //ie no button clicked

  //when moving buttons make sure it remains inside the image bounds
  //otherwise we'll lose sight (and control) of it.
  i := DefaultButtonSize div 2;
  rec := rasterLayer.Image.Bounds;
  rec := InflateRect(rec, -i, -i);
  pt := SafePoint(Point(X,Y), rec);

  dx := pt.X - clickedPoint.X;
  dy := pt.Y - clickedPoint.Y;
  clickedPoint := pt;

  //finally move the button and update its corresponding point
  //in the srcPts or dstPts array

  clickedLayer.Offset(dx, dy);
  if clickedLayer.GroupOwner = dstBtnGroup then
    dstPts[clickedLayer.Index] := clickedLayer.MidPoint else
    srcPts[clickedLayer.Index] := clickedLayer.MidPoint;

  PaintDesignerLayer;
  pnlLeft.Invalidate;

  if Assigned(dstPts) then
    updateNeeded := true; //update the transform when idle

end;
//------------------------------------------------------------------------------

procedure TForm1.pnlLeftMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickedLayer := nil;
end;
//------------------------------------------------------------------------------

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
var
  offRec, rec: TRect;
  scale: double;
  scaledSrc, scaledDst: TPathD;
begin
  Done := true;
  if not updateNeeded or not Assigned(dstPts) then Exit;
  updateNeeded := false;

  //get the offset amounts
  offRec.Top := StrToIntDef(SpinEdit2.Text,0);
  offRec.Right := StrToIntDef(SpinEdit3.Text,0);
  offRec.Bottom := StrToIntDef(SpinEdit4.Text,0);
  offRec.Left := StrToIntDef(SpinEdit1.Text,0);

  //transform the master image not the scaled image
  with rasterLayer do
  begin
    scale := MasterImage.Width/Image.Width;
    transformedImage.Assign(MasterImage);
  end;

  //scale the control points to the master image
  scaledSrc := ScalePath(srcPts, scale);
  scaledDst := ScalePath(dstPts, scale);
  offRec := ScaleRect(offRec, scale);

  //TRANSFORM...
  ProjectiveTransform(transformedImage, scaledSrc, scaledDst, offRec);

  transformedImage.ScaleToFit(pnlRight.ClientWidth, pnlRight.ClientHeight);

  //to avoid flicker, we don't repaint (clear) the whole right panel,
  //just the region under or to the right side of the transformed image
  if transformedImage.Height < pnlRight.ClientHeight then
  begin
    rec := ClientRect;
    rec.Top := transformedImage.Height;
  end
  else if transformedImage.Width < pnlRight.ClientWidth then
  begin
    rec := ClientRect;
    rec.Left := transformedImage.Width;
  end;
  pnlRight.Canvas.FillRect(rec);
  pnlRight.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.btnTransformClick(Sender: TObject);
begin
  //turns on transforming
  //(though the actual transforms are done in the OnIdle event)
  btnTransform.Enabled := false;
  if Assigned(dstPts) then Exit;
  dstPts := Rectangle(GetBoundsD(srcPts));
  dstBtnGroup := CreateButtonGroup(layeredImage.Root, dstPts,
    TButtonShape.bsRound, DefaultButtonSize, clBlue32);

  gbMargins.Visible := true;
  PaintDesignerLayer;
  pnlLeft.Invalidate;   //ensures the new buttons will be drawn
  updateNeeded := true; //flag set to transform when idle
end;
//------------------------------------------------------------------------------

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  if Assigned(dstPts) then
    updateNeeded := Assigned(dstPts);
end;
//------------------------------------------------------------------------------

procedure TForm1.PaintDesignerLayer;
begin
  designLayer.Image.Clear;
  if Assigned(srcPts) then
    DrawLine(designLayer.Image, srcPts, DPIAware(1), clRed32, esPolygon);

  if Assigned(dstPts) then
  begin
    DrawLine(designLayer.Image, dstPts, DPIAware(1), clBlue32, esPolygon);
    updateNeeded := true;
  end;
  pnlLeft.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  with rasterLayer do
    MasterImage.LoadFromFile(OpenDialog1.FileName);
  ResetImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.ResetImage;
var
  i: integer;
  rec: TRect;
begin
  FreeAndNil(srcBtnGroup);
  FreeAndNil(dstBtnGroup);
  srcPts := nil;
  dstPts := nil;
  SpinEdit1.Value := 0;
  SpinEdit2.Value := 0;
  SpinEdit3.Value := 0;
  SpinEdit4.Value := 0;
  gbMargins.Visible := false;
  transformedImage.Clear;
  btnTransform.Enabled := false;
  pnlLeft.Canvas.FillRect(pnlRight.ClientRect);
  pnlRight.Canvas.FillRect(pnlRight.ClientRect);

  with rasterLayer do
  begin
    //scale master image
    Image.Assign(MasterImage);
    Image.ScaleToFit(pnlLeft.ClientWidth, pnlLeft.ClientHeight);
    oldImageWidth := Image.Width;
    //reset source buttons to just inside image bounds
    i := DefaultButtonSize div 2;
    rec := Image.Bounds;
    rec := InflateRect(rec, -i, -i);
    srcPts := Rectangle(rec);
    srcBtnGroup := CreateButtonGroup(layeredImage.Root, srcPts,
      TButtonShape.bsRound, DefaultButtonSize, clRed32);

    //resize layeredImage to the size of the scaled image
    layeredImage.SetSize(Image.Width, Image.Height);

    //and resize the designer layer too.
    designLayer.SetSize(Image.Width, Image.Height);
  end;
  btnTransform.Enabled := true;
  PaintDesignerLayer;
  pnlLeft.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.Save1Click(Sender: TObject);
begin
  if transformedImage.IsEmpty or not SaveDialog1.Execute then Exit;
    transformedImage.SaveToFile(SaveDialog1.FileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuClearTransformClick(Sender: TObject);
begin
  if not Assigned(dstPts) then Exit;
  btnTransform.Enabled := true;
  dstPts := nil;
  FreeAndNil(dstBtnGroup);
  transformedImage.SetSize(0,0);
  pnlLeft.Invalidate;
  pnlRight.Canvas.FillRect(pnlRight.ClientRect);
  PaintDesignerLayer;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuHowToClick(Sender: TObject);
begin
  ShowMessage('Steps:'#10+
    '1. Open an image file'#10+
    '2. Position the red ''Source'' quadrilateral'#10+
    '3. Click the ''Start Transform'' button'#10+
    '4. Position the blue ''Destination'' quadrilateral'#10+
    '    and possibly reposition the ''Source'' quadrilateral too'#10+
    '5. Adjust the margins to include more or less of the image'#10+
    '6. Save the transformed image.');
end;
//------------------------------------------------------------------------------

end.
