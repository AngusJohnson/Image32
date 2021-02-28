unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, Dialogs, ShellApi,
  Image32, Image32_Layers, Image32_Ttf, Image32_Draw;

type

  //----------------------------------------------------------------------
  // A couple of custom layer classes ...
  //----------------------------------------------------------------------

  TMyVectorLayer32 = class(TVectorLayer32) //for vector drawn layers
  private
    fPenColor: TColor32;
    fBrushColor: TColor32;
  public
    procedure InitRandomColors;
    procedure ScaleAndDrawVector;
  end;

  TMyRasterLayer32 = class(TRasterLayer32) //for imported raster image layers
  public
    procedure InitAndPosition(const pt: TPoint);
  end;

  //----------------------------------------------------------------------
  //----------------------------------------------------------------------

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    SaveDialog1: TSaveDialog;
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
    OpenDialog1: TOpenDialog;
    PastefromClipboard1: TMenuItem;
    Action1: TMenuItem;
    mnuAddImage2: TMenuItem;
    mnuAddRectangle: TMenuItem;
    mnuAddEllipse: TMenuItem;
    mnuAddText: TMenuItem;
    N5: TMenuItem;
    mnuDeleteLayer2: TMenuItem;
    N6: TMenuItem;
    mnuBringtoFront2: TMenuItem;
    mnuSendtoBack2: TMenuItem;
    mnuAddImage: TMenuItem;
    mnuCutToClipboard: TMenuItem;
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuCopytoClipboardClick(Sender: TObject);
    procedure mnuBringToFrontClick(Sender: TObject);
    procedure mnuSendToBackClick(Sender: TObject);
    procedure mnuDeleteLayerClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure mnuAddEllipseClick(Sender: TObject);
    procedure mnuAddRectangleClick(Sender: TObject);
    procedure mnuAddTextClick(Sender: TObject);
    procedure mnuPastefromClipboardClick(Sender: TObject);
    procedure mnuAddImageClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure mnuCutToClipboardClick(Sender: TObject);
  private
    layeredImage32: TLayeredImage32;

    fontReader: TFontReader; //TTF font reader
    fontCache: TGlyphCache;

    wordStrings: TStringList;

    activeLayerMouseDown: Boolean;
    activeLayer: TLayer32;
    activeDesign: TDesignerLayer32;
    activeButton: TButtonDesignerLayer32;
    activeButtonGroup: TGroupLayer32;
    popupPoint: TPoint;
    clickPoint: TPoint;
    function AddRectangle(const pt: TPoint): TMyVectorLayer32;
    function AddEllipse(const pt: TPoint): TMyVectorLayer32;
    function AddText(const pt: TPoint): TMyVectorLayer32;
    procedure DeleteButtons;
    procedure UpdateDesignLayer;
    procedure MakeLayerActive(layer: TLayer32);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoDropImage(const filename: string);
  protected
    procedure WMDROPFILES(var msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{$R WORDS.RES}
{$R FONT.RES}

uses
  Image32_BMP, Image32_PNG, Image32_JPG,
  Image32_Vector, Image32_Extra, Image32_Clipper;

const
  margin = 100;


//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function MakeDarker(color: TColor32; percent: integer): TColor32;
var
  pcFrac: double;
  r: TARGB absolute Result;
begin
  percent := Max(0, Min(100, percent));
  pcFrac := percent/100;
  Result := color;
  r.R := r.R - Round(r.R * pcFrac);
  r.G := r.G - Round(r.G * pcFrac);
  r.B := r.B - Round(r.B * pcFrac);
end;

//------------------------------------------------------------------------------
// TMyVectorLayer32
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.InitRandomColors;
var
  hsl: THsl;
begin
  hsl.hue := Random(256);
  hsl.sat := 240;
  hsl.lum := 200;
  hsl.Alpha := 128;
  fBrushColor := HslToRgb(hsl);
  fPenColor := MakeDarker(fBrushColor, 60) or $FF000000;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.ScaleAndDrawVector;
var
  drawRect: TRect;
  vectorRect: TRectD;
  scaleX, scaleY: double;
begin
  CursorId := crHandPoint;
  //stretch the vectors so they fit perfectly into the layer
  drawRect := Bounds;
  drawRect := Image32_Vector.InflateRect(drawRect, -2, -2);
  vectorRect := GetBoundsD(paths);
  scaleX := drawRect.Width/vectorRect.Width;
  scaleY := drawRect.Height/vectorRect.Height;
  paths := ScalePath(paths, scaleX, scaleY);
  paths := OffsetPath(paths,
    2 - vectorRect.Left*scaleX, 2 - vectorRect.Top*scaleY);
  DrawPolygon(Image, paths, frEvenOdd, fBrushColor);
  DrawLine(Image, paths, 4, fPenColor, esPolygon);
  UpdateHitTestMask(paths, frEvenOdd);
end;

//------------------------------------------------------------------------------
// TMyRasterLayer32 methods
//------------------------------------------------------------------------------

procedure TMyRasterLayer32.InitAndPosition(const pt: TPoint);
begin
  MasterImage.CropTransparentPixels;
  image.Assign(MasterImage);
  PositionCenteredAt(pt);
  CursorId := crHandPoint;
  UpdateHitTestMaskTransparent;
end;

//------------------------------------------------------------------------------
// TForm1 methods
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  resStream: TResourceStream;
begin
  Randomize;
  DefaultButtonSize := DPIAware(10); //ie the layered image's control buttons

  OnKeyDown := DoKeyDown;

  //SETUP LAYEREDIMAGE32 (a non-visual class whose merged
  //images will be manually drawn onto the form canvas).
  layeredImage32 := TLayeredImage32.Create; //sized in FormResize() below.
  layeredImage32.AddLayer(TDesignerLayer32, nil, 'grid');

  fontReader := TFontReader.Create;
  fontReader.LoadFromResource('FONT_NSB', RT_RCDATA);
  fontCache := TGlyphCache.Create(fontReader, DPIAware(48));

  wordStrings := TStringList.Create;

  resStream := TResourceStream.Create(hInstance, 'WORDS', RT_RCDATA);
  try
    wordStrings.LoadFromStream(resStream);
  finally
    resStream.Free;
  end;

  DragAcceptFiles(Handle, True);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
  wordStrings.Free;
  fontCache.Free;
  fontReader.Free;
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.WMDROPFILES(var msg: TWMDropFiles);
var
  dropHdl: HDROP;
  len: integer;
  filename: string;
begin
  dropHdl := msg.Drop;
  len := DragQueryFile(dropHdl, 0, nil, 0);
  SetLength(filename, len);
  DragQueryFile(dropHdl, 0, PChar(FileName), len + 1);
  OpenDialog1.InitialDir := ExtractFilePath(filename);
  OpenDialog1.FileName := filename;
  DragFinish(dropHdl);
  msg.Result := 0;     //ie message has been handled.
  DoDropImage(filename);
end;
//------------------------------------------------------------------------------

procedure TMainForm.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 1; //don't erase!
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  if (csDestroying in ComponentState) then Exit;

  layeredImage32.SetSize(ClientWidth, ClientHeight);

  if (layeredImage32.Count > 0) and
    (layeredImage32[0] is TDesignerLayer32) then
      with TDesignerLayer32(layeredImage32.Root[0]) do
      begin
        SetSize(layeredImage32.Width, layeredImage32.Height);
        HatchBackground(Image);
      end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddEllipseClick(Sender: TObject);
begin
  MakeLayerActive(AddEllipse(popupPoint));
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddRectangleClick(Sender: TObject);
begin
  MakeLayerActive(AddRectangle(popupPoint));
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddTextClick(Sender: TObject);
begin
  MakeLayerActive(AddText(popupPoint));
end;
//------------------------------------------------------------------------------

function TMainForm.AddRectangle(const pt: TPoint): TMyVectorLayer32;
begin
  Result := TMyVectorLayer32(layeredImage32.AddLayer(TMyVectorLayer32));
  with TMyVectorLayer32(Result) do
  begin
    InitRandomColors;
    SetSize(DPIAware(50 + Random(200)), DPIAware(50 + Random(200)));
    paths := Image32_Vector.Paths(Rectangle(Bounds));
    CursorId := crHandPoint;
    ScaleAndDrawVector;
  end;
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

function TMainForm.AddEllipse(const pt: TPoint): TMyVectorLayer32;
begin
  Result := TMyVectorLayer32(layeredImage32.AddLayer(TMyVectorLayer32));
  with TMyVectorLayer32(Result) do
  begin
    InitRandomColors;
    SetSize(DPIAware(50 + Random(200)), DPIAware(50 + Random(200)));
    paths := Image32_Vector.Paths(Ellipse(Bounds));
    ScaleAndDrawVector;
  end;
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

function TMainForm.AddText(const pt: TPoint): TMyVectorLayer32;
var
  rec: TRect;
  randomWord: string;
  tmpPaths: TPathsD;
begin

  //create a vector layer
  Result := TMyVectorLayer32(layeredImage32.AddLayer(TMyVectorLayer32));
  with Result as TMyVectorLayer32 do
  begin
    InitRandomColors;
    randomWord := wordStrings[Random(wordStrings.Count)];
    fontCache.GetTextGlyphs(0,0,randomWord, tmpPaths);
    Paths := ScalePath(tmpPaths, 1, 2.0);
    rec := Image32_Vector.GetBounds(paths);
    SetSize(RectWidth(rec),RectHeight(rec));
    ScaleAndDrawVector;
  end;
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddImageClick(Sender: TObject);
var
  rasterLayer: TMyRasterLayer32;
begin
  //add a raster image
  if not OpenDialog1.Execute then Exit;
  rasterLayer := TMyRasterLayer32(layeredImage32.AddLayer(TMyRasterLayer32));
  rasterLayer.MasterImage.LoadFromFile(OpenDialog1.FileName);
  rasterLayer.InitAndPosition(Point(layeredImage32.MidPoint));
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuPastefromClipboardClick(Sender: TObject);
var
  rasterLayer: TMyRasterLayer32;
begin
  //paste (add) a raster image
  if not TImage32.CanPasteFromClipBoard then Exit;
  DeleteButtons;
  rasterLayer := TMyRasterLayer32(layeredImage32.AddLayer(TMyRasterLayer32));
  rasterLayer.MasterImage.PasteFromClipBoard;
  rasterLayer.InitAndPosition(Point(layeredImage32.MidPoint));
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DoDropImage(const filename: string);
var
  rasterLayer: TMyRasterLayer32;
  pt: TPoint;
begin
  //drag drop (add) a raster image
  if not TImage32.IsRegisteredFormat(ExtractFileExt(filename)) then Exit;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  DeleteButtons;
  rasterLayer := TMyRasterLayer32(layeredImage32.AddLayer(TMyRasterLayer32));
  rasterLayer.MasterImage.LoadFromFile(FileName);
  rasterLayer.InitAndPosition(pt);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRect: TRect;
begin
  //copy layeredImage32 onto pnlMain.Bitmap
  //nb: layeredImage32.GetMergedImage can also return the portion of
  //the image that's changed since the previous GetMergedImage call.
  //Updating only the changed region significantly speeds up drawing.
  with layeredImage32.GetMergedImage(false, updateRect) do
  begin
    CopyToDc(updateRect, self.Canvas.Handle,
      updateRect.Left, updateRect.Top, false);
  end;
end;
//------------------------------------------------------------------------------


procedure TMainForm.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and Assigned(activeLayer) then
  begin
    MakeLayerActive(nil); //deselect the active control
    Key := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DeleteButtons;
begin
  FreeAndNil(activeDesign);
  FreeAndNil(activeButtonGroup);
  activeLayer := nil;
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateDesignLayer;
var
  htOutline: TPathsD;
  dashLen, dashMargin: integer;
  alBounds: TRect;
begin
  //with activeDesign layer, draw a dashed outline of the image or vector
  if not assigned(activeLayer) or not assigned(activeDesign) then Exit;
  dashMargin := DPIAware(4);
  alBounds := Image32_Vector.InflateRect(activeLayer.Bounds,
    dashMargin, dashMargin);
  activeDesign.SetBounds(alBounds);
  with activeLayer as THitTestLayer32 do
    htOutline := GetPathsFromHitTestMask;
  htOutline := OffsetPath(htOutline, dashMargin, dashMargin);
  htOutline := Image32_Clipper.InflatePaths(htOutline, dashMargin);
  dashLen := DPIAware(5);
  activeDesign.Image.Clear;
  DrawDashedLine(activeDesign.Image,
    htOutline, [dashLen, dashLen], nil, DPIAware(1), clMaroon32, esPolygon);
end;
//------------------------------------------------------------------------------

procedure TMainForm.MakeLayerActive(layer: TLayer32);
begin
  if activeLayer = layer then Exit;
  DeleteButtons;
  activeLayer := layer;
  if assigned(layer) then
  begin
    //activeDesign := TDesignerLayer32(layeredImage32.AddLayer(TDesignerLayer32));
    UpdateDesignLayer;
    activeButtonGroup :=
      CreateSizingButtonGroup(layer, ssCorners, bsRound, DPIAware(10), clGreen32);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  clickedLayer: TLayer32;
begin
  clickPoint := Types.Point(X,Y);

  //get the clicked layer (if any)
  clickedLayer := layeredImage32.GetLayerAt(clickPoint);

  //if a control button was clicked then get ready to move it
  if Assigned(clickedLayer) and (clickedLayer is TButtonDesignerLayer32) then
  begin
    activeButton := TButtonDesignerLayer32(clickedLayer);
    //the activeLayer can't be a TButtonDesignerLayer32 so exit here
    Exit;
  end;

  activeButton := nil;
  //if anything but the activelayer was clicked then delete buttons
  if assigned(activeLayer) and  (activeLayer <> clickedLayer) then
    DeleteButtons;

  if Assigned(clickedLayer) then //assign a new activeLayer
  begin
    if not Assigned(activeLayer) then MakeLayerActive(clickedLayer);
    activeLayerMouseDown := true;
  end else
    Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy: integer;
  pt: TPoint;
  layer: TLayer32;
  rec: TRect;
begin
  pt := Types.Point(X,Y);

  dx := pt.X - clickPoint.X;
  dy := pt.Y - clickPoint.Y;
  clickPoint := pt;

  if activeLayerMouseDown then              //about to move a 'normal' layer
  begin
    activeLayer.Offset(dx, dy);
    if assigned(activeButtonGroup) then
       activeButtonGroup.Offset(dx, dy);
    if assigned(activeDesign) then
      activeDesign.Offset(dx, dy);
  end
  else if assigned(activeButton) then       //moving a button (layer)
  begin
    activeButton.Offset(dx, dy);

    //get and assign the new bounds for activeLayer using sizing button group
    rec := Rect(UpdateSizingButtonGroup(activeButton));
    activeLayer.SetBounds(rec);

    if activeLayer is TMyVectorLayer32 then
      with TMyVectorLayer32(activeLayer) do
    begin
      ScaleAndDrawVector;
    end
    else if activeLayer is TMyRasterLayer32 then
      with TMyRasterLayer32(activeLayer) do
    begin
      Image.Copy(MasterImage, MasterImage.Bounds, Image.Bounds);
      UpdateHitTestMaskTransparent;
    end;
    UpdateDesignLayer;
  end else
  begin
    //not moving anything so just update the cursor
    layer := layeredImage32.GetLayerAt(pt);
    if Assigned(layer) then
      Cursor := layer.CursorId else
      Cursor := crDefault;
    Exit;
  end;

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  activeLayerMouseDown := false;
  activeButton := nil;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuCutToClipboardClick(Sender: TObject);
begin
  if Assigned(activeLayer) then
  begin
    activeLayer.Image.CopyToClipBoard;
    layeredImage32.DeleteLayer(activeLayer);
    DeleteButtons;
    Invalidate;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuCopytoClipboardClick(Sender: TObject);
begin
  if Assigned(activeLayer) then
    activeLayer.Image.CopyToClipBoard;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuBringToFrontClick(Sender: TObject);
begin
  if not assigned(activeLayer) then Exit;
  if activeLayer.Index = activeLayer.GroupOwner.ChildCount -1 then Exit;
  if activeLayer.BringForwardOne then
    Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuSendToBackClick(Sender: TObject);
var
  oldIdx: integer;
begin
  oldIdx := activeLayer.Index;
  //send to bottom except for the hatched background (index = 0)
  if oldIdx = 1 then Exit;
  if activeLayer.SendBackOne then
    Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuDeleteLayerClick(Sender: TObject);
begin
  if not assigned(activeLayer) then Exit;
  layeredImage32.DeleteLayer(activeLayer);
  DeleteButtons;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  mnuBringToFront.Enabled := assigned(activeLayer) and
    (activeLayer.Index < layeredImage32.Root.ChildCount -1);
  mnuBringtoFront2.Enabled := mnuBringtoFront.Enabled;
  mnuSendToBack.Enabled := assigned(activeLayer) and (activeLayer.Index > 1);
  mnuSendtoBack2.Enabled := mnuSendtoBack.Enabled;
  mnuDeleteLayer.Enabled := assigned(activeLayer);
  mnuDeleteLayer2.Enabled := mnuDeleteLayer.Enabled;

  if Sender = PopupMenu1 then
  begin
    GetCursorPos(popupPoint);
    popupPoint := ScreenToClient(popupPoint);
  end else
    popupPoint := Point(layeredImage32.MidPoint);
end;
//------------------------------------------------------------------------------

end.
