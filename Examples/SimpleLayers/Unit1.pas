unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, Dialogs,
  Image32, Image32_Layers, BitmapPanels;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    SaveDialog1: TSaveDialog;
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
    procedure mnuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuSaveClick(Sender: TObject);
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
  private
    masterImageList: TImageList32;
    layeredImage32: TLayeredImage32;
    activeLayerMouseDown: Boolean;
    activeLayer: TLayer32;
    activeButtonLayer: TButtonDesignerLayer32;
    buttonGroupId: integer;
    popupPoint: TPoint;
    clickPoint: TPoint;
    function AddRectangle(const pt: TPoint): TLayer32;
    function AddEllipse(const pt: TPoint): TLayer32;
    function AddText(const pt: TPoint): TLayer32;
    procedure DeleteButtons;
    procedure MakeLayerActive(layer: TLayer32);
    procedure PaintLayeredImage;
    procedure DoDropImage(Sender: TObject; const filename: string);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SizeAndDrawLayer(layer: TLayer32; newWidth, newHeight: integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Image32_BMP, Image32_PNG, Image32_JPG, Image32_Draw, Image32_Vector,
  Image32_Extra, Image32_Text;

const
  margin = 100;

//------------------------------------------------------------------------------
// TMyImageLayer32: just some fun so only opaque pixels can be clicked in images
//------------------------------------------------------------------------------

type
  TMyImageLayer32 = class(TLayer32)
  protected
    function HitTest(const pt: TPoint): Boolean; override;
  end;

function TMyImageLayer32.HitTest(const pt: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, pt);
  if not Result then Exit;
  //nb: pt is relative to layeredImage32, so offset so it's relative to Image
  with OffsetPoint(pt, -left,-top) do
    Result := (Image.Pixel[X, Y] shr 24 > 0);
end;

//------------------------------------------------------------------------------
// TForm1 methods
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  layer: TLayer32;
  rec: TRect;
begin
  Randomize;
  DefaultButtonSize := DPI(10); //ie the layered image's control buttons

  //SETUP THE DISPLAY PANEL
  Panel1.BorderWidth := DPI(16);
  Panel1.BevelInner := bvLowered;
  //set Panel1.Tabstop := true to enable keyboard controls
  Panel1.TabStop := true;
  Panel1.FocusedColor := clGradientInactiveCaption;
  Panel1.BitmapProperties.Scale := 1;
  //enable (image) file drop
  Panel1.BitmapProperties.FileDropEnabled := true;
  Panel1.BitmapProperties.OnFileDrop := DoDropImage;
  //capture key events so ESC will clear ActiveLayer
  Panel1.BitmapProperties.OnKeyDown := DoKeyDown;

  //MASTER IMAGE LIST
  masterImageList := TImageList32.Create;

  //SETUP LAYEREDIMAGE32

  rec := Panel1.InnerClientRect;
  layeredImage32 :=
    TLayeredImage32.Create(RectWidth(rec), RectHeight(rec));
  //we don't need to worry about BackgroundColor here as
  //we're using a hatched image for the background instead.

  layer := layeredImage32.AddNewLayer(TDesignerLayer32, 'hatched bkgnd');
  //layer.SetSize() is the same as layer.image.SetSize()
  layer.SetSize(layeredImage32.Width, layeredImage32.Height);
  HatchBackground(layer.Image, $FFF0F0F0, clWhite32);

  //add a dummy image to masterImageList to simplify
  //sync'ing masterImageList with layeredImage32's layers.
  masterImageList.Add(nil);

  //DISPLAY STUFF - so far, just the hatched background :)
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAddEllipseClick(Sender: TObject);
begin
  MakeLayerActive(AddEllipse(popupPoint));
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAddRectangleClick(Sender: TObject);
begin
  MakeLayerActive(AddRectangle(popupPoint));
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAddTextClick(Sender: TObject);
begin
  MakeLayerActive(AddText(popupPoint));
end;
//------------------------------------------------------------------------------

function TForm1.AddRectangle(const pt: TPoint): TLayer32;
begin
  masterImageList.Add(nil); //just a place filler
  Result := layeredImage32.AddNewLayer('rectangle');
  SizeAndDrawLayer(Result, 128, 192);
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

function TForm1.AddEllipse(const pt: TPoint): TLayer32;
begin
  masterImageList.Add(nil); //just a place filler
  Result := layeredImage32.AddNewLayer('ellipse');
  SizeAndDrawLayer(Result, 192, 150);
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

function TForm1.AddText(const pt: TPoint): TLayer32;
var
  lf: TLogfont;
  rec: TRect;
  paths: TArrayOfArrayOfPointD;
  endPt: TPointD;
const
  RandText: array [0..5] of string =
    ('this','is','a','good','graphics','sample');
begin
  masterImageList.Add(nil); //just a place filler

  lf := DefaultLogfont;
  lf.lfHeight := -96;
  lf.lfFaceName := 'Impact';

  //get the text's outline and store it in Result.HitTestRegions
  //ready to draw it (see DrawLayer).
  Result := layeredImage32.AddNewLayer(RandText[Random(6)]);
  paths := GetTextOutline(10, 90,
    Result.Name, GetFontInfo(lf), taLeft, endPt);
  Result.HitTestRegions := paths;

  rec := GetBounds(paths);
  SizeAndDrawLayer(Result, RectWidth(rec), RectHeight(rec));
  Result.PositionCenteredAt(pt);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuAddImageClick(Sender: TObject);
var
  masterImage: TImage32;
begin
  //add a raster image
  if not OpenDialog1.Execute then Exit;
  masterImage := TImage32.Create;
  masterImageList.Add(masterImage);
  masterImage.LoadFromFile(OpenDialog1.FileName);
  masterImage.CropTransparentPixels;
  with layeredImage32.AddNewLayer(TMyImageLayer32) do
  begin
    image.Assign(masterImage);
    PositionCenteredAt(layeredImage32.MidPoint);
  end;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.SizeAndDrawLayer(layer: TLayer32; newWidth, newHeight: integer);
var
  paths: TArrayOfArrayOfPointD;
  img: TImage32;
  rec: TRect;
begin
  //draws everything except raster images
  img := layer.Image;
  img.SetSize(newWidth, newHeight);
  if img.IsEmpty then Exit;
  rec := Rect(0,0, newWidth, newHeight);

  if layer.Name = '' then
  begin
    //layer contains a raster image
    layer.Image.Assign(masterImageList[layer.Index]);
    layer.Image.Resize(newWidth, newHeight);
  end
  else if layer.Name = 'rectangle' then
  begin
    //allow room for line drawing (below)
    Windows.InflateRect(rec, -3, -3);
    SetLength(paths, 1);
    paths[0] := Rectangle(rec);
    DrawPolygon(img, paths, frNonZero, $AAFFDD66);
    DrawLine(img, paths, 6, $FFAA6600, esClosed);
  end
  else if layer.Name = 'ellipse' then
  begin
    //allow room for line drawing (below)
    Windows.InflateRect(rec, -3, -3);
    SetLength(paths, 1);
    paths[0] := Ellipse(rec);
    DrawPolygon(img, paths, frNonZero, $AA99FF99);
    DrawLine(img, paths, 6, clGreen32, esClosed);
  end else
  begin
    //layer contains text
    if not assigned(layer.HitTestRegions) then Exit;
    rec := GetBounds(layer.HitTestRegions);
    paths := OffsetPath(layer.HitTestRegions, -rec.Left, -rec.Top);
    paths := ScalePath(paths, layer.width/rec.Width, layer.Height/rec.Height);

    DrawShadow(img, paths, frNonZero, 3);
    DrawPolygon(img, paths, frNonZero, $FF00DD00);
    Draw3D(img, paths, frNonZero, 3,4);
    DrawLine(img, paths, 1, clBlack32, esClosed);
  end;
  if layer.Name <> '' then
    layer.HitTestRegions := paths;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuPastefromClipboardClick(Sender: TObject);
var
  masterImage: TImage32;
begin
  //paste (add) a raster image
  if not TImage32.CanPasteFromClipBoard then Exit;
  masterImage := TImage32.Create;
  masterImageList.Add(masterImage);
  masterImage.PasteFromClipBoard;
  masterImage.CropTransparentPixels;
  with layeredImage32.AddNewLayer(TMyImageLayer32) do
  begin
    image.Assign(masterImage);
    PositionCenteredAt(layeredImage32.MidPoint);
  end;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.DoDropImage(Sender: TObject; const filename: string);
var
  masterImage: TImage32;
  pt: TPoint;
begin
  //drag drop (add) a raster image
  if not TImage32.IsRegisteredFormat(ExtractFileExt(filename)) then Exit;

  GetCursorPos(pt);
  pt := Panel1.ScreenToClient(pt);
  Panel1.ClientToBitmap(pt);

  masterImage := TImage32.Create;
  masterImageList.Add(masterImage);
  masterImage.LoadFromFile(FileName);
  masterImage.CropTransparentPixels;
  with layeredImage32.AddNewLayer(TMyImageLayer32) do
  begin
    image.Assign(masterImage);
    PositionCenteredAt(pt);
  end;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.PaintLayeredImage;
begin
  //nb: BitmapPanels doesn't have to be used as it is here.
  //Image32 can draw to any component containing a TCanvas control.
  {$IFDEF SETSIZE}
  Panel1.Bitmap.SetSize(layeredImage32.Width, layeredImage32.Height);
  {$ELSE}
  Panel1.Bitmap.Width := layeredImage32.Width;
  Panel1.Bitmap.Height := layeredImage32.Height;
  {$ENDIF}
  layeredImage32.GetMergedImage(false).CopyToDc(Panel1.Bitmap.Canvas.Handle);
  Panel1.Refresh;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  masterImageList.Free;
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and Assigned(activeLayer) then
  begin
    //deselect the active control
    MakeLayerActive(nil);
    Key := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.DeleteButtons;
begin
  //deletes layer buttons (ie button layers)
  layeredImage32.DeleteGroup(buttonGroupId);
  buttonGroupId := 0;
  activeLayer := nil;
end;
//------------------------------------------------------------------------------

procedure TForm1.MakeLayerActive(layer: TLayer32);
begin
  DeleteButtons;
  activeLayer := layer;
  if assigned(layer) then
    buttonGroupId := CreateSizingBtnsGroup(activeLayer,
      ssCorners, $FF3333FF, DefaultButtonSize, [boDropShadow]);
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  clickedLayer: TLayer32;
begin
  clickPoint := Types.Point(X,Y);
  //convert clickPoint from Panel coordinates to bitmap coordinates
  if not Panel1.ClientToBitmap(clickPoint) then Exit;

  //get the clicked layer (if any)
  clickedLayer := layeredImage32.GetLayerAt(clickPoint);

  //disable panel1 scrolling if we're about to move an image or a button
  panel1.BitmapProperties.ZoomAndScrollEnabled := not Assigned(clickedLayer);

  //if a control button was clicked then get ready to move it
  if Assigned(clickedLayer) and (clickedLayer is TButtonDesignerLayer32) then
  begin
    activeButtonLayer := TButtonDesignerLayer32(clickedLayer);
    //the activeLayer can't be a TButtonDesignerLayer32 so exit here
    Exit;
  end else
    activeButtonLayer := nil;

  //if anything but the activelayer was clicked then delete buttons
  if assigned(activeLayer) and  (activeLayer <> clickedLayer) then
    DeleteButtons;

  if Assigned(clickedLayer) then //assign a new activeLayer
  begin
    if not Assigned(activeLayer) then MakeLayerActive(clickedLayer);
    activeLayerMouseDown := true;
  end else
    PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy: integer;
  pt: TPoint;
  layer: TLayer32;
  rec: TRect;
begin
  pt := Types.Point(X,Y);
  if not Panel1.ClientToBitmap(pt) then Exit;
  dx := pt.X - clickPoint.X;
  dy := pt.Y - clickPoint.Y;

  if activeLayerMouseDown then              //moving image (and buttons)
  begin
    activeLayer.Offset(dx, dy);
    layeredImage32.OffsetGroup(buttonGroupId, dx, dy);
  end
  else if assigned(activeButtonLayer) then  //moving a button
  begin
    activeButtonLayer.Offset(dx, dy);
    rec := UpdateButtonSizingGroup(activeButtonLayer);
    SizeAndDrawLayer(activeLayer, RectWidth(rec), RectHeight(rec));
    activeLayer.PositionAt(rec.TopLeft);
  end else
  begin
    //not moving anything so just update the cursor
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
  activeLayerMouseDown := false;
  activeButtonLayer := nil;
  //re-enable Panel1 zoom and scroll
  Panel1.BitmapProperties.ZoomAndScrollEnabled := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    layeredImage32.GetMergedImage(true).SaveToFile(SaveDialog1.FileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuCopytoClipboardClick(Sender: TObject);
begin
  layeredImage32.GetMergedImage(true).CopyToClipBoard;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuExitClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuBringToFrontClick(Sender: TObject);
var
  oldIdx, newIdx: integer;
begin
  if not assigned(activeLayer) or (buttonGroupId <= 0) then Exit;
  oldIdx := activeLayer.Index;
  //don't bring the active layer in front of button control layers ...
  newIdx := layeredImage32.GetFirstInGroupIdx(buttonGroupId) -1;

  if (newIdx > oldIdx) and activeLayer.BringForward(newIdx) then
  begin
    masterImageList.Move(oldIdx, activeLayer.Index);
    PaintLayeredImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuSendToBackClick(Sender: TObject);
var
  oldIdx: integer;
begin
  oldIdx := activeLayer.Index;
  //send to bottom except for the hatched background (index = 0)
  if oldIdx = 1 then Exit;
  if activeLayer.SendBack(1) then
  begin
    masterImageList.Move(oldIdx, activeLayer.Index);
    PaintLayeredImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuDeleteLayerClick(Sender: TObject);
begin
  if not assigned(activeLayer) then Exit;
  masterImageList.Delete(activeLayer.Index);
  layeredImage32.DeleteLayer(activeLayer.Index);
  DeleteButtons;
  PaintLayeredImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  mnuBringToFront.Enabled := assigned(activeLayer) and
    (activeLayer.Index < masterImageList.Count -1);
  mnuBringtoFront2.Enabled := mnuBringtoFront.Enabled;
  mnuSendToBack.Enabled := assigned(activeLayer) and (activeLayer.Index > 1);
  mnuSendtoBack2.Enabled := mnuSendtoBack.Enabled;
  mnuDeleteLayer.Enabled := assigned(activeLayer);
  mnuDeleteLayer2.Enabled := mnuDeleteLayer.Enabled;

  if Sender = PopupMenu1 then
  begin
    GetCursorPos(popupPoint);
    popupPoint := Panel1.ScreenToClient(popupPoint);
    Panel1.ClientToBitmap(popupPoint);
  end else
    popupPoint := Point(layeredImage32.MidPoint);
end;
//------------------------------------------------------------------------------

end.
