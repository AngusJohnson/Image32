unit main;

interface

uses
  Windows, Messages, SysUtils, Types, Classes, Graphics,
  Controls, Forms,  Dialogs, Math, ComCtrls, Menus,
  Img32, Img32.Layers, Img32.Text, Img32.Panels;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    File1: TMenuItem;
    mnuExit: TMenuItem;
    Action1: TMenuItem;
    mnuAddImage: TMenuItem;
    mnuAddText: TMenuItem;
    N1: TMenuItem;
    mnuRotate: TMenuItem;
    mnuDelete: TMenuItem;
    N2: TMenuItem;
    mnuSendBackOne: TMenuItem;
    mnuBringForwardOne: TMenuItem;
    mnuAddArrow: TMenuItem;
    mnuCopytoclipboard: TMenuItem;
    N3: TMenuItem;
    PopupMenu1: TPopupMenu;
    AddImage1: TMenuItem;
    AddText1: TMenuItem;
    AddArrow1: TMenuItem;
    N4: TMenuItem;
    Rotate1: TMenuItem;
    Delete1: TMenuItem;
    N5: TMenuItem;
    SendBackOne1: TMenuItem;
    BringForwardOne1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuAddTextClick(Sender: TObject);
    procedure mnuAddImageClick(Sender: TObject);
    procedure mnuRotateClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuSendBackOneClick(Sender: TObject);
    procedure mnuBringForwardOneClick(Sender: TObject);
    procedure mnuAddArrowClick(Sender: TObject);
    procedure mnuCopytoclipboardClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  public
    layeredImage       : TLayeredImage32;
    words              : TStringList;
    clickedLayer       : TLayer32;
    targetLayer        : TRotateLayer32;
    sizingButtonGroup  : TSizingGroupLayer32;
    rotatingButtonGroup: TRotatingGroupLayer32;
    arrowButtonGroup   : TGroupLayer32;
    clickPoint         : TPoint;
    UseAppOnIdle       : Boolean;
    delayedMovePending : Boolean;
    delayedShift       : TShiftState;
    delayedPos         : TPoint;
    imgPanel           : TImage32Panel;
    procedure DeleteAllControlButtons;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    procedure DelayedMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
  end;

var
  MainForm: TMainForm;

  fontReader: TFontReader; //TTF font reader
  fontCache: TGlyphCache;

type

  TMyRasterLayer32 = class(TRasterLayer32)
  public
    procedure Init(const filename: string; const centerPt: TPointD);
  end;

  TMyVectorLayer32 = class(TVectorLayer32)
  private
    BrushColor: TColor32;
    PenColor  : TColor32;
    PenWidth  : double;
    procedure InitRandomColors;
  protected
    procedure Draw; override;
  public
    constructor Create(Parent: TLayer32; const name: string = ''); override;
  end;

  TMyTextLayer32 = class(TMyVectorLayer32)
  public
    procedure Init(const word: string; const centerPt: TPointD);
  end;

const
  crRotate = 1;
  crMove   = 2;

implementation

{$R *.dfm}
{$R font.res}
{$R words.res}
{$R Cursors.res}

uses
  arrows, Img32.Draw, Img32.Extra, Img32.Vector, Img32.Fmt.BMP,
  Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Transform, Img32.Resamplers;

//------------------------------------------------------------------------------
// TMyRasterLayer32 - simplifies initialization of TRasterLayer32 objects
//------------------------------------------------------------------------------

procedure TMyRasterLayer32.Init(const filename: string; const centerPt: TPointD);
begin
  if not Image.LoadFromFile(filename) then
    Image.SetSize(100,100, clBlack32);
  MasterImage.CropTransparentPixels; //nb: crop MasterImage not Image
  PositionCenteredAt(centerPt);
  UpdateHitTestMaskTransparent; //hit-test to non-transparent pixels
  AutoPivot := false; // :)
end;

//------------------------------------------------------------------------------
// TMyVectorLayer32 - defines drawing behaviour for TVectorLayer32 objects
//------------------------------------------------------------------------------

constructor TMyVectorLayer32.Create(Parent: TLayer32; const name: string = '');
begin
  inherited;
  InitRandomColors;
  PenWidth := DPIAware(1);
  Margin := 20;
  AutoPivot := false; // :)
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.InitRandomColors;
var
  hsl: THsl;
begin
  hsl.hue := Random(256);
  hsl.sat := 240;
  hsl.lum := 200;
  hsl.Alpha := 172;
  BrushColor := HslToRgb(hsl);
  PenColor := MakeDarker(BrushColor, 80) or $FF000000;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.Draw;
var
  pp: TPathsD;
begin
  pp := OffsetPath(Paths, -Left, -Top);
  DrawShadow(Image, pp, frEvenOdd, DPIAware(4), angle45, clGray32, true);
  DrawPolygon(Image, pp, frEvenOdd, BrushColor);
  Draw3D(Image, pp, frEvenOdd, DPIAware(2.5), 2);
  DrawLine(Image, pp, PenWidth, PenColor, esPolygon);
  UpdateHitTestMask(pp, frEvenOdd);
end;

//------------------------------------------------------------------------------
// TMyTextLayer - simplifies initializing TVectorLayer32 objects with text paths
//------------------------------------------------------------------------------

procedure TMyTextLayer32.Init(const word: string; const centerPt: TPointD);
var
  rec: TRectD;
  tmp: TPathsD;
begin
  Name := word;
  tmp := fontCache.GetTextGlyphs(0, 0, word);
  tmp := ScalePath(tmp, 1, 2.0);
  rec := Img32.Vector.GetBoundsD(tmp);
  with centerPt do
    tmp := OffsetPath(tmp,
      X - rec.Left - rec.Width/2,
      Y -rec.Top - rec.Height/2);
  Paths := tmp;
end;

//------------------------------------------------------------------------------
// TMainForm
//------------------------------------------------------------------------------

procedure TMainForm.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 0; //speeds up drawing
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  resStream: TResourceStream;
begin

  //ADD A TIMAGE32PANEL COMPONENT TO THE FORM
  //nb: this can be done more easily when
  //TImage32Panel is installed in the IDE.
  imgPanel := TImage32Panel.Create(self);
  imgPanel.Parent := self;
  imgPanel.Align := alClient;
  imgPanel.OnMouseDown := PanelMouseDown;
  imgPanel.OnMouseMove := PanelMouseMove;
  //disable zoom and scroll since we will be
  //using the mouse to move layer 'buttons' around
  imgPanel.AllowScroll := false;
  imgPanel.AllowZoom := false;

  layeredImage := TLayeredImage32.Create;
  layeredImage.AddLayer(TDesignerLayer32);

  //layeredImage.Resampler := rNearestResampler;   //draft quality (fast)
  layeredImage.Resampler := rBiLinearResampler;  //high quality (moderately fast)
  //layeredImage.Resampler := rBiCubicResampler;   //excellent quality (slow)

  fontReader := FontManager.LoadFromResource('FONT_NSB', RT_RCDATA);
  fontCache := TGlyphCache.Create(fontReader, DPIAware(48));

  words := TStringList.Create;
  words.Capacity := $FFFF;
  resStream := TResourceStream.Create(HInstance, 'WORDS', RT_RCDATA);
  try
    words.LoadFromStream(resStream);
  finally
    resStream.Free;
  end;

  Screen.Cursors[crRotate] :=
    LoadImage(hInstance, 'ROTATE', IMAGE_CURSOR, 32,32, LR_DEFAULTSIZE);
  Screen.Cursors[crMove] :=
    LoadImage(hInstance, 'MOVE', IMAGE_CURSOR, 32,32, LR_DEFAULTSIZE);

  Randomize;

  //Using Application.OnIdle to process FormMouseMove code avoids
  //wasting a *lot* of CPU cycles drawing stuff that's immediately redrawn.
  //This will be especially noticeable when rotating images that have been
  //resized, or resizing images that have been rotated (and hence require
  //an additional affine transformation).
  UseAppOnIdle := true;//false;//
  if UseAppOnIdle then
    Application.OnIdle := AppOnIdle;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  words.Free;
  //fontReader can be Free'd here but will otherwise be done by FontManager :)
  //fontReader.Free;
  fontCache.Free;
  FreeAndNil(layeredImage); //see FormResize below
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  rec: TRect;
begin
  if not Assigned(layeredImage) then Exit; //ie when form closing

  //resize layeredImage
  rec := imgPanel.InnerClientRect;
  layeredImage.SetSize(RectWidth(rec), RectHeight(rec));

  //resize and repaint the designer background hatching
  with layeredImage[0] do
  begin
    SetSize(layeredImage.Width, layeredImage.Height);
    HatchBackground(Image);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.AppOnIdle(Sender: TObject; var Done: Boolean);
begin
  Done := true;
  if not delayedMovePending then Exit;
  delayedMovePending := false;
  DelayedMouseMove(Sender, delayedShift, delayedPos.X, delayedPos.Y);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuExitClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DeleteAllControlButtons;
begin
  FreeAndNil(sizingButtonGroup);
  FreeAndNil(rotatingButtonGroup);
  FreeAndNil(arrowButtonGroup);
end;
//------------------------------------------------------------------------------

procedure TMainForm.PanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //because the panel image's dimensions rarely corresponds
  //to the panel's clientrect, the X,Y coordinate needs adjusting
  clickPoint := imgPanel.ClientToImage(Point(X,Y));

  clickedLayer := layeredImage.GetLayerAt(clickPoint);
  if not assigned(clickedLayer) then
  begin
    DeleteAllControlButtons;
    targetLayer := nil;
  end

  else if (clickedLayer = targetLayer) or
    (clickedLayer is TButtonDesignerLayer32) then Exit

  else if (clickedLayer is TRotateLayer32) then
  begin
    DeleteAllControlButtons;
    targetLayer := TRotateLayer32(clickedLayer);

    if (clickedLayer is TMyArrowLayer32) then
      arrowButtonGroup := CreateButtonGroup(layeredImage.Root,
        TMyArrowLayer32(clickedLayer).Paths[0],
        bsRound, DefaultButtonSize, clGreen32)
    else
      sizingButtonGroup := CreateSizingButtonGroup(targetLayer,
        ssCorners, bsRound, DefaultButtonSize, clRed32);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  //because the panel image's dimensions rarely corresponds
  //to the panel's clientrect, the X,Y coordinate needs adjusting
  pt := imgPanel.ClientToImage(Point(X,Y));
  if UseAppOnIdle then
  begin
    delayedShift := Shift;
    delayedPos := pt;
    delayedMovePending := true;
  end else
    DelayedMouseMove(Sender, Shift, pt.X, pt.Y);
end;
//------------------------------------------------------------------------------

procedure TMainForm.DelayedMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  layer: TLayer32;
  rec: TRect;
  dx, dy: integer;
  newAngle: double;
begin
  dx := X - clickPoint.X;
  dy := Y - clickPoint.Y;
  clickPoint := Point(X, Y);

  if not (ssLeft in Shift) then
  begin
    //just moving the unclicked mouse
    //so update the cursor and exit
    layer := layeredImage.GetLayerAt(clickPoint);
    if assigned(layer) then
      imgPanel.Cursor := layer.CursorId else
      imgPanel.Cursor := crDefault;
    Exit;
  end;

  //however if nothing was clicked then exit
  if not Assigned(clickedLayer) then Exit;

  clickedLayer.Offset(dx, dy);

  //if moving a sizing button
  if (clickedLayer.Parent is TSizingGroupLayer32) then
  begin
    rec := UpdateSizingButtonGroup(clickedLayer);
    targetLayer.SetBounds(rec);
  end

  //if moving a rotate button
  else if (clickedLayer.Parent = rotatingButtonGroup) then
  begin
    if clickedLayer = rotatingButtonGroup.PivotButton then
    begin
      clickedLayer.Offset(-dx, -dy);     //undo button move above
      rotatingButtonGroup.Offset(dx,dy); //move the whole rotate group
      targetLayer.PivotPt := clickedLayer.MidPoint;
    end else
    begin
      //Update rotatingButtonGroup and get the new angle
      newAngle := UpdateRotatingButtonGroup(clickedLayer);
      TRotateLayer32(targetLayer).Angle := newAngle;
      if newAngle > PI then newAngle := newAngle - Pi*2;
      StatusBar1.SimpleText := format('angle: %1.0n deg.',[newAngle * 180/pi]);
    end;
  end

  //if moving an arrow designer button
  else if (clickedLayer.Parent = arrowButtonGroup) then
  begin
    with targetLayer as TMyArrowLayer32 do
      UpdateArrow(arrowButtonGroup, clickedLayer.Index)
  end

  //if moving targetlayer (ie not a button layer)
  else if (clickedLayer = targetLayer) then
  begin
    if Assigned(sizingButtonGroup) then
      sizingButtonGroup.Offset(dx, dy)
    else if Assigned(rotatingButtonGroup) then
    begin
      if TRotateLayer32(targetLayer).AutoPivot then
        rotatingButtonGroup.Offset(dx, dy);
    end
    else if Assigned(arrowButtonGroup) then
      arrowButtonGroup.Offset(dx, dy);
    StatusBar1.SimpleText := '';
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRec: TRect;
  img: TImage32;
begin
  //draw layeredImage onto the form's canvas. But to optimize performance,
  //only draw whatever's changed since the last draw (hence updateRec).
  img := layeredImage.GetMergedImage(false, updateRec);
  if (imgPanel.Image.Width <> img.Width) or
    (imgPanel.Image.Height <> img.Height) then
    imgPanel.Image.Assign(img) else
    imgPanel.Image.Copy(img, updateRec, updateRec);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddTextClick(Sender: TObject);
begin
  //in case the sizing, rotating buttons etc are visible
  DeleteAllControlButtons;

  //create a text layer
  targetLayer  := layeredImage.AddLayer(TMyTextLayer32) as TRotateLayer32;
  with TMyTextLayer32(targetLayer) do
    //Init(Words[Random(Words.Count)], layeredImage.MidPoint);
    Init(Words[Random(Words.Count)], PointD(220,220));
  //add sizing buttons
  sizingButtonGroup := CreateSizingButtonGroup(targetLayer, ssCorners,
    bsRound, DefaultButtonSize, clRed32);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddImageClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;

  DeleteAllControlButtons;

  //create a raster image layer
  targetLayer := layeredImage.AddLayer(TMyRasterLayer32) as TRotateLayer32;
  with TMyRasterLayer32(targetLayer) as TMyRasterLayer32 do
    Init(OpenDialog1.FileName, layeredImage.MidPoint);

  //add sizing buttons
  sizingButtonGroup := CreateSizingButtonGroup(targetLayer, ssCorners,
    bsRound, DefaultButtonSize, clRed32);
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddArrowClick(Sender: TObject);
begin
  DeleteAllControlButtons;

  //create an arrow layer
  targetLayer  := layeredImage.AddLayer(TMyArrowLayer32, nil, 'arrow') as TRotateLayer32;
  with TMyArrowLayer32(targetLayer) do
  begin
    Init(layeredImage.MidPoint);
    arrowButtonGroup := CreateButtonGroup(layeredImage.Root,
      Paths[0], bsRound, DefaultButtonSize, clGreen32);
  end;

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuRotateClick(Sender: TObject);
var
  displayAngle: double;
  pivot: TPointD;
begin
  if not assigned(targetLayer) then Exit;

  if assigned(rotatingButtonGroup) then
  begin
    //toggle off rotating buttons and toggle on sizing or control buttons
    DeleteAllControlButtons;
    if (targetLayer is TMyArrowLayer32) then
      arrowButtonGroup := CreateButtonGroup(layeredImage.Root,
        TMyArrowLayer32(targetLayer).Paths[0],
        bsRound, DefaultButtonSize, clGreen32)
    else
      sizingButtonGroup := CreateSizingButtonGroup(targetLayer,
        ssCorners, bsRound, DefaultButtonSize, clRed32);
  end else
  begin
    //toggle on the rotating button using the previous rotation angle
    DeleteAllControlButtons;

    pivot := targetLayer.MidPoint;

    with TRotateLayer32(targetLayer) do
      if not AutoPivot then
        targetLayer.PivotPt := pivot;

    displayAngle := targetLayer.Angle;
    rotatingButtonGroup := CreateRotatingButtonGroup(
      targetLayer, pivot, DPIAware(10),
      clWhite32, clLime32, displayAngle, -Angle90);
    rotatingButtonGroup.AngleButton.CursorId := crRotate;

    if displayAngle > PI then displayAngle := displayAngle - Pi*2;
    StatusBar1.SimpleText := format('angle: %1.0n deg.',[displayAngle * 180/pi]);
  end;

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuDeleteClick(Sender: TObject);
begin
  if not assigned(targetLayer) then Exit;

  FreeAndNil(targetLayer);
  DeleteAllControlButtons;
  clickedLayer := nil;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuSendBackOneClick(Sender: TObject);
begin
  if assigned(targetLayer) and
    (targetLayer.Index > 1) then //ie don't send behind the hatched layer :).
      targetLayer.SendBackOne;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuBringForwardOneClick(Sender: TObject);
begin
  if assigned(targetLayer) then targetLayer.BringForwardOne;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuCopytoclipboardClick(Sender: TObject);
begin
  with layeredImage.GetMergedImage(true) do
    CopyToClipBoard;
end;
//------------------------------------------------------------------------------

end.
