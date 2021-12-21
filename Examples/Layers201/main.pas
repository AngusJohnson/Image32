unit main;

interface

uses
  Windows, Messages, SysUtils, Types, Classes, Graphics,
  Controls, Forms,  Dialogs, Math, ComCtrls, Menus,    Img32.Storage,
  Img32, Img32.Layers, Img32.Text, Img32.Panels, ExtCtrls;

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
    N3: TMenuItem;
    mnuCloneLayer: TMenuItem;
    N6: TMenuItem;
    CloneLayer1: TMenuItem;
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
    procedure mnuCloneLayerClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  public
    Image32Panel1      : TImage32Panel;
    layeredImage       : TLayeredImage32;
    words              : TStringList;
    clickedLayer       : TLayer32;
    targetLayer        : TRotatableLayer32;
    sizingButtonGroup  : TSizingGroupLayer32;
    rotatingButtonGroup: TRotatingGroupLayer32;
    arrowButtonGroup   : TGroupLayer32;
    clickPoint         : TPoint;
    UseAppOnIdle       : Boolean;
    delayedMovePending : Boolean;
    delayedShift       : TShiftState;
    delayedPos         : TPoint;
    doFullRepaint      : Boolean;
    procedure PrepareNormalRepaint;
    procedure DeleteAllControlButtons;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    procedure DelayedMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
  end;

var
  MainForm: TMainForm;

  fontReader: TFontReader; //TTF font reader
  fontCache: TFontCache;

type
  TMyRasterLayer32 = class(TRasterLayer32)
  public
    procedure Init(const filename: string; const centerPt: TPointD);
    function Clone: TMyRasterLayer32;
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
    constructor Create(Parent: TStorage; const name: string = ''); override;
    function Clone: TMyVectorLayer32;
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
{$R words.res}   //lost of English words
{$R Cursors.res} //rotation cursor

uses
  arrows, Img32.Draw, Img32.Extra, Img32.Vector, Img32.Fmt.BMP,
  Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Transform, Img32.Resamplers;

var
  hsl: THsl;

//------------------------------------------------------------------------------
// TMyRasterLayer32 - simplifies initialization of TRasterLayer32 objects
//------------------------------------------------------------------------------

procedure TMyRasterLayer32.Init(const filename: string; const centerPt: TPointD);
begin
  if not Image.LoadFromFile(filename) then
    Image.SetSize(100,100, clBlack32);
  PositionCenteredAt(centerPt);
  AutoPivot := false;//true;// :)
end;
//------------------------------------------------------------------------------

function TMyRasterLayer32.Clone: TMyRasterLayer32;
var
  b: TRectD;
begin
  result := RootOwner.AddLayer(
    TMyRasterLayer32, Parent, Name) as TMyRasterLayer32;
  //image quality will be preserved if we clone copy MasterImage
  result.MasterImage.Assign(MasterImage);
  result.Angle := Angle;
  b := InnerBounds;
  OffsetRect(b, 50,50);
  result.SetInnerBounds(b);
end;

//------------------------------------------------------------------------------
// TMyVectorLayer32 - defines drawing behaviour for TVectorLayer32 objects
//------------------------------------------------------------------------------

constructor TMyVectorLayer32.Create(Parent: TStorage; const name: string = '');
begin
  inherited;
  InitRandomColors;
  PenWidth := DPIAware(1);
  OuterMargin := DPIAware(5);
  AutoPivot := false; // :)
end;
//------------------------------------------------------------------------------

function TMyVectorLayer32.Clone: TMyVectorLayer32;
begin
  result := RootOwner.AddLayer(
    TMyVectorLayer32, Parent, Name) as TMyVectorLayer32;
  Result.Paths := CopyPaths(Paths);
  Result.PositionAt(Left+50, Top+ 50);
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.InitRandomColors;
begin
  hsl.hue := (hsl.hue + 23) mod 256;
  BrushColor := HslToRgb(hsl);
  PenColor := MakeDarker(BrushColor, 80) or $FF000000;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.Draw;
var
  pp: TPathsD;
begin
  pp := OffsetPath(Paths, -Left +OuterMargin, -Top +OuterMargin);
  DrawShadow(Image, pp, frEvenOdd, OuterMargin, angle45, clGray32, true);
  DrawPolygon(Image, pp, frEvenOdd, BrushColor);
  Draw3D(Image, pp, frEvenOdd, DPIAware(2.5), 2);
  DrawLine(Image, pp, PenWidth, PenColor, esPolygon);
  UpdateHitTestMaskFromImage;
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
  tmp := fontCache.GetTextOutline(0, 0, word);
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
  message.Result := 0; //speeds up drawing (ie don't redraw)
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  resStream: TResourceStream;
begin

  //This TImage32Panel component allows very easy zooming and scrolling.
  //(The following would also be a little simpler if TImage32Panel was
  //installed in Delphi's IDE - using Image32's designtime package.)
  Image32Panel1 := TImage32Panel.Create(self);
  Image32Panel1.Parent := self;
  Image32Panel1.Align := alClient;
  Image32Panel1.AllowScrnScroll := false;
  Image32Panel1.OnMouseDown := PanelMouseDown;
  Image32Panel1.OnMouseMove := PanelMouseMove;

  layeredImage := TLayeredImage32.Create;
  //layeredImage.Resampler := rNearestResampler; //draft quality (fast)
  layeredImage.Resampler := rBiLinearResampler;  //high quality (pretty fast)
  //layeredImage.Resampler := rBiCubicResampler; //best quality (slower)

  layeredImage.AddLayer(TDesignerLayer32); //for background hatching

  fontReader := FontManager.Load('Arial Bold');
  fontCache := TFontCache.Create(fontReader, DPIAware(48));
  words := TStringList.Create;
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
  hsl.hue := Random(256);
  hsl.sat := 240;
  hsl.lum := 200;
  hsl.Alpha := 212;

  //Use Application.OnIdle to process FormMouseMove events. This avoids
  //wasting a *lot* of CPU cycles drawing stuff that's immediately redrawn.
  //This will be especially noticeable when rotating layers.
  UseAppOnIdle := true;//false;//
  if UseAppOnIdle then
    Application.OnIdle := AppOnIdle;
  doFullRepaint := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  words.Free;
  //fontReader.Free; //will be done by FontManager if not here :)
  fontCache.Free;
  FreeAndNil(layeredImage); //see FormResize below
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  rec: TRect;
begin
  if not Assigned(layeredImage) then Exit; //ie when form closing
  rec := Image32Panel1.InnerClientRect;
  Image32Panel1.Image.SetSize(RectWidth(rec), RectHeight(rec));

  //resize layeredImage
  layeredImage.SetSize(RectWidth(rec), RectHeight(rec));
  //resize and repaint the designer background hatching
  with layeredImage[0] do
  begin
    SetSize(layeredImage.Width, layeredImage.Height);
    HatchBackground(Image);
  end;
  Invalidate; //forces a repaint even when shrinking form
end;
//------------------------------------------------------------------------------

procedure TMainForm.PrepareNormalRepaint;
begin
  doFullRepaint := false;
  Invalidate;
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
  //delete all 'designer' buttons
  FreeAndNil(sizingButtonGroup);
  FreeAndNil(rotatingButtonGroup);
  FreeAndNil(arrowButtonGroup);
end;
//------------------------------------------------------------------------------

procedure TMainForm.PanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickPoint := Image32Panel1.ClientToImage(Point(X,Y));
  clickedLayer := layeredImage.GetLayerAt(clickPoint, false);
  if not assigned(clickedLayer) then
  begin
    //probably clicking on the hatched background
    DeleteAllControlButtons;
    targetLayer := nil;
  end

  else if (clickedLayer = targetLayer) or
    (clickedLayer is TButtonDesignerLayer32) then Exit

  else if (clickedLayer is TRotatableLayer32) then
  begin
    //nb: TMyRasterLayer32 and TMyVectorLayer32 are both TRotatableLayer32
    //so this is clicking on a new target layer
    DeleteAllControlButtons;
    targetLayer := TRotatableLayer32(clickedLayer);

    if (clickedLayer is TMyArrowLayer32) then
      arrowButtonGroup := CreateButtonGroup(layeredImage.Root,
        TMyArrowLayer32(clickedLayer).Paths[0],
        bsRound, DefaultButtonSize, clGreen32)
    else
      sizingButtonGroup := CreateSizingButtonGroup(targetLayer,
        ssCorners, bsRound, DefaultButtonSize, clRed32);
  end;
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  pt := Image32Panel1.ClientToImage(Point(X,Y));
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
    layer := layeredImage.GetLayerAt(clickPoint, false);
    if assigned(layer) then
      Image32Panel1.cursor := layer.CursorId else
      Image32Panel1.cursor := crDefault;
    Exit;
  end;

  //however if nothing was clicked then exit
  if not Assigned(clickedLayer) then Exit;

  clickedLayer.Offset(dx, dy);

  //if moving a sizing button
  if (clickedLayer.Parent is TSizingGroupLayer32) then
  begin
    rec := UpdateSizingButtonGroup(clickedLayer);
    targetLayer.SetInnerBounds(RectD(rec));
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
      TRotatableLayer32(targetLayer).Angle := newAngle;
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
      if TRotatableLayer32(targetLayer).AutoPivot then
        rotatingButtonGroup.Offset(dx, dy);
    end
    else if Assigned(arrowButtonGroup) then
      arrowButtonGroup.Offset(dx, dy);
  end;
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRec: TRect;
  img: TImage32;
begin
  if doFullRepaint then
  begin
    //this will repaint the whole of layeredImage
    Image32Panel1.Image.Assign(layeredImage.GetMergedImage);
    doFullRepaint := false;
  end else
  begin
    //draw layeredImage onto the form's canvas. But to optimize performance,
    //only draw whatever's changed since the last draw (hence updateRec).
    img := layeredImage.GetMergedImage(false, updateRec);
    Image32Panel1.CopyToImage(img, updateRec);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddTextClick(Sender: TObject);
begin
  //in case the sizing, rotating buttons etc are visible
  DeleteAllControlButtons;
  clickedLayer := nil;

  //create a text layer
  targetLayer  := layeredImage.AddLayer(TMyTextLayer32) as TRotatableLayer32;
  with TMyTextLayer32(targetLayer) do
    Init(Words[Random(Words.Count)], layeredImage.MidPoint);
  //add sizing buttons
  sizingButtonGroup := CreateSizingButtonGroup(targetLayer, ssCorners,
    bsRound, DefaultButtonSize, clRed32);
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddImageClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  DeleteAllControlButtons;
  clickedLayer := nil;

  //create a raster image layer
  targetLayer := layeredImage.AddLayer(TMyRasterLayer32) as TRotatableLayer32;
  with TMyRasterLayer32(targetLayer) as TMyRasterLayer32 do
    Init(OpenDialog1.FileName, layeredImage.MidPoint);

  //add sizing buttons
  sizingButtonGroup := CreateSizingButtonGroup(targetLayer, ssCorners,
    bsRound, DefaultButtonSize, clRed32);
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddArrowClick(Sender: TObject);
begin
  DeleteAllControlButtons;
  clickedLayer := nil;

  //create an arrow layer
  targetLayer  := layeredImage.AddLayer(TMyArrowLayer32, nil, 'arrow') as TRotatableLayer32;
  with TMyArrowLayer32(targetLayer) do
  begin
    Init(layeredImage.MidPoint);
    arrowButtonGroup := CreateButtonGroup(layeredImage.Root,
      Paths[0], bsRound, DefaultButtonSize, clGreen32);
  end;
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuRotateClick(Sender: TObject);
var
  displayAngle: double;
  pivot: TPointD;
begin
  if not assigned(targetLayer) then Exit;
  clickedLayer := nil;

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

    with TRotatableLayer32(targetLayer) do
      if not AutoPivot then
        targetLayer.PivotPt := pivot;

    displayAngle := targetLayer.Angle;
    rotatingButtonGroup := CreateRotatingButtonGroup(
      targetLayer, pivot, DPIAware(10),
      clWhite32, clLime32, displayAngle, -Angle90);
    rotatingButtonGroup.AngleButton.CursorId := crRotate;
  end;
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuDeleteClick(Sender: TObject);
begin
  if not assigned(targetLayer) then Exit;

  FreeAndNil(targetLayer);
  DeleteAllControlButtons;
  clickedLayer := nil;
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuSendBackOneClick(Sender: TObject);
begin
  if assigned(targetLayer) and
    (targetLayer.Index > 1) then //ie don't send behind the hatched layer :).
      targetLayer.SendBackOne;
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuBringForwardOneClick(Sender: TObject);
begin
  if assigned(targetLayer) then targetLayer.BringForwardOne;
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuCloneLayerClick(Sender: TObject);
begin
  if not assigned(targetLayer) then Exit;
  if targetLayer is TMyRasterLayer32 then
    TMyRasterLayer32(targetLayer).Clone
  else if targetLayer is TMyVectorLayer32 then
    TMyVectorLayer32(targetLayer).Clone;
  PrepareNormalRepaint;
end;
//------------------------------------------------------------------------------

end.
