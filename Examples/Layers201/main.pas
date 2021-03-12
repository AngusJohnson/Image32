unit main;

interface

uses
  Windows, Messages, SysUtils, Types, Classes, Graphics,
  Controls, Forms,  Dialogs, Math, ComCtrls, Menus,
  Image32, Image32_Layers, Image32_Ttf;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
  private
    { Private declarations }
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
    layeredImage       : TLayeredImage32;
    words              : TStringList;
    clickedLayer       : TLayer32;
    targetLayer        : TLayer32;
    sizingButtonGroup  : TSizingGroupLayer32;
    rotatingButtonGroup: TRotatingGroupLayer32;
    arrowButtonGroup   : TGroupLayer32;
    clickPoint         : TPoint;
    disableOnIdle      : Boolean;
    delayedMovePending : Boolean;
    delayedShift       : TShiftState;
    delayedPos         : TPoint;
    procedure DeleteAllControlButtons;
    procedure AppOnIdle(Sender: TObject; var Done: Boolean);
    procedure DelayedMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
  end;

var
  MainForm: TMainForm;

  fontReader: TFontReader; //TTF font reader
  fontCache: TGlyphCache;
  defArrowBtns: TPathD;


const
  crRotate = 1;
  crMove   = 2;

implementation

{$R *.dfm}
{$R font.res}
{$R words.res}
{$R Cursors.res}

uses
  Image32_Draw, Image32_Extra, Image32_Vector,
  Image32_BMP, Image32_PNG, Image32_JPG, Image32_Transform;


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
  public
    constructor Create(groupOwner: TGroupLayer32;
      const name: string = ''); override;
    procedure Draw(Sender: TObject);
  end;

  TMyTextLayer32 = class(TMyVectorLayer32)
  public
    procedure Init(const word: string; const centerPt: TPointD);
  end;

  TMyArrowLayer32 = class(TMyVectorLayer32)
  public
    procedure Init(const centerPt: TPointD);
    procedure UpdateArrow(btnGroup: TGroupLayer32; btnIdx: integer);
  end;


//------------------------------------------------------------------------------
// TMyRasterLayer32 - simplifies initialization of TRasterLayer32 objects
//------------------------------------------------------------------------------

procedure TMyRasterLayer32.Init(const filename: string; const centerPt: TPointD);
begin
  if not MasterImage.LoadFromFile(filename) then
    MasterImage.SetSize(100,100, clBlack32);
  MasterImage.CropTransparentPixels;
  Image.Assign(MasterImage);
  PositionCenteredAt(centerPt);
  UpdateHitTestMaskTransparent;
end;

//------------------------------------------------------------------------------
// TMyVectorLayer32 - defines drawing behaviour for TVectorLayer32 objects
//------------------------------------------------------------------------------

constructor TMyVectorLayer32.Create(groupOwner: TGroupLayer32;
  const name: string = '');
begin
  inherited;
  InitRandomColors;
  PenWidth := DPIAware(1.5);
  OnDraw := Draw;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.InitRandomColors;
var
  hsl: THsl;
begin
  hsl.hue := Random(256);
  hsl.sat := 240;
  hsl.lum := 200;
  hsl.Alpha := 128;
  BrushColor := HslToRgb(hsl);
  PenColor := MakeDarker(BrushColor, 80) or $FF000000;
end;
//------------------------------------------------------------------------------

procedure TMyVectorLayer32.Draw(Sender: TObject);
var
  p: TPathsD;
begin
  p := OffsetPath(Paths, -Left, -Top);
  DrawPolygon(Image, p, frEvenOdd, BrushColor);
  DrawLine(Image, p, PenWidth, PenColor, esPolygon);
  UpdateHitTestMask(p, frEvenOdd);
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

  fontCache.GetTextGlyphs(0, 0, word, tmp);
  tmp := ScalePath(tmp, 1, 2.0);
  rec := Image32_Vector.GetBoundsD(tmp);
  with centerPt do
    tmp := OffsetPath(tmp,
      X - rec.Left - rec.Width/2,
      Y -rec.Top - rec.Height/2);
  Paths := tmp;
end;

//------------------------------------------------------------------------------
// TMyArrowLayer32 - demonstrates using custom designer buttons
//------------------------------------------------------------------------------

procedure TMyArrowLayer32.Init(const centerPt: TPointD);
var
  rec: TRectD;
  tmp: TPathsD;
begin
  SetLength(tmp, 1);
  tmp[0] := defArrowBtns;
  rec := Image32_Vector.GetBoundsD(tmp);
  with centerPt do
    tmp := OffsetPath(tmp,
      X - rec.Left - rec.Width/2,
      Y -rec.Top - rec.Height/2);
  Paths := tmp;
end;
//------------------------------------------------------------------------------

procedure TMyArrowLayer32.UpdateArrow(btnGroup: TGroupLayer32;
  btnIdx: integer);
var
  i: integer;
  center, pt2, vec: TPointD;
  dist: Double;
  p: TPathD;
  newPt: TPointD;
begin
  //preserve arrow symmetry and avoids 'broken' non-arrow polygons
  p := Copy(Paths[0], 0, 7);
  center := Image32_Vector.MidPoint(p[3], p[4]);
  newPt := btnGroup[btnIdx].MidPoint;
  case btnIdx of
    0:
      begin
        newPt := ClosestPointOnLine(newPt, p[0], center);
        if (CrossProduct(newPt, p[1], p[6]) > 0) and
          (CrossProduct(newPt, p[1], p[2]) > 0) then
          p[0] := newPt;
      end;
    1:
      begin
        dist := Distance(p[2], p[5]) * 0.5;
        pt2 := ClosestPointOnLine(newPt, p[0], center);
        if (DotProduct(newPt, center, p[0]) <= 0) or
          (DotProduct(newPt, p[0], center) <= 0) or
          (Distance(newPt, pt2) <= dist) then Exit;
        p[1] := newPt;
        p[6] := ReflectPoint(newPt, pt2);
        if (CrossProduct(newPt, p[2], p[0]) <= 0) or
          (CrossProduct(newPt, p[2], p[5]) > 0) or
          (CrossProduct(newPt, p[2], p[3]) >= 0) then
        begin
          vec := GetUnitVector(pt2, p[1]);
          p[2] := PointD(pt2.X + vec.X * dist, pt2.Y + vec.Y * dist);
          p[5] := PointD(pt2.X + vec.X * -dist, pt2.Y + vec.Y * -dist);
        end;
      end;
    2:
      begin
        if (CrossProduct(newPt, p[1], p[0]) >= 0) or
          (CrossProduct(p[1], newPt, p[3]) >= 0) or
          (CrossProduct(newPt, p[3], p[4]) <= 0) or
          (CrossProduct(p[0], newPt, center) <= 0) then Exit;
        p[2] := newPt;
        pt2 := ClosestPointOnLine(newPt, p[0], center);
        p[5] := ReflectPoint(newPt, pt2);
        if CrossProduct(p[1], newPt, p[6]) > 0 then
        begin
          dist := Distance(p[1], p[6]) * 0.5;
          vec := GetUnitVector(pt2, newPt);
          p[1] := PointD(pt2.X + vec.X * dist, pt2.Y + vec.Y * dist);
          p[6] := PointD(pt2.X + vec.X * -dist, pt2.Y + vec.Y * -dist);
        end;
      end;
    3:
      begin
        if (CrossProduct(newPt, p[0], center) > 0) or
          (CrossProduct(p[1], p[2], newPt) >= 0) then Exit;
        p[3] := newPt;
        center := ClosestPointOnLine(newPt, p[0], center);
        p[4] := ReflectPoint(newPt, center);
      end;
    4:
      begin
        if (CrossProduct(newPt, p[0], center) < 0) or
          (CrossProduct(p[6], p[5], newPt) <= 0) then Exit;
        p[4] := newPt;
        center := ClosestPointOnLine(newPt, p[0], center);
        p[3] := ReflectPoint(newPt, center);
      end;
    5:
      begin
        if (CrossProduct(newPt, p[6], p[0]) <= 0) or
          (CrossProduct(p[6], newPt, p[4]) <= 0) or
          (CrossProduct(newPt, p[4], p[3]) >= 0) or
          (CrossProduct(p[0], newPt, center) >= 0) then Exit;
        p[5] := newPt;
        pt2 := ClosestPointOnLine(newPt, p[0], center);
        p[2] := ReflectPoint(newPt, pt2);
        if CrossProduct(p[6], newPt, p[1]) < 0 then
        begin
          dist := Distance(p[6], p[1]) * 0.5;
          vec := GetUnitVector(pt2, newPt);
          p[6] := PointD(pt2.X + vec.X * dist, pt2.Y + vec.Y * dist);
          p[1] := PointD(pt2.X + vec.X * -dist, pt2.Y + vec.Y * -dist);
        end;
      end;
    6:
      begin
        dist := Distance(p[5], p[2]) * 0.5;
        pt2 := ClosestPointOnLine(newPt, p[0], center);
        if (DotProduct(newPt, center, p[0]) <= 0) or
          (DotProduct(newPt, p[0], center) <= 0) or
          (Distance(newPt, pt2) <= dist) then Exit;
        p[6] := newPt;
        p[1] := ReflectPoint(newPt, pt2);
        if (CrossProduct(newPt, p[5], p[0]) >= 0) or
          (CrossProduct(newPt, p[5], p[2]) < 0) or
          (CrossProduct(newPt, p[5], p[4]) <= 0) then
        begin
          vec := GetUnitVector(pt2, p[6]);
          p[5] := PointD(pt2.X + vec.X * dist, pt2.Y + vec.Y * dist);
          p[2] := PointD(pt2.X + vec.X * -dist, pt2.Y + vec.Y * -dist);
        end;
      end;
  end;

  for i := 0 to btnGroup.ChildCount -1 do
    btnGroup[i].PositionCenteredAt(p[i]);
  Paths := Image32_Vector.Paths(p);
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
  defArrowBtns :=
    MakePathI([0,100, 100,0, 100,50, 200,50, 200,150, 100,150, 100,200]);

  layeredImage := TLayeredImage32.Create();
  layeredImage.AddLayer(TDesignerLayer32);

  fontReader := TFontReader.Create;
  fontReader.LoadFromResource('FONT_NSB', RT_RCDATA);
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
  //wasting a *lot* of CPU cycles drawing stuff that will never be seen.
  //Set 'disableOnIdle' to true to see the difference (esp. rotating images)
  disableOnIdle := false;
  Application.OnIdle := AppOnIdle;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  words.Free;
  fontReader.Free;
  fontCache.Free;
  FreeAndNil(layeredImage); //see FormResize below
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  rec: TRect;
begin
  if not Assigned(layeredImage) then Exit; //ie when form closing

  // resize layeredImage
  rec := ClientRect;
  Dec(rec.Bottom, StatusBar1.Height);
  layeredImage.SetSize(RectWidth(rec), RectHeight(rec));
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

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  clickPoint := Point(X,Y);
  clickedLayer := layeredImage.GetLayerAt(clickPoint);

  if not assigned(clickedLayer) then
  begin
    DeleteAllControlButtons;
    targetLayer := nil;
  end

  else if (clickedLayer = targetLayer) or
    (clickedLayer is TButtonDesignerLayer32) then Exit

  else if (clickedLayer is TMyRasterLayer32) or
    (clickedLayer is TMyArrowLayer32) or
    (clickedLayer is TMyTextLayer32) then
  begin
    DeleteAllControlButtons;
    targetLayer := clickedLayer;

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

procedure TMainForm.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if disableOnIdle then
  begin
    DelayedMouseMove(Sender, Shift, X, Y);
  end else
  begin
    delayedShift := Shift;
    delayedPos := Point(X,Y);
    delayedMovePending := true;
  end;
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
      Cursor := layer.CursorId else
      Cursor := crDefault;
    Exit;
  end;

  //however if nothing was clicked then exit
  if not Assigned(clickedLayer) then Exit;

  clickedLayer.Offset(dx, dy);

  //if moving a sizing button
  if (clickedLayer.GroupOwner is TSizingGroupLayer32) then
  begin
    rec := UpdateSizingButtonGroup(clickedLayer);
    targetLayer.SetBounds(rec);
  end

  //if moving the rotate button
  else if (clickedLayer.GroupOwner = rotatingButtonGroup) then
  begin
    //Update rotatingButtonGroup and get the new angle
    newAngle := UpdateRotatingButtonGroup(clickedLayer);

    if targetLayer is TMyVectorLayer32 then
      TMyVectorLayer32(targetLayer).Angle := newAngle
    else if targetLayer is TMyRasterLayer32 then
      TMyRasterLayer32(targetLayer).Angle := newAngle;

    if newAngle > PI then newAngle := newAngle - Pi*2;
    StatusBar1.SimpleText := format('angle: %1.0n deg.',[newAngle * 180/pi]);
  end

  //if moving an arrow designer button
  else if (clickedLayer.GroupOwner = arrowButtonGroup) then
  begin
    with targetLayer as TMyArrowLayer32 do
      UpdateArrow(arrowButtonGroup, clickedLayer.Index)
  end

  //if moving a (non-button) 'target' layer
  else if (clickedLayer = targetLayer) then
  begin
    if Assigned(sizingButtonGroup) then
      sizingButtonGroup.Offset(dx, dy)
    else if Assigned(rotatingButtonGroup) then
      rotatingButtonGroup.Offset(dx, dy)
    else if Assigned(arrowButtonGroup) then
      arrowButtonGroup.Offset(dx, dy);
    StatusBar1.SimpleText := '';
  end;

  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(rotatingButtonGroup) then StatusBar1.SimpleText := '';
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormPaint(Sender: TObject);
var
  updateRec: TRect;
begin
  //draw layeredImage onto the form's canvas. But to optimize performance,
  //only draw whatever's changed since the last draw (hence updateRec).
  with layeredImage.GetMergedImage(false, updateRec) do
  begin
    CopyToDc(updateRec, self.Canvas.Handle,
      updateRec.Left, updateRec.Top, false);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuAddTextClick(Sender: TObject);
begin
  //in case the sizing, rotating buttons etc are visible
  DeleteAllControlButtons;

  //create a text layer
  targetLayer  := layeredImage.AddLayer(TMyTextLayer32);
  with TMyTextLayer32(targetLayer) do
    Init(Words[Random(Words.Count)], layeredImage.MidPoint);
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
  targetLayer := layeredImage.AddLayer(TMyRasterLayer32);
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
  targetLayer  := layeredImage.AddLayer(TMyArrowLayer32);
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
  pt: TPointD;
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
    pt := targetLayer.MidPoint;
    if targetLayer is TMyRasterLayer32 then
      displayAngle := TMyRasterLayer32(targetLayer).Angle
    else if targetLayer is TMyVectorLayer32 then
      with TMyVectorLayer32(targetLayer) do
    begin
      PivotPt := pt;
      displayAngle := Angle;
    end else
      displayAngle := 0;

    rotatingButtonGroup := CreateRotatingButtonGroup(
      targetLayer, pt, DPIAware(10),
      clWhite32, clLime32, displayAngle, Angle90);
    rotatingButtonGroup.CursorId := crRotate;

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

end.
