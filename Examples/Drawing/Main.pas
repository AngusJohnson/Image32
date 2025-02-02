unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, ComCtrls, Menus,
  Img32, Img32.Layers, Img32.Panels;


const
  undoCount = 20;

type

  TUndoRedo = class
  private
    current: integer;
    undos: array[0..undoCount -1] of TArrayOfColor32;
  public
    constructor Create;
    procedure Add(img: TImage32);
    procedure Clear;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function Undo: TArrayOfColor32;
    function Redo: TArrayOfColor32;
  end;

  TfmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    File2: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    SaveAs1: TMenuItem;
    SaveAs2: TMenuItem;
    Exit1: TMenuItem;
    Exit2: TMenuItem;
    Help1: TMenuItem;
    New1: TMenuItem;
    pnlTools: TPanel;
    pnlMain: TPanel;
    TabControl1: TTabControl;
    mnuUndo: TMenuItem;
    StatusBar1: TStatusBar;
    mnuRedo: TMenuItem;
    N3: TMenuItem;
    mnuDelSelect: TMenuItem;
    N4: TMenuItem;
    mnuEditForeColor: TMenuItem;
    mnuEditBackColor: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure imgPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure mnuDelSelectClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure mnuEditBackColorClick(Sender: TObject);
  private
    undoRedo      : TUndoRedo;
    lineWidth     : double;
    mouseDown     : Boolean;
    layerStarted  : Boolean;
    focusedToolIdx: integer;
    masterImage   : TImage32;
    layeredImage  : TLayeredImage32;
    foreColor     : TColor32;
    backColor     : TColor32;
    forePanel     : TImage32Panel;
    backPanel     : TImage32Panel;
    imgPanel      : TImage32Panel;
    procedure SetColor(isForeColor: Boolean; color: TColor32);
    procedure EndLayer;
    procedure toolPnlClick(Sender: TObject);
    procedure VectorPanelDraw(Sender: TObject);
    function TopLayerIdx: integer;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}
{$R ToolIcons.res}

uses
  Img32.Draw, Img32.Extra, Img32.Vector, Color32Dialog, DialogsEx,
  Img32.FMT.BMP, Img32.Fmt.SVG, Img32.FMT.PNG, Img32.Text,
  Img32.Clipper2, Img32.Vectorizer;

const
  toolIconNames: array [0..7] of string = (
    'SELECT', 'WAND', 'LINE', 'SHAPE', 'FILL', 'TEXT', 'DELETE', 'DROPPER' );
  defaultImageWidth = 800;
  defaultImageHeight = 600;

resourcestring
  rsLinePolyDrawingTip = 'Left click to add vertices. Right click to end layer';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function GetCursorSize: TSize;
begin
  Result.cx := GetSystemMetrics(SM_CXCURSOR);
  Result.cy := GetSystemMetrics(SM_CYCURSOR);
end;
//------------------------------------------------------------------------------

function Create32BitCursorFromImage(img: TImage32; hotX, hotY: integer): HCURSOR;
var
  bih         : TBitmapV4Header;
  tmpImg      : TImage32;
  dc          : HDC;
  hColorBM    : HBITMAP;
  hMonoBM     : HBITMAP;
  bmpPxls     : Pointer;
  curInfo     : TIconInfo;
  cursorSize  : TSize;
begin
  hColorBM  := 0;
  hMonoBM   := 0;
  cursorSize := GetCursorSize;

  tmpImg := TImage32.Create(img);
  try
    tmpImg.Resize(cursorSize.cx, cursorSize.cy);
    FillChar(bih, sizeof(bih), #0);
    bih.bV4Size           := sizeof(TBitmapV4Header);
    bih.bV4Width          := tmpImg.Width;
    bih.bV4Height         := -tmpImg.Height; // else flip image :0
    bih.bV4Planes         := 1;
    bih.bV4BitCount       := 32;
    bih.bV4ClrUsed        := 0;
    bih.bV4SizeImage      := tmpImg.Width * tmpImg.Height;
    bih.bV4V4Compression  := BI_BITFIELDS;
    bih.bV4RedMask        := $FF shl 16;
    bih.bV4GreenMask      := $FF shl 8;
    bih.bV4BlueMask       := $FF;
    bih.bV4AlphaMask      := Cardinal($FF) shl 24;

    dc := GetDC(0);
    try
      hColorBM := CreateDIBSection(dc, PBitmapInfo(@bih)^, DIB_RGB_COLORS, bmpPxls, 0, 0);
      Move(tmpImg.PixelBase^, bmpPxls^,
        tmpImg.Width * tmpImg.Height * SizeOf(TColor32));
    finally
      ReleaseDC(dc, 0);
    end;

    hMonoBM := CreateBitmap(tmpImg.Width, tmpImg.Height, 1, 1, nil);
    curInfo.fIcon := False;
    curInfo.xHotspot := hotX;
    curInfo.yHotspot := hotY;
    curInfo.hbmMask := hMonoBM;
    curInfo.hbmColor := hColorBM;
    Result := CreateIconIndirect(curInfo);
  finally
    DeleteObject(hColorBM);
    DeleteObject(hMonoBM);
    tmpImg.free;
  end;
end;

//------------------------------------------------------------------------------
// TUndoRedo class
//------------------------------------------------------------------------------

constructor TUndoRedo.Create;
begin
  current := -1;
end;
//------------------------------------------------------------------------------

procedure TUndoRedo.Add(img: TImage32);
var
  len: integer;
begin
  current := (current +1) mod undoCount;
  // do a strict copy because it's not safe to reference count 'copy'
  len := Length(img.Pixels);
  SetLength(undos[current], len);
  Move(img.Pixels[0], undos[current][0], len * SizeOf(TColor32));
end;
//------------------------------------------------------------------------------

procedure TUndoRedo.Clear;
var
  i: integer;
begin
  for i := 0 to undoCount -1 do undos[i] := nil;
  current := -1;
end;
//------------------------------------------------------------------------------

function TUndoRedo.CanUndo: Boolean;
begin
  if current < 0 then
    Result := false
  else if (current = 0) then
    Result := Assigned(undos[UndoCount -1])
  else
    Result := Assigned(undos[current -1]);
end;
//------------------------------------------------------------------------------

function TUndoRedo.Undo: TArrayOfColor32;
begin
  if not CanUndo then Raise Exception.Create('TUndoDraw - nothing to undo!');
  Dec(current);
  if current < 0 then current := undoCount -1;
  Result := undos[current];
end;
//------------------------------------------------------------------------------

function TUndoRedo.CanRedo: Boolean;
begin
  if current < 0 then
    Result := false
  else if (current +1 = UndoCount) then
    Result := Assigned(undos[0])
  else
    Result := Assigned(undos[current +1]);
end;
//------------------------------------------------------------------------------

function TUndoRedo.Redo: TArrayOfColor32;
begin
  if not CanRedo then Raise Exception.Create('TUndoDraw - nothing to redo!');
  current := (current + 1) mod undoCount;
  Result := undos[current];
end;

//------------------------------------------------------------------------------
// TfmMain class
//------------------------------------------------------------------------------

procedure TfmMain.FormCreate(Sender: TObject);
var
  i, hotX, hotY: integer;
  cursorSize  : TSize;
  tmpImg: TImage32;
  buttonSize: integer;
const
  // offsets to the red dot hotspots on cursors (relative to cursor size)
  hotXs: array[0..7] of double = (0.71, 0.42, 0.89, 0.5, 0.86, 0.64, 0.89, 0.93);
  hotYs: array[0..7] of double = (0.71, 0.37, 0.25, 0.5, 0.93, 0.70, 0.50, 0.93);
begin
  masterImage := TImage32.Create(defaultImageWidth, defaultImageHeight);
  masterImage.Clear(clNone32);

  undoRedo := TUndoRedo.Create;
  layeredImage := TLayeredImage32.Create;

  FontManager.LoadFontReaderFamily('Arial');

  buttonSize := dpiAware(48);
  pnlTools.ClientWidth := buttonSize * 2;
  focusedToolIdx := 0;
  ActiveControl := imgPanel;
  cursorSize := GetCursorSize;
  DoubleBuffered := true;
  foreColor := clGreen32;
  backColor := $40338833;


  imgPanel := TImage32Panel.Create(self);
  with imgPanel do
  begin
    Parent := pnlMain;
    TabOrder := 1;
    Align := alClient;
    BorderWidth := 12;
    OnMouseDown := imgPanelMouseDown;
    OnMouseMove := imgPanelMouseMove;
    OnMouseUp := imgPanelMouseUp;
    TabStop := True;
  end;
  TabControl1.Align := alTop;

  // create tool button panels ...
  for i := 0 to High(toolIconNames) do
    with TImage32Panel.Create(pnlTools) do
    begin
      Parent := pnlTools;
      if Odd(i) then
        Left := buttonSize else
        Left := 0;
      Top := (i div 2) * buttonSize;
      Width := buttonSize;
      Height := buttonSize;
      BorderWidth := 4;
      Color := clBtnFace;
      Image.SetSize(DpiAware(32), DpiAware(32));
      Image.LoadFromResource(toolIconNames[i], 'SVG');
      TabStop := false;
      if i = focusedToolIdx then
        UnFocusedColor := FocusedColor;
      OnClick := toolPnlClick;
    end;

  forePanel := TImage32Panel.Create(pnlTools);
  with forePanel do
  begin
    Parent := pnlTools;
    Left := 0;
    Top := Length(toolIconNames) div 2 * buttonSize + 20;
    Width := buttonSize;
    Height := buttonSize;
    TabStop := false;
    BorderWidth := 4;
    Image.Clear(foreColor);
    HatchBackground(Image);
    OnClick := mnuEditBackColorClick;
  end;

  backPanel := TImage32Panel.Create(pnlTools);
  with backPanel do
  begin
    Parent := pnlTools;
    Left := buttonSize;
    Top := forePanel.Top;
    Width := buttonSize;
    Height := buttonSize;
    TabStop := false;
    BorderWidth := 4;
    Color := clBtnFace;
    Image.Clear(backColor);
    HatchBackground(Image);
    OnClick := mnuEditBackColorClick;
  end;

  // create and install 32bit custom cursors ...
  tmpImg := TImage32.Create(cursorSize.cx, cursorSize.cy);
  try
    for i := 0 to High(toolIconNames) do
    begin
      tmpImg.LoadFromResource(toolIconNames[i]+'2', 'SVG');
      hotX := Round(cursorSize.cx * hotXs[i]);
      hotY := Round(cursorSize.cy * hotYs[i]);
      Screen.Cursors[i +1] := Create32BitCursorFromImage(tmpImg, hotX, hotY);
    end;
  finally
    tmpImg.Free;
  end;

  undoRedo.Add(masterImage);
  imgPanel.Image.Assign(masterImage);
  HatchBackground(imgPanel.Image);
  imgPanel.Cursor := 1;
  ActiveControl := imgPanel;
end;
//------------------------------------------------------------------------------

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  undoRedo.Free;
  layeredImage.Free;
  masterImage.Free;
end;
//------------------------------------------------------------------------------

procedure TfmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TfmMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
{$IFDEF DEBUG}
  if Key = #27 then Close;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function TfmMain.TopLayerIdx: integer;
begin
  Result := layeredImage.Count -1;
end;
//------------------------------------------------------------------------------

procedure TfmMain.imgPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i                   : integer;
  pt                  : TPointD;
  tmp                 : TImage32;
  pp                  : TPathsD;
  currentDrawingLayer : TVectorLayer32;
  currentDesignerLayer: TVectorLayer32;
  //tbo                 : TTextBoxOptions;
  text                : string;
  fontReader          : TFontReader;
  font                : TFontCache;
begin
  //STARTS **OR CONTINUES** AN EXISTING LAYER

  if (Button <> mbLeft) and not (focusedToolIdx in [4,7]) then
  begin
    EndLayer;
    Exit;
  end;

  //if (focusedToolIdx > 4) then Exit;

  currentDrawingLayer   := nil;
  currentDesignerLayer  := nil;

  pt := PointD(imgPanel.ClientToImage(Point(X,Y)));
  // ignore mouse clicks outside the image
  if (pt.X < 0) or (pt.X >= masterImage.Width) or
    (pt.Y < 0) or (pt.Y >= masterImage.Height) then
      Exit;

  // todo - of course make LINEWIDTH user defined
  if focusedToolIdx < 2 then
    lineWidth := 2 else
    lineWidth := 20;

  if not layerStarted then layeredImage.Clear;

  if (focusedToolIdx = 4) then
    // do nothing here
  else if (focusedToolIdx = 7) then
  begin
    with Point(pt) do
      SetColor((button = mbLeft), masterImage.pixel[X,Y]);
    Exit;
  end
  else if (TopLayerIdx < 0) then
  begin
    // must be starting a new drawing op - so create new layers
    layeredImage.SetSize(MasterImage.Width, MasterImage.Height);

    // copy masterImage into layeredImage ready to merge with other layers
    with TRasterLayer32(layeredImage.AddLayer(TRasterLayer32)).Image do
      Assign(Self.MasterImage);

    // create a 'drawing' layer for all tools except selection tools
    if focusedToolIdx > 1 then
    begin
      currentDrawingLayer :=
        TVectorLayer32(layeredImage.AddLayer(TVectorLayer32));
      with currentDrawingLayer do
      begin
        OuterMargin := dpiAware(lineWidth);
        OnDraw := VectorPanelDraw;
      end;
    end;

    // and always create a design layer as the top layer
    currentDesignerLayer :=
      TVectorLayer32(layeredImage.AddLayer(TVectorLayer32));
    with currentDesignerLayer do
    begin
      OuterMargin := dpiAware(lineWidth);
      OnDraw := VectorPanelDraw;
      IsDesignerLayer := true;
    end;

  end else
  begin
    currentDesignerLayer := TVectorLayer32(layeredImage[TopLayerIdx]);
    if focusedToolIdx < 2 then
      currentDrawingLayer := nil else
      currentDrawingLayer := TVectorLayer32(layeredImage[TopLayerIdx-1]);
  end;

  case focusedToolIdx of
    0: // rectangular selection
      begin
        currentDesignerLayer.Paths := nil;
        currentDesignerLayer.AppendPoint(pt);
        mouseDown := true;
        layerStarted := true;
      end;

    1: // flood fill selection
      begin
        tmp := TImage32.Create;
        try

//            // doing contiguous selection
//            tmp.Assign(layeredImage.GetMergedImage(true));
//            FloodFill(tmp, pt.X, pt.Y, clNone32, 64);
//            TVectorLayer32(layeredImage[currentLayer]).Paths :=
//              img32.Vectorizer.Vectorize(tmp, clNone32, CompareAlpha, 64);

          // doing non-contiguous selection
          tmp.Assign(masterImage);
          RemoveColor(tmp, tmp.Pixel[Round(pt.X), Round(pt.Y)]);
          pp := img32.Vectorizer.Vectorize(tmp, clNone32, CompareAlpha, 64);
          // ignore tiny blended color matches
          for i := High(pp) downto 0 do
            if (Abs(Area(pp[i])) < 10) then pp[i] := nil;

//          //inflate paths - todo: use tool properties to set inflate amount
//          pp := InflatePaths(pp, -5, jsRound, etPolygon);

          currentDesignerLayer.Paths := pp;
        finally
          tmp.Free;
        end;
        mouseDown := false;
        layerStarted := false;
      end;

    2, 3, 6: // add a point to line or polygon or eraser
      begin
        // currentDesignerLayer path to contain only start +/- end points
        pp := currentDesignerLayer.Paths;
        if not Assigned(pp) then SetLength(pp, 1);
        if Length(pp[0]) > 1 then SetLength(pp[0], 1);
        AppendPoint(pp[0], pt);
        currentDesignerLayer.Paths := pp;
        currentDrawingLayer.AppendPoint(pt);
        mouseDown := true;
        layerStarted := true;
      end;

    4: // flood fill
      begin
        if (Button = mbLeft) then
          FloodFill(masterImage, Round(pt.X), Round(pt.Y), foreColor, 48) else
          FloodFill(masterImage, Round(pt.X), Round(pt.Y), backColor, 48);
        imgPanel.Image.Assign(masterImage);
        HatchBackground(imgPanel.Image);
        mouseDown := false;
        layerStarted := false;
        Exit;
      end;

    5:
    begin
      mouseDown := false;
      layerStarted := false;
      text := '';
      if not TextInputBox(self, 'Enter Text:', '', 'Drawing Demo', text) then
        Exit;

      fontReader := FontManager.GetBestMatchFont([]);
      font := TFontCache.Create(fontReader, DpiAware(16));
      try
        with currentDrawingLayer do
        begin
          Paths := font.GetTextOutline(pt.X, pt.Y, text);
          pp := PathsPositionAdjusted;
          DrawPolygon(Image, pp, frNonZero, foreColor);
        end;
      finally
        font.Free;
      end;

      layerStarted := true;
      EndLayer;
      Exit;
    end;

    else Exit;
  end; // end case

  StatusBar1.Panels[0].Text := Format(' %1.0n, %1.0n',[pt.X, pt.Y]);
  imgPanel.Image.Assign(layeredImage.GetMergedImage());
  HatchBackground(imgPanel.Image);
end;
//------------------------------------------------------------------------------

procedure TfmMain.imgPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  pp: TPathsD;
  pt: TPointD;
  currentDesignerLayer: TVectorLayer32;
  currentDrawingLayer: TVectorLayer32;
begin

  pt := PointD(imgPanel.ClientToImage(Point(X,Y)));
  // update cursor position in statusbar
  if (pt.X < 0) or (pt.X >= masterImage.Width) or
    (pt.Y < 0) or (pt.Y >= masterImage.Height) then
      StatusBar1.Panels[0].Text := '' else
      StatusBar1.Panels[0].Text := Format(' %1.0n, %1.0n',[pt.X, pt.Y]);

  if not mouseDown then Exit;

  currentDesignerLayer := TVectorLayer32(layeredImage[TopLayerIdx]);
  if focusedToolIdx = 0 then
  begin
    pp := currentDesignerLayer.Paths;
    SetLength(pp[0], 4);
    pp[0][1] := PointD(pt.X, pp[0][0].Y);
    pp[0][2] := PointD(pt.X, pt.Y);
    pp[0][3] := PointD(pp[0][0].X, pt.Y);
    currentDesignerLayer.Paths := pp;
  end else
  begin
    pp := currentDesignerLayer.Paths;
    SetLength(pp[0], 2);
    pp[0][1] := pt;
    currentDesignerLayer.Paths := pp;

    currentDrawingLayer := TVectorLayer32(layeredImage[TopLayerIdx-1]);
    pp := currentDrawingLayer.Paths;
    AppendPoint(pp[0], pt);
    pp := SimplifyPaths(pp, 1.0, false);
    currentDrawingLayer.Paths := pp;
  end;
  imgPanel.Image.Assign(layeredImage.GetMergedImage());
  HatchBackground(imgPanel.Image);
end;
//------------------------------------------------------------------------------

procedure TfmMain.imgPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not mouseDown then Exit;
  mouseDown := False;
  if (focusedToolIdx = 1) then layerStarted := false;
end;
//------------------------------------------------------------------------------

procedure TfmMain.VectorPanelDraw(Sender: TObject);
var
  len, buttonSize       : integer;
  backgroundImg         : TImage32;
  pp                    : TPathsD;
  p                     : TPathD;
  currentDesignerLayer  : TVectorLayer32;
  currentDrawingLayer   : TVectorLayer32;
begin
  currentDesignerLayer := TVectorLayer32(layeredImage[TopLayerIdx]);
  buttonSize := DPIAware(7);
  case focusedToolIdx of
    0, 1: // drawing a selection or wand select
      with currentDesignerLayer do
      begin
        backgroundImg := TImage32.Create(masterImage);
        try
          backgroundImg.Crop(Rect(OuterBounds));
          DrawInvertedDashedLine(Image, backgroundImg,
            PathsPositionAdjusted, [10,10], nil, lineWidth, esPolygon);
        finally
          backgroundImg.Free;
        end;
      end;
    2: // drawing a simple poly-line
      begin
        with currentDesignerLayer do
        begin
          pp := PathsPositionAdjusted;
          if Length(pp[0]) > 1 then
            DrawButton(Image, pp[0][1], buttonSize, clLime32, bsRound, [ba3D]) else
            DrawButton(Image, pp[0][0], buttonSize, clLime32, bsRound, [ba3D]);
        end;
        with TVectorLayer32(layeredImage[TopLayerIdx -1]) do
          DrawLine(Image, PathsPositionAdjusted, lineWidth, foreColor, esRound, jsRound);
      end;
    3: // drawing a polygon
      begin
        currentDrawingLayer :=
          TVectorLayer32(layeredImage[TopLayerIdx -1]);
        with currentDesignerLayer do
        begin
          pp := PathsPositionAdjusted;
          p := pp[0];

//          if Assigned(currentDrawingLayer.Paths) and
//            (Length(currentDrawingLayer.Paths[0]) > 2) then
//              DrawDashedLine(Image, pp, [10,10], nil, 1, clWhite32, esRound);

          if Length(p) > 1 then
          begin
            DrawButton(Image, p[0], buttonSize, clRed32, bsRound, [ba3D]);
            DrawButton(Image, p[1], buttonSize, clLime32, bsRound, [ba3D]);
          end else
            DrawButton(Image, p[0], buttonSize, clLime32, bsRound, [ba3D]);
        end;

        with currentDrawingLayer do
        begin
          pp := PathsPositionAdjusted;
          if not Assigned(pp) then Exit;
          len := Length(pp[0]);
          if len > 2 then
          begin
            DrawPolygon(Image, pp, frNonZero, backColor);
            DrawLine(Image, pp, lineWidth, foreColor, esPolygon, jsRound);
          end else
            DrawLine(Image, pp, lineWidth, foreColor, esRound, jsRound);
        end;
      end;
    6: // erase (using a poly-line)
      begin
        // draw a design button at the cursor pos
        with currentDesignerLayer do
        begin
          pp := PathsPositionAdjusted;
          if Length(pp[0]) > 1 then
            DrawButton(Image, pp[0][1], buttonSize, clLime32, bsRound, [ba3D]) else
            DrawButton(Image, pp[0][0], buttonSize, clLime32, bsRound, [ba3D]);
        end;
        // use the drawing layer path to erase from the copied masterimage layer
        // get the unlocalised erase paths ...
        pp := TVectorLayer32(layeredImage[TopLayerIdx -1]).Paths;
        with layeredImage[0] do
        begin
          // localise pp to the copied masterImage layer and erase
          pp := TranslatePath(pp, -Left + OuterMargin, -Top+ OuterMargin);
          EraseLine(Image, pp, lineWidth, esRound, jsRound);
        end;

      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TfmMain.EndLayer;
begin
  if not layerStarted then Exit;
  layerStarted := false;
  masterImage.Assign(layeredImage.GetMergedImage(true));

  imgPanel.Image.Assign(masterImage);
  HatchBackground(imgPanel.Image);
  layeredImage.Clear;
  undoRedo.Add(masterImage);
  // save to undo list
end;
//------------------------------------------------------------------------------

procedure TfmMain.toolPnlClick(Sender: TObject);
begin
  if TComponent(Sender).ComponentIndex = focusedToolIdx then Exit;
  with TImage32Panel(pnlTools.Components[focusedToolIdx]) do
  begin
    UnFocusedColor := clBtnFace;
    invalidate;
  end;
  with TImage32Panel(Sender) do
  begin
    focusedToolIdx := ComponentIndex;
    UnFocusedColor := FocusedColor;
  end;
  imgPanel.SetFocus;
  imgPanel.Cursor := focusedToolIdx +1;

  case focusedToolIdx of
    2,3: StatusBar1.Panels[1].Text := rsLinePolyDrawingTip;
    else StatusBar1.Panels[1].Text := '';
  end;
  EndLayer;
end;
//------------------------------------------------------------------------------

procedure TfmMain.SetColor(isForeColor: Boolean; color: TColor32);
begin
  if isForeColor then
  begin
    foreColor := Color;
    forePanel.Image.Clear(foreColor);
    if GetAlpha(foreColor) < 200 then
      HatchBackground(forePanel.Image);
  end else
  begin
    backColor := Color;
    backPanel.Image.Clear(backColor);
    if GetAlpha(backColor) < 200 then
      HatchBackground(backPanel.Image);
  end;
end;
//------------------------------------------------------------------------------

procedure TfmMain.mnuEditBackColorClick(Sender: TObject);
var
  isForeColor: Boolean;
begin
  isForeColor := (Sender = forePanel) or (Sender = mnuEditForeColor);
  with TColor32DialogForm.Create(nil) do
  try
    if isForeColor then
      Color := foreColor else
      Color := backColor;
    if ShowModal = mrOk then
      SetColor(isForeColor, Color);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TfmMain.mnuUndoClick(Sender: TObject);
begin
  if not layerStarted then
  begin
    if not undoRedo.CanUndo then Exit;
    with masterImage do
      AssignPixelArray(undoRedo.Undo, Width, Height);
  end else
  begin
    layeredImage.Clear;
    layerStarted := false;
  end;
  imgPanel.Image.Assign(masterImage);
  HatchBackground(imgPanel.Image);
end;
//------------------------------------------------------------------------------

procedure TfmMain.mnuRedoClick(Sender: TObject);
begin
  if layerStarted or not undoRedo.CanRedo then Exit;
  with masterImage do
    AssignPixelArray(undoRedo.Redo, Width, Height);
  imgPanel.Image.Assign(masterImage);
  HatchBackground(imgPanel.Image);
end;
//------------------------------------------------------------------------------

procedure TfmMain.mnuDelSelectClick(Sender: TObject);
var
  pp: TPathsD;
begin
  if focusedToolIdx > 1 then Exit;
  pp := TVectorLayer32(layeredImage[1]).Paths;
  if not Assigned(pp) then Exit;
  ErasePolygon(masterImage, pp, frEvenOdd);
  layeredImage.Clear;
  layerStarted := false;
  undoRedo.Add(masterImage);
  imgPanel.Image.Assign(masterImage);
  HatchBackground(imgPanel.Image);
end;
//------------------------------------------------------------------------------

end.
