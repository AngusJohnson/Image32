unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Math,
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
    ToolIdx       : integer;
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
    procedure VectorPanelDesign(Sender: TObject);
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
  Img32.FMT.BMP, Img32.Fmt.SVG, Img32.Text,
  Img32.Clipper2, Img32.Vectorizer;

const
  HAND = 0; LINE = 1; IRREGPOLY = 2; REGPOLY = 3;
  TXT = 4; ERASE = 5; DROPPER = 6; FILL = 7; SELECT = 8; WAND = 9;

  toolIconNames: array [0..9] of string = ('HAND', 'LINE', 'IRREGPOLY',
    'REGPOLY', 'TEXT', 'ERASE', 'DROPPER', 'FILL', 'SELECT', 'WAND');

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
//  hotXs: array[0..9] of double = (0.71, 0.42, 0.89, 0.5, 0.86, 0.64, 0.89, 0.93);
//  hotYs: array[0..9] of double = (0.71, 0.37, 0.25, 0.5, 0.93, 0.70, 0.50, 0.93);
  hotXs: array[0..9] of double = (0.40, 0.89, 0.52, 0.5, 0.64, 0.89, 0.93, 0.86, 0.71, 0.42);
  hotYs: array[0..9] of double = (0.10, 0.25, 0.37, 0.5, 0.70, 0.50, 0.93, 0.93, 0.71, 0.37);
begin
  masterImage := TImage32.Create(defaultImageWidth, defaultImageHeight);
  masterImage.Clear(clNone32);

  undoRedo := TUndoRedo.Create;
  layeredImage := TLayeredImage32.Create;

  FontManager.LoadFontReaderFamily('Arial');

  buttonSize := dpiAware(48);
  pnlTools.ClientWidth := buttonSize * 2;
  ToolIdx := HAND;
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
      if i = ToolIdx then
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

function CompareExact(master, current: TColor32; tolerance: Integer): Boolean;
begin
  Result := master = current;
end;
//------------------------------------------------------------------------------

function GetNormalizedRect(const pt1, pt2: TPointD): TRectD;
begin
  if pt1.X <= pt2.X then
  begin
    Result.Left := pt1.X;
    Result.Right := pt2.X;
  end else
  begin
    Result.Left := pt2.X;
    Result.Right := pt1.X;
  end;
  if pt1.Y <= pt2.Y then
  begin
    Result.Top := pt1.Y;
    Result.Bottom := pt2.Y;
  end else
  begin
    Result.Top := pt2.Y;
    Result.Bottom := pt1.Y;
  end;
end;
//------------------------------------------------------------------------------

type
  TNormalizeType = (ntMin, ntAvg, ntMax);

function GetNormalizedSquare(const pt1, pt2: TPointD; nt: TNormalizeType): TRectD;
var
  q: double;
begin
  if pt1.X <= pt2.X then
  begin
    Result.Left := pt1.X;
    Result.Right := pt2.X;
  end else
  begin
    Result.Left := pt2.X;
    Result.Right := pt1.X;
  end;
  if pt1.Y <= pt2.Y then
  begin
    Result.Top := pt1.Y;
    Result.Bottom := pt2.Y;
  end else
  begin
    Result.Top := pt2.Y;
    Result.Bottom := pt1.Y;
  end;
  case nt of
    ntMin: q := Min(Result.Width, Result.Height);
    ntAvg: q := Average(Result.Width, Result.Height);
    else {ntMax:} q := Max(Result.Width, Result.Height);
  end;
  Result.Right := Result.Left + q;
  Result.Bottom := Result.Top + q;
end;
//------------------------------------------------------------------------------

procedure TfmMain.imgPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt                  : TPointD;
  tmp                 : TImage32;
  p                   : TPathD;
  pp                  : TPathsD;
  currentDrawingLayer : TVectorLayer32;
  currentDesignerLayer: TVectorLayer32;
  //tbo                 : TTextBoxOptions;
  text                : string;
  color               : TColor32;
  fontReader          : TFontReader;
  font                : TFontCache;
  rec                 : TRectD;
begin
  //STARTS **OR CONTINUES** AN EXISTING LAYER

  if (Button <> mbLeft) and not (ToolIdx in [FILL, DROPPER]) then
  begin
    EndLayer;
    Exit;
  end;

  currentDrawingLayer   := nil;
  currentDesignerLayer  := nil;

  pt := PointD(imgPanel.ClientToImage(Point(X,Y)));
  // ignore mouse clicks outside the image
  if (pt.X < 0) or (pt.X >= masterImage.Width) or
    (pt.Y < 0) or (pt.Y >= masterImage.Height) then
      Exit;

  // todo - of course make LINEWIDTH user defined
  if ToolIdx in [SELECT, WAND] then lineWidth := DpiAware(1)
  else if ToolIdx = TXT then lineWidth := 0
  else lineWidth := DpiAware(10);

  if not layerStarted then layeredImage.Clear;

  if (ToolIdx = FILL) then
    // do nothing here
  else if (ToolIdx = DROPPER) then
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
    if not (ToolIdx in [SELECT, WAND]) then
    begin
      currentDrawingLayer :=
        TVectorLayer32(layeredImage.AddLayer(TVectorLayer32));
      with currentDrawingLayer do
      begin
        OuterMargin := lineWidth;
        OnDraw := VectorPanelDraw;
      end;
    end;

    // and always create a design layer as the top layer
    currentDesignerLayer :=
      TVectorLayer32(layeredImage.AddLayer(TVectorLayer32));
    with currentDesignerLayer do
    begin
      OuterMargin := lineWidth;
      OnDraw := VectorPanelDesign;
      IsDesignerLayer := true;
    end;

  end else
  begin
    currentDesignerLayer := TVectorLayer32(layeredImage[TopLayerIdx]);
    if ToolIdx in [SELECT, WAND] then
      currentDrawingLayer := nil else
      currentDrawingLayer := TVectorLayer32(layeredImage[TopLayerIdx-1]);
  end;

  case ToolIdx of

    SELECT:
      begin
        currentDesignerLayer.Paths := nil;
        currentDesignerLayer.AppendPoint(pt);
        mouseDown := true;
        layerStarted := true;
      end;

    WAND:
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
          color := tmp.Pixel[Round(pt.X), Round(pt.Y)];
          ReplaceColor(tmp, color, $01101010, 32);
          pp := img32.Vectorizer.Vectorize(tmp, $01101010, CompareExact, 0);

//          //inflate paths - todo: use tool properties to set inflate amount
//          pp := InflatePaths(pp, -5, jsRound, etPolygon);

          currentDesignerLayer.Paths := pp;
        finally
          tmp.Free;
        end;
        mouseDown := false;
        layerStarted := true;
      end;

    REGPOLY:
      begin
        // currentDesignerLayer path only contains top-left bottom-right
        pp := currentDesignerLayer.Paths;
        if not Assigned(pp) then SetLength(pp, 1);

        if Length(pp[0]) = 0 then
        begin
          AppendPoint(pp[0], pt);
          currentDesignerLayer.Paths := pp;
        end else
        begin
          if Length(pp[0]) = 1 then AppendPoint(pp[0], pt)
          else pp[0][1] := pt;
          currentDesignerLayer.Paths := pp;

          rec := GetNormalizedSquare(pp[0][0], pp[0][1], ntAvg);
          pp[0] := Ellipse(rec, 5);
          currentDrawingLayer.Paths := pp;
        end;
        mouseDown := true;
        layerStarted := true;
      end;

    LINE, IRREGPOLY:
      begin
        // currentDesignerLayer path only contains start and end points
        pp := currentDesignerLayer.Paths;
        if not Assigned(pp) then SetLength(pp, 1);
        if Length(pp[0]) > 1 then SetLength(pp[0], 1);
        AppendPoint(pp[0], pt);
        currentDesignerLayer.Paths := pp;
        currentDrawingLayer.AppendPoint(pt);
        mouseDown := true;
        layerStarted := true;
      end;

    ERASE:
      begin
        pp := currentDesignerLayer.Paths;
        if Assigned(pp) then
        begin
          SetLength(p, 2);
          p[0] := pp[0][0];
          p[1] := pt;
          with layeredImage[0] do
          begin
            p := TranslatePath(p, -Left +OuterMargin, -Top  +OuterMargin);
            EraseLine(Image, p, lineWidth, esRound, jsRound);
          end;
          pp[0][0] := pt;
          currentDesignerLayer.Paths := pp;
        end else
          currentDesignerLayer.AppendPoint(pt);
        mouseDown := true;
        layerStarted := true;
      end;

    FILL: // flood fill
      begin
        if (Button = mbLeft) then
          FloodFill(masterImage, Round(pt.X), Round(pt.Y), foreColor, 48) else
          FloodFill(masterImage, Round(pt.X), Round(pt.Y), backColor, 48);
        imgPanel.Image.Assign(masterImage);
        undoRedo.Add(masterImage);
        HatchBackground(imgPanel.Image);
        mouseDown := false;
        layerStarted := false;
        Exit;
      end;

    TXT:
    begin
      mouseDown := false;
      layerStarted := false;
      text := '';
      if not TextInputBox(self, 'Enter Text:', '', 'Drawing Demo', text) then
        Exit;

      fontReader := FontManager.GetBestMatchFont([]);
      font := TFontCache.Create(fontReader, DpiAware(14));
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

procedure TfmMain.imgPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pt                  : TPointD;
  p                   : TPathD;
  pp                  : TPathsD;
  rec                 : TRectD;
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
  with currentDesignerLayer do
  begin
    case ToolIdx of

      SELECT:
        begin
          pp := Paths;
          SetLength(pp[0], 4);
          pp[0][1] := PointD(pt.X, pp[0][0].Y);
          pp[0][2] := PointD(pt.X, pt.Y);
          pp[0][3] := PointD(pp[0][0].X, pt.Y);
          Paths := pp;
        end;

      REGPOLY:
        begin
          begin
            pp := Paths;
            if not Assigned(pp) then SetLength(pp, 1)
            else if Length(pp[0]) = 1 then AppendPoint(pt)
            else pp[0][1] := pt;
            Paths := pp;
          end;

          if Length(pp[0]) > 1 then
          begin
            rec := GetNormalizedSquare(pp[0][0], pp[0][1], ntAvg);
            SetLength(pp, 1);
            pp[0] := Ellipse(rec, 5);
            TVectorLayer32(layeredImage[TopLayerIdx-1]).Paths := pp;
          end;
        end;

      ERASE:
        begin
          pp := Paths;
          if Assigned(pp) then
          begin
            SetLength(p, 2);
            p[0] := pp[0][0];
            p[1] := pt;
            with layeredImage[0] do
            begin
              p := TranslatePath(p, -Left +OuterMargin, -Top  +OuterMargin);
              EraseLine(Image, p, lineWidth, esRound, jsRound);
            end;
            pp[0][0] := pt;
            Paths := pp;
          end else
            AppendPoint(pt);
        end;

      else
      begin
        pp := Paths;
        SetLength(pp[0], 2);
        pp[0][1] := pt;
        Paths := pp;

        currentDrawingLayer := TVectorLayer32(layeredImage[TopLayerIdx-1]);
        currentDrawingLayer.AppendPoint(pt);
        Paths := SimplifyPaths(Paths, 1.0, false);
      end;
    end;
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
end;
//------------------------------------------------------------------------------

procedure TfmMain.VectorPanelDraw(Sender: TObject);
var
  len                   : integer;
  q, buttonSize         : double;
  backgroundImg         : TImage32;
  pp                    : TPathsD;
  p                     : TPathD;
  rec                   : TRectD;
  currentDesignerLayer  : TVectorLayer32;
  currentDrawingLayer   : TVectorLayer32;
  dashPattern           : TArrayOfDouble;
begin
  buttonSize := Max(DpiAware(5), lineWidth);
  currentDrawingLayer := TVectorLayer32(layeredImage[TopLayerIdx -1]);
  with currentDrawingLayer do
  begin
    case ToolIdx of
      LINE:
        begin
          Image.Clear;
          DrawLine(Image, PathsPositionAdjusted,
            lineWidth, foreColor, esRound, jsRound);
        end;
      REGPOLY:
        begin
          p := PathsPositionAdjusted[0];
          if Assigned(p) then
            DrawLine(Image, p, lineWidth, foreColor, esClosed, jsRound);
        end;
      IRREGPOLY:
        begin
          Image.Clear;
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
  end;
end;
//------------------------------------------------------------------------------

procedure TfmMain.VectorPanelDesign(Sender: TObject);
var
  len                   : integer;
  q, buttonSize         : double;
  backgroundImg         : TImage32;
  pp                    : TPathsD;
  p                     : TPathD;
  rec                   : TRectD;
  currentDesignerLayer  : TVectorLayer32;
  currentDrawingLayer   : TVectorLayer32;
  dashPattern           : TArrayOfDouble;
begin
  buttonSize := Max(DpiAware(5), lineWidth);
  currentDesignerLayer := TVectorLayer32(layeredImage[TopLayerIdx]);
  with currentDesignerLayer do
  begin
    case ToolIdx of
      SELECT, WAND:
        begin
          // draw on currentDesignerLayer
          pp := PathsPositionAdjusted;
          backgroundImg := TImage32.Create(masterImage);
          try
            backgroundImg.Crop(Rect(OuterBounds));
            SetLength(dashPattern, 2);
            dashPattern[0] := DpiAware(5); dashPattern[1] := DpiAware(5);
            DrawInvertedDashedLine(Image, backgroundImg,
              PathsPositionAdjusted, dashPattern, nil, lineWidth, esPolygon);
          finally
            backgroundImg.Free;
          end;
        end;
      LINE, REGPOLY, IRREGPOLY, ERASE:
        begin
          pp := PathsPositionAdjusted;
          if (Length(pp[0]) > 1) and (ToolIdx <> ERASE) then
          begin
            DrawButton(Image, pp[0][0], buttonSize, clRed32, bsRound, [ba3D]);
            DrawButton(Image, pp[0][1], buttonSize, clLime32, bsRound, [ba3D])
          end else
            DrawButton(Image, pp[0][0], buttonSize, clLime32, bsRound, [ba3D]);
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
  if not (ToolIdx in [SELECT, WAND]) then
    undoRedo.Add(masterImage);
end;
//------------------------------------------------------------------------------

procedure TfmMain.toolPnlClick(Sender: TObject);
begin
  if TComponent(Sender).ComponentIndex = ToolIdx then Exit;
  with TImage32Panel(pnlTools.Components[ToolIdx]) do
  begin
    UnFocusedColor := clBtnFace;
    invalidate;
  end;
  with TImage32Panel(Sender) do
  begin
    ToolIdx := ComponentIndex;
    UnFocusedColor := FocusedColor;
  end;
  imgPanel.SetFocus;
  imgPanel.Cursor := ToolIdx +1;

  case ToolIdx of
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
      AssignPixelArray(undoRedo.Undo, Width, Height, true);
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
  if not layerStarted or not (ToolIdx in [SELECT, WAND]) then Exit;
  pp := TVectorLayer32(layeredImage[1]).Paths;
  layerStarted := false;
  layeredImage.Clear;
  if not Assigned(pp) then Exit;
  ErasePolygon(masterImage, pp, frEvenOdd);
  imgPanel.Image.Assign(masterImage);
  HatchBackground(imgPanel.Image);
  undoRedo.Add(masterImage);
end;
//------------------------------------------------------------------------------

end.
