unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  Types, Menus, ExtCtrls, ComCtrls, StdCtrls,
  Image32, Image32_Layers, Image32_Ttf, Image32_SmoothPath;

type

  TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    mnuHideControls: TMenuItem;
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    View1: TMenuItem;
    N2: TMenuItem;
    CopytoClipboard1: TMenuItem;
    PopupMenu1: TPopupMenu;
    N4: TMenuItem;
    mnuDeleteLast: TMenuItem;
    mnuDeletePath: TMenuItem;
    pnlTop: TPanel;
    Label2: TLabel;
    edWidth: TEdit;
    UpDown1: TUpDown;
    mnuRotateButtons: TMenuItem;
    N5: TMenuItem;
    mnuSmoothSym: TMenuItem;
    mnuSmoothAsym: TMenuItem;
    mnuSharpWithHdls: TMenuItem;
    mnuSharpNoHdls: TMenuItem;
    lblPenColor: TLabel;
    lblFillColor: TLabel;
    edPenColor: TEdit;
    edFillColor: TEdit;
    Shape1: TShape;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlMainMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlMainMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure pnlMainDblClick(Sender: TObject);
    procedure mnuHideControlsClick(Sender: TObject);
    procedure edPenColorChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure mnuDeleteLastClick(Sender: TObject);
    procedure mnuDeletePathClick(Sender: TObject);
    procedure mnuSharpWithHdlsClick(Sender: TObject);
    procedure mnuRotateButtonsClick(Sender: TObject);
  private
    layeredImage32   : TLayeredImage32;
    smoothGroupLayer : TSmoothPathGroupLayer32;
    clickedLayer     : TLayer32;
    clickPoint       : TPoint;

    fontReader       : TFontReader;
    glyphCache       : TGlyphCache;
    procedure UpdateSmoothPathLayerAttributes;
    procedure BeforeMerge(Sender: TObject);
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}
{$R Font.res}
{$R RotateCursor.res}

uses
  Image32_BMP, Image32_PNG, Image32_JPG, Image32_Draw, Image32_Vector,
  Image32_Extra, Image32_Clipper;

const
  crRotate = 1;
  margin = 100;
var
  lineWidth: integer;


//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function GetFillColor: TColor32;
begin
  result := StrToInt64Def(FrmMain.edFillColor.Text, clNone32);
end;
//------------------------------------------------------------------------------

function GetPenColor: TColor32;
begin
  result := StrToInt64Def(FrmMain.edPenColor.Text, clBlack32);
end;

//------------------------------------------------------------------------------
// TFrmMain
//------------------------------------------------------------------------------

procedure TFrmMain.FormCreate(Sender: TObject);
var
  rec: TRect;
begin
  //load a font for drawing text onto TImage32 objects
  fontReader := TFontReader.Create;
  fontReader.LoadFromResource('FONT_NSR', RT_RCDATA);
  glyphCache := TGlyphCache.Create(fontReader, DpiAware(11));

  //SETUP LAYEREDIMAGE32
  layeredImage32 := TLayeredImage32.Create(RectWidth(rec), RectHeight(rec));
  layeredImage32.BackgroundColor := Color32(clBtnFace);
  
  //the first layer will be a hatched TDesignerLayer32 which is 'non-clickable'
  layeredImage32.AddLayer(TDesignerLayer32);

  Screen.Cursors[crRotate] :=
    LoadImage(hInstance, 'ROTATE', IMAGE_CURSOR, 32,32,0);

  UpdateSmoothPathLayerAttributes;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  fontReader.Free;
  glyphCache.Free;
  layeredImage32.Free;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 1;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.FormResize(Sender: TObject);
begin
  if not visible then Exit;

  layeredImage32.SetSize(ClientWidth, ClientHeight);
  with layeredImage32 do
    layeredImage32[0].SetSize(Width, Height);
  HatchBackground(layeredImage32[0].Image);
end;
//------------------------------------------------------------------------------

function RgbColor(clr: TColor32): TColor;
var
  c: TARGB absolute clr;
  r: TARGB absolute Result;
begin
  r.A := 0; r.R := c.B; r.G := c.G; r.B := c.R;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.UpdateSmoothPathLayerAttributes;
var
  pc, bc: TColor32;
begin
  pc := TColor32(StrToInt64Def(edPenColor.Text, $990000));
  bc := TColor32(StrToInt64Def(edFillColor.Text, $FFAAAA));
  lineWidth := Max(2, Min(25, strtoint(edWidth.text)));

  FrmMain.Shape1.Pen.Width := lineWidth div 2;
  FrmMain.Shape1.Pen.Color := RgbColor(pc);
  FrmMain.Shape1.Brush.Color := RgbColor(bc);

  if not Assigned(smoothGroupLayer) then Exit;
  with smoothGroupLayer do
  begin
    PenColor := pc;
    BrushColor := bc;
    PenWidth := lineWidth;
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.BeforeMerge(Sender: TObject);
var
  i: integer;
  path: TPathD;
  outline: TPathsD;
begin
  with TSmoothPathGroupLayer32(Sender), DesignLayer do
  begin
    //draw coordinates on smoothGroupLayer's designer layer
    for i := 0 to SmoothPath.Count -1 do
      DrawText(Image,
        SmoothPath[i].X - Left, //offset the coords relative to the layer
        SmoothPath[i].Y - Top,
        Format('  %1.0n,%1.0n', [SmoothPath[i].X,SmoothPath[i].Y]),
        glyphCache, clMaroon32);

    //and draw a dashed outline of the smoothpath too
    path := SmoothPath.FlattenedPath;
    outline := InflatePath(path, lineWidth + DPIAware(8), jsRound, esRound);
    outline := OffsetPath(outline, -Left, -Top);
    DrawDashedLine(Image, outline, dashes, nil,
      DPIAware(1), clMaroon32, esPolygon);
  end;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.FormPaint(Sender: TObject);
var
  updateRec: TRect;
begin
  with layeredImage32.GetMergedImage(mnuHideControls.Checked, updateRec) do
    CopyToDc(updateRec, Canvas.Handle, updateRec.Left, updateRec.Top, false);
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainDblClick(Sender: TObject);
begin
  //double click adds a button control layer
  if not Assigned(smoothGroupLayer) then
  begin
    smoothGroupLayer :=
      TSmoothPathGroupLayer32(layeredImage32.AddLayer(TSmoothPathGroupLayer32));
    //make room to display coordinates
    smoothGroupLayer.DesignMargin := DPIAware(40);
    smoothGroupLayer.OnMerge := BeforeMerge;
    UpdateSmoothPathLayerAttributes;
  end;
  smoothGroupLayer.SmoothPath.Add(PointD(clickPoint), stSmoothSym);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ActiveControl := nil; //remove focus from edit controls in top panel

  clickPoint := Types.Point(X,Y);
  //get the layer that was clicked (if any)
  clickedLayer := layeredImage32.GetLayerAt(clickPoint);

  if (clickedLayer is TSmoothButtonLayer32) then
  begin
    if (clickedLayer = smoothGroupLayer.ActiveButtonLayer) then Exit;
    smoothGroupLayer.ActiveButtonLayer := TSmoothButtonLayer32(clickedLayer);
    Invalidate;
  end
  else if Assigned(smoothGroupLayer) and
    Assigned(smoothGroupLayer.ActiveButtonLayer) then
  begin
    smoothGroupLayer.ActiveButtonLayer := nil;
    Invalidate;
  end;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  layer: TLayer32;
begin
  clickPoint := Types.Point(X,Y);

  layer := layeredImage32.GetLayerAt(clickPoint, mnuHideControls.Checked);
  if Assigned(layer) then
    Cursor := layer.CursorId else
    Cursor := crDefault;

  if not Assigned(clickedLayer) or not (ssLeft in Shift) then Exit;

  if (clickedLayer is TSmoothButtonLayer32) and
    (clickedLayer.GroupOwner = smoothGroupLayer) then
  begin
    with TSmoothButtonLayer32(clickedLayer) do
      smoothGroupLayer.SmoothPath[PathIdx] := PointD(clickPoint);
    Invalidate;
  end;

end;
//------------------------------------------------------------------------------

procedure TFrmMain.pnlMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeleteLastClick(Sender: TObject);
begin
  if Assigned(smoothGroupLayer) then
    smoothGroupLayer.SmoothPath.DeleteLast;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuDeletePathClick(Sender: TObject);
begin
  if Assigned(smoothGroupLayer) then
    smoothGroupLayer.SmoothPath.Clear;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.CopytoClipboard1Click(Sender: TObject);
begin
  layeredImage32.GetMergedImage(mnuHideControls.Checked).CopyToClipBoard;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuHideControlsClick(Sender: TObject);
begin
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuSharpWithHdlsClick(Sender: TObject);
var
  st: TSmoothType;
begin
  if not Assigned(smoothGroupLayer) or 
    not Assigned(smoothGroupLayer.ActiveButtonLayer) then Exit;

  if Sender = mnuSharpWithHdls then st := stSharpWithHdls
  else if Sender = mnuSmoothAsym    then st := stSmoothAsym
  else if Sender = mnuSmoothSym then st := stSmoothSym
  else if Sender = mnuSharpNoHdls then st := stSharpNoHdls
  else Exit;

  with smoothGroupLayer.ActiveButtonLayer do
    smoothGroupLayer.SmoothPath.PointTypes[PathIdx] := st;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.mnuRotateButtonsClick(Sender: TObject);
begin

end;
//------------------------------------------------------------------------------

procedure TFrmMain.edPenColorChange(Sender: TObject);
begin
  if (Length(FrmMain.edPenColor.Text) = 9) and
    (Length(FrmMain.edFillColor.Text) = 9) then
      UpdateSmoothPathLayerAttributes;
end;
//------------------------------------------------------------------------------

procedure TFrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  procedure MovePoint(idx, dx,dy :integer);
  var
    pt: TPointD;
  begin
    if ssCtrl in Shift then
    begin
      dx := dx *5; dy := dy *5;
    end;
    pt := smoothGroupLayer.SmoothPath[idx];
    pt.X := pt.X +dx; pt.Y := pt.Y +dy;
    smoothGroupLayer.SmoothPath[idx] := pt;
    Invalidate;
  end;

begin
  case Key of
    VK_ESCAPE:
      begin
        clickedLayer := nil;
        Key := 0;
        Invalidate;
      end;
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:
      begin
        if Assigned(smoothGroupLayer) and 
          Assigned(smoothGroupLayer.ActiveButtonLayer) then 
            with smoothGroupLayer.ActiveButtonLayer do
              case Key of
                VK_UP: MovePoint(PathIdx, 0,-1);
                VK_DOWN: MovePoint(PathIdx, 0,1);
                VK_LEFT: MovePoint(PathIdx, -1,0);
                VK_RIGHT: MovePoint(PathIdx, 1,0);
              end;
      end;
  end;  
end;
//------------------------------------------------------------------------------

procedure TFrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
