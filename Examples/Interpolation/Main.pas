unit Main;

interface

uses
  Windows, Types, SysUtils, Classes, Controls,
  Forms, Math, ComCtrls, ShellApi,
  Img32, Img32.Panels, Img32.Vector, Img32.Extra,
  Img32.Fmt.PNG, Img32.Draw, Img32.Text;

type
  TMainForm = class(TForm)
    StatusBar1: TStatusBar;
    TabControl1: TTabControl;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    arial12: TFontCache;
    arial16: TFontCache;
    ImagePanel: TImage32Panel;
    procedure DoClosed1;
    procedure DoClosed2;
    procedure DoOpen;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  arialFont: TFontReader;
begin
  FontManager.Load('Arial', 800);
  arialFont := FontManager.GetFont('Arial');
  arial12 := TFontCache.Create(arialFont, DPIAware(12));
  arial16 := TFontCache.Create(arialFont, DPIAware(16));

  ImagePanel := TImage32Panel.Create(self);
  ImagePanel.Parent := TabControl1;
  ImagePanel.Align := alClient;
  ActiveControl := ImagePanel;
  with ImagePanel.InnerClientRect do
    ImagePanel.Image.SetSize(Width, Height);
  TabControl1Change(nil);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  arial12.Free;
  arial16.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.TabControl1Change(Sender: TObject);
begin
  ImagePanel.Image.Clear;
  case TabControl1.TabIndex of
    0: DoClosed1;
    1: DoClosed2;
    else DoOpen;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DoClosed1;
var
  adjustXY: integer;
  path, path2, smoothedPath: TPathD;
  rec: TRect;
const
  margin = 80;
begin
  path :=  MakePath([190,120, 240,160, 560,120, 190,490]);
  rec := ImagePanel.InnerClientRect;
  Types.InflateRect(rec, -margin, -margin);
  rec.Right := rec.Left + (rec.Right - rec.Left) div 3;
  adjustXY := rec.Left div 3;

  path := ScalePathToFit(path, rec);
  smoothedPath := SmoothPath(path, true, 0);

  DrawLine(ImagePanel.Image, path, 1, clRed32, esClosed);
  DrawLine(ImagePanel.Image, smoothedPath, 5, clBlue32, esClosed);
  DrawText(ImagePanel.Image, margin +adjustXY,
    rec.Top + adjustXY*4, 'Tension = 0', arial16);
  smoothedPath := SmoothPath(path, true, -1);
  path2 := TranslatePath(path, rec.Right - margin, 0);
  smoothedPath := TranslatePath(smoothedPath, rec.Right - margin, 0);

  DrawLine(ImagePanel.Image, path2, 1, clRed32, esClosed);
  DrawLine(ImagePanel.Image, smoothedPath, 5, clBlue32, esClosed);
  DrawText(ImagePanel.Image, rec.Right +adjustXY,
    rec.Top + adjustXY*4, 'Tension = -1', arial16);

  smoothedPath := SmoothPath(path, true, 0.5);
  path2 := TranslatePath(path, rec.Right *2 - margin*2, 0);
  smoothedPath := TranslatePath(smoothedPath, rec.Right *2 - margin*2, 0);

  DrawLine(ImagePanel.Image, path2, 1, clRed32, esClosed);
  DrawLine(ImagePanel.Image, smoothedPath, 5, clBlue32, esClosed);
  DrawText(ImagePanel.Image, rec.Right*2 +adjustXY - margin,
    rec.Top + adjustXY*4, 'Tension = 0.5', arial16);

end;
//------------------------------------------------------------------------------

procedure TMainForm.DoClosed2;
var
  i,j, dx, dy: integer;
  paths, smoothedPaths: TPathsD;
  rec: TRect;
  mp: TPoint;
const
  margin = 50;
  ptCount = 4;
  pathCount = 1;
begin
  rec := ImagePanel.InnerClientRect;
  Types.InflateRect(rec, -margin*3, -margin);
  dx := rec.Width div 2;
  dy := rec.Height div 2;
  mp := MidPoint(rec);

  SetLength(paths, pathCount);
  SetLength(smoothedPaths, pathCount);

  for i := 0 to High(paths) do
  begin
    SetLength(paths[i], ptCount);
    paths[i][0] := PointD(mp.X - Random(dx), mp.Y - Random(dy));
    paths[i][1] := PointD(mp.X + Random(dx), mp.Y - Random(dy));
    paths[i][2] := PointD(mp.X + Random(dx), mp.Y + Random(dy));
    paths[i][3] := PointD(mp.X - Random(dx), mp.Y + Random(dy));
  end;
  for i := 0 to High(smoothedPaths) do
    smoothedPaths[i] := SmoothPath(paths[i], true, 0);

  for i := 0 to High(smoothedPaths) do
  begin
    DrawLine(ImagePanel.Image, smoothedPaths[i], 5, clGreen32, esPolygon);
    for j := 0 to High(paths[i]) do
       DrawPoint(ImagePanel.Image, paths[i][j], 7, clRed32);
  end;

end;
//------------------------------------------------------------------------------

procedure TMainForm.DoOpen;
var
  i,j, dx: integer;
  paths, smoothedPaths: TPathsD;
  rec: TRect;
const
  margin = 50;
  ptCount = 8;
  pathCount = 3;
begin
  rec := ImagePanel.InnerClientRect;
  Types.InflateRect(rec, -margin, -margin);
  dx := rec.Width div (ptCount);

  SetLength(paths, pathCount);
  SetLength(smoothedPaths, pathCount);

  for i := 0 to High(paths) do
  begin
    SetLength(paths[i], ptCount);
    for j := 0 to High(paths[i]) do
      paths[i][j] := PointD(rec.Left + j*dx, rec.Bottom - Random(rec.Height));
  end;
  for i := 0 to High(smoothedPaths) do
    smoothedPaths[i] := SmoothPath(paths[i], false, 0);

  for i := 0 to High(smoothedPaths) do
  begin
    DrawLine(ImagePanel.Image, smoothedPaths[i], 5,
      RainbowColor(i/pathCount), esSquare);
    for j := 0 to High(paths[i]) do
       DrawPoint(ImagePanel.Image, paths[i][j], 5, clRed32);
  end;

end;
//------------------------------------------------------------------------------

end.
