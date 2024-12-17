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
    procedure FormResize(Sender: TObject);
  private
    arial12: TFontCache;
    arial16: TFontCache;
    ImagePanel: TImage32Panel;
    procedure ImagePanelClick(Sender: TObject);
    procedure DoClosedPaths1;
    procedure DoClosedPaths2;
    procedure DoOpenPaths;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  arialFont: TFontReader;
begin
  arialFont := FontManager.LoadFontReader('Arial');
  arial12 := TFontCache.Create(arialFont, DPIAware(12));
  arial16 := TFontCache.Create(arialFont, DPIAware(16));

  ImagePanel := TImage32Panel.Create(self);
  ImagePanel.Parent := TabControl1;
  ImagePanel.Align := alClient;
  ImagePanel.OnClick := ImagePanelClick;
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

procedure TMainForm.ImagePanelClick(Sender: TObject);
begin
  if TabControl1.TabIndex <> 0 then
    TabControl1Change(nil);
end;
//------------------------------------------------------------------------------

procedure TMainForm.TabControl1Change(Sender: TObject);
begin
  ImagePanel.Scale := 1.0;
  ImagePanel.Image.Clear;
  case TabControl1.TabIndex of
    0: DoClosedPaths1;
    1: DoClosedPaths2;
    else DoOpenPaths;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  with ImagePanel.InnerClientRect do
    ImagePanel.Image.SetSize(Width, Height);
  TabControl1Change(nil);
end;
//------------------------------------------------------------------------------

procedure TMainForm.DoClosedPaths1;
var
  margin, adjustX: integer;
  path, smoothedPath: TPathD;
  srcRec, spRec, dstRec: TRect;
  scale, dx, dy: double;
begin
  margin := dpiAware(20);
  path := MakePath([190,120, 240,160, 560,120, 190,490]);

  // get the bounds of the smoothpath with the largest bounds
  smoothedPath := SmoothPath(path, true, -1);
  spRec := GetBounds(smoothedPath);

  // get dstRec
  dstRec := ImagePanel.InnerClientRect;
  Types.InflateRect(dstRec, -margin, -margin);
  dstRec.Width := dstRec.Width div 3 - margin;
  inc(dstRec.Top, DPIAware(20));    //making sure there's room for text
  dec(dstRec.Bottom, DPIAware(20)); //making sure there's room for text
  adjustX := dstRec.Width + margin;

  DrawText(ImagePanel.Image, dstRec.Left, dstRec.Top - DpiAware(20),
    'SmoothPath function - using different tensions', arial16);

  scale := Min(dstRec.Width/spRec.Width, dstRec.Height/spRec.Height);
  path := ScalePath(path, scale);

  dx := dstRec.Left - spRec.Left * scale;
  dy := dstRec.Top - spRec.Top * scale;

  path := TranslatePath(path, dx, dy);
  srcRec := GetBounds(path);

  smoothedPath := SmoothPath(path, true, 0);
  DrawLine(ImagePanel.Image, path, DpiAware(1), clRed32, esClosed);
  DrawLine(ImagePanel.Image, smoothedPath, DpiAware(2), clBlue32, esClosed);
  DrawText(ImagePanel.Image, srcRec.Left, srcRec.Bottom + dpiAware(20), '0', arial16);

  path := TranslatePath(path, adjustX, 0);
  TranslateRect(srcRec, adjustX, 0);

  smoothedPath := SmoothPath(path, true, -1);
  DrawLine(ImagePanel.Image, path, DpiAware(1), clRed32, esClosed);
  DrawLine(ImagePanel.Image, smoothedPath, DpiAware(2), clBlue32, esClosed);
  DrawText(ImagePanel.Image, srcRec.Left, srcRec.Bottom + dpiAware(20), '-1', arial16);

  path := TranslatePath(path, adjustX, 0);
  TranslateRect(srcRec, adjustX, 0);

  smoothedPath := SmoothPath(path, true, 0.5);
  DrawLine(ImagePanel.Image, path, DpiAware(1), clRed32, esClosed);
  DrawLine(ImagePanel.Image, smoothedPath, DpiAware(2), clBlue32, esClosed);
  DrawText(ImagePanel.Image, srcRec.Left, srcRec.Bottom + dpiAware(20), '0.5', arial16);
end;
//------------------------------------------------------------------------------

procedure TMainForm.DoClosedPaths2;
var
  i,j, maxX,maxY: integer;
  path, smoothedPath: TPathD;
  dstRec, srcRec: TRect;
  scaleX, scaleY: double;
const
  margin = 50;
  ptCount = 3;
begin
  SetLength(path, ptCount);

  dstRec := ImagePanel.InnerClientRect;
  Types.InflateRect(dstRec, -margin, -margin);
  maxX := dstRec.Width;
  maxY := dstRec.Height;

  for i := 0 to ptCount -1 do
    path[i] := PointD(Random(maxX), Random(maxY));
  smoothedPath := SmoothPath(path, true, -0.5);
  srcRec := GetBounds(smoothedPath);
  scaleX := maxX /srcRec.Width;
  scaleY := maxY /srcRec.Height;
  path := ScalePath(path, scaleX, scaleY);

  // repeat smoothing now that the path has been properly scaled
  smoothedPath := SmoothPath(path, true, -0.5);
  // re-centre paths
  srcRec := GetBounds(smoothedPath);
  path := TranslatePath(path, margin - srcRec.Left, margin -srcRec.Top);
  smoothedPath := TranslatePath(smoothedPath, margin -srcRec.Left, margin -srcRec.Top);

  DrawLine(ImagePanel.Image, smoothedPath, DpiAware(2.5), clGreen32, esPolygon);
  for j := 0 to High(path) do
    DrawPoint(ImagePanel.Image, path[j], DpiAware(3.5), clRed32);

end;
//------------------------------------------------------------------------------

procedure TMainForm.DoOpenPaths;
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
    DrawLine(ImagePanel.Image, smoothedPaths[i], DpiAware(3),
      RainbowColor(i/pathCount), esSquare);
    for j := 0 to High(paths[i]) do
       DrawPoint(ImagePanel.Image, paths[i][j], DpiAware(2.5), clRed32);
  end;
end;
//------------------------------------------------------------------------------

end.
