unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls,
  Img32, SimpleSvgWrite, Dialogs, StdCtrls, ShellApi;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    View1: TMenuItem;
    mnuShowMonoImage: TMenuItem;
    mnuShowRawPoly: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    N3: TMenuItem;
    SaveDialog1: TSaveDialog;
    SaveAs1: TMenuItem;
    mnuShowSimplifiedSmoothed: TMenuItem;
    pnlSmooth: TPanel;
    lblSmooth: TLabel;
    TrackBar1: TTrackBar;
    mnuHighlightVertices: TMenuItem;
    lblSimplify: TLabel;
    TrackBar2: TTrackBar;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuShowSimplifiedClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure pnlSmoothExit(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuHighlightVerticesClick(Sender: TObject);
    procedure pnlSmoothEnter(Sender: TObject);
  private
    masterImg, workImg: TImage32;
    hasTransparency: Boolean; // must be checked before resizing
    rawPaths, bezierPaths, smoothedPaths: TPathsD;
    function GetDisplaySize: TSize;
    procedure DisplayImage;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R Image.res}

uses
  Img32.Draw, Img32.Vector, Img32.Extra, Img32.Vectorizer,
  Img32.Fmt.BMP, Img32.Fmt.JPG, Img32.Fmt.PNG;

 const
   margin = 20;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function PointToStr(const pt: TPointD): string;
begin
  result := Format('%1.0f,%1.0f, ', [pt.X, pt.Y]);
end;
//------------------------------------------------------------------------------

function PathToStr(const path: TPathD): string;
var
  i, len: integer;
begin
  result := '';
  if path = nil then Exit;
  for i := 0 to high(path) do
    result := result + PointToStr(path[i]);
  len := Length(Result);
  SetLength(Result, len + 2);
  Result[len - 1] := #13; Result[len]   := #10;
  Result[len + 1] := #13; Result[len + 2] := #10;
end;
//------------------------------------------------------------------------------

function PathsToStr(const paths: TPathsD): string;
var
  i: integer;
begin
  for i := 0 to high(paths) do
    result := result + PathToStr(paths[i]);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  masterImg := TImage32.Create;
  masterImg.LoadFromResource('beetle', 'PNG');
  hasTransparency := masterImg.HasTransparency;
  masterImg.ScaleToFit(1000, 1000);
  OpenDialog1.InitialDir :=
    ExtractFilePath(paramStr(0)) + 'sample_images';
  ForceDirectories(OpenDialog1.InitialDir);
  OpenDialog1.FileName := 'book.bmp';
  if hasTransparency then
    masterImg.CropTransparentPixels;
  workImg := TImage32.Create;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  masterImg.Free;
  workImg.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormPaint(Sender: TObject);
begin
  workImg.CopyToDc(Self.Canvas.Handle);
end;
//------------------------------------------------------------------------------

function TForm1.GetDisplaySize: TSize;
begin
  Result.cx := ClientWidth - pnlSmooth.Width;
  Result.cy := ClientHeight - StatusBar1.Height;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormResize(Sender: TObject);
begin
  if not (csDestroying in ComponentState) then
    DisplayImage;
end;
//------------------------------------------------------------------------------

function Count(const paths: TPathsD): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to High(paths) do
    inc(Result, Length(paths[i]));
end;
//------------------------------------------------------------------------------

procedure TForm1.DisplayImage;
var
  i, j: integer;
  scale, simplifyTol: double;
  vectorBounds: TRect;
  //tmpColors: TArrayOfColor32;
begin
  rawPaths := nil;
  bezierPaths := nil;
  smoothedPaths := nil;

  Invalidate;
  if masterImg.IsEmpty then Exit;

  if mnuShowMonoImage.Checked then
  begin
    workImg.Assign(masterImg); //shows the raw image only
    with GetDisplaySize do
      workImg.ScaleToFit(cx-margin * 2, cy-margin * 2);
//    // otherwise, show as a monochrome image
//    tmpColors := GetColorMask(workImg, clBlack32, CompareRGB, $32);
//    Move(tmpColors[0], workImg.Pixels[0], Length(tmpColors) * SizeOf(TColor32));
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := ' Raw raster image';
    Exit;
  end;

  with GetDisplaySize do workImg.SetSize(cx, cy);
  if mnuShowRawPoly.Checked then
    simplifyTol := 0 else
    simplifyTol := TrackBar2.Position * 0.5;

  // 1. Vectorize (now includes vector simplification):
  // converts simple (2 color) raster images into vector images
  if hasTransparency then
    rawPaths := Vectorize(masterImg, $FF000000, CompareAlpha, $80, simplifyTol) else
    rawPaths := Vectorize(masterImg, $FF000000, CompareRGB, $44, simplifyTol);

  vectorBounds := GetBounds(rawPaths);
  if vectorBounds.IsEmpty then Exit;

  // 1b. offset and scale the vector image

  rawPaths := TranslatePath(rawPaths,
    margin -vectorBounds.Left, margin -vectorBounds.Top);

  scale := Min(
    (workImg.Width - Margin * 2) / RectWidth(vectorBounds),
    (workImg.Height - Margin * 2) / RectHeight(vectorBounds));
  rawPaths := ScalePath(rawPaths, scale);

  if mnuShowRawPoly.Checked then
  begin
    smoothedPaths := rawPaths;
    StatusBar1.Panels[0].Text := Format(' Vertices: %d', [Count(rawPaths)]);
    StatusBar1.Panels[1].Text := ' Raw Vectors (no smoothing or simplification)';
  end else
  begin

    smoothedPaths := SmoothPaths(rawPaths, true, (10-TrackBar1.Position) / 10, 0.25);

    lblSmooth.Caption :=
      Format('Smooth'#10'Amount'#10'(%d)',[TrackBar1.Position]);
    lblSimplify.Caption :=
      Format('Simplify'#10'Amount'#10'(%d)',[TrackBar2.Position]);

    StatusBar1.Panels[0].Text := Format(' Vertices: %d', [Count(smoothedPaths)]);
    StatusBar1.Panels[1].Text := ' Simplified & smoothed';
  end;

  if mnuHighlightVertices.Checked then
  begin
    DrawPolygon(workImg, smoothedPaths, frEvenOdd, $20660000);
    DrawLine(workImg, smoothedPaths, DPIAware(2), clMaroon32, esPolygon);

    for i := 0 to High(smoothedPaths) do
      for j := 0 to High(smoothedPaths[i]) do
          DrawPoint(workImg, smoothedPaths[i][j], DPIAware(2.5), clNavy32);
  end else
  begin
    DrawPolygon(workImg, smoothedPaths, frEvenOdd, $FF660033);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  masterImg.LoadFromFile(OpenDialog1.FileName);
  hasTransparency := masterImg.HasTransparency;
  masterImg.ScaleToFit(1000, 1000);
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlSmoothEnter(Sender: TObject);
begin
  //pnlSmooth.Color := clGradientInactiveCaption;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlSmoothExit(Sender: TObject);
begin
  //pnlSmooth.Color := clBtnFace;
end;
//------------------------------------------------------------------------------

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuShowSimplifiedClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  pnlSmooth.Visible := Sender = mnuShowSimplifiedSmoothed;
  if pnlSmooth.Visible then TrackBar1.SetFocus;
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuHighlightVerticesClick(Sender: TObject);
begin
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  SaveDialog1.InitialDir := OpenDialog1.InitialDir;
  SaveDialog1.FileName :=
    ChangeFileExt(ExtractFileName(OpenDialog1.FileName), '.svg');
  if not SaveDialog1.Execute then Exit;

  with TSimpleSvgWriter.Create(frEvenOdd) do
  try
    AddPaths(smoothedPaths, false, $40000033, $FF000033, 1.2);
    SaveToFile(SaveDialog1.FileName, 800, 600);
  finally
    free;
  end;
  ShellExecute(0, 'open', PChar(SaveDialog1.FileName), nil, nil, SW_SHOW);

  StatusBar1.Panels[1].Text :=
    format(' %s saved.',[ExtractFilename(SaveDialog1.FileName)]);
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
