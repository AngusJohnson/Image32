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
    rawPaths, bezierPaths, flattenedPaths: TPathsD;
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
  SetLength(Result, len+2);
  Result[len-1] := #13; Result[len]   := #10;
  Result[len+1] := #13; Result[len+2] := #10;
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
  masterImg.LoadFromResource('book', 'BMP');
  OpenDialog1.InitialDir :=
    ExtractFilePath(paramStr(0)) + 'sample_images';
  ForceDirectories(OpenDialog1.InitialDir);
  OpenDialog1.FileName := 'book.bmp';
  if masterImg.HasTransparency then
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
  i,j, len: integer;
  scale, simplifyTol: double;
  vectorBounds: TRect;
begin
  rawPaths := nil;
  bezierPaths := nil;
  flattenedPaths := nil;

  if masterImg.IsEmpty then Exit;
  if mnuShowMonoImage.Checked then
  begin
    workImg.Assign(masterImg); //shows the raw image only
    with GetDisplaySize do
      workImg.ScaleToFit(cx-margin*2, cy-margin*2);
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := ' Raw raster image';
    Invalidate;
    Exit;
  end;

  with GetDisplaySize do workImg.SetSize(cx, cy);
  simplifyTol := TrackBar2.Position * 0.5 * workImg.Width/masterImg.Width;

  // 1. Vectorize (now includes vector simplification):
  // converts simple (2 color) raster images into vector images
  if masterImg.HasTransparency then
    rawPaths := Vectorize(masterImg, $FF000000, CompareAlpha, $80, simplifyTol) else
    rawPaths := Vectorize(masterImg, $FF000000, CompareRGB, $44, simplifyTol);
  vectorBounds := GetBounds(rawPaths);

  // 1b. scale the vector image
  scale := Min(
    (workImg.Width - Margin*2) / RectWidth(vectorBounds),
    (workImg.Height - Margin*2) / RectHeight(vectorBounds));
  rawPaths := ScalePath(rawPaths, scale);
  rawPaths := OffsetPath(rawPaths,
    margin -vectorBounds.Left, margin -vectorBounds.Top);

  if mnuShowRawPoly.Checked then
  begin
    flattenedPaths := rawPaths;
    StatusBar1.Panels[0].Text := Format(' Vertices: %d', [Count(rawPaths)]);
    StatusBar1.Panels[1].Text := ' Raw Vectors (no smoothing or simplification)';
  end else
  begin

    // 2. SmoothToCubicBezier and flatten result
    // returns a bezier path, NOT a flattened path
    if TrackBar1.Position > 0 then
    begin
      len := Length(rawPaths);
      setLength(bezierPaths, len);
      for i := 0 to len -1 do
        bezierPaths[i] := SmoothToCubicBezier(rawPaths[i],
          true, TrackBar1.Position);
      // flatten beziers
      SetLength(flattenedPaths, Length(bezierPaths));
      for i := 0 to High(bezierPaths) do
        flattenedPaths[i] := FlattenCBezier(bezierPaths[i]);
    end
    else
      flattenedPaths := CopyPaths(rawPaths);

    lblSmooth.Caption :=
      Format('Smooth'#10'Amount'#10'(%d)',[TrackBar1.Position]);
    lblSimplify.Caption :=
      Format('Simplify'#10'Amount'#10'(%d)',[TrackBar2.Position]);

    StatusBar1.Panels[0].Text := Format(' Vertices: %d', [Count(flattenedPaths)]);
    StatusBar1.Panels[1].Text := ' Simplified & smoothed';
  end;

  if mnuHighlightVertices.Checked then
  begin
    DrawPolygon(workImg, flattenedPaths, frEvenOdd, $20660000);
    DrawLine(workImg, flattenedPaths, DPIAware(2), clMaroon32, esPolygon);

    for i := 0 to High(flattenedPaths) do
      for j := 0 to High(flattenedPaths[i]) do
          DrawPoint(workImg, flattenedPaths[i][j], DPIAware(2.5), clNavy32);
  end else
  begin
    DrawPolygon(workImg, flattenedPaths, frEvenOdd, $FF660033);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  masterImg.LoadFromFile(OpenDialog1.FileName);
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
    AddPaths(flattenedPaths, false, $40000033, $FF000033, 1.2);
    SaveToFile(SaveDialog1.FileName, 800,600);
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
