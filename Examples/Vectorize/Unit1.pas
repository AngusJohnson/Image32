unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls,
  Image32, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    View1: TMenuItem;
    mnuShowMonoImage: TMenuItem;
    mnuShowRawPoly: TMenuItem;
    mnuShowSimplified: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    mnuShowPolygonCoordinates: TMenuItem;
    N3: TMenuItem;
    pnlMemo: TPanel;
    Panel3: TPanel;
    btnCloseMemo: TButton;
    Memo1: TMemo;
    rbBeziers: TRadioButton;
    rbFlat: TRadioButton;
    SaveDialog1: TSaveDialog;
    SaveAs1: TMenuItem;
    mnuShowSmoothedOnly: TMenuItem;
    mnuShowSimplifiedSmoothed: TMenuItem;
    rbRaw: TRadioButton;
    pnlSmooth: TPanel;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuShowSimplifiedClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure btnCloseMemoClick(Sender: TObject);
    procedure mnuShowPolygonCoordinatesClick(Sender: TObject);
    procedure pnlSmoothEnter(Sender: TObject);
    procedure pnlSmoothExit(Sender: TObject);
    procedure rbBeziersClick(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    masterImg, workImg: TImage32;
    rawPaths, bezierPaths, flattenedPaths: TPathsD;
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
  Image32_Draw, Image32_Vector, Image32_Extra,
  Image32_BMP, Image32_JPG, Image32_PNG;

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
  masterImg.LoadFromResource('sample2', 'PNG');

  workImg := TImage32.Create;
  DisplayImage;
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
  with workImg do
    CopyToDc(Bounds,
      Self.Canvas.Handle,
      (ClientWidth - pnlSmooth.Width - Width) div 2,
      (ClientHeight - Height) div 2);
end;
//------------------------------------------------------------------------------

procedure TForm1.FormResize(Sender: TObject);
begin
end;
//------------------------------------------------------------------------------

procedure TForm1.DisplayImage;
begin
  rawPaths := nil;  bezierPaths := nil; flattenedPaths := nil;
  TrackBar1.Enabled :=
    mnuShowSmoothedOnly.Checked or mnuShowSimplifiedSmoothed.Checked;
  rbBeziers.Enabled := TrackBar1.Enabled;
  rbRaw.Enabled := not mnuShowMonoImage.Checked;

  if mnuShowMonoImage.Checked then
  begin
    workImg.Assign(masterImg); //shows the raw image only
    StatusBar1.SimpleText := ' Raw raster image';
  end else
  begin
    workImg.SetSize(masterImg.Width, masterImg.Height);

    //get the raw vectors (either using transparency or filtering on black)
    if masterImg.HasTransparency then
      rawPaths := Vectorize(masterImg, $FF000000, CompareAlpha, $80) else
      rawPaths := Vectorize(masterImg, $FF000000, CompareRGB, $44);

    //RamerDouglasPeucker: simplifies raw vectors (removes extraneous vertices)
    //nb: the 'epsilon' value may need to be increased when the image was
    //enlarged before vectorizing, effectively making pixel dimensions >1.
    //Epsilon should be +1 greater than meaningful pixel size.
    if mnuShowSimplified.Checked then
      flattenedPaths := RamerDouglasPeucker(rawPaths, 1)
    else if mnuShowSimplifiedSmoothed.Checked then
      flattenedPaths := RamerDouglasPeucker(rawPaths, Min(3, TrackBar1.Position /2))
    else
      flattenedPaths := rawPaths;

    if mnuShowSmoothedOnly.Checked then
    begin
      //note: SmoothToBezier returns poly-bezier paths
      bezierPaths := SmoothToBezier(rawPaths, true, TrackBar1.Position, 2);
      //and finally 'flatten' the poly-bezier paths
      flattenedPaths := FlattenCBezier(bezierPaths);
    end
    else if mnuShowSimplifiedSmoothed.Checked then
    begin
      //note: SmoothToBezier returns poly-bezier paths (not flattened paths)
      bezierPaths := SmoothToBezier(flattenedPaths, true, TrackBar1.Position, 2);
      //and finally 'flatten' the poly-beziers
      flattenedPaths := FlattenCBezier(bezierPaths);
    end;
    DrawPolygon(workImg, flattenedPaths, frEvenOdd, clNavy32);

    if mnuShowRawPoly.Checked then
      StatusBar1.SimpleText := ' Polygons drawn using raw vectors'
    else if mnuShowSimplified.Checked then
      StatusBar1.SimpleText :=
        ' Polygons drawn using simplified vectors (Ramer-Douglas-Peucker)'
    else if mnuShowSmoothedOnly.Checked then
      StatusBar1.SimpleText := format(' Polygons drawn using '+
        'smoothing only (amount = %d)', [trackbar1.Position])
    else
      StatusBar1.SimpleText := format(' Polygons drawn using both '+
        'simplify and smoothing (amount = %d)', [trackbar1.Position]);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  btnCloseMemoClick(nil);
  masterImg.LoadFromFile(OpenDialog1.FileName);
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlSmoothEnter(Sender: TObject);
begin
  pnlSmooth.Color := clGradientInactiveCaption;
end;
//------------------------------------------------------------------------------

procedure TForm1.pnlSmoothExit(Sender: TObject);
begin
  pnlSmooth.Color := clBtnFace;
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
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuShowPolygonCoordinatesClick(Sender: TObject);
begin
  pnlSmooth.Hide;
  pnlMemo.Show;
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  if not rbFlat.Checked then
    rbFlat.Checked := true else
    rbBeziersClick(nil);
  memo1.SetFocus;
  Screen.Cursor := crDefault;
end;
//------------------------------------------------------------------------------

procedure TForm1.rbBeziersClick(Sender: TObject);
begin
  memo1.Lines.BeginUpdate;
  memo1.Clear;
  if rbBeziers.Checked then
    memo1.Lines.Add(PathsToStr(bezierPaths))
  else if rbRaw.Checked then
    memo1.Lines.Add(PathsToStr(rawPaths))
  else
    memo1.Lines.Add(PathsToStr(flattenedPaths));
  memo1.SelStart := 0;
  memo1.Lines.EndUpdate;
end;
//------------------------------------------------------------------------------

procedure TForm1.btnCloseMemoClick(Sender: TObject);
begin
  if not pnlSmooth.Visible then
  begin
    pnlSmooth.Show;
    pnlMemo.Hide;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    workImg.SaveToFile(SaveDialog1.FileName);
    StatusBar1.SimpleText :=
      format(' %s saved.',[ExtractFilename(SaveDialog1.FileName)]);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Case Key of
    Ord('A'):
      if (ssCtrl in Shift) and not pnlSmooth.Visible then
        Memo1.SelectAll;
    Ord('C'), Ord('X'):
      if (ssCtrl in Shift) and not pnlSmooth.Visible then
      begin
        if Memo1.SelLength = 0 then Memo1.SelectAll;
        Memo1.CopyToClipboard;
      end;
  End;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  if not pnlSmooth.Visible then
    btnCloseMemoClick(nil) else
    Close;
end;
//------------------------------------------------------------------------------

end.
