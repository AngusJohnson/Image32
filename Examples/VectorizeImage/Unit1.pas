unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls,
  Image32, BitmapPanels, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    View1: TMenuItem;
    mnuShowImage: TMenuItem;
    mnuShowPolygons: TMenuItem;
    mnuShowBoth: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    N2: TMenuItem;
    mnuShowPolygonCoordinates: TMenuItem;
    mnuSmoothCurve: TMenuItem;
    N3: TMenuItem;
    pnlMemo: TPanel;
    Panel3: TPanel;
    btnCloseMemo: TButton;
    Memo1: TMemo;
    pnlDisplayParent: TPanel;
    pnlSmooth: TPanel;
    DisplayPanel: TPanel;
    TrackBar1: TTrackBar;
    rbBeziers: TRadioButton;
    rbFlat: TRadioButton;
    SaveDialog1: TSaveDialog;
    SaveAs1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuShowBothClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure btnCloseMemoClick(Sender: TObject);
    procedure mnuShowPolygonCoordinatesClick(Sender: TObject);
    procedure pnlSmoothEnter(Sender: TObject);
    procedure pnlSmoothExit(Sender: TObject);
    procedure rbBeziersClick(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
  private
    img, displayImg: TImage32;
    bezierPaths, flattenedPaths: TArrayOfArrayOfPointD;
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
  Image32_Draw, Image32_Vector, Image32_Extra, Image32_PNG;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function PointToStr(const pt: TPointD): string;
begin
  result := Format('%1.0f,%1.0f, ', [pt.X, pt.Y]);
end;
//------------------------------------------------------------------------------

function PathToStr(const path: TArrayOfPointD): string;
var
  i, len: integer;
begin
  result := '';
  if path = nil then Exit;
  for i := 0 to high(path) do
    result := result + PointToStr(path[i]);
  len := Length(Result);
  SetLength(Result, len+2);
  Result[len-1] := #13;
  Result[len] := #10;
  Result[len+1] := #13;
  Result[len+2] := #10;
end;
//------------------------------------------------------------------------------

function PathsToStr(const paths: TArrayOfArrayOfPointD): string;
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
  pnlMemo.Align := alClient;
  pnlMemo.Visible := false;

  //SETUP THE DISPLAY PANEL
  pnlDisplayParent.Align := alClient;
  DisplayPanel.BorderWidth := DPI(16);
  //Panel1.BevelInner := bvLowered;     //set in IDE
  //Panel1.TabStop := true;             //set in IDE (for keyboard controls)
  DisplayPanel.FocusedColor := clGradientInactiveCaption;
  DisplayPanel.BitmapProperties.Scale := 1;
  //to enable image transparency (ie as far as the panel background)
  DisplayPanel.Bitmap.PixelFormat := pf32bit;
  DisplayPanel.BitmapProperties.ScaleType := stFit;

  img := TImage32.Create;
  //img.LoadFromFile('test.png');
  img.LoadFromResource('sample2', 'PNG');
  displayImg := TImage32.Create;

  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  img.Free;
  displayImg.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.DisplayImage;
begin
  if mnuShowPolygons.Checked then
    displayImg.SetSize(img.Width, img.Height) else
    displayImg.Assign(img);

  if not mnuShowImage.Checked then
  begin
    //ie display vectorized image ...

    if img.HasTransparency then
      flattenedPaths := Vectorize(img, $FF000000, CompareAlpha, $99) else
      flattenedPaths := Vectorize(img, $FF000000, CompareRGB, $99); //compare for black

    TrackBar1.Enabled := mnuSmoothCurve.Checked;
    if mnuSmoothCurve.Checked then
    begin
      //smooth the paths (nb: SmoothLine returns poly-bezier paths)
      bezierPaths := SmoothLine(flattenedPaths, true, TrackBar1.Position);
      //now 'flatten' the poly-bezier paths
      flattenedPaths := FlattenCBezier(bezierPaths);

      StatusBar1.SimpleText :=
        format(' Vectorized WITH line smoothing (amount = %d)',
        [trackbar1.Position]);
    end else
    begin
      bezierPaths := nil;
      StatusBar1.SimpleText := ' Vectorized WITHOUT line smoothing';
    end;

    DrawPolygon(displayImg, flattenedPaths, frEvenOdd, $BB990000); //clMaroon32
    //DrawLine(displayImg, flattenedPaths, 1, $ff000000, esClosed);
    //DrawPoint(displayImg, flattenedPaths, 1, clNavy32);
  end;

  {$IFDEF SETSIZE}
  DisplayPanel.Bitmap.SetSize(displayImg.Width, displayImg.Height);
  {$ELSE}
  DisplayPanel.Bitmap.Width := displayImg.Width;
  DisplayPanel.Bitmap.Height := displayImg.Height;
  {$ENDIF}

  DisplayPanel.ClearBitmap; //otherwise images will be blended
  displayImg.CopyToDc(DisplayPanel.Bitmap.Canvas.Handle);
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  btnCloseMemoClick(nil);
  img.LoadFromFile(OpenDialog1.FileName);
  DisplayPanel.BitmapProperties.ScaleType := stFit;
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
  if DisplayPanel.CanFocus then DisplayPanel.SetFocus;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuShowBothClick(Sender: TObject);
begin
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuShowPolygonCoordinatesClick(Sender: TObject);
begin
  pnlDisplayParent.Hide;
  pnlMemo.Show;
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
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
    memo1.Lines.Add(PathsToStr(bezierPaths)) else
    memo1.Lines.Add(PathsToStr(flattenedPaths));
  memo1.SelStart := 0;
  memo1.Lines.EndUpdate;
end;
//------------------------------------------------------------------------------

procedure TForm1.btnCloseMemoClick(Sender: TObject);
begin
  if not pnlDisplayParent.Visible then
  begin
    pnlDisplayParent.Show;
    DisplayPanel.SetFocus;
    pnlMemo.Hide;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    displayImg.SaveToFile(SaveDialog1.FileName);
    StatusBar1.SimpleText :=
      format(' %s saved.',[ExtractFilename(SaveDialog1.FileName)]);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  if not pnlDisplayParent.Visible then
    btnCloseMemoClick(nil) else
    Close;
end;
//------------------------------------------------------------------------------

end.
