unit Unit1;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls,
  Image32, ImagePanels;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PnlMainDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    timer: TTimer;
    pnlMain: TBitmapPanel;
    reversing: Boolean;
    imgIndex: integer;
    masterImageList: TImageList32;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Image32_Draw, Image32_Vector, Image32_Extra;

//------------------------------------------------------------------------------

const
  margin = 100;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, ImageSize, SpaceAbove, sqSize: integer;
  j,k: double;
  img, img2: TImage32;
  path, path2: TPathD;
  rec: TRect;
begin
  ImageSize := DPIAware(100);
  SpaceAbove := ImageSize *2;

  //SETUP THE DISPLAY PANEL
  pnlMain := TBitmapPanel.Create(self);
  pnlMain.Parent := self;
  pnlMain.Align := alClient;

  //Panel1.FocusedColor := clGradientInactiveCaption;
  //enable bitmap transparency (ie to the panel background)
  pnlMain.Bitmap.PixelFormat := pf32bit;
  pnlMain.Bitmap.Width := ImageSize;
  pnlMain.Bitmap.Height := ImageSize + SpaceAbove;
  pnlMain.AllowZoom := false;
  pnlMain.AllowScroll := false;
  pnlMain.OnDblClick := PnlMainDblClick;

  rec := Rect(0,0,ImageSize,ImageSize);
  Windows.InflateRect(rec, -15,-15);
  path := Ellipse(rec);

  //path2: for drawing a black line at the bottom of the image
  path2 := MakePathI([0, ImageSize*3-8, ImageSize, ImageSize*3 -8]);

  masterImageList := TImageList32.Create;

  //31 images (25 + 6) will be added to masterImageList. Each will be viewed
  //twice in each loop except for the top and bottom images. (60 frames/loop)

  img := TImage32.Create(ImageSize, ImageSize);
  DrawPolygon(img, path, frNonZero, clLime32);
  Draw3D(img, path, frNonZero, 8, 8);
  DrawLine(img, path, 3, clGreen32, esPolygon);

  //acceleration phase
  k := power(SpaceAbove, 1/25); //k^25 = SpaceAbove
  j := k;
  for i := 1 to 25 do
  begin
    img2 := TImage32.Create(ImageSize, ImageSize + SpaceAbove);
    rec := Rect(0, Round(j), ImageSize, Round(j) + ImageSize);
    j := j * k;
    img2.CopyBlend(img, img.Bounds, rec);
    DrawLine(img2, path2, 5, clBlack32, esSquare);
    masterImageList.Add(img2);
  end;

  //deceleration (squishing) phase :)
  sqSize := ImageSize *2 div 3;
  k := power(sqSize, 1/6); //k^6 = sqSize
  j := 100/k;
  for i := 1 to 6 do
  begin
    img2 := TImage32.Create(ImageSize, ImageSize + SpaceAbove);
    rec := Rect(0, SpaceAbove + sqSize -Round(j), ImageSize, SpaceAbove +ImageSize);
    j := j/k;
    img2.CopyBlend(img, img.Bounds, rec);
    DrawLine(img2, path2, 5, clBlack32, esSquare);
    masterImageList.Add(img2);
  end;
  img.Free;

  //set up the timer to kick things off
  timer := TTimer.Create(self);
  timer.OnTimer := Timer1Timer;
  //There are 60 images to cycle through here every second so, given that the
  //time to execute the OnTimer event is negligble, I'd expect to set the timer
  //interval to about 17 msec (ie 1000/60). Unfortunately Delphi's TTimer
  //(a wrapper for the Windows timer) isn't very accurate and, at least on my
  //PC, even a 16 msec interval takes ~1.3 secs to complete. And the timer
  //isn't uniformly slow since a 15 msec interval completes in ~0.9 secs, as
  //expected. Anyhow, if I wanted an accurate metronome, I'd need to replace
  //the TTimer component with a much more accurate one.
  timer.Interval := 15;
  timer.Enabled := true;

end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  masterImageList.Free;
  timer.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  masterImageList[imgIndex].CopyToDc(pnlMain.Bitmap.Canvas.Handle, 0,0, false);
  pnlMain.Repaint;
  if reversing then
  begin
    dec(imgIndex);
    if (imgIndex = 0) then reversing := false;
  end else
  begin
    inc(imgIndex);
    if (imgIndex = masterImageList.Count -1) then reversing := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TForm1.PnlMainDblClick(Sender: TObject);
begin
  //halve speed of bouncing ball
  timer.Interval := timer.Interval *2;
end;
//------------------------------------------------------------------------------

end.
