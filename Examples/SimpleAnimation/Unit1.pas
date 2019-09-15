unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls,
  Image32, BitmapPanels, Timer;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
  private
    timer: TTimerEx;
    reversing: Boolean;
    imgIndex: integer;
    masterImageList: TImageList32;
    procedure DoTimer(sender: TObject);
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
  path, path2: TArrayOfPointD;
  rec: TRect;
begin
  ImageSize := DPI(100);
  SpaceAbove := ImageSize *2;

  //SETUP THE DISPLAY PANEL
  Panel1.BorderWidth := DPI(16);
  Panel1.BevelInner := bvLowered;
  //set Panel1.Tabstop := true to enable keyboard controls
  Panel1.TabStop := true;
  Panel1.FocusedColor := clGradientInactiveCaption;
  Panel1.BitmapProperties.Scale := 1;
  Panel1.Bitmap.SetSize(ImageSize, ImageSize + SpaceAbove);

  rec := Rect(0,0,ImageSize,ImageSize);
  Windows.InflateRect(rec, -15,-15);
  path := Ellipse(rec);

  //path2: for drawing a black line at the bottom of the image
  path2 := MakePathI([0, ImageSize*3-8, ImageSize, ImageSize*3 -8]);

  masterImageList := TImageList32.Create;

  //add 31 images to masterImageList that are viewed twice each loop
  //except the top and bottom images (viewed once) ==> 60/loop
  img := TImage32.Create(ImageSize, ImageSize);
  DrawPolygon(img, path, frNonZero, clLime32);
  Draw3D(img, path, frNonZero, 8, 8);
  DrawLine(img, path, 3, clGreen32, esClosed);

  //acceleration phase
  k := power(SpaceAbove, 1/25); //k^25 = SpaceAbove
  j := k;
  for i := 1 to 25 do
  begin
    img2 := TImage32.Create(ImageSize, ImageSize + SpaceAbove);
    rec := Rect(0, Round(j), ImageSize, Round(j) + ImageSize);
    j := j * k;
    img2.CopyFrom(img, img.Bounds, rec);
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
    img2.CopyFrom(img, img.Bounds, rec);
    DrawLine(img2, path2, 5, clBlack32, esSquare);
    masterImageList.Add(img2);
  end;
  img.Free;

  //and setup the timer to kick things off
  timer := TTimerEx.Create;
  timer.OnTimer := DoTimer;
  timer.Interval := 1000/60; //1000msec/60frames = 60frames/sec
  timer.Priority := tpLowest;
  timer.Enabled := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  masterImageList.Free;
  timer.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.DoTimer(sender: TObject);
begin
  masterImageList[imgIndex].CopyToDc(Panel1.Bitmap.Canvas.Handle, 0,0, false);
  Panel1.Repaint;
  if reversing then
  begin
    dec(imgIndex);
    if (imgIndex = 0) then reversing := false;
  end else
  begin
    inc(imgIndex);
    if (imgIndex = masterImageList.Count -1) then
    begin
      reversing := true;
      //Windows.Beep(850,120); //synchronous, so keep it short!
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TForm1.Panel1DblClick(Sender: TObject);
begin
  //halve speed of bouncing ball
  timer.Interval := timer.Interval *2;
end;
//------------------------------------------------------------------------------

end.
