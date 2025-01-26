unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls,
  Img32, Img32.Layers;

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
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    timer: TTimer;
    reversing: Boolean;
    imgIndex: integer;
    layeredImage: TLayeredImage32;
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
    drawRec: TRect;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Img32.Draw, Img32.Vector, Img32.Extra;

//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  i,j, ballsize, SpaceAbove, maxSquishSize: integer;
  img: TImage32;
  path: TPathD;
  ballRec: TRect;
begin
  ballsize := DPIAware(100);
  SpaceAbove := ballsize *2;
  drawRec := Rect(0, 0, ballsize, ballsize + SpaceAbove);

  layeredImage := TLayeredImage32.Create;
  layeredImage.SetSize(ballsize, ballsize + SpaceAbove);
  layeredImage.BackgroundColor := Color32(clBtnFace);

  //path: for drawing a black line at the bottom of the image
  path := MakePath([0, ballsize*3-8, ballsize, ballsize*3 -8]);
  with layeredImage.AddLayer(TLayer32) do
  begin
    SetInnerBounds(RectD(drawRec));
    DrawLine(Image, path, 5, clBlack32, esSquare);
  end;

  ballRec := Rect(0,0,ballsize,ballsize);

  Windows.InflateRect(ballRec, -15,-15);
  path := Ellipse(ballRec);

  //31 images (25 + 6) will be added to masterImageList. Each will be viewed
  //twice in each loop except for the top and bottom images. (60 frames/loop)

  img := TImage32.Create(ballsize, ballsize);
  try
    //draw the ball
    DrawPolygon(img, path, frNonZero, clLime32);
    Draw3D(img, path, frNonZero, 8, 8);
    DrawLine(img, path, 3, clGreen32, esPolygon);

    //logarithmic acceleration phase
    for i := 1 to 25 do
    begin
      with layeredImage.AddLayer(TLayer32) do
      begin
        SetInnerBounds(RectD(drawRec));
        j := Round(power(SpaceAbove, i/25));
        ballRec := Rect(0, j, ballsize, j + ballsize);
        Image.CopyBlend(img, img.Bounds, ballRec);
        Visible := false;
      end;
    end;

    //logarithmic deceleration (squishing) phase :)
    maxSquishSize := Round(ballsize * 0.75);
    for i := 1 to 6 do
      with layeredImage.AddLayer(TLayer32) do
      begin
        SetInnerBounds(RectD(drawRec));
        ballRec := Rect(0,
          SpaceAbove +maxSquishSize -Round(power(maxSquishSize, (6-i)/6)),
          ballsize, SpaceAbove + ballsize);
        Image.CopyBlend(img, img.Bounds, ballRec);
        Visible := false;
      end;

    finally
      img.Free;
    end;


  //set up the timer to kick things off
  timer := TTimer.Create(self);
  timer.OnTimer := Timer1Timer;
  //There are 60 images to cycle through here every second so, given that the
  //time to execute the OnTimer event is negligible, I'd expect to set the timer
  //interval to about 17 msec (ie 1000/60), though Delphi's TTimer (a wrapper
  //for Windows' SetTimer / WM_TIMER event) isn't very accurate.
  timer.Interval := 15;
  timer.Enabled := true;

end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  layeredImage.Free;
  timer.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.WMERASEBKGND(var message: TMessage);
begin
  message.Result := 1;
  //this stops windows unhelpfully erasing the form's canvas.
  //We want full control of painting (see FormPaint below).
end;
//------------------------------------------------------------------------------

procedure TForm1.FormResize(Sender: TObject);
begin
  if csDestroying in ComponentState then Exit;

  //repaint the whole background only when resizing.
  Canvas.FillRect(ClientRect);

  //center the animation in the form clientrect
  TranslateRect(drawRec,
    -drawRec.Left + (ClientWidth - layeredImage.Width) div 2,
    -drawRec.Top + (ClientHeight - layeredImage.Height) div 2);

end;
//------------------------------------------------------------------------------

procedure TForm1.FormPaint(Sender: TObject);
begin
  with layeredImage.GetMergedImage do
    CopyToDc(Bounds, Canvas.Handle, drawRec.Left, drawRec.Top, false);
end;
//------------------------------------------------------------------------------

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if imgIndex > 0 then
    layeredImage[imgIndex].Visible := false;
  if reversing then
  begin
    dec(imgIndex);
    if (imgIndex = 1) then reversing := false;
  end else
  begin
    inc(imgIndex);
    if (imgIndex = layeredImage.Count -1) then reversing := true;
  end;
  layeredImage[imgIndex].Visible := true;
  Invalidate;
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
