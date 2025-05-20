unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, Img32;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    img: TImage32;
    imgCnt: integer;
    timer: TTimer;
    imgIndex: integer;
  protected
    procedure WMERASEBKGND(var message: TMessage); message WM_ERASEBKGND;
  public
    drawRec: TRect;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$R images.res}

uses
  Img32.Extra, Img32.Vector, Img32.Fmt.PNG;

//------------------------------------------------------------------------------

function GetMinRotationRect(const rec: TRect): TRect;
var
  mp: TPointD;
  radius: double;
begin
  mp.X := rec.Width / 2;
  mp.Y := rec.Height / 2;
  radius := Distance(PointD(rec.Left, rec.Top), mp);
  Result.Left := Ceil(mp.X - radius);
  Result.Right := Ceil(mp.X + radius);
  Result.Top := Ceil(mp.Y - radius);
  Result.Bottom := Ceil(mp.Y + radius);
end;
//------------------------------------------------------------------------------

function EnumResNameProc(hModule: HMODULE; lpszType, lpszName: PChar;
  lParam: LONG_PTR): BOOL; stdcall;
begin
  inc(PInteger(lParam)^);
  Result := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  angle, angleDelta: double;
begin

  img := TImage32.Create;
  img.LoadFromResource('RAZZ_00', 'PNG');
  with GetMinRotationRect(img.Bounds) do
  begin
    ClientWidth := Width;
    ClientHeight := Height;
  end;

  // count images in the PNG resource section ...
  EnumResourceNames(0, 'PNG', @EnumResNameProc, LONG_PTR(@imgCnt));

  // make the form **background** transparent
  Self.TransparentColor := true; // :))
  // and make sure BorderStyle = bsNone and BorderIcons = []

  timer := TTimer.Create(self);
  timer.OnTimer := Timer1Timer;
  timer.Interval := 33;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  img.Free;
  timer.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.WMERASEBKGND(var message: TMessage);
begin
  //stop windows unhelpfully erasing the form's canvas.
  //We want full control of painting (see FormPaint below).
  message.Result := 1;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormPaint(Sender: TObject);
var
  x, y: integer;
  mpForm: TPointD;
begin
  with ClientRect do
    mpForm := PointD(width / 2, Height / 2);
  img.LoadFromResource(format('RAZZ_%2.2d',[imgIndex]), 'PNG');
  x := Round(mpForm.X - img.MidPoint.X);
  y  := Round(mpForm.Y - img.MidPoint.Y);
  // clear the form canvas just before copying the image onto it
  // nb: Form1.TransparentColor = clBtnFace which is the same as
  // Form1.Canvas.Color ...
  Canvas.FillRect(ClientRect);
  img.CopyToDc(img.Bounds, Canvas.Handle, x, y, false);
end;
//------------------------------------------------------------------------------

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  timer.Enabled := not timer.Enabled;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(key, [#13, #27, #32]) then Close;
end;
//------------------------------------------------------------------------------

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  imgIndex := (imgIndex + 1) mod imgCnt;
  Invalidate;
end;
//------------------------------------------------------------------------------

end.
