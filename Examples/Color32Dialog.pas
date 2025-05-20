unit Color32Dialog;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  4.8                                                             *
* Date      :  3 Febuary 2025                                                  *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2025                                         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Windows, SysUtils, Classes, Forms, Graphics, ComCtrls,
  Controls, StdCtrls, ExtCtrls, Img32, Img32.Panels;

type
  TColor32DialogForm = class(TForm)
    eHexColor: TEdit;
    Label1: TLabel;
    tbRed: TTrackBar;
    eRed: TEdit;
    tbGreen: TTrackBar;
    eGreen: TEdit;
    Label2: TLabel;
    tbBlue: TTrackBar;
    eBlue: TEdit;
    Label3: TLabel;
    tbAlpha: TTrackBar;
    eAlpha: TEdit;
    Label4: TLabel;
    Bok: TButton;
    bCancel: TButton;
    Label5: TLabel;
    tbMain: TTrackBar;
    lblHue: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure tbMainChange(Sender: TObject);
    procedure tbBlueChange(Sender: TObject);
    procedure eGreenChange(Sender: TObject);
    procedure ipSubMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ipMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ipSubMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ipSubMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure eHexColorChange(Sender: TObject);
    procedure ipMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ipMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fBlockUpd : Boolean;
    fMouseDn  : Boolean;
    fSize     : integer;
    fHsl      : THsl;
    fARGB     : TARGB;
    fBytes    : TArrayOfByte;
    fRainbow  : TArrayOfColor32;
    ipMain    : TImage32Panel;
    ipSubMain : TImage32Panel;
    ipColor   : TImage32Panel;
    procedure SetColor(color: TColor32);
    function  GetColor: TColor32;
    procedure UpdateCtrls(Sender: TObject);
  public
    property Color: TColor32 read GetColor write SetColor;
  end;

implementation

uses Math, StrUtils, Img32.Vector, Img32.Extra, Img32.Draw;

{$R *.dfm}

//------------------------------------------------------------------------------

procedure TColor32DialogForm.FormCreate(Sender: TObject);
var
  i, w, h: integer;
begin

  // TTrackBar controls aren't being read correctly
  // from DFM streams in all versions of Delphi, so ...
  for i := 0 to ComponentCount -1 do
    if Components[i] is TTrackBar then
      with TTrackBar(Components[i]) do
      begin
        Height := DPIAware(34);
        ThumbLength := DPIAware(26);
      end;

  // create ImagePanel components dynamically, in case
  // ImagePanels haven't been installed in the IDE
  ipMain := TImage32Panel.Create(self);
  with ipMain do
  begin
    Parent := self;
    Left := 0;
    Top := DpiAware(206);
    Width := DpiAware(200);
    Height := DpiAware(32);
    BorderWidth := DpiAware(2);
    TabStop := False;
    OnMouseDown := ipMainMouseDown;
    OnMouseMove := ipMainMouseMove;
    OnMouseUp := ipMainMouseUp;
  end;

  ipSubMain := TImage32Panel.Create(self);
  with ipSubMain do
  begin
    Parent := self;
    Left := 0;
    Top := 0;
    Width := DpiAware(200);
    Height := DpiAware(200);
    BorderWidth := DpiAware(6);
    TabStop := False;
    OnMouseDown := ipSubMainMouseDown;
    OnMouseMove := ipSubMainMouseMove;
    OnMouseUp := ipSubMainMouseUp;
  end;

  ipColor := TImage32Panel.Create(self);
  with ipColor do
  begin
    Parent := self;
    Left := DpiAware(8);
    Top := bOK.Top;
    Width := DpiAware(80);
    Height := DpiAware(34);
    BorderWidth := 1;
    TabStop := False;
  end;

  RectWidthHeight(ipSubMain.InnerClientRect, w, h);
  fSize := Min(w, h);
  ipSubMain.Image.SetSize(fSize, fSize);
  SetLength(fBytes, fSize);
  for i := 1 to fSize do
    fBytes[i-1] := ClampByte(127 * (fSize - i) / fSize);

  fHsl.hue := 0;
  fHsl.sat := 255;
  fHsl.lum := 127;
  fHsl.alpha := 255;
  fARGB.Color := clRed32;

  RectWidthHeight(ipMain.InnerClientRect, w, h);
  tbMain.Max := w;
  SetLength(fRainbow, w);
  for i := 0 to w -1 do
  begin
    fHsl.hue := Round(255 * i / w);
    fRainbow[i] := HslToRgb(fHsl);
  end;
  fHsl.hue := 0;
  eHexColor.Font.Style := [fsBold];
  lblHue.Font.Color := clGray;
  lblHue.Left := ipMain.Left + ipMain.Width - lblHue.Width;
  tbMain.Width := lblHue.Left - tbMain.Left;

  UpdateCtrls(nil);
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.UpdateCtrls(Sender: TObject);
var
  i, j : integer;
  w, h : integer;
  r   : integer;
  hsl : THsl;
  pc  : PColor32;
  pt  : TPointD;
  p   : TPathD;
  dpi2: integer;
begin
  if fBlockUpd then Exit;
  try
    fBlockUpd := true;
    dpi2 := DPIAware(2);

    lblHue.Caption := inttostr(fHsl.hue);

    // update ipMain
    RectWidthHeight(ipMain.InnerClientRect, w, h);
    ipMain.Image.AssignPixelArray(fRainbow, w, 1, true);

    if Sender <> tbMain then
      tbMain.Position := Round(tbMain.Max * fHsl.hue / 255);

    j := tbMain.Position;
    for i := Max(0, j - dpi2) to Min(w - 1, j + dpi2) do
      ipMain.Image.Pixels[i] := clBlack32;
    ipMain.Image.Resize(w, h); //stretch image vertically

    // draw ipSubMain
    pc := ipSubMain.Image.PixelBase;
    hsl := fHsl;
    for i := 0 to fSize -1 do
    begin
      for j := 0 to fSize -1 do
      begin
        hsl.lum := 127 - fBytes[j] + fBytes[i];
        hsl.sat := fBytes[i] + fBytes[j];
        pc^ := HslToRGB(hsl);
        inc(pc);
      end;
    end;
    r := DPIAware(5);
    pt.X := fSize - fSize * fHsl.sat / 255;
    pt.Y := pt.X;
    if fHsl.lum > 127 then
    begin
      pt.X := pt.X + (fSize - pt.X) * (fHsl.lum - 128) / 127;
      pt.Y := pt.Y - pt.Y * (fHsl.lum - 128) / 127
    end else
    begin
      pt.X := pt.X - pt.X * (127 - fHsl.lum) / 127;
      pt.Y := pt.Y + (fSize - pt.Y) * (127 - fHsl.lum) / 127;
    end;

    p := Ellipse(RectD(pt.X - r, pt.Y - r, pt.X + r, pt.Y + r));

    if (pt.X * 255 / fSize) - (pt.Y * 255 / fSize) <= 0.0 then
      DrawLine(ipSubMain.Image, p, dpi2, clWhite32, esPolygon) else
      DrawLine(ipSubMain.Image, p, dpi2, clBlack32, esPolygon);

    eHexColor.Text := inttohex(fArgb.Color, 8);
    ipColor.Image.Clear(fArgb.Color);

    if fArgb.A < 200 then
    begin
      HatchBackground(ipColor.Image);
      HatchBackground(ipSubMain.Image);
    end;

    tbRed.Position := fArgb.R;
    tbGreen.Position := fArgb.G;
    tbBlue.Position := fArgb.B;
    tbAlpha.Position := fArgb.A;

    eRed.Text := inttostr(fArgb.R);
    eGreen.Text := inttostr(fArgb.G);
    eBlue.Text := inttostr(fArgb.B);
    eAlpha.Text := inttostr(fArgb.A);

    ipSubMain.Invalidate;
  finally
    fBlockUpd := false;
  end;
end;
//------------------------------------------------------------------------------

function TColor32DialogForm.GetColor: TColor32;
begin
  Result := fARGB.Color;
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.SetColor(color: TColor32);
begin
  if color = fARGB.Color then Exit;
  fARGB.Color := color;
  fHsl := RgbToHsl(color);
  UpdateCtrls(nil);
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.eHexColorChange(Sender: TObject);
var
  val: int64;
begin
  if fBlockUpd then Exit;
  val := StrToInt64Def('$' + eHexColor.Text, -1);
  if (val < 0) or (Length(eHexColor.Text) <> 8) then Exit;
  SetColor(TColor32(val));
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.tbBlueChange(Sender: TObject);
var
  c: TARGB;
begin
  if fBlockUpd then Exit;
  if Sender = tbRed then eRed.Text := IntToStr(tbRed.Position)
  else if Sender = tbGreen then eGreen.Text := IntToStr(tbGreen.Position)
  else if Sender = tbBlue then eBlue.Text := IntToStr(tbBlue.Position)
  else if Sender = tbAlpha then eAlpha.Text := IntToStr(tbAlpha.Position)
  else Exit;
  c.R := tbRed.Position;
  c.G := tbGreen.Position;
  c.B := tbBlue.Position;
  c.A := tbAlpha.Position;
  SetColor(c.Color);
end;
//------------------------------------------------------------------------------

function IsValidByteText(const text: string; out val: integer): Boolean;
begin
  val := StrToIntDef(text, -1);
  Result := (val >= 0) and (val < 256);
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.eGreenChange(Sender: TObject);
var
  val: integer;
  c: TARGB;
begin
  if fBlockUpd or not IsValidByteText(TEdit(Sender).Text, val) then Exit;

  if Sender = eRed then tbRed.Position := val
  else if Sender = eGreen then tbGreen.Position := val
  else if Sender = eBlue then tbBlue.Position := val
  else tbAlpha.Position := val;

  c.R := tbRed.Position;
  c.G := tbGreen.Position;
  c.B := tbBlue.Position;
  c.A := tbAlpha.Position;
  SetColor(c.Color);
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.ipMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  fMouseDn := true;
  pt := ipMain.ClientToImage(Point(X, Y));
  tbMain.Position := pt.X;
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.ipMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if not fMouseDn then Exit;
  pt := ipSubMain.ClientToImage(Point(X, Y));
  tbMain.Position := pt.X;
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.ipMainMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDn := false;
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.ipSubMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  fMouseDn := true;
  pt := ipSubMain.ClientToImage(Point(X, Y));
  ClampRange(pt.X, 0, fSize); ClampRange(pt.Y, 0, fSize);
  fHsl.sat := ClampByte(127 * (fSize - pt.X) / fSize + 127 * (fSize - pt.Y) / fSize);
  fHsl.lum := ClampByte(127 * pt.X / fSize + 127 * (fSize - pt.Y) / fSize);
  fARGB.Color := HslToRgb(fHsl);
  UpdateCtrls(nil);
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.ipSubMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  if not fMouseDn then Exit;
  pt := ipSubMain.ClientToImage(Point(X, Y));
  ClampRange(pt.X, 0, fSize); ClampRange(pt.Y, 0, fSize);
  fHsl.sat := ClampByte(127 * (fSize-pt.X) / fSize + 127 * (fSize - pt.Y) / fSize);
  fHsl.lum := ClampByte(127 * pt.X / fSize + 127 * (fSize - pt.Y) / fSize);
  fARGB.Color := HslToRgb(fHsl);
  UpdateCtrls(nil);
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.ipSubMainMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDn := false;
end;
//------------------------------------------------------------------------------

procedure TColor32DialogForm.tbMainChange(Sender: TObject);
begin
  if fBlockUpd then Exit;
  fHsl.hue := ClampByte( 255 * tbMain.Position / tbMain.Max);
  fARGB.Color := HslToRgb(fHsl);
  UpdateCtrls(tbMain);
end;
//------------------------------------------------------------------------------

end.
