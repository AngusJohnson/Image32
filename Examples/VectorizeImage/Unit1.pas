unit Unit1;

interface

//nb: Add Image32's Library folder to the project's search path
uses
  Windows, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls,
  Image32, BitmapPanels, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
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
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuShowBothClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
  private
    img, displayImg: TImage32;
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


procedure TForm1.FormCreate(Sender: TObject);
begin
  //SETUP THE DISPLAY PANEL
  Panel1.BorderWidth := DPI(16);
  Panel1.BevelInner := bvLowered;
  //set Panel1.Tabstop := true to enable keyboard controls
  Panel1.TabStop := true;
  Panel1.FocusedColor := clGradientInactiveCaption;
  Panel1.BitmapProperties.Scale := 1;

  img := TImage32.Create;
  //img.LoadFromFile('test7.png');
  img.LoadFromResource('sample', 'PNG');
  displayImg := TImage32.Create;

  DisplayImage;

//  ClientWidth := Panel1.InnerMargin*2 + img.Width;
//  ClientHeight := Panel1.InnerMargin *2 + img.Height + StatusBar1.Height;
  Panel1.BitmapProperties.ScaleType := stFit;

end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  img.Free;
  displayImg.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.DisplayImage;
var
  mask: TArrayOfByte;
  paths: TArrayOfArrayOfPointD;
begin
  if mnuShowPolygons.Checked then
    displayImg.SetSize(img.Width, img.Height) else
    displayImg.Assign(img);

  if not mnuShowImage.Checked then
  begin
    mask := CompareMask(img, $FF000000, CompareAlpha, 128);
    //and if you want to see what the mask looks like ...
    //DrawBoolMask(displayImg, mask, clNavy32);
    paths := MaskToPolygons(mask, img.Width);

    //paths := OffsetPath(paths, -4, -4);
    //DrawPoint(displayImg, paths, 1, clRed32);
    DrawPolygon(displayImg, paths, frEvenOdd, clMaroon32);
  end;

  {$IFDEF SETSIZE}
  Panel1.Bitmap.SetSize(displayImg.Width, displayImg.Height);
  {$ELSE}
  Panel1.Bitmap.Width := displayImg.Width;
  Panel1.Bitmap.Height := displayImg.Height;
  {$ENDIF}

  Panel1.ClearBitmap(pf32bit);
  displayImg.CopyToDc(Panel1.Bitmap.Canvas.Handle);
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  img.LoadFromFile(OpenDialog1.FileName);
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.mnuShowBothClick(Sender: TObject);
begin
  DisplayImage;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
