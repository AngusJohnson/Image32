unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, ShellApi,
  Img32, Img32.Draw, Img32.Fmt.PNG, Img32.SVG.Reader, Img32.SVG.Writer,
  Img32.Vector, Img32.Text, Img32.Panels, Dialogs, StdCtrls;

  //This sample app presumes that the TImage32Panel component
  //has been installed into your Delphi compiler's IDE.

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ImagePanel: TImage32Panel;
    Exit1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImagePanelResize(Sender: TObject);
  protected
  public
    memStream: TMemoryStream;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Img32.Transform, Img32.Extra;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  rec: TRect;
  pw: TSvgPathWriter;
begin

  ImagePanel.ParentBackground := false;
  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));

  memStream := TMemoryStream.Create;

    //create an SVG file ...
    with TSvgWriter.Create do
    try
      //image size <svg>
      Svg.width := 250;
      Svg.height := 300;

      //group <g> several shapes and (prepare to) rotate them
      with Svg.AddChild(TSvgGroupWriter) as TSvgGroupWriter do
      begin
        MatrixTranslate(Matrix, 10,20);
        MatrixRotate(Matrix, PointD(100,130), -angle15);

        //<text> and <tspan> owned by (ie inside) group
        with AddChild(TSvgTextWriter) as TSvgTextWriter do
        begin
          fontInfo.family := ttfSansSerif;
          fontInfo.size := 12;
          position := PointD(30,230);
          AddText('This is ');

          with AddChild(TSvgTSpanWriter) as TSvgTSpanWriter do
          begin
            FillColor := $FF990000;
            offset.cy := 5;
            fontInfo.size := 20;
            fontInfo.weight := 600;
            AddText('BIG');
          end;

          AddText(' text.');
        end;

        //<rect> inside group
        with AddChild(TSvgRectWriter) as TSvgRectWriter do
        begin
          FillColor   := $10FF3300;
          StrokeColor := clMaroon32;
          strokeWidth := 3.0;

          RecWH := RectWH(10,10, 180, 240);
          Radii := SizeD(20,30);
        end;

        //<path> inside group
        pw := AddChild(TSvgPathWriter) as TSvgPathWriter;

        with pw do
        begin
          FillColor   := $10007F7F;
          StrokeColor := clTeal32;
          strokeWidth := 3.0;

          MoveTo(110,80);
          CubicBezierTo(PointD(145,80), PointD(145,30), PointD(110,30));
          LineHTo(70);
          LineVTo(10);
          LIneTo(110,10);
          CubicBezierTo(PointD(160,10), PointD(160,100), PointD(110,100));
          CubicSplineTo(PointD(60,190), PointD(110,190));
          LineTo(150,190);
          LineVTo(170);
          LineHTo(110);
          CubicBezierTo(PointD(75,170), PointD(75,120), PointD(110,120));
          //ClosePath;
          Rotate(PointD(110,110), angle45);
          Skew(angle45, 0);
          Translate(-120, 25);
        end;
      end;

      //save the new SVG
      SaveToStream(memStream);
      //SaveToFile('test.svg');
    finally
      free;
    end;

    FontManager.Load('Arial');
    ImagePanelResize(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  memStream.Free;
end;

procedure TForm1.ImagePanelResize(Sender: TObject);
begin
  if not Assigned(memStream) then Exit;

  with ImagePanel.InnerClientRect do
    ImagePanel.Image.SetSize(width, height);

  with TSvgReader.Create do
  try
    LoadFromStream(memStream);
    DrawImage(ImagePanel.Image, true);
  finally
    Free;
  end;

end;

//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
