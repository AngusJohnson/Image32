unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, ShellApi,
  Image32, Image32_Draw, Image32_PNG, Image32_SVG_Reader, Image32_SVG_Writer,
  Image32_Vector, Image32_Ttf, Image32Panels, Dialogs, StdCtrls;

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
  protected
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Image32_Transform, Image32_Extra;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  rec: TRect;
  grpWrite : TSvgGroupWriter;
  pathWrite: TSvgPathWriter;
  rectWrite: TSvgRectWriter;
  textWrite: TSvgTextWriter;

  memStream: TMemoryStream;
begin

  ImagePanel.ParentBackground := false;
  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));

  memStream := TMemoryStream.Create;
  try

    //create an SVG file ...
    with TSvgWriter.Create do
    try
      //image size <svg>
      Svg.width := 250;
      Svg.height := 300;

      //group <g> several shapes and (prepare to) rotate them
      grpWrite := Svg.AddChild(TSvgGroupWriter) as TSvgGroupWriter;
      MatrixTranslate(grpWrite.Matrix, 10,20);
      MatrixRotate(grpWrite.Matrix, PointD(100,130), -angle15);

      //<text> and <tspan> owned by (ie inside) group
      textWrite := grpWrite.AddChild(TSvgTextWriter) as TSvgTextWriter;
      with textWrite do
      begin
        fontInfo.family := ttfSansSerif;
        fontInfo.size := 12;
        position := PointD(30,230);
        AddText('This is ');

        with AddChild(TSvgTSpanWriter) as TSvgTSpanWriter do
        begin
          FillColor := $FF990000;
          offset.sy := 5;
          fontInfo.size := 20;
          fontInfo.weight := 600;
          AddText('BIG');
        end;

        AddText(' text.');
      end;

      //<rect> inside group
      rectWrite  := grpWrite.AddChild(TSvgRectWriter) as TSvgRectWriter;
      with rectWrite do
      begin
        FillColor   := $10FF3300;
        StrokeColor := clMaroon32;
        strokeWidth := 3.0;

        RecWH := RectWH(10,10, 180, 240);
        Radii := SizeD(20,30);
      end;

      //<path> inside group
      pathWrite := grpWrite.AddChild(TSvgPathWriter) as TSvgPathWriter;
      with pathWrite do
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

      //save the new SVG
      SaveToStream(memStream);
      SaveToFile('test.svg');
      //SaveToFile('c:\temp\test.svg');
    finally
      free;
    end;

    FontLibrary.Add('Arial');

    //finally, draw the svg
    with TSvgReader.Create do
    try
      LoadFromStream(memStream);
      DrawImage(ImagePanel.Image, true);
    finally
      Free;
    end;

  finally
    memStream.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
