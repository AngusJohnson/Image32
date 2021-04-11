unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls,
  Image32, Image32_Draw, Image32_PNG, Image32_SVG_Reader,
  Image32_Vector, Image32Panels, Vcl.Dialogs;

  //This sample app presumes that the TImage32Panel component
  //has been installed into your Delphi compiler's IDE.

type

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ImagePanel: TImage32Panel;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
  private
  public
    svgReader: TSvgReader;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R image.res}

procedure TForm1.FormCreate(Sender: TObject);
var
  rs: TResourceStream;
begin

  ImagePanel.ParentBackground := false;
  ImagePanel.Image.SetSize(1200,800);

  svgReader := TSvgReader.Create;

  rs := TResourceStream.Create(hInstance, 'TIGER', 'SVG');
  try
    svgReader.LoadFromStream(rs);
  finally
    rs.Free;
  end;
  svgReader.DrawImage(ImagePanel.Image, true);
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  svgReader.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute and
    svgReader.LoadFromFile(OpenDialog1.FileName) then
  begin
    with ImagePanel.InnerClientRect do
      ImagePanel.Image.SetSize(Width, Height);
    Caption := 'Svg101 - ' + OpenDialog1.FileName;
    ImagePanel.Image.Clear();
    svgReader.DrawImage(ImagePanel.Image, True);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
