unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, ShellApi,
  Image32, Image32_Draw, Image32_PNG, Image32_SVG_Reader,
  Image32_Vector, Image32Panels, Dialogs;

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
  protected
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
  public
    svgReader: TSvgReader;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R image.res}

uses Image32_Transform, Image32_Extra;

procedure TForm1.FormCreate(Sender: TObject);
var
  rs: TResourceStream;
  rec: TRect;
begin
  DragAcceptFiles(Handle, True);

  ImagePanel.ParentBackground := false;
  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));

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
  DragAcceptFiles(Handle, False);
  svgReader.Free;
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
var
  rec: TRect;
begin
  if OpenDialog1.Execute and
    svgReader.LoadFromFile(OpenDialog1.FileName) then
  begin
    rec := ImagePanel.InnerClientRect;
    ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));
    Caption := 'Svg101 - ' + OpenDialog1.FileName;
    ImagePanel.Image.Clear();
    svgReader.DrawImage(ImagePanel.Image, True);
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.WMDropFiles(var Msg: TMessage);
var
  hDrop: THandle;
  filenameLen: Integer;
  filename: string;
begin
  Msg.Result := 0;
  hDrop:= Msg.wParam;
  filenameLen := DragQueryFile(hDrop, 0, nil, 0);
  SetLength(filename, filenameLen);
  DragQueryFile(hDrop, 0, Pointer(filename), filenameLen+1);
  DragFinish(hDrop);

  if not svgReader.LoadFromFile(fileName) then Exit;
  OpenDialog1.InitialDir := ExtractFilePath(fileName);

  with ImagePanel.InnerClientRect do
    ImagePanel.Image.SetSize(Width, Height);
  Caption := 'Svg101 - ' + fileName;
  ImagePanel.Image.Clear;
  svgReader.DrawImage(ImagePanel.Image, True);

end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
