unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Math, Types, Menus, ExtCtrls, ComCtrls, ShellApi,
  Img32, Img32.Draw, Img32.Fmt.SVG, Img32.Fmt.PNG, Img32.SVG.Reader,
  Img32.Vector, Img32.Text, Img32.Panels, Dialogs, StdCtrls;

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
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure ListSVGFilesInFolder;
    procedure ListBox1Click(Sender: TObject);
    procedure ImagePanelResize(Sender: TObject);
  protected
    folder: string;
    filename: string;
    procedure OpenFile(const filename: string);
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
  public
    svgReader: TSvgReader;
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
begin
  DragAcceptFiles(Handle, True);

  ImagePanel.ParentBackground := false;
  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));

  svgReader := TSvgReader.Create;
  //svgReader.BackgroundColor := clWhite32;

  FontManager.Load('Arial');
  FontManager.Load('Arial Bold');
  FontManager.Load('Arial Italic');
  FontManager.Load('Arial Bold Italic');
  FontManager.Load('Times New Roman');
  FontManager.Load('Times New Roman Bold');
  FontManager.Load('Times New Roman Italic');
  FontManager.Load('Times New Roman Bold Italic');

  OpenFile('.\Sample SVGs\*.svg');
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
  svgReader.Free;
end;
//------------------------------------------------------------------------------

function AppendSlash(const path: string): string;
var
  len: integer;
begin
  Result := path;
  len := Length(Result);
  if (len > 0) and (Result[len] <> '\') then
    Result := Result + '\';

end;
//------------------------------------------------------------------------------

procedure TForm1.ListSVGFilesInFolder;
var
  sr: TSearchRec;
  i, idx, searchResult: integer;
begin
  ListBox1.Items.Clear;
  searchResult := FindFirst(AppendSlash(folder) + '*.svg', faAnyFile, sr);
  idx := -1;
  while searchResult = 0 do
  begin
    if sr.Name[1] = '.' then Continue;
    i := ListBox1.Items.Add(sr.Name);
    if sr.Name = filename then idx := i;
    searchResult := FindNext(sr);
  end;
  ListBox1.ItemIndex := idx;
end;
//------------------------------------------------------------------------------

procedure TForm1.OpenFile(const filename: string);
begin
  self.filename := ExtractFileName(filename);
  folder := ExtractFilePath(filename);
  OpenDialog1.InitialDir := folder;
  ListSVGFilesInFolder;
  ListBox1.ItemIndex := ListBox1.Items.IndexOf(self.filename);
  if (ListBox1.ItemIndex < 0) and (ListBox1.Items.Count > 0) then
    ListBox1.ItemIndex := 0;
  ListBox1Click(nil);
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenFile(OpenDialog1.FileName);
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
  OpenFile(fileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.ListBox1Click(Sender: TObject);
var
  fn: string;
begin
  ImagePanel.Image.Clear;
  ImagePanel.Refresh;
  ListBox1.Visible := ListBox1.Items.Count > 0;
  if (ListBox1.ItemIndex < 0) then Exit;

  ActiveControl := ListBox1;
  filename := ListBox1.Items[ListBox1.ItemIndex];
  fn := AppendSlash(folder) + filename;

  if not svgReader.LoadFromFile(fn) then Exit;
  Screen.Cursor := crHourGlass;
  try
    with ImagePanel.InnerClientRect do
      ImagePanel.Image.SetSize(Width, Height);
    svgReader.DrawImage(ImagePanel.Image, true);
  finally
    Screen.Cursor := crDefault;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.ImagePanelResize(Sender: TObject);
var
  rec: TRect;
begin
  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));
  if Assigned(svgReader) and not svgReader.IsEmpty then
    ListBox1Click(nil);
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
