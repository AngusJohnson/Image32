unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Forms, Types, Menus, ExtCtrls, ComCtrls, ShellApi, Dialogs,
  Img32.Panels;

  //This sample app presumes that the TImage32Panel component
  //has been installed into your Delphi compiler's IDE.

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    PopupMenu11: TMenuItem;
    OpeninBrowser1: TMenuItem;
    StatusBar1: TStatusBar;
    ImagePanel: TImage32Panel;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure ListSVGFilesInFolder;
    procedure ListBox1Click(Sender: TObject);
    procedure ImagePanelResize(Sender: TObject);
    procedure PopupMenu11Click(Sender: TObject);
    procedure OpeninBrowser1Click(Sender: TObject);
  protected
    folder: string;
    filename: string;
    redrawPending: Boolean;
    procedure OpenFile(const filename: string);
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure DrawCurrentItem;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Img32, Img32.Fmt.PNG, Img32.Fmt.SVG, Img32.Text;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
var
  rec: TRect;
begin
  DragAcceptFiles(Handle, True);
  Application.OnIdle := AppIdle;

  ImagePanel.ParentBackground := false;
  ImagePanel.Color := clWhite;

  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));

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
end;
//------------------------------------------------------------------------------

procedure TForm1.AppIdle(Sender: TObject; var Done: Boolean);
begin
  Done := true;
  if not redrawPending then Exit;
  redrawPending := false;
  DrawCurrentItem;
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
  redrawPending := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenFile(OpenDialog1.FileName);
end;
//------------------------------------------------------------------------------

procedure TForm1.PopupMenu11Click(Sender: TObject);
var
  fn: string;
begin
  if ListBox1.ItemIndex < 0 then Exit;
  fn := AppendSlash(folder) + ListBox1.Items[ListBox1.ItemIndex];
  ShellExecute(handle, nil, 'notepad.exe', PChar(fn), nil, SW_SHOWNORMAL);
end;
//------------------------------------------------------------------------------

procedure TForm1.OpeninBrowser1Click(Sender: TObject);
var
  fn: string;
begin
  if ListBox1.ItemIndex < 0 then Exit;
  fn := AppendSlash(folder) + ListBox1.Items[ListBox1.ItemIndex];
  ShellExecute(handle, nil, PChar(fn), nil, nil, SW_SHOWNORMAL);
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
begin
  redrawPending := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.DrawCurrentItem;
var
  svgFilename: string;
  rec: TRect;
begin
  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));
  ImagePanel.Refresh;
  ListBox1.Visible := ListBox1.Items.Count > 0;
  if not ListBox1.Visible then Exit;
  ActiveControl := ListBox1;

  filename := ListBox1.Items[ListBox1.ItemIndex];
  svgFilename := AppendSlash(folder) + filename;

  Screen.Cursor := crHourGlass;
  try
    ImagePanel.Image.LoadFromFile(svgFilename);
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
  redrawPending := true;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

end.
