unit main;

{$I Img32.inc}

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF UNICODE}
  ShlWApi,
{$ENDIF}
  ShellApi, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls, Forms, Types, Menus, ExtCtrls, ComCtrls, Dialogs, Math,
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
    ListBox1: TListBox;
    Splitter1: TSplitter;
    SaveAs1: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure ListSVGFilesInFolder;
    procedure ListBox1Click(Sender: TObject);
    procedure ImagePanelResize(Sender: TObject);
    procedure PopupMenu11Click(Sender: TObject);
    procedure OpeninBrowser1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
  protected
    folder: string;
    filename: string;
    ImagePanel: TImage32Panel;
    procedure OpenFile(const filename: string);
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure DrawCurrentItem;
  end;

var
  Form1: TForm1;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

uses
  Img32, Img32.Vector, Img32.Draw,
  Img32.Fmt.PNG, Img32.Fmt.JPG, Img32.Fmt.SVG, Img32.Text;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{$IFDEF FPC}
procedure OpenDocumentWithDefaultTxt(const filename: string);
begin
  ShellExecute(0, PChar ('open'), 'Notepad.exe', PChar (filename), nil, 1);
end;
//------------------------------------------------------------------------------

{$ELSE}

procedure OpenDocument(const filename: string);
begin
  ShellExecute(0, 'open', PChar(filename), nil, nil, SW_SHOWNORMAL);
end;

{$IFDEF UNICODE}
function GetDefaultTextEditor: string;
var
  exeFileBuffer: array[0..1024] of char;
  len: DWORD;
begin
  len := 1024;
  if AssocQueryString(0, ASSOCSTR_EXECUTABLE,
      '.txt', nil, @exeFileBuffer[0], @len) = S_OK then
    SetString(Result, exeFileBuffer, len -1) else
    Result := '';
end;
//------------------------------------------------------------------------------

procedure OpenDocumentWithDefaultTxt(const filename: string);
var
  txtEditor: string;
begin
  txtEditor := GetDefaultTextEditor;
  ShellExecute(0, 'open', PChar(txtEditor), PChar('"'+filename+'"'), nil, SW_SHOWNORMAL);
end;
//------------------------------------------------------------------------------

{$ELSE}

procedure OpenDocumentWithDefaultTxt(const filename: string);
begin
  ShellExecute(0, PChar ('open'), 'Notepad.exe', PChar (filename), nil, 1);
end;
//------------------------------------------------------------------------------

{$ENDIF}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
var
  rec: TRect;
begin
  ImagePanel := TImage32Panel.create(self);
  ImagePanel.parent := self;
  ImagePanel.Align := alClient;
  ImagePanel.OnResize:= ImagePanelResize;
  ImagePanel.BkgType := pbtChessBoard;
  {$IFNDEF FPC}
  DragAcceptFiles(Handle, True);
  {$ENDIF}
  ImagePanel.ParentBackground := false;
  ImagePanel.Color := clBtnFace;
  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));

//  FontManager.Load('Segoe UI');
  FontManager.Load('Arial Bold');
  FontManager.Load('Arial Italic');
  FontManager.Load('Arial Bold Italic');
  FontManager.Load('Times New Roman');
  FontManager.Load('Times New Roman Bold');
  FontManager.Load('Times New Roman Italic');
  FontManager.Load('Times New Roman Bold Italic');

  FontManager.Load('Symbol');
  FontManager.Load('Webdings');
  FontManager.Load('Wingdings');
  FontManager.Load('Segoe UI Symbol');

  OpenFile('.\Sample SVGs 1\textpath2.svg');
  //OpenFile('.\*.svg');
  ListSVGFilesInFolder;
  DrawCurrentItem;
end;
//------------------------------------------------------------------------------

procedure TForm1.FormDestroy(Sender: TObject);
begin
  {$IFnDEF FPC}
  DragAcceptFiles(Handle, False);
  {$ENDIF}
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
  ListBox1.Items.BeginUpdate;
  try
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
    ListBox1.Visible := ListBox1.Items.Count > 0;
    if not ListBox1.Visible then Exit;
    idx := Max(0, idx);
    ListBox1.ItemIndex := idx;
  finally
    ListBox1.Items.EndUpdate;
  end;
end;
//------------------------------------------------------------------------------

procedure TForm1.OpenFile(const filename: string);
var
  i: integer;
begin
  self.filename := ExtractFileName(filename);
  folder := ExtractFilePath(filename);
  if (folder = '') or (folder[1] = '.') then
    folder := ExpandFileName(folder);
  OpenDialog1.InitialDir := folder;
  ListSVGFilesInFolder;
  i := ListBox1.Items.IndexOf(self.filename);
  if i <> ListBox1.ItemIndex then
    ListBox1.ItemIndex := i else
    DrawCurrentItem;
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
  OpenDocumentWithDefaultTxt(fn);
end;
//------------------------------------------------------------------------------

procedure TForm1.OpeninBrowser1Click(Sender: TObject);
var
  fn: string;
begin
  if ListBox1.ItemIndex < 0 then Exit;
  fn := AppendSlash(folder) + ListBox1.Items[ListBox1.ItemIndex];
   OpenDocument(PChar(fn));
end;
//------------------------------------------------------------------------------

procedure TForm1.WMDropFiles(var Msg: TMessage);
var
  hDrop: THandle;
  filenameLen: Integer;
  filename: string;
begin
  {$IFnDEF FPC}
  Msg.Result := 0;
  hDrop:= Msg.wParam;
  filenameLen := DragQueryFile(hDrop, 0, nil, 0);
  SetLength(filename, filenameLen);
  DragQueryFile(hDrop, 0, Pointer(filename), filenameLen+1);
  DragFinish(hDrop);
  OpenFile(fileName);
  {$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TForm1.ListBox1Click(Sender: TObject);
begin
  DrawCurrentItem;
end;
//------------------------------------------------------------------------------

procedure TForm1.DrawCurrentItem;
var
  svgFilenameAndPath: string;
  rec: TRect;
begin
  if ListBox1.ItemIndex < 0 then Exit;

  filename := ListBox1.Items[ListBox1.ItemIndex];
  svgFilenameAndPath := AppendSlash(folder) + filename;
  rec := ImagePanel.InnerClientRect;

  ImagePanel.Image.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));
    ImagePanel.Image.LoadFromFile(svgFilenameAndPath);
  finally
    ImagePanel.Image.EndUpdate;
    Screen.Cursor := crDefault;
  end;
  ActiveControl := ListBox1;
end;
//------------------------------------------------------------------------------

procedure TForm1.ImagePanelResize(Sender: TObject);
var
  rec: TRect;
begin
  rec := ImagePanel.InnerClientRect;
  ImagePanel.Image.SetSize(RectWidth(rec), RectHeight(rec));
  DrawCurrentItem;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    ImagePanel.Image.SaveToFile(SaveDialog1.FileName);
end;
//------------------------------------------------------------------------------

end.
