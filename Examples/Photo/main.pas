unit main;

interface

uses
  Windows, Messages, Types, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, ComCtrls, ExtCtrls, Math,
  ActiveX, ShlObj, ShellApi, ZipEx, DialogsEx,
  BitmapPanels, Image32, Image32_BMP, Image32_PNG, Image32_JPG, Image32_GIF,
  Image32_Layers, Image32_Draw, Image32_Text, Image32_Vector, IniFiles;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnuOpenFolder: TMenuItem;
    N1: TMenuItem;
    mnuEscape: TMenuItem;
    View1: TMenuItem;
    mnuSlideShow: TMenuItem;
    mnuMediumPreview: TMenuItem;
    mnuSmallPreview: TMenuItem;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    sbMain: TScrollBox;
    OpenDialog1: TOpenDialog;
    pnlLargeOnMainForm: TPanel;
    Help1: TMenuItem;
    About1: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuEditImage: TMenuItem;
    mnuDelete: TMenuItem;
    mnuRename: TMenuItem;
    mnuSlideShowInterval: TMenuItem;
    N3: TMenuItem;
    OpenFolderContainingtheImage1: TMenuItem;
    mnuFullScreen: TMenuItem;
    mnuLargePreview: TMenuItem;
    mnuLargeImage: TMenuItem;
    N4: TMenuItem;
    mnuEdit: TMenuItem;
    mnuTransition: TMenuItem;
    mnuFast: TMenuItem;
    mnuFadeOutThenIn: TMenuItem;
    mnuFadeInWithOut: TMenuItem;
    N2: TMenuItem;
    mnuSimpleFade: TMenuItem;
    N5: TMenuItem;
    mnuEdit2: TMenuItem;
    mnuDelete2: TMenuItem;
    mnuRename2: TMenuItem;
    mnuBackgroundColor: TMenuItem;
    ips1: TMenuItem;
    procedure mnuSlideShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuOpenFolderClick(Sender: TObject);
    procedure mnuEscapeClick(Sender: TObject);
    procedure ThumbnailKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sbMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sbMainMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbMainMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure About1Click(Sender: TObject);
    procedure mnuEditImageClick(Sender: TObject);
    procedure mnuRenameClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuChangeDisplaySizeClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure mnuSlideShowIntervalClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OpenFolderContainingtheImage1Click(Sender: TObject);
    procedure mnuFullScreenClick(Sender: TObject);
    procedure FullScreenMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FullScreenDblClick(Sender: TObject);
    procedure mnuTransitionClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuShowLargeImageInMainformClick(Sender: TObject);
    procedure pnlLargeOnMainFormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuBackgroundColorClick(Sender: TObject);
    procedure UpdateBackColors;
    procedure AppActivate(Sender: TObject);
    procedure ips1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    isLoading: boolean;
    cancelLoading: boolean;
    thumbnailSize: integer;
    currLargeImg32: TImage32;
    currentIdx: integer;
    currentFolder: string;
    currentYPos: integer;
    thumbnails: TImageList32;
    imageNames: TStringList;
    layeredImg: TLayeredImage32;
    slideShowTimer: TTimer;

    deletePrompt: Boolean;
    largeImagePanel: TPanel;
    largeImageAngle: double;

    procedure StartLargeImageView(largePanel: TPanel = nil);
    procedure EndLargeImageView;
    procedure ShowNextLargeImage;
    procedure ShowPrevLargeImage;
    procedure LoadThumbnailsFromCurrentFolder;
    procedure LoadThumbnailsFromImageFilenames;
    procedure AddThumbnail(x,y, thumbSize: integer; img: TImage32);
    procedure UpdateCurrentIdx(newIdx: integer = MaxInt);
    procedure UpdateLargeView(fade: Boolean);
    procedure PanelOnEnter(Sender: TObject);
    procedure ThumbnailDblClick(Sender: TObject);
    procedure LargeViewKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure ThumbnailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ThumbnailMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure VerticalScroll(dy: integer);
    function CountColumns: integer;
    function RepositionThumbnails: TSize;
    procedure SlideShowOnTimer(Sender: TObject);
    procedure SlideShowPauseResume;
    procedure DeleteCheck(Sender: TObject; isChecked: Boolean);
    procedure LoadIniSettings;
    procedure SaveIniSettings;
    procedure MainWinPosFromString(const s: string;
      minWidth: integer = 640; minHeight: integer = 480);
    function MainWinPosToString: string;
    procedure FullScreenKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCACTIVATE(var Msg: TWMNCActivate); message WM_NCACTIVATE;
    procedure PaintTextOnMenubar(const txt: string);
    procedure ClearTextFromMenubar;
    procedure ColorChanging(Sender: TObject;
      const S: string; var IsValid: Boolean);
  public
    { Public declarations }
  end;

var
  MainForm            : TMainForm;
  maxPreviewSize      : integer;
  midPreviewSize      : integer;
  minPreviewSize      : integer;
  slideShowInterval   : cardinal = 5000;
  backColor           : TColor   = clBtnFace;

const
  focusedRectColor    : TColor   = $0000C0; //lighter maroon
  slideShowFadeInt    : cardinal = 75;

{$R *.dfm}

implementation

resourcestring
  rsResumeSlideShow   = 'Resuming Slide Show';
  rsSlideShow         = 'slide show';
  rsPauseSlideShow    = 'Slide Show Paused';
  rsSlideShowInterval = 'Slide Show Interval';
  rsSlideShowSeconds  = 'Enter the slide show interval (secs)';
  rsDeleteFile        = 'Delete File';
  rsCheckDelete       = 'Are you sure you want to delete %s?';
  rsRenameFile        = 'Rename File';
  rsCancelLoad        = 'Image loading cancelled.';
  rsStatusbar1        = '%s  (Dimensions: %1.0n x %1.0n; Size: %1.0n KB)';
  rsStatusbar2        = '%s  (image %1.0n out of %1.0n)';
  rsOpenFolder        = 'Open Folder';
  rsLoadingFolder     = 'Loading folder ... ';
  rsBackColor         = 'Background Color';
  rsBackColor2        = 'Enter background color in hexidecimal (BBGGRR)';

  rsTips =
    'Opening a folder will populate '#$1D40F#$1D421#$1D428#$1D42D#$1D428' with thumbnails of all the '+
    'images  in that folder (*.bmp; *.png; *.jpg and *.gif). Thumbnails have '+
    '3 sizes: Small (100'#$D7'100) ('#$1D405#$1D7D1'); Medium (150'#$D7'150) ('#$1D405#$1D7D2'); and Large '+
    '(200'#$D7'200) ('#$1D405#$1D7D3'). By selecting a specific thumbnail ('#$23CE'), images '+
    'can be viewd individually, either within '+
    #$1D40F#$1D421#$1D428#$1D42D#$1D428'''s client window ('#$1D405#$1D7D5'), or '+
    'filling the entire screen ('#$1D405#$1D7D7').'#10#10+

    'While viewing images individually, typing '#$1D7CF' will display the image in its '+
    'normal unscaled state, '#$1D7D0' will display  the image at twice it''s '+
    'normal size, etc. Likewise typing Shift+'#$1D7CF' will display the image at '+
    '1/10 normal size, Shift+'#$1D7D0' will display the image at 2/10 normal '+
    'size etc. Also while viewing images individually, Ctrl+'#129092' or Ctrl+'+
    #129094' will display either the previous or next image respectively.'#10#10+

    'Images can also be viewed in a Slide Show ('#$1D405#$1D7D6') where '+
    #$1D40F#$1D421#$1D428#$1D42D#$1D428' will cycle through all the images '+
    'in the folder. Via the Edit menu, the speed of the slide show can be '+
    'adjusted, and also the type of transition that''s used between images.'#10#10+

    'In Slide Show there are 4 transition types:'#10+
    '1. Fast - transition is immediate.'#10+
    '2. Simple - the new image is faded in on top of the old image, and '+
    'the old image is removed once the new image is fully opaque. This effect '+
    'looks great when images are all the same size and have no transparency.'#10+
    '3. Concurrent - the new image is faded in while the old image is faded '+
    'out. This is probably the best option when images have different sizes '+
    'or images have transparency.'#10+
    '4. Consecutive - the old image is faded out before the new image is faded '+
    'in. This effect looks great when the background colour is very dark and '+
    'the transition appears to ''blink''.';

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

type
 TVSVersionInfo = packed record
    Length          :WORD;
    wValueLength    :WORD;
    wType           :WORD;
    szKey:array[0..Length('VS_VERSION_INFO'#0)] of WideChar;
    FixedInfo       : TVSFixedFileInfo;
  end;

function GetVersion(showBuild: Boolean = false): string;
var
  rs: TResourceStream;
  vi: TVSVersionInfo;
begin
  result := '';
  rs := TResourceStream.CreateFromID(hInstance, 1, RT_VERSION);
  try
    if rs.Size < sizeof(vi) then Exit;
    rs.read(vi, sizeof(vi));
    if vi.wValueLength <> sizeof(vi.FixedInfo) then exit;
    if showBuild then
      with vi.FixedInfo do
        result := format('%d.%d.%d (build %d)',
          [dwFileVersionMS shr 16, dwFileVersionMS and $FFFF,
          dwFileVersionLS shr 16, dwFileVersionLS and $FFFF])
    else
      with vi.FixedInfo do
        result := format('%d.%d.%d', [dwFileVersionMS shr 16,
          dwFileVersionMS and $FFFF, dwFileVersionLS shr 16]);
  finally
    rs.Free;
  end;
end;
//------------------------------------------------------------------------------

function ShellFileOperation(const fromFileOrFolder,
  toFileOrFolder: string; funcFlag: Integer; flags: WORD = 0): boolean;
var
  shellinfo: TSHFileOpStruct;
begin
  FillChar(shellinfo, sizeof(shellinfo), 0);
  with shellinfo do
  begin
    wnd   := Application.Handle;
    wFunc := funcFlag; //FO_MOVE, FO_COPY, FO_DELETE or FO_RENAME
    pFrom := PChar(fromFileOrFolder +#0); //double null terminator is needed
    if toFileOrFolder <> '' then
      pTo   := PChar(toFileOrFolder +#0); //double null terminator is needed
    fFlags := flags; //eg: FOF_ALLOWUNDO or FOF_NOCONFIRMATION;
  end;
  result := SHFileOperation(shellinfo) = 0;
end;
//------------------------------------------------------------------------------

function GetFolderPath(handle: HWnd; csidl: Integer; flags: DWORD): string;
begin
  SetLength(result, 1024);
  SHGetFolderPath(handle, csidl, 0, flags, PChar(result));
end;
//------------------------------------------------------------------------------

function GetFileSize(const Filename: string): Int64;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := 0;
  Handle := FindFirstFile(PChar(Filename), FindData);
  if Handle = INVALID_HANDLE_VALUE then Exit;
  Windows.FindClose(Handle);
  Result := Int64(FindData.nFileSizeHigh) shl 32 or FindData.nFileSizeLow;
end;
//------------------------------------------------------------------------------

function AppendSlash(const path: string): string;
var
  i: integer;
begin
  i := length(path);
  if (i = 0) or (path[i] = '\') then
    result := path else
    result := path + '\';
end;
//------------------------------------------------------------------------------

function StripSlash(const path: string): string;
var
  len: integer;
begin
  result := path;
  len := length(path);
  if (len = 0) or (path[len] <> '\') then exit;
  setlength(result,len-1);
end;
//------------------------------------------------------------------------------

function BrowseProc(hwnd: HWnd; uMsg: integer;
  lParam, lpData: LPARAM): integer; stdcall;
var
  sfi: TSHFileInfo;
begin
  case uMsg of
    BFFM_INITIALIZED:
      begin
        SendMessage(hwnd, BFFM_SETSTATUSTEXT,0, lpData);
        SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
      end;
    BFFM_SELCHANGED:
      begin
        ShGetFileInfo(PChar(lParam), 0,
          sfi, sizeof(sfi), SHGFI_DISPLAYNAME or SHGFI_PIDL);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, integer(@sfi.szDisplayName));
      end;
  end;
  result := 0;
end;
//------------------------------------------------------------------------------

function GetFolder(OwnerForm: TForm;
  const Caption: string; AllowCreateNew: boolean; var Folder: string): boolean;
var
  displayname: array[0..MAX_PATH] of Char;
  bi: TBrowseInfo;
  pidl: PItemIdList;
begin
  if not assigned(OwnerForm) then
    bi.hWndOwner := 0 else
    bi.hWndOwner := OwnerForm.Handle;
  bi.pIDLRoot := nil;
  bi.pszDisplayName := PChar(@displayname[0]);
  bi.lpszTitle := PChar(Caption);
  bi.ulFlags := BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT;
  if AllowCreateNew then bi.ulFlags := bi.ulFlags or BIF_NEWDIALOGSTYLE;
  bi.lpfn := @BrowseProc;
  if Folder <> '' then Folder := StripSlash(Folder);
  bi.lParam := integer(PChar(Folder));
  bi.iImage := 0;
  pidl := SHBrowseForFolder(bi);
  result := pidl <> nil;
  if result then
  try
    result := SHGetPathFromIDList(pidl,PChar(@displayname[0]));
    Folder := displayname;
  finally
    CoTaskMemFree(pidl);
  end;
end;
//------------------------------------------------------------------------------

function GetWindowPlacement(form: TForm): TWindowPlacement;
begin
  result.length := SizeOf(result);
  Windows.GetWindowPlacement(form.Handle, result);
end;
//------------------------------------------------------------------------------

function GetLuminance(c: TColor): Byte;
begin
  Result := RgbtoHsl(Color32(c)).lum;
end;
//------------------------------------------------------------------------------

function DoesContrastWithBackColor(img: TImage32; bgColor: TColor32): Boolean;
var
  pc: PARGB;
  i,minPxls: integer;
  bgHsl, hsl: THsl;
begin
  //Checks if the image has sufficient contrast with bgColor
  result := true;
  minPxls := 10;
  bgHsl := RgbtoHsl(bgColor);
  pc := PARGB(img.PixelBase);
  for i := 0 to img.Width * img.Height -1 do
  begin
    if pc.A = $FF then
    begin
      hsl := RgbtoHsl(pc.Color);
      if abs(bgHsl.lum - hsl.lum) > 64 then
      begin
        dec(minPxls);
        if minPxls = 0 then Exit;
      end;
    end;
    inc(pc);
  end;
  Result := false;
end;
//------------------------------------------------------------------------------

function GetControlIndex(ctrl: TControl): integer;
begin
  if ctrl is TPanel then
  begin
    result := 0;
    while ctrl.Parent.Controls[result] <> ctrl do inc(Result);
  end else
    result := -1;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  sbMain.Align := alClient;
  pnlLargeOnMainForm.Align := alClient;
  pnlLargeOnMainForm.TabStop := true;
  pnlLargeOnMainForm.Left := 0; pnlLargeOnMainForm.Top := 0;
  pnlLargeOnMainForm.BorderWidth := DPI(18);
  pnlLargeOnMainForm.FocusedColor := backColor;
  pnlLargeOnMainForm.OnKeyDown := LargeViewKeyDown;
  pnlLargeOnMainForm.CopyPasteEnabled := true;
  pnlLargeOnMainForm.Bitmap.PixelFormat := pf24bit;
  largeImagePanel := pnlLargeOnMainForm;

  StatusBar1.Canvas.Font.Assign(font);

  maxPreviewSize := DPI(200);
  midPreviewSize := DPI(150);
  minPreviewSize := DPI(100);
  thumbnailSize := midPreviewSize;
  currentIdx := -1;
  deletePrompt := true;

  thumbnails := TImageList32.Create;
  imageNames := TStringList.Create;
  layeredImg := TLayeredImage32.Create;
  currLargeImg32 := TImage32.Create;

  slideShowTimer := TTimer.Create(nil);
  slideShowTimer.Interval := slideShowInterval;
  slideShowTimer.OnTimer := SlideShowOnTimer;
  slideShowTimer.Enabled := false;

  Application.OnActivate := AppActivate;

  currentFolder := GetFolderPath(handle, CSIDL_MYPICTURES, SHGFP_TYPE_CURRENT);
  LoadIniSettings;
  UpdateBackColors;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  currLargeImg32.Free;
  slideShowTimer.Free;
  imageNames.Free;
  thumbnails.Free;
  layeredImg.Free;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  cancelLoading := true;
  Application.ProcessMessages;
  SaveIniSettings;
end;
//------------------------------------------------------------------------------

procedure TMainForm.AppActivate(Sender: TObject);
begin
  if pnlLargeOnMainForm.Visible then
    pnlLargeOnMainForm.SetFocus
  else if (currentIdx >= 0) and (currentIdx < sbMain.ControlCount) then
    TPanel(sbMain.Controls[currentIdx]).SetFocus
  else if sbMain.CanFocus then
    sbMain.SetFocus;
end;
//------------------------------------------------------------------------------

procedure TMainForm.LoadIniSettings;
var
  fn, formPos: string;
  i: integer;
begin
  fn := ChangeFileExt(paramstr(0), '.ini');
  with TIniFile.Create(fn) do
  try
    formPos := ReadString('Setup', 'WindowPos', '');
    MainWinPosFromString(formPos, 250,250);
    i := ReadInteger('Setup', 'SlideShowIntSecs', 5);
    slideShowInterval := Max(2, Min(300, i)) * 1000;
    i := ReadInteger('Setup', 'SlideShowTransType', 1);
    case i of
      0: mnuFast.Checked := true;
      2: mnuFadeInWithOut.Checked := true;
      3: mnuFadeOutThenIn.Checked := true;
      else mnuSimpleFade.Checked := true;
    end;
    i := ReadInteger('Setup', 'ThumbnailSize', 2);
    case i of
      0:
        begin
          mnuSmallPreview.Checked := true;
          thumbnailSize := minPreviewSize;
        end;
      1:
        begin
          mnuMediumPreview.Checked := true;
          thumbnailSize := midPreviewSize;
        end;
      else
        begin
          mnuLargePreview.Checked := true;
          thumbnailSize := maxPreviewSize;
        end;
    end;
    backColor := StrToIntDef('$'+ReadString('Setup', 'BackColor', ''), -1);
    if backColor < 0 then backColor := Color32(clBtnFace) and $FFFFFF;
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.SaveIniSettings;
var
  val: integer;
begin
  with TIniFile.Create(ChangeFileExt(paramstr(0), '.ini')) do
  try
    WriteString('Setup', 'WindowPos', MainWinPosToString);
    WriteInteger('Setup', 'SlideShowIntSecs', slideShowInterval div 1000);
    if mnuFast.Checked then val := 0
    else if mnuFadeInWithOut.Checked then val := 2
    else if mnuFadeOutThenIn.Checked then val := 3
    else val := 1; //ie mnuSimpleFade.Checked
    WriteInteger('Setup', 'SlideShowTransType', val);
    if mnuSmallPreview.Checked then val := 0
    else if mnuMediumPreview.Checked then val := 1
    else val := 2;
    WriteInteger('Setup', 'ThumbnailSize', val);

    WriteString('Setup', 'BackColor',
      inttohex(TColor32(backColor) and $FFFFFF, 6));
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function TMainForm.MainWinPosToString: string;
var
  wp: TWindowPlacement;
begin
  //format is Left:Top:Width:Height:IsMaximized
  if WindowState = wsMaximized then
  begin
    wp := GetWindowPlacement(self);
    with wp.rcNormalPosition do
      Result := format('%d:%d:%d:%d:1', [left, top, width, height]);
  end else
    Result := format('%d:%d:%d:%d:0',[left, top, width, height]);
end;
//------------------------------------------------------------------------------

procedure TMainForm.MainWinPosFromString(const s: string;
  minWidth, minHeight: integer);

  function GetVal(var charIdx, len: integer): integer;
  var
    startIdx: integer;
    isNeg: Boolean;
  begin
    isNeg := (charIdx <= len) and (s[charIdx] = '-');
    if isNeg then inc(charIdx);
    startIdx := charIdx;
    Result := 0;
    while (charIdx <= len) and CharInSet(s[charIdx], ['0'..'9']) do
    begin
      Result := Result * 10 + Ord(s[charIdx]) - Ord('0');
      inc(charIdx);
    end;
    if charIdx = startIdx then Result := -1
    else if isNeg then Result := -Result;
  end;

var
  i,len, l,t,w,h,ws: integer;
  rec: TRect;
begin
  len := Length(s);
  if len = 0 then Exit;
  i := 1;
  l := GetVal(i, len); inc(i);
  t := GetVal(i, len); inc(i);
  w := GetVal(i, len); inc(i);
  h := GetVal(i, len); inc(i);
  ws := GetVal(i, len);
  rec := Screen.WorkAreaRect;
  if ws < 0 then Exit;

  Position := poDesigned;
  Width := Min(rec.Width, Max(minWidth, w));
  Height := Min(rec.Height, Max(minHeight, h));
  left := Min(rec.Right - Width, Max(0, l));
  top := Min(rec.Bottom - Height, Max(0, t));
  if ws > 0 then WindowState := wsMaximized;
end;
//------------------------------------------------------------------------------

procedure TMainForm.LoadThumbnailsFromCurrentFolder;
var
  i: integer;
  searchRes: integer;
  sr: TSearchRec;
  slashedPath, ext: string;
  fileTime: integer;
begin
  slashedPath := AppendSlash(currentFolder);
  caption := application.Title + ' - ' + currentFolder;
  StatusBar1.Panels[0].Text := ' ' +rsLoadingFolder;
  cancelLoading := false;
  thumbnails.Clear;
  imageNames.Clear;
  //and clear thumbnail panels too
  sbMain.Hide;
  for i := sbMain.ControlCount -1 downto 0 do sbMain.Controls[i].Free;
  sbMain.Show;
  Application.ProcessMessages;

  isLoading := true;
  Screen.Cursor := crHourGlass;
  try
    //get the names and lastwrite times of all registered images
    //(bmp, jpg, png & gif) found in the selected folder
    searchRes := FindFirst(slashedPath + '*.*', faAnyFile, sr);
    while searchRes = 0 do
    begin
      ext := ExtractFileExt(sr.Name);
      if TImage32.IsRegisteredFormat(ext) then
      begin
        filetime := FileTimeToDosTime(sr.FindData.ftLastWriteTime);
        imageNames.AddObject(slashedPath + sr.Name, Pointer(fileTime));
        thumbnails.Add(nil);
      end;
      searchRes := FindNext(sr);
    end;
    FindClose(sr);

    currentIdx := -1;
    /////////////////////////////////
    LoadThumbnailsFromImageFilenames;
    /////////////////////////////////
    if (sbMain.ControlCount > 0) and (currentIdx = -1) then
      TPanel(sbMain.Controls[0]).SetFocus;
    if FileExists(slashedPath + 'photo.bak') then
      DeleteFile(slashedPath + 'photo.bak');
  finally
    Screen.Cursor := crDefault;
    isLoading := false;
    if (sbMain.ControlCount = 0) then StatusBar1.Panels[0].Text := '';
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.LoadThumbnailsFromImageFilenames;
var
  i,j,k,  w,h,t, x,y, cw, thumbSize: integer;
  folder, filename: string;
  img: TImage32;

  zip: TZipFileEx;
  data: ZipEx.TArrayOfByte;
  dosTime: integer;
begin
  if imageNames.Count = 0 then Exit;
  folder := AppendSlash(currentFolder);

  zip := TZipFileEx.Create;
  try
    zip.LoadFromFile(folder + 'photo.bin');

    //Large thumbnail images will be stored in zip files in the same folder
    //as the images as this dramatically speeds up subsequent folder previews.
    //So if we've previewed this folder previously, load thumbnails from the
    //zip file 'photo.bin', adding new images and replacing modified ones.

    currentIdx := -1;
    x := 0; y := 0;
    cw := sbMain.ClientWidth; thumbSize := thumbnailSize;
    for i := 0 to imageNames.Count -1 do
    begin
      Application.ProcessMessages;
      if cancelLoading then
      begin
        if sbMain.ControlCount > 0 then
        begin
          if currentIdx < 0 then currentIdx := 0;
          TPanel(sbMain.Controls[currentIdx]).SetFocus;
        end;
        Exit;
      end
      else if (currentIdx < 0) and (sbMain.ControlCount > 0) then
        TPanel(sbMain.Controls[0]).SetFocus
      else if (i mod 20 = 0) then
        StatusBar1.Panels[0].Text := ' ' +rsLoadingFolder;


      filename := ExtractFilename(imageNames[i]);
      dosTime := LongInt(imageNames.Objects[i]);

      img := TImage32.Create;

      j := zip.GetEntryIndex(filename);
      if j >= 0 then //found a thumbnail in zip with matching name
      begin
        zip.ExtractEntry(j, data);
        zip[j].cfh.CRC32 := 1; //reuse CRC32 to flag as in folder

        //thumbnails are stored in the following format:
        //width (4); height (4); fileage (4); 32bit thumbnail image
        if length(data) > 12 then
        begin
          w := PInteger(@data[0])^;
          h := PInteger(@data[4])^;
          t := PInteger(@data[8])^;
          //load the zipped thumbnail only if height, width & fileage all match
          if (w <= maxPreviewSize) and (h <= maxPreviewSize) and
            ((w = maxPreviewSize) or (h = maxPreviewSize)) and
            (t = dosTime) then
          begin
            img.SetSize(w, h);
            Move(data[12], img.PixelBase^, Length(data) - 12);
          end;
        end;
      end;

      if img.IsEmpty then
      begin
        //no match found ...
        //either no thumbnail was found in the zip or it needs updating, so
        //load the full sized image and scale it to the largest preview size
        try
          img.LoadFromFile(imageNames[i]);
        except
        end;

        if not img.IsEmpty then
        begin
          if img.Width >= img.Height then
            img.Scale(maxPreviewSize/img.Width) else
            img.Scale(maxPreviewSize/img.Height);
        end;
        //get ready to store the image's height, width, file age and
        //thumbnail image into the zip (photo.bin)
        k := img.Width * img.Height * sizeOf(TColor32);
        SetLength(data, k + 12);
        Move(img.Width, data[0], 4);
        Move(img.Height, data[4], 4);
        Move(dosTime, data[8], 4);
        if k > 0 then
          Move(img.PixelBase^, data[12], k);
        j := zip.AddEntry(filename, data,
          dupOverwrite, DosTimeToFileTime(dosTime));
        zip[j].cfh.CRC32 := 1; //reuse CRC32 to flag as in folder
        data := nil;
      end;

      //add the thumbnail image to the 'images' list, and add 'data' to the zip
      thumbnails[i] := img;

      //add the thumbnail image to a new TPanel container
      if x + thumbSize >= cw then
      begin
        x := 0;
        inc(y, thumbSize);
      end;
      AddThumbnail(x, y, thumbSize, img);
      inc(x, thumbSize);
    end;

    //remove files from zip that haven't been flagged as "in folder"
    //nb: cfh.CRC32 will be updated in SaveZipStructure() below
    for j := zip.Count -1 downto 0 do
      if zip[j].cfh.CRC32 <> 1 then zip.DeleteEntry(j);

    zip.SaveToFile(folder + 'photo.bin');
  finally
    zip.Free;
  end;

  isLoading := false;
  if (cw <> sbMain.ClientWidth) or (thumbSize <> thumbnailSize) then
    RepositionThumbnails;

  if (currentIdx >= 0) and (currentIdx < sbMain.ControlCount) then
    PanelOnEnter(sbMain.Controls[currentIdx]);
end;
//------------------------------------------------------------------------------

procedure TMainForm.AddThumbnail(x,y, thumbSize: integer; img: TImage32);
var
  pnl: TPanel;
begin
  pnl := TPanel.Create(self);
  pnl.Visible := false;
  pnl.Color := backColor;
  pnl.ParentBackground := false;
  pnl.Parent := sbMain;
  pnl.OnEnter := PanelOnEnter;
  pnl.OnDblClick := ThumbnailDblClick;
  pnl.OnMouseDown := ThumbnailMouseDown;
  pnl.OnMouseMove := ThumbnailMouseMove;
  pnl.OnKeyDown := ThumbnailKeyDown;
  pnl.BevelOuter := bvNone;
  pnl.BorderWidth := 5;
  pnl.Left := x;
  pnl.Top := y - sbMain.VertScrollBar.Position;
  pnl.Width := thumbSize;
  pnl.Height := thumbSize;
  pnl.Bitmap.PixelFormat := pf32bit;
  pnl.Bitmap.SetSize(img.Width, img.Height);
  pnl.ZoomAndScrollEnabled := false;
  pnl.FocusedColor := focusedRectColor;
  pnl.PopupMenu := PopupMenu1;
  pnl.TabStop := true;
  if not DoesContrastWithBackColor(img, backColor) then
  begin
    if GetLuminance(backColor) < $80 then
      img.SetRGB(clWhite32) else
      img.SetRGB(clBlack32);
  end;
  img.CopyToDc(pnl.Bitmap.Canvas.Handle, 0,0, false);
  pnl.Visible := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuOpenFolderClick(Sender: TObject);
begin
  if not GetFolder(self, rsOpenFolder, false, currentFolder) then
    Exit;
  cancelLoading := true;
  Application.ProcessMessages;
  EndLargeImageView;
  LoadThumbnailsFromCurrentFolder;
end;
//------------------------------------------------------------------------------

function TMainForm.CountColumns: integer;
var
  w: integer;
begin
  //returns the number of columns that'll fit inside the scrollbox container
  result := 1;
  w := thumbnailSize *2;
  while w < sbMain.ClientWidth do
  begin
    inc(result);
    inc(w, thumbnailSize);
  end;
end;
//------------------------------------------------------------------------------

function TMainForm.RepositionThumbnails: TSize;
var
  i,w: integer;
begin
  Result.cx := 0;
  Result.cy := 0;
  if isLoading then Exit;
  // reposition the thumbnail images inside the scrollbox.
  sbMain.VertScrollBar.Position := 0;
  w := sbMain.ClientWidth;
  for i := 0 to sbMain.ControlCount -1 do
  begin
    if Result.cx + sbMain.Controls[i].Width >= w then
    begin
      Result.cx := 0;
      inc(Result.cy, sbMain.Controls[i].Height);
    end;
    sbMain.Controls[i].Left := Result.cx;
    inc(Result.cx, sbMain.Controls[i].Width);
    sbMain.Controls[i].Top := Result.cy;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.StartLargeImageView(largePanel: TPanel);
begin
  if assigned(largePanel) then
    largeImagePanel := largePanel;

  if mnuSlideShow.Checked then
    with largeImagePanel.InnerClientRect do
      largeImagePanel.Bitmap.SetSize(Width, Height);
  largeImagePanel.ScaleType := stFit;
  largeImagePanel.Visible:= true;
  largeImageAngle := 0;

  currLargeImg32.Resize(0,0);
  layeredImg.Clear;
  largeImageAngle := 0;
  UpdateLargeView(false);
end;
//------------------------------------------------------------------------------

procedure TMainForm.EndLargeImageView;
begin
  mnuSlideShow.Checked := false;
  slideShowTimer.Enabled := false;
  ClearTextFromMenubar;
  pnlLargeOnMainForm.Visible := false;
  largeImagePanel.Visible := false;
  largeImagePanel := pnlLargeOnMainForm;
  layeredImg.Clear;
  currLargeImg32.SetSize(0,0);
  if sbMain.CanFocus and (currentIdx >= 0) then
    TPanel(sbMain.Controls[currentIdx]).SetFocus;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowNextLargeImage;
begin
  inc(currentIdx);
  if currentIdx >= imageNames.Count then currentIdx := 0;
  largeImageAngle := 0;
  currLargeImg32.SetSize(0,0);
  UpdateLargeView(mnuSlideShow.Checked);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowPrevLargeImage;
begin
  dec(currentIdx);
  if currentIdx < 0 then currentIdx := imageNames.Count -1;
  largeImageAngle := 0;
  currLargeImg32.SetSize(0,0);
  UpdateLargeView(mnuSlideShow.Checked);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  rec: TRect;
begin
  RepositionThumbnails;
  StatusBar1.Panels[0].Width := StatusBar1.ClientWidth;
  if pnlLargeOnMainForm.Visible then
  begin
    rec := pnlLargeOnMainForm.InnerClientRect;
    layeredImg.SetSize(rec.Width, rec.Height);
    pnlLargeOnMainForm.Bitmap.Width := rec.Width;
    pnlLargeOnMainForm.Bitmap.Height := rec.Height;
    UpdateLargeView(false);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.SlideShowOnTimer(Sender: TObject);
begin
  if not largeImagePanel.Visible then
  begin
    slideShowTimer.Enabled := false;
    Exit;
  end;

  if (layeredImg.Count > 1) then
  begin
    if (layeredImg.TopLayer is TDesignerLayer32) then
    begin
      //pause/resume message is showing so delete it
      layeredImg.DeleteLayer(layeredImg.TopLayer.Index);
      //now either restart or stop the slide show
      if mnuSlideShow.Checked then
        slideShowTimer.Interval := slideShowInterval div 2 else
        slideShowTimer.Enabled := false;
    end
    else if mnuSimpleFade.Checked then
    begin
      //gently opacify the top layer before deleting the bottom layer
      //slow & smooth transition that looks great, but only with
      //fully opaque images that are the same size.
      layeredImg[1].Opacity := layeredImg[1].Opacity + 15; //17
    end
    else if mnuFadeInWithOut.Checked then
    begin
      //fade in the top layer while fading out the bottom one
      //a seamless transition that looks good with all images
      layeredImg[0].Opacity := layeredImg[0].Opacity - 17; //51
      layeredImg[1].Opacity := layeredImg[1].Opacity + 17; //51
    end else
    begin
      //fades out the bottom layer before fading in the top one
      //a "blink" transition that looks good with all images
      if layeredImg[0].Opacity > 0 then
        layeredImg[0].Opacity := layeredImg[0].Opacity - 51 else //85
        layeredImg[1].Opacity := layeredImg[1].Opacity + 51;     //85
    end;

    if (layeredImg.Count > 1) and (layeredImg[1].Opacity = 255) then
    begin
      layeredImg.DeleteLayer(0);
      slideShowTimer.Enabled := mnuSlideShow.Checked;
      slideShowTimer.Interval := slideShowInterval;
      StatusBar1.Panels[0].Text := ' ' + ExtractFilename(imageNames[currentIdx]);
    end;

    layeredImg.GetMergedImage.CopyToDc(
      largeImagePanel.Bitmap.Canvas.Handle, 0,0, false);
    largeImagePanel.Repaint;
  end else
  begin
    ShowNextLargeImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.SlideShowPauseResume;
var
  w,h: integer;
  layer: TLayer32;
  lf: TLogFont;
  text: string;
  rec: TRect;
  pt: TPointD;
  pp: TArrayOfArrayOfPointD;
begin
  if layeredImg.Count = 0 then Exit;
  //if there's already a message layer then delete it!
  with layeredImg do
    if TopLayer is TDesignerLayer32 then DeleteLayer(TopLayer.Index);

  with largeImagePanel.InnerClientRect do
  begin
    w := Width; h := Height;
  end;
  largeImagePanel.ScaleType := stFit;

  if layeredImg.Count > 1 then
  begin
    //if pausing during a transition then finish it
    layeredImg.DeleteLayer(0);
    layeredImg[0].Opacity := 255;
  end;

  //When in a slide show, layeredImg will be the same size as largeImagePanel's
  //InnerClientRect, and each layer scaled to fit. When not in a slide show,
  //layeredImg will be made the same size as it's 'large' image and this will be
  //scaled by largeImagePanel so layeredImg fits inside InnerClientRect.

  if mnuSlideShow.Checked and
    ((layeredImg.Width <> w) or (layeredImg.Height <> h)) then
  begin
    layeredImg.SetSize(w, h);
    layeredImg[0].Image.Assign(currLargeImg32);
    rec := Types.Rect(0,0, w, h);
    layeredImg[0].Image.ScaleToFit(rec);
    layeredImg[0].PositionCenteredAt(MidPoint(rec));
  end;

  layer := layeredImg.AddLayer(TDesignerLayer32);
  layer.SetSize(w, h);
  lf := DefaultLogfont;
  lf.lfHeight := -DPI(15);
  if mnuSlideShow.Checked then
    text := rsResumeSlideShow else
    text := rsPauseSlideShow;
  pp := GetTextOutline(layer.Width - 14, -lf.lfHeight +10,
    text, GetFontInfo(lf), taRight, pt);
  DrawLine(layer.Image, pp, 6, Color32(backColor), esClosed);
  if GetLuminance(backColor) < $80 then
    DrawPolygon(layer.Image, pp, frEvenOdd, clWhite32) else
    DrawPolygon(layer.Image, pp, frEvenOdd, clBlack32);

  slideShowTimer.Interval := slideShowInterval div 2;
  slideShowTimer.Enabled := true;

  largeImagePanel.Bitmap.SetSize(w,h);
  layeredImg.GetMergedImage.CopyToDc(
    largeImagePanel.Bitmap.Canvas.Handle, 0,0, false);
  largeImagePanel.Repaint;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuTransitionClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := true;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuSlideShowClick(Sender: TObject);
begin
  mnuSlideShow.Checked := not mnuSlideShow.Checked;
  ClearTextFromMenubar;
  slideShowTimer.Enabled := mnuSlideShow.Checked;
  if largeImagePanel.Visible then
    SlideShowPauseResume else
    StartLargeImageView;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuChangeDisplaySizeClick(Sender: TObject);
var
  i: integer;
begin
  if isLoading then begin Beep; Exit; end;

  //either small, medium or large preview, or large image ...
  TMenuItem(Sender).Checked := true;

  if Sender = mnuLargePreview then
    thumbnailSize := maxPreviewSize
  else if Sender = mnuMediumPreview then
    thumbnailSize := midPreviewSize
  else
    thumbnailSize := minPreviewSize;

  for i := 0 to sbMain.ControlCount -1 do
  begin
    sbMain.Controls[i].Width := thumbnailSize;
    sbMain.Controls[i].Height := thumbnailSize;
  end;
  RepositionThumbnails;
  sbMain.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuShowLargeImageInMainformClick(Sender: TObject);
begin
  if (currentIdx < 0) then begin Beep; Exit; end;
  if mnuFullScreen.Checked then
    mnuFullScreenClick(nil) else
    StartLargeImageView(pnlLargeOnMainForm);
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateLargeView(fade: Boolean);
var
  w,h,s: integer;
  layer: TLayer32;
  rec: TRect;
  filename: string;
begin
  if (imageNames.Count = 0) then Exit;
  fade := fade and slideShowTimer.Enabled and not mnuFast.Checked;

  //To manage fading from one image to another, we use a LayeredImage32 object.
  //When a new image (layer) is added, it's placed on top of the current image.
  //Initially the new image is fully transparent, and it's made progressively
  //more opaque until fully opaque at which time the bottom image is removed.

  if fade then
  begin
    if (layeredImg.Count = 2) then layeredImg.DeleteLayer(0);
  end else
  begin
    layeredImg.Clear;
    slideShowTimer.Interval := SlideShowInterval;
  end;

  filename := imageNames[currentIdx];
  if (largeImagePanel = pnlLargeOnMainForm) then largeImagePanel.SetFocus;

  if mnuSlideShow.Checked then
    largeImageAngle := 0 else
    Screen.Cursor := crHourGlass;

  try
    layer := layeredImg.AddLayer;
    if currLargeImg32.IsEmpty then
    begin
      ////////////////////////////////////
      currLargeImg32.LoadFromFile(filename);
      ////////////////////////////////////

      if not DoesContrastWithBackColor(currLargeImg32, backColor) then
      begin
        if GetLuminance(backColor) < $80 then
          currLargeImg32.SetRGB(clWhite32) else
          currLargeImg32.SetRGB(clBlack32);
      end;
      layer.Image.Assign(currLargeImg32);
    end else
      layer.Image.Assign(currLargeImg32);

    if largeImageAngle <> 0 then
      layer.Image.Rotate(largeImageAngle);
    w := layer.Image.Width; h := layer.Image.Height;

    if mnuSlideShow.Checked then
    begin
      with largeImagePanel.InnerClientRect do
        rec := Types.Rect(0,0, Width, Height);
      layer.Image.ScaleToFit(rec);
      layeredImg.SetSize(rec.Width, rec.Height);
      layer.PositionCenteredAt(MidPoint(rec));
      if fade and (layer.Index > 0) then
      begin
        slideShowTimer.Interval := slideShowFadeInt;
        slideShowTimer.Enabled := true;
        layer.Opacity := 0;
      end;
    end else
    begin
      layeredImg.SetSize(w, h);
      rec := Types.Rect(0,0,w,h);
      layer.PositionAt(0,0);
      slideShowTimer.Enabled := false;
    end;

    if fade and (layeredImg.Count > 1) then
    begin
      slideShowTimer.Interval := slideShowFadeInt;
      slideShowTimer.Enabled := true;
      layer.Opacity := 0;
    end;

    largeImagePanel.Bitmap.SetSize(layeredImg.Width, layeredImg.Height);
    layeredImg.GetMergedImage.CopyToDc(
      largeImagePanel.Bitmap.Canvas.Handle, 0,0, false);

    largeImagePanel.ScaleType := stFit;
    largeImagePanel.Invalidate;

    s := GetFileSize(filename);
    StatusBar1.Panels[0].Text := Format(' '+ rsStatusbar1,
      [ExtractFilename(filename), w/1.0, h/1.0, s/1024]);
  finally
    Screen.Cursor := crDefault;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PanelOnEnter(Sender: TObject);
begin
  currentIdx := GetControlIndex(TPanel(Sender));
  if currentIdx < 0 then
    StatusBar1.Panels[0].Text := '' else
    StatusBar1.Panels[0].Text :=
      Format(' '+ rsStatusbar2, [ExtractFilename(imageNames[currentIdx]),
      (currentIdx+1)/1.0, ImageNames.Count/1.0]);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ThumbnailDblClick(Sender: TObject);
begin
  StartLargeImageView(pnlLargeOnMainForm);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuEscapeClick(Sender: TObject);
begin
  //either ...
  if largeImagePanel.Visible then       //1. exit a 'large view'
    EndLargeImageView
  else if isLoading then                //2. cancel folder loading
  begin
    cancelLoading := true;
    StatusBar1.Panels[0].Text := ' '+ rsCancelLoad;
  end
  else                                  //3. close the app.
    Close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.pnlLargeOnMainFormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if x < TPanel(Sender).ClientWidth div 4 then
    ShowPrevLargeImage
  else if x > TPanel(Sender).ClientWidth * 3 div 4 then
    ShowNextLargeImage;
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateCurrentIdx(newIdx: integer);
begin
  if imageNames.Count = 0 then Exit
  else if newIdx = MaxInt then newIdx := currentIdx +1;
  if newIdx < 0 then newIdx := imageNames.Count -1
  else if newIdx >= imageNames.Count then newIdx := 0;
  currentIdx := newIdx;

  if largeImagePanel.Visible then
  begin
    largeImageAngle := 0;
    currLargeImg32.SetSize(0,0);
    UpdateLargeView(true);
  end else
  begin
    TPanel(sbMain.Controls[newIdx]).SetFocus;
  end;

end;
//------------------------------------------------------------------------------

procedure TMainForm.LargeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    Ord('L'), Ord('R'):
      if mnuSlideShow.Checked then inherited
      else
      begin
        layeredImg.Clear;
        if Key = Ord('L') then
          largeImageAngle := largeImageAngle + angle90 else
          largeImageAngle := largeImageAngle - angle90;
        UpdateLargeView(false);
      end;
    VK_LEFT:
      if (Shift = [ssCtrl]) then
        ShowPrevLargeImage else
        inherited;
    VK_RIGHT:
      if (Shift = [ssCtrl]) then
        ShowNextLargeImage else
        inherited;

    else inherited;
  end;
end;
//------------------------------------------------------------------------------

//preview keyboard actions
procedure TMainForm.ThumbnailKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: integer;
begin
  if pnlLargeOnMainForm.Visible then Exit;
  case Key of
    VK_RETURN:
      begin
        StartLargeImageView(pnlLargeOnMainForm);
      end;
    VK_LEFT:
      begin
        i := GetControlIndex(TControl(Sender));
        if (i mod CountColumns > 0) then
          UpdateCurrentIdx(currentIdx -1);
      end;
    VK_RIGHT:
      begin
        i := GetControlIndex(TControl(Sender));
        if (i >= 0) and (i < sbMain.ControlCount -1) and
          ((i +1) mod CountColumns > 0) then
            UpdateCurrentIdx(currentIdx +1);
      end;
    VK_UP:
      begin
        i := GetControlIndex(TControl(Sender)) - CountColumns;
        if i >= 0 then UpdateCurrentIdx(i);
      end;
    VK_DOWN:
      begin
        i := GetControlIndex(TControl(Sender));
        if (i < 0) then Exit;
        i := i + CountColumns;
        if i < sbMain.ControlCount then UpdateCurrentIdx(i);

      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.VerticalScroll(dy: integer);
begin
  with sbMain.VertScrollBar do
    if dy < 0 then
    begin
      if Position > 0 then Position := Max(0, Position + dy);
    end else
    begin
      if Position < Range then Position := Min(Range, Position + dy);
    end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.sbMainMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  currentYPos := Y;
end;
//------------------------------------------------------------------------------

procedure TMainForm.sbMainMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
  if not (ssLeft in Shift) then Exit;
  VerticalScroll(currentYPos - Y);
  currentYPos := Y;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ThumbnailMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  pt := Types.Point(X,Y);
  pt := TPanel(Sender).ClientToScreen(pt);
  pt := ScreenToClient(pt);
  sbMainMouseDown(Sender, Button, Shift, pt.X, pt.Y);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ThumbnailMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  pt := Types.Point(X,Y);
  pt := TPanel(Sender).ClientToScreen(pt);
  pt := ScreenToClient(pt);
  sbMainMouseMove(Sender, Shift, pt.X, pt.Y);
end;
//------------------------------------------------------------------------------

procedure TMainForm.sbMainMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if sbMain.VertScrollBar.Range > 0 then
    VerticalScroll(-WheelDelta div 2);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ips1Click(Sender: TObject);
var
  msgboxOpts: TMessageBoxOptions;
begin
  msgboxOpts.Init;
  msgboxOpts.customIcon := Application.Icon;
  DialogsEx.MessageBox(self, 'Photo Tips', rsTips, 'Photo', MB_OK, msgboxOpts);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuBackgroundColorClick(Sender: TObject);
var
  hexboxOpts: THexBoxOptions;
  hex: string;
begin
  hexboxOpts.Init;
  hexboxOpts.customIcon := Application.Icon;
  hexboxOpts.editChangeCallBk := ColorChanging;
  hexboxOpts.maxLen := 6;
  hex := IntToHex(ColorToRGB(backColor), 6);
  DialogsEx.HexInputBox(self, rsBackColor,
    rsBackColor2, 'Photo', hex, hexboxOpts);
  backColor := StrToInt('$'+hex);
  UpdateBackColors;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ColorChanging(Sender: TObject;
  const S: string; var IsValid: Boolean);
begin
  backColor := StrToInt('$' + s);
  UpdateBackColors;
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateBackColors;
var
  i: integer;
begin
  Self.Color := backColor;
  sbMain.Color := backColor;
  layeredImg.BackgroundColor := Color32(backColor);
  StatusBar1.Invalidate;
  for i := 0 to sbMain.ControlCount -1 do
    TPanel(sbMain.Controls[i]).Color := backColor;
  largeImagePanel.Color := backColor;
  largeImagePanel.FocusedColor := backColor;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuEditClick(Sender: TObject);
begin
  mnuEdit2.Enabled := (currentIdx >= 0) and (currentIdx < imageNames.Count);
  mnuRename2.Enabled := mnuEdit2.Enabled and not isLoading;
  mnuDelete2.Enabled := mnuRename2.Enabled;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  mnuEditClick(nil);
  mnuEdit.Enabled := mnuEdit2.Enabled;
  mnuRename.Enabled := mnuRename2.Enabled;
  mnuDelete.Enabled := mnuDelete2.Enabled;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuEditImageClick(Sender: TObject);
begin
  ShellExecute(0, Nil, PChar(imageNames[currentIdx]), Nil, Nil, SW_NORMAL);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuRenameClick(Sender: TObject);
var
  textboxOpts: TTextBoxOptions;
  oldName, newName: string;
begin
  oldName := imageNames[currentIdx];
  newName := ExtractFileName(imageNames[currentIdx]);
  textboxOpts.Init;
  textboxOpts.customIcon := Application.Icon;
  if not DialogsEx.TextInputBox(self, rsRenameFile, '', 'Photo', newName,
    textboxOpts) then Exit;

  newName := AppendSlash(currentFolder) + newName;
  imageNames[currentIdx] := newName;
  ShellFileOperation(oldName, newName, FO_RENAME, FOF_ALLOWUNDO);
end;
//------------------------------------------------------------------------------

procedure TMainForm.DeleteCheck(Sender: TObject; isChecked: Boolean);
begin
  deletePrompt := not isChecked;
end;
//------------------------------------------------------------------------------

function DeleteZipEntry(const zipFileName, entryName: string): boolean;
var
  idx: integer;
  zip: TZipFileEx;
begin
  Result := FileExists(zipFileName);
  if not Result then Exit;
  zip := TZipFileEx.Create;
  try
    zip.LoadFromFile(zipFileName);
    idx := zip.GetEntryIndex(entryName);
    result := idx >= 0;
    if result then
        zip.DeleteEntry(idx);
  finally
    zip.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuDeleteClick(Sender: TObject);
var
  msgboxOpts: TMessageBoxOptions;
  filename: string;
begin
  filename := imageNames[currentIdx];
  msgboxOpts.Init;
  msgboxOpts.customIcon := Application.Icon;
  msgboxOpts.CheckBoxCallBk := DeleteCheck;
  if deletePrompt and (DialogsEx.MessageBox(self, rsDeleteFile,
    format(rsCheckDelete, [ExtractFilename(filename)]),
    'Photo', MB_YESNO or MB_DEFBUTTON2, msgboxOpts) <> mrYes) then Exit;

  Screen.Cursor := crHourGlass;
  try
    ShellFileOperation(filename, '', FO_DELETE, FOF_ALLOWUNDO);

    DeleteZipEntry(AppendSlash(currentFolder) + 'photo.bin', ExtractFileName(filename));
    sbMain.Controls[currentIdx].Free;
    thumbnails.Delete(currentIdx);
    imageNames.Delete(currentIdx);
    RepositionThumbnails;
    if currentIdx = imageNames.Count then dec(currentIdx);
    if currentIdx < 0 then Exit;

    if largeImagePanel.Visible then
      StartLargeImageView else
      ActiveControl := TForm(sbMain.Controls[currentIdx]);
  finally
    Screen.Cursor := crDefault;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuSlideShowIntervalClick(Sender: TObject);
var
  numBoxOptions: TNumBoxOptions;
  val: integer;
begin
  numBoxOptions.Init;
  numBoxOptions.customIcon := Application.Icon;
  numBoxOptions.minVal := 2;
  numBoxOptions.maxVal := 300;
  val := Round(slideShowInterval / 1000);
  if not NumInputBox(self, rsSlideShowInterval, rsSlideShowSeconds,
    Application.Title, val, numBoxOptions) then Exit;
  slideShowInterval := val * 1000;
  slideShowTimer.Interval := slideShowInterval;
end;
//------------------------------------------------------------------------------

procedure TMainForm.StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  StatusBar.Canvas.Brush.Color := self.Color;
  if GetLuminance(self.Color) < $80 then
    StatusBar1.Canvas.Font.Color := clWhite else
    StatusBar1.Canvas.Font.Color := clBlack;
  StatusBar.Canvas.FillRect(Rect);
  StatusBar.Canvas.TextOut(10, 2, StatusBar1.Panels[0].Text);
end;
//------------------------------------------------------------------------------

procedure TMainForm.OpenFolderContainingtheImage1Click(Sender: TObject);
begin
  ShellExecute(0, Nil, 'explorer.exe',
    PChar('/e, '+ currentFolder), Nil, SW_NORMAL);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FullScreenKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F8, ORD('P'):                                   //toggle slide show
      mnuSlideShowClick(nil);
    VK_F7, VK_F9:
      begin
        TForm(Sender).Tag := 1;                        //return to large view
        TForm(Sender).ModalResult := mrOK;
      end;
    VK_RETURN, VK_ESCAPE:                              //return to thumbnails
      TForm(Sender).ModalResult := mrOK;
    VK_LEFT:
      if (Shift = [ssCtrl]) then
        ShowPrevLargeImage else
        inherited;
    VK_RIGHT:
      if (Shift = [ssCtrl]) then
        ShowNextLargeImage else
        inherited;

    Ord('L'), Ord('R'):
      if not mnuSlideShow.Checked then
      begin
        layeredImg.Clear;
        if Key = Ord('L') then
          largeImageAngle := largeImageAngle + angle90 else
          largeImageAngle := largeImageAngle - angle90;
        UpdateLargeView(false);
      end else
        inherited;

    else inherited;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FullScreenMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if x < TPanel(Sender).ClientWidth div 4 then
    ShowPrevLargeImage
  else if x > TPanel(Sender).ClientWidth * 3 div 4 then
    ShowNextLargeImage;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FullScreenDblClick(Sender: TObject);
begin
  TForm(TControl(Sender).parent).ModalResult := mrOK;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuFullScreenClick(Sender: TObject);
var
  frm: TForm;
  pnl: TPanel;
begin
  if currentIdx < 0 then Exit;
  pnlLargeOnMainForm.Visible := true;

  frm := TForm.Create(nil);
  with frm do
  try
    BorderStyle := bsNone;
    WindowState := wsMaximized;
    KeyPreview  := true;
    OnKeyDown   := FullScreenKeyDown;

    pnl := TPanel.Create(frm);
    pnl.Parent := frm;
    pnl.Align := alClient;
    pnl.Color := backColor;
    pnl.ParentBackground := false;
    pnl.BevelOuter := bvNone;
    pnl.Cursor := crNone;
    pnl.OnMouseUp := FullScreenMouseUp;
    pnl.OnDblClick := FullScreenDblClick;
    StartLargeImageView(pnl);
    ShowModal;
  finally
    Free;
  end;

  //when tag == 1 (ie when toggling F9), close to the windowed large image
  if frm.tag = 1 then
    StartLargeImageView(pnlLargeOnMainForm) else
    EndLargeImageView;
end;
//------------------------------------------------------------------------------

procedure TMainForm.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if mnuSlideShow.Checked then
    PaintTextOnMenubar(rsSlideShow);
end;
//---------------------------------------------------------------------

procedure TMainForm.WMNCACTIVATE(var Msg: TWMNCActivate);
begin
  inherited;
  if mnuSlideShow.Checked then
    PaintTextOnMenubar(rsSlideShow);
end;
//---------------------------------------------------------------------

procedure TMainForm.ClearTextFromMenubar;
begin
  SetWindowPos(handle, 0, 0, 0, 0, 0, SWP_DRAWFRAME or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOZORDER);
end;
//---------------------------------------------------------------------

procedure TMainForm.PaintTextOnMenubar(const txt: string);
var
  MenubarInfo: TMenuBarInfo;
  rec: TRect;
  sz: TSize;
  dc: HDC;
  len: integer;
const
  margin: integer = 20;
begin
  if txt = '' then exit;
  len := length(txt);
  MenubarInfo.cbSize := SizeOf(MenubarInfo);
  GetMenuBarInfo(Handle, Integer(OBJID_MENU), 0, MenubarInfo);
  GetWindowRect(Handle, rec);
  with rec do OffsetRect(MenubarInfo.rcBar, -Left, -Top);
  dc := GetWindowDC(Handle);
  try
    SetBkMode(dc, TRANSPARENT);
    Windows.GetTextExtentPoint(dc, PChar(txt), len, sz);
    with MenubarInfo.rcBar do
      windows.TextOut(dc,
        Right - GetSystemMetrics(SM_CXBORDER) - margin - sz.cx,
        Top + (Bottom - top - sz.cy) div 2, PChar(txt), len);
  finally
    ReleaseDC(Handle, dc);
  end;
end;
//---------------------------------------------------------------------

procedure TMainForm.About1Click(Sender: TObject);
var
  msgboxOpts: TMessageBoxOptions;
begin
  msgboxOpts.Init;
  msgboxOpts.customIcon := Application.Icon;
  DialogsEx.MessageBox(self, 'Photo',
    'Version ' + GetVersion + #10+
    'Author: Angus Johnson'#10+
    'Copyright © 2020',
    'Photo', MB_OK, msgboxOpts);
end;
//------------------------------------------------------------------------------

end.
