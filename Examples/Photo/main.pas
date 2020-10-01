unit main;

interface

uses
  Windows, Messages, Types, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, ComCtrls, ExtCtrls, Math,
  ActiveX, ShlObj, ShellApi, ZipEx, DialogsEx,
  ImagePanels, Image32, Image32_BMP, Image32_PNG, Image32_JPG, Image32_GIF,
  Image32_Layers, Image32_Draw, Image32_Text, Image32_Vector, IniFiles;

type
  TArrayOfArrayOfColor32 = array of TArrayOfColor32;

  TThumbnail = record
    pixels     : TArrayOfColor32;
    width      : integer;
    height     : integer;
    filename   : string;
    fileAge    : longInt;
  end;
  TThumbnails = array of TThumbnail;

  TLoadState = (lsLoaded, lsLoading, lsLoadingRefresh);

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
    OpenDialog1: TOpenDialog;
    Help1: TMenuItem;
    mnuAbout: TMenuItem;
    PopupMenu1: TPopupMenu;
    mnuEdit: TMenuItem;
    mnuDelete: TMenuItem;
    mnuRename: TMenuItem;
    mnuSlideShowInterval: TMenuItem;
    N3: TMenuItem;
    mnuOpenImageFolder: TMenuItem;
    mnuFullScreen: TMenuItem;
    mnuLargePreview: TMenuItem;
    mnuLargeImage: TMenuItem;
    N4: TMenuItem;
    mnuMainEdit: TMenuItem;
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
    mnuTips: TMenuItem;
    tmrLoading: TTimer;
    tmrSlideShow: TTimer;
    procedure mnuSlideShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuOpenFolderClick(Sender: TObject);
    procedure mnuEscapeClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuRenameClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuChangeDisplaySizeClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure mnuSlideShowIntervalClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuOpenImageFolderClick(Sender: TObject);
    procedure mnuFullScreenClick(Sender: TObject);
    procedure FullScreenMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FullScreenDblClick(Sender: TObject);
    procedure mnuTransitionClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuShowLargeImageInMainformClick(Sender: TObject);
    procedure pnlLargeOnMainFormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuMainEditClick(Sender: TObject);
    procedure mnuBackgroundColorClick(Sender: TObject);
    procedure UpdateBackColors;
    procedure AppActivate(Sender: TObject);
    procedure mnuTipsClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure tmrLoadingTimer(Sender: TObject);
    procedure tmrSlideShowTimer(Sender: TObject);
  private
    pnlThumb: TImagePanel;
    pnlLarge: TBitmapPanel;
    largeImg32: TImage32;
    layeredImg: TLayeredImage32;

    slideShowInterval: integer;
    isLoading: boolean;
    cancelLoading: boolean;
    deletePrompt: Boolean;
    thumbnailSize: integer;
    currentIdx: integer;
    currentFolder: string;
    currentLargePanel: TBitmapPanel; //nb: pointer only
    currentLargeAngle: double;
    procedure StartLargeImageView(largePanel: TBitmapPanel = nil);
    procedure EndLargeImageView;
    procedure ShowNextLargeImage;
    procedure ShowPrevLargeImage;
    procedure GetFilenamesFromFolder(const folder: string; names: TStringList);
    procedure LoadThumbnails(const folder: string; names: TStringList);
    procedure ResetThumbnailRows;
    function BuildThumbnailRow(rowIdx, columnCount: integer): Boolean;
    procedure UpdateLargeView(fade: Boolean);
    procedure SetThumbnailFocus(idx: integer);
    procedure ThumbnailKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
    procedure ThumbnailClick(Sender: TObject);
    procedure ThumbnailDblClick(Sender: TObject);
    procedure LargeViewKeyDown(Sender: TObject;
      var Key: Word; Shift: TShiftState);
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
    function GetColumnCount: integer;
    procedure DrawThumbnails(Sender: TObject;
      dstCanvas: TCanvas; const srcRec, dstRec: TRect);
  public
    //an individual thumbnailRow is just the pixels for a 'row' image,
    //that's made from all the thumbnails in that row (duh!).
    thumbnailRows       : TArrayOfArrayOfColor32;
    //individual thumbnailRows are only loaded when they're first needed as
    //this dramatically speeds up the painting of folders with many images
    thumbnailRowsState  : TArrayOfByte;
    //Individual thumbnails are used to build thumbnailRows.
    //These store images using the largest thumbnail resolution.
    thumbnails          : TThumbnails;

    displayWidth        : integer;
    displayHeight       : integer;
    loadState           : TLoadState;

    maxPreviewSize      : integer;
    midPreviewSize      : integer;
    minPreviewSize      : integer;
    backColor           : TColor;
  end;

var
  MainForm            : TMainForm;

const
  focusedRectColor    : TColor   = $0000C0; //lighter maroon
  slideShowFadeInt    : cardinal = 75;
  space               : integer = 5;
  margin              : integer = 20;

{$R *.dfm}

implementation

const
  BUILD_COMPLETE    = 0;
  BUILD_INCOMPLETE  = 1;

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
  rsThumbProps        = '%s  (%1.0n x %1.0n pixels; file size: %1.0n KB)';
  rsOpenFolder        = 'Open Folder';
  rsLoadingCnt        = 'Loading file %1.0n of %1.0n';
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
    'filling the entire screen ('#$1D405#$1D7D6').'#10#10+

    'While viewing images individually, typing '#$1D7CF' will display the image in its '+
    'normal unscaled state, '#$1D7D0' will display  the image at twice it''s '+
    'normal size, etc. Likewise typing Shift+'#$1D7CF' will display the image at '+
    '1/10 normal size, Shift+'#$1D7D0' will display the image at 2/10 normal '+
    'size etc. Also while viewing images individually, Ctrl+'#129092' or Ctrl+'+
    #129094' will display either the previous or next image respectively.'#10#10+

    'Images can also be viewed in a Slide Show ('#$1D405#$1D7D7') where '+
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


function PointToStr(const pt: TPointD): string;
begin
  if (Round(pt.X * 10) mod 10 <> 0) or (Round(pt.Y * 10) mod 10 <> 0) then
    result := format('%1.1f,%1.1f',[pt.X, pt.Y]) else
    result := format('%1.0f,%1.0f',[pt.X, pt.Y]);
end;
//------------------------------------------------------------------------------

function PathToStr(const Path: TPathD): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to high(Path) do
    result := result + PointToStr(path[i])+ #13#10;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  backColor := clBtnFace;
  maxPreviewSize := DPIAware(200);
  midPreviewSize := DPIAware(150);
  minPreviewSize := DPIAware(100);
  thumbnailSize := midPreviewSize;
  currentIdx := -1;
  deletePrompt := true;
  loadState := lsLoaded;

  pnlThumb := TImagePanel.Create(self);
  pnlThumb.Parent := self;
  pnlThumb.Color := clBlack;
  pnlThumb.Align := alClient;
  pnlThumb.OnDrawImage := DrawThumbnails;
  pnlThumb.AllowZoom := false;
  pnlThumb.AutoCenter := false;
  pnlThumb.OnKeyDown := ThumbnailKeyDown;
  pnlThumb.OnClick := ThumbnailClick;
  pnlThumb.OnDblClick := ThumbnailDblClick;
  pnlThumb.PopupMenu := PopupMenu1;

  pnlLarge := TBitmapPanel.Create(self);
  pnlLarge.Parent := self;
  pnlLarge.Visible := false;
  pnlLarge.BevelOuter := bvNone;
  pnlLarge.BevelInner := bvNone;
  pnlLarge.BorderWidth := 0;
  pnlLarge.Align := alClient;
  pnlLarge.PopupMenu := PopupMenu1;
  pnlLarge.OnMouseUp := pnlLargeOnMainFormMouseUp;

  pnlLarge.Left := 0; pnlLarge.Top := 0;
  pnlLarge.FocusedColor := backColor;
  pnlLarge.OnKeyDown := LargeViewKeyDown;
  currentLargePanel := pnlLarge;

  StatusBar1.Canvas.Font.Assign(font);

  layeredImg := TLayeredImage32.Create;
  largeImg32 := TImage32.Create;
  slideShowInterval := tmrSlideShow.Interval;
  Application.OnActivate := AppActivate;

  currentFolder :=
    AppendSlash(GetFolderPath(handle, CSIDL_MYPICTURES, SHGFP_TYPE_CURRENT));
  LoadIniSettings;
  UpdateBackColors;
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  largeImg32.Free;
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
  if pnlLarge.Visible then pnlLarge.SetFocus
  else pnlThumb.SetFocus;
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

procedure TMainForm.GetFilenamesFromFolder(const folder: string; names: TStringList);
var
  searchRes: integer;
  sr: TSearchRec;
  ext: string;
  fileTime: integer;
begin
  isLoading := true;
  Screen.Cursor := crHourGlass;
  try
    //get the names and lastwrite times of all registered images
    //(bmp, jpg, png & gif) found in the selected folder
    searchRes := FindFirst(folder + '*.*', faAnyFile, sr);
    while searchRes = 0 do
    begin
      ext := ExtractFileExt(sr.Name);
      if TImage32.IsRegisteredFormat(ext) then
      begin
        filetime := FileTimeToDosTime(sr.FindData.ftLastWriteTime);
        names.AddObject(sr.Name, Pointer(fileTime));
      end;
      searchRes := FindNext(sr);
    end;
    FindClose(sr);

  finally
    Screen.Cursor := crDefault;
    isLoading := false;
  end;
end;
//------------------------------------------------------------------------------

function TMainForm.GetColumnCount: integer;
var
  thumbSizePlus: Integer;
begin
  thumbSizePlus := thumbnailSize + space;
  result := Max(1, (displayWidth - margin *2) div thumbSizePlus);
end;
//------------------------------------------------------------------------------

procedure TMainForm.LoadThumbnails(const folder: string; names: TStringList);
var
  i, k, count, imageSize: integer;
  img: TImage32;
  zip: TZipFileEx;
  data: ZipEx.TArrayOfByte;
  dosTime: integer;
const
  FILE_FOUND_IN_FOLDER = 1;
begin
  count := names.Count;
  if count = 0 then Exit;
  SetLength(thumbnails, names.Count);

  img := TImage32.Create;
  zip := TZipFileEx.Create;
  try
    zip.LoadFromFile(folder + 'photo.bin');

    //Large thumbnail images will be stored in 'photo.bin', a zip file
    //that's saved to any folder that's opened by this application. Storing
    //thumbnails there dramatically speeds up subsequent folder previews.
    //So if this folder has a 'photo.bin' file , load any thumbnails that still
    //match the folder's file age and file size, and replace any updated files

    for i := 0 to count -1 do
    begin
      Application.ProcessMessages;
      if cancelLoading then Exit;

      if (i mod 10 = 0) then
        StatusBar1.Panels[0].Text := format(rsLoadingCnt, [i/1, count/1]);

      thumbnails[i].pixels := nil;
      thumbnails[i].filename := names[i];
      dosTime := LongInt(names.Objects[i]);

      k := zip.GetEntryIndex(names[i]);
      if k >= 0 then //found a thumbnail in zip with matching name
      begin
        //copy the zip entry into 'data'
        zip.ExtractEntry(k, data);
        //When TZipFileEx loads files, any useful data in the CENTRAL file
        //header (CFH) is copied to the LOCAL file header (LFH). After that,
        //CFH is ignored (and over-written with LFH info when saving).
        //So it's perfectly safe to use CFH for temporary storage here.
        zip[k].cfh.CRC32 := FILE_FOUND_IN_FOLDER;
        //thumbnail 'data' is stored in the following format:
        //width (4); height (4); fileage (4); 32bit thumbnail image
        if length(data) > 12 then
          with thumbnails[i] do
          begin
            width := PInteger(@data[0])^;
            height := PInteger(@data[4])^;
            fileAge := PInteger(@data[8])^;
            //only when height, width & fileage match, load the thumbnail
            if (width <= maxPreviewSize) and (height <= maxPreviewSize) and
              ((Width = maxPreviewSize) or (height = maxPreviewSize)) and
              (fileAge = dosTime) then
            begin
              SetLength(pixels, width * height);
              Move(data[12], pixels[0], Length(data) - 12);
            end;
          end;
          data := nil;
      end;

      if Length(thumbnails[i].pixels) = 0 then
      begin
        //To get here, no match was found in the zip file. Either it didn't
        //exist or it needs updating. So load and scale the image to largest
        //thumbnail size and store that in thumbnails[i].pixels and also
        //update the zip entry with 'data' (ie header info plus pixels).
        try
          img.LoadFromFile(folder + names[i]);
        except
          img.SetSize(0,0);
        end;

        if not img.IsEmpty then
        begin
          if img.Width >= img.Height then
            img.Scale(maxPreviewSize/img.Width) else
            img.Scale(maxPreviewSize/img.Height);
          imageSize := img.Width * img.Height * sizeOf(TColor32);
          //copy the image's height, width, file age and thumbnail
          //image to 'data' ready to save in the zip file
          SetLength(data, imageSize + 12);
          Move(img.PixelBase^, data[12], imageSize);
          //and copy image to thumbnails array too
          SetLength(thumbnails[i].pixels, img.Width * img.Height);
          Move(img.PixelBase^, thumbnails[i].pixels[0], imageSize);
        end else
        begin
          SetLength(data, 12);
        end;
        Move(img.Width, data[0], 4);
        Move(img.Height, data[4], 4);
        Move(dosTime, data[8], 4);
        with thumbnails[i] do
        begin
          width := img.Width;
          height := img.Height;
          fileAge := dosTime;
        end;
        //copy 'data' to a new (or updated) zip entry
        k := zip.AddEntry(thumbnails[i].filename, data,
          dupOverwrite, DosTimeToFileTime(dosTime));
        zip[k].cfh.CRC32 := FILE_FOUND_IN_FOLDER;
        data := nil;
      end;
      loadState := lsLoadingRefresh; //see tmrLoadingTimer event
    end;

    //remove files from zip that haven't been flagged as "in folder"
    for k := zip.Count -1 downto 0 do
      if zip[k].cfh.CRC32 <> FILE_FOUND_IN_FOLDER then
        zip.DeleteEntry(k);

    //finally save the modified zip file
    zip.SaveToFile(folder + 'photo.bin');
  finally
    zip.Free;
    img.Free;
  end;
  pnlThumb.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ResetThumbnailRows;
var
  i, cnt, colCnt, rowCnt, thumbSizePlus: integer;
begin
  pnlThumb.Invalidate;
  currentIdx := -1;
  thumbnailRows := nil; //forces clear
  thumbnailRowsState := nil;
  cnt := length(thumbnails);
  if cnt = 0 then Exit;

  //get thumbnail row and column counts
  thumbSizePlus := thumbnailSize + space;
  colCnt := GetColumnCount;
  rowCnt := cnt div colCnt;
  if cnt mod colCnt > 0 then inc(rowCnt);

  //define the 'ImageSize' for the  virtual image of all thumbnails
  pnlThumb.ImageSize := Size(displayWidth,
    (rowCnt * thumbSizePlus) + margin * 2 - space);

  //initialize thumbnailRows
  cnt := displayWidth * thumbnailSize;
  SetLength(thumbnailRows, rowCnt);
  SetLength(thumbnailRowsState, rowCnt);
  for i := 0 to rowCnt -1 do
  begin
    SetLength(thumbnailRows[i], cnt);
    thumbnailRowsState[i] := BUILD_INCOMPLETE;
  end;
end;
//------------------------------------------------------------------------------

function TMainForm.BuildThumbnailRow(rowIdx, columnCount: integer): Boolean;
var
  i, left, x, cnt: integer;
  startIdx, endIdx: integer;
  thumbSizePlus, rowImageSize: integer;
  tmpThumbImg, tmpRowImg: TImage32;
  rec: TRect;
begin
  Result := true;
  //get thumbnail row and column counts and init thumbnailRows
  cnt := Length(thumbnails);
  thumbSizePlus := thumbnailSize + space;

  //create a temp image to display all resized thumb images in a row
  tmpRowImg := TImage32.Create(displayWidth, thumbnailSize);
  try
    rowImageSize := Length(tmpRowImg.Pixels) * SizeOf(TColor32);

    //create a temp thumb image that'll resize to the right size
    tmpThumbImg := TImage32.Create;
    try
      x := 0;
      //for each thumb image in thumbnails, resize it (if necessary)
      //and copying the result onto the row image
      startIdx := rowIdx * columnCount;
      endIdx := Min(cnt -1, startIdx + columnCount -1);
      for i := startIdx to endIdx do
        with thumbnails[i] do
        begin
          if pixels = nil then
          begin
            Result := false; //ie this thumbnail row must still be loading
            Break;
          end;

          //resize and center the thumbnail using tmpThumbImg
          //and then copy it to tmpRowImg

          left := x * thumbSizePlus + margin;
          rec := types.Rect(left, 0, left + thumbnailSize, thumbnailSize);

          tmpThumbImg.SetSize(width, height);
          Move(pixels[0],
            tmpThumbImg.Pixels[0], Length(pixels) * SizeOf(TColor32));

          if not DoesContrastWithBackColor(tmpThumbImg, backColor) then
            tmpThumbImg.InvertColors;

          tmpThumbImg.ScaleToFitCentered(thumbnailSize, thumbnailSize);
          tmpRowImg.Copy(tmpThumbImg, tmpThumbImg.Bounds, rec);
          inc(x);
        end;

      //finally copy tmpRowImg.pixels to the current thumbnailRow
      if x > 0 then
        Move(tmpRowImg.Pixels[0], thumbnailRows[rowIdx][0], rowImageSize);

      if Result then thumbnailRowsState[rowIdx] := BUILD_COMPLETE;
    finally
      tmpThumbImg.Free;
    end;
  finally
    tmpRowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.DrawThumbnails(Sender: TObject;
  dstCanvas: TCanvas; const srcRec, dstRec: TRect);
var
  i, thumbSizePlus, colCnt: integer;
  dstImg, rowImg: TImage32;
  rec: TRect;
  buildComplete: Boolean;

  function ThumbnailPos(idx: integer): TPoint;
  begin
    Result.X := (idx mod colCnt) * (thumbnailSize + space) +margin;
    Result.Y := (idx div colCnt) * (thumbnailSize + space) +margin -srcRec.Top;
  end;

begin
  thumbSizePlus := thumbnailSize + space;
  colCnt := GetColumnCount;

  buildComplete := true;
  dstCanvas.Brush.Color := pnlThumb.color;
  dstCanvas.FillRect(dstRec);

  //dstImg - image to finally draw onto the destination canvas
  dstImg := TImage32.Create(dstRec.Width, dstRec.Height);
  //rowImg - intermediate image, gets each thumbnailRow and draws it to dstImg
  rowImg := TImage32.Create(displayWidth, thumbnailSize);
  try
    for i := srcRec.Top div thumbSizePlus to High(thumbnailRows) do
    begin
      if (i * thumbSizePlus) - srcRec.Top >= dstRec.Bottom then Break;
      if thumbnailRowsState[i] = BUILD_INCOMPLETE then
        buildComplete := BuildThumbnailRow(i, colCnt);
      Move(thumbnailRows[i][0], rowImg.PixelBase^,
        Length(thumbnailRows[i]) * SizeOf(TColor32));
      rec := rowImg.Bounds;
      Image32_Vector.OffsetRect(rec, 0, (i * thumbSizePlus) - srcRec.Top + margin);
      dstImg.Copy(rowImg, rowImg.Bounds, rec);
      if not buildComplete then Break;
    end;
    if currentIdx >= 0 then
    begin
      rec.TopLeft := ThumbnailPos(currentIdx);
      rec.BottomRight := Point(rec.Left +thumbnailSize, rec.Top +thumbnailSize);
      Image32_Vector.InflateRect(rec, 2, 2);
      DrawLine(dstImg, Rectangle(rec), 4, clRed32, esClosed);
    end;
    //dstImg finally draws to dstCanvas
    dstImg.CopyToDc(dstCanvas.Handle, dstRec.Left, dstRec.Top);
  finally
    dstImg.Free;
    rowImg.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuOpenFolderClick(Sender: TObject);
var
  names: TStringList;
begin
  if loadState <> lsLoaded then
  begin
    cancelLoading := true;
    Exit;
  end;

  if not GetFolder(self, rsOpenFolder, false, currentFolder) then Exit;

  if currentLargePanel.Visible then
    EndLargeImageView;
  caption := application.Title + ' - ' + currentFolder;
  currentFolder := AppendSlash(currentFolder);
  StatusBar1.Panels[0].Text := ' ' +rsLoadingFolder;

  //start current loading
  thumbnails := nil;
  pnlThumb.ResetImage;
  currentIdx := -1;

  cancelLoading := false;
  names := TStringList.Create;
  try
    GetFilenamesFromFolder(currentFolder, names);
    SetLength(thumbnails, names.Count);

    //ResetThumbnailRows must be done after we set the length
    //of thumbnails so we can assign pnlThumb's ImageSize.
    ResetThumbnailRows;

    //when a folder is opened for the first time, the images need to be
    //converted to large thumbnail size both to store in zip format (which
    //dramatically speeds up any future folder viewing) and also to store in
    //the 'thumbnails' array for current viewing. This can take a while,
    //especially when the images are very large. So we don't have to wait
    //for this to finish, tmrLoading will check thumbnailRows every second
    //and refresh pnlThumb when any thumbnailRows have been updated.
    tmrLoading.Enabled := true;
    loadState := lsLoading;
    LoadThumbnails(currentFolder, names);
  finally
    loadState := lsLoaded;
    names.Free;
  end;

  if (Length(thumbnails) > 0) and (currentIdx < 0) then
    SetThumbnailFocus(0);
end;
//------------------------------------------------------------------------------

procedure TMainForm.StartLargeImageView(largePanel: TBitmapPanel);
begin
  if assigned(largePanel) then
    currentLargePanel := largePanel;

  if mnuSlideShow.Checked then
    currentLargePanel.Bitmap.SetSize(displayWidth, displayHeight);
  currentLargePanel.Visible:= true;
  currentLargeAngle := 0;

  largeImg32.Resize(0,0);
  layeredImg.Clear;
  currentLargeAngle := 0;
  UpdateLargeView(false);
end;
//------------------------------------------------------------------------------

procedure TMainForm.EndLargeImageView;
begin
  mnuSlideShow.Checked := false;
  ClearTextFromMenubar;
  pnlLarge.Visible := false;
  currentLargePanel := pnlLarge;
  layeredImg.Clear;
  largeImg32.SetSize(0,0);
  if pnlThumb.CanFocus then pnlThumb.SetFocus;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowNextLargeImage;
begin

  if currentIdx >= High(thumbnails) then
    SetThumbnailFocus(0) else
    SetThumbnailFocus(currentIdx +1);
  currentLargeAngle := 0;
  largeImg32.SetSize(0,0);
  UpdateLargeView(mnuSlideShow.Checked);
end;
//------------------------------------------------------------------------------

procedure TMainForm.ShowPrevLargeImage;
begin
  dec(currentIdx);
  if currentIdx < 0 then currentIdx := High(thumbnails);
  currentLargeAngle := 0;
  largeImg32.SetSize(0,0);
  UpdateLargeView(mnuSlideShow.Checked);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
var
  rec: TRect;
begin
  if (csDestroying in ComponentState) then Exit;

  with pnlThumb.InnerClientRect do
  begin
    displayWidth := Right - Left;
    displayHeight := Bottom - Top;
  end;
  pnlThumb.ImageSize := ImagePanels.Size(displayWidth, displayHeight);

  ResetThumbnailRows;
  StatusBar1.Panels[0].Width := StatusBar1.ClientWidth;
  if pnlLarge.Visible then
  begin
    rec := pnlLarge.InnerClientRect;
    layeredImg.SetSize(rec.Width, rec.Height);
    pnlLarge.Bitmap.Width := rec.Width;
    pnlLarge.Bitmap.Height := rec.Height;
    UpdateLargeView(false);
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.SlideShowPauseResume;
var
  w,h: integer;
  layer: TLayer32;
  lf: TLogFont;
  text: string;
  pt: TPointD;
  pp: TPathsD;
begin
  if layeredImg.Count = 0 then Exit;
  //if there's already a message layer then delete it!
  with layeredImg do
    if TopLayer is TDesignerLayer32 then DeleteLayer(TopLayer.Index);

  with currentLargePanel.InnerClientRect do
  begin
    w := Width; h := Height;
  end;
  currentLargePanel.ScaleToFit;

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
    layeredImg[0].Image.Assign(largeImg32);
    layeredImg[0].Image.ScaleToFit(w, h);
    layeredImg[0].PositionCenteredAt(Point(w div 2, h div 2));
  end;

  layer := layeredImg.AddLayer(TDesignerLayer32);
  layer.SetSize(w, h);
  lf := DefaultLogfont;
  lf.lfHeight := -DPIAware(15);
  if mnuSlideShow.Checked then
    text := rsResumeSlideShow else
    text := rsPauseSlideShow;
  pp := GetTextOutline(layer.Width - 14, -lf.lfHeight +10,
    text, GetFontInfo(lf), taRight, pt);
  //fill behind text ...
  DrawLine(layer.Image, pp, 6, Color32(backColor), esClosed);
  //draw text ...
  if GetLuminance(backColor) < $80 then
    DrawPolygon(layer.Image, pp, frEvenOdd, clWhite32) else
    DrawPolygon(layer.Image, pp, frEvenOdd, clBlack32);

  tmrSlideShow.Interval := slideShowInterval div 3;
  tmrSlideShow.Enabled := true;

  currentLargePanel.Bitmap.SetSize(w,h);
  layeredImg.GetMergedImage.CopyToDc(
    currentLargePanel.Bitmap.Canvas.Handle, 0,0, false);
  currentLargePanel.Repaint;
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
  tmrSlideShow.Enabled := mnuSlideShow.Checked;
  if currentLargePanel.Visible then
    SlideShowPauseResume else
    StartLargeImageView;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuChangeDisplaySizeClick(Sender: TObject);
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

  ResetThumbnailRows;
  pnlThumb.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuShowLargeImageInMainformClick(Sender: TObject);
begin
  if (currentIdx < 0) then begin Beep; Exit; end;
  if mnuFullScreen.Checked then
    mnuFullScreenClick(nil) else
    StartLargeImageView(pnlLarge);
end;
//------------------------------------------------------------------------------

procedure TMainForm.UpdateLargeView(fade: Boolean);
var
  w,h: integer;
  layer: TLayer32;
  rec: TRect;
  filename: string;
begin
  if Length(thumbnails) = 0 then Exit;
  fade := fade and tmrSlideShow.Enabled and not mnuFast.Checked;

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
    tmrSlideShow.Interval := SlideShowInterval;
  end;

  filename := thumbnails[currentIdx].filename;
  if (currentLargePanel = pnlLarge) then currentLargePanel.SetFocus;

  if mnuSlideShow.Checked then
    currentLargeAngle := 0 else
    Screen.Cursor := crHourGlass;

  try
    layer := layeredImg.AddLayer;
    if largeImg32.IsEmpty then
    begin
      ////////////////////////////////////
      largeImg32.LoadFromFile(currentFolder + filename);
      ////////////////////////////////////

      if not DoesContrastWithBackColor(largeImg32, backColor) then
        largeImg32.InvertColors;
      layer.Image.Assign(largeImg32);
    end else
      layer.Image.Assign(largeImg32);

    if currentLargeAngle <> 0 then
      layer.Image.Rotate(currentLargeAngle);

    if mnuSlideShow.Checked then
    begin
      rec := currentLargePanel.InnerClientRect;
      Image32_Vector.OffsetRect(rec, -rec.Left, -rec.Top);
      w := rec.Width; h := rec.Height;
      layer.Image.ScaleToFit(w, h);
      layeredImg.SetSize(w, h);
      layer.PositionCenteredAt(MidPoint(rec));
      if fade and (layer.Index > 0) then
      begin
        tmrSlideShow.Interval := slideShowFadeInt;
        tmrSlideShow.Enabled := true;
        layer.Opacity := 0;
      end;
    end else
    begin
      w := layer.Image.Width;
      h := layer.Image.Height;
      layeredImg.SetSize(w, h);
      rec := Types.Rect(0,0,w, h);
      layer.PositionAt(0,0);
      tmrSlideShow.Enabled := false;
    end;

    if fade and (layeredImg.Count > 1) then
    begin
      tmrSlideShow.Interval := slideShowFadeInt;
      tmrSlideShow.Enabled := true;
      layer.Opacity := 0;
    end;


    currentLargePanel.Bitmap.SetSize(w, h);
    layeredImg.GetMergedImage.CopyToDc(currentLargePanel.Bitmap.Canvas.Handle);
    currentLargePanel.ScaleToFit;
    currentLargePanel.Invalidate;
  finally
    Screen.Cursor := crDefault;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.tmrSlideShowTimer(Sender: TObject);
begin
  if not currentLargePanel.Visible then
  begin
    tmrSlideShow.Enabled := false;
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
        tmrSlideShow.Interval := slideShowInterval div 3 else
        tmrSlideShow.Enabled := false;
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
      tmrSlideShow.Enabled := mnuSlideShow.Checked;
      tmrSlideShow.Interval := slideShowInterval;
    end;

    layeredImg.GetMergedImage.CopyToDc(
      currentLargePanel.Bitmap.Canvas.Handle, 0,0, false);
    currentLargePanel.Repaint;
  end else
  begin
    ShowNextLargeImage;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.tmrLoadingTimer(Sender: TObject);
begin
  case loadState of
    lsLoading         : Exit; //nothing yet to refresh
    lsLoaded          : tmrLoading.Enabled := false;
    lsLoadingRefresh  : loadState := lsLoading;
  end;
  pnlThumb.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuEscapeClick(Sender: TObject);
begin
  //either ...
  if currentLargePanel.Visible then       //1. exit a 'large view'
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

procedure TMainForm.ThumbnailClick(Sender: TObject);
var
  pt: TPoint;
  row,col, colCnt: integer;
begin
  GetCursorPos(pt);
  pt := pnlThumb.ScreenToClient(pt);
  pt := pnlThumb.ClientToImage(pt);
  pt := OffsetPoint(pt, -margin, -margin);
  if (pt.X < 0) or (pt.Y < 0) then Exit;
  colCnt := GetColumnCount;
  col := pt.X div (thumbnailSize + space);
  if col >= colCnt then Exit;
  row := pt.Y div (thumbnailSize + space);
  if row * colCnt + col > High(thumbnails) then Exit;
  currentIdx := row * colCnt + col;
  SetThumbnailFocus(currentIdx);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ThumbnailDblClick(Sender: TObject);
begin
  if currentIdx >= 0 then
    StartLargeImageView(pnlLarge);
end;
//------------------------------------------------------------------------------

procedure TMainForm.SetThumbnailFocus(idx: integer);
var
  fs: Int64;
begin
  if Length(thumbnails) = 0 then
  begin
    currentIdx := -1;
  end else
  begin
    if idx < 0 then currentIdx := 0
    else if idx >= Length(thumbnails) then
      currentIdx := High(thumbnails)
    else
      currentIdx := idx;

    with thumbnails[currentIdx] do
    begin
      fs := GetFileSize(currentFolder + filename);
      StatusBar1.Panels[0].Text := Format(' '+ rsThumbProps,
        [ExtractFilename(filename), width/1.0, height/1.0, fs/1024])
    end;
  end;
  pnlThumb.Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.ThumbnailKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  yPos, colCnt: integer;

  procedure ScrollSelectionIntoView;
  begin
    yPos := (currentIdx div colCnt) * (thumbnailSize + space) + margin;
    if yPos < pnlThumb.Offset.Y then
      pnlThumb.Offset := Point(pnlThumb.Offset.X, yPos - space)
    else if yPos + thumbnailSize + space - pnlThumb.Offset.Y >
      pnlThumb.InnerClientRect.Height then
        pnlThumb.Offset := Point(pnlThumb.Offset.X,
          yPos + thumbnailSize + space - pnlThumb.InnerClientRect.Height);
  end;

begin

  if Length(thumbnails) = 0 then Exit;
  if currentIdx < 0 then
  begin
    currentIdx := 0;
    Invalidate;
    Exit;
  end;

  case Key of
    VK_RETURN:
      begin
        StartLargeImageView(pnlLarge);
        Key := 0;
      end;
    VK_UP:
      begin
        colCnt := GetColumnCount;
        Key := 0;
        SetThumbnailFocus(currentIdx - colCnt);
        ScrollSelectionIntoView;
        Invalidate;
      end;
    VK_LEFT:
      begin
        colCnt := GetColumnCount;
        Key := 0;
        if (currentIdx mod colCnt <> 0) then
        begin
          SetThumbnailFocus(currentIdx - 1);
          ScrollSelectionIntoView;
          Invalidate;
        end;
      end;
    VK_RIGHT:
      begin
        colCnt := GetColumnCount;
        Key := 0;
        if ((currentIdx + 1) mod colCnt <> 0) then
        begin
          SetThumbnailFocus(currentIdx + 1);
          ScrollSelectionIntoView;
          Invalidate;
        end;
      end;
    VK_DOWN:
      begin
        colCnt := GetColumnCount;
        Key := 0;
        SetThumbnailFocus(currentIdx + colCnt);
        ScrollSelectionIntoView;
        Invalidate;
      end;
    else inherited;
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
          currentLargeAngle := currentLargeAngle + angle90 else
          currentLargeAngle := currentLargeAngle - angle90;
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

procedure TMainForm.mnuTipsClick(Sender: TObject);
var
  msgboxOpts: IOptions;
begin
  msgboxOpts := TMessageBoxOptions.Create;
  msgboxOpts.Icon.Assign(Application.Icon);
  DialogsEx.MessageBox(self, 'Photo Tips', rsTips,
    'Photo', MB_OK, msgboxOpts as TMessageBoxOptions);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuBackgroundColorClick(Sender: TObject);
var
  hexboxOpts: IOptions;
  hex: string;
begin
  hexboxOpts := THexBoxOptions.Create;
  hexboxOpts.Icon.Assign(Application.Icon);
  with (hexboxOpts as THexBoxOptions) do
  begin
    editChangeCallBk := ColorChanging;
    maxLen := 6;
  end;
  hex := IntToHex(ColorToRGB(backColor), 6);
  DialogsEx.HexInputBox(self, rsBackColor,
    rsBackColor2, 'Photo', hex, hexboxOpts as THexBoxOptions);
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
begin
  Self.Color := backColor;
  pnlThumb.Color := backColor;
  pnlThumb.Refresh;
  layeredImg.BackgroundColor := Color32(backColor);
  StatusBar1.Invalidate;
  currentLargePanel.Color := backColor;
  currentLargePanel.FocusedColor := backColor;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuMainEditClick(Sender: TObject);
begin
  mnuEdit2.Enabled := (currentIdx >= 0) and (currentIdx < Length(thumbnails));
  mnuRename2.Enabled := mnuEdit2.Enabled and not isLoading;
  mnuDelete2.Enabled := mnuRename2.Enabled;
end;
//------------------------------------------------------------------------------

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  mnuMainEditClick(nil);
  mnuEdit.Enabled := mnuEdit2.Enabled;
  mnuRename.Enabled := mnuRename2.Enabled;
  mnuDelete.Enabled := mnuDelete2.Enabled;
  mnuOpenImageFolder.Enabled := mnuEdit.Enabled;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuEditClick(Sender: TObject);
begin
  if (currentIdx < 0) or (currentIdx > High(thumbnails)) then Exit;
  ShellExecute(0, Nil,
    PChar(currentFolder + thumbnails[currentIdx].filename), Nil, Nil, SW_NORMAL);
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuRenameClick(Sender: TObject);
var
  textboxOpts: IOptions;
  oldName, newName: string;
begin
  oldName := thumbnails[currentIdx].filename;
  newName := oldName;
  textboxOpts := TTextBoxOptions.Create;
  textboxOpts.Icon.Assign(Application.Icon);
  if not DialogsEx.TextInputBox(self, rsRenameFile, '',
    'Photo', newName, textboxOpts as TTextBoxOptions) then Exit;

  thumbnails[currentIdx].filename := newName;
  ShellFileOperation(currentFolder + oldName,
    currentFolder + newName, FO_RENAME, FOF_ALLOWUNDO);
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
  msgboxOpts: IOptions;
  filename: string;
  i, colCnt, rowNum: integer;
begin
  filename := thumbnails[currentIdx].filename;
  msgboxOpts := TMessageBoxOptions.Create;
  msgboxOpts.Icon.Assign(Application.Icon);
  (msgboxOpts as TMessageBoxOptions).CheckBoxCallBk := DeleteCheck;
  if deletePrompt and (DialogsEx.MessageBox(self, rsDeleteFile,
    format(rsCheckDelete, [filename]), 'Photo', MB_YESNO or MB_DEFBUTTON2,
    msgboxOpts as TMessageBoxOptions) <> mrYes) then Exit;

  Screen.Cursor := crHourGlass;
  try
    //delete the file
    ShellFileOperation(currentFolder + filename, '', FO_DELETE, FOF_ALLOWUNDO);
    //delete the zip entry too
    DeleteZipEntry(currentFolder + 'photo.bin', ExtractFileName(filename));
    //delete the thumbnail
    Delete(thumbnails, currentIdx, 1);

    //rebuild the thumbnail display
    colCnt := GetColumnCount;
    rowNum := currentIdx div colCnt;
    for i := rowNum to High(thumbnailRows) do
      thumbnailRowsState[i] := BUILD_INCOMPLETE;
    pnlThumb.Invalidate;
    if currentIdx = Length(thumbnails) then dec(currentIdx);
    if currentIdx < 0 then Exit;

    if currentLargePanel.Visible then
      StartLargeImageView else
      ActiveControl := pnlThumb;
  finally
    Screen.Cursor := crDefault;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.mnuSlideShowIntervalClick(Sender: TObject);
var
  numBoxOptions: IOptions;
  val: integer;
begin
  numBoxOptions := TNumBoxOptions.Create;
  numBoxOptions.Icon.Assign(Application.Icon);
  with (numBoxOptions as TNumBoxOptions) do
  begin
    minVal := 2;
    maxVal := 300;
  end;
  val := Round(slideShowInterval / 1000);
  if not NumInputBox(self, rsSlideShowInterval, rsSlideShowSeconds,
    Application.Title, val, numBoxOptions as TNumBoxOptions) then Exit;
  slideShowInterval := val * 1000;
  tmrSlideShow.Interval := slideShowInterval;
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

procedure TMainForm.mnuOpenImageFolderClick(Sender: TObject);
begin
  ShellExecute(0, Nil, 'explorer.exe',
    PChar('/e, '+ currentFolder), Nil, SW_NORMAL);
end;
//------------------------------------------------------------------------------

procedure TMainForm.FullScreenKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F9, ORD('P'):                                   //toggle slide show
      mnuSlideShowClick(nil);
    VK_F7, VK_F8:
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
          currentLargeAngle := currentLargeAngle + angle90 else
          currentLargeAngle := currentLargeAngle - angle90;
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
  pnl: TBitmapPanel;
begin
  if currentIdx < 0 then Exit;
  pnlLarge.Visible := true;

  frm := TForm.Create(nil);
  with frm do
  try
    BorderStyle := bsNone;
    WindowState := wsMaximized;
    KeyPreview  := true;
    OnKeyDown   := FullScreenKeyDown;

    pnl := TBitmapPanel.Create(frm);
    pnl.Parent := frm;
    pnl.Align := alClient;
    pnl.Color := backColor;
    pnl.ParentBackground := false;
    pnl.BevelOuter := bvNone;
    pnl.BevelInner := bvNone;
    pnl.BorderWidth := 0;
    pnl.Cursor := crNone;
    pnl.OnMouseUp := FullScreenMouseUp;
    pnl.OnDblClick := FullScreenDblClick;
    StartLargeImageView(pnl);
    ShowModal;

    //when tag == 1 (ie when toggling F8), close to the windowed large image
    if frm.tag = 1 then
      StartLargeImageView(pnlLarge) else
      EndLargeImageView;

  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  //non-client area painting - ie on menubar here
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
  with rec do
    Image32_Vector.OffsetRect(MenubarInfo.rcBar, -Left, -Top);
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

procedure TMainForm.mnuAboutClick(Sender: TObject);
var
  msgboxOpts: IOptions;
begin
  msgboxOpts := TMessageBoxOptions.Create;
  msgboxOpts.Icon.Assign(Application.Icon);
  DialogsEx.MessageBox(self, 'Photo',
    'Version ' + GetVersion + #10+
    'Author: Angus Johnson'#10+
    'Copyright © 2020',
    'Photo', MB_OK, msgboxOpts as TMessageBoxOptions);
end;
//------------------------------------------------------------------------------

end.
