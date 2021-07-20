program Image32UpgradeToVer3;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows, SysUtils, ActiveX, ShlObj, ShellApi, Classes, AnsiStrings;


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

function AppendSlash(const path: string): string;
var
  len: integer;
begin
  len := length(path);
  if (len = 0) or (path[len] = '\') then
    result := path else
    result := path+'\';
end;

//------------------------------------------------------------------------------
// GetFolder functions ...
//------------------------------------------------------------------------------

function IdListConstToPath(csidl: integer): string;
var
  path: array[0..MAX_PATH] of Char;
  pid: PItemIDList;
begin
    SHGetFolderLocation(0, csidl, 0, 0, pid);
    try
      SHGetPathFromIDList(pid, path);
      Result := path;
    finally
      CoTaskMemFree(pid);
    end;
end;
//------------------------------------------------------------------------------

function BrowseProc(hwnd: HWnd; uMsg: integer; lParam, lpData: LPARAM): integer; stdcall;
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
        ShGetFileInfo(PChar(lParam), 0, sfi,sizeof(sfi),SHGFI_DISPLAYNAME or SHGFI_PIDL);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT,0, integer(@sfi.szDisplayName));
      end;
  end;
  result := 0;
end;
//------------------------------------------------------------------------------

function GetFolder(const Caption: string; var Folder: string): boolean;
var
  displayname: array[0..MAX_PATH] of Char;
  bi: TBrowseInfo;
  pidl: PItemIdList;
begin
  bi.hWndOwner := 0;
  bi.pIDLRoot := nil;
  bi.pszDisplayName := PChar(@displayname[0]);
  bi.lpszTitle := PChar(Caption);
  bi.ulFlags := BIF_RETURNONLYFSDIRS or BIF_STATUSTEXT;
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
//------------------------------------------------------------------------------

procedure Replace(const filename: string;
  const OldPattern, NewPattern: AnsiString;
  backup: Boolean; Flags: TReplaceFlags);
var
  len: integer;
  ext: string;
  ms: TMemoryStream;
  utf8: AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(filename);
    SetLength(utf8, ms.Size);
    if ms.Size > 0 then
    begin
      Move(ms.Memory^, utf8[1], ms.Size);
      utf8 := ansistrings.StringReplace(utf8, OldPattern, NewPattern, Flags);
      len := Length(utf8);
      ms.SetSize(len);
      Move(utf8[1], ms.Memory^, len);
      if backup then
      begin
        ext := ExtractFileExt(filename);
        RenameFile(filename, ChangeFileExt(filename, ext +'_bak'));
      end;
      ms.SaveToFile(filename);
    end;
  finally
    ms.Free
  end;
end;
//------------------------------------------------------------------------------

(*
//DoUpdateFolder template
procedure DoUpdateFolder(const folderName: string; backup, recursive: Boolean);
var
  i: integer;
  ext: string;
  sr: TSearchRec;
begin
  Writeln(folderName);
  i := FindFirst(folderName+'*.*', faAnyFile, sr);
  try
    while i = 0 do
    begin
      if (sr.Name[1] <> '.') then
      begin
        ext := LowerCase(ExtractFileExt(sr.Name));
        if recursive and (sr.Attr and faDirectory <> 0) then
        begin
          DoUpdateFolder(folderName + AppendSlash(sr.Name),
            backup, recursive)
        end
        else if ext = '.dpr' then
        begin
          //do stuff
        end
        else if ext = '.pas' then
        begin
          //do stuff
        end;
      end;
      i := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;
end;
//------------------------------------------------------------------------------
*)

procedure DoUpdateFolder(const folderName: string; backup, recursive: Boolean);
var
  i: integer;
  ext: string;
  sr: TSearchRec;
begin
  Writeln(folderName);
  i := FindFirst(folderName+'*.*', faAnyFile, sr);
  try
    while i = 0 do
    begin
      if (sr.Name[1] <> '.') then
      begin
        ext := LowerCase(ExtractFileExt(sr.Name));
        if recursive and (sr.Attr and faDirectory <> 0) then
        begin
          DoUpdateFolder(folderName + AppendSlash(sr.Name), backup, recursive)
        end
        else if ext = '.dpr' then
        begin
          Replace(folderName + sr.Name,
            'Image32.inc','Img32.inc', backup, [rfIgnoreCase])
        end
        else if ext = '.pas' then
        begin
          Replace(folderName + sr.Name,
            'Image32Panels,','Img32.Panels,', backup, [rfReplaceAll, rfIgnoreCase]);

          Replace(folderName + sr.Name,
            'TImage32','TImage32!', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'edImage32','edImage32!', backup, [rfReplaceAll, rfIgnoreCase]);

          Replace(folderName + sr.Name,
            'Image32_SVG_','Img32.SVG.', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32_BMP','Img32.Fmt.BMP', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32_JPG','Img32.Fmt.JPG', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32_PNG','Img32.Fmt.PNG', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32_GIF','Img32.Fmt.GIF', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32_SVG','Img32.Fmt.SVG', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32_Ttf','Img32.Text', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32_','Img32.', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32.','Img32.', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32,','Img32,', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'Image32;','Img32;', backup, [rfReplaceAll, rfIgnoreCase]);

          Replace(folderName + sr.Name,
            'TImage32!','TImage32', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'edImage32!','edImage32', backup, [rfReplaceAll, rfIgnoreCase]);
          Replace(folderName + sr.Name,
            'FontLibrary.Add(','FontManager.Load(', backup, [rfReplaceAll, rfIgnoreCase]);
        end;
      end;
      i := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;
end;

//------------------------------------------------------------------------------
//application entry point
//------------------------------------------------------------------------------

var
  useRecursion, useBackup: Boolean;
  msg, folder: string;
const
  msgTemplate = 'CAUTION: You''re about to update - '#10'  %s'#10+
    '%s recursion and %s backup. Are you happy to proceed?';
  withBool: array[boolean] of string = ('without','with');
begin
  ///////////////////////////////////////////////
  useRecursion  := true;    //modify these
  useBackup     := false;   //as desired
  ///////////////////////////////////////////////

  folder := IdListConstToPath(CSIDL_MYDOCUMENTS);
  if not GetFolder('Select Folder', folder) then Exit;
  msg := Format(msgTemplate,
    [folder, withBool[useRecursion], withBool[useBackup]]);
  if MessageBox(0, PChar(msg),
    'Upgrading', MB_YESNO or MB_DEFBUTTON2) <> IDYES then Exit;

  DoUpdateFolder(AppendSlash(folder), useBackup, useRecursion);
end.
