unit ZipHelper;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.0                                                             *
* Date      :  2 February 2020                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  Module that extends the standard Embarcadero Zip module         *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Windows, Messages, Types, SysUtils, Variants, Classes, Math;

const

  MULTIPLE_DISK_SIG      = $08074b50; // 'PK'#7#8
  DATA_DESCRIPT_SIG      = MULTIPLE_DISK_SIG;
  LOCAL_HEADERSIG        = $04034b50; // 'PK'#3#4
  CENTRAL_HEADERSIG      = $02014b50; // 'PK'#1#2
  EOC_HEADERSIG          = $06054b50; // 'PK'#5#6
  PASSWORD_MAXLEN        = 80;        //Limit set by PKWare's Zip specs.
  ERROR_VALUE = -1;

type
  TArrayOfByte = array of Byte;

  TEndOfCentralHeader = packed record  //                           (total = 22)
    HeaderSig          : Cardinal; //EOC_HEADERSIG                           (4)
    ThisDiskNo         : Word;     //This disk's number (zero based)         (2)
    CentralDiskNo      : Word;     //Disk No. on which central dir starts    (2)
    ThisDiskEntries    : Word;     //Central dir entry count on this disk    (2)
    TotalEntries       : Word;     //Total entries in central dir            (2)
    CentralSize        : integer;  //Size of central directory               (4)
    CentralOffset      : integer;  //offset of central dir on CentralDiskNo  (4)
    ZipCommentLen      : Word;     //                                        (2)
  end;

  TCentralFileHeader = packed record  //                            (total = 46)
    HeaderSig          : Cardinal;    //CENTRAL_HEADERSIG                    (4)
    MadeByVersion      : Byte;        //                                     (1)
    HostVersionNo      : Byte;        //                                     (1)
    Version            : Word;        //version needed to extract            (2)
    Flag               : Word;        //                                     (2)
    CompressionMethod  : Word;        //                                     (2)
    FileDate           : Integer;     //convert with FileDateToDateTime      (4)
    CRC32              : Integer;     //                                     (4)
    CompressedSize     : Integer;     //                                     (4)
    UncompressedSize   : Integer;     //                                     (4)
    FileNameLength     : Word;        //                                     (2)
    ExtraFieldLength   : Word;        //                                     (2)
    FileCommentLen     : Word;        //                                     (2)
    StartOnDisk        : Word;        //disk # on which file starts          (2)
    IntFileAttrib      : Word;        //internal file attr. ie: Text/Binary  (2)
    ExtFileAttrib      : Cardinal;    //external file attr.                  (4)
    RelOffLocalHdr     : integer;     //relative offset of LocalHeader       (4)
  end;

  TLocalFileHeader = packed record    //                            (total = 30)
    HeaderSig          : Cardinal;    //LOCAL_HEADERSIG                      (4)
    VersionNeed        : Word;        //                                     (2)
    Flag               : Word;        //                                     (2)
    ComprMethod        : Word;        //                                     (2)
    FileTime           : Word;        //                                     (2)
    FileDate           : Word;        //                                     (2)
    CRC32              : Cardinal;    //                                     (4)
    ComprSize          : integer;     //                                     (4)
    UnComprSize        : integer;     //                                     (4)
    FileNameLen        : Word;        //                                     (2)
    ExtraLen           : Word;        //                                     (2)
  end;

  TEntryInfo = record
    filename     : UTF8String;          //may be modified directly
    comments     : UTF8String;          //may be modified directly
    centralExtra : TArrayOfByte;        //may be modified directly
    localExtra   : TArrayOfByte;        //may be modified directly
    localData    : TArrayOfByte;        //may be modified directly
    unComprSize   : Cardinal;           //must modify when localData modified
    cfh          : TCentralFileHeader;  //modified internally
    lfh          : TLocalFileHeader;    //modified internally
    IsValid      : Boolean;
  end;
  TEntryInfos = array of TEntryInfo;

  TZipStructure = record
    beforeZip : TArrayOfByte;           //may be modified directly
    AfterZip  : TArrayOfByte;           //may be modified directly (max 65K)
    eoc       : TEndOfCentralHeader;    //modified internally
    eis       : TEntryInfos;            //see above
    comments  : UTF8String;             //may be modified directly
  end;

function GetZipStructure(stream: TStream): TZipStructure;
procedure SaveZipStructure(outStream: TStream; zs: TZipStructure);

function GetEntryIndex(var zs: TZipStructure; const entryName: UTF8String): integer;
function RenameEntry(var zs: TZipStructure; index: integer; const newName: UTF8String): Boolean;
function DeleteEntry(var zs: TZipStructure; index: integer): Boolean;

implementation

//------------------------------------------------------------------------------

function ValidateEOCHeader(eoc: TEndOfCentralHeader): Boolean;
begin
  //nb: multi-disk zips are no longer supported
  Result := (eoc.HeaderSig = EOC_HEADERSIG) and
    (eoc.ThisDiskEntries = eoc.TotalEntries) and
    (eoc.ThisDiskNo = 0) and (eoc.CentralDiskNo = 0);
end;
//------------------------------------------------------------------------------

function GetEndCentralHeader(stream: TStream): TEndOfCentralHeader;
var
  eochOffset, buffSize: integer;
  buffer, buffptr: PChar;
begin
  result.HeaderSig := 0;
  eochOffset := stream.Size -sizeof(TEndOfCentralHeader);
  if eochOffset < 0 then Exit; //invalid zip file

  stream.Position := eochOffset;
  stream.Read(Result, sizeof(TEndOfCentralHeader));

  if Result.HeaderSig <> EOC_HEADERSIG then
  begin
    //A zip file comment may exist at the end of the archive just after
    //the EndOfCentralHeader record. Its length may be up to 65k.

    buffSize := Min(Stream.size, MAXWORD); //ie: buffSize <= 65k
    buffer := AllocMem(buffSize);
    try
      stream.Position := stream.Size - buffSize;
      //read the block into the buffer...
      if Stream.Read(buffer^,buffSize) <> buffSize then exit;

      //perform a backwards search for EOC_HEADERSIG
      buffptr := buffer + buffSize - sizeof(TEndOfCentralHeader);
      while buffptr >= buffer do
      begin
        if PDWord(buffptr)^ = EOC_HEADERSIG then //FOUND!!
        begin
          //move the stream position to the beginning of the EOCHeader...
          Stream.position := Stream.position - buffSize + (buffptr - buffer);
          eochOffset := stream.Position;
          Stream.Read(Result, sizeof(TEndOfCentralHeader));
          break;
        end;
        dec(buffptr);
      end;
    finally
      FreeMem(buffer);
    end;
  end;
  stream.Position := eochOffset;
end;
//------------------------------------------------------------------------------

procedure GetEntryInfo(stream: TStream; delta: integer; var ei: TEntryInfo);
var
  savedPos: integer;
begin
  stream.Read(ei.cfh, SizeOf(ei.cfh));

  SetLength(ei.filename, ei.cfh.FileNameLength);
  stream.Read(ei.filename[1], ei.cfh.FileNameLength);

  SetLength(ei.centralExtra, ei.cfh.ExtraFieldLength);
  if ei.cfh.ExtraFieldLength > 0 then
    stream.Read(ei.centralExtra[0], ei.cfh.ExtraFieldLength);

  SetLength(ei.comments, ei.cfh.FileCommentLen);
  if ei.cfh.FileCommentLen > 0 then
    stream.Read(ei.comments[1], ei.cfh.FileCommentLen);

  SavedPos := stream.Position;
  stream.Position := ei.cfh.RelOffLocalHdr + delta;
  stream.Read(ei.lfh, SizeOf(ei.lfh));
  ei.IsValid :=
    (ei.cfh.HeaderSig = CENTRAL_HEADERSIG) and
    (ei.lfh.HeaderSig = LOCAL_HEADERSIG);
  if ei.IsValid then
  begin
    stream.Position := stream.Position + ei.lfh.FileNameLen;
    SetLength(ei.localExtra, ei.lfh.ExtraLen);
    if ei.lfh.ExtraLen > 0 then
      stream.Read(ei.localExtra[0], ei.lfh.ExtraLen);
    SetLength(ei.localData, ei.lfh.ComprSize);
    if ei.lfh.ComprSize > 0 then
      stream.Read(ei.localData[0], ei.lfh.ComprSize);
    ei.unComprSize := ei.lfh.UnComprSize;
  end;
  stream.Position := SavedPos;
end;
//------------------------------------------------------------------------------

function GetZipStructure(stream: TStream): TZipStructure;
var
  i, delta, savedPos: integer;
begin
  Result.beforeZip := nil;
  Result.AfterZip := nil;
  Result.eis := nil;
  Result.eoc := GetEndCentralHeader(stream);
  if not ValidateEOCHeader(Result.eoc) then Exit;

  if Result.eoc.ZipCommentLen > 0 then
  begin
    savedPos := stream.Position;
    stream.Position := stream.Position + SizeOf(TEndOfCentralHeader);
    SetLength(Result.comments, Result.eoc.ZipCommentLen);
    stream.Read(Result.comments[1], Result.eoc.ZipCommentLen);
    stream.Position := savedPos;
  end;

  SetLength(Result.eis, Result.eoc.TotalEntries);
  stream.Position := stream.Position - Result.eoc.CentralSize;
  delta := stream.Position - Result.eoc.CentralOffset;
  for i := 0 to Result.eoc.TotalEntries -1 do
    GetEntryInfo(stream, delta, Result.eis[i]);

  if delta > 0 then
  begin
    SetLength(Result.beforeZip, delta);
    stream.Position := 0;
    stream.Read(Result.beforeZip[0], delta);
  end;

  i := Result.eoc.CentralOffset + Result.eoc.CentralSize +
    SizeOf(Result.eoc) + Result.eoc.ZipCommentLen;
  if i < stream.Size then
  begin
    stream.Position := i;
    delta := stream.Size - i;
    SetLength(Result.AfterZip, delta);
    stream.Read(Result.AfterZip[0], delta);
  end;
end;
//------------------------------------------------------------------------------

procedure SaveZipStructure(outStream: TStream; zs: TZipStructure);
var
  i, cnt, delta, lenFn, LenX, lenC, lenD: integer;
begin

  cnt := 0;
  delta := Length(zs.beforeZip);

  //write "before zip"
  if delta > 0 then
    outStream.Write(zs.beforeZip[0], delta);

  //write "local file headers" + compressed file data
  for i := 0 to High(zs.eis) do
  begin
    lenFn := Length(zs.eis[i].filename);
    if lenFn = 0 then Continue;
    zs.eis[i].cfh.RelOffLocalHdr := outStream.Position - delta;
    LenX := Length(zs.eis[i].localExtra);
    lenD := Length(zs.eis[i].localData);
    zs.eis[i].lfh.FileNameLen := lenFn;
    zs.eis[i].lfh.ExtraLen := LenX;
    zs.eis[i].lfh.ComprSize := lenD;
    zs.eis[i].lfh.UnComprSize := zs.eis[i].unComprSize;
    outStream.Write(zs.eis[i].lfh, SizeOf(TLocalFileHeader));
    outStream.Write(zs.eis[i].filename[1], lenFn);
    if LenX > 0 then outStream.Write(zs.eis[i].localExtra[0], LenX);
    if LenD > 0 then outStream.Write(zs.eis[i].localData[0], LenD);
  end;

  zs.eoc.CentralOffset := outStream.Position - delta;

  //write "central file headers"
  for i := 0 to High(zs.eis) do
  begin
    lenFn := Length(zs.eis[i].filename);
    if lenFn = 0 then Continue;
    inc(cnt);
    lenC := Length(zs.eis[i].comments);
    LenX := Length(zs.eis[i].centralExtra);
    zs.eis[i].cfh.FileNameLength := lenFn;
    zs.eis[i].cfh.ExtraFieldLength := LenX;
    outStream.Write(zs.eis[i].cfh, SizeOf(TCentralFileHeader));
    outStream.Write(zs.eis[i].filename[1], lenFn);
    if LenX > 0 then outStream.Write(zs.eis[i].centralExtra[0], LenX);
    if LenC > 0 then outStream.Write(zs.eis[i].comments[1], LenC);
  end;

  //write "end of central headers"
  zs.eoc.CentralSize := outStream.Position - delta - zs.eoc.CentralOffset;
  zs.eoc.ThisDiskEntries := cnt;
  zs.eoc.TotalEntries := cnt;
  zs.eoc.ZipCommentLen := Length(zs.comments);
  outStream.Write(zs.eoc, SizeOf(TEndOfCentralHeader));
  if zs.comments <> '' then
    outStream.Write(zs.comments[1], Length(zs.comments));

  //write "after zip"
  if assigned(zs.AfterZip) then
    outStream.Write(zs.AfterZip[0], Length(zs.AfterZip));
end;
//------------------------------------------------------------------------------

function GetEntryIndex(var zs: TZipStructure; const entryName: UTF8String): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to High(zs.eis) do
    if SameText(string(zs.eis[i].filename), string(entryName)) then
    begin
      Result := i;
      Exit;
    end;
end;
//------------------------------------------------------------------------------

function RenameEntry(var zs: TZipStructure; index: integer; const newName: UTF8String): Boolean;
begin
  result := (index >= 0) and (index < Length(zs.eis));
  if Result then zs.eis[index].filename := newName;
end;
//------------------------------------------------------------------------------

function DeleteEntry(var zs: TZipStructure; index: integer): Boolean;
begin
  Result := RenameEntry(zs, index, '');
end;
//------------------------------------------------------------------------------

end.
