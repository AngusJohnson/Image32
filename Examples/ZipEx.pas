unit ZipEx;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.1                                                             *
* Date      :  29 February 2020                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  This unit is an alternative to Delphi's System.Zip unit.        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Windows, Messages, Types, SysUtils, Classes, Math, ZLib;

type
  TArrayOfByte = array of Byte;
  TDupEntryType = (dupSkip, dupOverwrite);

type
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

  //Note: multi-disk ZIP archives are no longer supported (totally obsolete)

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
    VersionNeed        : Word;        //20 (version 2)                       (2)
    Flag               : Word;        //either 0 or 1 (encrypted) here       (2)
    ComprMethod        : Word;        //either 0 (STORE) or 8 (DEFLATE) here (2)
    FileTime           : Word;        //                                     (2)
    FileDate           : Word;        //                                     (2)
    CRC32              : Cardinal;    //crc of uncompressed data             (4)
    ComprSize          : integer;     //if = UnComprSize == STORE            (4)
    UnComprSize        : integer;     //                                     (4)
    FileNameLen        : Word;        //                                     (2)
    ExtraLen           : Word;        //                                     (2)
  end;

  //TEntryInfo: a custom record that's used by TZipStructure below
  PEntryInfo = ^TEntryInfo;
  TEntryInfo = record
    cfh            : TCentralFileHeader;  //modified internally
    lfh            : TLocalFileHeader;    //may be modified directly
    isEncrypted    : Boolean;             //modified internally
    password       : AnsiString;          //modified internally
    filename       : UTF8String;          //may be modified directly
    fileTime       : Windows.TFileTime;   //may be modified directly
    comments       : UTF8String;          //may be modified directly
    centralExtra   : TArrayOfByte;        //may be modified directly
    localExtra     : TArrayOfByte;        //may be modified directly
    compressedData : TArrayOfByte;        //see note1 below
  end;
  TEntryInfos = array of TEntryInfo;

  //note1: TEntryInfo.localData can't be changed without checking and updating
  //       TEntryInfo.unComprSize, TEntryInfo.lfh.CRC32, TEntryInfo.isEncrypted,
  //       and TEntryInfo.lfh.flags too. (See EditZipEntry below.)
  //note2: When loading zip files, CRC and file sizes in TEntryInfo.cfh may
  //       be used to update zeroed TEntryInfo.lfh values. After that, only
  //       TEntryInfo.lfh values will be used when saving back to file.

  TZipStructure = record
    beforeZip : TArrayOfByte;           //may be modified directly
    afterZip  : TArrayOfByte;           //may be modified directly (max 65K)
    eoc       : TEndOfCentralHeader;    //modified internally
    eis       : TEntryInfos;            //see above
    comments  : UTF8String;             //may be modified directly
    password  : AnsiString;
  end;

  TZipFileEx = class
  private
    zipStructure: TZipStructure;
    function GetCount: integer;
    function GetEntryInfo(index: integer): PEntryInfo;
  public
    constructor Create; overload;
    constructor Create(const filename: string); overload;
    constructor Create(stream: TStream); overload;
    destructor Destroy; override;
    procedure Clear;

    function LoadFromFile(const filename: string): Boolean;
    function LoadFromStream(stream: TStream): Boolean;
    function SaveToFile(const filename: string): Boolean;
    procedure SaveToStream(stream: TStream);
    function GetEntryIndex(const entryName: string): integer;

    //nb: fileTime below expects UTC filetime (ie unlocalized)
    function EditEntry(index: integer; const uncompressedData: TArrayOfByte;
      fileTime: TFileTime; const comment: string = ''): Boolean;
    function AddEntry(const name: string; uncompressedData: TArrayOfByte;
      dupType: TDupEntryType; fileTime: TFileTime; const comment: string = ''): integer;

    function ExtractEntry(index: integer; out buffer: TArrayOfByte;
      const password: ansiString = ''): Boolean; overload;
    function ExtractEntry(index: integer; outStream: TStream;
      const password: ansiString = ''): Boolean; overload;
    function DeleteEntry(index: integer): Boolean;

    //EncryptEntry: Encrypts an existing entry using PKWARE encryption. While
    //this encryption is weak, custom encryptions could be implemented here too.
    function EncryptEntry(index: integer; const password: AnsiString = ''): Boolean;
    //DecryptEntry: This simplifies ExtractEntry the EditEntry into one op
    function DecryptEntry(index: integer; const password: AnsiString = ''): Boolean;

    property Count: integer read GetCount;
    property Entries[index: integer]: PEntryInfo read GetEntryInfo; default;
  end;

function CompressData(const uncompressedData: TArrayOfByte): TArrayOfByte;
function DecompressData(const compressedData: TArrayOfByte): TArrayOfByte;

//Various date-time format conversion routines
function FileTimeToDosTime(fileTime: Windows.TFileTime): LongInt;
function DosTimeToFileTime(dosTime: LongInt): Windows.TFileTime;
function DatetimeToDosTime(datetime: TDatetime): LongInt;
function DosTimeToDatetime(dosTime: LongInt): TDatetime;
function FiletimeTotDateTime(filetime: Windows.TFileTime): TDatetime;
function DateTimeToFiletime(datetime: TDatetime): Windows.TFileTime;

implementation

ResourceString
  rsZipErrorComp   = 'Zip error while compressing.';
  rsZipErrorDecomp = 'Zip error while decompressing.';


const

  MULTIPLE_DISK_SIG      = $08074b50; // 'PK'#7#8
  DATA_DESCRIPT_SIG      = MULTIPLE_DISK_SIG;
  LOCAL_HEADERSIG        = $04034b50; // 'PK'#3#4
  CENTRAL_HEADERSIG      = $02014b50; // 'PK'#1#2
  EOC_HEADERSIG          = $06054b50; // 'PK'#5#6
  PASSWORD_MAXLEN        = 80;        //Limit set by PKWare's Zip specs.
  ERROR_VALUE = -1;

  crcTable:  ARRAY[0..255] OF DWORD =
 ($00000000, $77073096, $EE0E612C, $990951BA,
  $076DC419, $706AF48F, $E963A535, $9E6495A3,
  $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
  $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
  $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
  $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
  $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
  $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
  $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
  $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
  $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
  $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
  $26D930AC, $51DE003A, $C8D75180, $BFD06116,
  $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
  $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
  $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

  $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
  $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
  $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
  $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
  $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
  $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
  $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
  $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
  $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
  $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
  $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
  $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
  $5005713C, $270241AA, $BE0B1010, $C90C2086,
  $5768B525, $206F85B3, $B966D409, $CE61E49F,
  $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
  $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

  $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
  $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
  $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
  $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
  $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
  $F762575D, $806567CB, $196C3671, $6E6B06E7,
  $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
  $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
  $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
  $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
  $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
  $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
  $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
  $CC0C7795, $BB0B4703, $220216B9, $5505262F,
  $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
  $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

  $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
  $9C0906A9, $EB0E363F, $72076785, $05005713,
  $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
  $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
  $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
  $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
  $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
  $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
  $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
  $A7672661, $D06016F7, $4969474D, $3E6E77DB,
  $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
  $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
  $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
  $BAD03605, $CDD70693, $54DE5729, $23D967BF,
  $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
  $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

function CalcCRC32(p: PByte; length: integer): cardinal;
var
  i: integer;
begin
  result := $FFFFFFFF;
  for i := 0 to length-1 do
  begin
    result := (result shr 8) xor
      crcTable[p^ xor (result and $000000ff)];
    inc(p);
  end;
  result := not result;
end;
//--------------------------------------------------------------------------

function CompressData(const uncompressedData: TArrayOfByte): TArrayOfByte;
const
  delta = 256;
var
  zstream: TZStreamRec;
  inSize, outSize, code: Integer;
begin
  //note: any encryption should be done AFTER compression, if for no other
  //reason than it reduces the effectiveness of compression algorithms.
  Result := nil;
  inSize := Length(uncompressedData);
  if inSize = 0 then Exit;
  FillChar(zstream, SizeOf(TZStreamRec), 0);
  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  SetLength(Result, outSize);
  try
    zstream.next_in := @uncompressedData[0];
    zstream.avail_in := inSize;
    zstream.next_out := @Result[0];
    zstream.avail_out := outSize;

    //nb: a negative windowBits value is used here (-15) so 'raw' compressed
    //    data will be returned (ie without zlib headers or trailers)
    //    see https://www.zlib.net/manual.html
    if DeflateInit2(zstream, Z_BEST_COMPRESSION,
      Z_DEFLATED, -15, 9, Z_DEFAULT_STRATEGY) < 0 then
        raise Exception.Create(rsZipErrorComp);
    try
      while true do
      begin
        code := deflate(zstream, Z_FINISH);
        if code = Z_STREAM_END then Break
        else if (code < 0) and (code <> Z_BUF_ERROR) then
          raise Exception.Create(rsZipErrorComp);

        Inc(outSize, delta);
        SetLength(Result, outSize);
        zstream.next_out := @Result[0];
        zstream.avail_out := delta;
      end;
    finally
      if deflateEnd(zstream) < 0 then
        raise Exception.Create(rsZipErrorComp);
    end;
    SetLength(Result, zstream.total_out);
  except
    SetLength(Result, 0);
    raise;
  end;
end;
//--------------------------------------------------------------------------

function DecompressData(const compressedData: TArrayOfByte): TArrayOfByte;
var
  //precondition: compressedData must be decrypted BEFORE decompression
  zstream: TZStreamRec;
  delta, inSize, outSize, code: Integer;
  tmp: TArrayOfByte; //in case 'compressedData' is also receives the result
begin
  inSize := Length(compressedData);
  delta := (inSize + 255) and not 255;
  outSize := delta;
  SetLength(tmp, outSize);
  FillChar(zstream, SizeOf(TZStreamRec), 0);
  try
    zstream.next_in := @compressedData[0];
    zstream.avail_in := inSize;
    zstream.next_out := @tmp[0];
    zstream.avail_out := outSize;

    if InflateInit2(zstream, -15) < 0 then
      raise Exception.Create(rsZipErrorDecomp);
    try
      while true do
      begin
        code := inflate(zstream, Z_NO_FLUSH);
        if code = Z_STREAM_END then Break
        else if (code < 0) and (code <> Z_BUF_ERROR) then
          raise Exception.Create(rsZipErrorDecomp);

        Inc(outSize, delta);
        SetLength(tmp, outSize);
        zstream.next_out := PByte(@tmp[0]) + zstream.total_out;
        zstream.avail_out := delta;
      end;
    finally
      if inflateEnd(zstream) < 0 then
      raise Exception.Create(rsZipErrorDecomp);
    end;
    SetLength(tmp, zstream.total_out);
    Result := tmp;
  except
    SetLength(Result, 0);
    raise;
  end;
end;
//--------------------------------------------------------------------------

function ValidateEOCHeader(eoc: TEndOfCentralHeader): Boolean;
begin
  //nb: multi-disk zip archives are no longer supported
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

procedure LoadEntryInfo(stream: TStream; delta: integer; var ei: TEntryInfo);
var
  savedPos: integer;
  isValid: Boolean;
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
  isValid :=
    (ei.cfh.HeaderSig = CENTRAL_HEADERSIG) and
    (ei.lfh.HeaderSig = LOCAL_HEADERSIG);
  if isValid then
  begin
    stream.Position := stream.Position + ei.lfh.FileNameLen;
    SetLength(ei.localExtra, ei.lfh.ExtraLen);
    if ei.lfh.ExtraLen > 0 then
      stream.Read(ei.localExtra[0], ei.lfh.ExtraLen);
    if ei.lfh.ComprSize = 0 then
      ei.lfh.ComprSize := ei.cfh.CompressedSize;
    SetLength(ei.compressedData, ei.lfh.ComprSize);
    if ei.lfh.ComprSize > 0 then
      stream.Read(ei.compressedData[0], ei.lfh.ComprSize);
    if ei.lfh.FileDate = 0 then
      PInteger(@ei.lfh.FileTime)^ := ei.cfh.FileDate;
    DosDateTimeToFileTime(ei.lfh.FileDate, ei.lfh.FileTime, ei.fileTime);

    if ei.lfh.CRC32 = 0 then ei.lfh.CRC32 := ei.cfh.CRC32;
    if ei.lfh.UnComprSize = 0 then
      ei.lfh.UnComprSize := ei.cfh.UncompressedSize;
    ei.isEncrypted := Odd(ei.lfh.Flag);
  end;
  stream.Position := SavedPos;
end;
//------------------------------------------------------------------------------

function NewZipStructure: TZipStructure;
begin
  Result.beforeZip := nil;
  Result.afterZip := nil;
  Result.eis := nil;
  FillChar(Result.eoc, SizeOf(TEndOfCentralHeader), 0);
  Result.eoc.HeaderSig := EOC_HEADERSIG;
  Result.comments := '';
end;
//------------------------------------------------------------------------------

function LoadZipStructure(stream: TStream;
  out zs: TZipStructure): Boolean; overload;
var
  i, delta, savedPos: integer;
begin
  Result := false;
  zs.beforeZip := nil;
  zs.afterZip := nil;
  zs.eis := nil;
  zs.eoc := GetEndCentralHeader(stream);
  if not ValidateEOCHeader(zs.eoc) then Exit;

  //load comments
  if zs.eoc.ZipCommentLen > 0 then
  begin
    savedPos := stream.Position;
    stream.Position := stream.Position + SizeOf(TEndOfCentralHeader);
    SetLength(zs.comments, zs.eoc.ZipCommentLen);
    stream.Read(zs.comments[1], zs.eoc.ZipCommentLen);
    stream.Position := savedPos;
  end;

  //load all the entries
  SetLength(zs.eis, zs.eoc.TotalEntries);
  stream.Position := stream.Position - zs.eoc.CentralSize;
  delta := stream.Position - zs.eoc.CentralOffset;
  for i := 0 to zs.eoc.TotalEntries -1 do
    LoadEntryInfo(stream, delta, zs.eis[i]);

  if delta > 0 then
  begin
    SetLength(zs.beforeZip, delta);
    stream.Position := 0;
    stream.Read(zs.beforeZip[0], delta);
  end;

  i := zs.eoc.CentralOffset + zs.eoc.CentralSize +
    SizeOf(zs.eoc) + zs.eoc.ZipCommentLen;
  if i < stream.Size then
  begin
    stream.Position := i;
    delta := stream.Size - i;
    SetLength(zs.afterZip, delta);
    stream.Read(zs.afterZip[0], delta);
  end;
  Result := zs.eoc.HeaderSig = EOC_HEADERSIG;
end;
//------------------------------------------------------------------------------

function LoadZipStructure(const filename: string;
  out zs: TZipStructure): Boolean; overload;
var
  fs: TFileStream;
begin
  Result := false;
  if FileExists(filename) then
  try
    fs := TFileStream.Create(filename, fmOpenRead);
    try
      Result := LoadZipStructure(fs, zs);
    finally
      fs.Free;
    end;
  except
  end;
  if not result then zs := NewZipStructure;
end;
//------------------------------------------------------------------------------

procedure SaveZipStructure(outStream: TStream; var zs: TZipStructure); overload;
var
  i, cnt, delta, lenFn, LenX, lenC, lenD: integer;
  pei: PEntryInfo;
begin
  cnt := 0;
  delta := Length(zs.beforeZip);

  //write any data prepending the zip archive
  if delta > 0 then
    outStream.Write(zs.beforeZip[0], delta);

  //write "local file headers" + compressed file data
  for i := 0 to High(zs.eis) do
  begin
    lenFn := Length(zs.eis[i].filename);
    if lenFn = 0 then Continue;
    pei := @zs.eis[i];
    LenX := Length(pei.localExtra);
    lenD := Length(pei.compressedData);

    FillChar(pei.cfh, SizeOf(TCentralFileHeader), 0);
    pei.cfh.HeaderSig := CENTRAL_HEADERSIG;
    pei.cfh.RelOffLocalHdr := outStream.Position - delta;
    pei.lfh.VersionNeed := 20;
    pei.cfh.Version := pei.lfh.VersionNeed;
    pei.cfh.MadeByVersion := 0;  //MS-DOS (0)
    pei.cfh.HostVersionNo := 20; //version 2 (20/10);
    pei.cfh.Flag := pei.lfh.Flag;

    pei.cfh.FileDate := FileTimeToDosTime(pei.fileTime);
    PInteger(@pei.lfh.FileTime)^ := pei.cfh.FileDate;

    if lenD < pei.lfh.UnComprSize then
    begin
      pei.lfh.ComprMethod := 8;    //DEFLATE
      pei.lfh.ComprSize := lenD;
    end else
    begin
      pei.lfh.ComprMethod := 0;     //STORE
      lenD := pei.lfh.UnComprSize;
      pei.lfh.ComprSize := lenD;
    end;
    pei.cfh.CompressionMethod := pei.lfh.ComprMethod;
    pei.cfh.CompressedSize    := pei.lfh.ComprSize;
    pei.cfh.UncompressedSize  := pei.lfh.UnComprSize;

    pei.cfh.CRC32 := pei.lfh.CRC32;
    pei.lfh.FileNameLen := lenFn;
    pei.cfh.FileNameLength := lenFn;
    pei.lfh.ExtraLen := LenX;

    outStream.Write(pei.lfh, SizeOf(TLocalFileHeader));
    outStream.Write(pei.filename[1], lenFn);
    if LenX > 0 then outStream.Write(pei.localExtra[0], LenX);
    if LenD > 0 then outStream.Write(pei.compressedData[0], LenD);
  end;

  zs.eoc.CentralOffset := outStream.Position - delta;

  //write "central file headers"
  for i := 0 to High(zs.eis) do
  begin
    lenFn := Length(zs.eis[i].filename);
    if lenFn = 0 then Continue;
    inc(cnt);
    pei := @zs.eis[i];
    lenC := Length(pei.comments);
    LenX := Length(pei.centralExtra);
    pei.cfh.ExtraFieldLength := LenX;
    pei.cfh.FileCommentLen := lenC;

    outStream.Write(pei.cfh, SizeOf(TCentralFileHeader));
    outStream.Write(pei.filename[1], lenFn);
    if LenX > 0 then outStream.Write(pei.centralExtra[0], LenX);
    if LenC > 0 then outStream.Write(pei.comments[1], LenC);
  end;

  //write "end of central headers"
  zs.eoc.CentralSize := outStream.Position - delta - zs.eoc.CentralOffset;
  zs.eoc.ThisDiskEntries := cnt;
  zs.eoc.TotalEntries := cnt;
  zs.eoc.ZipCommentLen := Length(zs.comments);
  outStream.Write(zs.eoc, SizeOf(TEndOfCentralHeader));
  if zs.comments <> '' then
    outStream.Write(zs.comments[1], Length(zs.comments));

  //finally write any data appending the zip archive
  if assigned(zs.afterZip) then
    outStream.Write(zs.afterZip[0], Length(zs.afterZip));
end;
//------------------------------------------------------------------------------

function SaveZipStructure(const filename: string;
  var zs: TZipStructure): Boolean; overload;
var
  fs: TFileStream;
begin
  try
    fs := TFileStream.Create(filename, fmCreate);
    try
      SaveZipStructure(fs, zs);
      Result := true;
    finally
      fs.Free;
    end;
  except
    Result := false;
  end;
end;
//------------------------------------------------------------------------------

function GetZipEntryIndex(const zs: TZipStructure;
  const entryName: string): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to High(zs.eis) do
    if SameText(string(zs.eis[i].filename), entryName) then
    begin
      Result := i;
      Break;
    end;
end;

//------------------------------------------------------------------------------
// PKWare (weak) encryption
//------------------------------------------------------------------------------

function UpdateCRC32(key: DWord; b: byte): DWord;
begin
  Result := crcTable[Byte(key) xor b] xor (key shr 8);
end;
//------------------------------------------------------------------------------

procedure UpdateKeys(var keys: array of DWord; b: byte);
begin
  keys[0] := UpdateCRC32(keys[0], b);
  keys[1] := keys[1] + (keys[0] and $FF);
  keys[1] := longword(keys[1] * $8088405 + 1);
  keys[2] := UpdateCRC32(keys[2], byte(keys[1] shr 24));
end;
//------------------------------------------------------------------------------

function CryptByte(var keys: array of DWord): Byte;
var
  tmp: Word;
begin
  tmp := Word(keys[2]) or 2;
  Result := (tmp * (tmp xor 1)) shr 8;
end;
//------------------------------------------------------------------------------

function EncryptByte(var keys: array of DWord; b: byte): byte;
begin
  Result  := CryptByte(keys) xor b;
  UpdateKeys(keys, b);
end;
//------------------------------------------------------------------------------

procedure DecryptByte(var keys: array of DWord; var b: byte);
begin
  b  := CryptByte(keys) xor b;
  UpdateKeys(keys, b);
end;
//------------------------------------------------------------------------------

function PKWareEncryptEntry(var zs: TZipStructure; index: integer;
  const password: AnsiString = ''): Boolean;
var
  keys: array[0..2] of DWord;
  i, len: integer;
  pw: AnsiString;
  pei: PEntryInfo;
begin
  Result := false;
  pei := @zs.eis[index];
  len := Length(pei.compressedData);
  if pei.isEncrypted or (len = 0) then
    Exit
  else if (password <> '') then
    pw := password
  else
  begin
    if zs.password = '' then Exit;
    pw := zs.password;
  end;

  //expand and move localData to accommodate the 12 byte header
  SetLength(pei.compressedData, len + 12);
  Move(pei.compressedData[0], pei.compressedData[12], len);
  inc(len, 12);

  //initialize keys
  keys[0] := 305419896; keys[1] := 591751049; keys[2] := 878082192;
  for i := 1 to Length(pw) do UpdateKeys(keys, byte(pw[i]));

  //generate header
  Randomize;
  for i := 0 to 10 do pei.compressedData[i] := Random(256);
  pei.compressedData[11] := (pei.lfh.CRC32 shr 24);

  //encrypt the data
  for i := 0 to len -1 do
    pei.compressedData[i] := EncryptByte(keys, pei.compressedData[i]);

  pei.lfh.Flag := pei.lfh.Flag or $1; //set lfh encryption flag
  pei.isEncrypted := true;
  Result := true;
end;
//------------------------------------------------------------------------------

function PKWareDecryptEntry(var zs: TZipStructure; index: integer;
  const password: AnsiString; out data: TArrayOfByte): Boolean;
var
  keys: array[0..2] of DWord;
  pw: AnsiString;
  pei: PEntryInfo;
  i, len: integer;
  header: TArrayOfByte;
begin
  //note: unlike PKWareEncryptEntry, PKWareDecryptEntry decrypts only
  //      a COPY of the zip entry. To remove encryption from the actual
  //      entry, the entry will have to be edited (see EditZipEntry).
  Result := false;
  pei := @zs.eis[index];
  len := Length(pei.compressedData);

  if not pei.isEncrypted or (len <= 12) then exit //not encrypted
  else if (password <> '') then pw := password
  else
  begin
    if zs.password = '' then Exit;
    pw := zs.password;
  end;

  //initialize keys
  keys[0] := 305419896; keys[1] := 591751049; keys[2] := 878082192;
  for i := 1 to Length(pw) do UpdateKeys(keys, byte(pw[i]));

  //copy the 12 byte header and check that it's valid
  SetLength(header, 12);
  Move(pei.compressedData[0], header[0], 12);
  for i := 0 to 11 do
    DecryptByte(keys, header[i]);

  //validate header
  if header[11] <> (pei.lfh.CRC32 shr 24) then Exit; //oops!

  //decrypt the data
  dec(len, 12);
  SetLength(data, len);
  Move(pei.compressedData[12], data[0], len);
  for i := 0 to len -1 do
    DecryptByte(keys, data[i]);
  Result := true;
end;

//------------------------------------------------------------------------------
// Additional zip file functions (used internally by TZipFileEx)
//------------------------------------------------------------------------------

function DatetimeToDosTime(datetime: TDatetime): LongInt;
var
  sysTime: TSystemTime;
  filetime: Windows.TFileTime;
begin
  DateTimeToSystemTime(datetime, sysTime);
  if SystemTimeToFileTime(sysTime, filetime) and
      FileTimeToDosDateTime(filetime,
        LongRec(Result).Hi, LongRec(Result).Lo) then Exit;
  Result := 0;
end;
//------------------------------------------------------------------------------

function DosTimeToDatetime(dosTime: LongInt): TDatetime;
var
  sysTime: TSystemTime;
  filetime: Windows.TFileTime;
begin
  if DosDateTimeToFileTime(LongRec(dosTime).Hi,
            LongRec(dosTime).Lo, filetime) and
    FileTimeToSystemTime(filetime, sysTime) then
      Result := SystemTimeToDateTime(sysTime) else
      Result := 0;
end;
//------------------------------------------------------------------------------


function FileTimeToDosTime(fileTime: Windows.TFileTime): LongInt;
begin
  if not FileTimeToDosDateTime(FileTime,
    LongRec(Result).Hi, LongRec(Result).Lo) then Result := 0;
end;
//------------------------------------------------------------------------------

function DosTimeToFileTime(dosTime: LongInt): Windows.TFileTime;
begin
  if not DosDateTimeToFileTime(LongRec(dosTime).Hi,
    LongRec(dosTime).Lo, Result) then TLargeInteger(Result) := 0;
end;
//------------------------------------------------------------------------------

function DosDateTimeUtcToFiletime(dosTime: longint): Windows.TFileTime;
begin
  if not DosDateTimeToFileTime(LongRec(dosTime).Hi,
    LongRec(dosTime).Lo, result) then TLargeInteger(Result) := 0;
end;
//------------------------------------------------------------------------------

function FiletimeTotDateTime(filetime: Windows.TFileTime): TDatetime;
var
  sysTime: TSystemTime;
begin
  if FileTimeToSystemTime(filetime, sysTime) then
    Result := SystemTimeToDateTime(sysTime) else
    Result := 0;
end;
//------------------------------------------------------------------------------

function DateTimeToFiletime(datetime: TDatetime): Windows.TFileTime;
var
  sysTime: TSystemTime;
begin
  DateTimeToSystemTime(datetime, sysTime);
  if not SystemTimeToFileTime(sysTime, Result) then
    TLargeInteger(Result) := 0;
end;
//------------------------------------------------------------------------------

function AddZipEntry(var zs: TZipStructure; const name: string;
  const uncompressedData: TArrayOfByte; dupType: TDupEntryType;
  fileTime: TFileTime; const comment: string): integer;
var
  len: integer;
  dosTime: longint;
  compressedData: TArrayOfByte;
  pei: PEntryInfo;
begin
  Result := GetZipEntryIndex(zs, name);
  if Result >= 0 then
  begin
    if dupType = dupSkip then Exit;
    pei := @zs.eis[Result];
  end else
  begin
    Result := Length(zs.eis);
    SetLength(zs.eis, Result +1);
    pei := @zs.eis[Result];
    FillChar(pei.lfh, SizeOf(TLocalFileHeader), 0);
    pei.lfh.HeaderSig := LOCAL_HEADERSIG;
    pei.filename := UTF8Encode(name);
  end;

  compressedData := CompressData(uncompressedData);
  len := Length(uncompressedData);
  if len <= Length(compressedData) then
  begin
    compressedData := uncompressedData;
    pei.lfh.ComprMethod := 0; //stored
  end else
    pei.lfh.ComprMethod := 8; //deflate

  pei.compressedData := compressedData;
  pei.lfh.UnComprSize := len;
  dosTime := FileTimeToDosTime(fileTime);
  PInteger(@pei.lfh.FileTime)^ := dosTime;
  if len = 0 then
    pei.lfh.CRC32 := 0 else
    pei.lfh.CRC32 := CalcCRC32(@uncompressedData[0], len);
  pei.comments := UTF8Encode(comment);
end;
//------------------------------------------------------------------------------

function ExtractZipEntry(const zs: TZipStructure; index: integer): TArrayOfByte;
var
  len: cardinal;
begin
  //note: there's no range checking here and I'd recommend
  //testing this result with the stored CRC value.
  result := nil;
  len := Length(zs.eis[index].compressedData);
  if len = 0 then Exit
  else if zs.eis[index].cfh.CompressionMethod = 0 then
  begin
    SetLength(Result, len);
    Move(zs.eis[index].compressedData[0], Result[0], len);
  end else
    Result := DecompressData(zs.eis[index].compressedData);
 end;
//------------------------------------------------------------------------------

procedure EditZipEntry(var zs: TZipStructure; index: integer;
  const uncompressedData: TArrayOfByte; fileTime: TFileTime;
  const comment: string);
var
  len: integer;
  compressedData: TArrayOfByte;
  pei: PEntryInfo;
begin
  pei := @zs.eis[index];
  pei.comments := UTF8Encode(comment);
  //remove any trace of prior encryption
  pei.isEncrypted := false;
  pei.fileTime := fileTime;
  pei.lfh.Flag := pei.lfh.Flag and not $1;

  len := Length(uncompressedData);
  pei.lfh.UnComprSize := len;
  if len = 0 then
  begin
    pei.lfh.CRC32 := 0;
    compressedData := nil;
    Exit;
  end;

  //compress
  compressedData := CompressData(uncompressedData);
  //nb: when compressedData >= uncompressedData then SaveZipStructure()
  //    will automatically 'store' rather than 'deflate' then entry.
  if len <= Length(compressedData) then
    compressedData := uncompressedData;
  pei.compressedData := compressedData;

  pei.lfh.CRC32 := CalcCRC32(@uncompressedData[0], len);
end;
//------------------------------------------------------------------------------

procedure DeleteZipEntry(var zs: TZipStructure; index: integer);
begin
  Finalize(zs.eis[index]); //probably redundant
  Delete(zs.eis, index, 1);
end;

//------------------------------------------------------------------------------
// TZipFileEx class
//------------------------------------------------------------------------------

constructor TZipFileEx.Create;
begin
  zipStructure := NewZipStructure;
end;
//------------------------------------------------------------------------------

constructor TZipFileEx.Create(const filename: string);
begin
  Create;
  LoadFromFile(filename);
end;
//------------------------------------------------------------------------------

constructor TZipFileEx.Create(stream: TStream);
begin
  Create;
  LoadFromStream(stream);
end;
//------------------------------------------------------------------------------

destructor TZipFileEx.Destroy;
begin
  Clear;
end;
//------------------------------------------------------------------------------

function TZipFileEx.LoadFromFile(const filename: string): Boolean;
begin
  Clear;
  result := LoadZipStructure(filename, zipStructure);
end;
//------------------------------------------------------------------------------

function TZipFileEx.LoadFromStream(stream: TStream): Boolean;
begin
  Clear;
  result := LoadZipStructure(stream, zipStructure);
end;
//------------------------------------------------------------------------------

function TZipFileEx.SaveToFile(const filename: string): Boolean;
begin
  Result := SaveZipStructure(filename, zipStructure);
end;
//------------------------------------------------------------------------------

procedure TZipFileEx.SaveToStream(stream: TStream);
begin
  SaveZipStructure(stream, zipStructure);
end;
//------------------------------------------------------------------------------

function TZipFileEx.GetCount: integer;
begin
  Result := Length(zipStructure.eis);
end;
//------------------------------------------------------------------------------

function TZipFileEx.GetEntryInfo(index: integer): PEntryInfo;
begin
  if (index >= 0) and (index < Length(zipStructure.eis)) then
    Result := @zipStructure.eis[index] else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TZipFileEx.GetEntryIndex(const entryName: string): integer;
begin
  Result := GetZipEntryIndex(zipStructure, entryName);
end;
//------------------------------------------------------------------------------

procedure TZipFileEx.Clear;
begin
  Finalize(zipStructure);
  zipStructure := NewZipStructure;
end;
//------------------------------------------------------------------------------

function TZipFileEx.EditEntry(index: integer;
  const uncompressedData: TArrayOfByte;
  fileTime: TFileTime; const comment: string = ''): Boolean;
begin
  Result := (index >= 0) and (index < Length(zipStructure.eis));
  if Result then
    EditZipEntry(zipStructure, index, uncompressedData, fileTime, comment);
end;
//------------------------------------------------------------------------------

function TZipFileEx.AddEntry(const name: string;
  uncompressedData: TArrayOfByte; dupType: TDupEntryType;
  fileTime: TFileTime; const comment: string): integer;
begin
  Result := GetZipEntryIndex(zipStructure, name);
  if (Result >= 0) then
  begin
    if dupType = dupSkip then Exit;
    EditZipEntry(zipStructure, Result, uncompressedData, fileTime, comment);
  end else
    Result := AddZipEntry(zipStructure,
      name, uncompressedData, dupType, fileTime, comment);
end;
//------------------------------------------------------------------------------

function TZipFileEx.ExtractEntry(index: integer;
      out buffer: TArrayOfByte; const password: ansiString): Boolean;
begin
  result :=  (index >= 0) and (index < Length(zipStructure.eis));
  if not Result then Exit;
  if zipStructure.eis[index].isEncrypted then
  begin
    if PKWareDecryptEntry(zipStructure, index, password, buffer) and
      not (zipStructure.eis[index].lfh.ComprMethod = 0) then
        buffer := DecompressData(buffer);
  end else
    buffer := ExtractZipEntry(zipStructure, index);

  result :=
    CalcCRC32(@buffer[0], Length(buffer)) = zipStructure.eis[index].lfh.CRC32;
end;
//------------------------------------------------------------------------------

function TZipFileEx.ExtractEntry(index: integer;
  outStream: TStream; const password: ansiString): Boolean;
var
  buffer: TArrayOfByte;
  len: integer;
begin
  result :=  (index >= 0) and (index < Length(zipStructure.eis));
  if not Result then Exit;
  if zipStructure.eis[index].isEncrypted then
  begin
    if PKWareDecryptEntry(zipStructure, index, password, buffer) then
      buffer := DecompressData(buffer);
  end else
    buffer := ExtractZipEntry(zipStructure, index);

  len := Length(buffer);
  result := (len > 0) and
    (CalcCRC32(@buffer[0], len) = zipStructure.eis[index].lfh.CRC32) and
    (outStream.Write(buffer, len) = len);
end;
//------------------------------------------------------------------------------

function TZipFileEx.DeleteEntry(index: integer): Boolean;
begin
  Result := (index >= 0) and (index < Length(zipStructure.eis));
  if Result then
    DeleteZipEntry(zipStructure, index);
end;
//------------------------------------------------------------------------------

function TZipFileEx.EncryptEntry(index: integer; const password: AnsiString = ''): Boolean;
begin
  Result := (index >= 0) and (index < Length(zipStructure.eis)) and
    PKWareEncryptEntry(zipStructure, index, password);
end;
//------------------------------------------------------------------------------

function TZipFileEx.DecryptEntry(index: integer; const password: AnsiString = ''): Boolean;
var
  buffer: TArrayOfByte;
  pei: PEntryInfo;
begin
  pei := GetEntryInfo(index);
  Result := assigned(pei) and ExtractEntry(index, buffer, password) and
    EditEntry(index, buffer, pei.fileTime, string(pei.comments));
end;
//------------------------------------------------------------------------------

end.
