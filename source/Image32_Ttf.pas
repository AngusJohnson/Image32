unit Image32_Ttf;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.0                                                             *
* Date      :  17 September 2020                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2020                                              *
* Purpose   :  TrueType fonts for TImage32 (without Windows dependency)        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Types, SysUtils, Classes, Math, Image32, Image32_Vector;

type
  TFixed = type single;
  Int16 = type SmallInt;
  TFontFormat = (ffInvalid, ffTrueType, ffCompact);


  TTtfOffsetTable = packed record
    sfntVersion   : Cardinal;  //$10000 or 'OTTO'
    numTables     : WORD;
    searchRange   : WORD;
    entrySelector : WORD;
    rangeShift    : WORD;
  end;

  TTtfTable = packed record
    tag           : Cardinal;
    checkSum      : Cardinal;
    offset        : Cardinal;
    length        : Cardinal;
  end;

  TTtfTable_Cmap = packed record
    version       : Word;
    numTables     : WORD;
  end;

  TCmapTblRec = packed record
    platformID    : WORD; //Unicode = 0; Windows = 3 (obsolete);
    encodingID    : WORD;
    offset        : Cardinal;
  end;

  TCmapFormat0 = packed record
    format        : WORD; //0
    length        : WORD;
    language      : WORD;
  end;

  TCmapFormat4 = packed record
    format        : WORD; //4
    length        : WORD;
    language      : WORD;
    segCountX2    : WORD;
    searchRange   : WORD;
    entrySelector : WORD;
    rangeShift    : WORD;
    //endCodes    : array of WORD; //last = $FFFF
    //reserved    : WORD; //0
    //startCodes  : array of WORD;
  end;

  TCmapFormat6 = packed record
    format        : WORD; //6
    length        : WORD;
    language      : WORD;
    firstCode     : WORD;
    entryCount    : WORD;
  end;

  TTtfTable_Name = packed record
    format        : WORD;
    count         : WORD;
    stringOffset  : WORD;
    //nameRecords[]
  end;

  TNameRec = packed record
    platformID        : WORD;
    encodingID        : WORD;
    languageID        : WORD;
    nameID            : WORD;
    length            : WORD;
    offset            : WORD;
  end;

  TTtfTable_Head = packed record
    majorVersion   : Word;
    minorVersion   : Word;
    fontRevision   : TFixed;
    checkSumAdjust : Cardinal;
    magicNumber    : Cardinal; //$5F0F3CF5
    flags          : Word;
    unitsPerEm     : Word;
    dateCreated    : UInt64;
    dateModified   : UInt64;
    xMin           : Int16;
    yMin           : Int16;
    xMax           : Int16;
    yMax           : Int16;
    macStyle       : Word;
    lowestRecPPEM  : Word;
    fontDirHint    : Int16;
    indexToLocFmt  : Int16;
    glyphDataFmt   : Int16;
  end;

  TTtfTable_Maxp = packed record
    version        : TFixed;
    numGlyphs      : WORD;
    maxPoints      : WORD;
    maxContours    : WORD;
  end;

  TTtfTable_Glyf = packed record
    numContours    : Int16;
    xMin           : Int16;
    yMin           : Int16;
    xMax           : Int16;
    yMax           : Int16;
  end;

  TTtfTable_Hhea = packed record
    version        : TFixed;
    ascent         : Int16;
    descent        : Int16;
    lineGap        : Int16;
    advWidthMax    : WORD;
    minLSB         : Int16;
    minRSB         : Int16;
    xMaxExtent     : Int16; //max(lsb + (xMax-xMin)) ... see TTtfTable_Head
    caretSlopeRise : Int16;
    caretSlopeRun  : Int16;
    caretOffset    : Int16;
    reserved       : UInt64;
    metricDataFmt  : Int16;
    numLongHorMets : WORD;
  end;

  TTtfTable_Hmtx = packed record
    advanceWidth    : WORD;
    leftSideBearing : Int16;
  end;

  TTtfTable_Loca = packed record
    //offset   : Word or Cardinal array(see below);
    //offsets should be 32bit aligned and are relative to the 'glyf' table
    //offset[0] is the offset of the 'missing' character glyph
    //offset = WORD array when head.indexToLocFmt == 0 else it's Cardinals
  end;

  //TTtfFontInfo: custom record
  TTtfFontInfo = record
    fontFormat     : TFontFormat;
    faceName       : string;
    style          : string;
    copyright      : string;
    manufacturer   : string;
    glyphCount     : integer;

    unitsPerEm     : integer;
    xMin           : integer;
    yMin           : integer;
    xMax           : integer;
    yMax           : integer;
    ascent         : integer;
    descent        : integer;
    lineGap        : integer;
    advWidthMax    : integer;
    minLSB         : integer;
    minRSB         : integer;
    xMaxExtent     : integer;
  end;

  TGlyphMetrics = record
    glyf: TTtfTable_Glyf;
    hmtx: TTtfTable_Hmtx;
    upm : integer;
  end;

  TTtfTableArray = array of TTtfTable;
  TArrayOfByte = array of Byte;
  TArrayOfWord = array of WORD;
  TArrayOfCardinal = array of Cardinal;
  TArrayOfCmapTblRec = array of TCmapTblRec;

  TPointEx = record
    pt: TPointD;
    flag: byte;
  end;
  TPathEx = array of TPointEx;
  TPathsEx = array of TPathEx;

  TTableName = (name, head, hhea, cmap, maxp, loca, glyf, hmtx);

  TTtfFontReader = class
  private
    fStream            : TMemoryStream;
    fFontInfo          : TTtfFontInfo;
    fTableCount        : integer;
    fPointCount        : integer;
    fTables            : TTtfTableArray;
    fTblIndex          : array[TTableName] of integer;
    fTbl_name          : TTtfTable_Name;
    fTbl_head          : TTtfTable_Head;
    fTbl_hhea          : TTtfTable_Hhea;
    fTbl_cmap          : TTtfTable_Cmap;
    fTbl_maxp          : TTtfTable_Maxp;
    fTbl_glyf          : TTtfTable_Glyf;
    fTbl_hmtx          : TTtfTable_Hmtx;
    fTbl_loca2         : TArrayOfWord;
    fTbl_loca4         : TArrayOfCardinal;
    fCmapTblRecs       : TArrayOfCmapTblRec;
    fFormat0CodeMap    : array[0..255] of byte;
    fFormat4EndCodes   : TArrayOfWord;
    fFormat4StartCodes : TArrayOfWord;
    fFormat4IdDelta    : TArrayOfWord;
    fFormat4RangeOff   : TArrayOfWord;
    fFormat4Offset     : integer;

    function GetTables: Boolean;
    function GetTable_name: Boolean;
    function GetTable_cmap: Boolean;
    function GetTable_maxp: Boolean;
    function GetTable_head: Boolean;
    function GetTable_loca: Boolean;
    function GetTable_hhea: Boolean;
    function GetGlyphInfo(glyphIdx: integer): TPathsEx;
    function GetGlyphIdxFromCmapIdx(idx: Word): integer;
    function GetSimpleGlyph: TPathsEx;
    function GetCompositeGlyph: TPathsEx;
    function ConvertSplinesToBeziers(const pathsEx: TPathsEx): TPathsEx;
    procedure GetPathCoords(var paths: TPathsEx);
    function GetGlyphHorzMetrics(glyphIdx: integer): Boolean;
    function GetFontInfo: TTtfFontInfo;
    function GetGlyphMetrics: TGlyphMetrics;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function IsValidFontFormat: Boolean;
    function LoadFromStream(stream: TStream): Boolean;
    function LoadFromFile(const filename: string): Boolean;
    function GetGlyph(unicode: Word; out paths: TPathsD;
      out nextX: integer; out glyphMetrics: TGlyphMetrics): Boolean;
    function GetGlyphs(const text: string; out paths: TPathsD;
      out nextX: integer): Boolean;
    property FontInfo: TTtfFontInfo read GetFontInfo;
  end;

  TGlyphInfo = record
    unicode        : Word;
    contours       : TPathsD;
    metrics        : TGlyphMetrics;
  end;
  PGlyphInfo = ^TGlyphInfo;

  //TGlyphManager: speeds up text rendering by parsing font files only once for
  //each accessed character. It can also scale glyphs to a specified font
  //height and invert them too (which is necessary on Windows PCs).
  TGlyphManager = class
  private
    fGlyphInfoList     : TList;
    fSorted            : Boolean;
    fVerticalFlip      : Boolean;
    fFontHeight        : double;
    function FoundInList(c: Char): Boolean;
    procedure AddGlyph(unicode: Word; fontReader: TTtfFontReader;
      out paths: TPathsD; out glyphMetrics: TGlyphMetrics);
    procedure VerticalFlip(var paths: TPathsD);
    procedure Sort;
    function ListMissingChars(const charList: string): string;
    procedure FillMissingChars(const charList: string;
      fontReader: TTtfFontReader);
  public
    constructor Create(fontHeight: double = 0.0;
      verticalFlip: Boolean = true);
    destructor Destroy; override;
    procedure Clear;
    function GetChar(c: Char; fontReader: TTtfFontReader;
    out paths: TPathsD; out glyphMetrics: TGlyphMetrics): Boolean;
    function GetString(s: string; fontReader: TTtfFontReader;
      out paths: TPathsD; out nextX: double): Boolean;
  end;

implementation

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function WordSwap(val: WORD): WORD;
asm
  rol ax,8;
end;
//------------------------------------------------------------------------------

function Int16Swap(val: Int16): Int16;
asm
  rol ax,8;
end;
//------------------------------------------------------------------------------

function IntSwap(val: integer): integer;
asm
  bswap eax
end;
//------------------------------------------------------------------------------

function UInt64Swap(val: UInt64): UInt64;
asm
  MOV     EDX, val.Int64Rec.Lo
  BSWAP   EDX
  MOV     EAX, val.Int64Rec.Hi
  BSWAP   EAX
end;
//------------------------------------------------------------------------------

procedure GetByte(stream: TStream; out value: byte);
begin
  stream.Read(value, 1);
end;
//------------------------------------------------------------------------------

procedure GetShortInt(stream: TStream; out value: ShortInt);
begin
  stream.Read(value, 1);
end;
//------------------------------------------------------------------------------

function GetWord(stream: TStream; out value: WORD): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := WordSwap(value);
end;
//------------------------------------------------------------------------------

function GetInt16(stream: TStream; out value: Int16): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := Int16Swap(value);
end;
//------------------------------------------------------------------------------

function GetCardinal(stream: TStream; out value: Cardinal): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := Cardinal(IntSwap(Integer(value)));
end;
//------------------------------------------------------------------------------

function GetInt(stream: TStream; out value: integer): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := IntSwap(value);
end;
//------------------------------------------------------------------------------

function GetUInt64(stream: TStream; out value: UInt64): Boolean;
begin
  result := stream.Position + SizeOf(value) < stream.Size;
  stream.Read(value, SizeOf(value));
  value := UInt64Swap(value);
end;
//------------------------------------------------------------------------------

function Get2Dot14(stream: TStream; out value: single): Boolean;
var
  val: Int16;
begin
  result := GetInt16(stream, val);
  if result then value := val / 16384;
end;
//------------------------------------------------------------------------------

function GetFixed(stream: TStream; out value: TFixed): Boolean;
var
  val: integer;
begin
  result := GetInt(stream, val);
  value := val / 35536;
end;
//------------------------------------------------------------------------------

function GetWideString(stream: TStream; length: integer): string;
var
  i: integer;
  w: Word;
begin
  length := length div 2;
  setLength(Result, length);
  for i := 1 to length do
  begin
    GetWord(stream, w); //nb: reverses byte order
    result[i] := WideChar(w);
  end;
end;
//------------------------------------------------------------------------------

function GetAnsiString(stream: TStream; length: integer): string;
var
  ansi: AnsiString;
begin
  setLength(ansi, length);
  stream.Read(ansi[1], length);
  result := string(ansi);
end;

//------------------------------------------------------------------------------
// TTrueTypeReader
//------------------------------------------------------------------------------

constructor TTtfFontReader.Create;
begin
  fStream := TMemoryStream.Create;
end;
//------------------------------------------------------------------------------

destructor TTtfFontReader.Destroy;
begin
  Clear;
  fStream.Free;
end;
//------------------------------------------------------------------------------

procedure TTtfFontReader.Clear;
var
  i: integer;
begin
  fTableCount := 0;
  fPointCount := 0;
  fTables := nil;
  fCmapTblRecs := nil;
  fFormat4Offset := 0;
  fFormat4EndCodes := nil;
  fTbl_glyf.numContours := 0;
  fFontInfo.fontFormat := ffInvalid;
  fStream.Clear;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.IsValidFontFormat: Boolean;
begin
  result := fFontInfo.fontFormat = ffTrueType;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.LoadFromStream(stream: TStream): Boolean;
begin
  Clear;
  fStream.CopyFrom(stream, 0);
  fStream.Position := 0;
  result := GetTables;
  if not result then Clear;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.LoadFromFile(const filename: string): Boolean;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(fs);
  finally
    fs.free;
  end;
end;
//------------------------------------------------------------------------------

function GetOffsetTable(stream: TStream; out offTable: TTtfOffsetTable): Boolean;
begin
  result := stream.Position < stream.Size - SizeOf(TTtfOffsetTable);
  if not result then Exit;
  GetCardinal(stream, offTable.sfntVersion);
  GetWord(stream, offTable.numTables);
  GetWord(stream, offTable.searchRange);
  GetWord(stream, offTable.entrySelector);
  GetWord(stream, offTable.rangeShift);
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTables: Boolean;
var
  i: integer;
  tbl: TTableName;
  offTable: TTtfOffsetTable;
begin
  result := false;
  if not GetOffsetTable(fStream, offTable) then Exit;
  fTableCount := offTable.numTables;
  result := fStream.Position < fStream.Size - SizeOf(TTtfTable) * fTableCount;
  if not result then Exit;

  for tbl := Low(fTblIndex) to high(fTblIndex) do
    fTblIndex[tbl] := -1;
  SetLength(fTables, fTableCount);

  for i := 0 to fTableCount -1 do
  begin
    GetCardinal(fStream, fTables[i].tag);
    GetCardinal(fStream, fTables[i].checkSum);
    GetCardinal(fStream, fTables[i].offset);
    GetCardinal(fStream, fTables[i].length);
    case
      fTables[i].tag of
        $6E616D65: fTblIndex[name] := i;
        $68656164: fTblIndex[head] := i;
        $676C7966: fTblIndex[glyf] := i;
        $6C6F6361: fTblIndex[loca] := i;
        $6D617870: fTblIndex[maxp] := i;
        $636D6170: fTblIndex[cmap] := i;
        $68686561: fTblIndex[hhea] := i;
        $686D7478: fTblIndex[hmtx] := i;
    end;
  end;

  if fTblIndex[name] < 0 then fFontInfo.fontFormat := ffInvalid
  else if fTblIndex[glyf] < 0 then fFontInfo.fontFormat := ffCompact
  else fFontInfo.fontFormat := ffTrueType;

  result := (fFontInfo.fontFormat = ffTrueType) and
    (fTblIndex[name] >= 0) and GetTable_name and
    (fTblIndex[head] >= 0) and GetTable_head and
    (fTblIndex[hhea] >= 0) and GetTable_hhea and
    (fTblIndex[maxp] >= 0) and GetTable_maxp and
    (fTblIndex[loca] >= 0) and GetTable_loca and //loca must follow maxp
    (fTblIndex[cmap] >= 0) and GetTable_cmap;

end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_cmap: Boolean;
var
  i, segCount: integer;
  reserved: WORD;
  cmapRec: TCmapTblRec;
  format4Rec: TCmapFormat4;
  cmapTbl: TTtfTable;
begin
  Result := false;
  cmapTbl := fTables[fTblIndex[cmap]];
  if (fStream.Size <= cmapTbl.offset + cmapTbl.length) then Exit;

  fStream.Position := cmapTbl.offset;
  GetWord(fStream, fTbl_cmap.version);
  GetWord(fStream, fTbl_cmap.numTables);

  //only use the unicode table (0: always first)
  SetLength(fCmapTblRecs, fTbl_cmap.numTables);
  for i := 0 to fTbl_cmap.numTables -1 do
  begin
    GetWord(fStream, fCmapTblRecs[i].platformID);
    GetWord(fStream, fCmapTblRecs[i].encodingID);
    GetCardinal(fStream, fCmapTblRecs[i].offset);
  end;

  i := 0;
  while (i < fTbl_cmap.numTables) and
    (fCmapTblRecs[i].platformID <> 0) and
    (fCmapTblRecs[i].platformID <> 3) do inc(i);
  if i = fTbl_cmap.numTables then Exit;
  cmapRec := fCmapTblRecs[i];

  fStream.Position := cmapTbl.offset + cmapRec.offset;
  GetWord(fStream, format4Rec.format);
  GetWord(fStream, format4Rec.length);
  GetWord(fStream, format4Rec.language);

  if format4Rec.format = 0 then
  begin
    for i := 0 to 255 do
      GetByte(fStream, fFormat0CodeMap[i]);
    fFontInfo.glyphCount := 255;
  end
  else if format4Rec.format = 4 then
  begin
    fFontInfo.glyphCount := 0;
    GetWord(fStream, format4Rec.segCountX2);
    segCount := format4Rec.segCountX2 shr 1;
    GetWord(fStream, format4Rec.searchRange);
    GetWord(fStream, format4Rec.entrySelector);
    GetWord(fStream, format4Rec.rangeShift);
    SetLength(fFormat4EndCodes, segCount);
    for i := 0 to segCount -1 do
      GetWord(fStream, fFormat4EndCodes[i]);
    if fFormat4EndCodes[segCount-1] <> $FFFF then Exit; //error
    GetWord(fStream, reserved);
    if reserved <> 0 then Exit; //error
    SetLength(fFormat4StartCodes, segCount);
    for i := 0 to segCount -1 do
      GetWord(fStream, fFormat4StartCodes[i]);
    if fFormat4StartCodes[segCount-1] <> $FFFF then Exit; //error
    SetLength(fFormat4IdDelta, segCount);
    for i := 0 to segCount -1 do
      GetWord(fStream, fFormat4IdDelta[i]);
    SetLength(fFormat4RangeOff, segCount);
    fFormat4Offset := fStream.Position;
    for i := 0 to segCount -1 do
      GetWord(fStream, fFormat4RangeOff[i]);

    for i := 0 to segCount -1 do
      inc(fFontInfo.glyphCount,
        fFormat4EndCodes[i] - fFormat4StartCodes[i] + 1);
  end else
    Exit; //unsupported format

  Result := true;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyphIdxFromCmapIdx(idx: Word): integer;
var
  i: integer;
  w: WORD;
begin

  if fFormat4Offset = 0 then //must be Format0 mapping
  begin
    if idx > 255 then Result := 0
    else Result := fFormat0CodeMap[idx];
    Exit;
  end;

  //Format4 mapping

  result := 0; //default to the 'missing' glyph
  for i := 0 to High(fFormat4EndCodes) do
    if idx <= fFormat4EndCodes[i] then
    begin
      if idx < fFormat4StartCodes[i] then Exit;
      if fFormat4RangeOff[i] > 0 then
      begin
        fStream.Position := fFormat4Offset + fFormat4RangeOff[i] +
          2 * (i + idx - fFormat4StartCodes[i]);
        GetWord(fStream, w);
        if w < fTbl_maxp.numGlyphs then Result := w;
      end else
        result := (fFormat4IdDelta[i] + idx) and $FFFF;
      Exit;
    end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_maxp: Boolean;
const
  sizeof_ver_0_5 = SizeOf(TFixed) + SizeOf(WORD);
var
  maxpTbl: TTtfTable;
begin
  maxpTbl := fTables[fTblIndex[maxp]];
  Result := (fStream.Size > maxpTbl.offset + maxpTbl.length) and
    (maxpTbl.length >= sizeof_ver_0_5);
  if not Result then Exit;
  fStream.Position := maxpTbl.offset;
  GetFixed(fStream, fTbl_maxp.version);
  GetWord(fStream, fTbl_maxp.numGlyphs);
  if fTbl_maxp.version >= 1 then
  begin
    GetWord(fStream, fTbl_maxp.maxPoints);
    GetWord(fStream, fTbl_maxp.maxContours);
  end else
    Result := false;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_loca: Boolean;
var
  i: integer;
  locaTbl: TTtfTable;
begin
  locaTbl := fTables[fTblIndex[loca]];
  Result := fStream.Size > locaTbl.offset + locaTbl.length;
  if not Result then Exit;
  fStream.Position := locaTbl.offset;

  if fTbl_head.indexToLocFmt = 0 then
  begin
    SetLength(fTbl_loca2, fTbl_maxp.numGlyphs +1);
    for i := 0 to fTbl_maxp.numGlyphs do
      GetWord(fStream, fTbl_loca2[i]);
  end else
  begin
    SetLength(fTbl_loca4, fTbl_maxp.numGlyphs +1);
    for i := 0 to fTbl_maxp.numGlyphs do
      GetCardinal(fStream, fTbl_loca4[i]);
  end;
end;
//------------------------------------------------------------------------------


function IsUnicode(platformID: Word): Boolean;
begin
  Result := (platformID = 0) or (platformID = 3);
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_name: Boolean;

  function GetString(const nameRec: TNameRec): string;
  var
    sPos: integer;
  begin
    sPos := fStream.Position;
    fStream.Position := fTables[fTblIndex[name]].offset +
      fTbl_name.stringOffset + nameRec.offset;
    if IsUnicode(nameRec.platformID) then
      Result := GetWideString(fStream, nameRec.length) else
      Result := GetAnsiString(fStream, nameRec.length);
    fStream.Position := sPos;
  end;

var
  i, streamPos: integer;
  ansi: AnsiString;
  nameRec: TNameRec;
  nameTbl: TTtfTable;
begin
  fFontInfo.faceName := '';
  fFontInfo.style   := '';
  nameTbl := fTables[fTblIndex[name]];
  Result := (fStream.Size > nameTbl.offset + nameTbl.length) and
    (nameTbl.length >= SizeOf(TTtfTable_Name));
  if not Result then Exit;
  fStream.Position := nameTbl.offset;
  GetWord(fStream, fTbl_name.format);
  GetWord(fStream, fTbl_name.count);
  GetWord(fStream, fTbl_name.stringOffset);
  for i := 1 to fTbl_name.count do
  begin
    GetWord(fStream, nameRec.platformID);
    GetWord(fStream, nameRec.encodingID);
    GetWord(fStream, nameRec.languageID);
    GetWord(fStream, nameRec.nameID);
    GetWord(fStream, nameRec.length);
    GetWord(fStream, nameRec.offset);
    case nameRec.nameID of
      0: fFontInfo.copyright    := GetString(nameRec);
      1: fFontInfo.faceName         := GetString(nameRec);
      2: fFontInfo.style      := GetString(nameRec);
      3..7: continue;
      8: fFontInfo.manufacturer := GetString(nameRec);
      else break;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_head: Boolean;
var
  headTbl: TTtfTable;
begin
  headTbl := fTables[fTblIndex[head]];
  Result := (fStream.Size > headTbl.offset +
    headTbl.length) and
    (headTbl.length >= SizeOf(TTtfTable_Head));
  if not Result then Exit;
  fStream.Position := headTbl.offset;
  GetWord(fStream, fTbl_head.majorVersion);
  GetWord(fStream, fTbl_head.minorVersion);
  GetFixed(fStream, fTbl_head.fontRevision);
  GetCardinal(fStream, fTbl_head.checkSumAdjust);
  GetCardinal(fStream, fTbl_head.magicNumber);
  GetWord(fStream, fTbl_head.flags);
  GetWord(fStream, fTbl_head.unitsPerEm);
  GetUInt64(fStream, fTbl_head.dateCreated);
  GetUInt64(fStream, fTbl_head.dateModified);
  GetInt16(fStream, fTbl_head.xMin);
  GetInt16(fStream, fTbl_head.yMin);
  GetInt16(fStream, fTbl_head.xMax);
  GetInt16(fStream, fTbl_head.yMax);
  GetWord(fStream, fTbl_head.macStyle);
  GetWord(fStream, fTbl_head.lowestRecPPEM);
  GetInt16(fStream, fTbl_head.fontDirHint);
  GetInt16(fStream, fTbl_head.indexToLocFmt);
  GetInt16(fStream, fTbl_head.glyphDataFmt);
  result := fTbl_head.magicNumber = $5F0F3CF5
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_hhea: Boolean;
var
  hheaTbl: TTtfTable;
begin
  hheaTbl := fTables[fTblIndex[hhea]];
  Result := (fStream.Size > hheaTbl.offset + hheaTbl.length) and
    (hheaTbl.length >= SizeOf(TTtfTable_Hhea));
  if not Result then Exit;
  fStream.Position := hheaTbl.offset;

  GetFixed(fStream, fTbl_hhea.version);
  GetInt16(fStream, fTbl_hhea.ascent);
  GetInt16(fStream, fTbl_hhea.descent);
  GetInt16(fStream, fTbl_hhea.lineGap);
  GetWord(fStream, fTbl_hhea.advWidthMax);
  GetInt16(fStream, fTbl_hhea.minLSB);
  GetInt16(fStream, fTbl_hhea.minRSB);
  GetInt16(fStream, fTbl_hhea.xMaxExtent);
  GetInt16(fStream, fTbl_hhea.caretSlopeRise);
  GetInt16(fStream, fTbl_hhea.caretSlopeRun);
  GetInt16(fStream, fTbl_hhea.caretOffset);
  GetUInt64(fStream, fTbl_hhea.reserved);
  GetInt16(fStream, fTbl_hhea.metricDataFmt);
  GetWord(fStream, fTbl_hhea.numLongHorMets);
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyphHorzMetrics(glyphIdx: integer): Boolean;
var
  tmtxTbl: TTtfTable;
begin
  tmtxTbl := fTables[fTblIndex[hmtx]];
  Result := (fStream.Size > tmtxTbl.offset + tmtxTbl.length);
  if not Result then Exit;
  if glyphIdx < fTbl_hhea.numLongHorMets then
  begin
    fStream.Position := Integer(tmtxTbl.offset) + glyphIdx * 4;
    GetWord(fStream, fTbl_hmtx.advanceWidth);
    GetInt16(fStream, fTbl_hmtx.leftSideBearing);
  end else
  begin
    fStream.Position := Integer(tmtxTbl.offset) +
      Integer(fTbl_hhea.numLongHorMets -1) * 4;
    GetWord(fStream, fTbl_hmtx.advanceWidth);
    fStream.Position := Integer(tmtxTbl.offset +
      fTbl_hhea.numLongHorMets * 4) +
      2 * (glyphIdx - Integer(fTbl_hhea.numLongHorMets));
    GetInt16(fStream, fTbl_hmtx.leftSideBearing);
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyphInfo(glyphIdx: integer): TPathsEx;
var
  offset: cardinal;
  glyfTbl: TTtfTable;
begin
  result := nil;
  if fTbl_head.indexToLocFmt = 0 then
  begin
    offset := fTbl_loca2[glyphIdx] *2;
    if offset = fTbl_loca2[glyphIdx+1] *2 then Exit; //no contours
  end else
  begin
    offset := fTbl_loca4[glyphIdx];
    if offset = fTbl_loca4[glyphIdx+1] then Exit; //no contours
  end;
  glyfTbl := fTables[fTblIndex[glyf]];
  if offset >= glyfTbl.length then Exit;
  inc(offset, glyfTbl.offset);

  fStream.Position := offset;
  GetInt16(fStream, fTbl_glyf.numContours);
  GetInt16(fStream, fTbl_glyf.xMin);
  GetInt16(fStream, fTbl_glyf.yMin);
  GetInt16(fStream, fTbl_glyf.xMax);
  GetInt16(fStream, fTbl_glyf.yMax);

  if fTbl_glyf.numContours < 0 then
    result := GetCompositeGlyph else
    result := GetSimpleGlyph;
end;
//------------------------------------------------------------------------------

const
  //glyf flags - simple
  ON_CURVE                  = $1;
  X_SHORT_VECTOR            = $2;
  Y_SHORT_VECTOR            = $4;
  REPEAT_FLAG               = $8;
  X_DELTA                   = $10;
  Y_DELTA                   = $20;
//------------------------------------------------------------------------------

function TTtfFontReader.GetSimpleGlyph: TPathsEx;
var
  i,j: integer;
  instructLen: WORD;
  flag, repeats: byte;
  contourEnds: TArrayOfWord;
begin
  SetLength(contourEnds, fTbl_glyf.numContours);
  for i := 0 to High(contourEnds) do
    GetWord(fStream, contourEnds[i]);

  //hints are currently ignored
  GetWord(fStream, instructLen);
  fStream.Position := fStream.Position + instructLen;

  setLength(result, fTbl_glyf.numContours);
  setLength(result[0], contourEnds[0] +1);
  for i := 1 to High(result) do
    setLength(result[i], contourEnds[i] - contourEnds[i-1]);

  repeats := 0;
  for i := 0 to High(result) do
  begin
    for j := 0 to High(result[i]) do
    begin
      if repeats = 0 then
      begin
        GetByte(fStream, flag);
        if flag and REPEAT_FLAG = REPEAT_FLAG then
          GetByte(fStream, repeats);
      end else
        dec(repeats);
      result[i][j].flag := flag;
    end;
  end;
  GetPathCoords(result);
end;
//------------------------------------------------------------------------------

procedure TTtfFontReader.GetPathCoords(var paths: TPathsEx);
var
  i,j: integer;
  xi,yi: Int16;
  flag, xb,yb: byte;
  pt: TPoint;
begin
  if fTbl_glyf.numContours = 0 then Exit;

  //get X coords
  pt := Point(0,0);
  xi := 0;
  for i := 0 to high(paths) do
  begin
    for j := 0 to high(paths[i]) do
    begin
      flag := paths[i][j].flag;
      if flag and X_SHORT_VECTOR = X_SHORT_VECTOR then
      begin
        GetByte(fStream, xb);
        if (flag and X_DELTA) = 0 then
          dec(pt.X, xb) else
          inc(pt.X, xb);
      end else
      begin
        if flag and X_DELTA = 0 then
        begin
          GetInt16(fStream, xi);
          pt.X := pt.X + xi;
        end;
      end;
      paths[i][j].pt.X := pt.X;
    end;
  end;

  //get Y coords
  yi := 0;
  for i := 0 to high(paths) do
  begin
    for j := 0 to high(paths[i]) do
    begin
      flag := paths[i][j].flag;
      if flag and Y_SHORT_VECTOR = Y_SHORT_VECTOR then
      begin
        GetByte(fStream, yb);
        if (flag and Y_DELTA) = 0 then
          dec(pt.Y, yb) else
          inc(pt.Y, yb);
      end else
      begin
        if flag and Y_DELTA = 0 then
        begin
          GetInt16(fStream, yi);
          pt.Y := pt.Y + yi;
        end;
      end;
      paths[i][j].pt.Y := pt.Y;
    end;
  end;
end;
//------------------------------------------------------------------------------

function OnCurve(flag: byte): Boolean;
begin
  result := flag and ON_CURVE <> 0;
end;
//------------------------------------------------------------------------------

function MidPoint(const pt1, pt2: TPointEx): TPointEx;
begin
  Result.pt.X := (pt1.pt.X + pt2.pt.X) / 2;
  Result.pt.Y := (pt1.pt.Y + pt2.pt.Y) / 2;
  Result.flag := ON_CURVE;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.ConvertSplinesToBeziers(const pathsEx: TPathsEx): TPathsEx;
var
  i,j,k: integer;
  pt: TPointEx;
  prevOffCurve: Boolean;
begin
  SetLength(Result, Length(pathsEx));
  for i := 0 to High(pathsEx) do
  begin
    SetLength(Result[i], Length(pathsEx[i]) *2);
    Result[i][0] := pathsEx[i][0]; k := 1;
    prevOffCurve := false;
    for j := 1 to High(pathsEx[i]) do
    begin
      if OnCurve(pathsEx[i][j].flag) then
      begin
        prevOffCurve := false;
      end
      else if prevOffCurve then
      begin
        pt := MidPoint(pathsEx[i][j-1], pathsEx[i][j]);
        Result[i][k] := pt; inc(k);
      end else
        prevOffCurve := true;
      Result[i][k] := pathsEx[i][j]; inc(k);
    end;
    SetLength(Result[i], k);
  end;
end;
//------------------------------------------------------------------------------

procedure AppendPathsEx(var paths: TPathsEx; const extra: TPathsEx);
var
  i, len1, len2: integer;
begin
  len2 := length(extra);
  len1 := length(paths);
  setLength(paths, len1 + len2);
  for i := 0 to len2 -1 do
    paths[len1+i] := Copy(extra[i], 0, length(extra[i]));
end;
//------------------------------------------------------------------------------

procedure AffineTransform(const a,b,c,d,e,f: double; var pathsEx: TPathsEx);
const
  q = 9.2863575e-4; // 33/35536
var
  i,j: integer;
  m0,n0,m,n,xx: double;
begin
  m0 := max(abs(a), abs(b));
  n0 := max(abs(c), abs(d));

  if (m0 = 0) or (n0 = 0) then
  begin
    if (e = 0) and (f = 0) then Exit;

    for i := 0 to High(pathsEx) do
      for j := 0 to High(pathsEx[i]) do
        with pathsEx[i][j].pt do
        begin
          X := X + e;
          y := Y + f;
        end;

  end else
  begin
    //see https://developer.apple.com/fonts ...
    //... /TrueType-Reference-Manual/RM06/Chap6glyf.html

    if (abs(a)-abs(c))< q then m := 2 * m0 else m := m0;
    if (abs(b)-abs(d))< q then n := 2 * n0 else n := n0;

    for i := 0 to High(pathsEx) do
      for j := 0 to High(pathsEx[i]) do
        with pathsEx[i][j].pt do
        begin
          xx := m * ((a/m)*X + (c/m)*Y + e);
          y := m * ((b/n)*X + (d/n)*Y + f);
          X := xx;
        end;
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetCompositeGlyph: TPathsEx;
var
  streamPos: integer;
  flag, glyphIndex: WORD;
  arg1b, arg2b: ShortInt;
  arg1i, arg2i: Int16;
  tmp: single;
  a,b,c,d,e,f: double;
  compoundIdx, componentIdx: integer;
  componentPaths: TPathsEx;
  tbl_glyf_old: TTtfTable_Glyf;
const
  //glyf flags - composite
  ARG_1_AND_2_ARE_WORDS     = $1;
  ARGS_ARE_XY_VALUES        = $2;
  ROUND_XY_TO_GRID          = $4;
  WE_HAVE_A_SCALE           = $8;
  MORE_COMPONENTS           = $20;
  WE_HAVE_AN_X_AND_Y_SCALE  = $40;
  WE_HAVE_A_TWO_BY_TWO      = $80;
  WE_HAVE_INSTRUCTIONS      = $100;
  USE_MY_METRICS            = $200;
begin
  result := nil;
  flag := MORE_COMPONENTS;

  while (flag and MORE_COMPONENTS <> 0) do
  begin
    glyphIndex := 0;
    a := 0; b := 0; c := 0; d := 0; e := 0; f := 0;
    compoundIdx := 0; componentIdx := 0;

    GetWord(fStream, flag);
    GetWord(fStream, glyphIndex);

    if (flag and ARG_1_AND_2_ARE_WORDS <> 0) then
    begin
      GetInt16(fStream, arg1i);
      GetInt16(fStream, arg2i);
      if (flag and ARGS_ARE_XY_VALUES <> 0) then
      begin
        e := arg1i;
        f := arg2i;
      end else
      begin
        compoundIdx  := arg1i;
        componentIdx := arg2i;
      end;
    end else
    begin
      GetShortInt(fStream, arg1b);
      GetShortInt(fStream, arg2b);
      if (flag and ARGS_ARE_XY_VALUES <> 0) then
      begin
        e := arg1b;
        f := arg2b;
      end else
      begin
        compoundIdx  := Byte(arg1b);
        componentIdx := Byte(arg2b);
      end;
    end;

    if (flag and WE_HAVE_A_SCALE <> 0) then
    begin
      Get2Dot14(fStream, tmp);
      a := tmp; d := tmp;
    end
    else if (flag and WE_HAVE_AN_X_AND_Y_SCALE <> 0) then
    begin
      Get2Dot14(fStream, tmp); a := tmp;
      Get2Dot14(fStream, tmp); d := tmp;
    end
    else if (flag and WE_HAVE_A_TWO_BY_TWO <> 0) then
    begin
      Get2Dot14(fStream, tmp); a := tmp;
      Get2Dot14(fStream, tmp); b := tmp;
      Get2Dot14(fStream, tmp); c := tmp;
      Get2Dot14(fStream, tmp); d := tmp;
    end;

    tbl_glyf_old := fTbl_glyf;

    streamPos := fStream.Position;
    componentPaths := GetGlyphInfo(glyphIndex);
    fStream.Position := streamPos;

    if (flag and ARGS_ARE_XY_VALUES <> 0) then
    begin
      if (flag and USE_MY_METRICS <> 0) then
      begin
        if Result <> nil then
          AffineTransform(a,b,c,d,e,f, result);
      end else
        AffineTransform(a,b,c,d,e,f, componentPaths);
    end;

    if tbl_glyf_old.numContours > 0 then
    begin
      inc(fTbl_glyf.numContours, tbl_glyf_old.numContours);
      fTbl_glyf.xMin := Min(fTbl_glyf.xMin, tbl_glyf_old.xMin);
      fTbl_glyf.xMax := Max(fTbl_glyf.xMax, tbl_glyf_old.xMax);
      fTbl_glyf.yMin := Min(fTbl_glyf.yMin, tbl_glyf_old.yMin);
      fTbl_glyf.yMax := Max(fTbl_glyf.yMax, tbl_glyf_old.yMax);
    end;

    AppendPathsEx(result, componentPaths);
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyph(unicode: Word; out paths: TPathsD;
  out nextX: integer; out glyphMetrics: TGlyphMetrics): Boolean;
var
  i,j, glyphIdx: integer;
  pt2: TPointEx;
  bez: TPathD;
  pathsEx: TPathsEx;
begin
  paths  := nil;
  Result := IsValidFontFormat;
  if not Result then Exit;

  glyphIdx := GetGlyphIdxFromCmapIdx(unicode);
  if not GetGlyphHorzMetrics(glyphIdx) then Exit;

  pathsEx := GetGlyphInfo(glyphIdx); //gets raw splines
  pathsEx := ConvertSplinesToBeziers(pathsEx);
  glyphMetrics := GetGlyphMetrics; //nb: must follow GetGlyphInfo()
  nextX   := fTbl_hmtx.advanceWidth;
  if pathsEx = nil then Exit; //eg space character

  //now flatten ...
  setLength(paths, length(pathsEx));
  for i := 0 to High(pathsEx) do
  begin
    SetLength(paths[i],1);
    paths[i][0] := pathsEx[i][0].pt;
    for j := 1 to High(pathsEx[i]) do
    begin
      if OnCurve(pathsEx[i][j].flag) then
      begin
        AppendPoint(paths[i], pathsEx[i][j].pt);
      end else
      begin
        if j = High(pathsEx[i]) then
          pt2 := pathsEx[i][0] else
          pt2 := pathsEx[i][j+1];
        bez := FlattenQBezier(pathsEx[i][j-1].pt,
          pathsEx[i][j].pt, pt2.pt);
        paths[i] := JoinPaths(paths[i], bez);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyphs(const text: string;
  out paths: TPathsD; out nextX: integer): Boolean;
var
  i, currX, len: integer;
  tmpPaths: TPathsD;
  glyphMetrics: TGlyphMetrics;
begin
  paths := nil;
  Result := IsValidFontFormat;
  nextX := 0;
  len := Length(text);
  if not Result or (len = 0) then Exit;
  for i := 1 to len do
  begin
    result := GetGlyph(Ord(text[i]), tmpPaths, currX, glyphMetrics);
    if not result then Exit;
    if nextX > 0 then
      tmpPaths := OffsetPath(tmpPaths, nextX, 0);
    inc(nextX, currX);
    AppendPath(paths, tmpPaths);
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetFontInfo: TTtfFontInfo;
begin
  if not IsValidFontFormat then
  begin
    result.faceName := '';
    result.style := '';
    result.unitsPerEm := 0;
  end else
  begin
    result := fFontInfo;
    //and updated the record with everything except the strings
    result.unitsPerEm  := fTbl_head.unitsPerEm;
    result.xMin        := fTbl_head.xMin;
    result.xMax        := fTbl_head.xMax;
    result.yMin        := fTbl_head.yMin;
    result.yMax        := fTbl_head.yMax;
    result.ascent      := fTbl_hhea.ascent;
    result.descent     := fTbl_hhea.descent;
    result.lineGap     := fTbl_hhea.lineGap;
    result.advWidthMax := fTbl_hhea.advWidthMax;
    result.minLSB      := fTbl_hhea.minLSB;
    result.minRSB      := fTbl_hhea.minRSB;
    result.xMaxExtent  := fTbl_hhea.xMaxExtent;
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyphMetrics: TGlyphMetrics;
begin
  if IsValidFontFormat then
  begin
    result.glyf := fTbl_glyf;
    result.hmtx := ftbl_hmtx;
    result.upm  := fTbl_head.unitsPerEm;
  end else
    FillChar(result, sizeOf(Result), 0)
end;

//------------------------------------------------------------------------------
// TGlyphManager
//------------------------------------------------------------------------------

constructor TGlyphManager.Create(fontHeight: double = 0;
  verticalFlip: Boolean = true);
begin
  fGlyphInfoList := TList.Create;
  fSorted := false;
  fVerticalFlip := verticalFlip;
  fFontHeight := fontHeight;
end;
//------------------------------------------------------------------------------

destructor TGlyphManager.Destroy;
begin
  Clear;
  fGlyphInfoList.Free;
end;
//------------------------------------------------------------------------------

procedure TGlyphManager.Clear;
var
  i: integer;
begin
  for i := 0 to fGlyphInfoList.Count -1 do
    Dispose(PGlyphInfo(fGlyphInfoList[i]));
  fGlyphInfoList.Clear;
  fSorted := false;
end;
//------------------------------------------------------------------------------

function FindInSortedList(c: Char; glyphList: TList): integer;
var
  i,l,r: integer;
begin
  //binary search the sorted list ...
  l := 0;
  r := glyphList.Count -1;
  while l <= r do
  begin
    Result := (l + r) shr 1;
    i := PGlyphInfo(glyphList[Result]).unicode - Ord(c);
    if i < 0 then
    begin
      l := Result +1
    end else
    begin
      if i = 0 then Exit;
      r := Result -1;
    end;
  end;
  Result := -1;
end;
//------------------------------------------------------------------------------

function TGlyphManager.FoundInList(c: Char): Boolean;
begin
  if not fSorted then Sort;
  result := FindInSortedList(c, fGlyphInfoList) >= 0;
end;
//------------------------------------------------------------------------------

function TGlyphManager.ListMissingChars(const charList: string): string;
var
  i,j,r, len: integer;
  chars: string;
  c: Char;
begin
  chars := charList;
  len := length(chars);
  setLength(Result, len);
  r := 0;
  for i := 1 to len do
  begin
    c := chars[i];
    if c < #32 then continue
    else if not FoundInList(c) then
    begin
      inc(r);
      Result[r] := c;
    end;
    //avoid returning duplicates ...
    for j := i+1 to len do
      if chars[j] = c then
        chars[j] := #0;
  end;
  SetLength(Result, r);
end;
//------------------------------------------------------------------------------

procedure TGlyphManager.FillMissingChars(const charList: string;
  fontReader: TTtfFontReader);
var
  i: integer;
  paths: TPathsD;
  missing: string;
  dummy: TGlyphMetrics;
begin
  if not assigned(fontReader) or
    not fontReader.IsValidFontFormat then Exit;
  missing := ListMissingChars(charList);
  for i := 1 to Length(missing) do
    AddGlyph(Ord(missing[i]), fontReader, paths, dummy);
end;
//------------------------------------------------------------------------------

procedure TGlyphManager.VerticalFlip(var paths: TPathsD);
var
  i,j: integer;
begin
  for i := 0 to High(paths) do
    for j := 0 to High(paths[i]) do
      with paths[i][j] do Y := -Y;
end;
//------------------------------------------------------------------------------

function TGlyphManager.GetChar(c: Char; fontReader: TTtfFontReader;
  out paths: TPathsD; out glyphMetrics: TGlyphMetrics): Boolean;
var
  listIdx, dummy, adjUpm: integer;
  glyphInfo: PGlyphInfo;
begin
  if not fSorted then Sort;
  listIdx := FindInSortedList(c, fGlyphInfoList);
  Result := listIdx >= 0;
  if not Result then
  begin
    if not Assigned(fontReader) or
      not fontReader.IsValidFontFormat then Exit;
    AddGlyph(Ord(c), fontReader, paths, glyphMetrics);
  end else
  begin
    with PGlyphInfo(fGlyphInfoList[listIdx])^ do
    begin
      paths := contours;
      glyphMetrics := metrics;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TGlyphManager.GetString(s: string; fontReader: TTtfFontReader;
  out paths: TPathsD; out nextX: double): Boolean;
var
  i: integer;
  tmpPaths: TPathsD;
  metrics: TGlyphMetrics;
  dx: double;
begin
  Result := true;
  FillMissingChars(s, fontReader);
  nextX := 0;
  for i := 1 to Length(s) do
  begin
    Result := GetChar(s[i], nil, tmpPaths, metrics);
    if not result then Break;
    if i > 0 then
      tmpPaths := OffsetPath(tmpPaths, nextX, 0);
    if fFontHeight > 0 then
      dx := metrics.hmtx.advanceWidth * fFontHeight / metrics.upm else
      dx := metrics.hmtx.advanceWidth;
    nextX := nextX + dx;
    AppendPath(paths, tmpPaths);
  end;
end;
//------------------------------------------------------------------------------

function GlyphSorter(glyph1, glyph2: pointer): integer;
begin
  Result := PGlyphInfo(glyph1).unicode - PGlyphInfo(glyph2).unicode;
end;
//------------------------------------------------------------------------------

procedure TGlyphManager.Sort;
begin
  fGlyphInfoList.Sort(GlyphSorter);
  fSorted := true;
end;
//------------------------------------------------------------------------------

procedure TGlyphManager.AddGlyph(unicode: Word; fontReader: TTtfFontReader;
  out paths: TPathsD; out glyphMetrics: TGlyphMetrics);
var
  glyph: PGlyphInfo;
  dummy: integer;
begin
  fontReader.GetGlyph(unicode, paths, dummy, glyphMetrics);

  New(glyph);
  glyph.unicode := unicode;
  glyph.metrics := glyphMetrics;

  if fFontHeight > 0 then
    glyph.contours := ScalePath(paths,
      fFontHeight/fontReader.FontInfo.unitsPerEm)
  else
    glyph.contours := paths; //unscaled

  if fVerticalFlip then
    VerticalFlip(glyph.contours);

  fGlyphInfoList.Add(glyph);
  fSorted := false;
end;
//------------------------------------------------------------------------------

end.
