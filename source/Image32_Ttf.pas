unit Image32_Ttf;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  0.92 (initial draft)                                            *
* Date      :  15 September 2020                                               *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  TrueType fonts for TImage32 (without Windows dependancy)        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  Types, SysUtils, Classes, Math, Image32, Image32_Vector;

type
  TFixed = type single;
  Int16 = type SmallInt;

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

  //TTtfFontInfo: custom record
  TTtfFontInfo = record
    name           : string;
    subname        : string;
    unitsPerEm     : integer;
    xMin           : integer;
    yMin           : integer;
    xMax           : integer;
    yMax           : integer;
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
  TTtfGyphMetrics = TTtfTable_Glyf;

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

  TTtfTable_Htmx = packed record
    advanceWidth    : WORD;
    leftSideBearing : Int16;
  end;

  TTtfTable_Loca = packed record
    //offset   : Word or Cardinal array(see below);
    //offsets should be 32bit aligned and are relative to the 'glyf' table
    //offset[0] is the offset of the 'missing' character glyph
    //offset = WORD array when head.indexToLocFmt == 0 else it's Cardinals
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

  TTtfFontReader = class
  private
    stream: TMemoryStream;
    fontName: string;
    fontSubName: string;
    tableCount: integer;
    pointCount: integer;
    tables: TTtfTableArray;
    tblIdx_name, tblIdx_head, tblIdx_cmap, tblIdx_maxp,
    tblIdx_hhea, tblIdx_hmtx, tblIdx_loca, tblIdx_glyf: integer;
    tbl_name: TTtfTable_Name;
    tbl_head: TTtfTable_Head;
    tbl_hhea: TTtfTable_Hhea;
    tbl_cmap: TTtfTable_Cmap;
    tbl_maxp: TTtfTable_Maxp;
    tbl_glyf: TTtfTable_Glyf;
    tbl_loca_word: TArrayOfWord;
    tbl_loca_dword: TArrayOfCardinal;
    cmapTblRecs: TArrayOfCmapTblRec;

    //cmap Format0 array ...
    format0CodeMap: array[0..255] of byte;

    //cmap Format4 arrays ...
    format4EndCodes: TArrayOfWord;
    format4StartCodes: TArrayOfWord;
    format4IdDelta: TArrayOfWord;
    format4RangeOff: TArrayOfWord;
    format4Offset: integer;

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
    function GetGlyphHorzMetrics(glyphIdx: integer;
      out tbl_htmx: TTtfTable_Htmx): Boolean;
    function GetFontInfo: TTtfFontInfo;
    function GetLastGyphInfo: TTtfGyphMetrics;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function LoadFromStream(astream: TStream): Boolean;
    function LoadFromFile(const filename: string): Boolean;
    //GetGlyph: scale paths according to the following formula:
    //  scale => font.Size * screen.pixelsperinch / 72 / FontInfo.unitsPerEm
    function GetGlyph(chr: Char; out nextX: integer): TPathsD; overload;
    function GetGlyph(const text: string; out nextX: integer): TPathsD; overload;
    property FontInfo: TTtfFontInfo read GetFontInfo;
    //LastCharMetrics: metrics for the most recently accessed character
    property LastCharMetrics: TTtfGyphMetrics read GetLastGyphInfo;
  end;

implementation

const
  //glyf flags - simple
  ON_CURVE                  = $1;
  X_SHORT_VECTOR            = $2;
  Y_SHORT_VECTOR            = $4;
  REPEAT_FLAG               = $8;
  X_DELTA                   = $10;
  Y_DELTA                   = $20;

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
  stream := TMemoryStream.Create;
end;
//------------------------------------------------------------------------------

destructor TTtfFontReader.Destroy;
begin
  Clear;
  stream.Free;
end;
//------------------------------------------------------------------------------

procedure TTtfFontReader.Clear;
begin
  tableCount := 0;
  pointCount := 0;
  tables := nil;
  cmapTblRecs := nil;
  format4Offset := 0;
  format4EndCodes := nil;
  tbl_glyf.numContours := 0;
  stream.Clear;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.LoadFromStream(astream: TStream): Boolean;
begin
  Clear;
  stream.CopyFrom(astream, 0);
  stream.Position := 0;
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
  offTable: TTtfOffsetTable;
begin
  result := false;
  if not GetOffsetTable(stream, offTable) then Exit;
  tableCount := offTable.numTables;
  result := stream.Position < stream.Size - SizeOf(TTtfTable) * tableCount;
  if not result then Exit;

  tblIdx_head := -1; tblIdx_cmap := -1;
  tblIdx_hhea := -1; tblIdx_hmtx := -1;
  tblIdx_maxp := -1; tblIdx_loca := -1;
  tblIdx_glyf := -1; tblIdx_name := -1;

  SetLength(tables, tableCount);

  for i := 0 to tableCount -1 do
  begin
    GetCardinal(stream, tables[i].tag);
    GetCardinal(stream, tables[i].checkSum);
    GetCardinal(stream, tables[i].offset);
    GetCardinal(stream, tables[i].length);
    case
      tables[i].tag of
        $6E616D65: tblIdx_name := i;
        $68656164: tblIdx_head := i;
        $676C7966: tblIdx_glyf := i;
        $6C6F6361: tblIdx_loca := i;
        $6D617870: tblIdx_maxp := i;
        $636D6170: tblIdx_cmap := i;
        $68686561: tblIdx_hhea := i;
        $686D7478: tblIdx_hmtx := i;
    end;
  end;

  result := (tblIdx_glyf >= 0) and
    (tblIdx_name >= 0) and GetTable_name and
    (tblIdx_head >= 0) and GetTable_head and
    (tblIdx_hhea >= 0) and GetTable_hhea and
    (tblIdx_maxp >= 0) and GetTable_maxp and
    (tblIdx_loca >= 0) and GetTable_loca and //loca must follow maxp
    (tblIdx_cmap >= 0) and GetTable_cmap;

end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_cmap: Boolean;
var
  i, segCount: integer;
  reserved: WORD;
  cmapRec: TCmapTblRec;
  format4Rec: TCmapFormat4;
begin
  Result := false;
  if (stream.Size <= tables[tblIdx_cmap].offset +
    tables[tblIdx_cmap].length) then Exit;

  stream.Position := tables[tblIdx_cmap].offset;
  GetWord(stream, tbl_cmap.version);
  GetWord(stream, tbl_cmap.numTables);

  //only use the unicode table (0: always first)
  SetLength(cmapTblRecs, tbl_cmap.numTables);
  for i := 0 to tbl_cmap.numTables -1 do
  begin
    GetWord(stream, cmapTblRecs[i].platformID);
    GetWord(stream, cmapTblRecs[i].encodingID);
    GetCardinal(stream, cmapTblRecs[i].offset);
  end;

  if tbl_cmap.numTables = 0 then Exit; //should never happen

  cmapRec := cmapTblRecs[0];

  //we're only interesting in the unicode platform
  if cmapRec.platformID <> 0 then Exit;

  stream.Position := tables[tblIdx_cmap].offset + cmapRec.offset;
  GetWord(stream, format4Rec.format);
  GetWord(stream, format4Rec.length);
  GetWord(stream, format4Rec.language);

  if format4Rec.format = 0 then
  begin
    for i := 0 to 255 do
      GetByte(stream, format0CodeMap[i]);
  end
  else if format4Rec.format = 4 then
  begin
    GetWord(stream, format4Rec.segCountX2);
    segCount := format4Rec.segCountX2 shr 1;
    GetWord(stream, format4Rec.searchRange);
    GetWord(stream, format4Rec.entrySelector);
    GetWord(stream, format4Rec.rangeShift);
    SetLength(format4EndCodes, segCount);
    for i := 0 to segCount -1 do
      GetWord(stream, format4EndCodes[i]);
    if format4EndCodes[segCount-1] <> $FFFF then Exit; //error
    GetWord(stream, reserved);
    if reserved <> 0 then Exit; //error
    SetLength(format4StartCodes, segCount);
    for i := 0 to segCount -1 do
      GetWord(stream, format4StartCodes[i]);
    if format4StartCodes[segCount-1] <> $FFFF then Exit; //error
    SetLength(format4IdDelta, segCount);
    for i := 0 to segCount -1 do
      GetWord(stream, format4IdDelta[i]);
    SetLength(format4RangeOff, segCount);
    format4Offset := stream.Position;
    for i := 0 to segCount -1 do
      GetWord(stream, format4RangeOff[i]);
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

  if format4Offset = 0 then //must be Format0 mapping
  begin
    if idx > 255 then Result := 0
    else Result := format0CodeMap[idx];
    Exit;
  end;

  //Format4 mapping

  result := 0; //default to the 'missing' glyph
  for i := 0 to High(format4EndCodes) do
    if idx <= format4EndCodes[i] then
    begin
      if idx < format4StartCodes[i] then Exit;
      if format4RangeOff[i] > 0 then
      begin
        stream.Position := format4Offset + format4RangeOff[i] +
          2 * (i + idx - format4StartCodes[i]);
        GetWord(stream, w);
        if w < tbl_maxp.numGlyphs then Result := w;
      end else
        result := (format4IdDelta[i] + idx) and $FFFF;
      Exit;
    end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_maxp: Boolean;
const
  sizeof_ver_0_5 = SizeOf(TFixed) + SizeOf(WORD);
begin
  Result := (stream.Size > tables[tblIdx_maxp].offset +
    tables[tblIdx_maxp].length) and
    (tables[tblIdx_maxp].length >= sizeof_ver_0_5);
  if not Result then Exit;
  stream.Position := tables[tblIdx_maxp].offset;
  GetFixed(stream, tbl_maxp.version);
  GetWord(stream, tbl_maxp.numGlyphs);
  if tbl_maxp.version >= 1 then
  begin
    GetWord(stream, tbl_maxp.maxPoints);
    GetWord(stream, tbl_maxp.maxContours);
  end else
    Result := false;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_loca: Boolean;
var
  i: integer;
begin
  Result := stream.Size > tables[tblIdx_loca].offset + tables[tblIdx_loca].length;
  if not Result then Exit;
  stream.Position := tables[tblIdx_loca].offset;

  if tbl_head.indexToLocFmt = 0 then
  begin
    SetLength(tbl_loca_word, tbl_maxp.numGlyphs +1);
    for i := 0 to tbl_maxp.numGlyphs do
      GetWord(stream, tbl_loca_word[i]);
  end else
  begin
    SetLength(tbl_loca_dword, tbl_maxp.numGlyphs +1);
    for i := 0 to tbl_maxp.numGlyphs do
      GetCardinal(stream, tbl_loca_dword[i]);
  end;
end;
//------------------------------------------------------------------------------


function IsUnicode(platformID: Word): Boolean;
begin
  Result := (platformID = 0) or (platformID = 3);
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_name: Boolean;
var
  i, streamPos: integer;
  ansi: AnsiString;
  nameRec: TNameRec;
begin
  fontName := '';
  fontSubName   := '';
  Result := (stream.Size > tables[tblIdx_name].offset +
    tables[tblIdx_name].length) and
    (tables[tblIdx_name].length >= SizeOf(TTtfTable_Name));
  if not Result then Exit;
  stream.Position := tables[tblIdx_name].offset;
  GetWord(stream, tbl_name.format);
  GetWord(stream, tbl_name.count);
  GetWord(stream, tbl_name.stringOffset);
  for i := 1 to tbl_name.count do
  begin
    GetWord(stream, nameRec.platformID);
    GetWord(stream, nameRec.encodingID);
    GetWord(stream, nameRec.languageID);
    GetWord(stream, nameRec.nameID);
    GetWord(stream, nameRec.length);
    GetWord(stream, nameRec.offset);
    if (nameRec.nameID = 1) and (fontName = '') then
    begin
      streamPos := stream.Position;
      stream.Position := tables[tblIdx_name].offset +
        tbl_name.stringOffset + nameRec.offset;
      if IsUnicode(nameRec.platformID) then
        fontName := GetWideString(stream, nameRec.length) else
        fontName := GetAnsiString(stream, nameRec.length);
      stream.Position := streamPos;
    end
    else if (nameRec.nameID = 2) and (fontSubName = '') then
    begin
      streamPos := stream.Position;
      stream.Position := tables[tblIdx_name].offset +
        tbl_name.stringOffset + nameRec.offset;
      if IsUnicode(nameRec.platformID) then
        fontSubName := GetWideString(stream, nameRec.length) else
        fontSubName := GetAnsiString(stream, nameRec.length);
      Exit;
    end
    else if nameRec.nameID > 2 then
      break;
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_head: Boolean;
begin
  Result := (stream.Size > tables[tblIdx_head].offset +
    tables[tblIdx_head].length) and
    (tables[tblIdx_head].length >= SizeOf(TTtfTable_Head));
  if not Result then Exit;
  stream.Position := tables[tblIdx_head].offset;
  GetWord(stream, tbl_head.majorVersion);
  GetWord(stream, tbl_head.minorVersion);
  GetFixed(stream, tbl_head.fontRevision);
  GetCardinal(stream, tbl_head.checkSumAdjust);
  GetCardinal(stream, tbl_head.magicNumber);
  GetWord(stream, tbl_head.flags);
  GetWord(stream, tbl_head.unitsPerEm);
  GetUInt64(stream, tbl_head.dateCreated);
  GetUInt64(stream, tbl_head.dateModified);
  GetInt16(stream, tbl_head.xMin);
  GetInt16(stream, tbl_head.yMin);
  GetInt16(stream, tbl_head.xMax);
  GetInt16(stream, tbl_head.yMax);
  GetWord(stream, tbl_head.macStyle);
  GetWord(stream, tbl_head.lowestRecPPEM);
  GetInt16(stream, tbl_head.fontDirHint);
  GetInt16(stream, tbl_head.indexToLocFmt);
  GetInt16(stream, tbl_head.glyphDataFmt);
  result := tbl_head.magicNumber = $5F0F3CF5
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetTable_hhea: Boolean;
begin
  Result := (stream.Size > tables[tblIdx_hhea].offset +
    tables[tblIdx_hhea].length) and
    (tables[tblIdx_hhea].length >= SizeOf(TTtfTable_Hhea));
  if not Result then Exit;
  stream.Position := tables[tblIdx_hhea].offset;

  GetFixed(stream, tbl_hhea.version);
  GetInt16(stream, tbl_hhea.ascent);
  GetInt16(stream, tbl_hhea.descent);
  GetInt16(stream, tbl_hhea.lineGap);
  GetWord(stream, tbl_hhea.advWidthMax);
  GetInt16(stream, tbl_hhea.minLSB);
  GetInt16(stream, tbl_hhea.minRSB);
  GetInt16(stream, tbl_hhea.xMaxExtent);
  GetInt16(stream, tbl_hhea.caretSlopeRise);
  GetInt16(stream, tbl_hhea.caretSlopeRun);
  GetInt16(stream, tbl_hhea.caretOffset);
  GetUInt64(stream, tbl_hhea.reserved);
  GetInt16(stream, tbl_hhea.metricDataFmt);
  GetWord(stream, tbl_hhea.numLongHorMets);
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyphHorzMetrics(glyphIdx: integer;
  out tbl_htmx: TTtfTable_Htmx): Boolean;
begin
  Result := (tblIdx_hmtx > 0) and
    (stream.Size > tables[tblIdx_hmtx].offset + tables[tblIdx_hmtx].length);
  if not Result then Exit;
  if glyphIdx < tbl_hhea.numLongHorMets then
  begin
    stream.Position := Integer(tables[tblIdx_hmtx].offset) + glyphIdx * 4;
    GetWord(stream, tbl_htmx.advanceWidth);
    GetInt16(stream, tbl_htmx.leftSideBearing);
  end else
  begin
    stream.Position := Integer(tables[tblIdx_hmtx].offset) +
      Integer(tbl_hhea.numLongHorMets -1) * 4;
    GetWord(stream, tbl_htmx.advanceWidth);
    stream.Position := Integer(tables[tblIdx_hmtx].offset +
      tbl_hhea.numLongHorMets * 4) +
      2 * (glyphIdx - Integer(tbl_hhea.numLongHorMets));
    GetInt16(stream, tbl_htmx.leftSideBearing);
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyphInfo(glyphIdx: integer): TPathsEx;
var
  offset: cardinal;
begin
  result := nil;
  if tbl_head.indexToLocFmt = 0 then
  begin
    offset := tbl_loca_word[glyphIdx] *2;
    if offset = tbl_loca_word[glyphIdx+1] *2 then Exit; //no contours
  end else
  begin
    offset := tbl_loca_dword[glyphIdx];
    if offset = tbl_loca_dword[glyphIdx+1] then Exit; //no contours
  end;
  if offset >= tables[tblIdx_glyf].length then Exit;

  inc(offset, tables[tblIdx_glyf].offset);
  stream.Position := offset;

  GetInt16(stream, tbl_glyf.numContours);
  GetInt16(stream, tbl_glyf.xMin);
  GetInt16(stream, tbl_glyf.yMin);
  GetInt16(stream, tbl_glyf.xMax);
  GetInt16(stream, tbl_glyf.yMax);

  if tbl_glyf.numContours < 0 then
    result := GetCompositeGlyph else
    result := GetSimpleGlyph;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetSimpleGlyph: TPathsEx;
var
  i,j: integer;
  instructLen: WORD;
  flag, repeats: byte;
  contourEnds: TArrayOfWord;
begin
  SetLength(contourEnds, tbl_glyf.numContours);
  for i := 0 to High(contourEnds) do
    GetWord(stream, contourEnds[i]);

  //hints are currently ignored
  GetWord(stream, instructLen);
  stream.Position := stream.Position + instructLen;

  setLength(result, tbl_glyf.numContours);
  setLength(result[0], contourEnds[0] +1);
  for i := 1 to High(result) do
    setLength(result[i], contourEnds[i] - contourEnds[i-1]);

  repeats := 0;
  for i := 0 to High(result) do
  begin
    repeats := 0;
    for j := 0 to High(result[i]) do
    begin
      if repeats = 0 then
      begin
        GetByte(stream, flag);
        if flag and REPEAT_FLAG = REPEAT_FLAG then
          GetByte(stream, repeats);
      end else
        dec(repeats);
      result[i][j].flag := flag;
    end;
  end;
  GetPathCoords(result);
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
begin
  result := nil;
  flag := MORE_COMPONENTS;

  while (flag and MORE_COMPONENTS <> 0) do
  begin
    glyphIndex := 0;
    a := 0; b := 0; c := 0; d := 0; e := 0; f := 0;
    compoundIdx := 0; componentIdx := 0;

    GetWord(stream, flag);
    GetWord(stream, glyphIndex);

    if (flag and ARG_1_AND_2_ARE_WORDS <> 0) then
    begin
      GetInt16(stream, arg1i);
      GetInt16(stream, arg2i);
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
      GetShortInt(stream, arg1b);
      GetShortInt(stream, arg2b);
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
      Get2Dot14(stream, tmp);
      a := tmp; d := tmp;
    end
    else if (flag and WE_HAVE_AN_X_AND_Y_SCALE <> 0) then
    begin
      Get2Dot14(stream, tmp); a := tmp;
      Get2Dot14(stream, tmp); d := tmp;
    end
    else if (flag and WE_HAVE_A_TWO_BY_TWO <> 0) then
    begin
      Get2Dot14(stream, tmp); a := tmp;
      Get2Dot14(stream, tmp); b := tmp;
      Get2Dot14(stream, tmp); c := tmp;
      Get2Dot14(stream, tmp); d := tmp;
    end;

    tbl_glyf_old := tbl_glyf;

    streamPos := stream.Position;
    componentPaths := GetGlyphInfo(glyphIndex);
    stream.Position := streamPos;

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
      inc(tbl_glyf.numContours, tbl_glyf_old.numContours);
      tbl_glyf.xMin := Min(tbl_glyf.xMin, tbl_glyf_old.xMin);
      tbl_glyf.xMax := Max(tbl_glyf.xMax, tbl_glyf_old.xMax);
      tbl_glyf.yMin := Min(tbl_glyf.yMin, tbl_glyf_old.yMin);
      tbl_glyf.yMax := Max(tbl_glyf.yMax, tbl_glyf_old.yMax);
    end;

    AppendPathsEx(result, componentPaths);
  end;
end;
//------------------------------------------------------------------------------

procedure TTtfFontReader.GetPathCoords(var paths: TPathsEx);
var
  i,j: integer;
  xi,yi: Int16;
  flag, xb,yb: byte;
  pt: TPoint;
begin
  if tbl_glyf.numContours = 0 then Exit;

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
        GetByte(stream, xb);
        if (flag and X_DELTA) = 0 then
          pt.X := pt.X - xb else
          pt.X := pt.X + xb;
      end else
      begin
        if flag and X_DELTA = 0 then
        begin
          GetInt16(stream, xi);
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
        GetByte(stream, yb);
        if (flag and Y_DELTA) = 0 then
          pt.Y := pt.Y - yb else
          pt.Y := pt.Y + yb;
      end else
      begin
        if flag and Y_DELTA = 0 then
        begin
          GetInt16(stream, yi);
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

procedure AppendPathEx(var path: TPathEx; const pt: TPointEx);
var
  len: integer;
begin
  len := Length(path);
  SetLength(path, len+1);
  path[len] := pt;
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

function TTtfFontReader.GetGlyph(chr: Char; out nextX: integer): TPathsD;
var
  i,j, glyphIdx: integer;
  pt2: TPointEx;
  bez: TPathD;
  pathsEx: TPathsEx;
  tbl_htmx: TTtfTable_Htmx;
begin
  Result := nil;
  if stream.Size = 0 then Exit;

  glyphIdx := GetGlyphIdxFromCmapIdx(Ord(chr));
  GetGlyphHorzMetrics(glyphIdx, tbl_htmx);
  nextX := tbl_htmx.advanceWidth;

  pathsEx := GetGlyphInfo(glyphIdx); //gets raw splines
  pathsEx := ConvertSplinesToBeziers(pathsEx);
  if pathsEx = nil then Exit; //eg space character

  //now flatten ...
  setLength(result, length(pathsEx));
  for i := 0 to High(pathsEx) do
  begin
    SetLength(Result[i],1);
    Result[i][0] := pathsEx[i][0].pt;
    for j := 1 to High(pathsEx[i]) do
    begin
      if OnCurve(pathsEx[i][j].flag) then
      begin
        AppendPoint(Result[i], pathsEx[i][j].pt);
      end else
      begin
        if j = High(pathsEx[i]) then
          pt2 := pathsEx[i][0] else
          pt2 := pathsEx[i][j+1];
        bez := FlattenQBezier(pathsEx[i][j-1].pt,
          pathsEx[i][j].pt, pt2.pt);
        Result[i] := JoinPaths(Result[i], bez);
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetGlyph(const text: string;
  out nextX: integer): TPathsD;
var
  i, currX, len: integer;
  tmpPaths: TPathsD;
begin
  Result := nil;
  nextX := 0;
  len := Length(text);
  if (len = 0) or (stream.Size = 0) then Exit;
  for i := 1 to len do
  begin
    tmpPaths := GetGlyph(text[i], currX);
    if nextX > 0 then
      tmpPaths := OffsetPath(tmpPaths, nextX, 0);
    inc(nextX, currX);
    AppendPath(Result, tmpPaths);
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetFontInfo: TTtfFontInfo;
begin
  if stream.Size = 0 then
  begin
    result.name := '';
    result.subname := '';
    result.unitsPerEm := 0;
  end else
  begin
    result.name := fontName;
    result.subname := fontSubName;
    result.unitsPerEm := tbl_head.unitsPerEm;
    result.xMin := tbl_head.xMin;
    result.xMax := tbl_head.xMax;
    result.yMin := tbl_head.yMin;
    result.yMax := tbl_head.yMax;
  end;
end;
//------------------------------------------------------------------------------

function TTtfFontReader.GetLastGyphInfo: TTtfGyphMetrics;
begin
  if stream.Size = 0 then
    FillChar(result, sizeOf(Result), 0) else
    result := tbl_glyf;
end;
//------------------------------------------------------------------------------

end.
