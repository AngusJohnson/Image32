unit Image32_Text;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.53                                                            *
* Date      :  11 October 2020                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  Module to support text in the Image32 library                   *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  Windows, Types, SysUtils, Classes, Math,
{$IFDEF XPLAT_GENERICS}
  Generics.Collections, Generics.Defaults,
{$ENDIF}
  Image32, Image32_Draw;

const
  DEFAULT = -1;
type

{$IFNDEF UNICODE}
  UnicodeString = WideString;
{$ENDIF}

{$IFDEF ZEROBASEDSTR}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

  //TGlyphInfo: Object that's used internally be TFontInfo
  //Note - glyph outlines always use the NON-ZERO fill rule
  //https://docs.microsoft.com/en-us/typography/opentype/spec/glyf
  TGlyphInfo = class
    wc: WideChar;
    gm: TGlyphMetrics;
    paths: TPathsD;
    bounds: TRectD;
  end;

  //TFontInfo: Object that stores the information needed for drawing text of
  //a specified LogFont. The creation and destruction of TFontInfo objects is
  //handled by FontManager.
  TFontInfo = class
  private
    fAccessTime   : TDatetime;
    fHandle       : HFont;
    fLogFont      : TLogFont;
    fFontSortStr  : UnicodeString;
    fGlyphsSorted : Boolean;
    {$IFDEF XPLAT_GENERICS}
    fGlyphs       : TList<TGlyphInfo>;
    {$ELSE}
    fGlyphs       : TList;
    {$ENDIF}
    procedure SortGlyphs;
    function GetUnderLined: Boolean;
    procedure SetUnderlined(value: Boolean);
    function GetStrikeThrough: Boolean;
    procedure SetStrikeThrough(value: Boolean);
    function GetFontName: string;
    function GetGlyphInfo(wc: WideChar): TGlyphInfo;
    //AddGlyph: note that Windows.GetGlyphOutline() only works for
    //UCS-2 chars. UCS-4 characters are managed by the OS using font mapping
    procedure AddGlyph(wc: WideChar; memDc: HDC);
    function GetMissingChars(const s: UnicodeString): UnicodeString;
    procedure FillMissingChars(const ws: UnicodeString);
  public
    constructor Create(const logfont: TLogfont; InitAnsi: boolean = true);
    destructor Destroy; override;
    function MeasureText(const text: UnicodeString): TPointD;
    property LogFont: TLogFont read fLogFont;
    property Handle: HFont read fHandle;
    property Fontname: string read GetFontName;
    property GlyphInfo[wc: WideChar]: TGlyphInfo read GetGlyphInfo;
    property LastAccessed: TDatetime read fAccessTime write fAccessTime;
    property UnderLined: Boolean read GetUnderLined write SetUnderLined;
    property StrikeThrough: Boolean read GetStrikeThrough write SetStrikeThrough;
  end;

  //TFontManager: A single instance class that manages the creation and
  //destruction of TFontInfo objects which are used in text rendering.
  TFontManager = class
  private
    fCapacity  : integer;
    fSorted    : Boolean;
    {$IFDEF XPLAT_GENERICS}
    fFontInfos : TList<TFontInfo>;
    {$ELSE}
    fFontInfos : TList;
    {$ENDIF}
    fInstalledFonts: TStringList;
    function GetInstalledFonts: TStringList;
    procedure SetCapacity(capacity: integer);
    procedure Sort;
    function IndexOfOldest: integer;
    function AddFontInternal(const logFont: TLogFont; InitAnsi: boolean): TFontInfo;
  protected
    function AddFont(const logFont: TLogFont; InitAnsi: boolean = true): TFontInfo;
  public
    constructor Create(capacity: integer = 50);
    destructor Destroy; override;
    //GetPreferredFaceName: Returns the first facename listed in faceNameList
    //that's also listed in InstalledFonts. (FaceNameList must be a comma
    //separated list of one or more font names.)
    function GetPreferredFaceName(const faceNameList: string): string;
    //GetFontInfo: Returns a TFontInfo object that matches the supplied
    //logFont.<br> If AutoAdd is true and no match is found in the current
    //list of TFontInfo objects, then the fontManager will create a new
      //TFontInfo object to match the supplied logFont. Otherwise when no match
    //is found, the function will return nil.<br>
    //Also, it's much more efficient the derive path info for ranges of
    //characters, rather than deriving this info for individual characters.
    //So when InitAnsi is true, the FontManager will get path infor for all
    //characters in the range 32 .. 126;
    function GetFontInfo(const logFont: TLogFont; AutoAdd: Boolean = true;
      InitAnsi: boolean = true): TFontInfo; overload;
    function GetFontInfo(font: hFont; AutoAdd: Boolean = true;
      InitAnsi: boolean = true): TFontInfo; overload;
    //Clear: Deletes all stored TFontInfo object;
    procedure Clear;
    //Capacity: If left unchecked, the list of TFontInfos in font manager (FM)
    //could get very large and could slow could slow performance. Capacity
    //provides a limit to the number of active TFontInfo objects stored by FM
    //(the default Capacity = 50). When the FM reaches this limit, TFontInfo
    //objects with the longest time since last access will be removed first.
    property Capacity: integer read fCapacity write SetCapacity;
    //InstalledFonts: Lists all the fonts currently installed in Windows.
    property InstalledFonts: TStringList read GetInstalledFonts;
  end;

  TTextAlign = (taLeft, taRight, taCenter, taJustify);
  TTextVAlign = (tvaTop, tvaMiddle, tvaBottom);

  function GetTextOutline(x, y: double; const text: UnicodeString;
    fontInfo: TFontInfo; textAlign: TTextAlign; out textEndPos: TPointD;
    justifySpc: double = 0): TPathsD;

  function DrawText(image: TImage32; x,y: double; const text: UnicodeString;
    fontInfo: TFontInfo = nil; textAlign: TTextAlign = taLeft;
    textColor: TColor32 = clBlack32; justifySpc: double = 0): TPointD; overload;

  function DrawText(image: TImage32; x,y: double; const text: UnicodeString;
    fontInfo: TFontInfo; textAlign: TTextAlign; renderer: TCustomRenderer;
    justifySpc: double = 0): TPointD; overload;

  function DrawText_ClearType(image: TImage32; x,y: double;
    const text: UnicodeString; fontInfo: TFontInfo = nil;
    textAlign: TTextAlign = taLeft; textColor:
    TColor32 = clBlack32; opaqueBkColor: TColor32 = clWhite32;
    justifySpc: double = 0): TPointD;

  function GetTextAlongPathOutine(image: TImage32;
    const text: UnicodeString; const path: TPathD; fontInfo: TFontInfo;
    textAlign: TTextAlign; vertOffset: integer = 0;
    charSpacing: double = 0): TPathsD;

  function DrawWrappedText(image: TImage32; const rec: TRect;
    const text: UnicodeString; fontInfo: TFontInfo;
    textAlign: TTextAlign = taJustify; textAlignV: TTextVAlign = tvaTop;
    textColor: TColor32 = clBlack32;
    lineSpacing: double = DEFAULT; paraSpacing: double = DEFAULT): TPointD;

  function DrawWrappedText_LCD(image: TImage32; const rec: TRect;
    const text: UnicodeString; fontInfo: TFontInfo;
    textAlign: TTextAlign = taJustify; textAlignV: TTextVAlign = tvaTop;
    textColor: TColor32 = clBlack32;
    lineSpacing: double = DEFAULT; paraSpacing: double = DEFAULT): TPointD;

  //MeasureText: Returns the bounding rectangle of the text.<br>
  //MeasureText also returns via 'charOffsets' the offset for the start
  //of each character PLUS the offset of the next anticipated character.<br>
  //Hence Length(charPos) = Length(text) +1;
  procedure MeasureText(const text: UnicodeString;
    fontInfo: TFontInfo; out bounds: TRectD); overload;
  procedure MeasureText(const text: UnicodeString; fontInfo: TFontInfo;
    out bounds: TRectD; out charOffsets: TPathD); overload;

  //GetFontSize: Returns the font's 'point size' (DPI independant)
  function GetFontSize(logFont: TLogFont): integer;
  //GetFontHeight: Returns the font's height in (positive) 'logical units'
  function GetFontHeight(logFont: TLogFont): integer;
  //CheckFontHeight: Converts logFont.lfHeight from 'point size' into
  //'logical units' (ie returned logFont.lfHeight will always be negative).
  procedure CheckFontHeight(var logFont: TLogFont);
  //GetLogFontFromHFont: Returns a LogFont matching the supplied font handle.
  function GetLogFontFromHFont(font: HFont): TLogFont;

  function GetDefaultFontInfo: TFontInfo;
  function GetFontInfo(const lf: TLogFont): TFontInfo;
var
  DefaultLogfont : TLogfont;
  FontManager: TFontManager;

implementation

uses
  Image32_Vector;

resourcestring
   rsPostscriptNotSupported = 'Postscript fonts are not supported';

type
  TMeasureTextPrefer = (mtPreferNone, mtPreferX, mtPreferY);

const
  GGO_BEZIER        = $3;
  GGO_UNHINTED      = $100;
  TT_PRIM_CSPLINE   = $3;
  FixedMul          = $10000;
  DivFixed: double  = 1/FixedMul;

  vert_flip_mat2: TMat2 =
    (eM11:(fract: 0; value: 1); eM12:(fract: 0; value: 0);
     eM21:(fract: 0; value: 0); eM22:(fract: 0; value: -1));

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function GetFontSize(logFont: TLogFont): integer;
const
  _72Div96 = 72/96;
begin
  if logFont.lfHeight > 0 then result := logFont.lfHeight
  else result := Round(DpiAware(-logFont.lfHeight * _72Div96));
end;
//------------------------------------------------------------------------------

function GetFontHeight(logFont: TLogFont): integer;
const
  _96Div72 = 96/72;
begin
  if logFont.lfHeight < 0 then result := -logFont.lfHeight
  else result := Round(DpiAware(logFont.lfHeight * _96Div72));
end;
//------------------------------------------------------------------------------

procedure CheckFontHeight(var logFont: TLogFont);
const
  _96Div72 = 96/72;
begin
  if logFont.lfHeight > 0 then
    logFont.lfHeight := -Round(DpiAware(logFont.lfHeight * _96Div72));
end;
//------------------------------------------------------------------------------

function PointFxToPointD(const fxpt: TPointFX): TPointD;
begin
  Result.X := fxpt.X.Value + fxpt.X.Fract * DivFixed;
  Result.Y := fxpt.Y.Value + fxpt.Y.Fract * DivFixed;
end;
//------------------------------------------------------------------------------

function GetLogFontFromHFont(font: HFont): TLogFont;
begin
  GetObject(font, Sizeof(TLogFont), @Result);
end;
//------------------------------------------------------------------------------

function ParseFontCharInfo(info: PByte; infoSize: cardinal): TPathsD;
var
  i, cnt: integer;
  buff: TPathD;
  buffCnt, buffLen: integer;
  endInfo, endContour:  PByte;
  p1,p2,p3,hp: TPointD;

  procedure AddPoint(const pt: TPointD);
  begin
    if buffCnt = buffLen then
    begin
      inc(buffLen, 128);
      SetLength(buff, buffLen);
    end;
    buff[buffCnt] := pt;
    inc(buffCnt);
  end;

  procedure FinalizeBuffer;
  var
    len: integer;
  begin
    if buffCnt = 0 then Exit;
    SetLength(buff, buffCnt);
    len := length(result);
    setlength(Result, len+1);
    Result[len] := buff;
    buff := nil;
    buffCnt := 0;
    buffLen := 0;
  end;

  procedure QuadCurve(const P1, P2, P3: TPointD);
  const
    QBezierTolerance  = 0.20;
  var
    P12, P23, P123: TPointD;
  begin
    if Abs(P1.x + P3.x - 2 * P2.x) +
      Abs(P1.y + P3.y - 2 * P2.y) < QBezierTolerance then
        AddPoint(P1)
    else
    begin
      P12.X := (P1.X + P2.X) * 0.5;
      P12.Y := (P1.Y + P2.Y) * 0.5;
      P23.X := (P2.X + P3.X) * 0.5;
      P23.Y := (P2.Y + P3.Y) * 0.5;
      P123.X := (P12.X + P23.X) * 0.5;
      P123.Y := (P12.Y + P23.Y) * 0.5;
      QuadCurve(P1, P12, P123);
      QuadCurve(P123, P23, P3);
    end;
  end;

begin
  result := nil;
  buffCnt := 0;
  buffLen := 0;
  endInfo := info + infoSize;
  while Info < endInfo do
  begin
    with PTTPolygonHeader(info)^ do
    begin
      if dwType <> TT_POLYGON_TYPE then Exit;
      endContour := info + cb;
      FinalizeBuffer;
      AddPoint(PointFxToPointD(pfxStart));
      inc(info, SizeOf(TTTPolygonHeader));
    end;

    while info < endContour do
      with PTTPolyCurve(info)^ do
      begin
        cnt := cpfx;
        inc(info, SizeOf(Word)*2);
        case wType of
          TT_PRIM_LINE:
            for i := 1 to cnt do
            begin
              AddPoint(PointFxToPointD(PPointfx(info)^));
              inc(info, sizeOf(TPointfx));
            end;
          TT_PRIM_QSPLINE:
            begin
              p1 := buff[buffCnt-1];
              p2 := PointFxToPointD(PPointfx(info)^);
              inc(info, sizeOf(TPointfx));
              for i := 1 to cnt - 1 do
              begin
                p3 := PointFxToPointD(PPointfx(info)^);
                inc(info, sizeOf(TPointfx));
                if i < cnt - 1 then
                begin
                  hp.X := (P2.X + P3.X) * 0.5;
                  hp.Y := (P2.Y + P3.Y) * 0.5;
                end
                else
                  hp := p3;
                QuadCurve(p1, p2, hp);
                p1 := hp; p2 := p3;
              end;
            end;
          TT_PRIM_CSPLINE:
              raise Exception.Create(rsPostscriptNotSupported);

          else Exit; //oops
        end;
      end;
  end;
  FinalizeBuffer;
end;
//------------------------------------------------------------------------------

function GetPathsForChar(memDC: HDC;
  wc: WideChar; out metrics: TGlyphMetrics): TPathsD;
var
  size: DWord;
  info:  PByte;
begin
  FillChar(metrics, SizeOf(metrics), 0);
  size := GetGlyphOutline(memDC, cardinal(wc),
    GGO_NATIVE or GGO_UNHINTED, metrics, 0, nil, vert_flip_mat2);
  if (size = GDI_ERROR) or (size = 0) then
  begin
    if (size = GDI_ERROR) then
      FillChar(metrics, SizeOf(metrics), 0);
    result := nil;
    exit;
  end;

  GetMem(info, size);
  try
    if GetGlyphOutline(memDC, cardinal(wc), GGO_NATIVE or GGO_UNHINTED,
      metrics, size, info, vert_flip_mat2) <> GDI_ERROR then
        Result := ParseFontCharInfo(info, size);
  finally
    FreeMem(info);
  end;
end;
//------------------------------------------------------------------------------

function GetTextOutline(x, y: double; const text: UnicodeString;
  fontInfo: TFontInfo; textAlign: TTextAlign;
  out textEndPos: TPointD; justifySpc: double): TPathsD;
var
  i, len: integer;
  dx, lineWidth: double;
  underline, strikethrough: TPathD;
  paths2: TPathsD;
  glyphInfo: TGlyphInfo;
  bounds: TRectD;
  charPos, norms: TPathD;
  startPos: TPointD;
  ulOffset: double;
begin
  Result := nil;

  len := length(text);
  if (len = 0) then Exit;
  if fontInfo = nil then
    fontInfo := FontManager.GetFontInfo(DefaultLogfont);

  fontInfo.FillMissingChars(text);
  MeasureText(text, fontInfo, bounds, charPos);
  textEndPos := charPos[len];
  case textAlign of
    taRight:
      begin
        x := x - textEndPos.X;
        y := y - textEndPos.Y;
      end;
    taCenter:
      begin
        x := x - textEndPos.X/2 ;
        y := y - textEndPos.Y/2;
      end;
  end;
  startPos := PointD(x,y);
  if textAlign = taRight then
  begin
    textEndPos := startPos;
  end else
  begin
    textEndPos.X := x + textEndPos.X;
    textEndPos.Y := y + textEndPos.Y;
  end;

  dx := 0;
  for i := 1 to len do
  begin
    glyphInfo := fontInfo.GlyphInfo[text[i]];
    if not assigned(glyphInfo) then Continue;
    paths2 := glyphInfo.paths;
    if length(paths2) > 0 then
    begin
      paths2 := OffsetPath(paths2, startPos.X, startPos.Y);
      AppendPath(Result, paths2);
    end;
    if (justifySpc > 0) and (text[i] = #32) then
      dx := dx + justifySpc;
    startPos.X := x + dx + charPos[i].X;
    startPos.Y := y + charPos[i].Y;
  end;

  //underline & strikethrough ...
  if not assigned(fontInfo) or
    (not fontInfo.UnderLined and not fontInfo.StrikeThrough) then Exit;

  lineWidth := GetFontHeight(fontInfo.LogFont)/36;

  setLength(norms, 2);
  norms[0] := GetUnitNormal(PointD(x,y), startPos);
  norms[1] := PointD(-norms[0].X, -norms[0].Y);
  if fontInfo.StrikeThrough then
  begin
    SetLength(strikethrough, 2);
    strikethrough[0] := PointD(x,y);
    strikethrough[1] := startPos;

    ulOffset := lineWidth * 8;
    strikethrough[0] := OffsetPoint(strikethrough[0],
      ulOffset * norms[0].X, ulOffset * norms[0].Y);
    strikethrough[1] := OffsetPoint(strikethrough[1],
      ulOffset * norms[0].X, ulOffset * norms[0].Y);

    len := length(result);
    strikethrough := Grow(strikethrough, norms, lineWidth, jsMiter, 2);
    SetLength(result, len +1);
    result[len] := strikethrough;
  end;

  if fontInfo.UnderLined then
  begin
    SetLength(underline, 2);
    underline[0] := PointD(x,y);
    underline[1] := startPos;

    ulOffset := -lineWidth * 3 -2;
    underline[0] := OffsetPoint(underline[0],
      ulOffset * norms[0].X, ulOffset * norms[0].Y);
    underline[1] := OffsetPoint(underline[1],
      ulOffset * norms[0].X, ulOffset * norms[0].Y);
    len := length(result);
    underline := Grow(underline, norms, lineWidth, jsMiter, 2);
    SetLength(result, len +1);
    result[len] := underline;
  end;

end;
//------------------------------------------------------------------------------

function CountSpaces(const text: UnicodeString): integer;
  {$IFDEF INLINE} inline; {$ENDIF}
var
  i: integer;
begin
  result := 0;
  for i := 1 to Length(text) do if text[i] = #32 then inc(Result);
end;
//------------------------------------------------------------------------------

function DrawText(image: TImage32;
  x,y: double; const text: UnicodeString; fontInfo: TFontInfo;
  textAlign: TTextAlign; textColor: TColor32; justifySpc: double): TPointD;
var
  paths: TPathsD;
begin
  if (text = '') or (textColor shr 24 = 0) or image.IsEmpty then Exit;
  paths := GetTextOutline(x,y, text, fontInfo, textAlign, result, justifySpc);
  DrawPolygon(image, paths, frNonZero, textColor);
end;
//------------------------------------------------------------------------------

function DrawText(image: TImage32; x,y: double; const text: UnicodeString;
  fontInfo: TFontInfo; textAlign: TTextAlign;
  renderer: TCustomRenderer; justifySpc: double = 0): TPointD;
var
  paths: TPathsD;
begin
  if (text = '') or image.IsEmpty or not assigned(renderer) then
  begin
    Result := PointD(x,y);
    Exit;
  end;
  paths := GetTextOutline(x,y, text, fontInfo, textAlign, result, justifySpc);
  DrawPolygon(image, paths, frNonZero, renderer);
end;
//------------------------------------------------------------------------------

function DrawWrappedTextInternal(image: TImage32; const rec: TRect;
  const text: UnicodeString; fontInfo: TFontInfo; lcd: Boolean;
  textAlign: TTextAlign; textAlignV: TTextVAlign;
  textColor: TColor32; lineSpacing: double; paraSpacing: double): TPointD;
var
  bounds: TRectD;
  charPos: TPathD;
  fontHeight, x,y, dy: double;
  len: integer;

  function FindLineBreakIdx(idx: integer; width: double): integer;
  var
    wordBreakIdx: integer;
    dx: double;
  begin
    result := idx; //one based string index
    width := width + charPos[Result-1].X;
    wordBreakIdx := idx;
    while (result <= len) do
    begin
      if text[result] = #10 then Exit;
      dx := charPos[Result].X; //nb: charEndPos is zero base array
      if dx > width then Break;
      if text[result] = #32 then wordBreakIdx := Result;
      Inc(Result);
    end;
    //either all finished
    if (result > len) then Exit;
    //or the line needs breaking somewhere
    if (wordBreakIdx > idx) then Result := wordBreakIdx
    else dec(Result); //no space found for an obvious break
  end;

  function WrapText(measureOnly, lcd: Boolean): TPointD;
  var
    i, curr, spacesCnt: integer;
    textLine: UnicodeString;
    textWidth, justifySpace: double;
  begin
    curr := 1;
    i := 1;
    len := Length(text);
    while (curr <= len) and (y < rec.Bottom) do
    begin
      curr := FindLineBreakIdx(i, RectWidth(rec));
      if curr <= i then break; //all finished
      textLine := Trim(copy(text,i,curr-i+1));

      //get justify spacing
      justifySpace := 0;
      if (textAlign = taJustify) and
        (curr <= len) and (text[curr] <> #10) then
      begin
        textWidth := charPos[curr].X - charPos[i-1].X;
        spacesCnt := CountSpaces(textLine);
        if spacesCnt > 0 then
          justifySpace := (RectWidth(rec) - textWidth)/spacesCnt;
      end;

      if not measureOnly then
      begin
        if lcd then
          Result := DrawText_ClearType(image, x, y, textLine,
            fontInfo, textAlign, textColor, clWhite32, justifySpace)
        else
          Result := DrawText(image, x, y,
            textLine, fontInfo, textAlign, textColor, justifySpace);
      end;

      //vertical offset - either line of paragraph
      if (curr < len) and (text[curr] = #10) then
        y := y + dy + paraSpacing else
        y := y + dy;

      //and prepare for the next line
      inc(curr);
      i := curr;
    end;
  end;

begin
  case textAlign of
    taCenter : x := (rec.Left + rec.Right) / 2;
    taRight : x := rec.Right;
    else x := rec.Left;
  end;

  if (text = '') or (textColor shr 24 = 0) or image.IsEmpty then
  begin
    Result := PointD(x, Rec.Top);
    Exit;
  end;

  if fontInfo = nil then
    fontInfo := FontManager.GetFontInfo(DefaultLogfont);

  fontHeight := GetFontHeight(fontInfo.LogFont);
  if lineSpacing < 0 then lineSpacing := fontHeight * 0.3333;
  if paraSpacing < 0 then paraSpacing := lineSpacing;
  MeasureText(text, fontInfo, bounds, charPos);

  dy := fontHeight + lineSpacing;
  y := rec.Top + dy;

  case textAlignV of
    tvaMiddle:
      begin
        WrapText(true, lcd); //ie just measuring vertical offset
        if y >= rec.Bottom then
          y := rec.Top + dy else
          y := rec.Top + dy + (rec.Bottom - y)/2;
      end;
    tvaBottom:
      begin
        WrapText(true, lcd); //ie just measuring vertical offset
        if y >= rec.Bottom then
          y := rec.Top + dy else
          y := rec.Top + dy + (rec.Bottom - y);
      end;
  end;
  Result := WrapText(false, lcd);      //drawing text here

end;
//------------------------------------------------------------------------------

function DrawWrappedText(image: TImage32; const rec: TRect;
  const text: UnicodeString; fontInfo: TFontInfo;
  textAlign: TTextAlign; textAlignV: TTextVAlign;
  textColor: TColor32; lineSpacing: double; paraSpacing: double): TPointD;
begin
  Result := DrawWrappedTextInternal(image, rec, text, fontInfo,
    false, textAlign, textAlignV, textColor, lineSpacing, paraSpacing);
end;
//------------------------------------------------------------------------------

function DrawWrappedText_LCD(image: TImage32; const rec: TRect;
  const text: UnicodeString; fontInfo: TFontInfo;
  textAlign: TTextAlign; textAlignV: TTextVAlign; textColor: TColor32;
  lineSpacing: double; paraSpacing: double): TPointD;
begin
  Result := DrawWrappedTextInternal(image, rec, text, fontInfo,
    true, textAlign, textAlignV, textColor, lineSpacing, paraSpacing);
end;
//------------------------------------------------------------------------------

function DrawText_ClearType(image: TImage32; x,y: double;
  const text: UnicodeString; fontInfo: TFontInfo; textAlign: TTextAlign;
  textColor: TColor32; opaqueBkColor: TColor32; justifySpc: double): TPointD;
var
  paths: TPathsD;
begin
  Result := PointD(x,y);
  if (text = '') or image.IsEmpty then Exit;

  paths := GetTextOutline(x,y, text, fontInfo, textAlign, result, justifySpc);
  if not assigned(paths) then Exit;
  DrawPolygon_ClearType(image, paths, frNonZero, textColor, opaqueBkColor);
end;
//------------------------------------------------------------------------------

type
  TPathInfo = record
    pt     : TPointD;
    vector : TPointD;
    angle  : Double;
    dist   : double;
  end;
  TPathInfos = array of TPathInfo;

function GetTextAlongPathOutine(image: TImage32;
  const text: UnicodeString; const path: TPathD; fontInfo: TFontInfo;
  textAlign: TTextAlign; vertOffset: integer = 0;
  charSpacing: double = 0): TPathsD;
var
  i, textLen, pathLen: integer;
  left, center, center2, dist, dx: double;
  glyph: TGlyphInfo;
  pathInfo: TPathInfo;
  pathInfos: TPathInfos;
  pt, rotatePt: TPointD;
  tmpPaths: TPathsD;
const
  TwoPi = Pi * 2;

  function GetPathInfo(offset: double): TPathInfo;
  var
    i: integer;
  begin
    i := 0;
    while i < pathLen - 2 do
    begin
      if pathInfos[i+1].dist > offset then break;
      inc(i);
    end;
    Result := pathInfos[i];
    if Result.angle >= 0 then Exit;
    //initialize the remaining fields (ie other then 'dist')
    Result.angle  := -GetAngle(path[i], path[i+1]);
    Result.vector := GetUnitVector(path[i], path[i+1]);
    Result.pt     := path[i];
  end;

  function GetTextWidth: double;
  var
    i: integer;
  begin
    result := -charSpacing;
    for i := 1 to textLen do
      Result := Result +
        fontInfo.GetGlyphInfo(text[i]).gm.gmCellIncX + charSpacing;
  end;

begin
  Result := nil;
  pathLen := Length(path);
  textLen := Length(text);
  setLength(pathInfos, pathLen);
  if (pathLen < 2) or (textLen = 0) then Exit;

  dist := 0;
  for i:= 0 to pathLen -2 do
  begin
    pathInfos[i].angle := -1; //flags fields other than dist as uninitialized
    pathInfos[i].dist := dist;
    dist := dist + Distance(path[i], path[i+1]);
  end;
  pathInfos[pathLen -1].dist := dist;

  case textAlign of
    taCenter: Left := (dist - GetTextWidth) * 0.5;
    taRight : Left := dist - GetTextWidth;
    else      Left := 0;
  end;

  Result := nil;
  for i := 1 to textLen do
  begin
    glyph := fontInfo.GetGlyphInfo(text[i]);
    center := glyph.bounds.Width * 0.5;
    center2 := left + center;
    pathInfo := GetPathInfo(center2);
    rotatePt := PointD(center, -vertOffset);
    tmpPaths := RotatePath(glyph.paths, rotatePt, pathInfo.angle);
    dx := center2 - pathInfo.dist;
    pt.X := pathInfo.pt.X + pathInfo.vector.X * dx - rotatePt.X;
    pt.Y := pathInfo.pt.Y + pathInfo.vector.Y * dx - rotatePt.Y;

    tmpPaths := OffsetPath(tmpPaths, pt.X, pt.Y);
    AppendPath(Result, tmpPaths);
    left := left + glyph.gm.gmCellIncX + charSpacing;
  end;
end;
//------------------------------------------------------------------------------

procedure MeasureText(const text: UnicodeString; fontInfo: TFontInfo; out bounds: TRectD);
var
  charOffsets: TPathD;
begin
  MeasureText(text, fontInfo, bounds, charOffsets);
end;
//------------------------------------------------------------------------------

procedure MeasureText(const text: UnicodeString; fontInfo: TFontInfo;
  out bounds: TRectD; out charOffsets: TPathD);
var
  i, len: integer;
  delta, x,y: double;
  glyphInfo: TGlyphInfo;
  rec: TRectD;
  mtp: TMeasureTextPrefer;
  ws: UnicodeString;
begin
  ws := UnicodeString(text);
  len := Length(ws);
  setLength(charOffsets, len+1);
  bounds := RectD(0, 0, 0, 0);
  if len = 0 then EXit;
  charOffsets[0] := PointD(0,0);
  if fontInfo = nil then
    fontInfo := FontManager.GetFontInfo(DefaultLogfont);
  fontInfo.FillMissingChars(ws);

  //Positioning angled text is tricky since it requires working around the
  //integer values gmCellIncX and gmCellIncY which create cummulative
  //rounding errors. In spite of the workaround below, characters will still
  //vary from the baseline by as much as +/- 0.5px
  i := fontInfo.LogFont.lfEscapement;
  if (i = 0) or (Abs(i) = 900) or (Abs(i) = 1800) then
    delta := 0 else
    delta := tan(i * pi /1800);

  if delta = 0 then mtp := mtPreferNone
  else if abs(delta) < 1 then mtp := mtPreferX
  else mtp := mtPreferY;

  x := 0; y := 0;
  for i := 1 to len do
  begin
    glyphInfo := fontInfo.GlyphInfo[ws[i]];
    if not assigned(glyphInfo) then
    begin
      charOffsets[i] := charOffsets[i-1];
      Continue;
    end;
    rec := glyphInfo.bounds;
    OffsetRect(rec, x, y);
    bounds := UnionRect(bounds, rec);
    case mtp of
      mtPreferNone:
        begin
          x := x + glyphInfo.gm.gmCellIncX;
          y := y - glyphInfo.gm.gmCellIncY;
        end;
      mtPreferX:
        begin
          x := x + glyphInfo.gm.gmCellIncX;
          y := y - (delta * glyphInfo.gm.gmCellIncX);
        end;
      mtPreferY:
        begin
          x := x + (delta * glyphInfo.gm.gmCellIncY);
          y := y - glyphInfo.gm.gmCellIncY;
        end;
    end;
    charOffsets[i].X := x;
    charOffsets[i].Y := y;
  end;
end;
//------------------------------------------------------------------------------

function GetDefaultFontInfo: TFontInfo;
begin
  result := FontManager.GetFontInfo(DefaultLogfont);
end;
//------------------------------------------------------------------------------

function GetFontInfo(const lf: TLogFont): TFontInfo;
begin
  result := FontManager.GetFontInfo(lf);
end;
//------------------------------------------------------------------------------

function LogFontToSearchStr(const lf: TLogfont): UniCodeString;
var
  i,len: integer;
begin
  result := '';
  i := 1;
  while (i < 32) and (lf.lfFaceName[i] <> #0) do inc(i);
  len := 28 + i*2;
  SetLength(result, len div 2);
  Move(lf, Result[1], len);
end;
//------------------------------------------------------------------------------

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  sl: TStringList;
begin
  sl := TStringList(Data);
  sl.Add(LogFont.lfFaceName);
  Result := 1;
end;
//------------------------------------------------------------------------------

function GetCommaOffsets(const text: string): TArrayOfInteger;
var
  i,j, len: integer;
begin
  len := Length(text);
  SetLength(result, len);
  j := 0;
  for i := 1 to len do
  begin
    if text[i] <> ',' then Continue;
    Result[j] := i;
    inc(j);
  end;
  SetLength(Result, j);
end;
//------------------------------------------------------------------------------

function SplitCommaSeparatedString(const csv: string): TArrayOfString;
var
  i,j, len: integer;
  commaPos: TArrayOfInteger;
begin
  commaPos := GetCommaOffsets(csv);
  len := length(commaPos) +1;
  setLength(commaPos, len);
  commaPos[len-1] := length(csv) +1;
  setLength(Result, len);
  j := 1;
  for i := 0 to len -1 do
  begin
    Result[i] := Trim(Copy(csv, j, commaPos[i] - j));
    j := commaPos[i] + 1;
  end;
end;
//------------------------------------------------------------------------------

procedure InitDefaultLogfont;
var
  facename: string;
begin
  facename := FontManager.GetPreferredFaceName('Arial, Noto Sans, Liberation Sans');
  FillChar(DefaultLogfont, sizeof(DefaultLogfont), 0);
  with DefaultLogfont do
  begin
    lfHeight := 10;
    lfWeight := FW_NORMAL;
    lfCharSet := DEFAULT_CHARSET;
    lfOutPrecision := OUT_OUTLINE_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := PROOF_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    Move(facename[1], lfFaceName[0], length(facename) * SizeOf(Char));
  end;
end;
//------------------------------------------------------------------------------

procedure CreateFontManager;
begin
  FontManager := TFontManager.Create;
  InitDefaultLogfont;
  FontManager.AddFont(DefaultLogfont);
end;

//------------------------------------------------------------------------------
// TFontInfo class
//------------------------------------------------------------------------------

function TFontInfo.GetFontName: string;
begin
  result := fLogFont.lfFaceName;
end;
//------------------------------------------------------------------------------

function TFontInfo.GetUnderLined: Boolean;
begin
  result := fLogFont.lfUnderline <> 0;
end;
//------------------------------------------------------------------------------

procedure TFontInfo.SetUnderlined(value: Boolean);
begin
  if value then
    fLogFont.lfUnderline := 1 else
    fLogFont.lfUnderline := 0;
end;
//------------------------------------------------------------------------------

function TFontInfo.GetStrikeThrough: Boolean;
begin
  result := fLogFont.lfStrikeOut <> 0;
end;
//------------------------------------------------------------------------------

procedure TFontInfo.SetStrikeThrough(value: Boolean);
begin
  if value then
    fLogFont.lfStrikeOut := 1 else
    fLogFont.lfStrikeOut := 0;
end;
//------------------------------------------------------------------------------

{$IFDEF XPLAT_GENERICS}
function FindGlyph(wc: WideChar; glyphList: TList<TGlyphInfo>): integer;
{$ELSE}
function FindGlyph(wc: WideChar; glyphList: TList): integer;
{$ENDIF}
var
  i,l,r: integer;
begin
  //binary search the sorted list ...
  l := 0;
  r := glyphList.Count -1;
  while l <= r do
  begin
    Result := (l + r) shr 1;
    i := Ord(TGlyphInfo(glyphList[Result]).wc) - Ord(wc);
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

function GlyphSorter(glyph1, glyph2: pointer): integer;
begin
  Result := Ord(TGlyphInfo(glyph1).wc) - Ord(TGlyphInfo(glyph2).wc);
end;
//------------------------------------------------------------------------------

procedure TFontInfo.SortGlyphs;
begin
  if fGlyphsSorted then Exit;
  {$IFDEF XPLAT_GENERICS}
    fGlyphs.Sort(TComparer<TGlyphInfo>.Construct(
      function (const glyph1, glyph2: TGlyphInfo): integer
      begin
        Result := Ord(glyph1.wc) - Ord(glyph2.wc);
      end));
  {$ELSE}
    fGlyphs.Sort(GlyphSorter);
  {$ENDIF}
  fGlyphsSorted := true;
end;
//------------------------------------------------------------------------------

function TFontInfo.GetMissingChars(const s: UnicodeString): UnicodeString;
var
  i,j,r,len: integer;
begin
  len := Length(s);
  result := ''; r := 0;
  if (fHandle = 0) then Exit;

  if not fGlyphsSorted then SortGlyphs;
  Result := Copy(s, 1, len);
  for i := 1 to length(Result) do
  begin
    if Result[i] = #0 then Continue
    else if FindGlyph(Result[i], fGlyphs) < 0 then
    begin
      inc(r);
      Result[r] := Result[i];
      for j := i +1 to len do
          if Result[j] = Result[i] then Result[j] := #0;
    end;
  end;
  SetLength(Result, r);
end;
//------------------------------------------------------------------------------

procedure TFontInfo.FillMissingChars(const ws: UnicodeString);
var
  i,j: integer;
  memDC: HDC;
  oldFont: HFont;
  ordinals: TArrayOfWord;
  missing: UnicodeString;
  ordinal: Word;
begin
  missing := GetMissingChars(ws);
  if (missing = '') or (fHandle = 0) then Exit;

  memDC := CreateCompatibleDC(0);
  oldFont := SelectObject(memDC, fHandle);
  try
    for i := 1 to Length(missing) do
      if FindGlyph(missing[i], fGlyphs) < 0 then
        AddGlyph(missing[i], memDc);
  finally
    SelectObject(memDC, oldFont);
    DeleteDC(memDC);
  end;
  SortGlyphs;
end;
//------------------------------------------------------------------------------

function TFontInfo.MeasureText(const text: UnicodeString): TPointD;
var
  bounds: TRectD;
  dummy: TPathD;
begin
  Image32_Text.MeasureText(text, self, bounds, dummy);
  Result := PointD(bounds.Width, bounds.Height);
end;
//------------------------------------------------------------------------------

function TFontInfo.GetGlyphInfo(wc: WideChar): TGlyphInfo;
var
  i: integer;
begin
  Result := nil;
  if wc < #32 then Exit;
  if not fGlyphsSorted then SortGlyphs;
  i := FindGlyph(wc, fGlyphs);
  if i < 0 then
  begin
    FillMissingChars(wc);
    i := FindGlyph(wc, fGlyphs);
    if i >= 0 then Result := TGlyphInfo(fGlyphs[i])
  end
  else
    Result := TGlyphInfo(fGlyphs[i]);
end;
//------------------------------------------------------------------------------

procedure TFontInfo.AddGlyph(wc: WideChar; memDc: HDC);
var
  gi: TGlyphInfo;
  paths: TPathsD;
  gm: TGlyphMetrics;
begin
  paths := GetPathsForChar(memDC, wc, gm);
  if (gm.gmCellIncX = 0) and (gm.gmCellIncY = 0) then Exit;
  gi := TGlyphInfo.Create;
  gi.wc := wc;
  gi.gm := gm;
  gi.paths := paths;
  gi.bounds := GetBoundsD(paths);
  fGlyphs.Add(gi);
end;
//------------------------------------------------------------------------------

constructor TFontInfo.Create(const logfont: TLogfont; InitAnsi: boolean);
var
  i: integer;
  memDC: HDC;
  oldFont: HFont;
begin
  fLogFont := logfont;
  CheckFontHeight(fLogFont); //nb: needed before calling CreateFontIndirect
  fLogFont.lfOrientation := fLogFont.lfEscapement;

  {$IFDEF XPLAT_GENERICS}
  fGlyphs:= TList<TGlyphInfo>.Create;
  {$ELSE}
  fGlyphs := TList.Create;
  {$ENDIF}
  fHandle := CreateFontIndirect(fLogFont);
  if (fHandle > 0) and InitAnsi then
  begin
    memDC := CreateCompatibleDC(0);
    oldFont := SelectObject(memDC, fHandle);
    try
      for i := 32 to 126 do
         AddGlyph(WideChar(i), memDc);
    finally
      SelectObject(memDC, oldFont);
      DeleteDC(memDC);
    end;
  end;
end;
//------------------------------------------------------------------------------

destructor TFontInfo.Destroy;
var
  i: integer;
begin
  DeleteObject(fHandle);
  for i := 0 to fGlyphs.Count -1 do
    TGlyphInfo(fGlyphs[i]).Free;
  fGlyphs.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// TFontManager methods
//------------------------------------------------------------------------------

constructor TFontManager.Create(capacity: integer);
begin
  fInstalledFonts := TStringList.Create;
  fInstalledFonts.Sorted := true;
  fInstalledFonts.Duplicates := dupIgnore;
  fInstalledFonts.CaseSensitive := false;

  {$IFDEF XPLAT_GENERICS}
  fFontInfos := TList<TFontInfo>.Create;
  {$ELSE}
  fFontInfos := TList.Create;
  {$ENDIF}
  //fFontInfos.Sorted := true;

  fCapacity := Max(1, capacity);
end;
//------------------------------------------------------------------------------

destructor TFontManager.Destroy;
begin
  Clear;
  fFontInfos.Free;
  fInstalledFonts.Clear;
  fInstalledFonts.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TFontManager.GetInstalledFonts: TStringList;
var
  dc: HDC;
  lf: TLogFont;
begin
  result := fInstalledFonts;
  if result.Count > 0 then Exit;

  dc := GetDC(0);
  FillChar(lf, sizeof(lf), 0);
  lf.lfCharset := DEFAULT_CHARSET;
  EnumFontFamiliesEx(DC, lf, @EnumFontsProc, Windows.LPARAM(fInstalledFonts), 0);
  ReleaseDC(0, dc);
end;
//------------------------------------------------------------------------------

function TFontManager.GetPreferredFaceName(const faceNameList: string): string;
var
  i: integer;
  faceNames: TArrayOfString;
begin
  Result := '';
  GetInstalledFonts; //force initialization
  faceNames := SplitCommaSeparatedString(faceNameList);
  for i := 0 to high(faceNames) do
    if fInstalledFonts.IndexOf(faceNames[i]) >= 0 then
    begin
      Result := faceNames[i];
      break;
    end;
end;
//------------------------------------------------------------------------------

procedure TFontManager.Clear;
var
  i: integer;
begin
  for i := 0 to fFontInfos.Count -1 do
    TFontInfo(fFontInfos[i]).Free;
  fFontInfos.Clear;
end;
//------------------------------------------------------------------------------

procedure BytetoChrs(b: PByte; pc: PChar);
begin
  pc^ := Char(48 + (Ord(b^) shr 4));
  if Ord(pc^) > 57 then inc(pc^,7);
  inc(pc);
  pc^ := Char(48 + (Ord(b^) and $F));
  if Ord(pc^) > 57 then inc(pc^,7);
end;
//---------------------------------------------------------------------

function TFontManager.IndexOfOldest: integer;
var
  i: integer;
  time, time2: TDatetime;
begin
  Result := -1;
  if fFontInfos.Count = 0 then Exit;
  result := 0;
  time := TFontInfo(fFontInfos[0]).LastAccessed;
  for i := 1 to fFontInfos.Count -1 do
  begin
    time2 := TFontInfo(fFontInfos[i]).LastAccessed;
    if time2 >= time then Continue;
    time := time2;
    result := i;
  end;
end;
//------------------------------------------------------------------------------

procedure TFontManager.SetCapacity(capacity: integer);
var
  oldestIdx: integer;
begin
  fCapacity := Max(1, capacity);
  while fFontInfos.Count >= fCapacity do
  begin
    oldestIdx := IndexOfOldest;
    TFontInfo(fFontInfos[oldestIdx]).Free;
    fFontInfos.Delete(oldestIdx);
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF XPLAT_GENERICS}
function FindFontInfo(const lf: TLogFont; fontList: TList<TFontInfo>): integer;
{$ELSE}
function FindFontInfo(const lf: TLogFont; fontList: TList): integer;
{$ENDIF}
var
  i,l,r: integer;
  lfSearchStr: UnicodeString;
begin
  lfSearchStr := LogFontToSearchStr(lf);
  //binary search the sorted list ...
  l := 0;
  r := fontList.Count -1;
  while l <= r do
  begin
    Result := (l + r) shr 1;
    i := CompareStr(TFontInfo(fontList[Result]).fFontSortStr, lfSearchStr);
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

function FontInfoSorter(fontInfo1, fontInfo2: pointer): integer;
begin
  Result := CompareStr(TFontInfo(fontInfo1).fFontSortStr,
    TFontInfo(fontInfo2).fFontSortStr);
end;
//------------------------------------------------------------------------------

procedure TFontManager.Sort;
begin
  if fSorted then Exit;
  {$IFDEF XPLAT_GENERICS}
    fFontInfos.Sort(TComparer<TFontInfo>.Construct(
      function (const fontInfo1, fontInfo2: TFontInfo): integer
      begin
        Result := CompareStr(fontInfo1.fFontSortStr, fontInfo2.fFontSortStr);
      end));
  {$ELSE}
    fFontInfos.Sort(FontInfoSorter);
  {$ENDIF}
  fSorted := true;
end;
//------------------------------------------------------------------------------

function TFontManager.AddFontInternal(const logFont: TLogFont;
  InitAnsi: boolean): TFontInfo;
begin
  Result := TFontInfo.Create(logFont, InitAnsi);
  Result.fFontSortStr := LogFontToSearchStr(logFont);
  Result.LastAccessed := now;
  fFontInfos.Add(Result);
  fSorted := false;
end;
//------------------------------------------------------------------------------

function TFontManager.AddFont(const logFont: TLogFont; InitAnsi: boolean): TFontInfo;
var
  idx, oldestIdx: integer;
begin
  while fFontInfos.Count >= fCapacity do
  begin
    oldestIdx := IndexOfOldest;
    TFontInfo(fFontInfos[oldestIdx]).Free;
    fFontInfos.Delete(oldestIdx);
  end;
  Result := AddFontInternal(logFont, InitAnsi);
end;
//------------------------------------------------------------------------------

function TFontManager.GetFontInfo(const logFont: TLogFont;
  AutoAdd: Boolean; InitAnsi: boolean): TFontInfo;
var
  i: integer;
begin
  if not fSorted then Sort;
  i := FindFontInfo(logFont, fFontInfos);
  if i >= 0 then
  begin
    result := TFontInfo(fFontInfos[i]);
    Result.LastAccessed := now;
  end
  else if AutoAdd then
  begin
    Result := AddFontInternal(logFont, InitAnsi);
  end else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TFontManager.GetFontInfo(font: hFont; AutoAdd: Boolean = true;
  InitAnsi: boolean = true): TFontInfo;
var
  lf: TLogFont;
begin
  lf := GetLogFontFromHFont(font);
  Result := GetFontInfo(lf, AutoAdd, InitAnsi);
end;
//------------------------------------------------------------------------------

initialization
  CreateFontManager;

finalization
  FontManager.Free;
end.
