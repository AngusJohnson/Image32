unit Image32_SVG_Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.24                                                            *
* Date      :  22 May 2021                                                     *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2021                                         *
*                                                                              *
* Purpose   :  Read SVG files                                                  *
*                                                                              *
*              To fully support all the features of SVG version 2 is a huge    *
*              task and beyond my resources. Despite that, this unit still     *
*              compares very favourably with other Delphi SVG readers.         *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*******************************************************************************)

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types, Math,
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Image32, Image32_SVG_Core, Image32_Vector, Image32_Draw,
  Image32_Transform, Image32_Ttf;

type
  TElement          = class;
  TMarkerElement    = class;
  TFilterElement    = class;
  TClipPathElement  = class;
  TSvgElement       = class;

  TDrawInfo = record
    fillColor     : TColor32;
    fillRule      : TFillRule;
    fillEl        : TAnsiName;
    strokeColor   : TColor32;
    strokeWidth   : double;
    strokeEl      : TAnsiName;
    dashArray     : TArrayOfDouble;
    dashOffset    : double;
    markerStart   : TAnsiName;
    markerMiddle  : TAnsiName;
    markerEnd     : TAnsiName;
    filterEl      : TAnsiName;
    clipPathEl    : TAnsiName;
    opacity       : Byte;
    //lineCap       : TEndStyle;     //stroke-linecap
    //lineJoin      : TJoinStyle;    //stroke-linejoin
    //miterLim      : double;        //stroke-miterlimit
    matrix        : TMatrixD;
    visible       : Boolean;
    using         : Boolean;
  end;

  PAttrib = ^TAttrib;
  TAttrib = record
    aOwnerEl  : TElement;
    aName     : TAnsiName;
    aValue    : TAnsiName;
  end;
  TArrayOfAttrib = array of TAttrib;

  TSvgReader = class;

  TElementClass = class of TElement;

  TElement = class
  private
    fParent         : TElement;
    fReader         : TSvgReader;
{$IFDEF XPLAT_GENERICS}
    fChilds         : TList<TElement>;
{$ELSE}
    fChilds         : TList;
{$ENDIF}
    fHasContent     : Boolean;
    fIdAttribIdx    : integer;
    fStyleAttribIdx : integer;
    fName           : TAnsiName;
    fNameHash       : Cardinal;
    fAttribs        : TArrayOfAttrib;
    fDrawInfo       : TDrawInfo;
    fFontInfo       : TSVGFontInfo;
    fCurrent        : PAnsiChar;
    fCurrentEnd     : PAnsiChar;
    function  LoadChild: TElement;
    function  HashCurrentWord(out hash: Cardinal): Boolean;
    function  GetName(out name: TAnsiName): Boolean;
    function  SetAttribValue(attrib: PAttrib): Boolean;
    function  FindRefElement(refname: TAnsiName): TElement;
    procedure ProcessAttrib(const attrib: PAttrib);
    function  PeekNextChar: AnsiChar;
    procedure ParseClassAttrib(classAttrib: PAttrib);
    procedure ParseStyle(classStyle: PAnsiChar; len: integer);
    procedure ParseTransform(const transform: TAnsiName);
    function  AddAttribute: PAttrib;
    //function GetSvgElement: TSvgElement;
  protected
    measureUnit : TElementMeasureUnit;
    elRectWH    : TValueRecWH;
    function  LoadAttributes: Boolean; virtual;
    function  LoadContent: Boolean; virtual;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); virtual;
    destructor  Destroy; override;
  end;

  TSvgElement = class(TElement)
  protected
    viewboxWH : TRectWH;
  public
    constructor Create(reader: TSvgReader; hashName: Cardinal); reintroduce;
  end;

  TDefsElement = class(TElement)
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TStyleElement = class(TElement)
  protected
    function LoadContent: Boolean; override;
  end;

  TSwitchElement = class(TElement)
  end;

  TShapeElement = class(TElement)
  protected
    hasPaths    : Boolean;
    drawPathsO  : TPathsD;
    drawPathsC  : TPathsD;
    fillPaths   : TPathsD;
    function HasMarkers: Boolean;
    function GetVal(out val: double): Boolean;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); virtual;
    procedure DrawFilled(img: TImage32; const drawInfo: TDrawInfo);
    procedure DrawStroke(img: TImage32; const drawInfo: TDrawInfo; isClosed: Boolean);
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
    procedure DrawMarkers(img: TImage32; drawInfo: TDrawInfo);
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TUseElement = class(TShapeElement)
  protected
    refEl: TAnsiName;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  end;

  TPathElement = class(TShapeElement)
  private
    fCurrSeg      : PDpathSeg;
    fCurrSegCap   : integer;
    fCurrSegCnt   : integer;
    fCurrSegType  : TDsegType;
    fCurrDpath    : PDpath;
    fDpaths       : TDpaths;
    fLastPt       : TPointD;
    function GetSegType(out isRelative: Boolean): Boolean;
    function GetSingleDigit(out digit: integer): Boolean;
    procedure StartNewDpath;
    procedure StartNewSeg(segType: TDsegType);
    procedure AddSegValue(val: double);
    procedure AddSegPoint(const pt: TPointD);
    function Get2Num(var pt: TPointD; isRelative: Boolean): Boolean;
    procedure Flatten(index: integer; scalePending: double;
      out path: TPathD; out isClosed: Boolean);
  protected
    procedure ParseD(attrib: PAttrib);
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  end;

  TPolyElement = class(TShapeElement) //polyline or polygon
  protected
    path      : TPathD;
    procedure ParsePoints(attrib: PAttrib);
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  end;

  TLineElement = class(TShapeElement)
  protected
    path      : TPathD;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TCircleElement = class(TShapeElement)
  protected
    centerPt  : TValuePt;
    radius    : TValue;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  end;

  TEllipseElement = class(TShapeElement)
  protected
    centerPt  : TValuePt;
    radius    : TValuePt;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  end;

  TRectElement = class(TShapeElement)
  protected
    radius    : TValuePt;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  end;

  //TTextElement: although this is a TShapeElement descendant, it's really
  //only a container for 'tspan' and 'subtext' elements. (See Draw method.)
  TTextElement = class(TShapeElement)
  protected
    delta     : TPointD;
    tmpPt     : TPointD;
    procedure ResetTmpPt;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
    function AddSubtext(atext: PAnsiChar; len: integer): TElement;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TTSpanElement = class(TTextElement)
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TSubtextElement = class(TShapeElement)
  protected
    text      : PAnsiChar;
    textLen   : integer;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TTextPathElement = class(TSubtextElement)
  protected
    pathEl: TAnsiName;
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  end;

  TMarkerElement = class(TShapeElement)
  private
    fPoints     : TPathD;
  protected
    refPt       : TValuePt;
    angle       : double;
    markerBoxWH : TRectWH;
    autoStartReverse  : Boolean;
    procedure SetEndPoint(const pt: TPointD; angle: double);
    function SetMiddlePoints(const points: TPathD): Boolean;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TSvgColorStop = record
    offset    : double;
    color     : TColor32;
  end;
  TSvgColorStops = array of TSvgColorStop;

  TFillElement = class(TElement)
  protected
    refEl     : TAnsiName;
  end;

  TPatternElement = class(TFillElement)
  protected
    pattBoxWH : TRectWH;
    function PrepareRenderer(renderer: TImageRenderer;
      matrix: TMatrixD; const rec: TRectD): Boolean; virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TGradientElement = class(TFillElement)
  protected
    stops         : TSvgColorStops;
    gradientUnits : Cardinal;
    function LoadContent: Boolean; override;
    procedure AddStop(color: TColor32; offset: double);
    procedure AssignTo(other: TElement);  virtual;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRectD): Boolean; virtual;
  end;

  TRadGradElement = class(TGradientElement)
  protected
    radius: TValue;
    F, C: TValuePt;
    procedure AssignTo(other: TElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRectD): Boolean; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TLinGradElement = class(TGradientElement)
  protected
    startPt, endPt: TValuePt;
    procedure AssignTo(other: TElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      matrix: TMatrixD; rec: TRectD): Boolean; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TGradStopElement = class(TElement)
  protected
    offset: double;
    color: TColor32;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TFilterElement = class(TElement)
  private
    fSrcImg       : TImage32;
    fLastImg      : TImage32;
    fScale        : double;
    fFilterBounds : TRect;
    fObjectBounds : TRect;
    fImages       : array of TImage32;
    fNames        : TArrayOfAnsiName;
  protected
    procedure Clear;
    function GetAdjustedBounds(const bounds: TRectD; scale: double): TRectD;
    function FindNamedImage(const name: TAnsiName): TImage32;
    function AddNamedImage(const name: TAnsiName): TImage32;
    function GetNamedImage(const name: TAnsiName): TImage32;
    procedure Apply(img: TImage32;
      const filterBounds: TRect; const matrix: TMatrixD);
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
    destructor Destroy; override;
  end;

  TFeBaseElement = class(TElement)
  private
    function GetParentAsFilterEl: TFilterElement;
  protected
    in1: TAnsiName;
    in2: TAnsiName;
    res: TAnsiName;
    srcImg, dstImg: TImage32;
    srcRec, dstRec: TRect;
    function GetSrcAndDst: Boolean;
    function GetBounds(img: TImage32): TRect;
    procedure Apply; virtual; abstract;
    property ParentFilterEl: TFilterElement read GetParentAsFilterEl;
  end;

  TFeBlendElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TCompositeOp = (coOver, coIn, coOut, coAtop, coXOR);

  TFeCompositeElement  = class(TFeBaseElement)
  protected
    compositeOp: TCompositeOp;
    procedure Apply; override;
  end;

  TFeColorMatrixElement  = class(TFeBaseElement)
  protected
    values: TArrayOfDouble;
    procedure Apply; override;
  end;

  TFeDropShadowElement = class(TFeBaseElement)
  protected
    stdDev      : double;
    dxdy        : TValuePt;
    floodColor  : TColor32;
    procedure Apply; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TFeFloodElement  = class(TFeBaseElement)
  protected
    floodColor  : TColor32;
    procedure Apply; override;
  end;

  TFeGaussElement  = class(TFeBaseElement)
  protected
    stdDev: double;
    procedure Apply; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TFeMergeElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TFeMergeNodeElement  = class(TFeBaseElement)
  protected
    procedure Apply; override;
  end;

  TFeOffsetElement = class(TFeBaseElement)
  protected
    dxdy        : TValuePt;
    procedure Apply; override;
  end;

  TClipPathElement = class(TShapeElement)
  protected
    procedure GetUnscaledPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TAttribFunc = procedure (attrib: PAttrib);

  TSvgReader = class
  private
    fMemStream        : TMemoryStream;
    fEndStream        : PAnsiChar;
    fBackgroundColor  : TColor32;
    fTempImage        : TImage32;
    fIdList           : TStringList;
    fClassStyles      : TClassStylesList;
    fLinGradRenderer  : TLinearGradientRenderer;
    fRadGradRenderer  : TSvgRadialGradientRenderer;
    fImgRenderer      : TImageRenderer;
    fViewMatrix       : TMatrixD;
    fRootElement      : TSvgElement;
{$IFDEF XPLAT_GENERICS}
    fFontList   : TList<TFontReader>;
{$ELSE}
    fFontList   : TList;
{$ENDIF}
    fFontCache  : TGlyphCache;
  protected
    rawRect     : TRectD;
    function GetSvgStart(out svgStart: PAnsiChar): Boolean;
    procedure GetBestFont(const svgFontInfo: TSVGFontInfo);
    property RadGradRenderer: TSvgRadialGradientRenderer read fRadGradRenderer;
    property LinGradRenderer: TLinearGradientRenderer read fLinGradRenderer;
    property ImageRenderer  : TImageRenderer read fImgRenderer;

    property TempImage      : TImage32 read fTempImage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
{$IFDEF MSWINDOWS}
    function AddFont(const fontName: string): Boolean;
{$ENDIF}
    function AddFontFromResource(const resName: string; resType: PChar): Boolean;
    procedure DrawImage(img: TImage32; scaleToImage: Boolean);
    function LoadFromStream(stream: TStream): Boolean;
    function LoadFromFile(const filename: string): Boolean;
    property BackgroundColor: TColor32
      read fBackgroundColor write fBackgroundColor;
  end;

implementation

uses
  Image32_Extra, StrUtils;

const
  buffSize = 32;
  clInvalid = $00010001;
  SourceImage   : TAnsiName = (name: 'SourceGraphic'; len: 13);
  SourceAlpha   : TAnsiName = (name: 'SourceAlpha'; len: 11);
  tmpFilterImg  : TAnsiName = (name: 'tmp'; len: 3);

  {$I Image32_SVG_HashConsts.inc}

  defaultDrawInfo: TDrawInfo =
    (fillColor: clBlack32; fillRule: frNonZero; fillEl: (name: nil; len: 0);
    strokeColor: clInvalid; strokeWidth: 1.0;
    dashArray: nil; dashOffset: 0; markerStart: (name: nil; len: 0);
    markerMiddle: (name: nil; len: 0); markerEnd: (name: nil; len: 0);
    filterEl: (name: nil; len: 0);clipPathEl: (name: nil; len: 0); opacity: 255;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); visible: true; using: false);

  emptyDrawInfo: TDrawInfo =
    (fillColor: clInvalid; fillRule: frNonZero; fillEl: (name: nil; len: 0);
    strokeColor: clInvalid; strokeWidth: -Infinity;
    dashArray: nil; dashOffset: 0; markerStart: (name: nil; len: 0);
    markerMiddle: (name: nil; len: 0);markerEnd: (name: nil; len: 0);
    filterEl: (name: nil; len: 0);clipPathEl: (name: nil; len: 0);opacity: 255;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); visible: true; using: false);

  defaultFontInfo: TSVGFontInfo =
    (family: ttfSansSerif; size: 10; spacing: 0.0;
    styles: []; align: taLeft; decoration: fdNone);

var
  AttribFuncList : TStringList;
  defaultFontHeight: double = 20.0;

//------------------------------------------------------------------------------
// Miscellaneous functions ...
//------------------------------------------------------------------------------

function HashToElementClass(hash: Cardinal): TElementClass;
begin
  case hash of
    hClippath       : Result := TClipPathElement;
    hCircle         : Result := TCircleElement;
    hDefs           : Result := TDefsElement;
    hEllipse        : Result := TEllipseElement;
    hFilter         : Result := TFilterElement;
    hfeBlend        : Result := TFeBlendElement;
    hfeColorMatrix  : Result := TFeColorMatrixElement;
    hfeComposite    : Result := TFeCompositeElement;
    hfeDropShadow   : Result := TFeDropShadowElement;
    hfeFlood        : Result := TFeFloodElement;
    hFeGaussianBlur : Result := TFeGaussElement;
    hfeMerge        : Result := TFeMergeElement;
    hfeMergeNode    : Result := TFeMergeNodeElement;
    hfeOffset       : Result := TFeOffsetElement;
    hG              : Result := TElement;
    hLine           : Result := TLineElement;
    hLineargradient : Result := TLinGradElement;
    hMarker         : Result := TMarkerElement;
    hPath           : Result := TPathElement;
    hPattern        : Result := TPatternElement;
    hPolyline       : Result := TPolyElement;
    hPolygon        : Result := TPolyElement;
    hRadialgradient : Result := TRadGradElement;
    hRect           : Result := TRectElement;
    hStop           : Result := TGradStopElement;
    hStyle          : Result := TStyleElement;
    hSwitch         : Result := TSwitchElement;
    hText           : Result := TTextElement;
    hTextPath       : Result := TTextPathElement;
    hTSpan          : Result := TTSpanElement;
    hUse            : Result := TUseElement;
    else              Result := TElement;
  end;
end;
//------------------------------------------------------------------------------

procedure UpdateDrawInfo(var drawInfo: TDrawInfo; const primaryDI: TDrawInfo);
begin
  with primaryDI do
  begin
    drawInfo.fillRule := fillRule;
    if fillColor <> clInvalid then
      drawInfo.fillColor := fillColor;
    if Assigned(fillEl.name) then
      drawInfo.fillEl := fillEl;
    if strokeColor <> clInvalid then
      drawInfo.strokeColor := strokeColor;
    if strokeWidth >= 0 then
      drawInfo.strokeWidth := strokeWidth;
    if Assigned(dashArray) then
      drawInfo.dashArray := Copy(dashArray, 0, Length(dashArray));
    if Assigned(strokeEl.name) then
      drawInfo.strokeEl := strokeEl;
    if opacity < 255 then
      drawInfo.opacity := opacity;
    if Assigned(filterEl.name) then
      drawInfo.filterEl := filterEl;
    if not IsIdentityMatrix(matrix) then
      drawInfo.matrix := MatrixMultiply(drawInfo.matrix, matrix);
  end;
end;
//------------------------------------------------------------------------------

function IsFilled(const drawInfo: TDrawInfo): Boolean;
begin
  Result := drawInfo.visible and
    ((drawInfo.fillColor = clInvalid) or
    Assigned(drawInfo.fillEl.name) or (TARGB(drawInfo.fillColor).A > 0));
end;
//------------------------------------------------------------------------------

function IsStroked(const drawInfo: TDrawInfo): Boolean;
begin
  Result := drawInfo.visible and
    ((drawInfo.strokeWidth > 0) and
    (Assigned(drawInfo.strokeEl.name) or (TARGB(drawInfo.strokeColor).A > 0)));
end;
//------------------------------------------------------------------------------

function AttribToColor32(attrib: PAttrib; var color: TColor32): Boolean;
begin
  if ColorIsURL(attrib.aValue.name) then
  begin
    Result := true;
    if GetHashedName(attrib.aName) = hFill then
      attrib.aOwnerEl.fDrawInfo.fillEl := ExtractRefFromValue(attrib.aValue) else
      attrib.aOwnerEl.fDrawInfo.strokeEl := ExtractRefFromValue(attrib.aValue);
  end else
    Result := ValueToColor32(attrib.aValue, color);
end;
//------------------------------------------------------------------------------

function AttribToFloat(attrib: PAttrib; var value: double): Boolean; overload;
var
  c: PAnsiChar;
begin
  c := attrib.aValue.name;
  Result := ParseNextNum(c, c + attrib.aValue.len, false, value);
end;
//------------------------------------------------------------------------------

function AttribToFloat(attrib: PAttrib;
  var value: double; out measureUnit: TMeasureUnit): Boolean; overload;
var
  c: PAnsiChar;
begin
  c := attrib.aValue.name;
  Result := ParseNextNum(c, c + attrib.aValue.len, false, value, measureUnit);
end;
//------------------------------------------------------------------------------

procedure AttribToOpacity(attrib: PAttrib; var color: TColor32);
var
  opacity: double;
begin
  if color = clNone32 then Exit;
  if not AttribToFloat(attrib, opacity) then Exit;
  if (opacity < 0) or (opacity > 1) then Exit;
  color := (color and $FFFFFF) or (Round(255 * opacity) shl 24);
end;
//------------------------------------------------------------------------------

procedure AttribToFontInfo(attrib: PAttrib; var fontInfo: TSVGFontInfo);
var
  c, endC: PAnsiChar;
  hash: Cardinal;
begin
  c := attrib.aValue.name;
  endC := c + attrib.aValue.len;
  while (c < endC) and SkipBlanks(c, endC) do
  begin
    if c = ';' then
      break
    else if IsNumPending(c, endC, true) then
      ParseNextNum(c, endC, true, fontInfo.size)
    else
    begin
      hash := ParseNextWordHashed(c, endC);
      case hash of
        hSans_045_Serif   : fontInfo.family := ttfSansSerif;
        hSerif            : fontInfo.family := ttfSerif;
        hMonospace        : fontInfo.family := ttfMonospace;
        hBold             : Include(fontInfo.styles, fsBold);
        hItalic           : Include(fontInfo.styles, fsItalic);
        hStart            : fontInfo.align := taLeft;
        hMiddle           : fontInfo.align := taCenter;
        hEnd              : fontInfo.align := taRight;
        hline_045_through : fontInfo.decoration := fdStrikeThrough;
        hUnderline        : fontInfo.decoration := fdUnderline;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TDefsElement
//------------------------------------------------------------------------------

constructor TDefsElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.visible := false;
end;

//------------------------------------------------------------------------------
// TUseElement
//------------------------------------------------------------------------------

procedure TUseElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
var
  el: TElement;
  scale, dx, dy: double;
  di: TDrawInfo;
begin
  inherited;
  if Assigned(fillPaths) or not Assigned(refEl.name) then Exit;
  el := FindRefElement(refEl);
  if not Assigned(el) or not (el is TShapeElement) then Exit;

  if elRectWH.left.IsValid then dx := elRectWH.left.rawVal else dx := 0;
  if elRectWH.top.IsValid  then dy := elRectWH.top.rawVal  else dy := 0;
  di := drawInfo;
  UpdateDrawInfo(di, el.fDrawInfo);
  UpdateDrawInfo(di, fDrawInfo);
  di.using := true;
  scale := ExtractAvgScaleFromMatrix(di.matrix);
  MatrixTranslate(di.matrix, dx * scale, dy * scale);

  with TShapeElement(el) do
  begin
    GetUnscaledPaths(di);
    self.drawPathsC := CopyPaths(drawPathsC);
    self.drawPathsO := CopyPaths(drawPathsO);
  end;
end;
//------------------------------------------------------------------------------

procedure TUseElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
  el: TElement;
  scale, dx, dy: double;
  di: TDrawInfo;
begin
  el := FindRefElement(refEl);
  if not Assigned(el) then Exit;
  if elRectWH.left.IsValid then dx := elRectWH.left.rawVal else dx := 0;
  if elRectWH.top.IsValid  then dy := elRectWH.top.rawVal  else dy := 0;

  if el.fNameHash = hG then //<g> element
  begin
    UpdateDrawInfo(drawInfo, el.fDrawInfo);
    for i := 0 to el.fChilds.Count -1 do
      with el.fChilds[i] do
      begin
        di := drawInfo;
        UpdateDrawInfo(di, fDrawInfo);
        di.using := true;
        scale := ExtractAvgScaleFromMatrix(di.matrix);
        MatrixTranslate(di.matrix, dx * scale, dy * scale);
        Draw(img, di);
      end;
  end
  else if el is TShapeElement then
  begin
    UpdateDrawInfo(drawInfo, el.fDrawInfo);
    UpdateDrawInfo(drawInfo, fDrawInfo);
    drawInfo.using := true;
    scale := ExtractAvgScaleFromMatrix(drawInfo.matrix);
    MatrixTranslate(drawInfo.matrix, dx * scale, dy * scale);
    TShapeElement(el).Draw(img, drawInfo);
  end;
end;

//------------------------------------------------------------------------------
// TGradElement
//------------------------------------------------------------------------------

function TGradientElement.LoadContent: Boolean;
var
  i: integer;
begin
  Result := inherited LoadContent;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TGradStopElement then
      with TGradStopElement(fChilds[i]) do
        AddStop(color, offset);
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AddStop(color: TColor32; offset: double);
var
  i, len: integer;
begin
  len := Length(stops);
  setLength(stops, len+1);

  //make sure stops are in ascending offset order
  for i := 0 to len -1 do
    if offset < stops[i].offset then
    begin
      Move(stops[i], stops[i+1], (len -i) * SizeOf(TSvgColorStop));
      stops[i].offset := Min(1,Max(0, offset));
      stops[i].color := color;
      Exit;
    end;

  stops[len].offset := Min(1,Max(0, offset));
  stops[len].color := color;
end;
//------------------------------------------------------------------------------

procedure TGradientElement.AssignTo(other: TElement);
var
  i, len: integer;
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;

  with TGradientElement(other) do
  begin
    if gradientUnits = 0 then
      gradientUnits := Self.gradientUnits;

    if Length(stops) = 0 then
    begin
      len := Length(self.stops);
      SetLength(stops, len);
      for i := 0 to len -1 do
        stops[i] := Self.stops[i];
    end;

    if IsIdentityMatrix(fDrawInfo.matrix) then
      fDrawInfo.matrix := self.fDrawInfo.matrix;
  end;
end;
//------------------------------------------------------------------------------

function TGradientElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRectD): Boolean;
var
  i, hiStops: integer;
  el: TElement;
begin
  if Assigned(refEl.name) then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TGradientElement) then
      TGradientElement(el).AssignTo(self);
  end;

  hiStops := High(stops);
  Result := hiStops > 0;
  if not Result then Exit;
  for i := 0 to hiStops do
    with stops[i] do renderer.InsertColorStop(offset, color);
end;

//------------------------------------------------------------------------------
// TRadGradElement
//------------------------------------------------------------------------------

constructor TRadGradElement.Create(parent: TElement; hashName: Cardinal);
begin
  radius.Init;
  F.Init;
  C.Init;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TRadGradElement.AssignTo(other: TElement);
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;
  if other is TRadGradElement then
    with TRadGradElement(other) do
    begin
      if not radius.IsValid then radius := self.radius;
      if not C.IsValid then C := self.C;
      if not F.IsValid then F := self.F;
    end;
end;
//------------------------------------------------------------------------------

function TRadGradElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRectD): Boolean;
var
  hiStops: integer;
  cp, fp: TPointD;
  r: double;
  rec2, rec3: TRectD;
begin
  inherited;
  hiStops := High(stops);
  Result := hiStops > 0;
  if not Result then Exit;

//  if gradientUnits = hUserSpaceOnUse then
//    rec2 := fReader.fRootElement.viewboxWH.RectD else
//    rec2 := rec;

  if gradientUnits = hUserSpaceOnUse then
    rec2 := fReader.rawRect else
    rec2 := rec;

  if radius.IsValid then
  begin
    if radius.mu <> muPercent then
    begin
      r := radius.rawVal;
      r := r * ExtractAvgScaleFromMatrix(matrix);
    end else
      r := radius.GetValueXY(rec);
  end else
    r := (rec2.Width + rec2.Height) * 0.25;

  if C.IsValid then
  begin
    cp := C.GetPoint(rec2);
    MatrixApply(fDrawInfo.matrix, cp);
    if C.X.IsValid and (C.X.mu <> muPercent) then
      MatrixApply(matrix, cp);
    cp := OffsetPoint(cp, rec2.Left, rec2.Top);
  end else
    cp := rec2.MidPoint;

  rec3 := RectD(cp.X-r, cp.Y-r, cp.X+r, cp.Y+r);

  if F.IsValid then
  begin
    fp := F.GetPoint(rec2);
    MatrixApply(fDrawInfo.matrix, fp);
    if F.X.IsValid and (F.X.mu <> muPercent) then
      MatrixApply(matrix, fp);
    fp := OffsetPoint(fp, rec2.Left, rec2.Top);
  end else
    fp := MidPoint(rec3);

  with renderer as TSvgRadialGradientRenderer do
    SetParameters(Rect(rec3), Point(fp),
      stops[0].color, stops[High(stops)].color);
  //nb: must call inherited AFTER SetParameters
  Result := inherited PrepareRenderer(renderer, matrix, rec);
end;

//------------------------------------------------------------------------------
// TLinGradElement
//------------------------------------------------------------------------------

constructor TLinGradElement.Create(parent: TElement; hashName: Cardinal);
begin
  startPt.Init;
  endPt.Init;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLinGradElement.AssignTo(other: TElement);
begin
  if not Assigned(other) or not (other is TGradientElement) then Exit;
  inherited;
  if other is TLinGradElement then
    with TLinGradElement(other) do
    begin
      if not startPt.IsValid then startPt := self.startPt;
      if not endPt.IsValid then endPt := self.endPt;
    end;
end;
//------------------------------------------------------------------------------

function TLinGradElement.PrepareRenderer(
  renderer: TCustomGradientRenderer; matrix: TMatrixD; rec: TRectD): Boolean;
var
  pt1, pt2: TPointD;
  hiStops: integer;
  rec2: TRectD;
begin
  inherited;
  hiStops := High(stops);
  Result := (hiStops > 0);
  if not Result then Exit;

  if gradientUnits = hUserSpaceOnUse then
    rec2 := fReader.rawRect else
    rec2 := rec;

  with renderer as TLinearGradientRenderer do
  begin
    if startPt.X.IsValid then
      pt1.X := startPt.X.GetValueX(rec2) else
      pt1.X := 0;
    if startPt.Y.IsValid then
      pt1.Y := startPt.Y.GetValueY(rec2) else
      pt1.Y := 0;

    MatrixApply(fDrawInfo.matrix, pt1);
    if startPt.X.IsValid and (startPt.X.mu <> muPercent) then
      MatrixApply(matrix, pt1);
    pt1 := OffsetPoint(pt1, rec2.Left, rec2.Top);

    if endPt.X.IsValid then
      pt2.X := endPt.X.GetValueX(rec2) else
      pt2.X := rec2.Width;
    if endPt.Y.IsValid then
      pt2.Y := endPt.Y.GetValueY(rec2) else
      pt2.Y := 0;

    MatrixApply(fDrawInfo.matrix, pt2);
    if endPt.X.IsValid and (endPt.X.mu <> muPercent) then
      MatrixApply(matrix, pt2);
    pt2 := OffsetPoint(pt2, rec2.Left, rec2.Top);

    SetParameters(pt1, pt2, stops[0].color, stops[High(stops)].color);
  end;
  //nb: must call inherited after SetParameters
  Result := inherited PrepareRenderer(renderer, matrix, rec);
end;

//------------------------------------------------------------------------------
// TGradStopElement
//------------------------------------------------------------------------------

constructor TGradStopElement.Create(parent: TElement; hashName: Cardinal);
begin
  color := clBlack32;
  inherited;
end;

//------------------------------------------------------------------------------
// TFilterElement
//------------------------------------------------------------------------------

constructor TFilterElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

destructor TFilterElement.Destroy;
begin
  Clear;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TFilterElement.Clear;
var
  i: integer;
begin
  for i := 0 to High(fImages) do
    fImages[i].Free;
  fImages := nil;
  fNames := nil;
  fLastImg := nil;
end;
//------------------------------------------------------------------------------

function TFilterElement.GetAdjustedBounds(const bounds: TRectD;
  scale: double): TRectD;
var
  recWH: TRectWH;
begin
  fObjectBounds := Rect(bounds);
  if elRectWH.IsValid then
  begin
    recWH := elRectWH.GetRectWH(bounds);
    Result.Left := bounds.Left + recWH.Left * scale;
    Result.Top := bounds.Top + recWH.Top * scale;
    Result.Right := Result.Left + recWH.Width * scale;
    Result.Bottom := Result.Top + recWH.Height * scale;
  end else
    //default: inflate by 15%
    Result := InflateRect(bounds, bounds.Width * 0.15, bounds.Height * 0.15);
end;
//------------------------------------------------------------------------------

function TFilterElement.FindNamedImage(const name: TAnsiName): TImage32;
var
  i, len: integer;
begin
  Result := nil;
  len := Length(fNames);
  for i := 0 to len -1 do
    if name.Equals(fNames[i]) then
    begin
      Result := fImages[i];
      Break;
    end;
end;
//------------------------------------------------------------------------------

function TFilterElement.AddNamedImage(const name: TAnsiName): TImage32;
var
  len: integer;
begin
  len := Length(fNames);
  SetLength(fNames, len+1);
  SetLength(fImages, len+1);
  with fFilterBounds do
    Result := TImage32.Create(Width, Height);
  fImages[len] := Result;
  fNames[len] := name;
end;
//------------------------------------------------------------------------------

function TFilterElement.GetNamedImage(const name: TAnsiName): TImage32;
var
  i, len: integer;
  hash: Cardinal;
begin
  hash := GetHashedName(name);
  case hash of
    hSourceGraphic, hSourceAlpha:
      begin
        Result := FindNamedImage(name);
        if not Assigned(Result) then
        begin
          Result := AddNamedImage(name);
          Result.Copy(fSrcImg, fFilterBounds, Result.Bounds);
          if hash = hSourceAlpha then
            Result.SetRGB(clNone32, Result.Bounds);
        end;
        Exit;
      end;
  end;

  len := Length(fNames);
  for i := 0 to len -1 do
    if name.Equals(fNames[i]) then
    begin
      Result := fImages[i];
      Exit;
    end;
  Result := AddNamedImage(name);
end;
//------------------------------------------------------------------------------

procedure TFilterElement.Apply(img: TImage32;
  const filterBounds: TRect; const matrix: TMatrixD);
var
  i: integer;
begin
  fScale := ExtractAvgScaleFromMatrix(matrix);
  fFilterBounds := filterBounds;
  fObjectBounds := Image32_Vector.IntersectRect(img.Bounds, fObjectBounds);
  fSrcImg := img;
  try
    for i := 0 to fChilds.Count -1 do
      case TElement(fChilds[i]).fNameHash of
        hfeBlend        : TFeBlendElement(fChilds[i]).Apply;
        hfeColorMatrix  : TFeColorMatrixElement(fChilds[i]).Apply;
        hfeComposite    : TFeCompositeElement(fChilds[i]).Apply;
        hfeDropShadow   : TFeDropShadowElement(fChilds[i]).Apply;
        hfeFlood        : TFeFloodElement(fChilds[i]).Apply;
        hFeGaussianBlur : TFeGaussElement(fChilds[i]).Apply;
        hfeMerge        : TFeMergeElement(fChilds[i]).Apply;
        hfeOffset       : TFeOffsetElement(fChilds[i]).Apply;
      end;
    if fLastImg <> fSrcImg then
      fSrcImg.Copy(fLastImg, fLastImg.Bounds, fFilterBounds);
  finally
    Clear;
  end;


end;

//------------------------------------------------------------------------------
// TFeBaseElement
//------------------------------------------------------------------------------

function TFeBaseElement.GetParentAsFilterEl: TFilterElement;
var
  el: TElement;
begin
  el := fParent;
  while Assigned(el) and not (el is TFilterElement) do
    el := el.fParent;
  if not Assigned(el) then
    Result := nil else
    Result := TFilterElement(el);
end;
//------------------------------------------------------------------------------

function TFeBaseElement.GetBounds(img: TImage32): TRect;
var
  pfe: TFilterElement;
begin
  pfe := ParentFilterEl;
  if img = pfe.fSrcImg then
    Result := pfe.fFilterBounds else
    Result := img.Bounds;
end;
//------------------------------------------------------------------------------

function TFeBaseElement.GetSrcAndDst: Boolean;
var
  pfe: TFilterElement;
begin
  pfe := ParentFilterEl;
  if Assigned(in1.name) then
    srcImg := pfe.GetNamedImage(in1)
  else if Assigned(pfe.fLastImg) then
    srcImg := pfe.fLastImg
  else
    srcImg := pfe.GetNamedImage(SourceImage);

  if Assigned(res.name) then
    dstImg := pfe.GetNamedImage(res) else
    dstImg := pfe.fSrcImg;

  Result := Assigned(srcImg) and Assigned(dstImg);
  if not Result then Exit;
  pfe.fLastImg := dstImg;
  srcRec := GetBounds(srcImg);
  dstRec := GetBounds(dstImg);
end;

//------------------------------------------------------------------------------
// TFeBlendElement
//------------------------------------------------------------------------------

procedure TFeBlendElement.Apply;
var
  pfe: TFilterElement;
  srcImg2, dstImg2: TImage32;
  srcRec2, dstRec2: TRect;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  if not Assigned(in2.name) then Exit;
  if dstImg = srcImg then
    dstImg2 := pfe.AddNamedImage(tmpFilterImg) else
    dstImg2 := dstImg;
  dstRec2 := GetBounds(dstImg2);

  srcImg2 := pfe.GetNamedImage(in2);
  srcRec2 := GetBounds(srcImg2);
  dstImg2.CopyBlend(srcImg2, srcRec2, dstRec2, BlendToAlpha);
  dstImg2.CopyBlend(srcImg,  srcRec,  dstRec2, BlendToAlpha);
  if dstImg = srcImg then
    dstImg.Copy(dstImg2, dstRec2, dstRec);
end;

//------------------------------------------------------------------------------
// TFeCompositeElement
//------------------------------------------------------------------------------

procedure TFeCompositeElement.Apply;
var
  pfe: TFilterElement;
  srcImg2, dstImg2: TImage32;
  srcRec2, dstRec2: TRect;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  if not Assigned(in2.name) then Exit;
  if dstImg = srcImg then
    dstImg2 := pfe.AddNamedImage(tmpFilterImg) else
    dstImg2 := dstImg;
  dstRec2 := GetBounds(dstImg2);

  srcImg2 := pfe.GetNamedImage(in2);
  srcRec2 := GetBounds(srcImg2);

  case compositeOp of
    coIn:
      begin
        dstImg2.Copy(srcImg, srcRec, dstRec2);
        dstImg2.CopyBlend(srcImg2,  srcRec2,  dstRec2, BlendMask);
      end;
    coOut:
      begin
        dstImg2.Copy(srcImg, srcRec, dstRec2);
        dstImg2.CopyBlend(srcImg2,  srcRec2,  dstRec2, BlendInvertedMask);
      end;
//    coAtop: ;
//    coXOR: ;
    else     //coOver
      begin
        dstImg2.CopyBlend(srcImg2, srcRec2, dstRec2, BlendToAlpha);
        dstImg2.CopyBlend(srcImg,  srcRec,  dstRec2, BlendToAlpha);
      end;
  end;

  if dstImg = srcImg then
    dstImg.Copy(dstImg2, dstRec2, dstRec);
end;

//------------------------------------------------------------------------------
// TFeColorMatrixElement
//------------------------------------------------------------------------------

type
  TColorMatrix = array[0..19] of Byte;

function ApplyColorMatrix(color: TColor32; const mat: TColorMatrix): TColor32;
var
  clrIn : TARGB absolute color;
  clrOut: TARGB absolute Result;
begin
  clrOut.R := ClampByte(MulBytes(mat[0],clrIn.R) + MulBytes(mat[1],clrIn.G) +
    MulBytes(mat[2],clrIn.B) + MulBytes(mat[3],clrIn.A) + mat[4]);
  clrOut.G := ClampByte(MulBytes(mat[5],clrIn.R) + MulBytes(mat[6],clrIn.G) +
    MulBytes(mat[7],clrIn.B) + MulBytes(mat[8],clrIn.A) + mat[9]);
  clrOut.B := ClampByte(MulBytes(mat[10],clrIn.R) + MulBytes(mat[11],clrIn.G) +
    MulBytes(mat[12],clrIn.B) + MulBytes(mat[13],clrIn.A) + mat[14]);
  clrOut.A := ClampByte(MulBytes(mat[15],clrIn.R) + MulBytes(mat[16],clrIn.G) +
    MulBytes(mat[17],clrIn.B) + MulBytes(mat[18],clrIn.A) + mat[19]);
end;
//------------------------------------------------------------------------------

procedure TFeColorMatrixElement.Apply;
var
  i,j, dx1,dx2: integer;
  colorMatrix: TColorMatrix;
  p1, p2: PColor32;
begin
  if not GetSrcAndDst or not Assigned(values) then Exit;
  for i := 0 to 19 do
    colorMatrix[i] := ClampByte(Round(values[i]*255));

  dx1 := srcImg.Width - RectWidth(srcRec);
  dx2 := dstImg.Width - RectWidth(dstRec);
  p1 := @srcImg.Pixels[srcRec.Top * srcImg.Width + srcRec.Left];
  p2 := @dstImg.Pixels[dstRec.Top * dstImg.Width + dstRec.Left];
  for i := srcRec.Top to srcRec.Bottom -1 do
  begin
    for j := srcRec.Left to srcRec.Right -1 do
    begin
      p2^ := ApplyColorMatrix(p1^, colorMatrix);
      inc(p1); inc(p2);
    end;
    inc(p1, dx1); inc(p2, dx2);
  end;
end;

//------------------------------------------------------------------------------
// TFeDropShadowElement
//------------------------------------------------------------------------------

constructor TFeDropShadowElement.Create(parent: TElement; hashName: Cardinal);
begin
  stdDev := InvalidD;
  dxdy.X.SetValue(0);
  dxdy.Y.SetValue(0);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TFeDropShadowElement.Apply;
var
  alpha: Byte;
  off: TPointD;
  dstOffRec: TRect;
  pfe: TFilterElement;
  dropShadImg: TImage32;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  dropShadImg := pfe.GetNamedImage(tmpFilterImg);
  dropShadImg.Copy(srcImg, srcRec, dropShadImg.Bounds);

  off := dxdy.GetPoint(RectD(pfe.fObjectBounds));
  off := ScalePoint(off, pfe.fScale);
  dstOffRec := dstRec;
  with Point(off) do OffsetRect(dstOffRec, X, Y);
  dstImg.Copy(srcImg, srcRec, dstOffRec);
  dstImg.SetRGB(floodColor);
  alpha := floodColor shr 24;
  if (alpha > 0) and (alpha < 255) then
    dstImg.ReduceOpacity(alpha);
  if stdDev > 0 then
    FastGaussianBlur(dstImg, dstRec,
      Ceil(stdDev *0.75 * ParentFilterEl.fScale) , 0);
  dstImg.CopyBlend(dropShadImg, dropShadImg.Bounds, dstRec, BlendToAlpha);
end;

//------------------------------------------------------------------------------
// TFeFloodElement
//------------------------------------------------------------------------------

procedure TFeFloodElement.Apply;
var
  rec: TRect;
begin
  if not GetSrcAndDst then Exit;
  if elRectWH.IsValid then
    rec := Rect(elRectWH.GetRect(RectD(srcRec))) else
    rec := srcRec;
  dstImg.FillRect(rec, floodColor);
end;

//------------------------------------------------------------------------------
// TFeGaussElement
//------------------------------------------------------------------------------

constructor TFeGaussElement.Create(parent: TElement; hashName: Cardinal);
begin
  stdDev := InvalidD;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TFeGaussElement.Apply;
const
  blurQuality = 1;//0..2. 0: OK (faster); 1: Good; 2: best (slower)
begin
  if not GetSrcAndDst then Exit;

  if srcImg <> dstImg then
    dstImg.Copy(srcImg, srcRec, dstRec);

  ////True GaussianBlur is visually optimal, but it's also *extremely* slow.
  //GaussianBlur(dstImg, dstRec, Ceil(stdDev *PI * ParentFilterEl.fScale));

  //FastGaussianBlur is a very good approximation while very much faster.
  //Empirically stdDev * 0.75 produces a closer approx of other renderers.
  FastGaussianBlur(dstImg, dstRec,
    Ceil(stdDev *0.75 * ParentFilterEl.fScale) , blurQuality);
end;

//------------------------------------------------------------------------------
// TFeMergeElement
//------------------------------------------------------------------------------

procedure TFeMergeElement.Apply;
var
  i: integer;
  tmpImg: TImage32;
  tmpRec: TRect;
  pfe: TFilterElement;
begin
  tmpImg := nil; tmpRec := NullRect;
  if not GetSrcAndDst then Exit;

  for i := 0 to fChilds.Count -1 do
    if fChilds[i] is TFeMergeNodeElement then
      with TFeMergeNodeElement(fChilds[i]) do
      begin
        if not GetSrcAndDst then Continue;
        if Assigned(tmpImg) then
        begin
          tmpImg.CopyBlend(srcImg, srcRec, tmpRec, BlendToAlpha);
        end else
        begin
          tmpImg := srcImg;
          tmpRec := srcRec;
        end;
      end;

  dstImg.Copy(tmpImg, tmpRec, dstRec);
  pfe := ParentFilterEl;
  pfe.fLastImg := dstImg;
end;

//------------------------------------------------------------------------------
// TFeMergeNodeElement
//------------------------------------------------------------------------------

procedure TFeMergeNodeElement.Apply;
begin
  //should never get here ;)
end;

//------------------------------------------------------------------------------
// TFeOffsetElement
//------------------------------------------------------------------------------

procedure TFeOffsetElement.Apply;
var
  off: TPointD;
  dstOffRec: TRect;
  tmpImg: TImage32;
  pfe: TFilterElement;
begin
  if not GetSrcAndDst then Exit;
  pfe := ParentFilterEl;
  off := dxdy.GetPoint(RectD(pfe.fObjectBounds));
  off := ScalePoint(off, pfe.fScale);
  dstOffRec := dstRec;
  with Point(off) do OffsetRect(dstOffRec, X, Y);

  if srcImg = dstImg then
  begin
    tmpImg := pfe.GetNamedImage(tmpFilterImg);
    tmpImg.Copy(srcImg, srcRec, tmpImg.Bounds);
    dstImg.Clear(dstRec);
    dstImg.Copy(tmpImg, tmpImg.Bounds, dstOffRec);
  end else
  begin
    dstImg.Clear(dstRec);
    dstImg.Copy(srcImg, srcRec, dstOffRec);
  end;
end;

//------------------------------------------------------------------------------
// TClipPathElement
//------------------------------------------------------------------------------

constructor TClipPathElement.Create(parent: TElement; hashName: Cardinal);
begin
  fDrawInfo.visible := false;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TClipPathElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
var
  i: integer;
begin
  inherited;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        GetUnscaledPaths(drawInfo);
        AppendPath(self.drawPathsO, drawPathsO);
        AppendPath(self.drawPathsC, drawPathsC);
      end;
end;

//------------------------------------------------------------------------------
// TShapeElement
//------------------------------------------------------------------------------

constructor TShapeElement.Create(parent: TElement; hashName: Cardinal);
var
  className: AnsiString;
  style: AnsiString;
begin
  inherited;
  hasPaths := true;
  fDrawInfo.visible := true;
  //load any class styles
  className := LowercaseAnsi(fName);
  style := fReader.fClassStyles.GetStyle(classname);
  if style <> '' then
    ParseStyle(PAnsiChar(style), Length(style));
end;
//------------------------------------------------------------------------------

function TShapeElement.GetVal(out val: double): Boolean;
begin
  Result := ParseNextNum(fCurrent, fCurrentEnd, true, val);
end;
//------------------------------------------------------------------------------

function TShapeElement.HasMarkers: Boolean;
begin
  Result := IsStroked(fDrawInfo) and (Assigned(fDrawInfo.markerStart.name) or
    Assigned(fDrawInfo.markerMiddle.name) or Assigned(fDrawInfo.markerEnd.name));
end;
//------------------------------------------------------------------------------

procedure TShapeElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i         : integer;
  d         : double;
  scale     : double;
  stroked   : Boolean;
  filled    : Boolean;
  clipRec   : TRectD;
  clipRec2  : TRect;
  tmpImg    : TImage32;
  clipEl    : TElement;
  filterEl  : TElement;
  usingSpecialEffects: Boolean;
begin
  if not drawInfo.using then
    UpdateDrawInfo(drawInfo, fDrawInfo);

  filled := IsFilled(drawInfo);
  stroked := IsStroked(drawInfo);
  GetUnscaledPaths(drawInfo);

  if not (filled or stroked) or not hasPaths then
  begin
    inherited;
    Exit;
  end;

  MatrixApply(drawInfo.matrix, drawPathsO);
  MatrixApply(drawInfo.matrix, drawPathsC);

  fillPaths := drawPathsC;
  AppendPath(fillPaths, drawPathsO);

  if Assigned(DrawInfo.fillEl.name) then
  begin
   //check fill-opacity and update
   i := DrawInfo.fillColor shr 24;
   if (i > 0) and (i < 255) then DrawInfo.opacity := i;
  end;

  //usingSpecialEffects: Uses a temp image that'll be blended to the main image.
  //Needed when the element has a clip-path, reduced opacity or a filter effect.
  usingSpecialEffects := Assigned(fDrawInfo.clipPathEl.name) or
    (DrawInfo.opacity < 255) or Assigned(fDrawInfo.filterEl.name);

  if usingSpecialEffects then
  begin
    clipRec := NullRectD;
    clipEl := FindRefElement(fDrawInfo.clipPathEl);
    filterEl := FindRefElement(fDrawInfo.filterEl);

    if Assigned(clipEl) then
    begin
      with TClipPathElement(clipEl) do
      begin
        GetUnscaledPaths(drawInfo);
        fillPaths := drawPathsC;
        AppendPath(fillPaths, drawPathsO);

        //nb: cliprect paths may be 'open' or 'closed'
        MatrixApply(drawInfo.matrix, fillPaths);
        clipRec := GetBoundsD(fillPaths);
      end;
    end else
    begin
      clipRec := GetBoundsD(fillPaths);
      scale := ExtractAvgScaleFromMatrix(drawInfo.matrix);
      d := drawInfo.strokeWidth * scale * 0.5;
      clipRec := InflateRect(clipRec, d, d);
      clipRec := IntersectRect(clipRec, RectD(img.Bounds));
      if Assigned(filterEl) then
        with TFilterElement(filterEl) do
          clipRec := GetAdjustedBounds(clipRec, scale);
    end;

    clipRec2 := Rect(clipRec);
    clipRec2 := IntersectRect(clipRec2, img.Bounds);
    if clipRec2.IsEmpty then Exit;

    tmpImg := fReader.TempImage;
    tmpImg.Clear(clipRec2);

    if filled then
      DrawFilled(tmpImg, drawInfo);

    if stroked then
    begin
      if Assigned(drawPathsC) then
        DrawStroke(tmpImg, drawInfo, true);
      if Assigned(drawPathsO) then
        DrawStroke(tmpImg, drawInfo, false);
    end;

    if Assigned(filterEl) then
      with TFilterElement(filterEl) do
        Apply(tmpImg, clipRec2, drawInfo.matrix);

    if DrawInfo.opacity < 255 then
      tmpImg.ReduceOpacity(DrawInfo.opacity, clipRec2);

    if Assigned(clipEl) then
      with TClipPathElement(clipEl) do
        EraseOutsidePaths(tmpImg, fillPaths, fDrawInfo.fillRule, clipRec2);

    img.CopyBlend(tmpImg, clipRec2, clipRec2, BlendToAlpha);
  end else
  begin
    if filled then
      DrawFilled(img, drawInfo);
    if stroked then
    begin
      DrawStroke(img, drawInfo, true);
      DrawStroke(img, drawInfo, false);
    end;
  end;

  //todo: enable "paint-order" to change filled/stroked/marker paint order
  if HasMarkers then DrawMarkers(img, drawInfo);
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawMarkers(img: TImage32; drawInfo: TDrawInfo);
var
  i,j: integer;
  markerEl: TElement;
  strokePaths: TPathsD;
  pt1, pt2: TPointD;
begin
  drawInfo.matrix := fReader.fViewMatrix;

  //todo: currently markers are always scaled according to strokewidth
  MatrixScale(drawInfo.matrix, fDrawInfo.strokeWidth);

  if Assigned(fDrawInfo.markerStart.name) and Assigned(drawPathsO) then
  begin
    strokePaths := drawPathsO;
    markerEl := FindRefElement(fDrawInfo.markerStart);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        for i := 0 to High(Self.drawPathsO) do
        begin
          if Length(strokePaths[i]) < 2 then Continue;
          pt1 := strokePaths[i][0];
          pt2 := strokePaths[i][1];
          if autoStartReverse then
            SetEndPoint(pt1, GetAngle(pt2, pt1)) else
            SetEndPoint(pt1, GetAngle(pt1, pt2));
          Draw(img, drawInfo);
        end;
      end;
  end;

  if Assigned(fDrawInfo.markerMiddle.name) then
  begin
    if Assigned(drawPathsO) then
      strokePaths := drawPathsO else
      strokePaths := drawPathsC; //todo: check if this is necessary

    markerEl := FindRefElement(fDrawInfo.markerMiddle);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
        for i := 0 to High(strokePaths) do
          if SetMiddlePoints(strokePaths[i]) then
            Draw(img, drawInfo);
  end;

  if Assigned(fDrawInfo.markerEnd.name) and Assigned(drawPathsO) then
  begin
    strokePaths := drawPathsO;
    markerEl := FindRefElement(fDrawInfo.markerEnd);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        for i := 0 to High(strokePaths) do
        begin
          j := High(strokePaths[i]);
          if j < 1 then Continue;
          pt1 := strokePaths[i][j];
          pt2 := strokePaths[i][j-1];

          SetEndPoint(pt1, GetAngle(pt2, pt1));
          Draw(img, drawInfo);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
begin
  drawPathsO := nil; drawPathsC := nil; fillPaths := nil;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawFilled(img: TImage32; const drawInfo: TDrawInfo);
var
  rec: TRectD;
  refEl: TElement;
begin
  if not assigned(fillPaths) then Exit;

  if Assigned(drawInfo.fillEl.name) then
  begin
    refEl := FindRefElement(drawInfo.fillEl);
    if Assigned(refEl) and (refEl is TFillElement) then
    begin
      rec := GetBoundsD(fillPaths);
      if refEl is TRadGradElement then
      begin
        if TRadGradElement(refEl).PrepareRenderer(
          fReader.RadGradRenderer, drawInfo.matrix, rec) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, fReader.RadGradRenderer);
      end
      else if refEl is TLinGradElement then
      begin
        with TLinGradElement(refEl) do
          if PrepareRenderer(fReader.LinGradRenderer, drawInfo.matrix, rec) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, fReader.LinGradRenderer);
      end
      else if refEl is TPatternElement then
      begin
        with TPatternElement(refEl) do
          if PrepareRenderer(fReader.ImageRenderer, drawInfo.matrix, rec) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, fReader.ImageRenderer);
      end;
    end;
  end
  else if drawInfo.fillColor = clInvalid then
    DrawPolygon(img, fillPaths, drawInfo.fillRule, clBlack32)
  else
    DrawPolygon(img, fillPaths, drawInfo.fillRule, drawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawStroke(img: TImage32;
  const drawInfo: TDrawInfo; isClosed: Boolean);
var
  dashOffset: integer;
  dashArray: TArrayOfInteger;
  scale: Double;
  rec: TRectD;
  strokePaths: TPathsD;
  refEl: TElement;
const
  endStyle: array [Boolean] of TEndStyle = (esRound, esPolygon);
  endStyle2: array [Boolean] of TEndStyle = (esButt, esPolygon);
begin
  if isClosed then
    strokePaths := drawPathsC else
    strokePaths := drawPathsO;
  if not Assigned(strokePaths) then Exit;

  scale := ExtractAvgScaleFromMatrix(drawInfo.matrix);

  if Length(drawInfo.dashArray) > 1 then
  begin
    dashOffset := Round(drawInfo.dashOffset * scale);
    dashArray := MakeDashArray(drawInfo.dashArray, scale);
    DrawDashedLine(img, strokePaths, dashArray, @dashOffset,
      drawInfo.strokeWidth * scale, drawInfo.strokeColor, endStyle2[isClosed])
  end
  else if Assigned(drawInfo.strokeEl.name) then
  begin
    refEl := FindRefElement(drawInfo.strokeEl);
    if not Assigned(refEl) then Exit;

    rec := GetBoundsD(strokePaths);
    if refEl is TRadGradElement then
    begin
      with TRadGradElement(refEl) do
        PrepareRenderer(fReader.RadGradRenderer, drawInfo.matrix, rec);
      DrawLine(img, strokePaths, drawInfo.strokeWidth * scale,
        fReader.RadGradRenderer, endStyle[isClosed], jsAuto, scale);
    end
    else if refEl is TLinGradElement then
    begin
      with TLinGradElement(refEl) do
        PrepareRenderer(fReader.LinGradRenderer, drawInfo.matrix, rec);
      DrawLine(img, strokePaths, drawInfo.strokeWidth * scale,
        fReader.LinGradRenderer, endStyle[isClosed], jsAuto, scale);
    end
    else if refEl is TPatternElement then
    begin
      with TPatternElement(refEl) do
        PrepareRenderer(fReader.ImageRenderer, drawInfo.matrix, rec);
      DrawLine(img, strokePaths, drawInfo.strokeWidth * scale,
        fReader.ImageRenderer, endStyle[isClosed], jsAuto, scale);
    end;
  end else
  begin
    DrawLine(img, strokePaths, drawInfo.strokeWidth * scale,
      drawInfo.strokeColor, endStyle[isClosed], jsAuto, scale);
  end;
end;

//------------------------------------------------------------------------------
// TPathElement
//------------------------------------------------------------------------------

function TPathElement.GetSegType(out isRelative: Boolean): Boolean;
var
  c: AnsiChar;
begin
  Result := false;
  if not SkipBlanks(fCurrent, fCurrentEnd) then Exit;
  c := upcase(fCurrent^);
  result := CharInSet(c, ['A','C','H','M','L','Q','S','T','V','Z']);
  if not result then Exit;
  case c of
    'M': fCurrSegType := dsMove;
    'L': fCurrSegType := dsLine;
    'H': fCurrSegType := dsHorz;
    'V': fCurrSegType := dsVert;
    'A': fCurrSegType := dsArc;
    'Q': fCurrSegType := dsQBez;
    'C': fCurrSegType := dsCBez;
    'T': fCurrSegType := dsQSpline;
    'S': fCurrSegType := dsCSpline;
    'Z': fCurrSegType := dsClose;
  end;
  isRelative := fCurrent^ >= 'a';
  inc(fCurrent);
end;
//------------------------------------------------------------------------------

function TPathElement.GetSingleDigit(out digit: integer): Boolean;
begin
  Result := SkipBlanksAndComma(fCurrent, fCurrentEnd) and
    (fCurrent^ >= '0') and (fCurrent^ <= '9');
  if not Result then Exit;
  digit := Ord(fCurrent^) - Ord('0');
  inc(fCurrent);
end;
//------------------------------------------------------------------------------

procedure TPathElement.StartNewDpath;
var
  cnt: integer;
begin
  if Assigned(fCurrDpath) then
  begin
    if not Assigned(fCurrDpath.segs) then Exit;
    SetLength(fCurrSeg.vals, fCurrSegCnt); //trim the last segment;
  end;

  cnt := Length(fDpaths);
  SetLength(fDpaths, cnt +1);

  fCurrDpath := @fDpaths[cnt];
  fCurrDpath.firstPt := fLastPt;
  fCurrDpath.isClosed := False;
  fCurrDpath.segs := nil;
  fCurrSeg := nil;
end;
//------------------------------------------------------------------------------

procedure TPathElement.StartNewSeg(segType: TDsegType);
var
  cnt: integer;
begin
  if Assigned(fCurrSeg) then
    SetLength(fCurrSeg.vals, fCurrSegCnt)
  else if not Assigned(fCurrDpath) then
    StartNewDpath;

  fCurrSegType := segType;
  cnt := Length(fCurrDpath.segs);
  SetLength(fCurrDpath.segs, cnt +1);
  fCurrSeg := @fCurrDpath.segs[cnt];
  fCurrSeg.segType := segType;

  fCurrSegCap := buffSize;
  SetLength(fCurrSeg.vals, fCurrSegCap);
  fCurrSegCnt := 0;
end;
//------------------------------------------------------------------------------

procedure TPathElement.AddSegValue(val: double);
begin
  if not Assigned(fCurrSeg) then
    StartNewSeg(fCurrSegType);
  
  if fCurrSegCnt = fCurrSegCap then
  begin
    inc(fCurrSegCap, buffSize);
    SetLength(fCurrSeg.vals, fCurrSegCap);
  end;
  fCurrSeg.vals[fCurrSegCnt] := val;
  inc(fCurrSegCnt);
end;
//------------------------------------------------------------------------------

procedure TPathElement.AddSegPoint(const pt: TPointD);
begin
  AddSegValue(pt.X); AddSegValue(pt.Y);
end;
//------------------------------------------------------------------------------

function TPathElement.Get2Num(var pt: TPointD; isRelative: Boolean): Boolean;
begin
  Result := GetVal(pt.X) and GetVal(pt.Y);
  if not Result or not isRelative then Exit;
  pt.X := pt.X + fLastPt.X;
  pt.Y := pt.Y + fLastPt.Y;
end;
//------------------------------------------------------------------------------

procedure TPathElement.ParseD(attrib: PAttrib);
var
  i: integer;
  d: double;
  currPt: TPointD;
  isRelative: Boolean;
begin
  fCurrent := attrib.aValue.name;
  fCurrentEnd := fCurrent + attrib.aValue.len;

  isRelative := false;
  currPt := NullPointD;

  while GetSegType(isRelative) do
  begin
    fLastPt := currPt;

    if (fCurrSegType = dsMove) then
    begin
      if Assigned(fCurrSeg) then
      begin
        SetLength(fCurrSeg.vals, fCurrSegCnt); //trim buffer
        fCurrDpath.isClosed := false;
      end;
      fCurrDpath := nil;
      fCurrSeg := nil;

      if not Get2Num(currPt, isRelative) then break;
      self.fLastPt :=  currPt;

      //values immediately following a Move are implicitly Line statements
      if IsNumPending(fCurrent, fCurrentEnd, true) then
        fCurrSegType := dsLine else
        Continue;
    end
    else if (fCurrSegType = dsClose) then
    begin
      if not Assigned(fCurrSeg) then break;
      SetLength(fCurrSeg.vals, fCurrSegCnt); //trim buffer
      fCurrDpath.isClosed := true;
      fCurrDpath := nil;
      fCurrSeg := nil;
      Continue;
    end;

    if Assigned(fCurrSeg) then
      SetLength(fCurrSeg.vals, fCurrSegCnt); //trim buffer
    fCurrSeg := nil;

    case fCurrSegType of
      dsHorz:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          GetVal(currPt.X);
          if isRelative then
            currPt.X := currPt.X + fLastPt.X;
          AddSegValue(currPt.X);
        end;

      dsVert:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          GetVal(currPt.Y);
          if isRelative then
            currPt.Y := currPt.Y + fLastPt.Y;
          AddSegValue(currPt.Y);
        end;

      dsLine:
        while true do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
          SkipBlanks(fCurrent, fCurrentEnd);
          if IsNumPending(fCurrent, fCurrentEnd, true) then Continue;
          if LowerCaseTable[fCurrent^] = 'l' then GetSegType(isRelative)
          else break;
        end;

      dsQSpline:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;

      dsCSpline:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;

      dsQBez:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;

      dsCBez:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;

      dsArc:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not GetVal(currPt.X) or
            not GetVal(currPt.Y) then break;
          AddSegPoint(currPt);                              //radii
          if GetVal(d) then AddSegValue(d);                 //angle
          if not GetSingleDigit(i) then break;              //arc-flag
          AddSegValue(i);
          if not GetSingleDigit(i) then break;              //sweep-flag
          AddSegValue(i);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;
    end;
  end;
  if Assigned(fCurrSeg) then
    SetLength(fCurrSeg.vals, fCurrSegCnt); //trim buffer
  fCurrent    := fCurrentEnd +1;
  fCurrentEnd := fReader.fEndStream;
end;
//------------------------------------------------------------------------------

procedure TPathElement.Flatten(index: integer; scalePending: double;
  out path: TPathD; out isClosed: Boolean);
begin
  isClosed := fDpaths[index].isClosed;
  path := fDpaths[index].GetFlattened(scalePending);
end;
//------------------------------------------------------------------------------

procedure TPathElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
var
  i: integer;
  scalePending: double;
  isClosed: Boolean;
  path: TPathD;
begin
  inherited;
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  for i := 0 to High(fDpaths) do
  begin
    Flatten(i, scalePending, path, isClosed);
    if not Assigned(path) then Continue;
    if isClosed then
      AppendPath(drawPathsC, path) else
      AppendPath(drawPathsO, path);
  end;
end;

//------------------------------------------------------------------------------
// TPolyElement
//------------------------------------------------------------------------------

procedure TPolyElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
begin
  inherited;
  if not Assigned(path) then Exit;
  if (fNameHash = hPolygon) then
    AppendPath(drawPathsC, path) else   //hPolygon
    AppendPath(drawPathsO, path);       //hPolyline
end;
//------------------------------------------------------------------------------

procedure TPolyElement.ParsePoints(attrib: PAttrib);
var
  currCnt, currCap: integer;

  procedure AddPoint(const pt: TPointD);
  begin
    if currCnt = currCap then
    begin
      currCap := currCap + buffSize;
      SetLength(path, currCap);
    end;
    path[currCnt] := pt;
    inc(currCnt);
  end;

var
  pt: TPointD;
begin
  currCnt     := 0;
  currCap     := buffSize;
  fCurrent := attrib.aValue.name;
  SetLength(path, currCap);
  while IsNumPending(fCurrent, fCurrentEnd, true) and
    GetVal(pt.X) and GetVal(pt.Y) do
      AddPoint(pt);
  SetLength(path, currCnt);
end;

//------------------------------------------------------------------------------
// TLineElement
//------------------------------------------------------------------------------

constructor TLineElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  SetLength(path, 2);
  path[0] := NullPointD; path[1] := NullPointD;
end;
//------------------------------------------------------------------------------

procedure TLineElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
begin
  inherited;
  AppendPath(drawPathsO, path);
end;

//------------------------------------------------------------------------------
// TCircleElement
//------------------------------------------------------------------------------

procedure TCircleElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
var
  scalePending : double;
  rec   : TRectD;
  pt    : TPointD;
  path  : TPathD;
  r: double;
begin
  inherited;
  if not radius.IsValid then Exit;
  r := radius.GetValueXY(fReader.rawRect);
  pt := centerPt.GetPoint(fReader.rawRect);
  rec := RectD(pt.X -r, pt.Y -r, pt.X +r, pt.Y +r);
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  path := Ellipse(rec, scalePending);
  AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TEllipseElement
//------------------------------------------------------------------------------

procedure TEllipseElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
var
  scalePending  : double;
  rec       : TRectD;
  path      : TPathD;
  rad       : TPointD;
  centPt    : TPointD;
begin
  inherited;
  rad := radius.GetPoint(fReader.rawRect);
  centPt := centerPt.GetPoint(fReader.rawRect);
  with centPt do
    rec := RectD(X -rad.X, Y -rad.Y, X +rad.X, Y +rad.Y);
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  path := Ellipse(rec, scalePending);
  AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TRectElement
//------------------------------------------------------------------------------

procedure TRectElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
var
  rad: TPointD;
  rec: TRectD;
  path: TPathD;
begin
  inherited;
  rec := elRectWH.GetRect(fReader.rawRect);
  if rec.IsEmpty then Exit;

  rad := radius.GetPoint(rec);
  if (rad.X > 0) or (rad.Y > 0) then
  begin
    if (rad.X <= 0) then rad.X := rad.Y
    else if (rad.Y <= 0) then rad.Y := rad.X;
    //nb: elliptical rect rounding is not supported.
    path := RoundRect(rec, Average(rad.X, rad.Y))
  end else
    path := Rectangle(rec);
  AppendPath(drawPathsC, path);
end;

//------------------------------------------------------------------------------
// TTextElement
//------------------------------------------------------------------------------

constructor TTextElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  hasPaths := false;
end;
//------------------------------------------------------------------------------

procedure TTextElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
begin
  //deferred to TSubtextElement.GetDrawPaths
end;
//------------------------------------------------------------------------------

procedure TTextElement.ResetTmpPt;
begin
  tmpPt := InvalidPointD;
end;
//------------------------------------------------------------------------------

function TTextElement.AddSubtext(atext: PAnsiChar; len: integer): TElement;
begin
  Result := TSubtextElement.Create(self, 0);
  with TSubtextElement(Result) do
  begin
    fName.name := text; //todo: check this
    fName.len := 0;
    text := atext;
    textLen := len;
    elRectWH.left.Init;
    elRectWH.top.Init;
  end;
  fChilds.add(Result);
end;
//------------------------------------------------------------------------------

procedure TTextElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
begin
  if not IsIdentityMatrix(fDrawInfo.matrix) then
    drawInfo.matrix := MatrixMultiply(drawInfo.matrix, fDrawInfo.matrix);
  for i := 0 to fChilds.Count -1 do
    with TElement(fChilds[i]) do
      if fDrawInfo.visible then Draw(img, drawInfo);
end;

//------------------------------------------------------------------------------
// TSubtextElement
//------------------------------------------------------------------------------

constructor TSubtextElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  hasPaths := true;
end;
//------------------------------------------------------------------------------

procedure TSubtextElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
var
  tmpPt: TPointD;
  parentTextEl, topTextEl: TTextElement;
  isFirst: Boolean;
  s: string;
  i: integer;
  tmpX, offsetX, scale: double;
  mat: TMatrixD;
begin
  inherited;
  fReader.GetBestFont(fFontInfo);
  if (fFontInfo.size < 2) or not Assigned(fReader.fFontCache) then Exit;

  parentTextEl := TTextElement(fParent);
  topTextEl := parentTextEl;
  isFirst := (self = fParent.fChilds[0]);
  while topTextEl.fNameHash <> hText do
  begin
    isFirst := isFirst and (topTextEl = topTextEl.fParent.fChilds[0]);
    topTextEl := TTextElement(topTextEl.fParent);
  end;

  //if first subtext then reset X offset
  if isFirst then topTextEl.ResetTmpPt;

  //get the text offset
  tmpPt.Y := InvalidD;
  if (parentTextEl is TTSpanElement) and
    (self = parentTextEl.fChilds[0]) and
    parentTextEl.elRectWH.left.IsValid then
      tmpPt.X := parentTextEl.elRectWH.left.rawVal
  else if IsValid(topTextEl.tmpPt.X) then
    tmpPt := topTextEl.tmpPt
  else if topTextEl.elRectWH.left.IsValid then
    tmpPt.X := topTextEl.elRectWH.left.rawVal
  else
    tmpPt.X := 0;

  if not IsValid(tmpPt.Y) then
  begin
    if (parentTextEl is TTSpanElement) and
      (self = parentTextEl.fChilds[0]) and
      parentTextEl.elRectWH.top.IsValid then
        tmpPt.Y := parentTextEl.elRectWH.top.rawVal
    else if topTextEl.elRectWH.top.IsValid then
      tmpPt.Y := topTextEl.elRectWH.top.rawVal
    else
      tmpPt.Y := 0;
  end;

  topTextEl.tmpPt := tmpPt;

  if textLen = 0 then
    Exit; //nb: only exit AFTER setting parentTextEl.tmpPt.

  //trim CRLFs and multiple spaces
  {$IFDEF UNICODE}
  s := UTF8ToUnicodeString(HtmlDecode(GetUtf8String(text, textLen)));
  {$ELSE}
  s := HtmlDecode(GetUtf8String(text, textLen));
  {$ENDIF}
  for i := 1 to Length(s) do
    if s[i] < #32 then s[i] := #32;
  i := Pos(#32#32, s);
  while i > 0 do
  begin
    Delete(s, i, 1);
    i := PosEx(#32#32, s, i);
  end;

  drawPathsC := fReader.fFontCache.GetTextGlyphs(0, 0, s, tmpX);
  //by not changing the fontCache.FontHeight, the quality of
  //small font render improves very significantly (though of course
  //this requires additional glyph scaling and offsetting).
  scale := fFontInfo.size/fReader.fFontCache.FontHeight;
  mat := IdentityMatrix;
  MatrixScale(mat, scale);

  with topTextEl.tmpPt do
    case fFontInfo.align of
      taLeft:
        begin
          offsetX := X;
          X := X + tmpX * scale;
        end;
      taRight:
        begin
          offsetX := X - tmpX * scale;
          X := offsetX;
        end;
      else //taMiddle:
        begin
          offsetX := X - tmpX * scale /2;
          X := X + tmpX * scale/2;
        end;
    end;

  MatrixTranslate(mat, offsetX, tmpPt.Y);
  MatrixApply(mat, drawPathsC);
end;

//------------------------------------------------------------------------------
// TTSpanElement
//------------------------------------------------------------------------------

constructor TTSpanElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  elRectWH.Init;
  tmpPt := InvalidPointD;
  delta := InvalidPointD;
end;

//------------------------------------------------------------------------------
// TTextPathElement
//------------------------------------------------------------------------------

procedure TTextPathElement.GetUnscaledPaths(const drawInfo: TDrawInfo);
var
  tmpPt: TPointD;
  parentTextEl, topTextEl: TTextElement;
  el: TElement;
  isFirst: Boolean;
  s: string;
  i, charsThatFit: integer;
  tmpX, offsetX, scale, spacing, fh: double;
  utf8String: AnsiString;
  mat: TMatrixD;
  path: TPathD;
  tmpPaths: TPathsD;
  isClosed: Boolean;
begin
  drawPathsO := nil; drawPathsC := nil; fillPaths := nil;
  fReader.GetBestFont(fFontInfo);
  if (fFontInfo.size < 2) or not Assigned(fReader.fFontCache) then Exit;

  parentTextEl := TTextElement(fParent);
  topTextEl := parentTextEl;
  isFirst := (self = fParent.fChilds[0]);
  while topTextEl.fNameHash <> hText do
  begin
    isFirst := isFirst and (topTextEl = topTextEl.fParent.fChilds[0]);
    topTextEl := TTextElement(topTextEl.fParent);
  end;

  //if first subtext then reset X offset
  if not isFirst then Exit;
  topTextEl.ResetTmpPt;
  utf8String := '';

  //nb: only exit AFTER setting parentTextEl.tmpPt.
  if textLen = 0 then
  begin
    if (fChilds.Count = 0) or
      not (TElement(fChilds[0]) is TTSpanElement) then
        Exit;
    el := TElement(fChilds[0]);
    if (el.fChilds.Count = 0) or
      not (TElement(el.fChilds[0]) is TSubtextElement) then
        Exit;
    with TSubtextElement(el.fChilds[0]) do
    begin
      utf8String := GetUtf8String(text, textLen);
      spacing := fFontInfo.spacing;
    end;
  end else
  begin
    utf8String := GetUtf8String(text, textLen);
    spacing := fFontInfo.spacing;
  end;

  //trim CRLFs and multiple spaces
  {$IFDEF UNICODE}
  s := UTF8ToUnicodeString(HtmlDecode(utf8String));
  {$ELSE}
  s := HtmlDecode(utf8String);
  {$ENDIF}
  for i := 1 to Length(s) do
    if s[i] < #32 then s[i] := #32;

  el := FindRefElement(pathEl);
  if not (el is TPathElement) then Exit;
  scale := fFontInfo.size/fReader.fFontCache.FontHeight;
  with TPathElement(el) do
  begin
    mat := fDrawInfo.matrix;
    MatrixScale(mat, 1/scale);
    for i := 0 to High(fDpaths) do
    begin
      Flatten(i, scale, path, isClosed);
      MatrixApply(mat, path);
      AppendPath(self.drawPathsC,
        GetTextGlyphsOnPath(s, path, fReader.fFontCache,
          taLeft, 0, spacing/scale, charsThatFit));
      if charsThatFit = Length(s) then Break;
      Delete(s, 1, charsThatFit);
    end;
  end;
  drawPathsC := ScalePath(drawPathsC, scale);
end;

//------------------------------------------------------------------------------
// TMarkerElement
//------------------------------------------------------------------------------

constructor TMarkerElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i,j: integer;
  di: TDrawInfo;
  l,t,w,h,scale: double;
  mat: TMatrixD;
begin
  w := elRectWH.width.rawVal;
  h := elRectWH.height.rawVal;
  mat := drawInfo.matrix;
  if elRectWH.width.IsValid and elRectWH.height.IsValid and
    not markerBoxWH.IsEmpty then
      MatrixScale(mat, w/markerBoxWH.Width, h/markerBoxWH.Height);

  if refPt.X.IsValid and refPt.Y.IsValid then
  begin
    l := refPt.X.rawVal;
    t := refPt.Y.rawVal;
    scale := ExtractAvgScaleFromMatrix(mat);
    MatrixTranslate(mat, -l * scale, -t * scale);
  end;

  MatrixRotate(mat, NullPointD, angle);

  //for each 'point' draw the marker
  for i := 0 to High(fPoints) do
  begin
    di := defaultDrawInfo;
    di.matrix := mat;
    MatrixTranslate(di.matrix, fPoints[i].X, fPoints[i].Y);

    //for each marker shape (though there's very rarely more than one)
    for j := 0 to fChilds.Count -1 do
      if TElement(fChilds[j]) is TShapeElement then
        with TShapeElement(fChilds[j]) do
          Draw(img, di);
  end;
end;
//------------------------------------------------------------------------------

procedure TMarkerElement.SetEndPoint(const pt: TPointD; angle: double);
begin
  SetLength(fPoints, 1);
  fPoints[0] := pt;
  self.angle := angle;
end;
//------------------------------------------------------------------------------

function TMarkerElement.SetMiddlePoints(const points: TPathD): Boolean;
var
  len: integer;
begin
  angle := 0;
  len := Length(points);
  Result := len > 2;
  if Result then
    Self.fPoints := Copy(points, 1, len -2);
end;

//------------------------------------------------------------------------------
// TPatternElement
//------------------------------------------------------------------------------

constructor TPatternElement.Create(parent: TElement; hashName: Cardinal);
begin
  pattBoxWH.Width   := InvalidD;
  pattBoxWH.Height  := InvalidD;
  inherited;
end;
//------------------------------------------------------------------------------

function TPatternElement.PrepareRenderer(renderer: TImageRenderer;
  matrix: TMatrixD; const rec: TRectD): Boolean;
var
  i: integer;
  recWH: TRectWH;
  scalePending, sx,sy: double;
  el: TElement;
  closedPaths, openPaths: TPathsD;
  drawinfo: TDrawInfo;
begin
  Result := false;
  scalePending := ExtractAvgScaleFromMatrix(matrix);

  //todo: implement patternUnits & patternContentUnits too

  //rec = objectBoundingBox (default)
  //fReader.viewbox = userSpaceOnUse

  sx := 1; sy := 1;
  if elRectWH.Width.IsValid and elRectWH.Height.IsValid then
  begin
    recWH := elRectWH.GetRectWH(ScaleRect(rec, 1/scalePending));
    if not pattBoxWH.IsEmpty then
    begin
      sx := recWH.Width/pattBoxWH.Width;
      sy := recWH.Height/pattBoxWH.Height;
    end;
    renderer.Image.SetSize(
      Round(recWH.Width  * scalePending),
      Round(recWH.Height * scalePending));
  end
  else if not pattBoxWH.IsEmpty then
  begin
    recWH.Width   := pattBoxWH.Width;
    recWH.Height  := pattBoxWH.Width;
  end else
    Exit;

  Result := true;
  closedPaths := nil;
  openPaths   := nil;

  if Assigned(refEl.name) then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TShapeElement) then
      with TShapeElement(el) do
      begin
        drawinfo := fDrawInfo;
        MatrixScale(drawinfo.matrix, scalePending*sx, scalePending*sy);
        Draw(renderer.Image, drawinfo);
      end;
  end;

  for i := 0 to fChilds.Count -1 do
    with TElement(fChilds[i]) do
    begin
      drawinfo := fDrawInfo;
      MatrixScale(drawinfo.matrix, scalePending*sx, scalePending*sy);
      Draw(renderer.Image, drawinfo);
    end;
end;

//------------------------------------------------------------------------------
// TStyleElement
//------------------------------------------------------------------------------

function TStyleElement.LoadContent: Boolean;
var
  i, len, cap: integer;
  hash: Cardinal;
  dotted: Boolean;
  aclassName: TAnsiName;
  aStyle: TAnsiName;
  names: array of string;

  procedure AddName(const name: string);
  begin
    if len = cap then
    begin
      cap := cap + buffSize;
      SetLength(names, cap);
    end;
    names[len] := name;
    inc(len);
  end;

begin
  Result := false;
  len := 0; cap := 0;
  if (PeekNextChar = '<') and ((fCurrent+1)^ = '!') and
    ((fCurrent+2)^ = '[') and ((fCurrent+8)^ = '[') then
  begin
    aclassName.name := fCurrent + 3;
    aclassName.len := 5;
    if (GetHashedName(aclassName) <> hCDATA) then Exit;
    inc(fCurrent, 9);
  end;

  while SkipStyleBlanks(fCurrent,fCurrentEnd) and
    CharInSet(LowerCaseTable[PeekNextChar], ['.', '#', 'a'..'z']) do
  begin
    dotted := (fCurrent^ < 'a');  // ie '.' or '#'
    //get one or more class names for each pending style
    if dotted then inc(fCurrent);
    GetName(aclassName);
    if dotted then
    begin
      Dec(aclassName.name);
      inc(aclassName.len);
    end;

    AddName(String(LowercaseAnsi(aclassName)));
    if PeekNextChar = ',' then
    begin
      inc(fCurrent);
      Continue;
    end;
    if len = 0 then break;
    SetLength(names, len); //ie no more comma separated names

    //now get the style
    if PeekNextChar <> '{' then Break;
    inc(fCurrent);
    aStyle.name := fCurrent;
    while (fCurrent < fCurrentEnd) and (fCurrent^ <> '}') do
      inc(fCurrent);
    if (fCurrent = fCurrentEnd) then break;
    aStyle.len := fCurrent - aStyle.name;

    //finally, for each class name add (or append) this style
    for i := 0 to High(names) do
      fReader.fClassStyles.AddAppendStyle(names[i], LowercaseAnsi(aStyle));
    names := nil;
    len := 0; cap := 0;
    inc(fCurrent);
  end;

  //skip trailing unknown content
  while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
  //we should now be at </STYLE>
  if (PeekNextChar <> '<') then Exit;
  if ((fCurrent +1)^ <> '/') then Exit;
  inc(fCurrent, 2);
  Result := HashCurrentWord(hash) and (fNameHash = hash);
  if Result then inc(fCurrent); //trailing '>'
end;

//------------------------------------------------------------------------------
// TSvgElement
//------------------------------------------------------------------------------

constructor TSvgElement.Create(reader: TSvgReader; hashName: Cardinal);
var
  svgStart: PAnsiChar;
begin
  inherited Create(nil, hashName);

  fReader    := reader;
  with fReader do
    if not GetSvgStart(svgStart) then
    begin
      fCurrentEnd := PAnsiChar(fMemStream.Memory);
      Exit;
    end;

  fName.name := svgStart;
  fName.len := 3;
  inc(svgStart, 3);
  fCurrent := svgStart;
  fCurrentEnd := fReader.fEndStream;
  fDrawInfo :=  defaultDrawInfo;
end;

//------------------------------------------------------------------------------
// TElement
//------------------------------------------------------------------------------

constructor TElement.Create(parent: TElement; hashName: Cardinal);
begin
{$IFDEF XPLAT_GENERICS}
  fChilds         := TList<TElement>.create;
{$ELSE}
  fChilds         := TList.Create;
{$ENDIF}
  fNameHash       := hashName;
  fIdAttribIdx    := -1;
  fStyleAttribIdx := -1;
  if not Assigned(parent) then Exit;

  self.fParent := parent;
  fReader      := parent.fReader;
  fCurrentEnd  := fReader.fEndStream;
  fDrawInfo    := parent.fDrawInfo;
  fFontInfo     := parent.fFontInfo;

  fDrawInfo.matrix := IdentityMatrix;
  fDrawInfo.clipPathEl.name := nil;
  fCurrent     := parent.fCurrent;
  if SkipBlanks(fCurrent, fCurrentEnd) then GetName(fName);
  elRectWH.Init;
end;
//------------------------------------------------------------------------------

destructor  TElement.Destroy;
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    TElement(fChilds[i]).Free;
  fChilds.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TElement.GetName(out name: TAnsiName): Boolean;
begin
  name.len := 0;
  Result := IsAlpha(fCurrent^);
  if not Result then Exit;
  name.name := fCurrent;
  name.len := ParseNameLength(fCurrent, fCurrentEnd);
end;
//------------------------------------------------------------------------------

function TElement.HashCurrentWord(out hash: Cardinal): Boolean;
begin
  Result := IsAlpha(fCurrent^);
  if not Result then Exit;
  hash := ParseNextWordHashed(fCurrent, fCurrentEnd);
end;
//------------------------------------------------------------------------------

function TElement.SetAttribValue(attrib: PAttrib): Boolean;
begin
  Result := fCurrent^ = '"';
  if not Result then Exit;
  inc(fCurrent);
  attrib.aValue.name := fCurrent;
  while (fCurrent < fCurrentEnd) and (fCurrent^ <> '"')  do inc(fCurrent);
  attrib.avalue.len := fCurrent - attrib.aValue.name;
  TrimBlanks(attrib.aValue);
end;
//------------------------------------------------------------------------------

function TElement.AddAttribute: PAttrib;
var
  len: integer;
begin
  len := Length(fAttribs);
  setLength(fAttribs, len +1);
  Result := @fAttribs[len];
  Result.aOwnerEl := self;
end;
//------------------------------------------------------------------------------

function TElement.LoadAttributes: Boolean;
var
  attrib: PAttrib;
  nextCurrent: PAnsiChar;
begin
  Result := false;
  fHasContent := true;
  while SkipBlanks(fCurrent, fCurrentEnd) do
  begin
    if (fCurrent^ = '/') or (fCurrent^ = '>') then
    begin
      fHasContent := (fCurrent^ <> '/');
      if fHasContent then
        inc(fCurrent) else
        inc(fCurrent, 2);
      Result := true;
      break;
    end;
    attrib := AddAttribute;
    GetName(attrib.aName);
    if ParseNextChar(fCurrent, fCurrentEnd) <> '=' then Exit;
    SkipBlanks(fCurrent, fCurrentEnd);
    SetAttribValue(attrib);
    nextCurrent := fCurrent +1;
    //nb: ProcessAttrib() below sometimes adds additional attributes
    //which may cause the 'fAttribs' array to reallocate memory.
    //So the 'attrib' pointer isn't safe to use after calling ProcessAttrib().
    ProcessAttrib(attrib);
    fCurrent := nextCurrent;
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.ProcessAttrib(const attrib: PAttrib);
var
  i: integer;
  hash: Cardinal;
begin
  hash := GetHashedName(attrib.aName);
  i := AttribFuncList.IndexOf(inttohex(hash, 8));
  if i >= 0 then
    TAttribFunc(AttribFuncList.Objects[i])(attrib);
end;
//------------------------------------------------------------------------------

//function TElement.GetSvgElement: TSvgElement;
//var
//  el: TElement;
//begin
//  el := fParent;
//  while Assigned(el) and not (el is TSvgElement) do el := el.fParent;
//  if Assigned(el) then
//    Result := TSvgElement(el) else
//    Result := nil;
//end;
//------------------------------------------------------------------------------

function TElement.FindRefElement(refname: TAnsiName): TElement;
var
  i: integer;
begin
  result := nil;
  if not Assigned(refname.name) then Exit;

  if (refname.name^ = 'u') and
    ((refname.name +1)^ = 'r') and
    ((refname.name +2)^ = 'l') and
    ((refname.name +3)^ = '(') then
  begin
    inc(refname.name, 4);
    dec(refName.len, 5); //removes trailing ')'
  end;

  if refname.name^ = '#' then
  begin
    inc(refname.name);
    dec(refName.len);
  end;

  i := fReader.fIdList.IndexOf(string(LowercaseAnsi(refname)));
  if i >= 0 then
    Result := TElement(fReader.fIdList.Objects[i]) else
    Result := nil;
end;
//------------------------------------------------------------------------------

function TElement.LoadChild: TElement;
var
  hashedName: Cardinal;
  name: TAnsiName;
  savedCurrent: PAnsiChar;
  aClassName, style: AnsiString;
  elClass: TElementClass;
begin
  Result := nil;
  savedCurrent := fCurrent;
  if not GetName(name) then Exit;
  fCurrent := savedCurrent;
  hashedName := GetHashedName(name);

  elClass := HashToElementClass(hashedName);
  Result := elClass.Create(self, hashedName);

  if not Assigned(Result.fName.name) or
    not Result.LoadAttributes then
  begin
    FreeAndNil(Result);
    Exit;
  end;

  //parse 'style' AFTER other attributes have been set
  //since style values have precedence.
  with Result do
  begin
    if fStyleAttribIdx >= 0 then
      with fAttribs[fStyleAttribIdx] do
        ParseStyle(aValue.name, aValue.len);
    if fIdAttribIdx >= 0 then
      with fAttribs[fIdAttribIdx] do
      begin
        aClassName := '#' + LowercaseAnsi(aValue);
        style := fReader.fClassStyles.GetStyle(aClassname);
        if style <> '' then
          ParseStyle(PAnsiChar(style), Length(style));
      end;
  end;

  fChilds.Add(Result);
  fCurrent := Result.fCurrent;
end;
//------------------------------------------------------------------------------

function TElement.PeekNextChar: AnsiChar;
begin
  if not SkipBlanks(fCurrent, fCurrentEnd) then
    Result := #0 else
    Result := fCurrent^;
end;
//------------------------------------------------------------------------------

procedure TElement.ParseClassAttrib(classAttrib: PAttrib);
var
  style, aClassName: AnsiString;
begin
  with classAttrib^ do
      aClassName := '.' + LowercaseAnsi(aValue);
  with classAttrib.aOwnerEl.fReader do
    style := fClassStyles.GetStyle(aClassName);
  if style <> '' then
    ParseStyle(PAnsiChar(style), Length(style));
end;
//------------------------------------------------------------------------------

procedure TElement.ParseStyle(classStyle: PAnsiChar; len: integer);
var
  newAt: PAttrib;
  styleName, styleVal: TAnsiName;
  current, endCurrent: PAnsiChar;
begin
  current := classStyle;
  endCurrent := current + len;

  while SkipStyleBlanks(current, endCurrent) do
  begin
    styleName.name := current;
    styleName.len := ParseStyleNameLen(current, endCurrent);
    if styleName.len = 0 then Break;

    newAt := AddAttribute;
    newAt.aName := styleName;

    if (ParseNextChar(current, endCurrent) <> ':') or  //syntax check
      not SkipBlanks(current,endCurrent) then Break;
    styleVal.name := current;
    inc(current);
    while (current < endCurrent) and (current^ <> ';') do inc(current);
    styleVal.len := current - styleVal.name;
    newAt.aValue := styleVal;
    TrimBlanks(newAt.aValue);
    inc(current);
    ProcessAttrib(newAt);
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.ParseTransform(const transform: TAnsiName);
var
  i: integer;
  current, endC: PAnsiChar;
  c: AnsiChar;
  word: TAnsiName;
  values: array[0..5] of double;
  mat: TMatrixD;
  pMat: PMatrixD;
  mats: array of TMatrixD;

  function NewMatrix: PMatrixD;
  var
    len: integer;
  begin
    len := length(mats);
    setLength(mats, len +1);
    mats[len] := IdentityMatrix;
    Result := @mats[len];
  end;

begin
  current := transform.name;
  endC := current + transform.len;

  //surprisingly, and I think this is a bug in SVG not a feature ...
  //transform operations must be performed in reverse order
  while ParseNextWord(current, endC, word) do
  begin
    if (word.len < 5) then Exit;
    c := LowerCaseTable[(word.name +4)^];
    if ParseNextChar(current, endC) <> '(' then Exit; //syntax check
    //reset values variables
    for i := 0 to High(values) do values[i] := InvalidD;
    //and since every transform function requires at least one value
    if not ParseNextNum(current, endC, false, values[0]) then Exit;
    //now get additional variables
    i := 1;
    while (i < 6) and IsNumPending(current, endC, true) do
    begin
      ParseNextNum(current, endC, true, values[i]);
      inc(i);
    end;
    if ParseNextChar(current, endC) <> ')' then Exit; //syntax check

    pMat := NewMatrix;
    case c of
      'e' : //scalE
        if not IsValid(values[1]) then
          MatrixScale(pMat^, values[0]) else
            MatrixScale(pMat^, values[0], values[1]);
      'i' : //matrIx
        if IsValid(values[5]) then
        begin
          pMat[0,0] :=  values[0];
          pMat[0,1] :=  values[1];
          pMat[1,0] :=  values[2];
          pMat[1,1] :=  values[3];
          pMat[2,0] :=  values[4];
          pMat[2,1] :=  values[5];
        end;
      's' : //tranSlateX, tranSlateY & tranSlate
        if word.len =10  then
        begin
          if LowerCaseTable[(word.name +9)^] = 'x' then
            MatrixTranslate(pMat^, values[0], 0)
          else if LowerCaseTable[(word.name +9)^] = 'y' then
            MatrixTranslate(pMat^, 0, values[0]);
        end
        else if IsValid(values[1]) then
          MatrixTranslate(pMat^, values[0], values[1])
        else
          MatrixTranslate(pMat^, values[0], 0);
      't' : //rotaTe
        if IsValid(values[2]) then
          MatrixRotate(pMat^, PointD(values[1],values[2]), DegToRad(values[0]))
        else
          MatrixRotate(pMat^, NullPointD, DegToRad(values[0]));
       'x' : //skewX
         begin
            MatrixSkew(pMat^, DegToRad(values[0]), 0);
         end;
       'y' : //skewY
         begin
            MatrixSkew(pMat^, 0, DegToRad(values[0]));
         end;
    end;
  end;

  if not assigned(mats) then Exit;
  mat := mats[0];
  for i := 1 to High(mats) do
    mat := MatrixMultiply(mat, mats[i]);
  fDrawInfo.matrix := MatrixMultiply(fDrawInfo.matrix, mat);
end;
//------------------------------------------------------------------------------

function TElement.LoadContent: Boolean;
var
  c: PAnsiChar;
  el: TElement;
  hash: Cardinal;
begin
  Result := false;
  while SkipBlanks(fCurrent, fCurrentEnd) do
  begin
    if (fCurrent^ = '<') then
    begin
      inc(fCurrent);
      if fCurrent^ = '!' then
      begin
        //a comment section
        while (fCurrent < fCurrentEnd) do
        begin
          if (fCurrent^ = '>') and
            ((fCurrent -1)^ = '-') and
            ((fCurrent -2)^ = '-') then
          begin
            inc(fCurrent);
            break; //break out of inner comment loop
          end;
          inc(fCurrent);
        end;
      end
      else if fCurrent^ = '/' then
      begin
        //this should be the element close
        inc(fCurrent);
        Result := HashCurrentWord(hash) and (fNameHash = hash);
        if Result then
        begin
          SkipBlanks(fCurrent, fCurrentEnd);
          Result := fCurrent^ = '>';
          inc(fCurrent);
        end;
        Exit;
      end else
      begin
        //load a child element
        el := LoadChild;
        if not Assigned(el) then Break;
        if el.fHasContent then el.LoadContent;
        fCurrent := el.fCurrent;
      end;
    end
    else if fCurrent^ = '>' then
    begin
      //oops! something's wrong
      inc(fCurrent);
      break;
    end
    else if (self is TTextElement) or (self is TTextPathElement) then
    begin
      while (fCurrent -1)^ = #32 do dec(fCurrent);
      c := fCurrent;
      while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
      if (self is TTextElement) then
        TTextElement(self).AddSubtext(c, fCurrent - c)
      else
      begin
        TTextPathElement(self).text := c;
        TTextPathElement(self).textLen := fCurrent - c;
      end;
    end else
    begin
      //skip unknown content
      while (fCurrent < fCurrentEnd) and (fCurrent^ <> '<') do inc(fCurrent);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
  clipEl  : TElement;
  tmpImg  : TImage32;
  clipRec : TRect;
begin
  if fChilds.Count = 0 then
    Exit; //only 'shape' elements are drawn.

  if not IsIdentityMatrix(fDrawInfo.matrix) then
    drawInfo.matrix := MatrixMultiply(drawInfo.matrix, fDrawInfo.matrix);

  clipEl := FindRefElement(fDrawInfo.clipPathEl);
  if Assigned(clipEl) then
  begin
    with TClipPathElement(clipEl) do
    begin
      GetUnscaledPaths(drawInfo);
      MatrixApply(drawInfo.matrix, drawPathsC);
      MatrixApply(drawInfo.matrix, drawPathsO);

      fillPaths := CopyPaths(drawPathsC);
      AppendPath(fillPaths, drawPathsO);
      clipRec := GetBounds(fillPaths);
    end;
    if IsEmptyRect(clipRec) then Exit;

    tmpImg := TImage32.Create(img.Width, img.Height);
    try
      for i := 0 to fChilds.Count -1 do
        with TElement(fChilds[i]) do
          if fDrawInfo.visible then Draw(tmpImg, drawInfo);

      with TClipPathElement(clipEl) do
        EraseOutsidePaths(tmpImg, fillPaths, fDrawInfo.fillRule, clipRec);
      img.CopyBlend(tmpImg, clipRec, clipRec, BlendToAlpha);

    finally
      tmpImg.Free;
    end;

  end else
  begin
    for i := 0 to fChilds.Count -1 do
      with TElement(fChilds[i]) do
        if fDrawInfo.visible then
        begin
          Draw(img, drawInfo);
          if Self is TSwitchElement then break;
        end;
  end;
end;

//------------------------------------------------------------------------------
// TSvgReader
//------------------------------------------------------------------------------

constructor TSvgReader.Create;
begin
  fMemStream        := TMemoryStream.Create;
  fIdList           := TStringList.Create;
  //fIdList.Duplicates := dupError; // I'm unsure how best to manage this, but
  fIdList.Duplicates := dupIgnore;  // pro tem, duplicates will be ignored.
  fIdList.Sorted    := True;

  fClassStyles  := TClassStylesList.Create;

  fLinGradRenderer  := TLinearGradientRenderer.Create;
  fRadGradRenderer  := TSvgRadialGradientRenderer.Create;
  fImgRenderer      := TImageRenderer.Create;

{$IFDEF XPLAT_GENERICS}
    fFontList       := TList<TFontReader>.create;
{$ELSE}
    fFontList       := TList.create;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TSvgReader.Destroy;
var
  i: integer;
begin
  Clear;
  fMemStream.Free;
  fIdList.Free;
  fClassStyles.Free;
  for i := 0 to fFontList.Count -1 do
    TFontReader(fFontList[i]).Free;
  fFontList.Free;

  fLinGradRenderer.Free;
  fRadGradRenderer.Free;
  fImgRenderer.Free;
  FreeAndNil(fFontCache);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.Clear;
begin
  FreeAndNil(fRootElement);
  fMemStream.clear;
  fIdList.Clear;
  fClassStyles.Clear;
  fEndStream := nil;
  fLinGradRenderer.Clear;
  fRadGradRenderer.Clear;
  fImgRenderer.Image.Clear;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function TSvgReader.AddFont(const fontName: string): Boolean;
var
  fr: TFontReader;
begin
  try
    fr := TFontReader.Create(fontName);
  except;
    Result := false;
    Exit;
  end;
  Result := true;
  fFontList.Add(fr);
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TSvgReader.AddFontFromResource(const resName: string;
  resType: PChar): Boolean;
var
  fr: TFontReader;
begin
  try
    fr := TFontReader.CreateFromResource(resName, resType);
  except;
    Result := false;
    Exit;
  end;
  Result := true;
  fFontList.Add(fr);
end;
//------------------------------------------------------------------------------

procedure TSvgReader.DrawImage(img: TImage32; scaleToImage: Boolean);
var
  rawScale, sx, sy, w,h: double;
  di: TDrawInfo;
const
  endStyle: array[boolean] of TEndStyle = (esPolygon, esRound);
begin
  if not Assigned(fRootElement) then Exit;

  with fRootElement do
  begin
    di := emptyDrawInfo;
    MatrixTranslate(di.matrix, -viewboxWH.Left, -viewboxWH.Top);

    w := elRectWH.width.GetValueX(RectD(img.Bounds));
    h := elRectWH.height.GetValueY(RectD(img.Bounds));

    if viewboxWH.IsEmpty then
    begin
      fRootElement.viewboxWH := RectWH(0, 0, w, h);
      rawRect  := viewboxWH.RectD;
      if fRootElement.viewboxWH.IsEmpty then Exit;
    end
    else if (w > 0) or (h > 0) then
    begin
      rawRect  := viewboxWH.RectD;

      if (w > 0) then
        sx := w/viewboxWH.Width else
        sx := h/viewboxWH.Height;
      if (h > 0) then
        sy := h/viewboxWH.Height else
        sy := sx;
      //assume default preserveAspectRatio - ie xMidYMid.
      sx := (sx + sy) * 0.5; sy := sx;
      MatrixScale(di.matrix, sx, sy);
      viewboxWH.Width := viewboxWH.Width * sx;
      viewboxWH.Height := viewboxWH.Height * sy;
    end else
      rawRect  := viewboxWH.RectD;

    //the rootElement's matrix may have been modified by
    //the svg element's height/width and viewbox settings
    if scaleToImage and not img.IsEmpty then
    begin
      rawScale := img.width / (viewboxWH.Width);
      sy := img.height / (viewboxWH.Height);
      if sy < rawScale then rawScale := sy;
      MatrixScale(di.matrix, rawScale);
      img.SetSize(
        Round(viewboxWH.Width * rawScale),
        Round(viewboxWH.Height * rawScale));
    end else
      img.SetSize(Round(viewboxWH.Width), Round(viewboxWH.Height));
  end;

  if fBackgroundColor <> clNone32 then
    img.Clear(fBackgroundColor);

  fViewMatrix := di.matrix;
  fTempImage := TImage32.Create(img.Width, img.Height);
  try
    fRootElement.Draw(img, di);
  finally
    fTempImage.Free;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromFile(const filename: string): Boolean;
var
  Stream: TStream;
begin
  Result := FileExists(filename);
  if not Result then Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromStream(stream: TStream): Boolean;
begin
  Result := false;
  Clear;
  if not Assigned(stream) then Exit;

  fMemStream.LoadFromStream(stream);
  fEndStream := PAnsiChar(fMemStream.Memory) + fMemStream.Size;

  fRootElement := TSvgElement.Create(self, hSvg);
  with fRootElement do
  begin
    if not Assigned(fName.name) then Exit;
    result := fRootElement.LoadAttributes;
    if not Result then Exit;
    Result := not fHasContent or LoadContent;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.GetSvgStart(out svgStart: PAnsiChar): Boolean;
var
  svgEnd: PAnsiChar;
begin
  Result := false;
  svgStart := fMemStream.Memory;
  svgEnd := svgStart + fMemStream.Size -3;
  while (svgStart < svgEnd) do
  begin
    if (svgStart^ = '<') and
      (LowerCaseTable[(svgStart+1)^] = 's') and
      (LowerCaseTable[(svgStart+2)^] = 'v') and
      (LowerCaseTable[(svgStart+3)^] = 'g') then
    begin
      Result := true;
      inc(svgStart);
      Exit;
    end;
    inc(svgStart);
  end;
end;
//------------------------------------------------------------------------------

function MacStylesToFontStyles(macStyles: TMacStyles): TFontSyles;
begin
  Result := [];
  if msBold in macStyles then Include(Result, fsBold);
    if msItalic in macStyles then Include(Result, fsItalic);
end;
//------------------------------------------------------------------------------

procedure TSvgReader.GetBestFont(const svgFontInfo: TSVGFontInfo);
var
  i, bestIdx, currMask, bestMask: integer;
  fontStyles: TFontSyles;

  function GetMask(fr: TFontReader): integer;
  begin
    //font styles has priority over the font family
    fontStyles := MacStylesToFontStyles(fr.FontInfo.macStyles);
    Result := (3 - (Byte(fontStyles) and Byte(svgFontInfo.styles))) shl 16 or
      (Abs(Integer(Byte(fontStyles)) - Integer(Byte(svgFontInfo.styles))) shl 8) or
      Abs(Ord(svgFontInfo.family) - Ord(fr.FontFamily));
  end;

begin
  if fFontList.Count = 0 then Exit;
  bestIdx := 0;
  bestMask := GetMask(TFontReader(fFontList[0]));

  for i := 1 to fFontList.Count -1 do
  begin
    currMask := GetMask(TFontReader(fFontList[i]));
    if currMask >= bestMask then Continue;
    bestMask := currMask;
    bestIdx := i;
  end;

  if not Assigned(fFontCache) then
    fFontCache :=
      TGlyphCache.Create(TFontReader(fFontList[bestIdx]), defaultFontHeight)
  else
    fFontCache.FontReader := TFontReader(fFontList[bestIdx]);

  fFontCache.Underlined := False;
  fFontCache.StrikeOut := False;
  case svgFontInfo.decoration of
    fdUnderline     : fFontCache.Underlined := true;
    fdStrikeThrough : fFontCache.StrikeOut := true;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure RegisterAttribute(hashedName: Cardinal; func: TAttribFunc);
begin
  AttribFuncList.AddObject(InttoHex(hashedName, 8), @func) ;
end;
//------------------------------------------------------------------------------

procedure GradientTransform_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradientElement then
    with TGradientElement(attrib.aOwnerEl) do
      ParseTransform(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure Gradientunits_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradientElement then
    with TGradientElement(attrib.aOwnerEl) do
      gradientUnits := GetHashedName(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure Id_Attrib(attrib: PAttrib);
var
  id: AnsiString;
begin
  with attrib^ do
  begin
    id := LowercaseAnsi(aValue);
    aOwnerEl.fReader.fIdList.AddObject(string(id), aOwnerEl);
    aOwnerEl.fIdAttribIdx := High(aOwnerEl.fAttribs);
  end;
end;
//------------------------------------------------------------------------------

procedure In_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TFeBaseElement then
    TFeBaseElement(attrib.aOwnerEl).in1 := attrib.aValue;
end;
//------------------------------------------------------------------------------

procedure In2_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TFeBaseElement then
    TFeBaseElement(attrib.aOwnerEl).in2 := attrib.aValue;
end;
//------------------------------------------------------------------------------

procedure LetterSpacing_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TTextElement then
    with TTextElement(attrib.aOwnerEl) do
      AttribToFloat(attrib, fFontInfo.spacing);
end;
//------------------------------------------------------------------------------

procedure Href_Attrib(attrib: PAttrib);
var
  el: TElement;
begin
  el := attrib.aOwnerEl;
  case el.fNameHash of
    hUse:
      TUseElement(el).refEl := ExtractRefFromValue(attrib.aValue);
    hTextPath:
      TTextPathElement(el).pathEl := ExtractRefFromValue(attrib.aValue);
    else if el is TFillElement then
      TFillElement(el).refEl := ExtractRefFromValue(attrib.aValue);
  end;
end;
//------------------------------------------------------------------------------

procedure Class_Attrib(attrib: PAttrib);
begin
  TElement(attrib.aOwnerEl).ParseClassAttrib(attrib);
end;
//------------------------------------------------------------------------------

procedure ClipPath_Attrib(attrib: PAttrib);
begin
  attrib.aOwnerEl.fDrawInfo.clipPathEl := ExtractRefFromValue(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure D_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TPathElement then
    TPathElement(attrib.aOwnerEl).ParseD(attrib);
end;
//------------------------------------------------------------------------------

procedure Fill_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hfeDropShadow:
      AttribToColor32(attrib, TFeDropShadowElement(attrib.aOwnerEl).floodColor);
    hfeFlood:
      AttribToColor32(attrib, TFeFloodElement(attrib.aOwnerEl).floodColor);
    else
      AttribToColor32(attrib, attrib.aOwnerEl.fDrawInfo.fillColor);
  end;
end;
//------------------------------------------------------------------------------

procedure FillOpacity_Attrib(attrib: PAttrib);
begin
  case attrib.aOwnerEl.fNameHash of
    hfeDropShadow:
      AttribToOpacity(attrib, TFeDropShadowElement(attrib.aOwnerEl).floodColor);
    hfeFlood:
      AttribToOpacity(attrib, TFeFloodElement(attrib.aOwnerEl).floodColor);
    else
      AttribToOpacity(attrib, attrib.aOwnerEl.fDrawInfo.fillColor);
  end;
end;
//------------------------------------------------------------------------------

procedure DashArray_Attrib(attrib: PAttrib);
var
  current, currentEnd: PAnsiChar;
  val: double;
  len: integer;
begin
  current := attrib.aValue.name;
  currentEnd := current + attrib.aValue.len;
  with attrib.aOwnerEl.fDrawInfo do
  begin
    len := Length(dashArray);
    while ParseNextNum(current, currentEnd, true, val) do
    begin
      SetLength(dashArray, len +1);
      dashArray[len] := val;
      inc(len);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure DashOffset_Attrib(attrib: PAttrib);
var
  current, currentEnd: PAnsiChar;
begin
  current := attrib.aValue.name;
  currentEnd := current + attrib.aValue.len;
  with attrib.aOwnerEl.fDrawInfo do
    ParseNextNum(current, currentEnd, true, dashOffset);
end;
//------------------------------------------------------------------------------

procedure Display_Attrib(attrib: PAttrib);
begin
  if GetHashedName(attrib.aValue) = hNone then
    attrib.aOwnerEl.fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

procedure Font_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TTextElement then
    with TTextElement(attrib.aOwnerEl) do
      AttribToFontInfo(attrib, fFontInfo)
  else if attrib.aOwnerEl is TSubtextElement then
    with TSubtextElement(attrib.aOwnerEl) do
      AttribToFontInfo(attrib, fFontInfo);
end;
//------------------------------------------------------------------------------

procedure FontFamily_Attrib(attrib: PAttrib);

  procedure GetFamily(var fontInfo: TSVGFontInfo);
  var
    word: TAnsiName;
    c, endC: PAnsiChar;
  begin
    fontInfo.family := ttfSansSerif;
    c :=  attrib.aValue.name;
    endC := c + attrib.aValue.len;
    while ParseNextWord(c, endC, word) do
    begin
      case GetHashedName(word) of
        hSans_045_Serif, hArial  :
            fontInfo.family := ttfSansSerif;
        hSerif, hTimes:
            fontInfo.family := ttfSerif;
        hMonospace:
            fontInfo.family := ttfMonospace;
        else Continue;
      end;
      break;
    end;
  end;

begin
  if attrib.aOwnerEl is TTextElement then
    GetFamily(TTextElement(attrib.aOwnerEl).fFontInfo)
  else if attrib.aOwnerEl is TSubtextElement then
    GetFamily(TSubtextElement(attrib.aOwnerEl).fFontInfo);
end;
//------------------------------------------------------------------------------

procedure FontSize_Attrib(attrib: PAttrib);
var
  num: double;
begin
  with attrib.aValue do
    if not ParseNextNum(name, name + len, false, num) then Exit;
  attrib.aOwnerEl.fFontInfo.size := num;
end;
//------------------------------------------------------------------------------

procedure FontStyle_Attrib(attrib: PAttrib);

  procedure GetStyle(var fontInfo: TSVGFontInfo);
  begin
    if GetHashedName(attrib.aValue) = hItalic then
      Include(fontInfo.styles, fsItalic) else
      Exclude(fontInfo.styles, fsItalic);
  end;
begin
  if attrib.aOwnerEl is TTextElement then
    GetStyle(TTextElement(attrib.aOwnerEl).fFontInfo)
  else if attrib.aOwnerEl is TSubtextElement then
    GetStyle(TSubtextElement(attrib.aOwnerEl).fFontInfo);
end;
//------------------------------------------------------------------------------

procedure FontWeight_Attrib(attrib: PAttrib);

  procedure GetWeight(var fontInfo: TSVGFontInfo);
  var
    num: double;
    word: TAnsiName;
    c, endC: PAnsiChar;
  begin
    c := attrib.aValue.name; endC := c + attrib.aValue.len;
    if IsNumPending(c, endC, false) and ParseNextNum(c, endC, false, num) then
    begin
      if num >= 600 then
        Include(fontInfo.styles, fsBold) else
        Exclude(fontInfo.styles, fsBold);
    end
    else if ExtractWordFromValue(attrib.aValue, word) then
    begin
      case GetHashedName(word) of
        hBold, hBolder: Include(fontInfo.styles, fsBold);
        else Exclude(fontInfo.styles, fsBold);
      end;
    end;
  end;

begin
  if attrib.aOwnerEl is TTextElement then
    GetWeight(TTextElement(attrib.aOwnerEl).fFontInfo)
  else if attrib.aOwnerEl is TSubtextElement then
    GetWeight(TSubtextElement(attrib.aOwnerEl).fFontInfo);
end;
//------------------------------------------------------------------------------

procedure Fx_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  if (attrib.aOwnerEl is TRadGradElement) then
  begin
    AttribToFloat(attrib, val, mu);
    TRadGradElement(attrib.aOwnerEl).F.X.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Fy_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  if (attrib.aOwnerEl is TRadGradElement) then
  begin
    AttribToFloat(attrib, val, mu);
    TRadGradElement(attrib.aOwnerEl).F.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure TextAlign_Attrib(attrib: PAttrib);

  procedure GetAlign(var fontInfo: TSVGFontInfo);
  begin
    case GetHashedName(attrib.aValue) of
      hStart  : fontInfo.align := taLeft;
      hMiddle : fontInfo.align := taCenter;
      hEnd    : fontInfo.align := taRight;
    end;
  end;

begin
  if attrib.aOwnerEl is TTextElement then
    GetAlign(TTextElement(attrib.aOwnerEl).fFontInfo)
  else if attrib.aOwnerEl is TSubtextElement then
    GetAlign(TSubtextElement(attrib.aOwnerEl).fFontInfo);
end;
//------------------------------------------------------------------------------

procedure TextDecoration_Attrib(attrib: PAttrib);

  procedure GetDecoration(var fontInfo: TSVGFontInfo);
  begin
    case GetHashedName(attrib.aValue) of
      hUnderline        : fontInfo.decoration := fdUnderline;
      hline_045_through : fontInfo.decoration := fdStrikeThrough;
      else fontInfo.decoration := fdNone;
    end;
  end;

begin
  if attrib.aOwnerEl is TTextElement then
    GetDecoration(TTextElement(attrib.aOwnerEl).fFontInfo)
  else if attrib.aOwnerEl is TSubtextElement then
    GetDecoration(TSubtextElement(attrib.aOwnerEl).fFontInfo);
end;
//------------------------------------------------------------------------------


procedure MarkerStart_Attrib(attrib: PAttrib);
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  attrib.aOwnerEl.fDrawInfo.markerStart := ExtractRefFromValue(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure MarkerMiddle_Attrib(attrib: PAttrib);
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  attrib.aOwnerEl.fDrawInfo.markerMiddle := ExtractRefFromValue(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure MarkerEnd_Attrib(attrib: PAttrib);
begin
  if not (attrib.aOwnerEl is TShapeElement) then Exit;
  attrib.aOwnerEl.fDrawInfo.markerEnd := ExtractRefFromValue(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure Filter_Attrib(attrib: PAttrib);
begin
  if (attrib.aOwnerEl is TShapeElement) then
    attrib.aOwnerEl.fDrawInfo.filterEl := ExtractRefFromValue(attrib.aValue);
end;
//------------------------------------------------------------------------------


procedure Offset_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  if not (attrib.aOwnerEl is TGradStopElement) then Exit
  else if (mu = muPercent) then
    TGradStopElement(attrib.aOwnerEl).offset := val / 100
  else
    TGradStopElement(attrib.aOwnerEl).offset := val;
end;
//------------------------------------------------------------------------------

procedure Opacity_Attrib(attrib: PAttrib);
var
  opacity: double;
begin
  if not AttribToFloat(attrib, opacity) then Exit;
  if opacity < 0 then opacity := 0
  else if opacity > 1 then opacity := 1;
  attrib.aOwnerEl.fDrawInfo.opacity := Round(opacity * 255);
end;
//------------------------------------------------------------------------------

procedure Operator_Attrib(attrib: PAttrib);
begin
  if (attrib.aOwnerEl is TFeCompositeElement) then
    with TFeCompositeElement(attrib.aOwnerEl) do
      case GetHashedName(attrib.aValue) of
        hAtop : compositeOp := coAtop;
        hIn   : compositeOp := coIn;
        hOut  : compositeOp := coOut;
        hOver : compositeOp := coOver;
        hXor  : compositeOp := coXor;
      end;
end;
//------------------------------------------------------------------------------

procedure Orient_Attrib(attrib: PAttrib);
begin
  if (attrib.aOwnerEl is TMarkerElement) and
    (GetHashedName(attrib.aValue) = hauto_045_start_045_reverse) then
        TMarkerElement(attrib.aOwnerEl).autoStartReverse := true;
end;
//------------------------------------------------------------------------------

procedure StopColor_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradStopElement then
    AttribToColor32(attrib, TGradStopElement(attrib.aOwnerEl).color);
end;
//------------------------------------------------------------------------------

procedure StopOpacity_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TGradStopElement then
  AttribToOpacity(attrib, TGradStopElement(attrib.aOwnerEl).color);
end;
//------------------------------------------------------------------------------

procedure Points_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TPolyElement then
    TPolyElement(attrib.aOwnerEl).ParsePoints(attrib);
end;
//------------------------------------------------------------------------------

procedure Stroke_Attrib(attrib: PAttrib);
begin
  AttribToColor32(attrib, attrib.aOwnerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeOpacity_Attrib(attrib: PAttrib);
begin
  AttribToOpacity(attrib, attrib.aOwnerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeWidth_Attrib(attrib: PAttrib);
begin
    AttribToFloat(attrib, attrib.aOwnerEl.fDrawInfo.strokeWidth);
end;
//------------------------------------------------------------------------------

procedure FillRule_Attrib(attrib: PAttrib);
begin
  if LowerCaseTable[attrib.aValue.name[0]] = 'e' then
    attrib.aOwnerEl.fDrawInfo.fillRule := frEvenOdd else
    attrib.aOwnerEl.fDrawInfo.fillRule := frNonZero;
end;
//------------------------------------------------------------------------------

procedure Style_Attrib(attrib: PAttrib);
begin
  with attrib.aOwnerEl do
    case fNameHash of
      hFlowRegion, hFlowRoot: fDrawInfo.fillColor := clNone32;
      else fStyleAttribIdx := High(fAttribs);
    end;
end;
//------------------------------------------------------------------------------

procedure Transform_Attrib(attrib: PAttrib);
begin
  attrib.aOwnerEl.ParseTransform(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure Values_Attrib(attrib: PAttrib);
var
  cnt: integer;
  c, endC: PAnsiChar;
begin
  if attrib.aOwnerEl is TFeColorMatrixElement then
    with TFeColorMatrixElement(attrib.aOwnerEl) do
    begin
      SetLength(values, 20);
      c := attrib.aValue.name;
      endC := c + attrib.aValue.len;
      cnt := 0;
      while (cnt < 20) and ParseNextNum(c, endC, true, values[cnt]) do
        inc(cnt);
      if cnt < 20 then values := nil;
    end;
end;
//------------------------------------------------------------------------------


procedure Viewbox_Attrib(attrib: PAttrib);

  function LoadViewbox: TRectWH;
  var
    current, currentEnd: PAnsiChar;
  begin
    current := attrib.aValue.name;
    currentEnd := current + attrib.aValue.len;
    with Result do
    if not ParseNextNum(current, currentEnd, false, Left) or
      not ParseNextNum(current, currentEnd, true, Top) or
      not ParseNextNum(current, currentEnd, true, Width) or
      not ParseNextNum(current, currentEnd, true, Height) then
        Result := RectWH(0,0,0,0);
  end;

begin
  if attrib.aOwnerEl is TSvgElement then
    TSvgElement(attrib.aOwnerEl).viewboxWH := LoadViewbox
  else if attrib.aOwnerEl is TMarkerElement then
    TMarkerElement(attrib.aOwnerEl).markerBoxWH := LoadViewbox
  else if attrib.aOwnerEl is TPatternElement then
    TPatternElement(attrib.aOwnerEl).pattBoxWH := LoadViewbox;
end;
//------------------------------------------------------------------------------

procedure Height_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  with attrib.aOwnerEl.elRectWH do
  begin
    height.SetValue(val, mu);
    if attrib.aOwnerEl is TFilterElement then
      ConvertValueUnits(height, 2.5) else
      ConvertValueUnits(height, 1);
  end;
end;
//------------------------------------------------------------------------------

procedure Width_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  with attrib.aOwnerEl.elRectWH do
  begin
    width.SetValue(val, mu);
    if attrib.aOwnerEl is TFilterElement then
      ConvertValueUnits(width, 2.5) else
      ConvertValueUnits(width, 1);
  end;
end;
//------------------------------------------------------------------------------

procedure Cx_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hCircle:
      with TCircleElement(attrib.aOwnerEl) do centerPt.X.SetValue(val, mu);
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do centerPt.X.SetValue(val, mu);
    hRadialGradient:
      with TRadGradElement(attrib.aOwnerEl) do C.X.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Cy_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hCircle:
      with TCircleElement(attrib.aOwnerEl) do centerPt.Y.SetValue(val, mu);
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do centerPt.Y.SetValue(val, mu);
    hRadialGradient:
      with TRadGradElement(attrib.aOwnerEl) do C.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Dx_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hfeDropShadow:
      TFeDropShadowElement(attrib.aOwnerEl).dxdy.X.SetValue(val, mu);
    hfeOffset:
      TFeOffsetElement(attrib.aOwnerEl).dxdy.X.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Dy_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hfeDropShadow:
      TFeDropShadowElement(attrib.aOwnerEl).dxdy.Y.SetValue(val, mu);
    hfeOffset:
      TFeOffsetElement(attrib.aOwnerEl).dxdy.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Result_Attrib(attrib: PAttrib);
begin
  if (attrib.aOwnerEl is TFeBaseElement) then
    TFeBaseElement(attrib.aOwnerEl).res := ExtractRefFromValue(attrib.aValue);
end;
//------------------------------------------------------------------------------

procedure Rx_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do radius.X.SetValue(val, mu);
    hCircle:
      with TCircleElement(attrib.aOwnerEl) do radius.SetValue(val, mu);
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do radius.X.SetValue(val, mu);
    hRadialGradient:
      with TRadGradElement(attrib.aOwnerEl) do radius.SetValue(val, mu);
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do refPt.X.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Ry_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hRect:
      with TRectElement(attrib.aOwnerEl) do radius.Y.SetValue(val, mu);
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do radius.Y.SetValue(val, mu);
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do refPt.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure StdDev_Attrib(attrib: PAttrib);
var
  sd: double;
begin
  AttribToFloat(attrib, sd);
  if (sd < 0) and (sd > 100) then Exit;
  case attrib.aOwnerEl.fNameHash of
    hfeGaussianBlur:
      TFeGaussElement(attrib.aOwnerEl).stdDev := sd;
    hfeDropShadow:
      TFeDropShadowElement(attrib.aOwnerEl).stdDev := sd;
  end;
end;
//------------------------------------------------------------------------------

procedure X1_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hLine:
      TLineElement(attrib.aOwnerEl).path[0].X := val;
    hLinearGradient:
      with TLinGradElement(attrib.aOwnerEl) do
      begin
        startPt.X.SetValue(val, mu);
        ConvertValueUnits(startPt.X, 2.5);
      end;
    hFilter:
      begin
        attrib.aOwnerEl.elRectWH.left.SetValue(val, mu);
        ConvertValueUnits(attrib.aOwnerEl.elRectWH.left, 2.5);
      end;
    else
      attrib.aOwnerEl.elRectWH.left.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure X2_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hLine:
      TLineElement(attrib.aOwnerEl).path[1].X := val;
    hLinearGradient:
      with TLinGradElement(attrib.aOwnerEl) do
      begin
        endPt.X.SetValue(val, mu);
        ConvertValueUnits(endPt.X, 2.5);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure Y1_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hLine:
      TLineElement(attrib.aOwnerEl).path[0].Y := val;
    hLinearGradient:
      with TLinGradElement(attrib.aOwnerEl) do
      begin
        startPt.Y.SetValue(val, mu);
        ConvertValueUnits(startPt.Y, 1);
      end;
    hFilter:
      begin
        attrib.aOwnerEl.elRectWH.top.SetValue(val, mu);
        ConvertValueUnits(attrib.aOwnerEl.elRectWH.top, 2.5);
      end;
    else
      attrib.aOwnerEl.elRectWH.top.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Y2_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  case attrib.aOwnerEl.fNameHash of
    hLine:
      TLineElement(attrib.aOwnerEl).path[1].Y := val;
    hLinearGradient:
      with TLinGradElement(attrib.aOwnerEl) do
      begin
        endPt.Y.SetValue(val, mu);
        ConvertValueUnits(endPt.Y, 2.5);
      end;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure MakeAttribFuncList;
begin
  AttribFuncList := TStringList.Create;
  AttribFuncList.Duplicates := dupError;

  RegisterAttribute(hClass,                 Class_Attrib);
  RegisterAttribute(hClip_045_path,         ClipPath_Attrib);
  RegisterAttribute(hCx,                    Cx_Attrib);
  RegisterAttribute(hCy,                    Cy_Attrib);
  RegisterAttribute(hD,                     D_Attrib);
  RegisterAttribute(hDisplay,               Display_Attrib);
  RegisterAttribute(hDx,                    Dx_Attrib);
  RegisterAttribute(hDy,                    Dy_Attrib);
  RegisterAttribute(hStroke_045_DashArray,  DashArray_Attrib);
  RegisterAttribute(hStroke_045_DashOffset, DashOffset_Attrib);
  RegisterAttribute(hFill,                  Fill_Attrib);
  RegisterAttribute(hFill_045_Opacity,      FillOpacity_Attrib);
  RegisterAttribute(hFill_045_Rule,         FillRule_Attrib);
  RegisterAttribute(hFilter,                Filter_Attrib);
  RegisterAttribute(hflood_045_color,       Fill_Attrib);
  RegisterAttribute(hflood_045_opacity,     FillOpacity_Attrib);
  RegisterAttribute(hFont,                  Font_Attrib);
  RegisterAttribute(hFont_045_Family,       FontFamily_Attrib);
  RegisterAttribute(hFont_045_Size,         FontSize_Attrib);
  RegisterAttribute(hFont_045_Style,        FontStyle_Attrib);
  RegisterAttribute(hFont_045_Weight,       FontWeight_Attrib);
  RegisterAttribute(hFx,                    Fx_Attrib);
  RegisterAttribute(hFy,                    Fy_Attrib);
  RegisterAttribute(hGradientTransform,     GradientTransform_Attrib);
  RegisterAttribute(hGradientUnits,         Gradientunits_Attrib);
  RegisterAttribute(hHeight,                Height_Attrib);
  RegisterAttribute(hHref,                  Href_Attrib);
  RegisterAttribute(hId,                    Id_Attrib);
  RegisterAttribute(hIn,                    In_Attrib);
  RegisterAttribute(hIn2,                   In2_Attrib);
  RegisterAttribute(hletter_045_spacing,    LetterSpacing_Attrib);
  RegisterAttribute(hMarker_045_End,        MarkerEnd_Attrib);
  RegisterAttribute(hMarkerHeight,          Height_Attrib);
  RegisterAttribute(hMarker_045_Mid,        MarkerMiddle_Attrib);
  RegisterAttribute(hMarker_045_Start,      MarkerStart_Attrib);
  RegisterAttribute(hMarkerWidth,           Width_Attrib);
  RegisterAttribute(hOffset,                Offset_Attrib);
  RegisterAttribute(hOpacity,               Opacity_Attrib);
  RegisterAttribute(hOperator,              Operator_Attrib);
  RegisterAttribute(hOrient,                Orient_Attrib);
  RegisterAttribute(hPoints,                Points_Attrib);
  RegisterAttribute(hR,                     Rx_Attrib);
  RegisterAttribute(hRefX,                  Rx_Attrib);
  RegisterAttribute(hRefY,                  Ry_Attrib);
  RegisterAttribute(hResult,                Result_Attrib);
  RegisterAttribute(hRx,                    Rx_Attrib);
  RegisterAttribute(hRy,                    Ry_Attrib);
  RegisterAttribute(hstdDeviation,          StdDev_Attrib);
  RegisterAttribute(hStop_045_Color,        StopColor_Attrib);
  RegisterAttribute(hStop_045_Opacity,      StopOpacity_Attrib);
  RegisterAttribute(hStroke,                Stroke_Attrib);
  RegisterAttribute(hStroke_045_Opacity,    StrokeOpacity_Attrib);
  RegisterAttribute(hStroke_045_Width,      StrokeWidth_Attrib);
  RegisterAttribute(hStyle,                 Style_Attrib);
  RegisterAttribute(hText_045_Anchor,       TextAlign_Attrib);
  RegisterAttribute(hText_045_Decoration,   TextDecoration_Attrib);
  RegisterAttribute(hTransform,             Transform_Attrib);
  RegisterAttribute(hValues,                Values_Attrib);
  RegisterAttribute(hViewbox,               Viewbox_Attrib);
  RegisterAttribute(hWidth,                 Width_Attrib);
  RegisterAttribute(hX,                     X1_Attrib);
  RegisterAttribute(hX1,                    X1_Attrib);
  RegisterAttribute(hX2,                    X2_Attrib);
  RegisterAttribute(hXlink_058_Href,        Href_Attrib);
  RegisterAttribute(hY,                     Y1_Attrib);
  RegisterAttribute(hY1,                    Y1_Attrib);
  RegisterAttribute(hY2,                    Y2_Attrib);

  AttribFuncList.Sorted := true;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  MakeAttribFuncList;

finalization
  AttribFuncList.Free;
end.
