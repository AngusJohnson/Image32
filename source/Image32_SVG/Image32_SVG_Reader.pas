unit Image32_SVG_Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.24                                                            *
* Date      :  11 June 2021                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2021                                         *
*                                                                              *
* Purpose   :  Read SVG ver 2 files                                            *
*                                                                              *
*              To fully support all the features of SVG version 2 is a huge    *
*              task that would require a team of developers. Nevertheless this *
*              unit compares very favourably with other Delphi SVG readers.    *
*                                                                              *
*              Known unsupported features: animation, masks,                   *
*              a number of less commonly used filter effects,                  *
*              <image> and <foreignObject> and nested <svg> elements,          *
*              affine transforms of pattern and gradient fills.                *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
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
    currentColor  : TColor32;
    fillColor     : TColor32;
    fillRule      : TFillRule;
    fillEl        : TAnsiName;

    strokeColor   : TColor32;
    strokeWidth   : TValue;
    strokeCap     : TEndStyle;
    strokeJoin    : TJoinStyle;
    strokeMitLim  : double;
    strokeEl      : TAnsiName;
    dashArray     : TArrayOfDouble;
    dashOffset    : double;
    fontInfo      : TSVGFontInfo;
    markerStart   : TAnsiName;
    markerMiddle  : TAnsiName;
    markerEnd     : TAnsiName;
    filterEl      : TAnsiName;
    clipPathEl    : TAnsiName;
    opacity       : integer;
    matrix        : TMatrixD;
    visible       : Boolean;
    InUse         : Boolean; //avoids <USE> recursion
    bounds        : TRectD;
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
    fIdAttribIdx    : integer;        //delayed loading
    fStyleAttribIdx : integer;        //delayed loading
    fName           : TAnsiName;
    fNameHash       : Cardinal;
    fAttribs        : TArrayOfAttrib;
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
    fPcBelow  : double;         //assume percentage values below this value
    fDrawInfo : TDrawInfo;      //currently both static and dynamic vars
    elRectWH  : TValueRecWH;          //multifunction variable
    function  IsFirstChild: Boolean;
    function  LoadAttributes: Boolean; virtual;
    function  LoadContent: Boolean; virtual;
    procedure Draw(image: TImage32; drawInfo: TDrawInfo); virtual;
    procedure DrawChildren(image: TImage32; drawInfo: TDrawInfo); virtual;
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

  TStyleElement = class(TDefsElement)
  protected
    function LoadContent: Boolean; override;
  end;

  //-------------------------------------

  TShapeElement = class(TElement)
  protected
    hasPaths    : Boolean;
    drawPathsO  : TPathsD; //open only
    drawPathsC  : TPathsD; //closed only
    drawPathsF  : TPathsD; //both open and closed (for filling)
    function  GetBounds: TRectD; virtual;
    function  HasMarkers: Boolean;
    function  ParseVal(out val: double): Boolean;
    procedure GetPaths(const drawInfo: TDrawInfo); virtual;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; virtual;
    procedure DrawFilled(img: TImage32; drawInfo: TDrawInfo);
    procedure DrawStroke(img: TImage32; drawInfo: TDrawInfo; isClosed: Boolean);
    procedure DrawMarkers(img: TImage32; drawInfo: TDrawInfo);
    procedure Draw(image: TImage32; drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TGroupElement = class(TShapeElement)
  protected
    procedure Draw(image: TImage32; drawInfo: TDrawInfo); override;
  end;

  TSwitchElement = class(TShapeElement)
  protected
    procedure Draw(image: TImage32; drawInfo: TDrawInfo); override;
  end;

  TUseElement = class(TShapeElement)
  protected
    refEl: TAnsiName;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    procedure Draw(img: TImage32; drawInfo: TDrawInfo); override;
  end;

  TSymbolElement = class(TShapeElement)
  protected
    viewboxWH: TRectWH;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  //-------------------------------------

  TPathElement = class(TShapeElement)
  private
    fCurrSeg      : PDpathSeg;
    fCurrSegCap   : integer;
    fCurrSegCnt   : integer;
    fCurrSegType  : TDsegType;
    fCurrDpath    : PDpath;
    fDpaths       : TDpaths;
    fLastPt       : TPointD;
    function  GetSegType(out isRelative: Boolean): Boolean;
    function  GetSingleDigit(out digit: integer): Boolean;
    procedure StartNewDpath;
    procedure StartNewSeg(segType: TDsegType);
    procedure AddSegValue(val: double);
    procedure AddSegPoint(const pt: TPointD);
    function  Parse2Num(var pt: TPointD; isRelative: Boolean): Boolean;
    procedure Flatten(index: integer; scalePending: double;
      out path: TPathD; out isClosed: Boolean);
  protected
    function  GetBounds: TRectD; override;
    procedure ParseD(attrib: PAttrib);
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; override;
  end;

  TPolyElement = class(TShapeElement) //polyline or polygon
  protected
    path      : TPathD;
    function  GetBounds: TRectD; override;
    procedure ParsePoints(attrib: PAttrib);
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; override;
  end;

  TLineElement = class(TShapeElement)
  protected
    path      : TPathD;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TCircleElement = class(TShapeElement)
  protected
    centerPt  : TValuePt;
    radius    : TValue;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TEllipseElement = class(TShapeElement)
  protected
    centerPt  : TValuePt;
    radius    : TValuePt;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TRectElement = class(TShapeElement)
  protected
    radius    : TValuePt;
    function  GetBounds: TRectD; override;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
    function  GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  //TTextElement: although this is a TShapeElement descendant, it's really
  //only a container for 'tspan' and 'subtext' elements. (See Draw method.)
  TTextElement = class(TShapeElement)
  protected
    delta     : TPointD;
    tmpPt     : TPointD;
    procedure ResetTmpPt;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
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
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  //-------------------------------------

  TTextPathElement = class(TSubtextElement)
  protected
    pathEl: TAnsiName;
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  end;

  TMarkerElement = class(TShapeElement)
  private
    fPoints     : TPathD;
  protected
    refPt       : TValuePt;
    angle       : double;
    angle2      : double;
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
    refEl : TAnsiName;
    units : Cardinal;
  end;

  TPatternElement = class(TFillElement)
  protected
    pattBoxWH : TRectWH;
    function PrepareRenderer(renderer: TImageRenderer;
      drawInfo: TDrawInfo): Boolean; virtual;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  //nb: gradients with objectBoundingBox should not be applied to
  //elements without width and height.
  TGradientElement = class(TFillElement)
  protected
    stops         : TSvgColorStops;
    spreadMethod  : TGradientFillStyle;
    function LoadContent: Boolean; override;
    procedure AddStop(color: TColor32; offset: double);
    procedure AssignTo(other: TElement);  virtual;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawInfo: TDrawInfo): Boolean; virtual;
  end;

  TRadGradElement = class(TGradientElement)
  protected
    radius: TValuePt;
    F, C: TValuePt;
    procedure AssignTo(other: TElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawInfo: TDrawInfo): Boolean; override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  TLinGradElement = class(TGradientElement)
  protected
    startPt, endPt: TValuePt;
    procedure AssignTo(other: TElement); override;
    function PrepareRenderer(renderer: TCustomGradientRenderer;
      drawInfo: TDrawInfo): Boolean; override;
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
    function GetAdjustedBounds(const bounds: TRectD): TRectD;
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
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
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
    procedure GetPaths(const drawInfo: TDrawInfo); override;
  public
    constructor Create(parent: TElement; hashName: Cardinal); override;
  end;

  //-------------------------------------

  TAttribFunc = procedure (attrib: PAttrib);

  TSvgReader = class
  private
    fMemStream        : TMemoryStream;
    fEndStream        : PAnsiChar;
    fBackgroundColor  : TColor32;
    fTempImage        : TImage32;
    fBlurQuality      : integer;
    fIdList           : TStringList;
    fClassStyles      : TClassStylesList;
    fLinGradRenderer  : TLinearGradientRenderer;
    fRadGradRenderer  : TSvgRadialGradientRenderer;
    fImgRenderer      : TImageRenderer;
    fRootElement      : TSvgElement;
{$IFDEF XPLAT_GENERICS}
    fFontList     : TList<TFontReader>;
{$ELSE}
    fFontList     : TList;
{$ENDIF}
    fFontCache    : TGlyphCache;
    procedure SetBlurQuality(value: integer);
  protected
    rawRect       : TRectD;
    currentColor  : TColor32;
    function  GetSvgStart(out svgStart: PAnsiChar): Boolean;
    procedure GetBestFont(const svgFontInfo: TSVGFontInfo);
    property  RadGradRenderer: TSvgRadialGradientRenderer read fRadGradRenderer;
    property  LinGradRenderer: TLinearGradientRenderer read fLinGradRenderer;
    property  ImageRenderer  : TImageRenderer read fImgRenderer;

    property  TempImage      : TImage32 read fTempImage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
{$IFDEF MSWINDOWS}
    function  AddFont(const fontName: string): Boolean;
{$ENDIF}
    function  AddFontFromResource(const resName: string; resType: PChar): Boolean;
    procedure DrawImage(img: TImage32; scaleToImage: Boolean);
    function  LoadFromStream(stream: TStream): Boolean;
    function  LoadFromFile(const filename: string): Boolean;
    property  BackgroundColor: TColor32
      read fBackgroundColor write fBackgroundColor;
    //BlurQuality: high quality blurring can really slow down rendering.
    //Range 0..2 (0: OK & reasonably fast; 1: very good; 2: excellent but slow)
    property BlurQuality: integer read fBlurQuality write SetBlurQuality;
  end;

implementation

uses
  Image32_Extra, StrUtils;

const
  buffSize    = 32;
  clAlphaSet  = $00010101;
  SourceImage   : TAnsiName = (name: 'SourceGraphic'; len: 13);
  SourceAlpha   : TAnsiName = (name: 'SourceAlpha'; len: 11);
  tmpFilterImg  : TAnsiName = (name: 'tmp'; len: 3);

  {$I Image32_SVG_Hash_Consts.inc}

  //https://www.w3.org/TR/css-fonts-3/#font-family-prop
  emptyDrawInfo: TDrawInfo =
    (currentColor: clInvalid; fillColor: clInvalid; fillRule: frNonZero;
    fillEl: (name: nil; len: 0); strokeColor: clInvalid;
    strokeWidth: (rawVal: InvalidD; mu: muUndefined; pcBelow: 0);
    strokeCap: esButt; strokeJoin: jsMiter; strokeMitLim: 0.0;
    dashArray:nil; dashOffset:0;
    fontInfo: (family: ttfUnknown; size: 0; spacing: 0.0; textLength: 0;
    styles: sfsUndefined; align: staUndefined; decoration: fdUndefined;
    baseShift: (rawVal: InvalidD; mu: muUndefined; pcBelow: 0));
    markerStart: (name: nil; len: 0); markerMiddle: (name: nil; len: 0);
    markerEnd: (name: nil; len: 0); filterEl: (name: nil; len: 0);
    clipPathEl: (name: nil; len: 0); opacity: MaxInt;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); visible: true;
    InUse: false; bounds: (Left:0; Top:0; Right:0; Bottom:0));

var
  AttribFuncList : TStringList;
  //defaultFontHeight: this size will be used to retrieve all glyph contours
  //(and later scaled as necessary). This relatively large default ensures
  //that contours will have adequate detail.
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
    hG              : Result := TGroupElement;
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
    hSvg            : Result := TSvgElement;
    hSwitch         : Result := TSwitchElement;
    hSymbol         : Result := TSymbolElement;
    hText           : Result := TTextElement;
    hTextPath       : Result := TTextPathElement;
    hTSpan          : Result := TTSpanElement;
    hUse            : Result := TUseElement;
    else              Result := TElement;
  end;
end;
//------------------------------------------------------------------------------

procedure BestGuessUndefined(var val: TValue; const compare: TValue);
begin
  //determining implicit percentages seems to be more art than science :(
  if (val.mu <> muUndefined) or (val.rawVal > 1) then Exit;
  if (compare.mu <> muUndefined) or (compare.rawVal > 1)  then Exit;
  val.mu := muPercent;
  val.rawVal := val.rawVal * 100;
end;
//------------------------------------------------------------------------------

procedure UpdateDrawInfo(var drawInfo: TDrawInfo; thisElement: TElement);
begin
  with thisElement.fDrawInfo do
  begin
    if currentColor <> clInvalid then
      thisElement.fReader.currentColor := currentColor;
    drawInfo.fillRule := fillRule;
    if (fillColor = clCurrent) then
      drawInfo.fillColor := thisElement.fReader.currentColor
    else if (fillColor <> clInvalid) then
      drawInfo.fillColor := fillColor;
    if Assigned(fillEl.name) then
      drawInfo.fillEl := fillEl;
    if Assigned(clipPathEl.name) then
      drawInfo.clipPathEl := clipPathEl;
    if (strokeColor <> clInvalid) then
    begin
      if (strokeColor = clCurrent) then
        drawInfo.strokeColor := thisElement.fReader.currentColor else
        drawInfo.strokeColor := strokeColor;
      if not drawInfo.strokeWidth.IsValid then
        drawInfo.strokeWidth.SetValue(1);
    end;
    if strokeWidth.IsValid then
      drawInfo.strokeWidth := strokeWidth;
    if strokeMitLim > 0 then
      drawInfo.strokeMitLim := strokeMitLim;
    if Assigned(dashArray) then
      drawInfo.dashArray := Copy(dashArray, 0, Length(dashArray));
    if dashOffset <> 0 then
      drawInfo.dashOffset := dashOffset;
    if Assigned(strokeEl.name) then
      drawInfo.strokeEl := strokeEl;
    if opacity < MaxInt then
      drawInfo.opacity := opacity;
    if Assigned(filterEl.name) then
      drawInfo.filterEl := filterEl;

    if fontInfo.family <> ttfUnknown then
      drawInfo.fontInfo.family := fontInfo.family;
    if fontInfo.size > 0 then
      drawInfo.fontInfo.size := fontInfo.size;
    if fontInfo.spacing <> 0 then
      drawInfo.fontInfo.spacing := fontInfo.spacing;
    if fontInfo.textLength > 0 then
      drawInfo.fontInfo.textLength := fontInfo.textLength;

    if (fontInfo.styles <> sfsUndefined) then
      drawInfo.fontInfo.styles := fontInfo.styles;
    if fontInfo.align <> staUndefined then
      drawInfo.fontInfo.align := fontInfo.align;

    if (thisElement is TTextElement) or
      (fontInfo.decoration <> fdUndefined) then
      drawInfo.fontInfo.decoration := fontInfo.decoration;
    if fontInfo.baseShift.IsValid then
      drawInfo.fontInfo.baseShift := fontInfo.baseShift;
    if not IsIdentityMatrix(matrix) then
      drawInfo.matrix := MatrixMultiply(drawInfo.matrix, matrix);
  end;
end;
//------------------------------------------------------------------------------

function IsFilled(const drawInfo: TDrawInfo): Boolean;
begin
  Result :=
    (drawInfo.fillColor = clInvalid) or (drawInfo.fillColor = clCurrent) or
    Assigned(drawInfo.fillEl.name) or (TARGB(drawInfo.fillColor).A > 0);
end;
//------------------------------------------------------------------------------

function IsStroked(const drawInfo: TDrawInfo): Boolean;
begin
  if (drawInfo.strokeColor = clInvalid) or
    (drawInfo.strokeColor = clCurrent) or
    Assigned(drawInfo.strokeEl.name) then
      Result := (drawInfo.strokeWidth.rawVal > 0)
  else
      Result := TARGB(drawInfo.strokeColor).A > 0;
end;
//------------------------------------------------------------------------------

function AttribToColor32(attrib: PAttrib; var color: TColor32): Boolean;
var
  tmp: TAnsiName;
begin
  if ColorIsURL(attrib.aValue.name) then
  begin
    Result := true;
    if GetHashedName(attrib.aName) = hFill then
      attrib.aOwnerEl.fDrawInfo.fillEl := ExtractRefFromValue(attrib.aValue) else
      attrib.aOwnerEl.fDrawInfo.strokeEl := ExtractRefFromValue(attrib.aValue);

    //and check for a fallback color ...
    tmp := attrib.aValue;
    inc(tmp.name, 5);
    dec(tmp.len, 5);
    while (tmp.len > 1) and (tmp.name^ > #32) do
    begin
      inc(tmp.name);
      dec(tmp.len);
    end;
    if (tmp.len > 5) then
    begin
      inc(tmp.name); dec(tmp.len); //skip blank
      ValueToColor32(tmp, color);
    end;
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
  if color = clNone32 then
  begin
    color := clAlphaSet;
    Exit;
  end;

  if color = clInvalid then color := clNone32;
  if not AttribToFloat(attrib, opacity) then Exit;
  with TARGB(color) do
    if (opacity <= 0) then
    begin
      if Color = clNone32 then Color := clAlphaSet
      else A := 0;
    end
    else if (opacity >= 1) then A := 255
    else A := Round(255 * opacity);
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
        hBold             :
          if fontInfo.styles = sfsItalic then
            fontInfo.styles := sfsBoldItalic else
            fontInfo.styles := sfsBold;
        hItalic           :
          if fontInfo.styles = sfsBold then
            fontInfo.styles := sfsBoldItalic else
            fontInfo.styles := sfsItalic;
        hNormal           : fontInfo.styles := sfsNone;
        hStart            : fontInfo.align := staLeft;
        hMiddle           : fontInfo.align := staCenter;
        hEnd              : fontInfo.align := staRight;
        hline_045_through : fontInfo.decoration := fdStrikeThrough;
        hUnderline        : fontInfo.decoration := fdUnderline;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function MatrixApply(const paths: TPathsD; const matrix: TMatrixD): TPathsD; overload;
var
  i,j,len,len2: integer;
  pp,rr: PPointD;
begin
  if not Assigned(paths) then
    Result := nil
  else if IsIdentityMatrix(matrix) then
    Result := CopyPaths(paths)
  else
  begin
    len := Length(paths);
    SetLength(Result, len);
    for i := 0 to len -1 do
    begin
      len2 := Length(paths[i]);
      SetLength(Result[i], len2);
      if len2 = 0 then Continue;
      pp := @paths[i][0];
      rr := @Result[i][0];
      for j := 0 to High(paths[i]) do
      begin
        rr.X := pp.X * matrix[0, 0] + pp.Y * matrix[1, 0] + matrix[2, 0];
        rr.Y := pp.X * matrix[0, 1] + pp.Y * matrix[1, 1] + matrix[2, 1];
        inc(pp); inc(rr);
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
// TGroupElement
//------------------------------------------------------------------------------

procedure TGroupElement.Draw(image: TImage32; drawInfo: TDrawInfo);
var
  clipEl    : TElement;
  //filterEl  : TElement;
  tmpImg    : TImage32;
  clipPaths : TPathsD;
  clipRec   : TRect;
  //clipRec2  : TRectD;
begin
  if fChilds.Count = 0 then Exit;

  UpdateDrawInfo(drawInfo, self);

  //filterEl := FindRefElement(fDrawInfo.filterEl);
  clipEl := FindRefElement(drawInfo.clipPathEl);
  if Assigned(clipEl) then
  begin
    drawInfo.clipPathEl.name := nil; //avoids propagation
    with TClipPathElement(clipEl) do
    begin
      GetPaths(drawInfo);
      clipPaths := CopyPaths(drawPathsF);

      MatrixApply(drawInfo.matrix, clipPaths);
      clipRec := Image32_Vector.GetBounds(clipPaths);
    end;
    if IsEmptyRect(clipRec) then Exit;

    tmpImg := fReader.TempImage;
    tmpImg.Clear(clipRec);
    DrawChildren(tmpImg, drawInfo);

    with TClipPathElement(clipEl) do
      EraseOutsidePaths(tmpImg, clipPaths, fDrawInfo.fillRule, clipRec);
    image.CopyBlend(tmpImg, clipRec, clipRec, BlendToAlpha);
//  end
//  else if Assigned(filterEl) then
//  begin
//    clipRec2 := GetChildBounds;
//    with TFilterElement(filterEl) do
//      clipRec2 := GetAdjustedBounds(clipRec2);
//    MatrixApply(drawInfo.matrix, clipRec2);
//    clipRec := Rect(clipRec2);
//    clipRec := IntersectRect(clipRec, image.Bounds);
//
//    tmpImg := fReader.TempImage;
//    tmpImg.Clear(clipRec);
//    DrawChildren(tmpImg, drawInfo);
//
//    with TFilterElement(filterEl) do
//      Apply(tmpImg, clipRec, drawInfo.matrix);
//    image.CopyBlend(tmpImg, clipRec, clipRec, BlendToAlpha);
  end else
    DrawChildren(image, drawInfo);
end;

//------------------------------------------------------------------------------
// TSwitchElement
//------------------------------------------------------------------------------

procedure TSwitchElement.Draw(image: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
        if fDrawInfo.visible then
        begin
          Draw(image, drawInfo);
          break; //break after the first successful drawing
        end;
end;

//------------------------------------------------------------------------------
// TUseElement
//------------------------------------------------------------------------------

procedure TUseElement.GetPaths(const drawInfo: TDrawInfo);
var
  el: TElement;
  dx, dy: double;
begin
  if Assigned(drawPathsF) or not Assigned(refEl.name) then Exit;

  el := FindRefElement(refEl);
  if not Assigned(el) or not (el is TShapeElement) then Exit;
  with TShapeElement(el) do
  begin
    GetPaths(drawInfo);
    self.drawPathsC := CopyPaths(drawPathsC);
    self.drawPathsO := CopyPaths(drawPathsO);
  end;

  if elRectWH.left.IsValid then
    dx := elRectWH.left.rawVal else
    dx := 0;
  if elRectWH.top.IsValid  then
    dy := elRectWH.top.rawVal else
    dy := 0;

  if (dx <> 0) or (dy <> 0) then
  begin
    drawPathsC := OffsetPath(drawPathsC, dx, dy);
    drawPathsO := OffsetPath(drawPathsO, dx, dy);
  end;

  drawPathsF := CopyPaths(drawPathsC);
  AppendPath(drawPathsF, drawPathsO);
end;
//------------------------------------------------------------------------------

procedure TUseElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  el: TElement;
  s, dx, dy: double;
  scale, scale2: TSizeD;
  mat: TMatrixD;
begin
  el := FindRefElement(refEl);
  if not Assigned(el) or drawInfo.InUse then Exit;

  UpdateDrawInfo(drawInfo, el);
  UpdateDrawInfo(drawInfo, self); //nb: <use> attribs override el's.
  scale := ExtractScaleFromMatrix(drawInfo.matrix);

  if elRectWH.left.IsValid then dx := elRectWH.left.rawVal else dx := 0;
  if elRectWH.top.IsValid  then dy := elRectWH.top.rawVal  else dy := 0;

  mat := IdentityMatrix;
  MatrixTranslate(mat, dx, dy);
  drawInfo.matrix := MatrixMultiply(drawInfo.matrix, mat);

  if el is TSymbolElement then
  begin
    with TSymbolElement(el) do
    begin
      if not viewboxWH.IsEmpty then
      begin
        //scale the symbol according to its width and height attributes
        if elRectWH.width.IsValid and elRectWH.height.IsValid then
        begin
          scale2.sx := elRectWH.width.rawVal / viewboxWH.Width;
          scale2.sy := elRectWH.height.rawVal / viewboxWH.Height;
          if scale2.sy < scale2.sx then s := scale2.sy else s := scale2.sx;
          //the following 3 lines will scale without translating
          mat := IdentityMatrix;
          MatrixScale(mat, s, s);
          drawInfo.matrix := MatrixMultiply(drawInfo.matrix, mat);
        end;

        if self.elRectWH.width.IsValid and
          self.elRectWH.height.IsValid then
        begin
          //scale <symbol> proportionally to fill the <use> element
          scale2.sx := self.elRectWH.width.rawVal / viewboxWH.Width;
          scale2.sy := self.elRectWH.height.rawVal / viewboxWH.Height;
          if scale2.sy < scale2.sx then s := scale2.sy else s := scale2.sx;

          //again, scale without translating
          mat := IdentityMatrix;
          MatrixScale(mat, s, s);
          drawInfo.matrix := MatrixMultiply(drawInfo.matrix, mat);

          //now center after scaling
          if scale2.sx > scale2.sy then
          begin
            if scale2.sx > 1 then
            begin
              s := (self.elRectWH.width.rawVal - viewboxWH.Width) * 0.5;
              MatrixTranslate(drawInfo.matrix, s * scale.sx, 0);
            end;
          end else if scale2.sy > 1 then
          begin
            s := (self.elRectWH.height.rawVal - viewboxWH.Height) * 0.5;
            MatrixTranslate(drawInfo.matrix, 0, s * scale.sy);
          end;

        end;
      end;
      DrawChildren(img, drawInfo);
    end;
  end
  else if el is TShapeElement then
  begin
    drawInfo.InUse := true;              //flag <use> precedence
    el.Draw(img, drawInfo);
  end;
end;

//------------------------------------------------------------------------------
// TSymbolElement
//------------------------------------------------------------------------------

constructor TSymbolElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fPcBelow := 1.0;
  fDrawInfo.visible := false;
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
  len: integer;
begin
  //if a stop is less than previous stops, it is set equal to the largest stop.
  //If two stops are equal the last stop controls the color from that point.
  len := Length(stops);
  if (len > 0) and (stops[len-1].offset > offset) then
    offset := stops[len-1].offset;
  setLength(stops, len+1);
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
    if units = 0 then
      units := Self.units;

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
  renderer: TCustomGradientRenderer; drawInfo: TDrawInfo): Boolean;
var
  el: TElement;
begin
  if Assigned(refEl.name) then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TGradientElement) then
      TGradientElement(el).AssignTo(self);
  end;
  Result := Length(stops) > 0;
end;

//------------------------------------------------------------------------------
// TRadGradElement
//------------------------------------------------------------------------------

constructor TRadGradElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  radius.Init(1.0);
  F.Init(1.0);
  C.Init(1.0);
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

function TRadGradElement.PrepareRenderer(renderer: TCustomGradientRenderer;
  drawInfo: TDrawInfo): Boolean;
var
  i, hiStops: integer;
  cp, fp, r: TPointD;
  scale, scale2: TSizeD;
  rec2, rec3: TRectD;
begin
  inherited;
  hiStops := High(stops);
  Result := hiStops >= 0;
  if not Result then Exit;

  //w3c-coords-units-01-b.svg

  if units = hUserSpaceOnUse then
    rec2 := fReader.rawRect else
    rec2 := drawInfo.bounds;

  if radius.IsValid then
  begin
    r := radius.GetPoint(rec2, drawInfo.fontInfo.size);
  end else
  begin
    r.X := rec2.Width * 0.5;
    r.Y := rec2.Height * 0.5;
  end;
  scale := ExtractScaleFromMatrix(drawInfo.matrix);
  scale2 := ExtractScaleFromMatrix(fDrawInfo.matrix);
  r := ScalePoint(r, scale.sx * scale2.sx, scale.sy * scale2.sy);

  if C.IsValid then
  begin
    cp := C.GetPoint(rec2, drawInfo.fontInfo.size);
    cp := OffsetPoint(cp, rec2.Left, rec2.Top);
  end else
    cp := rec2.MidPoint;
  MatrixApply(fDrawInfo.matrix, cp);
  MatrixApply(drawInfo.matrix, cp);

  rec3 := RectD(cp.X-r.X, cp.Y-r.Y, cp.X+r.X, cp.Y+r.Y);

  if F.IsValid then
  begin
    fp := F.GetPoint(rec2, drawInfo.fontInfo.size);
    fp := OffsetPoint(fp, rec2.Left, rec2.Top);
    MatrixApply(fDrawInfo.matrix, fp);
    MatrixApply(drawInfo.matrix, fp);
  end else
    fp := MidPoint(rec3);

  with renderer as TSvgRadialGradientRenderer do
  begin
    SetParameters(Rect(rec3), Point(fp),
      stops[0].color, stops[hiStops].color, spreadMethod);
    for i := 0 to hiStops do
      with stops[i] do
        renderer.InsertColorStop(offset, color);
  end;
end;

//------------------------------------------------------------------------------
// TLinGradElement
//------------------------------------------------------------------------------

constructor TLinGradElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  startPt.Init(2.5);
  endPt.Init(2.5);
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
  renderer: TCustomGradientRenderer; drawInfo: TDrawInfo): Boolean;
var
  pt1, pt2: TPointD;
  i, hiStops: integer;
  rec2: TRectD;
begin
  inherited;
  hiStops := High(stops);
  Result := (hiStops >= 0);
  if not Result then Exit;

  //w3c-coords-units-01-b.svg

  //if gradientUnits=objectBoundingBox (default) then all values must be
  //percentages. Also... when the object's bounding box is not square, the
  //gradient may render non-perpendicular relative to the gradient vector
  //unless the gradient vector is vertical or horizontal.
  //https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/gradientUnits

  if units = hUserSpaceOnUse then
    rec2 := fReader.rawRect else
    rec2 := drawInfo.bounds;

  with TLinearGradientRenderer(renderer) do
  begin
    pt1.X := startPt.X.GetValueX(rec2.Width, drawInfo.fontInfo.size);
    pt1.Y := startPt.Y.GetValueY(rec2.Height, drawInfo.fontInfo.size);

    if (startPt.X.mu <> muPixel) or
      (units <> hUserSpaceOnUse) then
        pt1.X := pt1.X + rec2.Left;

    if (startPt.Y.mu <> muPixel) or
      (units <> hUserSpaceOnUse) then
        pt1.Y := pt1.Y + rec2.Top;

    MatrixApply(fDrawInfo.matrix, pt1);
    MatrixApply(drawInfo.matrix, pt1);


    if not endPt.X.IsValid then
      pt2.X := rec2.Width else
      pt2.X := endPt.X.GetValueX(rec2.Width, drawInfo.fontInfo.size);
    pt2.Y := endPt.Y.GetValueY(rec2.Height, drawInfo.fontInfo.size);
    pt2 := OffsetPoint(pt2, rec2.Left, rec2.Top);

    MatrixApply(fDrawInfo.matrix, pt2);
    MatrixApply(drawInfo.matrix, pt2);

    if (units <> hUserSpaceOnUse) and
      ((pt2.X <> pt1.X) or (pt2.Y <> pt1.Y)) then
    begin
      //skew the gradient
    end;

    SetParameters(pt1, pt2, stops[0].color,
      stops[hiStops].color, spreadMethod);
    for i := 0 to hiStops do
      with stops[i] do
        renderer.InsertColorStop(offset, color);
  end;
end;

//------------------------------------------------------------------------------
// TGradStopElement
//------------------------------------------------------------------------------

constructor TGradStopElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  color := clBlack32;
end;

//------------------------------------------------------------------------------
// TFilterElement
//------------------------------------------------------------------------------

constructor TFilterElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.visible := false;
  fPcBelow := 2.5;
  elRectWH.Init(fPcBelow);
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

function TFilterElement.GetAdjustedBounds(const bounds: TRectD): TRectD;
var
  recWH: TRectWH;
begin
  fObjectBounds := Rect(bounds);
  if elRectWH.IsValid then
  begin
    recWH := elRectWH.GetRectWH(bounds, fDrawInfo.fontInfo.size);
    Result.Left := bounds.Left + recWH.Left;
    Result.Top := bounds.Top + recWH.Top;
    Result.Right := Result.Left + recWH.Width;
    Result.Bottom := Result.Top + recWH.Height;
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
    begin
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
  inherited;
  stdDev := InvalidD;
  floodColor := clInvalid;
  dxdy.X.SetValue(0);
  dxdy.Y.SetValue(0);
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

  off := dxdy.GetPoint(RectD(pfe.fObjectBounds), fDrawInfo.fontInfo.size);
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

constructor TFeFloodElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  floodColor := clInvalid;
end;
//------------------------------------------------------------------------------

procedure TFeFloodElement.Apply;
var
  rec: TRect;
begin
  if not GetSrcAndDst then Exit;
  if elRectWH.IsValid then
    rec := Rect(elRectWH.GetRectD(RectD(srcRec), fDrawInfo.fontInfo.size))
  else
    rec := srcRec;
  dstImg.FillRect(rec, floodColor);
end;

//------------------------------------------------------------------------------
// TFeGaussElement
//------------------------------------------------------------------------------

constructor TFeGaussElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  stdDev := InvalidD;
end;
//------------------------------------------------------------------------------

procedure TFeGaussElement.Apply;
begin
  if not GetSrcAndDst then Exit;

  if srcImg <> dstImg then
    dstImg.Copy(srcImg, srcRec, dstRec);

  ////True GaussianBlur is visually optimal, but it's also *extremely* slow.
  //GaussianBlur(dstImg, dstRec, Ceil(stdDev *PI * ParentFilterEl.fScale));

  //FastGaussianBlur is a very good approximation and also very much faster.
  //Empirically stdDev * PI/4 more closely emulates other renderers.
  FastGaussianBlur(dstImg, dstRec,
    Ceil(stdDev * PI/4 * ParentFilterEl.fScale), fReader.BlurQuality);
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
  off := dxdy.GetPoint(RectD(pfe.fObjectBounds), fDrawInfo.fontInfo.size);
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
  inherited;
  fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

procedure TClipPathElement.GetPaths(const drawInfo: TDrawInfo);
var
  i: integer;
begin
  if Assigned(drawPathsC) or Assigned(drawPathsO) then Exit;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        GetPaths(drawInfo);
        AppendPath(self.drawPathsO, drawPathsO);
        AppendPath(self.drawPathsC, drawPathsC);
      end;
  drawPathsF := CopyPaths(drawPathsC);
  AppendPath(drawPathsF, drawPathsO);
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
  elRectWH.Init(fPcBelow);
  hasPaths := true;
  fDrawInfo.visible := true;
  //load any class styles
  className := LowercaseAnsi(fName);
  style := fReader.fClassStyles.GetStyle(classname);
  if style <> '' then
    ParseStyle(PAnsiChar(style), Length(style));
end;
//------------------------------------------------------------------------------

function TShapeElement.ParseVal(out val: double): Boolean;
begin
  Result := ParseNextNum(fCurrent, fCurrentEnd, true, val);
end;
//------------------------------------------------------------------------------

function  TShapeElement.GetBounds: TRectD;
var
  i: integer;
begin
  Result := NullRectD;
  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
       Result := UnionRect(Result, TShapeElement(fChilds[i]).GetBounds);
end;
//------------------------------------------------------------------------------

function TShapeElement.HasMarkers: Boolean;
begin
  Result := IsStroked(fDrawInfo) and (Assigned(fDrawInfo.markerStart.name) or
    Assigned(fDrawInfo.markerMiddle.name) or Assigned(fDrawInfo.markerEnd.name));
end;
//------------------------------------------------------------------------------

procedure TShapeElement.Draw(image: TImage32; drawInfo: TDrawInfo);
var
  d           : double;
  img         : TImage32;
  stroked     : Boolean;
  filled      : Boolean;
  clipRec     : TRectD;
  clipRec2    : TRect;
  clipPathEl  : TElement;
  filterEl    : TElement;
  clipPaths   : TPathsD;
  di          : TDrawInfo;
  usingSpecialEffects: Boolean;
begin
  UpdateDrawInfo(drawInfo, self);

  filled := IsFilled(drawInfo);
  stroked := IsStroked(drawInfo);
  GetPaths(drawInfo);

  if not (filled or stroked) or not hasPaths then Exit;
  drawInfo.bounds := GetBoundsD(drawPathsF);

  img := image;
  clipRec2 := NullRect;
  clipPathEl := nil; filterEl := nil;
  if Assigned(drawInfo.clipPathEl.name) then
    clipPathEl := FindRefElement(drawInfo.clipPathEl);
  if Assigned(drawInfo.filterEl.name) then
    filterEl := FindRefElement(drawInfo.filterEl);
  if Assigned(drawInfo.fillEl.name) then
    with TARGB(drawInfo.fillColor) do
      if (A > 0) and (A < 255) then DrawInfo.opacity := A;

  usingSpecialEffects :=
    Assigned(clipPathEl) or Assigned(filterEl) or (DrawInfo.opacity < 255);

  if usingSpecialEffects then
  begin
    //usingSpecialEffects - requires a temporary image
    img := fReader.TempImage;

    //get special effects bounds
    if Assigned(clipPathEl) then
    begin
      di := drawInfo;
      with TClipPathElement(clipPathEl) do
      begin
        GetPaths(di);
        clipPaths := MatrixApply(drawPathsF, di.matrix);
        clipRec := GetBoundsD(clipPaths);
      end;
    end else
    begin
      clipRec := GetBoundsD(drawPathsF);
      if stroked and drawInfo.strokeWidth.IsValid then
      begin
        d := 0.5 * drawInfo.strokeWidth.GetValueXY(
          clipRec, drawInfo.fontInfo.size);
        clipRec := InflateRect(clipRec, d, d);
      end;
      if Assigned(filterEl) then
        with TFilterElement(filterEl) do
          clipRec := GetAdjustedBounds(clipRec);
      MatrixApply(drawInfo.matrix, clipRec);
    end;
    clipRec2 := Rect(clipRec);
    clipRec2 := IntersectRect(clipRec2, img.Bounds);
    if clipRec2.IsEmpty then Exit;
    img.Clear(clipRec2);
  end;

  if filled then
    DrawFilled(img, drawInfo);

  if stroked then
  begin
    if Assigned(drawPathsC) then
      DrawStroke(img, drawInfo, true);
    if stroked and Assigned(drawPathsO) then
      DrawStroke(img, drawInfo, false);
  end;

  if Assigned(filterEl) then
    with TFilterElement(filterEl) do
      Apply(img, clipRec2, drawInfo.matrix);

  if DrawInfo.opacity < 255 then
    img.ReduceOpacity(DrawInfo.opacity, clipRec2);

  if Assigned(clipPathEl) then
    with TClipPathElement(clipPathEl) do
      EraseOutsidePaths(img, clipPaths, fDrawInfo.fillRule, clipRec2);

  if usingSpecialEffects then
    image.CopyBlend(img, clipRec2, clipRec2, BlendToAlpha);

  //todo: enable "paint-order" to change filled/stroked/marker paint order
  if HasMarkers then DrawMarkers(img, drawInfo);
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawMarkers(img: TImage32; drawInfo: TDrawInfo);
var
  i,j: integer;
  sw: double;
  markerEl: TElement;
  markerPaths: TPathsD;
  pt1, pt2: TPointD;
  di: TDrawInfo;
begin
  markerPaths := GetUncurvedPath(drawInfo);
  markerPaths := StripNearDuplicates(markerPaths, 0.01, false);

  if not Assigned(markerPaths) then Exit;
  MatrixApply(drawInfo.matrix, markerPaths);

  di := emptyDrawInfo;

  //prepare to scale the markers by the stroke width
  with fDrawInfo.strokeWidth do
    if IsValid then
      sw := fDrawInfo.strokeWidth.GetValueXY(
        drawInfo.bounds, drawInfo.fontInfo.size)
    else
      sw := 1;
  MatrixScale(di.matrix, sw * ExtractAvgScaleFromMatrix(drawInfo.matrix));

  if Assigned(fDrawInfo.markerStart.name) then
  begin
    markerEl := FindRefElement(fDrawInfo.markerStart);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        for i := 0 to High(markerPaths) do
        begin
          if Length(markerPaths[i]) < 2 then Continue;
          pt1 := markerPaths[i][0];
          pt2 := markerPaths[i][1];
          if autoStartReverse then
            SetEndPoint(pt1, GetAngle(pt2, pt1)) else
            SetEndPoint(pt1, GetAngle(pt1, pt2));
          Draw(img, di);
        end;
      end;
  end;

  if Assigned(fDrawInfo.markerMiddle.name) then
  begin
    markerEl := FindRefElement(fDrawInfo.markerMiddle);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
        for i := 0 to High(markerPaths) do
          if SetMiddlePoints(markerPaths[i]) then
            Draw(img, di);
  end;

  if Assigned(fDrawInfo.markerEnd.name) then
  begin
    markerEl := FindRefElement(fDrawInfo.markerEnd);
    if Assigned(markerEl) and (markerEl is TMarkerElement) then
      with TMarkerElement(markerEl) do
      begin
        for i := 0 to High(markerPaths) do
        begin
          j := High(markerPaths[i]);
          if j < 1 then Continue;
          pt1 := markerPaths[i][j];
          pt2 := markerPaths[i][j-1];
          SetEndPoint(pt1, GetAngle(pt2, pt1));
          Draw(img, di);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.GetPaths(const drawInfo: TDrawInfo);
begin
  //drawPathsO := nil; drawPathsC := nil; drawPathsF := nil;
end;
//------------------------------------------------------------------------------

function  TShapeElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
begin
  Result := nil;
end;
//------------------------------------------------------------------------------

procedure TShapeElement.DrawFilled(img: TImage32; drawInfo: TDrawInfo);
var
  refEl: TElement;
  fillPaths: TPathsD;
begin
  if not assigned(drawPathsF) then Exit;

  fillPaths := MatrixApply(drawPathsF, drawInfo.matrix);
  if Assigned(drawInfo.fillEl.name) then
  begin
    refEl := FindRefElement(drawInfo.fillEl);
    if Assigned(refEl) and (refEl is TFillElement) then
    begin
      if refEl is TRadGradElement then
      begin
        with TRadGradElement(refEl), fReader do
          if PrepareRenderer(RadGradRenderer, drawInfo) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, RadGradRenderer);
      end
      else if refEl is TLinGradElement then
      begin
        with TLinGradElement(refEl), fReader do
          if PrepareRenderer(LinGradRenderer, drawInfo) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, LinGradRenderer);
      end
      else if refEl is TPatternElement then
      begin
        with TPatternElement(refEl), fReader do
          if PrepareRenderer(ImageRenderer, drawInfo) then
            DrawPolygon(img, fillPaths, drawInfo.fillRule, ImageRenderer);
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
  drawInfo: TDrawInfo; isClosed: Boolean);
var
  dashOffset, scaledStrokeWidth, roundingScale: double;
  dashArray: TArrayOfInteger;
  scale: Double;
  strokePaths: TPathsD;
  refEl: TElement;
  endStyle: TEndStyle;
  joinStyle: TJoinStyle;
  bounds: TRectD;
begin
  if isClosed then
  begin
    strokePaths := MatrixApply(drawPathsC, drawInfo.matrix);
    endStyle := esPolygon;
  end else
  begin
    strokePaths := MatrixApply(drawPathsO, drawInfo.matrix);
    endStyle := fDrawInfo.strokeCap;
  end;
  if not Assigned(strokePaths) then Exit;
  joinStyle := fDrawInfo.strokeJoin;

  scale := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  bounds := fReader.rawRect;
  scaledStrokeWidth :=
    drawInfo.strokeWidth.GetValueXY(bounds,drawInfo.fontInfo.size) * scale;
  roundingScale := scale;

  if Length(drawInfo.dashArray) > 0 then
    dashArray := MakeDashArray(drawInfo.dashArray, scale) else
    dashArray := nil;

  if Assigned(dashArray) then
  begin
    dashOffset := drawInfo.dashOffset * scale;
    DrawDashedLine(img, strokePaths, dashArray, @dashOffset,
      scaledStrokeWidth, drawInfo.strokeColor, endStyle)
  end
  else if Assigned(drawInfo.strokeEl.name) then
  begin
    refEl := FindRefElement(drawInfo.strokeEl);
    if not Assigned(refEl) then Exit;

    if refEl is TRadGradElement then
    begin
      with TRadGradElement(refEl) do
        PrepareRenderer(fReader.RadGradRenderer, drawInfo);
      DrawLine(img, strokePaths, scaledStrokeWidth,
        fReader.RadGradRenderer, endStyle, joinStyle, roundingScale);
    end
    else if refEl is TLinGradElement then
    begin
      with TLinGradElement(refEl) do
        PrepareRenderer(fReader.LinGradRenderer, drawInfo);
      DrawLine(img, strokePaths, scaledStrokeWidth,
        fReader.LinGradRenderer, endStyle, joinStyle, roundingScale);
    end
    else if refEl is TPatternElement then
    begin
      with TPatternElement(refEl) do
        PrepareRenderer(fReader.ImageRenderer, drawInfo);
      DrawLine(img, strokePaths,  scaledStrokeWidth,
        fReader.ImageRenderer, endStyle, joinStyle, roundingScale);
    end;
  end
  else if (joinStyle = jsMiter) then
    DrawLine(img, strokePaths, scaledStrokeWidth,
      drawInfo.strokeColor, endStyle, joinStyle, drawInfo.strokeMitLim)
  else
    DrawLine(img, strokePaths,scaledStrokeWidth,
      drawInfo.strokeColor, endStyle, joinStyle, roundingScale);
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

function TPathElement.Parse2Num(var pt: TPointD; isRelative: Boolean): Boolean;
begin
  Result := ParseVal(pt.X) and ParseVal(pt.Y);
  if not Result or not isRelative then Exit;
  pt.X := pt.X + fLastPt.X;
  pt.Y := pt.Y + fLastPt.Y;
end;
//------------------------------------------------------------------------------

function TPathElement.GetBounds: TRectD;
var
  i: integer;
begin
  Result := NullRectD;
  for i := 0 to High(fDpaths) do
    Result := UnionRect(Result, fDpaths[i].GetBounds);
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

      if not Assigned(fCurrSeg) and not PointsEqual(currPt, NullPointD) then
        AddSegPoint(currPt);

      if Assigned(fCurrSeg) then
      begin
        SetLength(fCurrSeg.vals, fCurrSegCnt); //trim buffer
        fCurrDpath.isClosed := false;
      end;
      fCurrDpath := nil;
      fCurrSeg := nil;

      if not Parse2Num(currPt, isRelative) then break;
      self.fLastPt :=  currPt;

      //values immediately following a Move are implicitly Line statements
      if IsNumPending(fCurrent, fCurrentEnd, true) then
        fCurrSegType := dsLine else
        Continue;
    end
    else if (fCurrSegType = dsClose) then
    begin
      if not Assigned(fCurrSeg) and not PointsEqual(currPt, NullPointD) then
        AddSegPoint(currPt);

      if Assigned(fCurrSeg) then
      begin
        SetLength(fCurrSeg.vals, fCurrSegCnt); //trim buffer
        fCurrDpath.isClosed := true;
        fCurrDpath := nil;
        fCurrSeg := nil;
      end;
      Continue;
    end;

    if Assigned(fCurrSeg) then
      SetLength(fCurrSeg.vals, fCurrSegCnt); //trim buffer
    fCurrSeg := nil;

    case fCurrSegType of
      dsHorz:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          ParseVal(currPt.X);
          if isRelative then
            currPt.X := currPt.X + fLastPt.X;
          AddSegValue(currPt.X);
          fLastPt := currPt;
        end;

      dsVert:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          ParseVal(currPt.Y);
          if isRelative then
            currPt.Y := currPt.Y + fLastPt.Y;
          AddSegValue(currPt.Y);
          fLastPt := currPt;
        end;

      dsLine:
        while true do
        begin
          if not Parse2Num(currPt, isRelative) then break;
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
          if not Parse2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;

      dsCSpline:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Parse2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Parse2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;

      dsQBez:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Parse2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Parse2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;

      dsCBez:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not Parse2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Parse2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Parse2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          fLastPt := currPt;
        end;

      dsArc:
        while IsNumPending(fCurrent, fCurrentEnd, true) do
        begin
          if not ParseVal(currPt.X) or
            not ParseVal(currPt.Y) then break;
          AddSegPoint(currPt);                              //radii
          if ParseVal(d) then AddSegValue(d);                 //angle
          if not GetSingleDigit(i) then break;              //arc-flag
          AddSegValue(i);
          if not GetSingleDigit(i) then break;              //sweep-flag
          AddSegValue(i);
          if not Parse2Num(currPt, isRelative) then break;
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
  path := fDpaths[index].GetFlattenedPath(scalePending);
end;
//------------------------------------------------------------------------------

procedure TPathElement.GetPaths(const drawInfo: TDrawInfo);
var
  i: integer;
  scalePending: double;
  isClosed: Boolean;
  path: TPathD;
begin
  if Assigned(drawPathsC) or Assigned(drawPathsO) then Exit;
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  for i := 0 to High(fDpaths) do
  begin
    Flatten(i, scalePending, path, isClosed);
    if not Assigned(path) then Continue;

    if isClosed then
      AppendPath(drawPathsC, path) else
      AppendPath(drawPathsO, path);
  end;
  AppendPath(drawPathsF, drawPathsO);
  AppendPath(drawPathsF, drawPathsC);
end;
//------------------------------------------------------------------------------

function TPathElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
var
  i, len: integer;
begin
  len := Length(fDpaths);
  SetLength(Result, len);
  for i := 0 to High(fDpaths) do
    Result[i] := fDpaths[i].GetUncurvedPath;
end;

//------------------------------------------------------------------------------
// TPolyElement
//------------------------------------------------------------------------------

function TPolyElement.GetBounds: TRectD;
begin
  Result := GetBoundsD(path);
end;
//------------------------------------------------------------------------------

procedure TPolyElement.GetPaths(const drawInfo: TDrawInfo);
begin
  if Assigned(drawPathsC) or Assigned(drawPathsO) then Exit;
  if not Assigned(path) then Exit;
  if (fNameHash = hPolygon) then
  begin
    AppendPath(drawPathsC, path);                    //hPolygon
    drawPathsF := drawPathsC;
  end else
  begin
    AppendPath(drawPathsO, path);                   //hPolyline
    drawPathsF := drawPathsO;
  end;
end;
//------------------------------------------------------------------------------

function TPolyElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
begin
  Result := nil;
  AppendPath(Result, path);
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
    ParseVal(pt.X) and ParseVal(pt.Y) do
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

function TLineElement.GetBounds: TRectD;
begin
  Result := GetBoundsD(path);
end;
//------------------------------------------------------------------------------

procedure TLineElement.GetPaths(const drawInfo: TDrawInfo);
begin
  if Assigned(drawPathsO) then Exit;
  AppendPath(drawPathsO, path);
  drawPathsF := drawPathsO;
end;
//------------------------------------------------------------------------------

function TLineElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
begin
  Result := nil;
  AppendPath(Result, path);
end;

//------------------------------------------------------------------------------
// TCircleElement
//------------------------------------------------------------------------------

constructor TCircleElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  centerPt.Init(1.0);
  radius.Init(0.0);
end;
//------------------------------------------------------------------------------

function TCircleElement.GetBounds: TRectD;
var
  cp  : TPointD;
  r   : double;
begin
  Result := NullRectD;
  if not radius.IsValid then Exit;
  r := radius.GetValue(1);
  cp := centerPt.GetPoint(NullRectD, fDrawInfo.fontInfo.size);
  Result := RectD(cp.X -r, cp.Y -r, cp.X +r, cp.Y +r);
end;
//------------------------------------------------------------------------------

procedure TCircleElement.GetPaths(const drawInfo: TDrawInfo);
var
  scalePending : double;
  rec   : TRectD;
  pt    : TPointD;
  path  : TPathD;
  r: double;
begin
  if Assigned(drawPathsC) then Exit;
  if not radius.IsValid then Exit;

  BestGuessUndefined(radius, centerPt.X);
  r := radius.GetValueXY(drawInfo.bounds, drawInfo.fontInfo.size);
  pt := centerPt.GetPoint(drawInfo.bounds, drawInfo.fontInfo.size);
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  rec := RectD(pt.X -r, pt.Y -r, pt.X +r, pt.Y +r);
  path := Ellipse(rec, scalePending);
  AppendPath(drawPathsC, path);
  drawPathsF := drawPathsC;
end;

//------------------------------------------------------------------------------
// TEllipseElement
//------------------------------------------------------------------------------

constructor TEllipseElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  centerPt.Init(1.0);
  radius.Init(0.0);
end;
//------------------------------------------------------------------------------

function TEllipseElement.GetBounds: TRectD;
var
  cp  : TPointD;
  r   : TPointD;
begin
  Result := NullRectD;
  if not radius.IsValid then Exit;
  r := radius.GetPoint(NullRectD, fDrawInfo.fontInfo.size);
  cp := centerPt.GetPoint(NullRectD, fDrawInfo.fontInfo.size);
  Result := RectD(cp.X -r.X, cp.Y -r.Y, cp.X +r.X, cp.Y +r.X);
end;
//------------------------------------------------------------------------------

procedure TEllipseElement.GetPaths(const drawInfo: TDrawInfo);
var
  scalePending  : double;
  rec       : TRectD;
  path      : TPathD;
  rad       : TPointD;
  centPt    : TPointD;
begin
  if Assigned(drawPathsC) then Exit;

  BestGuessUndefined(radius.X, centerPt.X);
  BestGuessUndefined(radius.Y, centerPt.Y);
  rad := radius.GetPoint(drawInfo.bounds, drawInfo.fontInfo.size);
  centPt := centerPt.GetPoint(drawInfo.bounds, drawInfo.fontInfo.size);
  with centPt do
    rec := RectD(X -rad.X, Y -rad.Y, X +rad.X, Y +rad.Y);
  scalePending := ExtractAvgScaleFromMatrix(drawInfo.matrix);
  path := Ellipse(rec, scalePending);
  AppendPath(drawPathsC, path);
  drawPathsF := drawPathsC;
end;

//------------------------------------------------------------------------------
// TRectElement
//------------------------------------------------------------------------------

constructor TRectElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  radius.Init(1.0);
  elRectWH.width.SetValue(100, muPercent);
  elRectWH.height.SetValue(100, muPercent);
end;
//------------------------------------------------------------------------------

function  TRectElement.GetBounds: TRectD;
begin
  Result := elRectWH.GetRectD(NullRectD, fDrawInfo.fontInfo.size);
end;
//------------------------------------------------------------------------------

procedure TRectElement.GetPaths(const drawInfo: TDrawInfo);
var
  radXY : TPointD;
  bounds: TRectD;
  path  : TPathD;
begin
  if Assigned(drawPathsC) then Exit;
  bounds := elRectWH.GetRectD(drawInfo.bounds, drawInfo.fontInfo.size);
  if bounds.IsEmpty then Exit;

  radXY := radius.GetPoint(bounds, drawInfo.fontInfo.size);
  if (radXY.X > 0) or (radXY.Y > 0) then
  begin
    if (radXY.X <= 0) then radXY.X := radXY.Y
    else if (radXY.Y <= 0) then radXY.Y := radXY.X;
    path := RoundRect(bounds, radXY);
  end else
    path := Rectangle(bounds);
  AppendPath(drawPathsC, path);
  drawPathsF := drawPathsC;
end;
//------------------------------------------------------------------------------

function TRectElement.GetUncurvedPath(const drawInfo: TDrawInfo): TPathsD;
var
  rec: TRectD;
begin
  Result := nil;
  rec := elRectWH.GetRectD(drawInfo.bounds, drawInfo.fontInfo.size);
  if not rec.IsEmpty then
    AppendPath(Result, Rectangle(rec));
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

procedure TTextElement.GetPaths(const drawInfo: TDrawInfo);
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
    elRectWH.Init(0.0);
  end;
  fChilds.add(Result);
end;
//------------------------------------------------------------------------------

procedure TTextElement.Draw(img: TImage32; drawInfo: TDrawInfo);
var
  el        : TElement;
  topTextEl : TTextElement;
begin
  if Self is TTSpanElement then
  begin
    el := fParent;
    while (el is TTSpanElement) do el := el.fParent;
    if not (el is TTextElement) then Exit; //error
    topTextEl := TTextElement(el);
    if elRectWH.left.IsValid then
      tmpPt.X := elRectWH.left.rawVal else
      tmpPt.X := topTextEl.tmpPt.X;
    if elRectWH.top.IsValid then
      tmpPt.Y := elRectWH.top.rawVal else
      tmpPt.Y := topTextEl.tmpPt.Y;
    topTextEl.tmpPt := tmpPt;
  end else
  begin
    if elRectWH.left.IsValid then
      tmpPt.X := elRectWH.left.rawVal else
      tmpPt.X := 0;
    if elRectWH.top.IsValid then
      tmpPt.Y := elRectWH.top.rawVal else
      tmpPt.Y := 0;
  end;

  UpdateDrawInfo(drawInfo, self);
  DrawChildren(img, drawInfo);
end;

//------------------------------------------------------------------------------
// TSubtextElement
//------------------------------------------------------------------------------

constructor TSubtextElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  hasPaths := true;
  fDrawInfo := fParent.fDrawInfo;
  fDrawInfo.matrix := IdentityMatrix;
end;
//------------------------------------------------------------------------------

procedure TSubtextElement.GetPaths(const drawInfo: TDrawInfo);
var
  el : TElement;
  topTextEl : TTextElement;
  s: string;
  i: integer;
  tmpX, offsetX, scale, fontSize, bs: double;
  mat: TMatrixD;
begin
  if Assigned(drawPathsC) then Exit;
  fReader.GetBestFont(drawInfo.FontInfo);
  if drawInfo.FontInfo.size = 0 then
    fontSize := 16 else
    fontSize := drawInfo.FontInfo.size;
  if (textLen = 0) or (fontSize < 2) or
    not Assigned(fReader.fFontCache) then Exit;

  el := fParent;
  while el is TTSpanElement do el := el.fParent;
  if not (el is TTextElement) then Exit;
  topTextEl := TTextElement(el);

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
  scale := fontSize /fReader.fFontCache.FontHeight;

  with topTextEl.tmpPt do
    case drawInfo.FontInfo.align of
      staCenter:
        begin
          offsetX := X - tmpX * scale /2;
          X := X + tmpX * scale/2;
        end;
      staRight:
        begin
          offsetX := X - tmpX * scale;
          X := offsetX;
        end;
      else //staLeft
        begin
          offsetX := X;
          X := X + tmpX * scale;
        end;
    end;

  with drawInfo.fontInfo do
    if baseShift.rawVal = 0 then
      bs := 0 else
      bs := baseShift.GetValue(size);

  mat := IdentityMatrix;
  MatrixScale(mat, scale);
  MatrixTranslate(mat, offsetX, topTextEl.tmpPt.Y - bs);
  MatrixApply(mat, drawPathsC);
  drawPathsF := drawPathsC;
end;

//------------------------------------------------------------------------------
// TTSpanElement
//------------------------------------------------------------------------------

constructor TTSpanElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fDrawInfo.FontInfo.decoration := fdUndefined;
  fDrawInfo.FontInfo.baseShift.SetValue(0);

  elRectWH.Init(0.0);
  tmpPt := InvalidPointD;
  delta := InvalidPointD;
end;

//------------------------------------------------------------------------------
// TTextPathElement
//------------------------------------------------------------------------------

procedure TTextPathElement.GetPaths(const drawInfo: TDrawInfo);
var
  parentTextEl, topTextEl: TTextElement;
  el: TElement;
  isFirst: Boolean;
  s: string;
  i, len, charsThatFit: integer;
  d, fontScale, spacing: double;
  utf8String: AnsiString;
  mat: TMatrixD;
  tmpPath: TPathD;
  isClosed: Boolean;
begin
  if Assigned(drawPathsC) then Exit;
  fReader.GetBestFont(drawInfo.FontInfo);
  if (drawInfo.FontInfo.size < 2) or
    not Assigned(fReader.fFontCache) then Exit;

  parentTextEl := TTextElement(fParent);
  topTextEl := parentTextEl;
  isFirst := IsFirstChild;
  while topTextEl.fNameHash <> hText do
  begin
    isFirst := isFirst and topTextEl.IsFirstChild;
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
      spacing := fDrawInfo.FontInfo.spacing;
    end;
  end else
  begin
    utf8String := GetUtf8String(text, textLen);
    spacing := drawInfo.FontInfo.spacing;
  end;

  //trim CRLFs and multiple spaces
  {$IFDEF UNICODE}
  s := UTF8ToUnicodeString(HtmlDecode(utf8String));
  {$ELSE}
  s := HtmlDecode(utf8String);
  {$ENDIF}
  for i := 1 to Length(s) do
    if s[i] < #32 then s[i] := #32;

  i := PosEx(#32#32, s);
  while i > 0 do
  begin
    Delete(s, i, 1);
    i := PosEx(#32#32, s, i);
  end;

  el := FindRefElement(pathEl);
  if not (el is TPathElement) then Exit;
  fontScale := drawInfo.FontInfo.size/fReader.fFontCache.FontHeight;
  spacing := spacing /fontScale;

  //adjust glyph spacing when fFontInfo.textLength is assigned.
  len := Length(s);
  if (len > 1) and (drawInfo.FontInfo.textLength > 0) then
  begin
    d := fReader.fFontCache.GetTextWidth(s);
    spacing := (drawInfo.FontInfo.textLength/fontScale) - d;
    spacing := spacing / (len -1);
  end;

  with TPathElement(el) do
  begin
    mat := fDrawInfo.matrix;
    MatrixScale(mat, 1/fontScale);
    for i := 0 to High(fDpaths) do
    begin
      Flatten(i, fontScale, tmpPath, isClosed);
      //'path' is temporarily scaled to accommodate fReader.fFontCache's
      //static fontheight. The returned glyphs will be de-scaled later.
      MatrixApply(mat, tmpPath);
      AppendPath(self.drawPathsC,
        GetTextGlyphsOnPath(s, tmpPath, fReader.fFontCache,
          taLeft, 0, spacing, charsThatFit));
      if charsThatFit = Length(s) then Break;
      Delete(s, 1, charsThatFit);
    end;
  end;
  drawPathsC := ScalePath(drawPathsC, fontScale);
  drawPathsF := drawPathsC;
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
  i, len: integer;
  l,t,w,h,scale, a, a2: double;
  mat: TMatrixD;
  angles: TArrayOfDouble;
begin
  UpdateDrawInfo(drawInfo, self);
  mat := drawInfo.matrix;

  if elRectWH.width.IsValid and elRectWH.height.IsValid and
    not markerBoxWH.IsEmpty then
  begin
    w := elRectWH.width.rawVal;
    h := elRectWH.height.rawVal;
    //currently assume preserve aspect ratio
    scale := Min(w/markerBoxWH.Width, h/markerBoxWH.Height);
    MatrixScale(mat, scale, scale);
  end;

  if refPt.X.IsValid and refPt.Y.IsValid then
  begin
    l := refPt.X.rawVal;
    t := refPt.Y.rawVal;
    scale := ExtractAvgScaleFromMatrix(mat);
    MatrixTranslate(mat, -l * scale, -t * scale);
  end;

  len := Length(fPoints);
  if len = 0 then Exit;
  SetLength(angles, len);
  angles[0] := angle;
  a := angle;
  for i := 0 to len -2 do
  begin
    a2 := GetAngle(fPoints[i], fPoints[i+1]);
    angles[i] := Average(a, a2);
    a := a2;
  end;
  if len > 1 then
    angles[len -1] := Average(a, angle2);

  //for each 'point' draw the marker
  for i := 0 to len -1 do
  begin
    drawInfo.matrix := mat;
    MatrixRotate(drawInfo.matrix, NullPointD, angles[i]);
    MatrixTranslate(drawInfo.matrix, fPoints[i].X, fPoints[i].Y);
    DrawChildren(img, drawInfo);
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
  len := Length(points);
  Result := len > 2;
  if Result then
  begin
    angle := GetAngle(Points[0],Points[1]);
    angle2 := GetAngle(Points[len-2],Points[len-1]);
    Self.fPoints := Copy(points, 1, len -2);
  end;
end;

//------------------------------------------------------------------------------
// TPatternElement
//------------------------------------------------------------------------------

constructor TPatternElement.Create(parent: TElement; hashName: Cardinal);
begin
  inherited;
  fPcBelow := 1.0;
  elRectWH.Init(fPcBelow);
  pattBoxWH.Width   := InvalidD;
  pattBoxWH.Height  := InvalidD;
  fDrawInfo.visible := false;
end;
//------------------------------------------------------------------------------

function TPatternElement.PrepareRenderer(renderer: TImageRenderer;
  drawInfo: TDrawInfo): Boolean;
var
  i     : integer;
  recWH : TRectWH;
  el    : TElement;
  rec   : TRectD;
  mat   : TMatrixD;
  sx,sy : double;
  scale: TSizeD;
  closedPaths, openPaths: TPathsD;
begin
  Result := false;

  //ExtractAllFromMatrix(drawInfo.matrix, angle, scale, skew, trans);
  scale := ExtractScaleFromMatrix(drawInfo.matrix);

  if units = hUserSpaceOnUse then
    rec := fReader.rawRect else
    rec := drawInfo.bounds;

  //todo: implement patternUnits & patternContentUnits too

  sx := 1; sy := 1;
  if elRectWH.Width.IsValid and elRectWH.Height.IsValid then
  begin
    recWH := elRectWH.GetRectWH(rec, drawInfo.fontInfo.size);

    //also scale if necessary
    if not pattBoxWH.IsEmpty then
    begin
      sx := recWH.Width/pattBoxWH.Width;
      sy := recWH.Height/pattBoxWH.Height;
    end;

  end
  else if not pattBoxWH.IsEmpty then
  begin
    recWH.Width   := pattBoxWH.Width;
    recWH.Height  := pattBoxWH.Width;
  end else
    Exit;

  renderer.Image.SetSize(
    Round(recWH.Width * scale.sx),
    Round(recWH.Height * scale.sy));

  Result := true;
  closedPaths := nil;
  openPaths   := nil;

  mat := IdentityMatrix;
  MatrixScale(mat, scale.sx * sx, scale.sy * sy);

  //recWH.Left := 0; recWH.Top := 0;
  if Assigned(refEl.name) then
  begin
    el := FindRefElement(refEl);
    if Assigned(el) and (el is TShapeElement) then
      with TShapeElement(el) do
      begin
        drawInfo := fDrawInfo;
        drawInfo.matrix := mat;
        drawInfo.bounds := recWH.RectD;
        Draw(renderer.Image, drawInfo);
      end;
  end;

  for i := 0 to fChilds.Count -1 do
    if TElement(fChilds[i]) is TShapeElement then
      with TShapeElement(fChilds[i]) do
      begin
        drawInfo := fDrawInfo;
        drawInfo.matrix := mat;
        drawInfo.bounds := rec;
        Draw(renderer.Image, drawInfo);
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
  fDrawInfo :=  emptyDrawInfo;
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

  fPcBelow     := parent.fPcBelow;
  elRectWH.Init(fPcBelow);
  self.fParent := parent;
  fReader      := parent.fReader;
  fCurrentEnd  := fReader.fEndStream;
  fDrawInfo    := emptyDrawInfo;

  fCurrent     := parent.fCurrent;
  if SkipBlanks(fCurrent, fCurrentEnd) then
    GetName(fName);
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

function  TElement.IsFirstChild: Boolean;
begin
  Result := not Assigned(fParent) or (self = fParent.fChilds[0]);
end;
//------------------------------------------------------------------------------

procedure TElement.Draw(image: TImage32; drawInfo: TDrawInfo);
begin
  DrawChildren(image, drawInfo);
end;
//------------------------------------------------------------------------------

procedure TElement.DrawChildren(image: TImage32; drawInfo: TDrawInfo);
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    with TElement(fChilds[i]) do
      if fDrawInfo.visible then Draw(image, drawInfo);
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
var
  q: AnsiChar;
begin
  Result := (fCurrent^ = '"') or (fCurrent^ = quote);
  if not Result then Exit;
  q := fCurrent^;
  inc(fCurrent);
  attrib.aValue.name := fCurrent;
  while (fCurrent < fCurrentEnd) and (fCurrent^ <> q)  do inc(fCurrent);
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
    case fCurrent^ of
      '/', '>': //end of attributes
        begin
          fHasContent := (fCurrent^ <> '/');
          if fHasContent then
            inc(fCurrent) else
            inc(fCurrent, 2);
          Result := true;
          break;
        end;
      'x':  //ignore xml namespace prfix
        if ((fCurrent +1)^ = 'm') and
          ((fCurrent +2)^ = 'l') and
          ((fCurrent +3)^ = ':') then
            inc(fCurrent, 4);
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

  //parse 'style' attribute AFTER other attributes
  //have been set since style attributes have precedence.
  with Result do
  begin
    if fStyleAttribIdx >= 0 then
      with fAttribs[fStyleAttribIdx] do
        ParseStyle(aValue.name, aValue.len);

    //likewise parse class styles too
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
      if (fCurrent -1)^ = #32 then dec(fCurrent);
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
// TSvgReader
//------------------------------------------------------------------------------

constructor TSvgReader.Create;
begin
  fMemStream          := TMemoryStream.Create;
  fIdList             := TStringList.Create;
  //fIdList.Duplicates := dupError; // I'm unsure how best to manage this, but
  fIdList.Duplicates  := dupIgnore;  // pro tem, duplicates will be ignored.
  fIdList.Sorted      := True;
  fBlurQuality        := 1; //0..2: 0=OK (fast); 2=Great (slow)
  currentColor        := clBlack32;
  fClassStyles        := TClassStylesList.Create;

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
  currentColor  := clBlack32;
  rawRect       := NullRectD;
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
begin
  if not Assigned(fRootElement) then Exit;

  with fRootElement do
  begin
    di := emptyDrawInfo;
    MatrixTranslate(di.matrix, -viewboxWH.Left, -viewboxWH.Top);

    w := elRectWH.width.GetValueX(RectWidth(img.Bounds), 0);
    h := elRectWH.height.GetValueY(RectHeight(img.Bounds), 0);

    if viewboxWH.IsEmpty then
    begin
      fRootElement.viewboxWH := RectWH(0, 0, w, h);
      if fRootElement.viewboxWH.IsEmpty then
        fRootElement.viewboxWH := RectWH(0, 0, img.Width, img.Height);
      fDrawInfo.bounds := viewboxWH.RectD;
      rawRect  := viewboxWH.RectD;
    end
    else if (w > 0) or (h > 0) then
    begin
      fDrawInfo.bounds := viewboxWH.RectD;
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
    begin
      fDrawInfo.bounds := viewboxWH.RectD;
      rawRect  := viewboxWH.RectD;
    end;
    di.bounds := fDrawInfo.bounds;

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

function MacStylesToFontStyles(macStyles: TMacStyles): TSvgFontSyles;
begin
  if msBold in macStyles then
  begin
    if msItalic in macStyles then
      Result := sfsBoldItalic else
      Result := sfsBold;
  end
  else if msItalic in macStyles then
    Result := sfsItalic
  else
    Result := sfsNone;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.GetBestFont(const svgFontInfo: TSVGFontInfo);

  function GetStyleInt(fontReaderIdx: integer): integer;
  begin
    with TFontReader(fFontList[fontReaderIdx]) do
      Result := Max(1, Ord(MacStylesToFontStyles(FontInfo.macStyles)));
  end;

  function GetFamilyInt(fontReaderIdx: integer): integer;
  begin
    with TFontReader(fFontList[fontReaderIdx]) do
      Result := Ord(FontFamily);
  end;

  function CompareStyleInts(int1, int2: integer): integer;
  begin
    //sfsNone, sfsBold, sfsItalic, sfsBoldItalic
    Result := Abs(int1 - int2);
    if Result = 2 then Dec(Result);
  end;

  function CompareFamilyInts(int1, int2: integer): integer;
  begin
    //ttfUnknown, ttfSerif, ttfSansSerif, ttfMonospace
    Result := Min(1, Abs(int1 - int2));
    if (int1 = 0) or (int2 = 0) then Result := 0;
  end;

var
  i,j,k, bestIdx, styleDiff, familyDiff, thisStyle, thisFamily: integer;
  bestFontReader: TFontReader;
begin
  if fFontList.Count = 0 then Exit;
  thisStyle := Max(1, Ord(svgFontInfo.styles));
  if svgFontInfo.family = ttfUnknown then
    thisFamily := Ord(ttfSansSerif) else    //default to sans-serif
    thisFamily := Ord(svgFontInfo.family);

  bestIdx := 0;
  styleDiff := CompareStyleInts(thisStyle, GetStyleInt(0));
  familyDiff := CompareFamilyInts(thisFamily, GetFamilyInt(0));
  for i := 1 to fFontList.Count -1 do
  begin
    j := CompareStyleInts(thisStyle, GetStyleInt(i));
    k := CompareFamilyInts(thisFamily, GetFamilyInt(i));
    if (j < styleDiff) or ((j = styleDiff) and (k < familyDiff)) then
    begin
      bestIdx := i;
      styleDiff := j;
      familyDiff := k;
      if (j = 0) and (k = 0) then Break;
    end;
  end;

  bestFontReader := TFontReader(fFontList[bestIdx]);
  if Assigned(fFontCache) then
    fFontCache.FontReader := bestFontReader else
    fFontCache := TGlyphCache.Create(bestFontReader, defaultFontHeight);

  fFontCache.Underlined := False;
  fFontCache.StrikeOut := False;
  case svgFontInfo.decoration of
    fdUnderline     : fFontCache.Underlined := true;
    fdStrikeThrough : fFontCache.StrikeOut := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.SetBlurQuality(value: integer);
begin
  if value <= 0 then fBlurQuality := 0
  else if value >= 2 then fBlurQuality := 2
  else fBlurQuality := 1;
end;
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

procedure GradientUnits_Attrib(attrib: PAttrib);
begin
  if attrib.aOwnerEl is TFillElement then
    with TFillElement(attrib.aOwnerEl) do
      units := GetHashedName(attrib.aValue);
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
  with TTextElement(attrib.aOwnerEl) do
    AttribToFloat(attrib, fDrawInfo.FontInfo.spacing);
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

procedure BaselineShift_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
  word: TAnsiName;
  c, endC: PAnsiChar;
begin
  c := attrib.aValue.name;
  endC := c + attrib.aValue.len;
  ParseNextWord(c, endC, word);
  with attrib.aOwnerEl.fDrawInfo.FontInfo do
    case GetHashedName(word) of
      hSuper: baseShift.SetValue(50, muPercent);
      hSub: baseShift.SetValue(-50, muPercent);
      hBaseline: baseShift.SetValue(0, muPixel);
      else
      begin
        AttribToFloat(attrib, val, mu);
        baseShift.SetValue(val, mu);
      end;
    end;
end;
//------------------------------------------------------------------------------

procedure Class_Attrib(attrib: PAttrib);
begin
  TElement(attrib.aOwnerEl).ParseClassAttrib(attrib);
end;
//------------------------------------------------------------------------------

procedure Color_Attrib(attrib: PAttrib);
var
  color: TColor32;
begin
  color := clInvalid;
  AttribToColor32(attrib, color);
  //for setting currentcolor when drawing (eg drawing shapes)
  attrib.aOwnerEl.fDrawInfo.currentColor := color;
  //for setting currentcolor during element creation (eg gradient colors)
  attrib.aOwnerEl.fReader.currentColor := color;
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
  AttribToFontInfo(attrib, attrib.aOwnerEl.fDrawInfo.FontInfo);
end;
//------------------------------------------------------------------------------

procedure FontFamily_Attrib(attrib: PAttrib);
var
  word: TAnsiName;
  c, endC: PAnsiChar;
begin
  c :=  attrib.aValue.name;
  with attrib.aOwnerEl.fDrawInfo.FontInfo do
  begin
    family := ttfUnknown;
    endC := c + attrib.aValue.len;
    while ParseNextWordEx(c, endC, word) do
    begin
      case GetHashedName(word) of
        hSans_045_Serif, hArial  : family := ttfSansSerif;
        hSerif, hTimes: family := ttfSerif;
        hMonospace: family := ttfMonospace;
        else Continue;
      end;
      break;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure FontSize_Attrib(attrib: PAttrib);
var
  num: double;
begin
  with attrib.aValue do
    if not ParseNextNum(name, name + len, false, num) then Exit;
  attrib.aOwnerEl.fDrawInfo.FontInfo.size := num;
end;
//------------------------------------------------------------------------------

procedure FontStyle_Attrib(attrib: PAttrib);
begin
  with attrib.aOwnerEl.fDrawInfo.FontInfo do
    if GetHashedName(attrib.aValue) = hItalic then
      if styles = sfsBoldItalic then
        styles := sfsBold else
        styles := sfsNone;
end;
//------------------------------------------------------------------------------

procedure FontWeight_Attrib(attrib: PAttrib);

  procedure IncludeBold;
  begin
    with attrib.aOwnerEl.fDrawInfo.FontInfo do
      if styles = sfsItalic then
        styles := sfsBoldItalic else
        styles := sfsBold;
  end;

  procedure ExcludeBold;
  begin
    with attrib.aOwnerEl.fDrawInfo.FontInfo do
      if styles = sfsBoldItalic then
        styles := sfsItalic else
        styles := sfsNone;
  end;

var
  num: double;
  word: TAnsiName;
  c, endC: PAnsiChar;
begin
  c := attrib.aValue.name;
  endC := c + attrib.aValue.len;
  begin
    if IsNumPending(c, endC, false) and
      ParseNextNum(c, endC, false, num) then
    begin
      if num >= 600 then
        IncludeBold else
        ExcludeBold;
    end
    else if ExtractWordFromValue(attrib.aValue, word) then
    begin
      case GetHashedName(word) of
        hBold, hBolder: IncludeBold
        else ExcludeBold;
      end;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure Fx_Attrib(attrib: PAttrib);
begin
  if (attrib.aOwnerEl is TRadGradElement) then
    with TRadGradElement(attrib.aOwnerEl) do
    begin
      AttribToFloat(attrib, F.X.rawVal, F.X.mu);
    end;
end;
//------------------------------------------------------------------------------

procedure Fy_Attrib(attrib: PAttrib);
begin
  if (attrib.aOwnerEl is TRadGradElement) then
    with TRadGradElement(attrib.aOwnerEl) do
    begin
      AttribToFloat(attrib, F.Y.rawVal, F.Y.mu);
    end;
end;
//------------------------------------------------------------------------------

procedure TextAlign_Attrib(attrib: PAttrib);
begin
  with attrib.aOwnerEl.fDrawInfo.FontInfo do
    case GetHashedName(attrib.aValue) of
      hMiddle : align := staCenter;
      hEnd    : align := staRight;
      else align := staLeft;
    end;
end;
//------------------------------------------------------------------------------

procedure TextDecoration_Attrib(attrib: PAttrib);
begin
  with attrib.aOwnerEl.fDrawInfo.FontInfo do
    case GetHashedName(attrib.aValue) of
      hUnderline        : decoration := fdUnderline;
      hline_045_through : decoration := fdStrikeThrough;
      else                decoration := fdNone;
    end;
end;
//------------------------------------------------------------------------------

procedure TextLength_Attrib(attrib: PAttrib);
begin
  AttribToFloat(attrib, attrib.aOwnerEl.fDrawInfo.FontInfo.textLength);
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
  val: TValue;
begin
  if (attrib.aOwnerEl is TGradStopElement) then
    with TGradStopElement(attrib.aOwnerEl) do
    begin
      val.Init(0);
      AttribToFloat(attrib, val.rawVal, val.mu);
      offset := val.GetValue(1);
    end
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
var
  acolor: TColor32;
begin
  if attrib.aOwnerEl is TGradStopElement then
  begin
    acolor := clInvalid;
    AttribToColor32(attrib, acolor);
    with TGradStopElement(attrib.aOwnerEl) do
      if acolor = clCurrent then
        color := attrib.aOwnerEl.fReader.currentColor else
        color := acolor;
  end;
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

procedure StrokeLineCap_Attrib(attrib: PAttrib);
var
  word: TAnsiName;
  c, endC: PAnsiChar;
begin
  c := attrib.aValue.name;
  endC := c + attrib.aValue.len;
  ParseNextWord(c, endC, word);
  with attrib.aOwnerEl.fDrawInfo do
    case GetHashedName(word) of
      hButt   : strokeCap := esButt;
      hRound  : strokeCap := esRound;
      hSquare : strokeCap := esSquare;
    end;
end;
//------------------------------------------------------------------------------

procedure StrokeLineJoin_Attrib(attrib: PAttrib);
var
  word: TAnsiName;
  c, endC: PAnsiChar;
begin
  c := attrib.aValue.name;
  endC := c + attrib.aValue.len;
  ParseNextWord(c, endC, word);
  with attrib.aOwnerEl.fDrawInfo do
    case GetHashedName(word) of
      hMiter  : strokeJoin := jsMiter;
      hRound  : strokeJoin := jsRound;
      hBevel  : strokeJoin := jsSquare;
    end;
end;
//------------------------------------------------------------------------------

procedure StrokeMiterLimit_Attrib(attrib: PAttrib);
begin
  AttribToFloat(attrib, attrib.aOwnerEl.fDrawInfo.strokeMitLim);
end;
//------------------------------------------------------------------------------

procedure StrokeOpacity_Attrib(attrib: PAttrib);
begin
  AttribToOpacity(attrib, attrib.aOwnerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeWidth_Attrib(attrib: PAttrib);
begin
  with attrib.aOwnerEl do
  begin
    AttribToFloat(attrib,
      fDrawInfo.strokewidth.rawVal, fDrawInfo.strokewidth.mu);
  end;
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
  case attrib.aOwnerEl.fNameHash of
    hSvg    : TSvgElement(attrib.aOwnerEl).viewboxWH := LoadViewbox;
    hMarker : TMarkerElement(attrib.aOwnerEl).markerBoxWH := LoadViewbox;
    hSymbol : TSymbolElement(attrib.aOwnerEl).viewboxWH := LoadViewbox;
    else if attrib.aOwnerEl is TPatternElement then
      TPatternElement(attrib.aOwnerEl).pattBoxWH := LoadViewbox;
  end;
end;
//------------------------------------------------------------------------------

procedure Height_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  with attrib.aOwnerEl do
  begin
    elRectWH.height.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure Width_Attrib(attrib: PAttrib);
var
  mu: TMeasureUnit;
  val: double;
begin
  AttribToFloat(attrib, val, mu);
  with attrib.aOwnerEl do
  begin
    elRectWH.width.SetValue(val, mu);
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
      with TRadGradElement(attrib.aOwnerEl) do
      begin
        C.X.SetValue(val, mu);
      end;
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
      with TRadGradElement(attrib.aOwnerEl) do
      begin
        C.Y.SetValue(val, mu);
      end;
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
      with TRectElement(attrib.aOwnerEl) do
      begin
        radius.X.SetValue(val, mu);
      end;
    hCircle:
      with TCircleElement(attrib.aOwnerEl) do
      begin
        radius.SetValue(val, mu);
      end;
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do
      begin
        radius.X.SetValue(val, mu);
      end;
    hRadialGradient:
      with TRadGradElement(attrib.aOwnerEl) do
      begin
        radius.X. SetValue(val, mu);
        radius.Y. SetValue(val, mu);
      end;
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do
        refPt.X.SetValue(val, mu);
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
      with TRectElement(attrib.aOwnerEl) do
      begin
        radius.Y.SetValue(val, mu);
      end;
    hEllipse:
      with TEllipseElement(attrib.aOwnerEl) do
      begin
        radius.Y.SetValue(val, mu);
      end;
    hMarker:
      with TMarkerElement(attrib.aOwnerEl) do refPt.Y.SetValue(val, mu);
  end;
end;
//------------------------------------------------------------------------------

procedure SpreadMethod_Attrib(attrib: PAttrib);
var
  word: TAnsiName;
  c, endC: PAnsiChar;
begin
  if not (attrib.aOwnerEl is TGradientElement) then Exit;
  c := attrib.aValue.name;
  endC := c + attrib.aValue.len;
  ParseNextWord(c, endC, word);
  with TGradientElement(attrib.aOwnerEl) do
    case GetHashedName(word) of
      hPad      : spreadMethod := gfsClamp;
      hReflect  : spreadMethod := gfsMirror;
      hRepeat   : spreadMethod := gfsRepeat;
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
      end;
    hFilter:
      with attrib.aOwnerEl do
      begin
        elRectWH.left.SetValue(val, mu);
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
      end;
    hFilter:
      with attrib.aOwnerEl do
      begin
        elRectWH.top.SetValue(val, mu);
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
      end;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//https://oreillymedia.github.io/Using_SVG/guide/style.html

procedure MakeAttribFuncList;
begin
  AttribFuncList := TStringList.Create;
  AttribFuncList.Duplicates := dupError;

  RegisterAttribute(hbaseline_045_shift,    BaselineShift_Attrib);
  RegisterAttribute(hClass,                 Class_Attrib);
  RegisterAttribute(hColor,                 Color_Attrib);
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
  RegisterAttribute(hGradientUnits,         GradientUnits_Attrib);
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
  RegisterAttribute(hPatternUnits,          GradientUnits_Attrib);
  RegisterAttribute(hPatternTransform,      Transform_Attrib);
  RegisterAttribute(hPoints,                Points_Attrib);
  RegisterAttribute(hR,                     Rx_Attrib);
  RegisterAttribute(hRefX,                  Rx_Attrib);
  RegisterAttribute(hRefY,                  Ry_Attrib);
  RegisterAttribute(hResult,                Result_Attrib);
  RegisterAttribute(hRx,                    Rx_Attrib);
  RegisterAttribute(hRy,                    Ry_Attrib);
  RegisterAttribute(hSpreadMethod,          SpreadMethod_Attrib);
  RegisterAttribute(hstdDeviation,          StdDev_Attrib);
  RegisterAttribute(hStop_045_Color,        StopColor_Attrib);
  RegisterAttribute(hStop_045_Opacity,      StopOpacity_Attrib);
  RegisterAttribute(hStroke,                Stroke_Attrib);
  RegisterAttribute(hstroke_045_linecap,    StrokeLineCap_Attrib);
  RegisterAttribute(hstroke_045_linejoin,   StrokeLineJoin_Attrib);
  RegisterAttribute(hstroke_045_miterlimit, StrokeMiterLimit_Attrib);
  RegisterAttribute(hStroke_045_Opacity,    StrokeOpacity_Attrib);
  RegisterAttribute(hStroke_045_Width,      StrokeWidth_Attrib);
  RegisterAttribute(hStyle,                 Style_Attrib);
  RegisterAttribute(hText_045_Anchor,       TextAlign_Attrib);
  RegisterAttribute(hText_045_Decoration,   TextDecoration_Attrib);
  RegisterAttribute(hTextLength,            TextLength_Attrib);
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
