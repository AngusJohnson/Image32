unit Image32_SVG_Reader;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.23                                                            *
* Date      :  12 April 2021                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2021                                         *
*                                                                              *
* Purpose   :  Read simple SVG files                                           *
*                                                                              *
* Note: this unit is just an early beta release, there's still much to do.     *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*******************************************************************************)

//immediate todos:
//clipPath, url, gradients, class (for styles), style CDATA, lineCap, lineJoin

interface

{$I Image32.inc}

uses
  SysUtils, Classes, Types, Math,
  Image32, Image32_Vector, Image32_Transform;

type
 TCustomSvgFiller = class;

  PDrawInfo = ^TDrawInfo;
  TDrawInfo = record
  public
    fillColor   : TColor32;
    fillRule    : TFillRule;
    filler      : TCustomSvgFiller;

    strokeColor : TColor32;
    strokeWidth : double;
    //lineCap  : TEndStyle;     //stroke-linecap
    //lineJoin : TJoinStyle;    //stroke-linejoin
    //miterLim : double;        //stroke-miterlimit
    matrix      : TMatrixD;
    inDefs      : Boolean;
  end;

  TElement = class;

  PAttrib = ^TAttrib;
  TAttrib = {$IFDEF RECORD_METHODS} record {$ELSE} object {$ENDIF}
    ownerEl  : TElement;
    aname     : PAnsiChar;
    nameLen  : integer;
    value    : PAnsiChar;
    valueLen : integer;
    function GetName: AnsiString;
    function GetValue: AnsiString;
  end;
  TArrayOfAttrib = array of TAttrib;

  TShapeInfoClass = class of TShapeInfo;
  TShapeInfo = class
  protected
    readPos     : PAnsiChar;
    readEnd     : PAnsiChar;
    isClosed    : Boolean;
    path        : TPathD;
    fOwnerEl    : TElement;
    function GetVal(out val: double): Boolean;
    function GetCount: integer; virtual;
    function GetDrawInfo: TDrawInfo;
  public
    constructor Create(ownerEl: TElement; isClosed: Boolean);
    procedure LoadFromAttrib(const attrib: TAttrib); virtual;
    procedure LoadFromOwnerElement; virtual;
    function Save: AnsiString; virtual; abstract;
    function GetFlattenedPath(index: integer;
      out path: TPathD; out isClosed: Boolean): Boolean;  virtual;
    property Count: integer read GetCount;
    property DrawInfo: TDrawInfo read GetDrawInfo;
  end;

  TDsegType = (dsMove, dsLine, dsHorz, dsVert, dsArc,
    dsQBez, dsCBez, dsQSpline, dsCSpline, dsClose);

  PDpathSeg = ^TDpathSeg;
  TDpathSeg = record
    segType : TDsegType;
    vals    : TArrayOfDouble;
  end;

  PDpath = ^TDpath;
  TDpath = record
    firstPt   : TPointD;
    isClosed  : Boolean;
    segs      : array of TDpathSeg;
  end;
  TDpaths = array of TDpath;
  
  TDpathInfo = class(TShapeInfo)
  private
    currSeg     : PDpathSeg;
    currSegCap  : integer;
    currSegCnt  : integer;
    currSegType : TDsegType;
    currDpath   : PDpath;
    dpaths      : TDpaths;
    lastPt      : TPointD;
    function GetSegType(out isRelative: Boolean): Boolean;
    function GetSingleDigit(out digit: integer): Boolean;
    procedure StartNewDpath;
    procedure StartNewSeg(segType: TDsegType);
    procedure AddSegValue(val: double);
    procedure AddSegPoint(const pt: TPointD);
    function Get2Num(var pt: TPointD; isRelative: Boolean): Boolean;
    procedure Flatten(index: integer;  out path: TPathD; out isClosed: Boolean);
  protected
    function GetCount: integer; override;
  public
    procedure LoadFromAttrib(const attrib: TAttrib);  override;
    function Save: AnsiString; override;
    function GetFlattenedPath(index: integer;
      out path: TPathD; out isClosed: Boolean): Boolean;  override;
  end;

  TPolyInfo = class(TShapeInfo) //can be polyline or polygon
  private
    currCap     : integer;
    currCnt     : integer;
    procedure AddPoint(const pt: TPointD);
  public
    procedure LoadFromAttrib(const attrib: TAttrib); override;
  end;

  TSingleLineInfo = class(TShapeInfo)
  public
    procedure LoadFromOwnerElement; override;
  end;

  TCircleInfo = class(TShapeInfo)
  public
    procedure LoadFromOwnerElement; override;
  end;

  TEllipseInfo = class(TShapeInfo)
  public
    procedure LoadFromOwnerElement; override;
  end;

  TRectInfo = class(TShapeInfo)
  public
    procedure LoadFromOwnerElement; override;
  end;

  TSvgReader = class;

  TElement = class
  private
    fParent         : TElement;
    fReader         : TSvgReader;
    fChilds         : TList;
    fIsValid        : Boolean;
    fName           : PAnsiChar;
    fNameLen        : integer;
    fAttribs        : TArrayOfAttrib;
    fdrawInfo       : TDrawInfo;
    fSvgEnd         : PAnsiChar;
    fShapeInfo      : TShapeInfo;
    function GetChildCount: integer;
    function GetChild(index: integer): TElement;
    function LoadAttributes(var c: PAnsiChar;
      out hasContent: Boolean): Boolean;
    function LoadElemOrAttribName(var c: PAnsiChar;
      out name: PAnsiChar; out nameLen: integer): Boolean;
    function LoadAttribValue(var c: PAnsiChar;
      out value: PAnsiChar; out valueLen: integer): Boolean;
    procedure LoadUse;
    procedure LoadContent(var c: PAnsiChar);
    function GetAttribute(const attribName: AnsiString): PAttrib;
    procedure ProcessAttrib(const attrib: TAttrib);
  protected
     function CreateShapeInfo(shapeClass: TShapeInfoClass; isClosed: Boolean): TShapeInfo;
     property ShapeInfo : TShapeInfo read fShapeInfo;
  public
    constructor Create(parent: TElement; var c: PAnsiChar);
    destructor  Destroy; override;
    property   ChildCount: integer read GetChildCount;
    property   Child[index: integer]: TElement read GetChild;
  end;

  TAttribFunc = procedure (const attrib: TAttrib);

  TSvgReader = class
  private
    fMemStream     : TMemoryStream;
    fEndStream     : PAnsiChar;
    fIdList        : TStringList;
    fGradientsList : TStringList;
    fShapesList    : TList;
    fViewbox       : TRectD;
    fRootElement   : TElement;
    function GetSvgStart(out svgStart: PAnsiChar): Boolean;
    procedure GetViewBox;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DrawImage(img: TImage32; scaleToImage: Boolean);
    function LoadFromStream(stream: TStream): Boolean;
    function LoadFromFile(const filename: string): Boolean;
  end;

 TCustomSvgFiller = class
 end;

 TSvgColorStop = record
   color: TColor32;
   offset: double;
 end;
 TSvgColorStops = array of TSvgColorStop;

 TLinearGradientSvgFiller = class(TCustomSvgFiller)
 public
   fAttributeNames: array of string;
   fAttributeValues: array of string;
 public
 end;


implementation

uses
  Image32_Draw;

type
  TColorConst = record
    ColorName : string; ColorValue: Cardinal;
  end;

const
  ColorConsts: array[0..144] of TColorConst = (
    (ColorName: 'aliceblue'; ColorValue: $FFF0F8FF),
    (ColorName: 'antiquewhite'; ColorValue: $FFFAEBD7),
    (ColorName: 'aqua'; ColorValue: $FF00FFFF),
    (ColorName: 'aquamarine'; ColorValue: $FF7FFFD4),
    (ColorName: 'azure'; ColorValue: $FFF0FFFF),
    (ColorName: 'beige'; ColorValue: $FFF5F5DC),
    (ColorName: 'bisque'; ColorValue: $FFFFE4C4),
    (ColorName: 'black'; ColorValue: $FF000000),
    (ColorName: 'blanchedalmond'; ColorValue: $FFFFEBCD),
    (ColorName: 'blue'; ColorValue: $FF0000FF),
    (ColorName: 'blueviolet'; ColorValue: $FF8A2BE2),
    (ColorName: 'brown'; ColorValue: $FFA52A2A),
    (ColorName: 'burlywood'; ColorValue: $FFDEB887),
    (ColorName: 'cadetblue'; ColorValue: $FF5F9EA0),
    (ColorName: 'chartreuse'; ColorValue: $FF7FFF00),
    (ColorName: 'chocolate'; ColorValue: $FFD2691E),
    (ColorName: 'coral'; ColorValue: $FFFF7F50),
    (ColorName: 'cornflowerblue'; ColorValue: $FF6495ED),
    (ColorName: 'cornsilk'; ColorValue: $FFFFF8DC),
    (ColorName: 'crimson'; ColorValue: $FFDC143C),
    (ColorName: 'cyan'; ColorValue: $FF00FFFF),
    (ColorName: 'darkblue'; ColorValue: $FF00008B),
    (ColorName: 'darkcyan'; ColorValue: $FF008B8B),
    (ColorName: 'darkgoldenrod'; ColorValue: $FFB8860B),
    (ColorName: 'darkgray'; ColorValue: $FFA9A9A9),
    (ColorName: 'darkgreen'; ColorValue: $FF006400),
    (ColorName: 'darkgrey'; ColorValue: $FFA9A9A9),
    (ColorName: 'darkkhaki'; ColorValue: $FFBDB76B),
    (ColorName: 'darkmagenta'; ColorValue: $FF8B008B),
    (ColorName: 'darkolivegreen'; ColorValue: $FF556B2F),
    (ColorName: 'darkorange'; ColorValue: $FFFF8C00),
    (ColorName: 'darkorchid'; ColorValue: $FF9932CC),
    (ColorName: 'darkred'; ColorValue: $FF8B0000),
    (ColorName: 'darksalmon'; ColorValue: $FFE9967A),
    (ColorName: 'darkseagreen'; ColorValue: $FF8FBC8F),
    (ColorName: 'darkslateblue'; ColorValue: $FF483D8B),
    (ColorName: 'darkslategray'; ColorValue: $FF2F4F4F),
    (ColorName: 'darkslategrey'; ColorValue: $FF2F4F4F),
    (ColorName: 'darkturquoise'; ColorValue: $FF00CED1),
    (ColorName: 'darkviolet'; ColorValue: $FF9400D3),
    (ColorName: 'deeppink'; ColorValue: $FFFF1493),
    (ColorName: 'deepskyblue'; ColorValue: $FF00BFFF),
    (ColorName: 'dimgray'; ColorValue: $FF696969),
    (ColorName: 'dimgrey'; ColorValue: $FF696969),
    (ColorName: 'dodgerblue'; ColorValue: $FF1E90FF),
    (ColorName: 'firebrick'; ColorValue: $FFB22222),
    (ColorName: 'floralwhite'; ColorValue: $FFFFFAF0),
    (ColorName: 'forestgreen'; ColorValue: $FF228B22),
    (ColorName: 'fuchsia'; ColorValue: $FFFF00FF),
    (ColorName: 'gainsboro'; ColorValue: $FFDCDCDC),
    (ColorName: 'ghostwhite'; ColorValue: $FFF8F8FF),
    (ColorName: 'gold'; ColorValue: $FFFFD700),
    (ColorName: 'goldenrod'; ColorValue: $FFDAA520),
    (ColorName: 'gray'; ColorValue: $FF808080),
    (ColorName: 'green'; ColorValue: $FF008000),
    (ColorName: 'greenyellow'; ColorValue: $FFADFF2F),
    (ColorName: 'grey'; ColorValue: $FF808080),
    (ColorName: 'honeydew'; ColorValue: $FFF0FFF0),
    (ColorName: 'hotpink'; ColorValue: $FFFF69B4),
    (ColorName: 'indianred'; ColorValue: $FFCD5C5C),
    (ColorName: 'indigo'; ColorValue: $FF4B0082),
    (ColorName: 'ivory'; ColorValue: $FFFFFFF0),
    (ColorName: 'khaki'; ColorValue: $FFF0E68C),
    (ColorName: 'lavender'; ColorValue: $FFE6E6FA),
    (ColorName: 'lavenderblush'; ColorValue: $FFFFF0F5),
    (ColorName: 'lawngreen'; ColorValue: $FF7CFC00),
    (ColorName: 'lemonchiffon'; ColorValue: $FFFFFACD),
    (ColorName: 'lightblue'; ColorValue: $FFADD8E6),
    (ColorName: 'lightcoral'; ColorValue: $FFF08080),
    (ColorName: 'lightcyan'; ColorValue: $FFE0FFFF),
    (ColorName: 'lightgoldenrodyellow'; ColorValue: $FFFAFAD2),
    (ColorName: 'lightgray'; ColorValue: $FFD3D3D3),
    (ColorName: 'lightgreen'; ColorValue: $FF90EE90),
    (ColorName: 'lightpink'; ColorValue: $FFFFB6C1),
    (ColorName: 'lightsalmon'; ColorValue: $FFFFA07A),
    (ColorName: 'lightseagreen'; ColorValue: $FF20B2AA),
    (ColorName: 'lightskyblue'; ColorValue: $FF87CEFA),
    (ColorName: 'lightslategray'; ColorValue: $FF778899),
    (ColorName: 'lightslategrey'; ColorValue: $FF778899),
    (ColorName: 'lightsteelblue'; ColorValue: $FFB0C4DE),
    (ColorName: 'lightyellow'; ColorValue: $FFFFFFE0),
    (ColorName: 'lime'; ColorValue: $FF00FF00),
    (ColorName: 'limegreen'; ColorValue: $FF32CD32),
    (ColorName: 'linen'; ColorValue: $FFFAF0E6),
    (ColorName: 'magenta'; ColorValue: $FFFF00FF),
    (ColorName: 'maroon'; ColorValue: $FF800000),
    (ColorName: 'mediumaquamarine'; ColorValue: $FF66CDAA),
    (ColorName: 'mediumblue'; ColorValue: $FF0000CD),
    (ColorName: 'mediumorchid'; ColorValue: $FFBA55D3),
    (ColorName: 'mediumpurple'; ColorValue: $FF9370DB),
    (ColorName: 'mediumseagreen'; ColorValue: $FF3CB371),
    (ColorName: 'mediumslateblue'; ColorValue: $FF7B68EE),
    (ColorName: 'mediumspringgreen'; ColorValue: $FF00FA9A),
    (ColorName: 'mediumturquoise'; ColorValue: $FF48D1CC),
    (ColorName: 'mediumvioletred'; ColorValue: $FFC71585),
    (ColorName: 'midnightblue'; ColorValue: $FF191970),
    (ColorName: 'mintcream'; ColorValue: $FFF5FFFA),
    (ColorName: 'mistyrose'; ColorValue: $FFFFE4E1),
    (ColorName: 'moccasin'; ColorValue: $FFFFE4B5),
    (ColorName: 'navajowhite'; ColorValue: $FFFFDEAD),
    (ColorName: 'navy'; ColorValue: $FF000080),
    (ColorName: 'none'; ColorValue: $0),
    (ColorName: 'oldlace'; ColorValue: $FFFDF5E6),
    (ColorName: 'olive'; ColorValue: $FF808000),
    (ColorName: 'olivedrab'; ColorValue: $FF6B8E23),
    (ColorName: 'orange'; ColorValue: $FFFFA500),
    (ColorName: 'orangered'; ColorValue: $FFFF4500),
    (ColorName: 'orchid'; ColorValue: $FFDA70D6),
    (ColorName: 'palegoldenrod'; ColorValue: $FFEEE8AA),
    (ColorName: 'palegreen'; ColorValue: $FF98FB98),
    (ColorName: 'paleturquoise'; ColorValue: $FFAFEEEE),
    (ColorName: 'palevioletred'; ColorValue: $FFDB7093),
    (ColorName: 'papayawhip'; ColorValue: $FFFFEFD5),
    (ColorName: 'peachpuff'; ColorValue: $FFFFDAB9),
    (ColorName: 'peru'; ColorValue: $FFCD853F),
    (ColorName: 'pink'; ColorValue: $FFFFC0CB),
    (ColorName: 'plum'; ColorValue: $FFDDA0DD),
    (ColorName: 'powderblue'; ColorValue: $FFB0E0E6),
    (ColorName: 'purple'; ColorValue: $FF800080),
    (ColorName: 'red'; ColorValue: $FFFF0000),
    (ColorName: 'rosybrown'; ColorValue: $FFBC8F8F),
    (ColorName: 'royalblue'; ColorValue: $FF4169E1),
    (ColorName: 'saddlebrown'; ColorValue: $FF8B4513),
    (ColorName: 'salmon'; ColorValue: $FFFA8072),
    (ColorName: 'sandybrown'; ColorValue: $FFF4A460),
    (ColorName: 'seagreen'; ColorValue: $FF2E8B57),
    (ColorName: 'seashell'; ColorValue: $FFFFF5EE),
    (ColorName: 'sienna'; ColorValue: $FFA0522D),
    (ColorName: 'silver'; ColorValue: $FFC0C0C0),
    (ColorName: 'skyblue'; ColorValue: $FF87CEEB),
    (ColorName: 'slateblue'; ColorValue: $FF6A5ACD),
    (ColorName: 'slategray'; ColorValue: $FF708090),
    (ColorName: 'springgreen'; ColorValue: $FF00FF7F),
    (ColorName: 'steelblue'; ColorValue: $FF4682B4),
    (ColorName: 'tan'; ColorValue: $FFD2B48C),
    (ColorName: 'teal'; ColorValue: $FF008080),
    (ColorName: 'thistle'; ColorValue: $FFD8BFD8),
    (ColorName: 'tomato'; ColorValue: $FFFF6347),
    (ColorName: 'turquoise'; ColorValue: $FF40E0D0),
    (ColorName: 'violet'; ColorValue: $FFEE82EE),
    (ColorName: 'wheat'; ColorValue: $FFF5DEB3),
    (ColorName: 'white'; ColorValue: $FFFFFFFF),
    (ColorName: 'whitesmoke'; ColorValue: $FFF5F5F5),
    (ColorName: 'yellow'; ColorValue: $FFFFFF00),
    (ColorName: 'yellowgreen'; ColorValue: $FF9ACD32));

  buffSize = 32;

  defaultDrawInfo: TDrawInfo =
    (fillColor: clBlack32; fillRule: frNonZero; filler: nil;
    strokeColor: clNone32; strokeWidth: 1.0;
    matrix: ((1, 0, 0),(0, 1, 0),(0, 0, 1)); inDefs: false);

  crcTable:  ARRAY[0..255] OF DWORD =
 ($00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
  $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD,
  $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D,
  $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
  $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
  $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
  $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC,
  $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
  $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB,
  $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
  $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
  $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
  $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA,
  $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE,
  $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
  $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
  $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409,
  $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
  $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739,
  $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
  $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
  $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0,
  $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8,
  $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
  $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
  $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703,
  $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7,
  $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
  $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE,
  $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
  $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
  $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
  $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D,
  $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5,
  $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
  $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
  $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

  circleHash              = $D4B76579;
  defsHash                = $21B75A24;
  ellipseHash             = $4C03866A;
  idHash                  = $BF396750;
  lineargradientHash      = $54829198;
  lineHash                = $D114B4F6;
  polygonHash             = $C7A42112;
  polylineHash            = $CF285480;
  rectHash                = $B7D63381;
  useHash                 = $94B1CC4B;

var
  ColorConstList : TStringList;
  AttribFuncList : TStringList;
  LowerCaseTable : array[#0..#255] of AnsiChar;

//------------------------------------------------------------------------------
//Miscellaneous functions ...
//------------------------------------------------------------------------------

function AnsiLowercase(pName: PAnsiChar; len: integer): AnsiString;
var
  i: integer;
begin
  SetLength(Result, len);
  Move(pName^, Result[1], len);
  for i := 1 to len do
    Result[i] := LowerCaseTable[Result[i]];
end;
//--------------------------------------------------------------------------

function CalcCRC32Hash(p: PAnsiChar; length: integer): cardinal; overload;
var
  i: integer;
begin
  result := $FFFFFFFF;
  for i := 0 to length-1 do
  begin
    result := (result shr 8) xor
      crcTable[Ord(p^) xor (result and $000000ff) ];
    inc(p);
  end;
  result := not result;
end;
//--------------------------------------------------------------------------

function CalcCRC32Hash(const s: AnsiString): cardinal; overload;
begin
  Result := CalcCRC32Hash(PAnsiChar(s), Length(s));
end;
//--------------------------------------------------------------------------

function IsAlpha(c: AnsiChar): Boolean;
begin
  if c >= 'a' then dec(c, $20);
  Result := (c >= 'A') and (c <= 'Z');
end;
//------------------------------------------------------------------------------

function GetElemOrAttribNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
var
  startC: PAnsiChar;
const
  validChars =  ['0'..'9','A'..'Z','a'..'z','_',':','-'];
begin
  Result := 0;
  if not IsAlpha(c^) then Exit;
  startC := c; inc(c); 
  while (c < endC) and CharInSet(LowerCaseTable[c^], validChars) do inc(c);
  Result := c - startC;
end;
//------------------------------------------------------------------------------

function GetStyleNameLen(var c: PAnsiChar; endC: PAnsiChar): integer;
var
  startC: PAnsiChar;
const
  validChars =  ['0'..'9','A'..'Z','a'..'z','-'];
begin
  Result := 0;
  if not IsAlpha(c^) then Exit;
  startC := c; inc(c); 
  while (c < endC) and CharInSet(LowerCaseTable[c^], validChars) do inc(c);
  Result := c - startC;
end;
//------------------------------------------------------------------------------

function CompareNames(p: PAnsiChar; pLen: integer;
  p2: PAnsiChar; p2Len: integer): Boolean; overload;
var
  i: integer;
begin
  result := (p2Len = pLen);
  if not result then Exit;
  for i := 0 to pLen -1 do
    if LowerCaseTable[p^] = LowerCaseTable[p2^] then
    begin
      inc(p); inc(p2);
    end else
    begin
      Result := false;
      Exit;
    end;
end;
//------------------------------------------------------------------------------

//avoid AnsiString as much as possible as it really slow things down
function CompareNames(const text: AnsiString;
  p: PAnsiChar; pLen: integer): Boolean; overload;
begin
  result := CompareNames(PAnsiChar(text), Length(text), p, pLen);
end;
//------------------------------------------------------------------------------

function Color32ToHtml(clr: Cardinal): string;
begin
  result := format('#%.6x', [clr and $FFFFFF]);
end;
//------------------------------------------------------------------------------

function TextToHtml(const s: string): string;
var
  i, len: integer;
begin
  result := s;
  len := length(s);
  for i := len downto 1 do
  begin
    case result[i] of
      '&':
        begin
          len := len + 4;
          setlength(result, len);
          move(result[i+1], result[i+5], (len - i - 4) * sizeof(Char));
          move('amp;', result[i+1], 4 * sizeof(Char));
        end;
      '<':
        begin
          len := len + 3;
          setlength(result, len);
          move(result[i+1], result[i+4], (len - i - 3) * sizeof(Char));
          move('&lt;', result[i], 4 * sizeof(Char));
        end;
      '>':
        begin
          len := len + 3;
          setlength(result, len);
          move(result[i+1], result[i+4], (len - i - 3) * sizeof(Char));
          move('&lt;', result[i], 4 * sizeof(Char));
        end;
      '''':
        begin
          len := len + 5;
          setlength(result, len);
          move(result[i+1], result[i+6], (len - i - 5) * sizeof(Char));
          move('&apos;', result[i], 6 * sizeof(Char));
        end;
      '"':
        begin
          len := len + 5;
          setlength(result, len);
          move(result[i+1], result[i+6], (len - i - 5) * sizeof(Char));
          move('&quot;', result[i], 6 * sizeof(Char));
        end;
    end;
  end;
end;
//------------------------------------------------------------------------------

function TrigClamp(val: double): double;
begin
  if val < -1 then Result := -1
  else if val > 1 then Result := 1
  else Result := val;
end;
//------------------------------------------------------------------------------

function  Radian2(vx, vy: double): double;
begin
  Result := ArcCos( TrigClamp(vx / Sqrt( vx * vx + vy * vy)) );
  if( vy < 0.0 ) then Result := -Result;
end;
//------------------------------------------------------------------------------

function  Radian4(ux, uy, vx, vy: double): double;
var
  dp, md: double;
begin
  dp := ux * vx + uy * vy;
  md := Sqrt( ( ux * ux + uy * uy ) * ( vx * vx + vy * vy ) );
    Result := ArcCos( TrigClamp(dp / md) );
    if( ux * vy - uy * vx < 0.0 ) then Result := -Result;
end;
//------------------------------------------------------------------------------

//https://stackoverflow.com/a/12329083
function SvgArc(const p1, p2: TPointD; radii: TPointD;
  phi_rads: double; fA, fS: boolean;
  out startAngle, endAngle: double; out rec: TRectD): Boolean;
var
  x1_, y1_, rxry, rxy1_, ryx1_, s_phi, c_phi: double;
  hd_x, hd_y, hs_x, hs_y, sum_of_sq, lambda, coe: double;
  cx, cy, cx_, cy_, xcr1, xcr2, ycr1, ycr2, deltaAngle: double;
const
  twoPi: double = PI *2;
begin
    Result := false;
    if (radii.X < 0) then radii.X := -radii.X;
    if (radii.Y < 0) then radii.Y := -radii.Y;
    if (radii.X = 0) or (radii.Y = 0) then Exit;

    SinCos(phi_rads, s_phi, c_phi);;
    hd_x := (p1.X - p2.X) / 2.0; // half diff of x
    hd_y := (p1.Y - p2.Y) / 2.0; // half diff of y
    hs_x := (p1.X + p2.X) / 2.0; // half sum of x
    hs_y := (p1.Y + p2.Y) / 2.0; // half sum of y

    // F6.5.1
    x1_ := c_phi * hd_x + s_phi * hd_y;
    y1_ := c_phi * hd_y - s_phi * hd_x;

    // F.6.6 Correction of out-of-range radii
    // Step 3: Ensure radii are large enough
    lambda := (x1_ * x1_) / (radii.X * radii.X) +
      (y1_ * y1_) / (radii.Y * radii.Y);
    if (lambda > 1) then
    begin
      radii.X := radii.X * Sqrt(lambda);
      radii.Y := radii.Y * Sqrt(lambda);
    end;

    rxry := radii.X * radii.Y;
    rxy1_ := radii.X * y1_;
    ryx1_ := radii.Y * x1_;
    sum_of_sq := rxy1_ * rxy1_ + ryx1_ * ryx1_; // sum of square
    if (sum_of_sq = 0) then Exit;

    coe := Sqrt(Abs((rxry * rxry - sum_of_sq) / sum_of_sq));
    if (fA = fS) then coe := -coe;

    // F6.5.2
    cx_ := coe * rxy1_ / radii.Y;
    cy_ := -coe * ryx1_ / radii.X;

    // F6.5.3
    cx := c_phi * cx_ - s_phi * cy_ + hs_x;
    cy := s_phi * cx_ + c_phi * cy_ + hs_y;

    xcr1 := (x1_ - cx_) / radii.X;
    xcr2 := (x1_ + cx_) / radii.X;
    ycr1 := (y1_ - cy_) / radii.Y;
    ycr2 := (y1_ + cy_) / radii.Y;

    // F6.5.5
    startAngle := Radian2(xcr1, ycr1);
    NormalizeAngle(startAngle);

    // F6.5.6
    deltaAngle := Radian4(xcr1, ycr1, -xcr2, -ycr2);
    while (deltaAngle > twoPi) do deltaAngle := deltaAngle - twoPi;
    while (deltaAngle < 0.0) do deltaAngle := deltaAngle + twoPi;
    if not fS then deltaAngle := deltaAngle - twoPi;
    endAngle := startAngle + deltaAngle;
    NormalizeAngle(endAngle);

    rec.Left := cx - radii.X;
    rec.Right := cx + radii.X;
    rec.Top := cy - radii.Y;
    rec.Bottom := cy + radii.Y;

    Result := true;
end;

//------------------------------------------------------------------------------

function AttribToColor32(const attrib: TAttrib; var color: TColor32): Boolean;
var
  i: integer;
  c: TColor32;
  alpha: Byte;
begin
  Result := false;
  if (attrib.valueLen < 3) then Exit;
  alpha := color shr 24;

  if (attrib.value[0] = '#') then
  begin
    if (attrib.valueLen <> 7) then Exit;
    c := $0;
    for i := 1 to 6 do
      if (attrib.value[i] >= 'a') and (attrib.value[i] <= 'f') then
        c := c *16 + 10 + Ord(attrib.value[i]) - Ord('a')
      else if (attrib.value[i] >= 'A') and (attrib.value[i] <= 'F') then
        c := c *16 + 10 + Ord(attrib.value[i]) - Ord('A')
      else if (attrib.value[i] >= '0') and (attrib.value[i] <= '9') then
        c := c *16 + Ord(attrib.value[i]) - Ord('0')
      else Exit;
    c := c or $FF000000;
    color :=  c;
  end else
  begin
    i := ColorConstList.IndexOf(string(attrib.GetValue));
    if i < 0 then Exit;
    color := Cardinal(ColorConstList.Objects[i]);
  end;

  //and just in case the opacity has been set before the color
  if (alpha > 0) and (alpha < 255) then
    color := (color and $FFFFFF) or alpha shl 24;
  Result := true;
end;
//------------------------------------------------------------------------------

function AttribToFloat(const attrib: TAttrib; var value: double): Boolean;
var
  i, decPos: integer;
  isNeg, inExponent: Boolean;
  val: double;
begin
  Result := False;
  if (attrib.valueLen = 0) then Exit;

  decPos := -1;
  inExponent := false;
  isNeg := Ord(attrib.value[0]) = Ord('-');
  if isNeg then i := 1 else i := 0;

  val := 0;
  for i := i to attrib.valueLen -1 do
  begin
    if attrib.value[i] = '.' then
    begin
      if decPos >= 0 then Exit; //error
      decPos := 0;
    end
    else if (LowerCaseTable[attrib.value[i]] = 'e') then
    begin
      if inExponent then Break;
      break;// todo - inExponent := true;
    end
    else if (attrib.value[i] < '0') or (attrib.value[i] > '9') then
      break
    else
    begin
      val := val *10 + Ord(attrib.value[i]) - Ord('0');
      if decPos >= 0 then inc(decPos);
    end;
  end;
  if decPos > 0 then val := Power10(val, -decPos);
  if isNeg then val := -val;

  Result := true;
  value := val;
end;
//------------------------------------------------------------------------------

procedure AttribToOpacity(const attrib: TAttrib; var color: TColor32);
var
  opacity: double;
begin
  if not AttribToFloat(attrib, opacity) then Exit;
  if (opacity < 0) or (opacity > 1) then Exit;
  color := (color and $FFFFFF) or (Round(255 * opacity) shl 24);
end;
//------------------------------------------------------------------------------

function AttribToFillRule(const attrib: TAttrib): TFillRule;
begin
  Result := frNonZero; //default
  if (attrib.valueLen = 0) then Exit;
  if LowerCaseTable[attrib.value[0]] = 'e' then
    Result := frEvenOdd;
end;
//------------------------------------------------------------------------------

function SkipBlanks(var current: PAnsiChar; stop: PAnsiChar): Boolean;
begin
  while (current < stop) and (current^ <= #32) do inc(current);
  Result := (current < stop);
end;
//------------------------------------------------------------------------------

function SkipBlanksIgnoreComma(var current: PAnsiChar; stop: PAnsiChar): Boolean;
begin
  Result := SkipBlanks(current, stop);
  if Result and (current^ = ',') then
  begin
    inc(current);
    Result := SkipBlanks(current, stop);
  end;
end;
//------------------------------------------------------------------------------

function GetChar(var current: PAnsiChar; stop: PAnsiChar; out c: AnsiChar): Boolean;
begin
  Result := SkipBlanks(current, stop);
  if not Result then Exit;
  c := current^;
  inc(current);
end;
//------------------------------------------------------------------------------

function GetWord(var current: PAnsiChar; stop: PAnsiChar; out word: AnsiString): Boolean;
var
  i: integer;
  c: PAnsiChar;
begin
  Result := SkipBlanks(current, stop);
  if not Result then Exit;

  i := 0;
  c := current;
  while (current < stop) and
    (LowerCaseTable[current^] >= 'a') and (LowerCaseTable[current^] <= 'z') do
  begin
    inc(i);
    inc(current);
  end;
  Result := i > 0;
  if not Result then Exit;
  SetLength(word, i);
  Move(c^, word[1], i);
end;
//------------------------------------------------------------------------------

function GetValueLen(var current: PAnsiChar; stop: PAnsiChar): integer;
var
  c: PAnsiChar;
begin
  Result := 0;
  if not SkipBlanks(current, stop) then Exit;
  c := current;
  while (current < stop) and
    CharInSet(current^, ['.','-','#','0'..'9', 'A'..'Z', 'a'..'z']) do 
      inc(current);
  Result := current - c;
end;
//------------------------------------------------------------------------------

function PeekNum(current, stop: PAnsiChar; ignoreComma: Boolean): Boolean;
begin
  Result := false;

  //skip white space +/- single comma
  if ignoreComma then
    while (current < stop) and ((current^ <= ' ') or (current^ = ',')) do
      inc(current);
  while (current < stop) and (current^ <= ' ') do
    inc(current);
  if (current = stop) then Exit;

  if (current^ = '-') then inc(current);
  if (current^ = '.') then inc(current);
  Result := (current < stop) and ((current^ >= '0') and (current^ <= '9'));
end;
//------------------------------------------------------------------------------

function GetNum(var current: PAnsiChar; stop: PAnsiChar;
  skipComma: Boolean; out val: double): Boolean;
var
  loopCnt, decPos: integer;
  isNeg: Boolean;
begin
  Result := false;

  //skip white space +/- single comma
  if skipComma then while (current < stop) and
    ((current^ <= #32) or (current^ = ',')) do inc(current);

  while (current < stop) and (current^ <= ' ') do inc(current);
  if (current = stop) then Exit;

  isNeg := (current^ = '-');
  if isNeg then inc(current);
  if (current^ = '.') then
  begin
    decpos  := 0;
    loopCnt := 2;
    inc(current);
  end else
  begin
    decpos  := -1;
    loopCnt := 0;
  end;

  Result := (current < stop) and (current^ >= '0') and (current^ <= '9');
  if not Result then Exit;

  val := Ord(current^) - Ord('0');
  inc(current);
  while (current < stop) do
  begin
    if (current^ = '.') then
    begin
        if (decpos >= 0) then break;
        decpos := loopCnt;
    end
    else if (val = 0) and (decpos < 0) then break
    else if (current^ < '0') or (current^ > '9') then Break
    else val := val * 10 + Ord(current^) - Ord('0');
    inc(current); inc(loopCnt);
  end;
  if (decpos >= 0) then
  begin
      decpos := loopCnt - decpos -1;
      val := val * power(10, -decpos);
  end;
  if (isNeg) then val := -val;
end;
//------------------------------------------------------------------------------

function FloatToAnsi(val: double): AnsiString;
var
  i, len: integer;
  s: string;
begin
  s := FloatToStr(val);
  len := Length(s);
  i := 2;
  while (i < len) and (s[i] <> FormatSettings.DecimalSeparator) do inc(i);
  if i < len then
  begin
    inc(i);
    while (i <= len) and (s[i] <> '0') do inc(i);
    len := i -1;
  end;
  if s[len] = FormatSettings.DecimalSeparator then dec(len);
  SetLength(Result, len +1);
  for i := 1 to len do
    Result[i] := AnsiChar(s[i]);
  Result[len+1] := #32;
end;
//------------------------------------------------------------------------------

procedure LoadLine(el: TElement);
begin
  with el do
  begin
    fShapeInfo := CreateShapeInfo(TSingleLineInfo, false);
    fShapeInfo.LoadFromOwnerElement;
  end;
end;
//------------------------------------------------------------------------------

procedure LoadCircle(el: TElement);
begin
  with el do
  begin
    fShapeInfo := CreateShapeInfo(TCircleInfo, true);
    fShapeInfo.LoadFromOwnerElement;
  end;
end;
//------------------------------------------------------------------------------

procedure LoadEllipse(el: TElement);
begin
  with el do
  begin
    fShapeInfo := CreateShapeInfo(TEllipseInfo, true);
    fShapeInfo.LoadFromOwnerElement;
  end;
end;
//------------------------------------------------------------------------------

procedure LoadRectangle(el: TElement);
begin
  with el do
  begin
    fShapeInfo := CreateShapeInfo(TRectInfo, true);
    fShapeInfo.LoadFromOwnerElement;
  end;
end;


//------------------------------------------------------------------------------
// TShapeInfo
//------------------------------------------------------------------------------

constructor TShapeInfo.Create(ownerEl: TElement; isClosed: Boolean); 
begin
  fOwnerEl := ownerEl;
  Self.isClosed := isClosed;
end;
//------------------------------------------------------------------------------

procedure TShapeInfo.LoadFromAttrib(const attrib: TAttrib); 
begin
  //overridden by descendants
end;
//------------------------------------------------------------------------------

procedure TShapeInfo.LoadFromOwnerElement; 
begin
  //overridden by descendants
end;
//------------------------------------------------------------------------------

function TShapeInfo.GetDrawInfo: TDrawInfo;
begin
  if Assigned(fOwnerEl) then
    Result := fOwnerEl.fdrawInfo else
    Result := defaultDrawInfo;
end;
//------------------------------------------------------------------------------

function TShapeInfo.GetCount: integer;
begin
  Result := 1;
end;
//------------------------------------------------------------------------------

function TShapeInfo.GetVal(out val: double): Boolean;
begin
  Result := GetNum(readPos, readEnd, true, val);
end;
//------------------------------------------------------------------------------

function TShapeInfo.GetFlattenedPath(index: integer;
  out path: TPathD; out isClosed: Boolean): Boolean;  
begin
  Result := index = 0;
  if not Result then Exit;
  path := CopyPath(self.path);
  isClosed := self.isClosed;
end;


//------------------------------------------------------------------------------
// TDpath
//------------------------------------------------------------------------------

function TDpathInfo.GetSegType(out isRelative: Boolean): Boolean;
var
  c: AnsiChar;
begin
  Result := false;
  if not SkipBlanks(readPos, readEnd) then Exit;
  c := upcase(readPos^);
  result := CharInSet(c, ['A','C','H','M','L','Q','S','T','V','Z']);
  if not result then Exit;
  case c of
    'M': currSegType := dsMove;
    'L': currSegType := dsLine;
    'H': currSegType := dsHorz;
    'V': currSegType := dsVert;
    'A': currSegType := dsArc;
    'Q': currSegType := dsQBez;
    'C': currSegType := dsCBez;
    'T': currSegType := dsQSpline;
    'S': currSegType := dsCSpline;
    'Z': currSegType := dsClose;
  end;
  isRelative := readPos^ >= 'a';
  inc(readPos);
end;
//------------------------------------------------------------------------------

function TDpathInfo.GetSingleDigit(out digit: integer): Boolean;
begin
  Result := SkipBlanksIgnoreComma(readPos, readEnd) and
    (readPos^ >= '0') and (readPos^ <= '9');
  if not Result then Exit;
  digit := Ord(readPos^) - Ord('0');
  inc(readPos);
end;
//------------------------------------------------------------------------------

procedure TDpathInfo.StartNewDpath;
var
  cnt: integer;
begin
  if Assigned(currDpath) then 
  begin
    if not Assigned(currDpath.segs) then Exit;
    SetLength(CurrSeg.vals, currSegCnt); //trim the last segment;
  end;

  cnt := Length(dpaths);
  SetLength(dpaths, cnt +1);

  currDpath := @dpaths[cnt];
  currDpath.firstPt := lastPt;
  currDpath.isClosed := False;  
  currDpath.segs := nil;
  currSeg := nil;
end;
//------------------------------------------------------------------------------

procedure TDpathInfo.StartNewSeg(segType: TDsegType);
var
  cnt: integer;
begin
  if Assigned(currSeg) then 
    SetLength(CurrSeg.vals, currSegCnt)     
  else if not Assigned(currDpath) then
    StartNewDpath;

  currSegType := segType;
  cnt := Length(currDpath.segs);
  SetLength(currDpath.segs, cnt +1);
  currSeg := @currDpath.segs[cnt];
  currSeg.segType := segType;

  currSegCap := buffSize;
  SetLength(currSeg.vals, currSegCap);
  currSegCnt := 0;
end;
//------------------------------------------------------------------------------

procedure TDpathInfo.AddSegValue(val: double);
begin
  if not Assigned(currSeg) then 
    StartNewSeg(currSegType);
  
  if currSegCnt = currSegCap then
  begin
    inc(currSegCap, buffSize);
    SetLength(currSeg.vals, currSegCap);
  end;
  currSeg.vals[currSegCnt] := val;
  inc(currSegCnt);
end;
//------------------------------------------------------------------------------

procedure TDpathInfo.AddSegPoint(const pt: TPointD);
begin
  AddSegValue(pt.X); AddSegValue(pt.Y);
end;
//------------------------------------------------------------------------------

function TDpathInfo.Get2Num(var pt: TPointD; isRelative: Boolean): Boolean;
begin
  Result := GetVal(pt.X) and GetVal(pt.Y);
  if not Result or not isRelative then Exit;
  pt.X := pt.X + lastPt.X;
  pt.Y := pt.Y + lastPt.Y;
end;
//------------------------------------------------------------------------------

procedure TDpathInfo.LoadFromAttrib(const attrib: TAttrib);
var
  i: integer;
  d: double;
  currPt: TPointD;
  isRelative: Boolean;
begin
  readPos := attrib.value;
  readEnd := readPos + attrib.valueLen;

  isRelative := false;
  currPt := NullPointD;

  while GetSegType(isRelative) do
  begin
    lastPt := currPt;

    if (currSegType = dsMove) then
    begin
      if Assigned(currSeg) then
        SetLength(currSeg.vals, currSegCnt); //trim buffer
      currSeg := nil;
      if not Get2Num(currPt, isRelative) then break;
      self.lastPt :=  currPt;

      //values immediately following a Move are implicitly Line statements
      if PeekNum(readPos, readEnd, true) then 
        currSegType := dsLine else
        Continue;
    end
    else if (currSegType = dsClose) then
    begin
      if not Assigned(currSeg) then break;
      SetLength(currSeg.vals, currSegCnt); //trim buffer
      currDpath.isClosed := true;
      currDpath := nil;
      currSeg := nil;
      Continue;
    end;

    if Assigned(currSeg) then
      SetLength(currSeg.vals, currSegCnt); //trim buffer
    currSeg := nil;

    case currSegType of
      dsHorz:
        while PeekNum(readPos, readEnd, true) do
        begin
          GetVal(currPt.X);
          if isRelative then
            currPt.X := currPt.X + lastPt.X;
          AddSegValue(currPt.X);
        end;

      dsVert:
        while PeekNum(readPos, readEnd, true) do
        begin
          GetVal(currPt.Y);
          if isRelative then
            currPt.Y := currPt.Y + lastPt.Y;
          AddSegValue(currPt.Y);
        end;

      dsLine:
        while true do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
          SkipBlanks(readPos, readEnd);
          if PeekNum(readPos, readEnd, true) then Continue;
          if LowerCaseTable[readPos^] = 'l' then GetSegType(isRelative)
          else break;
        end;

      dsQSpline:
        while PeekNum(readPos, readEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCSpline:
        while PeekNum(readPos, readEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsQBez:
        while PeekNum(readPos, readEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsCBez:
        while PeekNum(readPos, readEnd, true) do
        begin
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          if not Get2Num(currPt, isRelative) then break;
          AddSegPoint(currPt);
          lastPt := currPt;
        end;

      dsArc:
        while PeekNum(readPos, readEnd, true) do
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
          lastPt := currPt;
        end;
    end;
  end;
  
  if Assigned(currSeg) then
    SetLength(currSeg.vals, currSegCnt); //trim buffer
end;
//------------------------------------------------------------------------------

function TDpathInfo.GetCount: integer;
begin
  Result := Length(dpaths);
end;
//------------------------------------------------------------------------------

function TDpathInfo.Save: AnsiString;

  function SegTypeToChar(st: TDsegType): AnsiChar;
  begin
    case st of
      dsMove:     Result := 'M';
      dsLine:     Result := 'L';
      dsHorz:     Result := 'H';
      dsVert:     Result := 'V';
      dsArc:      Result := 'A';
      dsQBez:     Result := 'Q';
      dsCBez:     Result := 'C';
      dsQSpline:  Result := 'T';
      dsCSpline:  Result := 'S';
      dsClose:    Result := 'Z';
      else Result := '?';
    end;
  end;

begin
  //todo :)
end;
//------------------------------------------------------------------------------

procedure TDpathInfo.Flatten(index: integer; 
  out path: TPathD; out isClosed: Boolean);
var
  i,j,k, pathLen, pathCap: integer;
  currPt, radii, pt1, pt2, pt3, pt4: TPointD;
  lastQCtrlPt, lastCCtrlPt: TPointD;
  arcFlag, sweepFlag: integer;
  angle, arc1, arc2: double;
  rec: TRectD;
  path2: TPathD;

  procedure AddPoint(const pt: TPointD);
  begin
    if pathLen = pathCap then
    begin
      pathCap := pathCap + buffSize;
      SetLength(path, pathCap);
    end;
    path[pathLen] := pt;
    currPt := pt;
    inc(pathLen);
  end;

  procedure AddPath(const p: TPathD);
  var
    i, pLen: integer;
  begin
    pLen := Length(p);
    if pathLen + pLen >= pathCap then
    begin
      pathCap := pathLen + pLen + buffSize;
      SetLength(path, pathCap);
    end;
    for i := 0 to pLen -1 do
    begin
      path[pathLen] := p[i];
      inc(pathLen);
    end;
    currPt := path[pathLen -1];
  end;

begin
  pathLen := 0; pathCap := 0;
  lastQCtrlPt := InvalidPointD;
  lastCCtrlPt := InvalidPointD;
  isClosed := dpaths[index].isClosed;
  with dpaths[index] do
  begin
    AddPoint(firstPt);
    for i := 0 to High(segs) do
      with segs[i] do
      begin
        case segType of
          dsLine:
            if High(vals) > 0 then
              for j := 0 to High(vals) div 2 do
                AddPoint(PointD(vals[j*2], vals[j*2 +1]));
          dsHorz:
            for j := 0 to High(vals) do
              AddPoint(PointD(vals[j], currPt.Y));
          dsVert:
            for j := 0 to High(vals) do
              AddPoint(PointD(currPt.X, vals[j]));
          dsArc:
            if High(vals) > 5 then
              for j := 0 to High(vals) div 7 do
              begin
                radii.X   := vals[j*7];
                radii.Y   := vals[j*7 +1];
                angle     := DegToRad(vals[j*7 +2]);
                arcFlag   := Round(vals[j*7 +3]);
                sweepFlag := Round(vals[j*7 +4]);
                pt2.X := vals[j*7 +5];
                pt2.Y := vals[j*7 +6];

                SvgArc(currPt, pt2, radii, angle,
                  arcFlag <> 0, sweepFlag <> 0, arc1, arc2, rec);
                if (sweepFlag = 0)  then
                begin
                  path2 := Arc(rec, arc2, arc1);
                  path2 := ReversePath(path2);
                end else
                  path2 := Arc(rec, arc1, arc2);
                path2 := RotatePath(path2, rec.MidPoint, angle);
                AddPath(path2);
              end;
          dsQBez:
            if High(vals) > 2 then
              for j := 0 to High(vals) div 4 do
              begin
                pt2.X := vals[j*4];
                pt2.Y := vals[j*4 +1];
                pt3.X := vals[j*4 +2];
                pt3.Y := vals[j*4 +3];
                lastQCtrlPt := pt2;
                path2 := FlattenQBezier(currPt, pt2, pt3);
                AddPath(path2);
              end;
          dsQSpline:
            if High(vals) > 0 then
              for j := 0 to High(vals) div 2 do
              begin
                if IsValid(lastQCtrlPt) then
                  pt2 := ReflectPoint(lastQCtrlPt, currPt) else
                  pt2 := currPt;
                pt3.X := vals[j*2];
                pt3.Y := vals[j*2 +1];
                lastQCtrlPt := pt2;
                path2 := FlattenQBezier(currPt, pt2, pt3);
                AddPath(path2);
              end;
          dsCBez:
            if High(vals) > 4 then
              for j := 0 to High(vals) div 6 do
              begin
                pt2.X := vals[j*6];
                pt2.Y := vals[j*6 +1];
                pt3.X := vals[j*6 +2];
                pt3.Y := vals[j*6 +3];
                pt4.X := vals[j*6 +4];
                pt4.Y := vals[j*6 +5];
                lastCCtrlPt := pt3;
                path2 := FlattenCBezier(currPt, pt2, pt3, pt4);
                AddPath(path2);
              end;
          dsCSpline:
            if High(vals) > 2 then
              for j := 0 to High(vals) div 4 do
              begin
                if IsValid(lastCCtrlPt) then
                  pt2 := ReflectPoint(lastCCtrlPt, currPt) else
                  pt2 := currPt;
                pt3.X := vals[j*2];
                pt3.Y := vals[j*2 +1];
                pt4.X := vals[j*2 +2];
                pt4.Y := vals[j*2 +3];
                lastCCtrlPt := pt3;
                path2 := FlattenCBezier(currPt, pt2, pt3, pt4);
                AddPath(path2);
              end;
        end;
      end;
  end;
  SetLength(path, pathLen);
end;
//------------------------------------------------------------------------------

function TDpathInfo.GetFlattenedPath(index: integer;
  out path: TPathD; out isClosed: Boolean): Boolean;
begin
  Result := (index >= 0) and (index < Length(dpaths));
  if not Result then Exit;
  Flatten(index, path, isClosed);
end;
//------------------------------------------------------------------------------

procedure TPolyInfo.AddPoint(const pt: TPointD);
begin
  if currCnt = currCap then
  begin
    currCap := currCap + buffSize;
    SetLength(path, currCap);
  end;
  path[currCnt] := pt;
  inc(currCnt);
end;
//------------------------------------------------------------------------------

procedure TPolyInfo.LoadFromAttrib(const attrib: TAttrib); 
var
  pt: TPointD;
begin
  readPos := attrib.value;
  readEnd := readPos + attrib.valueLen;
  while GetVal(pt.X) and GetVal(pt.Y) do AddPoint(pt);
  SetLength(path, currCnt);
end;

//------------------------------------------------------------------------------
// TSingleLineInfo
//------------------------------------------------------------------------------

procedure TSingleLineInfo.LoadFromOwnerElement;
var
  attrib: PAttrib;
begin
  SetLength(path, 2);
  path[0] := NullPointD; path[1] := NullPointD;
  
  with fOwnerEl do
  begin
    attrib := GetAttribute('x1');
    if Assigned(attrib) then 
      with attrib^ do
        GetNum(value, value + valueLen, false, path[0].X);
    attrib := GetAttribute('y1');
    if Assigned(attrib) then 
      with attrib^ do
        GetNum(value, value + valueLen, false, path[0].Y);
    attrib := GetAttribute('x2');
    if Assigned(attrib) then
      with attrib^ do
        GetNum(value, value + valueLen, false, path[1].X);
    attrib := GetAttribute('y2');
    if Assigned(attrib) then
      with attrib^ do
        GetNum(value, value + valueLen, false, path[1].Y);
  end;
end;

//------------------------------------------------------------------------------
// TCircleInfo
//------------------------------------------------------------------------------

procedure TCircleInfo.LoadFromOwnerElement; 
var
  centerPt: TPointD;
  radius: double;
  attrib: PAttrib;
begin
  centerPt := NullPointD;
  
  with fOwnerEl do
  begin
    attrib := GetAttribute('cx');
    if Assigned(attrib) then 
      with attrib^ do
        GetNum(value, value + valueLen, false, centerPt.X);
    attrib := GetAttribute('cy');
    if Assigned(attrib) then
      with attrib^ do
        GetNum(value, value + valueLen, false, centerPt.Y);
    attrib := GetAttribute('r');
    if not Assigned(attrib) then Exit;
    with attrib^ do
      GetNum(value, value + valueLen, false, radius);
  end;

  with centerPt do
    path := Ellipse(RectD(X -radius, Y -radius, X +radius, Y +radius));
  
end;

//------------------------------------------------------------------------------
// TEllipseInfo
//------------------------------------------------------------------------------

procedure TEllipseInfo.LoadFromOwnerElement; 
var
  centerPt, radii: TPointD;
  attrib: PAttrib;
begin
  centerPt := NullPointD;
  
  with fOwnerEl do
  begin
    attrib := GetAttribute('cx');
    if Assigned(attrib) then 
      with attrib^ do
        GetNum(value, value + valueLen, false, centerPt.X);
    attrib := GetAttribute('cy');
    if Assigned(attrib) then
      with attrib^ do
        GetNum(value, value + valueLen, false, centerPt.Y);

    attrib := GetAttribute('rx');
    if not Assigned(attrib) then Exit;
    with attrib^ do
      GetNum(value, value + valueLen, false, radii.X);

    attrib := GetAttribute('ry');
    if not Assigned(attrib) then Exit;
    with attrib^ do
      GetNum(value, value + valueLen, false, radii.Y);
  end;

  with centerPt do
    path := Ellipse(RectD(X -radii.X, Y -radii.Y, X +radii.X, Y +radii.Y));
end;

//------------------------------------------------------------------------------
// TRectInfo
//------------------------------------------------------------------------------

procedure TRectInfo.LoadFromOwnerElement; 
var
  rec: TRectD;
  radii: TPointD;
  attrib: PAttrib;
begin
  rec := NullRectD;
  radii := NullPointD;
  
  with fOwnerEl do
  begin
    attrib := GetAttribute('x');
    if Assigned(attrib) then 
      with attrib^ do
        GetNum(value, value + valueLen, false, rec.Left);
    attrib := GetAttribute('y');
    if Assigned(attrib) then
      with attrib^ do
        GetNum(value, value + valueLen, false, rec.Top);

    attrib := GetAttribute('width');
    if not Assigned(attrib) then Exit;
    with attrib^ do
      GetNum(value, value + valueLen, false, rec.Right);

    attrib := GetAttribute('height');
    if not Assigned(attrib) then Exit;
    with attrib^ do
      GetNum(value, value + valueLen, false, rec.Bottom);

    rec.Right := rec.Right + rec.Left;
    rec.Bottom := rec.Bottom + rec.Top;

    attrib := GetAttribute('rx');
    if Assigned(attrib) then
      with attrib^ do
        GetNum(value, value + valueLen, false, radii.X);
    attrib := GetAttribute('ry');
    if Assigned(attrib) then
      with attrib^ do
        GetNum(value, value + valueLen, false, radii.Y);
  end;
  if (radii.X <> 0) or (radii.Y <> 0) then
    path := RoundRect(rec, Average(radii.X, radii.Y)) else
    path := Rectangle(rec);
end;

//------------------------------------------------------------------------------
// TAttrib
//------------------------------------------------------------------------------

function TAttrib.GetName: AnsiString;
var
  i: integer;
  c: PAnsiChar;
begin
  SetLength(Result, nameLen);
  c := aname;
  for i := 1 to nameLen do
  begin
    Result[i] := LowerCaseTable[c^]; inc(c);
  end;
end;
//------------------------------------------------------------------------------

function TAttrib.GetValue: AnsiString;
var
  i: integer;
  c: PAnsiChar;
begin
  SetLength(Result, valueLen);
  c := value;
  for i := 1 to valueLen do
  begin
    Result[i] := LowerCaseTable[c^]; inc(c);
  end;
end;

//------------------------------------------------------------------------------
// TElement
//------------------------------------------------------------------------------

constructor TElement.Create(parent: TElement; var c: PAnsiChar);
var
  hasContent: Boolean;
  nameHash: Cardinal;
begin
  fChilds := TList.Create;
  if not Assigned(parent) then Exit; //svg root element

  self.fParent    := parent;
  self.fReader    := parent.fReader;
  self.fSvgEnd    := fReader.fEndStream;
  self.fDrawInfo  := parent.fDrawInfo;

  if c^ = '>' then inc(c);
  while (c < fSvgEnd) and (c^ <= #32) do inc(c);

  if not LoadElemOrAttribName(c, fName, fNameLen) then Exit;
  FDrawInfo.inDefs := 
    FDrawInfo.inDefs or (CalcCRC32Hash(fName, fNameLen) = defsHash);

  if not LoadAttributes(c, hasContent) then Exit;

  nameHash := CalcCRC32Hash(fName, fNameLen);
  case nameHash of
    useHash     : LoadUse;
    lineHash    : LoadLine(self);
    circleHash  : LoadCircle(self);
    ellipseHash : LoadEllipse(self);
    rectHash    : LoadRectangle(self);
  end;

  if not FDrawInfo.inDefs and Assigned(fShapeInfo) then
    fReader.fShapesList.Add(fShapeInfo);

  if hasContent then LoadContent(c);

end;
//------------------------------------------------------------------------------

destructor  TElement.Destroy;
var
  i: integer;
begin
  for i := 0 to fChilds.Count -1 do
    TElement(fChilds[i]).Free;
  fChilds.Free;
  FreeAndNil(fShapeInfo);
  inherited;
end;
//------------------------------------------------------------------------------

function TElement.CreateShapeInfo(shapeClass: TShapeInfoClass; isClosed: Boolean): TShapeInfo;
begin
  if not Assigned(fShapeInfo) then  
    fShapeInfo := shapeClass.Create(self, isClosed);
  Result := fShapeInfo;
end;
//------------------------------------------------------------------------------

function TElement.GetChildCount: integer;
begin
  Result := fChilds.Count;
end;
//------------------------------------------------------------------------------

function TElement.GetChild(index: integer): TElement;
begin
  if (index < 0) or (index >= fChilds.Count) then
    raise Exception.Create('TElement: child index out of bounds');
  Result := TElement(fChilds[index]);
end;
//------------------------------------------------------------------------------

function TElement.LoadElemOrAttribName(var c: PAnsiChar;
  out name: PAnsiChar; out nameLen: integer): Boolean;
begin
  Result := IsAlpha(c^);
  if not Result then Exit;
  name := c; nameLen := GetElemOrAttribNameLen(c, fSvgEnd);
end;
//------------------------------------------------------------------------------

function TElement.LoadAttribValue(var c: PAnsiChar;
  out value: PAnsiChar; out valueLen: integer): Boolean;
begin
  IsAlpha(c^);
  Result := c^ = '"';
  if not Result then Exit;
  inc(c);
  value := c;
  while (c < fSvgEnd) and (c^ <> '"')  do inc(c);
  valueLen := c - value;
end;
//------------------------------------------------------------------------------

function TElement.LoadAttributes(var c: PAnsiChar; out hasContent: Boolean): Boolean;
var
  attribCnt: integer;
  attrib: PAttrib;
begin
  Result := false;
  hasContent := true;
  attribCnt := 0;
  while SkipBlanks(c, fSvgEnd) do
  begin

    if (c^ = '/') or (c^ = '>') then
    begin
      hasContent := (c^ <> '/');
      if hasContent then
        inc(c) else
        inc(c, 2);
      Result := true;
      fIsValid := true;
      Exit;
    end;

    inc(attribCnt);
    setLength(fAttribs, attribCnt);
    attrib := @fAttribs[attribCnt-1];
    attrib.ownerEl := self;
    
    LoadElemOrAttribName(c, attrib.aname, attrib.nameLen);
    if c^ <> '=' then Exit; inc(c);
    LoadAttribValue(c, attrib.value, attrib.valueLen);
    if c^ <> '"' then Exit; inc(c);
    fIsValid := true;
    ProcessAttrib(attrib^);
  end;
end;
//------------------------------------------------------------------------------

procedure TElement.ProcessAttrib(const attrib: TAttrib);
var
  i: integer;
  hash: Cardinal;
begin
  hash := CalcCRC32Hash(attrib.aname, attrib.nameLen);
  i := AttribFuncList.IndexOf(inttohex(hash, 8));
  if i >= 0 then
    TAttribFunc(AttribFuncList.Objects[i])(attrib);
end;
//------------------------------------------------------------------------------

procedure TElement.LoadUse;
var
  i: integer;
  oldAttribs: TArrayOfAttrib;
  attrib: PAttrib;
  idElem: TElement;
  n, ref: AnsiString;

  function NewAttrib: PAttrib;
  var
    len: integer;
  begin
    len := length(fAttribs);
    setLength(fAttribs, len +1);
    Result := @fAttribs[len];
    Result.ownerEl := self;
  end;

begin
  attrib := GetAttribute('href');
  if not assigned(attrib) then
    attrib := GetAttribute('xlink:href');
  if not assigned(attrib) or (attrib.value[0] <> '#') then Exit;
  setLength(ref, attrib.valueLen);
  ref := AnsiLowercase(attrib.value, attrib.valueLen);
  i := fReader.fIdList.IndexOf(string(ref));
  if i < 0 then Exit;
  idElem := TElement(fReader.fIdList.Objects[i]);
  fName := idElem.fName;
  fNameLen := idElem.fNameLen;

  oldAttribs := fAttribs;
  fAttribs := nil;
  //copy the attributes from idElem and also change their ownership 
  fAttribs := Copy(idElem.fAttribs, 0, Length(idElem.fAttribs));  
  for i := 0 to High(fAttribs) do fAttribs[i].ownerEl := self;

  //append new attributes noting that only x, y, width & height are
  //allowed to *replace* referenced attribs.
  for i := 0 to High(oldAttribs) do
  begin
    n := oldAttribs[i].GetName;
    if (n = 'x') or (n = 'y') or (n = 'width') or (n = 'height') or
      not Assigned(GetAttribute(n)) then
      begin
        attrib := NewAttrib;
        attrib.aname := oldAttribs[i].aname;
        attrib.nameLen := oldAttribs[i].nameLen;
        attrib.value := oldAttribs[i].value;
        attrib.valueLen := oldAttribs[i].valueLen;
      end;
  end;

  //finally reprocess each attribute (except the ID attribute)
  for i := 0 to High(fAttribs) do
    with fAttribs[i] do
      if not (CalcCRC32Hash(aname, nameLen) = idHash) then
        ProcessAttrib(fAttribs[i]);
end;
//------------------------------------------------------------------------------

procedure TElement.LoadContent(var c: PAnsiChar);
var
  el: TElement;
  n: PAnsiChar;
  nLen: integer;
begin
  while SkipBlanks(c, fSvgEnd) do
  begin
    if (c^ = '<') then
    begin
      inc(c);
      if c^ = '!' then
      begin
        //a comment section
        while (c < fSvgEnd) do
        begin
          if (c^ = '>') and ((c -1)^ = '-') and  ((c -2)^ = '-') then
          begin
            inc(c);
            break;
          end;
          inc(c);
        end;
      end
      else if c^ = '/' then
      begin
        //this should be the element close
        inc(c);
        LoadElemOrAttribName(c, n, nLen);
        fIsValid := CompareNames(fName, fNameLen, n, nLen);
        if fIsValid then inc(c);
        Exit;
      end else
      begin
        //a child element
        el := TElement.Create(self, c);
        if not el.fIsValid then break;
        fChilds.Add(el);
      end;
    end
    else if c^ = '>' then
    begin
      //oops! something's wrong
      inc(c);
      fIsValid := false;
      break;
    end else
    begin
      //skip unknown (probably text) content
      while (c < fSvgEnd) and (c^ <> '<') do inc(c);
    end;
  end;
end;
//------------------------------------------------------------------------------

function TElement.GetAttribute(const attribName: AnsiString): PAttrib;
var
  i, nLen: integer;
begin
  Result := nil;
  nLen := Length(attribName);
  for i := 0 to High(fAttribs) do
    with fAttribs[i] do
      if (nLen = nameLen) and CompareNames(attribName, aname, nLen) then
    begin
      Result := @fAttribs[i];
      Break;
    end;
end;

//------------------------------------------------------------------------------
// TSvgReader
//------------------------------------------------------------------------------

constructor TSvgReader.Create;
begin
  fShapesList     := TList.Create;
  fMemStream      := TMemoryStream.Create;
  fIdList         := TStringList.Create;
  fIdList.Sorted  := True;
  fGradientsList  := TStringList.Create;
end;
//------------------------------------------------------------------------------

destructor TSvgReader.Destroy;
begin
  Clear;
  fMemStream.Free;
  fShapesList.Free;
  fIdList.Free;
  fGradientsList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.Clear;
begin
  FreeAndNil(fRootElement);
  fShapesList.Clear;
  fIdList.Clear;
  fViewbox := NullRectD;
  fEndStream := nil;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.DrawImage(img: TImage32; scaleToImage: Boolean);
var
  i,j: integer;
  rec: TRect;
  sx, sy: double;
  p: TPathD;
  closedPP, openPP: TPathsD;
const
  endStyle: array[boolean] of TEndStyle = (esPolygon, esRound);
begin
  if not Assigned(fRootElement) or fViewbox.IsEmpty then Exit;

  rec := Rect(fViewbox);

  if scaleToImage and not img.IsEmpty then
  begin
    sx := img.width / fViewbox.Width;
    sy := img.height / fViewbox.Height;
    if sy <= sx then sx := sy;
  end else
  begin
    img.SetSize(RectWidth(rec), RectHeight(rec));
    sx := 1;
  end;

  for i := 0 to fShapesList.Count -1 do
    with TShapeInfo(fShapesList[i]) do
    begin
      closedPP := nil;
      openPP := nil;
      
      for j := 0 to Count -1 do
      begin
        GetFlattenedPath(j, p, isClosed);
        if isClosed then
          AppendPath(closedPP, p) else
          AppendPath(openPP, p);
      end;
      
      //nb: apply the matrix before offsetting
      if not IsIdentityMatrix(DrawInfo.matrix) then
      begin
        MatrixApply(DrawInfo.matrix, openPP);
        MatrixApply(DrawInfo.matrix, closedPP);
      end;

      if Assigned(closedPP) then
      begin
        closedPP := OffsetPath(closedPP, -fViewbox.Left, -fViewbox.Top);
        if sx <> 1.0 then        
          closedPP := ScalePath(closedPP, sx);
        if (DrawInfo.fillColor <> clNone32) then
          DrawPolygon(img, closedPP, DrawInfo.fillRule, DrawInfo.fillColor);

        if (DrawInfo.strokeColor <> clNone32) and
          (DrawInfo.strokeWidth <> 0) then
            DrawLine(img, closedPP, DrawInfo.strokeWidth * sx, 
              DrawInfo.strokeColor, esPolygon);
      end;

      if Assigned(openPP) and 
        (DrawInfo.strokeColor <> clNone32) and 
        (DrawInfo.strokeWidth <> 0.0) then
      begin
        openPP := OffsetPath(openPP, -fViewbox.Left, -fViewbox.Top);
        if sx <> 1.0 then        
          openPP := ScalePath(openPP, sx);
        DrawLine(img, openPP, DrawInfo.strokeWidth * sx, 
          DrawInfo.strokeColor, esRound);
      end;      
    end;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromFile(const filename: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
//------------------------------------------------------------------------------

function TSvgReader.LoadFromStream(stream: TStream): Boolean;
var
  svgStart, dummy: PAnsiChar;
  hasContent: Boolean;
begin
  Result := false;
  Clear;
  if not Assigned(stream) then Exit;

  fMemStream.LoadFromStream(stream);
  fEndStream := PAnsiChar(fMemStream.Memory) + fMemStream.Size;

  fRootElement := TElement.Create(nil, dummy);
  fRootElement.fReader := self;
  fRootElement.fSvgEnd := self.fEndStream;
  fRootElement.fdrawInfo :=  defaultDrawInfo;

  if not GetSvgStart(svgStart) then Exit;
  fRootElement.fName := svgStart;
  fRootElement.fNameLen := 3;
  inc(svgStart, 3);
  if not fRootElement.LoadAttributes(svgStart, hasContent) or
    not hasContent then Exit;
  GetViewBox;

  fRootElement.LoadContent(svgStart);
  Result := fRootElement.fIsValid;
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
    if (svgStart^ = '<') then
    begin
      inc(svgStart);
      if (LowerCaseTable[svgStart^] <> 's') or
        (LowerCaseTable[(svgStart +1)^] <> 'v') or
        (LowerCaseTable[(svgStart +2)^] <> 'g') then Continue;
      Result := true;
      Exit;
    end;
    inc(svgStart);
  end;
end;
//------------------------------------------------------------------------------

procedure TSvgReader.GetViewBox;
var
  attrib: PAttrib;
  c, stop: PAnsiChar;
begin
  if not Assigned(fRootElement) then Exit;
  fViewbox := NullRectD;
  attrib := fRootElement.GetAttribute('viewbox');
  if Assigned(attrib) then
  begin
    c := PAnsiChar(attrib.value);
    if GetNum(c, fEndStream, false, fViewbox.Left) and
      GetNum(c, fEndStream, true, fViewbox.Top) and
      GetNum(c, fEndStream, true, fViewbox.Right) and
      GetNum(c, fEndStream, true, fViewbox.Bottom) then
    begin
      fViewbox.Right := fViewbox.Right + fViewbox.Left;
      fViewbox.Bottom := fViewbox.Bottom + fViewbox.Top;
    end;
  end;
  if not fViewbox.IsEmpty then Exit;
  attrib := fRootElement.GetAttribute('width');
  if not Assigned(attrib) then Exit;
  c := attrib.value;
  stop := c + attrib.valueLen;
  if not GetNum(c, stop, false, fViewbox.Right) then Exit;
  attrib := fRootElement.GetAttribute('height');
  if not Assigned(attrib) then Exit;
  c := attrib.value;
  stop := c + attrib.valueLen;
  GetNum(c, stop, false, fViewbox.Bottom);
end;
//------------------------------------------------------------------------------

procedure MakeLowerCaseTable;
var
  i: AnsiChar;
begin
  for i:= #0 to #$40 do LowerCaseTable[i]:= i;
  for i:= #$41 to #$5A do LowerCaseTable[i]:= AnsiChar(Ord(i) + $20);
  for i:= #$5B to #$FF do LowerCaseTable[i]:= i;
end;
//------------------------------------------------------------------------------

procedure RegisterAttribute(const attribName: AnsiString; func: TAttribFunc);
var
  hash: Cardinal;
begin
  hash := CalcCRC32Hash(PAnsiChar(attribName), Length(attribName));
  AttribFuncList.AddObject(InttoHex(hash, 8), @func) ;
end;
//------------------------------------------------------------------------------

procedure Id(const attrib: TAttrib); 
var
  id: AnsiString;
begin
  with attrib do
  begin
    id := AnsiLowercase(value, valueLen);
    ownerEl.fReader.fIdList.AddObject('#' + string(id), ownerEl);
  end;
end;
//------------------------------------------------------------------------------

procedure D(const attrib: TAttrib);
begin
  with attrib.ownerEl do
  begin
    fShapeInfo := CreateShapeInfo(TDpathInfo, false);
    fShapeInfo.LoadFromAttrib(attrib);
  end;
end;
//------------------------------------------------------------------------------

procedure Fill(const attrib: TAttrib);
begin
  AttribToColor32(attrib, attrib.ownerEl.fDrawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure FillOpacity(const attrib: TAttrib);
begin
  AttribToOpacity(attrib, attrib.ownerEl.fDrawInfo.fillColor);
end;
//------------------------------------------------------------------------------

procedure Points(const attrib: TAttrib);
var
  isClosed: Boolean;
begin
  with attrib.ownerEl do
  begin
    //owner element name: poly'G'on vs poly'L'ine ... 
    isClosed := LowerCaseTable[fName[4]] = 'g'; 

    fShapeInfo := CreateShapeInfo(TPolyInfo, isClosed);
    fShapeInfo.LoadFromAttrib(attrib);
  end;
end;
//------------------------------------------------------------------------------

procedure Stroke(const attrib: TAttrib);
begin
  AttribToColor32(attrib, attrib.ownerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeOpacity(const attrib: TAttrib);
begin
  AttribToOpacity(attrib, attrib.ownerEl.fDrawInfo.strokeColor);
end;
//------------------------------------------------------------------------------

procedure StrokeWidth(const attrib: TAttrib);
begin
    AttribToFloat(attrib, attrib.ownerEl.fDrawInfo.strokeWidth);
end;
//------------------------------------------------------------------------------

procedure FillRule(const attrib: TAttrib);
begin
  if LowerCaseTable[attrib.value[0]] = 'e' then
    attrib.ownerEl.fDrawInfo.fillRule := frEvenOdd else
    attrib.ownerEl.fDrawInfo.fillRule := frNonZero;
end;
//------------------------------------------------------------------------------

procedure Style(const attrib: TAttrib);
var
  attribCnt, nameLen: integer;
  newAt: PAttrib;
  element: TElement;
  current, endC, name: PAnsiChar;
  c: AnsiChar;
begin
  element := attrib.ownerEl;
  attribCnt := Length(element.fAttribs);
  current := attrib.value;
  endC := current + attrib.valueLen;
  
  while SkipBlanks(current, endC) do
  begin
    name := current; 
    nameLen := GetStyleNameLen(current, endC);
    if nameLen = 0 then Break;
    
    inc(attribCnt);
    SetLength(element.fAttribs, attribCnt);
    newAt := @element.fAttribs[attribCnt-1];
    newAt.ownerEl := element;
    
    newAt.aname := name;
    newAt.nameLen := nameLen;
    
    if not GetChar(current, endC, c) or (c <> ':') or  //syntax check
      not SkipBlanks(current,endC) then Break;
    newAt.value := current;
    newAt.valueLen := GetValueLen(current, endC);
    if newAt.valueLen = 0 then Break;
    element.ProcessAttrib(newAt^);
    if not GetChar(current, endC, c) or (c <> ';') then Break;  //syntax check
  end;
end;
//------------------------------------------------------------------------------

procedure Transform(const attrib: TAttrib);
var
  i: integer;
  word: AnsiString;
  values: array[0..5] of double;
  current, endC: PAnsiChar;
  c: AnsiChar;
  mat: PMatrixD;
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
  //surprisingly, and I think this is a bug in SVG not a feature ...
  //transform operations must be performed in reverse order
  current := attrib.value;
  endC := current + attrib.valueLen;

  while GetWord(current, endC, word) do
  begin
    if Length(word) < 5 then Exit;
    word[5] := LowerCaseTable[word[5]];
    if not GetChar(current, endC, c) or (c <> '(') then Exit; //syntax check
    //reset values variables
    for i := 0 to High(values) do values[i] := InvalidD;
    //and since every transform function requires at least one value
    if not GetNum(current, endC, false, values[0]) then Exit;
    //now get additional variables
    i := 1;
    while (i < 6) and PeekNum(current, endC, true) do
    begin
      GetNum(current, endC, true, values[i]);
      inc(i);
    end;
    if not GetChar(current, endC, c) or (c <> ')') then Exit; //syntax check

    mat := NewMatrix;
    case word[5] of
      'e' : //scalE
        if not IsValid(values[1]) then
          MatrixScale(mat^, values[0]) else
            MatrixScale(mat^, values[0], values[1]);
      'i' : //matrIx
        if IsValid(values[5]) then
        begin
          mat[0,0] :=  values[0];
          mat[0,1] :=  values[1];
          mat[1,0] :=  values[2];
          mat[1,1] :=  values[3];
          mat[2,0] :=  values[4];
          mat[2,1] :=  values[5];
        end;
      's' : //tranSlateX, tranSlateY & tranSlate
        if Length(word) =10  then
        begin
          if LowerCaseTable[word[10]] = 'x' then
            MatrixTranslate(mat^, values[0], 0)
          else if LowerCaseTable[word[10]] = 'y' then
            MatrixTranslate(mat^, 0, values[0]);
        end
        else if IsValid(values[1]) then
          MatrixTranslate(mat^, values[0], values[1])
        else
          MatrixTranslate(mat^, values[0], 0);
      't' : //rotaTe
        if IsValid(values[2]) then
          MatrixRotate(mat^, PointD(values[1],values[2]), DegToRad(values[0]))
        else
          MatrixRotate(mat^, NullPointD, DegToRad(values[0]));
       'x' : //skewX
         begin
            MatrixSkew(mat^, DegToRad(values[0]), 0);
         end;
       'y' : //skewY
         begin
            MatrixSkew(mat^, 0, DegToRad(values[0]));
         end;
    end;
  end;
  with attrib.ownerEl do
    for i := High(mats) downto 0 do
      fDrawInfo.matrix := MatrixMultiply(mats[i], fDrawInfo.matrix);
end;
//------------------------------------------------------------------------------

procedure MakeColorConstList;
var
  i: integer;
begin
  ColorConstList := TStringList.Create;
  ColorConstList.Capacity := Length(ColorConsts);
  for i := 0 to High(ColorConsts) do
    with ColorConsts[i] do
      ColorConstList.AddObject(ColorName, Pointer(ColorValue));
  ColorConstList.Sorted := true;
end;
//------------------------------------------------------------------------------

procedure MakeAttribFuncList;
begin
  AttribFuncList := TStringList.Create;
  AttribFuncList.Duplicates := dupError;

  RegisterAttribute('d',              D);
  RegisterAttribute('fill',           Fill);
  RegisterAttribute('fill-opacity',   FillOpacity);
  RegisterAttribute('fill-fule',      FillRule);
  RegisterAttribute('id',             Id);
  RegisterAttribute('points',         Points);
  RegisterAttribute('stroke',         Stroke);
  RegisterAttribute('stroke-opacity', StrokeOpacity);
  RegisterAttribute('stroke-width',   StrokeWidth);
  RegisterAttribute('style',          Style);
  RegisterAttribute('transform',      Transform);
  AttribFuncList.Sorted := true;
end;
//------------------------------------------------------------------------------

procedure MakeHashesConsts;
var
  sl: TStringList;

  procedure AddConst(const name: AnsiString);
  begin
    sl.Add(Format('%-24s= $%8.8x;',[name+'Hash', CalcCRC32Hash(name)]));
  end;

begin
  sl := TStringList.Create;
  try
    AddConst('id');
    AddConst('defs');
    AddConst('use');
    AddConst('lineargradient');
    AddConst('line');
    AddConst('circle');
    AddConst('ellipse');
    AddConst('rect');
    AddConst('polygon');
    AddConst('polyline');

    sl.Duplicates := dupError;
    sl.Sorted := true;
    sl.SaveToFile('hashConstList.txt');
  finally
    sl.Free;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  //MakeHashesConsts;
  MakeColorConstList;
  MakeLowerCaseTable;
  MakeAttribFuncList;

finalization
  AttribFuncList.Free;
  ColorConstList.Free;
end.
