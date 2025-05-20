unit Img32.Ctrls;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  0.0 (Experimental)                                              *
* Date      :  12 April 2025                                                   *
* Website   :  https://www.angusj.com                                          *
* Copyright :  Angus Johnson 2019-2025                                         *
*                                                                              *
* Purpose   :  Drawing controls (buttons, labels, edits, tabs, pages etc.)     *
*              This unit is EXPERIMENTAL. To do this properly would be a huge  *
*              task and I doubt there will be sufficient interest to justify   *
*              the effort since I'm largely reinventing the wheel (ie FMX).    *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types, TypInfo, Character,
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} // required for clipboard
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.Storage, Img32.Vector, Img32.Text, Img32.TextChunks,
  Img32.Layers, Img32.Fmt.SVG;

type
  TTextPosition       = (tpLeft, tpTop, tpRight, tpBottom);
  TTextPositionH      = (tphLeft, tphRight);
  TScrollOrientation  = (soUnknown, soHorizontal, soVertical);
  TAutoPosition       = (apNone, apCustom, apClient,
                          apLeft, apTop, apRight, apBottom);

  TMouseButton        = (mbNone, mbLeft, mbRight, mbMiddle);
  TShiftState         = Cardinal;

  TShortcut = record
    chr     : Char;
    flags   : TShiftState;
    chrIdx  : integer;
  end;

  TNameAndAddress = Record
    name: string;
    address: Pointer;
  end;
  TArrayOfNameAndAddress = array of TNameAndAddress;

  TLoadRec = record
    loadObj   : TStorage;
    propName  : string;
    targetId  : integer;
  end;
  TLoadRecArray = array of TLoadRec;

  TCtrlTheme = record
    Color: TColor32;
    AltColor: TColor32;
    FocusColor: TColor32;
    FontColor: TColor32;
    SoftHighlight: TColor32;
  end;

  TEventPropertyHandler = class(TStorage)
  private
    fNotifyEvents : TArrayOfNameAndAddress;
    fObjectProps  : TArrayOfNameAndAddress;
    fAutoFree     : Boolean;
    procedure GetNotifyEvents;
    function GetEventCount: integer;
    function GetPropCount: integer;
    function GetNotify(const name: string): TNotifyEvent;
    function GetPropObj(const name: string): TObject;
  public
    constructor Create(parent:  TStorage = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure RegisterProperties;
    procedure DeRegisterProperties;
    procedure Scale(delta: double); virtual;
    function  GetEventName(event: TNotifyEvent): string;
    function  GetPropName(prop: TObject): string;
    property  EventCount: integer read GetEventCount;
    property  PropCount: integer read GetPropCount;
    property  Event[const name: string]: TNotifyEvent read GetNotify;
    property  Prop[const name: string]: TObject read GetPropObj;
    property  AutoFreeProperties: Boolean read fAutoFree write fAutoFree;
  end;

  TCustomCtrl = class;
  TRootCtrl  = class;
  TCustomCtrlClass = class of TCustomCtrl;

  // TCtrlStorageManager - pretty much runs everything.
  // TCtrlStorageManager contains ...
  //   1. TEventPropertyHandler object
  //   2. TLayeredImage32 object that contains
  //         'root' layer that contains
  //           'RootCtrl' layer object that contains all 'ctrl' objects

  TCtrlStorageManager = class(TStorageManager)
  private
    fLayeredImg   : TLayeredImage32;
    fRootCtrl     : TRootCtrl;
    fLastCtrl     : TCustomCtrl;
    fMseDownCtrl  : TCustomCtrl;
    fCurrCursor   : integer;
    fShortcutList : TList;
    fEventHandler : TEventPropertyHandler;
    fDelayedLinks : TLoadRecArray;
    fMainHdl      : THandle;
    fAllowResize  : Boolean;
    fDesigning    : Boolean;
    fDesignTarget : TCustomCtrl;
    fClickLayer   : TLayer32;
    fSizingGroup  : TSizingGroupLayer32;
    fClickPt      : TPoint;
    function GetRepaintReq: Boolean;
    function GetFocusedCtrl: TCustomCtrl;
    procedure SetFocusedCtrl(ctrl: TCustomCtrl);
    function FindChildByLoadId(parent: TStorage; id: integer): TStorage;
    procedure AddDelayedLink(const delayedLink: TLoadRec);
    procedure SetDesigning(value: Boolean);
    procedure SetDesignTarget(ctrl: TCustomCtrl);
  protected
    procedure SetDesignScale(value: double); override;
    procedure BeginRead; override;
    function GetExternalProp(const str: string; out success: Boolean): TObject;
    procedure LoadStoredObjects(const utf8: UTF8String); override;
    function  GetExternalPropName(obj: TObject): string; override;
    function GetExternalEvent(const method: TMethod; out textId: string): Boolean; override;
    procedure RemoveShortcutOwner(ctrl: TCustomCtrl);
    property ShortcutList: TList read fShortcutList;
  public
    // Constructor also creates
    //   1. a TLayeredImage32 object containing a 'root' TGroupLayer32, and
    //   2. a TRootCtrl object 'RootCtrl' that's owned by the 'root'.
    constructor Create(parent: TStorage = nil; const name: string = ''); overload; override;
    destructor Destroy; override;
    procedure Quit;
    function  InsertChild(index: integer; storeClass: TStorageClass): TStorage; override;
    procedure MouseDown(mouseButton: TMouseButton; shift: TShiftState; const pt: TPoint);
    procedure MouseMove(shift: TShiftState; const pt: TPoint);
    procedure MouseUp(mouseButton: TMouseButton; shift: TShiftState; const pt: TPoint);
    function  MouseWheel(shift: TShiftState; wheelDelta: Integer; mousePos: TPoint): Boolean;
    procedure KeyDown(var Key: Word; shift: TShiftState);
    procedure KeyUp(var Key: Word; shift: TShiftState);
    procedure KeyPress(var Key: Char);
    procedure Resize(width, height: Cardinal);
    function  FindShortcutOwner(const aShortcut: TShortcut): TCustomCtrl;
{$IFDEF MSWINDOWS}
    function CreateMainWindow(const UniqueClassName: string;
      const WindowCaption: string; const IconResourceName: string = '';
      Width: integer = 0; Height: integer = 0): THandle;
{$ENDIF}
    property CurrentCursor: integer read fCurrCursor;
    property Designing: Boolean read fDesigning write SetDesigning;
    property DesignTarget: TCustomCtrl read fDesignTarget write SetDesignTarget;
    property EventAndPropertyHandler : TEventPropertyHandler read fEventHandler;
    property FocusedCtrl: TCustomCtrl read GetFocusedCtrl write SetFocusedCtrl;
    property LayeredImage: TLayeredImage32 read fLayeredImg write fLayeredImg;
    property MainHdl     : THandle read fMainHdl;
    property RootCtrl: TRootCtrl read fRootCtrl;
    property RepaintRequired: Boolean read GetRepaintReq;
  published
    property AllowResize: Boolean read fAllowResize write fAllowResize;
  end;

  TCustomCtrl = class(THitTestLayer32, INotifyRecipient)
  private
    fFont         : TFontCache;
    fShortcut     : TShortcut;
    fUsableFont   : TFontCache;
    fBevelHeight  : double;
    fEnabled      : Boolean;
    fColor        : TColor32;
    fFontColor    : TColor32;
    fCanFocus     : Boolean;
    fAutoPosition : TAutoPosition;
    fCaption      : string;
    fOnClick      : TNotifyEvent;
    fAfterPaint   : TNotifyEvent;
    fDelayedLinks : TLoadRecArray;
    fAllowRTEdit  : Boolean;        // run-time editing
    fIsthemed     : Boolean;
    procedure AddDelayedLink(const propName: string; targetId: integer);
    function  GetManager: TCtrlStorageManager;
    procedure SetBevHeight(value: double);
    procedure SetColor(color: TColor32);
    function  GetFont: TFontCache;
    function  GetHasFocus: Boolean;
    function  CanSetFocus: Boolean;
    function  IsEmpty: Boolean;
    // theme colors
    function  GetColor: TColor32;
    function  GetAltColor: TColor32;
    function  GetFocusColor: TColor32;
    function  GetFontColor: TColor32;
    procedure SetEnabled(value: Boolean);
    procedure SetShortcut(value: TShortcut);
    procedure SetAutoPosition(ap: TAutoPosition);
    function  GetFocusLineWidth: double; {$IFDEF INLINE} inline; {$ENDIF}
    function  GetRootCtrl: TRootCtrl;
  protected
    function  GetClientHeight: double; virtual;
    function  GetClientWidth: double; virtual;
    function  GetClientRect: TRectD; virtual;
    procedure SetVisible(value: Boolean); override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    function  CheckSkipWriteProperty(propInfo: PPropInfo): Boolean; override;
    procedure Clicked; virtual;
    function  GetUsableFont: Boolean;
    procedure SetCaption(const caption: string); virtual;
    procedure SetFont(font: TFontCache); virtual;
    function  HasShortcut: Boolean;
    procedure Paint; virtual;
    procedure DoMouseDown(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); virtual;
    procedure DoMouseMove(shift: TShiftState; const pt: TPoint); virtual;
    procedure DoMouseUp(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave(newCtrl: TStorage); virtual;
    function DoMouseWheel(Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    procedure DoDblClick; virtual;
    procedure DoKeyDown(var Key: Word; shift: TShiftState); virtual;
    procedure DoKeyUp(var Key: Word; shift: TShiftState); virtual;
    procedure DoKeyPress(var Key: Char); virtual;
    procedure FontChanged; virtual;
    procedure DoBeforeMerge; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor  Destroy; override;
    procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification); virtual;
    procedure Scale(value: double); virtual;
    procedure SetFocus; virtual;
    procedure KillFocus; virtual;
    function  GetCtrlWithFocus: TCustomCtrl; virtual;
    function  IsVisibleToCtrlRoot: Boolean;
    function  IsEnabledToCtrlRoot: Boolean;
    property ClientHeight: double read GetClientHeight;
    property ClientWidth: double read GetClientWidth;
    property ClientRect: TRectD read GetClientRect;
    property HasFocus   : Boolean read GetHasFocus;
    property RootCtrl   : TRootCtrl read GetRootCtrl;
    property StorageManager: TCtrlStorageManager read GetManager;
    property FocusLineWidth: double read GetFocusLineWidth;
    property Shortcut   : TShortcut read fShortcut write SetShortcut;
  published
    property AllowRTEdit  : Boolean read fAllowRTEdit write fAllowRTEdit;
    property AutoPosition : TAutoPosition
      read fAutoPosition write SetAutoPosition;
    property BevelHeight: double read fBevelHeight write SetBevHeight;
    // CanFocus - (local property) and not to be confused with CanSetFocus().
    property CanFocus   : Boolean read fCanFocus write fCanFocus;
    property Caption    : string read fCaption write SetCaption;
    property Color      : TColor32 read fColor write SetColor;
    property Enabled    : Boolean read fEnabled write SetEnabled;
    property Font       : TFontCache read GetFont write SetFont;
    property FontColor  : TColor32 read fFontColor write fFontColor;
    property IsThemed   : Boolean read fIsthemed write fIsthemed;
    property OnClick    : TNotifyEvent read fOnClick write fOnClick;
    property OnPainted  : TNotifyEvent read fAfterPaint write fAfterPaint;
  end;

  TCustomAutoPosCtrl = class(TCustomCtrl)
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
  end;

  TLabelCtrl = class(TCustomCtrl)
  private
    fTargetCtrl: TCustomCtrl;
  protected
    procedure Clicked; override;
    procedure Paint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
  published
    // TargetCtrl - control to receive focus when an
    // ampersanded char in the label's caption is ALT key triggered.
    property TargetCtrl: TCustomCtrl read fTargetCtrl write fTargetCtrl;
  end;

  TStatusbarCtrl = class(TCustomAutoPosCtrl)
  protected
    procedure Paint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
  end;

  TScrollCtrl = class;                  //scrollbar (vertical or horizontal)

  TScrollingCtrl = class(TCustomCtrl)   //class that can own and use a scrollbar
  private
    fScrollV      : TScrollCtrl;
    fScrollH      : TScrollCtrl;
    fInnerMargin  : double;
    fShadowSize   : double;
    fShadowAngle  : double;
    fScrollOffset : TPointD;
    fOnPaint      : TNotifyEvent;
    fOnChange     : TNotifyEvent;
  protected
    function  GetClientHeight: double; override;
    function  GetClientWidth: double; override;
    function  GetClientRect: TRectD; override;
    procedure DoChildAutoPositioning;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoAfterMerge; override;
    procedure Paint; override;
    procedure SetScrollH(scrollCtrl: TScrollCtrl); virtual;
    procedure SetScrollV(scrollCtrl: TScrollCtrl); virtual;
    procedure DoScrollX(newPos: double); virtual;
    procedure DoScrollY(newPos: double); virtual;
    property  ScrollOffset: TPointD read fScrollOffset write fScrollOffset;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    function   InsertChild(index: integer; layerClass: TLayer32Class;
      const name: string = ''): TLayer32; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure Scale(value: double); override;
    property ScrollH  : TScrollCtrl read fScrollH;
    property ScrollV  : TScrollCtrl read fScrollV;
  published
    property Margin   : double read fInnerMargin write fInnerMargin;
    property ShadowSize: double read fShadowSize write fShadowSize;
    property OnChange : TNotifyEvent read fOnChange write fOnChange;
    property OnPaint  : TNotifyEvent read fOnPaint write fOnPaint;
  end;

  TListItem = class(TStorage)
  private
    fText     : string;
    fImageIdx : integer;
  published
    property Text     : string read fText write fText;
    property ImageIdx: integer read fImageIdx write fImageIdx;
  end;

  TListCtrl = class(TScrollingCtrl)
  private
    fTopItem    : integer;
    fItemIndex  : integer;
    fAutoSize   : Boolean;
    fImgList    : TSvgImageList32;
    procedure SetItemIndex(index: integer); // sets selected item
    function  GetItemHeight: double;
    function  GetVisibleItems: integer;
    function  GetItemCount: integer;
    function  GetItem(index: integer): TListItem;
    procedure SetImgList(svgImageList: TSvgImageList32);
  protected
    procedure EndRead; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseDown(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); override;
    procedure DoScrollY(newPos: double); override;
    procedure SetScrollV(scrollCtrl: TScrollCtrl); override;
    procedure Paint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification); override;
    procedure ScrollItemIndexIntoView;
    function  AddChild(storeClass: TStorageClass; const text: string = ''): TStorage; reintroduce; overload; virtual;
    function InsertChild(index: integer; storeClass: TStorageClass): TStorage;  override;
    property Item[index: integer]: TListItem read GetItem; default;
    property ItemCount: integer read GetItemCount; // differs from ChildCount!
  published
    property AutoSize : Boolean read fAutoSize write fAutoSize;
    property ImageList: TSvgImageList32 read fImgList write SetImgList;
    property ItemIndex: integer read fItemIndex write SetItemIndex;
  end;

  TMenuItemType = (mitNormal, mitCheckbox, mitRadioBtn);

  TMenuItemCtrl = class(TCustomCtrl)
  private
    fCapWidth : double;
    fImgIdx   : integer;
    fMenuType   : TMenuItemType;
    fIsChecked  : Boolean;
    procedure UpdateItem;
  protected
    procedure Clicked; override;
    procedure SetCaption(const caption: string); override;
    procedure SetFont(font: TFontCache); override;
    // procedure Paint; override; // painting done by parent control :)
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    property CaptionWidth: double read fCapWidth;
  published
    property Checked: Boolean read fIsChecked write fIsChecked;
    property ImageIdx: integer read fImgIdx write fImgIdx;
    property MenuItemType: TMenuItemType read fMenuType write fMenuType;
  end;

  TBaseMenuCtrl = class(TCustomCtrl)
  private
    fItemIndex  : integer;
    fUsesIcons  : Boolean;
    fImgList    : TSvgImageList32;
    procedure SetItemIndex(index: integer);
    procedure SetImgList(svgImageList: TSvgImageList32);
  protected
    function GetMenuItemRect(idx: integer): TRectD; virtual; abstract;
    function GetItemWidths: TArrayOfDouble;
    function  GetItemHeight: double; virtual;
    procedure Clicked; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    property UsesIcons: Boolean read fUsesIcons;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification); override;
    property ItemIndex: integer read fItemIndex write SetItemIndex;
  published
    property ImageList: TSvgImageList32 read fImgList write SetImgList;
  end;

  TPopMenuCtrl = class(TBaseMenuCtrl)
  private
    fItemsSize  : TSizeD;
    fRealParent : TMenuItemCtrl;
  protected
    procedure GetItemsSize;
    procedure SetVisible(value: Boolean); override;
    procedure UpdateLayout; virtual;
    procedure DoMouseMove(shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseLeave(newCtrl: TStorage); override;
    procedure DoKeyPress(var Key: Char); override;
    procedure Paint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    function  GetMenuItemRect(idx: integer): TRectD; override;
  end;

  TMainMenuCtrl = class(TBaseMenuCtrl)
  protected
    function  GetItemHeight: double; override;
    procedure DoMouseMove(shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseLeave(newCtrl: TStorage); override;
    procedure Paint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    function  GetMenuItemRect(idx: integer): TRectD; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
  end;

  TMemoCtrl = class(TScrollingCtrl)
  private
    fTopLine        : integer;
    fChunkedText    : TChunkedText;
    fTextMargin     : TPointD;
    fPageMetrics    : TPageTextMetrics;
    fCursorChunkPos : TPoint;
    fSelStart       : TPoint;
    fSelEnd         : TPoint;
    fBuffer         : TImage32;
    fTextRect       : TRectD;
    fMouseLeftDown  : Boolean;
    fSavedImage     : TArrayOfColor32;
    procedure CheckPageMetrics;
    function  GetVisibleLines: integer;
    procedure SetTextMargin(const margin: TPointD);
    procedure ScrollCaretIntoView;
    procedure ResetSelPos;
  protected
    function  GetText: string;
    procedure SetText(const txt: string);
    procedure SetFont(font: TFontCache); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseMove(Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    function DoMouseWheel(Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure SetScrollV(scrollCtrl: TScrollCtrl); override;
    procedure DoScrollY(newPos: double); override;
    procedure FontChanged; override;
    procedure Paint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure Scale(value: double); override;
    function ChunkPosToPos(const chunkPos: TPoint): TPointD;
    function PosToChunkIdx(const relPos: TPointD): TPoint;
  published
    property Text       : string read GetText write SetText;
    property TextMargin : TPointD read fTextMargin write SetTextMargin;
  end;

  TEditCtrl = class(TScrollingCtrl)
  private
    fChunkedText    : TChunkedText;
    fPageMetrics    : TPageTextMetrics;
    fLineHeight     : double;
    fTextMargin     : TPointD;
    fCursorChunkPos : TPoint;
    fCursorMoved    : Boolean;
    fSelStart       : TPoint;
    fSelEnd         : TPoint;
    fMouseLeftDown  : Boolean;
    function GetTextRect(stripMargins: Boolean): TRectD;
    procedure SetTextMargin(const margin: TPointD);
    procedure ResetSelPos;
    procedure InsertChunkAtCursor(const chunkTxt: UnicodeString);
  protected
    procedure SetFont(font: TFontCache); override;
    procedure DoKeyDown(var key: Word; Shift: TShiftState); override;
    procedure DoKeyPress(var chr: Char); override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseMove(Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure FontChanged; override;
    procedure Paint; override;
    function GetText: string;
    procedure SetText(const txt: string);
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    function ChunkIdxToPos(const chunkIdx: TPoint): TPointD;
    function PosToChunkIdx(const relPos: TPointD): TPoint;
  published
    property TextMargin: TPointD read fTextMargin write SetTextMargin;
    property Text       : string read GetText write SetText;
  end;

  TButtonCtrl = class(TCustomCtrl)
  private
    fPadding  : double;
    fPressed  : Boolean;
    procedure SetPadding(value: double);
  protected
    procedure Clicked; override;
    procedure SetCaption(const text: string); override;
    procedure SetPressed(value: Boolean); virtual;
    procedure Paint; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(Button: TMouseButton;
      shift: TShiftState; const pt: TPoint); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure Scale(value: double); override;
    property Pressed: Boolean read fPressed write SetPressed;
  published
    property Padding: double read fPadding write SetPadding;
  end;

  TRoundedBtnCtrl = class(TButtonCtrl)
  protected
    procedure Paint; override;
  end;

  TEllipseBtnCtrl = class(TButtonCtrl)
  protected
    procedure Paint; override;
  end;

  TImageBtnCtrl = class(TButtonCtrl)
  private
    fTextPos  : TTextPosition;
    fImgIdx   : integer;
    fImgList  : TSvgImageList32;
    procedure SetTextPos(value: TTextPosition);
    procedure SetImgList(svgImageList: TSvgImageList32);
    procedure SetImgIdx(index: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
  published
    property TextPosition: TTextPosition read fTextPos write SetTextPos;
    property ImageList: TSvgImageList32 read fImgList write SetImgList;
    property ImageIdx: integer read fImgIdx write SetImgIdx;
  end;

  TCheckboxCtrl = class(TButtonCtrl)
  private
    fAutoState  : Boolean;
    fTriState   : TTriState;
    fTextPos    : TTextPositionH;
    procedure SetTriState(state: TTriState);
    procedure SetTextPos(value: TTextPositionH);
  protected
    procedure Clicked; override;
    procedure Paint; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
  published
    property AutoState: Boolean read fAutoState write fAutoState;
    property State: TTriState read fTriState write SetTriState;
    property TextPosition: TTextPositionH read fTextPos write SetTextPos;
  end;

  TRadioBtnCtrl = class(TCheckboxCtrl)
  protected
    procedure Paint; override;
  end;

  TPanelCtrl = class(TScrollingCtrl)
  protected
    procedure RecalcScrollMax; virtual;
    procedure CheckScrollOffset(const newBounds: TRectD);
    procedure DoScrollX(newPos: double); override;
    procedure DoScrollY(newPos: double); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
  end;

  TRootCtrl = class(TPanelCtrl)
  private
    fFocusedCtrl  : TCustomCtrl;
    fCaptureCtrl  : TCustomCtrl;
    fPopMenuCtrl  : TPopMenuCtrl;
    fTheme        : TCtrlTheme;
    procedure SetTheme(const theme: TCtrlTheme);
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure Scale(value: double); override;
    function SetFocus(ctrl: TCustomCtrl): Boolean; reintroduce; overload;
{$IFDEF MSWINDOWS}
    function SetCapture(ctrl: TCustomCtrl; hdl: HWnd): Boolean;
    function ReleaseCapture: Boolean;
    property CaptureCtrl : TCustomCtrl read fCaptureCtrl;
{$ENDIF}
    property FocusedCtrl : TCustomCtrl read fFocusedCtrl;
    property PopMenuCtrl : TPopMenuCtrl read fPopMenuCtrl write fPopMenuCtrl;
    property Theme       : TCtrlTheme read fTheme write SetTheme;
  end;

  TPageTabCtrl = class(TCustomAutoPosCtrl)
  protected
    procedure Clicked; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
  end;

  TPagePnlCtrl = class(TPanelCtrl) //ie TScrollingCtrl
  protected
    procedure Clicked; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
  end;

  TPageCtrl = class(TCustomCtrl)
  private
    fTabHeight  : double;
    fTabWidth   : double;
    fActiveIdx  : integer;
    fShadowSize : double;
    fOnPaint    : TNotifyEvent;
    fTabOffsets : TArrayOfDouble;
    procedure SetTabWidth(width: double);
    procedure SetTabHeight(height: double);
    procedure SetActiveIndex(index: integer);
    function  GetPagePanel(index: integer): TPagePnlCtrl;
    procedure DrawTab(idx: integer);
    procedure DrawTabs;
    procedure ResizeTabs;
  protected
    procedure Paint; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure EndRead; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure Scale(value: double); override;
    procedure ClearTabs;
    procedure AddTab(const caption: string);
    procedure AddTabs(const captions: TArrayOfString);
    function  ActivePage: TPagePnlCtrl;
    property  Panel[index: integer]: TPagePnlCtrl read GetPagePanel;
  published
    property  ActiveIndex: integer read fActiveIdx write SetActiveIndex;
    property  ShadowSize: double read fShadowSize write fShadowSize;
    property  TabWidth: double read fTabWidth write SetTabWidth;
    property  TabHeight: double read fTabHeight write SetTabHeight;
    property  OnPaint  : TNotifyEvent read fOnPaint write fOnPaint;
  end;

  TProgressCtrl = class(TCustomCtrl)
  private
    fSize         : integer;
    fStartColor   : TColor32;
    fEndColor     : TColor32;
    fOrientation  : TScrollOrientation;
    fPosition     : double;
    fMax          : integer;
    procedure SetMax(newMax: integer);
    procedure SetPosition(newPos: double);
    procedure SetOrientation(newOrient: TScrollOrientation);
  protected
    procedure FontChanged; override;
    procedure Paint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
  published
    property Max        : integer read fMax write SetMax;
    property Position   : double read fPosition write SetPosition;
    property Orientation: TScrollOrientation read fOrientation write SetOrientation;
    property StartColor : TColor32 read fStartColor write fStartColor;
    property EndColor   : TColor32 read fEndColor write fEndColor;
  end;

  TScrollState = (scNormal, scScrollBtn,
    scLeft, scTop = 2, scRight, scBottom  = 3);

  TSliderCtrl = class(TCustomCtrl)
  private
    fSize         : double;
    fScrollStep   : integer;
    fBtnSize      : double;
    fOrientation  : TScrollOrientation;
    fPressed      : Boolean;
    fPosition     : double;
    fMin          : double;
    fMax          : double;
    fScrollState  : TScrollState;
    fOnSlide     : TNotifyEvent;
    function  GetDelta: double;
    procedure SetBtnSize;
    procedure SetPressed(value: Boolean);
    procedure SetMin(newMin: double);
    procedure SetMax(newMax: double);
    procedure SetPosition(newPos: double);
    procedure SetOrientation(newOrient: TScrollOrientation);
  protected
    procedure FontChanged; override;
    procedure Paint; override;
    procedure DoMouseDown(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseMove(shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(button: TMouseButton; shift: TShiftState; const pt: TPoint); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    property Pressed    : Boolean read fPressed write SetPressed;
  published
    property Min        : double read fMin write SetMin;
    property Max        : double read fMax write SetMax;
    property Position   : double read fPosition write SetPosition;
    property Step       : integer read fScrollStep write fScrollStep;
    property Orientation: TScrollOrientation read fOrientation write SetOrientation;
    property OnSlide    : TNotifyEvent read fOnSlide write fOnSlide;
  end;

  TScrollCtrl = class(TCustomAutoPosCtrl)
  private
    fAutoHide     : Boolean;
    fSize         : double;
    fScrollRatio  : double;
    fHiddenSize   : double;
    fBtnSize      : double;
    fScrollStep   : double;   // double to facilitate scaling
    fOrientation  : TScrollOrientation;
    fPos          : double;
    fMax          : double;
    fMinBtnSize   : double;   // double to facilitate scaling
    fMousePos     : TPointD;
    fScrollState  : TScrollState;
    fTargetCtrl   : TScrollingCtrl; // Parent as TScrollingCtrl
    procedure SetMax(newMax: double);
    procedure SetScrollSize(newSize: double);
    procedure SetBtnPos(newPos: double);
    procedure SetAutoHide(value: Boolean);
    procedure SetScrollStep(value: integer);
    function  GetScrollStep: integer;
  protected
    procedure SetParent(parent: TStorage); override;
    procedure SetOrientation(orientation  : TScrollOrientation);
    procedure DoAutoPosition; virtual;
    procedure SetDefaultSize;
    function GetChevronSize: Double;
    procedure CheckAutoVisible(firstPass: Boolean);
    procedure GetScrollBtnInfo; overload;
    procedure GetScrollBtnInfo(out btnStart, btnEnd: double); overload;
    function CanScroll: Boolean;
    procedure Paint; override;
    procedure DoMouseDown(button: TMouseButton; shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseMove(shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(button: TMouseButton; shift: TShiftState; const pt: TPoint); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure Scale(value: double); override;
    procedure SetFocus; override;
    property TargetCtrl : TScrollingCtrl read fTargetCtrl;
  published
    property AutoHide     : Boolean read fAutoHide write SetAutoHide;
    property Size         : double read fSize write SetScrollSize;
    property Max          : double read fMax write SetMax;
    property Orientation  : TScrollOrientation
      read fOrientation write SetOrientation;
    property Position     : double read fPos write SetBtnPos; // relative to pixels
    property Step         : integer read GetScrollStep write SetScrollStep;
  end;

const
  RESET   = -1;
  ssShift = $8000;
  ssCtrl  = $4000;
  ssAlt   = $2000;

  lightTheme: TCtrlTheme = (color: clBtnFace32;
    altColor: $FFE0E0E0; focusColor: clGreen32; fontColor: clBlack32);
  darkTheme : TCtrlTheme = (color: clDarkGray32;
    altColor: $FF6A6A6A; focusColor: clLime32; fontColor: clWhite32);

var
  clDefDark32 :   TColor32 = $FF008000;
  clDefMid32  :   TColor32 = $FF33CC33;
  clDefLite32 :   TColor32 = $FFDDEEDD;

  tickPaths     :   TPathsD;
  queryPaths    :   TPathsD;

  sizeCursor  : HIcon;
  handCursor  : HIcon;
  arrowCursor : HIcon;

{$IFDEF MSWINDOWS}
  minDragDist   : integer;
  dblClickInt   : integer;
  lastClick     : integer;

  function BasicWindowProc(hWnd, uMsg, wParam: WPARAM; lParam: LPARAM): Integer; stdcall;
  function CenterWindow(windowHdl: THandle): boolean;
  procedure ResizeMainWindow(hdl: HWnd; width, height: Cardinal);
  function GetScreenResolution: double;
{$ENDIF}

implementation

uses
  RTTI, StrUtils, Img32.Extra, Img32.Draw, Img32.SVG.Core;

resourcestring
  rsClassNotRegistered  = 'Error: %s is not a registered TStorage class';
  rsListCtrlError       = 'TListCtrl error: index out of range.';

const
  VK_BACK   = $8;
  VK_TAB    = $9;
  VK_RETURN = $D;
  VK_ESCAPE = 27;
  VK_SPACE  = 32;
  VK_LEFT   = 37;
  VK_UP     = 38;
  VK_RIGHT  = 39;
  VK_DOWN   = 40;
  VK_DELETE = 46;

{$IFNDEF NO_STORAGE}
const
  squote  = '''';
  dquote  = '"';
  space   = #32;
  lt      = '<';
  gt      = '>';
  amp     = '&';
  tab     = #9;
{$ENDIF}

{$IFDEF MSWINDOWS}

//------------------------------------------------------------------------------
// Miscellaneous Windows functions
//------------------------------------------------------------------------------

function CenterWindow(windowHdl: THandle): boolean;
var
  DesktopRec, WindowRec: TRect;
  l, t: integer;
begin
  result := SystemParametersInfo(SPI_GETWORKAREA, 0, @DesktopRec, 0);
  if not result or (windowHdl = 0) then exit;
  GetWindowRect(windowHdl, WindowRec);
  l := ((DesktopRec.Right - DesktopRec.Left) - (WindowRec.Right - WindowRec.Left)) div 2;
  t := ((DesktopRec.Bottom - DesktopRec.Top) - (WindowRec.Bottom - WindowRec.Top)) div 2;
  if l < 0 then l := 0;
  if t < 0 then t := 0;
  result := SetWindowPos(windowHdl, 0, l, t, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;
//------------------------------------------------------------------------------

procedure ResizeMainWindow(hdl: HWnd; width, height: Cardinal);
var
  winRec, clientRec: TRect;
  dx, dy: cardinal;
begin
  GetWindowRect(hdl, winRec);
  GetClientRect(hdl, clientRec);
  dx := winrec.Width - clientRec.Width;
  dy := winrec.Height - clientRec.Height;
  SetWindowPos(hdl, 0,0,0, width + dx, height + dy, SWP_NOZORDER or SWP_NOMOVE);
end;
//------------------------------------------------------------------------------

function WParamToShiftState(wParam: Word): TShiftState;
begin
  Result := 0;
  if wParam and MK_SHIFT <> 0 then Result := Result + ssShift;
  if wParam and MK_CONTROL <> 0 then Result := Result + ssCtrl;
  if GetKeyState(VK_MENU) < 0 then Result := Result + ssAlt;
end;
//------------------------------------------------------------------------------

function LParamToShiftState(lParam: LPARAM): TShiftState;
const
  AltMask = $20000000;
begin
  Result := 0;
  if GetKeyState(VK_SHIFT) < 0 then Result := Result + ssShift;
  if GetKeyState(VK_CONTROL) < 0 then Result := Result + ssCtrl;
  if lParam and AltMask <> 0 then Result := Result + ssAlt;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Windows message handler - that passes keyboard and
// mouse messages to storageMngr for processing
//------------------------------------------------------------------------------

function BasicWindowProc(hWnd, uMsg,	wParam: WPARAM; lParam: LPARAM): Integer; stdcall;
var
  key     : Word;
  chr     : Char;
  w, h    : integer;
  pt      : TPoint;
  ps      : TPAINTSTRUCT;
  dc      : HDC;
  img     : TImage32;
  dx, dy  : integer;
  rec     : TRectD;
  shift   : TShiftState;
  layer   : TLayer32;
  updateRect    : TRect;
  storageMngr   : TCtrlStorageManager;
  layeredImg32  : TLayeredImage32;
begin
  storageMngr := Pointer(GetWindowLongPtr(hWnd, GWLP_USERDATA));
  case uMsg of
    WM_LBUTTONDOWN:
      begin
        Result := 0;
        SetCapture(hWnd);
        storageMngr.fClickPt := Img32.vector.Point(
          SmallInt(LoWord(lParam)),
          SmallInt(HiWord(lParam)));
        if storageMngr.Designing then
        begin
          layeredImg32 := storageMngr.fLayeredImg;
          storageMngr.fClickLayer := layeredImg32.GetLayerAt(storageMngr.fClickPt);
          if (storageMngr.fClickLayer is TCustomCtrl) and
            TCustomCtrl(storageMngr.fClickLayer).AllowRTEdit then
          begin
            storageMngr.DesignTarget := TCustomCtrl(storageMngr.fClickLayer);
            SetCursor(handCursor);
            //UpdateTargetPosDisplay;
            Exit;
          end
          else if (storageMngr.fClickLayer is TButtonDesignerLayer32) then
          begin
            SetCursor(sizeCursor);
            Exit;
          end
          else if not (storageMngr.fClickLayer is TBaseMenuCtrl) then
            Exit;
        end;
        // not designing so get storageMngr to process
        storageMngr.MouseDown(mbLeft,
          WParamToShiftState(wParam), storageMngr.fClickPt);
        if storageMngr.RepaintRequired then
          InvalidateRect(hWnd, nil, false);
      end;
    WM_MOUSEMOVE:
      begin
        Result := 0;
        pt := Img32.vector.Point(
          SmallInt(LoWord(lParam)),
          SmallInt(HiWord(lParam)));
        dx := pt.X - storageMngr.fClickPt.X;
        dy := pt.Y - storageMngr.fClickPt.Y;

        if storageMngr.Designing then
        begin
          if not assigned(storageMngr.fClickLayer) then
          begin
            layeredImg32 := storageMngr.fLayeredImg;
            layer := layeredImg32.GetLayerAt(pt);
            if Assigned(layer) and (layer is TCustomCtrl) and
              TCustomCtrl(layer).AllowRTEdit then
            begin
              if (layer is TButtonDesignerLayer32) then
                SetCursor(sizeCursor)
              else if layer = storageMngr.DesignTarget then
                SetCursor(handCursor)
              else
                SetCursor(arrowCursor);
            end else
            begin
              // ie don't try to design the menus
              storageMngr.MouseMove(WParamToShiftState(wParam), pt);
              if storageMngr.RepaintRequired then
                InvalidateRect(hWnd, nil, false);
            end;
            Exit;
          end;

          if (storageMngr.fClickLayer = storageMngr.DesignTarget) then
          begin
            SetCursor(handCursor);
            storageMngr.DesignTarget.Offset(dx, dy);
            storageMngr.fSizingGroup.Offset(dx, dy);
            storageMngr.fClickPt := pt;
            //UpdateTargetPosDisplay;
            InvalidateRect(hWnd, nil, false);
          end else if (storageMngr.fClickLayer is TButtonDesignerLayer32) then
          begin
            SetCursor(sizeCursor);
            storageMngr.fClickLayer.Offset(dx, dy);
            rec := RectD(UpdateSizingButtonGroup(storageMngr.fClickLayer));
            rec := storageMngr.DesignTarget.Parent.MakeRelative(rec);
            storageMngr.DesignTarget.SetInnerBounds(rec);
            storageMngr.fClickPt := pt;
            InvalidateRect(hWnd, nil, false);
          end;
        end else
        begin
          // not designing so get storageMngr to process
          storageMngr.MouseMove(WParamToShiftState(wParam), pt);
          if storageMngr.RepaintRequired then
            InvalidateRect(hWnd, nil, false);
        end;
      end;
    WM_LBUTTONUP:
      begin
        ReleaseCapture;
        pt := Img32.vector.Point(
          SmallInt(LoWord(lParam)),
          SmallInt(HiWord(lParam)));
        storageMngr.MouseUp(mbLeft, WParamToShiftState(wParam), pt);
        if storageMngr.RepaintRequired then
          InvalidateRect(hWnd, nil, false);
        storageMngr.fClickLayer := nil;
        Result := 0;
      end;
    WM_MOUSEWHEEL:
      begin
        if storageMngr.Designing then storageMngr.fClickLayer := nil;
        pt := Img32.vector.Point(
          SmallInt(LoWord(lParam)),
          SmallInt(HiWord(lParam)));
        if storageMngr.MouseWheel(WParamToShiftState(wParam),
          SmallInt(HiWord(wParam)), pt) and storageMngr.RepaintRequired then
            InvalidateRect(hWnd, nil, false);
        Result := 0;
      end;
    WM_SYSCOMMAND:
      if wParam = SC_KEYMENU then
        Result := 0 else //stops beeps with Alt key combos
        Result := DefWindowProc(hWnd, uMsg, wParam, lParam);
    WM_CHAR:
      begin
        if not storageMngr.Designing then
        begin
          chr := Char(wParam);
          storageMngr.KeyPress(chr);
          if storageMngr.RepaintRequired then
            InvalidateRect(hWnd, nil, false);
        end;
        Result := 0;
      end;
    WM_KEYDOWN:
      begin
        key := Word(wParam);
        shift := LParamToShiftState(lParam);

        if storageMngr.Designing and 
          Assigned(storageMngr.DesignTarget) then
        begin
          case Key of
            VK_DELETE:
              begin
                FreeAndNil(storageMngr.fSizingGroup);
                FreeAndNil(storageMngr.DesignTarget);
                InvalidateRect(hWnd, nil, false);
              end;
            VK_DOWN:
              begin
                if Shift and ssCtrl <> 0 then w := 5 else w := 1;
                storageMngr.DesignTarget.Offset(0, w);
                storageMngr.fSizingGroup.Offset(0, w);
                //UpdateTargetPosDisplay;
                InvalidateRect(hWnd, nil, false);
              end;
            VK_UP:
              begin
                if Shift and ssCtrl <> 0 then w := 5 else w := 1;
                storageMngr.DesignTarget.Offset(0, -w);
                storageMngr.fSizingGroup.Offset(0, -w);
                //UpdateTargetPosDisplay;
                InvalidateRect(hWnd, nil, false);
              end;
            VK_RIGHT:
              begin
                if Shift and ssCtrl <> 0 then w := 5 else w := 1;
                storageMngr.DesignTarget.Offset(w, 0);
                storageMngr.fSizingGroup.Offset(w, 0);
                //UpdateTargetPosDisplay;
                InvalidateRect(hWnd, nil, false);
              end;
            VK_LEFT:
              begin
                if Shift and ssCtrl <> 0 then w := 5 else w := 1;
                storageMngr.DesignTarget.Offset(-w, 0);
                storageMngr.fSizingGroup.Offset(-w, 0);
                //UpdateTargetPosDisplay;
                InvalidateRect(hWnd, nil, false);
              end;
          end;
        end
        else
        begin
          storageMngr.KeyDown(key, shift);
          if storageMngr.RepaintRequired then
            InvalidateRect(hWnd, nil, false);
        end;
        Result := 0;
      end;
    WM_SYSKEYDOWN:
      begin
        // eg alt keys
        key := Word(wParam);
        shift := LParamToShiftState(lParam);
        storageMngr.KeyDown(key, shift);
        if storageMngr.RepaintRequired then
          InvalidateRect(hWnd, nil, false);
        Result := 0;
      end;
    WM_SYSKEYUP:
      begin
        Result := 0;
      end;
    WM_KEYUP:
      begin
        key := Word(wParam);
        shift := LParamToShiftState(lParam);
        storageMngr.KeyUp(key, shift);
        if storageMngr.RepaintRequired then
          InvalidateRect(hWnd, nil, false);
        Result := 0;
      end;
    WM_SIZE:
      begin
        w := LoWord(lParam);
        h := HIWord(lParam);
        storageMngr.Resize(w, h);
        InvalidateRect(hWnd, nil, true);
        Result := 0;
      end;
    WM_PAINT:
      begin
        Result := 0;
        if not Assigned(storageMngr) then Exit;
        layeredImg32 := storageMngr.LayeredImage;
        img := layeredImg32.GetMergedImage(false, updateRect);
        //img.SaveToFile('c:\temp\test.bmp');
        dc := BeginPaint(hWnd, &ps);
        img.CopyToDc(updateRect, dc, updateRect.Left, updateRect.Top, false);
        EndPaint(hWnd, &ps);
      end;
    WM_ERASEBKGND: Result := 1;
    WM_GETDLGCODE: Result := DLGC_WANTALLKEYS;
    WM_DPICHANGED:
      begin
        //nb: Manifest DPI Awareness must be set to
        //at least 'Per Monitor' to receive this notification.
        DpiAwareOne := LoWord(wParam) / 96;
        DpiAware1 := Round(DpiAwareOne);
        Result := 0;
      end;
    WM_DESTROY:
      begin
        PostQuitMessage(0);
        result := 0;
        exit;
      end;
    else
      Result := DefWindowProc(hWnd, uMsg, wParam, lParam);
  end;
end;

//------------------------------------------------------------------------------
// Miscellaneous Windows functions
//------------------------------------------------------------------------------

function GetScreenResolution: double;
var
  dc: HDC;
begin
  dc := GetDC(0);
  try
    Result := GetDeviceCaps(dc, LOGPIXELSX) / 96;
  finally
    ReleaseDC(0, dc);
  end;
end;
//------------------------------------------------------------------------------

function LoadUnicodeTextFromClipboard: UnicodeString;
var
   len : integer;
   DataHandle: THandle;
   dataPtr: Pointer;
begin
  Result := '';
  if not OpenClipboard(0) then Exit;
  try
    if not IsClipboardFormatAvailable(CF_UNICODETEXT) then Exit;
    DataHandle := GetClipboardData(CF_UNICODETEXT);
    if DataHandle = 0 then Exit;
    len := GlobalSize(DataHandle) -2;
    if len < 2 then Exit;
    SetLength(Result, len div 2);
    dataPtr := GlobalLock(DataHandle);
    try
      Move(dataPtr^, Result[1], len);
    finally
      GlobalUnlock(DataHandle)
    end;
  finally
    CloseClipboard;
  end;
end;
//------------------------------------------------------------------------------

function SaveTextToClipboard(const text: UnicodeString): Boolean;
var
   len : integer;
   DataHandle: THandle;
   dataPtr: Pointer;
begin
  Result := false;
  if not OpenClipboard(0) then Exit;
  try
    len := Length(text) * 2 + 2;
    DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, len);
    dataPtr := GlobalLock(DataHandle);
    try
      Move(text[1], dataPtr^, len);
    finally
      GlobalUnlock(DataHandle);
    end;
    Result := SetClipboardData(CF_UNICODETEXT, DataHandle) > 0;
  finally
    CloseClipboard;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------

function IsShortcut(const shortcut: TShortcut): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (shortcut.chrIdx > 0);
end;
//------------------------------------------------------------------------------

function DeleteChar(const caption: string; idx: integer): string;
begin
  Result := caption;
  Delete(Result, idx, 1);
end;
//------------------------------------------------------------------------------

function HasAltOrCtrlKey(shiftState: TShiftState): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := shiftState and (ssCtrl or ssAlt) <> 0;
end;
//------------------------------------------------------------------------------

function HasAltKey(shiftState: TShiftState): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := shiftState and ssAlt <> 0;
end;
//------------------------------------------------------------------------------

function HasCtrlKey(shiftState: TShiftState): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := shiftState and ssCtrl <> 0;
end;
//------------------------------------------------------------------------------

function HasShiftKey(shiftState: TShiftState): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := shiftState and ssShift <> 0;
end;
//------------------------------------------------------------------------------

function MakeShortcut(key: Word; shift: TShiftState): TShortcut;
begin
  Result.flags := 0;
  Result.chr := #0;
  Result.chrIdx := 0;

  case (key) of
    8, 9, 13, 27, 32, 33, 34, 35, 37, 38, 39, 40, 45, 46: Result.chr := Char(key);
    else if key < 48 then Exit
    else Result.chr := Char(key);
  end;
  Result.flags := shift;
  //if HasShiftKey(shift) then Result.chr := UpCase(Result.chr);
end;
//------------------------------------------------------------------------------

function GetShortcutFromCaption(const caption: string;
  out shortcut: TShortcut): Boolean; overload;
var
  i, len: integer;
begin
  shortcut.chr := #0;
  shortcut.flags := 0;
  shortcut.chrIdx := 0;
  len := Length(caption);
  i := Pos('&', caption);
  Result := (i > 0) and (i < len);
  while Result and (caption[i + 1] = '&') do
  begin
    i := StrUtils.PosEx('&', caption, i + 2);
    Result := (i > 0) and (i < len);
  end;
  if not Result then Exit;
  shortcut.chrIdx := i;
  shortcut.flags := $2000;
  shortcut.chr := Upcase(caption[i + 1]);

end;
//------------------------------------------------------------------------------

function GetShortcutFromCaption(const caption: string): TShortcut; overload;
begin
  GetShortcutFromCaption(caption, Result);
end;
//------------------------------------------------------------------------------

function BoolToTriState(val: Boolean): TTriState;
begin
  if val then
    Result := tsYes else
    Result := tsNo;
end;
//------------------------------------------------------------------------------

function IsCorrectPosOrder(const pt1, pt2: TPoint): Boolean;
begin
  if pt1.X <> pt2.X then
  begin
    Result := pt1.X < pt2.X;
  end else
  begin
    Result := pt1.Y <= pt2.Y;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawBtnInternal(Image: TImage32; const p: TPathD;
  caption: string; font: TFontCache; bevHeight, padding: double;
  pressed: Boolean; enabled: Boolean; color: TColor32 = clNone32;
  textColor: TColor32 = clBlack32; textOffX: integer = 0;
  textOffY: integer = 0; closePath: Boolean = true);
var
  dx    : double;
  pp    : TPathsD;
  rec2  : TRectD;
  sc    : TShortcut;
begin
  if not enabled then pressed := false;

  if GetAlpha(color) > 2 then
    DrawPolygon(Image, p, frNonZero, color);

  if pressed then
    DrawEdge(Image, p, clSilver32, clWhite32, bevHeight, closePath) else
    DrawEdge(Image, p, clWhite32, clSilver32, bevHeight, closePath);

  if not Assigned(font) then Exit;

  sc := GetShortcutFromCaption(caption);
  if sc.chrIdx > 0 then
    caption := DeleteChar(caption, sc.chrIdx);

  rec2 := GetBoundsD(p);
  if padding > 0 then
    Img32.Vector.InflateRect(rec2, -Round(padding), -Round(padding));
  if (GetAlpha(textColor) < 3) or (Trim(caption) = '') then Exit;
  dx := font.LineHeight / 24;
  pp := font.GetTextOutline(rec2, caption, taCenter, tvaMiddle, sc.chrIdx);
  if pressed then
    pp := TranslatePath(pp, bevHeight + textOffX - dx, bevHeight + textOffY - dx) else
    pp := TranslatePath(pp, textOffX - dx, textOffY - dx);
  if GetLuminance(textColor) < 160 then
  begin
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    if not enabled then textColor := MakeLighter(textColor, 40);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, textColor);
  end else
  begin
    DrawPolygon(Image, pp, frNonZero, clBlack32);
    pp := TranslatePath(pp, dx, dx);
    if not enabled then
      textColor := MakeDarker(textColor, 30);
    DrawPolygon(Image, pp, frNonZero, textColor);
  end;
end;
//------------------------------------------------------------------------------

function GetRoundedRectPath(const rec: TRectD): TPathD;
var
  i : double;
begin
  i := Average(rec.Width, rec.Height);
  Result := RoundRect(rec, i * 0.333);
end;
//------------------------------------------------------------------------------

function GetTabOutLine(const rec: TRectD; radius: double): TPathD;
var
  diam  : double;
  rec2  : TRectD;
begin
  diam := radius * 2;
  SetLength(Result, 1);
  Result[0] := PointD(rec.Left, rec.Bottom);
  with rec do
    rec2 := RectD(Left, Top, Left + diam, Top + diam);
  ConcatPaths(Result, arc(rec2, angle180, angle270));
  TranslateRect(rec2, rec.Width - diam, 0);
  ConcatPaths(Result, arc(rec2, angle270, angle0));
  AppendPoint(Result, rec.BottomRight);
end;
//------------------------------------------------------------------------------

function MakeCumulativeArray(start, step: double; count: integer): TArrayOfDouble;
var
  i: integer;
begin
  SetLength(Result, count +1);
  Result[0] := start;
  for i := 1 to count do
    Result[i] := Result[i-1] + step;
end;
//------------------------------------------------------------------------------

function GetVariableWidthTabOffsets(const captions: array of string;
  font: TFontCache; startingOffset: double): TArrayOfDouble;
var
  i, len  : integer;
  padding : double;
begin
  Result := nil;
  len := Length(captions);
  if (len = 0) or not assigned(font) then Exit;
  padding := font.GetTextWidth(' ') * 6;
  SetLength(Result, len + 1);
  Result[0] := startingOffset;
  for i := 1 to len do
    Result[i] := Result[i - 1] +
      font.GetTextWidth(captions[i-1]) + padding;
end;
//------------------------------------------------------------------------------

procedure DrawCheckboxCtrl(Image: TImage32; const rec: TRect;
  bevHeight: double; triState: TTriState = tsNo; lineWidth: double = 1.0;
  color: TColor32 = clWhite32; enabled: Boolean = true);
var
  d,w   : double;
  hbh   : double;
  p     : TPathD;
  pp    : TPathsD;
  rec2  : TRectD;
begin
  if not enabled then triState := tsUnknown;
  hbh := bevHeight / 2;
  d := min(rec.Width, rec.Height) - bevHeight;
  rec2.Left := rec.Left + hbh;
  rec2.Top := rec.Top + hbh;
  rec2.Right := rec2.Left + d;
  rec2.Bottom := rec2.Top + d;

  Image.FillRect(Rect(rec2), clWhite32);
  if bevHeight > 0 then
    DrawEdge(Image, rec2, clSilver32, clWhite32, bevHeight);

  case triState of
    tsUnknown :
      begin
        w := RectWidth(rec);
        pp := ScalePath(queryPaths, w * 0.6 / 100);
        pp := TranslatePath(pp, rec.Left + w * 0.2, rec.Top + w * 0.2);
        DrawPolygon(Image, pp, frEvenOdd, SetAlpha(color, 80));
        DrawLine(Image, pp, lineWidth, clBlack32, esPolygon);
      end;
    tsYes :
      begin
        w := RectWidth(rec);
        // scale tick to approx. 2/3 of width and offset tick approx 1/6
        p := ScalePath(tickPaths[0],  w * 0.65 / 100);
        p := TranslatePath(p, rec.Left + w * 0.18, rec.Top + w * 0.18);
        DrawPolygon(Image, p, frEvenOdd, color);
        DrawLine(Image, p, lineWidth, clBlack32, esPolygon);
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawRadioCtrl(Image: TImage32; const ellipse: TPathD;
  color: TColor32; bevHeight: double; enabled: Boolean;
  triState: TTriState = tsNo; lineWidth: double = 1.0);
var
  d     : double;
  rec2  : TRectD;
  bc    : TColor32;
  p     : TPathD;
  pp    : TPathsD;
begin
  rec2 := GetBoundsD(ellipse);
  d := min(rec2.Width, rec2.Height) - bevHeight;

  if not enabled then
  begin
    triState := tsUnknown;
    bc := clBtnFace32;
  end else
    bc := clWhite32;

  DrawPolygon(Image, ellipse, frNonZero, bc);
  if bevHeight > 0 then
    DrawEdge(Image, ellipse, clSilver32, clWhite32, bevHeight);

  case triState of
    tsUnknown :
      begin
        pp := ScalePath(queryPaths, d * 0.7 / 100);
        pp := TranslatePath(pp, rec2.Left + d * 0.22, rec2.Top  + d * 0.22);
        DrawPolygon(Image, pp, frEvenOdd, SetAlpha(color, 80));
        DrawLine(Image, pp, lineWidth, clBlack32, esPolygon);
      end;
    tsYes :
      begin
        d := Ceil(d / 5);
        InflateRect(rec2, -d, -d);
        p := Img32.Vector.Ellipse(rec2);
        DrawPolygon(Image, p, frNonZero, color);
        DrawLine(Image, p, lineWidth, clBlack32, esPolygon);
      end
    else Exit;
  end;
end;

//------------------------------------------------------------------------------
// TCustomCtrl
//------------------------------------------------------------------------------

constructor TCustomCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  if not Assigned(StorageManager) then
    Raise Exception.Create('Error - there must be a storage manager');
  if (self is TRootCtrl) then
  begin
    if Assigned(StorageManager.fRootCtrl) then
      raise Exception.Create('Error - only one RootCtrl allowed :)');
    StorageManager.fRootCtrl := TRootCtrl(self);
  end;
  if Assigned(parent) and (parent is TCustomCtrl) then
  begin
    with TCustomCtrl(parent) do
    begin
      Self.Color        := Color;
      Self.FontColor    := FontColor;
      Self.Isthemed     := Isthemed;
      Self.AllowRTEdit  := AllowRTEdit;
    end;
  end else
  begin
    Color       := clBtnFace32;
    FontColor   := clBlack32;
    Isthemed    := True;
    AllowRTEdit := true;
  end;
  fBevelHeight  := dpiAware(2);
  OuterMargin   := DPIAware(5);
  fEnabled      := true;
  fCanFocus     := true;
end;
//------------------------------------------------------------------------------

destructor TCustomCtrl.Destroy;
begin
  if HasShortcut then
    StorageManager.RemoveShortcutOwner(self);

  //notify INotifySenders
  if Assigned(fFont) then fFont.DeleteRecipient(self);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.ReceiveNotification(Sender: TObject; notify: TImg32Notification);
begin
  if Sender is TFontCache then
  begin
    if notify = inDestroy then
      Setfont(nil) else
      FontChanged;
  end;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.Scale(value: double);
var
  i: integer;
  rec: TRectD;
begin
  if AllowRTEdit then
  begin
    fBevelHeight := fBevelHeight * value;
    OuterMargin := OuterMargin * value;

    case AutoPosition of
      apNone:
      begin
        rec := InnerBounds;
        rec := ScaleRect(rec, value);
        SetInnerBounds(rec);
        Invalidate;
      end;
      apLeft:
      begin
        rec := TCustomCtrl(Parent).ClientRect;
        rec.Right := rec.Left + Width * value;
        SetInnerBounds(rec);
        Invalidate;
      end;
      apRight:
      begin
        rec := TCustomCtrl(Parent).ClientRect;
        rec.Left := rec.Right - Width * value;
        SetInnerBounds(rec);
        Invalidate;
      end;
      apTop:
      begin
        rec := TCustomCtrl(Parent).ClientRect;
        rec.Bottom := rec.Top + Height * value;
        SetInnerBounds(rec);
        Invalidate;
      end;
      apBottom:
      begin
        rec := TCustomCtrl(Parent).ClientRect;
        rec.Top := rec.Bottom - Height * value;
        SetInnerBounds(rec);
        Invalidate;
      end;
    end;
  end;
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) then
      TCustomCtrl(Child[i]).Scale(value);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetCaption(const caption: string);
begin
  if fCaption = caption then Exit;
  fCaption := caption;
  Shortcut := GetShortcutFromCaption(caption);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetColor(color: TColor32);
begin
  if color = fColor then Exit;
  fColor := color;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetColor: TColor32;
begin
  if Isthemed then
  begin
    if Assigned(RootCtrl) then Result := RootCtrl.Theme.Color
    else if self is TRootCtrl then Result := TRootCtrl(self).Theme.Color
    else Result := fColor;
  end else
    Result := fColor;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetAltColor: TColor32;
begin
  if Isthemed then
  begin
    if Assigned(RootCtrl) then Result := RootCtrl.Theme.AltColor
    else if self is TRootCtrl then Result := TRootCtrl(self).Theme.AltColor
    else Result := MakeDarker(fColor, 20);
  end else
    Result := MakeDarker(fColor, 20);
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetFocusColor: TColor32;
begin
  if Isthemed and Assigned(RootCtrl) then
    Result := RootCtrl.Theme.FocusColor else
    Result := clDefDark32;
end;
//------------------------------------------------------------------------------

function  TCustomCtrl.GetFontColor: TColor32;
begin
    if Assigned(RootCtrl) then Result := RootCtrl.Theme.FontColor
    else if self is TRootCtrl then Result := TRootCtrl(self).Theme.FontColor
    else Result := fFontColor;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetManager: TCtrlStorageManager;
begin
  Result := inherited StorageManager as TCtrlStorageManager;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetBevHeight(value: double);
begin
  if value = fBevelHeight then Exit;
  fBevelHeight := value;
  Invalidate;
  //todo: this can alter bounds of some ctrls so that will need attention
end;
//------------------------------------------------------------------------------

function  TCustomCtrl.GetFocusLineWidth: double;
begin
  Result := StorageManager.DesignScale * DpiAware(1.25);
end;
//------------------------------------------------------------------------------

function  TCustomCtrl.GetRootCtrl: TRootCtrl;
begin
  Result := StorageManager.fRootCtrl;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.CheckSkipWriteProperty(propInfo: PPropInfo): Boolean;
var
  propName: string;
  //parentPropInfo: PPropInfo;
begin
   Result := False; // ie don't skip writing to storage

   // NOTE: individual properties could, and ideally should
   // be checked in descendant classes that actually contain
   // the named properties.

   if not Assigned(parent) then Exit;
   propName := string(propInfo.Name);
   //parentPropInfo := GetPropInfo(Parent.ClassInfo, propName);

   if (propName = 'AllowRTEdit') then
   begin
     // only write an AllowRTEdit property if it differs from its parent
     if Parent is TCustomCtrl then
       Result := TCustomCtrl(Parent).AllowRTEdit = AllowRTEdit else
       Result := False;
   end
   else if (propName = 'AutoPosition') then
   begin
     // only write an AutoPosition property if it isn't apNone
     Result := AutoPosition = apNone;
   end
   else if (propName = 'CanFocus') then
   begin
     // only write a CanFocus property if it isn't TRUE
     Result := CanFocus = True;
   end
   else if (propName = 'Color') then
   begin
     if Parent is TCustomCtrl then
       Result := TCustomCtrl(Parent).Color = Color else
       Result := False;
   end
   else if (propName = 'CursorId') then
   begin
     Result := CursorId = 0;
   end
   else if (propName = 'Enabled') then
   begin
     Result := Enabled = True;
   end
   else if (propName = 'Font') then
   begin
     // only write a Font property if it differs from its parent's Font
     if not HasPublishedProperty(parent.Classname, propName) then Exit;
     Result := Font = GetObjectProp(Parent, propName);
   end
   else if (propName = 'FontColor') then
   begin
     if Parent is TCustomCtrl then
       Result := TCustomCtrl(Parent).Color = Color else
       Result := FontColor = clBlack32;
   end
   else if (propName = 'IsThemed') then
   begin
     if Parent is TCustomCtrl then
       Result := TCustomCtrl(Parent).IsThemed = IsThemed else
       Result := False;
   end
   else if (propName = 'Opacity') then
   begin
     Result := Opacity = 255;
   end
   else if (propName = 'ShadowSize') then
   begin
     Result := GetFloatProp(self, 'ShadowSize') = 0;
   end
   else if (propName = 'Shortcut') then
   begin
     Result := Shortcut.chr = #0;
   end
   else if (propName = 'TabWidth') then
   begin
     Result := GetOrdProp(self, 'TabWidth') <= 0;
   end
   else if (propName = 'TabHeight') then
   begin
     Result := GetOrdProp(self, 'TabHeight') <= 0;
   end
   else if (propName = 'Visible') then
   begin
     Result := Visible = True;
   end
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetFont: TFontCache;
begin
  GetUsableFont;
  Result := fUsableFont;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetUsableFont: Boolean;
var
  ctrl: TLayer32;
begin
  ctrl := self.Parent;
  while not Assigned(fUsableFont) and (ctrl is TCustomCtrl) do
  begin
    fUsableFont := TCustomCtrl(ctrl).fUsableFont;
    ctrl := ctrl.Parent;
  end;
  Result := Assigned(fUsableFont);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetFont(font: TFontCache);
begin
  if font = fFont then Exit;
  if Assigned(fFont) then
    fFont.DeleteRecipient(self as INotifyRecipient);
  fFont := font;
  if Assigned(fFont) then
    fFont.AddRecipient(self as INotifyRecipient);
  FontChanged;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.FontChanged;
var
  i: integer;
begin
  if Assigned(fFont) and Assigned(fFont.FontReader) and
    (fFont.FontHeight > 0) then
  begin
    fUsableFont := fFont;
  end else
  begin
    fUsableFont := nil;
    GetUsableFont; //look for a font from a parent
  end;

  Invalidate;
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) and
      not Assigned(TCustomCtrl(Child[i]).fFont) then
        TCustomCtrl(Child[i]).FontChanged; //child inherits the parent's font
end;
//------------------------------------------------------------------------------

function TCustomCtrl.IsEmpty: Boolean;
begin
  Result := (Width = 0) or (Height = 0);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetEnabled(value: Boolean);
begin
  if fEnabled = value then Exit;
  fEnabled := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetShortcut(value: TShortcut);
var
  hadShortcut: Boolean;
begin
  hadShortcut := HasShortcut;
  fShortcut := value;
  if (hadShortcut = IsShortcut(value)) then Exit;
  if hadShortcut then
    StorageManager.RemoveShortcutOwner(self) else
    StorageManager.ShortcutList.Add(self);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetAutoPosition(ap: TAutoPosition);
begin
  if ap = fAutoPosition then Exit;
  fAutoPosition := ap;
  Invalidate;
  if IsVisibleToCtrlRoot and
    (Parent is TScrollingCtrl) then
      TScrollingCtrl(Parent).DoChildAutoPositioning;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetHasFocus: Boolean;
begin
  Result := (RootCtrl.FocusedCtrl = self);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetFocus;
begin
  RootCtrl.SetFocus(Self);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.KillFocus;
begin
  if self = RootCtrl.FocusedCtrl then
    RootCtrl.SetFocus(nil);
end;
//------------------------------------------------------------------------------

function TCustomCtrl.CanSetFocus: Boolean;
begin
  Result := fCanFocus and fEnabled and IsEnabledToCtrlRoot;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetCtrlWithFocus: TCustomCtrl;
begin
  Result := RootCtrl.FocusedCtrl;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if not Visible or not fEnabled then Exit;
  if CanSetFocus and not HasFocus then SetFocus;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseMove(shift: TShiftState; const pt: TPoint);
begin
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseEnter;
begin
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseLeave(newCtrl: TStorage);
begin
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseUp(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
{$IFDEF MSWINDOWS}
var
  lc: integer;
begin
  if button <> mbLeft then Exit;
  lc := lastClick;
  lastClick := GetTickCount;
  if lastClick - lc < dblClickInt then DoDblClick
  else Clicked;
end;
{$ELSE}
begin
  Clicked;
end;
{$ENDIF}
//------------------------------------------------------------------------------

function TCustomCtrl.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := false;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoDblClick;
begin
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    Clicked;
    Key := 0;
  end
  else if Assigned(Parent) and (Parent is TCustomCtrl) then
    TCustomCtrl(Parent).DoKeyDown(Key, Shift);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoKeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(Parent) and (Parent is TCustomCtrl) then
    TCustomCtrl(Parent).DoKeyUp(Key, Shift);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoKeyPress(var Key: Char);
begin
  if Assigned(Parent) and (Parent is TCustomCtrl) then
    TCustomCtrl(Parent).DoKeyPress(Key);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.Clicked;
begin
  if not HasFocus and CanSetFocus then SetFocus;
  if HasFocus and Assigned(fOnClick) then fOnClick(Self);
end;
//------------------------------------------------------------------------------

function TCustomCtrl.HasShortcut: Boolean;
begin
  Result := IsShortcut(fShortcut);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.Paint;
begin
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetClientHeight: double;
begin
  Result := Height - fBevelHeight * 2;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetClientWidth: double;
begin
  Result := Width - fBevelHeight * 2;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetClientRect: TRectD;
begin
  Result.Left := fBevelHeight;
  Result.Top := fBevelHeight;
  Result.Right := Width - fBevelHeight;
  Result.Bottom := Height - fBevelHeight;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetVisible(value: Boolean);

  procedure RecursiveNotifyVisibleChildren(obj: TLayer32);
  var
    i: integer;
  begin
    if (obj is TScrollingCtrl) then
      TScrollingCtrl(obj).DoChildAutoPositioning;
    for i := 0 to obj.ChildCount -1 do
      if obj[i].Visible then
        RecursiveNotifyVisibleChildren(obj[i]);
  end;

begin
  if Visible = value then Exit;
  inherited;
  if IsVisibleToCtrlRoot then
    RecursiveNotifyVisibleChildren(self)
  else
  begin
{$IFDEF MSWINDOWS}
    if (RootCtrl.CaptureCtrl = self) then
      RootCtrl.ReleaseCapture;
{$ENDIF}
  end;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.IsVisibleToCtrlRoot: Boolean;
var
  obj: TLayer32;
begin
  obj := Self;
  Result := Visible;
  while Result and not (obj is TRootCtrl) do
  begin
    obj := obj.Parent;
    Result := obj.Visible;
  end;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.IsEnabledToCtrlRoot: Boolean;
var
  obj: TLayer32;
begin
  Result := True;
  obj := Self;
  while Result and not (obj is TRootCtrl) do
  begin
    if obj is TCustomCtrl then
      Result := TCustomCtrl(obj).Enabled and obj.Visible else
      Result := Visible;
    obj := obj.Parent;
  end;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoBeforeMerge;
begin
  if UpdateInfo.updateMethod = umNone then Exit;
  Image.BlockNotify;
  try
    Paint;
  finally
    Image.UnblockNotify;
  end;
end;
//------------------------------------------------------------------------------

function IsIntValue(const txt: string; out val: integer): Boolean;
begin
  val := strToIntDef(txt, -1);
  Result := val > 0;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.AddDelayedLink(const propName: string; targetId: integer);
var
  len: integer;
begin
  len := Length(fDelayedLinks);
  SetLength(fDelayedLinks, len +1);
  fDelayedLinks[len].loadObj := self;
  fDelayedLinks[len].propName := propName;
  fDelayedLinks[len].targetId := targetId;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.ReadProperty(const propName, propVal: string): Boolean;
var
  pi      : PPropInfo;
  obj     : TObject;
  method  : TMethod;
  val     : integer;
begin
  Result := False;
  if IsPublishedProp(self, propName) then
  try
    pi := GetPropInfo(self.ClassType, propName);
    case pi.PropType^.Kind of
      tkEnumeration: SetEnumProp(self, propName, propVal);
      tkInteger: TypInfo.SetOrdProp(self, pi, Cardinal(strToIntDef(propVal, 0)));
      tkFloat: SetFloatProp(self, propName, strToFloatDef(propVal, 0.0));
      tkUString: SetPropValue(self, propName, SimpleXmlDecode(propVal));
      tkMethod:
      begin
        method := TMethod(storageManager.EventAndPropertyHandler.Event[propVal]);
        SetMethodProp(self, propName, method);
      end;
      tkClass:
      begin
        // TCustomCtrl properties are stored using their numeric Ids,
        // as opposed to 'external' property names
        if IsIntValue(propVal, val) then
        begin
          obj := storageManager.FindChildByLoadId(RootCtrl, val);
          if not Assigned(obj) then
          begin
            // the target object hasn't been constructed yet so
            // we'll need to fix these up later
            AddDelayedLink(propName, val);
            Exit;
          end
          else if not (obj is TCustomCtrl) then Exit;
        end
        else
          obj := storageManager.GetExternalProp(propVal, Result);

        SetObjectProp(self, propName, obj);
      end;

      else Exit; // eg record types not handled
    end;
    Result := true;
  except
    Result := False;
  end;
end;

//------------------------------------------------------------------------------
// TRootCtrl
//------------------------------------------------------------------------------

constructor TRootCtrl.Create(parent: TLayer32 = nil; const name: string = ''); 
begin
  inherited;
end;
//------------------------------------------------------------------------------

procedure TRootCtrl.Scale(value: double);
var
  i: integer;
begin
  if (value < 0.01) or (value > 100) or (value = 1.0) then Exit;
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) then
      TCustomCtrl(Child[i]).Scale(value);
end;
//------------------------------------------------------------------------------

function TRootCtrl.SetFocus(ctrl: TCustomCtrl): Boolean;
begin
  Result := False;
  if not Assigned(ctrl) then
  begin
    Result := true;
    if not Assigned(fFocusedCtrl) then Exit;
    FocusedCtrl.Invalidate;
    fFocusedCtrl := nil;
  end else if (ctrl <> FocusedCtrl) then
  begin
    while (ctrl <> Self) and not ctrl.CanSetFocus do
    begin
      if not (ctrl.Parent is TCustomCtrl) then Exit;
      ctrl := TCustomCtrl(ctrl.Parent);
    end;

    if Assigned(fFocusedCtrl) then FocusedCtrl.Invalidate;
    fFocusedCtrl := ctrl;
    ctrl.Invalidate;
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function TRootCtrl.SetCapture(ctrl: TCustomCtrl; hdl: HWnd): Boolean;
begin
  if Assigned(fCaptureCtrl) then ReleaseCapture;
  Result := Assigned(ctrl) and (hdl > 0);
  if Result then
  begin
    ctrl.SetFocus;
    Windows.SetCapture(hdl);
    fCaptureCtrl := ctrl;
  end else
    fCaptureCtrl := nil;
end;
//------------------------------------------------------------------------------

function TRootCtrl.ReleaseCapture: Boolean;
begin
  if Assigned(fCaptureCtrl) then
    Result := Windows.ReleaseCapture else
    Result := True;
  fCaptureCtrl := nil;
end;
//------------------------------------------------------------------------------
{$ENDIF}

procedure TRootCtrl.SetTheme(const theme: TCtrlTheme);
begin
  fTheme := theme;
  StorageManager.LayeredImage.Invalidate;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TCustomPosCtrl
//------------------------------------------------------------------------------

constructor TCustomAutoPosCtrl.Create(parent: TLayer32; const name: string);
begin
  fAutoPosition := apCustom;
  fAllowRTEdit := false;
  inherited;
end;

//------------------------------------------------------------------------------
// TLabelCtrl
//------------------------------------------------------------------------------

constructor TLabelCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fCanFocus := false;
  Color := clNone32;
end;
//------------------------------------------------------------------------------

procedure TLabelCtrl.Clicked;
begin
  if Assigned(fTargetCtrl) and fTargetCtrl.CanSetFocus then
    fTargetCtrl.SetFocus;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLabelCtrl.Paint;
var
  caption: string;
  pp: TPathsD;
  dx: double;
  rec: TRectD;
  sc: TShortcut;
begin
  Image.Clear(GetColor);
  if not GetUsableFont then Exit;

  caption := fCaption;
  if GetShortcutFromCaption(caption, sc) then
    Delete(caption, sc.chrIdx, 1);

  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  pp := fUsableFont.GetTextOutline(rec, caption, taCenter, tvaMiddle, sc.chrIdx);
  dx := fUsableFont.LineHeight / 24;
  pp := TranslatePath(pp, -dx, -dx);

  if GetLuminance(GetFontColor) < 160 then
    DrawPolygon(Image, pp, frNonZero, clWhite32) else
    DrawPolygon(Image, pp, frNonZero, clBlack32);
  pp := TranslatePath(pp, dx, dx);
  DrawPolygon(Image, pp, frNonZero, GetFontColor);
end;

//------------------------------------------------------------------------------
// TStatusbarCtrl
//------------------------------------------------------------------------------

constructor TStatusbarCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fCanFocus := false;
  fAutoPosition := apBottom;
end;
//------------------------------------------------------------------------------

procedure TStatusbarCtrl.SetInnerBounds(const newBounds: TRectD);
var
  h: double;
  rec : TRectD;
begin
  rec := newBounds;
  if GetUsableFont then
    h := fUsableFont.LineHeight + fBevelHeight * 3 else
    h := DPIAware(15);
    rec.Top := rec.Bottom - h;
//  if newBounds.Height = 0 then
//  begin
//    if GetUsableFont then
//      h := fUsableFont.LineHeight + fBevelHeight * 3 else
//      h := DPIAware(15);
//    rec.Top := rec.Bottom - h;
//  end;
  inherited SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

procedure TStatusbarCtrl.Paint;
var
  rec: TRect;
  bh: double;
begin
  if not (Parent is TPanelCtrl) then Exit;
  Image.Clear(GetColor);
  rec := Rect(InnerRect);
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  bh := fBevelHeight;
  DrawEdge(Image, rec, clWhite32, clSilver32, bh);
  Img32.Vector.InflateRect(rec, -DPIAware(10), -Round(bh));
  if GetUsableFont then
    DrawText(Image, rec.Left, rec.Top + BevelHeight + fUsableFont.Ascent,
      fCaption, fUsableFont, GetFontColor);
end;

//------------------------------------------------------------------------------
// TScrollingCtrl
//------------------------------------------------------------------------------

constructor TScrollingCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fBevelHeight := dpiAware(2);
  fInnerMargin := 0;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.GetClientHeight: double;
begin
  if Assigned(ScrollV) and ScrollV.visible and
    Assigned(ScrollH) and ScrollH.visible then
      Result := Height - ScrollH.Size - BevelHeight * 4
  else if Assigned(ScrollV) and ScrollV.visible then
    Result := Height - BevelHeight * 4
  else if Assigned(ScrollH) and ScrollH.visible then
    Result := Height - ScrollH.Size - BevelHeight * 3
  else
    Result := Height - BevelHeight * 2;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.GetClientWidth: double;
begin
  if Assigned(ScrollV) and ScrollV.visible and
    Assigned(ScrollH) and ScrollH.visible then
      Result := Width - ScrollV.Size - BevelHeight * 4
  else if Assigned(ScrollV) and ScrollV.visible then
    Result := Width - ScrollV.Size - BevelHeight * 3
  else if Assigned(ScrollH) and ScrollH.visible then
    Result := Width - BevelHeight * 4
  else
    Result := Width - BevelHeight * 2;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.GetClientRect: TRectD;
var
  bh2: double;
begin
  bh2 := BevelHeight * 2;
  if Assigned(ScrollV) and ScrollV.visible and
    Assigned(ScrollH) and ScrollH.visible then
      Result := RectD(bh2, bh2,
        Width - ScrollV.Size - bh2, Height - ScrollH.Size - bh2)
  else if Assigned(ScrollV) and ScrollV.visible then
    Result := RectD(BevelHeight, bh2,
      Width - ScrollV.Size - bh2, Height - bh2)
  else if Assigned(ScrollH) and ScrollH.visible then
    Result := RectD(bh2, BevelHeight,
      Width - bh2, Height - ScrollH.Size - bh2)
  else
    Result := RectD(BevelHeight, BevelHeight,
      Width - BevelHeight, Height - BevelHeight);
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.InsertChild(index: integer; layerClass: TLayer32Class;
  const name: string = ''): TLayer32;
begin
  //non-scroll ctrls must always precede scroll ctrls
  if layerClass.InheritsFrom(TScrollCtrl) then
  begin
    while (index < ChildCount) and not (Child[index] is TScrollCtrl) do inc(index);
  end else
  begin
    if index > ChildCount then index := ChildCount;
    while (index > 0) and (Child[index -1] is TScrollCtrl) do dec(index);
  end;
  Result := inherited InsertChild(index, layerClass, name);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  inherited;
  if IsVisibleToCtrlRoot then
    DoChildAutoPositioning;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoChildAutoPositioning;
var
  i   : integer;
  rec : TRectD;
  clientArea  : TRectD;
  clientAligningCtrl  : TCustomCtrl;
begin
  // this method can be relatively slow, so this method will
  // anly be called when the ctrl and all its parents are visible

  if (StorageState = ssLoading) or IsEmpty or
    not GetUsableFont then Exit;

  if Assigned(fScrollH) then fScrollH.CheckAutoVisible(true);
  if Assigned(fScrollV) then fScrollV.CheckAutoVisible(false);
//
  //reposition horz and vert scrollbars
  if Assigned(fScrollH) and fScrollH.Visible then
  begin
    fScrollH.DoAutoPosition;
    fScrollH.GetScrollBtnInfo;
  end;
  if Assigned(fScrollV) and fScrollV.Visible then
  begin
    fScrollV.DoAutoPosition;
    fScrollV.GetScrollBtnInfo;
  end;

  clientArea := GetClientRect; // get client area now scrollbars sorted
  // clientAligningCtrl: a ctrl with align == apClient
  clientAligningCtrl  := nil;

  // Note: TScrollCtrl.AutoPosition = apCustom
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) and
    not (Child[i] is TScrollCtrl) and
      (TCustomCtrl(Child[i]).fAutoPosition > apCustom) then
  begin
    case TCustomCtrl(Child[i]).fAutoPosition of
      apClient  :
        clientAligningCtrl := TCustomCtrl(Child[i]); // postpone setting bounds
      apLeft    :
        begin
          rec := clientArea;
          rec.Right := rec.Left + Min(rec.Width, Child[i].Width);
          Child[i].SetInnerBounds(rec);
          clientArea.Left := Child[i].Left + Child[i].Width;
        end;
      apTop     :
        begin
          rec := clientArea;
          rec.Bottom := rec.Top + Min(rec.Height, Child[i].Height);
          Child[i].SetInnerBounds(rec);
          clientArea.Top := Child[i].Top + Child[i].Height;
        end;
      apRight   :
        begin
          rec := clientArea;
          rec.Left := rec.Right - Min(rec.Width, Child[i].Width);
          Child[i].SetInnerBounds(rec);
          clientArea.Right := Child[i].Left;
        end;
      apBottom  :
        begin
          rec := clientArea;
          rec.Top := rec.Bottom - Min(rec.Height, Child[i].Height);
          Child[i].SetInnerBounds(rec);
          clientArea.Bottom := Child[i].Top;
        end;
    end;
  end;
  if Assigned(clientAligningCtrl) then
    clientAligningCtrl.SetInnerBounds(clientArea);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoAfterMerge;
var
  rec: TRectD;
begin
  // when both scrollbars are visible, fill in the bottom-right corner
  if not Assigned(MergeImage) or
    not Assigned(ScrollH) or not ScrollH.Visible or
    not Assigned(ScrollV) or not ScrollV.Visible then Exit;

  rec := GetInnerRectD;
  rec.Top := rec.Bottom - ScrollH.Height;
  rec.Left := rec.Right - ScrollV.Width;
  MergeImage.FillRect(Rect(rec), clBtnFace32);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.Paint;
var
  rec : TRectD;
  bh  : double;
begin
  image.Clear(clNone32);
  bh := fBevelHeight;
  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  image.FillRect(Rect(rec), GetColor);
  DrawEdge(Image, rec, clSilver32, clWhite32, bh);

  if Assigned(fOnPaint) then fOnPaint(Self);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.Scale(value: double);
begin
  if AllowRTEdit then
  begin
    fInnerMargin := fInnerMargin * value;
    fScrollOffset := ScalePoint(fScrollOffset, value, value);
    fShadowSize := fShadowSize * value;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetScrollH(scrollCtrl: TScrollCtrl);
begin
  if Assigned(fScrollH) and Assigned(scrollCtrl) then
    Raise Exception.Create('oops - horizontal scroll already assigned.');
  fScrollH := scrollCtrl;
  scrollCtrl.fTargetCtrl := Self;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetScrollV(scrollCtrl: TScrollCtrl);
begin
  if Assigned(fScrollV) and Assigned(scrollCtrl) then
    Raise Exception.Create('oops - vertical scroll already assigned.');
  fScrollV := scrollCtrl;
  scrollCtrl.fTargetCtrl := Self;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  case key of
    VK_LEFT:
      if Assigned(fScrollH) then
      begin
        fScrollH.Position := fScrollH.Position -fScrollH.Step;
        Key := 0;
      end;
    VK_RIGHT:
      if Assigned(fScrollH) then
      begin
        fScrollH.Position := fScrollH.Position +fScrollH.Step;
        Key := 0;
      end;
    VK_UP:
      if Assigned(fScrollV) then
      begin
        fScrollV.Position := fScrollV.Position -fScrollV.Step;
        Key := 0;
      end;
    VK_DOWN:
      if Assigned(fScrollV) then
      begin
        fScrollV.Position := fScrollV.Position +fScrollV.Step;
        Key := 0;
      end;
    else inherited;
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoScrollX(newPos: double);
begin
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoScrollY(newPos: double);
begin
end;

//------------------------------------------------------------------------------
// TListCtrl
//------------------------------------------------------------------------------

constructor TListCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fItemIndex := -1;
  fCanFocus := true;
  Color := clWhite32;
  fFontColor := clBlack32;
  fAutoSize := true;
end;
//------------------------------------------------------------------------------

destructor TListCtrl.Destroy;
begin
  SetImgList(nil);
  inherited;
end;
//------------------------------------------------------------------------------

function TListCtrl.GetItemCount: integer;
begin
  // TListItem objects always preceed TLayer32 objects
  Result := ChildCount;
  while (Result > 0) and (GetStorageChild(Result - 1) is TLayer32) do
    Dec(Result);
end;
//------------------------------------------------------------------------------

procedure TListCtrl.ReceiveNotification(Sender: TObject; notify: TImg32Notification);
begin
  if Sender is TSvgImageList32 then
  begin
    if notify = inDestroy then
      SetImgList(nil) else
      SetInnerBounds(GetInnerBounds);
  end
  else
    inherited;
end;
//------------------------------------------------------------------------------

function TListCtrl.GetItem(index: integer): TListItem;
begin
  if (index < 0) or (index >= GetItemCount) then
    raise Exception.Create(rsListCtrlError);
  result := GetStorageChild(index) as TListItem;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.SetItemIndex(index: integer);
begin
  if Assigned(StorageManager) and
    (StorageManager.StorageState <> ssLoading) and
      ((index < 0) or (index >= ItemCount)) then index := -1;
  if index = fItemIndex then Exit;
  fItemIndex := index;
  ScrollItemIndexIntoView;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TListCtrl.GetItemHeight: double;
begin
  if GetUsableFont then
  begin
    Result := fUsableFont.LineHeight * 1.1;
    if Assigned(fImgList) then
      Result := Math.Max(Result, fImgList.DefaultHeight);
  end
  else if Assigned(fImgList) then
    Result := fImgList.DefaultHeight
  else
    Result := 0;
end;
//------------------------------------------------------------------------------

function TListCtrl.GetVisibleItems: integer;
var
  itmHeight, onetenth: double;
begin
  itmHeight := GetItemHeight;
  onetenth := itmHeight * 0.1;
  if itmHeight = 0 then Result := 1
  else Result := Max(1, Trunc((ClientHeight + onetenth) / itmHeight));
end;
//------------------------------------------------------------------------------

procedure TListCtrl.EndRead;
begin
  inherited;
  if fItemIndex > 0 then
    ScrollItemIndexIntoView;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ItemCount > 0) and GetUsableFont then
  begin
    case key of
      VK_UP:
        if ItemIndex > 0 then
        begin
          ItemIndex := ItemIndex -1;
          Key := 0;
          ScrollItemIndexIntoView;
          Invalidate;
          Exit;
        end;
      VK_DOWN:
        if ItemIndex < ItemCount -1 then
        begin
          ItemIndex := ItemIndex +1;
          Key := 0;
          ScrollItemIndexIntoView;
          Invalidate;
          Exit;
        end;
    end;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
var
  pt2: TPointD;
begin
  if (ItemCount > 0) and GetUsableFont then
  begin
    pt2 := MakeRelative(PointD(pt.X, pt.Y - fBevelHeight));
    ItemIndex := Trunc(pt2.Y / GetItemHeight) + fTopItem;
    Invalidate;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.DoScrollY(newPos: double);
begin
  fTopItem := Min(Round(newPos / GetItemHeight), ItemCount - GetVisibleItems);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.SetScrollV(scrollCtrl: TScrollCtrl);
var
  itmHeight: double;
begin
  inherited;
  if (StorageState <> ssNormal) or not Assigned(ScrollV) or
    not GetUsableFont then Exit;
  itmHeight := GetItemHeight;
  ScrollV.Max := ItemCount * itmHeight;
  ScrollV.Step := Round(itmHeight);
end;
//------------------------------------------------------------------------------

procedure TListCtrl.Paint;
var
  i,cnt, spaceW, vis: integer;
  itemH, bh: double;
  rec, rec2: TRectD;
  img: TImage32;
  scrollShowing: Boolean;
  listItm: TListItem;
const
  drawShad = true;
begin
  Image.Clear;
  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  Image.Clear(Rect(rec), color);
  DrawEdge(Image, rec, clSilver32, clWhite32, fBevelHeight);

  cnt := ItemCount;
  if (cnt = 0) or not GetUsableFont then Exit;

  itemH := GetItemHeight;
  spaceW := Round(fUsableFont.GetSpaceWidth);
  bh := fBevelHeight;

  scrollShowing := Assigned(ScrollV) and ScrollV.Visible;

  vis := GetVisibleItems;
  if HasFocus and ((ItemIndex < fTopItem) or (ItemIndex >= fTopItem + vis)) then
  begin
    InflateRect(rec, bh, bh);
    if (OuterMargin > 0) then
      DrawShadowRect(Image, Rect(rec), OuterMargin, angle45, clDarkGray32);
    DrawLine(Image, Rectangle(rec), FocusLineWidth, GetFocusColor, esPolygon);
    InflateRect(rec, -bh, -bh);
  end;

  if Assigned(fImgList) then
    img := TImage32.Create(fImgList.DefaultWidth, fImgList.DefaultHeight) else
    img := nil;
  try
    InflateRect(rec, -bh, -bh);
    rec.Right := rec.Right - DPIAware(2);
    rec.Bottom := rec.Top + itemH;
    if scrollShowing then rec.Right := rec.Right - ScrollV.Width;

    if fTopItem < 0 then fTopItem := 0
    else if fTopItem > (ItemCount - vis) then
      fTopItem := Math.Max(0, ItemCount - vis);

    for i := fTopItem to fTopItem + vis -1 do
    begin
      if i >= ItemCount then Break;

      rec2 := rec;
      if i = fItemIndex then
      begin
        DrawLine(Image, Rectangle(rec),
          DpiAwareOne, clGray32, esPolygon);
        if HasFocus then
          Image.FillRect(Rect(rec2), clLiteBtn32)
        else
          Image.FillRect(Rect(rec2), clBtnFace32);
        Img32.Vector.InflateRect(rec2, - bh / 2, -bh / 2);
        if HasFocus then
          DrawLine(Image, Rectangle(rec2),
            FocusLineWidth, GetFocusColor, esPolygon);
      end;

      if Assigned(img) then
      begin
        if (i < fImgList.Count) then
        begin
          fImgList.GetImage(i, img);
          rec2.Left := rec2.Left + spaceW;
          rec2.Right := rec2.Left + img.Width;
          rec2.Bottom := rec2.Top + img.Height;
          Image.CopyBlend(img, img.Bounds, Rect(rec2), BlendToAlpha);
        end;
        rec.Left := rec.Left + img.Width + spaceW * 2;
        if not (GetStorageChild(i) is TListItem) then Break;
        listItm := TListItem(GetStorageChild(i));
        DrawText(Image, rec, listItm.Text, fUsableFont, fFontColor, taLeft, tvaMiddle);
        rec.Left := rec.Left - img.Width - spaceW * 2;
      end else
      begin
        if not (GetStorageChild(i) is TListItem) then Break;
        listItm := TListItem(GetStorageChild(i));
        DrawText(Image, rec, listItm.Text, fUsableFont, fFontColor, taLeft, tvaMiddle);
      end;
      TranslateRect(rec, 0, itemH);
    end; // for each item loop

  finally
    FreeAndNil(img);
  end;
end;
//------------------------------------------------------------------------------

function TListCtrl.AddChild(storeClass: TStorageClass; const text: string = ''): TStorage;
begin
  if storeClass.InheritsFrom(TListItem) then
  begin
    Result := InsertStorageChild(GetItemCount, storeClass);
    TListItem(Result).Text := text;
  end else if storeClass.InheritsFrom(TLayer32) then
    Result := InsertStorageChild(ChildCount, storeClass)
  else
    Raise Exception.Create('oops');
  Result.Name := text;
end;
//------------------------------------------------------------------------------

function TListCtrl.InsertChild(index: integer; storeClass: TStorageClass): TStorage;
begin
  if storeClass.InheritsFrom(TListItem) then
  begin
    Result := InsertStorageChild(Math.Min(GetItemCount, index), storeClass);
  end else if storeClass.InheritsFrom(TLayer32) then
    Result := InsertStorageChild(Math.Max(GetItemCount, index), storeClass)
  else
    Raise Exception.Create('oops');
end;
//------------------------------------------------------------------------------

procedure TListCtrl.SetImgList(svgImageList: TSvgImageList32);
begin
  if svgImageList = fImgList then Exit;
  if Assigned(fImgList) then
    fImgList.DeleteRecipient(self as INotifyRecipient);
  fImgList := svgImageList;
  if Assigned(fImgList) then
    fImgList.AddRecipient(self as INotifyRecipient);
  SetInnerBounds(InnerBounds);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.ScrollItemIndexIntoView;
var
  itmHeight: double;
  itemsInView: integer;
begin
  if not Assigned(ScrollV) or not ScrollV.Visible then Exit;
  itmHeight := GetItemHeight;
  if (fItemIndex < 0) or (itmHeight = 0) then Exit;
  itemsInView := GetVisibleItems;
  if fTopItem > fItemIndex then
    fTopItem := fItemIndex
  else if fItemIndex >= fTopItem + itemsInView then
    fTopItem := Math.Max(0, fItemIndex - itemsInView +1)
  else
    Exit;

  ScrollV.Position := fTopItem * itmHeight;
  Invalidate;
end;

//------------------------------------------------------------------------------
// TMenuItemCtrl
//------------------------------------------------------------------------------

constructor TMenuItemCtrl.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited Create(parent, name);
  Caption := name;
end;
//------------------------------------------------------------------------------

procedure TMenuItemCtrl.SetCaption(const caption: string);
begin
  inherited;
  UpdateItem;
end;
//------------------------------------------------------------------------------

procedure TMenuItemCtrl.Clicked;
begin
  if (Parent is TBaseMenuCtrl) then
  begin
    TBaseMenuCtrl(Parent).fItemIndex := Index;
    TBaseMenuCtrl(Parent).Clicked;
  end;
end;
//------------------------------------------------------------------------------

procedure TMenuItemCtrl.SetFont(font: TFontCache);
begin
  inherited;
  UpdateItem;
end;
//------------------------------------------------------------------------------

procedure TMenuItemCtrl.UpdateItem;
begin
  if (Caption = '') or not GetUsableFont or
    not (Parent is TBaseMenuCtrl) then
      fCapWidth := 0
  else
  begin
    fCapWidth := font.GetTextWidth(caption);
    if Parent is TPopMenuCtrl then
      TPopMenuCtrl(Parent).UpdateLayout;
  end;
end;

//------------------------------------------------------------------------------
// TBaseMenuCtrl
//------------------------------------------------------------------------------

constructor TBaseMenuCtrl.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited;
  fCanFocus := true;
  fColor := clBtnFace32;
  fItemIndex := -1;
  fAllowRTEdit := false;
end;
//------------------------------------------------------------------------------

destructor TBaseMenuCtrl.Destroy;
begin
  SetImgList(nil);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TBaseMenuCtrl.ReceiveNotification(Sender: TObject; notify: TImg32Notification);
begin
  if Sender is TSvgImageList32 then
  begin
    if notify = inDestroy then
      SetImgList(nil) else
      SetInnerBounds(GetInnerBounds);
  end
  else
    inherited;
end;
//------------------------------------------------------------------------------

function TBaseMenuCtrl.GetItemWidths: TArrayOfDouble;
var
  i, cnt: integer;
begin
  Result := nil;
  fUsesIcons := false;
  cnt := ChildCount;
  SetLength(Result, cnt);
  for i := 0 to cnt -1 do
    if Child[i] is TMenuItemCtrl then
      with TMenuItemCtrl(Child[i]) do
      begin
        Result[i] := fCapWidth;
        if MenuItemType <> mitNormal then fUsesIcons := true;
      end else
        Result[i] := 0;
end;
//------------------------------------------------------------------------------

procedure TBaseMenuCtrl.Clicked;
var
  mnuItem: TMenuItemCtrl;
  pt: TPointD;
  layer: TLayer32;
begin
  if (ItemIndex < 0) then
  begin
    if Self is TPopMenuCtrl then
      Visible := false;
    Exit;
  end;

  mnuItem := Child[ItemIndex] as TMenuItemCtrl;
  if mnuItem.ChildCount = 0 then
  begin
    if Self is TPopMenuCtrl then Visible := false;
    if mnuItem.fMenuType <> mitNormal then
    begin
      mnuItem.Checked := not mnuItem.Checked;
      Invalidate;
    end;
    if Assigned(mnuItem.OnClick) then
    begin
      layer := Parent;
      while layer <> RootCtrl do
      begin
        if layer is TPopMenuCtrl then
          layer.Visible := false
        else if layer is TMainMenuCtrl then
        begin
          TMainMenuCtrl(layer).fItemIndex := -1;
          Break;
        end;
        layer := layer.Parent;
      end;
      mnuItem.OnClick(mnuItem);
    end;
  end
  else if mnuItem[0] is TPopMenuCtrl then
  begin
    if Assigned(RootCtrl.PopMenuCtrl) then
      RootCtrl.PopMenuCtrl.Visible := false;
    if not GetUsableFont then Exit;
    pt.X := GetMenuItemRect(ItemIndex).Left + fUsableFont.GetSpaceWidth * 2;
    pt.Y := Height - fUsableFont.FontHeight * 0.2;
    pt := MakeAbsolute(pt);
    RootCtrl.PopMenuCtrl := TPopMenuCtrl(mnuItem[0]);
    with RootCtrl.PopMenuCtrl do
    begin
      fRealParent := mnuItem;
      Visible := true;
      Move(RootCtrl, MaxInt);
      PositionAt(pt);
      SetFocus;
      if (ChildCount > 0) and (Child[0] is TMenuItemCtrl) and
        TMenuItemCtrl(Child[0]).Enabled then ItemIndex := 0;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TBaseMenuCtrl.SetItemIndex(index: integer);
begin
  if (index < 0) or (index >= ChildCount) then index := -1;
  if index = fItemIndex then Exit;
  fItemIndex := index;
  Invalidate;
end;
//------------------------------------------------------------------------------

function  TBaseMenuCtrl.GetItemHeight: double;
begin
  if GetUsableFont then
    Result := fUsableFont.LineHeight * 1.1 else
    Result := 0;
end;
//------------------------------------------------------------------------------

procedure TBaseMenuCtrl.SetImgList(svgImageList: TSvgImageList32);
begin
  if svgImageList = fImgList then Exit;
  if Assigned(fImgList) then
    fImgList.DeleteRecipient(self as INotifyRecipient);
  fImgList := svgImageList;
  if Assigned(fImgList) then
    fImgList.AddRecipient(self as INotifyRecipient);
  SetInnerBounds(InnerBounds);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBaseMenuCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ChildCount = 0) or not GetUsableFont then Exit;
  case key of
    VK_LEFT:
      if (Self is TMainMenuCtrl) then
      begin
        if ItemIndex = -1 then
          ItemIndex := ChildCount -1 else
          ItemIndex := ItemIndex -1;
        Invalidate;
      end;
    VK_RIGHT:
      if (Self is TMainMenuCtrl) then
      begin
        if ItemIndex = ChildCount -1 then
          ItemIndex := -1 else
          ItemIndex := ItemIndex +1;
        Invalidate;
      end;
    VK_UP:
      if (Self is TPopMenuCtrl) then
      begin
        if ItemIndex = -1 then
          ItemIndex := ChildCount -1 else
          ItemIndex := ItemIndex -1;
        Invalidate;
      end;
    VK_DOWN:
      if (Self is TPopMenuCtrl) then
      begin
        if ItemIndex = ChildCount -1 then
          ItemIndex := -1 else
          ItemIndex := ItemIndex +1;
        Invalidate;
      end;
  end;
  Key := 0;
end;

//------------------------------------------------------------------------------
// TPopMenuCtrl
//------------------------------------------------------------------------------

constructor TPopMenuCtrl.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited Create(parent, name);
  inherited Visible := false;
  fAllowRTEdit := false;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.SetVisible(value: Boolean);
begin
  if (value = inherited Visible) then Exit;
  if Value = False then
  begin
    RootCtrl.Invalidate;
    RootCtrl.PopMenuCtrl := nil;
    if not Assigned(fRealParent) then Exit;
    Move(fRealParent, 0);
    fRealParent := nil;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.GetItemsSize;
var
  i, cnt: integer;
  dy: double;
begin
  fItemsSize.cx := 0;
  fItemsSize.cy := 0;
  fUsesIcons := false;
  cnt := ChildCount;
  dy := GetItemHeight;
  for i := 0 to cnt -1 do
    if Child[i] is TMenuItemCtrl then
      with TMenuItemCtrl(Child[i]) do
      begin
        fItemsSize.cx := Math.Max(fItemsSize.cx, fCapWidth);
        if Caption = '-' then
          fItemsSize.cy := fItemsSize.cy + dy * 0.5 else
          fItemsSize.cy := fItemsSize.cy + dy;
        if MenuItemType <> mitNormal then fUsesIcons := true;
      end;
end;
//------------------------------------------------------------------------------


procedure TPopMenuCtrl.UpdateLayout;
var
  h, bh2, w, spaceWidth: double;
begin
  if not GetUsableFont then Exit;
  GetItemsSize;
  bh2 := fBevelHeight * 2;
  spaceWidth := fUsableFont.GetSpaceWidth;
  w := fItemsSize.cx + spaceWidth * 2;
  if UsesIcons then
    w := w + fUsableFont.LineHeight + spaceWidth;
  h := fItemsSize.cy;
  SetInnerBounds(RectD(bh2, bh2, w + bh2 * 2, h + bh2 * 2));
end;
//------------------------------------------------------------------------------

function TPopMenuCtrl.GetMenuItemRect(idx: integer): TRectD;
var
  i: integer;
  y, dy, itmH: double;
begin
  GetUsableFont;
  y := 0;
  itmH := GetItemHeight;
  for i := 0 to idx -1 do
    if TCustomCtrl(Child[i]).Caption = '-' then
      y := y + itmH * 0.5 else
      y := y + itmH;
  if TCustomCtrl(Child[idx]).Caption = '-' then
    dy := itmH * 0.5 else
    dy := itmH;

  Result := ClientRect;
  Result.Top := y;
  Result.Bottom := Result.Top + dy;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.DoMouseMove(shift: TShiftState; const pt: TPoint);
var
  idx, i, cnt: integer;
  pt2: TPointD;
  y, dy: double;
begin
  cnt := ChildCount;
  if (cnt = 0) or not GetUsableFont then Exit;
  pt2 := MakeRelative(PointD(pt.X, pt.Y));
  y := BevelHeight * 2;
  dy := GetItemHeight;
  if not PtInRect(InnerBounds, PointD(pt)) then
  begin
    Visible := false;
    Exit;
  end;

  idx := -1;
  for i := 0 to cnt -1 do
  begin
    if TCustomCtrl(Child[i]).Caption = '-' then
      y := y + dy * 0.5 else
      y := y + dy;
    if pt2.Y < y then
    begin
      idx := i;
      Break;
    end;
  end;

  if idx = ItemIndex then Exit;
  ItemIndex := idx;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.DoMouseLeave(newCtrl: TStorage);
begin
  Visible := false;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.DoKeyPress(var Key: Char);
begin
  case Key of
    #10, #13, #32: Clicked;
    #27: Visible := False;
  end;
  Key := #0;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.Paint;
var
  i, cnt: integer;
  x, x2, y, padding, bh: double;
  dummy, iconSize, itmHeight: double;
  rec, rec2: TRectD;
  pp: TPathsD;
  cap: string;
begin
  Image.Clear(GetColor);
  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);

  DrawEdge(Image, rec, clWhite32, clSilver32, fBevelHeight);

  cnt := ChildCount;
  if (cnt = 0) or not GetUsableFont then Exit;
  padding := fUsableFont.GetSpaceWidth;
  bh := BevelHeight;
  if UsesIcons then
    iconSize := fUsableFont.LineHeight - bh else
    iconSize := 0;

  y := rec.Top;
  itmHeight := GetItemHeight;
  for i := 0 to cnt -1 do
  begin
    if not (Child[i] is TMenuItemCtrl) then Break;
    with TMenuItemCtrl(Child[i]) do
    begin
      rec := GetMenuItemRect(i);
      TranslateRect(rec, OuterMargin, OuterMargin);

      if fCaption = '-' then
      begin
        x := rec.Left + padding;
        x2 := rec.Right - padding;
        y := y + itmHeight * 0.25;
        DrawLine(Self.Image, PointD(x, y), PointD(x2, y), FocusLineWidth, clSilver32);
        y := y + itmHeight * 0.25;
      end else
      begin

        if (i = fItemIndex) then
          Self.Image.FillRect(Rect(rec), GetAltColor);

        if (fMenuType <> mitNormal) then
        begin
          rec2 := RectD(rec.Left, rec.Top + bh,
            rec.Left + iconsize, rec.Top + iconsize);
          DrawCheckboxCtrl(Self.Image, Rect(rec2), bh,
            BoolToTriState(checked), 1, GetFocusColor);
        end;

        if UsesIcons then TranslateRect(rec, iconSize + padding, 0);
        x := rec.Left + padding;
        if HasShortcut then
          cap := DeleteChar(fCaption, Shortcut.chrIdx) else
          cap := fCaption;
        pp := fUsableFont.GetTextOutline(x,
          y + fUsableFont.Ascent, cap, dummy, Shortcut.chrIdx);
        DrawPolygon(Self.Image, pp, frEvenOdd, GetFontColor);
        y := y + itmHeight;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TMainMenuCtrl
//------------------------------------------------------------------------------

constructor TMainMenuCtrl.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited;
  AutoPosition := apTop;
  Height := dpiAware(10);
  Visible := true;
end;
//------------------------------------------------------------------------------

procedure TMainMenuCtrl.SetInnerBounds(const newBounds: TRectD);
var
  rec: TRectD;
begin
  if not GetUsableFont then Exit;
  rec := newBounds;
  rec.Bottom := rec.Top + GetItemHeight + BevelHeight * 2;
  inherited SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

function TMainMenuCtrl.GetItemHeight: double;
begin
  if GetUsableFont then
    Result := fUsableFont.LineHeight * 1.25 else
    Result := 0;
end;
//------------------------------------------------------------------------------

function TMainMenuCtrl.GetMenuItemRect(idx: integer): TRectD;
var
  i: integer;
  padding: double;
  widths: TArrayOfDouble;
begin
  GetUsableFont;
  widths := GetItemWidths;
  padding := fUsableFont.GetSpaceWidth * 2;
  Result := ClientRect;
  Result.Bottom := Result.Bottom - BevelHeight;
  for i := 0 to idx -1 do
    Result.Left := Result.Left + widths[i] + padding * 2;
  Result.Right := Result.Left + widths[idx] + padding * 2;
end;
//------------------------------------------------------------------------------

procedure TMainMenuCtrl.DoMouseMove(shift: TShiftState; const pt: TPoint);
var
  idx, i, cnt: integer;
  pt2: TPointD;
  widths: TArrayOfDouble;
  x, padding: double;
begin
  cnt := ChildCount;
  if (cnt = 0) or not GetUsableFont then Exit;
  widths := GetItemWidths;
  padding := fUsableFont.GetSpaceWidth * 2;
  pt2 := MakeRelative(PointD(pt.X, pt.Y));
  x := BevelHeight;

  idx := -1;
  for i := 0 to cnt -1 do
  begin
    x := x + widths[i] + padding * 2;
    if pt2.X < x then
    begin
      idx := i;
      Break;
    end;
  end;

  if idx = ItemIndex then Exit;
  fItemIndex := idx;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainMenuCtrl.DoMouseLeave(newCtrl: TStorage);
begin
  ItemIndex := -1;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMainMenuCtrl.Paint;
var
  i, cnt  : integer;
  bh, y   : double;
  dummy   : double;
  padding : double;
  itemWs  : TArrayOfDouble;
  rec     : TRectD;
  mnuItem : TMenuItemCtrl;
  caption : string;
  pp      : TPathsD;
const
  drawShad = true;
begin
  Image.Clear;
  rec := Self.GetInnerBounds;
  TranslateRect(rec, OuterMargin, OuterMargin);
  Image.Clear(GetColor);
  bh := fBevelHeight;
  InflateRect(rec, -bh , -bh);

  // draw a very faint line under the main menu
  rec.Left := rec.Left + bh;
  rec.Right := rec.Right - bh;
  DrawLine(Image, PointD(rec.Left, rec.Bottom),
    rec.BottomRight, 1, GetAltColor);

  cnt := ChildCount;
  if (cnt = 0) or not GetUsableFont then Exit;
  itemWs := GetItemWidths;
  padding := fUsableFont.GetSpaceWidth * 2;
  y := rec.Top + fUsableFont.Ascent;

  for i := 0 to cnt -1 do
  begin
    if not (Child[i] is TMenuItemCtrl) then Break;
    mnuItem := TMenuItemCtrl(Child[i]);
    rec := GetMenuItemRect(i);
    TranslateRect(rec, OuterMargin, OuterMargin);

    if i = fItemIndex then
      Image.FillRect(Rect(rec), GetAltColor);

    if mnuItem.HasShortcut then
      caption := DeleteChar(mnuItem.caption, mnuItem.Shortcut.chrIdx) else
      caption := mnuItem.Caption;

    pp := fUsableFont.GetTextOutline(rec.Left + padding, y,
      caption, dummy, mnuItem.Shortcut.chrIdx);
    DrawPolygon(Image, pp, frNonZero, GetFontColor);
  end; // for loop
end;

//------------------------------------------------------------------------------
// TMemoCtrl
//------------------------------------------------------------------------------

constructor TMemoCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fBuffer := TImage32.Create;
  fBuffer.BlockNotify;
  fChunkedText := TChunkedText.Create;
  fInnerMargin := BevelHeight;
  Color := clWhite32;
  FontColor := clBlack32;
  BlendFunc  := nil; //assumes edit controls will always be fully opaque.
  fTextMargin := PointD(20, 5);
  fCursorChunkPos := NullPoint;
end;
//------------------------------------------------------------------------------

destructor TMemoCtrl.Destroy;
begin
  ScrollV.Free;
  fChunkedText.Free;
  fBuffer.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  inherited SetInnerBounds(newBounds);
  CheckPageMetrics;
  if fTopLine > fPageMetrics.totalLines - fPageMetrics.visibleLines then
    fTopLine := fPageMetrics.totalLines - fPageMetrics.visibleLines;
  ScrollCaretIntoView;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.Scale(value: double);
begin
  inherited;
  ScrollCaretIntoView;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.CheckPageMetrics;
var
  ScrollVAutoHide     : Boolean;
  scrollShowing       : Boolean;
  rec                 : TRectD;
begin
  if not GetUsableFont or IsEmpty or (StorageState <> ssNormal) or
    // and exit if page metrics already measured
   (RectWidth(fPageMetrics.bounds) > 0) and
   (rec.Width = fTextRect.Width) and (rec.Height = fTextRect.Height) then Exit;

  fSavedImage := nil;
  rec  := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  if HasFocus then
    InflateRect(rec, -TextMargin.X - fBevelHeight, -TextMargin.Y - fBevelHeight)
  else
    InflateRect(rec, -TextMargin.X, -TextMargin.Y);

  if RectsEqual(Rect(fTextRect), Rect(rec)) then Exit;

  fTextRect := rec;
  //start by assuming ScrollV is NOT required
  fPageMetrics := fChunkedText.GetPageMetrics(Rect(fTextRect), fUsableFont.LineHeight);

  ScrollVAutoHide := assigned(ScrollV) and ScrollV.AutoHide;
  scrollShowing := Assigned(ScrollV) and ScrollV.Visible;

  if RectHeight(fPageMetrics.bounds) < fTextRect.Height then
  begin
    if scrollShowing and ScrollVAutoHide then ScrollV.Visible := false;
  end
  else if ScrollVAutoHide and Assigned(ScrollV) then
  begin
    if not scrollShowing then ScrollV.Visible := true;
    if ScrollV.Size = 0 then ScrollV.SetDefaultSize;
    fTextRect.Right := fTextRect.Right - Round(ScrollV.Size);
    fPageMetrics :=
      fChunkedText.GetPageMetrics(Rect(fTextRect), fUsableFont.LineHeight);

    ScrollV.Max := fPageMetrics.lineHeight * (fPageMetrics.totalLines + 0.5);
    ScrollV.Step := Round(fPageMetrics.lineHeight);
  end;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetFont(font: TFontCache);
begin
  inherited;
  if not Assigned(fUsableFont) then Exit;
  fPageMetrics.bounds := NullRect; // force a fPageMetrics refresh
  fPageMetrics.lineHeight := fUsableFont.LineHeight;
  CheckPageMetrics;
  if fCaption <> '' then
    fChunkedText.ApplyNewFont(fUsableFont);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.ResetSelPos;
begin
  if not PointsEqual(fSelStart, fSelEnd) then
    fSavedImage := nil;
  fSelStart := fCursorChunkPos;
  fSelEnd := fCursorChunkPos;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.ScrollCaretIntoView;
var
  visLines: integer;
begin
  if fPageMetrics.bounds.IsEmpty then Exit;
  visLines := GetVisibleLines;
  if fCursorChunkPos.X < fPageMetrics.startOfLineIdx[fTopLine] then
  begin
    while fCursorChunkPos.X < fPageMetrics.startOfLineIdx[fTopLine] do dec(fTopLine);
  end
  else if (fTopLine + visLines < fPageMetrics.totalLines) and
    (fCursorChunkPos.X >= fPageMetrics.startOfLineIdx[fTopLine + visLines]) then
  begin
    while (fTopLine + visLines < fPageMetrics.totalLines) and
      (fCursorChunkPos.X >= fPageMetrics.startOfLineIdx[fTopLine + visLines -1]) do
        inc(fTopLine);
  end
  else Exit;

  fSavedImage := nil;
  with fPageMetrics do
    ScrollV.Position := fTopLine / (totalLines - visibleLines) * ScrollV.fMax;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);

  function GetPrev(var curr: TPoint; ctrlDown: Boolean): Boolean;
  begin
    if ctrlDown then
      Result := fChunkedText.GetPrevWord(curr) else
      Result := fChunkedText.GetPrevChar(curr);
  end;

  function GetNext(var curr: TPoint; ctrlDown: Boolean): Boolean;
  begin
    if ctrlDown then
      Result := fChunkedText.GetNextWord(curr) else
      Result := fChunkedText.GetNextChar(curr);
  end;

var
  x, y: integer;
  pos: TPointD;
  ctrlDown: Boolean;
begin
  if not GetUsableFont then
  begin
    inherited;
    Exit;
  end;

  ctrlDown :=  HasCtrlKey(shift);
{$IFDEF MSWINDOWS}
  case key of
    VK_LEFT:
      begin
        GetPrev(fCursorChunkPos, ctrlDown);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        ScrollCaretIntoView;
        Key := 0;
      end;
    VK_RIGHT:
      begin
        GetNext(fCursorChunkPos, ctrlDown);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        ScrollCaretIntoView;
        Key := 0;
      end;
    VK_UP:
      begin
        pos := ChunkPosToPos(fCursorChunkPos);
        pos.Y := pos.Y - fPageMetrics.lineHeight;
        fCursorChunkPos := PosToChunkIdx(pos);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        ScrollCaretIntoView;
        Key := 0;
      end;
    VK_DOWN:
      begin
        pos := ChunkPosToPos(fCursorChunkPos);
        pos.Y := pos.Y + fPageMetrics.lineHeight;
        fCursorChunkPos := PosToChunkIdx(pos);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
         ScrollCaretIntoView;
        Key := 0;
      end;
    VK_HOME:
      begin
        key := 0;
        if ctrlDown then
          x := 0
        else
        begin
          x := GetLineIndexFromChunkIdx(fPageMetrics, fCursorChunkPos.X);
          x := fPageMetrics.startOfLineIdx[x];
        end;
        if (x = fCursorChunkPos.X) and (fCursorChunkPos.Y = 0) then Exit;
        fCursorChunkPos := Types.Point(x, 0);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        ScrollCaretIntoView;
      end;
    VK_END:
      begin
        key := 0;
        if ctrlDown then
          x := fChunkedText.Count
        else
        begin
          x := GetLineIndexFromChunkIdx(fPageMetrics, fCursorChunkPos.X);
          if x = fPageMetrics.totalLines - 1 then
            x := fChunkedText.Count else
            x := fPageMetrics.startOfLineIdx[x + 1] -1;
        end;
        if x = fChunkedText.Count then
          y := 0 else
          y := fChunkedText[x].length;
        if (x = fCursorChunkPos.X) and (fCursorChunkPos.Y = y) then Exit;
        fCursorChunkPos := Types.Point(x, y);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        ScrollCaretIntoView;
      end;
    else
    begin
      inherited;
      Key := 0;
      Exit;
    end;
  end;
{$ENDIF}
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoDblClick;
var
  chunk: TTextChunk;
begin
  if (fCursorChunkPos.X < 0) or
    (fCursorChunkPos.X >= fChunkedText.Count) then Exit;

  chunk := fChunkedText.Chunk[fCursorChunkPos.X];
  if not chunk.IsText then Exit;
  fSelStart := Types.Point(fCursorChunkPos.X, 0);
  fSelEnd := Types.Point(fCursorChunkPos.X + 1, 0);
  fCursorChunkPos := fSelEnd;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
var
  relPos: TPointD;
begin
  inherited;
  if button <> mbLeft then Exit;
  fMouseLeftDown := true;

  relPos := MakeRelative(PointD(pt));
  fCursorChunkPos := PosToChunkIdx(relPos);

  if Shift = ssShift then
  begin
    fSelEnd := fCursorChunkPos;
  end else
  begin
    fSelStart := fCursorChunkPos;
    fSelEnd := fCursorChunkPos;
    fSavedImage := nil;
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoMouseMove(Shift: TShiftState; const pt: TPoint);
var
  relPos: TPointD;
begin
  if not fMouseLeftDown then Exit;

  relPos := MakeRelative(PointD(pt));
  fCursorChunkPos := PosToChunkIdx(relPos);
  fSelEnd := fCursorChunkPos;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
begin
  fMouseLeftDown := false;
  inherited;
end;
//------------------------------------------------------------------------------

function TMemoCtrl.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := true;
  if (fChunkedText.Count = 0) or (fPageMetrics.lineHeight = 0) then Exit;
  fScrollV.Position := fScrollV.Position -
    (WheelDelta div 120) * fPageMetrics.lineHeight;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TMemoCtrl.GetText: string;
begin
  Result := fChunkedText.Text;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetText(const txt: string);
begin
  if not GetUsableFont then Exit;
  fPageMetrics.bounds := NullRect; // force a fPageMetrics refresh
  fChunkedText.SetText(txt, fUsableFont, fontColor, fColor);
  CheckPageMetrics;
  fTopLine := 0;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetTextMargin(const margin: TPointD);
begin
  if PointsEqual(margin, fTextMargin) then Exit;
  fTextMargin := margin;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TMemoCtrl.ChunkPosToPos(const chunkPos: TPoint): TPointD;
var
  i       : integer;
  x, spcW : double;
  chunk   : TTextChunk;
begin
  Result := NullPointD;
  if not GetUsableFont or not IsValid(chunkPos) then Exit;

  // get chunkPos' start of line index
  i := fTopLine;
  with fPageMetrics do
    if chunkPos.X < startOfLineIdx[i] then
      while (i > 0) and (chunkPos.X < startOfLineIdx[i -1]) do Dec(i)
    else
      while (i < fPageMetrics.totalLines -1) and
        (chunkPos.X >= startOfLineIdx[i + 1]) do Inc(i);

  Result.Y := (i - fTopLine) * fPageMetrics.lineHeight + fTextMargin.Y;

  // get the justify offset for spaces
  spcW := fPageMetrics.justifyDeltas[i];

  // get the start of the word
  i := fPageMetrics.startOfLineIdx[i];
  x := 0;
  while (i < chunkPos.X) do
    with fChunkedText[i] do
    begin
      if IsBlankSpaces then
        x := x + width + spcW else
        x := x + width;
      inc(i);
    end;

  if chunkPos.X >= fChunkedText.Count then
  begin
    with fPageMetrics do
      Result.X := lineWidths[High(lineWidths)];
  end else
  begin
    chunk := fChunkedText[chunkPos.X];
    if chunkPos.Y >= chunk.length then
      x := x + chunk.glyphOffsets[chunk.length -1] else
      x := x + chunk.glyphOffsets[chunkPos.Y];
    Result.X := x +1;
  end;

  Result.X := Result.X + fTextMargin.X;
end;
//------------------------------------------------------------------------------

function TMemoCtrl.PosToChunkIdx(const relPos: TPointD): TPoint;
var
  i: integer;
  x, x2, y2     : double;
  spcW, chrW  : double;
  chunk       : TTextChunk;
begin
  Result := NullPoint;
  if not GetUsableFont or (fPageMetrics.bounds.IsEmpty) then Exit;

  x2 := relPos.X - fTextMargin.X;
  y2 := relPos.Y - fTextMargin.Y;

  i := Trunc(y2 / fPageMetrics.lineHeight);
  inc(i, fTopLIne);
  if i >= fPageMetrics.totalLines then
  begin
    Result.X := fChunkedText.Count;
    Exit;
  end
  else if i < 0 then
    Exit;

  //get start of line
  Result.X := fPageMetrics.startOfLineIdx[i];

  //and also the justify amounts for spaces
  spcW := fPageMetrics.justifyDeltas[i];

  //now offset to the correct word.
  x := 0;
  chunk := nil;
  while (Result.X < fChunkedText.Count) do
  begin
    chunk := fChunkedText[Result.X];
    if chunk.IsNewline then break
    else if chunk.IsBlankSpaces then
      chrW := chunk.width + spcW else
      chrW := chunk.width;

    if x + chrW > x2 then break;
    inc(Result.X);
    x := x + chrW;
  end;

  if (Result.X >= fChunkedText.Count) then
  begin
    Result.X := fChunkedText.Count;
    Exit;
  end;

  if not Assigned(chunk) or (chunk.IsNewline) then Exit;

  if (chunk.IsBlankSpaces) and
    (x2 - x > (chunk.width + spcW) / 2) then inc(Result.X)
  else
    while (Result.Y < High(chunk.glyphOffsets) -1) and
      (x + chunk.glyphOffsets[Result.Y] + (chunk.glyphOffsets[Result.Y + 1] -
        chunk.glyphOffsets[Result.Y]) * 0.5 < x2) do
          inc(Result.Y);
end;
//------------------------------------------------------------------------------

function TMemoCtrl.GetVisibleLines: integer;
begin
  with fPageMetrics do
    if (lineHeight > 0) and (totalLines > 0) then
      Result := Trunc(fTextRect.Height / lineHeight) else
      Result := 0;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetScrollV(scrollCtrl: TScrollCtrl);
begin
  inherited;
  if (StorageState <> ssNormal) or not Assigned(ScrollV) or
    not GetUsableFont or IsEmpty then Exit;
  CheckPageMetrics;
  ScrollV.Step := Round(fPageMetrics.lineHeight);
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoScrollY(newPos: double);
begin
  fTopLine := Round(newPos / fPageMetrics.lineHeight);
  fSavedImage := nil;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.FontChanged;
begin
  inherited;
  fPageMetrics.bounds := NullRect; // force a fPageMetrics refresh
  if not Assigned(fUsableFont) or (fChunkedText.Count = 0) then Exit;
  fChunkedText.ApplyNewFont(fUsableFont);
  Invalidate;
end;
//------------------------------------------------------------------------------

function ClipPathTopAndBot(const clipRect: TRectD; const path: TPathD): TPathD;
var
  i, len: integer;
  y, t, b: double;
begin
  len := Length(path);
  NewPointDArray(Result, len, True);
  t := clipRect.Top;
  b := clipRect.Bottom;
  for i := 0 to len -1 do
  begin
    if path[i].Y < t then y := t
    else if path[i].Y > b then y := b
    else y := path[i].Y;
    result[i] := PointD(path[i].X, y);
  end;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.Paint;
var
  i: integer;
  w: integer;
  p   : TPathD;
  selStartTop, selStartBot, selEndTop, selEndBot: TPointD;
var
  rec: TRectD;
begin
  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  InflateRect(rec, -fBevelHeight, -fBevelHeight);
  Image.Clear(Rect(rec), clWhite32);
  DrawEdge(Image, Rect(rec), clSilver32, clWhite32, fBevelHeight);

  if not GetUsableFont then
  begin
    Image.Clear(Rect(rec), clWhite32);
    DrawEdge(Image, Rect(rec), clSilver32, clWhite32, fBevelHeight);
    Exit;
  end;
  if fPageMetrics.bounds.IsEmpty then
    CheckPageMetrics;

  if Assigned(fSavedImage) and PointsEqual(fSelStart, fSelEnd) then
  begin
    System.Move(fSavedImage[0], Image.Pixels[0],
      Length(fSavedImage) * SizeOf(TColor32));
  end else
  begin

    // DRAW TEXT SELECTION HIGHLIGHT
    if IsValid(fSelStart) and IsValid(fSelEnd)  and
      not PointsEqual(fSelStart, fSelEnd) then
    begin
      if IsCorrectPosOrder(fSelStart, fSelEnd) then
      begin
        selStartTop := ChunkPosToPos(fSelStart);
        selEndTop := ChunkPosToPos(fSelEnd);
      end else
      begin
        selStartTop := ChunkPosToPos(fSelEnd);
        selEndTop := ChunkPosToPos(fSelStart);
      end;

      // it looks better if we offset the selection by a small amount
      selStartTop.Y := selStartTop.Y + fPageMetrics.lineHeight / 10;
      selEndTop.Y := selEndTop.Y + fPageMetrics.lineHeight / 10;

      selStartBot := PointD(selStartTop.X, selStartTop.Y + fPageMetrics.lineHeight);
      selEndBot := PointD(selEndTop.X, selEndTop.Y + fPageMetrics.lineHeight);

      if ValueAlmostZero(selEndTop.Y - selStartTop.Y, 0.01)  then
      begin
        //single line selection
        p := MakePath([selStartTop.X, selStartTop.Y,
          selEndTop.X, selEndTop.Y,
          selEndTop.X, selEndTop.Y + fPageMetrics.lineHeight,
          selStartTop.X, selStartTop.Y + fPageMetrics.lineHeight]);
        p := TranslatePath(p, OuterMargin, OuterMargin);
        p := ClipPathTopAndBot(rec, p);
        DrawPolygon(Image, p, frNonZero, clDefLite32);
        DrawLine(Image, p, 1, GetFocusColor, esClosed);
      end else
      begin
        //multi-line selection - we'll assume (pro tempore) that pt2 is below pt
        w := RectWidth(fPageMetrics.bounds) +7;
        p := Rectangle(selStartTop.X, selStartTop.Y, fTextMargin.X + w, selStartBot.Y);
        p := TranslatePath(p, OuterMargin, OuterMargin);
        p := ClipPathTopAndBot(rec, p);
        DrawPolygon(Image, p, frNonZero, clDefLite32);
        // draw selection outline
        p := MakePath([fTextMargin.X-3, selStartBot.Y,
          selStartTop.X, selStartBot.Y, selStartTop.X, selStartTop.Y,
          fTextMargin.X + w, selStartTop.Y, fTextMargin.X + w, selStartBot.Y]);
        p := TranslatePath(p, OuterMargin, OuterMargin);
        p := ClipPathTopAndBot(rec, p);
        DrawLine(Image, p, 1, GetFocusColor, esButt);
        while (selStartBot.Y + 1 < selEndTop.Y) do
        begin
          selStartTop.Y := selStartBot.Y;
          selStartBot.Y := selStartBot.Y + fPageMetrics.lineHeight;
          p := Rectangle(fTextMargin.X, selStartTop.Y, fTextMargin.X + w, selStartBot.Y);
          p := TranslatePath(p, OuterMargin, OuterMargin);
          p := ClipPathTopAndBot(rec, p);
          DrawPolygon(Image, p, frNonZero, clDefLite32);
          // draw selection outline
          p := MakePath([fTextMargin.X-3, selStartTop.Y, fTextMargin.X-3, selStartBot.Y]);
          p := TranslatePath(p, OuterMargin, OuterMargin);
          DrawLine(Image, p, 1, GetFocusColor, esButt);
          p := MakePath([fTextMargin.X + w, selStartTop.Y, fTextMargin.X +w, selStartBot.Y]);
          p := TranslatePath(p, OuterMargin, OuterMargin);
          DrawLine(Image, p, 1, GetFocusColor, esButt);
        end;
        p := Rectangle(fTextMargin.X, selStartBot.Y,
          selEndTop.X, selStartBot.Y + fPageMetrics.lineHeight);
        p := TranslatePath(p, OuterMargin, OuterMargin);
        p := ClipPathTopAndBot(rec, p);
        DrawPolygon(Image, p, frNonZero, clDefLite32);
        // finish drawing selection outline
        p := MakePath([fTextMargin.X-3, selEndTop.Y, fTextMargin.X-3, selEndBot.Y,
          selEndBot.X, selEndBot.Y, selEndTop.X, selEndTop.Y,
          fTextMargin.X + w, selEndTop.Y]);
        p := TranslatePath(p, OuterMargin, OuterMargin);
        p := ClipPathTopAndBot(rec, p);
        DrawLine(Image, p, 1, GetFocusColor, esButt);
      end;
    end;

    // DRAW THE (VISIBLE) TEXT

    fChunkedText.DrawText(Image, fPageMetrics.bounds,
      taJustify, tvaTop, Types.Point(fPageMetrics.startOfLineIdx[fTopLine], 0));

    // backup the image to facilitate much faster redraws
    SetLength(fSavedImage, Image.Width * Image.Height);
    System.Move(Image.Pixels[0], fSavedImage[0],
      Image.Width * Image.Height * SizeOf(TColor32));
  end;

  if HasFocus then
  begin
    Img32.Vector.InflateRect(rec, -fBevelHeight * 0.5, -fBevelHeight * 0.5);
    DrawLine(Image, Rectangle(rec), fBevelHeight, GetFocusColor, esPolygon);
  end;


  if not IsValid(fCursorChunkPos) then Exit;

  // FINALLY DRAW THE INSERTION POINT CURSOR

  i := Ceil(fPageMetrics.lineHeight * 0.1);
  selStartTop := ChunkPosToPos(fCursorChunkPos);
  selStartBot := PointD(selStartTop.X, selStartTop.Y + fPageMetrics.lineHeight);

  selStartTop.Y := selStartTop.Y + i;
  selStartBot.Y := selStartBot.Y + i;

  selStartTop := TranslatePoint(selStartTop, OuterMargin, OuterMargin);
  selStartBot := TranslatePoint(selStartBot, OuterMargin, OuterMargin);
  DrawLine(Image, selStartTop, selStartBot, fPageMetrics.lineHeight / 15, clMaroon32);
  with selStartTop do
    DrawLine(Image, PointD(X - 4, Y), PointD(X + 4, Y), fPageMetrics.lineHeight / 15, clMaroon32);
  with selStartBot do
    DrawLine(Image, PointD(X - 4, Y), PointD(X + 4, Y), fPageMetrics.lineHeight / 15, clMaroon32);
end;

//------------------------------------------------------------------------------
// TEditCtrl
//------------------------------------------------------------------------------

constructor TEditCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  Color := clWhite32;
  FontColor := clBlack32;
  fChunkedText := TChunkedText.Create;
  BlendFunc  := nil; //assumes edit controls will always be fully opaque.
  fTextMargin := DPIAware(PointD(5, 1));
  fCursorChunkPos := NullPoint;
end;
//------------------------------------------------------------------------------

destructor TEditCtrl.Destroy;
begin
  fChunkedText.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TEditCtrl.GetText: string;
begin
  Result := fChunkedText.Text;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.SetText(const txt: string);
begin
  GetUsableFont;
  fChunkedText.SetText(txt, fUsableFont, fontColor, fColor);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.SetFont(font: TFontCache);
begin
  inherited;
  if not Assigned(fUsableFont) then Exit;
  fChunkedText.ApplyNewFont(font);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoKeyDown(var key: Word; Shift: TShiftState);

  function GetPrev(var curr: TPoint; ctrlDown: Boolean): Boolean;
  begin
    if ctrlDown then
      Result := fChunkedText.GetPrevWord(curr) else
      Result := fChunkedText.GetPrevChar(curr);
  end;

  function GetNext(var curr: TPoint; ctrlDown: Boolean): Boolean;
  begin
    if ctrlDown then
      Result := fChunkedText.GetNextWord(curr) else
      Result := fChunkedText.GetNextChar(curr);
  end;

var
  i         : integer;
  ctrlDown  : Boolean;
  endPos    : TPoint;
  txt       : UnicodeString;
  tmpCT     : TChunkedText;
  ctrl      : TCustomCtrl;
begin
  if not GetUsableFont or not IsValid(fCursorChunkPos) then
  begin
    inherited;
    Exit;
  end;

  ctrlDown := HasCtrlKey(shift);
{$IFDEF MSWINDOWS}
  case key of
    VK_UP, VK_LEFT:
      begin
        GetPrev(fCursorChunkPos, ctrlDown);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        fCursorMoved := true;
        key := 0;
      end;
    VK_DOWN, VK_RIGHT:
      begin
        GetNext(fCursorChunkPos, ctrlDown);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        fCursorMoved := true;
        key := 0;
      end;
    VK_HOME:
      begin
        if fCursorChunkPos <> NullPoint then
        begin
          fCursorChunkPos := NullPoint;
          if HasShiftKey(shift) then
            fSelEnd := fCursorChunkPos else
            ResetSelPos;
          fCursorMoved := true;
        end;
        key := 0;
      end;
    VK_END:
      begin
        endPos := Types.Point(fChunkedText.Count, 0);
        if fCursorChunkPos <> endPos then
        begin
          fCursorChunkPos := endPos;
          if HasShiftKey(shift) then
            fSelEnd := fCursorChunkPos else
            ResetSelPos;
          fCursorMoved := true;
        end;
        key := 0;
      end;
    Ord('C'):
      begin
        if ctrlDown and not PointsEqual(fSelStart, fSelEnd) then
        begin
          if IsCorrectPosOrder(fSelStart, fSelEnd) then
            txt := fChunkedText.GetSubText(fSelStart, fSelEnd) else
            txt := fChunkedText.GetSubText(fSelEnd, fSelStart);
          if SaveTextToClipboard(txt) then
          begin
            ctrl := RootCtrl.FindChildByClass(TStatusbarCtrl) as TCustomCtrl;
            ctrl.Caption := 'Text copied to clipboard.';
          end;
          key := 0;
        end else
          inherited;
      end;
    Ord('X'):
      begin
        if ctrlDown and not PointsEqual(fSelStart, fSelEnd) then
        begin
          if IsCorrectPosOrder(fSelStart, fSelEnd) then
            txt := fChunkedText.GetSubText(fSelStart, fSelEnd) else
            txt := fChunkedText.GetSubText(fSelEnd, fSelStart);
            if SaveTextToClipboard(txt) then
            begin
              ctrl := RootCtrl.FindChildByClass(TStatusbarCtrl) as TCustomCtrl;
              ctrl.Caption := 'Text cut to clipboard.';
            end;
          fCursorChunkPos := fChunkedText.Delete(fSelStart, fSelEnd);
          ResetSelPos;
          key := 0;
        end else
          inherited;
      end;
    Ord('V'):
      begin
        if ctrlDown then
        begin
          txt := LoadUnicodeTextFromClipboard;
          if txt = '' then UnicodeString(LoadUnicodeTextFromClipboard);
          if txt <> '' then
          begin
            tmpCT := TChunkedText.Create(txt, Font, FontColor);
            try
              for i := 0 to tmpCT.Count -1 do
                InsertChunkAtCursor(tmpCT.Chunk[i].text);
            finally
              tmpCT.Free;
            end;

            ctrl := RootCtrl.FindChildByClass(TStatusbarCtrl) as TCustomCtrl;
            ctrl.Caption := 'Text pasted from clipboard.';
          end;
          key := 0;
        end else
          inherited;
      end;
    VK_DELETE:
      begin
        if PointsEqual(fSelStart, fSelEnd) then
        begin
          fSelStart := fCursorChunkPos;
          if fSelStart.X < fChunkedText.Count then
          begin
            fSelEnd := fSelStart;
            fChunkedText.GetNextChar(fSelEnd);
            fCursorChunkPos := fChunkedText.Delete(fSelStart, fSelEnd);
          end;
        end else
        begin
          fCursorChunkPos := fChunkedText.Delete(fSelStart, fSelEnd);
        end;
        ResetSelPos;
        key := 0;
      end;
    VK_BACK:
      begin
        if not PointsEqual(fSelStart, fSelEnd) then
        begin
          fCursorChunkPos := fChunkedText.Delete(fSelStart, fSelEnd);
        end
        else if (fCursorChunkPos.X <= fChunkedText.Count) and
          ((fCursorChunkPos.X > 0) or (fCursorChunkPos.Y > 0)) then
        begin
          fChunkedText.GetPrevChar(fCursorChunkPos);
          fSelStart := fCursorChunkPos;
          fSelEnd   := Types.Point(fSelStart.X, fSelStart.Y +1);
          fCursorChunkPos := fChunkedText.Delete(fSelStart, fSelEnd);
        end;
        ResetSelPos;
        key := 0;
      end
    else
    begin
      inherited;
      Exit;
    end;
  end;
{$ENDIF}
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.ResetSelPos;
begin
  fSelStart := fCursorChunkPos;
  fSelEnd := fCursorChunkPos;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoKeyPress(var chr: Char);
begin
  InsertChunkAtCursor(chr);
  chr := #0;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.InsertChunkAtCursor(const chunkTxt: UnicodeString);

  function PrependText(chunk: TTextChunk): TTextChunk;
  var
    i: integer;
    newTxt: UnicodeString;
  begin
    newTxt := chunkTxt + chunk.Text;
    i := chunk.index;
    fChunkedText.DeleteChunk(i);
    Result := fChunkedText.InsertTextChunk(fUsableFont, i, newTxt);
  end;

  function AppendText(chunk: TTextChunk): TTextChunk;
  var
    i: integer;
    newTxt: UnicodeString;
  begin
    newTxt := chunk.Text + chunkTxt;
    i := chunk.index;
    fChunkedText.DeleteChunk(i);
    Result := fChunkedText.InsertTextChunk(fUsableFont, i, newTxt);
  end;

  function InsertText(chunk: TTextChunk; out pos: TPoint): TTextChunk;
  var
    i               : integer;
    insertingSpaces : Boolean;
    txtBefore       : UnicodeString;
    txtAfter        : UnicodeString;
    newTxt          : UnicodeString;
  begin
    insertingSpaces := chunkTxt[1] = #32;
    if chunk.IsBlankSpaces = insertingSpaces then
    begin
      newTxt := chunk.Text;
      System.Insert(chunkTxt, newTxt, fCursorChunkPos.Y +1);
      i := chunk.index;
      fChunkedText.DeleteChunk(i);
      Result := fChunkedText.InsertTextChunk(fUsableFont, i, newTxt);
      pos := Types.Point(i, fCursorChunkPos.Y + Length(chunkTxt));
    end else
    begin
      newTxt := chunk.Text;
      txtBefore := System.Copy(newTxt, 1, fCursorChunkPos.Y);
      txtAfter := System.Copy(newTxt, fCursorChunkPos.Y +1, MaxInt);
      i := chunk.index;
      fChunkedText.DeleteChunk(i);
      fChunkedText.InsertTextChunk(fUsableFont, i, txtAfter);
      Result := fChunkedText.InsertTextChunk(fUsableFont, i, chunkTxt);
      fChunkedText.InsertTextChunk(fUsableFont, i, txtBefore);
      pos := Types.Point(Result.index +1, 0);
    end;
  end;

var
  chunk: TTextChunk;
  insertingSpaces: Boolean;
begin
  if (chunkTxt = '') or (chunkTxt[1] < #32) then Exit;

  // delete any text selections
  if not PointsEqual(fSelStart, fSelEnd) then
    fCursorChunkPos := fChunkedText.Delete(fSelStart, fSelEnd);

  // 'normalize' fCursorChunkPos.Y so it's never at the end of a chunk
  if (fCursorChunkPos.Y > 0) and
    (fCursorChunkPos.X < fChunkedText.Count) then
  begin
    chunk := fChunkedText[fCursorChunkPos.X];
    if fCursorChunkPos.Y = chunk.length then
    begin
      Inc(fCursorChunkPos.X);
      fCursorChunkPos.Y := 0;
    end;
  end;

  insertingSpaces := chunkTxt[1] = #32;
  if (fCursorChunkPos.X = 0) and (fCursorChunkPos.Y = 0) then
  begin
    chunk := fChunkedText.FirstChunk;
    if not Assigned(chunk) or (insertingSpaces = chunk.IsText) then
    begin
      fChunkedText.InsertTextChunk(fUsableFont, 0, chunkTxt);
      fCursorChunkPos := Types.Point(1, 0);
    end else
    begin
      PrependText(chunk);
      fCursorChunkPos := Types.Point(0, Length(chunkTxt));
    end;
  end
  else if (fCursorChunkPos.X = fChunkedText.Count) then
  begin
    chunk := fChunkedText.LastChunk;
    if not Assigned(chunk) or (insertingSpaces = chunk.IsText) then
    begin
      fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, chunkTxt);
      inc(fCursorChunkPos.X);
    end else
    begin
      chunk := AppendText(chunk);
      fCursorChunkPos := Types.Point(chunk.index +1, 0);
    end;
  end
  else if (fCursorChunkPos.Y = 0) then
  begin
    chunk := fChunkedText[fCursorChunkPos.X];
    if (insertingSpaces = chunk.IsBlankSpaces) then
    begin
      chunk := PrependText(chunk);
      fCursorChunkPos := Types.Point(chunk.index, Length(chunkTxt));
    end else
    begin
      chunk := chunk.Prev;
      chunk := AppendText(chunk);
      fCursorChunkPos := Types.Point(chunk.index +1, 0);
    end
  end else
  begin
    // inserting chunkTxt somewhere inside another chunk
    chunk := fChunkedText[fCursorChunkPos.X];
    InsertText(chunk, fCursorChunkPos);
  end;
  ResetSelPos;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoDblClick;
var
  chunk: TTextChunk;
begin
  if not IsValid(fCursorChunkPos) or
    (fCursorChunkPos.X < 0) or (fCursorChunkPos.X >= fChunkedText.Count) then
      Exit;
  chunk := fChunkedText.Chunk[fCursorChunkPos.X];
  if not chunk.IsText then Exit;
  fSelStart := Types.Point(fCursorChunkPos.X, 0);
  fSelEnd := Types.Point(fCursorChunkPos.X + 1, 0);
  fCursorChunkPos := fSelEnd;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
var
  relPos: TPointD;
begin
  inherited;
  if button <> mbLeft then Exit;
  fMouseLeftDown := true;

  relPos := MakeRelative(PointD(pt));
  fCursorChunkPos := PosToChunkIdx(relPos);

  ResetSelPos;
  if Shift = ssShift then
  begin
    fSelEnd := fCursorChunkPos;
  end else
  begin
    fSelStart := fCursorChunkPos;
    fSelEnd := fCursorChunkPos;
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoMouseMove(Shift: TShiftState; const pt: TPoint);
var
  relPos: TPointD;
begin
  if not fMouseLeftDown then Exit;
  relPos := MakeRelative(PointD(pt));
  fCursorChunkPos := PosToChunkIdx(relPos);
  fSelEnd := fCursorChunkPos;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
begin
  fMouseLeftDown := false;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.SetTextMargin(const margin: TPointD);
begin
  if PointsEqual(margin, fTextMargin) then Exit;
  fTextMargin := margin;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.SetInnerBounds(const newBounds: TRectD);
var
  rec: TRectD;
begin
  rec := newBounds;
  if GetUsableFont then
    rec.Bottom := rec.Top + fUsableFont.LineHeight * 1.1 +
      (fBevelHeight + TextMargin.Y) * 2;
  inherited SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

function TEditCtrl.GetTextRect(stripMargins: Boolean): TRectD;
var
  om: integer;
  bh: double;
begin
  om := Round(OuterMargin);
  Result := RectD(om, om, om + Width, om + Height);
  bh := fBevelHeight;
  if stripMargins then
    InflateRect(Result, -bh - TextMargin.X, -bh - TextMargin.Y) else
    InflateRect(Result, -bh, -bh);

  if assigned(ScrollV) and ScrollV.Visible then
    Result.Right := Result.Right - ScrollV.Width;
  if assigned(ScrollH) and ScrollH.Visible then
    Result.Bottom := Result.Bottom - ScrollH.Height;
end;
//------------------------------------------------------------------------------

function TEditCtrl.ChunkIdxToPos(const chunkIdx: TPoint): TPointD;
var
  i, j, om: integer;
  bh      : double;
  chunk   : TTextChunk;
begin
  Result := NullPointD;
  if not GetUsableFont or not IsValid(chunkIdx) then Exit;

  om := Round(OuterMargin);
  bh := fBevelHeight;
  Result.Y := om + bh + fTextMargin.Y;
  Result.X := om + bh + fTextMargin.X;
  if fChunkedText.Count = 0 then Exit;

  if chunkIdx.X > fChunkedText.Count then
    j := fChunkedText.Count else
    j := chunkIdx.X;

  for i := 0 to j-1 do
    Result.X := Result.X + fChunkedText[i].width;

  if j = fChunkedText.Count then Exit;

  chunk := fChunkedText[j];
  with chunk do
  if chunkIdx.Y > high(glyphOffsets) then
    Result.X := Result.X + glyphOffsets[high(glyphOffsets)] else
    Result.X := Result.X + glyphOffsets[chunkIdx.Y];
end;
//------------------------------------------------------------------------------

function TEditCtrl.PosToChunkIdx(const relPos: TPointD): TPoint;
var
  len     : integer;
  x, d, bh: double;
  textRec : TRectD;
  cwDiv2  : double;
  chunk   : TTextChunk;
begin
  Result  := NullPoint;
  textRec := GetTextRect(false);
  bh      := fBevelHeight;

  d := 0;
  x := relPos.X - fTextMargin.X - bh;

  chunk := nil;
  while (Result.X < fChunkedText.Count) do
  begin
    chunk := fChunkedText[Result.X];
    if d + chunk.width >= x then Break;
    d := d + chunk.width;
    inc(Result.X);
  end;

  if (chunk = nil) or (Result.X = fChunkedText.Count) then Exit;
  d := x - d;
  with chunk do
  begin
    len := high(glyphOffsets);
    cwDiv2 := (glyphOffsets[1] - glyphOffsets[0]) * 0.5;
    while (Result.Y < len) and (d > glyphOffsets[Result.Y] + cwDiv2) do
    begin
      inc(Result.Y);
      cwDiv2 := (glyphOffsets[Result.Y + 1] - glyphOffsets[Result.Y]) * 0.5;
    end;
  end;

  if (Result.Y = len) then
  begin
    inc(Result.X);
    Result.Y := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.FontChanged;
begin
  inherited;
  if not Assigned(fUsableFont) or (fChunkedText.Count = 0) then Exit;
  fChunkedText.ApplyNewFont(font);
  SetInnerBounds(GetInnerBounds);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.Paint;
var
  textRecD, rec: TRectD;
  lh: Double;
  cursorTop, cursorBot: TPointD;
begin
  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  InflateRect(rec, -fBevelHeight, -fBevelHeight);
  Image.Clear(Rect(rec), color);
  DrawEdge(Image, Rect(rec), clSilver32, clWhite32, fBevelHeight);

  if not GetUsableFont then Exit;
  fLineHeight := fUsableFont.LineHeight;

  if HasFocus then
  begin
    DrawShadowRect(Image, Rect(rec), OuterMargin, angle45, clDarkGray32);
    DrawLine(Image, Rectangle(rec), FocusLineWidth, GetFocusColor, esPolygon);

    // draw selection rect ...
    if not PointsEqual(fSelStart, fSelEnd) then
    begin
      rec.Left := ChunkIdxToPos(fSelStart).X;
      rec.Right := ChunkIdxToPos(fSelEnd).X;
      lh := fLineHeight / 5;
      rec.Bottom := rec.Bottom - lh;
      rec.Top := rec.Top + lh;
      DrawPolygon(Image, Rectangle(rec), frNonZero, clDefLite32);
      DrawLine(Image, Rectangle(rec), 1, GetFocusColor, esClosed);
    end;

    lh := fLineHeight / 20; // non-blinking cursor width
    cursorTop := ChunkIdxToPos(fCursorChunkPos);
    cursorBot := TranslatePoint(cursorTop, 0, fLineHeight);
    cursorTop.Y := cursorTop.Y + lh;
    DrawLine(Image, cursorTop, cursorBot, lh, $88AA0000);
  end;
  textRecD := GetTextRect(true);
  fPageMetrics := fChunkedText.DrawText(Image, Rect(textRecD), taLeft, tvaMiddle);
end;

//------------------------------------------------------------------------------
// TButtonCtrl
//------------------------------------------------------------------------------

constructor TButtonCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fPadding  := fBevelHeight;
  Color     := clNone32;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.Clicked;
begin
  if CanSetFocus then SetFocus;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.SetCaption(const text: string);
begin
  inherited;
  Shortcut := GetShortcutFromCaption(text);
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.SetPressed(value: Boolean);
begin
  if (value = fPressed) or not Enabled then Exit;
  fPressed := value;
  if value then Clicked;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if Enabled and CanSetFocus then
  begin
    SetFocus;
    fPressed := true;
    Invalidate;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.DoMouseUp(Button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  Pressed := false;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Visible and Enabled then
  begin
    Key := 0;
    Pressed := true;
  end
  else inherited;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.DoKeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Visible and Enabled then
  begin
    Key := 0;
    Pressed := false;
  end
  else inherited;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.SetPadding(value: double);
begin
  if value = fPadding then Exit;
  fPadding := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.Scale(value: double);
begin
  inherited;
  if AllowRTEdit then
    fPadding := fPadding * value;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.Paint;
var
  bh, hbh: double;
  rec: TRectD;
  P   : TPathD;
  txt: string;
begin
  bh := fBevelHeight;
  hbh := bh / 2;
  rec := InnerRect;
  if Pressed then
    TranslateRect(rec, OuterMargin + bh, OuterMargin + bh) else
    TranslateRect(rec, OuterMargin, OuterMargin);

  if GetUsableFont then txt := fCaption else txt := '';
  Image.Clear;
  Img32.Vector.InflateRect(rec, -hbh, -hbh);
  p := Rectangle(rec);
  if HasFocus then
  begin
    DrawShadowRect(Image, Rect(rec), OuterMargin, angle45, clDarkGray32);
    DrawBtnInternal(Image, p, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, GetColor, GetFontColor);
    DrawLine(Image, Rectangle(rec), FocusLineWidth, GetFocusColor, esPolygon);
  end else
    DrawBtnInternal(Image, Rectangle(rec), txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, GetColor, GetFontColor);
end;

//------------------------------------------------------------------------------
// TRoundedBtnCtrl
//------------------------------------------------------------------------------

procedure TRoundedBtnCtrl.Paint;
var
  bh, hbh: double;
  rec: TRectD;
  p: TPathD;
  txt: string;
begin
  image.Clear;
  if not GetUsableFont then Exit;
  bh := fBevelHeight;
  hbh := bh / 2;
  rec := InnerRect;
  if Pressed then
    TranslateRect(rec, OuterMargin + bh, OuterMargin + bh) else
    TranslateRect(rec, OuterMargin, OuterMargin);
  p := GetRoundedRectPath(rec);


  if GetUsableFont then txt := fCaption else txt := '';
  Image.Clear;
  Img32.Vector.InflateRect(rec, -hbh, -hbh);
  if HasFocus then
  begin
    DrawShadow(Image, p,
      frNonZero, OuterMargin, Angle45, clDarkGray32, true);
    DrawBtnInternal(Image, p, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, GetColor, GetFontColor);
    DrawLine(Image, p, FocusLineWidth, GetFocusColor, esPolygon);
  end else
    DrawBtnInternal(Image, p, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, GetColor, GetFontColor);

  if GetAlpha(GetColor) < 128 then
  begin
    HitTest.htImage.SetSize(Image.Width, Image.Height);
    DrawBtnInternal(HitTest.htImage, p, '',
      nil, bh, fPadding, false, enabled, clWhite32);
  end else
    UpdateHitTestMask;
end;

//------------------------------------------------------------------------------
// TEllipseBtnCtrl
//------------------------------------------------------------------------------

procedure TEllipseBtnCtrl.Paint;
var
  bh, hbh: double;
  rec: TRectD;
  ellip: TPathD;
  txt: string;
begin
  bh := fBevelHeight;
  hbh := bh / 2;
  rec := InnerRect;
  if Pressed then
    TranslateRect(rec, OuterMargin + bh, OuterMargin + bh) else
    TranslateRect(rec, OuterMargin, OuterMargin);

  image.Clear;
  InflateRect(rec, -hbh, -hbh);
  ellip := Ellipse(rec);
  if GetUsableFont then txt := fCaption else txt := '';
  if HasFocus then
  begin
    DrawShadow(Image, ellip,
      frNonZero, OuterMargin, Angle45, clDarkGray32, true);
    DrawBtnInternal(Image, ellip, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, GetColor, GetFontColor);
    DrawLine(Image, ellip, FocusLineWidth, GetFocusColor, esPolygon);
  end else
    DrawBtnInternal(Image, ellip, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, GetColor, GetFontColor);

  if GetAlpha(GetColor) < 128 then
  begin
    HitTest.htImage.SetSize(Image.Width, Image.Height);
    DrawBtnInternal(HitTest.htImage, ellip, '', nil, 0, 0, false, enabled, clWhite32)
  end else
    UpdateHitTestMask;
end;

//------------------------------------------------------------------------------
// TImageBtnCtrl
//------------------------------------------------------------------------------

constructor TImageBtnCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fTextPos := tpBottom;
  fPadding := DPIAware(3);
  Color   := clNone32;
end;
//------------------------------------------------------------------------------

destructor TImageBtnCtrl.Destroy;
begin
  SetImgList(nil);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TImageBtnCtrl.Paint;
var
  rec     : TRectD;
  bh      : double;
  pDelta  : double;
  ilImage : TImage32;
  pad     : integer;
begin
  Image.Clear;
  bh := fBevelHeight;
  if pressed and (bh = 0) then
    bh := DPIAware(1.5);

  pad := Round(fPadding);
  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);

  //if HasFocus and not fPressed then
  if HasFocus then
  begin
    DrawShadowRect(Image, Rect(rec), OuterMargin, angle45, clDarkGray32);
    if (bh > 0) then
      DrawBtnInternal(Image, Rectangle(rec), '', nil,
        bh, fPadding, fPressed, enabled, GetColor, clNone32);
    DrawLine(Image, Rectangle(rec), FocusLineWidth, GetFocusColor, esPolygon);
  end
  else if ( bh > 0) then
    DrawBtnInternal(Image, Rectangle(rec), '', nil,
      bh, fPadding, fPressed, enabled, GetColor, clNone32);

  if not GetUsableFont then Exit;
  if fPressed then pDelta := bh else pDelta := 0;


  if Assigned(fImgList) and (fImgIdx >=0) and (fImgIdx < fImgList.Count) then
    ilImage := fImgList.CreateImage(fImgIdx) else
    ilImage := nil;
  try
    //nb: padding is around edges, not between image and text.
    case fTextPos of
      tpLeft:
        begin
          if assigned(ilImage) then
          begin
            rec.Left := rec.Right -bh - ilImage.Width -pad + pDelta;
            rec.Top := rec.Top +(Height -ilImage.Height) / 2 +pDelta;
            rec.Right := rec.Left + ilImage.Width + pDelta;
            rec.Bottom := rec.Top + ilImage.Height;
            Image.CopyBlend(ilImage, ilImage.Bounds, Rect(rec), BlendToAlpha);
            rec.Right := rec.Left - pad;
            rec.Left := OuterMargin + pad + bh + pDelta * 2;
          end else
          begin
            InflateRect(rec, -pad, -pad);
            TranslateRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, rec, fCaption, fUsableFont, GetFontColor, taLeft, tvaMiddle);
        end;
      tpTop:
        begin
          if assigned(ilImage) then
          begin
            rec.Top := rec.Bottom -bh - ilImage.Height - pad + pDelta;
            rec.Left := rec.Left + (Width - ilImage.Width) / 2 + pDelta;
            rec.Bottom := rec.Top + ilImage.Height + pDelta;
            rec.Right := rec.Left + ilImage.Width;
            Image.CopyBlend(ilImage, ilImage.Bounds, Rect(rec), BlendToAlpha);
            rec.Bottom := rec.Top - pad;
            rec.Top := +OuterMargin + pad + pDelta * 2;
          end else
          begin
            InflateRect(rec, -pad, -pad);
            TranslateRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, rec, fCaption, fUsableFont, GetFontColor, taCenter, tvaTop);
        end;
      tpRight:
        begin
          if assigned(ilImage) then
          begin
            rec.Left := rec.Left +pad +pDelta;
            rec.Right := rec.Left +ilImage.Width;
            rec.Top := rec.Top +(Height -ilImage.Height) / 2 + pDelta;
            rec.Bottom := rec.Top + ilImage.Height;
            Image.CopyBlend(ilImage, ilImage.Bounds, Rect(rec), BlendToAlpha);
            rec.Left := rec.Right + pad + pDelta;
            rec.Right := Image.Width - OuterMargin - pad -bh * 2 + pDelta;
          end else
          begin
            InflateRect(rec, -pad - bh, -pad - bh);
            TranslateRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, rec, fCaption, fUsableFont, GetFontColor, taRight, tvaMiddle);
        end;
      tpBottom:
        begin
          if assigned(ilImage) then
          begin
            rec.Top := rec.Top + pad + pDelta;
            rec.Bottom := rec.Top + ilImage.Height;
            rec.Left := rec.Left + (Width -ilImage.Width) / 2 + pDelta;
            rec.Right := rec.Left + ilImage.Width;
            Image.CopyBlend(ilImage, ilImage.Bounds, Rect(rec), BlendToAlpha);
            rec.Top := rec.Bottom + pad + pDelta;
            rec.Bottom := Image.Height - OuterMargin - pad + pDelta - bh * 3;
          end else
          begin
            InflateRect(rec, -pad, -pad);
            TranslateRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, rec, fCaption, fUsableFont, GetFontColor, taCenter, tvaBottom);
        end;
    end;
  finally
    FreeAndNil(ilImage);
  end;
end;
//------------------------------------------------------------------------------

procedure TImageBtnCtrl.SetTextPos(value: TTextPosition);
begin
  if value = fTextPos then Exit;
  fTextPos := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImageBtnCtrl.SetImgList(svgImageList: TSvgImageList32);
begin
  if svgImageList = fImgList then Exit;
  fImgList := svgImageList;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImageBtnCtrl.SetImgIdx(index: integer);
begin
  if index = fImgIdx then Exit;
  fImgIdx := index;
  Invalidate;
end;

//------------------------------------------------------------------------------
// TCheckboxCtrl
//------------------------------------------------------------------------------

constructor TCheckboxCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fPadding    := DPIAware(8);
  fTextPos    := tphRight;
  fAutoState  := true;
  Color       := clNone32;
  fTriState   := tsUnknown;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if CanSetFocus then SetFocus;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and CanSetFocus then
  begin
    Key := 0;
    case State of
      tsUnknown : State := tsNo;
      tsYes     : State := tsNo;
      tsNo      : State := tsYes;
    end;
  end
  else inherited;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.DoKeyUp(var Key: Word; Shift: TShiftState);
begin
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.SetTriState(state: TTriState);
begin
  if state =  fTriState then Exit;
  fTriState := state;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.Clicked;
begin
  if fAutoState then
  begin
    if fTriState = tsNo then
      State := tsYes else
      State := tsNo;
    Invalidate;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.Paint;
var
  i       : integer;
  j       : double;
  bh, dx  : double;
  rec     : TRectD;
  pp      : TPathsD;
  caption : string;
begin
  image.Clear(GetColor);
  if not GetUsableFont then Exit;

  caption := fCaption;
  i := Pos('&', caption);
  if i > 0 then Delete(caption, i, 1);

  bh := fBevelHeight;
  j := fUsableFont.LineHeight - bh;
  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  rec.Bottom := rec.Top + j;
  TranslateRect(rec, 0, (Height - j) / 2);
  if fTextPos = tphRight then
    rec.Right := rec.Left + j else
    rec.Left := rec.Right - j;

  if HasFocus then
    DrawShadowRect(Image, Rect(rec), OuterMargin, angle45, clDarkGray32);

  if fTextPos = tphRight then
  begin
    DrawCheckboxCtrl(Image, Rect(rec),
      bh, fTriState, StorageManager.DesignScale, GetFocusColor, enabled);
    if HasFocus then
      DrawLine(Image, Rectangle(rec), FocusLineWidth, GetFocusColor, esPolygon);
    rec := InnerRect;
    TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Left := rec.Left + j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(rec, caption,
      taLeft, tvaMiddle, Shortcut.chrIdx);
    dx := fUsableFont.LineHeight / 24;
    pp := TranslatePath(pp, -dx, -dx);
    if GetLuminance(GetFontColor) < 160 then
      DrawPolygon(Image, pp, frNonZero, clWhite32) else
      DrawPolygon(Image, pp, frNonZero, clBlack32);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, GetFontColor);
  end else
  begin
    DrawCheckboxCtrl(Image, Rect(rec),
      bh, fTriState, StorageManager.DesignScale, GetFocusColor, enabled);
    if HasFocus then
      DrawLine(Image, Rectangle(rec), FocusLineWidth, GetFocusColor, esPolygon);

    rec := InnerRect;
    TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Right := rec.Right - j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(rec, fCaption, taRight,
      tvaMiddle, Shortcut.chrIdx);

    dx := fUsableFont.LineHeight / 24;
    pp := TranslatePath(pp, -dx, -dx);

    if GetLuminance(GetFontColor) < 160 then
      DrawPolygon(Image, pp, frNonZero, clWhite32) else
      DrawPolygon(Image, pp, frNonZero, clBlack32);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, GetFontColor);
  end;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.SetTextPos(value: TTextPositionH);
begin
  if value = fTextPos then Exit;
  fTextPos := value;
  Invalidate;
end;

//------------------------------------------------------------------------------
// TRadioBtnCtrl
//------------------------------------------------------------------------------

procedure TRadioBtnCtrl.Paint;
var
  i         : integer;
  j, bh, dx : double;
  rec       : TRectD;
  p         : TPathD;
  pp        : TPathsD;
  caption   : string;
begin
  image.Clear(GetColor);
  if not GetUsableFont then Exit;

  caption := fCaption;
  i := Pos('&', caption);
  if i > 0 then Delete(caption, i, 1);

  bh := fBevelHeight;
  j := fUsableFont.LineHeight - bh;
  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  rec.Bottom := rec.Top + j;
  TranslateRect(rec, 0, (Height-j) / 2);
  if fTextPos = tphRight then
    rec.Right := rec.Left + j else
    rec.Left := rec.Right - j;
  p := Ellipse(rec);

  if HasFocus then
    DrawShadow(Image, p, frNonZero, OuterMargin, Angle45, clDarkGray32, true);

  if fTextPos = tphRight then
  begin
    DrawRadioCtrl(Image, p, GetFocusColor, bh, enabled, fTriState,
      StorageManager.DesignScale);
    if HasFocus then
      DrawLine(Image, p, FocusLineWidth, GetFocusColor, esPolygon);
    rec := InnerRect;
    TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Left := rec.Left + j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);
    pp := fUsableFont.GetTextOutline(rec, caption, taLeft,
      tvaMiddle, Shortcut.chrIdx);
  end else
  begin
    //rec.Left := rec.Right - j;
    DrawRadioCtrl(Image, p, GetFocusColor, bh, enabled, fTriState,
      StorageManager.DesignScale);
    if HasFocus then
      DrawLine(Image, p, FocusLineWidth, GetFocusColor, esPolygon);
    rec := InnerRect;
    TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Right := rec.Right - j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);
    pp := fUsableFont.GetTextOutline(rec, caption, taRight,
      tvaMiddle, Shortcut.chrIdx);
  end;

  dx := fUsableFont.LineHeight / 24;
  pp := TranslatePath(pp, -dx, -dx);
  if GetLuminance(GetFontColor) < 160 then
    DrawPolygon(Image, pp, frNonZero, clWhite32) else
    DrawPolygon(Image, pp, frNonZero, clBlack32);
  pp := TranslatePath(pp, dx, dx);
  DrawPolygon(Image, pp, frNonZero, GetFontColor);
end;

//------------------------------------------------------------------------------
// TPanelCtrl
//------------------------------------------------------------------------------

constructor TPanelCtrl.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited;
  fBevelHeight := 0;
  fInnerMargin := dpiAware(10);
  fShadowAngle := angle45;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.RecalcScrollMax;
var
  i     : integer;
  x, y  : double;
begin
  if not Assigned(ScrollH) and not Assigned(ScrollV) then
    Exit;

  // todo - consider control visability test
  x := 0; y := 0;
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) then
      with TCustomCtrl(Child[i]) do
        if AutoPosition = apNone then
        begin
          if Left + Width > x then x := Left + Width;
          if Top + Height > y then y := Top + Height;
        end;

  if Assigned(ScrollH) then
    ScrollH.Max := ScrollOffset.X + x + {Right} Margin;
  if Assigned(ScrollV) then
    ScrollV.Max := ScrollOffset.Y + y + {Bottom} Margin;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  CheckScrollOffset(newBounds);
  inherited;
  RecalcScrollMax;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.CheckScrollOffset(const newBounds: TRectD);
var
  i: integer;
  dx, dy: double;
  oldBounds: TRectD;
begin
  if (fScrollOffset.X > 0) or (fScrollOffset.Y > 0) then
  begin
    oldBounds := GetInnerBounds;
    if (fScrollOffset.X > 0) and (newBounds.Width > oldBounds.Width) then
      dx := Min(fScrollOffset.X, newBounds.Width - oldBounds.Width) else
      dx := 0;
    if (fScrollOffset.Y > 0) and (newBounds.Height > oldBounds.Height) then
      dy := Min(fScrollOffset.Y, newBounds.Height - oldBounds.Height) else
      dy := 0;
    if (dx > 0) or (dy > 0) then
    begin
      fScrollOffset.X := fScrollOffset.X - dx;
      if fScrollOffset.X = 0 then ScrollH.fPos := 0;
      fScrollOffset.Y := fScrollOffset.Y - dy;
      if fScrollOffset.Y = 0 then ScrollV.fPos := 0;

      for i := 0 to ChildCount -1 do
        if (Child[i] is TCustomCtrl) and
          (TCustomCtrl(Child[i]).AutoPosition = apNone) then
            Child[i].Offset(dx, dy);
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.DoScrollX(newPos: double);
var
  i: integer;
  dx: double;
begin
  dx := newPos - ScrollOffset.X;
  ScrollOffset := PointD(newPos, ScrollOffset.Y);
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) and
      (TCustomCtrl(Child[i]).fAutoPosition = apNone) then
        Child[i].Offset(-dx, 0);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.DoScrollY(newPos: double);
var
  i: integer;
  dy: double;
begin
  dy := newPos - ScrollOffset.Y;
  ScrollOffset := PointD(ScrollOffset.X, newPos);
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) and
      (TCustomCtrl(Child[i]).fAutoPosition = apNone) then
        Child[i].Offset(0, -dy);
  Invalidate;
end;

//------------------------------------------------------------------------------
// TPageCtrl
//------------------------------------------------------------------------------

constructor TPageCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fActiveIdx := -1;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  inherited;
  ResizeTabs;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.Scale(value: double);
begin
  inherited;
  if AllowRTEdit then
  begin
    fTabHeight := fTabHeight * value;
    fTabWidth  := fTabWidth  * value;
    ResizeTabs;
  end;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.SetTabWidth(width: double);
begin
  if width = fTabWidth then Exit;
  fTabWidth := Max(0, width);
  if (StorageState <> ssLoading)  then
    ResizeTabs;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.SetTabHeight(height: double);
begin
  if height = fTabHeight then Exit;
  fTabHeight := Max(0, height);
  if (StorageState <> ssLoading)  then
    ResizeTabs;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.SetActiveIndex(index: integer);
var
  i, cnt: integer;
  pagePnl: TPagePnlCtrl;
begin
  cnt := ChildCount div 2;
  if index >= cnt then index := cnt -1;
  if index = fActiveIdx then Exit;
  if fActiveIdx < 0 then fActiveIdx := index;

  if cnt > 0 then
  begin
    if index <> fActiveIdx then
      Child[fActiveIdx * 2 + 1].Visible := false;
    pagePnl := Child[index * 2 + 1] as TPagePnlCtrl;
    pagePnl.Visible := true;

    if Assigned(RootCtrl.FocusedCtrl) then
      RootCtrl.FocusedCtrl.KillFocus;

    for i := 0 to pagePnl.ChildCount -1 do
      if (pagePnl[i] is TCustomCtrl) and
        TCustomCtrl(pagePnl[i]).CanSetFocus then
      begin
        TCustomCtrl(pagePnl[i]).SetFocus;
        Break;
      end;
  end;
  fActiveIdx := index;
  if Assigned(OnClick) then OnClick(self);

  Invalidate;
end;
//------------------------------------------------------------------------------

function TPageCtrl.GetPagePanel(index: integer): TPagePnlCtrl;
begin
  if (index < 0) or (index >= ChildCount div 2) then
    Result := nil else
    Result := TPagePnlCtrl(Child[index * 2 + 1]);
end;
//------------------------------------------------------------------------------

function TPageCtrl.ActivePage: TPagePnlCtrl;
begin
  Result := GetPagePanel(fActiveIdx);
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.DrawTab(idx: integer);
var
  h       : integer;
  lh, dx  : double;
  cap     : string;
  rec     : TRectD;
  rec2    : TRectD;
  p       : TPathD;
  p2      : TPathD;
  pp      : TPathsD;
  sc      : TShortcut;
  bkClr   : TColor32;
  txtClr  : TColor32;
begin
  lh := font.LineHeight;
  if fTabHeight = 0 then
    h := Ceil(fUsableFont.LineHeight * 1.33) else
    h := Round(fTabHeight);

  if idx = fActiveIdx then
  begin
    bkClr := GetColor;
    txtClr := GetFontColor;
    rec.Top := OuterMargin + 1;
    rec.Bottom := rec.Top + h + BevelHeight - 1;
  end else
  begin
    bkClr := clLiteBtn32;
    txtClr := clDarkGray32;
    rec.Top := BevelHeight + OuterMargin;
    rec.Bottom := rec.Top + h;
  end;
  rec.Left := BevelHeight + OuterMargin + fTabOffsets[idx];
  rec.Right := BevelHeight + OuterMargin + fTabOffsets[idx + 1];

  p := GetTabOutLine(rec, lh / 3);
  DrawPolygon(Image, p, frNonZero, bkClr);
  DrawEdge(Image, p, clWhite32, clSilver32, BevelHeight, false);

  if idx < fActiveIdx then
    p := TranslatePath(p, -1, -1)
  else if idx > fActiveIdx then
    p := TranslatePath(p, 1, -1)
  else
  begin
    p := TranslatePath(p, 0, -1);
    rec.Top := rec.Top - BevelHeight; // offsets caption
  end;
  DrawLine(Image, p, 1.0, clGray32, esButt);

  cap := TCustomCtrl(Child[idx * 2]).Caption;
  sc := GetShortcutFromCaption(cap);
  if sc.chrIdx > 0 then
    cap := DeleteChar(cap, sc.chrIdx);
  if sc.chrIdx > 0 then
    caption := DeleteChar(caption, sc.chrIdx);
  pp := font.GetTextOutline(rec, cap, taCenter, tvaMiddle, sc.chrIdx);
  dx := font.LineHeight / 24;
  if GetLuminance(txtClr) < 160 then
  begin
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, txtClr);
  end else
  begin
    DrawPolygon(Image, pp, frNonZero, clBlack32);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, txtClr);
  end;

  if idx <> fActiveIdx then Exit;
  rec2 := GetClientRect;
  TranslateRect(rec2, OuterMargin, OuterMargin);
  rec2.Top := rec2.Top + h;
  Image.Clear(Rect(rec2), bkClr);
  with rec2 do
    p2 := MakePath([p[High(p)].X, p[High(p)].Y, Right, Top, Right, Bottom,
      Left, Bottom, Left, Top, p[0].X, p[0].Y]);
  DrawEdge(Image, p2, clWhite32, clSilver32, BevelHeight, false);
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.DrawTabs;
var
  i, len : integer;
begin
  len := ChildCount div 2;
  if (len = 0) or not GetUsableFont then Exit;
  if Length(fTabOffsets) = 0 then
    ResizeTabs;

  Image.Clear(clNone32);
  for i := 0 to fActiveIdx -1 do DrawTab(i);
  for i := len -1 downto fActiveIdx +1 do DrawTab(i);
  DrawTab(fActiveIdx);
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.ResizeTabs;
var
  i, len    : integer;
  h,bh      : double;
  tabs      : TArrayOfString;
begin
  len := ChildCount div 2;
  SetLength(tabs, len);
  if (len = 0) or not GetUsableFont then Exit;
  for i := 0 to len -1 do
    tabs[i] := TCustomCtrl(Child[i * 2]).Caption;
  if fTabHeight = 0 then
    h := Ceil(fUsableFont.LineHeight * 1.33) else
    h := fTabHeight;
  bh := fBevelHeight;
  if fTabWidth > 0 then
    fTabOffsets := MakeCumulativeArray(BevelHeight, fTabWidth, len) else
    fTabOffsets := GetVariableWidthTabOffsets(tabs, fUsableFont, BevelHeight);

  for i := 0 to len - 1 do
  begin
    Child[i * 2].SetInnerBounds(RectD(fTabOffsets[i], 0, fTabOffsets[i + 1], h));
    Child[i * 2 + 1].SetInnerBounds(RectD(bh, h + bh * 2, width - bh, height - bh));
  end;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.Paint;
var
  rec: TRect;
  bhi: integer;
  om: integer;
begin
  Image.Clear;
  if not GetUsableFont or (ChildCount = 0) then Exit;
  bhi := Round(fBevelHeight);
  om := Round(OuterMargin);
  rec := Rect(0, 0, Ceil(Width),Ceil(Height));
  if (om > 0) then
    TranslateRect(rec, om, om);
  Img32.Vector.InflateRect(rec, -bhi, -bhi);
  if Assigned(fOnPaint) then fOnPaint(Self);
  DrawTabs;
  UpdateHitTestMask;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.AddTab(const caption: string);
begin
  AddTabs([caption]);
  ResizeTabs;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.AddTabs(const captions: TArrayOfString);
var
  i, len: integer;
begin
  len := Length(captions);
  if len = 0 then Exit;

  for i := 0 to len -1 do
  begin
    with TPageTabCtrl(AddChild(TPageTabCtrl, '')) do
      Caption := captions[i];
    with AddChild(TPagePnlCtrl, '') as TPagePnlCtrl do
    begin
      Shortcut := GetShortcutFromCaption(captions[i]);
      Visible := False;
    end;
  end;
  if (fActiveIdx < 0) and (ChildCount > 2) then
    SetActiveIndex(0);
  Panel[fActiveIdx].Visible := true;
  ResizeTabs;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.ClearTabs;
begin
  ClearChildren;
  fActiveIdx := 0;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
var
  i: integer;
begin
  if (Key = VK_TAB) and HasCtrlKey(shift) then
  begin
    if HasShiftKey(shift) then
    begin
      if fActiveIdx = 0 then
        i := ChildCount div 2 -1 else
        i := fActiveIdx -1;
    end else
    begin
      if fActiveIdx = ChildCount div 2 -1 then
        i := 0 else
        i := fActiveIdx +1;
    end;
    SetActiveIndex(i);
    Key := 0;
  end else
    inherited;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.EndRead;
var
  i: integer;
begin
  inherited;
  //in case a page other than the first page is visible
  for i := 1 to ChildCount div 2 -1 do
    if Panel[i].Visible then
    begin
      ActiveIndex := i;
      Break;
    end;
  if (ChildCount > 1) and (ActiveIndex < 0) then
    ActiveIndex := 0;
  ResizeTabs;
end;

//------------------------------------------------------------------------------
// TPagePnlCtrl
//------------------------------------------------------------------------------

constructor TPagePnlCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fAutoPosition := apClient;
  fCanFocus := false;
  fBevelHeight := 0;
  fColor := clNone32;
  fShadowSize := 0;
  OuterMargin  := 0;
end;
//------------------------------------------------------------------------------

procedure TPagePnlCtrl.Clicked;
begin
  //ie from a shortcut
  if Parent is TPageCtrl then
    TPageCtrl(Parent).SetActiveIndex(Index div 2);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPagePnlCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
var
  i, idx: integer;
begin
  if (Key = VK_TAB) and not HasCtrlKey(shift) then
  begin
    if HasShiftKey(shift) then
    begin
      if Assigned(RootCtrl.FocusedCtrl) then
        idx := RootCtrl.FocusedCtrl.Index else
        idx := 0;

      for i := idx-1 downto 0 do
        if Child[i] is TCustomCtrl then
          with TCustomCtrl(Child[i]) do
            if CanSetFocus then
            begin
              SetFocus;
              Key := 0;
              Exit;
            end;
      for i := ChildCount -1 downto idx do
        if Child[i] is TCustomCtrl then
          with TCustomCtrl(Child[i]) do
            if CanSetFocus then
            begin
              SetFocus;
              Key := 0;
              Exit;
            end;
    end else
    begin
      if Assigned(RootCtrl.FocusedCtrl) then
        idx := RootCtrl.FocusedCtrl.Index else
        idx := ChildCount -1;

      for i := idx +1 to ChildCount -1 do
        if Child[i] is TCustomCtrl then
          with TCustomCtrl(Child[i]) do
            if CanSetFocus then
            begin
              SetFocus;
              Key := 0;
              Exit;
            end;
      for i := 0 to idx do
        if Child[i] is TCustomCtrl then
          with TCustomCtrl(Child[i]) do
            if CanSetFocus then
            begin
              SetFocus;
              Key := 0;
              Exit;
            end;
    end;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPagePnlCtrl.DoKeyUp(var Key: Word; Shift: TShiftState);
begin
end;
//------------------------------------------------------------------------------

procedure TPagePnlCtrl.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
var
  i: integer;
begin
  inherited;
  if Assigned(RootCtrl) and Assigned(RootCtrl.fFocusedCtrl) and
    RootCtrl.fFocusedCtrl.IsOwnedBy(TCustomCtrl(Parent)) then Exit;
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) then
      with TCustomCtrl(Child[i]) do
      if CanSetFocus then
      begin
        SetFocus;
        Break;
      end;
end;

//------------------------------------------------------------------------------
// TPageTabCtrl
//------------------------------------------------------------------------------

constructor TPageTabCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fCanFocus := false;
end;
//------------------------------------------------------------------------------

procedure TPageTabCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  TPageCtrl(parent).SetActiveIndex(Index div 2);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPageTabCtrl.Clicked;
begin
  TPageCtrl(parent).SetActiveIndex(Index div 2);
  inherited;
end;

//------------------------------------------------------------------------------
// TProgressCtrl
//------------------------------------------------------------------------------

constructor TProgressCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fFontColor := clBlack32;
  fOrientation  := soHorizontal;
  if GetUsableFont then
    fSize   := Round(fUsableFont.LineHeight * 1.1) else
    fSize   := DPIAware(15);
  fMax := 100;
  fCanFocus := false;
  fStartColor := clDefMid32;
  fEndColor := clDefMid32;
end;
//------------------------------------------------------------------------------

procedure TProgressCtrl.FontChanged;
begin
  inherited;
  if not GetUsableFont then Exit;
  fSize := Round(fUsableFont.LineHeight * 1.1);
  SetInnerBounds(InnerBounds);
end;
//------------------------------------------------------------------------------

procedure TProgressCtrl.Paint;
var
  bh : integer;
  c   : TColor32;
  rec : TRectD;
  rec2: TRect;
begin
  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  rec2 := Rect(rec);
  Hatch(Image, rec2, clWhite32, clBtnFace32, DPIAware(3));

  //draw the outer border
  DrawEdge(Image, rec, clSilver32, clWhite32, fBevelHeight);
  //draw the progress

  c := GradientColor(fStartColor, fEndColor, Position / fMax);
  case fOrientation of
    soUnknown: Exit;
    soHorizontal:
      rec2.Right := rec2.Left + Round(Position / fMax * Width);
    soVertical:
      rec2.Bottom := rec2.top + Round(Position / fMax * Height);
  end;
  bh := Round(fBevelHeight);
  Types.InflateRect(rec2, -bh, -bh);
  Image.FillRect(rec2, c);

  if GetUsableFont then
    DrawText(Image, rec, Floattostr(Position),
      fUsableFont, fFontColor, taCenter, tvaMiddle);
end;
//------------------------------------------------------------------------------

procedure TProgressCtrl.SetInnerBounds(const newBounds: TRectD);
var
  rec : TRectD;
begin
  rec := newBounds;
  case fOrientation of
    soHorizontal  : rec.Bottom := rec.Top + fSize;
    soVertical    : rec.Right := rec.Left + fSize;
  end;
  inherited SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

procedure TProgressCtrl.SetMax(newMax: integer);
begin
  fMax := Abs(newMax);
end;
//------------------------------------------------------------------------------

procedure TProgressCtrl.SetPosition(newPos: double);
begin
  fPosition := Math.Min(fMax, Math.Max(0, newPos));
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TProgressCtrl.SetOrientation(newOrient: TScrollOrientation);
begin
  if (fOrientation = newOrient) or (newOrient = soUnknown) then Exit;
  fOrientation := newOrient;
  Image.SetSize(Round(Height), Round(Width));
  Invalidate;
end;

//------------------------------------------------------------------------------
// TSliderCtrl
//------------------------------------------------------------------------------

constructor TSliderCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fOrientation  := soHorizontal;
  if GetUsableFont then
  begin
    fSize     := fUsableFont.LineHeight * 1.1;
    SetBtnSize;
  end else
  begin
    fSize     := DPIAware(15);
    fBtnSize  := Round(fSize * 2);
  end;
  fMax          := 100;
  fScrollStep   := DPIAware(5);
end;
//------------------------------------------------------------------------------

function TSliderCtrl.GetDelta: double;
begin
  //Delta = real step size for one unit of position change
  case fOrientation of
    soHorizontal  : result := (Width - fBtnSize) / (fMax - fMin);
    soVertical    : result := (Height - fBtnSize) / (fMax - fMin);
    else Result := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetInnerBounds(const newBounds: TRectD);
var
  rec : TRectD;
begin
  rec := newBounds;
  case fOrientation of
    soHorizontal  : rec.Bottom := rec.Top + fSize;
    soVertical    : rec.Right := rec.Left + fSize;
  end;
  inherited SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.FontChanged;
begin
  inherited;
  if not GetUsableFont then Exit;
  SetBtnSize;
  fSize := Round(fUsableFont.LineHeight * 1.1);
  SetInnerBounds(InnerBounds);
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.Paint;
var
  hbh    : integer;
  d     : double;
  d2    : double;
  rec   : TRectD;
  recI  : TRect;
begin
  Image.Clear;
  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  recI := Rect(rec);
  Hatch(Image, recI, clWhite32, clBtnFace32, DPIAware(3));

  //draw the outer border
  DrawEdge(Image, recI, clSilver32, clWhite32, BevelHeight);

  //draw the button
  d := GetDelta;
  case fOrientation of
    soUnknown: Exit;
    soHorizontal:
     begin
      d2 := rec.Left + (fPosition - fMin) * d;
      rec := RectD(d2, rec.Top, d2 + fBtnSize, rec.Top + Height + BevelHeight);
     end;
    soVertical:
      begin
        d2 := rec.Top + (fPosition - fMin) * d;
        rec := RectD(rec.Left, d2, rec.Left + Width + BevelHeight, d2 + fBtnSize);
      end;
  end;
  if HasFocus then
    DrawShadowRect(Image, Rect(rec), OuterMargin, angle45, clDarkGray32);
  Image.FillRect(Rect(rec), GetColor);
  DrawLine(Image, Rectangle(rec), dpiAware1 + 1, clWhite32, esPolygon);

  Img32.Vector.InflateRect(rec, -dpiAwareOne, -dpiAwareOne);
  if fPressed then
    DrawEdge(Image, rec, clBlack32, clWhite32, dpiAwareOne);
    DrawEdge(Image, rec, clWhite32, clBlack32, dpiAwareOne);

  if HasFocus then
  begin
    hbh := Round(fBevelHeight / 2);
    recI := Rect(rec);
    Types.InflateRect(recI, hbh, hbh);
    DrawLine(Image, Rectangle(recI), FocusLineWidth * 1.5, GetFocusColor, esPolygon);
  end;


  if not GetUsableFont then Exit;
  if fPressed then
    TranslateRect(rec, DPIAwareOne, DPIAwareOne);
  DrawText(Image, rec, Floattostr(fPosition),
    fUsableFont, GetFontColor, taCenter, tvaMiddle);
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetOrientation(newOrient: TScrollOrientation);
begin
  if (fOrientation = newOrient) or (newOrient = soUnknown) then Exit;
  fOrientation := newOrient;
  SetInnerBounds(InnerBounds);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetPosition(newPos: double);
begin
  fPosition := Math.Min(fMax, Math.Max(fMin, newPos));
  Invalidate;
  if Assigned(fOnSlide) then fOnSlide(Self);
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetPressed(value: Boolean);
begin
  if fPressed = value then Exit;
  fPressed := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetBtnSize;
begin
  if GetUsableFont then
    fBtnSize := Round(fUsableFont.GetTextWidth(format(' %1.0f ', [fMax])));
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
var
  d : double;
  pt2: TPointD;
begin
  if not CanSetFocus then Exit;
  SetFocus;

  pt2 := MakeRelative(PointD(pt));
  d := GetDelta * (fPosition-fMin);
  case fOrientation of
    soUnknown: Exit;
    soHorizontal:
      begin
        if pt2.X < d then
        begin
          Position := Position - Step;
          fScrollState := scLeft;
        end
        else if pt2.X > d + fBtnSize then
        begin
          Position := Position + Step;
          fScrollState := scRight;
        end else
        begin
          fScrollState := scScrollBtn;
          fPressed := true;
          Invalidate;
        end;
      end;
    soVertical:
      begin
        if pt2.Y < d then
        begin
          Position := Position - Step;
          fScrollState := scTop;
        end
        else if pt2.Y > d + fBtnSize then
        begin
          Position := Position + Step;
          fScrollState := scBottom;
        end else
        begin
          fScrollState := scScrollBtn;
          fPressed := true;
          Invalidate;
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.DoMouseMove(shift: TShiftState; const pt: TPoint);
var
  pt2: TPointD;
  d, halfBtnSize: double;
begin
  if (fScrollState = scNormal) then Exit;
  halfBtnSize := fBtnSize / 2;
  pt2 := MakeRelative(PointD(pt));
  d := GetDelta;
  if d = 0 then Exit;

  case fOrientation of
    soUnknown: Exit;
    soHorizontal:
      begin
        case fScrollState of
          scLeft:
            if pt2.X > d * (fPosition - fMin) then
              fScrollState := scScrollBtn else
              Position := Round(fPosition - Step);
          scRight:
            if pt2.X < d * (fPosition - fMin) + fBtnSize then
              fScrollState := scScrollBtn else
              Position := Round(fPosition + Step);
        end;
        if fScrollState = scScrollBtn then
          Position := Round((pt2.X - halfBtnSize)  / d) + fMin;
      end;
    soVertical:
      begin
        case fScrollState of
          scTop:
            if pt2.Y > d * (fPosition - fMin) then
              fScrollState := scScrollBtn else
              Position := Round(fPosition - Step);
          scBottom:
            if pt2.Y < d * (fPosition - fMin) + fBtnSize then
              fScrollState := scScrollBtn else
              Position := Round(fPosition + Step);
        end;
        if fScrollState = scScrollBtn then
          Position := Round((pt2.Y - halfBtnSize) / d) + fMin;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.DoMouseUp(Button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if fPressed then
  begin
    fPressed := false;
    Invalidate;
  end;
  fScrollState := scNormal;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT, VK_UP:
      begin
        Position := Round(fPosition - Step);
        Key := 0;
      end;
    VK_RIGHT, VK_DOWN:
      begin
        Position := Round(fPosition + Step);
        Key := 0;
      end;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetMin(newMin: double);
begin
  fMin := Math.Min(newMin, fMax);
  if fPosition < fMin then
    fPosition := fMin;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetMax(newMax: double);
begin
  fMax := Math.Max(fMin, newMax);
  if fPosition > fMax then
    fPosition := fMax;
  SetBtnSize;
  Invalidate;
end;

//------------------------------------------------------------------------------
// TScrollCtrl
//------------------------------------------------------------------------------

constructor TScrollCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fCanFocus     := false;
  fSize         := 0;
  fOrientation  := soVertical;
  fMax          := 100;
  fHiddenSize   := 0;
  fMinBtnSize   := DPIAware(10);
  fAutoHide     := True;
  fScrollStep   := dpiAware(5);
  fScrollRatio  := 1;
  OuterMargin   := 0;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetParent(parent: TStorage);
begin
  inherited;
  if Assigned(Parent) and (Parent is TScrollingCtrl) then
    fTargetCtrl := TScrollingCtrl(Parent) else
    fTargetCtrl := nil;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetOrientation(orientation  : TScrollOrientation);
begin
  fOrientation := orientation;
  if not Assigned(fTargetCtrl) or (orientation = soUnknown) then Exit;
  if orientation = soHorizontal then
  begin
    if Assigned(fTargetCtrl.fScrollH) then
      Raise Exception.Create('oops - horizontal scroll already assigned.');
    fTargetCtrl.SetScrollH(self);
  end else
  begin
    if Assigned(fTargetCtrl.fScrollV) then
      Raise Exception.Create('oops - vertical scroll already assigned.');
    fTargetCtrl.SetScrollV(self);
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetFocus;
begin
  if Assigned(fTargetCtrl) and fTargetCtrl.CanSetFocus then
    fTargetCtrl.SetFocus;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.DoAutoPosition;
var
  rec: TRectD;
begin
  if (StorageState <> ssNormal) or not Assigned(fTargetCtrl) or
    fTargetCtrl.IsEmpty or (fOrientation = soUnknown) then Exit;
  if (Size = 0) then SetDefaultSize;
  rec := fTargetCtrl.ClientRect;
  if fOrientation = soVertical then
  begin
    fAutoPosition := apRight;
    if Visible then rec.Right := rec.Right + Size;
    rec.Left := rec.Right - Size;
  end else
  begin
    fAutoPosition := apBottom;
    if Visible then rec.Bottom := rec.Bottom + Size;
    rec.Top := rec.Bottom - Size;
  end;
  SetInnerBounds(rec);
  if not RectsEqual(Rect(rec), Rect(InnerBounds)) then
    SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetMax(newMax: double);
begin
  if (newMax < 0) then newMax := 0;
  fMax := newMax;
  DoAutoPosition;
  GetScrollBtnInfo;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetDefaultSize;
begin
  if GetUsableFont then
    fSize := fUsableFont.LineHeight * 0.8 else
    fSize := 0;
  if fOrientation = soVertical then
    Width := fSize else
    Height := fSize;
end;
//------------------------------------------------------------------------------

function TScrollCtrl.GetChevronSize: Double;
begin
  if (fSize = 0) then SetDefaultSize;
  Result := fSize;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.CheckAutoVisible(firstPass: Boolean);
begin
  if not Assigned(fTargetCtrl) or
    not AutoHide or (fOrientation = soUnknown) then Exit;
  if fOrientation = soVertical then
  begin
    if firstPass and Assigned(fTargetCtrl.fScrollH) then
      fTargetCtrl.fScrollH.CheckAutoVisible(false);
    Visible := fMax > fTargetCtrl.ClientHeight;
  end else
  begin
    if firstPass and Assigned(fTargetCtrl.fScrollV) then
      fTargetCtrl.fScrollV.CheckAutoVisible(false);
    Visible := fMax > fTargetCtrl.ClientWidth;
  end;
  if Visible and (fSize = 0) then SetDefaultSize;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.GetScrollBtnInfo;
var
  dummy1, dummy2: double;
begin
  GetScrollBtnInfo(dummy1, dummy2);
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.GetScrollBtnInfo(out btnStart, btnEnd: double);
var
  trackSize, chevSize: double;
begin
  btnStart := 0; btnEnd := 0; trackSize := 0;
  fBtnSize := 0; // error value
  if not Assigned(fTargetCtrl) or (fMax = 0) then Exit;

  try
    chevSize := GetChevronSize;
    case fOrientation of
      soUnknown: Exit;
      soHorizontal:
        begin
          fHiddenSize := fMax - fTargetCtrl.ClientWidth;
          trackSize := ClientWidth - chevSize * 2;
        end;
      soVertical:
        begin
          fHiddenSize := fMax - fTargetCtrl.ClientHeight;
          trackSize := ClientHeight - chevSize * 2;
        end;
    end;

    if (fHiddenSize <= 0) or (trackSize + DPIAware(10) < fMinBtnSize) then
      Exit;

    fBtnSize := Math.Max(fMinBtnSize, trackSize - fHiddenSize);
    fScrollRatio := fHiddenSize / (trackSize - fBtnSize);
    // nb: fScrollRatio = 1.0 UNLESS fBtnSize = fMinBtnSize

    if fPos > fHiddenSize then fPos := fHiddenSize;

    btnStart := chevSize + fPos / fScrollRatio;
    btnEnd := btnStart + fBtnSize;
  finally
    if AutoHide then Visible := (fBtnSize > 0);
    if not Visible then fPos := 0;
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetScrollSize(newSize: double);
begin
  if (fSize <> newSize) then
  begin
    fSize := newSize;
    case fOrientation of
      soVertical: Width := fSize;
      soHorizontal: Height := fSize;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetBtnPos(newPos: double);
begin
  if not Assigned(fTargetCtrl) or not CanScroll then Exit;
  if newPos > fHiddenSize then newPos := fHiddenSize
  else if newPos < 0 then newPos := 0;
  if (newPos = fPos) then Exit;
  fPos := newPos;
  case fOrientation of
    soVertical: fTargetCtrl.DoScrollY(fPos);
    soHorizontal: fTargetCtrl.DoScrollX(fPos);
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetAutoHide(value: Boolean);
begin
  if value = fAutoHide then Exit;
  fAutoHide := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetScrollStep(value: integer);
begin
  fScrollStep := value;
end;
//------------------------------------------------------------------------------

function TScrollCtrl.GetScrollStep: integer;
begin
  Result := Round(fScrollStep);
end;
//------------------------------------------------------------------------------

function TScrollCtrl.CanScroll: Boolean;
begin
  Result := Visible and (fMax > 0);
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
var
  ss, se: double;
begin
  fMousePos := MakeRelative(PointD(pt));
  GetScrollBtnInfo(ss, se);
  case fOrientation of
    soUnknown: Exit;
    soHorizontal:
      begin
        if fMousePos.X < ss then
        begin
          Position := Position - Step;
          fScrollState := scLeft;
        end
        else if fMousePos.X > se then
        begin
          Position := Position + Step;
          fScrollState := scRight;
        end else
        begin
          fScrollState := scScrollBtn;
        end;
      end;
    soVertical:
      begin
        if fMousePos.Y < ss then
        begin
          Position := Position - Step;
          fScrollState := scTop;
        end
        else if fMousePos.Y > se then
        begin
          Position := Position + Step;
          fScrollState := scBottom;
        end else
        begin
          fScrollState := scScrollBtn;
        end;
      end;
  end;
  if Assigned(fTargetCtrl) then fTargetCtrl.SetFocus;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.DoMouseMove(shift: TShiftState; const pt: TPoint);
var
  ss, se: double;
  pt2: TPointD;

  function CheckRange(newPos: double): double;
  begin
    if newPos > fHiddenSize then Result := fHiddenSize { + minor adjust??}
    else if newPos < 0 then Result := 0
    else Result := newPos;
  end;

begin
  if (fScrollState = scNormal) or not Assigned(fTargetCtrl) then Exit;
  GetScrollBtnInfo(ss, se);
  pt2 := MakeRelative(PointD(pt));

  case fOrientation of
    soUnknown: Exit;
    soHorizontal:
    begin
      if (fScrollState <> scScrollBtn) and
        PtInRect(RectD(ss, 0, se, Height), pt2) then
          fScrollState := scScrollBtn;
      case fScrollState of
        scScrollBtn : Position :=
          CheckRange(fPos + ((pt2.X - fMousePos.X) * fScrollRatio));
        scLeft      : Position := CheckRange(fPos - Step);
        scRight     : Position := CheckRange(fPos + Step);
      end;
    end;
    soVertical:
    begin
      if (fScrollState <> scScrollBtn) and
        PtInRect(RectD(0, ss, Width, se), pt2) then
          fScrollState := scScrollBtn;
      case fScrollState of
        scScrollBtn : Position :=
          CheckRange(fPos + ((pt2.Y - fMousePos.Y) * fScrollRatio));
        scTop       : Position := CheckRange(fPos - Step);
        scBottom    : Position := CheckRange(fPos + Step);
      end;
    end;
  end;
  fMousePos := pt2;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.DoMouseUp(Button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  fScrollState := scNormal;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.Scale(value: double);
begin
  fSize := fSize * value;
  case fOrientation of
    soUnknown: Exit;
    soVertical: Width := fSize;
    soHorizontal: Height := fSize;
  end;
  Max := Max * value;
  fMinBtnSize := fMinBtnSize * value;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.Paint;
var
  bhi     : integer;
  d, bh   : double;
  ss, se  : double;
  bhDiv2  : double;
  rec     : TRectD;
  tmpRec  : TRectD;
  p,p2    : TPathD;
  drawBtn : Boolean;
begin
  bh := fBevelHeight;
  bhi := Round(bh);
  bhDiv2 := bhi / 2;

  drawBtn := Assigned(fTargetCtrl) and (fMax > fTargetCtrl.ClientHeight);
  if drawBtn then GetScrollBtnInfo(ss, se);

  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  Hatch(Image, Rect(rec), clWhite32, clBtnFace32, DPIAware(3));

  tmpRec := rec;
  case fOrientation of
    soUnknown: Exit;
    soVertical:
    begin
      //draw the top chevron
      tmpRec.Bottom := tmpRec.Top + GetChevronSize;
      Image.FillRect(Rect(tmpRec), clBtnFace32);
      d := tmpRec.Width * 0.3;
      InflateRect(tmpRec, -d, -d);
      with tmpRec do
        p := MakePath([left-2, bottom, MidPoint.X, top + 2, right + 2, bottom]);
      p2 := TranslatePath(p, -bhDiv2, -bhDiv2);
      DrawLine(Image, p2, bhi, clWhite32, esRound);
      DrawLine(Image, p, bhi, clDarkGray32, esRound);

      //draw the bottom chevron
      tmpRec := rec;
      tmpRec.Top := tmpRec.Bottom - GetChevronSize;
      Image.FillRect(Rect(tmpRec), clBtnFace32);
      InflateRect(tmpRec, -d, -d);
      with tmpRec do
        p := MakePath([left - 2, top, MidPoint.X, bottom - 2, right + 2, top]);
      p2 := TranslatePath(p, -bhDiv2, -bhDiv2);
      DrawLine(Image, p2, bhi, clWhite32, esRound);
      DrawLine(Image, p, bhi, clDarkGray32, esRound);

      //draw the outer border
      DrawEdge(Image, rec, clSilver32, clWhite32, bh);

      if not drawBtn then Exit;

      //position and draw the button
      Img32.Vector.InflateRect(rec, -1, -1);
      rec.Top := rec.Top + ss;
      rec.Bottom := rec.Top + fBtnSize;
      rec.Left := rec.Left + 1 + bhDiv2;
      rec.Right := rec.Right -1;
      Image.FillRect(Rect(rec), clPaleGray32);

      DrawLine(Image, Rectangle(rec), dpiAware1 + 1, clWhite32, esPolygon);
      Img32.Vector.InflateRect(rec, -1, -1);
      DrawEdge(Image, rec, clWhite32, clBlack32, 1);
    end;
    soHorizontal:
    begin
      //draw the left chevron
      tmpRec.Right := tmpRec.Left + GetChevronSize;
      Image.FillRect(Rect(tmpRec), clBtnFace32);
      d := tmpRec.Height * 0.3;
      InflateRect(tmpRec, -d, -d);
      with tmpRec do
        p := MakePath([right, top, left + 2, MidPoint.Y + 1, right, bottom + 2]);
      p2 := TranslatePath(p, -bhDiv2, -bhDiv2);
      DrawLine(Image, p2, bhi, clWhite32, esRound);
      DrawLine(Image, p, bhi, clDarkGray32, esRound);

      //draw the right chevron
      tmpRec := rec;
      tmpRec.Left := tmpRec.Right - GetChevronSize;
      Image.FillRect(Rect(tmpRec), clBtnFace32);
      InflateRect(tmpRec, -d, -d);
      with tmpRec do
        p := MakePath([left, top, right -2, MidPoint.Y + 1, left, bottom + 2]);
      p2 := TranslatePath(p, -bhDiv2, -bhDiv2);
      DrawLine(Image, p2, bhi, clWhite32, esRound);
      DrawLine(Image, p, bhi, clDarkGray32, esRound);

      //draw the outer border
      DrawEdge(Image, rec, clSilver32, clWhite32, bh);

      if not drawBtn then Exit;

      //position and draw the button
      Img32.Vector.InflateRect(rec, -1, -1);
      rec.Left := rec.Left + ss;
      rec.Right := rec.Left + fBtnSize;
      rec.Top := rec.Top + bhDiv2 + 1;
      Image.FillRect(Rect(rec), clPaleGray32);
      DrawLine(Image, Rectangle(rec), dpiAware1 + 1, clWhite32, esPolygon);
      Img32.Vector.InflateRect(rec, -1, -1);
      DrawEdge(Image, rec, clWhite32, clBlack32, 1);
    end;
  end;
end;

//------------------------------------------------------------------------------
// TEventPropertyHandler
//------------------------------------------------------------------------------

constructor TEventPropertyHandler.Create(parent:  TStorage = nil; const name: string = '');
begin
  inherited;
  fAutoFree := true;
  GetNotifyEvents;
end;
//------------------------------------------------------------------------------

destructor TEventPropertyHandler.Destroy;
begin
  if fAutoFree then
    DeRegisterProperties;
  Inherited;
end;
//------------------------------------------------------------------------------

procedure TEventPropertyHandler.GetNotifyEvents;
var
  context: TRttiContext;
  aType: TRttiType;
  i,j: integer;
  methods: TArray<TRttiMethod>;
  params: TArray<TRttiParameter>;
begin
  aType := context.GetType(self.ClassType);
  methods := (aType as TRttiInstanceType).GetDeclaredMethods;
  SetLength(fNotifyEvents, Length(methods));
  j := 0;
  for i := 0 to High(methods) do
  begin
    //make sure this method is a procedure
    if Assigned(methods[i].ReturnType) then continue;
    //and make sure this procedure has a single class parameter
    params := methods[i].GetParameters;
    if (Length(params) <> 1) or
      (params[0].ParamType.TypeKind <> tkClass) then Continue;
    fNotifyEvents[j].name := methods[i].Name;
    fNotifyEvents[j].address := methods[i].CodeAddress;
    inc(j);
  end;
  SetLength(fNotifyEvents, j);
end;
//------------------------------------------------------------------------------

procedure TEventPropertyHandler.RegisterProperties;
var
  context: TRttiContext;
  aType: TRttiType;
  i, j: integer;
  properties: TArray<TRttiProperty>;
begin
  // This gets both public and published properties
  aType := context.GetType(self.ClassType);
  properties := (aType as TRttiInstanceType).GetDeclaredProperties;
  SetLength(fObjectProps, Length(properties));
  j := 0;
  for i := 0 to High(properties) do
  begin
    if (properties[i].PropertyType.TypeKind <> tkClass) or
      not properties[i].IsReadable or
      not properties[i].IsWritable then Continue;
    fObjectProps[j].name := properties[i].name;
    fObjectProps[j].address := properties[i].GetValue(self).AsObject;
    inc(j);
  end;
  SetLength(fObjectProps, j);
end;
//------------------------------------------------------------------------------

procedure TEventPropertyHandler.DeRegisterProperties;
var
  i: integer;
begin
  for i := 0 to High(fObjectProps) do
    FreeAndNil(TObject(fObjectProps[i].address));
  fObjectProps := nil;
end;
//------------------------------------------------------------------------------

procedure TEventPropertyHandler.Scale(delta: double);
begin
  // override in descendant classes
end;
//------------------------------------------------------------------------------

function TEventPropertyHandler.GetEventCount: integer;
begin
  Result := Length(fNotifyEvents);
end;
//------------------------------------------------------------------------------

function TEventPropertyHandler.GetPropCount: integer;
begin
  Result := Length(fObjectProps);
end;
//------------------------------------------------------------------------------

function TEventPropertyHandler.GetNotify(const name: string): TNotifyEvent;
var
  i: integer;
  method: TMethod;
begin
  for i := 0 to High(fNotifyEvents) do
    if SameText(name, fNotifyEvents[i].name) then
    begin
      method.Code := fNotifyEvents[i].address;
      method.Data := Self;
      Result := TNotifyEvent(method);
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TEventPropertyHandler.GetPropObj(const name: string): TObject;
var
  i: integer;
begin
  for i := 0 to High(fObjectProps) do
    if SameText(name, fObjectProps[i].name) then
    begin
      Result := fObjectProps[i].address;
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

function TEventPropertyHandler.GetEventName(event: TNotifyEvent): string;
var
  i: integer;
begin
  for i := 0 to High(fNotifyEvents) do
    if TMethod(event).Code = fNotifyEvents[i].address then
    begin
      Result := fNotifyEvents[i].name;
      Exit;
    end;
  Result := '';
end;
//------------------------------------------------------------------------------

function TEventPropertyHandler.GetPropName(prop: TObject): string;
var
  i: integer;
begin
  for i := 0 to High(fObjectProps) do
    if Pointer(prop) = fObjectProps[i].address then
    begin
      Result := fObjectProps[i].name;
      Exit;
    end;
  Result := '';
end;

//------------------------------------------------------------------------------
// TCtrlStorageManager
//------------------------------------------------------------------------------

constructor TCtrlStorageManager.Create(parent: TStorage; const name: string);
begin
  inherited;
  fShortcutList := TList.Create;
  fLayeredImg := TLayeredImage32.Create(self, 'TLayeredImage32');
  fRootCtrl   := fLayeredImg.InsertChild(0 , TRootCtrl) as TRootCtrl;
  fRootCtrl.CanFocus := false;
  RootCtrl.Theme := lightTheme;
{$IFDEF MSWINDOWS}
  DesignResolution := GetScreenResolution;
{$ENDIF}
end;
//------------------------------------------------------------------------------

destructor TCtrlStorageManager.Destroy;
begin
  fShortcutList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.Quit;
begin
{$IFDEF MSWINDOWS}
  if self.fMainHdl > 0 then
  begin
    DestroyWindow(fMainHdl);
    fMainHdl := 0;
  end;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.InsertChild(index: integer; storeClass: TStorageClass): TStorage;
begin
  if storeClass.InheritsFrom(TEventPropertyHandler) then
  begin
    if (Child[0] is TEventPropertyHandler) then
      Result := Child[0] else
      Result := inherited InsertChild(0, storeClass);
    fEventHandler := TEventPropertyHandler(Result);
    RegisterStorageClass(storeClass);
  end else
    Raise Exception.Create('TCtrlStorageManager error');
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.GetExternalProp(const str: string; out success: Boolean): TObject;
begin
  if Assigned(fEventHandler) then
    Result := fEventHandler.GetPropObj(str) else
    Result := nil;
  success := Assigned(Result);
end;
//------------------------------------------------------------------------------

function SkipBlanks(var c: PUTF8Char; endC: PUTF8Char): Boolean;
begin
  while (c < endC) and (c^ <= space) do inc(c);
  Result := (c < endC);
end;
//------------------------------------------------------------------------------

function MakeUTF8String(var c: PUTF8Char; endC: PUTF8Char): UTF8String;
var
  len: integer;
begin
  len := endC - c;
  SetLength(Result, len);
  if len = 0 then Exit;
  Move(c^, Result[1], len * SizeOf(UTF8Char));
  c := endC;
end;
//------------------------------------------------------------------------------

function GetWord(var c: PUTF8Char; endC: PUTF8Char; out word: Utf8String): Boolean;
var
  len: integer;
  c2: PUTF8Char;
begin
  c2 := c;
  inc(c);
  while (c < endC) and (c^ > space) do inc(c);
  len := c-c2;
  SetLength(word, len);
  Result := len > 0;
  if Result then Move(c2^, word[1], len * SizeOf(UTF8Char));
end;
//------------------------------------------------------------------------------

function GetName(var c: PUTF8Char; endC: PUTF8Char): string;
var
  c2: PUTF8Char;
begin
  c2 := c;
  if (c < endC) and
    (((c^ >= 'A') and (c^ <= 'Z')) or
    ((c^ >= 'a') and (c^ <= 'z'))) then
  begin
    inc(c);
    while (c < endC) and
      (((c^ >= 'A') and (c^ <= 'Z')) or
      ((c^ >= 'a') and (c^ <= 'z')) or
      ((c^ >= '0') and (c^ <= '9'))) do inc(c);
    Result := string(MakeUTF8String(c2, c));
  end else
    Result := '';
end;
//------------------------------------------------------------------------------

function GetNextChar(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  Result := #0;
  if not SkipBlanks(c, endC) then Exit;
  Result := c^;
  inc(c);
end;
//------------------------------------------------------------------------------

function GetQuote(var c: PUTF8Char; endC: PUTF8Char): UTF8Char;
begin
  if SkipBlanks(c, endC) and (c^ in [squote, dquote]) then
  begin
    Result := c^;
    inc(c);
  end else
    Result := #0;
end;
//------------------------------------------------------------------------------

function GetAttribValue(var c: PUTF8Char; endC: PUTF8Char): Utf8String;
var
  quoteChar : UTF8Char;
  c2: PUTF8Char;
begin
  Result := '';
  if GetNextChar(c, endC) <> '=' then Exit;
  quoteChar := GetQuote(c, endC);
  if quoteChar = #0 then Exit;
  c2 := c;
  while (c < endC) and (c^ <> quoteChar) do inc(c);
  Result := MakeUTF8String(c2, c);
  inc(c); //skip end quote
end;
//------------------------------------------------------------------------------

function IsCommentStart(c, endC: PUTF8Char): Boolean;
begin
  Result := False;
  if (c^ <> '<') or (endC - c < 7) then Exit;
  inc(c);
  if (c^ <> '!') then Exit;
  inc(c);
  if (c^ <> '-') then Exit;
  inc(c);
  if (c^ <> '-') then Exit;
end;
//------------------------------------------------------------------------------

function SkipComment(var c: PUTF8Char; endC: PUTF8Char): Boolean;
var
  str: Utf8String;
begin
  inc(c, 4);
  while SkipBlanks(c, endC) and
    GetWord(c, endC, str) and (str <> '-->') do;
  Result := (str = '-->');
end;
//------------------------------------------------------------------------------

function CheckSkipComment(var xmlCurr, xmlEnd: PUTF8Char): Boolean;
begin
  Result := IsCommentStart(xmlCurr, xmlEnd);
  if not Result then Exit;
  if not SkipComment(xmlCurr, xmlEnd) then xmlCurr := xmlEnd;
  SkipBlanks(xmlCurr, xmlEnd);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.AddDelayedLink(const delayedLink: TLoadRec);
var
  len: integer;
begin
  len := Length(fDelayedLinks);
  SetLength(fDelayedLinks, len +1);
  Move(delayedLink.loadObj, fDelayedLinks[len].loadObj, SizeOf(TLoadRec));
end;
//------------------------------------------------------------------------------

type
  TGetProtectedStorage = class(TStorage);

procedure TCtrlStorageManager.LoadStoredObjects(const utf8: UTF8String);

  function ParseStorageElement(stgObj: TStorage;
    var xmlCurr: PUTF8Char; xmlEnd: PUTF8Char): Boolean;
  var
    i         : integer;
    attName   : string;
    attVal    : string;
    clssName  : string;
    stgClass  : TStorageClass;
    child     : TStorage;
    target    : TStorage;
  begin
    Result := False; // assume error
    // get ATTRIBUTES
    while SkipBlanks(xmlCurr, xmlEnd) do
    begin
      if CheckSkipComment(xmlCurr, xmlEnd) then Continue;
      if (xmlCurr^ < 'A') then Break;
      attName := string(GetName(xmlCurr, xmlEnd));
      if (attName = '') then Break;
      attVal := string(GetAttribValue(xmlCurr, xmlEnd));
      if Assigned(stgObj) then
        TGetProtectedStorage(stgObj).ReadProperty(attName, attVal);
    end;
    if (xmlCurr^ <> '>') then Exit; // error
    inc(xmlCurr);

    // get obj CONTENT, either a child element or the element's closure
    while (xmlCurr <= xmlEnd) do
    begin
      if not SkipBlanks(xmlCurr, xmlEnd) or (xmlCurr^ <> '<') then Exit;
      if CheckSkipComment(xmlCurr, xmlEnd) then Continue;
      inc(xmlCurr); // '<'

      if (xmlCurr^ = '/') then // element closure
      begin
        inc(xmlCurr);
        SkipBlanks(xmlCurr, xmlEnd);
        clssName := GetName(xmlCurr, xmlEnd);
        if Assigned(stgObj) and not SameText(stgObj.ClassName, clssName) or
          not SkipBlanks(xmlCurr, xmlEnd) or (xmlCurr^ <> '>') then Exit;
        inc(xmlCurr); // '>'
        Break; //
      end;

      // get next child
      clssName := GetName(xmlCurr, xmlEnd);
      stgClass := GetStorageClass(clssName);

      if (clssName = 'TLayeredImage32') then
        child := fLayeredImg
      else if (clssName = 'TGroupLayer32') and (fLayeredImg.ChildCount < 2) then
        child := fLayeredImg.Root
      else if (clssName = 'TRootCtrl') and (fLayeredImg.ChildCount < 3) then
        child := RootCtrl
      else if Assigned(stgClass) then
      begin
        if stgClass.InheritsFrom(TEventPropertyHandler) then
        begin
          if not Assigned(EventAndPropertyHandler) then Exit;
          child := EventAndPropertyHandler;
        end else
          child := stgObj.AddChild(stgClass);
      end
      else
        Exit; // error

      child.StorageState := ssLoading;
      try
        if not ParseStorageElement(child, xmlCurr, xmlEnd) then Exit;
      finally
        child.StorageState := ssNormal;
      end;
    end;

    // join up parent-child controls (often TScrollCtrl)
    // while postponing other linkages until all controls have been loaded
    if (stgObj is TCustomCtrl) then
      with TCustomCtrl(stgObj) do
      begin
        for i := 0 to High(fDelayedLinks) do
          with fDelayedLinks[i] do
          begin
            target := FindChildByLoadId(self, targetId);
            if Assigned(target) then
              SetObjectProp(loadObj, propName, target)
            else
              self.AddDelayedLink(fDelayedLinks[i]);
          end;
        fDelayedLinks := nil;
      end;

    // finally process EndRead
    TGetProtectedStorage(stgObj).EndRead;
    Result := True;
  end;

var
  i: integer;
  xmlCurr, xmlEnd : PUTF8Char;
  savedDecSep     : Char;
  target          : TStorage;
begin
  if (utf8 = '') then Exit;
  savedDecSep := {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
  try
    {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    xmlCurr := PUTF8Char(utf8);
    xmlEnd := xmlCurr;
    inc(xmlEnd, Length(utf8));

    StorageState := ssLoading;
    try
      //get top storage element
      if not SkipBlanks(xmlCurr, xmlEnd) or
        (xmlCurr^ <> '<') then Exit;

      Inc(xmlCurr);
      if (GetName(xmlCurr, xmlEnd) <> self.ClassName) then Exit;
      StorageState := ssLoading;
      try
        ParseStorageElement(self, xmlCurr, xmlEnd);
      finally
        StorageState := ssNormal;
      end;
    finally
      StorageState := ssNormal;
    end;

    if fMainHdl > 0 then 
      ResizeMainWindow(mainHdl, fLayeredImg.Width, fLayeredImg.Height);

    // join up remaining unresolved linkages
    for i := 0 to High(fDelayedLinks) do
      with fDelayedLinks[i] do
      begin
        target := FindChildByLoadId(fRootCtrl, targetId);
        if Assigned(target) then
          SetObjectProp(self, propName, target);
      end;

  finally
    fDelayedLinks := nil;
    {$IFDEF FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator := savedDecSep;
  end;
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.FindChildByLoadId(parent: TStorage; id: integer): TStorage;

  function FindChild(so: TStorage): TStorage;
  var
    i: integer;
  begin
    // unfortunately we have to do this the slow way because
    // TScrollCtrl objects can change their order during construction.
    if id = so.Id then
    begin
      Result := so;
    end else
    begin
      Result := nil;
      for i := so.ChildCount -1 downto 0 do
      begin
        Result := FindChild(so.Child[i]);
        if Assigned(Result) then Break
      end;
    end;
  end;

begin
  Result := FindChild(parent);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.BeginRead;
begin
  if Assigned(RootCtrl) then
  begin
    RootCtrl.ClearChildren;
    RootCtrl.fFocusedCtrl := nil;
    RootCtrl.Theme := lightTheme;
  end;
  fLastCtrl := nil;
  fMseDownCtrl := nil;
  fCurrCursor := 0;
  fShortcutList.Clear;
  Designing := False;
  fAllowResize := False;
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.GetExternalPropName(obj: TObject): string;
begin
  if Assigned(fEventHandler) then
    Result := fEventHandler.GetPropName(obj) else
    Result := '';
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.GetExternalEvent(const method: TMethod; out textId: string): Boolean;
var
  i: integer;
begin
  if Assigned(fEventHandler) then
    with fEventHandler do
    begin
      for i := 0 to High(fNotifyEvents) do
        if method.Code = fNotifyEvents[i].address then
        begin
          textId := fNotifyEvents[i].name;
          Result := True;
          Exit;
        end;
    end;
  Result := false;
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.FindShortcutOwner(const aShortcut: TShortcut): TCustomCtrl;
var
  i: integer;
begin
  for i := 0 to fShortcutList.Count -1 do
    with TCustomCtrl(fShortcutList[i]) do
    begin
      if (fShortcut.flags <> aShortcut.flags) or
        (fShortcut.chr <> aShortcut.chr) then Continue;
      Result := TCustomCtrl(fShortcutList[i]);
      if not Result.IsEnabledToCtrlRoot then Continue;
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.RemoveShortcutOwner(ctrl: TCustomCtrl);
var
  i: integer;
begin
  for i := 0 to fShortcutList.Count -1 do
    if fShortcutList[i] = ctrl then
    begin
      fShortcutList.Delete(i);
      Break;
    end;
end;
//------------------------------------------------------------------------------

type THackedLayer32 = class(TLayer32);

function TCtrlStorageManager.GetRepaintReq: Boolean;
begin
  Result := Assigned(fLayeredImg) and
    Assigned(fLayeredImg.Root) and
      THackedLayer32(fLayeredImg.Root).UpdateNeeded;
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.GetFocusedCtrl: TCustomCtrl;
begin
  Result := fRootCtrl.fFocusedCtrl;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.SetFocusedCtrl(ctrl: TCustomCtrl);
begin
  if Assigned(fRootCtrl.fFocusedCtrl) then
    fRootCtrl.fFocusedCtrl.KillFocus;
  if Assigned(ctrl) and ctrl.CanSetFocus then
    fRootCtrl.fFocusedCtrl := ctrl else
    fRootCtrl.fFocusedCtrl := nil;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.MouseDown(mouseButton: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if not Assigned(fLayeredImg) then Exit;
  if Assigned(RootCtrl.fCaptureCtrl) then
  begin
    RootCtrl.fCaptureCtrl.DoMouseDown(mouseButton, shift, pt);
  end else
  begin
    fMseDownCtrl := fLayeredImg.GetLayerAt(pt) as TCustomCtrl;
    if not Assigned(fMseDownCtrl) then Exit;
    fMseDownCtrl.DoMouseDown(mouseButton, shift, pt);
    fLastCtrl := fMseDownCtrl;
  end;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.MouseMove(shift: TShiftState; const pt: TPoint);
var
  ctrl: TLayer32;
begin
  fCurrCursor := 0;
  if not Assigned(fRootCtrl) then Exit;

  if Assigned(RootCtrl.fCaptureCtrl) then
  begin
    RootCtrl.fCaptureCtrl.DoMouseMove(shift, pt);
    if {still} Assigned(RootCtrl.fCaptureCtrl) and (shift = 0) and
      not (RootCtrl.fCaptureCtrl is TPanelCtrl) then
        fCurrCursor := RootCtrl.fCaptureCtrl.CursorId;
    Exit;
  end;

  if Assigned(fMseDownCtrl) then
  begin
    fMseDownCtrl.DoMouseMove(shift, pt);
    Exit;
  end;

  ctrl := fLayeredImg.GetLayerAt(pt);

  if Assigned(fLastCtrl) and (fLastCtrl <> ctrl) then
    fLastCtrl.DoMouseLeave(ctrl);

  if not Assigned(ctrl) or not (ctrl is TCustomCtrl) then
  begin
    fLastCtrl := nil;
    Exit;
  end;

  if (fLastCtrl <> ctrl) then
  begin
    TCustomCtrl(ctrl).DoMouseEnter;
    fLastCtrl := TCustomCtrl(ctrl);
  end;

  TCustomCtrl(ctrl).DoMouseMove(shift, pt);
  //not moving anything so just update the cursor
  if (shift = 0) and not (ctrl is TPanelCtrl) then
    fCurrCursor := ctrl.CursorId;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.MouseUp(mouseButton: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if Assigned(RootCtrl.fCaptureCtrl) then
  begin
    RootCtrl.fCaptureCtrl.Clicked;
  end else
  begin
    if not Assigned(fMseDownCtrl) then Exit;
    fMseDownCtrl.DoMouseUp(mouseButton, shift, pt);
    fMseDownCtrl := nil;
  end;
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.MouseWheel(shift: TShiftState; wheelDelta:
  Integer; mousePos: TPoint): Boolean;
var
  ctrl: TCustomCtrl;
begin
  Result := false;
  ctrl := fRootCtrl.fFocusedCtrl;
  while Assigned(ctrl) do
  begin
    Result := ctrl.DoMouseWheel(shift, wheelDelta, mousePos);
    if Result or (ctrl.Parent = fRootCtrl) then Break;
    ctrl := TCustomCtrl(ctrl.Parent);
  end;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.KeyDown(var Key: Word; shift: TShiftState);
var
  ctrl: TStorage;
  shortcut: TShortcut;
begin
  if not Assigned(fRootCtrl) then Exit;

  if HasAltOrCtrlKey(shift) then
  begin
    shortcut := MakeShortcut(key, shift);
    if shortcut.flags > 0 then
    begin
      ctrl := FindShortcutOwner(shortcut);
      if Assigned(ctrl) then
        TCustomCtrl(ctrl).Clicked
      else if HasAltKey(shift) then Exit;
    end;
  end;

  ctrl := fRootCtrl.fFocusedCtrl;
  while Assigned(ctrl) and (ctrl is TCustomCtrl) do
  begin
    TCustomCtrl(ctrl).DoKeyDown(key, shift);
    if Key = 0 then Break;
    ctrl := ctrl.Parent;
  end;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.KeyUp(var Key: Word; shift: TShiftState);
begin
  if Assigned(fRootCtrl) and Assigned(fRootCtrl.fFocusedCtrl) then
    fRootCtrl.fFocusedCtrl.DoKeyUp(Key, shift);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.KeyPress(var Key: Char);
begin
  if (Key = #27) then
  begin
    if Assigned(RootCtrl.fPopMenuCtrl) then
      RootCtrl.fPopMenuCtrl.Visible := False;
  end
  else if Assigned(fRootCtrl) and Assigned(fRootCtrl.fFocusedCtrl) then
    fRootCtrl.fFocusedCtrl.DoKeyPress(Key);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.Resize(width, height: Cardinal);
var
  oldRec, newRec: TRectD;
  scale: double;
begin
  if Assigned(fRootCtrl) then
  begin
    if fAllowResize and Assigned(EventAndPropertyHandler) then
    begin
      oldRec := fRootCtrl.InnerBounds;
      fLayeredImg.SetSize(width, height);
      fRootCtrl.SetInnerBounds(RectD(0, 0, width, height));
      newRec := fRootCtrl.InnerBounds;
      scale := Average(newRec.Width / oldRec.Width, newRec.Height / oldRec.Height);
      DesignScale := DesignScale * scale;
    end else
    begin
      fLayeredImg.SetSize(width, height);
      fRootCtrl.SetInnerBounds(RectD(0, 0, width, height));
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.SetDesignScale(value: double);
var
  delta: double;
begin
  if (StorageState = ssLoading)  then
  begin
    if Assigned(EventAndPropertyHandler) then
    begin
      EventAndPropertyHandler.Scale(RESET);
      EventAndPropertyHandler.Scale(value);
    end;
    inherited SetDesignScale(value);
  end
  else if (value = RESET) then
  begin
    if Assigned(EventAndPropertyHandler) then
      EventAndPropertyHandler.Scale(RESET);
    inherited SetDesignScale(1);
  end
  else if (StorageState = ssNormal) and Assigned(RootCtrl) and
    AllowResize and (value >= 0.01) and (value <= 100)  then
  begin
    delta := value / DesignScale;
    inherited SetDesignScale(value);
    if Assigned(EventAndPropertyHandler) then // scale fonts etc before ctrls
      EventAndPropertyHandler.Scale(delta);
    RootCtrl.Scale(delta);
  end;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.SetDesignTarget(ctrl: TCustomCtrl);
begin
  if ctrl = designTarget then Exit;
  FreeAndNil(fSizingGroup);
  fDesignTarget := ctrl;
  if Assigned(fDesignTarget) then
    fSizingGroup := CreateSizingButtonGroup(fDesignTarget, ssCorners,
      bsRound, DefaultButtonSize, clLime32);
  InvalidateRect(mainHdl, nil, false);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.SetDesigning(value: Boolean);
begin
  if value = fDesigning then Exit;
  fDesigning := value;
  DesignTarget := nil;
end;
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function TCtrlStorageManager.CreateMainWindow(const UniqueClassName: string;
  const WindowCaption: string; const IconResourceName: string;
  Width: integer; Height: integer): THandle;
var
  Inst      : THandle;
  WinClass  : TWndClass;
const
  defWindowWidth   = 720;
  defWindowHeight  = 480;
begin
  Inst := hInstance;
  if Width <= 0 then Width := DpiAware(defWindowWidth);
  if Height <= 0 then Height := DpiAware(defWindowHeight);

  FillChar(WinClass, SizeOf(WinClass), 0);
  with WinClass do
  begin
    style              := CS_CLASSDC or CS_PARENTDC;
    lpfnWndProc        := @BasicWindowProc;
    hInstance          := Inst;
    hbrBackground      := COLOR_BTNFACE + 1;
    lpszClassname      := PChar(UniqueClassName);
    if IconResourceName <> '' then
      hIcon              := LoadIcon(hInstance, PChar(IconResourceName));
    hCursor            := LoadCursor(0, IDC_ARROW);
  end;
  RegisterClass(WinClass);
  fMainHdl := CreateWindow(PChar(UniqueClassName), PChar(WindowCaption),
              WS_OVERLAPPEDWINDOW, 0, 0,
              Width, Height, 0, 0, Inst, nil);
  Result := fMainHdl;
  if fMainHdl = 0 then Exit;
  SetWindowLongPtr(fMainHdl, GWLP_USERDATA, NativeInt(Self));
  CenterWindow(fMainHdl);
end;
{$ENDIF}

//------------------------------------------------------------------------------
// Initialization procedures
//------------------------------------------------------------------------------

procedure InitCursors;
begin
  // instantiate handles to 'shared' cursors (that won't need destroying)
  sizeCursor  := LoadCursor(0, IDC_SIZEALL);
  handCursor  := LoadCursor(0, IDC_HAND);
  arrowCursor := LoadCursor(0, IDC_ARROW);
end;
//------------------------------------------------------------------------------

procedure InitPaths;
begin
  // 100 x 100 vector icons for checkbox - question mark & tick symbols
  SetLength(queryPaths, 2);
  queryPaths[0] := MakePath([ 64,21, 52,18, 41,19, 32,22, 20,22, 15,12,
    32,4, 55,2, 77,8, 93,19, 98,34, 93,48, 77,60, 59,64, 33,64, 33,49,
    58,49, 59,49, 71,42, 72,41, 75,35, 75,33, 72,27, 71,26 ]);
  queryPaths[1] := MakePath([35,82, 63,82, 63,98, 35,98]);

  SetLength(tickPaths, 1);
  tickPaths[0] := MakePath([
      41.02, 60.75, 100, 0, 48.71, 100, 0, 65.82, 14.10, 37.97]);
end;
//------------------------------------------------------------------------------

procedure InitWindowsVars;
begin
{$IFDEF MSWINDOWS}
  minDragDist := GetSystemMetrics(SM_CXDRAG);
  dblClickInt := GetDoubleClickTime;
{$ENDIF}
end;

procedure RegisterClasses;
begin
  RegisterStorageClass(TRootCtrl);
  RegisterStorageClass(TButtonCtrl);
  RegisterStorageClass(TCheckboxCtrl);
  RegisterStorageClass(TCustomCtrl);
  RegisterStorageClass(TListItem);
  RegisterStorageClass(TListCtrl);
  RegisterStorageClass(TEditCtrl);
  RegisterStorageClass(TMemoCtrl);
  RegisterStorageClass(TEllipseBtnCtrl);
  RegisterStorageClass(TImageBtnCtrl);
  RegisterStorageClass(TLabelCtrl);
  RegisterStorageClass(TPageCtrl);
  RegisterStorageClass(TPageTabCtrl);
  RegisterStorageClass(TPagePnlCtrl);
  RegisterStorageClass(TPanelCtrl);
  RegisterStorageClass(TProgressCtrl);
  RegisterStorageClass(TRadioBtnCtrl);
  RegisterStorageClass(TRoundedBtnCtrl);
  RegisterStorageClass(TScrollCtrl);
  RegisterStorageClass(TSliderCtrl);
  RegisterStorageClass(TStatusbarCtrl);
  RegisterStorageClass(TMenuItemCtrl);
  RegisterStorageClass(TPopMenuCtrl);
  RegisterStorageClass(TMainMenuCtrl);
end;

initialization
  RegisterClasses;
  InitCursors;
  InitPaths;
  InitWindowsVars;

end.
