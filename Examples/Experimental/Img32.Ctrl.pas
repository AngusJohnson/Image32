unit Img32.Ctrl;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  0.0 (Experimental)                                              *
* Date      :  11 March 2025                                                   *
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
  {$IFDEF MSWINDOWS} Windows, {$ENDIF} // required for clipboard
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.Storage, Img32.Vector, Img32.Text, Img32.TextChunks,
  Img32.Layers, Img32.Fmt.SVG;

type
  TTextPosition       = (tpLeft, tpTop, tpRight, tpBottom);
  TTextPositionH      = (tphLeft, tphRight);
  TScrollOrientation  = (soHorizontal, soVertical);
  TAutoPosition       = (apNone, apCustom, apClient,
                          apLeft, apTop, apRight, apBottom);

  TMouseButton        = (mbNone, mbLeft, mbRight, mbMiddle);
  TShiftState         = Cardinal;

  TShortcut = record
    chr   : Char;
    flags : TShiftState;
  end;

  TNameAndAddress = Record
    name: string;
    address: Pointer;
  end;
  TArrayOfNameAndAddress = array of TNameAndAddress;

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
    procedure RefreshPropertyEvents;
    function  GetEventName(event: TNotifyEvent): string;
    function  GetPropName(prop: TObject): string;
    property  EventCount: integer read GetEventCount;
    property  PropCount: integer read GetPropCount;
    property  Event[const name: string]: TNotifyEvent read GetNotify;
    property  Prop[const name: string]: TObject read GetPropObj;
    property  AutoFreeProperties: Boolean read fAutoFree write fAutoFree;
  end;

  TCustomCtrl = class;
  TCustomCtrlClass = class of TCustomCtrl;

  TCtrlStorageManager = class(TStorageManager)
  private
    fLayeredImg   : TLayeredImage32;
    fRootCtrl     : TCustomCtrl;
    fLastClicked  : TCustomCtrl;
    fCurrCursor   : integer;
    fShortcutList : TList;
    fEventHandler : TEventPropertyHandler;
    fFocusLineW   : double;
    function GetRepaintReq: Boolean;
    function GetFocusedCtrl: TCustomCtrl;
    function GetEventProp(const str: string; out success: Boolean): TNotifyEvent;
  protected
    procedure DoBeforeLoad; override;
    procedure DoAfterLoad; override;
    procedure DoBeforeWrite; override;
    function GetEventName(event: TNotifyEvent): string; override;
    function GetExternPropName(prop: TObject): string; override;
    function GetExternalProp(const str: string; out success: Boolean): TObject;
    procedure WriteCustomProperties; override;
    function ReadInfoProperty(const propName, propVal: string): Boolean; override;
  public
    constructor Create(parent: TStorage = nil; const name: string = ''); override;
    destructor Destroy; override;
    function  InsertChild(index: integer; storeClass: TStorageClass): TStorage; override;
    procedure MouseDown(mouseButton: TMouseButton; shift: TShiftState; const pt: TPoint);
    procedure MouseMove(mouseButton: TMouseButton; shift: TShiftState; const pt: TPoint);
    procedure MouseUp(mouseButton: TMouseButton; shift: TShiftState; const pt: TPoint);
    function MouseWheel(shift: TShiftState; wheelDelta: Integer; mousePos: TPoint): Boolean;
    procedure KeyDown(var Key: Word; shift: TShiftState);
    procedure KeyUp(var Key: Word; shift: TShiftState);
    procedure KeyPress(var Key: Char);
    procedure Resize(width, height: Cardinal);
    procedure FindAllShortcutOwners;
    procedure DeleteShortcutOwner(ctrl: TCustomCtrl);
    procedure AddShortcutOwner(ctrl: TCustomCtrl);
    function  FindShortcutOwner(const aShortcut: TShortcut): TCustomCtrl;
    property CurrentCursor: integer read fCurrCursor;
    property EventAndPropertyHandler : TEventPropertyHandler
      read fEventHandler write fEventHandler;
    property FocusedCtrl: TCustomCtrl read GetFocusedCtrl;
    property FocusedLineWidth: double read fFocusLineW write fFocusLineW;
    property LayeredImage: TLayeredImage32 read fLayeredImg write fLayeredImg;
    property RootCtrl: TCustomCtrl read fRootCtrl write fRootCtrl;
    property LastClicked: TCustomCtrl read fLastClicked;
    property RepaintRequired: Boolean read GetRepaintReq;
  end;

  TCustomCtrl = class(THitTestLayer32, INotifyRecipient)
  private
    fFont         : TFontCache;
    fShortcut     : TShortcut;
    fUsableFont   : TFontCache;
    fBevelHeight  : double;
    fFocusedCtrl  : TCustomCtrl;
    fEnabled      : Boolean;
    fRootCtrl     : TCustomCtrl;
    fColor        : TColor32;
    fFontColor    : TColor32;
    fCanFocus     : Boolean;
    fAutoPosition : TAutoPosition;
    fText         : string;
    fOnClick      : TNotifyEvent;
    fAfterPaint   : TNotifyEvent;
    function  GetManager: TCtrlStorageManager;
    procedure SetBevHeight(value: double);
    procedure SetColor(color: TColor32);
    function  GetFont: TFontCache;
    function  GetHasFocus: Boolean;
    procedure SetCanFocus(value: Boolean);
    procedure SetEnabled(value: Boolean);
    procedure SetShortcut(value: TShortcut);
    procedure SetAutoPosition(ap: TAutoPosition);
    function  GetFocusLineWidth: double; {$IFDEF INLINE} inline; {$ENDIF}
    function  GetHalfFocusWidth: double; {$IFDEF INLINE} inline; {$ENDIF}
    function  GetFocusedCtrl: TCustomCtrl;
  protected
    procedure Clicked; virtual;
    function  GetUsableFont: Boolean;
    procedure SetText(const text: string); virtual;
    function  GetCanFocus: Boolean; virtual;
    procedure SetFont(font: TFontCache); virtual;
    procedure CheckScaleBounds(var value: double); virtual;
    procedure Repaint; virtual;
    procedure DoMouseDown(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); virtual;
    procedure DoMouseMove(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); virtual;
    procedure DoMouseUp(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); virtual;
    function DoMouseWheel(Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    procedure DoDblClick; virtual;
    procedure DoKeyDown(var Key: Word; shift: TShiftState); virtual;
    procedure DoKeyUp(var Key: Word; shift: TShiftState); virtual;
    procedure DoKeyPress(var Key: Char); virtual;
    procedure FontChanged; virtual;
    procedure DoBeforeMerge; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification); virtual;
    procedure Scale(value: double); virtual;
    procedure SetFocus; virtual;
    procedure KillFocus; virtual;
    function GetCtrlWithFocus: TCustomCtrl; virtual;

    property AutoPosition : TAutoPosition
      read fAutoPosition write SetAutoPosition;
    property BevelHeight: double read fBevelHeight write SetBevHeight;
    property CanFocus   : Boolean read GetCanFocus write SetCanFocus;
    property Color      : TColor32 read fColor write SetColor;
    property Enabled    : Boolean read fEnabled write SetEnabled;
    property FocusedCtrl: TCustomCtrl read GetFocusedCtrl;
    property Font       : TFontCache read GetFont write SetFont;
    property FontColor  : TColor32 read fFontColor write fFontColor;
    property HasFocus   : Boolean read GetHasFocus;
    property RootCtrl   : TCustomCtrl read fRootCtrl;
    property Shortcut   : TShortcut read fShortcut write SetShortcut;
    property Text       : string read fText write SetText;
    property OnClick    : TNotifyEvent read fOnClick write fOnClick;
    property OnPainted  : TNotifyEvent read fAfterPaint write fAfterPaint;
    property StorageManager: TCtrlStorageManager read GetManager;
    property FocusLineWidth: double read GetFocusLineWidth;
    property HalfFocusWidth: double read GetHalfFocusWidth;
  end;

  TAutoSizedCtrl = class(TCustomCtrl)
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
  end;

  TLabelCtrl = class(TCustomCtrl)
  private
    fTargetCtrl: TCustomCtrl;
  protected
    procedure Clicked; override;
    procedure SetText(const text: string); override;
    procedure Repaint; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    property TargetCtrl: TCustomCtrl read fTargetCtrl write fTargetCtrl;
  end;

  TStatusbarCtrl = class(TAutoSizedCtrl)
  protected
    procedure Repaint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
  end;

  TScrollCtrl = class;                  //scrollbar

  TScrollingCtrl = class(TCustomCtrl)   //scrollbar user
  private
    fScrollV      : TScrollCtrl;
    fScrollH      : TScrollCtrl;
    fInnerMargin  : double;
    fShadowSize   : double;
    fShadowAngle  : double;
    fOnPaint      : TNotifyEvent;
    fOnChange     : TNotifyEvent;
    procedure UpdateHorzScrollbarPos;
    procedure UpdateVertScrollbarPos;
  protected
    procedure DoChildAutoPositioning;
    procedure SetScrollH(scrollHorz: TScrollCtrl); virtual;
    procedure SetScrollV(scrollVert: TScrollCtrl); virtual;
    procedure DoScroll(dx, dy: double); virtual;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Repaint; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    function   InsertChild(layerClass: TLayer32Class;
      index: integer; const name: string = ''): TLayer32; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure Scale(value: double); override;
    property Margin   : double read fInnerMargin write fInnerMargin;
    property ScrollH  : TScrollCtrl read fScrollH write SetScrollH;
    property ScrollV  : TScrollCtrl read fScrollV write SetScrollV;
    property ShadowSize: double read fShadowSize write fShadowSize;
    property OnChange : TNotifyEvent read fOnChange write fOnChange;
    property OnPaint  : TNotifyEvent read fOnPaint write fOnPaint;
  end;

  TListCtrl = class(TScrollingCtrl)
  private
    fItems      : TArrayOfString;
    fTopItem    : integer;
    fScrollOff  : double;
    fItemIndex  : integer;
    fMaxVisible : integer;
    fAutoSize   : Boolean;
    fImgList    : TSvgImageList32;
    fOnClick    : TNotifyEvent;
    procedure SetItemIndex(index: integer);
    function  GetItemCount: integer;
    function  GetItemHeight: double;
    function  GetItem(index: integer): string;
    procedure SetImgList(svgImageList: TSvgImageList32);
    procedure SetVisibleItms(value: integer);
    procedure UpdateScrollV;
  protected
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseDown(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); override;
    procedure DoScroll(dx, dy: double); override;
    procedure SetText(const text: string); override;
    procedure SetScrollV(scrollVert: TScrollCtrl); override;
    procedure Repaint; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification); override;
    procedure ScrollItemIndexIntoView;
    procedure AddItem(const text: string);
    procedure InsertItem(index: integer; const itemStr: string);
    procedure DeleteItem(index: integer);
    property AutoSize : Boolean read fAutoSize write fAutoSize;
    property ImageList: TSvgImageList32 read fImgList write SetImgList;
    property Item[index: integer]: string read GetItem; default;
    property ItemIndex: integer read fItemIndex write SetItemIndex;
    property ItemCount: integer read GetItemCount;
    property MaxVisibleItems: integer read fMaxVisible write SetVisibleItms;
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
  end;

  TPopMenuCtrl = class(TScrollingCtrl)
  private
    fItems      : TArrayOfString;
    fItemIndex  : integer;
    fImgList    : TSvgImageList32;
    fOnClick    : TNotifyEvent;
    procedure SetItemIndex(index: integer);
    function  GetItemCount: integer;
    function  GetItemHeight: double;
    procedure SetImgList(svgImageList: TSvgImageList32);
  protected
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseDown(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); override;
    procedure SetText(const text: string); override;
    procedure Repaint; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure ReceiveNotification(Sender: TObject; notify: TImg32Notification); override;
    procedure AddItem(const text: string);
    procedure InsertItem(index: integer; const itemStr: string);
    procedure DeleteItem(index: integer);
    property ImageList: TSvgImageList32 read fImgList write SetImgList;
    property ItemIndex: integer read fItemIndex write SetItemIndex;
    property ItemCount: integer read GetItemCount;
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
  end;

  TInsertionPtCtrl = class(TCustomCtrl)
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
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
    fTextRect       : TRect;
    fMouseLeftDown  : Boolean;
    procedure CheckPageMetrics;
    function  GetVisibleLines: integer;
    procedure SetTextMargin(const margin: TPointD);
    procedure ScrollCaretIntoView;
    procedure ResetSelPos;
  protected
    procedure SetFont(font: TFontCache); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseMove(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    function DoMouseWheel(Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure SetScrollV(scrollVert: TScrollCtrl); override;
    procedure SetText(const text: string); override;
    procedure DoScroll(dx, dy: double); override;
    procedure FontChanged; override;
    procedure Repaint; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure Scale(value: double); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    function WordIdxToPos(const wordIdx: TPoint): TPointD;
    function PosToChunkIdx(const relPos: TPointD): TPoint;
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
    procedure DoMouseMove(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure SetText(const text: string); override;
    procedure FontChanged; override;
    procedure Repaint; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
    function GetText: string;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    function ChunkIdxToPos(const chunkIdx: TPoint): TPointD;
    function PosToChunkIdx(const relPos: TPointD): TPoint;
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
    procedure SetText(const text: string); override;
    procedure SetPressed(value: Boolean); virtual;
    procedure Repaint; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(Button: TMouseButton;
      shift: TShiftState; const pt: TPoint); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure Scale(value: double); override;
    property Padding: double read fPadding write SetPadding;
    property Pressed: Boolean read fPressed write SetPressed;
  end;

  TRoundedBtnCtrl = class(TButtonCtrl)
  protected
    procedure Repaint; override;
  end;

  TEllipseBtnCtrl = class(TButtonCtrl)
  protected
    procedure Repaint; override;
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
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
    procedure Repaint; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    property TextPosition: TTextPosition read fTextPos write SetTextPos;
    property ImageList: TSvgImageList32 read fImgList write SetImgList;
    property ImageListIdx: integer read fImgIdx write SetImgIdx;
  end;

  TCheckboxCtrl = class(TButtonCtrl)
  private
    fAutoState  : Boolean;
    fTriState   : TTriState;
    fTextPos    : TTextPositionH;
    fUseCross   : Boolean;
    procedure SetTriState(state: TTriState);
    procedure SetTextPos(value: TTextPositionH);
  protected
    procedure Clicked; override;
    procedure Repaint; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    property AutoState: Boolean read fAutoState write fAutoState;
    property TriState: TTriState read fTriState write SetTriState;
    property TextPosition: TTextPositionH read fTextPos write SetTextPos;
    property PreferCross: Boolean read fUseCross write fUseCross;
  end;

  TRadioBtnCtrl = class(TCheckboxCtrl)
  protected
    procedure Repaint; override;
  end;

  TPanelCtrl = class(TScrollingCtrl)
  private
    fScrollOffset: TPointD;
  protected
    procedure CheckScrollMax; virtual;
    procedure DoScroll(dx, dy: double); override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure Scale(value: double); override;
  end;

  TPageTabCtrl = class(TAutoSizedCtrl)
  protected
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
    procedure DrawTabs;
    procedure ResizeTabs;
  protected
    procedure Repaint; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure EndRead; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    procedure Scale(value: double); override;
    procedure ClearTabs;
    procedure AddTab(const caption: string);
    procedure AddTabs(const captions: TArrayOfString);
    property  Panel[index: integer]: TPagePnlCtrl read GetPagePanel;
    property  TabWidth: double read fTabWidth write SetTabWidth;
    property  TabHeight: double read fTabHeight write SetTabHeight;
    property  ActiveIndex: integer read fActiveIdx write SetActiveIndex;
    property  ShadowSize: double read fShadowSize write fShadowSize;
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
    procedure Repaint; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
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
    fOnSlider     : TNotifyEvent;
    function  GetDelta: double;
    procedure SetBtnSize;
    procedure SetPressed(value: Boolean);
    procedure SetMin(newMin: double);
    procedure SetMax(newMax: double);
    procedure SetPosition(newPos: double);
    procedure SetOrientation(newOrient: TScrollOrientation);
  protected
    procedure FontChanged; override;
    procedure Repaint; override;
    procedure DoMouseDown(button: TMouseButton;
      shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseMove(button: TMouseButton; shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(button: TMouseButton; shift: TShiftState; const pt: TPoint); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    property Min        : double read fMax write SetMin;
    property Max        : double read fMax write SetMax;
    property Position   : double read fPosition write SetPosition;
    property Pressed    : Boolean read fPressed write SetPressed;
    property Step       : integer read fScrollStep write fScrollStep;
    property Orientation: TScrollOrientation read fOrientation write SetOrientation;
    property OnSlider   : TNotifyEvent read fOnSlider write fOnSlider;
  end;

  TScrollCtrl = class(TAutoSizedCtrl)
  private
    fAutoHide     : Boolean;
    fSize         : double;
    fScrollStep   : integer;
    fOrientation  : TScrollOrientation;
    fPosition     : double;
    fMax          : double;
    fMin          : double;
    fBtnRec       : TRectWH;
    fMinBtnSize   : integer;
    fDeltaScale   : double;
    fMouseDownPos : TPointD;
    fScrollState  : TScrollState;
    fTargetCtrl   : TScrollingCtrl;
    procedure UnregisterTargetCtrl;
    procedure SetMin(newMin: double);
    procedure SetMax(newMax: double);
    procedure SetScrollSize(newSize: double);
    procedure SetPosition(newPos: double);
    procedure SetAutoHide(value: Boolean);
  protected
    procedure RegisterTargetCtrl(target: TScrollingCtrl);
    procedure CalcScale;
    function CanScroll: Boolean;
    procedure Repaint; override;
    procedure DoMouseDown(button: TMouseButton; shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseMove(button: TMouseButton; shift: TShiftState; const pt: TPoint); override;
    procedure DoMouseUp(button: TMouseButton; shift: TShiftState; const pt: TPoint); override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure Scale(value: double); override;
    procedure SetFocus; override;
    property AutoHide   : Boolean read fAutoHide write SetAutoHide;
    property TargetCtrl : TScrollingCtrl read fTargetCtrl;
    property Size       : double read fSize write SetScrollSize;
    property Min        : double read fMin write SetMin;
    property Max        : double read fMax write SetMax;
    property Position   : double read fPosition write SetPosition;
    property Step       : integer read fScrollStep write fScrollStep;
  end;

const
  ssShift = $8000;
  ssCtrl  = $4000;
  ssAlt   = $2000;

var
  clDefDark32 :   TColor32 = $FF008000;
  clDefMid32  :   TColor32 = $FF33FF33;
  clDefLite32 :   TColor32 = $FFDDEEDD;

{$IFDEF MSWINDOWS}
  minDragDist   : integer;
  dblClickInt   : integer;
  lastClick     : integer;
{$ENDIF}

  tickPaths     :   TPathsD;
  checkPaths    :   TPathsD;

implementation

uses
  RTTI, Img32.Extra, Img32.Draw, Img32.SVG.Core;

resourcestring
  rsListCtrlError = 'TListCtrl error: index out of range.';

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

//------------------------------------------------------------------------------
// Miscellaneous functions
//------------------------------------------------------------------------------


{$IFDEF MSWINDOWS}
function LoadAnsiTextFromClipboard: AnsiString;
var
   len : integer;
   DataHandle: THandle;
   dataPtr: Pointer;
begin
  Result := '';
  if not OpenClipboard(0) then Exit;
  try
    if not IsClipboardFormatAvailable(CF_TEXT) then Exit;
    DataHandle := GetClipboardData(CF_TEXT);
    if DataHandle = 0 then Exit;
    len := GlobalSize(DataHandle) -1; // ignore trailing zero
    if len < 1 then Exit;
    SetLength(Result, len);
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
    len := Length(text) *2 +2;
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
//------------------------------------------------------------------------------
{$ENDIF}

const
  shortcutStrings1: array[0..2] of string =
    ('Shift+', 'Ctrl+', 'Alt+');
  shortcutStrings2: array[0..13] of string =
    ('BkSp', 'Tab', 'Enter', 'Esc', 'Space', 'PgUp', 'PgDn',
    'End', 'Left', 'Up', 'Right', 'Down', 'Ins', 'Del');
  shortcutVirtKeys: array[0..13] of cardinal =
    (8, 9, 13, 27, 32, 33, 34, 35, 37, 38, 39, 40, 45, 46);

function StringToShortcut(str: string): TShortcut;
var
  i,j: integer;
begin
  Result.chr := #0;
  Result.flags := 0;
  if str = '' then Exit;

  for i := 0 to 2 do
  begin
    j := Pos(shortcutStrings1[i], str);
    if j = 0 then Continue;
    Delete(str, j, 6 - i);
    Result.flags := Result.flags or ($8000 shr i);
  end;

  if str = '' then //ie bare shift keys
  begin
    Result.flags := 0;
    Exit;
  end;

  if length(str) > 1 then
    for i := 0 to High(shortcutStrings2) do
    begin
      if Pos(shortcutStrings2[i], str) = 0 then Continue;
      Result.flags := Result.flags or shortcutVirtKeys[i];
      Exit;
    end;
  Result.chr := UpCase(str[1]);
end;
//------------------------------------------------------------------------------

function ShortcutToString(shortcut: TShortcut): string;
begin
  Result := '';
  case (shortcut.flags and $3F) of
    0:  Result := shortcut.chr;
    8:  Result := 'BkSp';
    9:  Result := 'Tab';
    13: Result := 'Enter';
    27: Result := 'Esc';
    32: Result := 'Space';
    33: Result := 'PgUp';
    34: Result := 'PgDn';
    35: Result := 'End';
    37: Result := 'Left';
    38: Result := 'Up';
    39: Result := 'Right';
    40: Result := 'Down';
    45: Result := 'Ins';
    46: Result := 'Del';
    else Exit;
  end;
  if shortcut.flags and ssShift <> 0 then Result := 'Shift+' + Result;
  if shortcut.flags and ssCtrl <> 0 then Result := 'Ctrl+' + Result;
  if shortcut.flags and ssAlt <> 0 then Result := 'Alt+' + Result;
end;
//------------------------------------------------------------------------------

function IsShortcut(const shortcut: TShortcut): Boolean;
  {$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (shortcut.chr <> #0);
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

  case (key) of
    8,9,13,27,32,33,34,35,37,38,39,40,45,46:
      Result.chr := Char(key);
    else if key < 48 then Exit
    else Result.chr := Char(key);
  end;
  Result.flags := shift;
  //if HasShiftKey(shift) then Result.chr := UpCase(Result.chr);
end;
//------------------------------------------------------------------------------

function GetShortcutFromCaption(const caption: string): TShortcut;
var
  i, len: integer;
begin
  Result.chr := #0;
  Result.flags := 0;
  len := Length(caption);
  i := Pos('&', caption);
  if (i = 0) or (i = len) or (caption[i+1] = '&') then Exit;
  Result.flags := $2000;
  Result.chr := Upcase(caption[i+1]);
end;
//------------------------------------------------------------------------------

function ArrayOfStringToString(const aos: TArrayOfString): string;
var
  i, len: integer;
begin
  Result := '';
  len := Length(aos);
  if len = 0 then Exit;
  for i := 0 to Len -2 do
    Result := Result + aos[i] + #10;
  Result := Result + aos[len-1];
end;
//------------------------------------------------------------------------------

function StringToArrayOfString(const str: string): TArrayOfString;
var
  i,j, len, cnt, cap: integer;
begin
  len := Length(str);
  cnt := 0;
  cap := 8;
  SetLength(Result, cap);
  j := 1;
  for i := 1 to  len do
    if str[i] = #10 then
    begin
      if cnt = cap then
      begin
        cap := cap *2;
        SetLength(Result, cap);
      end;
      Result[cnt] := Copy(str, j, i -j);
      j := i+1;
      inc(cnt);
    end;
    if j <= len then
    begin
      Result[cnt] := Copy(str, j, len -j +1);
      inc(cnt);
    end;
  SetLength(Result, cnt);
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
  ulIdx : integer;
begin
  if not enabled then pressed := false;

  if pressed then
    bevHeight := bevHeight * 5/4;

  if not enabled then textColor := clGray32;

  if GetAlpha(color) > 2 then
    DrawPolygon(Image, p, frNonZero, color);

  if pressed then
    DrawEdge(Image, p, clSilver32, clWhite32, bevHeight, closePath) else
    DrawEdge(Image, p, clWhite32, clSilver32, bevHeight, closePath);

  if not Assigned(font) then Exit;

  ulIdx := Pos('&', caption);
  if ulIdx > 0 then Delete(caption, ulIdx, 1);

  rec2 := GetBoundsD(p);
  if padding > 0 then
    Img32.Vector.InflateRect(rec2, -Round(padding), -Round(padding));
  if (GetAlpha(textColor) < 3) or (Trim(caption) = '') then Exit;
  pp := font.GetTextOutline(rec2, caption, taCenter, tvaMiddle, ulIdx);
  if pressed then pp := TranslatePath(pp, bevHeight, bevHeight);
  pp := TranslatePath(pp, textOffX, textOffY);

  dx := font.LineHeight/24;
  pp := TranslatePath(pp, -dx, -dx);
  DrawPolygon(Image, pp, frNonZero, clWhite32);
  pp := TranslatePath(pp, dx, dx);
  DrawPolygon(Image, pp, frNonZero, textColor);
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
  p2 : TPathD;
  rec2  : TRectD;
begin
  radius := radius *2;
  with rec do
    rec2 := RectD(Left, Top, Left + radius, Top + radius);
  Result := MakePath([rec.Left, rec.Bottom]);
  ConcatPaths(Result, arc(rec2, angle180, angle270));
  TranslateRect(rec2, rec.Width - radius, 0);
  p2 := arc(rec2, angle270, angle0);
  ConcatPaths(Result, p2);
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

function GetVaribleWidthTabOffsets(const captions: array of string;
  font: TFontCache; startingOffset: double): TArrayOfDouble;
var
  i, len  : integer;
  padding : double;
begin
  Result := nil;
  len := Length(captions);
  if (len = 0) or not assigned(font) then Exit;
  padding := font.GetTextWidth(' ') *6;
  SetLength(Result, len +1);
  Result[0] := startingOffset;
  for i := 1 to len do
    Result[i] := Result[i-1] +
      font.GetTextWidth(captions[i-1]) + padding;
end;
//------------------------------------------------------------------------------

procedure DrawTabCtrl(Image: TImage32; const captions: array of string;
  const offsets : TArrayOfDouble; font: TFontCache;
  bevHeight: double; selectedIdx: integer;
  tabWidth, tabHeight: double;
  color: TColor32; textColor: TColor32;
  selColor: TColor32; selTextColor: TColor32);
var
  i, len  : integer;
  hbh,bh  : double;
  totalW  : double;
  lh, r   : double;
  rec     : TRectD;
  recI    : TRect;
  img     : TImage32;
  p       : TPathD;
begin
  len := Length(captions);
  if (len = 0) or (len <> High(offsets)) then Exit;
  if selectedIdx < 0 then selectedIdx := 0
  else if selectedIdx >= len then selectedIdx := len-1;

  if assigned(font) then
    lh := font.LineHeight else
    lh := DPIAware(12);

  bh := bevHeight;
  hbh := bh/2;
  r := lh/3;

  totalW := offsets[len];

  if tabHeight <= 0 then
    tabHeight := Ceil(lh * 1.33);

  img := TImage32.Create(Ceil(totalW +bh), Ceil(tabHeight +bh*2));
  try
    //draw tabs before selected tab
    for i := 0 to selectedIdx-1 do
    begin
      rec := RectD(hbh + offsets[i], bh, hbh + offsets[i+1], bh + tabHeight);
      p := GetTabOutLine(rec, r);
      DrawBtnInternal(img, p, captions[i], font,
        hbh, 0, false, true, color, textColor, 0, 0, false);
      p := TranslatePath(p, -1, -1);
      DrawLine(img, p, 1, clGray32, esPolygon);
      p := TranslatePath(p, 1, 1);
    end;

    //draw tabs following selected tab
    for i := len -1 downto selectedIdx+1 do
    begin
      rec := RectD(hbh + offsets[i], bh, hbh + offsets[i+1], bh + tabHeight);
      p := GetTabOutLine(rec, r);
      DrawBtnInternal(img, p, captions[i], font,
        hbh, 0, false, true, color, textColor, 0, 0, false);
      p := TranslatePath(p, 1, -1);
      DrawLine(img, p, 1, clGray32, esPolygon);
      p := TranslatePath(p, -1, 1);
    end;

    //draw selected tab
    rec := RectD(hbh + offsets[selectedIdx], bh+4,
      hbh + offsets[selectedIdx+1], bh + tabHeight);
    img32.Vector.InflateRect(rec, 0, bh);
    p := GetTabOutLine(rec, r);
    DrawBtnInternal(img, p, captions[selectedIdx], font,
      hbh, 0, false, true, selColor, selTextColor, -Round(hbh), 0, false);
    p := Grow(p, nil, hbh, jsRound, 2);
    DrawLine(img, p, DPIAware(1.2), clLiteGrey32, esSquare);

    p := TranslatePath(p, 0, -1);
    DrawLine(img, p, 1, clGray32, esPolygon);
    p := TranslatePath(p, 0, 1);

    recI.Left := Round(offsets[0]);
    recI.Top := Round(bh)-1;
    recI.Right := recI.Left + img.Width;
    recI.Bottom := recI.Top + img.Height;
    Image.CopyBlend(img, img.Bounds, recI, BlendToAlpha);
  finally
    img.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawEditCtrl(Image: TImage32; const rect: TRect;
  bevHeight: double;  color: TColor32 = clWhite32);
begin
  Image.FillRect(rect, color);
  DrawEdge(Image, rect, clSilver32, clWhite32, bevHeight);
end;
//------------------------------------------------------------------------------

procedure DrawCheckboxCtrl(Image: TImage32; const rec: TRect;
  bevHeight: double; triState: TTriState = tsNo; preferTick: Boolean = false;
  color: TColor32 = clWhite32; enabled: Boolean = true);
var
  d,w   : double;
  hbh,pw: double;
  p     : TPathD;
  pp    : TPathsD;
  rec2  : TRectD;
begin
  if not enabled then triState := tsUnknown;
  hbh := bevHeight/2;
  d := min(rec.Width, rec.Height) - bevHeight;
  rec2.Left := rec.Left +hbh;
  rec2.Top := rec.Top +hbh;
  rec2.Right := rec2.Left + d;
  rec2.Bottom := rec2.Top + d;

  Image.FillRect(Rect(rec2), color);
  if bevHeight > 0 then
    DrawEdge(Image, rec2, clSilver32, clWhite32, bevHeight);

  case triState of
    tsUnknown :
      begin
        w := RectWidth(rec);
        pp := ScalePath(checkPaths, w * 0.6 /100);
        pp := TranslatePath(pp, rec.Left + w * 0.2, rec.Top + w * 0.2);
        DrawPolygon(Image, pp, frEvenOdd, SetAlpha(clDefDark32, $88));
//        d := Ceil(d/6);
//        Img32.Vector.InflateRect(rec2, -d, -d);
//        rec2.BottomRight := TranslatePoint(rec2.BottomRight, 1,1);
//        DrawPolygon(Image, Rectangle(rec2), frNonZero, clLiteGray32);
      end;
    tsYes :
      begin
        if preferTick then
        begin
          w := RectWidth(rec);
          p := ScalePath(tickPaths[0],  w * 0.8 /100);
          p := TranslatePath(p, rec.Left + w *0.1, rec.Top + w *0.1);
          DrawPolygon(Image, p, frEvenOdd, clDefDark32);
        end else
        begin
          pw := d/5;
          d := Ceil(d/4);
          Img32.Vector.InflateRect(rec2, -d, -d);
          rec2.BottomRight := TranslatePoint(rec2.BottomRight, 1,1);
          DrawLine(Image, rec2.TopLeft, rec2.BottomRight, pw, clDefDark32);
          DrawLine(Image, PointD(rec2.Left, rec2.Bottom),
            PointD(rec2.Right, rec2.Top), pw, clDefDark32);
        end;
      end;
  end;
end;
//------------------------------------------------------------------------------

procedure DrawRadioCtrl(Image: TImage32; const ellipse: TPathD;
  bevHeight: double; enabled: Boolean; triState: TTriState = tsNo;
  color: TColor32 = clWhite32);
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
    bc := color;

  DrawPolygon(Image, ellipse, frNonZero, bc);
  if bevHeight > 0 then
    DrawEdge(Image, ellipse, clSilver32, clWhite32, bevHeight);

  case triState of
    tsUnknown :
      begin
        pp := ScalePath(checkPaths, d * 0.7 /100);
        pp := TranslatePath(pp, rec2.Left + d * 0.22, rec2.Top  + d * 0.22);
        DrawPolygon(Image, pp, frEvenOdd, SetAlpha(clDefDark32, $88));
      end;
    tsYes     :
      begin
        d := Ceil(d/5);
        InflateRect(rec2, -d, -d);
        p := Img32.Vector.Ellipse(rec2);
        DrawPolygon(Image, p, frNonZero, SetAlpha(clDefDark32, $DD));
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
  if Assigned(StorageManager) then
    with StorageManager do
    begin
      if not Assigned(fRootCtrl) then fRootCtrl := self;
      self.fRootCtrl := fRootCtrl;
    end;
  fColor        := clBtnFace32;
  fFontColor    := clBlack32;
  fEnabled      := true;
  fCanFocus     := true;
  fBevelHeight  := dpiAware(2);
  OuterMargin   := DPIAware(5);
end;
//------------------------------------------------------------------------------

destructor TCustomCtrl.Destroy;
begin
  if IsShortcut(fShortcut) and
    Assigned(StorageManager) then
      StorageManager.DeleteShortcutOwner(self);

  //notify INotifySenders
  if Assigned(fFont) then fFont.DeleteRecipient(self);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  inherited SetInnerBounds(newBounds);
  Invalidate;
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

procedure TCustomCtrl.CheckScaleBounds(var value: double);
begin
  if value < 0.025 then value := 0.025
  else if value > 50 then value := 50;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.Scale(value: double);
var
  i: integer;
  rec: TRectD;
begin
  if AutoPosition <> apClient then
  begin
    CheckScaleBounds(value);
    rec := GetInnerBounds;
    rec := ScaleRect(rec, value);
    SetInnerBounds(rec);
    fBevelHeight := fBevelHeight * value;
    OuterMargin := OuterMargin * value;
    Invalidate;
  end;

  for i := 0 to ChildCount -1 do
    if Child[i] is TCustomCtrl then
      TCustomCtrl(Child[i]).Scale(value);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetText(const text: string);
begin
  if fText = text then Exit;
  fText := text;
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
  Result := StorageManager.fFocusLineW;
end;
//------------------------------------------------------------------------------

function  TCustomCtrl.GetHalfFocusWidth: double;
begin
  Result := StorageManager.fFocusLineW * 0.5;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetFocusedCtrl: TCustomCtrl;
begin
  if Assigned(fRootCtrl) then
    Result := fRootCtrl.fFocusedCtrl else
    Result := nil;
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
    if not Assigned(TCustomCtrl(Child[i]).fFont) then
      TCustomCtrl(Child[i]).FontChanged; //child inherits the parent's font
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
  hadShortcut := IsShortcut(fShortcut);
  fShortcut := value;
  if (hadShortcut = IsShortcut(fShortcut)) or
    not Assigned(StorageManager) then Exit;
  if hadShortcut then
    StorageManager.DeleteShortcutOwner(self) else
    StorageManager.AddShortcutOwner(self);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetAutoPosition(ap: TAutoPosition);
begin
  if ap = fAutoPosition then Exit;
  fAutoPosition := ap;
  Invalidate;
  if Assigned(Parent) and (Parent is TScrollingCtrl) then
    TScrollingCtrl(Parent).DoChildAutoPositioning;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetHasFocus: Boolean;
begin
  Result := fRootCtrl.fFocusedCtrl = self;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetCanFocus(value: Boolean);
begin
  fCanFocus := value;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.SetFocus;
begin
  if not CanFocus then
  begin
    if parent is TCustomCtrl then
      TCustomCtrl(parent).SetFocus;
    Exit;
  end;

  if (self = fRootCtrl.fFocusedCtrl) then Exit;
  if Assigned(fRootCtrl.fFocusedCtrl) then
    fRootCtrl.fFocusedCtrl.KillFocus;
  fRootCtrl.fFocusedCtrl := Self;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.KillFocus;
begin
  if self <> fRootCtrl.fFocusedCtrl then Exit;
  fRootCtrl.fFocusedCtrl := nil;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetCanFocus: Boolean;
begin
  Result := fCanFocus and fEnabled and Visible;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.GetCtrlWithFocus: TCustomCtrl;
begin
  Result := fRootCtrl.fFocusedCtrl;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if not Visible or not fEnabled then Exit;
  if CanFocus and not HasFocus then SetFocus;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseMove(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
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
  if Visible and fEnabled and Assigned(fOnClick) then
    fOnClick(Self);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.Repaint;
begin
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoBeforeMerge;
begin
  if UpdateInfo.updateMethod = umNone then Exit;
  Image.BlockNotify;
  try
    Repaint;
  finally
    Image.UnblockNotify;
  end;
end;
//------------------------------------------------------------------------------

function TCustomCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit;
  if propName = 'AutoPosition' then
    AutoPosition := TAutoPosition(GetIntProp(propVal, Result))
  else if propName = 'BevelHeight' then
    BevelHeight := GetDoubleProp(propVal, Result)
  else if propName = 'CanFocus' then
    CanFocus := GetBoolProp(propVal, Result)
  else if propName = 'Color' then
    Color := GetColorProp(propVal, Result)
  else if propName = 'Enabled' then
    Enabled := GetBoolProp(propVal, Result)
  else if propName = '?Font' then
    Font := StorageManager. GetExternalProp(
      propVal, Result) as TFontCache
  else if propName = 'Shortcut' then
    fShortcut := StringToShortcut(GetStringProp(propVal, Result))
  else if propName = 'Text' then
    Text := GetStringProp(propVal, Result)
  else if propName = '?OnClick' then
    self.fOnClick := StorageManager.GetEventProp(propVal, Result);
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.WriteProperties;
begin
  inherited;
  WriteIntProp('AutoPosition', Ord(AutoPosition));
  WriteDoubleProp('BevelHeight', BevelHeight);
  WriteBoolProp('CanFocus', CanFocus);
  WriteColorProp('Color', Color);
  if not fEnabled then WriteBoolProp('Enabled', Enabled);
  WriteExternalProp('Font', fFont);
  if (fShortcut.chr > #0) or (fShortcut.flags > 0) then
    WriteStrProp('Shortcut', ShortcutToString(fShortcut));
  if Text <> '' then WriteStrProp('Text', Text);
  if Assigned(fOnClick) then WriteEventProp('OnClick', fOnClick);
end;

//------------------------------------------------------------------------------
// TAutoSizedCtrl
//------------------------------------------------------------------------------

constructor TAutoSizedCtrl.Create(parent: TLayer32; const name: string);
begin
  fAutoPosition := apCustom;
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
  if Assigned(fTargetCtrl) and fTargetCtrl.CanFocus then
    fTargetCtrl.SetFocus;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TLabelCtrl.SetText(const text: string);
begin
  inherited;
  fShortcut := GetShortcutFromCaption(text);
end;
//------------------------------------------------------------------------------

procedure TLabelCtrl.Repaint;
var
  ulIdx: integer;
  caption: string;
  pp: TPathsD;
  dx: double;
  rec: TRect;
begin
  Image.Clear(Color);
  if not GetUsableFont then Exit;

  caption := fText;
  ulIdx := Pos('&', caption);
  if ulIdx > 0 then Delete(caption, ulIdx, 1);

  rec := Rect(InnerRect);
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  pp := fUsableFont.GetTextOutline(RectD(rec), caption, taCenter, tvaMiddle, ulIdx);
  dx := fUsableFont.LineHeight/24;
  pp := TranslatePath(pp, -dx, -dx);
  DrawPolygon(Image, pp, frNonZero, clWhite32);
  pp := TranslatePath(pp, dx, dx);
  DrawPolygon(Image, pp, frNonZero, clBlack32);
end;
//------------------------------------------------------------------------------

function TLabelCtrl.ReadProperty(const propName, propVal: string): Boolean;
var
  p: TCustomCtrl;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'TargetCtrl' then
  begin
    p := GetStorageProp(propVal, Result) as TCustomCtrl;
    if Assigned(p) and (p is TCustomCtrl) then
      TargetCtrl := TCustomCtrl(p);
  end;
end;
//------------------------------------------------------------------------------

procedure TLabelCtrl.WriteProperties;
begin
  inherited;
  WriteLocalStorageProp('TargetCtrl', fTargetCtrl);
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
  rec : TRectD;
  h   : double;
begin
  rec := newBounds;
  if GetUsableFont then
    h := fUsableFont.LineHeight + fBevelHeight *3 else
    h := DPIAware(15);
  rec.Top := rec.Bottom - h;
  inherited SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

procedure TStatusbarCtrl.Repaint;
var
  rec: TRect;
  bh: double;
begin
  if not (Parent is TPanelCtrl) then Exit;
  Image.Clear(Color);
  rec := Rect(InnerRect);
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  bh := fBevelHeight;
  DrawEdge(Image, rec, clWhite32, clSilver32, bh);
  Img32.Vector.InflateRect(rec, -DPIAware(10), -Round(bh));
  if GetUsableFont then
    DrawText(Image, rec.Left, rec.Top + fUsableFont.Ascent ,
      fText, fUsableFont, fFontColor);
end;

//------------------------------------------------------------------------------
// TScrollingCtrl
//------------------------------------------------------------------------------

constructor TScrollingCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fBevelHeight := dpiAware(2);
  //OuterMargin := DpiAwareOne*5;
  fInnerMargin := 0;
end;
//------------------------------------------------------------------------------

destructor TScrollingCtrl.Destroy;
begin
  if Assigned(fScrollH) then fScrollH.UnregisterTargetCtrl;
  if Assigned(fScrollV) then fScrollV.UnregisterTargetCtrl;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.UpdateHorzScrollbarPos;
begin
  if not Assigned(fScrollH) or Image.IsEmpty then Exit;
  fScrollH.CalcScale;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.UpdateVertScrollbarPos;
begin
  if not Assigned(fScrollV) or Image.IsEmpty then Exit;
  fScrollV.CalcScale;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.InsertChild(layerClass: TLayer32Class;
  index: integer; const name: string = ''): TLayer32;
begin
  //non-scroll ctrls must always preceed scroll ctrls
  if layerClass.InheritsFrom(TScrollCtrl) then
  begin
    while (index < ChildCount) and not (Child[index] is TScrollCtrl) do inc(index);
  end else
  begin
    if index > ChildCount then index := ChildCount;
    while (index > 0) and (Child[index -1] is TScrollCtrl) do dec(index);
  end;
  Result := inherited InsertChild(layerClass, index, name);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  inherited;
  DoChildAutoPositioning;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.DoChildAutoPositioning;
var
  i           : integer;
  w,h         : double;
  rec         : TRectD;
  clientRect  : TRectD;
  clientCtrl  : TCustomCtrl;
  scrollHsize : double;
  scrollVsize : double;
const
  scrollScale = 0.75;
begin
  if (StorageState = ssLoading) or (Width = 0) or (Height = 0) or
    not GetUsableFont then Exit;

  clientRect := InnerRect;
  //exclude regions non-usable for all childs

  //reposition horz and vert scrollbars
  if Assigned(fScrollH) and fScrollH.Visible then
  begin
    if fScrollH.fSize = 0 then
      scrollHsize := fUsableFont.LineHeight * scrollScale else
      scrollHsize := fScrollH.fSize;
    rec := clientRect;
    if Assigned(fScrollV) and fScrollV.Visible then
    begin
      if fScrollV.fSize = 0 then
        scrollVsize := fUsableFont.LineHeight * scrollScale else
        scrollVsize := fScrollV.fSize;
      rec.Right := rec.Right - scrollVsize;
    end;
    rec.Top := Rec.Bottom - scrollHsize;
    fScrollH.SetInnerBounds(rec);
    clientRect.Bottom := clientRect.Bottom - scrollHsize;
  end;
  if Assigned(fScrollV) and (fScrollV.Visible or fScrollV.fAutoHide) then
  begin
    if fScrollV.fSize = 0 then
      scrollVsize := fUsableFont.LineHeight * scrollScale else
      scrollVsize := fScrollV.fSize;
    rec := clientRect;
    rec.Left := Rec.Right - scrollVsize;
    fScrollV.SetInnerBounds(rec);
    clientRect.Right := clientRect.Right - scrollVsize;
  end;

  //now exclude the region that's non-usable for 'inner' childs
  if fInnerMargin > 0 then
    InflateRect(clientRect, -fInnerMargin, -fInnerMargin);

  clientCtrl  := nil;
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) and
    not (Child[i] is TScrollCtrl) and
      (TCustomCtrl(Child[i]).fAutoPosition > apCustom) then
  begin
    rec := clientRect;
    w := rec.Width; h := rec.Height;
    case TCustomCtrl(Child[i]).fAutoPosition of
      apClient  :
        clientCtrl := TCustomCtrl(Child[i]);
      apLeft    :
        begin
          rec.Right := rec.Left + Min(w, Child[i].Width);
          Child[i].SetInnerBounds(rec);
          clientRect.Left := Child[i].Left + Child[i].Width;
        end;
      apTop     :
        begin
          rec.Bottom := rec.Top + Min(h, Child[i].Height);
          Child[i].SetInnerBounds(rec);
          clientRect.Top := Child[i].Top + Child[i].Height;
        end;
      apRight   :
        begin
          rec.Left := rec.Right - Min(w, Child[i].Width);
          Child[i].SetInnerBounds(rec);
          clientRect.Right := Child[i].Left;
        end;
      apBottom  :
        begin
          rec.Top := rec.Bottom - Min(h, Child[i].Height);
          Child[i].SetInnerBounds(rec);
          clientRect.Bottom := Child[i].Top;
        end;
    end;
  end;
  if Assigned(clientCtrl) then
    clientCtrl.SetInnerBounds(clientRect);

  UpdateHorzScrollbarPos;
  UpdateVertScrollbarPos;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.Repaint;
var
  rec : TRectD;
  bh  : double;
begin
  image.Clear;

  image.Clear;
  bh := fBevelHeight;

  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  image.FillRect(Rect(rec), color);
  DrawEdge(Image, rec, clSilver32, clWhite32, bh);

  if Assigned(fOnPaint) then fOnPaint(Self);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.Scale(value: double);
begin
  inherited;
  fInnerMargin := fInnerMargin * value;
end;
//------------------------------------------------------------------------------

function TScrollingCtrl.ReadProperty(const propName, propVal: string): Boolean;
var
  p: TStorage;
begin
  Result := inherited ReadProperty(propName, propVal);

  if Result then Exit
  else if propName = 'ShadowSize' then
    ShadowSize := GetDoubleProp(propVal, Result)
  else if propName = 'Margin' then
    Margin := GetDoubleProp(propVal, Result)
  else if propName = 'ScrollH' then
  begin
    p := GetStorageProp(propVal, Result) as TScrollCtrl;
    if Assigned(p) and (p is TScrollCtrl) then
      ScrollH := TScrollCtrl(p);
  end
  else if propName = 'ScrollV' then
  begin
    p := GetStorageProp(propVal, Result) as TScrollCtrl;
    if Assigned(p) and (p is TScrollCtrl) then
      ScrollV := TScrollCtrl(p);
  end;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.WriteProperties;
begin
  inherited;
  WriteDoubleProp('Margin', fInnerMargin);
  if ShadowSize > 0 then WriteDoubleProp('ShadowSize', fShadowSize);
  WriteLocalStorageProp('ScrollH', ScrollH);
  WriteLocalStorageProp('ScrollV', ScrollV);
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetScrollH(scrollHorz: TScrollCtrl);
begin
  if Assigned(fScrollH) then fScrollH.UnregisterTargetCtrl;
  fScrollH := scrollHorz;
  if not Assigned(scrollHorz) then Exit;
  with scrollHorz do
  begin
    RegisterTargetCtrl(self);
    fOrientation := soHorizontal;
    Move(self, MaxInt);
    AutoPosition := apBottom;
  end;
  if (StorageState = ssNormal) then
    DoChildAutoPositioning;
end;
//------------------------------------------------------------------------------

procedure TScrollingCtrl.SetScrollV(scrollVert: TScrollCtrl);
begin
  if Assigned(fScrollV) then fScrollV.UnregisterTargetCtrl;
  fScrollV := scrollVert;
  if not Assigned(scrollVert) then Exit;
  with scrollVert do
  begin
    RegisterTargetCtrl(self);
    fOrientation := soVertical;
    Move(self, MaxInt);
    AutoPosition := apRight;
  end;
  if (StorageState = ssNormal) then
    DoChildAutoPositioning;
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

procedure TScrollingCtrl.DoScroll(dx, dy: double);
begin
end;

//------------------------------------------------------------------------------
// TListCtrl
//------------------------------------------------------------------------------

constructor TListCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fCanFocus := true;
  fColor := clWhite32;
  fMaxVisible := 6;
  fAutoSize := true;
end;
//------------------------------------------------------------------------------

destructor TListCtrl.Destroy;
begin
  SetImgList(nil);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.SetInnerBounds(const newBounds: TRectD);
var
  rec: TRectD;
  makeVis, requestVis: integer;
  itemH: double;
  scrollCanShow: Boolean;
begin
  itemH := GetItemHeight;
  if (itemH = 0) then
    Exit
  else if (StorageState = ssLoading) or not GetUsableFont then
  begin
    inherited SetInnerBounds(newBounds);
    Exit;
  end;

  requestVis := Ceil((newBounds.Height - BevelHeight*2)/itemH);
  scrollCanShow := Assigned(ScrollV) and (ScrollV.Visible or ScrollV.fAutoHide);

  if not fAutoSize then
  begin
    if fMaxVisible = 0 then
      makeVis := requestVis else
      makeVis := Min(requestVis, fMaxVisible)
  end
  else if (fMaxVisible > 0) then
  begin
    makeVis := Max(1, Min(fMaxVisible, ItemCount));
  end
  else if requestVis > 8 then
  begin
    makeVis := 8;
    ScrollV.Visible := true;
  end else
  begin
    ScrollV.Visible := false;
    makeVis := requestVis;
  end;

  if scrollCanShow and ScrollV.fAutoHide then
    ScrollV.Visible := makeVis < GetItemCount;

  rec := newBounds;
  rec.Bottom := rec.Top +
    (makeVis * itemH) + BevelHeight*2;
  inherited SetInnerBounds(rec);
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

function TListCtrl.GetItemCount: integer;
begin
  Result := Length(fItems);
end;
//------------------------------------------------------------------------------

function TListCtrl.GetItem(index: integer): string;
begin
  if (index < 0) or (index >= ItemCount) then
    raise Exception.Create(rsListCtrlError);
  result := fItems[index];
end;
//------------------------------------------------------------------------------

procedure TListCtrl.SetItemIndex(index: integer);
begin
  if (index < 0) or (index >= GetItemCount) then index := -1;
  if index = fItemIndex then Exit;
  fItemIndex := index;
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

procedure TListCtrl.DoScroll(dx, dy: double);
begin
  fScrollOff := fScrollOff + dy;
  fTopItem := Round(fScrollOff/GetItemHeight);
  if fTopItem < 0 then fTopItem := 0
  else if fTopItem > ItemCount - fMaxVisible then
    fTopItem := ItemCount - fMaxVisible;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.UpdateScrollV;
var
  itmHeight: double;
begin
  if Assigned(ScrollV) then
  begin
    itmHeight := GetItemHeight;
    ScrollV.Max := ItemCount * itmHeight;
    ScrollV.Step := Round(itmHeight);
  end;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.SetScrollV(scrollVert: TScrollCtrl);
begin
  inherited;
  if (StorageState = ssNormal) then
    UpdateScrollV;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.SetText(const text: string);
begin
  inherited;
  fItems := StringToArrayOfString(Text);
  SetInnerBounds(GetInnerBounds);
  UpdateScrollV;
  fItemIndex := -1;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.Repaint;
var
  i,cnt, spaceW, marg, om, hbh, vis: integer;
  itemH, bh: double;
  rec, rec2: TRectD;
  recI, rec2I: TRect;
  img: TImage32;
  scrollShowing: Boolean;
const
  drawShad = true;
begin

  Image.Clear;
  recI := Rect(InnerRect);
  TranslateRect(recI, Round(OuterMargin), Round(OuterMargin));
  Image.Clear(recI, color);
  DrawEdge(Image, recI, clSilver32, clWhite32, fBevelHeight);

  cnt := ItemCount;
  if (cnt = 0) or not GetUsableFont then Exit;

  itemH := GetItemHeight;
  marg := Round(fInnerMargin);
  spaceW := Round(fUsableFont.GetSpaceWidth);
  bh := fBevelHeight;
  hbh := Round(bh/2);
  om := Round(OuterMargin);

  scrollShowing := Assigned(ScrollV) and ScrollV.Visible;

  vis := Math.Min(Trunc((recI.Height - fBevelHeight *2) / itemH), cnt);
  if HasFocus then
  begin
    if (om > 0) then DrawShadowRect(Image, recI, om);
    if (ItemIndex < 0) then
    begin
      Types.InflateRect(recI, hbh*2, hbh*2);
      DrawLine(Image, Rectangle(recI), FocusLineWidth, clDefDark32, esPolygon);
      Types.InflateRect(recI, -hbh*2, -hbh*2);
    end;
  end;

  if Assigned(fImgList) then
    img := TImage32.Create(fImgList.DefaultWidth, fImgList.DefaultHeight) else
    img := nil;
  try
    with recI do
      rec := RectD(Left + bh, Top + bh, Right -bh -1, Top + bh +itemH);
    if scrollShowing then rec.Right := rec.Right - ScrollV.Width;

    if fTopItem < 0 then fTopItem := 0;
    for i := fTopItem to fTopItem + vis -1 do
    begin
      if i = fItemIndex then
      begin
        rec2 := rec;
        DrawLine(Image, Rectangle(rec),
          DpiAwareOne, clGray32, esPolygon);
        if HasFocus then
          Image.FillRect(Rect(rec2), clLiteBtn32) else
          Image.FillRect(Rect(rec2), clBtnFace32);
        Img32.Vector.InflateRect(rec2, -hbh, -hbh);
        DrawEdge(Image, rec2, clWhite32, clLiteGray32,
          DpiAwareOne * StorageManager.DesignFormScale);
        if HasFocus then
          DrawLine(Image, Rectangle(rec2),
            FocusLineWidth, clDefDark32, esPolygon);
      end;

      if Assigned(img) and (i < fImgList.Count) then
      begin
        recI := Rect(rec);
        fImgList.GetImage(i, img);
        rec2I.Left := recI.Left + marg + spaceW;
        rec2I.Top := recI.Top + Round((rec.Height - img.Height)/2);
        rec2I.Right := rec2I.Left + img.Width;
        rec2I.Bottom := rec2I.Top + img.Height;
        Image.CopyBlend(img, img.Bounds, rec2I, BlendToAlpha);
      end;

      if Assigned(img) then
        rec.Left := rec.Left + img.Width + spaceW*2;

      DrawText(Image, rec, fItems[i], fUsableFont, clWhite32, taLeft, tvaMiddle);
      TranslateRect(rec, hbh, hbh);
      DrawText(Image, rec, fItems[i], fUsableFont, clBlack32, taLeft, tvaMiddle);
      TranslateRect(rec, -hbh, itemH -hbh);

      if Assigned(img) then
        rec.Left := rec.Left - img.Width - spaceW*2;
    end;
  finally
    FreeAndNil(img);
  end;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.AddItem(const text: string);
begin
  InsertItem(MaxInt, text);
end;
//------------------------------------------------------------------------------

procedure TListCtrl.InsertItem(index: integer; const itemStr: string);
var
  len: integer;
begin
  len := GetItemCount;
  if (index < 0) then index := 0
  else if (index > len) then index := len;
  Insert(itemStr, fItems, index);
  Text := ArrayOfStringToString(fItems);
end;
//------------------------------------------------------------------------------

procedure TListCtrl.DeleteItem(index: integer);
begin
  if (index < 0) or (index >= GetItemCount) then Exit;
  Delete(fItems, index, 1);
  Text := ArrayOfStringToString(fItems);
  Invalidate;
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
  itemsInView := Trunc(Height/itmHeight);
  if fItemIndex < fTopItem then
    fTopItem := fItemIndex
  else if fItemIndex > fTopItem + itemsInView -1 then
    fTopItem := Math.Max(0, fItemIndex - itemsInView +1)
  else
    Exit;

  ScrollV.Position :=
    Min(fTopItem * itmHeight, ScrollV.fMax - Height);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.SetVisibleItms(value: integer);
begin
  if (value < 0) then value := 0;
  if fMaxVisible = value then Exit;
  fMaxVisible := value;
  Invalidate;
  SetInnerBounds(GetInnerBounds);
end;
//------------------------------------------------------------------------------

function TListCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'AutoSize' then
    fAutoSize := GetBoolProp(propVal, Result)
  else if propName = '?ImageList' then
    ImageList := StorageManager.GetExternalProp(propVal,
      Result) as TSvgImageList32
  else if propName = 'ItemIndex' then
    fItemIndex := GetIntProp(propVal, Result)
  else if propName = 'MaxVisibleItems' then
  begin
    fMaxVisible := 0;
    MaxVisibleItems := GetIntProp(propVal, Result)
  end;
end;
//------------------------------------------------------------------------------

procedure TListCtrl.WriteProperties;
begin
  inherited;
  WriteBoolProp('AutoSize', fAutoSize);
  WriteExternalProp('ImageList', ImageList);
  WriteIntProp('ItemIndex', fItemIndex);
  WriteIntProp('MaxVisibleItems', fMaxVisible);
end;

//------------------------------------------------------------------------------
// TPopMenuCtrl
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.SetItemIndex(index: integer);
begin
  if (index < 0) or (index >= GetItemCount) then index := -1;
  if index = fItemIndex then Exit;
  fItemIndex := index;
  Invalidate;
end;
//------------------------------------------------------------------------------

function  TPopMenuCtrl.GetItemCount: integer;
begin
  Result := Length(fItems);
end;
//------------------------------------------------------------------------------

function  TPopMenuCtrl.GetItemHeight: double;
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

procedure TPopMenuCtrl.SetImgList(svgImageList: TSvgImageList32);
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

procedure TPopMenuCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ItemCount > 0) and GetUsableFont then
  begin
    case key of
      VK_UP:
        if ItemIndex > 0 then
        begin
          ItemIndex := ItemIndex -1;
          Key := 0;
          Invalidate;
          Exit;
        end;
      VK_DOWN:
        if ItemIndex < ItemCount -1 then
        begin
          ItemIndex := ItemIndex +1;
          Key := 0;
          Invalidate;
          Exit;
        end;
    end;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
var
  pt2: TPointD;
begin
  if (ItemCount > 0) and GetUsableFont then
  begin
    pt2 := MakeRelative(PointD(pt.X, pt.Y - fBevelHeight));
    ItemIndex := Trunc(pt2.Y / GetItemHeight);
    Invalidate;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.SetText(const text: string);
begin
  inherited;
  fItems := StringToArrayOfString(Text);
  SetInnerBounds(GetInnerBounds);
  fItemIndex := -1;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.Repaint;
begin

end;
//------------------------------------------------------------------------------

function  TPopMenuCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = '?ImageList' then
    ImageList := StorageManager.GetExternalProp(propVal,
      Result) as TSvgImageList32
  else if propName = 'ItemIndex' then
    fItemIndex := GetIntProp(propVal, Result);
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.WriteProperties;
begin
  inherited;
  WriteExternalProp('ImageList', ImageList);
  WriteIntProp('ItemIndex', fItemIndex);
end;
//------------------------------------------------------------------------------

constructor TPopMenuCtrl.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited;
  fCanFocus := true;
  fColor := clWhite32;
end;
//------------------------------------------------------------------------------

destructor TPopMenuCtrl.Destroy;
begin
  SetImgList(nil);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.SetInnerBounds(const newBounds: TRectD);
var
  rec: TRectD;
  itemH: double;
begin
  itemH := GetItemHeight;
  if (itemH = 0) then Exit
  else if (StorageState = ssLoading) or not GetUsableFont then
  begin
    inherited SetInnerBounds(newBounds);
    Exit;
  end;

  rec := newBounds;
  rec.Bottom := rec.Top + (ItemCount * itemH) + BevelHeight*2;
  inherited SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.ReceiveNotification(Sender: TObject; notify: TImg32Notification);
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

procedure TPopMenuCtrl.AddItem(const text: string);
begin
  InsertItem(MaxInt, text);
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.InsertItem(index: integer; const itemStr: string);
var
  len: integer;
begin
  len := GetItemCount;
  if (index < 0) then index := 0
  else if (index > len) then index := len;
  Insert(itemStr, fItems, index);
  Text := ArrayOfStringToString(fItems);
end;
//------------------------------------------------------------------------------

procedure TPopMenuCtrl.DeleteItem(index: integer);
begin
  if (index < 0) or (index >= GetItemCount) then Exit;
  Delete(fItems, index, 1);
  Text := ArrayOfStringToString(fItems);
  Invalidate;
end;

//------------------------------------------------------------------------------
// TInsertionPtCtrl
//------------------------------------------------------------------------------

constructor TInsertionPtCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  CanFocus := false;
  Color := clNone32;
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
  Color := clWhite32;
  BlendFunc  := nil; //assumes edit controls will always be fully opaque.
  fTextMargin := PointD(20, 5);
  fCursorChunkPos := InvalidPoint;
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

procedure TMemoCtrl.Scale(value: double);
begin
  inherited;
  fTopLine  := 0;
  fSelStart := NullPoint;
  fSelEnd   := NullPoint;
  ScrollV.Position := 0;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.CheckPageMetrics;
var
  ScrollVAutoHide     : Boolean;
  scrollShowing       : Boolean;
  rec                 : TRect;
  om                  : integer;
begin
  if not GetUsableFont or
    // and exit if page metrics already measured
   (RectWidth(fPageMetrics.bounds) > 0) and
   (rec.Width = fTextRect.Width) and (rec.Height = fTextRect.Height) then Exit;

  om := Round(OuterMargin);
  rec  := Rect(InnerRect);
  TranslateRect(rec, om, om);
  if HasFocus then
    InflateRect(rec, Round(-TextMargin.X - fBevelHeight),
      Round(-TextMargin.Y - fBevelHeight))
  else
    InflateRect(rec, Round(-TextMargin.X), Round(-TextMargin.Y));

  //start by assuming ScrollV is NOT required
  ScrollVAutoHide := assigned(ScrollV) and ScrollV.AutoHide;
  scrollShowing := Assigned(ScrollV) and ScrollV.Visible;
  if not ScrollVAutoHide and scrollShowing then
    rec.Right := rec.Right - Round(ScrollV.Width);

  if not RectsEqual(fTextRect, rec) then
  begin
    fTextRect := rec;
    fPageMetrics :=
      fChunkedText.GetPageMetrics(fTextRect, fUsableFont.LineHeight);

    if RectHeight(fPageMetrics.bounds) < fTextRect.Height then
    begin
      if scrollShowing and ScrollVAutoHide then ScrollV.Visible := false;
    end
    else if ScrollVAutoHide and Assigned(ScrollV) then
    begin
      if not scrollShowing then ScrollV.Visible := true;
      fTextRect.Right := fTextRect.Right - Round(ScrollV.Width);
      fPageMetrics :=
        fChunkedText.GetPageMetrics(fTextRect, fUsableFont.LineHeight);
    end;
  end;

  if Assigned(ScrollV) and ScrollV.Visible then
  begin
    with fPageMetrics do
    begin
      ScrollV.Max := Round(lineHeight * totalLines);
      //ScrollV.Visible := true; ////////////
      ScrollV.Step := Round(lineHeight);
    end;
    ScrollV.Invalidate;
  end;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetFont(font: TFontCache);
begin
  inherited;
  if not Assigned(fUsableFont) then Exit;
  fPageMetrics.bounds := NullRect; // force a fPageMetrics refresh
  fPageMetrics.lineHeight := fUsableFont.LineHeight;
  if fText <> '' then
    fChunkedText.ApplyNewFont(fUsableFont);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.ResetSelPos;
begin
  fSelStart := fCursorChunkPos;
  fSelEnd := fCursorChunkPos;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.ScrollCaretIntoView;
var
  visLines: integer;
begin
  visLines := GetVisibleLines;
  if fCursorChunkPos.X < fPageMetrics.startOfLineIdx[fTopLine] then
  begin
    while fCursorChunkPos.X < fPageMetrics.startOfLineIdx[fTopLine] do
      dec(fTopLine);
    ScrollV.Position := fTopLine * fPageMetrics.lineHeight;
    Invalidate;
  end
  else if (fTopLine + visLines < fPageMetrics.totalLines) and
    (fCursorChunkPos.X >= fPageMetrics.startOfLineIdx[fTopLine + visLines]) then
  begin
    while (fTopLine + visLines < fPageMetrics.totalLines) and
      (fCursorChunkPos.X >= fPageMetrics.startOfLineIdx[fTopLine + visLines]) do
        inc(fTopLine);
    ScrollV.Position := fTopLine * fPageMetrics.lineHeight;
    Invalidate;
  end;
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
  pos: TPointD;
  ctrlDown: Boolean;
begin
  if not GetUsableFont then
  begin
    inherited;
    Exit;
  end;

  ctrlDown :=  HasCtrlKey(shift);
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
        pos := WordIdxToPos(fCursorChunkPos);
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
        pos := WordIdxToPos(fCursorChunkPos);
        pos.Y := pos.Y + fPageMetrics.lineHeight;
        fCursorChunkPos := PosToChunkIdx(pos);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        ScrollCaretIntoView;
        Key := 0;
      end;
    else
    begin
      inherited;
      Key := 0;
      Exit;
    end;
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoDblClick;
var
  chunk: TTextChunk;
begin
  if not IsValid(fCursorChunkPos) or
    (fCursorChunkPos.X < 0) or (fCursorChunkPos.X >= fChunkedText.Count) then
      Exit;
  chunk := fChunkedText.Chunk[fCursorChunkPos.X];
  if not chunk.IsText then Exit;
  fSelStart := Types.Point(fCursorChunkPos.X, 0);
  fSelEnd := Types.Point(fCursorChunkPos.X+1, 0);
  //fSelEnd := Types.Point(fCursorChunkPos.X, chunk.length);
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
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoMouseMove(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
var
  relPos: TPointD;
begin
  inherited;
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

procedure TMemoCtrl.SetScrollV(scrollVert: TScrollCtrl);
begin
  inherited;
  if Assigned(ScrollV) then
    ScrollV.Step := Round(fPageMetrics.lineHeight);
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetText(const text: string);
begin
  inherited;
  if not GetUsableFont then Exit;
  fPageMetrics.bounds := NullRect; // force a fPageMetrics refresh
  fChunkedText.SetText(text, fUsableFont);
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

procedure TMemoCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  inherited;
end;
//------------------------------------------------------------------------------

function TMemoCtrl.WordIdxToPos(const wordIdx: TPoint): TPointD;
var
  i       : integer;
  x, spcW : double;
  chunk   : TTextChunk;
begin
  Result := NullPointD;
  if not GetUsableFont then Exit;

  //get WordIdx's start of line index
  i := 0;
  while (i < fPageMetrics.totalLines -1) and
    (wordIdx.X >= fPageMetrics.startOfLineIdx[i+1]) do inc(i);

  //with the line index we now have Y pos
  Result.Y := (i - fTopLine) * fPageMetrics.lineHeight;

  //and get the justify offset for spaces
  spcW := fPageMetrics.justifyDeltas[i];

  //get the start of the word
  i := fPageMetrics.startOfLineIdx[i];
  x := 0;
  while (i < wordIdx.X) do
    with fChunkedText[i] do
    begin
      if IsBlankSpaces then
        x := x + width + spcW else
        x := x + width;
      inc(i);
    end;

  if wordIdx.X >= fChunkedText.Count then
  begin
    with fPageMetrics do
      Result.X := lineWidths[High(lineWidths)];
  end else
  begin
    chunk := fChunkedText[wordIdx.X];
    if wordIdx.Y >= chunk.length then
      x := x + chunk.glyphOffsets[chunk.length -1] else
      x := x + chunk.glyphOffsets[wordIdx.Y];
    Result.X := x +1;
  end;

  Result.X := Result.X + fTextMargin.X;
  Result.Y := Result.Y + fTextMargin.Y;
end;
//------------------------------------------------------------------------------

function TMemoCtrl.PosToChunkIdx(const relPos: TPointD): TPoint;
var
  i: integer;
  x,x2,y2     : double;
  spcW, chrW  : double;
  chunk       : TTextChunk;
begin
  Result := NullPoint;
  if not GetUsableFont or (fPageMetrics.bounds.IsEmpty) then Exit;

  x2 := relPos.X - fTextMargin.X;
  y2 := relPos.Y - fTextMargin.Y;

  i := Floor(y2 / fPageMetrics.lineHeight);
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
    (x2 - x > (chunk.width + spcW)/2) then inc(Result.X)
  else
    while (Result.Y < High(chunk.glyphOffsets) -1) and
      (x + chunk.glyphOffsets[Result.Y] + (chunk.glyphOffsets[Result.Y+1] -
        chunk.glyphOffsets[Result.Y]) *0.5 < x2) do
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

procedure TMemoCtrl.DoScroll(dx, dy: double);
var
  visibleLines: integer;
begin
  fTopLine := Trunc(ScrollV.Position / fPageMetrics.lineHeight);
  visibleLines := GetVisibleLines;
  if fTopLine > fPageMetrics.totalLines - visibleLines then
    fTopLine := fPageMetrics.totalLines - visibleLines;
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
  y, t,b: double;
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

procedure TMemoCtrl.Repaint;
var
  i: integer;
  w: integer;
  bh: double;
  p   : TPathD;
  rec: TRectD;
  selStartTop, selStartBot, selEndTop, selEndBot: TPointD;
begin
  if not GetUsableFont then Exit;
  bh  := fBevelHeight;

  rec  := InnerRect;
  TranslateRect(rec, outerMargin, outerMargin);

  image.Clear;
  image.FillRect(Rect(rec), color);
  CheckPageMetrics;

  // HIGHLIGHT TEXT SELECTION (even before drawing background)

  if IsValid(fSelStart) and IsValid(fSelEnd)  and
    not PointsEqual(fSelStart, fSelEnd) then
  begin
    if IsCorrectPosOrder(fSelStart, fSelEnd) then
    begin
      selStartTop := WordIdxToPos(fSelStart);
      selEndTop := WordIdxToPos(fSelEnd);
    end else
    begin
      selStartTop := WordIdxToPos(fSelEnd);
      selEndTop := WordIdxToPos(fSelStart);
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
      DrawPolygon(Image, p, frNonZero, SetAlpha(clDefDark32, 32));
      DrawLine(Image, p, 1, SetAlpha(clDefDark32, 128), esClosed);
    end else
    begin
      //multi-line selection - we'll assume (pro tempore) that pt2 is below pt
      w := RectWidth(fPageMetrics.bounds) +7;
      p := Rectangle(selStartTop.X, selStartTop.Y, fTextMargin.X + w, selStartBot.Y);
      p := TranslatePath(p, OuterMargin, OuterMargin);
      p := ClipPathTopAndBot(rec, p);
      DrawPolygon(Image, p, frNonZero, SetAlpha(clDefDark32, 32));
      // draw selection outline
      p := MakePath([fTextMargin.X-3, selStartBot.Y,
        selStartTop.X, selStartBot.Y, selStartTop.X, selStartTop.Y,
        fTextMargin.X + w, selStartTop.Y, fTextMargin.X + w, selStartBot.Y]);
      p := TranslatePath(p, OuterMargin, OuterMargin);
      p := ClipPathTopAndBot(rec, p);
      DrawLine(Image, p, 1, SetAlpha(clDefDark32, 128), esButt);
      while (selStartBot.Y + 1 < selEndTop.Y) do
      begin
        selStartTop.Y := selStartBot.Y;
        selStartBot.Y := selStartBot.Y + fPageMetrics.lineHeight;
        p := Rectangle(fTextMargin.X, selStartTop.Y, fTextMargin.X + w, selStartBot.Y);
        p := TranslatePath(p, OuterMargin, OuterMargin);
        p := ClipPathTopAndBot(rec, p);
        DrawPolygon(Image, p, frNonZero, SetAlpha(clDefDark32, 32));
        // draw selection outline
        p := MakePath([fTextMargin.X-3, selStartTop.Y, fTextMargin.X-3, selStartBot.Y]);
        p := TranslatePath(p, OuterMargin, OuterMargin);
        DrawLine(Image, p, 1, SetAlpha(clDefDark32, 128), esButt);
        p := MakePath([fTextMargin.X + w, selStartTop.Y, fTextMargin.X +w, selStartBot.Y]);
        p := TranslatePath(p, OuterMargin, OuterMargin);
        DrawLine(Image, p, 1, SetAlpha(clDefDark32, 128), esButt);
      end;
      p := Rectangle(fTextMargin.X, selStartBot.Y,
        selEndTop.X, selStartBot.Y + fPageMetrics.lineHeight);
      p := TranslatePath(p, OuterMargin, OuterMargin);
      p := ClipPathTopAndBot(rec, p);
      DrawPolygon(Image, p, frNonZero, SetAlpha(clDefDark32, 32));
      // finish drawing selection outline
      p := MakePath([fTextMargin.X-3, selEndTop.Y, fTextMargin.X-3, selEndBot.Y,
        selEndBot.X, selEndBot.Y, selEndTop.X, selEndTop.Y,
        fTextMargin.X + w, selEndTop.Y]);
      p := TranslatePath(p, OuterMargin, OuterMargin);
      p := ClipPathTopAndBot(rec, p);
      DrawLine(Image, p, 1, SetAlpha(clDefDark32, 128), esButt);
    end;
  end;

  DrawEdge(Image, rec, clSilver32, clWhite32, bh);
  if HasFocus then
  begin
    Img32.Vector.InflateRect(rec, bh, bh);
    DrawLine(Image, Rectangle(rec),fBevelHeight,clDefDark32,esPolygon);
  end;

  // DRAW THE (VISIBLE) TEXT

  fChunkedText.DrawText(Image, fPageMetrics.bounds,
    taJustify, tvaTop, Types.Point(fPageMetrics.startOfLineIdx[fTopLine], 0));

  if not IsValid(fCursorChunkPos) then Exit;

  // FINALLY DRAW THE INSERTION POINT CURSOR

  i := Ceil(fPageMetrics.lineHeight * 0.1);
  selStartTop := WordIdxToPos(fCursorChunkPos);
  selStartBot := PointD(selStartTop.X, selStartTop.Y + fPageMetrics.lineHeight);

  selStartTop.Y := selStartTop.Y + i;
  selStartBot.Y := selStartBot.Y + i;

  selStartTop := TranslatePoint(selStartTop, OuterMargin, OuterMargin);
  selStartBot := TranslatePoint(selStartBot, OuterMargin, OuterMargin);
  DrawLine(Image, selStartTop, selStartBot, fPageMetrics.lineHeight/15, clMaroon32);
  with selStartTop do
    DrawLine(Image, PointD(X-4,Y), PointD(X+4,Y), fPageMetrics.lineHeight/15, clMaroon32);
  with selStartBot do
    DrawLine(Image, PointD(X-4,Y), PointD(X+4,Y), fPageMetrics.lineHeight/15, clMaroon32);

//  fScrollV.Invalidate;
end;
//------------------------------------------------------------------------------

function  TMemoCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'TextMargin' then
    TextMargin := GetPointDProp(propVal, Result)
  else Result := false;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.WriteProperties;
begin
  inherited;
  WritePointDProp('TextMargin', fTextMargin);
end;

//------------------------------------------------------------------------------
// TEditCtrl
//------------------------------------------------------------------------------

constructor TEditCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fChunkedText := TChunkedText.Create;
  Color := clWhite32;
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

procedure TEditCtrl.SetFont(font: TFontCache);
begin
  inherited;
  if not Assigned(fUsableFont) then Exit;
  fChunkedText.ApplyNewFont(font);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoKeyDown(var key: Word; Shift: TShiftState);
var
  i         : integer;
  ctrlDown  : Boolean;
  endPos    : TPoint;
  txt       : UnicodeString;
  tmpCT     : TChunkedText;
begin
  if not GetUsableFont or not IsValid(fCursorChunkPos) then
  begin
    inherited;
    Exit;
  end;

  ctrlDown := HasCtrlKey(shift);
  case key of
    VK_UP, VK_LEFT:
      begin
        if ctrlDown then
          fChunkedText.GetPrevWord(fCursorChunkPos) else
          fChunkedText.GetPrevChar(fCursorChunkPos);
        if HasShiftKey(shift) then
          fSelEnd := fCursorChunkPos else
          ResetSelPos;
        fCursorMoved := true;
        key := 0;
      end;
    VK_DOWN, VK_RIGHT:
      begin
        if ctrlDown then
          fChunkedText.GetNextWord(fCursorChunkPos) else
          fChunkedText.GetNextChar(fCursorChunkPos);
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
          SaveTextToClipboard(txt);
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
          SaveTextToClipboard(txt);
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
//            if not PointsEqual(fSelStart, fSelEnd) then
//            begin
//              fCursorChunkPos := fChunkedText.Delete(fSelStart, fSelEnd);
//              ResetSelPos;
//            end;
            tmpCT := TChunkedText.Create(txt, Font, FontColor);
            try
              for i := 0 to tmpCT.Count -1 do
                InsertChunkAtCursor(tmpCT.Chunk[i].text);
            finally
              tmpCT.Free;
            end;
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

(* nb: InsertChunkAtCursor as a non-method would require the following params:
  fChunkedText, fUsableFont, var fCursorChunkPos, fselStart & fselEnd *)
procedure TEditCtrl.InsertChunkAtCursor(const chunkTxt: UnicodeString);

  procedure InsertSpaceInsideText(spaces: UnicodeString);
  var
    chunk: TTextChunk;
    text1, text2: UnicodeString;
  begin
    chunk := fChunkedText[fCursorChunkPos.X];
    text1 := Copy(chunk.text, 1, fCursorChunkPos.Y);
    text2 := Copy(chunk.text, fCursorChunkPos.Y +1, MaxInt);
    fChunkedText.DeleteChunk(fCursorChunkPos.X);
    if text2 <> '' then
      fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, text2);
    chunk := fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, spaces);
    if text1 <> '' then
      fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, text1);
    fCursorChunkPos := Types.Point(chunk.index, chunk.length);
  end;

  procedure AddAppendSpaceChunk(spaces: UnicodeString);
  var
    chunk, lastChunk: TTextChunk;
    newSpaces: UnicodeString;
  begin
    lastChunk := fChunkedText.LastChunk;
    if (fChunkedText.Count = 0) or
      ((fCursorChunkPos.X = 0) and (fCursorChunkPos.Y = 0) and
      not fChunkedText.FirstChunk.IsBlankSpaces) then
    begin
      // adding spaces to the very beginning
      chunk := fChunkedText.InsertTextChunk(fUsableFont, 0, spaces);
      fCursorChunkPos := Types.Point(0, chunk.length);
    end
    else if not LastChunk.IsBlankSpaces and
      ((fCursorChunkPos.X = fChunkedText.Count) or
      ((fCursorChunkPos.X = fChunkedText.Count -1) and
      (fCursorChunkPos.Y = LastChunk.length))) then
    begin
      // adding spaces to the very end where the last chunk is text
      fChunkedText.InsertTextChunk(fUsableFont, fChunkedText.Count, spaces);
      fCursorChunkPos := Types.Point(fChunkedText.Count, 0);
    end
    else if (fCursorChunkPos.X = fChunkedText.Count) then
    begin
      // adding space to the very end where the last chunk is also space
      newSpaces := lastChunk.text + spaces;
      fChunkedText.DeleteChunk(lastChunk.index);
      fChunkedText.InsertTextChunk(fUsableFont, fChunkedText.Count, newSpaces);
      fCursorChunkPos := Types.Point(fChunkedText.Count, 0);
    end else
    begin
      // adding spaces somewhere that's adjacent to or inside a space chunk
      chunk := fChunkedText[fCursorChunkPos.X];
      if chunk.IsBlankSpaces then
      begin
        // merge inside the current space chunk
        newSpaces := chunk.text + spaces;
        fChunkedText.DeleteChunk(chunk.index);
        fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, newSpaces);
        Inc(fCursorChunkPos.Y, Length(spaces));
      end
      else if fCursorChunkPos.Y = 0 then
      begin
        // merge with the preceding space chunk
        chunk := chunk.Prev;
        Dec(fCursorChunkPos.X);
        newSpaces := chunk.text + spaces;
        fChunkedText.DeleteChunk(chunk.index);
        fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, newSpaces);
        fCursorChunkPos := Types.Point(fCursorChunkPos.X +1, 0);
      end else
      begin
        // merge with the trailing space chunk
        chunk := chunk.Next;
        Inc(fCursorChunkPos.X);
        newSpaces := spaces + chunk.text;
        fChunkedText.DeleteChunk(chunk.index);
        fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, newSpaces);
        fCursorChunkPos := Types.Point(fCursorChunkPos.X, Length(spaces));
      end;
    end;
  end;

  procedure InsertTextInsideSpaces(text: UnicodeString);
  var
    chunk: TTextChunk;
    space1, space2: UnicodeString;
  begin
    chunk := fChunkedText[fCursorChunkPos.X];
    space1 := Copy(chunk.text, 1, fCursorChunkPos.Y);
    space2 := Copy(chunk.text, fCursorChunkPos.Y +1, MaxInt);
    fChunkedText.DeleteChunk(fCursorChunkPos.X);
    fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, space2);
    fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, text);
    chunk := fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, space1);
    fCursorChunkPos := Types.Point(chunk.index, chunk.length);
  end;

  // AddAppendTextChunk - add text inside other text or before or after spaces
  procedure AddAppendTextChunk(text: UnicodeString);
  var
    chunk, lastChunk: TTextChunk;
    newText: UnicodeString;
  begin
    lastChunk := fChunkedText.LastChunk;
    if (fChunkedText.Count = 0) or
      ((fCursorChunkPos.X = 0) and
      not fChunkedText.FirstChunk.IsText) then
    begin
      // adding text to the very beginning
      fChunkedText.InsertTextChunk(fUsableFont, 0, text);
      fCursorChunkPos := Types.Point(0,Length(text));
    end
    else if not LastChunk.IsText and
      ((fCursorChunkPos.X = fChunkedText.Count) or
      ((fCursorChunkPos.X = LastChunk.index) and
      (fCursorChunkPos.Y = LastChunk.length))) then
    begin
      // adding text to the very end where the last chunk is spaces
      fChunkedText.InsertTextChunk(fUsableFont, fChunkedText.Count, text);
      fCursorChunkPos := Types.Point(fChunkedText.Count, 0);
    end
    else if (fCursorChunkPos.X = fChunkedText.Count) then
    begin
      // adding text to the very end where the last chunk is also text
      newText := lastChunk.text + text;
      fChunkedText.DeleteChunk(lastChunk.index);
      fChunkedText.InsertTextChunk(fUsableFont, fChunkedText.Count, newText);
      fCursorChunkPos := Types.Point(fChunkedText.Count, 0);
    end else
    begin
      // adding text somewhere that's adjacent to or inside a text chunk
      chunk := fChunkedText[fCursorChunkPos.X];
      if chunk.IsText then
      begin
        // merge inside the current text chunk
        newText := chunk.text;
        Inc(fCursorChunkPos.Y);
        Insert(text, newText, fCursorChunkPos.Y);
        fChunkedText.DeleteChunk(chunk.index);
        fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, newText);
        Inc(fCursorChunkPos.Y, Length(text));
      end
      else if fCursorChunkPos.Y = 0 then
      begin
        // merge with the preceding text chunk
        chunk := chunk.Prev;
        Dec(fCursorChunkPos.X);
        newText := chunk.text + text;
        fChunkedText.DeleteChunk(chunk.index);
        chunk :=
          fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, newText);
        fCursorChunkPos.Y := chunk.length;
      end else
      begin
        // merge with the trailing text chunk
        chunk := chunk.Next;
        Inc(fCursorChunkPos.X);
        fCursorChunkPos.Y := Length(text);
        newText := text + chunk.text;
        fChunkedText.DeleteChunk(chunk.index);
        fChunkedText.InsertTextChunk(fUsableFont, fCursorChunkPos.X, newText);
      end;
    end;
  end;

var
  chunk: TTextChunk;
begin
  if (chunkTxt = '') or (chunkTxt[1] < #32) then Exit;

  // first delete a selection range
  if not PointsEqual(fSelStart, fSelEnd) then
    fCursorChunkPos := fChunkedText.Delete(fSelStart, fSelEnd);

  if chunkTxt[1] = #32 then
  begin
    if (fCursorChunkPos.X = fChunkedText.Count) then
      AddAppendSpaceChunk(chunkTxt)
    else
    begin
      chunk := fChunkedText[fCursorChunkPos.X];
      if not chunk.IsBlankSpaces and (fCursorChunkPos.Y > 0) and
        (fCursorChunkPos.Y < chunk.length) then
          InsertSpaceInsideText(chunkTxt) else
          AddAppendSpaceChunk(chunkTxt);
    end;
    ResetSelPos;
  end else
  begin
    if (fCursorChunkPos.X = fChunkedText.Count) then
      AddAppendTextChunk(chunkTxt)
    else
    begin
      chunk := fChunkedText[fCursorChunkPos.X];
      if not chunk.IsText and (fCursorChunkPos.Y > 0) and
        (fCursorChunkPos.Y < chunk.length) then
          InsertTextInsideSpaces(chunkTxt) else
          AddAppendTextChunk(chunkTxt);
    end;
    ResetSelPos;
  end;
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
  fSelEnd := Types.Point(fCursorChunkPos.X+1, 0);
  //fSelEnd := Types.Point(fCursorChunkPos.X, chunk.length);
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

procedure TEditCtrl.DoMouseMove(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
var
  relPos: TPointD;
begin
  inherited;
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

procedure TEditCtrl.SetText(const text: string);
begin
  inherited;
  if not GetUsableFont then Exit;
  fChunkedText.SetText(text, fUsableFont, fFontColor);
  fPageMetrics.bounds := NullRect; // force a fPageMetrics refresh
  Invalidate;
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
      (fBevelHeight + TextMargin.Y) *2;
  inherited SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

function TEditCtrl.GetTextRect(stripMargins: Boolean): TRectD;
var
  om: integer;
  bh: double;
begin
  om := Round(OuterMargin);
  Result := RectD(om, om, om+Width, om+Height);
  bh := fBevelHeight;
  if stripMargins then
    InflateRect(Result, -bh -TextMargin.X, -bh -TextMargin.Y) else
    InflateRect(Result, -bh, -bh);

  if assigned(ScrollV) and ScrollV.Visible then
    Result.Right := Result.Right - ScrollV.Width;
  if assigned(ScrollH) and ScrollH.Visible then
    Result.Bottom := Result.Bottom - ScrollH.Height;
end;
//------------------------------------------------------------------------------

function TEditCtrl.ChunkIdxToPos(const chunkIdx: TPoint): TPointD;
var
  i,j, om : integer;
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
  x,d, bh : double;
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
      cwDiv2 := (glyphOffsets[Result.Y+1] - glyphOffsets[Result.Y]) * 0.5;
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

procedure TEditCtrl.Repaint;
var
  om: integer;
  textRecD, rec: TRectD;
  lh: Double;
  cursorTop, cursorBot: TPointD;
begin
  inherited;
  if not GetUsableFont then Exit;
  fLineHeight := fUsableFont.LineHeight;
  textRecD := GetTextRect(true);

  if HasFocus then
  begin
    rec := InnerRect;
    om := Round(OuterMargin);
    TranslateRect(rec, om, om);
    DrawShadowRect(Image, Rect(rec), OuterMargin);
    DrawLine(Image, Rectangle(rec), FocusLineWidth, clDefDark32, esPolygon);

    // draw selection rect ...
    if not PointsEqual(fSelStart, fSelEnd) then
    begin
      rec.Left := ChunkIdxToPos(fSelStart).X;
      rec.Right := ChunkIdxToPos(fSelEnd).X;
      lh := fLineHeight /5;
      rec.Bottom := rec.Bottom- lh;
      rec.Top := rec.Top + lh;
      DrawPolygon(Image, Rectangle(rec), frNonZero, SetAlpha(clDefDark32, 32));
      DrawLine(Image, Rectangle(rec), 1, SetAlpha(clDefDark32, 196), esClosed);
    end;
  end;

  if HasFocus then
  begin
    lh := fLineHeight /20; // non-blinking cursor width
    cursorTop := ChunkIdxToPos(fCursorChunkPos);
    cursorBot := TranslatePoint(cursorTop, 0, fLineHeight);
    cursorTop.Y := cursorTop.Y + lh;
    DrawLine(Image, cursorTop, cursorBot, lh, $88AA0000);
  end;

  fPageMetrics := fChunkedText.DrawText(Image, Rect(textRecD), taLeft, tvaMiddle);
end;
//------------------------------------------------------------------------------

function  TEditCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'TextMargin' then
    TextMargin := GetPointDProp(propVal, Result)
  else Result := false;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.WriteProperties;
begin
  inherited;
  WritePointDProp('TextMargin', fTextMargin);
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
  if CanFocus then SetFocus;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.SetText(const text: string);
begin
  inherited;
  fShortcut := GetShortcutFromCaption(text);
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
  if Enabled and CanFocus then
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
  fPadding := fPadding * value;
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.Repaint;
var
  bh, hbh: double;
  rec: TRectD;
  P   : TPathD;
  txt: string;
begin
  bh := fBevelHeight;
  hbh := bh /2;
  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));

  if GetUsableFont then txt := fText else txt := '';
  Image.Clear;
  Img32.Vector.InflateRect(rec, -hbh, -hbh);
  p := Rectangle(rec);
  if HasFocus then
  begin
    DrawShadowRect(Image, Rect(rec), OuterMargin);
    DrawBtnInternal(Image, p, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, Color);
    DrawLine(Image, Rectangle(rec), FocusLineWidth, clDefDark32, esPolygon);
  end else
    DrawBtnInternal(Image, p, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, Color);
end;
//------------------------------------------------------------------------------

function TButtonCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'Padding' then
    Padding := GetDoubleProp(propVal, Result);
end;
//------------------------------------------------------------------------------

procedure TButtonCtrl.WriteProperties;
begin
  inherited;
  WriteDoubleProp('Padding', Padding);
end;

//------------------------------------------------------------------------------
// TRoundedBtnCtrl
//------------------------------------------------------------------------------

procedure TRoundedBtnCtrl.Repaint;
var
  bh, hbh: double;
  rec: TRectD;
  p: TPathD;
  txt: string;
begin
  image.Clear;
  if not GetUsableFont then Exit;
  bh := fBevelHeight;
  hbh := bh /2;
  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  p := GetRoundedRectPath(rec);


  if GetUsableFont then txt := fText else txt := '';
  Image.Clear;
  Img32.Vector.InflateRect(rec, -hbh, -hbh);
  if HasFocus then
  begin
    DrawShadow(Image, p,
      frNonZero, OuterMargin, Angle45, $80000000, true);
    DrawBtnInternal(Image, p, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, Color);
    DrawLine(Image, p, FocusLineWidth, clDefDark32, esPolygon);
  end else
    DrawBtnInternal(Image, p, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, Color);

  if GetAlpha(Color) < 128 then
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

procedure TEllipseBtnCtrl.Repaint;
var
  bh, hbh: double;
  rec: TRectD;
  ellip: TPathD;
  txt: string;
begin
  bh := fBevelHeight;
  hbh := bh /2;
  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  image.Clear;
  InflateRect(rec, -hbh, -hbh);
  ellip := Ellipse(rec);
  if GetUsableFont then txt := fText else txt := '';
  if HasFocus then
  begin
    DrawShadow(Image, ellip,
      frNonZero, OuterMargin, Angle45, $80000000, true);
    DrawBtnInternal(Image, ellip, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, Color);
    DrawLine(Image, ellip, FocusLineWidth, clDefDark32, esPolygon);
  end else
    DrawBtnInternal(Image, ellip, txt, fUsableFont,
      bh, fPadding, fPressed, fEnabled, Color);

  if GetAlpha(Color) < 128 then
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

function TImageBtnCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'TextPosition' then
    TextPosition := TTextPosition(GetIntProp(propVal, Result))
  else if propName = 'ImageListIdx' then
    ImageListIdx := GetIntProp(propVal, Result)
  else if propName = '?ImageList' then
    ImageList := StorageManager.GetExternalProp(propVal,
      Result) as TSvgImageList32;
end;
//------------------------------------------------------------------------------

procedure TImageBtnCtrl.WriteProperties;
begin
  inherited;
  WriteIntProp('TextPosition', Ord(TextPosition));
  WriteIntProp('ImageListIdx', ImageListIdx);
  WriteExternalProp('ImageList', ImageList);
end;
//------------------------------------------------------------------------------

procedure TImageBtnCtrl.Repaint;
var
  p       : TPathD;
  rec     : TRectD;
  bh      : double;
  pDelta  : double;
  ilImage : TImage32;
  pad     : integer;
begin
  Image.Clear(Color);
  bh := fBevelHeight;
  if pressed and (bh = 0) then
    bh := DPIAware(1.5);

  pad := Round(fPadding);
  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  p := Rectangle(rec);

  //if HasFocus and not fPressed then
  if HasFocus then
  begin
    DrawShadowRect(Image, Rect(rec), OuterMargin);
    if ( bh > 0) then
      DrawBtnInternal(Image, p, '', nil,
        bh, fPadding, fPressed, enabled, color, clNone32);
    //p2 := Grow(p, nil, -FocusLineWidth/2, jsAuto, 0);
    DrawLine(Image, p, FocusLineWidth, clDefDark32, esPolygon);
  end
  else if ( bh > 0) then
    DrawBtnInternal(Image, p, '', nil,
      bh, fPadding, fPressed, enabled, color, clNone32);


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
            rec.Left := rec.Right -bh - ilImage.Width -pad +pDelta;
            rec.Top := rec.Top +(Height -ilImage.Height)/2 +pDelta;
            rec.Right := rec.Left + ilImage.Width + pDelta;
            rec.Bottom := rec.Top + ilImage.Height;
            Image.CopyBlend(ilImage, ilImage.Bounds, Rect(rec), BlendToAlpha);
            rec.Right := rec.Left - pad;
            rec.Left := OuterMargin +pad +bh +pDelta *2;
          end else
          begin
            InflateRect(rec, -pad, -pad);
            TranslateRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, rec, fText, fUsableFont, fFontColor, taLeft, tvaMiddle);
        end;
      tpTop:
        begin
          if assigned(ilImage) then
          begin
            rec.Top := rec.Bottom -bh - ilImage.Height -pad +pDelta;
            rec.Left := rec.Left +(Width -ilImage.Width)/2 +pDelta;
            rec.Bottom := rec.Top + ilImage.Height + pDelta;
            rec.Right := rec.Left + ilImage.Width;
            Image.CopyBlend(ilImage, ilImage.Bounds, Rect(rec), BlendToAlpha);
            rec.Bottom := rec.Top - pad;
            rec.Top := +OuterMargin +pad +pDelta*2;
          end else
          begin
            InflateRect(rec, -pad, -pad);
            TranslateRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, rec, fText, fUsableFont, fFontColor, taCenter, tvaTop);
        end;
      tpRight:
        begin
          if assigned(ilImage) then
          begin
            rec.Left := rec.Left +pad +pDelta;
            rec.Right := rec.Left +ilImage.Width;
            rec.Top := rec.Top +(Height -ilImage.Height)/2 +pDelta;
            rec.Bottom := rec.Top + ilImage.Height;
            Image.CopyBlend(ilImage, ilImage.Bounds, Rect(rec), BlendToAlpha);
            rec.Left := rec.Right +pad +pDelta;
            rec.Right := Image.Width - OuterMargin -pad -bh*2 +pDelta;
          end else
          begin
            InflateRect(rec, -pad -bh, -pad -bh);
            TranslateRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, rec, fText, fUsableFont, fFontColor, taRight, tvaMiddle);
        end;
      tpBottom:
        begin
          if assigned(ilImage) then
          begin
            rec.Top := rec.Top +pad +pDelta;
            rec.Bottom := rec.Top +ilImage.Height;
            rec.Left := rec.Left +(Width -ilImage.Width)/2 +pDelta;
            rec.Right := rec.Left + ilImage.Width;
            Image.CopyBlend(ilImage, ilImage.Bounds, Rect(rec), BlendToAlpha);
            rec.Top := rec.Bottom +pad +pDelta;
            rec.Bottom := Image.Height - OuterMargin -pad +pDelta -bh*2;
          end else
          begin
            InflateRect(rec, -pad, -pad);
            TranslateRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, rec, fText, fUsableFont, fFontColor, taCenter, tvaBottom);
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
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if CanFocus then SetFocus;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) and CanFocus then
  begin
    Key := 0;
    case TriState of
      tsUnknown : TriState := tsNo;
      tsYes     : TriState := tsNo;
      tsNo      : TriState := tsYes;
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
      TriState := tsYes else
      TriState := tsNo;
    Invalidate;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.Repaint;
var
  i       : integer;
  j       : double;
  bh,dx   : double;
  rec     : TRectD;
  pp      : TPathsD;
  caption : string;
begin
  image.Clear(Color);
  if not GetUsableFont then Exit;

  caption := fText;
  i := Pos('&', caption);
  if i > 0 then Delete(caption, i, 1);

  bh := fBevelHeight;
  j := fUsableFont.LineHeight - bh;
  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  rec.Bottom := rec.Top + j;
  TranslateRect(rec, 0, (Height-j) /2);
  if fTextPos = tphRight then
    rec.Right := rec.Left + j else
    rec.Left := rec.Right - j;

  if HasFocus then
    DrawShadowRect(Image, Rect(rec), OuterMargin);

  if fTextPos = tphRight then
  begin
    DrawCheckboxCtrl(Image, Rect(rec),
      bh, fTriState, not fUseCross, clWhite32, enabled);
    if HasFocus then
      DrawLine(Image, Rectangle(rec), FocusLineWidth, clDefDark32, esPolygon);
    rec := InnerRect;
    TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Left := rec.Left + j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(rec, caption, taLeft, tvaMiddle);
    dx := fUsableFont.LineHeight/24;
    pp := TranslatePath(pp, -dx, -dx);
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, clBlack32);
  end else
  begin
    DrawCheckboxCtrl(Image, Rect(rec),
      bh, fTriState, not fUseCross, clWhite32, enabled);
    if HasFocus then
      DrawLine(Image, Rectangle(rec), FocusLineWidth, clDefDark32, esPolygon);

    rec := InnerRect;
    TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Right := rec.Right - j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(rec, fText, taRight, tvaMiddle);
    dx := fUsableFont.LineHeight/24;
    pp := TranslatePath(pp, -dx, -dx);
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, clBlack32);
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

function  TCheckboxCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'TextPosition' then
    TextPosition := TTextPositionH(GetIntProp(propVal, Result))
  else if propName = 'TriState' then
    TriState := TTriState(GetIntProp(propVal, Result))
  else if propName = 'PreferCross' then
    PreferCross := GetBoolProp(propVal, Result)
  else Result := false;
end;
//------------------------------------------------------------------------------

procedure TCheckboxCtrl.WriteProperties;
begin
  inherited;
  WriteIntProp('TextPosition', Ord(TextPosition));
  WriteIntProp('TriState', Ord(TriState));
  if PreferCross then
    WriteBoolProp('PreferCross', PreferCross);
end;

//------------------------------------------------------------------------------
// TRadioBtnCtrl
//------------------------------------------------------------------------------

procedure TRadioBtnCtrl.Repaint;
var
  j,bh,dx   : double;
  rec       : TRectD;
  p         : TPathD;
  pp        : TPathsD;
begin
  image.Clear(Color);
  if not GetUsableFont then Exit;

  bh := fBevelHeight;
  j := fUsableFont.LineHeight - bh;
  rec := InnerRect;
  TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
  rec.Bottom := rec.Top + j;
  TranslateRect(rec, 0, (Height-j) /2);
  if fTextPos = tphRight then
    rec.Right := rec.Left + j else
    rec.Left := rec.Right - j;
  p := Ellipse(rec);

  if HasFocus then
    DrawShadow(Image, p,
      frNonZero, OuterMargin, Angle45, $80000000, true);

  if fTextPos = tphRight then
  begin
    DrawRadioCtrl(Image, p, bh, enabled, fTriState, clWhite32);
    if HasFocus then
      DrawLine(Image, p, FocusLineWidth, clDefDark32, esPolygon);
    rec := InnerRect;
    TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Left := rec.Left + j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(rec, fText, taLeft, tvaMiddle);
    dx := fUsableFont.LineHeight/24;
    pp := TranslatePath(pp, -dx, -dx);
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, clBlack32);
  end else
  begin
    //rec.Left := rec.Right - j;
    DrawRadioCtrl(Image, p, bh, enabled, fTriState, clWhite32);
    if HasFocus then
      DrawLine(Image, p, FocusLineWidth, clDefDark32, esPolygon);
    rec := InnerRect;
    TranslateRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Right := rec.Right - j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(rec, fText, taRight, tvaMiddle);
    dx := fUsableFont.LineHeight/24;
    pp := TranslatePath(pp, -dx, -dx);
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := TranslatePath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, clBlack32);
  end;
end;

//------------------------------------------------------------------------------
// TPanelCtrl
//------------------------------------------------------------------------------

constructor TPanelCtrl.Create(parent: TLayer32 = nil; const name: string = '');
begin
  inherited;
  //fBevHeight := 0;
  fInnerMargin := dpiAware(10);
  fShadowAngle := angle45;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.CheckScrollMax;
var
  i: integer;
  x,y: double;
begin
  if not Assigned(ScrollH) and
    not Assigned(ScrollV) then
      Exit;

  x := 0; y := 0;
  for i := 0 to ChildCount -1 do
    if (Child[i] is TCustomCtrl) then
      with TCustomCtrl(Child[i]) do
      begin
        if AutoPosition <> apNone then Continue;
        if Left + Width > x then x := Left + Width;
        if Top + Height > y then y := Top + Height;
      end;
  if Assigned(ScrollH) then
  begin
    ScrollH.Max := fScrollOffset.X + Round(x + Margin);
  end;
  if Assigned(ScrollV) then
  begin
    ScrollV.Max := fScrollOffset.Y + Round(y + Margin);
  end;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  inherited;
  CheckScrollMax;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.Scale(value: double);
begin
  inherited;
  fScrollOffset := ScalePoint(fScrollOffset, value, value);
  if fAutoPosition <> apNone then CheckScrollMax;
end;
//------------------------------------------------------------------------------

procedure TPanelCtrl.DoScroll(dx, dy: double);
var
  i: integer;
begin
  fScrollOffset := TranslatePoint(fScrollOffset, dx, dy);
  for i := 0 to ChildCount -1 do
    if not (Child[i] is TScrollCtrl) then
      Child[i].Offset(-dx, -dy);
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
  fTabHeight := fTabHeight * value;
  fTabWidth  := fTabWidth  * value;
  ResizeTabs;
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
      Child[fActiveIdx*2 +1].Visible := false;
    pagePnl := Child[index*2 +1] as TPagePnlCtrl;
    pagePnl.Visible := true;

    if Assigned(fRootCtrl.fFocusedCtrl) then
      fRootCtrl.fFocusedCtrl.KillFocus;

    for i := 0 to pagePnl.ChildCount -1 do
      if (pagePnl[i] is TCustomCtrl) and
        TCustomCtrl(pagePnl[i]).CanFocus then
      begin
        TCustomCtrl(pagePnl[i]).SetFocus;
        Break;
      end;
  end;
  fActiveIdx := index;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TPageCtrl.GetPagePanel(index: integer): TPagePnlCtrl;
begin
  if (index < 0) or (index >= ChildCount div 2) then
    Result := nil else
    Result := TPagePnlCtrl(Child[index *2 +1]);
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.DrawTabs;
var
  i,h,len : integer;
  om    : double;
  bh,hbh: integer;
  tabs  : TArrayOfString;
  rec   : TRectD;
  p     : TPathD;
  pt1,pt2 : TPointD;
begin
  len := ChildCount div 2;
  if (len = 0) or not GetUsableFont then Exit;
  SetLength(tabs, len);
  for i := 0 to len -1 do
    tabs[i] := TCustomCtrl(Child[i*2]).text;

  rec := InnerRect;
  om := OuterMargin;
  TranslateRect(rec, om, om);

  bh := Round(BevelHeight);
  hbh := Round(BevelHeight/2);
  if fTabHeight = 0 then
    h := Ceil(fUsableFont.LineHeight * 1.33) else
    h := Round(fTabHeight);

  DrawTabCtrl(Image, tabs, fTabOffsets, fUsableFont,
    BevelHeight, fActiveIdx, fTabWidth, h,
    clLiteBtn32,clDarkGray32, self.Color, clBlack32);

  pt1.X := rec.Left + fTabOffsets[fActiveIdx];
  pt1.Y := rec.Top +h +bh;

  pt2.X := rec.Left + fTabOffsets[fActiveIdx +1];
  pt2.Y := pt1.Y;

  DrawLine(Image, PointD(rec.Left +bh, pt1.Y),
    PointD(rec.Right-bh, pt1.Y), hbh, self.Color);

  Image.Clear(Rect(RectD(bh, rec.Top+ h, rec.Right, rec.Bottom)), self.Color);

  SetLength(p, 6);
  p[0] := pt2;
  p[1] := PointD(rec.Right, pt1.Y);
  p[2] := PointD(rec.Right, rec.Bottom);
  p[3] := PointD(rec.Left, rec.Bottom);
  p[4] := PointD(rec.Left, pt1.Y);
  p[5] := pt1;
  DrawEdge(Image, p, clWhite32, clSilver32, bh, false);
  DrawLine(Image, PointD(rec.Left +bh, rec.Bottom),
    PointD(rec.Right-bh, rec.Bottom), 1, clGray32);
  DrawLine(Image, PointD(rec.Right, rec.Top + h +1),
    PointD(rec.Right, rec.Bottom), 1, clGray32);
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.ResizeTabs;
var
  i, len    : integer;
  h,bh      : double;
  tabs      : TArrayOfString;
begin
  if (Width = 0) or (Height = 0) or not GetUsableFont then Exit;
  len := ChildCount div 2;
  SetLength(tabs, len);
  for i := 0 to len -1 do
    tabs[i] := TCustomCtrl(Child[i*2]).text;
  if fTabHeight = 0 then
    h := Ceil(fUsableFont.LineHeight * 1.33) else
    h := fTabHeight;
  bh := fBevelHeight;
  if fTabWidth > 0 then
    fTabOffsets := MakeCumulativeArray(BevelHeight, fTabWidth, len) else
    fTabOffsets := GetVaribleWidthTabOffsets(tabs, fUsableFont, BevelHeight);

  if StorageState = ssLoading then Exit;
  for i := 0 to len -1 do
  begin
    Child[i*2].SetInnerBounds(RectD(fTabOffsets[i], 0, fTabOffsets[i+1], h));
    Child[i*2 +1].SetInnerBounds(RectD(bh, h+bh*2, width -bh, height -bh));
  end;
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.Repaint;
var
  rec: TRect;
  bhi: integer;
  om: integer;
begin
  Image.Clear;
  if not GetUsableFont or (ChildCount = 0) then Exit;
  bhi := Round(fBevelHeight);
  om := Round(OuterMargin);
  rec := Rect(0,0, Ceil(Width),Ceil(Height));
  if (om > 0) then
    TranslateRect(rec, om, om);
  Img32.Vector.InflateRect(rec, -bhi, -bhi);
  //rec.Top := fMetrics.bounds.Bottom + om;
  if Assigned(fOnPaint) then fOnPaint(Self);
  DrawTabs;

//  if ShadowSize > 0 then
//    DrawShadowRect(Image,
//      Rect(om+bhi, fMetrics.bounds.Bottom,
//      rec.Width+om+bhi*2, rec.Height+om+bhi), ShadowSize);
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
      Text := captions[i];
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

function TPageCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'ActiveIndex' then
    ActiveIndex := GetIntProp(propVal, Result)
  else if propName = 'ShadowSize' then
    ShadowSize := GetDoubleProp(propVal, Result)
  else if propName = 'TabWidth' then
    TabWidth := GetDoubleProp(propVal, Result)
  else if propName = 'TabHeight' then
    TabHeight := GetDoubleProp(propVal, Result)
  else Result := false;
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
end;
//------------------------------------------------------------------------------

procedure TPageCtrl.WriteProperties;
begin
  inherited;
  WriteIntProp('ActiveIndex', ActiveIndex);
  WriteDoubleProp('ShadowSize', fShadowSize);
  WriteDoubleProp('TabWidth', TabWidth);
  WriteDoubleProp('TabHeight', TabHeight);
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
      if Assigned(fRootCtrl.fFocusedCtrl) then
        idx := fRootCtrl.fFocusedCtrl.Index else
        idx := 0;

      for i := idx-1 downto 0 do
        if Child[i] is TCustomCtrl then
          with TCustomCtrl(Child[i]) do
            if CanFocus then
            begin
              SetFocus;
              Key := 0;
              Exit;
            end;
      for i := ChildCount -1 downto idx do
        if Child[i] is TCustomCtrl then
          with TCustomCtrl(Child[i]) do
            if CanFocus then
            begin
              SetFocus;
              Key := 0;
              Exit;
            end;
    end else
    begin
      if Assigned(fRootCtrl.fFocusedCtrl) then
        idx := fRootCtrl.fFocusedCtrl.Index else
        idx := ChildCount -1;

      for i := idx +1 to ChildCount -1 do
        if Child[i] is TCustomCtrl then
          with TCustomCtrl(Child[i]) do
            if CanFocus then
            begin
              SetFocus;
              Key := 0;
              Exit;
            end;
      for i := 0 to idx do
        if Child[i] is TCustomCtrl then
          with TCustomCtrl(Child[i]) do
            if CanFocus then
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
    with TCustomCtrl(Child[i]) do
    if CanFocus then
    begin
      SetFocus;
      Break;
    end;
end;

//------------------------------------------------------------------------------
// TPageTabCtrl
//------------------------------------------------------------------------------

procedure TPageTabCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  TPageCtrl(parent).SetActiveIndex(Index div 2);
  inherited;
end;
//------------------------------------------------------------------------------

constructor TPageTabCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
  fCanFocus := false;
end;

//------------------------------------------------------------------------------
// TProgressCtrl
//------------------------------------------------------------------------------

constructor TProgressCtrl.Create(parent: TLayer32; const name: string);
begin
  inherited;
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
  fSize := Round(fUsableFont.LineHeight *1.1);
  SetInnerBounds(InnerBounds);
end;
//------------------------------------------------------------------------------

procedure TProgressCtrl.Repaint;
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

  c := GradientColor(fStartColor, fEndColor, Position/fMax);
  case fOrientation of
    soHorizontal:
      rec2.Right := rec2.Left + Round(Position/fMax * Width);
    soVertical:
      rec2.Bottom := rec2.top + Round(Position/fMax * Height);
  end;
  bh := Round(fBevelHeight);
  Types.InflateRect(rec2, -bh, -bh);
  Image.FillRect(rec2, c);

  if GetUsableFont then
    DrawText(Image, rec, Floattostr(Position),
      fUsableFont, fFontColor, taCenter, tvaMiddle);
end;
//------------------------------------------------------------------------------

function  TProgressCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'Max' then
    Max := GetIntProp(propVal, Result)
  else if propName = 'Position' then
    Position := GetDoubleProp(propVal, Result)
  else if propName = 'Orientation' then
    Orientation := TScrollOrientation(GetIntProp(propVal, Result))
  else if propName = 'StartColor' then
    StartColor := GetColorProp(propVal, Result)
  else if propName = 'EndColor' then
    EndColor := GetColorProp(propVal, Result)
  else Result := false;
end;
//------------------------------------------------------------------------------

procedure TProgressCtrl.WriteProperties;
begin
  inherited;
  WriteIntProp('Max', fMax);
  WriteDoubleProp('Position', fPosition);
  WriteIntProp('Orientation', Ord(fOrientation));
  WriteColorProp('StartColor', fStartColor);
  WriteColorProp('EndColor', fEndColor);
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
  if fOrientation = newOrient then Exit;
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
    fSize     := fUsableFont.LineHeight *1.1;
    SetBtnSize;
  end else
  begin
    fSize     := DPIAware(15);
    fBtnSize  := Round(fSize*2);
  end;
  fMax          := 100;
  fScrollStep   := 5;
end;
//------------------------------------------------------------------------------

function TSliderCtrl.GetDelta: double;
begin
  //Delta = real step size for one unit of position change
  case fOrientation of
    soHorizontal  : result := (Width - fBtnSize)/ (fMax-fMin);
    soVertical    : result := (Height - fBtnSize)/ (fMax-fMin);
    else Result := 0; //otherwise compiler warning !???
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
  fSize := Round(fUsableFont.LineHeight *1.1);
  SetInnerBounds(InnerBounds);
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.Repaint;
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
    soHorizontal:
     begin
      d2 := rec.Left + (fPosition -fMin) * d;
      rec := RectD(d2, rec.Top, d2+fBtnSize, rec.Top + Height + BevelHeight);
     end;
    soVertical:
      begin
        d2 := rec.Top + (fPosition -fMin) * d;
        rec := RectD(rec.Left, d2, rec.Left + Width + BevelHeight, d2+fBtnSize);
      end;
  end;
  if HasFocus then DrawShadowRect(Image, Rect(rec), OuterMargin);
  Image.FillRect(Rect(rec), clPaleGray32);
  DrawLine(Image, Rectangle(rec), dpiAware1+1, clWhite32, esPolygon);

  Img32.Vector.InflateRect(rec, -dpiAwareOne, -dpiAwareOne);
  if fPressed then
    DrawEdge(Image, rec, clBlack32, clWhite32, dpiAwareOne);
    DrawEdge(Image, rec, clWhite32, clBlack32, dpiAwareOne);

  if HasFocus then
  begin
    hbh := Round(fBevelHeight /2);
    recI := Rect(rec);
    Types.InflateRect(recI, hbh, hbh);
    DrawLine(Image, Rectangle(recI), FocusLineWidth, clDefDark32, esPolygon);
  end;


  if not GetUsableFont then Exit;
  if fPressed then
    TranslateRect(rec, DPIAwareOne, DPIAwareOne);
  DrawText(Image, rec, Floattostr(fPosition),
    fUsableFont, fFontColor, taCenter, tvaMiddle);
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetOrientation(newOrient: TScrollOrientation);
begin
  if fOrientation = newOrient then Exit;
  fOrientation := newOrient;
  SetInnerBounds(InnerBounds);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.SetPosition(newPos: double);
begin
  fPosition := Math.Min(fMax, Math.Max(fMin, newPos));
  Invalidate;
  if Assigned(fOnSlider) then fOnSlider(Self);
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
  if not CanFocus then Exit;
  SetFocus;

  pt2 := MakeRelative(PointD(pt));
  d := GetDelta * (fPosition-fMin);
  case fOrientation of
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

procedure TSliderCtrl.DoMouseMove(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
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
    soHorizontal  :
      begin
        case fScrollState of
          scLeft      :
            if pt2.X > d*(fPosition-fMin) then
              fScrollState := scScrollBtn else
              Position := Round(fPosition - Step);
          scRight     :
            if pt2.X < d*(fPosition-fMin) + fBtnSize then
              fScrollState := scScrollBtn else
              Position := Round(fPosition + Step);
        end;
        if fScrollState = scScrollBtn then
          Position := Round((pt2.X-halfBtnSize)/d) + fMin;
      end;
    soVertical    :
      begin
        case fScrollState of
          scTop      :
            if pt2.Y > d*(fPosition-fMin) then
              fScrollState := scScrollBtn else
              Position := Round(fPosition - Step);
          scBottom   :
            if pt2.Y < d*(fPosition-fMin) + fBtnSize then
              fScrollState := scScrollBtn else
              Position := Round(fPosition + Step);
        end;
        if fScrollState = scScrollBtn then
          Position := Round((pt2.Y-halfBtnSize)/d) + fMin;
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

function TSliderCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'Min' then
    Min := GetDoubleProp(propVal, Result)
  else if propName = 'Max' then
    Max := GetDoubleProp(propVal, Result)
  else if propName = 'Position' then
    Position := GetDoubleProp(propVal, Result)
  else if propName = 'Orientation' then
    Orientation := TScrollOrientation(GetIntProp(propVal, Result))
  else if propName = 'Step' then
    Step := GetIntProp(propVal, Result)
  else if propName = '?OnSlider' then
    fOnSlider := StorageManager.GetEventProp(propVal, Result);
end;
//------------------------------------------------------------------------------

procedure TSliderCtrl.WriteProperties;
begin
  inherited;
  //nb: write Min and Max before Position so they'll also be loaded first.
  //This way the Position property won't be restricted to the default range
  WriteDoubleProp('Min', fMin);
  WriteDoubleProp('Max', fMax);
  WriteIntProp('Orientation', Ord(Orientation));
  WriteDoubleProp('Position', fPosition);
  WriteIntProp('Step', Step);
  if Assigned(fOnSlider) then
    WriteEventProp('OnSlider', fOnSlider);
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
  fMinBtnSize   := DPIAware(10);
  fAutoHide     := True;
  fScrollStep   := dpiAware(5);
  OuterMargin   := 0;
end;
//------------------------------------------------------------------------------

destructor TScrollCtrl.Destroy;
begin
  UnregisterTargetCtrl;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetFocus;
begin
  if CanFocus and Assigned(fTargetCtrl) and fTargetCtrl.CanFocus then
    fTargetCtrl.SetFocus;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.RegisterTargetCtrl(target: TScrollingCtrl);
begin
  UnregisterTargetCtrl;
  fTargetCtrl := target;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.UnregisterTargetCtrl;
begin
  if not Assigned(fTargetCtrl) then Exit;
  case fOrientation of
    soHorizontal:
      if Self = fTargetCtrl.fScrollH then
        fTargetCtrl.fScrollH := nil;
    soVertical:
      if Self = fTargetCtrl.fScrollV then
        fTargetCtrl.fScrollV := nil;
  end;
  fTargetCtrl := nil;
  SetSize(0,0);
  fBtnRec.Width := 0;
  fBtnRec.Height := 0;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetMin(newMin: double);
begin
  if newMin = fMin then Exit;
  fMin := newMin;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetMax(newMax: double);
begin
  if newMax = fMax then Exit;
  fMax := newMax;
  CalcScale;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetScrollSize(newSize: double);
var
  i: double;
  rec: TRectD;
begin
  if fSize = newSize then Exit;
  fSize := newSize;
  if (fSize > 0) or not GetUsableFont then
    i := fSize else
    i := fUsableFont.LineHeight;
  rec := InnerBounds;
  case fOrientation of
    soHorizontal: rec.top := rec.Bottom - i;
    soVertical: rec.Left := rec.Right -i;
  end;
  SetInnerBounds(rec);
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.SetPosition(newPos: double);
var
  delta: double;
begin
  if not CanScroll then Exit;
  case fOrientation of
    soHorizontal  :
      if newPos > fMax - fTargetCtrl.Width then
        newPos := fMax - fTargetCtrl.Width;
    soVertical    :
      if newPos > fMax - fTargetCtrl.Height then
        newPos := fMax - fTargetCtrl.Height;
  end;
  if newPos < 0 then newPos := 0;

  delta := newPos - fPosition;
  if delta = 0 then Exit;
  fPosition := fPosition + delta;
  if not Assigned(fTargetCtrl) then Exit;
  case fOrientation of
    soHorizontal  : fTargetCtrl.DoScroll(delta, 0);
    soVertical    : fTargetCtrl.DoScroll(0, delta);
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

function TScrollCtrl.CanScroll: Boolean;
begin
  Result := Visible and (fBtnRec.Width > 0) and (fBtnRec.Height > 0)
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.DoMouseDown(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  fMouseDownPos := MakeRelative(PointD(pt));
  case fOrientation of
    soHorizontal:
      begin
        if fMouseDownPos.X < fBtnRec.Left then
        begin
          Position := Position - Step;
          fScrollState := scLeft;
        end
        else if fMouseDownPos.X > fBtnRec.Left + fBtnRec.Width then
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
        if fMouseDownPos.Y < fBtnRec.Top then
        begin
          Position := Position - Step;
          fScrollState := scTop;
        end
        else if fMouseDownPos.Y > fBtnRec.Top + fBtnRec.Height then
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

procedure TScrollCtrl.DoMouseMove(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
var
  pt2: TPointD;
begin
  if (fScrollState = scNormal) or not Assigned(fTargetCtrl) then Exit;
  pt2 := MakeRelative(PointD(pt));

  if (fScrollState <> scScrollBtn) and
    PtInRect(fBtnRec.RectD, pt2) then
      fScrollState := scScrollBtn;

  case fOrientation of
    soHorizontal  :
      case fScrollState of
        scScrollBtn : Position := fPosition +
          Round((pt2.X - fMouseDownPos.X) * fDeltaScale);
        scLeft      : Position := Position - Step;
        scRight     : Position := Position + Step;
      end;
    soVertical    :
      case fScrollState of
        scScrollBtn : Position := fPosition +
          Round((pt2.Y - fMouseDownPos.Y) * fDeltaScale);
        scTop       : Position := Position - Step;
        scBottom    : Position := Position + Step;
      end;
  end;
  fMouseDownPos := pt2;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.DoMouseUp(Button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  fScrollState := scNormal;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.Scale(value: double);
begin
  inherited;
  fSize := fSize * value;
  fMax  := fMax * value;
  fMin  := fMin * value;
  fPosition  := fPosition * value;
  SetInnerBounds(InnerBounds);
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.CalcScale;
var
  maxDelta, maxTravel, k, bh: double;
begin
  if (StorageState = ssLoading) or (fMax = 0) or not Assigned(fTargetCtrl) or
    fTargetCtrl.Image.IsEmpty then Exit;

  bh := fBevelHeight;
  case fOrientation of
    soHorizontal:
      begin
        maxDelta := fMax - fTargetCtrl.Width;
        if maxDelta <= 0 then
        begin
          if fAutoHide then Visible := false;
          Exit;
        end;
        with fTargetCtrl do
          maxTravel := Width - (bh + self.Height)*2;
        k := fMax - fTargetCtrl.Width;
      end;
    soVertical:
      begin
        maxDelta := fMax - fTargetCtrl.Height;
        if maxDelta <= 0 then
        begin
          if fAutoHide then Visible := false;
          Exit;
        end;
        with fTargetCtrl do
          maxTravel := Height - (bh + self.Width)*2;
        k := fMax - fTargetCtrl.Height;
      end;

    else Exit;
  end;
  if (maxTravel > 10) and (maxDelta > maxTravel) then
    fDeltaScale := maxDelta/maxTravel else
    fDeltaScale := 1;
  if fAutoHide then Visible := true;
  if (k > 0) and (Position > k) then Position := Round(k);
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.Repaint;
var
  bhi     : integer;
  d, bh   : double;
  bhDiv2  : double;
  rec     : TRectD;
  rec2    : TRectD;
  recI    : TRect;
  p,p2    : TPathD;
begin
  bh := fBevelHeight;
  bhi := Round(bh);
  bhDiv2 := bhi / 2;
  if fDeltaScale = 0 then fDeltaScale := 1;

  rec := InnerRect;
  TranslateRect(rec, OuterMargin, OuterMargin);
  recI := Rect(rec);
  Hatch(Image, recI, clWhite32, clBtnFace32, DPIAware(3));

  rec2 := rec;
  if fOrientation = soVertical then
  begin
    //draw the top chevron
    rec2.Bottom := rec2.Top + rec2.Width;
    d := rec2.Width * 0.3;
    InflateRect(rec2, -d, -d);
    with rec2 do
      p := MakePath([left-2, bottom, MidPoint.X, top+2, right+2, bottom]);
    p2 := TranslatePath(p, -bhDiv2, -bhDiv2);
    DrawLine(Image, p2, bhi, clWhite32, esRound);
    DrawLine(Image, p, bhi, clDarkGray32, esRound);

    //draw the bottom chevron
    rec2 := rec;
    rec2.Top := rec2.Bottom - rec2.Width;
    InflateRect(rec2, -d, -d);
    with rec2 do
      p := MakePath([left-2, top, MidPoint.X, bottom-2, right+2, top]);
    p2 := TranslatePath(p, -bhDiv2, -bhDiv2);
    DrawLine(Image, p2, bhi, clWhite32, esRound);
    DrawLine(Image, p, bhi, clDarkGray32, esRound);

    //draw the outer border
    DrawEdge(Image, rec, clSilver32, clWhite32, bh);

    if not Assigned(fTargetCtrl) or
      (fMax <= fTargetCtrl.Height) then
    begin
      fBtnRec.Width := 0;
      fBtnRec.Height := 0;
      Exit;
    end;
    fBtnRec.Left := rec.Left;
    fBtnRec.Top := Round(fPosition/fDeltaScale) + rec.Width;
    fBtnRec.Width := rec.Width;
    fBtnRec.Height := Math.Max(fMinBtnSize,
      Self.Height - (rec.Width + bh) *2 - (fMax - fTargetCtrl.Height));

    //position and draw the button
    Img32.Vector.InflateRect(rec, -1, -rec.Width);
    rec.Top := rec.Top + fPosition/fDeltaScale;
    rec.Bottom := rec.Top + fBtnRec.Height;

    rec.Left := rec.Left + bhDiv2 +2;
    Image.FillRect(Rect(rec), clPaleGray32);
    DrawLine(Image, Rectangle(rec), dpiAware1+1, clWhite32, esPolygon);
    Img32.Vector.InflateRect(rec, -1, -1);
    DrawEdge(Image, rec, clWhite32, clBlack32, 1);
  end else
  begin
    //draw the left chevron
    rec2.Right := rec2.Left + rec2.Height;
    d := rec2.Height * 0.3;
    InflateRect(rec2, -d, -d);
    with rec2 do
      p := MakePath([right, top, left +2, MidPoint.Y+1, right, bottom+2]);
    p2 := TranslatePath(p, -bhDiv2, -bhDiv2);
    DrawLine(Image, p2, bhi, clWhite32, esRound);
    DrawLine(Image, p, bhi, clDarkGray32, esRound);

    //draw the bottom chevron
    rec2 := rec;
    rec2.Left := rec2.Right - rec2.Height;
    InflateRect(rec2, -d, -d);
    with rec2 do
      p := MakePath([left, top, right -2, MidPoint.Y+1, left, bottom+2]);
    p2 := TranslatePath(p, -bhDiv2, -bhDiv2);
    DrawLine(Image, p2, bhi, clWhite32, esRound);
    DrawLine(Image, p, bhi, clDarkGray32, esRound);

    //draw the outer border
    DrawEdge(Image, rec, clSilver32, clWhite32, bh);

    if not Assigned(fTargetCtrl) or
      (fMax <= fTargetCtrl.Height) then
    begin
      fBtnRec.Width := 0;
      fBtnRec.Height := 0;
      Exit;
    end;
    fBtnRec.Top := rec.Top;
    fBtnRec.Left := Round(fPosition/fDeltaScale) + rec.Height;
    fBtnRec.Height := rec.Height;
    fBtnRec.Width := Math.Max(fMinBtnSize,
      Self.Width - (rec.Height + bh) *2 - (fMax - fTargetCtrl.Width));

    //position and draw the button
    Img32.Vector.InflateRect(rec, -rec.Height, -1);
    rec.Left := rec.Left + fPosition/fDeltaScale;
    rec.Right := rec.Left + fBtnRec.Width;

    rec.Top := rec.Top + bhDiv2 +2;
    Image.FillRect(Rect(rec), clPaleGray32);
    DrawLine(Image, Rectangle(rec), dpiAware1+1, clWhite32, esPolygon);
    Img32.Vector.InflateRect(rec, -1, -1);
    DrawEdge(Image, rec, clWhite32, clBlack32, 1);
  end;
end;
//------------------------------------------------------------------------------

function TScrollCtrl.ReadProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadProperty(propName, propVal);
  if Result then Exit
  else if propName = 'Min' then
    Min := GetDoubleProp(propVal, Result)
  else if propName = 'Max' then
    Max := GetDoubleProp(propVal, Result)
  else if propName = 'Position' then
    Position := GetDoubleProp(propVal, Result)
  else if propName = 'Size' then
    Size := GetDoubleProp(propVal, Result)
  else if propName = 'Step' then
    Step := GetIntProp(propVal, Result)
  else if propName = 'AutoHide' then
    AutoHide := GetBoolProp(propVal, Result)
  else Result := false;
end;
//------------------------------------------------------------------------------

procedure TScrollCtrl.WriteProperties;
begin
  inherited;
  WriteDoubleProp('Min', fMin);
  WriteDoubleProp('Max', fMax);
  WriteDoubleProp('Position', fPosition);
  WriteDoubleProp('Size', fSize);
  WriteIntProp('Step', Step);
  WriteBoolProp('AutoHide', AutoHide);
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
var
  i: integer;
begin
  if fAutoFree then
    for i := 0 to High(fObjectProps) do
      FreeAndNil(TObject(fObjectProps[i].address));
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

procedure TEventPropertyHandler.RefreshPropertyEvents;
var
  context: TRttiContext;
  aType: TRttiType;
  i,j: integer;
  properties: TArray<TRttiProperty>;
begin
  aType := context.GetType(self.ClassType);
  properties := (aType as TRttiInstanceType).GetDeclaredProperties;
  SetLength(fObjectProps, Length(properties));
  j := 0;
  for i := 0 to High(properties) do
  begin
    if (properties[i].PropertyType.TypeKind <> tkClass) then continue;
    fObjectProps[j].name := properties[i].name;
    fObjectProps[j].address := properties[i].GetValue(self).AsObject;
    inc(j);
  end;
  SetLength(fObjectProps, j);
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
    if prop = fObjectProps[i].address then
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
  fFocusLineW := DPIAware(1);
end;
//------------------------------------------------------------------------------

destructor TCtrlStorageManager.Destroy;
begin
  fShortcutList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.InsertChild(index: integer; storeClass: TStorageClass): TStorage;
begin
  Result := inherited InsertChild(index, storeClass);
  if storeClass.InheritsFrom(TLayeredImage32) then
    fLayeredImg := Result as TLayeredImage32
  else if storeClass.InheritsFrom(TCustomCtrl) and
    not assigned(fRootCtrl) then
      fRootCtrl := Result as TCustomCtrl;
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.GetEventName(event: TNotifyEvent): string;
begin
  if Assigned(fEventHandler) then
    Result := fEventHandler.GetEventName(event) else
    Result := '';
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.GetExternPropName(prop: TObject): string;
begin
  if Assigned(fEventHandler) then
    Result := fEventHandler.GetPropName(prop) else
    Result := '';
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

procedure TCtrlStorageManager.WriteCustomProperties;
begin
  inherited;
  if Assigned(fEventHandler) then
    WriteStrProp('EventAndPropertyHandler', fEventHandler.ClassName);
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.ReadInfoProperty(const propName, propVal: string): Boolean;
begin
  Result := inherited ReadInfoProperty(propName, propVal);
  if Result then Exit;
  if (propName = 'EventAndPropertyHandler') then
  begin
    Result := true;
    //now make sure it's the right event handler!
    if Assigned(fEventHandler) and
      not SameText(fEventHandler.ClassName, propVal) then
        fEventHandler := nil;
  end;
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.GetEventProp(const str: string; out success: Boolean): TNotifyEvent;
begin
  if Assigned(fEventHandler) then
    Result := fEventHandler.GetNotify(str) else
    Result := nil;
  success := Assigned(Result);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.DoBeforeLoad;
begin
  inherited;
  if Assigned(fEventHandler) then
    fEventHandler.RefreshPropertyEvents;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.DoAfterLoad;
begin
  inherited;
  fLayeredImg := FindObjectByClass(TLayeredImage32) as TLayeredImage32;
  if Assigned(fLayeredImg) then
  begin
    fRootCtrl := FindObjectByClass(TCustomCtrl, fLayeredImg) as TCustomCtrl;
    FindAllShortcutOwners;
  end
  else
    fRootCtrl := nil;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.DoBeforeWrite;
begin
  inherited;
  if Assigned(fEventHandler) then
    fEventHandler.RefreshPropertyEvents;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.FindAllShortcutOwners;

  procedure FindAll(ctrl: TCustomCtrl);
  var
    i: integer;
  begin
    if HasAltOrCtrlKey(ctrl.Shortcut.flags) then
      fShortcutList.Add(ctrl);
    for i := 0 to ctrl.ChildCount -1 do
      FindAll(ctrl[i] as TCustomCtrl);
  end;

begin
  fShortcutList.Clear;
  if Assigned(fRootCtrl) then
    FindAll(fRootCtrl);
end;
//------------------------------------------------------------------------------

function TCtrlStorageManager.FindShortcutOwner(const aShortcut: TShortcut): TCustomCtrl;
var
  i: integer;
begin
  for i := 0 to fShortcutList.Count -1 do
    with TCustomCtrl(fShortcutList[i]).fShortcut do
    begin
      if (flags <> aShortcut.flags) or (chr <> aShortcut.chr) then Continue;
      Result := TCustomCtrl(fShortcutList[i]);
      Exit;
    end;
  Result := nil;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.DeleteShortcutOwner(ctrl: TCustomCtrl);
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

procedure TCtrlStorageManager.AddShortcutOwner(ctrl: TCustomCtrl);
begin
  fShortcutList.Add(ctrl);
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
  if Assigned(fRootCtrl) then
    Result := fRootCtrl.fFocusedCtrl else
    Result := nil;
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.MouseDown(mouseButton: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if not Assigned(fLayeredImg) then Exit;
  fLastClicked := fLayeredImg.GetLayerAt(pt) as TCustomCtrl;
  if Assigned(fLastClicked) then
    fLastClicked.DoMouseDown(mouseButton, shift, pt);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.MouseMove(mouseButton: TMouseButton;
  shift: TShiftState; const pt: TPoint);
var
  ctrl: TLayer32;
begin
  fCurrCursor := 0;
  if not Assigned(fRootCtrl) then Exit;

  if mouseButton <> mbLeft then
  begin
    ctrl := fLayeredImg.GetLayerAt(pt);
    while Assigned(ctrl) and (ctrl is TCustomCtrl) and
      not TCustomCtrl(ctrl).Enabled do ctrl := ctrl.Parent;

    //not moving anything so just update the cursor
    if Assigned(ctrl) and not (ctrl is TPanelCtrl) then
      fCurrCursor := ctrl.CursorId;
  end else if Assigned(fLastClicked) then
    fLastClicked.DoMouseMove(mouseButton, shift, pt);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.MouseUp(mouseButton: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
  if Assigned(fLastClicked) then
    fLastClicked.DoMouseUp(mbLeft, shift, pt);
  fLastClicked := nil;
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
  if HasAltOrCtrlKey(shift) {and (Key > 47)} then
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
  if Assigned(fRootCtrl.fFocusedCtrl) then
    fRootCtrl.fFocusedCtrl.DoKeyUp(Key, shift);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.KeyPress(var Key: Char);
begin
  if Assigned(fRootCtrl.fFocusedCtrl) then
    fRootCtrl.fFocusedCtrl.DoKeyPress(Key);
end;
//------------------------------------------------------------------------------

procedure TCtrlStorageManager.Resize(width, height: Cardinal);
begin
  if Assigned(fRootCtrl) then
  begin
    fLayeredImg.SetSize(width, height);
    fRootCtrl.SetInnerBounds(RectD(0, 0, width, height));
  end;
end;

//------------------------------------------------------------------------------
// Initialization procedures
//------------------------------------------------------------------------------

procedure InitPaths;
begin
  // 100 x 100 vector icons
  SetLength(checkPaths, 2);
  checkPaths[0] := MakePath([
    100,33.76, 94.18,49.56, 78.07,61.75, 58.32,66.42, 58.32,66,
    31.14,66, 31.14,47.37, 58.32,47.37, 62.63,44.95, 70.12,40.08,
    72.82,33.76, 70.12,27.43, 62.63,22.57, 52.09,20.25, 40.91,21.03,
    31.61,24.72, 26.38,30.50, 0,25.61, 11.28,11.21, 31.27,1.95, 55.37,0,
    78.07,5.76, 94.18,17.95 ]);
  checkPaths[1] := MakePath([33,74.58, 65,74.58, 65,100, 33,100]);

  SetLength(tickPaths, 1);
  tickPaths[0] :=
    MakePath([41.02, 60.75, 100, 0, 48.71, 100, 0, 65.82, 14.10, 37.97]);
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
  RegisterStorageClass(TButtonCtrl);
  RegisterStorageClass(TCheckboxCtrl);
  RegisterStorageClass(TCustomCtrl);
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
end;

initialization
  RegisterClasses;
  InitPaths;
  InitWindowsVars;

end.
