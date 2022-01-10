unit Img32.Ctrl;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  0.0 (Experimental)                                              *
* Date      :  12 December 2021                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2021                                         *
*                                                                              *
* Purpose   :  Drawing controls (buttons, labels, edits, tabs, pages etc.)     *
*              This unit is EXPERIMENTAL. To do this properly would be a huge  *
*              task and I doubt there will be sufficient interest to just that *
*              effort since I'm largely reinventing the wheel (ie FMX).        *
*                                                                              *
* License   :  Use, modification & distribution is subject to                  *
*              Boost Software License Ver 1                                    *
*              http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

{$I Img32.inc}

uses
  SysUtils, Classes, Math, Types, Character,
  {$IFDEF XPLAT_GENERICS} Generics.Collections, Generics.Defaults,{$ENDIF}
  Img32, Img32.Storage, Img32.Vector, Img32.Text, Img32.Layers, Img32.Fmt.SVG;

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
    procedure KeyDown(var Key: Word; shift: TShiftState);
    procedure KeyUp(var Key: Word; shift: TShiftState);
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
    fDblClicked   : Boolean;
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
    procedure DoDblClick; virtual;
    procedure DoKeyDown(var Key: Word; shift: TShiftState); virtual;
    procedure DoKeyUp(var Key: Word; shift: TShiftState); virtual;
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
    fTopLine      : integer;
    fWordList     : TWordInfoList;
    fLineHeight   : double;
    fTextMargin   : TPointD;
    fPageMetrics  : TTextPageMetrics;
    fCursorWordIdx: TPoint;
    fCursorMoved  : Boolean;
    fSelStart     : TPoint;
    fSelEnd       : TPoint;
    fBuffer       : TImage32;
    fSelectRect   : TRectD;
    fDoFullPaint  : Boolean;
    procedure FillWordList;
    function  GetVisibleLines: integer;
    procedure SetTextMargin(const margin: TPointD);
    procedure InvalidatePos;
    procedure ScrollCaretIntoView;
  protected
    procedure SetFont(font: TFontCache); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
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
    function PosToWordIdx(const relPos: TPointD): TPoint;
    property TextMargin: TPointD read fTextMargin write SetTextMargin;
  end;

  TEditCtrl = class(TScrollingCtrl)
  private
    fWordList     : TWordInfoList;
    fPageMetrics  : TTextPageMetrics;
    fLineHeight   : double;
    fTextMargin   : TPointD;
    fCursorWordIdx: TPoint;
    fCursorMoved  : Boolean;
    fSelStart     : TPoint;
    fSelEnd       : TPoint;
    procedure TextChanged(Sender: TObject);
    procedure GetWordList;
    function GetTextRect(stripMargins: Boolean): TRectD;
    procedure SetTextMargin(const margin: TPointD);
  protected
    procedure SetFont(font: TFontCache); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton;
      Shift: TShiftState; const pt: TPoint); override;
    procedure SetText(const text: string); override;
    procedure FontChanged; override;
    procedure Repaint; override;
    function  ReadProperty(const propName, propVal: string): Boolean; override;
    procedure WriteProperties; override;
  public
    constructor Create(parent: TLayer32 = nil; const name: string = ''); override;
    destructor Destroy; override;
    procedure SetInnerBounds(const newBounds: TRectD); override;
    function WordIdxToPos(const wordIdx: TPoint): TPointD;
    function PosToWordIdx(const relPos: TPointD): TPoint;
    property TextMargin: TPointD read fTextMargin write SetTextMargin;
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
    fTriState : TTriState;
    fTextPos  : TTextPositionH;
    fUseCross : Boolean;
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

procedure DrawBtnInternal(Image: TImage32; const p: TPathD;
  caption: string; font: TFontCache; bevHeight, padding: double;
  pressed: Boolean; enabled: Boolean; color: TColor32 = clNone32;
  textColor: TColor32 = clBlack32; textOffX: integer = 0;
  textOffY: integer = 0; closePath: Boolean = true);
var
  dx    : double;
  pp    : TPathsD;
  rec2  : TRect;
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

  rec2 := GetBounds(p);
  if padding > 0 then
    Img32.Vector.InflateRect(rec2, -Round(padding), -Round(padding));
  if (GetAlpha(textColor) < 3) or (Trim(caption) = '') then Exit;
  pp := font.GetTextOutline(rec2, caption, taCenter, tvaMiddle, ulIdx);
  if pressed then pp := OffsetPath(pp, bevHeight, bevHeight);
  pp := OffsetPath(pp, textOffX, textOffY);

  dx := font.LineHeight/24;
  pp := OffsetPath(pp, -dx, -dx);
  DrawPolygon(Image, pp, frNonZero, clWhite32);
  pp := OffsetPath(pp, dx, dx);
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
  Result := MakePathD([rec.Left, rec.Bottom]);
  AppendPath(Result, arc(rec2, angle180, angle270));
  OffsetRect(rec2, rec.Width - radius, 0);
  p2 := arc(rec2, angle270, angle0);
  AppendPath(Result, p2);
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
  hbh := bevHeight/2;
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
        bevHeight, 0, false, true, color, textColor, 0, 0, false);
    end;

    //draw tabs following selected tab
    for i := len -1 downto selectedIdx+1 do
    begin
      rec := RectD(hbh + offsets[i], bh, hbh + offsets[i+1], bh + tabHeight);
      p := GetTabOutLine(rec, r);
      DrawBtnInternal(img, p, captions[i], font,
        bevHeight, 0, false, true, color, textColor, 0, 0, false);
    end;

    //draw selected tab
    rec := RectD(hbh + offsets[selectedIdx], bh,
      hbh + offsets[selectedIdx+1], bh + tabHeight);
    img32.Vector.InflateRect(rec, 0, bh);
    p := GetTabOutLine(rec, r);
    DrawBtnInternal(img, p, captions[selectedIdx], font,
      bevHeight, 0, false, true, selColor, selTextColor, -Round(hbh), 0, false);
    p := Grow(p, nil, hbh, jsRound, 2);
    DrawLine(img, p, DPIAware(1.2), clLiteGrey32, esSquare);

    recI.Left := Round(offsets[0]);
    recI.Top := Round(bh);
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
  d     : double;
  hbh,pw: double;
  p     : TPathD;
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
        d := Ceil(d/6);
        Img32.Vector.InflateRect(rec2, -d, -d);
        rec2.BottomRight := OffsetPoint(rec2.BottomRight, 1,1);
        DrawPolygon(Image, Rectangle(rec2), frNonZero, clLiteGray32);
      end;
    tsYes :
      begin
        pw := d/5;
        d := Ceil(d/4);
        Img32.Vector.InflateRect(rec2, -d, -d);

        if preferTick then
        begin
          p := MakePathD([42,60, 88,12, 48,91, 10,64, 21,42]);
          p := ScalePath(p, RectWidth(rec)/100);
          p := OffsetPath(p, rec.Left, rec.Top);
          DrawPolygon(Image, p, frEvenOdd, clDefDark32);
        end else
        begin
          rec2.BottomRight := OffsetPoint(rec2.BottomRight, 1,1);
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
  bc,fc : TColor32;
  p     : TPathD;
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
    tsUnknown : fc := clLiteGray32;
    tsYes     : fc := clDefDark32;
    else Exit;
  end;

  d := Ceil(d/5);
  p := Grow(ellipse, nil, -d, jsAuto, 0);
  DrawPolygon(Image, p, frNonZero, fc);
end;
//------------------------------------------------------------------------------

//function DrawPageCtrl(Image: TImage32;
//  const captions: array of string; font: TFontCache;
//  const pageRect: TRect; selectedIdx: integer;
//  bevHeight: double;  tabWidth: integer = 0; tabHeight: integer = 0;
//  inactiveTabColor: TColor32 = clLiteBtn32;
//  inactiveTextColor: TColor32 = clDarkGray32;
//  activeTabColor: TColor32 = clDarkBtn32;
//  activeTextColor: TColor32 = clBlack32): TTabCtrlMetrics;
//var
//  hbh  : integer;
//  p   : TPathD;
//  pt1 : TPointD;
//  pt2 : TPointD;
//begin
//  hbh := Ceil(bevHeight/2);
//  Result := DrawTabCtrl(Image, captions, font,
//    Types.Point(pageRect.Left + hbh*2, pageRect.Top),
//    bevHeight, selectedIdx, tabWidth, tabHeight,
//    inactiveTabColor, inactiveTextColor, activeTabColor, activeTextColor);
//
//  pt1.X := Result.offsets[selectedIdx];
//  pt1.Y := Result.bounds.Bottom;
//
//  pt2.X := Result.offsets[selectedIdx +1];
//  pt2.Y := Result.bounds.Bottom;
//
//  with Result.bounds do
//  begin
//    DrawLine(Image, PointD(Left, Bottom), PointD(Right, Bottom), hbh, activeTabColor);
//    Image.Clear(Rect(Left, Bottom, pageRect.Right, pageRect.Bottom), activeTabColor);
//  end;
//
//  SetLength(p, 6);
//  p[0] := pt2;
//  p[1] := PointD(pageRect.Right -hbh, Result.bounds.Bottom);
//  p[2] := PointD(pageRect.Right -hbh, pageRect.Bottom -hbh);
//  p[3] := PointD(pageRect.Left +hbh, pageRect.Bottom -hbh);
//  p[4] := PointD(pageRect.Left +hbh, Result.bounds.Bottom);
//  p[5] := pt1;
//  DrawEdge(Image, p, clWhite32, clSilver32, bevHeight, false);
//end;
//
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
  Color       := clBtnFace32;
  fEnabled    := true;
  fCanFocus   := true;
  fBevelHeight:= dpiAware(2);
  OuterMargin := DPIAware(5);
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
  fDblClicked := false;
  Clicked;
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseMove(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoMouseUp(button: TMouseButton;
  shift: TShiftState; const pt: TPoint);
begin
end;
//------------------------------------------------------------------------------

procedure TCustomCtrl.DoDblClick;
begin
  fDblClicked := true;
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
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
  pp := fUsableFont.GetTextOutline(rec, caption, taLeft, tvaTop, ulIdx);
  dx := fUsableFont.LineHeight/24;
  pp := OffsetPath(pp, -dx, -dx);
  DrawPolygon(Image, pp, frNonZero, clWhite32);
  pp := OffsetPath(pp, dx, dx);
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
  i: integer;
  bh: double;
begin
  if not (Parent is TPanelCtrl) then Exit;
  Image.Clear(Color);
  rec := Rect(InnerRect);
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
  bh := fBevelHeight;
  DrawEdge(Image, rec, clWhite32, clSilver32, bh);
  i := DPIAware(5);
//  Img32.Vector.InflateRect(rec, -i, -i);
//  DrawEdge(Image, rec, clSilver32, clWhite32, bh);
  Img32.Vector.InflateRect(rec, -i, 0);
  if GetUsableFont then
    DrawText(Image, rec, fText, taLeft, tvaTop, fUsableFont);
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
  //don't allow non-scroll ctrls after scroll ctrls and vice-versa
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
  if Assigned(fScrollV) and fScrollV.Visible then
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
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
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
  OffsetRect(recI, Round(OuterMargin), Round(OuterMargin));
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
      recI := Rect(rec);

      DrawText(Image, recI, fItems[i],
        taLeft, tvaMiddle, fUsableFont, clWhite32);
      OffsetRect(rec, hbh, hbh);
      DrawText(Image, recI, fItems[i], taLeft, tvaMiddle, fUsableFont);
      OffsetRect(rec, -hbh, itemH -hbh);

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
  fWordList := TWordInfoList.Create;
  Color := clWhite32;
  BlendFunc  := nil; //assumes edit controls will always be fully opaque.
  fTextMargin := PointD(20, 5);
  fCursorWordIdx := NullPoint;
  fDoFullPaint := true;
  //fPageMetrics.lineCount := -1;
end;
//------------------------------------------------------------------------------

destructor TMemoCtrl.Destroy;
begin
  ScrollV.Free;
  fWordList.Free;
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

procedure TMemoCtrl.FillWordList;
begin
  Img32.Text.FillWordList(fText, fWordList, fUsableFont);
  fDoFullPaint := true;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetFont(font: TFontCache);
begin
  inherited;
  if not Assigned(fUsableFont) then Exit;
  if fText <> '' then
    UpdateWordList(fWordList, fUsableFont);
  fDoFullPaint := true;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.ScrollCaretIntoView;
var
  visLines: integer;
begin
  visLines := GetVisibleLines;
  if fCursorWordIdx.X < fPageMetrics.wordListOffsets[fTopLine] then
  begin
    while fCursorWordIdx.X < fPageMetrics.wordListOffsets[fTopLine] do
      dec(fTopLine);
    ScrollV.Position := fTopLine * fLineHeight;
    Invalidate;
    fDoFullPaint := true;
  end
  else if (fTopLine + visLines < fPageMetrics.lineCount) and
    (fCursorWordIdx.X >= fPageMetrics.wordListOffsets[fTopLine + visLines]) then
  begin
    while (fTopLine + visLines < fPageMetrics.lineCount) and
      (fCursorWordIdx.X >= fPageMetrics.wordListOffsets[fTopLine + visLines]) do
        inc(fTopLine);
    ScrollV.Position := fTopLine * fLineHeight;
    Invalidate;
    fDoFullPaint := true;
  end
  else
    InvalidatePos;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);

  function GetPrev(ctrlDown: Boolean): Boolean;
  begin
    Result := true;
    InvalidatePos;
    if ctrlDown then
    begin
      if (fCursorWordIdx.Y = 0) and (fCursorWordIdx.X > 0) then
      begin
        dec(fCursorWordIdx.X);
        while (fCursorWordIdx.X > 0) and
          (fWordList[fCursorWordIdx.X].word = #32) do
            dec(fCursorWordIdx.X);
      end else if fCursorWordIdx.Y > 0 then fCursorWordIdx.Y := 0
      else Result := false
    end
    else if fCursorWordIdx.Y > 0 then dec(fCursorWordIdx.Y)
    else if fCursorWordIdx.X = 0 then Result := false
    else
    begin
      dec(fCursorWordIdx.X);
      with fWordList[fCursorWordIdx.X] do
        fCursorWordIdx.Y := length-1;
    end;
    ScrollCaretIntoView;
  end;

  function GetNext(ctrlDown: Boolean): Boolean;
  var
    lastWord: Boolean;
  begin
    Result := fCursorWordIdx.X < fWordList.Count;
    if not Result then Exit;

    InvalidatePos;
    lastWord := fCursorWordIdx.X = fWordList.Count -1;
    if ctrlDown and not lastWord then
    begin
      inc(fCursorWordIdx.X);
      fCursorWordIdx.Y := 0;
      while (fWordList[fCursorWordIdx.X].word = #32) do
      begin
        if fCursorWordIdx.X = fWordList.Count -1 then break;
        inc(fCursorWordIdx.X);
      end;
    end
    else if fCursorWordIdx.Y < fWordList[fCursorWordIdx.X].length -1 then
      inc(fCursorWordIdx.Y)
    else if lastWord then
      Result := false
    else
    begin
      inc(fCursorWordIdx.X);
      fCursorWordIdx.Y := 0;
    end;
    ScrollCaretIntoView;
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
        if HasShiftKey(shift) then
        begin
          if PointsEqual(fSelStart, fCursorWordIdx) then
          begin
            GetPrev(ctrlDown);
            fSelStart := fCursorWordIdx;
          end else
          begin
            GetPrev(ctrlDown);
            fSelEnd := fCursorWordIdx;
          end;
        end else
        begin
          GetPrev(ctrlDown);
          fSelStart := fCursorWordIdx;
          fSelEnd := fCursorWordIdx;
        end;
        Key := 0;
        fCursorMoved := true;
      end;
    VK_RIGHT:
      begin
        if HasShiftKey(Shift) then
        begin
          if PointsEqual(fSelStart, fCursorWordIdx) then
          begin
            GetNext(ctrlDown);
            fSelStart := fCursorWordIdx;
          end else
          begin
            GetNext(ctrlDown);
            fSelEnd := fCursorWordIdx;
          end;
        end else
        begin
          GetNext(ctrlDown);
          fSelStart := fCursorWordIdx;
          fSelEnd := fCursorWordIdx;
        end;
        Key := 0;
        fCursorMoved := true;
      end;
    VK_UP:
      begin
        pos := WordIdxToPos(fCursorWordIdx);
        pos.Y := pos.Y - fLineHeight;
        InvalidatePos;
        fCursorWordIdx := PosToWordIdx(pos);
        ScrollCaretIntoView;
        Key := 0;
      end;
    VK_DOWN:
      begin
        pos := WordIdxToPos(fCursorWordIdx);
        pos.Y := pos.Y + fLineHeight;
        InvalidatePos;
        fCursorWordIdx := PosToWordIdx(pos);
        ScrollCaretIntoView;
        Key := 0;
      end;
    else
      inherited;
  end;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoDblClick;
begin
  inherited;
  if not IsValid(fCursorWordIdx) then Exit;
  fSelStart := Types.Point(fCursorWordIdx.X, 0);
  fSelEnd := Types.Point(fCursorWordIdx.X +1, 0);
  fCursorWordIdx := fSelEnd;
  Invalidate;
end;
//------------------------------------------------------------------------------


procedure TMemoCtrl.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
var
  relPos: TPointD;
begin
  if fDblClicked then
  begin
    fDblClicked := false;
    Exit;
  end;
  inherited;
  if button <> mbLeft then Exit;

  InvalidatePos;
  relPos := MakeRelative(PointD(pt));
  fCursorWordIdx := PosToWordIdx(relPos);
  if Shift = ssShift then
  begin
    fSelEnd := fCursorWordIdx;
  end else
  begin
    fSelStart := fCursorWordIdx;
    fSelEnd := fCursorWordIdx;
  end;
  InvalidatePos;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetScrollV(scrollVert: TScrollCtrl);
begin
  inherited;
  if Assigned(ScrollV) then
    ScrollV.Step := Round(fLineHeight);
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetText(const text: string);
begin
  inherited;
  if GetUsableFont then FillWordList;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetTextMargin(const margin: TPointD);
begin
  if PointsEqual(margin, fTextMargin) then Exit;
  fTextMargin := margin;
  Invalidate;
  fDoFullPaint := true;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.SetInnerBounds(const newBounds: TRectD);
begin
  inherited;
  fTopLine := 0;
  fDoFullPaint := true;
end;
//------------------------------------------------------------------------------

function TMemoCtrl.WordIdxToPos(const wordIdx: TPoint): TPointD;
var
  i       : integer;
  x, spcW : double;
  chrOffs : TArrayOfDouble;
  wordInfo: TWordInfo;
begin
  Result := NullPointD;
  if not GetUsableFont then Exit;

  //get WordIdx's start of line index
  i := 0;
  while (i < fPageMetrics.lineCount) and
    (wordIdx.X >= fPageMetrics.wordListOffsets[i+1]) do inc(i);

  //with the line index we now have Y pos
  Result.Y := (i - fTopLine) * fLineHeight;

  //and get the justify offset for spaces
  spcW := fPageMetrics.justifyDeltas[i];

  //get the start of the word
  i := fPageMetrics.wordListOffsets[i];
  x := 0;
  while (i < wordIdx.X) do
    with fWordList[i] do
    begin
      if word = #32 then
        x := x + width + spcW else
        x := x + width;
      inc(i);
    end;

  if wordIdx.X >= fWordList.Count then
  begin
    with fPageMetrics do
      Result.X := lineWidths[High(lineWidths)];
  end else
  begin
    wordInfo := fWordList[wordIdx.X];
    chrOffs := fUsableFont.GetCharOffsets(wordInfo.word);
    if wordIdx.Y >= wordInfo.length then
      x := x + chrOffs[wordInfo.length-1];
      x := x + chrOffs[wordIdx.Y];
    Result.X := x +1;
  end;

  Result.X := Result.X + fTextMargin.X;
  Result.Y := Result.Y + fTextMargin.Y;
end;
//------------------------------------------------------------------------------

function TMemoCtrl.PosToWordIdx(const relPos: TPointD): TPoint;
var
  i: integer;
  x,x2,y2     : double;
  spcW, chrW  : double;
  wordInfo: TWordInfo;
  offs    : TArrayOfDouble;
begin
  Result := NullPoint;
  if not GetUsableFont or (fPageMetrics.lineCount = 0) then Exit;

  x2 := relPos.X - fTextMargin.X;
  y2 := relPos.Y - fTextMargin.Y;

  i := Floor(y2 / fLineHeight);
  inc(i, fTopLIne);
  if i >= fPageMetrics.lineCount then
  begin
    Result.X := fWordList.Count;
    Exit;
  end
  else if i < 0 then
    Exit;

  //get start of line
  Result.X := fPageMetrics.wordListOffsets[i];

  //and also the justify offset for spaces
  spcW := fPageMetrics.justifyDeltas[i];

  //now offset to the correct word.
  x := 0;
  wordInfo := nil;
  while (Result.X < fWordList.Count) do
  begin
    wordInfo := fWordList[Result.X];
    if wordInfo.word =  #10 then
      break
    else if wordInfo.word =  #32 then
      chrW := wordInfo.width + spcW else
      chrW := wordInfo.width;

    if x + chrW > x2 then break;
    inc(Result.X);
    x := x + chrW;
  end;

  if (Result.X >= fWordList.Count) then
  begin
    Result.X := fWordList.Count;
    Exit;
  end;

  if wordInfo.word = #10 then
    Exit;

  //and calc the char offset.
  offs := fUsableFont.GetCharOffsets(wordInfo.word);

  if (wordInfo.word = #32) and
    (x2 - x > (wordInfo.width + spcW)/2) then
      inc(Result.X)
  else
    while (Result.Y < wordInfo.length-1) and
      (x + offs[Result.Y] + (offs[Result.Y+1] -
        offs[Result.Y]) *0.5 < x2) do
          inc(Result.Y);
end;
//------------------------------------------------------------------------------

function TMemoCtrl.GetVisibleLines: integer;
begin
  if (fLineHeight > 0) and (fPageMetrics.lineCount > 0) then
    Result := Trunc((Height - fBevelHeight*2 - fTextMargin.Y*2)/fLineHeight)
  else
    Result := 0;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.DoScroll(dx, dy: double);
begin
  //fScrollOffsetY := fScrollOffsetY + dy;
  //fTopLine := Round(fScrollOffsetY/fLineHeight);
  fTopLine := Round(ScrollV.Position/fLineHeight);
  if fTopLine >= Length(fPageMetrics.wordListOffsets) then
    fTopLine := High(fPageMetrics.wordListOffsets);
  fDoFullPaint := true;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.FontChanged;
begin
  inherited;
  if not Assigned(fUsableFont) then
  begin
    fLineHeight := 0;
    Exit;
  end;
  fLineHeight := fUsableFont.LineHeight;
  if Assigned(ScrollV) then ScrollV.Step := Round(fLineHeight);
  fDoFullPaint := true;
  if (Length(Text) = 0) then Exit;
  if (fWordList.Count = 0) then
    FillWordList else
    UpdateWordList(fWordList, fUsableFont);
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.InvalidatePos;
var
  om: integer;
  d: double;
  rec: TRectD;
begin
  d := fLineHeight/4;
  rec.TopLeft := WordIdxToPos(fCursorWordIdx);
  rec.BottomRight := PointD(rec.Left, rec.Top + fLineHeight);
  InflateRect(rec, d, d);
  om := Round(OuterMargin);
  OffsetRect(rec, Round(om-TextMargin.X+10), Round(om-TextMargin.Y -6));
  Invalidate(rec);
end;
//------------------------------------------------------------------------------

procedure TMemoCtrl.Repaint;
var
  i, bottomline: integer;
  bh, textWidth: double;
  p   : TPathD;
  pp  : TPathsD;
  textRecD, rec: TRectD;
  ri: TRect;
  selStartPt, selStartP2, selEndPt, selEndPt2: TPointD;
  ScrollVAutoHide: Boolean;
  scrollShowing: Boolean;
begin

  bh  := fBevelHeight;
  scrollShowing := Assigned(ScrollV) and ScrollV.Visible;
  ScrollVAutoHide := assigned(ScrollV) and ScrollV.AutoHide;
  //if fPageMetrics is stale, re-draw the whole edit window
  if fDoFullPaint then
  begin
    fDoFullPaint := false;
    image.Clear;
    fCursorMoved := false; //unset fast redraw
    rec  := InnerRect;
    OffsetRect(rec, outerMargin, outerMargin);
    ri := Rect(rec);
    image.FillRect(ri, color);
    DrawEdge(Image, rec, clSilver32, clWhite32, bh);
    textRecD := rec;

    if HasFocus then
    begin
      Img32.Vector.InflateRect(rec, bh, bh);
      DrawLine(Image, Rectangle(rec),fBevelHeight,clDefDark32,esPolygon);
    end;

    InflateRect(textRecD, -TextMargin.X, -TextMargin.Y);
    //initially assume ScrollV is NOT required unless forced.
    if not ScrollVAutoHide and scrollShowing then
      textRecD.Right := textRecD.Right - ScrollV.Width;
    if not GetUsableFont then Exit;

    textWidth := textRecD.Width;
    fPageMetrics := GetPageMetrics(textWidth, fWordList);

    if ScrollVAutoHide then
    begin
      if (fPageMetrics.lineCount > GetVisibleLines) then
      begin
        //text exceeds space so vscroll is needed and
        //must redo GetPageMetrics
        if not ScrollV.Visible then
        begin
          ScrollV.Visible := true;
          scrollShowing := true;
        end;

        textRecD.Right := textRecD.Right - ScrollV.Width;
        textWidth := textWidth - ScrollV.Width;
        fPageMetrics := GetPageMetrics(textWidth, fWordList);
      end
      else if ScrollV.Visible then
        ScrollV.Visible := false;
    end;

    if scrollShowing then
      ScrollV.Max := Round(fLineHeight*fPageMetrics.lineCount +
        fLineHeight/2 + fTextMargin.Y*2 + fBevelHeight*2);

    pp := fUsableFont.GetTextOutline(Rect(textRecD), fWordList,
      fPageMetrics, taJustify, fTopLine, fTopLine + GetVisibleLines -1);

    DrawPolygon(Image, pp, frNonZero, clBlack32);
    //copy the edit window to reuse
    fBuffer.Assign(Image);
  end else
  begin
    textRecD  := InnerRect;
    OffsetRect(textRecD, outerMargin, outerMargin);
    InflateRect(textRecD, -TextMargin.X, -TextMargin.Y);
    if scrollShowing then
      textRecD.Right := textRecD.Right - ScrollV.Width;

    rec := UpdateInfo.updateRegion;
    ri := Rect(rec);
    Image.Copy(fBuffer, ri, ri);
    //Image.FillRect(ri, clRed32);

//    OffsetRect(fSelectRect, -OuterMargin, -OuterMargin);
//    if fCursorMoved and not fullRefresh and
//      not fSelectRect.IsEmpty then
//        Invalidate(fSelectRect); //invalidate old selection
  end;

  if not IsValid(fCursorWordIdx) then Exit;

  if IsValid(fSelStart) and IsValid(fSelEnd)  and
    not PointsEqual(fSelStart, fSelEnd) then
  begin
    selStartPt := WordIdxToPos(fSelStart);
    selStartP2 := PointD(selStartPt.X, selStartPt.Y + fLineHeight);
    selEndPt := WordIdxToPos(fSelEnd);
    selEndPt2 := PointD(selEndPt.X, selEndPt.Y + fLineHeight);

    //get the update region
    fSelectRect.Left := textRecD.Left + TextMargin.X;
    fSelectRect.Top := selStartPt.Y + TextMargin.Y;
    fSelectRect.Right := textRecD.Right + TextMargin.X;
    fSelectRect.Bottom := selEndPt2.Y + TextMargin.Y;
    fSelectRect.Normalize;

    if ValueAlmostZero(selEndPt.Y - selStartPt.Y, 0.1)  then
    begin
      //single line selection
      p := MakePathD([selStartPt.X, selStartPt.Y,
        selEndPt.X, selEndPt.Y,
        selEndPt.X, selEndPt.Y + fLineHeight,
        selStartPt.X, selStartPt.Y + fLineHeight]);
      DrawPolygon(Image, p, frNonZero, $20000000);
    end else
    begin
      //multi-line selection - we'll assume (pro tempore) that pt2 is below pt
      p := MakePathD([selStartPt.X, selStartPt.Y, textRecD.Right, selStartPt.Y,
        textRecD.Right, selStartP2.Y, selStartPt.X, selStartP2.Y]);
      DrawPolygon(Image, p, frNonZero, $20000000);
      while (selStartP2.Y + 1 < selEndPt.Y) do
      begin
        selStartPt := PointD(textRecD.Left, selStartP2.Y);
        selStartP2 := PointD(textRecD.Left, selStartP2.Y + fLineHeight);
        p := MakePathD([selStartPt.X, selStartPt.Y, textRecD.Right, selStartPt.Y,
          textRecD.Right, selStartP2.Y, selStartPt.X, selStartP2.Y]);
        DrawPolygon(Image, p, frNonZero, $20000000);
      end;
      p := MakePathD([selEndPt.X, selEndPt.Y,
        selEndPt.X, selEndPt.Y + fLineHeight,
        textRecD.Left, selEndPt.Y + fLineHeight,
        textRecD.Left, selEndPt.Y]);
      DrawPolygon(Image, p, frNonZero, $20000000);
    end;

  end else
    fSelectRect := NullRectD;

  bottomline := fTopLine + GetVisibleLines -1;
  if bottomline < FPageMetrics.lineCount -1 then
    i := fPageMetrics.wordListOffsets[bottomline +1] else
    i := fWordList.Count;
  if not IsValid(fCursorWordIdx) or
    (fCursorWordIdx.X < fPageMetrics.wordListOffsets[fTopLine]) or
    (fCursorWordIdx.X >= i) then Exit;

  i := Ceil(fLineHeight * 0.1);
  selStartPt := WordIdxToPos(fCursorWordIdx);
  selEndPt := PointD(selStartPt.X, selStartPt.Y + fLineHeight);
  selStartPt.Y := selStartPt.Y + i;
  selEndPt.Y := selEndPt.Y - i;

  if IsEmptyRect(fSelectRect) then
  begin
    fSelectRect.Left := selStartPt.X -i;
    fSelectRect.Top := selStartPt.Y -i;
    fSelectRect.Right := selEndPt.X+i;
    fSelectRect.Bottom := selEndPt.Y+i;
    fCursorMoved := false;    //reset
  end;

  selStartPt := OffsetPoint(selStartPt, OuterMargin, OuterMargin);
  selEndPt := OffsetPoint(selEndPt, OuterMargin, OuterMargin);
  DrawLine(Image, selStartPt, selEndPt, i, clDefDark32);

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
  fWordList := TWordInfoList.Create;
  fWordList.OnChanged := TextChanged;
  Color := clWhite32;
  BlendFunc  := nil; //assumes edit controls will always be fully opaque.
  fTextMargin := DPIAware(PointD(5, 1));
  fCursorWordIdx := NullPoint;
end;
//------------------------------------------------------------------------------

destructor TEditCtrl.Destroy;
begin
  fWordList.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.GetWordList;
begin
  if not Assigned(fUsableFont) then Exit;
  if fWordList.Count = 0 then
  begin
    fWordList.OnChanged := nil;
    FillWordList(fText, fWordList, fUsableFont);
    fWordList.OnChanged := TextChanged;
  end;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.TextChanged(Sender: TObject);
begin
  text := fWordList.Text;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.SetFont(font: TFontCache);
begin
  inherited;
  if not Assigned(fUsableFont) then Exit;
  UpdateWordList(fWordList, font);
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoKeyDown(var Key: Word; Shift: TShiftState);

  function GetPrev(var curr: TPoint; ctrlDown: Boolean): Boolean;
  begin
    Result := true;
    if ctrlDown then
    begin
      if (curr.Y = 0) and (curr.X > 0) then
      begin
        dec(curr.X);
        while (curr.X > 0) and (fWordList[curr.X].word = #32) do
          dec(curr.X);
      end else if curr.Y > 0 then curr.Y := 0
      else Result := false
    end
    else if curr.Y > 0 then dec(curr.Y)
    else if curr.X = 0 then Result := false
    else
    begin
      dec(curr.X);
      with fWordList[curr.X] do
        curr.Y := length-1;
    end;
  end;

  function GetNext(var curr: TPoint; ctrlDown: Boolean): Boolean;
  var
    lastWord: Boolean;
  begin
    Result := true;
    lastWord := curr.X = fWordList.Count;
    if ctrlDown and not lastWord then
    begin
      inc(curr.X);
      if curr.X = fWordList.Count then Exit;
      curr.Y := 0;
      while (fWordList[curr.X].word = #32) do
      begin
        inc(curr.X);
        if curr.X = fWordList.Count then break;
      end;
    end
    else if lastWord then
      Result := false
    else if curr.Y < fWordList[curr.X].length -1 then
      inc(curr.Y)
    else
    begin
      inc(curr.X);
      curr.Y := 0;
    end;
  end;

var
  pos: TPointD;
  ctrlDown: Boolean;
  wordInfo: TWordInfo;
  newWord, newWord2: string;
  ch: Char;
begin
  if not GetUsableFont or
    not IsValid(fCursorWordIdx) then
  begin
    inherited;
    Exit;
  end;

  ctrlDown := HasCtrlKey(shift);
  case key of
    VK_LEFT:
      begin
        if HasShiftKey(shift) then
        begin
          if PointsEqual(fSelStart, fCursorWordIdx) then
          begin
            GetPrev(fCursorWordIdx, ctrlDown);
            fSelStart := fCursorWordIdx;
          end else
          begin
            GetPrev(fCursorWordIdx, ctrlDown);
            fSelEnd := fCursorWordIdx;
          end;
        end else
        begin
          GetPrev(fCursorWordIdx, ctrlDown);
          fSelStart := fCursorWordIdx;
          fSelEnd := fCursorWordIdx;
        end;
        Key := 0;
        fCursorMoved := true;
        Invalidate;
      end;
    VK_RIGHT:
      begin
        if HasShiftKey(shift) then
        begin
          if PointsEqual(fSelStart, fCursorWordIdx) then
          begin
            GetNext(fCursorWordIdx, ctrlDown);
            fSelStart := fCursorWordIdx;
          end else
          begin
            GetNext(fCursorWordIdx, ctrlDown);
            fSelEnd := fCursorWordIdx;
          end;
        end else
        begin
          GetNext(fCursorWordIdx, ctrlDown);
          fSelStart := fCursorWordIdx;
          fSelEnd := fCursorWordIdx;
        end;
        Key := 0;
        fCursorMoved := true;
        Invalidate;
      end;
    VK_UP:
      begin
        pos := WordIdxToPos(fCursorWordIdx);
        pos.Y := pos.Y - fLineHeight;
        fCursorWordIdx := PosToWordIdx(pos);
        Key := 0;
        Invalidate;
      end;
    VK_DOWN:
      begin
        pos := WordIdxToPos(fCursorWordIdx);
        pos.Y := pos.Y + fLineHeight;
        fCursorWordIdx := PosToWordIdx(pos);
        Key := 0;
        Invalidate;
      end;
    VK_DELETE:
      if (fCursorWordIdx.X < fWordList.Count) then
      begin
        wordInfo := fWordList[fCursorWordIdx.X];
        try
          newWord := wordInfo.word;
          Delete(newWord, fCursorWordIdx.Y+1, 1);
          fWordList.Edit(fUsableFont, fCursorWordIdx.X, newWord);
          if fCursorWordIdx.Y = wordInfo.length then
            GetNext(fCursorWordIdx, false);
          Key := 0;
          Invalidate;
        except
        end;
      end;
    VK_BACK:
      if (fCursorWordIdx.X > 0) or (fCursorWordIdx.Y > 0) then
      begin
        GetPrev(fCursorWordIdx, false);
        Key := VK_DELETE;
        DoKeyDown(key, Shift);
        Exit;
      end;
    VK_SPACE:
      begin
        if (fCursorWordIdx.Y = 0) then
        begin
          fWordList.InsertSpace(fUsableFont, fCursorWordIdx.X);
          inc(fCursorWordIdx.X);
        end else
        begin
          wordInfo := fWordList[fCursorWordIdx.X];
          with wordInfo do
          begin
            newWord := copy(word, 1, fCursorWordIdx.Y);
            newWord2 := copy(word, fCursorWordIdx.Y +1, length);
            fWordList.Edit(fUsableFont, fCursorWordIdx.X, newWord);
            inc(fCursorWordIdx.X);
            fWordList.InsertWord(fUsableFont, fCursorWordIdx.X, newWord2);
            fWordList.InsertSpace(fUsableFont, fCursorWordIdx.X);
            dec(fCursorWordIdx.X);
          end;
        end;
        Key := 0;
        Invalidate;
      end;
    else if Key > VK_SPACE then
    begin
      //todo: this is still buggy
      ch := Char(Key);
      if not HasShiftKey(shift) then ch := ch.ToLower;

      if (fCursorWordIdx.X = 0) and (fCursorWordIdx.Y = 0) then
      begin
        fWordList.AddWord(fUsableFont, ch);
      end else if (fCursorWordIdx.Y > 0) then
      begin
        wordInfo := fWordList[fCursorWordIdx.X];
        newWord := wordInfo.word;
        insert(ch, newWord, fCursorWordIdx.Y +1);
        fWordList.Edit(fUsableFont, fCursorWordIdx.X, newWord);
      end else
      begin
        GetPrev(fCursorWordIdx, false);
        newWord := fWordList[fCursorWordIdx.X].word;
        fWordList.Edit(fUsableFont, fCursorWordIdx.X, newWord + ch);
      end;
      wordInfo := fWordList[fCursorWordIdx.X];

      if fCursorWordIdx.Y = High(wordInfo.word) then
      begin
        inc(fCursorWordIdx.X);
        fCursorWordIdx.Y := 0;
      end else
        inc(fCursorWordIdx.Y);
      Key := 0;
      Invalidate;
    end else
      inherited;
  end;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoDblClick;
begin
  inherited;
  if not IsValid(fCursorWordIdx) then Exit;
  fSelStart := Types.Point(fCursorWordIdx.X, 0);
  fSelEnd := Types.Point(fCursorWordIdx.X +1, 0);
  fCursorWordIdx := fSelEnd;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; const pt: TPoint);
var
  relPos: TPointD;
begin
  if fDblClicked then
  begin
    fDblClicked := false;
    Exit;
  end;
  inherited;
  if button <> mbLeft then Exit;

  relPos := MakeRelative(PointD(pt));
  fCursorWordIdx := PosToWordIdx(relPos);
  if Shift = ssShift then
  begin
    fSelEnd := fCursorWordIdx;
  end else
  begin
    fSelStart := fCursorWordIdx;
    fSelEnd := fCursorWordIdx;
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.SetText(const text: string);
begin
  inherited;
  if GetUsableFont then GetWordList;
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

function TEditCtrl.WordIdxToPos(const wordIdx: TPoint): TPointD;
var
  i,j, om : integer;
  bh      : double;
  chrOffs : TArrayOfDouble;
  wordInfo: TWordInfo;
begin
  Result := NullPointD;
  if not GetUsableFont or not IsValid(wordIdx) then Exit;

  om := Round(OuterMargin);
  bh := fBevelHeight;
  Result.Y := om + bh + fTextMargin.Y;
  Result.X := om + bh + fTextMargin.X;
  if fWordList.Count = 0 then Exit;

  if wordIdx.X > fWordList.Count then
    j := fWordList.Count else
    j := wordIdx.X;

  for i := 0 to j-1 do
    Result.X := Result.X + fWordList[i].width;

  if j = fWordList.Count then Exit;

  wordInfo := fWordList[j];
  chrOffs := fUsableFont.GetCharOffsets(wordInfo.word);
  if wordIdx.Y >= wordInfo.length then
    Result.X := Result.X + chrOffs[wordInfo.length-1];
    Result.X := Result.X + chrOffs[wordIdx.Y];
end;
//------------------------------------------------------------------------------

function TEditCtrl.PosToWordIdx(const relPos: TPointD): TPoint;
var
  x,d, bh : double;
  textRec : TRectD;
  wordInfo: TWordInfo;
  offs    : TArrayOfDouble;
begin
  Result  := NullPoint;
  textRec := GetTextRect(false);
  bh      := fBevelHeight;

  d := 0;
  x := relPos.X - fTextMargin.X - bh;
  wordInfo := nil;
  while (Result.X < fWordList.Count) do
  begin
    wordInfo := fWordList[Result.X];
    if d + wordInfo.width >= x then Break;
    d := d + wordInfo.width;
    inc(Result.X);
  end;

  if (Result.X = fWordList.Count) or (wordInfo.word[1] < #32) then Exit;

  //and calc the char offset.
  if not GetUsableFont then Exit;
  offs := fUsableFont.GetCharOffsets(wordInfo.word);

  d := x - d;
  while (Result.Y < wordInfo.length-1) and
    (d > offs[Result.Y+1]) do
      inc(Result.Y);
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.FontChanged;
begin
  inherited;
  if not Assigned(fUsableFont) then Exit;

  if (fWordList.Count = 0) and (Length(Text) > 0) then
    GetWordList else
    UpdateWordList(fWordList, font);
  SetInnerBounds(GetInnerBounds);
end;
//------------------------------------------------------------------------------

procedure TEditCtrl.Repaint;
var
  om: integer;
  pp  : TPathsD;
  textRecD, rec: TRectD;
  textRecI: TRect;
  lh: Double;
  selStartPt, selEndPt: TPointD;
begin
  inherited;
  if not GetUsableFont then Exit;
  fLineHeight := fUsableFont.LineHeight;
  textRecD := GetTextRect(true);

  textRecI := Rect(textRecD);
  fPageMetrics := GetPageMetrics(textRecD.Width, fWordList);
  pp := fUsableFont.GetTextOutline(textRecI,
    fWordList, fPageMetrics, taLeft, 0, 1);

  if HasFocus then
  begin
    rec := InnerRect;
    om := Round(OuterMargin);
    OffsetRect(rec, om, om);
    DrawShadowRect(Image, Rect(rec), OuterMargin);
    DrawLine(Image, Rectangle(rec), FocusLineWidth, clDefDark32, esPolygon);
  end;
  DrawPolygon(Image, pp, frNonZero, clBlack32);

  if not HasFocus then Exit;

  lh := fLineHeight /10;
  selStartPt := WordIdxToPos(fCursorWordIdx);
  selEndPt := OffsetPoint(selStartPt, 0, fLineHeight- lh);
  selStartPt.Y := selStartPt.Y + lh;
  DrawLine(Image, selStartPt, selEndPt, lh, clDefDark32);
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
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));

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
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
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
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
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
  OffsetRect(rec, OuterMargin, OuterMargin);
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
            OffsetRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, Rect(rec), fText, taLeft, tvaMiddle, fUsableFont);
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
            OffsetRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, Rect(rec), fText, taCenter, tvaTop, fUsableFont);
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
            OffsetRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, Rect(rec), fText, taRight, tvaMiddle, fUsableFont);
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
            OffsetRect(rec, pDelta, pDelta);
          end;
          DrawText(Image, Rect(rec), fText, taCenter, tvaBottom, fUsableFont);
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
  fPadding := DPIAware(8);
  fTextPos := tphRight;
  Color    := clNone32;
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
//  if CanFocus then SetFocus;
  if fTriState = tsNo then
    TriState := tsYes else
    TriState := tsNo;
  Invalidate;
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
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
  rec.Bottom := rec.Top + j;
  OffsetRect(rec, 0, (Height-j) /2);
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
    OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Left := rec.Left + j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(Rect(rec), caption, taLeft, tvaMiddle, i);
    dx := fUsableFont.LineHeight/24;
    pp := OffsetPath(pp, -dx, -dx);
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := OffsetPath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, clBlack32);
  end else
  begin
    DrawCheckboxCtrl(Image, Rect(rec),
      bh, fTriState, not fUseCross, clWhite32, enabled);
    if HasFocus then
      DrawLine(Image, Rectangle(rec), FocusLineWidth, clDefDark32, esPolygon);

    rec := InnerRect;
    OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Right := rec.Right - j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(Rect(rec), fText, taRight, tvaMiddle);
    dx := fUsableFont.LineHeight/24;
    pp := OffsetPath(pp, -dx, -dx);
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := OffsetPath(pp, dx, dx);
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
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
  rec.Bottom := rec.Top + j;
  OffsetRect(rec, 0, (Height-j) /2);
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
    OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Left := rec.Left + j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(Rect(rec), fText, taLeft, tvaMiddle);
    dx := fUsableFont.LineHeight/24;
    pp := OffsetPath(pp, -dx, -dx);
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := OffsetPath(pp, dx, dx);
    DrawPolygon(Image, pp, frNonZero, clBlack32);
  end else
  begin
    //rec.Left := rec.Right - j;
    DrawRadioCtrl(Image, p, bh, enabled, fTriState, clWhite32);
    if HasFocus then
      DrawLine(Image, p, FocusLineWidth, clDefDark32, esPolygon);
    rec := InnerRect;
    OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
    rec.Right := rec.Right - j;
    Img32.Vector.InflateRect(rec, -fPadding, -fPadding);

    pp := fUsableFont.GetTextOutline(Rect(rec), fText, taRight, tvaMiddle);
    dx := fUsableFont.LineHeight/24;
    pp := OffsetPath(pp, -dx, -dx);
    DrawPolygon(Image, pp, frNonZero, clWhite32);
    pp := OffsetPath(pp, dx, dx);
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
  fScrollOffset := OffsetPoint(fScrollOffset, dx, dy);
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
  OffsetRect(rec, om, om);

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
    Types.OffsetRect(rec, om, om);
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
  OffsetRect(rec, Round(OuterMargin), Round(OuterMargin));
  rec2 := Rect(rec);
  //hatch the background.
  HatchBackground(Image, rec2, clWhite32, clBtnFace32, DPIAware(2));
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
    DrawText(Image, Rect(rec),
      Floattostr(Position), taCenter, tvaMiddle, fUsableFont);
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
  OffsetRect(rec, OuterMargin, OuterMargin);
  recI := Rect(rec);
  HatchBackground(Image, recI, clWhite32, clBtnFace32, DPIAware(2));

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
    OffsetRect(rec, DPIAwareOne, DPIAwareOne);
  DrawText(Image, Rect(rec),
    Floattostr(fPosition), taCenter, tvaMiddle, fUsableFont);
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
  if (fScrollState = scNormal) or
    not Assigned(fTargetCtrl) then Exit;
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
  OffsetRect(rec, OuterMargin, OuterMargin);
  recI := Rect(rec);
  HatchBackground(Image, recI, clWhite32, clBtnFace32, DPIAware(2));

  rec2 := rec;
  if fOrientation = soVertical then
  begin
    //draw the top chevron
    rec2.Bottom := rec2.Top + rec2.Width;
    d := rec2.Width * 0.3;
    InflateRect(rec2, -d, -d);
    with rec2 do
      p := MakePathD([left-2, bottom, MidPoint.X, top+2, right+2, bottom]);
    p2 := OffsetPath(p, -bhDiv2, -bhDiv2);
    DrawLine(Image, p2, bhi, clWhite32, esRound);
    DrawLine(Image, p, bhi, clDarkGray32, esRound);

    //draw the bottom chevron
    rec2 := rec;
    rec2.Top := rec2.Bottom - rec2.Width;
    InflateRect(rec2, -d, -d);
    with rec2 do
      p := MakePathD([left-2, top, MidPoint.X, bottom-2, right+2, top]);
    p2 := OffsetPath(p, -bhDiv2, -bhDiv2);
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
      p := MakePathD([right, top, left +2, MidPoint.Y+1, right, bottom+2]);
    p2 := OffsetPath(p, -bhDiv2, -bhDiv2);
    DrawLine(Image, p2, bhi, clWhite32, esRound);
    DrawLine(Image, p, bhi, clDarkGray32, esRound);

    //draw the bottom chevron
    rec2 := rec;
    rec2.Left := rec2.Right - rec2.Height;
    InflateRect(rec2, -d, -d);
    with rec2 do
      p := MakePathD([left, top, right -2, MidPoint.Y+1, left, bottom+2]);
    p2 := OffsetPath(p, -bhDiv2, -bhDiv2);
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

procedure TCtrlStorageManager.Resize(width, height: Cardinal);
begin
  if Assigned(fRootCtrl) then
  begin
    fLayeredImg.SetSize(width, height);
    fRootCtrl.SetInnerBounds(RectD(0, 0, width, height));
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

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

end.
