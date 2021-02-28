unit ImagePanels;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  2.0                                                             *
* Date      :  30 March 2020                                                   *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  Module that uses 2 custom TPanel descendants to display images  *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Windows, Messages, Types, Graphics,
  Controls, Forms, ExtCtrls, Themes, uxTheme, Math, ShellApi, ClipBrd;

//Note: apart from re-using Image32.inc here, ImagePanels is
//completely independent of the accompanying Image32 library.
{$I Image32.inc}

type
  TShowScrollBtns = (ssbFocused, ssAlways, ssNever);

  //TDrawImageEvent: template for TImagePanel's OnDrawImage event property.
  //nb: with scaling, srcRect & dstRect may have different widths +/- heights.
  TDrawImageEvent = procedure (Sender: TObject;
    dstCanvas: TCanvas; const srcRect, dstRect: TRect) of Object;

  TFileDropEvent = procedure (Sender: TObject; const filename: string) of Object;

  //TPanelScrollbar: used internally by TImagePanel and TBitmapPanel
  TPanelScrollbar = record
    btnSize        : integer; //in dst coords
    btnDelta       : double;  //how much src moves for each px of the ScrollBar
    srcOffset      : integer; //offset in unscaled src coords
    maxSrcOffset   : double;  //max offset in unscaled src coords
    MouseOver      : Boolean;
    MouseDown      : Boolean;
    MouseDownPos   : integer;
  end;

  //TImagePanel - draws custom images using the OnDrawImage event.
  //This component is designed to display very large images (eg those
  //that are too big for single TBitmap controls), where only portions of
  //images, or smaller scales of those images are displayed at any given time.
  //For example, this component would be ideal for displaying SVG images
  //that can scale to any size. Another use might be as a display surface
  //that contains thousands of thumbnail images. This component also provides
  //the whole range of scaling, scrolling and other options that make's it a
  //very powerful image viewer.
  TImagePanel = class(TPanel)
  private
    fImageSize      : TSize;
    fScale          : double;
    fScaleMin       : double;
    fScaleMax       : double;
    fFocusedColor   : TColor;
    fUnfocusedColor : TColor;
    fMouseDown      : Boolean;
    fScrollbarVert  : TPanelScrollbar;
    fScrollbarHorz  : TPanelScrollbar;
    fAutoCenter     : Boolean;
    fAllowZoom      : Boolean;
    fAllowScroll    : Boolean;
    fShowScrollBtns : TShowScrollBtns;
    fOnDrawImage    : TDrawImageEvent;
    fOnKeyDown      : TKeyEvent;
    fOnKeyUp        : TKeyEvent;
    fOnScrolling    : TNotifyEvent;
    fOnZooming      : TNotifyEvent;
{$IFDEF GESTURES}
    fLastDistance: integer;
    fLastLocation: TPoint;
{$ENDIF}
    procedure UpdateOffsetDelta(resetOrigin: Boolean);
    function GetMinScrollBtnSize: integer;
    function GetDstOffset: TPoint;
    function GetInnerMargin: integer;
    function GetOffset: TPoint;
    procedure SetOffset(const value: TPoint);
    function GetInnerClientRect: TRect;
    procedure SetScale(scale: double);
    procedure SetScaleMin(value: double);
    procedure SetScaleMax(value: double);
    //ScaleAtPoint: zooms in or out keeping 'pt' stationary relative to display
    procedure ScaleAtPoint(scaleDelta: double; const pt: TPoint);
    procedure SetSize(size: TSize);
    function  GetColor: TColor;
    procedure SetColor(acolor: TColor);
    procedure SetAutoCenter(value: Boolean);
    procedure SetAllowZoom(value: Boolean);
    procedure SetShowScrollButtons(value: TShowScrollBtns);
{$IFDEF GESTURES}
    procedure Gesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
{$ENDIF}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMEraseBkgnd(var message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    function DoMouseWheel(Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKey); message WM_KEYUP;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
    procedure DrawImage(Sender: TObject; dstCanvas: TCanvas;
      const srcRect, dstRect: TRect); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetImage;
    procedure ScaleToFit;
    function IsEmpty: Boolean;
    function IsScaledToFit: Boolean;
    function ClientToImage(const clientPt: TPoint): TPoint;
    function ImageToClient(const surfacePt: TPoint): TPoint;
    property InnerClientRect: TRect read GetInnerClientRect;
    property InnerMargin: integer read GetInnerMargin;
    property Offset: TPoint read GetOffset write SetOffset;
  published
    //AutoCenter: centers the image when its size is less than the display size
    property AutoCenter: Boolean read fAutoCenter write SetAutoCenter;
    property Color: TColor read GetColor write SetColor;
    //FocusedColor: colour of the border when the panel is focused
    property FocusedColor: TColor read fFocusedColor write fFocusedColor;
    property UnFocusedColor: TColor read fUnfocusedColor write fUnfocusedColor;
    //Scale: image scale (between ScaleMin and ScaleMax) if AllowZoom is enabled
    property Scale: double read fScale write SetScale;
    //ImageSize: ImageSize affects both scrollings and OnDrawImage bounds
    property ImageSize: TSize read fImageSize write SetSize;
    property ScaleMin: double read fScaleMin write SetScaleMin;
    property ScaleMax: double read fScaleMax write SetScaleMax;
    //ShowScrollButtons: defaults to ssbFocused (ie only when Panel has focus)
    property ShowScrollButtons : TShowScrollBtns
      read fShowScrollBtns write SetShowScrollButtons;
    property AllowScroll: Boolean read fAllowScroll write fAllowScroll;
    property AllowZoom: Boolean read fAllowZoom write SetAllowZoom;
    //OnDrawImage: event for custom image drawing
    property OnDrawImage: TDrawImageEvent read fOnDrawImage write fOnDrawImage;
    //OnKeyDown: optional event for custom keyboard actions
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnKeyUp: TKeyEvent read fOnKeyUp write fOnKeyUp;
    property OnScrolling: TNotifyEvent read fOnScrolling write fOnScrolling;
    property OnZooming: TNotifyEvent read fOnZooming write fOnZooming;
  end;

  //TBitmapPanel - a powerful image viewer for when you only need to display
  //a single TBitmap image. It uses all the scaling and scrolling features
  //of its ancestor TImagePanel component while adding copy and paste, and
  //file drag and drop functionality too.
  TBitmapPanel = class(TImagePanel)
  private
    fBmp               : TBitmap;
    fOnFileDrop        : TFileDropEvent;
    fOnPaste           : TNotifyEvent;
    fFileDropEnabled   : Boolean;
    fCopyPasteEnabled  : Boolean;
    fOnBeginPaint      : TDrawImageEvent;
    fOnEndPaint        : TDrawImageEvent;
    procedure SetFileDropEnabled(value: Boolean);
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure DrawImage(Sender: TObject; dstCanvas: TCanvas;
      const srcRect, dstRect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearBitmap;
    function CopyToClipboard: Boolean;
    function PasteFromClipboard: Boolean;
    property Bitmap: TBitmap read fBmp;
    property CopyPasteEnabled: Boolean
      read fCopyPasteEnabled write fCopyPasteEnabled;
    property FileDropEnabled: Boolean
      read fFileDropEnabled write SetFileDropEnabled;

    property OnBeginPaint: TDrawImageEvent
      read fOnBeginPaint write fOnBeginPaint;
    property OnEndPaint: TDrawImageEvent
      read fOnEndPaint write fOnEndPaint;
    property OnFileDrop: TFileDropEvent
      read fOnFileDrop write fOnFileDrop;
  end;

procedure Register;

function Size(cx, cy: Integer): TSize;

{$IFDEF FPC}
function AlphaBlend(DC: HDC; p2, p3, p4, p5: Integer;
    DC6: HDC; p7, p8, p9, p10: Integer; p11: TBlendFunction): BOOL;
    stdcall; external 'msimg32.dll' name 'AlphaBlend';
{$ENDIF}

var
  clickPt: TPoint; //debugging

implementation

procedure Register;

begin
  RegisterComponents('Image Panels', [TImagePanel, TBitmapPanel]);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  //The minimum width for scrolling buttons. If borders are too narrow
  //to properly display scroll buttons then scroll buttons will be disabled.
  MinBorderWidth: integer = 0; //see initialization

const
  MinBitmapScale = 0.001;
  MaxBitmapScale = 1000;
  tolerance = 0.01;

type
  PColor32 = ^TColor32;
  TColor32 = Cardinal;
  TARGB = record
    case boolean of
      false: (B, G, R, A: byte);
      true: (color: TColor);
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//DpiAware: the 'standard' screen resoluton is 96dpi;
//newer monitors however typically have higher resoulutions (eg 120, 144dpi).
//Without DPI scaling, application forms and controls would get progressively
//smaller as DPI resoulutons increase.

function DpiAware(value: integer): integer;
begin
  result := MulDiv(value, Screen.PixelsPerInch, 96);
end;
//------------------------------------------------------------------------------

function Size(cx, cy: Integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;
//------------------------------------------------------------------------------

procedure ScaleRect(var R: TRect; scale: double);
begin
  if scale = 1.0 then Exit;
  R.Left   := Round(R.Left * scale);
  R.Right  := Round(R.Right * scale);
  R.Top    := Round(R.Top * scale);
  R.Bottom := Round(R.Bottom * scale);
end;
//------------------------------------------------------------------------------

procedure InflateRect(var R: TRect; dx, dy: double);
begin
  R.Left   := Round(R.Left - dx);
  R.Right  := Round(R.Right + dx);
  R.Top    := Round(R.Top - dy);
  R.Bottom := Round(R.Bottom + dy);
end;
//------------------------------------------------------------------------------

//Since record methods were only added to Delphi in version D2006,
//the following functions provide backward compatability

function RectWidth(const rec: TRect): integer;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  result := rec.Right - rec.Left;
end;
//------------------------------------------------------------------------------

procedure SetRectWidth(var rec: TRect; width: integer);
{$IFDEF INLINE} inline; {$ENDIF}
begin
  rec.Right := rec.Left + width;
end;
//------------------------------------------------------------------------------

function RectHeight(const rec: TRect): integer;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  result := rec.Bottom - rec.Top;
end;
//------------------------------------------------------------------------------

procedure SetRectHeight(var rec: TRect; height: integer);
{$IFDEF INLINE} inline; {$ENDIF}
begin
  rec.Bottom := rec.Top + height;
end;
//------------------------------------------------------------------------------

function IsEmptyRect(const rec: TRect): Boolean;
{$IFDEF INLINE} inline; {$ENDIF}
begin
  Result := (rec.Right <= rec.Left) or (rec.Bottom <= rec.Top);
end;
//------------------------------------------------------------------------------

function HasThemeManifest: boolean;
begin
  result := FindResource(hInstance, makeintresource(1), MakeIntResource(24)) > 0;
end;
//------------------------------------------------------------------------------

function GetThemeColor(const className: widestring;
  part, state, propID: integer; out Color: TColor): boolean;
var
  thmHdl: HTheme;
  clrRef: COLORREF ABSOLUTE Color;
begin
  result := false;
{$IFDEF STYLESERVICES}
  if not StyleServices.Enabled or not HasThemeManifest then exit;
{$ELSE}
  if not ThemeServices.ThemesEnabled or not HasThemeManifest then exit;
{$ENDIF}
  thmHdl := OpenThemeData(0, LPCWSTR(className));
  if thmHdl <> 0 then
  try
    result :=
      Succeeded(uxTheme.GetThemeColor(thmHdl, part, state, propID, clrRef));
  finally
    CloseThemeData(thmHdl);
  end;
end;
//------------------------------------------------------------------------------

function OffsetPoint(const pt: TPoint; dx, dy: integer): TPoint;
begin
  Result.X := pt.X + dx;
  Result.Y := pt.Y + dy;
end;
//------------------------------------------------------------------------------

function LeftMouseBtnDown: Boolean;
begin
  Result := (GetKeyState(VK_LBUTTON) shr 8 > 0);
end;
//------------------------------------------------------------------------------

function MakeDarker(color: TColor; percent: integer): TColor;
var
  pcFrac: double;
  r: TARGB absolute Result;
begin
  percent := Max(0, Min(100, percent));
  pcFrac := percent/100;
  Result := ColorToRGB(color);
  r.R := r.R - Round(r.R * pcFrac);
  r.G := r.G - Round(r.G * pcFrac);
  r.B := r.B - Round(r.B * pcFrac);
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

constructor TImagePanel.Create(AOwner: TComponent);
begin
  inherited;
  Height := 200;
  Width  := 200;

  BevelWidth := 1;
  BorderWidth := 16;
  BevelInner := bvLowered;
  DoubleBuffered := true;
  TabStop := true;
  {$IFDEF GESTURES}
  OnGesture := Gesture;
  Touch.InteractiveGestures := [igPressAndTap, igZoom, igPan];
  {$ENDIF}

  fShowScrollBtns := ssbFocused;
  fAllowScroll := true;
  fAllowZoom := true;
  fAutoCenter := true;
  fFocusedColor := clActiveCaption;
  fUnfocusedColor := clBtnFace;

  fScale := 1.0;
  fScaleMin := 0.05;
  fScaleMax := 20;
  fImageSize := Size(200,200);
end;
//------------------------------------------------------------------------------

destructor TImagePanel.Destroy;
begin
  inherited;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.ResetImage;
begin
  SetScale(1.0);
  fScrollbarHorz.srcOffset := 0;
  fScrollbarVert.srcOffset := 0;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateOffsetDelta(true);
end;
//------------------------------------------------------------------------------

function TImagePanel.GetDstOffset: TPoint;
begin
  if not fAutoCenter then
    Result := Point(0,0)
  else
    with GetInnerClientRect do
    begin
      Result.X := Max(0, ((Right -Left) -Round(fImageSize.cx * fScale)) div 2);
      Result.Y := Max(0, ((Bottom -Top) -Round(fImageSize.cy * fScale)) div 2);
    end;
end;
//------------------------------------------------------------------------------

function TImagePanel.GetInnerMargin: integer;
begin
  //nb: BorderWidth is the space between outer and inner bevels
  Result := BorderWidth;
  if BevelInner <> bvNone then inc(result, BevelWidth);
  if BevelOuter <> bvNone then inc(result, BevelWidth);
  //BorderStyle changes the OUTSIDE of the panel so won't affect InnerMargin.
end;
//------------------------------------------------------------------------------

function TImagePanel.GetInnerClientRect: TRect;
var
  marg: integer;
begin
  marg := GetInnerMargin;
  result := ClientRect;
  InflateRect(result, -marg, -marg);
end;
//------------------------------------------------------------------------------

function TImagePanel.GetOffset: TPoint;
begin
  with fScrollbarHorz do Result.X := srcOffset;
  with fScrollbarVert do Result.Y := srcOffset;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetOffset(const value: TPoint);
begin
  fScrollbarHorz.srcOffset := value.X;
  fScrollbarVert.srcOffset := value.Y;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TImagePanel.IsEmpty: Boolean;
begin
  Result := (fImageSize.cx = 0) or (fImageSize.cy = 0);
end;
//------------------------------------------------------------------------------

function TImagePanel.IsScaledToFit: Boolean;
var
  rec: TRect;
  h,w: integer;
begin
  rec := GetInnerClientRect;
  h := RectHeight(rec); w := RectWidth(rec);
  Result := (abs(fImageSize.cx * fScale - w) < 1) or
    (abs(fImageSize.cy * fScale - h) < 1);
end;
//------------------------------------------------------------------------------

procedure TImagePanel.ScaleToFit;
var
  rec: TRect;
  h,w: integer;
begin
  if IsEmpty then Exit;
  ResetImage;
  rec := GetInnerClientRect;
  h := RectHeight(rec); w := RectWidth(rec);
  //fZoomFit := true;
  if w / fImageSize.cx < h / fImageSize.cy then
    SetScale(w / fImageSize.cx) else
    SetScale(h / fImageSize.cy);
end;
//------------------------------------------------------------------------------

procedure TImagePanel.ScaleAtPoint(scaleDelta: double; const pt: TPoint);
var
  marg: integer;
  p,q: double;
  pt1, pt2: TPoint;
begin
  p := scaleDelta * fScale;
  if p < fScaleMin then
  begin
    if fScale <= fScaleMin then Exit;
    scaleDelta := fScaleMin/fScale;
  end else if p > fScaleMax then
  begin
    if fScale >= fScaleMax then Exit;
    scaleDelta := fScaleMax/fScale;
  end;
  q := 1 - 1/scaleDelta;
  marg := GetInnerMargin;
  pt1 := ClientToImage(pt);
  pt2 := ClientToImage(Point(marg, marg));
  SetScale(fScale * scaleDelta);
  with fScrollbarHorz do
    inc(srcOffset, Round((pt1.X - pt2.X) * q));
  with fScrollbarVert do
    inc(srcOffset, Round((pt1.Y - pt2.Y) * q));
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetScale(scale: double);
begin
  if scale < fScaleMin then scale := fScaleMin
  else if scale > fScaleMax then scale := fScaleMax;
  if (fScale = scale) then Exit;
  fScale := scale;
  UpdateOffsetDelta(false);
  if Assigned(fOnZooming) then fOnZooming(Self);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetScaleMin(value: double);
begin
  fScaleMin := Max(MinBitmapScale, Min(fScaleMax, value));
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetScaleMax(value: double);
begin
  fScaleMax := Max(fScaleMin, Min(MaxBitmapScale, value));
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetSize(size: TSize);
begin
  if (fImageSize.cx = size.cx) and (fImageSize.cy = size.cy) then Exit;
  fImageSize.cx := size.cx;
  fImageSize.cy := size.cy;
  if not HandleAllocated then Exit;
  UpdateOffsetDelta(true);
  Invalidate;
end;
//------------------------------------------------------------------------------

function  TImagePanel.GetColor: TColor;
begin
  Result := inherited Color;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetColor(acolor: TColor);
begin
  if inherited Color = acolor then Exit;
  ParentBackground := false;
  ParentColor := false;
  inherited Color := acolor
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetAutoCenter(value: Boolean);
begin
  if value = fAutoCenter then Exit;
  fAutoCenter := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetAllowZoom(value: Boolean);
begin
  if value = fAllowZoom then Exit;
  fAllowZoom := value;
  if value then fAllowScroll := true;
end;
//------------------------------------------------------------------------------

function TImagePanel.GetMinScrollBtnSize: integer;
begin
  Result := Max(1, GetInnerMargin - DpiAware(5));
end;
//------------------------------------------------------------------------------

procedure TImagePanel.UpdateOffsetDelta(resetOrigin: Boolean);
var
  innerRec: TRect;
  innerWidth, innerHeight, btnMin: integer;
  scaledX, scaledY: double;
begin
  //we need to determine 2 things:
  //  1. scroll button size
  //  2. how much a 1px button move moves the scaled image

  btnMin      := GetMinScrollBtnSize;
  innerRec    := GetInnerClientRect;
  innerWidth  := innerRec.Right - innerRec.Left;
  innerHeight := innerRec.Bottom - innerRec.Top;
  scaledX     := fImageSize.cx * fScale;
  scaledY     := fImageSize.cy * fScale;
  with fScrollbarVert do
  begin
    if resetOrigin then srcOffset := 0;
    if (scaledY = 0) or
      (scaledY < innerHeight + tolerance) then //no scroll button needed
    begin
      btnSize := 0; btnDelta := 0; maxSrcOffset := 0;
    end else
    begin
      btnSize := Max(btnMin, Round(innerHeight * innerHeight / scaledY));
      maxSrcOffset := (scaledY - innerHeight) / fScale;
      btnDelta := (innerHeight - btnSize) / maxSrcOffset;
    end;
  end;

  with fScrollbarHorz do
  begin
    if resetOrigin then srcOffset := 0;
    if (scaledX = 0) or (scaledX < innerWidth + tolerance) then  //no scroll button needed
    begin
      btnSize := 0; btnDelta := 0; maxSrcOffset := 0;
    end else
    begin
      btnSize := Max(btnMin, Round(innerWidth * innerWidth / scaledX));
      maxSrcOffset := (scaledX - innerWidth) / fScale;
      btnDelta := (innerWidth - btnSize) / maxSrcOffset;
    end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font.Assign(Self.Font);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.SetShowScrollButtons(value: TShowScrollBtns);
begin
  if value = fShowScrollBtns then Exit;
  fShowScrollBtns := value;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TImagePanel.ClientToImage(const clientPt: TPoint): TPoint;
var
  marg: integer;
  pt: TPoint;
begin
  pt := GetDstOffset;
  marg := GetInnerMargin;
  Result.X := Round((clientPt.X -pt.X -marg)/fScale) +fScrollbarHorz.srcOffset;
  Result.Y := Round((clientPt.Y -pt.Y -marg)/fScale) +fScrollbarVert.srcOffset;
end;
//------------------------------------------------------------------------------

function TImagePanel.ImageToClient(const surfacePt: TPoint): TPoint;
var
  marg: integer;
  pt: TPoint;
begin
  pt := GetDstOffset;
  marg := GetInnerMargin;
  Result.X := Round((surfacePt.X -fScrollbarHorz.srcOffset)*fScale +marg +pt.X);
  Result.Y := Round((surfacePt.Y -fScrollbarVert.srcOffset)*fScale +marg +pt.Y);
end;
//------------------------------------------------------------------------------

procedure TImagePanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  rec: TRect;
begin
  inherited;

  if fAllowScroll then
  begin
    fMouseDown := true;
    fScrollbarHorz.MouseDownPos := X;
    fScrollbarVert.MouseDownPos := Y;
    rec := GetInnerClientRect;
    if (X > rec.Right) and (Y > rec.Top) and (Y < rec.Bottom) and
      (fScrollbarVert.btnSize > 0) then
    begin
      fScrollbarVert.MouseDown := true;
      Invalidate;
    end
    else if (Y > rec.Bottom)  and (X > rec.Left) and (X < rec.Right) and
      (fScrollbarHorz.btnSize > 0) then
    begin
      fScrollbarHorz.MouseDown := true;
      Invalidate;
    end;
  end;

  if TabStop and not Focused and CanFocus then
  begin
    SetFocus;
    Invalidate;
  end;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  marg: integer;
  rec: TRect;
  inDrawRegion, mobH, mobV: Boolean;
begin
  inherited;
  if not fAllowScroll then Exit;

  rec := GetInnerClientRect;
  inDrawRegion := PtInRect(rec, Point(X,Y));
//  if inDrawRegion then
//  begin
//    cursor := crHandPoint;
//  end;

  if not fMouseDown then
  begin
    if (BorderWidth >= MinBorderWidth) and
      fAllowScroll and ((fShowScrollBtns = ssAlways) or
      (focused and (fShowScrollBtns = ssbFocused))) then
    begin
      marg := GetInnerMargin;
      mobV := (X > ClientWidth - marg) and
        (Y < ClientHeight - marg) and (Y > marg) and
        (fScrollbarVert.btnSize > 0);
      mobH := (Y > ClientHeight - marg) and
        (X < ClientWidth - marg) and (X > marg) and
        (fScrollbarHorz.btnSize > 0);
      //now check for change in state ...
      if fScrollbarVert.MouseOver <> mobV then
      begin
        fScrollbarVert.MouseOver := mobV;
        if mobV then Cursor := crSizeNS;
        Invalidate;
      end;
      if fScrollbarHorz.MouseOver <> mobH then
      begin
        fScrollbarHorz.MouseOver := mobH;
        if mobH then Cursor := crSizeWE;
        Invalidate;
      end;
      if not inDrawRegion and not mobV and not mobH then
        cursor := crDefault;
    end;
    Exit;
  end;

  fScrollbarHorz.MouseOver := false;
  fScrollbarVert.MouseOver := false;
  if fScrollbarVert.MouseDown then
  begin
    //dragging vertical scrollbar
    with fScrollbarVert do
    begin
      inc(srcOffset, Round((Y - MouseDownPos) / btnDelta));
      MouseDownPos := Y;
    end;
  end
  else if fScrollbarHorz.MouseDown then
  begin
    //dragging horizontal scrollbar
    with fScrollbarHorz do
    begin
      inc(srcOffset, Round((X - MouseDownPos) / btnDelta));
      MouseDownPos := X;
    end;
  end else
  begin
    //click and drag the drawing image
    with fScrollbarVert do if btnDelta > 0 then
    begin
      dec(srcOffset, Round((Y - MouseDownPos) / fScale));
      MouseDownPos := Y;
    end;
    with fScrollbarHorz do if btnDelta > 0 then
    begin
      dec(srcOffset, Round((X - MouseDownPos) / fScale));
      MouseDownPos := X;
    end;
  end;
  if assigned(fOnScrolling) then fOnScrolling(self);
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not fAllowScroll or not fMouseDown then Exit;
  fMouseDown := false;
  fScrollbarHorz.MouseDown := false;
  fScrollbarHorz.MouseOver := false;
  fScrollbarVert.MouseDown := false;
  fScrollbarVert.MouseOver := false;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if fScrollbarHorz.MouseOver then
    fScrollbarHorz.MouseOver := false
  else if fScrollbarVert.MouseOver then
    fScrollbarVert.MouseOver := false
  else
    Exit;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.CMFocusChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.WMEraseBkgnd(var message: TMessage);
begin
  message.Result := 0; //ie don't bother erasing background
end;
//------------------------------------------------------------------------------

procedure TImagePanel.DrawImage(Sender: TObject; dstCanvas: TCanvas;
  const srcRect, dstRect: TRect);
begin
end;
//------------------------------------------------------------------------------

type TControl = class(Controls.TControl); //access protected Color property

procedure TImagePanel.Paint;

  procedure DrawFrame(rec: TRect; tlColor, brColor: TColor; width: integer);
  var
    bl, tr: TPoint;
  begin
    dec(rec.Right); dec(rec.Bottom);
    Canvas.Pen.Width := 1;
    while width > 0 do
    begin
      tr := Point(rec.Right, rec.Top);
      bl := Point(rec.Left, rec.Bottom);
      Canvas.Pen.Color := tlColor;
      Canvas.PolyLine([bl, rec.TopLeft, tr]);
      Canvas.Pen.Color := brColor;
      Canvas.PolyLine([tr, rec.BottomRight, bl]);
      InflateRect(rec, -1, -1);
      dec(width);
    end;
  end;

  procedure DrawScrollButton(const rec: TRect);
  begin
    Canvas.FillRect(rec);
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.MoveTo(rec.Left, rec.Bottom);
    Canvas.LineTo(rec.Left, rec.Top);
    Canvas.LineTo(rec.Right, rec.Top);
    Canvas.Pen.Color := cl3DDkShadow;
    Canvas.LineTo(rec.Right, rec.Bottom);
    Canvas.LineTo(rec.Left, rec.Bottom);
  end;

var
  marg, btnMin: integer;
  tmpRec, innerRec, srcRec, dstRec: TRect;
  backgroundPainted: Boolean;
  pt: TPoint;
begin
  //calculate un-scaled source rectangle that corresponds with dstRec
  marg := GetInnerMargin;
  innerRec := GetInnerClientRect;
  dstRec := innerRec;
  srcRec := dstRec;
  OffsetRect(srcRec, -marg, -marg);
  ScaleRect(srcRec, 1/fScale);

  //if the scaled drawing is smaller than InnerClientRect then center it
  pt := GetDstOffset;
  if pt.X > 0 then
  begin
    inc(dstRec.Left, pt.X); dec(dstRec.Right, pt.X);
    fScrollbarHorz.srcOffset := 0;
    srcRec.Left := 0;
    srcRec.Right := fImageSize.cx;
  end;
  if pt.Y > 0 then
  begin
    inc(dstRec.Top, pt.Y); dec(dstRec.Bottom, pt.Y);
    fScrollbarVert.srcOffset := 0;
    srcRec.Top := 0;
    srcRec.Bottom := fImageSize.cy;
  end;

  //calc offsets
  with fScrollbarHorz do
    if (srcOffset < 0) or (btnSize = 0) then srcOffset := 0;
  with fScrollbarVert do
    if (srcOffset < 0) or (btnSize = 0) then srcOffset := 0;

  if fScrollbarVert.srcOffset > fScrollbarVert.maxSrcOffset then
    fScrollbarVert.srcOffset := Round(fScrollbarVert.maxSrcOffset);
  if fScrollbarHorz.srcOffset > fScrollbarHorz.maxSrcOffset then
    fScrollbarHorz.srcOffset := Round(fScrollbarHorz.maxSrcOffset);
  OffsetRect(srcRec, fScrollbarHorz.srcOffset, fScrollbarVert.srcOffset);

  //paint innerRec background
  backgroundPainted := ParentBackground and
  {$IFDEF STYLESERVICES}
    StyleServices.Enabled and (seClient in StyleElements) and
  {$ELSE}
    ThemeServices.ThemesEnabled and
  {$ENDIF}
    Succeeded(DrawThemeParentBackground(Handle, Canvas.Handle, @innerRec));
  if not backgroundPainted then
  begin
    if ParentColor then
      Canvas.Brush.Color := TControl(parent).Color else
      Canvas.Brush.Color := self.Color;
    Canvas.FillRect(innerRec);
  end;

  //draw the image
  if Assigned(fOnDrawImage) then
    fOnDrawImage(Self, Canvas, srcRec, dstRec) else
    DrawImage(Self, Canvas, srcRec, dstRec);
  //prevent recursive paints (in case Invalidate etc called in fOnDrawImage)
  RedrawWindow(Handle, nil, 0, RDW_NOERASE or RDW_NOINTERNALPAINT or RDW_VALIDATE);

  //paint the outer bevel
  tmpRec := ClientRect;
  case BevelOuter of
    bvLowered: DrawFrame(tmpRec, clBtnShadow, clBtnHighlight, BevelWidth);
    bvRaised:  DrawFrame(tmpRec, clBtnHighlight, clBtnShadow, BevelWidth);
  end;

  //paint the border
  InflateRect(tmpRec, -BevelWidth, -BevelWidth);
  if Focused then
    DrawFrame(tmpRec, fFocusedColor, fFocusedColor, BorderWidth) else
    DrawFrame(tmpRec, fUnfocusedColor, fUnfocusedColor, BorderWidth);
  InflateRect(tmpRec, -BorderWidth, -BorderWidth);

  //paint the inner bevel
  case BevelInner of
    bvLowered: DrawFrame(tmpRec, clBtnShadow, clBtnHighlight, BevelWidth);
    bvRaised:  DrawFrame(tmpRec, clBtnHighlight, clBtnShadow, BevelWidth);
  end;

  if (BorderWidth >= MinBorderWidth) and
    fAllowScroll and ((fShowScrollBtns = ssAlways) or
    (Focused and (fShowScrollBtns = ssbFocused))) then
  begin
    btnMin := GetMinScrollBtnSize;

    //draw vertical scrollbar
    with fScrollbarVert do
      if (btnSize > 0) then
      begin
        tmpRec.Top := marg + Round(srcOffset * btnDelta);
        tmpRec.Bottom := tmpRec.Top + btnSize;
        tmpRec.Right := ClientWidth - DpiAware(3);
        tmpRec.Left := tmpRec.Right - btnMin;
        if MouseOver or MouseDown then Canvas.Brush.Color := clHotLight
        else if Focused then Canvas.Brush.Color := MakeDarker(fFocusedColor, 20)
        else Canvas.Brush.Color := MakeDarker(Color, 20);
        DrawScrollButton(tmpRec);
      end;

    //draw horizontal scrollbar
    with fScrollbarHorz do
      if (btnSize > 0) then
      begin
        tmpRec.Left := marg + Round(srcOffset * btnDelta);
        tmpRec.Right := tmpRec.Left + btnSize;
        tmpRec.Bottom := ClientHeight - DpiAware(3);
        tmpRec.Top := tmpRec.Bottom - btnMin;
        if MouseOver or MouseDown then Canvas.Brush.Color := clHotLight
        else if Focused then Canvas.Brush.Color := MakeDarker(fFocusedColor, 20)
        else Canvas.Brush.Color := MakeDarker(Color, 20);
        DrawScrollButton(tmpRec);
      end;
  end;
end;
//------------------------------------------------------------------------------

{$IFDEF GESTURES}
procedure TImagePanel.Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  p: double;
begin
  inherited;

  case EventInfo.GestureID of
    igiZoom:
      begin
        if not fAllowZoom then Exit;
        if not (gfBegin in EventInfo.Flags) then
        begin
          p := 1 + (EventInfo.Distance - FLastDistance)/fImageSize.cx;
          ScaleAtPoint(p, EventInfo.Location);
        end;
        FLastDistance := EventInfo.Distance;
        Handled := true;
      end;
    igiPan:
      begin
        if not fAllowScroll then Exit;
        if not (gfBegin in EventInfo.Flags) then
        begin
          with fScrollbarHorz do
            inc(srcOffset,
              Round((FLastLocation.X - EventInfo.Location.X) * btnDelta));
          with fScrollbarVert do
            inc(srcOffset,
              Round((FLastLocation.Y - EventInfo.Location.Y) * btnDelta));
          Invalidate;
        end;
        FLastLocation := EventInfo.Location;
        if assigned(fOnScrolling) then fOnScrolling(self);
        Handled := true;
      end;
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TImagePanel.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result or not focused then Exit;
  {$IFNDEF FPC}
  MousePos := ScreenToClient(MousePos);
  {$ENDIF}
  if (ssCtrl in Shift) and fAllowZoom then
  begin
    if WheelDelta > 0 then
      ScaleAtPoint(1.1, MousePos) else
      ScaleAtPoint(0.9, MousePos);
  end
  else if fAllowScroll then
  begin
    dec(fScrollbarVert.srcOffset, Round(WheelDelta / fScale));
    Invalidate;
  end;
  Result := true;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if not TabStop then Exit;
  Message.Result := Message.Result or DLGC_WANTCHARS or DLGC_WANTARROWS;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.WMKeyDown(var Message: TWMKey);
var
  mul: integer;
  midPoint: TPoint;
  charCode: Word;
  shiftState: TShiftState;
begin
  inherited;
  if not fAllowZoom and not fAllowScroll then Exit;

  shiftState := KeyDataToShiftState(Message.KeyData);

  if Assigned(fOnKeyDown) then
  begin
    charCode := Message.CharCode;
    fOnKeyDown(Self, charCode, shiftState);
    if charCode = 0 then Exit;
  end;

  case Message.CharCode of

    VK_LEFT..VK_DOWN:
      begin
        if ssCtrl in shiftState then
        begin
          if not fAllowZoom then Exit;
          //zoom in and out with CTRL+UP and CTRL+DOWN respectively
          midPoint := Point(ClientWidth div 2, ClientHeight div 2);
          case Message.CharCode of
            VK_UP: ScaleAtPoint(1.1, midPoint);
            VK_DOWN: ScaleAtPoint(0.9, midPoint);
            else Exit;
          end;
        end else
        begin
          if not fAllowScroll then Exit;
          //otherwise scroll the image with the arrow keys
          if ssShift in shiftState then
            mul := 5 else //ie scrolls 5 times faster with Shift key down
            mul := 1;
          case Message.CharCode of
            VK_LEFT:
              with fScrollbarHorz do
                dec(srcOffset, 5 * mul);
            VK_RIGHT:
              with fScrollbarHorz do
                inc(srcOffset, 5 * mul);
            VK_UP:
              with fScrollbarVert do
                dec(srcOffset, 5 * mul);
            VK_DOWN:
              with fScrollbarVert do
                inc(srcOffset, 5 * mul);
          end;
          if assigned(fOnScrolling) then fOnScrolling(self);
        end;
        Invalidate;
      end;

      Ord('0'):
        if fAllowZoom and not (ssCtrl in shiftState) then
        begin
          ScaleToFit;
        end;
      Ord('1')..Ord('9'): if not (ssCtrl in shiftState) then
        begin
          if not AllowZoom then Exit
          else if ssShift in shiftState then
            SetScale((Message.CharCode - Ord('0')) /10)
          else
            SetScale(Message.CharCode - Ord('0'));
        end;
  end;
end;
//------------------------------------------------------------------------------

procedure TImagePanel.WMKeyUp(var Message: TWMKey);
var
  charCode: Word;
  shiftState: TShiftState;
begin
  if Assigned(fOnKeyUp) then
  begin
    shiftState := KeyDataToShiftState(Message.KeyData);
    charCode := Message.CharCode;
    fOnKeyUp(Self, charCode, shiftState);
    if charCode = 0 then Exit;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
// TPnlBitmap - TBitmap descendant that's used by TBitmapPanel internally.
//              It just updates TBitmapPanel's ImageSize property automatically.
//------------------------------------------------------------------------------

type
  TPnlBitmap = class(TBitmap)
  private
    fOwner: TBitmapPanel;
  protected
    procedure Changed(Sender: TObject); override;
  end;

procedure TPnlBitmap.Changed(Sender: TObject);
begin
  inherited;
  if (fOwner.fImageSize.cx <> Width) or
    (fOwner.fImageSize.cy <> Height) then
  begin
    fOwner.SetSize(Size(Width, Height));
    fOwner.ResetImage;
  end else
    fOwner.Invalidate;
end;

//------------------------------------------------------------------------------
// TBitmapPanel
//------------------------------------------------------------------------------

constructor TBitmapPanel.Create(AOwner: TComponent);
begin
  inherited;
  fBmp := TPnlBitmap.Create;
  TPnlBitmap(fBmp).fOwner := self;
  fBmp.Width := 200;
  fBmp.Height := 200;
end;
//------------------------------------------------------------------------------

destructor TBitmapPanel.Destroy;
begin
  if fFileDropEnabled and HandleAllocated then
    DragAcceptFiles(Handle, False);
  fBmp.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TBitmapPanel.ClearBitmap;
begin
  if fBmp.Empty then Exit;
  if fBmp.PixelFormat = pf32bit then
  begin
    with fBmp do FillChar(ScanLine[Height -1]^, Width * Height * 4, 0);
  end else
  begin
    fBmp.Canvas.Brush.Color := Color;
    with fBmp do Canvas.FillRect(Rect(0, 0, Width, Height));
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TBitmapPanel.DrawImage(Sender: TObject; dstCanvas: TCanvas;
  const srcRect, dstRect: TRect);
var
  bf: BLENDFUNCTION;
  tmpBmp: TBitmap;
begin
  if IsEmptyRect(srcRect) or IsEmptyRect(dstRect) then Exit;

  if Assigned(fOnBeginPaint) then
  begin
    fOnBeginPaint(Self, dstCanvas, srcRect, dstRect);
    //and prevent recursive OnPoint calls
    RedrawWindow(Handle, nil, 0,
      RDW_NOERASE or RDW_NOINTERNALPAINT or RDW_VALIDATE);
  end;

  //copy the bitmap srcRect onto the panel's canvas dstRect
  if fBmp.PixelFormat = pf32bit then
  begin
    bf.BlendOp := AC_SRC_OVER;
    bf.BlendFlags := 0;
    bf.SourceConstantAlpha := 255;
    bf.AlphaFormat := AC_SRC_ALPHA;

    AlphaBlend(dstCanvas.Handle,
      dstRect.left, dstRect.top, RectWidth(dstRect), RectHeight(dstRect),
      fBmp.Canvas.Handle,
      srcRect.Left, srcRect.Top, RectWidth(srcRect), RectHeight(srcRect), bf)
  end else
  begin
    //the following function often seems to return poor color
    //rendering when scaling is also required ...
    //dstCanvas.CopyRect(dstRect, fBmp.canvas, srcRect);

    //everything looks fine if we separate out any scaling
    tmpBmp := TBitmap.Create;
    try
      tmpBmp.Width := RectWidth(srcRect);
      tmpBmp.Height := RectHeight(srcRect);
      tmpBmp.Canvas.CopyRect(Rect(0,0,RectWidth(srcRect),
        RectHeight(srcRect)), fBmp.Canvas, srcRect);
      dstCanvas.StretchDraw(dstRect, tmpBmp);
    finally
      tmpBmp.Free;
    end;
  end;

  if Assigned(fOnEndPaint) then
  begin
    fOnEndPaint(Self, dstCanvas, srcRect, dstRect);
    //and prevent recursive OnPoint calls
    RedrawWindow(Handle, nil, 0,
      RDW_NOERASE or RDW_NOINTERNALPAINT or RDW_VALIDATE);
  end;
end;
//------------------------------------------------------------------------------

procedure TBitmapPanel.CreateWnd;
begin
  inherited;

  if fFileDropEnabled then
    DragAcceptFiles(Handle, True);
end;
//------------------------------------------------------------------------------

procedure TBitmapPanel.DestroyWnd;
begin
  if fFileDropEnabled then
  begin
    DragAcceptFiles(Handle, False);
    fFileDropEnabled := false;
  end;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TBitmapPanel.SetFileDropEnabled(value: Boolean);
begin
  if (fFileDropEnabled = value) then Exit;
  if not (csDesigning in ComponentState) and HandleAllocated then
  begin
    if fFileDropEnabled then
      DragAcceptFiles(Handle, false) else
      DragAcceptFiles(Handle, true);
  end;
  fFileDropEnabled := value;
end;
//------------------------------------------------------------------------------

procedure TBitmapPanel.WMDropFiles(var Msg: TMessage);
var
  hDrop: THandle;
  filenameLen: Integer;
  filename: string;
begin
  Msg.Result := 0;
  hDrop:= Msg.wParam;
  filenameLen := DragQueryFile(hDrop, 0, nil, 0);
  SetLength(filename, filenameLen);
  DragQueryFile(hDrop, 0, Pointer(filename), filenameLen+1);
  DragFinish(hDrop);

  if assigned(fOnFileDrop) then fOnFileDrop(Self, filename)
  else if (Lowercase(ExtractFileExt(filename)) = '.bmp') then
  try
    fBmp.LoadFromFile(filename);
  except
  end;
end;
//------------------------------------------------------------------------------

function TBitmapPanel.CopyToClipboard: Boolean;
var
  fmt  : Word;
  data : THandle;
  pal  : HPALETTE;
begin
  result := not fBmp.Empty;
  if not result then Exit;
{$IFDEF FPC}
  fBmp.SaveToClipboardFormat(CF_BITMAP);
{$ELSE}
  Clipboard.Open;
  try
    fBmp.SaveToClipboardFormat(fmt, data, pal);
    Clipboard.SetAsHandle(fmt, data);
  finally
    Clipboard.Close;
  end;
{$ENDIF}
end;
//------------------------------------------------------------------------------

function TBitmapPanel.PasteFromClipboard: Boolean;
begin
  Clipboard.Open;
  try
    result := Clipboard.HasFormat(CF_BITMAP);
    if not result then Exit;
    ResetImage;
{$IFDEF FPC}
     fBmp.LoadFromClipBoardFormat(CF_BITMAP);
{$ELSE}
    fBmp.LoadFromClipBoardFormat(CF_BITMAP,
      ClipBoard.GetAsHandle(CF_BITMAP),0);
{$ENDIF}
  finally
    Clipboard.Close;
  end;
  Invalidate;
  if assigned(fOnPaste) then fOnPaste(self);
end;
//------------------------------------------------------------------------------

procedure TBitmapPanel.WMKeyDown(var Message: TWMKey);
var
  shiftState: TShiftState;
begin
  inherited;
  shiftState := KeyDataToShiftState(Message.KeyData);
  case Message.CharCode of
    Ord('C'):
      if (ssCtrl in shiftState) and fCopyPasteEnabled then
        CopyToClipboard;
    Ord('V'):
      if (ssCtrl in shiftState) and fCopyPasteEnabled then
        PasteFromClipboard;
  end;
end;
//------------------------------------------------------------------------------

initialization
  MinBorderWidth := DpiAware(10);

end.
