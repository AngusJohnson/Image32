unit BitmapPanels;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.03                                                            *
* Date      :  13 July 2019                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Module that allows a TPanel to display an image                 *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Windows, Messages, Types, Graphics,
  Controls, Forms, ExtCtrls, Math, ShellApi, ClipBrd;

{$IF COMPILERVERSION >= 17}
  //Nested types within classes
  {$DEFINE NESTED_TYPES}
  {$IF COMPILERVERSION >= 18}
    //Delphi 2006   - added TBitmap.SetSize method
    {$DEFINE SETSIZE}
    {$IF COMPILERVERSION >= 18.5}
      //Delphi 2007   - added inline compiler directive
      {$DEFINE INLINE}
      {$IF COMPILERVERSION >= 20}
        //Delphi 2009  - added TBitmap.AlphaFormat property
        {$DEFINE ALPHAFORMAT}
        {$IF COMPILERVERSION >= 21}
          //Delphi 2010 - added screen gesture support
          {$DEFINE GESTURES}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$IFEND}

type
  TBitmapProperties = class;

  //TFileDropEvent: Method template for TBitmapProperties.OnFileDrop
  TFileDropEvent = procedure (Sender: TObject; const filename: string) of Object;

  {$IFNDEF NESTED_TYPES}
  //TScaleType: Internal use only
  TScaleType = (stScaled, stFit, stStretched);
  {$ENDIF}

  TPanel = class(ExtCtrls.TPanel)
    {$IFDEF NESTED_TYPES}
    type TScaleType = (stScaled, stFit, stStretched);
    {$ENDIF}
  private
    fBmp: TBitmap;
    fScale: double;
    fScaleType: TScaleType;
    fDstRect: TRect;
    fOffsetX: integer;
    fOffsetY: integer;
{$IFDEF GESTURES}
    fLastDistance: integer;
    fLastLocation: TPoint;
{$ENDIF}
    fMouseDown: Boolean;
    fMouseOverBevelHorz: Boolean;
    fMouseOverBevelVert: Boolean;
    fMouseDownOverBevelH: Boolean;
    fMouseDownOverBevelV: Boolean;
    fMousePos: TPoint;
    fFocusedColor: TColor;
    fBitmapProperties: TBitmapProperties;
    procedure UpdateCursor;
    procedure BitmapScaleBestFit;
{$IFDEF GESTURES}
    procedure Gesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
{$ENDIF}
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;
    procedure WMEraseBkgnd(var message: TMessage); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure BmpChanged(Sender: TObject);
    procedure BitmapScaleAtPos(newScale: double; const mousePos: TPoint);
    function GetInnerMargin: integer;
    function GetInnerClientRect: TRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //ClientToBitmap: Convert panel client coordinates to bitmap coordinates
    function ClientToBitmap(var clientPt: TPoint): Boolean;
    //BitmapToClient: Convert bitmap coordinates to panel client coordinates
    procedure BitmapToClient(var pt: TPoint);
    function CopyToClipboard: Boolean;
    function PasteFromClipboard: Boolean;
    //ClearBitmap: Required only when drawing to the bitmap's Canvas property.
    procedure ClearBitmap(PixelFormat: TPixelFormat = pf32bit);
    property Bitmap: TBitmap read fBmp;
    property BitmapProperties: TBitmapProperties read fBitmapProperties;
    //FocusedColor: Panel's border color when focused (ie if TabStop = true)
    property FocusedColor: TColor read fFocusedColor write fFocusedColor;
    //InnerMargin = BorderWidth + BevelWidth *2 (if bevels assigned)
    property InnerMargin: integer read GetInnerMargin;
    property InnerClientRect: TRect read GetInnerClientRect;
  end;

  TBitmapProperties = class
  private
    fOwner: TPanel;
    fMinScale: double;
    fMaxScale: double;
    fZoomScrollEnabled: Boolean;
    fShowScrollbars: Boolean;
    fScalingCursor: integer;
    fScrollButtonColor: TColor;
    fScrollButtonColorHot: TColor;
    fOnBitmapResizing: TNotifyEvent;
    fOnScrolling: TNotifyEvent;
    fOnDragDrop: TFileDropEvent;
    fOnPaste: TNotifyEvent;
    fFileDropEnabled: Boolean;
    fCopyPasteEnabled: Boolean;
    function GetOffset: TPoint;
    procedure SetOffset(const Pt: TPoint);
    function GetScale: double;
    function GetRawScale: double;
    procedure SetScale(value: double);
    procedure SetFileDropEnabled(value: Boolean);
    procedure SetZoomScrollEnabled(value: Boolean);
    procedure SetScrollbarsVisible(value: Boolean);
    procedure SetScalingCursor(value: integer);
  public
    constructor Create(ownerPanel: TPanel);
    procedure ResetBitmap;
    //CopyPasteEnabled: Default = false.
    property CopyPasteEnabled: Boolean
      read fCopyPasteEnabled write fCopyPasteEnabled;
    //FileDropEnabled: Default = false.
    property FileDropEnabled: Boolean
      read fFileDropEnabled write SetFileDropEnabled;
    //Offset: The amount to offset the image.<br>
    //(Assumes the display image size exceeds the panel's display area.)
    property Offset: TPoint read GetOffset write SetOffset;
    //Scale: SCALE_BEST_FIT, SCALE_STRETCHED, or a value between ScaleMin and
    //ScaleMax.
    property Scale: double read GetScale write SetScale;
    //RawScale: Useful only when Scale = SCALE_BEST_FIT
    property RawScale: double read GetRawScale;
    //ScaleMin: Default = 0.05;
    property ScaleMin: double read fMinScale write fMinScale;
    //ScaleMax: Default = 10.0;
    property ScaleMax: double read fMaxScale write fMaxScale;
    //ScalingCursor: Default = crHandPoint
    property ScalingCursor: integer read fScalingCursor write SetScalingCursor;
    //ZoomAndScrollEnabled: Default = True
    property ZoomAndScrollEnabled: Boolean
      read fZoomScrollEnabled write SetZoomScrollEnabled;
    //ScrollButtonsVisible: Default = True (but scroll buttons will only
    //be visible when the image size exceeds the panel's display area.
    property ScrollButtonsVisible: boolean read
      fShowScrollbars write SetScrollbarsVisible;
    //ScrollButtonColor: Default = 20% darker than clBtnFace
    property ScrollButtonColor: TColor read
      fScrollButtonColor write fScrollButtonColor;
    //ScrollButtonColorHot: Default = clHotLight
    property ScrollButtonColorHot: TColor
      read fScrollButtonColorHot write fScrollButtonColorHot;
    property OnResizing: TNotifyEvent
      read fOnBitmapResizing write fOnBitmapResizing;
    property OnScrolling: TNotifyEvent
      read fOnScrolling write fOnScrolling;
    property OnFileDrop: TFileDropEvent read fOnDragDrop write fOnDragDrop;
    property OnPaste: TNotifyEvent read fOnPaste write fOnPaste;
  end;

const
  SCALE_STRETCHED = -1;
  SCALE_BEST_FIT = 0;

  //The minimum size for scrolling buttons. If borders are too narrow
  //to properly display scroll buttons then scroll buttons will be disabled.
  FMinScrollBtnSize = 5;

implementation

resourcestring
  rsClearBitmapError = 'Error in BitmapPanels.TPanel.ClearBitmap - bitmap is empty.';

const
  //Minimum zoom scale
  MinScaleSize = 0.05;
  //Maximum zoom scale
  MaxScaleSize = 10.0;

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

//Since record methods were only added to Delphi in version D2006,
//the following functions provide compatability for earlier versions

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

//DPI: the 'standard' screen resoluton is 96dpi;
//newer monitors however typically have higher resoulutions (eg 120, 144dpi).
//Without DPI scaling, application forms and controls would get progressively
//smaller as DPI resoulutons increase.

function DpiScale(value: integer): integer;
begin
  result := Round(value * Screen.PixelsPerInch div 96);
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
// TBitmapProperties class
//------------------------------------------------------------------------------

constructor TBitmapProperties.Create(ownerPanel: TPanel);
begin
  fOwner := ownerPanel;
  fOwner.fScale := 1;
  fOwner.fScaleType := stFit;
  fMinScale := MinScaleSize;
  fMaxScale := MaxScaleSize;
  fShowScrollbars := true;
  fScrollButtonColor := MakeDarker(clBtnFace, 20); //ie 20% darker
  fScrollButtonColorHot := clHotLight;
  fZoomScrollEnabled := true;
end;
//------------------------------------------------------------------------------

procedure TBitmapProperties.ResetBitmap;
begin
  fOwner.fScale := 1;
  fOwner.fOffsetX := 0;
  fOwner.fOffsetY := 0;
  fOwner.Invalidate;
end;
//------------------------------------------------------------------------------

function TBitmapProperties.GetOffset: TPoint;
begin
  result := Point(fOwner.fOffsetX, fOwner.fOffsetY);
end;
//------------------------------------------------------------------------------

procedure TBitmapProperties.SetOffset(const Pt: TPoint);
begin
  fOwner.fOffsetX := pt.X;
  fOwner.fOffsetY := pt.Y;
  fOwner.Invalidate;
end;
//------------------------------------------------------------------------------

function TBitmapProperties.GetScale: double;
begin
  case fOwner.fScaleType of
    stScaled: Result := fOwner.fScale;
    stFit   : Result := SCALE_BEST_FIT;
    else Result := SCALE_STRETCHED;
  end;
end;
//------------------------------------------------------------------------------

function TBitmapProperties.GetRawScale: double;
begin
  if fOwner.fScaleType = stStretched then
    Result := 1 else
    Result := fOwner.fScale;
end;
//------------------------------------------------------------------------------

procedure TBitmapProperties.SetScale(value: double);
var
  rec: TRect;
begin
  if value = SCALE_BEST_FIT then
  begin
    if fOwner.fScaleType = stFit then Exit;
    fOwner.fScaleType := stFit;
    fOwner.Invalidate;
  end else if value = SCALE_STRETCHED then
  begin
    if fOwner.fScaleType = stStretched then Exit;
    fOwner.fScaleType := stStretched;
    fOwner.Invalidate;
  end else
  begin
    if value < fMinScale then value := fMinScale
    else if value > fMaxScale then value := fMaxScale;
    fOwner.fScaleType := stScaled;
    //zoom in or out relative to the center of the image
    rec := fOwner.ClientRect;
    fOwner.BitmapScaleAtPos(value,
      Point(RectWidth(rec) div 2, RectHeight(rec) div 2));
  end;
end;
//------------------------------------------------------------------------------

procedure TBitmapProperties.SetScalingCursor(value: integer);
begin
  fScalingCursor := value;
  fOwner.UpdateCursor;
end;
//------------------------------------------------------------------------------


procedure TBitmapProperties.SetFileDropEnabled(value: Boolean);
begin
  if fFileDropEnabled = value then Exit;
  if fOwner.HandleAllocated then
  begin
    if fFileDropEnabled then
      DragAcceptFiles(fOwner.Handle, false) else
      DragAcceptFiles(fOwner.Handle, true);
  end;
  fFileDropEnabled := value;
end;
//------------------------------------------------------------------------------

procedure TBitmapProperties.SetZoomScrollEnabled(value: Boolean);
begin
  fZoomScrollEnabled := value;
  fOwner.UpdateCursor;
end;
//------------------------------------------------------------------------------

procedure TBitmapProperties.SetScrollbarsVisible(value: Boolean);
begin
  fShowScrollbars := value;
  fOwner.Invalidate;
end;

//------------------------------------------------------------------------------
// Imaged enhanced TPanel class
//------------------------------------------------------------------------------

constructor TPanel.Create(AOwner: TComponent);
begin
  inherited;
  fBitmapProperties := TBitmapProperties.Create(self);
  fScale := 1;
  fOffsetX := 0;
  fOffsetY := 0;
  fBmp := TBitmap.Create;
  fBmp.PixelFormat := pf32bit;
  {$IFDEF ALPHAFORMAT}
  fBmp.AlphaFormat := afPremultiplied;
  {$ENDIF}
  {$IFDEF GESTURES}
  OnGesture := Gesture;
  Touch.InteractiveGestures := [igZoom, igPan];
  {$ENDIF}
  fBmp.Canvas.Brush.Color := Color;
  fBmp.OnChange := BmpChanged;
  fFocusedColor := Color;
  DoubleBuffered := true;
end;
//------------------------------------------------------------------------------

destructor TPanel.Destroy;
begin
  fBmp.Free;
  fBitmapProperties.Free;
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPanel.BmpChanged(Sender: TObject);
begin
  if fScale <= 0  then BitmapScaleBestFit;
  //nb: also called when the bitmap canvas has been updated
  //fOffsetX := 0; fOffsetY := 0;
  UpdateCursor;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TPanel.ClientToBitmap(var clientPt: TPoint): Boolean;
begin
  Result := not fBmp.Empty and PtInRect(InnerClientRect, clientPt);
  if not Result then Exit;
  clientPt.X := Round((clientPt.X -fDstRect.Left + fOffsetX)/fScale);
  clientPt.Y := Round((clientPt.Y -fDstRect.Top + fOffsetY)/fScale);
  Result := (clientPt.X >= 0) and (clientPt.X < fBmp.Width) and
    (clientPt.Y >= 0) and (clientPt.Y < fBmp.Width);
end;
//------------------------------------------------------------------------------

procedure TPanel.BitmapToClient(var pt: TPoint);
begin
  if fBmp.Empty then Exit;
  pt.X := Round(pt.X * fScale +fDstRect.Left - fOffsetX);
  pt.Y := Round(pt.Y * fScale +fDstRect.Top - fOffsetY);
end;
//------------------------------------------------------------------------------

procedure TPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTCHARS or DLGC_WANTARROWS;
end;
//------------------------------------------------------------------------------

procedure TPanel.WMKeyDown(var Message: TWMKey);
var
  mul: integer;
  midPoint: TPoint;
begin
  inherited;
  if (Message.CharCode >= Ord('V')) and
    (ssCtrl in KeyDataToShiftState(Message.KeyData)) and
    fBitmapProperties.fCopyPasteEnabled then
  begin
    PasteFromClipboard;
    Exit;
  end;

  if fBmp.Empty then Exit;
  if (Message.CharCode >= Ord('C')) and
    (ssCtrl in KeyDataToShiftState(Message.KeyData)) and
    fBitmapProperties.fCopyPasteEnabled then
  begin
    CopyToClipboard;
    Exit;
  end;

  if not fBitmapProperties.fZoomScrollEnabled then Exit;
  if (Message.CharCode >= VK_LEFT) and (Message.CharCode <= VK_DOWN) then
  begin
    //zoom in and out with CTRL+UP and CTRL+DOWN respectively
    if ssCtrl in KeyDataToShiftState(Message.KeyData) then
    begin
      midPoint := Point(ClientWidth div 2, ClientHeight div 2);
      case Message.CharCode of
        VK_UP: BitmapScaleAtPos(fScale * 1.1, midPoint);
        VK_DOWN: BitmapScaleAtPos(fScale * 0.9, midPoint);
        else Exit;
      end;
      if assigned(fBitmapProperties.fOnBitmapResizing) then
        fBitmapProperties.fOnBitmapResizing(self);
    end
    else //otherwise scroll the image with the arrow keys
    begin
      if ssShift in KeyDataToShiftState(Message.KeyData) then
        mul := 5 else //ie scrolls 5 times faster with Shift key down
        mul := 1;
      case Message.CharCode of
        VK_LEFT: dec(fOffsetX, 5 * mul);
        VK_RIGHT: inc(fOffsetX, 5 * mul);
        VK_UP: dec(fOffsetY, 5 * mul);
        VK_DOWN: inc(fOffsetY, 5 * mul);
      end;
      if assigned(fBitmapProperties.fOnScrolling) then
        fBitmapProperties.fOnScrolling(self);
    end;
    Invalidate;
  end
  else if Message.CharCode = Ord('0') then
  begin
    fScaleType := stFit;
    Invalidate;
    if assigned(fBitmapProperties.fOnBitmapResizing) then
      fBitmapProperties.fOnBitmapResizing(self);
  end
  else if (Message.CharCode >= Ord('1')) and (Message.CharCode <= Ord('9')) then
  begin
    if ssShift in KeyDataToShiftState(Message.KeyData) then
      fBitmapProperties.Scale := (Message.CharCode - Ord('0')) /10 else
      fBitmapProperties.Scale := Message.CharCode - Ord('0');
  end;
end;
//------------------------------------------------------------------------------

procedure TPanel.WMEraseBkgnd(var message: TMessage);
begin
  if fBmp.Empty then
    inherited else
    message.Result := 0; //ie don't bother erasing background
end;
//------------------------------------------------------------------------------

procedure TPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  rec: TRect;
begin
  inherited;
  if fBmp.Empty or not fBitmapProperties.fZoomScrollEnabled or
    (fScaleType = stStretched) then Exit;

  fMouseDown := true;
  rec := GetInnerClientRect;
  if (X > rec.Right) then
    fMouseDownOverBevelV := true
  else if (Y > rec.Bottom) then
    fMouseDownOverBevelH := true;
  fMousePos := Point(X, Y);
  if TabStop and not Focused and CanFocus then
  begin
    SetFocus;
    Invalidate;
  end;
end;
//------------------------------------------------------------------------------

procedure TPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  btnLeft, btnTop, innerMarg: integer;
  ratio: double;
  rec: TRect;
  mobV, mobH: Boolean;
begin
  inherited;

  if not fMouseDown then
  begin
    if fBitmapProperties.fShowScrollbars then
    begin
     innerMarg := GetInnerMargin;
      mobH := (X <= innerMarg) or (X > ClientWidth - innerMarg);
      mobV := (Y <= innerMarg) or (Y > ClientHeight - innerMarg);
      //now check for change in state ...
      if fMouseOverBevelHorz <> mobH then
      begin
        fMouseOverBevelHorz := mobH;
        Invalidate;
      end;
      if fMouseOverBevelVert <> mobV then
      begin
        fMouseOverBevelVert := mobV;
        Invalidate;
      end;
    end;
    Exit;
  end;

  fMouseOverBevelHorz := false;
  fMouseOverBevelVert := false;
  if fMouseDownOverBevelV then
  begin
    //click and drag vertical scrollbar
    rec := GetInnerClientRect;
    ratio := RectHeight(rec) / (fBmp.Height * fScale);
    btnTop := Round(fOffsetY * ratio) + rec.Top;     //previous btnTop
    btnTop := btnTop - (fMousePos.Y - Y);            //new btnTop
    fOffsetY := Round((btnTop - rec.Top) / ratio);
  end
  else if fMouseDownOverBevelH then
  begin
    //click and drag horizontal scrollbar
    rec := GetInnerClientRect;
    ratio := RectWidth(rec) / (fBmp.Width * fScale);
    btnLeft := Round(fOffsetX * ratio) + rec.Left;   //previous btnLeft
    btnLeft := btnLeft - (fMousePos.X - X);          //new btnLeft
    fOffsetX := Round((btnLeft - rec.Left) / ratio);
  end else
  begin
    fOffsetX := fOffsetX + (fMousePos.X - X);
    fOffsetY := fOffsetY + (fMousePos.Y - Y);
  end;
  if assigned(fBitmapProperties.fOnScrolling) then
    fBitmapProperties.fOnScrolling(self);
  Invalidate;
  fMousePos := Point(X, Y);
end;
//------------------------------------------------------------------------------

procedure TPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not fMouseDown then Exit;
  fMouseDown := false;
  fMouseDownOverBevelH := false;
  fMouseDownOverBevelV := false;
  fMouseOverBevelHorz := false;
  fMouseOverBevelVert := false;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if fMouseOverBevelHorz then
    fMouseOverBevelHorz := false
  else if fMouseOverBevelVert then
    fMouseOverBevelVert := false
  else Exit;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TPanel.CopyToClipboard: Boolean;
var
  fmt  : Word;
  data : THandle;
  pal  : HPALETTE;
begin
  result := not fBmp.Empty;
  if not result then Exit;
  Clipboard.Open;
  try
    fBmp.SaveToClipboardFormat(fmt, data, pal);
    Clipboard.SetAsHandle(fmt, data);
  finally
    Clipboard.Close;
  end;
end;
//------------------------------------------------------------------------------

function TPanel.PasteFromClipboard: Boolean;
begin
  Clipboard.Open;
  try
    result := Clipboard.HasFormat(CF_BITMAP);
    if not result then Exit;
    fBmp.LoadFromClipBoardFormat(CF_BITMAP,
      ClipBoard.GetAsHandle(CF_BITMAP),0);
    fOffsetX := 0; fOffsetY := 0;
    if fScale = SCALE_BEST_FIT then
      BitmapScaleBestFit else
      fScale := 1;
    Invalidate;
    if assigned(fBitmapProperties.fOnPaste) then
      fBitmapProperties.fOnPaste(self);
  finally
    Clipboard.Close;
  end;
end;
//------------------------------------------------------------------------------

procedure TPanel.WMDropFiles(var Msg: TMessage);
var
  hDrop: THandle;
  filenameLen: Integer;
  filename: string;
begin
  Msg.Result := 0;
  if not assigned(fBitmapProperties.fOnDragDrop) then Exit;
  hDrop:= Msg.wParam;
  filenameLen := DragQueryFile(hDrop, 0, nil, 0);
  SetLength(filename, filenameLen);
  DragQueryFile(hDrop, 0, Pointer(filename), filenameLen+1);
  DragFinish(hDrop);
  fBitmapProperties.fOnDragDrop(Self, filename);
end;
//------------------------------------------------------------------------------

procedure TPanel.CreateWnd;
begin
  inherited;
  if fBitmapProperties.fFileDropEnabled then
    DragAcceptFiles(Handle, True);
end;
//------------------------------------------------------------------------------

procedure TPanel.DestroyWnd;
begin
  if fBitmapProperties.fFileDropEnabled then DragAcceptFiles(Handle, False);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPanel.Resize;
begin
  inherited;
  if (fScaleType = stFit) and
    assigned(fBitmapProperties.fOnBitmapResizing) then
      fBitmapProperties.fOnBitmapResizing(self);
end;
//------------------------------------------------------------------------------

procedure TPanel.ClearBitmap(PixelFormat: TPixelFormat);
var
  i: integer;
  pc: PColor32;
begin
  if fBmp.Empty then
    Raise Exception.Create(rsClearBitmapError);
  fBmp.PixelFormat := PixelFormat;
  if PixelFormat = pf32bit then
  begin
    {$IFDEF ALPHAFORMAT}
    fBmp.AlphaFormat := afPremultiplied;
    {$ENDIF}
    pc := PColor32(fBmp.ScanLine[fBmp.Height -1]);
    for i := 0 to fBmp.Width * fBmp.Height -1 do
    begin
      pc^ := 0;
      inc(pc);
    end;
  end else
  begin
    {$IFDEF ALPHAFORMAT}
    fBmp.AlphaFormat := afIgnored;
    {$ENDIF}
    fBmp.Canvas.Brush.Color := Self.Color;
    fBmp.Canvas.FillRect(Rect(0, 0, fBmp.Width, fBmp.Height));
  end;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TPanel.Paint;
var
  marginOff, w,h: integer;
  tmpRec, srcRec, srcScaled: TRect;
  tmpBmp: TBitmap;
  bf: BLENDFUNCTION;

  procedure DrawScrollButton(const rec: TRect; raised: Boolean);
  begin
    if raised then
      Canvas.Pen.Color := clBtnHighlight else
      Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(rec.Left, rec.Bottom);
    Canvas.LineTo(rec.Left, rec.Top);
    Canvas.LineTo(rec.Right, rec.Top);
    if raised then
      Canvas.Pen.Color := clBtnShadow else
      Canvas.Pen.Color := clBtnHighlight;
    Canvas.LineTo(rec.Right, rec.Bottom);
    Canvas.LineTo(rec.Left, rec.Bottom);
  end;

begin

  fDstRect := ClientRect;
  if TabStop and Focused then
    Canvas.Brush.Color := fFocusedColor else
    Canvas.Brush.Color := color;
  Canvas.FillRect(fDstRect); //needed because of WMEraseBkgnd result

  marginOff := GetInnerMargin;
  InflateRect(fDstRect, -marginOff, -marginOff);
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(fDstRect);

  //calling inherited draws the bevels and panel caption.
  inherited;

  if fBmp.Empty then
  begin
    Exit;
  end;

  srcRec := Rect(0, 0, fBmp.Width, fBmp.Height);

  if (fScaleType = stFit) then
    BitmapScaleBestFit;

  if (fScaleType <> stStretched) then
  begin
    //re-calculate both srcRec and dstRec

    //srcScaled - the virtual size of the scaled bitmap
    srcScaled := srcRec;
    SetRectWidth(srcScaled, Round(RectWidth(srcScaled) * fScale));
    SetRectHeight(srcScaled, Round(RectHeight(srcScaled) * fScale));
    //make sure the offsets are within range
    fOffsetX :=
      Max(0, Min(RectWidth(srcScaled) - RectWidth(fDstRect) -1, fOffsetX));
    fOffsetY :=
      Max(0, Min(RectHeight(srcScaled) - RectHeight(fDstRect) -1, fOffsetY));
    //offset srcScaled so it aligns with the top-left corner of dstRec
    //and clip srcScaled to dstRec
    OffsetRect(srcScaled, marginOff - fOffsetX, marginOff - fOffsetY);
    IntersectRect(srcScaled, srcScaled, fDstRect);
    //now unzoom the clipped virtual image to get the adjusted srcRec
    SetRectWidth(srcRec, Round(RectWidth(srcScaled) / fScale));
    SetRectHeight(srcRec, Round(RectHeight(srcScaled) / fScale));

    //micro-readjust to accommodate fractional scaling
    if RectWidth(srcRec) > fBmp.Width then
    begin
      SetRectWidth(srcRec, fBmp.Width);
      fOffsetX := 0;
    end;
    if RectHeight(srcRec) > fBmp.Height then
    begin
      SetRectHeight(srcRec, fBmp.Height);
      fOffsetY := 0;
    end;

    OffsetRect(srcRec, Round(fOffsetX/fscale), Round(fOffsetY/fscale));
    //if necessary, center the dstRec
    if (RectWidth(srcScaled) < RectWidth(fDstRect)) then
    begin
      fDstRect.Left :=
        (RectWidth(fDstRect) - RectWidth(srcScaled)) div 2 + marginOff;
      SetRectWidth(fDstRect, RectWidth(srcScaled));
    end;
    if RectHeight(srcScaled) < RectHeight(fDstRect) then
    begin
      fDstRect.Top :=
        (RectHeight(fDstRect) - RectHeight(srcScaled)) div 2 + marginOff;
      SetRectHeight(fDstRect, RectHeight(srcScaled));
    end;
  end;

  //now we finally have both the src and dst rects we can do the image copy
  if fBmp.PixelFormat = pf32bit then
  begin
    bf.BlendOp := AC_SRC_OVER;
    bf.BlendFlags := 0;
    bf.SourceConstantAlpha := 255;
    bf.AlphaFormat := AC_SRC_ALPHA;

    AlphaBlend(Canvas.Handle, fDstRect.left, fDstRect.top,
      RectWidth(fDstRect), RectHeight(fDstRect), fBmp.Canvas.Handle,
      srcRec.Left, srcRec.Top, RectWidth(srcRec), RectHeight(srcRec), bf)
  end else
  begin
    tmpBmp := TBitmap.Create; //temporary bitmap for clipping
    try
      {$IFDEF SETSIZE}
      tmpBmp.SetSize(srcRec.Width, srcRec.Height);
      {$ELSE}
      tmpBmp.Width := RectWidth(srcRec);
      tmpBmp.Height := RectHeight(srcRec);
      {$ENDIF}
      tmpBmp.Canvas.CopyRect(Rect(0,0,RectWidth(srcRec),
        RectHeight(srcRec)), fBmp.Canvas, srcRec);
      Canvas.StretchDraw(fDstRect, tmpBmp);
    finally
      tmpBmp.Free;
    end;
  end;

  if not fBitmapProperties.fShowScrollbars or
    (BorderWidth < DpiScale(FMinScrollBtnSize + 5)) then Exit;

  //draw vertical scrollbar
  tmpRec := GetInnerClientRect;
  if (fBmp.Height * fScale > RectHeight(tmpRec) +1) then
  begin
    h := Round(fBmp.Height * fScale);
    w := ClientWidth;
    tmpRec.Left := w - marginOff + DpiScale(2);
    tmpRec.Right := w - DpiScale(3);
    OffsetRect(tmpRec, 0, Round(fOffsetY * RectHeight(tmpRec) / h));
    SetRectHeight(tmpRec, RectHeight(tmpRec) * RectHeight(tmpRec) div h);
    if fMouseDownOverBevelV or fMouseOverBevelHorz then
      Canvas.Brush.Color := fBitmapProperties.fScrollButtonColorHot else
      Canvas.Brush.Color := fBitmapProperties.fScrollButtonColor;
    Canvas.FillRect(tmpRec);
    DrawScrollButton(tmpRec, true);
  end;

  //draw horizontal scrollbar
  tmpRec := GetInnerClientRect;
  if (fBmp.Width * fScale > RectWidth(tmpRec) +1) then
  begin
    w := Round(fBmp.Width * fScale);
    h := ClientHeight;
    tmpRec.Top := h - marginOff + DpiScale(2);
    tmpRec.Bottom := h - DpiScale(3);
    OffsetRect(tmpRec, Round(fOffsetX * RectWidth(tmpRec) / w), 0);
    SetRectWidth(tmpRec, RectWidth(tmpRec) * RectWidth(tmpRec) div w);
    if fMouseDownOverBevelH or fMouseOverBevelVert then
      Canvas.Brush.Color := fBitmapProperties.fScrollButtonColorHot else
      Canvas.Brush.Color := fBitmapProperties.fScrollButtonColor;
    Canvas.FillRect(tmpRec);
    DrawScrollButton(tmpRec, true);
  end;
end;
//------------------------------------------------------------------------------

procedure TPanel.CMFocusChanged(var Message: TCMFocusChanged);
begin
  Invalidate;
end;
//------------------------------------------------------------------------------

function TPanel.GetInnerMargin: integer;
begin
  //nb: If BorderStyle = bsSingle, a line will be drawn around the outside of
  //the Panel reducing ClientRect. Hence, further adjustments aren't required.
  Result := 0;
  if BevelInner <> bvNone then inc(result, BevelWidth);
  if BevelOuter <> bvNone then inc(result, BevelWidth);
  //BorderWidth: the space between outer and inner bevels
  inc(result, BorderWidth);
end;
//------------------------------------------------------------------------------

function TPanel.GetInnerClientRect: TRect;
var
  marg: integer;
begin
  marg := GetInnerMargin;
  result := ClientRect;
  InflateRect(result, -marg, -marg);
end;
//------------------------------------------------------------------------------

procedure TPanel.BitmapScaleBestFit;
var
  scaleX, scaleY, marg: double;
begin
  if fBmp.Empty then Exit;
  fOffsetX := 0; fOffsety := 0;
  marg := GetInnerMargin;
  scaleX := (ClientWidth - marg *2) / fBmp.Width;
  scaleY := (ClientHeight- marg *2) / fBmp.Height;
  if scaleY < scaleX then fScale := scaleY
  else fScale := scaleX;
end;
//------------------------------------------------------------------------------

procedure TPanel.UpdateCursor;
begin
  if fBmp.Empty or not fBitmapProperties.fZoomScrollEnabled or
    (fScaleType = stStretched) then
      cursor := crDefault
  else if fBitmapProperties.fScalingCursor <> 0 then
    cursor := fBitmapProperties.fScalingCursor
  else
    cursor := crHandPoint;
end;
//------------------------------------------------------------------------------

procedure TPanel.BitmapScaleAtPos(newScale: double; const mousePos: TPoint);
begin
  newScale := Max(fBitmapProperties.fMinScale,
    Min(fBitmapProperties.fMaxScale, newScale));
  if fBmp.Empty or (fScaleType = stStretched) then Exit;
  fScaleType := stScaled;
  fOffsetX := Round((mousePos.X + fOffsetX) * newScale/fScale - mousePos.X);
  fOffsetY := Round((mousePos.Y + fOffsetY) * newScale/fScale - mousePos.Y);
  fScale := newScale;
  Invalidate;
  if assigned(fBitmapProperties.fOnBitmapResizing) then
    fBitmapProperties.fOnBitmapResizing(self);
end;
//------------------------------------------------------------------------------

function OffsetPoint(const pt: TPoint; dx, dy: integer): TPoint;
begin
  Result.X := pt.X + dx;
  Result.Y := pt.Y + dy;
end;
//------------------------------------------------------------------------------

{$IFDEF GESTURES}
procedure TPanel.Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  changeFrac: double;
  marg: integer;
begin
  if fBmp.Empty or not fBitmapProperties.fZoomScrollEnabled or
    (fScaleType = stStretched) then
  begin
    inherited;
    Exit;
  end;

  case EventInfo.GestureID of
    igiZoom:
      begin
        if not (gfBegin in EventInfo.Flags) then
        begin
          changeFrac := 1 + (EventInfo.Distance - FLastDIstance)/fBmp.Width;
          marg := GetInnerMargin;
          BitmapScaleAtPos(fScale * changeFrac,
            OffsetPoint(EventInfo.Location, -marg, -marg));
          if assigned(fBitmapProperties.fOnBitmapResizing) then
            fBitmapProperties.fOnBitmapResizing(self);
        end;
        FLastDIstance := EventInfo.Distance;
        Handled := true;
      end;
    igiPan:
      begin
        if not (gfBegin in EventInfo.Flags) then
        begin
          fOffsetX := fOffsetX + (FLastLocation.X - EventInfo.Location.X);
          fOffsetY := fOffsetY + (FLastLocation.Y - EventInfo.Location.Y);
          if assigned(fBitmapProperties.fOnScrolling) then
            fBitmapProperties.fOnScrolling(self);
          Invalidate;
        end;
        FLastLocation := EventInfo.Location;
        Handled := true;
      end;
  end;
end;
//------------------------------------------------------------------------------
{$ENDIF}

function TPanel.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  marg: integer;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if fBmp.Empty or not fBitmapProperties.fZoomScrollEnabled or
    (fScaleType = stStretched) then Exit;
  MousePos := ScreenToClient(MousePos);
  marg := GetInnerMargin;
  MousePos := OffsetPoint(MousePos, -marg, -marg);
  BitmapScaleAtPos(fScale * 0.9, MousePos);
end;
//------------------------------------------------------------------------------

function TPanel.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  marg: integer;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if fBmp.Empty or not fBitmapProperties.fZoomScrollEnabled or
    (fScaleType = stStretched) then Exit;
  MousePos := ScreenToClient(MousePos);
  marg := GetInnerMargin;
  MousePos := OffsetPoint(MousePos, -marg, -marg);
  BitmapScaleAtPos(fScale * 1.1, MousePos);
end;
//------------------------------------------------------------------------------

end.
