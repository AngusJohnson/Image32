unit BitmapPanels;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  3.1                                                             *
* Date      :  19 February 2020                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2019-2020                                         *
* Purpose   :  Module that allows a TPanel to display an image                 *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*******************************************************************************)

interface

uses
  SysUtils, Classes, Windows, Messages, Types, Graphics,
  Controls, Forms, ExtCtrls, Themes, uxTheme, Math, ShellApi, ClipBrd;

{$I Image32.inc}

type
  TFileDropEvent = procedure (Sender: TObject; const filename: string) of Object;
  TScaleType = (stScaled, stFit, stStretched);

  TBitmap = class; //forward declaration

  TPanel = class(ExtCtrls.TPanel)
  private
    fBmp           : TBitmap;
    fScale         : double;
    fMinScale      : double;
    fMaxScale      : double;
    fScaleType     : TScaleType;
    fDstRect       : TRect;
    fOffsetX       : integer;
    fOffsetY       : integer;
    fOnScrolling   : TNotifyEvent;
    fOnDragDrop    : TFileDropEvent;
    fOnPaste       : TNotifyEvent;
    fOnBeginPaint  : TNotifyEvent;
    fOnEndPaint    : TNotifyEvent;
    fOnKeyDown     : TKeyEvent;
    fOnKeyUp       : TKeyEvent;

    fScalingCursor        : integer;
    fShowScrollbars       : Boolean;
    fAutoCenter           : Boolean;
    fScrollButtonColor    : TColor;
    fScrollButtonColorHot : TColor;
    fZoomScrollEnabled    : Boolean;
    fFileDropEnabled      : Boolean;
    fCopyPasteEnabled     : Boolean;

{$IFDEF GESTURES}
    fLastDistance: integer;
    fLastLocation: TPoint;
{$ENDIF}
    fMouseDown: Boolean;
    fMouseOverBevelVert: Boolean;
    fMouseOverBevelHorz: Boolean;
    fMouseDownOverBevelH: Boolean;
    fMouseDownOverBevelV: Boolean;
    fMousePos: TPoint;
    fFocusedColor: TColor;
    procedure UpdateCursor;
    procedure BitmapScaleBestFit;
    function MouseInsideInnerClientRect: Boolean;
{$IFDEF GESTURES}
    procedure Gesture(Sender: TObject;
      const EventInfo: TGestureEventInfo; var Handled: Boolean);
{$ENDIF}
    function ShowHorzScrollButton: Boolean;
    function ShowVertScrollButton: Boolean;
    procedure SetZoomScrollEnabled(value: Boolean);
    procedure SetScrollbarsVisible(value: Boolean);
    procedure SetAutoCenter(value: Boolean);
    procedure SetFileDropEnabled(value: Boolean);
    procedure SetScalingCursor(value: integer);

    function GetOffset: TPoint;
    procedure SetOffset(const Pt: TPoint);
    function GetScale: double;
    procedure SetScaleType(value: TScaleType);
    procedure SetScale(value: double);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKey); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKey); message WM_KEYUP;
    procedure WMEraseBkgnd(var message: TMessage); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure CMFocusChanged(var Message: TMessage); message CM_FOCUSCHANGED;
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
    procedure ClearBitmap;

    function CopyToClipboard: Boolean;
    function PasteFromClipboard: Boolean;

    property AutoCenter: Boolean                      //Default = true.
      read fAutoCenter write SetAutoCenter;
    property Bitmap: TBitmap read fBmp;
    property CopyPasteEnabled: Boolean                //Default = false.
      read fCopyPasteEnabled write fCopyPasteEnabled;
    property FileDropEnabled: Boolean                 //Default = false.
      read fFileDropEnabled write SetFileDropEnabled;

    property FocusedColor: TColor read fFocusedColor write fFocusedColor;
    property InnerMargin: integer read GetInnerMargin;
    property InnerClientRect: TRect read GetInnerClientRect;
    property Offset: TPoint read GetOffset write SetOffset;
    property ScalingCursor: integer read fScalingCursor write SetScalingCursor;

    property ScaleType: TScaleType read fScaleType write SetScaleType;
    property Scale: double read GetScale write SetScale;
    property ScaleMin: double read fMinScale write fMinScale;
    property ScaleMax: double read fMaxScale write fMaxScale;

    property ScrollButtonColor: TColor read
      fScrollButtonColor write fScrollButtonColor;
    property ScrollButtonColorHot: TColor
      read fScrollButtonColorHot write fScrollButtonColorHot;
    property ScrollButtonsVisible: boolean read
      fShowScrollbars write SetScrollbarsVisible;
    property ZoomAndScrollEnabled: Boolean
      read fZoomScrollEnabled write SetZoomScrollEnabled;

    property OnScrolling: TNotifyEvent
      read fOnScrolling write fOnScrolling;
    property OnFileDrop: TFileDropEvent read fOnDragDrop write fOnDragDrop;
    property OnPaste: TNotifyEvent read fOnPaste write fOnPaste;
    property OnBeginPaint: TNotifyEvent read fOnBeginPaint write fOnBeginPaint;
    property OnEndPaint: TNotifyEvent read fOnEndPaint write fOnEndPaint;
    property OnKeyDown: TKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnKeyUp: TKeyEvent read fOnKeyUp write fOnKeyUp;
  end;

  TBitmap = class(Graphics.TBitmap)
  private
    fOwner: TPanel;
  protected
    procedure Changed(Sender: TObject); override;
  public
    constructor Create(ownerPanel: TPanel); reintroduce; overload;
  end;

{$IFDEF FPC}
  function AlphaBlend(DC: HDC; p2, p3, p4, p5: Integer;
      DC6: HDC; p7, p8, p9, p10: Integer; p11: TBlendFunction): BOOL;
      stdcall; external 'msimg32.dll' name 'AlphaBlend';
{$ENDIF}

const
  //The minimum size for scrolling buttons. If borders are too narrow
  //to properly display scroll buttons then scroll buttons will be disabled.
  FMinScrollBtnSize = 5;

implementation

const
  MinBitmapScale = 0.05;
  MaxBitmapScale = 20;

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

//DPI: the 'standard' screen resoluton is 96dpi;
//newer monitors however typically have higher resoulutions (eg 120, 144dpi).
//Without DPI scaling, application forms and controls would get progressively
//smaller as DPI resoulutons increase.

function DpiScale(value: integer): integer;
begin
  result := Round(value * Screen.PixelsPerInch div 96);
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
// TBitmap class
//------------------------------------------------------------------------------

constructor TBitmap.Create(ownerPanel: TPanel);
begin
  inherited Create;
  fOwner := ownerPanel;
  PixelFormat := pf24bit;
  {$IFDEF ALPHAFORMAT}
  AlphaFormat := afPremultiplied;
  {$ENDIF}
end;
//------------------------------------------------------------------------------

procedure TBitmap.Changed(Sender: TObject);
begin
  inherited;
  if Assigned(fOwner) then fOwner.Invalidate;
end;

//------------------------------------------------------------------------------
// Enhanced TPanel class
//------------------------------------------------------------------------------

constructor TPanel.Create(AOwner: TComponent);
begin
  inherited;
  fScale := 1;
  fScaleType := stFit;
  fMinScale := MinBitmapScale;
  fMaxScale := MaxBitmapScale;
  fOffsetX := 0;
  fOffsetY := 0;

  fShowScrollbars := true;
  fAutoCenter := true;
  fScrollButtonColor := MakeDarker(clBtnFace, 20); //ie 20% darker
  fScrollButtonColorHot := clHotLight;
  fZoomScrollEnabled := true;

  {$IFDEF GESTURES}
  OnGesture := Gesture;
  Touch.InteractiveGestures := [igZoom, igPan];
  {$ENDIF}
  fBmp := TBitmap.Create(self);
  fBmp.Canvas.Brush.Color := Color;
  fFocusedColor := Color;
  DoubleBuffered := true;
end;
//------------------------------------------------------------------------------

destructor TPanel.Destroy;
begin
  fBmp.Free;
  inherited;
end;
//------------------------------------------------------------------------------

function TPanel.GetOffset: TPoint;
begin
  result := Point(fOffsetX, fOffsetY);
end;
//------------------------------------------------------------------------------

procedure TPanel.SetOffset(const Pt: TPoint);
begin
  fOffsetX := pt.X;
  fOffsetY := pt.Y;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TPanel.GetScale: double;
begin
  if fScaleType = stStretched then result := 1
  else Result := fScale;
end;
//------------------------------------------------------------------------------

procedure TPanel.SetScaleType(value: TScaleType);
begin
  if fScaleType = value then Exit;
  fScaleType := value;
  UpdateCursor;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TPanel.SetScale(value: double);
var
  rec: TRect;
begin
  if value < fMinScale then value := fMinScale
  else if value > fMaxScale then value := fMaxScale;
  SetScaleType(stScaled);
  //zoom in or out relative to the center of the image
  rec := ClientRect;
  BitmapScaleAtPos(value, Point(RectWidth(rec) div 2, RectHeight(rec) div 2));
end;
//------------------------------------------------------------------------------

function TPanel.ClientToBitmap(var clientPt: TPoint): Boolean;
var
  innerMarg: integer;
begin
  innerMarg := GetInnerMargin;
  clientPt.X := Round((clientPt.X - innerMarg + fOffsetX)/fScale);
  clientPt.Y := Round((clientPt.Y - innerMarg  + fOffsetY)/fScale);
  Result := (clientPt.X >= 0) and (clientPt.X < fBmp.Width) and
    (clientPt.Y >= 0) and (clientPt.Y < fBmp.Height);
end;
//------------------------------------------------------------------------------

procedure TPanel.BitmapToClient(var pt: TPoint);
var
  innerMarg: integer;
begin
  innerMarg := GetInnerMargin;
  //fDstRect == InnerClientRect
  pt.X := Round(pt.X * fScale) - fOffsetX + innerMarg;
  pt.Y := Round(pt.Y * fScale) - fOffsetY + innerMarg;
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
  charCode: Word;
  shiftState: TShiftState;
begin
  inherited;
  shiftState := KeyDataToShiftState(Message.KeyData);

  if Assigned(fOnKeyDown) then
  begin
    charCode := Message.CharCode;
    fOnKeyDown(Self, charCode, shiftState);
    if charCode = 0 then Exit;
  end;

  if (Message.CharCode >= Ord('V')) and (ssCtrl in shiftState) and
    fCopyPasteEnabled then
  begin
    PasteFromClipboard;
    Exit;
  end;

  if fBmp.Empty then Exit;

  if (Message.CharCode >= Ord('C')) and (ssCtrl in shiftState) and
    fCopyPasteEnabled then
  begin
    CopyToClipboard;
    Exit;
  end;

  if (fScaleType = stStretched) or not fZoomScrollEnabled then Exit;

  case Message.CharCode of
    VK_LEFT..VK_DOWN:
      begin
        if ssCtrl in shiftState then
        begin
          //zoom in and out with CTRL+UP and CTRL+DOWN respectively
          midPoint := Point(ClientWidth div 2, ClientHeight div 2);
          case Message.CharCode of
            VK_UP: BitmapScaleAtPos(fScale * 0.9, midPoint);
            VK_DOWN: BitmapScaleAtPos(fScale * 1.1, midPoint);
            else Exit;
          end;
        end else
        begin
          //otherwise scroll the image with the arrow keys
          if ssShift in shiftState then
            mul := 5 else //ie scrolls 5 times faster with Shift key down
            mul := 1;
          case Message.CharCode of
            VK_LEFT: dec(fOffsetX, 5 * mul);
            VK_RIGHT: inc(fOffsetX, 5 * mul);
            VK_UP: dec(fOffsetY, 5 * mul);
            VK_DOWN: inc(fOffsetY, 5 * mul);
          end;
          if assigned(fOnScrolling) then fOnScrolling(self);
        end;
        Invalidate;
      end;

      Ord('0'): if not (ssCtrl in shiftState) then SetScaleType(stFit);
      Ord('1')..Ord('9'): if not (ssCtrl in shiftState) then
        begin
          SetScaleType(stScaled);
          if ssShift in shiftState then
            Scale := (Message.CharCode - Ord('0')) /10 else
            Scale := Message.CharCode - Ord('0');
        end;
  end;
end;
//------------------------------------------------------------------------------

procedure TPanel.WMKeyUp(var Message: TWMKey);
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
  if not fBmp.Empty and fZoomScrollEnabled and
    (fScaleType <> stStretched) then
  begin
    fMouseDown := true;
    rec := GetInnerClientRect;
    if (X > rec.Right) then
      fMouseDownOverBevelV := true
    else if (Y > rec.Bottom) then
      fMouseDownOverBevelH := true;
    fMousePos := Point(X, Y);
  end;
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
  mobH, mobV: Boolean;
begin
  if MouseInsideInnerClientRect then
  begin
    UpdateCursor;
    inherited;
  end;

  if not fMouseDown then
  begin
    if fShowScrollbars then
    begin
      innerMarg := GetInnerMargin;
      mobV := (X > ClientWidth - innerMarg);
      mobH := (Y > ClientHeight - innerMarg);
      //now check for change in state ...
      if fMouseOverBevelVert <> mobV then
      begin
        fMouseOverBevelVert := mobV;
        if mobV and ShowVertScrollButton then
          Cursor := crSizeNS;
        Invalidate;
      end;
      if fMouseOverBevelHorz <> mobH then
      begin
        fMouseOverBevelHorz := mobH;
        if mobH and ShowHorzScrollButton then
          Cursor := crSizeWE;
        Invalidate;
      end;
    end;
    Exit;
  end;

  fMouseOverBevelVert := false;
  fMouseOverBevelHorz := false;
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
    //click and drag to alter view (ie offset :))
    fOffsetX := fOffsetX + (fMousePos.X - X);
    fOffsetY := fOffsetY + (fMousePos.Y - Y);
  end;
  if assigned(fOnScrolling) then fOnScrolling(self);
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
  fMouseOverBevelVert := false;
  fMouseOverBevelHorz := false;
  invalidate;
end;
//------------------------------------------------------------------------------

procedure TPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if fMouseOverBevelVert then
    fMouseOverBevelVert := false
  else if fMouseOverBevelHorz then
    fMouseOverBevelHorz := false
  else Exit;
  Invalidate;
end;
//------------------------------------------------------------------------------

procedure TPanel.SetFileDropEnabled(value: Boolean);
begin
  if fFileDropEnabled = value then Exit;
  if HandleAllocated then
  begin
    if fFileDropEnabled then
      DragAcceptFiles(Handle, false) else
      DragAcceptFiles(Handle, true);
  end;
  fFileDropEnabled := value;
end;
//------------------------------------------------------------------------------

procedure TPanel.SetScalingCursor(value: integer);
begin
  fScalingCursor := value;
  UpdateCursor;
end;
//------------------------------------------------------------------------------

procedure TPanel.SetZoomScrollEnabled(value: Boolean);
begin
  fZoomScrollEnabled := value;
  UpdateCursor;
end;
//------------------------------------------------------------------------------

procedure TPanel.SetScrollbarsVisible(value: Boolean);
begin
  fShowScrollbars := value;
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

function TPanel.PasteFromClipboard: Boolean;
begin
  Clipboard.Open;
  try
    result := Clipboard.HasFormat(CF_BITMAP);
    if not result then Exit;
{$IFDEF FPC}
     fBmp.LoadFromClipBoardFormat(CF_BITMAP);
{$ELSE}
    fBmp.LoadFromClipBoardFormat(CF_BITMAP,
      ClipBoard.GetAsHandle(CF_BITMAP),0);
{$ENDIF}
  finally
    Clipboard.Close;
  end;
  fOffsetX := 0; fOffsetY := 0;
  if fScaleType <> stFit then fScale := 1;
  Invalidate;
  if assigned(fOnPaste) then fOnPaste(self);
end;
//------------------------------------------------------------------------------

procedure TPanel.WMDropFiles(var Msg: TMessage);
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

  if assigned(fOnDragDrop) then
    fOnDragDrop(Self, filename)
  else if (Lowercase(ExtractFileExt(filename)) = '.bmp') then
  try
    fBmp.LoadFromFile(filename);
  except
  end;
end;
//------------------------------------------------------------------------------

procedure TPanel.CreateWnd;
begin
  inherited;
  if fFileDropEnabled then
    DragAcceptFiles(Handle, True);
end;
//------------------------------------------------------------------------------

procedure TPanel.DestroyWnd;
begin
  if fFileDropEnabled then DragAcceptFiles(Handle, False);
  inherited;
end;
//------------------------------------------------------------------------------

procedure TPanel.ClearBitmap;
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

function TPanel.ShowHorzScrollButton: Boolean;
begin
  Result := (fBmp.Width * fScale > ClientWidth - GetInnerMargin *2 +1);
end;
//------------------------------------------------------------------------------

function TPanel.ShowVertScrollButton: Boolean;
begin
  Result := (fBmp.Height * fScale > ClientHeight - GetInnerMargin *2 +1);
end;
//------------------------------------------------------------------------------

type TControl = class(Controls.TControl); //just to access (protected) Color

procedure TPanel.Paint;
const
  Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
{$IF COMPILERVERSION >= 17} //?? when was TVerticalAlignment introduced?
  VAlignments: array[TVerticalAlignment] of Longint = (DT_TOP,DT_BOTTOM,DT_VCENTER);
{$IFEND}
var
  marginOff, w,h: integer;
  tmpRec, srcRec, srcScaled: TRect;
  tmpBmp: TBitmap;
  bf: BLENDFUNCTION;
  backgroundPainted: Boolean;
  Flags: Longint;
  themeColor: TColor;

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
  //paint borders
  fDstRect := ClientRect;
  if TabStop and Focused then
  begin
    Canvas.Brush.Color := fFocusedColor;
    Canvas.FillRect(fDstRect);
    fDstRect := InnerClientRect;
  end;

  //paint inner client rect or all of client rect if not focused
  backgroundPainted := ParentBackground and
{$IFDEF STYLESERVICES}
    StyleServices.Enabled and (seClient in StyleElements) and
{$ELSE}
    ThemeServices.ThemesEnabled and
{$ENDIF}
    Succeeded(DrawThemeParentBackground(Handle, Canvas.Handle, @fDstRect));

  if not backgroundPainted then
  begin
    if ParentColor then
      Canvas.Brush.Color := TControl(parent).Color else
      Canvas.Brush.Color := self.Color;
    Canvas.FillRect(fDstRect);
  end;

  //paint the outer bevel
  fDstRect := ClientRect;
  case BevelOuter of
    bvLowered: Frame3D(Canvas, fDstRect, clBtnShadow, clBtnHighlight, 1);
    bvRaised:  Frame3D(Canvas, fDstRect, clBtnHighlight, clBtnShadow, 1);
  end;

  //paint the inner bevel
  fDstRect := InnerClientRect;
  case BevelInner of
    bvLowered: Frame3D(Canvas, fDstRect, clBtnShadow, clBtnHighlight, 1);
    bvRaised:  Frame3D(Canvas, fDstRect, clBtnHighlight, clBtnShadow, 1);
  end;

  //paint panel caption
  if Caption <> '' then
  begin
    canvas.Brush.Style := bsClear;
    canvas.Font := Self.Font;

    Flags := DT_EXPANDTABS or DT_SINGLELINE  or Alignments[Alignment] or
{$IF COMPILERVERSION >= 17} VAlignments[VerticalAlignment]; {$ELSE} DT_VCENTER; {$IFEND}
    Flags := DrawTextBiDiModeFlags(Flags);

{$IFDEF STYLESERVICES}
    if StyleServices.Enabled and (seFont in StyleElements) and
      GetThemeColor('BUTTON', BP_GROUPBOX, 0, TMT_TEXTCOLOR, themeColor) then
        Canvas.Font.Color := themeColor;
{$ELSE}
    if ThemeServices.ThemesEnabled and
      GetThemeColor('BUTTON', BP_GROUPBOX, 0, TMT_TEXTCOLOR, themeColor) then
        Canvas.Font.Color := themeColor;
{$ENDIF}
    DrawText(canvas.Handle, PChar(Caption), -1, fDstRect, Flags);
  end;

  if fBmp.Empty then Exit;

  marginOff := GetInnerMargin;
  if Assigned(fOnBeginPaint) then fOnBeginPaint(Self);

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
    if fAutoCenter then
    begin
      //if srcScaled is smaller than dstRect then center the image
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
    end else
    begin
      if (RectWidth(srcScaled) < RectWidth(fDstRect)) then
        SetRectWidth(fDstRect, RectWidth(srcScaled));
      if RectHeight(srcScaled) < RectHeight(fDstRect) then
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
      tmpBmp.Width := RectWidth(srcRec);
      tmpBmp.Height := RectHeight(srcRec);
      tmpBmp.Canvas.CopyRect(Rect(0,0,RectWidth(srcRec),
        RectHeight(srcRec)), fBmp.Canvas, srcRec);
      Canvas.StretchDraw(fDstRect, tmpBmp);
    finally
      tmpBmp.Free;
    end;
  end;

  if fShowScrollbars and (BorderWidth > DpiScale(FMinScrollBtnSize + 5)) then
  begin
    //draw vertical scrollbar
    tmpRec := GetInnerClientRect;
    if ShowVertScrollButton then
    begin
      h := Round(fBmp.Height * fScale);
      w := ClientWidth;
      tmpRec.Left := w - marginOff + DpiScale(2);
      tmpRec.Right := w - DpiScale(3);
      OffsetRect(tmpRec, 0, Round(fOffsetY * RectHeight(tmpRec) / h));
      SetRectHeight(tmpRec, RectHeight(tmpRec) * RectHeight(tmpRec) div h);
      if fMouseDownOverBevelV or
        (not LeftMouseBtnDown and fMouseOverBevelVert) then
          Canvas.Brush.Color := fScrollButtonColorHot else
          Canvas.Brush.Color := fScrollButtonColor;
      Canvas.FillRect(tmpRec);
      DrawScrollButton(tmpRec, true);
    end;

    //draw horizontal scrollbar
    tmpRec := GetInnerClientRect;
    if ShowHorzScrollButton then
    begin
      w := Round(fBmp.Width * fScale);
      h := ClientHeight;
      tmpRec.Top := h - marginOff + DpiScale(2);
      tmpRec.Bottom := h - DpiScale(3);
      OffsetRect(tmpRec, Round(fOffsetX * RectWidth(tmpRec) / w), 0);
      SetRectWidth(tmpRec, RectWidth(tmpRec) * RectWidth(tmpRec) div w);
      if fMouseDownOverBevelH or
        (not LeftMouseBtnDown and fMouseOverBevelHorz) then
          Canvas.Brush.Color := fScrollButtonColorHot else
          Canvas.Brush.Color := fScrollButtonColor;
      Canvas.FillRect(tmpRec);
      DrawScrollButton(tmpRec, true);
    end;
  end;
  if Assigned(fOnEndPaint) then fOnEndPaint(Self);
end;
//------------------------------------------------------------------------------

procedure TPanel.CMFocusChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;
//------------------------------------------------------------------------------

function TPanel.GetInnerMargin: integer;
begin
  //nb: BorderWidth is the space between outer and inner bevels
  Result := BorderWidth;
  if BevelInner <> bvNone then inc(result, BevelWidth);
  if BevelOuter <> bvNone then inc(result, BevelWidth);
  //BorderStyle changes the OUTSIDE of the panel so won't affect InnerMargin.
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

function TPanel.MouseInsideInnerClientRect: Boolean;
var
  mousePos: TPoint;
begin
  GetCursorPos(mousePos);
  mousePos := ScreenToClient(mousePos);
  Result := PtInRect(GetInnerClientRect, mousePos);
end;
//------------------------------------------------------------------------------

procedure TPanel.SetAutoCenter(value: Boolean);
begin
  if value = fAutoCenter then Exit;
  fAutoCenter := value;
  //when fAutoCenter is disabled, then neither
  //SCALE_BEST_FIT or SCALE_STRETCHED work sensibly
  if not fAutoCenter then SetScale(1);
end;
//------------------------------------------------------------------------------

procedure TPanel.UpdateCursor;
begin
  if not fBmp.Empty and MouseInsideInnerClientRect and
    (fScaleType = stScaled) and fZoomScrollEnabled then
  begin
    if (fScalingCursor <> 0) then cursor := fScalingCursor
    else cursor := crHandPoint;
  end
    else cursor := crDefault;
end;
//------------------------------------------------------------------------------

procedure TPanel.BitmapScaleAtPos(newScale: double; const mousePos: TPoint);
begin
  newScale := Max(fMinScale, Min(fMaxScale, newScale));
  SetScaleType(stScaled);
  fOffsetX := Round((mousePos.X + fOffsetX) * newScale/fScale - mousePos.X);
  fOffsetY := Round((mousePos.Y + fOffsetY) * newScale/fScale - mousePos.Y);
  fScale := newScale;
  if fBmp.Empty then Exit;
  Invalidate;
end;
//------------------------------------------------------------------------------

{$IFDEF GESTURES}
procedure TPanel.Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  changeFrac: double;
  marg: integer;
begin
  if fBmp.Empty or not fZoomScrollEnabled or
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
          if assigned(fOnScrolling) then fOnScrolling(self);
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
  if Result then Exit;
  if fBmp.Empty or not fZoomScrollEnabled or
    (fScaleType = stStretched) then Exit;
  {$IFNDEF FPC}
  MousePos := ScreenToClient(MousePos);
  {$ENDIF}
  marg := GetInnerMargin;
  MousePos := OffsetPoint(MousePos, -marg, -marg);
  BitmapScaleAtPos(fScale * 0.9, MousePos);
  Result := true;
end;
//------------------------------------------------------------------------------

function TPanel.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  marg: integer;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if Result then Exit;
  if fBmp.Empty or not fZoomScrollEnabled or
    (fScaleType = stStretched) then Exit;
  {$IFNDEF FPC}
  MousePos := ScreenToClient(MousePos);
  {$ENDIF}
  marg := GetInnerMargin;
  MousePos := OffsetPoint(MousePos, -marg, -marg);
  BitmapScaleAtPos(fScale * 1.1, MousePos);
  Result := true;
end;
//------------------------------------------------------------------------------

end.
