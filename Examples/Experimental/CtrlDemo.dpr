program CtrlDemo;

uses
  SysUtils,
  Classes,
  Windows,
  CommDlg,
  Types,
  Messages,
  TypInfo,
  Img32,
  Img32.Vector,
  Img32.Draw,
  Img32.Layers,
  Img32.Ctrls,
  Img32.Text,
  Img32.Extra,
  Img32.Fmt.BMP,
  Img32.Fmt.SVG,
  Img32.SVG.Reader;

(*
  NOTES:
  1. THIS APP USES IMAGE32 FOR **ALL** GUI CONTROLS
  IN OTHER WORDS - IT DOESN'T USE DELPHI'S VCL OR FMX FRAMEWORKS!
  2. THIS APP CONTINUES TO BE EXPERIMENTAL, MOSTLY BECAUSE
  THE GUI CONTROLS REMAIN VERY INCOMPLETE
*)

{$R Lorem.res}
{$R SvgImages.res}
{$R ..\resources.res}

{$WARN SYMBOL_PLATFORM OFF}

type

  // TEventPropHandler - contains properties and NotifyEvents
  // used by TCtrlStorageManager
  TEventPropHandler = class(TEventPropertyHandler)
  private
    fArial14      : TFontCache;
    fUnscaledFont : TFontCache;
    fSvgList      : TSvgImageList32;
    fSvgList2     : TSvgImageList32;
  public
    // events
    procedure LoadClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure SliderClick(Sender: TObject);
    procedure Slider2Click(Sender: TObject);
    procedure ClickMe(Sender: TObject);
    procedure ClickBtn(Sender: TObject);
    procedure CheckboxClick(Sender: TObject);
    procedure DesignModeClick(Sender: TObject);
    // public properties (these don't need to be published)
    property Arial14: TFontCache read fArial14 write fArial14;
    property UnscaledFont: TFontCache read fUnscaledFont write fUnscaledFont;
    property svgList   : TSvgImageList32 read fSvgList write fSvgList;
    property svgList2  : TSvgImageList32 read fSvgList2 write fSvgList2;
  end;

var
  WinClass: TWndClass;
  Inst, mainHdl: THandle;
  Msg: TMsg;

  fontReader  : TFontReader;

  // storageMngr runs the show together with its event handler
  storageMngr : TCtrlStorageManager;
  // this will be storageMngr's event handler
  epHandler: TEventPropHandler;

  // these 2 object are created in storageMngr's constructor
  // and exposed as properties of storageMngr and it's just
  // conveneint to expose these as global variables.
  layeredImg32: TLayeredImage32;
  rootCtrl    : TPanelCtrl;

  // these objects are created by the Setup procedure below
  // (or loaded from the stored XML file). It's also
  // convenient to expose them as global variables.
  pageCtrl    : TPageCtrl;
  statusCtrl  : TStatusbarCtrl;

  currentScale  : double;
  updateRect    : TRect;
  imageSize64   : double;
  imageSize24   : double;

  // designing globals
  designing     : Boolean;
  designTarget  : TCustomCtrl;
  clickLayer    : TLayer32;
  sizingGroup   : TSizingGroupLayer32;
  clickPt       : TPoint;

  sizeCursor  : HIcon;
  handCursor  : HIcon;
  arrowCursor : HIcon;

const
  DoLoadFromStorage = false; //true; //

//------------------------------------------------------------------------------
//Miscellaneous functions
//------------------------------------------------------------------------------

function IsOwnedBy(ctrl: TLayer32; ownerCtrlClass: TCustomCtrlClass): Boolean;
begin
  Result := false;
  while Assigned(ctrl.Parent) do
    if ctrl.Parent is ownerCtrlClass then
    begin
      Result := true;
      Break;
    end
    else
      ctrl := ctrl.Parent;
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

procedure ResizeMainWindow(hdl: HWnd; width, height: Cardinal);
var
  winRec, clientRec: TRect;
  dx,dy: cardinal;
begin
  GetWindowRect(hdl, winRec);
  GetClientRect(hdl, clientRec);
  dx := winrec.Width - clientRec.Width;
  dy := winrec.Height - clientRec.Height;
  SetWindowPos(hdl, 0,0,0, width + dx, height + dy, SWP_NOZORDER or SWP_NOMOVE);
end;
//------------------------------------------------------------------------------

function CenterWindow(windowHdl: THandle): boolean;
var
  DesktopRec, WindowRec: TRect;
  l,t: integer;
begin
  result := SystemParametersInfo(SPI_GETWORKAREA, 0, @DesktopRec, 0);
  if not result or (windowHdl = 0) then exit;
  GetWindowRect(windowHdl,WindowRec);
  l := ((DesktopRec.Right - DesktopRec.Left) - (WindowRec.Right - WindowRec.Left)) div 2;
  t := ((DesktopRec.Bottom - DesktopRec.Top) - (WindowRec.Bottom - WindowRec.Top)) div 2;
  if l < 0 then l := 0;
  if t < 0 then t := 0;
  result := SetWindowPos(windowHdl, 0, l, t, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;
//------------------------------------------------------------------------------

function OpenSaveXmlDialog(var filename: string; useSaveDialog: Boolean): Boolean;
var
  len: integer;
  ofn: TOpenfilename;
  tmpName: string;
begin
  Result := false;
  len := Length(filename);
  if (len > MAX_PATH) then Exit;
  SetLength(tmpName, MAX_PATH +1);
  if len > 0 then
    Move(filename[1], tmpName[1], (len +1) * SizeOf(Char));

  FillChar(ofn, SizeOf(ofn), 0);
  ofn.lStructSize := SizeOf(ofn);
  ofn.lpstrFile := PChar(tmpName);
  ofn.lpstrFilter := 'XML Files (*.xml)'#0'*.xml'#0;
  ofn.nMaxFile := MAX_PATH;
  if useSaveDialog then
    ofn.Flags := OFN_EXPLORER or OFN_HIDEREADONLY else
    ofn.Flags := OFN_EXPLORER or OFN_FILEMUSTEXIST or OFN_HIDEREADONLY;
  ofn.lpstrDefExt := 'xml';
  if useSaveDialog then
    Result := GetSaveFileName(ofn) else
    Result := GetOpenFileName(ofn);
  if Result then
    filename := ofn.lpstrFile;
end;
//------------------------------------------------------------------------------

procedure SetDesignTarget(ctrl: TCustomCtrl);
begin
  if ctrl = designTarget then Exit;
  FreeAndNil(sizingGroup);
  designTarget := ctrl;
  if Assigned(designTarget) then
    sizingGroup := CreateSizingButtonGroup(designTarget, ssCorners,
      bsRound, DefaultButtonSize, clLime32);
  InvalidateRect(mainHdl, nil, false);
end;
//------------------------------------------------------------------------------

procedure DoScale(newScale: double);
var
  scaleDelta  : double;
  designScale : double;
  ImgSz       : integer;
begin
  scaleDelta := newScale / currentScale;
  if ValueAlmostOne(scaleDelta) then Exit;
  currentScale := newScale;

  // StorageMngr.DesignScale and storageMngr.DesignResolution are
  // used when streaming controls to and from XML
  StorageMngr.DesignScale := newScale;

  if storageMngr.DesignResolution > 0 then
    designScale := storageMngr.DesignResolution / DpiAwareOne else
    designScale := 1;

  // Note: StorageMngr doesn't rescale its content because scaling
  // may not apply to all of its contents (as is the case here).

  // scale font and SVG image sizes
  with epHandler do
  begin
    UnscaledFont.FontHeight := DpiAware(14) * designScale;
    arial14.FontHeight := DpiAware(14) * designScale * newScale;

    ImgSz := Round(imageSize64 * designScale * newScale);
    svgList.DefaultWidth := ImgSz;
    svgList.DefaultHeight := ImgSz;

    ImgSz := Round(imageSize24 * designScale * newScale);
    svgList2.DefaultWidth := ImgSz;
    svgList2.DefaultHeight := ImgSz;
  end;

  // finally, update just pageCtrl and its contents
  // (because we don't scale everything)
  pageCtrl.Scale(scaleDelta);
end;

//------------------------------------------------------------------------------
// TEventPropertyHandler1 events
//------------------------------------------------------------------------------

procedure TEventPropHandler.LoadClick(Sender: TObject);
var
  filename: string;
begin
  filename := '.\*.xml';
  if not OpenSaveXmlDialog(filename, false) then Exit;
  storageMngr.LoadFromFile(filename);
  // LoadFromFile() will delete all controls contained by
  // RootCtrl prior to loading the new ones.

  pageCtrl := rootCtrl.FindChildByClass(TPageCtrl) as TPageCtrl;
  statusCtrl := rootCtrl.FindChildByClass(TStatusbarCtrl) as TStatusbarCtrl;

  // storageMngr.DesignScale may have changed so ...
  currentScale := storageMngr.DesignScale;
  DoScale(currentScale);

  ResizeMainWindow(mainHdl, layeredImg32.Width, layeredImg32.Height);
end;
//------------------------------------------------------------------------------

procedure TEventPropHandler.SaveClick(Sender: TObject);
var
  filename: string;
begin
  filename := '.\*.xml';
  if not OpenSaveXmlDialog(filename, true) then Exit;
  storageMngr.SaveToFile(filename, StorageMngr.DesignScale);
  statusCtrl.Caption := 'Controls saved';
end;
//------------------------------------------------------------------------------

procedure TEventPropHandler.SliderClick(Sender: TObject);
var
  senderPos, scale: double;
begin
  if not Assigned(sender) or not (Sender is TSliderCtrl) then Exit;

  senderPos := (Sender as TSliderCtrl).Position;
  scale := (100 + senderPos) * 0.01;
  DoScale(scale);
end;
//------------------------------------------------------------------------------

procedure TEventPropHandler.Slider2Click(Sender: TObject);
var
  senderPos: double;
begin
  senderPos := (Sender as TSliderCtrl).Position;
  with pageCtrl.FindChildByClass(TProgressCtrl) as TProgressCtrl do
    Position := senderPos;
end;
//------------------------------------------------------------------------------

procedure TEventPropHandler.ClickMe(Sender: TObject);
begin
  windows.MessageBox(0, '''Click Me'' button clicked!', 'CtrlDemo', 0);
end;
//------------------------------------------------------------------------------

procedure TEventPropHandler.ClickBtn(Sender: TObject);
begin
  statusCtrl.Caption := TButtonCtrl(Sender).Caption + ' clicked';
end;
//------------------------------------------------------------------------------

procedure TEventPropHandler.CheckboxClick(Sender: TObject);
begin
  with (Sender as TCheckboxCtrl) do
    case State of
      tsUnknown : State := tsNo;
      tsYes     : State := tsUnknown;
      tsNo      : State := tsYes;
    end;
end;
//------------------------------------------------------------------------------

procedure TEventPropHandler.DesignModeClick(Sender: TObject);
begin
  designing := TCheckboxCtrl(Sender).State = tsYes;
  if not designing then SetDesignTarget(nil);
end;

//------------------------------------------------------------------------------
// Windows message handler - that passes keyboard and
// mouse messages to storageMngr for processing
//------------------------------------------------------------------------------

function WindowProc(hWnd, uMsg,	wParam: WPARAM; lParam: LPARAM): Integer; stdcall;

  procedure UpdateTargetPosDisplay;
  begin
    if Assigned(designTarget) then
      with designTarget do
        statusCtrl.Caption := Format('  %1.0n, %1.0n',[Left, Top]);
  end;

  procedure UpdateFocusedCtrlDisplay;
  begin
    if Assigned(storageMngr.RootCtrl.FocusedCtrl) then
      with storageMngr.RootCtrl.FocusedCtrl do
        if Caption = '' then
          statusCtrl.Caption := Name else
          statusCtrl.Caption := StringReplace(Caption, '&', '', []);
  end;

var
  key     : Word;
  chr     : Char;
  w,h     : integer;
  pt      : TPoint;
  ps      : TPAINTSTRUCT;
  dc      : HDC;
  img     : TImage32;
  dx,dy   : integer;
  rec     : TRectD;
  shift   : TShiftState;
  layer   : TLayer32;
begin
  case uMsg of
    WM_LBUTTONDOWN:
      begin
        Result := 0;
        clickPt := Img32.vector.Point(
          SmallInt(LoWord(lParam)),
          SmallInt(HiWord(lParam)));
        if designing then
        begin
          clickLayer := layeredImg32.GetLayerAt(clickPt);
          if IsOwnedBy(clickLayer, TPagePnlCtrl) then
          begin
            SetDesignTarget(clickLayer as TCustomCtrl);
            SetCursor(handCursor);
            UpdateTargetPosDisplay;
            Exit;
          end
          else if (clickLayer is TButtonDesignerLayer32) then
          begin
            SetCursor(sizeCursor);
            Exit;
          end else
          begin
            SetDesignTarget(nil);
            clickLayer := nil;
          end;
        end;
        // not designing so get storageMngr to process
        storageMngr.MouseDown(mbLeft, WParamToShiftState(wParam), clickPt);
        //UpdateFocusedCtrlDisplay;
        if storageMngr.RepaintRequired then
          InvalidateRect(hWnd, nil, false);
      end;
    WM_MOUSEMOVE:
      begin
        Result := 0;
        pt := Img32.vector.Point(
          SmallInt(LoWord(lParam)), SmallInt(HiWord(lParam)));
        dx := pt.X - clickPt.X; dy := pt.Y - clickPt.Y;

        if designing then
        begin
          if not assigned(clickLayer) then
          begin
            layer := layeredImg32.GetLayerAt(pt);
            if Assigned(layer) then
            begin
              if (layer is TButtonDesignerLayer32) then
                SetCursor(sizeCursor)
              else if layer = designTarget then
                SetCursor(handCursor)
              else
                SetCursor(arrowCursor);
            end;
            Exit;
          end;

          if (clickLayer = designTarget) then
          begin
            SetCursor(handCursor);
            designTarget.Offset(dx,dy);
            sizingGroup.Offset(dx,dy);
            clickPt := pt;
            UpdateTargetPosDisplay;
            InvalidateRect(hWnd, nil, false);
          end else if (clickLayer is TButtonDesignerLayer32) then
          begin
            SetCursor(sizeCursor);
            clickLayer.Offset(dx, dy);
            rec := RectD(UpdateSizingButtonGroup(clickLayer));
            rec := designTarget.Parent.MakeRelative(rec);
            designTarget.SetInnerBounds(rec);
            clickPt := pt;
            InvalidateRect(hWnd, nil, false);
          end;
        end else
        begin
          // not designing so get storageMngr to process
          storageMngr.MouseMove(mbLeft, WParamToShiftState(wParam), pt);
          if storageMngr.RepaintRequired then
            InvalidateRect(hWnd, nil, false);
        end;
      end;
    WM_LBUTTONUP:
      begin
        pt := Img32.vector.Point(
          SmallInt(LoWord(lParam)),
          SmallInt(HiWord(lParam)));
        storageMngr.MouseUp(mbLeft, WParamToShiftState(wParam), pt);
        if storageMngr.RepaintRequired then
          InvalidateRect(hWnd, nil, false);
        clickLayer := nil;
        Result := 0;
      end;
    WM_MOUSEWHEEL:
      begin
        if designing then clickLayer := nil;
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
        if not designing then
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

        if (key = VK_ESCAPE) then
        begin
          PostQuitMessage(0);
        end

        else if designing and Assigned(designTarget) then
        begin
          case Key of
            VK_DELETE:
              begin
                FreeAndNil(sizingGroup);
                FreeAndNil(designTarget);
                InvalidateRect(hWnd, nil, false);
              end;
            VK_DOWN:
              begin
                if Shift and ssCtrl <> 0 then w := 5 else w := 1;
                designTarget.Offset(0,w);
                sizingGroup.Offset(0,w);
                UpdateTargetPosDisplay;
                InvalidateRect(hWnd, nil, false);
              end;
            VK_UP:
              begin
                if Shift and ssCtrl <> 0 then w := 5 else w := 1;
                designTarget.Offset(0,-w);
                sizingGroup.Offset(0,-w);
                UpdateTargetPosDisplay;
                InvalidateRect(hWnd, nil, false);
              end;
            VK_RIGHT:
              begin
                if Shift and ssCtrl <> 0 then w := 5 else w := 1;
                designTarget.Offset(w,0);
                sizingGroup.Offset(w,0);
                UpdateTargetPosDisplay;
                InvalidateRect(hWnd, nil, false);
              end;
            VK_LEFT:
              begin
                if Shift and ssCtrl <> 0 then w := 5 else w := 1;
                designTarget.Offset(-w,0);
                sizingGroup.Offset(-w,0);
                UpdateTargetPosDisplay;
                InvalidateRect(hWnd, nil, false);
              end;
          end;
        end
        else
        begin

          if (key = VK_TAB) then
            statusCtrl.Caption := 'TAB key pressed';

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
        storageMngr.Resize(w,h);
        InvalidateRect(hWnd,nil,true);
        Result := 0;
      end;
    WM_PAINT:
      begin
        Result := 0;
        img := layeredImg32.GetMergedImage(false, updateRect);
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

function GetUnicodeTextResource(const resName: string; resType: PChar): UnicodeString;
var
  bom: Word;
  len: integer;
  rs: TResourceStream;
begin
  Result := '';
  rs := TResourceStream.Create(hInstance, resName, resType);
  try
    len := rs.Size div 2 - 1;
    //nb: skipping unicode BOM
    if len < 1 then Exit;
    rs.Read(bom, 2);
    if bom <> $FEFF then Exit;
    //rs.Position := 2;
    SetLength(Result, len);
    rs.ReadBuffer(Result[1], len * 2);
  finally
    rs.Free;
  end;
end;

//------------------------------------------------------------------------------
// SetupCtrls - creates numerous GUI controls for the main Window.
// Alternatively, controls can be constructed using the properties and
// events stored in an xml file
//------------------------------------------------------------------------------

procedure SetupCtrls;
var
  i, pad    : integer;
  j,k,h,w   : double;
  bevelSize  : double;
  pagePnl     : TPagePnlCtrl;
  topPnlCtrl  : TPanelCtrl;
  sliderCtrl  : TSliderCtrl;
begin
  currentScale := 1;
  bevelSize := DPIAware(2);

  // rootCtrl is the container for all other controls
  // that's automatically created by storageMngr.
  rootCtrl.Margin := DPIAware(10);
  rootCtrl.Font := epHandler.UnscaledFont;
  rootCtrl.Color := clBtnFace32; //$FFFFFFD0;//

  statusCtrl := layeredImg32.AddLayer(TStatusbarCtrl, rootCtrl) as TStatusbarCtrl;
  with statusCtrl do
  begin
    BevelHeight := DPIAware(1.5);
    Color := clNone32;
    AutoPosition := apBottom;
    Caption := 'This GUI app doesn''t use either VCL or FMX, just Image32 !';
  end;

  topPnlCtrl := layeredImg32.AddLayer(TPanelCtrl, rootCtrl) as TPanelCtrl;
  topPnlCtrl.AutoPosition := apTop;
  topPnlCtrl.Height := DPIAware(40);
  topPnlCtrl.Color := clNone32;
  topPnlCtrl.BevelHeight := 0;
  topPnlCtrl.ShadowSize := DPIAware(10);

  //filler panel
  with layeredImg32.AddLayer(TPanelCtrl, rootCtrl) as TPanelCtrl do
  begin
    AutoPosition := apTop;
    Height := DPIAware(20);
    Color := clNone32;
    BevelHeight := 0;
  end;

  sliderCtrl := layeredImg32.AddLayer(TSliderCtrl, topPnlCtrl) as TSliderCtrl;
  with sliderCtrl do
  begin
    Orientation := soHorizontal;
    SetInnerBounds(DPIAware(RectD(70, 5, 345, 20)));
    BevelHeight := DPIAware(2);
    Min := -50;
    Max := 100;
    Position := 0;
    OnSlider := epHandler.SliderClick;
  end;

  with layeredImg32.AddLayer(TLabelCtrl, topPnlCtrl,'') as TLabelCtrl do
  begin
    Caption := '&Scale:';
    SetInnerBounds(DPIAware(RectD(20, 5, 70, 25)));
    TargetCtrl := sliderCtrl;
  end;

  with layeredImg32.AddLayer(TButtonCtrl, topPnlCtrl,'') as TButtonCtrl do
  begin
    Caption := '&Load ...';
    SetInnerBounds(DPIAware(RectD(365, 2, 435, 32)));
    BevelHeight := bevelSize;
    OnClick := epHandler.LoadClick;
  end;

  with layeredImg32.AddLayer(TButtonCtrl, topPnlCtrl,'') as TButtonCtrl do
  begin
    Caption := 'S&ave ...';
    SetInnerBounds(DPIAware(RectD(440, 2, 510, 32)));
    BevelHeight := bevelSize;
    OnClick := epHandler.SaveClick;
  end;

  with layeredImg32.AddLayer(TCheckboxCtrl, topPnlCtrl,'') as TCheckboxCtrl do
  begin
    Caption := '&Design Mode';
    SetInnerBounds(DPIAware(RectD(530, 2, 660, 32)));
    BevelHeight := bevelSize;
    State := tsNo;
    OnClick := epHandler.DesignModeClick;
  end;

  //add a page control with 3 tabs
  pageCtrl := layeredImg32.AddLayer(TPageCtrl, rootCtrl) as TPageCtrl;
  pageCtrl.AutoPosition := apClient;
  pageCtrl.BevelHeight := DPIAware(3);
  pageCtrl.AddTabs(['Page &1', 'Page &2', 'Page &Three']);
  pageCtrl.Font := epHandler.arial14;

  //now add a number of ctrls to each page

  // PAGE 1 ///////////////////////////////////////////////////////
  pagePnl := pageCtrl.Panel[0];
  //pagePnl.Color := $20FFFF00; //try it :)
  pagePnl.ScrollH := pagePnl.AddChild(TScrollCtrl) as TScrollCtrl;
  pagePnl.ScrollV := pagePnl.AddChild(TScrollCtrl) as TScrollCtrl;

  with layeredImg32.AddLayer(TButtonCtrl, pagePnl) as TButtonCtrl do
  begin
    Padding := DPIAware(10);
    Caption := 'Click &Me';
    SetInnerBounds(DPIAware(RectD(40, 50, 150, 85)));
    BevelHeight := bevelSize;
    Onclick := epHandler.ClickMe;
  end;

  with layeredImg32.AddLayer(TRoundedBtnCtrl, pagePnl) as TButtonCtrl do
  begin
    Caption := 'Transparent';
    SetInnerBounds(DPIAware(RectD(40, 100, 150, 135)));
    //Color := clBtnFace32;
    BevelHeight := bevelSize;
    Onclick := epHandler.ClickBtn;
  end;

  with layeredImg32.AddLayer(TEllipseBtnCtrl, pagePnl) as TButtonCtrl do
  begin
    Caption := 'Opaque';
    SetInnerBounds(DPIAware(RectD(40, 150, 150, 190)));
    Color := clBtnFace32;
    BevelHeight := bevelSize;
    Onclick := epHandler.ClickBtn;
  end;

  with layeredImg32.AddLayer(TButtonCtrl, pagePnl) as TButtonCtrl do
  begin
    Caption := 'Disabled';
    SetInnerBounds(DPIAware(RectD(40, 205, 150, 240)));
    Enabled := false;
    BevelHeight := bevelSize;
  end;

  with layeredImg32.AddLayer(TCheckboxCtrl, pagePnl) as TCheckboxCtrl do
  begin
    Caption := 'Tri-state checkbox';
    SetInnerBounds(DPIAware(RectD(180, 50, 380, 70)));
    BevelHeight := bevelSize;
    AutoState := false; // handle state changes manually in OnClick events
    Onclick := epHandler.CheckboxClick;
  end;

  with layeredImg32.AddLayer(TRadioBtnCtrl, pagePnl) as TRadioBtnCtrl do
  begin
    Caption := 'Tri-state radiobutton';
    SetInnerBounds(DPIAware(RectD(180, 90, 380, 110)));
    BevelHeight := bevelSize;
  end;

  with layeredImg32.AddLayer(TSliderCtrl, pagePnl) as TSliderCtrl do
  begin
    Name := 'sliderCtrl2';
    Orientation := soHorizontal;
    SetInnerBounds(DPIAware(RectD(180, 140, 400, 160)));
    Min := 0; Max := 100;
    Position := 10; //set position first otherwise the
    //OnSlider event will try to find progressCtrl which
    //doesn't currently exist
    OnSlider := epHandler.Slider2Click;
    BevelHeight := bevelSize;
  end;

  with layeredImg32.AddLayer(TProgressCtrl, pagePnl) as TProgressCtrl do
  begin
    Orientation := soHorizontal;
    SetInnerBounds(DPIAware(RectD(180, 180, 400, 200)));
    EndColor := clRed32;
    Position := 10;
    BevelHeight := bevelSize;
  end;

  with layeredImg32.AddLayer(TEditCtrl, pagePnl) as TEditCtrl do
  begin
    SetInnerBounds(DPIAware(RectD(180, 220, 400, 250)));
    Text := 'Try editing me :).';
    BevelHeight := bevelSize;
  end;

  with layeredImg32.AddLayer(TListCtrl, pagePnl) as TListCtrl do
  begin
    SetInnerBounds(DPIAware(RectD(430, 50, 560, 160)));
    SetItems(['List Item 1','List Item 2','List Item 3','List Item 4',
      'List Item 5','List Item 6','List Item 7','List Item 8']);
    BevelHeight := bevelSize;
    MaxVisibleItems := 6;
    Margin := 0;//2;
    ImageList := epHandler.svgList2;
    ScrollV := AddChild(TScrollCtrl) as TScrollCtrl;
    ScrollV.Name := 'ListScroll';
  end;

  // PAGE 2 ///////////////////////////////////////////////////////

  pagePnl := pageCtrl.Panel[1];
  //pagePnl.Color := $20FF00FF;
  //pagePnl.Margin := DPIAware(50); //margin only works for autopositioned ctrls
  //add vertical and horizontal scrollbars
  pagePnl.ScrollH := pagePnl.AddChild(TScrollCtrl) as TScrollCtrl;
  pagePnl.ScrollV := pagePnl.AddChild(TScrollCtrl) as TScrollCtrl;
  //pagePnl.ScrollH.ScrollSize := DPIAware(16);

  with layeredImg32.AddLayer(TLabelCtrl, pagePnl,'') as TLabelCtrl do
  begin
    Caption := 'These buttons use SVG images because they are ideal for scaling.';
    SetInnerBounds(DPIAware(RectD(40, 40, 480, 62)));
  end;

  //we're about to add a whole number of image buttons
  //so we'll do this in a loop

  //prepare for button positioning
  pad := DPIAware(4);
  w := bevelSize * 2 + pad * 2 + imageSize64; //top row button width (bevel, padding & image)
  h := imageSize64 + pad * 3 + bevelSize * 2 + epHandler.arial14.LineHeight;  //button height
  j := DPIAware(40);    //initial button X offset
  k := DPIAware(80);    //initial button Y offset

  for i := 0 to epHandler.svgList.Count -1 do
    with layeredImg32.AddLayer(TImageBtnCtrl, pagePnl) as TImageBtnCtrl do
    begin
      //Font := arial;
      Caption := 'Btn' + Format('%d', [i + 1]);
      Padding := pad;
      BevelHeight := bevelSize;
      SetInnerBounds(RectD(j, k, j + w, k + h));
      if i mod 8 = 7 then
      begin
        j := DPIAware(40 * currentScale);
        k := k + h + OuterMargin;
        //change button width and height for second row
        w := epHandler.arial14.GetTextWidth('Btn13')
          +16 + bevelSize * 2 + pad * 3 + imageSize64;
        h := imageSize64 + pad * 2 +bevelSize * 2;
      end else
        j := j + w +OuterMargin;

      //alternate text positions
      if i > 7 then
      begin
        if Odd(i) then
          TextPosition := tpRight else
          TextPosition := tpLeft;
      end else
      begin
        if Odd(i) then
          TextPosition := tpBottom else
          TextPosition := tpTop;
      end;
      ImageList := epHandler.svgList;
      ImageListIdx := i;
      Onclick := epHandler.ClickBtn;
      //Color := clBtnFace32;
    end;

  // PAGE 3 ///////////////////////////////////////////////////////
  pagePnl := pageCtrl.Panel[2];
  //pagePnl.Color := $2000FFFF;
  pagePnl.Margin := DpiAware(10);
  pageCtrl.ActiveIndex := 0;

  with layeredImg32.AddLayer(TMemoCtrl, pagePnl) as TMemoCtrl do
  begin
    Text := GetUnicodeTextResource('LOREM', 'TEXT');
    BevelHeight := bevelSize;
    AutoPosition := apClient;
    ScrollV := AddChild(TScrollCtrl) as TScrollCtrl;
    ScrollV.Name := 'MemoScroll';
    OuterMargin := 40;
  end;
end;

//------------------------------------------------------------------------------
// Application entry
//------------------------------------------------------------------------------

const
  mainWindowWidth   = 720;
  mainWindowHeight  = 480;

begin

  // Setup and register the TWndClass record needed to create the main window
  Inst := hInstance;
  with WinClass do
  begin
    style              := CS_CLASSDC or CS_PARENTDC;
    lpfnWndProc        := @WindowProc;
    hInstance          := Inst;
    hbrBackground      := COLOR_BTNFACE + 1;
    lpszClassname      := 'IMG32_DEMO';
    hIcon              := LoadIcon(hInstance,'MAINICON');
    hCursor            := LoadCursor(0, IDC_ARROW);
  end;
  RegisterClass(WinClass);

  //Create the main window and center it
  mainHdl := CreateWindow('IMG32_DEMO', 'Demo',
              WS_OVERLAPPEDWINDOW, 0, 0,
              DpiAware(mainWindowWidth),
              DpiAware(mainWindowHeight), 0, 0, Inst, nil);

  CenterWindow(mainHdl);

  imageSize64 := DpiAware(64.0);
  imageSize24 := DpiAware(24.0);

  // fontReader - construction and destruction managed by FontManager
  fontReader  := FontManager.LoadFontReader('Arial');

  // instantiate handles to 'shared' cursors.
  // nb: these won't need destroying
  sizeCursor  := LoadCursor(0, IDC_SIZEALL);
  handCursor  := LoadCursor(0, IDC_HAND);
  arrowCursor := LoadCursor(0, IDC_ARROW);

  storageMngr := TCtrlStorageManager.Create;
  epHandler := storageMngr.InsertChild(0, TEventPropHandler) as TEventPropHandler;

  with epHandler do
  begin
    arial14 := TFontCache.Create(fontReader, DPIAware(14));
    UnscaledFont := TFontCache.Create(fontReader, DPIAware(14));
    //arial18 := TFontCache.Create(fontReader, DPIAware(18));

    svgList  := TSvgImageList32.Create;
    svgList.ResourceName := 'SVG';  //automatically loads resource
    svgList.DefaultWidth := Round(imageSize64);
    svgList.DefaultHeight := Round(imageSize64);

    svgList2  := TSvgImageList32.Create;
    svgList2.ResourceName := 'SVG'; //automatically loads resource
    svgList2.DefaultWidth := Round(imageSize24);
    svgList2.DefaultHeight := Round(imageSize24);

    // for TEventPropHandler to function properly, the
    // TEventPropHandler.RegisterPropertyEvents method
    // should be called after assigning its properties
    RegisterProperties;
  end;

  // assign global variables to automatically constructed objects
  layeredImg32 := storageMngr.LayeredImage;
  rootCtrl := storageMngr.RootCtrl;
  // nb: storageMngr's 'rootCtrl' object is a TPanelCtrl that is
  // owned by layeredImg32's 'root' (TGroupLayer32) layer.


  if DoLoadFromStorage then
  begin
    storageMngr.LoadFromFile('.\CtrlDemo.xml');
    // LoadFromFile() will delete the existing controls
    // contained by RootCtrl before loading all the new ones.

    pageCtrl := rootCtrl.FindChildByClass(TPageCtrl) as TPageCtrl;
    statusCtrl := rootCtrl.FindChildByClass(TStatusbarCtrl) as TStatusbarCtrl;

    // storageMngr.DesignScale may have changed so ...
    currentScale := storageMngr.DesignScale;
    DoScale(currentScale);
    ResizeMainWindow(mainHdl, layeredImg32.Width, layeredImg32.Height);
  end else
  begin
    SetupCtrls;
    storageMngr.FindAllShortcutOwners;
  end;

  ShowWindow(mainHdl, cmdShow);
  UpdateWindow(mainHdl);

  while GetMessage(Msg, 0, 0, 0) do
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;

  storageMngr.free;

end.
