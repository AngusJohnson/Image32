program CtrlDemo;

uses
  SysUtils,
  Classes,
  Windows,
  CommDlg,
  Types,
  Messages,
  TypInfo,
  Math,
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
  1. THIS APP USES IMAGE32 FOR **ALL** GUI CONTROLS ... IN OTHER WORDS -
  IT DOESN'T USE DELPHI'S VCL OR FMX FRAMEWORKS!
  2. THIS APP IS EXPERIMENTAL BECAUSE THE GUI CONTROLS ARE QUITE INCOMPLETE.
*)

{$R Lorem.res}
{$R SvgImages.res}
{$R ..\resources.res}

{$WARN SYMBOL_PLATFORM OFF}

type

  // TMyEventPropHandler - a custom event & property handler
  TMyEventPropHandler = class(TEventPropertyHandler)
  private
    fScaledFnt      : TFontCache;
    fUnscaledFnt : TFontCache;
    fSvgList      : TSvgImageList32;
    fSvgList2     : TSvgImageList32;
  public
    procedure Scale(delta: double); override;
    // events
    procedure LoadClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure SliderClick(Sender: TObject);
    procedure ClickMe(Sender: TObject);
    procedure ClickBtn(Sender: TObject);
    procedure CheckboxClick(Sender: TObject);
    procedure AutoSizeClick(Sender: TObject);
    procedure DesigningClick(Sender: TObject);
    procedure DarkModeClick(Sender: TObject);
    procedure TabClicked(Sender: TObject);
    // properties
    property scaledFont: TFontCache read fScaledFnt write fScaledFnt;
    property unscaledFont: TFontCache read fUnscaledFnt write fUnscaledFnt;
    property svgList64   : TSvgImageList32 read fSvgList write fSvgList;
    property svgList24  : TSvgImageList32 read fSvgList2 write fSvgList2;
  end;

var
  // storageMngr runs the show together with its event handler
  storageMngr : TCtrlStorageManager;
  // my event and property handler that will be created by storageMngr
  mepHandler  : TMyEventPropHandler;

  imageSize64     : double;
  imageSize24     : double;
//------------------------------------------------------------------------------
//Miscellaneous functions
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

function BoolEnabledToString(isEnabled: Boolean): string;
begin
  if isEnabled then Result := ' enabled' else Result := ' disabled';
end;

//------------------------------------------------------------------------------
// SetupCtrls - creates lots of GUI controls for the main form.
// (Controls can also be constructed from objects stored in an xml file.)
//------------------------------------------------------------------------------

procedure SetupCtrls;
var
  i, pad      : integer;
  j,k,h,w     : double;
  bevelSize   : double;
  pagePnl     : TPagePnlCtrl;
  layeredImg  : TLayeredImage32;
  rootCtrl    : TRootCtrl;
  pageCtrl    : TPageCtrl;
  statusCtrl  : TStatusbarCtrl;
begin
  layeredImg := storageMngr.LayeredImage;
  layeredImg.Width := DPIAware(700);
  layeredImg.Height := DPIAware(370);

  rootCtrl   := storageMngr.RootCtrl;
  //rootCtrl.ClearChildren;
  bevelSize := DPIAware(2);

  // rootCtrl is the container for all other controls
  // and it was created automatically by storageMngr.
  rootCtrl.Margin := DPIAware(10);
  rootCtrl.Font := mepHandler.unscaledFont;
  rootCtrl.Isthemed := true;
  rootCtrl.theme := lightTheme;

  // note: when objects are created, if they have events
  // then these are assigned here too.

  // add main menus
  with rootCtrl.InsertChild(0, TMainMenuCtrl) as TMainMenuCtrl do
  begin
    AllowRTEdit := false;
    with AddChild(TMenuItemCtrl, '&File') as TMenuItemCtrl do
    begin
      with AddChild(TPopMenuCtrl, 'File Popup') as TPopMenuCtrl do
      begin
        with AddChild(TMenuItemCtrl, '&Load ...') as TMenuItemCtrl do
          OnClick := mepHandler.LoadClick;
        with AddChild(TMenuItemCtrl, '&Save ...') as TMenuItemCtrl do
          OnClick := mepHandler.SaveClick;
        AddChild(TMenuItemCtrl, '-');
        with AddChild(TMenuItemCtrl, 'E&xit') as TMenuItemCtrl do
          OnClick := mepHandler.ExitClick;
      end;
    end;
    with AddChild(TMenuItemCtrl, '&Options') as TMenuItemCtrl do
    begin
      with AddChild(TPopMenuCtrl, 'Options Popup') as TPopMenuCtrl do
      begin
        with AddChild(TMenuItemCtrl, '&AutoSizing') as TMenuItemCtrl do
        begin
          MenuItemType := TMenuItemType.mitCheckbox;
          OnClick := mepHandler.AutoSizeClick;
        end;
        with AddChild(TMenuItemCtrl, '&Designing') as TMenuItemCtrl do
        begin
          MenuItemType := TMenuItemType.mitCheckbox;
          OnClick := mepHandler.DesigningClick;
        end;
        with AddChild(TMenuItemCtrl, 'Dar&k Mode') as TMenuItemCtrl do
        begin
          MenuItemType := TMenuItemType.mitCheckbox;
          OnClick := mepHandler.DarkModeClick;
        end;
      end;
    end;
  end;

  // add a status bar at the bottom
  statusCtrl := layeredImg.AddLayer(TStatusbarCtrl, rootCtrl) as TStatusbarCtrl;
  with statusCtrl do
  begin
    BevelHeight := DPIAware(1.5);
    Color := clNone32;
    AutoPosition := apBottom;
    Font := mepHandler.scaledFont;
    Caption := 'This demo doesn''t use either VCL or FMX frameworks, just Image32 !';
  end;

  // add a page control with 3 tabs
  pageCtrl := layeredImg.AddLayer(TPageCtrl, rootCtrl) as TPageCtrl;
  pageCtrl.AutoPosition := apClient;
  pageCtrl.BevelHeight := DPIAware(3);
  pageCtrl.AllowRTEdit := True;
  pageCtrl.Font := mepHandler.scaledFont;
  pageCtrl.AddTabs(['Page &1', 'Page &2', 'Page &3']);
  pageCtrl.OnClick := mepHandler.TabClicked;

  // add a variety of ctrls to each page

  // PAGE 1 ///////////////////////////////////////////////////////
  pagePnl := pageCtrl.Panel[0];
  with pagePnl.AddChild(TScrollCtrl) as TScrollCtrl do
    Orientation := soHorizontal;
  with pagePnl.AddChild(TScrollCtrl) as TScrollCtrl do
    Orientation := soVertical;

  with layeredImg.AddLayer(TButtonCtrl, pagePnl) as TButtonCtrl do
  begin
    Padding := DPIAware(10);
    Caption := 'Click &Me';
    SetInnerBounds(DPIAware(RectD(40, 50, 150, 85)));
    BevelHeight := bevelSize;
    Onclick := mepHandler.ClickMe;
  end;

  with layeredImg.AddLayer(TRoundedBtnCtrl, pagePnl) as TButtonCtrl do
  begin
    Caption := 'Transparent';
    Isthemed := false;
    Color := clNone32;
    SetInnerBounds(DPIAware(RectD(40, 100, 150, 135)));
    BevelHeight := bevelSize;
    Onclick := mepHandler.ClickBtn;
  end;

  with layeredImg.AddLayer(TEllipseBtnCtrl, pagePnl) as TButtonCtrl do
  begin
    Caption := 'Opaque';
    SetInnerBounds(DPIAware(RectD(40, 150, 150, 190)));
    BevelHeight := bevelSize;
    Onclick := mepHandler.ClickBtn;
  end;

  with layeredImg.AddLayer(TButtonCtrl, pagePnl) as TButtonCtrl do
  begin
    Caption := 'Disabled';
    SetInnerBounds(DPIAware(RectD(40, 205, 150, 240)));
    Enabled := false;
    BevelHeight := bevelSize;
  end;

  with layeredImg.AddLayer(TCheckboxCtrl, pagePnl) as TCheckboxCtrl do
  begin
    Caption := 'Tri-state &checkbox';
    SetInnerBounds(DPIAware(RectD(180, 50, 380, 70)));
    BevelHeight := bevelSize;
    AutoState := false; // handle state changes manually in OnClick events
    Onclick := mepHandler.CheckboxClick;
  end;

  with layeredImg.AddLayer(TRadioBtnCtrl, pagePnl) as TRadioBtnCtrl do
  begin
    Caption := 'Tri-state &radiobutton';
    SetInnerBounds(DPIAware(RectD(180, 90, 380, 110)));
    BevelHeight := bevelSize;
  end;

  with layeredImg.AddLayer(TSliderCtrl, pagePnl) as TSliderCtrl do
  begin
    Orientation := soHorizontal;
    SetInnerBounds(DPIAware(RectD(180, 140, 490, 160)));
    Min := 0; Max := 100;
    Position := 10; //set position first otherwise the
    //OnSlider event will try to find progressCtrl which
    //doesn't currently exist
    OnSlide := mepHandler.SliderClick;
    BevelHeight := bevelSize;
  end;

  with layeredImg.AddLayer(TProgressCtrl, pagePnl,
    'ProgressCtrl1') as TProgressCtrl do // (named so it can be found easily)
  begin
    Orientation := soHorizontal;
    SetInnerBounds(DPIAware(RectD(180, 180, 490, 200)));
    EndColor := clRed32;
    Position := 10;
    BevelHeight := bevelSize;
  end;

  with layeredImg.AddLayer(TEditCtrl, pagePnl) as TEditCtrl do
  begin
    SetInnerBounds(DPIAware(RectD(180, 220, 650, 250)));
    Text := 'Try editing me :).';
    BevelHeight := bevelSize;
  end;

  with pagePnl.AddChild(TListCtrl, 'ListCtrl') as TListCtrl do
  begin
    SetInnerBounds(DPIAware(RectD(520, 50, 650, 178)));
    for i := 1 to 8 do
      AddChild(TListItem, 'List item ' + IntToStr(i));
    BevelHeight := bevelSize;
    ImageList := mepHandler.svgList24;
    with AddChild(TScrollCtrl, 'ListScroll') as TScrollCtrl do
      Orientation := soVertical;
  end;

  // PAGE 2 ///////////////////////////////////////////////////////

  pagePnl := pageCtrl.Panel[1];
  //add vertical and horizontal scrollbars
  with pagePnl.AddChild(TScrollCtrl) as TScrollCtrl do
    Orientation := soHorizontal;
  with pagePnl.AddChild(TScrollCtrl) as TScrollCtrl do
    Orientation := soVertical;

  with layeredImg.AddLayer(TLabelCtrl, pagePnl,'') as TLabelCtrl do
  begin
    Caption := 'These buttons use SVG images because they are ideal for scaling.';
    SetInnerBounds(DPIAware(RectD(40, 40, 480, 62)));
  end;

  //we're about to add a whole number of image buttons
  //so we'll do this in a loop

  //prepare for button positioning
  pad := DPIAware(2);
  w := bevelSize * 2 + pad * 2 + imageSize64; //top row button width (bevel, padding & image)
  h := imageSize64 + pad * 3 + bevelSize * 2 + mepHandler.scaledFont.LineHeight;  //button height
  j := DPIAware(40);    //initial button X offset
  k := DPIAware(80);    //initial button Y offset

  for i := 0 to mepHandler.svgList64.Count -1 do
    with layeredImg.AddLayer(TImageBtnCtrl, pagePnl) as TImageBtnCtrl do
    begin
      Caption := 'Btn' + Format('%d', [i + 1]);
      Padding := pad;
      BevelHeight := bevelSize;
      SetInnerBounds(RectD(j, k, j + w, k + h));
      if i mod 8 = 7 then
      begin
        j := DPIAware(40 * StorageMngr.DesignScale);
        k := k + h + OuterMargin * 2;
        //change button width and height for second row
        w := mepHandler.scaledFont.GetTextWidth('Btn13')
          +16 + bevelSize * 2 + pad * 3 + imageSize64;
        h := imageSize64 + pad * 2 +bevelSize * 2;
      end else
        j := j + w +OuterMargin;

      //alternate the text's position ...
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
      ImageList := mepHandler.svgList64;
      ImageIdx := i;
      Onclick := mepHandler.ClickBtn;
      Color := clNone32;
    end;

  // PAGE 3 ///////////////////////////////////////////////////////
  pagePnl := pageCtrl.Panel[2];
  pagePnl.Margin := DpiAware(10);
  pageCtrl.ActiveIndex := 0;

  with layeredImg.AddLayer(TMemoCtrl, pagePnl) as TMemoCtrl do
  begin
    OuterMargin := 40;
    Text := GetUnicodeTextResource('LOREM', 'TEXT');
    BevelHeight := bevelSize;
    AutoPosition := apClient;
    with AddChild(TScrollCtrl, 'MemoScroll') as TScrollCtrl do
      Orientation := soVertical;
  end;

  // Finally, resize the main window to the new control layout ...
  if storageMngr.MainHdl > 0 then
    ResizeMainWindow(storageMngr.MainHdl, layeredImg.Width, layeredImg.Height);
end;

//------------------------------------------------------------------------------
// TEventPropertyHandler1 handles all custom (ie user defined) events
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.Scale(delta: double);
var
  designScale : double;
begin
  if ValueAlmostOne(delta) then Exit;

  if delta = RESET then
  begin
    with mepHandler do
    begin
      unscaledFont.FontHeight := DpiAware(14);
      scaledFont.FontHeight := DpiAware(14);
      imageSize64 := DpiAware(64.0);
      imageSize24 := DpiAware(24.0);
    end;
  end else
  begin
    if storageMngr.DesignResolution > 0 then
      designScale := storageMngr.DesignResolution * storageMngr.DesignScale else
      designScale := storageMngr.DesignScale;
    // scale the font and image sizes in my TEventPropertyHandler ...
    with mepHandler do
    begin
      scaledFont.FontHeight := scaledFont.FontHeight * delta;
      imageSize64 := imageSize64 * delta;
      imageSize24 := imageSize24 * delta;
    end;
  end;
  svgList64.DefaultWidth := Round(imageSize64);
  svgList64.DefaultHeight := Round(imageSize64);
  svgList24.DefaultWidth := Round(imageSize24);
  svgList24.DefaultHeight := Round(imageSize24);
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.LoadClick(Sender: TObject);
var
  filename: string;
begin
  filename := '.\*.xml';
  if not OpenSaveXmlDialog(filename, false) then Exit;
  storageMngr.LoadFromFile(filename);
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.SaveClick(Sender: TObject);
var
  filename: string;
begin
  filename := '.\*.xml';
  if not OpenSaveXmlDialog(filename, true) then Exit;
  storageMngr.SaveToFile(filename, StorageMngr.DesignScale);
  with storageMngr.RootCtrl.FindChildByClass(TStatusbarCtrl) as TStatusbarCtrl do
    Caption := 'Controls saved';
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.ExitClick(Sender: TObject);
begin
  storageMngr.Quit;
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.SliderClick(Sender: TObject);
var
  senderPos: double;
begin
  senderPos := (Sender as TSliderCtrl).Position;
  with storageMngr.RootCtrl.FindChildByName('ProgressCtrl1') as TProgressCtrl do
    Position := senderPos;
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.ClickMe(Sender: TObject);
begin
  windows.MessageBox(0, '''Click Me'' button clicked!', 'CtrlDemo', 0);
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.ClickBtn(Sender: TObject);
begin
  with storageMngr.RootCtrl.FindChildByClass(TStatusbarCtrl) as TStatusbarCtrl do
    Caption := TButtonCtrl(Sender).Caption + ' clicked';
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.CheckboxClick(Sender: TObject);
begin
  with (Sender as TCheckboxCtrl) do
    case State of
      tsUnknown : State := tsNo;
      tsYes     : State := tsUnknown;
      tsNo      : State := tsYes;
    end;
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.DesigningClick(Sender: TObject);
begin
  storageMngr.Designing := TMenuItemCtrl(Sender).Checked;
  with storageMngr.RootCtrl.FindChildByClass(TStatusbarCtrl) as TStatusbarCtrl do
    Caption := 'Designing' + BoolEnabledToString(storageMngr.Designing);
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.AutoSizeClick(Sender: TObject);
begin
  storageMngr.AllowResize := TMenuItemCtrl(Sender).Checked;
  with storageMngr.RootCtrl.FindChildByClass(TStatusbarCtrl) as TStatusbarCtrl do
    Caption := 'Auto-sizing' + BoolEnabledToString(storageMngr.AllowResize);
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.DarkModeClick(Sender: TObject);
begin
  if storageMngr.RootCtrl.Theme.Color = darkTheme.Color then
    storageMngr.RootCtrl.Theme := lightTheme else
    storageMngr.RootCtrl.Theme := darkTheme;
  InvalidateRect(storageMngr.MainHdl, nil, true);
  with storageMngr.RootCtrl.FindChildByClass(TStatusbarCtrl) as TStatusbarCtrl do
    Caption := '';
end;
//------------------------------------------------------------------------------

procedure TMyEventPropHandler.TabClicked(Sender: TObject);
begin
  with storageMngr.RootCtrl.FindChildByClass(TStatusbarCtrl) as TStatusbarCtrl do
    Caption := '';
end;

//------------------------------------------------------------------------------
// Application entry
//------------------------------------------------------------------------------

var
  Msg: TMsg;
  tmpFontReader: TFontReader;
const
  DoLoadFromStorage = false; //true;//
begin

  // CREATE **STORAGE MANAGER** THAT CONTROLS ALMOST EVERYTHING,
  storageMngr := TCtrlStorageManager.Create;
  // AND ALSO CREATE THE CUSTOM EVENT AND PROPERTY HANDLER ...
  mepHandler := storageMngr.InsertChild(0, TMyEventPropHandler) as TMyEventPropHandler;
  // AND, BEFORE LOADING A WHOLE HOST OF CONTROLS, CREATE THE MAIN FORM ...
  storageMngr.CreateMainWindow('IMG32_DEMO', 'CtrlDemo', 'MAINICON');

  // Assign mepHandler properties here. (mepHandler's events will be
  // linked to controls as controls are created.)
  with mepHandler do
  begin
    // tmpFontReader is needed to create a couple of font caches
    // used by Storage Manager's event handler.
    // (tmpFontReader will be destroyed later by FontManager.)
    tmpFontReader  := FontManager.LoadFontReader('Arial');
    scaledFont := TFontCache.Create(tmpFontReader, DPIAware(14));
    unscaledFont := TFontCache.Create(tmpFontReader, DPIAware(14));

    svgList64  := TSvgImageList32.Create;
    svgList64.ResourceName := 'SVG';  //automatically loads resource
    svgList24  := TSvgImageList32.Create;
    svgList24.ResourceName := 'SVG'; //automatically loads resource

    // these properties also have to be registered after assignment
    RegisterProperties;
    Scale(RESET);
  end;

  // either create controls using object definitions in an xml file
  // or create controls using compiled code
  if DoLoadFromStorage then
    storageMngr.LoadFromFile('.\CtrlDemo.xml') else
    SetupCtrls;

  ShowWindow(storageMngr.MainHdl, cmdShow);
  UpdateWindow(storageMngr.MainHdl);

  // Perform the Windows message loop
  while GetMessage(Msg, 0, 0, 0) do
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;

  // Clean-up
  if storageMngr.MainHdl > 0 then
    DestroyWindow(storageMngr.MainHdl);
  storageMngr.free;

end.
