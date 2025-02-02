unit DialogsEx;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.8                                                             *
* Date      :  12 Febuary 2020                                                 *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Windows styled dialog boxes for messages, text input etc        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*                                                                              *
* Very loosely based on Andreas Rejbrand's code - http://english.rejbrand.se   *
*                                                                              *
*******************************************************************************)

interface

uses
  Windows, SysUtils, Types, Controls, Graphics, Forms,
  StdCtrls, ExtCtrls, CommCtrl;

type
  TAllowOnlyOption = (aoCapitalAZ, aoSmallAZ, aoAZ, aoLetters, aoDigits,
    aoSpace, aoPeriod, aoComma, aoSemicolon, aoHyphenMinus, aoPlus,
    aoUnderscore, aoAsterisk);
  TAllowOnlyOptions = set of TAllowOnlyOption;
  TNotifyEditChangedFunc =
    procedure (Sender: TObject; const S: string; var IsValid: Boolean) of object;
  TNotifyCheckboxChangedFunc =
    procedure (Sender: TObject; isChecked: Boolean) of object;

  TDialogPosition = (
    dpOwnerFormCenter, dpScreenCenter, dpDesktopCenter, dpMainFormCenter,
    dpRecTopLeft, dpRecTopRight, dpRecBottomLeft, dpRecBottomRight);


  IOptions = interface(IInterface)
    function AsObject: TInterfacedObject;
    function Icon: TIcon;
    procedure SetIcon(ico: TIcon);
    function ButtonCaps: TStringDynArray;
    procedure AddButtonCaption(const caption: string);
  end;

  TOptions = class(TInterfacedObject, IOptions)
  protected
    ico : TIcon;
    btnCaps : TStringDynArray;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AsObject: TInterfacedObject;
    function Icon: TIcon;
    procedure SetIcon(ico: TIcon);
    function ButtonCaps: TStringDynArray;
    procedure AddButtonCaption(const caption: string);
  end;

  TMessageBoxOptions = class(TOptions)
  public
    CheckBoxCallBk : TNotifyCheckboxChangedFunc;
  end;

  TTextBoxOptions = class(TOptions)
  public
    charCase         : TEditCharCase;
    allowEmptyString : Boolean;
    allowOnlyOptions : TAllowOnlyOptions;
    editChangeCallBk : TNotifyEditChangedFunc;
  end;

  TComboboxOptions = class(TOptions)
  private
    comboItems       : TStringDynArray;
  public
    editChangeCallBk : TNotifyEditChangedFunc;
    procedure AddComboboxItem(const item: string);
  end;

  TNumBoxOptions = class(TOptions)
  public
    minVal           : integer;
    maxVal           : integer;
    allowEmptyString : Boolean;
    allowOnlyOptions : TAllowOnlyOptions;
    editChangeCallBk : TNotifyEditChangedFunc;
  end;

  THexBoxOptions = class(TOptions)
  public
    maxLen           : integer;
    allowEmptyString : Boolean;
    allowOnlyOptions : TAllowOnlyOptions;
    editChangeCallBk : TNotifyEditChangedFunc;
  end;

  TFloatBoxOptions = class(TOptions)
  public
    minVal           : Double;
    maxVal           : Double;
    allowEmptyString : Boolean;
    allowOnlyOptions : TAllowOnlyOptions;
    editChangeCallBk : TNotifyEditChangedFunc;
  end;

  //MessageBox:
  //  style example: MB_YESNOCANCEL or MB_ICONQUESTION or MB_DEFBUTTON3
  //  note the MB_HELP style option is not supported.

  function MessageBox(AOwner: TCustomForm; const ABigText, ASmallText,
    ATitle: string; style: Cardinal): TModalResult; overload;
  function MessageBox(AOwner: TCustomForm;
    const ABigText, ASmallText, ATitle: string; style: Cardinal;
    const options: TMessageBoxOptions): TModalResult; overload;

  function TextInputBox(AOwner: TCustomForm; const ABigText, ASmallText,
    ATitle: string; var Value: string): boolean; overload;

  function TextInputBox(AOwner: TCustomForm;
    const ABigText, ASmallText, ATitle: string; var Value: string;
    const options: TTextBoxOptions): boolean; overload;

  function ComboInputBox(AOwner: TCustomForm;
    const ABigText, ASmallText, ATitle: string; var index: integer;
    const options: TComboBoxOptions): boolean;

  function NumInputBox(AOwner: TCustomForm; const ABigText, ASmallText,
    ATitle: string; var Value: integer): boolean; overload;

  function NumInputBox(AOwner: TCustomForm;
    const ABigText, ASmallText, ATitle: string; var Value: integer;
    const options: TNumBoxOptions): boolean; overload;

  function HexInputBox(AOwner: TCustomForm;
    const ABigText, ASmallText, ATitle: string; var hex: string;
    const options: THexBoxOptions): boolean;

  function FloatInputBox(AOwner: TCustomForm; const ABigText, ASmallText,
    ATitle: string; var Value: double): boolean; overload;
  function FloatInputBox(AOwner: TCustomForm;
    const ABigText, ASmallText, ATitle: string; var Value: double;
    const options: TFloatBoxOptions): boolean; overload;

var
  SoundBeepOnStop : Boolean = true;
  smallFontSize   : integer = 10;
  bigFontSize     : integer = 16;

const
  mrTryAgain = IDTRYAGAIN;
  mrContinue = IDCONTINUE;

  MB_CUSTOM2BUTTONS = $D; //2 button custom message dialog
  MB_CUSTOM3BUTTONS = $E; //3 button custom message dialog
  MB_CUSTOM4BUTTONS = $F; //4 button custom message dialog

  mrButton1 = 12;
  mrButton2 = 13;
  mrButton3 = 14;
  mrButton4 = 15;

{$R *.res}

implementation

uses Math, Messages;

resourcestring
  rsYes        = '&Yes';
  rsNo         = '&No';
  rsOK         = '&OK';
  rsCancel     = '&Cancel';
  rsAbort      = '&Abort';
  rsIgnore     = '&Ignore';
  rsRetry      = '&Retry';
  rsTryAgain   = '&Try again';
  rsContinue   = 'Co&ntinue';
  rsCheckbox   = '&Don''t display this message again.';
  rsInitFail   = 'CustomDialogs: Options object must be initialized.';
  rsExcessCaps = 'CustomDialogs: Too many custom captions.';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

constructor TOptions.Create;
begin
  ico := TIcon.Create;
end;
//------------------------------------------------------------------------------

destructor TOptions.Destroy;
begin
  ico.Free;
end;
//------------------------------------------------------------------------------

function TOptions.AsObject: TInterfacedObject;
begin
  result := self;
end;
//------------------------------------------------------------------------------

function TOptions.Icon: TIcon;
begin
  result := ico;
end;
//------------------------------------------------------------------------------

procedure TOptions.SetIcon(ico: TIcon);
begin
  self.ico := ico;
end;
//------------------------------------------------------------------------------

function TOptions.ButtonCaps: TStringDynArray;
begin
  result := btnCaps;
end;
//------------------------------------------------------------------------------

procedure TOptions.AddButtonCaption(const caption: string);
var
  i: integer;
begin
  i := Length(btnCaps);
  if i > 3 then Exit;
  SetLength(btnCaps, i+1);
  btnCaps[i] := caption;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TComboboxOptions.AddComboboxItem(const item: string);
var
  i: integer;
begin
  i := length(comboItems);
  SetLength(comboItems, i +1);
  comboItems[i] := item;
end;
//------------------------------------------------------------------------------

type
  TDialogType = (dtMessage, dtEdit, dtCombo);
  TFourButtons = array [0..3] of TButton;

  TCustomDialog = class
  private
    fForm: TForm;
    fEdit: TEdit;
    fCombo: TComboBox;
    fButtons: TFourButtons;
    fIconImage: TImage;
    fCheckbox: TCheckBox;
    fTitle, fBigText, fSmallText: string;
    fMinInt, fMaxInt: integer;
    fMinFloat, fMaxFloat: double;
    fAllowEmptyString: boolean;
    fAllowOnlyOptions: TAllowOnlyOptions;
    fEditCallbackFunc: TNotifyEditChangedFunc;
    fSpinHandle: HWND;
    fHorzLineYPos: integer;
    fBigTextRect,
    fSmallTextRect: TRect;
    procedure Paint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SetupDialog(style: cardinal;
      dialogType: TDialogType; options: IOptions; cbVisible : Boolean);
    procedure ValidateIntInput(Sender: TObject);
    procedure ValidateHexInput(Sender: TObject);
    procedure ValidateRealInput(Sender: TObject);
    procedure ValidateStrInput(Sender: TObject);
    function DoEditCallback(Sender: TObject): Boolean;
  public
    function MessageBox(AOwner: TCustomForm;
      const ABigText, ASmallText, ATitle: string; style: Cardinal;
      const options: TMessageBoxOptions): Cardinal;

    function CharInputBox(AOwner: TCustomForm;
      const ABigText, ASmallText, ATitle: string; var Value: char;
      const options: TTextBoxOptions): boolean;

    function TextInputBox(AOwner: TCustomForm;
      const ABigText, ASmallText, ATitle: string;
      var Value: string; const options: TTextBoxOptions): boolean;

    function ComboInputBox(AOwner: TCustomForm;
      const ABigText, ASmallText, ATitle: string;
      var index: integer; const options: TComboBoxOptions): boolean;

    function NumInputBox(AOwner: TCustomForm;
      const ABigText, ASmallText, ATitle: string; var Value: integer;
      const options: TNumBoxOptions): boolean;

    function HexInputBox(AOwner: TCustomForm;
      const ABigText, ASmallText, ATitle: string; var hex: string;
      const options: THexBoxOptions): boolean;

    function FloatInputBox(AOwner: TCustomForm;
      const ABigText, ASmallText, ATitle: string; var Value: double;
      const options: TFloatBoxOptions): boolean;

    property Buttons: TFourButtons read fButtons;
    property Checkbox: TCheckBox read fCheckbox;
    property Combobox: TComboBox read fCombo;
    property Edit: TEdit read fEdit;
    property Image: TImage read fIconImage;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function DPI(value: integer): integer;
begin
  result := Screen.PixelsPerInch * value div 96;
end;
//------------------------------------------------------------------------------

procedure TCustomDialog.Paint(Sender: TObject);
begin
  with fForm.Canvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Color := $00DFDFDF;
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, fForm.ClientWidth, fHorzLineYPos));
    MoveTo(0, fHorzLineYPos);
    LineTo(fForm.ClientWidth, fHorzLineYPos);
    if fBigText <> '' then
    begin
      Font.Size := bigFontSize;
      Font.Color := $00993300;
      DrawText(Handle, PChar(fBigText), Length(fBigText),
        fBigTextRect, DT_NOPREFIX or DT_WORDBREAK);
    end;
    if fSmallText <> '' then
    begin
      Font.Size := smallFontSize;
      Font.Color := $00000000;
      DrawText(Handle, PChar(fSmallText), Length(fSmallText),
        fSmallTextRect, DT_NOPREFIX or DT_WORDBREAK);
    end;
  end;
end;
//------------------------------------------------------------------------------

type
  TArrayOfInteger = array of integer;

procedure TCustomDialog.SetupDialog(style: cardinal;
  dialogType: TDialogType; options: IOptions; cbVisible : Boolean);
var
  i, j, len, iconSize, padding, clientWidth, tmpClientWidth: integer;
  visibleBtns, customBtnCaptionCnt: integer;
  dialogMargin, dpi7, dpi11, dpi20, dpi26, dpi75: integer;
  button: TButton;
  showIcon: Boolean;
  size: TSize;
const
  BUTTONMASK = $F;
  ICONMASK = $70;
  DEFBUTTONMASK = $300;
  MB_CANCELTRYCONTINUE = $6;
begin
  // https://docs.microsoft.com/en-us/windows/win32/uxguide/vis-layout
  // https://docs.microsoft.com/en-us/windows/win32/uxguide/vis-layout#recommended-sizing-and-spacing

  dpi7 := DPI(7); dpi11 := DPI(11); dpi20 := DPI(20);
  dpi26 := DPI(26); dpi75 := DPI(75);
  dialogMargin := dpi7 *2;
  tmpClientWidth := DPI(400);

  fForm.Font.Name := 'Segoe UI';
  fForm.Font.Size := smallFontSize;
  fForm.Canvas.Font.Assign(fForm.Font);
  fForm.Caption := fTitle;
  fForm.Position := poOwnerFormCenter;
  fForm.BorderStyle := bsDialog;
  fForm.FormStyle := fsStayOnTop;
  fForm.OnPaint := Paint;
  fForm.OnActivate := FormActivate;

  iconSize := 0;
  padding := dpi11;
  showIcon := Assigned(options.Icon) or (style and ICONMASK <> 0);

  // Icons //////////////////////////////////////////////
  if showIcon then
  begin
    iconSize := DPI(32);
    if iconSize >= 48 then iconSize := 48
    else if iconSize > 32 then iconSize := 40;

    fIconImage := TImage.Create(fForm);
    fIconImage.Parent := fForm;
    fIconImage.Left := dialogMargin;
    fIconImage.Top := dpi11;

    //CustomDialog.res contains Windows 10 style icons
    if assigned(options.Icon) then
      fIconImage.Picture.Icon.Assign(options.Icon)
    else if style and ICONMASK = MB_ICONINFORMATION then //aka MB_ICONASTERISK
      //fIconImage.Picture.Icon.Handle := LoadIcon(0, IDI_INFORMATION)
      fIconImage.Picture.Icon.Handle := LoadImage(hInstance,
        'XICON_INFO', IMAGE_ICON, iconSize, iconSize, 0)
    else if (style and ICONMASK = MB_ICONWARNING) then  //aka MB_ICONEXCLAMATION
      //fIconImage.Picture.Icon.Handle := LoadIcon(0, IDI_EXCLAMATION)
      fIconImage.Picture.Icon.Handle := LoadImage(hInstance,
        'XICON_WARNING', IMAGE_ICON, iconSize, iconSize, 0)
    else if style and ICONMASK = MB_ICONQUESTION then
      //fIconImage.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION)
      fIconImage.Picture.Icon.Handle := LoadImage(hInstance,
        'XICON_QUERY', IMAGE_ICON, iconSize, iconSize, 0)
    else                                //MB_ICONSTOP, MB_ICONERROR, MB_ICONHAND
      //fIconImage.Picture.Icon.Handle := LoadIcon(0, IDI_ERROR);
      fIconImage.Picture.Icon.Handle := LoadImage(hInstance,
        'XICON_ERROR', IMAGE_ICON, iconSize, iconSize, 0);

    if SoundBeepOnStop and (style and ICONMASK = MB_ICONSTOP) then Beep;
  end;

  // BigText //////////////////////////////////////////////
  fBigTextRect := Rect(0, 0, 0, 0);
  if fBigText <> '' then
  begin
    fBigTextRect := Rect(dialogMargin,
      dpi11, tmpClientWidth - dialogMargin, dpi11 + 2);
    if iconSize > 0 then
      inc(fBigTextRect.Left, iconSize + padding);
    fForm.Canvas.Font.Size := bigFontSize;
    DrawText(fForm.Canvas.Handle, PChar(fBigText), Length(fBigText),
      fBigTextRect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);

    GetTextExtentPoint(fForm.Canvas.Handle, 'Yy', 2, size);
    if (fBigTextRect.Bottom - fBigTextRect.Top) > size.cy then
    begin
      fBigTextRect.Right := Round(tmpClientWidth * 1.4) - dialogMargin;
      DrawText(fForm.Canvas.Handle, PChar(fBigText), Length(fBigText),
        fBigTextRect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);
    end;

    if iconSize > 0 then
    begin
      len := iconSize + dpi11 - fBigTextRect.Bottom;
      if len > 0 then
      begin
        inc(fBigTextRect.Top, len div 2);
        inc(fBigTextRect.Bottom, len);
      end;
      iconSize := 0;
    end;
  end;

  // SmallText ////////////////////////////////////////////
  fSmallTextRect := Rect(0, 0, 0, fBigTextRect.Bottom);
  fForm.Canvas.Font.Size := smallFontSize;
  if fSmallText <> '' then
  begin
    fSmallTextRect := Rect(dialogMargin, fBigTextRect.Bottom + dpi11,
      tmpClientWidth - dialogMargin, dpi11 +2);
    if iconSize > 0 then
      inc(fSmallTextRect.Left, iconSize + padding);

    DrawText(fForm.Canvas.Handle, PChar(fSmallText), Length(fSmallText),
      fSmallTextRect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);

    GetTextExtentPoint(fForm.Canvas.Handle, 'Yy', 2, size);
    if (fSmallTextRect.Bottom - fSmallTextRect.Top) > (size.cy * 3) then
    begin
      fSmallTextRect.Right := Round(tmpClientWidth * 1.4) - dialogMargin;
      DrawText(fForm.Canvas.Handle, PChar(fSmallText), Length(fSmallText),
        fSmallTextRect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);
    end;


    if iconSize > 0 then
    begin
      len := iconSize + dpi11 - fSmallTextRect.Bottom;
      if len > 0 then
      begin
        inc(fSmallTextRect.Top, len div 2);
        inc(fSmallTextRect.Bottom, len);
      end;
    end;
  end;

  // Create Buttons and Edit Controls //////////////////////

  fEdit := TEdit.Create(fForm);
  fEdit.Parent := fForm;
  fEdit.Top := fSmallTextRect.Bottom + dpi11;
  fEdit.Left := dialogMargin;

  fCombo := TComboBox.Create(fForm);
  fCombo.Parent := fForm;
  fCombo.Style := csDropDownList;
  fCombo.Top := fSmallTextRect.Bottom + dpi11;
  fCombo.Left := dialogMargin;

  case dialogType of
    dtMessage:
      begin
        fEdit.Visible := false;
        fCombo.Visible := false;
        fHorzLineYPos := fSmallTextRect.Bottom + dpi11 *2;
      end;
    dtEdit:
      begin
        fCombo.Visible := false;
        fHorzLineYPos := fEdit.Top + fEdit.Height  + dialogMargin;
        fForm.ActiveControl := fEdit;
      end;
    dtCombo:
      begin
        fEdit.Visible := false;
        fHorzLineYPos := fEdit.Top + fEdit.Height  + dialogMargin;
        fForm.ActiveControl := fCombo;
      end;
  end;

  fCheckbox := TCheckBox.Create(fForm);
  fCheckbox.Parent := fForm;
  fCheckbox.Visible := (dialogType = dtMessage) and cbVisible;

  if fCheckbox.Visible then
  begin
    fCheckbox.Top := fSmallTextRect.Bottom + dpi11;
    fCheckbox.Left := dialogMargin;
    fCheckbox.Height := fEdit.Height;
    fCheckbox.Caption := rsCheckbox;
    if fCheckbox.Visible then
      fHorzLineYPos := fCheckbox.Top + fCheckbox.Height  + dialogMargin div 2;
  end;

  for i := 3 downto 0 do
  begin
    fButtons[i] := TButton.Create(fForm);
    fButtons[i].Parent := fForm;
    fButtons[i].Top := fHorzLineYPos + 1{*} + dpi11;
    fButtons[i].Height := dpi26;
    fButtons[i].Width := dpi75;
    fButtons[i].Visible := false;
  end;
  fButtons[0].Cancel := true;
  fButtons[0].Visible := true;

  customBtnCaptionCnt := Length(options.ButtonCaps);
  if (dialogType <> dtMessage) and (customBtnCaptionCnt > 2) then
  begin
    if customBtnCaptionCnt = 3 then
      style := (style and not BUTTONMASK) or MB_CUSTOM3BUTTONS else
      style := (style and not BUTTONMASK) or MB_CUSTOM4BUTTONS;
  end;

  case style and BUTTONMASK of
    MB_OK:
      begin
        fButtons[0].Caption := rsOK;
        fButtons[0].ModalResult := mrOk;
        fButtons[0].Default := true;
      end;
    MB_OKCANCEL:
      begin
        fButtons[1].Caption := rsOK;
        fButtons[1].ModalResult := mrOK;
        fButtons[1].Visible := true;
        fButtons[0].Caption := rsCancel;
        fButtons[0].ModalResult := mrCancel;
        fButtons[1].Default := true;
      end;
    MB_ABORTRETRYIGNORE:
      begin
        fButtons[2].Caption := rsAbort;
        fButtons[2].ModalResult := mrAbort;
        fButtons[2].Visible := true;
        fButtons[1].Caption := rsRetry;
        fButtons[1].ModalResult := mrRetry;
        fButtons[1].Visible := true;
        fButtons[0].Caption := rsIgnore;
        fButtons[0].ModalResult := mrIgnore;
      end;
    MB_YESNOCANCEL:
      begin
        fButtons[2].Caption := rsYes;
        fButtons[2].ModalResult := mrYes;
        fButtons[2].Visible := true;
        fButtons[1].Caption := rsNo;
        fButtons[1].ModalResult := mrNo;
        fButtons[1].Visible := true;
        fButtons[0].Caption := rsCancel;
        fButtons[0].ModalResult := mrCancel;
      end;
    MB_YESNO:
      begin
        fButtons[1].Caption := rsYes;
        fButtons[1].ModalResult := mrYes;
        fButtons[1].Visible := true;
        fButtons[0].Caption := rsNo;
        fButtons[0].ModalResult := mrNo;
      end;
    MB_RETRYCANCEL:
      begin
        fButtons[1].Caption := rsRetry;
        fButtons[1].ModalResult := mrRetry;
        fButtons[1].Visible := true;
        fButtons[0].Caption := rsCancel;
        fButtons[0].ModalResult := mrCancel;
      end;
    MB_CANCELTRYCONTINUE:
      begin
        fButtons[2].Caption := rsCancel;
        fButtons[2].ModalResult := mrCancel;
        fButtons[2].Visible := true;
        fButtons[1].Caption := rsTryAgain;
        fButtons[1].ModalResult := mrTryAgain;
        fButtons[1].Visible := true;
        fButtons[0].Caption := rsContinue;
        fButtons[0].ModalResult := mrContinue;
        fButtons[0].Cancel := false;
      end;
      MB_CUSTOM2BUTTONS:
      begin
        fButtons[1].ModalResult := mrButton3;
        fButtons[1].Visible := true;
        fButtons[0].ModalResult := mrButton4;
      end;
      MB_CUSTOM3BUTTONS:
      begin
        fButtons[2].ModalResult := mrButton2;
        fButtons[2].Visible := true;
        fButtons[1].ModalResult := mrButton3;
        fButtons[1].Visible := true;
        fButtons[0].ModalResult := mrButton4;
      end;
      MB_CUSTOM4BUTTONS:
      begin
        fButtons[3].ModalResult := mrButton1;
        fButtons[3].Visible := true;
        fButtons[2].ModalResult := mrButton2;
        fButtons[2].Visible := true;
        fButtons[1].ModalResult := mrButton3;
        fButtons[1].Visible := true;
        fButtons[0].ModalResult := mrButton4;
      end;
  end;

  visibleBtns := 1;
  while (visibleBtns < 4) and fButtons[visibleBtns].Visible do
    inc(visibleBtns);

  // custom button captions ///////////////////////////////
  j := Min(visibleBtns, customBtnCaptionCnt) -1;
  for i := 0 to j do
  begin
    button := fButtons[j-i];
    button.Caption := options.ButtonCaps[i];
    button.Width := Max(dpi75, fForm.Canvas.TextWidth(button.caption) +dpi20);
  end;

  // dialog and edit control widths ///////////////////////
  clientWidth := dialogMargin;
  for i := 0 to visibleBtns -1 do
    inc(clientWidth, fButtons[i].Width + dpi7);
  inc(clientWidth, dialogMargin - dpi7);
  clientWidth := Max(clientWidth, Max(padding * 19,
    Max(fBigTextRect.Right, fSmallTextRect.Right) + dialogMargin));

  fEdit.Width := clientWidth - dialogMargin *2;
  fCombo.Width := clientWidth - dialogMargin *2;
  fCheckbox.Width := clientWidth - dialogMargin *2;

  fForm.ClientWidth := clientWidth;
  fForm.ClientHeight := fButtons[0].Top + fButtons[0].Height + dpi11;

  // position buttons horizontally
  j := clientWidth - dialogMargin + dpi7;
  for i := 0 to visibleBtns -1 do
  begin
    dec(j, fButtons[i].Width + dpi7);
    fButtons[i].Left := j;
  end;

  // set the default button
  if (style and MB_DEFBUTTON4 = MB_DEFBUTTON4) and (visibleBtns = 4) then
    button := fButtons[visibleBtns - 4]
  else if (style and MB_DEFBUTTON3 = MB_DEFBUTTON3) and (visibleBtns >= 3) then
    button := fButtons[visibleBtns - 3]
  else if (style and MB_DEFBUTTON2 = MB_DEFBUTTON2) and (visibleBtns >= 2) then
    button := fButtons[visibleBtns - 2]
  else
    button := fButtons[visibleBtns - 1];
  button.Default := true;
  if (dialogType = dtMessage) then fForm.ActiveControl := button;
end;
//------------------------------------------------------------------------------

function TCustomDialog.DoEditCallback(Sender: TObject): Boolean;
begin
  Result := true;
  if not Assigned(fEditCallbackFunc) then Exit;
  fEditCallbackFunc(self, fEdit.Text, Result);
end;
//------------------------------------------------------------------------------

function TCustomDialog.TextInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: string;
  const options: TTextBoxOptions): boolean;
begin
  fTitle := ATitle;
  fBigText := ABigText;
  fSmallText := ASmallText;
  fEditCallbackFunc := options.editChangeCallBk;

  fForm := TForm.Create(AOwner);
  try
    SetupDialog(MB_OKCANCEL, dtEdit, options, false);
    fEdit.Text := Value;
    fEdit.CharCase := options.charCase;
    fEdit.OnChange := ValidateStrInput;
    ValidateStrInput(nil);
    result := fForm.ShowModal = mrOK;
    if result then Value := fEdit.Text;
  finally
    fForm.Free;
  end;
end;
//------------------------------------------------------------------------------

function TCustomDialog.ComboInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string;
  var index: integer; const options: TComboBoxOptions): boolean;
var
  i: integer;
begin
  fTitle := ATitle;
  fBigText := ABigText;
  fSmallText := ASmallText;
  fEditCallbackFunc := options.editChangeCallBk;

  fForm := TForm.Create(AOwner);
  try
    SetupDialog(MB_OKCANCEL, dtCombo, options, false);
    for i := 0 to high(options.comboItems) do
      fCombo.Items.Add(options.comboItems[i]);
    fCombo.ItemIndex := index;
    fButtons[1].Enabled := fCombo.Items.Count > 0;
    result := fForm.ShowModal = mrOK;
    if result then index := fCombo.ItemIndex;
  finally
    fForm.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TCustomDialog.ValidateStrInput(Sender: TObject);

  function IsValidStr: boolean;
  var
    S: string;
    i: integer;
  begin
    S := fEdit.Text;

    result := (Length(S) > 0) or fAllowEmptyString;
    if not result then Exit; //error
    if fAllowOnlyOptions = [] then Exit; //success, nothing further required

    if aoLetters in fAllowOnlyOptions then
      Include(fAllowOnlyOptions, aoAZ);

    if aoAZ in fAllowOnlyOptions then
    begin
      Include(fAllowOnlyOptions, aoCapitalAZ);
      Include(fAllowOnlyOptions, aoSmallAZ);
    end;

    result := false;
    for i := 1 to Length(S) do
      case S[i] of
        'a'..'z':
          if not (aoSmallAZ in fAllowOnlyOptions) then Exit;
        'A'..'Z':
          if not (aoCapitalAZ in fAllowOnlyOptions) then Exit;
        '0'..'9':
          if not (aoDigits in fAllowOnlyOptions) then Exit;
        ' ':
          if not (aoSpace in fAllowOnlyOptions) then Exit;
        '.':
          if not (aoPeriod in fAllowOnlyOptions) then Exit;
        ',':
          if not (aoComma in fAllowOnlyOptions) then Exit;
        ';':
          if not (aoSemicolon in fAllowOnlyOptions) then Exit;
        '-':
          if not (aoHyphenMinus in fAllowOnlyOptions) then Exit;
        '+':
          if not (aoPlus in fAllowOnlyOptions) then Exit;
        '_':
          if not (aoUnderscore in fAllowOnlyOptions) then Exit;
        '*':
          if not (aoAsterisk in fAllowOnlyOptions) then Exit;
      else
{$IFDEF UNICODE}
        if (aoLetters in fAllowOnlyOptions) and not
          CharInSet(S[i], ['A'..'Z','a'..'z']) then Exit;
{$ELSE}
        if (aoLetters in fAllowOnlyOptions) and not
          (S[i] in['A'..'Z','a'..'z']) then Exit;
{$ENDIF}

      end;
      result := true;
  end;

begin
  fButtons[1].Enabled := IsValidStr and DoEditCallback(self);
end;
//------------------------------------------------------------------------------

function TCustomDialog.MessageBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; style: Cardinal;
  const options: TMessageBoxOptions): Cardinal;
var
  cbVisible: Boolean;
begin
  fTitle := ATitle;
  fBigText := ABigText;
  fSmallText := ASmallText;
  cbVisible := Assigned(options.CheckBoxCallBk);
  fForm := TForm.Create(AOwner);
  try
    SetupDialog(style, dtMessage, options, cbVisible);
    result := fForm.ShowModal;
    if cbVisible then
      options.CheckBoxCallBk(self, fCheckbox.Checked);
  finally
    fForm.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TCustomDialog.ValidateIntInput(Sender: TObject);
var
  n: integer;
begin
  fButtons[1].Enabled := TryStrToInt(fEdit.Text, n) and //a valid int value and
    //either no meaninful min/max values supplied or the value is in range
    ((fMinInt = fMaxInt) or InRange(n, fMinInt, fMaxInt)) and
    DoEditCallback(self);
end;
//------------------------------------------------------------------------------

procedure TCustomDialog.ValidateHexInput(Sender: TObject);
var
  i, len: integer;
  s: string;
begin

  s := UpperCase(fEdit.Text);
  len := Length(s);
  if len <> fMaxInt then
  begin
    fButtons[1].Enabled := false;
    Exit;
  end;
  i := 1;
  while i <= len do
    if ((s[i] >= '0') and (s[i] <= '9')) or
      ((s[i] >= 'A') and (s[i] <= 'F')) then
        inc(i) else
        break;
  fButtons[1].Enabled := (i > len) and DoEditCallback(self);
end;
//------------------------------------------------------------------------------

procedure TCustomDialog.ValidateRealInput(Sender: TObject);
var
  x: double;
begin
  fButtons[1].Enabled := TryStrToFloat(fEdit.Text, x) and
    ((fMinFloat = fMaxFloat) or InRange(x, fMinFloat, fMaxFloat)) and
    DoEditCallback(self);
end;
//------------------------------------------------------------------------------

function TCustomDialog.CharInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: char;
  const options: TTextBoxOptions): boolean;
begin
  fTitle := ATitle;
  fBigText := ABigText;
  fSmallText := ASmallText;
  fAllowEmptyString := false;
  fAllowOnlyOptions := options.allowOnlyOptions;
  fEditCallbackFunc := options.editChangeCallBk;

  fForm := TForm.Create(AOwner);
  try
    SetupDialog(MB_OKCANCEL, dtEdit, options, false);
    fEdit.Text := Value;
    fEdit.CharCase := options.CharCase;
    fEdit.OnChange := ValidateStrInput;
    fEdit.MaxLength := 1;
    ValidateStrInput(nil);
    result := fForm.ShowModal = mrOK;
    if result then Value := fEdit.Text[1];
  finally
    fForm.Free;
  end;
end;
//------------------------------------------------------------------------------

function TCustomDialog.FloatInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: double;
  const options: TFloatBoxOptions): boolean;
begin
  fMinFloat := options.minVal;
  fMaxFloat := options.maxVal;
  fTitle := ATitle;
  fBigText := ABigText;
  fSmallText := ASmallText;
  fEditCallbackFunc := options.editChangeCallBk;

  fForm := TForm.Create(AOwner);
  try
    SetupDialog(MB_OKCANCEL, dtEdit, options, false);
    fEdit.Text := FloatToStr(Value);
    fEdit.OnChange := ValidateRealInput;
    ValidateRealInput(nil);
    result := fForm.ShowModal = mrOK;
    if result then Value := StrToFloat(fEdit.Text);
  finally
    fForm.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TCustomDialog.FormActivate(Sender: TObject);
var
  b: BOOL; //must be BOOL (4 bytes)
begin
  if SystemParametersInfo(SPI_GETSNAPTODEFBUTTON, 0, @b, 0) and b then
    with fButtons[1] do
      with ClientToScreen(Point(Width div 2, Height div 2)) do
        SetCursorPos(x, y);
  fForm.OnActivate := nil;
end;
//------------------------------------------------------------------------------

function TCustomDialog.NumInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: integer;
  const options: TNumBoxOptions): boolean;
const
  UDM_SETPOS32 = WM_USER + 113;
var
  ICCX: TInitCommonControlsEx;
begin
  fMinInt := options.minVal;
  fMaxInt := options.maxVal;
  fTitle := ATitle;
  fBigText := ABigText;
  fSmallText := ASmallText;
  fEditCallbackFunc := options.editChangeCallBk;

  fForm := TForm.Create(AOwner);
  try
    SetupDialog(MB_OKCANCEL, dtEdit, options, false);
    ICCX.dwSize := sizeof(ICCX);
    ICCX.dwICC := ICC_UPDOWN_CLASS;
    InitCommonControlsEx(ICCX);
    fSpinHandle := CreateWindowEx(0, PChar(UPDOWN_CLASS), nil,
      WS_CHILDWINDOW or WS_VISIBLE or UDS_NOTHOUSANDS or UDS_SETBUDDYINT or
      UDS_ALIGNRIGHT or UDS_ARROWKEYS or UDS_HOTTRACK, 0, 0, 0, 0, fForm.Handle,
      0, HInstance, nil);
    SendMessage(fSpinHandle, UDM_SETRANGE32, fMinInt, fMaxInt);
    SendMessage(fSpinHandle, UDM_SETPOS32, 0, Value);
    SendMessage(fSpinHandle, UDM_SETBUDDY, fEdit.Handle, 0);

{$IFDEF UNICODE}
    if fMinInt >= 0 then
      fEdit.NumbersOnly := true;
{$ENDIF}
    fEdit.Text := IntToStr(value);
    fEdit.OnChange := ValidateIntInput;
    ValidateIntInput(nil);
    result := fForm.ShowModal = mrOK;
    if result then Value := StrToInt(fEdit.Text);
  finally
    fForm.Free;
  end;
end;
//------------------------------------------------------------------------------

function TCustomDialog.HexInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var hex: string;
  const options: THexBoxOptions): boolean;
begin
  fTitle := ATitle;
  fBigText := ABigText;
  fSmallText := ASmallText;
  fMaxInt := options.maxLen;
  if fMaxInt <= 0 then fMaxInt := 8 //default = 8
  else if fMaxInt > 16 then fMaxInt := 16;
  fEditCallbackFunc := options.editChangeCallBk;

  fForm := TForm.Create(AOwner);
  try
    SetupDialog(MB_OKCANCEL, dtEdit, options, false);
    fEdit.Text := hex;
    fEdit.OnChange := ValidateHexInput;
    ValidateHexInput(nil);
    result := fForm.ShowModal = mrOK;
    if result then hex := fEdit.Text;
  finally
    fForm.Free;
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function MessageBox(AOwner: TCustomForm; const ABigText, ASmallText,
  ATitle: string; style: Cardinal): TModalResult;
var
  options: IOptions;
begin
  options := TMessageBoxOptions.Create;
  Result := MessageBox(AOwner, ABigText, ASmallText, ATitle, style,
    TMessageBoxOptions(options.AsObject));
end;
//------------------------------------------------------------------------------

function MessageBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; style: Cardinal;
  const options: TMessageBoxOptions): TModalResult;
begin
  with TCustomDialog.Create do
  try
    Result := MessageBox(AOwner, ABigText, ASmallText, ATitle, style, options);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function TextInputBox(AOwner: TCustomForm; const ABigText, ASmallText,
  ATitle: string; var Value: string): boolean;
var
  options: IOptions;
begin
  options := TTextBoxOptions.Create;
  Result := TextInputBox(AOwner, ABigText, ASmallText, ATitle, Value,
    TTextBoxOptions(options.AsObject));
end;
//------------------------------------------------------------------------------

function TextInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: string;
  const options: TTextBoxOptions): boolean;
begin
  with TCustomDialog.Create do
  try
    Result := TextInputBox(AOwner, ABigText, ASmallText, ATitle, Value, options);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function ComboInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var index: integer;
  const options: TComboBoxOptions): boolean;
begin
  with TCustomDialog.Create do
  try
    Result := ComboInputBox(AOwner, ABigText, ASmallText,
      ATitle, index, options);
  finally
    Free;
  end;
end;
//------------------------------------------------------------------------------

function NumInputBox(AOwner: TCustomForm; const ABigText, ASmallText,
  ATitle: string; var Value: integer): boolean;
var
  options: IOptions;
begin
  options := TNumBoxOptions.Create;
  result := NumInputBox(AOwner, ABigText, ASmallText, ATitle, Value,
    TNumBoxOptions(options.AsObject));
end;
//------------------------------------------------------------------------------

function NumInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: integer;
  const options: TNumBoxOptions): boolean;
begin
  with TCustomDialog.Create do
  try
    result := NumInputBox(AOwner, ABigText, ASmallText, ATitle, Value, options);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function HexInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var hex: string;
  const options: THexBoxOptions): boolean;
begin
  with TCustomDialog.Create do
  try
    result := HexInputBox(AOwner, ABigText, ASmallText, ATitle, hex, options);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function FloatInputBox(AOwner: TCustomForm; const ABigText, ASmallText,
  ATitle: string; var Value: double): boolean;
var
  options: IOptions;
begin
  options := TFloatBoxOptions.Create;
  result := FloatInputBox(AOwner, ABigText, ASmallText, ATitle, Value,
    TFloatBoxOptions(options.AsObject));
end;
//------------------------------------------------------------------------------

function FloatInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: double;
  const options: TFloatBoxOptions): boolean;
begin
  with TCustomDialog.Create do
  try
    result := FloatInputBox(AOwner, ABigText, ASmallText, ATitle, Value, options);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

end.
