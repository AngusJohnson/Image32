unit CustomDialogs;

(*******************************************************************************
* Author    :  Angus Johnson                                                   *
* Version   :  1.6                                                             *
* Date      :  14 December 2019                                                *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2019                                         *
* Purpose   :  Windows styled dialog boxes for messages, text input etc        *
* License   :  http://www.boost.org/LICENSE_1_0.txt                            *
*                                                                              *
* Loosely based on Andreas Rejbrand's code - http://english.rejbrand.se        *
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

  PMessageBoxOptions = ^TMessageBoxOptions;
  TMessageBoxOptions = record
    customIcon     : TIcon;
    buttonCaptions : TStringDynArray;
    position       : TDialogPosition;        //still in development :)
    positionRec    : TRect;                  //still in development :)
    checkBoxCallBk : TNotifyCheckboxChangedFunc;
  end;

  TTextBoxOptions = record
    customIcon       : TIcon;
    buttonCaptions   : TStringDynArray;
    position         : TPosition;
    positionRec      : TRect;
    charCase         : TEditCharCase;
    allowEmptyString : Boolean;
    allowOnlyOptions : TAllowOnlyOptions;
    editChangeCallBk : TNotifyEditChangedFunc;
  end;

  TComboboxOptions = record
    customIcon       : TIcon;
    buttonCaptions   : TStringDynArray;
    position         : TPosition;
    positionRec      : TRect;
    editChangeCallBk : TNotifyEditChangedFunc;
    comboItems       : TStringDynArray;
  end;

  TNumBoxOptions = record
    customIcon       : TIcon;
    buttonCaptions   : TStringDynArray;
    position         : TPosition;
    positionRec      : TRect;
    minVal           : integer;
    maxVal           : integer;
    allowEmptyString : Boolean;
    allowOnlyOptions : TAllowOnlyOptions;
    editChangeCallBk : TNotifyEditChangedFunc;
  end;

  TFloatBoxOptions = record
    customIcon       : TIcon;
    buttonCaptions   : TStringDynArray;
    position         : TPosition;
    positionRec      : TRect;
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

  function FloatInputBox(AOwner: TCustomForm; const ABigText, ASmallText,
    ATitle: string; var Value: double): boolean; overload;
  function FloatInputBox(AOwner: TCustomForm;
    const ABigText, ASmallText, ATitle: string; var Value: double;
    const options: TFloatBoxOptions): boolean; overload;

  function StringToMBStyle(text: string): Cardinal;
  function AddNewlinesAtEndSentence(const txt: string): string;

var
  SoundBeepOnStop : Boolean = true;
  smallFontSize   : integer = 10;
  bigFontSize     : integer = 16;

const
  mrTryAgain = IDTRYAGAIN;
  mrContinue = IDCONTINUE;

implementation

{$R CustomDialogs.res}

uses Math, Messages;

resourcestring
  rsYes = '&Yes';
  rsNo  = '&No';
  rsOK = '&OK';
  rsCancel = '&Cancel';
  rsAbort = '&Abort';
  rsIgnore = '&Ignore';
  rsRetry = '&Retry';
  rsTryAgain = '&Try again';
  rsContinue = 'Co&ntinue';
  rsCheckbox = '&Don''t display this message again.';

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

function StringToMBStyle(text: string): Cardinal;
const
  MB_CANCELTRYCONTINUE = 6;
begin
  text := uppercase(text);
  result := 0;
  if pos('MB_CANCELTRYCONTINUE', text) > 0 then
    result := result or MB_CANCELTRYCONTINUE
  else if pos('MB_RETRYCANCEL', text) > 0 then
    result := result or MB_RETRYCANCEL
  else if pos('MB_YESNOCANCEL', text) > 0 then
    result := result or MB_YESNOCANCEL
  else if pos('MB_YESNO', text) > 0 then
    result := result or MB_YESNO
  else if pos('MB_OKCANCEL', text) > 0 then
    result := result or MB_OKCANCEL
  else if pos('MB_ABORTRETRYIGNORE', text) > 0 then
    result := result or MB_ABORTRETRYIGNORE;

  if pos('MB_ICONEXCLAMATION', text) > 0 then
    result := result or MB_ICONEXCLAMATION
  else if pos('MB_ICONWARNING', text) > 0 then
    result := result or MB_ICONWARNING
  else if pos('MB_ICONINFORMATION', text) > 0 then
    result := result or MB_ICONINFORMATION
  else if pos('MB_ICONASTERISK', text) > 0 then
    result := result or MB_ICONASTERISK
  else if pos('MB_ICONQUESTION', text) > 0 then
    result := result or MB_ICONQUESTION
  else if pos('MB_ICONSTOP', text) > 0 then
    result := result or MB_ICONSTOP
  else if pos('MB_ICONERROR', text) > 0 then
    result := result or MB_ICONERROR
  else if pos('MB_ICONHAND', text) > 0 then
    result := result or MB_ICONHAND;

  if pos('MB_DEFBUTTON2', text) > 0 then
    result := result or MB_DEFBUTTON2
  else if pos('MB_DEFBUTTON3', text) > 0 then
    result := result or MB_DEFBUTTON3;
end;
//------------------------------------------------------------------------------

function AddNewlinesAtEndSentence(const txt: string): string;
var
  i: integer;
begin
  //Adds newlines at end of sentences ...
  result := txt;
  for i := length(result) - 2 downto 2 do
{$IFDEF UNICODE}
    if CharInSet(result[i], ['.','?','!']) and (result[i+1] = #32) then
{$ELSE}
    if (result[i] in ['.','?','!']) and (result[i+1] = #32) then
{$ENDIF}
      result[i+1] := #10;
end;
//------------------------------------------------------------------------------

type
  TDialogType = (dtMessage, dtEdit, dtCombo);

  TCustomDialog = class
  private
    fForm: TForm;
    fEdit: TEdit;
    fCombo: TComboBox;
    fBtnYes,
    fBtnOK,
    fBtnCancel: TButton;
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
      dialogType: TDialogType; const options: TMessageBoxOptions);
    procedure ValidateIntInput(Sender: TObject);
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
    function FloatInputBox(AOwner: TCustomForm;
      const ABigText, ASmallText, ATitle: string; var Value: double;
      const options: TFloatBoxOptions): boolean;
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

procedure TCustomDialog.SetupDialog(style: cardinal;
  dialogType: TDialogType; const options: TMessageBoxOptions);
var
  i, len, iconSize, padding, clientWidth: integer;
  dialogMargin, scaled_11: integer;
  showIcon: Boolean;
const
  ICONMASK = $70;
  MB_CANCELTRYCONTINUE = $6;
begin
  // https://msdn.microsoft.com/en-us/windows/desktop/dn742486
  // https://msdn.microsoft.com/en-us/library/windows/desktop/dn742478(v=vs.85).aspx

  clientWidth := DPI(400); //maximum width
  scaled_11 := DPI(11);
  dialogMargin := scaled_11 *2;

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
  padding := scaled_11;
  showIcon := Assigned(options.CustomIcon) or (style and ICONMASK <> 0);

  // Icons //////////////////////////////////////////////
  if showIcon then
  begin
    iconSize := DPI(32);
    if iconSize >= 48 then iconSize := 48       //'large' fonts (150%)
    else if iconSize > 32 then iconSize := 40;  //(125%)

    fIconImage := TImage.Create(fForm);
    fIconImage.Parent := fForm;
    fIconImage.Left := dialogMargin;
    fIconImage.Top := scaled_11;

    //CustomDialog.res contains Windows 10 style icons
    if assigned(options.CustomIcon) then
      fIconImage.Picture.Icon.Handle := options.CustomIcon.Handle

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
    if iconSize > 0 then
      fBigTextRect := Rect(dialogMargin + iconSize + padding,
        scaled_11, clientWidth - dialogMargin, scaled_11 + 2)
    else
      fBigTextRect := Rect(dialogMargin,
        scaled_11, clientWidth - dialogMargin, scaled_11 + 2);
    fForm.Canvas.Font.Size := bigFontSize;
    DrawText(fForm.Canvas.Handle, PChar(fBigText), Length(fBigText),
      fBigTextRect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);
    if iconSize > 0 then
    begin
      len := iconSize + scaled_11 - fBigTextRect.Bottom;
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
  if fSmallText <> '' then
  begin
    if iconSize > 0 then
      fSmallTextRect := Rect(dialogMargin + iconSize + padding,
        fBigTextRect.Bottom + scaled_11,
        clientWidth - dialogMargin, scaled_11 + 2)
    else
      fSmallTextRect := Rect(dialogMargin,
        fBigTextRect.Bottom + scaled_11,
        clientWidth - dialogMargin, scaled_11 +2);

    fForm.Canvas.Font.Size := smallFontSize;
    DrawText(fForm.Canvas.Handle, PChar(fSmallText), Length(fSmallText),
      fSmallTextRect, DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK);

    if iconSize > 0 then
    begin
      len := iconSize + scaled_11 - fSmallTextRect.Bottom;
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
  fEdit.Top := fSmallTextRect.Bottom + scaled_11;
  fEdit.Left := dialogMargin;

  fCombo := TComboBox.Create(fForm);
  fCombo.Parent := fForm;
  fCombo.Style := csDropDownList;
  fCombo.Top := fSmallTextRect.Bottom + scaled_11;
  fCombo.Left := dialogMargin;

  case dialogType of
    dtMessage:
      begin
        fEdit.Visible := false;
        fCombo.Visible := false;
        //fHorzLineYPos := fEdit.Top + scaled_11;
        fHorzLineYPos := fSmallTextRect.Bottom + scaled_11 *2;
      end;
    dtEdit:
      begin
        fCombo.Visible := false;
        fHorzLineYPos := fEdit.Top + fEdit.Height  + dialogMargin;
      end;
    dtCombo:
      begin
        fEdit.Visible := false;
        fHorzLineYPos := fEdit.Top + fEdit.Height  + dialogMargin;
      end;
  end;

  fCheckbox := TCheckBox.Create(fForm);
  fCheckbox.Parent := fForm;
  fCheckbox.Visible :=
    (dialogType = dtMessage) and assigned(options.checkBoxCallBk);
  if fCheckbox.Visible then
  begin
    fCheckbox.Top := fSmallTextRect.Bottom + scaled_11;
    fCheckbox.Left := dialogMargin;
    fCheckbox.Height := fEdit.Height;
    fCheckbox.Caption := rsCheckbox;
    if fCheckbox.Visible then
      fHorzLineYPos := fCheckbox.Top + fCheckbox.Height  + dialogMargin div 2;
  end;

  fBtnYes := TButton.Create(fForm);
  fBtnYes.Parent := fForm;
  fBtnYes.Top := fHorzLineYPos + 1{*} + scaled_11;
  fBtnYes.Height := DPI(26);
  fBtnYes.Width := DPI(75);
  fBtnYes.Caption := rsYes;
  fBtnYes.ModalResult := mrYes;
  if ((style and MB_YESNOCANCEL) <> MB_YESNOCANCEL) and
    ((style and MB_CANCELTRYCONTINUE) <> MB_CANCELTRYCONTINUE) and
    ((style and MB_ABORTRETRYIGNORE) <> MB_ABORTRETRYIGNORE) then
      fBtnYes.Visible := false;

  fBtnOK := TButton.Create(fForm);
  fBtnOK.Parent := fForm;
  fBtnOK.Top := fBtnYes.Top;
  fBtnOK.Height := fBtnYes.Height;
  fBtnOK.Width := fBtnYes.Width;

  fBtnCancel := TButton.Create(fForm);
  fBtnCancel.Parent := fForm;
  fBtnCancel.Height := fBtnOK.Height;
  fBtnCancel.Width := fBtnOK.Width;
  fBtnCancel.Top := fBtnOK.Top;
  fBtnCancel.Caption := rsCancel;
  fBtnCancel.ModalResult := mrCancel;
  fBtnCancel.Cancel := true;

  // default button captions //////////////////////////////
  if (style and MB_CANCELTRYCONTINUE = MB_CANCELTRYCONTINUE) then
  begin
    fBtnYes.Caption := rsCancel;
    fBtnYes.ModalResult := mrCancel;
    fBtnOK.Caption := rsTryAgain;
    fBtnOK.ModalResult := mrTryAgain;
    fBtnCancel.Cancel := false;
    fBtnCancel.Caption := rsContinue;
    fBtnCancel.ModalResult := mrContinue;
  end
  else if (style and MB_RETRYCANCEL = MB_RETRYCANCEL) then
  begin
    fBtnOK.Caption := rsRetry;
    fBtnOK.ModalResult := mrRetry;
  end
  else if (style and MB_YESNO = MB_YESNO) then
  begin
    fBtnOK.Caption := rsYes;
    fBtnOK.ModalResult := mrYes;
    fBtnCancel.Caption := rsNo;
    fBtnCancel.ModalResult := mrNo;
  end
  else if (style and MB_YESNOCANCEL) = MB_YESNOCANCEL then
  begin
    fBtnOK.Caption := rsNo;
    fBtnOK.ModalResult := mrNo;
  end
  else if (style and MB_ABORTRETRYIGNORE = MB_ABORTRETRYIGNORE) then
  begin
    fBtnYes.Caption := rsAbort;
    fBtnYes.ModalResult := mrAbort;
    fBtnOK.Caption := rsRetry;
    fBtnOK.ModalResult := mrRetry;
    fBtnCancel.Caption := rsIgnore;
    fBtnCancel.ModalResult := mrIgnore;
    fBtnCancel.Cancel := false;
  end
  else if (style and MB_OKCANCEL = MB_OKCANCEL) then
  begin
    fBtnOK.Caption := rsOK;
    fBtnOK.ModalResult := mrOk;
    fBtnCancel.Caption := rsCancel;
    fBtnCancel.ModalResult := mrCancel;
  end else
  begin
    fBtnCancel.Caption := rsOK;
    fBtnCancel.ModalResult := mrOK;
    fBtnOK.Visible := false;
  end;

  // dialog and edit control widths ///////////////////////
  clientWidth := Max(fBigTextRect.Right, fSmallTextRect.Right) + dialogMargin;
  clientWidth := Max(clientWidth, padding * 19);
  if fBtnYes.Visible then fForm.ClientWidth :=
    Max(clientWidth, fBtnYes.Width * 3 + dialogMargin * 2 + padding *2)
  else if fBtnOK.Visible then fForm.ClientWidth :=
    Max(clientWidth, fBtnYes.Width * 2 + dialogMargin * 2 + padding)
  else fForm.ClientWidth :=
    Max(clientWidth, fBtnYes.Width + dialogMargin * 2);

  if fEdit.Visible then
    fEdit.Width := fForm.ClientWidth - dialogMargin *2
  else if fCombo.Visible then
    fCombo.Width := fForm.ClientWidth - dialogMargin *2 ;
  //nb: When dialogs and hence edit controls are very narrow (due to
  //very short main and sub texts), it may be necessary to right pad either
  //main or sub text with spaces. (Another option would be to add another
  //function parameter that indicates a minimum dialog (or edit) width.)

  fCheckbox.Width := fForm.ClientWidth - dialogMargin *2;

  // position and style buttons ///////////////////////////
  fBtnCancel.Left := fForm.ClientWidth - fBtnCancel.Width - dialogMargin;
  fBtnOK.Left := fBtnCancel.Left - fBtnOK.Width - padding;
  fBtnYes.Left := fBtnOK.Left - fBtnOK.Width - padding;

  if (style and MB_DEFBUTTON3 = MB_DEFBUTTON3) then
  begin
    if fBtnYes.Visible then fBtnCancel.Default := true;
  end
  else if (style and MB_DEFBUTTON2 = MB_DEFBUTTON2) then
  begin
    if fBtnYes.Visible then fBtnOK.Default := true
    else fBtnCancel.Default := true;
  end
  else if fBtnYes.Visible then fBtnYes.Default := true
  else if fBtnOK.Visible then fBtnOK.Default := true
  else fBtnCancel.Default := true;

  if dialogType = dtMessage then
  begin
    if fBtnOK.Default then fForm.ActiveControl := fBtnOK
    else if fBtnCancel.Default then fForm.ActiveControl := fBtnCancel
    else fForm.ActiveControl := fBtnYes;
  end;

  fForm.ClientHeight := fBtnOK.Top + fBtnOK.Height + scaled_11;

  // custom button captions ///////////////////////////////
  len := length(options.buttonCaptions);
  if (len > 0) then
  begin
    if (len = 3) and not fBtnYes.Visible then dec(len);
    if (len = 2) and not fBtnOK.Visible then dec(len);
    if (len = 3) then fBtnYes.Caption := options.buttonCaptions[0];
    if (len >= 2) then fBtnOK.Caption := options.buttonCaptions[len-2];
    fBtnCancel.Caption := options.buttonCaptions[len-1];
  end;

  // finally, make sure button captions fit
  i := fForm.Canvas.TextWidth(fBtnCancel.Caption) +DPI(20) -fBtnCancel.Width;
  if i > 0 then
  begin
    fBtnCancel.Left := fBtnCancel.Left - i;
    fBtnCancel.Width := fBtnCancel.Width + i;
    fBtnOK.Left := fBtnOK.Left - i;
    fBtnYes.Left := fBtnYes.Left - i;
  end;
  i := fForm.Canvas.TextWidth(fBtnOK.Caption) +DPI(20) -fBtnOK.Width;
  if i > 0 then
  begin
    fBtnOK.Left := fBtnOK.Left - i;
    fBtnOK.Width := fBtnOK.Width + i;
    fBtnYes.Left := fBtnYes.Left - i;
  end;
  i := fForm.Canvas.TextWidth(fBtnYes.Caption) +DPI(20) -fBtnYes.Width;
  if i > 0 then
  begin
    fBtnYes.Left := fBtnYes.Left - i;
    fBtnYes.Width := fBtnYes.Width + i;
  end;
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
    SetupDialog(MB_OKCANCEL, dtEdit, PMessageBoxOptions(@options)^);
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
    SetupDialog(MB_OKCANCEL, dtCombo, PMessageBoxOptions(@options)^);
    for i := 0 to high(options.comboItems) do
      fCombo.Items.Add(options.comboItems[i]);
    fCombo.ItemIndex := index;
    fBtnOK.Enabled := fCombo.Items.Count > 0;
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
  fBtnOK.Enabled := IsValidStr and DoEditCallback(self);
end;
//------------------------------------------------------------------------------

function TCustomDialog.MessageBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; style: Cardinal;
  const options: TMessageBoxOptions): Cardinal;
begin
  fTitle := ATitle;
  fBigText := ABigText;
  fSmallText := ASmallText;
  fForm := TForm.Create(AOwner);
  try
    SetupDialog(style, dtMessage, options);
    result := fForm.ShowModal;
    if Assigned(options.checkBoxCallBk) then
      options.checkBoxCallBk(self, fCheckbox.Checked);
  finally
    fForm.Free;
  end;
end;
//------------------------------------------------------------------------------

procedure TCustomDialog.ValidateIntInput(Sender: TObject);
var
  n: integer;
begin
  fBtnOK.Enabled := TryStrToInt(fEdit.Text, n) and //a valid int value and
    //either no meaninful min/max values supplied or the value is in range
    ((fMinInt = fMaxInt) or InRange(n, fMinInt, fMaxInt)) and
    DoEditCallback(self);
end;
//------------------------------------------------------------------------------

procedure TCustomDialog.ValidateRealInput(Sender: TObject);
var
  x: double;
begin
  fBtnOK.Enabled := TryStrToFloat(fEdit.Text, x) and
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
    SetupDialog(MB_OKCANCEL, dtEdit, PMessageBoxOptions(@options)^);
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
    SetupDialog(MB_OKCANCEL, dtEdit, PMessageBoxOptions(@options)^);
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
    with fBtnOK do
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
    SetupDialog(MB_OKCANCEL, dtEdit, PMessageBoxOptions(@options)^);

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
//------------------------------------------------------------------------------

function MessageBox(AOwner: TCustomForm; const ABigText, ASmallText,
  ATitle: string; style: Cardinal): TModalResult;
var
  options: TMessageBoxOptions;
begin
  FillChar(options, SizeOf(options), 0);
  Result := MessageBox(AOwner, ABigText, ASmallText, ATitle, style, options);
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
  options: TTextBoxOptions;
begin
  FillChar(options, SizeOf(options), 0);
  Result := TextInputBox(AOwner, ABigText, ASmallText, ATitle, Value, options);
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
  options: TNumBoxOptions;
begin
  FillChar(options, SizeOf(options), 0);
  result := NumInputBox(AOwner, ABigText, ASmallText, ATitle, Value, options);
end;
//------------------------------------------------------------------------------

function NumInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: integer;
  const options: TNumBoxOptions): boolean;
begin
  with TCustomDialog.Create do
  try
    result := NumInputBox(AOwner, ABigText, ASmallText, ATitle,
      Value, options);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

function FloatInputBox(AOwner: TCustomForm; const ABigText, ASmallText,
  ATitle: string; var Value: double): boolean;
var
  options: TFloatBoxOptions;
begin
  FillChar(options, SizeOf(options), 0);
  result := FloatInputBox(AOwner, ABigText, ASmallText, ATitle, Value, options);
end;
//------------------------------------------------------------------------------

function FloatInputBox(AOwner: TCustomForm;
  const ABigText, ASmallText, ATitle: string; var Value: double;
  const options: TFloatBoxOptions): boolean;
begin
  with TCustomDialog.Create do
  try
    result := FloatInputBox(AOwner, ABigText, ASmallText, ATitle,
      Value, options);
  finally
    free;
  end;
end;
//------------------------------------------------------------------------------

end.
