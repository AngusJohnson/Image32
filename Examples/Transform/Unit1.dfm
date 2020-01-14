object Form1: TForm1
  Left = 618
  Top = 336
  Caption = 'Image32 - Transform'
  ClientHeight = 389
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 370
    Height = 370
    Align = alClient
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnMouseDown = Panel1MouseDown
    OnMouseMove = Panel1MouseMove
    OnMouseUp = Panel1MouseUp
    ExplicitLeft = 8
    ExplicitTop = -6
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 370
    Width = 370
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      OnClick = File1Click
      object mnuOpen: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = mnuOpenClick
      end
      object mnuSave: TMenuItem
        Caption = '&Save ...'
        ShortCut = 16467
        OnClick = mnuSaveClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object CopytoClipboard1: TMenuItem
        Caption = '&Copy to Clipboard'
        ShortCut = 16451
        OnClick = CopytoClipboard1Click
      end
      object mnuPastefromClipboard: TMenuItem
        Caption = '&Paste from Clipboard'
        ShortCut = 16470
        OnClick = mnuPastefromClipboardClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
    object ransformType1: TMenuItem
      Caption = '&Transform Type'
      object mnuVertSkew: TMenuItem
        Caption = '&Vertical Skew'
        Checked = True
        GroupIndex = 11
        RadioItem = True
        OnClick = mnuVerticalSplineClick
      end
      object mnuHorizontalSkew: TMenuItem
        Caption = '&Horizontal Skew'
        GroupIndex = 11
        RadioItem = True
        OnClick = mnuVerticalSplineClick
      end
      object mnuVertProjective: TMenuItem
        Caption = 'Vertical &Projective'
        GroupIndex = 11
        RadioItem = True
        OnClick = mnuVerticalSplineClick
      end
      object mnuVerticalSpline: TMenuItem
        Caption = 'Vertical &Spline'
        GroupIndex = 11
        RadioItem = True
        OnClick = mnuVerticalSplineClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.bmp'
    Filter = 'Image Files (BMP, PNG, JPG)|*.bmp;*.png;*.jpg'
    Left = 184
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.bmp'
    Filter = 'Image Files (BMP, PNG, JPG)|*.bmp;*.png;*.jpg'
    Left = 248
    Top = 96
  end
  object PopupMenu1: TPopupMenu
    Left = 184
    Top = 144
    object mnuAddNewCtrlPoint: TMenuItem
      Caption = '&Add New Ctrl Point'
      OnClick = mnuAddNewCtrlPointClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mnuHideControls: TMenuItem
      AutoCheck = True
      Caption = '&Hide Designer Controls'
      ShortCut = 16456
      OnClick = mnuHideControlsClick
    end
  end
end
