object Form1: TForm1
  Left = 523
  Top = 233
  Caption = 'Transform'
  ClientHeight = 762
  ClientWidth = 740
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  OnDblClick = FormDblClick
  OnDestroy = FormDestroy
  OnMouseDown = pnlMainMouseDown
  OnMouseMove = pnlMainMouseMove
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 192
  TextHeight = 27
  object StatusBar1: TStatusBar
    Left = 0
    Top = 724
    Width = 740
    Height = 38
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
    ExplicitTop = 723
    ExplicitWidth = 726
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
        Caption = 'Save &As...'
        ShortCut = 16449
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
      Caption = '&Action'
      object mnuVertSkew: TMenuItem
        Caption = 'V&ertical Skew'
        Checked = True
        GroupIndex = 12
        RadioItem = True
        ShortCut = 16459
        OnClick = mnuVerticalSplineClick
      end
      object mnuHorizontalSkew: TMenuItem
        Caption = 'Hori&zontal Skew'
        GroupIndex = 12
        RadioItem = True
        ShortCut = 16471
        OnClick = mnuVerticalSplineClick
      end
      object mnuVertProjective: TMenuItem
        Caption = 'Vertical &Projective'
        GroupIndex = 12
        RadioItem = True
        ShortCut = 16464
        OnClick = mnuVerticalSplineClick
      end
      object mnuVerticalSpline: TMenuItem
        Caption = 'Vertical &Spline'
        GroupIndex = 12
        RadioItem = True
        ShortCut = 16467
        OnClick = mnuVerticalSplineClick
      end
      object mnuHorizontalSpline: TMenuItem
        Caption = 'Horizontal Splin&e'
        GroupIndex = 12
        ShortCut = 16453
        OnClick = mnuVerticalSplineClick
      end
      object Rotate1: TMenuItem
        Caption = '&Rotate'
        GroupIndex = 12
        RadioItem = True
        ShortCut = 16466
        OnClick = mnuVerticalSplineClick
      end
      object N4: TMenuItem
        Caption = '-'
        GroupIndex = 12
      end
      object Reset1: TMenuItem
        Caption = 'Rese&t'
        GroupIndex = 12
        ShortCut = 16468
        OnClick = Reset1Click
      end
      object N5: TMenuItem
        Caption = '-'
        GroupIndex = 12
      end
      object mnuHideDesigners: TMenuItem
        Caption = '&Hide Designers'
        GroupIndex = 12
        ShortCut = 16456
        OnClick = mnuHideDesignersClick
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
    OnPopup = PopupMenu1Popup
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
      Caption = '&Hide Designer Controls'
      ShortCut = 16456
      OnClick = mnuHideDesignersClick
    end
  end
end
