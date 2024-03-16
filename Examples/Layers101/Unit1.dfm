object MainForm: TMainForm
  Left = 404
  Top = 201
  Caption = 'Layers101'
  ClientHeight = 822
  ClientWidth = 910
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  PopupMenu = PopupMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseDown = pnlMainMouseDown
  OnMouseMove = pnlMainMouseMove
  OnMouseUp = pnlMainMouseUp
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 192
  TextHeight = 29
  object StatusBar1: TStatusBar
    Left = 0
    Top = 770
    Width = 910
    Height = 52
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
    ExplicitTop = 769
    ExplicitWidth = 896
  end
  object MainMenu1: TMainMenu
    Left = 112
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = mnuExitClick
      end
    end
    object Action1: TMenuItem
      Caption = '&Action'
      OnClick = PopupMenu1Popup
      object mnuAddImage2: TMenuItem
        Caption = 'Add &Image ...'
        ShortCut = 16457
        OnClick = mnuAddImageClick
      end
      object mnuAddRectangle: TMenuItem
        Caption = 'Add &Rectangle'
        ShortCut = 16466
        OnClick = mnuAddRectangleClick
      end
      object mnuAddEllipse: TMenuItem
        Caption = 'Add &Ellipse'
        ShortCut = 16453
        OnClick = mnuAddEllipseClick
      end
      object mnuAddText: TMenuItem
        Caption = 'Add &Text'
        ShortCut = 16468
        OnClick = mnuAddTextClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object mnuDeleteLayer2: TMenuItem
        Caption = '&Delete Layer'
        ShortCut = 46
        OnClick = mnuDeleteLayerClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnuCutToClipboard: TMenuItem
        Caption = 'C&ut to Clipboard'
        ShortCut = 16472
        OnClick = mnuCutToClipboardClick
      end
      object CopytoClipboard1: TMenuItem
        Caption = '&Copy to Clipboard'
        ShortCut = 16451
        OnClick = mnuCopytoClipboardClick
      end
      object PastefromClipboard1: TMenuItem
        Caption = '&Paste from Clipboard'
        ShortCut = 16470
        OnClick = mnuPastefromClipboardClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnuBringtoFront2: TMenuItem
        Caption = '&Bring to &Front'
        ShortCut = 16454
        OnClick = mnuBringToFrontClick
      end
      object mnuSendtoBack2: TMenuItem
        Caption = 'Send to &Back'
        ShortCut = 16450
        OnClick = mnuSendToBackClick
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.png'
    Filter = 'Image Files (BMP, PNG, JPG)|*.bmp;*.png;*.jpg'
    Left = 240
    Top = 80
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 232
    Top = 152
    object mnuAddImage: TMenuItem
      Caption = 'Add &Image ...'
      ShortCut = 16457
      OnClick = mnuAddImageClick
    end
    object AddEllipse1: TMenuItem
      Caption = 'Add &Ellipse'
      ShortCut = 16453
      OnClick = mnuAddEllipseClick
    end
    object AddRectangle1: TMenuItem
      Caption = 'Add &Rectangle'
      ShortCut = 16466
      OnClick = mnuAddRectangleClick
    end
    object AddText1: TMenuItem
      Caption = 'Add &Text'
      ShortCut = 16468
      OnClick = mnuAddTextClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mnuBringToFront: TMenuItem
      Caption = 'Bring &Forward'
      ShortCut = 16454
      OnClick = mnuBringToFrontClick
    end
    object mnuSendToBack: TMenuItem
      Caption = 'Push &Back'
      ShortCut = 16450
      OnClick = mnuSendToBackClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mnuDeleteLayer: TMenuItem
      Caption = '&Delete Layer'
      ShortCut = 46
      OnClick = mnuDeleteLayerClick
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.png'
    Filter = 
      'Image Files (BMP, PNG, JPG, QOI, SVG)|*.bmp;*.png;*.jpg;*.qoi;*.' +
      'svg'
    Left = 128
    Top = 144
  end
end
