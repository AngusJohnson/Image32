object Form1: TForm1
  Left = 594
  Top = 418
  Caption = 'Image32 - Layers example'
  ClientHeight = 412
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object StatusBar1: TStatusBar
    Left = 0
    Top = 386
    Width = 403
    Height = 26
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
        OnClick = mnuCopytoClipboardClick
      end
      object PastefromClipboard1: TMenuItem
        Caption = '&Paste from Clipboard'
        ShortCut = 16470
        OnClick = mnuPastefromClipboardClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
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
    Filter = 'Image Files (BMP, PNG, JPG)|*.bmp;*.png;*.jpg'
    Left = 128
    Top = 144
  end
end
