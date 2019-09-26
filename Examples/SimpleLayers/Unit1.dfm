object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Image32 - Layers example'
  ClientHeight = 414
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 403
    Height = 388
    Align = alClient
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnMouseDown = Panel1MouseDown
    OnMouseMove = Panel1MouseMove
    OnMouseUp = Panel1MouseUp
    ExplicitWidth = 458
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 388
    Width = 403
    Height = 26
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = ' Right-click to add additional objects'
    UseSystemFont = False
    ExplicitWidth = 458
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
        OnClick = CopytoClipboard1Click
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
    object View1: TMenuItem
      Caption = '&View'
      object mnuHideControls: TMenuItem
        AutoCheck = True
        Caption = '&Hide Controls'
        GroupIndex = 1
        ShortCut = 16456
        OnClick = mnuHideControlsClick
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.bmp'
    Filter = 'Image Files (BMP, PNG, JPG)|*.bmp;*.png;*.jpg'
    Left = 240
    Top = 80
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 232
    Top = 152
    object AddEllipse1: TMenuItem
      Caption = 'Add &Ellipse'
      OnClick = AddEllipse1Click
    end
    object AddRectangle1: TMenuItem
      Caption = 'Add &Rectangle'
      OnClick = AddRectangle1Click
    end
    object AddText1: TMenuItem
      Caption = 'Add &Text'
      OnClick = AddText1Click
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
end
