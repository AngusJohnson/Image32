object Form1: TForm1
  Left = 298
  Top = 205
  Caption = 'Svg101'
  ClientHeight = 1079
  ClientWidth = 1665
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -24
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 192
  TextHeight = 29
  object Splitter1: TSplitter
    Left = 348
    Top = 0
    Width = 10
    Height = 1033
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 1033
    Width = 1665
    Height = 46
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 348
    Height = 1033
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alLeft
    Color = 16316664
    ItemHeight = 29
    PopupMenu = PopupMenu1
    TabOrder = 1
    Visible = False
    OnClick = ListBox1Click
  end
  object MainMenu1: TMainMenu
    Left = 248
    Top = 56
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As ...'
        ShortCut = 16449
        OnClick = SaveAs1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'SVG Files (*.svg)|*.svg'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 360
    Top = 56
  end
  object PopupMenu1: TPopupMenu
    Left = 303
    Top = 56
    object PopupMenu11: TMenuItem
      Caption = '&Open in Text Editor'
      ShortCut = 120
      OnClick = PopupMenu11Click
    end
    object OpeninBrowser1: TMenuItem
      Caption = '&Open in Browser'
      ShortCut = 121
      OnClick = OpeninBrowser1Click
    end
    object FindinExplorer1: TMenuItem
      Caption = 'Find in Explorer'
      ShortCut = 122
      OnClick = FindinExplorer1Click
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.jpg'
    Filter = 'Image Files (*.png;*.jpg)'
    Left = 407
    Top = 56
  end
  object PopupMenu2: TPopupMenu
    Left = 512
    Top = 64
    object Copy1: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
  end
end
