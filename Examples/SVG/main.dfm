object Form1: TForm1
  Left = 298
  Top = 205
  Caption = 'Svg101'
  ClientHeight = 470
  ClientWidth = 756
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter1: TSplitter
    Left = 180
    Top = 0
    Height = 451
    ExplicitHeight = 452
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 451
    Width = 756
    Height = 19
    Panels = <>
  end
  object ImagePanel: TImage32Panel
    Left = 183
    Top = 0
    Width = 573
    Height = 451
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 12
    Color = clBtnFace
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 1
    TabStop = True
    Touch.InteractiveGestures = [igZoom, igPan, igPressAndTap]
    OnResize = ImagePanelResize
    AutoCenter = True
    FocusedColor = clActiveCaption
    UnFocusedColor = clBtnFace
    Scale = 1.000000000000000000
    ScaleMin = 0.050000000000000000
    ScaleMax = 20.000000000000000000
    ShowScrollButtons = ssbFocused
    AllowKeyScroll = True
    AllowScrnScroll = True
    AllowZoom = True
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 180
    Height = 451
    Align = alLeft
    Color = 16316664
    ItemHeight = 14
    PopupMenu = PopupMenu1
    TabOrder = 2
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
      Caption = '&Open in Notepad'
      OnClick = PopupMenu11Click
    end
    object OpeninBrowser1: TMenuItem
      Caption = '&Open in Browser'
      OnClick = OpeninBrowser1Click
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.jpg'
    Filter = 'Image Files (*.png;*.jpg)'
    Left = 407
    Top = 56
  end
end
