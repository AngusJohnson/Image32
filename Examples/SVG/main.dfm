object Form1: TForm1
  Left = 298
  Top = 205
  Caption = 'Svg101'
  ClientHeight = 424
  ClientWidth = 625
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
    Left = 161
    Top = 0
    Height = 424
    ExplicitLeft = 184
    ExplicitTop = 232
    ExplicitHeight = 100
  end
  object ImagePanel: TImage32Panel
    Left = 164
    Top = 0
    Width = 461
    Height = 424
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 12
    Color = clBtnFace
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 0
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
    AllowScroll = True
    AllowZoom = True
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 161
    Height = 424
    Align = alLeft
    Color = 16316664
    ItemHeight = 14
    TabOrder = 1
    Visible = False
    OnClick = ListBox1Click
  end
  object MainMenu1: TMainMenu
    Left = 192
    Top = 104
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open ...'
        ShortCut = 16463
        OnClick = Open1Click
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
    Left = 256
    Top = 104
  end
end
