object Form1: TForm1
  Left = 298
  Top = 205
  Caption = 'Svg101'
  ClientHeight = 508
  ClientWidth = 467
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
  object ImagePanel: TImage32Panel
    Left = 0
    Top = 0
    Width = 467
    Height = 508
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
    AllowKeyScroll = True
    AllowScrnScroll = True
    AllowZoom = True
  end
  object MainMenu1: TMainMenu
    Left = 192
    Top = 104
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        ShortCut = 27
        OnClick = Exit1Click
      end
    end
  end
end
